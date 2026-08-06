namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An independent transcription of CoreCLR's `MethodTableBuilder::HandleAutoLayout`
/// (methodtablebuilder.cpp:8266) for the value-type case — no parent instance fields, no
/// 32-bit-only offset bias, no inline arrays.
///
/// Written from the C++ rather than from `CliValueType.ComputeAutoLayoutFields`, and structured
/// differently on purpose: list folds over explicit size classes rather than mutable
/// bucket-index arrays. Two transcriptions that agree are much better evidence than one, because
/// the failure mode this guards against is a slip in the placement arithmetic, and a slip is
/// unlikely to be made identically in two differently-shaped pieces of code.
module private AutoLayoutOracle =

    /// Everything the layout algorithm needs to know about one field.
    type FieldShape =
        {
            Name : string
            Size : int
            /// The field's own alignment. Consulted only when it is a value class.
            Alignment : int
            /// True for a field that *is* a reference: those are hoisted to the front of the
            /// pointer-sized class.
            IsObjectReference : bool
            /// True for a genuine struct field, which CoreCLR places after every size class.
            /// Enums and `IntPtr` normalise to primitives and so are not value classes.
            IsValueClass : bool
            /// True for a value class that transitively holds a reference: those are placed at
            /// pointer alignment rather than at their own.
            ContainsReferences : bool
        }

    let private roundUp (alignment : int) (value : int) : int =
        let error = value % alignment
        if error = 0 then value else value + (alignment - error)

    /// Each field's offset by name, plus the end of the last field.
    let place (fields : FieldShape list) : Map<string, int> * int =
        let primitives = fields |> List.filter (fun f -> not f.IsValueClass)
        let valueClasses = fields |> List.filter (fun f -> f.IsValueClass)

        // One contiguous region per size class, largest class first.
        let placedPrimitives, afterPrimitives =
            [ 8 ; 4 ; 2 ; 1 ]
            |> List.fold
                (fun (acc, pos) slotSize ->
                    let inClass = primitives |> List.filter (fun f -> f.Size = slotSize)

                    if inClass.IsEmpty then
                        acc, pos
                    else

                    // References take the front of their class; both groups keep declaration order.
                    let ordered =
                        (inClass |> List.filter (fun f -> f.IsObjectReference))
                        @ (inClass |> List.filter (fun f -> not f.IsObjectReference))

                    let start = roundUp slotSize pos
                    let placed = ordered |> List.mapi (fun i f -> f.Name, start + (i * slotSize))
                    acc @ placed, start + (List.length ordered * slotSize)
                )
                ([], 0)

        // Value classes follow every size class, in declaration order.
        let placedValueClasses, finalEnd =
            valueClasses
            |> List.fold
                (fun (acc, pos) f ->
                    let alignment = if f.ContainsReferences then 8 else f.Alignment
                    let offset = roundUp alignment pos
                    acc @ [ f.Name, offset ], offset + f.Size
                )
                ([], afterPrimitives)

        (placedPrimitives @ placedValueClasses) |> Map.ofList, finalEnd

    /// The size CoreCLR reports for a value class containing references. Its `minAlign` is
    /// `containsGCPointers ? TARGET_POINTER_SIZE : largestAlignmentRequirement` once the type
    /// exceeds a pointer in size, and a type holding a reference always does.
    let size (fields : FieldShape list) : int =
        let _, finalEnd = place fields
        roundUp 8 finalEnd

[<TestFixture>]
module TestStructLayout =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private declaredHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.TypedReference

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int32

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Object

    let private cliField (name : string) (contents : CliType) (offset : int option) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = offset
            Type =
                match contents with
                | CliType.ObjectRef _ -> objectHandle
                | _ -> int32Handle
            MarshallingDescriptor = None
        }

    let private ofFields (layout : Layout) (fields : CliField list) : CliValueType =
        CliValueType.OfFields bct allCt declaredHandle layout CharSet.Ansi fields

    /// A field the property can hand to the implementation (`Field`) and to the oracle (`Shape`).
    /// Every generated field carries a distinct marker value, so that finding the right *shape*
    /// of thing at an offset is not enough — it has to be the right field.
    type private GeneratedField =
        {
            Field : CliField
            Shape : AutoLayoutOracle.FieldShape
        }

    /// `CliConcreteField` is private, so the properties below read the layout back through the
    /// public API instead: `DereferenceFieldAt` finds the field occupying a given (offset, size),
    /// which is exactly the question "did this field land where it should have?".
    let private tryFieldAt (offset : int) (size : int) (vt : CliValueType) : CliType option =
        try
            CliValueType.DereferenceFieldAt offset size vt |> Some
        with _ ->
            None

    /// A complete behavioural description of a value type's layout, obtained without any
    /// privileged access: what is visible at every (offset, size) a field could occupy. Two
    /// types with the same fingerprint are laid out identically as far as anything can tell.
    let private fingerprint (vt : CliValueType) : (int * int * CliType option) list =
        let size = (CliValueType.SizeOf vt).Size

        [
            for offset in 0 .. size - 1 do
                for slot in [ 1 ; 2 ; 4 ; 8 ] do
                    yield offset, slot, tryFieldAt offset slot vt
        ]

    let private makeKinds (index : int) : GeneratedField list =
        let name = $"f%d{index}"
        // Distinct per field, so that two same-shaped fields cannot be confused for each other.
        let marker = index + 1

        let primitive (contents : CliType) (size : int) (isRef : bool) : GeneratedField =
            {
                Field = cliField name contents None
                Shape =
                    {
                        AutoLayoutOracle.FieldShape.Name = name
                        Size = size
                        Alignment = size
                        IsObjectReference = isRef
                        IsValueClass = false
                        ContainsReferences = isRef
                    }
            }

        // Note the limit of this: a nested value class's `Size`/`Alignment` are read back from
        // the implementation under test, not fixed independently, so the oracle property checks
        // that the algorithm composes consistently with itself across one level of nesting rather
        // than that a nested struct is sized as CoreCLR would size it. The latter is pinned by
        // the hand-measured constants in `sourcesPure/StructLayoutGcAuto.cs` (`GcInner`, `NestGc`,
        // `GcOuter`), which the real runtime re-validates on every run.
        let valueClass (vt : CliValueType) (containsReferences : bool) : GeneratedField =
            let size = CliValueType.SizeOf vt

            {
                Field = cliField name (CliType.ValueType vt) None
                Shape =
                    {
                        AutoLayoutOracle.FieldShape.Name = name
                        Size = size.Size
                        Alignment = size.Alignment
                        IsObjectReference = false
                        IsValueClass = true
                        ContainsReferences = containsReferences
                    }
            }

        // A struct of two Int32s: 8 bytes, 4-aligned, no references.
        let plainStruct =
            ofFields
                Layout.Default
                [
                    cliField "a" (CliType.Numeric (CliNumericType.Int32 marker)) None
                    cliField "b" (CliType.Numeric (CliNumericType.Int32 0)) None
                ]

        // A struct wrapping a single reference: 8 bytes, pointer-aligned, holds a reference.
        let refStruct =
            ofFields Layout.Default [ cliField "r" (CliType.ObjectRef (Some (ManagedHeapAddress marker))) None ]

        [
            primitive (CliType.Numeric (CliNumericType.UInt8 (byte marker))) 1 false
            primitive (CliType.Bool (byte marker)) 1 false
            primitive (CliType.Numeric (CliNumericType.Int16 (int16 marker))) 2 false
            primitive (CliType.Char (byte marker, 0uy)) 2 false
            primitive (CliType.Numeric (CliNumericType.Int32 marker)) 4 false
            primitive (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (int64 marker)))) 8 false
            primitive (CliType.ObjectRef (Some (ManagedHeapAddress marker))) 8 true
            valueClass plainStruct false
            valueClass refStruct true
        ]

    /// Index into `makeKinds` of the two kinds that put a reference into the type, so that a
    /// generated field list can be forced onto the auto-layout path.
    let private referenceKindIndices = [ 6 ; 8 ]

    /// A field list guaranteed to contain at least one reference, so it takes the auto-layout
    /// path, at a random position so the reference is not always first or last.
    let private genGcFields : Gen<GeneratedField list> =
        gen {
            let! count = Gen.choose (0, 6)
            let! kindIndices = Gen.listOfLength count (Gen.choose (0, 8))
            let! referenceKind = Gen.elements referenceKindIndices
            let! position = Gen.choose (0, count)

            let indices =
                List.truncate position kindIndices
                @ [ referenceKind ]
                @ List.skip position kindIndices

            return indices |> List.mapi (fun i kind -> (makeKinds i).[kind])
        }

    /// `Pack` and `Size` values to sweep. Auto layout must ignore every one of them.
    let private genLayout : Gen<Layout> =
        Gen.oneof
            [
                Gen.constant Layout.Default
                gen {
                    let! packing = Gen.elements [ 0 ; 1 ; 2 ; 4 ; 8 ; 16 ]
                    let! size = Gen.elements [ 0 ; 1 ; 3 ; 9 ; 64 ]
                    return Layout.Custom (size = size, packingSize = packing)
                }
            ]

    [<Test>]
    let ``Auto layout agrees with an independent transcription of HandleAutoLayout`` () : unit =
        let property (generated : GeneratedField list) (layout : Layout) : unit =
            let vt = ofFields layout (generated |> List.map _.Field)
            let shapes = generated |> List.map _.Shape
            let expectedOffsets, _ = AutoLayoutOracle.place shapes

            (CliValueType.SizeOf vt).Size |> shouldEqual (AutoLayoutOracle.size shapes)

            for entry in generated do
                let expectedOffset = expectedOffsets.[entry.Shape.Name]

                match tryFieldAt expectedOffset entry.Shape.Size vt with
                | None ->
                    failwith
                        $"expected %s{entry.Shape.Name} at offset %d{expectedOffset} with size %d{entry.Shape.Size}, but nothing of that size is there"
                | Some contents -> contents |> shouldEqual entry.Field.Contents

        Prop.forAll (Arb.fromGen (Gen.zip genGcFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``The oracle places fields without overlap and inside the type`` () : unit =
        // Guards the guard: the property above is only as good as the transcription it compares
        // against, and a transcription that overlapped two fields would happily "agree" with an
        // implementation that did the same.
        let property (generated : GeneratedField list) : unit =
            let shapes = generated |> List.map _.Shape
            let offsets, _ = AutoLayoutOracle.place shapes
            let total = AutoLayoutOracle.size shapes

            let intervals =
                shapes |> List.map (fun s -> offsets.[s.Name], s.Size) |> List.sortBy fst

            for offset, size in intervals do
                if offset < 0 then
                    failwith $"negative offset %d{offset}"

                if offset + size > total then
                    failwith $"field at %d{offset} of size %d{size} runs past the type's %d{total} bytes"

            intervals
            |> List.pairwise
            |> List.iter (fun ((offsetA, sizeA), (offsetB, _)) ->
                if offsetA + sizeA > offsetB then
                    failwith $"field at %d{offsetA} of size %d{sizeA} overlaps the field at %d{offsetB}"
            )

        Prop.forAll (Arb.fromGen genGcFields) property |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Pack and Size are inert once a value type holds a reference`` () : unit =
        // The direct statement of CoreCLR's rule: a GC-containing type is routed to auto layout,
        // and auto layout reads neither the packing request nor the explicit size. This compares
        // the implementation against itself under varying inputs, so unlike the oracle property
        // it stays meaningful even if both transcriptions were wrong in the same way.
        let property (generated : GeneratedField list) (layouts : Layout list) : unit =
            let fields = generated |> List.map _.Field
            let baseline = ofFields Layout.Default fields
            let baselineFingerprint = fingerprint baseline

            for layout in layouts do
                let candidate = ofFields layout fields

                (CliValueType.SizeOf candidate) |> shouldEqual (CliValueType.SizeOf baseline)
                fingerprint candidate |> shouldEqual baselineFingerprint

        Prop.forAll (Arb.fromGen (Gen.zip genGcFields (Gen.listOfLength 4 genLayout))) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``A value type holding references is pointer-sized and pointer-aligned`` () : unit =
        // What `writeArrayBytes` relies on when it derives a clear length as
        // `byteLength / sizeof(IntPtr)`: if a GC-containing element were not a whole number of
        // pointer slots, the derived count would truncate and leave part of the element set.
        // The sweep over layouts also covers the explicit-layout case, where an explicit `Size`
        // is honoured as a floor and so could otherwise leave the type an odd length.
        let property (generated : GeneratedField list) (layout : Layout) : unit =
            let vt = ofFields layout (generated |> List.map _.Field)
            let size = CliValueType.SizeOf vt

            size.Size % NATIVE_INT_SIZE |> shouldEqual 0
            size.Alignment |> shouldEqual NATIVE_INT_SIZE

            // No reference straddles a pointer boundary, so no pointer-slot walk can ever cut
            // one in half.
            for offset in 0 .. size.Size - 1 do
                if offset % NATIVE_INT_SIZE <> 0 then
                    match tryFieldAt offset NATIVE_INT_SIZE vt with
                    | Some (CliType.ObjectRef _) -> failwith $"a reference is at misaligned offset %d{offset}"
                    | _ -> ()

        Prop.forAll (Arb.fromGen (Gen.zip genGcFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Sequential layout still honours Pack when no reference is present`` () : unit =
        // The counterpart guard: none of the above may leak into types CoreCLR leaves on the
        // sequential path. `[Pack = 1] struct { byte; long; }` stays 9 bytes with the long at 1.
        let vt =
            ofFields
                (Layout.Custom (size = 0, packingSize = 1))
                [
                    cliField "b" (CliType.Numeric (CliNumericType.UInt8 3uy)) None
                    cliField "l" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 7L))) None
                ]

        (CliValueType.SizeOf vt).Size |> shouldEqual 9

        tryFieldAt 1 8 vt
        |> shouldEqual (Some (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 7L))))

    [<Test>]
    let ``An explicit Size floor applies before the pointer rounding`` () : unit =
        // Explicit layout is not switched to auto layout, so `Size` is honoured — but a
        // GC-containing type still ends on a pointer boundary, which makes the order of the two
        // operations observable: `Size = 9` over a reference is 16 bytes, not 9.
        let withRef =
            ofFields (Layout.Custom (size = 9, packingSize = 0)) [ cliField "r" (CliType.ObjectRef None) (Some 0) ]

        (CliValueType.SizeOf withRef).Size |> shouldEqual 16

        // The same floor over a non-reference field is honoured exactly.
        let withoutRef =
            ofFields
                (Layout.Custom (size = 9, packingSize = 0))
                [
                    cliField "l" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (Some 0)
                ]

        (CliValueType.SizeOf withoutRef).Size |> shouldEqual 9

    [<Test>]
    let ``A reference-holding value type reports pointer alignment, not its widest field's`` () : unit =
        // CoreCLR takes `minAlign = containsGCPointers ? TARGET_POINTER_SIZE : largestAlignmentRequirement`
        // for a value class larger than a pointer. Without the GC arm, a type holding both a
        // reference and a wider-aligned field would demand that wider alignment of everything
        // containing it. The real-runtime shape this protects is `struct { object; Int128; }`;
        // `Int128`'s own 16-byte alignment requirement is a separate gap (parked as
        // `StructLayoutInt128Alignment.cs`), so this pins the alignment rule on its own.
        let wide =
            ofFields
                Layout.Default
                [
                    cliField "l" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (Some 0)
                    cliField "r" (CliType.ObjectRef None) (Some 8)
                ]

        (CliValueType.SizeOf wide).Alignment |> shouldEqual NATIVE_INT_SIZE
