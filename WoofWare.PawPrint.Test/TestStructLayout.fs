namespace WoofWare.PawPrint.Test

open System.Reflection
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

    /// `largestAlignmentRequirement` from the placement loop (methodtablebuilder.cpp:8500-8562).
    /// Note the asymmetry: a *value class* field contributes its own alignment, but anything else
    /// — including a one-byte primitive — contributes the pointer size, because "non-value-type
    /// fields always require pointer alignment" (:8554).
    let private largestAlignmentRequirement (fields : FieldShape list) : int =
        (1, fields)
        ||> List.fold (fun acc f ->
            let required =
                if not f.IsValueClass then 8
                elif f.ContainsReferences then 8
                else f.Alignment

            max acc required
        )

    /// The size and alignment CoreCLR reports for a value class (methodtablebuilder.cpp:8566-8605):
    /// a zero-length type is padded to one byte, then "the JITs like to copy full machine words,
    /// so if the size is bigger than a void* round it up to minAlign and if the size is smaller
    /// than void* round it up to next power of two".
    ///
    /// The reported alignment is that same `minAlign`: it is recorded as the type's custom field
    /// alignment exactly when it differs from `min(size, pointer)`, which is what
    /// `MethodTable::GetFieldAlignmentRequirement` (methodtable.cpp:8853) would otherwise return.
    let finalise (fields : FieldShape list) : int * int =
        let _, finalEnd = place fields
        let unrounded = max 1 finalEnd

        let minAlign =
            if unrounded > 8 then
                if fields |> List.exists (fun f -> f.ContainsReferences) then
                    8
                else
                    largestAlignmentRequirement fields
            else
                let mutable candidate = 1

                while candidate < unrounded do
                    candidate <- candidate * 2

                candidate

        roundUp minAlign unrounded, minAlign

    let size (fields : FieldShape list) : int = fst (finalise fields)

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

    let private ofFieldsWithKind
        (layoutKind : TypeLayoutKind)
        (layout : Layout)
        (fields : CliField list)
        : CliValueType
        =
        let facts : DeclaredTypeFacts =
            {
                IsValueType = true
                IsEnum = false
                NominalAlignment = None
                LayoutKind = layoutKind
                Layout = layout
                CharSet = CharSet.Ansi
            }

        CliValueType.OfFields bct allCt declaredHandle facts fields

    /// The overwhelmingly common shape in this file: a type whose metadata declares
    /// `LayoutKind.Sequential`, which reaches auto layout only if it holds a reference.
    let private ofFields (layout : Layout) (fields : CliField list) : CliValueType =
        ofFieldsWithKind TypeLayoutKind.Sequential layout fields

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

        // A struct of three bytes: 3 bytes, 1-aligned, no references — the only generated field
        // whose alignment is narrower than a pointer. Without it, every field list either is
        // empty or contains something pointer-aligned, so `largestAlignmentRequirement` is always
        // 8 and an implementation that ignored it entirely would agree with the oracle on every
        // case. `[Auto] struct { S3 x, y, z; }` is 9 bytes with alignment 1 on real .NET.
        let narrowStruct =
            ofFields
                Layout.Default
                [
                    cliField "p" (CliType.Numeric (CliNumericType.UInt8 (byte marker))) None
                    cliField "q" (CliType.Numeric (CliNumericType.UInt8 0uy)) None
                    cliField "r" (CliType.Numeric (CliNumericType.UInt8 0uy)) None
                ]

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
            valueClass narrowStruct false
        ]

    /// The number of distinct field kinds `makeKinds` offers, as an inclusive upper bound for
    /// `Gen.choose`.
    let private maxKindIndex = 9

    /// Index into `makeKinds` of the two kinds that put a reference into the type, so that a
    /// generated field list can be forced onto the auto-layout path.
    let private referenceKindIndices = [ 6 ; 8 ]

    /// Index into `makeKinds` of the kinds that put *no* reference into the type, so that a
    /// generated field list can be kept off the GC-promotion path and reach auto layout only by
    /// declaring it.
    let private plainKindIndices = [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 7 ; 9 ]

    /// A field list guaranteed to contain at least one reference, so it takes the auto-layout
    /// path, at a random position so the reference is not always first or last.
    let private genGcFields : Gen<GeneratedField list> =
        gen {
            let! count = Gen.choose (0, 6)
            let! kindIndices = Gen.listOfLength count (Gen.choose (0, maxKindIndex))
            let! referenceKind = Gen.elements referenceKindIndices
            let! position = Gen.choose (0, count)

            let indices =
                List.truncate position kindIndices
                @ [ referenceKind ]
                @ List.skip position kindIndices

            return indices |> List.mapi (fun i kind -> (makeKinds i).[kind])
        }

    /// A field list guaranteed to contain *no* reference, so the GC promotion cannot fire and the
    /// only route into auto layout is the declared kind. May be empty.
    let private genPlainFields : Gen<GeneratedField list> =
        gen {
            let! count = Gen.choose (0, 6)
            let! kindIndices = Gen.listOfLength count (Gen.elements plainKindIndices)
            return kindIndices |> List.mapi (fun i kind -> (makeKinds i).[kind])
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

    /// The shared body of the two oracle properties: whatever route took this type to auto
    /// layout, its size, its alignment and every field's offset must be the ones the
    /// transcription computes.
    let private agreesWithOracle (vt : CliValueType) (generated : GeneratedField list) : unit =
        let shapes = generated |> List.map _.Shape
        let expectedOffsets, _ = AutoLayoutOracle.place shapes
        let expectedSize, expectedAlignment = AutoLayoutOracle.finalise shapes
        let actual = CliValueType.SizeOf vt

        actual.Size |> shouldEqual expectedSize
        // Asserted separately from the size because it is a separate rule: the alignment a type
        // presents to whatever contains it is `minAlign`, which for a type of all-narrow value
        // classes is *not* the pointer size the size rounding might suggest.
        actual.Alignment |> shouldEqual expectedAlignment

        for entry in generated do
            let expectedOffset = expectedOffsets.[entry.Shape.Name]

            match tryFieldAt expectedOffset entry.Shape.Size vt with
            | None ->
                failwith
                    $"expected %s{entry.Shape.Name} at offset %d{expectedOffset} with size %d{entry.Shape.Size}, but nothing of that size is there"
            | Some contents -> contents |> shouldEqual entry.Field.Contents

    [<Test>]
    let ``Auto layout agrees with an independent transcription of HandleAutoLayout`` () : unit =
        // The GC-promotion route: the metadata says `Sequential`, but a reference in the type
        // sends it to auto layout anyway.
        let property (generated : GeneratedField list) (layout : Layout) : unit =
            agreesWithOracle (ofFields layout (generated |> List.map _.Field)) generated

        Prop.forAll (Arb.fromGen (Gen.zip genGcFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``A declared-Auto type without references is laid out by auto layout too`` () : unit =
        // The other route into `HandleAutoLayout` (`PlaceInstanceFields`,
        // methodtablebuilder.cpp:8212): the type declares `LayoutKind.Auto`. Nothing in these
        // field lists holds a reference, so the promotion rule cannot fire and the declared kind
        // is the only thing that can send them here — which is exactly what
        // `StructLayoutAutoWithoutReferences.cs` observes from a guest.
        let property (generated : GeneratedField list) (layout : Layout) : unit =
            agreesWithOracle (ofFieldsWithKind TypeLayoutKind.Auto layout (generated |> List.map _.Field)) generated

        Prop.forAll (Arb.fromGen (Gen.zip genPlainFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``A declared-Sequential type without references keeps declared order`` () : unit =
        // The control for the property above, and the one that fails if the layout kind is
        // ignored: the same reference-free field lists under `Sequential` must be placed in
        // declared order at their natural alignments, not bucketed. Stated against a direct
        // transcription of the sequential rule rather than against the auto oracle, so that
        // "these two routes differ" is asserted by construction.
        let property (generated : GeneratedField list) : unit =
            let vt =
                ofFieldsWithKind TypeLayoutKind.Sequential Layout.Default (generated |> List.map _.Field)

            let mutable cursor = 0

            for entry in generated do
                let offset =
                    let error = cursor % entry.Shape.Alignment

                    if error = 0 then
                        cursor
                    else
                        cursor + (entry.Shape.Alignment - error)

                cursor <- offset + entry.Shape.Size

                match tryFieldAt offset entry.Shape.Size vt with
                | None ->
                    failwith
                        $"expected %s{entry.Shape.Name} at declared-order offset %d{offset} with size %d{entry.Shape.Size}, but nothing of that size is there"
                | Some contents -> contents |> shouldEqual entry.Field.Contents

        Prop.forAll (Arb.fromGen genPlainFields) property |> Check.QuickThrowOnFailure

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
    let ``Pack and Size are inert once a value type declares LayoutKind.Auto`` () : unit =
        // The sibling of the property above for the other route into auto layout, and the reason
        // the layout kind has to reach the *sizing* code and not only the placement code: a
        // declared `Size` is a floor only where something reads it, and auto layout does not
        // (`HasLayoutMetadata` is false for an AutoLayout type, so `GetClassTotalSize` is never
        // consulted). The generator emits empty field lists too, which is the sharpest case:
        // `[StructLayout(LayoutKind.Auto, Size = 64)] struct Empty {}` is legal C# and is one
        // byte on real .NET.
        let property (generated : GeneratedField list) (layouts : Layout list) : unit =
            let fields = generated |> List.map _.Field
            let baseline = ofFieldsWithKind TypeLayoutKind.Auto Layout.Default fields
            let baselineFingerprint = fingerprint baseline

            for layout in layouts do
                let candidate = ofFieldsWithKind TypeLayoutKind.Auto layout fields

                (CliValueType.SizeOf candidate) |> shouldEqual (CliValueType.SizeOf baseline)
                fingerprint candidate |> shouldEqual baselineFingerprint

        Prop.forAll (Arb.fromGen (Gen.zip genPlainFields (Gen.listOfLength 4 genLayout))) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``A declared-Sequential type does read Pack and Size`` () : unit =
        // Guards the guard above: "inert" is only a claim about auto layout, and would be
        // vacuous if `Pack`/`Size` were inert everywhere. A `Size` floor over a single byte field
        // is honoured under `Sequential` and discarded under `Auto`.
        let field = [ cliField "b" (CliType.Numeric (CliNumericType.UInt8 7uy)) None ]
        let layout = Layout.Custom (size = 24, packingSize = 0)

        (CliValueType.SizeOf (ofFieldsWithKind TypeLayoutKind.Sequential layout field)).Size
        |> shouldEqual 24

        (CliValueType.SizeOf (ofFieldsWithKind TypeLayoutKind.Auto layout field)).Size
        |> shouldEqual 1

    [<Test>]
    let ``The layout kind is projected from TypeAttributes.LayoutMask`` () : unit =
        // `AutoLayout` is the zero bit pattern, so a type carrying no `[StructLayout]` at all
        // reports it (ECMA §II.10.1.2). Sweeping the unrelated bits pins that the mask is applied
        // rather than the whole attribute value compared.
        for noise in
            [
                TypeAttributes.Public
                TypeAttributes.Sealed
                TypeAttributes.UnicodeClass
                TypeAttributes.BeforeFieldInit
            ] do
            TypeLayoutKind.ofTypeAttributes (TypeAttributes.AutoLayout ||| noise)
            |> shouldEqual TypeLayoutKind.Auto

            TypeLayoutKind.ofTypeAttributes (TypeAttributes.SequentialLayout ||| noise)
            |> shouldEqual TypeLayoutKind.Sequential

            TypeLayoutKind.ofTypeAttributes (TypeAttributes.ExplicitLayout ||| noise)
            |> shouldEqual TypeLayoutKind.Explicit

        // The fourth bit pattern is not a legal LayoutMask value and CoreCLR refuses to load such
        // a type, so we refuse to guess at one rather than pick a layout it never had.
        let exn =
            Assert.Throws<exn> (fun () -> TypeLayoutKind.ofTypeAttributes TypeAttributes.LayoutMask |> ignore)

        exn.Message |> shouldContainText "not one of AutoLayout"

    [<Test>]
    let ``The declared layout kind is honoured for reference types too`` () : unit =
        // `TypeLayoutKind.applied` used to report `Sequential` for a declared-`Auto` reference
        // type -- which is every C# class. That was a holding position for the base-chain
        // flattening: laying a whole chain out in one pass and then bucketing it would have sorted
        // inherited fields in among the derived type's own, so honouring the declared kind would
        // have traded one infidelity for another.
        //
        // Layout is per-declaring-type now (issue #994), so the suppression is gone and `applied`
        // with it. `TestBaseChainLayout` is what holds the replacement in place, against real
        // .NET: `class Mixed { byte B; int I; long L; short S; }` is bucketed to `L@0 I@8 S@12
        // B@14` there, which is the row this gate used to get wrong.
        for attrs, expected in
            [
                TypeAttributes.AutoLayout, TypeLayoutKind.Auto
                TypeAttributes.SequentialLayout, TypeLayoutKind.Sequential
                TypeAttributes.ExplicitLayout, TypeLayoutKind.Explicit
            ] do
            TypeLayoutKind.ofTypeAttributes attrs |> shouldEqual expected

    [<Test>]
    let ``A declared-Auto type carrying field offsets is rejected`` () : unit =
        // Explicit layout is read off the fields, so a declared-`Auto` type that carries offsets
        // would silently be laid out explicitly rather than by the algorithm its kind names. That
        // combination cannot arise from the base-chain flattening — `applied` reports `Auto` only
        // for value types, which inherit no instance fields — so it is malformed input, and is
        // refused rather than reinterpreted.
        let withOffsets =
            [
                cliField "a" (CliType.Numeric (CliNumericType.Int32 1)) (Some 0)
                cliField "b" (CliType.Numeric (CliNumericType.Int32 2)) (Some 4)
            ]

        let withoutOffsets =
            [
                cliField "a" (CliType.Numeric (CliNumericType.Int32 1)) None
                cliField "b" (CliType.Numeric (CliNumericType.Int32 2)) None
            ]

        let exn =
            Assert.Throws<exn> (fun () -> ofFieldsWithKind TypeLayoutKind.Auto Layout.Default withOffsets |> ignore)

        exn.Message |> shouldContainText "carry a FieldOffset"

        // Every other combination of kind and field shape is reachable, because a reference type's
        // flattened base chain mixes fields governed by different kinds — see
        // `LayoutKindAcrossInheritance.cs`, where an explicit-layout class presents only its
        // sequential base's offset-free fields. They are laid out, not refused.
        for kind in [ TypeLayoutKind.Explicit ; TypeLayoutKind.Sequential ] do
            ofFieldsWithKind kind Layout.Default withOffsets |> ignore
            ofFieldsWithKind kind Layout.Default withoutOffsets |> ignore

        // A declared-`Explicit` type whose fields carry no offsets falls back to declared-order
        // placement, which is what it received before the layout kind was modelled at all.
        let fallback =
            ofFieldsWithKind TypeLayoutKind.Explicit Layout.Default withoutOffsets

        (CliValueType.SizeOf fallback).Size |> shouldEqual 8

        tryFieldAt 0 4 fallback
        |> shouldEqual (Some (CliType.Numeric (CliNumericType.Int32 1)))

        tryFieldAt 4 4 fallback
        |> shouldEqual (Some (CliType.Numeric (CliNumericType.Int32 2)))

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
    let ``A declared Size suppresses the alignment rounding`` () : unit =
        // A declared `ClassLayout.Size` and the alignment rounding are alternatives, not a
        // sequence: `managedSize = classSizeInMetadata <> 0 ? max(classSizeInMetadata, lastFieldEnd)
        // : AlignSize(lastFieldEnd, alignmentRequirement)` (classlayoutinfo.cpp:543-550).
        //
        // The two sibling tests here only ever exercise a floor *above* the rounded size, where
        // "round then floor" and "floor instead of round" agree, which is how the bug survived.
        // These are the cases where they disagree, in both directions.
        let fields =
            [
                cliField "l" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 1L))) None
                cliField "i" (CliType.Numeric (CliNumericType.Int32 2)) None
            ]

        // Fields end at 12 and demand 8-byte alignment, so with no declared size this rounds to 16.
        (CliValueType.SizeOf (ofFields Layout.Default fields)).Size |> shouldEqual 16

        // A declared size between the two suppresses the rounding entirely.
        (CliValueType.SizeOf (ofFields (Layout.Custom (size = 13, packingSize = 0)) fields)).Size
        |> shouldEqual 13

        // One below the fields loses to them, rather than truncating them or reinstating the
        // rounding: `max` picks the field extent, which is not a multiple of the alignment.
        (CliValueType.SizeOf (ofFields (Layout.Custom (size = 4, packingSize = 0)) fields)).Size
        |> shouldEqual 12

        // The alignment requirement is untouched by any of this, so a container still places one
        // of these on an 8-byte boundary however wide it turned out to be.
        for size in [ 0 ; 4 ; 13 ] do
            (CliValueType.SizeOf (ofFields (Layout.Custom (size = size, packingSize = 0)) fields)).Alignment
            |> shouldEqual 8

    [<Test>]
    let ``An explicit Size floor applies before the pointer rounding`` () : unit =
        // Explicit layout is not switched to auto layout, so `Size` is honoured — but a
        // GC-containing type still ends on a pointer boundary, which makes the order of the two
        // operations observable: `Size = 9` over a reference is 16 bytes, not 9.
        let withRef =
            ofFieldsWithKind
                TypeLayoutKind.Explicit
                (Layout.Custom (size = 9, packingSize = 0))
                [ cliField "r" (CliType.ObjectRef None) (Some 0) ]

        (CliValueType.SizeOf withRef).Size |> shouldEqual 16

        // The same floor over a non-reference field is honoured exactly.
        let withoutRef =
            ofFieldsWithKind
                TypeLayoutKind.Explicit
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
            ofFieldsWithKind
                TypeLayoutKind.Explicit
                Layout.Default
                [
                    cliField "l" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (Some 0)
                    cliField "r" (CliType.ObjectRef None) (Some 8)
                ]

        (CliValueType.SizeOf wide).Alignment |> shouldEqual NATIVE_INT_SIZE
