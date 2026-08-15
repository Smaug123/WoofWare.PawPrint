namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `CliValueType.TryComputeMarshalLayout` is the single walk that decides both how big a struct's
/// unmanaged image is and where each field lands inside it. `TryComputeMarshalSize` is that walk
/// with the placements dropped, and the struct-marshal stub is that walk with the placements
/// kept, so a slip that moved a field without moving the total would be invisible to any
/// size-only check — which is exactly why these properties are per-field.
///
/// The strongest of them is the prefix property: sequential layout is defined by a left fold, so
/// removing trailing fields must not disturb the placements of the fields that remain. That is
/// the invariant a fold whose recorded offset drifts from its running cursor would break, and it
/// is checked against the implementation's own output rather than against a transcription of the
/// same arithmetic (which would only re-check the transcription).
[<TestFixture>]
module TestMarshalLayout =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    /// `Corelib.concretizeAll` covers the base types the interpreter needs at startup, which does
    /// not include `System.DateTime` — so concretize that one explicitly rather than widening the
    /// startup set for a test's benefit.
    let private allCt, dateTimeHandle: AllConcreteTypes * ConcreteTypeHandle =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes =
                    Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loaded
                TypeConcretization.ConcretizationContext.BaseTypes = bct
            }

        let stk =
            DumpedAssembly.signatureTypeKind ctx.BaseTypes ctx.LoadedAssemblies bct.DateTime

        let handle, ctx =
            TypeConcretization.concretizeType
                ctx
                IAssemblyLoad.alreadyLoadedOnly
                bct.DateTime.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (bct.DateTime.Identity, stk))

        ctx.ConcreteTypes, handle

    let private handleOf (t : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt t

    /// The nominal declaring type of every generated struct. `TryComputeMarshalLayout` consults it
    /// only for the AutoLayout gate, so any non-AutoLayout corelib struct will do; the test below
    /// pins that this one really is non-AutoLayout, because if that ever changed every property
    /// here would pass vacuously on a `NotMarshalable` result.
    let private declaredHandle : ConcreteTypeHandle = handleOf bct.TypedReference

    let private cliFieldAt
        (name : string)
        (contents : CliType)
        (ty : ConcreteTypeHandle)
        (offset : int option)
        : CliField
        =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = offset
            Type = ty
            MarshallingDescriptor = None
        }

    let private cliField (name : string) (contents : CliType) (ty : ConcreteTypeHandle) : CliField =
        cliFieldAt name contents ty None

    /// A `System.DateTime`-typed field: structurally one `ulong _dateData`, but declared as
    /// corelib's `DateTime`, which is what `IsHostKnownDateTime` keys on.
    let private dateTimeValue : CliType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            dateTimeHandle
            Layout.Default
            CharSet.Ansi
            [
                cliField
                    "_dateData"
                    (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                    (handleOf bct.UInt64)
            ]
        |> CliType.ValueType

    /// The field kinds the sweep draws from, with the unmanaged size and alignment each is
    /// expected to contribute. `DateTime` is the interesting one: CoreCLR marshals a `DateTime`
    /// *field* as an 8-byte OADate double (`MARSHAL_TYPE_DATE`, mlinfo.cpp:1747) rather than as
    /// its managed `_dateData` image, and the sizes coincide only by luck — the alignment claim
    /// is what places it.
    let private fieldKinds : (string * (int -> CliType) * ConcreteTypeHandle * int) list =
        [
            "u8", (fun i -> CliType.Numeric (CliNumericType.UInt8 (byte i))), handleOf bct.Byte, 1
            "i16", (fun i -> CliType.Numeric (CliNumericType.Int16 (int16 i))), handleOf bct.Int16, 2
            "i32", (fun i -> CliType.Numeric (CliNumericType.Int32 i)), handleOf bct.Int32, 4
            "i64",
            (fun i -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (int64 i)))),
            handleOf bct.Int64,
            8
            "f32", (fun i -> CliType.Numeric (CliNumericType.Float32 (float32 i))), handleOf bct.Single, 4
            "f64", (fun i -> CliType.Numeric (CliNumericType.Float64 (float i))), handleOf bct.Double, 8
            "date", (fun _ -> dateTimeValue), dateTimeHandle, 8
        ]

    /// One generated field: what the implementation is given, and the unmanaged width/alignment
    /// the properties expect it to contribute.
    type private GeneratedField =
        {
            Field : CliField
            NativeWidth : int
        }

    let private genFields : Gen<GeneratedField list> =
        gen {
            let! count = Gen.choose (1, 6)
            let! kinds = Gen.listOfLength count (Gen.elements fieldKinds)

            return
                kinds
                |> List.mapi (fun i (kindName, make, ty, width) ->
                    {
                        Field = cliField $"f%d{i}_%s{kindName}" (make (i + 1)) ty
                        NativeWidth = width
                    }
                )
        }

    /// Explicit-layout fields: every field carries a `[FieldOffset]`, and the offsets are
    /// deliberately *not* in declaration order.
    ///
    /// The explicit arm of the layout fold is a different code path from the sequential one — it
    /// ignores the running cursor and records the declared offset — and none of the sequential
    /// properties can reach it, because they only ever generate `Offset = None`. Shuffling the
    /// offsets is what makes the coverage bite: an arm that fell back to the aligned cursor, or
    /// that read a neighbouring field's offset, would produce ascending offsets and agree on the
    /// total, so only the declared-offset comparison below catches it.
    let private genExplicitFields : Gen<GeneratedField list> =
        gen {
            let! count = Gen.choose (1, 5)
            let! kinds = Gen.listOfLength count (Gen.elements fieldKinds)
            // Distinct 8-byte slots, so no two fields overlap whatever widths are drawn.
            let! slots = Gen.shuffle [ 0 .. count - 1 ]

            return
                List.zip kinds (List.ofArray slots)
                |> List.mapi (fun i ((kindName, make, ty, width), slot) ->
                    {
                        Field = cliFieldAt $"f%d{i}_%s{kindName}" (make (i + 1)) ty (Some (slot * 8))
                        NativeWidth = width
                    }
                )
        }

    /// `Pack` values to sweep, plus the explicit `Size` floor. Packing caps each field's
    /// alignment, so it is the knob that most easily desynchronises a placement from its cursor.
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

    let private ofFields (layout : Layout) (fields : CliField list) : CliValueType =
        SynthesisedLayoutKind.ofFields bct allCt declaredHandle layout CharSet.Ansi fields

    let private layoutOf (layout : Layout) (fields : GeneratedField list) : SizeofResult * MarshalFieldPlacement list =
        let vt = ofFields layout (fields |> List.map _.Field)

        match CliValueType.TryComputeMarshalLayout allCt loaded bct vt with
        | Result.Ok result -> result
        | Result.Error err -> failwith $"expected a marshal layout, got %s{err.Reason}"

    let private packingOf (layout : Layout) : int =
        match layout with
        | Layout.Default -> 8
        | Layout.Custom (packingSize = 0) -> 8
        | Layout.Custom (packingSize = p) -> p

    [<Test>]
    let ``The nominal declaring type is not AutoLayout`` () : unit =
        // Guards every other property in this file: an AutoLayout declaring type makes
        // TryComputeMarshalLayout return NotMarshalable, and `layoutOf` would then fail rather
        // than quietly pass, but stating it once here names the assumption.
        CliValueType.IsAutoLayoutHandle allCt loaded declaredHandle |> shouldEqual false

    [<Test>]
    let ``Every declared field is placed exactly once, in declaration order`` () : unit =
        let property (fields : GeneratedField list) (layout : Layout) : unit =
            let _, placements = layoutOf layout fields

            placements
            |> List.map _.Field.Name
            |> shouldEqual (fields |> List.map _.Field.Name)

        Prop.forAll (Arb.fromGen (Gen.zip genFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Each field is placed at its own natural alignment, capped by Pack`` () : unit =
        let property (fields : GeneratedField list) (layout : Layout) : unit =
            let _, placements = layoutOf layout fields
            let packing = packingOf layout

            for generated, placement in List.zip fields placements do
                placement.NativeSize.Size |> shouldEqual generated.NativeWidth

                let cap = min placement.NativeSize.Alignment packing

                if placement.NativeOffset % cap <> 0 then
                    failwith
                        $"field %s{placement.Field.Name} is at offset %d{placement.NativeOffset}, which is not a multiple of its %d{cap}-byte alignment cap"

        Prop.forAll (Arb.fromGen (Gen.zip genFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Sequential placements ascend, do not overlap, and fit inside the image`` () : unit =
        let property (fields : GeneratedField list) (layout : Layout) : unit =
            let size, placements = layoutOf layout fields

            let mutable previousEnd = 0

            for placement in placements do
                if placement.NativeOffset < previousEnd then
                    failwith
                        $"field %s{placement.Field.Name} at offset %d{placement.NativeOffset} overlaps the field before it, which ended at %d{previousEnd}"

                previousEnd <- placement.NativeOffset + placement.NativeSize.Size

                if previousEnd > size.Size then
                    failwith
                        $"field %s{placement.Field.Name} runs to %d{previousEnd}, past the type's %d{size.Size}-byte unmanaged image"

        Prop.forAll (Arb.fromGen (Gen.zip genFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Dropping trailing fields leaves the remaining placements untouched`` () : unit =
        // The load-bearing one. Sequential layout is a left fold, so the placement of field k
        // depends only on fields 0..k-1. An implementation whose recorded offset drifted from the
        // cursor it advances would still agree with itself on the total for a fixed field list,
        // but could not survive this.
        let property (fields : GeneratedField list) (layout : Layout) : unit =
            let _, whole = layoutOf layout fields

            for k in 1 .. fields.Length do
                let _, prefix = layoutOf layout (List.truncate k fields)

                prefix
                |> List.map (fun p -> p.Field.Name, p.NativeOffset, p.NativeSize)
                |> shouldEqual (
                    whole
                    |> List.truncate k
                    |> List.map (fun p -> p.Field.Name, p.NativeOffset, p.NativeSize)
                )

        Prop.forAll (Arb.fromGen (Gen.zip genFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``An explicitly-laid-out field is placed at the offset it declares`` () : unit =
        let property (fields : GeneratedField list) (layout : Layout) : unit =
            let size, placements = layoutOf layout fields

            placements
            |> List.map (fun p -> p.Field.Name, p.NativeOffset)
            |> shouldEqual (fields |> List.map (fun f -> f.Field.Name, f.Field.Offset |> Option.get))

            for generated, placement in List.zip fields placements do
                placement.NativeSize.Size |> shouldEqual generated.NativeWidth

                if placement.NativeOffset + placement.NativeSize.Size > size.Size then
                    failwith
                        $"field %s{placement.Field.Name} runs to %d{placement.NativeOffset + placement.NativeSize.Size}, past the type's %d{size.Size}-byte unmanaged image"

        Prop.forAll (Arb.fromGen (Gen.zip genExplicitFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``The size-only entry point agrees with the layout it drops`` () : unit =
        let property (fields : GeneratedField list) (layout : Layout) : unit =
            let vt = ofFields layout (fields |> List.map _.Field)
            let fromLayout, _ = layoutOf layout fields

            CliValueType.TryComputeMarshalSize allCt loaded bct vt
            |> shouldEqual (Result.Ok fromLayout)

        Prop.forAll (Arb.fromGen (Gen.zip genFields genLayout)) (fun (f, l) -> property f l)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``A declared Size suppresses the native alignment rounding`` () : unit =
        // Native layout takes a declared `ClassLayout.Size` through the *same* helper the managed
        // layout does: `CollectNativeLayoutFieldMetadataThrowing` calls
        // `CalculateSizeWithMetadataSize` when the type `HasExplicitSize()` and `AlignSize`
        // otherwise (classlayoutinfo.cpp:939-977). So the floor and the rounding are alternatives
        // here too, and the sweep above cannot see it: `genLayout` only ever draws a `Size` that
        // is either 0 or larger than the fields need, where the two orderings agree.
        let fields =
            [
                {
                    Field =
                        cliField
                            "l"
                            (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 1L)))
                            (handleOf bct.Int64)
                    NativeWidth = 8
                }
                {
                    Field = cliField "i" (CliType.Numeric (CliNumericType.Int32 2)) (handleOf bct.Int32)
                    NativeWidth = 4
                }
            ]

        // Fields end at 12 and demand 8-byte alignment, so with no declared size this rounds to 16.
        (fst (layoutOf Layout.Default fields)).Size |> shouldEqual 16

        // A declared size between the two suppresses the rounding entirely.
        (fst (layoutOf (Layout.Custom (size = 13, packingSize = 0)) fields)).Size
        |> shouldEqual 13

        // One below the fields loses to them rather than truncating them.
        (fst (layoutOf (Layout.Custom (size = 4, packingSize = 0)) fields)).Size
        |> shouldEqual 12

    [<Test>]
    let ``A DateTime field claims eight bytes at eight-byte alignment`` () : unit =
        // Pins `MARSHAL_TYPE_DATE` at the layout level, independently of the sweep: the native
        // form is an OADate double, so `{ int; DateTime }` puts the date at offset 8 and is 16
        // bytes — not the 12 bytes a naive 4-aligned copy of the managed `_dateData` would give.
        let size, placements =
            layoutOf
                Layout.Default
                [
                    {
                        Field = cliField "Id" (CliType.Numeric (CliNumericType.Int32 7)) (handleOf bct.Int32)
                        NativeWidth = 4
                    }
                    {
                        Field = cliField "When" dateTimeValue dateTimeHandle
                        NativeWidth = 8
                    }
                ]

        placements |> List.map _.NativeOffset |> shouldEqual [ 0 ; 8 ]

        placements
        |> List.map _.NativeSize
        |> shouldEqual
            [
                {
                    Size = 4
                    Alignment = 4
                }
                {
                    Size = 8
                    Alignment = 8
                }
            ]

        size
        |> shouldEqual
            {
                Size = 16
                Alignment = 8
            }
