namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `CliType.TrySingleWholeValueField` decides whether a value type is *transparent*: whether it
/// holds exactly one field, at offset 0, spanning the whole value. Byref reinterpret elision is
/// justified by that answer, so these tests pin both directions — in particular that shapes which
/// merely *contain* a reference at offset 0 are rejected, since eliding those would silently drop
/// the rest of the struct.
[<TestFixture>]
module TestCliTypeSingleWholeField =

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

    let private int64Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int64

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Byte

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Object

    let private cliField
        (name : string)
        (contents : CliType)
        (offset : int option)
        (fieldType : ConcreteTypeHandle)
        : CliField
        =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = offset
            Type = fieldType
            MarshallingDescriptor = None
        }

    let private ofFields (fields : CliField list) : CliType =
        CliValueType.OfFields bct allCt declaredHandle Layout.Default CharSet.Ansi fields
        |> CliType.ValueType

    let private ofFieldsSized (size : int) (fields : CliField list) : CliType =
        CliValueType.OfFields
            bct
            allCt
            declaredHandle
            (Layout.Custom (size = size, packingSize = 0))
            CharSet.Ansi
            fields
        |> CliType.ValueType

    /// The `[InlineArray(N)] struct { T _item; }` shape with `T = string`, which is what
    /// `SegmentedArrayBuilder`'s `ScratchBuffer` is and what motivated this predicate.
    [<Test>]
    let ``single object-reference field spanning the whole value is transparent`` () : unit =
        let value = ofFields [ cliField "_item" (CliType.ObjectRef None) None objectHandle ]

        match CliType.TrySingleWholeValueField value with
        | None -> failwith "expected the single-reference-field wrapper to be transparent"
        | Some (id, contents) ->
            id |> shouldEqual (FieldId.named "_item")
            contents |> shouldEqual (CliType.ObjectRef None)

    [<Test>]
    let ``single primitive field spanning the whole value is transparent`` () : unit =
        let value =
            ofFields [ cliField "Value" (CliType.Numeric (CliNumericType.Int32 7)) None int32Handle ]

        match CliType.TrySingleWholeValueField value with
        | None -> failwith "expected the single-primitive-field wrapper to be transparent"
        | Some (id, contents) ->
            id |> shouldEqual (FieldId.named "Value")
            contents |> shouldEqual (CliType.Numeric (CliNumericType.Int32 7))

    /// The load-bearing negative. `struct { int N; Box B; }` has a reference in it, but the
    /// reference is not the whole struct, so reinterpreting the struct *as* `Box` must keep
    /// going through the byte-view path (which refuses) rather than being elided to field `B`.
    [<Test>]
    let ``value type mixing a primitive and a reference is not transparent`` () : unit =
        let value =
            ofFields
                [
                    cliField "N" (CliType.Numeric (CliNumericType.Int32 0)) None int32Handle
                    cliField "B" (CliType.ObjectRef None) None objectHandle
                ]

        CliType.TrySingleWholeValueField value |> shouldEqual None

    /// Two references is likewise not transparent: eliding to the first would strand the second.
    [<Test>]
    let ``value type holding two references is not transparent`` () : unit =
        let value =
            ofFields
                [
                    cliField "A" (CliType.ObjectRef None) None objectHandle
                    cliField "B" (CliType.ObjectRef None) None objectHandle
                ]

        CliType.TrySingleWholeValueField value |> shouldEqual None

    /// Explicit layout can alias two fields at offset 0. Eliding through either would leave the
    /// other stale on write, so overlap must be rejected even though both start at 0.
    [<Test>]
    let ``explicit-layout overlap at offset zero is not transparent`` () : unit =
        let value =
            ofFieldsSized
                4
                [
                    cliField "AsInt" (CliType.Numeric (CliNumericType.Int32 0)) (Some 0) int32Handle
                    cliField "Byte0" (CliType.Numeric (CliNumericType.UInt8 0uy)) (Some 0) byteHandle
                ]

        CliType.TrySingleWholeValueField value |> shouldEqual None

    /// Explicit layout can also alias from a *non-zero* offset: an 8-byte field at 0 and a 4-byte
    /// field at 4, in an 8-byte struct. The first field passes every offset-0 guard on its own, so
    /// only inspecting the whole field set catches the overlapping sibling that elision would
    /// strand.
    [<Test>]
    let ``explicit-layout overlap from a non-zero offset is not transparent`` () : unit =
        let value =
            ofFieldsSized
                8
                [
                    cliField
                        "AsLong"
                        (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                        (Some 0)
                        int64Handle
                    cliField "Upper" (CliType.Numeric (CliNumericType.Int32 0)) (Some 4) int32Handle
                ]

        CliType.TrySingleWholeValueField value |> shouldEqual None

    /// A field at offset 0 that does *not* span the value leaves trailing storage the reinterpret
    /// would silently discard.
    [<Test>]
    let ``single field with trailing padding is not transparent`` () : unit =
        let value =
            ofFieldsSized 16 [ cliField "Obj" (CliType.ObjectRef None) (Some 0) objectHandle ]

        CliType.TrySingleWholeValueField value |> shouldEqual None

    /// A field starting past offset 0 is not at the address the reinterpret would address.
    [<Test>]
    let ``single field at a non-zero offset is not transparent`` () : unit =
        let value =
            ofFieldsSized 16 [ cliField "Obj" (CliType.ObjectRef None) (Some 8) objectHandle ]

        CliType.TrySingleWholeValueField value |> shouldEqual None

    [<Test>]
    let ``value type with no fields is not transparent`` () : unit =
        CliType.TrySingleWholeValueField (ofFieldsSized 8 []) |> shouldEqual None

    [<Test>]
    let ``non-value-types are never transparent`` () : unit =
        CliType.TrySingleWholeValueField (CliType.ObjectRef None) |> shouldEqual None

        CliType.TrySingleWholeValueField (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual None

        CliType.TrySingleWholeValueField (CliType.Bool 0uy) |> shouldEqual None

    let private genFieldContents : Gen<CliType * ConcreteTypeHandle> =
        Gen.oneof
            [
                Gen.constant (CliType.ObjectRef None, objectHandle)
                ArbMap.defaults
                |> ArbMap.generate<int32>
                |> Gen.map (fun i -> CliType.Numeric (CliNumericType.Int32 i), int32Handle)
                ArbMap.defaults
                |> ArbMap.generate<byte>
                |> Gen.map (fun b -> CliType.Numeric (CliNumericType.UInt8 b), byteHandle)
            ]

    /// Whatever the field holds — reference or primitive — a sequential-layout wrapper of exactly
    /// one field is transparent, and hands back that same field. The elision is a structural
    /// property, not a property of the contents.
    [<Test>]
    let ``any single-field sequential wrapper is transparent and round-trips its field`` () : unit =
        let property (contents : CliType, handle : ConcreteTypeHandle) : bool =
            let value = ofFields [ cliField "F" contents None handle ]

            match CliType.TrySingleWholeValueField value with
            | Some (id, got) -> id = FieldId.named "F" && got = contents
            | None -> false

        property
        |> Prop.forAll (Arb.fromGen genFieldContents)
        |> Check.QuickThrowOnFailure

    /// Adding *any* second field destroys transparency, whatever the two fields hold. This is the
    /// property that keeps the classifier honest: it cannot be widened into "contains a reference
    /// at offset 0" without breaking it.
    [<Test>]
    let ``no two-field wrapper is ever transparent`` () : unit =
        let property ((c1, h1) : CliType * ConcreteTypeHandle, (c2, h2) : CliType * ConcreteTypeHandle) : bool =
            let value = ofFields [ cliField "F1" c1 None h1 ; cliField "F2" c2 None h2 ]

            CliType.TrySingleWholeValueField value |> Option.isNone

        property
        |> Prop.forAll (Arb.fromGen (Gen.zip genFieldContents genFieldContents))
        |> Check.QuickThrowOnFailure

    /// The sequential-layout property above cannot reach an *overlapping* second field, which is
    /// exactly the shape that slipped through an earlier version of this predicate: it only
    /// consulted fields starting at offset 0, so an 8-byte field at 0 plus a 4-byte field at any
    /// later offset looked like a lone whole-value field. Sweep the second field across every
    /// offset it can occupy — overlapping and not — and require rejection throughout.
    [<Test>]
    let ``no explicit-layout two-field wrapper is transparent at any second-field offset`` () : unit =
        let property (secondOffset : int) : bool =
            let value =
                ofFieldsSized
                    8
                    [
                        cliField
                            "First"
                            (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                            (Some 0)
                            int64Handle
                        cliField "Second" (CliType.Numeric (CliNumericType.Int32 0)) (Some secondOffset) int32Handle
                    ]

            CliType.TrySingleWholeValueField value |> Option.isNone

        property
        |> Prop.forAll (Arb.fromGen (Gen.choose (0, 4)))
        |> Check.QuickThrowOnFailure
