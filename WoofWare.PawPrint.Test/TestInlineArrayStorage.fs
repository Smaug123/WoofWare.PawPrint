namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `InlineArrayStorage.expand` is where `[InlineArray(N)]` becomes N storage cells. CoreCLR keeps
/// one `FieldDesc` and multiplies the type's instance size by N instead
/// (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:8612); PawPrint's value
/// storage is field-cell based, so it has to materialise the repeats.
///
/// The end-to-end sizes are checked against the real runtime by the sweep in
/// `TestInlineArrayLayout`. What is pinned here is the part that sweep cannot see: the *identity*
/// of the synthesised cells, and the conditions CoreCLR rejects a type-load for.
[<TestFixture>]
module TestInlineArrayStorage =

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

    /// Any real `FieldDefinitionHandle` will do: what matters is that the declared field carries a
    /// *metadata* identity, since that is what the synthesised slots are derived from.
    let private someFieldHandle =
        (bct.Int32.Fields
         |> List.filter (fun f -> not (f.Attributes.HasFlag System.Reflection.FieldAttributes.Static)))
            .Head.Handle

    let private describe () = "TestAssembly.Buffer"

    let private declaredField (name : string) (contents : CliType) (fieldType : ConcreteTypeHandle) : CliField =
        {
            Id = FieldId.metadata declaredHandle someFieldHandle name
            Name = name
            Contents = contents
            Offset = None
            Type = fieldType
            MarshallingDescriptor = None
        }

    let private intField =
        declaredField "_item" (CliType.Numeric (CliNumericType.Int32 0)) int32Handle

    let private refField = declaredField "_item" (CliType.ObjectRef None) objectHandle

    let private ofFields (fields : CliField list) : CliValueType =
        SynthesisedLayoutKind.ofFields bct allCt declaredHandle Layout.Default CharSet.Ansi fields

    [<Test>]
    let ``a type with no inline-array attribute is untouched`` () : unit =
        InlineArrayStorage.expand describe Layout.Default None [ intField ]
        |> shouldEqual [ intField ]

    /// `[InlineArray(1)]` is legal and means one slot: the declared field, unchanged. It must not
    /// acquire a synthesised identity it does not need.
    [<Test>]
    let ``a single-slot inline array is the declared field alone`` () : unit =
        InlineArrayStorage.expand describe Layout.Default (Some 1) [ intField ]
        |> shouldEqual [ intField ]

    /// Slot 0 *is* the declared field, so it keeps the metadata identity that `ldfld`,
    /// `Marshal.OffsetOf` and reflection all resolve through. Only the repeats are synthesised, and
    /// they carry both a distinct id and a distinct storage name.
    [<Test>]
    let ``repeats are synthesised with distinct identities and names`` () : unit =
        let expanded =
            InlineArrayStorage.expand describe Layout.Default (Some 3) [ intField ]

        expanded |> List.length |> shouldEqual 3

        expanded
        |> List.map (fun f -> f.Name)
        |> shouldEqual [ "_item" ; "_item[1]" ; "_item[2]" ]

        expanded.[0].Id |> shouldEqual intField.Id

        expanded.[1].Id
        |> shouldEqual (
            FieldId.InlineArrayElement (
                declaredHandle,
                ComparableFieldDefinitionHandle.Make someFieldHandle,
                "_item[1]",
                1
            )
        )

        // Every slot holds the declared field's type and zero value; only the identity differs.
        expanded
        |> List.iter (fun f ->
            f.Contents |> shouldEqual intField.Contents
            f.Type |> shouldEqual intField.Type
            f.Offset |> shouldEqual None
        )

    /// Pairwise distinctness under `exactlyEqual` is what stops two slots colliding in
    /// `CliValueType`'s field lookup, which fails loudly on an ambiguous match rather than picking
    /// one.
    [<Test>]
    let ``all slot identities are pairwise distinct`` () : unit =
        let property (n : int) : bool =
            let ids =
                InlineArrayStorage.expand describe Layout.Default (Some n) [ intField ]
                |> List.map (fun f -> f.Id)

            List.length ids = n
            && ids
               |> List.mapi (fun i a -> ids |> List.mapi (fun j b -> i = j || not (FieldId.exactlyEqual a b)))
               |> List.concat
               |> List.forall id

        property
        |> Prop.forAll (Arb.fromGen (Gen.choose (1, 16)))
        |> Check.QuickThrowOnFailure

    /// The declared name must still resolve — uniquely, to slot 0 — through the name-keyed lookup
    /// fallback. Naming every slot `_item` would make that report "ambiguous" for every inline
    /// array, which is why the repeats are suffixed.
    [<Test>]
    let ``the declared name still resolves uniquely to slot zero`` () : unit =
        let cvt =
            InlineArrayStorage.expand describe Layout.Default (Some 4) [ intField ]
            |> ofFields

        CliValueType.GetFieldLayout "_item" cvt |> shouldEqual (0, 4)
        CliValueType.GetFieldLayout "_item[2]" cvt |> shouldEqual (8, 4)

    /// The layout consequence: N identical slots laid out in sequence give CoreCLR's stride and
    /// size. (`TestInlineArrayLayout` checks this against the real runtime across element shapes;
    /// this pins the two routes through `ComputeConcreteFields` — plain sequential, and the
    /// auto-layout one a GC reference forces.)
    [<Test>]
    let ``expanded slots lay out at the element stride`` () : unit =
        let ints =
            InlineArrayStorage.expand describe Layout.Default (Some 3) [ intField ]
            |> ofFields

        CliValueType.SizeOf ints
        |> shouldEqual
            {
                Size = 12
                Alignment = 4
            }

        [ 0 ; 1 ; 2 ]
        |> List.iter (fun k ->
            let name = if k = 0 then "_item" else $"_item[%d{k}]"
            CliValueType.GetFieldLayout name ints |> shouldEqual (4 * k, 4)
        )

        let refs =
            InlineArrayStorage.expand describe Layout.Default (Some 2) [ refField ]
            |> ofFields

        CliValueType.SizeOf refs
        |> shouldEqual
            {
                Size = 16
                Alignment = 8
            }

        CliValueType.GetFieldLayout "_item" refs |> shouldEqual (0, 8)
        CliValueType.GetFieldLayout "_item[1]" refs |> shouldEqual (8, 8)

    /// `[InlineArray(N)]` on a reference type is inert: CoreCLR reads the attribute only inside the
    /// `IsValueClass()` branch of `PlaceInstanceFields` (methodtablebuilder.cpp:1738), so a class
    /// carrying it loads with its declared fields and none of the inline-array rules. C# cannot emit
    /// that (the attribute is `AttributeTargets.Struct`, which is why this is a unit test rather
    /// than an end-to-end one), but hand-written IL can, and honouring it would make us reject a
    /// two-field class that CoreCLR accepts.
    [<Test>]
    let ``the attribute is inert on a reference type`` () : unit =
        InlineArrayStorage.effectiveLength false (Some 4) |> shouldEqual None
        InlineArrayStorage.effectiveLength true (Some 4) |> shouldEqual (Some 4)
        InlineArrayStorage.effectiveLength false None |> shouldEqual None
        InlineArrayStorage.effectiveLength true None |> shouldEqual None

        // The consequence at the storage layer: a two-field type that declares the attribute keeps
        // exactly its declared fields when it is a class, and is refused when it is a struct.
        let twoFields = [ intField ; declaredField "other" intField.Contents int32Handle ]

        twoFields
        |> InlineArrayStorage.expand describe Layout.Default (InlineArrayStorage.effectiveLength false (Some 2))
        |> shouldEqual twoFields

        // Inert means inert: even a count CoreCLR would reject on a struct must not make us refuse
        // a class, because CoreCLR never looks at the attribute there and loads the type fine.
        InlineArrayStorage.effectiveLength false (Some 0) |> shouldEqual None

        twoFields
        |> InlineArrayStorage.expand describe Layout.Default (InlineArrayStorage.effectiveLength false (Some 0))
        |> shouldEqual twoFields

    /// CoreCLR raises `TypeLoadException` for each of these
    /// (`IDS_CLASSLOAD_INLINE_ARRAY_FIELD_COUNT`, `..._LENGTH`, `..._EXPLICIT`,
    /// `..._EXPLICIT_SIZE`, methodtablebuilder.cpp:1751/:1762/:1767/:1773). Only hand-crafted IL
    /// can reach them, and laying out something the guest did not ask for would be silent
    /// corruption, so each fails loudly and says which condition it was.
    [<Test>]
    let ``an inline array over anything but one offsetless metadata field is refused`` () : unit =
        let expectFailure (contains : string) (f : unit -> CliField list) : unit =
            let exn = Assert.Throws<exn> (fun () -> f () |> ignore<CliField list>)
            exn.Message |> shouldContainText contains

        expectFailure "exactly one" (fun () -> InlineArrayStorage.expand describe Layout.Default (Some 2) [])

        expectFailure
            "exactly one"
            (fun () ->
                InlineArrayStorage.expand
                    describe
                    Layout.Default
                    (Some 2)
                    [ intField ; declaredField "other" intField.Contents int32Handle ]
            )

        expectFailure
            "must be positive"
            (fun () -> InlineArrayStorage.expand describe Layout.Default (Some 0) [ intField ])

        expectFailure
            "must be positive"
            (fun () -> InlineArrayStorage.expand describe Layout.Default (Some -1) [ intField ])

        expectFailure
            "explicit layout"
            (fun () ->
                InlineArrayStorage.expand
                    describe
                    Layout.Default
                    (Some 2)
                    [
                        { intField with
                            Offset = Some 0
                        }
                    ]
            )

        // A declared `ClassSize` is a *separate* condition from explicit layout:
        // `[StructLayout(Sequential, Size = X)]` sets it without making the layout explicit.
        expectFailure
            "explicit size"
            (fun () ->
                InlineArrayStorage.expand describe (Layout.Custom (size = 32, packingSize = 0)) (Some 2) [ intField ]
            )

        // A `Pack` request without a size is legal, and must not be mistaken for one.
        InlineArrayStorage.expand describe (Layout.Custom (size = 0, packingSize = 1)) (Some 2) [ intField ]
        |> List.length
        |> shouldEqual 2

        // A repeat count whose product overflows a field offset must be refused *before* the slots
        // are materialised: expanding it would ask for hundreds of millions of `CliField` records
        // and take the interpreter down with an OOM, in place of the loud rejection CoreCLR gives.
        expectFailure
            "exceeds"
            (fun () -> InlineArrayStorage.expand describe Layout.Default (Some 1_000_000_000) [ intField ])

        // The bound is on the *product*, not on the count: the same count is legal or not depending
        // on the element's width. Both of these are rejected before any slot is materialised, so
        // asserting them costs nothing.
        expectFailure
            "exceeds"
            (fun () -> InlineArrayStorage.expand describe Layout.Default (Some ((134217720 / 4) + 1)) [ intField ])

        expectFailure
            "exceeds"
            (fun () -> InlineArrayStorage.expand describe Layout.Default (Some ((134217720 / 8) + 1)) [ refField ])

        // A large-but-legal count still expands, so the bound is a bound rather than a blanket
        // refusal. Deliberately *not* the exact boundary (33.5M slots for an `int` element): each
        // slot is a `CliField` carrying its own `FieldId` and name, so materialising the largest
        // legal inline array costs gigabytes of host memory for a nominal 128 MiB of guest storage.
        // That is a property of the field-cell storage model, shared with any other enormous value
        // type and not revisited here; no bound tighter than CoreCLR's is imposed, because that
        // would refuse types CoreCLR loads.
        InlineArrayStorage.expand describe Layout.Default (Some 4096) [ intField ]
        |> List.length
        |> shouldEqual 4096

        expectFailure
            "non-metadata identity"
            (fun () ->
                InlineArrayStorage.expand
                    describe
                    Layout.Default
                    (Some 2)
                    [
                        { intField with
                            Id = FieldId.named "_item"
                        }
                    ]
            )
