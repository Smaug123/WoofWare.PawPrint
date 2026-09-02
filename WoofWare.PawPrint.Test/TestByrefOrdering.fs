namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `cgt.un` and `clt.un` on two byrefs into one array.
///
/// Structural comparison orders such byrefs by element index, which is right only while each
/// stays inside the element its root names. A byte cursor that follows a field selection is
/// bounded relative to the field rather than the element, so it need not: for
/// `struct Pair { int X; int Y; }`, `&a[0].Y` viewed as bytes and advanced 4 *is* `&a[1]`
/// (issue #1293). Such a pair must be deferred to byte coordinates rather than answered from
/// the indices, and the deferral must then resolve to the right answer.
///
/// The oracle throughout is the byte coordinate a guest's construction asks for —
/// `index * 8 + fieldOffset + cursor` — with `Pair`'s layout (`X` at 0, `Y` at 4, stride 8)
/// taken from outside PawPrint.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByrefOrdering =

    /// Parsed once for all tests; DumpedAssembly is immutable, so sharing it under
    /// ParallelScope.All is safe.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private concreteTypeFor
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ConcreteType<ConcreteTypeHandle>
        =
        ConcreteType.makeFromIdentity
            typeInfo.Identity
            typeInfo.Namespace
            typeInfo.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

    let private byteType : ConcreteType<ConcreteTypeHandle> =
        concreteTypeFor baseClassTypes.Byte

    let private int32Type : ConcreteType<ConcreteTypeHandle> =
        concreteTypeFor baseClassTypes.Int32

    /// `struct Pair { int X; int Y; }` under default layout.
    let private pairValueType (state : IlMachineState) : CliValueType =
        let field (name : string) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 0)
                Offset = None
                Type = int32Handle
                MarshallingDescriptor = None
            }

        [ field "X" ; field "Y" ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    let private pairStride : int = 8

    let private pairFieldOffset (name : string) : int =
        match name with
        | "X" -> 0
        | "Y" -> 4
        | other -> failwith $"Pair has no field %s{other}"

    let private stateWithPairArray (len : int) : IlMachineState * ManagedHeapAddress =
        let state = state ()
        let zero = CliType.ValueType (pairValueType state)

        let arr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero int32Handle) (fun () -> zero) len state

        state, arr

    /// How a guest names a place in a `Pair[]`: `ref a[Index]`, then optionally `.X` or `.Y`,
    /// then optionally viewed as bytes (`Unsafe.As<_, byte>`) and advanced by `Cursor` bytes
    /// (`Unsafe.AddByteOffset`).
    type private Shape =
        {
            Index : int
            Field : string option
            Cursor : int option
        }

    /// Build the byref the way the interpreter would, through the production constructors,
    /// so that whole-element cursors fold into the index exactly as they do for a guest.
    let private build (state : IlMachineState) (arr : ManagedHeapAddress) (shape : Shape) : ManagedPointerSource =
        let element =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, shape.Index), [])

        let selected =
            match shape.Field with
            | None -> element
            | Some name -> ManagedPointerSource.appendProjection (ByrefProjection.Field (FieldId.named name)) element

        match shape.Cursor with
        | None -> selected
        | Some cursor ->
            let normalisation =
                ManagedPointerByteView.normalisationContextForPointers state [ selected ]

            ManagedPointerSource.addByteOffsetUnderReinterpret normalisation byteType cursor selected

    /// The byte coordinate within the array that `shape` names.
    let private address (shape : Shape) : int =
        let fieldOffset =
            match shape.Field with
            | None -> 0
            | Some name -> pairFieldOffset name

        shape.Index * pairStride + fieldOffset + defaultArg shape.Cursor 0

    let private pointer (src : ManagedPointerSource) : EvalStackValue = EvalStackValue.ManagedPointer src

    /// The pair from the issue: one address, reached from two elements.
    let private crossing : Shape =
        {
            Index = 0
            Field = Some "Y"
            Cursor = Some 4
        }

    let private element1 : Shape =
        {
            Index = 1
            Field = None
            Cursor = None
        }

    [<Test>]
    let ``a cursor after a field keeps its element as root`` () : unit =
        // The premise of the whole fixture: the field stops the cursor folding into the index,
        // so the two byrefs really do arrive at the comparison with different roots.
        let state, arr = stateWithPairArray 3

        match build state arr crossing with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (root, 0), _) -> root |> shouldEqual arr
        | other -> failwith $"expected the crossing byref to keep element 0 as its root, got %O{other}"

    [<Test>]
    let ``a cursor after a field is deferred rather than ordered by index`` () : unit =
        let state, arr = stateWithPairArray 3
        let p1 = build state arr crossing
        let p2 = build state arr element1

        match ManagedPointerSource.byteAddressDeltaSign p1 p2 with
        | ByteAddressDeltaSign.NeedsByteLocation _ -> ()
        | ByteAddressDeltaSign.Decided sign ->
            failwith $"expected structural comparison to defer the crossing pair, but it answered sign %d{sign}"

        match ManagedPointerSource.byteAddressDeltaSign p2 p1 with
        | ByteAddressDeltaSign.NeedsByteLocation _ -> ()
        | ByteAddressDeltaSign.Decided sign ->
            failwith
                $"expected structural comparison to defer the mirrored crossing pair, but it answered sign %d{sign}"

    [<Test>]
    let ``a field selected under a reinterpret is deferred`` () : unit =
        // The other way out of the element: a field offset measured in a type the element
        // need not contain. The type named here is immaterial to the structural rule.
        let state, arr = stateWithPairArray 3

        let underReinterpret =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arr, 0),
                [
                    ByrefProjection.ReinterpretAs int32Type
                    ByrefProjection.Field (FieldId.named "X")
                ]
            )

        let bare = build state arr element1

        match ManagedPointerSource.byteAddressDeltaSign underReinterpret bare with
        | ByteAddressDeltaSign.NeedsByteLocation _ -> ()
        | ByteAddressDeltaSign.Decided sign ->
            failwith $"expected a field under a reinterpret to be deferred, but it answered sign %d{sign}"

    [<Test>]
    let ``field-only chains at different indices are still ordered by index`` () : unit =
        // A field selected under the element's own type lies inside the element, so index
        // order is address order whatever the field: `a[0].Y` is below `a[1].X`.
        let state, arr = stateWithPairArray 3

        let element0Y =
            build
                state
                arr
                {
                    Index = 0
                    Field = Some "Y"
                    Cursor = None
                }

        let element1X =
            build
                state
                arr
                {
                    Index = 1
                    Field = Some "X"
                    Cursor = None
                }

        ManagedPointerSource.byteAddressDeltaSign element0Y element1X
        |> shouldEqual (ByteAddressDeltaSign.Decided 1)

        ManagedPointerSource.byteAddressDeltaSign element1X element0Y
        |> shouldEqual (ByteAddressDeltaSign.Decided -1)

    [<Test>]
    let ``the pure comparisons refuse the deferred pair`` () : unit =
        // Without machine state there is nothing to resolve the deferral with, so the
        // stateless `cgt.un` / `clt.un` must fail rather than fall back to the index answer.
        let state, arr = stateWithPairArray 3
        let p1 = pointer (build state arr crossing)
        let p2 = pointer (build state arr element1)

        for name, compare in
            [
                "clt.un", EvalStackValueComparisons.cltUn
                "cgt.un", EvalStackValueComparisons.cgtUn
            ] do
            for left, right in [ p1, p2 ; p2, p1 ] do
                let exn = Assert.Throws<System.Exception> (fun () -> compare left right |> ignore)

                exn.Message |> shouldContainText "no structural order"
                exn.Message |> shouldContainText name

    [<Test>]
    let ``the crossing pair resolves as one address`` () : unit =
        let state, arr = stateWithPairArray 3
        let p1 = pointer (build state arr crossing)
        let p2 = pointer (build state arr element1)
        let resolve = StorageLocation.resolveOrder baseClassTypes state

        EvalStackValueComparisons.cltUnDeferred p1 p2 |> resolve |> shouldEqual false
        EvalStackValueComparisons.cltUnDeferred p2 p1 |> resolve |> shouldEqual false
        EvalStackValueComparisons.cgtUnDeferred p1 p2 |> resolve |> shouldEqual false
        EvalStackValueComparisons.cgtUnDeferred p2 p1 |> resolve |> shouldEqual false

        // Trichotomy: neither above nor below, so equal — and byref `ceq`, resolved the same
        // way, agrees.
        EvalStackValueComparisons.ceqDeferred PointerHashState.empty p1 p2
        |> StorageLocation.resolveCeq baseClassTypes state
        |> shouldEqual true

    [<Test>]
    let ``a cursor short of the next element resolves below it`` () : unit =
        // Guards against a resolver that answers "equal" for every deferred pair: this pair is
        // deferred for the same reason as the crossing one, but the addresses differ.
        let state, arr = stateWithPairArray 3

        let insideElement0 =
            pointer (
                build
                    state
                    arr
                    {
                        Index = 0
                        Field = Some "Y"
                        Cursor = Some 2
                    }
            )

        let element1 = pointer (build state arr element1)
        let resolve = StorageLocation.resolveOrder baseClassTypes state

        EvalStackValueComparisons.cltUnDeferred insideElement0 element1
        |> resolve
        |> shouldEqual true

        EvalStackValueComparisons.cltUnDeferred element1 insideElement0
        |> resolve
        |> shouldEqual false

        EvalStackValueComparisons.cgtUnDeferred insideElement0 element1
        |> resolve
        |> shouldEqual false

        EvalStackValueComparisons.cgtUnDeferred element1 insideElement0
        |> resolve
        |> shouldEqual true

    [<Test>]
    let ``byrefs that conv.u has made native ints are deferred the same way`` () : unit =
        // C# pointer comparison (`p < q` on `byte*`) reaches `clt.un` with the byrefs wrapped
        // as native ints, and `conv.u8` widens them further; the deferral must see through
        // both wrappings, or those call sites would keep the pure path's refusal.
        let state, arr = stateWithPairArray 3
        let resolve = StorageLocation.resolveOrder baseClassTypes state

        let asNativeInt (src : ManagedPointerSource) : EvalStackValue =
            EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src)

        let asInt64 (src : ManagedPointerSource) : EvalStackValue =
            EvalStackValue.Int64 (Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer src) false)

        for wrap in [ asNativeInt ; asInt64 ] do
            let p1 = wrap (build state arr crossing)
            let p2 = wrap (build state arr element1)

            EvalStackValueComparisons.cltUnDeferred p1 p2 |> resolve |> shouldEqual false
            EvalStackValueComparisons.cgtUnDeferred p1 p2 |> resolve |> shouldEqual false

    [<Test>]
    let ``a deferred pair into two different arrays is still refused`` () : unit =
        // Byte coordinates settle an order only within one container; two arrays have no
        // relative placement, and the resolver must not invent one.
        let state, arr1 = stateWithPairArray 3
        let zero = CliType.ValueType (pairValueType state)

        let arr2, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero int32Handle) (fun () -> zero) 3 state

        let p1 = pointer (build state arr1 crossing)
        let p2 = pointer (build state arr2 element1)

        for outcome in
            [
                EvalStackValueComparisons.cltUnDeferred p1 p2
                EvalStackValueComparisons.cgtUnDeferred p2 p1
            ] do
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    StorageLocation.resolveOrder baseClassTypes state outcome |> ignore
                )

            exn.Message |> shouldContainText "no structural order"

    let private genShape : Gen<Shape> =
        gen {
            let! index = Gen.choose (0, 2)
            let! field = Gen.elements [ None ; Some "X" ; Some "Y" ]

            let! cursor = Gen.frequency [ 1, Gen.constant None ; 3, Gen.map Some (Gen.choose (0, 11)) ]

            return
                {
                    Index = index
                    Field = field
                    Cursor = cursor
                }
        }

    /// For every pair of shapes a guest can build, the resolved `clt.un` and `cgt.un` agree
    /// with the byte coordinates, and the stateless comparisons either agree or refuse —
    /// never answer wrongly.
    [<Test>]
    let ``resolved orderings agree with byte coordinates and the pure path never contradicts them`` () : unit =
        let state, arr = stateWithPairArray 4
        let resolve = StorageLocation.resolveOrder baseClassTypes state

        let mutable deferred = 0
        let mutable decided = 0

        let property (left : Shape, right : Shape) : unit =
            let p1 = build state arr left
            let p2 = build state arr right

            let expectedBelow = address left < address right
            let expectedAbove = address left > address right

            match ManagedPointerSource.byteAddressDeltaSign p1 p2 with
            | ByteAddressDeltaSign.NeedsByteLocation _ -> deferred <- deferred + 1
            | ByteAddressDeltaSign.Decided _ -> decided <- decided + 1

            let v1 = pointer p1
            let v2 = pointer p2

            let describe =
                $"%A{left} (byte %d{address left}) against %A{right} (byte %d{address right})"

            let resolvedBelow = EvalStackValueComparisons.cltUnDeferred v1 v2 |> resolve
            let resolvedAbove = EvalStackValueComparisons.cgtUnDeferred v1 v2 |> resolve

            if resolvedBelow <> expectedBelow then
                failwith $"clt.un answered %b{resolvedBelow} for %s{describe}"

            if resolvedAbove <> expectedAbove then
                failwith $"cgt.un answered %b{resolvedAbove} for %s{describe}"

            let pureOrRefused (compare : EvalStackValue -> EvalStackValue -> bool) : bool option =
                try
                    Some (compare v1 v2)
                with _ ->
                    None

            match pureOrRefused EvalStackValueComparisons.cltUn with
            | Some answer when answer <> expectedBelow ->
                failwith $"stateless clt.un answered %b{answer} for %s{describe}"
            | _ -> ()

            match pureOrRefused EvalStackValueComparisons.cgtUn with
            | Some answer when answer <> expectedAbove ->
                failwith $"stateless cgt.un answered %b{answer} for %s{describe}"
            | _ -> ()

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGen (Gen.zip genShape genShape)) property
        )

        // Both routes must have been exercised, or the law above is vacuous for one of them.
        if deferred = 0 then
            failwith "no generated pair was deferred to byte coordinates"

        if decided = 0 then
            failwith "no generated pair was decided structurally"
