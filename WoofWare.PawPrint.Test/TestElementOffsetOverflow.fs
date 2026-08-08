namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `IntrinsicHelpers.offsetManagedPointerByElements` is the shared element-offset walk behind
/// `Unsafe.Add`, `Unsafe.Subtract`, `Span<T>.get_Item` and friends. Every branch of it combines the
/// caller's element `offset` with a stored index or byte offset that PawPrint represents as an
/// int32.
///
/// Real IL does that arithmetic at native-int width (`sizeof !!T; conv.i; mul; add`), so an int32
/// product or sum that wraps is a *modelling* overflow, not the CLI's — and a wrapped result is not
/// merely imprecise, it puts the byref on the wrong side of the source address. Concretely,
/// `Unsafe.Add(ref a[1], Int32.MaxValue)` should land `+8589934592` bytes from `&a[0]`; wrapping the
/// index to `Int32.MinValue` reports `-8589934592`.
///
/// PawPrint cannot represent such a byref (the roots store int32 indices and byte offsets), and the
/// project prefers a loud refusal to a silently wrong answer, so these assert that the in-range
/// arithmetic is exact and that the out-of-range arithmetic fails loudly rather than wrapping.
[<TestFixture>]
module TestElementOffsetOverflow =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over its
    // sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private state () : IlMachineState =
        // Factory intentionally undisposed: state.Logger outlives this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ti

    let private int32Handle : ConcreteTypeHandle = handleFor bct.Int32
    let private int64Handle : ConcreteTypeHandle = handleFor bct.Int64

    let private allocateIntArray (length : int) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        IlMachineState.allocateArray
            (ConcreteTypeHandle.OneDimArrayZero int32Handle)
            (fun () -> CliType.Numeric (CliNumericType.Int32 0))
            length
            state

    /// A byref at `&a[index]`. The index is deliberately allowed to sit outside the array's stored
    /// bounds: PawPrint models an out-of-bounds byref symbolically and only refuses it on
    /// dereference, which is exactly what lets a large `Unsafe.Add` be expressed at all.
    let private elementByref (arr : ManagedHeapAddress) (index : int) : EvalStackValue =
        EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []))

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Element index and offset whose sum stays inside int32, so the walk has an exact answer that
    /// PawPrint can represent. Deliberately spans the whole int32 range rather than plausible array
    /// sizes: the wrap this guards against only shows up near the extremes.
    let private genRepresentableCase : Gen<int * int> =
        gen {
            let! index = ArbMap.defaults |> ArbMap.generate<int>
            let! offset = ArbMap.defaults |> ArbMap.generate<int>

            let sum = int64<int> index + int64<int> offset

            if sum < int64<int> System.Int32.MinValue || sum > int64<int> System.Int32.MaxValue then
                // Fold the pair back into range rather than discarding it, so the generator keeps
                // producing extreme-magnitude inputs instead of drifting towards small ones.
                return index, -offset
            else
                return index, offset
        }

    [<Test>]
    let ``in-range element walk lands on index plus offset`` () : unit =
        let property ((index, offset) : int * int) : unit =
            let arr, st = allocateIntArray 4 (state ())

            let result =
                IntrinsicHelpers.offsetManagedPointerByElements bct st int32Handle offset (elementByref arr index)
                |> fst

            // The int64 oracle: the walk is plain integer addition on the element index, and the
            // generator has already established the answer is representable.
            let expected = int32<int64> (int64<int> index + int64<int> offset)

            result
            |> shouldEqual (
                EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, expected), []))
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genRepresentableCase) property)

    [<Test>]
    let ``element walk refuses an index that overflows int32`` () : unit =
        // The exact shape Codex found: `Unsafe.Add(ref a[1], Int32.MaxValue)`. Real .NET lands
        // +8589934592 bytes from &a[0]; wrapping would report -8589934592.
        let arr, st = allocateIntArray 4 (state ())

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements
                    bct
                    st
                    int32Handle
                    System.Int32.MaxValue
                    (elementByref arr 1)
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "2147483648"

    [<Test>]
    let ``element walk refuses an index that underflows int32`` () : unit =
        let arr, st = allocateIntArray 4 (state ())

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements
                    bct
                    st
                    int32Handle
                    System.Int32.MinValue
                    (elementByref arr -1)
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "-2147483649"

    [<Test>]
    let ``byte-cursor walk refuses a byte delta that overflows int32`` () : unit =
        // `sizeof(T)` differing from the array's element stride, with a trailing `ReinterpretAs`,
        // routes through the byte cursor instead of the cell index. There the wrap is in
        // `tSize * offset` rather than `i + offset`: 8 bytes by 300_000_000 elements is 2.4e9,
        // which does not fit in the int32 `ByrefProjection.ByteOffset`.
        let arr, st = allocateIntArray 4 (state ())

        let int64View =
            AllConcreteTypes.lookup int64Handle concreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Int64 was not concretised")

        let src =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (arr, 0),
                    [ ByrefProjection.ReinterpretAs int64View ]
                )
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements bct st int64Handle 300000000 src
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "2400000000"

    [<Test>]
    let ``in-range byte-cursor walk still succeeds`` () : unit =
        // The control for the case above: a byte delta that does fit must keep working, so the new
        // guard is not simply refusing the whole byte-cursor path.
        let arr, st = allocateIntArray 4 (state ())

        let int64View =
            AllConcreteTypes.lookup int64Handle concreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Int64 was not concretised")

        let src =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (arr, 0),
                    [ ByrefProjection.ReinterpretAs int64View ]
                )
            )

        let result =
            IntrinsicHelpers.offsetManagedPointerByElements bct st int64Handle 1 src |> fst

        // One `long` forward is 8 bytes, and the array's stride is 4, so the byte cursor
        // normalises cleanly back onto cell 2 rather than leaving a trailing `ByteOffset`.
        match result with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (actual, index), projs)) ->
            actual |> shouldEqual arr
            index |> shouldEqual 2

            projs
            |> List.tryLast
            |> shouldEqual (Some (ByrefProjection.ReinterpretAs int64View))
        | other -> failwith $"expected a byte-view byref into the array, got %O{other}"
