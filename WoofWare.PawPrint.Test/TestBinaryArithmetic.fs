namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBinaryArithmetic =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

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

    let private stateWithIntArray (values : int list) : IlMachineState * ManagedHeapAddress =
        let state = state ()

        let elements : ImmutableArray<CliType> =
            values
            |> Seq.map (fun i -> CliType.Numeric (CliNumericType.Int32 i))
            |> ImmutableArray.CreateRange

        let array : AllocatedArray =
            {
                ConcreteType = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 1)
                Length = values.Length
                Elements = elements
            }

        let arr, heap = ManagedHeap.allocateArray array state.ManagedHeap

        { state with
            ManagedHeap = heap
        },
        arr

    let private arrayPointer (arr : ManagedHeapAddress) (index : int) : EvalStackValue =
        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), [])
        |> EvalStackValue.ManagedPointer

    let private byteViewPointer (arr : ManagedHeapAddress) (index : int) (byteOffset : int) : EvalStackValue =
        let projs =
            if byteOffset = 0 then
                [ ByrefProjection.ReinterpretAs byteType ]
            else
                [
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset byteOffset
                ]

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs)
        |> EvalStackValue.ManagedPointer

    let private expectArrayPointer
        (expectedArr : ManagedHeapAddress)
        (expectedIndex : int)
        (actual : EvalStackValue)
        : unit
        =
        match actual with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), [])) ->
            arr |> shouldEqual expectedArr
            index |> shouldEqual expectedIndex
        | other -> failwith $"expected array element pointer at index %d{expectedIndex}, got %O{other}"

    let private expectNativeInt (expected : int64) (actual : EvalStackValue) : unit =
        match actual with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim actual) -> actual |> shouldEqual expected
        | other -> failwith $"expected native int %d{expected}, got %O{other}"

    let private expectNativeIntValue (actual : EvalStackValue) : int64 =
        match actual with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim actual) -> actual
        | other -> failwith $"expected native int, got %O{other}"

    [<Test>]
    let ``add advances plain array byrefs by element offset`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        BinaryArithmetic.execute ArithmeticOperation.add state (arrayPointer arr 1) (EvalStackValue.Int32 2)
        |> expectArrayPointer arr 3

    [<Test>]
    let ``add supports integer offset on the left of an array byref`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        BinaryArithmetic.execute ArithmeticOperation.add state (EvalStackValue.Int32 2) (arrayPointer arr 1)
        |> expectArrayPointer arr 3

    [<Test>]
    let ``add accepts nativeint offsets for array byrefs`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        BinaryArithmetic.execute
            ArithmeticOperation.add
            state
            (arrayPointer arr 0)
            (EvalStackValue.NativeInt (NativeIntSource.Verbatim 3L))
        |> expectArrayPointer arr 3

    [<Test>]
    let ``array byref arithmetic permits one-past and negative offsets`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]

        BinaryArithmetic.execute ArithmeticOperation.add state (arrayPointer arr 2) (EvalStackValue.Int32 1)
        |> expectArrayPointer arr 3

        BinaryArithmetic.execute ArithmeticOperation.add state (arrayPointer arr 1) (EvalStackValue.Int32 -1)
        |> expectArrayPointer arr 0

    [<Test>]
    let ``subtracting an integer from an array byref moves backwards`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr 3) (EvalStackValue.Int32 2)
        |> expectArrayPointer arr 1

    [<Test>]
    let ``subtracting two plain byrefs in the same array returns element delta`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr 3) (arrayPointer arr 1)
        |> expectNativeInt 2L

        BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr 1) (arrayPointer arr 3)
        |> expectNativeInt -2L

    [<Test>]
    let ``subtracting byrefs into different arrays returns a large deterministic sentinel`` () : unit =
        let state, arr1 = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        let array2 : AllocatedArray =
            {
                ConcreteType = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 1)
                Length = 4
                Elements =
                    [ 100 ; 200 ; 300 ; 400 ]
                    |> Seq.map (fun i -> CliType.Numeric (CliNumericType.Int32 i))
                    |> ImmutableArray.CreateRange
            }

        let arr2, heap = ManagedHeap.allocateArray array2 state.ManagedHeap

        let state =
            { state with
                ManagedHeap = heap
            }

        let forward =
            BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr1 1) (arrayPointer arr2 2)
            |> expectNativeIntValue

        let backward =
            BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr2 2) (arrayPointer arr1 1)
            |> expectNativeIntValue

        forward + backward |> shouldEqual 0L

        if abs forward < 1_000_000L then
            failwith $"expected cross-array sentinel magnitude to be large, got %d{forward}"

    [<Test>]
    let ``subtracting array byte-view byrefs accounts for cell stride and byte offset`` () : unit =
        let state, arr = stateWithIntArray [ 0x11223344 ; 0x55667788 ; 0x01020304 ]

        // Normalised form of a byte cursor six bytes after arr[0]:
        // one whole int cell plus a two-byte in-cell offset.
        let sixBytesIn = byteViewPointer arr 1 2
        let origin = byteViewPointer arr 0 0

        BinaryArithmetic.execute ArithmeticOperation.sub state sixBytesIn origin
        |> expectNativeInt 6L

        BinaryArithmetic.execute ArithmeticOperation.sub state origin sixBytesIn
        |> expectNativeInt -6L
