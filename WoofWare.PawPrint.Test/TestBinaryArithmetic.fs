namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBinaryArithmetic =

    /// Parsed once for all tests; DumpedAssembly is immutable, so sharing it
    /// under ParallelScope.All is safe.
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

    let private allocatedIntArray (values : int list) : AllocatedArray =
        let elements : ImmutableArray<CliType> =
            values
            |> Seq.map (fun i -> CliType.Numeric (CliNumericType.Int32 i))
            |> ImmutableArray.CreateRange

        {
            ConcreteType = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 1)
            Length = values.Length
            Elements = elements
        }

    let private stateWithIntArray (values : int list) : IlMachineState * ManagedHeapAddress =
        let state = state ()
        let array = allocatedIntArray values

        let arr, heap = ManagedHeap.allocateArray array state.ManagedHeap

        { state with
            ManagedHeap = heap
        },
        arr

    let private stateWithTwoIntArrays
        (values1 : int list)
        (values2 : int list)
        : IlMachineState * ManagedHeapAddress * ManagedHeapAddress
        =
        let state, arr1 = stateWithIntArray values1

        let arr2, heap =
            ManagedHeap.allocateArray (allocatedIntArray values2) state.ManagedHeap

        { state with
            ManagedHeap = heap
        },
        arr1,
        arr2

    let private valuesOfLength (length : int) : int list = List.init length id

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

    let private expectSyntheticNativeIntValue (actual : EvalStackValue) : int64 =
        match actual with
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset actual) -> actual
        | other -> failwith $"expected synthetic cross-array native int, got %O{other}"

    let private arrayBytePosition
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        : int64
        =
        let obj = state.ManagedHeap.Arrays.[arr]

        let elementSize =
            if obj.Length = 0 then
                0
            else
                CliType.sizeOf obj.Elements.[0]

        int64 index * int64 elementSize + int64 byteOffset

    let private expectedCrossArrayByteDelta
        (state : IlMachineState)
        (arr1 : ManagedHeapAddress)
        (index1 : int)
        (byteOffset1 : int)
        (arr2 : ManagedHeapAddress)
        (index2 : int)
        (byteOffset2 : int)
        : int64
        =
        let (ManagedHeapAddress.ManagedHeapAddress id1) = arr1
        let (ManagedHeapAddress.ManagedHeapAddress id2) = arr2
        let arraySeparation = int64 (compare id1 id2) * (1L <<< 40)
        let delta1 = arrayBytePosition state arr1 index1 byteOffset1
        let delta2 = arrayBytePosition state arr2 index2 byteOffset2

        arraySeparation + (delta1 - delta2)

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    type private SameArrayCase =
        {
            Length : int
            Index : int
            FirstStep : int
            SecondStep : int
        }

    type private CrossArrayCase =
        {
            Length1 : int
            Index1 : int
            ByteOffset1 : int
            Length2 : int
            Index2 : int
            ByteOffset2 : int
        }

    let private genArrayLength : Gen<int> = Gen.choose (0, 8)

    let private genSmallOffset : Gen<int> = Gen.choose (-12, 12)

    let private genSameArrayCase : Gen<SameArrayCase> =
        gen {
            let! length = genArrayLength
            let! index = genSmallOffset
            let! firstStep = genSmallOffset
            let! secondStep = genSmallOffset

            return
                {
                    Length = length
                    Index = index
                    FirstStep = firstStep
                    SecondStep = secondStep
                }
        }

    let private genCrossArrayCase : Gen<CrossArrayCase> =
        gen {
            let! length1 = genArrayLength
            let! length2 = genArrayLength
            let! index1 = genSmallOffset
            let! index2 = genSmallOffset
            let! byteOffset1 = Gen.choose (-7, 7)
            let! byteOffset2 = Gen.choose (-7, 7)

            return
                {
                    Length1 = length1
                    Index1 = index1
                    ByteOffset1 = byteOffset1
                    Length2 = length2
                    Index2 = index2
                    ByteOffset2 = byteOffset2
                }
        }

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
    let ``subtracting byrefs into different arrays returns a tagged byte sentinel`` () : unit =
        let state, arr1, arr2 =
            stateWithTwoIntArrays [ 10 ; 20 ; 30 ; 40 ] [ 100 ; 200 ; 300 ; 400 ]

        let forward =
            BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr1 5) (arrayPointer arr2 3)
            |> expectSyntheticNativeIntValue

        let backward =
            BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr2 3) (arrayPointer arr1 5)
            |> expectSyntheticNativeIntValue

        forward + backward |> shouldEqual 0L
        forward |> shouldEqual (expectedCrossArrayByteDelta state arr1 5 0 arr2 3 0)

        if abs forward < 1_000_000L then
            failwith $"expected cross-array sentinel magnitude to be large, got %d{forward}"

    [<Test>]
    let ``synthetic cross-array subtraction refuses downstream arithmetic`` () : unit =
        let state, arr1, arr2 = stateWithTwoIntArrays [ 10 ; 20 ] [ 30 ; 40 ]

        let synthetic =
            BinaryArithmetic.execute ArithmeticOperation.sub state (arrayPointer arr1 0) (arrayPointer arr2 0)

        synthetic |> expectSyntheticNativeIntValue |> ignore

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                BinaryArithmetic.execute ArithmeticOperation.add state synthetic (EvalStackValue.Int32 1)
                |> ignore
            )

        ex.Message |> shouldContainText "non-verbatim native int"

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

    [<Test>]
    let ``subtracting array byte-view byrefs across arrays returns a tagged byte sentinel`` () : unit =
        let state, arr1, arr2 = stateWithTwoIntArrays [ 1 ; 2 ; 3 ] [ 4 ; 5 ; 6 ]
        let ptr1 = byteViewPointer arr1 2 1
        let ptr2 = byteViewPointer arr2 0 3

        BinaryArithmetic.execute ArithmeticOperation.sub state ptr1 ptr2
        |> expectSyntheticNativeIntValue
        |> shouldEqual (expectedCrossArrayByteDelta state arr1 2 1 arr2 0 3)

    [<Test>]
    let ``subtracting byte-view byrefs at different indices of an empty array fails explicitly`` () : unit =
        let state, arr = stateWithIntArray []

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                BinaryArithmetic.execute
                    ArithmeticOperation.sub
                    state
                    (byteViewPointer arr 1 0)
                    (byteViewPointer arr 0 0)
                |> ignore
            )

        ex.Message
        |> shouldContainText "cannot compute byte delta between different indices of empty array"

    [<Test>]
    let ``plain array byref arithmetic obeys generated add and subtract laws`` () : unit =
        let mutable negativeSteps = 0
        let mutable zeroSteps = 0
        let mutable positiveSteps = 0

        let property (case : SameArrayCase) : bool =
            if case.FirstStep < 0 then
                negativeSteps <- negativeSteps + 1
            elif case.FirstStep = 0 then
                zeroSteps <- zeroSteps + 1
            else
                positiveSteps <- positiveSteps + 1

            let state, arr = stateWithIntArray (valuesOfLength case.Length)
            let ptr = arrayPointer arr case.Index

            let afterFirst =
                BinaryArithmetic.execute ArithmeticOperation.add state ptr (EvalStackValue.Int32 case.FirstStep)

            afterFirst |> expectArrayPointer arr (case.Index + case.FirstStep)

            let afterBoth =
                BinaryArithmetic.execute ArithmeticOperation.add state afterFirst (EvalStackValue.Int32 case.SecondStep)

            let direct =
                BinaryArithmetic.execute
                    ArithmeticOperation.add
                    state
                    ptr
                    (EvalStackValue.Int32 (case.FirstStep + case.SecondStep))

            afterBoth |> shouldEqual direct

            BinaryArithmetic.execute ArithmeticOperation.sub state afterFirst (EvalStackValue.Int32 case.FirstStep)
            |> shouldEqual ptr

            BinaryArithmetic.execute ArithmeticOperation.sub state afterFirst ptr
            |> expectNativeInt (int64 case.FirstStep)

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSameArrayCase) property)

        if negativeSteps = 0 || zeroSteps = 0 || positiveSteps = 0 then
            failwith
                $"generator did not exercise all step signs: negative=%d{negativeSteps}, zero=%d{zeroSteps}, positive=%d{positiveSteps}"

    [<Test>]
    let ``cross-array byref subtraction is generated anti-symmetric and tagged`` () : unit =
        let mutable emptyArrayCases = 0
        let mutable nonEmptyArrayCases = 0
        let mutable nonZeroByteOffsetCases = 0

        let property (case : CrossArrayCase) : bool =
            if case.Length1 = 0 || case.Length2 = 0 then
                emptyArrayCases <- emptyArrayCases + 1

            if case.Length1 > 0 && case.Length2 > 0 then
                nonEmptyArrayCases <- nonEmptyArrayCases + 1

            if case.ByteOffset1 <> 0 || case.ByteOffset2 <> 0 then
                nonZeroByteOffsetCases <- nonZeroByteOffsetCases + 1

            let state, arr1, arr2 =
                stateWithTwoIntArrays (valuesOfLength case.Length1) (valuesOfLength case.Length2)

            let forward =
                BinaryArithmetic.execute
                    ArithmeticOperation.sub
                    state
                    (arrayPointer arr1 case.Index1)
                    (arrayPointer arr2 case.Index2)
                |> expectSyntheticNativeIntValue

            let backward =
                BinaryArithmetic.execute
                    ArithmeticOperation.sub
                    state
                    (arrayPointer arr2 case.Index2)
                    (arrayPointer arr1 case.Index1)
                |> expectSyntheticNativeIntValue

            forward + backward |> shouldEqual 0L

            forward
            |> shouldEqual (expectedCrossArrayByteDelta state arr1 case.Index1 0 arr2 case.Index2 0)

            let byteViewForward =
                BinaryArithmetic.execute
                    ArithmeticOperation.sub
                    state
                    (byteViewPointer arr1 case.Index1 case.ByteOffset1)
                    (byteViewPointer arr2 case.Index2 case.ByteOffset2)
                |> expectSyntheticNativeIntValue

            let byteViewBackward =
                BinaryArithmetic.execute
                    ArithmeticOperation.sub
                    state
                    (byteViewPointer arr2 case.Index2 case.ByteOffset2)
                    (byteViewPointer arr1 case.Index1 case.ByteOffset1)
                |> expectSyntheticNativeIntValue

            byteViewForward + byteViewBackward |> shouldEqual 0L

            byteViewForward
            |> shouldEqual (
                expectedCrossArrayByteDelta state arr1 case.Index1 case.ByteOffset1 arr2 case.Index2 case.ByteOffset2
            )

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCrossArrayCase) property)

        if emptyArrayCases = 0 || nonEmptyArrayCases = 0 || nonZeroByteOffsetCases = 0 then
            failwith
                $"generator missed required regimes: empty=%d{emptyArrayCases}, nonEmpty=%d{nonEmptyArrayCases}, nonZeroByteOffsets=%d{nonZeroByteOffsetCases}"
