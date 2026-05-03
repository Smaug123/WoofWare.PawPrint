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
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary<string, DumpedAssembly>.Empty.Add (corelib.Name.FullName, corelib)

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private execute
        (op : IArithmeticOperation)
        (state : IlMachineState)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : EvalStackValue
        =
        BinaryArithmetic.execute baseClassTypes op state val1 val2

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
            ConcreteType = ConcreteTypeHandle.OneDimArrayZero int32Handle
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

    let private taggedAddress (storage : ByteStorageIdentity option) (offset : int64) : EvalStackValue =
        EvalStackValue.UInt64 (
            UInt64Source.ManagedAddress
                {
                    Storage = storage
                    Offset = offset
                }
        )

    let private expectTaggedAddress
        (expectedStorage : ByteStorageIdentity option)
        (expectedOffset : int64)
        (actual : EvalStackValue)
        : unit
        =
        match actual with
        | EvalStackValue.UInt64 (UInt64Source.ManagedAddress actual) ->
            actual.Storage |> shouldEqual expectedStorage
            actual.Offset |> shouldEqual expectedOffset
        | other ->
            failwith $"expected tagged UInt64 managed address %O{expectedStorage} + %d{expectedOffset}, got %O{other}"

    let private expectTaggedVerbatimUInt64 (expected : int64) (actual : EvalStackValue) : unit =
        match actual with
        | EvalStackValue.UInt64 (UInt64Source.Verbatim actual) -> actual |> shouldEqual expected
        | other -> failwith $"expected tagged UInt64 verbatim bits %d{expected}, got %O{other}"

    let private expectManagedAddressProjection (state : IlMachineState) (ptr : ManagedPointerSource) : ManagedAddress =
        match IlMachineState.tryManagedPointerAddress baseClassTypes state ptr with
        | Some address -> address
        | None -> failwith $"expected managed pointer to project to an address: %O{ptr}"

    let private syntheticNativeIntSourceValue (actual : NativeIntSource) : int64 =
        match actual with
        | NativeIntSource.SyntheticCrossArrayOffset actual -> actual
        | other -> failwith $"expected synthetic cross-storage native int, got %O{other}"

    let private expectedCrossStorageMagnitude (maxOffsetMagnitude : int64) : int64 =
        NativeIntSource.syntheticCrossStorageSeparation - maxOffsetMagnitude

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    // FirstStep is zero with probability 1/25, so 650 cases puts the false
    // "missed zero" balance failure probability below 1e-11.
    let private sameArrayPropertyConfig : Config =
        Config.QuickThrowOnFailure.WithMaxTest 650

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

    type private CrossStorageCase =
        {
            OriginKind : string
            Origin : ByteStorageIdentity
            OriginOffset : int64
            TargetKind : string
            Target : ByteStorageIdentity
            TargetOffset : int64
        }

    type private TaggedAddressCase =
        {
            Kind : string
            Storage : ByteStorageIdentity
            BaseOffset : int64
            Step : int64
            OtherOffset : int64
        }

    [<RequireQualifiedAccess>]
    type private NormalisableRootKind =
        | LocalMemory
        | Array
        | String

    type private ByteOffsetNormalisationCase =
        {
            Kind : NormalisableRootKind
            RootOffset : int
            ArrayCellSize : int
            ByteOffset : int
        }

    let private genArrayLength : Gen<int> = Gen.choose (0, 8)

    let private genSmallOffset : Gen<int> = Gen.choose (-12, 12)

    let private storageIdentities : (string * ByteStorageIdentity) array =
        [|
            "array", ByteStorageIdentity.Array (ManagedHeapAddress 101)
            "string", ByteStorageIdentity.String (ManagedHeapAddress 102)
            "local-memory", ByteStorageIdentity.LocalMemory (ThreadId 0, FrameId 10, LocallocBlockId 0)
            "stack-local", ByteStorageIdentity.StackLocal (ThreadId 0, FrameId 11, 1us)
            "stack-argument", ByteStorageIdentity.StackArgument (ThreadId 0, FrameId 12, 2us)
        |]

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

    let private genCrossStorageCase : Gen<CrossStorageCase> =
        gen {
            let! originIndex = Gen.choose (0, storageIdentities.Length - 1)
            let! distance = Gen.choose (1, storageIdentities.Length - 1)
            let targetIndex = (originIndex + distance) % storageIdentities.Length
            let originKind, origin = storageIdentities.[originIndex]
            let targetKind, target = storageIdentities.[targetIndex]
            let! originOffset = Gen.choose (-16, 16)
            let! targetOffset = Gen.choose (-16, 16)

            return
                {
                    OriginKind = originKind
                    Origin = origin
                    OriginOffset = int64 originOffset
                    TargetKind = targetKind
                    Target = target
                    TargetOffset = int64 targetOffset
                }
        }

    let private genTaggedAddressCase : Gen<TaggedAddressCase> =
        gen {
            let! storageIndex = Gen.choose (0, storageIdentities.Length - 1)
            let kind, storage = storageIdentities.[storageIndex]
            let! baseOffset = Gen.choose (-16, 16)
            let! step = Gen.choose (0, 16)
            let! otherOffset = Gen.choose (-16, 16)

            return
                {
                    Kind = kind
                    Storage = storage
                    BaseOffset = int64 baseOffset
                    Step = int64 step
                    OtherOffset = int64 otherOffset
                }
        }

    let private genByteOffsetNormalisationCase : Gen<ByteOffsetNormalisationCase> =
        gen {
            let! kind =
                Gen.elements
                    [
                        NormalisableRootKind.LocalMemory
                        NormalisableRootKind.Array
                        NormalisableRootKind.String
                    ]

            let! rootOffset = Gen.choose (-8, 8)
            let! arrayCellSize = Gen.choose (1, 8)
            let! byteOffset = Gen.choose (-32, 32)

            return
                {
                    Kind = kind
                    RootOffset = rootOffset
                    ArrayCellSize = arrayCellSize
                    ByteOffset = byteOffset
                }
        }

    let private floorDivRem (value : int) (divisor : int) : int * int =
        let q = value / divisor
        let r = value - q * divisor

        if r < 0 then q - 1, r + divisor else q, r

    let private pointerForNormalisationCase (case : ByteOffsetNormalisationCase) : ManagedPointerSource =
        match case.Kind with
        | NormalisableRootKind.LocalMemory ->
            ManagedPointerSource.Byref (
                ByrefRoot.LocalMemoryByte (ThreadId 0, FrameId 0, LocallocBlockId 0, case.RootOffset),
                []
            )
        | NormalisableRootKind.Array ->
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 123, case.RootOffset), [])
        | NormalisableRootKind.String ->
            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (ManagedHeapAddress 456, case.RootOffset), [])

    let private expectedNormalisedPointer (case : ByteOffsetNormalisationCase) : ManagedPointerSource =
        let cellSize =
            match case.Kind with
            | NormalisableRootKind.LocalMemory -> 1
            | NormalisableRootKind.Array -> case.ArrayCellSize
            | NormalisableRootKind.String -> 2

        let cellAdvance, inCellOffset = floorDivRem case.ByteOffset cellSize

        let root =
            match case.Kind with
            | NormalisableRootKind.LocalMemory ->
                ByrefRoot.LocalMemoryByte (ThreadId 0, FrameId 0, LocallocBlockId 0, case.RootOffset + cellAdvance)
            | NormalisableRootKind.Array ->
                ByrefRoot.ArrayElement (ManagedHeapAddress 123, case.RootOffset + cellAdvance)
            | NormalisableRootKind.String ->
                ByrefRoot.StringCharAt (ManagedHeapAddress 456, case.RootOffset + cellAdvance)

        let projs =
            if inCellOffset = 0 then
                [ ByrefProjection.ReinterpretAs byteType ]
            else
                [
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset inCellOffset
                ]

        ManagedPointerSource.Byref (root, projs)

    [<Test>]
    let ``byte offset helper normalises every byte-addressable root with generated offsets`` () : unit =
        let mutable localMemoryCases = 0
        let mutable arrayCases = 0
        let mutable stringCases = 0
        let mutable negativeOffsets = 0
        let mutable zeroOffsets = 0
        let mutable positiveOffsets = 0
        let mutable residualOffsets = 0

        let property (case : ByteOffsetNormalisationCase) : bool =
            match case.Kind with
            | NormalisableRootKind.LocalMemory -> localMemoryCases <- localMemoryCases + 1
            | NormalisableRootKind.Array -> arrayCases <- arrayCases + 1
            | NormalisableRootKind.String -> stringCases <- stringCases + 1

            if case.ByteOffset < 0 then
                negativeOffsets <- negativeOffsets + 1
            elif case.ByteOffset = 0 then
                zeroOffsets <- zeroOffsets + 1
            else
                positiveOffsets <- positiveOffsets + 1

            let context =
                match case.Kind with
                | NormalisableRootKind.Array ->
                    ByteOffsetNormalisationContext.withArrayElementSize (ManagedHeapAddress 123) case.ArrayCellSize
                | NormalisableRootKind.LocalMemory
                | NormalisableRootKind.String -> ByteOffsetNormalisationContext.nonArrayRootsOnly

            let ptr = pointerForNormalisationCase case

            let raw =
                ptr
                |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs byteType)
                |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset case.ByteOffset)

            let smart =
                ManagedPointerSource.addByteOffsetUnderReinterpret context byteType case.ByteOffset ptr

            let byteViewSmart =
                ptr
                |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs byteType)
                |> ManagedPointerSource.addByteOffsetToByteView context case.ByteOffset

            let expected = expectedNormalisedPointer case

            smart |> shouldEqual expected

            byteViewSmart |> shouldEqual expected

            ManagedPointerSource.normaliseForComparison context raw
            |> NormalisedManagedPointerSource.value
            |> shouldEqual expected

            ManagedPointerSource.normaliseForComparison context smart
            |> NormalisedManagedPointerSource.value
            |> shouldEqual smart

            match expected with
            | ManagedPointerSource.Byref (_, [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset _ ]) ->
                residualOffsets <- residualOffsets + 1
            | _ -> ()

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genByteOffsetNormalisationCase) property)

        if localMemoryCases = 0 || arrayCases = 0 || stringCases = 0 then
            failwith
                $"generator missed normalisable roots: local-memory=%d{localMemoryCases}, array=%d{arrayCases}, string=%d{stringCases}"

        if negativeOffsets = 0 || zeroOffsets = 0 || positiveOffsets = 0 then
            failwith
                $"generator missed offset signs: negative=%d{negativeOffsets}, zero=%d{zeroOffsets}, positive=%d{positiveOffsets}"

        if residualOffsets = 0 then
            failwith "generator did not exercise non-zero in-cell residual offsets"

    [<Test>]
    let ``add advances plain array byrefs by element offset`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.add state (arrayPointer arr 1) (EvalStackValue.Int32 2)
        |> expectArrayPointer arr 3

    [<Test>]
    let ``add supports integer offset on the left of an array byref`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.add state (EvalStackValue.Int32 2) (arrayPointer arr 1)
        |> expectArrayPointer arr 3

    [<Test>]
    let ``add accepts nativeint offsets for array byrefs`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute
            ArithmeticOperation.add
            state
            (arrayPointer arr 0)
            (EvalStackValue.NativeInt (NativeIntSource.Verbatim 3L))
        |> expectArrayPointer arr 3

    [<Test>]
    let ``array byref arithmetic permits one-past and negative offsets`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]

        execute ArithmeticOperation.add state (arrayPointer arr 2) (EvalStackValue.Int32 1)
        |> expectArrayPointer arr 3

        execute ArithmeticOperation.add state (arrayPointer arr 1) (EvalStackValue.Int32 -1)
        |> expectArrayPointer arr 0

    [<Test>]
    let ``managed pointer address projection preserves negative array byte offsets`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arr, -1),
                [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 2 ]
            )

        match IlMachineState.tryManagedPointerAddress baseClassTypes state ptr with
        | Some {
                   Storage = Some (ByteStorageIdentity.Array actualArr)
                   Offset = actualOffset
               } ->
            actualArr |> shouldEqual arr
            actualOffset |> shouldEqual -2L
        | other -> failwith $"expected negative array byte address projection, got %O{other}"

    [<Test>]
    let ``checked int64 helpers report overflow without throwing`` () : unit =
        CheckedInt64.tryAdd System.Int64.MaxValue 1L |> shouldEqual None
        CheckedInt64.tryAdd System.Int64.MinValue (-1L) |> shouldEqual None
        CheckedInt64.trySubtract System.Int64.MinValue 1L |> shouldEqual None
        CheckedInt64.trySubtract System.Int64.MaxValue (-1L) |> shouldEqual None

        CheckedInt64.tryAdd 40L 2L |> shouldEqual (Some 42L)
        CheckedInt64.trySubtract 40L (-2L) |> shouldEqual (Some 42L)

    [<Test>]
    let ``storage-free tagged UInt64 addresses behave as verbatim bits`` () : unit =
        let storageFree = taggedAddress None 3L

        execute ArithmeticOperation.sub (state ()) (EvalStackValue.UInt64 (UInt64Source.Verbatim 10L)) storageFree
        |> expectTaggedVerbatimUInt64 7L

        let coerced =
            EvalStackValue.toCliTypeCoerced (CliType.Numeric (CliNumericType.Int64 0L)) (taggedAddress None 123L)

        coerced |> shouldEqual (CliType.Numeric (CliNumericType.Int64 123L))

    [<Test>]
    let ``tagged UInt64 arithmetic accepts nativeint managed pointer operands`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 1), [])
        let address = expectManagedAddressProjection state ptr
        let tagged = EvalStackValue.UInt64 (UInt64Source.ManagedAddress address)
        let nativePtr = EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)

        execute ArithmeticOperation.sub state tagged nativePtr
        |> expectTaggedVerbatimUInt64 0L

        let addEx =
            Assert.Throws<System.Exception> (fun () -> execute ArithmeticOperation.add state tagged nativePtr |> ignore)

        addEx.Message |> shouldContainText "refusing to add two managed addresses"

    [<Test>]
    let ``tagged UInt64 arithmetic accepts raw managed pointer operands`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 1), [])
        let address = expectManagedAddressProjection state ptr
        let tagged = EvalStackValue.UInt64 (UInt64Source.ManagedAddress address)
        let rawPtr = EvalStackValue.ManagedPointer ptr

        execute ArithmeticOperation.sub state tagged rawPtr
        |> expectTaggedVerbatimUInt64 0L

    [<Test>]
    let ``tagged UInt64 comparisons project nativeint managed pointer operands through state`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 1), [])
        let address = expectManagedAddressProjection state ptr
        let tagged = EvalStackValue.UInt64 (UInt64Source.ManagedAddress address)
        let nativePtr = EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)
        let directPtr = EvalStackValue.ManagedPointer ptr

        let taggedAfter =
            EvalStackValue.UInt64 (
                UInt64Source.ManagedAddress
                    { address with
                        Offset = address.Offset + 1L
                    }
            )

        let taggedBefore =
            EvalStackValue.UInt64 (
                UInt64Source.ManagedAddress
                    { address with
                        Offset = address.Offset - 1L
                    }
            )

        let projection = IlMachineState.tryManagedPointerAddress baseClassTypes state

        EvalStackValueComparisons.ceqWithManagedPointerProjection projection tagged nativePtr
        |> shouldEqual true

        EvalStackValueComparisons.ceqWithManagedPointerProjection projection tagged directPtr
        |> shouldEqual true

        EvalStackValueComparisons.cgtUnWithManagedPointerProjection projection taggedAfter nativePtr
        |> shouldEqual true

        EvalStackValueComparisons.cltUnWithManagedPointerProjection projection nativePtr taggedAfter
        |> shouldEqual true

        EvalStackValueComparisons.cgtUnWithManagedPointerProjection projection directPtr taggedBefore
        |> shouldEqual true

        EvalStackValueComparisons.cltUnWithManagedPointerProjection projection directPtr taggedAfter
        |> shouldEqual true

    [<Test>]
    let ``verbatim tagged UInt64 arithmetic preserves raw high-bit values`` () : unit =
        let highBit = System.Int64.MinValue

        execute
            ArithmeticOperation.add
            (state ())
            (EvalStackValue.UInt64 (UInt64Source.Verbatim highBit))
            (EvalStackValue.UInt64 (UInt64Source.Verbatim 1L))
        |> expectTaggedVerbatimUInt64 (highBit + 1L)

    [<Test>]
    let ``add ovf refuses raw tagged UInt64 operands instead of using signed overflow`` () : unit =
        let ex =
            Assert.Throws<System.Exception> (fun () ->
                execute
                    ArithmeticOperation.addOvf
                    (state ())
                    (EvalStackValue.UInt64 (UInt64Source.Verbatim System.Int64.MinValue))
                    (EvalStackValue.UInt64 (UInt64Source.Verbatim 0L))
                |> ignore
            )

        ex.Message |> shouldContainText "refusing raw UInt64 operands"

    [<Test>]
    let ``tagged UInt64 managed address arithmetic rejects raw negative byte offsets`` () : unit =
        let storage = Some (ByteStorageIdentity.Array (ManagedHeapAddress 101))

        let assertRejects (operation : IArithmeticOperation) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () ->
                    execute
                        operation
                        (state ())
                        (taggedAddress storage 0L)
                        (EvalStackValue.UInt64 (UInt64Source.Verbatim System.Int64.MinValue))
                    |> ignore
                )

            ex.Message |> shouldContainText "exceeds Int64.MaxValue"

        assertRejects ArithmeticOperation.add
        assertRejects ArithmeticOperation.sub

    [<Test>]
    let ``tagged UInt64 managed address offset overflow fails with managed-address context`` () : unit =
        let storage = Some (ByteStorageIdentity.Array (ManagedHeapAddress 101))

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                execute
                    ArithmeticOperation.add
                    (state ())
                    (taggedAddress storage System.Int64.MaxValue)
                    (EvalStackValue.UInt64 (UInt64Source.Verbatim 1L))
                |> ignore
            )

        ex.Message |> shouldContainText "managed address offset overflow"

    [<Test>]
    let ``unsigned tagged UInt64 managed address comparisons support null checks`` () : unit =
        let storage = Some (ByteStorageIdentity.Array (ManagedHeapAddress 101))
        let nonNull = taggedAddress storage 0L
        let nullTagged = taggedAddress None 0L
        let zero = EvalStackValue.UInt64 (UInt64Source.Verbatim 0L)

        EvalStackValueComparisons.cgtUn nonNull zero |> shouldEqual true
        EvalStackValueComparisons.cltUn nonNull zero |> shouldEqual false
        EvalStackValueComparisons.cgtUn zero nonNull |> shouldEqual false
        EvalStackValueComparisons.cltUn zero nonNull |> shouldEqual true

        EvalStackValueComparisons.cgtUn nullTagged zero |> shouldEqual false
        EvalStackValueComparisons.cltUn nullTagged zero |> shouldEqual false
        EvalStackValueComparisons.ceq nullTagged zero |> shouldEqual true

    [<Test>]
    let ``subtracting an integer from an array byref moves backwards`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.sub state (arrayPointer arr 3) (EvalStackValue.Int32 2)
        |> expectArrayPointer arr 1

    [<Test>]
    let ``subtracting two plain byrefs in the same array returns byte delta`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.sub state (arrayPointer arr 3) (arrayPointer arr 1)
        |> expectNativeInt 8L

        execute ArithmeticOperation.sub state (arrayPointer arr 1) (arrayPointer arr 3)
        |> expectNativeInt -8L

    [<Test>]
    let ``subtracting byrefs into different arrays returns a tagged byte sentinel`` () : unit =
        let state, arr1, arr2 =
            stateWithTwoIntArrays [ 10 ; 20 ; 30 ; 40 ] [ 100 ; 200 ; 300 ; 400 ]

        let forward =
            execute ArithmeticOperation.sub state (arrayPointer arr1 5) (arrayPointer arr2 3)
            |> expectSyntheticNativeIntValue

        let backward =
            execute ArithmeticOperation.sub state (arrayPointer arr2 3) (arrayPointer arr1 5)
            |> expectSyntheticNativeIntValue

        forward + backward |> shouldEqual 0L

        if abs forward < 1_000_000L then
            failwith $"expected cross-array sentinel magnitude to be large, got %d{forward}"

    [<Test>]
    let ``synthetic cross-array subtraction refuses downstream arithmetic`` () : unit =
        let state, arr1, arr2 = stateWithTwoIntArrays [ 10 ; 20 ] [ 30 ; 40 ]

        let synthetic =
            execute ArithmeticOperation.sub state (arrayPointer arr1 0) (arrayPointer arr2 0)

        synthetic |> expectSyntheticNativeIntValue |> ignore

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                execute ArithmeticOperation.add state synthetic (EvalStackValue.Int32 1)
                |> ignore
            )

        ex.Message |> shouldContainText "non-verbatim native int"

        let taggedEx =
            Assert.Throws<System.Exception> (fun () ->
                execute ArithmeticOperation.add state (EvalStackValue.UInt64 (UInt64Source.Verbatim 0L)) synthetic
                |> ignore
            )

        taggedEx.Message |> shouldContainText "synthetic cross-storage offset"

        let comparisonEx =
            Assert.Throws<System.Exception> (fun () ->
                EvalStackValueComparisons.ceq (EvalStackValue.UInt64 (UInt64Source.Verbatim 0L)) synthetic
                |> ignore
            )

        comparisonEx.Message |> shouldContainText "synthetic cross-storage offset"

    [<Test>]
    let ``subtracting array byte-view byrefs accounts for cell stride and byte offset`` () : unit =
        let state, arr = stateWithIntArray [ 0x11223344 ; 0x55667788 ; 0x01020304 ]

        // Normalised form of a byte cursor six bytes after arr[0]:
        // one whole int cell plus a two-byte in-cell offset.
        let sixBytesIn = byteViewPointer arr 1 2
        let origin = byteViewPointer arr 0 0

        execute ArithmeticOperation.sub state sixBytesIn origin |> expectNativeInt 6L

        execute ArithmeticOperation.sub state origin sixBytesIn |> expectNativeInt -6L

    [<Test>]
    let ``subtracting array byte-view byrefs across arrays returns a tagged byte sentinel`` () : unit =
        let state, arr1, arr2 = stateWithTwoIntArrays [ 1 ; 2 ; 3 ] [ 4 ; 5 ; 6 ]
        let ptr1 = byteViewPointer arr1 2 1
        let ptr2 = byteViewPointer arr2 0 3

        let plain =
            execute ArithmeticOperation.sub state (arrayPointer arr1 2) (arrayPointer arr2 0)
            |> expectSyntheticNativeIntValue

        let byteView =
            execute ArithmeticOperation.sub state ptr1 ptr2 |> expectSyntheticNativeIntValue

        byteView - plain |> shouldEqual -2L

    [<Test>]
    let ``subtracting byte-view byrefs in an empty array uses element type size`` () : unit =
        let state, arr = stateWithIntArray []

        execute ArithmeticOperation.sub state (byteViewPointer arr 1 0) (byteViewPointer arr 0 0)
        |> expectNativeInt 4L

        let projected =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 1), [])
            |> expectManagedAddressProjection state

        projected.Offset |> shouldEqual 4L

    [<Test>]
    let ``array byref arithmetic rejects int32 index overflow`` () : unit =
        let state, arr = stateWithIntArray [ 1 ]

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                execute ArithmeticOperation.add state (arrayPointer arr System.Int32.MaxValue) (EvalStackValue.Int32 1)
                |> ignore
            )

        ex.Message |> shouldContainText "overflowed int32 offset model"

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
                execute ArithmeticOperation.add state ptr (EvalStackValue.Int32 case.FirstStep)

            afterFirst |> expectArrayPointer arr (case.Index + case.FirstStep)

            let afterBoth =
                execute ArithmeticOperation.add state afterFirst (EvalStackValue.Int32 case.SecondStep)

            let direct =
                execute ArithmeticOperation.add state ptr (EvalStackValue.Int32 (case.FirstStep + case.SecondStep))

            afterBoth |> shouldEqual direct

            execute ArithmeticOperation.sub state afterFirst (EvalStackValue.Int32 case.FirstStep)
            |> shouldEqual ptr

            execute ArithmeticOperation.sub state afterFirst ptr
            |> expectNativeInt (int64 case.FirstStep * 4L)

            true

        Check.One (sameArrayPropertyConfig, Prop.forAll (Arb.fromGen genSameArrayCase) property)

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
                execute ArithmeticOperation.sub state (arrayPointer arr1 case.Index1) (arrayPointer arr2 case.Index2)
                |> expectSyntheticNativeIntValue

            let backward =
                execute ArithmeticOperation.sub state (arrayPointer arr2 case.Index2) (arrayPointer arr1 case.Index1)
                |> expectSyntheticNativeIntValue

            forward + backward |> shouldEqual 0L

            if abs forward < expectedCrossStorageMagnitude 128L then
                failwith $"expected generated cross-array sentinel magnitude to be large, got %d{forward}"

            let byteViewForward =
                execute
                    ArithmeticOperation.sub
                    state
                    (byteViewPointer arr1 case.Index1 case.ByteOffset1)
                    (byteViewPointer arr2 case.Index2 case.ByteOffset2)
                |> expectSyntheticNativeIntValue

            let byteViewBackward =
                execute
                    ArithmeticOperation.sub
                    state
                    (byteViewPointer arr2 case.Index2 case.ByteOffset2)
                    (byteViewPointer arr1 case.Index1 case.ByteOffset1)
                |> expectSyntheticNativeIntValue

            byteViewForward + byteViewBackward |> shouldEqual 0L

            if abs byteViewForward < expectedCrossStorageMagnitude 128L then
                failwith $"expected generated byte-view sentinel magnitude to be large, got %d{byteViewForward}"

            byteViewForward - forward
            |> shouldEqual (int64 (case.ByteOffset1 - case.ByteOffset2))

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCrossArrayCase) property)

        if emptyArrayCases = 0 || nonEmptyArrayCases = 0 || nonZeroByteOffsetCases = 0 then
            failwith
                $"generator missed required regimes: empty=%d{emptyArrayCases}, nonEmpty=%d{nonEmptyArrayCases}, nonZeroByteOffsets=%d{nonZeroByteOffsetCases}"

    [<Test>]
    let ``cross-storage byte offsets are generated anti-symmetric for all byte storage identities`` () : unit =
        let mutable arrayCases = 0
        let mutable stringCases = 0
        let mutable localMemoryCases = 0
        let mutable stackLocalCases = 0
        let mutable stackArgumentCases = 0

        let touchesKind (kind : string) (case : CrossStorageCase) : bool =
            case.OriginKind = kind || case.TargetKind = kind

        let property (case : CrossStorageCase) : bool =
            if touchesKind "array" case then
                arrayCases <- arrayCases + 1

            if touchesKind "string" case then
                stringCases <- stringCases + 1

            if touchesKind "local-memory" case then
                localMemoryCases <- localMemoryCases + 1

            if touchesKind "stack-local" case then
                stackLocalCases <- stackLocalCases + 1

            if touchesKind "stack-argument" case then
                stackArgumentCases <- stackArgumentCases + 1

            let forward =
                NativeIntSource.syntheticCrossStorageByteOffset
                    case.Origin
                    case.OriginOffset
                    case.Target
                    case.TargetOffset
                |> syntheticNativeIntSourceValue

            let backward =
                NativeIntSource.syntheticCrossStorageByteOffset
                    case.Target
                    case.TargetOffset
                    case.Origin
                    case.OriginOffset
                |> syntheticNativeIntSourceValue

            forward + backward |> shouldEqual 0L

            if abs forward < expectedCrossStorageMagnitude 32L then
                failwith $"expected generated cross-storage sentinel magnitude to be large, got %d{forward}"

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCrossStorageCase) property)

        if
            arrayCases = 0
            || stringCases = 0
            || localMemoryCases = 0
            || stackLocalCases = 0
            || stackArgumentCases = 0
        then
            failwith
                $"generator missed required storage identities: array=%d{arrayCases}, string=%d{stringCases}, local-memory=%d{localMemoryCases}, stack-local=%d{stackLocalCases}, stack-argument=%d{stackArgumentCases}"

    [<Test>]
    let ``tagged UInt64 managed address arithmetic preserves storage identity`` () : unit =
        let mutable arrayCases = 0
        let mutable stringCases = 0
        let mutable localMemoryCases = 0
        let mutable stackLocalCases = 0
        let mutable stackArgumentCases = 0
        let mutable zeroSteps = 0
        let mutable positiveSteps = 0

        let touchesKind (kind : string) (case : TaggedAddressCase) : bool = case.Kind = kind

        let property (case : TaggedAddressCase) : unit =
            if touchesKind "array" case then
                arrayCases <- arrayCases + 1

            if touchesKind "string" case then
                stringCases <- stringCases + 1

            if touchesKind "local-memory" case then
                localMemoryCases <- localMemoryCases + 1

            if touchesKind "stack-local" case then
                stackLocalCases <- stackLocalCases + 1

            if touchesKind "stack-argument" case then
                stackArgumentCases <- stackArgumentCases + 1

            if case.Step = 0L then
                zeroSteps <- zeroSteps + 1
            else
                positiveSteps <- positiveSteps + 1

            let state = state ()
            let storage = Some case.Storage
            let original = taggedAddress storage case.BaseOffset
            let offset = EvalStackValue.Int64 case.Step
            let expectedOffset = case.BaseOffset + case.Step
            let expected = taggedAddress storage expectedOffset

            execute ArithmeticOperation.add state original offset
            |> expectTaggedAddress storage expectedOffset

            execute ArithmeticOperation.addOvf state original offset
            |> expectTaggedAddress storage expectedOffset

            execute ArithmeticOperation.add state offset original
            |> expectTaggedAddress storage expectedOffset

            execute ArithmeticOperation.sub state expected offset
            |> expectTaggedAddress storage case.BaseOffset

            execute ArithmeticOperation.sub state expected original
            |> expectTaggedVerbatimUInt64 case.Step

            if not (EvalStackValueComparisons.ceq expected (taggedAddress storage expectedOffset)) then
                failwith $"expected same tagged address to compare equal: %O{expected}"

            if
                expectedOffset <> case.OtherOffset
                && EvalStackValueComparisons.ceq expected (taggedAddress storage case.OtherOffset)
            then
                failwith
                    $"expected distinct offsets in the same storage to compare unequal: %d{expectedOffset} vs %d{case.OtherOffset}"

            EvalStackValueComparisons.cgtUn expected (taggedAddress storage case.OtherOffset)
            |> shouldEqual (uint64 expectedOffset > uint64 case.OtherOffset)

            EvalStackValueComparisons.cltUn expected (taggedAddress storage case.OtherOffset)
            |> shouldEqual (uint64 expectedOffset < uint64 case.OtherOffset)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genTaggedAddressCase) property)

        if
            arrayCases = 0
            || stringCases = 0
            || localMemoryCases = 0
            || stackLocalCases = 0
            || stackArgumentCases = 0
        then
            failwith
                $"generator missed required tagged storage identities: array=%d{arrayCases}, string=%d{stringCases}, local-memory=%d{localMemoryCases}, stack-local=%d{stackLocalCases}, stack-argument=%d{stackArgumentCases}"

        if zeroSteps = 0 || positiveSteps = 0 then
            failwith $"generator missed tagged offset steps: zero=%d{zeroSteps}, positive=%d{positiveSteps}"

    [<Test>]
    let ``tagged UInt64 managed addresses do not order across storage identities`` () : unit =
        let property (case : CrossStorageCase) : unit =
            let left = taggedAddress (Some case.Origin) case.OriginOffset
            let right = taggedAddress (Some case.Target) case.TargetOffset

            if EvalStackValueComparisons.ceq left right then
                failwith $"expected different tagged storages to compare unequal: %O{left} vs %O{right}"

            let cgtEx =
                Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cgtUn left right |> ignore)

            cgtEx.Message |> shouldContainText "different storage"

            let cltEx =
                Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cltUn left right |> ignore)

            cltEx.Message |> shouldContainText "different storage"

            let subEx =
                Assert.Throws<System.Exception> (fun () ->
                    execute ArithmeticOperation.sub (state ()) left right |> ignore
                )

            subEx.Message |> shouldContainText "different storage"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCrossStorageCase) property)
