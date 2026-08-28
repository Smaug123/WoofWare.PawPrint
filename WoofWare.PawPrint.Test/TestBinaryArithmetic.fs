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

    let private execute
        (op : IArithmeticOperation)
        (state : IlMachineState)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : EvalStackValue
        =
        BinaryArithmetic.execute baseClassTypes op state val1 val2 |> fst

    /// Run a faulting operation on operands the test expects not to fault it.
    let private executeFaultingOk
        (op : FaultingArithmeticOperation)
        (state : IlMachineState)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : EvalStackValue
        =
        match BinaryArithmetic.executeFaulting baseClassTypes op state val1 val2 with
        | Ok (result, _) -> result
        | Error fault -> failwith $"%s{op.Name} of %O{val1} and %O{val2} unexpectedly faulted with %O{fault}"

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
            Shape =
                {
                    ConcreteType = ConcreteTypeHandle.OneDimArrayZero int32Handle
                    Length = values.Length
                    Lengths = ImmutableArray.Create values.Length
                    ElementStride = sizeof<int32>
                    ElementZero = CliType.Numeric (CliNumericType.Int32 0)
                }
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

    let private expectSyntheticNativeIntValue (actual : EvalStackValue) : SyntheticCrossArrayOffset =
        match actual with
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset actual) -> actual
        | other -> failwith $"expected synthetic cross-array native int, got %O{other}"

    let private syntheticNativeIntSourceValue (actual : NativeIntSource) : SyntheticCrossArrayOffset =
        match actual with
        | NativeIntSource.SyntheticCrossArrayOffset actual -> actual
        | other -> failwith $"expected synthetic cross-storage native int, got %O{other}"

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

    [<RequireQualifiedAccess>]
    type private NormalisableRootKind =
        | StackMemory
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
            "local-memory", ByteStorageIdentity.StackMemory (ThreadId 0, FrameId 10, StackMemoryBlockId 0)
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

    /// The whole space of byte-offset normalisation cases: every root kind, crossed with
    /// every root offset in [-8, 8], every array cell size in [1, 8], and every byte offset
    /// in [-32, 32]. `ArrayCellSize` is read only for `Array` roots, so it is pinned to 1
    /// for the other two kinds rather than repeating each of their walks eight times.
    let private allByteOffsetNormalisationCases : ByteOffsetNormalisationCase list =
        [
            for kind in
                [
                    NormalisableRootKind.StackMemory
                    NormalisableRootKind.Array
                    NormalisableRootKind.String
                ] do
                let cellSizes =
                    match kind with
                    | NormalisableRootKind.Array -> [ 1..8 ]
                    | NormalisableRootKind.StackMemory
                    | NormalisableRootKind.String -> [ 1 ]

                for rootOffset in -8 .. 8 do
                    for arrayCellSize in cellSizes do
                        for byteOffset in -32 .. 32 do
                            {
                                Kind = kind
                                RootOffset = rootOffset
                                ArrayCellSize = arrayCellSize
                                ByteOffset = byteOffset
                            }
        ]

    let private floorDivRem (value : int) (divisor : int) : int * int =
        let q = value / divisor
        let r = value - q * divisor

        if r < 0 then q - 1, r + divisor else q, r

    let private pointerForNormalisationCase (case : ByteOffsetNormalisationCase) : ManagedPointerSource =
        match case.Kind with
        | NormalisableRootKind.StackMemory ->
            ManagedPointerSource.Byref (
                ByrefRoot.StackMemoryByte (ThreadId 0, FrameId 0, StackMemoryBlockId 0, case.RootOffset),
                []
            )
        | NormalisableRootKind.Array ->
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 123, case.RootOffset), [])
        | NormalisableRootKind.String ->
            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (ManagedHeapAddress 456, case.RootOffset), [])

    let private expectedNormalisedPointer (case : ByteOffsetNormalisationCase) : ManagedPointerSource =
        let cellSize =
            match case.Kind with
            | NormalisableRootKind.StackMemory -> 1
            | NormalisableRootKind.Array -> case.ArrayCellSize
            | NormalisableRootKind.String -> 2

        let cellAdvance, inCellOffset = floorDivRem case.ByteOffset cellSize

        let root =
            match case.Kind with
            | NormalisableRootKind.StackMemory ->
                ByrefRoot.StackMemoryByte (ThreadId 0, FrameId 0, StackMemoryBlockId 0, case.RootOffset + cellAdvance)
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
    let ``byte offset helper normalises every byte-addressable root`` () : unit =
        let mutable stackMemoryCases = 0
        let mutable arrayCases = 0
        let mutable stringCases = 0
        let mutable negativeOffsets = 0
        let mutable zeroOffsets = 0
        let mutable positiveOffsets = 0
        let mutable residualOffsets = 0

        for case in allByteOffsetNormalisationCases do
            match case.Kind with
            | NormalisableRootKind.StackMemory -> stackMemoryCases <- stackMemoryCases + 1
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
                | NormalisableRootKind.StackMemory
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

            // The walk has no counterexample reporting of its own, so each case names itself.
            try
                smart |> shouldEqual expected

                byteViewSmart |> shouldEqual expected

                ManagedPointerSource.normaliseForComparison context raw
                |> NormalisedManagedPointerSource.value
                |> shouldEqual expected

                ManagedPointerSource.normaliseForComparison context smart
                |> NormalisedManagedPointerSource.value
                |> shouldEqual smart
            with e ->
                raise (System.Exception ($"%O{case}: %s{e.Message}", e))

            match expected with
            | ManagedPointerSource.Byref (_, [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset _ ]) ->
                residualOffsets <- residualOffsets + 1
            | _ -> ()

        // Exact counts, so narrowing any range in the enumeration is a failure rather than
        // a silently smaller walk.
        stackMemoryCases |> shouldEqual 1105
        arrayCases |> shouldEqual 8840
        stringCases |> shouldEqual 1105

        negativeOffsets |> shouldEqual 5440
        zeroOffsets |> shouldEqual 170
        positiveOffsets |> shouldEqual 5440

        // A residual is an offset that does not land on a cell boundary, so it survives
        // normalisation as a trailing ByteOffset projection. Stack-memory roots have
        // one-byte cells and so never produce one.
        residualOffsets |> shouldEqual 6358

    [<Test>]
    let ``add advances plain array byrefs by element offset`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.add state (arrayPointer arr 1) (EvalStackValue.Int32 (Int32Source.Verbatim 2))
        |> expectArrayPointer arr 3

    [<Test>]
    let ``add supports integer offset on the left of an array byref`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.add state (EvalStackValue.Int32 (Int32Source.Verbatim 2)) (arrayPointer arr 1)
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

        execute ArithmeticOperation.add state (arrayPointer arr 2) (EvalStackValue.Int32 (Int32Source.Verbatim 1))
        |> expectArrayPointer arr 3

        execute ArithmeticOperation.add state (arrayPointer arr 1) (EvalStackValue.Int32 (Int32Source.Verbatim -1))
        |> expectArrayPointer arr 0

    [<Test>]
    let ``subtracting an integer from an array byref moves backwards`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]

        execute ArithmeticOperation.sub state (arrayPointer arr 3) (EvalStackValue.Int32 (Int32Source.Verbatim 2))
        |> expectArrayPointer arr 1

    /// Weighted towards the range boundaries, so `sub.ovf`'s trapping regime is
    /// exercised as often as its ordinary arithmetic one.
    let private genOverflowProneInt32 : Gen<int32> =
        Gen.frequency
            [
                2, ArbMap.defaults |> ArbMap.generate<int32>
                2, Gen.choose (-8, 8)
                3,
                Gen.elements
                    [
                        System.Int32.MinValue
                        System.Int32.MinValue + 1
                        System.Int32.MaxValue
                        System.Int32.MaxValue - 1
                        0
                        -1
                        1
                    ]
            ]

    let private genOverflowProneInt64 : Gen<int64> =
        Gen.frequency
            [
                2, ArbMap.defaults |> ArbMap.generate<int64>
                2, Gen.choose (-8, 8) |> Gen.map int64<int>
                3,
                Gen.elements
                    [
                        System.Int64.MinValue
                        System.Int64.MinValue + 1L
                        System.Int64.MaxValue
                        System.Int64.MaxValue - 1L
                        0L
                        -1L
                        1L
                        int64<int32> System.Int32.MinValue
                        int64<int32> System.Int32.MaxValue
                    ]
            ]

    /// `None` means the operation faulted with `OpcodeFault.Overflow`, which `Sub_ovf` raises
    /// in the guest as `System.OverflowException`.
    let private trySubOvf
        (state : IlMachineState)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : EvalStackValue option
        =
        match BinaryArithmetic.executeFaulting baseClassTypes ArithmeticOperation.subOvf state val1 val2 with
        | Ok (result, _) -> Some result
        | Error OpcodeFault.Overflow -> None
        | Error fault -> failwith $"sub.ovf faulted with %O{fault}, but overflow is the only fault it can detect"

    [<Test>]
    let ``sub ovf on int32 traps exactly when the exact difference leaves int32 range`` () : unit =
        let state = state ()
        let mutable trapped = 0
        let mutable computed = 0

        let property (a : int32, b : int32) : unit =
            let exact = bigint a - bigint b

            let inRange =
                exact >= bigint System.Int32.MinValue && exact <= bigint System.Int32.MaxValue

            match
                trySubOvf
                    state
                    (EvalStackValue.Int32 (Int32Source.Verbatim a))
                    (EvalStackValue.Int32 (Int32Source.Verbatim b))
            with
            | Some actual ->
                computed <- computed + 1

                if not inRange then
                    failwith
                        $"sub.ovf %d{a} - %d{b} returned %O{actual}, but the exact difference %O{exact} is outside int32"

                actual
                |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim (int32 exact)))
                // In range, the checked and unchecked forms must agree.
                actual
                |> shouldEqual (
                    execute
                        ArithmeticOperation.sub
                        state
                        (EvalStackValue.Int32 (Int32Source.Verbatim a))
                        (EvalStackValue.Int32 (Int32Source.Verbatim b))
                )
            | None ->
                trapped <- trapped + 1

                if inRange then
                    failwith $"sub.ovf %d{a} - %d{b} trapped, but the exact difference %O{exact} fits in int32"

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip genOverflowProneInt32 genOverflowProneInt32)) property
        )

        if trapped = 0 || computed = 0 then
            failwith $"generator missed a regime: trapped=%d{trapped}, computed=%d{computed}"

    [<Test>]
    let ``sub ovf on int64 traps exactly when the exact difference leaves int64 range`` () : unit =
        let state = state ()
        let mutable trapped = 0
        let mutable computed = 0

        let property (a : int64, b : int64) : unit =
            let exact = bigint a - bigint b

            let inRange =
                exact >= bigint System.Int64.MinValue && exact <= bigint System.Int64.MaxValue

            let val1 = EvalStackValue.Int64 (Int64Source.Verbatim a)
            let val2 = EvalStackValue.Int64 (Int64Source.Verbatim b)

            match trySubOvf state val1 val2 with
            | Some actual ->
                computed <- computed + 1

                if not inRange then
                    failwith
                        $"sub.ovf %d{a} - %d{b} returned %O{actual}, but the exact difference %O{exact} is outside int64"

                actual
                |> shouldEqual (EvalStackValue.Int64 (Int64Source.Verbatim (int64 exact)))

                actual |> shouldEqual (execute ArithmeticOperation.sub state val1 val2)
            | None ->
                trapped <- trapped + 1

                if inRange then
                    failwith $"sub.ovf %d{a} - %d{b} trapped, but the exact difference %O{exact} fits in int64"

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip genOverflowProneInt64 genOverflowProneInt64)) property
        )

        if trapped = 0 || computed = 0 then
            failwith $"generator missed a regime: trapped=%d{trapped}, computed=%d{computed}"

    let private placeholderPointer (bits : int64) : EvalStackValue =
        EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)

    /// Bit patterns for the byrefs produced by `Unsafe.AsRef<T>((void*)bits)`.
    /// Zero is excluded: the placeholder invariant is that it never carries
    /// zero (that bit pattern is `Null`).
    let private genPlaceholderBits : Gen<int64> =
        Gen.frequency
            [
                2, ArbMap.defaults |> ArbMap.generate<int64>
                3,
                Gen.elements
                    [
                        System.Int64.MinValue
                        System.Int64.MinValue + 1L
                        System.Int64.MaxValue
                        System.Int64.MaxValue - 1L
                        -1L
                        1L
                        8L
                    ]
            ]
        |> Gen.map (fun bits -> if bits = 0L then 1L else bits)

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Byte

    let private nullPointer : EvalStackValue =
        EvalStackValue.ManagedPointer ManagedPointerSource.Null

    /// `Null` is the bit pattern 0, so offsetting it must give the byref with
    /// the offset as its bit pattern — the same answer the `Unsafe.Add<T>`
    /// intrinsic already produces (IntrinsicHelpers.offsetManagedPointerByElements).
    [<Test>]
    let ``offsetting the null byref treats it as the zero bit pattern`` () : unit =
        let state = state ()
        let mutable zeroResults = 0
        let mutable nonZeroResults = 0

        let property (offset : int32) : unit =
            let v = EvalStackValue.Int32 (Int32Source.Verbatim offset)

            let expect (expectedBits : int64) (actual : EvalStackValue) : unit =
                actual
                |> shouldEqual (EvalStackValue.ManagedPointer (ManagedPointerSource.ofBitPattern expectedBits))

            execute ArithmeticOperation.add state nullPointer v |> expect (int64 offset)
            // `int32 + &` is legal and also yields a byref.
            execute ArithmeticOperation.add state v nullPointer |> expect (int64 offset)

            executeFaultingOk ArithmeticOperation.addOvf state nullPointer v
            |> expect (int64 offset)

            execute ArithmeticOperation.sub state nullPointer v |> expect (-(int64 offset))

            executeFaultingOk ArithmeticOperation.subOvf state nullPointer v
            |> expect (-(int64 offset))

            if offset = 0 then
                zeroResults <- zeroResults + 1
            else
                nonZeroResults <- nonZeroResults + 1

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genOverflowProneInt32) property)

        if zeroResults = 0 || nonZeroResults = 0 then
            failwith $"generator missed a regime: zero=%d{zeroResults}, nonZero=%d{nonZeroResults}"

    [<Test>]
    let ``subtracting Int32 MinValue from the null byref is representable`` () : unit =
        // The symbolic offset model cannot negate Int32.MinValue, but the null
        // byref is not symbolic — it is the bit pattern 0.
        let state = state ()
        let expected = placeholderPointer 2147483648L

        execute
            ArithmeticOperation.sub
            state
            nullPointer
            (EvalStackValue.Int32 (Int32Source.Verbatim System.Int32.MinValue))
        |> shouldEqual expected

        trySubOvf state nullPointer (EvalStackValue.Int32 (Int32Source.Verbatim System.Int32.MinValue))
        |> shouldEqual (Some expected)

    [<Test>]
    let ``subtracting two bit-pattern byrefs yields a native int, including null`` () : unit =
        // ECMA-335: `& - & -> native int`. Null is just the zero bit pattern,
        // so this must hold when either or both sides are null.
        let state = state ()

        execute ArithmeticOperation.sub state nullPointer nullPointer
        |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L))

        execute ArithmeticOperation.sub state (placeholderPointer 24L) nullPointer
        |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.Verbatim 24L))

        execute ArithmeticOperation.sub state nullPointer (placeholderPointer 24L)
        |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.Verbatim -24L))

    [<Test>]
    let ``opcode and Unsafe Add intrinsic agree on bit-pattern byrefs`` () : unit =
        // The same arithmetic is reachable through the `Unsafe.Add<T>`
        // intrinsic and through the `add` opcode; the two must agree.
        let mutable nullCases = 0
        let mutable placeholderCases = 0

        let property (bits : int64, offset : int32) : unit =
            let start, viaOpcodeState =
                if bits = 0L then
                    nullCases <- nullCases + 1
                    nullPointer, state ()
                else
                    placeholderCases <- placeholderCases + 1
                    placeholderPointer bits, state ()

            let viaOpcode =
                execute
                    ArithmeticOperation.add
                    viaOpcodeState
                    start
                    (EvalStackValue.Int32 (Int32Source.Verbatim offset))

            // Byte elements, so "offset by n elements" is "offset by n bytes"
            // and the two paths are directly comparable.
            let viaIntrinsic, _ =
                IntrinsicHelpers.offsetManagedPointerByElements
                    baseClassTypes
                    (state ())
                    byteHandle
                    (int64<int> offset)
                    start

            if viaOpcode <> viaIntrinsic then
                failwith
                    $"add opcode gave %O{viaOpcode} but Unsafe.Add intrinsic gave %O{viaIntrinsic} for %O{start} + %d{offset}"

        let genBits = Gen.frequency [ 1, Gen.constant 0L ; 3, genPlaceholderBits ]

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genBits genOverflowProneInt32)) property)

        if nullCases = 0 || placeholderCases = 0 then
            failwith $"generator missed a regime: null=%d{nullCases}, placeholder=%d{placeholderCases}"

    /// Offsets outside the int32 range. A bit-pattern byref's offset is a
    /// native int, so these are perfectly ordinary; only the symbolic offset
    /// model is int32-bounded.
    let private genOversizeInt64Offset : Gen<int64> =
        Gen.frequency
            [
                2,
                Gen.elements
                    [
                        1L <<< 40
                        -(1L <<< 40)
                        int64<int32> System.Int32.MaxValue + 1L
                        int64<int32> System.Int32.MinValue - 1L
                        1L <<< 62
                    ]
                1, Gen.choose (0, 1 <<< 20) |> Gen.map (fun n -> int64<int> n + 4294967296L)
            ]

    [<Test>]
    let ``bit-pattern byrefs accept native int offsets outside the int32 range`` () : unit =
        let state = state ()
        let mutable nullCases = 0
        let mutable placeholderCases = 0

        let property (bits : int64, offset : int64) : unit =
            let start =
                if bits = 0L then
                    nullCases <- nullCases + 1
                    nullPointer
                else
                    placeholderCases <- placeholderCases + 1
                    placeholderPointer bits

            let asNativeInt = EvalStackValue.NativeInt (NativeIntSource.Verbatim offset)

            let expect (expectedBits : int64) (actual : EvalStackValue) : unit =
                actual
                |> shouldEqual (EvalStackValue.ManagedPointer (ManagedPointerSource.ofBitPattern expectedBits))

            execute ArithmeticOperation.add state start asNativeInt
            |> expect (bits + offset)

            execute ArithmeticOperation.sub state start asNativeInt
            |> expect (bits - offset)

            // `nativeint + &` is legal too, and must reach the same place.
            execute ArithmeticOperation.add state asNativeInt start
            |> expect (bits + offset)

        let genBits = Gen.frequency [ 1, Gen.constant 0L ; 3, genPlaceholderBits ]

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genBits genOversizeInt64Offset)) property)

        if nullCases = 0 || placeholderCases = 0 then
            failwith $"generator missed a regime: null=%d{nullCases}, placeholder=%d{placeholderCases}"

    [<Test>]
    let ``subtracting an oversize native int from the null byref is representable`` () : unit =
        let state = state ()

        execute
            ArithmeticOperation.sub
            state
            nullPointer
            (EvalStackValue.NativeInt (NativeIntSource.Verbatim (1L <<< 40)))
        |> shouldEqual (placeholderPointer -(1L <<< 40))

    [<Test>]
    let ``checked bit-pattern offsetting still traps on native int overflow with wide offsets`` () : unit =
        let state = state ()

        let bigOffset = EvalStackValue.NativeInt (NativeIntSource.Verbatim (1L <<< 62))

        // In range: no trap.
        executeFaultingOk ArithmeticOperation.addOvf state (placeholderPointer 8L) bigOffset
        |> shouldEqual (placeholderPointer ((1L <<< 62) + 8L))

        // Out of range: faults, and the unchecked form wraps instead.
        match
            BinaryArithmetic.executeFaulting
                baseClassTypes
                ArithmeticOperation.addOvf
                state
                (placeholderPointer System.Int64.MaxValue)
                bigOffset
        with
        | Error OpcodeFault.Overflow -> ()
        | Error fault -> failwith $"expected add.ovf to fault with Overflow, but it faulted with %O{fault}"
        | Ok (result, _) -> failwith $"expected add.ovf to fault with Overflow, but it returned %O{result}"

        execute ArithmeticOperation.add state (placeholderPointer System.Int64.MaxValue) bigOffset
        |> shouldEqual (placeholderPointer (System.Int64.MaxValue + (1L <<< 62)))

    [<Test>]
    let ``symbolic byrefs still refuse offsets outside the int32 offset model`` () : unit =
        // Guard against over-widening: an array byref's index is an int32, so
        // an oversize offset must fail loudly rather than silently truncate.
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ]

        Assert.Throws<System.Exception> (fun () ->
            execute
                ArithmeticOperation.add
                state
                (arrayPointer arr 0)
                (EvalStackValue.NativeInt (NativeIntSource.Verbatim (1L <<< 40)))
            |> ignore
        )
        |> ignore

    [<Test>]
    let ``sub ovf on bit-pattern byrefs traps exactly when the difference leaves native int range`` () : unit =
        // A `NativeIntPlaceholder`'s payload is a real native-int bit pattern,
        // not a symbolic offset, so this is the one pointer shape where the
        // checked form has machine-width bits to overflow.
        let state = state ()
        let mutable trapped = 0
        let mutable computed = 0

        let property (bits1 : int64, bits2 : int64) : unit =
            let exact = bigint bits1 - bigint bits2

            let inRange =
                exact >= bigint System.Int64.MinValue && exact <= bigint System.Int64.MaxValue

            let val1 = placeholderPointer bits1
            let val2 = placeholderPointer bits2

            // Unchecked `sub` wraps whatever happens; that is the behaviour
            // `sub.ovf` must not share.
            execute ArithmeticOperation.sub state val1 val2
            |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.Verbatim (bits1 - bits2)))

            match trySubOvf state val1 val2 with
            | Some actual ->
                computed <- computed + 1

                if not inRange then
                    failwith
                        $"sub.ovf on placeholders %d{bits1} - %d{bits2} returned %O{actual}, but %O{exact} is outside native int"

                actual
                |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64 exact)))
            | None ->
                trapped <- trapped + 1

                if inRange then
                    failwith $"sub.ovf on placeholders %d{bits1} - %d{bits2} trapped, but %O{exact} fits in native int"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip genPlaceholderBits genPlaceholderBits)) property)

        if trapped = 0 || computed = 0 then
            failwith $"generator missed a regime: trapped=%d{trapped}, computed=%d{computed}"

    [<Test>]
    let ``sub ovf traps when negating the minimum bit-pattern byref`` () : unit =
        let state = state ()
        let nullPtr = EvalStackValue.ManagedPointer ManagedPointerSource.Null

        // `Null - placeholder` is `0 - bits`, which overflows only at Int64.MinValue.
        trySubOvf state nullPtr (placeholderPointer (System.Int64.MinValue + 1L))
        |> shouldEqual (Some (EvalStackValue.NativeInt (NativeIntSource.Verbatim System.Int64.MaxValue)))

        trySubOvf state nullPtr (placeholderPointer System.Int64.MinValue)
        |> shouldEqual None

        // Unchecked `sub` wraps instead.
        execute ArithmeticOperation.sub state nullPtr (placeholderPointer System.Int64.MinValue)
        |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.Verbatim System.Int64.MinValue))

    [<Test>]
    let ``checked pointer offsetting of a bit-pattern byref traps exactly on native int overflow`` () : unit =
        let state = state ()
        let mutable addTrapped = 0
        let mutable addComputed = 0
        let mutable subTrapped = 0
        let mutable subComputed = 0

        let expectedPointer (bits : bigint) : EvalStackValue =
            if bits = bigint 0 then
                EvalStackValue.ManagedPointer ManagedPointerSource.Null
            else
                placeholderPointer (int64 bits)

        let check
            (checkedOp : FaultingArithmeticOperation)
            (uncheckedOp : IArithmeticOperation)
            (exact : bigint)
            (val1 : EvalStackValue)
            (val2 : EvalStackValue)
            : bool
            =
            let inRange =
                exact >= bigint System.Int64.MinValue && exact <= bigint System.Int64.MaxValue

            // The unchecked form always produces a (wrapped) pointer.
            execute uncheckedOp state val1 val2 |> ignore

            match BinaryArithmetic.executeFaulting baseClassTypes checkedOp state val1 val2 with
            | Ok (actual, _) ->
                if not inRange then
                    failwith
                        $"%s{checkedOp.Name} of %O{val1} and %O{val2} returned %O{actual}, but %O{exact} is outside native int"

                actual |> shouldEqual (expectedPointer exact)
                false
            | Error OpcodeFault.Overflow ->
                if inRange then
                    failwith $"%s{checkedOp.Name} of %O{val1} and %O{val2} trapped, but %O{exact} fits in native int"

                true
            | Error fault ->
                failwith
                    $"%s{checkedOp.Name} of %O{val1} and %O{val2} faulted with %O{fault}, but overflow is the only fault it can detect"

        let property (bits : int64, offset : int32) : unit =
            let ptr = placeholderPointer bits
            let v = EvalStackValue.Int32 (Int32Source.Verbatim offset)

            if check ArithmeticOperation.addOvf ArithmeticOperation.add (bigint bits + bigint offset) ptr v then
                addTrapped <- addTrapped + 1
            else
                addComputed <- addComputed + 1

            if check ArithmeticOperation.subOvf ArithmeticOperation.sub (bigint bits - bigint offset) ptr v then
                subTrapped <- subTrapped + 1
            else
                subComputed <- subComputed + 1

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip genPlaceholderBits genOverflowProneInt32)) property
        )

        if addTrapped = 0 || addComputed = 0 || subTrapped = 0 || subComputed = 0 then
            failwith
                $"generator missed a regime: addTrapped=%d{addTrapped}, addComputed=%d{addComputed}, subTrapped=%d{subTrapped}, subComputed=%d{subComputed}"

    [<Test>]
    let ``subtracting Int32 MinValue from a bit-pattern byref does not need to negate the offset`` () : unit =
        // `-Int32.MinValue` is not an int32, so the symbolic offset model
        // refuses it; a placeholder's payload is an int64 bit pattern, so the
        // subtraction is exact. The real runtime computes 2147483649 here.
        let state = state ()
        let expected = placeholderPointer 2147483649L

        execute
            ArithmeticOperation.sub
            state
            (placeholderPointer 1L)
            (EvalStackValue.Int32 (Int32Source.Verbatim System.Int32.MinValue))
        |> shouldEqual expected

        trySubOvf state (placeholderPointer 1L) (EvalStackValue.Int32 (Int32Source.Verbatim System.Int32.MinValue))
        |> shouldEqual (Some expected)

        // Still out of range when it overflows native int.
        trySubOvf
            state
            (placeholderPointer System.Int64.MaxValue)
            (EvalStackValue.Int32 (Int32Source.Verbatim System.Int32.MinValue))
        |> shouldEqual None

    [<Test>]
    let ``sub ovf shares sub's pointer semantics`` () : unit =
        // ECMA-335 III.3.68 gives sub.ovf the same `& - int -> &` and
        // `& - & -> native int` signatures as sub. Our pointers are symbolic
        // (a root plus offsets), so away from the bit-pattern placeholders
        // there is nothing to trap on and the two ops must be
        // indistinguishable. The placeholder cases below stay far from the
        // native-int boundary, so they agree here too; the trapping regime is
        // covered by the properties above.
        let state, arr1, arr2 = stateWithTwoIntArrays [ 10 ; 20 ; 30 ; 40 ] [ 50 ; 60 ]
        let nullPtr = EvalStackValue.ManagedPointer ManagedPointerSource.Null

        let placeholder (bits : int64) : EvalStackValue =
            EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)

        let cases : (EvalStackValue * EvalStackValue) list =
            [
                arrayPointer arr1 3, EvalStackValue.Int32 (Int32Source.Verbatim 2)
                arrayPointer arr1 1, EvalStackValue.Int32 (Int32Source.Verbatim -1)
                arrayPointer arr1 2, EvalStackValue.NativeInt (NativeIntSource.Verbatim 1L)
                arrayPointer arr1 3, arrayPointer arr1 1
                arrayPointer arr1 1, arrayPointer arr1 3
                arrayPointer arr1 1, arrayPointer arr2 0
                byteViewPointer arr1 1 3, byteViewPointer arr1 0 1
                arrayPointer arr1 2, nullPtr
                nullPtr, nullPtr
                placeholder 64L, placeholder 24L
                placeholder 64L, nullPtr
                nullPtr, placeholder 24L
                EvalStackValue.Int32 (Int32Source.Verbatim 7), nullPtr
            ]

        // Shapes our pointer model declines to subtract at all (here: an array
        // byte view against a plain array byref). Both ops must refuse them;
        // only the refusal is compared, not the messages.
        let refused : (EvalStackValue * EvalStackValue) list =
            [ byteViewPointer arr1 1 3, arrayPointer arr1 0 ]

        let run (op : IArithmeticOperation) (val1 : EvalStackValue) (val2 : EvalStackValue) : EvalStackValue option =
            try
                execute op state val1 val2 |> Some
            with _ ->
                None

        for val1, val2 in cases do
            let expected = execute ArithmeticOperation.sub state val1 val2

            match trySubOvf state val1 val2 with
            | Some actual ->
                if actual <> expected then
                    failwith $"sub.ovf %O{val1} - %O{val2} gave %O{actual}, but sub gave %O{expected}"
            | None -> failwith $"sub.ovf %O{val1} - %O{val2} trapped, but sub gave %O{expected}"

        for val1, val2 in refused do
            match run ArithmeticOperation.sub val1 val2 with
            | Some result -> failwith $"expected sub to refuse %O{val1} - %O{val2}, but it gave %O{result}"
            | None ->

            match run ArithmeticOperation.subOvf.Op val1 val2 with
            | Some result -> failwith $"sub refused %O{val1} - %O{val2}, but sub.ovf gave %O{result}"
            | None -> ()

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

        SyntheticCrossArrayOffset.negate forward |> shouldEqual backward

        SyntheticCrossArrayOffset.targetRoot forward
        |> shouldEqual (ByteStorageIdentity.Array arr1)

        SyntheticCrossArrayOffset.sourceRoot forward
        |> shouldEqual (ByteStorageIdentity.Array arr2)

    [<Test>]
    let ``synthetic cross-array subtraction refuses downstream arithmetic`` () : unit =
        let state, arr1, arr2 = stateWithTwoIntArrays [ 10 ; 20 ] [ 30 ; 40 ]

        let synthetic =
            execute ArithmeticOperation.sub state (arrayPointer arr1 0) (arrayPointer arr2 0)

        synthetic |> expectSyntheticNativeIntValue |> ignore

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                execute ArithmeticOperation.add state synthetic (EvalStackValue.Int32 (Int32Source.Verbatim 1))
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

        SyntheticCrossArrayOffset.targetRoot byteView
        |> shouldEqual (SyntheticCrossArrayOffset.targetRoot plain)

        SyntheticCrossArrayOffset.sourceRoot byteView
        |> shouldEqual (SyntheticCrossArrayOffset.sourceRoot plain)

        SyntheticCrossArrayOffset.targetOffset byteView
        - SyntheticCrossArrayOffset.targetOffset plain
        |> shouldEqual 1L

        SyntheticCrossArrayOffset.sourceOffset byteView
        - SyntheticCrossArrayOffset.sourceOffset plain
        |> shouldEqual 3L

    [<Test>]
    let ``subtracting byte-view byrefs in an empty array uses element type size`` () : unit =
        let state, arr = stateWithIntArray []

        execute ArithmeticOperation.sub state (byteViewPointer arr 1 0) (byteViewPointer arr 0 0)
        |> expectNativeInt 4L

    let private nativeMemoryPointer (block : int) (byteOffset : int) : EvalStackValue =
        ManagedPointerSource.Byref (
            ByrefRoot.NativeMemoryByte (NativeMemoryBlockId.NativeMemoryBlockId block, byteOffset),
            []
        )
        |> EvalStackValue.ManagedPointer

    let private expectNativeMemoryPointer
        (expectedBlock : int)
        (expectedByteOffset : int)
        (actual : EvalStackValue)
        : unit
        =
        match actual with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (NativeMemoryBlockId.NativeMemoryBlockId block,
                                                                                                 byteOffset),
                                                                     [])) ->
            block |> shouldEqual expectedBlock
            byteOffset |> shouldEqual expectedByteOffset
        | other ->
            failwith
                $"expected native memory byref at block %d{expectedBlock} byte %d{expectedByteOffset}, got %O{other}"

    [<Test>]
    let ``add advances native-memory byrefs by byte offset`` () : unit =
        let state = state ()

        execute ArithmeticOperation.add state (nativeMemoryPointer 0 4) (EvalStackValue.Int32 (Int32Source.Verbatim 6))
        |> expectNativeMemoryPointer 0 10

        execute ArithmeticOperation.add state (EvalStackValue.Int32 (Int32Source.Verbatim 6)) (nativeMemoryPointer 0 4)
        |> expectNativeMemoryPointer 0 10

    [<Test>]
    let ``sub on native-memory byrefs in the same block returns byte delta`` () : unit =
        let state = state ()

        execute ArithmeticOperation.sub state (nativeMemoryPointer 0 10) (nativeMemoryPointer 0 4)
        |> expectNativeInt 6L

        execute ArithmeticOperation.sub state (nativeMemoryPointer 0 4) (nativeMemoryPointer 0 10)
        |> expectNativeInt -6L

    [<Test>]
    let ``sub on native-memory byrefs in different blocks returns synthetic cross-storage offset`` () : unit =
        let state = state ()

        let forward =
            execute ArithmeticOperation.sub state (nativeMemoryPointer 0 5) (nativeMemoryPointer 1 3)
            |> expectSyntheticNativeIntValue

        let backward =
            execute ArithmeticOperation.sub state (nativeMemoryPointer 1 3) (nativeMemoryPointer 0 5)
            |> expectSyntheticNativeIntValue

        SyntheticCrossArrayOffset.negate forward |> shouldEqual backward

        SyntheticCrossArrayOffset.targetRoot forward
        |> shouldEqual (ByteStorageIdentity.NativeMemory (NativeMemoryBlockId.NativeMemoryBlockId 0))

        SyntheticCrossArrayOffset.sourceRoot forward
        |> shouldEqual (ByteStorageIdentity.NativeMemory (NativeMemoryBlockId.NativeMemoryBlockId 1))

        SyntheticCrossArrayOffset.targetOffset forward |> shouldEqual 5L
        SyntheticCrossArrayOffset.sourceOffset forward |> shouldEqual 3L

    [<Test>]
    let ``readManagedByrefBytesAs round-trips a typed cell through a native-memory byref`` () : unit =
        // Regression for Codex P2: native-memory byrefs must route through the byte-backed
        // read/write paths so that a stobj followed by an ldind via NativeMemoryByte
        // reconstitutes the value via the byte view, not via `readRootValue`'s typed-cell
        // fast path. Special-casing only `StackMemoryByte` lets `NativeMemoryByte` fall
        // through to `readManagedByref`/`writeManagedByrefWithBase`, which can't service
        // byte-backed reinterpretation when no typed cell exists at the requested offset.
        let ptr, state =
            IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized 4 (state ())

        // Plain typed-cell round-trip at the base offset.
        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                baseClassTypes
                state
                ptr
                (CliType.Numeric (CliNumericType.Int32 0x11223344))

        let roundTripped =
            IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 0))

        roundTripped |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x11223344))

    [<Test>]
    let ``readManagedByrefBytesAs reinterprets raw native-memory bytes as a typed cell`` () : unit =
        // Regression for Codex P2: the byte-backed read path must work for native-memory
        // byrefs even when the underlying block has no typed cell at the requested offset.
        // Routing bare `NativeMemoryByte` reads through `readManagedByref` → `readRootValue`
        // fails with "no typed cell here" instead of reading raw bytes.
        let ptr, state =
            IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized 4 (state ())

        let block =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, 0), []) -> block
            | other -> failwith $"expected bare NativeMemoryByte byref, got %O{other}"

        // Write raw bytes directly into the native-memory pool, bypassing typed-cell stores.
        let pool =
            NativeMemoryPool.writeBytes block 0 [| 0x44uy ; 0x33uy ; 0x22uy ; 0x11uy |] state.Kernel.NativeMemoryPool

        let state = IlMachineState.setNativeMemoryPool pool state

        let readBack =
            IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 0))

        // Little-endian assembly of the four bytes above gives 0x11223344.
        readBack |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x11223344))

    [<Test>]
    let ``array byref arithmetic rejects int32 index overflow`` () : unit =
        let state, arr = stateWithIntArray [ 1 ]

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                execute
                    ArithmeticOperation.add
                    state
                    (arrayPointer arr System.Int32.MaxValue)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 1))
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
                execute ArithmeticOperation.add state ptr (EvalStackValue.Int32 (Int32Source.Verbatim case.FirstStep))

            afterFirst |> expectArrayPointer arr (case.Index + case.FirstStep)

            let afterBoth =
                execute
                    ArithmeticOperation.add
                    state
                    afterFirst
                    (EvalStackValue.Int32 (Int32Source.Verbatim case.SecondStep))

            let direct =
                execute
                    ArithmeticOperation.add
                    state
                    ptr
                    (EvalStackValue.Int32 (Int32Source.Verbatim (case.FirstStep + case.SecondStep)))

            afterBoth |> shouldEqual direct

            execute
                ArithmeticOperation.sub
                state
                afterFirst
                (EvalStackValue.Int32 (Int32Source.Verbatim case.FirstStep))
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

            SyntheticCrossArrayOffset.negate forward |> shouldEqual backward

            SyntheticCrossArrayOffset.targetRoot forward
            |> shouldEqual (ByteStorageIdentity.Array arr1)

            SyntheticCrossArrayOffset.sourceRoot forward
            |> shouldEqual (ByteStorageIdentity.Array arr2)

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

            SyntheticCrossArrayOffset.negate byteViewForward |> shouldEqual byteViewBackward

            SyntheticCrossArrayOffset.targetOffset byteViewForward
            - SyntheticCrossArrayOffset.targetOffset forward
            |> shouldEqual (int64 case.ByteOffset1)

            SyntheticCrossArrayOffset.sourceOffset byteViewForward
            - SyntheticCrossArrayOffset.sourceOffset forward
            |> shouldEqual (int64 case.ByteOffset2)

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCrossArrayCase) property)

        if emptyArrayCases = 0 || nonEmptyArrayCases = 0 || nonZeroByteOffsetCases = 0 then
            failwith
                $"generator missed required regimes: empty=%d{emptyArrayCases}, nonEmpty=%d{nonEmptyArrayCases}, nonZeroByteOffsets=%d{nonZeroByteOffsetCases}"

    [<Test>]
    let ``cross-storage byte offsets are generated anti-symmetric for all byte storage identities`` () : unit =
        let mutable arrayCases = 0
        let mutable stringCases = 0
        let mutable stackMemoryCases = 0
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
                stackMemoryCases <- stackMemoryCases + 1

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

            SyntheticCrossArrayOffset.negate forward |> shouldEqual backward

            SyntheticCrossArrayOffset.targetRoot forward |> shouldEqual case.Target
            SyntheticCrossArrayOffset.sourceRoot forward |> shouldEqual case.Origin
            SyntheticCrossArrayOffset.targetOffset forward |> shouldEqual case.TargetOffset
            SyntheticCrossArrayOffset.sourceOffset forward |> shouldEqual case.OriginOffset

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCrossStorageCase) property)

        if
            arrayCases = 0
            || stringCases = 0
            || stackMemoryCases = 0
            || stackLocalCases = 0
            || stackArgumentCases = 0
        then
            failwith
                $"generator missed required storage identities: array=%d{arrayCases}, string=%d{stringCases}, local-memory=%d{stackMemoryCases}, stack-local=%d{stackLocalCases}, stack-argument=%d{stackArgumentCases}"

    // The following tests cover the BCL's portable wraparound idiom from
    // UnmanagedMemoryStream.Initialize: `((byte*)((long)pointer + capacity)) < pointer`
    // expressed as `Conv.U8 → ldc.i8 capacity → Add → Conv.U → ldarg pointer → Bge.un`.
    // On a 64-bit interpreter the wraparound is statically vacuous; we have to keep
    // pointer provenance flowing through the int64 widening so that the eventual
    // `Conv.U` recovers the byref and the comparison behaves correctly.

    let private convU8AndAdd (state : IlMachineState) (ptr : EvalStackValue) (capacity : int64) : EvalStackValue =
        let widened : EvalStackValue =
            EvalStackValue.convToUInt64 ptr |> EvalStackValue.Int64

        execute ArithmeticOperation.add state widened (EvalStackValue.Int64 (Int64Source.Verbatim capacity))

    [<Test>]
    let ``Conv.U8 then Conv.U on a byte-view byref recovers the original pointer`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]
        let ptr = byteViewPointer arr 1 3

        let widened : EvalStackValue =
            EvalStackValue.convToUInt64 ptr |> EvalStackValue.Int64

        let recovered : NativeIntSource =
            match EvalStackValue.toUnsignedNativeInt widened with
            | UnsignedNativeIntSource.FromManagedPointer mp -> NativeIntSource.ManagedPointer mp
            | other -> failwith $"expected FromManagedPointer, got %O{other}"

        let asNativeInt = EvalStackValue.NativeInt recovered

        if not (EvalStackValueComparisons.ceq PointerHashState.empty ptr asNativeInt) then
            failwith $"expected Conv.U8 → Conv.U to round-trip the byref, got %O{recovered} from %O{ptr}"

    [<Test>]
    let ``Conv.I8 then Conv.I on a byte-view byref recovers the original pointer`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]
        let ptr = byteViewPointer arr 2 1

        let widened : EvalStackValue =
            EvalStackValue.convToInt64 ptr |> EvalStackValue.Int64

        let recovered : NativeIntSource = EvalStackValue.toNativeInt widened

        if not (EvalStackValueComparisons.ceq PointerHashState.empty ptr (EvalStackValue.NativeInt recovered)) then
            failwith $"expected Conv.I8 → Conv.I to round-trip the byref, got %O{recovered}"

    [<Test>]
    let ``Conv.U8 of a null managed pointer normalises to verbatim zero`` () : unit =
        // Null is the zero pointer, so widening it must reduce to a plain Int64 0.
        // This keeps later arithmetic on the result usable without dragging the
        // null-pointer special case through every arm.
        match EvalStackValue.convToUInt64 (EvalStackValue.ManagedPointer ManagedPointerSource.Null) with
        | Int64Source.Verbatim 0L -> ()
        | other -> failwith $"expected null → verbatim 0, got %O{other}"

    [<Test>]
    let ``Conv.U8 then Add then Conv.U advances a byte-view byref`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ; 50 ]
        let ptr = byteViewPointer arr 1 0

        let advanced = convU8AndAdd state ptr 5L

        let recovered : NativeIntSource =
            match EvalStackValue.toUnsignedNativeInt advanced with
            | UnsignedNativeIntSource.FromManagedPointer mp -> NativeIntSource.ManagedPointer mp
            | other -> failwith $"expected FromManagedPointer after Conv.U, got %O{other}"

        // Original byref was at array index 1 with byte cursor 0; advancing 5 bytes
        // through the int64 round-trip must place the cursor 5 bytes ahead under
        // the same root and prefix. With element size 4 (int32), 5 bytes lands in
        // index 2 with a 1-byte residual cursor.
        match recovered with
        | NativeIntSource.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (resultArr, idx),
                                                                      [ ByrefProjection.ReinterpretAs _
                                                                        ByrefProjection.ByteOffset off ])) when
            resultArr = arr
            ->
            idx |> shouldEqual 2
            off |> shouldEqual 1
        | other -> failwith $"unexpected advanced byref shape: %O{other}"

    [<Test>]
    let ``UnmanagedMemoryStream wraparound check is statically not taken on 64-bit`` () : unit =
        // ECMA-335 III.3.4: bge.un is `not clt.un`. On 64-bit the BCL's wraparound
        // detection (advanced < pointer) is structurally false, so the branch over
        // the throw is always taken. We model this by checking cgeUn on the same
        // operands the BCL idiom produces.
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ; 50 ; 60 ; 70 ; 80 ]
        let ptr = byteViewPointer arr 0 0

        let advanced = convU8AndAdd state ptr 12L

        // Bge.un compares value1 (deeper, advanced) with value2 (top, original).
        // Per the C# `if (advanced < ptr) throw`, the IL skips the throw via
        // bge.un.s when advanced >= ptr; that condition must hold for any
        // non-negative capacity.
        if not (EvalStackValueComparisons.cgeUn advanced ptr) then
            failwith "expected the wraparound check to detect no wraparound for a non-negative capacity"

        if EvalStackValueComparisons.cltUn advanced ptr then
            failwith "expected clt.un to be false for advanced vs original byref"

    [<Test>]
    let ``cgt.un between byrefs of the same root reflects byte-cursor ordering`` () : unit =
        let state, arr = stateWithIntArray [ 10 ; 20 ; 30 ; 40 ]
        let earlier = byteViewPointer arr 1 0
        let later = byteViewPointer arr 1 3

        if not (EvalStackValueComparisons.cgtUn later earlier) then
            failwith "expected later byte-view byref to compare strictly greater unsigned"

        if EvalStackValueComparisons.cgtUn earlier later then
            failwith "expected earlier byte-view byref not to compare greater unsigned"

        if EvalStackValueComparisons.cgtUn earlier earlier then
            failwith "expected cgt.un on identical byrefs to be false"

    [<Test>]
    let ``cgt.un refuses to compare byrefs across distinct roots`` () : unit =
        // Cross-root pointer comparison has no defensible answer in our model;
        // this test pins the strict same-root requirement so the next loose use
        // of pointer-bit comparisons surfaces a clear diagnostic.
        let state, arr1, arr2 = stateWithTwoIntArrays [ 10 ; 20 ] [ 100 ; 200 ]

        let p1 = byteViewPointer arr1 0 0
        let p2 = byteViewPointer arr2 0 0

        let outcome =
            try
                EvalStackValueComparisons.cgtUn p1 p2 |> ignore
                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected cgt.un to refuse cross-root byref comparison"
        | Choice2Of2 e when e.Message.Contains "common root" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception from cgt.un: %s{e.Message}"

    [<Test>]
    let ``Conv.U8 round-trip via Add preserves provenance under arithmetic identity`` () : unit =
        // Property: for every non-negative offset that fits in int32, the round-trip
        // through Conv.U8 + Add + Conv.U must equal a direct pointer advance via
        // Add at the byref level. This pins the 64-bit assumption: the widened-int64
        // arithmetic and the native-int arithmetic agree.
        let state, arr = stateWithIntArray (List.init 32 id)

        let property (capacity : int) : bool =
            if capacity < 0 then
                true
            else
                let ptr = byteViewPointer arr 0 0

                let viaInt64 =
                    let advanced = convU8AndAdd state ptr (int64 capacity)

                    match EvalStackValue.toUnsignedNativeInt advanced with
                    | UnsignedNativeIntSource.FromManagedPointer mp ->
                        EvalStackValue.NativeInt (NativeIntSource.ManagedPointer mp)
                    | other -> failwith $"unexpected: %O{other}"

                let viaNativeInt =
                    execute ArithmeticOperation.add state ptr (EvalStackValue.Int32 (Int32Source.Verbatim capacity))

                EvalStackValueComparisons.ceq PointerHashState.empty viaInt64 viaNativeInt

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.choose (0, System.Int32.MaxValue / 2))) property)

    [<Test>]
    let ``tryByteAddressDeltaSign accepts canonical byrefs at distinct array indices`` () : unit =
        let _, arr = stateWithIntArray (List.init 8 id)

        let extractByref (esv : EvalStackValue) : ManagedPointerSource =
            match esv with
            | EvalStackValue.ManagedPointer mp -> mp
            | other -> failwith $"expected managed pointer, got %O{other}"

        let p0 = extractByref (byteViewPointer arr 0 0)
        let p1Plus3 = extractByref (byteViewPointer arr 1 3)

        match ManagedPointerSource.tryByteAddressDeltaSign p0 p1Plus3 with
        | Some sign when sign > 0 -> ()
        | other -> failwith $"expected positive sign for arr[0] -> arr[1]+3, got %O{other}"

        match ManagedPointerSource.tryByteAddressDeltaSign p1Plus3 p0 with
        | Some sign when sign < 0 -> ()
        | other -> failwith $"expected negative sign for arr[1]+3 -> arr[0], got %O{other}"

    [<Test>]
    let ``tryByteAddressDeltaSign throws on a non-canonical negative trailing byte cursor`` () : unit =
        let _, arr = stateWithIntArray (List.init 4 id)

        let canonical = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 0), [])

        // Manually construct a malformed pointer: the trailing ByteOffset
        // bypasses normaliseTrailingByteOffset's floor-division and is negative.
        // tryByteAddressDeltaSign's array fallback's correctness depends on each
        // residual sitting in [0, cellSize); a negative residual indicates a
        // construction-site bug and must not silently degrade to a wrong sign.
        let malformed =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arr, 1),
                [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset -1 ]
            )

        let outcome =
            try
                ManagedPointerSource.tryByteAddressDeltaSign canonical malformed |> ignore
                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected tryByteAddressDeltaSign to throw on negative trailing byte cursor"
        | Choice2Of2 e when e.Message.Contains "non-negative" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception: %s{e.Message}"

    [<Test>]
    let ``tryByteAddressDeltaSign throws on a ByteOffset at a non-trailing position`` () : unit =
        let _, arr = stateWithIntArray (List.init 4 id)

        let canonical = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 0), [])

        // ByrefProjection.ByteOffset is documented to only appear as the final
        // element preceded by ReinterpretAs. A non-trailing ByteOffset is an
        // invariant violation; the helper must throw rather than silently
        // returning a wrong sign.
        let malformed =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arr, 1),
                [
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 1
                    ByrefProjection.ReinterpretAs byteType
                ]
            )

        let outcome =
            try
                ManagedPointerSource.tryByteAddressDeltaSign canonical malformed |> ignore
                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected tryByteAddressDeltaSign to throw on non-trailing ByteOffset"
        | Choice2Of2 e when e.Message.Contains "non-trailing" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception: %s{e.Message}"

    [<Test>]
    let ``tryByteAddressDeltaSign throws on a trailing ByteOffset without ReinterpretAs`` () : unit =
        let _, arr = stateWithIntArray (List.init 4 id)

        let canonical = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 0), [])

        // A trailing ByteOffset must be preceded by ReinterpretAs (the byte
        // cursor is on top of a byte view of the cell). Without that pairing
        // the projection list is malformed.
        let malformed =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 1), [ ByrefProjection.ByteOffset 1 ])

        let outcome =
            try
                ManagedPointerSource.tryByteAddressDeltaSign canonical malformed |> ignore
                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () ->
            failwith "expected tryByteAddressDeltaSign to throw on trailing ByteOffset without ReinterpretAs"
        | Choice2Of2 e when e.Message.Contains "preceded by ReinterpretAs" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception: %s{e.Message}"

    // ---------------------------------------------------------------------
    // Byrefs that address one whole typed slot: `&local`, `&arg`,
    // `&staticField`, and a box's interior pointer.
    // ---------------------------------------------------------------------

    /// A `ComparableFieldDefinitionHandle` standing in for a real static field row. These tests
    /// never resolve it: the static-slot byref is only ever classified and (at a zero offset)
    /// handed straight back.
    let private syntheticFieldHandle : ComparableFieldDefinitionHandle =
        ComparableFieldDefinitionHandle.Make Unchecked.defaultof<System.Reflection.Metadata.FieldDefinitionHandle>

    /// The four roots that address a whole typed value, paired with a name for failure messages.
    /// Kept as one list so a root that stops being classified as a whole-value target fails here
    /// rather than only in whichever guest program happens to exercise it.
    let private wholeValueRoots : (string * ByrefRoot) list =
        [
            "local variable", ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 3, 1us)
            "argument", ByrefRoot.Argument (ThreadId.ThreadId 0, FrameId.FrameId 3, 2us)
            "static field", ByrefRoot.StaticField (int32Handle, syntheticFieldHandle, StaticOwner.Shared)
            "boxed value", ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 12)
        ]

    /// A two-`int` struct under default layout, so `A` sits at offset 0 and `B` at offset 4.
    let private pairValueType (state : IlMachineState) (a : int) (b : int) : CliValueType =
        let field (name : string) (value : int) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 value)
                Offset = None
                Type = int32Handle
                MarshallingDescriptor = None
            }

        [ field "A" a ; field "B" b ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    /// A boxed two-`int` struct, plus the byref to the whole boxed value.
    let private stateWithBoxedPair () : IlMachineState * ManagedPointerSource =
        let state = state ()
        let contents = pairValueType state 111 222

        let addr, state = IlMachineState.allocateManagedObject int32Handle contents state

        state, ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

    [<Test>]
    let ``adding zero to a whole-slot byref returns the very same byref`` () : unit =
        let state = state ()

        for name, root in wholeValueRoots do
            let ptr = ManagedPointerSource.Byref (root, [])

            // Resolving `p + 0` to the field at offset 0 would give one address two structural
            // forms, and `ceq` / `Unsafe.AreSame` compare structurally. `sub` routes through the
            // same helper with a negated offset, so it must agree.
            for opName, op in [ "add", ArithmeticOperation.add ; "sub", ArithmeticOperation.sub ] do
                for offset in
                    [
                        EvalStackValue.Int32 (Int32Source.Verbatim 0)
                        EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
                    ] do
                    match execute op state (EvalStackValue.ManagedPointer ptr) offset with
                    | EvalStackValue.ManagedPointer actual -> actual |> shouldEqual ptr
                    | other ->
                        failwith $"expected %s{opName} of zero on a %s{name} byref to give back %O{ptr}, got %O{other}"

    /// Offsets that land exactly on a field boundary as well as ones that land inside a field.
    /// Both must give a byte cursor: an offset moves an address, and the access width arrives
    /// later with the dereference. Resolving the on-boundary case to that field instead would
    /// read one field's width where the guest asked for another's.
    [<TestCase(1)>]
    [<TestCase(4)>]
    [<TestCase(7)>]
    let ``offsetting a whole-slot byref gives a byte cursor, on field boundaries included`` (offset : int) : unit =
        let state, ptr = stateWithBoxedPair ()

        match
            execute
                ArithmeticOperation.add
                state
                (EvalStackValue.ManagedPointer ptr)
                (EvalStackValue.Int32 (Int32Source.Verbatim offset))
        with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (root,
                                                                     [ ByrefProjection.ReinterpretAs viewType
                                                                       ByrefProjection.ByteOffset actualOffset ])) ->
            viewType.Name |> shouldEqual "Byte"
            actualOffset |> shouldEqual offset

            root
            |> shouldEqual (
                match ptr with
                | ManagedPointerSource.Byref (root, []) -> root
                | other -> failwith $"test set-up produced an unexpected pointer %O{other}"
            )
        | other -> failwith $"expected a byte cursor %d{offset} bytes in, got %O{other}"

    [<Test>]
    let ``offsetting a whole-slot byref and back returns to the original byref`` () : unit =
        let state, ptr = stateWithBoxedPair ()

        let advanced =
            match
                execute
                    ArithmeticOperation.add
                    state
                    (EvalStackValue.ManagedPointer ptr)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 4))
            with
            | EvalStackValue.ManagedPointer advanced -> advanced
            | other -> failwith $"expected a managed pointer, got %O{other}"

        let returned =
            match
                execute
                    ArithmeticOperation.sub
                    state
                    (EvalStackValue.ManagedPointer advanced)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 4))
            with
            | EvalStackValue.ManagedPointer returned -> returned
            | other -> failwith $"expected a managed pointer, got %O{other}"

        // The round trip need not be structurally identical — it keeps the byte view it
        // acquired, and a byte view of the whole slot at offset zero is the same address as
        // the slot. What must hold is what the guest can observe: `ceq` / `Unsafe.AreSame`
        // must say these are the same pointer.
        let normalise (p : ManagedPointerSource) : NormalisedManagedPointerSource =
            ManagedPointerSource.normaliseForComparison
                (ManagedPointerByteView.normalisationContextForPointer state p)
                p

        ManagedPointerSource.ceqNormalised "round trip test" (normalise returned) (normalise ptr)
        |> shouldEqual true

        // And the byte cursor really is back at zero rather than merely comparing equal by
        // some looser rule: nothing but a zero offset survives normalisation.
        match returned with
        | ManagedPointerSource.Byref (_, [])
        | ManagedPointerSource.Byref (_, [ ByrefProjection.ReinterpretAs _ ]) -> ()
        | other -> failwith $"expected the round trip to leave no byte offset behind, got %O{other}"

    [<Test>]
    let ``a byte cursor onto a whole slot reads and writes at the full access width`` () : unit =
        let state, ptr = stateWithBoxedPair ()

        // `B` is the second `int`, four bytes in. Reading through the cursor must produce
        // `B`'s whole value rather than its first byte, and writing must land on `B` alone.
        let cursor =
            match
                execute
                    ArithmeticOperation.add
                    state
                    (EvalStackValue.ManagedPointer ptr)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 4))
            with
            | EvalStackValue.ManagedPointer cursor -> cursor
            | other -> failwith $"expected a managed pointer, got %O{other}"

        IlMachineState.readManagedByrefBytesAs baseClassTypes state cursor (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 222))

        let state =
            IlMachineState.writeManagedByrefWithBase
                baseClassTypes
                state
                cursor
                (CliType.Numeric (CliNumericType.Int32 333))

        // Assert values, not storage shape: writing through a byte cursor may re-back the
        // struct's storage, and that is not something a guest can observe. What it can observe
        // is that `B` changed and `A` did not.
        let readField (name : string) : CliType =
            IlMachineState.readManagedByref baseClassTypes state ptr
            |> CliType.getFieldById (FieldId.named name)

        readField "B" |> shouldEqual (CliType.Numeric (CliNumericType.Int32 333))
        readField "A" |> shouldEqual (CliType.Numeric (CliNumericType.Int32 111))

    [<Test>]
    let ``a whole PE byte range refuses pointer arithmetic`` () : unit =
        let peByteRange =
            {
                AssemblyFullName = "Example"
                Source = PeByteRangePointerSource.FieldRva syntheticFieldHandle
                RelativeVirtualAddress = 4096
                Size = 8
            }

        let ptr = ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, [])
        let state = state ()

        // A PE byte range is byte-addressed, so field resolution would be a lie about the
        // storage. Nothing needs it yet, so it must fail rather than guess.
        let outcome =
            try
                execute
                    ArithmeticOperation.add
                    state
                    (EvalStackValue.ManagedPointer ptr)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 4))
                |> ignore

                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected pointer arithmetic on a whole PE byte range to throw"
        | Choice2Of2 e when e.Message.Contains "needs a byte cursor" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception: %s{e.Message}"

    [<Test>]
    let ``a RuntimeType cache cell refuses pointer arithmetic`` () : unit =
        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.ExposedClassObject (RuntimeTypeHandleTarget.Closed int32Handle), [])

        let state = state ()

        // The cell holds one object reference; there is no interior to offset into.
        let outcome =
            try
                execute
                    ArithmeticOperation.add
                    state
                    (EvalStackValue.ManagedPointer ptr)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 8))
                |> ignore

                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected pointer arithmetic on a RuntimeType cache cell to throw"
        | Choice2Of2 e when e.Message.Contains "no interior to address" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception: %s{e.Message}"

    /// The zero offset is the identity, and a slot whose value has no byte image can be written
    /// through either spelling: the two must agree. Zero-offset identity is canonicalisation —
    /// one byte location, one structural form — which is what the first half checks.
    [<Test>]
    let ``a whole-slot byref and a zero-length byte cursor write an imageless value alike`` () : unit =
        let state = state ()

        // A managed pointer has no bit pattern in this interpreter — it is a root plus
        // projections — so a struct holding one has no byte image. Object references, by
        // contrast, *do* serialise, which is why this test uses a byref field and not one of
        // those.
        let heldPointer =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 99), [])

        let imageless =
            [
                {
                    Id = FieldId.named "Reference"
                    Name = "Reference"
                    Contents = CliType.RuntimePointer (CliRuntimePointer.Managed heldPointer)
                    Offset = None
                    Type = AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.IntPtr
                    MarshallingDescriptor = None
                }
            ]
            |> SynthesisedLayoutKind.ofFields
                baseClassTypes
                state.ConcreteTypes
                int32Handle
                Layout.Default
                System.Runtime.InteropServices.CharSet.Ansi

        let addr, state = IlMachineState.allocateManagedObject int32Handle imageless state
        let ptr = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        // Guard the premise: if this storage acquired a byte image, the test would pass while
        // covering nothing.
        let toBytes =
            try
                CliType.ToBytes (CliType.ValueType imageless) |> ignore
                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match toBytes with
        | Choice1Of2 () -> failwith "expected a value type with an object-reference field to have no byte image"
        | Choice2Of2 _ -> ()

        // `&slot + 0` is the identity, so the write below goes through the whole slot.
        match
            execute
                ArithmeticOperation.add
                state
                (EvalStackValue.ManagedPointer ptr)
                (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        with
        | EvalStackValue.ManagedPointer actual -> actual |> shouldEqual ptr
        | other -> failwith $"expected the identity at offset zero, got %O{other}"

        let throughSlot =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state ptr (CliType.ValueType imageless)

        // The same write through a zero-length byte cursor — the form a non-identity zero offset
        // would have produced. It names the same single cell, so it must land the same value.
        let cursor =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [ ByrefProjection.ReinterpretAs byteType ])

        let throughCursor =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state cursor (CliType.ValueType imageless)

        let stored (state : IlMachineState) : CliType =
            (ManagedHeap.get addr state.ManagedHeap).Contents
            |> CliValueType.DereferenceField "Reference"

        stored throughCursor |> shouldEqual (stored throughSlot)

        stored throughSlot
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.Managed heldPointer))

    [<Test>]
    let ``subtracting a byte cursor and its own whole slot gives the byte delta`` () : unit =
        let state, ptr = stateWithBoxedPair ()

        let cursor =
            match
                execute
                    ArithmeticOperation.add
                    state
                    (EvalStackValue.ManagedPointer ptr)
                    (EvalStackValue.Int32 (Int32Source.Verbatim 4))
            with
            | EvalStackValue.ManagedPointer cursor -> cursor
            | other -> failwith $"expected a managed pointer, got %O{other}"

        // Advancing a whole-slot pointer has to be measurable, or `int* q = p + 1; q - p;`
        // would work in one direction only. The slot's own address is the zero point.
        execute ArithmeticOperation.sub state (EvalStackValue.ManagedPointer cursor) (EvalStackValue.ManagedPointer ptr)
        |> expectNativeInt 4L

        execute ArithmeticOperation.sub state (EvalStackValue.ManagedPointer ptr) (EvalStackValue.ManagedPointer cursor)
        |> expectNativeInt -4L

        execute ArithmeticOperation.sub state (EvalStackValue.ManagedPointer ptr) (EvalStackValue.ManagedPointer ptr)
        |> expectNativeInt 0L

    [<Test>]
    let ``subtracting pointers to two distinct whole slots is refused`` () : unit =
        let state = state ()

        let first =
            ManagedPointerSource.Byref (ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 3, 0us), [])

        let second =
            ManagedPointerSource.Byref (ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 3, 1us), [])

        // Two locals are separate storage here, not offsets into one address space, so there
        // is no byte distance to report. Inventing one would let a guest compute a difference
        // the real runtime would never produce.
        let outcome =
            try
                execute
                    ArithmeticOperation.sub
                    state
                    (EvalStackValue.ManagedPointer first)
                    (EvalStackValue.ManagedPointer second)
                |> ignore

                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected subtracting pointers to two distinct slots to throw"
        | Choice2Of2 e when e.Message.Contains "two distinct whole storage slots" -> ()
        | Choice2Of2 e -> failwith $"unexpected exception: %s{e.Message}"

    /// The relaxation that lets a cursor into an argument slot be measured must not let
    /// *unrelated* argument pointers through: two argument slots are separate storage, and an
    /// argument paired with anything else has no distance either.
    [<Test>]
    let ``subtracting unrelated argument pointers stays refused`` () : unit =
        let state = state ()
        let frame = FrameId.FrameId 3

        let argument (index : uint16) : ManagedPointerSource =
            ManagedPointerSource.Byref (ByrefRoot.Argument (ThreadId.ThreadId 0, frame, index), [])

        let local =
            ManagedPointerSource.Byref (ByrefRoot.LocalVariable (ThreadId.ThreadId 0, frame, 0us), [])

        let cases =
            [
                "two different argument slots", argument 0us, argument 1us
                "an argument and a local", argument 0us, local
                "a local and an argument", local, argument 0us
            ]

        for name, left, right in cases do
            let outcome =
                try
                    execute
                        ArithmeticOperation.sub
                        state
                        (EvalStackValue.ManagedPointer left)
                        (EvalStackValue.ManagedPointer right)
                    |> ignore

                    Choice1Of2 ()
                with e ->
                    Choice2Of2 e

            match outcome with
            | Choice1Of2 () -> failwith $"expected subtracting %s{name} to throw"
            | Choice2Of2 e when e.Message.Contains "pointers to arguments" -> ()
            | Choice2Of2 e -> failwith $"unexpected exception for %s{name}: %s{e.Message}"

    /// Two byte offsets accumulate by summing, and the cancellation check must not be phrased as
    /// `n = -m`: this codebase is `Checked`, so negating `Int32.MinValue` throws a host
    /// `OverflowException` even where the sum is representable. Reachable from a guest as
    /// `byte* q = p + int.MinValue; q += 1;`, which real .NET completes without complaint.
    [<TestCase(System.Int32.MinValue, 1, -2147483647)>]
    [<TestCase(1, System.Int32.MinValue, -2147483647)>]
    [<TestCase(System.Int32.MaxValue, -1, 2147483646)>]
    [<TestCase(7, -7, 0)>]
    [<TestCase(System.Int32.MinValue, 0, System.Int32.MinValue)>]
    let ``accumulating byte offsets sums them without negating either`` (first : int) (second : int) (expected : int) =
        let state, ptr = stateWithBoxedPair ()

        let advance (offset : int) (p : ManagedPointerSource) : ManagedPointerSource =
            match
                execute
                    ArithmeticOperation.add
                    state
                    (EvalStackValue.ManagedPointer p)
                    (EvalStackValue.Int32 (Int32Source.Verbatim offset))
            with
            | EvalStackValue.ManagedPointer result -> result
            | other -> failwith $"expected a managed pointer, got %O{other}"

        let actual = ptr |> advance first |> advance second

        // A zero total normalises the cursor away entirely, which is what makes a round trip
        // return to its starting pointer.
        match actual with
        | ManagedPointerSource.Byref (_, [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset offset ]) ->
            offset |> shouldEqual expected
            expected |> shouldNotEqual 0
        | ManagedPointerSource.Byref (_, [ ByrefProjection.ReinterpretAs _ ])
        | ManagedPointerSource.Byref (_, []) -> expected |> shouldEqual 0
        | other -> failwith $"expected a byte cursor or a bare slot pointer, got %O{other}"
