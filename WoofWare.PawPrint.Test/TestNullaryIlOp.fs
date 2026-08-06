namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNullaryIlOp =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    type private Int32DivUnCase =
        {
            Numerator : int32
            Denominator : int32
        }

    type private Int64DivUnCase =
        {
            Numerator : int64
            Denominator : int64
        }

    [<RequireQualifiedAccess>]
    type private NativeIntNegInput =
        | Verbatim of int64
        | SyntheticCrossArrayOffset of SyntheticCrossArrayOffset
        | ManagedPointerNull

    [<RequireQualifiedAccess>]
    type private NegCase =
        | Int32Value of int32
        | Int64Value of int64
        | NativeIntValue of NativeIntNegInput
        | FloatValue of float

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithNullary
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : NullaryIlOp)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectToString =
            baseClassTypes.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let op = IlOp.Nullary op

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            { MethodInstructions.onlyRet () with
                Instructions = [ op, 0 ]
                Locations = Map.empty |> Map.add 0 op
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il instructions) signature

        state, method

    let private stateWithNullary
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : NullaryIlOp)
        (stackValue : EvalStackValue)
        : IlMachineState * ThreadId
        =
        let state, method = initialState loggerFactory |> methodWithNullary loggerFactory op

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating nullary-op test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New (CpuId 0) methodState)
            }
            |> IlMachineState.pushToEvalStack' stackValue thread

        state, thread

    let private genNonZeroInt32Bits : Gen<int32> =
        gen {
            let! highBit = ArbMap.defaults |> ArbMap.generate<bool>
            let! lowBits = Gen.choose (1, System.Int32.MaxValue)

            if highBit then
                return int32 (uint32 lowBits ||| 0x80000000u)
            else
                return int32 lowBits
        }

    let private genInt32DivUnCase : Gen<Int32DivUnCase> =
        gen {
            let! numerator = ArbMap.defaults |> ArbMap.generate<int32>
            let! denominator = genNonZeroInt32Bits

            return
                {
                    Numerator = numerator
                    Denominator = denominator
                }
        }

    let private genNonZeroInt64Bits : Gen<int64> =
        gen {
            let! highBit = ArbMap.defaults |> ArbMap.generate<bool>
            let! raw = ArbMap.defaults |> ArbMap.generate<int64>
            let lowBits = raw &&& System.Int64.MaxValue

            let candidate =
                if highBit then
                    lowBits ||| System.Int64.MinValue
                else
                    lowBits

            return if candidate = 0L then 1L else candidate
        }

    let private genInt32NegCase : Gen<NegCase> =
        let arbitrary = ArbMap.defaults |> ArbMap.generate<int32>

        let edges = Gen.elements [ Int32.MinValue ; Int32.MaxValue ; -1 ; 0 ; 1 ]

        Gen.frequency [ 8, arbitrary ; 2, edges ] |> Gen.map NegCase.Int32Value

    let private genInt64NegCase : Gen<NegCase> =
        let arbitrary = ArbMap.defaults |> ArbMap.generate<int64>

        let edges = Gen.elements [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ]

        Gen.frequency [ 8, arbitrary ; 2, edges ] |> Gen.map NegCase.Int64Value

    let private syntheticStorageIdentities : ByteStorageIdentity array =
        [|
            ByteStorageIdentity.Array (ManagedHeapAddress 201)
            ByteStorageIdentity.String (ManagedHeapAddress 202)
            ByteStorageIdentity.StackMemory (ThreadId 0, FrameId 20, StackMemoryBlockId 0)
            ByteStorageIdentity.StackLocal (ThreadId 0, FrameId 21, 1us)
            ByteStorageIdentity.StackArgument (ThreadId 0, FrameId 22, 2us)
        |]

    let private genSyntheticCrossArrayOffset : Gen<SyntheticCrossArrayOffset> =
        gen {
            let! sourceIndex = Gen.choose (0, syntheticStorageIdentities.Length - 1)
            let! distance = Gen.choose (1, syntheticStorageIdentities.Length - 1)
            let targetIndex = (sourceIndex + distance) % syntheticStorageIdentities.Length
            let! sourceOffset = ArbMap.defaults |> ArbMap.generate<int64>
            let! targetOffset = ArbMap.defaults |> ArbMap.generate<int64>

            return
                SyntheticCrossArrayOffset.make
                    syntheticStorageIdentities.[targetIndex]
                    targetOffset
                    syntheticStorageIdentities.[sourceIndex]
                    sourceOffset
        }

    let private genNativeIntNegCase : Gen<NegCase> =
        let genBits : Gen<int64> =
            Gen.frequency
                [
                    8, ArbMap.defaults |> ArbMap.generate<int64>
                    2, Gen.elements [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ]
                ]

        Gen.frequency
            [
                8, genBits |> Gen.map (NativeIntNegInput.Verbatim >> NegCase.NativeIntValue)
                2,
                genSyntheticCrossArrayOffset
                |> Gen.map (NativeIntNegInput.SyntheticCrossArrayOffset >> NegCase.NativeIntValue)
                1, Gen.constant (NegCase.NativeIntValue NativeIntNegInput.ManagedPointerNull)
            ]

    let private genFloatNegCase : Gen<NegCase> =
        let finite =
            gen {
                let! numerator = Gen.choose (-1_000_000_000, 1_000_000_000)
                let! denominator = Gen.choose (1, 1_000)

                return float numerator / float denominator
            }

        let edges =
            Gen.elements
                [
                    0.0
                    -0.0
                    Double.Epsilon
                    -Double.Epsilon
                    Double.MaxValue
                    -Double.MaxValue
                    Double.PositiveInfinity
                    Double.NegativeInfinity
                    Double.NaN
                ]

        Gen.frequency [ 8, finite ; 2, edges ] |> Gen.map NegCase.FloatValue

    let private genNegCase : Gen<NegCase> =
        Gen.oneof [ genInt32NegCase ; genInt64NegCase ; genNativeIntNegCase ; genFloatNegCase ]

    let private genInt64DivUnCase : Gen<Int64DivUnCase> =
        gen {
            let! numerator = ArbMap.defaults |> ArbMap.generate<int64>
            let! denominator = genNonZeroInt64Bits

            return
                {
                    Numerator = numerator
                    Denominator = denominator
                }
        }

    let private expectedNegInt32 (value : int32) : int32 = -value

    let private expectedNegInt64 (value : int64) : int64 = -value

    let private negCaseValues (case : NegCase) : EvalStackValue * EvalStackValue =
        match case with
        | NegCase.Int32Value value -> EvalStackValue.Int32 value, EvalStackValue.Int32 (expectedNegInt32 value)
        | NegCase.Int64Value value ->
            EvalStackValue.Int64 (Int64Source.Verbatim value),
            EvalStackValue.Int64 (Int64Source.Verbatim (expectedNegInt64 value))
        | NegCase.NativeIntValue nativeInt ->
            match nativeInt with
            | NativeIntNegInput.Verbatim value ->
                EvalStackValue.NativeInt (NativeIntSource.Verbatim value),
                EvalStackValue.NativeInt (NativeIntSource.Verbatim (expectedNegInt64 value))
            | NativeIntNegInput.SyntheticCrossArrayOffset value ->
                EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset value),
                EvalStackValue.NativeInt (
                    NativeIntSource.SyntheticCrossArrayOffset (SyntheticCrossArrayOffset.negate value)
                )
            | NativeIntNegInput.ManagedPointerNull ->
                EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null),
                EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
        | NegCase.FloatValue value -> EvalStackValue.Float value, EvalStackValue.Float (-value)

    let private assertEvalStackValueEqual (expected : EvalStackValue) (actual : EvalStackValue) : unit =
        match expected, actual with
        | EvalStackValue.Float expected, EvalStackValue.Float actual ->
            BitConverter.DoubleToInt64Bits actual
            |> shouldEqual (BitConverter.DoubleToInt64Bits expected)
        | _ -> actual |> shouldEqual expected

    let private negEdgeCases : NegCase list =
        [
            NegCase.Int32Value Int32.MinValue
            NegCase.Int32Value Int32.MaxValue
            NegCase.Int32Value -1
            NegCase.Int32Value 0
            NegCase.Int32Value 1
            NegCase.Int64Value Int64.MinValue
            NegCase.Int64Value Int64.MaxValue
            NegCase.Int64Value -1L
            NegCase.Int64Value 0L
            NegCase.Int64Value 1L
            NegCase.NativeIntValue (NativeIntNegInput.Verbatim Int64.MinValue)
            NegCase.NativeIntValue (NativeIntNegInput.Verbatim Int64.MaxValue)
            NegCase.NativeIntValue (NativeIntNegInput.Verbatim -1L)
            NegCase.NativeIntValue (NativeIntNegInput.Verbatim 0L)
            NegCase.NativeIntValue (NativeIntNegInput.Verbatim 1L)
            NegCase.NativeIntValue (
                NativeIntNegInput.SyntheticCrossArrayOffset (
                    SyntheticCrossArrayOffset.make
                        syntheticStorageIdentities.[0]
                        Int64.MinValue
                        syntheticStorageIdentities.[1]
                        Int64.MaxValue
                )
            )
            NegCase.NativeIntValue (
                NativeIntNegInput.SyntheticCrossArrayOffset (
                    SyntheticCrossArrayOffset.make syntheticStorageIdentities.[0] -1L syntheticStorageIdentities.[2] 1L
                )
            )
            NegCase.NativeIntValue (
                NativeIntNegInput.SyntheticCrossArrayOffset (
                    SyntheticCrossArrayOffset.make syntheticStorageIdentities.[3] 0L syntheticStorageIdentities.[4] 0L
                )
            )
            NegCase.NativeIntValue NativeIntNegInput.ManagedPointerNull
            NegCase.FloatValue 0.0
            NegCase.FloatValue -0.0
            NegCase.FloatValue Double.Epsilon
            NegCase.FloatValue -Double.Epsilon
            NegCase.FloatValue Double.MaxValue
            NegCase.FloatValue -Double.MaxValue
            NegCase.FloatValue Double.PositiveInfinity
            NegCase.FloatValue Double.NegativeInfinity
            NegCase.FloatValue Double.NaN
        ]

    let private executeNegCase (case : NegCase) : unit =
        let input, expected = negCaseValues case
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithNullary loggerFactory NullaryIlOp.Neg input

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Neg with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let methodState = state.ThreadState.[thread].MethodState
            let actualStack = methodState.EvaluationStack.Values

            match actualStack with
            | [ actual ] -> assertEvalStackValueEqual expected actual
            | other -> failwith $"Expected Neg to leave one stack value, got %O{other}"

            methodState.IlOpIndex
            |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary NullaryIlOp.Neg))
        | other -> failwith $"Expected Neg to step, got %O{other}"

    [<Test>]
    let ``Div_un on int32 follows unsigned 32-bit division`` () : unit =
        let mutable highBitDenominators = 0
        let mutable lowBitDenominators = 0

        let property (case : Int32DivUnCase) : unit =
            if uint32 case.Denominator >= 0x80000000u then
                highBitDenominators <- highBitDenominators + 1
            else
                lowBitDenominators <- lowBitDenominators + 1

            let expected = uint32 case.Numerator / uint32 case.Denominator |> int32<uint32>

            match
                NullaryIlOp.divUnValues (EvalStackValue.Int32 case.Numerator) (EvalStackValue.Int32 case.Denominator)
            with
            | EvalStackValue.Int32 actual -> actual |> shouldEqual expected
            | other -> failwith $"Expected Int32 Div_un result, got %O{other}"

        Check.One (config, Prop.forAll (Arb.fromGen genInt32DivUnCase) property)

        if highBitDenominators < 100 || lowBitDenominators < 100 then
            failwith
                $"Div_un int32 generator was unbalanced: high-bit denominators %d{highBitDenominators}, low-bit denominators %d{lowBitDenominators}"

    [<Test>]
    let ``Div_un on native int follows unsigned native-width division`` () : unit =
        let mutable highBitDenominators = 0
        let mutable lowBitDenominators = 0

        let property (case : Int64DivUnCase) : unit =
            if case.Denominator < 0L then
                highBitDenominators <- highBitDenominators + 1
            else
                lowBitDenominators <- lowBitDenominators + 1

            let expected =
                uint64<int64> case.Numerator / uint64<int64> case.Denominator |> int64<uint64>

            match
                NullaryIlOp.divUnValues
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim case.Numerator))
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim case.Denominator))
            with
            | EvalStackValue.NativeInt (NativeIntSource.Verbatim actual) -> actual |> shouldEqual expected
            | other -> failwith $"Expected native int Div_un result, got %O{other}"

        Check.One (config, Prop.forAll (Arb.fromGen genInt64DivUnCase) property)

        if highBitDenominators < 100 || lowBitDenominators < 100 then
            failwith
                $"Div_un native-int generator was unbalanced: high-bit denominators %d{highBitDenominators}, low-bit denominators %d{lowBitDenominators}"

    [<Test>]
    let ``Neg executes unchecked numeric negation`` () : unit =
        for case in negEdgeCases do
            executeNegCase case

        Check.One (config, Prop.forAll (Arb.fromGen genNegCase) executeNegCase)

    /// PawPrint deliberately ignores `tail.` (see NullaryIlOp.fs), so the only thing it
    /// may do is step over its own two bytes: the arguments the following call will pop
    /// must be left untouched, and no prefix state may be left behind on the frame for a
    /// later instruction to trip over.
    [<Test>]
    let ``Tail is an executed no-op that steps over its two-byte encoding`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let input = EvalStackValue.Int32 42
        let state, thread = stateWithNullary loggerFactory NullaryIlOp.Tail input

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Tail with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let methodState = state.ThreadState.[thread].MethodState
            methodState.EvaluationStack.Values |> shouldEqual [ input ]
            methodState.PendingPrefix |> shouldEqual PrefixState.empty

            methodState.IlOpIndex
            |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary NullaryIlOp.Tail))
        | other -> failwith $"Expected Tail to step, got %O{other}"

    /// A `conv.ovf.i` source operand, together with enough structure to say what the
    /// answer must be. Numeric sources are checked against the host's own `conv.ovf.i`
    /// (`Checked.nativeint`, which the host emits as that exact opcode); tagged sources
    /// have no numeric oracle, and the property is instead that the tag survives.
    [<RequireQualifiedAccess>]
    type private ConvOvfICase =
        | Int32Value of int32
        | Int64Value of int64
        | NativeIntVerbatim of int64
        | FloatValue of float
        | Int64CrossArrayOffset of SyntheticCrossArrayOffset
        | NativeIntCrossArrayOffset of SyntheticCrossArrayOffset
        | Int64OpaqueHashBits of int64
        | NativeIntOpaqueHashBits of int64
        /// `conv.i8` / `conv.u8` of a pointer-shaped native int, narrowed back again.
        | WidenedPointer of signed : bool
        | ManagedPointerNull
        | NativeIntManagedPointerNull
        | NullObjectRef

    /// The host runs `conv.ovf.i` for us: F#'s `Checked.nativeint` compiles to that
    /// opcode, and the host is the same 64-bit width PawPrint models, so it is a true
    /// oracle for the numeric source shapes rather than a restatement of our code.
    let private hostConvOvfIFromInt64 (value : int64) : Result<int64, unit> =
        try
            Checked.nativeint value |> int64 |> Ok
        with :? OverflowException ->
            Error ()

    let private hostConvOvfIFromFloat (value : float) : Result<int64, unit> =
        try
            Checked.nativeint value |> int64 |> Ok
        with :? OverflowException ->
            Error ()

    /// The pointer shape used for the `WidenedNativeInt` round-trip. It is deliberately
    /// one the *bare* native-int path refuses: `conv.i8` of a method handle followed by
    /// `conv.ovf.i` must invert to the original handle (the widening is bit-preserving
    /// on 64 bits), whereas a method handle arriving directly at `conv.ovf.i` has no
    /// observed call site and so fails loudly. `Conv_ovf_u` draws the line in the same
    /// place.
    let private widenedPointerSource : NativeIntSource =
        NativeIntSource.MethodHandlePtr 7L

    let private convOvfICaseInput (case : ConvOvfICase) : EvalStackValue =
        match case with
        | ConvOvfICase.Int32Value value -> EvalStackValue.Int32 value
        | ConvOvfICase.Int64Value value -> EvalStackValue.Int64 (Int64Source.Verbatim value)
        | ConvOvfICase.NativeIntVerbatim value -> EvalStackValue.NativeInt (NativeIntSource.Verbatim value)
        | ConvOvfICase.FloatValue value -> EvalStackValue.Float value
        | ConvOvfICase.Int64CrossArrayOffset offset ->
            EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset offset)
        | ConvOvfICase.NativeIntCrossArrayOffset offset ->
            EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset offset)
        | ConvOvfICase.Int64OpaqueHashBits bits -> EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits)
        | ConvOvfICase.NativeIntOpaqueHashBits bits -> EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits)
        | ConvOvfICase.WidenedPointer signed ->
            EvalStackValue.Int64 (Int64Source.widenedNativeInt widenedPointerSource signed)
        | ConvOvfICase.ManagedPointerNull -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
        | ConvOvfICase.NativeIntManagedPointerNull ->
            EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
        | ConvOvfICase.NullObjectRef -> EvalStackValue.NullObjectRef

    let private convOvfIExpected (case : ConvOvfICase) : Result<NativeIntSource, unit> =
        let ofVerbatim (r : Result<int64, unit>) : Result<NativeIntSource, unit> =
            r |> Result.map NativeIntSource.Verbatim

        match case with
        | ConvOvfICase.Int32Value value -> hostConvOvfIFromInt64 (int64 value) |> ofVerbatim
        | ConvOvfICase.Int64Value value -> hostConvOvfIFromInt64 value |> ofVerbatim
        | ConvOvfICase.NativeIntVerbatim value -> hostConvOvfIFromInt64 value |> ofVerbatim
        | ConvOvfICase.FloatValue value -> hostConvOvfIFromFloat value |> ofVerbatim
        | ConvOvfICase.Int64CrossArrayOffset offset -> NativeIntSource.SyntheticCrossArrayOffset offset |> Ok
        | ConvOvfICase.NativeIntCrossArrayOffset offset -> NativeIntSource.SyntheticCrossArrayOffset offset |> Ok
        | ConvOvfICase.Int64OpaqueHashBits bits -> NativeIntSource.OpaqueHashBits bits |> Ok
        | ConvOvfICase.NativeIntOpaqueHashBits bits -> NativeIntSource.OpaqueHashBits bits |> Ok
        | ConvOvfICase.WidenedPointer _ -> Ok widenedPointerSource
        | ConvOvfICase.ManagedPointerNull -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok
        | ConvOvfICase.NativeIntManagedPointerNull -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok
        | ConvOvfICase.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok

    /// Doubles chosen to sit on and around every boundary `conv.ovf.i` cares about:
    /// `2^63` (the smallest double above Int64.MaxValue), `-2^63` (exactly
    /// Int64.MinValue, and in range), the neighbouring representable doubles either
    /// side of both, and the truncate-toward-zero cases near 0.
    let private convOvfIFloatEdges : float list =
        [
            0.0
            -0.0
            0.5
            -0.5
            0.9999999999999999
            -0.9999999999999999
            1.0
            -1.0
            100.5
            -100.5
            Double.Epsilon
            -Double.Epsilon
            // 2^63 - 1024: the largest double <= Int64.MaxValue, and in range.
            9223372036854774784.0
            // 2^63: the smallest double > Int64.MaxValue; overflows.
            9223372036854775808.0
            // -2^63: exactly Int64.MinValue; in range.
            -9223372036854775808.0
            // -2^63 - 2048: the largest-magnitude double below Int64.MinValue that is
            // still the next one down; overflows.
            -9223372036854777856.0
            Double.MaxValue
            -Double.MaxValue
            Double.PositiveInfinity
            Double.NegativeInfinity
            Double.NaN
        ]

    let private genConvOvfIFloat : Gen<float> =
        let finite =
            gen {
                let! numerator = Gen.choose (-1_000_000_000, 1_000_000_000)
                let! denominator = Gen.choose (1, 1_000)

                return float numerator / float denominator
            }

        // Scaled around the 2^63 boundary, so draws land just inside and just outside
        // the representable range rather than always deep within it.
        let nearBoundary =
            gen {
                let! scale = Gen.choose (-2_000, 2_000)
                return 9223372036854775808.0 * (1.0 + float scale / 1000.0)
            }

        // Strictly outside `[-2^63, 2^63)` in both directions: always overflows.
        let outOfRange =
            gen {
                let! magnitude = Gen.choose (1, 1_000)
                let! negative = ArbMap.defaults |> ArbMap.generate<bool>
                let value = 9223372036854775808.0 * (1.0 + float magnitude / 1000.0)
                return if negative then -value else value
            }

        let fromInt64Bits = ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map float

        Gen.frequency
            [
                3, finite
                4, nearBoundary
                5, outOfRange
                2, fromInt64Bits
                3, Gen.elements convOvfIFloatEdges
            ]

    let private genConvOvfICase : Gen<ConvOvfICase> =
        let genInt32 =
            Gen.frequency
                [
                    8, ArbMap.defaults |> ArbMap.generate<int32>
                    2, Gen.elements [ Int32.MinValue ; Int32.MaxValue ; -1 ; 0 ; 1 ]
                ]

        let genInt64 =
            Gen.frequency
                [
                    8, ArbMap.defaults |> ArbMap.generate<int64>
                    2, Gen.elements [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ]
                ]

        Gen.frequency
            [
                3, genInt32 |> Gen.map ConvOvfICase.Int32Value
                3, genInt64 |> Gen.map ConvOvfICase.Int64Value
                3, genInt64 |> Gen.map ConvOvfICase.NativeIntVerbatim
                // Floats are weighted heavily: on a 64-bit interpreter they are the only
                // source shape that can overflow at all.
                10, genConvOvfIFloat |> Gen.map ConvOvfICase.FloatValue
                1, genSyntheticCrossArrayOffset |> Gen.map ConvOvfICase.Int64CrossArrayOffset
                1, genSyntheticCrossArrayOffset |> Gen.map ConvOvfICase.NativeIntCrossArrayOffset
                1, genInt64 |> Gen.map ConvOvfICase.Int64OpaqueHashBits
                1, genInt64 |> Gen.map ConvOvfICase.NativeIntOpaqueHashBits
                1, ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map ConvOvfICase.WidenedPointer
                1, Gen.constant ConvOvfICase.ManagedPointerNull
                1, Gen.constant ConvOvfICase.NativeIntManagedPointerNull
                1, Gen.constant ConvOvfICase.NullObjectRef
            ]

    let private convOvfIEdgeCases : ConvOvfICase list =
        [
            for value in [ Int32.MinValue ; Int32.MaxValue ; -1 ; 0 ; 1 ] do
                ConvOvfICase.Int32Value value
            for value in [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ] do
                ConvOvfICase.Int64Value value
                ConvOvfICase.NativeIntVerbatim value
                ConvOvfICase.Int64OpaqueHashBits value
                ConvOvfICase.NativeIntOpaqueHashBits value
            for value in convOvfIFloatEdges do
                ConvOvfICase.FloatValue value
            ConvOvfICase.WidenedPointer true
            ConvOvfICase.WidenedPointer false
            ConvOvfICase.ManagedPointerNull
            ConvOvfICase.NativeIntManagedPointerNull
            ConvOvfICase.NullObjectRef
            ConvOvfICase.Int64CrossArrayOffset (
                SyntheticCrossArrayOffset.make syntheticStorageIdentities.[0] 0L syntheticStorageIdentities.[1] 0L
            )
            ConvOvfICase.NativeIntCrossArrayOffset (
                SyntheticCrossArrayOffset.make syntheticStorageIdentities.[2] -1L syntheticStorageIdentities.[3] 1L
            )
        ]

    let private checkConvOvfICase (case : ConvOvfICase) : unit =
        NullaryIlOp.convOvfI (convOvfICaseInput case)
        |> shouldEqual (convOvfIExpected case)

    [<Test>]
    let ``Conv_ovf_i agrees with the host's checked conversion and preserves provenance`` () : unit =
        let mutable overflows = 0
        let mutable successes = 0

        let property (case : ConvOvfICase) : unit =
            match convOvfIExpected case with
            | Ok _ -> successes <- successes + 1
            | Error () -> overflows <- overflows + 1

            checkConvOvfICase case

        for case in convOvfIEdgeCases do
            property case

        Check.One (config, Prop.forAll (Arb.fromGen genConvOvfICase) property)

        // Guard against a generator that silently stops exercising one side: on a
        // 64-bit interpreter only floats can overflow `conv.ovf.i`, so an unbalanced
        // float generator would turn this into a success-path-only test. The generator
        // is tuned to produce roughly 80 overflows in 500 draws, so 30 is a wide
        // margin rather than a threshold the run can drift across.
        if overflows < 30 || successes < 30 then
            failwith $"Conv_ovf_i generator was unbalanced: %d{overflows} overflows, %d{successes} successes"

    [<Test>]
    let ``Conv_ovf_i pushes a native int and advances past its own encoding`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let input = EvalStackValue.Int64 (Int64Source.Verbatim -5L)
        let state, thread = stateWithNullary loggerFactory NullaryIlOp.Conv_ovf_i input

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Conv_ovf_i with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let methodState = state.ThreadState.[thread].MethodState

            methodState.EvaluationStack.Values
            |> shouldEqual [ EvalStackValue.NativeInt (NativeIntSource.Verbatim -5L) ]

            methodState.IlOpIndex
            |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary NullaryIlOp.Conv_ovf_i))
        | other -> failwith $"Expected Conv_ovf_i to step, got %O{other}"

    [<Test>]
    let ``Conv_ovf_i raises OverflowException without advancing the faulting PC`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        // 2^63 is the smallest double greater than Int64.MaxValue.
        let input = EvalStackValue.Float 9223372036854775808.0
        let state, thread = stateWithNullary loggerFactory NullaryIlOp.Conv_ovf_i input

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Conv_ovf_i with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let threadState = state.ThreadState.[thread]

            // The runtime has pushed a frame running `OverflowException..ctor`.
            let ctor = threadState.MethodState.ExecutingMethod
            ctor.Name |> shouldEqual ".ctor"

            let declaring = ctor.DeclaringType
            declaring.Namespace |> shouldEqual "System"
            declaring.Name |> shouldEqual "OverflowException"

            // Exception dispatch needs the faulting instruction's offset, so the frame
            // that executed `conv.ovf.i` must still be sitting on it. It is no longer
            // the active frame, so find it by frame id.
            let faultingFrame =
                threadState.MethodStates
                |> Map.toSeq
                |> Seq.filter (fun (frameId, _) -> frameId <> threadState.ActiveMethodState)
                |> Seq.exactlyOne
                |> snd

            faultingFrame.IlOpIndex |> shouldEqual 0
            faultingFrame.EvaluationStack.Values |> shouldEqual []
        | other -> failwith $"Expected Conv_ovf_i overflow to step, got %O{other}"

    // --- Bitwise operations on tagged GC handles ---
    //
    // PawPrint models a GC handle as an opaque registry index with no numeric
    // address, but CoreLib stores tag bits in the handle's low bits and strips
    // them off again on every read. These pin the arithmetic that makes that
    // possible without inventing an address; see `TestTaggedPointerBits` for the
    // decision procedure itself, and
    // docs/plans/2026-08-06-tagged-gc-handles.md for the CoreLib IL each case
    // corresponds to.

    let private gcHandleUnderTest : GcHandleAddress = GcHandleAddress.GcHandleAddress 7

    let private taggedHandle (tag : int64) : EvalStackValue =
        EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (gcHandleUnderTest, tag))

    let private stateWithBinaryNullary
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : NullaryIlOp)
        (first : EvalStackValue)
        (second : EvalStackValue)
        : IlMachineState * ThreadId
        =
        let state, thread = stateWithNullary loggerFactory op first
        IlMachineState.pushToEvalStack' second thread state, thread

    let private runBinary (op : NullaryIlOp) (first : EvalStackValue) (second : EvalStackValue) : EvalStackValue =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithBinaryNullary loggerFactory op first second

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | [ actual ] -> actual
            | other -> failwith $"Expected %O{op} to leave one stack value, got %O{other}"
        | other -> failwith $"Expected %O{op} to step, got %O{other}"

    /// `conv.i` of an `ldc.i4` constant, which is how CoreLib materialises its
    /// tag masks: the operand reaches `and`/`or` as a native int, not an int32.
    let private nativeConst (value : int64) : EvalStackValue =
        EvalStackValue.NativeInt (NativeIntSource.Verbatim value)

    [<Test>]
    let ``Or tags a GC handle, and And reads the tag back`` () : unit =
        // `WeakReference.Create`: `h | TracksResurrectionBit`.
        runBinary NullaryIlOp.Or (taggedHandle 0L) (nativeConst 1L)
        |> shouldEqual (taggedHandle 1L)

        // `WeakReference.IsTrackResurrection`: `_taggedHandle & 1`.
        runBinary NullaryIlOp.And (taggedHandle 1L) (nativeConst 1L)
        |> shouldEqual (nativeConst 1L)

        runBinary NullaryIlOp.And (taggedHandle 0L) (nativeConst 1L)
        |> shouldEqual (nativeConst 0L)

    [<Test>]
    let ``And strips tag bits and leaves a usable handle`` () : unit =
        // `WeakReference.get_Target`: `_taggedHandle & ~TracksResurrectionBit`.
        // The ComAware bit (2) must survive that mask, and be cleared by
        // `get_WeakHandle`'s wider `& ~HandleTagBits`.
        runBinary NullaryIlOp.And (taggedHandle 1L) (nativeConst ~~~1L)
        |> shouldEqual (taggedHandle 0L)

        runBinary NullaryIlOp.And (taggedHandle 3L) (nativeConst ~~~1L)
        |> shouldEqual (taggedHandle 2L)

        runBinary NullaryIlOp.And (taggedHandle 3L) (nativeConst ~~~3L)
        |> shouldEqual (taggedHandle 0L)

    [<Test>]
    let ``an int32 operand is sign-extended before it reaches the tag region`` () : unit =
        // An `int32` mask on the stack alongside a native int: `-2` must not be
        // read as `0x00000000FFFFFFFE`, which would clear the handle's high bits.
        runBinary NullaryIlOp.And (taggedHandle 3L) (EvalStackValue.Int32 -2)
        |> shouldEqual (taggedHandle 2L)

        runBinary NullaryIlOp.And (EvalStackValue.Int32 -2) (taggedHandle 3L)
        |> shouldEqual (taggedHandle 2L)

        runBinary NullaryIlOp.Or (EvalStackValue.Int32 1) (taggedHandle 0L)
        |> shouldEqual (taggedHandle 1L)

    [<Test>]
    let ``Xor flips tag bits rather than losing the handle to hash synthesis`` () : unit =
        runBinary NullaryIlOp.Xor (taggedHandle 1L) (nativeConst 3L)
        |> shouldEqual (taggedHandle 2L)

        runBinary NullaryIlOp.Xor (nativeConst 3L) (taggedHandle 1L)
        |> shouldEqual (taggedHandle 2L)

    [<Test>]
    let ``an operand reaching outside the tag region is refused loudly`` () : unit =
        // These ask about bits of the handle's address, which PawPrint does not
        // model. Answering would mean inventing them.
        let shouldRefuse (op : NullaryIlOp) (operand : EvalStackValue) : unit =
            let exn =
                Assert.Throws (fun () -> runBinary op (taggedHandle 1L) operand |> ignore<EvalStackValue>)

            exn.Message |> shouldContainText "which PawPrint does not model"

        shouldRefuse NullaryIlOp.And (nativeConst 4L)
        shouldRefuse NullaryIlOp.And (EvalStackValue.Int32 4)
        shouldRefuse NullaryIlOp.Or (nativeConst 8L)
        shouldRefuse NullaryIlOp.Or (EvalStackValue.Int32 8)
        shouldRefuse NullaryIlOp.Xor (nativeConst 4L)

    [<Test>]
    let ``dereferencing a tagged GC handle is refused loudly`` () : unit =
        // Release CoreLib's `GCHandle.InternalGet` is `*(object*)handle`, and
        // managed code always masks the tag off first. A tagged dereference would
        // be a misaligned read in reality.
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread =
            stateWithNullary loggerFactory NullaryIlOp.Ldind_ref (taggedHandle 1L)

        let exn =
            Assert.Throws (fun () ->
                NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Ldind_ref
                |> ignore<ExecutionResult>
            )

        exn.Message |> shouldContainText "refusing to dereference GC handle"

    [<Test>]
    let ``a GC handle InternalCall refuses a tagged handle`` () : unit =
        let exn =
            Assert.Throws (fun () ->
                NativeCall.gcHandleAddressOfEvalStackValue "test" (taggedHandle 1L)
                |> ignore<GcHandleAddress>
            )

        exn.Message |> shouldContainText "expected an untagged GC handle"

        NativeCall.gcHandleAddressOfEvalStackValue "test" (taggedHandle 0L)
        |> shouldEqual gcHandleUnderTest
