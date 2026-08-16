namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Emit
open System.Runtime.InteropServices
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
            |> List.find (fun method -> method.Name = "ToString" && (MethodInfo.arity method = 0))

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
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }
            |> IlMachineState.pushToEvalStack' stackValue thread

        state, thread

    /// `stateWithNullary`, but for the binary opcodes. `val1` goes on first, so the opcode pops
    /// `val2` as its right-hand operand — the divisor, for the four divisions.
    let private stateWithBinary
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : NullaryIlOp)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : IlMachineState * ThreadId
        =
        let state, thread = stateWithNullary loggerFactory op val1
        IlMachineState.pushToEvalStack' val2 thread state, thread

    /// NUnit `TestCase` arguments have to be constants and a `NullaryIlOp` is not one, so the
    /// division cases name their opcode and resolve it here.
    let private faultingDivisionOp (name : string) : NullaryIlOp =
        match name with
        | "Div" -> NullaryIlOp.Div
        | "Div_un" -> NullaryIlOp.Div_un
        | "Rem" -> NullaryIlOp.Rem
        | "Rem_un" -> NullaryIlOp.Rem_un
        | other -> failwith $"test bug: %s{other} is not one of the four IL divisions"

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
        | NegCase.Int32Value value ->
            EvalStackValue.Int32 (Int32Source.Verbatim value),
            EvalStackValue.Int32 (Int32Source.Verbatim (expectedNegInt32 value))
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
                NullaryIlOp.divUnValues
                    (EvalStackValue.Int32 (Int32Source.Verbatim case.Numerator))
                    (EvalStackValue.Int32 (Int32Source.Verbatim case.Denominator))
            with
            | EvalStackValue.Int32 (Int32Source.Verbatim actual) -> actual |> shouldEqual expected
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

    /// `div`, `div.un`, `rem` and `rem.un` are the only arithmetic instructions that fault on
    /// their operands, and a guest can catch what they raise. These four cases pin the mapping
    /// from the fault to the exception type PawPrint manufactures — the end-to-end guest can only
    /// see that it caught *a* `DivideByZeroException`, whereas this sees which type PawPrint chose
    /// to construct, and so it discriminates `rem`'s two faults from each other.
    [<TestCase("Div", 7, 0, "DivideByZeroException")>]
    [<TestCase("Div", -2147483648, -1, "OverflowException")>]
    [<TestCase("Div_un", 7, 0, "DivideByZeroException")>]
    [<TestCase("Rem", 7, 0, "DivideByZeroException")>]
    [<TestCase("Rem", -2147483648, -1, "OverflowException")>]
    [<TestCase("Rem_un", 7, 0, "DivideByZeroException")>]
    let ``a faulting division raises the exception the CLR would``
        (opName : string)
        (numerator : int32)
        (denominator : int32)
        (expectedExceptionType : string)
        : unit
        =
        let op = faultingDivisionOp opName
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread =
            stateWithBinary
                loggerFactory
                op
                (EvalStackValue.Int32 (Int32Source.Verbatim numerator))
                (EvalStackValue.Int32 (Int32Source.Verbatim denominator))

        let faultingFrame = state.ThreadState.[thread].ActiveMethodState

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let threadState = state.ThreadState.[thread]

            // A ctor frame for the exception must have been pushed on top of the faulting one.
            threadState.ActiveMethodState |> shouldNotEqual faultingFrame

            let ctor = threadState.MethodState.ExecutingMethod
            ctor.Name |> shouldEqual ".ctor"

            let declaringType =
                ctor.TryDeclaringType
                |> Option.defaultWith (fun () -> failwith $"%s{opName} raised a ctor with no declaring type")

            declaringType.Name |> shouldEqual expectedExceptionType
            declaringType.Namespace |> shouldEqual "System"

            // Exception dispatch keys handler lookup and the stack-trace frame on the *faulting*
            // instruction's offset, so the frame that faulted must not have moved on. Nothing a
            // guest written in C# can observe — Roslyn never emits a division as the last
            // instruction of a protected region — but the contract `raiseRuntimeException`
            // documents, and the reason these opcodes cannot share the success path's
            // `advanceProgramCounter`.
            let faulted =
                threadState.MethodStates
                |> Map.tryFind faultingFrame
                |> Option.defaultWith (fun () -> failwith $"%s{opName} discarded the faulting frame")

            faulted.IlOpIndex |> shouldEqual 0
        | other -> failwith $"Expected %s{opName} to step, got %O{other}"

    /// The control for the tests above: these operand pairs do not fault, so the same opcodes must
    /// take the ordinary path — result pushed, program counter advanced, no frame pushed. Without
    /// it, a change that raised on *every* division would still satisfy everything above.
    [<TestCase("Div", 7, 2, 3)>]
    [<TestCase("Div_un", -1, 2, 2147483647)>]
    [<TestCase("Rem", 7, 2, 1)>]
    [<TestCase("Rem_un", -1, 10, 5)>]
    let ``a division that does not fault takes the ordinary path``
        (opName : string)
        (numerator : int32)
        (denominator : int32)
        (expected : int32)
        : unit
        =
        let op = faultingDivisionOp opName
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread =
            stateWithBinary
                loggerFactory
                op
                (EvalStackValue.Int32 (Int32Source.Verbatim numerator))
                (EvalStackValue.Int32 (Int32Source.Verbatim denominator))

        let executingFrame = state.ThreadState.[thread].ActiveMethodState

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let threadState = state.ThreadState.[thread]
            threadState.ActiveMethodState |> shouldEqual executingFrame

            match threadState.MethodState.EvaluationStack.Values with
            | [ EvalStackValue.Int32 (Int32Source.Verbatim actual) ] -> actual |> shouldEqual expected
            | other -> failwith $"Expected %s{opName} to leave one int32 on the stack, got %O{other}"

            threadState.MethodState.IlOpIndex
            |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary op))
        | other -> failwith $"Expected %s{opName} to step, got %O{other}"

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

        let input = EvalStackValue.Int32 (Int32Source.Verbatim 42)
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

    /// The pointer shape used for the `WidenedNativeInt` round-trip. It is
    /// one the *bare* native-int path refuses: `conv.i8` of a method handle followed by
    /// `conv.ovf.i` must invert to the original handle (the widening is bit-preserving
    /// on 64 bits), whereas a method handle arriving directly at `conv.ovf.i` has no
    /// observed call site and so fails loudly. `Conv_ovf_u` draws the line in the same
    /// place.
    let private widenedPointerSource : NativeIntSource =
        NativeIntSource.MethodHandlePtr 7L

    let private convOvfICaseInput (case : ConvOvfICase) : EvalStackValue =
        match case with
        | ConvOvfICase.Int32Value value -> EvalStackValue.Int32 (Int32Source.Verbatim value)
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

            let declaring = ctor.RequiredDeclaringType
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

    /// A `conv.ovf.i.un` source operand. The shapes are those of `ConvOvfICase`, plus
    /// the `NativeIntPlaceholder` byrefs — a placeholder is an exact bit pattern the
    /// guest supplied, so unlike a real byref it has a sign bit and can overflow.
    [<RequireQualifiedAccess>]
    type private ConvOvfIUnCase =
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
        | ManagedPointerPlaceholder of bits : int64
        | NativeIntPlaceholder of bits : int64
        | NullObjectRef

    /// The host runs `conv.ovf.i.un` for us. Unlike `conv.ovf.i` there is no F#
    /// conversion function that emits it from every source shape we care about
    /// (`Checked.nativeint` emits it only for an unsigned argument — never from an
    /// int32 or a float), so the oracle is the opcode itself, emitted into a
    /// `DynamicMethod` with the source's own stack type as the parameter. The host is
    /// the same 64-bit width PawPrint models, so this is a true oracle rather than a
    /// restatement of the code under test — in particular it, not this file, is what
    /// says an int32 source is zero-extended and that a float source ignores `.un`.
    let private hostConvOvfIUn<'source> () : 'source -> Result<int64, unit> =
        let dm =
            DynamicMethod ($"convOvfIUn_%s{typeof<'source>.Name}", typeof<nativeint>, [| typeof<'source> |])

        let il = dm.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Conv_Ovf_I_Un
        il.Emit OpCodes.Ret

        let compiled =
            dm.CreateDelegate typeof<Func<'source, nativeint>> :?> Func<'source, nativeint>

        fun value ->
            try
                compiled.Invoke value |> int64 |> Ok
            with :? OverflowException ->
                Error ()

    let private hostConvOvfIUnFromInt32 : int32 -> Result<int64, unit> =
        hostConvOvfIUn<int32> ()

    let private hostConvOvfIUnFromInt64 : int64 -> Result<int64, unit> =
        hostConvOvfIUn<int64> ()

    let private hostConvOvfIUnFromNativeInt : nativeint -> Result<int64, unit> =
        hostConvOvfIUn<nativeint> ()

    let private hostConvOvfIUnFromFloat : float -> Result<int64, unit> =
        hostConvOvfIUn<float> ()

    let private convOvfIUnCaseInput (case : ConvOvfIUnCase) : EvalStackValue =
        match case with
        | ConvOvfIUnCase.Int32Value value -> EvalStackValue.Int32 (Int32Source.Verbatim value)
        | ConvOvfIUnCase.Int64Value value -> EvalStackValue.Int64 (Int64Source.Verbatim value)
        | ConvOvfIUnCase.NativeIntVerbatim value -> EvalStackValue.NativeInt (NativeIntSource.Verbatim value)
        | ConvOvfIUnCase.FloatValue value -> EvalStackValue.Float value
        | ConvOvfIUnCase.Int64CrossArrayOffset offset ->
            EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset offset)
        | ConvOvfIUnCase.NativeIntCrossArrayOffset offset ->
            EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset offset)
        | ConvOvfIUnCase.Int64OpaqueHashBits bits -> EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits)
        | ConvOvfIUnCase.NativeIntOpaqueHashBits bits -> EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits)
        | ConvOvfIUnCase.WidenedPointer signed ->
            EvalStackValue.Int64 (Int64Source.widenedNativeInt widenedPointerSource signed)
        | ConvOvfIUnCase.ManagedPointerNull -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
        | ConvOvfIUnCase.NativeIntManagedPointerNull ->
            EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
        | ConvOvfIUnCase.ManagedPointerPlaceholder bits ->
            EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)
        | ConvOvfIUnCase.NativeIntPlaceholder bits ->
            EvalStackValue.NativeInt (NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits))
        | ConvOvfIUnCase.NullObjectRef -> EvalStackValue.NullObjectRef

    let private convOvfIUnExpected (case : ConvOvfIUnCase) : Result<NativeIntSource, unit> =
        let ofVerbatim (r : Result<int64, unit>) : Result<NativeIntSource, unit> =
            r |> Result.map NativeIntSource.Verbatim

        // Tagged shapes whose bits PawPrint *does* know: the host decides whether they
        // overflow, and the tag has to survive a success.
        let keepingTag (tag : NativeIntSource) (bits : int64) : Result<NativeIntSource, unit> =
            hostConvOvfIUnFromInt64 bits |> Result.map (fun _ -> tag)

        match case with
        | ConvOvfIUnCase.Int32Value value -> hostConvOvfIUnFromInt32 value |> ofVerbatim
        | ConvOvfIUnCase.Int64Value value -> hostConvOvfIUnFromInt64 value |> ofVerbatim
        | ConvOvfIUnCase.NativeIntVerbatim value -> hostConvOvfIUnFromNativeInt (nativeint value) |> ofVerbatim
        | ConvOvfIUnCase.FloatValue value -> hostConvOvfIUnFromFloat value |> ofVerbatim
        | ConvOvfIUnCase.Int64CrossArrayOffset offset
        | ConvOvfIUnCase.NativeIntCrossArrayOffset offset -> NativeIntSource.SyntheticCrossArrayOffset offset |> Ok
        | ConvOvfIUnCase.Int64OpaqueHashBits bits
        | ConvOvfIUnCase.NativeIntOpaqueHashBits bits -> keepingTag (NativeIntSource.OpaqueHashBits bits) bits
        | ConvOvfIUnCase.WidenedPointer _ -> Ok widenedPointerSource
        | ConvOvfIUnCase.ManagedPointerNull
        | ConvOvfIUnCase.NativeIntManagedPointerNull
        | ConvOvfIUnCase.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null |> Ok
        | ConvOvfIUnCase.ManagedPointerPlaceholder bits
        | ConvOvfIUnCase.NativeIntPlaceholder bits ->
            keepingTag (NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)) bits

    /// Full-width int64 draws. The default FsCheck int64 generator is size-bounded, so
    /// on its own it would never produce a source whose *top* bit is set — which is the
    /// only way a 64-bit source overflows this opcode.
    let private genWideInt64 : Gen<int64> =
        gen {
            let! hi = Gen.choose (Int32.MinValue, Int32.MaxValue)
            let! lo = Gen.choose (Int32.MinValue, Int32.MaxValue)
            return (int64 hi <<< 32) ||| int64<uint32> (uint32<int32> lo)
        }

    let private genConvOvfIUnCase : Gen<ConvOvfIUnCase> =
        let genInt32 =
            Gen.frequency
                [
                    6, Gen.choose (Int32.MinValue, Int32.MaxValue)
                    2, ArbMap.defaults |> ArbMap.generate<int32>
                    2, Gen.elements [ Int32.MinValue ; Int32.MaxValue ; -1 ; 0 ; 1 ]
                ]

        let genInt64 =
            Gen.frequency
                [
                    6, genWideInt64
                    2, ArbMap.defaults |> ArbMap.generate<int64>
                    2, Gen.elements [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ]
                ]

        Gen.frequency
            [
                3, genInt32 |> Gen.map ConvOvfIUnCase.Int32Value
                // Weighted heavily: an int64 source is the shape that distinguishes this
                // opcode from `conv.ovf.i`, half of whose range overflows here.
                6, genInt64 |> Gen.map ConvOvfIUnCase.Int64Value
                6, genInt64 |> Gen.map ConvOvfIUnCase.NativeIntVerbatim
                6, genConvOvfIFloat |> Gen.map ConvOvfIUnCase.FloatValue
                1, genSyntheticCrossArrayOffset |> Gen.map ConvOvfIUnCase.Int64CrossArrayOffset
                1, genSyntheticCrossArrayOffset |> Gen.map ConvOvfIUnCase.NativeIntCrossArrayOffset
                1, genInt64 |> Gen.map ConvOvfIUnCase.Int64OpaqueHashBits
                1, genInt64 |> Gen.map ConvOvfIUnCase.NativeIntOpaqueHashBits
                1, genInt64 |> Gen.map ConvOvfIUnCase.ManagedPointerPlaceholder
                1, genInt64 |> Gen.map ConvOvfIUnCase.NativeIntPlaceholder
                1,
                ArbMap.defaults
                |> ArbMap.generate<bool>
                |> Gen.map ConvOvfIUnCase.WidenedPointer
                1, Gen.constant ConvOvfIUnCase.ManagedPointerNull
                1, Gen.constant ConvOvfIUnCase.NativeIntManagedPointerNull
                1, Gen.constant ConvOvfIUnCase.NullObjectRef
            ]

    let private convOvfIUnEdgeCases : ConvOvfIUnCase list =
        [
            for value in [ Int32.MinValue ; Int32.MaxValue ; -1 ; 0 ; 1 ] do
                ConvOvfIUnCase.Int32Value value
            for value in [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ] do
                ConvOvfIUnCase.Int64Value value
                ConvOvfIUnCase.NativeIntVerbatim value
                ConvOvfIUnCase.Int64OpaqueHashBits value
                ConvOvfIUnCase.NativeIntOpaqueHashBits value
                ConvOvfIUnCase.ManagedPointerPlaceholder value
                ConvOvfIUnCase.NativeIntPlaceholder value
            for value in convOvfIFloatEdges do
                ConvOvfIUnCase.FloatValue value
            ConvOvfIUnCase.WidenedPointer true
            ConvOvfIUnCase.WidenedPointer false
            ConvOvfIUnCase.ManagedPointerNull
            ConvOvfIUnCase.NativeIntManagedPointerNull
            ConvOvfIUnCase.NullObjectRef
            ConvOvfIUnCase.Int64CrossArrayOffset (
                SyntheticCrossArrayOffset.make syntheticStorageIdentities.[0] 0L syntheticStorageIdentities.[1] 0L
            )
            ConvOvfIUnCase.NativeIntCrossArrayOffset (
                SyntheticCrossArrayOffset.make syntheticStorageIdentities.[2] -1L syntheticStorageIdentities.[3] 1L
            )
        ]

    [<Test>]
    let ``Conv_ovf_i_un agrees with the host's unsigned checked conversion and preserves provenance`` () : unit =
        let mutable overflows = 0
        let mutable successes = 0

        let property (case : ConvOvfIUnCase) : unit =
            match convOvfIUnExpected case with
            | Ok _ -> successes <- successes + 1
            | Error () -> overflows <- overflows + 1

            NullaryIlOp.convOvfIUn (convOvfIUnCaseInput case)
            |> shouldEqual (convOvfIUnExpected case)

        for case in convOvfIUnEdgeCases do
            property case

        Check.One (config, Prop.forAll (Arb.fromGen genConvOvfIUnCase) property)

        // Guard against a generator that silently stops exercising one side: roughly half
        // of the 64-bit draws should overflow, so a generator that had drifted to
        // non-negative sources only would turn this into a success-path-only test.
        if overflows < 30 || successes < 30 then
            failwith $"Conv_ovf_i_un generator was unbalanced: %d{overflows} overflows, %d{successes} successes"

    /// The two opcodes must disagree on exactly the sources whose top bit is set, and in
    /// opposite directions: an int32 `-1` is 4294967295 unsigned (both in range, but not
    /// equal), and an int64 `-1` is in range for one and overflows for the other. This
    /// would catch a `Conv_ovf_i_un` implemented by delegating to `convOvfI`.
    [<Test>]
    let ``Conv_ovf_i_un differs from Conv_ovf_i on sources with the top bit set`` () : unit =
        let int32MinusOne = EvalStackValue.Int32 (Int32Source.Verbatim -1)

        NullaryIlOp.convOvfI int32MinusOne
        |> shouldEqual (Ok (NativeIntSource.Verbatim -1L))

        NullaryIlOp.convOvfIUn int32MinusOne
        |> shouldEqual (Ok (NativeIntSource.Verbatim 4294967295L))

        let int64MinusOne = EvalStackValue.Int64 (Int64Source.Verbatim -1L)

        NullaryIlOp.convOvfI int64MinusOne
        |> shouldEqual (Ok (NativeIntSource.Verbatim -1L))

        NullaryIlOp.convOvfIUn int64MinusOne |> shouldEqual (Error ())

    [<Test>]
    let ``Conv_ovf_i_un pushes a native int and advances past its own encoding`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        // An int32 slot holding 0xFFFF_FFFF: zero-extended, not sign-extended.
        let input = EvalStackValue.Int32 (Int32Source.Verbatim -1)
        let state, thread = stateWithNullary loggerFactory NullaryIlOp.Conv_ovf_i_un input

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Conv_ovf_i_un with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let methodState = state.ThreadState.[thread].MethodState

            methodState.EvaluationStack.Values
            |> shouldEqual [ EvalStackValue.NativeInt (NativeIntSource.Verbatim 4294967295L) ]

            methodState.IlOpIndex
            |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary NullaryIlOp.Conv_ovf_i_un))
        | other -> failwith $"Expected Conv_ovf_i_un to step, got %O{other}"

    [<Test>]
    let ``Conv_ovf_i_un raises OverflowException without advancing the faulting PC`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        // Read as unsigned, 0xFFFF_FFFF_FFFF_FFFF is far above Int64.MaxValue.
        let input = EvalStackValue.Int64 (Int64Source.Verbatim -1L)
        let state, thread = stateWithNullary loggerFactory NullaryIlOp.Conv_ovf_i_un input

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Conv_ovf_i_un with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let threadState = state.ThreadState.[thread]

            // The runtime has pushed a frame running `OverflowException..ctor`.
            let ctor = threadState.MethodState.ExecutingMethod
            ctor.Name |> shouldEqual ".ctor"

            let declaring = ctor.RequiredDeclaringType
            declaring.Namespace |> shouldEqual "System"
            declaring.Name |> shouldEqual "OverflowException"

            // Exception dispatch needs the faulting instruction's offset, so the frame
            // that executed `conv.ovf.i.un` must still be sitting on it. It is no longer
            // the active frame, so find it by frame id.
            let faultingFrame =
                threadState.MethodStates
                |> Map.toSeq
                |> Seq.filter (fun (frameId, _) -> frameId <> threadState.ActiveMethodState)
                |> Seq.exactlyOne
                |> snd

            faultingFrame.IlOpIndex |> shouldEqual 0
            faultingFrame.EvaluationStack.Values |> shouldEqual []
        | other -> failwith $"Expected Conv_ovf_i_un overflow to step, got %O{other}"

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
        runBinary NullaryIlOp.And (taggedHandle 3L) (EvalStackValue.Int32 (Int32Source.Verbatim -2))
        |> shouldEqual (taggedHandle 2L)

        runBinary NullaryIlOp.And (EvalStackValue.Int32 (Int32Source.Verbatim -2)) (taggedHandle 3L)
        |> shouldEqual (taggedHandle 2L)

        runBinary NullaryIlOp.Or (EvalStackValue.Int32 (Int32Source.Verbatim 1)) (taggedHandle 0L)
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
        shouldRefuse NullaryIlOp.And (EvalStackValue.Int32 (Int32Source.Verbatim 4))
        shouldRefuse NullaryIlOp.Or (nativeConst 8L)
        shouldRefuse NullaryIlOp.Or (EvalStackValue.Int32 (Int32Source.Verbatim 8))
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

    // --- Bitwise operations on tagged type handles ---
    //
    // CoreCLR's `TypeHandle` is a tagged pointer: bit 1 is set exactly when the
    // handle wraps a `TypeDesc` rather than a `MethodTable`. The managed
    // `TypeHandle` struct in src/coreclr/System.Private.CoreLib reads that tag
    // (`IsTypeDesc` is `(nint)m_asTAddr & 2`) and strips it
    // (`AsTypeDesc` is `(nint)m_asTAddr & ~2`). PawPrint models no address for
    // either pointer kind, so the same decision procedure that answers GC-handle
    // masks answers these; see docs/plans/2026-08-06-typehandle-tag-bits.md.

    /// A MethodTable-shaped target: `IsTypeDesc` is false, so its tag is 0.
    let private methodTableTarget : RuntimeTypeHandleTarget =
        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 1)

    /// A TypeDesc-shaped target (`int*`): `IsTypeDesc` is true, so its tag is 2.
    let private typeDescTarget : RuntimeTypeHandleTarget =
        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 1))

    let private typeHandle (target : RuntimeTypeHandleTarget) : EvalStackValue =
        EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr target)

    let private methodTable (target : RuntimeTypeHandleTarget) : EvalStackValue =
        EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr target)

    [<Test>]
    let ``And reads a type handle's TypeDesc tag bit`` () : unit =
        // `TypeHandle.IsTypeDesc`: `((nint)m_asTAddr & 2) != 0`. The mask forces
        // the whole unmodelled base to zero, so the tag alone is the answer.
        runBinary NullaryIlOp.And (typeHandle typeDescTarget) (nativeConst 2L)
        |> shouldEqual (nativeConst 2L)

        runBinary NullaryIlOp.And (typeHandle methodTableTarget) (nativeConst 2L)
        |> shouldEqual (nativeConst 0L)

        // A MethodTable pointer is untagged but aligned, so masking to the tag
        // region is honestly zero.
        runBinary NullaryIlOp.And (methodTable methodTableTarget) (nativeConst 3L)
        |> shouldEqual (nativeConst 0L)

    [<Test>]
    let ``And with a base-preserving mask leaves a type handle intact`` () : unit =
        // The whole unknown base survives and the tag is unchanged, so the value
        // is bit-identical to the input. Answering `0` here would silently
        // replace a pointer with null.
        runBinary NullaryIlOp.And (typeHandle methodTableTarget) (nativeConst ~~~2L)
        |> shouldEqual (typeHandle methodTableTarget)

        runBinary NullaryIlOp.And (typeHandle methodTableTarget) (nativeConst -1L)
        |> shouldEqual (typeHandle methodTableTarget)

        runBinary NullaryIlOp.And (methodTable methodTableTarget) (nativeConst -1L)
        |> shouldEqual (methodTable methodTableTarget)

        runBinary NullaryIlOp.And (methodTable methodTableTarget) (nativeConst ~~~3L)
        |> shouldEqual (methodTable methodTableTarget)

    let private typeDesc (target : RuntimeTypeHandleTarget) : EvalStackValue =
        EvalStackValue.NativeInt (NativeIntSource.TypeDescPtr target)

    [<Test>]
    let ``And strips a type handle's tag to its TypeDesc pointer`` () : unit =
        // `TypeHandle.AsTypeDesc`: `(TypeDesc*)((nint)m_asTAddr & ~2)`. The base
        // survives but the tag clears, so the result is a different identity —
        // the target's TypeDesc pointer. Reachable from the public
        // `RuntimeTypeHandle.FromIntPtr`.
        runBinary NullaryIlOp.And (typeHandle typeDescTarget) (nativeConst ~~~2L)
        |> shouldEqual (typeDesc typeDescTarget)

        // A wider mask over the same region strips it just the same.
        runBinary NullaryIlOp.And (typeHandle typeDescTarget) (nativeConst ~~~3L)
        |> shouldEqual (typeDesc typeDescTarget)

    [<Test>]
    let ``a TypeDesc pointer is itself untagged`` () : unit =
        // Having been stripped, it carries no tag: base-preserving masks are the
        // identity and the tag region reads as zero. In particular `AsTypeDesc`
        // is idempotent, and `IsTypeDesc` of the result is false, as in CoreCLR.
        runBinary NullaryIlOp.And (typeDesc typeDescTarget) (nativeConst ~~~2L)
        |> shouldEqual (typeDesc typeDescTarget)

        runBinary NullaryIlOp.And (typeDesc typeDescTarget) (nativeConst -1L)
        |> shouldEqual (typeDesc typeDescTarget)

        runBinary NullaryIlOp.And (typeDesc typeDescTarget) (nativeConst 2L)
        |> shouldEqual (nativeConst 0L)

    [<Test>]
    let ``a TypeDesc pointer is not the type handle it came from`` () : unit =
        // In CoreCLR the two differ numerically by exactly the tag bit, so they
        // must not compare equal — otherwise `AreSameType` would conflate a
        // handle with the TypeDesc inside it.
        NativeIntSourceComparison.equalsForCli
            PointerHashState.empty
            (NativeIntSource.TypeDescPtr typeDescTarget)
            (NativeIntSource.TypeHandlePtr typeDescTarget)
        |> shouldEqual false

        NativeIntSourceComparison.equalsForCli
            PointerHashState.empty
            (NativeIntSource.TypeDescPtr typeDescTarget)
            (NativeIntSource.TypeDescPtr typeDescTarget)
        |> shouldEqual true

    [<Test>]
    let ``And on a type handle with a mask spanning the address is refused loudly`` () : unit =
        // These ask about bits of the pointer's address, which PawPrint does not
        // model. Answering would mean inventing them.
        let shouldRefuse (value : EvalStackValue) (operand : EvalStackValue) : unit =
            let exn =
                Assert.Throws (fun () -> runBinary NullaryIlOp.And value operand |> ignore<EvalStackValue>)

            exn.Message |> shouldContainText "which PawPrint does not model"

        shouldRefuse (typeHandle typeDescTarget) (nativeConst 4L)
        shouldRefuse (typeHandle methodTableTarget) (nativeConst 4L)
        shouldRefuse (typeHandle methodTableTarget) (EvalStackValue.Int32 (Int32Source.Verbatim 4))
        shouldRefuse (methodTable methodTableTarget) (nativeConst 4L)
        shouldRefuse (methodTable methodTableTarget) (nativeConst ~~~4L)

    // --- Narrowing a byref with conv.i4 / conv.u4 ---
    //
    // `SpanHelpers.IndexOfNullCharacter` opens with `((int)searchSpace & 1) != 0`,
    // so a byref has to survive `conv.i4` for the mask to be answerable. See
    // `Int32Source.NarrowedManagedPointer` for why the int32 stack slot carries
    // provenance at all, and `PointerAlignmentMask.cs` for the masks that are
    // answerable. These pin the *boundaries* of that answer, which an end-to-end
    // test cannot reach because each one throws.

    /// A byref into a native-heap block. `NativeMemoryByte` is the one root whose
    /// offset comes from `tryStableAddressBits` alone, so these tests need no heap.
    let private nativeBlockByref (byteOffset : int) : ManagedPointerSource =
        ManagedPointerSource.Byref (
            ByrefRoot.NativeMemoryByte (NativeMemoryBlockId.NativeMemoryBlockId 0, byteOffset),
            []
        )

    let private narrowed (byteOffset : int) : EvalStackValue =
        EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer (nativeBlockByref byteOffset))

    let private runUnary (op : NullaryIlOp) (value : EvalStackValue) : EvalStackValue =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithNullary loggerFactory op value

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | [ actual ] -> actual
            | other -> failwith $"Expected %O{op} to leave one stack value, got %O{other}"
        | other -> failwith $"Expected %O{op} to step, got %O{other}"

    [<Test>]
    let ``Conv_I4 keeps a byref alive instead of refusing`` () : unit =
        runUnary NullaryIlOp.Conv_I4 (EvalStackValue.ManagedPointer (nativeBlockByref 6))
        |> shouldEqual (narrowed 6)

        // Reaching `conv.i4` through the native-int slot (as it does after a
        // `conv.u`) must produce the same thing.
        runUnary NullaryIlOp.Conv_I4 (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer (nativeBlockByref 6)))
        |> shouldEqual (narrowed 6)

        runUnary NullaryIlOp.Conv_U4 (EvalStackValue.ManagedPointer (nativeBlockByref 6))
        |> shouldEqual (narrowed 6)

    [<Test>]
    let ``a byref with an exactly-known bit pattern narrows to an ordinary int32`` () : unit =
        // `Null` and the `Unsafe.AsRef<T>((void*)bits)` placeholder are values, not
        // unknown addresses. They must not become NarrowedManagedPointer, and they
        // must land on the int32 stack kind `conv.i4` is specified to push — a
        // native int there would break the int32 comparisons and stores that
        // legitimately follow.
        let placeholder (bits : int64) : EvalStackValue =
            EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)

        for op in [ NullaryIlOp.Conv_I4 ; NullaryIlOp.Conv_U4 ] do
            runUnary op (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
            |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

            runUnary op (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
            |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

            // The placeholder's high word is discarded.
            runUnary op (placeholder 0x1_0000_0004L)
            |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 4))

            // Bit 31 set: both conversions keep the same 32 bits, and neither may
            // sign-extend them back up into a native int.
            runUnary op (placeholder 0x8000_0000L)
            |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim Int32.MinValue))

    [<Test>]
    let ``a mask inside the container's alignment yields an int32`` () : unit =
        // `malloc` storage is at least 8-byte aligned, so the low three bits of
        // `block + 6` are exactly 6. The result is an int32 because the guest is
        // about to compare it against an `int` literal.
        runBinary NullaryIlOp.And (narrowed 6) (EvalStackValue.Int32 (Int32Source.Verbatim 1))
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        runBinary NullaryIlOp.And (narrowed 6) (EvalStackValue.Int32 (Int32Source.Verbatim 7))
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 6))

        runBinary NullaryIlOp.And (EvalStackValue.Int32 (Int32Source.Verbatim 3)) (narrowed 6)
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 2))

    [<Test>]
    let ``an all-ones mask leaves the narrowed byref untouched`` () : unit =
        runBinary NullaryIlOp.And (narrowed 6) (EvalStackValue.Int32 (Int32Source.Verbatim -1))
        |> shouldEqual (narrowed 6)

    /// The in-block offset is folded in `int64`, so a chain of byte cursors totalling more than
    /// `int32` produces the address it names rather than aborting on a limit the address model
    /// does not have. A synthetic address is 64 bits wide; the `int` that each `ByteOffset` step
    /// is stored in is a limit on the *step*, not on the total (issue #993).
    ///
    /// The chain is built directly because `appendProjection` coalesces adjacent `ByteOffset`s
    /// and refuses a total this size, since a single `ByteOffset` cannot hold it.
    [<Test>]
    let ``a stable address folds several cursors without a 32-bit limit`` () : unit =
        let byteConcreteType : ConcreteType<ConcreteTypeHandle> =
            ConcreteType.makeFromIdentity
                baseClassTypes.Byte.Identity
                baseClassTypes.Byte.Namespace
                baseClassTypes.Byte.Name
                System.Collections.Immutable.ImmutableArray<ConcreteTypeHandle>.Empty

        let cursors =
            [ Int32.MaxValue ; Int32.MaxValue ; 2 ]
            |> List.collect (fun n ->
                [
                    ByrefProjection.ReinterpretAs byteConcreteType
                    ByrefProjection.ByteOffset n
                ]
            )

        ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (NativeMemoryBlockId.NativeMemoryBlockId 0, 6), cursors)
        |> ManagedPointerSource.tryStableAddressBits
        |> shouldEqual (Some 4294967302L)

    [<Test>]
    let ``a mask reaching past the guaranteed alignment is refused`` () : unit =
        // `malloc` promises 8-byte alignment and no more, so bit 3 is a question
        // about the block's address.
        let exn =
            Assert.Throws (fun () ->
                runBinary NullaryIlOp.And (narrowed 6) (EvalStackValue.Int32 (Int32Source.Verbatim 15))
                |> ignore<EvalStackValue>
            )

        exn.Message |> shouldContainText "guaranteed 3-bit alignment"

    [<Test>]
    let ``aligning a narrowed byref down is refused rather than approximated`` () : unit =
        // `p & ~7` keeps every address bit but clears the offset's low bits, so the
        // answer is a different location in the same container. PawPrint has no way
        // to express that yet, and must not silently return the original pointer.
        let exn =
            Assert.Throws (fun () ->
                runBinary NullaryIlOp.And (narrowed 6) (EvalStackValue.Int32 (Int32Source.Verbatim -8))
                |> ignore<EvalStackValue>
            )

        exn.Message |> shouldContainText "the same container at a lower offset"

    [<Test>]
    let ``a byref whose container has no alignment claim answers only the address-independent masks`` () : unit =
        // Object fields expose no stable low bits, so the tag region is empty. That
        // still leaves `p & 0` and `p & -1` answerable for *any* address, and a
        // decision procedure that refused them would be refusing something it could
        // have answered.
        let fieldByref =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 1), [])

        let narrowedField =
            EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer fieldByref)

        runBinary NullaryIlOp.And narrowedField (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        runBinary NullaryIlOp.And narrowedField (EvalStackValue.Int32 (Int32Source.Verbatim -1))
        |> shouldEqual narrowedField

        // Every other mask is a question about the address.
        for mask in [ 1 ; 3 ; 7 ; -2 ] do
            let exn =
                Assert.Throws (fun () ->
                    runBinary NullaryIlOp.And narrowedField (EvalStackValue.Int32 (Int32Source.Verbatim mask))
                    |> ignore<EvalStackValue>
                )

            exn.Message |> shouldContainText "claims no alignment"

    let private peByteRangeByref (source : PeByteRangePointerSource) (rva : int) : ManagedPointerSource =
        ManagedPointerSource.Byref (
            ByrefRoot.PeByteRange
                {
                    AssemblyFullName = "TestAssembly"
                    Source = source
                    RelativeVirtualAddress = rva
                    Size = 16
                },
            []
        )

    [<Test>]
    let ``a method signature blob makes no alignment claim`` () : unit =
        // Same reasoning as the field variant below: a method's COR signature blob lives in the
        // metadata `#Blob` heap at an offset PawPrint does not track, so its RelativeVirtualAddress
        // is a placeholder 0 and its low bits belong to no address.
        let method =
            ComparableMethodDefinitionHandle.Make (
                System.Reflection.Metadata.Ecma335.MetadataTokens.MethodDefinitionHandle 1
            )

        let blob = peByteRangeByref (PeByteRangePointerSource.MethodSignatureBlob method) 0

        ManagedPointerSource.tryContainerAlignmentBits blob |> shouldEqual None

        let exn =
            Assert.Throws (fun () ->
                runBinary
                    NullaryIlOp.And
                    (EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer blob))
                    (EvalStackValue.Int32 (Int32Source.Verbatim 1))
                |> ignore<EvalStackValue>
            )

        exn.Message |> shouldContainText "claims no alignment"

    [<Test>]
    let ``a property signature blob makes no alignment claim`` () : unit =
        // Same reasoning as the method and field variants: an ECMA II.23.2.5 PropertySig lives in
        // the metadata `#Blob` heap at an offset PawPrint does not track, so its
        // RelativeVirtualAddress is a placeholder 0 and its low bits belong to no address.
        let property =
            ComparablePropertyDefinitionHandle.Make (
                System.Reflection.Metadata.Ecma335.MetadataTokens.PropertyDefinitionHandle 1
            )

        let blob =
            peByteRangeByref (PeByteRangePointerSource.PropertySignatureBlob property) 0

        ManagedPointerSource.tryContainerAlignmentBits blob |> shouldEqual None

        let exn =
            Assert.Throws (fun () ->
                runBinary
                    NullaryIlOp.And
                    (EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer blob))
                    (EvalStackValue.Int32 (Int32Source.Verbatim 1))
                |> ignore<EvalStackValue>
            )

        exn.Message |> shouldContainText "claims no alignment"

    [<Test>]
    let ``a field signature blob makes no alignment claim`` () : unit =
        // `FieldRva` and `ManagedResource` name real section offsets, and the image
        // base is page-aligned, so their low bits are the mapped address's low bits.
        let field =
            ComparableFieldDefinitionHandle.Make (
                System.Reflection.Metadata.Ecma335.MetadataTokens.FieldDefinitionHandle 1
            )

        ManagedPointerSource.tryContainerAlignmentBits (peByteRangeByref (PeByteRangePointerSource.FieldRva field) 6)
        |> shouldEqual (Some 3)

        ManagedPointerSource.tryContainerAlignmentBits (
            peByteRangeByref (PeByteRangePointerSource.ManagedResource "r") 6
        )
        |> shouldEqual (Some 3)

        // A signature blob lives in the metadata `#Blob` heap, and fixes its RVA at
        // 0 as a placeholder. Claiming alignment there would turn the byte cursor
        // into fabricated address bits.
        let blob = peByteRangeByref (PeByteRangePointerSource.FieldSignatureBlob field) 0

        ManagedPointerSource.tryContainerAlignmentBits blob |> shouldEqual None

        let exn =
            Assert.Throws (fun () ->
                runBinary
                    NullaryIlOp.And
                    (EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer blob))
                    (EvalStackValue.Int32 (Int32Source.Verbatim 1))
                |> ignore<EvalStackValue>
            )

        exn.Message |> shouldContainText "claims no alignment"

    [<Test>]
    let ``no int32 consumer silently takes a narrowed byref as a number`` () : unit =
        // The reason `Int32Source` exists rather than a narrowed pointer living in
        // the wider native-int slot: an int32 consumer must not be able to get at a
        // number without saying what to do when there isn't one. Sweep the ordinary
        // int32 opcodes and require each to refuse by name — a silent answer here
        // would be a fabricated address bit escaping into arithmetic.
        let unary =
            [
                NullaryIlOp.Neg
                NullaryIlOp.Not
                NullaryIlOp.Conv_I1
                NullaryIlOp.Conv_I2
                NullaryIlOp.Conv_I8
                NullaryIlOp.Conv_U1
                NullaryIlOp.Conv_U2
                NullaryIlOp.Conv_U8
                NullaryIlOp.Conv_R4
                NullaryIlOp.Conv_R8
                NullaryIlOp.Conv_ovf_i
                NullaryIlOp.Conv_ovf_u
            ]

        for op in unary do
            let result =
                try
                    Ok (runUnary op (narrowed 6))
                with e ->
                    Error e.Message

            match result with
            | Ok value -> failwith $"%O{op} accepted a truncated byref as a number: got %O{value}"
            | Error message -> message |> shouldContainText "truncated to 32 bits"

        // Binary arithmetic and ordering likewise.
        let binary =
            [
                NullaryIlOp.Add
                NullaryIlOp.Sub
                NullaryIlOp.Mul
                NullaryIlOp.Or
                NullaryIlOp.Xor
                NullaryIlOp.Clt
                NullaryIlOp.Cgt
            ]

        for op in binary do
            let result =
                try
                    Ok (runBinary op (narrowed 6) (EvalStackValue.Int32 (Int32Source.Verbatim 1)))
                with e ->
                    Error e.Message

            match result with
            | Ok value -> failwith $"%O{op} accepted a truncated byref as a number: got %O{value}"
            | Error message -> message |> shouldContainText "truncated to 32 bits"

    [<Test>]
    let ``widening a narrowed byref back to pointer width is refused`` () : unit =
        // The discarded high bits are gone; recovering the byref here would hand the
        // guest a pointer that the real truncation would have destroyed.
        for op in
            [
                NullaryIlOp.Conv_U
                NullaryIlOp.Conv_I
                NullaryIlOp.Conv_I8
                NullaryIlOp.Conv_U8
            ] do
            let result =
                try
                    Ok (runUnary op (narrowed 6))
                with e ->
                    Error e.Message

            match result with
            | Ok value -> failwith $"%O{op} widened a truncated byref instead of refusing: got %O{value}"
            | Error message -> message |> shouldContainText "truncat"

    /// The six unchecked narrowing conversions, paired with the destination width
    /// their diagnostics name. `TestEvalStack` pins what each *computes* from a
    /// pointer-shaped source; these cases pin that the op arm writes the resulting
    /// `PointerHashState` back into the machine state.
    ///
    /// Without that write-back each conversion would restart counter assignment from
    /// zero, so two distinct handles could be handed identical bits — which
    /// `docs/plans/2026-05-14-pointer-hash-counter-strategy.md` calls out as the one
    /// failure mode of synthesis that nothing downstream can detect, because it makes
    /// distinct pointers compare equal.
    let private narrowingOps : NullaryIlOp list =
        [
            NullaryIlOp.Conv_I1
            NullaryIlOp.Conv_I2
            NullaryIlOp.Conv_I4
            NullaryIlOp.Conv_U1
            NullaryIlOp.Conv_U2
            NullaryIlOp.Conv_U4
        ]

    [<TestCaseSource(nameof narrowingOps)>]
    let ``narrowing a pointer registers its identity in the machine state`` (op : NullaryIlOp) : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let handle = NativeIntSource.MethodHandlePtr 17L

        let state, thread =
            stateWithNullary loggerFactory op (EvalStackValue.NativeInt handle)

        state.PointerHashState |> shouldEqual PointerHashState.empty

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            // One identity assigned, and retained: a discarded counter map would leave
            // this at zero while still producing a plausible-looking number.
            PointerHashTestHelpers.nextCounter state.PointerHashState |> shouldEqual 1UL
            PointerHashTestHelpers.assignedCount state.PointerHashState |> shouldEqual 1
        | other -> failwith $"Expected %O{op} to step, got %O{other}"

    [<Test>]
    let ``narrowing reads the counters already in the machine state`` () : unit =
        // The other half of the write-back: the op arm must *read* the state's counter
        // map, not a fresh one. Given a state that has already assigned bits to one
        // handle, narrowing a second handle has to produce a different int32 — which is
        // the guest-observable fact a hashtable keyed on `IntPtr.GetHashCode` depends on.
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let narrow (counters : PointerHashState) (handle : NativeIntSource) : EvalStackValue * PointerHashState =
            let state, thread =
                stateWithNullary loggerFactory NullaryIlOp.Conv_I4 (EvalStackValue.NativeInt handle)

            let state =
                { state with
                    PointerHashState = counters
                }

            match NullaryIlOp.execute loggerFactory baseClassTypes state thread NullaryIlOp.Conv_I4 with
            | ExecutionResult.Stepped (state, _, _) ->
                IlMachineState.popEvalStack thread state |> fst, state.PointerHashState
            | other -> failwith $"Expected Conv_I4 to step, got %O{other}"

        let first, counters =
            narrow PointerHashState.empty (NativeIntSource.MethodHandlePtr 17L)

        let second, counters = narrow counters (NativeIntSource.MethodHandlePtr 18L)

        if first = second then
            failwith $"Conv_I4 gave both method handles the same int32 %O{first}"

        PointerHashTestHelpers.assignedCount counters |> shouldEqual 2

        // And the first handle still narrows to what it did before: assignment is
        // memoised, not re-derived.
        let again, _ = narrow counters (NativeIntSource.MethodHandlePtr 17L)
        again |> shouldEqual first

    // --- Byte views of a primitive cell at byte offset zero ---
    //
    // `*(byte*)&aLong` reads one byte at the address of an eight-byte cell. A *non-zero*
    // index (`p[1]`) arrives as a byref with a trailing byte-view projection, because C#
    // emits the offset as pointer arithmetic; index zero emits no arithmetic at all, so
    // the byref names the whole cell and the read has to decide for itself that a narrower
    // requested width means "reinterpret these bytes" rather than "convert this value".
    //
    // The oracle is `System.BitConverter`, which is not derived from PawPrint: the bytes the
    // interpreter hands back must be the bytes the host runtime finds at that address.

    /// A frame whose argument 0 holds `cell`, with `ManagedPointer` addressing that argument
    /// on the eval stack — the shape `ldloca`/`ldarga` produces, with no projections at all.
    let private stateWithByrefToCell
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : NullaryIlOp)
        (cell : CliType)
        : IlMachineState * ThreadId
        =
        // The placeholder is popped straight off again: the byref cannot be built until the
        // frame exists, and the frame does not exist until `stateWithNullary` has run.
        let state, thread =
            stateWithNullary loggerFactory op (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        let frame = state.ThreadState.[thread].ActiveMethodState
        let _, state = IlMachineState.popEvalStack thread state

        let ptr = ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, 0us), [])

        let state =
            state
            |> IlMachineState.setArgument thread frame 0us cell
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread

        state, thread

    let private runLdindOverCell (op : NullaryIlOp) (cell : CliType) : EvalStackValue =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithByrefToCell loggerFactory op cell

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed
            IlMachineState.popEvalStack thread state |> fst
        | other -> failwith $"Expected %O{op} to step, got %O{other}"

    let private int32Value (reason : string) (value : EvalStackValue) : int32 =
        match value with
        | EvalStackValue.Int32 source -> Int32Source.value reason source
        | other -> failwith $"Expected an int32 for %s{reason}, got %O{other}"

    [<Test>]
    let ``a narrow ldind over an int64 cell reinterprets its bytes`` () : unit =
        let property (value : int64) : unit =
            let cell = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value))
            let bytes = BitConverter.GetBytes value

            runLdindOverCell NullaryIlOp.Ldind_u1 cell
            |> int32Value "ldind.u1"
            |> shouldEqual (int32 bytes.[0])

            runLdindOverCell NullaryIlOp.Ldind_i1 cell
            |> int32Value "ldind.i1"
            |> shouldEqual (int32 (sbyte bytes.[0]))

            runLdindOverCell NullaryIlOp.Ldind_u2 cell
            |> int32Value "ldind.u2"
            |> shouldEqual (int32 (BitConverter.ToUInt16 (bytes, 0)))

            runLdindOverCell NullaryIlOp.Ldind_i2 cell
            |> int32Value "ldind.i2"
            |> shouldEqual (int32 (BitConverter.ToInt16 (bytes, 0)))

            runLdindOverCell NullaryIlOp.Ldind_i4 cell
            |> int32Value "ldind.i4"
            |> shouldEqual (BitConverter.ToInt32 (bytes, 0))

        // The default int64 generator is size-bounded, so drive the full range explicitly:
        // a value whose low byte differs from its low word is what distinguishes a byte view
        // from a truncating conversion at all.
        let gen : Gen<int64> =
            Gen.frequency
                [
                    8, ArbMap.defaults |> ArbMap.generate<int64>
                    2, Gen.elements [ Int64.MinValue ; Int64.MaxValue ; -1L ; 0L ; 1L ; 0x0102030405060708L ]
                ]

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a narrow ldind over a float64 cell reinterprets its bytes`` () : unit =
        // `*(float*)&aDouble` is a reinterpretation of the low four bytes of the bit
        // pattern, not the numeric narrowing `(float)aDouble`; 2.0 is the cleanest witness,
        // because its low four bytes are zero while `(float)2.0` is 2.0f.
        let cell = CliType.Numeric (CliNumericType.Float64 2.0)
        let bytes = BitConverter.GetBytes 2.0

        match runLdindOverCell NullaryIlOp.Ldind_r4 cell with
        | EvalStackValue.Float actual -> actual |> shouldEqual (float (BitConverter.ToSingle (bytes, 0)))
        | other -> failwith $"Expected a float from ldind.r4, got %O{other}"

        runLdindOverCell NullaryIlOp.Ldind_i4 cell
        |> int32Value "ldind.i4"
        |> shouldEqual (BitConverter.ToInt32 (bytes, 0))

    [<Test>]
    let ``a narrow ldind over a native-int cell reinterprets its bytes`` () : unit =
        let value = 0x1122334455667788L

        let cell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim value))

        let bytes = BitConverter.GetBytes value

        runLdindOverCell NullaryIlOp.Ldind_u1 cell
        |> int32Value "ldind.u1"
        |> shouldEqual (int32 bytes.[0])

        runLdindOverCell NullaryIlOp.Ldind_i4 cell
        |> int32Value "ldind.i4"
        |> shouldEqual (BitConverter.ToInt32 (bytes, 0))

    [<Test>]
    let ``a narrow ldind unwraps a primitive-like wrapper before deciding`` () : unit =
        // A guest's `IntPtr` local is stored as the single-field wrapper, not as the bare
        // native int: `EvalStackValue.ofCliType` flattens it on the way to the eval stack,
        // so a routing decision taken on the wrapper's own shape would answer a different
        // question from the one the value-coercion path asks.
        let value = 0x1122334455667788L

        let intPtrHandle =
            AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.IntPtr

        let cell =
            {
                CliField.Id = FieldId.named "_value"
                CliField.Name = "_value"
                Contents = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim value))
                Offset = None
                Type = intPtrHandle
                MarshallingDescriptor = None
            }
            |> List.singleton
            |> SynthesisedLayoutKind.ofFields baseClassTypes concreteTypes intPtrHandle Layout.Default CharSet.Ansi
            |> CliType.ValueType

        match cell with
        | CliType.ValueType vt -> vt.PrimitiveLikeKind |> shouldEqual (Some PrimitiveLikeKind.FlattenToNativeInt)
        | other -> failwith $"Expected a value type, got %O{other}"

        let bytes = BitConverter.GetBytes value

        runLdindOverCell NullaryIlOp.Ldind_u1 cell
        |> int32Value "ldind.u1"
        |> shouldEqual (int32 bytes.[0])

    [<Test>]
    let ``an equal-width ldind over a provenance-bearing cell keeps the provenance`` () : unit =
        // The other side of the routing decision. A pointer identity has no byte image at all
        // (`CliType.ToBytes` refuses one), so a read at the pointer's own width must return the
        // cell rather than its bytes: widening the predicate to divert equal-width reads too
        // would turn every such read into a byte-addressability refusal.
        let handle = RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 1)

        let cell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr handle))

        runLdindOverCell NullaryIlOp.Ldind_i cell
        |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr handle))

        // Synthesised hash bits are byte-imageless for the same reason, and `ldind.i8` over a
        // native-int cell is a widening rather than a narrowing, so it too stays typed.
        let hashCell =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.OpaqueHashBits 0x40L))

        runLdindOverCell NullaryIlOp.Ldind_i8 hashCell
        |> shouldEqual (EvalStackValue.Int64 (Int64Source.OpaqueHashBits 0x40L))

    // ---------------------------------------------------------------------
    // `And` over an array-element byref.
    //
    // The offset of element `i` is `i * stride`, with the stride taken from
    // the array's recorded `ElementStride` — a property of the element type,
    // so the question is answerable even for `Array.Empty<T>()`, which has no
    // cell to measure. `MemoryMarshal.GetArrayDataReference` hands out a
    // byref to index 0 of an empty array without a bounds check, and
    // `Unsafe.Add` walks it, so such a byref is reachable from legal IL.
    // ---------------------------------------------------------------------

    /// `And` whose operands must be built against an already-populated heap, and so cannot
    /// be handed to `stateWithNullary` up front.
    let private runAndAgainstHeap
        (build : IlMachineState -> IlMachineState * EvalStackValue * EvalStackValue)
        : EvalStackValue
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let op = NullaryIlOp.And

        let state, thread =
            stateWithNullary loggerFactory op (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        // Drop the seed value `stateWithNullary` pushed; this test supplies both operands.
        let _, state = IlMachineState.popEvalStack thread state
        let state, first, second = build state

        let state =
            state
            |> IlMachineState.pushToEvalStack' first thread
            |> IlMachineState.pushToEvalStack' second thread

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid, _) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | [ actual ] -> actual
            | other -> failwith $"Expected And to leave one stack value, got %O{other}"
        | other -> failwith $"Expected And to step, got %O{other}"

    /// The byte offset `And` computes for element `index` of a fresh `elementType[len]`,
    /// recovered by masking with -1 (which preserves every bit).
    let private elementByteOffset
        (elementType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (len : int)
        (index : int)
        : int64
        =
        let result =
            runAndAgainstHeap (fun state ->
                let handle = AllConcreteTypes.getRequiredNonGenericHandle concreteTypes elementType
                let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

                let arr, state =
                    IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero handle) (fun () -> zero) len state

                let ptr =
                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), [])
                    |> EvalStackValue.ManagedPointer

                state, ptr, nativeConst -1L
            )

        match result with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim bits) -> bits
        | other -> failwith $"expected And to yield verbatim bits, got %O{other}"

    [<Test>]
    let ``And scales an array-element index by the element's stride`` () : unit =
        elementByteOffset baseClassTypes.Int32 4 2
        |> shouldEqual (2L * int64 sizeof<int32>)

        elementByteOffset baseClassTypes.Int64 4 3
        |> shouldEqual (3L * int64 sizeof<int64>)

        elementByteOffset baseClassTypes.Byte 4 3
        |> shouldEqual (3L * int64 sizeof<byte>)

    [<Test>]
    let ``And answers for an empty array, at every index rather than only zero`` () : unit =
        elementByteOffset baseClassTypes.Int32 0 0 |> shouldEqual 0L

        elementByteOffset baseClassTypes.Int32 0 2
        |> shouldEqual (2L * int64 sizeof<int32>)

        elementByteOffset baseClassTypes.Int64 0 3
        |> shouldEqual (3L * int64 sizeof<int64>)

        // The empty array must agree with the populated one of the same element type: the
        // offset is a fact about the element type, not about how many cells exist.
        elementByteOffset baseClassTypes.Int32 0 2
        |> shouldEqual (elementByteOffset baseClassTypes.Int32 8 2)
