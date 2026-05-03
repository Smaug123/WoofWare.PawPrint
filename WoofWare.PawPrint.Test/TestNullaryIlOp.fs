namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata.Ecma335
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

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

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

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Object

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
        | SyntheticCrossArrayOffset of int64
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
            |> MethodInfo.setMethodVars (Some instructions) signature

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
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }
            |> IlMachineState.pushToEvalStack' stackValue thread

        state, thread

    let private executeNullarySingle (op : NullaryIlOp) (stackValue : EvalStackValue) : EvalStackValue =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithNullary loggerFactory op stackValue

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let methodState = state.ThreadState.[thread].MethodState

            methodState.IlOpIndex |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary op))

            match methodState.EvaluationStack.Values with
            | [ actual ] -> actual
            | other -> failwith $"Expected %O{op} to leave one stack value, got %O{other}"
        | other -> failwith $"Expected %O{op} to step, got %O{other}"

    let private executeNullaryBinary
        (op : NullaryIlOp)
        (value1 : EvalStackValue)
        (value2 : EvalStackValue)
        : EvalStackValue
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithNullary loggerFactory op value1
        let state = state |> IlMachineState.pushToEvalStack' value2 thread

        match NullaryIlOp.execute loggerFactory baseClassTypes state thread op with
        | ExecutionResult.Stepped (state, whatWeDid) ->
            whatWeDid |> shouldEqual WhatWeDid.Executed

            let methodState = state.ThreadState.[thread].MethodState

            methodState.IlOpIndex |> shouldEqual (IlOp.NumberOfBytes (IlOp.Nullary op))

            match methodState.EvaluationStack.Values with
            | [ actual ] -> actual
            | other -> failwith $"Expected %O{op} to leave one stack value, got %O{other}"
        | other -> failwith $"Expected %O{op} to step, got %O{other}"

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
                genBits
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
        | NegCase.Int64Value value -> EvalStackValue.Int64 value, EvalStackValue.Int64 (expectedNegInt64 value)
        | NegCase.NativeIntValue nativeInt ->
            match nativeInt with
            | NativeIntNegInput.Verbatim value ->
                EvalStackValue.NativeInt (NativeIntSource.Verbatim value),
                EvalStackValue.NativeInt (NativeIntSource.Verbatim (expectedNegInt64 value))
            | NativeIntNegInput.SyntheticCrossArrayOffset value ->
                EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset value),
                EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset (expectedNegInt64 value))
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
            NegCase.NativeIntValue (NativeIntNegInput.SyntheticCrossArrayOffset Int64.MinValue)
            NegCase.NativeIntValue (NativeIntNegInput.SyntheticCrossArrayOffset -1L)
            NegCase.NativeIntValue (NativeIntNegInput.SyntheticCrossArrayOffset 0L)
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

        executeNullarySingle NullaryIlOp.Neg input |> assertEvalStackValueEqual expected

    let private stablePeByteRangePointerIn
        (assemblyFullName : string)
        (relativeVirtualAddress : int)
        (byteOffset : int)
        : ManagedPointerSource
        =
        let peByteRange =
            {
                AssemblyFullName = assemblyFullName
                Source = PeByteRangePointerSource.ManagedResource "Example.resources"
                RelativeVirtualAddress = relativeVirtualAddress
                Size = 64
            }

        ManagedPointerSource.Byref (
            ByrefRoot.PeByteRange peByteRange,
            [
                ByrefProjection.ReinterpretAs byteType
                ByrefProjection.ByteOffset byteOffset
            ]
        )

    let private stablePeByteRangePointerAt (relativeVirtualAddress : int) (byteOffset : int) : ManagedPointerSource =
        stablePeByteRangePointerIn "Example" relativeVirtualAddress byteOffset

    let private staticFieldPointer (fieldRow : int) : ManagedPointerSource =
        let field =
            MetadataTokens.FieldDefinitionHandle fieldRow
            |> ComparableFieldDefinitionHandle.Make

        ManagedPointerSource.Byref (ByrefRoot.StaticField (objectHandle, field), [])

    let private syntheticIntegerAddressBits (ptr : ManagedPointerSource) : int64 =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        match IlMachineState.tryManagedPointerIntegerAddress (initialState loggerFactory) ptr with
        | Some address -> address.Bits
        | None -> failwith $"Expected synthetic integer address bits for %O{ptr}"

    [<Test>]
    let ``Conv_U8 exposes stable PE byte-range synthetic address bits`` () : unit =
        let ptr = stablePeByteRangePointerAt 4096 7
        let ptrBits = syntheticIntegerAddressBits ptr
        let expected = EvalStackValue.Int64 ptrBits

        executeNullarySingle NullaryIlOp.Conv_U8 (EvalStackValue.ManagedPointer ptr)
        |> shouldEqual expected

        executeNullarySingle NullaryIlOp.Conv_U8 (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr))
        |> shouldEqual expected

        ptrBits &&& 4_294_967_295L |> shouldEqual 4103L

        let sameRvaInOtherAssembly = stablePeByteRangePointerIn "Other" 4096 7

        syntheticIntegerAddressBits sameRvaInOtherAssembly |> shouldNotEqual ptrBits

    [<Test>]
    let ``Synthetic integer address bits distinguish static fields on the same type`` () : unit =
        let first = staticFieldPointer 1
        let second = staticFieldPointer 2

        syntheticIntegerAddressBits first
        |> shouldNotEqual (syntheticIntegerAddressBits second)

    [<Test>]
    let ``Conv_U8 converts null managed pointer to zero`` () : unit =
        let expected = EvalStackValue.Int64 0L

        executeNullarySingle NullaryIlOp.Conv_U8 (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
        |> shouldEqual expected

        executeNullarySingle
            NullaryIlOp.Conv_U8
            (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        |> shouldEqual expected

    [<Test>]
    let ``Conv_U8 refuses unsupported managed pointer address`` () : unit =
        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress 707), [])

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                executeNullarySingle NullaryIlOp.Conv_U8 (EvalStackValue.ManagedPointer ptr)
                |> ignore
            )

        ex.Message |> shouldContainText "Conv_U8: refusing to convert managed pointer"

    [<Test>]
    let ``Cgt_un and Clt_un compare stable managed pointer address bits`` () : unit =
        let ptr = stablePeByteRangePointerAt 4096 4
        let ptrAddressBits = syntheticIntegerAddressBits ptr

        let afterPtrBits =
            EvalStackValue.NativeInt (NativeIntSource.Verbatim (ptrAddressBits + 1L))

        let ptrValue = EvalStackValue.ManagedPointer ptr

        executeNullaryBinary NullaryIlOp.Clt_un ptrValue afterPtrBits
        |> shouldEqual (EvalStackValue.Int32 1)

        executeNullaryBinary NullaryIlOp.Cgt_un ptrValue (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
        |> shouldEqual (EvalStackValue.Int32 1)

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
