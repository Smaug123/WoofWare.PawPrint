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
module TestUnaryConstIlOp =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

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

    type private SignedBranchKind =
        | Blt
        | Bgt

    type private ComparisonRelation =
        | Less
        | Equal
        | Greater

    type private SignedBranchCase =
        {
            Kind : SignedBranchKind
            Value1 : int32
            Value2 : int32
            Offset : int32
        }

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithUnaryConst
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : UnaryConstIlOp)
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

        let op = IlOp.UnaryConst op

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

    let private stateWithUnaryConstStackValues
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : UnaryConstIlOp)
        (value1 : EvalStackValue)
        (value2 : EvalStackValue)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState loggerFactory |> methodWithUnaryConst loggerFactory op

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
                failwith $"Unexpected missing assembly references creating signed-branch test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }
            |> IlMachineState.pushToEvalStack' value1 thread
            |> IlMachineState.pushToEvalStack' value2 thread

        state, thread

    let private stateWithSignedBranch
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : UnaryConstIlOp)
        (value1 : int32)
        (value2 : int32)
        : IlMachineState * ThreadId
        =
        stateWithUnaryConstStackValues loggerFactory op (EvalStackValue.Int32 value1) (EvalStackValue.Int32 value2)

    let private genComparablePair : Gen<int32 * int32> =
        let generated =
            gen {
                let! pivot = Gen.choose (-1_000_000, 1_000_000)
                let! delta = Gen.choose (1, 1_000)

                let! relation =
                    Gen.elements
                        [
                            ComparisonRelation.Less
                            ComparisonRelation.Equal
                            ComparisonRelation.Greater
                        ]

                match relation with
                | ComparisonRelation.Less -> return int32 pivot, int32 (pivot + delta)
                | ComparisonRelation.Equal -> return int32 pivot, int32 pivot
                | ComparisonRelation.Greater -> return int32 (pivot + delta), int32 pivot
            }

        let edges =
            Gen.elements
                [
                    Int32.MinValue, Int32.MaxValue
                    Int32.MaxValue, Int32.MinValue
                    Int32.MinValue, Int32.MinValue
                    Int32.MaxValue, Int32.MaxValue
                    -1, 0
                    0, -1
                    0, 0
                ]

        Gen.frequency [ 8, generated ; 1, edges ]

    let private genNonZeroOffset : Gen<int32> =
        Gen.oneof [ Gen.choose (-20, -1) ; Gen.choose (1, 20) ] |> Gen.map int32

    let private genSignedBranchCase : Gen<SignedBranchCase> =
        gen {
            let! kind = Gen.elements [ SignedBranchKind.Blt ; SignedBranchKind.Bgt ]
            let! value1, value2 = genComparablePair
            let! offset = genNonZeroOffset

            return
                {
                    Kind = kind
                    Value1 = value1
                    Value2 = value2
                    Offset = offset
                }
        }

    let private opOfCase (case : SignedBranchCase) : UnaryConstIlOp =
        match case.Kind with
        | SignedBranchKind.Blt -> UnaryConstIlOp.Blt case.Offset
        | SignedBranchKind.Bgt -> UnaryConstIlOp.Bgt case.Offset

    let private isTaken (case : SignedBranchCase) : bool =
        match case.Kind with
        | SignedBranchKind.Blt -> case.Value1 < case.Value2
        | SignedBranchKind.Bgt -> case.Value1 > case.Value2

    let private executeBranch (op : UnaryConstIlOp) (value1 : EvalStackValue) (value2 : EvalStackValue) : MethodState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, thread = stateWithUnaryConstStackValues loggerFactory op value1 value2
        let state, whatWeDid = UnaryConstIlOp.execute state thread op

        whatWeDid |> shouldEqual WhatWeDid.Executed

        state.ThreadState.[thread].MethodState

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

    let private syntheticIntegerAddressBits (ptr : ManagedPointerSource) : int64 =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        match IlMachineState.tryManagedPointerIntegerAddress (initialState loggerFactory) ptr with
        | Some address -> address.Bits
        | None -> failwith $"Expected synthetic integer address bits for %O{ptr}"

    [<Test>]
    let ``Blt and Bgt branch relative to next instruction iff signed comparison holds`` () : unit =
        let mutable bltTaken = 0
        let mutable bltNotTaken = 0
        let mutable bgtTaken = 0
        let mutable bgtNotTaken = 0

        let property (case : SignedBranchCase) : unit =
            let op = opOfCase case
            let taken = isTaken case

            match case.Kind, taken with
            | SignedBranchKind.Blt, true -> bltTaken <- bltTaken + 1
            | SignedBranchKind.Blt, false -> bltNotTaken <- bltNotTaken + 1
            | SignedBranchKind.Bgt, true -> bgtTaken <- bgtTaken + 1
            | SignedBranchKind.Bgt, false -> bgtNotTaken <- bgtNotTaken + 1

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory

            let state, thread = stateWithSignedBranch loggerFactory op case.Value1 case.Value2

            let state, whatWeDid = UnaryConstIlOp.execute state thread op

            whatWeDid |> shouldEqual WhatWeDid.Executed

            let instructionSize = IlOp.NumberOfBytes (IlOp.UnaryConst op)
            let branchDelta = if taken then int case.Offset else 0
            let expectedPc = instructionSize + branchDelta

            let methodState = state.ThreadState.[thread].MethodState
            methodState.IlOpIndex |> shouldEqual expectedPc
            methodState.EvaluationStack.Values |> shouldEqual []

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSignedBranchCase) property)

        if bltTaken = 0 || bltNotTaken = 0 || bgtTaken = 0 || bgtNotTaken = 0 then
            failwith
                $"generator missed required regimes: bltTaken=%d{bltTaken}, bltNotTaken=%d{bltNotTaken}, bgtTaken=%d{bgtTaken}, bgtNotTaken=%d{bgtNotTaken}"

    [<Test>]
    let ``Unsigned branches compare stable managed pointer address bits`` () : unit =
        let ptr = stablePeByteRangePointerAt 4096 4
        let ptrAddressBits = syntheticIntegerAddressBits ptr
        let ptrBits = EvalStackValue.NativeInt (NativeIntSource.Verbatim ptrAddressBits)

        let afterPtrBits =
            EvalStackValue.NativeInt (NativeIntSource.Verbatim (ptrAddressBits + 1L))

        let ptrValue = EvalStackValue.ManagedPointer ptr
        let nativePtrValue = EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)

        let cases : (UnaryConstIlOp * EvalStackValue * EvalStackValue * bool) list =
            [
                UnaryConstIlOp.Bge_un_s 7y, ptrBits, ptrValue, true
                UnaryConstIlOp.Blt_un_s 7y, ptrValue, afterPtrBits, true
                UnaryConstIlOp.Bgt_un_s 7y, nativePtrValue, ptrBits, false
                UnaryConstIlOp.Ble_un_s 7y, nativePtrValue, ptrBits, true
                UnaryConstIlOp.Bgt_un_s 7y, ptrValue, EvalStackValue.ManagedPointer ManagedPointerSource.Null, true
            ]

        for op, value1, value2, taken in cases do
            let methodState = executeBranch op value1 value2
            let instructionSize = IlOp.NumberOfBytes (IlOp.UnaryConst op)
            let expectedPc = instructionSize + if taken then 7 else 0

            methodState.IlOpIndex |> shouldEqual expectedPc
            methodState.EvaluationStack.Values |> shouldEqual []

    [<Test>]
    let ``Unsigned branch refuses unsupported managed pointer address comparison`` () : unit =
        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress 707), [])

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                executeBranch
                    (UnaryConstIlOp.Bge_un_s 7y)
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim 1L))
                    (EvalStackValue.ManagedPointer ptr)
                |> ignore
            )

        ex.Message |> shouldContainText "invalid for comparing"

    [<Test>]
    let ``Unsigned branch refuses different managed pointer address roots`` () : unit =
        let left = stablePeByteRangePointerIn "Left" 4096 4
        let right = stablePeByteRangePointerIn "Right" 4096 4

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                executeBranch
                    (UnaryConstIlOp.Bge_un_s 7y)
                    (EvalStackValue.ManagedPointer left)
                    (EvalStackValue.ManagedPointer right)
                |> ignore
            )

        ex.Message |> shouldContainText "different address roots"
