namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestFaultHandlers =

    // The factory is intentionally not disposed: the DumpedAssembly keeps the logger.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithRegions
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (regions : ExceptionRegion seq)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectConstructor =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = ".ctor" && (MethodInfo.arity method = 0))

        let state, signature =
            IlMachineState.concretizeMethodSignature
                loggerFactory
                bct
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                objectConstructor.Signature

        let ops : (IlOp * int) list =
            [
                IlOp.Nullary NullaryIlOp.Nop, 0
                IlOp.Nullary NullaryIlOp.Nop, 1
                IlOp.Nullary NullaryIlOp.Endfinally, 10
                IlOp.Nullary NullaryIlOp.Ret, 11
            ]

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            {
                Instructions = ops
                Locations = ops |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
                LocalsInit = false
                LocalVars = None
                ExceptionRegions = regions |> ImmutableArray.CreateRange
            }

        let method =
            objectConstructor
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::.ctor is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::.ctor is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il instructions) signature

        state, method

    let private stateWithMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (regions : ExceptionRegion seq)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState loggerFactory |> methodWithRegions loggerFactory regions

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    bct
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating fault-handler test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
        },
        thread

    let private faultOffset : ExceptionOffset =
        {
            TryOffset = 0
            TryLength = 4
            HandlerOffset = 10
            HandlerLength = 1
        }

    let private shouldBeResumeAfterFinally
        (expectedTarget : int)
        (actual : ExceptionContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option)
        : unit
        =
        match actual with
        | Some (ExceptionContinuation.ResumeAfterFinally target) -> target |> shouldEqual expectedTarget
        | other -> failwith $"Expected ResumeAfterFinally %i{expectedTarget}, got %O{other}"

    let private appendReturningFrame (state : IlMachineState) (thread : ThreadId) : IlMachineState * FrameId * FrameId =
        let threadState = state.ThreadState.[thread]
        let callerFrameId = threadState.ActiveMethodState

        let returnState : MethodReturnState =
            {
                JumpTo = callerFrameId
                WasInitialisingType = None
                Constructing = ConstructionState.NotConstructing
                CallSiteIlOpIndex = threadState.MethodState.IlOpIndex
                ReturnValueDisposition = ReturnValueDisposition.PushToCaller
                WrapExceptionInTargetInvocation = false
            }

        let calleeFrame =
            { threadState.MethodState with
                ReturnState = Some returnState
            }

        let calleeFrameId, threadState = ThreadState.appendFrame calleeFrame threadState
        let threadState = ThreadState.setActiveFrame calleeFrameId threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        },
        callerFrameId,
        calleeFrameId

    [<Test>]
    let ``returning from a frame removes the completed frame`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread = stateWithMethod loggerFactory []
        let state, callerFrameId, calleeFrameId = appendReturningFrame state thread

        state.ThreadState.[thread].LiveFrameCount |> shouldEqual 2

        let state =
            match IlMachineState.returnStackFrame loggerFactory bct thread state with
            | ReturnFrameResult.NormalReturn state -> state
            | other -> failwith $"Expected normal frame return, got %O{other}"

        let threadState = state.ThreadState.[thread]
        threadState.ActiveMethodState |> shouldEqual callerFrameId
        threadState.LiveFrameCount |> shouldEqual 1

        let ex =
            Assert.Throws<System.Exception> (fun () -> ThreadState.getFrame calleeFrameId threadState |> ignore)

        ex.Message.Contains "not live" |> shouldEqual true

        let nextFrameId, _ = ThreadState.appendFrame threadState.MethodState threadState
        nextFrameId = calleeFrameId |> shouldEqual false

    [<Test>]
    let ``exception unwinding removes the unwound frame`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread = stateWithMethod loggerFactory []
        let state, callerFrameId, calleeFrameId = appendReturningFrame state thread

        state.ThreadState.[thread].LiveFrameCount |> shouldEqual 2

        let exceptionObject = ManagedHeapAddress 42

        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Object

        let cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                ExceptionObject = exceptionObject
                StackTrace = []
            }

        // Neither frame has any exception region, so the first pass runs straight off the end of
        // the stack and the second pass has no cleanup to run — it only unwinds. The callee frame
        // must be gone by the time the exception is reported unhandled, and the outermost frame
        // must not be: that is the frame the report is about.
        let state =
            match ExceptionDispatching.dispatchException loggerFactory bct state thread cliException objectHandle with
            | ExceptionDispatchResult.ExceptionUnhandled (state, _) -> state
            | other -> failwith $"Expected unhandled exception after unwinding to caller, got %O{other}"

        let threadState = state.ThreadState.[thread]
        threadState.ActiveMethodState |> shouldEqual callerFrameId
        threadState.LiveFrameCount |> shouldEqual 1

        let ex =
            Assert.Throws<System.Exception> (fun () -> ThreadState.getFrame calleeFrameId threadState |> ignore)

        ex.Message.Contains "not live" |> shouldEqual true

    [<Test>]
    let ``Fault handler is entered as exceptional cleanup`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithMethod loggerFactory [ ExceptionRegion.Fault faultOffset ]

        let exceptionObject = ManagedHeapAddress 42

        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Object

        let methodState = state.ThreadState.[thread].MethodState

        let regions =
            match MethodInfo.tryIlBody methodState.ExecutingMethod with
            | Some instructions -> instructions.ExceptionRegions :> seq<_>
            | None -> failwith "expected the test method to have an IL body"

        // A `fault` is not a clause an exception can be *delivered* to, so the first pass never
        // offers it; it is the second pass, unwinding out of the frame entirely, that selects it.
        let handler =
            match ExceptionHandling.cleanupRegionsBetween regions 1 None with
            | [ ExceptionRegion.Fault offset ] -> ExceptionRegion.Fault offset
            | other -> failwith $"Expected exactly one fault handler to run, got %O{other}"

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 123)) thread

        let methodState =
            { state.ThreadState.[thread].MethodState with
                PendingPrefix =
                    { PrefixState.empty with
                        Tail = true
                    }
            }

        let threadState =
            ThreadState.setFrame state.ThreadState.[thread].ActiveMethodState methodState state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread threadState
            }

        let unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Exception =
                    {
                        ExceptionObject = exceptionObject
                        StackTrace = []
                    }
                ExceptionType = objectHandle
                Frame = state.ThreadState.[thread].ActiveMethodState
                PC = 1
                Target = ExceptionSearchOutcome.NoHandler
            }

        let offset =
            match handler with
            | ExceptionRegion.Fault offset -> offset
            | other -> failwith $"Expected fault handler, got %O{other}"

        let state =
            ExceptionDispatching.enterFaultHandler thread methodState threadState state offset unwind

        let methodState = state.ThreadState.[thread].MethodState
        methodState.IlOpIndex |> shouldEqual faultOffset.HandlerOffset
        methodState.EvaluationStack.Values |> shouldEqual []
        methodState.PendingPrefix |> shouldEqual PrefixState.empty

        match methodState.ExceptionContinuation with
        | Some (ExceptionContinuation.PropagatingException actual) ->
            actual.Exception.ExceptionObject |> shouldEqual exceptionObject
            actual.Exception.StackTrace |> List.isEmpty |> shouldEqual true
        | other -> failwith $"Expected propagating exception continuation, got %O{other}"

    [<Test>]
    let ``Leave schedules finally but not fault`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let finallyOffset : ExceptionOffset =
            {
                TryOffset = 0
                TryLength = 4
                HandlerOffset = 20
                HandlerLength = 1
            }

        let _state, method =
            initialState loggerFactory
            |> methodWithRegions
                loggerFactory
                [ ExceptionRegion.Fault faultOffset ; ExceptionRegion.Finally finallyOffset ]

        let blocks = ExceptionHandling.findFinallyBlocksToRun 1 8 method

        blocks |> shouldEqual [ finallyOffset ]

    /// CoreCLR compares a filter's result with `EXCEPTION_EXECUTE_HANDLER` (1) exactly, so every
    /// other int32, zero or not, declines the exception.
    [<Test>]
    let ``Endfilter accepts exactly the int32 result 1`` () : unit =
        NullaryIlOp.endfilterAccepts (EvalStackValue.Int32 (Int32Source.Verbatim 0))
        |> shouldEqual false

        NullaryIlOp.endfilterAccepts (EvalStackValue.Int32 (Int32Source.Verbatim 1))
        |> shouldEqual true

        for rejected in [ 2 ; -1 ; 256 ; 0x10000 ; System.Int32.MinValue ; System.Int32.MaxValue ] do
            NullaryIlOp.endfilterAccepts (EvalStackValue.Int32 (Int32Source.Verbatim rejected))
            |> shouldEqual false

    [<Test>]
    let ``Endfilter accepts no int32 result other than 1`` () : unit =
        let property (result : int32) : bool =
            NullaryIlOp.endfilterAccepts (EvalStackValue.Int32 (Int32Source.Verbatim result)) = (result = 1)

        let config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

        Check.One (
            config,
            Prop.forAll (Arb.fromGen (Gen.choose (System.Int32.MinValue, System.Int32.MaxValue))) property
        )

    [<Test>]
    let ``Endfilter refuses a truncated managed pointer rather than accepting it`` () : unit =
        // `conv.i4` on a byref keeps the pointer alive so an alignment mask can be
        // asked of it; the low half of that address is unknown and may be 1. An
        // answer either way here would run, or skip, an exception handler on the
        // strength of an address PawPrint does not model.
        let narrowedByref =
            EvalStackValue.Int32 (
                Int32Source.NarrowedManagedPointer (
                    ManagedPointerSource.Byref (
                        ByrefRoot.NativeMemoryByte (NativeMemoryBlockId.NativeMemoryBlockId 0, 4),
                        []
                    )
                )
            )

        let exn =
            Assert.Throws (fun () -> NullaryIlOp.endfilterAccepts narrowedByref |> ignore<bool>)

        exn.Message |> shouldContainText "truncated to 32 bits"

    [<Test>]
    let ``Exception continuation stack is last-in first-out`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread = stateWithMethod loggerFactory []

        let methodState = state.ThreadState.[thread].MethodState

        let first = ExceptionContinuation.ResumeAfterFinally 10
        let second = ExceptionContinuation.ResumeAfterFinally 20

        let methodState =
            methodState
            |> MethodState.pushExceptionContinuation (ExceptionContinuationScope.FinallyHandler faultOffset) first
            |> MethodState.pushExceptionContinuation (ExceptionContinuationScope.FinallyHandler faultOffset) second

        methodState.ExceptionContinuation |> shouldBeResumeAfterFinally 20

        let popped, methodState = MethodState.popExceptionContinuation methodState

        popped |> Option.map _.Continuation |> shouldBeResumeAfterFinally 20
        methodState.ExceptionContinuation |> shouldBeResumeAfterFinally 10

        let popped, methodState = MethodState.popExceptionContinuation methodState

        popped |> Option.map _.Continuation |> shouldBeResumeAfterFinally 10
        methodState.ExceptionContinuation |> Option.isNone |> shouldEqual true
