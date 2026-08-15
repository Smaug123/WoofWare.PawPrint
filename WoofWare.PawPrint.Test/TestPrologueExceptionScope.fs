namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A frame whose prologue has not finished has not started, so its exception-handling regions are
/// not in scope for the exception the prologue itself raises.
///
/// The CLR emits the type-initialisation check outside the method's EH regions, so a failure goes
/// to the *caller* even when the method's try region starts at the first instruction and catches
/// exactly that type. Measured on .NET 10 with a whole-body `catch (TypeInitializationException)`
/// over a type already marked failed: the caller catches it and the method's own handler never
/// runs.
///
/// PawPrint raises that exception with the callee's frame established — which is what lets the
/// trace name the method — so the frame's clauses are reachable and have to be excluded
/// deliberately. Before `PendingTypeInit` gated them, the frame's own `catch` would be selected.
///
/// Asserted here rather than by a `sourcesPure` guest because the guest cannot reach it: the test
/// harness compiles unoptimized, which puts a `nop` at IL offset 0 *outside* the try, so the
/// region does not cover the offset the prologue raises from. Optimized IL — which is what every
/// BCL assembly PawPrint interprets is — starts the try at 0.
[<TestFixture>]
module TestPrologueExceptionScope =

    // The factory is intentionally not disposed: the DumpedAssembly keeps the logger.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    /// A `catch (object)` covering IL offsets 0..3, with its handler body at offset 10. Catching
    /// `System.Object` makes the clause match anything, so a search that considers this frame at
    /// all must select it — the test cannot pass by the exception simply not being assignable.
    let private catchAllOffset : ExceptionOffset =
        {
            TryOffset = 0
            TryLength = 4
            HandlerOffset = 10
            HandlerLength = 1
        }

    let private methodWithRegion
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (region : ExceptionRegion)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectConstructor =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = ".ctor" && (MethodInfo.arity method = 0))

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        bct
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectConstructor.Signature

        let ops : (IlOp * int) list =
            [
                IlOp.Nullary NullaryIlOp.Nop, 0
                IlOp.Nullary NullaryIlOp.Nop, 1
                IlOp.Nullary NullaryIlOp.Pop, 10
                IlOp.Nullary NullaryIlOp.Ret, 11
            ]

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            {
                Instructions = ops
                Locations = ops |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
                LocalsInit = false
                LocalVars = None
                ExceptionRegions = ImmutableArray.Create region
            }

        let method =
            objectConstructor
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::.ctor is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::.ctor is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il instructions) signature

        state, method

    /// Two frames of the given method, the callee returning into the caller at offset 0 — so the
    /// caller's try covers its call site and its handler is a genuine candidate.
    let private twoFrames
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (region : ExceptionRegion)
        : IlMachineState * ThreadId * FrameId * FrameId
        =
        let state =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = concreteTypes
            }

        let state, method = methodWithRegion loggerFactory region state

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
            | Error missing -> failwith $"Unexpected missing assembly references creating test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }

        let threadState = state.ThreadState.[thread]
        let callerFrameId = threadState.ActiveMethodState

        let returnState : MethodReturnState =
            {
                JumpTo = callerFrameId
                WasInitialisingType = None
                Constructing = ConstructionState.NotConstructing
                CallSiteIlOpIndex = 0
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
        thread,
        callerFrameId,
        calleeFrameId

    let private objectHandle () : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Object

    let private dispatchAtCallee
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState
        =
        let cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                ExceptionObject = ManagedHeapAddress 42
                StackTrace = []
            }

        match ExceptionDispatching.dispatchException loggerFactory bct state thread cliException (objectHandle ()) with
        | ExceptionDispatchResult.Dispatched state -> state
        | other -> failwith $"Expected the exception to be delivered to a handler, got %O{other}"

    let private catchAll : ExceptionRegion =
        ExceptionRegion.Catch (MetadataToken.TypeDefinition bct.Object.TypeDefHandle, catchAllOffset)

    [<Test>]
    let ``a frame awaiting its prologue does not catch`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state, thread, callerFrameId, calleeFrameId = twoFrames loggerFactory catchAll

        // The callee has not run its type-initialisation check, so it has not started.
        let state =
            state
            |> IlMachineState.mapFrame thread calleeFrameId (MethodState.withPendingTypeInit (objectHandle ()))

        let state = dispatchAtCallee loggerFactory state thread

        // The caller's handler took it, and the callee frame was unwound on the way.
        let threadState = state.ThreadState.[thread]
        threadState.ActiveMethodState |> shouldEqual callerFrameId
        threadState.LiveFrameCount |> shouldEqual 1
        threadState.MethodState.IlOpIndex |> shouldEqual catchAllOffset.HandlerOffset

    [<Test>]
    let ``a frame that has started does catch`` () : unit =
        // The control for the test above: with no pending prologue the callee's own clause is
        // selected, so that test is measuring the gate rather than some unrelated reason the
        // callee could never have caught anything.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state, thread, _callerFrameId, calleeFrameId = twoFrames loggerFactory catchAll

        let state = dispatchAtCallee loggerFactory state thread

        let threadState = state.ThreadState.[thread]
        threadState.ActiveMethodState |> shouldEqual calleeFrameId
        threadState.LiveFrameCount |> shouldEqual 2
        threadState.MethodState.IlOpIndex |> shouldEqual catchAllOffset.HandlerOffset

    [<Test>]
    let ``a frame awaiting its prologue runs no cleanup`` () : unit =
        // The second pass obeys the same scope rule as the first. A frame that never began has no
        // `finally` to run, so the unwind must pass straight through it — otherwise cleanup for a
        // method whose first instruction never executed would run against uninitialised locals.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread, callerFrameId, calleeFrameId =
            twoFrames loggerFactory (ExceptionRegion.Finally catchAllOffset)

        let state =
            state
            |> IlMachineState.mapFrame thread calleeFrameId (MethodState.withPendingTypeInit (objectHandle ()))

        let cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                ExceptionObject = ManagedHeapAddress 42
                StackTrace = []
            }

        // Both frames run the same method, so both carry the same `finally`; the *caller's* is
        // legitimate and does run. What distinguishes the two is which frame the machine ends up
        // in — the callee's cleanup would be entered first, with the callee still live.
        let state =
            match
                ExceptionDispatching.dispatchException loggerFactory bct state thread cliException (objectHandle ())
            with
            | ExceptionDispatchResult.Dispatched state -> state
            | other -> failwith $"Expected the caller's finally to be entered, got %O{other}"

        let threadState = state.ThreadState.[thread]
        threadState.MethodState.IlOpIndex |> shouldEqual catchAllOffset.HandlerOffset
        threadState.ActiveMethodState |> shouldEqual callerFrameId
        threadState.LiveFrameCount |> shouldEqual 1

    [<Test>]
    let ``a frame that has started does run its cleanup`` () : unit =
        // The control for the test above: the same two frames and the same `finally`, differing
        // only in whether the callee's prologue is outstanding. Here the callee's own cleanup is
        // entered, with the callee still live — which is what the assertion above rules out.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread, _callerFrameId, calleeFrameId =
            twoFrames loggerFactory (ExceptionRegion.Finally catchAllOffset)

        let cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                ExceptionObject = ManagedHeapAddress 42
                StackTrace = []
            }

        let state =
            match
                ExceptionDispatching.dispatchException loggerFactory bct state thread cliException (objectHandle ())
            with
            | ExceptionDispatchResult.Dispatched state -> state
            | other -> failwith $"Expected the callee's finally to be entered, got %O{other}"

        let threadState = state.ThreadState.[thread]
        threadState.ActiveMethodState |> shouldEqual calleeFrameId
        threadState.LiveFrameCount |> shouldEqual 2
        threadState.MethodState.IlOpIndex |> shouldEqual catchAllOffset.HandlerOffset
