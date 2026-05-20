namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineThreadState =
    // --- Cross-thread frame resolution primitives ---

    let getFrame (thread : ThreadId) (frameId : FrameId) (state : IlMachineState) : MethodState =
        ThreadState.getFrame frameId state.ThreadState.[thread]

    let setFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (frame : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.setFrame frameId frame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let mapFrame
        (thread : ThreadId)
        (frameId : FrameId)
        (f : MethodState -> MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let threadState = ThreadState.mapFrame frameId f threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    /// Replace the `WasConstructingObj` of the active frame's `ReturnState` with `Some newAddr`.
    /// Used by InternalCall constructors that allocate the constructed object themselves
    /// (e.g. `String..ctor(char*)`): the placeholder allocated by `executeNewobj` is discarded
    /// and the next `returnStackFrame` pushes `newAddr` onto the caller's eval stack instead.
    /// Fails loudly if invoked outside a constructor frame.
    let withReplacedConstructedObject
        (newAddr : ManagedHeapAddress)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let activeFrameId = threadState.ActiveMethodState

        let updateFrame (frame : MethodState) : MethodState =
            match frame.ReturnState with
            | None ->
                failwith
                    $"withReplacedConstructedObject: active frame %s{frame.ExecutingMethod.Name} has no ReturnState; cannot redirect a non-existent constructor return"
            | Some returnState ->
                match returnState.WasConstructingObj with
                | None ->
                    failwith
                        $"withReplacedConstructedObject: active frame %s{frame.ExecutingMethod.Name} is not a constructor frame (WasConstructingObj is None)"
                | Some _ ->
                    { frame with
                        ReturnState =
                            Some
                                { returnState with
                                    WasConstructingObj = Some newAddr
                                }
                    }

        let threadState = ThreadState.mapFrame activeFrameId updateFrame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    /// Set `WrapExceptionInTargetInvocation = true` on the active frame's `ReturnState`.
    /// Used by `Activator.CreateInstance<T>()` after `ensureTypeInitialised` has just
    /// pushed `T`'s `.cctor` frame: marking it ensures that if the .cctor throws (producing
    /// a `TypeInitializationException` via the existing `WasInitialisingType` wrap), the
    /// dispatcher *also* wraps the resulting TIE in a fresh `TargetInvocationException`
    /// when the cctor frame unwinds, matching CoreCLR's `CreateInstanceOfT` semantics for
    /// the cctor-failure path. Fails loudly if invoked on a frame with no `ReturnState`.
    let markActiveFrameWrapInTargetInvocation (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let threadState = state.ThreadState.[thread]
        let activeFrameId = threadState.ActiveMethodState

        let updateFrame (frame : MethodState) : MethodState =
            match frame.ReturnState with
            | None ->
                failwith
                    $"markActiveFrameWrapInTargetInvocation: active frame %s{frame.ExecutingMethod.Name} has no ReturnState; cannot install wrap marker"
            | Some returnState ->
                { frame with
                    ReturnState =
                        Some
                            { returnState with
                                WrapExceptionInTargetInvocation = true
                            }
                }

        let threadState = ThreadState.mapFrame activeFrameId updateFrame threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let pushToEvalStack' (o : EvalStackValue) (thread : ThreadId) (state : IlMachineState) =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack' o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let pushToEvalStack (o : CliType) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let activeThreadState = state.ThreadState.[thread]

        let newThreadState =
            activeThreadState
            |> ThreadState.pushToEvalStack o activeThreadState.ActiveMethodState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let peekEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue option =
        ThreadState.peekEvalStack state.ThreadState.[thread]

    let popEvalStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue * IlMachineState =
        let ret, popped = ThreadState.popFromEvalStack state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread popped
            }

        ret, state

    let advanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.advanceProgramCounter |> Some
                    )
        }

    let setArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.setArrayValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let getArrayValue (arrayAllocation : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.getArrayValue arrayAllocation index state.ManagedHeap

    /// Pops a synthetic frame that is only a dispatch trampoline, not a real method return.
    /// The concrete callee it dispatches to is responsible for producing any return value.
    let returnFromSyntheticStackFrame (currentThread : ThreadId) (state : IlMachineState) : ReturnFrameResult =
        let threadStateWithSyntheticFrame = state.ThreadState.[currentThread]
        let syntheticFrameId = threadStateWithSyntheticFrame.ActiveMethodState

        match threadStateWithSyntheticFrame.MethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->
            match returnState.WasConstructingObj with
            | Some _ ->
                failwith
                    $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly represented object construction"
            | None ->
                if returnState.DispatchAsExceptionOnReturn then
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly requested exception dispatch on return"

                match returnState.WasInitialisingType with
                | None -> ()
                | Some _ ->
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly represented type initialisation"

                match threadStateWithSyntheticFrame.MethodState.EvaluationStack.Values with
                | [] -> ()
                | _ ->
                    failwith
                        $"Synthetic stack frame %s{threadStateWithSyntheticFrame.MethodState.ExecutingMethod.Name} unexpectedly had evaluation stack values"

                let callerFrame =
                    ThreadState.getFrame returnState.JumpTo threadStateWithSyntheticFrame

                let threadState =
                    threadStateWithSyntheticFrame
                    |> ThreadState.setActiveFrame returnState.JumpTo
                    |> ThreadState.removeFrame syntheticFrameId

                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread threadState
                }
                |> ReturnFrameResult.NormalReturn

    /// There might be no stack frame to return to, so you might get NoFrameToReturn.
    let returnStackFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ReturnFrameResult
        =
        let threadStateAtEndOfMethod = state.ThreadState.[currentThread]
        let returningFrameId = threadStateAtEndOfMethod.ActiveMethodState
        let returningMethodState = threadStateAtEndOfMethod.MethodState

        match returningMethodState.ReturnState with
        | None -> ReturnFrameResult.NoFrameToReturn
        | Some returnState ->

        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some finishedInitialising -> state.WithTypeEndInit currentThread finishedInitialising

        // Return to previous stack frame
        let callerFrame = ThreadState.getFrame returnState.JumpTo threadStateAtEndOfMethod

        let threadState =
            threadStateAtEndOfMethod
            |> ThreadState.setActiveFrame returnState.JumpTo
            |> ThreadState.removeFrame returningFrameId

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        match returnState.WasConstructingObj with
        | Some constructing ->
            if returnState.DispatchAsExceptionOnReturn then
                // This ctor was constructing a runtime-synthesised exception object.
                // Don't push it onto the eval stack; signal to the caller that exception
                // dispatch should occur.
                let constructed = state.ManagedHeap.NonArrayObjects.[constructing]
                ReturnFrameResult.DispatchException (state, constructing, constructed.ConcreteType)
            else

            // Assumption: a constructor can't also return a value.
            // If we were constructing a reference type, we push a reference to it.
            // Otherwise, extract the now-complete object from the heap and push it to the stack directly.
            let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

            let ty =
                AllConcreteTypes.lookup constructed.ConcreteType state.ConcreteTypes
                |> Option.get

            let ty' =
                state.LoadedAssembly (ty.Assembly)
                |> Option.get
                |> fun a -> a.TypeDefs.[ty.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty' then
                state
                // TODO: ordering of fields probably important
                |> pushToEvalStack (CliType.ValueType constructed.Contents) currentThread
            else
                state |> pushToEvalStack (CliType.ofManagedObject constructing) currentThread
            |> ReturnFrameResult.NormalReturn
        | None ->
            let retType = returningMethodState.ExecutingMethod.Signature.ReturnType

            match retType, returningMethodState.EvaluationStack.Values with
            | MethodReturnType.Void, [] -> state
            | MethodReturnType.Void, _ ->
                failwith
                    $"Invalid CIL: void method %s{returningMethodState.ExecutingMethod.Name} returned with a non-empty evaluation stack"
            | MethodReturnType.Returns _, [] ->
                failwith
                    $"Invalid CIL: non-void method %s{returningMethodState.ExecutingMethod.Name} returned with an empty evaluation stack"
            | MethodReturnType.Returns retType, [ retVal ] ->
                let zero, state =
                    IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes retType

                let toPush = EvalStackValue.toCliTypeCoerced zero retVal

                state |> pushToEvalStack toPush currentThread
            | MethodReturnType.Returns _, _ ->
                failwith
                    $"Invalid CIL: method %s{returningMethodState.ExecutingMethod.Name} returned with more than one evaluation stack value"

            |> ReturnFrameResult.NormalReturn

    let initial
        (lf : ILoggerFactory)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (entryAssembly : DumpedAssembly)
        : IlMachineState
        =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name
        let logger = lf.CreateLogger "IlMachineState"

        let state =
            {
                ConcreteTypes = AllConcreteTypes.Empty
                Logger = logger
                NextThreadId = 0
                // CallStack = []
                ManagedHeap = ManagedHeap.empty
                ThreadState = Map.empty
                InternedStrings = ImmutableDictionary.Empty
                _LoadedAssemblies = ImmutableDictionary.Empty
                _Statics = ImmutableDictionary.Empty
                TypeInitTable = ImmutableDictionary.Empty
                DotnetRuntimeDirs = dotnetRuntimeDirs
                TypeHandles = TypeHandleRegistry.empty ()
                GcHandles = GcHandleRegistry.empty ()
                FieldHandles = FieldHandleRegistry.empty ()
                MethodHandles = MethodHandleRegistry.empty ()
                HardwareIntrinsics = HardwareIntrinsicsProfile.ScalarOnly
                Debugger = DebuggerState.Detached
                RuntimeAssemblyObjects = ImmutableDictionary.Empty
                RuntimeModuleObjects = ImmutableDictionary.Empty
                ManagedThreadObjects = Map.empty
                NextManagedThreadId = 2
                PointerHashCounters = PointerHashCounters.empty
                Kernel = EmulatedKernel.initial
                Scheduling = SchedulerState.RoundRobin
            }

        state.WithLoadedAssembly assyName entryAssembly

    let addThread (newThreadState : MethodState) (state : IlMachineState) : IlMachineState * ThreadId =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState = state.ThreadState |> Map.add thread (ThreadState.New newThreadState)
            }

        newState, thread

    /// Allocate a fresh `ThreadId` for a Thread heap object that the guest has
    /// just constructed (i.e. its `Initialize` ran) but not yet started. The
    /// resulting `ThreadState` is frame-less and has status `NotStarted`; the
    /// scheduler will not pick it until `Thread.StartInternal` populates the
    /// bottom frame and flips status to `Runnable` via `startUnstartedThread`.
    /// Binds the new ThreadId to `threadAddr` in `ManagedThreadObjects` so
    /// helpers like `threadIdFromThreadAddr` can reverse-look-up the thread
    /// during the pre-Start window (notably for the `IsBackground` QCalls).
    let allocateUnstartedThread (threadAddr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState * ThreadId =
        let thread = ThreadId state.NextThreadId

        // Frame-less stub mirroring the test helpers in TestLowLevelMonitor /
        // TestWaitHandle / TestSyncBlockMonitor: `ActiveMethodState` points at
        // a sentinel `FrameId` not present in the empty `MethodStates` map, so
        // any premature attempt to dereference it crashes loudly rather than
        // executing arbitrary IL on an unprepared thread.
        let unstartedState : ThreadState =
            {
                MethodStates = Map.empty
                NextFrameId = 0
                ActiveMethodState = FrameId -1
                Status = ThreadStatus.NotStarted
                IsBackground = false
                Name = None
            }

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState = state.ThreadState |> Map.add thread unstartedState
                ManagedThreadObjects = state.ManagedThreadObjects |> Map.add thread threadAddr
            }

        newState, thread

    /// Allocate a fresh `ThreadId` for a PawPrint-internal auxiliary
    /// thread (currently the signal dispatcher spawned by
    /// `SystemNative_InitializeTerminalAndSignalHandling`). The thread
    /// has no managed `Thread` heap mirror — it is not entered in
    /// `ManagedThreadObjects` — and its `ThreadState` is frameless with
    /// status `ThreadStatus.Parked`, so the scheduler never picks it.
    /// `ActiveMethodState` points at a sentinel `FrameId` that is not
    /// live in the empty `MethodStates` map, mirroring the
    /// `allocateUnstartedThread` shape: any attempt to dereference it
    /// crashes loudly rather than executing arbitrary IL on an
    /// unprepared thread.
    ///
    /// A future slice that wires signal dispatch will introduce an
    /// explicit transition out of `Parked` (driven by the signal
    /// subsystem, not by guest IL); until then a thread allocated here
    /// remains `Parked` for the lifetime of the run.
    let allocateParkedThread (state : IlMachineState) : IlMachineState * ThreadId =
        let thread = ThreadId state.NextThreadId

        let parkedState : ThreadState =
            {
                MethodStates = Map.empty
                NextFrameId = 0
                ActiveMethodState = FrameId -1
                Status = ThreadStatus.Parked
                IsBackground = false
                Name = None
            }

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState = state.ThreadState |> Map.add thread parkedState
            }

        newState, thread

    /// Populate the bottom frame of a `Parked` signal-dispatcher thread
    /// with a handler-invocation method state, and flip its status to
    /// `Runnable`. Symmetric to `startUnstartedThread`, but for the
    /// kernel-owned dispatcher allocated by `allocateParkedThread`:
    /// the thread was sitting frameless with `Status = Parked` and
    /// `ActiveMethodState = FrameId -1`, and the signal-dispatch
    /// subsystem has now decided to wake it onto a handler. Fails loud
    /// if the thread is missing, not in `Parked` status (concurrent
    /// dispatch on the single dispatcher would be a logic error), or
    /// already has live frames (a previous re-park should have cleared
    /// them).
    let startParkedDispatcher
        (thread : ThreadId)
        (newMethodState : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let existing =
            state.ThreadState
            |> Map.tryFind thread
            |> Option.defaultWith (fun () -> failwith $"startParkedDispatcher: thread {thread} has no ThreadState")

        match existing.Status with
        | ThreadStatus.Parked -> ()
        | other ->
            failwith
                $"startParkedDispatcher: thread {thread} is in status %O{other}, expected Parked. The dispatcher is only ever woken from Parked; finding it in another state indicates either concurrent dispatch (the caller forgot to gate on Parked) or a missed re-park after a previous handler returned."

        if not (Map.isEmpty existing.MethodStates) then
            failwith
                $"startParkedDispatcher: thread {thread} unexpectedly has live frames before dispatch; a prior re-park should have cleared them."

        let started =
            existing
            |> ThreadState.replaceFrames newMethodState
            |> fun ts ->
                { ts with
                    Status = ThreadStatus.Runnable
                }

        { state with
            ThreadState = state.ThreadState |> Map.add thread started
        }

    /// Inverse of `startParkedDispatcher`: a handler frame on the signal
    /// dispatcher has just `ret`urned past its bottom (`Ret` surfaced as
    /// `ExecutionResult.Terminated`, because the bottom frame has no
    /// `ReturnState`). Clear the now-stale `MethodStates`, reset
    /// `ActiveMethodState` to the sentinel `FrameId -1`, and flip
    /// `Runnable -> Parked`, mirroring the shape `allocateParkedThread`
    /// left the thread in originally. Drops the int handler return value
    /// (left on the bottom frame's eval stack) on the floor — its
    /// real-CLR meaning (0 = run default disposition, 1 = consumed) is
    /// not yet modelled. Fails loud if the thread isn't actually the
    /// dispatcher in a post-`ret` shape.
    let reParkDispatcher (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let existing =
            state.ThreadState
            |> Map.tryFind thread
            |> Option.defaultWith (fun () -> failwith $"reParkDispatcher: thread {thread} has no ThreadState")

        match existing.Status with
        | ThreadStatus.Runnable -> ()
        | other ->
            failwith
                $"reParkDispatcher: thread {thread} is in status %O{other}, expected Runnable (a handler frame should have been mid-execution before its bottom `ret`)."

        let parked : ThreadState =
            {
                MethodStates = Map.empty
                NextFrameId = 0
                ActiveMethodState = FrameId -1
                Status = ThreadStatus.Parked
                IsBackground = existing.IsBackground
                Name = existing.Name
            }

        { state with
            ThreadState = state.ThreadState |> Map.add thread parked
        }

    /// Populate the bottom frame of a `NotStarted` thread with the user's
    /// delegate target and flip its status to `Runnable`. The thread was
    /// previously allocated by `allocateUnstartedThread` at `Thread.Initialize`
    /// time. Fails loud if the thread is missing, in a non-`NotStarted`
    /// status (double-Start would be the typical cause; the real CLR raises
    /// `ThreadStateException` here), or already has live frames.
    let startUnstartedThread
        (thread : ThreadId)
        (newMethodState : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        let existing =
            state.ThreadState
            |> Map.tryFind thread
            |> Option.defaultWith (fun () -> failwith $"startUnstartedThread: thread {thread} has no ThreadState")

        match existing.Status with
        | ThreadStatus.NotStarted -> ()
        | other ->
            failwith
                $"startUnstartedThread: thread {thread} is in status %O{other}, expected NotStarted. Most likely cause: double-Start on a Thread object. The real CLR raises ThreadStateException here, which PawPrint does not yet synthesise."

        if not (Map.isEmpty existing.MethodStates) then
            failwith $"startUnstartedThread: thread {thread} unexpectedly has live frames before Start"

        let started =
            existing
            |> ThreadState.replaceFrames newMethodState
            |> fun ts ->
                { ts with
                    Status = ThreadStatus.Runnable
                }

        { state with
            ThreadState = state.ThreadState |> Map.add thread started
        }

    let allocateArray
        (arrayType : ConcreteTypeHandle)
        (zeroOfType : unit -> CliType)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                ConcreteType = arrayType
                Length = len
                Lengths = ImmutableArray.Create len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    /// Allocate a multi-dimensional array of `arrayType` (which should be a
    /// `ConcreteTypeHandle.Array (elementHandle, rank)`), zero-initialised in row-major
    /// layout. Each entry of `dimensionLengths` must be non-negative and the array
    /// must have rank >= 1; multi-dim arrays with non-zero lower bounds are not
    /// representable here (C# never emits them, and ECMA-335 II.14.2 calls them out
    /// as a separate constructor form).
    let allocateMultiDimArray
        (arrayType : ConcreteTypeHandle)
        (zeroOfType : unit -> CliType)
        (dimensionLengths : ImmutableArray<int>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        if dimensionLengths.Length = 0 then
            failwith
                "TODO: cannot allocate multi-dim array with rank 0; this should have been ruled out at the dispatch site"

        // Match CoreCLR's product-overflow rule (vm/gchelpers.cpp `AllocateArrayEx`):
        // accumulate the running product in unsigned 32-bit, throwing OutOfMemoryException
        // only if the multiply itself would overflow UInt32 — *not* on a transient prefix
        // that exceeds Int32.MaxValue but later gets zeroed by a 0 dimension. So
        // `new int[50000, 50000, 0]` allocates an empty array (the prefix 2.5e9 fits in
        // UInt32 and the trailing zero brings the product back to 0), while
        // `new int[65536, 65536, ...]` throws because 65536 * 65536 overflows UInt32 at
        // the multiply step regardless of any later zero. After the loop, the final
        // product must also fit in Int32, since our backing-store length is Int32.
        let mutable totalLength : uint32 = 1u

        for i = 0 to dimensionLengths.Length - 1 do
            let d = dimensionLengths.[i]

            if d < 0 then
                failwith
                    $"TODO: multi-dim array constructor was given a negative length %d{d} at dimension %d{i}; should raise OverflowException"

            let dU = uint32 d
            // Multiplying by zero is always safe; it just zeroes the running product.
            if dU <> 0u && totalLength > System.UInt32.MaxValue / dU then
                failwith
                    $"TODO: multi-dim array running product overflows UInt32 at dimension %d{i}; should raise OutOfMemoryException"

            totalLength <- totalLength * dU

        if totalLength > uint32 System.Int32.MaxValue then
            failwith "TODO: multi-dim array total length exceeds Int32.MaxValue; should raise OutOfMemoryException"

        let totalLength = int totalLength

        let initialisation =
            (fun _ -> zeroOfType ()) |> Seq.init totalLength |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                ConcreteType = arrayType
                Length = totalLength
                Lengths = dimensionLengths
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.setStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject
        (ty : ConcreteTypeHandle)
        (fields : CliValueType)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Contents = fields
                ConcreteType = ty
                SyncBlock = SyncBlock.Empty
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let popFromStackToLocalVariable
        (thread : ThreadId)
        (localVariableIndex : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToVariable localVariableIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let popFromStackToArgument (thread : ThreadId) (argumentIndex : int) (state : IlMachineState) : IlMachineState =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStackToArg argumentIndex threadState.MethodState

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState methodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread threadState
        }

    let jumpProgramCounter (thread : ThreadId) (bytes : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) -> state |> ThreadState.jumpProgramCounter bytes |> Some
                    )
        }

    let loadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some state -> state |> ThreadState.loadArgument index |> Some
                    )
        }

    let getLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (state : IlMachineState)
        : CliType
        =
        (getFrame thread frameId state).LocalVariables.[int<uint16> varIndex]

    let setLocalVariable
        (thread : ThreadId)
        (frameId : FrameId)
        (varIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set variable in nonactive thread"
                        | Some existing -> existing |> ThreadState.setLocalVariable frameId varIndex value |> Some
                    )
        }

    let setArgument
        (thread : ThreadId)
        (frameId : FrameId)
        (argIndex : uint16)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun existing ->
                        match existing with
                        | None -> failwith "tried to set argument in nonactive thread"
                        | Some existing -> existing |> ThreadState.setArgument frameId argIndex value |> Some
                    )
        }

    let allocateStackMemory
        (thread : ThreadId)
        (initialization : MemoryBlockInitialization)
        (byteCount : int)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let threadState = state.ThreadState.[thread]
        let frameId = threadState.ActiveMethodState
        let frame = ThreadState.getFrame frameId threadState

        let blockId, pool =
            StackMemoryPool.allocate initialization byteCount frame.StackMemoryPool

        let frame =
            { frame with
                StackMemoryPool = pool
            }

        let state = setFrame thread frameId frame state

        ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frameId, blockId, 0), []), state

    let getStackMemoryPool (thread : ThreadId) (frameId : FrameId) (state : IlMachineState) : StackMemoryPool =
        (getFrame thread frameId state).StackMemoryPool

    let setStackMemoryPool
        (thread : ThreadId)
        (frameId : FrameId)
        (pool : StackMemoryPool)
        (state : IlMachineState)
        : IlMachineState
        =
        let frame = getFrame thread frameId state

        let frame =
            { frame with
                StackMemoryPool = pool
            }

        setFrame thread frameId frame state

    /// Allocate a block of native heap memory of the given size.  Returned as a
    /// byref into byte 0 of the freshly-allocated block; callers that want a
    /// `nativeint` pointer convert it via `NativeIntSource.ManagedPointer`.
    let allocateNativeMemory
        (initialization : MemoryBlockInitialization)
        (byteCount : int)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let blockId, pool =
            NativeMemoryPool.allocate initialization byteCount state.Kernel.NativeMemoryPool

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    NativeMemoryPool = pool
                }
            )

        ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []), state

    /// Free a previously-allocated native-heap block. Throws on double free.
    /// Use-after-free is caught later by `NativeMemoryPool.getBlock` when any
    /// retained byref into the block is dereferenced.
    let freeNativeMemory (blockId : NativeMemoryBlockId) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            { kernel with
                NativeMemoryPool = NativeMemoryPool.free blockId kernel.NativeMemoryPool
            }
        )

    let getNativeMemoryPool (state : IlMachineState) : NativeMemoryPool = state.Kernel.NativeMemoryPool

    let setNativeMemoryPool (pool : NativeMemoryPool) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            { kernel with
                NativeMemoryPool = pool
            }
        )

    let setSyncBlock
        (addr : ManagedHeapAddress)
        (syncBlockValue : SyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = state.ManagedHeap |> ManagedHeap.setSyncBlock addr syncBlockValue
        }

    let getSyncBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        state.ManagedHeap |> ManagedHeap.getSyncBlock addr
