namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Property-based and example-based tests for the deterministic
/// `SyncBlockMonitor` state machine that backs the managed
/// `Monitor.Wait` / `Monitor.Pulse` / `Monitor.PulseAll` QCalls.
///
/// SyncBlockMonitor has no Enter/Exit surface of its own — those live in
/// `ExternImplementations/System.Threading.Monitor.fs` as `TryEnter_FastPath` /
/// `Exit_FastPath` because they are tied to QCall dispatch. We therefore set
/// up `SyncBlockLock.Held` preconditions directly via `setSyncBlock` and
/// exercise the `wait`/`pulse`/`pulseAll`/`spuriousWake`/`applySpuriousWakeups`
/// transitions in isolation, with a stub thread state (no method frames).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSyncBlockMonitor =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// ThreadState placeholder: monitor transitions only touch `Status`,
    /// so a frame-less stub is sufficient. `ActiveMethodState` is set to
    /// a sentinel FrameId that does not appear in the empty MethodStates
    /// map; any code path that tries to dereference it would crash the
    /// test loudly, which is the correct response if the monitor module
    /// ever started reaching for frames.
    let private stubThreadState (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            Name = None
        }

    let private withThreads (threads : ThreadId list) (state : IlMachineState) : IlMachineState =
        let threadMap =
            threads
            |> List.map (fun tid -> tid, stubThreadState ThreadStatus.Runnable)
            |> Map.ofList

        { state with
            ThreadState = threadMap
        }

    let private statusOf (thread : ThreadId) (state : IlMachineState) : ThreadStatus = state.ThreadState.[thread].Status

    let private syncBlockOf (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        IlMachineState.getSyncBlock addr state

    /// Allocate a placeholder non-array heap object whose `Contents` and
    /// `ConcreteType` are never touched by `SyncBlockMonitor`. We construct
    /// them as `Unchecked.defaultof<_>` because spinning up a real
    /// `CliValueType` requires `BaseClassTypes` plumbing that is irrelevant
    /// to the transitions under test. If a future SyncBlockMonitor change
    /// starts reading either field, those tests will fail loudly with an
    /// NRE rather than silently masking a bug.
    let private allocateHeapObject (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let stub : AllocatedNonArrayObject =
            {
                Contents = Unchecked.defaultof<CliValueType>
                ConcreteType = ConcreteTypeHandle.Concrete 0
            }

        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray stub

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    /// The array counterpart of `allocateHeapObject`. Arrays carry an object header
    /// just like any other heap object, so every `SyncBlockMonitor` transition must
    /// behave identically whichever kind of address it is handed; the tests below
    /// exercise both.
    let private allocateHeapArray (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let stub : AllocatedArray =
            {
                ConcreteType = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 0)
                Length = 0
                Lengths = ImmutableArray.Create 0
                Elements = ImmutableArray.Empty
            }

        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateArray stub

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    /// Splice the SyncBlock at `addr` into `Lock = Held(thread, depth, queue)`
    /// regardless of its current state, and flip `thread`'s status to
    /// Runnable. This is the only way to mint the precondition for `wait` /
    /// `pulse` / `pulseAll` because `SyncBlockMonitor` exposes no Enter
    /// primitive — those live in the QCall layer.
    let private forceHeld
        (thread : ThreadId)
        (depth : int)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState
        =
        let block = syncBlockOf addr state

        let acquireQueue =
            match block.Lock with
            | SyncBlockLock.Free -> []
            | SyncBlockLock.Held l -> l.AcquireQueue

        let held : LockedSyncBlock =
            {
                LockingThread = thread
                ReentrancyCount = depth
                AcquireQueue = acquireQueue
            }

        let block =
            { block with
                Lock = SyncBlockLock.Held held
            }

        state
        |> IlMachineState.setSyncBlock addr block
        |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

    /// Drive `thread` through "acquire then wait" at the given reentrancy
    /// depth, leaving it in the SyncBlock's `WaitQueue` carrying that
    /// snapshot. Callers that want multiple parked waiters must sequence
    /// these calls so each `wait` observes the lock as currently owned by
    /// `thread`.
    let private parkInWaitAtDepth
        (thread : ThreadId)
        (depth : int)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState
        =
        let state = forceHeld thread depth addr state
        SyncBlockMonitor.wait thread addr None state

    let private parkInWait (thread : ThreadId) (addr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState =
        parkInWaitAtDepth thread 1 addr state

    let private t0 = ThreadId 0
    let private t1 = ThreadId 1
    let private t2 = ThreadId 2
    let private t3 = ThreadId 3

    // ----- Real-frame test scaffolding -----
    //
    // `fireTimeout` rewrites the parked thread's top-of-eval-stack
    // (`Int32 1 → Int32 0`), so any test that drives `fireTimeout` needs
    // a thread with a live MethodState — the bare `stubThreadState`
    // above is intentionally unusable for eval-stack mechanics.
    //
    // We borrow the pattern from `TestLowLevelMonitor`/`TestNullaryIlOp`:
    // build a real frame backed by `System.Object::ToString`, since any
    // concretized method is sufficient (the IL body is never executed by
    // these tests; only the EvaluationStack is observed).

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private baseStateWithFrames () : IlMachineState =
        { baseState () with
            ConcreteTypes = concreteTypes
        }

    let private mintFrame (state : IlMachineState) : IlMachineState * MethodState =
        let _, loggerFactory = LoggerFactory.makeTest ()

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

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il (MethodInstructions.onlyRet ())) signature

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
            | Ok ms -> ms
            | Error missing -> failwith $"Unexpected missing assembly references constructing test frame: %O{missing}"

        state, methodState

    /// Install a real `ThreadState` (single frame, status Runnable) for each
    /// thread id. Each thread gets its own freshly constructed frame so that
    /// eval-stack operations on one thread do not visibly bleed into another.
    let private withRealThreads (threads : ThreadId list) (state : IlMachineState) : IlMachineState =
        let state, threadMap =
            threads
            |> List.fold
                (fun (state : IlMachineState, acc : Map<ThreadId, ThreadState>) (tid : ThreadId) ->
                    let state, methodState = mintFrame state
                    state, acc |> Map.add tid (ThreadState.New methodState)
                )
                (state, Map.empty)

        { state with
            ThreadState = threadMap
        }

    let private topOfStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue =
        match IlMachineState.peekEvalStack thread state with
        | Some v -> v
        | None -> failwith $"thread %O{thread} has empty eval stack"

    // -------------------------------------------------------------------
    // Wait — basic shape
    // -------------------------------------------------------------------

    [<Test>]
    let ``wait releases the lock and parks the caller in WaitQueue with snapshot depth`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let state = SyncBlockMonitor.wait t0 addr None state

        let block = syncBlockOf addr state
        block.Lock |> shouldEqual SyncBlockLock.Free
        block.WaitQueue |> shouldEqual [ t0, 1 ]

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, None))

    [<Test>]
    let ``wait snapshots the caller's reentrancy depth verbatim`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 7 addr state

        let state = SyncBlockMonitor.wait t0 addr None state

        // The whole point of carrying the snapshot is that the depth restored
        // on resume matches the depth the caller had at the Wait call site.
        (syncBlockOf addr state).WaitQueue |> shouldEqual [ t0, 7 ]

    [<Test>]
    let ``wait with non-empty AcquireQueue transfers ownership to the FIFO head`` () : unit =
        // t0 owns; t1 already parked in AcquireQueue waiting for ownership;
        // t2 parked behind t1. t0 calls Wait: ownership flips directly to t1
        // (at the snapshot or fresh depth recorded on its AcquireQueue entry),
        // and t0 parks on the WaitQueue.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let addr, state = allocateHeapObject state

        let held : LockedSyncBlock =
            {
                LockingThread = t0
                ReentrancyCount = 1
                AcquireQueue = [ (t1, None) ; (t2, None) ]
            }

        let state =
            state
            |> IlMachineState.setSyncBlock
                addr
                {
                    Lock = SyncBlockLock.Held held
                    WaitQueue = []
                }
            |> Scheduler.setThreadStatus t1 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))
            |> Scheduler.setThreadStatus t2 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        let state = SyncBlockMonitor.wait t0 addr None state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t1
            // Fresh entrant restores to depth 1.
            l.ReentrancyCount |> shouldEqual 1
            l.AcquireQueue |> shouldEqual [ (t2, None) ]
        | SyncBlockLock.Free -> failwith "expected Held after ownership transfer"

        block.WaitQueue |> shouldEqual [ t0, 1 ]
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

        statusOf t2 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, None))

    [<Test>]
    let ``wait restores reentrancy depth on FIFO ownership transfer from snapshot`` () : unit =
        // The AcquireQueue head was queued by a previous Wait at depth 5;
        // when the current owner releases via Wait, the snapshot must come
        // back as the new owner's ReentrancyCount — depth 1 (fresh) would
        // make the IL after Wait observe the wrong nesting.
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state

        let held : LockedSyncBlock =
            {
                LockingThread = t0
                ReentrancyCount = 1
                AcquireQueue = [ (t1, Some 5) ]
            }

        let state =
            state
            |> IlMachineState.setSyncBlock
                addr
                {
                    Lock = SyncBlockLock.Held held
                    WaitQueue = []
                }
            |> Scheduler.setThreadStatus t1 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        let state = SyncBlockMonitor.wait t0 addr None state

        match (syncBlockOf addr state).Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t1
            l.ReentrancyCount |> shouldEqual 5
        | SyncBlockLock.Free -> failwith "expected Held after ownership transfer"

    [<Test>]
    let ``wait by non-owner fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.wait t1 addr None state |> ignore)

        exn.Message |> shouldContainText "does not own"

    [<Test>]
    let ``wait on Free SyncBlock fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.wait t0 addr None state |> ignore)

        exn.Message |> shouldContainText "Free"

    // -------------------------------------------------------------------
    // Pulse / PulseAll — basic shape
    // -------------------------------------------------------------------

    [<Test>]
    let ``pulse on empty wait queue is a no-op`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let before = syncBlockOf addr state
        let state' = SyncBlockMonitor.pulse t0 addr state
        let after = syncBlockOf addr state'

        // Lock unchanged; WaitQueue still empty; status unchanged.
        after |> shouldEqual before
        statusOf t0 state' |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``pulse moves a single waiter to the AcquireQueue tail and keeps owner`` () : unit =
        // t1 parked in the WaitQueue at depth 3. t0 owns and calls Pulse.
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t1 3 addr state
        // t1 is now in WaitQueue; lock is Free. Hand the lock to t0.
        let state = forceHeld t0 1 addr state

        let state = SyncBlockMonitor.pulse t0 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            l.ReentrancyCount |> shouldEqual 1
            // Snapshot from the WaitQueue entry survives onto the AcquireQueue.
            l.AcquireQueue |> shouldEqual [ (t1, Some 3) ]
        | SyncBlockLock.Free -> failwith "Pulse must not release the lock"

        block.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

        statusOf t1 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

    [<Test>]
    let ``pulse appends to existing AcquireQueue tail (FIFO)`` () : unit =
        // Existing fresh acquirer (t2) is queued behind the owner; pulse
        // moves the waiter (t1) onto the AcquireQueue *tail*, behind t2.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t1 2 addr state

        let held : LockedSyncBlock =
            {
                LockingThread = t0
                ReentrancyCount = 1
                AcquireQueue = [ (t2, None) ]
            }

        let block = syncBlockOf addr state

        let state =
            state
            |> IlMachineState.setSyncBlock
                addr
                { block with
                    Lock = SyncBlockLock.Held held
                }
            |> Scheduler.setThreadStatus t0 ThreadStatus.Runnable
            |> Scheduler.setThreadStatus t2 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        let state = SyncBlockMonitor.pulse t0 addr state

        match (syncBlockOf addr state).Lock with
        | SyncBlockLock.Held l -> l.AcquireQueue |> shouldEqual [ (t2, None) ; (t1, Some 2) ]
        | SyncBlockLock.Free -> failwith "Pulse must not release the lock"

    [<Test>]
    let ``pulseAll drains the WaitQueue onto AcquireQueue preserving FIFO and snapshots`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t1 11 addr state
        let state = parkInWaitAtDepth t2 22 addr state
        let state = parkInWaitAtDepth t3 33 addr state
        let state = forceHeld t0 1 addr state

        let state = SyncBlockMonitor.pulseAll t0 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            l.AcquireQueue |> shouldEqual [ (t1, Some 11) ; (t2, Some 22) ; (t3, Some 33) ]
        | SyncBlockLock.Free -> failwith "PulseAll must not release the lock"

        block.WaitQueue |> shouldEqual []

        statusOf t1 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        statusOf t2 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        statusOf t3 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

    [<Test>]
    let ``pulseAll on empty wait queue is a no-op`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let before = syncBlockOf addr state
        let state' = SyncBlockMonitor.pulseAll t0 addr state
        let after = syncBlockOf addr state'

        after |> shouldEqual before

    [<Test>]
    let ``pulse by non-owner fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.pulse t1 addr state |> ignore)

        exn.Message |> shouldContainText "does not own"

    [<Test>]
    let ``pulse on Free SyncBlock fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.pulse t0 addr state |> ignore)

        exn.Message |> shouldContainText "Free"

    [<Test>]
    let ``pulseAll by non-owner fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.pulseAll t1 addr state |> ignore)

        exn.Message |> shouldContainText "does not own"

    [<Test>]
    let ``pulseAll on Free SyncBlock fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.pulseAll t0 addr state |> ignore)

        exn.Message |> shouldContainText "Free"

    // -------------------------------------------------------------------
    // Spurious wakeups — single-transition behaviour
    // -------------------------------------------------------------------

    [<Test>]
    let ``spuriousWake on Free SyncBlock grants ownership at snapshot depth`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 4 addr state

        let state = SyncBlockMonitor.spuriousWake addr t0 state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            // Snapshot must be restored verbatim — depth 1 would silently
            // collapse nested-lock IL after Wait.
            l.ReentrancyCount |> shouldEqual 4
            l.AcquireQueue |> shouldEqual []
        | SyncBlockLock.Free -> failwith "spuriousWake on Free must take ownership"

        block.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``spuriousWake on Held SyncBlock parks waiter at AcquireQueue tail with snapshot`` () : unit =
        // t0 parks in Wait at depth 9; t1 then synthetically owns; spuriously
        // waking t0 must queue it behind t1 carrying Some 9 so the eventual
        // ownership transfer restores depth 9.
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 9 addr state
        let state = forceHeld t1 1 addr state

        let state = SyncBlockMonitor.spuriousWake addr t0 state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t1
            l.AcquireQueue |> shouldEqual [ (t0, Some 9) ]
        | SyncBlockLock.Free -> failwith "Held lock must remain Held"

        block.WaitQueue |> shouldEqual []

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``spuriousWake fails loud when thread is not in the WaitQueue`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.spuriousWake addr t0 state |> ignore)

        exn.Message |> shouldContainText "WaitQueue"

    // -------------------------------------------------------------------
    // Allocation kind is irrelevant to monitors
    //
    // An array is a heap object with an object header like any other (CoreCLR:
    // `ArrayBase` derives from `Object`, and `EnterObjMonitor`/`Wait`/`Pulse`
    // are defined once on `Object`). So the whole transition system must be
    // blind to whether an address names an array or a non-array object. Rather
    // than duplicating every case above, run the same scripts against both
    // allocation kinds and assert the observable outcomes coincide.
    // -------------------------------------------------------------------

    /// Every transition `SyncBlockMonitor` exposes that does not need real method
    /// frames, expressed as a script we can replay against any allocation kind.
    let private allTransitionScripts : (string * (ManagedHeapAddress -> IlMachineState -> IlMachineState)) list =
        [
            "wait", fun addr state -> state |> forceHeld t0 3 addr |> SyncBlockMonitor.wait t0 addr None

            "wait then pulse",
            fun addr state ->
                state
                |> parkInWaitAtDepth t0 5 addr
                |> forceHeld t1 1 addr
                |> SyncBlockMonitor.pulse t1 addr

            "wait twice then pulseAll",
            fun addr state ->
                state
                |> parkInWaitAtDepth t0 2 addr
                |> parkInWaitAtDepth t1 4 addr
                |> forceHeld t2 1 addr
                |> SyncBlockMonitor.pulseAll t2 addr

            "wait then spuriousWake",
            fun addr state -> state |> parkInWaitAtDepth t0 6 addr |> SyncBlockMonitor.spuriousWake addr t0

            "wait then applySpuriousWakeups AlwaysAll",
            fun addr state ->
                state
                |> parkInWaitAtDepth t0 1 addr
                |> parkInWaitAtDepth t1 2 addr
                |> SyncBlockMonitor.applySpuriousWakeups SyncBlockSpuriousWakeupStrategy.AlwaysAll 0L
        ]

    [<Test>]
    let ``monitor transitions are identical for array and non-array targets`` () : unit =
        let run
            (script : ManagedHeapAddress -> IlMachineState -> IlMachineState)
            (allocate : IlMachineState -> ManagedHeapAddress * IlMachineState)
            =
            let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
            let addr, state = allocate state
            let state = script addr state
            syncBlockOf addr state, (state.ThreadState |> Map.map (fun _ ts -> ts.Status))

        for name, script in allTransitionScripts do
            // Both allocators hand out address 1 from a fresh heap, so the statuses —
            // which embed the address — are directly comparable.
            let objBlock, objStatuses = run script allocateHeapObject
            let arrBlock, arrStatuses = run script allocateHeapArray

            if arrBlock <> objBlock then
                failwith
                    $"script %s{name}: array target produced SyncBlock %A{arrBlock} but non-array target produced %A{objBlock}"

            if arrStatuses <> objStatuses then
                failwith
                    $"script %s{name}: array target produced thread statuses %A{arrStatuses} but non-array target produced %A{objStatuses}"

    [<Test>]
    let ``a freshly allocated array is an unlocked monitor target`` () : unit =
        let state = baseState ()
        let addr, state = allocateHeapArray state

        syncBlockOf addr state |> shouldEqual SyncBlock.Empty

    // -------------------------------------------------------------------
    // applySpuriousWakeups — strategy interpretation
    // -------------------------------------------------------------------

    /// Snapshot of just the parts of `IlMachineState` that
    /// `applySpuriousWakeups` is allowed to touch. ThreadState as a whole
    /// is not equality-comparable (it embeds MethodStates with structural
    /// non-equality), so we compare what we care about explicitly.
    let private wakeupVisibleState (state : IlMachineState) =
        let blocks = state.ManagedHeap.SyncBlocks

        let statuses = state.ThreadState |> Map.map (fun _ ts -> ts.Status)
        blocks, statuses

    [<Test>]
    let ``applySpuriousWakeups Disabled is bit-identical to the input state`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state
        let state = parkInWaitAtDepth t1 2 addr state

        let state' =
            SyncBlockMonitor.applySpuriousWakeups SyncBlockSpuriousWakeupStrategy.Disabled 42L state

        wakeupVisibleState state' |> shouldEqual (wakeupVisibleState state)

    [<Test>]
    let ``applySpuriousWakeups AlwaysAll drains WaitQueue in FIFO order with snapshots restored`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 7 addr state
        let state = parkInWaitAtDepth t1 8 addr state
        let state = parkInWaitAtDepth t2 9 addr state

        let state =
            SyncBlockMonitor.applySpuriousWakeups SyncBlockSpuriousWakeupStrategy.AlwaysAll 0L state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            // FIFO: t0 wakes first into a Free lock and takes ownership at
            // its snapshot depth; t1 and t2 then queue behind t0 carrying
            // their own snapshots.
            l.LockingThread |> shouldEqual t0
            l.ReentrancyCount |> shouldEqual 7
            l.AcquireQueue |> shouldEqual [ (t1, Some 8) ; (t2, Some 9) ]
        | SyncBlockLock.Free -> failwith "AlwaysAll must wake t0 into ownership"

        block.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

        statusOf t1 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        statusOf t2 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

    [<Test>]
    let ``applySpuriousWakeups AlwaysAll processes objects in ascending address order`` () : unit =
        // Two independent SyncBlocks, allocate so addr1 < addr2; AlwaysAll
        // sees both queues but must process addr1 before addr2 (FIFO across
        // objects). The order is observable via which thread takes
        // ownership of each.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let addr1, state = allocateHeapObject state
        let addr2, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr1 state
        let state = parkInWaitAtDepth t1 2 addr1 state
        let state = parkInWaitAtDepth t2 3 addr2 state
        let state = parkInWaitAtDepth t3 4 addr2 state

        let state =
            SyncBlockMonitor.applySpuriousWakeups SyncBlockSpuriousWakeupStrategy.AlwaysAll 0L state

        let b1 = syncBlockOf addr1 state
        let b2 = syncBlockOf addr2 state

        match b1.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            l.AcquireQueue |> shouldEqual [ (t1, Some 2) ]
        | SyncBlockLock.Free -> failwith "expected b1 owned by t0"

        match b2.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t2
            l.AcquireQueue |> shouldEqual [ (t3, Some 4) ]
        | SyncBlockLock.Free -> failwith "expected b2 owned by t2"

        b1.WaitQueue |> shouldEqual []
        b2.WaitQueue |> shouldEqual []

    [<Test>]
    let ``applySpuriousWakeups Scripted only fires at the named tick`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state
        let state = parkInWaitAtDepth t1 2 addr state

        let script = SyncBlockSpuriousWakeupStrategy.Scripted [ 5L, addr, t1 ]

        // At tick 0: nothing scripted; state passes through.
        let state0 = SyncBlockMonitor.applySpuriousWakeups script 0L state
        (syncBlockOf addr state0).WaitQueue |> shouldEqual [ t0, 1 ; t1, 2 ]

        // At tick 5: only t1 wakes (and takes ownership because lock is
        // free); t0 remains parked.
        let state5 = SyncBlockMonitor.applySpuriousWakeups script 5L state
        let block = syncBlockOf addr state5

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t1
            l.ReentrancyCount |> shouldEqual 2
        | SyncBlockLock.Free -> failwith "Scripted wake must take ownership of Free lock"

        block.WaitQueue |> shouldEqual [ t0, 1 ]
        statusOf t1 state5 |> shouldEqual ThreadStatus.Runnable

        statusOf t0 state5
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, None))

    [<Test>]
    let ``applySpuriousWakeups Scripted fails loud on a stale waiter`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let script = SyncBlockSpuriousWakeupStrategy.Scripted [ 0L, addr, t0 ]

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.applySpuriousWakeups script 0L state |> ignore)

        exn.Message |> shouldContainText "WaitQueue"

    [<Test>]
    let ``applySpuriousWakeups Random with probability 0.0 is a no-op`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state
        let state = parkInWaitAtDepth t1 2 addr state

        let state' =
            SyncBlockMonitor.applySpuriousWakeups (SyncBlockSpuriousWakeupStrategy.Random (42UL, 0.0)) 7L state

        wakeupVisibleState state' |> shouldEqual (wakeupVisibleState state)

    [<Test>]
    let ``applySpuriousWakeups Random with probability 1.0 matches AlwaysAll`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state
        let state = parkInWaitAtDepth t1 2 addr state
        let state = parkInWaitAtDepth t2 3 addr state

        let always =
            SyncBlockMonitor.applySpuriousWakeups SyncBlockSpuriousWakeupStrategy.AlwaysAll 0L state

        let random =
            SyncBlockMonitor.applySpuriousWakeups (SyncBlockSpuriousWakeupStrategy.Random (0UL, 1.0)) 0L state

        wakeupVisibleState random |> shouldEqual (wakeupVisibleState always)

    [<Test>]
    let ``applySpuriousWakeups Random is deterministic in (seed, tick)`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state
        let state = parkInWaitAtDepth t1 2 addr state
        let state = parkInWaitAtDepth t2 3 addr state
        let state = parkInWaitAtDepth t3 4 addr state

        let strategy = SyncBlockSpuriousWakeupStrategy.Random (123UL, 0.5)

        let r1 = SyncBlockMonitor.applySpuriousWakeups strategy 17L state
        let r2 = SyncBlockMonitor.applySpuriousWakeups strategy 17L state

        wakeupVisibleState r1 |> shouldEqual (wakeupVisibleState r2)

    [<Test>]
    let ``applySpuriousWakeups Random rejects NaN probability`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                SyncBlockMonitor.applySpuriousWakeups
                    (SyncBlockSpuriousWakeupStrategy.Random (1UL, System.Double.NaN))
                    0L
                    state
                |> ignore
            )

        exn.Message |> shouldContainText "NaN"

    [<Test>]
    let ``applySpuriousWakeups Random rejects probability above 1.0`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                SyncBlockMonitor.applySpuriousWakeups (SyncBlockSpuriousWakeupStrategy.Random (1UL, 1.5)) 0L state
                |> ignore
            )

        exn.Message |> shouldContainText "outside"

    [<Test>]
    let ``applySpuriousWakeups Random rejects negative probability`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = parkInWaitAtDepth t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                SyncBlockMonitor.applySpuriousWakeups (SyncBlockSpuriousWakeupStrategy.Random (1UL, -0.1)) 0L state
                |> ignore
            )

        exn.Message |> shouldContainText "outside"

    // -------------------------------------------------------------------
    // Property tests
    // -------------------------------------------------------------------

    /// FIFO transfer oracle. Park `n` waiters via Wait, then PulseAll into a
    /// fresh-owner Held lock. The resulting AcquireQueue must equal the
    /// original WaitQueue in order, depths and all. This is the load-bearing
    /// property for guest fairness across `Monitor.PulseAll` — any reordering
    /// changes the interleaving that wakes resume in.
    [<Test>]
    let ``Property: PulseAll preserves FIFO order and snapshot depths of WaitQueue`` () : unit =
        let property (PositiveInt n) : bool =
            let n = min 8 n
            let waiters = [ 0 .. n - 1 ] |> List.map ThreadId
            let owner = ThreadId 100
            let allThreads = owner :: waiters

            let state = baseState () |> withThreads allThreads
            let addr, state = allocateHeapObject state

            // Park each waiter at a distinct snapshot depth so we can
            // observe both order AND payload preservation.
            let state =
                waiters
                |> List.mapi (fun i tid -> tid, i + 2) // depth >= 2 so no collision with fresh-entrant depth 1
                |> List.fold (fun s (tid, d) -> parkInWaitAtDepth tid d addr s) state

            let expectedQueue = waiters |> List.mapi (fun i tid -> tid, Some (i + 2))

            let state = forceHeld owner 1 addr state
            let state = SyncBlockMonitor.pulseAll owner addr state

            let block = syncBlockOf addr state

            match block.Lock with
            | SyncBlockLock.Held l ->
                l.LockingThread = owner
                && l.AcquireQueue = expectedQueue
                && block.WaitQueue = []
                && waiters
                   |> List.forall (fun tid -> statusOf tid state = ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))
            | SyncBlockLock.Free -> false

        Check.One (config, property)

    /// Symmetry oracle: pulse-then-exit drains the wait queue back to
    /// the resumed-waiter state. Concretely: park `n` waiters; an owner
    /// PulseAlls; the owner then "exits" by synthetically advancing the
    /// AcquireQueue head into ownership. After `n` such transfers the lock
    /// is owned by the original last waiter, AcquireQueue is empty,
    /// WaitQueue is empty, and every previously-parked thread either owns
    /// the lock or is queued behind in the canonical FIFO order.
    [<Test>]
    let ``Property: pulseAll then n ownership transfers drains to the last waiter`` () : unit =
        let property (PositiveInt n) : bool =
            let n = min 6 n
            let waiters = [ 0 .. n - 1 ] |> List.map ThreadId
            let owner = ThreadId 100

            let state = baseState () |> withThreads (owner :: waiters)
            let addr, state = allocateHeapObject state

            let state =
                waiters
                |> List.mapi (fun i tid -> tid, i + 2)
                |> List.fold (fun s (tid, d) -> parkInWaitAtDepth tid d addr s) state

            let state = forceHeld owner 1 addr state
            let state = SyncBlockMonitor.pulseAll owner addr state

            // Synthetically advance ownership through the queue n times.
            // Each step pops the AcquireQueue head and makes it the owner
            // at its snapshot depth. We don't go through `wait` here
            // because we're testing the post-pulseAll structure, not the
            // wait transition itself.
            let mutable st = state

            for _ in 1..n do
                let block = syncBlockOf addr st

                match block.Lock with
                | SyncBlockLock.Held l ->
                    match l.AcquireQueue with
                    | [] -> failwith "AcquireQueue drained early"
                    | (next, snap) :: rest ->
                        let depth = snap |> Option.defaultValue 1

                        let held' : LockedSyncBlock =
                            {
                                LockingThread = next
                                ReentrancyCount = depth
                                AcquireQueue = rest
                            }

                        st <-
                            st
                            |> IlMachineState.setSyncBlock
                                addr
                                { block with
                                    Lock = SyncBlockLock.Held held'
                                }
                            |> Scheduler.setThreadStatus next ThreadStatus.Runnable
                | SyncBlockLock.Free -> failwith "lock became Free mid-drain"

            let final = syncBlockOf addr st

            match final.Lock with
            | SyncBlockLock.Held l ->
                l.LockingThread = List.last waiters
                && l.ReentrancyCount = (n - 1) + 2
                && l.AcquireQueue = []
                && final.WaitQueue = []
            | SyncBlockLock.Free -> false

        Check.One (config, property)

    /// Invariants-preserved oracle: AlwaysAll wakes every waiter, and the
    /// final SyncBlock must satisfy:
    ///   1. WaitQueue is empty.
    ///   2. The directional invariant: Owner = None implies AcquireQueue = []
    ///      (the forbidden state is parked acquirers with no owner).
    ///   3. The multiset of (Owner ++ AcquireQueue threads) equals the
    ///      original WaitQueue threads — no thread lost or duplicated.
    ///   4. Each woken thread's restored depth equals its original snapshot
    ///      (snapshots survive AlwaysAll verbatim).
    [<Test>]
    let ``Property: AlwaysAll preserves invariants and snapshot depths`` () : unit =
        let property (PositiveInt n) : bool =
            let n = min 6 n
            let waiters = [ 0 .. n - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads waiters
            let addr, state = allocateHeapObject state

            let depths = waiters |> List.mapi (fun i tid -> tid, i + 3)

            let state =
                depths |> List.fold (fun s (tid, d) -> parkInWaitAtDepth tid d addr s) state

            let originalWaiters = (syncBlockOf addr state).WaitQueue

            let state =
                SyncBlockMonitor.applySpuriousWakeups SyncBlockSpuriousWakeupStrategy.AlwaysAll 0L state

            let block = syncBlockOf addr state

            let owners =
                match block.Lock with
                | SyncBlockLock.Free -> []
                | SyncBlockLock.Held l -> [ l.LockingThread ]

            let acquireQueueThreads =
                match block.Lock with
                | SyncBlockLock.Free -> []
                | SyncBlockLock.Held l -> l.AcquireQueue |> List.map fst

            // The directional invariant: AcquireQueue must be empty when
            // Lock is Free. With non-empty parked acquirers and no owner
            // the scheduler would have no one to transfer to.
            let invariantHolds =
                match block.Lock with
                | SyncBlockLock.Free -> List.isEmpty acquireQueueThreads
                | SyncBlockLock.Held _ -> true

            let allRecovered = owners @ acquireQueueThreads
            let originalSet = originalWaiters |> List.map fst |> Set.ofList
            let recoveredSet = allRecovered |> Set.ofList

            // Depth check: for each woken thread, find its restored depth
            // (either as Owner.ReentrancyCount or as Some d in the
            // AcquireQueue) and compare to its snapshot.
            let restoredDepth =
                match block.Lock with
                | SyncBlockLock.Free -> Map.empty
                | SyncBlockLock.Held l ->
                    let inAcq =
                        l.AcquireQueue
                        |> List.choose (fun (t, d) -> d |> Option.map (fun d -> t, d))
                        |> Map.ofList

                    Map.add l.LockingThread l.ReentrancyCount inAcq

            let snapshotsPreserved =
                originalWaiters
                |> List.forall (fun (t, snap) ->
                    match Map.tryFind t restoredDepth with
                    | Some d -> d = snap
                    | None -> false
                )

            block.WaitQueue = []
            && invariantHolds
            && originalSet = recoveredSet
            && snapshotsPreserved

        Check.One (config, property)

    // -------------------------------------------------------------------
    // wait deadline plumbing
    //
    // `BlockedOnSyncBlockWait` now carries an optional deadline. The
    // scheduler in `Program.fireExpiredDeadlines` keys off this option to
    // decide whether the thread is a `Monitor.Wait(obj, timeout)` waiter
    // (finite Some) or an untimed `Monitor.Wait(obj)` waiter (None).
    // -------------------------------------------------------------------

    [<Test>]
    let ``wait with no deadline records None on the BlockedOnSyncBlockWait status`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let state = SyncBlockMonitor.wait t0 addr None state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, None))

    [<Test>]
    let ``wait with a finite deadline records Some on the BlockedOnSyncBlockWait status`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let state = SyncBlockMonitor.wait t0 addr (Some 1234L) state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, Some 1234L))

    [<Test>]
    let ``wait deadline is recorded even when the waiter inherits ownership transfer`` () : unit =
        // t0 owns; t1 is parked in AcquireQueue; t0 calls Wait(deadline).
        // Ownership transfers to t1 and t0 joins the WaitQueue carrying its
        // snapshot. The deadline payload must survive the transfer path
        // (which writes the status from the same branch as the no-transfer
        // case).
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state

        let held : LockedSyncBlock =
            {
                LockingThread = t0
                ReentrancyCount = 1
                AcquireQueue = [ (t1, None) ]
            }

        let state =
            state
            |> IlMachineState.setSyncBlock
                addr
                {
                    Lock = SyncBlockLock.Held held
                    WaitQueue = []
                }
            |> Scheduler.setThreadStatus t1 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        let state = SyncBlockMonitor.wait t0 addr (Some 9999L) state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, Some 9999L))

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        (syncBlockOf addr state).WaitQueue |> shouldEqual [ t0, 1 ]

    // -------------------------------------------------------------------
    // fireTimeout
    //
    // These tests need real frames so the eval-stack rewrite
    // (`Int32 1 → Int32 0`) is observable. They simulate the
    // park-time optimistic push by pushing `Int32 1` themselves before
    // calling `fireTimeout`.
    // -------------------------------------------------------------------

    /// Park `thread` on `addr` with the given snapshot depth, a finite
    /// deadline, and the park-time optimistic `Int32 1` already on its
    /// eval stack — mirroring what the `Monitor_Wait` QCall handler does
    /// at the IL boundary.
    let private parkInTimedWaitAtDepth
        (thread : ThreadId)
        (depth : int)
        (addr : ManagedHeapAddress)
        (deadlineMs : int64)
        (state : IlMachineState)
        : IlMachineState
        =
        let state = forceHeld thread depth addr state
        let state = state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) thread
        SyncBlockMonitor.wait thread addr (Some deadlineMs) state

    let private parkInTimedWait
        (thread : ThreadId)
        (addr : ManagedHeapAddress)
        (deadlineMs : int64)
        (state : IlMachineState)
        : IlMachineState
        =
        parkInTimedWaitAtDepth thread 1 addr deadlineMs state

    [<Test>]
    let ``fireTimeout on free SyncBlock takes ownership at snapshot depth and rewrites Int32 1 to Int32 0`` () : unit =
        let state = baseStateWithFrames () |> withRealThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedWaitAtDepth t0 5 addr 100L

        // Sanity: t0 parked at depth 5, lock Free, optimistic 1 on stack.
        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait (addr, Some 100L))

        let block = syncBlockOf addr state
        block.Lock |> shouldEqual SyncBlockLock.Free
        block.WaitQueue |> shouldEqual [ t0, 5 ]
        topOfStack t0 state |> shouldEqual (EvalStackValue.Int32 1)

        let state = SyncBlockMonitor.fireWaitTimeout t0 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            // Depth restored from the snapshot — the resumed IL after Wait
            // must observe the same nesting it had at the call site.
            l.ReentrancyCount |> shouldEqual 5
            l.AcquireQueue |> shouldEqual []
        | SyncBlockLock.Free -> failwith "expected Held after fireTimeout against free lock"

        block.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        // Monitor.Wait returns false (Int32 0) = timed out.
        topOfStack t0 state |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``fireTimeout on held SyncBlock parks at AcquireQueue tail with Some depth and rewrites Int32 1 to Int32 0``
        ()
        : unit
        =
        // t0 timed-waits at depth 3; t1 then takes ownership. Firing t0's
        // timeout must not let t0 steal the lock — it queues behind t1
        // with its snapshot preserved as `Some 3`.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedWaitAtDepth t0 3 addr 100L
        let state = forceHeld t1 1 addr state

        let state = SyncBlockMonitor.fireWaitTimeout t0 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t1
            l.ReentrancyCount |> shouldEqual 1
            l.AcquireQueue |> shouldEqual [ (t0, Some 3) ]
        | SyncBlockLock.Free -> failwith "expected Held after fireTimeout against held lock"

        block.WaitQueue |> shouldEqual []

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        topOfStack t0 state |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``fireTimeout fails loud when the thread is not in WaitQueue`` () : unit =
        let state = baseStateWithFrames () |> withRealThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state
        // t0 owns; it is not in the WaitQueue. Reaching `fireTimeout` for
        // such a thread indicates a structural bug — the deadline must
        // have been observed for a status the SyncBlock does not back.

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.fireWaitTimeout t0 addr state |> ignore)

        exn.Message |> shouldContainText "WaitQueue"

    [<Test>]
    let ``fireTimeout preserves FIFO across multiple AcquireQueue parks`` () : unit =
        // Two contenders already in AcquireQueue; t2 times out while
        // parked in WaitQueue. The timeout-wake must push t2 to the
        // *tail* of AcquireQueue, not the head, so an earlier owner
        // hand-off doesn't accidentally jump t2 over t1.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ; t2 ]
        let addr, state = allocateHeapObject state

        // Force t2 to own at depth 1 with t0 and t1 queued behind.
        let held : LockedSyncBlock =
            {
                LockingThread = t2
                ReentrancyCount = 1
                AcquireQueue = [ (t0, None) ; (t1, None) ]
            }

        let state =
            state
            |> IlMachineState.setSyncBlock
                addr
                {
                    Lock = SyncBlockLock.Held held
                    WaitQueue = []
                }
            |> Scheduler.setThreadStatus t0 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))
            |> Scheduler.setThreadStatus t1 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        // t2 calls Wait(deadline): pushes optimistic 1 then parks; ownership
        // transfers to t0 (FIFO head of AcquireQueue) at fresh depth 1.
        let state = state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) t2
        let state = SyncBlockMonitor.wait t2 addr (Some 5L) state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            l.AcquireQueue |> shouldEqual [ (t1, None) ]
        | SyncBlockLock.Free -> failwith "expected Held after wait transfers ownership"

        block.WaitQueue |> shouldEqual [ t2, 1 ]

        let state = SyncBlockMonitor.fireWaitTimeout t2 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            // t2 lands at the *tail*, behind t1.
            l.AcquireQueue |> shouldEqual [ (t1, None) ; (t2, Some 1) ]
        | SyncBlockLock.Free -> failwith "expected Held after fireTimeout"

        block.WaitQueue |> shouldEqual []

        statusOf t2 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        topOfStack t2 state |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``fireTimeout in WaitQueue head-first order preserves FIFO across same-tick expiries`` () : unit =
        // Pin the contract that `Program.fireExpiredDeadlines` must
        // respect: when two `Monitor.Wait` waiters on the same SyncBlock
        // expire in the same virtual-clock tick, firing them in
        // WaitQueue order gives ownership to the FIFO head. The bug we
        // guard against is iterating threads by ThreadId, which would
        // let a later-parked waiter with a smaller id steal the lock.
        //
        // Setup: t1 parks *after* t2 so WaitQueue = [(t2, 1); (t1, 1)].
        let state = baseStateWithFrames () |> withRealThreads [ t1 ; t2 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedWait t2 addr 50L
        let state = state |> parkInTimedWait t1 addr 50L

        (syncBlockOf addr state).WaitQueue |> shouldEqual [ (t2, 1) ; (t1, 1) ]

        // Fire in WaitQueue order (head first).
        let state = SyncBlockMonitor.fireWaitTimeout t2 addr state
        let state = SyncBlockMonitor.fireWaitTimeout t1 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t2
            l.ReentrancyCount |> shouldEqual 1
            l.AcquireQueue |> shouldEqual [ (t1, Some 1) ]
        | SyncBlockLock.Free -> failwith "expected Held after timeouts grant ownership"

        block.WaitQueue |> shouldEqual []
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable

        statusOf t1 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        topOfStack t2 state |> shouldEqual (EvalStackValue.Int32 0)
        topOfStack t1 state |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``fireTimeout in ThreadId order (not WaitQueue order) reverses FIFO — guards against scheduler bug`` () : unit =
        // Companion to the test above: explicitly demonstrate that firing
        // in ThreadId order — which is what unsorted `Map.toSeq` dispatch
        // would do — violates FIFO. This is the bug
        // `Program.fireExpiredDeadlines` must avoid by sorting its
        // expired list by SyncBlock-WaitQueue position.
        let state = baseStateWithFrames () |> withRealThreads [ t1 ; t2 ]
        let addr, state = allocateHeapObject state
        // Same setup as the previous test: WaitQueue = [(t2, 1); (t1, 1)].
        let state = state |> parkInTimedWait t2 addr 50L
        let state = state |> parkInTimedWait t1 addr 50L

        // Fire in ThreadId order (tail first).
        let state = SyncBlockMonitor.fireWaitTimeout t1 addr state
        let state = SyncBlockMonitor.fireWaitTimeout t2 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            // t1 stole the lock by firing first; t2 (the original head)
            // is now queued behind. This is the FIFO violation.
            l.LockingThread |> shouldEqual t1
            l.AcquireQueue |> shouldEqual [ (t2, Some 1) ]
        | SyncBlockLock.Free -> failwith "expected Held"

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

        statusOf t2 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

    [<Test>]
    let ``fireTimeout preserves snapshot depth across ownership-transfer reacquire`` () : unit =
        // Re-park scenario: t0 originally Wait()s at depth 7. While
        // parked, the lock is held by t1. When t0's deadline fires,
        // its `Some 7` snapshot must land in the AcquireQueue tail so
        // that t1's eventual Exit transfers ownership back at depth 7.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedWaitAtDepth t0 7 addr 100L
        let state = forceHeld t1 1 addr state

        let state = SyncBlockMonitor.fireWaitTimeout t0 addr state

        match (syncBlockOf addr state).Lock with
        | SyncBlockLock.Held l -> l.AcquireQueue |> shouldEqual [ (t0, Some 7) ]
        | SyncBlockLock.Free -> failwith "expected Held"

    [<Test>]
    let ``fireTimeout leaves other threads' eval stacks untouched`` () : unit =
        // Cross-thread isolation: rewriting t0's stack must not perturb
        // t1's stack.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedWait t0 addr 100L
        // t1 is Runnable with a sentinel value on its stack.
        let state = state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 42) t1

        let state = SyncBlockMonitor.fireWaitTimeout t0 addr state

        topOfStack t0 state |> shouldEqual (EvalStackValue.Int32 0)
        topOfStack t1 state |> shouldEqual (EvalStackValue.Int32 42)

    // -------------------------------------------------------------------
    // fireAcquireTimeout — Monitor.TryEnter(obj, ms) slowpath deadlines
    // -------------------------------------------------------------------

    /// Drive `acquirer` into `BlockedOnSyncBlockAcquire (addr, Some deadlineMs)`,
    /// queued behind `owner`, with the optimistic `Int32 1` on `acquirer`'s eval
    /// stack — mirroring the state `TryEnter_Slowpath` leaves the caller in when
    /// the lock was contended.
    let private parkInTimedAcquire
        (owner : ThreadId)
        (acquirer : ThreadId)
        (addr : ManagedHeapAddress)
        (deadlineMs : int64)
        (state : IlMachineState)
        : IlMachineState
        =
        let state = forceHeld owner 1 addr state
        let block = syncBlockOf addr state

        let locked =
            match block.Lock with
            | SyncBlockLock.Held l -> l
            | SyncBlockLock.Free -> failwith "parkInTimedAcquire: forceHeld returned Free"

        let locked =
            { locked with
                AcquireQueue = locked.AcquireQueue @ [ (acquirer, None) ]
            }

        state
        |> IlMachineState.setSyncBlock
            addr
            {
                Lock = SyncBlockLock.Held locked
                WaitQueue = block.WaitQueue
            }
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) acquirer
        |> Scheduler.setThreadStatus acquirer (ThreadStatus.BlockedOnSyncBlockAcquire (addr, Some deadlineMs))

    [<Test>]
    let ``fireAcquireTimeout dequeues without transferring ownership and rewrites Int32 1 to Int32 0`` () : unit =
        // t0 holds; t1 parked in AcquireQueue with a 100ms deadline.
        // Firing t1's deadline must remove t1 from the queue, leave t0
        // still owning at the same depth, and rewrite t1's optimistic
        // `Int32 1` (acquired) to `Int32 0` (timed out). t1 flips to
        // Runnable.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedAcquire t0 t1 addr 100L

        // Sanity: t1 parked with deadline, t0 holds, optimistic 1 on t1's stack.
        statusOf t1 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, Some 100L))

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            l.AcquireQueue |> shouldEqual [ (t1, None) ]
        | SyncBlockLock.Free -> failwith "expected Held before fire"

        topOfStack t1 state |> shouldEqual (EvalStackValue.Int32 1)

        let state = SyncBlockMonitor.fireAcquireTimeout t1 addr state

        let block = syncBlockOf addr state

        match block.Lock with
        | SyncBlockLock.Held l ->
            // Ownership unchanged — fireAcquireTimeout MUST NOT transfer
            // the lock; the timed waiter just gives up.
            l.LockingThread |> shouldEqual t0
            l.ReentrancyCount |> shouldEqual 1
            l.AcquireQueue |> shouldEqual []
        | SyncBlockLock.Free -> failwith "expected Held after fireAcquireTimeout"

        block.WaitQueue |> shouldEqual []
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        // TryEnter_Slowpath observes Int32 0 ⇒ BCL returns false.
        topOfStack t1 state |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``fireAcquireTimeout preserves FIFO among other queued acquirers`` () : unit =
        // t0 holds; t1 then t2 parked in AcquireQueue. Only t1's deadline
        // fires (t2's is later). t2 must remain in the queue at the same
        // position (head, now that t1 is gone) — its `None` snapshot and
        // status untouched.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ; t2 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedAcquire t0 t1 addr 50L
        // Add t2 to the queue without a deadline (mirrors `Monitor.Enter`).
        let block = syncBlockOf addr state

        let locked =
            match block.Lock with
            | SyncBlockLock.Held l -> l
            | SyncBlockLock.Free -> failwith "expected Held"

        let state =
            state
            |> IlMachineState.setSyncBlock
                addr
                {
                    Lock =
                        SyncBlockLock.Held
                            { locked with
                                AcquireQueue = locked.AcquireQueue @ [ (t2, None) ]
                            }
                    WaitQueue = block.WaitQueue
                }
            |> Scheduler.setThreadStatus t2 (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        let state = SyncBlockMonitor.fireAcquireTimeout t1 addr state

        match (syncBlockOf addr state).Lock with
        | SyncBlockLock.Held l ->
            l.LockingThread |> shouldEqual t0
            // t2 stays at the head (now the only entry).
            l.AcquireQueue |> shouldEqual [ (t2, None) ]
        | SyncBlockLock.Free -> failwith "expected Held"

        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

        statusOf t2 state
        |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        topOfStack t1 state |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``fireAcquireTimeout fails loud when the thread is not in AcquireQueue`` () : unit =
        // t0 holds; t1 is NOT in the AcquireQueue. Reaching the fire path
        // for such a thread indicates a structural bug — the deadline-
        // enumeration step in Program.fireExpiredDeadlines selects on
        // BlockedOnSyncBlockAcquire status, so a thread in that status
        // must still be queued.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.fireAcquireTimeout t1 addr state |> ignore)

        exn.Message |> shouldContainText "AcquireQueue"

    [<Test>]
    let ``fireAcquireTimeout fails loud when the SyncBlock is Free`` () : unit =
        // Free SyncBlock has no AcquireQueue; reaching the fire path
        // means a parked acquirer outlived its lock owner — a structural
        // invariant violation worth surfacing.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ]
        let addr, state = allocateHeapObject state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.fireAcquireTimeout t0 addr state |> ignore)

        exn.Message |> shouldContainText "Free"

    [<Test>]
    let ``fireAcquireTimeout leaves other threads' eval stacks untouched`` () : unit =
        // Cross-thread isolation: rewriting t1's stack must not perturb t0's.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let addr, state = allocateHeapObject state
        let state = state |> parkInTimedAcquire t0 t1 addr 100L
        // t0 has its own sentinel value on its stack.
        let state = state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 42) t0

        let state = SyncBlockMonitor.fireAcquireTimeout t1 addr state

        topOfStack t1 state |> shouldEqual (EvalStackValue.Int32 0)
        topOfStack t0 state |> shouldEqual (EvalStackValue.Int32 42)
