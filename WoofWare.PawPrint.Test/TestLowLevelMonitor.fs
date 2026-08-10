namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Property-based tests for the deterministic `LowLevelMonitor` state
/// machine that backs the `SystemNative_LowLevelMonitor_*` QCalls.
/// The state machine is exercised in isolation through a stub
/// `IlMachineState`: we never need a real method-state for these tests,
/// so each ThreadState's `MethodStates` map is empty and its
/// `ActiveMethodState` is a sentinel — the monitor module only reads and
/// writes `Status`, the `LowLevelMonitors` registry, and
/// `NextLowLevelMonitorId`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLowLevelMonitor =

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
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            IsRaisingForeignException = false
            Name = None
            Cpu = CpuId 0
            // Inert here: a frameless stub cannot execute the `SystemNative_*OSThreadId`
            // P/Invoke that reads it. Do not reuse this literal for a stub standing in
            // for more than one thread -- guest OS thread ids must be distinct.
            OsThreadId = OsThreadId 1u
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

    let private monitorOf (id : LowLevelMonitorId) (state : IlMachineState) : LowLevelMonitorState =
        Map.find id state.Kernel.LowLevelMonitors

    let private acquired (outcome : LowLevelMonitor.AcquireOutcome) : IlMachineState =
        match outcome with
        | LowLevelMonitor.AcquireOutcome.Acquired state -> state
        | LowLevelMonitor.AcquireOutcome.Blocked _ -> failwith "expected Acquired but got Blocked"

    let private blocked (outcome : LowLevelMonitor.AcquireOutcome) : IlMachineState =
        match outcome with
        | LowLevelMonitor.AcquireOutcome.Blocked state -> state
        | LowLevelMonitor.AcquireOutcome.Acquired _ -> failwith "expected Blocked but got Acquired"

    let private t0 = ThreadId 0
    let private t1 = ThreadId 1
    let private t2 = ThreadId 2
    let private t3 = ThreadId 3

    // ----- Real-frame test scaffolding -----
    //
    // `fireTimeout` rewrites the parked thread's top-of-eval-stack
    // (`Int32 1 → Int32 0`), so any test that drives `fireTimeout` needs a
    // thread with a live MethodState — the bare `stubThreadState` above is
    // intentionally unusable for eval-stack mechanics.
    //
    // We borrow the pattern from `TestNullaryIlOp.fs`: build a real frame
    // backed by `System.Object::ToString`, since any concretized method is
    // sufficient (the IL body is never executed by these tests; only the
    // EvaluationStack is observed).

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
    /// eval-stack operations on one thread do not visibly bleed into
    /// another.
    let private withRealThreads (threads : ThreadId list) (state : IlMachineState) : IlMachineState =
        let state, threadMap =
            threads
            |> List.fold
                (fun (state : IlMachineState, acc : Map<ThreadId, ThreadState>) (tid : ThreadId) ->
                    let state, methodState = mintFrame state
                    // Distinct OS thread ids, minted by the same policy the
                    // real allocation sites use: these stand in for guest
                    // threads, and no two threads may share an id.
                    let osThreadId = EmulatedKernel.osThreadId tid

                    state, acc |> Map.add tid (ThreadState.New (CpuId 0) osThreadId methodState)
                )
                (state, Map.empty)

        { state with
            ThreadState = threadMap
        }

    let private topOfStack (thread : ThreadId) (state : IlMachineState) : EvalStackValue =
        match IlMachineState.peekEvalStack thread state with
        | Some v -> v
        | None -> failwith $"thread %O{thread} has empty eval stack"

    [<Test>]
    let ``create mints distinct ids`` () : unit =
        let state = baseState ()
        let id1, state = LowLevelMonitor.create state
        let id2, state = LowLevelMonitor.create state
        let id3, _ = LowLevelMonitor.create state

        id1 |> shouldNotEqual id2
        id1 |> shouldNotEqual id3
        id2 |> shouldNotEqual id3

    [<Test>]
    let ``newly minted monitor is unowned with empty queues`` () : unit =
        let state = baseState ()
        let id, state = LowLevelMonitor.create state

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual None
        monitor.AcquireQueue |> shouldEqual []
        monitor.WaitQueue |> shouldEqual []

    [<Test>]
    let ``minted handles are never IntPtr.Zero (BCL OOM guard never fires)`` () : unit =
        let state = baseState ()
        let id, _ = LowLevelMonitor.create state
        // The handle must be non-zero so the guest's `if _nativeMonitor ==
        // IntPtr.Zero throw OOM` check stays quiet for successful creates.
        let (LowLevelMonitorId i) = id
        i |> shouldNotEqual 0

    [<Test>]
    let ``uncontended acquire takes ownership and leaves status Runnable`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state

        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual (Some t0)
        monitor.AcquireQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``contended acquire parks the caller on the FIFO acquire queue`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual (Some t0)
        monitor.AcquireQueue |> shouldEqual [ t1 ]
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

    [<Test>]
    let ``FIFO acquire queue is preserved across multiple parks`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.acquire t2 id state |> blocked
        let state = LowLevelMonitor.acquire t3 id state |> blocked

        (monitorOf id state).AcquireQueue |> shouldEqual [ t1 ; t2 ; t3 ]

    [<Test>]
    let ``release with no waiters clears ownership`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.release t0 id state

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual None
        monitor.AcquireQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``release with waiters transfers ownership to FIFO head`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.acquire t2 id state |> blocked
        let state = LowLevelMonitor.release t0 id state

        let monitor = monitorOf id state
        // Ownership was transferred directly to t1; t1 is Runnable and
        // will resume past its `Acquire` call site already holding the
        // monitor. t2 remains parked behind t1.
        monitor.Owner |> shouldEqual (Some t1)
        monitor.AcquireQueue |> shouldEqual [ t2 ]
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

    [<Test>]
    let ``wait moves caller to wait queue and releases monitor`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.wait t0 id None state

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual None
        monitor.AcquireQueue |> shouldEqual []
        monitor.WaitQueue |> shouldEqual [ t0 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, None))

    [<Test>]
    let ``wait while AcquireQueue is non-empty transfers ownership to the head`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.acquire t2 id state |> blocked
        let state = LowLevelMonitor.wait t0 id None state

        let monitor = monitorOf id state
        // Wait's release path transferred ownership to the
        // AcquireQueue head (t1); t0 parked on the WaitQueue.
        monitor.Owner |> shouldEqual (Some t1)
        monitor.AcquireQueue |> shouldEqual [ t2 ]
        monitor.WaitQueue |> shouldEqual [ t0 ]
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, None))

    [<Test>]
    let ``signalRelease promotes wait-queue head to owner via the release path`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        // Park t1 in the wait queue: contended acquire, then release
        // transfers ownership to t1, then t1 calls Wait.
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.release t0 id state
        let state = LowLevelMonitor.wait t1 id None state
        // Another thread acquires and signal-releases.
        let state = LowLevelMonitor.acquire t2 id state |> acquired
        let state = LowLevelMonitor.signalRelease t2 id state

        let monitor = monitorOf id state
        // Signal_Release moved t1 from WaitQueue onto the (empty)
        // AcquireQueue, then the release path transferred ownership to
        // the new head — which is t1.
        monitor.Owner |> shouldEqual (Some t1)
        monitor.WaitQueue |> shouldEqual []
        monitor.AcquireQueue |> shouldEqual []
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``signalRelease with non-empty acquire queue keeps waiter behind earlier acquires`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let id, state = LowLevelMonitor.create state
        // Park t1 in the wait queue: acquire then wait.
        let state = LowLevelMonitor.acquire t1 id state |> acquired
        let state = LowLevelMonitor.wait t1 id None state
        // t3 acquires uncontended; t2 then parks in the acquire queue.
        let state = LowLevelMonitor.acquire t3 id state |> acquired
        let state = LowLevelMonitor.acquire t2 id state |> blocked
        // Signal_Release from t3: t1 moves to the acquire queue tail behind t2;
        // release then transfers ownership to the new head (t2). t1 stays
        // BlockedOnMonitorAcquire until a subsequent release reaches it.
        let state = LowLevelMonitor.signalRelease t3 id state

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual (Some t2)
        monitor.AcquireQueue |> shouldEqual [ t1 ]
        monitor.WaitQueue |> shouldEqual []
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

    [<Test>]
    let ``signalRelease with empty wait queue degenerates to release`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.signalRelease t0 id state

        let monitor = monitorOf id state
        // With no waiter to signal, signalRelease is plain release —
        // ownership is transferred to the AcquireQueue head (t1).
        monitor.Owner |> shouldEqual (Some t1)
        monitor.AcquireQueue |> shouldEqual []
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``destroy removes a quiescent monitor from the registry`` () : unit =
        let state = baseState ()
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.destroy id state

        Map.containsKey id state.Kernel.LowLevelMonitors |> shouldEqual false

    [<Test>]
    let ``destroy fails loud when the monitor is still owned`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.destroy id state |> ignore)

        exn.Message |> shouldContainText "still held"

    [<Test>]
    let ``destroy asserts the Owner/AcquireQueue invariant defensively`` () : unit =
        // The standard transitions all preserve the invariant
        // (Owner = None iff AcquireQueue = []), so to exercise the
        // defensive check we splice a corrupt monitor into the registry
        // directly. This guards against future regressions in
        // release/wait/signalRelease that might leave the queue
        // non-empty with no owner.
        let state = baseState () |> withThreads [ t1 ]
        let id, state = LowLevelMonitor.create state

        let corrupt : LowLevelMonitorState =
            {
                Owner = None
                AcquireQueue = [ t1 ]
                WaitQueue = []
            }

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    LowLevelMonitors = kernel.LowLevelMonitors |> Map.add id corrupt
                }
            )

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.destroy id state |> ignore)

        exn.Message |> shouldContainText "BlockedOnMonitorAcquire"

    [<Test>]
    let ``destroy fails loud with parked waiters`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.wait t0 id None state

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.destroy id state |> ignore)

        exn.Message |> shouldContainText "BlockedOnMonitorWait"

    [<Test>]
    let ``destroy on an unknown handle fails loud (use-after-free)`` () : unit =
        let state = baseState ()
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.destroy id state

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.destroy id state |> ignore)

        exn.Message |> shouldContainText "not registered"

    [<Test>]
    let ``recursive acquire fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.acquire t0 id state |> ignore)

        exn.Message |> shouldContainText "recursive"

    [<Test>]
    let ``release by non-owner fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.release t1 id state |> ignore)

        exn.Message |> shouldContainText "owned by"

    [<Test>]
    let ``release of unowned monitor fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.release t0 id state |> ignore)

        exn.Message |> shouldContainText "unowned"

    [<Test>]
    let ``wait by non-owner fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.wait t1 id None state |> ignore)

        exn.Message |> shouldContainText "owned by"

    [<Test>]
    let ``signalRelease by non-owner fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.signalRelease t1 id state |> ignore)

        exn.Message |> shouldContainText "owned by"

    /// FIFO fairness oracle. For any sequence of contended acquirers that
    /// park behind a single owner, releasing the monitor once per current
    /// owner must transfer ownership through the parked threads in
    /// registration order. This is the load-bearing property for
    /// `LowLevelLock` fairness — moving to LIFO or arbitrary order would
    /// change the observable interleaving of any guest-level lock built
    /// on top.
    [<Test>]
    let ``Property: release transfers ownership through parked threads in FIFO order`` () : unit =
        let property (PositiveInt waiterCount) : bool =
            // Cap waiter count so the test stays fast and threads have
            // distinct stable IDs.
            let n = min 16 waiterCount
            let owner = ThreadId 0
            let waiters = [ 1..n ] |> List.map ThreadId
            let allThreads = owner :: waiters

            let state = baseState () |> withThreads allThreads
            let id, state = LowLevelMonitor.create state
            let state = LowLevelMonitor.acquire owner id state |> acquired

            // Park every waiter in registration order.
            let state =
                waiters
                |> List.fold (fun s w -> LowLevelMonitor.acquire w id s |> blocked) state

            // Snapshot the queue order before any releases run.
            let expectedTransferOrder = (monitorOf id state).AcquireQueue

            // Each release transfers ownership to the FIFO head; the new
            // owner is now Runnable. We then release on its behalf.
            let mutable currentOwner = owner
            let mutable state = state
            let mutable observedOrder = []

            for _ in 1..n do
                state <- LowLevelMonitor.release currentOwner id state

                let next =
                    match (monitorOf id state).Owner with
                    | Some next when statusOf next state = ThreadStatus.Runnable -> next
                    | Some next -> failwith $"release transferred to %O{next} but status is %O{statusOf next state}"
                    | None -> failwith "release did not transfer ownership"

                currentOwner <- next
                observedOrder <- observedOrder @ [ next ]

            observedOrder = expectedTransferOrder

        Check.One (config, property)

    /// Symmetry oracle: every paired (acquire, release) drains the
    /// monitor back to its quiescent state. For any sequence of acquires
    /// and releases that's balanced and well-ordered, the final monitor
    /// should be unowned with empty queues, and every thread should be
    /// Runnable.
    [<Test>]
    let ``Property: balanced acquire-release sequence returns to quiescent state`` () : unit =
        let property (PositiveInt n) : bool =
            let n = min 8 n
            let threads = [ 0 .. n - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads threads
            let id, state = LowLevelMonitor.create state

            // Each thread acquires then immediately releases, in sequence.
            let state =
                threads
                |> List.fold
                    (fun s tid ->
                        let s = LowLevelMonitor.acquire tid id s |> acquired
                        LowLevelMonitor.release tid id s
                    )
                    state

            let monitor = monitorOf id state

            monitor.Owner = None
            && monitor.AcquireQueue = []
            && monitor.WaitQueue = []
            && threads |> List.forall (fun t -> statusOf t state = ThreadStatus.Runnable)

        Check.One (config, property)

    // -------------------------------------------------------------------
    // Spurious-wakeup injection
    // -------------------------------------------------------------------

    /// Helper: drive `thread` through Acquire+Wait on `id`, leaving it in
    /// `WaitQueue` and the monitor unowned (provided no later thread
    /// claims it). Caller is responsible for the ordering invariant —
    /// callers that drive multiple threads must do so sequentially so
    /// each Wait observes the monitor as currently owned.
    let private parkInWait (thread : ThreadId) (id : LowLevelMonitorId) (state : IlMachineState) : IlMachineState =
        let state = LowLevelMonitor.acquire thread id state |> acquired
        LowLevelMonitor.wait thread id None state

    [<Test>]
    let ``spuriousWake on free monitor grants ownership to the woken thread`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id

        // Pre: t0 parked, monitor unowned.
        (monitorOf id state).Owner |> shouldEqual None
        (monitorOf id state).WaitQueue |> shouldEqual [ t0 ]

        let state = LowLevelMonitor.spuriousWake id t0 state

        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t0)
        m.AcquireQueue |> shouldEqual []
        m.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``spuriousWake on held monitor parks the waiter at the AcquireQueue tail`` () : unit =
        // t0 waits, t1 then owns; spurious-waking t0 must queue it behind
        // t1, not steal the monitor.
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = LowLevelMonitor.acquire t1 id state |> acquired

        let state = LowLevelMonitor.spuriousWake id t0 state

        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t1)
        m.AcquireQueue |> shouldEqual [ t0 ]
        m.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable

    [<Test>]
    let ``spuriousWake fails loud when the thread is not in WaitQueue`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        // t0 currently owns; not in WaitQueue.

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.spuriousWake id t0 state |> ignore)

        exn.Message |> shouldContainText "WaitQueue"

    /// Snapshot of just the parts of `IlMachineState` that
    /// `applySpuriousWakeups` is allowed to touch. ThreadState as a whole
    /// is not equality-comparable (it embeds MethodStates with structural
    /// non-equality), so we compare what we care about explicitly.
    let private wakeupVisibleState (state : IlMachineState) =
        let monitors = state.Kernel.LowLevelMonitors
        let statuses = state.ThreadState |> Map.map (fun _ ts -> ts.Status)
        monitors, statuses

    [<Test>]
    let ``applySpuriousWakeups Disabled is bit-identical to the input state`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = state |> parkInWait t1 id
        // Both waiters parked; Owner = None; WaitQueue = [t0; t1].

        let state' =
            LowLevelMonitor.applySpuriousWakeups SpuriousWakeupStrategy.Disabled 42L state

        wakeupVisibleState state' |> shouldEqual (wakeupVisibleState state)

    [<Test>]
    let ``applySpuriousWakeups AlwaysAll drains the WaitQueue in FIFO order`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = state |> parkInWait t1 id
        let state = state |> parkInWait t2 id
        // WaitQueue = [t0; t1; t2], Owner = None.

        let state =
            LowLevelMonitor.applySpuriousWakeups SpuriousWakeupStrategy.AlwaysAll 0L state

        let m = monitorOf id state
        // FIFO: t0 wakes first into a free monitor and takes ownership.
        // t1 and t2 then queue behind t0.
        m.Owner |> shouldEqual (Some t0)
        m.AcquireQueue |> shouldEqual [ t1 ; t2 ]
        m.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

    [<Test>]
    let ``applySpuriousWakeups AlwaysAll processes monitors in ascending id order`` () : unit =
        // Two independent monitors. Order of monitor processing is
        // observable through which owner each ends up with, but more
        // importantly we want to know the iteration is deterministic.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let id1, state = LowLevelMonitor.create state
        let id2, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id1
        let state = state |> parkInWait t1 id1
        let state = state |> parkInWait t2 id2
        let state = state |> parkInWait t3 id2

        let state =
            LowLevelMonitor.applySpuriousWakeups SpuriousWakeupStrategy.AlwaysAll 0L state

        let m1 = monitorOf id1 state
        let m2 = monitorOf id2 state
        m1.Owner |> shouldEqual (Some t0)
        m1.AcquireQueue |> shouldEqual [ t1 ]
        m1.WaitQueue |> shouldEqual []
        m2.Owner |> shouldEqual (Some t2)
        m2.AcquireQueue |> shouldEqual [ t3 ]
        m2.WaitQueue |> shouldEqual []

    [<Test>]
    let ``applySpuriousWakeups Scripted only fires at the named tick`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = state |> parkInWait t1 id

        let script = SpuriousWakeupStrategy.Scripted [ 5L, id, t1 ]

        // At tick 0: nothing scripted; state passes through.
        let state0 = LowLevelMonitor.applySpuriousWakeups script 0L state
        (monitorOf id state0).WaitQueue |> shouldEqual [ t0 ; t1 ]

        // At tick 5: only t1 wakes (and takes ownership because monitor
        // is free).
        let state5 = LowLevelMonitor.applySpuriousWakeups script 5L state
        let m = monitorOf id state5
        m.WaitQueue |> shouldEqual [ t0 ]
        m.Owner |> shouldEqual (Some t1)
        statusOf t1 state5 |> shouldEqual ThreadStatus.Runnable
        statusOf t0 state5 |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, None))

    [<Test>]
    let ``applySpuriousWakeups Scripted fails loud on a stale waiter`` () : unit =
        // Script names a thread that is not in any WaitQueue; the
        // interpreter must reject this so scripts can't silently drift.
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let script = SpuriousWakeupStrategy.Scripted [ 0L, id, t0 ]

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.applySpuriousWakeups script 0L state |> ignore)

        exn.Message |> shouldContainText "WaitQueue"

    [<Test>]
    let ``applySpuriousWakeups Random with probability 0.0 is a no-op`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = state |> parkInWait t1 id

        let state' =
            LowLevelMonitor.applySpuriousWakeups (SpuriousWakeupStrategy.Random (42UL, 0.0)) 7L state

        wakeupVisibleState state' |> shouldEqual (wakeupVisibleState state)

    [<Test>]
    let ``applySpuriousWakeups Random with probability 1.0 matches AlwaysAll`` () : unit =
        // probability=1.0 should wake every (mid, tid) regardless of
        // seed, producing the same state as AlwaysAll.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = state |> parkInWait t1 id
        let state = state |> parkInWait t2 id

        let always =
            LowLevelMonitor.applySpuriousWakeups SpuriousWakeupStrategy.AlwaysAll 0L state

        let random =
            LowLevelMonitor.applySpuriousWakeups (SpuriousWakeupStrategy.Random (0UL, 1.0)) 0L state

        wakeupVisibleState random |> shouldEqual (wakeupVisibleState always)

    [<Test>]
    let ``applySpuriousWakeups Random is deterministic in (seed, tick)`` () : unit =
        // Identical inputs must produce identical outputs — the whole
        // point of a deterministic strategy is replayability.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id
        let state = state |> parkInWait t1 id
        let state = state |> parkInWait t2 id
        let state = state |> parkInWait t3 id

        let strategy = SpuriousWakeupStrategy.Random (123UL, 0.5)

        let r1 = LowLevelMonitor.applySpuriousWakeups strategy 17L state
        let r2 = LowLevelMonitor.applySpuriousWakeups strategy 17L state

        wakeupVisibleState r1 |> shouldEqual (wakeupVisibleState r2)

    [<Test>]
    let ``applySpuriousWakeups Random rejects NaN probability`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInWait t0 id

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                LowLevelMonitor.applySpuriousWakeups (SpuriousWakeupStrategy.Random (1UL, System.Double.NaN)) 0L state
                |> ignore
            )

        exn.Message |> shouldContainText "NaN"

    [<Test>]
    let ``applySpuriousWakeups preserves the Owner/AcquireQueue invariant on a contended monitor`` () : unit =
        // Pre-existing acquirer + multiple waiters + AlwaysAll wake — the
        // hard case for the invariant `Owner = None iff AcquireQueue = []`.
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ; t3 ]
        let id, state = LowLevelMonitor.create state
        // t0 owns; t1 contends.
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        // t0 waits — t1 inherits ownership; t0 joins the WaitQueue.
        let state = LowLevelMonitor.wait t0 id None state
        // t1 waits — Owner becomes None, WaitQueue = [t0; t1].
        let state = LowLevelMonitor.wait t1 id None state

        let state =
            LowLevelMonitor.applySpuriousWakeups SpuriousWakeupStrategy.AlwaysAll 0L state

        let m = monitorOf id state
        m.WaitQueue |> shouldEqual []
        // Invariant: Owner = None implies AcquireQueue = [].
        match m.Owner, m.AcquireQueue with
        | None, _ :: _ -> failwith "invariant violated: AcquireQueue non-empty with no Owner"
        | _ -> ()
        // And specifically: t0 took ownership, t1 queued behind.
        m.Owner |> shouldEqual (Some t0)
        m.AcquireQueue |> shouldEqual [ t1 ]

    [<Test>]
    let ``Property: applySpuriousWakeups preserves invariants for AlwaysAll on randomly parked waiters`` () : unit =
        // Park a random number of threads via Acquire+Wait, then apply
        // AlwaysAll. Final state must satisfy:
        //   1. WaitQueue is empty on the affected monitor.
        //   2. Owner = None implies AcquireQueue = [] (the directional
        //      invariant).
        //   3. The set of (Owner ++ AcquireQueue) equals the original
        //      WaitQueue (no thread lost or duplicated).
        let property (PositiveInt n) : bool =
            let n = min 6 n
            let threads = [ 0 .. n - 1 ] |> List.map ThreadId
            let state = baseState () |> withThreads threads
            let id, state = LowLevelMonitor.create state

            let state = threads |> List.fold (fun s tid -> parkInWait tid id s) state

            let beforeWaiters = (monitorOf id state).WaitQueue

            let state =
                LowLevelMonitor.applySpuriousWakeups SpuriousWakeupStrategy.AlwaysAll 0L state

            let m = monitorOf id state

            let owners =
                match m.Owner with
                | Some o -> [ o ]
                | None -> []

            // The forbidden state is Owner = None with non-empty queue.
            let invariantHolds =
                match m.Owner, m.AcquireQueue with
                | None, _ :: _ -> false
                | _ -> true

            m.WaitQueue = []
            && invariantHolds
            && Set.ofList (owners @ m.AcquireQueue) = Set.ofList beforeWaiters

        Check.One (config, property)

    // ----- LowLevelMonitor.wait deadline plumbing -----
    //
    // `BlockedOnMonitorWait` now carries an optional deadline. The
    // scheduler in `Program.fireExpiredDeadlines` keys off this option to
    // decide whether the thread is a TimedWait waiter (finite Some) or
    // an untimed `Wait` waiter (None). These tests pin the variant
    // payload that is the load-bearing contract.

    [<Test>]
    let ``wait with no deadline records None on the BlockedOnMonitorWait status`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let state = LowLevelMonitor.wait t0 id None state

        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, None))

    [<Test>]
    let ``wait with a finite deadline records Some on the BlockedOnMonitorWait status`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired

        let state = LowLevelMonitor.wait t0 id (Some 1234L) state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, Some 1234L))

    [<Test>]
    let ``wait deadline is recorded even when the waiter inherits ownership transfer`` () : unit =
        // t0 owns, t1 contends, then t0 waits — ownership transfers to t1
        // and t0 joins the WaitQueue. The deadline payload must survive
        // that transfer path (which writes the status from the
        // empty-AcquireQueue branch of `wait`).
        let state = baseState () |> withThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked

        let state = LowLevelMonitor.wait t0 id (Some 9999L) state

        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, Some 9999L))

        (monitorOf id state).Owner |> shouldEqual (Some t1)
        (monitorOf id state).WaitQueue |> shouldEqual [ t0 ]

    // ----- LowLevelMonitor.fireTimeout -----
    //
    // These tests need real frames so the eval-stack rewrite
    // (`Int32 1 → Int32 0`) is observable. They simulate the
    // park-time optimistic push by pushing `Int32 1` themselves before
    // calling `fireTimeout`.

    /// Park `thread` on `id` with a finite deadline and the park-time
    /// optimistic `Int32 1` already on its eval stack, mirroring what
    /// `SystemNative_LowLevelMonitor_TimedWait` does at the IL boundary.
    let private parkInTimedWait
        (thread : ThreadId)
        (id : LowLevelMonitorId)
        (deadlineTicks : int64)
        (state : IlMachineState)
        : IlMachineState
        =
        let state = LowLevelMonitor.acquire thread id state |> acquired

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) thread

        LowLevelMonitor.wait thread id (Some deadlineTicks) state

    [<Test>]
    let ``fireTimeout on free monitor grants ownership and rewrites Int32 1 to Int32 0`` () : unit =
        let state = baseStateWithFrames () |> withRealThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInTimedWait t0 id 100L

        // Sanity: t0 parked with the optimistic 1 sitting on its eval
        // stack, monitor unowned, deadline recorded on Status.
        statusOf t0 state
        |> shouldEqual (ThreadStatus.BlockedOnMonitorWait (id, Some 100L))

        (monitorOf id state).Owner |> shouldEqual None
        (monitorOf id state).WaitQueue |> shouldEqual [ t0 ]

        topOfStack t0 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

        let state = LowLevelMonitor.fireTimeout t0 id state

        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t0)
        m.AcquireQueue |> shouldEqual []
        m.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual ThreadStatus.Runnable
        // TimedWait returns 0 = timed out.
        topOfStack t0 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``fireTimeout on held monitor parks at AcquireQueue tail and rewrites Int32 1 to Int32 0`` () : unit =
        // t0 timed-waits; t1 then owns. Firing t0's timeout must not let
        // t0 steal the monitor — it queues behind t1.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInTimedWait t0 id 100L
        let state = LowLevelMonitor.acquire t1 id state |> acquired

        let state = LowLevelMonitor.fireTimeout t0 id state

        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t1)
        m.AcquireQueue |> shouldEqual [ t0 ]
        m.WaitQueue |> shouldEqual []
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        // The rewrite happens regardless of which reacquire path is taken;
        // by the time t0 is selected by the scheduler past TimedWait's
        // call site it will observe `0` on top of stack.
        topOfStack t0 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``fireTimeout fails loud when the thread is not in WaitQueue`` () : unit =
        let state = baseStateWithFrames () |> withRealThreads [ t0 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        // t0 currently owns; not in WaitQueue. Calling fireTimeout here
        // mirrors the structural-bug case the scheduler is supposed to
        // surface: a deadline observed for a thread the monitor does not
        // know about as a waiter.

        let exn =
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.fireTimeout t0 id state |> ignore)

        exn.Message |> shouldContainText "WaitQueue"

    [<Test>]
    let ``fireTimeout preserves FIFO across multiple AcquireQueue parks`` () : unit =
        // Two contenders already in AcquireQueue; t2 times out while
        // parked in WaitQueue. The timeout-wake must push t2 to the
        // *tail* of AcquireQueue, not the head, so an earlier owner
        // hand-off doesn't accidentally jump t2 over t1.
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) t2

        let state = LowLevelMonitor.acquire t2 id state |> blocked
        // Move t2 from AcquireQueue into WaitQueue via a wait-with-deadline:
        // it must currently own the monitor to call wait, so first hand
        // ownership over. Easiest path: release t0, let t1 take ownership,
        // release t1, let t2 take ownership, then t2 waits.
        let state = LowLevelMonitor.release t0 id state
        // After release, t1 owns; t2 still queued.
        (monitorOf id state).Owner |> shouldEqual (Some t1)
        let state = LowLevelMonitor.release t1 id state
        // After release, t2 owns; queue empty.
        (monitorOf id state).Owner |> shouldEqual (Some t2)
        // Now park t0 and t1 in AcquireQueue against t2's ownership.
        let state = LowLevelMonitor.acquire t0 id state |> blocked
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        // Finally, t2 waits with a deadline (TimedWait posture).
        let state = LowLevelMonitor.wait t2 id (Some 5L) state
        // After wait: ownership transfers to t0 (FIFO head of AcquireQueue),
        // t1 still queued, t2 in WaitQueue.
        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t0)
        m.AcquireQueue |> shouldEqual [ t1 ]
        m.WaitQueue |> shouldEqual [ t2 ]

        let state = LowLevelMonitor.fireTimeout t2 id state

        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t0)
        // t2 must go to the tail, behind t1.
        m.AcquireQueue |> shouldEqual [ t1 ; t2 ]
        m.WaitQueue |> shouldEqual []
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

        topOfStack t2 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``fireTimeout in WaitQueue head-first order preserves FIFO across same-tick expiries`` () : unit =
        // Pin the contract that `Program.fireExpiredDeadlines` must
        // respect: when two TimedWait waiters on the same monitor expire
        // in the same virtual-clock tick, firing them in WaitQueue order
        // gives ownership to the FIFO head. The bug we're guarding
        // against: iterating threads by ThreadId would let a later-parked
        // waiter with a smaller id (the *tail* of WaitQueue here) steal
        // the monitor by firing first against the unowned monitor.
        //
        // Setup t1 parks *after* t2 so WaitQueue = [t2; t1]. Then firing
        // in WaitQueue order yields t2 as owner.
        let state = baseStateWithFrames () |> withRealThreads [ t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInTimedWait t2 id 50L
        let state = state |> parkInTimedWait t1 id 50L

        (monitorOf id state).WaitQueue |> shouldEqual [ t2 ; t1 ]

        // Fire in WaitQueue order (head first).
        let state = LowLevelMonitor.fireTimeout t2 id state
        let state = LowLevelMonitor.fireTimeout t1 id state

        let m = monitorOf id state
        m.Owner |> shouldEqual (Some t2)
        m.AcquireQueue |> shouldEqual [ t1 ]
        m.WaitQueue |> shouldEqual []
        statusOf t2 state |> shouldEqual ThreadStatus.Runnable
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

        topOfStack t2 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        topOfStack t1 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

    [<Test>]
    let ``fireTimeout in ThreadId order (not WaitQueue order) reverses FIFO — guards against scheduler bug`` () : unit =
        // Companion to the test above: explicitly demonstrate that firing
        // in ThreadId order — which is what the unsorted `Map.toSeq`
        // dispatch would do — violates FIFO. This is the bug
        // `Program.fireExpiredDeadlines` must avoid by sorting its
        // expired list by monitor-WaitQueue position. The assertion is
        // the *broken* outcome; if a future change accidentally drops
        // the sort, this test still passes but its sibling above starts
        // documenting the contract that was lost.
        let state = baseStateWithFrames () |> withRealThreads [ t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        // Same setup as the previous test: WaitQueue = [t2; t1].
        let state = state |> parkInTimedWait t2 id 50L
        let state = state |> parkInTimedWait t1 id 50L

        // Fire in ThreadId order (tail first).
        let state = LowLevelMonitor.fireTimeout t1 id state
        let state = LowLevelMonitor.fireTimeout t2 id state

        let m = monitorOf id state
        // t1 stole the monitor by firing first; t2 (the original head)
        // is now queued behind. This is the FIFO violation.
        m.Owner |> shouldEqual (Some t1)
        m.AcquireQueue |> shouldEqual [ t2 ]
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)

    [<Test>]
    let ``fireTimeout leaves other threads' eval stacks untouched`` () : unit =
        // Cross-thread isolation: rewriting t0's stack must not perturb
        // t1's stack. (The IlMachineState eval-stack helpers are keyed on
        // ThreadId, but pinning this contract guards against accidental
        // sharing of MethodStates across threads.)
        let state = baseStateWithFrames () |> withRealThreads [ t0 ; t1 ]
        let id, state = LowLevelMonitor.create state
        let state = state |> parkInTimedWait t0 id 100L
        // t1 is Runnable with a sentinel value on its stack.
        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 42)) t1

        let state = LowLevelMonitor.fireTimeout t0 id state

        topOfStack t0 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))

        topOfStack t1 state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 42))
