namespace WoofWare.PawPrint.Test

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
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
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
        let state = LowLevelMonitor.wait t0 id state

        let monitor = monitorOf id state
        monitor.Owner |> shouldEqual None
        monitor.AcquireQueue |> shouldEqual []
        monitor.WaitQueue |> shouldEqual [ t0 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorWait id)

    [<Test>]
    let ``wait while AcquireQueue is non-empty transfers ownership to the head`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.acquire t2 id state |> blocked
        let state = LowLevelMonitor.wait t0 id state

        let monitor = monitorOf id state
        // Wait's release path transferred ownership to the
        // AcquireQueue head (t1); t0 parked on the WaitQueue.
        monitor.Owner |> shouldEqual (Some t1)
        monitor.AcquireQueue |> shouldEqual [ t2 ]
        monitor.WaitQueue |> shouldEqual [ t0 ]
        statusOf t1 state |> shouldEqual ThreadStatus.Runnable
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnMonitorAcquire id)
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnMonitorWait id)

    [<Test>]
    let ``signalRelease promotes wait-queue head to owner via the release path`` () : unit =
        let state = baseState () |> withThreads [ t0 ; t1 ; t2 ]
        let id, state = LowLevelMonitor.create state
        let state = LowLevelMonitor.acquire t0 id state |> acquired
        // Park t1 in the wait queue: contended acquire, then release
        // transfers ownership to t1, then t1 calls Wait.
        let state = LowLevelMonitor.acquire t1 id state |> blocked
        let state = LowLevelMonitor.release t0 id state
        let state = LowLevelMonitor.wait t1 id state
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
        let state = LowLevelMonitor.wait t1 id state
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
        let state = LowLevelMonitor.wait t0 id state

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
            Assert.Throws<System.Exception> (fun () -> LowLevelMonitor.wait t1 id state |> ignore)

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
