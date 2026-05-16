namespace WoofWare.PawPrint.Test

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
                SyncBlock = SyncBlock.Empty
            }

        let addr, heap = state.ManagedHeap |> ManagedHeap.allocateNonArray stub

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
        SyncBlockMonitor.wait thread addr state

    let private parkInWait (thread : ThreadId) (addr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState =
        parkInWaitAtDepth thread 1 addr state

    let private t0 = ThreadId 0
    let private t1 = ThreadId 1
    let private t2 = ThreadId 2
    let private t3 = ThreadId 3

    // -------------------------------------------------------------------
    // Wait — basic shape
    // -------------------------------------------------------------------

    [<Test>]
    let ``wait releases the lock and parks the caller in WaitQueue with snapshot depth`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 1 addr state

        let state = SyncBlockMonitor.wait t0 addr state

        let block = syncBlockOf addr state
        block.Lock |> shouldEqual SyncBlockLock.Free
        block.WaitQueue |> shouldEqual [ t0, 1 ]
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait addr)

    [<Test>]
    let ``wait snapshots the caller's reentrancy depth verbatim`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state
        let state = forceHeld t0 7 addr state

        let state = SyncBlockMonitor.wait t0 addr state

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
            |> Scheduler.setThreadStatus t1 (ThreadStatus.BlockedOnSyncBlockAcquire addr)
            |> Scheduler.setThreadStatus t2 (ThreadStatus.BlockedOnSyncBlockAcquire addr)

        let state = SyncBlockMonitor.wait t0 addr state

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
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait addr)

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
            |> Scheduler.setThreadStatus t1 (ThreadStatus.BlockedOnSyncBlockAcquire addr)

        let state = SyncBlockMonitor.wait t0 addr state

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
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.wait t1 addr state |> ignore)

        exn.Message |> shouldContainText "does not own"

    [<Test>]
    let ``wait on Free SyncBlock fails loud`` () : unit =
        let state = baseState () |> withThreads [ t0 ]
        let addr, state = allocateHeapObject state

        let exn =
            Assert.Throws<System.Exception> (fun () -> SyncBlockMonitor.wait t0 addr state |> ignore)

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
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)

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
            |> Scheduler.setThreadStatus t2 (ThreadStatus.BlockedOnSyncBlockAcquire addr)

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
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)
        statusOf t3 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)

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
        statusOf t0 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)
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
    // applySpuriousWakeups — strategy interpretation
    // -------------------------------------------------------------------

    /// Snapshot of just the parts of `IlMachineState` that
    /// `applySpuriousWakeups` is allowed to touch. ThreadState as a whole
    /// is not equality-comparable (it embeds MethodStates with structural
    /// non-equality), so we compare what we care about explicitly.
    let private wakeupVisibleState (state : IlMachineState) =
        let blocks = state.ManagedHeap.NonArrayObjects |> Map.map (fun _ v -> v.SyncBlock)

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
        statusOf t1 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)
        statusOf t2 state |> shouldEqual (ThreadStatus.BlockedOnSyncBlockAcquire addr)

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
        statusOf t0 state5 |> shouldEqual (ThreadStatus.BlockedOnSyncBlockWait addr)

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
                   |> List.forall (fun tid -> statusOf tid state = ThreadStatus.BlockedOnSyncBlockAcquire addr)
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
