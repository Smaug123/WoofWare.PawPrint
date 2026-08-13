namespace WoofWare.PawPrint

/// Deterministic state-machine for the `Monitor_Wait` / `Monitor_Pulse` /
/// `Monitor_PulseAll` QCalls that back `System.Threading.Monitor`'s managed
/// condition-variable surface. CoreCLR sits this on the `SyncBlock` /
/// `AwareLock` / `CLREventBase` triple; we reproduce the observable semantics
/// directly over `SyncBlock` (defined in `ManagedHeap.fs`) and three
/// `ThreadStatus` cases (`Runnable`, `BlockedOnSyncBlockAcquire`,
/// `BlockedOnSyncBlockWait`).
///
/// The module is pure: every transition is `IlMachineState -> IlMachineState`
/// and never reads from a real clock, the host's mutex implementation, or any
/// nondeterministic source. All ordering decisions are FIFO over the
/// `AcquireQueue` / `WaitQueue`, which is load-bearing for fairness:
/// deviating from FIFO changes the observable interleaving and is not a
/// refactor.
///
/// Reentrancy: SyncBlocks ARE reentrant (unlike `LowLevelMonitor`).
/// `Monitor.Wait` snapshots the caller's `ReentrancyCount`, fully releases
/// the lock, parks the caller in `WaitQueue`, and — on subsequent wake-up
/// via `Pulse` / `PulseAll` (or a spurious wake) — re-enters at the same
/// snapshot depth via the `(ThreadId * int option) list` AcquireQueue: the
/// `int option` carries the snapshot for resumed waiters and `None` for
/// fresh `Monitor.Enter` entrants. `Exit_FastPath`'s ownership-transfer path
/// reads the snapshot and restores the depth verbatim, so the IL after
/// `Wait` resumes already owning the lock at the right depth.
///
/// Ownership transfer (same model as `LowLevelMonitor`): when `wait` gives
/// up the lock and the AcquireQueue is non-empty, ownership is handed
/// directly from the releaser to the FIFO head — the head's status flips to
/// `Runnable` and its `ReentrancyCount` is set from the snapshot. The
/// released thread does NOT briefly observe a free lock with a non-empty
/// AcquireQueue; that intermediate state would mean the next thread resumes
/// past its `Enter` site without owning the lock.
///
/// Pulse does NOT release the lock. The caller of `Monitor.Pulse` /
/// `Monitor.PulseAll` keeps ownership; the woken waiter is moved to the
/// AcquireQueue tail and only acquires when the current owner's eventual
/// `Exit` transfers ownership to it (possibly after intervening fresh
/// entrants). This mirrors CoreCLR exactly.
///
/// Spurious wakeups: injected from outside this module under control of
/// `EmulatedKernel.SyncBlockSpuriousWakeup`. The transition is `spuriousWake`
/// below; `applySpuriousWakeups` snapshots the wait-queue membership at entry
/// and applies the strategy's wakeup list in deterministic order. Guest code
/// that depends on the absence of spurious wakeups is incorrect against real
/// CoreCLR; switching the strategy to `AlwaysAll` is the deterministic way
/// to expose those bugs.
[<RequireQualifiedAccess>]
module SyncBlockMonitor =

    /// Read the SyncBlock for `addr`, or fail loud. (The underlying heap
    /// lookup raises if `addr` is not a non-array object, which is the only
    /// failure mode we can hit here since Monitor.Wait/Pulse/PulseAll on a
    /// missing object would have failed earlier at the `evalStackValueToObjectRef`
    /// step.)
    let private readBlock (addr : ManagedHeapAddress) (state : IlMachineState) : SyncBlock =
        IlMachineState.getSyncBlock addr state

    /// Write back a SyncBlock with the given `Lock` portion and `WaitQueue`.
    let private writeBlock
        (addr : ManagedHeapAddress)
        (lockState : SyncBlockLock)
        (waitQueue : (ThreadId * int) list)
        (state : IlMachineState)
        : IlMachineState
        =
        IlMachineState.setSyncBlock
            addr
            {
                Lock = lockState
                WaitQueue = waitQueue
            }
            state

    /// `Monitor.Wait`: the caller must hold the lock; the call snapshots
    /// the reentrancy depth, parks the caller on the WaitQueue (FIFO tail)
    /// with the snapshot, and fully releases the lock (transferring
    /// ownership to the AcquireQueue head if any, else setting Lock = Free).
    /// Caller's status flips to `BlockedOnSyncBlockWait`. Resumption is
    /// driven by `pulse` / `pulseAll` / `spuriousWake` / `fireWaitTimeout`, each
    /// of which moves the waiter to the AcquireQueue carrying the snapshot
    /// for depth restoration on re-acquire.
    ///
    /// `deadlineTicks = None` is `Monitor.Wait(obj)` (infinite); `Some ms` is
    /// `Monitor.Wait(obj, timeout)` (finite). The deadline is the absolute
    /// virtual-clock tick at which the wait expires; on expiry the
    /// driver fires `fireWaitTimeout` which routes the waiter through the same
    /// reacquire path and rewrites the optimistic `Int32 1` slot pushed at
    /// park time to `Int32 0` (timed out).
    ///
    /// Atomicity: from the guest's perspective, Wait is a single operation
    /// — the caller cannot observe a state in which it has released the
    /// lock but has not yet been parked, because both happen in one call.
    let wait
        (thread : ThreadId)
        (addr : ManagedHeapAddress)
        (deadlineTicks : int64 option)
        (state : IlMachineState)
        : IlMachineState
        =
        let block = readBlock addr state

        let locked =
            match block.Lock with
            | SyncBlockLock.Free ->
                failwith
                    $"Monitor.Wait on object %O{addr}: SyncBlock is Free but Wait requires the caller to own the lock. The BCL would raise SynchronizationLockException; we fail loud."
            | SyncBlockLock.Held l when l.LockingThread <> thread ->
                failwith
                    $"Monitor.Wait on object %O{addr}: caller %O{thread} does not own the lock (owner = %O{l.LockingThread}). The BCL would raise SynchronizationLockException."
            | SyncBlockLock.Held l -> l

        let snapshot = locked.ReentrancyCount
        let newWaitQueue = block.WaitQueue @ [ (thread, snapshot) ]

        // Atomically release the lock. If anyone is queued for Enter, transfer
        // ownership; otherwise the lock becomes Free with the WaitQueue
        // preserved.
        let newLock, wakeNextOwner =
            match locked.AcquireQueue with
            | [] -> SyncBlockLock.Free, None
            | (nextOwner, ackSnapshot) :: rest ->
                // `None` snapshot = fresh entrant (depth 1); `Some d` = woken
                // from an earlier Wait (depth d). Same rule Exit_FastPath uses.
                let restoredDepth = ackSnapshot |> Option.defaultValue 1

                let next =
                    {
                        LockingThread = nextOwner
                        ReentrancyCount = restoredDepth
                        AcquireQueue = rest
                    }

                SyncBlockLock.Held next, Some nextOwner

        let state = writeBlock addr newLock newWaitQueue state

        let state =
            match wakeNextOwner with
            | None -> state
            | Some nextOwner -> Scheduler.setThreadStatus nextOwner ThreadStatus.Runnable state

        Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnSyncBlockWait (addr, deadlineTicks)) state

    /// `Monitor.Pulse`: caller must hold the lock; wakes at most one waiter
    /// from the FIFO head of `WaitQueue`. The woken waiter is moved to the
    /// FIFO tail of `AcquireQueue` carrying its snapshot depth as a
    /// `Some depth` entry; its status flips from `BlockedOnSyncBlockWait`
    /// to `BlockedOnSyncBlockAcquire`. The lock is NOT released — the
    /// caller keeps ownership until its own `Exit`.
    ///
    /// Pulse on an empty wait queue is a documented no-op (CoreCLR exact).
    let pulse (thread : ThreadId) (addr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState =
        let block = readBlock addr state

        let locked =
            match block.Lock with
            | SyncBlockLock.Free ->
                failwith
                    $"Monitor.Pulse on object %O{addr}: SyncBlock is Free but Pulse requires the caller to own the lock. The BCL would raise SynchronizationLockException."
            | SyncBlockLock.Held l when l.LockingThread <> thread ->
                failwith
                    $"Monitor.Pulse on object %O{addr}: caller %O{thread} does not own the lock (owner = %O{l.LockingThread}). The BCL would raise SynchronizationLockException."
            | SyncBlockLock.Held l -> l

        match block.WaitQueue with
        | [] ->
            // No waiter to wake. Pulse is a no-op (BCL contract).
            state
        | (waiter, depth) :: restWait ->
            let locked =
                { locked with
                    AcquireQueue = locked.AcquireQueue @ [ (waiter, Some depth) ]
                }

            state
            |> writeBlock addr (SyncBlockLock.Held locked) restWait
            |> Scheduler.setThreadStatus waiter (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

    /// `Monitor.PulseAll`: caller must hold the lock; drains the entire
    /// `WaitQueue` onto the FIFO tail of `AcquireQueue`, preserving FIFO
    /// order. Each woken waiter's status flips from `BlockedOnSyncBlockWait`
    /// to `BlockedOnSyncBlockAcquire`. The lock is NOT released. PulseAll
    /// on an empty wait queue is a no-op.
    let pulseAll (thread : ThreadId) (addr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState =
        let block = readBlock addr state

        let locked =
            match block.Lock with
            | SyncBlockLock.Free ->
                failwith
                    $"Monitor.PulseAll on object %O{addr}: SyncBlock is Free but PulseAll requires the caller to own the lock. The BCL would raise SynchronizationLockException."
            | SyncBlockLock.Held l when l.LockingThread <> thread ->
                failwith
                    $"Monitor.PulseAll on object %O{addr}: caller %O{thread} does not own the lock (owner = %O{l.LockingThread}). The BCL would raise SynchronizationLockException."
            | SyncBlockLock.Held l -> l

        match block.WaitQueue with
        | [] -> state
        | waiters ->
            let newEntries = waiters |> List.map (fun (tid, depth) -> tid, Some depth)

            let locked =
                { locked with
                    AcquireQueue = locked.AcquireQueue @ newEntries
                }

            let state = writeBlock addr (SyncBlockLock.Held locked) [] state

            waiters
            |> List.fold
                (fun acc (tid, _) ->
                    Scheduler.setThreadStatus tid (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None)) acc
                )
                state

    /// Pull `thread` out of `addr`'s `WaitQueue` and route it through the
    /// same reacquire path that `pulse` would take. If the lock is currently
    /// `Free`, the woken thread becomes the new owner directly (status
    /// flips to `Runnable`, ReentrancyCount set to its snapshot depth); if
    /// the lock is `Held`, the woken thread is parked at the AcquireQueue
    /// tail carrying its snapshot as `Some depth` (status flips to
    /// `BlockedOnSyncBlockAcquire`). In both cases the waiter eventually
    /// resumes past its `Wait` call site already owning the lock at its
    /// snapshot depth — the same shape `pulse` produces — so the QCall
    /// handler does not need to distinguish signalled from spurious wakeups.
    ///
    /// Fails loudly if `thread` is not in `addr`'s `WaitQueue`. The only
    /// caller is `applySpuriousWakeups`, which enumerates the waiters
    /// itself; a miss indicates either a script that named a stale thread
    /// (silent skip would let the script drift unnoticed) or a bug in the
    /// strategy interpreter.
    let spuriousWake (addr : ManagedHeapAddress) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let block = readBlock addr state

        let entry = block.WaitQueue |> List.tryFind (fun (t, _) -> t = thread)

        let depth =
            match entry with
            | Some (_, d) -> d
            | None ->
                failwith
                    $"SyncBlockMonitor.spuriousWake: cannot spuriously wake thread %O{thread} on object %O{addr} because it is not in WaitQueue (queue: %A{block.WaitQueue})."

        let newWaitQueue = block.WaitQueue |> List.filter (fun (t, _) -> t <> thread)

        match block.Lock with
        | SyncBlockLock.Free ->
            // Uncontended: take ownership directly with the snapshot depth restored.
            let locked =
                {
                    LockingThread = thread
                    ReentrancyCount = depth
                    AcquireQueue = []
                }

            state
            |> writeBlock addr (SyncBlockLock.Held locked) newWaitQueue
            |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

        | SyncBlockLock.Held locked ->
            // Contended: park at the AcquireQueue tail. Ownership will be
            // handed to us atomically when our predecessor releases, and
            // ReentrancyCount will be set to our snapshot depth on transfer.
            let locked =
                { locked with
                    AcquireQueue = locked.AcquireQueue @ [ (thread, Some depth) ]
                }

            state
            |> writeBlock addr (SyncBlockLock.Held locked) newWaitQueue
            |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

    /// Fire the finite-timeout wake for `thread` parked on `addr`'s
    /// `WaitQueue`. Routes the thread through the same reacquire path as
    /// `pulse`/`spuriousWake` (take ownership directly with the snapshot
    /// depth restored if the lock is Free, otherwise park at the
    /// AcquireQueue tail as `Some depth`), then rewrites the optimistic
    /// `Int32 1` (signalled) slot pushed at park time by the
    /// `Monitor_Wait` QCall handler to `Int32 0` (timed out). On
    /// resumption past the `Wait` call site the thread already re-owns
    /// the lock at its snapshot depth and observes `false` from
    /// `Monitor.Wait(obj, timeout)` — mirroring CoreCLR's contract.
    ///
    /// Fails loud if `thread` is not in `addr`'s `WaitQueue`. The only
    /// caller is `Program.fireExpiredDeadlines`, which enumerates
    /// `BlockedOnSyncBlockWait` statuses itself; a miss would mean a
    /// pulse/spurious wake raced our enumeration without flipping the
    /// status, or the deadline-firing path was reached for an untimed
    /// waiter — both indicate a structural bug worth surfacing here
    /// rather than letting the eval-stack pop misbehave silently.
    let fireWaitTimeout (thread : ThreadId) (addr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState =
        let block = readBlock addr state

        let entry = block.WaitQueue |> List.tryFind (fun (t, _) -> t = thread)

        let depth =
            match entry with
            | Some (_, d) -> d
            | None ->
                failwith
                    $"SyncBlockMonitor.fireWaitTimeout: cannot fire timeout for thread %O{thread} on object %O{addr} because it is not in WaitQueue (queue: %A{block.WaitQueue})."

        let newWaitQueue = block.WaitQueue |> List.filter (fun (t, _) -> t <> thread)

        let state =
            match block.Lock with
            | SyncBlockLock.Free ->
                // Uncontended: take ownership directly with the snapshot depth restored.
                let locked =
                    {
                        LockingThread = thread
                        ReentrancyCount = depth
                        AcquireQueue = []
                    }

                state
                |> writeBlock addr (SyncBlockLock.Held locked) newWaitQueue
                |> Scheduler.setThreadStatus thread ThreadStatus.Runnable

            | SyncBlockLock.Held locked ->
                // Contended: park at the AcquireQueue tail carrying the
                // snapshot. Ownership will be handed to us atomically when
                // our predecessor releases; ReentrancyCount is restored
                // from the `Some depth` snapshot at transfer time.
                let locked =
                    { locked with
                        AcquireQueue = locked.AcquireQueue @ [ (thread, Some depth) ]
                    }

                state
                |> writeBlock addr (SyncBlockLock.Held locked) newWaitQueue
                |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnSyncBlockAcquire (addr, None))

        // Rewrite the park-time `Int32 1` (signalled) slot pushed by the
        // Monitor_Wait handler to `Int32 0` (timed out). Pop-then-push
        // keeps the stack depth invariant across the wake.
        let _, state = IlMachineState.popEvalStack thread state
        IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) thread state

    /// Fire the finite-timeout wake for `thread` parked on `addr`'s
    /// `AcquireQueue` from a timed `Monitor.TryEnter(obj, ms)` slowpath
    /// call. Dequeues `thread` from the AcquireQueue (the lock owner does
    /// NOT change — the head of the queue at fire time is still entitled
    /// to ownership when the current owner's `Exit` lands), flips status
    /// to `Runnable`, and rewrites the optimistic `Int32 1` (acquired)
    /// slot pushed at park time by `TryEnter_Slowpath` to `Int32 0`
    /// (timed out). The BCL's `TryEnter_Slowpath` return is treated as
    /// `bool`: `Int32 0` ⇒ `false`, mirroring CoreCLR's contract.
    ///
    /// Fails loud if `thread` is not in `addr`'s `AcquireQueue`. The only
    /// caller is `Program.fireExpiredDeadlines`, which selects threads
    /// in `BlockedOnSyncBlockAcquire (_, Some _)` status; by the time
    /// we are called, the thread is still parked, so it must still be
    /// in the queue. A miss means a structural invariant has been
    /// violated (e.g. `Exit_FastPath` dequeued without flipping the
    /// status, or some other code path forgot to keep the queue and
    /// the status in sync).
    let fireAcquireTimeout (thread : ThreadId) (addr : ManagedHeapAddress) (state : IlMachineState) : IlMachineState =
        let block = readBlock addr state

        let locked =
            match block.Lock with
            | SyncBlockLock.Free ->
                failwith
                    $"SyncBlockMonitor.fireAcquireTimeout: SyncBlock for object %O{addr} is Free, but thread %O{thread} is parked in BlockedOnSyncBlockAcquire on it — the queue lives in the Held state and a parked acquirer implies Held. Structural invariant violation."
            | SyncBlockLock.Held l -> l

        if not (locked.AcquireQueue |> List.exists (fun (t, _) -> t = thread)) then
            failwith
                $"SyncBlockMonitor.fireAcquireTimeout: cannot fire timeout for thread %O{thread} on object %O{addr} because it is not in AcquireQueue (queue: %A{locked.AcquireQueue}). The deadline-firing path selected this thread based on its BlockedOnSyncBlockAcquire status; missing from the queue is a structural invariant violation."

        let newAcquireQueue = locked.AcquireQueue |> List.filter (fun (t, _) -> t <> thread)

        let locked =
            { locked with
                AcquireQueue = newAcquireQueue
            }

        let state = writeBlock addr (SyncBlockLock.Held locked) block.WaitQueue state
        let state = Scheduler.setThreadStatus thread ThreadStatus.Runnable state

        // Rewrite the park-time `Int32 1` (acquired) slot pushed by the
        // TryEnter_Slowpath handler to `Int32 0` (timed out). Pop-then-push
        // keeps the stack depth invariant across the wake.
        let _, state = IlMachineState.popEvalStack thread state
        IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) thread state

    /// SplitMix64-style hash, deriving a per-(tick, addr, thread) coin flip
    /// in `[0.0, 1.0)`. Replayability comes from the function being a pure
    /// hash with no mutable PRNG state.
    let private coinFlip (seed : uint64) (tick : int64) (ManagedHeapAddress aid) (ThreadId tid) : float =
        let mix (h : uint64) (x : uint64) : uint64 =
            let h = h ^^^ x
            h * 0x100000001B3UL

        let finalise (h : uint64) : uint64 =
            let h = h ^^^ (h >>> 33)
            let h = h * 0xff51afd7ed558ccdUL
            let h = h ^^^ (h >>> 33)
            let h = h * 0xc4ceb9fe1a85ec53UL
            h ^^^ (h >>> 33)

        let h = seed
        let h = mix h (uint64 tick)
        let h = mix h (uint64 aid)
        let h = mix h (uint64 tid)
        let h = finalise h
        float (h >>> 11) / float (1UL <<< 53)

    /// Enumerate the spurious wakeups requested by `strategy` for `tick`
    /// against the current managed-heap SyncBlock `WaitQueue` membership,
    /// then apply each `spuriousWake` in deterministic order (ascending
    /// `ManagedHeapAddress`, then FIFO position within the queue).
    /// Snapshotting the (addr, thread) list first means strategy decisions
    /// are computed against the state at entry — applying an earlier wake
    /// never affects which later wakes the strategy would have chosen on
    /// this tick.
    ///
    /// `Disabled` is the identity. `Random.probability` outside
    /// `[0.0, 1.0]` (or NaN) is a programmer error and fails loud.
    /// `Scripted` triples that name a thread not in the named SyncBlock's
    /// `WaitQueue` at the named tick fail loud — silent skip would let
    /// scripts drift when the underlying interleaving changes.
    let applySpuriousWakeups
        (strategy : SyncBlockSpuriousWakeupStrategy)
        (tick : int64)
        (state : IlMachineState)
        : IlMachineState
        =
        match strategy with
        | SyncBlockSpuriousWakeupStrategy.Disabled -> state

        | SyncBlockSpuriousWakeupStrategy.AlwaysAll ->
            ManagedHeap.syncBlockWaiters state.ManagedHeap
            |> List.fold (fun acc (addr, tid) -> spuriousWake addr tid acc) state

        | SyncBlockSpuriousWakeupStrategy.Random (seed, probability) ->
            if probability < 0.0 || probability > 1.0 || System.Double.IsNaN probability then
                failwith
                    $"SyncBlockSpuriousWakeupStrategy.Random: probability %f{probability} is outside [0.0, 1.0] (NaN or out of range)."

            ManagedHeap.syncBlockWaiters state.ManagedHeap
            |> List.filter (fun (addr, tid) -> coinFlip seed tick addr tid < probability)
            |> List.fold (fun acc (addr, tid) -> spuriousWake addr tid acc) state

        | SyncBlockSpuriousWakeupStrategy.Scripted wakeups ->
            wakeups
            |> List.filter (fun (t, _, _) -> t = tick)
            |> List.fold (fun acc (_, addr, tid) -> spuriousWake addr tid acc) state
