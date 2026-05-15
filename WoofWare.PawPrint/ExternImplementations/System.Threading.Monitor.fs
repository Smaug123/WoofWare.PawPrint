namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint

[<RequireQualifiedAccess>]
module System_Threading_Monitor =
    /// EnterHelperResult enum (nested in System.Threading.Monitor): Contention=0, Entered=1, UseSlowPath=2.
    /// LeaveHelperAction enum (nested in System.Threading.Monitor): None=0, Signal=1, Yield=2, Contention=3, Error=4.

    let private popOneObject (currentThread : ThreadId) (argIndex : int) (state : IlMachineState) =
        state
        |> IlMachineState.loadArgument currentThread argIndex
        |> IlMachineState.popEvalStack currentThread

    let private popInt32 (currentThread : ThreadId) (argIndex : int) (state : IlMachineState) =
        state
        |> IlMachineState.loadArgument currentThread argIndex
        |> IlMachineState.popEvalStack currentThread

    /// Write back a SyncBlock whose `Lock` portion is freshly held by `owner`
    /// at `depth`, preserving the existing `WaitQueue` and accepting a fresh
    /// `AcquireQueue`. Used by the ownership-transfer paths in `Exit_FastPath`
    /// (and re-used by `SyncBlockMonitor.pulse` indirectly via the same shape).
    let private writeHeld
        (addr : ManagedHeapAddress)
        (waitQueue : (ThreadId * int) list)
        (locked : LockedSyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        IlMachineState.setSyncBlock
            addr
            {
                Lock = SyncBlockLock.Held locked
                WaitQueue = waitQueue
            }
            state

    /// Park `thread` at the FIFO tail of `addr`'s AcquireQueue as a fresh entrant
    /// (`None` snapshot — first `Enter` since `Free`, or a thread resumed from
    /// `Monitor.Wait` would supply `Some prior-depth` via `SyncBlockMonitor.pulse`).
    /// Flips status to `BlockedOnSyncBlockAcquire`. Assumes `addr`'s SyncBlock is
    /// `Held` by some other thread (the caller has already checked self-vs-other).
    /// When the owner's `Monitor.Exit` decrements the reentrancy count to zero,
    /// ownership is handed directly to the FIFO head and the head flips back to
    /// `Runnable` already holding the lock — mirroring `LowLevelMonitor`'s
    /// ownership-transfer model so the IL after the `Enter` call site is correctly
    /// held.
    let private parkOnAcquireQueue
        (addr : ManagedHeapAddress)
        (thread : ThreadId)
        (block : SyncBlock)
        (locked : LockedSyncBlock)
        (state : IlMachineState)
        : IlMachineState
        =
        let locked =
            { locked with
                AcquireQueue = locked.AcquireQueue @ [ (thread, None) ]
            }

        state
        |> writeHeld addr block.WaitQueue locked
        |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnSyncBlockAcquire addr)

    /// .NET 10 InternalCall: Monitor.TryEnter_FastPath(obj) -> bool.
    /// Backs `Monitor.Enter(obj)`: the BCL's IL is "if (!TryEnter_FastPath(obj)) Enter_Slowpath(obj)",
    /// so returning false here routes the caller through the (unimplemented) `Enter_Slowpath` QCall.
    /// PawPrint can answer Free / SelfHeld / OtherHeld directly from the SyncBlock and we know
    /// contention precisely, so we collapse Enter's blocking semantics into this handler:
    /// on contention we park the caller in `BlockedOnSyncBlockAcquire` at the FIFO tail of the
    /// SyncBlock's `AcquireQueue` and push `true` — when the lock owner's Exit transfers
    /// ownership to us the resumed thread's IL is already past this call site and observes
    /// `true`, mirroring the `LowLevelMonitor` ownership-transfer model.
    let TryEnter_FastPath
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state

        let state =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None -> failwith "TODO: Monitor.TryEnter_FastPath should throw ArgumentNullException for null obj"
            | Some addr ->
                let block = IlMachineState.getSyncBlock addr state

                match block.Lock with
                | SyncBlockLock.Free ->
                    let locked =
                        {
                            LockingThread = currentThread
                            ReentrancyCount = 1
                            AcquireQueue = []
                        }

                    writeHeld addr block.WaitQueue locked state
                | SyncBlockLock.Held locked ->
                    if locked.LockingThread = currentThread then
                        let locked =
                            { locked with
                                ReentrancyCount = locked.ReentrancyCount + 1
                            }

                        writeHeld addr block.WaitQueue locked state
                    else
                        // Locked by another thread: park at the FIFO tail of the AcquireQueue.
                        // When ownership is transferred to us by the owner's Exit, the resumed
                        // thread's IL is already past this call site and observes `true`, so the
                        // BCL's `if (!TryEnter_FastPath(obj)) Enter_Slowpath(obj)` skips the
                        // Slowpath. This collapses Monitor.Enter's blocking semantics into the
                        // fast-path handler — PawPrint never needs the Enter_Slowpath QCall.
                        parkOnAcquireQueue addr currentThread block locked state

        let state = IlMachineState.pushToEvalStack (CliType.ofBool true) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.TryEnter_FastPath_WithTimeout(obj, int32) -> EnterHelperResult.
    /// Caller treats the result as: 0 (Contention) → return false; 1 (Entered) → return true;
    /// 2 (UseSlowPath) → call Monitor.TryEnter_Slowpath. We never need the slowpath because
    /// PawPrint can answer Free / SelfHeld / OtherHeld directly from the SyncBlock.
    ///
    /// Contention with a non-zero timeout parks the caller in `BlockedOnSyncBlockAcquire`
    /// and pushes `1` (Entered) onto the stack: the IL pointer advances past this call
    /// site as part of returning `Executed`, and when ownership is later transferred to
    /// us the resumed thread already holds the lock. Finite non-zero timeouts fail loud
    /// because PawPrint has no virtual clock yet — `LowLevelMonitor.timedWait` makes the
    /// same call and we keep both posture-consistent.
    let TryEnter_FastPath_WithTimeout
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state
        let timeoutVal, state = popInt32 currentThread 1 state

        let timeout =
            match timeoutVal with
            | EvalStackValue.Int32 i -> i
            | other -> failwith $"Monitor.TryEnter_FastPath_WithTimeout: expected int32 timeout, got %O{other}"

        let result, state =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None ->
                failwith "TODO: Monitor.TryEnter_FastPath_WithTimeout should throw ArgumentNullException for null obj"
            | Some addr ->
                let block = IlMachineState.getSyncBlock addr state

                match block.Lock with
                | SyncBlockLock.Free ->
                    let locked =
                        {
                            LockingThread = currentThread
                            ReentrancyCount = 1
                            AcquireQueue = []
                        }

                    let state = writeHeld addr block.WaitQueue locked state
                    1, state
                | SyncBlockLock.Held locked ->
                    if locked.LockingThread = currentThread then
                        let locked =
                            { locked with
                                ReentrancyCount = locked.ReentrancyCount + 1
                            }

                        let state = writeHeld addr block.WaitQueue locked state
                        1, state
                    elif timeout = 0 then
                        // Non-blocking poll: report contention without waiting.
                        0, state
                    elif timeout = System.Threading.Timeout.Infinite then
                        // Blocking acquire: park at the FIFO tail and push "Entered" —
                        // when the scheduler resumes us, ownership has been transferred
                        // and the IL pointer is already past this call site.
                        let state = parkOnAcquireQueue addr currentThread block locked state
                        1, state
                    else
                        // Finite non-zero timeout would require a virtual clock to honour;
                        // silently treating it as Infinite would hide guest bugs. Same
                        // envelope as `LowLevelMonitor.timedWait`.
                        failwith
                            $"TODO: Monitor.TryEnter_FastPath_WithTimeout with finite non-zero timeout %d{timeout}ms requires a virtual clock; not yet implemented"

        let state =
            IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 result)) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.IsEnteredNative(obj) -> bool.
    /// Returns true if the SyncBlock for `obj` is held by the current thread.
    let IsEnteredNative
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state

        let result =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None -> failwith "TODO: Monitor.IsEnteredNative should throw ArgumentNullException for null obj"
            | Some addr ->
                match (IlMachineState.getSyncBlock addr state).Lock with
                | SyncBlockLock.Free -> false
                | SyncBlockLock.Held locked -> locked.LockingThread = currentThread

        let state =
            IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.Exit_FastPath(obj) -> LeaveHelperAction.
    /// LeaveHelperAction.None (0) means the unlock fully succeeded and IL skips the slowpath;
    /// any non-zero value (Signal/Yield/Contention/Error) routes the IL through Exit_Slowpath.
    /// PawPrint can decrement the SyncBlock directly, so we always return None on success and
    /// fail loud if the unlock would have surfaced as Error in the real runtime.
    ///
    /// When the final `Exit` releases the lock and the `AcquireQueue` is non-empty, ownership
    /// is transferred directly to the FIFO head: that thread's status flips back to `Runnable`
    /// already holding the lock with `ReentrancyCount = 1` for fresh entrants (`None`
    /// snapshot) or with its prior depth restored (`Some depth` snapshot, set when the head
    /// was woken from `Monitor.Wait`). Mirrors `LowLevelMonitor`'s ownership-transfer model.
    /// `WaitQueue` is preserved across every transition: waiters do not contend for the lock
    /// until `Pulse` / `PulseAll` moves them onto `AcquireQueue`, and a fully-released lock
    /// with a non-empty wait queue is a legitimate state (the SyncBlock stays present, just
    /// with `Lock = Free`).
    let Exit_FastPath
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ExecutionResult
        =
        let lockObj, state = popOneObject currentThread 0 state

        let state =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state lockObj with
            | None -> failwith "TODO: Monitor.Exit_FastPath should throw ArgumentNullException for null obj"
            | Some addr ->
                let block = IlMachineState.getSyncBlock addr state

                match block.Lock with
                | SyncBlockLock.Free ->
                    failwith "TODO: Monitor.Exit_FastPath on a Free SyncBlock should throw SynchronizationLockException"
                | SyncBlockLock.Held locked ->
                    if locked.LockingThread <> currentThread then
                        failwith
                            "TODO: Monitor.Exit_FastPath by a non-owning thread should throw SynchronizationLockException"
                    elif locked.ReentrancyCount > 1 then
                        let locked =
                            { locked with
                                ReentrancyCount = locked.ReentrancyCount - 1
                            }

                        writeHeld addr block.WaitQueue locked state
                    else
                        // Last release. If anyone is queued for Enter, ownership transfers atomically
                        // to the FIFO head; otherwise the lock becomes Free (but the SyncBlock
                        // record persists so the WaitQueue is preserved across the transition).
                        match locked.AcquireQueue with
                        | [] ->
                            IlMachineState.setSyncBlock
                                addr
                                {
                                    Lock = SyncBlockLock.Free
                                    WaitQueue = block.WaitQueue
                                }
                                state
                        | (nextOwner, snapshot) :: rest ->
                            // `None` snapshot = fresh entrant from Monitor.Enter (ReentrancyCount = 1).
                            // `Some depth` = waiter resumed from Monitor.Wait, restored to its prior depth.
                            let restoredDepth = snapshot |> Option.defaultValue 1

                            let nextLocked =
                                {
                                    LockingThread = nextOwner
                                    ReentrancyCount = restoredDepth
                                    AcquireQueue = rest
                                }

                            state
                            |> writeHeld addr block.WaitQueue nextLocked
                            |> Scheduler.setThreadStatus nextOwner ThreadStatus.Runnable

        // LeaveHelperAction.None = 0 — caller's IL takes the early Ret branch.
        let state =
            IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped
