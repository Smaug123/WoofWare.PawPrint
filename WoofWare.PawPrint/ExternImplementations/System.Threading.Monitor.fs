namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint
open WoofWare.PosixKernel

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
    ///
    /// `deadlineTicks = None` is an infinite acquire (`Monitor.Enter(obj)` /
    /// `Monitor.TryEnter(obj, Timeout.Infinite)`); `Some ms` is a finite
    /// positive timeout from the `TryEnter_Slowpath` route, expressed as
    /// the absolute virtual-clock tick at which the timed acquire
    /// expires. If the deadline fires while still queued,
    /// `SyncBlockMonitor.fireAcquireTimeout` dequeues the thread without
    /// transferring ownership.
    let private parkOnAcquireQueue
        (addr : ManagedHeapAddress)
        (thread : ThreadId)
        (block : SyncBlock)
        (locked : LockedSyncBlock)
        (deadlineTicks : int64 option)
        (state : IlMachineState)
        : IlMachineState
        =
        let locked =
            { locked with
                AcquireQueue = locked.AcquireQueue @ [ (thread, None) ]
            }

        state
        |> writeHeld addr block.WaitQueue locked
        |> Scheduler.setThreadStatus thread (ThreadStatus.BlockedOnSyncBlockAcquire (addr, deadlineTicks))

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
                        // Infinite (no-deadline) park: a `Monitor.Enter` waiter has no timeout.
                        parkOnAcquireQueue addr currentThread block locked None state

        let state = IlMachineState.pushToEvalStack (CliType.ofBool true) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 InternalCall: Monitor.TryEnter_FastPath_WithTimeout(obj, int32) -> EnterHelperResult.
    /// The BCL wrapper at `Monitor.TryEnter(obj, ms)` (and the `ref taken` overload) treats the
    /// result as: `Entered` (1) → return true; `Contention` (0) AND `ms == 0` → return false;
    /// anything else falls through to the `Monitor_TryEnter_Slowpath` QCall. So the `Contention`
    /// → "return false" shortcut only applies for `timeout = 0`; positive finite timeouts must
    /// either return `Entered` outright or `UseSlowPath` and let the slowpath park.
    ///
    /// Behaviour by branch:
    /// * Free or self-held → claim/reenter, push `Entered` (1).
    /// * Contended, `timeout = 0` → push `Contention` (0); BCL shortcircuits to `false`.
    /// * Contended, `timeout = -1` (Infinite) → park at the FIFO tail of `AcquireQueue` with
    ///   no deadline and push `Entered` (1). When ownership is later transferred to us, the
    ///   IL pointer is already past this call site and observes `Entered` ⇒ the BCL returns
    ///   `true` without ever calling the slowpath. This collapses the infinite-blocking path
    ///   into the fast-path handler — PawPrint never needs the slowpath for infinite timeouts.
    /// * Contended, `timeout > 0` → push `UseSlowPath` (2) and do NOT park; the BCL's
    ///   wrapper falls through to `Monitor_TryEnter_Slowpath`, which does the timed park.
    ///   Pushing `Entered` here is unsound: it would commit us to "lock acquired" before
    ///   the deadline fires, and there is no value we can rewrite the eval-stack slot to
    ///   that would make the BCL return `false` directly (any non-`Entered`-with-non-zero-
    ///   timeout result routes through the slowpath).
    /// * Contended, `timeout < -1` → fail loud; the BCL's wrapper gates the QCall with
    ///   `millisecondsTimeout >= -1`, so reaching here means the wrapper was bypassed.
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
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
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
                        // BCL shortcircuits Contention + ms=0 to `return false`.
                        0, state
                    elif timeout = System.Threading.Timeout.Infinite then
                        // Blocking acquire: park at the FIFO tail (no deadline)
                        // and push `Entered` — when the scheduler resumes us, ownership
                        // has been transferred and the IL pointer is already past this
                        // call site. The BCL sees `Entered` and skips the slowpath.
                        let state = parkOnAcquireQueue addr currentThread block locked None state
                        1, state
                    elif timeout > 0 then
                        // Positive finite timeout: push `UseSlowPath` (2) without
                        // parking. The BCL wrapper falls through to
                        // `Monitor_TryEnter_Slowpath`, which does the timed park.
                        2, state
                    else
                        // `timeout < -1`. The BCL's `Monitor.TryEnter(obj, ms)` wrapper
                        // gates the QCall behind `millisecondsTimeout >= -1`, so
                        // reaching here means the wrapper was bypassed. Silently
                        // treating it as Infinite or zero would hide guest bugs.
                        failwith
                            $"Monitor.TryEnter_FastPath_WithTimeout: negative timeout %d{timeout} ms is not Infinite (-1); the BCL's TryEnter(obj, int) wrapper validates this before the QCall, so reaching here means the wrapper was bypassed."

        let state =
            IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 result)) currentThread state

        (state, WhatWeDid.Executed) |> ExecutionResult.stepped

    /// .NET 10 QCall: `Monitor_TryEnter_Slowpath(ObjectHandleOnStack obj, int millisecondsTimeout) -> int`.
    /// The BCL wrapper treats the int return as bool (`0 = false`, anything else = true). Reached only
    /// when the fast-path `TryEnter_FastPath_WithTimeout` returned `UseSlowPath` (2) — i.e. the lock was
    /// contended with a positive finite timeout. We park the caller on the SyncBlock's `AcquireQueue`
    /// with `Some deadline`, push optimistic `Int32 1` (acquired), and let one of two things resolve:
    ///   * Ownership transfer (current owner's `Exit_FastPath` dequeues this thread): wakes Runnable
    ///     with `Int32 1` on the stack — caller observes `true`.
    ///   * Deadline fire (`SyncBlockMonitor.fireAcquireTimeout`): dequeues without transferring ownership
    ///     and rewrites the optimistic `Int32 1` to `Int32 0` — caller observes `false`.
    ///
    /// `timeout = 0` and `timeout = -1` are unreachable from a compliant BCL caller because the
    /// fast-path resolves both before falling through. `timeout < -1` is also wrapper-validated.
    /// All three fail loud here.
    let TryEnter_Slowpath
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (addr : ManagedHeapAddress)
        (timeout : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let block = IlMachineState.getSyncBlock addr state

        let state =
            match block.Lock with
            | SyncBlockLock.Free ->
                // Fast-path would have answered `Entered` and BCL would never reach the
                // slowpath, but the BCL wrapper unconditionally calls the slowpath after
                // any non-`Entered` fast-path result, so a free-but-just-released-by-the-
                // moment-the-slowpath-ran race is conceivable. Be robust: claim the lock.
                let locked =
                    {
                        LockingThread = currentThread
                        ReentrancyCount = 1
                        AcquireQueue = []
                    }

                writeHeld addr block.WaitQueue locked state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) currentThread
            | SyncBlockLock.Held locked ->
                if locked.LockingThread = currentThread then
                    // Reentrant: bump depth and report acquired. Mirrors the fast-path
                    // self-held branch.
                    let locked =
                        { locked with
                            ReentrancyCount = locked.ReentrancyCount + 1
                        }

                    writeHeld addr block.WaitQueue locked state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) currentThread
                elif timeout = 0 then
                    failwith
                        $"Monitor_TryEnter_Slowpath: unexpected timeout = 0 — the fast-path resolves Contention + ms=0 to `return false` without calling the slowpath; reaching here means the BCL wrapper was bypassed."
                elif timeout = System.Threading.Timeout.Infinite then
                    failwith
                        $"Monitor_TryEnter_Slowpath: unexpected timeout = -1 (Infinite) — the fast-path parks-and-pushes Entered on infinite contention, so the BCL wrapper observes Entered and never falls through to the slowpath; reaching here means the wrapper was bypassed."
                elif timeout < 0 then
                    failwith
                        $"Monitor_TryEnter_Slowpath: negative timeout %d{timeout} ms is not Infinite (-1); the BCL's TryEnter(obj, int) wrapper validates this before the QCall, so reaching here means the wrapper was bypassed."
                else
                    // Positive finite timeout, contended. Park with `Some deadline`
                    // and push optimistic `Int32 1` (acquired). Resolution happens
                    // via either ownership transfer (Exit_FastPath head dequeue) or
                    // deadline fire (`SyncBlockMonitor.fireAcquireTimeout` rewrites
                    // to `Int32 0`).
                    let deadlineTicks =
                        state.Kernel.VirtualClockTicks
                        + int64 timeout * UnixMachineState.ticksPerMillisecond

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) currentThread
                    |> parkOnAcquireQueue addr currentThread block locked (Some deadlineTicks)

        state

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
