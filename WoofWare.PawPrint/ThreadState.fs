namespace WoofWare.PawPrint

open System.Reflection

/// Scheduling status of a thread. The scheduler only picks Runnable threads; a thread in any
/// other state is paused until something external (another thread terminating, for instance)
/// flips it back to Runnable.
type ThreadStatus =
    | Runnable
    /// The managed `Thread` heap object has been constructed (its `Initialize`
    /// QCall / InternalCall ran, allocating a `ThreadId` slot for it) but
    /// `Thread.Start` has not yet been called, so no IL has executed on this
    /// thread. The scheduler never picks `NotStarted` threads; `StartInternal`
    /// flips the status to `Runnable` after populating the bottom frame with
    /// the user's delegate target. Lets us model state that lives on the
    /// managed `Thread` object before its first execution (notably
    /// `IsBackground`, which the thread-pool worker setup writes between the
    /// constructor call and `Start`) by keeping the per-thread record alive
    /// from construction time. Threads that are constructed and never started
    /// (e.g. a `new Thread(...)` reference that the guest then drops on the
    /// floor) remain `NotStarted` for the lifetime of the run; they do not
    /// contribute to deadlock detection beyond appearing in the stuck-threads
    /// description.
    | NotStarted
    /// This thread is blocked inside `Thread.Join`, waiting for the named
    /// thread to terminate. The wake comes from
    /// `Scheduler.onThreadTerminated`, which sweeps every
    /// `BlockedOnJoin (target, _)` whose `target` matches the terminating
    /// thread and flips them back to `Runnable`.
    ///
    /// `deadlineMs = None` is an infinite wait (`Thread.Join()` /
    /// `Thread.Join(-1)` / `Timeout.Infinite`); `Some ms` is a finite
    /// timeout (`Thread.Join(int)` with a non-zero positive value),
    /// expressed as the absolute virtual-clock millisecond at which the
    /// wait expires. When `VirtualClockMs` advances past the deadline and
    /// the thread is still parked, the driver fires a timeout wake
    /// (`Scheduler.fireJoinTimeout`): the optimistic `Int32 1` slot
    /// pushed at park time by the Join handler is rewritten to `Int32 0`
    /// (so the BCL's `Join(int)` returns `false`), and the status flips
    /// back to `Runnable`. Storing the deadline in the status itself
    /// rather than in a parallel map makes the invariant "no deadline
    /// once Runnable again" structural — a wake naturally forgets it.
    /// `Thread.Join(0)` is a non-blocking poll handled at the call site
    /// (no `BlockedOnJoin` transition); only `> 0` finite timeouts and
    /// `-1` infinite reach this variant.
    | BlockedOnJoin of target : ThreadId * deadlineMs : int64 option
    /// This thread tried to access a type whose .cctor is currently being run by another
    /// thread. Per ECMA-335 II.10.5.3.3 it must wait for that thread to finish initialising
    /// the type before it can proceed.
    | BlockedOnClassInit of blocker : ThreadId
    /// This thread called `SystemNative_LowLevelMonitor_Acquire` (or transitioned out
    /// of `BlockedOnMonitorWait` and is now waiting to reacquire) on a monitor whose
    /// owner is another thread. The scheduler unblocks at most one such thread when
    /// the owner calls `Release` (or `Signal_Release`); FIFO order over the acquire
    /// queue is load-bearing for fairness of higher-level locks built on top of this
    /// primitive (e.g. `LowLevelLock`).
    | BlockedOnMonitorAcquire of monitor : LowLevelMonitorId
    /// This thread called `SystemNative_LowLevelMonitor_Wait` (infinite
    /// timeout) or `SystemNative_LowLevelMonitor_TimedWait` (finite
    /// timeout) and is sitting on the monitor's wait queue with the
    /// monitor temporarily released. A subsequent `Signal_Release` from
    /// another thread transitions the head of the wait queue to
    /// `BlockedOnMonitorAcquire`; reacquisition then runs through the
    /// normal acquire path.
    ///
    /// `deadlineMs = None` is an infinite wait; `Some ms` is a finite
    /// timeout, expressed as the absolute virtual-clock millisecond at
    /// which the wait expires. When `VirtualClockMs` advances to that
    /// point and the thread is still parked, the driver fires a timeout
    /// wake (`LowLevelMonitor.fireTimeout`): the thread is dequeued from
    /// the monitor's `WaitQueue`, moved to the `AcquireQueue` tail (or
    /// granted ownership directly if the monitor is unowned), and the
    /// `Int32 1` optimistic-signalled slot pushed at park time is
    /// rewritten to `Int32 0` so the BCL's `TimedWait` returns `false`.
    /// Storing the deadline in the status itself (rather than alongside
    /// in a separate map) makes "no deadline once the wake has fired"
    /// structural — the new status carries no deadline field.
    | BlockedOnMonitorWait of monitor : LowLevelMonitorId * deadlineMs : int64 option
    /// This thread called `Monitor.Enter` (or its `TryEnter` cousin with a non-zero
    /// timeout) on an object whose SyncBlock is `Held` by a different thread, and
    /// is parked at the SyncBlock's `AcquireQueue`. The lock owner's eventual
    /// `Monitor.Exit` transfers ownership directly to the FIFO head of the queue,
    /// flipping that thread back to `Runnable` already holding the lock — mirroring
    /// the `LowLevelMonitor` ownership-transfer model so the resumed thread's IL
    /// continues past the `Enter` call site already owning the SyncBlock.
    | BlockedOnSyncBlockAcquire of lockObject : ManagedHeapAddress
    /// This thread called `Monitor.Wait` on an object's SyncBlock and is parked at
    /// the SyncBlock's `WaitQueue` with the lock fully released. A subsequent
    /// `Monitor.Pulse` / `PulseAll` from another thread (or a spurious wake)
    /// transitions the head of the wait queue onto the SyncBlock's `AcquireQueue`
    /// carrying its prior reentrancy depth as a `Some depth` snapshot; the resumed
    /// thread becomes `BlockedOnSyncBlockAcquire` until the current owner's `Exit`
    /// hands ownership over, at which point its `ReentrancyCount` is restored to
    /// the snapshotted depth and the IL resumes past the `Wait` call site already
    /// re-owning the lock. Parallel with `BlockedOnMonitorWait` but for managed
    /// SyncBlocks rather than `LowLevelMonitor`.
    ///
    /// `deadlineMs = None` is an infinite wait (`Monitor.Wait(obj)` / managed
    /// `Timeout.Infinite`); `Some ms` is a finite timeout
    /// (`Monitor.Wait(obj, ms)`), expressed as the absolute virtual-clock
    /// millisecond at which the wait expires. When `VirtualClockMs` advances
    /// past the deadline and the thread is still parked, the driver fires a
    /// timeout wake (`SyncBlockMonitor.fireTimeout`): the thread is dequeued
    /// from the SyncBlock's `WaitQueue`, routed through the same reacquire
    /// path that `pulse`/`spuriousWake` use (carrying the snapshot depth into
    /// the new owner/AcquireQueue entry), and the optimistic `Int32 1`
    /// (signalled) slot pushed at park time is rewritten to `Int32 0`
    /// (timed out). Storing the deadline in the status itself rather than
    /// in a parallel map makes the invariant "no deadline once Runnable
    /// again" structural — a wake naturally forgets it.
    | BlockedOnSyncBlockWait of lockObject : ManagedHeapAddress * deadlineMs : int64 option
    /// This thread called `WaitHandle.WaitOne` (via the `WaitHandle_WaitOneCore`
    /// QCall) on a wait handle whose count was zero / unsignalled, and is
    /// parked at the handle's FIFO `WaitQueue`. A subsequent state change that
    /// produces a wake (semaphore `Release`, event `Set`, mutex unlock) flips
    /// the head of the queue back to `Runnable`; the IL `WaitOne` call site
    /// has already advanced past itself, so when the scheduler picks the woken
    /// thread it resumes with `WAIT_OBJECT_0` already on the evaluation stack.
    /// Single-handle blocking only; multi-handle wait will need a separate
    /// variant carrying a list plus a wait-all/wait-any flag.
    ///
    /// `deadlineMs = None` is an infinite wait (Win32 `INFINITE` / managed
    /// `Timeout.Infinite`); `Some ms` is a finite timeout, expressed as the
    /// absolute virtual-clock millisecond at which the wait expires. When
    /// `VirtualClockMs` advances to that point and the thread is still
    /// parked, the driver fires a timeout wake (`WaitHandle.fireTimeout`):
    /// the thread is dequeued from the handle's `WaitQueue`, the
    /// `WAIT_OBJECT_0` slot pushed at park time is rewritten to
    /// `WAIT_TIMEOUT`, and the status flips back to `Runnable`. Storing the
    /// deadline in the status itself (rather than alongside in a separate
    /// map) makes the invariant "no deadline once Runnable again" structural
    /// — a wake naturally forgets it.
    | BlockedOnWaitHandle of handle : WaitHandleId * deadlineMs : int64 option
    /// This thread has executed its final `ret`; it will never run again. Its state is kept
    /// only so other threads can observe termination (e.g. to satisfy Join).
    | Terminated
    /// PawPrint-internal auxiliary thread: it exists for kernel-side
    /// bookkeeping rather than to run guest IL, and the scheduler never
    /// picks it. The dispatcher thread spawned by
    /// `SystemNative_InitializeTerminalAndSignalHandling` is the current
    /// (and only) inhabitant: mirrors real CoreCLR's `SignalHandlerLoop`
    /// pthread, which the runtime owns and the guest never names.
    ///
    /// The semantic difference from `NotStarted` is that no managed
    /// `Thread` heap object backs a `Parked` thread — there is no
    /// `Thread.Start` call that will ever fire to flip it to `Runnable`.
    /// A future slice that wires signal-dispatch will introduce an
    /// explicit transition out of `Parked` (driven by the signal
    /// subsystem, not by guest IL); for now `Parked` is permanent for
    /// the run.
    ///
    /// Permanently-`Parked` threads do not cause spurious deadlock
    /// detection because the driver short-circuits to `NormalExit` as
    /// soon as the entry thread terminates; the scheduler is never
    /// asked to find another `Runnable` thread after that point.
    | Parked

[<RequireQualifiedAccess>]
module ThreadStatus =
    /// True iff the thread's `ActiveMethodState` references no live frame
    /// — i.e. the sentinel `FrameId -1` set up by `allocateUnstartedThread`
    /// / `allocateParkedThread` is still in place. Callers reading
    /// `threadState.MethodState` / `ActiveAssembly` / `ActiveMethodState`
    /// must check this first to avoid dereferencing the sentinel and
    /// crashing.
    ///
    /// `Terminated` is *not* frame-less: a terminating thread keeps its
    /// final frames around so other threads can observe state for Join
    /// (and so the debugger can show what was running when the thread
    /// ended). The set is therefore exactly `NotStarted` and `Parked`,
    /// the two states a thread enters before any IL has executed on it.
    ///
    /// Implemented as a fully-enumerated match (not `| _ -> false`) so a
    /// new frameless `ThreadStatus` variant fires an exhaustiveness
    /// error here instead of silently masking bugs in the dozens of
    /// callers that read frame data behind this guard.
    let hasNoActiveFrame (status : ThreadStatus) : bool =
        match status with
        | ThreadStatus.NotStarted -> true
        | ThreadStatus.Parked -> true
        | ThreadStatus.Runnable -> false
        | ThreadStatus.Terminated -> false
        | ThreadStatus.BlockedOnJoin _ -> false
        | ThreadStatus.BlockedOnClassInit _ -> false
        | ThreadStatus.BlockedOnMonitorAcquire _ -> false
        | ThreadStatus.BlockedOnMonitorWait _ -> false
        | ThreadStatus.BlockedOnSyncBlockAcquire _ -> false
        | ThreadStatus.BlockedOnSyncBlockWait _ -> false
        | ThreadStatus.BlockedOnWaitHandle _ -> false

type ThreadState =
    {
        // TODO: thread-local storage, synchronisation state, exception handling context
        MethodStates : Map<FrameId, MethodState>
        NextFrameId : int
        ActiveMethodState : FrameId
        Status : ThreadStatus
        /// Mirrors the CoreCLR `Thread.IsBackground` flag set via the
        /// `ThreadNative_SetIsBackground` QCall. The interpreter does not yet
        /// model the "process terminates when the last foreground thread
        /// exits" semantics; this field exists so the QCall can store the
        /// guest's request faithfully and the paired getter can return it,
        /// preserving round-trip semantics for guest code that reads back
        /// `Thread.IsBackground`. Default `false` matches the BCL.
        IsBackground : bool
        /// Diagnostic mirror of the thread's name, populated when the guest
        /// invokes the `ThreadNative_InformThreadNameChange` QCall (i.e. via
        /// the managed `Thread.Name` setter). The canonical name lives in the
        /// managed `Thread._name` field; the BCL getter reads that field
        /// directly without consulting us, so this mirror is *not* an
        /// authoritative source for guest reads. It exists so PawPrint's
        /// debugger, tracing, and snapshot tooling can surface a thread's
        /// name without walking heap fields. Reflection-based writes to
        /// `_name` would not update this mirror, but such drift is invisible
        /// to guests because they read `_name`, not this field. `None` means
        /// the guest has either never set the name or has cleared it.
        Name : string option
    }

    // --- Frame resolution primitives ---

    static member getFrame (frameId : FrameId) (s : ThreadState) : MethodState =
        match s.MethodStates |> Map.tryFind frameId with
        | Some frame -> frame
        | None -> failwith $"Frame %O{frameId} is not live in this thread"

    static member setFrame (frameId : FrameId) (frame : MethodState) (s : ThreadState) : ThreadState =
        if not (s.MethodStates |> Map.containsKey frameId) then
            failwith $"Cannot update frame %O{frameId} because it is not live in this thread"

        { s with
            MethodStates = s.MethodStates |> Map.add frameId frame
        }

    static member mapFrame (frameId : FrameId) (f : MethodState -> MethodState) (s : ThreadState) : ThreadState =
        ThreadState.setFrame frameId (f (ThreadState.getFrame frameId s)) s

    static member appendFrame (frame : MethodState) (s : ThreadState) : FrameId * ThreadState =
        let newId = FrameId s.NextFrameId

        let s =
            { s with
                NextFrameId = s.NextFrameId + 1
                MethodStates = s.MethodStates |> Map.add newId frame
            }

        newId, s

    static member removeFrame (frameId : FrameId) (s : ThreadState) : ThreadState =
        if frameId = s.ActiveMethodState then
            failwith $"Cannot remove active frame %O{frameId}; switch active frames first"

        if not (s.MethodStates |> Map.containsKey frameId) then
            failwith $"Cannot remove frame %O{frameId} because it is not live in this thread"

        { s with
            MethodStates = s.MethodStates |> Map.remove frameId
        }

    static member setActiveFrame (frameId : FrameId) (s : ThreadState) : ThreadState =
        if not (s.MethodStates |> Map.containsKey frameId) then
            failwith $"Cannot make frame %O{frameId} active because it is not live in this thread"

        { s with
            ActiveMethodState = frameId
        }

    static member replaceFrames (methodState : MethodState) (s : ThreadState) : ThreadState =
        let newId = FrameId s.NextFrameId

        { s with
            ActiveMethodState = newId
            MethodStates = Map.empty |> Map.add newId methodState
            NextFrameId = s.NextFrameId + 1
        }

    // --- Derived operations (implemented via the primitives above) ---

    member this.MethodState : MethodState =
        ThreadState.getFrame this.ActiveMethodState this

    member this.ActiveAssembly : AssemblyName =
        this.MethodState.ExecutingMethod.DeclaringType.Assembly

    member this.LiveFrameCount : int = this.MethodStates.Count

    static member New (methodState : MethodState) =
        {
            ActiveMethodState = FrameId 0
            MethodStates = Map.empty |> Map.add (FrameId 0) methodState
            NextFrameId = 1
            Status = ThreadStatus.Runnable
            IsBackground = false
            Name = None
        }

    static member peekEvalStack (state : ThreadState) : EvalStackValue option =
        MethodState.peekEvalStack (ThreadState.getFrame state.ActiveMethodState state)

    static member popFromEvalStack (state : ThreadState) : EvalStackValue * ThreadState =
        let activeFrame = ThreadState.getFrame state.ActiveMethodState state
        let ret, popped = activeFrame |> MethodState.popFromStack
        let state = ThreadState.setFrame state.ActiveMethodState popped state
        ret, state

    static member pushToEvalStack (o : CliType) (frameId : FrameId) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame frameId (MethodState.pushToEvalStack o) state

    static member pushToEvalStack' (e : EvalStackValue) (frameId : FrameId) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame frameId (MethodState.pushToEvalStack' e) state

    static member jumpProgramCounter (bytes : int) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame state.ActiveMethodState (MethodState.jumpProgramCounter bytes) state

    static member advanceProgramCounter (state : ThreadState) : ThreadState =
        ThreadState.mapFrame state.ActiveMethodState MethodState.advanceProgramCounter state

    static member loadArgument (i : int) (state : ThreadState) : ThreadState =
        ThreadState.mapFrame state.ActiveMethodState (MethodState.loadArgument i) state

    static member setLocalVariable
        (frameId : FrameId)
        (localVariable : uint16)
        (value : CliType)
        (s : ThreadState)
        : ThreadState
        =
        ThreadState.mapFrame
            frameId
            (fun frame ->
                { frame with
                    LocalVariables = frame.LocalVariables.SetItem (int<uint16> localVariable, value)
                }
            )
            s

    static member setArgument
        (frameId : FrameId)
        (argument : uint16)
        (value : CliType)
        (s : ThreadState)
        : ThreadState
        =
        ThreadState.mapFrame
            frameId
            (fun frame ->
                { frame with
                    Arguments = frame.Arguments.SetItem (int<uint16> argument, value)
                }
            )
            s
