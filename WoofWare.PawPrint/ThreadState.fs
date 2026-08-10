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
    /// `deadlineTicks = None` is an infinite wait (`Thread.Join()` /
    /// `Thread.Join(-1)` / `Timeout.Infinite`); `Some ms` is a finite
    /// timeout (`Thread.Join(int)` with a non-zero positive value),
    /// expressed as the absolute virtual-clock tick at which the
    /// wait expires. When `VirtualClockTicks` advances past the deadline and
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
    | BlockedOnJoin of target : ThreadId * deadlineTicks : int64 option
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
    /// `deadlineTicks = None` is an infinite wait; `Some ms` is a finite
    /// timeout, expressed as the absolute virtual-clock tick at
    /// which the wait expires. When `VirtualClockTicks` advances to that
    /// point and the thread is still parked, the driver fires a timeout
    /// wake (`LowLevelMonitor.fireTimeout`): the thread is dequeued from
    /// the monitor's `WaitQueue`, moved to the `AcquireQueue` tail (or
    /// granted ownership directly if the monitor is unowned), and the
    /// `Int32 1` optimistic-signalled slot pushed at park time is
    /// rewritten to `Int32 0` so the BCL's `TimedWait` returns `false`.
    /// Storing the deadline in the status itself (rather than alongside
    /// in a separate map) makes "no deadline once the wake has fired"
    /// structural — the new status carries no deadline field.
    | BlockedOnMonitorWait of monitor : LowLevelMonitorId * deadlineTicks : int64 option
    /// This thread called `Monitor.Enter` (or its `TryEnter` cousin with a non-zero
    /// timeout) on an object whose SyncBlock is `Held` by a different thread, and
    /// is parked at the SyncBlock's `AcquireQueue`. The lock owner's eventual
    /// `Monitor.Exit` transfers ownership directly to the FIFO head of the queue,
    /// flipping that thread back to `Runnable` already holding the lock — mirroring
    /// the `LowLevelMonitor` ownership-transfer model so the resumed thread's IL
    /// continues past the `Enter` call site already owning the SyncBlock.
    ///
    /// `deadlineTicks = None` is an infinite acquire (`Monitor.Enter(obj)` /
    /// `Monitor.TryEnter(obj, Timeout.Infinite)`); `Some ms` is a finite
    /// timeout (`Monitor.TryEnter(obj, ms)` with `ms > 0`), expressed as the
    /// absolute virtual-clock tick at which the timed acquire expires.
    /// Only the `TryEnter_Slowpath` path produces `Some _` — the fast-path
    /// short-circuits zero-timeout contention and parks infinite-timeout
    /// contention with `None` directly. When `VirtualClockTicks` advances past
    /// the deadline and the thread is still parked, the driver fires
    /// `SyncBlockMonitor.fireAcquireTimeout`: the thread is dequeued from the
    /// SyncBlock's `AcquireQueue`, the optimistic `Int32 1` (acquired) slot
    /// pushed at park time is rewritten to `Int32 0` (timed out), and the
    /// status flips back to `Runnable`. Storing the deadline in the status
    /// itself rather than in a parallel map makes the invariant "no deadline
    /// once Runnable again" structural — a wake naturally forgets it.
    | BlockedOnSyncBlockAcquire of lockObject : ManagedHeapAddress * deadlineTicks : int64 option
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
    /// `deadlineTicks = None` is an infinite wait (`Monitor.Wait(obj)` / managed
    /// `Timeout.Infinite`); `Some ms` is a finite timeout
    /// (`Monitor.Wait(obj, ms)`), expressed as the absolute virtual-clock
    /// millisecond at which the wait expires. When `VirtualClockTicks` advances
    /// past the deadline and the thread is still parked, the driver fires a
    /// timeout wake (`SyncBlockMonitor.fireWaitTimeout`): the thread is dequeued
    /// from the SyncBlock's `WaitQueue`, routed through the same reacquire
    /// path that `pulse`/`spuriousWake` use (carrying the snapshot depth into
    /// the new owner/AcquireQueue entry), and the optimistic `Int32 1`
    /// (signalled) slot pushed at park time is rewritten to `Int32 0`
    /// (timed out). Storing the deadline in the status itself rather than
    /// in a parallel map makes the invariant "no deadline once Runnable
    /// again" structural — a wake naturally forgets it.
    | BlockedOnSyncBlockWait of lockObject : ManagedHeapAddress * deadlineTicks : int64 option
    /// This thread called `WaitHandle.WaitOne` (via the `WaitHandle_WaitOneCore`
    /// QCall) on a wait handle whose count was zero / unsignalled, and is
    /// parked at the handle's FIFO `WaitQueue`. A subsequent state change that
    /// produces a wake (semaphore `Release`, event `Set`, mutex unlock) flips
    /// the head of the queue back to `Runnable`; the IL `WaitOne` call site
    /// has already advanced past itself, so when the scheduler picks the woken
    /// thread it resumes with `WAIT_OBJECT_0` already on the evaluation stack.
    /// Single-handle blocking only; `BlockedOnWaitHandles` is the multi-handle
    /// counterpart.
    ///
    /// `deadlineTicks = None` is an infinite wait (Win32 `INFINITE` / managed
    /// `Timeout.Infinite`); `Some ms` is a finite timeout, expressed as the
    /// absolute virtual-clock tick at which the wait expires. When
    /// `VirtualClockTicks` advances to that point and the thread is still
    /// parked, the driver fires a timeout wake (`WaitHandle.fireTimeout`):
    /// the thread is dequeued from the handle's `WaitQueue`, the
    /// `WAIT_OBJECT_0` slot pushed at park time is rewritten to
    /// `WAIT_TIMEOUT`, and the status flips back to `Runnable`. Storing the
    /// deadline in the status itself (rather than alongside in a separate
    /// map) makes the invariant "no deadline once Runnable again" structural
    /// — a wake naturally forgets it.
    | BlockedOnWaitHandle of handle : WaitHandleId * deadlineTicks : int64 option
    /// This thread called `WaitHandle.WaitAny` / `WaitAll` (via the
    /// `WaitHandle_WaitMultipleIgnoringSyncContext` QCall) and could not be
    /// satisfied immediately, so it is parked at the FIFO tail of *every* named
    /// handle's `WaitQueue` (once per distinct handle — `handles` may repeat an
    /// entry for a wait-any, and the index reported to the guest is recovered
    /// from this list rather than from queue membership).
    ///
    /// `waitAll = false` (wait-any) is satisfied by any one handle becoming
    /// acquirable. `waitAll = true` requires every handle to be simultaneously
    /// acquirable, and acquires them atomically; duplicate handles are
    /// rejected for that mode (with a guest `DuplicateWaitObjectException`,
    /// as CoreCLR raises), so `handles` is distinct whenever `waitAll` is
    /// set.
    ///
    /// Unlike the single-handle case, the value the guest sees is not known at
    /// park time — a wait-any returns `WAIT_OBJECT_0 + index` for whichever
    /// handle satisfied it. The waker therefore rewrites the optimistic
    /// `WAIT_OBJECT_0` slot pushed at park time, exactly as the timeout path
    /// does. A multi-wait is the only waiter kind whose *signal* wake rewrites
    /// the slot; a single-handle wake still leaves it untouched.
    ///
    /// A parked multi-waiter that is not currently satisfiable stays queued
    /// while the handles it is queued on become signalled and are handed to
    /// other waiters — see the weakened queue invariants documented on
    /// `SemaphoreState` / `MutexState` / `EventState`.
    ///
    /// `deadlineTicks` has the same meaning as on `BlockedOnWaitHandle`: `None`
    /// for `INFINITE`, `Some ms` for an absolute virtual-clock deadline, at
    /// which `WaitHandle.fireMultipleTimeout` dequeues the thread from every
    /// handle it is registered on and rewrites its slot to `WAIT_TIMEOUT`.
    | BlockedOnWaitHandles of handles : WaitHandleId list * waitAll : bool * deadlineTicks : int64 option
    /// This thread called `Thread.Sleep` (routed via the `ThreadNative_Sleep`
    /// QCall) and is parked against the virtual clock with no associated
    /// wait-queue or signalling primitive. There is no per-primitive FIFO
    /// here because the wake is purely time-driven: the scheduler advances
    /// `VirtualClockTicks` one tick at a time, and once it crosses the deadline
    /// the driver fires `Scheduler.fireSleepTimeout`, which flips the status
    /// back to `Runnable`. The IL `Sleep(int)` call site has already
    /// advanced past itself (Sleep returns `void`, so there is no
    /// eval-stack slot to rewrite at park time — distinguishes this from
    /// `BlockedOnJoin`/`BlockedOnSyncBlockWait` et al., which use the
    /// optimistic-push-then-rewrite pattern).
    ///
    /// `deadlineTicks = None` is an infinite sleep (`Thread.Sleep(-1)` /
    /// `Timeout.Infinite`), which currently parks the thread forever
    /// because `Thread.Interrupt` is not yet implemented (a future slice
    /// that wires interrupt will be the only way out of an infinite
    /// sleep — matching real CoreCLR semantics). `Some ms` is a finite
    /// timeout (`Thread.Sleep(ms)` with `ms > 0`), expressed as the
    /// absolute virtual-clock tick at which the sleep expires.
    /// `Thread.Sleep(0)` does not produce a `BlockedOnSleep` transition
    /// at all: it is a no-op handled inline at the call site (the BCL
    /// uses it as a yield hint; PawPrint has no preemption to invoke).
    /// Storing the deadline in the status itself rather than in a
    /// parallel map makes the invariant "no deadline once Runnable
    /// again" structural — a wake naturally forgets it.
    | BlockedOnSleep of deadlineTicks : int64 option
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
        | ThreadStatus.BlockedOnWaitHandles _ -> false
        | ThreadStatus.BlockedOnSleep _ -> false

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
        /// The simulated logical processor this thread is pinned to: what
        /// `sched_getcpu(3)` (`SystemNative_SchedGetCpu`, and hence
        /// `Thread.GetCurrentProcessorId()`) reports while this thread runs.
        ///
        /// Assigned once, at thread creation, by
        /// `EmulatedKernel.cpuForRotation`. PawPrint's scheduler runs one
        /// thread at a time and never migrates a thread between cores, so
        /// "pinned to" and "currently executing on" coincide and one field
        /// answers both questions `sched_getcpu` could be asked. This is the
        /// seat a future core-aware scheduler would rewrite to model migration.
        ///
        /// A total field rather than a `Map<ThreadId, CpuId>` in
        /// `EmulatedKernel` (where the per-thread sigprocmask lives) precisely
        /// because there is no truthful default for an absent key: "no signals
        /// blocked" genuinely is the state of a fresh thread, whereas no
        /// processor index is an identity element, so a missing entry could
        /// only be answered with an arbitrary lie or with an `option` every
        /// caller must handle despite it being structurally unreachable. As a
        /// field, the compiler asks each future thread-creation site which core
        /// it wants.
        Cpu : CpuId
        /// The OS thread identifier this thread reports to the guest through
        /// `SystemNative_TryGetUInt32OSThreadId` (Linux CoreLib) and
        /// `SystemNative_GetUInt64OSThreadId` (macOS CoreLib) — the value
        /// `System.Threading.Lock` then uses as its owner identity.
        ///
        /// Assigned once, at thread creation, and never reused: real kernels do
        /// recycle thread ids after a thread exits, but PawPrint never removes
        /// a thread from `IlMachineState.ThreadState`, and a recycled id would
        /// let a stale `Lock._owningThreadId` be mistaken for a live owner.
        ///
        /// Stored rather than recomputed at each read, even though
        /// `EmulatedKernel.osThreadId` currently derives it from the thread's
        /// `ThreadId` and so could answer every query without this field. The
        /// field is what makes the id a *per-thread fact* rather than a
        /// coincidence of the minting formula: `Cpu` next door is already one,
        /// the test stubs that build a `ThreadState` directly can state an id
        /// without reproducing the formula, and a future scheme that stopped
        /// being a function of `ThreadId` — modelled tid recycling, say, or an
        /// id a guest can influence — would need no change here.
        ///
        /// A total field rather than a `Map<ThreadId, OsThreadId>` on
        /// `EmulatedKernel` for the same reason `Cpu` is: there is no truthful
        /// default for an absent key. Every thread the scheduler can run has an
        /// id; a missing entry could only be answered with an arbitrary lie —
        /// and here the lie would be an *aliased* id, which is precisely the
        /// failure this type exists to prevent.
        OsThreadId : OsThreadId
        /// Threads this one must see run — or see leave the Runnable set — before the
        /// scheduler will choose it again. Empty for every thread that has not just yielded,
        /// and empty means eligible; `Scheduler.candidates` filters out any Runnable thread
        /// whose debt is still outstanding.
        ///
        /// This is `sched_yield(2)` semantics stated as data: an honoured `Thread.Yield()` /
        /// `Thread.Sleep(0)` sends the caller to the back of the run queue, and the queue is
        /// "everyone who was Runnable alongside me at that moment".
        /// `Scheduler.onStepOutcome` charges the debt (see `WhatWeDid.VoluntaryYield`);
        /// `Scheduler.dischargeYieldDebts`, applied to every retired step at the driver's
        /// single step seam, removes members as they run.
        ///
        /// Bounded and self-clearing by construction, which is the whole point of the
        /// representation. The set only ever shrinks, and `Scheduler.candidates` additionally
        /// intersects it with the live Runnable set at read time, so a member that blocks,
        /// parks or terminates stops counting even before its id is removed — no wake path
        /// needs a cleanup hook.
        ///
        /// A peer that never yields nonetheless *discharges* the debt by running, and that is
        /// load-bearing rather than incidental. A rule that instead held a yielder out until
        /// its peers also yielded would let one non-yielding busy-waiter exclude it forever:
        /// `Thread.Yield(); f = true;` racing `while (!f) {}` would livelock under such a
        /// rule.
        YieldDebt : Set<ThreadId>
        /// Set by `Exception.PrepareForForeignExceptionRaise` and consumed by the next throw on
        /// this thread, which is the only reader. It means: the exception about to be raised is
        /// carrying frames restored from an earlier throw, so keep them instead of starting a
        /// fresh trace, and mark the last of them as the point where that earlier trace ended.
        ///
        /// `ExceptionDispatchInfo.Throw()` is the only thing that sets it, via
        /// `Exception.RestoreDispatchState` (Exception.CoreCLR.cs:145), and the `throw` that
        /// follows in the same method is what consumes it — so in practice the window in which
        /// this is `true` is a single guest instruction wide.
        ///
        /// This is CoreCLR's `TEF_ForeignExceptionRaise` (exstate.h:113), which likewise lives on
        /// the thread's exception state and not on the exception. It has to be per-thread rather
        /// than per-exception because the flag is set before the runtime knows which object will
        /// be raised: `RestoreDispatchState` runs to completion, and only then does `throw`
        /// nominate a target. Two threads rethrowing the same captured exception concurrently
        /// therefore do not interfere.
        ///
        /// A `ThreadState` field rather than kernel state: `EmulatedKernel` holds what the guest
        /// could learn by asking the OS, and nothing in this is OS-visible. It sits beside
        /// `IsBackground` as another per-thread runtime fact the guest can only influence
        /// indirectly.
        IsRaisingForeignException : bool
    }

    // --- Frame resolution primitives ---

    /// The frame, if it is still live on this thread. A frame stops being live when it is unwound
    /// — by an ordinary return, or by exception dispatch passing through it — so a caller holding a
    /// `FrameId` across an operation that can unwind wants this rather than `getFrame`, which
    /// treats absence as a bug. No such caller exists today; this is the honest lookup that
    /// `getFrame` is defined in terms of.
    static member tryGetFrame (frameId : FrameId) (s : ThreadState) : MethodState option =
        s.MethodStates |> Map.tryFind frameId

    static member getFrame (frameId : FrameId) (s : ThreadState) : MethodState =
        match ThreadState.tryGetFrame frameId s with
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

    /// `cpu` and `osThreadId` are the simulated logical processor to pin the
    /// new thread to, and the OS thread id it will report. They are parameters
    /// rather than defaults so that callers must consult the kernel's policies
    /// (`EmulatedKernel.cpuForRotation` and `EmulatedKernel.osThreadId`), which
    /// `ThreadState` cannot reach itself — `EmulatedKernel` is compiled after
    /// this file. For `osThreadId` there is the additional reason that a
    /// default would be a *shared* id, and aliasing thread ids silently breaks
    /// `System.Threading.Lock`.
    static member New (cpu : CpuId) (osThreadId : OsThreadId) (methodState : MethodState) =
        {
            ActiveMethodState = FrameId 0
            MethodStates = Map.empty |> Map.add (FrameId 0) methodState
            NextFrameId = 1
            Status = ThreadStatus.Runnable
            IsBackground = false
            Name = None
            Cpu = cpu
            OsThreadId = osThreadId
            YieldDebt = Set.empty
            // A fresh thread has raised nothing yet.
            IsRaisingForeignException = false
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

    /// Look up the IL operation the thread's active frame is about to execute
    /// without modifying any state. Used by the PCT scheduler to classify the
    /// imminent op via `ContextSwitchPrior.ofIlOp` before deciding whether to
    /// demote the running thread; mirrors the dispatch pattern in
    /// `AbstractMachine.executeOneStep` but without the side-effecting machinery.
    ///
    /// Returns:
    ///   - `Some op` when the active frame has an `Il` body and the current
    ///     `IlOpIndex` is in its `Locations` map. This is the only case the
    ///     classifier can reason about; every other case carries no IL.
    ///   - `None` when the active frame's body is `InternalCall`, `PInvoke`,
    ///     or `RuntimeProvided _`. These dispatch through native handlers that
    ///     execute as a single atomic step from the scheduler's point of view,
    ///     and have no `IlOp` to classify. The caller (PCT) treats `None` as
    ///     "always-guest-visible" because native steps almost always have
    ///     observable effects (writes, I/O, syscalls).
    ///
    /// Fails loudly if:
    ///   - the thread has no active frame (`NotStarted`, `Parked`, sentinel
    ///     `FrameId -1`) — calling this on a non-runnable thread is a caller
    ///     bug; the scheduler only peeks at Runnable threads.
    ///   - the body is `Abstract` — virtual dispatch should already have
    ///     resolved to a concrete override, so reaching this here mirrors
    ///     `AbstractMachine.executeOneStep`'s own BUG path for the same case.
    ///   - the body is `Il` but the `IlOpIndex` is missing from `Locations`
    ///     — same invariant the dispatch path asserts; surfacing it here
    ///     means a corrupted PC, not a missing IL handler.
    static member peekNextOp (state : ThreadState) : IlOp option =
        if ThreadStatus.hasNoActiveFrame state.Status then
            failwith
                $"ThreadState.peekNextOp: thread has no active frame (status: %O{state.Status}); the scheduler should only peek at threads with a live frame."

        let frame = ThreadState.getFrame state.ActiveMethodState state

        match frame.ExecutingMethod.Body with
        | MethodBody.Il instr ->
            match instr.Locations.TryGetValue frame.IlOpIndex with
            | true, op -> Some op
            | false, _ ->
                failwith
                    $"ThreadState.peekNextOp: IlOpIndex %d{frame.IlOpIndex} is not in Locations for method %s{frame.ExecutingMethod.DeclaringType.Name}.%s{frame.ExecutingMethod.Name}; the PC is corrupt."
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _ -> None
        | MethodBody.Abstract ->
            failwith
                $"ThreadState.peekNextOp: reached abstract method %s{frame.ExecutingMethod.DeclaringType.Name}.%s{frame.ExecutingMethod.Name}; virtual dispatch should have resolved to a concrete override."
