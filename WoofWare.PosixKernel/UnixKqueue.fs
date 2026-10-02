namespace WoofWare.PosixKernel

/// One Darwin `struct kevent`: an entry of the changelist `kevent(2)` reads, or
/// of the eventlist it fills, each field in Darwin's own numbering.
type Kevent =
    {
        /// `ident`: what the filter watches. For `KeventFilter.Read` and
        /// `KeventFilter.Write`, a descriptor number.
        Ident : uint64
        /// `filter`: which filter, such as `KeventFilter.Read`.
        Filter : int16
        /// `flags`: what to do with the filter, and how it reports, such as
        /// `KeventFlags.Add`.
        Flags : uint16
        /// `fflags`: flags particular to the filter.
        FilterFlags : uint32
        /// `data`: a value particular to the filter.
        Data : int64
        /// `udata`: the caller's own value, which every event the filter
        /// reports carries back verbatim.
        UserData : uint64
    }

/// Darwin's `<sys/event.h>` filter numbers.
[<RequireQualifiedAccess>]
module KeventFilter =
    /// `EVFILT_READ`.
    [<Literal>]
    let Read : int16 = -1s

    /// `EVFILT_WRITE`.
    [<Literal>]
    let Write : int16 = -2s

/// Darwin's `<sys/event.h>` flag bits for a change in the changelist.
[<RequireQualifiedAccess>]
module KeventFlags =
    /// `EV_ADD`: add the filter, or change it if it is already registered.
    [<Literal>]
    let Add : uint16 = 0x0001us

    /// `EV_DELETE`: remove the filter.
    [<Literal>]
    let Delete : uint16 = 0x0002us

    /// `EV_CLEAR`: reset the filter's state once its event has been taken.
    [<Literal>]
    let Clear : uint16 = 0x0020us

    /// `EV_RECEIPT`: report the outcome of the change in the eventlist, rather
    /// than failing the call.
    [<Literal>]
    let Receipt : uint16 = 0x0040us

/// The `timeout` argument of `kevent(2)`, as the kernel's copy-in finds it.
///
/// The caller classifies it, because only the caller knows what its memory
/// holds.
[<RequireQualifiedAccess>]
type KeventTimeout =
    /// A null pointer: the call waits for as long as it takes.
    | Null
    /// The pointer names a readable `struct timespec` holding these.
    | Readable of seconds : int64 * nanoseconds : int64
    /// The pointer is not null, and names no readable `struct timespec`.
    | Unreadable

/// Why this kernel will not answer a `kqueue(2)`.
[<RequireQualifiedAccess>]
type KqueueRefusal =
    /// This kernel is not Darwin-flavoured, and only Darwin has kqueue.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module KqueueRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which of its entry points asked.
    let describe (refusal : KqueueRefusal) : string =
        match refusal with
        | KqueueRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and kqueue exists on Darwin only."

/// What became of a `kevent(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type KeventOutcome =
    /// `kevent` failed with this errno, having changed nothing.
    ///
    /// A finishing call fails with `EBADF` when a close has drained the
    /// kqueue (see `KqueueState.Drained`), and with `EINTR` when a signal with
    /// a handler interrupts the wait.
    | Failed of error : UnixError
    /// `kevent` returned these events, in the order it reports them.
    ///
    /// Always empty here: `kevent` refuses every change, so a kqueue never
    /// holds a registration that could report. Empty is what `kevent` returns
    /// for a wait that timed out, for a timeout of zero with nothing to report,
    /// and for an `nevents` of zero or less.
    | Answered of events : Kevent list
    /// `kevent` did not return. The calling task is parked, and sleeps until
    /// `WakeCondition.satisfied` of this condition is non-empty and
    /// `UnixWait.wakes` wakes it; then `UnixKqueue.finishKevent` finishes the
    /// call.
    | WouldBlock of WakeCondition

/// Why this kernel will not answer a `kevent(2)`.
[<RequireQualifiedAccess>]
type KeventRefusal =
    /// This kernel is not Darwin-flavoured, and only Darwin has kqueue.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The changelist holds these changes, and the call would apply them.
    ///
    /// This library models no filter: what a registered filter reports, and
    /// when, is not measured.
    | Changelist of changes : Kevent list
    /// Nothing is reportable, and the timeout ends past the last instant the
    /// machine's monotonic clock (`UnixMachineState.NanosecondsSinceBoot`, an
    /// `int64` of nanoseconds) can represent: `nanosecondsSinceBoot` plus the
    /// timeout overflows it.
    | DeadlineBeyondClock of nanosecondsSinceBoot : int64 * seconds : int64 * nanoseconds : int64
    /// The wait was asleep, and both a close has drained the kqueue and the
    /// wait's deadline has passed. Darwin answers whichever of the two reached
    /// the sleeping task first, which this library does not record.
    | DrainBesideDeadline of kqueue : OpenFileDescriptionId
    /// The wait was asleep and a signal is pending for the task, and this
    /// library will not say how the signal ends it.
    | Interruption of SyscallInterruptionRefusal

[<RequireQualifiedAccess>]
module KeventRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it actually passed.
    let describe (refusal : KeventRefusal) : string =
        match refusal with
        | KeventRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and kevent exists on Darwin only."
        | KeventRefusal.Changelist changes ->
            $"the changelist holds %d{List.length changes} change(s) the call would apply (%A{changes}), and this kernel models no kqueue filter: what a registered EVFILT_READ or EVFILT_WRITE reports on each kind of descriptor, and which of its events EV_CLEAR resets, are not measured."
        | KeventRefusal.DeadlineBeyondClock (now, seconds, nanoseconds) ->
            $"the machine has been up for %d{now} ns and the timeout is %d{seconds} s and %d{nanoseconds} ns, which ends past the last nanosecond the monotonic clock can represent."
        | KeventRefusal.DrainBesideDeadline kqueue ->
            $"a task asleep in kevent on kqueue %O{kqueue} has both had the kqueue drained by a close (EBADF) and reached its deadline (0 events). Darwin answers whichever of the two reached the sleeping task first, and this kernel does not record which did."
        | KeventRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal

/// Darwin's kqueue: `kqueue(2)`, and `kevent(2)` as a wait.
[<RequireQualifiedAccess>]
module UnixKqueue =

    /// `kqueue(2)`: create a kqueue, and a descriptor onto it, the lowest one
    /// not in use. The descriptor is blocking.
    ///
    /// Measured on Darwin, `kqueue()` also sets `FD_CLOEXEC`, which this kernel
    /// does not model, since it models neither `exec` nor any per-descriptor
    /// flag.
    ///
    /// Under the Linux flavour every call is refused: Linux has no kqueue.
    let kqueue<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int * UnixSystem<'Task, 'Handler>, KqueueRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> Error (KqueueRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux)
        | SimulatedUnixFlavour.Darwin ->

        // Measured on 27.0.0 (`kqueue-kevent.c`, section A): the lowest free
        // descriptor, O_RDWR, not O_NONBLOCK.
        let fd, registry =
            FileDescriptorRegistry.createKqueue system.Process.FileDescriptors

        Ok (
            fd,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        )

    let private nanosecondsPerSecond : int64 = 1_000_000_000L

    /// Whether a close has drained the kqueue `kqueue`: the same question the
    /// wake condition of a waiter on it asks.
    let private drained<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (kqueue : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        WakeCondition.satisfied task (WakeCondition.Primitive (WakePrimitive.KqueueDrained kqueue)) system
        |> Set.isEmpty
        |> not

    /// `kevent(2)`, made by `task` through the descriptor `kq`, with a
    /// changelist of `nchanges` entries and room for `nevents` events.
    ///
    /// `changes` is what the copy-in finds at the changelist, from its start:
    /// every one of the `nchanges` entries when all are readable, and fewer
    /// when one is not, the next entry being the first that cannot be read.
    /// It is never longer than `nchanges`, and is empty when `nchanges` is zero
    /// or less, since then no entry is read. `eventlist` is the buffer events
    /// would be copied out to; no call here copies any, so it is never read.
    ///
    /// Every argument check is answered, in Darwin's order: `EFAULT` for an
    /// unreadable timeout and `EINVAL` for a timeout whose `tv_sec` is below 0
    /// or above `INT32_MAX`, or whose `tv_nsec` is below 0 or above
    /// 1000000000; then `EBADF` for a descriptor that is not open or is not a
    /// kqueue; then, for an `nchanges` above zero, `EFAULT` when the first
    /// change cannot be read. A call that would apply a change is refused
    /// (`KeventRefusal.Changelist`): this library models no filter. Then an
    /// `nevents` of zero or less returns no events at once, whatever the
    /// timeout. A failure changes nothing.
    ///
    /// A wait on a kqueue a close has drained fails with `EBADF` at once (see
    /// `KqueueState.Drained`). Otherwise there is nothing to report, so a
    /// timeout of zero returns no events at once; a positive one parks `task`
    /// until that much time has passed on the machine's monotonic clock, and a
    /// null one parks it with no deadline. A parked wait is finished with
    /// `finishKevent`.
    ///
    /// Every waiter on one kqueue is woken by the close that drains it, and a
    /// deadline or a signal is each waiter's own.
    ///
    /// `task` must not already be parked. Under the Linux flavour every call
    /// is refused: Linux has no kqueue.
    let kevent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (kq : int)
        (nchanges : int)
        (changes : Kevent list)
        (nevents : int)
        (eventlist : UserBuffer)
        (timeout : KeventTimeout)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<KeventOutcome * UnixSystem<'Task, 'Handler>, KeventRefusal>
        =
        // `eventlist` is read only to copy events out, and a kqueue here never
        // has any; measured, a null and an unreadable eventlist answer every
        // other argument exactly as a real one does.
        ignore<UserBuffer> eventlist

        if List.length changes > max nchanges 0 then
            failwith
                $"UnixKqueue.kevent: %d{List.length changes} changes were passed for an nchanges of %d{nchanges}, and the copy-in reads at most nchanges and none for zero or less (this is a bug in the client)."

        match UnixTaskTable.parkedFor task system.Tasks with
        | Some parked ->
            failwith
                $"UnixKqueue.kevent: task %O{task} is parked in %A{parked}, and is issuing a kevent. A task blocks in one syscall at a time; a parked wait is finished with `finishKevent` (this is a bug in the client)."
        | None ->

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> Error (KeventRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux)
        | SimulatedUnixFlavour.Darwin ->

        let failed (error : UnixError) = Ok (KeventOutcome.Failed error, system)

        // Measured on 27.0.0 (`kqueue-kevent.c`, section B, all 5040 calls):
        // the timeout first, then the descriptor, then the changelist, then
        // nevents, then the kqueue itself.
        let timeout =
            match timeout with
            | KeventTimeout.Unreadable -> Error UnixError.EFAULT
            | KeventTimeout.Readable (seconds, nanoseconds) when
                seconds < 0L
                || seconds > int64 System.Int32.MaxValue
                || nanoseconds < 0L
                || nanoseconds > nanosecondsPerSecond
                ->
                Error UnixError.EINVAL
            | KeventTimeout.Readable (seconds, nanoseconds) -> Ok (Some (seconds, nanoseconds))
            | KeventTimeout.Null -> Ok None

        match timeout with
        | Error error -> failed error
        | Ok timeout ->

        // "Not a kqueue" is EBADF, as "not open" is.
        let kqueue =
            match FileDescriptorRegistry.tryFindWithId kq system.Process.FileDescriptors with
            | Some (id,
                    {
                        Target = OpenFileTarget.Kqueue _
                    }) -> Some id
            | Some _
            | None -> None

        match kqueue with
        | None -> failed UnixError.EBADF
        | Some kqueue ->

        if nchanges > 0 then
            match changes with
            | [] -> failed UnixError.EFAULT
            | changes -> Error (KeventRefusal.Changelist changes)
        elif nevents <= 0 then
            Ok (KeventOutcome.Answered [], system)
        elif drained task kqueue system then
            failed UnixError.EBADF
        else

        let park (deadline : int64 option) =
            let parked =
                ParkedSyscall.Kevent
                    {
                        Kqueue = kqueue
                        Fd = kq
                        MaxEvents = nevents
                        Deadline = deadline
                    }

            Ok (KeventOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked system)

        match timeout with
        | None -> park None
        | Some (0L, 0L) -> Ok (KeventOutcome.Answered [], system)
        | Some (seconds, nanoseconds) ->

        // Within `int64`: the screen above bounds `seconds` by INT32_MAX.
        let duration = seconds * nanosecondsPerSecond + nanoseconds
        let now = system.Machine.NanosecondsSinceBoot

        if now > System.Int64.MaxValue - duration then
            Error (KeventRefusal.DeadlineBeyondClock (now, seconds, nanoseconds))
        else
            park (Some (now + duration))

    let private finishKeventHolding<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<KeventOutcome * UnixSystem<'Task, 'Handler>, KeventRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.Kevent parked) -> parked
            | Some other ->
                failwith
                    $"UnixKqueue.finishKevent: task %O{task} is parked in %A{other}, not in a kevent, so there is no wait to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixKqueue.finishKevent: task %O{task} is not parked, so there is no wait to finish. Only a task `kevent` answered `WouldBlock` finishes here (this is a bug in the client)."

        let isDrained = drained task parked.Kqueue system

        let timedOut =
            match parked.Deadline with
            | Some deadline -> system.Machine.NanosecondsSinceBoot >= deadline
            | None -> false

        let finished =
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }

        let completing (outcome : KeventOutcome) =
            SyscallInterruption.beforeCompleting task system
            |> Result.mapError KeventRefusal.Interruption
            |> Result.map (fun () -> outcome, finished)

        if isDrained && timedOut then
            Error (KeventRefusal.DrainBesideDeadline parked.Kqueue)
        elif isDrained then
            // Measured on 27.0.0 (`kqueue-kevent.c`, section E).
            completing (KeventOutcome.Failed UnixError.EBADF)
        elif timedOut then
            completing (KeventOutcome.Answered [])
        else

        // Measured on 27.0.0 (`kqueue-kevent.c`, section D): a caught signal
        // ends the wait with EINTR, under SA_RESTART or not.
        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (KeventRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> Ok (KeventOutcome.Failed UnixError.EINTR, finished)
        | Ok (Some SyscallInterruption.Restart) ->
            failwith
                "UnixKqueue.finishKevent: a kevent restarted after a signal, where `SyscallInterruption.ruleOf` says one never restarts (this is a bug in this library)."
        | Ok None ->
            let parkedAgain = ParkedSyscall.Kevent parked
            Ok (KeventOutcome.WouldBlock (WakeCondition.ofPark parkedAgain), UnixWait.park task parkedAgain system)

    /// Finish the `kevent` wait `task` is parked in, as a woken real wait does.
    ///
    /// Answers `EBADF` when a close has drained the kqueue; no events when the
    /// deadline has passed; `EINTR` when a signal with a handler interrupts the
    /// wait; and otherwise re-parks the task on the same kqueue and deadline.
    /// A signal pending beside either of the first two answers is refused, as
    /// is a drain beside an expired deadline: Darwin answers whichever reached
    /// the sleeping task first, and this library does not record which did. An
    /// answer clears the park, and with it the call's hold on the kqueue, which
    /// goes then if no descriptor names it any more.
    ///
    /// `task` must be parked in a `kevent`.
    let finishKevent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<KeventOutcome * UnixSystem<'Task, 'Handler>, KeventRefusal>
        =
        let held =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some parked -> ParkedSyscall.descriptions parked
            | None -> []

        // A drain ends the wait while the kqueue may have no descriptor left
        // (`kqueue-kevent.c` section E1), and the call's reference to it goes
        // as it returns.
        finishKeventHolding task system
        |> Result.map (fun (outcome, after) ->
            outcome, ObjectLifetime.releaseUnreferencedUnrefusable "UnixKqueue.finishKevent" held after
        )
