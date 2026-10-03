namespace WoofWare.PosixKernel

/// Why this kernel will not answer a `poll`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type PollRefusal =
    /// The entry names an event queue, which this kernel does not answer
    /// `poll(2)` for: under Linux an epoll instance, whatever was asked; under
    /// Darwin a kqueue asked for a bit that registers `EVFILT_READ` on it.
    ///
    /// Reachable in a way epoll's equivalent is not: `epoll_ctl` screens the
    /// targets it will accept, and `poll(2)` accepts any descriptor.
    | UnmodelledTarget of fd : int
    /// Under Darwin, the entry asks a socket of a kind whose kqueue filters
    /// this kernel does not model (see `DarwinReadiness.modelsSocket`) for a
    /// bit that registers one.
    | UnmodelledSocket of fd : int * domain : SocketDomain * kind : SocketKind
    /// Under Darwin, nothing is ready and the call would sleep with an
    /// `EVFILT_VNODE` filter registered on the regular file or directory `fd`
    /// names: such a filter reports only when the file changes, which this
    /// kernel does not model.
    | UnmodelledVnodeWait of fd : int
    /// Under Darwin, nothing is ready and the call would sleep with a timeout
    /// of `milliseconds`, which is negative and not -1.
    ///
    /// Darwin waits for ever on -1 alone. By its source it reads any other
    /// timeout as an unsigned count of milliseconds, so -2 would wait about
    /// 49.7 days: no measurement can tell that from waiting for ever, and a
    /// client that advances its clock to the next deadline would reach it at
    /// once.
    | UnmeasuredNegativeTimeout of milliseconds : int
    /// Under Darwin, the call has `count` entries, more than `FD_SETSIZE`
    /// (1024) and no more than `OPEN_MAX` (10240). Darwin answers `EINVAL` for
    /// such a count exactly when it exceeds the process's `RLIMIT_NOFILE` soft
    /// limit, or for root only when it does and also exceeds 1024 (measured
    /// for a process that is not root, `poll-darwin.c` section N); this kernel
    /// models no `RLIMIT_NOFILE`.
    | UnmodelledEntryCount of count : int
    /// Under Darwin, a sleeping call has both something to report and reached
    /// its deadline. Darwin answers whichever reached the sleeping task first,
    /// which this kernel does not record.
    | EventsBesideDeadline
    /// Nothing is ready, and the timeout ends past the last instant the
    /// machine's monotonic clock (`UnixMachineState.NanosecondsSinceBoot`,
    /// an `int64` of nanoseconds) can represent: `nanosecondsSinceBoot` plus
    /// `timeoutMilliseconds` overflows it.
    | DeadlineBeyondClock of nanosecondsSinceBoot : int64 * timeoutMilliseconds : int
    /// The poll was asleep and a signal is pending for the task, and this
    /// library will not say how the signal ends it.
    | Interruption of SyscallInterruptionRefusal

/// What became of a `poll(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type PollOutcome =
    /// `poll` returned: the `revents` for each entry, in order, raw bits in the
    /// flavour's own `<poll.h>` numbering, and the return value, which counts
    /// the entries carrying anything.
    | Answered of revents : int16 list * count : int
    /// `poll` failed with this errno: under Darwin, `EINVAL` for more than
    /// 10240 entries; and a poll that was asleep fails with `EINTR` when a
    /// signal with a handler interrupts it.
    | Failed of error : UnixError
    /// `poll` did not return. The calling task is parked, and sleeps until
    /// `WakeCondition.satisfied` of this condition is non-empty; then
    /// `UnixPoll.finishPoll` finishes the call.
    | WouldBlock of WakeCondition

[<RequireQualifiedAccess>]
module PollRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it should do instead.
    let describe (refusal : PollRefusal) : string =
        match refusal with
        | PollRefusal.UnmodelledTarget fd ->
            $"fd %d{fd} names an event queue (an epoll instance, or a kqueue asked for a read bit), which this kernel does not answer `poll(2)` for. An event queue's own readiness depends on re-reading what it has queued, and what that leaves queued is unmeasured; model that before answering."
        | PollRefusal.UnmodelledSocket (fd, domain, kind) ->
            $"fd %d{fd} is a %O{kind} socket in %O{domain}, and the entry asks for a bit that registers a kqueue filter on it. This kernel models those filters for IPv4 and IPv6 stream sockets only: what activates a datagram socket's filters is not modelled, and a Unix-domain socket's are not measured."
        | PollRefusal.UnmodelledVnodeWait fd ->
            $"nothing is ready, and the poll would sleep with an EVFILT_VNODE filter registered on fd %d{fd} for a vnode bit (POLLEXTEND, POLLATTRIB, POLLNLINK or POLLWRITE). That filter reports when the file changes, which this kernel does not model."
        | PollRefusal.UnmeasuredNegativeTimeout milliseconds ->
            $"nothing is ready, and the timeout is %d{milliseconds}ms. Darwin waits for ever on -1 alone, and by its source reads any other negative timeout as an unsigned count of milliseconds (about 49.7 days for -2), which no measurement can tell from waiting for ever."
        | PollRefusal.UnmodelledEntryCount count ->
            $"the poll has %d{count} entries, more than FD_SETSIZE (1024) and no more than OPEN_MAX (10240). Darwin answers EINVAL for such a count exactly when it exceeds the RLIMIT_NOFILE soft limit (for root, the source says, only when it exceeds 1024 too), and this kernel models no RLIMIT_NOFILE."
        | PollRefusal.EventsBesideDeadline ->
            "a task asleep in poll has both something to report and reached its deadline (0). Darwin answers whichever reached the sleeping task first, and this kernel does not record which did."
        | PollRefusal.DeadlineBeyondClock (now, timeoutMilliseconds) ->
            $"the machine has been up for %d{now} ns and the timeout is %d{timeoutMilliseconds}ms, which ends past the last nanosecond the monotonic clock can represent. Linux's source saturates such a deadline, making the wait infinite, but that is unmeasured."
        | PollRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal

/// The flags `epoll_create1(2)` accepts, in Linux's numbering.
[<RequireQualifiedAccess>]
module EpollCreateFlags =
    /// `EPOLL_CLOEXEC`: set `FD_CLOEXEC` on the new descriptor. The same value
    /// as `O_CLOEXEC`.
    [<Literal>]
    let CloseOnExec : int = OpenFlagNumbering.LinuxCloseOnExec

/// Why this kernel will not answer an `epoll_create1(2)`.
[<RequireQualifiedAccess>]
type EpollCreateRefusal =
    /// This kernel is not Linux-flavoured, and only Linux has epoll.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module EpollCreateRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which of its entry points asked.
    let describe (refusal : EpollCreateRefusal) : string =
        match refusal with
        | EpollCreateRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and epoll_create1 exists on Linux only. This flavour's counterpart is kqueue (UnixKqueue.kqueue)."

/// What became of an `epoll_wait(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type EpollWaitOutcome =
    /// `epoll_wait` failed with this errno.
    ///
    /// The call as first made changes nothing when it fails. A finishing call
    /// fails only with `EINTR`, when a signal with a handler interrupts the
    /// wait, and what its walk of the epoll instance consumed stays consumed.
    | Failed of error : UnixError
    /// `epoll_wait` returned these events, in delivery order: each the
    /// registration's `data` and the `events` written for it, in Linux's
    /// `<sys/epoll.h>` numbering (`EpollEvents`). Empty when the call timed
    /// out, or had a timeout of 0 and found nothing.
    | Answered of events : (uint64 * uint32) list
    /// `epoll_wait` did not return. The calling task is parked, and sleeps
    /// until `WakeCondition.satisfied` of this condition is non-empty and
    /// `UnixWait.wakes` wakes it; then `UnixPoll.finishEpollWait` finishes the
    /// call.
    | WouldBlock of WakeCondition

/// Why this kernel will not answer an `epoll_wait(2)`.
[<RequireQualifiedAccess>]
type EpollWaitRefusal =
    /// This kernel is not Linux-flavoured, and only Linux has epoll.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The buffer reached the up-front address screen and has no address to
    /// screen.
    | Buffer of BufferRefusal
    /// The wait has events to deliver, and the buffer is unmapped, so copying
    /// them out faults.
    ///
    /// Linux answers EFAULT only when the first event's copy faults, and a
    /// real buffer can be partly mapped; which of the walked entries stay
    /// pending after a fault is unmeasured.
    | UnmeasuredCopyOutFault of epoll : OpenFileDescriptionId
    /// Nothing is deliverable, and the timeout ends past the last instant the
    /// machine's monotonic clock (`UnixMachineState.NanosecondsSinceBoot`, an
    /// `int64` of nanoseconds) can represent: `nanosecondsSinceBoot` plus
    /// `timeoutMilliseconds` overflows it.
    | DeadlineBeyondClock of nanosecondsSinceBoot : int64 * timeoutMilliseconds : int
    /// The wait was asleep and a signal is pending for the task, and this
    /// library will not say how the signal ends it.
    | Interruption of SyscallInterruptionRefusal

[<RequireQualifiedAccess>]
module EpollWaitRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it actually passed.
    let describe (refusal : EpollWaitRefusal) : string =
        match refusal with
        | EpollWaitRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and epoll_wait exists on Linux only. This flavour's counterpart is kevent (UnixKqueue.kevent)."
        | EpollWaitRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | EpollWaitRefusal.UnmeasuredCopyOutFault epoll ->
            $"the epoll instance %O{epoll} has events to deliver, so this call copies them out -- but the buffer is unmapped, so that copy faults. Which of the events the walk took stay pending after the fault, and whether the call answers EFAULT or the count copied before it, are unmeasured."
        | EpollWaitRefusal.DeadlineBeyondClock (now, timeoutMilliseconds) ->
            $"the machine has been up for %d{now} ns and the timeout is %d{timeoutMilliseconds}ms, which ends past the last nanosecond the monotonic clock can represent. Linux's source saturates such a deadline, making the wait infinite, but that is unmeasured."
        | EpollWaitRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal

/// The `event` argument of `epoll_ctl(2)`, as the kernel's copy-in finds it.
///
/// The caller classifies it, because only the caller knows what its memory
/// holds; the consequence is the kernel's, which is `EFAULT` for any operation
/// but `EPOLL_CTL_DEL` (which never reads it) ahead of every other check.
[<RequireQualifiedAccess>]
type EpollEventArgument =
    /// The pointer names a whole readable `struct epoll_event`, holding these.
    /// `events` is in Linux's `<sys/epoll.h>` numbering (`EpollEvents`), and
    /// may carry any bits: `epoll_ctl` rejects none of them outright.
    | Readable of events : uint32 * data : uint64
    /// The pointer is null, or does not name a whole readable
    /// `struct epoll_event`.
    | Unreadable

/// Why `epoll_ctl(2)` failed, in the order Linux decides them.
///
/// Measured on Linux 6.18.5 by `epoll-ctl.c`
/// (docs/plans/2026-08-23-posix-kernel-extraction), which tried every
/// combination of seven kinds of `epfd`, fourteen kinds of target, nine
/// operation values, a null and a real event pointer, and 36 event masks;
/// each adjacent pair below is separated by an input that provokes exactly one
/// of the two.
[<RequireQualifiedAccess>]
type EpollCtlError =
    /// The event could not be read, for an operation that reads one (every
    /// operation value but `EPOLL_CTL_DEL`, including unrecognised ones);
    /// `EFAULT`. First of everything.
    | EventUnreadable
    /// `epfd` is not a live descriptor; `EBADF`.
    | BadPortFd
    /// The target fd is not a live descriptor; `EBADF`.
    | BadTargetFd
    /// The target supports no poll -- a regular file or a directory; `EPERM`.
    /// Ahead of the not-an-epoll-instance check, so a file as both epoll instance and target is
    /// `EPERM`.
    | TargetNotPollable
    /// `epfd` is not an epoll instance, or `epfd` and the target name the same
    /// open file description (a `dup` of the epoll instance included); `EINVAL`.
    | NotAnEventPort
    /// The event carries `EPOLLEXCLUSIVE` where it is not permitted: on
    /// `EPOLL_CTL_MOD`, or on `EPOLL_CTL_ADD` with an epoll instance as the
    /// target or with any bit outside `EPOLLIN`, `EPOLLOUT`, `EPOLLERR`,
    /// `EPOLLHUP`, `EPOLLWAKEUP`, `EPOLLET` and `EPOLLEXCLUSIVE` itself;
    /// `EINVAL`. Ahead of the table, so it wins over `EEXIST` and `ENOENT`.
    | ExclusiveNotPermitted
    /// `EPOLL_CTL_ADD` of a target already registered; `EEXIST`.
    | AlreadyRegistered
    /// `EPOLL_CTL_MOD` or `EPOLL_CTL_DEL` of a target not registered;
    /// `ENOENT`.
    | NotRegistered
    /// The operation is none of `EPOLL_CTL_ADD`, `EPOLL_CTL_DEL` and
    /// `EPOLL_CTL_MOD`; `EINVAL`. Last: every check above still applies to
    /// it, the copy-in included.
    | UnrecognisedOperation

[<RequireQualifiedAccess>]
module EpollCtlError =
    /// The errno `epoll_ctl(2)` answers for this failure.
    ///
    /// Not injective: two failures share `EBADF` and three share `EINVAL`, so a
    /// client wanting to know *which* keeps the case.
    let toErrno (error : EpollCtlError) : UnixError =
        match error with
        | EpollCtlError.EventUnreadable -> UnixError.EFAULT
        | EpollCtlError.BadPortFd
        | EpollCtlError.BadTargetFd -> UnixError.EBADF
        | EpollCtlError.TargetNotPollable -> UnixError.EPERM
        | EpollCtlError.NotAnEventPort
        | EpollCtlError.ExclusiveNotPermitted
        | EpollCtlError.UnrecognisedOperation -> UnixError.EINVAL
        | EpollCtlError.AlreadyRegistered -> UnixError.EEXIST
        | EpollCtlError.NotRegistered -> UnixError.ENOENT

/// What `epoll_ctl(2)` answered.
[<RequireQualifiedAccess>]
type EpollCtlAnswer =
    /// Applied. The system this rides with carries the new interest table, and
    /// the ready list if the change made the target pending.
    | Changed
    /// `epoll_ctl(2)` failed. Nothing changed: the system comes back as it
    /// was.
    | Failed of reason : EpollCtlError

/// Why this kernel will not answer an `epoll_ctl(2)`.
///
/// Distinct from a `Failed` answer: that is `epoll_ctl(2)` refusing, and this is
/// this library having nothing to say. Every refusal but `UnmodelledFlavour`
/// is given only where the real call would succeed, having passed every check
/// that could fail it; so a call that would fail is answered with its failure
/// whatever it asks for.
[<RequireQualifiedAccess>]
type EpollCtlRefusal =
    /// This kernel is not Linux-flavoured, and only Linux has epoll.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// An `EPOLL_CTL_ADD` whose target is itself an epoll instance.
    ///
    /// Linux accepts one, subject to a loop check and a nesting depth of at
    /// most four epoll instances (`ELOOP` beyond either). This library does not model
    /// what a nested epoll instance reports or how a wake propagates through it.
    | NestedPort of targetFd : int
    /// The event carries `EPOLLEXCLUSIVE`, which changes which of several epoll instances
    /// on one target a wake reaches. This library's wakes reach every epoll instance.
    | Exclusive
    /// The event carries `EPOLLONESHOT`, which disarms the registration once it
    /// has reported. This library's registrations stay armed.
    | OneShot
    /// The event carries `EPOLLWAKEUP`. The kernel keeps it only for a caller
    /// with `CAP_BLOCK_SUSPEND` on a kernel built with power management, and
    /// silently clears it otherwise; this library models neither.
    | WakeUp
    /// The event lacks `EPOLLET`, asking to be level-triggered. This library's epoll
    /// models edge-triggered registrations only: a wait consumes every entry it
    /// walks and never re-arms a still-ready one, so a wait after a partly
    /// drained level would sleep where a real `epoll_wait` returns again.
    | LevelTriggered
    /// An `EPOLL_CTL_ADD` whose target is an end of a pipe the process made.
    /// An edge-triggered registration reports what the pipe's own wakes
    /// signal, and which transfers and closes signal a pipe's waiters, and with
    /// which events, is not measured.
    | PipeTarget of targetFd : int

[<RequireQualifiedAccess>]
module EpollCtlRefusal =
    /// What this kernel knows about why it will not answer. The client supplies
    /// its own half -- which of its entry points was asked, and on whose
    /// behalf.
    let describe (refusal : EpollCtlRefusal) : string =
        match refusal with
        | EpollCtlRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and epoll_ctl exists on Linux only. This flavour's counterpart is a kevent changelist (UnixKqueue.kevent)."
        | EpollCtlRefusal.NestedPort targetFd ->
            $"fd %d{targetFd} is itself an epoll instance. Linux would register it (subject to a loop check and a nesting depth of four, both ELOOP), but what a nested epoll instance reports and how a wake propagates through one are not modelled."
        | EpollCtlRefusal.Exclusive ->
            "the event carries EPOLLEXCLUSIVE, and the registration would succeed. An exclusive registration changes which of several epoll instances on one target a wake reaches, and this library's wakes reach every epoll instance. Model wake-one delivery before answering."
        | EpollCtlRefusal.OneShot ->
            "the event carries EPOLLONESHOT, and the registration would succeed. A one-shot registration disarms once it has reported until a MOD re-arms it, and this library's registrations stay armed. Model disarming before answering."
        | EpollCtlRefusal.WakeUp ->
            "the event carries EPOLLWAKEUP, and the registration would succeed. The kernel keeps the bit only for a caller with CAP_BLOCK_SUSPEND on a kernel built with power management, clearing it silently otherwise, and this library models neither capabilities nor wakeup sources."
        | EpollCtlRefusal.PipeTarget targetFd ->
            $"fd %d{targetFd} is an end of a pipe the process made, and the registration would succeed. An edge-triggered registration is made pending by the wakes its target signals, and which reads, writes and closes signal a pipe's waiters, with which events, is unmeasured: Linux's pipe_write, for one, wakes readers on every write once a waiter has polled the pipe, not only on the write that makes it non-empty. poll(2) on a pipe is answered; measure the pipe's wakes before registering one."
        | EpollCtlRefusal.LevelTriggered ->
            "the event lacks EPOLLET, asking to be level-triggered, and the registration would succeed. This library's epoll models edge-triggered registrations only: the ready list is consumed as it is drained and a still-ready entry is never re-armed, so a wait after a partly drained level would sleep where a real epoll_wait returns again. Register with EPOLLET, or model level-triggering before answering."

[<RequireQualifiedAccess>]
module UnixPoll =

    /// `epoll_wait(2)`'s four screens, in Linux's order: the epoll instance to
    /// wait on, an errno to fail with, or the buffer's refusal.
    ///
    /// The order is measured on Linux 6.18.5 rather than read off the kernel
    /// source: the widely-reproduced `do_epoll_wait` listing checks `maxevents`
    /// and `access_ok` *before* `fdget`, and current kernels do not.
    ///
    /// Changes nothing: everything a wait does before it reaches the instance
    /// is a question.
    let private admitEpollWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (maxEvents : int)
        (buffer : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<OpenFileDescriptionId, UnixError>, BufferRefusal>
        =
        // Measured on 6.18.5, each adjacent pair separated by an input that
        // provokes exactly one of the two: descriptor, then `maxevents`, then
        // the buffer, then is-it-an-epoll-instance.
        match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
        | None -> Ok (Error UnixError.EBADF)
        | Some (epoll, description) ->

        let architecture = SimulatedUnixPlatform.architecture system.Machine.UnixPlatform

        // The kernel's predicate is `maxevents <= 0 || maxevents > EP_MAX_EVENTS`.
        // Measured (`epoll-wait.c`, section G): a negative maxevents is
        // screened exactly as zero is.
        if maxEvents <= 0 || maxEvents > LinuxEpollLimits.maxEvents architecture then
            Ok (Error UnixError.EINVAL)
        else

        // The byte range `access_ok(events, maxevents * sizeof(struct
        // epoll_event))` screens. This multiplication is safe only *below* the
        // cap just applied, which is what `EP_MAX_EVENTS` exists for: it is
        // `INT_MAX / eventSize`, so every count that reaches here has a product
        // inside `int32`.
        let bufferExtent =
            uint64 maxEvents * uint64 (LinuxEpollLimits.eventSize architecture)

        // Not a mappedness check. On 64-bit Linux `access_ok` only rejects
        // ranges reaching into the kernel half, so a merely-unmapped userspace
        // address passes and the wait then blocks, faulting at delivery --
        // which is why this must not eagerly demand that the buffer be real
        // before sleeping.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                bufferExtent
        with
        | Error refusal -> Error refusal
        | Ok true -> Ok (Error UnixError.EFAULT)
        | Ok false ->

        match description.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _ ->
            // A live descriptor onto the wrong kind of object. EINVAL is epoll's
            // own answer for it, and it is the last of the four screens --
            // behind the buffer, which is why an unmappable buffer on a
            // non-epoll descriptor is EFAULT rather than this.
            //
            // A socket is measured to be exactly like the other two here rather
            // than assumed to be: `epoll_wait` on a socket fd is EINVAL, and
            // EFAULT still wins ahead of it for an unmappable buffer.
            Ok (Error UnixError.EINVAL)
        | OpenFileTarget.Kqueue _ ->
            failwith
                $"UnixPoll.epollWait: fd %d{fd} names a kqueue, which a Linux-flavoured kernel cannot hold (this is a bug in the caller's state construction)."
        | OpenFileTarget.Epoll _ -> Ok (Ok epoll)

    /// `epoll_ctl(2)`: apply `op` to the interest table of the epoll instance
    /// `epfd` names, for the target `fd` names, with the `event` the caller
    /// passed.
    ///
    /// `op` is the raw operation value, in Linux's numbering: `EPOLL_CTL_ADD`
    /// is 1, `EPOLL_CTL_DEL` 2 and `EPOLL_CTL_MOD` 3, and every other value is
    /// answered as Linux answers it. `event` is what the kernel's copy-in finds
    /// (see `EpollEventArgument`); it is not read for `EPOLL_CTL_DEL`.
    ///
    /// Under the Linux flavour every failure is answered, in Linux's order (see
    /// `EpollCtlError`), and so is every `EPOLL_CTL_DEL` and every `ADD` or
    /// `MOD` of an edge-triggered registration without `EPOLLEXCLUSIVE`,
    /// `EPOLLONESHOT` or `EPOLLWAKEUP`, whatever other bits it carries. The
    /// registration stores the caller's `events` with `EPOLLERR` and `EPOLLHUP`
    /// added, and a wait reports the target's readiness masked by that (see
    /// `LinuxReadiness`); an `ADD` or `MOD` whose target is ready under the new
    /// mask makes the registration pending at once, and a `MOD` of an entry
    /// already pending leaves its place alone. An `ADD` or `MOD` that would
    /// succeed with one of the modes this library does not model, and an
    /// `ADD` of another epoll instance, are refused (see `EpollCtlRefusal`).
    /// Under the Darwin flavour every call is refused: Darwin has no epoll.
    let epollCtl<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (epfd : int)
        (op : int)
        (fd : int)
        (event : EpollEventArgument)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollCtlAnswer * UnixSystem<'Task, 'Handler>, EpollCtlRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (EpollCtlRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let failed (error : EpollCtlError) =
            Ok (EpollCtlAnswer.Failed error, system)

        // Linux's `EPOLL_CTL_*` values.
        let add = 1
        let del = 2
        let modify = 3

        // Measured on 6.18.5 (`epoll-ctl.c`), each step separated from the
        // next by an input that provokes exactly one of the two. The copy-in
        // comes first, and is skipped for DEL alone: an unrecognised op reads
        // the event too.
        let readEvent =
            if op = del then
                Ok (0u, 0UL)
            else
                match event with
                | EpollEventArgument.Readable (events, data) -> Ok (events, data)
                | EpollEventArgument.Unreadable -> Error ()

        match readEvent with
        | Error () -> failed EpollCtlError.EventUnreadable
        | Ok (events, data) ->

        let registry = system.Process.FileDescriptors

        match FileDescriptorRegistry.tryFindWithId epfd registry with
        | None -> failed EpollCtlError.BadPortFd
        | Some (epollId, epollDescription) ->

        match FileDescriptorRegistry.tryFindWithId fd registry with
        | None -> failed EpollCtlError.BadTargetFd
        | Some (targetId, targetDescription) ->

        // `file_can_poll`: the target must have a `->poll` handler, which a
        // regular file and a directory lack.
        match targetDescription.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        // Measured on `/dev/null` and `/dev/urandom` for ADD and MOD, through
        // descriptors opened for reading and for writing (`devices.c`, EPOLL
        // rows): neither driver has a poll operation either.
        | OpenFileTarget.CharacterDevice _ -> failed EpollCtlError.TargetNotPollable
        | OpenFileTarget.Kqueue _ ->
            failwith
                $"UnixPoll.epollCtl: fd %d{fd} names a kqueue, which a Linux-flavoured kernel cannot hold (this is a bug in the caller's state construction)."
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Socket _
        | OpenFileTarget.Pipe _ ->

        // One kernel test, `f.file == tf.file || !is_file_epoll(f.file)`, so
        // one answer: a `dup` of the epoll instance as target is this, not success.
        let epollState =
            match epollDescription.Target with
            | OpenFileTarget.Epoll epollState when epollId <> targetId -> Some epollState
            | OpenFileTarget.Epoll _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Pipe _ -> None

        match epollState with
        | None -> failed EpollCtlError.NotAnEventPort
        | Some epollState ->

        let targetIsPort =
            match targetDescription.Target with
            | OpenFileTarget.Epoll _ -> true
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Pipe _ -> false

        // The EPOLLEXCLUSIVE screen, ahead of the table (so ahead of EEXIST
        // and ENOENT) and applied to ADD and MOD only: 20000 random masks for
        // each of six (op, table state) rows found no exception to it.
        let exclusivePermitted =
            EpollEvents.In
            ||| EpollEvents.Out
            ||| EpollEvents.Err
            ||| EpollEvents.Hup
            ||| EpollEvents.WakeUp
            ||| EpollEvents.EdgeTriggered
            ||| EpollEvents.Exclusive

        let exclusive = events &&& EpollEvents.Exclusive <> 0u

        if
            exclusive
            && (op = modify
                || op = add && (targetIsPort || events &&& ~~~exclusivePermitted <> 0u))
        then
            failed EpollCtlError.ExclusiveNotPermitted
        // Where Linux runs its loop and depth checks, which cannot fail on any
        // table this library builds (it never holds a nested epoll instance) but which
        // precede EEXIST in the kernel.
        elif op = add && targetIsPort then
            Error (EpollCtlRefusal.NestedPort fd)
        else

        let key = fd, targetId
        let registered = Map.containsKey key epollState.Registrations

        // The modes this library's epoll does not model, refused only where the call
        // would otherwise commit.
        let unmodelledMode : EpollCtlRefusal option =
            if exclusive then
                Some EpollCtlRefusal.Exclusive
            elif events &&& EpollEvents.OneShot <> 0u then
                Some EpollCtlRefusal.OneShot
            elif events &&& EpollEvents.WakeUp <> 0u then
                Some EpollCtlRefusal.WakeUp
            elif events &&& EpollEvents.EdgeTriggered = 0u then
                Some EpollCtlRefusal.LevelTriggered
            else
                None

        // `epoll_ctl` forces these two into every stored mask (measured through
        // `/proc/self/fdinfo`, `fdinfo.c`).
        let stored = events ||| EpollEvents.Err ||| EpollEvents.Hup

        let withRegistry (registry : FileDescriptorRegistry) (system : UnixSystem<'Task, 'Handler>) =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        // An ADD or MOD whose target is ready under the new mask makes the
        // registration pending at that moment (measured rows E, I and K), and
        // a MOD of an entry already pending leaves its place alone (row L).
        let pendIfReady (system : UnixSystem<'Task, 'Handler>) : UnixSystem<'Task, 'Handler> =
            let alreadyPending = List.contains key epollState.Ready

            if
                not alreadyPending
                && LinuxReadiness.ofDescription targetId system &&& stored <> 0u
            then
                withRegistry (FileDescriptorRegistry.appendEpollReady epollId key system.Process.FileDescriptors) system
            else
                system

        // A pipe the process was launched with is answered: its far end is the
        // client's, so every wake it can signal is one this kernel knows. The
        // read end of a pipe the client supplies is woken by the client's
        // write into the pipe a read emptied, and by its close, which
        // `UnixReadWrite.read` signals (measured, supplied-pipe-epoll.c);
        // once the client has closed, nothing wakes it, since a read wakes the
        // read end's waiters only after it slept, and none here does (measured
        // there too: reads of a pipe whose writer had closed woke nothing). A
        // drained pipe's read end is the client's, and its write end is woken
        // only by the client's drain of a write that filled it, which
        // `UnixReadWrite.write` signals (measured, drained-pipe-epoll.c). A
        // pipe whose reader was gone before the process started never changes:
        // nothing is ever taken into it or out of it.
        let targetIsPipe =
            match targetDescription.Target with
            | OpenFileTarget.Pipe (pipeId, _) ->
                match (UnixMachineState.pipe pipeId system.Machine).Origin with
                | PipeOrigin.Made _ -> true
                | PipeOrigin.Launched _ -> false
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _ -> false

        if op = add then
            if registered then
                failed EpollCtlError.AlreadyRegistered
            else

            match unmodelledMode with
            | Some refusal -> Error refusal
            | None ->

            if targetIsPipe then
                Error (EpollCtlRefusal.PipeTarget fd)
            else

            let ordinal = system.Machine.NextEventRegistrationOrdinal

            let registration =
                {
                    Events = stored
                    Data = data
                    RegisteredAt = ordinal
                }

            let system =
                { withRegistry (FileDescriptorRegistry.addEpollRegistration epollId key registration registry) system with
                    Machine =
                        { system.Machine with
                            NextEventRegistrationOrdinal = ordinal + 1L
                        }
                }

            Ok (EpollCtlAnswer.Changed, pendIfReady system)
        elif op = del then
            if registered then
                Ok (
                    EpollCtlAnswer.Changed,
                    withRegistry (FileDescriptorRegistry.removeEpollRegistration epollId key registry) system
                )
            else
                failed EpollCtlError.NotRegistered
        elif op = modify then
            if not registered then
                failed EpollCtlError.NotRegistered
            else

            match unmodelledMode with
            | Some refusal -> Error refusal
            | None ->

            // No stored mask here carries EPOLLEXCLUSIVE, whose MOD the kernel
            // would answer EINVAL, because an exclusive ADD is refused.
            let system =
                withRegistry (FileDescriptorRegistry.modifyEpollRegistration epollId key stored data registry) system

            Ok (EpollCtlAnswer.Changed, pendIfReady system)
        else
            failed EpollCtlError.UnrecognisedOperation

    // Linux's `<poll.h>` numbering: the bits a Linux-flavoured `poll(2)` reads
    // in `events` and writes in `revents`.
    //
    // Measured 2026-09-23 on Linux 6.18.5 aarch64 (glibc 2.41) by
    // `docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c`, which
    // printed the header and then polled every object and state
    // `LinuxReadiness.ofDescription` answers for with all 65536 request masks
    // at timeout 0. On every object, every answer was `level & (events |
    // POLLERR | POLLHUP)` and `rv` counted exactly the entries with a non-zero
    // `revents`; a descriptor that is not open answered `POLLNVAL` alone to all
    // 65536; no request failed.
    //
    // No modelled object presents `POLLPRI`, `POLLRDBAND` or `POLLMSG`.
    // `POLLREMOVE` (0x1000), the unassigned
    // 0x0800 and the kernel-internal 0x4000 and 0x8000 were never reported
    // either, which is the shape of `do_pollfd`: it reads a request through
    // `demangle_poll`, which maps only the named bits other than `POLLREMOVE`,
    // so the rest never reach the filter the level is masked by. That held
    // for 0x8000 even on a socket with `SO_BUSY_POLL` set, the one case in
    // which a socket's own poll handler adds that bit to its mask.
    let private linuxPollErr : int16 = 0x0008s
    let private linuxPollHup : int16 = 0x0010s
    let private linuxPollNval : int16 = 0x0020s

    /// What `entry` reports right now under the Linux flavour, as `do_pollfd`
    /// computes it: by looking its descriptor up afresh.
    let private reportOne<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        (entry : PollEntry)
        : Result<int16, PollRefusal>
        =
        if entry.Fd < 0 then
            // Measured on both kernels: a negative descriptor is ignored,
            // reports nothing, and does not count towards the return value.
            // It is not an error and not NVAL.
            Ok 0s
        else

        match FileDescriptorRegistry.tryFindWithId entry.Fd system.Process.FileDescriptors with
        | None ->
            // POLLNVAL is a statement about the entry, not a readiness
            // level: measured, it is reported alone, whatever was asked
            // for, `events = 0` included.
            Ok linuxPollNval
        | Some (descriptionId, description) ->

        match description.Target with
        // Measured on Linux (`poll-alphabet.c`): POLLIN|POLLRDNORM when an
        // event is deliverable, nothing otherwise, under the same
        // `level & (events | POLLERR | POLLHUP)` rule. Refused anyway,
        // because the kernel computes that level by re-polling the ready
        // list, and whether the walk drops a stale entry, as `drain` does,
        // is unmeasured.
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _ -> Error (PollRefusal.UnmodelledTarget entry.Fd)
        | OpenFileTarget.Socket _
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.CharacterDevice _
        | OpenFileTarget.Pipe _ ->
            // `do_pollfd`'s own shape: the level, filtered by the request
            // with POLLERR and POLLHUP added whatever was asked.
            // The level's bits all lie below 0x10000, where `<poll.h>`
            // and `<sys/epoll.h>` share their numbering.
            int16 (LinuxReadiness.ofDescription descriptionId system)
            &&& (entry.Events ||| linuxPollErr ||| linuxPollHup)
            |> Ok

    /// Every entry's report, in list order, stopping at the first entry that
    /// cannot be answered: a real `poll` inspects its entries in order, so that
    /// is the entry a refusal names.
    let private scan<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        (entries : PollEntry list)
        : Result<int16 list * int, PollRefusal>
        =
        let rec report (remaining : PollEntry list) (acc : int16 list) : Result<int16 list, PollRefusal> =
            match remaining with
            | [] -> Ok (List.rev acc)
            | entry :: rest ->
                match reportOne system entry with
                | Error refusal -> Error refusal
                | Ok events -> report rest (events :: acc)

        match report entries [] with
        | Error refusal -> Error refusal
        | Ok reported ->
            let triggered =
                reported |> List.filter (fun revents -> revents <> 0s) |> List.length

            Ok (reported, triggered)

    let private nanosecondsPerMillisecond : int64 = 1_000_000L

    /// The deadline a relative timeout of `milliseconds` sets at `now`, as both
    /// `poll(2)` and `epoll_wait(2)` read one: `None` for a negative timeout,
    /// which is infinite; and an `Error` for a positive one whose deadline the
    /// clock cannot represent.
    ///
    /// Measured (`poll-timeout.c` and `epoll-wait.c`): Linux turns a timeout of
    /// `ms` into a deadline `ms` milliseconds from now on the monotonic clock,
    /// and never returns before it; every negative timeout is infinite.
    /// Returning at the deadline is this library's answer; a real wait returns
    /// at or a little after it.
    ///
    /// Must not be asked of a timeout of 0, which never sleeps at all.
    let private relativeDeadline (now : int64) (milliseconds : int) : Result<int64 option, unit> =
        if milliseconds = 0 then
            failwith
                "UnixPoll.relativeDeadline: a timeout of 0 answers at once and sets no deadline (this is a bug in this library)."
        elif milliseconds < 0 then
            Ok None
        else
            let timeout = int64 milliseconds * nanosecondsPerMillisecond

            // Linux saturates such a deadline and so waits for ever, by reading
            // of its source rather than by measurement, which would take 292
            // years of uptime; so the caller refuses rather than answers.
            if now > System.Int64.MaxValue - timeout then
                Error ()
            else
                Ok (Some (now + timeout))

    /// A Linux-flavoured `poll(2)`, after the parked-task check: see `poll`.
    let private linuxPoll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        match scan system entries with
        | Error refusal -> Error refusal
        | Ok (reported, triggered) ->

        if triggered > 0 || milliseconds = 0 then
            Ok (PollOutcome.Answered (reported, triggered), system)
        else

        // Every entry carries nothing, so every non-negative descriptor is open
        // (a closed one would carry NVAL): the park records the description
        // each named, which is what a real poll sleeps on.
        let parkedEntries =
            entries
            |> List.map (fun entry ->
                if entry.Fd < 0 then
                    ParkedPollEntry.Ignored entry.Fd
                else
                    match FileDescriptorRegistry.tryFindId entry.Fd system.Process.FileDescriptors with
                    | Some description -> ParkedPollEntry.Watched (entry.Fd, description, entry.Events)
                    | None ->
                        failwith
                            $"UnixPoll.poll: fd %d{entry.Fd} reported nothing but names no open file description, where a closed descriptor reports POLLNVAL (this is a bug in this library)."
            )

        let now = system.Machine.NanosecondsSinceBoot

        match relativeDeadline now milliseconds with
        | Error () -> Error (PollRefusal.DeadlineBeyondClock (now, milliseconds))
        | Ok deadline ->

        let parked =
            ParkedSyscall.Poll
                {
                    Entries = parkedEntries
                    Deadline = deadline
                }

        Ok (PollOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked system)

    /// Finish the Linux-flavoured `poll` `task` is parked in: see `finishPoll`.
    let private finishLinuxPoll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (parked : ParkedPoll)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        let descriptions =
            FileDescriptorRegistry.descriptions system.Process.FileDescriptors

        let entries =
            parked.Entries
            |> List.map (fun entry ->
                match entry with
                | ParkedPollEntry.Ignored fd ->
                    {
                        PollEntry.Fd = fd
                        Events = 0s
                    }
                | ParkedPollEntry.Watched (fd, description, events) ->
                    if not (Map.containsKey description descriptions) then
                        failwith
                            $"UnixPoll.finishPoll: task %O{task}'s poll watches open file description %O{description}, which is not in the table, but a park holds its descriptions until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."

                    match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                    | Some current when current = description ->
                        {
                            PollEntry.Fd = fd
                            Events = events
                        }
                    | other ->
                        failwith
                            $"UnixPoll.finishPoll: task %O{task}'s poll watches fd %d{fd} on open file description %O{description}, but fd %d{fd} now names %A{other}. `close` refuses to close a descriptor a parked poll watches (this is a bug in this library, or in a caller that closed it without UnixDescriptor.close)."
            )

        match scan system entries with
        | Error refusal -> Error refusal
        | Ok (reported, triggered) ->

        let timedOut =
            match parked.Deadline with
            | Some deadline -> system.Machine.NanosecondsSinceBoot >= deadline
            | None -> false

        let finished =
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }

        // Measured on Linux 6.18.5 (`signal-interrupt-requeue.c`, sections D
        // and E), with the sleeper held off the CPU until both held: a ready
        // descriptor beats a pending signal, and a pending signal beats an
        // expired deadline, whichever came first.
        if triggered > 0 then
            SyscallInterruption.beforeCompleting task system
            |> Result.mapError PollRefusal.Interruption
            |> Result.map (fun () -> PollOutcome.Answered (reported, triggered), finished)
        else

        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (PollRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> Ok (PollOutcome.Failed UnixError.EINTR, finished)
        | Ok (Some SyscallInterruption.Restart) ->
            failwith
                "UnixPoll.finishPoll: a poll restarted after a signal, where `SyscallInterruption.ruleOf` says a poll never restarts (this is a bug in this library)."
        | Ok None ->

        if timedOut then
            Ok (PollOutcome.Answered (reported, triggered), finished)
        else
            let parkedAgain = ParkedSyscall.Poll parked
            Ok (PollOutcome.WouldBlock (WakeCondition.ofPark parkedAgain), UnixWait.park task parkedAgain system)

    // Darwin's limits on how many entries one call takes (`OPEN_MAX` and
    // `FD_SETSIZE` in `poll_nocancel`), measured in `poll-darwin.c` section N.
    let private darwinOpenMax = 10240
    let private darwinFdSetSize = 1024

    /// What registering one group of an entry's filters does.
    [<RequireQualifiedAccess>]
    type private DarwinRegistration =
        /// The registration fails, and the entry answers `POLLNVAL`.
        | Fails
        /// The filter registers.
        | Registers of KqueueFilter
        /// An `EVFILT_VNODE` registers on a regular file or a directory. It
        /// reports only when the file changes, which nothing here does while
        /// a call runs, so it reports nothing to a call that does not sleep.
        | RegistersVnode

    /// Which groups of filters `events` registers, in the order `poll`
    /// registers them: `EVFILT_READ` (with `EV_OOBAND` when `true`),
    /// `EVFILT_WRITE`, then `EVFILT_VNODE`.
    let private darwinGroups (events : int16) : (KqueueFilter * bool) option list =
        [
            if events &&& DarwinPollEvents.ReadGroup <> 0s then
                Some (KqueueFilter.Read, events &&& DarwinPollEvents.OutOfBandGroup <> 0s)
            if events &&& DarwinPollEvents.WriteGroup <> 0s then
                Some (KqueueFilter.Write, false)
            if events &&& DarwinPollEvents.VnodeGroup <> 0s then
                None
        ]

    /// What registering `group` (a read or write filter, or `None` for
    /// `EVFILT_VNODE`) through the descriptor `fd` does.
    let private darwinRegistration<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (group : KqueueFilter option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<DarwinRegistration, PollRefusal>
        =
        // Measured on Darwin 27.0.0 (`poll-darwin.c` section S), each row of
        // which agreed with XNU's own filters: a descriptor that is not open
        // registers nothing (EBADF); `EVFILT_VNODE` registers on a vnode
        // alone, so on a regular file or a directory, and fails (EINVAL) on a
        // socket, a pipe and a kqueue; read and write filters fail on a
        // directory, and a kqueue takes a read filter but not a write one.
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors, group with
        | None, _ -> Ok DarwinRegistration.Fails
        | Some (OpenFileTarget.Socket _), None
        | Some (OpenFileTarget.Pipe _), None
        | Some (OpenFileTarget.Kqueue _), None -> Ok DarwinRegistration.Fails
        | Some (OpenFileTarget.File _), None
        | Some (OpenFileTarget.Directory _), None -> Ok DarwinRegistration.RegistersVnode
        | Some (OpenFileTarget.Directory _), Some _ -> Ok DarwinRegistration.Fails
        | Some (OpenFileTarget.File _), Some filter
        | Some (OpenFileTarget.Pipe _), Some filter -> Ok (DarwinRegistration.Registers filter)
        | Some (OpenFileTarget.Socket socketId), Some filter ->
            let socket = UnixMachineState.socket socketId system.Machine

            if DarwinReadiness.modelsSocket socket then
                Ok (DarwinRegistration.Registers filter)
            else
                Error (PollRefusal.UnmodelledSocket (fd, socket.Domain, socket.Kind))
        | Some (OpenFileTarget.Kqueue _), Some KqueueFilter.Write -> Ok DarwinRegistration.Fails
        | Some (OpenFileTarget.Kqueue _), Some KqueueFilter.Read -> Error (PollRefusal.UnmodelledTarget fd)
        | Some (OpenFileTarget.Epoll _), _ ->
            failwith
                $"UnixPoll.poll: fd %d{fd} names an epoll instance, which a Darwin-flavoured kernel cannot hold (this is a bug in the caller's state construction)."
        | Some (OpenFileTarget.CharacterDevice _), _ ->
            failwith
                $"UnixPoll.poll: fd %d{fd} names a character device, which a Darwin-flavoured kernel cannot hold: its device filesystem is not modelled, so no path opens one (this is a bug in the caller's state construction)."

    /// Register every entry's filters, as `poll_nocancel` does before it scans:
    /// each entry's `revents` so far (`POLLNVAL` for an entry whose
    /// registration failed, 0 otherwise), the registrations, and the
    /// descriptors an `EVFILT_VNODE` registered on.
    let private darwinRegister<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (entries : PollEntry list)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int16 list * Map<int * KqueueFilter, PollRegistration> * int list, PollRefusal>
        =
        let rec registerGroups
            (index : int)
            (entry : PollEntry)
            (groups : (KqueueFilter * bool) option list)
            (registrations : Map<int * KqueueFilter, PollRegistration>)
            (vnodes : int list)
            : Result<bool * Map<int * KqueueFilter, PollRegistration> * int list, PollRefusal>
            =
            match groups with
            | [] -> Ok (false, registrations, vnodes)
            | group :: rest ->

            match darwinRegistration entry.Fd (group |> Option.map fst) system with
            | Error refusal -> Error refusal
            // The first group that fails ends the entry's registration, and the
            // groups registered before it stay registered (measured,
            // `poll-darwin.c` M3: `IN|EXTEND` on a socket answers `IN|NVAL`
            // beside another entry).
            | Ok DarwinRegistration.Fails -> Ok (true, registrations, vnodes)
            | Ok DarwinRegistration.RegistersVnode -> registerGroups index entry rest registrations (entry.Fd :: vnodes)
            | Ok (DarwinRegistration.Registers filter) ->
                let outOfBand =
                    match group with
                    | Some (_, outOfBand) -> outOfBand
                    | None -> false

                let key = entry.Fd, filter

                // A pair an earlier entry registered is registered again, which
                // keeps its flags and its place and gives it this entry
                // (measured, `poll-darwin.c` M1 and M17-M24).
                let registration =
                    match Map.tryFind key registrations with
                    | Some existing ->
                        { existing with
                            Entry = index
                        }
                    | None ->
                        {
                            Entry = index
                            OutOfBand = outOfBand
                            RegisteredAt = Map.count registrations
                        }

                registerGroups index entry rest (Map.add key registration registrations) vnodes

        let folded =
            ((Ok ([], Map.empty, [])
             : Result<int16 list * Map<int * KqueueFilter, PollRegistration> * int list, PollRefusal>),
             List.indexed entries)
            ||> List.fold (fun state (index, entry) ->
                match state with
                | Error refusal -> Error refusal
                | Ok (revents, registrations, vnodes) ->

                if entry.Fd < 0 then
                    Ok (0s :: revents, registrations, vnodes)
                else

                match registerGroups index entry (darwinGroups entry.Events) registrations vnodes with
                | Error refusal -> Error refusal
                | Ok (failed, registrations, vnodes) ->
                    let answer = if failed then DarwinPollEvents.Nval else 0s
                    Ok (answer :: revents, registrations, vnodes)
            )

        folded
        |> Result.map (fun (revents, registrations, vnodes) -> List.rev revents, registrations, List.rev vnodes)

    /// How many entries carry anything: `poll`'s return value.
    let private triggered (revents : int16 list) : int =
        revents |> List.filter (fun revents -> revents <> 0s) |> List.length

    /// A Darwin-flavoured `poll(2)`, after the parked-task check: see `poll`.
    let private darwinPoll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        let count = List.length entries

        // Measured (`poll-darwin.c` section N): above `OPEN_MAX` is EINVAL
        // whatever the process's limit and whatever the buffer, ahead of the
        // copy-in's EFAULT.
        if count > darwinOpenMax then
            Ok (PollOutcome.Failed UnixError.EINVAL, system)
        elif count > darwinFdSetSize then
            Error (PollRefusal.UnmodelledEntryCount count)
        else

        match darwinRegister entries system with
        | Error refusal -> Error refusal
        | Ok (revents, registrations, vnodes) ->

        // `poll_nocancel` scans only if some entry registered, so a call whose
        // every entry failed answers POLLNVAL for each and nothing else
        // (measured, `poll-darwin.c` M4), where a failure beside an entry that
        // did not fail -- a negative descriptor included -- still lets the
        // failing entry's earlier filters report (M3, M5).
        let failedAll =
            count > 0
            && revents |> List.forall (fun revents -> revents = DarwinPollEvents.Nval)

        if failedAll then
            Ok (PollOutcome.Answered (revents, count), system)
        else

        let active = KqueuePoll.activeAtRegistration registrations system

        let revents, registrations, active =
            KqueuePoll.scan entries revents registrations active system

        let reported = triggered revents

        // A failed registration makes the scan immediate, but it has already
        // put POLLNVAL in that entry, so it is counted here too.
        if reported > 0 || milliseconds = 0 then
            Ok (PollOutcome.Answered (revents, reported), system)
        elif milliseconds < -1 then
            Error (PollRefusal.UnmeasuredNegativeTimeout milliseconds)
        else

        match vnodes with
        | fd :: _ -> Error (PollRefusal.UnmodelledVnodeWait fd)
        | [] ->

        let now = system.Machine.NanosecondsSinceBoot

        let deadline =
            if milliseconds = -1 then
                Ok None
            else
                relativeDeadline now milliseconds

        match deadline with
        | Error () -> Error (PollRefusal.DeadlineBeyondClock (now, milliseconds))
        | Ok deadline ->

        let parked =
            ParkedSyscall.KqueuePoll
                {
                    Entries = entries
                    Registrations = registrations
                    Active = active
                    Deadline = deadline
                }

        Ok (PollOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked system)

    /// Finish the Darwin-flavoured `poll` `task` is parked in: see `finishPoll`.
    let private finishDarwinPoll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (parked : ParkedKqueuePoll)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        let zero = parked.Entries |> List.map (fun _ -> 0s)

        let revents, registrations, active =
            KqueuePoll.scan parked.Entries zero parked.Registrations parked.Active system

        let reported = triggered revents

        let timedOut =
            match parked.Deadline with
            | Some deadline -> system.Machine.NanosecondsSinceBoot >= deadline
            | None -> false

        let finished =
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }

        // A woken Darwin wait answers whichever of its wake-ups reached it
        // first -- a report, its deadline or a signal -- and this library does
        // not record which; `SyscallInterruption.beforeCompleting` refuses a
        // signal pending beside an answer, as it does for `kevent`.
        if reported > 0 && timedOut then
            Error PollRefusal.EventsBesideDeadline
        elif reported > 0 then
            SyscallInterruption.beforeCompleting task system
            |> Result.mapError PollRefusal.Interruption
            |> Result.map (fun () -> PollOutcome.Answered (revents, reported), finished)
        elif timedOut then
            SyscallInterruption.beforeCompleting task system
            |> Result.mapError PollRefusal.Interruption
            |> Result.map (fun () -> PollOutcome.Answered (zero, 0), finished)
        else

        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (PollRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> Ok (PollOutcome.Failed UnixError.EINTR, finished)
        | Ok (Some SyscallInterruption.Restart) ->
            failwith
                "UnixPoll.finishPoll: a poll restarted after a signal, where `SyscallInterruption.ruleOf` says a poll never restarts (this is a bug in this library)."
        | Ok None ->
            // Whatever woke the task has gone again. Its scan consumed what
            // reported without adding anything, and dropped what is no longer
            // ready, as a woken real poll's does, and it sleeps on.
            let parkedAgain =
                ParkedSyscall.KqueuePoll
                    { parked with
                        Registrations = registrations
                        Active = active
                    }

            Ok (PollOutcome.WouldBlock (WakeCondition.ofPark parkedAgain), UnixWait.park task parkedAgain system)

    /// `poll(2)`: what each entry reports, and how many entries carry anything;
    /// or, when nothing does and the timeout lets it, the calling task sleeps.
    ///
    /// Each entry's `Events`, and each `revents` answered for it, is the raw
    /// bits in the simulated flavour's own `<poll.h>` numbering. The count is
    /// `poll(2)`'s own return value, and it is neither the number of entries
    /// nor the number of *conditions*: it counts entries carrying something.
    /// A negative descriptor reports nothing and is not counted.
    ///
    /// **Under Linux** every bit is answered as a real kernel answers it: each
    /// named bit is reported when the descriptor presents it and the entry
    /// asked for it, `POLLERR` and `POLLHUP` whether asked for or not, and
    /// `POLLNVAL` alone for a descriptor that is not open. A bit Linux does not
    /// read (`POLLREMOVE`, 0x0800, 0x4000 and 0x8000) is ignored, as it is
    /// there. An entry naming an epoll instance is refused.
    ///
    /// **Under Darwin** the call is answered as XNU builds it, over a kqueue it
    /// makes for its own use: each entry registers `EVFILT_READ` for any of
    /// `IN|RDNORM|PRI|RDBAND|HUP`, `EVFILT_WRITE` for any of `OUT|WRBAND`, and
    /// `EVFILT_VNODE` for any of the vnode bits (see `DarwinPollEvents`); an
    /// entry whose registration fails answers `POLLNVAL`; and each filter's
    /// report is folded into its entry by `KqueuePoll.callback`. So a request
    /// of none of those bits reports nothing, even for a descriptor that is not
    /// open; one descriptor named by several entries reports into the last of
    /// them alone; and a reported hang-up suppresses `POLLOUT`. A call with
    /// more than 10240 entries fails with `EINVAL`. Refused (see
    /// `PollRefusal`): more than 1024 entries and at most 10240; a socket
    /// whose filters are not modelled; a kqueue asked for a read bit; and a
    /// call that would sleep with a vnode filter registered, or with a
    /// negative timeout other than -1.
    ///
    /// `milliseconds` is read as the flavour reads it. Zero answers now. A
    /// positive timeout, when nothing is reported, parks `task` until something
    /// is or `milliseconds` have passed on the machine's monotonic clock,
    /// whichever is first; at the deadline and not before, the call finishes
    /// with 0. -1 is infinite on both, and under Linux so is every negative
    /// timeout. A wait with nothing to watch and no deadline parks until a
    /// signal ends it. A foreign-function layer that screens some negative
    /// values itself does that before calling.
    ///
    /// A poll with anything to report is answered at every timeout. The system
    /// comes back unchanged unless the task parked.
    ///
    /// `task` must not already be parked: a task blocks in one syscall at a
    /// time, and a parked `poll` is finished with `finishPoll`.
    let poll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some parked ->
            failwith
                $"UnixPoll.poll: task %O{task} is parked in %A{parked}, and is issuing a poll. A task blocks in one syscall at a time; a parked poll is finished with `finishPoll` (this is a bug in the client)."
        | None ->

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> linuxPoll task entries milliseconds system
        | SimulatedUnixFlavour.Darwin -> darwinPoll task entries milliseconds system

    /// Finish the `poll` `task` parked in, as a woken real poll does, and
    /// answer.
    ///
    /// **Under Linux** it scans the entries the call was made with, not
    /// whatever the caller's array holds now, looking each descriptor up
    /// afresh; `close` refuses to close one a parked poll watches, so each
    /// still names the description the call went to sleep on. It answers the
    /// count when any entry carries anything, whether or not the deadline has
    /// passed or a signal is pending too; `Failed EINTR` when a signal with a
    /// handler interrupts it, whether or not the deadline has passed; and 0,
    /// with every `revents` 0, when only the deadline has.
    ///
    /// **Under Darwin** it scans the kqueue the call made for itself, which a
    /// close of a watched descriptor has left without that descriptor's
    /// filters. It answers the count when anything is reported, 0 when the
    /// deadline has passed, and `Failed EINTR` when a signal with a handler
    /// interrupts it; and refuses each pair of those that hold at once, since
    /// Darwin answers whichever reached the sleeping task first.
    ///
    /// Otherwise it re-parks the task on the same call and deadline, since
    /// whatever woke it has gone again. An answer clears the park.
    ///
    /// `task` must be parked in a `poll`.
    let finishPoll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some (ParkedSyscall.Poll parked as held) ->
            // `close` refuses to close a descriptor a parked Linux poll
            // watches, so each description the call held is still named by its
            // descriptor; released all the same, as every finishing call
            // releases what its park held.
            finishLinuxPoll task parked system
            |> Result.map (fun (outcome, after) ->
                outcome,
                ObjectLifetime.releaseUnreferencedUnrefusable
                    "UnixPoll.finishPoll"
                    (ParkedSyscall.descriptions held)
                    after
            )
        // A Darwin poll holds no description (see `ParkedSyscall.descriptions`),
        // so its return releases nothing.
        | Some (ParkedSyscall.KqueuePoll parked) -> finishDarwinPoll task parked system
        | Some other ->
            failwith
                $"UnixPoll.finishPoll: task %O{task} is parked in %A{other}, not in a poll, so there is no poll to finish (this is a bug in the client)."
        | None ->
            failwith
                $"UnixPoll.finishPoll: task %O{task} is not parked, so there is no poll to finish. Only a task `poll` answered `WouldBlock` finishes here (this is a bug in the client)."

    /// `epoll_create1(2)`: create an epoll instance and a descriptor onto it, the
    /// lowest one not in use.
    ///
    /// `flags` is Linux's: 0, or `EpollCreateFlags.CloseOnExec`, which is
    /// accepted and has no effect here, since it sets `FD_CLOEXEC`, which
    /// matters only across `exec`, and this kernel models neither `exec` nor any
    /// per-descriptor flag. Any other bit is `EINVAL`, and changes nothing.
    ///
    /// Under the Darwin flavour every call is refused: Darwin has no epoll.
    let epollCreate1<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<int * UnixSystem<'Task, 'Handler>, UnixError>, EpollCreateRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (EpollCreateRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        // Measured on 6.18.5 (`epoll-wait.c`, section A): 0 and EPOLL_CLOEXEC
        // create an epoll instance; every other single bit, EPOLL_CLOEXEC beside any other
        // bit, -1 and INT_MIN are EINVAL.
        if flags &&& ~~~EpollCreateFlags.CloseOnExec <> 0 then
            Ok (Error UnixError.EINVAL)
        else

        let fd, registry = FileDescriptorRegistry.createEpoll system.Process.FileDescriptors

        Ok (
            Ok (
                fd,
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors = registry
                        }
                }
            )
        )

    /// Whether the events `delivered` can be copied out to `buffer`: a call
    /// that delivers nothing copies nothing, and so never looks at the buffer.
    let private copyOut
        (epoll : OpenFileDescriptionId)
        (buffer : UserBuffer)
        (delivered : (uint64 * uint32) list)
        : Result<unit, EpollWaitRefusal>
        =
        if List.isEmpty delivered then
            Ok ()
        else
            match buffer with
            | UserBuffer.Mapped -> Ok ()
            | UserBuffer.Unmapped _ -> Error (EpollWaitRefusal.UnmeasuredCopyOutFault epoll)
            | UserBuffer.Opaque -> Error (EpollWaitRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (EpollWaitRefusal.Buffer BufferRefusal.AddresslessAtTransfer)

    /// Park `task` in a wait on the epoll instance `epoll` for up to
    /// `maxEvents` events, until `deadline`.
    let private parkEpollWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (epoll : OpenFileDescriptionId)
        (maxEvents : int)
        (buffer : UserBuffer)
        (deadline : int64 option)
        (system : UnixSystem<'Task, 'Handler>)
        : EpollWaitOutcome * UnixSystem<'Task, 'Handler>
        =
        let parked =
            ParkedSyscall.EpollWait
                {
                    Epoll = epoll
                    MaxEvents = maxEvents
                    Buffer = buffer
                    Deadline = deadline
                }

        EpollWaitOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked system

    /// `epoll_wait(2)`, made by `task`: take up to `maxEvents` events off the
    /// epoll instance `epfd` names, sleeping for up to `milliseconds` if it has
    /// none.
    ///
    /// Every argument check is answered, in Linux's order: `EBADF` for a
    /// descriptor that is not open, then `EINVAL` for a `maxEvents` that is not
    /// positive or exceeds the architecture's bound, then `EFAULT` for a buffer
    /// reaching into kernel space (see `UserBufferCheck.BeforeOperation`), then
    /// `EINVAL` for a descriptor that is not an epoll instance. A failure
    /// changes nothing.
    ///
    /// Delivery walks the epoll instance's pending registrations in order, reporting each
    /// one whose target is still ready and consuming each one walked, stale or
    /// not (see `EpollReadyList.drain`). A wait that finds something answers it
    /// whatever the timeout. One that finds nothing answers no events at once
    /// for a timeout of 0; parks `task` for a positive timeout until an event is
    /// deliverable or `milliseconds` have passed on the machine's monotonic
    /// clock; and parks it until an event is deliverable for any negative
    /// timeout, which is infinite. A parked wait is finished with
    /// `finishEpollWait`. The walk's consumption stands whether or not the call
    /// then sleeps.
    ///
    /// A call that delivers events copies them out to `buffer`, which must be
    /// `Mapped`: this library does not answer for a copy that faults or a buffer
    /// whose bytes it cannot hold (see `EpollWaitRefusal`). A call that delivers
    /// nothing never looks at the buffer past the screen.
    ///
    /// Of several tasks parked on one epoll instance, one event wakes one of them (see
    /// `UnixWait.wakes`).
    ///
    /// `task` must not already be parked. Under the Darwin flavour every call
    /// is refused: Darwin has no epoll.
    let epollWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (epfd : int)
        (maxEvents : int)
        (buffer : UserBuffer)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollWaitOutcome * UnixSystem<'Task, 'Handler>, EpollWaitRefusal>
        =
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some parked ->
            failwith
                $"UnixPoll.epollWait: task %O{task} is parked in %A{parked}, and is issuing an epoll_wait. A task blocks in one syscall at a time; a parked wait is finished with `finishEpollWait` (this is a bug in the client)."
        | None ->

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (EpollWaitRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        match admitEpollWait epfd maxEvents buffer system with
        | Error refusal -> Error (EpollWaitRefusal.Buffer refusal)
        | Ok (Error error) -> Ok (EpollWaitOutcome.Failed error, system)
        | Ok (Ok epoll) ->

        let delivered, system = EpollReadyList.drain epoll maxEvents system

        // Measured (`epoll-wait.c`, sections B and C): a wait that finds
        // something answers at once whatever the timeout, and one that finds
        // nothing answers at once for a timeout of 0.
        if not (List.isEmpty delivered) || milliseconds = 0 then
            copyOut epoll buffer delivered
            |> Result.map (fun () -> EpollWaitOutcome.Answered delivered, system)
        else

        let now = system.Machine.NanosecondsSinceBoot

        match relativeDeadline now milliseconds with
        | Error () -> Error (EpollWaitRefusal.DeadlineBeyondClock (now, milliseconds))
        | Ok deadline -> Ok (parkEpollWait task epoll maxEvents buffer deadline system)

    let private finishEpollWaitHolding<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollWaitOutcome * UnixSystem<'Task, 'Handler>, EpollWaitRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.EpollWait parked) -> parked
            | Some other ->
                failwith
                    $"UnixPoll.finishEpollWait: task %O{task} is parked in %A{other}, not in an epoll_wait, so there is no wait to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixPoll.finishEpollWait: task %O{task} is not parked, so there is no wait to finish. Only a task `epollWait` answered `WouldBlock` finishes here (this is a bug in the client)."

        let delivered, system = EpollReadyList.drain parked.Epoll parked.MaxEvents system

        let timedOut =
            match parked.Deadline with
            | Some deadline -> system.Machine.NanosecondsSinceBoot >= deadline
            | None -> false

        let finished =
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }

        // Measured on Linux 6.18.5 (`signal-interrupt-requeue.c`, sections D
        // and E), with the sleeper held off the CPU until both held: an event
        // beats a pending signal, and so does an expired deadline, whichever
        // came first.
        if not (List.isEmpty delivered) || timedOut then
            SyscallInterruption.beforeCompleting task system
            |> Result.mapError EpollWaitRefusal.Interruption
            |> Result.bind (fun () -> copyOut parked.Epoll parked.Buffer delivered)
            |> Result.map (fun () -> EpollWaitOutcome.Answered delivered, finished)
        else

        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (EpollWaitRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> Ok (EpollWaitOutcome.Failed UnixError.EINTR, finished)
        | Ok (Some SyscallInterruption.Restart) ->
            failwith
                "UnixPoll.finishEpollWait: an epoll_wait restarted after a signal, where `SyscallInterruption.ruleOf` says one never restarts (this is a bug in this library)."
        | Ok None -> Ok (parkEpollWait task parked.Epoll parked.MaxEvents parked.Buffer parked.Deadline system)

    /// Finish the wait on an epoll instance that `task` is parked in: walk
    /// the epoll instance again, as a woken real wait does, and answer.
    ///
    /// Delivers from the epoll instance the call was made on and with the `maxEvents` it
    /// was made with, not whatever the caller's arguments hold now: the parked
    /// call holds the epoll instance's open file description, which outlives its last
    /// descriptor until the call returns, and goes then.
    ///
    /// Answers the events it finds, whether or not the deadline has passed or a
    /// signal is pending too (measured, `epoll-wait.c` section E: an event and
    /// an expired deadline both holding as the waiter runs report the event);
    /// no events when the deadline has passed, whether or not a signal is
    /// pending; `Failed EINTR` when a signal with a handler interrupts it; and
    /// otherwise re-parks the task on the same epoll instance and deadline, since
    /// whatever woke it has gone again. A re-park goes to the back of park
    /// order, which puts it first in line for the epoll instance's next event. An answer
    /// clears the park.
    ///
    /// Delivering events copies them out to the buffer the call was made with;
    /// see `epollWait` for the buffers that refuses.
    ///
    /// `task` must be parked in an `epoll_wait`.
    let finishEpollWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollWaitOutcome * UnixSystem<'Task, 'Handler>, EpollWaitRefusal>
        =
        let held =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some parked -> ParkedSyscall.descriptions parked
            | None -> []

        // The call's reference to the epoll instance goes as it returns, and with it the
        // epoll instance, if no descriptor names it any more (`open-file-references.c`
        // section E).
        finishEpollWaitHolding task system
        |> Result.map (fun (outcome, after) ->
            outcome, ObjectLifetime.releaseUnreferencedUnrefusable "UnixPoll.finishEpollWait" held after
        )
