namespace WoofWare.PosixKernel

/// One entry of a `poll(2)` call, as its caller supplied it: `struct pollfd`'s
/// `fd` and `events`, without the `revents` the kernel writes back.
type PollEntry =
    {
        /// The descriptor to poll. A negative one is not an error: measured on
        /// both kernels, it is ignored, reports nothing, and does not count
        /// towards the return value.
        Fd : int
        /// What the caller asked about: `events`, as raw bits in the simulated
        /// flavour's own `<poll.h>` numbering. `POLLERR`, `POLLHUP` and
        /// `POLLNVAL` are reported whether or not they appear here, so a caller
        /// may leave them out and still be told about them.
        Events : int16
    }

/// Why this kernel will not answer a `poll`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type PollRefusal =
    /// This kernel models `poll(2)` for one flavour only, and it is not this
    /// one.
    ///
    /// Darwin's answer is not one level masked by the request, as Linux's is:
    /// what it reports depends on which bits were asked for together. A request
    /// carrying `POLLEXTEND`, `POLLATTRIB`, `POLLNLINK` or `POLLWRITE` on a
    /// socket answers `POLLNVAL`; a TCP socket whose peer has closed answers
    /// `POLLOUT` to a request for `POLLOUT` but `POLLIN|POLLHUP` to a request
    /// for `POLLIN|POLLOUT`; a request of 0 reports nothing, even for a
    /// descriptor that is not open. So it is a second model rather than an
    /// extra column.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The entry names a socket event port, which this kernel does not answer
    /// `poll(2)` for.
    ///
    /// Reachable in a way epoll's equivalent is not: `epoll_ctl` screens the
    /// targets it will accept, and `poll(2)` accepts any descriptor.
    | UnmodelledTarget of fd : int
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
    /// `poll` failed with this errno. Only a finishing call answers this: a
    /// poll that was asleep fails with `EINTR` when a signal with a handler
    /// interrupts it.
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
        | PollRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and `poll(2)` is modelled here for Linux only. Darwin's answer is not one level masked by the request: it registers a kqueue filter per group of requested bits, so which bits were asked together decides what is reported (a vnode bit on a socket answers POLLNVAL, a reported HUP suppresses OUT, and a request of 0 reports nothing even for a descriptor that is not open). Model that before polling under this flavour."
        | PollRefusal.UnmodelledTarget fd ->
            $"fd %d{fd} names a socket event port, which this kernel does not answer `poll(2)` for. Linux answers it by re-polling the port's ready list, as `epoll_wait` does, and what that walk leaves in the list is unmeasured; model that before answering."
        | PollRefusal.DeadlineBeyondClock (now, timeoutMilliseconds) ->
            $"the machine has been up for %d{now} ns and the timeout is %d{timeoutMilliseconds}ms, which ends past the last nanosecond the monotonic clock can represent. Linux's source saturates such a deadline, making the wait infinite, but that is unmeasured."
        | PollRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal

/// What a wait for socket events settles before it can either deliver or sleep:
/// `epoll_wait(2)`'s screens under one flavour, `kevent(2)`'s under the other.
///
/// Five of the eight measured rows differ between the two, so this is a
/// flavour-branching ladder throughout rather than in one place -- which is why
/// it is a kernel answer rather than something a client can assemble from parts.
[<RequireQualifiedAccess>]
type SocketWaitAdmission =
    /// The syscall was reached and failed. A client that keeps a last-error slot
    /// records this errno, and one whose foreign-function layer writes a
    /// sentinel through the caller's count does that too.
    | Failed of error : UnixError
    /// Answered with no events, having neither consulted the port nor slept.
    ///
    /// The one input on which the flavours disagree about whether the call
    /// blocks at all: measured, `kevent(kq, NULL, 0, evs, 0, NULL)` returns 0
    /// immediately, as it does for any negative count, where `epoll_wait` with
    /// `maxevents <= 0` is EINVAL.
    | NoEvents
    /// The call reaches the port: take up to `maxEvents` events off it, and
    /// sleep if that delivers nothing.
    | DeliverOrWait of port : OpenFileDescriptionId * maxEvents : int

/// Why this kernel will not answer a wait for socket events.
[<RequireQualifiedAccess>]
type SocketWaitRefusal =
    /// The buffer reached this platform's up-front address screen and has no
    /// address to screen.
    ///
    /// Only one flavour has such a screen -- Darwin's `kevent` checks no buffer
    /// at all, and a wait that never delivers never copies -- so this is
    /// reachable under Linux alone.
    | Buffer of BufferRefusal

[<RequireQualifiedAccess>]
module SocketWaitRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it actually passed.
    let describe (refusal : SocketWaitRefusal) : string =
        match refusal with
        | SocketWaitRefusal.Buffer refusal -> BufferRefusal.describe refusal

/// The flags `epoll_create1(2)` accepts, in Linux's numbering.
[<RequireQualifiedAccess>]
module EpollCreateFlags =
    /// `EPOLL_CLOEXEC`: set `FD_CLOEXEC` on the new descriptor. The same value
    /// as `O_CLOEXEC`.
    [<Literal>]
    let CloseOnExec : int = 0x80000

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
            $"this kernel is %O{flavour}-flavoured, and epoll_create1 exists on Linux only. A socket event port on this flavour is a kqueue, which this library does not create through a call of its own."

/// What became of an `epoll_wait(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type EpollWaitOutcome =
    /// `epoll_wait` failed with this errno.
    ///
    /// The call as first made changes nothing when it fails. A finishing call
    /// fails only with `EINTR`, when a signal with a handler interrupts the
    /// wait, and what its walk of the port consumed stays consumed.
    | Failed of error : UnixError
    /// `epoll_wait` returned these events, in delivery order: each the
    /// registration's `data` and the `events` written for it, in Linux's
    /// `<sys/epoll.h>` numbering (`EpollEvents`). Empty when the call timed
    /// out, or had a timeout of 0 and found nothing.
    | Answered of events : (uint64 * uint32) list
    /// `epoll_wait` did not return. The calling task is parked, and sleeps
    /// until `WakeCondition.satisfied` of this condition is non-empty and
    /// `UnixWait.wakes` wakes it; then `UnixPoll.finishSocketWait` finishes the
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
    | UnmeasuredCopyOutFault of port : OpenFileDescriptionId
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
            $"this kernel is %O{flavour}-flavoured, and epoll_wait exists on Linux only. A wait on this flavour's socket event port is a kevent, which this library does not answer through a call of its own."
        | EpollWaitRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | EpollWaitRefusal.UnmeasuredCopyOutFault port ->
            $"the socket event port %O{port} has events to deliver, so this call copies them out -- but the buffer is unmapped, so that copy faults. Which of the events the walk took stay pending after the fault, and whether the call answers EFAULT or the count copied before it, are unmeasured."
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
    /// Ahead of the not-a-port check, so a file as both port and target is
    /// `EPERM`.
    | TargetNotPollable
    /// `epfd` is not an epoll instance, or `epfd` and the target name the same
    /// open file description (a `dup` of the port included); `EINVAL`.
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
    /// This kernel models `epoll_ctl` for one flavour only, and it is not this
    /// one.
    ///
    /// kqueue's model is *structurally* different rather than differently
    /// numbered: registration is per `(ident, filter)`, a re-`ADD` silently
    /// replaces where epoll answers `EEXIST`, a regular file registers where
    /// epoll answers `EPERM`, and a `DEL` of a dead target answers `ENOENT`
    /// where epoll answers `EBADF`. Each of those is measured only far enough to
    /// know that it diverges, which is not far enough to model the state a call
    /// leaves behind.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// An `EPOLL_CTL_ADD` whose target is itself an epoll instance.
    ///
    /// Linux accepts one, subject to a loop check and a nesting depth of at
    /// most four ports (`ELOOP` beyond either). This library does not model
    /// what a nested port reports or how a wake propagates through it.
    | NestedPort of targetFd : int
    /// The event carries `EPOLLEXCLUSIVE`, which changes which of several ports
    /// on one target a wake reaches. This library's wakes reach every port.
    | Exclusive
    /// The event carries `EPOLLONESHOT`, which disarms the registration once it
    /// has reported. This library's registrations stay armed.
    | OneShot
    /// The event carries `EPOLLWAKEUP`. The kernel keeps it only for a caller
    /// with `CAP_BLOCK_SUSPEND` on a kernel built with power management, and
    /// silently clears it otherwise; this library models neither.
    | WakeUp
    /// The event lacks `EPOLLET`, asking to be level-triggered. This port
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
            $"this kernel is %O{flavour}-flavoured, and epoll_ctl is modelled here for Linux only. kqueue's semantics -- per-filter state, a silently-replacing ADD, file targets succeeding -- are unmeasured beyond the fact that they diverge from epoll's, and the return codes alone are not a model of the state a call leaves behind. Measure them before answering."
        | EpollCtlRefusal.NestedPort targetFd ->
            $"fd %d{targetFd} is itself an epoll instance. Linux would register it (subject to a loop check and a nesting depth of four, both ELOOP), but what a nested port reports and how a wake propagates through one are not modelled."
        | EpollCtlRefusal.Exclusive ->
            "the event carries EPOLLEXCLUSIVE, and the registration would succeed. An exclusive registration changes which of several ports on one target a wake reaches, and this library's wakes reach every port. Model wake-one delivery before answering."
        | EpollCtlRefusal.OneShot ->
            "the event carries EPOLLONESHOT, and the registration would succeed. A one-shot registration disarms once it has reported until a MOD re-arms it, and this library's registrations stay armed. Model disarming before answering."
        | EpollCtlRefusal.WakeUp ->
            "the event carries EPOLLWAKEUP, and the registration would succeed. The kernel keeps the bit only for a caller with CAP_BLOCK_SUSPEND on a kernel built with power management, clearing it silently otherwise, and this library models neither capabilities nor wakeup sources."
        | EpollCtlRefusal.PipeTarget targetFd ->
            $"fd %d{targetFd} is an end of a pipe the process made, and the registration would succeed. An edge-triggered registration is made pending by the wakes its target signals, and which reads, writes and closes signal a pipe's waiters, with which events, is unmeasured: Linux's pipe_write, for one, wakes readers on every write once a waiter has polled the pipe, not only on the write that makes it non-empty. poll(2) on a pipe is answered; measure the pipe's wakes before registering one."
        | EpollCtlRefusal.LevelTriggered ->
            "the event lacks EPOLLET, asking to be level-triggered, and the registration would succeed. This port models edge-triggered registrations only: the ready list is consumed as it is drained and a still-ready entry is never re-armed, so a wait after a partly drained level would sleep where a real epoll_wait returns again. Register with EPOLLET, or model level-triggering before answering."

[<RequireQualifiedAccess>]
module UnixPoll =

    /// Everything a wait for socket events settles before it consults the port:
    /// `epoll_wait(2)`'s four screens or `kevent(2)`'s two, in the order each
    /// kernel applies them. See `SocketWaitAdmission`.
    ///
    /// `epollWait` is the whole of `epoll_wait(2)`, these screens included; a
    /// client needs this only for `kevent(2)`, whose wait this library does not
    /// yet answer as a call of its own.
    ///
    /// A negative `maxEvents` is answered as 0 is, by both kernels.
    ///
    /// Each ordering is measured, on Linux 6.18.5 and Darwin 25.6.0, rather than
    /// read off the kernel sources: the widely-reproduced `do_epoll_wait` listing
    /// checks `maxevents` and `access_ok` *before* `fdget`, and current kernels
    /// do not.
    ///
    /// Changes nothing: everything a wait does before it reaches the port is a
    /// question.
    let admitSocketWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (maxEvents : int)
        (buffer : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SocketWaitAdmission, SocketWaitRefusal>
        =
        let openFile =
            FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            // Measured on 6.18.5, each adjacent pair separated by an input that
            // provokes exactly one of the two: descriptor, then `maxevents`,
            // then the buffer, then is-it-an-epoll-instance.
            match openFile with
            | None -> Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | Some (port, description) ->

            let architecture = SimulatedUnixPlatform.architecture system.Machine.UnixPlatform

            // The kernel's predicate is `maxevents <= 0 || maxevents > EP_MAX_EVENTS`.
            // Measured (`epoll-wait.c`, section G): a negative maxevents is
            // screened exactly as zero is.
            if maxEvents <= 0 || maxEvents > LinuxEpollLimits.maxEvents architecture then
                Ok (SocketWaitAdmission.Failed UnixError.EINVAL)
            else

            // The byte range `access_ok(events, maxevents * sizeof(struct
            // epoll_event))` screens. This multiplication is safe only *below*
            // the cap just applied, which is what `EP_MAX_EVENTS` exists for: it
            // is `INT_MAX / eventSize`, so every count that reaches here has a
            // product inside `int32`.
            let bufferExtent =
                uint64 maxEvents * uint64 (LinuxEpollLimits.eventSize architecture)

            // Not a mappedness check. On 64-bit Linux `access_ok` only rejects
            // ranges reaching into the kernel half, so a merely-unmapped
            // userspace address passes and the wait then blocks, faulting at
            // delivery -- which is why this must not eagerly demand that the
            // buffer be real before sleeping.
            match
                UserBufferCheck.faultsBeforeOperationFor
                    (UnixMachineState.userBufferCheck system.Machine)
                    buffer
                    bufferExtent
            with
            | Error refusal -> Error (SocketWaitRefusal.Buffer refusal)
            | Ok true -> Ok (SocketWaitAdmission.Failed UnixError.EFAULT)
            | Ok false ->

            match description.Target with
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Pipe _ ->
                // A live descriptor onto the wrong kind of object. EINVAL is
                // epoll's own answer for it, and it is the last of the four
                // screens -- behind the buffer, which is why an unmappable
                // buffer on a non-port descriptor is EFAULT rather than this.
                //
                // A socket is measured to be exactly like the other two here
                // rather than assumed to be: `epoll_wait` on a socket fd is
                // EINVAL, and EFAULT still wins ahead of it for an unmappable
                // buffer.
                Ok (SocketWaitAdmission.Failed UnixError.EINVAL)
            | OpenFileTarget.SocketEventPort _ -> Ok (SocketWaitAdmission.DeliverOrWait (port, maxEvents))
        | SimulatedUnixFlavour.Darwin ->
            // Measured on 25.6.0, and flatter: `kevent` resolves the descriptor
            // before its `nevents == 0` early return, has no "wrong kind of
            // object" answer to give, and screens no buffer at all -- so the
            // whole ladder is one question about the descriptor followed by one
            // about the count.
            match openFile with
            | None -> Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | Some (port, description) ->

            match description.Target with
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Pipe _ ->
                // EBADF, where epoll says EINVAL: kqueue folds "not a kqueue"
                // into "bad descriptor". Measured on a socket too, and for both
                // a zero and a non-zero event count.
                Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | OpenFileTarget.SocketEventPort portState ->

            // Measured on 27.0.0 (`kevent-negative-count.c`): a negative
            // `nevents` returns 0 at once, as zero does, whatever the port holds.
            if maxEvents <= 0 then
                Ok SocketWaitAdmission.NoEvents
            else

            // No buffer screen, so an unmappable buffer sleeps here rather than
            // faulting: `UserBufferCheck.AtCopyTime` is Darwin's answer, and a
            // wait that never delivers an event never copies anything.
            //
            // The port is empty by construction on this flavour -- the Darwin
            // registration arm refuses every change, so nothing can ever become
            // deliverable -- which is what makes it faithful to hand this to the
            // same delivery walk epoll uses and have it sleep. The assertion ties
            // those two facts together rather than leaving the second to be
            // rediscovered.
            if not (Map.isEmpty portState.Registrations) then
                failwith
                    $"UnixPoll.admitSocketWait: a Darwin-flavoured kernel holds %d{Map.count portState.Registrations} socket event registrations, but the Darwin registration arm refuses every change (this is a bug in the caller's state construction)."

            Ok (SocketWaitAdmission.DeliverOrWait (port, maxEvents))

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
    /// Under the Darwin flavour every call is refused: kqueue is not epoll.
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
        | Some (portId, portDescription) ->

        match FileDescriptorRegistry.tryFindWithId fd registry with
        | None -> failed EpollCtlError.BadTargetFd
        | Some (targetId, targetDescription) ->

        // `file_can_poll`: the target must have a `->poll` handler, which a
        // regular file and a directory lack.
        match targetDescription.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _ -> failed EpollCtlError.TargetNotPollable
        | OpenFileTarget.SocketEventPort _
        | OpenFileTarget.Socket _
        | OpenFileTarget.Pipe _ ->

        // One kernel test, `f.file == tf.file || !is_file_epoll(f.file)`, so
        // one answer: a `dup` of the port as target is this, not success.
        let portState =
            match portDescription.Target with
            | OpenFileTarget.SocketEventPort portState when portId <> targetId -> Some portState
            | OpenFileTarget.SocketEventPort _
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Pipe _ -> None

        match portState with
        | None -> failed EpollCtlError.NotAnEventPort
        | Some portState ->

        let targetIsPort =
            match targetDescription.Target with
            | OpenFileTarget.SocketEventPort _ -> true
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
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
        // table this library builds (it never holds a nested port) but which
        // precede EEXIST in the kernel.
        elif op = add && targetIsPort then
            Error (EpollCtlRefusal.NestedPort fd)
        else

        let key = fd, targetId
        let registered = Map.containsKey key portState.Registrations

        // The modes this port does not model, refused only where the call
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
            let alreadyPending = List.contains key portState.Ready

            if
                not alreadyPending
                && LinuxReadiness.ofDescription targetId system &&& stored <> 0u
            then
                withRegistry
                    (FileDescriptorRegistry.appendSocketEventReady portId key system.Process.FileDescriptors)
                    system
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
            | OpenFileTarget.SocketEventPort _ -> false

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

            let ordinal = system.Machine.NextSocketEventRegistrationOrdinal

            let registration =
                {
                    Events = stored
                    Data = data
                    RegisteredAt = ordinal
                }

            let system =
                { withRegistry (FileDescriptorRegistry.addEpollRegistration portId key registration registry) system with
                    Machine =
                        { system.Machine with
                            NextSocketEventRegistrationOrdinal = ordinal + 1L
                        }
                }

            Ok (EpollCtlAnswer.Changed, pendIfReady system)
        elif op = del then
            if registered then
                Ok (
                    EpollCtlAnswer.Changed,
                    withRegistry (FileDescriptorRegistry.removeEpollRegistration portId key registry) system
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
                withRegistry (FileDescriptorRegistry.modifyEpollRegistration portId key stored data registry) system

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
        | OpenFileTarget.SocketEventPort _ -> Error (PollRefusal.UnmodelledTarget entry.Fd)
        | OpenFileTarget.Socket _
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
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

    /// `poll(2)`: what each entry reports, and how many entries carry anything;
    /// or, when nothing does and the timeout lets it, the calling task sleeps.
    ///
    /// Each entry's `Events`, and each `revents` answered for it, is the raw
    /// bits in the simulated flavour's own `<poll.h>` numbering. Under the Linux
    /// flavour every bit is answered as a real kernel answers it: each named
    /// bit is reported when the descriptor presents it and the entry asked for
    /// it, `POLLERR` and `POLLHUP` whether asked for or not, and `POLLNVAL`
    /// alone for a descriptor that is not open. A bit Linux does not read
    /// (`POLLREMOVE`, 0x0800, 0x4000 and 0x8000) is ignored, as it is there.
    /// Under the Darwin flavour every poll is refused.
    ///
    /// The count is `poll(2)`'s own return value, and it is neither the number
    /// of entries nor the number of *conditions*: it counts entries carrying
    /// something.
    ///
    /// `milliseconds` is read as Linux's `poll(2)` reads it. Zero answers now.
    /// A positive timeout, when nothing is ready, parks `task` until a watched
    /// descriptor becomes ready or `milliseconds` have passed on the machine's
    /// monotonic clock, whichever is first; at the deadline and not before, the
    /// call finishes with 0. A negative timeout of any size is infinite. A wait
    /// with nothing to watch and no deadline parks until a signal ends it. A
    /// foreign-function layer that screens some negative values itself does
    /// that before calling.
    ///
    /// A poll with anything ready is answered at every timeout: an entry
    /// carrying anything at all -- a requested `IN`/`OUT`, an unrequested
    /// `HUP`, or `NVAL` -- makes a real poll return at once. The system comes
    /// back unchanged unless the task parked.
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

        // Ahead of the entries, and so ahead of an empty entry list too: a
        // zero-entry poll answers `rv = 0` identically on both flavours and
        // consults no readiness at all, but answering that one row would be a
        // branch reachable only from a flavour whose every other row refuses.
        //
        // Darwin's alphabet (`<poll.h>` on 25.6.0 arm64, 2026-09-23): the six
        // shared bits, POLLRDNORM 0x40, POLLRDBAND 0x80, POLLWRNORM = POLLOUT,
        // POLLWRBAND 0x100, POLLEXTEND 0x200, POLLATTRIB 0x400, POLLNLINK 0x800,
        // POLLWRITE 0x1000. `poll-alphabet.c`'s full sweep there finds no
        // request that fails, and no object for which the answer is one level
        // masked by the request: `poll` registers EVFILT_READ for any of
        // IN/RDNORM/PRI/RDBAND/HUP, EVFILT_WRITE for any of OUT/WRNORM/WRBAND,
        // and EVFILT_VNODE for any of the four vnode bits, and a filter the
        // descriptor cannot take turns the whole entry into POLLNVAL (a vnode
        // bit on every socket, pipe and kqueue; a read or write bit on a
        // directory; a write bit on a kqueue; any of them on a descriptor that
        // is not open). ERR and NVAL, and 0x2000..0x8000, register nothing, so
        // a request of only those reports nothing at all.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (PollRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

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

    let private finishPollHolding<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.Poll parked) -> parked
            | Some other ->
                failwith
                    $"UnixPoll.finishPoll: task %O{task} is parked in %A{other}, not in a poll, so there is no poll to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixPoll.finishPoll: task %O{task} is not parked, so there is no poll to finish. Only a task `poll` answered `WouldBlock` finishes here (this is a bug in the client)."

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

    /// Finish the `poll` `task` parked in: scan its entries again, as a woken
    /// real poll does, and answer.
    ///
    /// Scans the entries the call was made with, not whatever the caller's
    /// array holds now: a real kernel copied them in when the call began. A
    /// descriptor is looked up afresh, as a real poll does, and `close` refuses
    /// to close one a parked poll watches, so each still names the description
    /// the call went to sleep on.
    ///
    /// Answers the count when any entry carries anything, whether or not the
    /// deadline has passed or a signal is pending too; `Failed EINTR` when a
    /// signal with a handler interrupts it, whether or not the deadline has
    /// passed; 0, with every `revents` 0, when only the deadline has; and
    /// otherwise re-parks the task on the same entries and deadline, since
    /// whatever woke it has gone again. An answer clears the park.
    ///
    /// `task` must be parked in a `poll`.
    let finishPoll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PollOutcome * UnixSystem<'Task, 'Handler>, PollRefusal>
        =
        let held =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some parked -> ParkedSyscall.descriptions parked
            | None -> []

        // `close` refuses to close a descriptor a parked poll watches, so each
        // description the call held is still named by its descriptor.
        finishPollHolding task system
        |> Result.map (fun (outcome, after) ->
            outcome, ObjectLifetime.releaseUnreferencedUnrefusable "UnixPoll.finishPoll" held after
        )


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
        // create a port; every other single bit, EPOLL_CLOEXEC beside any other
        // bit, -1 and INT_MIN are EINVAL.
        if flags &&& ~~~EpollCreateFlags.CloseOnExec <> 0 then
            Ok (Error UnixError.EINVAL)
        else

        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

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
        (port : OpenFileDescriptionId)
        (buffer : UserBuffer)
        (delivered : (uint64 * uint32) list)
        : Result<unit, EpollWaitRefusal>
        =
        if List.isEmpty delivered then
            Ok ()
        else
            match buffer with
            | UserBuffer.Mapped -> Ok ()
            | UserBuffer.Unmapped _ -> Error (EpollWaitRefusal.UnmeasuredCopyOutFault port)
            | UserBuffer.Opaque -> Error (EpollWaitRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (EpollWaitRefusal.Buffer BufferRefusal.AddresslessAtTransfer)

    /// Park `task` in a wait on the socket event port `port` for up to
    /// `maxEvents` events, until `deadline`.
    let private parkSocketWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (port : OpenFileDescriptionId)
        (maxEvents : int)
        (buffer : UserBuffer)
        (deadline : int64 option)
        (system : UnixSystem<'Task, 'Handler>)
        : EpollWaitOutcome * UnixSystem<'Task, 'Handler>
        =
        let parked =
            ParkedSyscall.SocketWait
                {
                    Port = port
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
    /// Delivery walks the port's pending registrations in order, reporting each
    /// one whose target is still ready and consuming each one walked, stale or
    /// not (see `SocketEventPort.drain`). A wait that finds something answers it
    /// whatever the timeout. One that finds nothing answers no events at once
    /// for a timeout of 0; parks `task` for a positive timeout until an event is
    /// deliverable or `milliseconds` have passed on the machine's monotonic
    /// clock; and parks it until an event is deliverable for any negative
    /// timeout, which is infinite. A parked wait is finished with
    /// `finishSocketWait`. The walk's consumption stands whether or not the call
    /// then sleeps.
    ///
    /// A call that delivers events copies them out to `buffer`, which must be
    /// `Mapped`: this library does not answer for a copy that faults or a buffer
    /// whose bytes it cannot hold (see `EpollWaitRefusal`). A call that delivers
    /// nothing never looks at the buffer past the screen.
    ///
    /// Of several tasks parked on one port, one event wakes one of them (see
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
                $"UnixPoll.epollWait: task %O{task} is parked in %A{parked}, and is issuing an epoll_wait. A task blocks in one syscall at a time; a parked wait is finished with `finishSocketWait` (this is a bug in the client)."
        | None ->

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (EpollWaitRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        match admitSocketWait epfd maxEvents buffer system with
        | Error (SocketWaitRefusal.Buffer refusal) -> Error (EpollWaitRefusal.Buffer refusal)
        | Ok (SocketWaitAdmission.Failed error) -> Ok (EpollWaitOutcome.Failed error, system)
        | Ok SocketWaitAdmission.NoEvents ->
            failwith
                "UnixPoll.epollWait: the socket wait admission answered NoEvents under the Linux flavour, which only kevent answers (this is a bug in this library)."
        | Ok (SocketWaitAdmission.DeliverOrWait (port, maxEvents)) ->

        let delivered, system = SocketEventPort.drain port maxEvents system

        // Measured (`epoll-wait.c`, sections B and C): a wait that finds
        // something answers at once whatever the timeout, and one that finds
        // nothing answers at once for a timeout of 0.
        if not (List.isEmpty delivered) || milliseconds = 0 then
            copyOut port buffer delivered
            |> Result.map (fun () -> EpollWaitOutcome.Answered delivered, system)
        else

        let now = system.Machine.NanosecondsSinceBoot

        match relativeDeadline now milliseconds with
        | Error () -> Error (EpollWaitRefusal.DeadlineBeyondClock (now, milliseconds))
        | Ok deadline -> Ok (parkSocketWait task port maxEvents buffer deadline system)

    let private finishSocketWaitHolding<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollWaitOutcome * UnixSystem<'Task, 'Handler>, EpollWaitRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.SocketWait parked) -> parked
            | Some other ->
                failwith
                    $"UnixPoll.finishSocketWait: task %O{task} is parked in %A{other}, not in a socket event wait, so there is no wait to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixPoll.finishSocketWait: task %O{task} is not parked, so there is no wait to finish. Only a task `epollWait` answered `WouldBlock` finishes here (this is a bug in the client)."

        let delivered, system = SocketEventPort.drain parked.Port parked.MaxEvents system

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
            |> Result.bind (fun () -> copyOut parked.Port parked.Buffer delivered)
            |> Result.map (fun () -> EpollWaitOutcome.Answered delivered, finished)
        else

        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (EpollWaitRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> Ok (EpollWaitOutcome.Failed UnixError.EINTR, finished)
        | Ok (Some SyscallInterruption.Restart) ->
            failwith
                "UnixPoll.finishSocketWait: a socket event wait restarted after a signal, where `SyscallInterruption.ruleOf` says one never restarts (this is a bug in this library)."
        | Ok None -> Ok (parkSocketWait task parked.Port parked.MaxEvents parked.Buffer parked.Deadline system)

    /// Finish the wait on a socket event port that `task` is parked in: walk
    /// the port again, as a woken real wait does, and answer.
    ///
    /// Delivers from the port the call was made on and with the `maxEvents` it
    /// was made with, not whatever the caller's arguments hold now: the parked
    /// call holds the port's open file description, which outlives its last
    /// descriptor until the call returns, and goes then.
    ///
    /// Answers the events it finds, whether or not the deadline has passed or a
    /// signal is pending too (measured, `epoll-wait.c` section E: an event and
    /// an expired deadline both holding as the waiter runs report the event);
    /// no events when the deadline has passed, whether or not a signal is
    /// pending; `Failed EINTR` when a signal with a handler interrupts it; and
    /// otherwise re-parks the task on the same port and deadline, since
    /// whatever woke it has gone again. A re-park goes to the back of park
    /// order, which puts it first in line for the port's next event. An answer
    /// clears the park.
    ///
    /// Delivering events copies them out to the buffer the call was made with;
    /// see `epollWait` for the buffers that refuses.
    ///
    /// `task` must be parked in a socket event wait.
    let finishSocketWait<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollWaitOutcome * UnixSystem<'Task, 'Handler>, EpollWaitRefusal>
        =
        let held =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some parked -> ParkedSyscall.descriptions parked
            | None -> []

        // The call's reference to the port goes as it returns, and with it the
        // port, if no descriptor names it any more (`open-file-references.c`
        // section E).
        finishSocketWaitHolding task system
        |> Result.map (fun (outcome, after) ->
            outcome, ObjectLifetime.releaseUnreferencedUnrefusable "UnixPoll.finishSocketWait" held after
        )
