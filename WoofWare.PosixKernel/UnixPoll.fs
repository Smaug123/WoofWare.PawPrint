namespace WoofWare.PosixKernel

/// What a socket event port -- an `epoll` instance, or a `kqueue` -- would
/// report if a wait on it were re-polled now, and what draining one does.
///
/// The *consumer* half of the port model. The producer half -- seeding the
/// pending list when a registration is added or modified, and signalling a
/// registration when its target's level changes -- belongs to the operations
/// that make those changes: `UnixPoll.changeSocketEventRegistration`, and the
/// socket operations in `UnixConnection`.
[<RequireQualifiedAccess>]
module SocketEventPort =

    /// The epoll readiness of the descriptor `targetId` names, for computing
    /// what a registration on it would report.
    ///
    /// A standard stream's level is a constant of the launch shape PawPrint
    /// models (measured, `pipes.c`): stdin is the read end of a pipe whose
    /// write end the launcher closed — the same claim `SystemNative_Read`'s
    /// immediate-EOF makes — which presents `EPOLLHUP`, and the output
    /// streams are write ends with space and a live reader, which present
    /// `EPOLLOUT`. No modelled operation changes either, so the streams need
    /// no producer. A file or port target cannot reach here: the registry
    /// answers EPERM for the one and refuses the other.
    let epollReadinessOfDescription<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (targetId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : ReadinessLevel
        =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"SocketEventPort.epollReadinessOfDescription: %O{targetId} names no live open file description. FileDescriptorRegistry.dropDescriptor sweeps destroyed descriptions out of every interest table, so this is an interpreter bug."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId -> UnixMachineState.socketReadinessLevel socketId system.Machine
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            { ReadinessLevel.none with
                Hup = true
            }
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardOutput
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardError ->
            { ReadinessLevel.none with
                Out = true
            }
        | OpenFileTarget.File _ ->
            failwith
                $"SocketEventPort.epollReadinessOfDescription: %O{targetId} is a regular file, which epoll_ctl answers EPERM for, so no registration can name it (this is an interpreter bug)."
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"SocketEventPort.epollReadinessOfDescription: %O{targetId} is itself a socket event port; the registry refuses a nested-port registration, so no registration can name it (this is an interpreter bug)."

    /// Each pending entry of the port, in delivery order, with what it would
    /// report if `epoll_wait` re-polled it right now: the target's current
    /// level restricted to the registration's interest.
    let private annotatedReady<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portState : SocketEventPortState)
        (system : UnixSystem<'Task, 'Handler>)
        : ((int * OpenFileDescriptionId) * SocketEventRegistration * ReadinessLevel) list
        =
        portState.Ready
        |> List.map (fun (_, targetId as key) ->
            let registration =
                match Map.tryFind key portState.Registrations with
                | Some registration -> registration
                | None ->
                    failwith
                        $"SocketEventPort.annotatedReady: pending entry %A{key} has no registration. FileDescriptorRegistryDefect.SocketEventReadyEntryUnregistered exists to make this unreachable, so this is an interpreter bug."

            let reported =
                epollReadinessOfDescription targetId system
                |> ReadinessLevel.reportedUnder registration.Interest

            key, registration, reported
        )

    /// Whether an `epoll_wait` on the port `portId` names would return at
    /// least one event right now — the wake condition a parked waiter is
    /// polled against, and by construction the same question `drain` answers,
    /// because both read the same annotated walk.
    ///
    /// Loudly partial in `portId`, exactly as a parked `flock`'s wake condition
    /// is: this library's descriptor table models no reference from a waiter to
    /// what it waits on, so a client that parks a task on a port must stop that
    /// port being destroyed while it waits — which is what `close`'s port
    /// refusal does. Asking about a port that has gone is that obligation being
    /// broken, and neither answer is honest: `true` wakes the waiter into an
    /// `EBADF` no kernel produces, and `false` sleeps for ever.
    let hasDeliverableEvent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match Map.tryFind portId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"SocketEventPort.hasDeliverableEvent: %O{portId} names no live open file description, so a task parked on a wait for it has had that description closed underneath it. This library's table models no reference from a waiter to what it waits on, so a client that parks must refuse such a close (as `close` does)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Socket _ ->
            failwith
                $"SocketEventPort.hasDeliverableEvent: %O{portId} is not a socket event port, so no wait can be parked on it (this is an interpreter bug)."
        | OpenFileTarget.SocketEventPort portState ->
            annotatedReady portState system
            |> List.exists (fun (_, _, reported) -> not (ReadinessLevel.isEmpty reported))

    /// Drain the port as one `epoll_wait(maxevents = maxCount)` would: walk
    /// the pending entries in order, re-polling each; report the ones whose
    /// re-poll is nonempty, silently drop the stale ones, and stop once
    /// `maxCount` events are reported — every walked entry is consumed, and
    /// the entries the stop spared stay pending in order (measured,
    /// `order2.c` row J).
    ///
    /// Returns the reported rows — each the registration's `Data` and the
    /// reported readiness, in epoll's terms; the conversion to a client's own
    /// event encoding (the PAL's `EPOLLHUP` folding into `EPOLLIN|EPOLLOUT`)
    /// is the caller's — and the system with the walked entries consumed.
    ///
    /// Loudly partial in `portId`: callers hold a live port description in
    /// hand.
    let drain<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portId : OpenFileDescriptionId)
        (maxCount : int)
        (system : UnixSystem<'Task, 'Handler>)
        : (uint64 * ReadinessLevel) list * UnixSystem<'Task, 'Handler>
        =
        if maxCount <= 0 then
            failwith
                $"SocketEventPort.drain: maxCount %d{maxCount} is not positive; epoll answers EINVAL for it before reaching the ready list, so this is an interpreter bug."

        match Map.tryFind portId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"SocketEventPort.drain: %O{portId} names no live open file description (this is an interpreter bug)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.File _
        | OpenFileTarget.Socket _ ->
            failwith $"SocketEventPort.drain: %O{portId} is not a socket event port (this is an interpreter bug)."
        | OpenFileTarget.SocketEventPort portState ->

        let rec walk
            (delivered : (uint64 * ReadinessLevel) list)
            (remaining : ((int * OpenFileDescriptionId) * SocketEventRegistration * ReadinessLevel) list)
            : (uint64 * ReadinessLevel) list * (int * OpenFileDescriptionId) list
            =
            match remaining with
            | [] -> List.rev delivered, []
            | (_, registration, reported) :: rest ->
                if List.length delivered = maxCount then
                    List.rev delivered, remaining |> List.map (fun (key, _, _) -> key)
                elif ReadinessLevel.isEmpty reported then
                    walk delivered rest
                else
                    walk ((registration.Data, reported) :: delivered) rest

        let delivered, surviving = walk [] (annotatedReady portState system)

        delivered,
        { system with
            Process =
                { system.Process with
                    FileDescriptors =
                        FileDescriptorRegistry.setSocketEventReady portId surviving system.Process.FileDescriptors
                }
        }

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
    /// No entry carries anything and the timeout is not zero, so a real `poll`
    /// sleeps here until a descriptor becomes ready or the timeout expires.
    ///
    /// Not `SyscallOutcome.WouldBlock`, for the reason `accept`'s `WouldPark` is
    /// not: blocking is an outcome only where there is a `WakeCondition` to hand
    /// back, and this library has none carrying a poll's captured entry set and
    /// its deadline.
    ///
    /// Every *other* case is answerable whatever the timeout, which is measured
    /// rather than assumed: an entry carrying anything at all -- a requested
    /// `IN`/`OUT`, an unrequested `HUP`, or `NVAL` -- makes a real poll return
    /// immediately at any timeout.
    | WouldPark of timeoutMilliseconds : int

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
        | PollRefusal.WouldPark timeoutMilliseconds ->
            $"no entry carries anything and the timeout is %d{timeoutMilliseconds}ms, so a real `poll(2)` would sleep. This library models no parked poll: `WakeCondition` has no case carrying a poll's entry set and its deadline, so a park here would never end. A poll with anything already ready is answered at any timeout; only this case needs the park."

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
    /// immediately where `epoll_wait` with `maxevents == 0` is EINVAL.
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

/// What an epoll-style registration change answered.
[<RequireQualifiedAccess>]
type SocketEventRegistrationAnswer =
    /// Applied. The system this rides with carries the new interest table, and
    /// the ready list if the change made the target pending.
    | Changed
    /// `epoll_ctl(2)` refused it. `SocketEventRegistrationError.toErrno` is the
    /// number; the case itself says which of the two `EBADF`s this is, and a
    /// client that does not care can drop it.
    ///
    /// Nothing changed: the system comes back as it was.
    | Failed of reason : SocketEventRegistrationError

/// Why this kernel will not answer an epoll-style registration change.
///
/// Distinct from a `Failed` answer: that is `epoll_ctl(2)` refusing, and this is
/// this library having nothing to say.
[<RequireQualifiedAccess>]
type SocketEventRegistrationRefusal =
    /// This kernel models registration for one flavour only, and it is not this
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
    /// The registration asked to be level-triggered, and this port models
    /// edge-triggered registrations only: `drain` consumes every entry it
    /// walks and never re-arms a still-ready one, so a wait after a partly
    /// drained level would sleep where a real `epoll_wait` returns again.
    | LevelTriggered

[<RequireQualifiedAccess>]
module SocketEventRegistrationRefusal =
    /// What this kernel knows about why it will not register anything. The
    /// client supplies its own half -- which of its entry points was asked, and
    /// on whose behalf.
    let describe (refusal : SocketEventRegistrationRefusal) : string =
        match refusal with
        | SocketEventRegistrationRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and registration is modelled here for Linux only. kqueue's semantics -- per-filter state, a silently-replacing ADD, file targets succeeding -- are unmeasured beyond the fact that they diverge from epoll's, and the return codes alone are not a model of the state a call leaves behind. Measure them before answering."
        | SocketEventRegistrationRefusal.LevelTriggered ->
            "the registration asked to be level-triggered, and this port models edge-triggered registrations only: the ready list is consumed as it is drained and a still-ready entry is never re-armed, so a wait after a partly drained level would sleep where a real epoll_wait returns again. Register with EPOLLET, or model level-triggering before answering."

[<RequireQualifiedAccess>]
module UnixPoll =

    /// Everything a wait for socket events settles before it consults the port:
    /// `epoll_wait(2)`'s four screens or `kevent(2)`'s two, in the order each
    /// kernel applies them. See `SocketWaitAdmission`.
    ///
    /// `maxEvents` must not be negative. Neither kernel is ever asked one -- a
    /// foreign-function layer that reads it out of a caller's cell screens it
    /// there -- so a caller that has not is asking a question this library has no
    /// answer for.
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
        if maxEvents < 0 then
            failwith
                $"UnixPoll.admitSocketWait: maxEvents %d{maxEvents} is negative, which neither kernel is ever asked -- the layer that reads it out of the caller's cell answers for a negative itself. Screen this in the client (this is a bug in the caller)."

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

            // The kernel's predicate is `maxevents <= 0 || maxevents > EP_MAX_EVENTS`.
            // Zero is the only non-positive value that reaches here, negatives
            // having been screened by the caller.
            if maxEvents = 0 || maxEvents > LinuxEpollLimits.MaxEvents then
                Ok (SocketWaitAdmission.Failed UnixError.EINVAL)
            else

            // The byte range `access_ok(events, maxevents * sizeof(struct
            // epoll_event))` screens. This multiplication is safe only *below*
            // the cap just applied, which is what `EP_MAX_EVENTS` exists for: it
            // is `INT_MAX / EventSize`, so every count that reaches here has a
            // product inside `int32`.
            let bufferExtent = uint64 maxEvents * uint64 LinuxEpollLimits.EventSize

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
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ ->
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
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ ->
                // EBADF, where epoll says EINVAL: kqueue folds "not a kqueue"
                // into "bad descriptor". Measured on a socket too, and for both
                // a zero and a non-zero event count.
                Ok (SocketWaitAdmission.Failed UnixError.EBADF)
            | OpenFileTarget.SocketEventPort portState ->

            if maxEvents = 0 then
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

    /// `epoll_ctl(2)` past a caller's own screens: apply `change` to the port's
    /// interest table, and bring the ready list with it.
    ///
    /// An ADD or MOD whose target is ready under the *new* interest makes the
    /// registration pending at that moment (measured rows E, I and K: the entry
    /// enters at ADD/MOD time), and a MOD of an entry already pending leaves its
    /// place alone (row L).
    ///
    /// `change` is derived from what a caller *claimed* about the current and
    /// new interest rather than from this table -- that derivation belongs to
    /// whoever holds the caller's arguments, and a wrong claim is answered here
    /// with `AlreadyRegistered` or `NotRegistered` exactly as a real
    /// `epoll_ctl` answers it.
    let changeSocketEventRegistration<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portFd : int)
        (targetFd : int)
        (change : SocketEventRegistrationChange)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SocketEventRegistrationAnswer * UnixSystem<'Task, 'Handler>, SocketEventRegistrationRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin ->
            Error (SocketEventRegistrationRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let levelTriggered =
            match change with
            | SocketEventRegistrationChange.Add (SocketEventTrigger.LevelTriggered, _, _)
            | SocketEventRegistrationChange.Modify (SocketEventTrigger.LevelTriggered, _, _) -> true
            | SocketEventRegistrationChange.Add (SocketEventTrigger.EdgeTriggered, _, _)
            | SocketEventRegistrationChange.Modify (SocketEventTrigger.EdgeTriggered, _, _)
            | SocketEventRegistrationChange.Remove -> false

        if levelTriggered then
            Error SocketEventRegistrationRefusal.LevelTriggered
        else

        let ordinal = system.Machine.NextSocketEventRegistrationOrdinal

        match
            FileDescriptorRegistry.changeSocketEventRegistration
                portFd
                targetFd
                ordinal
                change
                system.Process.FileDescriptors
        with
        | Error error -> Ok (SocketEventRegistrationAnswer.Failed error, system)
        | Ok registry ->

        let system =
            { system with
                Machine =
                    { system.Machine with
                        NextSocketEventRegistrationOrdinal =
                            match change with
                            | SocketEventRegistrationChange.Add _ -> ordinal + 1L
                            | SocketEventRegistrationChange.Modify _
                            | SocketEventRegistrationChange.Remove -> ordinal
                    }
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        match change with
        | SocketEventRegistrationChange.Remove -> Ok (SocketEventRegistrationAnswer.Changed, system)
        | SocketEventRegistrationChange.Add (_, interest, _)
        | SocketEventRegistrationChange.Modify (_, interest, _) ->

        // Both fds resolved a moment ago inside the registry change, so these
        // lookups cannot miss.
        let portId =
            match FileDescriptorRegistry.tryFindId portFd system.Process.FileDescriptors with
            | Some id -> id
            | None ->
                failwith
                    $"UnixPoll.changeSocketEventRegistration: port fd %d{portFd} was live moments ago (this is a bug in this library)."

        let key, targetId =
            match FileDescriptorRegistry.tryFindId targetFd system.Process.FileDescriptors with
            | Some id -> (targetFd, id), id
            | None ->
                failwith
                    $"UnixPoll.changeSocketEventRegistration: target fd %d{targetFd} was live moments ago (this is a bug in this library)."

        let alreadyPending =
            match Map.tryFind portId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
            | Some description ->
                match description.Target with
                | OpenFileTarget.SocketEventPort portState -> List.contains key portState.Ready
                | _ ->
                    failwith
                        $"UnixPoll.changeSocketEventRegistration: %O{portId} committed a registration change moments ago yet is not a socket event port (this is a bug in this library)."
            | None ->
                failwith
                    $"UnixPoll.changeSocketEventRegistration: %O{portId} was live moments ago (this is a bug in this library)."

        let readyNow =
            SocketEventPort.epollReadinessOfDescription targetId system
            |> ReadinessLevel.reportedUnder interest
            |> ReadinessLevel.isEmpty
            |> not

        if readyNow && not alreadyPending then
            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors =
                                FileDescriptorRegistry.appendSocketEventReady portId key system.Process.FileDescriptors
                        }
                }

            Ok (SocketEventRegistrationAnswer.Changed, system)
        else
            Ok (SocketEventRegistrationAnswer.Changed, system)

    // Linux's `<poll.h>` numbering: the bits a Linux-flavoured `poll(2)` reads
    // in `events` and writes in `revents`.
    //
    // Measured 2026-09-23 on Linux 6.18.5 aarch64 (glibc 2.41) by
    // `docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c`, which
    // printed the header and then polled every object and state
    // `linuxPollLevel` answers for with all 65536 request masks at timeout 0. On every object,
    // every answer was `level & (events | POLLERR | POLLHUP)` and `rv` counted
    // exactly the entries with a non-zero `revents`; a descriptor that is not
    // open answered `POLLNVAL` alone to all 65536; no request failed.
    //
    // No modelled object presents `POLLPRI`, `POLLRDBAND` or `POLLMSG`, which
    // is why they have no literal here. `POLLREMOVE` (0x1000), the unassigned
    // 0x0800 and the kernel-internal 0x4000 and 0x8000 were never reported
    // either, which is the shape of `do_pollfd`: it reads a request through
    // `demangle_poll`, which maps only the named bits other than `POLLREMOVE`,
    // so the rest never reach the filter the level is masked by. That held
    // for 0x8000 even on a socket with `SO_BUSY_POLL` set, the one case in
    // which a socket's own poll handler adds that bit to its mask.
    let private linuxPollIn : int16 = 0x0001s
    let private linuxPollOut : int16 = 0x0004s
    let private linuxPollErr : int16 = 0x0008s
    let private linuxPollHup : int16 = 0x0010s
    let private linuxPollNval : int16 = 0x0020s
    let private linuxPollRdNorm : int16 = 0x0040s
    let private linuxPollWrNorm : int16 = 0x0100s
    let private linuxPollWrBand : int16 = 0x0200s
    let private linuxPollRdHup : int16 = 0x2000s

    /// The conditions the descriptor `targetId` names presents to a
    /// Linux-flavoured `poll(2)`, in Linux's `<poll.h>` numbering: what it
    /// would report to a request of every bit.
    let private linuxPollLevel<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (targetId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : int16
        =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"UnixPoll.linuxPollLevel: %O{targetId} names no live open file description. `poll` answers POLLNVAL for an fd that names nothing, without ever reaching here (this is a bug in this library)."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId ->
            // The per-socket level is the one epoll reads (`socketReadinessLevel`):
            // both waiters take their mask from the socket's own `->poll`
            // handler. That handler also sets bits epoll's interest cannot ask
            // for, and measurement pins each to a condition the level already
            // holds: every measured socket presents POLLRDNORM exactly when it
            // presents POLLIN, and POLLWRNORM exactly when POLLOUT.
            let level = UnixMachineState.socketReadinessLevel socketId system.Machine
            let socket = UnixMachineState.socket socketId system.Machine

            // POLLWRBAND is the one bit that depends on more than the level.
            // Measured, it rides with POLLOUT on UDP (IPv4 and IPv6, with and
            // without a peer) and on every Unix-domain socket (stream,
            // datagram, raw and seqpacket, fresh), and never on TCP (idle,
            // listening, established with the peer alive or gone, refused) --
            // `tcp_poll` sets POLLOUT|POLLWRNORM, where `datagram_poll` and the
            // Unix-domain handlers set POLLOUT|POLLWRNORM|POLLWRBAND.
            let writeBand =
                match socket.Domain, socket.Kind with
                | SocketDomain.Unix, _ -> true
                | SocketDomain.InterNetwork, SocketKind.Datagram
                | SocketDomain.InterNetworkV6, SocketKind.Datagram -> true
                | SocketDomain.InterNetwork, SocketKind.Stream
                | SocketDomain.InterNetworkV6, SocketKind.Stream -> false
                | SocketDomain.InterNetwork, (SocketKind.Raw | SocketKind.SeqPacket)
                | SocketDomain.InterNetworkV6, (SocketKind.Raw | SocketKind.SeqPacket) ->
                    failwith
                        $"UnixPoll.linuxPollLevel: socket %O{socketId} is %O{socket.Kind} in %O{socket.Domain}, which this kernel never creates (an IP raw socket needs CAP_NET_RAW, and nothing here creates SCTP), so what `poll(2)` reports for it is unmeasured (this is a bug in the caller's state construction)."

            (if level.In then linuxPollIn ||| linuxPollRdNorm else 0s)
            ||| (if level.Out then
                     linuxPollOut ||| linuxPollWrNorm ||| (if writeBand then linuxPollWrBand else 0s)
                 else
                     0s)
            ||| (if level.RdHup then linuxPollRdHup else 0s)
            ||| (if level.Hup then linuxPollHup else 0s)
            ||| (if level.Err then linuxPollErr else 0s)
        | OpenFileTarget.File _ ->
            // Measured: a regular file answers IN|OUT|RDNORM|WRNORM at every
            // offset, empty or not, and under every access mode, and a directory
            // answers the same. Files have no `->poll` handler, so `vfs_poll`
            // reports `DEFAULT_POLLMASK` for them; nothing about this varies with
            // the file's contents or the description's position.
            linuxPollIn ||| linuxPollOut ||| linuxPollRdNorm ||| linuxPollWrNorm
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            // The launch shape `SocketEventPort.epollReadinessOfDescription`
            // states too: stdin is the read end of a pipe whose writer the
            // launcher closed, measured to present HUP alone.
            linuxPollHup
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardOutput
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardError ->
            // The write end of a pipe with space and a live reader.
            linuxPollOut ||| linuxPollWrNorm
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"UnixPoll.linuxPollLevel: %O{targetId} is a socket event port, and what `poll(2)` reports for one is unmeasured. `poll` refuses such an entry before reaching here (this is a bug in this library)."

    /// `poll(2)`: what each entry reports right now, and how many entries carry
    /// anything.
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
    /// something. Derivable from the list, and answered here so that no client
    /// re-derives a kernel rule.
    ///
    /// `milliseconds` is read as `poll(2)` reads it -- zero means "answer now",
    /// and every other value means "sleep until something happens", negative
    /// included. A foreign-function layer that screens some negative values
    /// itself does that before calling.
    ///
    /// Changes nothing and returns no system: a `poll` asks.
    let poll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int16 list * int, PollRefusal>
        =
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

        let reportOne (entry : PollEntry) : Result<int16, PollRefusal> =
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
            | OpenFileTarget.StandardStream _ ->
                // `do_pollfd`'s own shape: the level, filtered by the request
                // with POLLERR and POLLHUP added whatever was asked.
                linuxPollLevel descriptionId system
                &&& (entry.Events ||| linuxPollErr ||| linuxPollHup)
                |> Ok

        // In list order, stopping at the first entry that cannot be answered:
        // a real `poll` inspects its entries in order, so that is the entry a
        // refusal names.
        let rec report (remaining : PollEntry list) (acc : int16 list) : Result<int16 list, PollRefusal> =
            match remaining with
            | [] -> Ok (List.rev acc)
            | entry :: rest ->
                match reportOne entry with
                | Error refusal -> Error refusal
                | Ok events -> report rest (events :: acc)

        let reported = report entries []

        match reported with
        | Error refusal -> Error refusal
        | Ok reported ->

        let triggered =
            reported |> List.filter (fun revents -> revents <> 0s) |> List.length

        if triggered = 0 && milliseconds <> 0 then
            Error (PollRefusal.WouldPark milliseconds)
        else
            Ok (reported, triggered)
