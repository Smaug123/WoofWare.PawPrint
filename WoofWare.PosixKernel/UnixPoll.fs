namespace WoofWare.PosixKernel

/// What a socket event port -- an `epoll` instance, or a `kqueue` -- would
/// report if a wait on it were re-polled now, and what draining one does.
///
/// The *consumer* half of the port model. The producer half -- seeding the
/// pending list when a registration is added or modified, and signalling a
/// registration when its target's level changes -- is still the client's, so
/// this library can say whether a port would deliver while owning no modelled
/// operation that makes one start to.
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
                $"SocketEventPort.epollReadinessOfDescription: %O{targetId} names no live open file description. FileDescriptorRegistry.close sweeps destroyed descriptions out of every interest table, so this is an interpreter bug."
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
        /// What the caller asked about. `Err`, `Hup` and `Nval` are reported
        /// whether or not they appear here, so a caller may leave them out and
        /// still be told about them.
        Requested : PollEvents
    }

/// Why this kernel will not answer a `poll`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type PollRefusal =
    /// This kernel models `poll(2)`'s readiness for one flavour only, and it is
    /// not this one.
    ///
    /// Darwin's answers differ on almost every measured row -- an idle TCP
    /// socket presents nothing where Linux presents `OUT|HUP`, a directory and a
    /// character device answer `NVAL` where Linux answers `IN|OUT`, and `ERR`
    /// and `HUP` are not output-only there -- so it is a second readiness model
    /// rather than an extra column.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The entry names a socket event port, and what `poll(2)` reports for one
    /// is unmeasured.
    ///
    /// Reachable in a way epoll's equivalent is not: `epoll_ctl` screens the
    /// targets it will accept, and `poll(2)` accepts any descriptor.
    | UnmeasuredTarget of fd : int
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
            $"this kernel is %O{flavour}-flavoured, and `poll(2)`'s readiness is modelled here for Linux only. The Darwin rows are measured but unimplemented, and they are a second readiness model rather than an extra column: ERR and HUP are not output-only there, an idle stream socket presents nothing, and file targets split by kind. Model Darwin readiness before polling under this flavour."
        | PollRefusal.UnmeasuredTarget fd ->
            $"fd %d{fd} names a socket event port, and what `poll(2)` reports for one is unmeasured. Measure what such a descriptor reports with and without ready events before answering."
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

[<RequireQualifiedAccess>]
module SocketEventRegistrationRefusal =
    /// What this kernel knows about why it will not register anything. The
    /// client supplies its own half -- which of its entry points was asked, and
    /// on whose behalf.
    let describe (refusal : SocketEventRegistrationRefusal) : string =
        match refusal with
        | SocketEventRegistrationRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and registration is modelled here for Linux only. kqueue's semantics -- per-filter state, a silently-replacing ADD, file targets succeeding -- are unmeasured beyond the fact that they diverge from epoll's, and the return codes alone are not a model of the state a call leaves behind. Measure them before answering."

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
        | SocketEventRegistrationChange.Add (interest, _)
        | SocketEventRegistrationChange.Modify (interest, _) ->

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

    /// The readiness of the descriptor `targetId` names, for a `poll(2)` caller.
    ///
    /// A sibling of `SocketEventPort.epollReadinessOfDescription` rather than a
    /// widening of it: the two dispatchers refuse different things, because
    /// `epoll_ctl` screens targets that `poll(2)` accepts. The per-socket level
    /// they share (`socketReadinessLevel`) is the part measurement says is one
    /// function.
    ///
    /// Linux rows only; `poll` refuses the Darwin flavour before calling this,
    /// which is what lets the file row below be a single answer -- on Darwin a
    /// regular file polls `IN|PRI|OUT` but a directory polls `NVAL`, so the same
    /// `OpenFileTarget.File` would need two.
    let pollReadinessOfDescription<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (targetId : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : ReadinessLevel
        =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None ->
            failwith
                $"UnixPoll.pollReadinessOfDescription: %O{targetId} names no live open file description. `poll` answers POLLNVAL for an fd that names nothing, without ever reaching here, so this is a bug in the caller."
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId -> UnixMachineState.socketReadinessLevel socketId system.Machine
        | OpenFileTarget.File _ ->
            // Measured (`pollgaps.c`): a regular file answers IN|OUT at every
            // offset and under O_RDONLY as much as O_RDWR, and a directory
            // answers the same. Files have no `->poll` handler, so the VFS
            // default reports them always-ready; nothing about this varies
            // with the file's contents or the description's position.
            { ReadinessLevel.none with
                In = true
                Out = true
            }
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            // The same launch-shape constants `SocketEventPort.epollReadinessOfDescription`
            // holds, and poll agrees with both on Linux (`pollmask.c` rows 19
            // and 20). Not shared with that function: it refuses two of the
            // targets this one answers, so the common part is the socket
            // level, not the dispatch.
            { ReadinessLevel.none with
                Hup = true
            }
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardOutput
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardError ->
            { ReadinessLevel.none with
                Out = true
            }
        | OpenFileTarget.SocketEventPort _ ->
            failwith
                $"UnixPoll.pollReadinessOfDescription: %O{targetId} is a socket event port, and what `poll(2)` reports for one is unmeasured. `poll` refuses such an entry before reaching here, so this is a bug in the caller."

    /// `poll(2)`: what each entry reports right now, and how many entries carry
    /// anything.
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
        : Result<PollEvents list * int, PollRefusal>
        =
        // Ahead of the entries, and so ahead of an empty entry list too: a
        // zero-entry poll answers `rv = 0` identically on both flavours and
        // consults no readiness at all, but answering that one row would be a
        // branch reachable only from a flavour whose every other row refuses.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (PollRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let reportOne (entry : PollEntry) : Result<PollEvents, PollRefusal> =
            if entry.Fd < 0 then
                // Measured on both kernels: a negative descriptor is ignored,
                // reports nothing, and does not count towards the return value.
                // It is not an error and not NVAL.
                Ok PollEvents.none
            else

            match FileDescriptorRegistry.tryFindWithId entry.Fd system.Process.FileDescriptors with
            | None ->
                // POLLNVAL is a statement about the entry, not a readiness
                // level, and it is reported whether or not anything was asked
                // for.
                Ok
                    { PollEvents.none with
                        Nval = true
                    }
            | Some (descriptionId, description) ->

            match description.Target with
            | OpenFileTarget.SocketEventPort _ -> Error (PollRefusal.UnmeasuredTarget entry.Fd)
            | OpenFileTarget.Socket _
            | OpenFileTarget.File _
            | OpenFileTarget.StandardStream _ ->
                pollReadinessOfDescription descriptionId system
                |> PollEvents.ofLevel entry.Requested
                |> Ok

        let reported =
            List.foldBack
                (fun entry acc ->
                    match acc, reportOne entry with
                    | Error refusal, _ -> Error refusal
                    | _, Error refusal -> Error refusal
                    | Ok rest, Ok events -> Ok (events :: rest)
                )
                entries
                (Ok [])

        match reported with
        | Error refusal -> Error refusal
        | Ok reported ->

        let triggered = reported |> List.filter (PollEvents.isEmpty >> not) |> List.length

        if triggered = 0 && milliseconds <> 0 then
            Error (PollRefusal.WouldPark milliseconds)
        else
            Ok (reported, triggered)
