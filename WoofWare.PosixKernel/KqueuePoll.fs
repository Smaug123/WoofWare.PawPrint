namespace WoofWare.PosixKernel

/// Darwin's `<poll.h>` numbering: the bits a Darwin-flavoured `poll(2)` reads in
/// `events` and writes in `revents`.
///
/// Measured on Darwin 25.6.0 and 27.0.0 arm64 (`poll-alphabet.c`,
/// `poll-darwin.c` in docs/plans/2026-08-23-posix-kernel-extraction). The six
/// below `POLLRDNORM` are numbered as Linux numbers them; the rest are Darwin's
/// own. `POLLWRNORM` is `POLLOUT`.
[<RequireQualifiedAccess>]
module DarwinPollEvents =
    /// `POLLIN`.
    [<Literal>]
    let In : int16 = 0x0001s

    /// `POLLPRI`.
    [<Literal>]
    let Pri : int16 = 0x0002s

    /// `POLLOUT`, which is also `POLLWRNORM`.
    [<Literal>]
    let Out : int16 = 0x0004s

    /// `POLLERR`.
    [<Literal>]
    let Err : int16 = 0x0008s

    /// `POLLHUP`.
    [<Literal>]
    let Hup : int16 = 0x0010s

    /// `POLLNVAL`.
    [<Literal>]
    let Nval : int16 = 0x0020s

    /// `POLLRDNORM`.
    [<Literal>]
    let RdNorm : int16 = 0x0040s

    /// `POLLRDBAND`.
    [<Literal>]
    let RdBand : int16 = 0x0080s

    /// `POLLWRBAND`.
    [<Literal>]
    let WrBand : int16 = 0x0100s

    /// `POLLEXTEND`, a vnode bit.
    [<Literal>]
    let Extend : int16 = 0x0200s

    /// `POLLATTRIB`, a vnode bit.
    [<Literal>]
    let Attrib : int16 = 0x0400s

    /// `POLLNLINK`, a vnode bit.
    [<Literal>]
    let NLink : int16 = 0x0800s

    /// `POLLWRITE`, a vnode bit.
    [<Literal>]
    let Write : int16 = 0x1000s

    /// The bits for which `poll` registers `EVFILT_READ`.
    [<Literal>]
    let ReadGroup : int16 = 0x00D3s

    /// The bits for which `poll` registers `EVFILT_READ` with `EV_OOBAND`.
    [<Literal>]
    let OutOfBandGroup : int16 = 0x0082s

    /// The bits for which `poll` registers `EVFILT_WRITE`.
    [<Literal>]
    let WriteGroup : int16 = 0x0104s

    /// The bits for which `poll` registers `EVFILT_VNODE`.
    [<Literal>]
    let VnodeGroup : int16 = 0x1E00s

/// What a Darwin `poll(2)` makes of the reports of the filters it registered in
/// its own kqueue: XNU's `poll_callback`, and the scan that feeds it.
///
/// The registration half, which decides which filters an entry registers, is
/// `UnixPoll.poll`'s.
[<RequireQualifiedAccess>]
module KqueuePoll =

    /// What the registration of `filter` through the descriptor `fd` reports
    /// now, and whether the report carries `EV_OOBAND`; or `None` when its
    /// filter is not ready.
    ///
    /// Loudly partial: a poll registers only on the targets answered here, and
    /// closing a descriptor removes every registration made through it.
    let private reportOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int, filter : KqueueFilter as key)
        (registration : PollRegistration)
        (system : UnixSystem<'Task, 'Handler>)
        : (KqueueFilterReport * bool) option
        =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        // A socket's filter clears `EV_OOBAND` when it attaches (XNU's
        // `filt_sockattach`), so its report never carries the flag: no socket
        // here holds out-of-band data.
        | Some (OpenFileTarget.Socket socketId) ->
            DarwinReadiness.ofSocket filter socketId system.Machine
            |> Option.map (fun report -> report, false)
        // Every other filter keeps the flags it was registered with, and so
        // reports `EV_OOBAND` back when it was asked for (measured,
        // `poll-darwin.c` section S: a pipe holding data answers `POLLPRI`).
        | Some (OpenFileTarget.Pipe (pipeId, pipeEnd)) ->
            DarwinReadiness.ofPipe filter pipeId pipeEnd system
            |> Option.map (fun report -> report, registration.OutOfBand)
        // `poll` registers with `EV_POLL`, under which a regular file is
        // readable whatever its size and offset, and is always writable
        // (XNU's `vnode_readable_data_count`; measured, `poll-darwin.c`
        // section S, at every access mode and offset).
        | Some (OpenFileTarget.File _) -> Some (KqueueFilterReport.Ready (1L), registration.OutOfBand)
        | other ->
            failwith
                $"KqueuePoll: a poll's kqueue registers %A{key}, whose descriptor names %A{other}. A poll registers a filter only on a socket, a pipe or a regular file, and closing a descriptor removes every registration made through it (this is a bug in this library, or in a caller that assembled the park by hand)."

    /// What a report of `filter`, for an entry asking `events`, adds to the
    /// entry's `revents` so far: XNU's `poll_callback`, in Darwin's numbering
    /// (`DarwinPollEvents`).
    ///
    /// `EV_EOF` adds `POLLHUP`. A read report then adds what was asked of
    /// `IN|RDNORM|PRI|RDBAND` once `revents` holds `POLLHUP`, and otherwise of
    /// `IN|RDNORM`, with `PRI|RDBAND` too when the report carries `EV_OOBAND`.
    /// A write report adds what was asked of `OUT|WRBAND`, but only while
    /// `revents` does not hold `POLLHUP`: which is why a reported hang-up
    /// suppresses `POLLOUT`, and why the order a socket's filters report in
    /// matters.
    let callback
        (events : int16)
        (filter : KqueueFilter)
        (report : KqueueFilterReport)
        (outOfBand : bool)
        (revents : int16)
        : int16
        =
        let revents =
            match report with
            | KqueueFilterReport.EndOfFile _ -> revents ||| DarwinPollEvents.Hup
            | KqueueFilterReport.Ready _ -> revents

        let hungUp = revents &&& DarwinPollEvents.Hup <> 0s

        match filter with
        | KqueueFilter.Read ->
            let mask =
                if hungUp then
                    DarwinPollEvents.In
                    ||| DarwinPollEvents.RdNorm
                    ||| DarwinPollEvents.Pri
                    ||| DarwinPollEvents.RdBand
                elif outOfBand then
                    DarwinPollEvents.In
                    ||| DarwinPollEvents.RdNorm
                    ||| DarwinPollEvents.Pri
                    ||| DarwinPollEvents.RdBand
                else
                    DarwinPollEvents.In ||| DarwinPollEvents.RdNorm

            revents ||| (events &&& mask)
        | KqueueFilter.Write ->
            if hungUp then
                revents
            else
                revents ||| (events &&& (DarwinPollEvents.Out ||| DarwinPollEvents.WrBand))

    /// Whether the registration of `key` is of a socket's filter: the only
    /// registrations a scan walks in the order they were activated (see
    /// `ParkedKqueuePoll.Active`).
    let private isSocket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int, _ : KqueueFilter)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket _) -> true
        | Some _
        | None -> false

    /// The registrations of sockets' filters among `registrations` that are
    /// ready now, in the order they were made: the queue a call has once it
    /// has registered everything, since registering a ready filter activates
    /// it.
    let activeAtRegistration<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (registrations : Map<int * KqueueFilter, PollRegistration>)
        (system : UnixSystem<'Task, 'Handler>)
        : (int * KqueueFilter) list
        =
        registrations
        |> Map.toList
        |> List.sortBy (fun (_, registration) -> registration.RegisteredAt)
        |> List.filter (fun (key, registration) ->
            isSocket key system && Option.isSome (reportOf key registration system)
        )
        |> List.map fst

    /// One scan of a poll's kqueue, as `kqueue_scan` makes one: walk the
    /// activated socket registrations in activation order, then every other
    /// registration whose filter is ready, in the order they were made; fold
    /// each report into its entry's `revents` (see `callback`); and remove
    /// each registration that reports, since `poll` registers each once only,
    /// whether or not its report added anything. A socket's registration that
    /// is no longer ready leaves the active list and stays registered.
    ///
    /// Answers each entry's `revents`, in order, starting from `revents`, and
    /// the registrations and active list the scan leaves.
    let scan<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (entries : PollEntry list)
        (revents : int16 list)
        (registrations : Map<int * KqueueFilter, PollRegistration>)
        (active : (int * KqueueFilter) list)
        (system : UnixSystem<'Task, 'Handler>)
        : int16 list * Map<int * KqueueFilter, PollRegistration> * (int * KqueueFilter) list
        =
        if List.length revents <> List.length entries then
            failwith
                $"KqueuePoll.scan: %d{List.length revents} revents for %d{List.length entries} entries (this is a bug in this library)."

        let lazilyActive =
            registrations
            |> Map.toList
            |> List.sortBy (fun (_, registration) -> registration.RegisteredAt)
            |> List.filter (fun (key, registration) ->
                not (isSocket key system) && Option.isSome (reportOf key registration system)
            )
            |> List.map fst

        let events = entries |> List.map (fun entry -> entry.Events) |> List.toArray

        let walk
            (revents : int16 array, registrations : Map<int * KqueueFilter, PollRegistration>)
            (_, filter as key : int * KqueueFilter)
            =
            match Map.tryFind key registrations with
            | None ->
                failwith
                    $"KqueuePoll.scan: the active list names %A{key}, which is not registered. ParkedKqueuePoll.Active is a subset of its registrations, and UnixSystem.checkInvariants says so (this is a bug in this library, or in a caller that assembled the park by hand)."
            | Some registration ->

            match reportOf key registration system with
            | None -> revents, registrations
            | Some (report, outOfBand) ->
                let entry = registration.Entry
                let revents = Array.copy revents
                revents.[entry] <- callback events.[entry] filter report outOfBand revents.[entry]
                revents, Map.remove key registrations

        let revents, remaining =
            ((List.toArray revents, registrations), active @ lazilyActive)
            ||> List.fold walk

        List.ofArray revents, remaining, []

    /// Whether the Darwin `poll` `task` is asleep in would report something were
    /// it to scan its kqueue now: what `WakePrimitive.KqueuePollReportable`
    /// asks.
    ///
    /// Loudly partial: `task` must be asleep in a Darwin `poll`.
    let reportable<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some (ParkedSyscall.KqueuePoll poll) ->
            let zero = poll.Entries |> List.map (fun _ -> 0s)
            let revents, _, _ = scan poll.Entries zero poll.Registrations poll.Active system
            revents |> List.exists (fun revents -> revents <> 0s)
        | other ->
            failwith
                $"KqueuePoll.reportable: task %O{task} is parked in %A{other}, not in a Darwin poll (this is a bug in the caller that recorded the park)."

    /// What closing the descriptor `fd` does to the kqueue of every Darwin
    /// `poll` asleep in `tasks`: each filter registered through `fd` goes, as
    /// `FileDescriptorRegistry.dropDescriptor` removes those of every kqueue the
    /// process holds (XNU's `knote_fdclose`). The poll sleeps on, and the entry
    /// reports nothing more, whatever a new descriptor at the number does.
    let dropRegistrationsThrough<'Task when 'Task : comparison>
        (fd : int)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        tasks
        |> Map.map (fun _ task ->
            match task.Parked with
            | Some ({
                        Syscall = ParkedSyscall.KqueuePoll poll
                    } as park) when poll.Registrations |> Map.exists (fun (registered, _) _ -> registered = fd) ->
                { task with
                    Parked =
                        Some
                            { park with
                                Syscall =
                                    ParkedSyscall.KqueuePoll
                                        { poll with
                                            Registrations =
                                                poll.Registrations
                                                |> Map.filter (fun (registered, _) _ -> registered <> fd)
                                            Active = poll.Active |> List.filter (fun (active, _) -> active <> fd)
                                        }
                            }
                }
            | Some _
            | None -> task
        )
