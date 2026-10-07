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

/// Darwin's `<sys/event.h>` flag bits, of a change in the changelist and of an
/// entry in the eventlist.
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

    /// `EV_ERROR`: in an entry `kevent` writes, the entry echoes a change, and
    /// `data` holds its outcome (0, or an errno).
    [<Literal>]
    let Error : uint16 = 0x4000us

    /// `EV_EOF`: in an event `kevent` reports, the descriptor can receive no
    /// more (or, for `EVFILT_WRITE`, send no more).
    [<Literal>]
    let Eof : uint16 = 0x8000us

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
    /// A descriptor the call would make lies at or above the bound this kernel
    /// assumes the process's `RLIMIT_NOFILE` reaches.
    | DescriptorLimit of DescriptorLimitRefusal
    /// This kernel is not Darwin-flavoured, and only Darwin has kqueue.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module KqueueRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which of its entry points asked.
    let describe (refusal : KqueueRefusal) : string =
        match refusal with
        | KqueueRefusal.DescriptorLimit refusal -> DescriptorLimitRefusal.describe refusal
        | KqueueRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and kqueue exists on Darwin only."

/// What became of a `kevent(2)` this kernel could answer.
[<RequireQualifiedAccess>]
type KeventOutcome =
    /// `kevent` failed with this errno.
    ///
    /// Any change the call applied before it failed stays applied, with the
    /// system this rides with. A finishing call fails with `EBADF` when a close
    /// has drained the kqueue (see `KqueueState.Drained`), and with `EINTR`
    /// when a signal with a handler interrupts the wait.
    | Failed of error : UnixError
    /// `kevent` returned these entries of its changelist, in changelist order:
    /// each change made with `EV_RECEIPT`, or that failed, while the eventlist
    /// had room. Each is the change as given, with `KeventFlags.Error` added to
    /// its flags and `data` its outcome: 0, or the raw Darwin errno it failed
    /// with.
    ///
    /// A call that returns any such entry reports no events and does not wait,
    /// whatever is queued and whatever its timeout.
    | Echoed of changes : Kevent list
    /// `kevent` failed with this errno, having first written these entries of
    /// its changelist into the eventlist, as `Echoed` describes them. The call
    /// returned -1, so only the eventlist's memory shows them.
    ///
    /// Any change the call applied before it failed stays applied, with the
    /// system this rides with.
    | FailedAfterEchoing of error : UnixError * changes : Kevent list
    /// `kevent` returned these events, in the order it reports them.
    ///
    /// Empty for a wait that timed out, for a timeout of zero with nothing to
    /// report, and for an `nevents` of zero or less.
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
    /// The changelist holds this change, whose flags are not one of the
    /// combinations this library applies: `EV_ADD`, optionally with
    /// `EV_CLEAR` and `EV_RECEIPT`, or `EV_DELETE`, optionally with
    /// `EV_RECEIPT`.
    | UnmodelledFlags of change : Kevent
    /// The changelist holds this change, whose filter is neither `EVFILT_READ`
    /// nor `EVFILT_WRITE`.
    | UnmodelledFilter of change : Kevent
    /// The changelist holds this change, whose `fflags` or `data` is not zero.
    /// What a filter makes of its parameters (`NOTE_LOWAT`, for instance) is
    /// not modelled.
    | UnmodelledFilterParameters of change : Kevent
    /// The changelist holds this `EV_ADD`, whose descriptor names something
    /// other than a socket. A regular file, a pipe and a kqueue register on
    /// Darwin, and what they report is not modelled.
    | UnmodelledTarget of change : Kevent * target : OpenFileTarget
    /// The changelist holds this `EV_ADD`, whose descriptor names a socket of
    /// a kind whose filters this library does not model (see
    /// `DarwinReadiness.modelsSocket`).
    | UnmodelledSocket of change : Kevent * domain : SocketDomain * kind : SocketKind
    /// The eventlist reached a copy and has no address to copy to, or bytes
    /// the caller cannot produce.
    | Buffer of BufferRefusal
    /// The call has events to copy out to the eventlist, and the eventlist is
    /// unmapped, so the copy faults. What Darwin answers then for several
    /// events, and what becomes of each, is not measured. (An entry of the
    /// changelist echoed into such an eventlist is answered: `EFAULT`.)
    | UnmeasuredCopyOutFault of kqueue : OpenFileDescriptionId
    /// Nothing is reportable, and the timeout ends past the last instant the
    /// machine's monotonic clock (`UnixMachineState.NanosecondsSinceBoot`, an
    /// `int64` of nanoseconds) can represent: `nanosecondsSinceBoot` plus the
    /// timeout overflows it.
    | DeadlineBeyondClock of nanosecondsSinceBoot : int64 * seconds : int64 * nanoseconds : int64
    /// The wait was asleep, and both a close has drained the kqueue and the
    /// wait's deadline has passed. Darwin answers whichever of the two reached
    /// the sleeping task first, which this library does not record.
    | DrainBesideDeadline of kqueue : OpenFileDescriptionId
    /// The wait was asleep, and both a close has drained the kqueue and it has
    /// an event to report. Darwin answers whichever of the two reached the
    /// sleeping task first, which this library does not record.
    | DrainBesideEvents of kqueue : OpenFileDescriptionId
    /// The wait was asleep, and both its deadline has passed and the kqueue has
    /// an event to report. Darwin answers whichever of the two reached the
    /// sleeping task first, which this library does not record.
    | EventsBesideDeadline of kqueue : OpenFileDescriptionId
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
        | KeventRefusal.UnmodelledFlags change ->
            $"the changelist holds %A{change}, whose flags 0x%x{change.Flags} are not EV_ADD (optionally with EV_CLEAR and EV_RECEIPT) or EV_DELETE (optionally with EV_RECEIPT), the only changes this kernel applies."
        | KeventRefusal.UnmodelledFilter change ->
            $"the changelist holds %A{change}, whose filter %d{change.Filter} is neither EVFILT_READ nor EVFILT_WRITE, the only filters this kernel models."
        | KeventRefusal.UnmodelledFilterParameters change ->
            $"the changelist holds %A{change}, whose fflags (0x%x{change.FilterFlags}) or data (%d{change.Data}) is not zero, and this kernel does not model what a filter makes of its parameters."
        | KeventRefusal.UnmodelledTarget (change, target) ->
            $"the changelist holds %A{change}, an EV_ADD on a descriptor naming %A{target}. Darwin registers a filter on a regular file, a pipe and a kqueue too, but this kernel models a filter's readiness for sockets only."
        | KeventRefusal.UnmodelledSocket (change, domain, kind) ->
            $"the changelist holds %A{change}, an EV_ADD on a %O{kind} socket in %O{domain}. This kernel models a filter's readiness for IPv4 and IPv6 stream sockets only: what activates a datagram socket's filters is not modelled, and a Unix-domain socket's are not measured."
        | KeventRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | KeventRefusal.UnmeasuredCopyOutFault kqueue ->
            $"kevent on kqueue %O{kqueue} has events to copy out, and the eventlist is unmapped, so the copy faults. What Darwin answers then, and what becomes of each event it would have copied, is not measured."
        | KeventRefusal.DeadlineBeyondClock (now, seconds, nanoseconds) ->
            $"the machine has been up for %d{now} ns and the timeout is %d{seconds} s and %d{nanoseconds} ns, which ends past the last nanosecond the monotonic clock can represent."
        | KeventRefusal.DrainBesideDeadline kqueue ->
            $"a task asleep in kevent on kqueue %O{kqueue} has both had the kqueue drained by a close (EBADF) and reached its deadline (0 events). Darwin answers whichever of the two reached the sleeping task first, and this kernel does not record which did."
        | KeventRefusal.DrainBesideEvents kqueue ->
            $"a task asleep in kevent on kqueue %O{kqueue} has both had the kqueue drained by a close (EBADF) and an event to report. Darwin answers whichever of the two reached the sleeping task first, and this kernel does not record which did."
        | KeventRefusal.EventsBesideDeadline kqueue ->
            $"a task asleep in kevent on kqueue %O{kqueue} has both reached its deadline (0 events) and an event to report. Darwin answers whichever of the two reached the sleeping task first, and this kernel does not record which did."
        | KeventRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal

/// Darwin's kqueue: `kqueue(2)`, and `kevent(2)`.
[<RequireQualifiedAccess>]
module UnixKqueue =

    /// `kqueue(2)`: create a kqueue, and a descriptor onto it, the lowest one
    /// not in use. The description is blocking, and the descriptor has
    /// `FD_CLOEXEC` and `FD_CLOFORK`.
    ///
    /// Under the Linux flavour every call is refused: Linux has no kqueue.
    let kqueue<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int * UnixSystem<'Task, 'Handler>, KqueueRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> Error (KqueueRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux)
        | SimulatedUnixFlavour.Darwin ->

        match
            FileDescriptorRegistry.room
                (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform)
                0
                1
                (UnixSystemState.fileDescriptors system)
        with
        | Error refusal -> Error (KqueueRefusal.DescriptorLimit refusal)
        | Ok () ->

        // Measured on 27.0.0 (`kqueue-kevent.c`, section A): the lowest free
        // descriptor, O_RDWR, not O_NONBLOCK; and (`fcntl-dup.c`, KIND rows)
        // F_GETFD reports FD_CLOEXEC|FD_CLOFORK.
        let fd, registry =
            FileDescriptorRegistry.createKqueue (UnixSystemState.fileDescriptors system)
            |> fun (fd, registry) ->
                fd,
                FileDescriptorRegistry.setFlags
                    fd
                    {
                        CloseOnExec = true
                        CloseOnFork = true
                    }
                    registry

        Ok (fd, UnixSystemState.withFileDescriptors registry system)

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

    let private kqueueState<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : KqueueState
        =
        match Map.tryFind kqueue (OpenFileTable.descriptions system.Machine.OpenFiles) with
        | Some {
                   Target = OpenFileTarget.Kqueue state
               } -> state
        | other ->
            failwith
                $"UnixKqueue: %O{kqueue} names %A{other} rather than a live kqueue, where the call resolved it as one (this is a bug in this library)."

    let private withKqueueState<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (state : KqueueState)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        UnixSystemState.mapOpenFiles (OpenFileTable.setKqueueState kqueue state) system

    let private filterNumber (filter : KqueueFilter) : int16 =
        match filter with
        | KqueueFilter.Read -> KeventFilter.Read
        | KqueueFilter.Write -> KeventFilter.Write

    /// The event `report` is, in Darwin's numbering.
    let private eventOf (platform : SimulatedUnixPlatform) (report : KqueueReport) : Kevent =
        let registration = report.Registration

        // Measured on 27.0.0 (`kevent-register.c`): an event carries the
        // registration's flags as its first EV_ADD gave them (D3, D4, X7), and
        // EV_EOF with the pending error in `fflags` (P3).
        let flags =
            KeventFlags.Add
            ||| (if registration.Clear then KeventFlags.Clear else 0us)
            ||| (if registration.Receipt then KeventFlags.Receipt else 0us)

        let flags, filterFlags, data =
            match report.Report with
            | KqueueFilterReport.Ready data -> flags, 0u, data
            | KqueueFilterReport.EndOfFile (data, error) ->
                let filterFlags =
                    match error with
                    | None -> 0u
                    | Some error ->
                        uint32 (UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error)

                flags ||| KeventFlags.Eof, filterFlags, data

        {
            Ident = uint64 report.Fd
            Filter = filterNumber report.Filter
            Flags = flags
            FilterFlags = filterFlags
            Data = data
            UserData = registration.UserData
        }

    /// Whether `count` entries or events can be copied out to `eventlist`: a
    /// call that copies nothing never looks at it.
    let private copyOut
        (kqueue : OpenFileDescriptionId)
        (eventlist : UserBuffer)
        (count : int)
        : Result<unit, KeventRefusal>
        =
        if count = 0 then
            Ok ()
        else
            match eventlist with
            | UserBuffer.Mapped -> Ok ()
            | UserBuffer.Unmapped _ -> Error (KeventRefusal.UnmeasuredCopyOutFault kqueue)
            | UserBuffer.Opaque -> Error (KeventRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (KeventRefusal.Buffer BufferRefusal.AddresslessAtTransfer)

    /// Apply one readable change to the kqueue `kqueue`: `None` when it
    /// succeeded, or the errno it failed with, and the system it left.
    let private applyChange<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (change : Kevent)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixError option * UnixSystem<'Task, 'Handler>, KeventRefusal>
        =
        let add = KeventFlags.Add
        let delete = KeventFlags.Delete

        let adds =
            [
                add
                add ||| KeventFlags.Clear
                add ||| KeventFlags.Receipt
                add ||| KeventFlags.Clear ||| KeventFlags.Receipt
            ]

        let deletes = [ delete ; delete ||| KeventFlags.Receipt ]

        let filter =
            match change.Filter with
            | KeventFilter.Read -> Some KqueueFilter.Read
            | KeventFilter.Write -> Some KqueueFilter.Write
            | _ -> None

        if not (List.contains change.Flags adds || List.contains change.Flags deletes) then
            Error (KeventRefusal.UnmodelledFlags change)
        else

        match filter with
        | None -> Error (KeventRefusal.UnmodelledFilter change)
        | Some filter ->

        if change.FilterFlags <> 0u || change.Data <> 0L then
            Error (KeventRefusal.UnmodelledFilterParameters change)
        else

        let state = kqueueState kqueue system

        if List.contains change.Flags deletes then
            // Measured on 27.0.0 (`kevent-register.c`, R4, R10, X2, X3):
            // deleting what is not registered is ENOENT whatever the ident
            // names, a closed descriptor and every non-socket included. Only a
            // descriptor number is ever registered, so a larger ident names
            // nothing.
            let key =
                if change.Ident <= uint64 System.Int32.MaxValue then
                    Some (int change.Ident, filter)
                else
                    None

            match key with
            | Some key when Map.containsKey key state.Registrations ->
                let state =
                    { state with
                        Registrations = Map.remove key state.Registrations
                        Active = state.Active |> List.filter (fun active -> active <> key)
                    }

                Ok (None, withKqueueState kqueue state system)
            | Some _
            | None -> Ok (Some UnixError.ENOENT, system)
        else

        // Measured on 27.0.0 (`kevent-register.c`, X2, over six high halves by
        // five low halves): EV_ADD reads the ident's low 32 bits as a
        // descriptor, and one that is not open (a negative one included) is
        // EBADF; an open one with any high bit set is EINVAL.
        let fd = int (uint32 (change.Ident &&& 0xFFFF_FFFFUL))

        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | None -> Ok (Some UnixError.EBADF, system)
        | Some (OpenFileTarget.Socket socketId) ->
            let socket = UnixMachineState.socket socketId system.Machine

            if not (DarwinReadiness.modelsSocket socket) then
                Error (KeventRefusal.UnmodelledSocket (change, socket.Domain, socket.Kind))
            elif change.Ident >>> 32 <> 0UL then
                Ok (Some UnixError.EINVAL, system)
            else

            let key = fd, filter

            // Measured on 27.0.0 (`kevent-register.c`, D1 to D8, X7): an ADD of
            // a pair already registered keeps its flags and replaces its udata;
            // either way the registration is activated if its filter is ready.
            let state, system =
                match Map.tryFind key state.Registrations with
                | Some registration ->
                    { state with
                        Registrations =
                            Map.add
                                key
                                { registration with
                                    UserData = change.UserData
                                }
                                state.Registrations
                    },
                    system
                | None ->
                    let ordinal = system.Machine.NextEventRegistrationOrdinal

                    let registration =
                        {
                            Clear = change.Flags &&& KeventFlags.Clear <> 0us
                            Receipt = change.Flags &&& KeventFlags.Receipt <> 0us
                            UserData = change.UserData
                            RegisteredAt = ordinal
                        }

                    { state with
                        Registrations = Map.add key registration state.Registrations
                    },
                    { system with
                        Machine =
                            { system.Machine with
                                NextEventRegistrationOrdinal = ordinal + 1L
                            }
                    }

            Ok (
                None,
                withKqueueState kqueue state system
                |> KqueueQueue.activateRegistration kqueue key
            )
        | Some target -> Error (KeventRefusal.UnmodelledTarget (change, target))

    /// The outcome of the changelist: the entries it echoes, or the errno that
    /// ended the call and the entries echoed before it; and the system its
    /// applied changes left.
    [<RequireQualifiedAccess>]
    type private ChangelistOutcome<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
        | Applied of echoed : Kevent list * system : UnixSystem<'Task, 'Handler>
        | Failed of error : UnixError * echoed : Kevent list * system : UnixSystem<'Task, 'Handler>

    /// Apply the readable `changes` of a changelist of `nchanges` entries in
    /// order, with room for `room` entries in `eventlist`.
    let private applyChangelist<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (kqueue : OpenFileDescriptionId)
        (nchanges : int)
        (changes : Kevent list)
        (room : int)
        (eventlist : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ChangelistOutcome<'Task, 'Handler>, KeventRefusal>
        =
        let numbering = SimulatedUnixPlatform.rawErrnoNumbering system.Machine.UnixPlatform

        // Measured on 27.0.0 (`kevent-register.c`, sections R, X and Y): a
        // change with EV_RECEIPT, or one that fails, is echoed while there is
        // room; with no room a receipt is dropped and its change still applies,
        // and a failure ends the call, the changes after it unapplied and the
        // entries echoed before it left in the eventlist (Y4). An echo into an
        // eventlist that cannot be written ends the call with EFAULT, its change
        // applied (Y5).
        let rec apply
            (echoed : Kevent list)
            (room : int)
            (system : UnixSystem<'Task, 'Handler>)
            (remaining : Kevent list)
            : Result<ChangelistOutcome<'Task, 'Handler>, KeventRefusal>
            =
            match remaining with
            | [] -> Ok (ChangelistOutcome.Applied (List.rev echoed, system))
            | change :: rest ->

            match applyChange kqueue change system with
            | Error refusal -> Error refusal
            | Ok (failure, system) ->

            let echoes = failure.IsSome || change.Flags &&& KeventFlags.Receipt <> 0us

            if not echoes then
                apply echoed room system rest
            elif room > 0 then
                match eventlist with
                | UserBuffer.Opaque -> Error (KeventRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
                | UserBuffer.Addressless -> Error (KeventRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
                | UserBuffer.Unmapped _ -> Ok (ChangelistOutcome.Failed (UnixError.EFAULT, List.rev echoed, system))
                | UserBuffer.Mapped ->
                    let entry =
                        { change with
                            Flags = change.Flags ||| KeventFlags.Error
                            Data =
                                match failure with
                                | None -> 0L
                                | Some error -> int64 (UnixError.toRawErrnoUnder numbering error)
                        }

                    apply (entry :: echoed) (room - 1) system rest
            else
                match failure with
                | Some error -> Ok (ChangelistOutcome.Failed (error, List.rev echoed, system))
                | None -> apply echoed room system rest

        match apply [] room system changes with
        // Measured (Y3): a change the copy-in cannot read ends the call with
        // EFAULT, the readable ones before it applied.
        | Ok (ChangelistOutcome.Applied (echoed, system)) when nchanges > List.length changes ->
            Ok (ChangelistOutcome.Failed (UnixError.EFAULT, echoed, system))
        | other -> other

    /// `kevent(2)`, made by `task` through the descriptor `kq`, with a
    /// changelist of `nchanges` entries and room for `nevents` events in
    /// `eventlist`.
    ///
    /// `changes` is what the copy-in finds at the changelist, from its start:
    /// every one of the `nchanges` entries when all are readable, and fewer
    /// when one is not, the next entry being the first that cannot be read.
    /// It is never longer than `nchanges`, and is empty when `nchanges` is zero
    /// or less, since then no entry is read.
    ///
    /// The argument checks come first, in Darwin's order: `EFAULT` for an
    /// unreadable timeout and `EINVAL` for a timeout whose `tv_sec` is below 0
    /// or above `INT32_MAX`, or whose `tv_nsec` is below 0 or above
    /// 1000000000; then `EBADF` for a descriptor that is not open or is not a
    /// kqueue. Such a failure changes nothing.
    ///
    /// Then the changes are applied in order. `EV_ADD` registers a filter on
    /// the socket the change's descriptor names, or, of a pair already
    /// registered, replaces its `udata`; `EV_DELETE` removes one. A change with
    /// `EV_RECEIPT`, or one that fails, is echoed into the eventlist while it
    /// has room (`KeventOutcome.Echoed`); a failure with no room left ends the
    /// call with its errno, a change the copy-in could not read with `EFAULT`,
    /// and an echo into an `Unmapped` eventlist with `EFAULT` too, and either
    /// way the changes before it stay applied and the entries echoed before it
    /// stay written (`KeventOutcome.FailedAfterEchoing`). A call that echoes
    /// anything returns at once with no events.
    ///
    /// Otherwise an `nevents` of zero or less returns no events at once; a
    /// kqueue a close has drained (see `KqueueState.Drained`) fails with
    /// `EBADF`; and otherwise the call reports up to `nevents` events (see
    /// `KqueueQueue.drain`). With nothing to report, a timeout of zero returns
    /// no events at once; a positive one parks `task` until an event is
    /// reportable or that much time has passed on the machine's monotonic
    /// clock, and a null one parks it until an event is reportable. A parked
    /// wait is finished with `finishKevent`.
    ///
    /// Every waiter on one kqueue is woken by an event to report and by the
    /// close that drains it, and a deadline or a signal is each waiter's own.
    ///
    /// Refused are a change this library does not apply (see `KeventRefusal`),
    /// a copy of events to an eventlist that is not `Mapped`, and an echo into
    /// an `Opaque` or `Addressless` one.
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
        | Error error -> Ok (KeventOutcome.Failed error, system)
        | Ok timeout ->

        // "Not a kqueue" is EBADF, as "not open" is.
        let kqueue =
            match FileDescriptorRegistry.tryFindWithId kq (UnixSystemState.fileDescriptors system) with
            | Some (id,
                    {
                        Target = OpenFileTarget.Kqueue _
                    }) -> Some id
            | Some _
            | None -> None

        match kqueue with
        | None -> Ok (KeventOutcome.Failed UnixError.EBADF, system)
        | Some kqueue ->

        match applyChangelist kqueue nchanges changes (max nevents 0) eventlist system with
        | Error refusal -> Error refusal
        | Ok (ChangelistOutcome.Failed (error, [], system)) -> Ok (KeventOutcome.Failed error, system)
        | Ok (ChangelistOutcome.Failed (error, echoed, system)) ->
            Ok (KeventOutcome.FailedAfterEchoing (error, echoed), system)
        | Ok (ChangelistOutcome.Applied (echoed, system)) ->

        if not (List.isEmpty echoed) then
            Ok (KeventOutcome.Echoed echoed, system)
        elif nevents <= 0 then
            Ok (KeventOutcome.Answered [], system)
        // Measured (`kevent-register.c`, Y1): a wait that reaches a drained
        // kqueue is EBADF even with an event to report.
        elif drained task kqueue system then
            Ok (KeventOutcome.Failed UnixError.EBADF, system)
        else

        // The walk's consumption stands whether or not the call then sleeps.
        let reports, system = KqueueQueue.drain kqueue nevents system

        if not (List.isEmpty reports) then
            copyOut kqueue eventlist (List.length reports)
            |> Result.map (fun () ->
                KeventOutcome.Answered (reports |> List.map (eventOf system.Machine.UnixPlatform)), system
            )
        else

        let park (deadline : int64 option) =
            let parked =
                ParkedSyscall.Kevent
                    {
                        Kqueue = kqueue
                        Fd = kq
                        MaxEvents = nevents
                        Buffer = eventlist
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

        let deliverable = KqueueQueue.hasDeliverableEvent parked.Kqueue system

        let timedOut =
            match parked.Deadline with
            | Some deadline -> system.Machine.NanosecondsSinceBoot >= deadline
            | None -> false

        let completing (outcome : KeventOutcome) (answered : UnixSystem<'Task, 'Handler>) =
            SyscallInterruption.beforeCompleting task system
            |> Result.mapError KeventRefusal.Interruption
            |> Result.map (fun () ->
                outcome,
                { answered with
                    Tasks = UnixTaskTable.unpark task answered.Tasks
                }
            )

        if isDrained && timedOut then
            Error (KeventRefusal.DrainBesideDeadline parked.Kqueue)
        elif isDrained && deliverable then
            Error (KeventRefusal.DrainBesideEvents parked.Kqueue)
        elif isDrained then
            // Measured on 27.0.0 (`kqueue-kevent.c`, section E).
            completing (KeventOutcome.Failed UnixError.EBADF) system
        elif deliverable && timedOut then
            Error (KeventRefusal.EventsBesideDeadline parked.Kqueue)
        elif deliverable then
            let reports, answered = KqueueQueue.drain parked.Kqueue parked.MaxEvents system

            copyOut parked.Kqueue parked.Buffer (List.length reports)
            |> Result.bind (fun () ->
                completing
                    (KeventOutcome.Answered (reports |> List.map (eventOf system.Machine.UnixPlatform)))
                    answered
            )
        elif timedOut then
            completing (KeventOutcome.Answered []) system
        else

        // Measured on 27.0.0 (`kqueue-kevent.c`, section D): a caught signal
        // ends the wait with EINTR, under SA_RESTART or not.
        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (KeventRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) ->
            Ok (
                KeventOutcome.Failed UnixError.EINTR,
                { system with
                    Tasks = UnixTaskTable.unpark task system.Tasks
                }
            )
        | Ok (Some SyscallInterruption.Restart) ->
            failwith
                "UnixKqueue.finishKevent: a kevent restarted after a signal, where `SyscallInterruption.ruleOf` says one never restarts (this is a bug in this library)."
        | Ok None ->
            // Whatever woke the task has gone again: another waiter took it, or
            // its filter is no longer ready. The task walks the queue as a woken
            // real wait does, which reports nothing and drops each entry whose
            // filter is no longer ready, and sleeps again.
            let _, swept = KqueueQueue.drain parked.Kqueue parked.MaxEvents system
            let parkedAgain = ParkedSyscall.Kevent parked
            Ok (KeventOutcome.WouldBlock (WakeCondition.ofPark parkedAgain), UnixWait.park task parkedAgain swept)

    /// Finish the `kevent` wait `task` is parked in, as a woken real wait does.
    ///
    /// Answers `EBADF` when a close has drained the kqueue; the events it has to
    /// report (see `KqueueQueue.drain`), up to the call's `nevents`; no events
    /// when the deadline has passed; `EINTR` when a signal with a handler
    /// interrupts the wait; and otherwise re-parks the task on the same kqueue
    /// and deadline. Refused, since Darwin answers whichever reached the
    /// sleeping task first and this library does not record which did: a
    /// drain beside an expired deadline or an event, an event beside an
    /// expired deadline, and a signal pending beside any answer but `EINTR`.
    /// An answer clears the park, and with it the call's hold on the kqueue,
    /// which goes then if no descriptor names it any more.
    ///
    /// Reporting events copies them out to the eventlist the call was made
    /// with; see `kevent` for the eventlists that refuses.
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
