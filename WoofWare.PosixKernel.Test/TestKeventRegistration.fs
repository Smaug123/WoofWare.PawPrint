namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `kevent`'s changelist, against a reference model of registrations and their
/// activation, and the refusals around it.
///
/// The model below restates, independently of the library, what
/// `docs/plans/2026-08-23-posix-kernel-extraction/kevent-register.c` measured on
/// Darwin 27.0.0: how a change is applied and echoed, which filters each socket event
/// activates and in what order, how a wait walks the queue, and that a close removes
/// the registrations made through the descriptor. A random sequence of socket
/// operations, changelists and waits is run through the kernel and the model side by
/// side, and the two must agree on every answer and on every kqueue's registrations
/// and queue after every step. `TestKeventMeasured` holds the measured rows
/// themselves.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKeventRegistration =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    // ------------------------------------------------------------------
    // The model
    // ------------------------------------------------------------------

    type private ModelRegistration =
        {
            Clear : bool
            Receipt : bool
            UserData : uint64
            RegisteredAt : int64
        }

    type private ModelKqueue =
        {
            Registrations : Map<int * KqueueFilter, ModelRegistration>
            Active : (int * KqueueFilter) list
        }

    type private Model =
        {
            Kqueues : Map<int, ModelKqueue>
            NextOrdinal : int64
        }

    /// The socket a descriptor names in `system`, if it names one.
    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketId option =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) -> Some socketId
        | _ -> None

    /// What the filter reports of a stream socket, as measured, with the send buffer's
    /// free space as `None`: `(data, end of file, raw pending error)`, or `None` when it
    /// is not ready.
    let private measuredReadiness
        (filter : KqueueFilter)
        (socketId : SocketId)
        (system : UnixSystem<int, string>)
        : (int64 option * bool * uint32) option
        =
        let phase = (UnixMachineState.socket socketId system.Machine).Phase

        let peerOpen (connection : ConnectionId) =
            system.Machine.Sockets
            |> Map.exists (fun other socket ->
                other <> socketId
                && (
                    match socket.Phase with
                    | SocketPhase.Established c -> c = connection
                    | SocketPhase.Listening listenState -> List.contains connection listenState.Queue
                    | _ -> false
                )
            )

        match phase, filter with
        | SocketPhase.Listening listenState, KqueueFilter.Read when not (List.isEmpty listenState.Queue) ->
            Some (Some (int64 (List.length listenState.Queue)), false, 0u)
        | SocketPhase.Established connection, KqueueFilter.Read when not (peerOpen connection) ->
            Some (Some 0L, true, 0u)
        | SocketPhase.Established _, KqueueFilter.Write -> Some (None, false, 0u)
        | SocketPhase.Refused error, _ ->
            let errno =
                match error with
                | RefusalError.Pending -> 61u
                | RefusalError.Reported -> 0u

            let data =
                match filter with
                | KqueueFilter.Read -> Some 0L
                | KqueueFilter.Write -> None

            Some (data, true, errno)
        | _ -> None

    let private filterOf (number : int16) : KqueueFilter option =
        match number with
        | -1s -> Some KqueueFilter.Read
        | -2s -> Some KqueueFilter.Write
        | _ -> None

    /// Queue, in every kqueue, each registration of `socketId` for each of `filters`
    /// whose filter is ready and which is not already queued: the newest first among
    /// one filter's.
    let private activate
        (system : UnixSystem<int, string>)
        (socketId : SocketId)
        (filters : KqueueFilter list)
        (model : Model)
        : Model
        =
        { model with
            Kqueues =
                model.Kqueues
                |> Map.map (fun _ kq ->
                    let entering =
                        filters
                        |> List.collect (fun filter ->
                            kq.Registrations
                            |> Map.toList
                            |> List.filter (fun ((fd, f), _) -> f = filter && socketOf fd system = Some socketId)
                            |> List.sortByDescending (fun (_, registration) -> registration.RegisteredAt)
                            |> List.map fst
                        )
                        |> List.filter (fun (fd, filter as key) ->
                            not (List.contains key kq.Active)
                            && (measuredReadiness filter (Option.get (socketOf fd system)) system).IsSome
                        )

                    { kq with
                        Active = kq.Active @ entering
                    }
                )
        }

    /// Remove every registration made through `fd`.
    let private sweep (fd : int) (model : Model) : Model =
        { model with
            Kqueues =
                model.Kqueues
                |> Map.map (fun _ kq ->
                    {
                        Registrations = kq.Registrations |> Map.filter (fun (registered, _) _ -> registered <> fd)
                        Active = kq.Active |> List.filter (fun (active, _) -> active <> fd)
                    }
                )
        }

    /// Apply a changelist of supported changes to the model's kqueue `kq`, with room
    /// for `room` entries: the entries echoed or the errno that ends the call, and the
    /// model after.
    let private applyChanges
        (system : UnixSystem<int, string>)
        (kq : int)
        (changes : Kevent list)
        (room : int)
        (model : Model)
        : Result<Kevent list, UnixError * Kevent list> * Model
        =
        let rec go (echoed : Kevent list) (room : int) (model : Model) (remaining : Kevent list) =
            match remaining with
            | [] -> Ok (List.rev echoed), model
            | change :: rest ->
                let state = model.Kqueues.[kq]
                let filter = Option.get (filterOf change.Filter)

                let failure, model =
                    if change.Flags &&& KeventFlags.Delete <> 0us then
                        let key = int change.Ident, filter

                        if change.Ident <= 0x7fffffffUL && Map.containsKey key state.Registrations then
                            None,
                            { model with
                                Kqueues =
                                    model.Kqueues
                                    |> Map.add
                                        kq
                                        {
                                            Registrations = Map.remove key state.Registrations
                                            Active = state.Active |> List.filter ((<>) key)
                                        }
                            }
                        else
                            Some UnixError.ENOENT, model
                    else
                        let fd = int (uint32 change.Ident)

                        match socketOf fd system with
                        | None -> Some UnixError.EBADF, model
                        | Some _ when change.Ident > 0xffffffffUL -> Some UnixError.EINVAL, model
                        | Some socketId ->
                            let key = fd, filter

                            let registrations, nextOrdinal =
                                match Map.tryFind key state.Registrations with
                                | Some registration ->
                                    Map.add
                                        key
                                        { registration with
                                            UserData = change.UserData
                                        }
                                        state.Registrations,
                                    model.NextOrdinal
                                | None ->
                                    Map.add
                                        key
                                        {
                                            Clear = change.Flags &&& KeventFlags.Clear <> 0us
                                            Receipt = change.Flags &&& KeventFlags.Receipt <> 0us
                                            UserData = change.UserData
                                            RegisteredAt = model.NextOrdinal
                                        }
                                        state.Registrations,
                                    model.NextOrdinal + 1L

                            let ready = (measuredReadiness filter socketId system).IsSome

                            let active =
                                if ready && not (List.contains key state.Active) then
                                    state.Active @ [ key ]
                                else
                                    state.Active

                            None,
                            {
                                Kqueues =
                                    model.Kqueues
                                    |> Map.add
                                        kq
                                        {
                                            Registrations = registrations
                                            Active = active
                                        }
                                NextOrdinal = nextOrdinal
                            }

                if failure.IsNone && change.Flags &&& KeventFlags.Receipt = 0us then
                    go echoed room model rest
                elif room > 0 then
                    let entry =
                        { change with
                            Flags = change.Flags ||| 0x4000us
                            Data =
                                match failure with
                                | None -> 0L
                                | Some error -> int64 (UnixError.toRawErrnoUnder RawErrnoNumbering.Darwin error)
                        }

                    go (entry :: echoed) (room - 1) model rest
                else
                    match failure with
                    | Some error -> Error (error, List.rev echoed), model
                    | None -> go echoed room model rest

        go [] room model changes

    /// A wait on the model's kqueue `kq` for up to `room` events: what it reports, as
    /// `(fd, filter, registration, readiness)`, and the model after.
    let private wait
        (system : UnixSystem<int, string>)
        (kq : int)
        (room : int)
        (model : Model)
        : (int * KqueueFilter * ModelRegistration * (int64 option * bool * uint32)) list * Model
        =
        let state = model.Kqueues.[kq]

        let rec walk reported requeued remaining =
            match remaining with
            | _ when List.length reported = room -> List.rev reported, remaining @ List.rev requeued
            | [] -> List.rev reported, List.rev requeued
            | (fd, filter as key) :: rest ->
                let registration = state.Registrations.[key]

                match measuredReadiness filter (Option.get (socketOf fd system)) system with
                | None -> walk reported requeued rest
                | Some readiness ->
                    let reported = (fd, filter, registration, readiness) :: reported

                    if registration.Clear then
                        walk reported requeued rest
                    else
                        walk reported (key :: requeued) rest

        let reported, active = walk [] [] state.Active

        reported,
        { model with
            Kqueues =
                model.Kqueues
                |> Map.add
                    kq
                    { state with
                        Active = active
                    }
        }

    /// The model's events as the kernel reports them.
    let private asEvents
        (reported : (int * KqueueFilter * ModelRegistration * (int64 option * bool * uint32)) list)
        : KeventEvent list
        =
        reported
        |> List.map (fun (fd, filter, registration, (data, eof, errno)) ->
            {
                Ident = uint64 fd
                Filter =
                    match filter with
                    | KqueueFilter.Read -> -1s
                    | KqueueFilter.Write -> -2s
                Flags =
                    0x1us
                    ||| (if registration.Clear then 0x20us else 0us)
                    ||| (if registration.Receipt then 0x40us else 0us)
                    ||| (if eof then 0x8000us else 0us)
                FilterFlags = errno
                Data =
                    match data with
                    | Some data -> KqueueEventData.Exactly data
                    | None -> KqueueEventData.SendBufferSpace
                UserData = registration.UserData
            }
        )

    // ------------------------------------------------------------------
    // The operations
    // ------------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type private Op =
        /// A new non-blocking socket connecting to the listener.
        | Connect
        /// A new non-blocking socket connecting to a port nothing listens on.
        | ConnectRefused
        /// A new non-blocking socket, left idle, so that it can be registered before
        /// it connects.
        | Socket
        /// A connect of one of the idle sockets, to the listener or to a port nothing
        /// listens on.
        | ConnectIdle of pick : int * refused : bool
        /// An accept on the listener, when it holds a connection.
        | Accept
        /// A close of one of the open sockets other than the listener.
        | Close of pick : int
        | Dup of pick : int
        /// `getsockopt(SO_ERROR)` on one of the open sockets.
        | ReadError of pick : int
        /// A changelist to one of the two kqueues: each change picks a descriptor (one
        /// of the open sockets, or a closed number), a filter, one of the six supported
        /// flag combinations and a udata; and the room in the eventlist.
        | Change of second : bool * changes : (int * bool * int * int) list * room : int
        /// A wait with a zero timeout on one of the two kqueues, with this much room.
        | Wait of second : bool * room : int

    let private supportedFlags : uint16 list =
        [ 0x01us ; 0x21us ; 0x41us ; 0x61us ; 0x02us ; 0x42us ]

    let private opGen : Gen<Op> =
        let pick = Gen.choose (0, 1000)

        Gen.frequency
            [
                3, Gen.constant Op.Connect
                1, Gen.constant Op.ConnectRefused
                2, Gen.constant Op.Socket
                2, Gen.map2 (fun pick refused -> Op.ConnectIdle (pick, refused)) pick (Gen.elements [ false ; true ])
                2, Gen.constant Op.Accept
                2, Gen.map Op.Close pick
                1, Gen.map Op.Dup pick
                1, Gen.map Op.ReadError pick
                6,
                gen {
                    let! second = Gen.elements [ false ; true ]

                    let! changes =
                        Gen.listOf (
                            Gen.zip
                                (Gen.zip pick (Gen.elements [ false ; true ]))
                                (Gen.zip (Gen.choose (0, 5)) (Gen.choose (0, 3)))
                            |> Gen.map (fun ((fd, filter), (flags, udata)) -> fd, filter, flags, udata)
                        )
                        |> Gen.map (List.truncate 4)

                    let! room = Gen.choose (-1, 3)
                    return Op.Change (second, changes, room)
                }
                4,
                Gen.map2 (fun second room -> Op.Wait (second, room)) (Gen.elements [ false ; true ]) (Gen.choose (1, 4))
            ]

    /// The world: a listener on fd 3 at port 5000, and two kqueues on 4 and 5.
    let private world : UnixSystem<int, string> * int * int * int =
        let l, system = KeventWorld.listenerAt 5000us KeventWorld.darwin
        let kq1, system = KeventWorld.kqueue system
        let kq2, system = KeventWorld.kqueue system
        system, l, kq1, kq2

    /// The open socket descriptors other than the listener, in order.
    let private sockets (listener : int) (system : UnixSystem<int, string>) : int list =
        FileDescriptorRegistry.fds system.Process.FileDescriptors
        |> Map.toList
        |> List.map fst
        |> List.filter (fun fd -> fd <> listener && (socketOf fd system).IsSome)

    let private kqueueStates (kqs : int list) (system : UnixSystem<int, string>) : Map<int, ModelKqueue> =
        kqs
        |> List.map (fun kq ->
            let state = KeventWorld.stateOf kq system

            kq,
            {
                Registrations =
                    state.Registrations
                    |> Map.map (fun _ registration ->
                        {
                            Clear = registration.Clear
                            Receipt = registration.Receipt
                            UserData = registration.UserData
                            RegisteredAt = registration.RegisteredAt
                        }
                    )
                Active = state.Active
            }
        )
        |> Map.ofList

    /// Run `ops` through the kernel and the model, checking they agree after each.
    let private run (ops : Op list) : unit =
        let system, listener, kq1, kq2 = world

        let model =
            {
                Kqueues = kqueueStates [ kq1 ; kq2 ] system
                NextOrdinal = system.Machine.NextSocketEventRegistrationOrdinal
            }

        let listenerSocket = Option.get (socketOf listener system)

        let step (system : UnixSystem<int, string>, model : Model) (op : Op) : UnixSystem<int, string> * Model =
            let open' = sockets listener system

            let picked (pick : int) : int option =
                match open' with
                | [] -> None
                | fds -> Some (List.item (pick % List.length fds) fds)

            let system, model =
                let queued =
                    match (UnixMachineState.socket listenerSocket system.Machine).Phase with
                    | SocketPhase.Listening listenState -> List.length listenState.Queue
                    | phase -> failwith $"the listener left listening: %A{phase}"

                match op with
                // The listen backlog is 8; a connect past it is refused as unmodelled.
                | Op.Connect when queued >= 8 -> system, model
                | Op.Connect ->
                    let fd, after = KeventWorld.client 5000us system
                    let client = Option.get (socketOf fd after)

                    after,
                    model
                    |> activate after client [ KqueueFilter.Write ; KqueueFilter.Read ]
                    |> activate after listenerSocket [ KqueueFilter.Read ]
                | Op.ConnectRefused ->
                    let fd, after = KeventWorld.client 6000us system

                    after,
                    activate after (Option.get (socketOf fd after)) [ KqueueFilter.Write ; KqueueFilter.Read ] model
                | Op.Socket -> KeventWorld.stream true system |> snd, model
                | Op.ConnectIdle (pick, refused) ->
                    let idle =
                        open'
                        |> List.filter (fun fd ->
                            (UnixMachineState.socket (Option.get (socketOf fd system)) system.Machine).Phase = SocketPhase.Idle
                        )

                    match idle with
                    | [] -> system, model
                    | _ when not refused && queued >= 8 -> system, model
                    | idle ->
                        let fd = List.item (pick % List.length idle) idle
                        let socketId = Option.get (socketOf fd system)

                        let after =
                            KeventWorld.connect fd (if refused then 6000us else 5000us) system |> snd

                        let model = activate after socketId [ KqueueFilter.Write ; KqueueFilter.Read ] model

                        if refused then
                            after, model
                        else
                            after, activate after listenerSocket [ KqueueFilter.Read ] model
                | Op.Accept when queued > 0 -> KeventWorld.accept listener system |> snd, model
                | Op.Accept -> system, model
                | Op.Close pick ->
                    match picked pick with
                    | None -> system, model
                    | Some fd ->
                        let socketId = Option.get (socketOf fd system)

                        let last =
                            FileDescriptorRegistry.fds system.Process.FileDescriptors
                            |> Map.filter (fun other _ -> other <> fd && socketOf other system = Some socketId)
                            |> Map.isEmpty

                        let after = KeventWorld.close fd system
                        let model = sweep fd model

                        // The peer's FIN reaches the other end of an established
                        // connection when its last descriptor closes.
                        match (UnixMachineState.socket socketId system.Machine).Phase with
                        | SocketPhase.Established connection when last ->
                            let survivors =
                                after.Machine.Sockets
                                |> Map.filter (fun _ socket -> socket.Phase = SocketPhase.Established connection)
                                |> Map.toList
                                |> List.map fst

                            after,
                            (model, survivors)
                            ||> List.fold (fun model survivor -> activate after survivor [ KqueueFilter.Read ] model)
                        | _ -> after, model
                | Op.Dup pick ->
                    match picked pick with
                    | None -> system, model
                    | Some fd -> KeventWorld.dup fd system |> snd, model
                | Op.ReadError pick ->
                    match picked pick with
                    | None -> system, model
                    | Some fd -> KeventWorld.readSocketError fd system |> snd, model
                | Op.Change (second, changes, room) ->
                    let kq = if second then kq2 else kq1

                    let candidates = listener :: open'

                    let changes =
                        changes
                        |> List.map (fun (pick, isWrite, flags, udata) ->
                            let fd =
                                if pick % 5 = 0 then
                                    // A number nothing is open on.
                                    1000
                                else
                                    List.item (pick % List.length candidates) candidates

                            KeventWorld.change
                                fd
                                (if isWrite then KeventFilter.Write else KeventFilter.Read)
                                supportedFlags.[flags]
                                (uint64 udata)
                        )

                    let expected, model = applyChanges system kq changes (max room 0) model
                    let outcome, after = KeventWorld.apply kq changes room system

                    match expected, outcome with
                    | Error (error, []), KeventOutcome.Failed actual -> actual |> shouldEqual error
                    | Error (error, echoed), KeventOutcome.FailedAfterEchoing (actual, written) ->
                        (actual, written) |> shouldEqual (error, echoed)
                    | Ok [], KeventOutcome.Answered [] when room <= 0 -> ()
                    | Ok (_ :: _ as echoed), KeventOutcome.Echoed actual -> actual |> shouldEqual echoed
                    | Ok [], _ ->
                        // Nothing echoed, and room for events: the call goes on to
                        // report, as a wait does, which `Wait` checks. Here, the
                        // registrations it made are what is compared below.
                        ()
                    | expected, actual -> failwith $"%A{op}: the model answered %A{expected}, the kernel %A{actual}"

                    match expected, outcome with
                    | Ok [], KeventOutcome.Answered events when room > 0 ->
                        let reported, model = wait after kq room model
                        events |> shouldEqual (asEvents reported)
                        after, model
                    | _ -> after, model
                | Op.Wait (second, room) ->
                    let kq = if second then kq2 else kq1
                    let outcome, after = KeventWorld.apply kq [] room system
                    let reported, model = wait system kq room model

                    match outcome with
                    | KeventOutcome.Answered events -> events |> shouldEqual (asEvents reported)
                    | other -> failwith $"%A{op}: expected events, got %A{other}"

                    after, model

            kqueueStates [ kq1 ; kq2 ] system |> shouldEqual model.Kqueues

            system.Machine.NextSocketEventRegistrationOrdinal
            |> shouldEqual model.NextOrdinal

            UnixSystem.checkInvariants system |> shouldEqual []

            FileDescriptorRegistry.checkInvariants system.Process.FileDescriptors
            |> shouldEqual []

            system, model

        ((system, model), ops) ||> List.fold step |> ignore

    [<Test>]
    let ``registrations, activations and waits agree with the reference model`` () : unit =
        let ops = Gen.listOf opGen |> Gen.map (List.truncate 30)
        Check.One (config, Prop.forAll (Arb.fromGen ops) run)

    // ------------------------------------------------------------------
    // Refusals
    // ------------------------------------------------------------------

    let private ready () : UnixSystem<int, string> * int * int * int =
        // A listener holding a connection, its client, and a kqueue.
        let system, listener, kq, _ = world
        let client, system = KeventWorld.client 5000us system
        system, listener, client, kq

    let private refusalOf (kq : int) (changes : Kevent list) (system : UnixSystem<int, string>) : KeventRefusal =
        match
            UnixKqueue.kevent
                1
                kq
                (List.length changes)
                changes
                8
                UserBuffer.Mapped
                (KeventTimeout.Readable (0L, 0L))
                system
        with
        | Error refusal -> refusal
        | Ok answered -> failwith $"expected a refusal, got %A{answered}"

    [<Test>]
    let ``changes outside the modelled flags, filters and parameters are refused`` () : unit =
        let system, listener, _, kq = ready ()
        let read = KeventFilter.Read

        // Measured (R16): every one of these does something on Darwin, and none of it is
        // modelled.
        for flags in
            [
                KeventFlags.Add ||| 0x0010us
                KeventFlags.Add ||| 0x0080us
                KeventFlags.Add ||| 0x0008us
                0x0004us
                0x0008us
                0us
                KeventFlags.Add ||| KeventFlags.Delete
                KeventFlags.Delete ||| KeventFlags.Clear
                KeventFlags.Add ||| KeventFlags.Error
                KeventFlags.Add ||| KeventFlags.Eof
            ] do
            let change = KeventWorld.change listener read flags 6UL

            refusalOf kq [ change ] system
            |> shouldEqual (KeventRefusal.UnmodelledFlags change)

        for filter in [ 0s ; -3s ; -15s ; -100s ; 1s ] do
            let change =
                KeventWorld.change listener filter (KeventFlags.Add ||| KeventFlags.Clear) 6UL

            refusalOf kq [ change ] system
            |> shouldEqual (KeventRefusal.UnmodelledFilter change)

        for fflags, data in [ 1u, 2L ; 1u, 0L ; 0u, 100L ] do
            for flags in [ KeventFlags.Add ; KeventFlags.Delete ] do
                let change =
                    { KeventWorld.change listener read flags 6UL with
                        FilterFlags = fflags
                        Data = data
                    }

                refusalOf kq [ change ] system
                |> shouldEqual (KeventRefusal.UnmodelledFilterParameters change)

    [<Test>]
    let ``an ADD on anything but an IPv4 or IPv6 stream socket is refused, and a DELETE of it is ENOENT`` () : unit =
        let system, _, _, kq = ready ()

        let udp, system =
            NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

        let unixStream, system =
            NewSocket.create SocketDomain.Unix SocketKind.Stream SocketProtocol.Default system

        let file, system =
            let fd, registry =
                FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

            fd, KeventWorld.withRegistry registry system

        let targetOf (fd : int) =
            (FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors).Value

        for filter in [ KeventFilter.Read ; KeventFilter.Write ] do
            let add (fd : int) =
                KeventWorld.change fd filter (KeventFlags.Add ||| KeventFlags.Clear) 1UL

            // Measured (P9, F): Darwin registers on all of these.
            refusalOf kq [ add udp ] system
            |> shouldEqual (KeventRefusal.UnmodelledSocket (add udp, SocketDomain.Inet, SocketKind.Datagram))

            refusalOf kq [ add unixStream ] system
            |> shouldEqual (KeventRefusal.UnmodelledSocket (add unixStream, SocketDomain.Unix, SocketKind.Stream))

            for fd in [ 0 ; 1 ; file ; kq ] do
                refusalOf kq [ add fd ] system
                |> shouldEqual (KeventRefusal.UnmodelledTarget (add fd, targetOf fd))

            // Measured (X3): deleting what is not registered is ENOENT on every kind.
            for fd in [ 0 ; 1 ; file ; kq ; udp ; unixStream ] do
                KeventWorld.apply kq [ KeventWorld.change fd filter KeventFlags.Delete 1UL ] 0 system
                |> shouldEqual (KeventOutcome.Failed UnixError.ENOENT, system)

    [<Test>]
    let ``a refused change stops the call whatever came before it, and changes nothing`` () : unit =
        let system, listener, client, kq = ready ()

        let good =
            KeventWorld.change listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 1UL

        let bad = KeventWorld.change client -3s (KeventFlags.Add ||| KeventFlags.Clear) 1UL

        refusalOf kq [ good ; bad ] system
        |> shouldEqual (KeventRefusal.UnmodelledFilter bad)

    [<Test>]
    let ``copying to an eventlist that is not mapped is EFAULT for an echo and refused for events`` () : unit =
        let system, listener, _, kq = ready ()

        let call (changes : Kevent list) (eventlist : UserBuffer) =
            UnixKqueue.kevent 1 kq (List.length changes) changes 4 eventlist (KeventTimeout.Readable (0L, 0L)) system

        let receipt =
            KeventWorld.change
                listener
                KeventFilter.Read
                (KeventFlags.Add ||| KeventFlags.Clear ||| KeventFlags.Receipt)
                1UL

        let plain =
            KeventWorld.change listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 1UL

        let kqueueId = KeventWorld.idOf kq system

        // Measured (Y5): an echo into an eventlist that cannot be written is EFAULT,
        // its change applied. Events to such an eventlist are not measured beyond one.
        match call [ receipt ] (UserBuffer.Unmapped 8UL) with
        | Ok (KeventOutcome.Failed UnixError.EFAULT, after) ->
            (KeventWorld.stateOf kq after).Registrations
            |> Map.containsKey (listener, KqueueFilter.Read)
            |> shouldEqual true
        | other -> failwith $"expected EFAULT with the change applied, got %A{other}"

        call [ plain ] (UserBuffer.Unmapped 8UL)
        |> shouldEqual (Error (KeventRefusal.UnmeasuredCopyOutFault kqueueId))

        for changes in [ [ receipt ] ; [ plain ] ] do

            call changes UserBuffer.Opaque
            |> shouldEqual (Error (KeventRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

            call changes UserBuffer.Addressless
            |> shouldEqual (Error (KeventRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

        // With nothing to copy, the eventlist is never looked at.
        match
            call
                [
                    KeventWorld.change listener KeventFilter.Write (KeventFlags.Add ||| KeventFlags.Clear) 1UL
                ]
                (UserBuffer.Unmapped 8UL)
        with
        | Ok (KeventOutcome.Answered [], _) -> ()
        | other -> failwith $"expected no events and no copy, got %A{other}"

    // ------------------------------------------------------------------
    // A sleeping wait
    // ------------------------------------------------------------------

    let private parkIn
        (task : int)
        (kq : int)
        (timeout : KeventTimeout)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match UnixKqueue.kevent task kq 0 [] 4 UserBuffer.Mapped timeout system with
        | Ok (KeventOutcome.WouldBlock _, parked) -> parked
        | other -> failwith $"expected task %d{task} to park, got %A{other}"

    [<Test>]
    let ``an event wakes every sleeper, the first to finish takes it, and the rest sleep again`` () : unit =
        // Measured (Y2): an ADD of a ready filter from another thread wakes a sleeper.
        // And the waiters on one kqueue showed no fixed order (`signal-interrupt-requeue.c`,
        // section C), which is every one of them waking to race.
        for clear in [ true ; false ] do
            let system, listener, kq, _ = world
            let kqueueId = KeventWorld.idOf kq system

            let system =
                system |> parkIn 1 kq KeventTimeout.Null |> parkIn 2 kq KeventTimeout.Null

            UnixWait.wakes (Set.ofList [ 1 ; 2 ]) system |> shouldEqual []

            let flags =
                if clear then
                    KeventFlags.Add ||| KeventFlags.Clear
                else
                    KeventFlags.Add

            let system = KeventWorld.register kq listener KeventFilter.Read flags 9UL system

            // Registered, but nothing is ready: nobody wakes.
            UnixWait.wakes (Set.ofList [ 1 ; 2 ]) system |> shouldEqual []

            let _, system = KeventWorld.client 5000us system

            UnixWait.wakes (Set.ofList [ 1 ; 2 ]) system
            |> shouldEqual
                [
                    1, Set.singleton (WakePrimitive.KqueueEventDeliverable kqueueId)
                    2, Set.singleton (WakePrimitive.KqueueEventDeliverable kqueueId)
                ]

            let expected =
                {
                    Ident = uint64 listener
                    Filter = KeventFilter.Read
                    Flags = flags
                    FilterFlags = 0u
                    Data = KqueueEventData.Exactly 1L
                    UserData = 9UL
                }

            let system =
                match UnixKqueue.finishKevent 2 system with
                | Ok (KeventOutcome.Answered [ event ], finished) ->
                    event |> shouldEqual expected
                    UnixTaskTable.parkedFor 2 finished.Tasks |> shouldEqual None
                    finished
                | other -> failwith $"expected task 2 to take the event, got %A{other}"

            match UnixKqueue.finishKevent 1 system with
            | Ok (KeventOutcome.WouldBlock _, reparked) when clear ->
                // Taken: the other sleeper walks an empty queue and sleeps again.
                (KeventWorld.stateOf kq reparked).Active |> shouldEqual []
                UnixWait.wakes (Set.singleton 1) reparked |> shouldEqual []
            | Ok (KeventOutcome.Answered [ event ], _) when not clear ->
                // Without EV_CLEAR the registration stays queued while ready.
                event |> shouldEqual expected
            | other -> failwith $"unexpected finish for task 1 (EV_CLEAR %b{clear}): %A{other}"

            UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``a sleeper reports at most the nevents it asked for, and the rest stay queued`` () : unit =
        let system, listener, kq, _ = world

        let system =
            match UnixKqueue.kevent 1 kq 0 [] 1 UserBuffer.Mapped KeventTimeout.Null system with
            | Ok (KeventOutcome.WouldBlock _, parked) -> parked
            | other -> failwith $"expected a park, got %A{other}"

        let socket, system = KeventWorld.stream true system

        let system =
            KeventWorld.register kq socket KeventFilter.Write (KeventFlags.Add ||| KeventFlags.Clear) 2UL system

        let system =
            KeventWorld.register kq listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 1UL system

        let system = KeventWorld.connect socket 5000us system |> snd

        match UnixKqueue.finishKevent 1 system with
        | Ok (KeventOutcome.Answered [ event ], finished) ->
            event.Ident |> shouldEqual (uint64 socket)

            (KeventWorld.stateOf kq finished).Active
            |> shouldEqual [ listener, KqueueFilter.Read ]
        | other -> failwith $"expected one event, got %A{other}"

    [<Test>]
    let ``an event beside a drain, a deadline or a signal is refused`` () : unit =
        let system, listener, kq, _ = world
        let kqueueId = KeventWorld.idOf kq system
        let d, system = KeventWorld.dup kq system

        let system =
            KeventWorld.register kq listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 1UL system

        let parked = parkIn 1 kq (KeventTimeout.Readable (0L, 5L)) system
        let withEvent = KeventWorld.client 5000us parked |> snd

        // Darwin answers whichever reached the sleeper first.
        UnixKqueue.finishKevent 1 (KeventWorld.close kq withEvent)
        |> shouldEqual (Error (KeventRefusal.DrainBesideEvents kqueueId))

        let expired =
            { withEvent with
                Machine = UnixMachineState.advanceClock 5L withEvent.Machine
            }

        UnixKqueue.finishKevent 1 expired
        |> shouldEqual (Error (KeventRefusal.EventsBesideDeadline kqueueId))

        let signalled =
            { withEvent with
                Process =
                    { withEvent.Process with
                        Signals =
                            withEvent.Process.Signals
                            |> SignalState.setDisposition
                                Signal.SIGUSR1
                                (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
                            |> SignalState.enqueue
                                {
                                    Signal = Signal.SIGUSR1
                                    Target = ValueSome 1
                                }
                    }
            }

        match UnixKqueue.finishKevent 1 signalled with
        | Error (KeventRefusal.Interruption (SyscallInterruptionRefusal.SignalBesideCompletion SimulatedUnixFlavour.Darwin)) ->
            ()
        | other -> failwith $"expected the signal beside the event to be refused, got %A{other}"

        ignore<int> d

    [<Test>]
    let ``a wait on a drained kqueue is EBADF even with an event, while changes still apply`` () : unit =
        // Measured (Y1).
        let system, listener, kq, _ = world
        let d, system = KeventWorld.dup kq system

        let system =
            KeventWorld.register kq listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 1UL system

        let system = parkIn 1 kq KeventTimeout.Null system |> KeventWorld.close kq

        let system =
            match UnixKqueue.finishKevent 1 system with
            | Ok (KeventOutcome.Failed UnixError.EBADF, finished) -> finished
            | other -> failwith $"expected EBADF, got %A{other}"

        let system = KeventWorld.client 5000us system |> snd

        (KeventWorld.stateOf d system).Active
        |> shouldEqual [ listener, KqueueFilter.Read ]

        KeventWorld.apply d [] 4 system
        |> shouldEqual (KeventOutcome.Failed UnixError.EBADF, system)

        let receipt =
            KeventWorld.change listener KeventFilter.Write (KeventFlags.Add ||| KeventFlags.Receipt) 2UL

        match KeventWorld.apply d [ receipt ] 4 system with
        | KeventOutcome.Echoed [ entry ], after ->
            entry.Data |> shouldEqual 0L

            (KeventWorld.stateOf d after).Registrations
            |> Map.containsKey (listener, KqueueFilter.Write)
            |> shouldEqual true
        | other -> failwith $"expected the receipt, got %A{other}"

    [<Test>]
    let ``a kqueue drained by its last descriptor's close keeps its registrations until the call holding it returns``
        ()
        : unit
        =
        // The sleeper holds the kqueue, so the close drains it without destroying it; its
        // registrations, made through the listener's descriptor, go with it when the call
        // returns, not with the kqueue's descriptor.
        let system, listener, kq, _ = world
        let kqueueId = KeventWorld.idOf kq system

        let system =
            KeventWorld.register kq listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 1UL system

        let closed = parkIn 1 kq KeventTimeout.Null system |> KeventWorld.close kq
        UnixSystem.checkInvariants closed |> shouldEqual []

        match Map.tryFind kqueueId (FileDescriptorRegistry.descriptions closed.Process.FileDescriptors) with
        | Some {
                   Target = OpenFileTarget.Kqueue state
               } ->
            state.Drained |> shouldEqual true

            state.Registrations
            |> Map.containsKey (listener, KqueueFilter.Read)
            |> shouldEqual true
        | other -> failwith $"expected the held kqueue, got %A{other}"

        // An event can still reach the held kqueue, and then meets the drain.
        let withEvent = KeventWorld.client 5000us closed |> snd

        UnixKqueue.finishKevent 1 withEvent
        |> shouldEqual (Error (KeventRefusal.DrainBesideEvents kqueueId))

        let finished =
            match UnixKqueue.finishKevent 1 closed with
            | Ok (KeventOutcome.Failed UnixError.EBADF, finished) -> finished
            | other -> failwith $"expected EBADF, got %A{other}"

        FileDescriptorRegistry.descriptions finished.Process.FileDescriptors
        |> Map.containsKey kqueueId
        |> shouldEqual false

        UnixSystem.checkInvariants finished |> shouldEqual []

        // Nothing is left to activate.
        let afterEvent = KeventWorld.client 5000us finished |> snd
        UnixSystem.checkInvariants afterEvent |> shouldEqual []

    // ------------------------------------------------------------------
    // Invariants
    // ------------------------------------------------------------------

    [<Test>]
    let ``checkInvariants rejects a kqueue registration no call could have made`` () : unit =
        let system, listener, kq, _ = world
        let kqueueId = KeventWorld.idOf kq system

        let forged (state : KqueueState) =
            KeventWorld.withRegistry
                (FileDescriptorRegistry.setKqueueState kqueueId state system.Process.FileDescriptors)
                system
            |> UnixSystem.checkInvariants

        let registration : KqueueRegistration =
            {
                Clear = true
                Receipt = false
                UserData = 0UL
                RegisteredAt = 0L
            }

        let honest =
            {
                Drained = false
                Registrations = Map.ofList [ (listener, KqueueFilter.Read), registration ]
                Active = []
            }

        // Ordinal 0 was never minted in this system.
        forged honest
        |> shouldEqual [ UnixSystemDefect.SocketEventRegistrationOrdinalNotFresh (0L, kqueueId, 0L) ]

        let system =
            { system with
                Machine =
                    { system.Machine with
                        NextSocketEventRegistrationOrdinal = 1L
                    }
            }

        let forged (state : KqueueState) =
            KeventWorld.withRegistry
                (FileDescriptorRegistry.setKqueueState kqueueId state system.Process.FileDescriptors)
                system
            |> UnixSystem.checkInvariants

        forged honest |> shouldEqual []

        let defects (state : KqueueState) =
            FileDescriptorRegistry.setKqueueState kqueueId state system.Process.FileDescriptors
            |> FileDescriptorRegistry.checkInvariants

        defects honest |> shouldEqual []

        defects
            { honest with
                Registrations = Map.ofList [ (40, KqueueFilter.Read), registration ]
            }
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.KqueueRegistrationThroughClosedDescriptor (kqueueId, 40, KqueueFilter.Read)
            ]

        defects
            { honest with
                Registrations = Map.ofList [ (0, KqueueFilter.Read), registration ]
            }
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.KqueueRegistrationNotOnSocket (
                    kqueueId,
                    0,
                    KqueueFilter.Read,
                    (FileDescriptorRegistry.tryFindTarget 0 system.Process.FileDescriptors).Value
                )
            ]

        defects
            { honest with
                Active = [ listener, KqueueFilter.Write ]
            }
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.KqueueActiveEntryUnregistered (kqueueId, listener, KqueueFilter.Write)
            ]

        defects
            { honest with
                Active = [ listener, KqueueFilter.Read ; listener, KqueueFilter.Read ]
            }
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.KqueueActiveEntryDuplicated (kqueueId, listener, KqueueFilter.Read)
            ]
