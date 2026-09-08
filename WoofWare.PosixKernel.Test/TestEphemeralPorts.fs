namespace WoofWare.PosixKernel.Test

open FsCheck
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The ephemeral port that `bind(2)` with port 0, `listen(2)` on an unbound
/// socket, and `connect(2)` on an unbound socket each choose, driven through
/// those syscalls on one system so that the three choices are held to one
/// rule.
///
/// The occupant these tests care about is a TCP connection whose client
/// closed: the connection survives in the accepted socket, and a real kernel
/// keeps the closed client's port in its bind table until the connection is
/// gone. `bind(0)` and `listen` must therefore not hand that port out, while
/// `connect` towards a *different* listener may, because a kernel selects a
/// connect-time port by four-tuple.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEphemeralPorts =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private wildcard (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port

    /// A fresh system whose ephemeral range is `low..high`, so that the
    /// allocator wraps within a test.
    let private systemOn (platform : SimulatedUnixPlatform) (low : uint16, high : uint16) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        { system with
            Machine = UnixMachineState.withEphemeralPortRange (low, high) system.Machine
        }

    let private newStream (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        UnixSocket.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp system

    let private bindWithReuse
        (reuseAddress : bool)
        (fd : int)
        (endpoint : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : Result<BindAnswer, BindRefusal> * UnixSystem<int, string>
        =
        match UnixSocket.bind fd UserBuffer.Mapped 16 reuseAddress inetFamily (Some endpoint) system with
        | Ok (answer, system) -> Ok answer, system
        | Error refusal -> Error refusal, system

    let private bindTo
        (fd : int)
        (endpoint : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : Result<BindAnswer, BindRefusal> * UnixSystem<int, string>
        =
        bindWithReuse false fd endpoint system

    /// `bind(2)` to the wildcard address and port 0, answering the port the
    /// kernel chose.
    let private bindEphemeral
        (fd : int)
        (system : UnixSystem<int, string>)
        : Result<uint16, BindRefusal> * UnixSystem<int, string>
        =
        match bindTo fd (wildcard 0us) system with
        | Ok (BindAnswer.Bound endpoint), system -> Ok endpoint.Port, system
        | Ok (BindAnswer.Failed error), _ -> failwith $"bind(0.0.0.0:0) failed with %A{error}"
        | Error refusal, system -> Error refusal, system

    let private listenOn (fd : int) (system : UnixSystem<int, string>) : ListenAnswer * UnixSystem<int, string> =
        match UnixSocket.listen fd 8 system with
        | Ok (answer, system) -> answer, system
        | Error refusal -> failwith $"listen refused: %A{refusal}"

    /// A new socket bound to loopback at `port` and listening.
    let private listener (port : uint16) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = newStream system

        let system =
            match bindTo fd (loopback port) system with
            | Ok (BindAnswer.Bound _), system -> system
            | other, _ -> failwith $"binding the listener at port %d{port}: %A{other}"

        match listenOn fd system with
        | ListenAnswer.Listening _, system -> fd, system
        | ListenAnswer.Failed error, _ -> failwith $"listen at port %d{port} failed with %A{error}"

    let private connectTo
        (fd : int)
        (destination : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : ConnectOutcome * UnixSystem<int, string>
        =
        match UnixConnection.connect fd UserBuffer.Mapped 16 inetFamily (Some destination) system with
        | Ok result -> result
        | Error refusal -> failwith $"connect refused: %s{ConnectRefusal.describe refusal}"

    let private acceptFrom (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixConnection.accept fd UserBuffer.Mapped 16 system with
        | Ok (AcceptAnswer.Accepted (accepted, _, _), system) -> accepted, system
        | Ok (AcceptAnswer.Failed error, _) -> failwith $"accept failed with %A{error}"
        | Error refusal -> failwith $"accept refused: %A{refusal}"

    let private closeFd (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | Ok (answer, _) -> failwith $"close answered %A{answer}"
        | Error refusal -> failwith $"close refused: %A{refusal}"

    /// The local port the socket behind `fd` holds.
    let private localPort (fd : int) (system : UnixSystem<int, string>) : uint16 =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Binding with
            | Some binding -> binding.Endpoint.Port
            | None -> failwith $"fd %d{fd} is unbound"
        | other -> failwith $"fd %d{fd} is %A{other}, not a socket"

    let private noDefects (system : UnixSystem<int, string>) : unit =
        UnixSystem.checkInvariants system |> shouldEqual []

    /// A listener at 5000, a client that connected to it from the first
    /// ephemeral port and then closed, and the accepted socket still holding
    /// the connection. Answers the closed client's port and the system.
    let private closedClientAtFirstPort
        (platform : SimulatedUnixPlatform)
        (range : uint16 * uint16)
        : uint16 * UnixSystem<int, string>
        =
        let system = systemOn platform range
        let listenerFd, system = listener 5000us system
        let clientFd, system = newStream system

        let system =
            match connectTo clientFd (loopback 5000us) system with
            | ConnectOutcome.Completed, system -> system
            | ConnectOutcome.Failed error, _ -> failwith $"the first connect failed with %A{error}"

        let clientPort = localPort clientFd system
        clientPort |> shouldEqual (fst range)

        let _accepted, system = acceptFrom listenerFd system
        let system = closeFd clientFd system

        // The connection outlives its client.
        system.Machine.Connections
        |> Map.exists (fun _ connection -> connection.ClientAddress = loopback clientPort)
        |> shouldEqual true

        noDefects system
        clientPort, system

    /// `closedClientAtFirstPort`, with the allocator's cursor put back on the
    /// closed client's port so that it is the first port considered.
    let private cursorOnClosedClient
        (platform : SimulatedUnixPlatform)
        (range : uint16 * uint16)
        : uint16 * UnixSystem<int, string>
        =
        let occupied, system = closedClientAtFirstPort platform range

        occupied,
        { system with
            Machine =
                { system.Machine with
                    NextEphemeralPort = occupied
                }
        }

    [<TestCaseSource(nameof platforms)>]
    let ``bind(0) skips a port a closed client's connection still occupies`` (platform : SimulatedUnixPlatform) : unit =
        let occupied, system = cursorOnClosedClient platform (40000us, 40001us)

        let fd1, system = newStream system
        let port1, system = bindEphemeral fd1 system
        port1 |> shouldEqual (Ok 40001us)
        noDefects system

        // The sweep passed the occupied port and found nothing else.
        let fd2, system = newStream system
        let port2, system = bindEphemeral fd2 system

        port2
        |> shouldEqual (Error (BindRefusal.EphemeralPortsExhausted (40000us, 40001us)))

        // ...and the socket that did get a port can connect to the listener,
        // which is what crashed when the occupied port was handed out.
        match connectTo fd1 (loopback 5000us) system with
        | ConnectOutcome.Completed, system ->
            localPort fd1 system |> shouldNotEqual occupied
            noDefects system
        | ConnectOutcome.Failed error, _ -> failwith $"connect from the bound socket failed with %A{error}"

    [<TestCaseSource(nameof platforms)>]
    let ``listen on an unbound socket skips a port a closed client's connection still occupies``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let _, system = cursorOnClosedClient platform (40000us, 40001us)

        let fd, system = newStream system

        match listenOn fd system with
        | ListenAnswer.Listening endpoint, system ->
            endpoint.Port |> shouldEqual 40001us
            noDefects system
        | ListenAnswer.Failed error, _ -> failwith $"listen failed with %A{error}"

    /// The client's port is the one the cursor has come back round to, so
    /// the connect-time choice is between reusing it and skipping it.
    let private cursorBackAtClosedClient (platform : SimulatedUnixPlatform) : uint16 * int * UnixSystem<int, string> =
        let occupied, system = closedClientAtFirstPort platform (40000us, 40001us)
        let listener1, system = listener 5001us system

        // Takes 40001, wrapping the cursor to 40000.
        let filler, system = newStream system

        let system =
            match connectTo filler (loopback 5001us) system with
            | ConnectOutcome.Completed, system ->
                localPort filler system |> shouldEqual 40001us
                system
            | ConnectOutcome.Failed error, _ -> failwith $"the filler connect failed with %A{error}"

        let _, system = acceptFrom listener1 system
        let system = closeFd filler system
        system.Machine.NextEphemeralPort |> shouldEqual 40000us
        occupied, listener1, system

    [<TestCaseSource(nameof platforms)>]
    let ``connect reuses a closed client's port towards a different listener``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let occupied, _, system = cursorBackAtClosedClient platform

        // Towards the other listener the four-tuple is free, so the kernel's
        // connect-time selection reuses the port.
        let fd, system = newStream system

        match connectTo fd (loopback 5001us) system with
        | ConnectOutcome.Completed, system ->
            localPort fd system |> shouldEqual occupied
            noDefects system
        | ConnectOutcome.Failed error, _ -> failwith $"connect to the second listener failed with %A{error}"

    [<TestCaseSource(nameof platforms)>]
    let ``connect skips a closed client's port towards the same listener`` (platform : SimulatedUnixPlatform) : unit =
        let occupied, _, system = cursorBackAtClosedClient platform

        // 40000 is taken towards 5000; 40001 is taken only towards 5001.
        let fd, system = newStream system

        match connectTo fd (loopback 5000us) system with
        | ConnectOutcome.Completed, system ->
            localPort fd system |> shouldNotEqual occupied
            localPort fd system |> shouldEqual 40001us
            noDefects system
        | ConnectOutcome.Failed error, _ -> failwith $"connect to the first listener failed with %A{error}"

    /// A connection queued at a wildcard listener has a concrete server
    /// endpoint that no socket is bound at, and the listener owns it through
    /// its queue: it is not an orphan, so it takes nothing away from a bind
    /// the measured conflict rule admits beside the listener.
    [<Test>]
    let ``a queued connection's server endpoint belongs to the listener`` () : unit =
        // Darwin's measured rule: a specific reuse-address bind beside a
        // wildcard reuse-address listener on the same port is admitted, so
        // the only thing that could refuse the port is a misread of the queue.
        let system = systemOn SimulatedUnixPlatform.macOsArm64 (40000us, 40001us)

        let listenerFd, system = newStream system

        let system =
            match bindWithReuse true listenerFd (wildcard 0us) system with
            | Ok (BindAnswer.Bound endpoint), system ->
                endpoint.Port |> shouldEqual 40000us
                system
            | other, _ -> failwith $"binding the wildcard listener: %A{other}"

        let system =
            match listenOn listenerFd system with
            | ListenAnswer.Listening _, system -> system
            | ListenAnswer.Failed error, _ -> failwith $"listen failed with %A{error}"

        let clientFd, system = newStream system

        let system =
            match connectTo clientFd (loopback 40000us) system with
            | ConnectOutcome.Completed, system -> system
            | ConnectOutcome.Failed error, _ -> failwith $"connect failed with %A{error}"

        // The client took 40001; put the cursor back on the listener's port.
        localPort clientFd system |> shouldEqual 40001us

        let system =
            { system with
                Machine =
                    { system.Machine with
                        NextEphemeralPort = 40000us
                    }
            }

        let bindBesideListener (system : UnixSystem<int, string>) : uint16 =
            let fd, system = newStream system

            match bindWithReuse true fd (loopback 0us) system with
            | Ok (BindAnswer.Bound endpoint), _ -> endpoint.Port
            | other, _ -> failwith $"binding beside the listener: %A{other}"

        // Not yet accepted: the connection sits in the listener's queue.
        bindBesideListener system |> shouldEqual 40000us

        // Accepted: the accepted socket is bound at the server endpoint, and
        // the same answer follows from the measured established-socket row.
        let _, system = acceptFrom listenerFd system
        bindBesideListener system |> shouldEqual 40000us

    // ------------------------------------------------------------------
    // For all interleavings: the three choices never contradict each other
    // ------------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type private Op =
        /// A new stream socket, into the next free slot.
        | New
        /// `bind(0.0.0.0:0)` on an unbound slot.
        | BindEphemeral of slot : int
        /// `listen` on a slot; the implicit bind if it is unbound.
        | Listen of slot : int
        /// `connect` from a slot towards one of the two fixed listeners.
        | Connect of slot : int * listener : int
        /// `accept` on a fixed listener, when its queue is non-empty.
        | Accept of listener : int
        /// `close` a slot.
        | Close of slot : int

    type private Run =
        {
            System : UnixSystem<int, string>
            /// Slot to fd, for sockets this run created and has not closed.
            Slots : Map<int, int>
            NextSlot : int
            /// The fds of the two fixed listeners, at 5000 and 5001.
            Listeners : int list
        }

    let private listenerPort (listener : int) : uint16 = 5000us + uint16 listener

    /// Whether `slot`'s socket is in a state the op can lawfully be asked of:
    /// the generator is constructive, so an op that the model refuses
    /// outright (rather than answering an errno) is never generated.
    let private phaseOf (fd : int) (run : Run) : SocketPhase * bool =
        match FileDescriptorRegistry.tryFindTarget fd run.System.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) ->
            let socket = UnixMachineState.socket socketId run.System.Machine
            socket.Phase, socket.Binding.IsSome
        | other -> failwith $"slot fd %d{fd} is %A{other}"

    /// Whether `connect` from the unbound socket behind `fd` towards
    /// `destination` would find a port to take.
    let private implicitBindHasAPort (fd : int) (destination : InternetEndpoint) (run : Run) : bool =
        match FileDescriptorRegistry.tryFindTarget fd run.System.Process.FileDescriptors with
        | Some (OpenFileTarget.Socket socketId) ->
            let socket = UnixMachineState.socket socketId run.System.Machine

            let candidate (port : uint16) : SocketBinding =
                {
                    Endpoint = loopback port
                    LockedAddress = None
                    LockedPort = false
                }

            UnixMachineState.allocateEphemeralPort
                (EphemeralPortUse.ConnectTo destination)
                socketId
                socket
                candidate
                run.System.Machine
            |> Option.isSome
        | other -> failwith $"slot fd %d{fd} is %A{other}"

    let private queueLength (listenerFd : int) (run : Run) : int =
        match phaseOf listenerFd run with
        | SocketPhase.Listening state, _ -> List.length state.Queue
        | phase, _ -> failwith $"listener is in phase %A{phase}"

    /// One op against the run. Every answer the kernel gives is accepted;
    /// what the property rejects is a throw, an invariant defect, or an
    /// ephemeral port handed to `bind`/`listen` that a surviving connection
    /// endpoint still occupies.
    let private execute (op : Op) (run : Run) : Run =
        match op with
        | Op.New ->
            let fd, system = newStream run.System

            { run with
                System = system
                Slots = Map.add run.NextSlot fd run.Slots
                NextSlot = run.NextSlot + 1
            }
        | Op.BindEphemeral slot ->
            let fd = run.Slots.[slot]

            match bindEphemeral fd run.System with
            | Ok _, system
            | Error (BindRefusal.EphemeralPortsExhausted _), system ->
                { run with
                    System = system
                }
            | Error refusal, _ -> failwith $"bind refused: %A{refusal}"
        | Op.Listen slot ->
            let fd = run.Slots.[slot]

            match UnixSocket.listen fd 8 run.System with
            | Ok (_, system) ->
                { run with
                    System = system
                }
            | Error (ListenRefusal.EphemeralPortsExhausted _) -> run
            | Error refusal -> failwith $"listen refused: %A{refusal}"
        | Op.Connect (slot, listener) ->
            let fd = run.Slots.[slot]
            let _, system = connectTo fd (loopback (listenerPort listener)) run.System

            { run with
                System = system
            }
        | Op.Accept listener ->
            let _, system = acceptFrom run.Listeners.[listener] run.System

            { run with
                System = system
            }
        | Op.Close slot ->
            let fd = run.Slots.[slot]
            let system = closeFd fd run.System

            { run with
                System = system
                Slots = Map.remove slot run.Slots
            }

    /// The next op, chosen among those the model answers rather than refuses.
    let private nextOp (rng : System.Random) (run : Run) : Op =
        let slots = run.Slots |> Map.toList

        let candidates : Op list =
            [
                yield Op.New

                for slot, fd in slots do
                    match phaseOf fd run with
                    | SocketPhase.Idle, false ->
                        yield Op.BindEphemeral slot
                        yield Op.Listen slot
                    | SocketPhase.Idle, true -> yield Op.Listen slot
                    | _ -> ()

                    match phaseOf fd run with
                    | SocketPhase.Idle, bound ->
                        for listener in 0..1 do
                            // A listener whose queue is full has no measured
                            // answer, so keep every queue short of it; and an
                            // unbound connect with no port left to take is a
                            // refusal `connectSocket` still states as a throw,
                            // which is not what this property is about.
                            if
                                queueLength run.Listeners.[listener] run < 8
                                && (bound || implicitBindHasAPort fd (loopback (listenerPort listener)) run)
                            then
                                yield Op.Connect (slot, listener)
                    | _ -> ()

                    // A listener created by `Listen` may hold a queue only
                    // through the fixed listeners, which are never closed,
                    // so any slot may close; and any established peer's
                    // close is answered, never refused.
                    yield Op.Close slot

                for listener in 0..1 do
                    if queueLength run.Listeners.[listener] run > 0 then
                        yield Op.Accept listener
            ]

        candidates.[rng.Next candidates.Length]

    /// Every port `bind(0)` or an unbound `listen` handed out in this run,
    /// checked against the endpoints of every surviving connection whose
    /// socket has gone.
    let private orphanedEndpointPorts (system : UnixSystem<int, string>) : Set<uint16> =
        let boundEndpoints =
            system.Machine.Sockets
            |> Map.toSeq
            |> Seq.choose (fun (_, socket) -> socket.Binding |> Option.map (fun binding -> binding.Endpoint))
            |> Set.ofSeq

        system.Machine.Connections
        |> Map.toSeq
        |> Seq.collect (fun (_, connection) -> [ connection.ClientAddress ; connection.ServerAddress ])
        |> Seq.filter (fun endpoint -> not (Set.contains endpoint boundEndpoints))
        |> Seq.map (fun endpoint -> endpoint.Port)
        |> Set.ofSeq

    [<TestCaseSource(nameof platforms)>]
    let ``no interleaving of the three choices crashes, and no reserved port is an orphaned connection's``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let property (NonNegativeInt seed : NonNegativeInt) : bool =
            let rng = System.Random seed
            let system = systemOn platform (40000us, 40000us + uint16 (rng.Next 4))
            let listener0, system = listener (listenerPort 0) system
            let listener1, system = listener (listenerPort 1) system

            let initial =
                {
                    System = system
                    Slots = Map.empty
                    NextSlot = 0
                    Listeners = [ listener0 ; listener1 ]
                }

            let steps = 1 + rng.Next 24

            (initial, [ 1..steps ])
            ||> List.fold (fun run _ ->
                let op = nextOp rng run
                let run' = execute op run

                match UnixSystem.checkInvariants run'.System with
                | [] -> ()
                | defects -> failwith $"after %A{op}: %A{defects}"

                // Any bound socket's port that is also an orphaned
                // endpoint's must have been an explicit bind, which this
                // generator never makes: so any such overlap is a reserved
                // port that should have been skipped.
                let orphaned = orphanedEndpointPorts run'.System

                let reservedOverlap =
                    run'.System.Machine.Sockets
                    |> Map.toList
                    |> List.choose (fun (socketId, socket) ->
                        match socket.Binding, socket.Phase with
                        | Some binding, (SocketPhase.Idle | SocketPhase.Listening _) when
                            binding.Endpoint.Port <> listenerPort 0
                            && binding.Endpoint.Port <> listenerPort 1
                            && Set.contains binding.Endpoint.Port orphaned
                            ->
                            Some (socketId, binding.Endpoint)
                        | _ -> None
                    )

                match reservedOverlap with
                | [] -> ()
                | overlap -> failwith $"after %A{op}: reserved ports on orphaned endpoints: %A{overlap}"

                run'
            )
            |> ignore<Run>

            true

        Check.One (propertyConfig, property)
