namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.accept`, driven directly on a constructed system.
///
/// The guest tier already exercises the answers a managed program can reach
/// (`sourcesPure/SocketAccept.cs`, and the errno numbers in
/// `sourcesImpure/SocketAccept{Linux,Darwin}.cs`). What it cannot reach is the
/// other half: a guest runs one flavour, cannot hold a socket of a kind this
/// kernel refuses, and cannot ask for an accept through a buffer whose bytes
/// nobody can produce. Every refusal, and both flavours, are reachable only from
/// here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAccept =

    let private context : string = "TestAccept"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private machineOn (platform : SimulatedUnixPlatform) : UnixMachineState =
        {
            Sockets = Map.empty
            Connections = Map.empty
            NextConnectionId = ConnectionId 0L
            NextSocketEventRegistrationOrdinal = 0L
            NextEphemeralPort = 32768us
            EphemeralPortRange = (32768us, 60999us)
            SoMaxConn = 4096
            LocalAddresses = []
            LocalRoutes = []
            NextSocketId = SocketId 0L
            VirtualClockTicks = 0L
            WallClockEpochMs = 0L
            NonCryptoRandomState = 1UL
            CryptoRandomState = 1UL
            ProcessorCount = 1
            UserAddressLimit = 0x7FFFFFFFFFFFUL
            UnixPlatform = platform
            FileSystem = VirtualFileSystem.empty epoch
            FileSystemType = EmulatedFileSystemType.Tmpfs
        }

    let private processState : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.initial
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = Map.empty
            CurrentDirectory = AbsoluteUnixPath.parseOrFail context "/"
            CurrentDirectoryInode = InodeNumber 1L
            ProcessPath = None
            DirectoryStreams = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            UserId = 1000u
            GroupId = 1000u
            Umask = PermissionBits.parseOrFail context 0o022
            Signals = SignalState.empty
        }

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        {
            Machine = machineOn platform
            Process = processState
            Tasks = Map.empty
        }

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// The server address every listener below is bound at.
    let private serverAddress : InternetEndpoint = loopback 5000us

    /// Put `socket` in the table and open a descriptor onto it.
    ///
    /// `NextSocketId` is advanced past the id, which is not decoration: an
    /// accept mints its socket at `NextSocketId`, so a fixture that left it
    /// behind would have the accepted socket overwrite the listener in the
    /// table -- and the resulting failure names the listener's phase rather
    /// than anything about ids.
    let private withSocket
        (socketId : SocketId)
        (socket : SocketDescription)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        let (SocketId raw) = socketId
        let (SocketId next) = system.Machine.NextSocketId

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (max next (raw + 1L))
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// A stream socket in the IPv4 domain, in `phase`.
    let private streamSocket (phase : SocketPhase) : SocketDescription =
        {
            Domain = SocketDomain.InterNetwork
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding =
                Some
                    {
                        Endpoint = serverAddress
                        LockedAddress = None
                    }
            ReuseAddress = false
            Phase = phase
        }

    /// Enter `count` completed connections into the machine's connection table,
    /// oldest first, each from a distinct client port. Answers their ids in that
    /// order.
    let private withConnections
        (count : int)
        (system : UnixSystem<int, string>)
        : ConnectionId list * UnixSystem<int, string>
        =
        let ids, connections, next =
            List.fold
                (fun (ids, connections, ConnectionId next) index ->
                    let id = ConnectionId next

                    let connection =
                        {
                            ClientAddress = loopback (40000us + uint16 index)
                            ServerAddress = serverAddress
                        }

                    ids @ [ id ], Map.add id connection connections, ConnectionId (next + 1L)
                )
                ([], system.Machine.Connections, system.Machine.NextConnectionId)
                [ 0 .. count - 1 ]

        ids,
        { system with
            Machine =
                { system.Machine with
                    Connections = connections
                    NextConnectionId = next
                }
        }

    /// A listener holding `queued` completed connections, and the descriptor it
    /// is open on. `SocketId 0L` throughout, so tests can name it.
    let private listenerWith
        (platform : SimulatedUnixPlatform)
        (queued : int)
        : int * ConnectionId list * UnixSystem<int, string>
        =
        let connections, system = withConnections queued (systemOn platform)

        let listener =
            streamSocket (
                SocketPhase.Listening
                    {
                        Backlog = 8
                        Queue = connections
                    }
            )

        let fd, system = withSocket (SocketId 0L) listener system
        fd, connections, system

    let private acceptOrFail
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (system : UnixSystem<int, string>)
        : AcceptAnswer * UnixSystem<int, string>
        =
        match UnixSystem.accept fd destination declaredLength system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{AcceptRefusal.describe refusal}"

    let private queueOf (socketId : SocketId) (system : UnixSystem<int, string>) : ConnectionId list =
        match (UnixMachineState.socket socketId system.Machine).Phase with
        | SocketPhase.Listening listenState -> listenState.Queue
        | other -> failwith $"expected Listening, got %A{other}"

    // ------------------------------------------------------------------
    // The success path
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``an accept dequeues the oldest connection and reports its client address``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let fd, connections, system = listenerWith platform 2

        match acceptOrFail fd UserBuffer.Mapped 16 system with
        | AcceptAnswer.Failed error, _ -> failwith $"expected an accept, got %O{error}"
        | AcceptAnswer.Accepted (acceptedFd, peer, reportedLength), system ->

        peer |> shouldEqual (loopback 40000us)
        reportedLength |> shouldEqual 16

        // The head is gone and the tail is not.
        queueOf (SocketId 0L) system |> shouldEqual [ List.item 1 connections ]

        let acceptedId =
            match FileDescriptorRegistry.tryFindTarget acceptedFd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"expected a socket target, got %A{other}"

        let accepted = UnixMachineState.socket acceptedId system.Machine
        accepted.Phase |> shouldEqual (SocketPhase.Established (List.head connections))
        accepted.Kind |> shouldEqual SocketKind.Stream

        // Bound at the *server* address, which is what the accepted socket's own
        // `getsockname` reports.
        accepted.Binding
        |> shouldEqual (
            Some
                {
                    Endpoint = serverAddress
                    LockedAddress = None
                }
        )

    [<TestCaseSource(nameof platforms)>]
    let ``the accepted socket inherits the listener's SO_REUSEADDR`` (platform : SimulatedUnixPlatform) : unit =
        let connections, system = withConnections 1 (systemOn platform)

        let listener =
            { streamSocket (
                  SocketPhase.Listening
                      {
                          Backlog = 8
                          Queue = connections
                      }
              ) with
                ReuseAddress = true
            }

        let fd, system = withSocket (SocketId 0L) listener system

        match acceptOrFail fd UserBuffer.Mapped 16 system with
        | AcceptAnswer.Failed error, _ -> failwith $"expected an accept, got %O{error}"
        | AcceptAnswer.Accepted (acceptedFd, _, _), system ->

        let acceptedId =
            match FileDescriptorRegistry.tryFindTarget acceptedFd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"expected a socket target, got %A{other}"

        (UnixMachineState.socket acceptedId system.Machine).ReuseAddress
        |> shouldEqual true

    [<TestCaseSource(nameof platforms)>]
    let ``accepts come out in the order the connects completed`` (platform : SimulatedUnixPlatform) : unit =
        let fd, connections, system = listenerWith platform 3

        let peers, system =
            List.fold
                (fun (peers, system) (_ : int) ->
                    match acceptOrFail fd UserBuffer.Mapped 16 system with
                    | AcceptAnswer.Failed error, _ -> failwith $"expected an accept, got %O{error}"
                    | AcceptAnswer.Accepted (_, peer, _), system -> peers @ [ peer ], system
                )
                ([], system)
                [ 0..2 ]

        peers |> shouldEqual [ loopback 40000us ; loopback 40001us ; loopback 40002us ]
        queueOf (SocketId 0L) system |> shouldEqual []
        connections |> List.length |> shouldEqual 3

    /// The measurement `getsockname` recorded, asked again here because
    /// `accept(2)` shares the kernel's one sockaddr copy-out helper: the caller's
    /// declared length bounds what a client *writes* and bounds nothing that is
    /// reported.
    [<TestCaseSource(nameof platforms)>]
    let ``the declared length does not bound what is reported`` (platform : SimulatedUnixPlatform) : unit =
        for declaredLength in [ 1 ; 8 ; 16 ; 17 ; 128 ; 4096 ] do
            let fd, _, system = listenerWith platform 1

            match acceptOrFail fd UserBuffer.Mapped declaredLength system with
            | AcceptAnswer.Failed error, _ -> failwith $"expected an accept at %d{declaredLength}, got %O{error}"
            | AcceptAnswer.Accepted (_, _, reportedLength), _ -> reportedLength |> shouldEqual 16

    /// A call that writes nothing never looks at the destination, so every
    /// buffer succeeds at a declared length of zero — including the three that a
    /// nonzero length refuses.
    [<TestCaseSource(nameof platforms)>]
    let ``a declared length of zero accepts through any buffer`` (platform : SimulatedUnixPlatform) : unit =
        for destination in
            [
                UserBuffer.Mapped
                UserBuffer.Unmapped 4096UL
                UserBuffer.Opaque
                UserBuffer.Addressless
            ] do
            let fd, _, system = listenerWith platform 1

            match acceptOrFail fd destination 0 system with
            | AcceptAnswer.Failed error, _ -> failwith $"expected an accept through %A{destination}, got %O{error}"
            | AcceptAnswer.Accepted (_, _, reportedLength), system ->

            reportedLength |> shouldEqual 16
            queueOf (SocketId 0L) system |> shouldEqual []

    // ------------------------------------------------------------------
    // Errnos
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not open is EBADF`` (platform : SimulatedUnixPlatform) : unit =
        let _, _, system = listenerWith platform 1

        acceptOrFail 99 UserBuffer.Mapped 16 system
        |> fst
        |> shouldEqual (AcceptAnswer.Failed UnixError.EBADF)

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not a socket is ENOTSOCK`` (platform : SimulatedUnixPlatform) : unit =
        let system = systemOn platform

        // A standard stream, a regular file and a socket event port: the three
        // things a descriptor can name that are not sockets.
        let fileFd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

        let portFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        for fd in [ 0 ; fileFd ; portFd ] do
            acceptOrFail fd UserBuffer.Mapped 16 system
            |> fst
            |> shouldEqual (AcceptAnswer.Failed UnixError.ENOTSOCK)

    /// The kind check beats the listening check: a datagram socket is also "not
    /// listening", and answers EOPNOTSUPP rather than EINVAL — blocking or not.
    [<TestCaseSource(nameof platforms)>]
    let ``a datagram socket is EOPNOTSUPP`` (platform : SimulatedUnixPlatform) : unit =
        for nonBlocking in [ false ; true ] do
            let socket =
                { streamSocket SocketPhase.Idle with
                    Kind = SocketKind.Datagram
                    Protocol = SocketProtocol.Udp
                }

            let fd, system = withSocket (SocketId 0L) socket (systemOn platform)

            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors =
                                FileDescriptorRegistry.setNonBlocking fd nonBlocking system.Process.FileDescriptors
                        }
                }

            acceptOrFail fd UserBuffer.Mapped 16 system
            |> fst
            |> shouldEqual (AcceptAnswer.Failed UnixError.EOPNOTSUPP)

    /// ...and the listening check beats blocking behaviour: a *blocking* stream
    /// socket that is not listening answers EINVAL immediately rather than
    /// parking.
    [<TestCaseSource(nameof platforms)>]
    let ``a stream socket that is not listening is EINVAL`` (platform : SimulatedUnixPlatform) : unit =
        let phases =
            [
                SocketPhase.Idle
                SocketPhase.EstablishedPendingReport (ConnectionId 7L)
                SocketPhase.Established (ConnectionId 7L)
                SocketPhase.RefusedPendingDelivery
                SocketPhase.Dead
            ]

        for phase in phases do
            let fd, system = withSocket (SocketId 0L) (streamSocket phase) (systemOn platform)

            acceptOrFail fd UserBuffer.Mapped 16 system
            |> fst
            |> shouldEqual (AcceptAnswer.Failed UnixError.EINVAL)

    [<TestCaseSource(nameof platforms)>]
    let ``an empty queue on a non-blocking listener is EAGAIN`` (platform : SimulatedUnixPlatform) : unit =
        let fd, _, system = listenerWith platform 0

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = FileDescriptorRegistry.setNonBlocking fd true system.Process.FileDescriptors
                    }
            }

        acceptOrFail fd UserBuffer.Mapped 16 system
        |> fst
        |> shouldEqual (AcceptAnswer.Failed UnixError.EAGAIN)

    /// `O_NONBLOCK` is a fact about the open file description rather than about
    /// the socket, so an accept through a `dup` of a non-blocking listener
    /// answers EAGAIN too — and the original descriptor, naming the same
    /// description, is not a second opinion.
    [<TestCaseSource(nameof platforms)>]
    let ``non-blocking follows the description, not the descriptor`` (platform : SimulatedUnixPlatform) : unit =
        let fd, _, system = listenerWith platform 0

        let registry =
            FileDescriptorRegistry.setNonBlocking fd true system.Process.FileDescriptors

        let duplicate, registry =
            match FileDescriptorRegistry.dup fd registry with
            | Ok result -> result
            | Error error -> failwith $"could not dup: %A{error}"

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        acceptOrFail duplicate UserBuffer.Mapped 16 system
        |> fst
        |> shouldEqual (AcceptAnswer.Failed UnixError.EAGAIN)

    /// Every failing arm hands back the system it was given, which is what makes
    /// the queue survivable: measured on both flavours, a failed `accept` leaves
    /// a queued connection queued.
    [<TestCaseSource(nameof platforms)>]
    let ``a failed accept changes nothing`` (platform : SimulatedUnixPlatform) : unit =
        let _, _, system = listenerWith platform 1

        let _, after = acceptOrFail 99 UserBuffer.Mapped 16 system
        after |> shouldEqual system

    // ------------------------------------------------------------------
    // Refusals
    // ------------------------------------------------------------------

    /// The socket here is `Idle`, so the domain screen is shown to precede the
    /// phase screen: a socket that is *also* not listening is refused rather
    /// than answered EINVAL, because there would be no peer address to report
    /// even if the accept itself succeeded.
    [<TestCaseSource(nameof platforms)>]
    let ``a socket in an unmodelled domain is refused`` (platform : SimulatedUnixPlatform) : unit =
        for domain in [ SocketDomain.InterNetworkV6 ; SocketDomain.Unix ] do
            let socket =
                { streamSocket SocketPhase.Idle with
                    Domain = domain
                }

            let fd, system = withSocket (SocketId 0L) socket (systemOn platform)

            UnixSystem.accept fd UserBuffer.Mapped 16 system
            |> shouldEqual (Error (AcceptRefusal.UnmodelledDomain (SocketId 0L, domain)))

    [<TestCaseSource(nameof platforms)>]
    let ``a socket of an unmeasured kind is refused`` (platform : SimulatedUnixPlatform) : unit =
        for kind in [ SocketKind.Raw ; SocketKind.SeqPacket ] do
            let socket =
                { streamSocket SocketPhase.Idle with
                    Kind = kind
                }

            let fd, system = withSocket (SocketId 0L) socket (systemOn platform)

            UnixSystem.accept fd UserBuffer.Mapped 16 system
            |> shouldEqual (Error (AcceptRefusal.UnmeasuredKind (SocketId 0L, kind)))

    [<TestCaseSource(nameof platforms)>]
    let ``an empty queue on a blocking listener is refused`` (platform : SimulatedUnixPlatform) : unit =
        let fd, _, system = listenerWith platform 0

        UnixSystem.accept fd UserBuffer.Mapped 16 system
        |> shouldEqual (Error (AcceptRefusal.WouldPark (SocketId 0L)))

    /// An unmapped destination faults the copy-out, and the fault happens once a
    /// connection has already been taken off the queue -- which is the case
    /// `getsockname` answers EFAULT for and this one cannot.
    [<TestCaseSource(nameof platforms)>]
    let ``a copy-out through an unmapped destination is refused`` (platform : SimulatedUnixPlatform) : unit =
        let fd, _, system = listenerWith platform 1

        UnixSystem.accept fd (UserBuffer.Unmapped 4096UL) 16 system
        |> shouldEqual (Error (AcceptRefusal.UnmeasuredCopyOutFault (SocketId 0L)))

    /// A destination whose bytes the client cannot produce is a different
    /// refusal, and deliberately so: the kernel would have succeeded here, and
    /// it is the client that has run out of representation. Calling that a
    /// copy-out fault would tell a caller the kernel faulted when it did not.
    [<TestCaseSource(nameof platforms)>]
    let ``a copy-out through a buffer the client cannot represent is refused``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let rows =
            [
                UserBuffer.Opaque, BufferRefusal.OpaqueAtTransfer
                UserBuffer.Addressless, BufferRefusal.AddresslessAtTransfer
            ]

        for destination, expected in rows do
            let fd, _, system = listenerWith platform 1

            UnixSystem.accept fd destination 16 system
            |> shouldEqual (Error (AcceptRefusal.Buffer expected))

    /// A refusal carries no system, so the connection it would have handed over
    /// is still queued. That is what makes the refusal safe to raise from: the
    /// caller has lost nothing by asking.
    [<TestCaseSource(nameof platforms)>]
    let ``a refused copy-out leaves the connection queued`` (platform : SimulatedUnixPlatform) : unit =
        let fd, connections, system = listenerWith platform 1

        UnixSystem.accept fd UserBuffer.Opaque 16 system
        |> shouldEqual (Error (AcceptRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        queueOf (SocketId 0L) system |> shouldEqual connections

    /// The buffer is screened only once a connection is there to hand over: a
    /// buffer naming no storage is not what an accept with an empty queue
    /// complains about.
    [<TestCaseSource(nameof platforms)>]
    let ``an empty queue outranks an unwritable buffer`` (platform : SimulatedUnixPlatform) : unit =
        let fd, _, system = listenerWith platform 0

        UnixSystem.accept fd UserBuffer.Opaque 16 system
        |> shouldEqual (Error (AcceptRefusal.WouldPark (SocketId 0L)))


    // ------------------------------------------------------------------
    // O_NONBLOCK inheritance
    // ------------------------------------------------------------------

    let private acceptedDescription (acceptedFd : int) (system : UnixSystem<int, string>) : OpenFileDescription =
        match FileDescriptorRegistry.tryFind acceptedFd system.Process.FileDescriptors with
        | Some description -> description
        | None -> failwith $"the accepted descriptor %d{acceptedFd} is not live"

    /// The measured answers, as literals: Linux 6.18.5 hands back a *blocking*
    /// socket through a non-blocking listener, Darwin 25.6.0 a non-blocking one.
    /// Taken 2026-08-28 with
    /// `docs/plans/2026-08-23-posix-kernel-extraction/accept-inherits-nonblock.c`.
    ///
    /// Written out rather than asked of
    /// `SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking`, which is the
    /// function under test: a test that consults it would move with it and could
    /// not see it flipped.
    let private inheritanceRows : obj[] list =
        [
            [| box SimulatedUnixPlatform.linuxX64 ; box false |]
            [| box SimulatedUnixPlatform.macOsArm64 ; box true |]
        ]

    /// A guest cannot see this at all -- CoreCLR's `SystemNative_Accept` clears
    /// the flag on the flavours that set it, precisely so that its own socket
    /// code does not have to care -- so this fixture is the only thing that
    /// distinguishes the two answers. (`sourcesImpure/SocketAcceptDarwin.cs`
    /// covers the *clearing*, which is a different claim.)
    [<TestCaseSource(nameof inheritanceRows)>]
    let ``a non-blocking listener's accepted socket inherits the flavour's answer``
        (platform : SimulatedUnixPlatform)
        (inherits : bool)
        : unit
        =
        let fd, _, system = listenerWith platform 1

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = FileDescriptorRegistry.setNonBlocking fd true system.Process.FileDescriptors
                    }
            }

        match acceptOrFail fd UserBuffer.Mapped 16 system with
        | AcceptAnswer.Failed error, _ -> failwith $"expected an accept, got %O{error}"
        | AcceptAnswer.Accepted (acceptedFd, _, _), system ->

        (acceptedDescription acceptedFd system).NonBlocking |> shouldEqual inherits

    /// The other half of the same measurement, and the one that keeps the rule
    /// from being "Darwin's accepted sockets are non-blocking": a blocking
    /// listener yields a blocking accepted socket on both flavours, so what is
    /// inherited is the flag rather than the platform.
    [<TestCaseSource(nameof platforms)>]
    let ``a blocking listener's accepted socket is blocking`` (platform : SimulatedUnixPlatform) : unit =
        let fd, _, system = listenerWith platform 1

        match acceptOrFail fd UserBuffer.Mapped 16 system with
        | AcceptAnswer.Failed error, _ -> failwith $"expected an accept, got %O{error}"
        | AcceptAnswer.Accepted (acceptedFd, _, _), system ->

        (acceptedDescription acceptedFd system).NonBlocking |> shouldEqual false

    /// Inherited from the *description*, not from the descriptor: an accept
    /// through a `dup` of a non-blocking listener inherits it too, because both
    /// numbers name the same description.
    [<TestCaseSource(nameof inheritanceRows)>]
    let ``inheritance follows the description through a dup``
        (platform : SimulatedUnixPlatform)
        (inherits : bool)
        : unit
        =
        let fd, _, system = listenerWith platform 1

        let registry =
            FileDescriptorRegistry.setNonBlocking fd true system.Process.FileDescriptors

        let duplicate, registry =
            match FileDescriptorRegistry.dup fd registry with
            | Ok result -> result
            | Error error -> failwith $"could not dup: %A{error}"

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        match acceptOrFail duplicate UserBuffer.Mapped 16 system with
        | AcceptAnswer.Failed error, _ -> failwith $"expected an accept, got %O{error}"
        | AcceptAnswer.Accepted (acceptedFd, _, _), system ->

        (acceptedDescription acceptedFd system).NonBlocking |> shouldEqual inherits

    /// `acceptConnection` is the state transition without the entry point, and a
    /// `SocketId` names no description -- so it cannot inherit anything, and
    /// says so. This pins that, because the alternative (scanning the registry
    /// for a description naming the socket) would be a plausible-looking bug.
    [<Test>]
    let ``acceptConnection alone does not inherit`` () : unit =
        let fd, _, system = listenerWith SimulatedUnixPlatform.macOsArm64 1

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = FileDescriptorRegistry.setNonBlocking fd true system.Process.FileDescriptors
                    }
            }

        let acceptedFd, _, system = UnixSystem.acceptConnection (SocketId 0L) system

        (acceptedDescription acceptedFd system).NonBlocking |> shouldEqual false

    // ------------------------------------------------------------------
    // Caller bugs
    // ------------------------------------------------------------------

    [<Test>]
    let ``a negative declared length is a caller bug`` () : unit =
        let fd, _, system = listenerWith SimulatedUnixPlatform.linuxX64 1

        let e =
            Assert.Throws<exn> (fun () -> UnixSystem.accept fd UserBuffer.Mapped -1 system |> ignore<_>)

        e.Message |> shouldContainText "is negative, which no kernel is ever asked"

    [<Test>]
    let ``a stream socket holding a datagram peer is a caller bug`` () : unit =
        let socket = streamSocket (SocketPhase.DatagramPeer (loopback 9000us))

        let fd, system =
            withSocket (SocketId 0L) socket (systemOn SimulatedUnixPlatform.linuxX64)

        let e =
            Assert.Throws<exn> (fun () -> UnixSystem.accept fd UserBuffer.Mapped 16 system |> ignore<_>)

        e.Message |> shouldContainText "socket invariants forbid"

    [<Test>]
    let ``acceptConnection refuses an empty queue`` () : unit =
        let _, _, system = listenerWith SimulatedUnixPlatform.linuxX64 0

        let e =
            Assert.Throws<exn> (fun () -> UnixSystem.acceptConnection (SocketId 0L) system |> ignore<_>)

        e.Message |> shouldContainText "the accept queue is empty"

    [<Test>]
    let ``acceptConnection refuses a socket that is not listening`` () : unit =
        let _, system =
            withSocket (SocketId 0L) (streamSocket SocketPhase.Idle) (systemOn SimulatedUnixPlatform.linuxX64)

        let e =
            Assert.Throws<exn> (fun () -> UnixSystem.acceptConnection (SocketId 0L) system |> ignore<_>)

        e.Message |> shouldContainText "not listening"
