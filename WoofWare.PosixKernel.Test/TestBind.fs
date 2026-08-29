namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.bind`, driven directly on a constructed system.
///
/// The screens it shares with `connect` are `TestConnect`'s; what is only here
/// is what `bind` adds — the fault *ordering*, which the two flavours disagree
/// about, the `SO_REUSEADDR` write that outlives every failure, and the
/// ephemeral allocation a request for port 0 performs.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBind =

    let private context : string = "TestBind"

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
            LocalAddresses = [ InternetEndpoint.LoopbackAddress ]
            LocalRoutes = [ Ipv4Prefix.create 0x7F000000u 8 ]
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

    /// The length of a well-formed `sockaddr_in`, which every row below declares
    /// unless it is testing the length itself.
    let private exactLength : int = 16

    let private inetFamily : int = SimulatedUnixPlatform.internetAddressFamily

    let private socketOfKind (kind : SocketKind) (phase : SocketPhase) : SocketDescription =
        {
            Domain = SocketDomain.InterNetwork
            Kind = kind
            Protocol =
                if kind = SocketKind.Stream then
                    SocketProtocol.Tcp
                else
                    SocketProtocol.Udp
            Binding = None
            ReuseAddress = false
            Phase = phase
        }

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

    /// A fresh unbound TCP socket, and the descriptor onto it.
    let private stream (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        withSocket (SocketId 0L) (socketOfKind SocketKind.Stream SocketPhase.Idle) (systemOn platform)

    let private bindOrFail
        (fd : int)
        (endpoint : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : BindAnswer * UnixSystem<int, string>
        =
        match UnixSystem.bind fd UserBuffer.Mapped exactLength false (Some inetFamily) (Some endpoint) system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{BindRefusal.describe refusal}"

    let private bound (fd : int) (endpoint : InternetEndpoint) (system : UnixSystem<int, string>) =
        match bindOrFail fd endpoint system with
        | BindAnswer.Bound bound, system -> bound, system
        | BindAnswer.Failed error, _ -> failwith $"expected the bind to succeed, got %O{error}"

    let private bindingOf (socketId : SocketId) (system : UnixSystem<int, string>) : SocketBinding option =
        (UnixMachineState.socket socketId system.Machine).Binding

    // ------------------------------------------------------------------
    // The happy path
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a bind of a local address takes it`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform
        let where, system = bound fd (loopback 5000us) system

        where |> shouldEqual (loopback 5000us)

        bindingOf (SocketId 0L) system
        |> shouldEqual (
            Some
                {
                    Endpoint = loopback 5000us
                    // `bind(2)`'s own address is locked, so a later Linux refusal
                    // delivery reverts a connect's source resolution to exactly it.
                    LockedAddress = Some InternetEndpoint.LoopbackAddress
                }
        )

    /// A request for port 0 asks for any free port, so the answer says where the
    /// socket actually landed rather than echoing the request.
    [<TestCaseSource(nameof platforms)>]
    let ``a bind of port zero allocates an ephemeral port`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform
        let where, system = bound fd (loopback 0us) system

        let low, high = system.Machine.EphemeralPortRange
        (where.Port >= low && where.Port <= high) |> shouldEqual true
        where.Address |> shouldEqual InternetEndpoint.LoopbackAddress

        // ...and the socket records where it landed, not the zero it asked for.
        match bindingOf (SocketId 0L) system with
        | Some binding -> binding.Endpoint |> shouldEqual where
        | None -> failwith "the socket is not bound"

    /// The wildcard is always bindable, whatever this machine's addresses are.
    [<TestCaseSource(nameof platforms)>]
    let ``the wildcard address is always bindable`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform

        bound fd (InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 5000us) system
        |> fst
        |> shouldEqual (InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 5000us)

    // ------------------------------------------------------------------
    // The individual faults
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``binding twice is EINVAL`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform
        let _, system = bound fd (loopback 5000us) system

        bindOrFail fd (loopback 5001us) system
        |> fst
        |> shouldEqual (BindAnswer.Failed UnixError.EINVAL)

    /// An address this machine does not hold. `127.0.0.9` is inside loopback's
    /// `/8` route but is not an assigned address, which is the row the two
    /// flavours read differently — Linux takes anything inside a local prefix,
    /// Darwin assigns loopback exactly one address.
    [<Test>]
    let ``an unassigned address inside a local prefix splits by flavour`` () : unit =
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, None
                SimulatedUnixPlatform.macOsArm64, Some UnixError.EADDRNOTAVAIL
            ]

        for platform, expected in rows do
            let fd, system = stream platform
            let inside = InternetEndpoint.ofParts 0x7F000009u 5000us

            match bindOrFail fd inside system |> fst, expected with
            | BindAnswer.Bound where, None -> where |> shouldEqual inside
            | BindAnswer.Failed error, Some expected -> error |> shouldEqual expected
            | answer, expected -> failwith $"platform %O{platform}: got %A{answer}, expected %A{expected}"

    /// A port below the ceiling needs root, and root is a property of the
    /// process rather than of the socket.
    [<TestCaseSource(nameof platforms)>]
    let ``a privileged port needs root`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform

        // 1023 is the last privileged port; 1024 is the first free one.
        bindOrFail fd (loopback 1023us) system
        |> fst
        |> shouldEqual (BindAnswer.Failed UnixError.EACCES)

        bindOrFail fd (loopback 1024us) system
        |> fst
        |> shouldEqual (BindAnswer.Bound (loopback 1024us))

        let asRoot =
            { system with
                Process =
                    { system.Process with
                        UserId = 0u
                    }
            }

        bindOrFail fd (loopback 1023us) asRoot
        |> fst
        |> shouldEqual (BindAnswer.Bound (loopback 1023us))

    /// Port 0 is not a privileged port: it is a request for an allocation rather
    /// than for a number, so the ceiling does not apply to it.
    [<TestCaseSource(nameof platforms)>]
    let ``port zero is not privileged`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform

        match bindOrFail fd (loopback 0us) system |> fst with
        | BindAnswer.Bound _ -> ()
        | BindAnswer.Failed error -> failwith $"expected an allocation, got %O{error}"

    /// Two sockets of the same kind cannot hold the same address; two of
    /// *different* kinds can, which is measured — the transports have separate
    /// port namespaces.
    [<TestCaseSource(nameof platforms)>]
    let ``a taken address is EADDRINUSE, and the namespaces are per-transport``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let first, system = stream platform
        let _, system = bound first (loopback 5000us) system

        let second, system =
            withSocket (SocketId 1L) (socketOfKind SocketKind.Stream SocketPhase.Idle) system

        bindOrFail second (loopback 5000us) system
        |> fst
        |> shouldEqual (BindAnswer.Failed UnixError.EADDRINUSE)

        let datagram, system =
            withSocket (SocketId 2L) (socketOfKind SocketKind.Datagram SocketPhase.Idle) system

        bindOrFail datagram (loopback 5000us) system
        |> fst
        |> shouldEqual (BindAnswer.Bound (loopback 5000us))

    /// A family that is neither `AF_INET` nor `AF_UNSPEC`.
    [<TestCaseSource(nameof platforms)>]
    let ``a foreign family is EAFNOSUPPORT`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform

        UnixSystem.bind fd UserBuffer.Mapped exactLength false (Some 99) (Some (loopback 5000us)) system
        |> shouldEqual (Ok (BindAnswer.Failed UnixError.EAFNOSUPPORT, system))

    /// `AF_UNSPEC` is two rules. Linux takes it only with an all-zero address;
    /// Darwin reads the address out of it and binds, exactly as for `AF_INET`.
    [<Test>]
    let ``AF_UNSPEC is two different rules`` () : unit =
        for platform in platforms do
            let fd, system = stream platform

            // A zero address: both take it.
            match
                UnixSystem.bind
                    fd
                    UserBuffer.Mapped
                    exactLength
                    false
                    (Some 0)
                    (Some (InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 5000us))
                    system
            with
            | Ok (BindAnswer.Bound _, _) -> ()
            | other -> failwith $"platform %O{platform}: expected a bind of the zero address, got %A{other}"

            // A non-zero one: Linux refuses, Darwin binds it.
            let answer =
                UnixSystem.bind fd UserBuffer.Mapped exactLength false (Some 0) (Some (loopback 5000us)) system

            match SimulatedUnixPlatform.flavour platform, answer with
            | SimulatedUnixFlavour.Linux, Ok (BindAnswer.Failed UnixError.EAFNOSUPPORT, _) -> ()
            | SimulatedUnixFlavour.Darwin, Ok (BindAnswer.Bound where, _) -> where |> shouldEqual (loopback 5000us)
            | flavour, other -> failwith $"flavour %O{flavour}: got %A{other}"

    // ------------------------------------------------------------------
    // The ordering, which is what `bind` adds over its parts
    // ------------------------------------------------------------------

    /// The two flavours rank the faults differently, and these are the two pairs
    /// that show it. The expected errnos are written out rather than asked of
    /// `firstBindFault`, which is the thing under test: a row that consulted it
    /// would agree with any order at all.
    ///
    /// Measured (`bindFaultOrder`): Linux ranks the address ahead of the
    /// privileged port ahead of already-bound; Darwin ranks already-bound ahead
    /// of the address ahead of the privileged port.
    [<Test>]
    let ``already-bound and privileged-port are ordered per flavour`` () : unit =
        let rows =
            [
                // Linux: PrivilegedPort comes first, so EACCES.
                SimulatedUnixPlatform.linuxX64, UnixError.EACCES
                // Darwin: AlreadyBound comes first, so EINVAL.
                SimulatedUnixPlatform.macOsArm64, UnixError.EINVAL
            ]

        for platform, expected in rows do
            let fd, system = stream platform
            let _, system = bound fd (loopback 5000us) system

            bindOrFail fd (loopback 1023us) system
            |> fst
            |> shouldEqual (BindAnswer.Failed expected)

    /// The other disagreeing pair, and the one that decides whether a multicast
    /// address is *refused* or answered: an already-bound socket asking for one.
    /// Linux ranks the address first and so reaches the refusal; Darwin ranks
    /// already-bound first and answers EINVAL without ever judging the address.
    ///
    /// This is what "refused late" buys — a gap in the model that a fault the
    /// platform ranks higher can hide.
    [<Test>]
    let ``a multicast address is refused only where the address is judged first`` () : unit =
        let multicast = 0xE0000001u

        let attempt (platform : SimulatedUnixPlatform) =
            let fd, system = stream platform
            let _, system = bound fd (loopback 5000us) system

            UnixSystem.bind
                fd
                UserBuffer.Mapped
                exactLength
                false
                (Some inetFamily)
                (Some (InternetEndpoint.ofParts multicast 5000us))
                system

        match attempt SimulatedUnixPlatform.linuxX64 with
        | Error (BindRefusal.UnmodelledMulticast (SocketId 0L, address)) -> address |> shouldEqual multicast
        | other -> failwith $"Linux: expected the refusal, got %A{other}"

        match attempt SimulatedUnixPlatform.macOsArm64 with
        | Ok (BindAnswer.Failed UnixError.EINVAL, _) -> ()
        | other -> failwith $"Darwin: expected EINVAL, got %A{other}"

    // ------------------------------------------------------------------
    // SO_REUSEADDR, which survives every failure
    // ------------------------------------------------------------------

    /// Measured: the option is set by a separate call that no failure of the
    /// bind undoes, so it is recorded above every answer — the address fault
    /// included.
    [<TestCaseSource(nameof platforms)>]
    let ``SO_REUSEADDR survives a failing bind`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform

        (UnixMachineState.socket (SocketId 0L) system.Machine).ReuseAddress
        |> shouldEqual false

        // A privileged port, which fails — and an unmapped buffer, which fails
        // earlier still, inside the shared admission.
        for destination, declaredLength in [ UserBuffer.Mapped, exactLength ; UserBuffer.Unmapped 4096UL, exactLength ] do
            let family, endpoint =
                match destination with
                | UserBuffer.Mapped -> Some inetFamily, Some (loopback 1023us)
                | _ -> None, None

            match UnixSystem.bind fd destination declaredLength true family endpoint system with
            | Ok (BindAnswer.Failed _, after) ->
                (UnixMachineState.socket (SocketId 0L) after.Machine).ReuseAddress
                |> shouldEqual true
            | other -> failwith $"expected a failure, got %A{other}"

    /// It is only set when the caller asks: a client with no such layer passes
    /// `false` and the flag is left alone.
    [<TestCaseSource(nameof platforms)>]
    let ``SO_REUSEADDR is not set unless asked for`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform
        let _, system = bound fd (loopback 5000us) system

        (UnixMachineState.socket (SocketId 0L) system.Machine).ReuseAddress
        |> shouldEqual false

    // ------------------------------------------------------------------
    // Refusals
    // ------------------------------------------------------------------

    /// Refused *late*: a fault the platform ranks ahead of the address is one
    /// this kernel does know the answer to, so it is reported instead.
    [<TestCaseSource(nameof platforms)>]
    let ``a multicast address is refused`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform
        let multicast = 0xE0000001u

        UnixSystem.bind
            fd
            UserBuffer.Mapped
            exactLength
            false
            (Some inetFamily)
            (Some (InternetEndpoint.ofParts multicast 5000us))
            system
        |> shouldEqual (Error (BindRefusal.UnmodelledMulticast (SocketId 0L, multicast)))

    /// The library refuses rather than inventing `EADDRINUSE` when a port-0 bind
    /// finds the whole range taken.
    [<Test>]
    let ``an exhausted ephemeral range is refused`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        // One port in the range, and a socket already holding it.
        let system =
            { system with
                Machine =
                    { system.Machine with
                        EphemeralPortRange = (40000us, 40000us)
                        NextEphemeralPort = 40000us
                    }
            }

        let holder, system =
            withSocket (SocketId 1L) (socketOfKind SocketKind.Stream SocketPhase.Idle) system

        let _, system = bound holder (loopback 40000us) system

        let fd, system =
            withSocket (SocketId 0L) (socketOfKind SocketKind.Stream SocketPhase.Idle) system

        UnixSystem.bind fd UserBuffer.Mapped exactLength false (Some inetFamily) (Some (loopback 0us)) system
        |> shouldEqual (Error (BindRefusal.EphemeralPortsExhausted (40000us, 40000us)))

    /// The field-consistency contract `connect` states, restated because `bind`
    /// is a second caller of it and a wrong set here is just as silent.
    [<TestCaseSource(nameof platforms)>]
    let ``supplying fields the admission did not ask for is a caller bug`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = stream platform

        let e =
            Assert.Throws<exn> (fun () ->
                UnixSystem.bind fd UserBuffer.Mapped exactLength false None None system
                |> ignore<_>
            )

        e.Message |> shouldContainText "have different measured answers"
