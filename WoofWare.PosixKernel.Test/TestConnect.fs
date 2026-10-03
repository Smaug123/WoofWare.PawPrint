namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSocket.admitSockaddrCopy` and `UnixConnection.connect`, driven directly on a
/// constructed system.
///
/// Two jobs. The first is the admission itself, which is new: which screens
/// precede the sockaddr copy, and which fields of it the copy reaches. The
/// second is a floor under `connectSocket`, which arrived in this library with
/// its exhaustive rows still in `WoofWare.PawPrint.Test/TestEmulatedKernelSockets.fs`
/// — a client that is not PawPrint had, until this fixture, no test of the
/// largest function in the library.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConnect =

    let private context : string = "TestConnect"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// `AF_INET`'s number, which the two platforms agree on -- so, unlike
    /// `AF_INET6`, it takes no platform argument.
    let private inetFamily (_platform : SimulatedUnixPlatform) : int =
        SimulatedUnixPlatform.internetAddressFamily

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it, on a machine with no local routes, booted from an image
    /// `configure` configured.
    let private systemOnWith
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        (platform : SimulatedUnixPlatform)
        : UnixSystem<int, string>
        =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.withLocalAddresses UnixSystem.defaultLocalAddresses []
        |> configure
        |> UnixBootImage.boot

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it, on a machine with no local routes.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> = systemOnWith id platform


    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

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

    let private streamSocket (binding : SocketBinding option) (phase : SocketPhase) : SocketDescription =
        {
            Domain = SocketDomain.Inet
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding = binding
            ReuseAddress = false
            Phase = phase
        }

    let private boundAt (endpoint : InternetEndpoint) : SocketBinding option =
        Some
            {
                Endpoint = endpoint
                LockedAddress = Some endpoint.Address
                LockedPort = true
            }

    /// A client socket bound to loopback, and the descriptor it is open on.
    let private client (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        withSocket (SocketId 0L) (streamSocket (boundAt (loopback 40000us)) SocketPhase.Idle) (systemOn platform)

    /// A client bound to loopback plus a listener at `port` with an empty queue.
    let private clientAndListener (platform : SimulatedUnixPlatform) (port : uint16) : int * UnixSystem<int, string> =
        let fd, system = client platform

        let listener =
            streamSocket
                (boundAt (loopback port))
                (SocketPhase.Listening
                    {
                        Backlog = 8
                        Queue = []
                        Drained = false
                    })

        let _, system = withSocket (SocketId 1L) listener system
        fd, system

    let private admitOrFail
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (system : UnixSystem<int, string>)
        : SockaddrCopyAdmission
        =
        match UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd destination declaredLength system with
        | Ok admission -> admission
        | Error refusal -> failwith $"expected an admission, got a refusal: %s{SockaddrCopyRefusal.describe refusal}"

    /// The full call, for a caller that has already read whatever the admission
    /// asked for out of a well-formed IPv4 sockaddr.
    let private connectTo
        (fd : int)
        (declaredLength : uint32)
        (destination : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : ConnectOutcome * UnixSystem<int, string>
        =
        let platform = system.Machine.UnixPlatform

        let family, endpoint =
            match admitOrFail fd UserBuffer.Mapped declaredLength system with
            | SockaddrCopyAdmission.Answered _ -> None, None
            | SockaddrCopyAdmission.Transfer (_, SockaddrCopyFields.Nothing) -> None, None
            | SockaddrCopyAdmission.Transfer (_, SockaddrCopyFields.Family) -> Some (inetFamily platform), None
            | SockaddrCopyAdmission.Transfer (_, SockaddrCopyFields.FamilyAndEndpoint) ->
                Some (inetFamily platform), Some destination

        match UnixConnection.connect fd UserBuffer.Mapped declaredLength family endpoint system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{ConnectRefusal.describe refusal}"

    // ------------------------------------------------------------------
    // The admission's screens
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not open is EBADF`` (platform : SimulatedUnixPlatform) : unit =
        admitOrFail 99 UserBuffer.Mapped 16u (systemOn platform)
        |> shouldEqual (SockaddrCopyAdmission.Answered UnixError.EBADF)

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not a socket is ENOTSOCK`` (platform : SimulatedUnixPlatform) : unit =
        let system = systemOn platform

        let fileFd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

        let portFd, registry = FileDescriptorRegistry.createEpoll registry

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        for fd in [ 0 ; fileFd ; portFd ] do
            admitOrFail fd UserBuffer.Mapped 16u system
            |> shouldEqual (SockaddrCopyAdmission.Answered UnixError.ENOTSOCK)

    /// A copy that succeeds reaches a socket in an unmodelled domain, which is
    /// refused: there is no destination to connect to. (Linux judges the length
    /// and the copy before the socket, so a length it rejects outright is
    /// answered there instead; `TestSocketAddressLength` holds that order.)
    [<TestCaseSource(nameof platforms)>]
    let ``a socket in an unmodelled domain is refused`` (platform : SimulatedUnixPlatform) : unit =
        for domain in [ SocketDomain.Inet6 ; SocketDomain.Unix ] do
            let socket =
                { streamSocket None SocketPhase.Idle with
                    Domain = domain
                }

            let fd, system = withSocket (SocketId 0L) socket (systemOn platform)

            UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd UserBuffer.Mapped 16u system
            |> shouldEqual (Error (SockaddrCopyRefusal.UnmodelledDomain (SocketId 0L, domain)))

    /// Measured: Linux takes 16 through 128 and answers EINVAL above, Darwin
    /// insists on exactly 16, answers EINVAL up to 255, and ENAMETOOLONG beyond.
    /// Only the outright rejections are answers *before the copy*; the rest reach
    /// the ladder, which has its own answer for a length it will not accept.
    [<Test>]
    let ``an over-long sockaddr is rejected before the copy`` () : unit =
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, 129u, UnixError.EINVAL
                SimulatedUnixPlatform.macOsArm64, 256u, UnixError.ENAMETOOLONG
            ]

        for platform, declaredLength, expected in rows do
            let fd, system = client platform

            admitOrFail fd UserBuffer.Mapped declaredLength system
            |> shouldEqual (SockaddrCopyAdmission.Answered expected)

    /// Under the length the *other* flavour rejects, each admits the copy — which
    /// is what stops the row above from passing for the wrong reason.
    [<Test>]
    let ``each flavour admits the length the other rejects`` () : unit =
        for platform, declaredLength in
            [
                SimulatedUnixPlatform.linuxX64, 128u
                SimulatedUnixPlatform.macOsArm64, 255u
            ] do
            let fd, system = client platform

            admitOrFail fd UserBuffer.Mapped declaredLength system
            |> shouldEqual (SockaddrCopyAdmission.Transfer (int declaredLength, SockaddrCopyFields.FamilyAndEndpoint))

    // ------------------------------------------------------------------
    // Which fields the copy reaches
    // ------------------------------------------------------------------

    /// Linux's `move_addr_to_kernel` copies at any positive length; Darwin's
    /// `getsockaddr` reads nothing at a length that does not reach `sa_family`.
    /// So the two disagree about whether the caller's buffer is touched at all
    /// for a length of 1, and that is the whole reason the admission exists.
    [<Test>]
    let ``the copy's extent is the flavour's, not the length's`` () : unit =
        let rows =
            [
                // (platform, declaredLength, expected admission)
                SimulatedUnixPlatform.linuxX64, 0u, SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.linuxX64, 1u, SockaddrCopyAdmission.Transfer (1, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.linuxX64, 2u, SockaddrCopyAdmission.Transfer (2, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.linuxX64, 7u, SockaddrCopyAdmission.Transfer (7, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.linuxX64,
                8u,
                SockaddrCopyAdmission.Transfer (8, SockaddrCopyFields.FamilyAndEndpoint)
                SimulatedUnixPlatform.linuxX64,
                16u,
                SockaddrCopyAdmission.Transfer (16, SockaddrCopyFields.FamilyAndEndpoint)

                // Darwin's family is one byte at offset 1, so a length of 2
                // reaches it and a length of 1 does not — and at 1 the kernel
                // reads nothing at all.
                SimulatedUnixPlatform.macOsArm64, 0u, SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.macOsArm64, 1u, SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.macOsArm64, 2u, SockaddrCopyAdmission.Transfer (2, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.macOsArm64, 7u, SockaddrCopyAdmission.Transfer (7, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.macOsArm64,
                8u,
                SockaddrCopyAdmission.Transfer (8, SockaddrCopyFields.FamilyAndEndpoint)
                SimulatedUnixPlatform.macOsArm64,
                16u,
                SockaddrCopyAdmission.Transfer (16, SockaddrCopyFields.FamilyAndEndpoint)
            ]

        for platform, declaredLength, expected in rows do
            let fd, system = client platform
            admitOrFail fd UserBuffer.Mapped declaredLength system |> shouldEqual expected

    /// A call whose copy takes no bytes never looks at the buffer, so every
    /// classification succeeds there — including the two a copy refuses.
    [<Test>]
    let ``a copy of no bytes admits any buffer`` () : unit =
        let buffers =
            [
                UserBuffer.Mapped
                UserBuffer.Unmapped 4096UL
                UserBuffer.Opaque
                UserBuffer.Addressless
            ]

        // Length 0 on both, and Darwin's length 1, which reaches no family.
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, 0u
                SimulatedUnixPlatform.macOsArm64, 0u
                SimulatedUnixPlatform.macOsArm64, 1u
            ]

        for platform, declaredLength in rows do
            for destination in buffers do
                let fd, system = client platform

                admitOrFail fd destination declaredLength system
                |> shouldEqual (SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing))

    /// An unmapped buffer is an ordinary EFAULT once the copy happens: unlike
    /// `accept`'s copy-*out*, nothing has been consumed by the time it faults.
    [<TestCaseSource(nameof platforms)>]
    let ``an unmapped buffer the copy reaches is EFAULT`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = client platform

        admitOrFail fd (UserBuffer.Unmapped 4096UL) 16u system
        |> shouldEqual (SockaddrCopyAdmission.Answered UnixError.EFAULT)

    /// The two classifications a client cannot represent are refusals, not
    /// EFAULT: the memory really is there and a real kernel really would copy it.
    [<TestCaseSource(nameof platforms)>]
    let ``a buffer the client cannot represent is refused`` (platform : SimulatedUnixPlatform) : unit =
        let rows =
            [
                UserBuffer.Opaque, BufferRefusal.OpaqueAtTransfer
                UserBuffer.Addressless, BufferRefusal.AddresslessAtTransfer
            ]

        for destination, expected in rows do
            let fd, system = client platform

            UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd destination 16u system
            |> shouldEqual (Error (SockaddrCopyRefusal.Buffer expected))

    /// The length verdict precedes the buffer: a length the copy helper rejects
    /// outright answers its own errno rather than EFAULT, whatever the pointer.
    [<Test>]
    let ``the length verdict outranks the buffer`` () : unit =
        for platform, declaredLength, expected in
            [
                SimulatedUnixPlatform.linuxX64, 129u, UnixError.EINVAL
                SimulatedUnixPlatform.macOsArm64, 256u, UnixError.ENAMETOOLONG
            ] do
            let fd, system = client platform

            admitOrFail fd (UserBuffer.Unmapped 4096UL) declaredLength system
            |> shouldEqual (SockaddrCopyAdmission.Answered expected)

    // ------------------------------------------------------------------
    // `connect` against what the admission asked for
    // ------------------------------------------------------------------

    /// A field this kernel could not read and a field the caller did not read
    /// have different measured answers, so `connect` refuses to be handed the
    /// wrong set rather than silently answering for the wrong one.
    [<TestCaseSource(nameof platforms)>]
    let ``supplying fields the admission did not ask for is a caller bug`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = client platform

        // The copy reaches both fields at 16, so every other combination is wrong.
        let wrong =
            [ None, None ; Some (inetFamily platform), None ; None, Some (loopback 5000us) ]

        for family, endpoint in wrong do
            let e =
                Assert.Throws<exn> (fun () ->
                    UnixConnection.connect fd UserBuffer.Mapped 16u family endpoint system
                    |> ignore<_>
                )

            e.Message |> shouldContainText "have different measured answers"

    /// ...and the admission's own answers come back through `connect` unchanged,
    /// so a caller that never asked is not punished for it.
    [<TestCaseSource(nameof platforms)>]
    let ``connect repeats the admission's answers`` (platform : SimulatedUnixPlatform) : unit =
        UnixConnection.connect 99 UserBuffer.Mapped 16u None None (systemOn platform)
        |> shouldEqual (Ok (ConnectOutcome.Failed UnixError.EBADF, systemOn platform))

    // ------------------------------------------------------------------
    // A floor under the ladder itself
    // ------------------------------------------------------------------

    /// The happy path, end to end through the entry point: a blocking connect to
    /// a listening loopback socket completes and queues the connection.
    [<TestCaseSource(nameof platforms)>]
    let ``a blocking connect to a listener completes`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = clientAndListener platform 5000us

        let outcome, system = connectTo fd 16u (loopback 5000us) system
        outcome |> shouldEqual ConnectOutcome.Completed

        match (UnixMachineState.socket (SocketId 0L) system.Machine).Phase with
        | SocketPhase.Established _ -> ()
        | other -> failwith $"expected Established, got %A{other}"

        match (UnixMachineState.socket (SocketId 1L) system.Machine).Phase with
        | SocketPhase.Listening listenState -> listenState.Queue |> List.length |> shouldEqual 1
        | other -> failwith $"expected Listening, got %A{other}"

    /// Measured on both kernels, even on loopback: a non-blocking connect answers
    /// EINPROGRESS and latches the completion on the phase.
    [<TestCaseSource(nameof platforms)>]
    let ``a non-blocking connect answers EINPROGRESS`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = clientAndListener platform 5000us

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = FileDescriptorRegistry.setNonBlocking fd true system.Process.FileDescriptors
                    }
            }

        let outcome, system = connectTo fd 16u (loopback 5000us) system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)

        // The connection is made whatever the syscall answered.
        match (UnixMachineState.socket (SocketId 1L) system.Machine).Phase with
        | SocketPhase.Listening listenState -> listenState.Queue |> List.length |> shouldEqual 1
        | other -> failwith $"expected Listening, got %A{other}"

    /// A second connect on an established socket, which is the one row the two
    /// flavours reach by different routes and answer the same.
    [<TestCaseSource(nameof platforms)>]
    let ``connecting an established socket is EISCONN`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = clientAndListener platform 5000us
        let _, system = connectTo fd 16u (loopback 5000us) system

        connectTo fd 16u (loopback 5000us) system
        |> fst
        |> shouldEqual (ConnectOutcome.Failed UnixError.EISCONN)

    /// Nothing is listening, so the SYN is refused. Blocking, so the refusal is
    /// delivered inline rather than latched.
    [<TestCaseSource(nameof platforms)>]
    let ``a blocking connect to a closed port is ECONNREFUSED`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = client platform

        connectTo fd 16u (loopback 5000us) system
        |> fst
        |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)

    // ------------------------------------------------------------------
    // What this kernel refuses, by type
    // ------------------------------------------------------------------

    /// The refusal `connect` answers, for the rows about what this kernel
    /// will not model. Each is a case a client can match on rather than a
    /// message it would have to read.
    let private refusedBy
        (fd : int)
        (family : int option)
        (destination : InternetEndpoint option)
        (system : UnixSystem<int, string>)
        : ConnectRefusal
        =
        match UnixConnection.connect fd UserBuffer.Mapped 16u family destination system with
        | Error refusal -> refusal
        | Ok answer -> failwith $"expected a refusal, got %A{answer}"

    [<TestCaseSource(nameof platforms)>]
    let ``a seqpacket socket's connect is refused as unmeasured`` (platform : SimulatedUnixPlatform) : unit =
        let seqPacket =
            { streamSocket None SocketPhase.Idle with
                Kind = SocketKind.SeqPacket
            }

        let fd, system = withSocket (SocketId 0L) seqPacket (systemOn platform)

        refusedBy fd (Some (inetFamily platform)) (Some (loopback 5000us)) system
        |> shouldEqual (ConnectRefusal.UnmeasuredKind (SocketId 0L, SocketKind.SeqPacket))

    [<TestCaseSource(nameof platforms)>]
    let ``an unbound connect to a local address other than 127.0.0.1 is refused``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        // 127.0.0.2 is local on both flavours' loopback route, but the source
        // a kernel picks for it is unmeasured.
        let fd, system =
            withSocket (SocketId 0L) (streamSocket None SocketPhase.Idle) (systemOn platform)

        let system =
            { system with
                Machine =
                    { system.Machine with
                        LocalRoutes = UnixSystem.defaultLocalRoutes
                    }
            }

        let other = InternetEndpoint.ofParts 0x7F000002u 5000us

        refusedBy fd (Some (inetFamily platform)) (Some other) system
        |> shouldEqual (ConnectRefusal.SourceForNonLoopbackDestination (SocketId 0L, other, false))

    [<TestCaseSource(nameof platforms)>]
    let ``an implicit bind with no port left is refused, naming the range`` (platform : SimulatedUnixPlatform) : unit =
        // One port in the range, and a live socket already holding it.
        let holder =
            streamSocket (boundAt (InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 40000us)) SocketPhase.Idle

        let _, system =
            withSocket
                (SocketId 0L)
                holder
                (systemOnWith (UnixBootImage.withEphemeralPortRange (40000us, 40000us)) platform)

        let fd, system =
            withSocket (SocketId 1L) (streamSocket None SocketPhase.Idle) system

        refusedBy fd (Some (inetFamily platform)) (Some (loopback 5000us)) system
        |> shouldEqual (ConnectRefusal.EphemeralPortsExhausted (40000us, 40000us))

    /// The sockaddr copy's refusals come through `connect` under `Copy`, so a
    /// client matching on one type sees both kinds of refusal.
    [<TestCaseSource(nameof platforms)>]
    let ``a copy refusal is carried as a connect refusal`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = client platform

        match UnixConnection.connect fd UserBuffer.Addressless 16u None None system with
        | Error (ConnectRefusal.Copy _) -> ()
        | other -> failwith $"expected a copy refusal, got %A{other}"
