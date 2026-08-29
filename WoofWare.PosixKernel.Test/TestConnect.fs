namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.admitSockaddrCopy` and `UnixSystem.connect`, driven directly on a
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
    /// happened to it.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }


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
            Domain = SocketDomain.InterNetwork
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
                    })

        let _, system = withSocket (SocketId 1L) listener system
        fd, system

    let private admitOrFail
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (system : UnixSystem<int, string>)
        : SockaddrCopyAdmission
        =
        match UnixSystem.admitSockaddrCopy fd destination declaredLength system with
        | Ok admission -> admission
        | Error refusal -> failwith $"expected an admission, got a refusal: %s{SockaddrCopyRefusal.describe refusal}"

    /// The full call, for a caller that has already read whatever the admission
    /// asked for out of a well-formed IPv4 sockaddr.
    let private connectTo
        (fd : int)
        (declaredLength : int)
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

        match UnixSystem.connect fd UserBuffer.Mapped declaredLength family endpoint system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{SockaddrCopyRefusal.describe refusal}"

    // ------------------------------------------------------------------
    // The admission's screens
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not open is EBADF`` (platform : SimulatedUnixPlatform) : unit =
        admitOrFail 99 UserBuffer.Mapped 16 (systemOn platform)
        |> shouldEqual (SockaddrCopyAdmission.Answered UnixError.EBADF)

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not a socket is ENOTSOCK`` (platform : SimulatedUnixPlatform) : unit =
        let system = systemOn platform

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
            admitOrFail fd UserBuffer.Mapped 16 system
            |> shouldEqual (SockaddrCopyAdmission.Answered UnixError.ENOTSOCK)

    /// The domain screen precedes the length verdict: an unmodelled-domain socket
    /// is refused even at a length that would have been rejected outright,
    /// because there would be no destination to connect to either way.
    [<TestCaseSource(nameof platforms)>]
    let ``a socket in an unmodelled domain is refused`` (platform : SimulatedUnixPlatform) : unit =
        for domain in [ SocketDomain.InterNetworkV6 ; SocketDomain.Unix ] do
            let socket =
                { streamSocket None SocketPhase.Idle with
                    Domain = domain
                }

            let fd, system = withSocket (SocketId 0L) socket (systemOn platform)

            UnixSystem.admitSockaddrCopy fd UserBuffer.Mapped 4096 system
            |> shouldEqual (Error (SockaddrCopyRefusal.UnmodelledDomain (SocketId 0L, domain)))

    /// Measured: Linux takes 16 through 128 and answers EINVAL above, Darwin
    /// insists on exactly 16, answers EINVAL up to 255, and ENAMETOOLONG beyond.
    /// Only the outright rejections are answers *before the copy*; the rest reach
    /// the ladder, which has its own answer for a length it will not accept.
    [<Test>]
    let ``an over-long sockaddr is rejected before the copy`` () : unit =
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, 129, UnixError.EINVAL
                SimulatedUnixPlatform.macOsArm64, 256, UnixError.ENAMETOOLONG
            ]

        for platform, declaredLength, expected in rows do
            let fd, system = client platform

            admitOrFail fd UserBuffer.Mapped declaredLength system
            |> shouldEqual (SockaddrCopyAdmission.Answered expected)

    /// Under the length the *other* flavour rejects, each admits the copy — which
    /// is what stops the row above from passing for the wrong reason.
    [<Test>]
    let ``each flavour admits the length the other rejects`` () : unit =
        for platform, declaredLength in [ SimulatedUnixPlatform.linuxX64, 128 ; SimulatedUnixPlatform.macOsArm64, 255 ] do
            let fd, system = client platform

            admitOrFail fd UserBuffer.Mapped declaredLength system
            |> shouldEqual (SockaddrCopyAdmission.Transfer (declaredLength, SockaddrCopyFields.FamilyAndEndpoint))

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
                SimulatedUnixPlatform.linuxX64, 0, SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.linuxX64, 1, SockaddrCopyAdmission.Transfer (1, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.linuxX64, 2, SockaddrCopyAdmission.Transfer (2, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.linuxX64, 7, SockaddrCopyAdmission.Transfer (7, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.linuxX64,
                8,
                SockaddrCopyAdmission.Transfer (8, SockaddrCopyFields.FamilyAndEndpoint)
                SimulatedUnixPlatform.linuxX64,
                16,
                SockaddrCopyAdmission.Transfer (16, SockaddrCopyFields.FamilyAndEndpoint)

                // Darwin's family is one byte at offset 1, so a length of 2
                // reaches it and a length of 1 does not — and at 1 the kernel
                // reads nothing at all.
                SimulatedUnixPlatform.macOsArm64, 0, SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.macOsArm64, 1, SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing)
                SimulatedUnixPlatform.macOsArm64, 2, SockaddrCopyAdmission.Transfer (2, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.macOsArm64, 7, SockaddrCopyAdmission.Transfer (7, SockaddrCopyFields.Family)
                SimulatedUnixPlatform.macOsArm64,
                8,
                SockaddrCopyAdmission.Transfer (8, SockaddrCopyFields.FamilyAndEndpoint)
                SimulatedUnixPlatform.macOsArm64,
                16,
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
                SimulatedUnixPlatform.linuxX64, 0
                SimulatedUnixPlatform.macOsArm64, 0
                SimulatedUnixPlatform.macOsArm64, 1
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

        admitOrFail fd (UserBuffer.Unmapped 4096UL) 16 system
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

            UnixSystem.admitSockaddrCopy fd destination 16 system
            |> shouldEqual (Error (SockaddrCopyRefusal.Buffer expected))

    /// The length verdict precedes the buffer: a length the copy helper rejects
    /// outright answers its own errno rather than EFAULT, whatever the pointer.
    [<Test>]
    let ``the length verdict outranks the buffer`` () : unit =
        for platform, declaredLength, expected in
            [
                SimulatedUnixPlatform.linuxX64, 129, UnixError.EINVAL
                SimulatedUnixPlatform.macOsArm64, 256, UnixError.ENAMETOOLONG
            ] do
            let fd, system = client platform

            admitOrFail fd (UserBuffer.Unmapped 4096UL) declaredLength system
            |> shouldEqual (SockaddrCopyAdmission.Answered expected)

    [<TestCaseSource(nameof platforms)>]
    let ``a negative declared length is a caller bug`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = client platform

        let e =
            Assert.Throws<exn> (fun () -> UnixSystem.admitSockaddrCopy fd UserBuffer.Mapped -1 system |> ignore<_>)

        e.Message |> shouldContainText "is negative, which no kernel is ever asked"

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
                    UnixSystem.connect fd UserBuffer.Mapped 16 family endpoint system |> ignore<_>
                )

            e.Message |> shouldContainText "have different measured answers"

    /// ...and the admission's own answers come back through `connect` unchanged,
    /// so a caller that never asked is not punished for it.
    [<TestCaseSource(nameof platforms)>]
    let ``connect repeats the admission's answers`` (platform : SimulatedUnixPlatform) : unit =
        UnixSystem.connect 99 UserBuffer.Mapped 16 None None (systemOn platform)
        |> shouldEqual (Ok (ConnectOutcome.Failed UnixError.EBADF, systemOn platform))

    // ------------------------------------------------------------------
    // A floor under the ladder itself
    // ------------------------------------------------------------------

    /// The happy path, end to end through the entry point: a blocking connect to
    /// a listening loopback socket completes and queues the connection.
    [<TestCaseSource(nameof platforms)>]
    let ``a blocking connect to a listener completes`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = clientAndListener platform 5000us

        let outcome, system = connectTo fd 16 (loopback 5000us) system
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

        let outcome, system = connectTo fd 16 (loopback 5000us) system
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
        let _, system = connectTo fd 16 (loopback 5000us) system

        connectTo fd 16 (loopback 5000us) system
        |> fst
        |> shouldEqual (ConnectOutcome.Failed UnixError.EISCONN)

    /// Nothing is listening, so the SYN is refused. Blocking, so the refusal is
    /// delivered inline rather than latched.
    [<TestCaseSource(nameof platforms)>]
    let ``a blocking connect to a closed port is ECONNREFUSED`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = client platform

        connectTo fd 16 (loopback 5000us) system
        |> fst
        |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)
