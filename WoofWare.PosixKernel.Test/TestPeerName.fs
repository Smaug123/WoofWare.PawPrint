namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `getpeername(2)`: which phases have a peer to report, what a socket with
/// none answers, and the length and destination rules it shares with
/// `getsockname(2)`.
///
/// The rows are literals of the measurements, per flavour: Linux 6.18.5 (arm64,
/// under Apple's `container`) and Darwin 27.0.0, with
/// `docs/probes/getpeername/getpeername.c`. `TestPeerNameAgainstHost` puts the
/// same questions to the kernel running the suite.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPeerName =

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64
    let private platforms : SimulatedUnixPlatform list = [ linux ; darwin ]

    let private flavourColumn (platform : SimulatedUnixPlatform) (onLinux : 'a) (onDarwin : 'a) : 'a =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> onLinux
        | SimulatedUnixFlavour.Darwin -> onDarwin

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private wildcard (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port

    let private listenerPort : uint16 = 5000us
    let private receiverPort : uint16 = 5001us

    /// Nothing listens or is bound here in any system these tests build.
    let private nobody : InternetEndpoint = loopback 5999us

    /// `(socklen_t)-1`, which Linux reads as the `int` -1.
    let private minus1 : uint32 = System.UInt32.MaxValue

    let private fresh (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot

    let private endpointOf (fd : int) (system : UnixSystem<int, string>) : InternetEndpoint =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Binding with
            | Some binding -> binding.Endpoint
            | None -> failwith $"fd %d{fd} is not bound"
        | other -> failwith $"fd %d{fd} names %A{other}, not a socket"

    let private stream (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

    let private datagram (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

    let private bindAt (fd : int) (endpoint : InternetEndpoint) (system : UnixSystem<int, string>) =
        match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet (UnixSystem.platform system) endpoint) system with
        | Ok (BindAnswer.Bound _, system) -> system
        | other -> failwith $"binding fd %d{fd} to %O{endpoint} answered %A{other}"

    let private connectBlob (fd : int) (blob : byte[]) (system : UnixSystem<int, string>) =
        match CopyIn.connect fd UserBuffer.Mapped 16u blob system with
        | Ok answer -> answer
        | Error refusal -> failwith $"connect refused: %s{ConnectRefusal.describe refusal}"

    let private connect (fd : int) (destination : InternetEndpoint) (system : UnixSystem<int, string>) =
        connectBlob fd (CopyIn.inet (UnixSystem.platform system) destination) system

    let private nonBlocking (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.setNonBlocking fd true system with
        | SetNonBlockingAnswer.Set, system -> system
        | other, _ -> failwith $"setting O_NONBLOCK on fd %d{fd} answered %A{other}"

    let private listening (address : InternetEndpoint) (system : UnixSystem<int, string>) =
        let fd, system = stream system
        let system = bindAt fd address system

        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> fd, system
        | other -> failwith $"listen answered %A{other}"

    let private accept (listener : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixConnection.accept 0 listener UserBuffer.Mapped 16u system with
        | Ok (AcceptOutcome.Accepted (fd, _, _), system) -> fd, system
        | other -> failwith $"accept answered %A{other}"

    let private close (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed _, system) -> system
        | other -> failwith $"close of fd %d{fd} answered %A{other}"

    /// A listener at `listenerPort`, a client connected to it whose connection
    /// is still queued, and the client's descriptor.
    let private queued (platform : SimulatedUnixPlatform) : int * int * UnixSystem<int, string> =
        let listener, system = listening (loopback listenerPort) (fresh platform)
        let client, system = stream system
        let outcome, system = connect client (loopback listenerPort) system
        outcome |> shouldEqual ConnectOutcome.Completed
        listener, client, system

    let private ask (fd : int) (declared : uint32) (system : UnixSystem<int, string>) =
        UnixSocket.getpeername fd UserBuffer.Mapped declared system

    let private reported
        (platform : SimulatedUnixPlatform)
        (endpoint : InternetEndpoint)
        : Result<GetSockNameAnswer, GetSockNameRefusal>
        =
        Ok (GetSockNameAnswer.Reported (CopyOut.expected platform endpoint 16u, 16))

    let private failed (error : UnixError) : Result<GetSockNameAnswer, GetSockNameRefusal> =
        Ok (GetSockNameAnswer.Failed (error, None))

    // ------------------------------------------------------------------
    // No peer
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a socket never connected has no peer, at any declared length`` (platform : SimulatedUnixPlatform) =
        let system = fresh platform
        let freshFd, system = stream system
        let boundFd, system = stream system
        let system = bindAt boundFd (loopback 0us) system
        let listener, system = listening (loopback listenerPort) system
        let udp, system = datagram system

        // Measured on both, -1 included: the phase is judged before the
        // length, so Linux's EINVAL for a negative one never comes.
        for fd in [ freshFd ; boundFd ; listener ; udp ] do
            for declared in [ 0u ; 16u ; minus1 ] do
                ask fd declared system |> shouldEqual (failed UnixError.ENOTCONN)

    [<TestCaseSource(nameof platforms)>]
    let ``an IPv6 or Unix-domain socket that cannot be connected has no peer`` (platform : SimulatedUnixPlatform) =
        let system = fresh platform

        for domain, kind in
            [
                SocketDomain.Inet6, SocketKind.Stream
                SocketDomain.Inet6, SocketKind.Datagram
                SocketDomain.Unix, SocketKind.Stream
                SocketDomain.Unix, SocketKind.Datagram
            ] do
            let fd, system = NewSocket.create domain kind SocketProtocol.Default system
            ask fd 16u system |> shouldEqual (failed UnixError.ENOTCONN)
            ask fd minus1 system |> shouldEqual (failed UnixError.ENOTCONN)

    [<TestCaseSource(nameof platforms)>]
    let ``a refused connect leaves no peer: ENOTCONN on Linux and EINVAL on Darwin``
        (platform : SimulatedUnixPlatform)
        =
        let expected = failed (flavourColumn platform UnixError.ENOTCONN UnixError.EINVAL)

        // Non-blocking: the refusal pending, then taken by an SO_ERROR read.
        let fd, system = stream (fresh platform)
        let system = nonBlocking fd system
        let outcome, system = connect fd nobody system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)
        ask fd 16u system |> shouldEqual expected

        let level = SimulatedUnixPlatform.socketOptionLevel platform
        let optionName = SimulatedUnixPlatform.socketErrorOption platform

        let system =
            match UnixSocket.getsockopt fd level optionName UserBuffer.Mapped UserBuffer.Mapped (Some 4u) system with
            | Ok (GetSockOptAnswer.Reported (OptionValue.Int error), system) when error <> 0 -> system
            | other -> failwith $"SO_ERROR answered %A{other}"

        ask fd 16u system |> shouldEqual expected
        ask fd minus1 system |> shouldEqual expected

        // Blocking: Linux's socket is idle again, Darwin's stays refused.
        let fd, system = stream (fresh platform)
        let outcome, system = connect fd nobody system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.ECONNREFUSED)
        ask fd 16u system |> shouldEqual expected

    [<TestCaseSource(nameof platforms)>]
    let ``a descriptor that is not a socket answers before the destination is looked at``
        (platform : SimulatedUnixPlatform)
        =
        let system = fresh platform

        UnixSocket.getpeername 99 UserBuffer.Opaque 16u system
        |> shouldEqual (failed UnixError.EBADF)

        // Descriptor 0 is a pipe end in a booted system.
        UnixSocket.getpeername 0 UserBuffer.Opaque 16u system
        |> shouldEqual (failed UnixError.ENOTSOCK)

        let fd, system = stream system

        UnixSocket.getpeername fd UserBuffer.Opaque 16u system
        |> shouldEqual (failed UnixError.ENOTCONN)

    // ------------------------------------------------------------------
    // A peer
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``each end of a connection reports the other, queued, accepted, and after the other closes``
        (platform : SimulatedUnixPlatform)
        =
        let listener, client, system = queued platform
        let clientAddress = endpointOf client system
        ask client 16u system |> shouldEqual (reported platform (loopback listenerPort))

        let server, system = accept listener system
        ask client 16u system |> shouldEqual (reported platform (loopback listenerPort))
        ask server 16u system |> shouldEqual (reported platform clientAddress)

        // Measured on both: an orderly close of one end leaves the other
        // reporting it, before and after it reads the end of the stream.
        ask client 16u (close server system)
        |> shouldEqual (reported platform (loopback listenerPort))

        ask server 16u (close client system)
        |> shouldEqual (reported platform clientAddress)

    [<TestCaseSource(nameof platforms)>]
    let ``a non-blocking connect reports its peer at once`` (platform : SimulatedUnixPlatform) =
        // Real Darwin answers ENOTCONN until the loopback handshake lands, just
        // after the EINPROGRESS; this kernel completes it inside the connect.
        let _, system = listening (loopback listenerPort) (fresh platform)
        let fd, system = stream system
        let system = nonBlocking fd system
        let outcome, system = connect fd (loopback listenerPort) system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EINPROGRESS)
        ask fd 16u system |> shouldEqual (reported platform (loopback listenerPort))

    [<TestCaseSource(nameof platforms)>]
    let ``a connect aimed at the wildcard reports the address it reached`` (platform : SimulatedUnixPlatform) =
        let _, system = listening (wildcard listenerPort) (fresh platform)
        let fd, system = stream system
        let outcome, system = connect fd (wildcard listenerPort) system
        outcome |> shouldEqual ConnectOutcome.Completed
        ask fd 16u system |> shouldEqual (reported platform (loopback listenerPort))

    [<TestCaseSource(nameof platforms)>]
    let ``the declared length bounds what is written and not what is reported`` (platform : SimulatedUnixPlatform) =
        let _, client, system = queued platform
        let whole = CopyOut.expected platform (loopback listenerPort) 16u

        ask client 8u system
        |> shouldEqual (Ok (GetSockNameAnswer.Reported (CopyOut.expected platform (loopback listenerPort) 8u, 16)))

        ask client 128u system
        |> shouldEqual (Ok (GetSockNameAnswer.Reported (whole, 16)))

        UnixSocket.getpeername client (UserBuffer.Unmapped 0UL) 0u system
        |> shouldEqual (Ok (GetSockNameAnswer.Reported (ImmutableArray.Empty, 16)))

        // Linux reads the cell as an `int`, Darwin as the `socklen_t` it is.
        ask client minus1 system
        |> shouldEqual (flavourColumn platform (failed UnixError.EINVAL) (Ok (GetSockNameAnswer.Reported (whole, 16))))

    /// What the length cell holds after the copy faults, by the version of the
    /// kernel each preset runs (`docs/probes/sockname-fault-length`, where
    /// getpeername's rows are getsockname's): 16 on Linux 6.18.5, and the
    /// declared length, untouched, on Linux 6.17 and Darwin.
    let private faultPlatforms : TestCaseData list =
        [
            SimulatedUnixPlatform.linuxArm64, Some 16
            SimulatedUnixPlatform.linuxX64, None
            SimulatedUnixPlatform.macOsArm64, None
        ]
        |> List.map (fun (platform, lengthAfterFault) -> TestCaseData (platform, lengthAfterFault))

    [<TestCaseSource(nameof faultPlatforms)>]
    let ``a faulting destination answers EFAULT, with getsockname's length-cell rule``
        (platform : SimulatedUnixPlatform, lengthAfterFault : int option)
        =
        let _, client, system = queued platform

        UnixSocket.getpeername client (UserBuffer.Unmapped 0x1000UL) 13u system
        |> shouldEqual (Ok (GetSockNameAnswer.Failed (UnixError.EFAULT, lengthAfterFault)))

        UnixSocket.getpeername client UserBuffer.Opaque 16u system
        |> shouldEqual (Error (GetSockNameRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        UnixSocket.getpeername client UserBuffer.Addressless 16u system
        |> shouldEqual (Error (GetSockNameRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

    // ------------------------------------------------------------------
    // Datagram sockets
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a connected datagram socket reports its default peer`` (platform : SimulatedUnixPlatform) =
        let receiver, system = datagram (fresh platform)
        let system = bindAt receiver (loopback receiverPort) system
        let fd, system = datagram system

        let outcome, system = connect fd (loopback receiverPort) system
        outcome |> shouldEqual ConnectOutcome.Completed
        ask fd 16u system |> shouldEqual (reported platform (loopback receiverPort))

        // The wildcard is resolved at the connect: measured 127.0.0.1 on both
        // for a socket with no concrete address of its own.
        let outcome, system = connect fd (wildcard receiverPort) system
        outcome |> shouldEqual ConnectOutcome.Completed
        ask fd 16u system |> shouldEqual (reported platform (loopback receiverPort))

    [<Test>]
    let ``Linux: a datagram socket connected to port 0, or dissolved, has no peer`` () =
        let receiver, system = datagram (fresh linux)
        let system = bindAt receiver (loopback receiverPort) system
        let fd, system = datagram system

        let outcome, system = connect fd (loopback 0us) system
        outcome |> shouldEqual ConnectOutcome.Completed
        ask fd 16u system |> shouldEqual (failed UnixError.ENOTCONN)

        let _, system = connect fd (loopback receiverPort) system
        ask fd 16u system |> shouldEqual (reported linux (loopback receiverPort))

        let outcome, system = connectBlob fd (CopyIn.blob linux 0 (wildcard 0us)) system
        outcome |> shouldEqual ConnectOutcome.Completed
        ask fd 16u system |> shouldEqual (failed UnixError.ENOTCONN)

    [<Test>]
    let ``Darwin: a failed datagram connect leaves no peer`` () =
        let receiver, system = datagram (fresh darwin)
        let system = bindAt receiver (loopback receiverPort) system
        let fd, system = datagram system

        let _, system = connect fd (loopback receiverPort) system
        let outcome, system = connect fd (loopback 0us) system
        outcome |> shouldEqual (ConnectOutcome.Failed UnixError.EADDRNOTAVAIL)
        ask fd 16u system |> shouldEqual (failed UnixError.ENOTCONN)
