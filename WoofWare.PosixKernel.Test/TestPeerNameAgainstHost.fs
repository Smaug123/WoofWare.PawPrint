namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open System.Threading
open NUnit.Framework
open WoofWare.PosixKernel

/// `getpeername(2)` put to the kernel running the suite and to the model of the
/// same flavour, in every socket phase the model can reach, at every declared
/// length and through every kind of destination.
///
/// Each host falsifies its own column: macOS locally, Linux in CI.
/// `TestPeerName` carries both columns as literals. The probe is
/// `docs/probes/getpeername/getpeername.c`, outputs beside it.
///
/// The two sides allocate ephemeral ports independently, so an address is
/// compared by which of the scenario's endpoints it names, never by its bytes.
[<TestFixture>]
module TestPeerNameAgainstHost =

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "bind", SetLastError = true)>]
    extern int private hostBind(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "listen", SetLastError = true)>]
    extern int private hostListen(int fd, int backlog)

    [<DllImport("libc", EntryPoint = "connect", SetLastError = true)>]
    extern int private hostConnect(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "accept", SetLastError = true)>]
    extern int private hostAccept(int fd, nativeint address, nativeint length)

    [<DllImport("libc", EntryPoint = "getsockname", SetLastError = true)>]
    extern int private hostGetSockName(int fd, byte[] address, uint32& length)

    [<DllImport("libc", EntryPoint = "getpeername", SetLastError = true)>]
    extern int private hostGetPeerName(int fd, nativeint address, nativeint length)

    [<DllImport("libc", EntryPoint = "getsockopt", SetLastError = true)>]
    extern int private hostGetSockOpt(int fd, int level, int optionName, byte[] value, uint32& optionLength)

    // `fcntl(2)` is variadic, which a P/Invoke cannot call portably (Apple's
    // arm64 ABI passes variadic arguments on the stack), so this goes through
    // the fixed-arity wrapper in .NET's own `System.Native` library.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll(int64[] fds, unativeint count, int timeoutMs)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "mmap", SetLastError = true)>]
    extern nativeint private hostMmap(
        nativeint address,
        unativeint length,
        int protection,
        int flags,
        int fd,
        int64 offset
    )

    [<Literal>]
    let private AF_UNIX = 1

    [<Literal>]
    let private AF_INET = 2

    [<Literal>]
    let private SOCK_STREAM = 1

    [<Literal>]
    let private SOCK_DGRAM = 2

    [<Literal>]
    let private POLLOUT = 4s

    let private mapPrivateAnonymous (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x02 ||| 0x20
        | SimulatedUnixFlavour.Darwin -> 0x02 ||| 0x1000

    /// One reserved page nothing can be mapped over, whose address faults on
    /// every access. Never unmapped: it is a process-lifetime fixture.
    let private faultingPage : Lazy<uint64> =
        lazy
            match HostPlatform.flavour () with
            | None -> failwith "TestPeerNameAgainstHost: no Unix host, so nothing to reserve"
            | Some flavour ->
                let page = hostMmap (0n, 4096un, 0, mapPrivateAnonymous flavour, -1, 0L)

                if page = -1n then
                    failwith $"TestPeerNameAgainstHost: mmap failed with errno %d{Marshal.GetLastPInvokeError ()}"

                uint64 (int64 page)

    /// Where one socket is in its life when `getpeername` is asked about it.
    [<RequireQualifiedAccess>]
    type Scenario =
        | FreshStream
        | BoundStream
        | Listening
        /// A blocking connect to a listener that has not accepted it.
        | ClientQueued
        | ClientAccepted
        | Accepted
        /// The client, once the accepted end has closed.
        | ClientPeerClosed
        /// The accepted end, once the client has closed.
        | AcceptedPeerClosed
        /// A non-blocking connect to a listener, once it has completed.
        | NonBlockingClient
        /// A blocking connect to a wildcard-bound listener, aimed at 0.0.0.0.
        | ClientToWildcard
        /// A non-blocking connect to a port nothing listens on, once refused.
        | RefusedPending
        /// As `RefusedPending`, once an `SO_ERROR` read has taken the refusal.
        | RefusedTaken
        /// A blocking connect to a port nothing listens on.
        | RefusedBlocking
        | FreshDatagram
        | DatagramConnected
        /// A datagram socket connected to 0.0.0.0 at a receiver's port.
        | DatagramToWildcard
        /// A datagram socket connected to 127.0.0.1:0.
        | DatagramPortZero
        /// A connected datagram socket that then connected to `AF_UNSPEC`.
        | DatagramDissolved
        | FreshInet6Stream
        | FreshInet6Datagram
        | FreshUnixStream
        | FreshUnixDatagram
        | ClosedDescriptor

    let scenarios : Scenario list =
        [
            Scenario.FreshStream
            Scenario.BoundStream
            Scenario.Listening
            Scenario.ClientQueued
            Scenario.ClientAccepted
            Scenario.Accepted
            Scenario.ClientPeerClosed
            Scenario.AcceptedPeerClosed
            Scenario.NonBlockingClient
            Scenario.ClientToWildcard
            Scenario.RefusedPending
            Scenario.RefusedTaken
            Scenario.RefusedBlocking
            Scenario.FreshDatagram
            Scenario.DatagramConnected
            Scenario.DatagramToWildcard
            Scenario.DatagramPortZero
            Scenario.DatagramDissolved
            Scenario.FreshInet6Stream
            Scenario.FreshInet6Datagram
            Scenario.FreshUnixStream
            Scenario.FreshUnixDatagram
            Scenario.ClosedDescriptor
        ]

    /// The scenario's named endpoints: the listener or receiver at 127.0.0.1
    /// and its port, and the client's own address.
    type private Roles = Map<string, InternetEndpoint>

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private wildcard (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port

    // ---------------------------------------------------------------- host

    let private check (what : string) (result : int) : unit =
        if result < 0 then
            failwith $"the host's %s{what} failed with errno %d{Marshal.GetLastPInvokeError ()}"

    let private hostNew (domain : int) (kind : int) : int =
        let fd = hostSocket (domain, kind, 0)
        check "socket" fd
        fd

    let private hostBindTo (platform : SimulatedUnixPlatform) (fd : int) (endpoint : InternetEndpoint) : unit =
        let address = SimulatedUnixPlatform.encodeInternetSockaddr platform endpoint
        check "bind" (hostBind (fd, address, uint32 address.Length))

    let private hostName (fd : int) : InternetEndpoint =
        let name = Array.zeroCreate<byte> 16
        let mutable length = 16u
        check "getsockname" (hostGetSockName (fd, name, &length))

        InternetEndpoint.ofParts
            (Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan (name, 4, 4)))
            (Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan (name, 2, 2)))

    /// The errno of a connect, 0 for success.
    let private hostConnectTo (platform : SimulatedUnixPlatform) (fd : int) (endpoint : InternetEndpoint) : int =
        let address = SimulatedUnixPlatform.encodeInternetSockaddr platform endpoint
        Marshal.SetLastPInvokeError 0

        if hostConnect (fd, address, uint32 address.Length) = 0 then
            0
        else
            Marshal.GetLastPInvokeError ()

    /// Wait until a non-blocking connect in flight on the host has an outcome,
    /// as the model's connect has one at once.
    let private settle (fd : int) : unit =
        // struct pollfd { int fd; short events; short revents; }, little-endian.
        let entry = [| int64 (uint32 fd) ||| (int64 POLLOUT <<< 32) |]

        if hostPoll (entry, 1un, 5000) <> 1 then
            failwith $"the host's connect on fd %d{fd} had no outcome after five seconds"

    let private hostListener (platform : SimulatedUnixPlatform) (address : InternetEndpoint) : int * uint16 =
        let fd = hostNew AF_INET SOCK_STREAM
        hostBindTo platform fd address
        check "listen" (hostListen (fd, 16))
        fd, (hostName fd).Port

    let private hostDeadPort (platform : SimulatedUnixPlatform) : uint16 =
        let fd = hostNew AF_INET SOCK_STREAM
        hostBindTo platform fd (loopback 0us)
        let port = (hostName fd).Port
        hostClose fd |> ignore<int>
        port

    let private hostTakeError (platform : SimulatedUnixPlatform) (fd : int) : unit =
        let value = Array.zeroCreate<byte> 4
        let mutable length = 4u

        check
            "getsockopt(SO_ERROR)"
            (hostGetSockOpt (
                fd,
                SimulatedUnixPlatform.socketOptionLevel platform,
                SimulatedUnixPlatform.socketErrorOption platform,
                value,
                &length
            ))

    /// Build `scenario` on the host: the descriptor to ask, its roles, and
    /// every descriptor to close afterwards.
    let private hostBuild (platform : SimulatedUnixPlatform) (scenario : Scenario) : int * Roles * int list =
        let connected (listenerAddress : InternetEndpoint) (aimAt : uint16 -> InternetEndpoint) =
            let listener, port = hostListener platform listenerAddress
            let client = hostNew AF_INET SOCK_STREAM
            let errno = hostConnectTo platform client (aimAt port)

            if errno <> 0 then
                failwith $"the host's connect answered errno %d{errno}"

            let roles = Map.ofList [ "listener", loopback port ; "client", hostName client ]

            listener, client, roles

        let accept (listener : int) : int =
            let fd = hostAccept (listener, 0n, 0n)
            check "accept" fd
            fd

        match scenario with
        | Scenario.FreshStream ->
            let fd = hostNew AF_INET SOCK_STREAM
            fd, Map.empty, [ fd ]
        | Scenario.BoundStream ->
            let fd = hostNew AF_INET SOCK_STREAM
            hostBindTo platform fd (loopback 0us)
            fd, Map.ofList [ "self", hostName fd ], [ fd ]
        | Scenario.Listening ->
            let fd, port = hostListener platform (loopback 0us)
            fd, Map.ofList [ "self", loopback port ], [ fd ]
        | Scenario.ClientQueued ->
            let listener, client, roles = connected (loopback 0us) loopback
            client, roles, [ client ; listener ]
        | Scenario.ClientAccepted ->
            let listener, client, roles = connected (loopback 0us) loopback
            let server = accept listener
            client, roles, [ client ; server ; listener ]
        | Scenario.Accepted ->
            let listener, client, roles = connected (loopback 0us) loopback
            let server = accept listener
            server, roles, [ client ; server ; listener ]
        | Scenario.ClientPeerClosed ->
            let listener, client, roles = connected (loopback 0us) loopback
            hostClose (accept listener) |> ignore<int>
            Thread.Sleep 50
            client, roles, [ client ; listener ]
        | Scenario.AcceptedPeerClosed ->
            let listener, client, roles = connected (loopback 0us) loopback
            let server = accept listener
            hostClose client |> ignore<int>
            Thread.Sleep 50
            server, roles, [ server ; listener ]
        | Scenario.NonBlockingClient ->
            let listener, port = hostListener platform (loopback 0us)
            let client = hostNew AF_INET SOCK_STREAM
            check "O_NONBLOCK" (hostSetNonBlocking (nativeint client, 1))
            hostConnectTo platform client (loopback port) |> ignore<int>
            settle client

            client, Map.ofList [ "listener", loopback port ; "client", hostName client ], [ client ; listener ]
        | Scenario.ClientToWildcard ->
            let listener, client, roles = connected (wildcard 0us) wildcard
            client, roles, [ client ; listener ]
        | Scenario.RefusedPending
        | Scenario.RefusedTaken ->
            let port = hostDeadPort platform
            let client = hostNew AF_INET SOCK_STREAM
            check "O_NONBLOCK" (hostSetNonBlocking (nativeint client, 1))
            hostConnectTo platform client (loopback port) |> ignore<int>
            settle client

            if scenario = Scenario.RefusedTaken then
                hostTakeError platform client

            client, Map.ofList [ "dead", loopback port ], [ client ]
        | Scenario.RefusedBlocking ->
            let port = hostDeadPort platform
            let client = hostNew AF_INET SOCK_STREAM
            hostConnectTo platform client (loopback port) |> ignore<int>
            client, Map.ofList [ "dead", loopback port ], [ client ]
        | Scenario.FreshDatagram ->
            let fd = hostNew AF_INET SOCK_DGRAM
            fd, Map.empty, [ fd ]
        | Scenario.DatagramConnected
        | Scenario.DatagramToWildcard
        | Scenario.DatagramPortZero
        | Scenario.DatagramDissolved ->
            let receiver = hostNew AF_INET SOCK_DGRAM
            hostBindTo platform receiver (loopback 0us)
            let port = (hostName receiver).Port
            let fd = hostNew AF_INET SOCK_DGRAM

            match scenario with
            | Scenario.DatagramToWildcard -> hostConnectTo platform fd (wildcard port) |> ignore<int>
            | Scenario.DatagramPortZero -> hostConnectTo platform fd (loopback 0us) |> ignore<int>
            | _ -> hostConnectTo platform fd (loopback port) |> ignore<int>

            if scenario = Scenario.DatagramDissolved then
                let unspecified = CopyIn.blob platform 0 (wildcard 0us)
                hostConnect (fd, unspecified, 16u) |> ignore<int>

            fd, Map.ofList [ "receiver", loopback port ], [ fd ; receiver ]
        | Scenario.FreshInet6Stream ->
            let fd =
                hostNew (SimulatedUnixPlatform.internetV6AddressFamily platform) SOCK_STREAM

            fd, Map.empty, [ fd ]
        | Scenario.FreshInet6Datagram ->
            let fd = hostNew (SimulatedUnixPlatform.internetV6AddressFamily platform) SOCK_DGRAM
            fd, Map.empty, [ fd ]
        | Scenario.FreshUnixStream ->
            let fd = hostNew AF_UNIX SOCK_STREAM
            fd, Map.empty, [ fd ]
        | Scenario.FreshUnixDatagram ->
            let fd = hostNew AF_UNIX SOCK_DGRAM
            fd, Map.empty, [ fd ]
        | Scenario.ClosedDescriptor -> -1, Map.empty, []

    // --------------------------------------------------------------- model

    let private modelName (fd : int) (system : UnixSystem<int, string>) : InternetEndpoint =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            match (UnixMachineState.socket socketId system.Machine).Binding with
            | Some binding -> binding.Endpoint
            | None -> failwith $"the model's fd %d{fd} is not bound"
        | other -> failwith $"the model's fd %d{fd} names %A{other}, not a socket"

    let private modelBuild
        (platform : SimulatedUnixPlatform)
        (scenario : Scenario)
        : int * Roles * UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let create (domain : SocketDomain) (kind : SocketKind) (protocol : SocketProtocol) system =
            NewSocket.create domain kind protocol system

        let bindTo (fd : int) (endpoint : InternetEndpoint) (system : UnixSystem<int, string>) =
            match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet platform endpoint) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"the model's bind of fd %d{fd} answered %A{other}"

        let connectTo (fd : int) (blob : byte[]) (system : UnixSystem<int, string>) =
            match CopyIn.connect fd UserBuffer.Mapped 16u blob system with
            | Ok (_, system) -> system
            | Error refusal -> failwith $"the model refused a connect: %s{ConnectRefusal.describe refusal}"

        let listener (address : InternetEndpoint) (system : UnixSystem<int, string>) =
            let fd, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = bindTo fd address system

            match UnixSocket.listen fd 16 system with
            | Ok (ListenAnswer.Listening _, system) -> fd, (modelName fd system).Port, system
            | other -> failwith $"the model's listen answered %A{other}"

        let nonBlocking (fd : int) (system : UnixSystem<int, string>) =
            match UnixDescriptor.setNonBlocking fd true system with
            | SetNonBlockingAnswer.Set, system -> system
            | other, _ -> failwith $"the model would not set O_NONBLOCK: %A{other}"

        let accept (listener : int) (system : UnixSystem<int, string>) =
            match UnixConnection.accept 0 listener UserBuffer.Mapped 16u system with
            | Ok (AcceptOutcome.Accepted (fd, _, _), system) -> fd, system
            | other -> failwith $"the model's accept answered %A{other}"

        let close (fd : int) (system : UnixSystem<int, string>) =
            match UnixDescriptor.close fd system with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | Ok (SyscallAnswer.Failed error, _) -> failwith $"the model's close answered %O{error}"
            | Error refusal -> failwith $"the model refused a close: %s{CloseRefusal.describe refusal}"

        let connected (listenerAddress : InternetEndpoint) (aimAt : uint16 -> InternetEndpoint) system =
            let listenerFd, port, system = listener listenerAddress system

            let client, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = connectTo client (CopyIn.inet platform (aimAt port)) system

            let roles =
                Map.ofList [ "listener", loopback port ; "client", modelName client system ]

            listenerFd, client, roles, system

        let deadPort (system : UnixSystem<int, string>) =
            let fd, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = bindTo fd (loopback 0us) system
            let port = (modelName fd system).Port
            port, close fd system

        match scenario with
        | Scenario.FreshStream ->
            let fd, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            fd, Map.empty, system
        | Scenario.BoundStream ->
            let fd, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = bindTo fd (loopback 0us) system
            fd, Map.ofList [ "self", modelName fd system ], system
        | Scenario.Listening ->
            let fd, port, system = listener (loopback 0us) system
            fd, Map.ofList [ "self", loopback port ], system
        | Scenario.ClientQueued ->
            let _, client, roles, system = connected (loopback 0us) loopback system
            client, roles, system
        | Scenario.ClientAccepted ->
            let listenerFd, client, roles, system = connected (loopback 0us) loopback system
            let _, system = accept listenerFd system
            client, roles, system
        | Scenario.Accepted ->
            let listenerFd, _, roles, system = connected (loopback 0us) loopback system
            let server, system = accept listenerFd system
            server, roles, system
        | Scenario.ClientPeerClosed ->
            let listenerFd, client, roles, system = connected (loopback 0us) loopback system
            let server, system = accept listenerFd system
            client, roles, close server system
        | Scenario.AcceptedPeerClosed ->
            let listenerFd, client, roles, system = connected (loopback 0us) loopback system
            let server, system = accept listenerFd system
            server, roles, close client system
        | Scenario.NonBlockingClient ->
            let _, port, system = listener (loopback 0us) system

            let client, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = nonBlocking client system
            let system = connectTo client (CopyIn.inet platform (loopback port)) system

            client, Map.ofList [ "listener", loopback port ; "client", modelName client system ], system
        | Scenario.ClientToWildcard ->
            let _, client, roles, system = connected (wildcard 0us) wildcard system
            client, roles, system
        | Scenario.RefusedPending
        | Scenario.RefusedTaken ->
            let port, system = deadPort system

            let client, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = nonBlocking client system
            let system = connectTo client (CopyIn.inet platform (loopback port)) system

            let system =
                if scenario = Scenario.RefusedTaken then
                    let level = SimulatedUnixPlatform.socketOptionLevel platform
                    let optionName = SimulatedUnixPlatform.socketErrorOption platform

                    match
                        UnixSocket.getsockopt
                            client
                            level
                            optionName
                            UserBuffer.Mapped
                            UserBuffer.Mapped
                            (Some 4u)
                            system
                    with
                    | Ok (GetSockOptAnswer.Reported _, system) -> system
                    | other -> failwith $"the model's SO_ERROR read answered %A{other}"
                else
                    system

            client, Map.ofList [ "dead", loopback port ], system
        | Scenario.RefusedBlocking ->
            let port, system = deadPort system

            let client, system =
                create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            let system = connectTo client (CopyIn.inet platform (loopback port)) system
            client, Map.ofList [ "dead", loopback port ], system
        | Scenario.FreshDatagram ->
            let fd, system =
                create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

            fd, Map.empty, system
        | Scenario.DatagramConnected
        | Scenario.DatagramToWildcard
        | Scenario.DatagramPortZero
        | Scenario.DatagramDissolved ->
            let receiver, system =
                create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

            let system = bindTo receiver (loopback 0us) system
            let port = (modelName receiver system).Port

            let fd, system =
                create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

            let aim =
                match scenario with
                | Scenario.DatagramToWildcard -> wildcard port
                | Scenario.DatagramPortZero -> loopback 0us
                | _ -> loopback port

            let system = connectTo fd (CopyIn.inet platform aim) system

            let system =
                if scenario = Scenario.DatagramDissolved then
                    connectTo fd (CopyIn.blob platform 0 (wildcard 0us)) system
                else
                    system

            fd, Map.ofList [ "receiver", loopback port ], system
        | Scenario.FreshInet6Stream ->
            let fd, system =
                create SocketDomain.Inet6 SocketKind.Stream SocketProtocol.Default system

            fd, Map.empty, system
        | Scenario.FreshInet6Datagram ->
            let fd, system =
                create SocketDomain.Inet6 SocketKind.Datagram SocketProtocol.Default system

            fd, Map.empty, system
        | Scenario.FreshUnixStream ->
            let fd, system =
                create SocketDomain.Unix SocketKind.Stream SocketProtocol.Default system

            fd, Map.empty, system
        | Scenario.FreshUnixDatagram ->
            let fd, system =
                create SocketDomain.Unix SocketKind.Datagram SocketProtocol.Default system

            fd, Map.empty, system
        | Scenario.ClosedDescriptor -> -1, Map.empty, system

    // ------------------------------------------------------------- compare

    [<RequireQualifiedAccess>]
    type private Destination =
        | Real
        | Null
        | Faulting

    let private destinations : Destination list =
        [ Destination.Real ; Destination.Null ; Destination.Faulting ]

    let private lengths : uint32 list =
        [
            0u
            1u
            2u
            4u
            8u
            15u
            16u
            17u
            28u
            128u
            0x7fff_ffffu
            0x8000_0000u
            UInt32.MaxValue
        ]

    [<Literal>]
    let private BufferSize : int = 128

    let private sentinel : byte = 0xaauy

    /// What one call answered, with every address it wrote reduced to the
    /// roles whose encoding it matches.
    type private Observed =
        {
            Errno : int
            Cell : uint32
            /// Through real storage: how many leading bytes the call wrote, and
            /// which roles' encodings they are a prefix of.
            Written : (int * Set<string>) option
        }

    /// How many leading bytes of `buffer` differ from a run of `sentinel` to
    /// its end.
    let private writtenBefore (sentinel : byte) (buffer : byte[]) : int =
        let mutable n = buffer.Length

        while n > 0 && buffer.[n - 1] = sentinel do
            n <- n - 1

        n

    /// The roles whose encodings the first `written` bytes of `buffer` are a
    /// prefix of.
    let private describeBuffer
        (platform : SimulatedUnixPlatform)
        (roles : Roles)
        (written : int)
        (buffer : byte[])
        : int * Set<string>
        =
        let matching =
            roles
            |> Map.filter (fun _ endpoint ->
                let encoded = SimulatedUnixPlatform.encodeInternetSockaddr platform endpoint

                written <= encoded.Length
                && Seq.forall2 (=) (Seq.take written buffer) (Seq.take written encoded)
            )
            |> Map.keys
            |> Set.ofSeq

        written, matching

    /// One host call through a buffer filled with `fill`: its errno, the cell
    /// it left, and the buffer.
    let private hostCall
        (fd : int)
        (destination : Destination)
        (declared : uint32)
        (fill : byte)
        : int * uint32 * byte[]
        =
        let storage = Marshal.AllocHGlobal BufferSize
        let cell = Marshal.AllocHGlobal 4

        try
            Marshal.Copy (Array.create BufferSize fill, 0, storage, BufferSize)
            Marshal.WriteInt32 (cell, int declared)

            let address =
                match destination with
                | Destination.Real -> storage
                | Destination.Null -> 0n
                | Destination.Faulting -> nativeint (int64 faultingPage.Value)

            Marshal.SetLastPInvokeError 0

            let errno =
                if hostGetPeerName (fd, address, cell) = 0 then
                    0
                else
                    Marshal.GetLastPInvokeError ()

            let buffer = Array.zeroCreate<byte> BufferSize
            Marshal.Copy (storage, buffer, 0, BufferSize)
            errno, uint32 (Marshal.ReadInt32 cell), buffer
        finally
            Marshal.FreeHGlobal storage
            Marshal.FreeHGlobal cell

    /// The call twice, through buffers filled with two different bytes, so that
    /// a written byte equal to one fill cannot be mistaken for an untouched one.
    /// `getpeername` changes nothing, so the second call sees what the first did.
    let private hostQuery
        (platform : SimulatedUnixPlatform)
        (roles : Roles)
        (fd : int)
        (destination : Destination)
        (declared : uint32)
        : Observed
        =
        let errno, cell, buffer = hostCall fd destination declared sentinel

        let errnoAgain, cellAgain, bufferAgain =
            hostCall fd destination declared ~~~sentinel

        if (errno, cell) <> (errnoAgain, cellAgain) then
            failwith $"the host answered %d{errno}, %d{cell} and then %d{errnoAgain}, %d{cellAgain}"

        let written =
            max (writtenBefore sentinel buffer) (writtenBefore ~~~sentinel bufferAgain)

        {
            Errno = errno
            Cell = cell
            Written =
                match destination with
                | Destination.Real -> Some (describeBuffer platform roles written buffer)
                | Destination.Null
                | Destination.Faulting -> None
        }

    let private modelQuery
        (roles : Roles)
        (fd : int)
        (destination : Destination)
        (declared : uint32)
        (system : UnixSystem<int, string>)
        : Observed
        =
        let platform = system.Machine.UnixPlatform

        let userBuffer =
            match destination with
            | Destination.Real -> UserBuffer.Mapped
            | Destination.Null -> UserBuffer.Unmapped 0UL
            | Destination.Faulting -> UserBuffer.Unmapped faultingPage.Value

        let written (copied : ImmutableArray<byte>) =
            match destination with
            | Destination.Real ->
                let buffer = Array.create BufferSize sentinel
                copied.CopyTo buffer
                Some (describeBuffer platform roles copied.Length buffer)
            | Destination.Null
            | Destination.Faulting -> None

        match UnixSocket.getpeername fd userBuffer declared system with
        | Error refusal -> failwith $"the model refused: %s{GetSockNameRefusal.describe refusal}"
        | Ok (GetSockNameAnswer.Reported (copied, reported)) ->
            {
                Errno = 0
                Cell = uint32 reported
                Written = written copied
            }
        | Ok (GetSockNameAnswer.Failed (error, overwritten)) ->
            {
                Errno = UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error
                Cell =
                    match overwritten with
                    | Some length -> uint32 length
                    | None -> declared
                Written = written ImmutableArray.Empty
            }

    [<TestCaseSource(nameof scenarios)>]
    let ``getpeername answers as this kernel does`` (scenario : Scenario) : unit =
        HostPlatform.onUnixHost (fun flavour ->
            if not BitConverter.IsLittleEndian then
                Assert.Ignore "the model's presets are little-endian machines"

            let platform = HostPlatform.platformOf flavour
            let hostFd, hostRoles, owned = hostBuild platform scenario

            try
                let modelFd, modelRoles, system = modelBuild platform scenario

                if Map.keys hostRoles |> Set.ofSeq <> (Map.keys modelRoles |> Set.ofSeq) then
                    failwith $"the scenario's roles disagree: host %A{hostRoles}, model %A{modelRoles}"

                let disagreements =
                    [
                        for destination in destinations do
                            for declared in lengths do
                                let host = hostQuery platform hostRoles hostFd destination declared
                                let model = modelQuery modelRoles modelFd destination declared system

                                if host <> model then
                                    $"%A{destination} at %d{declared}: the host observed %A{host}, the model %A{model}"
                    ]

                if not disagreements.IsEmpty then
                    failwith (String.Join ("\n", disagreements))
            finally
                for fd in owned do
                    hostClose fd |> ignore<int>
        )
