namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// `socket(2)` for a test that wants a socket of a known shape and has no
/// interest in how the request was numbered.
[<RequireQualifiedAccess>]
module NewSocket =

    /// The numbers `UnixSocket.socket` takes for this shape, in `platform`'s
    /// own numbering.
    let arguments
        (platform : SimulatedUnixPlatform)
        (domain : SocketDomain)
        (kind : SocketKind)
        (protocol : SocketProtocol)
        : int * int * int
        =
        let domain =
            match domain with
            | SocketDomain.Unix -> 1
            | SocketDomain.Inet -> SimulatedUnixPlatform.internetAddressFamily
            | SocketDomain.Inet6 -> SimulatedUnixPlatform.internetV6AddressFamily platform

        let kind =
            match kind with
            | SocketKind.Stream -> 1
            | SocketKind.Datagram -> 2
            | SocketKind.SeqPacket -> 5

        let protocol =
            match protocol with
            | SocketProtocol.Default -> 0
            | SocketProtocol.Tcp -> 6
            | SocketProtocol.Udp -> 17

        domain, kind, protocol

    /// Create a blocking socket of this shape, failing the test if the kernel
    /// does not.
    let create<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (domain : SocketDomain)
        (kind : SocketKind)
        (protocol : SocketProtocol)
        (system : UnixSystem<'Task, 'Handler>)
        : int * UnixSystem<'Task, 'Handler>
        =
        let rawDomain, rawKind, rawProtocol =
            arguments system.Machine.UnixPlatform domain kind protocol

        match UnixSocket.socket rawDomain rawKind rawProtocol system with
        | Ok (Ok created) -> created
        | Ok (Error error) ->
            failwith $"NewSocket.create: socket(%d{rawDomain}, %d{rawKind}, %d{rawProtocol}) answered %O{error}"
        | Error refusal ->
            failwith
                $"NewSocket.create: socket(%d{rawDomain}, %d{rawKind}, %d{rawProtocol}) was refused: %s{SocketRefusal.describe refusal}"
