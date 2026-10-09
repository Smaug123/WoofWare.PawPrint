namespace WoofWare.PosixKernel

/// What `read(2)` does on a socket with no peer: one that is fresh from
/// `socket(2)`, bound, or listening.
[<RequireQualifiedAccess>]
type internal UnconnectedSocketRead =
    /// The call returns 0, having read nothing.
    | Empty
    /// The call fails with `error`.
    | Fails of error : UnixError
    /// The call sleeps until a datagram arrives.
    | Sleeps

/// What `write(2)` does on a socket with no peer: one that is fresh from
/// `socket(2)`, bound, or listening.
[<RequireQualifiedAccess>]
type internal UnconnectedSocketWrite =
    /// The call fails with `error`, and raises no signal.
    | Fails of error : UnixError
    /// The call fails with `EPIPE` and raises `SIGPIPE`, at the writing task, as
    /// a write into a pipe with no reader does on Linux.
    | Breaks
    /// The call fails with `EMSGSIZE` if it is for more bytes than the socket's
    /// send buffer holds less 32, and with `ENOTCONN` otherwise. The size of the
    /// send buffer is set by `SO_SNDBUF` and, before that, by a sysctl, neither
    /// of which this kernel models.
    | DependsOnSendBuffer

/// How each flavour answers `read(2)` and `write(2)` on a socket with no peer.
[<RequireQualifiedAccess>]
module internal UnconnectedSocketRules =

    /// What `read(2)` of `count` bytes does on an unconnected socket of this
    /// domain and kind, through a description that carries `O_NONBLOCK` if
    /// `nonBlocking`.
    ///
    /// Fails if no socket of this flavour has this domain and kind.
    let read
        (flavour : SimulatedUnixFlavour)
        (domain : SocketDomain)
        (kind : SocketKind)
        (nonBlocking : bool)
        (count : uint64)
        : UnconnectedSocketRead
        =
        // Measured by socket-unconnected-transfer.c on Linux 6.18.5 and Darwin
        // 27.0, every phase with no peer alike, at 0, 1 and 65536 bytes:
        //
        //   socket                 count   Linux       Darwin
        //   any                    0       0           (below)
        //   INET/INET6 stream      >0      ENOTCONN    ENOTCONN, and at 0
        //   UNIX stream            >0      EINVAL      ENOTCONN, and at 0
        //   UNIX seqpacket         >0      ENOTCONN    (no such socket)
        //   datagram               0       0           0
        //   datagram, blocking     >0      sleeps      sleeps
        //   datagram, O_NONBLOCK   >0      EAGAIN      EAGAIN
        match flavour, kind with
        | SimulatedUnixFlavour.Linux, _ when count = 0UL -> UnconnectedSocketRead.Empty
        | _, SocketKind.Datagram when count = 0UL -> UnconnectedSocketRead.Empty
        | _, SocketKind.Datagram when nonBlocking -> UnconnectedSocketRead.Fails UnixError.EAGAIN
        | _, SocketKind.Datagram -> UnconnectedSocketRead.Sleeps
        | SimulatedUnixFlavour.Linux, SocketKind.Stream ->
            match domain with
            | SocketDomain.Unix -> UnconnectedSocketRead.Fails UnixError.EINVAL
            | SocketDomain.Inet
            | SocketDomain.Inet6 -> UnconnectedSocketRead.Fails UnixError.ENOTCONN
        | SimulatedUnixFlavour.Darwin, SocketKind.Stream -> UnconnectedSocketRead.Fails UnixError.ENOTCONN
        | SimulatedUnixFlavour.Linux, SocketKind.SeqPacket ->
            match domain with
            | SocketDomain.Unix -> UnconnectedSocketRead.Fails UnixError.ENOTCONN
            | SocketDomain.Inet
            | SocketDomain.Inet6 ->
                failwith $"UnconnectedSocketRules.read: no Linux socket in %O{domain} is SOCK_SEQPACKET"
        | SimulatedUnixFlavour.Darwin, SocketKind.SeqPacket ->
            failwith "UnconnectedSocketRules.read: no Darwin socket is SOCK_SEQPACKET"

    /// What `write(2)` of `count` bytes does on an unconnected socket of this
    /// domain and kind. Whether the description carries `O_NONBLOCK` makes no
    /// difference.
    ///
    /// Fails if no socket of this flavour has this domain and kind.
    let write
        (flavour : SimulatedUnixFlavour)
        (domain : SocketDomain)
        (kind : SocketKind)
        (count : uint64)
        : UnconnectedSocketWrite
        =
        // Measured by socket-unconnected-transfer.c on Linux 6.18.5 and Darwin
        // 27.0, every phase with no peer alike, blocking or not, at 0, 1, 4096,
        // 65507, 65508, 65527, 65528, 65535, 65536, 212960 and 1048576 bytes:
        //
        //   socket               Linux                         Darwin
        //   INET/INET6 stream    EPIPE, SIGPIPE at the writer  ENOTCONN, no signal
        //   UNIX stream          ENOTCONN                      ENOTCONN
        //   UNIX seqpacket       ENOTCONN                      (no such socket)
        //   INET datagram        EDESTADDRREQ to 65535 bytes,  EDESTADDRREQ
        //                        EMSGSIZE from 65536
        //   INET6 datagram       EDESTADDRREQ                  EDESTADDRREQ
        //   UNIX datagram        ENOTCONN to 212960 bytes,     EDESTADDRREQ
        //                        EMSGSIZE at 1048576
        //
        // Linux checks an IPv4 datagram against UDP's 16-bit length before it
        // looks for a destination, and a Unix-domain one against the send
        // buffer: 212960 is the default 212992-byte buffer less 32.
        match flavour, kind, domain with
        | SimulatedUnixFlavour.Linux, SocketKind.Stream, SocketDomain.Inet
        | SimulatedUnixFlavour.Linux, SocketKind.Stream, SocketDomain.Inet6 -> UnconnectedSocketWrite.Breaks
        | SimulatedUnixFlavour.Linux, SocketKind.Stream, SocketDomain.Unix
        | SimulatedUnixFlavour.Linux, SocketKind.SeqPacket, SocketDomain.Unix ->
            UnconnectedSocketWrite.Fails UnixError.ENOTCONN
        | SimulatedUnixFlavour.Linux, SocketKind.SeqPacket, SocketDomain.Inet
        | SimulatedUnixFlavour.Linux, SocketKind.SeqPacket, SocketDomain.Inet6 ->
            failwith $"UnconnectedSocketRules.write: no Linux socket in %O{domain} is SOCK_SEQPACKET"
        | SimulatedUnixFlavour.Linux, SocketKind.Datagram, SocketDomain.Inet ->
            if count > 65535UL then
                UnconnectedSocketWrite.Fails UnixError.EMSGSIZE
            else
                UnconnectedSocketWrite.Fails UnixError.EDESTADDRREQ
        | SimulatedUnixFlavour.Linux, SocketKind.Datagram, SocketDomain.Inet6 ->
            UnconnectedSocketWrite.Fails UnixError.EDESTADDRREQ
        | SimulatedUnixFlavour.Linux, SocketKind.Datagram, SocketDomain.Unix ->
            UnconnectedSocketWrite.DependsOnSendBuffer
        | SimulatedUnixFlavour.Darwin, SocketKind.Stream, _ -> UnconnectedSocketWrite.Fails UnixError.ENOTCONN
        | SimulatedUnixFlavour.Darwin, SocketKind.Datagram, _ -> UnconnectedSocketWrite.Fails UnixError.EDESTADDRREQ
        | SimulatedUnixFlavour.Darwin, SocketKind.SeqPacket, _ ->
            failwith "UnconnectedSocketRules.write: no Darwin socket is SOCK_SEQPACKET"

    /// Whether `write(2)` on a socket of this domain and kind with no peer and
    /// no port first gives it an ephemeral port, and keeps that binding
    /// whatever the write then answers. An unbound socket is bound to the
    /// wildcard address; one bound to an address with port 0 keeps its
    /// address. Reading binds nothing.
    let writeBindsFirst (flavour : SimulatedUnixFlavour) (domain : SocketDomain) (kind : SocketKind) : bool =
        // Measured by socket-unconnected-autobind.c: getsockname(2) after a
        // failed write of 0, 1 or 65536 bytes reports an ephemeral port for a
        // Linux INET or INET6 datagram socket, EMSGSIZE included, because
        // Linux binds before it hands the write to the protocol. Linux's
        // stream and Unix-domain sockets, and every Darwin socket, are left as
        // they were.
        match flavour, kind, domain with
        | SimulatedUnixFlavour.Linux, SocketKind.Datagram, SocketDomain.Inet
        | SimulatedUnixFlavour.Linux, SocketKind.Datagram, SocketDomain.Inet6 -> true
        | SimulatedUnixFlavour.Linux, _, _
        | SimulatedUnixFlavour.Darwin, _, _ -> false
