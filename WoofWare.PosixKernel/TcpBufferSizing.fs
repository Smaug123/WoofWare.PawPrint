namespace WoofWare.PosixKernel

/// How big a new loopback TCP connection's buffers are, derived from the
/// machine's configuration rather than measured as constants: a measured
/// total would match one write size and nothing else.
[<RequireQualifiedAccess>]
module internal TcpBufferSizing =

    /// A TCP segment's payload over Darwin's loopback, in bytes: the interface's
    /// MTU of 16384, less the IP and TCP headers and the 12-byte timestamp
    /// option both ends use. Measured as `TCP_MAXSEG` (`kevent-write-data.c`).
    let darwinLoopbackSegment (domain : SocketDomain) : int =
        match domain with
        | SocketDomain.Inet -> 16384 - 40 - 12
        | SocketDomain.Inet6 -> 16384 - 60 - 12
        | SocketDomain.Unix ->
            failwith
                "TcpBufferSizing.darwinLoopbackSegment: a Unix-domain socket has no TCP segments (this is a bug in the caller)."

    let private roundUpToSegments (bytes : int) (segment : int) : int =
        (bytes + segment - 1) / segment * segment

    /// The send buffer of each end of a Darwin loopback connection over
    /// `domain`, for a `net.inet.tcp.sendspace` of `sendSpace`: rounded up on
    /// the handshake to whole segments (`tcp_mss`), and capped at
    /// `kern.ipc.maxsockbuf`. 146988 over IPv4 and 146808 over IPv6 at the
    /// default 131072. Measured on Darwin 27.0.0, and explained from XNU's
    /// source, in `kevent-write-data.c`.
    ///
    /// The route to 127.0.0.1 would first raise a smaller buffer to its send
    /// pipe, but `UnixBootImage.withTcpSendSpace` admits nothing below that.
    let darwinSendBuffer (sendSpace : int) (domain : SocketDomain) : int =
        if
            sendSpace < UnixMachineState.darwinLoopbackSendPipe
            || sendSpace > UnixMachineState.darwinSocketBufferMax
        then
            failwith
                $"TcpBufferSizing.darwinSendBuffer: a sendspace of %d{sendSpace}, which UnixBootImage.withTcpSendSpace refuses on Darwin (this is a bug in a caller that assembled the machine by hand)."

        min (roundUpToSegments sendSpace (darwinLoopbackSegment domain)) UnixMachineState.darwinSocketBufferMax

    /// How much a Darwin receive buffer grows by at a time when its free space
    /// runs short (`tcp_sbrcv_grow_rwin`): sixteen segments.
    let private darwinReceiveGrowth (domain : SocketDomain) : int = 16 * darwinLoopbackSegment domain

    /// Why `darwinReceiveBuffer` cannot answer for a `net.inet.tcp.recvspace`
    /// of `recvSpace`, or `None` when it can.
    ///
    /// Below the route's receive pipe the buffer depends on which route the
    /// connection took, which this kernel does not model. Where the handshake
    /// leaves the buffer at sixteen segments or more, or exactly at
    /// `recvSpace` plus sixteen, it grows again as data arrives, by an amount
    /// that depends on how full it is when it is asked; the fixed-size model
    /// cannot follow that.
    let darwinReceiveSpaceRefusal (recvSpace : int) : string option =
        let segments =
            [
                darwinLoopbackSegment SocketDomain.Inet
                darwinLoopbackSegment SocketDomain.Inet6
            ]

        let ceiling = 15 * List.min segments

        if recvSpace < UnixMachineState.darwinLoopbackReceivePipe then
            Some
                $"%d{recvSpace} is below %d{UnixMachineState.darwinLoopbackReceivePipe}, the receive pipe of Darwin's route to 127.0.0.1. A connection's handshake grows a receive buffer that small to the receive pipe of the route it takes, and this kernel does not model routes."
        elif recvSpace > ceiling then
            Some
                $"%d{recvSpace} is above %d{ceiling}, fifteen of the smaller loopback segment. A buffer that size grows again as data arrives, by an amount that depends on how full it is, which this kernel does not model."
        else
            match segments |> List.tryFind (fun segment -> recvSpace % segment = 0) with
            | Some segment ->
                Some
                    $"%d{recvSpace} is a whole number of %d{segment}-byte loopback segments, so the handshake leaves the buffer at exactly the size at which Darwin grows it again as data arrives, by an amount that depends on how full it is, which this kernel does not model."
            | None -> None

    /// The receive buffer a Darwin loopback connection's handshake leaves at
    /// an end over `domain` whose socket started with a buffer, and an ideal
    /// size, of `initial` bytes: a listener's `SO_RCVBUF`, which an accepted
    /// socket inherits, or else `net.inet.tcp.recvspace`.
    ///
    /// Explained from XNU's source (`bsd/netinet/tcp_input.c` and
    /// `tcp_subr.c`). On the handshake `tcp_mss` raises the buffer to the
    /// route's receive pipe if it is smaller, and rounds it up to whole
    /// segments: 131072 becomes 146988, nine segments over IPv4. The window
    /// the handshake advertises comes from `tcp_sbspace`, which calls
    /// `tcp_sbrcv_grow_rwin`: while fewer than sixteen segments are free and
    /// the buffer is no bigger than its ideal size plus sixteen segments, it
    /// grows by sixteen segments. The buffer is empty, so it grows at most
    /// once: 146988 becomes 408300, and the second test then fails, 408300
    /// against 392384.
    ///
    /// `tcp-transfer.c` section C measured the accepted end on Darwin 27.0.0
    /// over IPv4 with a listener `SO_RCVBUF` of 4096, 16384, 65536 and 262144,
    /// and with none at the default 131072; `TestTcpBufferSizing` holds this
    /// to every one. The connecting end runs the same code, but was not
    /// measured.
    let darwinHandshakeReceiveBuffer (initial : int) (domain : SocketDomain) : int =
        let growth = darwinReceiveGrowth domain

        let rounded =
            roundUpToSegments (max initial UnixMachineState.darwinLoopbackReceivePipe) (darwinLoopbackSegment domain)

        // The other half of `tcp_sbrcv_grow_rwin`'s test, that the buffer be
        // no bigger than its ideal size plus sixteen segments, always holds
        // here: rounding up adds less than a segment, and the receive pipe is
        // less than sixteen.
        if rounded < growth then rounded + growth else rounded

    /// The receive buffer of each end of a Darwin loopback connection over
    /// `domain`, for a `net.inet.tcp.recvspace` of `recvSpace`: what
    /// `darwinHandshakeReceiveBuffer` leaves, 408300 over IPv4 and 407800 over
    /// IPv6 at the default 131072. `recvSpace` must be one
    /// `darwinReceiveSpaceRefusal` admits, so that the buffer does not grow
    /// again as data arrives.
    ///
    /// Darwin's timestamp-driven autotuning (`tcp_sbrcv_grow`), which grew the
    /// buffer during the fill in two of section C's 54 trials at the default
    /// size, both with `TCP_NODELAY`, is not modelled.
    let darwinReceiveBuffer (recvSpace : int) (domain : SocketDomain) : int =
        match darwinReceiveSpaceRefusal recvSpace with
        | Some reason ->
            failwith
                $"TcpBufferSizing.darwinReceiveBuffer: %s{reason} UnixBootImage.withTcpReceiveSpace refuses it on Darwin (this is a bug in a caller that assembled the machine by hand)."
        | None -> ()

        let buffer = darwinHandshakeReceiveBuffer recvSpace domain

        // `tcp_sbrcv_grow_rwin` grows a buffer again only while it is no
        // bigger than its ideal size plus sixteen segments.
        if buffer <= recvSpace + darwinReceiveGrowth domain then
            failwith
                $"TcpBufferSizing.darwinReceiveBuffer: a recvspace of %d{recvSpace} leaves a buffer of %d{buffer}, which grows again as data arrives (this is a bug in darwinReceiveSpaceRefusal)."

        buffer

    /// The buffers of a TCP connection that has just completed over `domain`
    /// on `machine`: the same at each end.
    ///
    /// Linux's send buffer is `TcpSendSpaceMax`, which autotuning reaches as a
    /// writer fills it (measured: 3939840 straight after `connect`, 4194304
    /// during the fill), and its receive buffer is `TcpReceiveSpace`. Darwin's
    /// are `darwinSendBuffer` and `darwinReceiveBuffer`. Neither flavour's
    /// totals are matched exactly by counting bytes; see `TcpTransfer`.
    let newTransfer (domain : SocketDomain) (machine : UnixMachineState) : TcpTransfer =
        match domain with
        | SocketDomain.Inet
        | SocketDomain.Inet6 -> ()
        | SocketDomain.Unix ->
            failwith
                "TcpBufferSizing.newTransfer: a Unix-domain socket makes no TCP connection (this is a bug in the caller)."

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        match flavour with
        | SimulatedUnixFlavour.Linux -> TcpTransfer.create flavour machine.TcpSendSpaceMax machine.TcpReceiveSpace
        | SimulatedUnixFlavour.Darwin ->
            TcpTransfer.create
                flavour
                (darwinSendBuffer machine.TcpSendSpace domain)
                (darwinReceiveBuffer machine.TcpReceiveSpace domain)
