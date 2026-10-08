namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// A POSIX socket option the kernel models, which the shim's
/// `SystemNative_SetSockOpt` and `SystemNative_GetSockOpt` pass a managed
/// option through to.
[<RequireQualifiedAccess>]
type KernelSocketOption =
    | Error
    | NoDelay
    | Ipv6Only
    | Linger

/// What the shim's `SystemNative_SetSockOpt` and `SystemNative_GetSockOpt` do
/// with a managed `SocketOptionLevel` and `SocketOptionName`, once past their
/// own screens of the buffers.
[<RequireQualifiedAccess>]
type ShimSocketOption =
    /// The shim makes the syscall at a POSIX option the kernel models.
    | Kernel of KernelSocketOption
    /// The shim makes a syscall at a POSIX option the kernel does not model,
    /// named here, or (for `SO_REUSEADDR` and the exclusive-use option it
    /// derives from it, and Darwin's `SO_ACCEPTCONN`) does some work of its own
    /// that reaches one.
    | Unmodelled of description : string
    /// `TryGetPlatformSocketOption` has no mapping: the shim answers ENOTSUP
    /// without a syscall, and sets no errno.
    | NotSupported

/// The shim's `LingerOption`, which `SystemNative_SetLingerOption` and
/// `SystemNative_GetLingerOption` read and write: two `int32`s.
type LingerOption =
    {
        /// Non-zero to linger.
        OnOff : int
        /// The linger time, in seconds.
        Seconds : int
    }

/// The managed socket-option numbering the shim receives, and what it does with
/// each number (`pal_networking.c`, `TryGetPlatformSocketOption` and the
/// special cases ahead of it in `SystemNative_SetSockOpt` and
/// `SystemNative_GetSockOpt`), and the shim's linger plumbing.
[<RequireQualifiedAccess>]
module SocketOptionPal =

    /// `SocketOptionLevel_SOL_SOCKET` (`System.Net.Sockets.SocketOptionLevel.Socket`).
    [<Literal>]
    let SolSocket : int = 0xffff

    [<Literal>]
    let private SolIp : int = 0

    [<Literal>]
    let private SolIpv6 : int = 41

    [<Literal>]
    let private SolTcp : int = 6

    [<Literal>]
    let private SolUdp : int = 17

    /// `SocketOptionName_SO_REUSEADDR`.
    [<Literal>]
    let ReuseAddress : int = 0x0004

    /// `SocketOptionName_SO_EXCLUSIVEADDRUSE`, which the shim defines as
    /// `~SocketOptionName_SO_REUSEADDR`.
    [<Literal>]
    let ExclusiveAddressUse : int = -5

    /// `SocketOptionName_SO_ACCEPTCONN`.
    [<Literal>]
    let AcceptConnection : int = 0x0002

    /// The managed pair's meaning, as `TryGetPlatformSocketOption` decides it on
    /// the flavour the shim was compiled for.
    ///
    /// Every name the switch maps is mapped on both flavours: the `#ifdef`s
    /// guarding some of them hold on both, and `SO_IP_DONTFRAGMENT` maps to
    /// Linux's `IP_MTU_DISCOVER` and Darwin's `IP_DONTFRAG`.
    let decode (level : int) (name : int) : ShimSocketOption =
        let unmodelled (posix : string) =
            ShimSocketOption.Unmodelled $"the shim passes it on as %s{posix}, which the kernel does not model"

        match level with
        | SolSocket ->
            match name with
            | 0x0001 -> unmodelled "SO_DEBUG"
            | 0x0002 -> unmodelled "SO_ACCEPTCONN"
            | 0x0004 -> unmodelled "SO_REUSEADDR"
            | 0x0008 -> unmodelled "SO_KEEPALIVE"
            | 0x0010 -> unmodelled "SO_DONTROUTE"
            | 0x0020 -> unmodelled "SO_BROADCAST"
            | 0x0080 -> ShimSocketOption.Kernel KernelSocketOption.Linger
            | 0x0100 -> unmodelled "SO_OOBINLINE"
            | 0x1001 -> unmodelled "SO_SNDBUF"
            | 0x1002 -> unmodelled "SO_RCVBUF"
            | 0x1003 -> unmodelled "SO_SNDLOWAT"
            | 0x1004 -> unmodelled "SO_RCVLOWAT"
            | 0x1005 -> unmodelled "SO_SNDTIMEO"
            | 0x1006 -> unmodelled "SO_RCVTIMEO"
            | 0x1007 -> ShimSocketOption.Kernel KernelSocketOption.Error
            | 0x1008 -> unmodelled "SO_TYPE"
            | _ -> ShimSocketOption.NotSupported
        | SolIp ->
            match name with
            | 1 -> unmodelled "IP_OPTIONS"
            | 2 -> unmodelled "IP_HDRINCL"
            | 3 -> unmodelled "IP_TOS"
            | 4 -> unmodelled "IP_TTL"
            | 9 -> unmodelled "IP_MULTICAST_IF"
            | 10 -> unmodelled "IP_MULTICAST_TTL"
            | 11 -> unmodelled "IP_MULTICAST_LOOP"
            | 12 -> unmodelled "IP_ADD_MEMBERSHIP"
            | 13 -> unmodelled "IP_DROP_MEMBERSHIP"
            | 14 -> unmodelled "IP_MTU_DISCOVER or IP_DONTFRAG"
            | 15 -> unmodelled "IP_ADD_SOURCE_MEMBERSHIP"
            | 16 -> unmodelled "IP_DROP_SOURCE_MEMBERSHIP"
            | 17 -> unmodelled "IP_BLOCK_SOURCE"
            | 18 -> unmodelled "IP_UNBLOCK_SOURCE"
            | 19 -> unmodelled "IP_PKTINFO"
            | _ -> ShimSocketOption.NotSupported
        | SolIpv6 ->
            match name with
            | 21 -> unmodelled "IPV6_HOPLIMIT"
            | 27 -> ShimSocketOption.Kernel KernelSocketOption.Ipv6Only
            | 19 -> unmodelled "IPV6_RECVPKTINFO"
            | 9 -> unmodelled "IPV6_MULTICAST_IF"
            | 11 -> unmodelled "IPV6_MULTICAST_LOOP"
            | 10 -> unmodelled "IPV6_MULTICAST_HOPS"
            | 4 -> unmodelled "IPV6_UNICAST_HOPS"
            | _ -> ShimSocketOption.NotSupported
        | SolTcp ->
            match name with
            | 1 -> ShimSocketOption.Kernel KernelSocketOption.NoDelay
            | 16 -> unmodelled "TCP_KEEPCNT"
            | 3 -> unmodelled "TCP_KEEPIDLE or TCP_KEEPALIVE"
            | 17 -> unmodelled "TCP_KEEPINTVL"
            | 15 -> unmodelled "TCP_FASTOPEN"
            | _ -> ShimSocketOption.NotSupported
        | SolUdp -> ShimSocketOption.NotSupported
        | _ -> ShimSocketOption.NotSupported

    /// The POSIX `level` and option name the kernel takes for `option`, in the
    /// simulated platform's numbering.
    let numbered (platform : SimulatedUnixPlatform) (option : KernelSocketOption) : int * int =
        match option with
        | KernelSocketOption.Error ->
            SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.socketErrorOption platform
        | KernelSocketOption.NoDelay ->
            SimulatedUnixPlatform.tcpOptionLevel platform, SimulatedUnixPlatform.noDelayOption platform
        | KernelSocketOption.Ipv6Only ->
            SimulatedUnixPlatform.ipv6OptionLevel platform, SimulatedUnixPlatform.ipv6OnlyOption platform
        | KernelSocketOption.Linger ->
            SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.lingerOption platform

    /// The option `SystemNative_SetLingerOption` and `SystemNative_GetLingerOption`
    /// make their syscall at: `SO_LINGER_SEC` on Darwin, whose `SO_LINGER` is
    /// in hundredths of a second, and `SO_LINGER` elsewhere. Both in seconds.
    let lingerOptionNumbered (platform : SimulatedUnixPlatform) : int * int =
        let level = SimulatedUnixPlatform.socketOptionLevel platform

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Darwin ->
            match SimulatedUnixPlatform.lingerSecondsOption platform with
            | Some name -> level, name
            | None -> failwith $"SocketOptionPal: %O{platform} is a Darwin flavour without SO_LINGER_SEC"
        | SimulatedUnixFlavour.Linux -> level, SimulatedUnixPlatform.lingerOption platform

    /// The longest linger time `SystemNative_SetLingerOption` passes on; it
    /// answers EINVAL without a syscall for one turning lingering on with a time
    /// outside 0 to this. Elsewhere the smaller of Winsock's 65535 and the
    /// largest `int`; on Darwin, 32767 over `sysconf(_SC_CLK_TCK)`, so that the
    /// time in hundredths fits Darwin's sixteen bits. `_SC_CLK_TCK` measured
    /// 100 on both (`docs/probes/sockopt-options/`).
    let maxLingerSeconds (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 65535
        | SimulatedUnixFlavour.Darwin -> 32767 / 100

    /// Whether `SystemNative_SetLingerOption` answers a failed `setsockopt`'s
    /// EINVAL with success: on Darwin, which answers EINVAL for a socket whose
    /// peer has gone, the shim reports success, though the syscall left errno
    /// set.
    let lingerSwallowsInvalid (platform : SimulatedUnixPlatform) : bool =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// A `LingerOption` as the shim's struct lays it out: `OnOff` then
    /// `Seconds`, each a little-endian `int32`.
    let encodeLingerOption (option : LingerOption) : byte[] =
        let bytes = Array.zeroCreate<byte> 8
        System.Buffers.Binary.BinaryPrimitives.WriteInt32LittleEndian (System.Span<byte> (bytes, 0, 4), option.OnOff)
        System.Buffers.Binary.BinaryPrimitives.WriteInt32LittleEndian (System.Span<byte> (bytes, 4, 4), option.Seconds)
        bytes

    /// The `LingerOption` in the shim's eight bytes.
    let decodeLingerOption (bytes : System.Collections.Immutable.ImmutableArray<byte>) : LingerOption =
        if bytes.Length <> 8 then
            failwith $"SocketOptionPal.decodeLingerOption: a LingerOption is 8 bytes, not %d{bytes.Length}"

        {
            OnOff = System.Buffers.Binary.BinaryPrimitives.ReadInt32LittleEndian (bytes.AsSpan().Slice (0, 4))
            Seconds = System.Buffers.Binary.BinaryPrimitives.ReadInt32LittleEndian (bytes.AsSpan().Slice (4, 4))
        }
