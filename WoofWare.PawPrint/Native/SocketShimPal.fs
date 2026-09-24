namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The four `sizeof`s `SystemNative_GetSocketAddressSizes` reports in one call,
/// which `System.Net.Primitives`' `SocketAddressPal` class initialiser latches
/// and every `SocketAddress` is then sized by.
///
/// Compile-time properties of the native shim rather than of any socket.
/// Measured with a `sizeof` probe compiled on macOS arm64 and
/// on Linux, rather than recalled; all four are invariant of pointer width, since
/// every member of these structs is fixed-width and the two variable-length tails
/// (`sun_path`, `sockaddr_storage`'s padding) are sized from a constant.
type SocketAddressSizes =
    {
        /// <summary>
        /// <c>sizeof(struct sockaddr_in)</c>.
        /// </summary>
        /// <example>16 on both Linux and Darwin.</example>
        InterNetwork : int
        /// <summary>
        /// <c>sizeof(struct sockaddr_in6)</c>.
        /// </summary>
        /// <example>28 on both Linux and Darwin.</example>
        InterNetworkV6 : int
        /// `sizeof(struct sockaddr_un)`. The one of the four that differs: 110 on
        /// Linux, whose `sun_path` is 108 bytes, against 106 on Darwin, whose is
        /// 104.
        UnixDomain : int
        /// `sizeof(struct sockaddr_storage)`. 128 on both, and the same number
        /// `SystemNative_GetMaximumAddressSize` reports through its own entry
        /// point — hence `SimulatedUnixPlatform.maximumSocketAddressSize` rather
        /// than a second literal.
        Storage : int
    }

/// Compile-time constants of the CoreCLR socket shim, which it reports through
/// entry points of their own.
[<RequireQualifiedAccess>]
module SocketShimPal =

    /// The sizes `SystemNative_GetSocketAddressSizes` reports. See
    /// `SocketAddressSizes` for where each number was measured.
    let socketAddressSizes (platform : SimulatedUnixPlatform) : SocketAddressSizes =
        {
            InterNetwork = SimulatedUnixPlatform.internetSocketAddressSize
            InterNetworkV6 = 28
            UnixDomain =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> 110
                | SimulatedUnixFlavour.Darwin -> 106
            Storage = SimulatedUnixPlatform.maximumSocketAddressSize
        }

    /// Whether this platform's sockets report IPv4 packet information on a
    /// dual-mode socket — an IPv6 socket receiving IPv4-mapped traffic. Reported
    /// to the guest by `SystemNative_PlatformSupportsDualModeIPv4PacketInfo`.
    ///
    /// A compile-time property of the native shim rather than of any socket:
    /// upstream the whole function body is
    /// `#if HAVE_SUPPORT_FOR_DUAL_MODE_IPV4_PACKET_INFO return 1 #else return 0`,
    /// and `configure.cmake` sets that define to 1 for every Linux target and
    /// leaves it 0 elsewhere. There is no probe of the running kernel involved, so
    /// this is not a fact about the machine but about which shim was built.
    ///
    /// (Linux includes Android here: the `NOT CLR_CMAKE_TARGET_ANDROID` test
    /// nested inside that `if` scopes only a `CMAKE_REQUIRED_LIBRARIES` setting,
    /// not the define.)
    ///
    /// Follows the flavour rather than conservatively reporting `false`
    /// everywhere, because both of CoreLib's readers of it are guest-visible
    /// control flow (see the handler arm for which): answering `false` while
    /// impersonating Linux makes a guest see a `PlatformNotSupportedException`
    /// real Linux does not raise, and does so silently, with no abort and no
    /// diagnostic.
    ///
    /// Answering `true` carries an obligation for whoever implements the socket
    /// emulation this leads on to: a Linux-flavour `recvmsg` on a dual-mode
    /// socket must actually produce the IPv4 `pktinfo` control message, because
    /// CoreLib latches this once per process and will thereafter ask for the
    /// packet information and expect to be given it. Reporting support and then
    /// handing back a default `IPPacketInformation` would be the data-level
    /// version of the lie this function exists to avoid.
    let supportsDualModeIPv4PacketInfo (platform : SimulatedUnixPlatform) : bool =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false
