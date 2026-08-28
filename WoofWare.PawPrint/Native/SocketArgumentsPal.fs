namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// Why `socketCreation` would not hand back a socket.
[<RequireQualifiedAccess>]
type SocketCreationRefusal =
    /// The shim's address-family conversion has no case for this value, so
    /// it returns `Error_EAFNOSUPPORT` without reaching `socket(2)`.
    | AddressFamily
    /// The shim's socket-type conversion has no case for this value:
    /// `Error_EPROTOTYPE`. Note that is the *shim's* choice of errno; a
    /// kernel asked the same question would say `ESOCKTNOSUPPORT`.
    | SocketType
    /// The shim's protocol conversion has no case for this value *in this
    /// address family*: `Error_EPROTONOSUPPORT`. Per-family, so the same
    /// protocol number can convert under one family and be refused under
    /// another.
    | Protocol
    /// Every one of the shim's screens passed, so a real run would reach
    /// `socket(2)` — and PawPrint has not decided what this socket is. Not
    /// an errno: there is nothing truthful to report.
    | Unmodelled


/// The BCL's `AddressFamily`/`SocketType`/`ProtocolType` numbering
/// (`pal_networking.h`), and the argument screens the socket shims apply across
/// it before any syscall runs.
///
/// This is PawPrint's half of the socket-creation boundary, as `UnixErrorPal`
/// is its half of the errno one. The numbering is .NET's own — upstream chose
/// values that coincide with no kernel's, precisely so the shim has to
/// translate — and the screens are pure C that runs in user space, so both are
/// exactly knowable and neither is a fact about any kernel.
///
/// What *is* a fact about the kernel stays in the library:
/// `SimulatedUnixPlatform.creatableSockets` says which sockets it makes, and
/// this module's job past the screens is only to name a PAL triple in that
/// set's vocabulary.
///
/// Named for the arguments rather than for the shim, because CoreLib has a
/// managed `System.Net.Sockets.SocketPal` of its own that several comments in
/// `NativeSystemNative` already cite, and it is a layer above this one.
[<RequireQualifiedAccess>]
module SocketArgumentsPal =

    /// The PAL numbering `SystemNative_Socket`'s three arguments arrive in
    /// (`AddressFamily`, `SocketType` and `ProtocolType` in `pal_networking.h`).
    /// Platform-independent by construction: upstream chose values that do not
    /// coincide with any kernel's, precisely so the shim has to translate.
    [<RequireQualifiedAccess>]
    module private Pal =
        [<Literal>]
        let AfUnspec = 0

        [<Literal>]
        let AfUnix = 1

        [<Literal>]
        let AfInet = 2

        [<Literal>]
        let AfInet6 = 23

        [<Literal>]
        let AfPacket = 65536

        [<Literal>]
        let AfCan = 65537

        [<Literal>]
        let SockStream = 1

        [<Literal>]
        let SockDgram = 2

        [<Literal>]
        let SockRaw = 3

        [<Literal>]
        let SockRdm = 4

        [<Literal>]
        let SockSeqPacket = 5

        [<Literal>]
        let PtUnspecified = 0

        [<Literal>]
        let PtIcmp = 1

        [<Literal>]
        let PtIgmp = 2

        [<Literal>]
        let PtTcp = 6

        [<Literal>]
        let PtUdp = 17

        [<Literal>]
        let PtRouting = 43

        [<Literal>]
        let PtFragment = 44

        [<Literal>]
        let PtIcmpV6 = 58

        [<Literal>]
        let PtNone = 59

        [<Literal>]
        let PtDstOpts = 60

        [<Literal>]
        let PtRaw = 255

    /// `TryConvertAddressFamilyPalToPlatform` (`pal_networking.c:218`): the
    /// platform `AF_*` this PAL address family names, or `None` where the shim's
    /// switch has no case for it.
    ///
    /// `None` is not the same as "refuse". Upstream the failing branch still
    /// stores `(sa_family_t) palAddressFamily` — truncated to
    /// `SockaddrFamilyField.width` — through the out-parameter before returning
    /// false, so a caller that writes the family into a blob writes a truncated
    /// value there *and* reports `EAFNOSUPPORT`. Callers must reproduce both
    /// halves; see the `SystemNative_SetAddressFamily` handler.
    ///
    /// `AF_PACKET` and `AF_CAN` are the only flavour-dependent arms, and their
    /// dependence is the shim's `#ifdef`s rather than any kernel's: Linux's
    /// headers define the symbols (17 and 29, measured) and Darwin's do not, so
    /// on Darwin those two arms are not compiled and the value falls to the
    /// default.
    let addressFamilyPalToPlatform (platform : SimulatedUnixPlatform) (palAddressFamily : int) : int option =
        let isLinux =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        match palAddressFamily with
        | Pal.AfUnspec -> Some 0
        | Pal.AfUnix -> Some 1
        | Pal.AfInet -> Some SimulatedUnixPlatform.internetAddressFamily
        | Pal.AfInet6 -> Some (SimulatedUnixPlatform.internetV6AddressFamily platform)
        | Pal.AfPacket -> if isLinux then Some 17 else None
        | Pal.AfCan -> if isLinux then Some 29 else None
        | _ -> None

    /// `TryConvertAddressFamilyPlatformToPal` (`pal_networking.c:184`), the
    /// inverse of `addressFamilyPalToPlatform` over exactly the same rows.
    ///
    /// `None` where the switch has no case. Upstream's failing branch copies the
    /// platform number through unconverted, but `SystemNative_GetAddressFamily`
    /// — its only caller that a guest can reach — overwrites that with
    /// `AddressFamily_AF_UNKNOWN` and still reports success, so the unconverted
    /// value never escapes and this returns no analogue of it.
    let addressFamilyPlatformToPal (platform : SimulatedUnixPlatform) (platformAddressFamily : int) : int option =
        let isLinux =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        match platformAddressFamily with
        | 0 -> Some Pal.AfUnspec
        | 1 -> Some Pal.AfUnix
        | family when family = SimulatedUnixPlatform.internetAddressFamily -> Some Pal.AfInet
        | family when family = SimulatedUnixPlatform.internetV6AddressFamily platform -> Some Pal.AfInet6
        | 17 -> if isLinux then Some Pal.AfPacket else None
        | 29 -> if isLinux then Some Pal.AfCan else None
        | _ -> None

    /// Is this the PAL protocol type `SystemNative_Bind` sets `SO_REUSEADDR`
    /// for? The C keys on its own `protocolType` *argument* being `PT_TCP`
    /// (`pal_networking.c:1770`), not on the socket's protocol, so this asks
    /// about the argument.
    let isTcpProtocolType (palProtocolType : int) : bool = palProtocolType = Pal.PtTcp

    /// The `SocketDomain`/`SocketKind`/`SocketProtocol` a PAL triple names, or
    /// `None` where it names none.
    ///
    /// Partial on every axis, and the shapes it refuses are *not* the ones a
    /// screen refuses: `AF_UNSPEC`, `AF_PACKET`, `AF_CAN`, `SOCK_RDM` and every
    /// protocol but the three modelled ones all convert in their screen and
    /// still have no word in the library's vocabulary. Whether that is the
    /// reason a triple was refused is what a client should ask before writing a
    /// diagnostic, because it wants a different fix from a shape the kernel's
    /// table merely omits.
    let shapeOf
        (palAddressFamily : int)
        (palSocketType : int)
        (palProtocolType : int)
        : (SocketDomain * SocketKind * SocketProtocol) option
        =
        let domain =
            match palAddressFamily with
            | Pal.AfInet -> Some SocketDomain.InterNetwork
            | Pal.AfInet6 -> Some SocketDomain.InterNetworkV6
            | Pal.AfUnix -> Some SocketDomain.Unix
            | _ -> None

        let kind =
            match palSocketType with
            | Pal.SockStream -> Some SocketKind.Stream
            | Pal.SockDgram -> Some SocketKind.Datagram
            | Pal.SockRaw -> Some SocketKind.Raw
            | Pal.SockSeqPacket -> Some SocketKind.SeqPacket
            | _ -> None

        let protocol =
            match palProtocolType with
            | Pal.PtUnspecified -> Some SocketProtocol.Unspecified
            | Pal.PtTcp -> Some SocketProtocol.Tcp
            | Pal.PtUdp -> Some SocketProtocol.Udp
            | _ -> None

        match domain, kind, protocol with
        | Some domain, Some kind, Some protocol -> Some (domain, kind, protocol)
        | _ -> None

    /// What `SystemNative_Socket` does with a domain, type and protocol, all in
    /// the PAL numbering its caller supplies them in.
    ///
    /// Three of the four answers are this shim's own screens, transcribed from
    /// `TryConvertAddressFamilyPalToPlatform`,
    /// `TryConvertSocketTypePalToPlatform` and
    /// `TryConvertProtocolTypePalToPlatform` (`pal_networking.c:218`, `:2497`,
    /// `:2535`) and applied in the order `SystemNative_Socket` applies them.
    /// They are pure C running before any syscall, so they are exactly
    /// knowable, and their flavour-dependence is the shim's `#ifdef`s rather
    /// than any kernel's behaviour.
    ///
    /// The fourth, `Unmodelled`, stands where the kernel's answer would be, and
    /// is `SimulatedUnixPlatform.creatableSockets` — which says why that set is
    /// as small as it is. Reaching it means every screen has passed, so a real
    /// run would now call `socket(2)`.
    ///
    /// A triple `shapeOf` cannot name answers `Unmodelled` too, and that is a
    /// different thing from a shape the kernel's table omits. The distinction
    /// is deliberately not in this type: nothing maps `Unmodelled` to an errno,
    /// so it costs a caller only which diagnostic it writes, and `shapeOf` is
    /// public so that a caller who cares can ask.
    let socketCreation
        (platform : SimulatedUnixPlatform)
        (palAddressFamily : int)
        (palSocketType : int)
        (palProtocolType : int)
        : Result<SocketDomain * SocketKind * SocketProtocol, SocketCreationRefusal>
        =
        // `TryConvertAddressFamilyPalToPlatform`, which is
        // `addressFamilyPalToPlatform` above — the same C function screens
        // `SystemNative_Socket`'s first argument and converts
        // `SystemNative_SetAddressFamily`'s, so there is one rule here, not two.
        // Only whether it converts matters to this caller; the number it converts
        // to is a socket address's business.
        let familyConverts = (addressFamilyPalToPlatform platform palAddressFamily).IsSome

        if not familyConverts then
            Error SocketCreationRefusal.AddressFamily
        else

        // `TryConvertSocketTypePalToPlatform`. Every arm is `#ifdef`-guarded on a
        // `SOCK_*` symbol, but both flavours define all five, so this screen
        // takes no flavour and fires only for a value outside the enum.
        let typeConverts =
            match palSocketType with
            | Pal.SockStream
            | Pal.SockDgram
            | Pal.SockRaw
            | Pal.SockRdm
            | Pal.SockSeqPacket -> true
            | _ -> false

        if not typeConverts then
            Error SocketCreationRefusal.SocketType
        else

        // `TryConvertProtocolTypePalToPlatform`, whose table is per address
        // family. Only the *converts or not* answer matters here: the platform
        // protocol number it produces can differ from the PAL one it was given
        // (`AF_INET6` with `PT_ICMP` becomes `IPPROTO_ICMPV6`), and it is the PAL
        // value that is worth keeping.
        let protocolConverts =
            match palAddressFamily with
            // The `AF_PACKET` arm passes the number straight through as an IEEE
            // 802.3 protocol in network order, so every value converts.
            | Pal.AfPacket -> true
            // `#if HAVE_LINUX_CAN_H` — a `check_include_files` probe of the
            // *shim's* build host (`configure.cmake:970`) rather than of any
            // kernel. PawPrint models the header as present, which is what an
            // official linux-x64 build has. Were it absent, this arm would
            // vanish and every `AF_CAN` protocol would be refused below.
            | Pal.AfCan ->
                match palProtocolType with
                | Pal.PtUnspecified
                | Pal.PtRaw -> true
                | _ -> false
            | Pal.AfInet ->
                match palProtocolType with
                | Pal.PtUnspecified
                | Pal.PtIcmp
                | Pal.PtTcp
                | Pal.PtUdp
                | Pal.PtIgmp
                | Pal.PtRaw -> true
                | _ -> false
            | Pal.AfInet6 ->
                match palProtocolType with
                | Pal.PtUnspecified
                | Pal.PtIcmpV6
                | Pal.PtIcmp
                | Pal.PtTcp
                | Pal.PtUdp
                | Pal.PtIgmp
                | Pal.PtRaw
                | Pal.PtDstOpts
                | Pal.PtNone
                | Pal.PtRouting
                | Pal.PtFragment -> true
                | _ -> false
            // `AF_UNSPEC` and `AF_UNIX` share the C's `default` arm, which
            // accepts the unspecified protocol and nothing else.
            | _ ->
                match palProtocolType with
                | Pal.PtUnspecified -> true
                | _ -> false

        if not protocolConverts then
            Error SocketCreationRefusal.Protocol
        else

        // Past every screen the shim applies, so a real run would now call
        // `socket(2)`, and what this kernel creates is `creatableSockets`.
        match shapeOf palAddressFamily palSocketType palProtocolType with
        | Some shape when Set.contains shape (SimulatedUnixPlatform.creatableSockets platform) -> Ok shape
        | _ -> Error SocketCreationRefusal.Unmodelled
