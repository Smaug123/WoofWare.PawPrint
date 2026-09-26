namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// Which of the native shim's argument screens refused a `SystemNative_Socket`
/// before any kernel was asked.
[<RequireQualifiedAccess>]
type SocketArgumentScreen =
    /// The shim's address-family conversion has no case for this value, so
    /// it returns `Error_EAFNOSUPPORT`.
    | AddressFamily
    /// The shim's socket-type conversion has no case for this value:
    /// `Error_EPROTOTYPE`. Note that is the *shim's* choice of errno; a
    /// kernel asked the same question would say `ESOCKTNOSUPPORT` (Linux) or
    /// `EPROTONOSUPPORT` (Darwin).
    | SocketType
    /// The shim's protocol conversion has no case for this value *in this
    /// address family*: `Error_EPROTONOSUPPORT`. Per-family, so the same
    /// protocol number can convert under one family and be refused under
    /// another.
    | Protocol

[<RequireQualifiedAccess>]
module SocketArgumentScreen =
    /// The error the shim returns for this screen.
    let error (screen : SocketArgumentScreen) : UnixError =
        match screen with
        | SocketArgumentScreen.AddressFamily -> UnixError.EAFNOSUPPORT
        | SocketArgumentScreen.SocketType -> UnixError.EPROTOTYPE
        | SocketArgumentScreen.Protocol -> UnixError.EPROTONOSUPPORT


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
/// What *is* a fact about the kernel stays in the library: past the screens,
/// this module converts a PAL triple to the platform's own numbers, which a
/// caller hands to `UnixSocket.socket` as the shim hands them to `socket(2)`.
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

    /// `TryConvertSocketTypePalToPlatform` (`pal_networking.c:2497`): the
    /// platform `SOCK_*` this PAL socket type names, or `None` where the shim's
    /// switch has no case for it.
    ///
    /// Every arm is `#ifdef`-guarded on a `SOCK_*` symbol, but both flavours
    /// define all five, and number them alike and as the PAL does.
    let socketTypePalToPlatform (palSocketType : int) : int option =
        match palSocketType with
        | Pal.SockStream -> Some 1
        | Pal.SockDgram -> Some 2
        | Pal.SockRaw -> Some 3
        | Pal.SockRdm -> Some 4
        | Pal.SockSeqPacket -> Some 5
        | _ -> None

    /// `TryConvertProtocolTypePalToPlatform` (`pal_networking.c:2535`): the
    /// platform protocol number this PAL protocol names *in this PAL address
    /// family*, or `None` where the shim's table for that family has no case
    /// for it.
    ///
    /// The `IPPROTO_*` numbers are the same on both flavours, and mostly the
    /// PAL's own; the exceptions are `PT_ICMP` under `AF_INET6`, which the shim
    /// sends as `IPPROTO_ICMPV6`, and `PT_RAW` under `AF_CAN`, which is
    /// `CAN_RAW`.
    let protocolTypePalToPlatform (palAddressFamily : int) (palProtocolType : int) : int option =
        match palAddressFamily with
        // The `AF_PACKET` arm passes the number straight through as an IEEE
        // 802.3 protocol in network order, so every value converts. Only
        // reachable on Linux: on Darwin the address-family screen refuses
        // `AF_PACKET` first.
        | Pal.AfPacket -> Some palProtocolType
        // `#if HAVE_LINUX_CAN_H` — a `check_include_files` probe of the
        // *shim's* build host (`configure.cmake:970`) rather than of any
        // kernel. PawPrint models the header as present, which is what an
        // official linux-x64 build has. Were it absent, this arm would
        // vanish and every `AF_CAN` protocol would be refused below.
        | Pal.AfCan ->
            match palProtocolType with
            | Pal.PtUnspecified -> Some 0
            | Pal.PtRaw -> Some 1
            | _ -> None
        | Pal.AfInet ->
            match palProtocolType with
            | Pal.PtUnspecified -> Some 0
            | Pal.PtIcmp -> Some 1
            | Pal.PtTcp -> Some 6
            | Pal.PtUdp -> Some 17
            | Pal.PtIgmp -> Some 2
            | Pal.PtRaw -> Some 255
            | _ -> None
        | Pal.AfInet6 ->
            match palProtocolType with
            | Pal.PtUnspecified -> Some 0
            | Pal.PtIcmpV6
            | Pal.PtIcmp -> Some 58
            | Pal.PtTcp -> Some 6
            | Pal.PtUdp -> Some 17
            | Pal.PtIgmp -> Some 2
            | Pal.PtRaw -> Some 255
            | Pal.PtDstOpts -> Some 60
            | Pal.PtNone -> Some 59
            | Pal.PtRouting -> Some 43
            | Pal.PtFragment -> Some 44
            | _ -> None
        // `AF_UNSPEC` and `AF_UNIX` share the C's `default` arm, which
        // accepts the unspecified protocol and nothing else.
        | _ ->
            match palProtocolType with
            | Pal.PtUnspecified -> Some 0
            | _ -> None

    /// Linux's `SOCK_CLOEXEC`, which `SystemNative_Socket` ORs into every type
    /// under `#ifdef SOCK_CLOEXEC`. Darwin's headers do not define it, so there
    /// the shim sets `FD_CLOEXEC` with a separate `fcntl` after the call, which
    /// has nothing to change in a kernel that models no `exec`.
    [<Literal>]
    let private LinuxSockCloExec = 0x80000

    /// The `socket(2)` arguments `SystemNative_Socket` passes for a domain, type
    /// and protocol in the PAL numbering, or the screen that refuses them first.
    ///
    /// The screens are `TryConvertAddressFamilyPalToPlatform`,
    /// `TryConvertSocketTypePalToPlatform` and
    /// `TryConvertProtocolTypePalToPlatform`, applied in the order
    /// `SystemNative_Socket` applies them. They are pure C running before any
    /// syscall, so they are exactly knowable, and their flavour-dependence is
    /// the shim's `#ifdef`s rather than any kernel's behaviour.
    let socketArguments
        (platform : SimulatedUnixPlatform)
        (palAddressFamily : int)
        (palSocketType : int)
        (palProtocolType : int)
        : Result<int * int * int, SocketArgumentScreen>
        =
        // `TryConvertAddressFamilyPalToPlatform`, which is
        // `addressFamilyPalToPlatform` above — the same C function screens
        // `SystemNative_Socket`'s first argument and converts
        // `SystemNative_SetAddressFamily`'s, so there is one rule here, not two.
        match addressFamilyPalToPlatform platform palAddressFamily with
        | None -> Error SocketArgumentScreen.AddressFamily
        | Some domain ->

        match socketTypePalToPlatform palSocketType with
        | None -> Error SocketArgumentScreen.SocketType
        | Some socketType ->

        match protocolTypePalToPlatform palAddressFamily palProtocolType with
        | None -> Error SocketArgumentScreen.Protocol
        | Some protocol ->

        let socketType =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> socketType ||| LinuxSockCloExec
            | SimulatedUnixFlavour.Darwin -> socketType

        Ok (domain, socketType, protocol)
