namespace WoofWare.PawPrint

/// An IPv4 transport endpoint: the pair `bind(2)` gives a socket and
/// `getsockname(2)` reports back.
///
/// Both fields are in *host* order. The wire layout a guest hands us is network
/// order, and converting once at the entry point keeps everything inside — the
/// wildcard test, the privileged-port comparison, the prefix match — written the
/// way the rule reads rather than byte-swapped.
///
/// IPv4 only, deliberately. `SocketPal.CreateSocket` sets `IPV6_V6ONLY` on every
/// non-raw `AF_INET6` socket and `SystemNative_SetSockOpt` is unimplemented, so
/// no managed guest can hold an IPv6 socket to bind; the entry points refuse one
/// loudly rather than model an address nothing can reach.
type InternetEndpoint =
    {
        /// The address, host order: `127.0.0.1` is `0x7F000001`.
        Address : uint32
        /// The port, host order.
        Port : uint16
    }

/// One IPv4 address this machine holds, with the prefix length it was assigned
/// with — `127.0.0.1/8`, as `ip addr` and `ifconfig` both print it.
///
/// Both halves are needed, because the flavours read them differently and each
/// reads only one. Darwin treats the *address* as the assigned one, so
/// `127.0.0.1` binds and `127.9.9.9` is `EADDRNOTAVAIL`; Linux treats the whole
/// *prefix* as local, so both bind. Recording only the address would make the
/// Linux rule unstateable, and recording only the network address would leave
/// Darwin unable to bind loopback at all.
type Ipv4InterfaceAddress =
    {
        /// The assigned address, host order: `127.0.0.1` is `0x7F000001`.
        Address : uint32
        /// The prefix length assigned with it, in `[0, 32]`.
        PrefixBits : int
    }

[<RequireQualifiedAccess>]
module InternetEndpoint =

    /// `INADDR_ANY`: the address a socket binds to mean "every local address".
    [<Literal>]
    let WildcardAddress = 0u

    /// `INADDR_LOOPBACK`.
    [<Literal>]
    let LoopbackAddress = 0x7F000001u

    let ofParts (address : uint32) (port : uint16) : InternetEndpoint =
        {
            Address = address
            Port = port
        }

    let isWildcard (endpoint : InternetEndpoint) : bool = endpoint.Address = WildcardAddress

    /// Do these two bindings claim any address in common? The wildcard covers
    /// every address, so it overlaps everything; two specific addresses overlap
    /// only when equal.
    ///
    /// This is the address half of a bind conflict, and deliberately not the
    /// whole of it: whether an overlap is *refused* depends on the flavour, on
    /// both sockets' `SO_REUSEADDR` and on whether either is listening. See
    /// `SimulatedUnixPlatform.bindConflict`.
    let addressesOverlap (a : InternetEndpoint) (b : InternetEndpoint) : bool =
        isWildcard a || isWildcard b || a.Address = b.Address

    /// Dotted quad, for a diagnostic. Not a guest-visible rendering: nothing in
    /// the emulated kernel formats an address for a guest to read.
    let toString (endpoint : InternetEndpoint) : string =
        let a = endpoint.Address

        sprintf
            "%d.%d.%d.%d:%d"
            ((a >>> 24) &&& 0xFFu)
            ((a >>> 16) &&& 0xFFu)
            ((a >>> 8) &&& 0xFFu)
            (a &&& 0xFFu)
            endpoint.Port

[<RequireQualifiedAccess>]
module Ipv4InterfaceAddress =

    let create (address : uint32) (prefixBits : int) : Ipv4InterfaceAddress =
        if prefixBits < 0 || prefixBits > 32 then
            failwith $"Ipv4InterfaceAddress.create: a prefix length of %d{prefixBits} is not in [0, 32]."

        {
            Address = address
            PrefixBits = prefixBits
        }

    /// Is `address` the one assigned here? Darwin's rule, which is why
    /// `127.0.0.1` binds there and `127.9.9.9` does not.
    let isAssigned (address : uint32) (assigned : Ipv4InterfaceAddress) : bool = assigned.Address = address

    /// Is `address` inside the prefix this address was assigned with? Linux's
    /// rule, which is why both of those bind there.
    let isWithinPrefix (address : uint32) (assigned : Ipv4InterfaceAddress) : bool =
        let mask =
            if assigned.PrefixBits = 0 then
                0u
            else
                System.UInt32.MaxValue <<< (32 - assigned.PrefixBits)

        (address &&& mask) = (assigned.Address &&& mask)
