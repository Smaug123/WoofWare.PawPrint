namespace WoofWare.PosixKernel

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

/// A route this machine has to a range of IPv4 addresses, as Linux's local
/// routing table holds them.
///
/// Distinct from an *assigned* address, and the distinction is guest-visible.
/// Linux lets `bind(2)` take any address it has a local route to, which is why
/// `127.9.9.9` binds there: `127.0.0.0/8` is in the local table. It does **not**
/// extend that to an interface's subnet — holding `192.168.1.10/24` does not
/// make `192.168.1.11` bindable, because that is a route to a *peer*, not to
/// this machine. Darwin takes only assigned addresses either way.
type Ipv4Prefix =
    {
        /// The network address, host order.
        Network : uint32
        /// How many leading bits the prefix fixes, in `[0, 32]`.
        Bits : int
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
module Ipv4Prefix =

    let create (network : uint32) (bits : int) : Ipv4Prefix =
        if bits < 0 || bits > 32 then
            failwith $"Ipv4Prefix.create: a prefix length of %d{bits} is not in [0, 32]."

        {
            Network = network
            Bits = bits
        }

    /// Rejects a prefix whose fields were built by hand rather than through
    /// `create` — the record is public, so `{ Network = x ; Bits = 99 }` is
    /// representable, and a shift count outside [0, 32] is masked by the CLI
    /// rather than faulting, which would silently produce an unrelated mask.
    let assertValid (context : string) (prefix : Ipv4Prefix) : Ipv4Prefix =
        if prefix.Bits < 0 || prefix.Bits > 32 then
            failwith $"%s{context}: a prefix length of %d{prefix.Bits} is not in [0, 32]."

        prefix

    let contains (address : uint32) (prefix : Ipv4Prefix) : bool =
        let mask =
            if prefix.Bits = 0 then
                0u
            else
                System.UInt32.MaxValue <<< (32 - prefix.Bits)

        (address &&& mask) = (prefix.Network &&& mask)

/// One TCP connection, as the emulated kernel's connection table holds it.
///
/// Keyed by `ConnectionId` and holding only the two endpoints' addresses.
/// Deliberately no references back to the sockets on its ends: a connection
/// outlives the client that opened it (measured: close the client while its
/// connection sits in an accept queue, and `accept(2)` still returns it), and
/// the server end has no socket at all until that accept, so an end-to-socket
/// field would spend most of its life dangling or `None`. Cleanup instead
/// scans the socket table for references, which `EmulatedKernel.closeFd`
/// does.
type TcpConnection =
    {
        /// The connecting side's address — what `accept(2)` reports as the
        /// peer.
        ClientAddress : InternetEndpoint
        /// The accepted side's address: the destination the client connected
        /// to, with a wildcard destination already rewritten to loopback. The
        /// accepted socket's own `getsockname(2)` reports this.
        ServerAddress : InternetEndpoint
    }
