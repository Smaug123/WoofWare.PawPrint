namespace WoofWare.PosixKernel

/// <summary>
/// An IPv4 transport endpoint.
/// </summary>
/// <remarks>
/// This is what <c>bind(2)</c> associates a socket with, and what <c>getsockname(2)</c> reports back.
///
/// Both fields are stored in host order.
/// The wire layout which we get from a guest is network order.
///
/// We currently only model IPv4.
/// </remarks>
type InternetEndpoint =
    {
        /// <summary>
        /// The address, host order.
        /// </summary>
        /// <example>
        /// <c>127.0.0.1</c> is <c>0x7F000001</c>.
        /// </example>
        Address : uint32
        /// The port, host order.
        Port : uint16
    }

/// <summary>
/// An IPv4 prefix that Linux considers local to this host, as represented by a route of type <c>local</c>
/// in Linux's local routing table.
/// </summary>
/// <remarks>
/// Not the same thing as the prefix length attached to an interface address assignment.
///
/// The distinction is guest-visible.
/// Linux lets <c>bind(2)</c> take any address which Linux's routing machinery regards as
/// locally delivered: it consults its local routing table to determine this, and e.g.
/// <c>127.0.0.0/8</c> is in the local table by default, so Linux permits binding to <c>127.9.9.9</c>.
/// (It does <i>not</i> extend that to an interface's subnet. Having <c>192.168.1.10/24</c> assigned
/// to an interface doesn't make <c>192.168.1.11</c> bindable, because that is considered a
/// route to a <i>peer</i>, not to this machine.)
/// Darwin instead restricts binding to addresses assigned to the host.
/// </remarks>
type Ipv4Prefix =
    {
        /// <summary>
        /// The network address, host order.
        /// </summary>
        /// <example>
        /// <c>127.0.0.1</c> is <c>0x7F000001</c>.
        /// </example>
        Network : uint32
        /// <summary>How many leading bits the prefix fixes, in <c>[0, 32]</c>.</summary>
        Bits : int
    }

[<RequireQualifiedAccess>]
module InternetEndpoint =

    /// <summary>
    /// <c>INADDR_ANY</c>: the address a socket binds to mean "every local address".
    /// </summary>
    [<Literal>]
    let WildcardAddress = 0u

    /// <summary>
    /// <c>INADDR_LOOPBACK</c>: localhost via the loopback device.
    /// </summary>
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

    /// <summary>
    /// Format this endpoint as a human-readable dotted quad.
    /// </summary>
    /// <example>
    /// "192.168.0.1:8080"
    /// </example>
    /// <remarks>
    /// Not a guest-visible rendering. (Nothing in the emulated kernel formats an address as a string
    /// for a guest to read.)
    /// </remarks>
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

    /// <summary>
    /// Assemble an IPv4 prefix from its parts.
    /// </summary>
    /// <returns>
    /// Throws if the <c>bits</c> count is outside <c>[0, 32]</c>.
    /// </returns>
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

    /// <summary>
    /// True iff the given <c>address</c> has the given <c>prefix</c>.
    /// </summary>
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
/// scans the socket table for references, which `UnixDescriptor.close` does.
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
