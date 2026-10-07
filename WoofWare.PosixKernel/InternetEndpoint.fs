namespace WoofWare.PosixKernel

/// <summary>
/// An IPv4 transport endpoint.
/// </summary>
/// <remarks>
/// This is what <c>bind(2)</c> associates a socket with, and what <c>getsockname(2)</c> reports back.
///
/// Both fields are stored in host order.
/// The wire layout a process passes is network order.
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
/// The distinction is visible to a process.
/// Linux lets <c>bind(2)</c> take any address which Linux's routing machinery regards as
/// locally delivered: it consults its local routing table to determine this, and e.g.
/// <c>127.0.0.0/8</c> is in the local table by default, so Linux permits binding to <c>127.9.9.9</c>.
/// (It does <i>not</i> extend that to an interface's subnet. Having <c>192.168.1.10/24</c> assigned
/// to an interface doesn't make <c>192.168.1.11</c> bindable, because that is considered a
/// route to a <i>peer</i>, not to this machine.)
/// Darwin instead restricts binding to addresses assigned to the host.
///
/// Two prefixes are equal exactly when they name the same set of addresses: the
/// network address has no bit set past the prefix length.
///
/// Build one with <c>Ipv4Prefix.create</c>; read it with <c>Ipv4Prefix.network</c> and
/// <c>Ipv4Prefix.bits</c>. Its default value is <c>0.0.0.0/0</c>, which is a prefix
/// <c>create</c> admits, so every value of this type is one.
/// </remarks>
[<Struct>]
type Ipv4Prefix = private | Ipv4Prefix of network : uint32 * bits : int

/// Why `Ipv4Prefix.create` refuses a network address and length.
///
/// Linux refuses the same two for a route (`rtm_to_fib_config` in
/// `net/ipv4/fib_frontend.c`, `EINVAL`), so no routing table holds either.
[<RequireQualifiedAccess>]
type Ipv4PrefixRefusal =
    /// The length is outside `[0, 32]`: an IPv4 address has 32 bits, so no
    /// prefix fixes fewer than none or more than all of them. Contradictory.
    | LengthOutOfRange of bits : int
    /// The network address has a bit set past its first `bits`, so it is not
    /// the network of a `bits`-long prefix. Contradictory.
    | HostBitsSet of network : uint32 * bits : int

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
    /// Not a rendering any process sees. (Nothing in the emulated kernel formats an address as a
    /// string for a process to read.)
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
module Ipv4PrefixRefusal =
    let private dottedQuad (address : uint32) : string =
        sprintf
            "%d.%d.%d.%d"
            ((address >>> 24) &&& 0xFFu)
            ((address >>> 16) &&& 0xFFu)
            ((address >>> 8) &&& 0xFFu)
            (address &&& 0xFFu)

    /// What this library knows about why it refused the prefix, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : Ipv4PrefixRefusal) : string =
        match refusal with
        | Ipv4PrefixRefusal.LengthOutOfRange bits ->
            $"a prefix length of %d{bits} is not in [0, 32], the bits an IPv4 address has."
        | Ipv4PrefixRefusal.HostBitsSet (network, bits) ->
            $"%s{dottedQuad network}/%d{bits} has a bit set past its first %d{bits}, so it is not a network address of that length; Linux refuses such a route with EINVAL."

[<RequireQualifiedAccess>]
module Ipv4Prefix =

    /// <summary>
    /// The prefix whose first <c>bits</c> bits are those of <c>network</c> (host order).
    /// </summary>
    /// <returns>
    /// A refusal if <c>bits</c> is outside <c>[0, 32]</c>, or if <c>network</c> has a bit set
    /// past its first <c>bits</c>: <c>127.0.0.1/8</c> is refused, and <c>127.0.0.0/8</c> is the prefix.
    /// </returns>
    let create (network : uint32) (bits : int) : Result<Ipv4Prefix, Ipv4PrefixRefusal> =
        if bits < 0 || bits > 32 then
            Error (Ipv4PrefixRefusal.LengthOutOfRange bits)
        // A shift by 32 is masked to a shift by 0 by the CLI, so length 32
        // (which has no host bits) cannot go through the shift.
        elif bits < 32 && (network <<< bits) <> 0u then
            Error (Ipv4PrefixRefusal.HostBitsSet (network, bits))
        else
            Ok (Ipv4Prefix (network, bits))

    /// The network address, host order. Has no bit set past the first `bits`.
    let network (prefix : Ipv4Prefix) : uint32 =
        match prefix with
        | Ipv4Prefix (network, _) -> network

    /// How many leading bits the prefix fixes, in `[0, 32]`.
    let bits (prefix : Ipv4Prefix) : int =
        match prefix with
        | Ipv4Prefix (_, bits) -> bits

    /// <summary>
    /// <c>127.0.0.0/8</c>, loopback's network (<c>IN_LOOPBACKNET</c>).
    /// </summary>
    let loopbackNetwork : Ipv4Prefix = Ipv4Prefix (0x7F000000u, 8)

    /// <summary>
    /// True iff the given <c>address</c> has the given <c>prefix</c>.
    /// </summary>
    let contains (address : uint32) (prefix : Ipv4Prefix) : bool =
        match prefix with
        | Ipv4Prefix (network, bits) ->

        // A shift by 32 is masked to a shift by 0 by the CLI, so length 0
        // cannot go through the shift.
        let mask =
            if bits = 0 then
                0u
            else
                System.UInt32.MaxValue <<< (32 - bits)

        address &&& mask = network

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
