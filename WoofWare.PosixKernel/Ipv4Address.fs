namespace WoofWare.PosixKernel

/// <summary>
/// An IPv4 address, stored in host order.
/// The wire layout a process passes is network order.
/// </summary>
/// <remarks>
/// Every 32-bit value is an address, so the case is public: build one with <c>Ipv4Address 0x7F000001u</c>
/// and take it apart by matching on it.
/// </remarks>
/// <example>
/// <c>127.0.0.1</c> is <c>Ipv4Address 0x7F000001u</c>.
/// </example>
[<Struct>]
type Ipv4Address = | Ipv4Address of uint32

/// <summary>
/// An IPv4 transport endpoint: every field of <c>struct sockaddr_in</c> but the family and <c>sin_zero</c>.
/// </summary>
/// <remarks>
/// This is what <c>bind(2)</c> associates a socket with, and what <c>getsockname(2)</c> reports back.
///
/// Both fields are stored in host order.
/// The wire layout a process passes is network order.
/// </remarks>
type Ipv4Endpoint =
    {
        /// The address.
        Address : Ipv4Address
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
type Ipv4Prefix = private | Ipv4Prefix of network : Ipv4Address * bits : int

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
    | HostBitsSet of network : Ipv4Address * bits : int

[<RequireQualifiedAccess>]
module Ipv4Address =

    /// <summary>
    /// <c>INADDR_ANY</c>: the address a socket binds to mean "every local address".
    /// </summary>
    let any : Ipv4Address = Ipv4Address 0u

    /// <summary>
    /// <c>INADDR_LOOPBACK</c>: localhost via the loopback device.
    /// </summary>
    let loopback : Ipv4Address = Ipv4Address 0x7F000001u

    /// <summary>
    /// <c>INADDR_BROADCAST</c>: the limited broadcast address, every bit set.
    /// </summary>
    let broadcast : Ipv4Address = Ipv4Address System.UInt32.MaxValue

    /// <summary>
    /// Format this address as a human-readable dotted quad.
    /// </summary>
    /// <example>
    /// "192.168.0.1"
    /// </example>
    /// <remarks>
    /// Not a rendering any process sees. (Nothing in the emulated kernel formats an address as a
    /// string for a process to read.)
    /// </remarks>
    let toString (address : Ipv4Address) : string =
        let (Ipv4Address a) = address

        sprintf "%d.%d.%d.%d" ((a >>> 24) &&& 0xFFu) ((a >>> 16) &&& 0xFFu) ((a >>> 8) &&& 0xFFu) (a &&& 0xFFu)

[<RequireQualifiedAccess>]
module Ipv4Endpoint =

    let ofParts (address : Ipv4Address) (port : uint16) : Ipv4Endpoint =
        {
            Address = address
            Port = port
        }

    /// Whether the endpoint's address is the wildcard, <c>INADDR_ANY</c>.
    let isWildcard (endpoint : Ipv4Endpoint) : bool = endpoint.Address = Ipv4Address.any

    /// Do these two bindings claim any address in common? The wildcard covers
    /// every address, so it overlaps everything; two specific addresses overlap
    /// only when equal.
    ///
    /// This is the address half of a bind conflict, and deliberately not the
    /// whole of it: whether an overlap is *refused* depends on the flavour, on
    /// both sockets' `SO_REUSEADDR` and on whether either is listening. See
    /// `UnixSocket.bind`.
    let addressesOverlap (a : Ipv4Endpoint) (b : Ipv4Endpoint) : bool =
        isWildcard a || isWildcard b || a.Address = b.Address

    /// <summary>
    /// Format this endpoint as a human-readable dotted quad and port.
    /// </summary>
    /// <example>
    /// "192.168.0.1:8080"
    /// </example>
    /// <remarks>
    /// Not a rendering any process sees. (Nothing in the emulated kernel formats an address as a
    /// string for a process to read.)
    /// </remarks>
    let toString (endpoint : Ipv4Endpoint) : string =
        sprintf "%s:%d" (Ipv4Address.toString endpoint.Address) endpoint.Port

[<RequireQualifiedAccess>]
module Ipv4PrefixRefusal =
    /// What this library knows about why it refused the prefix, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : Ipv4PrefixRefusal) : string =
        match refusal with
        | Ipv4PrefixRefusal.LengthOutOfRange bits ->
            $"a prefix length of %d{bits} is not in [0, 32], the bits an IPv4 address has."
        | Ipv4PrefixRefusal.HostBitsSet (network, bits) ->
            $"%s{Ipv4Address.toString network}/%d{bits} has a bit set past its first %d{bits}, so it is not a network address of that length; Linux refuses such a route with EINVAL."

[<RequireQualifiedAccess>]
module Ipv4Prefix =

    /// <summary>
    /// The prefix whose first <c>bits</c> bits are those of <c>network</c>.
    /// </summary>
    /// <returns>
    /// A refusal if <c>bits</c> is outside <c>[0, 32]</c>, or if <c>network</c> has a bit set
    /// past its first <c>bits</c>: <c>127.0.0.1/8</c> is refused, and <c>127.0.0.0/8</c> is the prefix.
    /// </returns>
    let create (network : Ipv4Address) (bits : int) : Result<Ipv4Prefix, Ipv4PrefixRefusal> =
        let (Ipv4Address networkBits) = network

        if bits < 0 || bits > 32 then
            Error (Ipv4PrefixRefusal.LengthOutOfRange bits)
        // A shift by 32 is masked to a shift by 0 by the CLI, so length 32
        // (which has no host bits) cannot go through the shift.
        elif bits < 32 && (networkBits <<< bits) <> 0u then
            Error (Ipv4PrefixRefusal.HostBitsSet (network, bits))
        else
            Ok (Ipv4Prefix (network, bits))

    /// The network address. Has no bit set past the first `bits`.
    let network (prefix : Ipv4Prefix) : Ipv4Address =
        match prefix with
        | Ipv4Prefix (network, _) -> network

    /// How many leading bits the prefix fixes, in `[0, 32]`.
    let bits (prefix : Ipv4Prefix) : int =
        match prefix with
        | Ipv4Prefix (_, bits) -> bits

    /// <summary>
    /// <c>127.0.0.0/8</c>, loopback's network (<c>IN_LOOPBACKNET</c>).
    /// </summary>
    let loopbackNetwork : Ipv4Prefix = Ipv4Prefix (Ipv4Address 0x7F000000u, 8)

    /// <summary>
    /// True iff the given <c>address</c> has the given <c>prefix</c>.
    /// </summary>
    let contains (address : Ipv4Address) (prefix : Ipv4Prefix) : bool =
        match prefix with
        | Ipv4Prefix (Ipv4Address network, bits) ->

        // A shift by 32 is masked to a shift by 0 by the CLI, so length 0
        // cannot go through the shift.
        let mask =
            if bits = 0 then
                0u
            else
                System.UInt32.MaxValue <<< (32 - bits)

        let (Ipv4Address address) = address
        address &&& mask = network
