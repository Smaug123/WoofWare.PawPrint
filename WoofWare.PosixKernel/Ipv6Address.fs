namespace WoofWare.PosixKernel

open System
open System.Buffers.Binary

/// <summary>
/// An IPv6 address: the 128 bits of <c>sin6_addr</c>.
/// </summary>
/// <remarks>
/// Every 128-bit value is an address, and so is the default value, which is <c>::</c>.
/// Two addresses are equal exactly when their bytes are, and they are ordered as their
/// bytes are, read as one big-endian number.
///
/// Build one with <c>Ipv6Address.ofBytes</c>, <c>Ipv6Address.v4Mapped</c>, or take
/// <c>Ipv6Address.loopback</c> or <c>Ipv6Address.any</c>; read it with
/// <c>Ipv6Address.toBytes</c>, <c>Ipv6Address.tryV4Mapped</c> and <c>Ipv6Address.toString</c>.
/// </remarks>
[<Struct>]
type Ipv6Address = private | Ipv6Address of high : uint64 * low : uint64

/// <summary>
/// An IPv6 transport endpoint: every field of <c>struct sockaddr_in6</c> but the family
/// and Darwin's <c>sin6_len</c>.
/// </summary>
/// <remarks>
/// Each field is held as a number, not in the byte order the struct stores it in.
/// </remarks>
type Ipv6Endpoint =
    {
        /// <summary><c>sin6_addr</c>.</summary>
        Address : Ipv6Address
        /// <summary><c>sin6_port</c>, as a number (the struct holds it in network order).</summary>
        Port : uint16
        /// <summary><c>sin6_flowinfo</c>, as a number (the struct holds it in network order).</summary>
        FlowInfo : uint32
        /// <summary>
        /// <c>sin6_scope_id</c>: which interface a scoped (for instance link-local) address is
        /// on, or 0 for none. The struct holds it in the machine's own byte order.
        /// </summary>
        ScopeId : uint32
    }

[<RequireQualifiedAccess>]
module Ipv6Address =

    /// <summary>
    /// The address whose sixteen bytes, in network order (as <c>sin6_addr</c> holds them),
    /// are <c>bytes</c>.
    /// </summary>
    /// <returns><c>None</c> if <c>bytes</c> is not exactly sixteen bytes long.</returns>
    let ofBytes (bytes : ReadOnlySpan<byte>) : Ipv6Address option =
        if bytes.Length <> 16 then
            None
        else
            Some (
                Ipv6Address (
                    BinaryPrimitives.ReadUInt64BigEndian (bytes.Slice (0, 8)),
                    BinaryPrimitives.ReadUInt64BigEndian (bytes.Slice (8, 8))
                )
            )

    /// <summary>
    /// The address's sixteen bytes in network order, as <c>sin6_addr</c> holds them, in an
    /// array of the caller's own.
    /// </summary>
    let toBytes (address : Ipv6Address) : byte[] =
        match address with
        | Ipv6Address (high, low) ->
            let bytes = Array.zeroCreate<byte> 16
            BinaryPrimitives.WriteUInt64BigEndian (Span<byte> (bytes, 0, 8), high)
            BinaryPrimitives.WriteUInt64BigEndian (Span<byte> (bytes, 8, 8), low)
            bytes

    /// <summary><c>::</c> (<c>in6addr_any</c>): the address a socket binds to mean "every local address".</summary>
    let any : Ipv6Address = Ipv6Address (0UL, 0UL)

    /// <summary><c>::1</c> (<c>in6addr_loopback</c>).</summary>
    let loopback : Ipv6Address = Ipv6Address (0UL, 1UL)

    /// <summary>
    /// <c>::ffff:a.b.c.d</c>, the IPv4 address <c>address</c> mapped into IPv6.
    /// </summary>
    let v4Mapped (address : Ipv4Address) : Ipv6Address =
        let (Ipv4Address bits) = address
        Ipv6Address (0UL, 0x0000FFFF00000000UL ||| uint64 bits)

    /// <summary>
    /// The IPv4 address that <c>address</c> maps, if it is <c>::ffff:a.b.c.d</c>.
    /// </summary>
    /// <returns><c>None</c> for any address outside <c>::ffff:0:0/96</c>.</returns>
    let tryV4Mapped (address : Ipv6Address) : Ipv4Address option =
        match address with
        | Ipv6Address (0UL, low) when low >>> 32 = 0xFFFFUL -> Some (Ipv4Address (uint32 low))
        | Ipv6Address _ -> None

    /// `::a.b.c.d`, the deprecated IPv4-compatible form (RFC 4291 §2.5.5.1)
    /// of the IPv4 `address`.
    let internal v4Compatible (address : Ipv4Address) : Ipv6Address =
        let (Ipv4Address bits) = address
        Ipv6Address (0UL, uint64 bits)

    /// The eight 16-bit groups of the text form, most significant first.
    let private groups (address : Ipv6Address) : uint16[] =
        match address with
        | Ipv6Address (high, low) ->
            Array.init
                8
                (fun i ->
                    let word = if i < 4 then high else low
                    uint16 (word >>> (16 * (3 - i % 4)))
                )

    /// Whether the text form writes the last 32 bits as a dotted quad: the
    /// IPv4-compatible `::a.b.c.d` and mapped `::ffff:a.b.c.d` (the last 32
    /// bits other than `0.0.0.x`), the IPv4-translated `::ffff:0:a.b.c.d`, and
    /// ISATAP's `x:x:x:x:0:5efe:a.b.c.d`. These are .NET's choices
    /// (`IPv6AddressHelper.ShouldHaveIpv4Embedded`), which RFC 5952 §5 leaves
    /// to the formatter.
    let private embedsIpv4 (g : uint16[]) : bool =
        let leadingZero = g.[0] = 0us && g.[1] = 0us && g.[2] = 0us && g.[3] = 0us

        let compatibleMappedOrTranslated =
            leadingZero
            && g.[6] <> 0us
            && ((g.[4] = 0us && (g.[5] = 0us || g.[5] = 0xFFFFus))
                || (g.[4] = 0xFFFFus && g.[5] = 0us))

        compatibleMappedOrTranslated || (g.[4] = 0us && g.[5] = 0x5EFEus)

    /// `groups` in RFC 5952 form: lower-case hex without leading zeros, and
    /// the longest run of two or more zero groups (the first, on a tie)
    /// written as `::`.
    let private writeGroups (g : uint16[]) : string =
        let hex (group : uint16) : string = group.ToString "x"

        // The longest run of zero groups, as (start, length).
        let mutable best = (0, 0)
        let mutable runStart = 0
        let mutable runLength = 0

        for i in 0 .. g.Length - 1 do
            if g.[i] = 0us then
                if runLength = 0 then
                    runStart <- i

                runLength <- runLength + 1

                if runLength > snd best then
                    best <- (runStart, runLength)
            else
                runLength <- 0

        match best with
        | start, length when length >= 2 ->
            let before = g.[.. start - 1] |> Array.map hex |> String.concat ":"
            let after = g.[start + length ..] |> Array.map hex |> String.concat ":"
            before + "::" + after
        | _ -> g |> Array.map hex |> String.concat ":"

    /// <summary>
    /// The address as text, in RFC 5952's form: for instance <c>2001:db8::1</c>,
    /// <c>::1</c>, or <c>::ffff:127.0.0.1</c>.
    /// </summary>
    /// <remarks>
    /// The same text as .NET's <c>System.Net.IPAddress.ToString</c> for an address of scope 0,
    /// including which addresses have their last 32 bits written as a dotted quad.
    ///
    /// Not a rendering any process sees. (Nothing in the emulated kernel formats an address as a
    /// string for a process to read.)
    /// </remarks>
    let toString (address : Ipv6Address) : string =
        let g = groups address

        if embedsIpv4 g then
            let head = writeGroups g.[..5]
            let separator = if head.EndsWith ':' then "" else ":"

            let quad =
                $"%d{g.[6] >>> 8}.%d{g.[6] &&& 0xFFus}.%d{g.[7] >>> 8}.%d{g.[7] &&& 0xFFus}"

            head + separator + quad
        else
            writeGroups g

[<RequireQualifiedAccess>]
module Ipv6Endpoint =

    /// <summary>
    /// The endpoint as text: <c>[address]:port</c>, with <c>%scope</c> after the address
    /// when the scope id is not 0, as in <c>[fe80::1%2]:80</c>.
    /// </summary>
    /// <remarks>
    /// The flow info is not written: no text form of an address has one.
    ///
    /// The same text as .NET's <c>System.Net.IPEndPoint.ToString</c>.
    /// Not a rendering any process sees.
    /// </remarks>
    let toString (endpoint : Ipv6Endpoint) : string =
        let scope =
            if endpoint.ScopeId = 0u then
                ""
            else
                $"%%%d{endpoint.ScopeId}"

        $"[%s{Ipv6Address.toString endpoint.Address}%s{scope}]:%d{endpoint.Port}"
