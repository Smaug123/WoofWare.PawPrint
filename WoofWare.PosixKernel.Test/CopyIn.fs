namespace WoofWare.PosixKernel.Test

open System.Buffers.Binary
open System.Collections.Immutable
open WoofWare.PosixKernel

/// A caller of `bind(2)` and `connect(2)`, for a test that wants to say which
/// address it means rather than lay one out: the blob a caller holds, and the
/// two calls a client makes with it, admitting the copy and then passing the
/// bytes the admission named.
[<RequireQualifiedAccess>]
module CopyIn =

    /// How long `blob` is: longer than any copy-in takes, which is at most 128
    /// bytes on Linux and 255 on Darwin.
    [<Literal>]
    let Length : int = 256

    /// A caller's sockaddr buffer holding `family` and `endpoint` as a
    /// `struct sockaddr_in` laid out for `platform`, then zeros to `Length`.
    ///
    /// Laid out here by hand rather than by the library's encoder, so that a
    /// test of the library's decoding does not take its layout from the code
    /// under test. Darwin's `sa_len` byte holds 16, as a caller filling in a
    /// `sockaddr_in` would store; no kernel reads it.
    let blob (platform : SimulatedUnixPlatform) (family : int) (endpoint : InternetEndpoint) : byte[] =
        let blob = Array.zeroCreate<byte> Length

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            BinaryPrimitives.WriteUInt16LittleEndian (System.Span<byte> (blob, 0, 2), uint16 family)
        | SimulatedUnixFlavour.Darwin ->
            blob.[0] <- 16uy
            blob.[1] <- byte family

        BinaryPrimitives.WriteUInt16BigEndian (System.Span<byte> (blob, 2, 2), endpoint.Port)
        BinaryPrimitives.WriteUInt32BigEndian (System.Span<byte> (blob, 4, 4), endpoint.Address)
        blob

    /// A caller's sockaddr buffer holding `family`, `port` and the sixteen
    /// bytes of `address` as a `struct sockaddr_in6` laid out for `platform`,
    /// with `sin6_flowinfo` and `sin6_scope_id` zero, then zeros to `Length`.
    /// Darwin's `sa_len` byte holds 28. Laid out by hand, as `blob` is.
    let blob6 (platform : SimulatedUnixPlatform) (family : int) (address : byte[]) (port : uint16) : byte[] =
        if address.Length <> 16 then
            failwith $"CopyIn.blob6: an IPv6 address is 16 bytes, not %d{address.Length}"

        let blob = Array.zeroCreate<byte> Length

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            BinaryPrimitives.WriteUInt16LittleEndian (System.Span<byte> (blob, 0, 2), uint16 family)
        | SimulatedUnixFlavour.Darwin ->
            blob.[0] <- 28uy
            blob.[1] <- byte family

        BinaryPrimitives.WriteUInt16BigEndian (System.Span<byte> (blob, 2, 2), port)
        address.CopyTo (blob, 8)
        blob

    /// The sixteen bytes of `::ffff:a.b.c.d`, the IPv4 address `address` (host
    /// order) mapped into IPv6.
    let mappedAddress (address : uint32) : byte[] =
        let bytes = Array.zeroCreate<byte> 16
        bytes.[10] <- 0xFFuy
        bytes.[11] <- 0xFFuy
        BinaryPrimitives.WriteUInt32BigEndian (System.Span<byte> (bytes, 12, 4), address)
        bytes

    /// `blob6` holding `AF_INET6` and `endpoint` mapped into IPv6.
    let inet6Mapped (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) : byte[] =
        blob6
            platform
            (SimulatedUnixPlatform.internetV6AddressFamily platform)
            (mappedAddress endpoint.Address)
            endpoint.Port

    /// `blob` holding `AF_INET` and `endpoint`.
    let inet (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) : byte[] =
        blob platform SimulatedUnixPlatform.internetAddressFamily endpoint

    /// The 16 bytes a copy-in of a `struct sockaddr_in` holding `AF_INET` and
    /// `endpoint`, declared 16 bytes long, takes on `platform`.
    let inetCopy (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (inet platform endpoint, 0, SimulatedUnixPlatform.internetSocketAddressSize)

    /// The first `length` bytes of `blob`.
    let prefix (blob : byte[]) (length : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (blob, 0, length)

    /// What a copy-in of `declaredLength` bytes from real storage holding `blob`
    /// takes, for a caller of `UnixConnection.connectSocket`.
    let mapped (platform : SimulatedUnixPlatform) (declaredLength : uint32) (blob : byte[]) : ImmutableArray<byte> =
        prefix blob (UnixSocket.mappedCopyLength platform declaredLength)

    /// The bytes `syscall`'s copy-in takes from `blob`: as many as
    /// `admitSockaddrCopy` answers `Transfer` with, and none otherwise.
    let admitted<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (syscall : SockaddrCopySyscall)
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (blob : byte[])
        (system : UnixSystem<'Task, 'Handler>)
        : ImmutableArray<byte>
        =
        match UnixSocket.admitSockaddrCopy syscall fd destination declaredLength system with
        | Ok (SockaddrCopyAdmission.Transfer length) -> prefix blob length
        | Ok (SockaddrCopyAdmission.Answered _)
        | Error _ -> ImmutableArray.Empty

    /// `bind(2)` as a client makes it: admit the copy, then pass the bytes it
    /// takes from `blob`.
    let bind<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (blob : byte[])
        (system : UnixSystem<'Task, 'Handler>)
        : Result<BindAnswer * UnixSystem<'Task, 'Handler>, BindRefusal>
        =
        UnixSocket.bind
            fd
            destination
            declaredLength
            (admitted SockaddrCopySyscall.Bind fd destination declaredLength blob system)
            system

    /// `connect(2)` as a client makes it: admit the copy, then pass the bytes
    /// it takes from `blob`.
    let connect<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : uint32)
        (blob : byte[])
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ConnectOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>
        =
        UnixConnection.connect
            fd
            destination
            declaredLength
            (admitted SockaddrCopySyscall.Connect fd destination declaredLength blob system)
            system

/// What `getsockname(2)` and `accept(2)` copy out, read back and predicted
/// without the library's own encoder.
[<RequireQualifiedAccess>]
module CopyOut =

    /// The endpoint in a copied-out `struct sockaddr_in` that reaches
    /// `sin_addr`: the port at 2 and the address at 4, both network order.
    let endpoint (copiedOut : ImmutableArray<byte>) : InternetEndpoint =
        if copiedOut.Length < 8 then
            failwith $"CopyOut.endpoint: %d{copiedOut.Length} bytes do not reach sin_addr"

        let span = copiedOut.AsSpan ()

        InternetEndpoint.ofParts
            (BinaryPrimitives.ReadUInt32BigEndian (span.Slice (4, 4)))
            (BinaryPrimitives.ReadUInt16BigEndian (span.Slice (2, 2)))

    /// What a kernel copies out of `endpoint` to a caller that declared
    /// `declaredLength` bytes of room: the first `min(declaredLength, 16)` bytes
    /// of the `struct sockaddr_in` holding it, which on Darwin begins with an
    /// `sa_len` of 16 (`sockaddr-decoding.c`, T).
    let expected (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) (declaredLength : uint32) =
        CopyIn.prefix (CopyIn.inet platform endpoint) (int (min declaredLength 16u))
