namespace WoofWare.PosixKernel.Test

open System
open System.Net
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The `struct sockaddr` layout this library states, against the one the machine
/// running the test actually has.
///
/// `IPEndPoint.Serialize()` hands back a real platform `sockaddr`: the runtime
/// builds it with the same writes a guest's would make -- the length byte from
/// `SocketAddress`'s own constructor, the family and the transport fields from
/// the platform shim -- so its bytes are evidence about this machine rather than
/// about .NET. No socket is opened and nothing is sent.
///
/// Every fact this fixture checks about *offsets* is one both platforms agree
/// on, so both columns check all of them; what splits by platform is the family
/// field and `AF_INET6`'s number, and each host falsifies its own column of
/// those. macOS locally and Linux in CI between them cover both.
///
/// One thing this cannot witness: the copy-*out* direction. `Serialize` shows
/// what a caller writes on the way *in*, while
/// `SimulatedUnixPlatform.encodeInternetSockaddr` models what a kernel writes on
/// the way out. They agree byte for byte, and the evidence for the kernel's own
/// `sa_len` is the measurement recorded on that function.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSockaddrLayoutAgainstHost =

    /// Which flavour the machine running this test is, so the expectations can
    /// name the column being checked. Unrelated to `SimulatedUnixPlatform`: that
    /// is what PawPrint *simulates*, and this is what the test is running on.
    let private hostFlavour : SimulatedUnixFlavour =
        if OperatingSystem.IsMacOS () then
            SimulatedUnixFlavour.Darwin
        elif OperatingSystem.IsLinux () then
            SimulatedUnixFlavour.Linux
        else
            failwith "this fixture measures a Unix host; it should have been skipped"

    /// A platform of the host's own flavour, so that the library's per-flavour
    /// answers can be compared against the host's bytes.
    let private hostPlatform : SimulatedUnixPlatform =
        match hostFlavour with
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

    /// Deliberately asymmetric, so that a byte-order mistake cannot pass: every
    /// byte of the address differs, and the port differs in both halves.
    let private port : uint16 = 0x1234us

    let private serialize (address : string) : byte[] =
        let socketAddress = IPEndPoint(IPAddress.Parse address, int port).Serialize ()
        Array.init socketAddress.Size (fun i -> socketAddress.[i])

    let private v4 : byte[] = serialize "1.2.3.4"

    /// A scope id that is neither zero nor a repeated byte, so its four bytes
    /// pin both the offset and the order.
    let private v6 : byte[] = serialize "::1%7"

    let private field (f : SockaddrField) (blob : byte[]) : byte[] = Array.sub blob f.Offset f.Width

    [<OneTimeSetUp>]
    let onlyOnUnix () : unit =
        if not (OperatingSystem.IsMacOS () || OperatingSystem.IsLinux ()) then
            Assert.Ignore "this fixture measures a Unix host's sockaddr layout"

    // ------------------------------------------------------------------
    // Sizes
    // ------------------------------------------------------------------

    /// `sizeof(struct sockaddr_in)` and `sizeof(struct sockaddr_in6)`, which
    /// `SystemNative_GetSocketAddressSizes` reports and this library states.
    [<Test>]
    let ``the address sizes are this machine's`` () : unit =
        let sizes = SimulatedUnixPlatform.socketAddressSizes hostPlatform
        v4.Length |> shouldEqual sizes.InterNetwork
        v6.Length |> shouldEqual sizes.InterNetworkV6

    // ------------------------------------------------------------------
    // The fields both platforms agree about
    // ------------------------------------------------------------------

    /// `sin_port` at 2, network order — so the high byte of 0x1234 comes first,
    /// whichever way round this machine stores its own integers.
    [<Test>]
    let ``the IPv4 port is where this machine puts it`` () : unit =
        field InternetSockaddr.port v4 |> shouldEqual [| 0x12uy ; 0x34uy |]

    /// `sin_addr` at 4, four bytes moved verbatim: 1.2.3.4 reads as 01 02 03 04
    /// in the blob, which is what makes the no-swap contract visible.
    [<Test>]
    let ``the IPv4 address is where this machine puts it`` () : unit =
        field InternetSockaddr.address v4 |> shouldEqual [| 1uy ; 2uy ; 3uy ; 4uy |]

    /// `sin6_port` is a different field of a different struct that happens to
    /// sit at the same offset, which is why the library states it separately.
    [<Test>]
    let ``the IPv6 port is where this machine puts it`` () : unit =
        field InternetV6Sockaddr.port v6 |> shouldEqual [| 0x12uy ; 0x34uy |]

    /// `sin6_flowinfo` at 4, which nothing sets, so it reads as zero.
    [<Test>]
    let ``the IPv6 flow info is where this machine puts it`` () : unit =
        field InternetV6Sockaddr.flowInfo v6 |> shouldEqual (Array.zeroCreate<byte> 4)

    /// `sin6_addr` at 8, sixteen bytes: `::1` is fifteen zeroes and a one.
    [<Test>]
    let ``the IPv6 address is where this machine puts it`` () : unit =
        let expected = Array.zeroCreate<byte> 16
        expected.[15] <- 1uy
        field InternetV6Sockaddr.address v6 |> shouldEqual expected

    /// `sin6_scope_id` at 24, in the *host's* byte order rather than the
    /// network's — the one field of the four that is not byte-swapped, and the
    /// reason the descriptors carry no byte order of their own.
    [<Test>]
    let ``the IPv6 scope id is where this machine puts it, in host order`` () : unit =
        let expected = Array.zeroCreate<byte> 4
        BitConverter.TryWriteBytes (Span<byte> expected, 7u) |> shouldEqual true
        field InternetV6Sockaddr.scopeId v6 |> shouldEqual expected

    // ------------------------------------------------------------------
    // The two facts that split by platform
    // ------------------------------------------------------------------

    /// The family field's position and width, which is the one part of the
    /// layout that moves: two bytes at 0 on Linux, one byte at 1 on Darwin with
    /// `sa_len` in front of it.
    [<Test>]
    let ``the family field is where this machine puts it`` () : unit =
        let descriptor = SimulatedUnixPlatform.sockaddrFamilyField hostPlatform

        let familyBytes =
            field
                {
                    Offset = SockaddrFamilyField.offset descriptor
                    Width = SockaddrFamilyField.width descriptor
                }
                v4

        let expected =
            match SockaddrFamilyField.width descriptor with
            | 1 -> [| byte SimulatedUnixPlatform.internetAddressFamily |]
            | _ ->
                let bytes = Array.zeroCreate<byte> 2

                BitConverter.TryWriteBytes (Span<byte> bytes, uint16 SimulatedUnixPlatform.internetAddressFamily)
                |> shouldEqual true

                bytes

        familyBytes |> shouldEqual expected

    /// `sa_len`, which only the BSD layout has. On Linux byte 0 is the family's
    /// low byte instead, so this asserts the *absence* there rather than
    /// skipping: a Linux machine that grew a length byte would fail here.
    [<Test>]
    let ``the length byte is present exactly where this machine has one`` () : unit =
        match SimulatedUnixPlatform.sockaddrFamilyField hostPlatform with
        | SockaddrFamilyField.OneByteAtOffsetOne ->
            v4.[0] |> shouldEqual (byte v4.Length)
            v6.[0] |> shouldEqual (byte v6.Length)
        | SockaddrFamilyField.TwoBytesAtOffsetZero ->
            v4.[0] |> shouldEqual (byte SimulatedUnixPlatform.internetAddressFamily)

    /// `AF_INET6`'s number, which the two flavours disagree about where they
    /// agree on `AF_INET`: 10 on Linux, 30 on Darwin.
    [<Test>]
    let ``AF_INET6's number is this machine's`` () : unit =
        let descriptor = SimulatedUnixPlatform.sockaddrFamilyField hostPlatform

        let familyBytes =
            field
                {
                    Offset = SockaddrFamilyField.offset descriptor
                    Width = SockaddrFamilyField.width descriptor
                }
                v6

        let observed =
            match familyBytes.Length with
            | 1 -> int familyBytes.[0]
            | _ -> int (BitConverter.ToUInt16 (familyBytes, 0))

        observed
        |> shouldEqual (SimulatedUnixPlatform.internetV6AddressFamily hostPlatform)

    // ------------------------------------------------------------------
    // The encoder, against the same evidence
    // ------------------------------------------------------------------

    /// `encodeInternetSockaddr` models the kernel's copy-*out*, and `Serialize`
    /// witnesses a caller's copy-*in*. They are different directions of the same
    /// struct, and on this machine they agree byte for byte — including the
    /// `sa_len` byte, which each writes for its own reason.
    [<Test>]
    let ``the encoder agrees with this machine, byte for byte`` () : unit =
        let endpoint =
            InternetEndpoint.ofParts (BitConverter.ToUInt32 ([| 4uy ; 3uy ; 2uy ; 1uy |], 0)) port

        // The address as a host-order word, spelled out rather than parsed: the
        // library holds it in network order, which on a little-endian machine is
        // the bytes reversed.
        SimulatedUnixPlatform.encodeInternetSockaddr hostPlatform endpoint
        |> shouldEqual v4
