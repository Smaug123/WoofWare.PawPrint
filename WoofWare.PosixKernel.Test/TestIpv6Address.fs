namespace WoofWare.PosixKernel.Test

open System
open System.Net
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `Ipv6Address` against the sixteen bytes of `sin6_addr` it stands for, and
/// its text against the host's own `IPAddress` formatter.
///
/// The oracles here read bytes one at a time, so they share no word
/// arithmetic with the code under test.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIpv6Address =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// One 16-bit group of an address, weighted towards the groups the text
    /// form treats specially: runs of zeros are what `::` compresses, and
    /// `ffff` and `5efe` are what decide whether the last 32 bits are written
    /// as a dotted quad.
    let private group : Gen<uint16> =
        Gen.frequency
            [
                5, Gen.constant 0us
                1, Gen.constant 0xFFFFus
                1, Gen.constant 0x5EFEus
                1, Gen.choose (1, 0xFF) |> Gen.map uint16
                2, ArbMap.defaults |> ArbMap.generate<uint16>
            ]

    let private bytesOfGroups (groups : uint16[]) : byte[] =
        let bytes = Array.zeroCreate<byte> 16

        for i in 0..7 do
            bytes.[2 * i] <- byte (groups.[i] >>> 8)
            bytes.[2 * i + 1] <- byte groups.[i]

        bytes

    /// The byte check the IPv6 sockaddr decoder made before `Ipv6Address`
    /// existed: ten zero bytes, then `ff ff`, then the IPv4 address in network
    /// order.
    let private mappedByBytes (bytes : byte[]) : uint32 option =
        let prefixZero = seq { 0..9 } |> Seq.forall (fun i -> bytes.[i] = 0uy)

        if prefixZero && bytes.[10] = 0xFFuy && bytes.[11] = 0xFFuy then
            Some (
                (uint32 bytes.[12] <<< 24)
                ||| (uint32 bytes.[13] <<< 16)
                ||| (uint32 bytes.[14] <<< 8)
                ||| uint32 bytes.[15]
            )
        else
            None

    let private anyIpv4 : Gen<uint32> =
        Gen.oneof
            [
                Gen.elements [ 0u ; 1u ; 0x7F000001u ; 0x00FFFFFFu ; 0x01000000u ; UInt32.MaxValue ]
                ArbMap.defaults |> ArbMap.generate<uint32>
            ]

    /// `::ffff:a.b.c.d`, and its neighbours: the same with one byte of the
    /// 96-bit prefix replaced, often by a value one bit away from the right one.
    let private nearMapped : Gen<byte[]> =
        gen {
            let! v4 = anyIpv4
            let! perturbed = Gen.elements [ true ; false ]
            let! index = Gen.choose (0, 11)

            let! value =
                Gen.oneof
                    [
                        Gen.elements [ 0uy ; 1uy ; 0x80uy ; 0x7Fuy ; 0xFEuy ; 0xFFuy ]
                        ArbMap.defaults |> ArbMap.generate<byte>
                    ]

            let bytes = CopyIn.mappedAddress v4

            if perturbed then
                bytes.[index] <- value

            return bytes
        }

    /// The prefixes after which a formatter may write the last 32 bits as a
    /// dotted quad (IPv4-compatible, mapped, translated, and ISATAP's with a
    /// random interface prefix), followed by an IPv4 address that is often
    /// below `0.1.0.0`, which is where the host's formatter declines to.
    let private embeddingPrefix : Gen<byte[]> =
        gen {
            let! prefix =
                Gen.oneof
                    [
                        Gen.constant [| 0us ; 0us ; 0us ; 0us ; 0us ; 0us |]
                        Gen.constant [| 0us ; 0us ; 0us ; 0us ; 0us ; 0xFFFFus |]
                        Gen.constant [| 0us ; 0us ; 0us ; 0us ; 0xFFFFus ; 0us |]
                        Gen.arrayOfLength 4 group
                        |> Gen.map (fun interfacePrefix -> Array.append interfacePrefix [| 0us ; 0x5EFEus |])
                    ]

            let! v4 = Gen.oneof [ anyIpv4 ; Gen.choose (0, 0x1FFFF) |> Gen.map uint32 ]

            return bytesOfGroups (Array.append prefix [| uint16 (v4 >>> 16) ; uint16 v4 |])
        }

    /// Sixteen bytes: uniformly random, built from special-heavy groups, near
    /// the v4-mapped prefix, or after a prefix that embeds IPv4.
    let private addressBytes : Gen<byte[]> =
        Gen.oneof
            [
                Gen.arrayOfLength 16 (ArbMap.defaults |> ArbMap.generate<byte>)
                Gen.arrayOfLength 8 group |> Gen.map bytesOfGroups
                nearMapped
                embeddingPrefix
            ]

    let private ofBytesOrFail (bytes : byte[]) : Ipv6Address =
        match Ipv6Address.ofBytes (ReadOnlySpan<byte> bytes) with
        | Some address -> address
        | None -> failwith $"ofBytes refused sixteen bytes: %A{bytes}"

    [<Test>]
    let ``ofBytes takes every sixteen bytes, and toBytes gives them back`` () : unit =
        let property (bytes : byte[]) : unit =
            ofBytesOrFail bytes |> Ipv6Address.toBytes |> shouldEqual bytes

        Check.One (config, Prop.forAll (Arb.fromGen addressBytes) property)

    [<Test>]
    let ``ofBytes refuses every length but sixteen`` () : unit =
        let lengths =
            Gen.oneof [ Gen.choose (0, 40) ; Gen.choose (0, 1000) ]
            |> Gen.filter (fun length -> length <> 16)

        let gen =
            gen {
                let! length = lengths
                return! Gen.arrayOfLength length (ArbMap.defaults |> ArbMap.generate<byte>)
            }

        let property (bytes : byte[]) : unit =
            Ipv6Address.ofBytes (ReadOnlySpan<byte> bytes) |> shouldEqual None

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// Equality and ordering are the bytes', read as a 128-bit big-endian
    /// number: `compare` on two arrays of one length is lexicographic.
    [<Test>]
    let ``equality and ordering are those of the bytes`` () : unit =
        let property (a : byte[], b : byte[]) : unit =
            let x = ofBytesOrFail a
            let y = ofBytesOrFail b
            (x = y) |> shouldEqual (a = b)
            sign (compare x y) |> shouldEqual (sign (compare a b))

        // Half the pairs share one address, so that equal pairs are common.
        let pairs =
            Gen.oneof
                [
                    Gen.zip addressBytes addressBytes
                    addressBytes |> Gen.map (fun a -> a, Array.copy a)
                ]

        Check.One (config, Prop.forAll (Arb.fromGen pairs) property)

    [<Test>]
    let ``the named addresses and the default value`` () : unit =
        let loopback = Array.zeroCreate<byte> 16
        loopback.[15] <- 1uy

        Ipv6Address.toBytes Ipv6Address.loopback |> shouldEqual loopback
        Ipv6Address.toBytes Ipv6Address.any |> shouldEqual (Array.zeroCreate<byte> 16)
        // A struct's default value is a value of the type, so it had better be
        // an address: it is `::`.
        Unchecked.defaultof<Ipv6Address> |> shouldEqual Ipv6Address.any

    [<Test>]
    let ``v4Mapped lays out ::ffff:a.b.c.d, and tryV4Mapped reads it back`` () : unit =
        let property (v4 : uint32) : unit =
            let mapped = Ipv6Address.v4Mapped (Ipv4Address v4)
            Ipv6Address.toBytes mapped |> shouldEqual (CopyIn.mappedAddress v4)
            Ipv6Address.tryV4Mapped mapped |> shouldEqual (Some (Ipv4Address v4))

        Check.One (config, Prop.forAll (Arb.fromGen anyIpv4) property)

    [<RequireQualifiedAccess>]
    type private MappedLabel =
        | Mapped
        /// One byte of the 96-bit prefix differs from `::ffff:0:0/96`'s.
        | OneByteOffThePrefix
        | Other

    [<Test>]
    let ``tryV4Mapped agrees with the byte check, on and around the mapped prefix`` () : unit =
        let label (bytes : byte[]) : MappedLabel =
            let prefix = (CopyIn.mappedAddress 0u).[0..11]

            match Seq.zip prefix bytes.[0..11] |> Seq.filter (fun (p, b) -> p <> b) |> Seq.length with
            | 0 -> MappedLabel.Mapped
            | 1 -> MappedLabel.OneByteOffThePrefix
            | _ -> MappedLabel.Other

        let property (cover : MappedLabel -> unit) (bytes : byte[]) : unit =
            cover (label bytes)

            Ipv6Address.tryV4Mapped (ofBytesOrFail bytes)
            |> shouldEqual (mappedByBytes bytes |> Option.map Ipv4Address)

        let coverage = CoverageSample.check config (Arb.fromGen addressBytes) property

        for l in [ MappedLabel.Mapped ; MappedLabel.OneByteOffThePrefix ; MappedLabel.Other ] do
            if coverage.Count l < 100 then
                failwith $"the fixed sample reached %A{l} only %d{coverage.Count l} times: %A{coverage.Reached}"

    [<RequireQualifiedAccess>]
    type private TextLabel =
        /// The last 32 bits written as a dotted quad.
        | EmbeddedIpv4
        /// A run of zero groups written as `::`.
        | Compressed
        | Plain

    /// The host's formatter is RFC 5952's (lower case, no leading zeros, the
    /// longest run of two or more zero groups compressed, the first on a tie),
    /// with its own rule for when the last 32 bits are a dotted quad.
    /// `IPAddress(byte[])` has scope id 0, so it writes no `%`.
    [<Test>]
    let ``toString agrees with the host's IPAddress`` () : unit =
        let property (cover : TextLabel -> unit) (bytes : byte[]) : unit =
            let expected = IPAddress(bytes).ToString ()

            if expected.Contains '.' then cover TextLabel.EmbeddedIpv4
            elif expected.Contains "::" then cover TextLabel.Compressed
            else cover TextLabel.Plain

            Ipv6Address.toString (ofBytesOrFail bytes) |> shouldEqual expected

        let coverage = CoverageSample.check config (Arb.fromGen addressBytes) property

        for l in [ TextLabel.EmbeddedIpv4 ; TextLabel.Compressed ; TextLabel.Plain ] do
            if coverage.Count l < 100 then
                failwith $"the fixed sample reached %A{l} only %d{coverage.Count l} times: %A{coverage.Reached}"

    /// The host writes `[address%scope]:port`, the `%scope` only when it is
    /// not zero. Neither writes the flow info, which has no text form.
    [<Test>]
    let ``Ipv6Endpoint.toString agrees with the host's IPEndPoint`` () : unit =
        let scopeIds =
            Gen.frequency [ 1, Gen.constant 0u ; 2, ArbMap.defaults |> ArbMap.generate<uint32> ]

        let gen =
            Gen.zip
                (Gen.zip addressBytes (ArbMap.defaults |> ArbMap.generate<uint16>))
                (Gen.zip (ArbMap.defaults |> ArbMap.generate<uint32>) scopeIds)

        let property ((bytes : byte[], port : uint16), (flowInfo : uint32, scopeId : uint32)) : unit =
            let endpoint : Ipv6Endpoint =
                {
                    Address = ofBytesOrFail bytes
                    Port = port
                    FlowInfo = flowInfo
                    ScopeId = scopeId
                }

            Ipv6Endpoint.toString endpoint
            |> shouldEqual (IPEndPoint(IPAddress (bytes, int64 scopeId), int port).ToString ())

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// The text the host's formatter gives for addresses picked by hand, so
    /// that a reader can see the rules without running anything.
    [<TestCase("::", "::")>]
    [<TestCase("::1", "::1")>]
    [<TestCase("::ffff:127.0.0.1", "::ffff:127.0.0.1")>]
    [<TestCase("::1.2.3.4", "::1.2.3.4")>]
    [<TestCase("2001:db8:0:0:1:0:0:1", "2001:db8::1:0:0:1")>]
    [<TestCase("2001:0db8:0000:0000:0000:0000:0000:0001", "2001:db8::1")>]
    [<TestCase("1:0:1:0:1:0:1:0", "1:0:1:0:1:0:1:0")>]
    let ``toString on addresses picked by hand`` (input : string, expected : string) : unit =
        let bytes = IPAddress.Parse(input).GetAddressBytes ()
        IPAddress(bytes).ToString () |> shouldEqual expected
        Ipv6Address.toString (ofBytesOrFail bytes) |> shouldEqual expected
