namespace WoofWare.PosixKernel.Test

open System.Buffers.Binary
open System.Net
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The renderings of an IPv4 address and endpoint agree with the host's own
/// `IPAddress` and `IPEndPoint`, which share no formatting code with the
/// library.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIpv4Address =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// The host's `IPAddress` for the host-order `address`, built from its
    /// network-order bytes.
    let private hostAddress (address : uint32) : IPAddress =
        let bytes = Array.zeroCreate<byte> 4
        BinaryPrimitives.WriteUInt32BigEndian (System.Span<byte> bytes, address)
        IPAddress bytes

    /// Every octet drawn from the whole of `byte`, so that one- two- and
    /// three-digit octets all occur in every position.
    let private anyAddress : Gen<uint32> =
        ArbMap.defaults
        |> ArbMap.generate<byte>
        |> Gen.arrayOfLength 4
        |> Gen.map (fun bytes -> BinaryPrimitives.ReadUInt32BigEndian (System.ReadOnlySpan<byte> bytes))

    [<Test>]
    let ``toString is the host's dotted quad`` () : unit =
        let property (address : uint32) : unit =
            Ipv4Address.toString (Ipv4Address address)
            |> shouldEqual ((hostAddress address).ToString ())

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen anyAddress) property)

    [<Test>]
    let ``Ipv4Endpoint.toString is the host's rendering of the endpoint`` () : unit =
        let property (address : uint32, port : uint16) : unit =
            Ipv4Endpoint.toString (Ipv4Endpoint.ofParts (Ipv4Address address) port)
            |> shouldEqual (IPEndPoint(hostAddress address, int port).ToString ())

        let gen =
            gen {
                let! address = anyAddress
                let! port = ArbMap.defaults |> ArbMap.generate<uint16>
                return address, port
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``the named addresses are the C library's`` () : unit =
        Ipv4Address.any |> shouldEqual (Ipv4Address 0u)
        Ipv4Address.toString Ipv4Address.loopback |> shouldEqual "127.0.0.1"
        Ipv4Address.toString Ipv4Address.broadcast |> shouldEqual "255.255.255.255"
