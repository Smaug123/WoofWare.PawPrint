namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `Ipv4Prefix.create` admits exactly the prefixes Linux's `rtm_to_fib_config`
/// admits for a route, and `withLocalAddresses` stores whatever lists of them
/// it is given. The oracles here read one bit at a time, so they share no
/// shift or mask arithmetic with the code under test.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIpv4Prefix =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// Bit `index` of `value`, counting from the most significant.
    let private leadingBit (value : uint32) (index : int) : bool = (value >>> (31 - index)) &&& 1u = 1u

    /// Whether `network` has no bit set past its first `bits`.
    let private hostBitsClear (network : uint32) (bits : int) : bool =
        seq { bits..31 } |> Seq.forall (fun index -> not (leadingBit network index))

    /// `address` with every bit past its first `bits` cleared.
    let private keepLeading (address : uint32) (bits : int) : uint32 =
        seq { 0 .. bits - 1 }
        |> Seq.fold
            (fun acc index ->
                if leadingBit address index then
                    acc ||| (1u <<< (31 - index))
                else
                    acc
            )
            0u

    /// A prefix length weighted towards the edges of `[0, 32]`, where a range
    /// test goes wrong, as well as spread across the whole of `int`.
    let private anyBits : Gen<int> =
        Gen.oneof
            [
                Gen.choose (-2, 34)
                Gen.elements
                    [
                        Int32.MinValue
                        Int32.MinValue + 1
                        -1
                        0
                        1
                        31
                        32
                        33
                        Int32.MaxValue
                    ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    let private anyAddress : Gen<uint32> =
        Gen.oneof
            [
                Gen.elements [ 0u ; 1u ; 0x7F000000u ; 0x80000000u ; UInt32.MaxValue ]
                ArbMap.defaults |> ArbMap.generate<uint32>
            ]

    /// A network and a length, about half of them with the host bits cleared:
    /// an arbitrary address with a length much above zero almost never has
    /// them clear by chance, so without this the admitted side would be
    /// reached only near length 32.
    let private networkAndBits : Gen<uint32 * int> =
        gen {
            let! address = anyAddress
            let! bits = anyBits
            let! clear = Gen.elements [ true ; false ]

            let network =
                if clear && bits >= 0 && bits <= 32 then
                    keepLeading address bits
                else
                    address

            return network, bits
        }

    let private prefixOrFail (network : uint32) (bits : int) : Ipv4Prefix =
        match Ipv4Prefix.create network bits with
        | Ok prefix -> prefix
        | Error refusal ->
            failwith $"test bug: a prefix the test means to be valid: %s{Ipv4PrefixRefusal.describe refusal}"

    let private validPrefix : Gen<Ipv4Prefix> =
        gen {
            let! address = anyAddress
            let! bits = Gen.choose (0, 32)
            return prefixOrFail (keepLeading address bits) bits
        }

    [<Test>]
    let ``create admits exactly the lengths in [0, 32] with no host bit set, and its accessors give back the input``
        ()
        : unit
        =
        let property (network : uint32, bits : int) : unit =
            let inRange = bits >= 0 && bits <= 32

            match Ipv4Prefix.create network bits with
            | Ok prefix ->
                if not (inRange && hostBitsClear network bits) then
                    failwith $"0x%08X{network}/%d{bits} was admitted"

                Ipv4Prefix.network prefix |> shouldEqual network
                Ipv4Prefix.bits prefix |> shouldEqual bits
            | Error refusal ->
                if not inRange then
                    refusal |> shouldEqual (Ipv4PrefixRefusal.LengthOutOfRange bits)
                elif not (hostBitsClear network bits) then
                    refusal |> shouldEqual (Ipv4PrefixRefusal.HostBitsSet (network, bits))
                else
                    failwith $"0x%08X{network}/%d{bits} was refused: %s{Ipv4PrefixRefusal.describe refusal}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen networkAndBits) property)

    [<Test>]
    let ``create's boundaries`` () : unit =
        let outcome (network : uint32) (bits : int) : Result<uint32 * int, Ipv4PrefixRefusal> =
            Ipv4Prefix.create network bits
            |> Result.map (fun prefix -> Ipv4Prefix.network prefix, Ipv4Prefix.bits prefix)

        outcome 0u -1 |> shouldEqual (Error (Ipv4PrefixRefusal.LengthOutOfRange -1))
        outcome 0u 0 |> shouldEqual (Ok (0u, 0))
        outcome 1u 0 |> shouldEqual (Error (Ipv4PrefixRefusal.HostBitsSet (1u, 0)))

        outcome 0x80000000u 0
        |> shouldEqual (Error (Ipv4PrefixRefusal.HostBitsSet (0x80000000u, 0)))

        outcome 0x80000000u 1 |> shouldEqual (Ok (0x80000000u, 1))

        outcome 0x40000000u 1
        |> shouldEqual (Error (Ipv4PrefixRefusal.HostBitsSet (0x40000000u, 1)))

        outcome 0x7F000000u 8 |> shouldEqual (Ok (0x7F000000u, 8))

        outcome 0x7F000001u 8
        |> shouldEqual (Error (Ipv4PrefixRefusal.HostBitsSet (0x7F000001u, 8)))

        outcome 0xFFFFFFFEu 31 |> shouldEqual (Ok (0xFFFFFFFEu, 31))

        outcome UInt32.MaxValue 31
        |> shouldEqual (Error (Ipv4PrefixRefusal.HostBitsSet (UInt32.MaxValue, 31)))

        outcome UInt32.MaxValue 32 |> shouldEqual (Ok (UInt32.MaxValue, 32))
        outcome 0u 33 |> shouldEqual (Error (Ipv4PrefixRefusal.LengthOutOfRange 33))
        // Out of range is reported before the host bits, which a length
        // outside [0, 32] does not define.
        outcome UInt32.MaxValue 33
        |> shouldEqual (Error (Ipv4PrefixRefusal.LengthOutOfRange 33))

    [<Test>]
    let ``contains holds exactly for an address agreeing with the network on the first bits`` () : unit =
        let property (prefix : Ipv4Prefix, address : uint32, agree : bool) : unit =
            let network = Ipv4Prefix.network prefix
            let bits = Ipv4Prefix.bits prefix

            // Half the time, an address inside the prefix by construction: an
            // arbitrary one is almost never inside a long prefix.
            let address =
                if agree then
                    network ||| (address &&& ~~~(keepLeading UInt32.MaxValue bits))
                else
                    address

            let expected =
                seq { 0 .. bits - 1 }
                |> Seq.forall (fun index -> leadingBit address index = leadingBit network index)

            Ipv4Prefix.contains address prefix |> shouldEqual expected

        let gen =
            gen {
                let! prefix = validPrefix
                let! address = anyAddress
                let! agree = Gen.elements [ true ; false ]
                return prefix, address, agree
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    /// A prefix is a struct, so a client can always conjure its default
    /// without `create`; that default must be a prefix `create` admits, or
    /// `withLocalAddresses` could be handed a route no machine has.
    [<Test>]
    let ``the default prefix is 0.0.0.0/0, which create admits`` () : unit =
        let forged = Unchecked.defaultof<Ipv4Prefix>
        Ipv4Prefix.create 0u 0 |> shouldEqual (Ok forged)
        Ipv4Prefix.contains 0x08080808u forged |> shouldEqual true

    [<Test>]
    let ``loopbackNetwork is 127.0.0.0/8`` () : unit =
        Ipv4Prefix.create 0x7F000000u 8 |> shouldEqual (Ok Ipv4Prefix.loopbackNetwork)

    /// Overlapping and repeated entries are what real machines hold: every
    /// Linux's local table has `127.0.0.0/8` and `127.0.0.1/32`, and an address
    /// assigned to two interfaces is two routes to one prefix. So the setter
    /// stores the lists as given, whatever they are.
    [<Test>]
    let ``withLocalAddresses stores any lists of addresses and prefixes as given`` () : unit =
        let property (platform : SimulatedUnixPlatform, addresses : uint32 list, routes : Ipv4Prefix list) : unit =
            let machine =
                (UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                 |> UnixBootImage.withLocalAddresses addresses routes
                 |> UnixBootImage.boot)
                    .Machine

            machine.LocalAddresses |> shouldEqual addresses
            machine.LocalRoutes |> shouldEqual routes

        let gen =
            gen {
                let! platform = Gen.elements [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]
                let! addresses = Gen.listOf anyAddress
                let! routes = Gen.listOf validPrefix
                // Repeat some of them, so that duplicates are reached every run.
                let! addresses = Gen.elements [ addresses ; addresses @ addresses ]
                let! routes = Gen.elements [ routes ; routes @ routes ]
                return platform, addresses, routes
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

        let overlapping = [ Ipv4Prefix.loopbackNetwork ; prefixOrFail 0x7F000001u 32 ]
        property (SimulatedUnixPlatform.linuxX64, [ InternetEndpoint.LoopbackAddress ], overlapping)
