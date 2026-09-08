namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Security.Cryptography
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `Sha256` against the host's `SHA256.HashData`, which is the oracle throughout: the
/// implementation is never compared with itself.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSha256 =

    let private hex (bytes : ImmutableArray<byte>) : string =
        Convert.ToHexString (bytes.AsSpan ()) |> fun s -> s.ToLowerInvariant ()

    let private oracle (bytes : byte[]) : string =
        Convert.ToHexString (SHA256.HashData bytes) |> fun s -> s.ToLowerInvariant ()

    /// FIPS 180-4's own worked examples, pinned from outside PawPrint.
    [<TestCase("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")>]
    [<TestCase("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")>]
    [<TestCase("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
               "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")>]
    let ``known answers`` (message : string, expected : string) : unit =
        Text.Encoding.ASCII.GetBytes message
        |> ImmutableArray.CreateRange
        |> Sha256.hash
        |> hex
        |> shouldEqual expected

    /// Lengths at which the padding changes shape: the message plus the 0x80 byte and the
    /// 8-byte length fits one block up to 55 bytes and spills into a second from 56; 63/64/65
    /// straddle a block boundary; 119/120/121 do both at once; 4096 is what
    /// `HashAlgorithm.ComputeHash(Stream)` reads at a time.
    let private boundaryLengths : int list =
        [
            0
            1
            55
            56
            57
            63
            64
            65
            119
            120
            121
            127
            128
            129
            191
            192
            4095
            4096
            4097
        ]

    /// `Gen.choose` throughout rather than the size-bounded default int generator, so that the
    /// large lengths are actually reached.
    let private genLength : Gen<int> =
        Gen.frequency
            [
                3, Gen.elements boundaryLengths
                3, Gen.choose (0, 200)
                2, Gen.choose (0, 3000)
                1, Gen.choose (0, 6) |> Gen.map (fun blocks -> blocks * Sha256.BlockSize)
            ]

    let private genBytes (length : int) : Gen<byte[]> =
        Gen.arrayOfLength length (Gen.choose (0, 255) |> Gen.map byte)

    /// A chunking of `total` bytes. The chunk-size ceiling is itself drawn, so that the runs
    /// cover byte-at-a-time feeding, a handful of bytes, block-sized-ish chunks and one big
    /// chunk; and an empty chunk is thrown in now and then, since `ReadOnlySpan.IsEmpty` guards
    /// in CoreLib are the only thing keeping those away from the real implementation.
    let private genChunks (total : int) : Gen<int list> =
        gen {
            let! maxChunk =
                Gen.frequency
                    [
                        1, Gen.constant 1
                        2, Gen.choose (1, 8)
                        3, Gen.choose (1, 100)
                        2, Gen.choose (1, 5000)
                    ]

            let rec go (remaining : int) (acc : int list) : Gen<int list> =
                gen {
                    let! emptyFirst = Gen.frequency [ 7, Gen.constant false ; 1, Gen.constant true ]
                    let acc = if emptyFirst then 0 :: acc else acc

                    if remaining = 0 then
                        return List.rev acc
                    else
                        let! chunk = Gen.choose (1, min remaining maxChunk)
                        return! go (remaining - chunk) (chunk :: acc)
                }

            return! go total []
        }

    let private genCase : Gen<byte[] * int list> =
        gen {
            let! length = genLength
            let! bytes = genBytes length
            let! chunks = genChunks length
            return bytes, chunks
        }

    /// Feed `chunks` of `bytes` through `update`, returning the state after each chunk.
    let private feed (bytes : byte[]) (chunks : int list) : (int * Sha256State) list =
        let _, states =
            chunks
            |> List.fold
                (fun (offset, states : (int * Sha256State) list) chunk ->
                    let _, previous = List.head states

                    let next =
                        Sha256.update (ImmutableArray.Create<byte> (bytes, offset, chunk)) previous

                    offset + chunk, (offset + chunk, next) :: states
                )
                (0, [ 0, Sha256.empty ])

        List.rev states

    /// Whether some chunk boundary falls strictly inside a block, which is the case
    /// `Sha256.update`'s pending-bytes carry exists for.
    let private straddlesABlock (chunks : int list) : bool =
        chunks
        |> List.scan (+) 0
        |> List.exists (fun offset -> offset % Sha256.BlockSize <> 0)

    [<Test>]
    let ``incremental feeding in any chunking agrees with the host on the whole message`` () : unit =
        let straddling = ref 0
        let twoBlockPadding = ref 0
        let multiBlock = ref 0

        let property (bytes : byte[], chunks : int list) : unit =
            List.sum chunks |> shouldEqual bytes.Length

            let states = feed bytes chunks
            let _, final = List.last states

            Sha256.length final |> shouldEqual (uint64 bytes.Length)
            hex (Sha256.finish final) |> shouldEqual (oracle bytes)

            if straddlesABlock chunks then
                straddling.Value <- straddling.Value + 1

            if bytes.Length % Sha256.BlockSize >= 56 then
                twoBlockPadding.Value <- twoBlockPadding.Value + 1

            if bytes.Length >= Sha256.BlockSize then
                multiBlock.Value <- multiBlock.Value + 1

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen genCase) property)

        // The generator's job is to reach the carry, the second padding block and the
        // multi-block compressions; these bounds sit far below the observed rates.
        straddling.Value |> shouldBeGreaterThan 300
        twoBlockPadding.Value |> shouldBeGreaterThan 60
        multiBlock.Value |> shouldBeGreaterThan 300

    /// `finish` is a pure function of the state: finishing after any prefix of the chunks gives
    /// the host's digest of that prefix, and the same state then continues to the whole
    /// message. This is what lets `EvpDigestCurrent` be answered from the live context without a
    /// copy.
    [<Test>]
    let ``finishing a prefix does not disturb the computation`` () : unit =
        let property (bytes : byte[], chunks : int list) : unit =
            let states = feed bytes chunks

            for offset, state in states do
                hex (Sha256.finish state) |> shouldEqual (oracle bytes.[0 .. offset - 1])

            let _, final = List.last states
            hex (Sha256.finish final) |> shouldEqual (oracle bytes)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen genCase) property)

    /// A state is a value: two continuations from one state are independent of each other.
    [<Test>]
    let ``a state can be continued in two directions independently`` () : unit =
        let genBranching : Gen<byte[] * int * byte[] * byte[]> =
            gen {
                let! prefixLength = genLength
                let! prefix = genBytes prefixLength
                let! left = genLength >>= genBytes
                let! right = genLength >>= genBytes
                return prefix, prefixLength, left, right
            }

        let property (prefix : byte[], _prefixLength : int, left : byte[], right : byte[]) : unit =
            let shared = Sha256.update (ImmutableArray.CreateRange prefix) Sha256.empty
            let leftState = Sha256.update (ImmutableArray.CreateRange left) shared
            let rightState = Sha256.update (ImmutableArray.CreateRange right) shared

            hex (Sha256.finish leftState) |> shouldEqual (oracle (Array.append prefix left))

            hex (Sha256.finish rightState)
            |> shouldEqual (oracle (Array.append prefix right))

            hex (Sha256.finish shared) |> shouldEqual (oracle prefix)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen genBranching) property)
