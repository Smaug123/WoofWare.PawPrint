namespace WoofWare.PosixKernel.Test

open FsCheck
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The pool's generator, against Vigna's reference and against a naive
/// reimplementation of the stream it documents.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEntropyPool =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// The byte stream `EntropyPool` documents, written as plainly as possible:
    /// splitmix64's outputs, each unpacked least significant byte first, with a
    /// final partial output truncated. Deliberately shares no code with the
    /// library.
    let private reference (seed : uint64) (count : int) : byte[] * uint64 =
        let mutable state = seed
        let bytes = ResizeArray<byte> ()

        while bytes.Count < count do
            state <- state + 0x9E3779B97F4A7C15UL
            let z = state
            let z = (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
            let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
            let z = z ^^^ (z >>> 31)

            for i in 0..7 do
                if bytes.Count < count then
                    bytes.Add (byte ((z >>> (8 * i)) &&& 0xFFUL))

        bytes.ToArray (), state

    let private hex (bytes : byte seq) : string =
        bytes |> Seq.map (fun (b : byte) -> b.ToString "x2") |> String.concat ""

    /// The first three outputs of `splitmix64.c` from a zero seed, obtained by
    /// compiling and running the reference itself, and written here in the
    /// little-endian order the pool unpacks them in.
    [<Test>]
    let ``the pool is splitmix64, unpacked little-endian`` () : unit =
        let bytes, _ = EntropyPool.draw 24 (EntropyPool.ofSeed 0UL)

        hex bytes
        |> shouldEqual (
            "afcd1d7b39a820e2" // 0xe220a8397b1dcdaf
            + "f465b9a16a9e786e" // 0x6e789e6aa1b965f4
            + "4f450980185dc406" // 0x06c45d188009454f
        )

    [<Test>]
    let ``a draw is the reference stream`` () : unit =
        let property (seed : uint64) (count : byte) : unit =
            let count = int count
            let drawn, after = EntropyPool.draw count (EntropyPool.ofSeed seed)
            let expected, expectedState = reference seed count

            Seq.toArray drawn |> shouldEqual expected
            // Where the pool resumes from, which is what the next draw sees.
            after |> shouldEqual (EntropyPool.ofSeed expectedState)

        Check.One (config, property)

    [<Test>]
    let ``drawing nothing leaves the pool where it was`` () : unit =
        let property (seed : uint64) : unit =
            let pool = EntropyPool.ofSeed seed
            let drawn, after = EntropyPool.draw 0 pool
            drawn.IsEmpty |> shouldEqual true
            after |> shouldEqual pool

        Check.One (config, property)

    /// A draw's unused tail is discarded rather than carried into the next one,
    /// so a draw of `n` bytes leaves the pool where a draw of `n` rounded up to
    /// a multiple of eight would.
    [<Test>]
    let ``a partial output is discarded, not carried`` () : unit =
        let property (seed : uint64) (count : byte) : unit =
            let count = int count
            let rounded = (count + 7) / 8 * 8
            let pool = EntropyPool.ofSeed seed
            let _, afterCount = EntropyPool.draw count pool
            let _, afterRounded = EntropyPool.draw rounded pool
            afterCount |> shouldEqual afterRounded

        Check.One (config, property)

    [<Test>]
    let ``a negative count is refused`` () : unit =
        (fun () -> EntropyPool.draw -1 (EntropyPool.ofSeed 0UL) |> ignore)
        |> shouldFail<exn>

    /// Every client that does not state its own seed replays against this one,
    /// so the bytes it produces are pinned rather than derived.
    [<Test>]
    let ``the default seed's first bytes`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64

        let bytes, _ = EntropyPool.draw 16 system.Machine.EntropyPool
        hex bytes |> shouldEqual "21a2be4a9ff6b02c8989142347031794"

        // The flavour does not choose the seed.
        let darwin : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64

        darwin.Machine.EntropyPool |> shouldEqual system.Machine.EntropyPool
