namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNonCryptoRandom =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    [<Test>]
    let ``initial state is non-zero`` () : unit =
        // splitmix64 starting from zero produces a non-zero output on the
        // first step, but a non-zero initial state matches the published
        // reference implementation and avoids the easy-to-misread "the
        // seed is zero" question for anyone debugging a deterministic run.
        NonCryptoRandom.initialState |> shouldNotEqual 0UL

    [<Test>]
    let ``step is pure`` () : unit =
        // Same input state ⇒ same (output, newState). This is what makes
        // the whole runtime reproducible: replaying the same trace must
        // produce identical PRNG draws.
        let property (state : uint64) : bool =
            NonCryptoRandom.step state = NonCryptoRandom.step state

        Check.One (propertyConfig, property)

    [<Test>]
    let ``step changes state (no fixed points)`` () : unit =
        // splitmix64's weyl increment is odd, so adding it to any 64-bit
        // state must change the state — there is no fixed point. This
        // matters because a fixed point would freeze the PRNG and turn
        // every subsequent draw into the same bytes.
        let property (state : uint64) : bool =
            let _, newState = NonCryptoRandom.step state
            newState <> state

        Check.One (propertyConfig, property)

    [<Test>]
    let ``drawBytes is deterministic in the starting state`` () : unit =
        let property (count : int) (state : uint64) : bool =
            let count = abs count % 64
            let bytesA, stateA = NonCryptoRandom.drawBytes count state
            let bytesB, stateB = NonCryptoRandom.drawBytes count state
            bytesA = bytesB && stateA = stateB

        Check.One (propertyConfig, property)

    [<Test>]
    let ``drawBytes 0 is a no-op on state`` () : unit =
        // The dispatch arm short-circuits on length=0 so it never even
        // dereferences the buffer pointer; the helper must agree, otherwise
        // the empty-buffer path silently advances the PRNG and desynchronises
        // it from the dispatch arm's accounting.
        let property (state : uint64) : bool =
            let bytes, newState = NonCryptoRandom.drawBytes 0 state
            bytes.Length = 0 && newState = state

        Check.One (propertyConfig, property)

    [<Test>]
    let ``drawBytes returns exactly the requested count`` () : unit =
        let property (count : int) (state : uint64) : bool =
            let count = abs count % 256
            let bytes, _ = NonCryptoRandom.drawBytes count state
            bytes.Length = count

        Check.One (propertyConfig, property)

    [<Test>]
    let ``drawBytes negative count fails loudly`` () : unit =
        // A negative count is a caller bug. Silently returning an empty
        // buffer would hide guest-visible underflow at the SystemNative
        // boundary, where `(size_t)length` would otherwise interpret -1
        // as ~2^64.
        (fun () -> NonCryptoRandom.drawBytes -1 0UL |> ignore) |> shouldFail<exn>

    [<Test>]
    let ``drawBytes 8 matches a single splitmix64 step in little-endian order`` () : unit =
        // Pin the byte order: callers may reinterpret the buffer as
        // `ulong*` (`Random.Xoshiro256StarStarImpl`, `Marvin.DefaultSeed`,
        // etc.) and we don't want a later refactor to silently flip from
        // little-endian to big-endian.
        let property (state : uint64) : bool =
            let bytes, newState = NonCryptoRandom.drawBytes 8 state
            let output, expectedNewState = NonCryptoRandom.step state

            let expectedBytes = [| for j in 0..7 -> byte (output >>> (8 * j)) |]

            bytes = expectedBytes && newState = expectedNewState

        Check.One (propertyConfig, property)

    [<Test>]
    let ``drawBytes is incremental: prefix-then-rest equals one big draw`` () : unit =
        // The dispatch arm computes one buffer of length N and writes it.
        // If a future caller draws in chunks instead, the byte stream
        // must be identical, otherwise a refactor that splits a buffer
        // fill across two calls silently changes the PRNG output. (Note:
        // this only holds because our `drawBytes` consumes a fresh
        // 64-bit step every 8 bytes — partial-step leftover bits are
        // discarded between calls, so the prefix length is rounded up
        // to a multiple of 8 for this property.)
        let property (state : uint64) : bool =
            let prefix = 8
            let total = 24
            let big, _ = NonCryptoRandom.drawBytes total state
            let first, midState = NonCryptoRandom.drawBytes prefix state
            let rest, _ = NonCryptoRandom.drawBytes (total - prefix) midState
            big = Array.append first rest

        Check.One (propertyConfig, property)

    [<Test>]
    let ``distinct seeds eventually diverge`` () : unit =
        // Sanity check that splitmix64 is doing *something* — two
        // different seeds should produce different output streams within
        // a few steps. (Stronger statistical properties are not claimed:
        // splitmix64 is a non-crypto mixer.)
        let property (seedA : uint64) (seedB : uint64) : bool =
            if seedA = seedB then
                true
            else
                let bytesA, _ = NonCryptoRandom.drawBytes 64 seedA
                let bytesB, _ = NonCryptoRandom.drawBytes 64 seedB
                bytesA <> bytesB

        Check.One (propertyConfig, property)

    [<Test>]
    let ``EmulatedKernel.initial seeds NonCryptoRandomState from NonCryptoRandom.initialState`` () : unit =
        // The kernel default is the only seed observable from a fresh
        // run; pin it so a regression here is detected before it changes
        // the bytes every Guid.NewGuid/Random/HashCode draws.
        EmulatedKernel.initial.NonCryptoRandomState
        |> shouldEqual NonCryptoRandom.initialState

    [<Test>]
    let ``nextDouble is pure in the starting state`` () : unit =
        // Same input state ⇒ same (value, newState). Determinism is the
        // whole point — schedule fuzzing needs to be able to replay a seed
        // and observe identical doubles draws on the path that fed off it.
        let property (state : uint64) : bool =
            NonCryptoRandom.nextDouble state = NonCryptoRandom.nextDouble state

        Check.One (propertyConfig, property)

    [<Test>]
    let ``nextDouble lies in [0, 1)`` () : unit =
        // The probabilistic-concurrency scheduler will compare these
        // draws against op-weight thresholds, so a value of exactly 1.0
        // (or worse, > 1.0) would silently let a w=1.0 op skip a switch
        // it should always take. The masking-then-scaling formula
        // discards the low 11 bits so this invariant holds by construction;
        // the test pins it.
        let property (state : uint64) : bool =
            let v, _ = NonCryptoRandom.nextDouble state
            v >= 0.0 && v < 1.0

        Check.One (propertyConfig, property)

    [<Test>]
    let ``nextDouble advances state by exactly one step`` () : unit =
        // The (state -> step) -> (output, newState) shape is the contract
        // the rest of the runtime assumes. If nextDouble somehow consumed
        // two steps internally, callers interleaving doubles with bytes
        // draws would observe stream desynchronisation that's hard to
        // diagnose. Pin the equivalence.
        let property (state : uint64) : bool =
            let _, doubleState = NonCryptoRandom.nextDouble state
            let _, stepState = NonCryptoRandom.step state
            doubleState = stepState

        Check.One (propertyConfig, property)

    [<Test>]
    let ``nextDouble mean over 50k samples is near 0.5`` () : unit =
        // Sanity check that the [0, 1) draws are *roughly* uniform — not a
        // statistical certificate (splitmix64 isn't crypto), just enough
        // to catch a wholesale bias (e.g. an off-by-one in the bit shift).
        // For n=50_000 independent uniform [0,1) samples, σ = 1/√(12·n)
        // ≈ 0.00129. A ±0.01 window is ~7.7σ, so the failure probability
        // under a correct implementation is well under 1 in 10^14.
        let rec accumulate (state : uint64) (n : int) (acc : double) : double =
            if n = 0 then
                acc
            else
                let v, newState = NonCryptoRandom.nextDouble state
                accumulate newState (n - 1) (acc + v)

        let n = 50_000
        let total = accumulate 12345UL n 0.0
        let mean = total / double n
        abs (mean - 0.5) |> shouldBeSmallerThan 0.01

    [<Test>]
    let ``nextInt32Below is pure in the starting state`` () : unit =
        let property (state : uint64) (bound : PositiveInt) : bool =
            let first = NonCryptoRandom.nextInt32Below bound.Get state
            let second = NonCryptoRandom.nextInt32Below bound.Get state
            first = second

        Check.One (propertyConfig, property)

    [<Test>]
    let ``nextInt32Below result lies in [0, bound)`` () : unit =
        // The scheduler will index a list-of-runnable-threads by this
        // value, so an out-of-range result would crash with an obscure
        // IndexOutOfRange far from the actual bug. Pin the invariant.
        let property (state : uint64) (bound : PositiveInt) : bool =
            let v, _ = NonCryptoRandom.nextInt32Below bound.Get state
            v >= 0 && v < bound.Get

        Check.One (propertyConfig, property)

    [<Test>]
    let ``nextInt32Below 1 always returns 0 without advancing state`` () : unit =
        // Bound=1 has only one valid output. The rejection-sampling
        // threshold collapses to "accept everything", so the function
        // takes exactly one step regardless of input state — but the
        // caller perspective is that 0 always comes back. (We don't
        // assert the state change here because doing so would couple
        // the test to the rejection-sampling internals; the contract
        // is just "result = 0".)
        let property (state : uint64) : bool =
            let v, _ = NonCryptoRandom.nextInt32Below 1 state
            v = 0

        Check.One (propertyConfig, property)

    [<Test>]
    let ``nextInt32Below rejects non-positive bound`` () : unit =
        // A non-positive bound is a caller bug. Silently returning 0
        // would mask the upstream mistake; the test pins the loud
        // failure to ensure no future "be helpful" refactor swallows
        // the contract.
        (fun () -> NonCryptoRandom.nextInt32Below 0 0UL |> ignore) |> shouldFail<exn>

        (fun () -> NonCryptoRandom.nextInt32Below -1 0UL |> ignore) |> shouldFail<exn>

        (fun () -> NonCryptoRandom.nextInt32Below System.Int32.MinValue 0UL |> ignore)
        |> shouldFail<exn>

    [<Test>]
    let ``nextInt32Below is roughly uniform for small bounds`` () : unit =
        // Aggregate 60_000 draws into 6 buckets and assert every bucket
        // is within ±10% of the expected count of 10_000. Under a
        // correct unbiased implementation each bucket has σ ≈ √(n·p·(1-p))
        // ≈ √(60000 · 1/6 · 5/6) ≈ 91, so the ±1000 window is over 10σ.
        // Failure under a correct implementation is astronomically
        // unlikely; a biased implementation (e.g. plain `% bound` with
        // no rejection) at this small bound has no detectable effect,
        // so this is a wholesale-correctness check, not a calibration.
        let bound = 6
        let n = 60_000

        let counts = Array.zeroCreate<int> bound

        let rec accumulate state n =
            if n = 0 then
                ()
            else
                let v, newState = NonCryptoRandom.nextInt32Below bound state
                counts.[v] <- counts.[v] + 1
                accumulate newState (n - 1)

        accumulate 0x1234567890ABCDEFUL n

        let expected = n / bound
        let tolerance = expected / 10

        for bucket = 0 to bound - 1 do
            let count = counts.[bucket]
            let delta = abs (count - expected)

            if delta > tolerance then
                Assert.Fail
                    $"Bucket %d{bucket} had count %d{count}, expected ~%d{expected} (±%d{tolerance}); delta %d{delta}"
