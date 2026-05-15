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
