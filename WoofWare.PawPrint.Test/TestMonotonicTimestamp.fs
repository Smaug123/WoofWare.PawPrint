namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.monotonicTimestampNanos` is the value
/// `SystemNative_GetTimestamp` returns: nanoseconds since the simulated process
/// booted, derived from the deterministic virtual clock. CoreLib exposes it
/// unchanged as `Stopwatch.GetTimestamp()` and pairs it with a hard-coded
/// `Stopwatch.Frequency` of 1e9 (Stopwatch.Unix.cs), so the units are not ours
/// to choose and the conversion is worth pinning.
///
/// The sibling `TestSystemTimeAsTicks` covers the *wall* clock derived from the
/// same field; the cross-entry-point agreement between the two monotonic
/// readings (`SystemNative_GetTimestamp` and
/// `SystemNative_GetLowResolutionTimestamp`) is asserted here, because upstream
/// those are the same clock read at two resolutions.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMonotonicTimestamp =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private maxClockMs : int64 = EmulatedKernel.maxMonotonicTimestampClockMs

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// A kernel whose virtual clock has advanced to `clockMs`. Set by
    /// record-copy because the driver loop is its only production writer.
    let private kernelWith (clockMs : int64) : EmulatedKernel =
        { EmulatedKernel.initial with
            VirtualClockMs = clockMs
        }

    let private int64s = ArbMap.defaults |> ArbMap.arbitrary<int64>
    let private int64Pairs = ArbMap.defaults |> ArbMap.arbitrary<int64 * int64>

    [<Test>]
    let ``a default kernel boots at timestamp zero`` () =
        // Part of the replay contract: change this and every recorded trace's
        // Stopwatch readings change with it. Real CLOCK_MONOTONIC counts from
        // an unspecified origin (system boot on Linux), which is exactly the
        // kind of host dependence PawPrint exists to remove.
        EmulatedKernel.monotonicTimestampNanos EmulatedKernel.initial |> shouldEqual 0L

    [<Test>]
    let ``maxMonotonicTimestampClockMs is the last non-overflowing millisecond`` () =
        // Pinned against int64 arithmetic rather than against the literal, so a
        // slip in the literal is caught here.
        System.Int64.MaxValue / EmulatedKernel.nanosecondsPerMillisecond
        |> shouldEqual EmulatedKernel.maxMonotonicTimestampClockMs

        // The bound is tight, not merely safe: the boundary itself is
        // representable...
        maxClockMs * EmulatedKernel.nanosecondsPerMillisecond |> shouldBeGreaterThan 0L

        // ...and one millisecond further is not. The assertion is on the
        // wrapped int64 product itself, because a negative product is precisely
        // the failure the bound exists to prevent: a monotonic clock that had
        // run backwards.
        (maxClockMs + 1L) * EmulatedKernel.nanosecondsPerMillisecond
        |> shouldBeSmallerThan 0L

    [<Test>]
    let ``the bound is tighter than the wall clock's`` () =
        // A virtual-clock reading `systemTimeAsTicks` still accepts can be too
        // large for the nanosecond derivation. This is a real asymmetry between
        // the two clock views, not an oversight, and the guard exists precisely
        // because of it.
        maxClockMs < EmulatedKernel.maxWallClockEpochMs |> shouldEqual true

    [<Test>]
    let ``the timestamp is the virtual clock scaled to nanoseconds`` () =
        // The oracle is decimal arithmetic (exact for these magnitudes), so a
        // slip in the int64 multiply is not restated as its own oracle.
        let property (seed : int64) : bool =
            let clockMs = intoRange maxClockMs seed

            let expected = decimal clockMs * 1_000_000M

            decimal (EmulatedKernel.monotonicTimestampNanos (kernelWith clockMs)) = expected

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the two monotonic PAL readings agree`` () =
        // Upstream, `SystemNative_GetTimestamp` (minipal_hires_ticks) and
        // `SystemNative_GetLowResolutionTimestamp` (minipal_lowres_ticks) read
        // the same clock at nanosecond and millisecond resolution. A guest that
        // compares `Environment.TickCount64` against a `Stopwatch` must not see
        // them disagree, so the low-resolution reading has to be exactly the
        // high-resolution one truncated to milliseconds.
        let property (seed : int64) : bool =
            let clockMs = intoRange maxClockMs seed
            let kernel = kernelWith clockMs

            EmulatedKernel.monotonicTimestampNanos kernel
            / EmulatedKernel.nanosecondsPerMillisecond = kernel.VirtualClockMs

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the wall-clock epoch cannot perturb the monotonic clock`` () =
        // `CLOCK_MONOTONIC` is immune to wall-clock changes, and here that is
        // structural rather than enforced: `WallClockEpochMs` does not appear
        // in the derivation at all. Pinned anyway, because it is exactly the
        // property a future NTP-skew model would be at risk of quietly
        // breaking, and because every other property here holds the epoch at
        // zero.
        let property (epochSeed : int64, clockSeed : int64) : bool =
            let epochMs = intoRange EmulatedKernel.maxWallClockEpochMs epochSeed
            let clockMs = intoRange maxClockMs clockSeed

            let shifted =
                { kernelWith clockMs with
                    WallClockEpochMs = epochMs
                }

            EmulatedKernel.monotonicTimestampNanos shifted = EmulatedKernel.monotonicTimestampNanos (kernelWith clockMs)

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the monotonic and wall clocks agree about elapsed time`` () =
        // The point of deriving both from one field: a guest that times an
        // interval with `Stopwatch` and one that times it with
        // `DateTime.UtcNow` must get the same answer. Compared in 100ns ticks,
        // the coarser of the two units, which is also exactly the conversion
        // `Stopwatch` performs when it hands out a `TimeSpan`.
        let nanosPerTick : int64 = int64 System.TimeSpan.NanosecondsPerTick

        let property (firstSeed : int64, secondSeed : int64) : bool =
            // Constrained to the range legal for *both* clocks; the asymmetry
            // between their bounds has its own test above.
            let bound = min maxClockMs EmulatedKernel.maxWallClockEpochMs
            let first = intoRange bound firstSeed
            let second = intoRange bound secondSeed

            let elapsedNanos =
                EmulatedKernel.monotonicTimestampNanos (kernelWith second)
                - EmulatedKernel.monotonicTimestampNanos (kernelWith first)

            let elapsedTicks =
                EmulatedKernel.systemTimeAsTicks (kernelWith second)
                - EmulatedKernel.systemTimeAsTicks (kernelWith first)

            elapsedNanos / nanosPerTick = elapsedTicks

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the timestamp tracks the virtual clock strictly monotonically`` () =
        // `Stopwatch` never runs backwards, and never stands still while the
        // monotonic clock moves: guest code that polls until elapsed time
        // exceeds a threshold must make progress.
        let property (firstSeed : int64, secondSeed : int64) : bool =
            let first = intoRange maxClockMs firstSeed
            let second = intoRange maxClockMs secondSeed

            let firstNanos = EmulatedKernel.monotonicTimestampNanos (kernelWith first)
            let secondNanos = EmulatedKernel.monotonicTimestampNanos (kernelWith second)

            compare firstNanos secondNanos = compare first second

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``every reachable reading is a non-negative int64`` () =
        // The guard's whole purpose: a wrapped negative timestamp would make
        // every `Stopwatch` in the guest report a negative elapsed time.
        let property (seed : int64) : bool =
            EmulatedKernel.monotonicTimestampNanos (kernelWith (intoRange maxClockMs seed))
            >= 0L

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the reading has millisecond granularity`` () =
        // Documented consequence of deriving from a millisecond clock: every
        // timestamp is a multiple of 1,000,000 ns, so `Stopwatch` is not a
        // source of unique values here. Real `clock_gettime(CLOCK_MONOTONIC)`
        // makes no uniqueness guarantee either, so this is a faithful gap
        // rather than one to paper over.
        let property (seed : int64) : bool =
            let nanos =
                EmulatedKernel.monotonicTimestampNanos (kernelWith (intoRange maxClockMs seed))

            nanos % EmulatedKernel.nanosecondsPerMillisecond = 0L

        Check.One (propertyConfig, Prop.forAll int64s property)

    /// Did the thunk complete, rather than failing the way PawPrint reports a
    /// violated kernel invariant?
    let private succeeds (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            true
        with _ ->
            false

    [<Test>]
    let ``a clock outside the derivable range is rejected at the point of use`` () =
        // The virtual clock has no setter to validate, so `monotonicTimestampNanos`
        // is the only place the invariant can be asserted. It must fail loudly
        // rather than quietly wrapping.
        let property (clockMs : int64) : bool =
            let derivable = clockMs >= 0L && clockMs <= maxClockMs

            succeeds (fun () -> EmulatedKernel.monotonicTimestampNanos (kernelWith clockMs)) = derivable

        Check.One (propertyConfig, Prop.forAll int64s property)
