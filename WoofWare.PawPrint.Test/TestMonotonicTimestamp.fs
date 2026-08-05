namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.monotonicTimestampNs` is the value
/// `SystemNative_GetTimestamp` returns: nanoseconds on a monotonic clock with
/// an arbitrary origin, which CoreLib surfaces unchanged as
/// `Stopwatch.GetTimestamp()` (Stopwatch.Unix.cs). Unlike the wall clock there
/// is no BCL type to bound the range, so the constraint this module has to
/// establish is arithmetic: the derivation must never overflow int64, because
/// the one thing a *monotonic* clock promises its callers is that it does not
/// run backwards.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMonotonicTimestamp =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private maxClockMs : int64 = EmulatedKernel.maxMonotonicClockMs

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// A kernel whose virtual clock has advanced to `clockMs`. Set by
    /// record-copy because the driver loop is its only production writer.
    let private kernelAt (clockMs : int64) : EmulatedKernel =
        { EmulatedKernel.initial with
            VirtualClockMs = clockMs
        }

    let private int64s = ArbMap.defaults |> ArbMap.arbitrary<int64>
    let private int64Pairs = ArbMap.defaults |> ArbMap.arbitrary<int64 * int64>

    /// Did the thunk complete, rather than failing the way PawPrint reports a
    /// violated kernel invariant?
    let private succeeds (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            true
        with _ ->
            false

    [<Test>]
    let ``the nanosecond scale is the one CoreLib fixes`` () =
        // `Stopwatch` on Unix never asks the PAL for its frequency:
        // `Stopwatch.Unix.cs`'s `GetFrequency()` returns the literal
        // 1_000_000_000, so the unit of this PAL entry is nanoseconds whether we
        // like it or not. Anchored on the BCL's own constants rather than on a
        // repeated literal, so this fails if the scale factor is ever retyped.
        EmulatedKernel.nanosecondsPerMillisecond
        |> shouldEqual (TimeSpan.TicksPerMillisecond * int64 TimeSpan.NanosecondsPerTick)

        EmulatedKernel.nanosecondsPerMillisecond |> shouldEqual 1_000_000L

    [<Test>]
    let ``maxMonotonicClockMs is the last reading that does not overflow`` () =
        // Pinned against the arithmetic that defines it, so a mistyped literal
        // is caught rather than merely being self-consistent.
        maxClockMs
        |> shouldEqual (Int64.MaxValue / EmulatedKernel.nanosecondsPerMillisecond)

        // The boundary itself is representable...
        maxClockMs * EmulatedKernel.nanosecondsPerMillisecond |> shouldBeGreaterThan 0L

        // ...and one millisecond further is not, which is precisely why the
        // bound exists: unchecked, this would wrap to a negative timestamp and
        // hand the guest a monotonic clock that had run backwards.
        (maxClockMs + 1L) * EmulatedKernel.nanosecondsPerMillisecond
        |> shouldBeSmallerThan 0L

    [<Test>]
    let ``a default kernel boots at zero`` () =
        // Part of the replay contract, and a deliberate choice: real
        // `CLOCK_MONOTONIC` promises nothing about its origin, so a guest
        // reading meaning into an absolute timestamp is broken upstream too.
        EmulatedKernel.monotonicTimestampNs EmulatedKernel.initial |> shouldEqual 0L

    [<Test>]
    let ``the reading tracks the virtual clock strictly monotonically`` () =
        // The defining property of the clock CoreLib thinks it is reading: it
        // never runs backwards, and never stands still while time passes.
        let property (firstSeed : int64, secondSeed : int64) : bool =
            let first = intoRange maxClockMs firstSeed
            let second = intoRange maxClockMs secondSeed

            let firstNs = EmulatedKernel.monotonicTimestampNs (kernelAt first)
            let secondNs = EmulatedKernel.monotonicTimestampNs (kernelAt second)

            compare firstNs secondNs = compare first second

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the reading is non-negative`` () =
        let property (seed : int64) : bool =
            EmulatedKernel.monotonicTimestampNs (kernelAt (intoRange maxClockMs seed)) >= 0L

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the wall-clock epoch cannot perturb the monotonic clock`` () =
        // `CLOCK_MONOTONIC` is immune to wall-clock changes, and here that is
        // structural rather than enforced: `WallClockEpochMs` does not appear in
        // the derivation at all. Pinned anyway, because it is the property a
        // future NTP-skew model would be at risk of quietly breaking.
        let property (epochSeed : int64, clockSeed : int64) : bool =
            let epochMs = intoRange EmulatedKernel.maxWallClockEpochMs epochSeed
            let clockMs = intoRange maxClockMs clockSeed

            let withEpoch =
                { kernelAt clockMs with
                    WallClockEpochMs = epochMs
                }

            EmulatedKernel.monotonicTimestampNs withEpoch = EmulatedKernel.monotonicTimestampNs (kernelAt clockMs)

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the monotonic and wall clocks agree about elapsed time`` () =
        // The point of deriving both from one field: a guest that times an
        // interval with `Stopwatch` and one that times it with `DateTime.UtcNow`
        // must get the same answer. Compared in 100ns ticks, the coarser of the
        // two units, since that is exactly the conversion `Stopwatch` performs
        // when it hands out a `TimeSpan`.
        let nsPerTick : int64 = int64 TimeSpan.NanosecondsPerTick

        let property (firstSeed : int64, secondSeed : int64) : bool =
            // Constrained to the range legal for *both* clocks; the asymmetry
            // between their bounds gets its own test below.
            let bound = min maxClockMs EmulatedKernel.maxWallClockEpochMs
            let first = intoRange bound firstSeed
            let second = intoRange bound secondSeed

            let elapsedNs =
                EmulatedKernel.monotonicTimestampNs (kernelAt second)
                - EmulatedKernel.monotonicTimestampNs (kernelAt first)

            let elapsedTicks =
                EmulatedKernel.systemTimeAsTicks (kernelAt second)
                - EmulatedKernel.systemTimeAsTicks (kernelAt first)

            elapsedNs / nsPerTick = elapsedTicks

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the reading has millisecond granularity`` () =
        // Documented consequence of deriving from a millisecond clock: the low
        // six digits are always zero, so `Stopwatch` cannot resolve anything
        // finer than one retired IL instruction. Not observable in a run — the
        // scheduler advances the clock a whole millisecond per instruction, so
        // no two reads separated by guest code collide — but it is the reason
        // `Stopwatch.Frequency` overstates the real resolution by a factor of a
        // million, and that is worth pinning rather than discovering.
        let property (seed : int64) : bool =
            let ns = EmulatedKernel.monotonicTimestampNs (kernelAt (intoRange maxClockMs seed))
            ns % EmulatedKernel.nanosecondsPerMillisecond = 0L

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``a kernel record-copied out of range is rejected at the point of use`` () =
        // Nothing bounds `VirtualClockMs` at its writer, and record-copy bypasses
        // any setter, so the derivation re-asserts: past the horizon the guest
        // must get a loud failure and not a wrapped, negative timestamp.
        let property (overshootSeed : int64) : bool =
            let clockMs = maxClockMs + 1L + intoRange 1_000_000L overshootSeed
            not (succeeds (fun () -> EmulatedKernel.monotonicTimestampNs (kernelAt clockMs)))

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``a negative virtual clock is rejected`` () =
        let property (seed : int64) : bool =
            let clockMs = -1L - intoRange 1_000_000L seed
            not (succeeds (fun () -> EmulatedKernel.monotonicTimestampNs (kernelAt clockMs)))

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the monotonic clock is bounded more tightly than the wall clock`` () =
        // Pins a real asymmetry rather than asserting it is fine. There is a
        // band of virtual-clock readings from which `DateTime.UtcNow` and
        // `Environment.TickCount64` can still be derived but
        // `Stopwatch.GetTimestamp` cannot, because nanoseconds run out of int64
        // long before `DateTime` runs out of years. Nothing bounds the field
        // centrally, so each clock enforces its own ceiling at the moment the
        // guest reads it; if that is ever fixed at the scheduler, this test
        // should start failing and be deleted along with the asymmetry.
        maxClockMs |> shouldBeSmallerThan EmulatedKernel.maxWallClockEpochMs

        let inBand = maxClockMs + 1L

        succeeds (fun () -> EmulatedKernel.systemTimeAsTicks (kernelAt inBand))
        |> shouldEqual true

        succeeds (fun () -> EmulatedKernel.monotonicTimestampNs (kernelAt inBand))
        |> shouldEqual false
