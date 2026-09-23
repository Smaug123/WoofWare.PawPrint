namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `ClockPal.monotonicTimestampNanos` is the value
/// `SystemNative_GetTimestamp` returns: nanoseconds since the simulated process
/// booted, read from the emulated kernel's monotonic clock, which the
/// deterministic virtual clock advances. CoreLib exposes it
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

    let private maxClockTicks : int64 = EmulatedKernel.maxVirtualClockTicks

    /// The machine a simulated process boots with. The two flavours read
    /// different clocks for these entry points, and every property here holds of
    /// both; see `flavours`.
    let private initialMachine : UnixMachineState = EmulatedKernel.initial.Machine

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// A kernel on `platform` whose virtual clock has advanced to `clockTicks`
    /// 100 ns ticks, through the setter the driver loop uses.
    let private kernelOn (platform : SimulatedUnixPlatform) (clockTicks : int64) : EmulatedKernel =
        EmulatedKernel.create platform
        |> EmulatedKernel.withVirtualClockTicks clockTicks

    let private machineOn (platform : SimulatedUnixPlatform) (clockTicks : int64) : UnixMachineState =
        (kernelOn platform clockTicks).Machine

    let private machineWith (clockTicks : int64) : UnixMachineState =
        machineOn SimulatedUnixPlatform.linuxX64 clockTicks

    /// Both flavours, which read different clocks for the same entry points:
    /// `CLOCK_MONOTONIC` and `CLOCK_MONOTONIC_COARSE` on Linux, `CLOCK_UPTIME_RAW`
    /// for both on Darwin.
    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private int64s = ArbMap.defaults |> ArbMap.arbitrary<int64>
    let private int64Pairs = ArbMap.defaults |> ArbMap.arbitrary<int64 * int64>

    [<Test>]
    let ``a default kernel boots at timestamp zero`` () =
        // Part of the replay contract: change this and every recorded trace's
        // Stopwatch readings change with it. Real CLOCK_MONOTONIC counts from
        // an unspecified origin (system boot on Linux), which is exactly the
        // kind of host dependence PawPrint exists to remove.
        ClockPal.monotonicTimestampNanos initialMachine |> shouldEqual 0L

        for platform in platforms do
            ClockPal.monotonicTimestampNanos (machineOn platform 0L) |> shouldEqual 0L
            ClockPal.lowResolutionTimestampMs (machineOn platform 0L) |> shouldEqual 0L

    [<Test>]
    let ``maxVirtualClockTicks is the last tick whose nanoseconds fit an int64`` () =
        // Pinned against int64 arithmetic rather than against the literal, so a
        // slip in the literal is caught here.
        System.Int64.MaxValue / ClockPal.nanosecondsPerTick
        |> shouldEqual EmulatedKernel.maxVirtualClockTicks

        // The bound is tight, not merely safe: the boundary itself is
        // representable...
        maxClockTicks * ClockPal.nanosecondsPerTick |> shouldBeGreaterThan 0L

        // ...and one millisecond further is not. The assertion is on the
        // wrapped int64 product itself, because a negative product is precisely
        // the failure the bound exists to prevent: a monotonic clock that had
        // run backwards.
        (maxClockTicks + 1L) * ClockPal.nanosecondsPerTick |> shouldBeSmallerThan 0L

    [<Test>]
    let ``the bound is tighter than the wall clock's`` () =
        // The virtual clock's horizon sits far inside the wall clock's ceiling in
        // the same unit (100 ns ticks), so the wall clock runs out only through a
        // late boot instant; `TestSystemTimeAsTicks` covers that case.
        maxClockTicks < ClockPal.maxWallClockTicks |> shouldEqual true

    [<Test>]
    let ``the timestamp is the virtual clock scaled to nanoseconds`` () =
        // The oracle is decimal arithmetic (exact for these magnitudes), so a
        // slip in the int64 multiply is not restated as its own oracle.
        let property (seed : int64) : bool =
            let clockTicks = intoRange maxClockTicks seed

            let expected = decimal clockTicks * 100M

            platforms
            |> List.forall (fun platform ->
                decimal (ClockPal.monotonicTimestampNanos (machineOn platform clockTicks)) = expected
            )

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the two monotonic PAL readings agree`` () =
        // Upstream, `SystemNative_GetTimestamp` (minipal_hires_ticks) and
        // `SystemNative_GetLowResolutionTimestamp` (minipal_lowres_ticks) read
        // the same clock at nanosecond and millisecond resolution. A guest that
        // compares `Environment.TickCount64` against a `Stopwatch` must not see
        // them disagree, so the low-resolution reading has to be exactly the
        // high-resolution one truncated to milliseconds.
        // Neither side restates the other's arithmetic: the left is the high-resolution PAL
        // reading converted from nanoseconds to milliseconds using the BCL's own factor, the
        // right is the low-resolution PAL reading. Compare the two *projections*, not a
        // projection against the clock field — the latter is a tautology about
        // `monotonicTimestampNanos` and covers the low-resolution one not at all.
        let property (seed : int64) : bool =
            platforms
            |> List.forall (fun platform ->
                let machine = machineOn platform (intoRange maxClockTicks seed)

                let hiResMs = ClockPal.monotonicTimestampNanos machine / 1_000_000L

                hiResMs = ClockPal.lowResolutionTimestampMs machine
            )

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the low-resolution reading is the clock truncated to milliseconds`` () =
        // Sub-millisecond clock values are the interesting ones: they are unreachable at the
        // current instruction cost but reachable at any finer one, and they are what the
        // agreement property above cannot distinguish if the conversion factor is wrong in a
        // way that happens to preserve whole milliseconds.
        for ticks, expected in
            [
                0L, 0L
                1L, 0L
                ClockPal.ticksPerMillisecond - 1L, 0L
                ClockPal.ticksPerMillisecond, 1L
                ClockPal.ticksPerMillisecond + 1L, 1L
                7L * ClockPal.ticksPerMillisecond - 1L, 6L
            ] do
            for platform in platforms do
                ClockPal.lowResolutionTimestampMs (machineOn platform ticks)
                |> shouldEqual expected

    [<Test>]
    let ``the wall-clock epoch cannot perturb the monotonic clock`` () =
        // `CLOCK_MONOTONIC` is immune to wall-clock changes, and here that is
        // structural rather than enforced: the boot instant does not appear in
        // the monotonic clock's reading at all. Pinned anyway, because it is exactly the
        // property a future NTP-skew model would be at risk of quietly
        // breaking, and because every other property here holds the epoch at
        // zero.
        let property (epochSeed : int64, clockSeed : int64) : bool =
            let epochMs = intoRange ClockPal.maxWallClockEpochMs epochSeed
            let clockMs = intoRange maxClockTicks clockSeed

            platforms
            |> List.forall (fun platform ->
                let shifted =
                    (EmulatedKernel.create platform
                     |> EmulatedKernel.withWallClockEpochMs epochMs
                     |> EmulatedKernel.withVirtualClockTicks clockMs)
                        .Machine

                ClockPal.monotonicTimestampNanos shifted = ClockPal.monotonicTimestampNanos (
                    machineOn platform clockMs
                )
            )

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the monotonic and wall clocks agree about elapsed time`` () =
        // The point of deriving both from one field: a guest that times an
        // interval with `Stopwatch` and one that times it with
        // `DateTime.UtcNow` must get the same answer. Compared in 100ns ticks,
        // the coarser of the two units, which is also exactly the conversion
        // `Stopwatch` performs when it hands out a `TimeSpan`. On Linux only:
        // Darwin's realtime clock reports whole microseconds, so there the two
        // agree only to the microsecond.
        let nanosPerTick : int64 = int64 System.TimeSpan.NanosecondsPerTick

        let property (firstSeed : int64, secondSeed : int64) : bool =
            // Constrained to the range legal for *both* clocks; the asymmetry
            // between their bounds has its own test above.
            let bound = min maxClockTicks ClockPal.maxWallClockEpochMs
            let first = intoRange bound firstSeed
            let second = intoRange bound secondSeed

            let elapsedNanos =
                ClockPal.monotonicTimestampNanos (machineWith second)
                - ClockPal.monotonicTimestampNanos (machineWith first)

            let elapsedTicks =
                ClockPal.systemTimeAsTicks (machineWith second)
                - ClockPal.systemTimeAsTicks (machineWith first)

            elapsedNanos / nanosPerTick = elapsedTicks

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the timestamp tracks the virtual clock strictly monotonically`` () =
        // `Stopwatch` never runs backwards, and never stands still while the
        // monotonic clock moves: guest code that polls until elapsed time
        // exceeds a threshold must make progress.
        let property (firstSeed : int64, secondSeed : int64) : bool =
            let first = intoRange maxClockTicks firstSeed
            let second = intoRange maxClockTicks secondSeed

            platforms
            |> List.forall (fun platform ->
                let firstNanos = ClockPal.monotonicTimestampNanos (machineOn platform first)

                let secondNanos = ClockPal.monotonicTimestampNanos (machineOn platform second)

                compare firstNanos secondNanos = compare first second
            )

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``every reachable reading is a non-negative int64`` () =
        // The guard's whole purpose: a wrapped negative timestamp would make
        // every `Stopwatch` in the guest report a negative elapsed time.
        let property (seed : int64) : bool =
            platforms
            |> List.forall (fun platform ->
                ClockPal.monotonicTimestampNanos (machineOn platform (intoRange maxClockTicks seed))
                >= 0L
            )

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the reading has 100ns granularity`` () =
        // Consequence of PawPrint advancing the clock only by whole 100 ns ticks:
        // every timestamp is a multiple of 100 ns. That is coarser than real
        // `clock_gettime(CLOCK_MONOTONIC)`, so `Stopwatch` is not a source of unique
        // values here — a faithful gap rather than one to paper over, since the real
        // thing makes no uniqueness guarantee either.
        let property (seed : int64) : bool =
            platforms
            |> List.forall (fun platform ->
                let nanos =
                    ClockPal.monotonicTimestampNanos (machineOn platform (intoRange maxClockTicks seed))

                nanos % ClockPal.nanosecondsPerTick = 0L
            )

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
    let ``a clock outside the derivable range is rejected`` () =
        // No reading may come from a clock outside the range whose nanoseconds
        // fit an int64: that must fail loudly rather than quietly wrapping. The
        // writer is where it fails, since the kernel cannot hold such a clock.
        let property (clockMs : int64) : bool =
            let derivable = clockMs >= 0L && clockMs <= maxClockTicks

            succeeds (fun () -> ClockPal.monotonicTimestampNanos (machineWith clockMs)) = derivable

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the clock writer rejects moving past the representable horizon`` () : unit =
        // Regression guard for a reachable overflow. A finite deadline is
        // `clock + timeoutMs * ticksPerMillisecond`; `Thread.Sleep(Int32.MaxValue)` adds about
        // 2.1e13 ticks, and the driver's deadline jump advances the clock to a deadline *without*
        // retiring a step. So a guest looping on that sleep reaches `Int64.MaxValue` in ~430,000
        // cheap iterations. Wrapping would hand the next sleeper a negative deadline that fires
        // immediately, and time would stop advancing — a silent wrong answer, so the writer
        // faults instead.
        let atHorizon =
            kernelOn SimulatedUnixPlatform.linuxX64 EmulatedKernel.maxVirtualClockTicks

        // The horizon itself is legal: a reading can still be derived from it.
        ClockPal.monotonicTimestampNanos atHorizon.Machine |> shouldBeGreaterThan 0L

        let beyond () =
            EmulatedKernel.withVirtualClockTicks (EmulatedKernel.maxVirtualClockTicks + 1L) atHorizon
            |> ignore<EmulatedKernel>

        Assert.Throws<Exception> (TestDelegate beyond) |> ignore<Exception>

    [<Test>]
    let ``the clock writer rejects moving backwards`` () : unit =
        // Monotonicity is the one guarantee every derived clock rests on, and `MapKernel` makes
        // it easy for a future caller to compute a smaller value by accident (a `min` for a
        // `max`, say). Cheap to assert at the writer.
        let kernel = kernelOn SimulatedUnixPlatform.linuxX64 5_000L

        EmulatedKernel.withVirtualClockTicks 5_000L kernel
        |> fun k -> k.VirtualClockTicks |> shouldEqual 5_000L

        let backwards () =
            EmulatedKernel.withVirtualClockTicks 4_999L kernel |> ignore<EmulatedKernel>

        Assert.Throws<Exception> (TestDelegate backwards) |> ignore<Exception>

    [<Test>]
    let ``the clock writer rejects negative targets even when moving forwards`` () : unit =
        // The monotonicity check alone waves this through: -10,000 is *greater* than -20,000, so
        // the move is forwards and only an independent non-negativity check catches it. Reachable
        // because a kernel assembled by record-copy never passed through the writer. Left
        // untested, the writer would enforce a narrower range than its own doc comment claims.
        let negativeKernel =
            { EmulatedKernel.initial with
                Machine =
                    { EmulatedKernel.initial.Machine with
                        NanosecondsSinceBoot = -20_000L * ClockPal.nanosecondsPerTick
                    }
            }

        let forwardsButNegative () =
            EmulatedKernel.withVirtualClockTicks -10_000L negativeKernel
            |> ignore<EmulatedKernel>

        Assert.Throws<Exception> (TestDelegate forwardsButNegative) |> ignore<Exception>

    [<Test>]
    let ``a clock advanced by part of a tick is refused rather than rounded`` () : unit =
        // PawPrint advances the kernel's clock only by whole ticks, which is what makes every
        // tick-denominated reading exact. A kernel whose clock something else advanced by a
        // fraction of one would read back rounded, so the view refuses it instead.
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.mapMachine (UnixMachineState.advanceClock 150L)

        let read () =
            kernel.VirtualClockTicks |> ignore<int64>

        Assert.Throws<Exception> (TestDelegate read) |> ignore<Exception>

        (EmulatedKernel.initial
         |> EmulatedKernel.mapMachine (UnixMachineState.advanceClock 200L))
            .VirtualClockTicks
        |> shouldEqual 2L
