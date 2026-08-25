namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `UnixMachineState.monotonicTimestampNanos` is the value
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

    let private maxClockTicks : int64 = UnixMachineState.maxMonotonicTimestampClockTicks

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// A kernel whose virtual clock has advanced to the given number of
    /// 100 ns ticks (despite the parameter's name, this is `VirtualClockTicks`,
    /// not milliseconds). Set by record-copy because the driver loop is its
    /// only production writer.
    let private kernelWith (clockMs : int64) : EmulatedKernel =
        { EmulatedKernel.initial with
            Machine =
                { EmulatedKernel.initial.Machine with
                    VirtualClockTicks = clockMs
                }
        }

    let private int64s = ArbMap.defaults |> ArbMap.arbitrary<int64>
    let private int64Pairs = ArbMap.defaults |> ArbMap.arbitrary<int64 * int64>

    [<Test>]
    let ``a default kernel boots at timestamp zero`` () =
        // Part of the replay contract: change this and every recorded trace's
        // Stopwatch readings change with it. Real CLOCK_MONOTONIC counts from
        // an unspecified origin (system boot on Linux), which is exactly the
        // kind of host dependence PawPrint exists to remove.
        UnixMachineState.monotonicTimestampNanos EmulatedKernel.initial.Machine
        |> shouldEqual 0L

    [<Test>]
    let ``maxMonotonicTimestampClockTicks is the last non-overflowing millisecond`` () =
        // Pinned against int64 arithmetic rather than against the literal, so a
        // slip in the literal is caught here.
        System.Int64.MaxValue / UnixMachineState.nanosecondsPerTick
        |> shouldEqual UnixMachineState.maxMonotonicTimestampClockTicks

        // The bound is tight, not merely safe: the boundary itself is
        // representable...
        maxClockTicks * UnixMachineState.nanosecondsPerTick |> shouldBeGreaterThan 0L

        // ...and one millisecond further is not. The assertion is on the
        // wrapped int64 product itself, because a negative product is precisely
        // the failure the bound exists to prevent: a monotonic clock that had
        // run backwards.
        (maxClockTicks + 1L) * UnixMachineState.nanosecondsPerTick
        |> shouldBeSmallerThan 0L

    [<Test>]
    let ``the bound is tighter than the wall clock's`` () =
        // A virtual-clock reading `systemTimeAsTicks` still accepts can be too
        // large for the nanosecond derivation: a real asymmetry between the
        // two clock views. Compared against the wall clock's ceiling in the
        // same unit (100 ns ticks).
        maxClockTicks < UnixMachineState.maxWallClockTicks |> shouldEqual true

    [<Test>]
    let ``the timestamp is the virtual clock scaled to nanoseconds`` () =
        // The oracle is decimal arithmetic (exact for these magnitudes), so a
        // slip in the int64 multiply is not restated as its own oracle.
        let property (seed : int64) : bool =
            let clockTicks = intoRange maxClockTicks seed

            let expected = decimal clockTicks * 100M

            decimal (UnixMachineState.monotonicTimestampNanos (kernelWith clockTicks).Machine) = expected

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
            let kernel = kernelWith (intoRange maxClockTicks seed)

            let hiResMs = UnixMachineState.monotonicTimestampNanos kernel.Machine / 1_000_000L

            hiResMs = UnixMachineState.lowResolutionTimestampMs kernel.Machine

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
                UnixMachineState.ticksPerMillisecond - 1L, 0L
                UnixMachineState.ticksPerMillisecond, 1L
                UnixMachineState.ticksPerMillisecond + 1L, 1L
                7L * UnixMachineState.ticksPerMillisecond - 1L, 6L
            ] do
            UnixMachineState.lowResolutionTimestampMs (kernelWith ticks).Machine
            |> shouldEqual expected

    [<Test>]
    let ``the wall-clock epoch cannot perturb the monotonic clock`` () =
        // `CLOCK_MONOTONIC` is immune to wall-clock changes, and here that is
        // structural rather than enforced: `WallClockEpochMs` does not appear
        // in the derivation at all. Pinned anyway, because it is exactly the
        // property a future NTP-skew model would be at risk of quietly
        // breaking, and because every other property here holds the epoch at
        // zero.
        let property (epochSeed : int64, clockSeed : int64) : bool =
            let epochMs = intoRange UnixMachineState.maxWallClockEpochMs epochSeed
            let clockMs = intoRange maxClockTicks clockSeed

            let shifted =
                let baseKernel = kernelWith clockMs

                { baseKernel with
                    Machine =
                        { baseKernel.Machine with
                            WallClockEpochMs = epochMs
                        }
                }

            UnixMachineState.monotonicTimestampNanos shifted.Machine = UnixMachineState.monotonicTimestampNanos
                (kernelWith clockMs).Machine

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
            let bound = min maxClockTicks UnixMachineState.maxWallClockEpochMs
            let first = intoRange bound firstSeed
            let second = intoRange bound secondSeed

            let elapsedNanos =
                UnixMachineState.monotonicTimestampNanos (kernelWith second).Machine
                - UnixMachineState.monotonicTimestampNanos (kernelWith first).Machine

            let elapsedTicks =
                UnixMachineState.systemTimeAsTicks (kernelWith second).Machine
                - UnixMachineState.systemTimeAsTicks (kernelWith first).Machine

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

            let firstNanos = UnixMachineState.monotonicTimestampNanos (kernelWith first).Machine

            let secondNanos =
                UnixMachineState.monotonicTimestampNanos (kernelWith second).Machine

            compare firstNanos secondNanos = compare first second

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``every reachable reading is a non-negative int64`` () =
        // The guard's whole purpose: a wrapped negative timestamp would make
        // every `Stopwatch` in the guest report a negative elapsed time.
        let property (seed : int64) : bool =
            UnixMachineState.monotonicTimestampNanos (kernelWith (intoRange maxClockTicks seed)).Machine
            >= 0L

        Check.One (propertyConfig, Prop.forAll int64s property)

    [<Test>]
    let ``the reading has 100ns granularity`` () =
        // Documented consequence of deriving from a 100 ns clock: every timestamp is
        // a multiple of 100 ns. That is coarser than real
        // `clock_gettime(CLOCK_MONOTONIC)`, so `Stopwatch` is not a source of unique
        // values here — a faithful gap rather than one to paper over, since the real
        // thing makes no uniqueness guarantee either.
        let property (seed : int64) : bool =
            let nanos =
                UnixMachineState.monotonicTimestampNanos (kernelWith (intoRange maxClockTicks seed)).Machine

            nanos % UnixMachineState.nanosecondsPerTick = 0L

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
            let derivable = clockMs >= 0L && clockMs <= maxClockTicks

            succeeds (fun () -> UnixMachineState.monotonicTimestampNanos (kernelWith clockMs).Machine) = derivable

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
            { EmulatedKernel.initial with
                Machine =
                    { EmulatedKernel.initial.Machine with
                        VirtualClockTicks = UnixMachineState.maxMonotonicTimestampClockTicks
                    }
            }

        // The horizon itself is legal: a reading can still be derived from it.
        UnixMachineState.monotonicTimestampNanos atHorizon.Machine
        |> shouldBeGreaterThan 0L

        let beyond () =
            EmulatedKernel.mapMachine
                (UnixMachineState.withVirtualClockTicks (UnixMachineState.maxMonotonicTimestampClockTicks + 1L))
                atHorizon
            |> ignore<EmulatedKernel>

        Assert.Throws<Exception> (TestDelegate beyond) |> ignore<Exception>

    [<Test>]
    let ``the clock writer rejects moving backwards`` () : unit =
        // Monotonicity is the one guarantee every derived clock rests on, and `MapKernel` makes
        // it easy for a future caller to compute a smaller value by accident (a `min` for a
        // `max`, say). Cheap to assert at the writer.
        let kernel =
            { EmulatedKernel.initial with
                Machine =
                    { EmulatedKernel.initial.Machine with
                        VirtualClockTicks = 5_000L
                    }
            }

        EmulatedKernel.mapMachine (UnixMachineState.withVirtualClockTicks 5_000L) kernel
        |> fun k -> k.VirtualClockTicks |> shouldEqual 5_000L

        let backwards () =
            EmulatedKernel.mapMachine (UnixMachineState.withVirtualClockTicks 4_999L) kernel
            |> ignore<EmulatedKernel>

        Assert.Throws<Exception> (TestDelegate backwards) |> ignore<Exception>

    [<Test>]
    let ``the clock writer rejects negative targets even when moving forwards`` () : unit =
        // The monotonicity check alone waves this through: -10,000 is *greater* than -20,000, so
        // the move is forwards and only an independent non-negativity check catches it. Reachable
        // because a kernel assembled by record-copy never passed through the writer, which is the
        // same reason the per-reader guards exist. Left untested, the writer would enforce a
        // narrower range than its own doc comment claims.
        let negativeKernel =
            { EmulatedKernel.initial with
                Machine =
                    { EmulatedKernel.initial.Machine with
                        VirtualClockTicks = -20_000L
                    }
            }

        let forwardsButNegative () =
            EmulatedKernel.mapMachine (UnixMachineState.withVirtualClockTicks -10_000L) negativeKernel
            |> ignore<EmulatedKernel>

        Assert.Throws<Exception> (TestDelegate forwardsButNegative) |> ignore<Exception>

    [<Test>]
    let ``the instruction cost is configurable and validated`` () : unit =
        // The rate is guest-observable — a guest can measure it by counting work against
        // `Environment.TickCount64`, and it decides whether `SpinWait` reaches its blocking
        // rung — so it is part of the replay contract and belongs in `KernelConfig` rather than
        // being a constant a host cannot see.
        KernelConfig.Default.InstructionCostTicks
        |> shouldEqual EmulatedKernel.defaultInstructionCostTicks

        let configured =
            EmulatedKernel.initial
            |> KernelConfig.applyTo
                { KernelConfig.Default with
                    InstructionCostTicks = 10_000L
                }

        configured.InstructionCostTicks |> shouldEqual 10_000L

        // Zero would freeze the clock, so every guest waiting for time to pass would spin
        // forever: a hang rather than a wrong answer, and the sort of thing a host sweeping the
        // knob could reach by off-by-one. Rejected at the setter, like `ProcessorCount`.
        for bad in [ 0L ; -1L ] do
            let apply () =
                EmulatedKernel.initial
                |> KernelConfig.applyTo
                    { KernelConfig.Default with
                        InstructionCostTicks = bad
                    }
                |> ignore<EmulatedKernel>

            Assert.Throws<Exception> (TestDelegate apply) |> ignore<Exception>
