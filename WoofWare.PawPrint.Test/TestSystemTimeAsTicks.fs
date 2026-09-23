namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `ClockPal.systemTimeAsTicks` is the value
/// `SystemNative_GetSystemTimeAsTicks` returns: 100ns ticks since the Unix
/// epoch, read from the emulated kernel's realtime clock, which is the boot
/// instant plus the deterministic virtual clock. CoreLib turns
/// it into `DateTime.UtcNow` with
/// `new DateTime(((ulong)(ticks + UnixEpochTicks)) | KindUtc)`
/// (DateTime.Unix.cs) — the *unvalidated* private ctor, so "every value we can
/// produce names a real DateTime" is a property this module has to establish
/// rather than one the BCL will enforce on our behalf.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSystemTimeAsTicks =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private maxEpochMs : int64 = ClockPal.maxWallClockEpochMs

    /// The machine a simulated process boots with, on Linux, whose realtime
    /// clock reports to the nanosecond. Darwin's reports whole microseconds, and
    /// has its own tests below.
    let private initialMachine : UnixMachineState =
        (EmulatedKernel.create SimulatedUnixPlatform.linuxX64).Machine

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// A kernel on `platform` booting at `epochMs` whose virtual clock has since
    /// advanced to `clockTicks` — note the units differ: the boot instant is a
    /// millisecond offset, which is the unit a host configures it in, while the
    /// clock is in the 100 ns ticks PawPrint counts it in. Reached through the
    /// setters `KernelConfig.toKernel` and the driver loop use.
    let private machineOn (platform : SimulatedUnixPlatform) (epochMs : int64) (clockTicks : int64) : UnixMachineState =
        (EmulatedKernel.create platform
         |> EmulatedKernel.withWallClockEpochMs epochMs
         |> EmulatedKernel.withVirtualClockTicks clockTicks)
            .Machine

    let private machineWith (epochMs : int64) (clockTicks : int64) : UnixMachineState =
        machineOn SimulatedUnixPlatform.linuxX64 epochMs clockTicks

    /// The guest-visible instant, computed exactly as CoreLib does but through
    /// the range-*checking* `DateTime` ctor, so a reading the private ctor would
    /// have silently corrupted surfaces here as an exception instead.
    let private guestUtcNow (machine : UnixMachineState) : DateTime =
        DateTime (DateTime.UnixEpoch.Ticks + ClockPal.systemTimeAsTicks machine, DateTimeKind.Utc)

    /// Draw an epoch (ms) and a virtual-clock reading (100 ns ticks) whose
    /// combination is still representable — i.e. exactly the states a
    /// legally-configured kernel can reach, with the wall clock inside
    /// `DateTime`'s range and the virtual clock inside
    /// `EmulatedKernel.maxVirtualClockTicks`.
    let private reachable (epochSeed : int64, clockSeed : int64) : int64 * int64 =
        let epochMs = intoRange maxEpochMs epochSeed

        let clockTicks =
            intoRange
                (min ((maxEpochMs - epochMs) * ClockPal.ticksPerMillisecond) EmulatedKernel.maxVirtualClockTicks)
                clockSeed

        epochMs, clockTicks

    let private int64Pairs = ArbMap.defaults |> ArbMap.arbitrary<int64 * int64>

    [<Test>]
    let ``maxWallClockEpochMs is the last millisecond DateTime can represent`` () =
        // Pinned against the BCL rather than against the arithmetic that
        // produced the literal, so a slip in that arithmetic is caught here.
        (DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks)
        / ClockPal.ticksPerMillisecond
        |> shouldEqual ClockPal.maxWallClockEpochMs

        guestUtcNow (machineWith maxEpochMs 0L)
        |> shouldEqual (DateTime (9999, 12, 31, 23, 59, 59, 999, DateTimeKind.Utc))

    [<Test>]
    let ``maxWallClockTicks is the last tick DateTime can represent`` () =
        // Also pinned against the BCL. The tempting derivation
        // `maxWallClockEpochMs * ticksPerMillisecond` is wrong by 9,999 ticks:
        // that is the last whole *millisecond*, and the clock resolves finer
        // than that, so deriving it would reject the final sub-millisecond of
        // representable time.
        DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks
        |> shouldEqual ClockPal.maxWallClockTicks

        ClockPal.maxWallClockTicks - maxEpochMs * ClockPal.ticksPerMillisecond
        |> shouldEqual (ClockPal.ticksPerMillisecond - 1L)

        // The last representable instant really is accepted, not rejected one
        // sub-millisecond early: this is the exact case the derived ceiling got
        // wrong, so assert the boundary itself rather than only the constant.
        guestUtcNow (machineWith maxEpochMs (ClockPal.ticksPerMillisecond - 1L))
        |> shouldEqual DateTime.MaxValue

    [<Test>]
    let ``a default kernel boots at the Unix epoch`` () =
        // The replay contract: change this and every recorded trace's timestamps
        // change with it.
        ClockPal.systemTimeAsTicks initialMachine |> shouldEqual 0L

        guestUtcNow initialMachine |> shouldEqual DateTime.UnixEpoch

    [<Test>]
    let ``every reachable reading names a representable UTC instant`` () =
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds
            let now = guestUtcNow (machineWith epochMs clockTicks)

            now.Kind = DateTimeKind.Utc
            && now >= DateTime.UnixEpoch
            && now <= DateTime.MaxValue

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the reading is the boot instant plus the elapsed virtual clock`` () =
        // The oracle is the BCL's own date arithmetic rather than a restatement
        // of the implementation's multiply.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds

            guestUtcNow (machineWith epochMs clockTicks) = DateTime.UnixEpoch
                .AddTicks(epochMs * ClockPal.ticksPerMillisecond)
                .AddTicks (clockTicks)

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``moving elapsed time into the boot instant is unobservable`` () =
        // Affine-ness: the guest cannot tell "booted at E, ran for C" apart from
        // "booted at E+C, ran for nothing". This is what makes the wall clock a
        // pure view of the monotonic one rather than an independent axis, and it
        // is the property that would have to be given up to model NTP steps.
        //
        // Only a *whole millisecond* of elapsed time can be moved, because the
        // boot instant is denominated in milliseconds and the clock is not. The
        // sub-millisecond remainder has to stay on the clock; that it does, and
        // that the reading is unchanged, is the substance of the property.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds
            let wholeMs = clockTicks / ClockPal.ticksPerMillisecond
            let remainder = clockTicks % ClockPal.ticksPerMillisecond

            ClockPal.systemTimeAsTicks (machineWith epochMs clockTicks) = ClockPal.systemTimeAsTicks (
                machineWith (epochMs + wholeMs) remainder
            )

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the reading tracks the virtual clock strictly monotonically`` () =
        // `DateTime.UtcNow` never goes backwards, and never stands still while
        // the monotonic clock moves: guest code that waits for the wall clock to
        // advance must make progress.
        let property (epochSeed : int64, firstSeed : int64, secondSeed : int64) : bool =
            let epochMs = intoRange maxEpochMs epochSeed
            let headroom = maxEpochMs - epochMs
            let first = intoRange headroom firstSeed
            let second = intoRange headroom secondSeed

            let firstTicks = ClockPal.systemTimeAsTicks (machineWith epochMs first)

            let secondTicks = ClockPal.systemTimeAsTicks (machineWith epochMs second)

            compare firstTicks secondTicks = compare first second

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64 * int64 * int64>) property)

    [<Test>]
    let ``the reading has the full 100ns granularity of DateTime`` () =
        // The clock counts 100 ns ticks and `DateTime.UtcNow` resolves every one of
        // them, which is as fine as `DateTime` itself goes and close to real
        // `clock_gettime(CLOCK_REALTIME)`. Stated as "the reading carries the clock's
        // sub-millisecond digits" rather than as a modulus, because the interesting
        // claim is that nothing is being rounded away.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds

            let ticks = ClockPal.systemTimeAsTicks (machineWith epochMs clockTicks)

            ticks % ClockPal.ticksPerMillisecond = clockTicks % ClockPal.ticksPerMillisecond

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the inode stamp is the same instant, in a timespec`` () =
        // `UnixMachineState.realtime` is what a write stamps on an inode's mtime
        // and ctime.
        // It must be a *re-denomination* of the wall clock rather than a second
        // clock: a guest that writes a file and then reads `DateTime.UtcNow` sees
        // two readings of one instant. Stated as an exact identity, because a
        // scaling mistake — nanoseconds where 100 ns ticks were meant, or the
        // seconds and the fraction crossed — still moves forward when a file is
        // written, so no "the timestamp advanced" test can see it.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds
            let machine = machineWith epochMs clockTicks

            let ticks = ClockPal.systemTimeAsTicks machine
            let stamp = UnixMachineState.realtime machine

            // Reassembled with the BCL's own arithmetic rather than by inverting
            // the implementation's division.
            let reassembled =
                DateTime.UnixEpoch
                    .AddSeconds(float (UnixTimestamp.seconds stamp))
                    .AddTicks (int64 (UnixTimestamp.nanoseconds stamp) / ClockPal.nanosecondsPerTick)

            UnixTimestamp.seconds stamp >= 0L
            && UnixTimestamp.nanoseconds stamp >= 0
            && UnixTimestamp.nanoseconds stamp < 1_000_000_000
            // PawPrint advances the clock only by whole 100 ns ticks, so the
            // nanosecond part can never carry a finer digit.
            && int64 (UnixTimestamp.nanoseconds stamp) % ClockPal.nanosecondsPerTick = 0L
            && reassembled = DateTime.UnixEpoch.AddTicks ticks

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``a default kernel stamps inodes at the Unix epoch`` () =
        UnixMachineState.realtime initialMachine |> shouldEqual UnixTimestamp.epoch

    /// Did the thunk complete, rather than failing the way PawPrint reports a
    /// violated kernel invariant?
    let private succeeds (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            true
        with _ ->
            false

    [<Test>]
    let ``withWallClockEpochMs accepts exactly the representable epochs`` () =
        let property (epochMs : int64) : bool =
            let representable = epochMs >= 0L && epochMs <= maxEpochMs

            let accepted =
                succeeds (fun () -> EmulatedKernel.withWallClockEpochMs epochMs EmulatedKernel.initial)

            accepted = representable

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``a wall clock run past DateTime's range is rejected at the point of use`` () =
        // A legal boot instant late in `DateTime`'s range and a legal uptime can
        // together name an instant past its end, so `systemTimeAsTicks` must
        // assert: a guest must never observe a tick count naming no `DateTime`,
        // and it must fail loudly rather than quietly wrapping.
        let property (epochSeed : int64, overshootSeed : int64) : bool =
            // Late enough that the virtual clock can reach past the end.
            let earliest =
                maxEpochMs - EmulatedKernel.maxVirtualClockTicks / ClockPal.ticksPerMillisecond
                + 1_000L

            let epochMs = earliest + intoRange (maxEpochMs - earliest) epochSeed

            let headroom = ClockPal.maxWallClockTicks - epochMs * ClockPal.ticksPerMillisecond
            // Strictly past the representable end of time.
            let clockTicks = headroom + 1L + intoRange 1_000_000L overshootSeed

            not (succeeds (fun () -> ClockPal.systemTimeAsTicks (machineWith epochMs clockTicks)))

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    /// A Darwin kernel's realtime clock reports whole microseconds through
    /// `clock_gettime`, and the shim reads it there, so `DateTime.UtcNow` on the
    /// Darwin flavour drops the tick digit a Linux one keeps.
    [<Test>]
    let ``the Darwin flavour reports whole microseconds`` () =
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds

            let darwin =
                ClockPal.systemTimeAsTicks (machineOn SimulatedUnixPlatform.macOsArm64 epochMs clockTicks)

            let linux = ClockPal.systemTimeAsTicks (machineWith epochMs clockTicks)

            darwin = linux - linux % 10L

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

        // Pinned where the two differ, so the property is not vacuously true of a
        // Darwin flavour that happened to read the Linux clock.
        ClockPal.systemTimeAsTicks (machineOn SimulatedUnixPlatform.macOsArm64 0L 17L)
        |> shouldEqual 10L

        ClockPal.systemTimeAsTicks (machineWith 0L 17L) |> shouldEqual 17L

    /// The inode stamp is the kernel's own realtime clock, which is not truncated
    /// on Darwin: only `clock_gettime` reports whole microseconds.
    [<Test>]
    let ``the Darwin flavour stamps inodes to the tick`` () =
        UnixMachineState.realtime (machineOn SimulatedUnixPlatform.macOsArm64 0L 17L)
        |> shouldEqual (UnixTimestamp.createOrFail "TestSystemTimeAsTicks" 0L 1_700)
