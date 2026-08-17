namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedKernel.systemTimeAsTicks` is the value
/// `SystemNative_GetSystemTimeAsTicks` returns: 100ns ticks since the Unix
/// epoch, derived affinely from the deterministic virtual clock. CoreLib turns
/// it into `DateTime.UtcNow` with
/// `new DateTime(((ulong)(ticks + UnixEpochTicks)) | KindUtc)`
/// (DateTime.Unix.cs) — the *unvalidated* private ctor, so "every value we can
/// produce names a real DateTime" is a property this module has to establish
/// rather than one the BCL will enforce on our behalf.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSystemTimeAsTicks =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private maxEpochMs : int64 = EmulatedKernel.maxWallClockEpochMs

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// A kernel booting at `epochMs` whose virtual clock has since advanced to
    /// `clockTicks` — note the units differ: the boot instant is a millisecond
    /// offset (that is what `KernelConfig` takes), while the clock is in the
    /// 100 ns ticks it is denominated in.
    let private kernelWith (epochMs : int64) (clockTicks : int64) : EmulatedKernel =
        let kernel = EmulatedKernel.initial |> EmulatedKernel.withWallClockEpochMs epochMs

        // The clock is set by record-copy because the driver loop is its only
        // production writer.
        { kernel with
            VirtualClockTicks = clockTicks
        }

    /// The guest-visible instant, computed exactly as CoreLib does but through
    /// the range-*checking* `DateTime` ctor, so a reading the private ctor would
    /// have silently corrupted surfaces here as an exception instead.
    let private guestUtcNow (kernel : EmulatedKernel) : DateTime =
        DateTime (DateTime.UnixEpoch.Ticks + EmulatedKernel.systemTimeAsTicks kernel, DateTimeKind.Utc)

    /// Draw an epoch (ms) and a virtual-clock reading (100 ns ticks) whose
    /// combination is still representable — i.e. exactly the states a
    /// legally-configured kernel can reach.
    let private reachable (epochSeed : int64, clockSeed : int64) : int64 * int64 =
        let epochMs = intoRange maxEpochMs epochSeed

        let clockTicks =
            intoRange ((maxEpochMs - epochMs) * EmulatedKernel.ticksPerMillisecond) clockSeed

        epochMs, clockTicks

    let private int64Pairs = ArbMap.defaults |> ArbMap.arbitrary<int64 * int64>

    [<Test>]
    let ``maxWallClockEpochMs is the last millisecond DateTime can represent`` () =
        // Pinned against the BCL rather than against the arithmetic that
        // produced the literal, so a slip in that arithmetic is caught here.
        (DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks)
        / EmulatedKernel.ticksPerMillisecond
        |> shouldEqual EmulatedKernel.maxWallClockEpochMs

        guestUtcNow (kernelWith maxEpochMs 0L)
        |> shouldEqual (DateTime (9999, 12, 31, 23, 59, 59, 999, DateTimeKind.Utc))

    [<Test>]
    let ``maxWallClockTicks is the last tick DateTime can represent`` () =
        // Also pinned against the BCL. The tempting derivation
        // `maxWallClockEpochMs * ticksPerMillisecond` is wrong by 9,999 ticks:
        // that is the last whole *millisecond*, and the clock resolves finer
        // than that, so deriving it would reject the final sub-millisecond of
        // representable time.
        DateTime.MaxValue.Ticks - DateTime.UnixEpoch.Ticks
        |> shouldEqual EmulatedKernel.maxWallClockTicks

        EmulatedKernel.maxWallClockTicks
        - maxEpochMs * EmulatedKernel.ticksPerMillisecond
        |> shouldEqual (EmulatedKernel.ticksPerMillisecond - 1L)

        // The last representable instant really is accepted, not rejected one
        // sub-millisecond early: this is the exact case the derived ceiling got
        // wrong, so assert the boundary itself rather than only the constant.
        guestUtcNow (kernelWith maxEpochMs (EmulatedKernel.ticksPerMillisecond - 1L))
        |> shouldEqual DateTime.MaxValue

    [<Test>]
    let ``a default kernel boots at the Unix epoch`` () =
        // The replay contract: change this and every recorded trace's timestamps
        // change with it.
        EmulatedKernel.systemTimeAsTicks EmulatedKernel.initial |> shouldEqual 0L
        guestUtcNow EmulatedKernel.initial |> shouldEqual DateTime.UnixEpoch

    [<Test>]
    let ``every reachable reading names a representable UTC instant`` () =
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds
            let now = guestUtcNow (kernelWith epochMs clockTicks)

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

            guestUtcNow (kernelWith epochMs clockTicks) = DateTime.UnixEpoch
                .AddTicks(epochMs * EmulatedKernel.ticksPerMillisecond)
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
            let wholeMs = clockTicks / EmulatedKernel.ticksPerMillisecond
            let remainder = clockTicks % EmulatedKernel.ticksPerMillisecond

            EmulatedKernel.systemTimeAsTicks (kernelWith epochMs clockTicks) = EmulatedKernel.systemTimeAsTicks (
                kernelWith (epochMs + wholeMs) remainder
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

            let firstTicks = EmulatedKernel.systemTimeAsTicks (kernelWith epochMs first)
            let secondTicks = EmulatedKernel.systemTimeAsTicks (kernelWith epochMs second)

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

            let ticks = EmulatedKernel.systemTimeAsTicks (kernelWith epochMs clockTicks)
            ticks % EmulatedKernel.ticksPerMillisecond = clockTicks % EmulatedKernel.ticksPerMillisecond

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the inode stamp is the same instant, in a timespec`` () =
        // `fileTimestamp` is what a write stamps on an inode's mtime and ctime.
        // It must be a *re-denomination* of the wall clock rather than a second
        // clock: a guest that writes a file and then reads `DateTime.UtcNow` sees
        // two readings of one instant. Stated as an exact identity, because a
        // scaling mistake — nanoseconds where 100 ns ticks were meant, or the
        // seconds and the fraction crossed — still moves forward when a file is
        // written, so no "the timestamp advanced" test can see it.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockTicks = reachable seeds
            let kernel = kernelWith epochMs clockTicks

            let ticks = EmulatedKernel.systemTimeAsTicks kernel
            let stamp = EmulatedKernel.fileTimestamp kernel

            // Reassembled with the BCL's own arithmetic rather than by inverting
            // the implementation's division.
            let reassembled =
                DateTime.UnixEpoch
                    .AddSeconds(float (UnixTimestamp.seconds stamp))
                    .AddTicks (int64 (UnixTimestamp.nanoseconds stamp) / EmulatedKernel.nanosecondsPerTick)

            UnixTimestamp.seconds stamp >= 0L
            && UnixTimestamp.nanoseconds stamp >= 0
            && UnixTimestamp.nanoseconds stamp < 1_000_000_000
            // 100 ns is the clock's own quantum, so the nanosecond part can never
            // carry a finer digit.
            && int64 (UnixTimestamp.nanoseconds stamp) % EmulatedKernel.nanosecondsPerTick = 0L
            && reassembled = DateTime.UnixEpoch.AddTicks ticks

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``a default kernel stamps inodes at the Unix epoch`` () =
        EmulatedKernel.fileTimestamp EmulatedKernel.initial
        |> shouldEqual UnixTimestamp.epoch

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
                succeeds (fun () -> EmulatedKernel.initial |> EmulatedKernel.withWallClockEpochMs epochMs)

            accepted = representable

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``a kernel record-copied out of range is rejected at the point of use`` () =
        // The setter can be bypassed by record-copy, so `systemTimeAsTicks`
        // re-asserts: a guest must never observe a tick count naming no
        // `DateTime`, and it must fail loudly rather than quietly wrapping.
        let property (epochSeed : int64, overshootSeed : int64) : bool =
            let epochMs = intoRange maxEpochMs epochSeed

            let headroom =
                EmulatedKernel.maxWallClockTicks - epochMs * EmulatedKernel.ticksPerMillisecond
            // Strictly past the representable end of time.
            let clockTicks = headroom + 1L + intoRange 1_000_000L overshootSeed

            let kernel =
                { EmulatedKernel.initial with
                    WallClockEpochMs = epochMs
                    VirtualClockTicks = clockTicks
                }

            not (succeeds (fun () -> EmulatedKernel.systemTimeAsTicks kernel))

        Check.One (propertyConfig, Prop.forAll int64Pairs property)
