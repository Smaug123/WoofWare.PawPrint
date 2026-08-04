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
    /// `clockMs`. The clock is set by record-copy because the driver loop is its
    /// only production writer.
    let private kernelWith (epochMs : int64) (clockMs : int64) : EmulatedKernel =
        let kernel = EmulatedKernel.initial |> EmulatedKernel.withWallClockEpochMs epochMs

        { kernel with
            VirtualClockMs = clockMs
        }

    /// The guest-visible instant, computed exactly as CoreLib does but through
    /// the range-*checking* `DateTime` ctor, so a reading the private ctor would
    /// have silently corrupted surfaces here as an exception instead.
    let private guestUtcNow (kernel : EmulatedKernel) : DateTime =
        DateTime (DateTime.UnixEpoch.Ticks + EmulatedKernel.systemTimeAsTicks kernel, DateTimeKind.Utc)

    /// Draw an epoch and a virtual-clock reading whose sum is still
    /// representable — i.e. exactly the states a legally-configured kernel can
    /// reach.
    let private reachable (epochSeed : int64, clockSeed : int64) : int64 * int64 =
        let epochMs = intoRange maxEpochMs epochSeed
        let clockMs = intoRange (maxEpochMs - epochMs) clockSeed
        epochMs, clockMs

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
    let ``a default kernel boots at the Unix epoch`` () =
        // The replay contract: change this and every recorded trace's timestamps
        // change with it.
        EmulatedKernel.systemTimeAsTicks EmulatedKernel.initial |> shouldEqual 0L
        guestUtcNow EmulatedKernel.initial |> shouldEqual DateTime.UnixEpoch

    [<Test>]
    let ``every reachable reading names a representable UTC instant`` () =
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockMs = reachable seeds
            let now = guestUtcNow (kernelWith epochMs clockMs)

            now.Kind = DateTimeKind.Utc
            && now >= DateTime.UnixEpoch
            && now <= DateTime.MaxValue

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``the reading is the boot instant plus the elapsed virtual clock`` () =
        // The oracle is the BCL's own date arithmetic rather than a restatement
        // of the implementation's multiply.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockMs = reachable seeds

            guestUtcNow (kernelWith epochMs clockMs) = DateTime.UnixEpoch
                .AddTicks(epochMs * EmulatedKernel.ticksPerMillisecond)
                .AddTicks (clockMs * EmulatedKernel.ticksPerMillisecond)

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

    [<Test>]
    let ``moving elapsed time into the boot instant is unobservable`` () =
        // Affine-ness: the guest cannot tell "booted at E, ran for C" apart from
        // "booted at E+C, ran for nothing". This is what makes the wall clock a
        // pure view of the monotonic one rather than an independent axis, and it
        // is the property that would have to be given up to model NTP steps.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockMs = reachable seeds

            EmulatedKernel.systemTimeAsTicks (kernelWith epochMs clockMs) = EmulatedKernel.systemTimeAsTicks (
                kernelWith (epochMs + clockMs) 0L
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
    let ``the reading has millisecond granularity`` () =
        // Documented consequence of deriving from a millisecond clock: every
        // tick value is a multiple of 10,000, so `DateTime.UtcNow` is not a
        // source of unique values. Real `clock_gettime(CLOCK_REALTIME)` makes no
        // uniqueness guarantee either, so this is a faithful gap rather than one
        // to paper over.
        let property (seeds : int64 * int64) : bool =
            let epochMs, clockMs = reachable seeds

            let ticks = EmulatedKernel.systemTimeAsTicks (kernelWith epochMs clockMs)
            ticks % EmulatedKernel.ticksPerMillisecond = 0L

        Check.One (propertyConfig, Prop.forAll int64Pairs property)

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
            let headroom = maxEpochMs - epochMs
            // Strictly past the representable end of time.
            let clockMs = headroom + 1L + intoRange 1_000_000L overshootSeed

            let kernel =
                { EmulatedKernel.initial with
                    WallClockEpochMs = epochMs
                    VirtualClockMs = clockMs
                }

            not (succeeds (fun () -> EmulatedKernel.systemTimeAsTicks kernel))

        Check.One (propertyConfig, Prop.forAll int64Pairs property)
