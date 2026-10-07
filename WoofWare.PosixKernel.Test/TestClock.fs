namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The machine's clock: `advanceClock` and `withBootTime`, which are the only ways
/// it changes, and `realtime` and `UnixClock.clockGettime`, which are how it is read.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestClock =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private imageOn (flavour : SimulatedUnixFlavour) : UnixBootImage<int, string> =
        UnixSystem.initial<int, string> (HostPlatform.platformOf flavour) UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private machineOn (flavour : SimulatedUnixFlavour) : UnixMachineState =
        (UnixBootImage.boot (imageOn flavour)).Machine

    /// A value in `[low, high]`, weighted towards both ends as well as spread across
    /// the whole range: the ends are where the arithmetic can overflow, and a uniform
    /// draw over a range of 2^63 essentially never lands near them.
    let private anywhereIn (low : int64) (high : int64) : Gen<int64> =
        Gen.oneof
            [
                Gen.choose64 (low, high)
                Gen.choose64 (low, (if high - low > 1000L then low + 1000L else high))
                Gen.choose64 ((if high - low > 1000L then high - 1000L else low), high)
            ]

    let private flavourGen : Gen<SimulatedUnixFlavour> = Gen.elements flavours

    let private succeeds (f : unit -> 'a) : bool =
        try
            f () |> ignore<'a>
            true
        with _ ->
            false

    /// Exact nanoseconds a timestamp names, in a type that cannot overflow for any
    /// `time_t`, so an oracle built on it shares none of the implementation's carry
    /// arithmetic.
    let private exactNanoseconds (timestamp : UnixTimestamp) : bigint =
        bigint (UnixTimestamp.seconds timestamp) * 1_000_000_000I
        + bigint (UnixTimestamp.nanoseconds timestamp)

    /// A boot instant this flavour admits, drawn from the whole of `withBootTime`'s range.
    let private bootTimeGen (flavour : SimulatedUnixFlavour) : Gen<UnixTimestamp> =
        gen {
            let! seconds = anywhereIn 0L UnixMachineState.maxBootTimeSeconds
            let! nanos = anywhereIn 0L 999_999_999L
            let nanos = int nanos

            let nanos =
                match flavour with
                | SimulatedUnixFlavour.Linux -> nanos
                | SimulatedUnixFlavour.Darwin -> nanos - nanos % 1000

            return UnixTimestamp.createOrFail "TestClock" seconds nanos
        }

    /// A process on a machine booted at `bootTime` which has been up for
    /// `sinceBoot` nanoseconds, reached the only way a client can reach one.
    let private systemWith
        (flavour : SimulatedUnixFlavour)
        (bootTime : UnixTimestamp)
        (sinceBoot : int64)
        : UnixSystem<int, string>
        =
        imageOn flavour
        |> UnixBootImage.withBootTime bootTime
        |> Configured.expectOk BootTimeRefusal.describe
        |> UnixBootImage.boot
        |> UnixSystem.advanceClock sinceBoot

    /// The machine `systemWith` runs on.
    let private machineWith
        (flavour : SimulatedUnixFlavour)
        (bootTime : UnixTimestamp)
        (sinceBoot : int64)
        : UnixMachineState
        =
        (systemWith flavour bootTime sinceBoot).Machine

    /// Any machine a client can reach: any flavour, any admissible boot instant, any uptime.
    let private reachableGen : Gen<SimulatedUnixFlavour * UnixTimestamp * int64> =
        gen {
            let! flavour = flavourGen
            let! bootTime = bootTimeGen flavour
            let! up = anywhereIn 0L Int64.MaxValue
            return flavour, bootTime, up
        }

    // ------------------------------------------------------------ advancing

    [<Test>]
    let ``a fresh machine has been up for no time and booted at the epoch`` () : unit =
        for flavour in flavours do
            let machine = machineOn flavour
            machine.NanosecondsSinceBoot |> shouldEqual 0L
            machine.BootTime |> shouldEqual UnixTimestamp.epoch
            UnixMachineState.realtime machine |> shouldEqual UnixTimestamp.epoch

    [<Test>]
    let ``advancing twice adds both amounts`` () : unit =
        let gen =
            gen {
                let! first = anywhereIn 0L (Int64.MaxValue / 2L)
                let! second = anywhereIn 0L (Int64.MaxValue / 2L)
                return first, second
            }

        let property (first : int64, second : int64) : bool =
            let machine =
                machineOn SimulatedUnixFlavour.Linux
                |> UnixMachineState.advanceClock first
                |> UnixMachineState.advanceClock second

            machine.NanosecondsSinceBoot = first + second

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``advancing changes nothing but the uptime`` () : unit =
        let machine = machineOn SimulatedUnixFlavour.Darwin
        let advanced = UnixMachineState.advanceClock 12_345L machine

        advanced
        |> shouldEqual
            { machine with
                NanosecondsSinceBoot = 12_345L
            }

        UnixMachineState.advanceClock 0L advanced |> shouldEqual advanced

    [<Test>]
    let ``advancing refuses a negative amount`` () : unit =
        let property (amount : int64) : bool =
            let machine =
                machineOn SimulatedUnixFlavour.Linux |> UnixMachineState.advanceClock 1_000L

            not (succeeds (fun () -> UnixMachineState.advanceClock amount machine))

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (anywhereIn Int64.MinValue -1L)) property)

    [<Test>]
    let ``advancing accepts exactly the amounts that keep the uptime inside int64`` () : unit =
        let gen =
            gen {
                let! up = anywhereIn 0L Int64.MaxValue
                let headroom = Int64.MaxValue - up
                // Straddle the headroom, clipped to the non-negative int64s.
                let! offset = Gen.choose64 (-1_000L, 1_000L)

                let amount =
                    if offset > 0L && headroom > Int64.MaxValue - offset then
                        Int64.MaxValue
                    else
                        max 0L (headroom + offset)

                return up, amount
            }

        let property (up : int64, amount : int64) : bool =
            let machine =
                machineOn SimulatedUnixFlavour.Linux |> UnixMachineState.advanceClock up

            let fits = amount <= Int64.MaxValue - up
            succeeds (fun () -> UnixMachineState.advanceClock amount machine) = fits

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

        let atHorizon =
            machineOn SimulatedUnixFlavour.Linux
            |> UnixMachineState.advanceClock Int64.MaxValue

        atHorizon.NanosecondsSinceBoot |> shouldEqual Int64.MaxValue

        succeeds (fun () -> UnixMachineState.advanceClock 1L atHorizon)
        |> shouldEqual false

    [<Test>]
    let ``advancing refuses a machine whose uptime was record-copied negative`` () : unit =
        let machine =
            { machineOn SimulatedUnixFlavour.Linux with
                NanosecondsSinceBoot = -5L
            }

        // Asserted on the message, not merely on a throw: for a negative uptime the
        // overflow check's `Int64.MaxValue - uptime` wraps negative and throws too, so a
        // bare "it threw" would pass without the guard that makes the arithmetic sound.
        let thrown =
            Assert.Throws<Exception> (fun () -> UnixMachineState.advanceClock 10L machine |> ignore<UnixMachineState>)

        thrown.Message |> shouldContainText "which is negative"

    // ------------------------------------------------------------ booting

    [<Test>]
    let ``maxBootTimeSeconds is the last boot second from which realtime cannot leave time_t`` () : unit =
        // Pinned against bigint arithmetic rather than against the literal: the most a
        // realtime reading's seconds can exceed the boot instant's by is the whole
        // seconds in the longest uptime, plus one carried from the two nanosecond parts.
        let maxUptimeSeconds = bigint Int64.MaxValue / 1_000_000_000I

        bigint UnixMachineState.maxBootTimeSeconds + maxUptimeSeconds + 1I
        |> shouldEqual (bigint Int64.MaxValue)

    [<Test>]
    let ``withBootTime accepts exactly the instants from the epoch to maxBootTimeSeconds`` () : unit =
        let gen =
            gen {
                let! seconds =
                    Gen.oneof
                        [
                            anywhereIn Int64.MinValue Int64.MaxValue
                            anywhereIn -1_000L 1_000L
                            anywhereIn (UnixMachineState.maxBootTimeSeconds - 1_000L) Int64.MaxValue
                            // Each side of both boundaries, which a draw from even
                            // a thousand seconds lands on rarely.
                            Gen.elements
                                [
                                    -1L
                                    0L
                                    UnixMachineState.maxBootTimeSeconds
                                    UnixMachineState.maxBootTimeSeconds + 1L
                                ]
                        ]

                let! micros = Gen.choose64 (0L, 999_999L)
                return seconds, int micros * 1000
            }

        let property (seconds : int64, nanos : int) : unit =
            let timestamp = UnixTimestamp.createOrFail "TestClock" seconds nanos

            let expected : Result<unit, BootTimeRefusal> =
                if seconds < 0L then
                    Error (BootTimeRefusal.BeforeEpoch timestamp)
                elif seconds > UnixMachineState.maxBootTimeSeconds then
                    Error (BootTimeRefusal.PastMaxBootTime (timestamp, UnixMachineState.maxBootTimeSeconds))
                else
                    Ok ()

            for flavour in flavours do
                match UnixBootImage.withBootTime timestamp (imageOn flavour) with
                | Ok image ->
                    expected |> shouldEqual (Ok ())
                    (UnixBootImage.boot image).Machine.BootTime |> shouldEqual timestamp
                | Error refusal -> Error refusal |> shouldEqual expected

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``withBootTime's boundaries`` () : unit =
        let outcome (flavour : SimulatedUnixFlavour) (seconds : int64) (nanos : int) : Result<unit, BootTimeRefusal> =
            UnixBootImage.withBootTime (UnixTimestamp.createOrFail "TestClock" seconds nanos) (imageOn flavour)
            |> Result.map ignore<UnixBootImage<int, string>>

        let latest = UnixMachineState.maxBootTimeSeconds

        for flavour in flavours do
            outcome flavour -1L 999_999_000
            |> shouldEqual (
                Error (BootTimeRefusal.BeforeEpoch (UnixTimestamp.createOrFail "TestClock" -1L 999_999_000))
            )

            outcome flavour 0L 0 |> shouldEqual (Ok ())
            outcome flavour latest 999_999_000 |> shouldEqual (Ok ())

            outcome flavour (latest + 1L) 0
            |> shouldEqual (
                Error (BootTimeRefusal.PastMaxBootTime (UnixTimestamp.createOrFail "TestClock" (latest + 1L) 0, latest))
            )

        // Before the epoch is refused as that first, even where Darwin would
        // also refuse the sub-microsecond part.
        outcome SimulatedUnixFlavour.Darwin -1L 1
        |> shouldEqual (Error (BootTimeRefusal.BeforeEpoch (UnixTimestamp.createOrFail "TestClock" -1L 1)))

        outcome SimulatedUnixFlavour.Darwin 0L 1
        |> shouldEqual (Error (BootTimeRefusal.FinerThanMicrosecond (UnixTimestamp.createOrFail "TestClock" 0L 1)))

    [<Test>]
    let ``Darwin refuses a boot instant finer than a microsecond, and Linux does not`` () : unit =
        let gen =
            gen {
                let! seconds = anywhereIn 0L UnixMachineState.maxBootTimeSeconds
                let! nanos = anywhereIn 0L 999_999_999L
                return seconds, int nanos
            }

        let property (seconds : int64, nanos : int) : unit =
            let timestamp = UnixTimestamp.createOrFail "TestClock" seconds nanos

            let outcome (flavour : SimulatedUnixFlavour) : Result<UnixTimestamp, BootTimeRefusal> =
                UnixBootImage.withBootTime timestamp (imageOn flavour)
                |> Result.map (fun image -> (UnixBootImage.boot image).Machine.BootTime)

            outcome SimulatedUnixFlavour.Linux |> shouldEqual (Ok timestamp)

            outcome SimulatedUnixFlavour.Darwin
            |> shouldEqual (
                if nanos % 1000 = 0 then
                    Ok timestamp
                else
                    Error (BootTimeRefusal.FinerThanMicrosecond timestamp)
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``realtime is the boot instant plus the uptime, on every reachable machine`` () : unit =
        let property (flavour : SimulatedUnixFlavour, bootTime : UnixTimestamp, up : int64) : bool =
            let reading = UnixMachineState.realtime (machineWith flavour bootTime up)

            exactNanoseconds reading = exactNanoseconds bootTime + bigint up

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen reachableGen) property)

        // The one corner the carry is most likely to get wrong.
        for flavour in flavours do
            let latest =
                UnixTimestamp.createOrFail "TestClock" UnixMachineState.maxBootTimeSeconds 999_999_000

            property (flavour, latest, Int64.MaxValue) |> shouldEqual true

    // ------------------------------------------------------------ clock_gettime

    /// What `clock_gettime` does with an id, as a class, ignoring the reading itself.
    [<RequireQualifiedAccess>]
    type private Outcome =
        | Realtime
        | Monotonic
        | Invalid
        | Refused

    /// The measured table, restated independently of the implementation.
    ///
    /// Measured 2026-09-23: Linux 6.18.5 (aarch64, glibc 2.41, Debian trixie
    /// container) answers every id in {0..9, 11}, and nine negative ids in
    /// [-4096, -1], the CPU-time clocks those encode; EINVAL for every other id
    /// swept (-4096..4096, then a stride of 65521 across the whole int range, and
    /// the four extremes). macOS 26 (arm64) answers {0, 4, 5, 6, 8, 9, 12, 16}
    /// and is EINVAL for every other id in the same sweep.
    ///
    /// Linux's auxiliary clocks, 16..23, are refused: they answer EINVAL on a kernel
    /// built without them (the container above) and ENODEV on one built with them
    /// but not enabled (GitHub's ubuntu-24.04 runner, 2026-09-23).
    /// `TestClockAgainstHost` re-measures the split on whichever host runs it.
    let private expected (flavour : SimulatedUnixFlavour) (clockId : int) : Outcome =
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            match clockId with
            // CLOCK_REALTIME_COARSE reads the realtime clock as of the last
            // tick, and this machine's last update is its current reading.
            | 0
            | 5 -> Outcome.Realtime
            | 1
            | 4
            | 6
            | 7 -> Outcome.Monotonic
            | 2
            | 3
            | 8
            | 9
            | 11 -> Outcome.Refused
            | id when id < 0 -> Outcome.Refused
            | id when id >= 16 && id <= 23 -> Outcome.Refused
            | _ -> Outcome.Invalid
        | SimulatedUnixFlavour.Darwin ->
            match clockId with
            | 0 -> Outcome.Realtime
            | 4
            | 6
            | 8 -> Outcome.Monotonic
            | 5
            | 9
            | 12
            | 16 -> Outcome.Refused
            | _ -> Outcome.Invalid

    /// A machine whose two clocks differ in their seconds, so a reading of one cannot
    /// be mistaken for the other at any granularity.
    let private telling (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        systemWith flavour (UnixTimestamp.ofSeconds 1_700_000_000L) 123_456_789_987L

    /// Classify an answer by which of the two clocks it read.
    let private classify (system : UnixSystem<int, string>) (clockId : int) : Outcome =
        match UnixClock.clockGettime clockId system with
        | Error _ -> Outcome.Refused
        | Ok (Error UnixError.EINVAL) -> Outcome.Invalid
        | Ok (Error other) -> failwith $"clock id %d{clockId} failed with %O{other}, which no measured kernel does"
        | Ok (Ok reading) ->
            if UnixTimestamp.seconds reading = UnixSystem.nanosecondsSinceBoot system / 1_000_000_000L then
                Outcome.Monotonic
            elif UnixTimestamp.seconds reading = UnixTimestamp.seconds (UnixMachineState.realtime system.Machine) then
                Outcome.Realtime
            else
                failwith $"clock id %d{clockId} read %O{reading}, which is neither clock"

    let private sweptIds : int list =
        [ -4096 .. 4096 ]
        @ [ Int32.MinValue ; Int32.MinValue + 1 ; Int32.MaxValue - 1 ; Int32.MaxValue ]

    [<Test>]
    let ``each clock id reads the clock the measured table says, on each flavour`` () : unit =
        for flavour in flavours do
            let system = telling flavour

            for clockId in sweptIds do
                let actual = classify system clockId

                if actual <> expected flavour clockId then
                    failwith $"%O{flavour} clock id %d{clockId}: expected %A{expected flavour clockId}, got %A{actual}"

        let property (flavour : SimulatedUnixFlavour, clockId : int) : bool =
            classify (telling flavour) clockId = expected flavour clockId

        let gen =
            gen {
                let! flavour = flavourGen
                let! clockId = Gen.choose (Int32.MinValue, Int32.MaxValue)
                return flavour, clockId
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``each refusal names the id it was asked about`` () : unit =
        for flavour in flavours do
            for clockId in sweptIds do
                match UnixClock.clockGettime clockId (telling flavour) with
                | Error refusal ->
                    let named =
                        match refusal with
                        | ClockGettimeRefusal.CpuTime id
                        | ClockGettimeRefusal.EncodedClock id
                        | ClockGettimeRefusal.Coarse id
                        | ClockGettimeRefusal.MachineDependent id -> id

                    named |> shouldEqual clockId
                | Ok _ -> ()

    /// The reading an answered id gives, from the machine's two fields in `bigint`
    /// arithmetic, so the oracle shares neither the implementation's carry nor its
    /// truncation.
    let private expectedReading
        (flavour : SimulatedUnixFlavour)
        (clockId : int)
        (bootTime : UnixTimestamp)
        (up : int64)
        : bigint
        =
        let realtime = exactNanoseconds bootTime + bigint up
        let monotonic = bigint up
        let toMicroseconds (n : bigint) = n - n % 1000I

        match flavour, clockId with
        | SimulatedUnixFlavour.Linux, 0
        | SimulatedUnixFlavour.Linux, 5 -> realtime
        | SimulatedUnixFlavour.Linux, _ -> monotonic
        | SimulatedUnixFlavour.Darwin, 0 -> toMicroseconds realtime
        | SimulatedUnixFlavour.Darwin, 6 -> toMicroseconds monotonic
        | SimulatedUnixFlavour.Darwin, _ -> monotonic

    [<Test>]
    let ``every answered clock reads exactly what its flavour reports`` () : unit =
        let property (flavour : SimulatedUnixFlavour, bootTime : UnixTimestamp, up : int64) : bool =
            let system = systemWith flavour bootTime up

            [ 0..16 ]
            |> List.filter (fun clockId ->
                match expected flavour clockId with
                | Outcome.Realtime
                | Outcome.Monotonic -> true
                | Outcome.Invalid
                | Outcome.Refused -> false
            )
            |> List.forall (fun clockId ->
                match UnixClock.clockGettime clockId system with
                | Ok (Ok reading) -> exactNanoseconds reading = expectedReading flavour clockId bootTime up
                | other -> failwith $"%O{flavour} clock id %d{clockId} should have answered, got %A{other}"
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen reachableGen) property)

    [<Test>]
    let ``Darwin's microsecond clocks drop exactly the sub-microsecond digits`` () : unit =
        // The property above cannot tell truncation from rounding at a reading whose
        // sub-microsecond part is below 500, so pin one above it.
        let system =
            systemWith SimulatedUnixFlavour.Darwin (UnixTimestamp.ofSeconds 10L) 1_000_999L

        match UnixClock.clockGettime 0 system, UnixClock.clockGettime 6 system, UnixClock.clockGettime 8 system with
        | Ok (Ok realtime), Ok (Ok monotonic), Ok (Ok uptime) ->
            realtime |> shouldEqual (UnixTimestamp.createOrFail "TestClock" 10L 1_000_000)
            monotonic |> shouldEqual (UnixTimestamp.createOrFail "TestClock" 0L 1_000_000)
            uptime |> shouldEqual (UnixTimestamp.createOrFail "TestClock" 0L 1_000_999)
        | other -> failwith $"expected three readings, got %A{other}"
