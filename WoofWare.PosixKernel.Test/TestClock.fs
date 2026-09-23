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

    let private machineOn (flavour : SimulatedUnixFlavour) : UnixMachineState =
        (UnixSystem.initial<int, string> (HostPlatform.platformOf flavour)).Machine

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

    /// A machine booted at `bootTime` which has been up for `sinceBoot` nanoseconds,
    /// reached the only way a client can reach one.
    let private machineWith
        (flavour : SimulatedUnixFlavour)
        (bootTime : UnixTimestamp)
        (sinceBoot : int64)
        : UnixMachineState
        =
        machineOn flavour
        |> UnixMachineState.withBootTime bootTime
        |> UnixMachineState.advanceClock sinceBoot

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

        succeeds (fun () -> UnixMachineState.advanceClock 10L machine)
        |> shouldEqual false

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
                        ]

                let! micros = Gen.choose64 (0L, 999_999L)
                return seconds, int micros * 1000
            }

        let property (seconds : int64, nanos : int) : bool =
            let timestamp = UnixTimestamp.createOrFail "TestClock" seconds nanos
            let admissible = seconds >= 0L && seconds <= UnixMachineState.maxBootTimeSeconds

            flavours
            |> List.forall (fun flavour ->
                succeeds (fun () -> UnixMachineState.withBootTime timestamp (machineOn flavour)) = admissible
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``Darwin refuses a boot instant finer than a microsecond, and Linux does not`` () : unit =
        let gen =
            gen {
                let! seconds = anywhereIn 0L UnixMachineState.maxBootTimeSeconds
                let! nanos = anywhereIn 0L 999_999_999L
                return seconds, int nanos
            }

        let property (seconds : int64, nanos : int) : bool =
            let timestamp = UnixTimestamp.createOrFail "TestClock" seconds nanos

            let linux =
                succeeds (fun () -> UnixMachineState.withBootTime timestamp (machineOn SimulatedUnixFlavour.Linux))

            let darwin =
                succeeds (fun () -> UnixMachineState.withBootTime timestamp (machineOn SimulatedUnixFlavour.Darwin))

            linux && darwin = (nanos % 1000 = 0)

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
    /// `TestClockAgainstHost` re-measures the split on whichever host runs it.
    let private expected (flavour : SimulatedUnixFlavour) (clockId : int) : Outcome =
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            match clockId with
            | 0 -> Outcome.Realtime
            | 1
            | 4
            | 6
            | 7 -> Outcome.Monotonic
            | 2
            | 3
            | 5
            | 8
            | 9
            | 11 -> Outcome.Refused
            | id when id < 0 -> Outcome.Refused
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
    let private telling (flavour : SimulatedUnixFlavour) : UnixMachineState =
        machineWith flavour (UnixTimestamp.ofSeconds 1_700_000_000L) 123_456_789_987L

    /// Classify an answer by which of the two clocks it read.
    let private classify (machine : UnixMachineState) (clockId : int) : Outcome =
        match UnixClock.clockGettime clockId machine with
        | Error _ -> Outcome.Refused
        | Ok (Error UnixError.EINVAL) -> Outcome.Invalid
        | Ok (Error other) -> failwith $"clock id %d{clockId} failed with %O{other}, which no measured kernel does"
        | Ok (Ok reading) ->
            if UnixTimestamp.seconds reading = machine.NanosecondsSinceBoot / 1_000_000_000L then
                Outcome.Monotonic
            elif UnixTimestamp.seconds reading = UnixTimestamp.seconds (UnixMachineState.realtime machine) then
                Outcome.Realtime
            else
                failwith $"clock id %d{clockId} read %O{reading}, which is neither clock"

    let private sweptIds : int list =
        [ -4096 .. 4096 ]
        @ [ Int32.MinValue ; Int32.MinValue + 1 ; Int32.MaxValue - 1 ; Int32.MaxValue ]

    [<Test>]
    let ``each clock id reads the clock the measured table says, on each flavour`` () : unit =
        for flavour in flavours do
            let machine = telling flavour

            for clockId in sweptIds do
                let actual = classify machine clockId

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
        | SimulatedUnixFlavour.Linux, 0 -> realtime
        | SimulatedUnixFlavour.Linux, _ -> monotonic
        | SimulatedUnixFlavour.Darwin, 0 -> toMicroseconds realtime
        | SimulatedUnixFlavour.Darwin, 6 -> toMicroseconds monotonic
        | SimulatedUnixFlavour.Darwin, _ -> monotonic

    [<Test>]
    let ``every answered clock reads exactly what its flavour reports`` () : unit =
        let property (flavour : SimulatedUnixFlavour, bootTime : UnixTimestamp, up : int64) : bool =
            let machine = machineWith flavour bootTime up

            [ 0..16 ]
            |> List.filter (fun clockId ->
                match expected flavour clockId with
                | Outcome.Realtime
                | Outcome.Monotonic -> true
                | Outcome.Invalid
                | Outcome.Refused -> false
            )
            |> List.forall (fun clockId ->
                match UnixClock.clockGettime clockId machine with
                | Ok (Ok reading) -> exactNanoseconds reading = expectedReading flavour clockId bootTime up
                | other -> failwith $"%O{flavour} clock id %d{clockId} should have answered, got %A{other}"
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen reachableGen) property)

    [<Test>]
    let ``Darwin's microsecond clocks drop exactly the sub-microsecond digits`` () : unit =
        // The property above cannot tell truncation from rounding at a reading whose
        // sub-microsecond part is below 500, so pin one above it.
        let machine =
            machineWith SimulatedUnixFlavour.Darwin (UnixTimestamp.ofSeconds 10L) 1_000_999L

        match UnixClock.clockGettime 0 machine, UnixClock.clockGettime 6 machine, UnixClock.clockGettime 8 machine with
        | Ok (Ok realtime), Ok (Ok monotonic), Ok (Ok uptime) ->
            realtime |> shouldEqual (UnixTimestamp.createOrFail "TestClock" 10L 1_000_000)
            monotonic |> shouldEqual (UnixTimestamp.createOrFail "TestClock" 0L 1_000_000)
            uptime |> shouldEqual (UnixTimestamp.createOrFail "TestClock" 0L 1_000_999)
        | other -> failwith $"expected three readings, got %A{other}"
