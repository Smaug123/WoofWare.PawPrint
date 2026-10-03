namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The read-only queries a client reads the kernel's state through rather than
/// its records' fields: each answers what the setter or syscall that last wrote
/// the state put there.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStateQueries =

    let private context : string = "TestStateQueries"

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private imageOn (platform : SimulatedUnixPlatform) : UnixBootImage<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private initialOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        imageOn platform |> UnixBootImage.boot

    [<Test>]
    let ``platform is the one the machine was started on`` () : unit =
        for platform in platforms do
            UnixMachineState.platform (initialOn platform).Machine |> shouldEqual platform

    [<Test>]
    let ``processorCount is the default until set, and then what was set`` () : unit =
        for platform in platforms do
            UnixMachineState.processorCount (initialOn platform).Machine
            |> shouldEqual UnixSystem.defaultProcessorCount

        let property (platform : SimulatedUnixPlatform, count : int) : unit =
            imageOn platform
            |> UnixBootImage.withProcessorCount count
            |> UnixBootImage.boot
            |> fun system -> UnixMachineState.processorCount system.Machine
            |> shouldEqual count

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) (Gen.choose (1, 1 <<< 16)))) property
        )

    /// The clock each flavour reads to the nanosecond since boot: Linux's
    /// `CLOCK_BOOTTIME` and Darwin's `CLOCK_MONOTONIC_RAW`.
    let private fullPrecisionSinceBoot (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 7
        | SimulatedUnixFlavour.Darwin -> 4

    [<Test>]
    let ``nanosecondsSinceBoot is the sum of the advances, and what a full-precision uptime clock reads`` () : unit =
        let property (platform : SimulatedUnixPlatform, advances : int64 list) : unit =
            let machine =
                ((initialOn platform).Machine, advances)
                ||> List.fold (fun machine advance -> UnixMachineState.advanceClock advance machine)

            let uptime = UnixMachineState.nanosecondsSinceBoot machine
            uptime |> shouldEqual (List.sum advances)

            match UnixClock.clockGettime (fullPrecisionSinceBoot platform) machine with
            | Ok (Ok reading) ->
                UnixTimestamp.seconds reading * 1_000_000_000L
                + int64 (UnixTimestamp.nanoseconds reading)
                |> shouldEqual uptime
            | other -> failwith $"clock_gettime of the uptime clock answered %A{other}"

        // Each advance is bounded so that no sum of a generated list can overflow.
        let advance = Gen.choose64 (0L, 1L <<< 40)

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) (Gen.listOf advance))) property
        )

    [<Test>]
    let ``delivered is every write to a drained stream, oldest first`` () : unit =
        let property (platform : SimulatedUnixPlatform, writes : (int * int) list) : unit =
            let initial = initialOn platform

            DeliveryLog.count (UnixMachineState.delivered initial.Machine) |> shouldEqual 0

            let final, expected =
                ((initial, []), List.indexed writes)
                ||> List.fold (fun (system, expected) (index, (fd, count)) ->
                    let bytes =
                        ImmutableArray.Create<byte> (Array.init count (fun i -> byte (index + i)))

                    match WriteOutcomes.write fd bytes system with
                    | Ok (WriteAnswer.Completed _, after) ->
                        let expected =
                            if count = 0 then
                                expected
                            else
                                {
                                    Endpoint = ExternalEndpoint fd
                                    Bytes = bytes
                                }
                                :: expected

                        after, expected
                    | other -> failwith $"write(%d{fd}, %d{count} bytes) answered %A{Result.map fst other}"
                )

            DeliveryLog.toList (UnixMachineState.delivered final.Machine)
            |> shouldEqual (List.rev expected)

        let write = Gen.zip (Gen.elements [ 1 ; 2 ]) (Gen.choose (0, 4))

        Check.One (
            propertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip (Gen.elements platforms) (Gen.listOf write |> Gen.resize 10))) property
        )

    /// Arbitrary non-NUL bytes, short and drawn from a small alphabet.
    let private entryGen : Gen<UnixByteString> =
        Gen.elements [ 0x3Duy ; 0x41uy ; 0xFFuy ]
        |> Gen.listOf
        |> Gen.resize 4
        |> Gen.map (fun bytes ->
            match UnixByteString.ofBytes (ImmutableArray.CreateRange bytes) with
            | Ok entry -> entry
            | Error defect -> failwith $"generator produced a NUL: %s{UnixByteString.describe defect}"
        )

    [<Test>]
    let ``environment is empty until set, and then exactly what was set`` () : unit =
        for platform in platforms do
            UnixProcessState.environment (initialOn platform).Process |> shouldEqual []

        let property (entries : UnixByteString list) : unit =
            imageOn SimulatedUnixPlatform.linuxX64
            |> UnixBootImage.withEnvironment context entries
            |> UnixBootImage.boot
            |> fun system -> UnixProcessState.environment system.Process
            |> shouldEqual entries

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.listOf entryGen |> Gen.resize 6)) property)

    [<Test>]
    let ``processPath is the default until set, and then what was set`` () : unit =
        UnixProcessState.processPath (initialOn SimulatedUnixPlatform.linuxX64).Process
        |> shouldEqual UnixSystem.defaultProcessPath

        for path in [ None ; Some (AbsoluteUnixPath.parseOrFail context "/bin/guest") ] do
            imageOn SimulatedUnixPlatform.linuxX64
            |> UnixBootImage.withProcessPath context path
            |> UnixBootImage.boot
            |> fun system -> UnixProcessState.processPath system.Process
            |> shouldEqual path

    [<Test>]
    let ``signals is the process's own, read under its platform's numbering`` () : unit =
        for platform in platforms do
            UnixProcessState.signals (initialOn platform).Process
            |> SignalState.numbering
            |> shouldEqual (SimulatedUnixPlatform.signalNumbering platform)

    [<Test>]
    let ``park is absent until a task sleeps, and then names the syscall it sleeps in`` () : unit =
        for platform in platforms do
            let system = initialOn platform

            UnixTaskState.park (UnixTaskTable.get 0 system.Tasks) |> shouldEqual None

            let system =
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (3, 4), system) -> system
                | other -> failwith $"pipe2: %A{other}"

            let system =
                match UnixReadWrite.read 0 3 UserBuffer.Mapped 16UL system with
                | Ok (ReadOutcome.WouldBlock _, system) -> system
                | other -> failwith $"expected a read of the empty pipe to sleep, got %A{other}"

            let park = UnixTaskState.park (UnixTaskTable.get 0 system.Tasks)
            park |> shouldEqual (UnixTaskTable.parkOf 0 system.Tasks)

            match park |> Option.map (fun park -> park.Syscall) with
            | Some (ParkedSyscall.PipeRead _) -> ()
            | other -> failwith $"expected the leader to be parked in its pipe read, got %A{other}"
