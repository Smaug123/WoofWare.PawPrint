namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Every task is on a processor the machine has, in `[0, ProcessorCount)`:
/// `UnixTaskLifecycle.spawn` throws for any other, a launch is refused for a
/// leader on any other, and `checkInvariants` reports a task on any other.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCpuRange =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// A processor count, and a processor index weighted towards both ends of
    /// the machine's range, where a bound goes wrong, as well as spread across
    /// the whole of `int`.
    let private countAndCpu : Gen<SimulatedUnixPlatform * int * int> =
        gen {
            let! platform = Gen.elements platforms
            let! count = Gen.oneof [ Gen.choose (1, 8) ; Gen.choose (1, 1024) ]

            let! cpu =
                Gen.oneof
                    [
                        Gen.choose (-2, count + 2)
                        Gen.elements [ -1 ; 0 ; count - 1 ; count ]
                        Gen.elements [ Int32.MinValue ; Int32.MaxValue ]
                        ArbMap.defaults |> ArbMap.generate<int>
                    ]

            return platform, count, cpu
        }

    let private inRange (count : int) (cpu : int) : bool = cpu >= 0 && cpu < count

    let private imageWith (platform : SimulatedUnixPlatform) (count : int) : UnixBootImage<int, string> =
        UnixSystem.initial platform
        |> UnixBootImage.withProcessorCount count
        |> Configured.expectOk ProcessorCountRefusal.describe

    let private systemWith (platform : SimulatedUnixPlatform) (count : int) : UnixSystem<int, string> =
        imageWith platform count
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// Which side of the machine's range a drawn processor is on.
    [<RequireQualifiedAccess>]
    type private Side =
        | Inside
        | Outside

    /// The sides a property's fixed sample drew, so that a property whose
    /// generator stopped reaching one side of the bound fails rather than
    /// passing on the other side alone.
    let private assertBothSides (coverage : Coverage<Side>) : unit =
        let inside = coverage.Count Side.Inside
        let outside = coverage.Count Side.Outside

        // The fixed sample draws 196 processors in range and 304 out of it, so
        // 100 of 500 on either side is far below what a working generator gives.
        if inside < 100 || outside < 100 then
            failwith $"the generator drew %d{inside} processors in range and %d{outside} out of it, of 500"

    [<Test>]
    let ``spawn throws exactly when the processor is not the machine's`` () : unit =
        let property (cover : Side -> unit) (platform : SimulatedUnixPlatform, count : int, cpu : int) : unit =
            let system = systemWith platform count

            let spawned =
                try
                    Ok (UnixTaskLifecycle.spawn 0 1 (CpuId cpu) system)
                with e ->
                    Error e.Message

            match spawned, inRange count cpu with
            | Ok (Ok (SpawnAnswer.Spawned _, after)), true ->
                cover Side.Inside
                (UnixTaskTable.get 1 after.Tasks).Cpu |> shouldEqual (CpuId cpu)
                UnixSystem.checkInvariants after |> shouldEqual []
            | Error message, false ->
                cover Side.Outside
                message |> shouldContainText $"%O{CpuId cpu}"
                message |> shouldContainText $"has %d{count} logical processors"
            | other, expected -> failwith $"%O{CpuId cpu} on %d{count} processors: got %A{other}, in range %b{expected}"

        CoverageSample.check propertyConfig (Arb.fromGen countAndCpu) property
        |> assertBothSides

    [<Test>]
    let ``boot refuses exactly a leader on a processor the machine does not have`` () : unit =
        let property (cover : Side -> unit) (platform : SimulatedUnixPlatform, count : int, cpu : int) : unit =
            let launch = Launched.launch platform UnixSystem.pipedStandardStreams 0 (CpuId cpu)

            match UnixBootImage.boot launch (imageWith platform count), inRange count cpu with
            | Ok system, true ->
                cover Side.Inside
                (UnixTaskTable.get 0 system.Tasks).Cpu |> shouldEqual (CpuId cpu)
                UnixSystem.checkInvariants system |> shouldEqual []
            | Error refusal, false ->
                cover Side.Outside
                refusal |> shouldEqual (LaunchRefusal.LeaderCpuBeyondMachine (CpuId cpu, count))
            | other, expected -> failwith $"%O{CpuId cpu} on %d{count} processors: got %A{other}, in range %b{expected}"

        CoverageSample.check propertyConfig (Arb.fromGen countAndCpu) property
        |> assertBothSides

    [<Test>]
    let ``launch refuses exactly a leader on a processor the machine does not have`` () : unit =
        let property (cover : Side -> unit) (platform : SimulatedUnixPlatform, count : int, cpu : int) : unit =
            let machine = SimulatedMachine.ofSystem (systemWith platform count)

            let launch = Launched.launch platform UnixSystem.pipedStandardStreams 0 (CpuId cpu)

            match SimulatedMachine.launch launch machine, inRange count cpu with
            | Ok (pid, after), true ->
                cover Side.Inside

                (UnixTaskTable.get 0 (Machines.viewOf pid after).Tasks).Cpu
                |> shouldEqual (CpuId cpu)

                SimulatedMachine.checkInvariants after |> shouldEqual []
            | Error refusal, false ->
                cover Side.Outside

                refusal
                |> shouldEqual (ProcessCreationRefusal.Launch (LaunchRefusal.LeaderCpuBeyondMachine (CpuId cpu, count)))
            | other, expected -> failwith $"%O{CpuId cpu} on %d{count} processors: got %A{other}, in range %b{expected}"

        CoverageSample.check propertyConfig (Arb.fromGen countAndCpu) property
        |> assertBothSides

    /// `system` with task `task`'s processor overwritten with `cpu`, as no
    /// public route can do.
    let private forgeCpu (task : int) (cpu : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Tasks =
                system.Tasks
                |> Map.add
                    task
                    { UnixTaskTable.get task system.Tasks with
                        Cpu = CpuId cpu
                    }
        }

    [<Test>]
    let ``checkInvariants reports exactly a task on a processor the machine does not have`` () : unit =
        let property (cover : Side -> unit) (platform : SimulatedUnixPlatform, count : int, cpu : int) : unit =
            let system = systemWith platform count |> Tasks.spawn 1 |> forgeCpu 1 cpu

            let machine =
                SimulatedMachine.ofSystem (systemWith platform count)
                |> Machines.inProcess UnixSystem.defaultProcessId (fun view -> (), Tasks.spawn 1 view |> forgeCpu 1 cpu)
                |> snd

            if inRange count cpu then
                cover Side.Inside
                UnixSystem.checkInvariants system |> shouldEqual []
                SimulatedMachine.checkInvariants machine |> shouldEqual []
            else
                cover Side.Outside
                let defect = UnixSystemDefect.CpuBeyondMachine (1, CpuId cpu, count)
                UnixSystem.checkInvariants system |> shouldEqual [ defect ]

                SimulatedMachine.checkInvariants machine
                |> shouldEqual [ SimulatedMachineDefect.View (UnixSystem.defaultProcessId, defect) ]

        CoverageSample.check propertyConfig (Arb.fromGen countAndCpu) property
        |> assertBothSides

    [<Test>]
    let ``the refusal names the processor and the machine's count`` () : unit =
        LaunchRefusal.describe (LaunchRefusal.LeaderCpuBeyondMachine (CpuId 4, 4))
        |> shouldEqual
            "the launch puts its leader on <cpu #4>, but the machine has 4 logical processors, numbered from 0."
