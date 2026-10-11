namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `ParkedSyscall.Sleep`, which is reserved: nothing parks a task in it yet.
/// What every reader of a park already answers for it (its wake condition, its
/// deadline, the descriptions it holds, how a signal ends it), and the
/// invariant that reports a task parked in it, which is what makes "nothing
/// parks a task in it" checked wherever a fuzzer checks invariants.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSleepPark =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private int64Gen : Gen<int64> = ArbMap.defaults |> ArbMap.generate<int64>

    let private destinationGen : Gen<UserBuffer> =
        Gen.oneof
            [
                Gen.constant UserBuffer.Mapped
                ArbMap.defaults |> ArbMap.generate<uint64> |> Gen.map UserBuffer.Unmapped
                Gen.constant UserBuffer.Opaque
                Gen.constant UserBuffer.Addressless
            ]

    let private sleepGen : Gen<ParkedSleep> =
        gen {
            let! deadline = int64Gen

            let! remainder =
                Gen.oneof
                    [
                        Gen.constant SleepRemainder.Unreported
                        Gen.map2
                            (fun until destination -> SleepRemainder.Reported (until, destination))
                            int64Gen
                            destinationGen
                    ]

            return
                {
                    Deadline = deadline
                    Remainder = remainder
                }
        }

    [<Test>]
    let ``a sleep waits for its deadline or a signal, holds no description, and fails with EINTR under SA_RESTART``
        ()
        : unit
        =
        let property (sleep : ParkedSleep) : unit =
            let parked = ParkedSyscall.Sleep sleep

            WakeCondition.ofPark parked
            |> shouldEqual (
                WakeCondition.AnyOf (
                    WakeCondition.Primitive (WakePrimitive.DeadlinePassed sleep.Deadline),
                    [ WakeCondition.Primitive WakePrimitive.SignalDeliverable ]
                )
            )

            WakeCondition.ofPark parked
            |> WakeCondition.deadlines
            |> shouldEqual [ sleep.Deadline ]

            ParkedSyscall.descriptions parked |> shouldEqual []

            for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                SyscallInterruption.ruleOf flavour parked
                |> shouldEqual SignalRestartRule.FailsWithEintr

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen sleepGen) property)

    let private boot (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private task : int = 1

    [<Test>]
    let ``a task parked in a sleep is a ReservedPark defect, whose deadline the wait machinery reads`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let deadline = 1_000_000L

            let parked =
                ParkedSyscall.Sleep
                    {
                        Deadline = deadline
                        Remainder = SleepRemainder.Reported (deadline, UserBuffer.Mapped)
                    }

            let awake = boot flavour |> Tasks.ensure task
            awake |> UnixSystem.checkInvariants |> shouldEqual []

            let asleep = UnixWait.park task parked awake

            asleep
            |> UnixSystem.checkInvariants
            |> shouldEqual [ UnixSystemDefect.ReservedPark (task, parked) ]

            UnixWait.deadlines (Set.singleton task) asleep |> shouldEqual [ deadline ]
            UnixWait.satisfied task asleep |> shouldEqual (Some Set.empty)

            let elapsed =
                { asleep with
                    Scheduling =
                        UnixSchedulingState.advanceClock
                            (deadline - asleep.Scheduling.NanosecondsSinceBoot)
                            asleep.Scheduling
                }

            UnixWait.satisfied task elapsed
            |> shouldEqual (Some (Set.singleton (WakePrimitive.DeadlinePassed deadline)))
