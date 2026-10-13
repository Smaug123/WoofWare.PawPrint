namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `ParkedSyscall.SignalWait` and `WakePrimitive.AwaitedSignalPending`, which
/// are reserved: nothing parks a task in a signal wait yet. What every reader
/// of a park already answers for it (its wake condition, its deadline, the
/// descriptions it holds, how a signal ends it), and the invariant that reports
/// a task parked in it, which is what makes "nothing parks a task in it"
/// checked wherever a fuzzer checks invariants.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalWaitPark =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private maskGen : Gen<SignalMask> =
        gen {
            let! numbering = Gen.elements [ SignalNumbering.Linux ; SignalNumbering.Darwin ]
            let! word = ArbMap.defaults |> ArbMap.generate<uint64>

            let word =
                match numbering with
                | SignalNumbering.Linux -> word
                | SignalNumbering.Darwin -> word &&& 0xFFFF_FFFFUL

            return
                match SignalMask.ofWord numbering word with
                | Ok mask -> mask
                | Error refusal -> failwith $"%s{SignalMaskRefusal.describe refusal}"
        }

    let private destinationGen : Gen<UserBuffer> =
        Gen.oneof
            [
                Gen.constant UserBuffer.Mapped
                ArbMap.defaults |> ArbMap.generate<uint64> |> Gen.map UserBuffer.Unmapped
                Gen.constant UserBuffer.Opaque
                Gen.constant UserBuffer.Addressless
            ]

    let private waitGen : Gen<ParkedSignalWait> =
        gen {
            let! awaited = maskGen
            let! deadline = Gen.optionOf (ArbMap.defaults |> ArbMap.generate<int64>)
            let! destination = Gen.optionOf destinationGen

            return
                {
                    Awaited = awaited
                    Deadline = deadline
                    Destination = destination
                }
        }

    [<Test>]
    let ``a signal wait waits for an awaited signal, its deadline or a signal, holds no description, and fails with EINTR under SA_RESTART``
        ()
        : unit
        =
        let property (wait : ParkedSignalWait) : unit =
            let parked = ParkedSyscall.SignalWait wait

            let deadline =
                wait.Deadline
                |> Option.map (WakePrimitive.DeadlinePassed >> WakeCondition.Primitive)
                |> Option.toList

            WakeCondition.ofPark parked
            |> shouldEqual (
                WakeCondition.AnyOf (
                    WakeCondition.Primitive (WakePrimitive.AwaitedSignalPending wait.Awaited),
                    deadline @ [ WakeCondition.Primitive WakePrimitive.SignalDeliverable ]
                )
            )

            WakeCondition.ofPark parked
            |> WakeCondition.deadlines
            |> shouldEqual (Option.toList wait.Deadline)

            ParkedSyscall.descriptions parked |> shouldEqual []

            for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                SyscallInterruption.ruleOf flavour parked
                |> shouldEqual SignalRestartRule.FailsWithEintr

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen waitGen) property)

    let private boot (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private task : int = 1

    [<Test>]
    let ``a task parked in a signal wait is a ReservedPark defect, whose deadline the wait machinery reads`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            let deadline = 1_000_000L

            let parked =
                ParkedSyscall.SignalWait
                    {
                        Awaited = SignalMask.ofSignals numbering (Set.ofList [ Signal.SIGUSR1 ])
                        Deadline = Some deadline
                        Destination = Some UserBuffer.Mapped
                    }

            let awake = boot flavour |> Tasks.ensure task
            awake |> UnixSystem.checkInvariants |> shouldEqual []

            let asleep = UnixWait.park task parked awake

            asleep
            |> UnixSystem.checkInvariants
            |> shouldEqual [ UnixSystemDefect.ReservedPark (task, parked) ]

            UnixWait.deadlines (Set.singleton task) asleep |> shouldEqual [ deadline ]

            // Which pending signals a wait may take is not modelled, so asking
            // fails loudly rather than guessing.
            let error =
                Assert.Throws<Exception> (fun () -> UnixWait.satisfied task asleep |> ignore<Set<WakePrimitive> option>)

            error.Message |> shouldContainText "SignalWait"
