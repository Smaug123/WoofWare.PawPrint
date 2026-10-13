namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `ParkedSyscall.DatagramReceive` and `ParkedSyscall.DatagramSend`, which are
/// reserved: nothing parks a task in either yet. What every reader of a park
/// already answers for them (the wake condition, the description each holds,
/// how a signal ends each), and the invariant that reports a task parked in
/// one, which is what makes "nothing parks a task in it" checked wherever a
/// fuzzer checks invariants.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDatagramPark =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private parkGen : Gen<ParkedSyscall> =
        gen {
            let! socket = ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map OpenFileDescriptionId

            return!
                Gen.elements
                    [
                        ParkedSyscall.DatagramReceive
                            {
                                ParkedDatagramReceive.Socket = socket
                            }
                        ParkedSyscall.DatagramSend
                            {
                                ParkedDatagramSend.Socket = socket
                            }
                    ]
        }

    let private socketOf (parked : ParkedSyscall) : OpenFileDescriptionId =
        match parked with
        | ParkedSyscall.DatagramReceive receive -> receive.Socket
        | ParkedSyscall.DatagramSend send -> send.Socket
        | other -> failwith $"not a datagram park: %A{other}"

    [<Test>]
    let ``a datagram transfer waits for a signal alone while reserved, holds its socket, and restarts under SA_RESTART``
        ()
        : unit
        =
        let property (parked : ParkedSyscall) : unit =
            WakeCondition.ofPark parked
            |> shouldEqual (WakeCondition.Primitive WakePrimitive.SignalDeliverable)

            WakeCondition.ofPark parked |> WakeCondition.deadlines |> shouldEqual []

            // Measured (`docs/probes/datagram-park`, row R3): Linux's call keeps
            // the socket across a close of its descriptor.
            ParkedSyscall.descriptions parked |> shouldEqual [ socketOf parked ]

            // Measured (rows R2 and S4): a receive restarts under SA_RESTART on
            // both flavours, and a Linux Unix-domain send does too. A Darwin
            // send never sleeps.
            for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                SyscallInterruption.ruleOf flavour parked
                |> shouldEqual SignalRestartRule.RestartsUnderSaRestart

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen parkGen) property)

    let private boot (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private task : int = 1

    [<Test>]
    let ``a task parked in a datagram transfer is a ReservedPark defect`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            let awake = boot flavour |> Tasks.ensure task
            awake |> UnixSystem.checkInvariants |> shouldEqual []

            let socket =
                match FileDescriptorRegistry.tryFindId 0 (UnixSystemState.fileDescriptors awake) with
                | Some id -> id
                | None -> failwith "fd 0 names no description"

            for parked in
                [
                    ParkedSyscall.DatagramReceive
                        {
                            ParkedDatagramReceive.Socket = socket
                        }
                    ParkedSyscall.DatagramSend
                        {
                            ParkedDatagramSend.Socket = socket
                        }
                ] do
                let asleep = UnixWait.park task parked awake

                asleep
                |> UnixSystem.checkInvariants
                |> shouldEqual [ UnixSystemDefect.ReservedPark (task, parked) ]

                // The park holds the description it names, once.
                OpenFileTable.holdCount socket awake.Machine.OpenFiles |> shouldEqual (Some 0)
                OpenFileTable.holdCount socket asleep.Machine.OpenFiles |> shouldEqual (Some 1)
                UnixWait.deadlines (Set.singleton task) asleep |> shouldEqual []
                UnixWait.satisfied task asleep |> shouldEqual (Some Set.empty)
