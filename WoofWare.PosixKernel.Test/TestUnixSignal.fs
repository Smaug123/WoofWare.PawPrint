namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSignal =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private systemOn (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour)

    let private linux : UnixSystem<int, string> = systemOn SimulatedUnixFlavour.Linux

    let private withPid (pid : int32) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process = UnixProcessState.withProcessId "test" (ProcessId.parseOrFail "test" pid) system.Process
        }

    let private live : ImmutableArray<int> = ImmutableArray.Create 0

    let private self : int32 = ProcessId.toInt32 (UnixSystem.processId linux)

    /// The highest signal number `kill(2)` accepts, written out rather than
    /// taken from `Signal.highestSignoUnder`, so that this oracle and the
    /// implementation share nothing but the measurement.
    let private highestSigno (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 64
        | SimulatedUnixFlavour.Darwin -> 31

    /// A signal number from anywhere in `int`, weighted towards the edges of the
    /// valid range, where an off-by-one would show.
    let private signoGen : Gen<int> =
        Gen.oneof
            [
                Gen.choose (-3, 70)
                Gen.elements [ Int32.MinValue ; Int32.MinValue + 1 ; Int32.MaxValue ; 255 ; 256 ; 1000 ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    [<Test>]
    let ``kill of the calling process with SIGKILL terminates it`` () : unit =
        match UnixSignal.kill live self 9 linux with
        | Ok (Ok (generation, _)) -> generation |> shouldEqual (SignalGeneration.ProcessTerminated (Signal.Other 9))
        | other -> failwith $"unexpected answer: %O{other}"

    [<Test>]
    let ``the null signal to the calling process changes nothing`` () : unit =
        for flavour in flavours do
            let system = systemOn flavour

            UnixSignal.kill live self 0 system
            |> shouldEqual (Ok (Ok (SignalGeneration.ProcessContinues, system)))

    [<Test>]
    let ``a signal the calling process cannot yet receive is left pending`` () : unit =
        // No live thread, so SIGTERM waits in the process-wide pending set.
        match UnixSignal.kill ImmutableArray.Empty self 15 linux with
        | Ok (Ok (generation, after)) ->
            generation |> shouldEqual SignalGeneration.ProcessContinues

            SignalState.pending after.Process.Signals
            |> shouldEqual
                [
                    {
                        Signal = Signal.SIGTERM
                        Target = ValueNone
                    }
                ]
        | other -> failwith $"unexpected answer: %O{other}"

    [<Test>]
    let ``kill is answered only for the calling process`` () : unit =
        // A pid that differs from the caller's by one, so a comparison against
        // the wrong field (or an off-by-one) is caught.
        UnixSignal.kill live (self + 1) 0 linux
        |> shouldEqual (Error (KillRefusal.OtherProcess (self + 1)))

        for pid in [ 0 ; -1 ; -self ; Int32.MinValue ] do
            UnixSignal.kill live pid 0 linux
            |> shouldEqual (Error (KillRefusal.ProcessGroup pid))

        // The same pid is the caller's own once the process is configured with it.
        UnixSignal.kill live (self + 1) 0 (withPid (self + 1) linux)
        |> shouldEqual (Ok (Ok (SignalGeneration.ProcessContinues, withPid (self + 1) linux)))

    [<Test>]
    let ``kill by an init process is refused`` () : unit =
        let init = withPid 1 linux

        UnixSignal.kill live 1 9 init |> shouldEqual (Error KillRefusal.InitProcess)

    /// Measured by `docs/plans/2026-08-23-posix-kernel-extraction/kill-arguments.c`
    /// on Linux 6.18.5 and Darwin 25.6.0: the rows with the calling process as
    /// the target. 32 and 64 are signals on Linux (the probe sent them to its
    /// own child rather than to itself, and both were accepted) and are past
    /// Darwin's `NSIG` of 32.
    [<Test>]
    let ``kill of the calling process answers the measured rows`` () : unit =
        let answer (flavour : SimulatedUnixFlavour) (signo : int) : string =
            match UnixSignal.kill live self signo (systemOn flavour) with
            | Ok (Ok _) -> "OK"
            | Ok (Error errno) -> $"%O{errno}"
            | Error refusal -> $"refused %O{refusal}"

        let rows : (int * string * string) list =
            [
                // signo, Linux, Darwin
                0, "OK", "OK"
                -1, "EINVAL", "EINVAL"
                65, "EINVAL", "EINVAL"
                1000, "EINVAL", "EINVAL"
                Int32.MinValue, "EINVAL", "EINVAL"
                32, "OK", "EINVAL"
                64, "OK", "EINVAL"
            ]

        for signo, onLinux, onDarwin in rows do
            (signo, answer SimulatedUnixFlavour.Linux signo) |> shouldEqual (signo, onLinux)

            (signo, answer SimulatedUnixFlavour.Darwin signo)
            |> shouldEqual (signo, onDarwin)

    [<Test>]
    let ``kill of the calling process is EINVAL exactly when the number is neither 0 nor a signal`` () : unit =
        let gen =
            gen {
                let! flavour = Gen.elements flavours
                let! signo = signoGen
                return flavour, signo
            }

        let property (flavour : SimulatedUnixFlavour, signo : int) : unit =
            let system = systemOn flavour
            let valid = signo >= 0 && signo <= highestSigno flavour

            match UnixSignal.kill live self signo system, valid with
            | Ok (Error errno), false -> errno |> shouldEqual UnixError.EINVAL
            | Ok (Ok (generation, after)), true ->
                // A valid number is sent, and sending is exactly generating
                // the signal it names at the whole process.
                let expected =
                    if signo = 0 then
                        SignalGeneration.ProcessContinues, system.Process.Signals
                    else
                        SignalState.generate
                            live
                            {
                                Signal = Signal.Other signo
                                Target = ValueNone
                            }
                            system.Process.Signals

                (generation, after.Process.Signals) |> shouldEqual expected

                { after with
                    Process =
                        { after.Process with
                            Signals = system.Process.Signals
                        }
                }
                |> shouldEqual system

            | other, _ -> failwith $"kill(self, %d{signo}) under %O{flavour}: valid=%b{valid}, got %O{other}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    /// Linux finds the target before it looks at the signal number: `kill(2)`
    /// of a pid naming no process is ESRCH whatever the number, where Darwin
    /// says EINVAL (measured, `kill-arguments.c`). Whether a pid names a process
    /// is exactly what this kernel cannot know, so a number it would call
    /// EINVAL for the calling process must still be refused for another.
    [<Test>]
    let ``kill of any other target is refused whatever the signal number`` () : unit =
        let gen =
            gen {
                let! flavour = Gen.elements flavours
                let! pid = ArbMap.defaults |> ArbMap.generate<int> |> Gen.filter (fun pid -> pid <> self)
                let! signo = signoGen
                return flavour, pid, signo
            }

        let property (flavour : SimulatedUnixFlavour, pid : int, signo : int) : unit =
            match UnixSignal.kill live pid signo (systemOn flavour) with
            | Error (KillRefusal.OtherProcess refused) when pid > 0 -> refused |> shouldEqual pid
            | Error (KillRefusal.ProcessGroup refused) when pid <= 0 -> refused |> shouldEqual pid
            | other -> failwith $"kill(%d{pid}, %d{signo}) under %O{flavour}: expected a refusal, got %O{other}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)
