namespace WoofWare.PosixKernel.Test

open System
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
        UnixSystem.initial (HostPlatform.platformOf flavour) UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private linux : UnixSystem<int, string> = systemOn SimulatedUnixFlavour.Linux

    let private withPid (pid : int32) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixSystem.withProcessId "test" (ProcessId.parseOrFail "test" pid) system

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
    let ``kill of the calling process with SIGKILL ends it`` () : unit =
        let system = linux |> HandlerFrames.enterIn "h" 0 (Set.singleton Signal.SIGUSR1)

        match UnixSignal.kill self 9 system with
        | Ok (Ok (KillOutcome.ProcessEnded ended)) ->
            ended.Termination
            |> shouldEqual (ProcessTermination.Signaled (Signal.Other 9, false))

            ended.Machine |> shouldEqual system.Machine
            // The process's end takes its tasks' per-task entries with them.
            SignalState.tasksWithFrames ended.FinalProcess.Signals |> shouldBeEmpty

            { ended.FinalProcess with
                Signals = system.Process.Signals
            }
            |> shouldEqual system.Process
        | other -> failwith $"unexpected answer: %O{other}"

    [<Test>]
    let ``a death by a signal that dumps core carries the core flag exactly when the process writes dumps`` () : unit =
        // SIGQUIT dumps core on both flavours; SIGTERM on neither.
        for flavour in flavours do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                let system =
                    let system = systemOn flavour

                    { system with
                        Process = UnixProcessState.withCoreDumps coreDumps system.Process
                    }

                let death (signo : int) : ProcessTermination =
                    match UnixSignal.kill self signo system with
                    | Ok (Ok (KillOutcome.ProcessEnded ended)) -> ended.Termination
                    | other -> failwith $"kill(self, %d{signo}) under %O{flavour}: %O{other}"

                death 3
                |> shouldEqual (ProcessTermination.Signaled (Signal.SIGQUIT, coreDumps = CoreDumps.Written))

                death 15 |> shouldEqual (ProcessTermination.Signaled (Signal.SIGTERM, false))

    [<Test>]
    let ``the null signal to the calling process changes nothing`` () : unit =
        for flavour in flavours do
            let system = systemOn flavour

            UnixSignal.kill self 0 system
            |> shouldEqual (Ok (Ok (KillOutcome.ProcessContinues system)))

    [<Test>]
    let ``a signal the calling process cannot yet receive is left pending`` () : unit =
        // Its only task is in a handler that blocks SIGTERM, so SIGTERM waits
        // in the process-wide pending set.
        let blocking = linux |> HandlerFrames.enterIn "h" 0 (Set.singleton Signal.SIGTERM)

        match UnixSignal.kill self 15 blocking with
        | Ok (Ok (KillOutcome.ProcessContinues after)) ->
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
        UnixSignal.kill (self + 1) 0 linux
        |> shouldEqual (Error (KillRefusal.OtherProcess (self + 1)))

        for pid in [ 0 ; -1 ; -self ; Int32.MinValue ] do
            UnixSignal.kill pid 0 linux
            |> shouldEqual (Error (KillRefusal.ProcessGroup pid))

        // The same pid is the caller's own once the process is configured with it.
        UnixSignal.kill (self + 1) 0 (withPid (self + 1) linux)
        |> shouldEqual (Ok (Ok (KillOutcome.ProcessContinues (withPid (self + 1) linux))))

    [<Test>]
    let ``kill by an init process is refused`` () : unit =
        let init = withPid 1 linux

        UnixSignal.kill 1 9 init |> shouldEqual (Error KillRefusal.InitProcess)

    /// Measured by `docs/plans/2026-08-23-posix-kernel-extraction/kill-arguments.c`
    /// on Linux 6.18.5 and Darwin 25.6.0: the rows with the calling process as
    /// the target. 32 and 64 are signals on Linux (the probe sent them to its
    /// own child rather than to itself, and both were accepted) and are past
    /// Darwin's `NSIG` of 32.
    [<Test>]
    let ``kill of the calling process answers the measured rows`` () : unit =
        let answer (flavour : SimulatedUnixFlavour) (signo : int) : string =
            match UnixSignal.kill self signo (systemOn flavour) with
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
                let! coreDumps = Gen.elements [ CoreDumps.Suppressed ; CoreDumps.Written ]
                return flavour, signo, coreDumps
            }

        let property (flavour : SimulatedUnixFlavour, signo : int, coreDumps : CoreDumps) : unit =
            let system =
                let system = systemOn flavour

                { system with
                    Process = UnixProcessState.withCoreDumps coreDumps system.Process
                }

            let valid = signo >= 0 && signo <= highestSigno flavour

            match UnixSignal.kill self signo system, valid with
            | Ok (Error errno), false -> errno |> shouldEqual UnixError.EINVAL
            | Ok (Ok outcome), true ->
                // A valid number is sent, and sending is exactly generating
                // the signal it names at the whole process.
                let expected =
                    if signo = 0 then
                        SignalGeneration.ProcessContinues system.Process.Signals
                    else
                        match
                            SignalState.generate
                                coreDumps
                                0
                                (Set.singleton 0)
                                {
                                    Signal = Signal.Other signo
                                    Target = ValueNone
                                }
                                system.Process.Signals
                        with
                        | Ok generation -> generation
                        | Error refusal -> failwith $"generating %d{signo} was refused: %O{refusal}"

                // Everything but the signals is untouched by a process that
                // carries on or stops.
                let survivor (after : UnixSystem<int, string>) : unit =
                    { after with
                        Process =
                            { after.Process with
                                Signals = system.Process.Signals
                            }
                    }
                    |> shouldEqual system

                match outcome, expected with
                | KillOutcome.ProcessContinues after, SignalGeneration.ProcessContinues signals ->
                    after.Process.Signals |> shouldEqual signals
                    survivor after
                | KillOutcome.ProcessStopped (stoppedBy, after), SignalGeneration.ProcessStopped (signal, signals) ->
                    stoppedBy |> shouldEqual signal
                    after.Process.Signals |> shouldEqual signals
                    survivor after
                | KillOutcome.ProcessEnded ended, SignalGeneration.ProcessTerminated (signal, coreDumped) ->
                    ended.Termination
                    |> shouldEqual (ProcessTermination.Signaled (signal, coreDumped))

                    ended.Machine |> shouldEqual system.Machine
                    ended.FinalProcess |> shouldEqual system.Process
                | _ ->
                    failwith
                        $"kill(self, %d{signo}) under %O{flavour} answered %O{outcome}, but generating it is %O{expected}"

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
            match UnixSignal.kill pid signo (systemOn flavour) with
            | Error (KillRefusal.OtherProcess refused) when pid > 0 -> refused |> shouldEqual pid
            | Error (KillRefusal.ProcessGroup refused) when pid <= 0 -> refused |> shouldEqual pid
            | other -> failwith $"kill(%d{pid}, %d{signo}) under %O{flavour}: expected a refusal, got %O{other}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)
