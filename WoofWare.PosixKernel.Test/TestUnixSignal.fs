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

    let private systemOnWith
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        (flavour : SimulatedUnixFlavour)
        : UnixSystem<int, string>
        =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> configure
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private systemLaunchedWith
        (configure : ProcessLaunch<int> -> ProcessLaunch<int>)
        (flavour : SimulatedUnixFlavour)
        : UnixSystem<int, string>
        =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.bootWith configure UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private systemOn (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> = systemOnWith id flavour

    let private linux : UnixSystem<int, string> = systemOn SimulatedUnixFlavour.Linux

    /// A Linux process booted with `pid`.
    let private linuxWithPid (pid : int32) : UnixSystem<int, string> =
        systemOnWith (Launched.processId (ProcessId.parseOrFail "test" pid)) SimulatedUnixFlavour.Linux

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
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGKILL, false))

            EndedMachine.assertTasksGone system ended
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
                let system = systemLaunchedWith (ProcessLaunch.withCoreDumps coreDumps) flavour

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
        UnixSignal.kill (self + 1) 0 (linuxWithPid (self + 1))
        |> shouldEqual (Ok (Ok (KillOutcome.ProcessContinues (linuxWithPid (self + 1)))))

    [<Test>]
    let ``kill by an init process is refused`` () : unit =
        let init = linuxWithPid 1

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
            let system = systemLaunchedWith (ProcessLaunch.withCoreDumps coreDumps) flavour

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
                                    Signal =
                                        match
                                            Signal.ofRawSignoUnder (SignalState.numbering system.Process.Signals) signo
                                        with
                                        | ValueSome signal -> signal
                                        | ValueNone -> failwith $"%d{signo} was sent, so it is a signal"
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

                    EndedMachine.assertTasksGone system ended
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

    // ------------- A default that terminates, taken on return ------------- //

    /// `SIG_BLOCK` and `SIG_UNBLOCK` as each `<signal.h>` numbers them.
    let private sigBlock (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0
        | SimulatedUnixFlavour.Darwin -> 1

    let private sigUnblock (flavour : SimulatedUnixFlavour) : int = sigBlock flavour + 1

    /// The signals a death is checked for, each with the core flag its death
    /// carries under `coreDumps`: SIGQUIT dumps core on both flavours, and
    /// SIGTERM on neither.
    let private fatalSignals (coreDumps : CoreDumps) : (Signal * bool) list =
        [ Signal.SIGTERM, false ; Signal.SIGQUIT, (coreDumps = CoreDumps.Written) ]

    /// A process of `flavour` whose only task blocks `signal`, which it has
    /// sent itself with `kill(2)`, so that it is pending at its default.
    let private pendingBehindMask
        (flavour : SimulatedUnixFlavour)
        (coreDumps : CoreDumps)
        (signal : Signal)
        : UnixSystem<int, string>
        =
        let system = systemLaunchedWith (ProcessLaunch.withCoreDumps coreDumps) flavour
        let numbering = SignalState.numbering system.Process.Signals
        let only = SignalMask.ofSignals numbering (Set.singleton signal)

        let system =
            match UnixSignal.pthreadSigmask 0 (sigBlock flavour) (Some only) system with
            | Ok (_, system) -> system
            | Error errno -> failwith $"pthread_sigmask: %O{errno}"

        match UnixSignal.kill self (Signal.toRawSignoUnder numbering signal) system with
        | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
        | other -> failwith $"kill(self, %O{signal}) under %O{flavour}: %O{other}"

    /// The process `before` was, ended by `onReturnToUser` of its only task,
    /// and then ended on its machine: the termination each reports.
    let private endedOnReturn (before : UnixSystem<int, string>) : ProcessTermination * ProcessTermination =
        match UnixSignal.onReturnToUser 0 before with
        | Ok (ReturnToUserOutcome.ProcessEnded ended) ->
            let onMachine, machine =
                match SimulatedMachine.endProcess ended (SimulatedMachine.ofSystem (EndedProcess.endedIn ended)) with
                | Ok ended -> ended
                | Error refusal -> failwith (ProcessEndRefusal.describe refusal)

            EndedProcess.processId ended |> shouldEqual (UnixSystem.processId before)
            SimulatedMachine.processIds machine |> shouldBeEmpty
            SimulatedMachine.checkInvariants machine |> shouldEqual []
            EndedProcess.termination ended, onMachine
        | other -> failwith $"onReturnToUser answered %O{other}"

    [<Test>]
    let ``a pending default that terminates, which sigsuspend unblocks, ends the process as the task returns``
        ()
        : unit
        =
        for flavour in flavours do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                for signal, coreDumped in fatalSignals coreDumps do
                    let suspended =
                        match UnixSignal.sigsuspend 0 SignalMask.empty (pendingBehindMask flavour coreDumps signal) with
                        | Ok (SigsuspendOutcome.Failed UnixError.EINTR, system) -> system
                        | other -> failwith $"sigsuspend under %O{flavour}: %O{other}"

                    let expected = ProcessTermination.Signaled (signal, coreDumped)
                    endedOnReturn suspended |> shouldEqual (expected, expected)

    [<Test>]
    let ``a pending default that terminates, which pthread_sigmask unblocks, ends the process as the task returns``
        ()
        : unit
        =
        for flavour in flavours do
            for coreDumps in [ CoreDumps.Suppressed ; CoreDumps.Written ] do
                for signal, coreDumped in fatalSignals coreDumps do
                    let pending = pendingBehindMask flavour coreDumps signal
                    let numbering = SignalState.numbering pending.Process.Signals
                    let only = SignalMask.ofSignals numbering (Set.singleton signal)

                    let unblocked =
                        match UnixSignal.pthreadSigmask 0 (sigUnblock flavour) (Some only) pending with
                        | Ok (_, system) -> system
                        | Error errno -> failwith $"pthread_sigmask: %O{errno}"

                    let expected = ProcessTermination.Signaled (signal, coreDumped)
                    endedOnReturn unblocked |> shouldEqual (expected, expected)

    /// The signals the property below sends, between them at every default
    /// action: SIGTERM and SIGHUP terminate, SIGQUIT terminates with a core
    /// dump, SIGCHLD is ignored, SIGCONT continues and SIGTSTP stops.
    let private returnPool : Signal list =
        [
            Signal.SIGTERM
            Signal.SIGHUP
            Signal.SIGQUIT
            Signal.SIGUSR1
            Signal.SIGCHLD
            Signal.SIGCONT
            Signal.SIGTSTP
        ]

    /// One send: `kill(2)` of the calling process when `target` is `None`, and
    /// `pthread_kill(3)` of the task `target` names otherwise.
    type private Send =
        {
            Target : int option
            Signal : Signal
        }

    type private ReturnCase =
        {
            Flavour : SimulatedUnixFlavour
            CoreDumps : CoreDumps
            /// How many tasks the process has, numbered from 0, its leader.
            Tasks : int
            Dispositions : (Signal * SignalDisposition<string>) list
            /// Each task's mask while the signals are sent.
            BlockedWhileSent : Set<Signal> list
            Sends : Send list
            /// The task that returns to user mode, and the mask it does so under.
            Returning : int
            MaskOnReturn : Set<Signal>
        }

    let private returnCaseGen : Gen<ReturnCase> =
        gen {
            let! flavour = Gen.elements flavours
            let! coreDumps = Gen.elements [ CoreDumps.Suppressed ; CoreDumps.Written ]
            let! tasks = Gen.choose (1, 2)

            let disposition : Gen<SignalDisposition<string>> =
                Gen.frequency
                    [
                        3, Gen.constant SignalDisposition.Default
                        1, Gen.constant SignalDisposition.Ignore
                        1, Gen.constant (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
                    ]

            let! dispositions = Gen.listOfLength returnPool.Length disposition
            let someSignals = Gen.subListOf returnPool |> Gen.map Set.ofList

            // Mostly everything blocked, so that the sends stay pending rather
            // than taking their defaults at once.
            let blocked =
                Gen.frequency [ 3, Gen.constant (Set.ofList returnPool) ; 1, someSignals ]

            let! blockedWhileSent = Gen.listOfLength tasks blocked

            let send =
                gen {
                    let! target = Gen.elements (None :: List.init tasks Some)
                    let! signal = Gen.elements returnPool

                    return
                        {
                            Target = target
                            Signal = signal
                        }
                }

            let! sends = Gen.choose (0, 5) |> Gen.bind (fun n -> Gen.listOfLength n send)
            let! returning = Gen.choose (0, tasks - 1)
            let! maskOnReturn = someSignals

            return
                {
                    Flavour = flavour
                    CoreDumps = coreDumps
                    Tasks = tasks
                    Dispositions = List.zip returnPool dispositions
                    BlockedWhileSent = blockedWhileSent
                    Sends = sends
                    Returning = returning
                    MaskOnReturn = maskOnReturn
                }
        }

    /// The process `case` describes, with its signals sent: `None` if a send
    /// stopped or ended it at once, as `kill` and `pthread_kill` answer that.
    /// The paths of a return to user mode that the property must reach.
    [<RequireQualifiedAccess>]
    type private ReturnLabel =
        /// A default that terminates ended the process.
        | Ended
        /// Such an ending dumped core.
        | DumpedCore
        /// The return pushed handler frames.
        | RanHandlers

    let private sentFor (case : ReturnCase) : UnixSystem<int, string> option =
        let setMask (task : int) (signals : Set<Signal>) (system : UnixSystem<int, string>) =
            let mask =
                SignalMask.ofSignals (SignalState.numbering system.Process.Signals) signals

            match UnixSignal.pthreadSigmask task (sigBlock case.Flavour + 2) (Some mask) system with
            | Ok (_, system) -> system
            | Error errno -> failwith $"pthread_sigmask: %O{errno}"

        let booted =
            systemLaunchedWith (ProcessLaunch.withCoreDumps case.CoreDumps) case.Flavour
            |> fun system ->
                (system, [ 1 .. case.Tasks - 1 ])
                ||> List.fold (fun system task -> Tasks.spawn task system)

        let configured =
            (booted, case.Dispositions)
            ||> List.fold (fun system (signal, disposition) ->
                match
                    UnixSignal.sigaction
                        (Signal.toRawSignoUnder (SignalState.numbering system.Process.Signals) signal)
                        (Some disposition)
                        system
                with
                | Ok (_, system) -> system
                | Error errno -> failwith $"sigaction of %O{signal}: %O{errno}"
            )

        let masked =
            (configured, List.indexed case.BlockedWhileSent)
            ||> List.fold (fun system (task, signals) -> setMask task signals system)

        let sent =
            (Some masked, case.Sends)
            ||> List.fold (fun system send ->
                match system with
                | None -> None
                | Some system ->

                let signo =
                    Signal.toRawSignoUnder (SignalState.numbering system.Process.Signals) send.Signal

                let answer =
                    match send.Target with
                    | None ->
                        UnixSignal.kill self signo system
                        |> Result.mapError (fun refusal -> $"%O{refusal}")
                    | Some task ->
                        UnixSignal.pthreadKill task signo system
                        |> Result.mapError (fun refusal -> $"%O{refusal}")

                match answer with
                | Ok (Ok (KillOutcome.ProcessContinues system)) -> Some system
                | Ok (Ok (KillOutcome.ProcessStopped _))
                | Ok (Ok (KillOutcome.ProcessEnded _)) -> None
                // A signal the leader blocks and another task does not is
                // refused; the send is skipped.
                | Error _ -> Some system
                | Ok (Error errno) -> failwith $"sending %O{send.Signal}: %O{errno}"
            )

        sent |> Option.map (setMask case.Returning case.MaskOnReturn)

    [<Test>]
    let ``a task's return to user mode answers what the signal state decides, and a default that terminates ends the process``
        ()
        : unit
        =
        let property (cover : ReturnLabel -> unit) (case : ReturnCase) : unit =
            match sentFor case with
            | None -> ()
            | Some system ->

            let tasks = system.Tasks |> Map.keys |> Set.ofSeq

            let decided =
                SignalState.onReturnToUser
                    system.Process.CoreDumps
                    system.Leader
                    tasks
                    case.Returning
                    system.Process.Signals

            let withSignals (signals : SignalState<int, string>) : UnixSystem<int, string> =
                { system with
                    Process =
                        { system.Process with
                            Signals = signals
                        }
                }

            match UnixSignal.onReturnToUser case.Returning system, decided with
            | Error refusal, Error expected -> refusal |> shouldEqual expected
            | Ok (ReturnToUserOutcome.Resumes after), Ok (None, signals) -> after |> shouldEqual (withSignals signals)
            | Ok (ReturnToUserOutcome.RunHandlers (frames, after)),
              Ok (Some (SignalDelivery.RunHandlers expected), signals) ->
                cover ReturnLabel.RanHandlers
                frames |> shouldEqual expected
                after |> shouldEqual (withSignals signals)
            | Ok (ReturnToUserOutcome.ProcessStopped (signal, after)),
              Ok (Some (SignalDelivery.DefaultStop expected), signals) ->
                signal |> shouldEqual expected
                after |> shouldEqual (withSignals signals)
            | Ok (ReturnToUserOutcome.ContinueDiscarded (signal, after)),
              Ok (Some (SignalDelivery.DefaultContinue expected), signals) ->
                signal |> shouldEqual expected
                after |> shouldEqual (withSignals signals)
            | Ok (ReturnToUserOutcome.ProcessEnded death),
              Ok (Some (SignalDelivery.DefaultTerminate (signal, coreDumped)), signals) ->
                cover ReturnLabel.Ended

                if coreDumped then
                    cover ReturnLabel.DumpedCore

                // Of the pool, SIGQUIT alone dumps core, on both flavours.
                coreDumped
                |> shouldEqual (signal = Signal.SIGQUIT && case.CoreDumps = CoreDumps.Written)

                EndedProcess.termination death
                |> shouldEqual (ProcessTermination.Signaled (signal, coreDumped))

                EndedMachine.assertTasksGone system death
                SignalState.tasksWithFrames death.FinalProcess.Signals |> shouldBeEmpty

                // The process as it stood once it had taken the signal, less
                // what it held for its tasks.
                let final = withSignals signals

                { death.FinalProcess with
                    Signals = final.Process.Signals
                }
                |> shouldEqual final.Process

                SignalState.pending death.FinalProcess.Signals
                |> shouldEqual (SignalState.pending signals |> List.filter (fun entry -> entry.Target.IsNone))

                match SimulatedMachine.endProcess death (SimulatedMachine.ofSystem (EndedProcess.endedIn death)) with
                | Ok (termination, machine) ->
                    termination |> shouldEqual (EndedProcess.termination death)
                    SimulatedMachine.processIds machine |> shouldBeEmpty
                    SimulatedMachine.checkInvariants machine |> shouldEqual []
                | Error refusal -> failwith (ProcessEndRefusal.describe refusal)
            | actual, expected ->
                failwith $"onReturnToUser answered %A{actual}, but the signal state decided %A{expected}"

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 2000) (Arb.fromGen returnCaseGen) property

        // Each about a third of what the fixed sample reaches.
        coverage.Count ReturnLabel.Ended |> shouldBeGreaterThan 121
        coverage.Count ReturnLabel.DumpedCore |> shouldBeGreaterThan 15
        coverage.Count ReturnLabel.RanHandlers |> shouldBeGreaterThan 59
