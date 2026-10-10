namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSignal.pthreadKill`, held to the rows of
/// `docs/plans/2026-08-23-posix-kernel-extraction/raise-sweep.c`, measured on
/// Linux 6.18.5 (aarch64, glibc 2.41) and Darwin 27.0.0 (arm64).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPthreadKill =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private leader : int = 0
    let private worker : int = 1

    let private systemOnWith
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        (flavour : SimulatedUnixFlavour)
        : UnixSystem<int, string>
        =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> configure
        |> Launched.boot UnixSystem.pipedStandardStreams leader (CpuId 0)

    let private systemLaunchedWith
        (configure : ProcessLaunch<int> -> ProcessLaunch<int>)
        (flavour : SimulatedUnixFlavour)
        : UnixSystem<int, string>
        =
        UnixSystem.initial (HostPlatform.platformOf flavour)
        |> Launched.bootWith configure UnixSystem.pipedStandardStreams leader (CpuId 0)

    let private systemOn (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> = systemOnWith id flavour

    let private withWorker (system : UnixSystem<int, string>) : UnixSystem<int, string> = Tasks.spawn worker system

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

    let private self (system : UnixSystem<int, string>) : int32 =
        ProcessId.toInt32 (UnixSystem.processId system)

    /// The highest signal number, written out rather than taken from
    /// `Signal.highestSignoUnder`, so that this oracle and the implementation
    /// share nothing but the measurement.
    let private highestSigno (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 64
        | SimulatedUnixFlavour.Darwin -> 31

    /// The numbers a task can be sent with `pthread_kill`: every signal but
    /// glibc's own 32 and 33, measured to be refused with EINVAL though
    /// `kill(2)` sends them.
    let private sendable (flavour : SimulatedUnixFlavour) (signo : int) : bool =
        signo = 0
        || signo >= 1
           && signo <= highestSigno flavour
           && not (flavour = SimulatedUnixFlavour.Linux && (signo = 32 || signo = 33))

    /// Every signal a handler can be installed for: the probe's
    /// `all_catchable`.
    let private catchable (flavour : SimulatedUnixFlavour) : int list =
        [ 1 .. highestSigno flavour ]
        |> List.filter (fun signo ->
            match flavour, signo with
            | _, 9 -> false
            | SimulatedUnixFlavour.Linux, (19 | 32 | 33) -> false
            | SimulatedUnixFlavour.Darwin, 17 -> false
            | _ -> true
        )

    let private signoGen : Gen<int> =
        Gen.oneof
            [
                Gen.choose (-3, 70)
                Gen.elements [ Int32.MinValue ; Int32.MinValue + 1 ; Int32.MaxValue ; 128 ; 255 ; 1000 ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    let private answer (system : UnixSystem<int, string>) (target : int) (signo : int) : string =
        match UnixSignal.pthreadKill target signo system with
        | Ok (Ok _) -> "OK"
        | Ok (Error errno) -> $"%O{errno}"
        | Error refusal -> $"refused %O{refusal}"

    /// The probe's `args` rows: `raise` and `pthread_kill(pthread_self())`
    /// answered alike on both flavours, row for row.
    [<Test>]
    let ``pthread_kill of the calling task answers the measured rows`` () : unit =
        let rows : (int * string * string) list =
            [
                // signo, Linux, Darwin
                0, "OK", "OK"
                -1, "EINVAL", "EINVAL"
                1, "OK", "OK"
                31, "OK", "OK"
                32, "EINVAL", "EINVAL"
                33, "EINVAL", "EINVAL"
                34, "OK", "EINVAL"
                64, "OK", "EINVAL"
                65, "EINVAL", "EINVAL"
                66, "EINVAL", "EINVAL"
                128, "EINVAL", "EINVAL"
                1000, "EINVAL", "EINVAL"
                Int32.MinValue, "EINVAL", "EINVAL"
                Int32.MaxValue, "EINVAL", "EINVAL"
            ]

        for signo, onLinux, onDarwin in rows do
            (signo, answer (systemOn SimulatedUnixFlavour.Linux) leader signo)
            |> shouldEqual (signo, onLinux)

            (signo, answer (systemOn SimulatedUnixFlavour.Darwin) leader signo)
            |> shouldEqual (signo, onDarwin)

    [<Test>]
    let ``pthread_kill is EINVAL exactly when the number cannot be sent, and otherwise generates the signal at its target``
        ()
        : unit
        =
        let gen =
            gen {
                let! flavour = Gen.elements flavours
                let! signo = signoGen
                let! target = Gen.elements [ leader ; worker ]
                let! coreDumps = Gen.elements [ CoreDumps.Suppressed ; CoreDumps.Written ]
                // A handler frame on either task, blocking a few signals, so
                // that some sends find their target blocking them.
                let! framed = Gen.elements [ None ; Some leader ; Some worker ]
                let! blocked = Gen.subListOf [ 1 ; 2 ; 10 ; 15 ; 20 ; 28 ; 29 ; 34 ]
                return flavour, signo, target, coreDumps, framed, blocked
            }

        let property
            (
                flavour : SimulatedUnixFlavour,
                signo : int,
                target : int,
                coreDumps : CoreDumps,
                framed : int option,
                blocked : int list
            )
            : unit
            =
            let numbering = numberingOf flavour

            let system =
                let system =
                    systemLaunchedWith (ProcessLaunch.withCoreDumps coreDumps) flavour |> withWorker

                match framed with
                | None -> system
                | Some task ->
                    let mask =
                        blocked
                        |> List.choose (fun signo -> Signal.ofRawSignoUnder numbering signo |> ValueOption.toOption)
                        |> Set.ofList

                    HandlerFrames.enterIn "h" task mask system

            match UnixSignal.pthreadKill target signo system, sendable flavour signo with
            | Ok (Error errno), false -> errno |> shouldEqual UnixError.EINVAL
            | Ok (Ok outcome), true ->
                let expected =
                    if signo = 0 then
                        SignalGeneration.ProcessContinues system.Process.Signals
                    else
                        match
                            SignalState.generate
                                coreDumps
                                leader
                                (Set.ofList [ leader ; worker ])
                                {
                                    Signal =
                                        match
                                            Signal.ofRawSignoUnder (SignalState.numbering system.Process.Signals) signo
                                        with
                                        | ValueSome signal -> signal
                                        | ValueNone -> failwith $"%d{signo} was sent, so it is a signal"
                                    Target = ValueSome target
                                }
                                system.Process.Signals
                        with
                        | Ok generation -> generation
                        | Error refusal -> failwith $"generating %d{signo} at %d{target} was refused: %O{refusal}"

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
                | _ ->
                    failwith
                        $"pthread_kill(%d{target}, %d{signo}) under %O{flavour} answered %O{outcome}, but generating it is %O{expected}"
            | other, valid ->
                failwith $"pthread_kill(%d{target}, %d{signo}) under %O{flavour}: sendable=%b{valid}, got %O{other}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    /// Every delivery of `signal` to `task` from here on: `task` returns to user
    /// mode, runs each handler frame it is given and returns from it, until it
    /// is given nothing more. Fails on any delivery but a handler frame.
    let private drain
        (signal : Signal)
        (task : int)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let rec go (count : int) (rounds : int) (system : UnixSystem<int, string>) =
            if rounds > 100 then
                failwith $"%O{task} was still being given signals after 100 returns"

            match UnixSignal.onReturnToUser task system with
            | Error refusal -> failwith $"onReturnToUser %O{task} refused: %O{refusal}"
            | Ok (ReturnToUserOutcome.Resumes system) -> count, system
            | Ok (ReturnToUserOutcome.RunHandlers (frames, system)) ->
                let ofSignal =
                    frames |> List.filter (fun frame -> frame.Entry.Signal = signal) |> List.length

                let system =
                    (system, frames)
                    ||> List.fold (fun system frame -> UnixSignal.sigreturn task frame.Id system)

                go (count + ofSignal) (rounds + 1) system
            | Ok (ReturnToUserOutcome.ProcessStopped _ as other)
            | Ok (ReturnToUserOutcome.ContinueDiscarded _ as other)
            | Ok (ReturnToUserOutcome.ProcessEnded _ as other) -> failwith $"onReturnToUser %O{task} answered %A{other}"

        go 0 0 system

    let private catchIn (signal : Signal) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals =
                        SignalState.setDisposition
                            signal
                            (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
                            system.Process.Signals
                }
        }

    /// `task` inside a handler that blocks `signal`, entered through a carrier
    /// other than `signal`.
    let private blocking (signal : Signal) (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let carrier =
            if signal = HandlerFrames.carrier then
                Signal.SIGWINCH
            else
                HandlerFrames.carrier

        let tasks = system.Tasks |> Map.keys |> Set.ofSeq

        { system with
            Process =
                { system.Process with
                    Signals =
                        HandlerFrames.enterVia
                            carrier
                            "carrier"
                            system.Leader
                            tasks
                            task
                            (Set.singleton signal)
                            system.Process.Signals
                }
        }

    let private leave (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals = HandlerFrames.leave task system.Process.Signals
                }
        }

    let private continues (what : string) (answer : Result<Result<KillOutcome<int, string>, UnixError>, 'Refusal>) =
        match answer with
        | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
        | other -> failwith $"%s{what}: %O{other}"

    /// The probe's `handler` rows: a caught signal raised by either thread runs
    /// its handler on that thread, before `raise` returns, and on no other.
    [<Test>]
    let ``a caught signal raised by a task is taken by that task alone, at its next return to user mode`` () : unit =
        for flavour in flavours do
            let numbering = numberingOf flavour

            for signo in catchable flavour do
                let signal = Signal.ofRawSignoUnder numbering signo |> ValueOption.get

                for raiser in [ leader ; worker ] do
                    let bystander = if raiser = leader then worker else leader

                    let raised =
                        systemOn flavour
                        |> withWorker
                        |> catchIn signal
                        |> UnixSignal.pthreadKill raiser signo
                        |> continues $"%O{flavour} raise %d{signo} on %d{raiser}"

                    let atBystander, raised = drain signal bystander raised
                    let atRaiser, _ = drain signal raiser raised

                    (flavour, signo, raiser, atRaiser, atBystander)
                    |> shouldEqual (flavour, signo, raiser, 1, 0)

    /// The probe's `blocked` rows: a signal a task raises while it blocks it
    /// stays pending on that task, though another task does not block it, and
    /// the raiser takes it once it unblocks it.
    [<Test>]
    let ``a signal a task raises while blocking it waits for that task, whoever else could take it`` () : unit =
        for flavour in flavours do
            let numbering = numberingOf flavour

            for signo in catchable flavour do
                let signal = Signal.ofRawSignoUnder numbering signo |> ValueOption.get

                for raiser in [ leader ; worker ] do
                    let bystander = if raiser = leader then worker else leader

                    let raised =
                        systemOn flavour
                        |> withWorker
                        |> catchIn signal
                        |> blocking signal raiser
                        |> UnixSignal.pthreadKill raiser signo
                        |> continues $"%O{flavour} raise %d{signo} on %d{raiser}"

                    let atBystander, raised = drain signal bystander raised
                    let whileBlocked, raised = drain signal raiser raised

                    SignalState.pending raised.Process.Signals
                    |> shouldEqual
                        [
                            {
                                Signal = signal
                                Target = ValueSome raiser
                            }
                        ]

                    let afterUnblock, _ = raised |> leave raiser |> drain signal raiser

                    (flavour, signo, raiser, atBystander, whileBlocked, afterUnblock)
                    |> shouldEqual (flavour, signo, raiser, 0, 0, 1)

    /// One of the probe's `pair` shapes: what every task blocks, which sends are
    /// made, and how many instances each task took, main then worker, once each
    /// unblocked in turn, main first. `None` where this library refuses.
    type private PairShape =
        {
            Name : string
            Tasks : int list
            /// `kill(2)`, or `pthread_kill` of the task named.
            Sends : int option list
            Linux : (int * int) option
            Darwin : (int * int) option
        }

    let private pairShapes : PairShape list =
        // Measured with every signal `catchable` names, the same for each; the
        // sender of a `kill` does not matter to this library, so the probe's
        // shapes 4 and 5 (the worker kills) are 2 and 3 here. Darwin took the
        // kill and the main thread's raise as one instance, the Darwin rows
        // this library refuses.
        [
            {
                Name = "one thread: kill, raise"
                Tasks = [ leader ]
                Sends = [ None ; Some leader ]
                Linux = Some (2, 0)
                Darwin = None
            }
            {
                Name = "one thread: raise, kill"
                Tasks = [ leader ]
                Sends = [ Some leader ; None ]
                Linux = Some (2, 0)
                Darwin = None
            }
            {
                Name = "two threads: kill, raise on main"
                Tasks = [ leader ; worker ]
                Sends = [ None ; Some leader ]
                Linux = Some (2, 0)
                Darwin = None
            }
            {
                Name = "two threads: kill, pthread_kill of the worker"
                Tasks = [ leader ; worker ]
                Sends = [ None ; Some worker ]
                Linux = Some (1, 1)
                Darwin = Some (1, 1)
            }
        ]

    [<Test>]
    let ``a signal sent to the process and one sent to a task are held as the probe measured, or refused`` () : unit =
        for flavour in flavours do
            let numbering = numberingOf flavour

            for signo in catchable flavour do
                let signal = Signal.ofRawSignoUnder numbering signo |> ValueOption.get

                for shape in pairShapes do
                    let start =
                        let system = systemOn flavour

                        let system =
                            if List.contains worker shape.Tasks then
                                withWorker system
                            else
                                system

                        (catchIn signal system, shape.Tasks)
                        ||> List.fold (fun system task -> blocking signal task system)

                    let send (system : UnixSystem<int, string>) (target : int option) =
                        match target with
                        | None ->
                            match UnixSignal.kill (self system) signo system with
                            | Ok (Ok (KillOutcome.ProcessContinues system)) -> Ok system
                            | Error (KillRefusal.Receiver refusal) -> Error refusal
                            | other -> failwith $"kill: %O{other}"
                        | Some task ->
                            match UnixSignal.pthreadKill task signo system with
                            | Ok (Ok (KillOutcome.ProcessContinues system)) -> Ok system
                            | Error (ThreadKillRefusal.Receiver refusal) -> Error refusal
                            | other -> failwith $"pthread_kill: %O{other}"

                    let sent =
                        (Ok start, shape.Sends)
                        ||> List.fold (fun system target -> system |> Result.bind (fun system -> send system target))

                    let expected =
                        match flavour with
                        | SimulatedUnixFlavour.Linux -> shape.Linux
                        | SimulatedUnixFlavour.Darwin -> shape.Darwin

                    match sent, expected with
                    | Ok sent, Some (onMain, onWorker) ->
                        let atMain, sent = sent |> leave leader |> drain signal leader

                        let atWorker =
                            if List.contains worker shape.Tasks then
                                sent |> leave worker |> drain signal worker |> fst
                            else
                                0

                        (flavour, signo, shape.Name, atMain, atWorker)
                        |> shouldEqual (flavour, signo, shape.Name, onMain, onWorker)
                    | Error refusal, None ->
                        refusal |> shouldEqual (SignalReceiverRefusal.PendingForProcessAndLeader signal)
                    | _ -> failwith $"%O{flavour} %d{signo} %s{shape.Name}: expected %A{expected}, got %A{sent}"

    [<Test>]
    let ``Darwin's refusal is only of a signal that would be left pending`` () : unit =
        // SIGTERM pending on the process, at its default disposition, with the
        // leader no longer blocking it: raising it at the leader kills the
        // process rather than leaving a second instance.
        let numbering = numberingOf SimulatedUnixFlavour.Darwin

        let pendingOnProcess (signal : Signal) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
            let signo = Signal.toRawSignoUnder numbering signal

            system
            |> catchIn signal
            |> blocking signal leader
            |> fun system -> UnixSignal.kill (self system) signo system
            |> continues "kill"
            |> leave leader
            |> fun system ->
                { system with
                    Process =
                        { system.Process with
                            Signals = SignalState.setDisposition signal SignalDisposition.Default system.Process.Signals
                        }
                }

        let system = systemOn SimulatedUnixFlavour.Darwin |> pendingOnProcess Signal.SIGTERM

        match UnixSignal.pthreadKill leader 15 system with
        | Ok (Ok (KillOutcome.ProcessEnded ended)) ->
            ended.Termination
            |> shouldEqual (ProcessTermination.Signaled (Signal.SIGTERM, false))
        | other -> failwith $"raise SIGTERM: %O{other}"

    [<Test>]
    let ``a default SIGCONT the leader does not block is discarded as it is sent, leaving nothing to merge with``
        ()
        : unit
        =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/sigcont-generation.c`
        // on Linux 6.18.5 and Darwin 27.0.0: discarded at generation, as an
        // ignored signal is.
        for flavour in flavours do
            let system = systemOn flavour
            let sigcont = Signal.toRawSignoUnder (numberingOf flavour) Signal.SIGCONT

            let sent = UnixSignal.kill (self system) sigcont system |> continues "kill SIGCONT"
            SignalState.pending sent.Process.Signals |> shouldEqual []

            UnixSignal.pthreadKill leader sigcont sent
            |> continues "raise SIGCONT"
            |> fun system -> SignalState.pending system.Process.Signals
            |> shouldEqual []

    [<Test>]
    let ``Darwin's refusal covers SIGCONT at its default, which the leader blocks`` () : unit =
        // Pending on the process, so a raise of it at the leader would be the
        // second instance.
        let darwin = systemOn SimulatedUnixFlavour.Darwin |> blocking Signal.SIGCONT leader

        let sigcont =
            Signal.toRawSignoUnder (numberingOf SimulatedUnixFlavour.Darwin) Signal.SIGCONT

        let sent = UnixSignal.kill (self darwin) sigcont darwin |> continues "kill SIGCONT"

        UnixSignal.pthreadKill leader sigcont sent
        |> shouldEqual (
            Error (ThreadKillRefusal.Receiver (SignalReceiverRefusal.PendingForProcessAndLeader Signal.SIGCONT))
        )

        // Linux keeps the two apart, as Darwin does for another thread.
        let linux = systemOn SimulatedUnixFlavour.Linux |> blocking Signal.SIGCONT leader

        let sigcont =
            Signal.toRawSignoUnder (numberingOf SimulatedUnixFlavour.Linux) Signal.SIGCONT

        let sent = UnixSignal.kill (self linux) sigcont linux |> continues "kill SIGCONT"

        UnixSignal.pthreadKill leader sigcont sent
        |> continues "raise SIGCONT"
        |> fun system -> SignalState.pending system.Process.Signals |> List.length
        |> shouldEqual 2

    [<Test>]
    let ``pthread_kill by an init process is refused`` () : unit =
        for flavour in flavours do
            let init =
                systemOnWith (Launched.processId (ProcessId.parseOrFail "test" 1)) flavour

            UnixSignal.pthreadKill leader 15 init
            |> shouldEqual (Error ThreadKillRefusal.InitProcess)

    [<Test>]
    let ``pthread_kill of a task the process does not have fails loudly`` () : unit =
        for flavour in flavours do
            for signo in [ 0 ; -1 ; 15 ] do
                Assert.Throws<exn> (fun () -> UnixSignal.pthreadKill worker signo (systemOn flavour) |> ignore<_>)
                |> ignore<exn>
