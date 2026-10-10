namespace WoofWare.PosixKernel.Test

open System
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// When a SIGCONT that nothing blocks is discarded: every "gen", "sleep" and
/// "unblock" row of `docs/plans/2026-08-23-posix-kernel-extraction/sigcont-generation.c`
/// (measured 2026-10-10 on Linux 6.18.5 aarch64 with glibc 2.41, and Darwin
/// 27.0.0 arm64), replayed through the library from its embedded outputs, and
/// what its "stopinfo" rows say about a SIGSTOP and a pending SIGCONT.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSigcontGeneration =

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

    /// The probe's main thread, the process's leader: "m" in its rows.
    let private main : int = 0

    /// The probe's second thread: "s" in its rows.
    let private second : int = 1

    let private taskName (task : int) : string = if task = main then "m" else "s"

    /// The probe's names for the signals its rows name.
    let private probeName (signal : Signal) : string =
        match signal with
        | Signal.SIGCONT -> "CONT"
        | Signal.SIGUSR1 -> "USR1"
        | Signal.SIGURG -> "URG"
        | other -> failwith $"the probe names no row after %O{other}"

    /// The probe's dispositions, by its names for them.
    [<RequireQualifiedAccess>]
    type private Disposition =
        | DFL
        | IGN
        | H

    let private dispositionName (disposition : Disposition) : string =
        match disposition with
        | Disposition.DFL -> "DFL"
        | Disposition.IGN -> "IGN"
        | Disposition.H -> "H"

    /// The probe's counting handler.
    let private handler : SignalDisposition<string> =
        SignalDisposition.Catch (SignalCatch.ofHandler "count")

    let private toDisposition (disposition : Disposition) : SignalDisposition<string> =
        match disposition with
        | Disposition.DFL -> SignalDisposition.Default
        | Disposition.IGN -> SignalDisposition.Ignore
        | Disposition.H -> handler

    /// Why a replay stopped before its row's child exited: the library would
    /// not answer.
    [<RequireQualifiedAccess>]
    type private Stop =
        /// It will not say which task takes a signal, or what it does.
        | Receiver of SignalReceiverRefusal
        /// Any other refusal, which no measured row expects.
        | Other of string

    /// One replay of a probe row against the library.
    type private Replay =
        {
            Flavour : SimulatedUnixFlavour
            System : UnixSystem<int, string>
            /// The probe's `g_phase`.
            Phase : int
            /// Every handler run, as the probe's handler logs it, latest first.
            Runs : (Signal * int * int) list
            /// What the row prints, latest first.
            Printed : string list
            /// The sleeper's call's answer and the phase it came in, once it
            /// has returned.
            Returned : (string * int) option
        }

    type private Step = Replay -> Result<Replay, Stop>

    let private (>=>) (first : Step) (second : Step) : Step = fun r -> Result.bind second (first r)

    let private nothing : Step = Ok

    let private numbering (r : Replay) : SignalNumbering = numberingOf r.Flavour

    let private signo (r : Replay) (signal : Signal) : int32 =
        Signal.toRawSignoUnder (numbering r) signal

    let private withSystem (system : UnixSystem<int, string>) (r : Replay) : Replay =
        { r with
            System = system
        }

    let private print (text : string) : Step =
        fun r ->
            Ok
                { r with
                    Printed = text :: r.Printed
                }

    let private phase (n : int) : Step =
        fun r ->
            Ok
                { r with
                    Phase = n
                }

    /// `task` returns to user mode: it runs every handler the kernel pushes,
    /// each logged as the probe's handler logs itself, and the return goes on
    /// past a SIGCONT the kernel discards.
    let rec private returns (task : int) : Step =
        fun r ->
            match UnixSignal.onReturnToUser task r.System with
            | Error refusal -> Error (Stop.Receiver refusal)
            | Ok (ReturnToUserOutcome.Resumes system) -> Ok (withSystem system r)
            | Ok (ReturnToUserOutcome.ContinueDiscarded (_, system)) -> returns task (withSystem system r)
            | Ok (ReturnToUserOutcome.RunHandlers (frames, system)) -> runFrames task frames (withSystem system r)
            | Ok other -> failwith $"task %d{task}'s return to user mode: %A{other}"

    /// Each of `frames`' handlers runs, innermost first; each `sigreturn` is a
    /// return to user mode of its own.
    and private runFrames (task : int) (frames : HandlerFrame<int, string> list) : Step =
        fun r ->
            match frames with
            | [] -> Ok r
            | frame :: outer ->
                let r =
                    { r with
                        Runs = (frame.Entry.Signal, task, r.Phase) :: r.Runs
                        System = UnixSignal.sigreturn task frame.Id r.System
                    }

                (returns task >=> runFrames task outer) r

    /// A call `task` makes that answers at once, then its return to user mode.
    let private call (task : int) (syscall : UnixSystem<int, string> -> UnixSystem<int, string>) : Step =
        fun r -> returns task (withSystem (syscall r.System) r)

    let private signals (list : Signal list) (r : Replay) : SignalMask =
        SignalMask.ofSignals (numbering r) (Set.ofList list)

    /// `pthread_sigmask(SIG_SETMASK)` by `task`, to block exactly `blocked`.
    let private setMask (task : int) (blocked : Signal list) : Step =
        fun r ->
            let setmask =
                match r.Flavour with
                | SimulatedUnixFlavour.Linux -> 2
                | SimulatedUnixFlavour.Darwin -> 3

            match UnixSignal.pthreadSigmask task setmask (Some (signals blocked r)) r.System with
            | Ok (_, system) -> returns task (withSystem system r)
            | Error errno -> failwith $"pthread_sigmask by %d{task} failed with %O{errno}"

    /// `sigaction` by `task`.
    let private install (task : int) (signal : Signal) (disposition : SignalDisposition<string>) : Step =
        fun r ->
            match UnixSignal.sigaction (signo r signal) (Some disposition) r.System with
            | Ok (_, system) -> returns task (withSystem system r)
            | Error errno -> failwith $"sigaction of %O{signal} failed with %O{errno}"

    let private spawnSecond : Step =
        fun r ->
            match UnixTaskLifecycle.spawn main second (CpuId 0) r.System with
            | Ok (SpawnAnswer.Spawned _, system) -> (returns main >=> returns second) (withSystem system r)
            | other -> failwith $"spawn: %A{other}"

    /// Where a send goes.
    [<RequireQualifiedAccess>]
    type private Route =
        | Process
        | Task of int

    let private routeName (route : Route) : string =
        match route with
        | Route.Process -> "proc"
        | Route.Task task -> taskName task

    /// `kill(getpid())` or `pthread_kill` by `sender`.
    let private send (sender : int) (route : Route) (signal : Signal) : Step =
        fun r ->
            let outcome =
                match route with
                | Route.Process ->
                    let pid = ProcessId.toInt32 (UnixSystem.processId r.System)

                    match UnixSignal.kill pid (signo r signal) r.System with
                    | Error (KillRefusal.Receiver refusal) -> Error (Stop.Receiver refusal)
                    | Error other -> Error (Stop.Other $"kill: %A{other}")
                    | Ok outcome -> Ok outcome
                | Route.Task target ->
                    match UnixSignal.pthreadKill target (signo r signal) r.System with
                    | Error (ThreadKillRefusal.Receiver refusal) -> Error (Stop.Receiver refusal)
                    | Error other -> Error (Stop.Other $"pthread_kill: %A{other}")
                    | Ok outcome -> Ok outcome

            match outcome with
            | Error stop -> Error stop
            | Ok (Ok (KillOutcome.ProcessContinues system)) -> returns sender (withSystem system r)
            | Ok other -> failwith $"sending %O{signal}: %A{other}"

    /// `sigpending` by `task`, printed as the probe prints it.
    let private pending (label : string) (task : int) : Step =
        fun r ->
            let word = SignalMask.toWord (UnixSignal.sigpending task r.System)
            (print $" %s{label}=%x{word}" >=> returns task) r

    /// The probe's `out_runs`.
    let private printRuns : Step =
        fun r ->
            let runs =
                match List.rev r.Runs with
                | [] -> "-"
                | runs ->
                    runs
                    |> List.map (fun (signal, task, phase) -> $"%s{probeName signal}:%s{taskName task}@%d{phase}")
                    |> String.concat ","

            print $" runs=%s{runs}" r

    /// Every other thread wakes from its wait as `actor` announces a stage,
    /// and returns to user mode.
    let private advance (actor : int) : Step =
        fun r ->
            let others =
                r.System.Tasks
                |> Map.keys
                |> Seq.filter (fun task -> task <> actor)
                |> List.ofSeq

            let step = (nothing, others) ||> List.fold (fun step task -> step >=> returns task)
            step r

    // --------------------------------------------------------------- gen -- //

    type private GenRow =
        {
            Signal : Signal
            Disposition : Disposition
            Route : Route
            Sender : int
            BlockMain : bool
            BlockSecond : bool
        }

    let private blocking (signal : Signal) (blocks : bool) : Signal list = if blocks then [ signal ] else []

    let private gen (row : GenRow) : Step =
        let other = if row.Sender = main then second else main

        let late =
            match row.Disposition with
            | Disposition.H -> nothing
            | _ -> install main row.Signal handler

        install main row.Signal (toDisposition row.Disposition)
        >=> setMask main (blocking row.Signal row.BlockMain)
        >=> spawnSecond
        >=> setMask second (blocking row.Signal row.BlockSecond)
        >=> advance second
        >=> send row.Sender row.Route row.Signal
        >=> pending "A" row.Sender
        >=> advance row.Sender
        >=> pending "B" other
        >=> advance other
        >=> phase 1
        >=> late
        >=> phase 2
        >=> setMask main []
        >=> advance main
        >=> phase 3
        >=> setMask second []
        >=> advance second
        >=> printRuns

    let private genTag (row : GenRow) : string =
        let blk =
            match row.BlockMain, row.BlockSecond with
            | false, false -> "none"
            | true, false -> "m"
            | false, true -> "s"
            | true, true -> "both"

        $"gen %s{probeName row.Signal} %s{dispositionName row.Disposition} to=%s{routeName row.Route} from=%s{taskName row.Sender} blk=%s{blk}"

    let private genRows : GenRow list =
        let shapes (signal : Signal) (disposition : Disposition) (routes : Route list) =
            [
                for route in routes do
                    for sender in [ main ; second ] do
                        for blockMain, blockSecond in [ false, false ; true, false ; false, true ; true, true ] do
                            {
                                Signal = signal
                                Disposition = disposition
                                Route = route
                                Sender = sender
                                BlockMain = blockMain
                                BlockSecond = blockSecond
                            }
            ]

        [
            for disposition in [ Disposition.DFL ; Disposition.IGN ; Disposition.H ] do
                yield! shapes Signal.SIGCONT disposition [ Route.Process ; Route.Task main ; Route.Task second ]
            yield! shapes Signal.SIGUSR1 Disposition.IGN [ Route.Process ]
            yield! shapes Signal.SIGURG Disposition.DFL [ Route.Process ]
        ]

    // ------------------------------------------------------------- sleep -- //

    type private SleepRow =
        {
            Disposition : Disposition
            Sleeper : int
            ToProcess : bool
            InSigsuspend : bool
        }

    /// The sleeper's call has answered `answer`: it returns to user mode.
    let private sleeperReturns (sleeper : int) (answer : string) : Step =
        fun r ->
            returns
                sleeper
                { r with
                    Returned = Some (answer, r.Phase)
                }

    /// If the sleeper is woken, its call finishes: it returns, or sleeps on.
    let private wakeIfDue (row : SleepRow) : Step =
        fun r ->
            match UnixTaskTable.parkedFor row.Sleeper r.System.Tasks with
            | None -> Ok r
            | Some _ ->

            match UnixWait.wakes (Set.singleton row.Sleeper) r.System with
            | [] -> Ok r
            | _ ->

            if row.InSigsuspend then
                match UnixSignal.finishSigsuspend row.Sleeper r.System with
                | Error (SigsuspendRefusal.Receiver refusal) -> Error (Stop.Receiver refusal)
                | Error other -> Error (Stop.Other $"finishSigsuspend: %A{other}")
                | Ok (SigsuspendOutcome.Failed UnixError.EINTR, system) ->
                    sleeperReturns row.Sleeper "-1/EINTR" (withSystem system r)
                | Ok (SigsuspendOutcome.WouldBlock _, system) -> Ok (withSystem system r)
                | Ok other -> failwith $"finishSigsuspend: %A{other}"
            else
                match UnixPoll.finishPoll row.Sleeper r.System with
                | Error (PollRefusal.Interruption (SyscallInterruptionRefusal.Receiver refusal)) ->
                    Error (Stop.Receiver refusal)
                | Error other -> Error (Stop.Other $"finishPoll: %A{other}")
                | Ok (PollOutcome.Failed UnixError.EINTR, system) ->
                    sleeperReturns row.Sleeper "-1/EINTR" (withSystem system r)
                | Ok (PollOutcome.Answered ([], 0), system) -> sleeperReturns row.Sleeper "0/0" (withSystem system r)
                | Ok (PollOutcome.WouldBlock _, system) -> Ok (withSystem system r)
                | Ok other -> failwith $"finishPoll: %A{other}"

    /// The sleeper makes its call, which sleeps.
    let private sleeps (row : SleepRow) : Step =
        fun r ->
            if row.InSigsuspend then
                match UnixSignal.sigsuspend row.Sleeper (signals [] r) r.System with
                | Ok (SigsuspendOutcome.WouldBlock _, system) -> Ok (withSystem system r)
                | Error (SigsuspendRefusal.Receiver refusal) -> Error (Stop.Receiver refusal)
                | other -> failwith $"sigsuspend did not sleep: %A{other}"
            else
                match UnixPoll.poll row.Sleeper [] 300 r.System with
                | Ok (PollOutcome.WouldBlock _, system) -> Ok (withSystem system r)
                | other -> failwith $"poll did not sleep: %A{other}"

    /// 300 ms pass, and a poll still asleep times out.
    let private pollTimesOut (row : SleepRow) : Step =
        fun r ->
            if row.InSigsuspend || r.Returned.IsSome then
                Ok r
            else
                let machine = UnixMachineState.advanceClock 300_000_000L r.System.Machine

                wakeIfDue
                    row
                    { r with
                        System =
                            { r.System with
                                Machine = machine
                            }
                    }

    let private sleep (row : SleepRow) : Step =
        let sender = if row.Sleeper = main then second else main

        let route =
            if row.ToProcess then
                Route.Process
            else
                Route.Task row.Sleeper

        let wake = wakeIfDue row

        let late =
            match row.Disposition with
            | Disposition.H -> nothing
            | _ -> install sender Signal.SIGCONT handler

        let ending : Step =
            fun r ->
                if row.InSigsuspend && r.Returned.IsNone then
                    (send sender (Route.Task row.Sleeper) Signal.SIGUSR1 >=> wake) r
                else
                    Ok r

        let report : Step =
            fun r ->
                match r.Returned with
                | None -> failwith "the sleeper never returned"
                | Some (answer, phase) -> print $" ret=%s{answer}@%d{phase}" r

        install main Signal.SIGCONT (toDisposition row.Disposition)
        >=> install main Signal.SIGUSR1 handler
        >=> spawnSecond
        >=> setMask row.Sleeper (if row.InSigsuspend then [ Signal.SIGCONT ] else [])
        >=> advance row.Sleeper
        >=> sleeps row
        >=> setMask sender [ Signal.SIGCONT ]
        >=> phase 1
        >=> send sender route Signal.SIGCONT
        >=> pending "A" sender
        >=> wake
        >=> pending "B" sender
        >=> wake
        >=> late
        >=> phase 2
        >=> wake
        >=> pending "C" sender
        >=> phase 3
        >=> ending
        >=> pollTimesOut row
        >=> report
        >=> printRuns

    let private sleepTag (row : SleepRow) : string =
        let call = if row.InSigsuspend then "sigsuspend" else "poll"
        let route = if row.ToProcess then "proc" else "sleeper"
        $"sleep in=%s{call} %s{dispositionName row.Disposition} sleeper=%s{taskName row.Sleeper} to=%s{route}"

    let private sleepRows : SleepRow list =
        [
            for inSigsuspend in [ false ; true ] do
                for disposition in [ Disposition.DFL ; Disposition.IGN ; Disposition.H ] do
                    for sleeper in [ main ; second ] do
                        for toProcess in [ false ; true ] do
                            {
                                Disposition = disposition
                                Sleeper = sleeper
                                ToProcess = toProcess
                                InSigsuspend = inSigsuspend
                            }
        ]

    // ----------------------------------------------------------- unblock -- //

    let private unblock (disposition : Disposition) (toProcess : bool) : Step =
        install main Signal.SIGCONT (toDisposition disposition)
        >=> setMask main [ Signal.SIGCONT ]
        >=> send main (if toProcess then Route.Process else Route.Task main) Signal.SIGCONT
        >=> pending "A" main
        >=> phase 1
        >=> setMask main []
        >=> setMask main [ Signal.SIGCONT ]
        >=> pending "B" main
        >=> install main Signal.SIGCONT handler
        >=> phase 2
        >=> setMask main []
        >=> printRuns

    // ------------------------------------------------------------ replay -- //

    /// Every row replayed, by its tag.
    let private rows : (string * Step) list =
        [
            for row in genRows do
                genTag row, gen row
            for row in sleepRows do
                sleepTag row, sleep row
            for disposition in [ Disposition.DFL ; Disposition.IGN ] do
                for toProcess in [ false ; true ] do
                    let route = if toProcess then "proc" else "self"
                    $"unblock %s{dispositionName disposition} to=%s{route}", unblock disposition toProcess
        ]

    /// The rows this library refuses rather than answers, by flavour, with
    /// the signal it names.
    ///
    /// Under Linux, a signal sent to the process that the leader blocks is
    /// queued on the process, for another thread that does not block it to
    /// take; this library delivers a process's signals to its leader alone.
    /// Under Darwin the same refusal stands for a caught signal, which Darwin
    /// hands to another thread; an ignored one, and SIGCONT at its default,
    /// it discards instead.
    let private refused : Map<SimulatedUnixFlavour * string, SignalReceiverRefusal> =
        [
            for row in genRows do
                let leaderBlocksOnly =
                    row.Route = Route.Process && row.BlockMain && not row.BlockSecond

                let refusedOn =
                    match row.Disposition with
                    | Disposition.H -> [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]
                    | Disposition.DFL
                    | Disposition.IGN -> [ SimulatedUnixFlavour.Linux ]

                if leaderBlocksOnly then
                    for flavour in refusedOn do
                        (flavour, genTag row), SignalReceiverRefusal.LeaderBlocks row.Signal
            for row in sleepRows do
                // The sender is the leader, and blocks SIGCONT; the sleeper does
                // not block it during its call.
                if row.ToProcess && row.Sleeper = second then
                    let refusedOn =
                        match row.Disposition with
                        | Disposition.H -> [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]
                        | Disposition.DFL
                        | Disposition.IGN -> [ SimulatedUnixFlavour.Linux ]

                    for flavour in refusedOn do
                        (flavour, sleepTag row), SignalReceiverRefusal.LeaderBlocks Signal.SIGCONT
        ]
        |> Map.ofList

    /// The probe's output on `flavour`, embedded from beside the probe.
    let private probeLines (flavour : SimulatedUnixFlavour) : string list =
        let name =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "sigcont-generation.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "sigcont-generation.darwin.txt"

        let assembly = Reflection.Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream $"WoofWare.PosixKernel.Test.%s{name}" with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new IO.StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.TrimEnd '\r')
        |> Array.filter (fun line -> not (line.StartsWith '#'))
        |> List.ofArray

    /// A row's line, as `in_children` prints it: the tag, how many of its
    /// repetitions printed the rest, and the rest.
    let private rowLine : Regex = Regex @"^(?<tag>.*?) x(?<count>\d+)(?<rest> .*)$"

    /// The measured outcome of every replayed row on `flavour`, by tag. Every
    /// repetition of each row printed the same line.
    let private measured (flavour : SimulatedUnixFlavour) : Map<string, string> =
        probeLines flavour
        |> List.filter (fun line -> not (line.StartsWith "stopinfo"))
        |> List.map (fun line ->
            let m = rowLine.Match line

            if not m.Success then
                failwith $"unparsed probe line: %s{line}"

            m.Groups.["count"].Value |> shouldEqual "10"
            m.Groups.["tag"].Value, m.Groups.["rest"].Value
        )
        |> fun pairs ->
            // A tag that printed two different lines would appear twice.
            pairs
            |> List.map fst
            |> List.distinct
            |> List.length
            |> shouldEqual (List.length pairs)

            Map.ofList pairs

    let private replay (flavour : SimulatedUnixFlavour) (step : Step) : Result<string, Stop> =
        let start =
            {
                Flavour = flavour
                System =
                    UnixSystem.initial (HostPlatform.platformOf flavour)
                    |> Launched.boot UnixSystem.pipedStandardStreams main (CpuId 0)
                Phase = 0
                Runs = []
                Printed = []
                Returned = None
            }

        step start
        |> Result.map (fun r ->
            UnixSystem.checkInvariants r.System |> shouldEqual []
            String.concat "" (List.rev r.Printed) + " end=exit0"
        )

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    [<TestCaseSource(nameof flavours)>]
    let ``the probe's rows and the replayed rows are the same rows`` (flavour : SimulatedUnixFlavour) : unit =
        measured flavour
        |> Map.keys
        |> Set.ofSeq
        |> shouldEqual (rows |> List.map fst |> Set.ofList)

    [<TestCaseSource(nameof flavours)>]
    let ``every row replays as measured, or is refused as listed`` (flavour : SimulatedUnixFlavour) : unit =
        let measured = measured flavour

        let failures =
            rows
            |> List.choose (fun (tag, step) ->
                let expected =
                    match Map.tryFind (flavour, tag) refused with
                    | Some refusal -> Error (Stop.Receiver refusal)
                    | None -> Ok (Map.find tag measured)

                let actual = replay flavour step

                if actual = expected then
                    None
                else
                    Some $"%s{tag}\n  measured %A{expected}\n  replayed %A{actual}"
            )

        if not (List.isEmpty failures) then
            failwith (String.concat "\n" failures)

    [<Test>]
    let ``every refused row is a probe row`` () : unit =
        for flavour, tag in Map.keys refused do
            measured flavour |> Map.containsKey tag |> shouldEqual true

    [<Test>]
    let ``a SIGSTOP discards a pending SIGCONT, as Linux's siginfo shows`` () : unit =
        // Linux keeps the first instance's siginfo when a second SIGCONT
        // coalesces into a pending one: without a stop, the child's own
        // instance was delivered; with one, the parent's, so the stop had
        // discarded the child's. Darwin reports a pid of 0 for the child's own
        // instance, so its rows cannot tell.
        let stopinfo (flavour : SimulatedUnixFlavour) : string list =
            probeLines flavour
            |> List.filter (fun line -> line.StartsWith "stopinfo")
            |> List.distinct

        stopinfo SimulatedUnixFlavour.Linux
        |> shouldEqual
            [
                "stopinfo stop=0 stopped=0 before=20000 after=20000 runs=1 si_pid=self end=exit0"
                "stopinfo stop=1 stopped=1 before=20000 after=20000 runs=1 si_pid=parent end=exit0"
            ]

        stopinfo SimulatedUnixFlavour.Darwin
        |> shouldEqual
            [
                "stopinfo stop=0 stopped=0 before=40000 after=40000 runs=1 si_pid=0 end=exit0"
                "stopinfo stop=1 stopped=1 before=40000 after=40000 runs=1 si_pid=parent end=exit0"
            ]

        let flavour = SimulatedUnixFlavour.Linux

        let start =
            {
                Flavour = flavour
                System =
                    UnixSystem.initial (HostPlatform.platformOf flavour)
                    |> Launched.boot UnixSystem.pipedStandardStreams main (CpuId 0)
                Phase = 0
                Runs = []
                Printed = []
                Returned = None
            }

        let stopped =
            (install main Signal.SIGCONT handler
             >=> setMask main [ Signal.SIGCONT ]
             >=> send main Route.Process Signal.SIGCONT)
                start
            |> Result.map (fun r ->
                UnixSignal.sigpending main r.System
                |> shouldEqual (SignalMask.ofSignals (numberingOf flavour) (Set.singleton Signal.SIGCONT))

                let pid = ProcessId.toInt32 (UnixSystem.processId r.System)

                match UnixSignal.kill pid (signo r Signal.SIGSTOP) r.System with
                | Ok (Ok (KillOutcome.ProcessStopped (Signal.SIGSTOP, system))) -> system
                | other -> failwith $"SIGSTOP: %A{other}"
            )

        match stopped with
        | Ok system -> SignalMask.toWord (UnixSignal.sigpending main system) |> shouldEqual 0UL
        | Error stop -> failwith $"%A{stop}"
