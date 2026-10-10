namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A mask call by one task that unblocks a signal pending for another, which
/// on Darwin is any `sigprocmask(2)` (it changes every task's mask).
///
/// Held to `docs/plans/2026-08-23-posix-kernel-extraction/unblock-wakes-sleeper.c`,
/// measured on Darwin 27.0.0 and Linux 6.18.5: on Darwin the other task, asleep
/// in any of the calls this library parks in, slept on until its own condition
/// ended the call, and whether the signal was then delivered depended on the
/// task's history, which this library does not record. So a Darwin
/// `sigprocmask` that would leave a pending signal deliverable to a task other
/// than the caller refuses. On Linux every mask call changes the caller's
/// mask alone, so no other task's sleep changes.
///
/// The search is exhaustive over the call the sleeper is asleep in, the
/// signal's disposition, which task it is pending on (or the process), which
/// tasks sleep and unblock, whether the signal is generated before or during
/// the sleep, and the mask call, on both flavours.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnblockForAnotherTask =

    let private context : string = "TestUnblockForAnotherTask"

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private numberingOf (flavour : SimulatedUnixFlavour) : SignalNumbering =
        SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

    let private signo (flavour : SimulatedUnixFlavour) (signal : Signal) : int =
        Signal.toRawSignoUnder (numberingOf flavour) signal

    /// `SIG_UNBLOCK` and `SIG_SETMASK`, as each `<signal.h>` numbers them.
    let private unblockHow (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 1
        | SimulatedUnixFlavour.Darwin -> 2

    let private setmaskHow (flavour : SimulatedUnixFlavour) : int = unblockHow flavour + 1

    /// The process's tasks: 0 is the leader.
    let private tasks : int list = [ 0 ; 1 ; 2 ]

    /// The task holding the lock a sleeper in `flock` waits for. A lock is
    /// the open file description's, so this task may sleep or unblock too.
    let private holder : int = 2

    /// The calls a task can be asleep in.
    [<RequireQualifiedAccess>]
    type Sleep =
        | PollOfNothing
        | PollListener
        | Accept
        | Flock
        | PipeRead
        | PipeWrite
        /// `epoll_wait` on Linux, `kevent` on Darwin.
        | EventPort
        | Sigsuspend
        | Pause

    let private sleeps : Sleep list =
        [
            Sleep.PollOfNothing
            Sleep.PollListener
            Sleep.Accept
            Sleep.Flock
            Sleep.PipeRead
            Sleep.PipeWrite
            Sleep.EventPort
            Sleep.Sigsuspend
            Sleep.Pause
        ]

    /// The signal, and what the process does with it.
    [<RequireQualifiedAccess>]
    type Disposition =
        /// SIGUSR1, caught, with SA_RESTART or without.
        | Caught of restart : bool
        /// SIGUSR1 under `SIG_IGN`.
        | Ignored
        /// SIGTERM at its default, which terminates.
        | DefaultTerminate
        /// SIGCONT at its default, which continues.
        | DefaultContinue
        /// SIGWINCH at its default, which discards it.
        | DefaultIgnore

    let private dispositions : Disposition list =
        [
            Disposition.Caught false
            Disposition.Caught true
            Disposition.Ignored
            Disposition.DefaultTerminate
            Disposition.DefaultContinue
            Disposition.DefaultIgnore
        ]

    let private signalOf (disposition : Disposition) : Signal =
        match disposition with
        | Disposition.Caught _
        | Disposition.Ignored -> Signal.SIGUSR1
        | Disposition.DefaultTerminate -> Signal.SIGTERM
        | Disposition.DefaultContinue -> Signal.SIGCONT
        | Disposition.DefaultIgnore -> Signal.SIGWINCH

    /// Where the signal is generated.
    [<RequireQualifiedAccess>]
    type Target =
        /// `kill(2)` of the process.
        | Process
        /// `pthread_kill(3)` of this task.
        | Task of int

    let private targets : Target list =
        Target.Process :: (tasks |> List.map Target.Task)

    /// The mask call the unblocker makes.
    [<RequireQualifiedAccess>]
    type Route =
        /// `sigprocmask(SIG_UNBLOCK, {S})`.
        | SigprocmaskUnblock
        /// `sigprocmask(SIG_SETMASK, {})`.
        | SigprocmaskSetEmpty
        /// `pthread_sigmask(SIG_UNBLOCK, {S})`.
        | PthreadSigmaskUnblock

    let private routes : Route list =
        [
            Route.SigprocmaskUnblock
            Route.SigprocmaskSetEmpty
            Route.PthreadSigmaskUnblock
        ]

    type Case =
        {
            Flavour : SimulatedUnixFlavour
            Sleep : Sleep
            Disposition : Disposition
            Target : Target
            Sleeper : int
            Unblocker : int
            GeneratedBeforeSleep : bool
            Route : Route
        }

    /// Every case.
    let private cases : Case list =
        [
            for flavour in flavours do
                for sleep in sleeps do
                    for disposition in dispositions do
                        for target in targets do
                            for sleeper in tasks do
                                for unblocker in tasks do
                                    if unblocker <> sleeper then
                                        for before in [ true ; false ] do
                                            for route in routes do
                                                {
                                                    Flavour = flavour
                                                    Sleep = sleep
                                                    Disposition = disposition
                                                    Target = target
                                                    Sleeper = sleeper
                                                    Unblocker = unblocker
                                                    GeneratedBeforeSleep = before
                                                    Route = route
                                                }
        ]

    // --- the world ---

    type private World =
        {
            System : UnixSystem<int, string>
            Listener : int
            /// The epoll instance on Linux, the kqueue on Darwin.
            Port : int
            WaitingThrough : int
            EmptyPipeReadEnd : int
            FullPipeWriteEnd : int
        }

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// A system with tasks 0 to 2; a blocking listener at loopback port 5000;
    /// an event port (on Linux an epoll instance holding the listener, on
    /// Darwin an empty kqueue); a file opened twice, the first description
    /// locked by `holder`; an empty pipe; and a full one, its write end
    /// blocking.
    let private worldOn (flavour : SimulatedUnixFlavour) : World =
        let system =
            (UnixSystem.initial<int, string> (HostPlatform.platformOf flavour)
             |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0),
             tasks)
            ||> List.fold (fun system name -> Tasks.ensure name system)

        let listener, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match
                CopyIn.bind
                    listener
                    UserBuffer.Mapped
                    16u
                    (CopyIn.inet (UnixSystem.platform system) (loopback 5000us))
                    system
            with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"binding the listener: %A{other}"

        let system =
            match UnixSocket.listen listener 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listening: %A{other}"

        let port, system =
            match flavour with
            | SimulatedUnixFlavour.Darwin ->
                match UnixKqueue.kqueue system with
                | Ok (fd, system) -> fd, system
                | other -> failwith $"kqueue: %A{other}"
            | SimulatedUnixFlavour.Linux ->
                let epoll, system =
                    match UnixPoll.epollCreate1 0 system with
                    | Ok (Ok (fd, system)) -> fd, system
                    | other -> failwith $"epoll_create1: %A{other}"

                match
                    UnixPoll.epollCtl
                        epoll
                        1
                        listener
                        (EpollEventArgument.Readable (EpollEvents.In ||| EpollEvents.EdgeTriggered, 7UL))
                        system
                with
                | Ok (EpollCtlAnswer.Changed, system) -> epoll, system
                | other -> failwith $"epoll_ctl: %A{other}"

        let inode, filesystem =
            match
                VirtualFileSystem.createFile
                    (InodeNumber 1L)
                    (DirectoryEntryName.parseOrFail context "f")
                    (PermissionBits.parseOrFail context 0o644)
                    (InodeOwner.ofProcess system.Process.Credentials)
                    (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                    ImmutableArray.Empty
                    system.Machine.FileSystem
            with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed the file: %O{error}"

        let lockedThrough, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite (UnixSystemState.fileDescriptors system)

        let waitingThrough, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite registry

        let system =
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
            |> UnixSystemState.withFileDescriptors registry

        let system =
            match UnixDescriptor.flock holder lockedThrough 2 system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), system) -> system
            | other -> failwith $"the holder's lock: %A{other}"

        let pipe (system : UnixSystem<int, string>) : int * int * UnixSystem<int, string> =
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readEnd, writeEnd), system) -> readEnd, writeEnd, system
            | other -> failwith $"pipe2: %A{other}"

        let emptyRead, _, system = pipe system
        let _, fullWrite, system = pipe system

        // Filled by non-blocking writes, a page and then a byte at a time,
        // until nothing more fits.
        let _, system = UnixDescriptor.setNonBlocking fullWrite true system

        let rec fill (chunk : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
            match
                WriteOutcomes.admitThenWrite
                    system.Leader
                    fullWrite
                    UserBuffer.Mapped
                    (ImmutableArray.CreateRange (Array.create chunk 0x61uy))
                    system
            with
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed _, system)) -> fill chunk system
            | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system)) ->
                if chunk = 1 then system else fill 1 system
            | other -> failwith $"filling the pipe: %A{other}"

        let system = fill 4096 system
        let _, system = UnixDescriptor.setNonBlocking fullWrite false system

        {
            System = system
            Listener = listener
            Port = port
            WaitingThrough = waitingThrough
            EmptyPipeReadEnd = emptyRead
            FullPipeWriteEnd = fullWrite
        }

    let private worlds : Map<SimulatedUnixFlavour, World> =
        flavours |> List.map (fun flavour -> flavour, worldOn flavour) |> Map.ofList

    /// `world` with `task` asleep in `sleep`, or `None` if the call answered
    /// at once.
    let private asleep (world : World) (task : int) (sleep : Sleep) (system : UnixSystem<int, string>) =
        let parkedOrNot (what : string) (result : Result<'outcome * UnixSystem<int, string>, 'refusal>) =
            match result with
            | Ok (_, system) when (UnixTaskTable.parkOf task system.Tasks).IsSome -> Some system
            | Ok _ -> None
            | Error refusal -> failwith $"%s{what} was refused: %A{refusal}"

        match sleep with
        | Sleep.PollOfNothing ->
            UnixPoll.poll
                task
                [
                    {
                        Fd = -1
                        Events = 0x0001s
                    }
                ]
                -1
                system
            |> parkedOrNot "the poll of nothing"
        | Sleep.PollListener ->
            UnixPoll.poll
                task
                [
                    {
                        Fd = world.Listener
                        Events = 0x0001s
                    }
                ]
                -1
                system
            |> parkedOrNot "the poll"
        | Sleep.Accept ->
            UnixConnection.accept task world.Listener UserBuffer.Mapped 16u system
            |> parkedOrNot "the accept"
        | Sleep.Flock ->
            UnixDescriptor.flock task world.WaitingThrough 2 system
            |> parkedOrNot "the flock"
        | Sleep.PipeRead ->
            UnixReadWrite.read task world.EmptyPipeReadEnd UserBuffer.Mapped 1UL system
            |> parkedOrNot "the read"
        | Sleep.PipeWrite ->
            match
                WriteOutcomes.admitThenWrite
                    task
                    world.FullPipeWriteEnd
                    UserBuffer.Mapped
                    (ImmutableArray.Create 0x62uy)
                    system
            with
            | Ok (WriteOutcome.WouldBlock (_, system)) -> Some system
            | Ok _ -> None
            | Error refusal -> failwith $"the write was refused: %A{refusal}"
        | Sleep.EventPort ->
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux ->
                UnixPoll.epollWait task world.Port 8 UserBuffer.Mapped -1 system
                |> parkedOrNot "the epoll_wait"
            | SimulatedUnixFlavour.Darwin ->
                UnixKqueue.kevent task world.Port 0 [] 1 UserBuffer.Mapped KeventTimeout.Null system
                |> parkedOrNot "the kevent"
        | Sleep.Sigsuspend ->
            let mask = SignalState.maskOf task system.Process.Signals
            UnixSignal.sigsuspend task mask system |> parkedOrNot "the sigsuspend"
        | Sleep.Pause -> UnixSignal.pause task system |> parkedOrNot "the pause"

    let private orFail (what : string) (result : Result<'a * UnixSystem<int, string>, 'e>) : UnixSystem<int, string> =
        match result with
        | Ok (_, system) -> system
        | Error e -> failwith $"%s{what}: %A{e}"

    /// `system` with the case's signal generated, failing the test if the
    /// generation did anything but leave the process running.
    let private generate (case : Case) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let raw = signo case.Flavour (signalOf case.Disposition)

        let outcome =
            match case.Target with
            | Target.Process ->
                UnixSignal.kill (ProcessId.toInt32 (UnixSystem.processId system)) raw system
                |> Result.mapError (fun refusal -> $"%A{refusal}")
            | Target.Task task ->
                UnixSignal.pthreadKill task raw system
                |> Result.mapError (fun refusal -> $"%A{refusal}")

        match outcome with
        | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
        | other -> failwith $"%A{case}: generating the signal answered %A{other}"

    /// What the unblocker's call answered.
    [<RequireQualifiedAccess>]
    type private Unblocked =
        | Answered of UnixSystem<int, string>
        | Refused of SigprocmaskRefusal<int>

    let private unblock (case : Case) (system : UnixSystem<int, string>) : Unblocked =
        let s =
            SignalMask.ofSignals (numberingOf case.Flavour) (Set.singleton (signalOf case.Disposition))

        let answered (result : Result<SignalMask * UnixSystem<int, string>, UnixError>) : Unblocked =
            match result with
            | Ok (_, system) -> Unblocked.Answered system
            | Error errno -> failwith $"%A{case}: the mask call failed with %O{errno}"

        match case.Route with
        | Route.PthreadSigmaskUnblock ->
            UnixSignal.pthreadSigmask case.Unblocker (unblockHow case.Flavour) (Some s) system
            |> answered
        | Route.SigprocmaskUnblock
        | Route.SigprocmaskSetEmpty ->
            let how, set =
                match case.Route with
                | Route.SigprocmaskUnblock -> unblockHow case.Flavour, s
                | _ -> setmaskHow case.Flavour, SignalMask.empty

            match UnixSignal.sigprocmask case.Unblocker how (Some set) system with
            | Ok answer -> answered answer
            | Error refusal -> Unblocked.Refused refusal

    /// The system just before the unblocker's call: every task blocks the
    /// signal, its disposition is installed, the sleeper is asleep and the
    /// signal has been generated, in the case's order. `None` where the
    /// sleeper's call answered at once.
    let private before (case : Case) : UnixSystem<int, string> option =
        let world = Map.find case.Flavour worlds
        let numbering = numberingOf case.Flavour
        let signal = signalOf case.Disposition
        let s = SignalMask.ofSignals numbering (Set.singleton signal)

        let disposition : SignalDisposition<string> option =
            match case.Disposition with
            | Disposition.Caught restart ->
                Some (
                    SignalDisposition.Catch
                        { SignalCatch.ofHandler "h" with
                            Restart = restart
                        }
                )
            | Disposition.Ignored -> Some SignalDisposition.Ignore
            | Disposition.DefaultTerminate
            | Disposition.DefaultContinue
            | Disposition.DefaultIgnore -> None

        let system =
            match disposition with
            | None -> world.System
            | Some disposition ->
                UnixSignal.sigaction (signo case.Flavour signal) (Some disposition) world.System
                |> orFail "sigaction"

        let system =
            (system, tasks)
            ||> List.fold (fun system task ->
                UnixSignal.pthreadSigmask task (setmaskHow case.Flavour) (Some s) system
                |> orFail "pthread_sigmask"
            )

        if case.GeneratedBeforeSleep then
            generate case system |> asleep world case.Sleeper case.Sleep
        else
            asleep world case.Sleeper case.Sleep system |> Option.map (generate case)

    /// Whether the signal is pending once generated with every task blocking
    /// it: on Linux always (a blocked ignored signal stays pending,
    /// `sigpending-scope.c`); on Darwin unless it is discarded at generation,
    /// which an ignored one is, SIGCONT apart (the same probe's "ignored"
    /// rows).
    let private pendsWhileBlocked (case : Case) : bool =
        match case.Flavour, case.Disposition with
        | SimulatedUnixFlavour.Linux, _ -> true
        | SimulatedUnixFlavour.Darwin, Disposition.Ignored
        | SimulatedUnixFlavour.Darwin, Disposition.DefaultIgnore -> false
        | SimulatedUnixFlavour.Darwin, Disposition.Caught _
        | SimulatedUnixFlavour.Darwin, Disposition.DefaultTerminate
        | SimulatedUnixFlavour.Darwin, Disposition.DefaultContinue -> true

    /// The task other than the unblocker for which the call would leave the
    /// signal pending and no longer blocked, if any: Darwin's `sigprocmask`
    /// unblocks it for every task, and holds a signal sent to the process
    /// on the main thread (`sigpending-scope.c`, the "proc" rows).
    let private leftDeliverableTo (case : Case) : int option =
        match case.Flavour, case.Route with
        | SimulatedUnixFlavour.Linux, _
        | SimulatedUnixFlavour.Darwin, Route.PthreadSigmaskUnblock -> None
        | SimulatedUnixFlavour.Darwin, Route.SigprocmaskUnblock
        | SimulatedUnixFlavour.Darwin, Route.SigprocmaskSetEmpty ->
            if not (pendsWhileBlocked case) then
                None
            else
                let holder =
                    match case.Target with
                    | Target.Process -> 0
                    | Target.Task task -> task

                if holder = case.Unblocker then None else Some holder

    /// What the sleeper would take, if `after` wakes it into an answer rather
    /// than into a refusal or a sleep resumed.
    let private wokenInto (case : Case) (after : UnixSystem<int, string>) : string option =
        match UnixWait.wakes (Set.singleton case.Sleeper) after with
        | [] -> None
        | _ ->
            let tasks = after.Tasks |> Map.keys |> Set.ofSeq

            let suspended =
                match case.Sleep with
                | Sleep.Sigsuspend
                | Sleep.Pause -> true
                | _ -> false

            // A finishing call ends with what the sleeper would take as it
            // returned to user mode: handlers, which end every call here; a
            // death, which ends a sigsuspend and which every other call
            // refuses; and anything else refused or slept on.
            match
                SignalState.onReturnToUser after.Process.CoreDumps after.Leader tasks case.Sleeper after.Process.Signals
            with
            | Ok (Some (SignalDelivery.RunHandlers frames), _) ->
                Some $"handlers for %A{frames |> List.map (fun frame -> frame.Entry.Signal)}"
            | Ok (Some (SignalDelivery.DefaultTerminate (signal, _)), _) when suspended -> Some $"a death by %O{signal}"
            | Ok _
            | Error _ -> None

    /// What is wrong with the library's answer to `case`, if anything.
    let private check (case : Case) : string option =
        match before case with
        | None -> None
        | Some system ->

        // The perturbation reaches the sleeper only through the unblock: before
        // it, nothing wakes the sleeper.
        if not (List.isEmpty (UnixWait.wakes (Set.singleton case.Sleeper) system)) then
            Some "the sleeper was woken before the unblock"
        else

        match unblock case system, leftDeliverableTo case with
        | Unblocked.Refused refusal, Some task ->
            if refusal = SigprocmaskRefusal.DarwinUnblockedForAnotherTask (task, signalOf case.Disposition) then
                None
            else
                Some $"refused with %A{refusal}"
        | Unblocked.Refused refusal, None -> Some $"refused with %A{refusal} where Darwin leaves no task deliverable"
        | Unblocked.Answered after, Some task ->
            let woken =
                match wokenInto case after with
                | Some taken -> $", and woke the sleeper into %s{taken}"
                | None -> ""

            Some $"answered, leaving the signal deliverable to task %d{task}%s{woken}"
        | Unblocked.Answered after, None ->
            // Darwin's sleeper slept on, and Linux's mask call does not touch
            // the sleeper's mask. A wake is right only into a refusal.
            wokenInto case after
            |> Option.map (fun taken -> $"woke the sleeper into %s{taken}")

    [<Test>]
    let ``a mask call never leaves a signal deliverable to another task, nor wakes one into an answer`` () : unit =
        let failures =
            cases
            |> List.choose (fun case -> check case |> Option.map (fun failure -> case, failure))

        if not (List.isEmpty failures) then
            let shown =
                failures
                |> List.truncate 40
                |> List.map (fun (case, failure) -> $"%A{case}: %s{failure}")
                |> String.concat "\n"

            Assert.Fail $"%d{List.length failures} of %d{List.length cases} cases failed; the first:\n%s{shown}"

    [<Test>]
    let ``the search puts a task to sleep in every call on both flavours, and reaches both answers on Darwin``
        ()
        : unit
        =
        // Guards against a vacuous search: every call parks on each flavour,
        // and on Darwin each has cases the unblock must refuse for the sleeper
        // itself, beside cases it must answer.
        let parked = cases |> List.filter (fun case -> (before case).IsSome)

        for flavour in flavours do
            for sleep in sleeps do
                let these =
                    parked |> List.filter (fun case -> case.Flavour = flavour && case.Sleep = sleep)

                (flavour, sleep, List.isEmpty these) |> shouldEqual (flavour, sleep, false)

                match flavour with
                | SimulatedUnixFlavour.Linux -> ()
                | SimulatedUnixFlavour.Darwin ->
                    (sleep, these |> List.exists (fun case -> leftDeliverableTo case = Some case.Sleeper))
                    |> shouldEqual (sleep, true)

                    (sleep, these |> List.exists (fun case -> (leftDeliverableTo case).IsNone))
                    |> shouldEqual (sleep, true)
