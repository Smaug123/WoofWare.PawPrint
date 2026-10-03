namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A signal with a handler, delivered to a task asleep in a syscall: whether it
/// wakes the task, how the syscall's finishing call then answers, and what is
/// left for the task's return to user mode.
///
/// The oracle is a table stated here, from the measurements rather than from
/// `SyscallInterruption.ruleOf`: `flock` and `accept` restart under SA_RESTART
/// and fail with EINTR without it; `poll` and `epoll_wait` fail with EINTR
/// either way (`signal-sigaction-flags.c`, and `signal-interrupt-requeue.c`
/// section A for a poll that watches nothing). The call's own answer beats the
/// signal on Linux (section D), `epoll_wait`'s deadline beats the signal and the
/// signal beats `poll`'s (section E); Darwin answers whichever reached the
/// sleeper first (section F), which this library refuses.
///
/// Every world is built through the syscalls themselves: a listener the
/// `accept`, `poll` and `epoll_wait` sleep on, made ready by a `connect`; and a
/// file two descriptions lock, the sleeper's waiting for another task's lock
/// to be released.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSyscallInterruption =

    let private context : string = "TestSyscallInterruption"
    let private nanosecondsPerMillisecond : int64 = 1_000_000L

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// The task that holds the lock the sleeper waits for. Never the sleeper.
    let private holder : int = 3

    /// The syscalls a task can be asleep in, with what each was asked.
    [<RequireQualifiedAccess>]
    type Sleep =
        | Flock
        | Accept
        /// `poll` of the listener, for ever or for this many milliseconds.
        | Poll of milliseconds : int
        /// `poll` of nothing but an ignored entry, for ever.
        | PollOfNothing
        /// `epoll_wait` on a port holding the listener, for ever or for this
        /// many milliseconds.
        | EpollWait of milliseconds : int

    /// How the call ended, whichever call it was.
    [<RequireQualifiedAccess>]
    type Ending =
        | Completed
        | TimedOut
        | Eintr
        | Restart
        | Reparked
        | Refused of SyscallInterruptionRefusal

    /// What restarting would take, as the measurements say: `true` for a call
    /// that SA_RESTART restarts.
    let private restartable (sleep : Sleep) : bool =
        match sleep with
        | Sleep.Flock
        | Sleep.Accept -> true
        | Sleep.Poll _
        | Sleep.PollOfNothing
        | Sleep.EpollWait _ -> false

    let private timeoutOf (sleep : Sleep) : int option =
        match sleep with
        | Sleep.Poll ms
        | Sleep.EpollWait ms when ms > 0 -> Some ms
        | Sleep.Flock
        | Sleep.Accept
        | Sleep.Poll _
        | Sleep.PollOfNothing
        | Sleep.EpollWait _ -> None

    type private World =
        {
            System : UnixSystem<int, string>
            Listener : int
            Port : int
            LockedThrough : int
            WaitingThrough : int
        }

    let private withRegistry
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// A system on `platform` with tasks 1 to 3, a blocking listener at
    /// loopback port 5000, on Linux an epoll port holding it edge-triggered,
    /// and a file opened twice, the first description locked exclusively by
    /// `holder`.
    let private worldOn (platform : SimulatedUnixPlatform) : World =
        let system =
            (UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
             |> UnixBootImage.boot,
             [ 1..3 ])
            ||> List.fold (fun system name -> Tasks.ensure name system)

        let listener, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match UnixSocket.bind listener UserBuffer.Mapped 16u inetFamily (Some (loopback 5000us)) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"binding the listener: %A{other}"

        let system =
            match UnixSocket.listen listener 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listening: %A{other}"

        let port, system =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> -1, system
            | SimulatedUnixFlavour.Linux ->

            let port, system =
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok (fd, system)) -> fd, system
                | other -> failwith $"expected a port, got %A{other}"

            match
                UnixPoll.epollCtl
                    port
                    1
                    listener
                    (EpollEventArgument.Readable (EpollEvents.In ||| EpollEvents.EdgeTriggered, 7UL))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> port, system
            | other -> failwith $"expected the registration to succeed, got %A{other}"

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
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        let waitingThrough, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite registry

        let system =
            { withRegistry registry system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }

        let system =
            match UnixDescriptor.flock holder lockedThrough 2 system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), system) -> system
            | other -> failwith $"expected the holder's lock, got %A{other}"

        {
            System = system
            Listener = listener
            Port = port
            LockedThrough = lockedThrough
            WaitingThrough = waitingThrough
        }

    let private linuxWorld : World = worldOn SimulatedUnixPlatform.linuxX64
    let private darwinWorld : World = worldOn SimulatedUnixPlatform.macOsArm64

    /// `world` with `task` asleep in `sleep`.
    let private asleep (world : World) (task : int) (sleep : Sleep) : UnixSystem<int, string> =
        let parked
            (what : string)
            (result : Result<'outcome * UnixSystem<int, string>, 'refusal>)
            (isPark : 'outcome -> bool)
            =
            match result with
            | Ok (outcome, system) when isPark outcome -> system
            | other -> failwith $"expected %s{what} to sleep, got %A{other}"

        match sleep with
        | Sleep.Flock ->
            parked
                "the flock"
                (UnixDescriptor.flock task world.WaitingThrough 2 world.System)
                (function
                | SyscallOutcome.WouldBlock _ -> true
                | _ -> false
                )
        | Sleep.Accept ->
            parked
                "the accept"
                (UnixConnection.accept task world.Listener UserBuffer.Mapped 16u world.System)
                (function
                | AcceptOutcome.WouldBlock _ -> true
                | _ -> false
                )
        | Sleep.Poll milliseconds ->
            parked
                "the poll"
                (UnixPoll.poll
                    task
                    [
                        {
                            Fd = world.Listener
                            Events = 0x0001s
                        }
                    ]
                    milliseconds
                    world.System)
                (function
                | PollOutcome.WouldBlock _ -> true
                | _ -> false
                )
        | Sleep.PollOfNothing ->
            parked
                "the poll of nothing"
                (UnixPoll.poll
                    task
                    [
                        {
                            Fd = -1
                            Events = 0x0001s
                        }
                    ]
                    -1
                    world.System)
                (function
                | PollOutcome.WouldBlock _ -> true
                | _ -> false
                )
        | Sleep.EpollWait milliseconds ->
            parked
                "the epoll_wait"
                (UnixPoll.epollWait task world.Port 8 UserBuffer.Mapped milliseconds world.System)
                (function
                | EpollWaitOutcome.WouldBlock _ -> true
                | _ -> false
                )

    /// What the sleeper waits for comes about: a connection is queued on the
    /// listener, or the holder releases its lock.
    let private makeReady (world : World) (sleep : Sleep) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match sleep with
        | Sleep.Flock ->
            match UnixDescriptor.flock holder world.LockedThrough 8 system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), system) -> system
            | other -> failwith $"expected the holder's release, got %A{other}"
        | Sleep.PollOfNothing -> failwith "a poll of nothing is never ready"
        | Sleep.Accept
        | Sleep.Poll _
        | Sleep.EpollWait _ ->
            let client, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            match UnixConnection.connect client UserBuffer.Mapped 16u inetFamily (Some (loopback 5000us)) system with
            | Ok (ConnectOutcome.Completed, system) -> system
            | other -> failwith $"connecting: %A{other}"

    /// `task`'s sleeping call, finished.
    let private finish
        (task : int)
        (sleep : Sleep)
        (system : UnixSystem<int, string>)
        : Ending * UnixSystem<int, string>
        =
        let unexpected (what : obj) : Ending * UnixSystem<int, string> =
            failwith $"%O{sleep}: unexpected finish %A{what}"

        match sleep with
        | Sleep.Flock ->
            match UnixDescriptor.flockAcquire task system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), system) -> Ending.Completed, system
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EINTR), system) -> Ending.Eintr, system
            | Ok (SyscallOutcome.Restarts, system) -> Ending.Restart, system
            | Ok (SyscallOutcome.WouldBlock _, system) -> Ending.Reparked, system
            | Error (FLockRefusal.Interruption refusal) -> Ending.Refused refusal, system
            | other -> unexpected other
        | Sleep.Accept ->
            match UnixConnection.finishAccept task system with
            | Ok (AcceptOutcome.Accepted _, system) -> Ending.Completed, system
            | Ok (AcceptOutcome.Failed UnixError.EINTR, system) -> Ending.Eintr, system
            | Ok (AcceptOutcome.Restarts, system) -> Ending.Restart, system
            | Ok (AcceptOutcome.WouldBlock _, system) -> Ending.Reparked, system
            | Error (AcceptRefusal.Interruption refusal) -> Ending.Refused refusal, system
            | other -> unexpected other
        | Sleep.Poll _
        | Sleep.PollOfNothing ->
            match UnixPoll.finishPoll task system with
            | Ok (PollOutcome.Answered (_, count), system) when count > 0 -> Ending.Completed, system
            | Ok (PollOutcome.Answered (_, 0), system) -> Ending.TimedOut, system
            | Ok (PollOutcome.Failed UnixError.EINTR, system) -> Ending.Eintr, system
            | Ok (PollOutcome.WouldBlock _, system) -> Ending.Reparked, system
            | Error (PollRefusal.Interruption refusal) -> Ending.Refused refusal, system
            | other -> unexpected other
        | Sleep.EpollWait _ ->
            match UnixPoll.finishEpollWait task system with
            | Ok (EpollWaitOutcome.Answered (_ :: _), system) -> Ending.Completed, system
            | Ok (EpollWaitOutcome.Answered [], system) -> Ending.TimedOut, system
            | Ok (EpollWaitOutcome.Failed UnixError.EINTR, system) -> Ending.Eintr, system
            | Ok (EpollWaitOutcome.WouldBlock _, system) -> Ending.Reparked, system
            | Error (EpollWaitRefusal.Interruption refusal) -> Ending.Refused refusal, system
            | other -> unexpected other

    /// A signal sent to a sleeper, and its disposition.
    [<RequireQualifiedAccess>]
    type Sent =
        /// Caught, its handler installed with SA_RESTART or without.
        | Caught of signal : Signal * restart : bool
        /// `SIG_IGN`.
        | Ignored of signal : Signal

    let private signalOf (sent : Sent) : Signal =
        match sent with
        | Sent.Caught (signal, _)
        | Sent.Ignored signal -> signal

    /// `sent`, installed and then generated for `task` alone: the system's
    /// signals, and how many of them are now pending.
    let private send (task : int) (sent : Sent list) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let signals =
            (system.Process.Signals, sent)
            ||> List.fold (fun signals sent ->
                let disposition =
                    match sent with
                    | Sent.Caught (_, restart) ->
                        SignalDisposition.Catch
                            { SignalCatch.ofHandler "h" with
                                Restart = restart
                            }
                    | Sent.Ignored _ -> SignalDisposition.Ignore

                signals
                |> SignalState.setDisposition (signalOf sent) disposition
                |> SignalState.enqueue
                    {
                        Signal = signalOf sent
                        Target = ValueSome task
                    }
            )

        { system with
            Process =
                { system.Process with
                    Signals = signals
                }
        }

    /// How the call should end, from the measured table alone.
    let private oracle
        (flavour : SimulatedUnixFlavour)
        (sleep : Sleep)
        (sent : Sent list)
        (ready : bool)
        (pastDeadline : bool)
        : Ending
        =
        // Each signal is sent once, so a caught one is a handler that runs.
        let restarts =
            sent
            |> List.choose (fun sent ->
                match sent with
                | Sent.Caught (_, restart) -> Some restart
                | Sent.Ignored _ -> None
            )
            |> List.distinct

        let signalled = not (List.isEmpty restarts)

        let interrupted () : Ending =
            if not (restartable sleep) then
                Ending.Eintr
            else
                match restarts with
                | [ true ] -> Ending.Restart
                | [ false ] -> Ending.Eintr
                | _ ->
                    let signalsWith (restart : bool) =
                        sent
                        |> List.choose (fun sent ->
                            match sent with
                            | Sent.Caught (signal, r) when r = restart -> Some signal
                            | _ -> None
                        )

                    Ending.Refused (SyscallInterruptionRefusal.MixedRestartFlags (signalsWith true, signalsWith false))

        match flavour with
        | SimulatedUnixFlavour.Darwin when ready && signalled ->
            Ending.Refused (SyscallInterruptionRefusal.SignalBesideCompletion SimulatedUnixFlavour.Darwin)
        | _ when ready -> Ending.Completed
        | _ ->

        match sleep with
        | Sleep.EpollWait _ when pastDeadline -> Ending.TimedOut
        | _ when signalled -> interrupted ()
        | _ when pastDeadline -> Ending.TimedOut
        | _ -> Ending.Reparked

    /// The descriptor `sleep` was entered through, where a close of it is one
    /// Linux lets the call sleep on (`open-file-references.c`): the only
    /// descriptor onto the description the sleeping call holds. A poll's is
    /// not, since closing a descriptor a poll watches is refused.
    let private enteredThrough (world : World) (sleep : Sleep) : int option =
        match sleep with
        | Sleep.Flock -> Some world.WaitingThrough
        | Sleep.Accept -> Some world.Listener
        | Sleep.EpollWait _ -> Some world.Port
        | Sleep.Poll _
        | Sleep.PollOfNothing -> None

    let private sleepsOn (flavour : SimulatedUnixFlavour) : Sleep list =
        match flavour with
        | SimulatedUnixFlavour.Darwin -> [ Sleep.Flock ; Sleep.Accept ]
        | SimulatedUnixFlavour.Linux ->
            [
                Sleep.Flock
                Sleep.Accept
                Sleep.Poll -1
                Sleep.Poll 5
                Sleep.PollOfNothing
                Sleep.EpollWait -1
                Sleep.EpollWait 5
            ]

    /// Every way of sending SIGUSR1 and SIGUSR2, which catch, ignore and
    /// default alike on both flavours, and which the leader blocks in no
    /// handler frame here.
    let private everySent : Sent list list =
        let one (signal : Signal) : Sent option list =
            [
                None
                Some (Sent.Caught (signal, true))
                Some (Sent.Caught (signal, false))
                Some (Sent.Ignored signal)
            ]

        [
            for usr1 in one Signal.SIGUSR1 do
                for usr2 in one Signal.SIGUSR2 do
                    List.choose id [ usr1 ; usr2 ]
        ]

    [<Test>]
    let ``a sleeping call ends as the measured table says, and only a woken sleeper ends`` () : unit =
        let mutable seen : Set<string> = Set.empty

        // Whether the call's own answer is ready; a poll of nothing has none.
        let readies (sleep : Sleep) : bool list =
            if sleep = Sleep.PollOfNothing then
                [ false ]
            else
                [ false ; true ]

        let pastDeadlines (sleep : Sleep) : bool list =
            if timeoutOf sleep = None then
                [ false ]
            else
                [ false ; true ]

        // On Linux, the last descriptor onto what the call sleeps on is closed
        // under it, which changes nothing about how it ends.
        let closedUnders (flavour : SimulatedUnixFlavour) (sleep : Sleep) : bool list =
            match flavour, enteredThrough linuxWorld sleep with
            | SimulatedUnixFlavour.Linux, Some _ -> [ false ; true ]
            | _ -> [ false ]

        // The inputs are finite, so every one of them is run, and whether
        // every ending is reached does not depend on which were drawn.
        let cases =
            [
                for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
                    for sleep in sleepsOn flavour do
                        // The leader as well as other tasks: a thread-directed
                        // signal reaches either.
                        for task in 0..2 do
                            for sent in everySent do
                                for ready in readies sleep do
                                    for pastDeadline in pastDeadlines sleep do
                                        for readyFirst in [ false ; true ] do
                                            for closedUnder in closedUnders flavour sleep do
                                                flavour, sleep, task, sent, ready, pastDeadline, readyFirst, closedUnder
            ]

        let property (flavour, sleep, task, sent, ready, pastDeadline, readyFirst, closedUnder) =
            let world =
                match flavour with
                | SimulatedUnixFlavour.Linux -> linuxWorld
                | SimulatedUnixFlavour.Darwin -> darwinWorld

            let system = asleep world task sleep

            let held =
                enteredThrough world sleep
                |> Option.map (fun fd ->
                    match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                    | Some id -> id
                    | None -> failwith $"fd %d{fd} names no description"
                )

            let system =
                if closedUnder then
                    match UnixDescriptor.close (Option.get (enteredThrough world sleep)) system with
                    | Ok (SyscallAnswer.Completed 0L, system) -> system
                    | other -> failwith $"%O{sleep}: expected the close to succeed, got %A{other}"
                else
                    system

            let signalled = send task sent
            let readied = if ready then makeReady world sleep else id

            let system =
                if readyFirst then
                    system |> readied |> signalled
                else
                    system |> signalled |> readied

            let system =
                match timeoutOf sleep with
                | Some ms when pastDeadline ->
                    { system with
                        Machine = UnixMachineState.advanceClock (int64 ms * nanosecondsPerMillisecond) system.Machine
                    }
                | _ -> system

            let expected = oracle flavour sleep sent ready pastDeadline

            // A sleeper wakes exactly when its finishing call has something to
            // say other than "sleep again".
            let woken = UnixWait.wakes (Set.singleton task) system |> List.map fst
            woken |> shouldEqual (if expected = Ending.Reparked then [] else [ task ])

            let ending, after = finish task sleep system
            ending |> shouldEqual expected

            seen <- Set.add $"%A{ending}" seen

            // The description the call held outlives it only if a descriptor
            // still names it, or the call sleeps on.
            match held, ending with
            | _, Ending.Refused _
            | None, _ -> ()
            | Some held, ending ->
                let survives = not closedUnder || ending = Ending.Reparked

                if closedUnder then
                    seen <- Set.add $"closed under, then %A{ending}" seen

                FileDescriptorRegistry.descriptions after.Process.FileDescriptors
                |> Map.containsKey held
                |> shouldEqual survives

                UnixSystem.checkInvariants after |> shouldEqual []

            match ending with
            | Ending.Reparked
            | Ending.Refused _ -> ()
            | Ending.Completed
            | Ending.TimedOut
            | Ending.Eintr
            | Ending.Restart ->
                UnixTaskTable.parkedFor task after.Tasks |> shouldEqual None

                // The signals are still pending, for the return to user mode
                // that follows: every caught one gets a frame there.
                let caught =
                    sent
                    |> List.choose (fun sent ->
                        match sent with
                        | Sent.Caught (signal, _) -> Some signal
                        | Sent.Ignored _ -> None
                    )
                    |> Set.ofList

                match UnixSignal.onReturnToUser task after with
                | Ok (None, _) -> caught |> shouldEqual Set.empty
                | Ok (Some (SignalDelivery.RunHandlers frames), _) ->
                    frames
                    |> List.map (fun frame -> frame.Entry.Signal)
                    |> Set.ofList
                    |> shouldEqual caught
                | other -> failwith $"unexpected return to user mode: %A{other}"

        for case in cases do
            try
                property case
            with e ->
                raise (System.Exception ($"failed on %A{case}", e))

        // Every ending was reached.
        seen
        |> shouldEqual (
            set
                [
                    "Completed"
                    "TimedOut"
                    "Eintr"
                    "Restart"
                    "Reparked"
                    "closed under, then Completed"
                    "closed under, then TimedOut"
                    "closed under, then Eintr"
                    "closed under, then Restart"
                    "closed under, then Reparked"
                    $"%A{Ending.Refused (SyscallInterruptionRefusal.SignalBesideCompletion SimulatedUnixFlavour.Darwin)}"
                    $"%A{Ending.Refused (SyscallInterruptionRefusal.MixedRestartFlags ([ Signal.SIGUSR1 ], [ Signal.SIGUSR2 ]))}"
                    $"%A{Ending.Refused (SyscallInterruptionRefusal.MixedRestartFlags ([ Signal.SIGUSR2 ], [ Signal.SIGUSR1 ]))}"
                ]
        )

    [<Test>]
    let ``the rule each sleeping call follows is the measured one`` () : unit =
        let parked (sleep : Sleep) : ParkedSyscall =
            match UnixTaskTable.parkedFor 1 (asleep linuxWorld 1 sleep).Tasks with
            | Some parked -> parked
            | None -> failwith $"%O{sleep} did not park"

        for sleep in sleepsOn SimulatedUnixFlavour.Linux do
            SyscallInterruption.ruleOf (parked sleep)
            |> shouldEqual (
                if restartable sleep then
                    SignalRestartRule.RestartsUnderSaRestart
                else
                    SignalRestartRule.FailsWithEintr
            )

    [<Test>]
    let ``a signal sent to the process interrupts the leader asleep in an accept`` () : unit =
        // `kill(2)` aims at the process, and the leader takes it.
        for restart, expected in [ true, Ending.Restart ; false, Ending.Eintr ] do
            let system = asleep linuxWorld 0 Sleep.Accept

            let system =
                { system with
                    Process =
                        { system.Process with
                            Signals =
                                SignalState.setDisposition
                                    Signal.SIGUSR1
                                    (SignalDisposition.Catch
                                        { SignalCatch.ofHandler "h" with
                                            Restart = restart
                                        })
                                    system.Process.Signals
                        }
                }

            let self = ProcessId.toInt32 (UnixSystem.processId system)

            let system =
                match UnixSignal.kill self 10 system with
                | Ok (Ok (KillOutcome.ProcessContinues system)) -> system
                | other -> failwith $"unexpected kill: %A{other}"

            UnixWait.wakes (Set.singleton 0) system
            |> shouldEqual [ 0, Set.singleton WakePrimitive.SignalDeliverable ]

            fst (finish 0 Sleep.Accept system) |> shouldEqual expected

    [<Test>]
    let ``a task asleep in a syscall is not returning to user mode`` () : unit =
        let system =
            asleep linuxWorld 0 Sleep.Accept
            |> send 0 [ Sent.Caught (Signal.SIGUSR1, true) ]

        let thrown =
            Assert.Throws<exn> (fun () -> UnixSignal.onReturnToUser 0 system |> ignore)

        thrown.Message |> shouldContainText "asleep"

    [<Test>]
    let ``a signal whose default takes effect is refused rather than guessed at`` () : unit =
        // A default SIGCONT stays pending, and would be the first thing the
        // sleeper took on its return to user mode.
        let system =
            asleep linuxWorld 1 Sleep.Accept
            |> fun system ->
                { system with
                    Process =
                        { system.Process with
                            Signals =
                                SignalState.enqueue
                                    {
                                        Signal = Signal.SIGCONT
                                        Target = ValueSome 1
                                    }
                                    system.Process.Signals
                        }
                }

        UnixWait.wakes (Set.singleton 1) system |> List.map fst |> shouldEqual [ 1 ]

        fst (finish 1 Sleep.Accept system)
        |> shouldEqual (Ending.Refused (SyscallInterruptionRefusal.DefaultAction Signal.SIGCONT))
