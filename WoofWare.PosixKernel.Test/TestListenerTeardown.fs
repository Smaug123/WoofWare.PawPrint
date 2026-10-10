namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A listener that goes while connections wait in its accept queue: each
/// queued client is reset, as the listener's last reference is released,
/// whatever the listener's `SO_LINGER`.
///
/// The Q section of `tcp-shutdown.c` (docs/plans/2026-10-08-tcp-shutdown-linger),
/// measured on Linux 6.18.5 aarch64 and Darwin 27.0 and embedded from beside
/// the probe, is replayed through the syscalls on both flavours, printing the
/// probe's lines; so is `listener-reset-order.c` beside it, which measured the
/// order in which the resets reach one event queue.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestListenerTeardown =

    let private port : uint16 = 5000us

    let private platformOf (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform =
        match flavour with
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    /// The embedded output of the probe `name` names, for `flavour`.
    let private resource (name : string) (flavour : SimulatedUnixFlavour) : string list =
        let flavourName =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "linux"
            | SimulatedUnixFlavour.Darwin -> "darwin"

        let resource = $"WoofWare.PosixKernel.Test.%s{name}.%s{flavourName}.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.map (fun line -> line.TrimEnd '\r')
        |> Array.filter (fun line -> line <> "")
        |> Array.toList

    /// A booted system of `platform`, with tasks 1 to 4.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial<int, string> platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> fun system -> ([ 1..4 ], system) ||> List.foldBack Tasks.ensure

    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} names %A{other}"

    /// The probe's `set_linger(fd, on, seconds)`: `SO_LINGER` on Linux, and
    /// `SO_LINGER_SEC` on Darwin.
    let private setLinger
        (fd : int)
        (onOff : int)
        (seconds : int)
        (system : UnixSystem<int, string>)
        : Result<SetSockOptAnswer * UnixSystem<int, string>, SocketOptionRefusal>
        =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.socketOptionLevel platform

        let name =
            match SimulatedUnixPlatform.lingerSecondsOption platform with
            | Some seconds -> seconds
            | None -> SimulatedUnixPlatform.lingerOption platform

        let value = OptionValue.ofLinger onOff seconds

        let supplied =
            match UnixSocket.admitSetSockOpt fd level name UserBuffer.Mapped (uint32 value.Length) system with
            | Ok (SetSockOptAdmission.Transfer count) -> Some (ImmutableArray.Create (value, 0, count))
            | Ok SetSockOptAdmission.NoCopy
            | Ok (SetSockOptAdmission.Answered _)
            | Error _ -> None

        UnixSocket.setsockopt fd level name UserBuffer.Mapped (uint32 value.Length) supplied system

    let private lingerZero (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match setLinger fd 1 0 system with
        | Ok (SetSockOptAnswer.Set, system) -> system
        | other -> failwith $"setting SO_LINGER {{1, 0}} on fd %d{fd}: %A{other}"

    let private errnoName (error : UnixError) : string =
        match error with
        | UnixError.EAGAIN -> "EAGAIN"
        | UnixError.EPIPE -> "EPIPE"
        | UnixError.ECONNRESET -> "ECONNRESET"
        | other -> failwith $"the probe has no name for %O{other} that this test knows"

    /// The probe's `ans`: a count, or -1 and the errno's name.
    let private answer (result : Result<int64, UnixError>) : string =
        match result with
        | Ok count -> $"%d{count}"
        | Error error -> $"-1 %s{errnoName error}"

    let private listenerAt (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        KeventWorld.listenerAt port system

    /// A blocking socket connected to the listener at `port` by a blocking
    /// connect, as the probe's `connect_to` makes one.
    let private connectTo (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = KeventWorld.stream false system

        match KeventWorld.connect fd port system with
        | ConnectOutcome.Completed, system -> fd, system
        | other, _ -> failwith $"connect: %A{other}"

    /// The probe's `do_read`: a read of 4096 bytes.
    let private read (fd : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        match ReadOutcomes.read fd UserBuffer.Mapped 4096UL system with
        | Ok (ReadAnswer.Completed bytes, system) -> answer (Ok (int64 bytes.Length)), system
        | Ok (ReadAnswer.Failed error, system) -> answer (Error error), system
        | other -> failwith $"a read through fd %d{fd}: %A{other}"

    /// The probe's `do_write`: a write of `count` bytes, marked `+SIGPIPE` if
    /// it raised the signal.
    let private write (fd : int) (count : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        let bytes = ImmutableArray.Create<byte> (Array.zeroCreate<byte> count)

        match WriteOutcomes.admitThenWrite system.Leader fd UserBuffer.Mapped bytes system with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, system)) -> answer (Ok written), system
        | Ok (WriteOutcome.Returns (WriteAnswer.Failed error, system)) -> answer (Error error), system
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed error, entry, system)) when entry.Signal = Signal.SIGPIPE ->
            answer (Error error) + "+SIGPIPE", system
        | other -> failwith $"a write of %d{count} through fd %d{fd}: %A{other}"

    /// The probe's `do_soerr`: what `getsockopt(SO_ERROR)` read.
    let private socketError (fd : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        match KeventWorld.readSocketError fd system with
        | GetSockOptAnswer.Reported (OptionValue.Int 0), system -> "0", system
        | GetSockOptAnswer.Reported (OptionValue.Int raw), system ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering system.Machine.UnixPlatform

            match UnixError.ofRawErrnoUnder numbering raw with
            | Some error -> errnoName error, system
            | None -> failwith $"SO_ERROR read %d{raw}, which names no errno"
        | other, _ -> failwith $"getsockopt(SO_ERROR) through fd %d{fd}: %A{other}"

    /// The probe's `bind_free`: whether a fresh socket binds the loopback
    /// endpoint at `port`.
    let private bindFree (port : uint16) (system : UnixSystem<int, string>) : string =
        let fresh, system = KeventWorld.stream false system

        match
            CopyIn.bind
                fresh
                UserBuffer.Mapped
                16u
                (CopyIn.inet system.Machine.UnixPlatform (KeventWorld.loopback port))
                system
        with
        | Ok (BindAnswer.Bound _, _) -> "bind-ok"
        | Ok (BindAnswer.Failed UnixError.EADDRINUSE, _) -> "bind-EADDRINUSE"
        | other -> failwith $"binding a fresh socket at port %d{port}: %A{other}"

    /// The probe's `rdy`: `poll`'s revents for IN|OUT|PRI (and RDHUP on
    /// Linux); a fresh epoll instance's first report for the same (Linux), or
    /// a fresh kqueue's `EVFILT_READ` and `EVFILT_WRITE` (Darwin); then
    /// `FIONREAD`. What it creates is closed again.
    let private readiness (fd : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        let events =
            match flavour with
            | SimulatedUnixFlavour.Linux -> 0x2007s
            | SimulatedUnixFlavour.Darwin -> 0x0007s

        let revents, system =
            match
                UnixPoll.poll
                    1
                    [
                        {
                            Fd = fd
                            Events = events
                        }
                    ]
                    0
                    system
            with
            | Ok (PollOutcome.Answered ([ revents ], _), system) -> int (uint16 revents), system
            | other -> failwith $"poll: %A{other}"

        let waiters, system =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let epoll, system =
                    match UnixPoll.epollCreate1 0 system with
                    | Ok (Ok created) -> created
                    | other -> failwith $"epoll_create1: %A{other}"

                // The probe's registration is level-triggered, which this
                // kernel refuses; a fresh registration's first wait reports
                // the same either way, from the ADD's own edge.
                let interest =
                    EpollEvents.In
                    ||| EpollEvents.Out
                    ||| EpollEvents.Pri
                    ||| EpollEvents.RdHup
                    ||| EpollEvents.EdgeTriggered

                let system =
                    match UnixPoll.epollCtl epoll 1 fd (EpollEventArgument.Readable (interest, uint64 fd)) system with
                    | Ok (EpollCtlAnswer.Changed, system) -> system
                    | other -> failwith $"epoll_ctl: %A{other}"

                let reported, system =
                    match UnixPoll.epollWait 1 epoll 1 UserBuffer.Mapped 0 system with
                    | Ok (EpollWaitOutcome.Answered [], system) -> 0u, system
                    | Ok (EpollWaitOutcome.Answered [ _, events ], system) -> events, system
                    | other -> failwith $"epoll_wait: %A{other}"

                $"epoll=0x%x{reported}", KeventWorld.close epoll system
            | SimulatedUnixFlavour.Darwin ->
                let kq, system = KeventWorld.kqueue system

                let changes =
                    [
                        KeventWorld.change fd KeventFilter.Read KeventFlags.Add 0UL
                        KeventWorld.change fd KeventFilter.Write KeventFlags.Add 0UL
                    ]

                let reported, system =
                    match KeventWorld.apply kq changes 0 system with
                    | KeventOutcome.Answered [], system ->
                        match KeventWorld.apply kq [] 2 system with
                        | KeventOutcome.Answered events, system -> events, system
                        | other, _ -> failwith $"kevent: %A{other}"
                    | other, _ -> failwith $"kevent: %A{other}"

                let render (filter : int16) : string =
                    match reported |> List.tryFind (fun event -> event.Filter = filter) with
                    | None -> "-"
                    | Some event ->
                        let eof =
                            if event.Flags &&& KeventFlags.Eof <> 0us then
                                "/EOF"
                            else
                                ""

                        $"%d{event.Data}%s{eof}/%d{event.FilterFlags}"

                let read = render KeventFilter.Read
                let write = render KeventFilter.Write
                $"kq-read=%s{read} kq-write=%s{write}", KeventWorld.close kq system

        let readable =
            match UnixDescriptor.bytesAvailable fd UserBuffer.Mapped system with
            | Ok (BytesAvailableAnswer.Reported count) -> count
            | other -> failwith $"FIONREAD: %A{other}"

        $"poll=0x%x{revents} %s{waiters} fionread=%d{readable}", system

    // ------------------------------------------------------------------
    // Section Q
    // ------------------------------------------------------------------

    /// The Q lines of `mode`, with the section and mode stripped.
    let private measuredQ (flavour : SimulatedUnixFlavour) (mode : string) : string list =
        resource "tcpShutdown" flavour
        |> List.choose (fun line ->
            let prefix = $"Q\t%s{mode}\t"

            if line.StartsWith (prefix, StringComparison.Ordinal) then
                Some (line.Substring prefix.Length)
            else
                None
        )

    /// Section Q's row `mode` made on the kernel: q0 idle, q1 having written
    /// 100 bytes and q2 closed, all three queued; the listener closed; then
    /// what q0 and q1 see, and whether the listener's port binds. The lines
    /// are printed as the probe prints them.
    ///
    /// The probe sets the listener's linger after the three connects, which
    /// this kernel refuses (`ListenerWithQueuedConnections`, which
    /// `the measured order of the linger's set is refused` pins), so the
    /// linger row sets it before them: the listener's linger is what its close
    /// sees either way, and nothing is accepted.
    let private kernelQ (flavour : SimulatedUnixFlavour) (lingering : bool) : string list =
        let system = systemOn (platformOf flavour)

        let system =
            { system with
                Process =
                    { system.Process with
                        Signals =
                            SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore system.Process.Signals
                    }
            }

        let listener, system = listenerAt system
        let system = if lingering then lingerZero listener system else system
        let q0, system = connectTo system
        let q1, system = connectTo system
        let q2, system = connectTo system
        let _, system = UnixDescriptor.setNonBlocking q0 true system
        let _, system = UnixDescriptor.setNonBlocking q1 true system

        let system =
            match write q1 100 system with
            | "100", system -> system
            | other, _ -> failwith $"q1's write of 100: %s{other}"

        let system = KeventWorld.close q2 system

        let closed, system =
            match UnixDescriptor.close listener system with
            | Ok (SyscallAnswer.Completed 0L, system) -> "0", system
            | Ok (SyscallAnswer.Completed other, _) -> failwith $"closing the listener answered %d{other}"
            | Ok (SyscallAnswer.Failed error, system) -> answer (Error error), system
            | Error refusal -> failwith $"closing the listener was refused: %s{CloseRefusal.describe refusal}"

        UnixSystem.checkInvariants system |> shouldEqual []

        let lines, system =
            (([ closed ], system), [ 0, q0 ; 1, q1 ])
            ||> List.fold (fun (lines, system) (k, q) ->
                let ready, system = readiness q system
                let r1, system = read q system
                let w1, system = write q 100 system
                let e, system = socketError q system
                let r2, system = read q system
                UnixSystem.checkInvariants system |> shouldEqual []

                lines
                @ [
                    $"q%d{k} rdy\t%s{ready}"
                    $"q%d{k} read=%s{r1} write100=%s{w1} soerr=%s{e} read=%s{r2}"
                ],
                system
            )

        lines @ [ $"listeners-port %s{bindFree port system}" ]

    [<Test>]
    let ``each flavour's run holds the Q rows this kernel replays`` () : unit =
        for flavour in flavours do
            for mode in [ "close-nolinger" ; "close-linger0" ] do
                measuredQ flavour mode |> List.length |> shouldEqual 6

    [<Test>]
    let ``closing a listener resets each queued client, as each flavour was measured to`` () : unit =
        for flavour in flavours do
            kernelQ flavour false |> shouldEqual (measuredQ flavour "close-nolinger")

    [<Test>]
    let ``closing a listener under SO_LINGER {1, 0} resets each queued client, as each flavour was measured to``
        ()
        : unit
        =
        for flavour in flavours do
            kernelQ flavour true |> shouldEqual (measuredQ flavour "close-linger0")

    /// Section Q sets the listener's `SO_LINGER` after its connections have
    /// queued, which this kernel refuses, as it refuses any option's change on
    /// such a listener: the change would have to be kept apart from the
    /// options each queued connection completed with, which this kernel does
    /// not record. When it does, the linger row above follows the probe's own
    /// order, and this test goes.
    [<Test>]
    let ``the measured order of the linger's set is refused`` () : unit =
        for flavour in flavours do
            let system = systemOn (platformOf flavour)
            let listener, system = listenerAt system
            let _, system = connectTo system

            match setLinger listener 1 0 system with
            | Error (SocketOptionRefusal.ListenerWithQueuedConnections socket) ->
                socket |> shouldEqual (socketOf listener system)
            | other -> failwith $"%A{flavour}: expected the set to be refused, got %A{other}"

    /// What Kestrel's stop makes of its listener when nothing is queued: the
    /// linger set, then the close, both answered, and the port free.
    [<Test>]
    let ``a listener with nothing queued closes under SO_LINGER {1, 0}`` () : unit =
        for flavour in flavours do
            let system = systemOn (platformOf flavour)
            let listener, system = listenerAt system
            let system = lingerZero listener system

            match UnixDescriptor.close listener system with
            | Ok (SyscallAnswer.Completed 0L, system) ->
                UnixSystem.checkInvariants system |> shouldEqual []
                bindFree port system |> shouldEqual "bind-ok"
            | other -> failwith $"%A{flavour}: %A{other}"

    // ------------------------------------------------------------------
    // The order of the resets
    // ------------------------------------------------------------------

    /// `listener-reset-order.c` made on the kernel: four clients queued, each
    /// registered edge-triggered in `order`, the queue drained, the listener
    /// closed (under `SO_LINGER` {1, 0} if `lingering`), and the queue read,
    /// printed as the probe prints it.
    let private kernelOrder (flavour : SimulatedUnixFlavour) (order : int list) (lingering : bool) : string =
        let system = systemOn (platformOf flavour)
        let listener, system = listenerAt system
        // As in section Q, the linger is set before the connects.
        let system = if lingering then lingerZero listener system else system

        let clients, system =
            (([], system), [ 0..3 ])
            ||> List.fold (fun (clients, system) _ ->
                let client, system = connectTo system
                clients @ [ client ], system
            )

        let events, system =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let epoll, system =
                    match UnixPoll.epollCreate1 0 system with
                    | Ok (Ok created) -> created
                    | other -> failwith $"epoll_create1: %A{other}"

                let interest =
                    EpollEvents.In
                    ||| EpollEvents.Out
                    ||| EpollEvents.RdHup
                    ||| EpollEvents.EdgeTriggered

                let system =
                    (system, order)
                    ||> List.fold (fun system i ->
                        match
                            UnixPoll.epollCtl
                                epoll
                                1
                                clients.[i]
                                (EpollEventArgument.Readable (interest, uint64 i))
                                system
                        with
                        | Ok (EpollCtlAnswer.Changed, system) -> system
                        | other -> failwith $"epoll_ctl: %A{other}"
                    )

                let wait (system : UnixSystem<int, string>) =
                    match UnixPoll.epollWait 1 epoll 8 UserBuffer.Mapped 0 system with
                    | Ok (EpollWaitOutcome.Answered events, system) -> events, system
                    | other -> failwith $"epoll_wait: %A{other}"

                let _, system = wait system
                let system = KeventWorld.close listener system
                let events, system = wait system

                events |> List.map (fun (data, events) -> $"q%d{data}(0x%x{events})"), system
            | SimulatedUnixFlavour.Darwin ->
                let kq, system = KeventWorld.kqueue system

                let system =
                    (system, order)
                    ||> List.fold (fun system i ->
                        KeventWorld.register
                            kq
                            clients.[i]
                            KeventFilter.Read
                            (KeventFlags.Add ||| KeventFlags.Clear)
                            (uint64 i)
                            system
                    )

                let wait (system : UnixSystem<int, string>) =
                    match KeventWorld.apply kq [] 8 system with
                    | KeventOutcome.Answered events, system -> events, system
                    | other, _ -> failwith $"kevent: %A{other}"

                let _, system = wait system
                let system = KeventWorld.close listener system
                let events, system = wait system

                events
                |> List.map (fun event ->
                    let eof = if event.Flags &&& KeventFlags.Eof <> 0us then "EOF" else ""
                    $"q%d{event.UserData}(%s{eof}/%d{event.FilterFlags})"
                ),
                system

        UnixSystem.checkInvariants system |> shouldEqual []

        let registered = order |> List.map string |> String.concat ""
        let lingerFlag = if lingering then 1 else 0
        let events = events |> List.map (fun event -> event + " ") |> String.concat ""
        $"registered %s{registered} linger %d{lingerFlag}: %s{events}"

    [<Test>]
    let ``a closing listener resets its queued clients oldest first, as each flavour was measured to`` () : unit =
        for flavour in flavours do
            let measured = resource "listenerResetOrder" flavour

            let replayed =
                [
                    for order in [ [ 0 ; 1 ; 2 ; 3 ] ; [ 3 ; 2 ; 1 ; 0 ] ; [ 2 ; 0 ; 3 ; 1 ] ] do
                        for lingering in [ false ; true ] do
                            kernelOrder flavour order lingering
                ]

            measured |> List.length |> shouldEqual 6
            replayed |> shouldEqual measured

    // ------------------------------------------------------------------
    // A client whose connect has not reported its completion
    // ------------------------------------------------------------------

    /// A Linux client whose non-blocking connect completed, unreported, is
    /// reset as any queued client is. Its `SO_ERROR` and `poll` answer the
    /// reset; a connect, which would report the completion, is refused
    /// (`ConnectRefusal.ResetBeforeReport`), as after any reset.
    [<Test>]
    let ``a Linux client whose connect is unreported is reset by the listener's close`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64
        let listener, system = listenerAt system
        let client, system = KeventWorld.client port system

        match (UnixMachineState.socket (socketOf client system) system.Machine).Phase with
        | SocketPhase.EstablishedPendingReport _ -> ()
        | other -> failwith $"expected the connect's report pending, got %A{other}"

        let system = KeventWorld.close listener system
        UnixSystem.checkInvariants system |> shouldEqual []

        let ready, system = readiness client system
        ready |> shouldEqual "poll=0x201d epoll=0x201d fionread=0"

        CopyIn.connect
            client
            UserBuffer.Mapped
            16u
            (CopyIn.inet SimulatedUnixPlatform.linuxX64 (KeventWorld.loopback port))
            system
        |> Result.map fst
        |> shouldEqual (Error (ConnectRefusal.ResetBeforeReport (socketOf client system)))

        let error, system = socketError client system
        error |> shouldEqual "ECONNRESET"
        let ready, _ = readiness client system
        ready |> shouldEqual "poll=0x2015 epoll=0x2015 fionread=0"
