namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Random interleavings of several processes' syscalls on one
/// `SimulatedMachine`, each made in its own process's view. After every call:
/// every invariant holds, the machine's and each view's; no other process's
/// slot has moved; and the calling process's new descriptors are the lowest
/// its own table had free, whatever the other processes hold.
///
/// Excluded, for stage 4 of `docs/plans/2026-10-07-multi-process-machine.md`
/// (wakes across processes) to lift:
///
/// - **Every call that would block.** Sockets are made non-blocking, and
///   `epoll_wait` and `kevent` are polled with a zero timeout, so no task is
///   ever parked. A process's view sees only its own parked tasks, so a call in
///   one process cannot yet wake a task parked in another.
/// - **On Darwin, kqueues beside sockets in more than one process.**
///   `KqueueQueue.activate` fails loudly on *any* socket event in one process
///   while a kqueue another process owns registers anything, since that
///   kqueue's registrations name descriptors in its owner's table, which the
///   event's view cannot read. So a Darwin run is either `DarwinKqueue`, where
///   only process 0 makes kqueues and touches sockets and the others use only
///   pipes, files and directories, or `DarwinSockets`, where every process uses
///   sockets and none makes a kqueue.
/// - **Darwin `poll`.** A blocking one parks in a `ParkedKqueuePoll` that only
///   its own process's events activate; it is not in the op set at all.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMultiProcessFuzz =

    [<RequireQualifiedAccess>]
    type private Regime =
        | Linux
        | DarwinSockets
        | DarwinKqueue

    /// One call; each index is read modulo whatever it picks from.
    [<RequireQualifiedAccess>]
    type private Op =
        | Socket
        | Bind of fd : int * port : int
        | Listen of fd : int
        | Connect of fd : int * port : int
        | Accept of fd : int
        | Pipe
        | Open of path : int
        | Close of fd : int
        | Dup of fd : int
        | MkDir of dir : int
        | RmDir of dir : int
        | ChDir of dir : int
        | EpollCreate
        | EpollAdd of epoll : int * fd : int
        | EpollWait of epoll : int
        | Kqueue
        | KeventAdd of kqueue : int * fd : int
        | KeventPoll of kqueue : int

    let private ports : uint16 list = [ 8080us ; 8081us ]
    let private files : string list = [ "/f0" ; "/f1" ]
    let private dirs : string list = [ "/d0" ; "/d1" ]
    let private standing : string list = "/" :: dirs

    let private small : Gen<int> = Gen.choose (0, 63)

    let private socketOps : (int * Gen<Op>) list =
        [
            3, Gen.constant Op.Socket
            3, Gen.map2 (fun f p -> Op.Bind (f, p)) small small
            3, Gen.map Op.Listen small
            4, Gen.map2 (fun f p -> Op.Connect (f, p)) small small
            3, Gen.map Op.Accept small
        ]

    let private fileOps : (int * Gen<Op>) list =
        [
            2, Gen.constant Op.Pipe
            2, Gen.map Op.Open small
            4, Gen.map Op.Close small
            2, Gen.map Op.Dup small
            2, Gen.map Op.MkDir small
            2, Gen.map Op.RmDir small
            2, Gen.map Op.ChDir small
        ]

    let private epollOps : (int * Gen<Op>) list =
        [
            2, Gen.constant Op.EpollCreate
            6, Gen.map2 (fun e f -> Op.EpollAdd (e, f)) small small
            3, Gen.map Op.EpollWait small
        ]

    let private kqueueOps : (int * Gen<Op>) list =
        [
            2, Gen.constant Op.Kqueue
            6, Gen.map2 (fun k f -> Op.KeventAdd (k, f)) small small
            3, Gen.map Op.KeventPoll small
        ]

    /// The op set a process of the regime may draw from: only what that regime
    /// admits, so no step needs to be thrown away. `socketBias` weights the
    /// socket calls against the rest, and is itself drawn per case, so some
    /// runs are mostly sockets and some mostly files.
    let private opsFor (socketBias : int) (regime : Regime) (proc : int) : Gen<Op> =
        let sockets = socketOps |> List.map (fun (weight, op) -> weight * socketBias, op)

        match regime with
        | Regime.Linux -> Gen.frequency (sockets @ fileOps @ epollOps)
        | Regime.DarwinSockets -> Gen.frequency (sockets @ fileOps)
        | Regime.DarwinKqueue ->
            if proc = 0 then
                Gen.frequency (sockets @ fileOps @ kqueueOps)
            else
                Gen.frequency fileOps

    type private Case =
        {
            Regime : Regime
            Processes : int
            Steps : (int * Op) list
        }

    let private caseGen : Gen<Case> =
        gen {
            let! regime = Gen.elements [ Regime.Linux ; Regime.DarwinSockets ; Regime.DarwinKqueue ]
            let! processes = Gen.choose (2, 3)
            // Bias towards one process at a time or towards interleaving, so
            // both long runs within one process and constant switching occur.
            let! stickiness = Gen.choose (0, 9)
            let! socketBias = Gen.choose (1, 8)
            let! length = Gen.choose (10, 80)

            let rec steps (n : int) (current : int) (acc : (int * Op) list) : Gen<(int * Op) list> =
                if n = 0 then
                    Gen.constant (List.rev acc)
                else
                    gen {
                        let! switch = Gen.choose (0, 9)
                        let! other = Gen.choose (0, processes - 1)
                        let proc = if switch >= stickiness then other else current
                        let! op = opsFor socketBias regime proc
                        return! steps (n - 1) proc ((proc, op) :: acc)
                    }

            let! steps = steps length 0 []

            return
                {
                    Regime = regime
                    Processes = processes
                    Steps = steps
                }
        }

    /// How often each path the property exists for was reached.
    type private Coverage =
        {
            mutable CrossConnects : int
            mutable CrossAddressInUse : int
            mutable Accepts : int
            mutable CrossRmDirOfStanding : int
            mutable EpollAdds : int
            mutable KeventAdds : int
            mutable Refusals : int
            mutable Steps : int
        }

    let private fdsOf (view : UnixSystem<int, string>) : Map<int, OpenFileTarget> =
        let registry = UnixSystemState.fileDescriptors view

        FileDescriptorRegistry.fds registry
        |> Map.map (fun fd _ ->
            match FileDescriptorRegistry.tryFindTarget fd registry with
            | Some target -> target
            | None -> failwith $"fd %d{fd} names no description"
        )

    let private pick (i : int) (from : 'a list) : 'a option =
        if from.IsEmpty then None else Some from.[i % from.Length]

    let private ofKind (accept : OpenFileTarget -> bool) (view : UnixSystem<int, string>) : int list =
        fdsOf view
        |> Map.filter (fun _ target -> accept target)
        |> Map.keys
        |> List.ofSeq

    let private isSocket (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.Socket _ -> true
        | _ -> false

    /// The descriptors of `view` naming a socket `accept` admits: each call
    /// draws from the sockets it can do something with, so few steps are
    /// spent on the errors other tests cover.
    let private socketsWhere (accept : SocketDescription -> bool) (view : UnixSystem<int, string>) : int list =
        fdsOf view
        |> Map.filter (fun _ target ->
            match target with
            | OpenFileTarget.Socket socket -> accept (UnixMachineState.socket socket view.Machine)
            | _ -> false
        )
        |> Map.keys
        |> List.ofSeq

    let private unbound (socket : SocketDescription) : bool = socket.Binding.IsNone

    let private boundIdle (socket : SocketDescription) : bool =
        socket.Binding.IsSome
        && (
            match socket.Phase with
            | SocketPhase.Idle -> true
            | _ -> false
        )

    let private idle (socket : SocketDescription) : bool =
        match socket.Phase with
        | SocketPhase.Idle -> true
        | _ -> false

    let private listening (socket : SocketDescription) : bool =
        match socket.Phase with
        | SocketPhase.Listening _ -> true
        | _ -> false

    let private isEpoll (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.Epoll _ -> true
        | _ -> false

    let private isKqueue (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.Kqueue _ -> true
        | _ -> false

    /// The process whose descriptors name a socket bound at `port` in the
    /// listening phase, if any.
    let private listenerOwner (port : uint16) (machine : SimulatedMachine<int, string>) : ProcessId option =
        machine.Processes
        |> Map.toSeq
        |> Seq.tryPick (fun (pid, _) ->
            let view = SimulatedMachine.focus pid machine |> Option.get

            fdsOf view
            |> Map.exists (fun _ target ->
                match target with
                | OpenFileTarget.Socket socket ->
                    let description = UnixMachineState.socket socket view.Machine

                    match description.Phase, description.Binding with
                    | SocketPhase.Listening _, Some binding -> binding.Endpoint.Port = port
                    | _ -> false
                | _ -> false
            )
            |> fun found -> if found then Some pid else None
        )

    /// Whether the process `pid` holds a socket bound at `port`.
    let private holdsPort (port : uint16) (view : UnixSystem<int, string>) : bool =
        fdsOf view
        |> Map.exists (fun _ target ->
            match target with
            | OpenFileTarget.Socket socket ->
                match (UnixMachineState.socket socket view.Machine).Binding with
                | Some binding -> binding.Endpoint.Port = port
                | None -> false
            | _ -> false
        )

    let private openCreating : OpenFlags =
        {
            Access = FileAccessMode.ReadWrite
            Create = true
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    /// One call in the view `view` of the process `pid`, on `machine` as it
    /// stood before: the view after it. A refusal changes nothing.
    let private execute
        (coverage : Coverage)
        (machine : SimulatedMachine<int, string>)
        (pid : ProcessId)
        (op : Op)
        (view : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        let refused (view : UnixSystem<int, string>) =
            coverage.Refusals <- coverage.Refusals + 1
            view

        let sockets = ofKind isSocket view
        let any = fdsOf view |> Map.keys |> List.ofSeq

        match op with
        | Op.Socket -> KeventWorld.stream true view |> snd
        | Op.Bind (fd, port) ->
            match pick fd (socketsWhere unbound view) with
            | None -> view
            | Some fd ->
                let port = ports.[port % ports.Length]

                match
                    CopyIn.bind
                        fd
                        UserBuffer.Mapped
                        16u
                        (CopyIn.inet (UnixSystem.platform view) (KeventWorld.loopback port))
                        view
                with
                | Ok (BindAnswer.Failed UnixError.EADDRINUSE, after) ->
                    if not (holdsPort port view) then
                        coverage.CrossAddressInUse <- coverage.CrossAddressInUse + 1

                    after
                | Ok (_, after) -> after
                | Error _ -> refused view
        | Op.Listen fd ->
            match pick fd (socketsWhere boundIdle view) with
            | None -> view
            | Some fd ->
                match UnixSocket.listen fd 8 view with
                | Ok (_, after) -> after
                | Error _ -> refused view
        | Op.Connect (fd, port) ->
            match pick fd (socketsWhere idle view) with
            | None -> view
            | Some fd ->
                let port = ports.[port % ports.Length]
                let owner = listenerOwner port machine

                match
                    CopyIn.connect
                        fd
                        UserBuffer.Mapped
                        16u
                        (CopyIn.inet (UnixSystem.platform view) (KeventWorld.loopback port))
                        view
                with
                | Ok (outcome, after) ->
                    match outcome, owner with
                    | ConnectOutcome.Completed, Some owner
                    | ConnectOutcome.Failed UnixError.EINPROGRESS, Some owner when owner <> pid ->
                        coverage.CrossConnects <- coverage.CrossConnects + 1
                    | _ -> ()

                    after
                | Error _ -> refused view
        | Op.Accept fd ->
            match pick fd (socketsWhere listening view) with
            | None -> view
            | Some fd ->
                match UnixConnection.accept 0 fd UserBuffer.Mapped 16u view with
                | Ok (AcceptOutcome.Accepted _, after) ->
                    coverage.Accepts <- coverage.Accepts + 1
                    after
                | Ok (_, after) -> after
                | Error _ -> refused view
        | Op.Pipe ->
            match UnixPipe.pipe2 0 UserBuffer.Mapped view with
            | Ok (_, after) -> after
            | Error _ -> refused view
        | Op.Open path ->
            match OpenFlagWords.openPath openCreating (PathArg.ofText files.[path % files.Length]) 0o644 view with
            | Ok (_, after) -> after
            | Error _ -> refused view
        | Op.Close fd ->
            match pick fd any with
            | None -> view
            | Some fd ->
                match UnixDescriptor.close fd view with
                | Ok (_, after) -> after
                | Error _ -> refused view
        | Op.Dup fd ->
            match pick fd any with
            | None -> view
            | Some fd -> Answered.dup fd view |> snd
        | Op.MkDir dir ->
            match UnixNamespace.mkdir (PathArg.ofText dirs.[dir % dirs.Length]) 0o755 view with
            | Ok (_, after) -> after
            | Error _ -> refused view
        | Op.RmDir dir ->
            let path = dirs.[dir % dirs.Length]

            let inode =
                match
                    UnixPathResolution.resolvePath
                        AtDirectory.CurrentDirectory
                        SymlinkPolicy.NoFollowFinal
                        (UnixPath.parseOrFail "test" path)
                        view
                with
                | Ok inode -> Some inode
                | Error _ -> None

            match UnixNamespace.rmdir (PathArg.ofText path) view with
            | Ok (SyscallAnswer.Completed 0L, after) ->
                let standsElsewhere =
                    machine.Processes
                    |> Map.exists (fun other slot -> other <> pid && Some slot.Process.CurrentDirectoryInode = inode)

                if standsElsewhere then
                    coverage.CrossRmDirOfStanding <- coverage.CrossRmDirOfStanding + 1

                after
            | Ok (_, after) -> after
            | Error _ -> refused view
        | Op.ChDir dir ->
            match UnixPathResolution.chdir (PathArg.ofText standing.[dir % standing.Length]) view with
            | Ok (_, after) -> after
            | Error _ -> refused view
        | Op.EpollCreate ->
            match UnixPoll.epollCreate1 0 view with
            | Ok (Ok (_, after)) -> after
            | Ok (Error _) -> view
            | Error _ -> refused view
        | Op.EpollAdd (epoll, fd) ->
            match pick epoll (ofKind isEpoll view), pick fd any with
            | Some epoll, Some fd ->
                match
                    UnixPoll.epollCtl
                        epoll
                        1
                        fd
                        (EpollEventArgument.Readable (
                            EpollEvents.In ||| EpollEvents.Out ||| EpollEvents.EdgeTriggered,
                            0UL
                        ))
                        view
                with
                | Ok (EpollCtlAnswer.Changed, after) ->
                    coverage.EpollAdds <- coverage.EpollAdds + 1
                    after
                | Ok (_, after) -> after
                | Error _ -> refused view
            | _ -> view
        | Op.EpollWait epoll ->
            match pick epoll (ofKind isEpoll view) with
            | None -> view
            | Some epoll ->
                match UnixPoll.epollWait 0 epoll 8 UserBuffer.Mapped 0 view with
                | Ok (EpollWaitOutcome.WouldBlock condition, _) ->
                    failwith $"epoll_wait with no timeout parked on %A{condition}"
                | Ok (_, after) -> after
                | Error _ -> refused view
        | Op.Kqueue -> KeventWorld.kqueue view |> snd
        | Op.KeventAdd (kqueue, fd) ->
            match pick kqueue (ofKind isKqueue view), pick fd sockets with
            | Some kqueue, Some fd ->
                match
                    UnixKqueue.kevent
                        0
                        kqueue
                        1
                        [ KeventWorld.change fd -1s 0x1us 0UL ]
                        0
                        UserBuffer.Mapped
                        (KeventTimeout.Readable (0L, 0L))
                        view
                with
                | Ok (KeventOutcome.Answered [], after) ->
                    coverage.KeventAdds <- coverage.KeventAdds + 1
                    after
                | Ok (KeventOutcome.WouldBlock condition, _) ->
                    failwith $"kevent with no timeout parked on %A{condition}"
                | Ok (_, after) -> after
                | Error _ -> refused view
            | _ -> view
        | Op.KeventPoll kqueue ->
            match pick kqueue (ofKind isKqueue view) with
            | None -> view
            | Some kqueue ->
                match UnixKqueue.kevent 0 kqueue 0 [] 8 UserBuffer.Mapped (KeventTimeout.Readable (0L, 0L)) view with
                | Ok (KeventOutcome.WouldBlock condition, _) ->
                    failwith $"kevent with no timeout parked on %A{condition}"
                | Ok (_, after) -> after
                | Error _ -> refused view

    let private assertClean (machine : SimulatedMachine<int, string>) : unit =
        SimulatedMachine.checkInvariants machine |> shouldEqual []

        for pid in SimulatedMachine.processIds machine do
            let view = SimulatedMachine.focus pid machine |> Option.get
            UnixSystem.checkInvariants view |> shouldEqual []

            VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes view) view.Machine.FileSystem
            |> shouldEqual []

    let private run (coverage : Coverage) (case : Case) : unit =
        let platform =
            match case.Regime with
            | Regime.Linux -> SimulatedUnixPlatform.linuxX64
            | Regime.DarwinSockets
            | Regime.DarwinKqueue -> SimulatedUnixPlatform.macOsArm64

        let first =
            UnixSystem.initial<int, string> platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let pids, machine =
            (([ UnixSystem.processId first ], SimulatedMachine.ofSystem first), [ 2 .. case.Processes ])
            ||> List.fold (fun (pids, machine) _ ->
                match
                    SimulatedMachine.launch
                        (Launched.launch platform UnixSystem.pipedStandardStreams 0 (CpuId 0))
                        machine
                with
                | Ok (pid, machine) -> pids @ [ pid ], machine
                | Error refusal -> failwith $"launch: %s{ProcessCreationRefusal.describe refusal}"
            )

        assertClean machine

        (machine, case.Steps)
        ||> List.fold (fun machine (proc, op) ->
            coverage.Steps <- coverage.Steps + 1
            let pid = pids.[proc]
            let before = SimulatedMachine.focus pid machine |> Option.get
            let beforeFds = fdsOf before |> Map.keys |> Set.ofSeq

            let after =
                match SimulatedMachine.inView pid (fun view -> (), execute coverage machine pid op view) machine with
                | Some ((), after) -> after
                | None -> failwith $"no process %O{pid}"

            // (b) Every other process is exactly as it was.
            for other in pids do
                if other <> pid then
                    after.Processes.[other] |> shouldEqual machine.Processes.[other]

            // (c) The caller's new descriptors are the lowest its own table had
            // free, whatever any other process holds.
            let afterFds =
                fdsOf (SimulatedMachine.focus pid after |> Option.get) |> Map.keys |> Set.ofSeq

            let added = Set.difference afterFds beforeFds

            let lowestFree =
                Seq.initInfinite id
                |> Seq.filter (fun fd -> not (Set.contains fd beforeFds))
                |> Seq.take added.Count
                |> Set.ofSeq

            added |> shouldEqual lowestFree

            // (a) Every invariant holds.
            assertClean after
            after
        )
        |> ignore

    [<Test>]
    let ``random interleavings of several processes' calls keep every invariant and touch no other process`` () : unit =
        let coverage =
            {
                CrossConnects = 0
                CrossAddressInUse = 0
                Accepts = 0
                CrossRmDirOfStanding = 0
                EpollAdds = 0
                KeventAdds = 0
                Refusals = 0
                Steps = 0
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen caseGen) (run coverage))

        // The paths the property exists for, each reached often enough that a
        // generator regression shows here rather than as a silently weaker
        // test.
        // About a third of what 1000 cases reached when this was written.
        coverage.CrossConnects |> shouldBeGreaterThan 80
        coverage.CrossAddressInUse |> shouldBeGreaterThan 120
        coverage.Accepts |> shouldBeGreaterThan 70
        coverage.CrossRmDirOfStanding |> shouldBeGreaterThan 20
        coverage.EpollAdds |> shouldBeGreaterThan 50
        coverage.KeventAdds |> shouldBeGreaterThan 30
        // Refusals are steps that tested nothing; they must stay rare.
        coverage.Refusals * 10 |> shouldBeSmallerThan coverage.Steps
