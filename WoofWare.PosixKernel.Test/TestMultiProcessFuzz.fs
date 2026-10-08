namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Random interleavings of several processes' syscalls on one
/// `SimulatedMachine`, each made by one of its process's tasks in that
/// process's view, with the machine's own steps between them: a wake pass that
/// finishes the calls it wakes, each in its own process's view, the clock
/// moving on, and a process ending.
///
/// After every step: every invariant holds, the machine's, each view's and each
/// view's descriptor table's; no pipe is named in two processes' tables; and
/// no slot but the stepping process's has moved, except where the step is the
/// machine's (a wake pass finishes calls in whichever processes they are in,
/// one process at a time, and each finish moves its own process's slot
/// alone; a process's end removes its own). After a call, the calling
/// process's new descriptors are the lowest its own table had free, whatever
/// the other processes hold.
///
/// Calls block: sockets are blocking or not, `accept`, `epoll_wait`, `kevent`,
/// `poll`, `flock` and pipe transfers sleep, and a sleeping task makes no call
/// until a wake pass finishes its own. A connected socket's `read` and `write`
/// move bytes between processes, through buffers small enough that writes
/// fill them, and a blocking one sleeps until the other process's transfer,
/// close or end wakes it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMultiProcessFuzz =

    /// One step; each index is read modulo whatever it picks from.
    [<RequireQualifiedAccess>]
    type private Op =
        | Socket of nonBlocking : bool
        | Bind of fd : int * port : int
        | Listen of fd : int
        | Connect of fd : int * port : int
        | Accept of fd : int
        /// A read of a connected socket, of `size` bytes: one whose
        /// description is non-blocking when `nonBlocking` and there is one.
        | Receive of fd : int * size : int * nonBlocking : bool
        /// A write of `size` bytes to a connected socket, chosen as for
        /// `Receive`.
        | Send of fd : int * size : int * nonBlocking : bool
        /// Set `O_NONBLOCK` on a connected socket's description.
        | MakeNonBlocking of fd : int
        | Pipe
        | PipeRead of fd : int
        | PipeWrite of fd : int * size : int
        | Open of path : int
        | Close of fd : int
        | Dup of fd : int
        | Flock of fd : int * operation : int
        | MkDir of dir : int
        | RmDir of dir : int
        | ChDir of dir : int
        | EpollCreate
        | EpollAdd of epoll : int * fd : int
        | EpollWait of epoll : int * timeout : int
        | Kqueue
        | KeventAdd of kqueue : int * fd : int * write : bool
        | KeventWait of kqueue : int * timeout : int
        /// A `poll` of `fds` for IN, and for OUT too unless `readOnly`.
        | Poll of fds : int list * timeout : int * readOnly : bool
        | Exit of status : int

    /// A step of the machine's own, or a call made by a task of a process.
    [<RequireQualifiedAccess>]
    type private Step =
        | Call of proc : int * task : int * op : Op
        /// Find which sleeping calls the machine wakes, and finish up to
        /// `finishing` of the woken calls not yet finished, oldest woken first.
        | WakePass of finishing : int
        /// Let `milliseconds` pass on the machine's clock.
        | Advance of milliseconds : int

    let private ports : uint16 list = [ 8080us ; 8081us ; 8082us ]
    let private files : string list = [ "/f0" ; "/f1" ]
    let private dirs : string list = [ "/d0" ; "/d1" ]
    let private standing : string list = "/" :: dirs
    let private tasks : int = 3

    // `flock`'s operations, the same on both flavours.
    let private lockShared : int = 1
    let private lockExclusive : int = 2
    let private unlock : int = 8

    let private small : Gen<int> = Gen.choose (0, 63)

    /// -1 waits for ever, 0 not at all, and 5 milliseconds until an advance.
    let private timeout : Gen<int> = Gen.elements [ -1 ; -1 ; 0 ; 5 ]

    let private socketOps : (int * Gen<Op>) list =
        [
            3, Gen.map Op.Socket (Gen.elements [ true ; false ])
            3, Gen.map2 (fun f p -> Op.Bind (f, p)) small small
            3, Gen.map Op.Listen small
            4, Gen.map2 (fun f p -> Op.Connect (f, p)) small small
            3, Gen.map Op.Accept small
        ]

    /// Transfers on connected sockets, weighted apart from `socketOps` so that
    /// a case biased towards sockets is not one biased towards transfers.
    let private transferOps : (int * Gen<Op>) list =
        [
            3,
            Gen.map3
                (fun f s n -> Op.Receive (f, s, n))
                small
                (Gen.elements [ 0 ; 1 ; 1000 ; 65536 ])
                (Gen.elements [ true ; false ])
            3,
            Gen.map3
                (fun f s n -> Op.Send (f, s, n))
                small
                (Gen.elements [ 0 ; 1 ; 1000 ; 70000 ; 300000 ])
                (Gen.elements [ true ; false ])
            1, Gen.map Op.MakeNonBlocking small
        ]

    let private fileOps : (int * Gen<Op>) list =
        [
            2, Gen.constant Op.Pipe
            2, Gen.map Op.PipeRead small
            2, Gen.map2 (fun f s -> Op.PipeWrite (f, s)) small (Gen.elements [ 1 ; 4096 ; 70000 ])
            3, Gen.map Op.Open small
            4, Gen.map Op.Close small
            2, Gen.map Op.Dup small
            5,
            Gen.map2
                (fun f o -> Op.Flock (f, o))
                small
                (Gen.elements [ lockShared ; lockExclusive ; lockExclusive ; unlock ])
            2, Gen.map Op.MkDir small
            2, Gen.map Op.RmDir small
            2, Gen.map Op.ChDir small
            5,
            Gen.map3
                (fun fds t r -> Op.Poll (fds, t, r))
                (Gen.listOfLength 2 small)
                timeout
                (Gen.elements [ false ; true ])
        ]

    let private epollOps : (int * Gen<Op>) list =
        [
            2, Gen.constant Op.EpollCreate
            5, Gen.map2 (fun e f -> Op.EpollAdd (e, f)) small small
            4, Gen.map2 (fun e t -> Op.EpollWait (e, t)) small timeout
        ]

    let private kqueueOps : (int * Gen<Op>) list =
        [
            2, Gen.constant Op.Kqueue
            5, Gen.map3 (fun k f w -> Op.KeventAdd (k, f, w)) small small (Gen.elements [ false ; true ])
            4, Gen.map2 (fun k t -> Op.KeventWait (k, t)) small timeout
        ]

    /// The calls a process on `platform` may make. `socketBias` weights the
    /// socket calls against the rest, and `transferBias` the transfers on
    /// connected sockets, each drawn per case, so some runs are mostly sockets,
    /// some mostly transfers and some mostly files.
    let private opsFor (socketBias : int) (transferBias : int) (platform : SimulatedUnixPlatform) : Gen<Op> =
        let sockets = socketOps |> List.map (fun (weight, op) -> weight * socketBias, op)

        let transferOps =
            transferOps |> List.map (fun (weight, op) -> weight * transferBias, op)

        let exit = [ 1, Gen.map Op.Exit (Gen.choose (0, 3)) ]
        // A process's end is rare, so that most of a run has several processes.
        let common (ops : (int * Gen<Op>) list) =
            ops |> List.map (fun (weight, op) -> 3 * weight, op)

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> Gen.frequency (common (sockets @ transferOps @ fileOps @ epollOps) @ exit)
        | SimulatedUnixFlavour.Darwin -> Gen.frequency (common (sockets @ transferOps @ fileOps @ kqueueOps) @ exit)

    type private Case =
        {
            Platform : SimulatedUnixPlatform
            Processes : int
            /// Whether each process starts with a listener at a port of its
            /// own, an epoll instance or kqueue watching it, and a file another
            /// process can lock, so that the random calls after meet something
            /// another process's calls change.
            Prepared : bool
            Steps : Step list
        }

    let private caseGen : Gen<Case> =
        gen {
            let! platform = Gen.elements [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]
            let! processes = Gen.choose (2, 3)
            let! prepared = Gen.elements [ true ; false ]
            // Bias towards one process at a time or towards interleaving, so
            // both long runs within one process and constant switching occur.
            let! stickiness = Gen.choose (0, 9)
            let! socketBias = Gen.choose (1, 6)
            let! transferBias = Gen.elements [ 1 ; 1 ; 1 ; 3 ; 8 ]
            let! length = Gen.choose (20, 120)

            let rec steps (n : int) (current : int) (acc : Step list) : Gen<Step list> =
                if n = 0 then
                    Gen.constant (List.rev acc)
                else
                    gen {
                        let! kind = Gen.choose (0, 19)

                        match kind with
                        | k when k < 2 ->
                            let! finishing = Gen.choose (1, 4)
                            return! steps (n - 1) current (Step.WakePass finishing :: acc)
                        | 2 ->
                            let! milliseconds = Gen.elements [ 1 ; 5 ; 10 ]
                            return! steps (n - 1) current (Step.Advance milliseconds :: acc)
                        | _ ->
                            let! switch = Gen.choose (0, 9)
                            let! other = Gen.choose (0, processes - 1)
                            let proc = if switch >= stickiness then other else current
                            let! task = Gen.choose (0, tasks - 1)
                            let! op = opsFor socketBias transferBias platform
                            return! steps (n - 1) proc (Step.Call (proc, task, op) :: acc)
                    }

            let! steps = steps length 0 []

            return
                {
                    Platform = platform
                    Processes = processes
                    Prepared = prepared
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
            mutable Parks : int
            /// A call in one process that made a call asleep in another wakeable.
            mutable CrossWakes : int
            /// How many of those each kind of wake condition woke.
            mutable CrossWakeKinds : Map<string, int>
            /// A finish that answered, in a process other than the one whose
            /// call made it wakeable.
            mutable Finishes : int
            mutable DarwinPollParks : int
            mutable KeventParks : int
            mutable Exits : int
            mutable ExitRefusals : int
            /// A read that took bytes another process's socket had written.
            mutable CrossReads : int
            /// A write that took fewer bytes than offered, or none (`EAGAIN`).
            mutable ShortWrites : int
            /// A read or write that met a reset (`ECONNRESET` or `EPIPE`).
            mutable Resets : int
            /// A read that met end of file.
            mutable EndsOfFile : int
            /// A blocking read of a connected socket that slept.
            mutable ConnectionReadParks : int
            /// A blocking write to a connected socket that slept, having taken
            /// part of its bytes or none.
            mutable ConnectionWriteParks : int
            /// A finished connection transfer that answered.
            mutable ConnectionFinishes : int
            mutable Refusals : int
            mutable Calls : int
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

    let private isFile (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.File _ -> true
        | _ -> false

    let private isPipeEnd (pipeEnd : PipeEnd) (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.Pipe (_, named) -> named = pipeEnd
        | _ -> false

    let private isEpoll (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.Epoll _ -> true
        | _ -> false

    let private isKqueue (target : OpenFileTarget) : bool =
        match target with
        | OpenFileTarget.Kqueue _ -> true
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

    let private connected (socket : SocketDescription) : bool =
        (SocketPhase.connectionEnd socket.Phase).IsSome

    /// The descriptors of `view` naming a connected socket, through a
    /// non-blocking description if `nonBlocking` and `view` has one.
    let private connectedFds (nonBlocking : bool) (view : UnixSystem<int, string>) : int list =
        let all = socketsWhere connected view

        let unblocked =
            all
            |> List.filter (fun fd ->
                match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors view) with
                | Some description -> description.NonBlocking
                | None -> false
            )

        if nonBlocking && not unblocked.IsEmpty then
            unblocked
        else
            all

    /// The process whose descriptors name the socket at the other end of the
    /// connected socket `socketId`, if one is.
    let private peerOwner (socketId : SocketId) (machine : SimulatedMachine<int, string>) : ProcessId option =
        let view =
            SimulatedMachine.focus (Seq.head (SimulatedMachine.processIds machine)) machine
            |> Option.get

        match SocketPhase.connectionEnd (UnixMachineState.socket socketId view.Machine).Phase with
        | None -> None
        | Some (connection, connectionEnd) ->
            let other =
                match connectionEnd with
                | ConnectionEnd.Client -> ConnectionEnd.Server
                | ConnectionEnd.Server -> ConnectionEnd.Client

            match UnixMachineState.socketHoldingEnd connection other view.Machine with
            | None -> None
            | Some peer ->
                machine.Processes
                |> Map.toSeq
                |> Seq.tryPick (fun (pid, _) ->
                    let names =
                        fdsOf (SimulatedMachine.focus pid machine |> Option.get)
                        |> Map.exists (fun _ target -> target = OpenFileTarget.Socket peer)

                    if names then Some pid else None
                )

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

    /// What became of a call or a finish.
    [<RequireQualifiedAccess>]
    type private Made =
        /// It answered, or changed nothing, and the process carries on as this.
        | Answered of UnixSystem<int, string>
        /// The calling task sleeps, in this.
        | Sleeps of UnixSystem<int, string>
        /// The call ended the process.
        | Ended of EndedProcess<int, string>
        /// The library refused it, which changes nothing.
        | Refused

    let private ofWrite (outcome : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>) : Made =
        match outcome with
        | Error _ -> Made.Refused
        | Ok (WriteOutcome.Returns (_, view))
        | Ok (WriteOutcome.ReturnsRaising (_, _, view))
        | Ok (WriteOutcome.Restarts view) -> Made.Answered view
        | Ok (WriteOutcome.WouldBlock (_, view)) -> Made.Sleeps view
        | Ok (WriteOutcome.ProcessEnded ended) -> Made.Ended ended

    /// One call by `task` of the process `pid`, in its view `view`, on `machine`
    /// as it stood before.
    let private call
        (coverage : Coverage)
        (machine : SimulatedMachine<int, string>)
        (pid : ProcessId)
        (task : int)
        (op : Op)
        (view : UnixSystem<int, string>)
        : Made
        =
        let any = fdsOf view |> Map.keys |> List.ofSeq
        let sockets = ofKind isSocket view

        let answered (result : Result<'a * UnixSystem<int, string>, 'r>) : Made =
            match result with
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused

        match op with
        | Op.Socket nonBlocking -> Made.Answered (KeventWorld.stream nonBlocking view |> snd)
        | Op.Bind (fd, port) ->
            match pick fd (socketsWhere unbound view) with
            | None -> Made.Answered view
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

                    Made.Answered after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
        | Op.Listen fd ->
            match pick fd (socketsWhere boundIdle view) with
            | None -> Made.Answered view
            | Some fd -> UnixSocket.listen fd 8 view |> answered
        | Op.Connect (fd, port) ->
            match pick fd (socketsWhere idle view) with
            | None -> Made.Answered view
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

                    Made.Answered after
                | Error _ -> Made.Refused
        | Op.Accept fd ->
            match pick fd (socketsWhere listening view) with
            | None -> Made.Answered view
            | Some fd ->
                match UnixConnection.accept task fd UserBuffer.Mapped 16u view with
                | Ok (AcceptOutcome.Accepted _, after) ->
                    coverage.Accepts <- coverage.Accepts + 1
                    Made.Answered after
                | Ok (AcceptOutcome.WouldBlock _, after) -> Made.Sleeps after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
        | Op.MakeNonBlocking fd ->
            match pick fd (socketsWhere connected view) with
            | None -> Made.Answered view
            | Some fd -> Made.Answered (UnixDescriptor.setNonBlocking fd true view |> snd)
        | Op.Receive (fd, size, nonBlocking) ->
            match pick fd (connectedFds nonBlocking view) with
            | None -> Made.Answered view
            | Some fd ->
                let socketId =
                    match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors view) with
                    | Some (OpenFileTarget.Socket socketId) -> socketId
                    | other -> failwith $"fd %d{fd} names %A{other}"

                let writer = peerOwner socketId machine

                match UnixReadWrite.read task fd UserBuffer.Mapped (uint64 size) view with
                | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                    if not bytes.IsEmpty && writer.IsSome && writer <> Some pid then
                        coverage.CrossReads <- coverage.CrossReads + 1

                    if bytes.IsEmpty && size > 0 then
                        coverage.EndsOfFile <- coverage.EndsOfFile + 1

                    Made.Answered after
                | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.ECONNRESET), after)
                | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EPIPE), after) ->
                    coverage.Resets <- coverage.Resets + 1
                    Made.Answered after
                | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN), after) -> Made.Answered after
                | Ok (ReadOutcome.WouldBlock _, after) ->
                    coverage.ConnectionReadParks <- coverage.ConnectionReadParks + 1
                    Made.Sleeps after
                | Ok (outcome, _) -> failwith $"a read of a connected socket answered %A{outcome}"
                | Error refusal ->
                    failwith $"a read of a connected socket was refused: %s{ReadRefusal.describe refusal}"
        | Op.Send (fd, size, nonBlocking) ->
            match pick fd (connectedFds nonBlocking view) with
            | None -> Made.Answered view
            | Some fd ->
                let bytes = ImmutableArray.Create<byte> (Array.init size byte)

                match WriteOutcomes.admitThenWrite task fd UserBuffer.Mapped bytes view with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed written, after)) ->
                    if written < int64 size then
                        coverage.ShortWrites <- coverage.ShortWrites + 1

                    Made.Answered after
                | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, after)) ->
                    coverage.ShortWrites <- coverage.ShortWrites + 1
                    Made.Answered after
                | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.ECONNRESET, _))
                | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, _, _))
                | Ok (WriteOutcome.ProcessEnded _) as outcome ->
                    coverage.Resets <- coverage.Resets + 1
                    ofWrite outcome
                | Ok (WriteOutcome.WouldBlock (_, after)) ->
                    coverage.ConnectionWriteParks <- coverage.ConnectionWriteParks + 1
                    Made.Sleeps after
                | Ok outcome -> failwith $"a write to a connected socket answered %A{outcome}"
                | Error refusal ->
                    failwith $"a write to a connected socket was refused: %s{WriteRefusal.describe refusal}"
        | Op.Pipe -> UnixPipe.pipe2 0 UserBuffer.Mapped view |> answered
        | Op.PipeRead fd ->
            match pick fd (ofKind (isPipeEnd PipeEnd.Read) view) with
            | None -> Made.Answered view
            | Some fd ->
                match UnixReadWrite.read task fd UserBuffer.Mapped 4096UL view with
                | Ok (ReadOutcome.WouldBlock _, after) -> Made.Sleeps after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
        | Op.PipeWrite (fd, size) ->
            match pick fd (ofKind (isPipeEnd PipeEnd.Write) view) with
            | None -> Made.Answered view
            | Some fd ->
                WriteOutcomes.admitThenWrite
                    task
                    fd
                    UserBuffer.Mapped
                    (ImmutableArray.Create<byte> (Array.zeroCreate<byte> size))
                    view
                |> ofWrite
        | Op.Open path ->
            OpenFlagWords.openPath openCreating (PathArg.ofText files.[path % files.Length]) 0o644 view
            |> answered
        | Op.Close fd ->
            match pick fd any with
            | None -> Made.Answered view
            | Some fd -> UnixDescriptor.close fd view |> answered
        | Op.Dup fd ->
            match pick fd any with
            | None -> Made.Answered view
            | Some fd -> Made.Answered (Answered.dup fd view |> snd)
        | Op.Flock (fd, operation) ->
            match pick fd (ofKind isFile view) with
            | None -> Made.Answered view
            | Some fd ->
                match UnixDescriptor.flock task fd operation view with
                | Ok (SyscallOutcome.WouldBlock _, after) -> Made.Sleeps after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
        | Op.MkDir dir ->
            UnixNamespace.mkdir (PathArg.ofText dirs.[dir % dirs.Length]) 0o755 view
            |> answered
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

                Made.Answered after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Op.ChDir dir ->
            UnixPathResolution.chdir (PathArg.ofText standing.[dir % standing.Length]) view
            |> answered
        | Op.EpollCreate ->
            match UnixPoll.epollCreate1 0 view with
            | Ok (Ok (_, after)) -> Made.Answered after
            | Ok (Error _) -> Made.Answered view
            | Error _ -> Made.Refused
        | Op.EpollAdd (epoll, fd) ->
            match pick epoll (ofKind isEpoll view), pick fd sockets with
            | Some epoll, Some fd ->
                match
                    UnixPoll.epollCtl
                        epoll
                        1
                        fd
                        (EpollEventArgument.Readable (
                            EpollEvents.In
                            ||| EpollEvents.Out
                            ||| EpollEvents.RdHup
                            ||| EpollEvents.EdgeTriggered,
                            0UL
                        ))
                        view
                with
                | Ok (EpollCtlAnswer.Changed, after) ->
                    coverage.EpollAdds <- coverage.EpollAdds + 1
                    Made.Answered after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
            | _ -> Made.Answered view
        | Op.EpollWait (epoll, milliseconds) ->
            match pick epoll (ofKind isEpoll view) with
            | None -> Made.Answered view
            | Some epoll ->
                match UnixPoll.epollWait task epoll 8 UserBuffer.Mapped milliseconds view with
                | Ok (EpollWaitOutcome.WouldBlock _, after) -> Made.Sleeps after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
        | Op.Kqueue -> Made.Answered (KeventWorld.kqueue view |> snd)
        | Op.KeventAdd (kqueue, fd, write) ->
            match pick kqueue (ofKind isKqueue view), pick fd sockets with
            | Some kqueue, Some fd ->
                let filter = if write then KeventFilter.Write else KeventFilter.Read

                match
                    UnixKqueue.kevent
                        task
                        kqueue
                        1
                        [ KeventWorld.change fd filter (KeventFlags.Add ||| KeventFlags.Clear) 0UL ]
                        0
                        UserBuffer.Mapped
                        (KeventTimeout.Readable (0L, 0L))
                        view
                with
                | Ok (KeventOutcome.Answered [], after) ->
                    coverage.KeventAdds <- coverage.KeventAdds + 1
                    Made.Answered after
                | Ok (KeventOutcome.WouldBlock condition, _) -> failwith $"kevent with no room parked on %A{condition}"
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
            | _ -> Made.Answered view
        | Op.KeventWait (kqueue, milliseconds) ->
            match pick kqueue (ofKind isKqueue view) with
            | None -> Made.Answered view
            | Some kqueue ->
                let timeout =
                    if milliseconds < 0 then
                        KeventTimeout.Null
                    else
                        KeventTimeout.Readable (0L, int64 milliseconds * 1_000_000L)

                match UnixKqueue.kevent task kqueue 0 [] 8 UserBuffer.Mapped timeout view with
                | Ok (KeventOutcome.WouldBlock _, after) ->
                    coverage.KeventParks <- coverage.KeventParks + 1
                    Made.Sleeps after
                | Ok (_, after) -> Made.Answered after
                | Error _ -> Made.Refused
        | Op.Poll (fds, milliseconds, readOnly) ->
            let entries =
                fds
                |> List.choose (fun fd -> pick fd (if fd % 4 = 0 then any else sockets))
                |> List.map (fun fd ->
                    {
                        Fd = fd
                        Events = if readOnly then 0x0001s else 0x0001s ||| 0x0004s
                    }
                )

            match UnixPoll.poll task entries milliseconds view with
            | Ok (PollOutcome.WouldBlock _, after) ->
                match SimulatedUnixPlatform.flavour view.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Darwin -> coverage.DarwinPollParks <- coverage.DarwinPollParks + 1
                | SimulatedUnixFlavour.Linux -> ()

                Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Op.Exit status -> Made.Ended (UnixTaskLifecycle.exitGroup task status view)

    /// The call `task` of the view `view` is asleep in, finished.
    let private finish (coverage : Coverage) (task : int) (view : UnixSystem<int, string>) : Made =
        match UnixTaskTable.parkedFor task view.Tasks with
        | Some (ParkedSyscall.Accept _) ->
            match UnixConnection.finishAccept task view with
            | Ok (AcceptOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.EpollWait _) ->
            match UnixPoll.finishEpollWait task view with
            | Ok (EpollWaitOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.Kevent _) ->
            match UnixKqueue.finishKevent task view with
            | Ok (KeventOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.Poll _)
        | Some (ParkedSyscall.KqueuePoll _) ->
            match UnixPoll.finishPoll task view with
            | Ok (PollOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.Flock _) ->
            match UnixDescriptor.flockAcquire task view with
            | Ok (SyscallOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.PipeRead _) ->
            match UnixReadWrite.finishRead task view with
            | Ok (ReadOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) -> Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.ConnectionRead _) ->
            match UnixReadWrite.finishRead task view with
            | Ok (ReadOutcome.WouldBlock _, after) -> Made.Sleeps after
            | Ok (_, after) ->
                coverage.ConnectionFinishes <- coverage.ConnectionFinishes + 1
                Made.Answered after
            | Error _ -> Made.Refused
        | Some (ParkedSyscall.PipeWrite _)
        | Some (ParkedSyscall.ConnectionWrite _) ->
            let connection =
                match UnixTaskTable.parkedFor task view.Tasks with
                | Some (ParkedSyscall.ConnectionWrite _) -> true
                | _ -> false

            let made =
                match UnixReadWrite.admitFinishWrite task view with
                | Error _ -> Made.Refused
                | Ok (WriteOutcome.Returns (WriteResumption.Transfer (offset, count), admitted)) ->
                    UnixReadWrite.finishWrite
                        task
                        (ImmutableArray.Create<byte> (Array.init count (fun i -> byte (offset + i))))
                        admitted
                    |> ofWrite
                | Ok (WriteOutcome.Returns (WriteResumption.Answered _, after))
                | Ok (WriteOutcome.ReturnsRaising (_, _, after))
                | Ok (WriteOutcome.Restarts after) -> Made.Answered after
                | Ok (WriteOutcome.WouldBlock (_, after)) -> Made.Sleeps after
                | Ok (WriteOutcome.ProcessEnded ended) -> Made.Ended ended

            match made with
            | Made.Answered _ when connection -> coverage.ConnectionFinishes <- coverage.ConnectionFinishes + 1
            | _ -> ()

            made
        | None -> failwith $"task %d{task} is woken, and parked in nothing"

    /// Everything the run tracks besides the machine.
    type private World =
        {
            Machine : SimulatedMachine<int, string>
            /// The processes still on the machine, in launch order.
            Live : ProcessId list
            /// The tasks held asleep in a call.
            Asleep : Set<ProcessId * int>
            /// The tasks a wake pass has woken whose calls have not finished, in
            /// the order they woke.
            Woken : (ProcessId * int) list
        }

    let private asleepByProcess (asleep : Set<ProcessId * int>) : Map<ProcessId, Set<int>> =
        asleep
        |> Set.toList
        |> List.groupBy fst
        |> List.map (fun (pid, tasks) -> pid, tasks |> List.map snd |> Set.ofList)
        |> Map.ofList

    let private wakeable (world : World) : Map<ProcessId * int, Set<WakePrimitive>> =
        SimulatedMachine.wakes (asleepByProcess world.Asleep) world.Machine
        |> Map.ofList

    let private assertClean (machine : SimulatedMachine<int, string>) : unit =
        SimulatedMachine.checkInvariants machine |> shouldEqual []

        for pid in SimulatedMachine.processIds machine do
            let view = SimulatedMachine.focus pid machine |> Option.get
            UnixSystem.checkInvariants view |> shouldEqual []

            FileDescriptorRegistry.checkInvariants (UnixSystem.fileDescriptors view)
            |> shouldEqual []

            VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes view) view.Machine.FileSystem
            |> shouldEqual []

        // Nothing passes a descriptor to another process, so each pipe's ends
        // are one process's.
        let pipesOf (pid : ProcessId) : Set<PipeId> =
            fdsOf (SimulatedMachine.focus pid machine |> Option.get)
            |> Map.values
            |> Seq.choose (fun target ->
                match target with
                | OpenFileTarget.Pipe (pipe, _) -> Some pipe
                | _ -> None
            )
            |> Set.ofSeq

        SimulatedMachine.processIds machine
        |> Seq.toList
        |> List.collect (fun pid -> pipesOf pid |> Set.toList)
        |> List.countBy id
        |> List.filter (fun (_, count) -> count > 1)
        |> shouldEqual []

    /// Every slot but `except`'s is as it was.
    let private othersUnmoved
        (except : ProcessId)
        (before : SimulatedMachine<int, string>)
        (after : SimulatedMachine<int, string>)
        : unit
        =
        for KeyValue (pid, slot) in after.Processes do
            if pid <> except then
                slot |> shouldEqual before.Processes.[pid]

    /// `made`, by `task` of `pid`, written into `world`: a view written back,
    /// the task asleep, or the process ended.
    let private settle (coverage : Coverage) (pid : ProcessId) (task : int) (made : Made) (world : World) : World =
        match made with
        | Made.Refused ->
            coverage.Refusals <- coverage.Refusals + 1
            world
        | Made.Answered view ->
            { world with
                Machine = SimulatedMachine.unfocus view world.Machine
            }
        | Made.Sleeps view ->
            coverage.Parks <- coverage.Parks + 1

            { world with
                Machine = SimulatedMachine.unfocus view world.Machine
                Asleep = Set.add (pid, task) world.Asleep
            }
        | Made.Ended ended ->
            match SimulatedMachine.endProcess ended world.Machine with
            | Error _ ->
                coverage.ExitRefusals <- coverage.ExitRefusals + 1
                world
            | Ok (_, machine) ->
                coverage.Exits <- coverage.Exits + 1
                othersUnmoved pid world.Machine machine
                machine.Processes.ContainsKey pid |> shouldEqual false

                {
                    Machine = machine
                    Live = world.Live |> List.filter ((<>) pid)
                    Asleep = world.Asleep |> Set.filter (fun (owner, _) -> owner <> pid)
                    Woken = world.Woken |> List.filter (fun (owner, _) -> owner <> pid)
                }

    let private step (coverage : Coverage) (world : World) (next : Step) : World =
        match next with
        | Step.Advance milliseconds ->
            match world.Live with
            | [] -> world
            | pid :: _ ->

            let before = world.Machine

            let machine =
                SimulatedMachine.inView
                    pid
                    (fun view -> (), UnixSystem.advanceClock (int64 milliseconds * 1_000_000L) view)
                    world.Machine
                |> Option.get
                |> snd

            othersUnmoved pid before machine

            { world with
                Machine = machine
            }
        | Step.WakePass finishing ->
            let woken =
                SimulatedMachine.wakes (asleepByProcess world.Asleep) world.Machine
                |> List.map fst

            let world =
                { world with
                    Asleep = Set.difference world.Asleep (Set.ofList woken)
                    Woken = world.Woken @ woken
                }

            let finishing, waiting = List.splitAt (min finishing world.Woken.Length) world.Woken

            ({ world with
                Woken = waiting
             },
             finishing)
            ||> List.fold (fun world (pid, task) ->
                // A process an earlier finish ended has taken its tasks with it.
                if not (List.contains pid world.Live) then
                    world
                else

                let before = world.Machine

                let made =
                    finish coverage task (SimulatedMachine.focus pid world.Machine |> Option.get)

                match made with
                | Made.Refused ->
                    coverage.Refusals <- coverage.Refusals + 1
                    // Left woken, for a later pass.
                    { world with
                        Woken = world.Woken @ [ pid, task ]
                    }
                | made ->
                    match made with
                    | Made.Answered _ -> coverage.Finishes <- coverage.Finishes + 1
                    | _ -> ()

                    let world = settle coverage pid task made world

                    // A finish whose process's end was refused leaves the task
                    // parked, woken still, for a later pass.
                    let world =
                        match Map.tryFind pid world.Machine.Processes with
                        | Some slot when
                            (UnixTaskTable.parkedFor task slot.Tasks).IsSome
                            && not (Set.contains (pid, task) world.Asleep)
                            ->
                            { world with
                                Woken = world.Woken @ [ pid, task ]
                            }
                        | _ -> world

                    if world.Machine.Processes.ContainsKey pid then
                        othersUnmoved pid before world.Machine

                    assertClean world.Machine
                    world
            )
        | Step.Call (proc, task, op) ->
            match world.Live with
            | [] -> world
            | live ->

            let pid = live.[proc % live.Length]

            if Set.contains (pid, task) world.Asleep || List.contains (pid, task) world.Woken then
                world
            else

            coverage.Calls <- coverage.Calls + 1
            let before = world.Machine
            let view = SimulatedMachine.focus pid before |> Option.get
            let beforeFds = fdsOf view |> Map.keys |> Set.ofSeq

            let othersWakeable =
                wakeable world
                |> Map.filter (fun (owner, _) _ -> owner <> pid)
                |> Map.keys
                |> Set.ofSeq

            let world = settle coverage pid task (call coverage before pid task op view) world

            // A call in this process, its end included, that made a call
            // asleep in another wakeable.
            let newlyWakeable =
                wakeable world
                |> Map.filter (fun (owner, _ as key) _ -> owner <> pid && not (Set.contains key othersWakeable))

            if not newlyWakeable.IsEmpty then
                coverage.CrossWakes <- coverage.CrossWakes + 1

                for KeyValue (_, fired) in newlyWakeable do
                    for primitive in fired do
                        let kind = (sprintf "%A" primitive).Split(' ').[0]

                        coverage.CrossWakeKinds <-
                            coverage.CrossWakeKinds
                            |> Map.change kind (fun count -> Some (1 + Option.defaultValue 0 count))

            match Map.tryFind pid world.Machine.Processes with
            | None -> world
            | Some _ ->

            othersUnmoved pid before world.Machine

            // The caller's new descriptors are the lowest its own table had free.
            let afterFds =
                fdsOf (SimulatedMachine.focus pid world.Machine |> Option.get)
                |> Map.keys
                |> Set.ofSeq

            let added = Set.difference afterFds beforeFds

            let lowestFree =
                Seq.initInfinite id
                |> Seq.filter (fun fd -> not (Set.contains fd beforeFds))
                |> Seq.take added.Count
                |> Set.ofSeq

            added |> shouldEqual lowestFree
            world

    /// The process's listener at `port`, the flavour's event queue watching
    /// it, and `/f0` open.
    let private prepare (port : uint16) (view : UnixSystem<int, string>) : UnixSystem<int, string> =
        let listener, view = KeventWorld.listenerAt port view

        let view =
            match SimulatedUnixPlatform.flavour view.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux ->
                let epoll, view =
                    match UnixPoll.epollCreate1 0 view with
                    | Ok (Ok created) -> created
                    | other -> failwith $"epoll_create1: %A{other}"

                match
                    UnixPoll.epollCtl
                        epoll
                        1
                        listener
                        (EpollEventArgument.Readable (EpollEvents.In ||| EpollEvents.EdgeTriggered, 0UL))
                        view
                with
                | Ok (EpollCtlAnswer.Changed, view) -> view
                | other -> failwith $"epoll_ctl: %A{other}"
            | SimulatedUnixFlavour.Darwin ->
                let kq, view = KeventWorld.kqueue view

                match
                    UnixKqueue.kevent
                        0
                        kq
                        1
                        [
                            KeventWorld.change listener KeventFilter.Read (KeventFlags.Add ||| KeventFlags.Clear) 0UL
                        ]
                        0
                        UserBuffer.Mapped
                        (KeventTimeout.Readable (0L, 0L))
                        view
                with
                | Ok (KeventOutcome.Answered [], view) -> view
                | other -> failwith $"kevent: %A{other}"

        match OpenFlagWords.openPath openCreating (PathArg.ofText files.[0]) 0o644 view with
        | Ok (_, view) -> view
        | Error refusal -> failwith $"open: %A{refusal}"

    /// TCP buffers far smaller than the defaults, so that a few writes fill
    /// them: as small as each flavour admits.
    let private smallBuffers (image : UnixBootImage<int, string>) : UnixBootImage<int, string> =
        match SimulatedUnixPlatform.flavour (UnixBootImage.platform image) with
        | SimulatedUnixFlavour.Linux ->
            image
            |> UnixBootImage.withTcpSendSpaceMax (Some 16384)
            |> Configured.expectOk TcpSendSpaceMaxRefusal.describe
            |> UnixBootImage.withTcpReceiveSpace (Some 8192)
            |> Configured.expectOk TcpReceiveSpaceRefusal.describe
        | SimulatedUnixFlavour.Darwin ->
            image
            |> UnixBootImage.withTcpSendSpace (Some UnixMachineState.darwinLoopbackSendPipe)
            |> Configured.expectOk TcpSendSpaceRefusal.describe
            |> UnixBootImage.withTcpReceiveSpace (Some UnixMachineState.darwinLoopbackReceivePipe)
            |> Configured.expectOk TcpReceiveSpaceRefusal.describe

    let private run (coverage : Coverage) (case : Case) : unit =
        let pids, machine =
            Machines.withTasksOn smallBuffers case.Platform case.Processes tasks

        let machine =
            if case.Prepared then
                let machine =
                    (machine, List.indexed pids)
                    ||> List.fold (fun machine (i, pid) -> Machines.doIn pid (prepare ports.[i % ports.Length]) machine)

                // Then each process connects to the next one's listener, and
                // the next accepts, so that bytes cross between processes from
                // the first step: the connecting end blocking, so that its
                // transfers sleep, and the accepted end not.
                (machine, List.indexed pids)
                ||> List.fold (fun machine (i, client) ->
                    let next = (i + 1) % pids.Length

                    let machine =
                        Machines.doIn
                            client
                            (fun view ->
                                let fd, view = KeventWorld.client ports.[next] view
                                UnixDescriptor.setNonBlocking fd false view |> snd
                            )
                            machine

                    Machines.doIn
                        pids.[next]
                        (fun view ->
                            let listener = socketsWhere listening view |> List.head
                            let accepted, view = KeventWorld.accept listener view
                            UnixDescriptor.setNonBlocking accepted true view |> snd
                        )
                        machine
                )
            else
                machine

        assertClean machine

        let world =
            {
                Machine = machine
                Live = pids
                Asleep = Set.empty
                Woken = []
            }

        (world, case.Steps)
        ||> List.fold (fun world next ->
            let world = step coverage world next
            assertClean world.Machine
            world
        )
        |> ignore

    [<Test>]
    let ``random interleavings of several processes' calls, wakes and ends keep every invariant and touch no other process``
        ()
        : unit
        =
        let coverage =
            {
                CrossConnects = 0
                CrossAddressInUse = 0
                Accepts = 0
                CrossRmDirOfStanding = 0
                EpollAdds = 0
                KeventAdds = 0
                Parks = 0
                CrossWakes = 0
                CrossWakeKinds = Map.empty
                Finishes = 0
                DarwinPollParks = 0
                KeventParks = 0
                Exits = 0
                ExitRefusals = 0
                CrossReads = 0
                ShortWrites = 0
                Resets = 0
                EndsOfFile = 0
                ConnectionReadParks = 0
                ConnectionWriteParks = 0
                ConnectionFinishes = 0
                Refusals = 0
                Calls = 0
            }

        // 1500 cases, because the cases biased towards transfers leave fewer
        // steps for the rest, and 1000 reached the rarest of the paths below
        // too seldom for its bound to be safe.
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1500, Prop.forAll (Arb.fromGen caseGen) (run coverage))
        printfn $"%A{coverage}"

        // The paths the property exists for, each reached often enough that a
        // generator regression shows here rather than as a silently weaker
        // test: about a third of what 1000 cases reached when each was added,
        // so well under a third of what 1500 reach.
        coverage.CrossConnects |> shouldBeGreaterThan 150
        coverage.CrossAddressInUse |> shouldBeGreaterThan 150
        coverage.Accepts |> shouldBeGreaterThan 100
        coverage.CrossRmDirOfStanding |> shouldBeGreaterThan 4
        coverage.EpollAdds |> shouldBeGreaterThan 90
        coverage.KeventAdds |> shouldBeGreaterThan 180
        coverage.Parks |> shouldBeGreaterThan 1000
        coverage.DarwinPollParks |> shouldBeGreaterThan 80
        coverage.KeventParks |> shouldBeGreaterThan 120
        coverage.Finishes |> shouldBeGreaterThan 200
        coverage.Exits |> shouldBeGreaterThan 60
        coverage.CrossWakes |> shouldBeGreaterThan 100
        coverage.CrossReads |> shouldBeGreaterThan 6
        coverage.ShortWrites |> shouldBeGreaterThan 30
        coverage.Resets |> shouldBeGreaterThan 3
        coverage.EndsOfFile |> shouldBeGreaterThan 7
        coverage.ConnectionReadParks |> shouldBeGreaterThan 50
        coverage.ConnectionWriteParks |> shouldBeGreaterThan 25
        coverage.ConnectionFinishes |> shouldBeGreaterThan 20

        let crossWakes (kind : string) : int =
            Map.tryFind kind coverage.CrossWakeKinds |> Option.defaultValue 0

        crossWakes "AcceptQueueNonEmpty" |> shouldBeGreaterThan 80
        crossWakes "EpollEventDeliverable" |> shouldBeGreaterThan 10
        crossWakes "KqueueEventDeliverable" |> shouldBeGreaterThan 10
        crossWakes "KqueuePollReportable" |> shouldBeGreaterThan 4
        crossWakes "FlockGrantable" |> shouldBeGreaterThan 6
        crossWakes "DescriptorReady" |> shouldBeGreaterThan 2
        crossWakes "ConnectionReadable" |> shouldBeGreaterThan 20
        crossWakes "ConnectionWritable" |> shouldBeGreaterThan 3
        // Refusals are steps that tested nothing; they must stay rare.
        coverage.Refusals * 10 |> shouldBeSmallerThan coverage.Calls
