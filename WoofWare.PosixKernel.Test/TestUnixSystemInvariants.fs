namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.checkInvariants`, for the six rules nothing else exercises.
///
/// Five of these six were found by mutating each rule in turn and seeing which
/// mutants no suite killed, and three of those are the same gap twice over: the
/// `>=` against a counter is only ever tested with a strictly greater identity,
/// so the boundary the rule exists for is untested.
/// The sixth, `CurrentDirectoryIsNotADirectory`, arrived with the setter that
/// establishes the current directory.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemInvariants =

    let private context : string = "TestUnixSystemInvariants"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// A sound Linux system, before anything has happened to it. One flavour,
    /// because every rule below is about the tables rather than about the
    /// platform.
    let private system : UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }


    /// `system` launched with no descriptors at all, and so no pipes: the base
    /// for a row that replaces the whole descriptor table.
    let private unlaunched : UnixSystem<int, string> =
        let bare : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64 Map.empty 0 (CpuId 0)
            |> UnixBootImage.boot

        { bare with
            Machine =
                { bare.Machine with
                    LocalRoutes = []
                }
        }

    /// A sound system is the control every row below is a single edit away
    /// from: without it, a row that reported *some* defect would pass while
    /// naming the wrong one.
    [<Test>]
    let ``the starting system is sound`` () : unit =
        UnixSystem.checkInvariants system |> shouldEqual []

    // ------------------------------------------------------------------
    // The phase/kind rule, in the direction the other test does not take
    // ------------------------------------------------------------------

    /// The mismatch rule has two halves — a datagram socket in a stream phase,
    /// and a stream socket holding a datagram peer — written as two arms of one
    /// match. A test for either half alone leaves the other's arm free to say
    /// anything.
    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``a stream socket holding a datagram peer is a defect`` (stream : bool) : unit =
        let kind = if stream then SocketKind.Stream else SocketKind.SeqPacket

        let phase =
            SocketPhase.DatagramPeer (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 80us)

        let forged =
            (UnixSystemState.withFileDescriptors
                (FileDescriptorRegistry.Unchecked.ofParts
                    (Map.ofList [ 3, OpenFileDescriptionId 0L ])
                    (Map.ofList
                        [
                            OpenFileDescriptionId 0L,
                            {
                                Target = OpenFileTarget.Socket (SocketId 0L)
                                AccessMode = FileAccessMode.ReadWrite
                                NonBlocking = false
                                Flock = None
                                Status = OpenFileStatus.none
                            }
                        ])
                    (OpenFileDescriptionId 1L))
                { unlaunched with
                    Machine =
                        { unlaunched.Machine with
                            Sockets =
                                Map.ofList
                                    [
                                        SocketId 0L,
                                        {
                                            Domain = SocketDomain.Inet
                                            Kind = kind
                                            Protocol = SocketProtocol.Tcp
                                            Binding = None
                                            ReuseAddress = false
                                            Options = SocketOptions.initial
                                            Phase = phase
                                        }
                                    ]
                            NextSocketId = SocketId 1L
                        }
                })

        UnixSystem.checkInvariants forged
        |> shouldEqual [ UnixSystemDefect.SocketPhaseKindMismatch (SocketId 0L, kind, phase) ]

    // ------------------------------------------------------------------
    // The counters, at the boundary rather than past it
    // ------------------------------------------------------------------

    /// `NextConnectionId` equal to a live connection's identity, which is the
    /// state the rule exists for: the next connect mints that identity again
    /// and the two connections become one. A counter strictly *below* a live
    /// identity is caught by a strict comparison too.
    [<Test>]
    let ``NextConnectionId equal to a live connection is a defect`` () : unit =
        let connection = ConnectionId 2L

        let forged =
            (UnixSystemState.withFileDescriptors
                (FileDescriptorRegistry.Unchecked.ofParts
                    (Map.ofList [ 3, OpenFileDescriptionId 0L ])
                    (Map.ofList
                        [
                            OpenFileDescriptionId 0L,
                            {
                                Target = OpenFileTarget.Socket (SocketId 0L)
                                AccessMode = FileAccessMode.ReadWrite
                                NonBlocking = false
                                Flock = None
                                Status = OpenFileStatus.none
                            }
                        ])
                    (OpenFileDescriptionId 1L))
                { unlaunched with
                    Machine =
                        { unlaunched.Machine with
                            Sockets =
                                Map.ofList
                                    [
                                        SocketId 0L,
                                        {
                                            Domain = SocketDomain.Inet
                                            Kind = SocketKind.Stream
                                            Protocol = SocketProtocol.Tcp
                                            Binding = None
                                            ReuseAddress = false
                                            Options = SocketOptions.initial
                                            Phase = SocketPhase.Established (connection, ConnectionEnd.Client)
                                        }
                                    ]
                            NextSocketId = SocketId 1L
                            Connections =
                                Map.ofList
                                    [
                                        connection,
                                        {
                                            ClientAddress =
                                                InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 40000us
                                            ServerAddress =
                                                InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 80us
                                            Transfer = TcpBufferSizing.newTransfer SocketDomain.Inet unlaunched.Machine
                                        }
                                    ]
                            NextConnectionId = connection
                        }
                })

        UnixSystem.checkInvariants forged
        |> shouldEqual [ UnixSystemDefect.NextConnectionIdNotFresh (connection, connection) ]

    /// A registration stamped with the ordinal the counter is about to mint, so
    /// the next ADD repeats it. The ordinal's whole job is to order same-signal
    /// ties, which a repeat leaves unspecified. Same boundary as the connection
    /// counter above, and untested for the same reason.
    [<Test>]
    let ``a registration ordinal equal to the next to mint is a defect`` () : unit =
        let queueId = OpenFileDescriptionId 0L
        let ordinal = 7L

        let queueState =
            {
                Registrations =
                    Map.ofList
                        [
                            (3, OpenFileDescriptionId 1L),
                            {
                                Events =
                                    EpollEvents.In
                                    ||| EpollEvents.Err
                                    ||| EpollEvents.Hup
                                    ||| EpollEvents.EdgeTriggered
                                Data = 0UL
                                RegisteredAt = ordinal
                            }
                        ]
                Ready = []
            }

        let forged =
            (UnixSystemState.withFileDescriptors
                (FileDescriptorRegistry.Unchecked.ofParts
                    (Map.ofList [ 4, queueId ])
                    (Map.ofList
                        [
                            queueId,
                            {
                                Target = OpenFileTarget.Epoll queueState
                                AccessMode = FileAccessMode.ReadWrite
                                NonBlocking = false
                                Flock = None
                                Status = OpenFileStatus.none
                            }
                        ])
                    (OpenFileDescriptionId 1L))
                { unlaunched with
                    Machine =
                        { unlaunched.Machine with
                            NextEventRegistrationOrdinal = ordinal
                        }
                })

        UnixSystem.checkInvariants forged
        |> shouldEqual
            [
                UnixSystemDefect.EventRegistrationOrdinalNotFresh (ordinal, queueId, ordinal)
            ]

    // ------------------------------------------------------------------
    // A current directory that is not a directory
    // ------------------------------------------------------------------

    /// A system standing in `/outer/inner`, from which both rows below are a
    /// single edit away.
    let private standing : UnixSystem<int, string> =
        let seed =
            Map.ofList
                [
                    DirectoryEntryName.parseOrFail context "outer",
                    SeedEntry.directory (
                        Map.ofList
                            [
                                DirectoryEntryName.parseOrFail context "inner", SeedEntry.directory FileSystemSeed.empty
                                DirectoryEntryName.parseOrFail context "file", SeedEntry.file ImmutableArray<byte>.Empty
                            ]
                    )
                ]

        match
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                Owners.linuxDefault
                seed
                (AbsoluteUnixPath.parseOrFail context "/outer/inner")
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"the fixture's own seed did not boot: %O{fault}."

    let private inodeOf (path : string) : InodeNumber =
        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits standing.Machine.UnixPlatform)
                Owners.root
                SymlinkProtection.Off
                (VirtualFileSystem.root standing.Machine.FileSystem)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail context path)
                standing.Machine.FileSystem
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}."

    [<Test>]
    let ``a current directory that is a regular file is a defect`` () : unit =
        let file = inodeOf "/outer/file"

        { standing with
            Process =
                { standing.Process with
                    CurrentDirectoryInode = file
                }
        }
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.CurrentDirectoryIsNotADirectory file ]

    [<Test>]
    let ``a current directory the filesystem does not contain is a defect`` () : unit =
        // The same rule reached by the other input: an inode with no content at
        // all reads as "not a directory" rather than as its own defect, which is
        // what lets one rule cover both. A row asserting only the file case
        // would leave a checker that special-cased `Some` passing.
        let absent = VirtualFileSystem.nextInode standing.Machine.FileSystem

        { standing with
            Process =
                { standing.Process with
                    CurrentDirectoryInode = absent
                }
        }
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.CurrentDirectoryIsNotADirectory absent ]

    // ------------------------------------------------------------------
    // Tasks against the descriptor table
    // ------------------------------------------------------------------

    let private task : int = 1

    /// `system` with one registered task, parked as `parked` says.
    let private withTask (parked : ParkedSyscall option) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let registered = Tasks.ensure task system

        match parked with
        | None -> registered
        | Some parked -> UnixWait.park task parked registered

    /// `system` with one registered task, parked as `parked` says on a
    /// description the open file table may not hold, which no syscall parks on.
    let private withForgedTask (parked : ParkedSyscall) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        Tasks.ensure task system |> ForgedPark.onAbsent task parked

    /// A description id nothing in `system` holds.
    let private absentDescription : OpenFileDescriptionId = OpenFileDescriptionId 999L

    /// `system` with the description stdin names, and every descriptor naming
    /// it, gone from the descriptor table but for the description itself.
    let private stdinUnnamed (system : UnixSystem<int, string>) : OpenFileDescriptionId * UnixSystem<int, string> =
        let stdin =
            FileDescriptorRegistry.tryFindId 0 (UnixSystemState.fileDescriptors system)
            |> Option.get

        // Held while the descriptor goes, so that the description survives
        // it, and let go of afterwards, so that nothing references it.
        let registry =
            match
                FileDescriptorRegistry.dropDescriptor
                    system.Process.ProcessId
                    0
                    (UnixSystemState.fileDescriptors system
                     |> FileDescriptorRegistry.mapOpenFiles (OpenFileTable.hold stdin))
            with
            | Ok (registry, None) -> FileDescriptorRegistry.mapOpenFiles (OpenFileTable.releaseHold stdin) registry
            | other -> failwith $"expected the description to survive, got %A{other}"

        stdin, UnixSystemState.withFileDescriptors registry system

    [<Test>]
    let ``a description nothing references is a defect, and one a parked call holds is not`` () : unit =
        let stdin, unnamed = stdinUnnamed system

        UnixSystem.checkInvariants unnamed
        |> shouldEqual [ UnixSystemDefect.UnreferencedDescription stdin ]

        // Held by a read asleep on it, as a read of the launched pipe's empty
        // read end would be.
        unnamed
        |> withTask (
            Some (
                ParkedSyscall.PipeRead
                    {
                        Reader = SleepTarget.Waiting (stdin, 0)
                        Buffer = UserBuffer.Mapped
                        Count = 1
                    }
            )
        )
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``a task parked on an flock of an absent description is a defect`` () : unit =
        system
        |> withForgedTask (
            ParkedSyscall.Flock
                {
                    Requester = absentDescription
                    Mode = FlockMode.Exclusive
                }
        )
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.ParkedOnAbsentDescription (task, absentDescription) ]

    [<Test>]
    let ``a task parked in an epoll_wait on an absent description is a defect`` () : unit =
        system
        |> withForgedTask (
            ParkedSyscall.EpollWait
                {
                    Epoll = absentDescription
                    MaxEvents = 1
                    Buffer = UserBuffer.Mapped
                    Deadline = None
                }
        )
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.ParkedOnAbsentDescription (task, absentDescription) ]

    [<Test>]
    let ``a task parked in an epoll_wait on a description that is not an epoll instance is a defect`` () : unit =
        // stdout, which every system holds and which is not an epoll instance.
        let stdoutDescription, target =
            match FileDescriptorRegistry.tryFindWithId 1 (UnixSystemState.fileDescriptors system) with
            | Some (id, description) -> id, description.Target
            | None -> failwith "the fixture has no stdout"

        system
        |> withTask (
            Some (
                ParkedSyscall.EpollWait
                    {
                        Epoll = stdoutDescription
                        MaxEvents = 1
                        Buffer = UserBuffer.Mapped
                        Deadline = None
                    }
            )
        )
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.ParkedEpollWaitOnNonEpoll (task, stdoutDescription, target) ]

    /// `system` with tasks 1 and 2 parked on stdout's description, in that order.
    let private twoParked : UnixSystem<int, string> =
        let stdoutDescription =
            match FileDescriptorRegistry.tryFindWithId 1 (UnixSystemState.fileDescriptors system) with
            | Some (id, _) -> id
            | None -> failwith "the fixture has no stdout"

        let parked =
            ParkedSyscall.Flock
                {
                    Requester = stdoutDescription
                    Mode = FlockMode.Shared
                }

        system
        |> Tasks.ensure 1
        |> Tasks.ensure 2
        |> UnixWait.park 1 parked
        |> UnixWait.park 2 parked

    /// `system` with task `name`'s park stamped `ordinal`, as only a record copy past
    /// `UnixWait.park` could.
    let private forgeOrdinal (name : int) (ordinal : ParkOrdinal) (system : UnixSystem<int, string>) =
        let task = UnixTaskTable.get name system.Tasks

        { system with
            Tasks =
                system.Tasks
                |> Map.add
                    name
                    { task with
                        Parked =
                            task.Parked
                            |> Option.map (fun park ->
                                { park with
                                    Ordinal = ordinal
                                }
                            )
                    }
        }

    [<Test>]
    let ``parks minted by UnixWait are sound`` () : unit =
        UnixSystem.checkInvariants twoParked |> shouldEqual []

    [<Test>]
    let ``two parks stamped with one ordinal are a defect`` () : unit =
        let first = (UnixTaskTable.parkOf 1 twoParked.Tasks |> Option.get).Ordinal

        forgeOrdinal 2 first twoParked
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.DuplicateParkOrdinal first ]

    [<Test>]
    let ``a park stamped at or past the next ordinal is a defect`` () : unit =
        let next = twoParked.Machine.NextParkOrdinal

        forgeOrdinal 2 next twoParked
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.ParkOrdinalNotFresh (next, 2, next) ]

    /// The control for the three rows above: a park onto a live object of the
    /// right kind is sound, so those rows are not passing because every park
    /// is reported.
    [<Test>]
    let ``a task parked on a live epoll instance or file is sound`` () : unit =
        let queueFd, registry =
            FileDescriptorRegistry.createEpoll (UnixSystemState.fileDescriptors system)

        let queueId =
            match FileDescriptorRegistry.tryFindWithId queueFd registry with
            | Some (id, _) -> id
            | None -> failwith "the epoll instance just created is not in the table"

        let stdoutDescription =
            match FileDescriptorRegistry.tryFindWithId 1 registry with
            | Some (id, _) -> id
            | None -> failwith "the fixture has no stdout"

        let queueSystem = UnixSystemState.withFileDescriptors registry system

        queueSystem
        |> withTask (
            Some (
                ParkedSyscall.EpollWait
                    {
                        Epoll = queueId
                        MaxEvents = 1
                        Buffer = UserBuffer.Mapped
                        Deadline = None
                    }
            )
        )
        |> UnixSystem.checkInvariants
        |> shouldEqual []

        queueSystem
        |> withTask (
            Some (
                ParkedSyscall.Flock
                    {
                        Requester = stdoutDescription
                        Mode = FlockMode.Shared
                    }
            )
        )
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    // ------------------------------------------------------------------
    // Sockets whose binding cannot have come from bind or listen
    // ------------------------------------------------------------------

    let private streamSocket (binding : SocketBinding option) (phase : SocketPhase) : SocketDescription =
        {
            Domain = SocketDomain.Inet
            Kind = SocketKind.Stream
            Protocol = SocketProtocol.Tcp
            Binding = binding
            ReuseAddress = false
            Options = SocketOptions.initial
            Phase = phase
        }

    /// `system` holding one socket, open on a descriptor so that the socket
    /// table's own rules are satisfied.
    let private withSocket (socket : SocketDescription) : SocketId * UnixSystem<int, string> =
        let socketId = system.Machine.NextSocketId
        let (SocketId raw) = socketId

        let _, registry =
            FileDescriptorRegistry.createSocket socketId (UnixSystemState.fileDescriptors system)

        socketId,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
        }
        |> UnixSystemState.withFileDescriptors registry

    let private boundAt (port : uint16) : SocketBinding option =
        Some
            {
                Endpoint = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port
                LockedAddress = None
                LockedPort = false
            }

    let private listening : SocketPhase =
        SocketPhase.Listening
            {
                Backlog = 8
                Queue = []
                Drained = false
            }

    [<Test>]
    let ``a listener without a binding is a defect`` () : unit =
        let socketId, faulty = withSocket (streamSocket None listening)

        UnixSystem.checkInvariants faulty
        |> shouldEqual [ UnixSystemDefect.ListenerWithoutBinding socketId ]

        // The control: a bound listener is sound.
        let _, sound = withSocket (streamSocket (boundAt 5000us) listening)
        UnixSystem.checkInvariants sound |> shouldEqual []

    [<Test>]
    let ``a socket bound to port 0 is a defect`` () : unit =
        let socketId, faulty = withSocket (streamSocket (boundAt 0us) SocketPhase.Idle)

        UnixSystem.checkInvariants faulty
        |> shouldEqual [ UnixSystemDefect.BoundToPortZero socketId ]

        let _, sound = withSocket (streamSocket (boundAt 1us) SocketPhase.Idle)
        UnixSystem.checkInvariants sound |> shouldEqual []

    // ------------------------------------------------------------------
    // Signals against the task table
    // ------------------------------------------------------------------

    let private withSignals
        (signals : SignalState<int, string>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    Signals = signals
                }
        }

    [<Test>]
    let ``handler frames for a task the table does not hold are a defect`` () : unit =
        // Task 42 in a handler, in a state that is then given to a system
        // without it.
        let inHandler (inside : int) : SignalState<int, string> =
            SignalState.initial SignalNumbering.Linux Set.empty
            |> HandlerFrames.enter "h" 0 (Set.ofList [ 0 ; inside ]) inside (Set.singleton Signal.SIGINT)

        system
        |> withTask None
        |> withSignals (inHandler 42)
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.HandlerFramesWithoutTask 42 ]

        system
        |> withTask None
        |> withSignals (inHandler task)
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``a pending signal directed at a task the table does not hold is a defect`` () : unit =
        let directedAt (target : int voption) : SignalState<int, string> =
            SignalState.initial SignalNumbering.Linux Set.empty
            |> SignalState.enqueue
                {
                    Signal = Signal.SIGCHLD
                    Target = target
                }

        system
        |> withTask None
        |> withSignals (directedAt (ValueSome 43))
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.PendingSignalTargetWithoutTask (43, Signal.SIGCHLD) ]

        // Directed at a live task, or at the process: sound.
        system
        |> withTask None
        |> withSignals (directedAt (ValueSome task))
        |> UnixSystem.checkInvariants
        |> shouldEqual []

        system
        |> withTask None
        |> withSignals (directedAt ValueNone)
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``a signal state reading signals under a foreign numbering is a defect`` () : unit =
        // Reachable only by assembling the state by hand: `initial` derives
        // the signal state's numbering from the platform it is given.
        system
        |> withSignals (SignalState.initial SignalNumbering.Darwin Set.empty)
        |> UnixSystem.checkInvariants
        |> shouldEqual
            [
                UnixSystemDefect.SignalNumberingMismatch (SignalNumbering.Darwin, SignalNumbering.Linux)
            ]

        // The control: the numbering `initial` derived is sound.
        system
        |> withSignals (SignalState.initial SignalNumbering.Linux Set.empty)
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``a mount the flavour cannot report is a defect`` () : unit =
        // Reachable only by assembling the record by hand: `UnixSystem.initial`
        // derives the type from the flavour, and
        // `UnixBootImage.withMount` refuses one the machine's
        // flavour cannot mount.
        let linux =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        { linux with
            Machine =
                { linux.Machine with
                    Mount = EmulatedMount.Apfs ApfsMount.defaults
                }
        }
        |> UnixSystem.checkInvariants
        |> shouldEqual
            [
                UnixSystemDefect.FileSystemTypeNotReportable (SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Apfs)
            ]

        // A hand-built machine whose pair does describe one system is sound.
        UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot
        |> fun darwin ->
            { darwin with
                Machine =
                    { darwin.Machine with
                        Mount = EmulatedMount.Nfs
                    }
            }
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    // ------------------------------------------------------------------
    // Thread IDs. Every one of these is reachable only by a record copy past
    // `UnixSystem.initial`, `UnixTaskLifecycle.spawn` and the identity setters.

    let private spawned (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        match
            UnixTaskLifecycle.spawn
                0
                1
                (CpuId 0)
                (UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                 |> UnixBootImage.boot)
        with
        | Ok (SpawnAnswer.Spawned _, system) -> system
        | Ok (SpawnAnswer.Failed error, _) -> failwith $"spawn failed: %O{error}"
        | Error refusal -> failwith $"spawn was refused: %s{SpawnRefusal.describe refusal}"

    [<Test>]
    let ``a leader that is not a task is a defect`` () : unit =
        let system = spawned SimulatedUnixPlatform.linuxX64

        { system with
            Leader = 7
        }
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.LeaderWithoutTask 7 ]

    [<Test>]
    let ``two tasks with one thread ID are a defect`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let system = spawned platform
            let leader = UnixTaskTable.get 0 system.Tasks

            { system with
                Tasks =
                    Map.add
                        1
                        { UnixTaskTable.get 1 system.Tasks with
                            OsThreadId = leader.OsThreadId
                        }
                        system.Tasks
            }
            |> UnixSystem.checkInvariants
            |> shouldEqual
                [
                    UnixSystemDefect.DuplicateOsThreadId (leader.OsThreadId, [ 0 ; 1 ])
                    // Task 1's own ID, which the allocator still records, is
                    // held by no task now.
                    UnixSystemDefect.LiveThreadIdsMismatch (
                        Set.singleton (UnixTaskTable.osThreadIdOf 1 system.Tasks),
                        Set.empty
                    )
                ]

    [<Test>]
    let ``on Linux a leader whose thread ID is not the process ID is a defect, and on Darwin it is not`` () : unit =
        let moved (system : UnixSystem<int, string>) : UnixSystem<int, string> =
            { system with
                Process =
                    { system.Process with
                        ProcessId = ProcessId.parseOrFail "test" 17
                    }
            }

        let linux = spawned SimulatedUnixPlatform.linuxX64

        moved linux
        |> UnixSystem.checkInvariants
        |> shouldEqual
            [
                UnixSystemDefect.LeaderThreadIdNotProcessId (
                    0,
                    UnixTaskTable.osThreadIdOf 0 linux.Tasks,
                    ProcessId.parseOrFail "test" 17
                )
            ]

        moved (spawned SimulatedUnixPlatform.macOsArm64)
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``a thread ID the counter could not have handed out is a defect`` () : unit =
        // Linux: a live tid no pid_max admits, by taking one from a Darwin
        // machine. A tid at or above the pid_max now in force is not one, because
        // the administrator may lower pid_max beneath live ids.
        let linux = spawned SimulatedUnixPlatform.linuxX64

        linux
        |> UnixSystem.writePidMaxSysctl "test" 1000
        |> UnixSystem.checkInvariants
        |> shouldEqual []

        let withDarwinId (id : uint64) : OsThreadId * UnixSystem<int, string> =
            let foreign =
                (UnixSystem.initial<int, string>
                    SimulatedUnixPlatform.macOsArm64
                    UnixSystem.pipedStandardStreams
                    0
                    (CpuId 0)
                 |> UnixBootImage.withLeaderThreadId "test" id
                 |> UnixBootImage.boot)
                    .Tasks
                |> UnixTaskTable.osThreadIdOf 0

            let replaced = UnixTaskTable.osThreadIdOf 1 linux.Tasks

            foreign,
            { linux with
                Machine =
                    { linux.Machine with
                        // Recording the foreign ID as live in place of the one it
                        // replaces, so that what the counter could have minted is
                        // all that is wrong.
                        ThreadIds =
                            { linux.Machine.ThreadIds with
                                Live = linux.Machine.ThreadIds.Live |> Set.remove replaced |> Set.add foreign
                            }
                    }
                Tasks =
                    Map.add
                        1
                        { UnixTaskTable.get 1 linux.Tasks with
                            OsThreadId = foreign
                        }
                        linux.Tasks
            }

        let _, below = withDarwinId 4194303UL
        UnixSystem.checkInvariants below |> shouldEqual []

        let foreign, at = withDarwinId 4194304UL

        UnixSystem.checkInvariants at
        |> shouldEqual [ UnixSystemDefect.OsThreadIdNotMintable (1, foreign, at.Machine.ThreadIds) ]

        // Darwin: an id the counter has not reached, which it would hand out again.
        let darwin = spawned SimulatedUnixPlatform.macOsArm64

        let behind =
            (UnixSystem.initial<int, string>
                SimulatedUnixPlatform.macOsArm64
                UnixSystem.pipedStandardStreams
                0
                (CpuId 0)
             |> UnixBootImage.withLeaderThreadId "test" 4242UL
             |> UnixBootImage.boot)
                .Machine.ThreadIds
            |> fun behind ->
                { behind with
                    Live = darwin.Machine.ThreadIds.Live
                }

        { darwin with
            Machine =
                { darwin.Machine with
                    ThreadIds = behind
                }
        }
        |> UnixSystem.checkInvariants
        |> shouldEqual
            [
                UnixSystemDefect.OsThreadIdNotMintable (1, UnixTaskTable.osThreadIdOf 1 darwin.Tasks, behind)
            ]

    [<Test>]
    let ``a thread ID counter of the other flavour is a defect`` () : unit =
        let linux =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let darwin =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let swapped (system : UnixSystem<int, string>) (from : UnixSystem<int, string>) =
            { system with
                Machine =
                    { system.Machine with
                        ThreadIds = from.Machine.ThreadIds
                    }
            }
            |> UnixSystem.checkInvariants

        // Both leaders are 4242 and both counters are past it, so the flavour is all
        // that is wrong.
        swapped linux darwin
        |> shouldEqual
            [
                UnixSystemDefect.ThreadIdAllocatorNotOfFlavour (SimulatedUnixFlavour.Linux, darwin.Machine.ThreadIds)
            ]

        swapped darwin linux
        |> shouldEqual
            [
                UnixSystemDefect.ThreadIdAllocatorNotOfFlavour (SimulatedUnixFlavour.Darwin, linux.Machine.ThreadIds)
            ]
