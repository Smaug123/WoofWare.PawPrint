namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.checkInvariants`, for the five rules nothing else exercises.
///
/// The other twelve are covered from `WoofWare.PawPrint.Test`, which is where
/// these rules' tests were written before the checker moved here; they move in
/// their own stages. These five were found by mutating each rule in turn and
/// seeing which mutants no suite killed, and three of them are the same gap
/// twice over: the `>=` against a counter is only ever tested with a strictly
/// greater identity, so the boundary the rule exists for is untested.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemInvariants =

    let private context : string = "TestUnixSystemInvariants"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private machine : UnixMachineState =
        {
            Sockets = Map.empty
            Connections = Map.empty
            NextConnectionId = ConnectionId 0L
            NextSocketEventRegistrationOrdinal = 0L
            NextEphemeralPort = 32768us
            EphemeralPortRange = (32768us, 60999us)
            SoMaxConn = 4096
            LocalAddresses = [ InternetEndpoint.LoopbackAddress ]
            LocalRoutes = []
            NextSocketId = SocketId 0L
            VirtualClockTicks = 0L
            WallClockEpochMs = 0L
            NonCryptoRandomState = 1UL
            CryptoRandomState = 1UL
            ProcessorCount = 1
            UserAddressLimit = 0x7FFFFFFFFFFFUL
            UnixPlatform = SimulatedUnixPlatform.linuxX64
            FileSystem = VirtualFileSystem.empty epoch
            FileSystemType = EmulatedFileSystemType.Tmpfs
        }

    let private processState : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.initial
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = Map.empty
            CurrentDirectory = AbsoluteUnixPath.parseOrFail context "/"
            CurrentDirectoryInode = InodeNumber 1L
            ProcessPath = None
            DirectoryStreams = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            UserId = 1000u
            GroupId = 1000u
            Umask = PermissionBits.parseOrFail context 0o022
            Signals = SignalState.empty
        }

    let private system : UnixSystem<int, string> =
        {
            Machine = machine
            Process = processState
            Tasks = Map.empty
        }

    /// A sound system is the control every row below is a single edit away
    /// from: without it, a row that reported *some* defect would pass while
    /// naming the wrong one.
    [<Test>]
    let ``the starting system is sound`` () : unit =
        UnixSystem.checkInvariants system |> shouldEqual []

    // ------------------------------------------------------------------
    // Directory streams over something that is not a directory
    // ------------------------------------------------------------------

    /// A stream whose inode is a regular file. Unreachable through `opendir`,
    /// which refuses a non-directory, so it can only be forged — but the rule
    /// is what makes `readdir`'s walk total, and a `readdir` through such a
    /// stream would crash the interpreter rather than name a cause.
    [<Test>]
    let ``a stream over a regular file is a defect`` () : unit =
        let fileInode, filesystem =
            match
                VirtualFileSystem.createFile
                    (InodeNumber 1L)
                    (FileName.parseOrFail context "f")
                    (PermissionBits.parseOrFail context 0o644)
                    epoch
                    ImmutableArray<byte>.Empty
                    system.Machine.FileSystem
            with
            | Ok result -> result
            | Error error -> failwith $"could not create the file: %O{error}"

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
                Process =
                    { system.Process with
                        DirectoryStreams =
                            Map.ofList
                                [
                                    DirectoryStreamId 0L,
                                    {
                                        Fd = 3
                                        Inode = fileInode
                                        Cursor = DirectoryCursor.Start
                                    }
                                ]
                        NextDirectoryStreamId = DirectoryStreamId 1L
                    }
            }

        UnixSystem.checkInvariants forged
        |> shouldEqual
            [
                UnixSystemDefect.DirectoryStreamIsNotADirectory (DirectoryStreamId 0L, fileInode)
            ]

    /// A stream whose inode the filesystem no longer holds. Unreachable by
    /// construction, since `UnixProcessState.heldInodes` counts a stream's inode
    /// among the things pinning it — which is exactly why a violation is an
    /// interpreter bug, and why nothing but a forged state can reach the rule.
    [<Test>]
    let ``a stream over a freed inode is a defect`` () : unit =
        let absent = InodeNumber 99L

        let forged =
            { system with
                Process =
                    { system.Process with
                        DirectoryStreams =
                            Map.ofList
                                [
                                    DirectoryStreamId 0L,
                                    {
                                        Fd = 3
                                        Inode = absent
                                        Cursor = DirectoryCursor.Start
                                    }
                                ]
                        NextDirectoryStreamId = DirectoryStreamId 1L
                    }
            }

        UnixSystem.checkInvariants forged
        |> shouldEqual [ UnixSystemDefect.DanglingDirectoryStreamInode (DirectoryStreamId 0L, absent) ]

    // ------------------------------------------------------------------
    // The phase/kind rule, in the direction the other test does not take
    // ------------------------------------------------------------------

    /// The mismatch rule has two halves — a datagram socket in a stream phase,
    /// and a stream socket holding a datagram peer — written as two arms of one
    /// match. A test for either half alone leaves the other's arm free to say
    /// anything.
    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``a stream socket holding a datagram peer is a defect`` (connectionOriented : bool) : unit =
        let kind =
            if connectionOriented then
                SocketKind.Stream
            else
                SocketKind.Raw

        let phase =
            SocketPhase.DatagramPeer (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 80us)

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.ofList
                                [
                                    SocketId 0L,
                                    {
                                        Domain = SocketDomain.InterNetwork
                                        Kind = kind
                                        Protocol = SocketProtocol.Tcp
                                        Binding = None
                                        ReuseAddress = false
                                        Phase = phase
                                    }
                                ]
                        NextSocketId = SocketId 1L
                    }
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.Unchecked.ofParts
                                (Map.ofList [ 3, OpenFileDescriptionId 0L ])
                                (Map.ofList
                                    [
                                        OpenFileDescriptionId 0L,
                                        {
                                            Target = OpenFileTarget.Socket (SocketId 0L)
                                            AccessMode = FileAccessMode.ReadWrite
                                            NonBlocking = false
                                            Flock = None
                                        }
                                    ])
                                (OpenFileDescriptionId 1L)
                    }
            }

        UnixSystem.checkInvariants forged
        |> shouldEqual [ UnixSystemDefect.SocketPhaseKindMismatch (SocketId 0L, kind, phase) ]

    // ------------------------------------------------------------------
    // The counters, at the boundary rather than past it
    // ------------------------------------------------------------------

    /// `NextConnectionId` equal to a live connection's identity, which is the
    /// state the rule exists for: the next connect mints that identity again
    /// and the two connections become one. A counter strictly *below* a live
    /// identity, which is what the fixture in `WoofWare.PawPrint.Test` forges,
    /// is caught by a strict comparison too.
    [<Test>]
    let ``NextConnectionId equal to a live connection is a defect`` () : unit =
        let connection = ConnectionId 2L

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.ofList
                                [
                                    SocketId 0L,
                                    {
                                        Domain = SocketDomain.InterNetwork
                                        Kind = SocketKind.Stream
                                        Protocol = SocketProtocol.Tcp
                                        Binding = None
                                        ReuseAddress = false
                                        Phase = SocketPhase.Established connection
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
                                        ServerAddress = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 80us
                                    }
                                ]
                        NextConnectionId = connection
                    }
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.Unchecked.ofParts
                                (Map.ofList [ 3, OpenFileDescriptionId 0L ])
                                (Map.ofList
                                    [
                                        OpenFileDescriptionId 0L,
                                        {
                                            Target = OpenFileTarget.Socket (SocketId 0L)
                                            AccessMode = FileAccessMode.ReadWrite
                                            NonBlocking = false
                                            Flock = None
                                        }
                                    ])
                                (OpenFileDescriptionId 1L)
                    }
            }

        UnixSystem.checkInvariants forged
        |> shouldEqual [ UnixSystemDefect.NextConnectionIdNotFresh (connection, connection) ]

    /// A registration stamped with the ordinal the counter is about to mint, so
    /// the next ADD repeats it. The ordinal's whole job is to order same-signal
    /// ties, which a repeat leaves unspecified. Same boundary as the connection
    /// counter above, and untested for the same reason.
    [<Test>]
    let ``a registration ordinal equal to the next to mint is a defect`` () : unit =
        let portId = OpenFileDescriptionId 0L
        let ordinal = 7L

        let port =
            {
                Registrations =
                    Map.ofList
                        [
                            (3, OpenFileDescriptionId 1L),
                            {
                                Interest =
                                    {
                                        In = true
                                        Out = false
                                        RdHup = false
                                    }
                                Data = 0UL
                                RegisteredAt = ordinal
                            }
                        ]
                Ready = []
            }

        let forged =
            { system with
                Machine =
                    { system.Machine with
                        NextSocketEventRegistrationOrdinal = ordinal
                    }
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.Unchecked.ofParts
                                (Map.ofList [ 4, portId ])
                                (Map.ofList
                                    [
                                        portId,
                                        {
                                            Target = OpenFileTarget.SocketEventPort port
                                            AccessMode = FileAccessMode.ReadWrite
                                            NonBlocking = false
                                            Flock = None
                                        }
                                    ])
                                (OpenFileDescriptionId 1L)
                    }
            }

        UnixSystem.checkInvariants forged
        |> shouldEqual
            [
                UnixSystemDefect.SocketEventRegistrationOrdinalNotFresh (ordinal, portId, ordinal)
            ]
