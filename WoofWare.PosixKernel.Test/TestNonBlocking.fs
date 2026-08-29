namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.setNonBlocking`, `UnixSystem.isNonBlocking` and
/// `UnixSystem.createSocket`.
///
/// The flag's whole subtlety is *where it lives* and *which targets may carry
/// it*: it is a property of the open file description rather than of the
/// descriptor, one target refuses it, and one stores it while reporting a
/// failure — which is a flavour split no guest can reach, since a guest runs one
/// flavour and the managed surface never sets the flag on an event port.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNonBlocking =

    let private context : string = "TestNonBlocking"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private machineOn (platform : SimulatedUnixPlatform) : UnixMachineState =
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
            UnixPlatform = platform
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

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        {
            Machine = machineOn platform
            Process = processState
            Tasks = Map.empty
        }

    let private linux : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.linuxX64

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private setOrFail
        (fd : int)
        (value : bool)
        (system : UnixSystem<int, string>)
        : SetNonBlockingAnswer * UnixSystem<int, string>
        =
        match UnixSystem.setNonBlocking fd value system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{SetNonBlockingRefusal.describe refusal}"

    let private set (fd : int) (value : bool) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match setOrFail fd value system with
        | SetNonBlockingAnswer.Set, system -> system
        | SetNonBlockingAnswer.Failed error, _ -> failwith $"expected the flag to be set, got %O{error}"

    // ------------------------------------------------------------------
    // createSocket
    // ------------------------------------------------------------------

    /// The socket and its descriptor are minted together, and the identity the
    /// one mints is the identity the other names.
    [<TestCaseSource(nameof platforms)>]
    let ``a created socket and its descriptor agree`` (platform : SimulatedUnixPlatform) : unit =
        let system = systemOn platform

        let fd, system =
            UnixSystem.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp system

        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"expected a socket target, got %A{other}"

        let socket = UnixMachineState.socket socketId system.Machine
        socket.Domain |> shouldEqual SocketDomain.InterNetwork
        socket.Kind |> shouldEqual SocketKind.Stream
        socket.Protocol |> shouldEqual SocketProtocol.Tcp

        // A fresh socket is unbound, idle, and carries no reuse flag; all three
        // are what every later screen keys on.
        socket.Binding |> shouldEqual None
        socket.Phase |> shouldEqual SocketPhase.Idle
        socket.ReuseAddress |> shouldEqual false

        // ...and it is not born non-blocking.
        UnixSystem.isNonBlocking fd system |> shouldEqual (Some false)

    /// Each socket gets its own identity: the counter advances, so a second
    /// socket cannot overwrite the first in the table.
    [<TestCaseSource(nameof platforms)>]
    let ``each created socket gets a fresh identity`` (platform : SimulatedUnixPlatform) : unit =
        let first, system =
            UnixSystem.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp (systemOn platform)

        let second, system =
            UnixSystem.createSocket SocketDomain.InterNetwork SocketKind.Datagram SocketProtocol.Udp system

        first |> shouldNotEqual second
        system.Machine.Sockets |> Map.count |> shouldEqual 2

        let targetOf fd =
            FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors

        targetOf first |> shouldNotEqual (targetOf second)

    // ------------------------------------------------------------------
    // Where the flag lives
    // ------------------------------------------------------------------

    /// The flag is a property of the open file *description*, where POSIX keeps
    /// the status flags — so a `dup` sees it, and setting it through either
    /// number is the same act.
    [<Test>]
    let ``the flag lives on the description, so a dup shares it`` () : unit =
        let fd, system =
            UnixSystem.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp linux

        let duplicate, registry =
            match FileDescriptorRegistry.dup fd system.Process.FileDescriptors with
            | Ok result -> result
            | Error error -> failwith $"could not dup: %A{error}"

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        let system = set fd true system
        UnixSystem.isNonBlocking duplicate system |> shouldEqual (Some true)

        // ...and clearing it through the *other* number clears it for both.
        let system = set duplicate false system
        UnixSystem.isNonBlocking fd system |> shouldEqual (Some false)

    [<Test>]
    let ``a descriptor that is not open has no flag and cannot be set`` () : unit =
        UnixSystem.isNonBlocking 99 linux |> shouldEqual None

        setOrFail 99 true linux
        |> fst
        |> shouldEqual (SetNonBlockingAnswer.Failed UnixError.EBADF)

    // ------------------------------------------------------------------
    // Which targets may carry it
    // ------------------------------------------------------------------

    /// A regular file and a socket both take it. Both kernels give `O_NONBLOCK`
    /// no effect on a regular file, so an operation that never looks at it is
    /// right not to — storing it is still what a real `fcntl` does.
    [<Test>]
    let ``a file and a socket both take the flag`` () : unit =
        let socketFd, system =
            UnixSystem.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp linux

        let fileFd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        for fd in [ socketFd ; fileFd ] do
            let after = set fd true system
            UnixSystem.isNonBlocking fd after |> shouldEqual (Some true)

    /// A standard stream refuses to be *set*, because no modelled stream
    /// transfer consults the flag and storing it would keep blocking semantics
    /// silently.
    [<Test>]
    let ``setting the flag on a standard stream is refused`` () : unit =
        let rows =
            [
                0, FileDescriptorRole.StandardInput
                1, FileDescriptorRole.StandardOutput
                2, FileDescriptorRole.StandardError
            ]

        for fd, role in rows do
            UnixSystem.setNonBlocking fd true linux
            |> shouldEqual (Error (SetNonBlockingRefusal.UnmodelledOnStandardStream role))

    /// ...but *clearing* it is answered, because `false` is what a stream
    /// already reads back: the refusal is about a divergence that clearing does
    /// not create.
    [<Test>]
    let ``clearing the flag on a standard stream is answered`` () : unit =
        for fd in [ 0 ; 1 ; 2 ] do
            setOrFail fd false linux |> fst |> shouldEqual SetNonBlockingAnswer.Set
            UnixSystem.isNonBlocking fd linux |> shouldEqual (Some false)

    // ------------------------------------------------------------------
    // The event port, where store and answer come apart
    // ------------------------------------------------------------------

    /// Measured: the platforms agree that the bit toggles and disagree on the
    /// answer — Linux succeeds where Darwin reports a failure **with the bit
    /// toggled anyway**. That is why the answer and the stored flag are checked
    /// separately, and why the failing arm still hands back a system.
    [<Test>]
    let ``an event port stores the flag whatever it answers`` () : unit =
        for platform in platforms do
            let system = systemOn platform

            let portFd, registry =
                FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors = registry
                        }
                }

            let answer, after = setOrFail portFd true system

            // The bit toggled on both.
            UnixSystem.isNonBlocking portFd after |> shouldEqual (Some true)

            let expected =
                match SimulatedUnixPlatform.eventPortSetStatusFlagsError platform with
                | None -> SetNonBlockingAnswer.Set
                | Some error -> SetNonBlockingAnswer.Failed error

            answer |> shouldEqual expected

    /// ...and the two flavours really do differ here, stated as literals so that
    /// the row above cannot agree with any rule at all.
    [<Test>]
    let ``the event port answer splits by flavour`` () : unit =
        let answerOn (platform : SimulatedUnixPlatform) =
            let system = systemOn platform

            let portFd, registry =
                FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

            let system =
                { system with
                    Process =
                        { system.Process with
                            FileDescriptors = registry
                        }
                }

            setOrFail portFd true system |> fst

        answerOn SimulatedUnixPlatform.linuxX64 |> shouldEqual SetNonBlockingAnswer.Set

        answerOn SimulatedUnixPlatform.macOsArm64
        |> shouldEqual (SetNonBlockingAnswer.Failed UnixError.ENOTTY)
