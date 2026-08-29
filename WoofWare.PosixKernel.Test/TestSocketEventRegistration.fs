namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.changeSocketEventRegistration`: `epoll_ctl(2)` past a caller's
/// own screens.
///
/// `FileDescriptorRegistry.changeSocketEventRegistration` already has the
/// refusal ladder's own rows. What is only reachable here is what this function
/// adds on top of it: the flavour refusal, the ordinal that only an `Add`
/// consumes, and the rule that an `Add` or `Modify` whose target is *already*
/// ready makes the registration pending at that moment.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventRegistration =

    let private context : string = "TestSocketEventRegistration"

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

    let private everything : SocketEventInterest =
        {
            In = true
            Out = true
            RdHup = true
        }

    /// Interest in reading only, which an idle stream socket does not satisfy —
    /// its level is `OUT|HUP`, and `HUP` is reported whether asked for or not,
    /// so this is *not* a way to make a registration non-pending. Kept for the
    /// row that says so.
    let private readOnly : SocketEventInterest =
        {
            In = true
            Out = false
            RdHup = false
        }

    let private withPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// An idle stream socket, whose epoll level is `OUT|HUP` — so it is ready
    /// under any interest, which is what makes the pending rows below fire.
    let private withSocket (socketId : SocketId) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let socket =
            {
                Domain = SocketDomain.InterNetwork
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                Binding = None
                ReuseAddress = false
                Phase = SocketPhase.Idle
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        let (SocketId raw) = socketId

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// A port and an idle socket, and the two descriptors onto them.
    let private portAndSocket (system : UnixSystem<int, string>) : int * int * UnixSystem<int, string> =
        let portFd, system = withPort system
        let socketFd, system = withSocket (SocketId 0L) system
        portFd, socketFd, system

    let private changeOrFail
        (portFd : int)
        (targetFd : int)
        (change : SocketEventRegistrationChange)
        (system : UnixSystem<int, string>)
        : SocketEventRegistrationAnswer * UnixSystem<int, string>
        =
        match UnixSystem.changeSocketEventRegistration portFd targetFd change system with
        | Ok result -> result
        | Error refusal ->
            failwith $"expected an answer, got a refusal: %s{SocketEventRegistrationRefusal.describe refusal}"

    let private applied
        (portFd : int)
        (targetFd : int)
        (change : SocketEventRegistrationChange)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match changeOrFail portFd targetFd change system with
        | SocketEventRegistrationAnswer.Changed, system -> system
        | SocketEventRegistrationAnswer.Failed reason, _ -> failwith $"expected the change to apply, got %O{reason}"

    /// The port's pending list, in delivery order.
    let private ready (portFd : int) (system : UnixSystem<int, string>) : (int * OpenFileDescriptionId) list =
        match FileDescriptorRegistry.tryFindTarget portFd system.Process.FileDescriptors with
        | Some (OpenFileTarget.SocketEventPort portState) -> portState.Ready
        | other -> failwith $"expected an event port, got %A{other}"

    // ------------------------------------------------------------------
    // The errnos
    // ------------------------------------------------------------------

    /// `epoll_ctl(2)`'s six answers, measured on Linux 6.18.5 and stated here as
    /// literals rather than read back off `toErrno`, which is the function under
    /// test. Each case's own docstring says the same thing in prose; this is
    /// what makes the two disagreeing a failure rather than a discrepancy
    /// nobody runs.
    [<Test>]
    let ``each refusal carries the errno epoll_ctl answers`` () : unit =
        let rows =
            [
                SocketEventRegistrationError.BadPortFd, UnixError.EBADF
                SocketEventRegistrationError.BadTargetFd, UnixError.EBADF
                SocketEventRegistrationError.TargetNotPollable, UnixError.EPERM
                SocketEventRegistrationError.NotAnEventPort, UnixError.EINVAL
                SocketEventRegistrationError.AlreadyRegistered, UnixError.EEXIST
                SocketEventRegistrationError.NotRegistered, UnixError.ENOENT
            ]

        for reason, expected in rows do
            SocketEventRegistrationError.toErrno reason |> shouldEqual expected

    /// Not injective, and the row exists so that a future reader does not
    /// "simplify" the two `EBADF` cases into one: they are distinguished by
    /// *which* descriptor was bad, which the errno cannot carry and a client
    /// diagnosing a guest wants.
    [<Test>]
    let ``the two EBADF refusals stay distinguishable`` () : unit =
        SocketEventRegistrationError.BadPortFd
        |> shouldNotEqual SocketEventRegistrationError.BadTargetFd

        SocketEventRegistrationError.toErrno SocketEventRegistrationError.BadPortFd
        |> shouldEqual (SocketEventRegistrationError.toErrno SocketEventRegistrationError.BadTargetFd)

    /// Every case maps somewhere, and no two *distinct* errnos collide beyond
    /// the pair above: five refusals, four numbers.
    [<Test>]
    let ``the six refusals use exactly five errnos`` () : unit =
        let all =
            [
                SocketEventRegistrationError.BadPortFd
                SocketEventRegistrationError.BadTargetFd
                SocketEventRegistrationError.TargetNotPollable
                SocketEventRegistrationError.NotAnEventPort
                SocketEventRegistrationError.AlreadyRegistered
                SocketEventRegistrationError.NotRegistered
            ]

        all |> List.length |> shouldEqual 6

        all
        |> List.map SocketEventRegistrationError.toErrno
        |> List.distinct
        |> List.length
        |> shouldEqual 5

    // ------------------------------------------------------------------
    // The flavour
    // ------------------------------------------------------------------

    /// Ahead of everything, including the descriptor lookups: kqueue's model is
    /// structurally different rather than differently numbered, so there is no
    /// row of it to answer even for inputs epoll would refuse.
    [<Test>]
    let ``a Darwin-flavoured kernel refuses every registration change`` () : unit =
        let portFd, socketFd, darwin =
            portAndSocket (systemOn SimulatedUnixPlatform.macOsArm64)

        let expected =
            Error (SocketEventRegistrationRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)

        let changes =
            [
                SocketEventRegistrationChange.Add (everything, 1UL)
                SocketEventRegistrationChange.Modify (everything, 1UL)
                SocketEventRegistrationChange.Remove
            ]

        for change in changes do
            UnixSystem.changeSocketEventRegistration portFd socketFd change darwin
            |> shouldEqual expected

            // ...including for descriptors epoll itself would refuse.
            UnixSystem.changeSocketEventRegistration 99 99 change darwin
            |> shouldEqual expected

    // ------------------------------------------------------------------
    // The ordinal
    // ------------------------------------------------------------------

    /// Only an `Add` mints a registration, so only an `Add` consumes an ordinal.
    /// A `Modify` rebuilds an existing one and a `Remove` destroys it, and
    /// neither may shift the numbering a later `Add` will get.
    [<Test>]
    let ``only an Add consumes an ordinal`` () : unit =
        let portFd, socketFd, system = portAndSocket linux
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 0L

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (everything, 1UL)) system

        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 1L

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Modify (everything, 2UL)) system

        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 1L

        let system = applied portFd socketFd SocketEventRegistrationChange.Remove system
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 1L

        // ...and the next Add takes the number the Modify and Remove left alone.
        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (everything, 3UL)) system

        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 2L

    /// A refused change consumes nothing either: the ordinal is taken by the
    /// registry commit, which did not happen.
    [<Test>]
    let ``a refused change consumes no ordinal`` () : unit =
        let portFd, socketFd, system = portAndSocket linux

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (everything, 1UL)) system

        let before = system.Machine.NextSocketEventRegistrationOrdinal

        match changeOrFail portFd socketFd (SocketEventRegistrationChange.Add (everything, 2UL)) system with
        | SocketEventRegistrationAnswer.Changed, _ -> failwith "expected EEXIST"
        | SocketEventRegistrationAnswer.Failed reason, after ->
            reason |> shouldEqual SocketEventRegistrationError.AlreadyRegistered
            after.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual before

            // Nothing changed at all, not merely the ordinal.
            after |> shouldEqual system

    // ------------------------------------------------------------------
    // Pending at registration time
    // ------------------------------------------------------------------

    /// The rule this function adds over the registry's: an `Add` whose target is
    /// already ready under the *new* interest becomes pending at that moment,
    /// rather than waiting for something to happen to the target.
    [<Test>]
    let ``an Add of an already-ready target is pending at once`` () : unit =
        let portFd, socketFd, system = portAndSocket linux
        ready portFd system |> shouldEqual []

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (everything, 1UL)) system

        let portId =
            match FileDescriptorRegistry.tryFindId portFd system.Process.FileDescriptors with
            | Some id -> id
            | None -> failwith "the port is not live"

        let targetId =
            match FileDescriptorRegistry.tryFindId socketFd system.Process.FileDescriptors with
            | Some id -> id
            | None -> failwith "the target is not live"

        portId |> shouldNotEqual targetId
        ready portFd system |> shouldEqual [ (socketFd, targetId) ]

    /// `HUP` is reported whether it was asked for or not, so an idle stream
    /// socket is ready under a read-only interest too. The row exists to stop
    /// the one above from looking like "any interest at all makes it pending".
    [<Test>]
    let ``an interest that misses OUT still sees the unrequested HUP`` () : unit =
        let portFd, socketFd, system = portAndSocket linux

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (readOnly, 1UL)) system

        ready portFd system |> List.length |> shouldEqual 1

    /// A `Modify` of an entry already pending leaves its place alone rather than
    /// appending it a second time.
    [<Test>]
    let ``a Modify of an already-pending entry does not re-append it`` () : unit =
        let portFd, socketFd, system = portAndSocket linux

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (everything, 1UL)) system

        let afterAdd = ready portFd system

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Modify (everything, 2UL)) system

        ready portFd system |> shouldEqual afterAdd

    /// A `Remove` never makes anything pending, whatever the target's level.
    [<Test>]
    let ``a Remove appends nothing`` () : unit =
        let portFd, socketFd, system = portAndSocket linux

        let system =
            applied portFd socketFd (SocketEventRegistrationChange.Add (everything, 1UL)) system

        let system = applied portFd socketFd SocketEventRegistrationChange.Remove system

        // The registration is gone, so its pending entry goes with it.
        ready portFd system
        |> List.filter (fun (fd, _) -> fd = socketFd)
        |> shouldEqual []

    /// A target that is *not* ready does not become pending: registering it is
    /// not itself an event. Standard input's epoll level is `HUP` under the
    /// launch shape this kernel models, so the row that shows this needs a
    /// target with an empty level — an output stream under a read-only
    /// interest, whose level is `OUT` alone.
    [<Test>]
    let ``an Add of a target with nothing to report is not pending`` () : unit =
        let portFd, system = withPort linux

        // fd 1 is standard output: its level is OUT and nothing else, so a
        // read-only interest reports nothing at all.
        let system =
            applied portFd 1 (SocketEventRegistrationChange.Add (readOnly, 1UL)) system

        ready portFd system |> shouldEqual []
