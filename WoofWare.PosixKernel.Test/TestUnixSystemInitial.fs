namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.initial`: which fields the platform fixes, and which it
/// deliberately does not.
///
/// The distinction is the whole point of the constructor. Before it existed
/// every fixture built the record by hand, and all ten wrote Linux's `SoMaxConn`
/// and `Tmpfs` under both flavours — a Darwin machine the library's own
/// `EmulatedFileSystemType.isReportableUnder` says cannot exist.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemInitial =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    // ------------------------------------------------------------------
    // What the platform fixes
    // ------------------------------------------------------------------

    /// Stated as literals rather than by calling the same derivation the
    /// constructor calls: a row that asked `defaultSoMaxConn` what to expect
    /// would agree with any constructor at all, including one that ignored the
    /// platform.
    [<TestCase("linux", 4096)>]
    [<TestCase("darwin", 128)>]
    let ``somaxconn is the flavour's`` (flavour : string, expected : int) : unit =
        let platform =
            match flavour with
            | "linux" -> SimulatedUnixPlatform.linuxX64
            | "darwin" -> SimulatedUnixPlatform.macOsArm64
            | other -> failwith $"unknown flavour %s{other}"

        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        system.Machine.SoMaxConn |> shouldEqual expected

    [<TestCase("linux")>]
    [<TestCase("darwin")>]
    let ``the filesystem type is one that flavour can report`` (flavour : string) : unit =
        let platform, expected =
            match flavour with
            | "linux" -> SimulatedUnixPlatform.linuxX64, EmulatedMount.Tmpfs TmpfsMount.defaults
            | "darwin" -> SimulatedUnixPlatform.macOsArm64, EmulatedMount.Apfs ApfsMount.defaults
            | other -> failwith $"unknown flavour %s{other}"

        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        system.Machine.Mount |> shouldEqual expected

        // The rule the pair exists to satisfy, asserted directly: a machine
        // claiming a type its flavour never mounts would hand a process a fact
        // no real system could tell it.
        EmulatedFileSystemType.isReportableUnder
            (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
            (EmulatedMount.fileSystemType system.Machine.Mount)
        |> shouldEqual true

    /// `somaxconn` left unconfigured is the machine's own flavour's default,
    /// so a Darwin machine never carries Linux's 4096 and a configured value
    /// is carried as given.
    [<TestCaseSource(nameof platforms)>]
    let ``withSoMaxConn None takes the machine's own flavour's default`` (platform : SimulatedUnixPlatform) : unit =
        let image : UnixBootImage<int, string> = UnixSystem.initial platform

        let flavour = SimulatedUnixPlatform.flavour platform

        let configured =
            image
            |> UnixBootImage.withSoMaxConn (Some 7)
            |> Configured.expectOk SoMaxConnRefusal.describe

        (Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0) configured).Machine.SoMaxConn
        |> shouldEqual 7

        // Back to the default, from an image that no longer carries it.
        (configured
         |> UnixBootImage.withSoMaxConn None
         |> Configured.expectOk SoMaxConnRefusal.describe
         |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0))
            .Machine.SoMaxConn
        |> shouldEqual (UnixMachineState.defaultSoMaxConn flavour)

    /// The platform is fixed at construction and nothing validates it later,
    /// so a forged one has to be refused here, naming this constructor.
    [<Test>]
    let ``a forged platform is refused by the constructor`` () : unit =
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                UnixSystem.initial<int, string> Unchecked.defaultof<SimulatedUnixPlatform>
                |> ignore<UnixBootImage<int, string>>
            )

        exn.Message |> shouldContainText "UnixSystem.initial"

    [<TestCaseSource(nameof platforms)>]
    let ``the platform asked for is the platform reported`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        system.Machine.UnixPlatform |> shouldEqual platform

    // ------------------------------------------------------------------
    // What it deliberately does not fix
    // ------------------------------------------------------------------

    /// The buffer check's limit is a property of the machine's paging depth as
    /// well as of its architecture, so the default is the commonest machine of
    /// each architecture's, and a platform that screens nothing has none. Stated
    /// as literals rather than by calling the derivation the constructor calls.
    [<Test>]
    let ``the buffer check follows the platform's architecture`` () : unit =
        let checkOn (platform : SimulatedUnixPlatform) : UserBufferCheck =
            (UnixSystem.initial<int, string> platform
             |> (Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)))
                .Machine.UserBufferCheck

        checkOn SimulatedUnixPlatform.linuxX64
        |> shouldEqual (UserBufferCheck.BeforeOperation 0x0000_7FFF_FFFF_F000UL)

        checkOn SimulatedUnixPlatform.linuxArm64
        |> shouldEqual (UserBufferCheck.BeforeOperation 0x0001_0000_0000_0000UL)

        checkOn SimulatedUnixPlatform.macOsArm64
        |> shouldEqual UserBufferCheck.AtCopyTime

    /// The ephemeral range and the process identity are each flavour's shipped
    /// defaults, stated as literals rather than by calling the derivation the
    /// constructor calls: Linux's `ip_local_port_range` and first user, and
    /// Darwin's `net.inet.ip.portrange.first`/`last` and first user (uid 501,
    /// primary group `staff`), both measured.
    [<TestCase("linux", 32768, 60999, 1000, 1000)>]
    [<TestCase("darwin", 49152, 65535, 501, 20)>]
    let ``the ephemeral range and the process identity are the flavour's``
        (flavour : string, low : int, high : int, uid : int, gid : int)
        : unit
        =
        let platform =
            match flavour with
            | "linux" -> SimulatedUnixPlatform.linuxX64
            | "darwin" -> SimulatedUnixPlatform.macOsArm64
            | other -> failwith $"unknown flavour %s{other}"

        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        system.Machine.EphemeralPortRange |> shouldEqual (uint16 low, uint16 high)
        system.Machine.NextEphemeralPort |> shouldEqual (uint16 low)

        system.Process.Credentials
        |> shouldEqual (
            Credentials.ofIds (UserId.parseOrFail "test" (uint32 uid)) (GroupId.parseOrFail "test" (uint32 gid)) []
        )

    /// Both clocks belong to the simulation rather than to the machine it
    /// claims to be, so a recorded trace's timestamps must not depend on which
    /// flavour the process thinks it is running on.
    ///
    /// `TestMonotonicTimestamp` and `TestSystemTimeAsTicks` derive every reading
    /// they check from a system booted on one arbitrary flavour, on the strength
    /// of exactly this.
    [<TestCaseSource(nameof platforms)>]
    let ``both clocks boot at zero on every flavour`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        system.Machine.NanosecondsSinceBoot |> shouldEqual 0L
        system.Machine.BootTime |> shouldEqual UnixTimestamp.epoch

    /// The cursor is where `bind(2)` for port 0 begins its sweep, and
    /// `allocateEphemeralPort` walks *upward* from it, wrapping at the top. A
    /// cursor parked at the top of the range is therefore not a harmless
    /// starting point: it hands out the last port first.
    ///
    /// Asserted by allocating rather than by reading the field back, because the
    /// field only matters through what it makes the allocator do.
    [<TestCaseSource(nameof platforms)>]
    let ``the first ephemeral port drawn is the bottom of the range`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let socket : SocketDescription =
            {
                Addressing = SocketAddressing.Inet None
                Kind = SocketKind.Stream
                Protocol = SocketProtocol.Tcp
                ReuseAddress = false
                Options = SocketOptions.initial
                Phase = SocketPhase.Idle
            }

        let candidate (port : uint16) : SocketBinding =
            {
                Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port
                LockedAddress = None
                LockedPort = false
            }

        match
            UnixMachineState.allocateEphemeralPort
                EphemeralPortUse.Reserve
                (SocketId 0L)
                socket
                candidate
                system.Machine
        with
        | None -> failwith "a fresh machine could allocate no ephemeral port at all"
        | Some (bound, machine) ->
            bound.Endpoint.Port |> shouldEqual (fst system.Machine.EphemeralPortRange)
            // ...and the cursor advances by one, so the next draw is not the same port.
            machine.NextEphemeralPort
            |> shouldEqual (fst system.Machine.EphemeralPortRange + 1us)

    // ------------------------------------------------------------------
    // The pairs that must start consistent
    // ------------------------------------------------------------------

    /// The directory a fresh process stands in is the root of *this* system's
    /// own filesystem, and the path derived from it is "/". A constructor that
    /// built the filesystem twice would hold an inode from the other one, which
    /// no path here reaches.
    [<TestCaseSource(nameof platforms)>]
    let ``the current directory is the root of this system's own filesystem``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        system.Process.CurrentDirectoryInode
        |> shouldEqual (VirtualFileSystem.root system.Machine.FileSystem)

        UnixPathResolution.currentDirectoryPath system
        |> shouldEqual (Some AbsoluteUnixPath.root)

    /// Every rule at once, which is the cheapest statement that a fresh system
    /// is a system at all.
    [<TestCaseSource(nameof platforms)>]
    let ``a fresh system is sound`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        UnixSystem.checkInvariants system |> shouldEqual []

    /// Only the three standard streams, which is what "before anything has
    /// happened to it" means for the descriptor table: each a pipe end of its
    /// own, whose far end the client holds as the launch table says.
    [<TestCaseSource(nameof platforms)>]
    let ``only the standard streams are open`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        for fd, pipeEnd, client in
            [
                0, PipeEnd.Read, ClientEnd.WriteEndClosed
                1, PipeEnd.Write, ClientEnd.Draining
                2, PipeEnd.Write, ClientEnd.Draining
            ] do
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Pipe (pipeId, actualEnd)) ->
                actualEnd |> shouldEqual pipeEnd

                (UnixMachineState.pipe pipeId system.Machine).Origin
                |> shouldEqual (PipeOrigin.Launched (ExternalEndpoint (UnixSystem.processId system, fd), client))
            | other -> failwith $"fd %d{fd} is %A{other}, not the %O{pipeEnd} end of a launched pipe"

        FileDescriptorRegistry.tryFind 3 (UnixSystemState.fileDescriptors system)
        |> shouldEqual None

        system.Machine.Pipes |> Map.count |> shouldEqual 3
        DeliveryLog.count system.Machine.Delivered |> shouldEqual 0
        UnixSystem.checkInvariants system |> shouldEqual []

    /// A launch table of any shape: the descriptors it names and no others,
    /// each onto a pipe of its own with the end and the far holder its entry
    /// says, at the access mode `pipe(2)` gives that end, blocking. The pipes
    /// are the machine's first, the system is sound, and the next descriptor a
    /// process makes is the lowest the table left free.
    [<Test>]
    let ``initial launches exactly the table it is given`` () : unit =
        // Supplied payloads either side of the 64 KiB both flavours' pipes
        // take before the client's write sleeps.
        let descriptor =
            Gen.oneof
                [
                    Gen.constant LaunchDescriptor.Drained
                    Gen.constant LaunchDescriptor.Gone
                    Gen.elements [ 0 ; 1 ; 100 ; 65535 ; 65536 ; 65537 ; 100000 ]
                    |> Gen.map (fun length ->
                        LaunchDescriptor.Supplied (ImmutableArray.Create<byte> (Array.init length byte))
                    )
                ]

        let table =
            Gen.zip (Gen.choose (0, 12)) descriptor |> Gen.listOf |> Gen.map Map.ofList

        let platform = Gen.elements platforms

        let property (launch : Map<int, LaunchDescriptor>, platform : SimulatedUnixPlatform) : unit =
            let system : UnixSystem<int, string> =
                UnixSystem.initial platform |> (Launched.boot launch 0 (CpuId 0))

            let registry = UnixSystemState.fileDescriptors system

            FileDescriptorRegistry.fds registry
            |> Map.keys
            |> Set.ofSeq
            |> shouldEqual (launch |> Map.keys |> Set.ofSeq)

            OpenFileTable.descriptions (FileDescriptorRegistry.openFiles registry)
            |> Map.count
            |> shouldEqual launch.Count

            for KeyValue (fd, entry) in launch do
                let description =
                    match FileDescriptorRegistry.tryFind fd registry with
                    | Some description -> description
                    | None -> failwith $"fd %d{fd} is not open"

                // What the client's write puts in before it sleeps: all of it,
                // if it fits in the 64 KiB the pipe takes.
                let expectedEnd, expectedMode, expectedHeld, expectedUnwritten =
                    match entry with
                    | LaunchDescriptor.Supplied bytes ->
                        let held = min bytes.Length 65536
                        PipeEnd.Read, FileAccessMode.ReadOnly, held, bytes.Length - held
                    | LaunchDescriptor.Drained
                    | LaunchDescriptor.Gone -> PipeEnd.Write, FileAccessMode.WriteOnly, 0, 0

                description.AccessMode |> shouldEqual expectedMode
                description.NonBlocking |> shouldEqual false
                description.Flock |> shouldEqual None

                match description.Target with
                | OpenFileTarget.Pipe (pipeId, pipeEnd) ->
                    pipeEnd |> shouldEqual expectedEnd

                    let pipe = UnixMachineState.pipe pipeId system.Machine

                    match pipe.Origin, entry with
                    | PipeOrigin.Launched (endpoint, ClientEnd.Draining), LaunchDescriptor.Drained
                    | PipeOrigin.Launched (endpoint, ClientEnd.ReadEndClosed), LaunchDescriptor.Gone
                    | PipeOrigin.Launched (endpoint, ClientEnd.WriteEndClosed), LaunchDescriptor.Supplied _ ->
                        endpoint |> shouldEqual (ExternalEndpoint (UnixSystem.processId system, fd))
                        expectedUnwritten |> shouldEqual 0
                    | PipeOrigin.Launched (endpoint, ClientEnd.Supplying unwritten), LaunchDescriptor.Supplied bytes ->
                        endpoint |> shouldEqual (ExternalEndpoint (UnixSystem.processId system, fd))
                        unwritten.Length |> shouldEqual expectedUnwritten

                        unwritten.ToImmutableArray ()
                        |> Seq.toList
                        |> shouldEqual (bytes |> Seq.skip expectedHeld |> Seq.toList)
                    | origin, _ -> failwith $"fd %d{fd}, launched as %A{entry}, made a pipe of origin %A{origin}"

                    PipeBuffer.held pipe.Buffer |> shouldEqual expectedHeld
                | other -> failwith $"fd %d{fd} is %A{other}, not a pipe end"

            system.Machine.Pipes |> Map.count |> shouldEqual launch.Count
            system.Machine.NextPipeId |> shouldEqual (PipeId (int64 launch.Count))
            UnixSystem.checkInvariants system |> shouldEqual []

            let lowestFree =
                Seq.initInfinite id |> Seq.find (fun fd -> not (Map.containsKey fd launch))

            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readFd, _), _) -> readFd |> shouldEqual lowestFree
            | other -> failwith $"pipe2 did not make a pipe: %A{other}"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip table platform)) property
        )

    [<Test>]
    let ``a launch table naming a negative descriptor is refused`` () : unit =
        let exn =
            Assert.Throws (fun () ->
                UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64
                |> Launched.boot (Map.ofList [ -1, LaunchDescriptor.Drained ]) 0 (CpuId 0)
                |> ignore
            )

        exn.Message |> shouldContainText "descriptor -1"
