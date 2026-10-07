namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `KernelConfig.split` divides a run's configuration into what describes the machine
/// (`MachineConfig`) and what describes one process on it (`ProcessConfig`), and
/// `KernelConfig.toKernel` boots through the two. These rows hold that division to the
/// kernel the configuration describes, and each part's refusals to the knob a host set.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKernelConfigSplit =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    /// The kernel `config` describes, booted with every setter applied to one image in the
    /// order `KernelConfig` lists its fields, rather than through `KernelConfig.split`: the
    /// reference the split is held to. It is what `KernelConfig.toKernel` did before the
    /// split, and stays here as the oracle that dividing the setters between the machine and
    /// the process, and applying each part's together, changes nothing.
    let private reference (config : KernelConfig) : EmulatedKernel =
        let platform = config.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform

        let credentials =
            Credentials.ofIds
                (config.UserId
                 |> Option.map (UserId.parseOrFail "reference")
                 |> Option.defaultValue (UnixSystem.defaultUserId flavour))
                (config.GroupId
                 |> Option.map (GroupId.parseOrFail "reference")
                 |> Option.defaultValue (UnixSystem.defaultGroupId flavour))
                (config.SupplementaryGroups |> List.map (GroupId.parseOrFail "reference"))

        let rec stateOwners
            (owner : InodeOwner)
            (entries : Map<DirectoryEntryName, SeedEntry>)
            : Map<DirectoryEntryName, SeedEntry>
            =
            entries
            |> Map.map (fun _ entry ->
                match entry with
                | SeedEntry.File (contents, permissions, stated) ->
                    SeedEntry.File (contents, permissions, Some (Option.defaultValue owner stated))
                | SeedEntry.Symlink (target, stated) ->
                    SeedEntry.Symlink (target, Some (Option.defaultValue owner stated))
                | SeedEntry.Directory (children, permissions, stated) ->
                    SeedEntry.Directory (
                        stateOwners owner children,
                        permissions,
                        Some (Option.defaultValue owner stated)
                    )
            )

        let ok (result : Result<'a, 'e>) : 'a =
            match result with
            | Ok a -> a
            | Error e -> failwith $"reference: refused: %A{e}"

        let machine = KernelImage.mapMachine
        let launch = KernelImage.mapProcess

        EmulatedKernel.image platform config.StandardStreams
        |> launch (ProcessLaunch.withCoreDumps config.CoreDumps)
        |> EmulatedKernel.withEnvironment "reference" config.Environment
        |> machine (UnixBootImage.withProcessorCount config.ProcessorCount >> ok)
        |> machine (fun image ->
            match config.UserAddressLimit with
            | None -> image
            | Some limit -> UnixBootImage.withUserAddressLimit limit image |> ok
        )
        |> EmulatedKernel.withWallClockEpochMs config.WallClockEpochMs
        |> machine (UnixBootImage.withMount config.Mount >> ok)
        |> launch (ProcessLaunch.withProcessPath "reference" config.ProcessPath)
        |> EmulatedKernel.withFileSystemAndCurrentDirectory
            (UnixTimestamp.ofMillisecondsSinceEpoch config.WallClockEpochMs)
            (config.FileSystemRootOwner
             |> Option.defaultValue (InodeOwner.ofProcess credentials))
            (stateOwners (InodeOwner.ofProcess credentials) config.FileSystem)
            config.CurrentDirectory
        |> launch (ProcessLaunch.withCredentials credentials >> ok)
        |> machine (
            UnixBootImage.withEphemeralPortRange (
                config.EphemeralPortRange
                |> Option.defaultValue (UnixSystem.defaultEphemeralPortRange flavour)
            )
            >> ok
        )
        |> machine (UnixBootImage.withSoMaxConn config.SoMaxConn >> ok)
        |> machine (UnixBootImage.withTcpSendSpace config.TcpSendSpace >> ok)
        |> machine (UnixBootImage.withProtectedFiles config.ProtectedFiles >> ok)
        |> machine (UnixBootImage.withLocalAddresses config.LocalAddresses config.LocalRoutes)
        |> launch (ProcessLaunch.withUmask config.Umask >> ok)
        |> machine (UnixBootImage.withProcessId config.ProcessId >> ok)
        |> machine (fun image ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> image
            | SimulatedUnixFlavour.Darwin ->
                let id =
                    config.LeaderThreadId
                    |> Option.defaultValue (uint64 (ProcessId.toInt32 config.ProcessId))

                UnixBootImage.withLeaderThreadId id image |> ok
        )
        |> EmulatedKernel.bootInheritingSignalIgnores "reference" config.InheritedSignalIgnores
        |> EmulatedKernel.mapUnix (fun system ->
            match config.PidMax with
            | None -> system
            | Some pidMax -> UnixSystem.writePidMaxSysctl "reference" pidMax system
        )
        |> fun kernel ->
            match config.CLibrary with
            | None -> kernel
            | Some library -> EmulatedKernel.withCLibrary "reference" library kernel
        |> EmulatedKernel.withInstructionCostTicks config.InstructionCostTicks
        |> EmulatedKernel.withClockJitter config.ClockJitter
        |> EmulatedKernel.withOptimalMaxSpinWaitsPerSpinIteration config.OptimalMaxSpinWaitsPerSpinIteration

    let private ownerGen : Gen<InodeOwner> =
        Gen.elements
            [
                {
                    User = UserId.root
                    Group = GroupId.parseOrFail "test" 0u
                }
                {
                    User = UserId.parseOrFail "test" 37u
                    Group = GroupId.parseOrFail "test" 38u
                }
            ]

    /// A seed holding `/outer/inner/`, so a current directory there resolves, with each
    /// entry's owner stated or left to the configuration.
    let private seedGen : Gen<Map<DirectoryEntryName, SeedEntry>> =
        gen {
            let! owners = Gen.listOfLength 3 (Gen.oneof [ Gen.constant None ; ownerGen |> Gen.map Some ])

            return
                Map.ofList
                    [
                        name "outer",
                        SeedEntry.Directory (
                            Map.ofList
                                [
                                    name "inner",
                                    SeedEntry.Directory (Map.empty, SeedEntry.defaultPermsForDirectory, owners.[0])
                                    name "file",
                                    SeedEntry.File (noBytes, SeedEntry.defaultPermsForRegularFile, owners.[1])
                                ],
                            SeedEntry.defaultPermsForDirectory,
                            owners.[2]
                        )
                    ]
        }

    /// A configuration `KernelConfig.toKernel` boots, on either platform, varying every
    /// field a host may set.
    let private configGen : Gen<KernelConfig> =
        gen {
            let! platform = Gen.elements [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]
            let flavour = SimulatedUnixPlatform.flavour platform
            let! environment = Gen.subListOf [ "A=1" ; "B=two" ; "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=0" ; "C" ]
            let! processors = Gen.choose (1, 8)

            let! userAddressLimit =
                match flavour with
                | SimulatedUnixFlavour.Linux -> Gen.elements [ None ; Some 0x7FFFFFFFF000UL ]
                | SimulatedUnixFlavour.Darwin -> Gen.constant None

            let! cost = Gen.choose (1, 50)

            let! jitter =
                Gen.elements
                    [
                        ClockJitterStrategy.Disabled
                        ClockJitterStrategy.EagerDeadlines (17UL, 0.25, 1000L)
                    ]

            let! spins = Gen.choose (1, 12)
            let! epochMs = Gen.choose (0, 1_000_000) |> Gen.map (fun n -> int64 n * 1_000_000L)
            let! cLibrary = Gen.elements [ None ; Some (CLibrary.ofPlatform platform) ]
            let! currentDirectory = Gen.elements [ absolute "/" ; absolute "/outer/inner" ]
            let! processPath = Gen.elements [ UnixSystem.defaultProcessPath ; Some (absolute "/usr/bin/dotnet") ; None ]
            let! fileSystem = seedGen
            let! rootOwner = Gen.oneof [ Gen.constant None ; ownerGen |> Gen.map Some ]
            let! userId = Gen.elements [ None ; Some 0u ; Some 37u ]
            let! groupId = Gen.elements [ None ; Some 0u ; Some 38u ]
            let! supplementary = Gen.elements [ [] ; [ 5u ] ; [ 4000u ; 3000u ] ]
            let! umask = Gen.elements [ 0o022 ; 0o077 ; 0o000 ]
            let! processId = Gen.choose (300, 90000)
            let! ports = Gen.elements [ None ; Some (40000us, 40100us) ]
            let! soMaxConn = Gen.elements [ None ; Some 17 ]

            let! tcpSendSpace =
                match flavour with
                | SimulatedUnixFlavour.Linux -> Gen.constant None
                | SimulatedUnixFlavour.Darwin -> Gen.elements [ None ; Some 65536 ]

            let! protectedFiles =
                match flavour with
                | SimulatedUnixFlavour.Darwin -> Gen.constant ProtectedFiles.off
                | SimulatedUnixFlavour.Linux ->
                    Gen.elements
                        [
                            ProtectedFiles.off
                            { ProtectedFiles.off with
                                Symlinks = SymlinkProtection.InWorldWritableStickyDirectories
                            }
                        ]

            let! ignores = Gen.subListOf [ Signal.SIGHUP ; Signal.SIGUSR1 ]
            let! output = Gen.elements [ OutputStreamReader.Drained ; OutputStreamReader.Gone ]
            let! coreDumps = Gen.elements [ CoreDumps.Suppressed ; CoreDumps.Written ]

            let! pidMax =
                match flavour with
                | SimulatedUnixFlavour.Darwin -> Gen.constant None
                | SimulatedUnixFlavour.Linux -> Gen.elements [ None ; Some (processId + 1000) ]

            let! leaderThreadId =
                match flavour with
                | SimulatedUnixFlavour.Linux -> Gen.constant None
                | SimulatedUnixFlavour.Darwin -> Gen.elements [ None ; Some 0x1234UL ]

            return
                {
                    Environment = environment
                    ProcessorCount = processors
                    UserAddressLimit = userAddressLimit
                    InstructionCostTicks = int64 cost
                    ClockJitter = jitter
                    OptimalMaxSpinWaitsPerSpinIteration = spins
                    WallClockEpochMs = epochMs
                    UnixPlatform = platform
                    CLibrary = cLibrary
                    CurrentDirectory = currentDirectory
                    ProcessPath = processPath
                    FileSystem = fileSystem
                    FileSystemRootOwner = rootOwner
                    UserId = userId
                    GroupId = groupId
                    SupplementaryGroups = supplementary
                    Umask = PermissionBits.parseOrFail "test" umask
                    ProcessId = ProcessId.parseOrFail "test" processId
                    Mount = None
                    EphemeralPortRange = ports
                    SoMaxConn = soMaxConn
                    TcpSendSpace = tcpSendSpace
                    ProtectedFiles = protectedFiles
                    LocalAddresses = UnixSystem.defaultLocalAddresses
                    LocalRoutes = UnixSystem.defaultLocalRoutes
                    InheritedSignalIgnores = Set.ofList ignores
                    StandardStreams =
                        { StandardStreamsConfig.piped with
                            Output = output
                        }
                    CoreDumps = coreDumps
                    PidMax = pidMax
                    LeaderThreadId = leaderThreadId
                }
        }

    [<Test>]
    let ``booting through the split is the kernel every setter applied to one image boots`` () : unit =
        let property (config : KernelConfig) : unit =
            KernelConfig.toKernel config |> shouldEqual (reference config)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen configGen) property)

    [<Test>]
    let ``the split's two parts boot the kernel KernelConfig.toKernel boots`` () : unit =
        let property (config : KernelConfig) : unit =
            let machine, proc = KernelConfig.split config

            MachineConfig.boot "MachineConfig" "ProcessConfig" machine proc
            |> shouldEqual (KernelConfig.toKernel config)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 100, Prop.forAll (Arb.fromGen configGen) property)

    let private message (f : unit -> unit) : string =
        (Assert.Throws<exn> (fun () -> f ())).Message

    [<Test>]
    let ``a refusal names the knob of the part that holds it`` () : unit =
        let machine, proc =
            KernelConfig.split
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                }

        // Darwin has none of Linux's protected-file sysctls: a machine knob.
        message (fun () ->
            MachineConfig.boot
                "MachineConfig"
                "ProcessConfig"
                { machine with
                    ProtectedFiles =
                        { ProtectedFiles.off with
                            Symlinks = SymlinkProtection.InWorldWritableStickyDirectories
                        }
                }
                proc
            |> ignore<EmulatedKernel>
        )
        |> shouldContainText "MachineConfig.ProtectedFiles"

        // glibc is no Darwin process's C library: a process knob.
        message (fun () ->
            MachineConfig.boot
                "MachineConfig"
                "ProcessConfig"
                machine
                { proc with
                    CLibrary = Some (CLibrary.ofPlatform SimulatedUnixPlatform.linuxX64)
                }
            |> ignore<EmulatedKernel>
        )
        |> shouldContainText "ProcessConfig.CLibrary"

        // A directory the machine's filesystem does not hold names both parts' knobs.
        let text =
            message (fun () ->
                MachineConfig.boot
                    "MachineConfig"
                    "ProcessConfig"
                    machine
                    { proc with
                        CurrentDirectory = absolute "/nowhere"
                    }
                |> ignore<EmulatedKernel>
            )

        text |> shouldContainText "MachineConfig.FileSystem"
        text |> shouldContainText "ProcessConfig.CurrentDirectory"
