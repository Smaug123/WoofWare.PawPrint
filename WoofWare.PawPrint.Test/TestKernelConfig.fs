namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `KernelConfig` is everything a host may set about the simulated process
/// before a run, and `toKernel` is the only production path that writes those
/// fields onto a kernel. These are the rows about that layer itself — that its
/// defaults agree with the kernel's, that it reaches the field it names, and
/// that it validates rather than passing a bad value through — as opposed to
/// the rows about what any one field *means*, which belong with that field.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKernelConfig =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    /// `outer/inner/` beside `outer/file`: enough for a row to set a current
    /// directory that the seed really contains.
    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList
            [
                name "outer",
                SeedEntry.directory (
                    Map.ofList
                        [
                            name "inner", SeedEntry.directory Map.empty
                            name "file", SeedEntry.file noBytes
                        ]
                )
            ]

    /// The inode a path names, resolved independently of the kernel — so a row
    /// asserting "the kernel held *this* inode" is checked against the graph
    /// rather than against the kernel's own answer.
    let private inodeOf (kernel : EmulatedKernel) (path : string) : InodeNumber =
        let vfs = (UnixSystem.fileSystem kernel.System)

        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.UnixPlatform)
                // Root, whom no directory's search bit refuses.
                (Credentials.ofIds UserId.root (GroupId.parseOrFail "test" 0u) [])
                SymlinkProtection.Off
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" path)
                vfs
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}"

    [<Test>]
    let ``the process ID is configurable and validated`` () : unit =
        KernelConfig.Default.ProcessId |> shouldEqual UnixSystem.defaultProcessId

        (UnixSystem.processId (KernelConfig.toKernel KernelConfig.Default).System)
        |> shouldEqual UnixSystem.defaultProcessId

        let configured =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    ProcessId = ProcessId.parseOrFail "test" 3
                }

        (UnixSystem.processId configured.System) |> ProcessId.toInt32 |> shouldEqual 3

        // The one value of the type that did not come from `parse`.
        let apply () =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    ProcessId = Unchecked.defaultof<ProcessId>
                }
            |> ignore<EmulatedKernel>

        let exn = Assert.Throws<Exception> (TestDelegate apply)

        exn.Message.StartsWith ("KernelConfig.ProcessId: ", StringComparison.Ordinal)
        |> shouldEqual true

    [<Test>]
    let ``the fs.protected sysctls default to Off, apply on Linux, and are refused on Darwin`` () : unit =
        KernelConfig.Default.ProtectedFiles |> shouldEqual ProtectedFiles.off

        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            UnixSystem.protectedFiles
                (KernelConfig.toKernel
                    { KernelConfig.Default with
                        UnixPlatform = platform
                    })
                    .System
            |> shouldEqual ProtectedFiles.off

        let configured : ProtectedFiles =
            {
                Symlinks = SymlinkProtection.InWorldWritableStickyDirectories
                RegularFiles = CreationProtection.InGroupOrWorldWritableStickyDirectories
                Fifos = CreationProtection.InWorldWritableStickyDirectories
                Hardlinks = HardlinkProtection.NonOwnersNeedReadAndWrite
            }

        UnixSystem.protectedFiles
            (KernelConfig.toKernel
                { KernelConfig.Default with
                    ProtectedFiles = configured
                })
                .System
        |> shouldEqual configured

        let darwin () =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    ProtectedFiles = configured
                }
            |> ignore<EmulatedKernel>

        (Assert.Throws<Exception> (TestDelegate darwin)).Message
        |> shouldContainText "KernelConfig.ProtectedFiles: "

    /// The machine's boot setters refuse rather than throw, and each says why
    /// without knowing what the host called the value; `toKernel` is what
    /// names the knob, so a host is told which field to change.
    [<Test>]
    let ``a machine setting the library refuses fails toKernel naming its knob`` () : unit =
        let darwin =
            { KernelConfig.Default with
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
            }

        let rows : (string * KernelConfig) list =
            [
                "KernelConfig.ProcessorCount: ",
                { KernelConfig.Default with
                    ProcessorCount = 0
                }
                "KernelConfig.UserAddressLimit: ",
                { KernelConfig.Default with
                    UserAddressLimit = Some 1UL
                }
                "KernelConfig.UserAddressLimit: ",
                { darwin with
                    UserAddressLimit = Some ObservedUserAddressLimit.Arm64FortyEightBit
                }
                "KernelConfig.Mount: ",
                { KernelConfig.Default with
                    Mount = Some (EmulatedMount.Apfs ApfsMount.defaults)
                }
                "KernelConfig.Mount: ",
                { darwin with
                    Mount = Some (EmulatedMount.Tmpfs TmpfsMount.defaults)
                }
                "KernelConfig.EphemeralPortRange: ",
                { KernelConfig.Default with
                    EphemeralPortRange = Some (0us, 10us)
                }
                "KernelConfig.EphemeralPortRange: ",
                { KernelConfig.Default with
                    EphemeralPortRange = Some (2us, 1us)
                }
                "KernelConfig.SoMaxConn: ",
                { KernelConfig.Default with
                    SoMaxConn = Some 0
                }
                "KernelConfig.TcpSendSpace: ",
                { KernelConfig.Default with
                    TcpSendSpace = Some 16384
                }
                "KernelConfig.TcpSendSpace: ",
                { darwin with
                    TcpSendSpace = Some 1
                }
                "KernelConfig.TcpSendSpace: ",
                { darwin with
                    TcpSendSpace = Some Int32.MaxValue
                }
            ]

        for prefix, config in rows do
            let apply () =
                KernelConfig.toKernel config |> ignore<EmulatedKernel>

            let exn = Assert.Throws<Exception> (TestDelegate apply)

            if not (exn.Message.StartsWith (prefix, StringComparison.Ordinal)) then
                failwith $"expected a failure starting %s{prefix}, got: %s{exn.Message}"

    [<Test>]
    let ``the instruction cost is configurable and validated`` () : unit =
        // The rate is guest-observable — a guest can measure it by counting work against
        // `Environment.TickCount64`, and it decides whether `SpinWait` reaches its blocking
        // rung — so it is part of the replay contract and belongs in `KernelConfig` rather than
        // being a constant a host cannot see.
        KernelConfig.Default.InstructionCostTicks
        |> shouldEqual EmulatedKernel.defaultInstructionCostTicks

        let configured =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    InstructionCostTicks = 10_000L
                }

        configured.InstructionCostTicks |> shouldEqual 10_000L

        // Zero would freeze the clock, so every guest waiting for time to pass would spin
        // forever: a hang rather than a wrong answer, and the sort of thing a host sweeping the
        // knob could reach by off-by-one. Rejected at the setter, like `ProcessorCount`.
        for bad in [ 0L ; -1L ] do
            let apply () =
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        InstructionCostTicks = bad
                    }
                |> ignore<EmulatedKernel>

            Assert.Throws<Exception> (TestDelegate apply) |> ignore<Exception>

    /// The platform is the one field of `KernelConfig` that others are
    /// derived from, so a Darwin configuration must yield a kernel that is
    /// Darwin in every derived field, not one re-flavoured after the fact.
    /// Stated as the measured literals: a row that asked each derivation
    /// what to expect would agree with any derivation at all.
    [<Test>]
    let ``a Darwin configuration yields a kernel that is Darwin throughout`` () : unit =
        let kernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                }

        kernel.UnixPlatform |> shouldEqual SimulatedUnixPlatform.macOsArm64

        (UnixSystem.mount kernel.System)
        |> shouldEqual (EmulatedMount.Apfs ApfsMount.defaults)

        (UnixSystem.soMaxConn kernel.System) |> shouldEqual 128
        (UnixSystem.ephemeralPortRange kernel.System) |> shouldEqual (49152us, 65535us)

        (UnixSystem.credentials kernel.System)
        |> shouldEqual (Credentials.ofIds (UserId.parseOrFail "test" 501u) (GroupId.parseOrFail "test" 20u) [])

        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        // ...and a configured value is carried as given, on either flavour.
        let configured =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    EphemeralPortRange = Some (40000us, 40010us)
                    UserId = Some 1000u
                    GroupId = Some 1000u
                }

        (UnixSystem.ephemeralPortRange configured.System)
        |> shouldEqual (40000us, 40010us)

        (UnixSystem.credentials configured.System)
        |> shouldEqual (Credentials.ofIds (UserId.parseOrFail "test" 1000u) (GroupId.parseOrFail "test" 1000u) [])

        // The default configuration is Linux's, in every one of those fields.
        let linux = KernelConfig.toKernel KernelConfig.Default
        linux.UnixPlatform |> shouldEqual SimulatedUnixPlatform.linuxX64
        (UnixSystem.ephemeralPortRange linux.System) |> shouldEqual (32768us, 60999us)

        (UnixSystem.credentials linux.System)
        |> shouldEqual (Credentials.ofIds (UserId.parseOrFail "test" 1000u) (GroupId.parseOrFail "test" 1000u) [])

    [<Test>]
    let ``KernelConfig's ids are the process's real, effective and saved ids, and its groups are carried as given``
        ()
        : unit
        =
        let groups = [ 3000u ; 1000u ; 2000u ; 1000u ]

        let kernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UserId = Some 37u
                    GroupId = Some 38u
                    SupplementaryGroups = groups
                }

        (UnixSystem.credentials kernel.System)
        |> shouldEqual (
            Credentials.ofIds
                (UserId.parseOrFail "test" 37u)
                (GroupId.parseOrFail "test" 38u)
                (groups |> List.map (GroupId.parseOrFail "test"))
        )

        KernelConfig.Default.SupplementaryGroups |> shouldEqual []

    [<Test>]
    let ``KernelConfig's seed belongs to the configured user and group`` () : unit =
        let seed =
            Map.ofList
                [
                    DirectoryEntryName.parseOrFail "test" "d",
                    SeedEntry.directory (
                        Map.ofList
                            [
                                DirectoryEntryName.parseOrFail "test" "f",
                                SeedEntry.file System.Collections.Immutable.ImmutableArray.Empty
                            ]
                    )
                ]

        let configured : InodeOwner =
            {
                User = UserId.parseOrFail "test" 37u
                Group = GroupId.parseOrFail "test" 38u
            }

        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let kernel =
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        UnixPlatform = platform
                        UserId = Some 37u
                        GroupId = Some 38u
                        FileSystem = seed
                    }

            // The device filesystem the kernel mounts at boot is root's, and is
            // not the seed's.
            (UnixSystem.fileSystem kernel.System)
            |> VirtualFileSystem.inodes
            |> Map.filter (fun number _ ->
                (VirtualFileSystem.mountedRootOf number (UnixSystem.fileSystem kernel.System)).IsNone
            )
            |> Map.iter (fun _ inode -> inode.Owner |> shouldEqual configured)

            // An owner that is the configured one is not foreign, so it is taken.
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = platform
                    UserId = Some 37u
                    GroupId = Some 38u
                    FileSystem =
                        Map.ofList
                            [
                                DirectoryEntryName.parseOrFail "test" "g",
                                SeedEntry.File (
                                    System.Collections.Immutable.ImmutableArray.Empty,
                                    SeedEntry.defaultPermsForRegularFile,
                                    Some configured
                                )
                            ]
                }
            |> ignore<EmulatedKernel>

    /// The owner of the inode `path` names, walked as root without following
    /// a final symlink, so that a link's own owner is what is read.
    let private ownerAt (kernel : EmulatedKernel) (path : string) : InodeOwner =
        let vfs = (UnixSystem.fileSystem kernel.System)

        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.UnixPlatform)
                (Credentials.ofIds UserId.root (GroupId.parseOrFail "test" 0u) [])
                SymlinkProtection.Off
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.NoFollowFinal
                (UnixPath.parseOrFail "test" path)
                vfs
        with
        | Ok inode ->
            match VirtualFileSystem.tryGet inode vfs with
            | Some inode -> inode.Owner
            | None -> failwith $"%s{path} resolved to an inode the filesystem does not hold"
        | Error error -> failwith $"could not resolve %s{path} in the seed: %O{error}"

    [<Test>]
    let ``KernelConfig realises every owner a seed states, and the configured one elsewhere`` () : unit =
        // Four users and groups, one of which is the configured pair, so that a
        // stated owner is sometimes the configured one and usually not.
        let configured : InodeOwner =
            {
                User = UserId.parseOrFail "test" 37u
                Group = GroupId.parseOrFail "test" 38u
            }

        let ownerGen : Gen<InodeOwner> =
            Gen.elements
                [
                    configured
                    {
                        User = UserId.root
                        Group = GroupId.parseOrFail "test" 0u
                    }
                    {
                        User = UserId.parseOrFail "test" 37u
                        Group = GroupId.parseOrFail "test" 0u
                    }
                    {
                        User = UserId.parseOrFail "test" 1001u
                        Group = GroupId.parseOrFail "test" 38u
                    }
                ]

        let statedGen : Gen<InodeOwner option> =
            Gen.oneof [ Gen.constant None ; ownerGen |> Gen.map Some ]

        let rec entriesGen (depth : int) : Gen<Map<DirectoryEntryName, SeedEntry>> =
            gen {
                let! count = Gen.choose (0, (if depth = 0 then 1 else 3))
                let! kinds = Gen.listOfLength count (Gen.choose (0, 2))
                let! owners = Gen.listOfLength count statedGen

                let! entries =
                    List.zip kinds owners
                    |> List.mapi (fun i (kind, stated) ->
                        let entryName = name $"e%d{i}"

                        match kind with
                        | 0 ->
                            Gen.constant (
                                entryName,
                                SeedEntry.File (noBytes, SeedEntry.defaultPermsForRegularFile, stated)
                            )
                        | 1 ->
                            Gen.constant (
                                entryName,
                                SeedEntry.Symlink (SymlinkTarget.parseOrFail "test" "nowhere", stated)
                            )
                        | _ ->
                            entriesGen (depth - 1)
                            |> Gen.map (fun children ->
                                entryName, SeedEntry.Directory (children, SeedEntry.defaultPermsForDirectory, stated)
                            )
                    )
                    |> Gen.sequenceToList

                return Map.ofList entries
            }

        /// Every path the seed names, with the owner it states or the
        /// configured one: an entry's owner is never its directory's.
        let rec expected (prefix : string) (entries : Map<DirectoryEntryName, SeedEntry>) : (string * InodeOwner) list =
            entries
            |> Map.toList
            |> List.collect (fun (entryName, entry) ->
                let path = prefix + "/" + DirectoryEntryName.toEscaped entryName

                match entry with
                | SeedEntry.File (_, _, stated)
                | SeedEntry.Symlink (_, stated) -> [ path, Option.defaultValue configured stated ]
                | SeedEntry.Directory (children, _, stated) ->
                    (path, Option.defaultValue configured stated) :: expected path children
            )

        let property (seed : Map<DirectoryEntryName, SeedEntry>, rootOwner : InodeOwner option) : unit =
            for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
                let kernel =
                    KernelConfig.toKernel
                        { KernelConfig.Default with
                            UnixPlatform = platform
                            UserId = Some 37u
                            GroupId = Some 38u
                            FileSystem = seed
                            FileSystemRootOwner = rootOwner
                        }

                ownerAt kernel "/" |> shouldEqual (Option.defaultValue configured rootOwner)

                let paths = expected "" seed

                for path, owner in paths do
                    (path, ownerAt kernel path) |> shouldEqual (path, owner)

                // Nothing on the root filesystem but the root and the seed's own
                // entries; the device filesystem the kernel mounts at boot is
                // not the seed's.
                VirtualFileSystem.inodes (UnixSystem.fileSystem kernel.System)
                |> Map.filter (fun number _ ->
                    (VirtualFileSystem.mountedRootOf number (UnixSystem.fileSystem kernel.System)).IsNone
                )
                |> Map.count
                |> shouldEqual (List.length paths + 1)

        let gen =
            gen {
                let! seed = entriesGen 3
                let! rootOwner = statedGen
                return seed, rootOwner
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``KernelConfig refuses credentials no process could hold, naming the knob`` () : unit =
        let refusal (config : KernelConfig) : string =
            (Assert.Throws<System.Exception> (fun () -> KernelConfig.toKernel config |> ignore<EmulatedKernel>)).Message

        refusal
            { KernelConfig.Default with
                UserId = Some System.UInt32.MaxValue
            }
        |> fun message ->
            message.StartsWith ("KernelConfig.UserId: ", System.StringComparison.Ordinal)
            |> shouldEqual true

        refusal
            { KernelConfig.Default with
                GroupId = Some System.UInt32.MaxValue
            }
        |> fun message ->
            message.StartsWith ("KernelConfig.GroupId: ", System.StringComparison.Ordinal)
            |> shouldEqual true

        refusal
            { KernelConfig.Default with
                SupplementaryGroups = [ 5u ; System.UInt32.MaxValue ]
            }
        |> fun message ->
            message.StartsWith ("KernelConfig.SupplementaryGroups: ", System.StringComparison.Ordinal)
            |> shouldEqual true

        // Darwin's NGROUPS_MAX is 16.
        refusal
            { KernelConfig.Default with
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
                SupplementaryGroups = List.init 17 (fun i -> uint32 (100 + i))
            }
        |> fun message ->
            message.StartsWith ("KernelConfig: ", System.StringComparison.Ordinal)
            |> shouldEqual true

    [<Test>]
    let ``KernelConfig refuses a umask its flavour never stores, naming the knob`` () : unit =
        // Linux's umask(2) keeps only 0o777 of its argument, so no Linux process
        // has 0o7022; Darwin's keeps all twelve bits, so one there can.
        let configured (platform : SimulatedUnixPlatform) (bits : int) : KernelConfig =
            { KernelConfig.Default with
                UnixPlatform = platform
                Umask = PermissionBits.parseOrFail "test" bits
            }

        let message =
            (Assert.Throws<System.Exception> (fun () ->
                KernelConfig.toKernel (configured SimulatedUnixPlatform.linuxX64 0o7022)
                |> ignore<EmulatedKernel>
            ))
                .Message

        message.StartsWith ("KernelConfig.Umask: ", System.StringComparison.Ordinal)
        |> shouldEqual true

        for platform, bits in
            [
                SimulatedUnixPlatform.linuxX64, 0o777
                SimulatedUnixPlatform.macOsArm64, 0o7022
            ] do
            let kernel = KernelConfig.toKernel (configured platform bits)

            (UnixSystem.fileModeCreationMask kernel.System)
            |> shouldEqual (PermissionBits.parseOrFail "test" bits)

            EmulatedKernel.checkInvariants kernel |> shouldEqual []

    [<Test>]
    let ``KernelConfig applies the current directory whatever else it sets`` () : unit =
        let config =
            { KernelConfig.Default with
                FileSystem = seed
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
                CurrentDirectory = absolute "/outer/inner"
            }

        let kernel = KernelConfig.toKernel config

        (UnixSystem.currentDirectoryInode kernel.System)
        |> shouldEqual (inodeOf kernel "/outer/inner")

        EmulatedKernel.checkInvariants kernel |> shouldEqual []
