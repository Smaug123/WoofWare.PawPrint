namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The device filesystem the kernel mounts over the root's `dev` at boot: a
/// devtmpfs holding `/dev/null` and `/dev/urandom` on Linux, and Darwin's devfs,
/// which is not modelled and so refuses every path that reaches it.
///
/// The constants are the rows `devices.c` and `mountpoint.c` measured on Linux
/// 6.18.5 (a devtmpfs at `/dev`, as root and as uid 1000) and Darwin 27.0 (uid
/// 501), in `docs/plans/2026-08-23-posix-kernel-extraction/`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDeviceFileSystem =

    let private context : string = "TestDeviceFileSystem"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private rootOwner : InodeOwner =
        {
            User = UserId.root
            Group = GroupId.parseOrFail context 0u
        }

    let private asRoot (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixSystem.withCredentials context (Credentials.ofIds UserId.root (GroupId.parseOrFail context 0u) []) system

    let private reading : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            Directory = false
        }

    let private creating : OpenFlags =
        { reading with
            Access = FileAccessMode.WriteOnly
            Create = true
        }

    /// A machine booted at `bootTime` whose root, owned by root as a real one
    /// is, holds a file `f` and a directory `d`, and whose process is the
    /// flavour's default unprivileged user.
    let private bootedAt (platform : SimulatedUnixPlatform) (bootTime : UnixTimestamp) : UnixSystem<int, string> =
        let seed =
            Map.ofList
                [
                    name "f", SeedEntry.File (ImmutableArray.Empty, PermissionBits.parseOrFail context 0o666, None)
                    name "d", SeedEntry.Directory (Map.empty, PermissionBits.parseOrFail context 0o777, None)
                ]

        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

        match UnixSystem.withFileSystemAndCurrentDirectory bootTime rootOwner seed AbsoluteUnixPath.root system with
        | Ok system -> system
        | Error fault -> failwith $"booting failed: %A{fault}"

    let private booted (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        bootedAt platform (UnixTimestamp.ofSeconds 1_700_000_000L)

    let private stat (path : string) (system : UnixSystem<int, string>) : Result<FileStatusAnswer, StatRefusal> =
        UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText path) system

    let private reported (path : string) (system : UnixSystem<int, string>) : FileStatus =
        match stat path system with
        | Ok (FileStatusAnswer.Reported status) -> status
        | other -> failwith $"stat %s{path}: expected a status, got %A{other}"

    let private failed (answer : SyscallAnswer * UnixSystem<int, string>) : UnixError option =
        match fst answer with
        | SyscallAnswer.Failed error -> Some error
        | SyscallAnswer.Completed _ -> None

    /// A name that is no device's: generated, so that no particular unlisted
    /// name is all that is checked.
    let private unknownNameGen : Gen<string> =
        gen {
            let! length = Gen.choose (1, 20)

            let! characters =
                Gen.elements ([ 'a' .. 'z' ] @ [ '0' .. '9' ] @ [ '.' ; '-' ; '_' ])
                |> Gen.listOfLength length

            return System.String (Array.ofList characters)
        }
        |> Gen.filter (fun text -> text <> "." && text <> ".." && text <> "null" && text <> "urandom")

    // ------------------------------------------------------------------ stat

    [<Test>]
    let ``each device node reports the measured stat fields`` () : unit =
        let property (seconds : int64) : unit =
            let bootTime = UnixTimestamp.ofSeconds (abs seconds % 2_000_000_000L)
            let system = bootedAt linux bootTime

            for path, rawDevice in [ "/dev/null", 259L ; "/dev/urandom", 265L ] do
                let status = reported path system
                status.Mode |> shouldEqual 0o020666
                status.LinkCount |> shouldEqual 1L
                status.UserId |> shouldEqual UserId.root
                status.GroupId |> shouldEqual (GroupId.parseOrFail context 0u)
                status.Size |> shouldEqual 0L
                status.SpecialFileDevice |> shouldEqual rawDevice
                // devtmpfs's own device, which is not the root filesystem's.
                status.DeviceId |> shouldEqual DevtmpfsMount.defaults.DeviceId
                status.DeviceId |> shouldNotEqual (reported "/f" system).DeviceId
                // Made at boot, and moved by nothing since.
                status.AccessTime |> shouldEqual bootTime
                status.ModificationTime |> shouldEqual bootTime
                status.StatusChangeTime |> shouldEqual bootTime
                status.BirthTime |> shouldEqual None
                status.FileFlags |> shouldEqual None

                UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofText path) system
                |> shouldEqual (Ok (FileStatusAnswer.Reported status))

            (reported "/dev/null" system).Inode
            |> shouldNotEqual (reported "/dev/urandom" system).Inode

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.choose (0, System.Int32.MaxValue) |> Gen.map int64)) property)

    [<Test>]
    let ``the device filesystem's root is refused, for its size and link count`` () : unit =
        let system = booted linux

        for path in [ "/dev" ; "/dev/" ; "/dev/." ; "/d/../dev" ] do
            match stat path system with
            | Error (StatRefusal.DeviceFileSystemRoot _) -> ()
            | other -> failwith $"stat %s{path}: expected the device root's refusal, got %A{other}"

    [<Test>]
    let ``a name the device filesystem does not hold is refused, final or not`` () : unit =
        let system = booted linux

        let property (unknown : string) : unit =
            for path in [ $"/dev/%s{unknown}" ; $"/dev/%s{unknown}/x" ; $"/d/../dev/%s{unknown}" ] do
                match stat path system with
                | Error (StatRefusal.Path (PathRefusal.UnmodelledDeviceName (_, refused))) ->
                    refused |> shouldEqual (name unknown)
                | other -> failwith $"stat %s{path}: expected a refusal, got %A{other}"

            match OpenFlagWords.openPath creating (PathArg.ofText $"/dev/%s{unknown}") 0o644 system with
            | Error (OpenRefusal.Path (PathRefusal.UnmodelledDeviceName _)) -> ()
            | other -> failwith $"open(O_CREAT) /dev/%s{unknown}: expected a refusal, got %A{other}"

            match UnixNamespace.mkdir (PathArg.ofText $"/dev/%s{unknown}") 0o755 system with
            | Error (PathRefusal.UnmodelledDeviceName _) -> ()
            | other -> failwith $"mkdir /dev/%s{unknown}: expected a refusal, got %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen unknownNameGen) property)

    [<Test>]
    let ``a path that continues through a device is ENOTDIR`` () : unit =
        let system = booted linux

        for path in [ "/dev/null/x" ; "/dev/null/" ; "/dev/urandom/." ] do
            stat path system |> shouldEqual (Ok (FileStatusAnswer.Failed UnixError.ENOTDIR))

    [<Test>]
    let ``dot-dot from the device filesystem's root is the root filesystem's root`` () : unit =
        let system = booted linux

        (reported "/dev/../dev/null" system)
        |> shouldEqual (reported "/dev/null" system)

        (reported "/dev/.." system) |> shouldEqual (reported "/" system)

        let _, system = Answered.chdir (PathArg.ofText "/dev") system

        UnixPathResolution.currentDirectoryPath system
        |> shouldEqual (Some (AbsoluteUnixPath.parseOrFail context "/dev"))

        (reported "null" system) |> shouldEqual (reported "/dev/null" system)

        let _, system = Answered.chdir (PathArg.ofText "..") system

        UnixPathResolution.currentDirectoryPath system
        |> shouldEqual (Some AbsoluteUnixPath.root)

    // ------------------------------------------------------------------ statfs

    [<Test>]
    let ``statfs of the device filesystem is devtmpfs's, through a node and through its root`` () : unit =
        let system = booted linux

        let expected =
            FileSystemStatisticsAnswer.Reported (
                FileSystemStatistics.Linux
                    {
                        Type = 0x01021994L
                        Geometry =
                            Ok
                                {
                                    BlockSize = 4096L
                                    FragmentSize = 4096L
                                    NameLengthLimit = 255L
                                }
                        Capacity = Error CapacityRefusal.DeviceFileSystem
                        FileSystemId =
                            Ok
                                {
                                    First = int32 0x544366deu
                                    Second = int32 0xad9235deu
                                }
                        Flags = Ok 0x1022L
                    }
            )

        for path in [ "/dev" ; "/dev/null" ; "/dev/urandom" ] do
            Answered.statfs (PathArg.ofText path) system |> shouldEqual expected

        let fd, system =
            match Answered.openPath DirectoryReading.flags (UnixPath.parseOrFail context "/dev") 0 system with
            | SyscallAnswer.Completed fd, system -> int fd, system
            | other -> failwith $"opening /dev: %A{other}"

        UnixPathResolution.fstatfs fd system |> shouldEqual expected

        // The root filesystem is not the device filesystem.
        Answered.statfs (PathArg.ofText "/f") system |> shouldNotEqual expected

    // ------------------------------------------------------------- listing

    [<Test>]
    let ``a listing of the root reports dev with the inode it covers`` () : unit =
        for platform in [ linux ; darwin ] do
            let system = booted platform

            let fd, system =
                match DirectoryReading.openDirectory (UnixPath.ofAbsolute AbsoluteUnixPath.root) system with
                | Ok fd, system -> fd, system
                | Error error, _ -> failwith $"opening /: %O{error}"

            let records, _ = DirectoryReading.drain fd system

            let dev =
                records
                |> List.filter (fun record -> record.Name = DirectoryStreamName.Entry (name "dev"))
                |> List.exactlyOne

            dev.Kind |> shouldEqual DirectoryEntryKind.Directory

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux ->
                // `stat("/dev")` names the mounted root, which is refused, but
                // the refusal names the inode it would have reported.
                match stat "/dev" system with
                | Error (StatRefusal.DeviceFileSystemRoot mountedRoot) -> dev.Inode |> shouldNotEqual mountedRoot
                | other -> failwith $"stat /dev: expected the device root's refusal, got %A{other}"
            | SimulatedUnixFlavour.Darwin -> ()

    [<Test>]
    let ``a listing of the device filesystem is refused, and so is its status`` () : unit =
        let system = booted linux

        let fd, system =
            match DirectoryReading.openDirectory (UnixPath.parseOrFail context "/dev") system with
            | Ok fd, system -> fd, system
            | Error error, _ -> failwith $"opening /dev: %O{error}"

        match UnixNamespace.readDirectoryEntry fd system with
        | Error (ReadDirectoryRefusal.DeviceFileSystem _) -> ()
        | other -> failwith $"reading /dev: expected a refusal, got %A{other}"

        match UnixPathResolution.fstat fd system with
        | Error (FStatRefusal.DeviceFileSystemRoot _) -> ()
        | other -> failwith $"fstat of /dev: expected a refusal, got %A{other}"

    // ---------------------------------------------------- the mount boundary

    [<Test>]
    let ``a rename across the mount is EXDEV, before either final name is looked up`` () : unit =
        // Measured on Linux as uid 1000 and as root alike, whether or not the
        // source exists and whether or not the destination is a name a real
        // devtmpfs holds.
        for system in [ booted linux ; asRoot (booted linux) ] do
            let rename (source : string) (destination : string) =
                UnixNamespace.rename (PathArg.ofText source) (PathArg.ofText destination) system

            for source, destination in
                [
                    "/f", "/dev/x"
                    "/d", "/dev/x"
                    "/dev/null", "/d/x"
                    "/dev/null", "/f"
                    "/f", "/dev/null"
                    "/dev/nonexistent", "/d/x"
                    "/nonexistent", "/dev/new"
                ] do
                match rename source destination with
                | Ok answer -> failed answer |> shouldEqual (Some UnixError.EXDEV)
                | Error refusal ->
                    failwith $"rename %s{source} -> %s{destination} was refused: %s{RenameRefusal.describe refusal}"

    [<Test>]
    let ``the mount point itself follows the measured rows`` () : unit =
        // The root is root's, mode 0755: uid 1000 may not change its names.
        let unprivileged = booted linux
        let root = asRoot unprivileged

        let removal (result : Result<SyscallAnswer * UnixSystem<int, string>, RemovalRefusal>) =
            match result with
            | Ok answer -> failed answer
            | Error refusal -> failwith $"refused: %s{RemovalRefusal.describe refusal}"

        let renamed (system : UnixSystem<int, string>) (destination : string) =
            match UnixNamespace.rename (PathArg.ofText "/dev") (PathArg.ofText destination) system with
            | Ok answer -> failed answer
            | Error refusal -> failwith $"refused: %s{RenameRefusal.describe refusal}"

        removal (UnixNamespace.rmdir (PathArg.ofText "/dev") unprivileged)
        |> shouldEqual (Some UnixError.EACCES)

        removal (UnixNamespace.rmdir (PathArg.ofText "/dev") root)
        |> shouldEqual (Some UnixError.EBUSY)

        removal (UnixNamespace.unlink (PathArg.ofText "/dev") unprivileged)
        |> shouldEqual (Some UnixError.EACCES)

        removal (UnixNamespace.unlink (PathArg.ofText "/dev") root)
        |> shouldEqual (Some UnixError.EISDIR)

        for system in [ unprivileged ; root ] do
            removal (UnixNamespace.rmdir (PathArg.ofText "/dev/.") system)
            |> shouldEqual (Some UnixError.EINVAL)

        renamed unprivileged "/devx" |> shouldEqual (Some UnixError.EACCES)
        renamed root "/devx" |> shouldEqual (Some UnixError.EBUSY)

        // Unmeasured: the mount point as the thing displaced.
        match UnixNamespace.rename (PathArg.ofText "/d") (PathArg.ofText "/dev") root with
        | Error (RenameRefusal.MountPoint _) -> ()
        | other -> failwith $"rename /d -> /dev: expected a refusal, got %A{other}"

        UnixNamespace.mkdir (PathArg.ofText "/dev") 0o755 unprivileged
        |> Result.map failed
        |> shouldEqual (Ok (Some UnixError.EEXIST))

    [<Test>]
    let ``no name in the device filesystem is created, removed or moved`` () : unit =
        let unprivileged = booted linux
        let root = asRoot unprivileged

        // Its root is root's, mode 0755: uid 1000 is refused by the bits.
        match UnixNamespace.unlink (PathArg.ofText "/dev/null") unprivileged with
        | Ok answer -> failed answer |> shouldEqual (Some UnixError.EACCES)
        | Error refusal -> failwith $"refused: %s{RemovalRefusal.describe refusal}"

        match UnixNamespace.unlink (PathArg.ofText "/dev/null") root with
        | Error (RemovalRefusal.DeviceFileSystem (_, removed)) -> removed |> shouldEqual (name "null")
        | other -> failwith $"unlink /dev/null as root: expected a refusal, got %A{other}"

        match UnixNamespace.rename (PathArg.ofText "/dev/null") (PathArg.ofText "/dev/urandom") root with
        | Error (RenameRefusal.DeviceFileSystem _) -> ()
        | other -> failwith $"rename within /dev as root: expected a refusal, got %A{other}"

        match UnixNamespace.rename (PathArg.ofText "/dev/null") (PathArg.ofText "/dev/urandom") unprivileged with
        | Ok answer -> failed answer |> shouldEqual (Some UnixError.EACCES)
        | Error refusal -> failwith $"refused: %s{RenameRefusal.describe refusal}"

        // A device's name is taken.
        match
            OpenFlagWords.openPath
                { creating with
                    Exclusive = true
                }
                (PathArg.ofText "/dev/null")
                0o644
                root
        with
        | Ok answer -> failed answer |> shouldEqual (Some UnixError.EEXIST)
        | Error refusal -> failwith $"refused: %s{OpenRefusal.describe refusal}"

    [<Test>]
    let ``FICLONE between the device filesystem and the root filesystem is EXDEV`` () : unit =
        // Linux compares the two descriptors' mounts first, then whether either
        // is a directory: a directory on /dev against a file on / is EXDEV, as
        // a pipe against a file is.
        let system = booted linux

        let dev, system =
            match DirectoryReading.openDirectory (UnixPath.parseOrFail context "/dev") system with
            | Ok fd, system -> fd, system
            | Error error, _ -> failwith $"opening /dev: %O{error}"

        let directory, system =
            match DirectoryReading.openDirectory (UnixPath.parseOrFail context "/d") system with
            | Ok fd, system -> fd, system
            | Error error, _ -> failwith $"opening /d: %O{error}"

        let file, system =
            match
                Answered.openPath
                    { reading with
                        Access = FileAccessMode.ReadWrite
                    }
                    (UnixPath.parseOrFail context "/f")
                    0
                    system
            with
            | SyscallAnswer.Completed fd, system -> int fd, system
            | other -> failwith $"opening /f: %A{other}"

        UnixDescriptor.fileClone file dev system |> shouldEqual (Ok UnixError.EXDEV)
        UnixDescriptor.fileClone dev file system |> shouldEqual (Ok UnixError.EXDEV)
        // On one filesystem, the directory is what answers.
        UnixDescriptor.fileClone file directory system
        |> shouldEqual (Ok UnixError.EISDIR)

    // ------------------------------------------------------- the node itself

    [<Test>]
    let ``a device node is a node, whatever asks`` () : unit =
        let system = booted linux

        match OpenFlagWords.openPath reading (PathArg.ofText "/dev/urandom") 0 system with
        | Error (OpenRefusal.CharacterDevice (_, CharacterDevice.URandom)) -> ()
        | other -> failwith $"open /dev/urandom: expected a refusal until devices open, got %A{other}"

        match
            OpenFlagWords.openPath
                { reading with
                    Directory = true
                }
                (PathArg.ofText "/dev/null")
                0
                system
        with
        | Ok answer -> failed answer |> shouldEqual (Some UnixError.ENOTDIR)
        | Error refusal -> failwith $"refused: %s{OpenRefusal.describe refusal}"

        UnixPathResolution.access (PathArg.ofText "/dev/urandom") 6 system
        |> shouldEqual (Ok (SyscallAnswer.Completed 0L))

        UnixPathResolution.access (PathArg.ofText "/dev/urandom") 1 system
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EACCES))

        UnixNamespace.readlink (PathArg.ofText "/dev/null") UserBuffer.Mapped 16 system
        |> shouldEqual (Ok (ReadLinkAnswer.Failed UnixError.EINVAL))

        Answered.chdir (PathArg.ofText "/dev/null") system
        |> failed
        |> shouldEqual (Some UnixError.ENOTDIR)

        // Only the owner may change the mode, and root owns it.
        match UnixPathResolution.chmod (PathArg.ofText "/dev/null") 0o666 system with
        | Ok answer -> failed answer |> shouldEqual (Some UnixError.EPERM)
        | Error refusal -> failwith $"refused: %s{ChModRefusal.describe refusal}"

        match UnixPathResolution.chmod (PathArg.ofText "/dev/null") 0o600 (asRoot system) with
        | Ok (SyscallAnswer.Completed 0L, changed) -> (reported "/dev/null" changed).Mode |> shouldEqual 0o020600
        | other -> failwith $"chmod /dev/null as root: %A{other}"

    // --------------------------------------------------------------- boot

    [<Test>]
    let ``a booted system is sound`` () : unit =
        for platform in [ linux ; darwin ] do
            UnixSystem.checkInvariants (booted platform) |> shouldEqual []

            UnixSystem.checkInvariants (
                UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            )
            |> shouldEqual []

    [<Test>]
    let ``a seed's empty dev is the directory the mount covers, and a populated one is refused`` () : unit =
        let system : UnixSystem<int, string> =
            UnixSystem.initial linux UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let epoch = UnixTimestamp.ofSeconds 0L

        let emptyDev = Map.ofList [ name "dev", SeedEntry.directory Map.empty ]

        match UnixSystem.withFileSystemAndCurrentDirectory epoch rootOwner emptyDev AbsoluteUnixPath.root system with
        | Ok booted ->
            (reported "/dev/null" booted).Mode |> shouldEqual 0o020666
            UnixSystem.checkInvariants booted |> shouldEqual []
        | Error fault -> failwith $"an empty dev was refused: %A{fault}"

        for populated in
            [
                SeedEntry.directory (Map.ofList [ name "x", SeedEntry.file ImmutableArray.Empty ])
                SeedEntry.file ImmutableArray.Empty
            ] do
            UnixSystem.withFileSystemAndCurrentDirectory
                epoch
                rootOwner
                (Map.ofList [ name "dev", populated ])
                AbsoluteUnixPath.root
                system
            |> Result.map ignore
            |> shouldEqual (Error (CurrentDirectoryFault.SeedCoversDeviceFileSystem (name "dev")))

    [<Test>]
    let ``checkInvariants reports each forged mount defect`` () : unit =
        let system = booted linux
        let vfs = system.Machine.FileSystem

        let mountedRoot =
            match stat "/dev" system with
            | Error (StatRefusal.DeviceFileSystemRoot root) -> root
            | other -> failwith $"stat /dev: %A{other}"

        let mount =
            match VirtualFileSystem.mountOf mountedRoot vfs with
            | Some mount -> mount
            | None -> failwith "/dev is not a mount"

        let null' = (reported "/dev/null" system).Inode
        let file = (reported "/f" system).Inode

        VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual []

        // A node recorded as on the root filesystem.
        VirtualFileSystem.Unchecked.setMountMember null' None vfs
        |> VirtualFileSystem.checkInvariants Set.empty
        |> shouldEqual
            [
                VirtualFileSystemDefect.MountMembershipMismatch (
                    mountedRoot,
                    Set.ofList [ mountedRoot ; (reported "/dev/urandom" system).Inode ],
                    Set.ofList [ mountedRoot ; null' ; (reported "/dev/urandom" system).Inode ]
                )
            ]

        // A covered number that an inode has.
        VirtualFileSystem.Unchecked.setMount
            mountedRoot
            (Some
                { mount with
                    Covered = file
                })
            vfs
        |> VirtualFileSystem.checkInvariants Set.empty
        |> shouldEqual [ VirtualFileSystemDefect.CoveredInodeInUse (mountedRoot, file) ]

        // A covered number the counter has not handed out.
        let unissued = VirtualFileSystem.nextInode vfs

        VirtualFileSystem.Unchecked.setMount
            mountedRoot
            (Some
                { mount with
                    Covered = unissued
                })
            vfs
        |> VirtualFileSystem.checkInvariants Set.empty
        |> shouldEqual [ VirtualFileSystemDefect.NextInodeNotFresh (unissued, unissued) ]

        // A mount whose root is a regular file.
        VirtualFileSystem.Unchecked.setMount file (Some mount) vfs
        |> VirtualFileSystem.checkInvariants Set.empty
        |> List.contains (VirtualFileSystemDefect.MountRootNotDirectory file)
        |> shouldEqual true

    // -------------------------------------------------------------- Darwin

    [<Test>]
    let ``on Darwin every path that reaches dev is refused`` () : unit =
        let system = booted darwin

        for path in [ "/dev" ; "/dev/null" ; "/dev/urandom" ; "/dev/x/y" ; "/d/../dev" ] do
            match stat path system with
            | Error (StatRefusal.Path (PathRefusal.UnmodelledFileSystem (_, MountedFileSystem.Devfs))) -> ()
            | other -> failwith $"stat %s{path} on Darwin: expected a refusal, got %A{other}"

        match UnixPathResolution.chdir (PathArg.ofText "/dev") system with
        | Error (PathRefusal.UnmodelledFileSystem _) -> ()
        | other -> failwith $"chdir /dev on Darwin: expected a refusal, got %A{other}"
