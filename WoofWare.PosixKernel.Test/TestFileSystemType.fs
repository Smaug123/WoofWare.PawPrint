namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `statfs(2)` and `fstatfs(2)`: the measured table of what they answer for
/// each kind of object on each mount, and the coherence rule between a mount
/// and the flavour claiming to have mounted it.
///
/// The measurements are in the headers of
/// `docs/plans/2026-08-23-posix-kernel-extraction/statfs-fields.c` and
/// `statfs-accounting.c`; the rows below restate them as literals rather than
/// deriving them from the code under test.
///
/// The host oracles compare only the fields that are facts of the object's
/// or filesystem's type. The library's filesystem is in memory and claims to
/// be whatever mount its client configures, where this host's mounts have
/// sizes, identities and options of their own.
[<TestFixture>]
module TestFileSystemType =

    [<DllImport("libc", SetLastError = true)>]
    extern int private pipe(int[] fds)

    [<DllImport("libc", SetLastError = true)>]
    extern int private socket(int domain, int kind, int protocol)

    /// Darwin's anonymous-inode object. Declared unconditionally: a `DllImport`
    /// binds on first call, so naming a symbol this host lacks costs nothing
    /// until something calls it.
    [<DllImport("libc", SetLastError = true)>]
    extern int private kqueue()

    /// Linux's.
    [<DllImport("libc", SetLastError = true)>]
    extern int private epoll_create1(int flags)

    [<DllImport("libc")>]
    extern int private close(int fd)

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    /// Darwin arm64's `fstat`, whose `struct stat` begins with a 4-byte
    /// `st_dev`.
    [<DllImport("libc", EntryPoint = "fstat", SetLastError = true)>]
    extern int private darwinFstat(int fd, byte[] buffer)

    let private context : string = "TestFileSystemType"

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private utf8 (name : string) : UnixByteString =
        match UnixByteString.ofString name with
        | Ok name -> name
        | Error defect -> failwith $"test bug: %s{name} is not a Unix string: %O{defect}"

    /// The machine a simulated process boots with on `flavour`'s platform.
    let private machineOn (flavour : SimulatedUnixFlavour) : UnixMachineState =
        (UnixSystem.initial<int, string> (HostPlatform.platformOf flavour)).Machine

    let private everyFlavour : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private everyPlatform : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// The (flavour, mount) pairs that describe one machine, written out rather
    /// than filtered through `isReportableUnder` so that this list is an oracle
    /// for that function rather than a restatement of it.
    let private everyCoherentPair : (SimulatedUnixFlavour * EmulatedFileSystemType) list =
        [
            SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Tmpfs
            SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Nfs
            SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Apfs
            SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Nfs
        ]

    let private everyIncoherentPair : (SimulatedUnixFlavour * EmulatedFileSystemType) list =
        [
            SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Tmpfs
            SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Apfs
        ]

    let private everyFileSystemType : EmulatedFileSystemType list =
        [
            EmulatedFileSystemType.Tmpfs
            EmulatedFileSystemType.Apfs
            EmulatedFileSystemType.Nfs
        ]

    // ------------------------------------------------------------ the table

    /// Every tmpfs field measured on both Linux kernels, with the `f_fsid` the
    /// mount was configured with. Capacity is that of a tmpfs with no limits;
    /// the flags are `ST_VALID | ST_NOATIME`.
    let private measuredTmpfs (fsid : FileSystemId) : FileSystemStatistics =
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
                Capacity =
                    Ok
                        {
                            Blocks = 0UL
                            FreeBlocks = 0UL
                            AvailableBlocks = 0UL
                            Files = 0UL
                            FreeFiles = 0UL
                        }
                FileSystemId = Ok fsid
                Flags = Ok 0x420L
            }

    /// Every APFS field measured on Darwin, with the fields the mount was
    /// configured with. The flags are local, root, volfs, journaled,
    /// multilabel and no-atime; the `f_fsid` is `{ st_dev, f_type }` for the
    /// `st_dev` every inode here reports.
    let private measuredApfs (apfs : ApfsMount) : FileSystemStatistics =
        FileSystemStatistics.Darwin
            {
                Type = 0x1Au
                TypeName = utf8 "apfs"
                Geometry =
                    Ok
                        {
                            BlockSize = 4096u
                            IoSize = apfs.IoSize
                        }
                Capacity = Error CapacityRefusal.Apfs
                FileSystemId =
                    Ok
                        {
                            First = 0x1000001
                            Second = 0x1A
                        }
                Mount =
                    Ok
                        {
                            Owner = apfs.Owner
                            Flags = 0x1480D000u
                            ExtendedFlags = 0u
                            SubType = 0u
                            MountedOn = utf8 "/"
                            MountedFrom = apfs.MountedFrom
                        }
            }

    let private measuredNfs (flavour : SimulatedUnixFlavour) : FileSystemStatistics =
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            FileSystemStatistics.Linux
                {
                    Type = 0x6969L
                    Geometry = Error GeometryRefusal.Nfs
                    Capacity = Error CapacityRefusal.Nfs
                    FileSystemId = Error FileSystemIdRefusal.Nfs
                    Flags = Error MountFieldsRefusal.Nfs
                }
        | SimulatedUnixFlavour.Darwin ->
            FileSystemStatistics.Darwin
                {
                    Type = 2u
                    TypeName = utf8 "nfs"
                    Geometry = Error GeometryRefusal.Nfs
                    Capacity = Error CapacityRefusal.Nfs
                    FileSystemId = Error FileSystemIdRefusal.Nfs
                    Mount = Error MountFieldsRefusal.Nfs
                }

    /// What `fstatfs` answers for an object on no mount: on Linux, the
    /// pseudo-filesystem's fields, measured identical on both kernels except
    /// the `f_fsid`; on Darwin, EINVAL.
    let private measuredNotOnAMount
        (flavour : SimulatedUnixFlavour)
        (fileSystem : PseudoFileSystem)
        : FileSystemStatisticsAnswer
        =
        match flavour with
        | SimulatedUnixFlavour.Darwin -> FileSystemStatisticsAnswer.Failed UnixError.EINVAL
        | SimulatedUnixFlavour.Linux ->
            let magic =
                match fileSystem with
                | PseudoFileSystem.Pipe -> 0x50495045L
                | PseudoFileSystem.Socket -> 0x534F434BL
                | PseudoFileSystem.AnonymousInode -> 0x09041934L

            FileSystemStatistics.Linux
                {
                    Type = magic
                    Geometry =
                        Ok
                            {
                                BlockSize = 4096L
                                FragmentSize = 4096L
                                NameLengthLimit = 255L
                            }
                    Capacity =
                        Ok
                            {
                                Blocks = 0UL
                                FreeBlocks = 0UL
                                AvailableBlocks = 0UL
                                Files = 0UL
                                FreeFiles = 0UL
                            }
                    FileSystemId = Error (FileSystemIdRefusal.PseudoFileSystemDevice fileSystem)
                    Flags = Ok 0x20L
                }
            |> FileSystemStatisticsAnswer.Reported

    // ------------------------------------------------------------ the model

    let private name (s : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context s

    /// `f`, a file; `d`, a directory; `ld`, a link to `d`; `dang`, a link to
    /// nothing; and `loop`, a link to itself.
    let private tree : Map<DirectoryEntryName, SeedEntry> =
        let link (target : string) =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target)

        Map.ofList
            [
                name "f", SeedEntry.file (ImmutableArray.Create<byte> [| 1uy ; 2uy ; 3uy |])
                name "d", SeedEntry.directory Map.empty
                name "ld", link "d"
                name "dang", link "missing"
                name "loop", link "loop"
            ]

    let private systemWith (platform : SimulatedUnixPlatform) (mount : EmulatedMount) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        let system =
            { system with
                Machine = UnixMachineState.withMount (Some mount) system.Machine
            }

        match
            UnixSystem.withFileSystemAndCurrentDirectory
                (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                tree
                AbsoluteUnixPath.root
                system
        with
        | Ok system -> system
        | Error fault -> failwith $"test bug: the tree does not seed: %A{fault}"

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

    let private openPath (path : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixNamespace.openPath reading (UnixPath.parseOrFail context path) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | SyscallAnswer.Failed error, _ -> failwith $"test bug: open %s{path} failed with %O{error}"

    /// `fstatfs` on the file, the directory and the root, and `statfs` on each
    /// of their paths and through the link: every one of them is on the one
    /// mount, so every one has the same answer.
    let private everyAnswerOnTheMount (system : UnixSystem<int, string>) : FileSystemStatisticsAnswer list =
        let fileFd, system = openPath "/f" system
        let dirFd, system = openPath "/d" system
        let rootFd, system = openPath "/" system

        [
            UnixPathResolution.fstatfs fileFd system
            UnixPathResolution.fstatfs dirFd system
            UnixPathResolution.fstatfs rootFd system
            UnixPathResolution.statfs (UnixPath.parseOrFail context "/f") system
            UnixPathResolution.statfs (UnixPath.parseOrFail context "/d") system
            UnixPathResolution.statfs (UnixPath.parseOrFail context "/") system
            UnixPathResolution.statfs (UnixPath.parseOrFail context "ld") system
            UnixPathResolution.statfs (UnixPath.parseOrFail context "d/") system
        ]

    // ----------------------------------------------------------- type fields

    [<Test>]
    let ``each filesystem's type fields are the ones its kernel reports`` () : unit =
        // Linux's are the magic numbers in `<linux/magic.h>`, tmpfs's measured
        // on `/dev/shm`. Darwin's APFS row was measured by `fstatfs` on macOS
        // 26.6, and its NFS row read from the NFS kext's registration (see the
        // comments on `EmulatedFileSystemType.fieldsFor`).
        EmulatedFileSystemType.fieldsFor SimulatedUnixFlavour.Linux EmulatedFileSystemType.Tmpfs
        |> shouldEqual (FileSystemTypeFields.Linux 0x01021994L)

        EmulatedFileSystemType.fieldsFor SimulatedUnixFlavour.Linux EmulatedFileSystemType.Nfs
        |> shouldEqual (FileSystemTypeFields.Linux 0x6969L)

        EmulatedFileSystemType.fieldsFor SimulatedUnixFlavour.Darwin EmulatedFileSystemType.Apfs
        |> shouldEqual (FileSystemTypeFields.Darwin (0x1Au, utf8 "apfs"))

        EmulatedFileSystemType.fieldsFor SimulatedUnixFlavour.Darwin EmulatedFileSystemType.Nfs
        |> shouldEqual (FileSystemTypeFields.Darwin (2u, utf8 "nfs"))

    [<Test>]
    let ``no two filesystems a flavour mounts share type fields`` () : unit =
        // A collision would make a configuration silently mean a different
        // one to any caller that tells mounts apart by type.
        for flavour in everyFlavour do
            let fields =
                everyCoherentPair
                |> List.filter (fst >> (=) flavour)
                |> List.map (fun (flavour, fsType) -> EmulatedFileSystemType.fieldsFor flavour fsType)

            fields |> List.distinct |> List.length |> shouldEqual (List.length fields)

    [<Test>]
    let ``type fields for a pair that describes no machine are refused`` () : unit =
        for flavour, fsType in everyIncoherentPair do
            Assert.Throws (fun () -> EmulatedFileSystemType.fieldsFor flavour fsType |> ignore<FileSystemTypeFields>)
            |> ignore<exn>

    // ------------------------------------------------ mount and flavour agree

    [<Test>]
    let ``every flavour's default is a filesystem that flavour can mount`` () : unit =
        // Without this, adding a flavour whose default was copied from its
        // neighbour would give a kernel that refuses its own default the moment
        // a host spells it out explicitly.
        for flavour in everyFlavour do
            let chosen = EmulatedFileSystemType.defaultFor flavour

            if not (EmulatedFileSystemType.isReportableUnder flavour chosen) then
                failwith $"%O{flavour} defaults to %O{chosen}, which it cannot report."

            EmulatedMount.fileSystemType (EmulatedMount.defaultFor flavour)
            |> shouldEqual chosen

    [<Test>]
    let ``each mount's default is a mount of that type`` () : unit =
        for fsType in everyFileSystemType do
            EmulatedMount.fileSystemType (EmulatedMount.defaultOf fsType)
            |> shouldEqual fsType

    [<Test>]
    let ``omitting the mount takes the flavour's own default`` () : unit =
        for flavour in everyFlavour do
            let kernel = machineOn flavour |> UnixMachineState.withMount None

            kernel.Mount |> shouldEqual (EmulatedMount.defaultFor flavour)

    [<Test>]
    let ``the kernel's platform and its mount always agree`` () : unit =
        // `fstatfs` answers a *file* from the mount and every other
        // descriptor from the platform's flavour, so a kernel carrying one of
        // each would report a combination no machine could produce. Asserted
        // on the record rather than on the setter's argument, because that is
        // what `fstatfs` reads.
        for flavour in everyFlavour do
            for requested in None :: List.map (EmulatedMount.defaultOf >> Some) everyFileSystemType do
                let permitted =
                    match requested with
                    | None -> true
                    | Some mount ->
                        EmulatedFileSystemType.isReportableUnder flavour (EmulatedMount.fileSystemType mount)

                if permitted then
                    let kernel = machineOn flavour |> UnixMachineState.withMount requested

                    let carried = SimulatedUnixPlatform.flavour kernel.UnixPlatform

                    if carried <> flavour then
                        failwith $"asked for %O{flavour}, but the kernel carries %O{carried}."

                    if
                        not (
                            EmulatedFileSystemType.isReportableUnder carried (EmulatedMount.fileSystemType kernel.Mount)
                        )
                    then
                        failwith
                            $"a kernel built as %O{flavour} from %O{requested} carries mount %O{kernel.Mount}, which %O{carried} cannot report."

    [<Test>]
    let ``a mount the flavour could not have is refused`` () : unit =
        // Both directions, because a guard that only ever refused one of them
        // would leave the other pair silently constructible.
        for flavour, fsType in everyIncoherentPair do
            let thrown =
                Assert.Throws (fun () ->
                    machineOn flavour
                    |> UnixMachineState.withMount (Some (EmulatedMount.defaultOf fsType))
                    |> ignore<UnixMachineState>
                )

            thrown.Message |> shouldContainText (string<EmulatedFileSystemType> fsType)

    [<Test>]
    let ``a mount the flavour does have is accepted`` () : unit =
        // The other half of the pair above: a guard that refused everything
        // would pass that test and break every host.
        for flavour, fsType in everyCoherentPair do
            let mount = EmulatedMount.defaultOf fsType
            let kernel = machineOn flavour |> UnixMachineState.withMount (Some mount)

            kernel.Mount |> shouldEqual mount

    // ------------------------------------------------------ the measured table

    [<Test>]
    let ``every file on a tmpfs reports the measured table, with the configured fsid`` () : unit =
        let property (first : int32) (second : int32) (onArm : bool) : unit =
            let platform =
                if onArm then
                    SimulatedUnixPlatform.linuxArm64
                else
                    SimulatedUnixPlatform.linuxX64

            let fsid =
                {
                    First = first
                    Second = second
                }

            let expected = FileSystemStatisticsAnswer.Reported (measuredTmpfs fsid)

            for answer in
                everyAnswerOnTheMount (
                    systemWith
                        platform
                        (EmulatedMount.Tmpfs
                            {
                                FileSystemId = fsid
                            })
                ) do
                answer |> shouldEqual expected

        Check.One (propertyConfig, property)

    [<Test>]
    let ``every file on an APFS volume reports the measured table, with the configured mount`` () : unit =
        let property (ioSize : int32) (owner : uint32) (from : byte list) : unit =
            // A device name is any NUL-free bytes.
            let from =
                from
                |> List.map (fun b -> if b = 0uy then 1uy else b)
                |> Array.ofList
                |> ImmutableArray.Create<byte>

            let from =
                match UnixByteString.ofBytes from with
                | Ok from -> from
                | Error defect -> failwith $"test bug: %O{defect}"

            let apfs =
                {
                    IoSize = ioSize
                    Owner = owner
                    MountedFrom = from
                }

            let expected = FileSystemStatisticsAnswer.Reported (measuredApfs apfs)

            for answer in everyAnswerOnTheMount (systemWith SimulatedUnixPlatform.macOsArm64 (EmulatedMount.Apfs apfs)) do
                answer |> shouldEqual expected

        Check.One (propertyConfig, property)

    [<Test>]
    let ``every file on an NFS mount reports its type and nothing else`` () : unit =
        for platform in everyPlatform do
            let flavour = SimulatedUnixPlatform.flavour platform
            let expected = FileSystemStatisticsAnswer.Reported (measuredNfs flavour)

            for answer in everyAnswerOnTheMount (systemWith platform EmulatedMount.Nfs) do
                answer |> shouldEqual expected

    [<Test>]
    let ``the default mounts report the defaults they are documented with`` () : unit =
        // The literal defaults, so that a changed default is a visible act.
        everyAnswerOnTheMount (
            systemWith SimulatedUnixPlatform.linuxX64 (EmulatedMount.defaultFor SimulatedUnixFlavour.Linux)
        )
        |> List.distinct
        |> shouldEqual
            [
                FileSystemStatisticsAnswer.Reported (
                    measuredTmpfs
                        {
                            First = int32 0xda683e7au
                            Second = 0x5631524e
                        }
                )
            ]

        everyAnswerOnTheMount (
            systemWith SimulatedUnixPlatform.macOsArm64 (EmulatedMount.defaultFor SimulatedUnixFlavour.Darwin)
        )
        |> List.distinct
        |> shouldEqual
            [
                FileSystemStatisticsAnswer.Reported (
                    measuredApfs
                        {
                            IoSize = 1048576
                            Owner = 0u
                            MountedFrom = utf8 "/dev/disk1s1"
                        }
                )
            ]

    [<Test>]
    let ``an object on no mount reports its own filesystem, whatever the mount`` () : unit =
        // The standard streams through `fstatfs` itself, so that the wiring
        // from the descriptor table is covered; the objects this library
        // cannot open without a socket table through `ofObject`, which is
        // what `fstatfs` hands every held descriptor to.
        for platform in everyPlatform do
            let flavour = SimulatedUnixPlatform.flavour platform

            for fsType in everyCoherentPair |> List.filter (fst >> (=) flavour) |> List.map snd do
                let mount = EmulatedMount.defaultOf fsType
                let system = systemWith platform mount

                for fd in [ 0 ; 1 ; 2 ] do
                    UnixPathResolution.fstatfs fd system
                    |> shouldEqual (measuredNotOnAMount flavour PseudoFileSystem.Pipe)

                FileSystemStatistics.ofObject platform mount (OpenFileObject.Socket (SocketId 3L))
                |> shouldEqual (measuredNotOnAMount flavour PseudoFileSystem.Socket)

                FileSystemStatistics.ofObject platform mount OpenFileObject.AnonymousInode
                |> shouldEqual (measuredNotOnAMount flavour PseudoFileSystem.AnonymousInode)

    [<Test>]
    let ``a descriptor the process does not hold is EBADF`` () : unit =
        for platform in everyPlatform do
            let system =
                systemWith platform (EmulatedMount.defaultFor (SimulatedUnixPlatform.flavour platform))

            for fd in [ -1 ; 3 ; 4242 ; Int32.MaxValue ; Int32.MinValue ] do
                UnixPathResolution.fstatfs fd system
                |> shouldEqual (FileSystemStatisticsAnswer.Failed UnixError.EBADF)

    [<Test>]
    let ``Darwin's fsid names the device stat reports`` () : unit =
        // The rule measured on every APFS volume: `f_fsid.val[0]` is the
        // volume's `st_dev`. Checked against the model's own `fstat`, so the two
        // syscalls cannot drift apart.
        let system =
            systemWith SimulatedUnixPlatform.macOsArm64 (EmulatedMount.Apfs ApfsMount.defaults)

        let fd, system = openPath "/f" system

        let device =
            match UnixPathResolution.fstat fd system with
            | Ok (FileStatusAnswer.Reported status) -> status.DeviceId
            | other -> failwith $"fstat: %A{other}"

        match UnixPathResolution.fstatfs fd system with
        | FileSystemStatisticsAnswer.Reported (FileSystemStatistics.Darwin darwin) ->
            darwin.FileSystemId
            |> shouldEqual (
                Ok
                    {
                        First = int32 device
                        Second = int32 darwin.Type
                    }
            )
        | other -> failwith $"fstatfs: %A{other}"

    // ------------------------------------------------------- statfs's paths

    /// Measured on both flavours: a final symlink is followed, and each failure
    /// is the path's.
    let private measuredPathRows : (string * UnixError option) list =
        [
            "/missing", Some UnixError.ENOENT
            "/f/x", Some UnixError.ENOTDIR
            "/f/", Some UnixError.ENOTDIR
            "/dang", Some UnixError.ENOENT
            "/loop", Some UnixError.ELOOP
            "", Some UnixError.ENOENT
            "/ld", None
            "/f", None
        ]

    [<Test>]
    let ``statfs answers each measured path as its kernel does`` () : unit =
        for platform in everyPlatform do
            let flavour = SimulatedUnixPlatform.flavour platform

            for fsType in everyCoherentPair |> List.filter (fst >> (=) flavour) |> List.map snd do
                let system = systemWith platform (EmulatedMount.defaultOf fsType)

                for path, expected in measuredPathRows do
                    let answer = UnixPathResolution.statfs (UnixPath.parseOrFail context path) system

                    match expected, answer with
                    | Some error, FileSystemStatisticsAnswer.Failed actual when actual = error -> ()
                    | None, FileSystemStatisticsAnswer.Reported _ -> ()
                    | _ ->
                        failwith
                            $"%O{platform} on %O{fsType}: statfs(\"%s{path}\") answered %A{answer}, but %A{expected} was measured."

    [<Test>]
    let ``statfs fails exactly where stat, following links, fails`` () : unit =
        // `statfs` is a path resolution and then the mount's answer, so its
        // failures are `stat`'s. The generator walks names that exist, names
        // that do not, links of every kind, and `.` and `..`.
        let components =
            Gen.elements [ "f" ; "d" ; "ld" ; "dang" ; "loop" ; "missing" ; "." ; ".." ; "" ]

        let paths =
            gen {
                let! rooted = Gen.elements [ true ; false ]
                let! parts = Gen.listOf components
                let! trailing = Gen.elements [ true ; false ]
                let body = String.Join ("/", parts)
                let body = if rooted then "/" + body else body
                return if trailing && body <> "" then body + "/" else body
            }

        let system =
            systemWith SimulatedUnixPlatform.linuxX64 (EmulatedMount.defaultFor SimulatedUnixFlavour.Linux)

        let darwin =
            systemWith SimulatedUnixPlatform.macOsArm64 (EmulatedMount.defaultFor SimulatedUnixFlavour.Darwin)

        let property (path : string) : unit =
            for system in [ system ; darwin ] do
                let parsed = UnixPath.parseOrFail context path

                let viaStat =
                    match UnixPathResolution.stat SymlinkPolicy.Follow parsed system with
                    | Ok (FileStatusAnswer.Failed error) -> Some error
                    | Ok (FileStatusAnswer.Reported _) -> None
                    | Error refusal -> failwith $"test bug: stat refused: %s{StatRefusal.describe refusal}"

                let viaStatfs =
                    match UnixPathResolution.statfs parsed system with
                    | FileSystemStatisticsAnswer.Failed error -> Some error
                    | FileSystemStatisticsAnswer.Reported _ -> None

                if viaStat <> viaStatfs then
                    failwith $"\"%s{path}\": stat says %A{viaStat}, statfs says %A{viaStatfs}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen paths) property)

    // ---------------------------------------------------------- coherence

    [<Test>]
    let ``answering for a machine that is no machine is refused, whatever is asked`` () : unit =
        // `UnixMachineState` is a public record, so `{ machine with Mount =
        // ... }` bypasses the setter that keeps the platform and the mount
        // together. This is what stops such a kernel producing a *quietly*
        // wrong answer — one machine's files with another's pipes — rather
        // than a loud one. Every kind of question, including ones that fail
        // before reaching the mount, since a check made after the lookup
        // would let those through.
        for flavour, fsType in everyIncoherentPair do
            let coherent =
                systemWith (HostPlatform.platformOf flavour) (EmulatedMount.defaultFor flavour)

            let incoherent =
                { coherent with
                    Machine =
                        { coherent.Machine with
                            Mount = EmulatedMount.defaultOf fsType
                        }
                }

            for fd in [ 0 ; 4242 ] do
                Assert.Throws (fun () -> UnixPathResolution.fstatfs fd incoherent |> ignore<FileSystemStatisticsAnswer>)
                |> ignore<exn>

            for path in [ "/f" ; "/missing" ] do
                Assert.Throws (fun () ->
                    UnixPathResolution.statfs (UnixPath.parseOrFail context path) incoherent
                    |> ignore<FileSystemStatisticsAnswer>
                )
                |> ignore<exn>

            for target in
                [
                    OpenFileObject.File (InodeNumber 7L)
                    OpenFileObject.StandardStream FileDescriptorRole.StandardInput
                    OpenFileObject.Socket (SocketId 1L)
                    OpenFileObject.AnonymousInode
                ] do
                Assert.Throws (fun () ->
                    FileSystemStatistics.ofObject incoherent.Machine.UnixPlatform incoherent.Machine.Mount target
                    |> ignore<FileSystemStatisticsAnswer>
                )
                |> ignore<exn>

    // ------------------------------------------------------------- the host

    /// The model's answer with the groups it states and the host's answer
    /// agree on; a group the model refuses is not compared.
    let private agreeOnStatedGroups
        (label : string)
        (host : FileSystemStatisticsAnswer)
        (model : FileSystemStatisticsAnswer)
        : unit
        =
        let same (field : string) (host : 'a) (model : Result<'a, 'r>) : unit =
            match model with
            | Error _ -> ()
            | Ok model ->
                if model <> host then
                    failwith $"%s{label}: this host's %s{field} is %A{host}, but the model says %A{model}."

        let stated (field : string) (host : Result<'a, 'r>) : 'a =
            match host with
            | Ok host -> host
            | Error _ -> failwith $"test bug: the host reader left %s{field} unstated"

        match host, model with
        | FileSystemStatisticsAnswer.Failed host, FileSystemStatisticsAnswer.Failed model when host = model -> ()
        | FileSystemStatisticsAnswer.Reported (FileSystemStatistics.Linux host),
          FileSystemStatisticsAnswer.Reported (FileSystemStatistics.Linux model) ->
            same "f_type" host.Type (Ok model.Type : Result<int64, unit>)
            same "geometry" (stated "geometry" host.Geometry) model.Geometry
            same "capacity" (stated "capacity" host.Capacity) model.Capacity
            same "f_fsid" (stated "f_fsid" host.FileSystemId) model.FileSystemId
            same "f_flags" (stated "f_flags" host.Flags) model.Flags
        | _ -> failwith $"%s{label}: this host answers %A{host}, but the model says %A{model}."

    [<Test>]
    let ``this host's own fstatfs answers what the model says for each kind of object`` () : unit =
        // The outside oracle for the rows that turn on the kind of object. Each
        // row is manufactured on the real kernel, handed to its `fstatfs`, and
        // compared with what the model says a kernel of *this* host's flavour
        // would answer: every field the model states, for Linux, and the
        // errno, for Darwin. Only this host's column is checked, so macOS
        // covers Darwin locally and CI covers Linux.
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let mount = EmulatedMount.defaultFor flavour

            let anonymousInode () : int =
                match flavour with
                | SimulatedUnixFlavour.Darwin -> kqueue ()
                | SimulatedUnixFlavour.Linux -> epoll_create1 0

            let ends : int[] = Array.zeroCreate 2

            if pipe ends <> 0 then
                failwith $"pipe(2) failed: errno %d{Marshal.GetLastWin32Error ()}"

            // AF_INET, AF_UNIX, SOCK_STREAM and SOCK_DGRAM are 2, 1, 1 and 2 on
            // both of the Unixes modelled.
            let inet = socket (2, 1, 0)
            let datagram = socket (2, 2, 0)
            let local = socket (1, 1, 0)

            for s in [ inet ; datagram ; local ] do
                if s < 0 then
                    failwith $"socket(2) failed: errno %d{Marshal.GetLastWin32Error ()}"

            let port = anonymousInode ()

            if port < 0 then
                failwith $"anonymous-inode object failed: errno %d{Marshal.GetLastWin32Error ()}"

            try
                let rows =
                    [
                        "pipe read end",
                        ends.[0],
                        Some (OpenFileObject.StandardStream FileDescriptorRole.StandardInput)
                        "pipe write end",
                        ends.[1],
                        Some (OpenFileObject.StandardStream FileDescriptorRole.StandardOutput)
                        "AF_INET stream socket", inet, Some (OpenFileObject.Socket (SocketId 1L))
                        "AF_INET datagram socket", datagram, Some (OpenFileObject.Socket (SocketId 2L))
                        "AF_UNIX stream socket", local, Some (OpenFileObject.Socket (SocketId 3L))
                        "anonymous inode", port, Some OpenFileObject.AnonymousInode
                        // An fd this process does not hold. 4242 rather than -1, so
                        // that a libc screening negative numbers before the syscall
                        // could not be what produced the answer.
                        "unheld descriptor", 4242, None
                    ]

                for label, fd, target in rows do
                    let hostSaid = HostFileSystemType.statisticsFor flavour fd

                    let modelSaid =
                        match target with
                        | None -> FileSystemStatisticsAnswer.Failed UnixError.EBADF
                        | Some target -> FileSystemStatistics.ofObject platform mount target

                    agreeOnStatedGroups $"a %s{label} on this %O{flavour} host" hostSaid modelSaid
            finally
                close ends.[0] |> ignore<int>
                close ends.[1] |> ignore<int>
                close inet |> ignore<int>
                close datagram |> ignore<int>
                close local |> ignore<int>
                close port |> ignore<int>
        )

    [<Test>]
    let ``this host's fstatfs of a descriptor it does not hold is what the model says`` () : unit =
        // Through `fstatfs` itself rather than `ofObject`, for the one row
        // that needs no object.
        HostPlatform.onUnixHost (fun flavour ->
            let system =
                systemWith (HostPlatform.platformOf flavour) (EmulatedMount.defaultFor flavour)

            HostFileSystemType.statisticsFor flavour 4242
            |> shouldEqual (UnixPathResolution.fstatfs 4242 system)
        )

    /// A read-only descriptor onto this host's directory of the filesystem the
    /// library defaults to for `flavour`, if it has one: `/dev/shm` on Linux,
    /// the temporary directory on macOS.
    let private withHostDirectory
        (flavour : SimulatedUnixFlavour)
        (action : int -> FileSystemStatistics -> unit)
        : unit
        =
        let path =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "/dev/shm"
            | SimulatedUnixFlavour.Darwin -> Path.GetTempPath ()

        let fd = hostOpen (path, 0, 0)

        if fd < 0 then
            Assert.Ignore $"cannot open %s{path}: errno %d{Marshal.GetLastWin32Error ()}"

        try
            let wanted =
                EmulatedFileSystemType.fieldsFor flavour (EmulatedFileSystemType.defaultFor flavour)

            match HostFileSystemType.statisticsFor flavour fd with
            | FileSystemStatisticsAnswer.Reported host when FileSystemStatistics.typeFields host = wanted ->
                action fd host
            | other -> Assert.Ignore $"%s{path} is not the flavour's default filesystem: fstatfs answers %A{other}"
        finally
            close fd |> ignore<int>

    [<Test>]
    let ``a directory on this host's own filesystem reports the model's type and geometry`` () : unit =
        // The file row, checked where the host has the filesystem: tmpfs at
        // `/dev/shm` on Linux, APFS for a macOS temporary directory. Only the
        // fields that are facts of the filesystem's type; the rest are this
        // host's mount's own.
        HostPlatform.onUnixHost (fun flavour ->
            withHostDirectory
                flavour
                (fun _ host ->
                    let model =
                        FileSystemStatistics.ofMount
                            (HostPlatform.platformOf flavour)
                            (EmulatedMount.defaultFor flavour)

                    match host, model with
                    | FileSystemStatistics.Linux host, FileSystemStatistics.Linux model ->
                        host.Type |> shouldEqual model.Type
                        host.Geometry |> shouldEqual model.Geometry

                        match host.Flags with
                        | Ok flags when flags &&& 0x20L <> 0L -> ()
                        | other -> failwith $"this host's tmpfs does not set ST_VALID: %A{other}"
                    | FileSystemStatistics.Darwin host, FileSystemStatistics.Darwin model ->
                        host.Type |> shouldEqual model.Type
                        host.TypeName |> shouldEqual model.TypeName

                        match host.Geometry, model.Geometry with
                        | Ok host, Ok model -> host.BlockSize |> shouldEqual model.BlockSize
                        | other -> failwith $"test bug: %A{other}"

                        // The bits the model states as facts of APFS: local,
                        // volfs, journaled, multilabel.
                        let apfsBits = 0x1000u ||| 0x8000u ||| 0x800000u ||| 0x4000000u

                        match host.Mount, model.Mount with
                        | Ok host, Ok model ->
                            if host.Flags &&& apfsBits <> apfsBits then
                                failwith
                                    $"this host's APFS volume reports flags 0x%X{host.Flags}, without every one of 0x%X{apfsBits}"

                            if model.Flags &&& apfsBits <> apfsBits then
                                failwith $"the model's APFS flags 0x%X{model.Flags} lack one of 0x%X{apfsBits}"
                        | other -> failwith $"test bug: %A{other}"
                    | other -> failwith $"test bug: %A{other}"
                )
        )

    [<Test>]
    let ``this host's APFS fsid is its st_dev and its type`` () : unit =
        // The rule the model's Darwin fsid is derived by, checked against the
        // host rather than restated.
        match HostPlatform.flavour (), RuntimeInformation.ProcessArchitecture with
        | Some SimulatedUnixFlavour.Darwin, Architecture.Arm64 ->
            withHostDirectory
                SimulatedUnixFlavour.Darwin
                (fun fd host ->
                    let buffer = Array.zeroCreate<byte> 4096

                    if darwinFstat (fd, buffer) <> 0 then
                        failwith $"fstat failed: errno %d{Marshal.GetLastWin32Error ()}"

                    let device = BitConverter.ToInt32 (buffer, 0)

                    match host with
                    | FileSystemStatistics.Darwin host ->
                        host.FileSystemId
                        |> shouldEqual (
                            Ok
                                {
                                    First = device
                                    Second = int32 host.Type
                                }
                        )
                    | other -> failwith $"test bug: %A{other}"
                )
        | _ -> Assert.Ignore "no Darwin arm64 kernel to measure"
