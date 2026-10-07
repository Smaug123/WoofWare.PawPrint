namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of a scenario, on a path relative to the scenario's own root.
[<RequireQualifiedAccess>]
type LinkCountStep =
    | MakeDirectory of path : string
    | Create of path : string
    | Unlink of path : string
    | RemoveDirectory of path : string
    | Rename of source : string * destination : string
    /// Open `path` read-only and keep the descriptor, under the path's name.
    | Hold of path : string

/// What a scenario reads once its steps have run.
[<RequireQualifiedAccess>]
type LinkCountObservation =
    /// `lstat` of a path.
    | Path of path : string
    /// `fstat` of a descriptor a `Hold` step kept.
    | Held of path : string

/// `st_nlink`, `st_rdev` and `st_flags` as `stat` and `fstat` report them: the
/// rows `stat-fields.c` measured, as literals, and the same scenarios run
/// against this host's own kernel.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLinkCount =

    let private context : string = "TestLinkCount"

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    // ------------------------------------------------------------ the model

    let private reading : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    let private creating : OpenFlags =
        { reading with
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = true
        }

    let private rooted (relative : string) : UnixPath =
        UnixPath.parseOrFail context $"/%s{relative}"

    let private completed
        (what : string)
        (answer : SyscallAnswer, system : UnixSystem<int, string>)
        : int64 * UnixSystem<int, string>
        =
        match answer with
        | SyscallAnswer.Completed value -> value, system
        | SyscallAnswer.Failed error -> failwith $"%s{what} failed with %O{error}"

    /// A fresh system on `platform`, its filesystem the flavour's default.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixBootImage<int, string> = UnixSystem.initial platform


        let fsType =
            EmulatedFileSystemType.defaultFor (SimulatedUnixPlatform.flavour platform)

        system
        |> UnixBootImage.withMount (Some (EmulatedMount.defaultOf fsType))
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private applyToModel
        (held : Map<string, int>, system : UnixSystem<int, string>)
        (step : LinkCountStep)
        : Map<string, int> * UnixSystem<int, string>
        =
        match step with
        | LinkCountStep.MakeDirectory path ->
            let _, system =
                Answered.mkdir (PathArg.ofPath (rooted path)) 0o755 system
                |> completed $"mkdir %s{path}"

            held, system
        | LinkCountStep.Create path ->
            let fd, system =
                Answered.openPath creating (rooted path) 0o644 system
                |> completed $"creat %s{path}"

            match UnixDescriptor.close (int fd) system with
            | Ok (SyscallAnswer.Completed _, system) -> held, system
            | other -> failwith $"close %d{fd}: %A{other}"
        | LinkCountStep.Unlink path -> held, Answered.unlink (rooted path) system |> completed $"unlink %s{path}" |> snd
        | LinkCountStep.RemoveDirectory path ->
            held, Answered.rmdir (rooted path) system |> completed $"rmdir %s{path}" |> snd
        | LinkCountStep.Rename (source, destination) ->
            match UnixNamespace.rename (PathArg.ofText $"/%s{source}") (PathArg.ofText $"/%s{destination}") system with
            | Ok result -> held, result |> completed $"rename %s{source} %s{destination}" |> snd
            | Error refusal -> failwith $"rename %s{source} %s{destination} was refused: %A{refusal}"
        | LinkCountStep.Hold path ->
            let fd, system =
                Answered.openPath reading (rooted path) 0 system |> completed $"open %s{path}"

            Map.add path (int fd) held, system

    let private reported (what : string) (answer : Result<FileStatusAnswer, 'refusal>) : FileStatus =
        match answer with
        | Ok (FileStatusAnswer.Reported status) -> status
        | other -> failwith $"%s{what}: expected a status, got %A{other}"

    let private observeModel
        (held : Map<string, int>)
        (system : UnixSystem<int, string>)
        (observation : LinkCountObservation)
        : FileStatus
        =
        match observation with
        | LinkCountObservation.Path path ->
            UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofPath (rooted path)) system
            |> reported $"lstat %s{path}"
        | LinkCountObservation.Held path -> UnixPathResolution.fstat held.[path] system |> reported $"fstat %s{path}"

    /// The scenarios, each with what it observes. Every row is one
    /// `stat-fields.c` measured.
    let private scenarios : (string * LinkCountStep list * LinkCountObservation list) list =
        [
            "an empty directory", [ LinkCountStep.MakeDirectory "d" ], [ LinkCountObservation.Path "d" ]
            "a directory holding a file",
            [ LinkCountStep.MakeDirectory "d" ; LinkCountStep.Create "d/f" ],
            [ LinkCountObservation.Path "d" ; LinkCountObservation.Path "d/f" ]
            "a directory holding a file and a subdirectory",
            [
                LinkCountStep.MakeDirectory "d"
                LinkCountStep.Create "d/f"
                LinkCountStep.MakeDirectory "d/s"
            ],
            [ LinkCountObservation.Path "d" ; LinkCountObservation.Path "d/s" ]
            "a subdirectory's own contents",
            [
                LinkCountStep.MakeDirectory "d"
                LinkCountStep.Create "d/f"
                LinkCountStep.MakeDirectory "d/s"
                LinkCountStep.MakeDirectory "d/s/inner"
                LinkCountStep.Create "d/s/g"
            ],
            [ LinkCountObservation.Path "d" ; LinkCountObservation.Path "d/s" ]
            "a directory removed while held",
            [
                LinkCountStep.MakeDirectory "r"
                LinkCountStep.Hold "r"
                LinkCountStep.RemoveDirectory "r"
            ],
            [ LinkCountObservation.Held "r" ]
            "a directory that held a subdirectory until just before its removal",
            [
                LinkCountStep.MakeDirectory "r"
                LinkCountStep.MakeDirectory "r/c"
                LinkCountStep.Hold "r"
                LinkCountStep.RemoveDirectory "r/c"
                LinkCountStep.RemoveDirectory "r"
            ],
            [ LinkCountObservation.Held "r" ]
            "a directory displaced by rename while held",
            [
                LinkCountStep.MakeDirectory "m"
                LinkCountStep.MakeDirectory "x"
                LinkCountStep.Hold "x"
                LinkCountStep.Rename ("m", "x")
            ],
            [ LinkCountObservation.Held "x" ; LinkCountObservation.Path "x" ]
            "a file unlinked while held",
            [ LinkCountStep.Create "u" ; LinkCountStep.Hold "u" ; LinkCountStep.Unlink "u" ],
            [ LinkCountObservation.Held "u" ]
            "a file displaced by rename while held",
            [
                LinkCountStep.Create "a"
                LinkCountStep.Create "b"
                LinkCountStep.Hold "b"
                LinkCountStep.Rename ("a", "b")
            ],
            [ LinkCountObservation.Held "b" ; LinkCountObservation.Path "b" ]
        ]

    /// The `st_nlink` each observation of each scenario reported on Linux 6.18.5
    /// (tmpfs) and on Darwin 27.0 (APFS).
    let private measured : Map<string, (int64 * int64) list> =
        Map.ofList
            [
                "an empty directory", [ 2L, 2L ]
                "a directory holding a file", [ (2L, 3L) ; (1L, 1L) ]
                "a directory holding a file and a subdirectory", [ (3L, 4L) ; (2L, 2L) ]
                "a subdirectory's own contents", [ (3L, 4L) ; (3L, 4L) ]
                "a directory removed while held", [ 0L, 2L ]
                "a directory that held a subdirectory until just before its removal", [ 0L, 2L ]
                "a directory displaced by rename while held", [ (0L, 2L) ; (2L, 2L) ]
                "a file unlinked while held", [ 0L, 0L ]
                "a file displaced by rename while held", [ (0L, 0L) ; (1L, 1L) ]
            ]

    [<Test>]
    let ``every scenario reports the measured link counts`` () : unit =
        for name, steps, observations in scenarios do
            for platform in [ linux ; darwin ] do
                let held, system = List.fold applyToModel (Map.empty, systemOn platform) steps

                let actual =
                    observations
                    |> List.map (fun observation -> (observeModel held system observation).LinkCount)

                let expected =
                    measured.[name]
                    |> List.map (fun (onLinux, onDarwin) ->
                        match SimulatedUnixPlatform.flavour platform with
                        | SimulatedUnixFlavour.Linux -> onLinux
                        | SimulatedUnixFlavour.Darwin -> onDarwin
                    )

                if actual <> expected then
                    failwith $"%s{name} on %O{platform}: measured %A{expected}, the model reports %A{actual}"

    [<Test>]
    let ``a fresh filesystem's root counts as any directory does`` () : unit =
        // Measured on a freshly mounted tmpfs and a fresh APFS disk image: the
        // root's count follows the same rule as any other directory's. Besides
        // what the steps make, the root holds `dev`, the directory the kernel
        // mounts its device filesystem on at boot.
        for platform in [ linux ; darwin ] do
            let _, system =
                [
                    LinkCountStep.MakeDirectory "a"
                    LinkCountStep.MakeDirectory "b"
                    LinkCountStep.Create "f"
                ]
                |> List.fold applyToModel (Map.empty, systemOn platform)

            let root =
                UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText "/") system
                |> reported "stat /"

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> root.LinkCount |> shouldEqual 5L
            | SimulatedUnixFlavour.Darwin -> root.LinkCount |> shouldEqual 6L

    [<Test>]
    let ``a directory removed while it is the current directory counts as removed`` () : unit =
        for platform in [ linux ; darwin ] do
            let _, system =
                List.fold applyToModel (Map.empty, systemOn platform) [ LinkCountStep.MakeDirectory "c" ]

            let _, system = Answered.chdir (PathArg.ofText "/c") system |> completed "chdir"

            let _, system = Answered.rmdir (rooted "c") system |> completed "rmdir"

            let here =
                UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText ".") system
                |> reported "stat ."

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> here.LinkCount |> shouldEqual 0L
            | SimulatedUnixFlavour.Darwin -> here.LinkCount |> shouldEqual 2L

    [<Test>]
    let ``a symbolic link counts its names, and is no subdirectory`` () : unit =
        // Measured: `linkat(..., 0)` of a link gives it a second name, both
        // reporting 2, and a link to a subdirectory moves an APFS directory's
        // count as any name does and a tmpfs directory's not at all.
        let name (s : string) : DirectoryEntryName =
            DirectoryEntryName.parseOrFail context s

        let now = UnixTimestamp.createOrFail context 1_700_000_000L 0
        let vfs = VirtualFileSystem.empty now Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let ok (result : Result<'a, UnixError>) : 'a =
            match result with
            | Ok value -> value
            | Error error -> failwith $"expected success, got %O{error}"

        let directory, vfs =
            VirtualFileSystem.createDirectory
                root
                (name "d")
                SeedEntry.defaultPermsForDirectory
                Owners.linuxDefault
                now
                vfs
            |> ok

        let _, vfs =
            VirtualFileSystem.createDirectory
                directory
                (name "s")
                SeedEntry.defaultPermsForDirectory
                Owners.linuxDefault
                now
                vfs
            |> ok

        let link, vfs =
            VirtualFileSystem.createSymlink
                directory
                (name "l")
                SymlinkModes.linux
                Owners.linuxDefault
                now
                (SymlinkTarget.parseOrFail context "s")
                vfs
            |> ok

        let vfs = VirtualFileSystem.hardLink directory (name "l2") link now vfs |> ok

        for platform in [ linux ; darwin ] do
            let system = systemOn platform

            let system =
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = vfs
                        }
                }

            let statOf (inode : InodeNumber) : FileStatus =
                match UnixPathResolution.statOf inode system with
                | Some (Ok status) -> status
                | other -> failwith $"stat of inode %O{inode}: %A{other}"

            (statOf link).LinkCount |> shouldEqual 2L

            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> (statOf directory).LinkCount |> shouldEqual 3L
            | SimulatedUnixFlavour.Darwin -> (statOf directory).LinkCount |> shouldEqual 5L

    // ------------------------------------------------------- Darwin's ceiling

    /// A filesystem whose root holds the directory `d`, holding `count`
    /// subdirectories, and the directory's inode.
    let private wideDirectory (count : int) : InodeNumber * VirtualFileSystem =
        let now = UnixTimestamp.createOrFail context 1_700_000_000L 0
        let vfs = VirtualFileSystem.empty now Owners.linuxDefault

        let created (result : Result<InodeNumber * VirtualFileSystem, UnixError>) =
            match result with
            | Ok created -> created
            | Error error -> failwith $"expected success, got %O{error}"

        let directory, vfs =
            VirtualFileSystem.createDirectory
                (VirtualFileSystem.root vfs)
                (DirectoryEntryName.parseOrFail context "d")
                SeedEntry.defaultPermsForDirectory
                Owners.linuxDefault
                now
                vfs
            |> created

        let vfs =
            (vfs, [ 0 .. count - 1 ])
            ||> List.fold (fun vfs i ->
                VirtualFileSystem.createDirectory
                    directory
                    (DirectoryEntryName.parseOrFail context $"e%d{i}")
                    SeedEntry.defaultPermsForDirectory
                    Owners.linuxDefault
                    now
                    vfs
                |> created
                |> snd
            )

        directory, vfs

    [<Test>]
    let ``Darwin reports a link count no higher than 65535, and Linux keeps counting`` () : unit =
        // Measured by `stat-nlink-limit.c` out to 70000 names: an APFS
        // directory reports 2 plus its names up to 65535 and 65535 beyond, and
        // a tmpfs directory 2 plus its subdirectories, 70002 at 70000.
        let rows =
            [
                65532, 65534L, 65534L
                65533, 65535L, 65535L
                65534, 65536L, 65535L
                70000, 70002L, 65535L
            ]

        for count, onLinux, onDarwin in rows do
            let directory, vfs = wideDirectory count

            for platform, expected in [ linux, onLinux ; darwin, onDarwin ] do
                let system = systemOn platform

                let system =
                    { system with
                        Machine =
                            { system.Machine with
                                FileSystem = vfs
                            }
                    }

                match UnixPathResolution.statOf directory system with
                | Some (Ok status) -> status.LinkCount |> shouldEqual expected
                | other -> failwith $"stat of a directory of %d{count} on %O{platform}: %A{other}"

    // ------------------------------------------------------ rdev and flags

    [<Test>]
    let ``st_rdev is 0 and st_flags is Darwin's alone, and 0, for every kind`` () : unit =
        // Measured on both for a regular file (a dot-file too), a directory, a
        // symbolic link, a FIFO and a pipe: st_rdev 0 throughout, and Darwin's
        // st_flags 0 throughout until chflags sets one.
        for platform in [ linux ; darwin ] do
            let held, system =
                [
                    LinkCountStep.MakeDirectory "d"
                    LinkCountStep.Create "d/f"
                    LinkCountStep.Create "d/.hidden"
                    LinkCountStep.Hold "d/f"
                ]
                |> List.fold applyToModel (Map.empty, systemOn platform)

            let (readFd, writeFd), system =
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
                | other -> failwith $"pipe2: %A{other}"

            let statuses =
                [
                    observeModel held system (LinkCountObservation.Path "d")
                    observeModel held system (LinkCountObservation.Path "d/f")
                    observeModel held system (LinkCountObservation.Path "d/.hidden")
                    observeModel held system (LinkCountObservation.Held "d/f")
                    UnixPathResolution.fstat readFd system |> reported "fstat read end"
                    UnixPathResolution.fstat writeFd system |> reported "fstat write end"
                ]

            let flags =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> None
                | SimulatedUnixFlavour.Darwin -> Some 0u

            for status in statuses do
                status.SpecialFileDevice |> shouldEqual 0L
                status.FileFlags |> shouldEqual flags

    [<Test>]
    let ``a pipe counts one link on Linux and none on Darwin`` () : unit =
        // Measured through each end, and through the read end once the write
        // end has closed.
        for platform, expected in [ linux, 1L ; darwin, 0L ] do
            let system = systemOn platform

            let (readFd, writeFd), system =
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
                | other -> failwith $"pipe2: %A{other}"

            (UnixPathResolution.fstat readFd system |> reported "read end").LinkCount
            |> shouldEqual expected

            (UnixPathResolution.fstat writeFd system |> reported "write end").LinkCount
            |> shouldEqual expected

            let system =
                match UnixDescriptor.close writeFd system with
                | Ok (SyscallAnswer.Completed _, system) -> system
                | other -> failwith $"close: %A{other}"

            (UnixPathResolution.fstat readFd system |> reported "read end, writer closed").LinkCount
            |> shouldEqual expected

    // ------------------------------------------------------------ the host

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "mkdir", SetLastError = true)>]
    extern int private hostMkdir(string path, uint32 mode)

    [<DllImport("libc", EntryPoint = "unlink", SetLastError = true)>]
    extern int private hostUnlink(string path)

    [<DllImport("libc", EntryPoint = "rmdir", SetLastError = true)>]
    extern int private hostRmdir(string path)

    [<DllImport("libc", EntryPoint = "rename", SetLastError = true)>]
    extern int private hostRename(string source, string destination)

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

    let private hostSucceeded (what : string) (result : int) : unit =
        if result <> 0 then
            failwith $"host %s{what} failed: errno %d{Marshal.GetLastPInvokeError ()}"

    let private hostOpened (what : string) (fd : int) : int =
        if fd < 0 then
            failwith $"host %s{what} failed: errno %d{Marshal.GetLastPInvokeError ()}"

        fd

    let private applyToHost (root : string) (held : Map<string, int>) (step : LinkCountStep) : Map<string, int> =
        let under (relative : string) = Path.Combine (root, relative)

        match step with
        | LinkCountStep.MakeDirectory path ->
            hostMkdir (under path, 0o755u) |> hostSucceeded $"mkdir %s{path}"
            held
        | LinkCountStep.Create path ->
            File.Create(under path).Dispose ()
            held
        | LinkCountStep.Unlink path ->
            hostUnlink (under path) |> hostSucceeded $"unlink %s{path}"
            held
        | LinkCountStep.RemoveDirectory path ->
            hostRmdir (under path) |> hostSucceeded $"rmdir %s{path}"
            held
        | LinkCountStep.Rename (source, destination) ->
            hostRename (under source, under destination)
            |> hostSucceeded $"rename %s{source} %s{destination}"

            held
        | LinkCountStep.Hold path -> Map.add path (hostOpen (under path, 0, 0) |> hostOpened $"open %s{path}") held

    [<Test>]
    let ``every scenario agrees with this host's kernel`` () : unit =
        HostStat.onMeasuredHost (fun flavour ->
            let hostBase =
                match HostFileSystemType.directoryOfDefaultType flavour with
                | Some directory -> directory
                | None ->
                    Assert.Ignore
                        $"this %O{flavour} host has no %O{EmulatedFileSystemType.defaultFor flavour} directory to measure"

                    failwith "unreachable: Assert.Ignore throws"

            let platform = HostPlatform.platformOf flavour

            for name, steps, observations in scenarios do
                let unique = Guid.NewGuid().ToString "N"
                let root = Path.Combine (hostBase, $"posixkernel-nlink-%s{unique}")

                hostMkdir (root, 0o755u) |> hostSucceeded "mkdir root"
                let mutable hostHeld = Map.empty

                try
                    for step in steps do
                        hostHeld <- applyToHost root hostHeld step

                    let modelHeld, system = List.fold applyToModel (Map.empty, systemOn platform) steps

                    for observation in observations do
                        let host =
                            match observation with
                            | LinkCountObservation.Path path -> HostStat.lstat flavour (Path.Combine (root, path))
                            | LinkCountObservation.Held path -> HostStat.fstat flavour hostHeld.[path]

                        let model = observeModel modelHeld system observation |> HostStat.ofModel

                        if host <> model then
                            failwith
                                $"%s{name}, %A{observation}: this %O{flavour} host reports %A{host}, the model %A{model}"
                finally
                    for fd in hostHeld |> Map.values do
                        hostClose fd |> ignore<int>

                    Directory.Delete (root, true)
        )

    [<Test>]
    let ``a pipe agrees with this host's kernel`` () : unit =
        HostStat.onMeasuredHost (fun flavour ->
            let fds = Array.zeroCreate<int> 2
            hostPipe fds |> hostSucceeded "pipe"

            try
                let system = systemOn (HostPlatform.platformOf flavour)

                let (readFd, writeFd), system =
                    match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                    | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
                    | other -> failwith $"pipe2: %A{other}"

                for hostFd, modelFd in [ fds.[0], readFd ; fds.[1], writeFd ] do
                    let host = HostStat.fstat flavour hostFd

                    let model =
                        UnixPathResolution.fstat modelFd system
                        |> reported "fstat pipe"
                        |> HostStat.ofModel

                    if host <> model then
                        failwith $"a pipe end: this %O{flavour} host reports %A{host}, the model %A{model}"
            finally
                hostClose fds.[0] |> ignore<int>
                hostClose fds.[1] |> ignore<int>
        )
