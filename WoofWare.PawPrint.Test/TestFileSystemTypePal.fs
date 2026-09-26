namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `FileSystemTypePal` transcribes part of `MapFileSystemNameToEnum`, so
/// nothing in the type system keeps its numbers right. Its oracles are the
/// pinned `pal_io.c`, which the name rows are re-derived from, and this host's
/// own shim, which the composition with the library is compared against.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileSystemTypePal =

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetFileSystemType", SetLastError = true)>]
    extern uint32 private hostGetFileSystemType(nativeint fd)

    [<DllImport("libc", SetLastError = true)>]
    extern int private pipe(int[] fds)

    [<DllImport("libc", SetLastError = true)>]
    extern int private socket(int domain, int kind, int protocol)

    /// Darwin's anonymous-inode object. A `DllImport` binds on first call, so
    /// naming a symbol this host lacks costs nothing until something calls it.
    [<DllImport("libc", SetLastError = true)>]
    extern int private kqueue()

    /// Linux's anonymous-inode object.
    [<DllImport("libc", SetLastError = true)>]
    extern int private epoll_create1(int flags)

    [<DllImport("libc")>]
    extern int private close(int fd)

    let private runtimeSrc : string option =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" -> None
        | dir -> Some dir

    let private pinnedSource () : string =
        match runtimeSrc with
        | None ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | Some dir ->

        let path = Path.Combine (dir, "src", "native", "libs", "System.Native", "pal_io.c")

        if not (File.Exists path) then
            failwith
                $"TestFileSystemTypePal: expected the pinned PAL source at %s{path}. If the sparse checkout in flake.nix no longer includes it, this transcription has lost its oracle."

        File.ReadAllText path

    /// `if (strcmp(fileSystemName, "apfs") == 0) result = 0x1A;` and its
    /// `else if` siblings.
    let private nameRow : Regex =
        Regex (
            @"strcmp\(fileSystemName, ""(?<name>[^""]+)""\) == 0\) result = 0x(?<value>[0-9A-Fa-f]+);",
            RegexOptions.Multiline
        )

    let private pinnedNameTable () : Map<string, uint32> =
        let rows =
            nameRow.Matches (pinnedSource ())
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToUInt32 (m.Groups.["value"].Value, 16))
            |> List.ofSeq

        // Far fewer rows than upstream has means the regex has stopped
        // matching its shape, and the comparisons below would be vacuous.
        if rows.Length < 100 then
            failwith
                $"TestFileSystemTypePal: read %d{rows.Length} rows of MapFileSystemNameToEnum from the pinned pal_io.c, expected over 100. Its shape has changed; teach this test to read it."

        Map.ofList rows

    [<Test>]
    let ``each Darwin name row is upstream's`` () : unit =
        let pinned = pinnedNameTable ()

        for name, value in FileSystemTypePal.darwinNameRows do
            match Map.tryFind name pinned with
            | Some upstream when upstream = value -> ()
            | Some upstream ->
                failwith
                    $"FileSystemTypePal maps %s{name} to 0x%X{value}, but the pinned pal_io.c maps it to 0x%X{upstream}."
            | None -> failwith $"FileSystemTypePal has a row for %s{name}, which the pinned pal_io.c does not."

    let private everyCoherentPair : (SimulatedUnixFlavour * EmulatedFileSystemType) list =
        [
            SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Tmpfs
            SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Nfs
            SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Apfs
            SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Nfs
        ]

    let private everyTarget : OpenFileObject option list =
        [
            Some (OpenFileObject.File (InodeNumber 7L))
            Some (OpenFileObject.StandardStream FileDescriptorRole.StandardInput)
            Some (OpenFileObject.Socket (SocketId 1L))
            Some OpenFileObject.AnonymousInode
            None
        ]

    /// What `SystemNative_GetFileSystemType` returns: the PAL's number, or 0 for
    /// any failure.
    let private shimAnswer (answer : FileSystemStatisticsAnswer) : uint32 =
        match answer with
        | FileSystemStatisticsAnswer.Reported statistics ->
            FileSystemTypePal.ofFields (FileSystemStatistics.typeFields statistics)
        | FileSystemStatisticsAnswer.Failed _ -> 0u

    /// What the library's `fstatfs` answers for `target` on a machine of
    /// `flavour` whose mount is a default one of `fsType`; `None` is a
    /// descriptor the process does not hold.
    let private modelAnswer
        (flavour : SimulatedUnixFlavour)
        (fsType : EmulatedFileSystemType)
        (target : OpenFileObject option)
        : FileSystemStatisticsAnswer
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial (HostPlatform.platformOf flavour)

        let machine =
            UnixMachineState.withMount (Some (EmulatedMount.defaultOf fsType)) system.Machine

        match target with
        | None ->
            { system with
                Machine = machine
            }
            |> UnixPathResolution.fstatfs 4242
        | Some target -> FileSystemStatistics.ofObject machine.UnixPlatform machine.Mount target

    [<Test>]
    let ``the shim's number is CoreLib's UnixFileSystemTypes member for every modelled answer`` () : unit =
        // Transcribed from `Interop.Sys.UnixFileSystemTypes`: `tmpfs =
        // 0x01021994`, `apfs = 0x1A`, `nfs = 0x6969`, `pipefs = 0x50495045`,
        // `sockfs = 0x534F434B`, `anoninode = 0x09041934`. That enum is what
        // CoreLib casts the number to, so each answer must land on its member.
        let expected
            (flavour : SimulatedUnixFlavour)
            (fsType : EmulatedFileSystemType)
            (target : OpenFileObject option)
            =
            match target, flavour with
            | None, _ -> 0u
            | Some (OpenFileObject.File _), _ ->
                match fsType with
                | EmulatedFileSystemType.Tmpfs -> 0x01021994u
                | EmulatedFileSystemType.Apfs -> 0x1Au
                | EmulatedFileSystemType.Nfs -> 0x6969u
            // Darwin's `fstatfs` fails on every object not on a filesystem.
            | Some _, SimulatedUnixFlavour.Darwin -> 0u
            | Some (OpenFileObject.StandardStream _), SimulatedUnixFlavour.Linux -> 0x50495045u
            | Some (OpenFileObject.Socket _), SimulatedUnixFlavour.Linux -> 0x534F434Bu
            | Some OpenFileObject.AnonymousInode, SimulatedUnixFlavour.Linux -> 0x09041934u

        for flavour, fsType in everyCoherentPair do
            for target in everyTarget do
                modelAnswer flavour fsType target
                |> shimAnswer
                |> shouldEqual (expected flavour fsType target)

    [<Test>]
    let ``no filesystem a file can be on is reported as zero`` () : unit =
        // Zero is how the shim reports *failure*, so a file on a filesystem
        // whose number were 0 would be indistinguishable from a descriptor that
        // does not exist, and `CanLockTheFile` would refuse to lock it.
        for flavour, fsType in everyCoherentPair do
            EmulatedFileSystemType.fieldsFor flavour fsType
            |> FileSystemTypePal.ofFields
            |> shouldNotEqual 0u

    [<Test>]
    let ``a Darwin name with no transcribed row is refused`` () : unit =
        let name =
            match UnixByteString.ofString "zfs" with
            | Ok name -> name
            | Error defect -> failwith $"test bug: %O{defect}"

        Assert.Throws (fun () ->
            FileSystemTypePal.ofFields (FileSystemTypeFields.Darwin (0x1Au, name))
            |> ignore<uint32>
        )
        |> ignore<exn>

    [<Test>]
    let ``this host's own shim answers what the library and the PAL compose to`` () : unit =
        // The outside oracle for the whole path a guest sees, for the rows that
        // turn on the kind of object rather than on the mount. Each row is
        // manufactured on the real kernel, handed to the real shim, and compared
        // with what PawPrint would push for a kernel of *this* host's flavour.
        // Only this host's column is checked, so macOS covers Darwin locally
        // and CI covers Linux.
        HostPlatform.onUnixHost (fun flavour ->
            let anonymousInode () : int =
                match flavour with
                | SimulatedUnixFlavour.Darwin -> kqueue ()
                | SimulatedUnixFlavour.Linux -> epoll_create1 0

            let ends : int[] = Array.zeroCreate 2

            if pipe ends <> 0 then
                failwith $"pipe(2) failed: errno %d{Marshal.GetLastWin32Error ()}"

            // AF_INET and SOCK_STREAM are 2 and 1 on both of the Unixes modelled.
            let sock = socket (2, 1, 0)

            if sock < 0 then
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
                        "AF_INET socket", sock, Some (OpenFileObject.Socket (SocketId 1L))
                        "anonymous inode", port, Some OpenFileObject.AnonymousInode
                        // 4242 rather than -1, so that a shim screening negative
                        // numbers before the syscall could not be what produced
                        // the answer.
                        "unheld descriptor", 4242, None
                    ]

                for label, fd, target in rows do
                    Marshal.SetLastSystemError 0
                    let hostSaid = hostGetFileSystemType (nativeint fd)
                    let hostErrno = Marshal.GetLastPInvokeError ()

                    let modelSaid =
                        modelAnswer flavour (EmulatedFileSystemType.defaultFor flavour) target

                    if hostSaid <> shimAnswer modelSaid then
                        failwith
                            $"a %s{label} on this %O{flavour} host: the shim reports 0x%X{hostSaid}, but the library and FileSystemTypePal compose to 0x%X{shimAnswer modelSaid} (from %A{modelSaid})."

                    // The shim folds every failure to 0, so the errno is what
                    // separates "no such descriptor" from "not on a
                    // filesystem", and it is what a guest declaring
                    // `SetLastError` would see.
                    match modelSaid with
                    | FileSystemStatisticsAnswer.Failed error ->
                        let expected = UnixError.toRawErrno error

                        if hostErrno <> expected then
                            failwith
                                $"a %s{label} on this %O{flavour} host fails with errno %d{hostErrno}, but the library says %O{error} (errno %d{expected})."
                    | FileSystemStatisticsAnswer.Reported _ -> ()
            finally
                close ends.[0] |> ignore<int>
                close ends.[1] |> ignore<int>
                close sock |> ignore<int>
                close port |> ignore<int>
        )
