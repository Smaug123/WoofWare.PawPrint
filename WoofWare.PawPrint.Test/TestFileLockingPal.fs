namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `FileLockingPal` transcribes .NET 11's `SystemNative_FileSystemSupportsLocking`. Its oracles are
/// the pinned .NET 11 `pal_io.c`, which its constants are re-read from; .NET 10's managed
/// `CanLockTheFile`, which made the same decision over `SystemNative_GetFileSystemType` and must
/// agree with it on every filesystem the library models; and .NET 11's own shim on this host.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileLockingPal =

    [<UnmanagedFunctionPointer(CallingConvention.Cdecl, SetLastError = true)>]
    type private FileSystemSupportsLocking = delegate of nativeint * int32 * int32 -> uint32

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

    /// `PAL_LOCK_SH`, `PAL_LOCK_EX`, both with `PAL_LOCK_NB`, and values naming neither. CoreLib
    /// passes only the first two, and the native asserts as much in a debug build alone.
    let private lockOperations : int32 list = [ 0 ; 1 ; 2 ; 3 ; 5 ; 6 ; 8 ]

    let private accessWrites : int32 list = [ 0 ; 1 ; -1 ]

    /// .NET 10's `SafeFileHandle.CanLockTheFile` (SafeFileHandle.Unix.cs:457) with file locking
    /// enabled, given what `SystemNative_GetFileSystemType` answers for the descriptor.
    let private net10CanLockTheFile (lockOperation : int32) (accessWrite : int32) (fileSystemType : uint32) : bool =
        // `Interop.Sys.UnixFileSystemTypes`' `nfs`, `smb`, `smb2` and `cifs`.
        let refused = [ 0x6969u ; 0x517Bu ; 0xFE534D42u ; 0xFF534D42u ]

        if lockOperation = 2 then true
        elif accessWrite = 0 then true
        // `TryGetFileSystemType` reports the shim's 0 as failure, and then no lock is taken.
        elif fileSystemType = 0u then false
        else not (List.contains fileSystemType refused)

    /// What .NET 11's shim answers for a descriptor whose `fstatfs(2)` would answer `answer`, as
    /// PawPrint's handler composes it.
    let private net11Native (lockOperation : int32) (accessWrite : int32) (answer : FileSystemStatisticsAnswer) : bool =
        if not (FileLockingPal.asksFileSystem lockOperation accessWrite) then
            true
        else
            match answer with
            | FileSystemStatisticsAnswer.Reported statistics ->
                FileLockingPal.supportsLockingOn (FileSystemStatistics.typeFields statistics)
            | FileSystemStatisticsAnswer.Failed _ -> false

    let private pinnedFile (relative : string list) : string =
        let path =
            Path.Combine (Array.ofList (FrameworkUnderTest.requireRuntimeSourceOf EmulatedRuntime.Net11 :: relative))

        if not (File.Exists path) then
            failwith
                $"TestFileLockingPal: expected .NET 11's pinned source at %s{path}. If the sparse checkout in flake.nix no longer includes it, FileLockingPal has lost its oracle."

        File.ReadAllText path

    /// The definition in `source` that begins with the line `header`, up to the brace that closes
    /// it in the first column.
    let private definition (source : string) (header : string) : string =
        let start = source.IndexOf ("\n" + header + "\n{", StringComparison.Ordinal)

        if start < 0 then
            failwith $"TestFileLockingPal: the pinned pal_io.c has no definition beginning %s{header}"

        let finish = source.IndexOf ("\n}\n", start, StringComparison.Ordinal)
        source.Substring (start, finish - start)

    [<Test>]
    let ``each constant is .NET 11's`` () : unit =
        let palIo = pinnedFile [ "src" ; "native" ; "libs" ; "System.Native" ; "pal_io.c" ]

        let palIoHeader =
            pinnedFile [ "src" ; "native" ; "libs" ; "System.Native" ; "pal_io.h" ]

        let native =
            definition
                palIo
                "uint32_t SystemNative_FileSystemSupportsLocking(intptr_t fd, int32_t lockOperation, int32_t accessWrite)"

        let byName =
            definition palIo "static uint32_t FileSystemNameSupportsLocking(const char* fileSystemName)"

        // The test of `asksFileSystem`'s two arguments, which it transcribes.
        native
        |> shouldContainText "if (lockOperation == PAL_LOCK_EX || accessWrite == 0)\n    {\n        return 1;"

        palIoHeader |> shouldContainText $"PAL_LOCK_EX = %d{FileLockingPal.PalLockEx},"

        Regex.Matches (native, @"f_type == 0x(?<value>[0-9A-Fa-f]+)")
        |> Seq.map (fun m -> Convert.ToUInt32 (m.Groups.["value"].Value, 16))
        |> List.ofSeq
        |> shouldEqual FileLockingPal.linuxRefusedMagicNumbers

        Regex.Matches (byName, @"strcmp\(fileSystemName, ""(?<name>[^""]+)""\) == 0")
        |> Seq.map (fun m -> m.Groups.["name"].Value)
        |> List.ofSeq
        |> shouldEqual FileLockingPal.darwinRefusedNames

    [<Test>]
    let ``.NET 11's native decides as .NET 10's CanLockTheFile did, on every modelled answer`` () : unit =
        let mutable outcomes = Set.empty

        for flavour, fsType in TestFileSystemTypePal.everyCoherentPair do
            let targets =
                match flavour with
                | SimulatedUnixFlavour.Linux -> TestFileSystemTypePal.everyTarget
                | SimulatedUnixFlavour.Darwin ->
                    TestFileSystemTypePal.everyTarget @ TestFileSystemTypePal.darwinOnlyTargets

            for target in targets do
                let answer = TestFileSystemTypePal.modelAnswer flavour fsType target

                for lockOperation in lockOperations do
                    for accessWrite in accessWrites do
                        let net11 = net11Native lockOperation accessWrite answer

                        let net10 =
                            net10CanLockTheFile lockOperation accessWrite (TestFileSystemTypePal.shimAnswer answer)

                        if net11 <> net10 then
                            failwith
                                $"%O{flavour} on %O{fsType}, %A{target}, lock %d{lockOperation}, write %d{accessWrite}: .NET 11's native says %b{net11}, .NET 10's CanLockTheFile %b{net10} (fstatfs: %A{answer})."

                        let asked = FileLockingPal.asksFileSystem lockOperation accessWrite

                        let failed =
                            match answer with
                            | FileSystemStatisticsAnswer.Failed _ -> true
                            | FileSystemStatisticsAnswer.Reported _ -> false

                        outcomes <- Set.add (asked, asked && failed, net11) outcomes

        // Every way the native can answer is reached: without asking, refused by the filesystem,
        // refused for a failed `fstatfs`, and allowed by the filesystem.
        outcomes
        |> shouldEqual (
            Set.ofList
                [
                    false, false, true
                    true, false, false
                    true, true, false
                    true, false, true
                ]
        )

    [<RequireQualifiedAccess>]
    type private Verdict =
        | Refused
        | Supported

    let private genLinuxFType : Gen<int64> =
        gen {
            let! low =
                Gen.frequency
                    [
                        3, Gen.elements FileLockingPal.linuxRefusedMagicNumbers
                        1, Gen.elements [ 0x01021994u ; 0x1Au ; 0xEF53u ; 0x6968u ; 0x696Au ]
                        2, ArbMap.defaults |> ArbMap.generate<uint32>
                    ]

            let! high = Gen.frequency [ 1, Gen.constant 0u ; 1, ArbMap.defaults |> ArbMap.generate<uint32> ]

            return int64 ((uint64 high <<< 32) ||| uint64 low)
        }

    [<Test>]
    let ``on Linux, .NET 11's native refuses exactly the f_types .NET 10 did`` () : unit =
        // The shim truncates `f_type` to 32 bits before comparing, as `GetFileSystemType` did.
        let property (cover : Verdict -> unit) (fType : int64) : unit =
            let fields = FileSystemTypeFields.Linux fType
            let shim = FileSystemTypePal.ofFields fields
            let supported = FileLockingPal.supportsLockingOn fields
            cover (if supported then Verdict.Supported else Verdict.Refused)

            // A magic number of 0 would be one .NET 10 mistook for a failed `fstatfs`; no filesystem
            // has it.
            if shim <> 0u then
                supported |> shouldEqual (net10CanLockTheFile 1 1 shim)

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 500) (Arb.fromGen genLinuxFType) property

        coverage.Count Verdict.Refused |> shouldBeGreaterThan 50
        coverage.Count Verdict.Supported |> shouldBeGreaterThan 50

    [<Test>]
    let ``on Darwin, .NET 11's native reads the filesystem's name and never its f_type`` () : unit =
        let genCase : Gen<uint32 * string> =
            Gen.zip
                (ArbMap.defaults |> ArbMap.generate<uint32>)
                (Gen.elements (
                    FileLockingPal.darwinRefusedNames
                    @ List.map fst FileSystemTypePal.darwinNameRows
                    @ [ "smbfs" ; "afpfs" ; "NFS" ; "nfs4" ]
                ))

        let property (cover : Verdict -> unit) (fType : uint32, name : string) : unit =
            let name =
                match UnixByteString.ofString name with
                | Ok name -> name
                | Error defect -> failwith $"test bug: %O{defect}"

            let supported =
                FileLockingPal.supportsLockingOn (FileSystemTypeFields.Darwin (fType, name))

            cover (if supported then Verdict.Supported else Verdict.Refused)

            FileLockingPal.supportsLockingOn (FileSystemTypeFields.Darwin (0u, name))
            |> shouldEqual supported

            // .NET 10's composition, wherever `FileSystemTypePal` has the name's row.
            match
                FileSystemTypePal.darwinNameRows
                |> List.tryFind (fun (row, _) -> UnixByteString.ofString row = Ok name)
            with
            | Some _ ->
                supported
                |> shouldEqual (
                    net10CanLockTheFile 1 1 (FileSystemTypePal.ofFields (FileSystemTypeFields.Darwin (fType, name)))
                )
            | None -> ()

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 300) (Arb.fromGen genCase) property

        coverage.Count Verdict.Refused |> shouldBeGreaterThan 30
        coverage.Count Verdict.Supported |> shouldBeGreaterThan 30

    /// .NET 11's `SystemNative_FileSystemSupportsLocking`, from that runtime's `libSystem.Native`
    /// on this host.
    let private hostNative () : FileSystemSupportsLocking =
        let directory =
            FrameworkUnderTest.requireSharedFrameworkDirectoryOf EmulatedRuntime.Net11

        let file =
            if OperatingSystem.IsMacOS () then
                "libSystem.Native.dylib"
            else
                "libSystem.Native.so"

        let library = NativeLibrary.Load (Path.Combine (directory, file))

        NativeLibrary.GetExport (library, "SystemNative_FileSystemSupportsLocking")
        |> Marshal.GetDelegateForFunctionPointer<FileSystemSupportsLocking>

    [<Test>]
    let ``.NET 11's own shim on this host answers what the library and the PAL compose to`` () : unit =
        // The rows that turn on the kind of object rather than on the mount, as in
        // `TestFileSystemTypePal`'s host oracle: each is manufactured on the real kernel and
        // handed to .NET 11's real shim. Only this host's flavour is checked.
        HostPlatform.onUnixHost (fun flavour ->
            let native = hostNative ()

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
                        "pipe read end", ends.[0], Some (OpenFileObject.Pipe (PipeId 0L))
                        "AF_INET socket", sock, Some (OpenFileObject.Socket (SocketId 1L))
                        "anonymous inode", port, Some OpenFileObject.AnonymousInode
                        "unheld descriptor", 4242, None
                    ]

                for label, fd, target in rows do
                    let modelSaid =
                        TestFileSystemTypePal.modelAnswer flavour (EmulatedFileSystemType.defaultFor flavour) target

                    // `PAL_LOCK_SH` and `PAL_LOCK_EX` alone: a debug build of the shim asserts on
                    // anything else, and this host's may be one.
                    for lockOperation in [ 1 ; 2 ] do
                        for accessWrite in [ 0 ; 1 ] do
                            Marshal.SetLastSystemError 0
                            let hostSaid = native.Invoke (nativeint fd, lockOperation, accessWrite)
                            let hostErrno = Marshal.GetLastPInvokeError ()
                            let expected = net11Native lockOperation accessWrite modelSaid

                            if hostSaid <> (if expected then 1u else 0u) then
                                failwith
                                    $"a %s{label} on this %O{flavour} host, lock %d{lockOperation}, write %d{accessWrite}: .NET 11's shim answers %d{hostSaid}, but the library and FileLockingPal compose to %b{expected} (from %A{modelSaid})."

                            // The errno is what separates a refusal by the filesystem from a
                            // failed `fstatfs`, and an answer given without asking leaves it be.
                            let expectedErrno =
                                match FileLockingPal.asksFileSystem lockOperation accessWrite, modelSaid with
                                | true, FileSystemStatisticsAnswer.Failed error ->
                                    UnixError.toRawErrnoUnder
                                        (SimulatedUnixPlatform.rawErrnoNumbering (HostPlatform.platformOf flavour))
                                        error
                                | _ -> 0

                            if hostErrno <> expectedErrno then
                                failwith
                                    $"a %s{label} on this %O{flavour} host, lock %d{lockOperation}, write %d{accessWrite}: .NET 11's shim leaves errno %d{hostErrno}, expected %d{expectedErrno}."
            finally
                close ends.[0] |> ignore<int>
                close ends.[1] |> ignore<int>
                close sock |> ignore<int>
                close port |> ignore<int>
        )
