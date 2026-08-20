namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `SystemNative_GetFileSystemType`: the table of what `fstatfs(2)` answers for
/// each kind of descriptor, and the coherence rule between a mount's type and
/// the flavour claiming to have mounted it.
///
/// The *file* row is deliberately absent from the host comparison below.
/// PawPrint's filesystem is in memory and claims to be whatever
/// `KernelConfig.FileSystemType` says, where this host's `/tmp` is on ext4,
/// APFS or overlayfs depending on the machine — so a host measurement is no
/// oracle for it. What the host *is* an oracle for is every row that turns on
/// the kind of kernel object rather than on the mount, and those are the rows
/// no guest could pin against anything but PawPrint's own beliefs.
[<TestFixture>]
module TestFileSystemType =

    /// The real export, in the shim this test host runs against — the exact
    /// function the model claims to reproduce, rather than a hand-transcribed
    /// `struct statfs` read that would have to fork by platform. Precedent:
    /// `TestPlatformSocketSupport` measures its entry point the same way.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetFileSystemType", SetLastError = true)>]
    extern uint32 private hostGetFileSystemType(nativeint fd)

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

    let private hostFlavour () : SimulatedUnixFlavour option =
        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            Some SimulatedUnixFlavour.Darwin
        elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then
            Some SimulatedUnixFlavour.Linux
        else
            None

    let private platformOf (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform =
        match flavour with
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64

    let private everyFlavour : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    let private everyFileSystemType : EmulatedFileSystemType list =
        [
            EmulatedFileSystemType.Tmpfs
            EmulatedFileSystemType.Apfs
            EmulatedFileSystemType.Nfs
        ]

    [<Test>]
    let ``each filesystem's magic number is the one CoreLib reads it back as`` () : unit =
        // Transcribed from `Interop.Sys.UnixFileSystemTypes` in the runtime
        // source, which is the only thing that consumes these numbers: whatever
        // PawPrint reports, CoreLib casts straight to that enum. An outside
        // oracle rather than a restatement — these come from upstream, not from
        // the table under test — and each was also seen on a live kernel
        // (tmpfs on Linux's `/dev/shm`, APFS on a macOS `/tmp`).
        EmulatedFileSystemType.magic EmulatedFileSystemType.Tmpfs
        |> shouldEqual 0x01021994u

        EmulatedFileSystemType.magic EmulatedFileSystemType.Apfs |> shouldEqual 0x1Au
        EmulatedFileSystemType.magic EmulatedFileSystemType.Nfs |> shouldEqual 0x6969u

    [<Test>]
    let ``no two filesystems share a magic number`` () : unit =
        // They are distinguishable to `CanLockTheFile`, which is the whole
        // point of the `Nfs` case: a collision would make a configuration
        // silently mean a different one.
        everyFileSystemType
        |> List.map EmulatedFileSystemType.magic
        |> List.distinct
        |> List.length
        |> shouldEqual (List.length everyFileSystemType)

    [<Test>]
    let ``no filesystem reports zero`` () : unit =
        // Zero is how the PAL reports *failure*, so a filesystem whose magic
        // were 0 would be indistinguishable from a descriptor that does not
        // exist — and `CanLockTheFile` would refuse to lock it.
        for fsType in everyFileSystemType do
            EmulatedFileSystemType.magic fsType |> shouldNotEqual 0u

    [<Test>]
    let ``every flavour's default is a filesystem that flavour can mount`` () : unit =
        // Without this, adding a flavour whose default was copied from its
        // neighbour would give a kernel that refuses its own default the moment
        // a host spells it out explicitly.
        for flavour in everyFlavour do
            let chosen = EmulatedFileSystemType.defaultFor flavour

            if not (EmulatedFileSystemType.isReportableUnder flavour chosen) then
                failwith $"%O{flavour} defaults to %O{chosen}, which it cannot report."

    [<Test>]
    let ``omitting the filesystem type takes the flavour's own default`` () : unit =
        for flavour in everyFlavour do
            let kernel =
                EmulatedKernel.initial
                |> EmulatedKernel.withFileSystemType (platformOf flavour) None

            kernel.FileSystemType |> shouldEqual (EmulatedFileSystemType.defaultFor flavour)

    [<Test>]
    let ``a filesystem the flavour could not mount is refused`` () : unit =
        // The provoking test for the coherence guard. Both directions, because
        // a guard that only ever refused one of them would leave the other pair
        // silently constructible.
        let refused =
            [
                SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Tmpfs
                SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Apfs
            ]

        for flavour, fsType in refused do
            let thrown =
                Assert.Throws (fun () ->
                    EmulatedKernel.initial
                    |> EmulatedKernel.withFileSystemType (platformOf flavour) (Some fsType)
                    |> ignore<EmulatedKernel>
                )

            thrown.Message |> shouldContainText (string<EmulatedFileSystemType> fsType)

    [<Test>]
    let ``a filesystem the flavour does mount is accepted`` () : unit =
        // The other half of the pair above: a guard that refused everything
        // would pass that test and break every host.
        let accepted =
            [
                SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Tmpfs
                SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Apfs
                // The one both mount, so it is also the one that says the guard
                // is not simply "the flavour's own default and nothing else".
                SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Nfs
                SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Nfs
            ]

        for flavour, fsType in accepted do
            let kernel =
                EmulatedKernel.initial
                |> EmulatedKernel.withFileSystemType (platformOf flavour) (Some fsType)

            kernel.FileSystemType |> shouldEqual fsType

    [<Test>]
    let ``a file reports the mount's own type, whatever the flavour`` () : unit =
        // The one row that is about the mount rather than the kernel object, so
        // the only one where the configured type must come through unchanged.
        for flavour in everyFlavour do
            for fsType in everyFileSystemType do
                let answer =
                    EmulatedFileSystemType.reportedFor flavour fsType (Some (OpenFileObject.File (InodeNumber 7L)))

                answer
                |> shouldEqual (FileSystemTypeAnswer.Reported (EmulatedFileSystemType.magic fsType))

    [<Test>]
    let ``a descriptor that is not on the mount ignores the mount's type`` () : unit =
        // A pipe is on `pipefs` whatever the filesystem the process's files live
        // on. Without this, a handler that answered every descriptor with the
        // mount's type would still pass the file row above.
        let notOnTheMount =
            [
                Some (OpenFileObject.StandardStream FileDescriptorRole.StandardInput)
                Some (OpenFileObject.Socket (SocketId 1L))
                Some OpenFileObject.AnonymousInode
                None
            ]

        for flavour in everyFlavour do
            for target in notOnTheMount do
                everyFileSystemType
                |> List.map (fun fsType -> EmulatedFileSystemType.reportedFor flavour fsType target)
                |> List.distinct
                |> List.length
                |> shouldEqual 1

    [<Test>]
    let ``this host's own shim answers what the model says for each kind of object`` () : unit =
        // The outside oracle for the rows no guest can arbitrate: a guest
        // asserting them would only be restating PawPrint's beliefs back at
        // itself. Each row is manufactured on the real kernel, handed to the
        // real PAL, and compared with what the model says a kernel of *this*
        // host's flavour would answer.
        //
        // Only this host's column is checked, so macOS covers Darwin locally
        // and CI covers Linux. That is the same split `pathLimits` lives with,
        // and the reason the per-flavour guests exist alongside this.
        match hostFlavour () with
        | None -> Assert.Ignore $"no Unix shim to measure (%s{RuntimeInformation.OSDescription})"
        | Some flavour ->

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
                    // Both ends, because the model has one answer for a stream
                    // whatever its direction and a kernel that disagreed would
                    // show up here rather than in a guest.
                    "pipe read end", ends.[0], Some (OpenFileObject.StandardStream FileDescriptorRole.StandardInput)
                    "pipe write end", ends.[1], Some (OpenFileObject.StandardStream FileDescriptorRole.StandardOutput)
                    "AF_INET socket", sock, Some (OpenFileObject.Socket (SocketId 1L))
                    "anonymous inode", port, Some OpenFileObject.AnonymousInode
                    // An fd this process does not hold. 4242 rather than -1, so
                    // that a shim screening negative numbers before the syscall
                    // could not be what produced the answer.
                    "unheld descriptor", 4242, None
                ]

            for label, fd, target in rows do
                Marshal.SetLastSystemError 0
                let hostSaid = hostGetFileSystemType (nativeint fd)
                let hostErrno = Marshal.GetLastWin32Error ()

                // The mount is irrelevant to every row here, which the test
                // above pins independently; the flavour's default is passed
                // only because the function needs one.
                let modelSaid =
                    EmulatedFileSystemType.reportedFor flavour (EmulatedFileSystemType.defaultFor flavour) target

                match modelSaid with
                | FileSystemTypeAnswer.Reported magic ->
                    if hostSaid <> magic then
                        failwith
                            $"a %s{label} on this %O{flavour} host reports filesystem 0x%X{hostSaid}, but EmulatedFileSystemType.reportedFor says 0x%X{magic}."
                | FileSystemTypeAnswer.Failed error ->
                    // The PAL folds every failure to 0, so the number alone
                    // cannot tell "no such descriptor" from "not on a
                    // filesystem" — the errno is what separates them, and it
                    // is what a guest declaring `SetLastError` would see.
                    let expected = UnixError.toRawErrno error

                    if hostSaid <> 0u || hostErrno <> expected then
                        failwith
                            $"a %s{label} on this %O{flavour} host reports 0x%X{hostSaid} with errno %d{hostErrno}, but EmulatedFileSystemType.reportedFor says it fails with %O{error} (errno %d{expected})."
        finally
            close ends.[0] |> ignore<int>
            close ends.[1] |> ignore<int>
            close sock |> ignore<int>
            close port |> ignore<int>

    [<Test>]
    let ``this host's own filesystem is one CoreCLR will lock`` () : unit =
        // Not a claim about the model: it is the environmental premise the
        // differential half rests on. `sourcesPure/FlockContentionSeeded.cs`
        // compares PawPrint's locking against the real runtime's, and the real
        // runtime takes a shared lock under write access only when the scratch
        // directory's filesystem is not NFS, CIFS or SMB. On a machine where
        // that failed, those guest checks would pass vacuously against a
        // runtime that locked nothing — so the premise is asserted here, where
        // a failure names the actual cause.
        match hostFlavour () with
        | None -> Assert.Ignore $"no Unix shim to measure (%s{RuntimeInformation.OSDescription})"
        | Some _ ->

        let path = System.IO.Path.GetTempFileName ()

        try
            use handle =
                System.IO.File.OpenHandle (path, System.IO.FileMode.Open, System.IO.FileAccess.Read)

            let hostSaid = hostGetFileSystemType (handle.DangerousGetHandle ())

            // The four `SafeFileHandle.CanLockTheFile` refuses, plus 0, which it
            // treats as "unknown, so do not lock".
            let unlockable =
                Map.ofList
                    [
                        0u, "an unknown filesystem"
                        0x6969u, "nfs"
                        0x517Bu, "smb"
                        0xFE534D42u, "smb2"
                        0xFF534D42u, "cifs"
                    ]

            match Map.tryFind hostSaid unlockable with
            | None -> ()
            | Some name ->
                failwith
                    $"this host's temporary directory is on %s{name} (0x%X{hostSaid}), where CoreCLR declines to take a shared lock under write access. FlockContentionSeeded.cs's write-access checks would pass vacuously here."
        finally
            System.IO.File.Delete path
