namespace WoofWare.PosixKernel.Test

open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `fstatfs(2)`: the table of what it answers for each kind of descriptor, and
/// the coherence rule between a mount's type and the flavour claiming to have
/// mounted it.
///
/// The *file* row is deliberately absent from the per-object host comparison
/// below. The library's filesystem is in memory and claims to be whatever
/// mount type its client configures, where this host's `/tmp` is on ext4,
/// APFS or overlayfs depending on the machine — so a host measurement is an
/// oracle for it only where the host happens to have that filesystem, which
/// the APFS test below checks. What the host *is* always an oracle for is
/// every row that turns on the kind of kernel object rather than on the mount.
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

    /// The machine a simulated process boots with on `flavour`'s platform.
    let private machineOn (flavour : SimulatedUnixFlavour) : UnixMachineState =
        (UnixSystem.initial<int, string> (HostPlatform.platformOf flavour)).Machine

    let private everyFlavour : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

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

    let private everyFileSystemType : EmulatedFileSystemType list =
        [
            EmulatedFileSystemType.Tmpfs
            EmulatedFileSystemType.Apfs
            EmulatedFileSystemType.Nfs
        ]

    let private utf8 (name : string) : UnixByteString =
        match UnixByteString.ofString name with
        | Ok name -> name
        | Error defect -> failwith $"test bug: %s{name} is not a Unix string: %O{defect}"

    [<Test>]
    let ``each filesystem's type fields are the ones its kernel reports`` () : unit =
        // Linux's are the magic numbers in `<linux/magic.h>`, tmpfs's measured
        // on `/dev/shm`. Darwin's APFS row was measured by `fstatfs` on macOS
        // 26.6, and its NFS row read from the NFS kext's registration (see the
        // comments on `EmulatedFileSystemType.fieldsFor`). An outside oracle
        // rather than a restatement: these come from kernels, not from the
        // table under test.
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
        for flavour, fsType in
            [
                SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Tmpfs
                SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Apfs
            ] do
            Assert.Throws (fun () -> EmulatedFileSystemType.fieldsFor flavour fsType |> ignore<FileSystemTypeFields>)
            |> ignore<exn>

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
            let kernel = machineOn flavour |> UnixMachineState.withFileSystemType None

            kernel.FileSystemType |> shouldEqual (EmulatedFileSystemType.defaultFor flavour)

    [<Test>]
    let ``the kernel's platform and its filesystem always agree`` () : unit =
        // The invariant the handler relies on: it answers a *file* from the
        // filesystem type and every other descriptor from the platform's
        // flavour, so a kernel carrying one of each would report a combination
        // no machine could produce. Asserted on the record rather than on the
        // setter's argument, because that is what the handler reads — a setter
        // that validated its input and then wrote only one of the two fields
        // would pass every other test in this file.
        for flavour in everyFlavour do
            for requested in None :: List.map Some everyFileSystemType do
                let permitted =
                    match requested with
                    | None -> true
                    | Some fsType -> EmulatedFileSystemType.isReportableUnder flavour fsType

                if permitted then
                    let kernel = machineOn flavour |> UnixMachineState.withFileSystemType requested

                    let carried = SimulatedUnixPlatform.flavour kernel.UnixPlatform

                    if carried <> flavour then
                        failwith $"asked for %O{flavour}, but the kernel carries %O{carried}."

                    if not (EmulatedFileSystemType.isReportableUnder carried kernel.FileSystemType) then
                        failwith
                            $"a kernel built as %O{flavour} from %O{requested} carries filesystem %O{kernel.FileSystemType}, which %O{carried} cannot report."

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
                    machineOn flavour
                    |> UnixMachineState.withFileSystemType (Some fsType)
                    |> ignore<UnixMachineState>
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
            let kernel = machineOn flavour |> UnixMachineState.withFileSystemType (Some fsType)

            kernel.FileSystemType |> shouldEqual fsType

    [<Test>]
    let ``a file reports the mount's own type, whatever the flavour`` () : unit =
        // The one row that is about the mount rather than the kernel object, so
        // the only one where the configured type must come through unchanged.
        // Between them these pairs cover every filesystem.
        for flavour, fsType in everyCoherentPair do
            let answer =
                EmulatedFileSystemType.reportedFor flavour fsType (Some (OpenFileObject.File (InodeNumber 7L)))

            answer
            |> shouldEqual (FileSystemTypeAnswer.Reported (EmulatedFileSystemType.fieldsFor flavour fsType))

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
            let mounts = everyCoherentPair |> List.filter (fst >> (=) flavour) |> List.map snd

            for target in notOnTheMount do
                mounts
                |> List.map (fun fsType -> EmulatedFileSystemType.reportedFor flavour fsType target)
                |> List.distinct
                |> List.length
                |> shouldEqual 1

    [<Test>]
    let ``answering for a pair that describes no machine is refused`` () : unit =
        // `UnixMachineState` is a public record, so `{ machine with UnixPlatform
        // = ... }` bypasses the setter that keeps the two fields together. This
        // is what stops such a kernel producing a *quietly* wrong answer — one
        // machine's files with another's pipes — rather than a loud one.
        let incoherent =
            [
                SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Tmpfs
                SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Apfs
            ]

        for flavour, fsType in incoherent do
            // Every descriptor kind, not just a file: the mount is irrelevant to
            // the others, so a check placed after the `match` would let them
            // through.
            let targets =
                [
                    Some (OpenFileObject.File (InodeNumber 7L))
                    Some (OpenFileObject.StandardStream FileDescriptorRole.StandardInput)
                    Some (OpenFileObject.Socket (SocketId 1L))
                    Some OpenFileObject.AnonymousInode
                    None
                ]

            for target in targets do
                Assert.Throws (fun () ->
                    EmulatedFileSystemType.reportedFor flavour fsType target
                    |> ignore<FileSystemTypeAnswer>
                )
                |> ignore<exn>

    [<Test>]
    let ``this host's own fstatfs answers what the model says for each kind of object`` () : unit =
        // The outside oracle for the rows that turn on the kind of object. Each
        // row is manufactured on the real kernel, handed to its `fstatfs`, and
        // compared with what the model says a kernel of *this* host's flavour
        // would answer.
        //
        // Only this host's column is checked, so macOS covers Darwin locally
        // and CI covers Linux. That is the same split `pathLimits` lives with.
        match HostPlatform.flavour () with
        | None -> Assert.Ignore $"no Unix kernel to measure (%s{RuntimeInformation.OSDescription})"
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
                    // whatever its direction, and a kernel that disagreed would
                    // show up here.
                    "pipe read end", ends.[0], Some (OpenFileObject.StandardStream FileDescriptorRole.StandardInput)
                    "pipe write end", ends.[1], Some (OpenFileObject.StandardStream FileDescriptorRole.StandardOutput)
                    "AF_INET socket", sock, Some (OpenFileObject.Socket (SocketId 1L))
                    "anonymous inode", port, Some OpenFileObject.AnonymousInode
                    // An fd this process does not hold. 4242 rather than -1, so
                    // that a libc screening negative numbers before the syscall
                    // could not be what produced the answer.
                    "unheld descriptor", 4242, None
                ]

            for label, fd, target in rows do
                let hostSaid = HostFileSystemType.answerFor flavour fd

                // The mount is irrelevant to every row here, which the test
                // above pins independently; the flavour's default is passed
                // only because the function needs one.
                let modelSaid =
                    EmulatedFileSystemType.reportedFor flavour (EmulatedFileSystemType.defaultFor flavour) target

                if hostSaid <> modelSaid then
                    failwith
                        $"a %s{label} on this %O{flavour} host: fstatfs answers %A{hostSaid}, but EmulatedFileSystemType.reportedFor says %A{modelSaid}."
        finally
            close ends.[0] |> ignore<int>
            close ends.[1] |> ignore<int>
            close sock |> ignore<int>
            close port |> ignore<int>

    [<Test>]
    let ``a file on this host's APFS reports what the model says for APFS`` () : unit =
        // The file row, checked where the host has the filesystem: a macOS
        // temporary directory is on APFS. Skips anywhere else, including a
        // macOS whose temporary directory is on some other filesystem.
        match HostPlatform.flavour () with
        | Some SimulatedUnixFlavour.Darwin ->
            let path = Path.GetTempPath ()
            let fd = hostOpen (path, 0, 0)

            if fd < 0 then
                failwith $"open(%s{path}) failed: errno %d{Marshal.GetLastWin32Error ()}"

            try
                let expected =
                    EmulatedFileSystemType.fieldsFor SimulatedUnixFlavour.Darwin EmulatedFileSystemType.Apfs

                match HostFileSystemType.answerFor SimulatedUnixFlavour.Darwin fd with
                | FileSystemTypeAnswer.Reported (FileSystemTypeFields.Darwin (_, name) as fields) when
                    name = utf8 "apfs"
                    ->
                    if fields <> expected then
                        failwith
                            $"%s{path} is on APFS, whose fstatfs reports %A{fields}, but EmulatedFileSystemType.fieldsFor says %A{expected}."
                | other -> Assert.Ignore $"%s{path} is not on APFS: fstatfs answers %A{other}"
            finally
                close fd |> ignore<int>
        | _ -> Assert.Ignore "no Darwin kernel to measure"
