namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `posix_fadvise(2)`: a hint that changes nothing this kernel represents, so
/// every row here is about which answer it gives rather than about what it did.
///
/// The rows restate `docs/probes/fadvise/measured-fadvise-linux.txt`, measured
/// on Linux 6.18.5 with `docs/probes/fadvise/fadvise.py`. Two of them are worth
/// naming because reading the man page instead would have got them wrong: a
/// socket and an epoll port both answer *success*, where "not seekable" would
/// predict ESPIPE, and only a pipe answers ESPIPE.
///
/// The advice is Linux's raw `POSIX_FADV_*` number (0 to 5 on x86-64 and on
/// aarch64 alike), screened where the kernel screens it: after the descriptor.
/// The Linux rows were measured on aarch64 (`measured-fadvise-linux.txt`); the
/// kernel path is architecture-independent, and x86-64 is not separately
/// measured.
/// Darwin's libc has no such call, and `posixFadvise` refuses there rather than
/// answer for a call no program could have made.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPosixFadvise =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    /// `/dir/` holding one regular file, so that both a file descriptor and a
    /// directory descriptor are available.
    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList
            [
                name "dir",
                SeedEntry.directory (
                    Map.ofList
                        [
                            name "file",
                            SeedEntry.file (ImmutableArray.CreateRange (Text.Encoding.UTF8.GetBytes "hello"))
                        ]
                )
            ]

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        match
            UnixSystem.initial<int, string> platform
            |> UnixSystem.withFileSystemAndCurrentDirectory createdAt seed (absolute "/dir")
        with
        | Ok system -> system
        | Error fault -> failwith $"the fixture's own seed did not boot: %O{fault}."

    let private inodeOf (system : UnixSystem<int, string>) (path : string) : InodeNumber =
        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
                CallerPrivilege.Privileged
                (VirtualFileSystem.root system.Machine.FileSystem)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" path)
                system.Machine.FileSystem
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}"

    let private openedAt
        (path : string)
        (mode : FileAccessMode)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, registry =
            FileDescriptorRegistry.openFile (inodeOf system path) mode system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private eventPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        fd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private socket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

    /// Linux's `POSIX_FADV_*` numbers for the advice values the rows use.
    [<RequireQualifiedAccess>]
    module private Advice =
        [<Literal>]
        let Normal = 0

        [<Literal>]
        let Sequential = 2

    /// Every advice value Linux accepts, `POSIX_FADV_NORMAL` (0) to
    /// `POSIX_FADV_NOREUSE` (5), so that no row silently exercises only the
    /// default.
    let private everyAdvice : int list = [ 0..5 ]

    // ------------------------------------------------------- descriptor kinds

    [<Test>]
    let ``a regular file succeeds under every advice`` () : unit =
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual (Ok FileAdviceAnswer.Completed)

    [<Test>]
    let ``a read-only descriptor succeeds too`` () : unit =
        // fadvise is a read hint, so unlike `ftruncate` it does not care about
        // the access mode: measured on O_RDONLY, O_WRONLY and O_RDWR alike.
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadOnly

        UnixDescriptor.posixFadvise fd 0L 0L Advice.Sequential system
        |> shouldEqual (Ok FileAdviceAnswer.Completed)

    [<Test>]
    let ``a directory succeeds`` () : unit =
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir" FileAccessMode.ReadOnly

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual (Ok FileAdviceAnswer.Completed)

    [<Test>]
    let ``a socket succeeds`` () : unit =
        // Measured, on an INET stream socket and on a Unix-domain one: success,
        // not the ESPIPE that "a socket is not seekable" would predict.
        let fd, system = systemOn SimulatedUnixPlatform.linuxX64 |> socket

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual (Ok FileAdviceAnswer.Completed)

    [<Test>]
    let ``a socket event port succeeds`` () : unit =
        // Measured on an epoll port, which answers success for the same reason
        // the socket does.
        let fd, system = systemOn SimulatedUnixPlatform.linuxX64 |> eventPort

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual (Ok FileAdviceAnswer.Completed)

    [<TestCase 0>]
    [<TestCase 1>]
    [<TestCase 2>]
    let ``a standard stream is ESPIPE`` (fd : int) : unit =
        // This kernel models the standard streams as pipe ends — see
        // `FileDescriptorRegistry.initial` — and measured, both ends of a pipe
        // answer ESPIPE.
        let system = systemOn SimulatedUnixPlatform.linuxX64

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.ESPIPE))

    [<Test>]
    let ``an unopened descriptor is EBADF`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise 99 0L 0L advice system
            |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.EBADF))

    // -------------------------------------------------------- offset and length

    [<TestCase(-1L)>]
    [<TestCase(Int64.MinValue)>]
    let ``a negative length is EINVAL`` (length : int64) : unit =
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        UnixDescriptor.posixFadvise fd 0L length Advice.Normal system
        |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.EINVAL))

    [<TestCase(-1L, 0L)>]
    [<TestCase(-1L, 4096L)>]
    [<TestCase(Int64.MinValue, 0L)>]
    [<TestCase(0L, Int64.MaxValue)>]
    [<TestCase(Int64.MaxValue, 1L)>]
    [<TestCase(Int64.MaxValue, Int64.MaxValue)>]
    let ``the offset is never validated`` (offset : int64, length : int64) : unit =
        // Measured: only the length is screened. A negative offset, and an
        // offset plus length that overflows, both succeed.
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        UnixDescriptor.posixFadvise fd offset length Advice.Normal system
        |> shouldEqual (Ok FileAdviceAnswer.Completed)

    // ------------------------------------------------------------------ ordering

    [<Test>]
    let ``a bad descriptor beats a bad length`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        UnixDescriptor.posixFadvise 99 0L -1L Advice.Normal system
        |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.EBADF))

    [<Test>]
    let ``ESPIPE beats a bad length`` () : unit =
        // Measured: the pipe screen sits in the syscall entry, ahead of the
        // range check the generic path makes.
        let system = systemOn SimulatedUnixPlatform.linuxX64

        UnixDescriptor.posixFadvise 0 0L -1L Advice.Normal system
        |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.ESPIPE))

    [<Test>]
    let ``an advice Linux does not know is EINVAL, after the descriptor`` () : unit =
        // Measured: 6 and -1 are EINVAL on every descriptor that reaches the
        // generic path, EBADF on a closed one and ESPIPE on a pipe, so the
        // advice is screened after both.
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        for advice in [ 6 ; 7 ; -1 ; Int32.MaxValue ; Int32.MinValue ] do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.EINVAL))

            UnixDescriptor.posixFadvise 99 0L 0L advice system
            |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.EBADF))

            UnixDescriptor.posixFadvise 0 0L 0L advice system
            |> shouldEqual (Ok (FileAdviceAnswer.Failed UnixError.ESPIPE))

    /// The measured rule, restated independently of the implementation: the
    /// descriptor first (EBADF, then ESPIPE for a pipe), then the length and
    /// the advice, which both answer EINVAL.
    let private oracle (kind : string) (length : int64) (advice : int) : FileAdviceAnswer =
        match kind with
        | "closed" -> FileAdviceAnswer.Failed UnixError.EBADF
        | "pipe" -> FileAdviceAnswer.Failed UnixError.ESPIPE
        | _ when length < 0L || advice < 0 || advice > 5 -> FileAdviceAnswer.Failed UnixError.EINVAL
        | _ -> FileAdviceAnswer.Completed

    [<Test>]
    let ``every descriptor kind, advice and length answers as measured`` () : unit =
        let file, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        let directory, system = system |> openedAt "/dir" FileAccessMode.ReadOnly
        let sock, system = socket system
        let port, system = eventPort system

        let kinds =
            [
                "closed", 99
                "pipe", 1
                "file", file
                "directory", directory
                "socket", sock
                "port", port
            ]

        let property (kindIndex : int) (offset : int64) (length : int64) (advice : int) : bool =
            let kind, fd = kinds.[abs (kindIndex % kinds.Length)]
            UnixDescriptor.posixFadvise fd offset length advice system = Ok (oracle kind length advice)

        FsCheck.Check.One (FsCheck.Config.QuickThrowOnFailure.WithMaxTest 2000, property)

        // The boundary itself, which random ints rarely land on.
        for kind, fd in kinds do
            for advice in -2 .. 8 do
                for length in [ -1L ; 0L ; 1L ] do
                    UnixDescriptor.posixFadvise fd 0L length advice system
                    |> shouldEqual (Ok (oracle kind length advice))

    // ---------------------------------------------------- platform availability

    [<Test>]
    let ``Darwin has no posix_fadvise, so the call is refused rather than answered`` () : unit =
        let system = systemOn SimulatedUnixPlatform.macOsArm64

        for fd in [ 0 ; 99 ] do
            for advice in [ 0 ; 6 ] do
                UnixDescriptor.posixFadvise fd 0L 0L advice system
                |> shouldEqual (Error PosixFadviseRefusal.NotProvided)


    [<Test>]
    let ``the platform states which flavour provides the call`` () : unit =
        SimulatedUnixPlatform.providesPosixFadvise SimulatedUnixPlatform.linuxX64
        |> shouldEqual true

        SimulatedUnixPlatform.providesPosixFadvise SimulatedUnixPlatform.macOsArm64
        |> shouldEqual false

    [<Test>]
    let ``the host agrees about whether its libc provides posix_fadvise`` () : unit =
        // The host-equality half of the row above: macOS falsifies one column
        // and Linux the other, so a wrong preset cannot survive both CI legs.
        HostPlatform.onUnixHost (fun flavour ->
            let handle = NativeLibrary.GetMainProgramHandle ()
            let mutable export = IntPtr.Zero
            let hostProvides = NativeLibrary.TryGetExport (handle, "posix_fadvise", &export)

            SimulatedUnixPlatform.providesPosixFadvise (HostPlatform.platformOf flavour)
            |> shouldEqual hostProvides
        )
