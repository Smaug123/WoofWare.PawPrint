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
/// Every row drives a Linux system, `UnixDescriptor.posixFadvise` modelling only
/// the platform that has the call. What Darwin does about not having it is not
/// that function's to say: the two rows at the bottom pin the guard callers
/// consult instead, and `sourcesImpure/PosixFAdviseWiringDarwinSeeded.cs` pins
/// what PawPrint's own shim answers once it has.
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
        UnixSocket.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp system

    /// Every advice value, so that no row silently exercises only the default.
    let private everyAdvice : FileAccessAdvice list =
        [
            FileAccessAdvice.Normal
            FileAccessAdvice.Random
            FileAccessAdvice.Sequential
            FileAccessAdvice.WillNeed
            FileAccessAdvice.DontNeed
            FileAccessAdvice.NoReuse
        ]

    // ------------------------------------------------------- descriptor kinds

    [<Test>]
    let ``a regular file succeeds under every advice`` () : unit =
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual FileAdviceAnswer.Completed

    [<Test>]
    let ``a read-only descriptor succeeds too`` () : unit =
        // fadvise is a read hint, so unlike `ftruncate` it does not care about
        // the access mode: measured on O_RDONLY, O_WRONLY and O_RDWR alike.
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadOnly

        UnixDescriptor.posixFadvise fd 0L 0L FileAccessAdvice.Sequential system
        |> shouldEqual FileAdviceAnswer.Completed

    [<Test>]
    let ``a directory succeeds`` () : unit =
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir" FileAccessMode.ReadOnly

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual FileAdviceAnswer.Completed

    [<Test>]
    let ``a socket succeeds`` () : unit =
        // Measured, on an INET stream socket and on a Unix-domain one: success,
        // not the ESPIPE that "a socket is not seekable" would predict.
        let fd, system = systemOn SimulatedUnixPlatform.linuxX64 |> socket

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual FileAdviceAnswer.Completed

    [<Test>]
    let ``a socket event port succeeds`` () : unit =
        // Measured on an epoll port, which answers success for the same reason
        // the socket does.
        let fd, system = systemOn SimulatedUnixPlatform.linuxX64 |> eventPort

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise fd 0L 0L advice system
            |> shouldEqual FileAdviceAnswer.Completed

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
            |> shouldEqual (FileAdviceAnswer.Failed UnixError.ESPIPE)

    [<Test>]
    let ``an unopened descriptor is EBADF`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        for advice in everyAdvice do
            UnixDescriptor.posixFadvise 99 0L 0L advice system
            |> shouldEqual (FileAdviceAnswer.Failed UnixError.EBADF)

    // -------------------------------------------------------- offset and length

    [<TestCase(-1L)>]
    [<TestCase(Int64.MinValue)>]
    let ``a negative length is EINVAL`` (length : int64) : unit =
        let fd, system =
            systemOn SimulatedUnixPlatform.linuxX64
            |> openedAt "/dir/file" FileAccessMode.ReadWrite

        UnixDescriptor.posixFadvise fd 0L length FileAccessAdvice.Normal system
        |> shouldEqual (FileAdviceAnswer.Failed UnixError.EINVAL)

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

        UnixDescriptor.posixFadvise fd offset length FileAccessAdvice.Normal system
        |> shouldEqual FileAdviceAnswer.Completed

    // ------------------------------------------------------------------ ordering

    [<Test>]
    let ``a bad descriptor beats a bad length`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        UnixDescriptor.posixFadvise 99 0L -1L FileAccessAdvice.Normal system
        |> shouldEqual (FileAdviceAnswer.Failed UnixError.EBADF)

    [<Test>]
    let ``ESPIPE beats a bad length`` () : unit =
        // Measured: the pipe screen sits in the syscall entry, ahead of the
        // range check the generic path makes.
        let system = systemOn SimulatedUnixPlatform.linuxX64

        UnixDescriptor.posixFadvise 0 0L -1L FileAccessAdvice.Normal system
        |> shouldEqual (FileAdviceAnswer.Failed UnixError.ESPIPE)

    // ---------------------------------------------------- platform availability

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
