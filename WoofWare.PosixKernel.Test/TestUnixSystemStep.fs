namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The syscall surface, driven the way a client that is not PawPrint would
/// drive it.
///
/// This is the first fixture at this altitude, and it is here for the reachable
/// set rather than for the arithmetic: the guest tier runs one flavour, so the
/// Darwin arms of an ordering divergence are unreachable from a guest and
/// reachable from here by constructing a Darwin system directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemStep =

    let private context : string = "TestUnixSystemStep"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// A machine with an empty filesystem, on the flavour asked for. Spelled out
    /// rather than built by a constructor because the library has none: see the
    /// note in the extraction plan.
    let private machineOn (platform : SimulatedUnixPlatform) : UnixMachineState =
        {
            Sockets = Map.empty
            Connections = Map.empty
            NextConnectionId = ConnectionId 0L
            NextSocketEventRegistrationOrdinal = 0L
            NextEphemeralPort = 32768us
            EphemeralPortRange = (32768us, 60999us)
            SoMaxConn = 4096
            LocalAddresses = []
            LocalRoutes = []
            NextSocketId = SocketId 0L
            VirtualClockTicks = 0L
            WallClockEpochMs = 0L
            NonCryptoRandomState = 1UL
            CryptoRandomState = 1UL
            ProcessorCount = 1
            UserAddressLimit = 0x7FFFFFFFFFFFUL
            UnixPlatform = platform
            FileSystem = VirtualFileSystem.empty epoch
            FileSystemType = EmulatedFileSystemType.Tmpfs
        }

    let private rootInode : InodeNumber = InodeNumber 1L

    let private processOn () : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.initial
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = Map.empty
            CurrentDirectory = AbsoluteUnixPath.parseOrFail context "/"
            CurrentDirectoryInode = rootInode
            ProcessPath = None
            DirectoryStreams = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            UserId = 1000u
            GroupId = 1000u
            Umask = PermissionBits.parseOrFail context 0o022
            Signals = SignalState.empty
        }

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        {
            Machine = machineOn platform
            Process = processOn ()
            Tasks = Map.empty
        }

    let private linux : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.linuxX64

    let private darwin : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.macOsArm64

    /// A system holding one five-byte regular file, and the descriptor onto it.
    let private withOpenFile (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let inode, filesystem =
            match
                VirtualFileSystem.createFile
                    rootInode
                    (FileName.parseOrFail context "f")
                    (PermissionBits.parseOrFail context 0o644)
                    epoch
                    (ImmutableArray.CreateRange [ 1uy ; 2uy ; 3uy ; 4uy ; 5uy ])
                    system.Machine.FileSystem
            with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed the file: %O{error}"

        let fd, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = filesystem
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private answered (result : Result<SyscallAnswer * UnixSystem<int, string>, 'a>) : SyscallAnswer =
        match result with
        | Ok (answer, _) -> answer
        | Error e -> failwith $"expected an answer, got a refusal: %O{e}"

    [<Test>]
    let ``an unnamed inode a descriptor still holds is not freed`` () : unit =
        // The rule that makes `read` on an unlinked file keep working: the last
        // *name* has gone, but an open file description is still a reference.
        let fd, system = withOpenFile linux

        let unnamed =
            match
                VirtualFileSystem.unbind
                    UnbindTargetEffect.LostALink
                    rootInode
                    (FileName.parseOrFail context "f")
                    epoch
                    system.Machine.FileSystem
            with
            | Ok (_, filesystem) ->
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
            | Error error -> failwith $"could not unlink the file: %O{error}"

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd unnamed.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file descriptor, got %O{other}"

        VirtualFileSystem.bindingCount inode unnamed.Machine.FileSystem |> shouldEqual 0
        UnixSystem.pinnedInodes unnamed |> Set.contains inode |> shouldEqual true

        let attempted = UnixSystem.forgetIfUnheld inode unnamed

        (VirtualFileSystem.tryGet inode attempted.Machine.FileSystem).IsSome
        |> shouldEqual true

    [<Test>]
    let ``an unnamed inode nothing holds is freed`` () : unit =
        // The other half of the same rule, and the pair is what makes the row
        // above load-bearing: without it, a `forgetIfUnheld` that never freed
        // anything would pass.
        let fd, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file descriptor, got %O{other}"

        let unnamed =
            match
                VirtualFileSystem.unbind
                    UnbindTargetEffect.LostALink
                    rootInode
                    (FileName.parseOrFail context "f")
                    epoch
                    system.Machine.FileSystem
            with
            | Ok (_, filesystem) -> filesystem
            | Error error -> failwith $"could not unlink the file: %O{error}"

        let released =
            match FileDescriptorRegistry.close fd system.Process.FileDescriptors with
            | Ok (registry, _) -> registry
            | Error error -> failwith $"could not close the descriptor: %O{error}"

        let orphaned =
            {
                Machine =
                    { system.Machine with
                        FileSystem = unnamed
                    }
                Process =
                    { system.Process with
                        FileDescriptors = released
                    }
                Tasks = system.Tasks
            }

        UnixSystem.pinnedInodes orphaned |> Set.contains inode |> shouldEqual false

        let reaped = UnixSystem.forgetIfUnheld inode orphaned

        (VirtualFileSystem.tryGet inode reaped.Machine.FileSystem).IsSome
        |> shouldEqual false

    [<Test>]
    let ``geteuid is total, and its type says so`` () : unit =
        // Not a `SyscallAnswer`: `geteuid(2)` cannot fail, so a shape that
        // admitted `Failed` would make an unreachable state representable. The
        // per-syscall function is the primitive for exactly this reason.
        UnixSystem.effectiveUserId linux |> shouldEqual 1000u

    [<Test>]
    let ``step agrees with the primitive it dispatches to`` () : unit =
        // The dispatcher is sugar. If the two ever disagree, the surface a client
        // logs and replays through is not the surface it computes through.
        match UnixSystem.step Syscall.GetEffectiveUserId linux with
        | Ok (SyscallAnswer.Completed answer, after) ->
            answer |> shouldEqual (int64 (UnixSystem.effectiveUserId linux))
            after |> shouldEqual linux
        | other -> failwith $"unexpected: %O{other}"

    [<Test>]
    let ``dup of a closed descriptor is EBADF and changes nothing`` () : unit =
        let answer, after = UnixSystem.dup 7 linux
        answer |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)
        after |> shouldEqual linux

    [<Test>]
    let ``dup shares the description and takes the lowest free descriptor`` () : unit =
        let fd, system = withOpenFile linux
        let answer, after = UnixSystem.dup fd system

        let duplicated =
            match answer with
            | SyscallAnswer.Completed newFd -> int newFd
            | other -> failwith $"expected a descriptor, got %O{other}"

        duplicated |> shouldNotEqual fd

        // The same open file description, which is what makes the offset shared.
        FileDescriptorRegistry.tryFindTarget duplicated after.Process.FileDescriptors
        |> shouldEqual (FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors)

    [<Test>]
    let ``lseek on a closed descriptor is EBADF whatever the whence`` () : unit =
        // Measured ahead of everything on both platforms, including the whences
        // that are not portable at all.
        for system in [ linux ; darwin ] do
            for whence in [ 0 ; 1 ; 2 ; 3 ; 4 ; 99 ] do
                UnixSystem.lseek 7 0L whence system
                |> answered
                |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)

    [<Test>]
    let ``an invalid whence and an unseekable descriptor are ordered by flavour`` () : unit =
        // The divergence this fixture exists for, and one a guest cannot reach:
        // CI's guests run Linux, so the Darwin row below has no other home.
        // Linux validates `whence` before asking whether the object is seekable;
        // Darwin the other way round. Standard input is a pipe, so it is
        // unseekable on both.
        let unseekableFd = 0

        UnixSystem.lseek unseekableFd 0L 99 linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

        UnixSystem.lseek unseekableFd 0L 99 darwin
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.ESPIPE)

    [<Test>]
    let ``a seek that lands moves the offset, and one that does not leaves it`` () : unit =
        let fd, system = withOpenFile linux

        let after =
            match UnixSystem.lseek fd 3L 0 system with
            | Ok (SyscallAnswer.Completed 3L, after) -> after
            | other -> failwith $"expected to land at 3, got %O{other}"

        // A failed seek does not move the description — measured.
        match UnixSystem.lseek fd -1L 0 after with
        | Ok (SyscallAnswer.Failed UnixError.EINVAL, unmoved) ->
            match UnixSystem.lseek fd 0L 1 unmoved with
            | Ok (SyscallAnswer.Completed position, _) -> position |> shouldEqual 3L
            | other -> failwith $"expected the offset to be where it was, got %O{other}"
        | other -> failwith $"expected EINVAL, got %O{other}"

    [<Test>]
    let ``seeking past the end of an int64 diverges in errno, not in ordering`` () : unit =
        // The one place the *errno* differs rather than the order: measured on a
        // tmpfs-backed file so the filesystem is held constant.
        for system, expected in [ linux, UnixError.EINVAL ; darwin, UnixError.EOVERFLOW ] do
            let fd, seeded = withOpenFile system

            UnixSystem.lseek fd (System.Int64.MaxValue - 4L) 2 seeded
            |> answered
            |> shouldEqual (SyscallAnswer.Failed expected)

    [<Test>]
    let ``the sparseness whences are refused, named for the simulated platform`` () : unit =
        // Both refusals, on both flavours: the raw number names a different
        // operation on each, which is half the reason there is no answer to give.
        for system, three, four in
            [
                linux, SeekExtension.SeekData, SeekExtension.SeekHole
                darwin, SeekExtension.SeekHole, SeekExtension.SeekData
            ] do
            let fd, seeded = withOpenFile system

            UnixSystem.lseek fd 0L 3 seeded
            |> shouldEqual (Error (LSeekRefusal.Sparseness (3, three)))

            UnixSystem.lseek fd 0L 4 seeded
            |> shouldEqual (Error (LSeekRefusal.Sparseness (4, four)))

    [<Test>]
    let ``a refusal describes what this kernel measured, and nothing about the caller`` () : unit =
        // The library's half of a diagnostic. It says why no answer exists; it
        // cannot say which entry point asked, because it never saw one.
        let described =
            LSeekRefusal.describe (LSeekRefusal.Sparseness (3, SeekExtension.SeekData))

        described |> shouldContainText "sparseness"
        described |> shouldContainText "SEEK_DATA"
        described |> shouldNotContainText "SystemNative"

    [<Test>]
    let ``step reports a refusal as an error carrying no system`` () : unit =
        // The shape's whole point: a refused call cannot hand back a state, so a
        // client that catches one still holds the system it passed in.
        let fd, seeded = withOpenFile linux

        UnixSystem.step (Syscall.LSeek (fd, 0L, 3)) seeded
        |> shouldEqual (Error (SyscallRefusal.LSeek (LSeekRefusal.Sparseness (3, SeekExtension.SeekData))))

    /// A system holding one directory, and the read-only descriptor onto it that
    /// `open` would give: the only access mode a directory can have.
    let private withOpenDirectory (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let inode, filesystem =
            match
                VirtualFileSystem.createDirectory
                    rootInode
                    (FileName.parseOrFail context "d")
                    (PermissionBits.parseOrFail context 0o755)
                    epoch
                    system.Machine.FileSystem
            with
            | Ok pair -> pair
            | Error error -> failwith $"could not seed the directory: %O{error}"

        let fd, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadOnly system.Process.FileDescriptors

        fd,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = filesystem
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    [<Test>]
    let ``ftruncate validates the length before the descriptor`` () : unit =
        // Measured on both: the same unknown fd is EBADF at length 0 and EINVAL
        // at length -1, so the length really is checked first rather than the two
        // faults merely sharing an errno. A row per fault alone could not tell.
        UnixSystem.ftruncate 99 0L linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EBADF)

        UnixSystem.ftruncate 99 -1L linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

    [<Test>]
    let ``ftruncate of a read-only descriptor is EINVAL, not EBADF`` () : unit =
        // `ftruncate(2)` differs from `write(2)` here, measured on both. It is
        // also what makes a directory answer EINVAL without a type check, since
        // a directory can only ever be opened read-only.
        let fd, system = withOpenDirectory linux

        UnixSystem.ftruncate fd 0L system
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

    [<Test>]
    let ``ftruncate shortens the file and stamps it`` () : unit =
        let fd, system = withOpenFile linux

        let after =
            match UnixSystem.ftruncate fd 2L system with
            | Ok (SyscallAnswer.Completed 0L, after) -> after
            | other -> failwith $"expected success, got %O{other}"

        // Seeking to the end is how the length is read back without a `stat`.
        UnixSystem.lseek fd 0L 2 after
        |> answered
        |> shouldEqual (SyscallAnswer.Completed 2L)

    [<Test>]
    let ``an unlockable operation is EINVAL on Linux and refused on Darwin`` () : unit =
        // Linux validates strictly: exactly one of SH/EX/UN, optionally with NB.
        // Darwin is laxer *and* answers differently per input, which is why the
        // whole of it is refused rather than one row of it modelled.
        let shAndEx = 1 ||| 2

        UnixSystem.flock 0 shAndEx linux
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EINVAL)

        UnixSystem.flock 0 shAndEx darwin
        |> shouldEqual (Error (FLockRefusal.DarwinMalformedOperation shAndEx))

    [<Test>]
    let ``flock on a pipe is Linux's business and Darwin's refusal`` () : unit =
        // The standard streams are pipes here. Linux permits `flock` on one and
        // returns 0; Darwin answers ENOTSUP, and what that leaves the lock state
        // as is unmeasured.
        UnixSystem.flock 0 2 linux
        |> answered
        |> shouldEqual (SyscallAnswer.Completed 0L)

        match UnixSystem.flock 0 2 darwin with
        | Error (FLockRefusal.DarwinStandardStream _) -> ()
        | other -> failwith $"expected a Darwin standard-stream refusal, got %O{other}"

    [<Test>]
    let ``a contended blocking lock is refused, and a non-blocking one is EAGAIN`` () : unit =
        // Two descriptions of one file, so the second acquire genuinely
        // contends. The refusal must not quietly become the non-blocking answer:
        // that would hand a caller an EWOULDBLOCK no kernel would have produced.
        let first, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget first system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file, got %O{other}"

        let second, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        let held =
            match UnixSystem.flock first 2 system with
            | Ok (SyscallAnswer.Completed 0L, held) -> held
            | other -> failwith $"expected the first lock to be granted, got %O{other}"

        UnixSystem.flock second (2 ||| 4) held
        |> answered
        |> shouldEqual (SyscallAnswer.Failed UnixError.EAGAIN)

        UnixSystem.flock second 2 held
        |> shouldEqual (Error (FLockRefusal.WouldBlockIndefinitely FlockMode.Exclusive))

        // And the refusal carries no system, so the caller still holds the one it
        // passed in. That is deliberate and it costs something real: a kernel
        // that could park would have dropped the caller's old lock before
        // sleeping, and this discards that. The alternative — a refusal that
        // carries a state — reintroduces exactly the "which state is this?"
        // ambiguity refusals exist to remove. When blocking gets an outcome of
        // its own, this row is what will have to change on purpose.
        match UnixSystem.flock second 2 held with
        | Error _ -> ()
        | Ok (answer, _) -> failwith $"expected a refusal carrying no system, got %O{answer}"

    [<Test>]
    let ``a failing flock still advances the descriptor table`` () : unit =
        // The design's most distinctive claim, and the reason state rides
        // alongside a `Failed` rather than being withheld: a conversion that
        // cannot be granted has already dropped the caller's old lock.
        let first, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget first system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file, got %O{other}"

        let second, registry =
            FileDescriptorRegistry.openFile inode FileAccessMode.ReadWrite system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        // `second` holds a shared lock; `first` takes one too, then tries to
        // convert to exclusive, which cannot be granted while `second` holds its.
        let held =
            match UnixSystem.flock second 1 system with
            | Ok (SyscallAnswer.Completed 0L, held) -> held
            | other -> failwith $"expected the shared lock to be granted, got %O{other}"

        let both =
            match UnixSystem.flock first 1 held with
            | Ok (SyscallAnswer.Completed 0L, both) -> both
            | other -> failwith $"expected the second shared lock to be granted, got %O{other}"

        let afterFailedConversion =
            match UnixSystem.flock first (2 ||| 4) both with
            | Ok (SyscallAnswer.Failed UnixError.EAGAIN, after) -> after
            | other -> failwith $"expected EAGAIN, got %O{other}"

        // The failure dropped `first`'s lock rather than leaving it: the table
        // the caller gets back is not the one it passed in.
        afterFailedConversion |> shouldNotEqual both

    [<Test>]
    let ``truncateAt refuses a negative length rather than emptying the file`` () : unit =
        // `truncateAt` is shared with `open`'s `O_TRUNC`, so unlike `ftruncate`
        // it has callers that have not screened the length. A negative one
        // reaches `Array.Take` as an empty prefix, which would silently empty the
        // file and stamp it — and the guard that stops it must not be a
        // `Debug.Assert`, which a Release build compiles out.
        let fd, system = withOpenFile linux

        let inode =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.File (inode, _)) -> inode
            | other -> failwith $"expected a file, got %O{other}"

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixSystem.truncateAt inode -1L system
                |> ignore<Result<UnixSystem<int, string>, TruncationRefusal>>
            )

        exn.Message |> shouldContainText "negative"
