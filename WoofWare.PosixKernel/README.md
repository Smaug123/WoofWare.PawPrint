# WoofWare.PosixKernel

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logos/dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="logos/light.svg">
  <img alt="Project logo: minimalistic face of a cartoon Shiba Inu, drawn in outline, sitting at the centre of three concentric rings." src="logos/light.svg" width="300">
</picture>

A deterministic, purely functional simulation of a POSIX process.

This library models what a Unix kernel tells a process about the world.
It includes:

* the filesystem, including permissions, with every name a string of bytes exactly as the kernel stores it, never decoded as text
* file descriptors, the open file descriptions they name, and pipes
* sockets and connections, `poll`, Linux's epoll, and Darwin's kqueue
* signals: sending them, their dispositions, each task's mask, delivery to a handler, and a process ended by one
* the process's tasks (its threads), and the syscalls they block in
* clock
* entropy

The simulated kernel is a state machine, and each syscall is a pure function from one state to an answer and the next state.
WoofWare.PosixKernel performs no I/O and makes no host reads: all state is simulated.
Two runs from the same starting state see the same world, on any machine, in any order.

## History

This was developed incidentally during the construction of WoofWare.PawPrint,
which (being a deterministic simulation of a .NET runtime) must manufacture the results of e.g. the `open` syscall.
I expect it may be of independent interest, so it is now extracted as a standalone component which knows nothing about the CLR.

It speaks POSIX alone, in each flavour's own numbering wherever a number is involved: an `<errno.h>` error, a signal, an `open(2)` flag word.
Converting those to and from a client's own encoding is the client's business.

### Slop status

100% vibe-coded, by the hand of Claude Opus 4.6 through 5, Claude Fable 5, and GPT-5.5 through 5.6 Sol.

## How a client uses it

A client is whatever runs the process's own code: an emulator trapping `syscall` instructions, a replayer working through an `strace` log, a test harness, or a language runtime intercepting C library calls.
It translates each call the process makes into a call of a function here, and gets back the answer and the kernel's next state.
WoofWare.PosixKernel never calls back into the client.

The rest of this document follows a client through the life of a process: booting a machine, making calls, moving bytes, sleeping and waking, signals, threads, and the end of the process, then several processes on one machine.
After that come the parts of a running process that are the client's rather than the kernel's, and how the simulated platforms relate to real ones.

### The system

The whole kernel is one value, a `UnixSystem<'Task, 'Handler>`, made of three parts:

* the machine (`UnixMachineState`): the filesystem, the open file descriptions (with the state of each epoll instance and kqueue, and how many descriptors and calls in flight reference each), sockets, connections and pipes, the thread IDs live tasks hold, the processes on it and the directories they stand in, the clock, the entropy pool, and the platform being simulated;
* the process (`UnixProcessState`): the descriptor table, which says only which open file description each descriptor names, and the process's credentials, umask, current directory, environment and signal state;
* the tasks (`UnixTaskState`): the process's tasks, and what each is blocked in, if anything. A client looks a task up in `UnixSystem.tasks` by its own name for it, which has no entry for a task that was never created or has exited, and reads what it finds with `UnixTaskState`'s readers: `UnixTaskState.parkedIn` (the syscall it sleeps in), `UnixTaskState.cpu` and `UnixTaskState.osThreadId` (what `gettid(2)` answers).

Those records are opaque outside the library.
A client reads a system through `UnixSystem`'s queries, such as `UnixSystem.leader`, `UnixSystem.tasks`, `UnixSystem.signals`, `UnixSystem.credentials`, `UnixSystem.fileDescriptors`, `UnixSystem.openFiles`, `UnixSystem.delivered` and `UnixSystem.descriptorTarget`, and changes a running one only through the syscalls and the two operations of the outside world described under "When time passes".

`'Task` is whatever the client calls a thread, and `'Handler` whatever it calls a signal handler.
The library never looks inside either; it only compares them.

## Booting a machine

`UnixSystem.initial` builds the *boot image* of a machine of the given `SimulatedUnixPlatform` that has not done anything yet: a `UnixBootImage`, which no syscall takes.
Configure it with the setters in the `UnixBootImage` module, such as `UnixBootImage.withFileSystem`, `UnixBootImage.withMount`, `UnixBootImage.withBootTime`, `UnixBootImage.withProcessorCount` and `UnixBootImage.withProcessId` (the first process's ID, where the machine's counters start).
Since no setter takes a booted system, configuration can only describe the machine from the moment it booted.

How a process starts is a `ProcessLaunch`.
`ProcessLaunch.create (platform : SimulatedUnixPlatform) (streams : Map<int, LaunchDescriptor>) (leader : 'Task) (leaderCpu : CpuId)` takes the descriptors it is launched with, its first task's name and the processor that task starts on.
`UnixSystem.pipedStandardStreams` gives descriptors 0, 1 and 2 as pipes: 0 reads a pipe whose far end supplied nothing, and 1 and 2 write to pipes the client drains (see "The far end of a launched pipe").
The setters in the `ProcessLaunch` module set the rest, such as `ProcessLaunch.withCredentials`, `ProcessLaunch.withCurrentDirectory`, `ProcessLaunch.withEnvironment` and `ProcessLaunch.withUmask`.
`UnixBootImage.boot launch` launches the machine's first process, to give the `UnixSystem` its first syscall takes.

A process's leader starts blocking no signal; a client launching one whose parent left signals blocked sets that mask with `UnixSignal.pthreadSigmask` before the leader's first instruction, as it installs inherited ignores with `UnixSignal.sigaction`.

```fsharp
open WoofWare.PosixKernel

let path (text : string) : PathArgumentBytes =
    match UnixByteString.ofString text with
    | Ok bytes -> PathArgumentBytes.Bytes bytes
    | Error defect -> failwith $"not a path: %O{defect}"

let orFail (describe : 'Refusal -> string) (result : Result<'a, 'Refusal>) : 'a =
    match result with
    | Ok value -> value
    | Error refusal -> failwith (describe refusal)

// A process on a Linux x86-64 machine, before anything has happened to it:
// a filesystem holding nothing but /dev, and descriptors 0, 1 and 2 as pipes. It has one task,
// which this client names 0, on logical processor 0, and runs as root.
let system : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> orFail LaunchTableRefusal.describe
        |> ProcessLaunch.withCredentials (Credentials.ofIds UserId.root (GroupId.parseOrFail "example" 0u) [])
        |> orFail CredentialsRefusal.describe

    UnixSystem.initial SimulatedUnixPlatform.linuxX64
    |> UnixBootImage.boot launch
    |> orFail LaunchRefusal.describe

// mkdir(2) is mkdirat(2) from AT_FDCWD, in the flavour's own numbering.
let atFdCwd : int = AtDirectory.atFdCwd SimulatedUnixFlavour.Linux

let mkdir (system : UnixSystem<int, unit>) : UnixSystem<int, unit> =
    match UnixSystem.step 0 (Syscall.MkDirAt (atFdCwd, path "/tmp", 0o755)) system with
    | Error refusal -> failwith $"this kernel will not say what happens: %s{SyscallRefusal.describe refusal}"
    | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed result), system) ->
        printfn "mkdir returned %d" result
        system
    | Ok (SyscallOutcome.Answered (SyscallAnswer.Failed error), system) ->
        let numbering = SimulatedUnixPlatform.rawErrnoNumbering (UnixSystem.platform system)
        printfn "mkdir failed with errno %d" (UnixError.toRawErrnoUnder numbering error)
        system
    | Ok (SyscallOutcome.WouldBlock _, _)
    | Ok (SyscallOutcome.Restarts, _) -> failwith "mkdir never sleeps"

system |> mkdir |> mkdir |> ignore

// Prints:
// mkdir returned 0
// mkdir failed with errno 17
```

### Giving the machine files

A machine boots with an empty filesystem, apart from the device filesystem the kernel mounts over `/dev` (see "Flavours and divergence from host platforms").
`UnixBootImage.withFileSystem (createdAt : UnixTimestamp) (defaultOwner : InodeOwner) (seed : Map<DirectoryEntryName, SeedEntry>)` replaces it with the tree `seed` describes, created at `createdAt`.
A `SeedEntry` is a file, a directory or a symbolic link, each with an optional owner: an entry that names none belongs to `defaultOwner`, and so does the root.
`SeedEntry.file` and `SeedEntry.directory` give the modes a `umask 022` process would have created; the `SeedEntry.File` and `SeedEntry.Directory` cases take a mode and an owner of their own.
It refuses (`FileSystemSeedFault`) a name the flavour could never have created, and anything at `/dev` but an empty directory.

The filesystem's type is what `fstatfs(2)` reports for a file, and it changes a directory's `st_size` and where `lseek(2)` with `SEEK_END` lands on one.
It is tmpfs on Linux and APFS on Darwin unless `UnixBootImage.withMount` says otherwise: `Some (EmulatedMount.defaultOf EmulatedFileSystemType.Nfs)`, for example, or `EmulatedMount.Tmpfs` and `EmulatedMount.Apfs` with their fields set.
A type the flavour could not report is refused (`MountRefusal`).
Path resolution keeps its flavour's limits whichever type is chosen.

```fsharp
open System.Collections.Immutable
open WoofWare.PosixKernel

let orFail (describe : 'Refusal -> string) (result : Result<'a, 'Refusal>) : 'a =
    match result with
    | Ok value -> value
    | Error refusal -> failwith (describe refusal)

let platform = SimulatedUnixPlatform.linuxX64
let name (text : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "seed" text

// /etc/hostname holding "sim\n", and a world-writable sticky /tmp. An entry
// that names no owner belongs to the owner withFileSystem is given: root.
let seed : Map<DirectoryEntryName, SeedEntry> =
    Map.ofList
        [
            name "etc", SeedEntry.directory (Map.ofList [ (name "hostname", SeedEntry.file (ImmutableArray.Create<byte> "sim\n"B)) ])
            name "tmp", SeedEntry.Directory (Map.empty, PermissionBits.parseOrFail "seed" 0o1777, None)
        ]

let root : InodeOwner = InodeOwner.ofProcess (Credentials.ofIds UserId.root (GroupId.parseOrFail "seed" 0u) [])

let system : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> orFail LaunchTableRefusal.describe

    UnixSystem.initial platform
    |> UnixBootImage.withFileSystem UnixTimestamp.epoch root seed
    |> orFail FileSystemSeedFault.describe
    // fstatfs(2) reports NFS rather than Linux's default, tmpfs.
    |> UnixBootImage.withMount (Some (EmulatedMount.defaultOf EmulatedFileSystemType.Nfs))
    |> orFail MountRefusal.describe
    |> UnixBootImage.boot launch
    |> orFail LaunchRefusal.describe

let path (text : string) : PathArgumentBytes =
    match UnixByteString.ofString text with
    | Ok bytes -> PathArgumentBytes.Bytes bytes
    | Error defect -> failwith $"not a path: %O{defect}"

// open("/etc/hostname", O_RDONLY), and read(fd, buf, 64). O_RDONLY is 0 on every flavour.
match UnixNamespace.openPath 0 (path "/etc/hostname") 0 system |> orFail OpenRefusal.describe with
| SyscallAnswer.Completed fd, system ->
    match UnixReadWrite.read 0 (int fd) UserBuffer.Mapped 64UL system |> orFail ReadRefusal.describe with
    | ReadOutcome.Answered (ReadAnswer.Completed bytes), _ ->
        printf "%s" (System.Text.Encoding.ASCII.GetString (bytes.AsSpan ()))
    | outcome, _ -> failwith $"unexpected read: %A{outcome}"
| SyscallAnswer.Failed error, _ -> failwith $"open failed: %O{error}"

// Prints:
// sim
```

## Making a call

Each syscall is a function in the module for its family, listed under "The syscalls" below.
It takes its arguments as the kernel does, raw where the kernel validates them (a descriptor, a flag word, a mode, a `whence`, in the flavour's own numbering), and the system last.
It gives back its answer with the system as the call left it, or the answer alone for a call that cannot change anything.

A function takes the calling task, as its first argument, when the call can sleep, or when its answer depends on the caller's own signal state: its mask, or the signals pending for it.
The thread calls (`UnixTaskLifecycle.spawn`, `UnixTaskLifecycle.exitThread`, `UnixTaskLifecycle.exitGroup`) and the scheduling calls take it too.
`UnixSignal.pthreadKill`'s first argument is the task the signal is sent to, not the caller.
Every other call answers the same whichever task makes it, and takes no task.

A call the process made through its C library and one it made as a raw system call are the same call here, except where the two differ.
Where they do, the function says which it is, and the tables below mark it: (3) is the C library's function, and (2) the system call beneath it.

* `UnixSignal.sigaction` is `sigaction(3)`, which on Linux refuses the C library's own signals 32 and 33 before the kernel sees them; `UnixSignal.sigactionSyscall` is the system call (Linux's `rt_sigaction`).
* `UnixSignal.pthreadSigmask`, and on Linux `UnixSignal.sigprocmask`, drop 32 and 33 from a set as the C library does; `UnixSignal.rtSigprocmask` is Linux's `rt_sigprocmask(2)`, which can block them. `UnixSignal.rtSigsuspend` is Linux's `rt_sigsuspend(2)`, which differs from `UnixSignal.sigsuspend` only in taking the set's size.
* `UnixSignal.pthreadKill` is `pthread_kill(3)` and `raise(3)`; the `tgkill(2)` beneath them is not modelled.
* `UnixDescriptor.terminalAttributes` is `tcgetattr(3)`, `UnixPathResolution.getcwd` is `getcwd(3)` (a library routine on Darwin), and `UnixDescriptor.posixFadvise` returns its error as `posix_fadvise(3)` does, rather than setting errno.

A client replaying a record of raw system calls, such as an `strace` log, uses the (2) forms.

### Answers and refusals

A syscall's result has two levels.

* `Ok` is what the kernel does: it answers, perhaps with an errno (a `UnixError`), or the calling task sleeps (see "Sleeping and waking"). Either way it comes with the system after the call; a failing call can still change the system, just as a real one can.
* `Error` is a refusal: this library will not say what the kernel does, usually because nobody has measured it on the platform being simulated, or because it is not modelled. A refusal says why, and carries no system. A client decides what a refusal means for it; retrying will not help.

Four functions refuse nothing, and use `Error` for the errno instead: `UnixSignal.sigaction`, `UnixSignal.sigactionSyscall`, `UnixSignal.pthreadSigmask` and `UnixSignal.rtSigprocmask` answer `Result<_, UnixError>`, where `Error` is a failed call (an `EINVAL`, say) that the client reports to the process.
A function that cannot fail, such as `UnixClock.gettimeofday`, answers without a `Result` at all.

Every refusal type has a `describe`, which says in English what the library knows about why it refused, such as `ReadRefusal.describe` and `SyscallRefusal.describe`.
The library does not know what the client called the value or which of its entry points made the call, so it names neither; the client does that.
The errors of the parsers that build a value, such as `UnixByteString.ofString`'s, are not refusals, and say what was wrong with the input.

A setter of the machine's boot configuration or of a process's launch refuses the same way.
Each one that can refuse a value returns a `Result`, with a refusal type of its own (`BootTimeRefusal`, `MountRefusal`, `ProcessIdRefusal`, `CredentialsRefusal`, `FileSystemSeedFault` and so on) whose cases state the facts: a value this library has not measured or does not model, or one no machine of the flavour could have.

A call that a real kernel would not let happen at all is a bug in the client, and throws: a task making a syscall while it is asleep in another, a name that is no task, or `UnixScheduling.getcpu` of a task that is not running.
A value no `parse` could have produced, such as one built with `Unchecked.defaultof`, is a bug in the client too, and throws.

### The syscalls

In each table, the function's last argument, the system, is left out, and so are the type arguments `'Task` and `'Handler`: `UnixSystem` is `UnixSystem<'Task, 'Handler>`, and `WriteOutcome<WriteAnswer>` is `WriteOutcome<WriteAnswer, 'Task, 'Handler>`.
A path is the argument's bytes (`PathArgumentBytes`), which the kernel copies in where the call's kernel does.
A buffer is a `UserBuffer` (see "User memory"), which says whether the address the caller passed is mapped; the bytes it holds cross the API separately.

A `SyscallAnswer` is the `int64` a call returns (`SyscallAnswer.Completed`), or the errno it fails with (`SyscallAnswer.Failed`).
A `SyscallOutcome` is a `SyscallAnswer` (`SyscallOutcome.Answered`), the task asleep (`SyscallOutcome.WouldBlock`), or the call restarting (`SyscallOutcome.Restarts`).

#### Descriptors

| Call | Function | Answers |
| --- | --- | --- |
| `dup(2)` | `UnixDescriptor.dup (fd : int)` | `Result<SyscallAnswer * UnixSystem, DescriptorLimitRefusal>` |
| `dup2(2)` | `UnixDescriptor.dup2 (oldFd : int) (newFd : int)` | `Result<SyscallAnswer * UnixSystem, Dup2Refusal>` |
| `dup3(2)` | `UnixDescriptor.dup3 (oldFd : int) (newFd : int) (flags : int)` | `Result<SyscallAnswer * UnixSystem, Dup3Refusal>` |
| `fcntl(2)`: `F_DUPFD`, `F_DUPFD_CLOEXEC`, `F_GETFD`, `F_SETFD`, `F_GETFL`, `F_SETFL` | `UnixDescriptor.fcntl (fd : int) (command : int) (argument : int)` | `Result<SyscallAnswer * UnixSystem, FcntlRefusal>` |
| `fcntl(F_SETFL)` of `fcntl(F_GETFL)` with `O_NONBLOCK` changed | `UnixDescriptor.setNonBlocking (fd : int) (isNonBlocking : bool)` | `SetNonBlockingAnswer * UnixSystem` |
| `O_NONBLOCK` in `fcntl(F_GETFL)` | `UnixDescriptor.isNonBlocking (fd : int)` | `bool option` |
| `lseek(2)` | `UnixDescriptor.lseek (fd : int) (offset : int64) (whence : int)` | `Result<SyscallAnswer * UnixSystem, LSeekRefusal>` |
| `flock(2)` | `UnixDescriptor.flock (task : 'Task) (fd : int) (operation : int)` | `Result<SyscallOutcome * UnixSystem, FLockRefusal>` |
| `ftruncate(2)` | `UnixDescriptor.ftruncate (fd : int) (length : int64)` | `Result<SyscallAnswer * UnixSystem, TruncationRefusal>` |
| `posix_fadvise(3)` | `UnixDescriptor.posixFadvise (fd : int) (offset : int64) (length : int64) (advice : int)` | `Result<FileAdviceAnswer, PosixFadviseRefusal>` |
| `close(2)` | `UnixDescriptor.close (fd : int)` | `Result<SyscallAnswer * UnixSystem, CloseRefusal>` |
| `ioctl(FIONREAD)` | `UnixDescriptor.bytesAvailable (fd : int) (destination : UserBuffer)` | `Result<BytesAvailableAnswer, BytesAvailableRefusal>` |
| `ioctl(FICLONE)` | `UnixDescriptor.fileClone (destination : int) (source : int)` | `Result<UnixError, FileCloneRefusal>` |
| `tcgetattr(3)`, `isatty(3)` | `UnixDescriptor.terminalAttributes (fd : int)` | `TerminalAttributesAnswer` |
| `geteuid(2)` | `UnixDescriptor.effectiveUserId` | `UserId` |
| `getegid(2)` | `UnixDescriptor.effectiveGroupId` | `GroupId` |
| `getgroups(2)` | `UnixDescriptor.getgroups (destination : UserBuffer) (size : int)` | `Result<GetGroupsAnswer, GetGroupsRefusal>` |

#### Paths and their files

| Call | Function | Answers |
| --- | --- | --- |
| `stat(2)` (`SymlinkPolicy.Follow`), `lstat(2)` (`SymlinkPolicy.NoFollowFinal`) | `UnixPathResolution.stat (policy : SymlinkPolicy) (path : PathArgumentBytes)` | `Result<FileStatusAnswer, StatRefusal>` |
| `fstat(2)` | `UnixPathResolution.fstat (fd : int)` | `Result<FileStatusAnswer, FStatRefusal>` |
| `fstatat(2)` | `UnixPathResolution.fstatat (dirfd : int) (path : PathArgumentBytes) (flags : int)` | `Result<FileStatusAnswer, FStatAtRefusal>` |
| `chmod(2)` | `UnixPathResolution.chmod (path : PathArgumentBytes) (mode : int)` | `Result<SyscallAnswer * UnixSystem, ChModRefusal>` |
| `fchmod(2)` | `UnixPathResolution.fchmod (fd : int) (mode : int)` | `Result<SyscallAnswer * UnixSystem, FChModRefusal>` |
| `fchmodat(2)` | `UnixPathResolution.fchmodat (dirfd : int) (path : PathArgumentBytes) (mode : int) (flags : int)` | `Result<SyscallAnswer * UnixSystem, FChModAtRefusal>` |
| `chown(2)` | `UnixPathResolution.chown (path : PathArgumentBytes) (user : UserId option) (group : GroupId option)` | `Result<SyscallAnswer * UnixSystem, ChOwnRefusal>` |
| `lchown(2)` | `UnixPathResolution.lchown (path : PathArgumentBytes) (user : UserId option) (group : GroupId option)` | `Result<SyscallAnswer * UnixSystem, ChOwnRefusal>` |
| `fchown(2)` | `UnixPathResolution.fchown (fd : int) (user : UserId option) (group : GroupId option)` | `Result<SyscallAnswer * UnixSystem, FChOwnRefusal>` |
| `fchownat(2)` | `UnixPathResolution.fchownat (dirfd : int) (path : PathArgumentBytes) (user : UserId option) (group : GroupId option) (flags : int)` | `Result<SyscallAnswer * UnixSystem, FChOwnAtRefusal>` |
| `utimensat(2)`, and Linux's `futimens(3)` | `UnixPathResolution.utimensat (dirfd : int) (path : NullablePathArgument) (times : TimesArgument) (flags : int)` | `Result<SyscallAnswer * UnixSystem, UTimensAtRefusal>` |
| `statfs(2)` | `UnixPathResolution.statfs (path : PathArgumentBytes)` | `Result<FileSystemStatisticsAnswer, PathRefusal>` |
| `fstatfs(2)` | `UnixPathResolution.fstatfs (fd : int)` | `FileSystemStatisticsAnswer` |
| `getcwd(3)` | `UnixPathResolution.getcwd (destination : UserBuffer) (capacity : uint64)` | `Result<GetCwdAnswer, GetCwdRefusal>` |
| `chdir(2)` | `UnixPathResolution.chdir (path : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, PathRefusal>` |
| `access(2)` | `UnixPathResolution.access (path : PathArgumentBytes) (mode : int)` | `Result<SyscallAnswer, AccessRefusal>` |
| `faccessat(2)` | `UnixPathResolution.faccessat (dirfd : int) (path : PathArgumentBytes) (mode : int) (flags : int)` | `Result<SyscallAnswer, AccessRefusal>` |

`UnixPathResolution.currentDirectoryPath` is the current directory's path, for a client that wants it without a buffer.

#### Names in the filesystem

| Call | Function | Answers |
| --- | --- | --- |
| `open(2)` | `UnixNamespace.openPath (flags : int) (path : PathArgumentBytes) (mode : int)` | `Result<SyscallAnswer * UnixSystem, OpenRefusal>` |
| `openat(2)` | `UnixNamespace.openat (dirfd : int) (path : PathArgumentBytes) (flags : int) (mode : int)` | `Result<SyscallAnswer * UnixSystem, OpenRefusal>` |
| `readlink(2)` | `UnixNamespace.readlink (path : PathArgumentBytes) (destination : UserBuffer) (capacity : int)` | `Result<ReadLinkAnswer, ReadLinkRefusal>` |
| `readlinkat(2)` | `UnixNamespace.readlinkat (dirfd : int) (path : PathArgumentBytes) (destination : UserBuffer) (capacity : int)` | `Result<ReadLinkAnswer, ReadLinkRefusal>` |
| one entry of `getdents(2)` or `getdirentries(2)`, without its byte layout | `UnixNamespace.readDirectoryEntry (fd : int)` | `Result<ReadDirectoryAnswer * UnixSystem, ReadDirectoryRefusal>` |
| `mkdir(2)` | `UnixNamespace.mkdir (path : PathArgumentBytes) (mode : int)` | `Result<SyscallAnswer * UnixSystem, PathRefusal>` |
| `mkdirat(2)` | `UnixNamespace.mkdirat (dirfd : int) (path : PathArgumentBytes) (mode : int)` | `Result<SyscallAnswer * UnixSystem, PathRefusal>` |
| `mknod(2)`, regular files only | `UnixNamespace.mknod (path : PathArgumentBytes) (mode : int) (dev : uint32)` | `Result<SyscallAnswer * UnixSystem, MkNodRefusal>` |
| `mknodat(2)`, regular files only | `UnixNamespace.mknodat (dirfd : int) (path : PathArgumentBytes) (mode : int) (dev : uint32)` | `Result<SyscallAnswer * UnixSystem, MkNodRefusal>` |
| `unlink(2)` | `UnixNamespace.unlink (path : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, RemovalRefusal>` |
| `rmdir(2)` | `UnixNamespace.rmdir (path : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, RemovalRefusal>` |
| `unlinkat(2)` | `UnixNamespace.unlinkat (dirfd : int) (path : PathArgumentBytes) (flags : int)` | `Result<SyscallAnswer * UnixSystem, UnlinkAtRefusal>` |
| `rename(2)` | `UnixNamespace.rename (source : PathArgumentBytes) (destination : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, RenameRefusal>` |
| `renameat(2)` | `UnixNamespace.renameat (olddirfd : int) (oldpath : PathArgumentBytes) (newdirfd : int) (newpath : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, RenameRefusal>` |
| Darwin's `clonefile(2)` | `UnixNamespace.cloneFile (source : PathArgumentBytes) (destination : PathArgumentBytes) (flags : int)` | `Result<SyscallAnswer * UnixSystem, CloneFileRefusal>` |
| `symlink(2)` | `UnixNamespace.symlink (target : PathArgumentBytes) (path : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, SymlinkRefusal>` |
| `symlinkat(2)` | `UnixNamespace.symlinkat (target : PathArgumentBytes) (dirfd : int) (path : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, SymlinkRefusal>` |
| `link(2)` | `UnixNamespace.link (oldpath : PathArgumentBytes) (newpath : PathArgumentBytes)` | `Result<SyscallAnswer * UnixSystem, LinkRefusal>` |
| `linkat(2)` | `UnixNamespace.linkat (olddirfd : int) (oldpath : PathArgumentBytes) (newdirfd : int) (newpath : PathArgumentBytes) (flags : int)` | `Result<SyscallAnswer * UnixSystem, LinkRefusal>` |

#### Reading and writing

| Call | Function | Answers |
| --- | --- | --- |
| `read(2)` | `UnixReadWrite.read (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64)` | `Result<ReadOutcome * UnixSystem, ReadRefusal>` |
| `recv(2)` | `UnixReadWrite.recv (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (flags : int)` | `Result<ReadOutcome * UnixSystem, ReceiveRefusal>` |
| `pread(2)` | `UnixReadWrite.pread (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (offset : int64)` | `Result<ReadAnswer * UnixSystem, PReadRefusal>` |
| `write(2)`, before the copy | `UnixReadWrite.admitWrite (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64)` | `Result<WriteOutcome<WriteAdmission>, WriteRefusal>` |
| `write(2)`, given the bytes | `UnixReadWrite.write (task : 'Task) (fd : int) (bytes : ImmutableArray<byte>)` | `Result<WriteOutcome<WriteAnswer>, WriteRefusal>` |
| `write(2)`, given the first bytes of a write that then sleeps | `UnixReadWrite.writeThenSleep (task : 'Task) (fd : int) (total : int) (bytes : ImmutableArray<byte>)` | `Result<WriteOutcome<WriteAnswer>, WriteRefusal>` |
| `send(2)`, before the copy | `UnixReadWrite.admitSend (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (flags : int)` | `Result<WriteOutcome<WriteAdmission>, SendRefusal>` |
| `send(2)`, given the bytes | `UnixReadWrite.send (task : 'Task) (fd : int) (bytes : ImmutableArray<byte>) (flags : int)` | `Result<WriteOutcome<WriteAnswer>, SendRefusal>` |
| `send(2)`, given the first bytes of a send that then sleeps | `UnixReadWrite.sendThenSleep (task : 'Task) (fd : int) (total : int) (bytes : ImmutableArray<byte>) (flags : int)` | `WriteOutcome<WriteAnswer>` |
| `pwrite(2)`, before the copy | `UnixReadWrite.admitPWrite (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (offset : int64)` | `Result<PWriteAdmission, PWriteRefusal>` |
| `pwrite(2)`, given the bytes | `UnixReadWrite.pwrite (task : 'Task) (fd : int) (bytes : ImmutableArray<byte>) (offset : int64)` | `Result<WriteAnswer * UnixSystem, PWriteRefusal>` |
| `copy_file_range(2)` | `UnixReadWrite.copyFileRange (inFd : int) (outFd : int) (length : uint64) (flags : int)` | `Result<SyscallAnswer * UnixSystem, CopyFileRangeRefusal>` |
| `pipe2(2)`, and `pipe(2)` as `pipe2` with no flags | `UnixPipe.pipe2 (flags : int) (destination : UserBuffer)` | `Result<Pipe2Answer * UnixSystem, Pipe2Refusal>` |

#### Sockets

| Call | Function | Answers |
| --- | --- | --- |
| `socket(2)` | `UnixSocket.socket (domain : int) (socketType : int) (protocol : int)` | `Result<Result<int * UnixSystem, UnixError>, SocketRefusal>` |
| `bind(2)` and `connect(2)`, before the copy | `UnixSocket.admitSockaddrCopy (syscall : SockaddrCopySyscall) (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `Result<SockaddrCopyAdmission, SockaddrCopyRefusal>` |
| `bind(2)`, given the bytes | `UnixSocket.bind (fd : int) (destination : UserBuffer) (declaredLength : uint32) (copied : ImmutableArray<byte>)` | `Result<BindAnswer * UnixSystem, BindRefusal>` |
| `connect(2)`, given the bytes | `UnixConnection.connect (fd : int) (destination : UserBuffer) (declaredLength : uint32) (copied : ImmutableArray<byte>)` | `Result<ConnectOutcome * UnixSystem, ConnectRefusal>` |
| `listen(2)` | `UnixSocket.listen (fd : int) (backlog : int)` | `Result<ListenAnswer * UnixSystem, ListenRefusal>` |
| `accept(2)` | `UnixConnection.accept (task : 'Task) (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `Result<AcceptOutcome * UnixSystem, AcceptRefusal>` |
| `getsockname(2)` | `UnixSocket.getsockname (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `Result<GetSockNameAnswer, GetSockNameRefusal>` |
| `getpeername(2)` | `UnixSocket.getpeername (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `Result<GetSockNameAnswer, GetSockNameRefusal>` |
| `setsockopt(2)`, before the copy | `UnixSocket.admitSetSockOpt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (optionLength : uint32)` | `Result<SetSockOptAdmission, SocketOptionRefusal>` |
| `setsockopt(2)`, given the bytes | `UnixSocket.setsockopt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (optionLength : uint32) (supplied : ImmutableArray<byte> option)` | `Result<SetSockOptAnswer * UnixSystem, SocketOptionRefusal>` |
| `getsockopt(2)`, before the length is read | `UnixSocket.admitGetSockOpt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (length : UserBuffer)` | `Result<GetSockOptAdmission, SocketOptionRefusal>` |
| `getsockopt(2)`, given the length | `UnixSocket.getsockopt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (length : UserBuffer) (declaredLength : uint32 option)` | `Result<GetSockOptAnswer * UnixSystem, SocketOptionRefusal>` |

A connected TCP socket's `read` and `write` move bytes through its connection (`TcpConnection.Transfer`), which holds each direction's bytes in the sender's send buffer and the receiver's receive buffer, sized from the machine's TCP sysctls, and each end's state: open, a FIN received, reset, or closed. A close over bytes left unread resets the peer; otherwise it sends a FIN behind what it had sent. `poll`, epoll and kqueue read a connected socket's readiness from the same state, and each transfer wakes the waiters each flavour wakes: every arrival of bytes, and room freed in a send buffer (on Linux once after a write ran out of room, when the buffer has drained to two thirds full; on Darwin as bytes leave it). `FIONREAD` reports what waits to be read, and `SO_ERROR` takes a reset's error. A blocking read with nothing to answer sleeps until bytes, a FIN or a reset arrive; a blocking write takes what fits and sleeps for the rest, woken on Linux once its send buffer has drained to two thirds full and on Darwin once there is room for it to take something, and it returns once every byte is taken. Every task asleep on a socket wakes for what it waits on.

`recv` and `send` reach a connected TCP socket through the same transfer, sleep and wake as `read` and `write`, and take their flag word raw, in the flavour's numbering (`MessageFlag.number`). `MSG_PEEK` answers bytes without taking them, `MSG_DONTWAIT` makes a `recv`, and a Linux `send`, non-blocking (Darwin's `send` ignores it), and `MSG_NOSIGNAL` keeps an `EPIPE` from raising `SIGPIPE`; any other flag is refused, naming it. Where they part from `read` and `write` is measured: Linux screens their buffer before it looks up the descriptor, a Linux `recv` of nothing waits as a longer one would, and Darwin's `send` marks no description written.

#### Waiting for several things

| Call | Function | Answers |
| --- | --- | --- |
| `poll(2)` | `UnixPoll.poll (task : 'Task) (entries : PollEntry list) (milliseconds : int)` | `Result<PollOutcome * UnixSystem, PollRefusal>` |
| `epoll_create1(2)` | `UnixPoll.epollCreate1 (flags : int)` | `Result<Result<int * UnixSystem, UnixError>, EpollCreateRefusal>` |
| `epoll_ctl(2)` | `UnixPoll.epollCtl (epfd : int) (op : int) (fd : int) (event : EpollEventArgument)` | `Result<EpollCtlAnswer * UnixSystem, EpollCtlRefusal>` |
| `epoll_wait(2)` | `UnixPoll.epollWait (task : 'Task) (epfd : int) (maxEvents : int) (buffer : UserBuffer) (milliseconds : int)` | `Result<EpollWaitOutcome * UnixSystem, EpollWaitRefusal>` |
| Darwin's `kqueue(2)` | `UnixKqueue.kqueue` | `Result<int * UnixSystem, KqueueRefusal>` |
| Darwin's `kevent(2)` | `UnixKqueue.kevent (task : 'Task) (kq : int) (nchanges : int) (changes : Kevent list) (nevents : int) (eventlist : UserBuffer) (timeout : KeventTimeout)` | `Result<KeventOutcome * UnixSystem, KeventRefusal>` |

#### Signals

| Call | Function | Answers |
| --- | --- | --- |
| `kill(2)` | `UnixSignal.kill (pid : int) (signo : int)` | `Result<Result<KillOutcome, UnixError>, KillRefusal>` |
| `pthread_kill(3)`, `raise(3)` | `UnixSignal.pthreadKill (target : 'Task) (signo : int)` | `Result<Result<KillOutcome, UnixError>, ThreadKillRefusal>` |
| `sigaction(3)` | `UnixSignal.sigaction (signo : int) (newAction : SignalDisposition option)` | `Result<SignalDisposition * UnixSystem, UnixError>` |
| `sigaction(2)`: Linux's `rt_sigaction` | `UnixSignal.sigactionSyscall (signo : int) (newAction : SignalDisposition option)` | `Result<SignalDisposition * UnixSystem, UnixError>` |
| `pthread_sigmask(3)` | `UnixSignal.pthreadSigmask (task : 'Task) (how : int) (set : SignalMask option)` | `Result<SignalMask * UnixSystem, UnixError>` |
| `sigprocmask(3)` on Linux, `sigprocmask(2)` on Darwin | `UnixSignal.sigprocmask (task : 'Task) (how : int) (set : SignalMask option)` | `Result<Result<SignalMask * UnixSystem, UnixError>, SigprocmaskRefusal>` |
| Linux's `rt_sigprocmask(2)` | `UnixSignal.rtSigprocmask (task : 'Task) (how : int) (set : SignalMask option) (sigsetSize : uint64)` | `Result<SignalMask * UnixSystem, UnixError>` |
| `sigpending(2)` | `UnixSignal.sigpending (task : 'Task)` | `SignalMask` |
| `sigsuspend(2)` | `UnixSignal.sigsuspend (task : 'Task) (mask : SignalMask)` | `Result<SigsuspendOutcome * UnixSystem, SigsuspendRefusal>` |
| Linux's `rt_sigsuspend(2)` | `UnixSignal.rtSigsuspend (task : 'Task) (mask : SignalMask) (sigsetSize : uint64)` | `Result<SigsuspendOutcome * UnixSystem, SigsuspendRefusal>` |
| `pause(2)` | `UnixSignal.pause (task : 'Task)` | `Result<SigsuspendOutcome * UnixSystem, SigsuspendRefusal>` |
| the return to user mode | `UnixSignal.onReturnToUser (task : 'Task)` | `Result<ReturnToUserOutcome, SignalReceiverRefusal>` |
| `sigreturn(2)` | `UnixSignal.sigreturn (task : 'Task) (frame : HandlerFrameId)` | `UnixSystem` |

A signal mask crosses the API as a `SignalMask`, the bits of a `sigset_t` under one numbering (`SignalMask.ofWord`, `SignalMask.toWord`, `SignalMask.ofSignals`), because Darwin keeps a bit that names no signal.
A signal number is raw, under the process's own numbering: `Signal.toRawSignoUnder` and `Signal.ofRawSignoUnder` convert, with the numbering from `SimulatedUnixPlatform.signalNumbering`.
`how` is raw too: `SIG_BLOCK`, `SIG_UNBLOCK` and `SIG_SETMASK` are 0, 1 and 2 on Linux, and 1, 2 and 3 on Darwin.

#### Time, entropy, identity and threads

| Call | Function | Answers |
| --- | --- | --- |
| `clock_gettime(2)` | `UnixClock.clockGettime (clockId : int)` | `Result<Result<UnixTimestamp, UnixError>, ClockGettimeRefusal>` |
| `gettimeofday(2)` | `UnixClock.gettimeofday` | `UnixTimestamp` |
| Linux's `getrandom(2)` | `UnixEntropy.getRandom (task : 'Task) (buffer : UserBuffer) (count : uint64) (flags : uint32)` | `Result<GetRandomAnswer * UnixSystem, GetRandomRefusal>` |
| Darwin's `getentropy(2)` | `UnixEntropy.getEntropy (buffer : UserBuffer) (length : uint64)` | `Result<GetEntropyAnswer * UnixSystem, GetEntropyRefusal>` |
| `getpid(2)` | `UnixSystem.processId` | `ProcessId` |
| `umask(2)` | `UnixSystem.umask (mask : int)` | `PermissionBits * UnixSystem` |
| `getresuid(2)` | `UnixCredentials.getresuid (real : UserBuffer) (effective : UserBuffer) (saved : UserBuffer)` | `Result<GetIdsAnswer<UserId>, GetIdsRefusal>` |
| `getresgid(2)` | `UnixCredentials.getresgid (real : UserBuffer) (effective : UserBuffer) (saved : UserBuffer)` | `Result<GetIdsAnswer<GroupId>, GetIdsRefusal>` |
| `setresuid(2)` | `UnixCredentials.setresuid (real : UserId option) (effective : UserId option) (saved : UserId option)` | `Result<SyscallAnswer * UnixSystem, SetIdsRefusal>` |
| `setresgid(2)` | `UnixCredentials.setresgid (real : GroupId option) (effective : GroupId option) (saved : GroupId option)` | `Result<SyscallAnswer * UnixSystem, SetIdsRefusal>` |
| `setgroups(2)` | `UnixCredentials.setgroups (size : int) (words : GroupListWords)` | `Result<SyscallAnswer * UnixSystem, SetGroupsRefusal>` |
| `clone(2)` with `CLONE_THREAD`, as `pthread_create(3)` makes it | `UnixTaskLifecycle.spawn (parent : 'Task) (child : 'Task) (cpu : CpuId)` | `Result<SpawnAnswer * UnixSystem, SpawnRefusal>` |
| Linux's thread `exit(2)`, as `pthread_exit(3)` makes it | `UnixTaskLifecycle.exitThread (task : 'Task) (status : int)` | `Result<TaskOutcome, ThreadExitRefusal>` |
| `exit_group(2)`, as `exit(3)` and `_exit(2)` make it | `UnixTaskLifecycle.exitGroup (task : 'Task) (status : int)` | `EndedProcess` |
| the client's report that a task runs on a processor | `UnixScheduling.dispatch (task : 'Task) (cpu : CpuId)` | `UnixSystem` |
| where a task is running | `UnixScheduling.runningOn (task : 'Task)` | `CpuId option` |
| Linux's `getcpu(2)` | `UnixScheduling.getcpu (task : 'Task)` | `Result<GetCpuAnswer, GetCpuRefusal>` |

`UnixEntropy.getRandomMaxTransfer` is the most one `getrandom` moves.
`gettid(2)` is `UnixTaskState.osThreadId` of the task, and `getuid(2)` and `getgid(2)` are fields of `UnixSystem.credentials` (`Credentials.RealUser`, `Credentials.RealGroup`).
`uname(2)` reports the platform: `SimulatedUnixPlatform.unixRelease` and `SimulatedUnixPlatform.architecture` of `UnixSystem.platform`.

#### What is not modelled

These calls have no function, and a client should refuse them rather than guess: `getppid`, `nanosleep` and `clock_nanosleep` (a client can sleep a task as a `poll` of nothing with a timeout), `select`, `pselect` and `ppoll`, `readv` and `writev`, `sendto`, `recvfrom`, `sendmsg` and `recvmsg`, `shutdown`, `socketpair`, `truncate`, `fsync` and `fdatasync`, `getrlimit` and `setrlimit`, `sched_yield`, `futex`, `mmap` and its kin, `fork`, `exec` and `wait`, `sigaltstack`, and the timers (`alarm`, `setitimer`, `timer_create`).
"What the kernel leaves to the client" says which of those are the client's by design.

#### Calls that copy in from the caller's memory

A real kernel copies an argument in from the caller's memory at a point in the call, and what happens before that point is observable: a bad descriptor is `EBADF` whatever the buffer holds.
So each call that copies bytes in comes as two functions.
The first, an admission (`UnixReadWrite.admitWrite`, `UnixReadWrite.admitSend`, `UnixReadWrite.admitPWrite`, `UnixSocket.admitSockaddrCopy`, `UnixSocket.admitSetSockOpt`, `UnixSocket.admitGetSockOpt`), changes nothing, and answers either the call's answer (a failure before the copy) or how many bytes to copy.
The second takes exactly those bytes.

Calls that copy in two pathnames come as phases too, for a client that reads its arguments out of a live process, where reading the second pathname too early is itself observable: `UnixNamespace.renameatSourcePhase` or `UnixNamespace.renameSourcePhase` then `UnixNamespace.renameWithDestination`; `UnixNamespace.linkatSourcePhase` or `UnixNamespace.linkSourcePhase` then `UnixNamespace.linkWithDestination`; `UnixNamespace.symlinkatTargetPhase` or `UnixNamespace.symlinkTargetPhase` then `UnixNamespace.symlinkWithPath`; `UnixNamespace.cloneFileFlagsPhase`, `UnixNamespace.cloneFileSourcePhase` then `UnixNamespace.cloneFileWithDestination`; and `UnixPathResolution.faccessatScreenPhase` or `UnixPathResolution.accessScreenPhase` then `UnixPathResolution.accessWithPath`.
A client that holds every argument already, such as a replayer, uses the one-shot functions in the tables.

#### One entry point

`UnixSystem.step (task : 'Task) (call : Syscall)` reaches some of the syscalls through one function, as cases of the `Syscall` type, for a client that wants to log, replay or generate them; it answers `Result<SyscallOutcome * UnixSystem, SyscallRefusal>`.
Only the calls with a `Syscall` case are reachable through it, from `dup` and `fcntl` to `setgroups`: each answers a `SyscallOutcome`, which is one integer, an errno, or a sleep.
Many calls are not there, `open`, `socket`, `bind`, `connect`, `kill` and `sigaction` among them, and are reached only through their own functions.

### Checking the tables

`UnixSystem.checkInvariants` lists every way a system's tables disagree with each other.
No sequence of syscalls should ever produce one.
It is two halves: the rules about the machine, which read facts every process on it contributes to, and `UnixSystem.checkViewInvariants`, the rules about one process's view of it.
A process alone on its machine is held to both in full. Of a view a `SimulatedMachine` focused, only the machine's rules that one process can check truthfully are run; `SimulatedMachine.checkInvariants` runs the rest, against every process at once.
Each table's own rules are checked apart from these: `FileDescriptorRegistry.checkInvariants` of `UnixSystem.fileDescriptors` for the descriptor table and the open file descriptions it names, and `VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes system) (UnixSystem.fileSystem system)` for the filesystem.
A fact a syscall needs about other processes is kept on the machine's object rather than derived from the processes: each open file description counts the descriptors naming it and the holds of calls in flight on it, the thread ID allocator records which IDs live tasks hold, a kqueue records the process that owns it, whose descriptor numbers its registrations name, and each kqueue registration records the socket it is attached to.

Every descriptor lies below `SimulatedUnixPlatform.descriptorBound`, the soft `RLIMIT_NOFILE` a process of the flavour starts with (1024 on Linux, 256 on Darwin): the library assumes the process's limit is at least that, and refuses a call that would put a descriptor at or above it (`DescriptorLimitRefusal`), whose answer would depend on the limit.

## Moving bytes

This library never reads or writes user memory.
A call that moves bytes takes them or returns them, and the client copies them to or from the process's memory.

**Reading.** `UnixReadWrite.read` answers a `ReadOutcome`.
`ReadOutcome.Answered` holds a `ReadAnswer`: the bytes read (`ReadAnswer.Completed`), for the client to copy into the caller's buffer, and whose length the call returns; or, for `/dev/urandom`, a draw from the entropy pool (`ReadAnswer.Drawn`), whose bytes the client takes with `EntropyDraw.bytes` and whose length is `EntropyDraw.count`; or an errno (`ReadAnswer.Failed`).
`ReadOutcome.WouldBlock` is a read that sleeps, and `ReadOutcome.Restarts` one a signal handler interrupted, which the client issues again once the handlers have run.

**Writing.** A write is up to four calls.

1. `UnixReadWrite.admitWrite` answers a `WriteAdmission`, before the kernel reads the caller's buffer:
   * `WriteAdmission.Answered` is the call's answer, with no bytes read.
   * `WriteAdmission.Transfer count`: copy exactly `count` bytes from the start of the caller's buffer and pass them to `UnixReadWrite.write`.
   * `WriteAdmission.TransferThenSleep (count, total)`: a blocking write of `total` bytes, of which only `count` fit now. Copy those `count` and pass them to `UnixReadWrite.writeThenSleep`, which puts them in and sleeps for the rest. A blocking write of more than a pipe holds (64 KiB on Linux) reaches this at once.
2. A write that sleeps answers `WriteOutcome.WouldBlock`, from whichever of these functions it reached. Once `UnixWait.wakes` wakes it, `UnixReadWrite.admitFinishWrite` answers a `WriteResumption`: the call's answer (`WriteResumption.Answered`), or `WriteResumption.Transfer (offset, count)`, the next `count` bytes of the caller's buffer from `offset` on, which the client passes to `UnixReadWrite.finishWrite`. That may sleep again, and the client repeats this step.

Every step answers a `WriteOutcome`, whose cases besides `WouldBlock` are:

* `WriteOutcome.Returns (answer, system)`: the call answers.
* `WriteOutcome.ReturnsRaising (answer, signal, system)`: the call answers, and raised `signal`, which is now pending (or was discarded). The one signal a write raises is `SIGPIPE`, with `EPIPE`, for a write into a pipe with no reader, into a Linux stream socket with no peer, or to a connection that was reset.
* `WriteOutcome.ProcessEnded ended`: that `SIGPIPE`, at its default, killed the process (see "Ending the process").
* `WriteOutcome.Restarts system`: a signal handler interrupted the sleep before anything was written, and the client issues the `write` again once the handlers have run. Only `admitFinishWrite` answers this.

`send` is the same with `UnixReadWrite.admitSend`, `UnixReadWrite.send` and `UnixReadWrite.sendThenSleep`, and `pwrite` with `UnixReadWrite.admitPWrite` and `UnixReadWrite.pwrite`, which never sleeps.

**Buffers.** Where a real kernel would check a buffer's address, the client describes the address as a `UserBuffer`, and the library decides whether and when the call answers `EFAULT`, against the limit set by `UnixBootImage.withUserAddressLimit`.
A client with a flat address space uses `UserBuffer.Mapped` for an address whose bytes it can read or write and `UserBuffer.Unmapped address` for one it cannot.
The other two cases, `UserBuffer.Opaque` and `UserBuffer.Addressless`, are for a client whose pointers are not always numbers.

## Sleeping and waking

A call that would block does not block.
It answers `WouldBlock` with a `WakeCondition`, and the system that comes with it records the calling task as parked in that call (`UnixTaskState.parkedIn`).
That is the state the kernel sleeps in, which can differ from the one the call arrived with: `flock` gives up the caller's old lock before it waits for the new one.

The library has no scheduler, and does not want one.
Waking is pulled rather than pushed.
The client keeps the set of tasks it is holding asleep, and after each step asks `UnixWait.wakes asleep`, which answers the tasks of that set that may wake now, in the order they parked.
A woken task stays parked until its call is finished, so the client takes it out of its set as it wakes it, and finishes the call when it chooses.
`UnixWait.satisfied` asks of one task whether its call could get any further, which is cheaper, but does not decide which of several waiters on one queue wakes: where a kernel wakes one waiter at a time, as for an epoll instance or a listener, only `wakes` chooses.

A woken task finishes its call through its family's finishing function, which may answer, park the task again, or say the call restarts because a signal handler interrupted it:

| The task is parked in (`ParkedSyscall`) | Finish it with |
| --- | --- |
| `ParkedSyscall.Flock` | `UnixDescriptor.flockAcquire (task : 'Task)` |
| `ParkedSyscall.Poll`, and `ParkedSyscall.KqueuePoll` (Darwin's `poll`) | `UnixPoll.finishPoll (task : 'Task)` |
| `ParkedSyscall.EpollWait` | `UnixPoll.finishEpollWait (task : 'Task)` |
| `ParkedSyscall.Kevent` | `UnixKqueue.finishKevent (task : 'Task)` |
| `ParkedSyscall.Accept` | `UnixConnection.finishAccept (task : 'Task)` |
| `ParkedSyscall.PipeRead`, `ParkedSyscall.ConnectionRead` (a `read` or `recv`) | `UnixReadWrite.finishRead (task : 'Task)` |
| `ParkedSyscall.PipeWrite`, `ParkedSyscall.ConnectionWrite` (a `write` or `send`) | `UnixReadWrite.admitFinishWrite (task : 'Task)`, then `UnixReadWrite.finishWrite (task : 'Task) (bytes : ImmutableArray<byte>)` |
| `ParkedSyscall.SigSuspend` (a `sigsuspend`, `rt_sigsuspend` or `pause`) | `UnixSignal.finishSigsuspend (task : 'Task)` |

Each answers what the call that parked answered, as in the tables above.

A call that waits with a timeout (`poll`, `epoll_wait`, `kevent`) wakes once its deadline passes.
With nothing runnable, the client asks `UnixWait.deadlines asleep`, which answers each deadline as an instant, in nanoseconds since boot.
The earliest is when the first of those calls times out: the client advances the clock by it less `UnixSystem.nanosecondsSinceBoot`, and asks `UnixWait.wakes` again.

A parked call holds the open file descriptions it waits on (`ParkedSyscall.descriptions`), as a real one holds a reference to each file: a description goes when no descriptor names it and no call holds it, so one closed under a sleeping call goes when the call returns.

This client makes a pipe, parks a second task on reading it, wakes it with a write, and ends the process:

```fsharp
open System.Collections.Immutable
open WoofWare.PosixKernel

let orFail (describe : 'Refusal -> string) (result : Result<'a, 'Refusal>) : 'a =
    match result with
    | Ok value -> value
    | Error refusal -> failwith (describe refusal)

let platform = SimulatedUnixPlatform.linuxX64

let booted : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> orFail LaunchTableRefusal.describe

    UnixSystem.initial platform
    |> UnixBootImage.boot launch
    |> orFail LaunchRefusal.describe

// Every call ends with the calling task's return to user mode, where it takes
// its signals. This client installs no handler, so it expects none to run.
let resume (task : int) (system : UnixSystem<int, unit>) : UnixSystem<int, unit> =
    match UnixSignal.onReturnToUser task system |> orFail SignalReceiverRefusal.describe with
    | ReturnToUserOutcome.Resumes system -> system
    | other -> failwith $"unexpected at the return to user mode: %A{other}"

// pipe2(fds, 0) by task 0. UserBuffer.Mapped: the array for the two descriptors is writable.
let readFd, writeFd, piped =
    match UnixPipe.pipe2 0 UserBuffer.Mapped booted |> orFail Pipe2Refusal.describe with
    | Pipe2Answer.Created (readFd, writeFd), system -> readFd, writeFd, resume 0 system
    | Pipe2Answer.Failed error, _ -> failwith $"pipe2 failed: %O{error}"

// Task 0 starts a thread, which this client names 1, on processor 0.
let spawned =
    match UnixTaskLifecycle.spawn 0 1 (CpuId 0) piped |> orFail SpawnRefusal.describe with
    | SpawnAnswer.Spawned _, system -> resume 0 system
    | SpawnAnswer.Failed error, _ -> failwith $"spawn failed: %O{error}"

// Task 1 reads the empty pipe, and sleeps. The client holds it asleep.
let asleep, parked =
    match UnixReadWrite.read 1 readFd UserBuffer.Mapped 64UL spawned |> orFail ReadRefusal.describe with
    | ReadOutcome.WouldBlock _, system -> Set.singleton 1, system
    | outcome, _ -> failwith $"expected the read to sleep: %A{outcome}"

// Task 0 writes "hello": admitWrite says how many bytes the call takes from
// the caller's buffer, and write takes them.
let written =
    let payload = "hello"B

    match
        UnixReadWrite.admitWrite 0 writeFd UserBuffer.Mapped (uint64 payload.Length) parked
        |> orFail WriteRefusal.describe
    with
    | WriteOutcome.Returns (WriteAdmission.Transfer count, system) ->
        match UnixReadWrite.write 0 writeFd (ImmutableArray.Create (payload, 0, count)) system |> orFail WriteRefusal.describe with
        | WriteOutcome.Returns (WriteAnswer.Completed n, system) ->
            printfn "task 0 wrote %d bytes" n
            resume 0 system
        | outcome -> failwith $"unexpected write: %A{outcome}"
    | outcome -> failwith $"unexpected admission: %A{outcome}"

// The write woke task 1, and finishRead finishes its call.
let woken =
    match UnixWait.wakes asleep written with
    | [ 1, _ ] ->
        match UnixReadWrite.finishRead 1 written |> orFail ReadRefusal.describe with
        | ReadOutcome.Answered (ReadAnswer.Completed bytes), system ->
            printfn "task 1 read %s" (System.Text.Encoding.ASCII.GetString (bytes.AsSpan ()))
            resume 1 system
        | outcome, _ -> failwith $"unexpected read: %A{outcome}"
    | wakes -> failwith $"expected task 1 to wake: %A{wakes}"

// Task 1 exits, and then task 0 calls exit_group(3), which ends the process.
let ended =
    match UnixTaskLifecycle.exitThread 1 0 woken |> orFail ThreadExitRefusal.describe with
    | TaskOutcome.Continues system -> UnixTaskLifecycle.exitGroup 0 3 system
    | TaskOutcome.ProcessEnded _ -> failwith "task 0 is still running"

// The client ends the process on its machine, which closes its descriptors.
match SimulatedMachine.ofSystem (EndedProcess.endedIn ended) |> SimulatedMachine.endProcess ended with
| Ok (termination, _) ->
    printfn "the process ended: shell status %d" (ProcessTermination.shellStatus (SimulatedUnixPlatform.signalNumbering platform) termination)
| Error refusal -> failwith (ProcessEndRefusal.describe refusal)

// Prints:
// task 0 wrote 5 bytes
// task 1 read hello
// the process ended: shell status 3
```

## Signals

`UnixSignal.kill` and `UnixSignal.pthreadKill` generate a signal, and answer a `KillOutcome`: the process carries on (`KillOutcome.ProcessContinues`), with the signal pending or discarded; the signal stopped it (`KillOutcome.ProcessStopped`); or the signal killed it at once (`KillOutcome.ProcessEnded`).
`kill` of another process, or of a process group, is refused.
`UnixSignal.sigaction` installs a `SignalDisposition`: `SignalDisposition.Default`, `SignalDisposition.Ignore`, or `SignalDisposition.Catch` of a `SignalCatch`, made with `SignalCatch.ofHandler`, which holds the client's `'Handler`.

`UnixSignal.sigprocmask` changes every task's mask on Darwin, as Darwin's does, and refuses (`SigprocmaskRefusal`) a call that would unblock a signal pending for a task other than the caller: Darwin was measured neither to wake that task for it nor always to deliver it as the task next returns to user mode, both of which this library would do.
`UnixSignal.sigsuspend` replaces a task's mask until a signal ends the call, and keeps the mask it replaced in the signal state (`SignalState.maskToRestore`) rather than in the park, because the call's answer comes before the task's return to user mode, which is what restores it: the first handler frame that return pushes saves the mask from before the call.

### The return to user mode

A signal is taken as a task returns to user mode, so after each call a task makes, and before it runs any of its own code, the client asks `UnixSignal.onReturnToUser task`.
It answers a `ReturnToUserOutcome`:

* `ReturnToUserOutcome.Resumes system`: the task took nothing the client acts on, and runs its own code next.
* `ReturnToUserOutcome.RunHandlers (frames, system)`: the frames the kernel pushed, innermost first, each with the signal, the disposition and the mask to restore. The client runs each handler, and when one returns it calls `UnixSignal.sigreturn` with that frame's `HandlerFrame.Id` and asks `onReturnToUser` again.
* `ReturnToUserOutcome.ProcessEnded ended`: a signal whose default action terminates the process killed it (see below).
* `ReturnToUserOutcome.ProcessStopped (signal, system)`: a signal whose default action stops the process stopped it (see below).
* `ReturnToUserOutcome.ContinueDiscarded (signal, system)`: the kernel discarded a `SIGCONT` at its default as the task took it, and the client asks `onReturnToUser` again, since the return goes on, under a `sigsuspend`'s temporary mask if the task is returning from one.

A signal whose default terminates the process ends it where the library applies the default: at generation, when some task can take it (`KillOutcome.ProcessEnded`, and `WriteOutcome.ProcessEnded` for a write's `SIGPIPE`), and otherwise when a task that blocked it can take it, as the task returns to user mode (`ReturnToUserOutcome.ProcessEnded`): after a mask call or a `sigreturn` that unblocks it, or from a `sigsuspend` whose temporary mask lets it through.
Either answer is the `EndedProcess`, killed by the signal (`ProcessTermination.Signaled`, with the core flag), and no handler runs, not even one whose frame was pushed at the same return; the client ends the process on its machine with `SimulatedMachine.endProcess`, as for an exit.

A signal whose default stops the process is reported (`KillOutcome.ProcessStopped`, `ReturnToUserOutcome.ProcessStopped`) for the client to act on.
The library holds no stopped state, and nothing here continues a stopped process, so a client should refuse a stop rather than carry on as if the process were still running.

This client blocks `SIGTERM`, sends it to itself, and dies of it as it unblocks it:

```fsharp
open WoofWare.PosixKernel

let orFail (describe : 'Refusal -> string) (result : Result<'a, 'Refusal>) : 'a =
    match result with
    | Ok value -> value
    | Error refusal -> failwith (describe refusal)

let platform = SimulatedUnixPlatform.linuxX64
let numbering = SimulatedUnixPlatform.signalNumbering platform

let booted : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> orFail LaunchTableRefusal.describe

    UnixSystem.initial platform
    |> UnixBootImage.boot launch
    |> orFail LaunchRefusal.describe

let returnToUser (system : UnixSystem<int, unit>) : ReturnToUserOutcome<int, unit> =
    UnixSignal.onReturnToUser 0 system |> orFail SignalReceiverRefusal.describe

let resumed (outcome : ReturnToUserOutcome<int, unit>) : UnixSystem<int, unit> =
    match outcome with
    | ReturnToUserOutcome.Resumes system -> system
    | other -> failwith $"unexpected at the return to user mode: %A{other}"

// pthread_sigmask(how, {SIGTERM}, NULL), with `how` under Linux's numbering:
// SIG_BLOCK is 0 and SIG_UNBLOCK 1. Its Error is an errno, not a refusal.
let sigterm = SignalMask.ofSignals numbering (Set.singleton Signal.SIGTERM)

let mask (how : int) (system : UnixSystem<int, unit>) : UnixSystem<int, unit> =
    match UnixSignal.pthreadSigmask 0 how (Some sigterm) system with
    | Ok (_previous, system) -> system
    | Error errno -> failwith $"pthread_sigmask failed: %O{errno}"

let blocked = booted |> mask 0 |> returnToUser |> resumed

// kill(getpid(), SIGTERM): the signal stays pending, because task 0 blocks it.
let pending =
    let pid = ProcessId.toInt32 (UnixSystem.processId blocked)
    let signo = Signal.toRawSignoUnder numbering Signal.SIGTERM

    match UnixSignal.kill pid signo blocked |> orFail KillRefusal.describe with
    | Ok (KillOutcome.ProcessContinues system) -> system |> returnToUser |> resumed
    | Ok outcome -> failwith $"unexpected kill: %A{outcome}"
    | Error errno -> failwith $"kill failed: %O{errno}"

// Unblocking it: as task 0 returns to user mode it takes SIGTERM, whose
// default action ends the process.
match pending |> mask 1 |> returnToUser with
| ReturnToUserOutcome.ProcessEnded ended ->
    match SimulatedMachine.ofSystem (EndedProcess.endedIn ended) |> SimulatedMachine.endProcess ended with
    | Ok (termination, _) ->
        printfn "%A" termination
        printfn "shell status %d" (ProcessTermination.shellStatus numbering termination)
    | Error refusal -> failwith (ProcessEndRefusal.describe refusal)
| other -> failwith $"expected SIGTERM to end the process: %A{other}"

// Prints:
// Signaled (SIGTERM, false)
// shell status 143
```

## Threads, and the end of the process

`UnixTaskLifecycle.spawn parent child cpu` starts a task the client names `child`, on processor `cpu`, with a copy of `parent`'s signal mask, and answers its thread ID (`SpawnAnswer.Spawned`).
`UnixTaskLifecycle.exitThread task status` ends one task, and answers `TaskOutcome.Continues` with the process carrying on, or `TaskOutcome.ProcessEnded` if it was the last.
It refuses (`ThreadExitRefusal`) a task parked in a syscall, the process's leader while any other task lives, and, on Darwin, the process's last task: a Darwin client ends a process with `exitGroup`, never by its last thread exiting.
`UnixTaskLifecycle.exitGroup task status` ends every task at once, parked ones included.

A call that ends the process answers an `EndedProcess` instead of a system: `exitGroup`, an `exitThread` of the last task, a `kill`, `pthreadKill` or write whose signal killed it, and a return to user mode that takes such a signal.
`EndedProcess.termination` is how the process ended (`ProcessTermination.Exited` or `ProcessTermination.Signaled`), which is what its parent's `wait` reads; `ProcessTermination.waitpidStatus` encodes it as `waitpid(2)` would.
`EndedProcess.processId` is which process it was, and `EndedProcess.endedIn` the view it ended in.
Only such a call makes one, and `SimulatedMachine.endProcess` takes nothing else.

`SimulatedMachine.endProcess ended machine` ends the process on the machine: it releases what only the process's calls held, then closes every descriptor in the order each kernel measurably does (Linux drops them lowest first and releases the last let go of first; Darwin closes them highest first), as `close` closes each: sending its peers their FINs, or resets where it left bytes unread, and letting its locks, pipes, listeners and event queues go, and removes the process.
It refuses (`ProcessEndRefusal`) where a close would: a listener holding a connection another process's open socket made is not released, since what the reset does to that socket is not measured.
A process alone on its machine is ended on the machine `SimulatedMachine.ofSystem (EndedProcess.endedIn ended)` makes of the view the process ended in.

## Several processes

A `SimulatedMachine` holds several processes on one machine.
`SimulatedMachine.ofSystem` makes one of a booted system, `SimulatedMachine.processIds` lists the processes on it, and `SimulatedMachine.launch launch` starts another process on it from a `ProcessLaunch`, whose current directory (`ProcessLaunch.withCurrentDirectory`) must exist in the machine's filesystem; the kernel chooses its process ID, as Linux does from its thread ID counter and Darwin from a process ID counter of its own.
Each process names its own tasks: two processes may both call a task `0`.

A syscall is still made in a `UnixSystem`.
`SimulatedMachine.focus` gives one process's view of the machine, which holds that process and no other, so no call can read or change another process's own state, and `SimulatedMachine.unfocus` writes the view back.
`SimulatedMachine.inView` and `SimulatedMachine.step` do both around one call.
Focusing for each call and writing the view back after it is always correct.
A view records which state of the machine it was taken from, and `unfocus` refuses a view taken before some other write-back, whose copy of the machine is stale.
A client that would rather keep a view of a process that is making no calls, while other processes' views are written back, can ask `SimulatedMachine.holdsProcessOf` whether only the machine in it has gone stale, so that focusing the process again loses nothing.

Everything one process's call does to another goes through the machine they share: ports, connections, pipes, files and their locks, the open file descriptions, the directories processes stand in, the thread and process IDs, and the processors.
A dispatch made in one process's view can displace another process's task from its processor, which is a change to the machine, so every other view goes stale as for any other call.
`SimulatedMachine.checkInvariants` holds every process to the machine and to each other; `UnixSystem.checkInvariants` and `FileDescriptorRegistry.checkInvariants` of one view check only what one process can see truthfully.

A call asleep in one process wakes for what another process's call does: a connection queued on its listener, bytes arriving on its socket or room freed in its send buffer, a peer's FIN or reset, a lock let go of.
`SimulatedMachine.wakes asleep` is `UnixWait.wakes` of every process at once, where `asleep` maps each process to the tasks of it the client holds asleep.
It must name every task the client holds asleep, in every process: a parked task it leaves out counts as one the client has woken and not yet finished, which, on a queue whose waiters wake one at a time, will take what arrives, so none of the others wakes for it.
Each sleeping task's condition is asked of its own process's view, and where a kernel wakes one waiter of a queue at a time, the one is chosen across every process by the machine's park order.
A woken call is finished in its own process's view.
A kqueue registration, and a filter a sleeping Darwin `poll` registered, is attached to the socket its descriptor named, as XNU attaches a knote to the socket, so an event on the socket reaches it whichever process caused the event; a sleeping Darwin poll's kqueue (`PollQueue`) is the machine's for as long as the call sleeps.

Nothing passes a descriptor from one process to another (no `fork`, no `SCM_RIGHTS`, and a launched pipe's other end is the client's), so no pipe, listener or epoll instance is shared, and `kill` of another process is refused.

## What the kernel leaves to the client

A running process relies on more than its kernel, and this library is only the kernel.
Each part below is something a real process has which this library does not do, so the client must.
For each part, this section says what the part is, why it belongs to the client, and how the client and the library meet at that line.
Where a part was placed by a decision, that is said; where it is only how things stand today, that is said instead.

### User memory

**What.** The process's address space, and the calls that change it: `mmap`, `munmap`, `mprotect`, `brk`.
A thread's stack and its thread-local storage are part of it.

**Why the client's.** The client runs the process's code, so it already holds every byte that code loads and stores.
A second copy in the kernel would have to agree with the client's after every store.
A syscall needs only the bytes it moves, and the client can hand those over.

**Where they meet.** See "Moving bytes": the library takes and returns the bytes a call moves, and decides from a `UserBuffer` whether and when a call answers `EFAULT`.
A pointer that a C library function dereferences in the process itself, before it makes any system call, is the client's to answer for, since no kernel is involved.

A mapping of a file is not modelled, and a client should refuse one rather than copy the file's bytes into memory of its own.
Its pages are the file's bytes, and it holds the file's open file description as a descriptor does, so it crosses this line: a private copy would not see later writes through a descriptor, and the library would not count the mapping as holding the file open.
Nothing here maps a file.

### Blocks inside the process

**What.** A thread that waits without asking the kernel to watch a descriptor or a clock: Linux's `futex`, Darwin's `__psynch_*` and `__ulock_*` calls, and the mutexes, condition variables and timed waits that the C library builds on them.

**Why the client's, for now.** A `futex` wait is keyed by a user address and compares a word of user memory, which is the client's (see above).
So today the client keeps these blocks itself.
The long-term plan is to model `futex` and `__psynch` in the kernel, so that every block happens there and the client has no blocked tasks of its own (see [the tracking issue](https://github.com/Smaug123/WoofWare.PawPrint/issues/1726)).
That is larger work, separate from the rest, and not yet scheduled.

**Where they meet.** A task the client holds blocked is parked in no call this library knows of, so as far as the library knows it could run.
The client removes such tasks from the set this library would call runnable (see the next part).
`UnixWait.wakes` decides only the parks this library recorded: whether a signal or a deadline ends one of the client's own blocks is the client's to decide.
When nothing can run, the client advances the clock to the earlier of its own deadlines and those `UnixWait.deadlines` reports.

### Which task runs

**What.** Choosing the next task to run, and on which processor.

**Why the client's.** By decision: the kernel never chooses which task runs.
It answers which tasks could run (not parked in the kernel; the client subtracts its own blocked threads), and records what the client says happened.
Choosing is policy, and a client that explores schedules, or a harness that steers one from outside, must own it; the kernel keeps only the facts a choice is made from.

**Where they meet.** `UnixSystem.tasks` lists the process's tasks, and `UnixTaskState.park` says which are parked in a syscall.
The client keeps the set of tasks it holds asleep and passes it to `UnixWait.wakes`, which says which may wake now; it decides when a woken task finishes its call.
Which processor a task belongs on is the client's policy too.
The client reports each task it runs with `UnixScheduling.dispatch task cpu`: "task T is now running on processor c".
The kernel keeps what Linux keeps.
Each task has a processor, the one it last ran on, or before it first runs, the one its creator named (`ProcessLaunch.create`, `UnixTaskLifecycle.spawn`); `UnixTaskState.cpu` reports it, for a task found in `UnixSystem.tasks`.
Each processor runs at most one task, so a dispatch displaces whichever task the processor ran before, whatever process it belongs to; that task keeps the processor as its own, as a preempted task does.
A task stops running when it parks in a syscall, and when it exits or its process ends; a parked task may still be dispatched, which is how a woken task gets back onto a processor to finish its call.
`UnixScheduling.runningOn` says where a task is running, and `UnixScheduling.getcpu` answers only for a running task, failing loudly for any other: a task making a syscall is running, so a client whose records say otherwise has a bug.
Nothing else requires its caller to have been dispatched, so a client that never reads placement need never report it, and a woken task's call can be finished without a dispatch first.
Every processor named must be one the machine has (`UnixSystem.processorCount`).
Not built yet: recording the processor time a task used, and a default policy (round-robin, and PCT, probabilistic concurrency testing) as a module that knows nothing of the kernel.

### When time passes

**What.** How far the clock moves, and when.

**Why the client's.** Only the client knows how long the process's code took to run, and how fast its simulated machine is.
Nothing in this library moves the clock on its own.
By decision, `UnixSystem.advanceClock` stays a function from one system to the next, with no other result.
If clock-driven signals (`alarm`, `setitimer`, `timer_create`) are modelled later, an expiry will be written into the state as a pending signal, which the client finds through `UnixWait.wakes` as it finds a passed deadline.

**Where they meet.** The client calls `UnixSystem.advanceClock nanoseconds` between syscalls, to move the clock on by that many nanoseconds, and reads the time with `UnixSystem.nanosecondsSinceBoot`.
With nothing runnable, the earliest of `UnixWait.deadlines` is the instant the first sleeping call times out (see "Sleeping and waking").
An administrator's write of Linux's `kernel.pid_max` is the outside world acting on the machine in the same way: `UnixSystem.writePidMaxSysctl`.

### The errno slot

**What.** The per-thread `errno` that a C program reads after a failed call.

**Why the client's.** On a real Unix, `errno` lives in the C library rather than in the kernel: the kernel returns an error code, and the syscall wrapper stores it.
The same goes for the C library's other state in user space, such as the copy of the environment that `setenv` and `putenv` change.

**Where they meet.** A failed call answers `Failed` with a `UnixError`.
The client converts it to a number with `UnixError.toRawErrnoUnder` and stores it wherever its C library would.
The environment the library holds is the one the process was started with (`ProcessLaunch.withEnvironment`).

### Running a signal handler

**What.** Running the handler's code on the task, on some stack, with the arguments it expects.

**Why the client's.** The handler is the process's code, and the client runs that code.
The library never learns what a handler is: it stores whatever the client passes as `'Handler` and hands it back.

**Where they meet.** `UnixSignal.onReturnToUser` answers the frames to run, and `UnixSignal.sigreturn` pops one (see "The return to user mode").
A `SignalCatch` holds only the flags that change what the kernel does (`SA_NODEFER`, `SA_RESETHAND`, `SA_RESTART`); `SA_SIGINFO` and `SA_ONSTACK` choose how and on which stack the client calls its handler, so a client that honours them keeps them with its `'Handler`.
A handler that leaves by `siglongjmp` instead of returning has no operation yet.

### The alternate signal stack

**What.** `sigaltstack`, which names a region of memory for handlers to run on.

**Why the client's.** The region is user memory, and the answers `sigaltstack` gives (`SS_ONSTACK`, and `EPERM` for changing the stack while on it) depend on the user stack pointer, which only the client knows.
If a client needs it answered, a form that takes the client's stack pointer as an argument would be small.

**Where they meet.** Nowhere yet: the library has no `sigaltstack`, and runs no handler, so it never chooses a stack.

### Starting and ending processes

**What.** Creating a process, choosing the program it runs, and collecting its exit status.

**Why the client's.** There is no `fork`, `exec` or `wait`.
Loading and running a program is the client's, as running any of the process's code is, and `fork`'s copy of the address space would be a copy of user memory.
The descriptor table, signal dispositions, mask and credentials that `fork` and `exec` pass on are the kernel's, so those calls would be the library's to model; they are not modelled, and a client should refuse a process's request to create or replace a process.

**Where they meet.** The client starts a process with `SimulatedMachine.launch` and a `ProcessLaunch`, which sets up what a launcher fixes before `exec`; the kernel chooses the process ID.
When a call ends a process, its answer is an `EndedProcess`, which the client passes to `SimulatedMachine.endProcess`: that closes the process's descriptors, removes it, and answers how it ended, as a `ProcessTermination` holding the status a parent's `wait` would read.
No `wait` is modelled, so nothing keeps the ended process for a parent to reap, and reporting its status is the client's.

### The far end of a launched pipe

**What.** Whatever sits outside the simulated machine at the other end of a process's launch descriptors, such as its standard streams.

**Why the client's.** It is the outside world, which the client stands in for.
The pipe itself is the kernel's: its capacity, atomic writes and `EPIPE` are measured rules that the library holds and tests.
So the client chooses only what its end does: supply bytes (`LaunchDescriptor.Supplied`), read every byte (`LaunchDescriptor.Drained`), or be closed already (`LaunchDescriptor.Gone`).

**Where they meet.** The client reads what the process wrote from `UnixSystem.delivered`, a `DeliveryLog` (`DeliveryLog.toList`, or `DeliveryLog.since` a count it read before).

## Flavours and divergence from host platforms

WoofWare.PosixKernel's behaviour does not depend on the host platform; indeed, it probably works on Windows.
However, POSIX is extremely underspecified (and implementations frequently diverge from their documentation!),
and I only have easy access to a few flavours.

A `SimulatedUnixPlatform` names the kernel being simulated: its flavour (Linux or Darwin) and, for Linux, the version of the source it was built from (`LinuxKernelVersion`), its architecture, its page size and its release.
The release is only what `uname` reports; where Linux's behaviour changed between versions, the platform's version decides which answer applies.
Only the combinations that have been measured can be built: Linux on x86-64 and on aarch64 with 4 KiB pages, and Darwin on arm64 with 16 KiB pages.
`SimulatedUnixPlatform.linuxX64`, `SimulatedUnixPlatform.linuxArm64` and `SimulatedUnixPlatform.macOsArm64` are the presets.
Where the flavours disagree, the platform says which answer applies, down to whose numbering an errno or a signal is reported in.
The filesystem type is chosen separately (see "Giving the machine files"), from those its flavour has been seen to report.

The kernel mounts a device filesystem over `/dev` at boot, as a real one does.
On Linux it is a devtmpfs holding a node for each device the kernel has a driver for (`/dev/null` and `/dev/urandom`); since a real devtmpfs holds hundreds, any other name in it is refused rather than answered ENOENT, and so are listing it and adding or removing a name in it.
Opening a node gives a descriptor whose syscalls are the driver's: `/dev/null` reads nothing and swallows every write, and `/dev/urandom` reads from the same entropy pool `getrandom` draws on.
On Darwin it is devfs, which is not modelled, so any path that reaches `/dev` is refused.

WoofWare.PosixKernel is intended to be fully POSIX-compliant eventually.
A design goal of WoofWare.PosixKernel is that the library refuses rather than providing an answer which has not been measured on the platform it's been told to simulate.
It is fully deterministic, so e.g. it chooses a traversal order for directory listing even though that is POSIX-unspecified.
We try very hard to return only values that we have observed from a real system using that platform, rather than just copying semantics from the docs.

Where real platforms of a given flavour have been observed to disagree, we choose a permitted answer (generally an *inconvenient* one, since I want to help you avoid accidentally relying on unspecified behaviour).
For example, directory enumeration order has been observed to be extremely odd on Linux: we've even seen `..` and `.` appear at the *end* of the enumeration, in the GitHub Actions ext4 runner!

## Licence

MIT.
