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

* the machine: the filesystem, the open file descriptions (with the state of each epoll instance, kqueue and inotify instance, and how many descriptors and calls in flight reference each), sockets, connections and pipes, the thread IDs live tasks hold, the processes on it and the directories they stand in, the clock, the entropy pool, and the platform being simulated;
* the process: the descriptor table, which says only which open file description each descriptor names, and the process's credentials, umask, current directory, environment and signal state;
* the tasks (`UnixTaskState`): the process's tasks, and what each is blocked in, if anything. A client looks a task up in `UnixSystem.tasks` by its own name for it, which has no entry for a task that was never created or has exited, and reads what it finds with `UnixTaskState`'s readers: `UnixTaskState.parkedIn` (the syscall it sleeps in), `UnixTaskState.cpu` and `UnixTaskState.osThreadId` (what `gettid(2)` answers).

Those records are opaque outside the library.
A client reads a system through `UnixSystem`'s queries, such as `UnixSystem.leader`, `UnixSystem.tasks`, `UnixSystem.signals`, `UnixSystem.credentials`, `UnixSystem.fileDescriptors`, `UnixSystem.openFiles`, `UnixSystem.delivered` and `UnixSystem.descriptorTarget`, and changes a running one only through the syscalls and the two operations of the outside world described under "When time passes".
`UnixSystem.descriptorTarget` answers what a descriptor's description refers to, an `OpenFileTarget`, of which `OpenFileTarget.Fifo` (a FIFO's inode and an opaque `FifoAttachment`) is reserved: nothing opens a FIFO yet, `FileDescriptorRegistry.checkInvariants` reports a description naming one as a defect, and the case exists so that a client's matches already handle it when FIFOs arrive.
`OpenFileTarget.CharacterDevice` carries, besides the device and its node, an opaque `CharacterDeviceState`: the description's own state, which holds nothing in this version, because Linux keeps no position for a device and Darwin's devices, which keep one per description, are refused.

`'Task` is whatever the client calls a thread, and `'Handler` whatever it calls a signal handler.
The library never looks inside either; it only compares them.

### The client's own operations

Besides the syscalls, a client calls these between them (the system argument is left out, as in the tables under "The syscalls"):

| Operation | Function | Answers |
| --- | --- | --- |
| a task, by the client's name for it | `UnixSystem.tasks` | `Map<'Task, UnixTaskState>` |
| which syscall a task sleeps in | `UnixTaskState.parkedIn (task : UnixTaskState)` | `ParkedSyscall option` |
| the platform, and its flavour | `UnixSystem.platform` | `SimulatedUnixPlatform` |
| the time since boot | `UnixSystem.nanosecondsSinceBoot` | `int64` |
| time passing | `UnixSystem.advanceClock (nanoseconds : int64)` | `UnixSystem` |
| which sleeping tasks wake | `UnixWait.wakes (asleep : Set<'Task>)` | `('Task * Set<WakePrimitive>) list` |
| whether one sleeping task could go on | `UnixWait.satisfied (task : 'Task)` | `Set<WakePrimitive> option` |
| when sleeping calls time out | `UnixWait.deadlines (asleep : Set<'Task>)` | `int64 list` |
| every way the tables disagree | `UnixSystem.checkInvariants` | `UnixSystemDefect list` |

`SimulatedUnixPlatform.flavour` of a platform is its `SimulatedUnixFlavour`, `SimulatedUnixFlavour.Linux` or `SimulatedUnixFlavour.Darwin`, which decides how a raw number is read.
In prose, as in the tables, a type's `'Task` and `'Handler` arguments are left out: `EndedProcess` is `EndedProcess<'Task, 'Handler>`.

## Booting a machine

`UnixSystem.initial` builds the *boot image* of a machine of the given `SimulatedUnixPlatform` that has not done anything yet: a `UnixBootImage`, which no syscall takes.
Configure it with the setters in the `UnixBootImage` module, such as `UnixBootImage.withFileSystem`, `UnixBootImage.withMount`, `UnixBootImage.withBootTime`, `UnixBootImage.withProcessorCount` and `UnixBootImage.withProcessId` (the first process's ID, where the machine's counters start).
Sysctls are configuration too: `UnixBootImage.withInotifyLimits` sets Linux's `fs.inotify` limits (`InotifyLimits`), which default to `UnixSystem.defaultInotifyLimits` (128 instances and 8192 watches per user, 16384 records per instance).
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

// A process on a Linux x86-64 machine, before anything has happened to it:
// a filesystem holding nothing but /dev, and descriptors 0, 1 and 2 as pipes. It has one task,
// which this client names 0, on logical processor 0, and runs as root.
let system : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> ProcessLaunch.withCredentials (Credentials.ofIds UserId.root (GroupId.parseOrFail "example" 0u) [])

    UnixSystem.initial SimulatedUnixPlatform.linuxX64 |> UnixBootImage.boot launch

// mkdir(2) is mkdirat(2) from AT_FDCWD, in the flavour's own numbering.
let atFdCwd : int = AtDirectory.atFdCwd SimulatedUnixFlavour.Linux

let mkdir (system : UnixSystem<int, unit>) : UnixSystem<int, unit> =
    match UnixSystem.step 0 (Syscall.MkDirAt (atFdCwd, path "/tmp", 0o755)) system with
    | SyscallOutcome.Answered (SyscallAnswer.Completed result), system ->
        printfn "mkdir returned %d" result
        system
    | SyscallOutcome.Answered (SyscallAnswer.Failed error), system ->
        let numbering = SimulatedUnixPlatform.rawErrnoNumbering (UnixSystem.platform system)
        printfn "mkdir failed with errno %d" (UnixError.toRawErrnoUnder numbering error)
        system
    | outcome, _ -> failwith $"unexpected from mkdir: %A{outcome}"

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
It throws `ArgumentException` for a name the flavour could never have created, and for anything at `/dev` but an empty directory.

The filesystem's type is what `fstatfs(2)` reports for a file, and it changes a directory's `st_size` and where `lseek(2)` with `SEEK_END` lands on one.
It is tmpfs on Linux and APFS on Darwin unless `UnixBootImage.withMount` says otherwise: `Some (EmulatedMount.defaultOf EmulatedFileSystemType.Nfs)`, for example, or `EmulatedMount.Tmpfs` and `EmulatedMount.Apfs` with their fields set.
A type the flavour could not report is refused: on Linux, which has no APFS, with `ArgumentException`, and on Darwin, where a tmpfs has not been measured, with `UnmodelledException`.
Path resolution keeps its flavour's limits whichever type is chosen.

```fsharp
open System.Collections.Immutable
open WoofWare.PosixKernel

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

    UnixSystem.initial platform
    |> UnixBootImage.withFileSystem UnixTimestamp.epoch root seed
    // fstatfs(2) reports NFS rather than Linux's default, tmpfs.
    |> UnixBootImage.withMount (Some (EmulatedMount.defaultOf EmulatedFileSystemType.Nfs))
    |> UnixBootImage.boot launch

let path (text : string) : PathArgumentBytes =
    match UnixByteString.ofString text with
    | Ok bytes -> PathArgumentBytes.Bytes bytes
    | Error defect -> failwith $"not a path: %O{defect}"

// Task 0, the process's leader, calls open("/etc/hostname", O_RDONLY), and read(fd, buf, 64).
// O_RDONLY is 0 on every flavour.
match UnixNamespace.openPath 0 0 (path "/etc/hostname") 0 system with
| SyscallOutcome.Answered (SyscallAnswer.Completed fd), system ->
    match UnixReadWrite.read 0 (int fd) UserBuffer.Mapped 64UL system with
    | ReadOutcome.Answered (ReadAnswer.Completed bytes), _ ->
        printf "%s" (System.Text.Encoding.ASCII.GetString (bytes.AsSpan ()))
    | outcome, _ -> failwith $"unexpected read: %A{outcome}"
| outcome, _ -> failwith $"unexpected open: %A{outcome}"

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

### Answers, and what this library will not answer

A syscall answers what the kernel does: perhaps an errno (a `UnixError`), or the calling task asleep (see "Sleeping and waking"). Either way it comes with the system after the call; a failing call can still change the system, just as a real one can.

A few functions answer a `Result` whose `Error` is an errno: those whose own failure is a value the client reports to the process, such as `UnixSocket.socket`, `UnixPoll.epollCreate1`, `UnixSignal.kill` and `UnixClock.clockGettime`, and the four that answer nothing else, `UnixSignal.sigaction`, `UnixSignal.sigactionSyscall`, `UnixSignal.pthreadSigmask` and `UnixSignal.rtSigprocmask`.
A function that cannot fail, such as `UnixClock.gettimeofday`, answers without a `Result` at all.
The errors of the parsers that build a value, such as `UnixByteString.ofString`'s, say what was wrong with the input.

Where this library will not say what the kernel does, the call throws `UnmodelledException`, and the docstrings say the call is refused.
Nobody has measured what a real kernel does there, on the platform being simulated, or the model cannot hold what the answer needs.
That is a feature request for this library, not a condition a client handles in normal operation, and retrying will not help.

* Any call may throw `UnmodelledException`, and a minor version may add or remove the places that do: each newly measured behaviour, such as a difference between Linux and Darwin, can bring a new one, and each newly modelled one removes one.
* Its message is for a human, and has no stable structure. It names the function that threw and says what the library knows about why; it does not know what the client called the value, or which of the client's own entry points made the call, and the client adds those.
* A throw leaves nothing behind. Every value this library takes is immutable, so a client may catch the exception and carry on from the state it passed in.

A call that no client following the documentation could make throws the exception .NET uses for that mistake instead, and its docstring says so: `ArgumentException` or `ArgumentOutOfRangeException` for a value no machine of the flavour could have, such as a boot setter's contradictory setting; `InvalidOperationException` for a call made where the documentation rules it out, such as the exit of a task parked in a syscall; and `PlatformNotSupportedException` for a call the flavour does not have, such as `epoll_create1` on Darwin, which a client could have checked for first.

```fsharp
open System.Collections.Immutable
open WoofWare.PosixKernel

let system : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

    UnixSystem.initial SimulatedUnixPlatform.linuxX64 |> UnixBootImage.boot launch

// socket(AF_INET, SOCK_STREAM, 0), in Linux's numbering.
let fd, socketSystem =
    match UnixSocket.socket 2 1 0 system with
    | Ok (fd, system) -> fd, system
    | Error error -> failwith $"socket failed with %O{error}"

// Linux's struct sockaddr_in: the family in host order, then the port and the address in network order.
let sockaddr (address : byte[]) : ImmutableArray<byte> =
    ImmutableArray.CreateRange (Array.concat [ [| 2uy ; 0uy ; 0x13uy ; 0x88uy |] ; address ; Array.zeroCreate 8 ])

// A bind to the multicast group 224.0.0.1 is not modelled: this library models no group membership.
let multicast =
    try
        Some (UnixSocket.bind fd UserBuffer.Mapped 16u (sockaddr [| 224uy ; 0uy ; 0uy ; 1uy |]) socketSystem)
    with :? UnmodelledException ->
        None

printfn "multicast bind answered: %b" multicast.IsSome

// The throw left socketSystem as it was, so the client carries on from it: bind 127.0.0.1:5000.
match UnixSocket.bind fd UserBuffer.Mapped 16u (sockaddr [| 127uy ; 0uy ; 0uy ; 1uy |]) socketSystem with
| BindAnswer.Bound (SocketAddress.Inet endpoint), _ ->
    printfn "bound %s:%d" (Ipv4Address.toString endpoint.Address) endpoint.Port
| answer, _ -> failwith $"unexpected bind: %A{answer}"

// Prints:
// multicast bind answered: false
// bound 127.0.0.1:5000
```

A few public types whose names end in "Refusal" or "Fault" are values a client holds rather than refusals: `BufferRefusal`, which classifies the client's own buffer; `Ipv4PrefixRefusal` and `SignalMaskRefusal`, the errors of parsers; `DirectorySourceRefusal` and `GetCwdDestinationFault`, flavours' rules; and `BindFault`, one of `bind(2)`'s checks.

The setters of the machine's boot configuration and of a process's launch refuse the same way.
A value this library has not measured or does not model throws `UnmodelledException`; one no machine of the flavour could have throws `ArgumentException` or `ArgumentOutOfRangeException`, naming the parameter.

A call that a real kernel would not let happen at all is a bug in the client, and throws: a task making a syscall while it is asleep in another, a name that is no task, or `UnixScheduling.getcpu` of a task that is not running.
A value no `parse` could have produced, such as one built with `Unchecked.defaultof`, is a bug in the client too, and throws.

### Unions that gain cases

From version 1.0, adding a case to any union whose cases are public is a breaking change, and comes only in a major version.
F# code compiled against a union dispatches on its case numbers, so code built against an older version can silently run the wrong arm for a new case.
The model will still grow: a refusal becomes an answer once its behaviour is measured, and each newly measured difference between Linux and Darwin adds a place that throws `UnmodelledException` (see "Answers, and what this library will not answer"), which needs no new case.
Cases that are certain to appear are already present, and nothing produces them yet: `InodeContent.Socket`, `InodeContent.Fifo`, `DirectoryEntryKind.Socket`, `DirectoryEntryKind.Fifo`, `CharacterDevice.Zero`, `CharacterDevice.Random`, `CharacterDevice.Tty`, `OpenFileTarget.Fifo`, `ParkedSyscall.Sleep`, `ParkedSyscall.FifoOpen`, `ParkedSyscall.Select`, `ParkedSyscall.SignalWait`, `WakePrimitive.FifoPartnerOpened` and `WakePrimitive.AwaitedSignalPending`.
Removing a case, changing a case's fields, or changing the shape of any other type is breaking too.

The defects the `checkInvariants` functions report (`UnixSystemDefect`, `VirtualFileSystemDefect`, `FileDescriptorRegistryDefect` and `SimulatedMachineDefect`) are open too, and have no patterns at all.
A defect is a bug in this library, and the thing to do with one is report it with its `describe`; no client needs to branch on which rule broke.
So any version may add a defect, and any version may remove one, as it will when the types come to make the state it forbids unrepresentable.

Before 1.0, any version may change anything.

### The syscalls

In each table, the function's last argument, the system, is left out, and so are the type arguments `'Task` and `'Handler`: `UnixSystem` is `UnixSystem<'Task, 'Handler>`, and `WriteOutcome<WriteAnswer>` is `WriteOutcome<WriteAnswer, 'Task, 'Handler>`.
A path is the argument's bytes (`PathArgumentBytes`), which the kernel copies in where the call's kernel does.
A buffer is a `UserBuffer` (see "User memory"), which says whether the address the caller passed is mapped; the bytes it holds cross the API separately.

A `SyscallAnswer` is the `int64` a call returns (`SyscallAnswer.Completed`), or the errno it fails with (`SyscallAnswer.Failed`).
A `SyscallOutcome` is a `SyscallAnswer` (`SyscallOutcome.Answered`), the task asleep (`SyscallOutcome.WouldBlock`), or the call restarting (`SyscallOutcome.Restarts`).

#### Descriptors

| Call | Function | Answers |
| --- | --- | --- |
| `dup(2)` | `UnixDescriptor.dup (fd : int)` | `SyscallAnswer * UnixSystem` |
| `dup2(2)` | `UnixDescriptor.dup2 (oldFd : int) (newFd : int)` | `SyscallAnswer * UnixSystem` |
| `dup3(2)` | `UnixDescriptor.dup3 (oldFd : int) (newFd : int) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `fcntl(2)`: `F_DUPFD`, `F_DUPFD_CLOEXEC`, `F_GETFD`, `F_SETFD`, `F_GETFL`, `F_SETFL` | `UnixDescriptor.fcntl (fd : int) (command : int) (argument : int)` | `SyscallAnswer * UnixSystem` |
| `fcntl(F_SETFL)` of `fcntl(F_GETFL)` with `O_NONBLOCK` changed | `UnixDescriptor.setNonBlocking (fd : int) (isNonBlocking : bool)` | `SetNonBlockingAnswer * UnixSystem` |
| `O_NONBLOCK` in `fcntl(F_GETFL)` | `UnixDescriptor.isNonBlocking (fd : int)` | `bool option` |
| `lseek(2)` | `UnixDescriptor.lseek (fd : int) (offset : int64) (whence : int)` | `SyscallAnswer * UnixSystem` |
| `flock(2)` | `UnixDescriptor.flock (task : 'Task) (fd : int) (operation : int)` | `SyscallOutcome * UnixSystem` |
| `ftruncate(2)` | `UnixDescriptor.ftruncate (fd : int) (length : int64)` | `SyscallAnswer * UnixSystem` |
| `posix_fadvise(3)` | `UnixDescriptor.posixFadvise (fd : int) (offset : int64) (length : int64) (advice : int)` | `FileAdviceAnswer` |
| `close(2)` | `UnixDescriptor.close (fd : int)` | `SyscallAnswer * UnixSystem` |
| `ioctl(FIONREAD)` | `UnixDescriptor.bytesAvailable (fd : int) (destination : UserBuffer)` | `BytesAvailableAnswer` |
| `ioctl(FICLONE)` | `UnixDescriptor.fileClone (destination : int) (source : int)` | `UnixError` |
| `tcgetattr(3)`, `isatty(3)` | `UnixDescriptor.terminalAttributes (fd : int)` | `TerminalAttributesAnswer` |
| `geteuid(2)` | `UnixDescriptor.effectiveUserId` | `UserId` |
| `getegid(2)` | `UnixDescriptor.effectiveGroupId` | `GroupId` |
| `getgroups(2)` | `UnixDescriptor.getgroups (destination : UserBuffer) (size : int)` | `GetGroupsAnswer` |

#### Paths and their files

| Call | Function | Answers |
| --- | --- | --- |
| `stat(2)` (`SymlinkPolicy.Follow`), `lstat(2)` (`SymlinkPolicy.NoFollowFinal`) | `UnixPathResolution.stat (policy : SymlinkPolicy) (path : PathArgumentBytes)` | `FileStatusAnswer` |
| `fstat(2)` | `UnixPathResolution.fstat (fd : int)` | `FileStatusAnswer` |
| `fstatat(2)` | `UnixPathResolution.fstatat (dirfd : int) (path : PathArgumentBytes) (flags : int)` | `FileStatusAnswer` |
| `chmod(2)` | `UnixPathResolution.chmod (path : PathArgumentBytes) (mode : int)` | `SyscallAnswer * UnixSystem` |
| `fchmod(2)` | `UnixPathResolution.fchmod (fd : int) (mode : int)` | `SyscallAnswer * UnixSystem` |
| `fchmodat(2)` | `UnixPathResolution.fchmodat (dirfd : int) (path : PathArgumentBytes) (mode : int) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `chown(2)` | `UnixPathResolution.chown (path : PathArgumentBytes) (user : UserId option) (group : GroupId option)` | `SyscallAnswer * UnixSystem` |
| `lchown(2)` | `UnixPathResolution.lchown (path : PathArgumentBytes) (user : UserId option) (group : GroupId option)` | `SyscallAnswer * UnixSystem` |
| `fchown(2)` | `UnixPathResolution.fchown (fd : int) (user : UserId option) (group : GroupId option)` | `SyscallAnswer * UnixSystem` |
| `fchownat(2)` | `UnixPathResolution.fchownat (dirfd : int) (path : PathArgumentBytes) (user : UserId option) (group : GroupId option) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `utimensat(2)`, and Linux's `futimens(3)` | `UnixPathResolution.utimensat (dirfd : int) (path : NullablePathArgument) (times : TimesArgument) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `statfs(2)` | `UnixPathResolution.statfs (path : PathArgumentBytes)` | `FileSystemStatisticsAnswer` |
| `fstatfs(2)` | `UnixPathResolution.fstatfs (fd : int)` | `FileSystemStatisticsAnswer` |
| `getcwd(3)` | `UnixPathResolution.getcwd (destination : UserBuffer) (capacity : uint64)` | `GetCwdAnswer` |
| `chdir(2)` | `UnixPathResolution.chdir (path : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `access(2)` | `UnixPathResolution.access (path : PathArgumentBytes) (mode : int)` | `SyscallAnswer` |
| `faccessat(2)` | `UnixPathResolution.faccessat (dirfd : int) (path : PathArgumentBytes) (mode : int) (flags : int)` | `SyscallAnswer` |

`UnixPathResolution.currentDirectoryPath` is the current directory's path, for a client that wants it without a buffer.

A symbolic link's own mode is `UnixPathResolution.fchmodat` with `AT_SYMLINK_NOFOLLOW`: Linux answers `EOPNOTSUPP`, and Darwin changes it.
There is no `lchmod` function, since neither kernel has that syscall.
On Linux, glibc's `lchmod(3)` is that `fchmodat` from `AT_FDCWD`, so a client implementing it calls `fchmodat`.
Darwin's C library implements it with `setattrlist(2)` instead, which answers a non-owner `EACCES` rather than `EPERM`, and refuses `S_ISGID` outside the inode's group with `EPERM` rather than dropping the bit; this library does not model `setattrlist`.

#### Names in the filesystem

| Call | Function | Answers |
| --- | --- | --- |
| `open(2)` | `UnixNamespace.openPath (task : 'Task) (flags : int) (path : PathArgumentBytes) (mode : int)` | `SyscallOutcome * UnixSystem` |
| `openat(2)` | `UnixNamespace.openat (task : 'Task) (dirfd : int) (path : PathArgumentBytes) (flags : int) (mode : int)` | `SyscallOutcome * UnixSystem` |
| `readlink(2)` | `UnixNamespace.readlink (path : PathArgumentBytes) (destination : UserBuffer) (capacity : int)` | `ReadLinkAnswer` |
| `readlinkat(2)` | `UnixNamespace.readlinkat (dirfd : int) (path : PathArgumentBytes) (destination : UserBuffer) (capacity : int)` | `ReadLinkAnswer` |
| one entry of `getdents(2)` or `getdirentries(2)`, without its byte layout | `UnixNamespace.readDirectoryEntry (fd : int)` | `ReadDirectoryAnswer * UnixSystem` |
| `mkdir(2)` | `UnixNamespace.mkdir (path : PathArgumentBytes) (mode : int)` | `SyscallAnswer * UnixSystem` |
| `mkdirat(2)` | `UnixNamespace.mkdirat (dirfd : int) (path : PathArgumentBytes) (mode : int)` | `SyscallAnswer * UnixSystem` |
| `mknod(2)`, regular files only | `UnixNamespace.mknod (path : PathArgumentBytes) (mode : int) (dev : uint32)` | `SyscallAnswer * UnixSystem` |
| `mknodat(2)`, regular files only | `UnixNamespace.mknodat (dirfd : int) (path : PathArgumentBytes) (mode : int) (dev : uint32)` | `SyscallAnswer * UnixSystem` |
| `unlink(2)` | `UnixNamespace.unlink (path : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `rmdir(2)` | `UnixNamespace.rmdir (path : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `unlinkat(2)` | `UnixNamespace.unlinkat (dirfd : int) (path : PathArgumentBytes) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `rename(2)` | `UnixNamespace.rename (source : PathArgumentBytes) (destination : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `renameat(2)` | `UnixNamespace.renameat (olddirfd : int) (oldpath : PathArgumentBytes) (newdirfd : int) (newpath : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| Darwin's `clonefile(2)` | `UnixNamespace.cloneFile (source : PathArgumentBytes) (destination : PathArgumentBytes) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `symlink(2)` | `UnixNamespace.symlink (target : PathArgumentBytes) (path : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `symlinkat(2)` | `UnixNamespace.symlinkat (target : PathArgumentBytes) (dirfd : int) (path : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `link(2)` | `UnixNamespace.link (oldpath : PathArgumentBytes) (newpath : PathArgumentBytes)` | `SyscallAnswer * UnixSystem` |
| `linkat(2)` | `UnixNamespace.linkat (olddirfd : int) (oldpath : PathArgumentBytes) (newdirfd : int) (newpath : PathArgumentBytes) (flags : int)` | `SyscallAnswer * UnixSystem` |

`UnixNamespace.openPath` and `UnixNamespace.openat` take the task because an open can sleep: a blocking open of one end of a FIFO waits until the other end is opened.
Nothing makes a FIFO yet, so both always answer `SyscallOutcome.Answered`.

An inode may be a socket file (`InodeContent.Socket`), which a directory entry reports as `DirectoryEntryKind.Socket`, or a FIFO (`InodeContent.Fifo`), which a directory entry reports as `DirectoryEntryKind.Fifo`.
Nothing produces either yet: `mknod` of `S_IFSOCK` or `S_IFIFO`, and a Unix-domain `bind`, are refused.
The cases exist so that a client's matches already handle them when pathname sockets and FIFOs arrive.

#### Reading and writing

| Call | Function | Answers |
| --- | --- | --- |
| `read(2)` | `UnixReadWrite.read (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64)` | `ReadOutcome * UnixSystem` |
| `recv(2)` | `UnixReadWrite.recv (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (flags : int)` | `ReadOutcome * UnixSystem` |
| `pread(2)` | `UnixReadWrite.pread (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (offset : int64)` | `ReadAnswer * UnixSystem` |
| `write(2)`, before the copy | `UnixReadWrite.admitWrite (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64)` | `WriteOutcome<WriteAdmission>` |
| `write(2)`, given the bytes | `UnixReadWrite.write (task : 'Task) (fd : int) (bytes : ImmutableArray<byte>)` | `WriteOutcome<WriteAnswer>` |
| `write(2)`, given the first bytes of a write that then sleeps | `UnixReadWrite.writeThenSleep (task : 'Task) (fd : int) (total : int) (bytes : ImmutableArray<byte>)` | `WriteOutcome<WriteAnswer>` |
| `send(2)`, before the copy | `UnixReadWrite.admitSend (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (flags : int)` | `WriteOutcome<WriteAdmission>` |
| `send(2)`, given the bytes | `UnixReadWrite.send (task : 'Task) (fd : int) (bytes : ImmutableArray<byte>) (flags : int)` | `WriteOutcome<WriteAnswer>` |
| `send(2)`, given the first bytes of a send that then sleeps | `UnixReadWrite.sendThenSleep (task : 'Task) (fd : int) (total : int) (bytes : ImmutableArray<byte>) (flags : int)` | `WriteOutcome<WriteAnswer>` |
| `pwrite(2)`, before the copy | `UnixReadWrite.admitPWrite (task : 'Task) (fd : int) (buffer : UserBuffer) (count : uint64) (offset : int64)` | `PWriteAdmission` |
| `pwrite(2)`, given the bytes | `UnixReadWrite.pwrite (task : 'Task) (fd : int) (bytes : ImmutableArray<byte>) (offset : int64)` | `WriteAnswer * UnixSystem` |
| `copy_file_range(2)` | `UnixReadWrite.copyFileRange (inFd : int) (outFd : int) (length : uint64) (flags : int)` | `SyscallAnswer * UnixSystem` |
| `pipe2(2)`, and `pipe(2)` as `pipe2` with no flags | `UnixPipe.pipe2 (flags : int) (destination : UserBuffer)` | `Pipe2Answer * UnixSystem` |

#### Sockets

| Call | Function | Answers |
| --- | --- | --- |
| `socket(2)` | `UnixSocket.socket (domain : int) (socketType : int) (protocol : int)` | `Result<int * UnixSystem, UnixError>` |
| `bind(2)` and `connect(2)`, before the copy | `UnixSocket.admitSockaddrCopy (syscall : SockaddrCopySyscall) (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `SockaddrCopyAdmission` |
| `bind(2)`, given the bytes | `UnixSocket.bind (fd : int) (destination : UserBuffer) (declaredLength : uint32) (copied : ImmutableArray<byte>)` | `BindAnswer * UnixSystem` |
| `connect(2)`, given the bytes | `UnixConnection.connect (fd : int) (destination : UserBuffer) (declaredLength : uint32) (copied : ImmutableArray<byte>)` | `ConnectOutcome * UnixSystem` |
| `listen(2)` | `UnixSocket.listen (fd : int) (backlog : int)` | `ListenAnswer * UnixSystem` |
| `accept(2)` | `UnixConnection.accept (task : 'Task) (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `AcceptOutcome * UnixSystem` |
| `shutdown(2)` | `UnixConnection.shutdown (fd : int) (how : int)` | `ShutdownAnswer * UnixSystem` |
| `getsockname(2)` | `UnixSocket.getsockname (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `GetSockNameAnswer` |
| `getpeername(2)` | `UnixSocket.getpeername (fd : int) (destination : UserBuffer) (declaredLength : uint32)` | `GetSockNameAnswer` |
| `setsockopt(2)`, before the copy | `UnixSocket.admitSetSockOpt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (optionLength : uint32)` | `SetSockOptAdmission` |
| `setsockopt(2)`, given the bytes | `UnixSocket.setsockopt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (optionLength : uint32) (supplied : ImmutableArray<byte> option)` | `SetSockOptAnswer * UnixSystem` |
| `getsockopt(2)`, before the length is read | `UnixSocket.admitGetSockOpt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (length : UserBuffer)` | `GetSockOptAdmission` |
| `getsockopt(2)`, given the length | `UnixSocket.getsockopt (fd : int) (level : int) (optionName : int) (value : UserBuffer) (length : UserBuffer) (declaredLength : uint32 option)` | `GetSockOptAnswer * UnixSystem` |

An address crosses the syscall boundary as bytes, laid out as the platform's `struct sockaddr`: `bind` and `connect` take them, and `getsockname`, `getpeername` and `accept` answer them.
Where the kernel hands out an address already decoded, in an answer or a socket's `SocketPhase.DatagramPeer`, it is a `SocketAddress`: an `Ipv4Endpoint`, an `Ipv6Endpoint`, or a `UnixSocketName`, whose representation is hidden and which `UnixSocketName.view` reads as a path or a Linux abstract name (`UnixSocketNameView`).
`BindAnswer.Bound` and `ListenAnswer.Listening` carry the address as `getsockname` would report it at once, and so in the socket's own family: a dual-mode IPv6 socket (`IPV6_V6ONLY` off) bound to an IPv4 address answers it v4-mapped, `::ffff:a.b.c.d`.
This kernel's IPv6 sockets reach only IPv4 peers, through v4-mapped addresses, so a refused `connect` names the IPv4 endpoint it resolved, even from an IPv6 socket.
It binds no Unix-domain socket yet, so nothing it hands out holds a `UnixSocketName`.
A socket's own state, which `UnixSystem.socket` reads, keeps its local address in its domain's own types: `SocketAddressing.Inet` holds an IPv4 binding, `SocketAddressing.Inet6DualMode` a `DualModeBinding`, `SocketAddressing.Inet6V6Only` an IPv6 binding, and `SocketAddressing.Unix` a `UnixSocketName`, each as an option that is `None` while the socket is unbound. An IP binding is an `InetBinding` over the family's endpoint and address types, so an IPv4 socket cannot hold an IPv6 address.
A dual-mode socket bound to an IPv4 address holds it as `DualModeBinding.Mapped`, the IPv4 transport, and presents it v4-mapped.
Only IPv4 bindings are made yet: `UnixSystem.checkInvariants` reports an IPv6 binding or a Unix-domain name as a defect, as it does a datagram peer outside its socket's family.

A connected TCP socket's `read` and `write` move bytes through its connection, which holds each direction's bytes in the sender's send buffer and the receiver's receive buffer, sized from the machine's TCP sysctls, each direction's FIN (not sent, queued behind unsent bytes, or arrived), and each end's state: open, reset, or closed. A close over bytes left unread resets the peer; otherwise it sends a FIN behind what it had sent. A listener's last close resets every client still in its accept queue, oldest first, whatever the listener's `SO_LINGER`. Each queued connection keeps the options the listener held when it completed (`QueuedConnection`), so a change to a listener's options does not reach the connections already queued, and the socket `accept` makes for one holds them; under Darwin a listener lingering for no time gives it a linger of 120 seconds. `poll`, epoll and kqueue read a connected socket's readiness from the same state, and each transfer wakes the waiters each flavour wakes: every arrival of bytes, and room freed in a send buffer (on Linux once after a write ran out of room, when the buffer has drained to two thirds full; on Darwin as bytes leave it). `FIONREAD` reports what waits to be read, and `SO_ERROR` takes a reset's error. A blocking read with nothing to answer sleeps until bytes, a FIN or a reset arrive; a blocking write takes what fits and sleeps for the rest, woken on Linux once its send buffer has drained to two thirds full and on Darwin once there is room for it to take something, and it returns once every byte is taken. Every task asleep on a socket wakes for what it waits on.

`shutdown` shuts a connected TCP socket's sides by each flavour's measured rules: shutting the send side sends a FIN behind what is still unsent, and every later write answers `EPIPE`; shutting the receive side makes reads answer end of file once what was queued is read, which Darwin discards, and an arrival at a Darwin socket shut for receiving, or a Linux one shut both ways, resets the connection. Linux answers 0 to every call on a connection neither reset nor done with its FINs, and `ENOTCONN` otherwise; Darwin answers `ENOTCONN` to a side already shut and applies `SHUT_RDWR` side by side. The socket's readiness, its waiters, its sleeping reads and writes, `getpeername`, Darwin's `setsockopt` and the endpoint it holds follow: an end whose FIN was made after the peer's and has arrived frees its endpoint, open or closed (on Linux, an explicitly bound port only once the socket closes), where one made first holds it. While an end holds its endpoint, a `bind` of it is refused as one over a connected socket is, whether the end's socket is open or has closed: on Linux unless both that socket and the new one carry `SO_REUSEADDR` (a closed end keeps the flag its socket carried), and on Darwin unless the new one does. Linux judges an end in `TIME_WAIT` by the flag it had on entering it, so a Linux `setsockopt` changing the flag there is refused. A connection is forgotten once neither socket refers to it, so the `TIME_WAIT` that an end whose FIN was made first goes on holding after both sockets have closed is not modelled; nor is any TCP timer, so an end that still holds its endpoint once its socket has closed goes on holding it until a reset reaches the connection, its passive FIN arrives, or its peer's socket closes too. Refused: a listener, a Linux socket with no connection (whose `ENOTCONN` still shuts its sides), and Darwin's receive side shut before the peer's unsent bytes, after which only a timer decides; so is Darwin's close behind its own queued FIN once the peer's has arrived, over bytes unread.

`recv` and `send` reach a connected TCP socket through the same transfer, sleep and wake as `read` and `write`, and take their flag word raw, in the flavour's numbering (`MessageFlag.number`). `MSG_PEEK` answers bytes without taking them, `MSG_DONTWAIT` makes a `recv`, and a Linux `send`, non-blocking (Darwin's `send` ignores it), and `MSG_NOSIGNAL` keeps an `EPIPE` from raising `SIGPIPE`; any other flag is refused, naming it. Where they part from `read` and `write` is measured: Linux screens their buffer before it looks up the descriptor, a Linux `recv` of nothing waits as a longer one would, and Darwin's `send` marks no description written.

#### Waiting for several things

| Call | Function | Answers |
| --- | --- | --- |
| `poll(2)` | `UnixPoll.poll (task : 'Task) (entries : PollEntry list) (milliseconds : int)` | `PollOutcome * UnixSystem` |
| `epoll_create1(2)` | `UnixPoll.epollCreate1 (flags : int)` | `Result<int * UnixSystem, UnixError>` |
| `epoll_ctl(2)` | `UnixPoll.epollCtl (epfd : int) (op : int) (fd : int) (event : EpollEventArgument)` | `EpollCtlAnswer * UnixSystem` |
| `epoll_wait(2)` | `UnixPoll.epollWait (task : 'Task) (epfd : int) (maxEvents : int) (buffer : UserBuffer) (milliseconds : int)` | `EpollWaitOutcome * UnixSystem` |
| Darwin's `kqueue(2)` | `UnixKqueue.kqueue` | `int * UnixSystem` |
| Darwin's `kevent(2)` | `UnixKqueue.kevent (task : 'Task) (kq : int) (nchanges : int) (changes : Kevent list) (nevents : int) (eventlist : UserBuffer) (timeout : KeventTimeout)` | `KeventOutcome * UnixSystem` |

#### Watching the filesystem

Linux's inotify.
An instance is an open file description, `OpenFileTarget.Inotify`, holding its watches and the records it has queued (`InotifyState`); a `dup` of its descriptor shares it, and it goes with its last reference.
`read(2)` takes whole records from it, each laid out as `struct inotify_event` (`InotifyRecord.toBytes`), and answers `EINVAL` if the oldest does not fit; with none queued it answers `EAGAIN` through a non-blocking description and otherwise sleeps.
`poll(2)` and epoll report it readable while it holds a record, and `ioctl(FIONREAD)` (`UnixDescriptor.bytesAvailable`) reports the bytes it holds.
Each user's instances and watches count against the machine's `InotifyLimits`, in every process.
A change to the filesystem, made by any process, queues a record on every instance watching what it changed whose watch asks for any of its events, carrying all of them, in Linux's order:
a name created, removed or renamed is `IN_CREATE`, `IN_DELETE`, or `IN_MOVED_FROM` and `IN_MOVED_TO` sharing a cookie, at the watch on its directory, with `IN_ISDIR` for a directory;
an inode's gained or lost link (a hard link, an unlink, a rename over it) is `IN_ATTRIB` at its own watch;
and `chmod`, `chown`, `utimensat` or an `open` with `O_TRUNC` through a path is one record, `IN_ATTRIB` or `IN_MODIFY` as Linux chooses, at the watch on the directory of the name the path reached and then at the inode's own.
A change made through a descriptor (`write`, `pwrite`, `copy_file_range`, `ftruncate`, `fchmod`, `fchown`, `futimens`, or the empty path under `AT_EMPTY_PATH`) is reported the same way, under the name the descriptor was opened through, which each open file description records:
a rename of that name moves it, and an unlink of it, a `rmdir` of it or a rename over it leaves it in place, unlinked, and the change is still reported there, except that a watch with `IN_EXCL_UNLINK`, on the directory or on the inode, hears nothing of a write through an unlinked name (it does hear its `ftruncate`, `fchmod` and the rest, and the `IN_ATTRIB` of the set-ID bits a write clears).
A file unlinked while open holds the directory it was named in, as Linux's does, so that directory outlives its own `rmdir` until the file is closed.
A record repeating the newest queued is dropped, and a full queue takes one `IN_Q_OVERFLOW` record; `inotify_rm_watch` queues `IN_IGNORED`.
A rename is `IN_MOVE_SELF` at the moved inode's own watch, after the other records of the rename.
A watched inode goes, for inotify, when a name of it goes while it has no link left, as Linux's dentry does, rather than when the inode itself does:
an unlink, `rmdir` or rename over a name no description was opened through, or the close of the last description opened through a name since unlinked (a name linked again is another name).
Its watches then queue `IN_DELETE_SELF` if they ask for it, and `IN_IGNORED`, and end, before the `IN_DELETE` of the name at its directory; afterwards `inotify_rm_watch` of one answers `EINVAL`.
So a file opened through one name and unlinked through another goes at that unlink, though the descriptor still reads it; a directory, having one name, goes when nothing holds it, a process standing in it included.
One change is reported by Linux through a name this kernel does not keep, and throws `UnmodelledException`, naming that name, wherever a watch could hear of it; where none could, it goes through:
a change to a directory `rmdir` removed, reached by "." (as the current directory), or through a descriptor opened that way, which Linux reports at its old parent under its old name, is refused under a watch on that parent asking for an event it raises.
A watch asking for an event this kernel does not raise, `IN_ACCESS`, `IN_OPEN`, `IN_CLOSE_WRITE`, `IN_CLOSE_NOWRITE` or `IN_UNMOUNT`, or carrying `IN_ONESHOT`, is refused.
Darwin has no inotify, and every call throws `PlatformNotSupportedException`.

| Call | Function | Answers |
| --- | --- | --- |
| `inotify_init1(2)` | `UnixInotify.inotifyInit1 (flags : int)` | `Result<int * UnixSystem, UnixError>` |
| `inotify_add_watch(2)` | `UnixInotify.inotifyAddWatch (fd : int) (path : PathArgumentBytes) (mask : uint32)` | `SyscallAnswer * UnixSystem` |
| `inotify_rm_watch(2)` | `UnixInotify.inotifyRmWatch (fd : int) (wd : int)` | `SyscallAnswer * UnixSystem` |

#### Signals

| Call | Function | Answers |
| --- | --- | --- |
| `kill(2)` | `UnixSignal.kill (pid : int) (signo : int)` | `Result<KillOutcome, UnixError>` |
| `pthread_kill(3)`, `raise(3)` | `UnixSignal.pthreadKill (target : 'Task) (signo : int)` | `Result<KillOutcome, UnixError>` |
| `sigaction(3)` | `UnixSignal.sigaction (signo : int) (newAction : SignalDisposition option)` | `Result<SignalDisposition * UnixSystem, UnixError>` |
| `sigaction(2)`: Linux's `rt_sigaction` | `UnixSignal.sigactionSyscall (signo : int) (newAction : SignalDisposition option)` | `Result<SignalDisposition * UnixSystem, UnixError>` |
| `pthread_sigmask(3)` | `UnixSignal.pthreadSigmask (task : 'Task) (how : int) (set : SignalMask option)` | `Result<SignalMask * UnixSystem, UnixError>` |
| `sigprocmask(3)` on Linux, `sigprocmask(2)` on Darwin | `UnixSignal.sigprocmask (task : 'Task) (how : int) (set : SignalMask option)` | `Result<SignalMask * UnixSystem, UnixError>` |
| Linux's `rt_sigprocmask(2)` | `UnixSignal.rtSigprocmask (task : 'Task) (how : int) (set : SignalMask option) (sigsetSize : uint64)` | `Result<SignalMask * UnixSystem, UnixError>` |
| `sigpending(2)` | `UnixSignal.sigpending (task : 'Task)` | `SignalMask` |
| `sigsuspend(2)` | `UnixSignal.sigsuspend (task : 'Task) (mask : SignalMask)` | `SigsuspendOutcome * UnixSystem` |
| Linux's `rt_sigsuspend(2)` | `UnixSignal.rtSigsuspend (task : 'Task) (mask : SignalMask) (sigsetSize : uint64)` | `SigsuspendOutcome * UnixSystem` |
| `pause(2)` | `UnixSignal.pause (task : 'Task)` | `SigsuspendOutcome * UnixSystem` |
| the return to user mode | `UnixSignal.onReturnToUser (task : 'Task)` | `ReturnToUserOutcome` |
| `sigreturn(2)` | `UnixSignal.sigreturn (task : 'Task) (frame : HandlerFrameId)` | `UnixSystem` |

A signal mask crosses the API as a `SignalMask`, the bits of a `sigset_t` under one numbering (`SignalMask.ofWord`, `SignalMask.toWord`, `SignalMask.ofSignals`), because Darwin keeps a bit that names no signal.
A signal number is raw, under the process's own numbering: `Signal.toRawSignoUnder` and `Signal.ofRawSignoUnder` convert, with the numbering from `SimulatedUnixPlatform.signalNumbering`.
`how` is raw too: `SIG_BLOCK`, `SIG_UNBLOCK` and `SIG_SETMASK` are 0, 1 and 2 on Linux, and 1, 2 and 3 on Darwin.

#### Time, entropy, identity and threads

| Call | Function | Answers |
| --- | --- | --- |
| `clock_gettime(2)` | `UnixClock.clockGettime (clockId : int)` | `Result<UnixTimestamp, UnixError>` |
| `gettimeofday(2)` | `UnixClock.gettimeofday` | `UnixTimestamp` |
| Linux's `getrandom(2)` | `UnixEntropy.getRandom (task : 'Task) (buffer : UserBuffer) (count : uint64) (flags : uint32)` | `GetRandomAnswer * UnixSystem` |
| Darwin's `getentropy(2)` | `UnixEntropy.getEntropy (buffer : UserBuffer) (length : uint64)` | `GetEntropyAnswer * UnixSystem` |
| `getpid(2)` | `UnixSystem.processId` | `ProcessId` |
| `umask(2)` | `UnixSystem.umask (mask : int)` | `PermissionBits * UnixSystem` |
| `getresuid(2)` | `UnixCredentials.getresuid (real : UserBuffer) (effective : UserBuffer) (saved : UserBuffer)` | `GetIdsAnswer<UserId>` |
| `getresgid(2)` | `UnixCredentials.getresgid (real : UserBuffer) (effective : UserBuffer) (saved : UserBuffer)` | `GetIdsAnswer<GroupId>` |
| `setresuid(2)` | `UnixCredentials.setresuid (real : UserId option) (effective : UserId option) (saved : UserId option)` | `SyscallAnswer * UnixSystem` |
| `setresgid(2)` | `UnixCredentials.setresgid (real : GroupId option) (effective : GroupId option) (saved : GroupId option)` | `SyscallAnswer * UnixSystem` |
| `setgroups(2)` | `UnixCredentials.setgroups (size : int) (words : GroupListWords)` | `SyscallAnswer * UnixSystem` |
| `clone(2)` with `CLONE_THREAD`, as `pthread_create(3)` makes it | `UnixTaskLifecycle.spawn (parent : 'Task) (child : 'Task) (cpu : CpuId)` | `SpawnAnswer * UnixSystem` |
| Linux's thread `exit(2)`, as `pthread_exit(3)` makes it | `UnixTaskLifecycle.exitThread (task : 'Task) (status : int)` | `TaskOutcome` |
| `exit_group(2)`, as `exit(3)` and `_exit(2)` make it | `UnixTaskLifecycle.exitGroup (task : 'Task) (status : int)` | `EndedProcess` |
| the client's report that a task runs on a processor | `UnixScheduling.dispatch (task : 'Task) (cpu : CpuId)` | `UnixSystem` |
| where a task is running | `UnixScheduling.runningOn (task : 'Task)` | `CpuId option` |
| Linux's `getcpu(2)` | `UnixScheduling.getcpu (task : 'Task)` | `GetCpuAnswer` |

`UnixEntropy.getRandomMaxTransfer` is the most one `getrandom` moves.
`gettid(2)` is `UnixTaskState.osThreadId` of the task, and `getuid(2)` and `getgid(2)` are fields of `UnixSystem.credentials` (`Credentials.RealUser`, `Credentials.RealGroup`).
`uname(2)` reports the platform: `SimulatedUnixPlatform.unixRelease` and `SimulatedUnixPlatform.architecture` of `UnixSystem.platform`.

#### What is not modelled

These calls have no function, and a client should refuse them rather than guess: `getppid`, `nanosleep` and `clock_nanosleep` (a client can sleep a task as a `poll` of nothing with a timeout), `select`, `pselect` and `ppoll`, `readv` and `writev`, `sendto`, `recvfrom`, `sendmsg` and `recvmsg`, `socketpair`, `truncate`, `fsync` and `fdatasync`, `getrlimit` and `setrlimit`, `sched_yield`, `futex`, `mmap` and its kin, `fork`, `exec` and `wait`, `sigaltstack`, and the timers (`alarm`, `setitimer`, `timer_create`).
"What the kernel leaves to the client" says which of those are the client's by design.

#### Calls that copy in from the caller's memory

A real kernel copies an argument in from the caller's memory at a point in the call, and what happens before that point is observable: a bad descriptor is `EBADF` whatever the buffer holds.
So each call that copies bytes in comes as two functions.
The first, an admission, answers either the call's answer, reached before the copy, or how many bytes to copy; the second takes exactly those bytes.
`UnixReadWrite.admitPWrite`, `UnixSocket.admitSockaddrCopy`, `UnixSocket.admitSetSockOpt` and `UnixSocket.admitGetSockOpt` change nothing, and answer no system.
`UnixReadWrite.admitWrite` and `UnixReadWrite.admitSend` answer a `WriteOutcome`, which carries the system the admission left: a blocking write with no room has parked the task, a write into a broken pipe has raised `SIGPIPE`, and on Darwin a write can move a pipe's timestamps without taking a byte. The client carries on from that system, as from any other call's (see "Moving bytes").

Calls that copy in two pathnames come as phases too, for a client that reads its arguments out of a live process, where reading the second pathname too early is itself observable: `UnixNamespace.renameatSourcePhase` or `UnixNamespace.renameSourcePhase` then `UnixNamespace.renameWithDestination`; `UnixNamespace.linkatSourcePhase` or `UnixNamespace.linkSourcePhase` then `UnixNamespace.linkWithDestination`; `UnixNamespace.symlinkatTargetPhase` or `UnixNamespace.symlinkTargetPhase` then `UnixNamespace.symlinkWithPath`; `UnixNamespace.cloneFileFlagsPhase`, `UnixNamespace.cloneFileSourcePhase` then `UnixNamespace.cloneFileWithDestination`; `UnixPathResolution.faccessatScreenPhase` or `UnixPathResolution.accessScreenPhase` then `UnixPathResolution.accessWithPath`; and `UnixInotify.inotifyAddWatchScreenPhase` then `UnixInotify.inotifyAddWatchWithPath`, since Linux answers a bad mask or descriptor without reading the path.
A client that holds every argument already, such as a replayer, uses the one-shot functions in the tables.

#### One entry point

`UnixSystem.step (task : 'Task) (call : Syscall)` reaches some of the syscalls through one function, as cases of the `Syscall` type, for a client that wants to log, replay or generate them; it answers `SyscallOutcome * UnixSystem`.
Only the calls with a `Syscall` case are reachable through it, from `dup` and `fcntl` to `setgroups`, `inotify_init1` and `inotify_rm_watch`: each answers a `SyscallOutcome`, which is one integer, an errno, or a sleep.
Many calls are not there, `open`, `socket`, `bind`, `connect`, `kill` and `sigaction` among them, and are reached only through their own functions.

### Checking the tables

`UnixSystem.checkInvariants` lists every way a system's tables disagree with each other.
No sequence of syscalls should ever produce one: a defect is a bug in this library.
Each kind of defect has a `describe`, such as `UnixSystemDefect.describe`, which names the rule that does not hold and the values that break it, for a bug report; a client cannot match a defect's cases (see "Unions that gain cases").
It is two halves: the rules about the machine, which read facts every process on it contributes to, and `UnixSystem.checkViewInvariants`, the rules about one process's view of it.
A process alone on its machine is held to both in full. Of a view a `SimulatedMachine` focused, only the machine's rules that one process can check truthfully are run; `SimulatedMachine.checkInvariants` runs the rest, against every process at once.
Each table's own rules are checked apart from these: `FileDescriptorRegistry.checkInvariants` of `UnixSystem.fileDescriptors` for the descriptor table and the open file descriptions it names, and `VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes system) (UnixSystem.fileSystem system)` for the filesystem.
A fact a syscall needs about other processes is kept on the machine's object rather than derived from the processes: each open file description counts the descriptors naming it and the holds of calls in flight on it, the thread ID allocator records which IDs live tasks hold, a kqueue records the process that owns it, whose descriptor numbers its registrations name, and each kqueue registration records the socket it is attached to.

Every descriptor lies below `SimulatedUnixPlatform.descriptorBound`, the soft `RLIMIT_NOFILE` a process of the flavour starts with (1024 on Linux, 256 on Darwin): the library assumes the process's limit is at least that, and refuses a call that would put a descriptor at or above it, whose answer would depend on the limit.

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
| `ParkedSyscall.PipeRead`, `ParkedSyscall.ConnectionRead`, `ParkedSyscall.InotifyRead` (a `read` or `recv`) | `UnixReadWrite.finishRead (task : 'Task)` |
| `ParkedSyscall.PipeWrite`, `ParkedSyscall.ConnectionWrite` (a `write` or `send`) | `UnixReadWrite.admitFinishWrite (task : 'Task)`, then `UnixReadWrite.finishWrite (task : 'Task) (bytes : ImmutableArray<byte>)` |
| `ParkedSyscall.SigSuspend` (a `sigsuspend`, `rt_sigsuspend` or `pause`) | `UnixSignal.finishSigsuspend (task : 'Task)` |

Each answers what the call that parked answered, as in the tables above.

`ParkedSyscall.Sleep` (a `nanosleep` or `clock_nanosleep`, carrying a `ParkedSleep`), `ParkedSyscall.FifoOpen` (a blocking `open` of one end of a FIFO, carrying a `ParkedFifoOpen`), `ParkedSyscall.Select` (a `select` or `pselect`, carrying a `ParkedSelect`, whose representation is internal) and `ParkedSyscall.SignalWait` (a `sigwait`, `sigwaitinfo` or `sigtimedwait`, carrying a `ParkedSignalWait`) are reserved: nothing parks a task in any of them yet, and none has a finishing function, so `UnixSystem.checkInvariants` reports a task parked in one as a defect.
So are the primitives a FIFO open and a signal wait wait for, `WakePrimitive.FifoPartnerOpened` and `WakePrimitive.AwaitedSignalPending`, which no condition contains yet.
The cases exist so that a client's matches already handle them when sleeps, FIFOs, `select` and signal waits arrive.

A call that waits with a timeout (`poll`, `epoll_wait`, `kevent`) wakes once its deadline passes.
With nothing runnable, the client asks `UnixWait.deadlines asleep`, which answers each deadline as an instant, in nanoseconds since boot.
The earliest is when the first of those calls times out: the client advances the clock by it less `UnixSystem.nanosecondsSinceBoot`, and asks `UnixWait.wakes` again.

A parked call holds the open file descriptions it waits on (`ParkedSyscall.descriptions`), as a real one holds a reference to each file: a description goes when no descriptor names it and no call holds it, so one closed under a sleeping call goes when the call returns.

This client makes a pipe, parks a second task on reading it, wakes it with a write, and ends the process:

```fsharp
open System.Collections.Immutable
open WoofWare.PosixKernel

let platform = SimulatedUnixPlatform.linuxX64

let booted : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

    UnixSystem.initial platform |> UnixBootImage.boot launch

// Every call ends with the calling task's return to user mode, where it takes
// its signals. This client installs no handler, so it expects none to run.
let resume (task : int) (system : UnixSystem<int, unit>) : UnixSystem<int, unit> =
    match UnixSignal.onReturnToUser task system with
    | ReturnToUserOutcome.Resumes system -> system
    | other -> failwith $"unexpected at the return to user mode: %A{other}"

// pipe2(fds, 0) by task 0. UserBuffer.Mapped: the array for the two descriptors is writable.
let readFd, writeFd, piped =
    match UnixPipe.pipe2 0 UserBuffer.Mapped booted with
    | Pipe2Answer.Created (readFd, writeFd), system -> readFd, writeFd, resume 0 system
    | answer, _ -> failwith $"unexpected from pipe2: %A{answer}"

// Task 0 starts a thread, which this client names 1, on processor 0.
let spawned =
    match UnixTaskLifecycle.spawn 0 1 (CpuId 0) piped with
    | SpawnAnswer.Spawned _, system -> resume 0 system
    | answer, _ -> failwith $"unexpected from spawn: %A{answer}"

// Task 1 reads the empty pipe, and sleeps. The client holds it asleep.
let asleep, parked =
    match UnixReadWrite.read 1 readFd UserBuffer.Mapped 64UL spawned with
    | ReadOutcome.WouldBlock _, system -> Set.singleton 1, system
    | outcome, _ -> failwith $"expected the read to sleep: %A{outcome}"

// Task 0 writes "hello": admitWrite says how many bytes the call takes from
// the caller's buffer, and write takes them.
let written =
    let payload = "hello"B

    match UnixReadWrite.admitWrite 0 writeFd UserBuffer.Mapped (uint64 payload.Length) parked with
    | WriteOutcome.Returns (WriteAdmission.Transfer count, system) ->
        match UnixReadWrite.write 0 writeFd (ImmutableArray.Create (payload, 0, count)) system with
        | WriteOutcome.Returns (WriteAnswer.Completed n, system) ->
            printfn "task 0 wrote %d bytes" n
            resume 0 system
        | outcome -> failwith $"unexpected write: %A{outcome}"
    | outcome -> failwith $"unexpected admission: %A{outcome}"

// The write woke task 1, and finishRead finishes its call.
let woken =
    match UnixWait.wakes asleep written with
    | [ 1, _ ] ->
        match UnixReadWrite.finishRead 1 written with
        | ReadOutcome.Answered (ReadAnswer.Completed bytes), system ->
            printfn "task 1 read %s" (System.Text.Encoding.ASCII.GetString (bytes.AsSpan ()))
            resume 1 system
        | outcome, _ -> failwith $"unexpected read: %A{outcome}"
    | wakes -> failwith $"expected task 1 to wake: %A{wakes}"

// Task 1 exits, and then task 0 calls exit_group(3), which ends the process.
let ended =
    match UnixTaskLifecycle.exitThread 1 0 woken with
    | TaskOutcome.Continues system -> UnixTaskLifecycle.exitGroup 0 3 system
    | other -> failwith $"task 0 is still running, so expected the process to continue: %A{other}"

// The client ends the process on its machine, which closes its descriptors.
let termination, _ =
    SimulatedMachine.ofSystem (EndedProcess.endedIn ended) |> SimulatedMachine.endProcess ended

printfn "the process ended: shell status %d" (ProcessTermination.shellStatus (SimulatedUnixPlatform.signalNumbering platform) termination)

// Prints:
// task 0 wrote 5 bytes
// task 1 read hello
// the process ended: shell status 3
```

## Signals

`UnixSignal.kill` and `UnixSignal.pthreadKill` generate a signal, and answer a `KillOutcome`: the process carries on (`KillOutcome.ProcessContinues`), with the signal pending or discarded; the signal stopped it (`KillOutcome.ProcessStopped`); or the signal killed it at once (`KillOutcome.ProcessEnded`).
`kill` of another process, or of a process group, is refused.
A signal the process ignores, and a `SIGCONT` at its default, which has nothing to resume, are discarded as they are sent, unless the task they are aimed at blocks them: for a signal sent to the process, that is the leader under Linux and every task under Darwin. So such a signal never wakes a sleeping call.
One sent while blocked stays pending, and is discarded as a task takes it.
Under Linux, `kill` of such a signal that the leader blocks while another task does not is refused, as a caught one is: Linux would queue it for that other task, and this library delivers a process's signals to its leader only.
`UnixSignal.sigaction` installs a `SignalDisposition`: `SignalDisposition.Default`, `SignalDisposition.Ignore`, or `SignalDisposition.Catch` of a `SignalCatch`, made with `SignalCatch.ofHandler`, which holds the client's `'Handler`.

`UnixSignal.sigprocmask` changes every task's mask on Darwin, as Darwin's does, and refuses a call that would unblock a signal pending for a task other than the caller: Darwin was measured neither to wake that task for it nor always to deliver it as the task next returns to user mode, both of which this library would do.
`UnixSignal.sigsuspend` replaces a task's mask until a signal ends the call, and keeps the mask it replaced in the signal state (`SignalState.maskToRestore`) rather than in the park, because the call's answer comes before the task's return to user mode, which is what restores it: the first handler frame that return pushes saves the mask from before the call.

### The return to user mode

A signal is taken as a task returns to user mode, so after each call a task makes, and before it runs any of its own code, the client asks `UnixSignal.onReturnToUser task`.
It answers a `ReturnToUserOutcome`:

* `ReturnToUserOutcome.Resumes system`: the task took nothing the client acts on, and runs its own code next.
* `ReturnToUserOutcome.RunHandlers (frames, system)`: the frames the kernel pushed, innermost first, each with the signal, the disposition and the mask to restore. The client runs each handler, and when one returns it calls `UnixSignal.sigreturn` with that frame's `HandlerFrame.Id` and asks `onReturnToUser` again.
* `ReturnToUserOutcome.ProcessEnded ended`: a signal whose default action terminates the process killed it (see below).
* `ReturnToUserOutcome.ProcessStopped (signal, system)`: a signal whose default action stops the process stopped it (see below).
* `ReturnToUserOutcome.ContinueDiscarded (signal, system)`: the kernel discarded a `SIGCONT` at its default, pending while the task blocked it, as the task took it, and the client asks `onReturnToUser` again, since the return goes on, under a `sigsuspend`'s temporary mask if the task is returning from one.

A signal whose default terminates the process ends it where the library applies the default: at generation, when some task can take it (`KillOutcome.ProcessEnded`, and `WriteOutcome.ProcessEnded` for a write's `SIGPIPE`), and otherwise when a task that blocked it can take it, as the task returns to user mode (`ReturnToUserOutcome.ProcessEnded`): after a mask call or a `sigreturn` that unblocks it, or from a `sigsuspend` whose temporary mask lets it through.
Either answer is the `EndedProcess`, killed by the signal (`ProcessTermination.Signaled`, with the core flag), and no handler runs, not even one whose frame was pushed at the same return; the client ends the process on its machine with `SimulatedMachine.endProcess`, as for an exit.

A signal whose default stops the process is reported (`KillOutcome.ProcessStopped`, `ReturnToUserOutcome.ProcessStopped`) for the client to act on.
The library holds no stopped state, and nothing here continues a stopped process, so a client should refuse a stop rather than carry on as if the process were still running.

This client blocks `SIGTERM`, sends it to itself, and dies of it as it unblocks it:

```fsharp
open WoofWare.PosixKernel

let platform = SimulatedUnixPlatform.linuxX64
let numbering = SimulatedUnixPlatform.signalNumbering platform

let booted : UnixSystem<int, unit> =
    let launch =
        ProcessLaunch.create platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

    UnixSystem.initial platform |> UnixBootImage.boot launch

let returnToUser (system : UnixSystem<int, unit>) : ReturnToUserOutcome<int, unit> =
    UnixSignal.onReturnToUser 0 system

let resumed (outcome : ReturnToUserOutcome<int, unit>) : UnixSystem<int, unit> =
    match outcome with
    | ReturnToUserOutcome.Resumes system -> system
    | other -> failwith $"unexpected at the return to user mode: %A{other}"

// pthread_sigmask(how, {SIGTERM}, NULL), with `how` under Linux's numbering:
// SIG_BLOCK is 0 and SIG_UNBLOCK 1. Its Error is an errno.
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

    match UnixSignal.kill pid signo blocked with
    | Ok (KillOutcome.ProcessContinues system) -> system |> returnToUser |> resumed
    | Ok outcome -> failwith $"unexpected kill: %A{outcome}"
    | Error errno -> failwith $"kill failed: %O{errno}"

// Unblocking it: as task 0 returns to user mode it takes SIGTERM, whose
// default action ends the process.
match pending |> mask 1 |> returnToUser with
| ReturnToUserOutcome.ProcessEnded ended ->
    let termination, _ =
        SimulatedMachine.ofSystem (EndedProcess.endedIn ended) |> SimulatedMachine.endProcess ended

    printfn "%A" termination
    printfn "shell status %d" (ProcessTermination.shellStatus numbering termination)
| other -> failwith $"expected SIGTERM to end the process: %A{other}"

// Prints:
// Signaled (SIGTERM, false)
// shell status 143
```

## Threads, and the end of the process

`UnixTaskLifecycle.spawn parent child cpu` starts a task the client names `child`, on processor `cpu`, with a copy of `parent`'s signal mask, and answers its thread ID, an `OsThreadId` (`SpawnAnswer.Spawned`).
`UnixTaskLifecycle.exitThread task status` ends one task, and answers `TaskOutcome.Continues` with the process carrying on, or `TaskOutcome.ProcessEnded` if it was the last.
It throws `InvalidOperationException` for a task parked in a syscall, and refuses the process's leader while any other task lives and, on Darwin, the process's last task: a Darwin client ends a process with `exitGroup`, never by its last thread exiting.
`UnixTaskLifecycle.exitGroup task status` ends every task at once, parked ones included.

A call that ends the process answers an `EndedProcess` instead of a system: `exitGroup`, an `exitThread` of the last task, a `kill`, `pthreadKill` or write whose signal killed it, and a return to user mode that takes such a signal.
`EndedProcess.termination` is how the process ended (`ProcessTermination.Exited` or `ProcessTermination.Signaled`), which is what its parent's `wait` reads; `ProcessTermination.waitpidStatus` encodes it as `waitpid(2)` would.
`EndedProcess.processId` is which process it was, and `EndedProcess.endedIn` the view it ended in.
Only such a call makes one, and `SimulatedMachine.endProcess` takes nothing else.

`SimulatedMachine.endProcess ended machine` ends the process on the machine: it releases what only the process's calls held, then closes every descriptor in the order each kernel measurably does (Linux drops them lowest first and releases the last let go of first; Darwin closes them highest first), as `close` closes each: sending its peers their FINs, or resets where it left bytes unread, resetting the clients left in its listeners' accept queues, whichever process holds them, and letting its locks, pipes, listeners and event queues go, and removes the process.
It refuses where a close would: so far, the last close of a connected socket whose `SO_LINGER` would make it reset the connection or wait.
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
A client that asks after every step, as one that interleaves instructions does, can ask `SimulatedMachine.wakesRemembering memo asleep` instead, starting from `WakeSweepMemo.empty` and passing each call the memo the last one answered.
It answers what `wakes` answers, and asks no wake condition when the last sweep woke nobody and nothing a condition reads has changed since but the clock, which has not reached any sleeping task's deadline.
Advancing the clock, and dispatching a task to the processor it is on, leave the memo answering; a syscall, and a dispatch that moves a task to another processor, make the next call sweep.
A kqueue registration, and a filter a sleeping Darwin `poll` registered, is attached to the socket its descriptor named, as XNU attaches a knote to the socket, so an event on the socket reaches it whichever process caused the event; a sleeping Darwin poll's kqueue (`PollQueue`) is the machine's for as long as the call sleeps.
A client reads that kqueue through `UnixSystem.pollQueue`, then `PollQueue.registered`, `PollQueue.entry` and `PollQueue.activated`.
A kqueue's own registrations, and what each is attached to, are not public (`KqueueState`'s representation is internal), so a later version can attach a registration to a pipe or a timer without a change of shape.
`KqueueFilter.Timer` is reserved: `kevent` refuses `EVFILT_TIMER`, and a poll registers only `KqueueFilter.Read` and `KqueueFilter.Write`, so nothing registers a timer yet.

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

**Where they meet.** `UnixSystem.tasks` lists the process's tasks, and `UnixTaskState.parkedIn` says which are parked in a syscall.
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
`CharacterDevice.Zero`, `CharacterDevice.Random` and `CharacterDevice.Tty` exist too, so that a client's matches already handle them when their drivers arrive, but nothing produces them yet: `CharacterDevice.all` does not list them, so no node names any of them.
On Darwin it is devfs, which is not modelled, so any path that reaches `/dev` is refused.

Some interfaces are one flavour's alone, and the other flavour refuses every call of them rather than answering: no kernel of that flavour has the call, so there is no answer to measure.
Linux has epoll and inotify, and Darwin kqueue.

WoofWare.PosixKernel is intended to be fully POSIX-compliant eventually.
A design goal of WoofWare.PosixKernel is that the library refuses rather than providing an answer which has not been measured on the platform it's been told to simulate.
It is fully deterministic, so e.g. it chooses a traversal order for directory listing even though that is POSIX-unspecified.
We try very hard to return only values that we have observed from a real system using that platform, rather than just copying semantics from the docs.

Where real platforms of a given flavour have been observed to disagree, we choose a permitted answer (generally an *inconvenient* one, since I want to help you avoid accidentally relying on unspecified behaviour).
For example, directory enumeration order has been observed to be extremely odd on Linux: we've even seen `..` and `.` appear at the *end* of the enumeration, in the GitHub Actions ext4 runner!

## Changing its API

The library's public API is recorded in `SurfaceBaseline.txt`, beside its project file, and a test in `WoofWare.PosixKernel.Test` fails whenever the compiled API differs from it.
After an intended change to the API, regenerate the file and commit it with the change:

```sh
nix develop -c dotnet test WoofWare.PosixKernel.Test/WoofWare.PosixKernel.Test.fsproj --filter "FullyQualifiedName~TestSurface&Name~Update"
```

(The NUnit adapter cannot parse a filter value containing spaces, so the filter cannot give the test's full name, which has them.)
A conflict in `SurfaceBaseline.txt` after a rebase is resolved by regenerating the file, never by hand.

The file also records the order of each union's cases: F# numbers the cases of a union of more than one case in a nested `Tags` class, and the file gives each tag's value, as in `WoofWare.PosixKernel.AbsoluteUnixPathError+Tags.ContainsNul [static field]: int = 2`.
A second test fails if a union has lost or reordered cases, or has gained a case anywhere but at its end, since the version the file records.
Code built against the recorded version would then misread the cases it already knew, which an appended case does not make it do.
From 1.0 an appended case is a major version too (see "Unions that gain cases" above), and the first test already fails on one.
It also fails if a union whose cases the file records as public no longer has public cases, which is expected when a union becomes internal, or is deleted or renamed.
Before 1.0 such a change may still be made deliberately, by the same command.

A third test fails if a public record that the file does not list, or a field that a listed record has gained, uses a field label that another listed record already has.
F# resolves a label that several records share to the most recently declared of them, in a record pattern, in a `x.Label` access on a value whose type is not yet inferred, and in a record expression whose labels do not single out one record.
So a reused label can change which record a client's existing code means, and that code can stop compiling.
Labels that listed records already share, such as `Address` in `Ipv4Endpoint` and `Ipv6Endpoint`, are allowed.
Before 1.0 the fix is to rename the field; regenerating the file instead allows the collision from then on, and is a deliberate choice.

## Licence

MIT.
