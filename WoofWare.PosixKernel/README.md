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
* signals: sending them, their dispositions, delivery to a handler, and a process ended by one
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
(Clients translate their own foreign-function layer into requests against the state machine, and WoofWare.PosixKernel doesn't call back into the client.)

It speaks POSIX alone, in each flavour's own numbering wherever a number is involved: an `<errno.h>` error, a signal, an `open(2)` flag word.
Converting those to and from a client's own encoding is the client's business.

### Slop status

100% vibe-coded, by the hand of Claude Opus 4.6 through 5, Claude Fable 5, and GPT-5.5 through 5.6 Sol.

## Using it

The whole kernel is one value, a `UnixSystem<'Task, 'Handler>`, made of three parts:

* the machine (`UnixMachineState`): the filesystem, the open file descriptions (with the state of each epoll instance and kqueue, and how many descriptors and calls in flight reference each), sockets, connections and pipes, the thread IDs live tasks hold, the processes on it and the directories they stand in, the clock, the entropy pool, and the platform being simulated;
* the process (`UnixProcessState`): the descriptor table, which says only which open file description each descriptor names, and the process's credentials, umask, current directory, environment and signal state;
* the tasks (`UnixTaskState`): the process's tasks, and what each is blocked in, if anything.

Those records are opaque outside the library.
A client reads a system through `UnixSystem`'s queries, such as `leader`, `tasks`, `signals`, `fileDescriptors`, `openFiles`, `delivered` and `descriptorTarget`, and changes a running one only through the syscalls and the two operations of the outside world described below.

`'Task` is whatever the client calls a thread, and `'Handler` whatever it calls a signal handler.
The library never looks inside either; it only compares them.

`UnixSystem.initial` builds the *boot image* of a machine that has not done anything yet: a `UnixBootImage`, which no syscall takes.
Configure it with the setters in the `UnixBootImage` module, such as `withFileSystem`, `withBootTime` and `withProcessId` (the first process's ID, where the machine's counters start).
How a process starts is a `ProcessLaunch`: `ProcessLaunch.create` takes the descriptors it is launched with and its first task, and the setters in the `ProcessLaunch` module set the rest, such as `withCredentials`, `withCurrentDirectory` and `withEnvironment`.
`UnixBootImage.boot` launches the machine's first process from one, to get the `UnixSystem` its first syscall takes.
A setter that rejects a value, such as `withBootTime` or `withMount`, returns `Result`, with a refusal type of its own whose `describe` says why (see "Answers and refusals" below); only the caller knows what it called the value.
Since no setter takes a booted system, configuration can only describe the machine from the moment it booted.
What changes while it runs is a syscall's effect, or the outside world acting on it: `UnixSystem.advanceClock` (time passes) and `UnixSystem.writePidMaxSysctl` (the administrator writes `kernel.pid_max`).

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
    | Error refusal -> failwith $"this kernel will not say what happens: %O{refusal}"
    | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed result), system) ->
        printfn "mkdir returned %d" result
        system
    | Ok (SyscallOutcome.Answered (SyscallAnswer.Failed error), system) ->
        let numbering = SimulatedUnixPlatform.rawErrnoNumbering (UnixSystem.platform system)
        printfn "mkdir failed with errno %d" (UnixError.toRawErrnoUnder numbering error)
        system
    | Ok (SyscallOutcome.WouldBlock _, _)
    | Ok (SyscallOutcome.Restarts, _) -> failwith "mkdir never sleeps"

// Prints "mkdir returned 0", then "mkdir failed with errno 17" (EEXIST).
system |> mkdir |> mkdir |> ignore
```

### Several processes

A `SimulatedMachine` holds several processes on one machine. `SimulatedMachine.ofSystem` makes one of a booted system, and `SimulatedMachine.launch` starts another process on it from a `ProcessLaunch`, in a directory of the machine's filesystem; the kernel chooses its process ID, as Linux does from its thread ID counter and Darwin from a process ID counter of its own.

A syscall is still made in a `UnixSystem`: `SimulatedMachine.focus` gives one process's view of the machine, which holds that process and no other, so no call can read or change another process's own state, and `SimulatedMachine.unfocus` writes the view back. `inView` and `step` do both around one call. A view records which state of the machine it was taken from, and `unfocus` refuses a view taken before some other write-back, whose copy of the machine is stale. A client that keeps the view of a process making no calls while others' views are written back can ask `SimulatedMachine.holdsProcessOf` whether only the machine in it has gone stale, so that focusing the process again loses nothing.

Everything one process's call does to another goes through the machine they share: ports, connections, pipes, files and their locks, the open file descriptions, the directories processes stand in, and the thread and process IDs. `SimulatedMachine.checkInvariants` holds every process to the machine and to each other; `UnixSystem.checkInvariants` and `FileDescriptorRegistry.checkInvariants` of one view check only what one process can see truthfully.

A call asleep in one process wakes for what another process's call does: a connection queued on its listener, a peer's FIN, a lock let go of. `SimulatedMachine.wakes` is `UnixWait.wakes` of every process at once: each sleeping task's condition is asked of its own process's view, and where a kernel wakes one waiter of a queue at a time, the one is chosen across every process by the machine's park order. A woken call is finished in its own process's view. A kqueue registration, and a filter a sleeping Darwin `poll` registered, is attached to the socket its descriptor named, as XNU attaches a knote to the socket, so an event on the socket reaches it whichever process caused the event; a sleeping Darwin poll's kqueue (`PollQueue`) is the machine's for as long as the call sleeps.

`SimulatedMachine.endProcess` ends a process on the machine, as the call that ended it (`exit_group`, the last thread's exit, a signal) answered it: it releases what only the process's calls held, then closes every descriptor in the order each kernel measurably does (Linux drops them lowest first and releases the last let go of first; Darwin closes them highest first), sending its peers their FINs and letting its locks, pipes, listeners and event queues go, and removes the process. It refuses where a close would: a listener holding a connection another process's open socket made is not released, since what the reset does to that socket is not measured.

Nothing passes a descriptor from one process to another (no `fork`, no `SCM_RIGHTS`, and a launched pipe's other end is the client's), so no pipe, listener or epoll instance is shared, and `kill` of another process is refused.

### The syscalls

Each syscall is a function in the module for its family.
It takes its arguments as the kernel does, raw where the kernel validates them, and gives back its answer with the system as the call left it (or the answer alone, for a call that cannot change anything).

| Module | Syscalls |
| --- | --- |
| `UnixDescriptor` | `dup`, `dup2`, `dup3`, `fcntl` (`F_DUPFD`, `F_DUPFD_CLOEXEC`, `F_GETFD`, `F_SETFD`, `F_GETFL`, `F_SETFL`), `lseek`, `flock`, `ftruncate`, `posix_fadvise`, `close`, `ioctl` (`FICLONE` and `FIONREAD`), `tcgetattr`, `geteuid`, `getegid`, `getgroups` |
| `UnixPathResolution` | `stat`, `fstat`, `fstatat`, `chmod`, `fchmod`, `chown`, `lchown`, `fchown`, `futimens`, `statfs`, `fstatfs`, `getcwd`, `chdir`, `access`, `faccessat` |
| `UnixNamespace` | `open`, `openat`, `readlink`, `readlinkat`, reading a directory, `mkdir`, `mkdirat`, `unlink`, `rmdir`, `unlinkat`, `rename`, `renameat`, `clonefile`, `symlink`, `symlinkat`, `link`, `linkat` |
| `UnixReadWrite` | `read`, `pread`, `write`, `pwrite`, `copy_file_range` |
| `UnixPipe` | `pipe2` |
| `UnixSocket` | `socket`, `bind`, `listen`, `getsockname`, `setsockopt`, `getsockopt` |
| `UnixConnection` | `connect`, `accept` |
| `UnixPoll` | `poll`, `epoll_create1`, `epoll_ctl`, `epoll_wait` |
| `UnixKqueue` | `kqueue`, `kevent` |
| `UnixSignal` | `kill`, `pthread_kill`, `sigaction`, `sigreturn`, and the signals a task takes as it returns to user mode |
| `UnixClock` | `clock_gettime` |
| `UnixEntropy` | `getrandom`, `getentropy` |
| `UnixCredentials` | `getresuid`, `getresgid`, `setresuid`, `setresgid`, `setgroups` |
| `UnixTaskLifecycle` | starting a thread, a thread exiting, `exit_group` |
| `UnixSystem` | `getpid`, `umask` |

`UnixSystem.step` puts the syscalls whose answer is a single integer behind one entry point, as cases of the `Syscall` type, for a client that wants to log, replay or generate them.
A syscall whose answer carries more than that, such as the bytes `read` returns, has no `Syscall` case, and is reached only through its own function.

Every descriptor lies below `SimulatedUnixPlatform.descriptorBound`, the soft `RLIMIT_NOFILE` a process of the flavour starts with (1024 on Linux, 256 on Darwin): the library assumes the process's limit is at least that, and refuses a call that would put a descriptor at or above it (`DescriptorLimitRefusal`), whose answer would depend on the limit.

`UnixSystem.checkInvariants` lists every way a system's tables disagree with each other.
No sequence of syscalls should ever produce one.
It is two halves: `UnixSystem.checkMachineInvariants`, the rules about the machine, which takes every process on it, and `UnixSystem.checkViewInvariants`, those about one process's view of it.
A fact a syscall needs about other processes is kept on the machine's object rather than derived from the processes: each open file description counts the descriptors naming it and the holds of calls in flight on it, the thread ID allocator records which IDs live tasks hold, a kqueue records the process that owns it, whose descriptor numbers its registrations name, and each kqueue registration records the socket it is attached to.

### Answers and refusals

A syscall's result has two levels.

* `Ok` is what the kernel does: it answers, perhaps with an errno (a `UnixError`), or the calling task sleeps (see below). Either way it comes with the system after the call; a failing call can still change the system, just as a real one can.
* `Error` is a refusal: this library will not say what the kernel does, usually because nobody has measured it on the platform being simulated, or because it is not modelled. A refusal says why, and carries no system. A client decides what a refusal means for it; retrying will not help.

A call that a real kernel would not let happen at all, such as a task making a syscall while it is blocked in another, is a bug in the client, and throws.

A setter of the machine's boot configuration or of a process's launch refuses the same way.
Each one that can refuse a value returns a `Result`, with a refusal type of its own (`BootTimeRefusal`, `MountRefusal`, `ProcessIdRefusal`, `CredentialsRefusal` and so on) whose cases state the facts: a value this library has not measured or does not model, or one no machine of the flavour could have.
Each refusal type has a `describe`.
The library does not know what the client called the value, so it names no knob; the client does that.
A value no `parse` could have produced, such as one built with `Unchecked.defaultof`, is still a bug in the client, and throws.

### Blocking

A call that would block does not block.
It answers `WouldBlock` with a `WakeCondition`, and the system that comes with it records the calling task as parked in that call.
That is the state the kernel sleeps in, which can differ from the one the call arrived with: `flock` gives up the caller's old lock before it waits for the new one.

The library has no scheduler, and does not want one.
Waking is pulled rather than pushed: after each step, the client asks `UnixWait.wakes` which of the tasks it holds asleep may wake now, and with nothing runnable, `UnixWait.deadlines` says how far it may advance the clock.
A woken task finishes its call through the family's finishing function (`UnixDescriptor.flockAcquire`, `UnixPoll.finishPoll`, `UnixReadWrite.finishRead`, and so on), which may answer, park again, or say the call restarts because a signal handler interrupted it.

A parked call holds the open file descriptions it waits on (`ParkedSyscall.descriptions`), as a real one holds a reference to each file: a description goes when no descriptor names it and no call holds it, so one closed under a sleeping call goes when the call returns.

## Flavours and divergence from host platforms

WoofWare.PosixKernel's behaviour does not depend on the host platform; indeed, it probably works on Windows.
However, POSIX is extremely underspecified (and implementations frequently diverge from their documentation!),
and I only have easy access to a few flavours.

A `SimulatedUnixPlatform` names the kernel being simulated: its flavour (Linux or Darwin), its architecture, its page size and its release.
Only the combinations that have been measured can be built: Linux on x86-64 and on aarch64 with 4 KiB pages, and Darwin on arm64 with 16 KiB pages.
`SimulatedUnixPlatform.linuxX64`, `linuxArm64` and `macOsArm64` are the presets.
Where the flavours disagree, the platform says which answer applies, down to whose numbering an errno or a signal is reported in.
The filesystem type is chosen separately (currently tmpfs, APFS or NFS), from those its flavour has been seen to report.

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
