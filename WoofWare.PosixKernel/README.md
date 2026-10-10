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
(Clients translate their own foreign-function layer into requests against the state machine, and WoofWare.PosixKernel doesn't call back into the client.)

It speaks POSIX alone, in each flavour's own numbering wherever a number is involved: an `<errno.h>` error, a signal, an `open(2)` flag word.
Converting those to and from a client's own encoding is the client's business.

### Slop status

100% vibe-coded, by the hand of Claude Opus 4.6 through 5, Claude Fable 5, and GPT-5.5 through 5.6 Sol.

## Using it

The whole kernel is one value, a `UnixSystem<'Task, 'Handler>`, made of three parts:

* the machine (`UnixMachineState`): the filesystem, the open file descriptions (with the state of each epoll instance and kqueue, and how many descriptors and calls in flight reference each), sockets, connections and pipes, the thread IDs live tasks hold, the processes on it and the directories they stand in, the clock, the entropy pool, and the platform being simulated;
* the process (`UnixProcessState`): the descriptor table, which says only which open file description each descriptor names, and the process's credentials, umask, current directory, environment and signal state;
* the tasks (`UnixTaskState`): the process's tasks, and what each is blocked in, if anything. A client looks a task up in `UnixSystem.tasks` by its own name for it, which has no entry for a task that was never created or has exited, and reads what it finds with `UnixTaskState`'s readers.

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
A process's leader starts blocking no signal; a client launching one whose parent left signals blocked sets that mask with `UnixSignal.pthreadSigmask` before the leader's first instruction, as it installs inherited ignores with `UnixSignal.sigaction`.
A signal mask crosses the API as a `SignalMask`, the bits of a `sigset_t` under one numbering (`SignalMask.ofWord`, `SignalMask.toWord`), because Darwin keeps a bit that names no signal.
`UnixSignal.sigprocmask` changes every task's mask on Darwin, as Darwin's does, and refuses (`SigprocmaskRefusal`) a call that would unblock a signal pending for a task other than the caller: Darwin was measured neither to wake that task for it nor always to deliver it as the task next returns to user mode, both of which this library would do.
`UnixSignal.sigsuspend` replaces a task's mask until a signal ends the call, and keeps the mask it replaced in the signal state (`SignalState.maskToRestore`) rather than in the park, because the call's answer comes before the task's return to user mode, which is what restores it: the first handler frame that return pushes saves the mask from before the call.
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

A call asleep in one process wakes for what another process's call does: a connection queued on its listener, bytes arriving on its socket or room freed in its send buffer, a peer's FIN or reset, a lock let go of. `SimulatedMachine.wakes` is `UnixWait.wakes` of every process at once: each sleeping task's condition is asked of its own process's view, and where a kernel wakes one waiter of a queue at a time, the one is chosen across every process by the machine's park order. A woken call is finished in its own process's view. A kqueue registration, and a filter a sleeping Darwin `poll` registered, is attached to the socket its descriptor named, as XNU attaches a knote to the socket, so an event on the socket reaches it whichever process caused the event; a sleeping Darwin poll's kqueue (`PollQueue`) is the machine's for as long as the call sleeps.

`SimulatedMachine.endProcess` ends a process on the machine, as the call that ended it (`exit_group`, the last thread's exit, a signal) answered it: it releases what only the process's calls held, then closes every descriptor in the order each kernel measurably does (Linux drops them lowest first and releases the last let go of first; Darwin closes them highest first), as `close` closes each: sending its peers their FINs, or resets where it left bytes unread, and letting its locks, pipes, listeners and event queues go, and removes the process. It refuses where a close would: a listener holding a connection another process's open socket made is not released, since what the reset does to that socket is not measured.

A call that ends the process answers an `EndedProcess` instead of a system, and so does a task's return to user mode that applies a signal's default and terminates it (`UnixSignal.onReturnToUser`). `EndedProcess.termination` is how the process ended, which is what its parent's `wait` reads, and `EndedProcess.processId` is which process it was. Only such a call or return makes one, and `SimulatedMachine.endProcess` takes nothing else. A process alone on its machine is ended the same way, on the machine `SimulatedMachine.ofSystem (EndedProcess.endedIn ended)` makes of the view the process ended in.

Nothing passes a descriptor from one process to another (no `fork`, no `SCM_RIGHTS`, and a launched pipe's other end is the client's), so no pipe, listener or epoll instance is shared, and `kill` of another process is refused.

### The syscalls

Each syscall is a function in the module for its family.
It takes its arguments as the kernel does, raw where the kernel validates them, and gives back its answer with the system as the call left it (or the answer alone, for a call that cannot change anything).

| Module | Syscalls |
| --- | --- |
| `UnixDescriptor` | `dup`, `dup2`, `dup3`, `fcntl` (`F_DUPFD`, `F_DUPFD_CLOEXEC`, `F_GETFD`, `F_SETFD`, `F_GETFL`, `F_SETFL`), `lseek`, `flock`, `ftruncate`, `posix_fadvise`, `close`, `ioctl` (`FICLONE` and `FIONREAD`), `tcgetattr`, `geteuid`, `getegid`, `getgroups` |
| `UnixPathResolution` | `stat`, `fstat`, `fstatat`, `chmod`, `fchmod`, `fchmodat`, `chown`, `lchown`, `fchown`, `fchownat`, `utimensat`, `statfs`, `fstatfs`, `getcwd`, `chdir`, `access`, `faccessat` |
| `UnixNamespace` | `open`, `openat`, `readlink`, `readlinkat`, reading a directory, `mkdir`, `mkdirat`, `mknod` and `mknodat` (regular files only), `unlink`, `rmdir`, `unlinkat`, `rename`, `renameat`, `clonefile`, `symlink`, `symlinkat`, `link`, `linkat` |
| `UnixReadWrite` | `read`, `recv`, `pread`, `write`, `send`, `pwrite`, `copy_file_range` |
| `UnixPipe` | `pipe2` |
| `UnixSocket` | `socket`, `bind`, `listen`, `getsockname`, `getpeername`, `setsockopt`, `getsockopt` |
| `UnixConnection` | `connect`, `accept` |
| `UnixPoll` | `poll`, `epoll_create1`, `epoll_ctl`, `epoll_wait` |
| `UnixKqueue` | `kqueue`, `kevent` |
| `UnixSignal` | `kill`, `pthread_kill`, `sigaction`, `sigprocmask`, `pthread_sigmask`, `rt_sigprocmask`, `sigpending`, `sigsuspend`, `rt_sigsuspend`, `pause`, `sigreturn`, and the signals a task takes as it returns to user mode |
| `UnixClock` | `clock_gettime`, `gettimeofday` |
| `UnixEntropy` | `getrandom`, `getentropy` |
| `UnixCredentials` | `getresuid`, `getresgid`, `setresuid`, `setresgid`, `setgroups` |
| `UnixTaskLifecycle` | starting a thread, a thread exiting, `exit_group` |
| `UnixScheduling` | `getcpu`, and the client's report of which task it runs on which processor |
| `UnixSystem` | `getpid`, `umask` |

A connected TCP socket's `read` and `write` move bytes through its connection (`TcpConnection.Transfer`), which holds each direction's bytes in the sender's send buffer and the receiver's receive buffer, sized from the machine's TCP sysctls, and each end's state: open, a FIN received, reset, or closed. A close over bytes left unread resets the peer; otherwise it sends a FIN behind what it had sent. `poll`, epoll and kqueue read a connected socket's readiness from the same state, and each transfer wakes the waiters each flavour wakes: every arrival of bytes, and room freed in a send buffer (on Linux once after a write ran out of room, when the buffer has drained to two thirds full; on Darwin as bytes leave it). `FIONREAD` reports what waits to be read, and `SO_ERROR` takes a reset's error. A blocking read with nothing to answer sleeps until bytes, a FIN or a reset arrive; a blocking write takes what fits and sleeps for the rest, woken on Linux once its send buffer has drained to two thirds full and on Darwin once there is room for it to take something, and it returns once every byte is taken. Every task asleep on a socket wakes for what it waits on.

`recv` and `send` reach a connected TCP socket through the same transfer, sleep and wake as `read` and `write`, and take their flag word raw, in the flavour's numbering (`MessageFlag.number`). `MSG_PEEK` answers bytes without taking them, `MSG_DONTWAIT` makes a `recv`, and a Linux `send`, non-blocking (Darwin's `send` ignores it), and `MSG_NOSIGNAL` keeps an `EPIPE` from raising `SIGPIPE`; any other flag is refused, naming it. Where they part from `read` and `write` is measured: Linux screens their buffer before it looks up the descriptor, a Linux `recv` of nothing waits as a longer one would, and Darwin's `send` marks no description written.

`UnixSystem.step` puts the syscalls whose answer is a single integer behind one entry point, as cases of the `Syscall` type, for a client that wants to log, replay or generate them.
A syscall whose answer carries more than that, such as the bytes `read` returns, has no `Syscall` case, and is reached only through its own function.

Every descriptor lies below `SimulatedUnixPlatform.descriptorBound`, the soft `RLIMIT_NOFILE` a process of the flavour starts with (1024 on Linux, 256 on Darwin): the library assumes the process's limit is at least that, and refuses a call that would put a descriptor at or above it (`DescriptorLimitRefusal`), whose answer would depend on the limit.

`UnixSystem.checkInvariants` lists every way a system's tables disagree with each other.
No sequence of syscalls should ever produce one.
It is two halves: the rules about the machine, which read facts every process on it contributes to, and `UnixSystem.checkViewInvariants`, the rules about one process's view of it.
A process alone on its machine is held to both in full. Of a view a `SimulatedMachine` focused, only the machine's rules that one process can check truthfully are run; `SimulatedMachine.checkInvariants` runs the rest, against every process at once.
Each table's own rules are checked apart from these: `FileDescriptorRegistry.checkInvariants` of `UnixSystem.fileDescriptors` for the descriptor table and the open file descriptions it names, and `VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes system) (UnixSystem.fileSystem system)` for the filesystem.
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
`UnixWait.satisfied` asks of one task whether its call could get any further, which is cheaper, and does not decide which of several waiters on one queue wakes.
A woken task finishes its call through the family's finishing function (`UnixDescriptor.flockAcquire`, `UnixPoll.finishPoll`, `UnixReadWrite.finishRead`, `UnixSignal.finishSigsuspend`, and so on), which may answer, park again, or say the call restarts because a signal handler interrupted it.

A parked call holds the open file descriptions it waits on (`ParkedSyscall.descriptions`), as a real one holds a reference to each file: a description goes when no descriptor names it and no call holds it, so one closed under a sleeping call goes when the call returns.

## What the kernel leaves to the client

A running process relies on more than its kernel, and this library is only the kernel.
Each part below is something a real process has which this library does not do, so the client must.
For each part, this section says what the part is, why it belongs to the client, and how the client and the library meet at that line.
Where a part was placed by a decision, the decision is dated; where it is only how things stand today, that is said instead.

### User memory

**What.** The process's address space, and the calls that change it: `mmap`, `munmap`, `mprotect`, `brk`.
A thread's stack and its thread-local storage are part of it.

**Why the client's.** The client runs the process's code, so it already holds every byte that code loads and stores.
A second copy in the kernel would have to agree with the client's after every store.
A syscall needs only the bytes it moves, and the client can hand those over.

**Where they meet.** This library never reads or writes user memory.
A call that moves bytes takes them or returns them: `UnixReadWrite.read` returns the bytes it read, for the client to copy into the caller's buffer, and for a `write` the client asks `UnixReadWrite.admitWrite` how many bytes the call takes, copies that many out, and passes them to `UnixReadWrite.write`.
Where a real kernel would check a buffer's address, the client describes the address as a `UserBuffer` (`Unmapped`, `Mapped`, `Opaque` or `Addressless`), and the library decides whether and when the call answers EFAULT, against the limit set by `UnixBootImage.withUserAddressLimit`.
A pointer that the client's own foreign-function layer dereferences before making the call is the client's to answer for, since no kernel is involved.

A mapping of a file is not modelled, and a client should refuse one rather than copy the file's bytes into memory of its own.
Its pages are the file's bytes, and it holds the file's open file description as a descriptor does, so it crosses this line: a private copy would not see later writes through a descriptor, and the library would not count the mapping as holding the file open.
Nothing here maps a file.

### Blocks inside the process

**What.** A thread that waits without asking the kernel to watch a descriptor or a clock: Linux's `futex`, Darwin's `__psynch_*` and `__ulock_*` calls, and the mutexes, condition variables and timed waits that the C library builds on them.

**Why the client's, for now.** A `futex` wait is keyed by a user address and compares a word of user memory, which is the client's (see above).
So today the client keeps these blocks itself.
The long-term plan (#1726) is to model `futex` and `__psynch` in the kernel, so that every block happens there and the client has no blocked tasks of its own.
That is larger work, separate from the rest, and not yet scheduled.

**Where they meet.** A task the client holds blocked is parked in no call this library knows of, so as far as the library knows it could run.
The client removes such tasks from the set this library would call runnable (see the next part).
`UnixWait.wakes` decides only the parks this library recorded: whether a signal or a deadline ends one of the client's own blocks is the client's to decide.
When nothing can run, the client advances the clock to the earlier of its own deadlines and those `UnixWait.deadlines` reports.

### Which task runs

**What.** Choosing the next task to run, and on which processor.

**Why the client's.** Decided 2026-10-03 (#1726):

> The kernel never chooses which task runs. It answers which tasks could run (not parked in the kernel; the client subtracts its own blocked threads), and records what the client says happened.

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
Nothing else requires its caller to have been dispatched, so a client that never reads placement need never report it.
Every processor named must be one the machine has (`UnixSystem.processorCount`).
Not built yet: recording the processor time a task used, and a default policy (round-robin, and PCT, probabilistic concurrency testing) as a module that knows nothing of the kernel.
Both are planned in #1726.

### When time passes

**What.** How far the clock moves, and when.

**Why the client's.** Only the client knows how long the process's code took to run, and how fast its simulated machine is.
Nothing in this library moves the clock on its own.
Decided 2026-10-07 (#1726): `UnixSystem.advanceClock` stays a function from one system to the next, with no other result.
If clock-driven signals (`alarm`, `setitimer`, `timer_create`) are modelled later, an expiry will be written into the state as a pending signal, which the client finds through `UnixWait.wakes` as it finds a passed deadline.

**Where they meet.** The client calls `UnixSystem.advanceClock` between syscalls, reads the time with `UnixSystem.nanosecondsSinceBoot`, and with nothing runnable asks `UnixWait.deadlines` how far it may jump.
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

**Where they meet.** Before a task runs its own code, the client asks `UnixSignal.onReturnToUser`.
A `ReturnToUserOutcome.Resumes` answer means the task took nothing the client acts on, and runs its own code next.
A `ReturnToUserOutcome.RunHandlers` answer is the frames the kernel pushed, innermost first, each with the signal, the disposition and the mask to restore.
The client runs each handler, and when one returns it calls `UnixSignal.sigreturn` with that frame and asks `onReturnToUser` again.
A `SignalCatch` holds only the flags that change what the kernel does (`SA_NODEFER`, `SA_RESETHAND`, `SA_RESTART`); `SA_SIGINFO` and `SA_ONSTACK` choose how and on which stack the client calls its handler, so a client that honours them keeps them with its `'Handler`.
A handler that leaves by `siglongjmp` instead of returning has no operation yet.

A signal whose default terminates the process ends it where the library applies the default: at generation, when some task can take it (`KillOutcome.ProcessEnded`), and otherwise when a task that blocked it can take it, as the task returns to user mode (`ReturnToUserOutcome.ProcessEnded`): after a mask call or a `sigreturn` that unblocks it, or from a `sigsuspend` whose temporary mask lets it through.
Either answer is the `EndedProcess`, killed by the signal (`ProcessTermination.Signaled`, with the core flag), and no handler runs, not even one whose frame was pushed at the same return; the client ends the process on its machine with `SimulatedMachine.endProcess`, as for an exit.

A signal whose default stops the process is reported (`KillOutcome.ProcessStopped`, `ReturnToUserOutcome.ProcessStopped`) for the client to act on.
The library holds no stopped state, and nothing here continues a stopped process, so a client should refuse a stop rather than carry on as if the process were still running.
A SIGCONT at its default, on the other hand, has nothing to resume: `ReturnToUserOutcome.ContinueDiscarded` says the kernel discarded it as the task took it, and the client asks `onReturnToUser` again, since the return goes on, under a `sigsuspend`'s temporary mask if the task is returning from one.

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

**Where they meet.** The client reads what the process wrote from `UnixSystem.delivered`.

## Flavours and divergence from host platforms

WoofWare.PosixKernel's behaviour does not depend on the host platform; indeed, it probably works on Windows.
However, POSIX is extremely underspecified (and implementations frequently diverge from their documentation!),
and I only have easy access to a few flavours.

A `SimulatedUnixPlatform` names the kernel being simulated: its flavour (Linux or Darwin) and, for Linux, the version of the source it was built from (`LinuxKernelVersion`), its architecture, its page size and its release.
The release is only what `uname` reports; where Linux's behaviour changed between versions, the platform's version decides which answer applies.
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
