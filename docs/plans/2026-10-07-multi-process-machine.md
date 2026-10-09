# Several processes on one simulated machine

## Goal

Two PawPrint interpreter instances, a Kestrel server and an `HttpClient` client,
run as two processes on one `WoofWare.PosixKernel` machine and talk over loopback
TCP. The host boots each process fresh, each with its own image. `fork(2)` and
`exec(2)` stay out of scope.

On the throwaway `rung-i-spike` branch, the same server and client already talk
inside *one* process (`docs/plans/2026-08-17-aspnet-critical-path`, rung M, on
the Linux CoreLib flavour). This plan covers what a second process needs. The
socket features rung M also needs are listed at the end: byte transfer,
`recv`/`send`, socket options, dual-mode IPv6, `getpeername` and `shutdown`.

## Where things stand

`UnixSystem = { Machine; Process; Tasks; Leader }` (decision 4a of
`2026-08-23-posix-kernel-extraction.md`).

**On the machine already:** sockets, connections, pipes, the clock, entropy,
ephemeral ports, the park ordinal and the OS thread-id allocator.

**On the process:** the whole `FileDescriptorRegistry`, both `Fds` and
`Descriptions`. A description holds its target, and for an epoll instance or a
kqueue that target holds the interest table and the ready list.

These walks look at "every description", and would see only one process's:

- `SocketWake.signal` and `KqueueQueue.activate`. A connect from A must wake an
  epoll instance in B.
- `ObjectLifetime.pinnedInodes`. A file open in B must survive an unlink by A.
- `ObjectLifetime.heldByCalls`, and the flock conflict check.
- The tids already held, read when a task is spawned.
- The `Delivered` output log, keyed by fd number alone.
- `checkInvariants`.

## Decisions

### 1. How a kernel operation names its process

- **(A) Zipper.** `UnixSystem` gains `Others : Map<ProcessId, _>` beside the
  focused process, and syscall signatures are unchanged. But a walk that should
  cover every process still compiles if it reads only the focused one: silent
  wrongness, prevented only by discipline.
- **(B) Explicit caller.** `UnixSystem = { Machine; Processes : Map<ProcessId, _> }`,
  and every syscall takes the caller's `ProcessId`: about 281 signatures and
  every PawPrint call site. A mechanical rewrite of `system.Process` to
  `system.Processes.[caller]` leaves each walk above exactly as wrong as before,
  so it buys no enforcement for the cost.
- **(C) A per-process view.** Today's `UnixSystem` becomes one process's view of
  the machine. A new `SimulatedMachine = { Machine; Processes : Map<ProcessId, ProcessSlot> }`
  holds every process, and `focus` and `unfocus` move between the two. A syscall
  cannot reach another process, because its argument has no field holding one.

**Chosen: (C)**, made sound by decision 2. Once descriptions live on the
machine, every walk above except two is a walk over the machine. The other
two are `heldByCalls` and the held tids. They become facts kept on the object
itself, as the real kernel does: a hold count on the description, and the live
set inside the `ThreadIdAllocator`. Cross-process *effects*, such as `kill`
delivering to another pid or `wait` reaping a child, would be returned as data
for the machine layer to apply. Nothing needs that yet, so it is not built.

### 2. Where descriptions live

- **(a)** Keep them per process, and qualify each id with its `ProcessId`.
- **(b)** A machine-wide id allocator, with the tables still per process.
- **(c)** Move the `Descriptions` table, with the epoll and kqueue state inside
  it, to the machine, and keep only `Fds` on the process. This is the real
  kernel's split between `struct file` and the fd table.

**Chosen: (c).** (a) cannot represent a description that two processes share,
which `fork` and `SCM_RIGHTS` would need. (b) fixes the ids colliding, but leaves
every walk above per process. Under (c), a description counts the fds naming it,
across all processes. `checkInvariants` gains a clause at the machine level, that
the count equals the fds naming it in every process. `Delivered` is keyed by
process and fd.

### 3. Where PawPrint keeps the shared machine

- **(a′) Swap the machine.** Each `IlMachineState` keeps its own process's view.
  A driver owns the one `UnixMachineState`, puts it into the program about to
  step, and takes it out afterwards. An idle program holds a stale machine,
  which only the driver can see, and a generation stamp is checked at each
  handover in Debug builds.
- **(b) Thread it.** Take the machine out of `IlMachineState`, and pass
  `(machine, state)` through about 377 sites. This rules out a stale copy
  structurally.

**Provisionally (a′).** The stale copy is confined to one function in the
imperative shell, and (b) is still open if that copy ever causes a bug. This is
the decision most worth Patrick's review before stage 5.

### 4. Time and interleaving

One machine clock, which is already on `UnixMachineState`, and one global
interleaving. A seeded choice picks the next program, and that program's
scheduler picks its thread. The jump to the nearest deadline, and the deadlock
report, move up to the driver. When no program has a runnable thread, it jumps
to the earliest deadline across all of them. A deadlock means none is runnable
and none has a deadline. The alternative was rounds, which treat the programs
like parallel CPUs, but that would invent a second notion of time.

## Stages

Each stage is a PR, green alone.

1. **Descriptions on the machine.** Move `Descriptions`, its id allocator and the
   epoll and kqueue state from `FileDescriptorRegistry` to `UnixMachineState`,
   with the count of fds naming each description. `Delivered` is keyed by
   process and fd. No behaviour changes; the existing suites are the oracle.
2. **Facts on the object.** The hold count for calls in flight moves onto the
   description, and the live tids into the `ThreadIdAllocator`.
   `checkInvariants` splits into machine clauses and view clauses.
3. **`SimulatedMachine`.** Add `focus`, `unfocus`, and booting a second process
   onto an existing machine. A property test: a random interleaving of two
   processes' syscalls keeps every invariant, and leaves the other process's
   slot unchanged.
4. **Wakes across processes.** Done. Tests of the kernel alone
   (`TestCrossProcess`, and `TestMultiProcessFuzz` with stage 3's exclusions
   lifted):
   - B's connect wakes A's accept, A's epoll instance and A's kqueue, and A's
     sleeping Darwin `poll`. `SimulatedMachine.wakes` asks each sleeping
     task's condition of its own process's view, and picks the one waiter of
     an exclusive queue across every process by the machine's park order. No
     such queue is shared between processes yet: nothing passes a descriptor
     to another process, so no pipe, listener or epoll instance is.
   - A kqueue registration, and a filter a sleeping Darwin `poll` registered,
     records the socket its descriptor named (`KqueueRegistration.Socket`,
     `PollRegistration.Socket`), as XNU attaches a knote to the socket, so
     activation walks every kqueue on the machine without reading a
     descriptor table. A sleeping Darwin poll's kqueue (`PollQueue`) moved
     from its park to the machine, for the same reason, and lives exactly as
     long as the park.
   - A exits, and B sees the FIN: `SimulatedMachine.endProcess` releases what
     A's calls held, then closes every descriptor of A's in each flavour's
     measured order (`exit-close-order.c`: Linux drops them lowest first and
     releases the last let go of first, Darwin closes them highest first),
     and removes A. A listener holding another process's open client is refused, as
     `close` refuses it: the reset is not measured. The single-process
     `EndedProcess.Machine` still leaves the descriptors open.
   - A file unlinked by A survives while B has it open, and goes as B closes
     it.
   - `FileDescriptorRegistry.checkInvariants` of one view checks only what
     one descriptor table can tell (`DescriptorCensus`).

   `kill` of another process is still refused.
5. **PawPrint driver.** An N-program driver in `Program` owns the machine, and
   deadline jumps and deadlock detection move into it. The one-program path
   becomes the N = 1 case, and the old loop is deleted.
6. **Two guests.** Done, in 5d's `TestSeveralPrograms`. A listener guest
   and a connecting guest, as two processes, exit 0, on the host's CoreLib
   and on the Linux one: the listener accepts one connection on a fixed
   port and checks its peer's address, and the connector retries on
   `ECONNREFUSED` until the listener is up.

After that come the socket features rung M needs, each a stage of its own:

- byte transfer on connected sockets, with buffer limits and the wakes when a
  socket becomes writable;
- `recv` and `send`, with `MSG_PEEK`;
- the options `TCP_NODELAY`, `IPV6_V6ONLY` and `SO_LINGER`;
- dual-mode IPv6 sockets;
- `getpeername`;
- `shutdown`, with the reset that linger 0 causes.

Then the Kestrel server and the `HttpClient` client run as two guests.

## Stage 5 design

Line numbers are those of the stage 4 tip (`c12d35cc`).

### Measurements

A Release build, a 100,000-iteration arithmetic loop as the guest, timed
after a warm-up run:

- One tick (`stepPrepared`) costs 860 ns and allocates 3,470 bytes.
- One handover costs 169 ns and 944 bytes. That is `SimulatedMachine.focus`,
  then `IlMachineState.MapKernel (EmulatedKernel.withUnix view)`, then
  `unfocus`: 20% of a tick's time and 27% of its allocation.
- `SimulatedMachine.ofSystem` costs 201 ns and 552 bytes.

So nothing may hand over, or call `ofSystem`, on every tick of a run of one
program.

### The driver's state

```
MultiProgram =                        // opaque
    { Machine : SimulatedMachine      // stale while a program is checked out
      Current : ProcessId * ProgramSlot
      Idle : Map<ProcessId, ProgramSlot>
      Ended : (ProcessId * RunEnd) list
      Choice : ProgramChoice
      ... }
ProgramSlot = Starting of StartingProgram | Running of RunningProgram
```

`RunningProgram` holds what `PreparedProgram` holds today: `State`,
`BaseClassTypes`, `EntryThread`, `EntryFrame` and `LastRan`. `StartingProgram`
holds what `Startup` holds today.

Exactly one program is checked out: `Current`. Its `Kernel.System` is the
authoritative view. `Machine` is the copy of the machine that view was
focused from, so the `Origin` guard accepts the view when it comes back. An
idle program holds a view whose process and tasks are current, because
nothing but its own steps changes them, and whose machine is stale.

The stored machine is only re-synchronised when the driver switches
programs. To switch, it calls `unfocus` on the current view, then `focus`es
the next program and puts that view into the next program's kernel. A
program of one never switches.

The step counter travels with the machine. The checked-out program's
`Kernel.StepCounter` is the global tick. A switch copies it into the next
program, as a switch carries the clock across inside the machine. So
`StepCounter` stays where every test reads it, and for one program it is
what it is today.

`InstructionCostTicks` and `ClockJitter` describe the machine, and govern
the clock advance and the jitter, which move to the driver. So in 5c they
move off `EmulatedKernel` with them: every reader moved, and the driver
holds them as a `MachineClock`, which `MachineConfig.clock` checks and
makes.

5c built the driver for one program: `MultiProgram = { Machine; Current;
Clock }`, with `Current` a `RunningProgram` (what `PreparedProgram` held).
5d added `Idle`, `Ended`, `Launched`, the program choice and the program
chosen last. There is no `ProgramSlot`: a program's phase of startup is
what its entry thread is running, so it is `EntryFrameKind.StartupCall` of
a `StartupPhase`, and a `RunningProgram` is a program in any phase.
`RunningProgram.stepDecided` moves it to its next phase, or installs
`Main`, when the pumped call returns. The per-program core of startup moved
from `Program.beginStartup` to `ProgramStartup`, which the driver's
`launchAll` runs for each program; `Program.Startup` and
`Program.PreparedProgram` are both a driver of one program.

### 1. What stays per program, and what moves to the driver

The tick is now: the driver's preamble, then the program choice, then the
chosen program's `stepDecided`.

- **Spurious wakeups** (`Program.fs:527-534`). Per program, for every live
  program, every tick, keyed on the global tick. They read no
  `Kernel.System`, so they run on idle programs without a handover.
- **The tick and the clock** (`Program.fs:540`, `560`;
  `EmulatedKernel.retireStep`, `EmulatedKernel.fs:1365`). Moves to the
  driver: once per global tick, on the checked-out program's kernel. That
  program's fused one-record `retireStep` is kept.
- **Clock jitter** (`Program.fs:577-595`). Moves to the driver: keyed on the
  global tick, drawn from every program's `pendingDeadlines`
  (`Program.fs:422`), and applied to the checked-out view. `pendingDeadlines`
  reads thread statuses and the process's own parks (`UnixWait.deadlines`,
  `UnixWait.fs:305`), both current on an idle view.
- **Expired deadlines** (`Program.fs:605`, `fireExpiredDeadlines` at `251`).
  Per program, every live program, every tick. It reads the clock at
  `Program.fs:252`, which is a machine read and so stale on an idle view.
  It will take `now` from the driver as an argument instead. The fire
  functions it calls (`WaitHandle`, `LowLevelMonitor`, `SyncBlockMonitor`,
  `Scheduler`) read no `Kernel.System`.
- **Signal poll** (`Program.fs:614-617`; `SignalDispatch.poll`,
  `SignalDispatch.fs:598`). Per program, and the only preamble phase that
  needs the machine: it writes the signal pipe. The driver hands over to a
  program only if its poll can do something, which it decides from
  process-local facts: a signal is pending (`SignalState.pending`), or the
  dispatcher is `Parked`. Otherwise the poll is the no-op its own fast path
  (`SignalDispatch.fs:301`) already returns. If the poll ends the process,
  that is one program ending (§4).
- **Syscall wakes** (`Program.fs:623`, `fireSyscallWakes` at `391`). Moves to
  the driver; see §2.
- **The quiescence jump** (`Program.fs:656-684`). Moves to the driver. It
  stops if any program has a runnable thread. Otherwise it jumps to the
  minimum of every program's deadlines, fires expired deadlines in every
  program, runs the global wakes, and repeats.
- **The deadlock** (`Program.fs:803-806`, where `chooseNext` answers
  `None`). Moves to the driver: if no program is runnable after the jump,
  the driver reports `Deadlocked` with each live program's process ID and
  `GuestLocation.describe`. The chosen program's `chooseNext` then always
  answers `Some`, and that is asserted.
- **Per program and unchanged:** `stepDecided`'s execution, `afterStep`,
  `latchMainReturnValue`, and startup's phase changes.

The operations the driver runs on idle programs are listed in one place:
spurious wakeups, `fireExpiredDeadlines now`, `pendingDeadlines`,
`hasAnyRunnable`, `asleepInSyscall`, `wakeFromSyscall`, and the poll
pre-check. A Debug build asserts that none of them changed the program's
`Kernel.System` by reference, which catches a write through a stale view.
It cannot catch a read; the clock was the one machine read among them, and
it is now an argument. When the driver focuses a program, a Debug build
also asserts that the slot's process and tasks are the idle view's, by
reference.

### 2. Syscall wakes: `SimulatedMachine.wakes`

The driver calls it once per tick, with each program's `asleepInSyscall`,
and only when some program has a syscall waiter, which is today's fast
path. It flips each woken `(pid, task)` in that task's program. It calls
`wakes` on `unfocus currentView Machine`, a value it then throws away. That
costs about one `Map.add` and the identity checks, and needs no handover.

The alternative was each view's own `UnixWait.wakes`, relying on there being
no exclusive wait queue across processes. Nothing enforces that: it holds
only because nothing passes a descriptor to another process yet, and
`241e564a` forges the state in which it fails. The first PR that adds `fork`
or `SCM_RIGHTS` would leave PawPrint's wakes silently wrong. Stage 4 built
the machine-wide wakes for exactly this. For one program the two agree,
since `UnixWait.wakes` is `wakesAmong` of a single process.

One per-view use remains. `SignalDispatch.fs:535` asks `UnixWait.wakes`
whether the dispatcher's own read of the signal pipe wakes. The process's
own shim creates that pipe, so this holds until descriptors can be inherited.
The note at that call will say so.

### 3. Startup, the host API, and the configuration split

**Configuration.** `KernelConfig` splits into two parts.

`MachineConfig` takes:
- `ProcessorCount`, `UserAddressLimit` and `WallClockEpochMs`.
- `UnixPlatform`, `FileSystem`, `FileSystemRootOwner` and `Mount`.
- `EphemeralPortRange`, `SoMaxConn`, `TcpSendSpace`, `ProtectedFiles`,
  `LocalAddresses` and `LocalRoutes`.
- `PidMax`.
- `InstructionCostTicks` and `ClockJitter`.
- `ProcessId` and `LeaderThreadId`, which are now the first process's: later
  processes' IDs are the kernel's choice.
- A new `ProgramChoice`, which is `RoundRobin` or `Seeded of uint64`.

`ProcessConfig` takes:
- `Environment`, `CurrentDirectory`, `ProcessPath` and `StandardStreams`.
- `UserId`, `GroupId`, `SupplementaryGroups` and `Umask`.
- `InheritedSignalIgnores` and `CoreDumps`.
- `CLibrary` and `OptimalMaxSpinWaitsPerSpinIteration`, both userspace.

`KernelConfig` stays as the one-program sugar, and `KernelConfig.split`
returns both parts. `toKernel`'s setters become `MachineConfig.toImage` and
`ProcessConfig.toLaunch platform`, so each failure still names its knob.

A launch carries a `ProcessConfig` rather than a library `ProcessLaunch`.
PawPrint's environment seed, the standard-stream launch table and the named
refusals are PawPrint's, and a launch built by hand would bypass them.
`bootInheritingSignalIgnores` (`EmulatedKernel.fs:1065`) splits into the
machine's boot and `EmulatedKernel.ofView`. `ofView` is the userspace set-up
done in the process's focused view: the signal dispositions, Darwin's
`getentropy(32)`, and the C library.

**API.**

```
ProgramLaunch = { Image : Stream; OriginalPath; DotnetRuntimeDirs;
                  Process : ProcessConfig; Argv; AssemblyPath; AppContext;
                  PctSeed : uint64 option }
MultiProgram.begin : ILoggerFactory -> MachineConfig -> ProgramLaunch list -> MultiProgram
MultiProgram.step  : ILoggerFactory -> ILogger -> MultiProgram -> MultiStepOutcome
    | Stepped of MultiProgram * ProcessId * ProgramEvent
        // InstructionStepped (thread, whatWeDid, effect) | WorkerTerminated thread
        // | PhaseAdvanced | Ended of RunEnd
    | Finished of (ProcessId * RunEnd) list           // launch order
    | Deadlocked of MultiProgram * (ProcessId * string) list
MultiProgram.run   : ... -> (ProcessId * RunEnd) list // fails loudly on a deadlock, as pumpPrepared does
```

`begin` boots the machine with `UnixBootImage.boot` and `ofSystem`, applies
`pid_max`, and launches the other processes with `SimulatedMachine.launch`,
all in launch order. It then focuses each program in turn, builds its
kernel with `ofView`, and runs `beginStartup`'s per-program core on that
kernel. Entropy is drawn from the shared pool, so launch order is part of
the replay contract.

**Startup interleaving.** Startups interleave from the first tick, chosen as
`Main` ticks are. The alternative was to run each startup to completion in
turn, but a static initialiser that waits on another process would then
deadlock where real processes would not.

**Choosing a program.** The choice is made among programs with a runnable
thread. `RoundRobin` takes the next such program after the last in launch
order. `Seeded s` draws uniformly by a pure hash of `(s, tick)`, as
`ClockJitter.draw` does, so there is no RNG state. With one candidate there
is nothing to draw.

### 4. A program ends while others run

The program's step, or its poll, completes with a `RunOutcome`. The driver
then calls `SimulatedMachine.endProcess` with that outcome's `EndedProcess`
against `Machine`. The `EndedIn` view descends from the checkout, so its
`Origin` holds. The driver records `(pid, RunEnd)`, then focuses the next
live program from the machine that `endProcess` returned.

Until 5a the `EndedProcess` was thrown away where the kernel produced it:
at `EmulatedKernel.exitGroup` and `abort`, in `NativeLibc`'s kill, in the
SIGPIPE path of `NativeSystemNative`'s write, and in `SignalDispatch`. Now
`exitGroup` and `abort` answer it, and `ExecutionResult.SignalTerminated`,
`SignalPoll.ProcessKilled` and `NativeSystemNative`'s
`NonCanceledPosixSignal.Terminated` carry it to where `Program` makes the
`RunOutcome`. `RunOutcome` keeps its shape: a host has no use for the
view a process ended in, and about 300 matches on it would otherwise
change. In 5c the driver takes the end from there.

`Environment.Exit`, an unhandled exception, `FailFast` or a fatal signal
ends that program alone. The others see only what the kernel shows them,
such as a FIN or a lock let go of. Each program's exit code is its own
`RunEnd`, and its output is its own `OutputLog`, keyed by its process ID.

A `ProcessEndRefusal` fails the run loudly, naming the program, as a
refused `close` does. A host failure, meaning a `failwith` in the
interpreter, fails the whole run, because what the program would have done
is unknown. Each program's step is still wrapped in `annotating` with its
own state. A failure in a driver phase is annotated with every live
program's `GuestLocation`.

A run of one program also calls `endProcess`, once at the end, and discards
its machine. That way the whole Guest suite exercises `endProcess` and the
`Origin` guard.

### 5. Fork snapshots and the debugger: one program, by type

`PreparedProgram` and `Startup` become opaque handles on a `MultiProgram`
of one. They get `State`, `EntryThread`, `LastRan` and `BaseClassTypes`
members, and a `withState`; one test replaces the state by record copy
(`TestScheduleFork.fs:480`).

`stepPrepared`, `stepStartup`, `beginStartup`, `prepare`, `run`,
`pumpPrepared`, `runToFirstFork`, `runToNextFork` and `resumeFork` keep
their signatures and desugar to the driver. The fork prefix keeps its
two-call probe: `advance` returns the driver's preamble, and then comes
the choice.

The fork snapshots and the debugger's `stepPrepared` take a `PreparedProgram`,
so a run of several programs cannot reach them. No runtime refusal is
needed. Allowing several programs there would require counting the choice
between programs among the decisions that make a fork, which is out of
scope.

### 6. Risk to one program, and speed

A program of one never switches, so it never hands over. Its tick does what
it does today, plus one copy of the driver record. That is a few dozen bytes
against 3,470. The `unfocus` for wakes runs only on ticks with a syscall
waiter, in place of today's `UnixWait.wakes`. The `endProcess` at the end is
new behaviour, and a guest whose end it refuses would now fail. The
implementation PR reports the Performance benchmarks against `main`.

With several programs, a switch costs 169 ns (20%), and it happens only
when the chosen program is not the checked-out one. Under per-tick
`RoundRobin` that is almost every tick. A quantum, which keeps a program for
K ticks, would amortise the switches. It is not built until stage 6 shows
it is needed, since it changes which interleavings can be reached.

### 7. PRs

- **5a.** Carry `EndedProcess` to where `Program` makes the `RunOutcome`,
  and end every run's process on a machine of its own
  (`SimulatedMachine.ofSystem`, then `endProcess`), failing loudly if it is
  refused. The machine left is thrown away until 5c.
- **5b.** Split `KernelConfig` into `MachineConfig` and `ProcessConfig`
  (`KernelConfig.split`; `MachineConfig.boot` boots the machine with its
  first process, and `toKernel` is the two), add `EmulatedKernel.ofView`,
  and make `fireExpiredDeadlines` take `now`. A property test: for
  generated `KernelConfig`s, booting through `split` gives the kernel that
  every setter applied to one image gives. 5a and 5b are one PR.
- **5c.** Done. The driver core, with one program running through it.
  `advanceToDecision`, `stepTick` and their per-program jump and deadlock
  are deleted. The tick wakes through `SimulatedMachine.wakes`, and
  `endProcess` runs at every end. The oracles are the whole suite, Guest
  fixtures included; `TestScheduleFork`'s bit-identity; and `TestClockJitter`
  and `TestRetireStep`. The PR reports the performance numbers.
- **5d.** Done. Several programs. `begin` is an F# keyword, so the host API
  is `MultiProgram.start`, `step` and `run`. An idle program is checked
  out for its signal poll only when `SignalDispatch.mayAct` says the poll
  can do something. That leaves out a dispatcher asleep in its read of the
  signal pipe, which only its own process writes, and whose poll runs
  while that process is still checked out; so no idle program is checked
  out for its poll until a process can signal another or share its pipe. A failure in a driver phase is annotated with the
  program checked out when the tick began, and a failure in a step with
  the program that stepped, rather than with every program: `GuestLocation`
  names threads, which two programs share the names of. New tests in
  `TestSeveralPrograms`, whose guests write through `SystemNative_Write`
  so that they stay fast:
  - **`two guests on one machine report distinct process IDs and keep
    separate output logs`.** The first process has the configured ID and
    the second has the kernel's next ID, and each log holds only its own
    line.
  - **`the clock does not jump to a sleeper's deadline while another
    program is runnable`.** A sleeps 50 ms, and then the time it actually
    slept must be in [50, 51) ms. B spins for 100 ms on
    `Environment.TickCount64`, and the largest gap it sees between two reads
    must be 1 ms or less. Making the jump per program again must turn this
    red.
  - **`both programs blocked forever is one deadlock naming both`.**
  - **`one program's Environment.Exit, or its unhandled exception, leaves
    the other running`.** The exit codes are per program.
  - **`one launch through MultiProgram is the same run as Program.run`.**
    The exit code, output, `StepCounter` and clock all match.
  - **`the same program choice seed replays the same interleaving`.**
