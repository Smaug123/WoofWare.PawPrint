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
     A's sleeping calls held, then closes every descriptor of A's, highest
     first (measured on both flavours by `exit-close-order.c`), and removes
     A. A listener holding another process's open client is refused, as
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
6. **Two guests.** A listener guest and a connecting guest, as two processes,
   exit 0.

After that come the socket features rung M needs, each a stage of its own:

- byte transfer on connected sockets, with buffer limits and the wakes when a
  socket becomes writable;
- `recv` and `send`, with `MSG_PEEK`;
- the options `TCP_NODELAY`, `IPV6_V6ONLY` and `SO_LINGER`;
- dual-mode IPv6 sockets;
- `getpeername`;
- `shutdown`, with the reset that linger 0 causes.

Then the Kestrel server and the `HttpClient` client run as two guests.
