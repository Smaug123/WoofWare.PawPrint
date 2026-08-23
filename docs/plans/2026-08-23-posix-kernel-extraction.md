# WoofWare.PosixKernel: extracting the emulated POSIX process from PawPrint

`EmulatedKernel.fs` is 6,948 lines and holds two unrelated things: a simulated
Unix process (filesystem, descriptors, sockets, signals, clock, credentials)
and a simulated CoreCLR runtime environment (`LowLevelMonitor`, `WaitHandle`,
the `AllocHGlobal` arena, spurious-wakeup and clock-jitter search strategies).
Meanwhile the syscall *semantics* — `open`'s flag decode, its errno ordering,
`epoll_wait`'s copy-out shape — live in `Native/NativeSystemNative.fs`
(7,618 lines) interleaved with PAL decoding and guest-memory access.

This plan extracts the first thing into a new top-level component,
`WoofWare.PosixKernel`, as a general POSIX process simulator with no
IL/CLR/BCL/PAL concepts in it, and makes PawPrint one client of it.

## Commissioned scope

**Stages 1–3 only, as a spike.** Those three stages are rename-only: they move
the 8,190 lines that are already free of every CLR concept, and they change no
behaviour. What they are meant to establish is whether the boundary is real —
whether `WoofWare.PosixKernel` can build and test with no reference to
`WoofWare.PawPrint.Domain`, and whether the host-equality tests still pass on
both platforms once they live on the other side of it. Stages 4–9 are written
down here because the spike is only worth running if there is somewhere for it
to go, but they are **not commissioned**: revisit them once stage 3 has
answered its question.

## Goal

A pure state machine that answers syscalls. Specifically:

* It owns the filesystem, the descriptor table, sockets and connections,
  the environment, signal dispositions, credentials, the umask, the clock,
  entropy, and the Unix platform profile.
* Every transition is `State -> Request -> Response * State`. No effects, no
  host reads, no blocking.
* It knows nothing about IL, the CLR type system, managed heaps, eval stacks,
  P/Invoke, or the `libSystem.Native` PAL. A second client — a POSIX simulator
  for some other language runtime — should be able to use it without meeting
  any PawPrint concept.

### Non-goals for this work

* Multi-process. `fork(2)`/`exec(2)` stay unmodelled; see decision 4 for why
  the *shape* accommodates them without the *implementation* existing.
* Changing any guest-observable behaviour. Every stage's oracle is that the
  existing suite — including the `Guest` fixtures — is unchanged and green.
* Repackaging as a NuGet package on day one; see the open questions.

## What is already true

Measured, not assumed. Grepping the eleven candidate files for CLR-side type
names (`MethodInfo`, `ConcreteTypeHandle`, `CliType`, `ManagedHeapAddress`,
`EvalStackValue`, `NativeMemoryPool`, `ThreadId`, `IlMachineState`, …):

| file | lines | CLR types referenced |
| --- | --- | --- |
| `UnixError.fs` | 653 | none |
| `UnixPathText.fs` | 85 | none |
| `AbsoluteUnixPath.fs` | 190 | none |
| `UnixPath.fs` | 479 | none |
| `VirtualFileSystem.fs` | 3,069 | none |
| `FileSystemSeed.fs` | 101 | none |
| `InternetEndpoint.fs` | 112 | none |
| `FileDescriptorRegistry.fs` | 1,904 | none |
| `Signal.fs` | 247 | none |
| `SignalState.fs` | 347 | `MethodInfo<ConcreteTypeHandle,…>`, `ThreadId` |
| `EmulatedKernel.fs` | 6,948 | `ThreadId`, `CpuId`, `OsThreadId`, `NativeMemoryPool`, `NativeMemoryBlockId`, `LowLevelMonitor*`, `WaitHandle*`, `NonCryptoRandom`, `ManagedHeapAddress` |

**Nine of the eleven files are already CLR-free.** 6,840 lines can move with a
namespace change and nothing else. That is the single most important fact
here: it turns what looks like a rewrite into a sequence of renames followed by
one genuinely hard split.

Two further measurements:

* 122 files across the repo reference at least one of these types (39 in
  `WoofWare.PawPrint`, 2 in `.App`, 81 in `.Test`). Each needs one added
  `open WoofWare.PosixKernel`; `.IlDump` and `.Performance` need none.
* 76 of the last 420 commits (18%) touched one of `EmulatedKernel.fs`,
  `VirtualFileSystem.fs`, `FileDescriptorRegistry.fs`,
  `Native/NativeSystemNative.fs`, and there are ~45 live worktrees under
  `.claude/worktrees/`. Every move stage must be a single mechanical commit
  landed fast, or it will spend its life in conflict.

## Options considered

### 1. Where the boundary sits

**(a) Shared vocabulary only.** Move the state types and their operations;
PawPrint keeps `EmulatedKernel` as a composite that *contains* them, and every
syscall handler stays in `NativeSystemNative.fs`.

Cheap and low-risk, but it does not deliver a POSIX simulator. The interesting
content — that `O_CREAT|O_EXCL` does not follow a final symlink, that an
unrecognised open flag is EINVAL before any path is looked at, that a failed
`epoll_wait` writes 0 through `*count` on Linux and -1 on Darwin — stays in
PawPrint, and a second client would have to re-derive all of it. The boundary
also has no oracle of its own: nothing can test the library except by running a
guest.

**(b) Syscall service.** The library owns a `Syscall` request DU and a pure
`step : UnixSystem -> TaskId -> Syscall -> SyscallOutcome * UnixSystem`.
`NativeSystemNative` becomes a translator: decode PAL arguments into a
`Syscall`, apply it, encode the outcome onto the eval stack / into errno / into
guest memory.

Expensive, but it is what was asked for, and it gives the library an oracle
that needs no guest at all: `TestVirtualFileSystemAgainstHost` and the
`SocketFuzz` differential fuzzer both already drive the model directly and
compare against a real kernel. Lifting them one altitude up — from
`VirtualFileSystem.createFile` to `Syscall.Open` — makes them test the thing a
client actually consumes.

**Chosen: (b), reached through (a).** (a) is a strict prefix of (b) and is
independently valuable, so it is stages 1–6 and (b) is stages 7–9. If the
project stalls after stage 6 we still have a clean, separately-tested library;
we just have a smaller one.

### 2. Does the library see guest memory?

**(a) The library owns the user address space.** Move `NativeMemoryPool` in;
syscalls take addresses and perform their own copy-in and copy-out, so EFAULT
and SIGSEGV are modelled entirely inside.

This is wrong for PawPrint, and the reason is structural rather than a matter
of taste. PawPrint's memory is not a flat byte array: it is cell-typed
(`CliType`), with provenance-tracked pointers, byref containers, managed-heap
interior pointers, and a whole `ManagedPointerByteView` / `StorageLocation`
machinery that exists to preserve exactly the information a `byte[]` throws
away. A buffer argument may be a managed object's interior, which has no
address. Flattening it to satisfy the library would destroy the model.

**(b) Value in, effect descriptions out.** A syscall request carries
`ImmutableArray<byte>` payloads and *abstract* user addresses; the response
carries an ordered list of effects (`WriteBytes of UserAddress * bytes`,
`SetErrno of int`, `Return of int64`) that the client applies. The library
keeps `UserAddressLimit` and `UserBufferCheck`, because those are a pure
predicate on `(address, length)` and not a memory access.

Buffers reach the library as a three-way classification, because PawPrint
already distinguishes all three and the distinction is guest-visible:

```fsharp
type UserBuffer =
    /// A null pointer. The syscall answers EFAULT.
    | Null
    /// Non-null, but names no storage. A real run takes SIGSEGV; today
    /// PawPrint `failwith`s at each such site. Making it a response case
    /// states the contract instead of scattering it.
    | Unmapped of address : uint64
    | Mapped of address : uint64 * bytes : ImmutableArray<byte>
```

**Chosen: (b).** This is the gospel's "compute a description of what to do,
then do it", and it is the only option that preserves PawPrint's pointer
provenance. Ordering of copy-out relative to the errno write becomes *data*
(the order of the effect list) rather than discipline in 7,600 lines of
handler.

The client still needs kernel constants to build a request — how many bytes to
read for a path before giving up. Those are already exposed
(`PathLimits.pathMaxBytes`), and stay exposed.

### 3. How blocking is expressed

**(a) The library owns a scheduler**, with runnable/blocked task states and a
run queue.

Two schedulers in one process is a disaster, and PawPrint's is entangled with
class-init locks, monitors, exception dispatch and its own fairness search.
Also, `WaitHandle` and `LowLevelMonitor` are not POSIX objects, so the
library's scheduler would immediately need to know about them.

**(b) The library never blocks.** A syscall that would block returns
`SyscallOutcome.WouldBlock of WakeCondition`, where `WakeCondition` is data
(`SocketEventsDeliverable of OpenFileDescriptionId`, `Deadline of ticks`, …),
and the library exposes a pure `WakeCondition.isSatisfied : WakeCondition ->
UnixSystem -> bool`. The client's scheduler decides what parking means and
polls the predicate.

**Chosen: (b).** This is already the de-facto design and needs only naming:
`SystemNative_WaitForSocketEvents` parks by storing `ParkedSocketWait` in the
kernel and calling `Scheduler.blockOnSocketEvents`, and `Program`'s readiness
sweep wakes it by polling `EmulatedKernel.hasDeliverableSocketEvents`.
Formalising it means the library states the wake contract instead of it being
an agreement between two files.

### 4. The state split

GPT-5.6's suggested three-way split (machine / process / task) is right, and
the question is only how far to take it.

**(a) Three records, exactly one process.**

```fsharp
type UnixSystem =
    { Machine : UnixMachineState
      Process : UnixProcessState
      Tasks : Map<TaskId, UnixTaskState> }
```

The *typing* separates the three scopes, so the compiler answers "where does
this fact live?" — which the `emulated-posix-kernel` skill names as the most
common mistake in this area, and which today is answered by a comment on a
flat 46-field record. Adding `fork` later is changing one field to a
`Map<ProcessId, UnixProcessState>`.

**(b) Multi-process from the start.** Every syscall signature gains a
`ProcessId`; every test gains a process.

Rejected. Nothing exercises it, `fork` is a real design job (the fd *table* is
copied but the open file *descriptions* are shared, and PawPrint has no way to
test that it got it right), and by the gospel's own standard an abstraction
that cannot be exercised has not earned its place. The migration path is
already open, and not by accident: `FileDescriptorRegistry` separates `Fds :
Map<int, OpenFileDescriptionId>` from `Descriptions :
Map<OpenFileDescriptionId, OpenFileDescription>`, which is precisely the split
`fork` needs.

**Chosen: (a).**

One consequence worth flagging, because it is a behaviour change and needs its
own test. Today `LastSystemError` and `SignalState.Blocked` are
`Map<ThreadId, _>` that must *delete* an entry when it returns to its default,
because `EmulatedKernel` is compared for structural equality to decide whether
a step changed anything, and a stored default is a state that looks different
while behaving identically. Under (a) errno and the blocked mask become fields
of a per-task record, which is created when a task is registered and removed
when it exits. That removes the canonicalisation obligation entirely — but only
if task registration and removal are exact. The property tests in
`TestSignalState.fs` and `TestLastError.fs` currently assert "no default may
ever be stored"; their replacement must assert "the task set is exactly the
live tasks".

### 5. `SignalState` carries a CLR type

`SignalHandler` wraps a `MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle,
ConcreteTypeHandle>`, and `SignalInitState.Initialized` carries the `ThreadId`
of PawPrint's internal dispatcher thread. So `SignalState` is *not* CLR-free,
and is the only kernel-side type that is not.

**(a) Erase to an opaque token.** The library holds `HandlerToken of int64`;
the client keeps a side table mapping tokens to methods.

Loses `SignalHandler`'s equality (`MethodInfo.NominallyEqual`), which is
load-bearing — two registrations of the same method must compare equal — and
adds a table whose entries can go stale.

**(b) Make it generic: `SignalState<'Handler>` with `'Handler : equality`.**
PawPrint instantiates `'Handler = SignalHandler`. A real kernel's `sa_handler`
*is* a user address, so a client that has addresses instantiates it with one;
PawPrint has no address for a managed method, and the generic parameter is
exactly the honest statement of that.

**Chosen: (b).** The dispatcher `ThreadId` is a different matter: no real Unix
has a dispatcher thread — it is PawPrint's model of asynchronous delivery. That
identity moves out to PawPrint, leaving the library with a payload-free
"signal handling has been initialised" bit. The DU's invariant ("the dispatcher
exists iff signal handling is initialised") is preserved on the PawPrint side.

### 6. The shared PRNG

`NonCryptoRandom` lives in `WoofWare.PawPrint.Domain` and is used by both the
kernel (`NonCryptoRandomState`, `CryptoRandomState`, clock jitter) and
PawPrint's scheduler. `Domain` is CLR metadata and must not gain a POSIX
dependency; the new library must not gain a CLR one.

**(a) Move it to `WoofWare.PosixKernel`.** PawPrint references both, so its
scheduler keeps working. But `WoofWare.PawPrint.Domain` is a published NuGet
package, and deleting a public module from it is a breaking change — and it
would put the scheduler's PRNG in a package about POSIX, which is no more
honest than leaving the kernel's in a package about CLR metadata.

**(b) Duplicate the 126 lines.** No breaking change; two copies of a PRNG that
must agree for no reason, which is exactly the "two versions of the truth"
the gospel warns about.

**(c) A third project** owning the PRNG, referenced by both.

**Chosen: (c).** The two consumers are doing genuinely different things with
it — the kernel draws entropy a guest reads back through `getrandom`/
`/dev/urandom`, the scheduler draws an interleaving to explore — and neither is
a special case of the other. A deterministic PRNG is a utility, and a utility
with two unrelated consumers belongs in neither consumer. `Domain` holding it
today is an accident of where it was first needed.

This does not arise until stage 5: `NonCryptoRandom` appears in
`EmulatedKernel.fs` only in the `NonCryptoRandomState` / `CryptoRandomState`
fields and their initialisation, and not at all in the platform-profile block
that stage 3 moves. The commissioned spike therefore leaves it exactly where it
is.

## Target shape

```fsharp
namespace WoofWare.PosixKernel

/// One scheduling entity as the kernel sees it: what `gettid(2)` names.
/// PawPrint's `ThreadId` maps onto this at task registration.
type TaskId = TaskId of int

type UnixMachineState =
    { Platform : SimulatedUnixPlatform          // facts true of this kernel image
      FileSystem : VirtualFileSystem
      FileSystemType : EmulatedFileSystemType   // a mount fact
      Clock : UnixClock                         // VirtualClockTicks, WallClockEpochMs
      Entropy : UnixEntropy                     // NonCryptoRandomState, CryptoRandomState
      Network : UnixNetworkConfiguration        // LocalAddresses, LocalRoutes,
                                                // EphemeralPortRange, NextEphemeralPort, SoMaxConn
      Sockets : SocketTable                     // Sockets, Connections, and their id counters
      ProcessorCount : int
      UserAddressLimit : uint64 }               // TASK_SIZE_MAX

type UnixProcessState<'Handler> =
    { Environment : Map<string, string>
      CurrentDirectory : AbsoluteUnixPath
      CurrentDirectoryInode : InodeNumber
      ProcessPath : AbsoluteUnixPath option
      Umask : PermissionBits
      Identity : UnixCredentials                // UserId, GroupId
      FileDescriptors : FileDescriptorRegistry
      DirectoryStreams : Map<DirectoryStreamId, DirectoryStream>
      Signals : SignalState<'Handler>           // dispositions, enabled set, pending queue
      StandardStreams : ImmutableArray<OutputLogEntry> }

type UnixTaskState =
    { Errno : int
      Cpu : CpuId
      OsThreadId : OsThreadId
      BlockedSignals : Set<Signal>
      Parked : ParkedSyscall option }
```

`CpuId` and `OsThreadId` move across from `AbstractMachineDomain.fs`: both are
values the *guest* reads from the kernel (`sched_getcpu`, `gettid`), and
`EmulatedKernel.cpuForRotation` / `osThreadId` are already their only
producers. `ThreadId` stays in PawPrint; the mapping `ThreadId -> TaskId` is
established once, at thread creation.

`DirectoryStreamId` is new: today `DirectoryStreams` is keyed by
`NativeMemoryBlockId`, which is PawPrint's identity for the block backing the
guest's `DIR*`. The library mints an opaque id; the client decides how to
represent it to its guest.

### What stays in PawPrint, and why

Each of these fails the test "would a POSIX kernel have this?":

| stays | because |
| --- | --- |
| `LowLevelMonitors`, `WaitHandles`, `SemaphoreState`, `MutexState`, `EventState` | CoreCLR PAL sync objects and .NET `WaitHandle`s. A POSIX kernel owns futexes, not these. If PawPrint ever interprets the real pthread PAL, `futex` becomes a library syscall and these become its client-side users |
| `NativeMemoryPool` | the `malloc` arena is libc, not kernel: PawPrint models no `brk`/`mmap` |
| `SpuriousWakeup`, `SyncBlockSpuriousWakeup`, `ClockJitter`, `StepCounter`, `InstructionCostTicks` | determinism-*search* strategies over PawPrint's own execution. `InstructionCostTicks` in particular is a client policy ("this machine runs at 10 MIPS"); the library exposes `Clock.advance : ticks -> …` and the client decides how many |
| `OptimalMaxSpinWaitsPerSpinIteration` | reaches the guest through an internal `System.Threading.Thread` property |
| `NextEventPipeId` | .NET diagnostics |
| `LastPInvokeError` | `Marshal.GetLastPInvokeError` is a CLR per-thread slot written by `SetLastError=true` marshalling. Distinct from `LastSystemError`, which *is* errno and does move |

## Settled

* **Packaging.** `WoofWare.PosixKernel` is **not** `IsPackable`. Publishing
  before the syscall layer exists would commit us to the state-record shape as
  a public API, and the state records are the part stages 5–6 are expected to
  rearrange.
* **`.fsi` discipline.** None for now. `.fsi` files are the only thing that
  actually hides an F# representation within an assembly, so the syscall layer
  gets them when it is built (stages 7–9); writing them for 14k moved lines
  would turn rename-only stages into rewrites and destroy their oracle.
* **The shared PRNG.** Its own project — see decision 6. Not needed before
  stage 5.

## Still open

* **Where the `emulated-posix-kernel` skill lives.** It documents the new
  library, and its measured divergence tables are the most expensive thing in
  this area. Recommendation: it stays at `.claude/skills/` in this repo (the
  library is not a separate repository), with its file paths updated in stage 3
  — the divergence tables and the platform profile move together, so that is
  the moment its paths go stale.

## Implementation plan

Implement this plan with each stage on its own branch, stacked as necessary on
previous branches, so that a reviewer can review each branch in isolation.

The move stages (1, 2, 4, 5, 6) share an oracle worth stating once, because it
is what makes a 6,000-line diff reviewable: **a rename-only check**. Strip
`namespace` and `open` lines from both sides and diff; the result must be
empty. Write that check as a script in stage 1 and reuse it. `git diff -M`
should show renames, not deletions plus additions.

Every stage's baseline oracle is the full suite, both halves:

```
nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj
nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "TestCategory=Guest"
```

plus the new `WoofWare.PosixKernel.Test` project from stage 1 onward.

---

### Stage 1: project skeleton, and the path/errno vocabulary

**Dependencies**: none.

**Implements**: "What is already true".

Create `WoofWare.PosixKernel/` (net8.0, `IsPackable=false`) and
`WoofWare.PosixKernel.Test/`; add both to `WoofWare.PawPrint.slnx` and to
`.github/workflows/ci.yaml`. `WoofWare.PawPrint` references the new project.
Move, with namespace change only: `UnixPathText.fs`, `AbsoluteUnixPath.fs`,
`UnixPath.fs`, `UnixError.fs` (1,407 lines) and their tests
(`TestAbsoluteUnixPath.fs`, `TestUnixPath.fs`, `TestPathCursor.fs`,
`TestUnixError.fs`). Add `open WoofWare.PosixKernel` to every referencing file.

**Correctness oracle**:
* The rename-only script reports an empty diff for every moved file.
* Both halves of the existing suite are green and the test counts are
  unchanged except for the tests that moved projects; those counts must sum to
  the old total.
* `WoofWare.PosixKernel.dll` has no reference to `WoofWare.PawPrint.Domain`
  (assert this, in CI: it is the invariant the whole plan rests on, and it is
  cheap to check with `ikdasm`/`System.Reflection.Metadata`).

### Stage 2: the CLR-free state modules

**Dependencies**: stage 1.

Move `VirtualFileSystem.fs`, `FileSystemSeed.fs`, `InternetEndpoint.fs`,
`FileDescriptorRegistry.fs`, `Signal.fs` (5,433 lines) and the tests that do
not reference `SimulatedUnixPlatform` (`TestVirtualFileSystem.fs`,
`TestFileSystemSeed.fs`, `TestFileDescriptorRegistry.fs`, `TestSignal.fs`).
Tests that *do* reference the platform (`TestVirtualFileSystemAgainstHost.fs`,
`TestSocketBinding.fs`, `TestSocketCreation.fs`) stay put until stage 3.

**Correctness oracle**: as stage 1. After this stage every CLR-free file has
moved, and `WoofWare.PosixKernel` is 6,840 lines with no PawPrint dependency.

### Stage 3: the Unix platform profile

**Dependencies**: stage 2.

Measured: lines 559–1913 of `EmulatedKernel.fs` reference **nothing** defined
outside that range except in five doc comments and error-message strings
(`KernelConfig.FileSystemType`, `EmulatedKernel.UserAddressLimit`,
`EmulatedKernel.withUnixPlatformAndFileSystemType`). So this block, too, is a
rename — but those five cross-references will need their names updating, so run
the rename-only check *first* and land the comment edits as a separate,
reviewable commit on the same branch.

Move lines ~559–1913 of `EmulatedKernel.fs` — `SimulatedUnixFlavour`,
`SimulatedUnixPlatform`, `EmulatedFileSystemType`, `SimulatedUnixReleaseError`,
`UserBufferCheck`, `ObservedUserAddressLimit`, `LinuxEpollLimits`,
`SocketAddressSizes`, `SockaddrFamilyField`, `SocketCreationRefusal`,
`CreatingOpenRules`, `MkDirRules`, `UnlinkRules`, `RmDirRules`,
`OpenDirRules`, `BindLengthVerdict`, `BindFault` — into
`WoofWare.PosixKernel/SimulatedUnixPlatform.fs` (~1,350 lines). This block is
already CLR-free and already self-contained; it just happens to live in the
same file as things that are not.

Move the host-equality tests with it:
`TestVirtualFileSystemAgainstHost.fs`, `TestOpenDirRules.fs`,
`TestRmDirRules.fs`, `TestFileSystemType.fs`, `TestLinuxEpollLimits.fs`,
`TestSocketBinding.fs`, `TestSocketCreation.fs`.

**Correctness oracle**:
* The rename-only script, on the move commit.
* The host-equality tests are the real oracle here and they must pass **on both
  platforms**: locally on macOS and in CI on Linux. Each falsifies a different
  column of the divergence tables, so a green run on one is half a result.
* `.claude/skills/emulated-posix-kernel/` and its `reference/` files name
  `EmulatedKernel.fs` throughout. Update the paths in this stage; the skill is
  the only durable record of how those constants were measured, and a skill
  that points at a file which no longer holds the thing it describes is worse
  than no skill.

### Stage 3 is the spike's exit

The question the commissioned work answers: **is the boundary real?** Concretely,
after stage 3:

* `WoofWare.PosixKernel.dll` builds and its tests pass with no reference to
  `WoofWare.PawPrint.Domain` or `WoofWare.PawPrint` — asserted in CI, not
  eyeballed.
* 8,190 of the 14,135 candidate lines have moved without a behavioural diff,
  and the rename-only check says so mechanically.
* The host-equality suite — the most expensive-to-establish thing in this area —
  passes from the far side of the boundary on both macOS and Linux.

If any of those fails, the finding is worth more than the move: it means a CLR
concept is reaching the POSIX model somewhere the type names did not reveal,
and stages 4–9 need re-costing before anyone starts them.

---

*Stages 4–9 below are not commissioned. They are recorded so that stage 3's
result can be judged against where it was heading; re-read and re-cost them
before starting any of them.*

### Stage 4: `SignalState` genericised

**Dependencies**: stage 2.

Parameterise `SignalState` over `'Handler` (decision 5); move
`SignalInitState`'s dispatcher `ThreadId` out to PawPrint, leaving an
initialised bit. Move `SignalState.fs` and `TestSignalState.fs`,
`TestSignalHandler.fs`. `SignalDispatch.fs`, `TestSignalDispatch.fs`,
`TestSignalDispatcherThread.fs`, `TestSignalTermination.fs` stay in PawPrint —
they are about the dispatcher thread, which is PawPrint's.

**Correctness oracle**:
* `TestSignalState.fs`'s existing property tests against a store-everything
  oracle, unchanged, at `'Handler = SignalHandler`.
* A new instantiation at `'Handler = int` in the library's own test project,
  running the same properties. This is what proves the generic parameter is
  genuinely a parameter rather than a `SignalHandler` in disguise.
* `TestSignalDispatcherThread.fs` green: the dispatcher identity moved and the
  "dispatcher exists iff initialised" invariant must survive the move.

### Stage 5: introduce `UnixSystem` inside PawPrint

**Dependencies**: stages 3 and 4.

**Implements**: decision 4.

Do the state split *before* moving it, so that every step is compiler-checked
against the existing 4,450-test suite. Add `Unix : UnixSystem` to
`EmulatedKernel` and migrate the fields one group at a time, keeping the old
`EmulatedKernel` accessors as forwarding members so no call site changes in
this stage:

1. machine: `FileSystem`, `FileSystemType`, `UnixPlatform`,
   `VirtualClockTicks`, `WallClockEpochMs`, `NonCryptoRandomState`,
   `CryptoRandomState`, `ProcessorCount`, `UserAddressLimit`, the network
   fields, the socket table;
2. process: `Environment`, `CurrentDirectory`, `CurrentDirectoryInode`,
   `ProcessPath`, `Umask`, `UserId`, `GroupId`, `FileDescriptors`,
   `DirectoryStreams` (rekeyed to `DirectoryStreamId`), `Signals`,
   `OutputLog`;
3. tasks: `LastSystemError`, `ParkedSocketWaits`, and `Cpu`/`OsThreadId`
   pulled off `ThreadState`.

`KernelConfig` splits along the same lines but keeps its current surface, so
`HostConfig` and every test registration are untouched.

**Correctness oracle**:
* The full suite after each of the three sub-steps.
* **A new test that the task set is exactly the live threads** — created on
  thread creation, removed on thread exit. This is the obligation that replaces
  the "never store a default" canonicalisation (decision 4), and without it a
  leaked task record is a determinism bug no existing test can see. Mutation-test
  it: delete the removal, and the test must go red.
* The `SocketFuzz` differential fuzzer, which drives the socket model directly
  against real Linux epoll in a container, is the sharpest available check that
  the socket-table regrouping preserved behaviour. Run it.

### Stage 6: move `UnixSystem` to the library

**Dependencies**: stage 5.

Move `UnixSystem` and its operations across. `EmulatedKernel` keeps only the
CLR-side fields listed in "What stays in PawPrint", plus `Unix : UnixSystem`.
Delete the forwarding accessors from stage 5 and update the ~180 call sites in
`NativeSystemNative.fs`.

**Correctness oracle**: the full suite, plus the CI assertion from stage 1 that
the library references nothing of PawPrint's. At this point the goal's second
bullet is met and the third is met; the first is met for state but not yet for
transitions.

### Stage 7: the syscall request layer, on the pure syscalls first

**Dependencies**: stage 6.

**Implements**: decisions 1(b), 2(b).

Introduce `Syscall`, `SyscallOutcome`, `SyscallEffect`, `UserBuffer`, and
`UnixSystem.step`. Hoist the syscalls that carry no buffer and cannot block:
`getcwd`, `getuid`, `geteuid`, `umask`, `chdir`, `unlink`, `rmdir`, `mkdir`,
`dup`, `close`, `flock`, `lseek`, `ftruncate`. `NativeSystemNative`'s handlers
for these shrink to PAL decode, `step`, encode.

Sad paths first, per the ordering guidance: the errno rows for each of these
are simpler than the success path and exercise the whole request/response
shape.

**Correctness oracle**:
* A new differential test in `WoofWare.PosixKernel.Test` that drives `step`
  directly and compares against the host, extending
  `TestVirtualFileSystemAgainstHost`'s existing technique one altitude up. This
  is the first test that exercises the library as a client would.
* The existing guest fixtures, unchanged — the whole point is that no guest can
  tell.
* Mutation test the effect *ordering*: swap `SetErrno` and `WriteBytes` in one
  response and confirm a test goes red. If none does, the ordering is not yet
  covered and a test for it is part of this stage.

### Stage 8: buffer-carrying syscalls

**Dependencies**: stage 7.

`open`, `read`, `write`, `pread`, `pwrite`, `stat`, `lstat`, `fstat`,
`readlink`, `opendir`, `readdir`, `closedir`, and the socket-address entry
points. Each is one increment and can land individually.

**Correctness oracle**: per syscall, the host-differential test at the new
altitude, plus the existing guest fixtures. `TestGuestPathBytes.fs` and the
`ENAMETOOLONG`/`EFAULT` rows are the interesting cases: they are exactly where
the `UserBuffer` three-way classification earns its place, and each of the
three cases needs a row that only it can satisfy.

### Stage 9: blocking syscalls, and packaging

**Dependencies**: stage 8.

**Implements**: decision 3.

`WaitForSocketEvents`, `poll`, `accept`, `connect`: `WouldBlock of
WakeCondition` plus `WakeCondition.isSatisfied`. PawPrint's `Program` readiness
sweep becomes a poll of that predicate. Then: `README`, the
`emulated-posix-kernel` skill's paths, `docs/divergences.md`, and the
packaging decision from the open questions.

**Correctness oracle**:
* `SocketFuzz` against real Linux epoll, driven through `step` rather than
  through the model functions — the strongest oracle in the repo for this
  area, now pointed at the public surface.
* `TestSocketEventsWait.fs`, `TestSocketEventsWaitReason.fs` and the
  `Guest`-category socket fixtures.
* A property: for every `WouldBlock` outcome, `isSatisfied` on the returned
  condition is `false` in the state that produced it. A wake condition already
  satisfied at the moment of parking is a lost wakeup, and it is the failure
  mode this stage can most easily introduce.
