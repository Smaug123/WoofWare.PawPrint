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
the 9,650 lines that are already free of every CLR concept, and they change no
behaviour. What they are meant to establish is whether the boundary is real —
whether `WoofWare.PosixKernel` can build and test with no reference to
`WoofWare.PawPrint.Domain`, and whether the host-equality tests still pass on
both platforms once they live on the other side of it. Stages 4–9 are written
down here because the spike is only worth running if there is somewhere for it
to go, but they are **not commissioned**: revisit them once stage 3 has
answered its question.

## Spike result (2026-08-23)

All three commissioned stages are done. Measured on `dc341c58`:

| | |
| --- | --- |
| lines moved into `WoofWare.PosixKernel` | 9,652 across 10 files |
| project references from the library | **none** (asserted by `TestNoPawPrintReference.fs`) |
| test files now on the library side | 14, including the 27-case host-equality suite |
| test total | 3,056 + 448 = 3,504, against a 3,502 baseline, the difference being two new tests |
| behavioural diff | none; `scripts/check-move-is-rename-only.sh` verifies every move, and the split |
| guest fixtures | 1,011 passing, unchanged, after each of the three stages |

Answering the question the spike was for: **the boundary is real.** Nothing in
the moved 9,652 lines needed a CLR concept, and the one place PawPrint was
reaching through an abstraction — `EmulatedKernel.checkInvariants` reading
`FileDescriptorRegistry`'s private `Descriptions` — was predicted before the
move and fixed with an accessor that already existed.

Two findings worth carrying forward:

* **The assembly boundary now enforces twelve private representations** that
  were previously respected only by discipline. That is the extraction's first
  concrete benefit and it arrived before any of the syscall work.
* **Stage 3 doubled in size between the plan being written and being executed**,
  because #1153 landed `RenameRules` inside the block. Specifying by definition
  name rather than line range is what made that a non-event. Any future stage
  touching this block should assume the same.

**What the spike also found, which the plan did not anticipate.** The boundary
is real for *state* but not yet for *vocabulary*. Seventeen definitions in the
library speak CoreCLR's PAL encodings rather than POSIX ones; the exact set is
`scripts/pal-residue-allowlist.txt`, and the `pal-residue` flake check keeps it
exact.

They are genuinely misplaced — a second, non-.NET client would have to learn
`Interop.Sys.AddressFamily` to call `socketCreation` — and they are nearly all
leaves, consumed almost entirely by `Native/NativeSystemNative.fs` (49 call
sites for `toPal` alone, 1–3 for each of the others). Stage 7 moves them; stage
3.5 below contains them until then.

They were *not* fixed inside stages 1–3, deliberately: doing so would have meant
non-rename edits to five moved files and re-pointing sixty call sites, which
would have destroyed the rename-only oracle that is the only reason a
9,650-line diff is reviewable at all. The claim in `AGENTS.md` and the package
README was corrected to state the gap instead.

### Stage 3.5: contain the PAL residue — **decided: (b), done**

**Dependencies**: stage 3.

The residue is not homogeneous, which is what made this a choice rather than a
chore:

| group | functions | content |
| --- | --- | --- |
| managed-enum mapping | `Signal.ofPosixSignalEnum`, `toPosixSignalEnum` | pure `System.Runtime.InteropServices.PosixSignal` values; no kernel content whatever |
| PAL numbering of a POSIX concept | `UnixError.toPal`, `palOfRawErrno`, `palOfRawErrnoUnder` | one table carrying `.Raw` (POSIX) and `.Pal` (CoreCLR) together |
| PAL bit mask | `SocketEventInterest.ofBits` | the PAL's `SocketEvents` bits, plus its EINVAL screen |
| PAL↔platform numbering | `addressFamilyPalToPlatform`, `addressFamilyPlatformToPal` | transcriptions of `TryConvertAddressFamily*` |
| **mixed** | `socketCreation` | the shim's three screens **and** this kernel's own declared protocol table, in one function |

`PollEvents.ofBits`/`toBits` were on an earlier draft of this list and should not
be: `POLLIN`…`POLLNVAL` are 0x01…0x20 in .NET's `Interop.Poll.Structs.cs`, in
Linux's `<poll.h>` (measured, arm64 container) and in Darwin's, so those are
POSIX values with a PAL-flavoured docstring. Fixed the docstring, not the code.

**(a) Split every table at the boundary, now.** The library keeps the POSIX
values; PawPrint gains a mirror table for the PAL ones. Immediate, and
independent of everything downstream. The cost is two exhaustive matches over
`UnixError` that the compiler will keep *exhaustive* but cannot keep *in
agreement*, and — the real problem — it forces a decision on `socketCreation`'s
mixture before there is anything to decide it against.

**(b) Defer the whole thing to stage 7.** `Syscall`/`SyscallOutcome` carry POSIX
values by construction, so PawPrint's translator is where PAL naturally lives,
and these eleven then move as part of a change that must touch them anyway. No
work is done twice and the adapter is designed once, against a real interface.
The cost is that the library's public API speaks PAL until stage 7 lands —
indefinitely, if this stalls.

**(c) Split the eight clean ones now, defer `socketCreation`.** Attractive until
you notice `socketCreation` *calls* `addressFamilyPalToPlatform`, so those two
have to stay behind with it or be duplicated. Leaves the residue at three rather
than eleven, at the price of a half-migrated boundary — the state the gospel
warns about specifically.

**(d) Parameterise the library over its own encoding**, so PawPrint supplies the
PAL numbering as data. Rejected: the PAL is one client's encoding, not an axis
the library varies along, and nothing else would use the generality.

**Chosen: (b), plus the containment measure borrowed from (a).** The move waits
for stage 7, where `Syscall`/`SyscallOutcome` give the adapter a real interface
to be designed against. What landed now is the assertion: an allowlist that
pins the residue exactly, may shrink, and may not grow. The failure mode here is
accretion rather than any single function, so the assertion is worth more than
the move, and it costs nothing that stage 7 would redo.

**Where it lives.** `scripts/check-pal-residue.py` plus
`scripts/pal-residue-allowlist.txt`, run as the `pal-residue` flake check, which
CI's existing `flake-check` job already covers. The alternative was an NUnit
test in `WoofWare.PosixKernel.Test`, which would reach every developer's
`dotnet test` rather than only `nix flake check` — but no test in this repo
reads the source tree, and one that did would have to find it from
`bin/Release/net10.0/`. A flake check is the idiom the repo already has for
asserting a fact about the sources (`runtime-version-pin`), so this follows it.

**How a definition is detected.** By its name (`toPal`, `palOfRawErrno`,
`ofPosixSignalEnum`) or by its body mentioning a PAL encoding (`Pal.`, `.Pal`,
`SocketEvents`, `PosixSignal`), with comments excluded so that a docstring
citing the PAL to explain a POSIX value's provenance is not a hit. That is a
proxy and not a proof — a PAL-encoded `int` is indistinguishable from any other
`int` — but the PAL constants live in one `module private Pal`, so an adapter
essentially cannot be written without tripping it.

**The detector corrected the census in both directions**, which is the argument
for having written it rather than curating the list by hand. Off: `PollEvents`,
as above. On: `SimulatedUnixPlatform.isTcpProtocolType` (its parameter is
literally named `palProtocolType`), the `module private Pal` constant table,
`UnixErrorNumbering.Pal` and its two private constructors, the
`palSuccess`/`palNonStandard` sentinels, and — found by Codex's review of the
detector rather than by the detector — `UnixError.numbering`, the 200-row
`Interop.Error` table itself, whose rows are numeric literals and so tripped no
token. Eleven was an undercount reached by reading; seventeen is what is there.

**What the detector deliberately does not see**, all found by Codex across two
review rounds: PAL vocabulary arriving as a union case (`| ToPal of int`), a
`static member`, or an instance member (`member _.ToPal`, whose name the parser
does not extract); and a definition that merely delegates to an allowlisted one
(`let managedError e = UnixError.toPal e`), which would need a transitive
closure over call sites. It also sees one entry for a weak reason:
`SocketEventInterest.ofBits` is recognised only by the word `SocketEvents` in
its failure message, its body being bare hex masks — so rewording that message
reports the entry stale rather than retiring the conversion. Measured, not
assumed.

None of these is closed. The library is module-and-`let` throughout, and this
work stream finishes before other development resumes, so accretion would have
to arrive in a form nothing here uses. What the check must not do is *claim*
more than it enforces, so the script's header states each gap, and the stale
message tells the reader that a vanished entry may mean a blind detector rather
than a retired conversion.

**Correctness oracle**: the full suite including `Guest`, unchanged; plus the
check itself, mutation-tested. Seven mutants, all killed:

| mutant | caught by |
| --- | --- |
| `palOfSomethingNew` | name rule |
| `let isUdpProtocolType (t : int) : bool = t = Pal.PtUdp` | body rule — the mutant that matters, since a name-only rule would miss `socketCreation` too |
| `convertToPalValue` | name rule, camelCase boundary |
| a second errno table, `portable 0x10042 1` rows, no PAL token | table-row rule |
| the same, in a new `WoofWare.PosixKernel/Sub/` | recursive scan |
| an allowlisted function that stops speaking PAL | stale detection, so the list cannot rot |
| unmutated control | passes |

The second was also run end-to-end through `nix flake check`, to confirm the
derivation fails rather than passing quietly.

The last two mutants were added after the first battery passed, and both found
real defects: the scan was non-recursive, and the name rule required `pal` at a
word boundary, so a camelCase `toPalSomething` slipped through. The first
battery had used a leading-`pal` name and so could not see it. Twelve name
probes now pin the boundary in both directions (`Palette`, `principal`,
`palindrome` must not match).

One thing outstanding before the spike is fully closed: the host-equality suite
has been verified from the far side of the boundary **on macOS only**. It
falsifies a different column on Linux, so CI's run is the other half of that
result.

The guest fixtures were run after each stage and were 1,011 passing every time,
including after stage 3 — the run that matters most, since stage 3 is the only
one that reorganised a file's contents rather than relocating whole files.

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

**Chosen: (c).** Note first that (a) and (c) are *equally* breaking to
`Domain` — both delete a public module from it — so the breaking change is not
what separates them, and the argument for (c) has to stand on dependency shape
alone. It does: the two consumers are doing genuinely different things with
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

* **Packaging.** `WoofWare.PosixKernel` **is** `IsPackable`, with its own
  `PackageId`, following the `WoofWare.PawPrint.Domain` precedent — `Domain` is
  a `ProjectReference` of `WoofWare.PawPrint`, is packable, and CI uploads both
  nupkgs.

  This overturns an earlier "not packable" decision, which was made without the
  following, established by probe rather than from memory: a `ProjectReference`
  to an `IsPackable=false` project emits `<dependency id="…" version="1.0.0"/>`
  into the referencing package's nuspec **and omits the referenced assembly from
  `lib/`**, silently and with no warning. Since `WoofWare.PawPrint` is packable
  and `nuget-pack`/`expected-pack` are required checks that only count nupkg
  *files*, a non-packable `WoofWare.PosixKernel` would have made the published
  PawPrint package depend on a package that does not exist and ship without the
  assembly, with CI green throughout.

  The cost this incurs — the state records become public surface — is smaller
  than it looks while the publish jobs remain `if: false`: nothing reaches
  nuget.org today, so what is committed to is the *shape of the package graph*,
  not an API contract with users.

* **`.fsi` discipline.** None for now. `.fsi` files are the only thing that
  actually hides an F# representation within an assembly, so the syscall layer
  gets them when it is built (stages 7–9); writing them for 9,650 moved lines
  would turn rename-only stages into rewrites and destroy their oracle.

* **The shared PRNG.** Its own project — see decision 6. Not needed before
  stage 5.

## What the assembly boundary starts enforcing

F# `private` on a record or single-case union declared in a *namespace* is
assembly-scoped: it does not hide the representation from other files in the
same assembly, but it does hide it across an assembly boundary. Twelve of the
moving types have private representations (`AbsoluteUnixPath`, `FileName`,
`UnixPath`, `PathCursor`, `SymlinkTarget`, `PermissionBits`, `UnixTimestamp`,
`VirtualFileSystem`, `PathLimits`, `FileDescriptorRegistry`, `SignalState`,
`SignalHandler`), so anything in PawPrint reaching into one becomes `FS1093`
the moment it is a different assembly.

Measured across `WoofWare.PawPrint`, `.App` and `.Test`, for every constructor,
pattern match and field access outside the defining file: **there is exactly
one**, `EmulatedKernel.fs`'s `checkInvariants` reading
`kernel.FileDescriptors.Descriptions`. `FileDescriptorRegistry.descriptions`
already exists as a public accessor, so it is a one-line change in stage 2.

That is a good result for this plan's premise. The private representations were
already being respected by discipline; the assembly boundary will now enforce
what discipline was achieving, which is the machine-checks-invariants principle
applied to a module boundary.

## Still open

* **Where the `emulated-posix-kernel` skill lives.** It documents the new
  library, and its measured divergence tables are the most expensive thing in
  this area. Recommendation: it stays at `.claude/skills/` in this repo (the
  library is not a separate repository), with its file paths updated in stage 3
  — the divergence tables and the platform profile move together, so that is
  the moment its paths go stale.
* **Two test files worth splitting, deferred.** `TestAbsoluteUnixPath.fs` has
  four cases that forge an `EmulatedKernel`, and `TestFileSystemSeed.fs` has
  eight that call `RealRuntime.validateSeedForOracle`. In both, the minority is
  genuinely a PawPrint test and the majority is genuinely a library test.
  Splitting them is the right end state but is not rename-only, so the spike
  leaves both whole in `WoofWare.PawPrint.Test`.

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

### How the test files were assigned

The first draft of this plan assigned test files by assuming they followed
their implementation files. They do not, and four of the assignments were
wrong. Assignment is therefore by grep over each *test* file's own code
references (comments stripped), against two lists: names that will never be on
the library side (`EmulatedKernel`, `KernelConfig`, `RealRuntime`,
`HostPlatform`, `IlMachineState`, `NativeSystemNative`, …), and
`SimulatedUnixPlatform`, which arrives only in stage 3.

| test file | blockers | needs platform | lands in |
| --- | --- | --- | --- |
| `TestUnixError.fs` | — | no | stage 1 |
| `TestUnixPath.fs` | — | no | stage 1 |
| `TestFileDescriptorRegistry.fs` | — | no | stage 2 |
| `TestSignal.fs` | — | no | stage 2 |
| `TestDirectoryEnumeration.fs` | — | no | stage 2 |
| `TestPathCursor.fs` | — | yes | stage 3 |
| `TestVirtualFileSystem.fs` | — | yes | stage 3 |
| `TestVirtualFileSystemAgainstHost.fs` | — | yes | stage 3 |
| `TestOpenDirRules.fs` | — | yes | stage 3 |
| `TestRmDirRules.fs` | — | yes | stage 3 |
| `TestUnlinkRules.fs` | — | yes | stage 3 |
| `TestRenameRules.fs` | — | yes | stage 3 |
| `TestLinuxEpollLimits.fs` | — | yes | stage 3 |
| `TestAbsoluteUnixPath.fs` | `EmulatedKernel`, `KernelConfig` | yes | stays |
| `TestFileSystemSeed.fs` | `RealRuntime` | yes | stays |
| `TestFileSystemType.fs` | `EmulatedKernel`, `HostPlatform` | yes | stays |
| `TestSocketBinding.fs` | `EmulatedKernel` | yes | stays |
| `TestUserBufferCheck.fs` | `EmulatedKernel` | yes | stays |
| `TestUserBufferCheckAgainstHost.fs` | `EmulatedKernel`, `HostPlatform` | yes | stays |
| `TestGuestPathBytes.fs` | `NativeSystemNative` | yes | stays |
| `TestSignalHandler.fs` | `IlMachineState`, `MethodInfo` | no | stage 4 |
| `TestSignalState.fs` | — | no | stage 4 |
| `TestSocketCreation.fs` | embedded resource, see below | yes | stays |

A test staying behind is not a problem: `WoofWare.PawPrint.Test` references the
new library, so every one of these keeps testing the moved code. Moving a test
buys one thing only — that the library is tested *without* a PawPrint
reference — and the thirteen that do move include the host-equality suite,
which is the expensive one.

`TestSocketCreation.fs` is the one commissioned file that is not a rename: it
reads embedded resources through `Assembly.GetExecutingAssembly()` with the
hard-coded logical name `"WoofWare.PawPrint.Test.socketMatrix.%s"`, so moving it
also means moving the `<EmbeddedResource Include="socketMatrix\*.tsv" />` item
and editing that string. It stays behind in the spike. Assembly-identity-
dependent code is a class the rename-only check cannot see; this is the only
instance among the movers, and the check's documentation should say so.

---

### Stage 1: project skeleton, and the path/errno vocabulary

**Dependencies**: none.

**Implements**: "What is already true"; the packaging decision.

Create `WoofWare.PosixKernel/` (net8.0, `IsPackable=true`, `PackageId`
`WoofWare.PosixKernel`) and `WoofWare.PosixKernel.Test/` (net10.0 — the devshell
carries only the net10 runtime, so a net8.0 test host will not run; NUnit 4.4.0,
NUnit3TestAdapter 5.1.0, FsUnit 7.1.1, FsCheck(.NUnit) 3.3.2,
Microsoft.NET.Test.Sdk 17.14.1, to match the existing test project). Add both to
`WoofWare.PawPrint.slnx`; add a `WoofWare.PosixKernel` nupkg upload to the
`nuget-pack` job. `WoofWare.PawPrint` references the new project.

The new package gets its **own** `version.json` and its **own** `README.md`,
rather than sharing the repository root's. Both existing packages carry a
`version.json` whose `pathFilters` decide which tree changes bump that
package's version — so a shared file would tie the new package's version to
PawPrint's churn, which is exactly the coupling the extraction is trying to
remove. `WoofWare.PawPrint`'s own filters already list
`:/WoofWare.PawPrint.Domain` because it depends on it; add
`:/WoofWare.PosixKernel` for the same reason. The `README.md` is separate
because it is the package's front page on nuget.org and the repository root's
describes an IL interpreter, which this library is not.

Move, in compile order and with namespace change only: `UnixError.fs`,
`UnixPathText.fs`, `AbsoluteUnixPath.fs`, `UnixPath.fs` (1,407 lines), and the
tests `TestUnixError.fs`, `TestUnixPath.fs`. Add `open WoofWare.PosixKernel` to
every referencing file.

**Correctness oracle**:
* The rename-only script reports an empty diff for every moved file.
* The default suite is green and its total is the 3,502-test baseline minus
  exactly the tests that moved projects; the two projects' totals must sum to
  3,502. The `Guest` half is unchanged.
* **Check the empty-filter case before landing**: CI's second step is
  `dotnet test --filter "TestCategory=Guest"`, and `WoofWare.PosixKernel.Test`
  contains no `Guest`-category test at all. Confirm VSTest treats "no test
  matches" in one assembly of a solution-level run as a warning rather than a
  failure. If it does not, that is a stage-1 blocker and needs a filter change,
  not a workaround later.
* `WoofWare.PosixKernel.dll` references neither `WoofWare.PawPrint` nor
  `WoofWare.PawPrint.Domain` — asserted in CI, because it is the invariant the
  whole plan rests on and it is cheap to check with `System.Reflection.Metadata`.

### Stage 2: the CLR-free state modules

**Dependencies**: stage 1.

Move `VirtualFileSystem.fs`, `FileSystemSeed.fs`, `InternetEndpoint.fs`,
`FileDescriptorRegistry.fs`, `Signal.fs` (5,433 lines) and the tests
`TestFileDescriptorRegistry.fs`, `TestSignal.fs`, `TestDirectoryEnumeration.fs`.

One non-rename edit, in its own commit: `EmulatedKernel.checkInvariants` reads
`kernel.FileDescriptors.Descriptions`, which the assembly boundary now hides.
Replace with the existing public accessor
`FileDescriptorRegistry.descriptions`.

**Correctness oracle**: as stage 1. The compiler is the real oracle for the
private-representation question — if more than the one predicted `FS1093`
appears, the measurement in "What the assembly boundary starts enforcing" was
wrong and the surplus needs understanding rather than papering over with
accessors.

### Stage 3: the Unix platform profile

**Dependencies**: stage 2.

**Specify this stage by definition name, never by line range.** The first draft
named lines 559–1913, and commit #1153 landed `RenameRules` inside that window
between the measurement and the commit, silently moving every boundary. The
block is 18% of recent commits by volume; assume it will move again.

Move into `WoofWare.PosixKernel/SimulatedUnixPlatform.fs`, from
`EmulatedKernel.fs`: `SimulatedUnixFlavour`, `EmulatedFileSystemType`,
`FileSystemTypeAnswer`, `SimulatedUnixReleaseError`, `UserBufferCheck`,
`ObservedUserAddressLimit`, `LinuxEpollLimits`, `SimulatedUnixPlatform`,
`SocketAddressSizes`, `SockaddrFamilyField`, `SocketCreationRefusal`,
`CreatingOpenRules`/`CreatingOpenVerdict`, `MkDirRules`/`MkDirVerdict`,
`UnlinkRules`/`UnlinkVerdict`, `RemovalChecks`, `DirectoryEntryNameLength`,
`GetCwdOrphanAnswer`, `RmDirRules`/`RmDirVerdict`, `OpenDirRules`/
`OpenDirVerdict`, `RenameRules`/`RenameVerdict`/`RenameChecks`,
`BindLengthVerdict`, `BindFault`, **and `module SimulatedUnixPlatform`**.

That last is not optional and the first draft omitted it. The module holds every
flavour derivation — `linuxX64`, `macOsArm64`, `pathLimits`,
`creatingOpenRules`, `rawErrnoNumbering`, `socketCreation`, the bind rules — so
it *is* the divergence tables, and six of the eight moving tests call into it.
Leaving it behind would also split `type SimulatedUnixPlatform` from `module
SimulatedUnixPlatform` across two namespaces, making resolution in any file
opening both depend on `open` order. Verified CLR-free along with the rest of
the block.

Measured on `dc341c58`: this block references nothing defined outside it except
in nine doc comments and one interpolated error string, all naming
`EmulatedKernel.*` or `KernelConfig.*`. Land the move first, then the comment
rewrites as a separate commit — and rewrite them into *library* vocabulary
rather than merely repointing the names. The error string in
`EmulatedFileSystemType.reportedFor` currently instructs its caller to use
`EmulatedKernel.withUnixPlatformAndFileSystemType`, which after the move is
client vocabulary inside a library that has no idea its client has such a
function.

Move the tests: `TestPathCursor.fs`, `TestVirtualFileSystem.fs`,
`TestVirtualFileSystemAgainstHost.fs`, `TestOpenDirRules.fs`,
`TestRmDirRules.fs`, `TestUnlinkRules.fs`, `TestRenameRules.fs`,
`TestLinuxEpollLimits.fs`.

**Correctness oracle**:
* The rename-only script, on the move commit — but note this stage is a
  **split**, not a rename: `EmulatedKernel.fs` keeps its name, so `git diff -M`
  detects nothing. The check must diff the old file against
  (remainder + extracted file) explicitly.
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
  `WoofWare.PawPrint` or `WoofWare.PawPrint.Domain` — asserted in CI, not
  eyeballed.
* 9,650 of the ~15,000 candidate lines have moved without a behavioural diff,
  and the rename-only check says so mechanically.
* Thirteen test files, including the host-equality suite, run from the far side
  of the boundary, on both macOS and Linux.

If any of those fails, the finding is worth more than the move: it means a CLR
concept is reaching the POSIX model somewhere the type names did not reveal,
and stages 4–9 need re-costing before anyone starts them.

---

*Stages 4–9 below are not commissioned. They are recorded so that stage 3's
result can be judged against where it was heading; re-read and re-cost them
before starting any of them.*

### Stage 4: `SignalState` genericised — **done; two decisions taken in absentia**

The plan as written missed that `SignalState` uses `ThreadId` in four places,
one of which destructures its representation
(`List.sortBy (fun (ThreadId.ThreadId tid) -> tid)`). Two questions followed,
both answered the reversible way:

* **Task identity: a second type parameter, not a library `TaskId`.**
  `SignalState<'Task, 'Handler>` with `'Task : comparison`. The alternative —
  introducing the library's own `TaskId` now — is where stage 5 is heading
  anyway, but it needs a `ThreadId`↔`TaskId` mapping established at thread
  creation, which is stage 5's work. A type parameter is purely additive and
  collapses to `TaskId` later for free. The destructuring sort becomes
  `List.sort`, which is identical for a single-field DU over `int`.
* **The dispatcher payload stays on `Initialized`, rather than moving out.**
  The plan proposed reducing `SignalInitState` to a payload-free bit with
  PawPrint holding the dispatcher alongside. That is conceptually cleaner — no
  real kernel has a managed-handler dispatch thread — but it splits an
  invariant the original DU shape exists to make unrepresentable-to-violate
  ("the dispatcher exists iff signal handling is initialised"). Keeping it as
  `Initialized of dispatcher : 'Task` preserves the machine-checked invariant
  and is honest at the library's altitude: the type records *which* task
  dispatches without claiming to know what a task is.

`SignalHandler` (which wraps a CLR `MethodInfo`) split out into its own PawPrint
file and stayed. `TestSignalState.fs` moved and instantiates both parameters
with nominal stand-in types of its own rather than `int` — an `int` could
satisfy the signature through a numeric path without the parameter being
genuinely opaque.

The existing tests never exercised the handler slot at all, so two were added
for it, and both are mutation-tested: making `setHandler` discard the rest of
the state, and making it first-writer-wins, each kill exactly one.

### Stage 4 (as originally specified): `SignalState` genericised

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
against the existing 4,450-test suite. Add a nested record to `EmulatedKernel`
and migrate the fields one group at a time, keeping the old accessors as
forwarding members:

**Corrected while doing sub-step 1: "no call site changes" is true of reads
only.** Nineteen forwarding members left every read site untouched, exactly as
planned — but F# cannot forward a record *update*, so `{ kernel with
FileSystem = fs }` has to become `{ kernel with Machine = { kernel.Machine with
FileSystem = fs } }`. The compiler named 124 such sites; they collapse to 76
update expressions.

Converting every write to a per-field setter first, as a separate prior step —
the shape the `DirectoryStreams` re-key used — would have preserved the property
literally. Measured, and rejected: **35 of the 72** kernel-update expressions set
fields from more than one group at once, so per-field setters would fragment one
record copy into a pipeline, and the later sub-steps would fragment it again.
Nesting inline keeps each expression to one outer copy and one inner copy.

Two smaller deviations, both to keep the diff mechanical:

* The field is `Machine : UnixMachineState` directly on `EmulatedKernel`, not
  `Unix : UnixSystem`. A `UnixSystem` wrapper holding one field would make every
  write three levels deep for no gain; it can be introduced once all three parts
  exist, as a change internal to this file.
* `UnixMachineState` holds all nineteen fields flat. The target shape's
  `UnixClock` / `UnixEntropy` / `UnixNetworkConfiguration` / `SocketTable`
  grouping is a separate concern from relocation, and once the forwarding members
  exist it costs no call site to introduce.

The groups:

1. machine: `FileSystem`, `FileSystemType`, `UnixPlatform`,
   `VirtualClockTicks`, `WallClockEpochMs`, `NonCryptoRandomState`,
   `CryptoRandomState`, `ProcessorCount`, `UserAddressLimit`, the network
   fields, the socket table;
2. process: `Environment`, `CurrentDirectory`, `CurrentDirectoryInode`,
   `ProcessPath`, `Umask`, `UserId`, `GroupId`, `FileDescriptors`,
   `DirectoryStreams` (**rekeyed already; move it as-is**), `Signals`,
   `OutputLog`;
3. tasks: `ParkedSocketWaits`, and `Cpu`/`OsThreadId` pulled off `ThreadState`.

**Corrected while doing sub-step 3: `LastSystemError` is not kernel state.** On a
real Unix errno lives in libc, not the kernel — the kernel returns an error code
and the syscall wrapper stores it — so a POSIX simulator should return errors,
not hold them. PawPrint's map is that wrapper's slot, and CoreCLR reuses it for
Windows last-error too: `NativeWaitHandle` really does write Win32 numbers
(`ERROR_TOO_MANY_POSTS` 298, `ERROR_INVALID_PARAMETER` 87) into it. It stays on
`EmulatedKernel` beside `LastPInvokeError`, and the `Errno : int` in the target
shape above is wrong.

That correction is what makes the rest of the sub-step sound. The `Cpu` and
`OsThreadId` docstrings argued they were *fields* rather than map entries
because "there is no truthful default for an absent key", and errno — whose
absence truthfully means 0 — was the one field that would have forced the task
record to be partial. Without it every field is total, so a missing key is a bug
rather than a default, and `checkTaskInvariants` says so.

**The oracle the plan named cannot be written.** It asked for "the task set is
exactly the live threads — created on thread creation and removed on thread
exit", mutation-tested by deleting the removal. Nothing removes a thread from
`IlMachineState.ThreadState`; there is no removal site in the repository, and
`OsThreadId`'s non-reuse argument depends on that. So the honest assertion is
that the task set equals the thread set, both monotonic: it catches a thread
created without `registerTask` and a task minted for a thread that never
existed, and it is not a leak check because nothing can leak yet.

**Two of the seven mutants survived the first battery, and both were fixture
defects rather than code defects.** Placing every task on core 0 instead of its
rotation slot survived because `addThread` has one production caller — the entry
thread, always at rotation 0, where `cpuForRotation 0` and `CpuId 0` agree — so
the mutant is equivalent under every call the program makes; the new row calls
`addThread` twice on a four-core machine to pin the contract anyway, so a second
caller cannot silently land on core 0. Clobbering the core while parking survived
because the park row used a one-processor machine whose only thread was already
on core 0; it now parks the *second* thread of a four-core machine. Both are the
same lesson: a fixture whose inputs are all zero cannot tell "left alone" from
"set to zero".

Sub-step 2 took `NextDirectoryStreamId` across with `DirectoryStreams`: it is the
counter that mints stream ids, so it is the same kernel state, and only
`DirectoryStreamBlocks` — PawPrint's representation of a `DIR*` — stays behind.
`UnixProcessState` also holds `Signals` concretely as
`SignalState<ThreadId, SignalHandler>` rather than generically: the type
parameters stage 4 added exist so the *library* need not know PawPrint's
`ThreadId`, and nothing needs that until stage 6 moves the record across.

`KernelConfig` splits along the same lines but keeps its current surface, so
`HostConfig` and every test registration are untouched.

**Settled, and done: `DirectoryStreams`' key** — option (ii), as its own step
before the migration. `DirectoryStreamId` is minted from a
`NextDirectoryStreamId` counter on `EmulatedKernel`, exactly as `SocketId` and
`InodeNumber` are, and `DirectoryStreamBlocks : Map<NativeMemoryBlockId,
DirectoryStreamId>` is the client's representation of a `DIR*`. That second map
is a PawPrint field and stays behind when `DirectoryStreams` moves in stage 6,
which is the whole point: the stream table is kernel state a POSIX simulator
owns whatever its client hands out, and `NativeMemoryBlockId` never crosses.

`DirectoryStreamId` is declared beside `DirectoryStream` in `EmulatedKernel.fs`
rather than in the library today. The rule being enforced is "no PawPrint
identity inside the library", which is already satisfied; publishing a type into
the package before anything there uses it would be placement ahead of need, and
the two move together in stage 6 like every other field.

The general setter is gone. `withNewDirectoryStream` mints, and
`withDirectoryCursor` updates under the existing id — so "a `readdir` mints a
second id, leaving the old stream unreachable and its directory pinned for the
run" is not a mistake the API can express. Four invariants are new:
`NextDirectoryStreamIdNotFresh`; `DirectoryStreamBlockDangling` /
`UnreachableDirectoryStream` for the two directions in which the maps can
disagree; and `DirectoryStreamNamedTwice`, because `DirectoryStreamBlocks` must
be *injective* and neither of the other two can see that — the unreachable check
reduces the ids to a set, in which two blocks naming one stream collapse to one
element and both directions come back clean. Codex found that; measured before
fixing, `checkInvariants` accepted the state, and closing either block would then
have taken the stream out from under the other.

**Correctness oracle, met**: the full suite including `Guest`, plus a new
`TestDirectoryStreamId` fixture — thirteen rows and a property that drives an
arbitrary sequence of opens and closes and asserts the maps agree at the end.
Mutation-tested, seven mutants, all killed: forgetting either map on close
(two), a counter that does not advance, a cursor advance routed through the open
path, and each of the three new invariant checks computed but never reported.
Three of them were killed by exactly one row each, which is the row written for
them.

The original option set follows, for the record.

**The question was: `DirectoryStreams`' key.** This
stage's whole safety property is "forwarding accessors, so no call site
changes", and rekeying `Map<NativeMemoryBlockId, DirectoryStream>` to a minted
`DirectoryStreamId` contradicts it — `Native/NativeSystemNative.fs` keys those
by block id, so a forwarding accessor would have to maintain a block-id →
stream-id mapping, which is design work smuggled into a mechanical stage.

* **(i) Move the field as-is, rekey later.** The library ends up holding a
  `NativeMemoryBlockId`-keyed map for a stage, which is a PawPrint identity
  inside the library — ugly, and visible in the public surface.
* **(ii) Rekey first, as its own step before the migration.** One focused
  change to the `opendir`/`readdir`/`closedir` handlers, with the block-id →
  stream-id mapping introduced deliberately and tested, then the field moves
  mechanically like every other. Costs an extra step; keeps this stage's
  safety property intact.
* **(iii) Leave `DirectoryStreams` on `EmulatedKernel` entirely** and revisit
  when stage 8 moves `opendir`. A directory stream *is* kernel state, so this
  is a deferral rather than an answer, but it is the smallest step.

Chosen: **(ii)**. The mapping has to exist eventually, the handlers are few, and
doing it separately keeps "no call site changes" true of the migration itself —
which is the only reason a 40-field move is reviewable.

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
