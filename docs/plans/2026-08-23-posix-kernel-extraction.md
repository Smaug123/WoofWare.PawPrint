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

**`TaskId` turned out to be unnecessary, and stage 6c dropped it.** The sketch
above has the library mint a `TaskId` with the client maintaining a
`ThreadId -> TaskId` mapping. There is a cheaper answer that this codebase had
already found: make the table generic in the task name, exactly as
`SignalState<'Task, 'Handler>` is. `UnixTaskTable`'s operations take a
`Map<'Task, UnixTaskState>` and never learn what names a task, so no second
identity and no mapping to maintain. `EmulatedKernel` keys the table by its own
`ThreadId` and nothing in the library knows.

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

### Interlude: the docstrings the moves detached

Stages 5 and 6 moved definitions between files with scripted `cut`s that took
the definition and left the prose. An F# `///` block binds to the declaration
that *follows* it, so a stranded block does not vanish — it silently starts
documenting whatever comes next, and where it lands immediately above another
docstring the two fuse into one. Nothing catches that: it is invisible to the
compiler, to the tests, and to a diff that looks local and reasonable.

Nine sites were wrong by the end of stage 6c, and repairing them by eye had
already failed twice (PRs #1162 and #1166 each found some and missed others).
`scripts/check-docstring-attachment.py` is the oracle that finds them all: it
pairs every block with the declaration it precedes on both sides of a revision
range and reports the pairs that moved. Two of the nine predate the extraction
entirely, which is what a mechanical check buys over reading — one of those
(`nanosecondsPerTick` still opening "Nanoseconds per millisecond") had been
stale since #848 renamed the constant.

Run it against the branch point for every remaining stage; the moves are not
finished.

**The check had a blind spot, and it was the shape that caused both of the
strandings found by eye.** `MERGED` recognised a fusion by splitting the fused
text into blocks that had *each* stood on their own at the base revision, so it
saw two existing docstrings pushed together but not the far commoner case: a
declaration inserted between a block and its subject, carrying a docstring
written on the spot. The second half never existed at the base, so there was
nothing to split against and the check stayed silent — which is how #1080 could
take `withUmask`'s prose and #1089 `cpuForRotation`'s with the tool in the
repository. It now takes the longest opening that stood on its own before and
asks about the *declarations* rather than about the text: the new block must
precede at least one declaration that opening did not already document, and some
declaration that did have it must no longer have it anywhere in its docstring.
A declaration that had the prose and still has it was left alone, however much
that prose has grown above or below it; and being detached from prose is exactly
no longer having it. Both counted per declaration, since one normalised block can
precede several.

Five review rounds arrived at that phrasing, and the first four are why it is
phrased that way. Asking only "did the subject change" reported a docstring that
merely gained a paragraph. Asking additionally "does the opening still stand
alone as a block" missed a stranding whose fused text opened with a *longer*
block that was still standing; fixing that by continuing the search rather than
stopping still misreported an opening retained inside its own subject's expanded
block; asking the subjects existentially missed a block borne by two declarations
where only one lost it; and asking whether the surviving docstring *starts* with
the block reported prose merely prepended above it. Every one is a text-shaped
approximation of a question about declarations, which is what five rounds on one
predicate usually means.

None of the shapes occurs in this repository's history: the whole-repository
sweep reports the same thirty-five fusions throughout. Their oracle is
`scripts/test-docstring-attachment.sh`, which puts all fifteen shapes in one
throwaway repository — seven that must report, eight that must stay silent, plus
the exit status and the exact number of findings — and runs as the
`docstring-attachment` flake check, so a change to the checker cannot quietly
retire it. It is itself mutation-tested: eight mutants of the guard, each
killed by exactly the rows written for it. Two of those rows exist only because a
mutant survived the first battery, which is the usual reason to keep a fixture
honest: with two *distinct* subjects `kept < count` and `kept < 1` agree, and
without a partial-word case the word-boundary padding is unobservable.

Measured by running the check across every commit in the repository against its
own parent, over the F# files that commit touched. Thirty-five fusions of the
new shape, of which nine are strandings still in the tree: five in files this
extraction has been moving (`cpuForRotation`, `defaultUserId`,
`sockaddrFamilyField`, `socketCreation`, `GetCwdOrphanAnswer`) and four not.
Eight are repaired as pure relocations. The ninth,
`MemoryBlock.readNamedBytes`, needs writing rather than relocating: the stranded
half says the function refuses a cell whose typed view is not byte-addressable,
which is exactly the cell it *names* instead.

Three more reports are not defects, and the line separating them is worth
holding. Where the inserted declaration is a *generalisation* that legitimately
inherited the prose — `tryNameCellWith`, `allocateZeroedAs`, `invokeStringQCall`
— the block still describes what it now precedes; only the specialisation is
left undocumented, and giving it words is writing rather than reattaching. The
rest of the thirty-five are renames that carried their prose along, or
strandings #1166 and #1171 had already repaired.

### Stage 6: move `UnixSystem` to the library

**Dependencies**: stage 5.

Move `UnixSystem` and its operations across. `EmulatedKernel` keeps only the
CLR-side fields listed in "What stays in PawPrint", plus `Unix : UnixSystem`.
Delete the forwarding accessors from stage 5 and update the ~180 call sites in
`NativeSystemNative.fs`.

**Done, in seven sub-stages** (6a leaf types, 6b–6d the three records, 6e–6g the
forwarders they left behind), with one deviation: `EmulatedKernel` holds the
three records flat rather than a `Unix : UnixSystem` aggregate. See 6g for why
that waits for stage 7.

**Correctness oracle**: the full suite, plus the CI assertion from stage 1 that
the library references nothing of PawPrint's. At this point the goal's second
bullet is met and the third is met; the first is met for state but not yet for
transitions.

#### Stage 6d: `UnixProcessState` — done

The record and nine process-only operations moved, generic in both of
`SignalState`'s parameters because it holds one:
`UnixProcessState<'Task, 'Handler>`. The `UnixProcessState<'Handler>` in the
target shape above is therefore wrong, in the same way that block's
`UnixTaskState.Errno` is: it was written before stage 4 measured that a signal
state has to be generic in what names a task as well as in what a handler is. `environmentEntryProblem` went with them,
being the rule `withEnvironment` enforces and no use of a CLR concept.
`descriptionsNamingSocket` was private and stops being so: two of its three
callers moved, and the third — `signalSocketDataReady`, which is mixed and stays
— now reaches the library definition rather than a wrapper existing for one call
site. `EmulatedKernel` keeps a thin forwarder for each of the rest, as stage 6c
did, and stage 6e deletes them all.

**Decided while doing it: who names the knob in a rejection.** Three setters
validate and throw, and three tests assert the message names what a *host* would
have to fix — `EmulatedKernel.ProcessPath`, `EmulatedKernel.Umask`,
`KernelConfig.Environment`. None of those names is the library's to know, and two
of them stop existing at stage 6e.

* **(a) The library names its own field**, and the tests relax to
  `UnixProcessState.Environment`. Simplest, and stops being wrong at 6e — but a
  host that trips the check is told which field of a package it has never heard
  of is unhappy, rather than which line of its own configuration to edit. The
  test comments say avoiding exactly that is why they exist.
* **(b) The client validates first and throws its own message**, with the
  library re-checking behind it. Keeps both messages accurate, at the cost of two
  statements of one rule that can drift apart, and of a second scan.
* **(c) The caller supplies a `context` string the library prefixes**, which is
  what `AbsoluteUnixPath.assertValid` and `PermissionBits.assertValid` already
  do throughout this library, and for the same reason.

Chosen: **(c)**. It is the house pattern, it is one parameter rather than a
duplicated rule, and it puts the name in the hands of whoever owns it — so stage
6e re-spells `"EmulatedKernel.ProcessPath"` by editing a string at the call site
instead of a message inside the package. PawPrint passes
`"EmulatedKernel.Environment (set from KernelConfig.Environment)"`, which names
both the field and the knob; the three tests pass unchanged.

**Found by eye while scoping, then confirmed mechanically**: `withUmask` had
never had a docstring. #1080 inserted it directly beneath
`withUserAndGroupId`'s, so it adopted that one and left the function that does
set the ids undocumented. `scripts/check-docstring-attachment.py 5ebe390d^`
reports it. This is the tool's documented blind spot — a mistake already in the
base is invisible to a check between two revisions — and the way out is to run
it across the commit suspected of making it.

#### Stage 6e: the task forwarders deleted — done

The six wrappers over `UnixTaskTable` are gone and their thirty-three call sites
name the library directly: `UnixTaskTable.cpuOf thread kernel.Tasks` and its
peers for the reads, `EmulatedKernel.mapTasks (UnixTaskTable.register …)` for the
writes. `EmulatedKernel.task` was not replaced by anything, having had no caller
anywhere since it was written.

**Decided while doing it: what a write call site says.** F# cannot forward a
record update, so deleting a `with`-style forwarder leaves the call site to
perform the update itself.

* **(a) Write the record update inline** — `{ kernel with Tasks =
  UnixTaskTable.register … kernel.Tasks }`. Nothing new to learn, and each site
  says exactly what it does. But five of the eight write sites are in `|>` or
  `MapKernel` position, where an expression is not enough and a
  `(fun k -> { k with … })` has to be written out.
* **(b) One combinator per record**, `mapTasks : (Map<ThreadId, UnixTaskState> ->
  Map<ThreadId, UnixTaskState>) -> EmulatedKernel -> EmulatedKernel`. Three of
  these replace fifty-four forwarders across the three records, pipelines
  survive, and the operation the call site names is still the library's.
* **(c) Keep the write forwarders and delete only the reads.** Smallest diff, and
  wrong: the wrappers exist to be deleted, and half a deletion leaves both
  spellings live.

Chosen: **(b)**. It is not a forwarder by another name — it restates none of the
library's API, it is a lens over `EmulatedKernel`'s own field, and it is what
lets a call site keep saying *which part of the kernel* it touches. `MapKernel`
on `IlMachineState` is the naming precedent.

**Nothing was lost by deleting the prose.** Five of the six forwarders'
docstrings say what the library's already say. The sixth, `parkedSocketWaitFor`,
carried a paragraph the library's does not — that the value is present from the
park through the wake to the delivering re-entry, so the close-time retention
check covers the woken-but-not-yet-run window — and `closeFd` already states that
where it relies on it ("Checked against the in-flight wait map rather than thread
status"). `scripts/check-docstring-attachment.py` against the branch point
confirms no block changed subject.

**Correctness oracle**: the full suite including `Guest`, and the docstring
check. There is no behaviour to test: every call site is the same computation
spelled differently, and the compiler checks the spelling.

The remaining two records follow the same shape — stage 6f for
`UnixProcessState`'s nine wrappers and about thirty call sites, stage 6g for
`UnixMachineState`'s twenty-four and about two hundred. Split by record because
one diff of ~260 mechanical call sites is not reviewable, and the split is the
same one 6a–6d used.

#### Stage 6f: the process forwarders deleted — done

Nine wrappers gone, `mapProcess` added beside `mapTasks`, about forty call sites
respelled. Three of the nine took a hard-coded `context` string, and 6d's choice
(c) pays off here exactly as it was meant to: `KernelConfig.applyTo` now passes
`"KernelConfig.ProcessPath"`, `"KernelConfig.Umask"` and
`"KernelConfig.Environment"` — the knob a host actually turns — instead of
`EmulatedKernel.ProcessPath` and a field name that is about to become
`kernel.Process.Environment`. Tests pass `"test"`, which is what every other
`context`-taking call in this repository does.

**The two tests that asserted a message had to move with the name.** Both called
a setter directly and asserted the string that setter hard-coded. With the name
supplied by the caller, a test that calls the library directly is asserting a
string it chose itself, which is no test at all — the library's own fixture
already covers that the parameter is not ignored. What PawPrint still owns is
*which* name it passes, and that is only observable through `KernelConfig.applyTo`:

* `TestProcessPath`'s "rejects a forged path" now drives `applyTo` with a
  defaulted `AbsoluteUnixPath` and asserts `KernelConfig.ProcessPath`.
* `TestEnvironmentEntryInvariant`'s direct-setter rejection is deleted. Its
  sibling already drove the same `rejected` corpus through `applyTo` and asserted
  the same thing, expressly so that an `applyTo` which assigned `Environment` by
  record-copy would be caught; that sibling inherits the deleted test's account
  of why the boundary matters.

Mutation-tested, both surviving assertions: change either context string at its
`applyTo` call site and the corresponding test fails. `KernelConfig.Umask`'s has
no such test and cannot have one — 6d measured that a defaulted `PermissionBits`
is `0o000`, a legal `umask 000`, so no forged value reaches that guard.

**Correctness oracle**: the non-`Guest` suite, the library suite, and the
docstring check; `Guest` runs in CI. The count drops by one, which is the deleted
test.

Stage 6g is the last: `UnixMachineState`'s twenty-four wrappers and about two
hundred call sites.

#### Stage 6g: the machine forwarders deleted — stage 6 done

The last twenty-four wrappers are gone and `mapMachine` joins `mapProcess` and
`mapTasks`. One hundred and eighty-eight call sites were respelled by script,
sixteen more by hand, and nine files gained an `open WoofWare.PosixKernel`.
`EmulatedKernel` now holds `Machine`, `Process` and `Tasks` and restates none of
the library's API: the three lenses take an operation and apply it to a field,
which is the only thing left that is PawPrint's to say.

**Redone from scratch once, and the reason is worth keeping.** The first attempt
rewrote reads with a regex whose first capture was `\S+`, so
`EmulatedKernel.socket (SocketId client) kernel` matched with the argument split
at the space inside the parentheses — producing
`UnixMachineState.socket (SocketId client.Machine) kernel`, which projects the
wrong value and drops the kernel. The compiler caught those, but "the compiler
caught the ones that did not typecheck" is not the same as "none survived", and
a transformation applied 188 times has to be trustworthy rather than
spot-checked. The tree was reset and everything redone with one
argument-aware rewriter that consumes balanced parentheses and **reports what it
cannot parse instead of guessing** — three calls whose arguments spanned a line
break, which were then done by hand.

**The audit that makes that trustworthy** is a token multiset comparison against
the branch point, written separately from the rewriter: normalise away exactly
what the rewrite may change (which module qualifies a name, the `.Machine` that
projects a kernel, the `mapMachine` lens) and every remaining token must match.
It does, for all twenty-one rewritten files — the only residue is the deleted
block, the added `open`s, and the three deliberate hand edits, each of which the
audit names. Whole-file rather than per-line, because `fantomas` reflows.

**`allocateEphemeralPort` was the one operation that could not be mechanical**:
it answers a port *and* advances the machine, so the wrapper existed to re-wrap
`(port * UnixMachineState) option` into `(port * EmulatedKernel) option`. A
fourth lens for option-returning state operations would be a shape invented for
one function, so instead the seven test sites thread `UnixMachineState` directly
— ephemeral-port allocation is a machine operation, and they only ever built
kernels to reach the machine — and the two production sites rebuild the kernel
in the match arm, which is a line longer and says plainly where the advance goes.

**`UnixSystem` is deliberately still absent.** The target shape has
`EmulatedKernel` hold `Unix : UnixSystem` rather than three fields, and stage 5
deferred it on the grounds that a one-field wrapper buys nothing. It is still
deferred, now for a better reason: its consumer is stage 7's
`step : Syscall -> UnixSystem -> SyscallOutcome * UnixSystem`, which cannot take
three separate arguments and return three. Introducing the aggregate with its
consumer costs one mechanical `kernel.Machine` -> `kernel.Unix.Machine` rename;
introducing it now would be a rename with nothing to justify it.

**Correctness oracle**: the token audit, the non-`Guest` suite and the library
suite (`Guest` runs in CI), and the docstring check. No behaviour changes: every
call site is the same computation spelled differently.

### Stage 7 design (proposed)

Stage 7's specification below was written before stage 6, and measuring against
today's code moves several things. This proposal was reviewed by a second model,
which found two blocking defects and five smaller ones; what follows is the
revision, with the review's findings marked where they changed the design.

#### What is actually there

The stage names thirteen syscalls. Measured against
`Native/NativeSystemNative.fs`:

* three have **no handler at all** — `getuid`, `umask`, `chdir`. There is nothing
  to hoist.
* four **do touch guest memory**: `getcwd` writes a buffer; `unlink`, `rmdir` and
  `mkdir` each read a NUL-terminated path. The stage says "carry no buffer",
  which is true only of the output buffer.

The buffer-free set that exists today is the remaining **six**: `GetEUid`, `Dup`,
`Close`, `FLock`, `LSeek`, `FTruncate`. None reads or writes guest memory and
none blocks — `FLock`'s blocking case is a refusal today rather than a park. So
stage 7 is these six, and every path-carrying syscall moves to stage 8 where the
buffer machinery belongs.

#### Refusals are two different things, and only one of them is data

The six carry eight `failwith`s between their match arms, and a mechanical
conversion of all eight into a returned outcome would be a correctness bug.
Reading them:

| site | says | kind |
| --- | --- | --- |
| `NativeSystemNative.fs:4599` | SEEK_DATA/SEEK_HOLE: no notion of sparseness | measured, unmodelled |
| `:4659` | directory SEEK_END: "no portable answer: measured, …" | measured, unmodelled |
| `:3692`, `:3811` | Darwin `flock`; blocking `flock` | measured, unmodelled |
| `:4611`, `:4618` | "(this is an interpreter bug)" | invariant |
| `:4625` | "the open file description must keep it alive" | invariant |
| `:4641` | "`open` resolves symlinks, so no descriptor should name one" | invariant |

An invariant violation is a corrupted `UnixSystem`. Returning it as a lawful
outcome hands a second client something it can catch and continue past, with the
broken state alongside — which is exactly what "correctness over availability"
forbids, and what the gospel's fail-fast assertions are for. **Only the measured
refusals become data; the invariant arms stay `failwith` inside the library.**

The census also stops too early. `Close` delegates to `EmulatedKernel.closeFd`,
which carries refusals of both kinds — two measured close-under-waiter cases
(`EmulatedKernel.fs:2107`, `:2110`), a measured listener-RST case (`:2207`), and
an invariant (`:2126`, `DanglingSocket`). `step` exposes call trees, not match
arms, so the classification is a job for every operation the six reach.

#### A refusal must not hand back a state

If `step` returned `SyscallOutcome * UnixSystem` with a refusal case, the refusal
would arrive paired with *some* state, and nothing in the type says which.
`closeFd` makes that concrete: `FileDescriptorRegistry.close` runs at
`EmulatedKernel.fs:2064`, and the refusals fire after it, so the obvious
conversion returns the advanced registry beside a refusal.

So the refusal is the outer error, and carries no state:

```fsharp
val step :
    Syscall -> UnixSystem<'Task, 'Handler> ->
        Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SyscallRefusal>
```

A refused call structurally cannot yield a continuable state: the client still
holds the one it passed in. (The first draft had a three-case outcome, and its
own worked example discarded the state on the refusal arm with a `_` — which was
the tell.)

#### The types

```fsharp
type UnixSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    {
        Machine : UnixMachineState
        Process : UnixProcessState<'Task, 'Handler>
        Tasks : Map<'Task, UnixTaskState>
    }

type Syscall =
    | GetEffectiveUserId
    | Dup of fd : int
    | Close of fd : int
    | FLock of fd : int * operation : int
    | LSeek of fd : int * offset : int64 * whence : int
    | FTruncate of fd : int * length : int64

/// What the entry point returns, for a request the library could answer.
type SyscallAnswer =
    /// The entry point returns this.
    | Completed of answer : int64
    /// The entry point returns its failure sentinel, and the client stores
    /// `error` wherever its libc keeps errno. A failure still changes the
    /// system: a failing `flock` advances the descriptor table, measured.
    | Failed of error : UnixError
```

`SyscallRefusal` is a DU per refusing syscall (`LSeekRefusal.SeekDataHole`,
`FLockRefusal.DarwinUnmodelled of divergence`, …) rather than a `string`. The
precedent is in the library already: `SocketCreationRefusal.Unmodelled`
(`SimulatedUnixPlatform.fs:416`) is payload-free, and `NativeSystemNative.fs:5188`
composes the crash message from the raw PAL arguments the library never saw.
Prose describing *what the client asked for* cannot be the library's, so the
library names the case and the client writes the sentence.

**The core is per-syscall functions; `step` is a total dispatcher over them.** A
single `SyscallAnswer` for every syscall makes illegal outcomes representable:
`geteuid(2)` cannot fail (`pal_uid.c` is `return geteuid();`), and `Dup`,
`Close` and `FTruncate` have no refusal at all. Typed:

```fsharp
val effectiveUserId : UnixSystem<'T,'H> -> uint32                                    // total
val dup : fd : int -> UnixSystem<'T,'H> -> SyscallAnswer * UnixSystem<'T,'H>          // fallible, unrefusable
val lseek : fd : int -> offset : int64 -> whence : int -> UnixSystem<'T,'H>
         -> Result<SyscallAnswer * UnixSystem<'T,'H>, LSeekRefusal>
```

`step` still earns its place — uniform syscall logging, replay, and `SocketFuzz`
generation all want one surface — but as sugar over the primitives rather than as
the primitive. Cost: two surfaces to keep aligned, mitigated by the dispatcher
being one arm per case and trivially total.

#### `SyscallEffect`: deferred implementation, but the target shape is committed now

No syscall in stage 7 writes guest memory, so nothing in stage 7 can exercise an
effect list, and a shape no test can observe is a shape chosen ahead of its
evidence. Stage 7 builds none.

But the *evidence* for the eventual shape is already in this document, so
deferring the decision as well would be a false economy. Decision 1 records that
a failed `epoll_wait` writes 0 through `*count` on Linux and -1 on Darwin — so
**`Failed` carries writes too**, and `SyscallAnswer` gains a `writes` component
on both arms when the first buffer-carrying syscall lands. Committing to that
here is the same argument the task parameter gets below, applied consistently.

What stage 8 must still *measure* is whether `writes` is an ordered list. The
claim to test is that it need not be: a syscall is atomic with respect to
PawPrint's scheduler, and errno is per-calling-thread
(`LastSystemError : Map<ThreadId, int>`), so no other guest thread can observe an
interleaving and the caller sees every write only after return. If that holds, a
set or a record is the honest shape and decision 2(b)'s ordered list is
over-specified. **Decision 2(b) must be amended in place when this lands**, not
left contradicting stage 7 — two versions of the truth in one plan document is
the state the plan's own migration section warns about.

One honesty note for stage 8: `Completed of answer` is "what the entry point
returns, when that is a value the library knows". `getcwd`'s success value is the
caller's own buffer pointer, which the library never possesses; the client
composes it.

#### Raw versus parsed: the rule, restated

The first draft said "a value crosses raw exactly when rejecting it is behaviour
the library models". The biconditional is false, and settled material falsifies
it: rejecting a null pointer with EFAULT *is* library behaviour, yet decision 2
has the pointer cross **parsed**, as `UserBuffer`'s three-way classification,
because only the client can tell whether an address names storage. The rule
conflated two independent questions. Separated:

* **Who can classify?** Whoever holds the knowledge. The client knows what an
  address names; the library knows what a path resolves to.
* **Who owns the consequence?** Whoever models the behaviour. EFAULT for `Null`
  is the library's answer even though the client did the classifying.

A value crosses raw when the library can classify it *and* owns the consequence.
With one carve-out that stage 8 will need: **raw means raw kernel ABI, not PAL.**
`open`'s flags are PAL values the C shim translates, and letting them cross would
grow `scripts/pal-residue-allowlist.txt`, which may only shrink. Stage 7's two
raw ints are safe because the handlers say so — `flock`'s operation bits are
"*not* PAL values that the C translates" (`NativeSystemNative.fs:3652`) and
`SeekWhence`'s numbering "is also POSIX's" (`:4437`).

#### The PAL residue stage 3.5 assigned here

`scripts/pal-residue-allowlist.txt` says in its header that "Stage 7 … retires
what is left", and stage 3.5 chose containment on the strength of that promise.
The re-scoped stage 7 touches none of the socket cluster, so it cannot keep the
whole promise — but it can keep part, and must say so rather than silently drop
it. Stage 7 retires the `UnixError` PAL cluster (`toPal`, `palOfRawErrno`,
`palOfRawErrnoUnder`), which belongs beside the errno-encoding helper stage 7
builds anyway; the socket cluster moves to stages 8 and 9 with the syscalls that
use it; the allowlist header is updated to say so.

#### Two questions the review changed my mind on

**(1) Does `step` take the calling task?** I leaned yes. The review's argument
against is stronger, and it is a mutation-testing argument: **no stage-7 or
stage-8 syscall's answer depends on the caller**, so no test and no mutant can
distinguish a correct caller argument from a wrong one. Every call site written
in stages 7–8 would pass an unaudited value, and when stage 9 makes it
load-bearing nothing — not the compiler, not a review — revisits them. That is
`fixture-default-can-hide-the-mutant` at the scale of a whole API.

Chosen instead, a third option neither draft listed: **carry the caller in the
payload of the syscalls that depend on it**, which from stage 9 means
`WaitForSocketEvents of caller : 'Task * …`. "This syscall's answer cannot depend
on who asks" then becomes structural for every other case rather than a
convention. Cost: `Syscall` becomes generic in `'Task` at stage 9. Note this
supersedes decision 1(b)'s sketched
`step : UnixSystem -> TaskId -> Syscall -> …`, which predates the census.

**(2) When does `UnixSystem` appear?** Unchanged: its own commit, first. It is a
mechanical rename measured at **320 sites** (195 `.Machine`, 98 `.Process`, 27
`.Tasks`), auditable by the token-multiset check stage 6g used, and burying that
under the design work is what makes a diff unreadable.

#### What a handler becomes, and what it does not

```fsharp
| Some "SystemNative_FLock", [ ConcreteIntPtr _ ; _ ], MethodReturnType.Returns (... Int32) ->
    let fd = fdArgument operation instruction.Arguments.[0]
    let request = NativeCall.int32Argument operation instruction.Arguments.[1]

    match UnixSystem.step (Syscall.FLock (fd, request)) state.Kernel.Unix with
    | Error refusal -> failwith (FLockRefusal.describe operation fd request refusal)
    | Ok (SyscallAnswer.Failed error, unix) -> failingWith operation error unix ctx state
    | Ok (SyscallAnswer.Completed answer, unix) -> answeringInt32 (int32 answer) unix ctx state
```

`failingWith` is shared: it converts `UnixError` to the raw errno under this
kernel's numbering, stores it in `LastSystemError` for the calling thread, and
pushes the sentinel. There is **not** a single `answeringWith`, because the six
push four different eval-stack shapes — `UInt32` for GetEUid
(`NativeSystemNative.fs:2540`), `NativeInt` for Dup (`:4954`), `Int64` for LSeek
(`:4685`), `Int32` for the rest — so the encode half is a small per-width family.

Nor does "the handlers shrink to decode, `step`, encode" hold of the *library*
side. `EmulatedKernel.closeFd` has three callers (`:3407` CloseDir, `:4970`
Close, `:6254` a socket path) and `commitTruncation` two (`:2875` Open's
`O_TRUNC`, `:3551` FTruncate), and in both cases only one caller hoists in stage
7. Those operations move into the library as directly-callable functions that
`step` delegates to, and the stage-7 move budget for `Close` is `closeFd`'s
socket-teardown logic rather than a match arm.

#### Two things implementing it changed

**`UnixSystem` is projected, not stored — so it lands with `step` after all.**
The design had `EmulatedKernel` hold `Unix : UnixSystem` in its own prior commit,
a rename measured at 320 sites. Implementing showed a third option neither draft
considered:

* **(a) Storage.** `EmulatedKernel` holds one field; 302 reads and 144
  field-assignments change. Faithful to the target shape.
* **(b) Projection.** `EmulatedKernel` keeps the three fields flat, and a lens
  pair — `unix : EmulatedKernel -> UnixSystem<…>` and `withUnix` — assembles and
  disassembles at the boundary. About thirty lines; **no call site changes at
  all**. Costs one three-field allocation per syscall, which is nothing against
  an interpreter that allocates per IL instruction, and it is a *function*
  rather than a property so the cost reads at the call site.
* **(c) Storage, but later.** Defers the choice without deciding it.

Chosen **(b)**, on blast radius and reversibility: 446 mechanical sites for a
shape exactly one boundary needs is not a trade this stage has to make, and if
the aggregate ever earns storage the rename is still there and no harder. It
also dissolves the sequencing question — with no rename to separate,
`UnixSystem` arrives with the consumer that justifies it, which is what stage 6g
argued for.

The two directions must be total inverses, so `TestUnixSystemProjection` asserts
the round trip both ways: an answer is lost if a caller forgets `withUnix`, and
a state is resurrected if a caller writes back a system it did not step.

**The library cannot construct one of itself.** `TestUnixSystemStep` has to
spell out all nineteen fields of `UnixMachineState` and all twelve of
`UnixProcessState`, because the library exposes no constructor for either —
`EmulatedKernel.initial` is PawPrint's, and the defaults it uses
(`defaultUnixPlatform`, `defaultEphemeralPortRange`, `defaultUserId`, …) live in
`EmulatedKernel.fs`. A second client cannot make a `UnixSystem` at all without
transcribing those. That is a real gap in the stated goal, found by being the
second client for the first time, and it is deliberately *not* fixed here:
choosing what an `initial` takes as arguments is API design, and folding it into
a stage about the syscall surface would decide it by accident. It gets its own
stage.

#### What landed first

`GetEffectiveUserId`, `Dup` and `LSeek` — chosen so that the first increment
exercises every part of the shape rather than the easy part. `GetEffectiveUserId`
is the syscall whose *type* says it cannot fail; `Dup` is a plain
`Completed`/`Failed` pair; and `LSeek` is the one that refuses, so the
refusal-versus-invariant split and the stateless `Error` are both under test from
the start. `NativeSystemNative`'s `LSeek` arm goes from 246 lines to 33.

The split that design predicted holds in the messages: the library says why no
answer exists ("the two platforms transpose the numbers"), and PawPrint says
which managed caller could have asked ("CoreLib never sends these —
`Interop.Sys.SeekWhence` is 0, 1, 2"). Neither half can write the other's.

`FLock` and `FTruncate` followed. Between them they brought the first refusals
with more than one reason — `flock` has six, five of which are `refuseDarwin`
call sites that the old code spelled as one `failwith` — and the first operation
shared with a syscall that has not hoisted: `commitTruncation` becomes
`UnixSystem.truncateAt`, which `ftruncate` calls and which PawPrint's `open`
still calls directly for `O_TRUNC`. That is the "directly-callable operations
with `step` delegating" shape the design predicted, arriving on schedule rather
than as a surprise.

**An existing guest test caught the split putting prose on the wrong side.**
`TestFlockBlocking` asserts the refusal names `issue #956`, and the first cut of
the message lost it: the issue tracks *PawPrint's* scheduler work, so it belongs
in PawPrint's half, and I had left it in neither. The rule the split needs is
narrower than "measurement is the library's": a pointer to work is owned by
whoever would do that work. The library cannot block a caller because it has no
scheduler; the issue for building one is the client's.

**Publishing a helper inherits the preconditions its private callers kept.**
Review found that `truncateAt`, being public where `commitTruncation` was
private, admits a negative length — and `VirtualFileSystem.truncateFile` guarded
that with a `Debug.Assert`, which a Release build compiles out, after which the
negative reaches `Array.Take` as an empty prefix and the file is silently emptied
and stamped. The guard is now a `failwith`, mirroring
`FileDescriptorRegistry.setOffset`'s treatment of a negative offset, which is the
same precondition one layer over. This is the hazard `unparking-inherits-the-
refusals-validations` names, in a new place: *widening* a definition's audience
inherits every rule its old audience happened to satisfy, and a `Debug.Assert` is
not a rule, it is a hope.

**A refusal that is really stage 9's outcome, and what it costs.** Review
pointed out that `FLockRefusal.WouldBlockIndefinitely` discards a state change a
real kernel makes: `flock` removes the caller's old lock before it sleeps, so
the registry has already advanced by the time the contention is discovered, and
a refusal hands back no system. That is correct as far as this stage goes —
PawPrint crashes, and a refusal must not look continuable — but it is a real
loss for a client that *could* park, and it is the tell that this case is not a
refusal at all. Decision 3 already says blocking becomes
`WouldBlock of WakeCondition`, an *outcome*; when stage 9 builds that, this case
moves there and carries the advance. Making it carry a state now would undo the
property that a refusal cannot hand back a half-step, which is the more valuable
of the two. The contract is pinned by a row, so stage 9 has to change it
deliberately rather than discover it.

`Close` is last, because it drags `closeFd` — 217 lines, refusals of both kinds,
and two callers besides `Close` itself.

**`Close`'s prerequisite went first, on its own.** `closeFd` reaps the inode a
closing description was the last reference to, which is `forgetIfUnheld`, which
is `pinnedInodes`. Both were PawPrint's, both are pure POSIX object-lifetime
rules over the filesystem and the process's own references, and neither reads a
CLR-side field — so they move, and `closeFd` cannot move before them without
taking them as callbacks, which is the dependency injection the gospel rejects.

Moving them first rather than inside the `Close` commit is the same argument
this document made for the `UnixSystem` rename: it is a rename-only change at
seventeen production and test call sites, and the *design* work in `close` is
the refusal split. Audited as rename-only by a token-multiset comparison of the
two definitions before and after, which reported exactly the intended signature
substitutions and nothing else.

It also adds the fourth lens, `EmulatedKernel.mapUnix`, beside `mapMachine`,
`mapProcess` and `mapTasks`: an operation spanning all three parts is called
through it, and it is `withUnix ∘ f ∘ unix` composed once rather than at every
call site. Its own row is in `TestUnixSystemProjection` because a `mapUnix` that
dropped its function's result, or wrote back the projection it read, passes both
existing round-trip rows.

**The tests for the moved rules stayed at PawPrint's altitude**, and that is a
known cost rather than an oversight: `TestEmulatedKernelInodeLifetime` builds its
kernels from `EmulatedKernel.initial`, and the library still exposes no
constructor of its own (the gap recorded above). Two rows at the new altitude
went into `TestUnixSystemStep` — an unnamed inode a descriptor holds, and one
nothing holds — so that the pair is exercised by a client that is not PawPrint;
moving the rest waits for the constructor stage.

**`Close` landed last, and it is the one the design's own census under-read.**
The stage's six syscalls carry eight `failwith`s between their match arms;
`closeFd` carries four more, and the split between them is the whole point. Three
are measured gaps — Linux's last port descriptor with a parked waiter, Darwin's
any port descriptor with one, and a listener close that would RST an unaccepted
connection's live client — and became `CloseRefusal`. The fourth, a descriptor
naming a socket the table does not hold, stays a `failwith` inside the library:
it is a corrupted system, and handing it back as a lawful outcome would let a
client catch it and continue with the corruption alongside.

**`CloseRefusal` is generic in `'Task`, and so `SyscallRefusal` had to become
generic too.** Two of the three refusals are about a task parked in a wait, and
the alternative — the refusal names only the port, and the client re-finds the
waiter — is wrong for a measured reason: nothing stops two tasks parking on the
same port, `Map.tryPick` chooses one, and a client repeating the search could
name a different one from the one the refusal is about. That is a diagnostic
naming the wrong thread, which is worse than a type parameter. The blast radius
was two types and one test row.

**Turning the refusals into data made three tests stronger.**
`TestSocketEventDelivery` pinned all three by catching an exception and matching
its message, which passes for any of the three; they now match the refusal's own
case, and the two port rows assert *which* task the refusal names. The three
`failwith` texts did not disappear: their measured half is the library's
`describe`, and their "what to build instead" half is PawPrint's
`closeRefusalMessage`, shared by all three entry points so that the same refusal
reads the same way whichever one a guest went through.

**The two non-syscall callers hoisted with it**, as `truncateAt` predicted:
`SystemNative_CloseDir` and `SystemNative_CloseSocketEventPort` both call
`close(2)` underneath, so all three entry points now call `UnixSystem.close` and
differ only in how they encode its answer — `-1`-and-errno, `0`, or a PAL code.

**One numbering changed, and it is asserted rather than assumed.** `Close` and
`CloseSocketEventPort` reported EBADF through the portable `UnixError.toRawErrno`
while `CloseDir` used the flavour-numbered `toRawErrnoUnder`; the shared
`withErrno` makes all three the numbered form. Identical for every portable
errno, which is the reason such a move is safe — so `TestUnixError` now states
it, over every portable case and both flavours, instead of leaving it implied.

**Test scaffolding: `KernelSyscall`.** Twenty-two call sites across five fixtures
drove `EmulatedKernel.closeFd` against a whole kernel. Each would have become a
projection, a library call and a write-back, and a copy that dropped the
write-back would leave its own fixture asserting against a state the syscall
never produced — silently, and only there. One definition instead, in a module
those fixtures share.

**Mutation found a hole the fast suite had.** Turning `close`'s EBADF into a
success leaves the whole 3057-test PawPrint suite green: nothing there closes a
descriptor that is not open. `SocketFuzz` has an EBADF arm, but its generator is
constructive — it only closes a slot it knows holds a live fd — so that arm is
unreachable by construction rather than merely unexercised, and it now says so.
The row that kills the mutant is the new one at the library's altitude, which is
the clearest instance so far of what that altitude is for.

#### The errno PAL cluster, retired

Stage 3.5 chose containment over splitting on the strength of a promise that
stage 7 would retire the residue, and the re-scoped stage 7 could keep only the
`UnixError` part of it. This is that part: nine of the seventeen allowlisted
definitions, and the whole cluster the plan assigned here.

`UnixError` carried two numbers per case — the raw `<errno.h>` value, which is
what a kernel states, and .NET's `Interop.Error` value, which is one client's
encoding. The PAL half is now `WoofWare.PawPrint`'s `UnixErrorPal`.
`palOfRawErrno`/`palOfRawErrnoUnder` moved whole rather than being split: they
are `SystemNative_ConvertErrorPlatformToPal`, a shim function rather than a
kernel one. Two measurements made the cut clean — **the library read the PAL
column in exactly zero places**, and all 75 production uses of it were in one
file.

`isPortableRawErrno` and `isUnambiguouslyNonStandardRawErrno` became public.
They state which errno numbers every Unix agrees on, which is POSIX content
rather than PAL content, and the converter cannot be written without them.

**The mirrored-table cost, and why it is not the cost it looked like.** Stage
3.5's option (a) was rejected partly because splitting leaves two exhaustive
matches over `UnixError` that the compiler keeps *complete* but cannot keep *in
agreement*. That framing had the oracle wrong. Agreement with the library was
never the property that mattered: the PAL column's authority is upstream's
`Interop.Errors.cs`, and `TestUnixErrorPal` re-derives all 47 values from the
pinned source exactly as the joint table's test did. A wrong number is caught by
the same authority as before, and a missing case by the compiler. Confirmed by
mutation: a single wrong PAL row is caught by the upstream-derived row and by
nothing else in either suite.

Audited by parsing both columns out of the old file and the two new ones — the
PAL values and the raw values are each identical to what the branch point held.

**One thing the move exposed rather than fixed.** The `UnixError` docstring
claimed the type carried two numbers; rewriting it forced the question of what
the type's *membership* rule is, and the honest answer is that the vocabulary
was chosen against one client too: `ENOTBLK` is absent because .NET's enum has
no name for it. That is now stated in the docstring rather than left implicit. A
second client wanting `ENOTBLK` adds the case; nothing about the design refuses
it.

**And one it deliberately did not touch.** The library's prose still says
"PawPrint" in 106 places, which is a different residue from the encodings this
check counts, and the `pal-residue` check cannot see it — a docstring is not a
definition. **It is not to be swept.** Patrick will read the whole library by
hand once the extraction finishes, ahead of releasing it, and the wrong names
are the marker for what he has not yet checked; removing them mechanically would
destroy that marker while leaving the prose unread.

#### Correctness oracle

* **A new `TestUnixSystemStep.fs` in `WoofWare.PosixKernel.Test`** driving the
  primitives and `step`: the first test that uses the library the way a second
  client would. One row per errno arm of the six, since the sad paths are the
  shape's real exercise.
* **One row per distinct refusal reason** — of which there are more than the
  eight `failwith`s suggest, since `refuseDarwin` is one `failwith` behind five
  call sites. This tier is the only place they are all reachable: CI's guests run
  the Linux flavour, and a test here can construct a Darwin `UnixSystem`
  directly. That reachability is the whole point of the new altitude.
* **A row for `Failed` changing the system**: assert the descriptor table
  advanced after a failing `flock`. It is the design's most distinctive claim and
  the first draft's oracle did not test it.
* **A host-differential row for each of the six**, in the manner of
  `TestVirtualFileSystemAgainstHost`, so the new altitude keeps the oracle it had.
* **An equivalence assertion for errno numbering**: `Dup` and `Close` use
  portable `UnixError.toRawErrno` today (`:4950`, `:4975`) while `LSeek` and
  `FLock` use `toRawErrnoUnder`. A shared `failingWith` changes the first two to
  the numbered form. Identical for EBADF, but assert it rather than assume it.
* **The existing guest fixtures, unchanged.** The whole claim is that no guest
  can tell.
* **Mutation**, per the skill: break one errno arm of each of the six and confirm
  a row dies.

#### What this stage deliberately does not do

No `UserBuffer`, no writes component, no `WouldBlock`. Each waits for the stage
that has a syscall needing it, so that its shape is chosen against evidence
rather than against a sketch — with the one exception recorded above, where the
evidence for the target shape already exists and is written down now.

### Stage 7: the syscall request layer, on the pure syscalls first (as originally specified)

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

### Stage 8 design (proposed)

Stage 8's specification above was written before stage 6, and — as with stage 7
— measuring it against today's code moves several things. This is the census and
the revision.

#### What is actually there

The stage names thirteen syscalls plus "the socket-address entry points".
Measured in `Native/NativeSystemNative.fs`, which holds 73 match arms over 66
distinct entry points (several entry points have more than one signature arm):

| handler | lines | what it carries |
| --- | --- | --- |
| `Open` | 333 | path in |
| `PRead` | 237 | buffer out, explicit offset |
| `Poll` | 230 | array in *and* out |
| `Read` / `Write` | 214 / 217 | buffer out / buffer in |
| `ReadLink` | 178 | path in, buffer out |
| `PWrite` | 164 | buffer in |
| `GetCwd` | 133 | buffer out |
| `OpenDir` / `ReadDir` | 114 / 102 | path in / struct out |
| `GetSockName` | 100 | socket address out |
| `MkDir` / `RmDir` / `Unlink` | 88 / 88 / 86 | path in |
| `FStat` | 88 | struct out |
| `Stat` / `LStat` | 3 / 8 | both delegate to one `statLike` |

Across the whole file there are **47 buffer-pointer arguments and 39
write-throughs**, so the named list is roughly a third of the eventual surface.

#### Three findings that change the specification

**(1) The buffer classification is four-way, and two of the four are refusals
rather than EFAULT.** Decision 2(b) sketches `UserBuffer` as `Null | Unmapped |
Mapped`, and says of `Unmapped` that "a real run takes SIGSEGV; today PawPrint
`failwith`s at each such site". That is false today. `BufferPointer`
(`NativeSystemNative.fs:16`) has four cases, and `BufferPointer.dereferenceable`
answers `None` — hence EFAULT — for *every* raw address, null or not, because
"real `write(2)` and `getcwd(3)` alike answer EFAULT for both, having performed
no I/O". What actually crashes is the other two:

* `Symbolic` — the address of a method table or type handle. A real runtime has
  that memory mapped and readable, so the host transfers those bytes; **EFAULT
  would be a wrong answer rather than an approximate one**, and PawPrint has no
  bytes to transfer.
* `Unstatable` — the difference of two pointers into separate storages, which
  names no address at all. Under a platform that screens up front there is
  nothing to compare against the limit, so the answer is not "out of range", it
  is unknown.

Both are measured-but-unmodelled in exactly stage 7's sense, so they are
*refusals*. My first draft concluded they were therefore **the client's**
refusals — only PawPrint can produce either — and that they never reach the
library. **That is wrong, and the review that found it also found the
counter-examples.** Production is client-side; *timing* is not, and after the
move the library owns the order these fire in:

| call today | today's answer | eager client refusal |
| --- | --- | --- |
| `read(badfd, symbolic, n)` | EBADF — the buffer is never even classified | crash |
| `read(stdin, symbolic, n)` | **0** — the stdin arm precedes any dereference | crash |
| `read(dir, symbolic, n)` | EISDIR | crash |
| `write(rdonlyFd, unstatable, n)` | EBADF — the access-mode arm precedes the screen | crash |

Every one of those is a currently-working call that a classification-time
refusal would break, and conditioning the refusal on `screensUserBufferUpFront`
does not help: it fixes *whether* and not *when*, and the descriptor checks that
must precede it are exactly what moves into the library. **You cannot condition
your way out of an ordering problem once you no longer own the order.**

So the classification is four-way after all, and the two extra cases are library
vocabulary rather than PawPrint's — a client whose memory is not byte-addressable
is a general situation, not a CLR one:

* `Opaque` — a real user address the client cannot name or transfer bytes
  through. Passes every screen (`faultsBeforeOperation` already answers `false`
  for it), and refuses at the transfer, which is where PawPrint runs out of
  bytes.
* `Addressless` — no address at all. Refuses at the *screen* on a screening
  platform, because there is nothing to compare against the limit, and at the
  transfer otherwise.

Both refusals are the library's `Error`, returned at the step where the buffer is
consulted, so every arm above keeps its answer. Both still satisfy "a refusal
carries no state": for `read` and `write` the refusal points precede every state
change, the offset advancing only after the copy.

**And the classification is two-way, not three.** Decision 2 gives `Null` its
own case. Measured: every one of the sixteen places that distinguishes
`RawAddress 0UL` from any other raw address is a **C shim's own null screen**,
executed before it calls the kernel at all — `SystemNative_FcntlGetIsNonBlocking`
("the C tests the pointer before its first `fcntl`, so a null pointer with a
nonsensical descriptor is EFAULT, not EBADF"), `GetSocketAddressSizes` (all four
out-parameters screened together, before any is written), and fourteen more in
the same family. No *kernel* path in the modelled set tells them apart: the
up-front range check is arithmetic that address 0 passes like any other low
address, and `dereferenceable` collapses every raw address to EFAULT.

So `Null` is a shim concept that leaked into the kernel model, and the library's
classification should be `Unmapped of address : uint64 | Mapped`. The shim's null
screens stay in PawPrint, where their authority — the C source — lives. This is
the same mistake as `Symbolic`, one level up: a distinction real somewhere else
being attributed to the kernel.

**(2) The buffer is an input consulted at several measured points, not only an
output effect.** `SystemNative_Read`'s order, every step of it measured:

1. `bufferSize < 0` → EINVAL — the shim's guard, ahead of the descriptor
2. descriptor, access mode, object kind → EBADF / EINVAL / ENXIO
3. **the up-front buffer screen — Linux only** (`screensUserBufferUpFront`)
4. stdin → 0, *without touching the buffer*
5. `EISDIR`, *without touching the buffer*
6. the transfer window
7. a zero-length transfer → 0, *without touching the buffer*
8. dereference → EFAULT
9. the copy-out, and only then the offset advance

Three of those steps are "the buffer is not touched here", one is "it is screened
here on one platform only", and one is the transfer. A design in which the buffer
appears only in the *response* can express step 9 and none of the rest. So the
classification crosses **in**, and is consulted; the bytes come **out**. That is
decision 2(b)'s intent, confirmed, but the plan's phrasing ("the response carries
an ordered list of effects") describes only half of it.

`faultsBeforeOperation` (`:334`) is already most of the library function this
needs: it consults `UserBufferCheck` and `screensUserBufferUpFront`, both
library, and differs only in being typed over PawPrint's four-way DU.

**(3) There are two kinds of copy-out, and conflating them would grow the PAL
residue this work is supposed to shrink.**

* **Opaque bytes at a caller-supplied address** — `read`, `pread`, `readlink`,
  `getcwd`. The library knows the bytes; the client knows where they go.
* **A structured value the client encodes to its own ABI** — `fstat`/`stat`/
  `lstat` fill a guest `Interop.Sys.FileStatus`; `readdir` fills a `dirent`;
  `getsockname` fills a `sockaddr`. `writeFileStatus` (`:1055`) writes at ABI
  offsets, checks `CliType.sizeOf` against the guest's own struct, and resolves
  it through a `ConcreteTypeHandle` — a CLR concept end to end.

If stage 8 gave the library one generic `writes : (address, bytes)` component,
`fstat` would have to emit `FileStatus` bytes, and the library would acquire a
.NET struct layout — a **new** entry in `scripts/pal-residue-allowlist.txt`, in
the stage whose job is to empty it. So the second kind must cross as a typed
value and be encoded by the client.

#### The open decision: how the answer carries a payload

**(A) One `SyscallAnswer`, with a generic `writes` component**, as decision 2(b)
sketches. Uniform, and `step` keeps one surface. Costs: every write needs a
destination tag once a syscall has two output buffers (`getsockname` has address
and length; `poll` and `recvfrom` likewise), and finding 3 means a *second*
mechanism is needed anyway for the structured answers — so the generic list buys
uniformity for one of the two kinds and no uniformity overall.

**(B) Per-syscall typed answers, and no generic writes.** `read` answers
`ImmutableArray<byte>`; `fstat` answers a `FileStatusAnswer` record; `getcwd`
answers a path. The client places every one of them, which it must do anyway.
`SyscallAnswer` becomes a DU whose cases enumerate the *shapes* of answer rather
than one shape with an effect list. Costs: `step`'s uniform surface weakens —
a client logging or replaying through it must match on more cases — and the case
set grows as syscalls land, which is a closed-set-that-grows.

**(C) (B), plus a two-phase surface for the source direction.** Forced by a
defect in (B) that review found and that the code states outright. `Write`'s
handler defers decoding its buffer on purpose:

> Decoding the `buffer` pointer is deferred until we are about to dereference it.
> `Common_Write` is documented to perform no dereference for `bufferSize < 0`
> (ERANGE bail) or `bufferSize = 0` (no-op on every Unix we model), so a guest
> calling e.g. `SystemNative_Write((IntPtr)1, (byte*)123, 0)` must succeed on
> PawPrint as it does on the real CLR — eagerly decoding `buffer` would crash
> here.

A `Mapped of bytes` case forces the client to extract *before* it calls, so
`write(badfd, threeByteBuf, 10)` — today EBADF, because the room check runs only
after every descriptor check passes — becomes a host crash, as does every
byte-addressability failure in the per-byte walk. So for source-carrying
syscalls the surface is two calls: an **admission** (`fd`, count, classification)
answering how many bytes the kernel will take, and a **commit** taking the bytes
the client then extracted. The admission's answer is a value the library
constructs and the client cannot forge — `private` on a record is assembly-scoped
in F#, which is exactly the boundary here — so a commit cannot be reached without
the checks having passed.

**Recommended: (C)**, which is (B) everywhere except the source direction. (B)'s
argument stands on finding 3: the generic effects list was proposed when the only
imagined copy-out was opaque bytes, and against the measured set it covers half
the cases while forcing the library to learn a .NET struct layout for the other
half. Its defect is confined to buffers the client reads *from*, and two-phase is
the smallest thing that keeps extraction after the checks. Both keep the "no
illegal states" property that made stage 7's per-syscall functions the primitives
and `step` the sugar: a `read` that returned a `FileStatus` would not typecheck,
and a commit without an admission would not either.

**One asymmetry left open on purpose.** Stage 7 recorded that a failed
`epoll_wait` writes 0 through `*count` on Linux and −1 on Darwin — so `Failed`
carries writes too. (B)/(C) type the *success* payloads per syscall and leave
`Failed of UnixError` untyped, which that evidence falsifies. Nothing in stage 8
needs it: the first syscall with a failure-write is `poll`/`epoll`, which is
stage 9's. Written down so that stage 9 finds a known gap rather than a surprise.

Concretely:

```fsharp
/// Where a buffer argument is, as far as this kernel's own address check can
/// see. No `Null` case — that is the shim's concept, above — and no address on
/// `Mapped`, because `faultsBeforeOperation` never asks for one.
type UserBuffer =
    /// A raw address naming no storage: EFAULT, whenever the syscall gets round
    /// to looking.
    | Unmapped of address : uint64
    /// Real storage the client can transfer bytes through.
    | Mapped
    /// A real user address the client can neither name nor transfer through.
    /// Passes every screen; refuses at the transfer.
    | Opaque
    /// Not an address at all. Refuses at the screen where there is one, at the
    /// transfer otherwise.
    | Addressless
```

One type, not two. My draft had a `SourceBuffer` carrying bytes and a
`DestinationBuffer` carrying none, on a make-illegal-states-unrepresentable
argument — but a source buffer must not carry its bytes, so the asymmetry that
justified the split does not exist.

#### A second genus of refusal, which should be named rather than absorbed

Stage 7's definition of a refusal is "this library has measured what real kernels
do here and found no single answer to give". Three of stage 8's refusals do not
fit it. For `Opaque`, for `Addressless`, and for 8b's invalid-UTF-8 path, the
kernel's answer is perfectly well known — transfer the bytes at that address;
look up those raw bytes — and what is missing is a *representation*, in the
library's model or in the client's memory.

The stretch is right: both genera are "no answer to give", both must not be
errnos, and both must carry no state. But `describe`'s contract says the library
reports what it *measured*, and for these it reports what its model cannot say.
Naming the second genus is what stops the messages overclaiming a measurement
they do not have.

#### Paths force a question larger than the syscall move

`parseGuestPathBytes` (`:827`) does two things. ENAMETOOLONG is a library rule
(`PathLimits.pathMaxBytes`). The other is a UTF-8 decode, with a `failwith` for
invalid input whose message says it plainly: "a Unix kernel looks up the raw
bytes, but PawPrint models a filename as a .NET string". That model is the
*library's* — `UnixPath.parse` takes a string — so the limitation is the
library's too, and the `failwith` is sitting on the wrong side of the boundary.

By the raw-versus-parsed rule a path should cross **raw**, as
`ImmutableArray<byte>`: the library can classify it (it owns `PathLimits`) and
owns the consequence (ENAMETOOLONG). Doing that hands the library the non-UTF-8
refusal, which is correct — but it is a change to the stated contract of the
library's filename model, not a syscall move, and it should land **before** any
path syscall rather than inside one.

#### The increments

Ordered so that each has an oracle before the next depends on it.

* **8a — the buffer vocabulary, and the six call sites that already use it.**
  My draft said "the screen moves into the library" and "two existing call sites
  consume it". Both were wrong. `UserBufferCheck.faultsBeforeOperation` is
  *already* in the library (`SimulatedUnixPlatform.fs:276`), and PawPrint's
  same-named function is a thin typed adapter over it; and there are **six**
  call sites, not two — `PRead` (`:3819`), `PWrite` (`:4008`), `Read` (`:4170`),
  `Poll` (`:6456`) and `Write` (`:6904`, `:6956`). So 8a as drafted was
  vocabulary with no library consumer until 8c, which is speculative generality
  by this plan's own standard.

  Redefined: 8a introduces `UserBuffer` with the four cases above, gives the
  library the two refusals *with their timing*, and re-plumbs those six sites
  onto it. That is real, testable work — the finding-1 table is its oracle — and
  it must land before 8c, because `read` is where a wrong classification arity
  becomes unfixable without a surface break.
* **8b — paths cross raw.** `UnixPath` gains a bytes-shaped entry point, and the
  non-UTF-8 refusal moves into the library as a refusal case. Oracle:
  `TestGuestPathBytes`, plus the ENAMETOOLONG boundary rows, which are the ones
  that can tell a byte budget from a character budget.
  Two things implementing it settled. **Where it lives**: `PathArgument` sits in
  `VirtualFileSystem.fs` beside `PathLimits`, because that is where `PathLimits`
  and its two supporting types are, and `UnixPath.fs` compiles before them. The
  alternative was moving all three types up into `UnixPath.fs` so that every path
  concept lives in one file — a ~150-line rename-only move, which is exactly the
  thing this plan keeps refusing to bundle with a design change. `PathLimits`'s
  own docstring already says its rule is "enforced at the syscall boundary rather
  than in the walk", so the file was never only about the image.

  **And the refusal carries no payload.** The bytes are the client's — it read
  them — so a `NotUtf8 of bytes` would have the library hand back something the
  caller already holds, purely so the caller could render it. The hex rendering
  stays in PawPrint's half of the message, along with the entry point's name and
  the reachability sentence, and the library's half is the one fact the client
  cannot state: that this kernel models a filename as characters and has no such
  name to look up.

* **8c — `read`**, first because its measured ordering exercises every part of
  (2): three buffer-untouched short-circuits, a platform-dependent up-front
  screen, and a copy-out. `pread` follows as the same operation with an explicit
  offset, which is the whole difference between them.

  Worth saying in advance, because it looks like scope creep when it arrives:
  moving `read` also moves its epoll-port arm (EINVAL on Linux, ENXIO on Darwin)
  and its socket arm, and the socket arm is a *refusal* — "PawPrint models no
  socket connection state, and `read(2)` on a socket is an answer about exactly
  that". After the move that gap is the library's own, so a socket-shaped
  refusal lands here, ahead of stage 9's socket work.

  Three things implementing it settled.

  **`read` is not reachable through `step`, and that is the (B) cost arriving.**
  Its answer carries bytes, so `SyscallAnswer` would have to grow a shape for
  them — and nothing consumes that shape, because no client logs or replays a
  buffer-carrying syscall yet. Inventing an encoding before there is a client to
  be wrong about is the speculative generality this plan keeps refusing; the
  first thing that genuinely needs it chooses it. `step`'s docstring now says so
  rather than implying total coverage.

  **The empty answer is load-bearing, and the type does not enforce it.**
  `ReadAnswer.Completed ImmutableArray.Empty` means "moved nothing *and did not
  touch the buffer*", which is three of `read`'s measured steps. A client that
  resolved its pointer before checking for empty turns `read(f, NULL, 5)` at
  end-of-file from 0 into EFAULT, and a symbolic buffer from 0 into a crash. The
  docstring states it and the client guards on it; a shape that made it
  structural would need a third case for what is otherwise the same answer.

  **The screen really does precede the shortcuts, and only one buffer can show
  it.** An addressless buffer under Linux is refused *even for a read that would
  have moved nothing*, because the address check runs before the transfer window
  is computed; under Darwin the same call reaches the shortcut and answers 0.
  Two rows pin that pair, and they were written only after the first draft of
  them asserted the opposite and failed — the library was right and the test was
  wrong, which is the outcome to want from a faithful transcription.
* **8d — `write`**, the source-buffer direction, and the only place the
  two-phase surface of (C) is built.

  **Implementing it removed the witness.** (C) proposed an admission carrying an
  unforgeable token, so that a commit could not be reached without the checks
  having passed. What the code wanted is simpler: `admitWrite` answers every
  question a write settles *without reading the buffer* — the descriptor, the
  object kind, the screen, the zero-length no-op, the faulting address — and
  otherwise says how many bytes to extract. `write` then takes the fd and the
  bytes and **no buffer at all**. A signature that cannot ask a buffer question
  is a stronger guarantee than a token that says the questions were asked, and
  it needs no private constructor. `write` still answers the descriptor
  questions itself, so a caller that skips the admission gets a kernel's answer
  rather than an inconsistent one.

  `admitWrite` returns no system, which is the property that makes the pair
  safe: everything a write does before the copy is a question, so a caller may
  ask and then decline. Its own row asserts that.

  `pread`/`pwrite` follow each of 8c and 8d as small increments of their own,
  the `p`-variant being the same operation with an explicit offset and no
  description update. Splitting them that way is what 8c did with `read`, and
  keeps each diff about one thing.

  **Review found a real ordering error, and measuring it found a second.**
  `admitWrite` refused a socket before the buffer screen, which is what the
  handler did before the move — so the transcription was faithful and the
  *original* was wrong. Measured on both platforms (a small C probe over an
  unconnected TCP, UNIX-stream and datagram socket, with `SIGPIPE` ignored,
  run on macOS and in a Linux container):

  | call | Linux | Darwin |
  | --- | --- | --- |
  | `write(socket, (void*)-1, 1)` | **EFAULT** | ENOTCONN / EDESTADDRREQ |
  | `write(socket, (void*)-1, 0)` | **EFAULT** | ENOTCONN |
  | `write(socket, buf, 0)` | EPIPE / ENOTCONN / EDESTADDRREQ | ENOTCONN / EDESTADDRREQ |
  | `read(socket, (void*)-1, 1)` | **EFAULT** | ENOTCONN |
  | `read(socket, (void*)-1, 0)` | **EFAULT** | ENOTCONN |
  | `read(socket, buf, 0)` | **0** | ENOTCONN |

  So Linux's screen precedes the object's own operation for sockets exactly as it
  does for files, and the fix is the library's existing `screensUserBufferUpFront`
  fact applied in the right order rather than a new claim. The zero-length no-op
  does *not* move with it: `write(socket, buf, 0)` is the socket's own error on
  both, so the socket refusal sits between the screen and the no-op.

  The probe's first run looked like a broken container — exit 141 with no
  output. That was the measurement: 141 is 128 + SIGPIPE, and Linux raises
  SIGPIPE alongside EPIPE for a write to an unconnected TCP socket. Ignoring the
  signal produced the table.

  **`read` had the same error and is already merged.** Rows 4-6 above are not
  what stage 8c does: it refuses the socket ahead of both the screen and the
  zero-length shortcut, so `read(socket, buf, 0)` crashes where Linux answers 0.
  It refuses rather than answering wrongly, so it is a "declines more than it
  needs to" defect rather than a divergence.

* **8e — `read`'s socket arm moves after the screen**, applying the same measured
  ordering. Widening the probe first, because a rule drawn from one socket kind
  is not a rule: on Linux `read(sock, buf, 0)` is **0** for an INET stream, a
  UNIX-domain stream *and* a datagram socket, while all three answer ENOTCONN at
  length 1 — so the short-circuit is about the length rather than about the
  socket. The socket event port does not share it (`read(port, buf, 0)` is EINVAL
  on Linux, like every other length), which is why the port arm stays ahead of
  the screen where it already was.

  Darwin has no such short-circuit: its stream sockets answer ENOTCONN at length
  0 too, and only a datagram socket answers 0. So Linux's zero-length answer is
  knowable without modelling connection state and Darwin's is not, which is
  exactly the split the refusal already draws — the flavour match is a
  consequence of that rather than a new axis.

  One probe row was spoiled and is not in the table: `read(file, buf, 0)` on
  Darwin reported EBADF, because the probe opened `/etc/hostname`, which does not
  exist on macOS, so the descriptor was -1. The Linux half of that row stands;
  the Darwin half says nothing.

  **Review then asked the right question about the wrong axis, and widening the
  probe answered both.** The objection was that a zero-length read of a Linux
  datagram socket with a datagram queued might not be 0 — that the rule was drawn
  from one socket *phase* as well as one kind. Measured across every phase this
  kernel can produce:

  | socket state | Linux | Darwin |
  | --- | --- | --- |
  | INET stream, idle | 0 | ENOTCONN |
  | UNIX stream, idle | 0 | ENOTCONN |
  | datagram, idle | 0 | 0 |
  | INET stream, bound not listening | 0 | ENOTCONN |
  | INET stream, listening | 0 | ENOTCONN |
  | stream, connected, nothing queued | 0 | 0 |
  | stream, connected, a byte queued | 0 | 0 |
  | datagram, connected, empty | 0 | 0 |
  | datagram, connected, one queued | 0 | 0 |
  | stream, peer closed | 0 | 0 |

  **Linux answers 0 in every state**, so keying the arm on the flavour alone is
  right and the objection is falsified — including the queued-datagram case it
  named. Darwin's answer is 0 too except for a stream socket that is *not*
  connected, and separating those means modelling exactly the connection state
  this refusal exists to avoid, so Darwin declines the whole class: it
  over-refuses the connected cases and never answers wrongly.

  A row now drives all five constructible phases, at length 0 and at length 1, so
  a future rule drawn from one phase fails.
* **8f — `pread`**, the same operation as `read` with an explicit offset, and
  the increment that shows how little of `read` that actually leaves in common.
  What the two genuinely share — the transfer window, the shortcut that touches
  no buffer, and the one point at which the buffer must hold bytes — is now a
  private `readFileAt`, so a future fix to a measured rule cannot land in one
  syscall and miss the other. What is *not* shared is the ordering, which is
  where all of `pread`'s content is.

  **`pread` needs a seekable object, and that changes the answer for three of
  the five descriptor kinds.** A pipe, a socket and a socket event port are all
  ESPIPE, where `read` gives EBADF, a refusal, and EINVAL/ENXIO respectively.
  The socket one is the interesting case: `pread` needs **no socket refusal at
  all**, because a socket's *read operation* is an answer about connection state
  while its *seekability* is not — every socket is unseekable whatever it is
  connected to, so `pread` never reaches the operation to ask. So its signature
  is `Result<ReadAnswer, BufferRefusal>`: the only refusal genus it can produce
  is the buffer's, and an arm for a socket refusal would need an invented
  message to fill.

  **And it returns no system.** A `pread` changes nothing in one: it moves no
  file offset, and nothing in this kernel moves `atime` (`InodeTimes.Access` is
  stored and only `createdAt` ever sets it). The signature says so, which is the
  same move 8d made in taking no buffer — a shape that cannot express the wrong
  thing beats a comment asking a caller not to. PawPrint's handler therefore has
  nothing to write back and so cannot forget to, and `withErrnoOnly` is the
  errno half of `withErrno` without the write-back that would claim otherwise.

  **The ordering, measured on macOS and in a Linux container.** A *single*-fault
  input agrees on both flavours, so only an input with two things wrong at once
  separates them:

  | input | Linux | Darwin |
  | --- | --- | --- |
  | negative offset alone | EINVAL | EINVAL |
  | negative offset + bad fd | EINVAL | EBADF |
  | negative offset + pipe | EINVAL | ESPIPE |
  | negative offset + socket | EINVAL | ESPIPE |
  | negative offset + port | EINVAL | ESPIPE |
  | negative offset + O_WRONLY | EINVAL | EBADF |
  | negative offset + directory | EINVAL | EINVAL |
  | negative offset + bad address | EINVAL | EINVAL |

  Linux validates the offset before it looks the descriptor up at all
  (`do_pread` checks `pos < 0` ahead of `fdget`); Darwin resolves the
  descriptor, its seekability and its access mode first. The last two rows are
  the control: EISDIR and the buffer screen both follow the offset check on
  *both*, so only the descriptor steps move and one flag suffices rather than
  two orderings.

  The second table is the ESPIPE/EBADF tie, which the flavours break
  differently because a pipe's write end fails two tests at once:

  | descriptor | Linux | Darwin |
  | --- | --- | --- |
  | pipe read end (unseekable) | ESPIPE | ESPIPE |
  | pipe write end (also unreadable) | ESPIPE | EBADF |
  | regular file O_WRONLY (seekable) | EBADF | EBADF |

  The third row is the control that says this is about the tie rather than
  about unreadability generally.

  **The probe was widened before the rows were written, and it changed two of
  them.** The handler being transcribed had measured sockets only with a valid
  buffer at a non-zero length, and had not measured a socket or a port against a
  negative offset at all — so four of the rows above were symmetry arguments
  rather than measurements. Measuring found them all correct, which is the
  outcome to want but not one to assume: 8d and 8e were each a rule drawn from
  one axis that turned out to need another. Two rows were added to the tests as
  a result. One probe row is deliberately absent: nothing here measures
  `pread` against a *symbolic* buffer, which is PawPrint's concept and not a
  kernel's.

  Eight mutants, all killed by the row that names the rule: the offset-order
  flag flipped; the port answering `read`'s errno; Darwin's tie-break dropped;
  the access-mode check deleted; the screen made not to answer; the
  moved-nothing shortcut made to consult its buffer; `read` advancing by what
  was asked rather than what moved; and `pread` reading from the start rather
  than from its argument.

* **8g — `pwrite`**, which stands to `write` as 8f stands to `read`: an explicit
  offset, no description update, and the two-phase admission unchanged. Listed
  as its own bullet rather than left implicit in 8d's prose, for the reason the
  `getcwd` bullet below gives about itself — a list that does not partition its
  own census table is not a plan.

  **The temptation this increment exists to resist is transcribing 8f's order
  with the words swapped, and measuring says that would be wrong in the very
  first step.** `pwrite` validates a negative offset *ahead of everything, on
  both flavours*, where `pread` does so only on Linux. Nine second faults give
  way to it on both — a bad descriptor, either end of a pipe, a read-only file,
  a directory, a socket, a socket event port, an unscreenable address and a zero
  length — so the per-flavour flag 8f needed is not merely unnecessary here, it
  would fail every one of those rows.

  The seekability tie *is* 8f's mirrored, with the roles swapped: standard input
  is now the descriptor that fails two tests at once, being neither seekable nor
  open for writing.

  | descriptor | Linux | Darwin |
  | --- | --- | --- |
  | pipe write end (unseekable) | ESPIPE | ESPIPE |
  | pipe read end (also unwritable) | ESPIPE | EBADF |
  | regular file O_RDONLY (seekable) | EBADF | EBADF |

  **`PWriteRefusal` is `WriteRefusal` without its socket case**, rather than the
  same type. A socket is unseekable, so `pwrite` answers ESPIPE and never
  reaches the socket's write operation to ask about connection state — the same
  argument 8f made for `pread`, and the reason the plan keeps stating it is that
  the two syscalls' refusal *sets* differ even though their answer sets look
  alike. Sharing the type would hand every client an arm it could not reach and
  would have to invent a message for. The length refusal's sentence is shared
  between the two types, being one fact reached from a different offset.

  It does return a system, unlike `pread`: the description's offset does not
  move, but the file's contents and timestamps do. So the pair of signatures
  states exactly which of the two directions writes.

  PawPrint's `commitFileWrite` goes with the move. `UnixSystem.pwrite` was its
  last caller, and it was the last place the set-ID rule and the timestamp rule
  were applied outside the library.

  Ten mutants, all killed by the row that names the rule — including the one
  this increment is really about: giving `pwrite` `pread`'s per-flavour offset
  flag dies on the nine-row table above.

* **8h — `fstat`**, the smallest structured answer, and what settles (3) against
  a real encoder.

  **`stat`/`lstat` do *not* come along for free, and the draft was wrong to say
  so.** What they share with `fstat` is the *encoder*, which stays in PawPrint
  either way, being .NET's ABI. What they do differently is the whole of the
  path side — reading a NUL-terminated name out of guest memory, parsing it,
  resolving it under a symlink policy — and only the resolution part crosses.
  So they are 8i, and 8h is `fstat` alone. Meanwhile `statLike` keeps its own
  resolution and asks the library for the answer, which is `statOf`.

  **The answer is a record of POSIX facts, and it omits rather than zeroes.**
  There is no `st_nlink`, no `st_blksize`, no `st_blocks` and no BSD `st_flags`
  in `FileStatus`, because this kernel models none of them and a zero in a
  client's struct is indistinguishable from a measurement. A client whose ABI
  has those fields writes what its own runtime writes for a filesystem with no
  such notion — which is exactly what PawPrint's encoder now does, in its own
  comments, rather than the library pretending to an answer.

  `BirthTime` is an option for the same reason and it is the sharper case: on a
  Linux flavour `stat(2)` has no such field, and `None` says so where a zero
  would be a claim that the inode was born at the epoch — a distinction that,
  for an inode *actually* created at the epoch, no zeroed field could carry.
  PawPrint turns the option into the pair the BCL reads: a cleared
  `FileStatusFlags.HasBirthTime` and a zeroed field, which is what `pal_io.c`
  writes under `#else`.

  **`st_mode` is one `int`, composed by the library.** The alternative was a
  `FileType` DU plus `PermissionBits`, with the client assembling the two bands
  — more structured, and the project's usual preference. Rejected because the
  assembly is correctness-critical knowledge that belongs where the type bits
  are defined: splitting it moves a chance to get it wrong to every client, and
  buys only a match-instead-of-mask for a question no client currently asks. It
  can gain an accessor later without a surface break, which the reverse could
  not.

  **Three refusals**, one per descriptor this kernel holds no inode for: a
  standard stream, a socket event port, a socket. One genus — "a real kernel
  answers this and the model has no inode to answer it from" — but three
  `describe`s, because their measurements are different: the flavours agree on
  not one field for a port, and only Linux gives a socket an inode at all. A
  shared sentence would hand a client rendering one of them the other's
  evidence.

  `EmulatedKernel.simulatedDeviceId` moves to `VirtualFileSystem.deviceId`, the
  encoder being its last reader. A public deletion from a published package.

  Measured, on macOS and in a Linux container: `fstat(999, (struct stat*)-1)` is
  EBADF on both, so a bad descriptor beats a bad address and the output pointer
  is decoded only on the path that writes through it; and a failed `fstat`
  leaves the caller's struct byte-for-byte untouched on both, which is what
  `ConvertFileStatus` relies on. Twelve mutants, all killed — including the two
  that drop one band of `st_mode`, and the one that compares the device id
  against the constant it came from, which is why that row asserts the literal.

* **8i — `stat` and `lstat`**, which is the path resolution crossing: a library
  `resolvePath` over `VirtualFileSystem.resolveFull`, taking the cwd inode, the
  privilege and the limits that `UnixSystem` already holds. PawPrint keeps the
  guest-memory half — reading a NUL-terminated name within `PATH_MAX` — because
  that is its memory and not a kernel's. Once it exists, `mkdir`/`rmdir`/
  `unlink` and `open` all want it, which is why it is its own increment rather
  than a rider on one of them.

  `stat` is `resolvePath` plus `statOf`, the symlink policy being the entire
  difference between it and `lstat`. It **cannot be refused**, unlike `fstat`:
  every inode a path resolves to is one this filesystem holds, so the three
  inode-free descriptors `fstat` refuses for are unreachable from a path. The
  signature says so by returning a bare `FileStatusAnswer` rather than a
  `Result` — the same move 8f and 8g each made for their own refusal sets, and
  the third time the plan has found that two syscalls with matching *answers*
  have different *refusals*.

  **A branch did not survive the move, and mutation testing is what found it.**
  Replacing the start directory in the rooted arm changed no test, and reading
  `resolveFull` says why: it asks `isRooted` itself and starts at the root
  regardless of what it is handed. So the `if isRooted then root else cwd` that
  PawPrint had been computing was choosing a value the walk discards. Deleted
  rather than given an invented test, with the reason recorded at the call —
  which is the outcome to want from a surviving mutant, and worth stating
  because the first instinct on one is to reach for another row.
* **8j — `mkdir`, `rmdir`, `unlink`**, three nearly identical path syscalls that
  land together once 8b exists.

  Their rules were already the library's — `MkDirRules`, `UnlinkRules`,
  `RmDirRules` and their verdicts — so what was left standing between those
  rules and the syscall boundary was three handler bodies. With 8i's walk
  across, each is now resolve, verdict, commit, reap.

  **None of the three can be refused**, every outcome being a success or an
  errno, so they return a bare `SyscallAnswer * system`. Being payload-free they
  also join `step`, which the buffer-carrying syscalls could not: that is the
  first time since stage 7 that the dispatcher has grown, and it is the shape
  the `step` docstring predicted would be able to.

  `unlink` and `rmdir` carry `forgetIfUnheld` with them, which is the part they
  add over the filesystem's own `unbind`.

  On PawPrint's side the three handlers collapse into one `pathSyscall` — decode
  a NUL-terminated path out of guest memory, hand it to the kernel, turn the
  answer into the zero or the -1-with-errno the C returns. That is the shape
  they always shared, and the syscall itself is now the only parameter.

  **Three of the seven mutants survived the first battery, and all three were
  rows that could not discriminate rather than rules that were untested.** A
  `mkdir` onto a symbolic link to an *existing* file is EEXIST whether the final
  component is dereferenced or not, so only a *dangling* link separates the two
  readings. `Syscall.MkDir` at mode 0o755 and at 0o777 both become 0o755 under
  the default umask, so a dispatcher that dropped the mode agreed. And the
  `rmdir` ctime divergence — Linux moves the removed directory's, Darwin does
  not — is visible only through a descriptor *held across the call*, an unheld
  inode being reaped with nothing left to ask. Each of those is the same trap in
  a different costume: an input whose two candidate rules agree.
* **8k — `open`** (done; 333 lines), the largest of the file syscalls, and the
  one whose flags are PAL values. **`opendir`/`readdir` are not bundled with it**:
  8h taught that this list's bundlings are worth re-checking, and those two
  return an opaque `DIR*` that PawPrint materialises as guest memory, which is a
  different boundary question from `open`'s. They are 8l.

  Most of the handler is kernel behaviour that simply moves — `CreatingOpenRules`
  and its verdict are already the library's, and 8i's walk is across. What needs
  a decision first is the *flags*, and the plan's own raw-versus-parsed rule
  says only half of it: raw means raw kernel ABI, never PAL, so
  `Interop.Sys.OpenFlags` cannot cross as an integer. What shape it crosses in
  is open. **This needs confirming before the code is written**, because it adds
  a public vocabulary type to a package that is about to be released.

  **Decided: (B), a parsed `OpenFlags` record** — access mode, and a bool per
  `O_CREAT`/`O_EXCL`/`O_TRUNC`/`O_NOFOLLOW`/`O_CLOEXEC`/`O_SYNC`. PawPrint maps
  the PAL bits onto it and the library never sees a numbering.

  The argument that settled it is stronger than the one this bullet originally
  made, which was about where platform knowledge lives (`O_CREAT` is 0o100 on
  Linux and 0x200 on Darwin, and `SimulatedUnixPlatform` is the library's).
  Patrick's objection to (A) is about what an `int` lets the *emulator* do:
  given a bit pattern, a flag this kernel does not model is indistinguishable
  from one it does, so it would silently do something the caller did not ask
  for — the caller believing the bits mean something, and the kernel guessing.
  A record has exactly the fields the kernel acts on, so what is supported is
  legible at the boundary. An `int -> OpenFlags` decoder can be added later if
  something wants one; it cannot be taken away once the surface is a number.

  This also keeps the two shim-level rejections where they belong — an
  unrecognised *bit* is EINVAL and so is an access mode that is none of the
  three, both being the C's own checks rather than any kernel's, and neither
  expressible once the flags are parsed. The cost accepted is that a future
  `fcntl(F_GETFL)` would have to invent a numbering, and that the library
  cannot model a kernel that rejects a flag *combination* by its bits; if
  either arrives it wants a per-flavour numbering *in the library*, which is
  where (B) leaves room for it.

  One rule the shape has to be careful about, and the tests pin it: `O_EXCL`
  crosses **as the caller set it**, not pre-ANDed with `O_CREAT`. That it does
  nothing on its own is a measured kernel fact the library owns, and a client
  that combined them first would leave the library with nothing to be right or
  wrong about.

  Either way the `mode` argument crosses raw and unvalidated, which is settled
  and measured: `SafeFileHandle.OpenReadOnly` passes 0666 even for a read-only
  open of an existing file, so a handler rejecting a nonzero mode without
  `O_CREAT` would refuse the BCL's own read path; and `mode` 0o10777 creates
  0o0755 on both kernels, so a bit above the permission word is dropped rather
  than refused.

* **8l — `opendir` and `readdir`** (done), split out of the old 8k for the
  reason given there. Their own question was where the directory stream lives: PawPrint
  materialises the `DIR*` as guest memory whose address *is* the handle, and the
  `d_name` buffer inside it is sized by an ABI constant — so the stream's
  identity and its bytes look to be on different sides of the boundary, which
  none of stage 8's other syscalls has had to arrange.

  **Reading the code says the boundary is already drawn**, and Patrick agrees:
  the stream *state* is library-side already (`DirectoryStreamId`,
  `DirectoryStream` with its fd, inode and cursor, and the `DirectoryStreams`
  map on `UnixProcessState`), the address-to-id mapping is client-side already
  (`DirectoryStreamBlocks : Map<NativeMemoryBlockId, DirectoryStreamId>`, and
  `NativeMemoryBlockId` is PawPrint's own), and the 1024-byte name buffer is
  Darwin's `__DARWIN_MAXPATHLEN`, an ABI constant that stays with the client by
  the raw-versus-PAL rule. It fits by construction: NAME_MAX is 255 bytes on
  Linux and 255 UTF-16 code units (at most 765 bytes) on Darwin.

  So `readdir` returns a name, an inode type and a new cursor, and PawPrint owns
  the blob exactly as it owns `getcwd`'s destination. **The increment confirmed
  it**: nothing had to be arranged that stage 8's other syscalls had not already
  arranged.

  What the code did add is a small vocabulary, `DirectoryEntryKind`, because
  neither of the two obvious types would do. `InodeContent` carries the payload,
  and a caller enumerating a directory is owed each entry's *type* rather than
  the bytes of every file in it; `fileTypeBits` is the `S_IFMT` numbering `stat`
  reports, where `readdir` has its own (`DT_REG` and friends) and the two are
  different numbers. So the kind crosses as a kind and each client encodes
  whichever its own struct wants — the same shape the `open` flags settled on,
  arrived at from the other direction.

  `EmulatedKernel.withNewDirectoryStream` becomes `withDirectoryStreamBlock`:
  opening a stream is now two steps, the library minting the identity and the
  client binding its address to it. That is the split this increment is, and it
  is the machine that holds it rather than discipline —
  `checkInvariants` already refuses a state in which the two maps disagree in
  either direction, so a client that took only one of the steps is caught.

  `closedir` stays where it is. It already delegates to `UnixSystem.close` and
  `UnixSystem.forgetIfUnheld`; what is left of it is block bookkeeping and the
  ordering that reaps a directory whose last name went while a stream held it,
  which is client-side by the same rule.
* **8m — `getcwd`, `readlink` and `getsockname`** (done). These
  three appear in the census table and had no home in the first draft of this
  list, which is a drafting failure the census itself should have caught: an
  increment list that does not partition its own table is not a plan. All three
  are destination-buffer syscalls and all three land after 8c has settled that
  shape. `getcwd` is the one that needs the honesty note stage 7 wrote down: its
  success value is the caller's own buffer pointer, which the library never
  possesses, so the client composes it.

  `getcwd` went first and took the increment on its own, because measuring it
  turned up two things the shipped handler had wrong, neither of which a probe
  passing a valid buffer can see:

  * **An unwritable destination is not EFAULT everywhere.** Linux's `getcwd` is
    a syscall whose `copy_to_user` reports one; Darwin's assembles the path with
    stores executed in the caller's own context, so a destination it cannot
    write kills the process — SIGSEGV unmapped, SIGBUS read-only. A `PROT_READ`
    page is the probe that discriminates the two mechanisms, an unmapped address
    being consistent with either, and `readlink` answers EFAULT on *both* in the
    same probe, so this is `getcwd`'s own property rather than a general one.
    The handler answered EFAULT for both flavours. `GetCwdRefusal` says so
    instead, on the reasoning `requireStorage` already had written down: a dead
    process is not an errno, and answering one turns a crash into a plausible
    wrong answer.
  * **A user-space `getcwd` stores before it decides, so its unwritable
    destination refuses more widely than the success path.** Darwin can die on a
    call that would otherwise report ERANGE or ENOENT, and whether it does turns
    on the current directory's length against a libc threshold measured at 1016
    bytes — neither PATH_MAX (1024) nor any documented constant, but one build's
    internal slack selecting between the `__getcwd` syscall and the user-space
    backward assembly. This library models kernels, not that route selection, so
    it refuses from capacity 2 up whatever the path length, deliberately
    over-refusing the cells where the real call answers without storing.
    Capacity 0 and 1 still answer, that flavour having been measured to write
    nothing there on either side of the threshold.

  * **Darwin's failing `getcwd` scribbles on the caller's buffer, and this
    library does not reproduce it.** This looked at first like the answer to the
    asymmetry the `poll` bullet below leaves open — `Failed` carrying writes,
    arriving on a syscall small enough to hold in view — and it is not. Two
    successive attempts to model it from partial measurements were wrong in
    different ways, the first writing past the caller's declared capacity, and
    what the sweep eventually showed is BSD `getcwd(3)` assembling the path
    *backwards* from the end of the buffer and moving it to the front once it
    fits. The residue is a function of libc's internal progress, not of anything
    a kernel decides. `GetCwdAnswer.Failed` therefore carries an errno and says
    nothing about the destination; `docs/divergences.md` records the measured
    table and the reasoning, and `Interop.Sys.GetCwd` cannot observe any of it.

    So the `poll` asymmetry is still open, and this increment is evidence about
    *how* to close it: a `Failed` that carries writes wants a syscall whose
    failure writes are a kernel's decision. `poll`'s are. `getcwd`'s are libc's,
    which is a different thing wearing the same shape.

  `readlink` followed and was uneventful by comparison, which is itself worth
  recording: the same `PROT_READ` probe that showed `getcwd` taking a signal
  showed `readlink` answering EFAULT on both flavours, so it needs no refusal at
  all beyond the buffer vocabulary's own. Its one subtlety is that truncation is
  **not** an error path — `Interop.Sys.ReadLink` starts with a 256-byte
  `stackalloc` and doubles while the result fills the buffer, so a kernel that
  refused to truncate would break `FileInfo.LinkTarget` for every target of 256
  bytes or more — and that the truncation is in *bytes*, which only a multi-byte
  target can detect.

  Moving it also dropped a claim that had gone stale where it stood: the
  handler's note on the unmoved `atime` said this would be "the first mutation
  of the emulated filesystem in the interpreter", and that no handler writes
  back `Kernel.FileSystem`. Several do now. The contract half of that note is on
  `UnixSystem.readlink`; the falsified half is gone rather than carried across.

    The ordering the handler already had is otherwise confirmed exactly, including
  two cells that only a size sweep reaches: a too-small buffer is ERANGE
  whatever the destination is, on both flavours, and a removed current directory
  outranks even that on Linux, where an unmapped destination is ENOENT rather
  than EFAULT.

  `getsockname` went last and is the one that **closes the `poll` asymmetry
  above**, which is more than its own increment was expected to be worth. The
  question stage 7 left open was whether a `Failed` should carry writes, and
  `getcwd` turned out to be the wrong witness: its failure-path writes are
  libc's. `getsockname`'s are a kernel's, and the two flavours order them
  differently — measured against a wholly unmapped destination with sentinel
  lengths of 7, 13, 100 and 4096, Linux 6.18.5 has already stored the
  untruncated length in the caller's cell when the address copy faults, where
  macOS 26.6 leaves it reading what it went in with. A descriptor that fails
  earlier touches it on neither. So `GetSockNameAnswer.Failed` carries an errno
  **and** a `lengthOverwritten`, and that is the shape `poll` should take: a
  `Failed` carrying exactly the writes the kernel had committed by the time it
  failed, not a generic ordered effect list.

  This is invisible to the PAL, which is worth stating so that nobody later
  reads the field as dead: `SystemNative_GetSockName` hands `getsockname(2)` a
  local `socklen_t` and copies it back only on success, so Linux's store lands
  on the shim's stack. The handler drops the field and says why. A client
  speaking raw POSIX has to honour it, and that is who the library is for.

  The blob does **not** cross. The library answers an `InternetEndpoint` and the
  reported length; PawPrint keeps `internetSockaddrBlob` and writes
  `min declaredLength reportedLength` bytes of it. That is the same division
  `bind(2)` and `connect(2)` already use in the other direction — they hand
  `EmulatedKernel.connectSocket` a decoded endpoint rather than bytes — and it
  keeps `SockaddrOffsets` private to PawPrint, where 21 of its 23 uses are. The
  alternative, moving the encoder into the library on the ground that
  `struct sockaddr_in` is raw kernel ABI rather than PAL, would have made
  `getsockname` the only socket entry point whose bytes cross.

  Two of its rules were confirmed rather than changed, both by the size sweep: a
  descriptor error outranks a destination that names nothing (EBADF and ENOTSOCK
  at every declared length probed, against unmapped, read-only and null
  destinations alike), and the declared length bounds what is written without
  bounding what is reported — the shim's own
  `assert(addrLen <= *socketAddressLen)` is false on both platforms and is
  compiled out of the shipped build.
* **`poll` defers to stage 9**, and not for the reason the others do. It carries
  an array in *and* out, it is the syscall whose failure path writes (the
  asymmetry `getsockname` above now settles: a typed `Failed` carrying the
  writes the kernel had committed), and it is where blocking becomes
  unavoidable — it
  needs `WouldBlock`, which stage 9 defines. It is also already entangled with
  8a: it is one of the six `faultsBeforeOperation` call sites, so 8a must re-plumb
  it even though the syscall itself does not move.
* The **socket-address entry points** — nine handlers, about 600 lines — are the
  largest homogeneous group of buffer handlers in the file, and where they go was
  the first thing this census got wrong. Two claims had to be corrected by
  measuring:

  * "No kernel state at all" is false. All nine read `state.Kernel.UnixPlatform`.
    The true claim is narrower and still useful: they read the *platform profile*
    and no mutable kernel state — never the descriptor table, the filesystem, the
    socket table or the tasks — so they are `UnixSystem`-shaped only in the way a
    pure function of the platform is.
  * "Splitting the socket PAL cluster across two stages is a half-migration" was
    the argument for deferring all nine to stage 9, and it holds for **two** of
    them. Only `GetAddressFamily` and `SetAddressFamily` touch the library's
    remaining PAL residue (`addressFamilyPlatformToPal` and its inverse). The
    other seven touch none of it: their only PAL contact is `UnixErrorPal`, which
    is PawPrint's own as of stage 7's last increment.

  Both corrections stood, and then the refusal-timing census below overtook them
  with a third: every buffer these nine take is *wrapper*-touched, so none of
  them crosses the boundary, and they read no mutable kernel state either. There
  is nothing here to hoist. All nine stay in PawPrint as client-side decode; the
  two that touch the address-family PAL cluster still wait for stage 9, but for
  that cluster's sake rather than their own.

#### Correctness oracle

Per increment, the host-differential test at the new altitude plus the existing
guest fixtures, as before. Specifically:

* **A row per buffer classification, per screening platform.** The interesting
  cell is `Unmapped` under Darwin, which does *not* screen up front and so
  reaches the operation and answers from it — that is the cell a single-platform
  test cannot see. Note where it can run: the host-differential fixtures cover
  one column per host by construction, CI being Linux, so a Darwin-only cell is
  exercised on Patrick's laptop and nowhere else. Say so at the row, as
  `TestVirtualFileSystemAgainstHost` already does for its own halves, rather than
  letting a green CI imply both columns were checked.
* **A row per buffer-untouched short-circuit**, since "the buffer is not touched
  here" is invisible to any test that passes a valid buffer. The two probes are
  not interchangeable, and the difference is easy to get backwards: a **null**
  pointer *passes* the Linux up-front screen, because that screen is arithmetic
  and address 0 is a low address in range, so it reaches the short-circuits —
  `read(f, NULL, 5)` at EOF is 0, `read(dir, NULL, 5)` is EISDIR. A **high**
  address such as `(void*)-1` fails the range check and so probes step 3
  instead: `read(wronlyFd, (void*)-1, 4)` is EBADF, `read(port, (void*)-1, 8)` is
  EINVAL on Linux and ENXIO on Darwin. A test using only one of the two measures
  only one of the two steps.
* **A row pinning what an unrepresentable buffer does at each short-circuit.**
  The finding-1 table is that row set, and without it the regression it names
  lands green: nothing in the suite today asserts that `read(stdin, symbolic, n)`
  answers 0.
* **`TestGuestPathBytes` for 8b**, where a byte budget and a character budget
  agree on ASCII and disagree on anything else — plus the *ordering* row, which
  is a separate claim: a path that is both over-long and invalid UTF-8 must be
  ENAMETOOLONG rather than a refusal, because `PATH_MAX` is enforced by
  `getname()`/`copyinstr` when the kernel copies the string in, before anything
  looks at what it says. `parseGuestPathBytes` records this and its measurement
  (1023 bytes resolves on macOS, 1024 does not); the byte entry point must state
  that the bytes exclude the terminating NUL, that the comparison is therefore
  against `pathMaxBytes - 1`, and that the limit is per-flavour.
* **Mutation, per increment**: break one ordering step and confirm a row dies.
  The ordering steps are what this stage is really moving, and they are the part
  no type can hold.

#### The census this design owed, taken

The missing axis was **where each refusal fires relative to its syscall's step
order**. Measured across all thirty handlers that classify a buffer, recording
for each argument the offsets at which it is classified, screened, dereferenced
and transferred. Three results, and the first is the one that matters.

**(i) PawPrint already draws the line the design needs, per *argument* rather
than per handler, and it draws it by who dereferences.** There are two policies
for a non-null address naming no storage, and the choice between them is not
arbitrary:

* **The kernel touches it** — `read`'s buffer, `write`'s, `getcwd`'s, `bind`'s
  address blob. `BufferPointer.dereferenceable` answers `None` and the handler
  reports **EFAULT**, which is the kernel's own answer.
* **The wrapper touches it** — `getsockname`'s `socketAddressLen`,
  `CreateSocketEventPort`'s out-parameter, every socket-address codec's blob.
  `requireStorage` **refuses**: a real run faults inside the shim, and PawPrint
  models no such fault.

`bind` and `getsockname` each use both, on different arguments, and the code says
so where it switches: "This is the opposite of
`SystemNative_CreateSocketEventPort`'s out-parameter, which the wrapper itself
dereferences, and which `requireStorage` refuses for."

This retires the confusion the last three revisions have been circling. Decision
2(b) said an unmapped address `failwith`s; I said it answers EFAULT; **both are
true, of different arguments**, and the discriminator is whose code does the
dereference. It follows that only **kernel-touched** buffers ever cross the
boundary — a wrapper-touched argument is decoded by the client before any
syscall exists — so `Unmapped` means EFAULT unconditionally for everything the
library ever sees, and `Opaque`/`Addressless` are the only cases whose timing the
library must own.

**(ii) The classify-to-dereference gap is nonzero in six handlers and zero in
six.** `Open`, `MkDir`, `Unlink`, `RmDir`, `OpenDir` and `FStat` dereference on
the line after they classify, so for them an eager refusal is exactly today's
behaviour. `GetCwd` (65 lines), `Read` (68), `PRead` (62), `Write` (17), `PWrite`
(19) and `ReadLink` have real steps in between, and those six are where finding 1
lives. The four regressions finding 1 names are all in that second group, and
none is in the first.

**(iii) 8h should not exist.** The seven socket-address codecs read only the
platform profile, and every buffer they take is wrapper-touched. There is no
kernel operation to hoist and no buffer that crosses — so scheduling them as a
stage-8 increment was a category error, not a scheduling one. They are pure
client-side decode and they stay in PawPrint. What remains for stage 9 is the two
that touch the address-family PAL cluster, exactly as before.

#### Decision 2(b), amended in place

Stage 7 committed to `SyscallAnswer` gaining a writes component and left the
ordered-versus-unordered question for measurement here. The census answers it by
dissolving it: under option (B) there is no generic writes component to order.
The claim stage 7 wanted tested — that a syscall is atomic with respect to
PawPrint's scheduler, so no guest thread can observe an interleaving — is still
true and is what lets a *typed* answer be applied in whatever order the client
likes. Decision 2(b)'s "ordered list of effects" is superseded, and the sentence
in stage 7's section that promised the amendment is discharged here.

### Stage 9: blocking syscalls, and packaging

**Dependencies**: stage 8.

**Implements**: decision 3.

`WaitForSocketEvents`, `poll`, `accept`, `connect`: `WouldBlock of
WakeCondition` plus `WakeCondition.isSatisfied`. **And `flock`**, whose blocking
case stage 7 had to spell as a refusal: moving it here is what lets it carry the
descriptor-table advance a real kernel makes before it sleeps, which a refusal
cannot. PawPrint's `Program` readiness
sweep becomes a poll of that predicate. Then: `README`, the
`emulated-posix-kernel` skill's paths, `docs/divergences.md`, and the
packaging decision from the open questions. (Of those four, one was already done
in stage 1 and one needed nothing; see "Stage 9 packaging" at the end of this
document for what the other two actually owed.)

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

#### Stage 9a: `flock` blocks, and the vocabulary the rest of the stage joins

**Dependencies**: stage 8.

The smallest instance of decision 3, and the one that moves nothing: `flock` is
already wholly inside the library, exactly one refusal case converts, and its
wake condition is a question about the descriptor table alone. It is also an IOU
written into the code — `FLockRefusal.WouldBlockIndefinitely` says of itself
that "when blocking gets an outcome of its own rather than a refusal, this case
moves there and carries the advance with it", and the test that pins it names
itself as the row that will have to change on purpose.

The other four blocking syscalls do not convert here. Nor does PawPrint park:
the handler keeps a `failwith`, but the message changes owner, from relaying the
library's "this library has no scheduler" to stating PawPrint's own missing
feature. That reassignment is the point of the slice — parking is the client's
business, and the library stops pretending the question is unanswerable.

**Where `WouldBlock` attaches.** Not as a third case of `SyscallAnswer`: that
type is spelled throughout as *what the entry point returns* ("The entry point
returns this"; "The entry point returns its failure sentinel"), and a blocked
syscall has not returned — and eight syscalls that cannot block would gain an
arm they can never take, which is the objection `ReadTarget` already makes for
itself. Not as a per-syscall `FLockAnswer` either: the per-syscall answer types
exist because their payloads do not fit `SyscallAnswer`, and they pay for it by
having no case in `Syscall` and so being unreachable through `step`. Paying that
for `flock` would cost the request layer its one blocking syscall and buy
nothing. So a sibling, as decision 3 named it:

```fsharp
type SyscallOutcome =
    | Answered of SyscallAnswer
    | WouldBlock of WakeCondition
```

with `flock` and `step` returning `Result<SyscallOutcome * UnixSystem<_, _>, _>`.
Both `Ok` cases ride with a state because both are states a client may continue
from, which is what distinguishes them from the refusal that must carry none.

A fourth option, rejected here and revisited in 9b: **the kernel records the
park itself**, taking the calling task and storing a `ParkedFlockWait` in
`UnixTaskState`, as `ParkedSocketWait` already is. That is park *bookkeeping*
rather than decision 3(a)'s scheduler, so it is not excluded on principle — but
it forces a task parameter onto `flock`, which stages 7 and 8 deliberately kept
off every syscall whose answer does not depend on the caller.

**What the condition carries.** `FlockGrantable of requester :
OpenFileDescriptionId * mode : FlockMode`, with `isSatisfied` resolving the
requester to learn its object. A requester whose description has been closed
while a task was parked on it is an invariant violation rather than an
unmeasured gap — a real waiter holds a file reference, so its description cannot
die underneath it — and the library `failwith`s.

The alternative was to carry the object too, so that a vanished requester
excludes nothing and `isSatisfied` stays total. Rejected for two reasons. Its
answer in that case is `true`, so the client wakes, re-issues `flock` on a
descriptor that no longer exists, and gets `EBADF` — which no real kernel
produces. And the trick does not generalise: it works only because this
condition is a pure function of one object and one mode, where the socket
condition needs live port registrations that no snapshot can carry. Keying every
wake condition on live kernel objects, and letting `close` be what keeps them
alive, is the uniform rule for the whole stage.

`isSatisfied` calls the same conflict scan `FileDescriptorRegistry.flock` uses
to decide the acquire, extracted for the purpose. "The condition is exactly the
acquire path's own test" is then enforced by construction rather than by keeping
two copies in agreement.

**What 9b inherits.** The client park: a thread status, a readiness sweep,
`close`'s matching refusal, and a way to *finish* a woken call.

That last one is not obvious, and review found it. A parked call cannot be
completed by re-issuing it with the arguments it arrived with: those named a
descriptor, and descriptor numbers are reused as soon as they are free. Another
task closing the number a waiter parked on — which a `dup` elsewhere makes
survivable, so `close` refusing the *last* close does not cover it — leaves that
number naming something else entirely, and a resume through it would take a lock
on the wrong object. So 9b needs an acquisition keyed on the description, which
is what the condition already names, and which is the same shape
`ParkedSocketWait` takes for the same reason. It is deliberately not built here:
nothing can call it until something parks. That last one is why the vanished requester is worth
naming now — `close` can refuse a close that would strand a parked
`WaitForSocketEvents` only because the park is recorded library-side in
`UnixTaskState.ParkedSocketWait`, and a flock park recorded only client-side
gives `close` nothing to scan. So 9b takes the fourth option above after all,
for the park record specifically. It is a decision rather than drift, and the
same reconciliation is owed when `WaitForSocketEvents` converts: that park
carries re-entry state (`MaxEvents`) which is not wake state and must not move
into `WakeCondition`, so the library will hold a park record *and* return a
condition, and what ties the two together needs stating.

Two further constraints the later slices must not violate. `poll`'s condition is
a disjunction over descriptors *or* a deadline, and PawPrint's driver advances
the virtual clock to the nearest deadline when nothing is runnable — so the
client must be able to read a deadline out of a condition as *data*, which works
only because `WakeCondition` is a transparent DU. It must never grow a
predicate-valued payload. And a compound per-syscall case is to be preferred
over generic combinators (`Any of WakeCondition list`) until a second syscall
needs them.

**Correctness oracle**:
* The stage's property, on its first instance: every `WouldBlock` `flock`
  produces has `isSatisfied = false` in the system returned *with* it. Its
  complement is what stops it being vacuous — releasing the conflicting lock
  makes the same condition answer `true`.
* A blocked *conversion*, which is the only scenario in which the advance is
  visible at all: a fresh contended acquire leaves the table structurally
  unchanged, so it cannot witness that `WouldBlock` carries the advanced system
  rather than the one passed in.
* The condition names the goal rather than the obstacle: a *new* description
  taking a conflicting lock after the park leaves it unsatisfied.
* `Syscall.FLock` still reaches `WouldBlock` through `step`, carrying the same
  advanced system.
* `FlockRawSeeded.cs` and `FlockContentionSeeded.cs`, unchanged: they exercise
  only the `LOCK_NB` paths, and are the check that converting the blocking one
  disturbed no answered path.

**A widening worth stating**: under the Darwin flavour a contended *fresh*
blocking acquire reached the flavour-neutral refusal and now reaches
`WouldBlock`. That is sound — the only Darwin-observable divergence in blocking
`flock` is drop-versus-keep on conversion, and `FLockRefusal.DarwinConversion`
refuses conversions before the contention test is reached — but it does widen
what a Darwin-flavoured kernel answers.

#### Stage 9b: PawPrint parks on `flock`

**Dependencies**: stage 9a.

The client half of 9a, and what stops `WakeCondition.isSatisfied` being a
predicate with no consumer but its own tests. It also replaces an abort with
faithful behaviour: `TestFlockBlocking`'s guests are single-threaded and hold
the conflicting lock themselves, so they now reach `Deadlocked` — which is what
hanging for ever *is* here — instead of crashing the interpreter.

**Where the park is recorded.** In two places, and only one of them carries the
payload. The library-side record is not optional: `close` must refuse a close
that would destroy a description a task is parked on, that rule is a kernel's,
and a library function cannot see PawPrint's `ThreadStatus`. So `UnixTaskState`
gains `ParkedFlock`. `ThreadStatus.BlockedOnFlock` then carries *nothing*, where
`BlockedOnSocketEvents` carries its port — because a payload there would be a
second copy of a fact with nothing keeping the two equal, and the port payload's
only consumer today is its sweep's fold. The sweep asks the kernel instead.

That is a deliberate divergence from the adjacent mechanism, and the reconciling
move goes the other way: when `WaitForSocketEvents` converts, its park should
move to this shape rather than this one growing a payload to match. A marker
also merges cleanly into the `BlockedInSyscall` that four more parking syscalls
will want, where a payload would not — `waitDeadline` is a pure function of
`ThreadStatus` today, which is what would make that conversion expensive once
`poll`'s deadline-carrying condition arrives.

**Who writes the record.** `UnixSystem.parkFlock` *derives* it from the
condition it is handed, rather than the handler building one beside the
condition it destructured. 9a's notes said 9b would take the "kernel records the
park" option; taken literally that means a task parameter on `flock`, which
stages 7 and 8 kept off every syscall whose answer does not depend on the
caller. This keeps both: the library owns the record's coherence, and a record
disagreeing with the condition a client polls is unwritable rather than merely
unwritten.

**How a woken call finishes.** Not by re-issuing it. `UnixSystem.flockAcquire`
takes the open file description, and `FileDescriptorRegistry.flockOn` is the
by-description primitive the by-fd `flock` now delegates to. A guest test pins
why: it closes the descriptor it parked through — kept alive by a `dup` — and
watches the number be handed to a different file before the lock frees.

Most of what `flock` screens is not re-applied, and the rule rather than the
case list is that *a screen over immutable facts is spent; a screen over mutable
state is not*. The operation bits are captured, and `flockAcquire`'s signature
makes a malformed resume unrepresentable; the Darwin refusals for a pipe, a
socket and an event port are about the description's object kind, which never
changes. `DarwinConversion` is the exception and is reachable: 9a's widening
made Darwin parks possible, and while a waiter holds nothing another task
through a `dup` can take a lock on its description — a first acquisition, which
Darwin serves — so the resume becomes the conversion whose keep-versus-drop
divergence is unmeasured.

**Several waiters wake, and the socket sweep's refusal is not copied.**
`fireSocketReadiness` refuses loudly when several threads park on one port,
because epoll's wait queue is *exclusive*: a real edge wakes exactly one, by
park order, which PawPrint keeps no state to reproduce. `flock` has no exclusive
handoff — a release wakes every blocker and they race — and, decisively, *which*
waiter wins is not observable from userspace on any platform. So waking them all
and letting the seeded scheduler pick is declining to invent a winner rather
than inventing one. The losers re-enter, find the lock taken, and park again;
that path is the mechanism rather than an edge case, and `flockAcquire` answers
`WouldBlock` for it.

**What a park costs a `close`.** `CloseRefusal.LastFlockedDescriptorWithWaiter`,
which discharges 9a's obligation and makes `isSatisfied`'s vanished-description
arm unreachable. Flavour-blind, unlike the two port refusals it sits beside:
those split Linux from Darwin because two *measured platform behaviours* differ,
whereas this models no platform at all — both flavours keep the file alive, and
it is this table that cannot represent the reference. Splitting it would imply a
measurement had been taken and invite someone to complete the Darwin arm.

**Correctness oracle**:
* `TestFlockPark`: a two-thread guest that contends, releases and completes —
  driven by *stepping*, asserting a thread was seen in `BlockedOnFlock`, because
  an exit code cannot tell a park that worked from a park that never happened
  (if the waiter arrives after the release, it succeeds uncontended and covers
  nothing). Plus the descriptor-reuse guest, a guest whose release is the
  holder's `close` rather than `LOCK_UN` (which is what the sweep-not-push design
  buys), and a single-threaded parked *conversion* asserting on the deadlocked
  state's kernel that the requester holds nothing — the only deterministic
  observer of the advance, since a fresh contended acquire has nothing to drop.
* `TestFlockBlocking`'s blocking cases, now asserting `Deadlocked` precisely
  rather than "did not crash": an implementation that parks by leaving the
  thread `Runnable` and re-entering in a loop hits the step cap instead, and
  only the outcome shape tells the two apart.
* Library tests for the resume, the beaten resume, the Darwin re-screen,
  `parkFlock`'s derivation, and both sides of `close`'s new refusal.
* `EmulatedKernel.checkTaskInvariants` gains the park agreement, stated as an
  implication plus a bound rather than an equivalence: `BlockedOnFlock` implies
  a record, and a record implies `BlockedOnFlock` *or* `Runnable`. The
  `Runnable` half is not slack — between the wake and the re-entry the thread is
  runnable with its record intact, and the record is what tells the re-entered
  handler that it is a re-entry.

**What is left of stage 9**: the four socket syscalls, the socket park's move to
this shape, and the packaging items.
#### Stage 9c: the socket park moves to the marker shape

**Dependencies**: stage 9b.

The reconciling move 9b promised, taken in the direction 9b said it should go:
`ThreadStatus.BlockedOnSocketEvents` loses its port and becomes a marker, and the
readiness sweep, the deadlock report and the debugger all read
`UnixTaskState.ParkedSocketWait` instead. No behaviour moves — the same threads park,
wake and deliver, in the same order — so the existing socket fixtures are the oracle for
that half, and what is new is a question none of them could ask.

**What the duplication was.** The port was written down twice: in the status, by
`Scheduler.blockOnSocketEvents`, and in the kernel's record, by the handler beside it.
The library needs the record independently — `close` refuses a close that would strand a
waiter, and that rule is a kernel's — so the *status* is the copy that goes. The two
halves that must agree read different ones: `Program.fireSocketReadiness` decided whether
to wake from the status, and the re-entered handler delivered from the record. Nothing
today can make them disagree (one producer writes both from one binding), so this is a
redundancy removed rather than a bug fixed; what it buys is that "woken on the readiness
of port A, delivering from port B" stops being a thing a later edit can write.

**`BlockedInSyscall` is not taken here**, though the plan names it as where four more
parking syscalls are heading. Not for the reason that first suggests itself — splitting a
merged case again is a mechanical change the compiler drives — but because with two
independent optional record fields, a merged status forces each sweep to *skip* threads
lacking its record, and a `BlockedInSyscall` thread with no `ParkedFlock` is
indistinguishable, from `fireFlockGrantable`'s seat, from someone else's waiter. That
would destroy exactly the fail-loud property the new sweep test pins. The merge becomes
right after the records are merged, not before.

**Nor are the records merged here**, into the single `Parked : ParkedSyscall option`
that would make "parked on one thing at a time" true by construction. That is the better
shape and it is owed; it is a separate slice because it changes `WoofWare.PosixKernel`'s
published surface, where this one changes only PawPrint's internals and can be judged
entirely against fixtures that must not move. Note that it does not collapse as much as
it looks: `close`'s two refusal ladders stay two, the port one being flavour-split and
gated on the object kind while the lock one is flavour-blind and gated on destruction.

**The debugger keeps its payload.** `DebuggerServer.writeThreadStatus` rendered the
port out of the status, and was handed only the status — which is why the `flock` park
beside it had been reduced to a bare `"blockedOnFlock"` since 9b. Dropping the port would
have been the cheap answer and no client in the tree would have noticed; instead the
renderer is handed the task, because a renderer that structurally cannot say what a
thread is waiting for is the wrong shape for a debugger. `flock`'s arm gains its
description and mode at the same time, that being the gap its own comment described.

**Correctness oracle**:
* `TestSocketEventsWait`, unchanged except where the port moved: the `dup` guest now
  asserts the identity on the record, which is where the sweep and the delivery both read
  it, and the deadlock report asserts the text the new derivation produces.
* A two-port guest, which is the question one port could never ask: the sweep must wake a
  waiter because *its own* port has something to deliver, not because some port does. A
  spurious wake is invisible to the guest — the waiter re-enters, finds its port empty and
  parks again — so the observation is that the quiet waiter is never *scheduled* again
  after it parks, made over the driver's own steps, with vacuity guards that both waiters
  parked and that the edge really woke the other one.
* A socket waiter whose record is taken away, stepped once: the sweep refuses rather than
  skipping it. There is no `flock` analogue of this today; `fireFlockGrantable`'s matching
  failwith is untested, and that is a gap this slice notes rather than closes.
* `checkTaskInvariants` gains the socket park agreement in the same shape as the `flock`
  one, and `TestTaskState`'s round-trip now parks the *status* with the record — a
  `ParkedSocketWait` on a `NotStarted` thread was a state that test constructed and that
  no wait can produce.

**What is left of stage 9**: the four socket syscalls, and the packaging items.
#### Stage 9d: one park per task, recorded once and named once

**Dependencies**: stage 9c.

The follow-up 9c recorded as owed, plus the status merge 9c deferred — together,
because separating them rewrites the same functions twice and because 9c's reason for
deferring the second dissolves under the first.

`UnixTaskState` held two independent optional park records, and `ThreadStatus` had a
marker per parking syscall. Now it is one `Parked : ParkedSyscall option` and one
`BlockedInSyscall`. Nothing about what any guest sees changes.

**Why before the socket conversions.** Not "each conversion adds a field" — the
`WaitForSocketEvents` conversion adds none, since its record already exists and what
moves is the *decision*. It is `poll`, `accept` and `connect` that would each add a
field, an accessor pair, an invariant block and two defect cases. Merging first is also
the incomplete-migration tax paid early, and it changes `WoofWare.PosixKernel`'s
published surface, which deserves a PR a reviewer can judge on its own.

**The status merge, reconsidered.** 9c rejected `BlockedInSyscall` on two grounds and
the decisive one was that, with two independent record fields, a merged status would
force each sweep to *skip* threads lacking its record — making "no record at all"
indistinguishable from "someone else's waiter" and destroying the fail-loud property
9c's own sweep test pins. One record field dissolves exactly that: "parked with a
`Flock` record" is a legitimate thing for `fireSocketReadiness` to skip, and "parked
with no record" is the bug both sweeps still refuse.

The residual objection was that two exhaustive classifiers would answer once for all
parking syscalls while four are unmeasured. That turns out to be derivable rather than
unmeasured: a parking syscall completes by writing through the caller's own pointers —
`poll`'s `revents`, `accept`'s sockaddr, the wait's event buffer — or, for `connect`,
with a result unknowable at park time, and PawPrint can only write a caller's memory
from that caller's frame. So all four are necessarily frame-retaining re-entrant parks,
and `hasNoActiveFrame` and `parksPastTheBlockingCall` are forced, not guessed. A future
syscall that broke that gets its own status, which the compiler drives from the one
match arm.

**A hazard the merge creates, and where it is caught.** Two fields kept the evidence of
a completion that forgot to clear its record: both set at once is a state
`checkTaskInvariants` reports. One field would instead let the task's next park
*overwrite* the stale record, destroying the evidence and passing every check. So
`UnixTaskTable.withParked` refuses to replace a park of one syscall with a park of
another, and each handler's entry probe refuses a record of the wrong kind rather than
treating it as a first entry. That refusal has to be at the write rather than in the
invariant, because `checkTaskInvariants` is a test-time oracle that nothing in the
driver loop runs.

Equality is deliberately not required of a *same*-syscall re-park: a beaten `flock`
waiter re-parks on the same condition today, but a re-parking call may lawfully revise
its own re-entry state, and a timeout with less of itself left to run is the obvious
future instance.

**What licenses the merge at all** is that no task can legitimately hold two records,
which rests on four things worth stating because a future `Thread.Interrupt` or `fork`
would have to re-establish them: every write of `Runnable` outside the two wake helpers
is gated on a status that is not a park; signal dispatch runs handlers on the dedicated
dispatcher thread and never touches a waiter's status, and both parks are `EINTR`-immune
by their PALs' own retry loops; nothing terminates or interrupts another thread; and both
handlers probe their own record before any argument read, so the first scheduled step
after a wake re-enters the same syscall.

**What each reader does now.** The two sweeps share one collection of parked threads —
`syscallWaiters` — which hands each of them its own kind, *typed*: a sweep needs the
payload its syscall parked with, and no other syscall's payload has that type, so
mis-selecting is a compile error rather than a rule the sweeps have to keep. That matters
because both payloads are largely `OpenFileDescriptionId`, and a lock waiter's requester
read as a port is a plausible-looking wrong answer rather than a crash. The refusal of a
record-less waiter lives there too, for a duller reason: with both sweeps validating every
parked thread, whichever ran first would report and the other's arm would be unreachable. `GuestLocation` names the syscall as well as its object, because the
status no longer does — "for a lock on open file description 3, Exclusive". The debugger
derives its `kind` string from the record, so the wire shapes for a well-formed park are
unchanged and the kind-less object is reached only in a state the invariant calls a
defect. `close`'s two refusal ladders stay two: the port one is flavour-split, gated on
the object kind and fires on any Darwin close, where the lock one is flavour-blind and
fires only on destruction. A description can match both — nothing on Linux refuses an
`flock` of a port descriptor — in which case the port refusal wins and the lock one is
never named; a refusal either way, so the shadowing costs only which message is reported.

**Correctness oracle**:
* `checkTaskInvariants`' park agreement, now one block, exercised once per park kind —
  which is the point: the rule is about *whether* a thread is parked and *whether* it
  recorded a park, and a fifth syscall needs no fifth statement of it.
* The cross-kind overwrite refused, the same-kind re-park allowed, and a cleared park
  followed by the other syscall allowed — the three rows that pin the refusal at the
  strength it has rather than at "no park may ever be overwritten".
* Library-side rows for `close`'s *port* ladder, which had none: it was covered only
  from `WoofWare.PawPrint.Test`, so a mutant in the library's own scan needed another
  package's tests to die.
* `TestSyscallPark`: one thread parked on a lock and another on an event port at the
  same time, which is the workload that tells "select the parks I own" apart from "take
  every parked thread". Every other park fixture has one kind of waiter, under which the
  two agree. Driven by stepping, because a waiter woken by the wrong sweep would simply
  park again and no exit code would record it — so the assertion is that the port waiter
  is never scheduled after it parks, with a guard that the two-park state occurred at all.
* The socket and flock park fixtures unchanged in what they assert, save for the
  diagnostic strings the merge rewrites.

Two refusals are deliberately left untested, and neither is reachable by a single change:
each handler's wrong-kind entry probe, and each wake helper's kind guard. The first fires
only if a completion leaves a record behind — which `withParked` now refuses at the write
— and the second only if a sweep mis-selects, which is itself a mutation. They are second
lines, and a test for either would have to break two things at once.

**What is left of stage 9**: the four socket syscalls, and the packaging items.



#### Stage 9e: the port's consumer half moves to the library

**Dependencies**: stage 9d.

Stage 9's remaining socket work assumed `WaitForSocketEvents`, `poll`, `accept` and
`connect` were library syscalls whose blocking case needed converting, as `flock`'s did
in 9a. They are not: they are hundreds of lines of PAL ladder apiece in
`Native/NativeSystemNative.fs`, and stage 8's socket entry points never happened. So
each conversion is a *move* first, of stage-8 size.

This stage moves the part of that which the blocking work needs and which nothing else
is entangled with: the consumer half of the socket event port.
`SocketEventPort.epollReadinessOfDescription`, its private `annotatedReady`, the
deliverability predicate and the drain leave `EmulatedKernel` for
`WoofWare.PosixKernel`, retyped from `EmulatedKernel` onto `UnixSystem`. Nothing about
what a guest sees changes.

**Why the consumer half alone.** The producer half — seeding a port's pending list when
a registration is added or modified, and signalling a registration when its target's
level changes — stays in PawPrint, because it is reached through the PAL wrapper's own
screens. That leaves the library able to say whether a port would deliver while owning
no modelled operation that makes one start to, which is a real asymmetry and is written
down in the module's docstring rather than left to be discovered. `poll`'s sibling
readiness function stays too: it belongs to `poll`'s own conversion and nothing in the
library would call it.

**What the move is for.** `Program`'s two sweeps ask their questions at different
altitudes: the `flock` sweep evaluates the library's `WakeCondition.isSatisfied`, and
the socket sweep asks a PawPrint-side predicate, because the predicate was PawPrint's.
A `WakeCondition` case for the socket wait is only possible once the predicate the
library would evaluate lives in the library — `WakeCondition` is data, and no case may
carry a function. So this is the move that stage 9f's condition needs, taken on its own
because it is mechanical and its diff is mostly call sites.

**The naming constraint, which is not cosmetic.** `scripts/check-pal-residue.py` treats
the bare token `SocketEvents` — the PAL's socket-event bit names — as evidence that a
definition speaks .NET's vocabulary rather than POSIX's, and the allowlist it pins may
shrink but never grow. `hasDeliverableSocketEvents` and `deliverSocketEvents` are caught
by their own `let` lines, so moving them under their existing names would have failed
the `pal-residue` flake check. Measured, not reasoned: the plural names report two new
residue entries and the singular ones report the allowlisted eight. The library's own
vocabulary is already singular throughout (`SocketEventPortState`,
`SocketEventRegistration`, `setSocketEventReady`), so `hasDeliverableEvent` and `drain`
join it.

**The predicate becomes strict.** It answered `false` for a dead or non-port
description, justified as "a thread can park on a port whose last descriptor later
closes, and a real `epoll_wait` sleeps on regardless". That justification predates the
port close refusal stage 7 added: `UnixSystem.close` refuses the last-descriptor close
on Linux and any close on Darwin precisely so a parked waiter's port cannot be
destroyed, all three of PawPrint's close sites turn that refusal into a failure, and
there is no other path that closes a descriptor. So the `false` arms are unreachable
under a waiter, and both answers are wrong if they are ever reached: "not yet" sleeps
for ever, and "grantable" wakes the waiter into an `EBADF` no kernel produces. This is
the rule 9a set for the whole stage — every wake condition keyed on live kernel
objects, with `close` as what keeps them alive — applied to the one condition that had
not been written that way.

**Correctness oracle**:
* The claim the shared walk exists to make true, made executable: a drain reports
  something exactly when the predicate said it would. It was a docstring assertion with
  nothing checking it, and each reader looks correct alone. Checked on every call
  through the test adapters the move needs anyway, so all forty-odd measured delivery
  rows and every generated `Wait` op of the socket fuzzer carry it, and as its own
  library-side row over an empty port, a pending one, and the state a drain leaves
  behind — which is the state a waiter that found nothing parks in.
* Library rows for the two refusals the strictening adds, and for the drain's
  non-positive count, which had none.
* The existing corpus unchanged in what it asserts: `TestSocketEventDelivery`'s measured
  rows, `TestEmulatedKernelSockets`, `SocketFuzz`, and the socket park fixtures.

**What is left of stage 9**: the socket wait's `WakeCondition` and the sweep merge (9f),
the socket syscalls' own moves, and the packaging items.

#### Stage 9f: one wake condition vocabulary, and one sweep

**Dependencies**: stage 9e.

The socket wait's park now names a `WakeCondition` — `SocketEventDeliverable of port` —
which `isSatisfied` answers through the predicate 9e moved into the library. With that,
both parking syscalls speak one vocabulary, and `Program`'s two readiness sweeps and
`Scheduler`'s two wake helpers become one each. Nothing a guest sees changes.

**Why the sweeps merge.** 9d gave each sweep its own *typed* list of waiters so that
mis-selecting — a socket sweep reading a lock waiter's requester as a port — was a
compile error rather than a rule to keep. One sweep gives that up in form and recovers
it in substance: nothing in the sweep destructures a park at all, because it asks
`WakeCondition.ofPark` and compares conditions. The mistake becomes writable in exactly
one place, `ofPark` itself, which is three lines with a direct test.

That is a relocation, not a strengthening, and it is worth saying which way each shape
fails. Under two sweeps a future parking syscall whose author forgets to add a sweep
fails at runtime, as a silent deadlock. Under one it fails at compile time, in `ofPark`
and `isSatisfied`. Three more parking syscalls are scheduled, which is what decides it.
The merge also deletes the two kind assertions 9d added and recorded as deliberately
untested second lines.

**The trap in the merge, and the test that was owed first.** A merged sweep still has to
keep the two syscalls' wake policies apart: epoll queues waiters *exclusively*, so
several threads parked on one deliverable port is unmodelled and refused, where `flock`
wakes every blocker on a release. Written as "refuse any group of more than one", that
refusal crashes a lawful guest — two threads that block on `flock` through one *shared*
descriptor record structurally equal conditions, because the lock belongs to the open
file description rather than to the descriptor.

Nothing in the suite covered that shape. The existing two-waiter guest opens a
description *per thread*, deliberately — conflicts are between descriptions, so two
threads sharing one would not contend at all — which makes its waiters' conditions
differ in the requester. Measured: with a shared descriptor the parked records collapse
to one distinct value, and with per-thread opens there are two. So the guest was written
first, and the refusal matches exhaustively on the condition's case rather than counting,
which also forces a future syscall to state its own policy instead of inheriting
"broadcast" by silence.

**The direction that generalises.** A park record is *richer* than its condition: a
socket wait also carries the event count its finishing call copies out with, which no
condition mentions. So record to condition is total, and condition to record is not —
only `flock`, whose record is exactly its condition, has a `parkFlock` going the other
way, and that function now refuses a socket condition rather than approximating one.
Deriving rather than storing is what keeps the two from disagreeing: a client cannot
park a task on one object while polling for another, because the thing polled *is* the
thing parked on.

**The naming constraint, again.** `SocketEventsDeliverable` would carry the plural token
`SocketEvents`, which `check-pal-residue.py` reads as PAL vocabulary. Measured on a copy
of the library carrying the whole change: the plural name reports four definitions —
`WakeCondition`, `isSatisfied`, `ofPark`, and `parkFlock`, whose refusal arm has to name
the case it refuses — and the singular one reports the allowlisted eight.

**Correctness oracle**:
* Two threads parked through one shared descriptor, asserted to record *one* distinct
  condition between them — the input a group-size refusal would crash, and the one no
  existing fixture produced. Its own vacuity guard is measured, not assumed: giving each
  worker its own `open` makes the assertion fail with two.
* `ofPark` per park kind, directly, including a lock whose requester is itself a port
  description — the corner where a mis-map would find a real port and answer an ordinary
  "not yet" instead of refusing.
* The round trip between the two derivations: `ofPark` of the record `parkFlock` derives
  from a condition gives that condition back.
* `parkFlock`'s refusal of a socket condition, which is one call away in a published
  package.
* The socket condition through `isSatisfied`, unsatisfied on a quiet port and satisfied
  on a pending one.
* The exclusivity refusal unchanged in what it asserts, including the message text a
  guest fixture pins.

The refusal sits ahead of every wake, but as hygiene rather than as a guarantee:
`IlMachineState` is immutable and the refusal is a `failwith`, so a partially-woken state
is discarded when the exception leaves the driver and nothing can observe it. A mutant
that wakes first is equivalent, and is not claimed otherwise.

**What is left after 9f**: the socket PAL cluster, the four socket entry points' own
moves into the library, and the packaging items.

#### Stage 9g: the `SocketEvents` alphabet leaves the library

**Dependencies**: none beyond stage 7's precedent.

Not an entry-point move and not packaging: the third PAL-residue cluster, which this
document assigned to "stages 8 and 9, alongside the syscalls that use them". Stage 8's
entry-point moves never happened, so it sat unretired while 9a–9f went past. It is the
smallest remaining unit in stage 9, and it takes the allowlist from eight definitions to
seven.

`SocketEventInterest.ofBits` was the entry. Retiring it *alone* would have been worse
than leaving it: the checker flags that function only through the word `SocketEvents` in
its failure message, and the type it returns trips no marker at all — so deleting the
function turns the check green while five PAL-named fields stay in the library with
nothing recording them. The rename is the same decision, not scope beside it.

**What the interest type becomes.** Three fields, `In`/`Out`/`RdHup`, named for epoll's
bits. `Close` and `Error` are deleted rather than renamed, and the argument is not that
nothing reads them today — though nothing does; `reportedUnder` is the only consumer of a
registration's interest at all — but that no client *could*. `epoll_ctl` forces
`EPOLLERR|EPOLLHUP` into every stored mask, so a caller that asks for them and one that
does not have made the same registration.

That is measured rather than reasoned, on the one surface that shows a stored interest
mask at all (`docs/plans/2026-08-23-posix-kernel-extraction/fdinfo.c`, Linux 6.18.5):
`/proc/self/fdinfo` reports `events: 18` for both interest 0 and `EPOLLHUP|EPOLLERR`, and
`19` for both `EPOLLIN` and `EPOLLIN|EPOLLHUP|EPOLLERR`. The bits a caller passes are not
recoverable even there, and what fdinfo shows is derivable from the three that remain.

The alternative was five epoll-named fields with `Hup` and `Err` forced true by a
constructor, which makes the record literally the mask `epoll_ctl` stored and
`reportedUnder` a uniform five-way `&&` with no exception to remember. Rejected: it buys
that uniformity with an invariant an F# record cannot enforce — two fields whose only
lawful value is `true`, which any record-update expression can violate silently.

A third shape, reusing `ReadinessLevel` as the interest type, was rejected for erasing
the distinction between what is asked for and what is ready: `reportedUnder` could then
be called with its arguments swapped and still typecheck.

The *name* `SocketEventInterest` stays, because the singular `SocketEvent*` family is the
library's own vocabulary (stage 9e's decision) and renaming one member of it would leave
the family inconsistent.

**Where the alphabet goes.** `WoofWare.PawPrint/Native/SocketEventsPal.fs`, modelled on
stage 7's `Native/UnixErrorPal.fs` down to having the pinned upstream tree as its oracle.
It takes the *out* direction with it: the `EPOLLHUP` fold and the mask construction in
`SystemNative_WaitForSocketEvents` were inline and unnamed, transcribing two upstream
functions with no unit test between them and no way to say which had drifted. They are
now `ofReadiness` (`GetSocketEvents`, all five rows) and `delivered`
(`ConvertEventEPollToSocketAsync`, the fold then that), kept apart precisely so the
pinned source can check each one alone. The wrapper's EINVAL screen and the parse now
share `supported` rather than writing `0x1F` in two projects.

**Correctness oracle**:
* The five `SocketEvents` values, the `SupportedEvents` screen, the two conversions'
  *pairings*, and the delivery fold, all read out of the pinned
  `pal_networking.{c,h}` — the screen by the five constants it names rather than by the
  number it ORs to, and the conversions by the rows in their own function bodies rather
  than by the enum alone. Review is what forced that second half: a pin that re-paired
  `GetSocketEvents` without renumbering anything would have left a numbers-only test
  entirely green.

  What stays written down on this side is which `ReadinessLevel` field names which epoll
  bit, because that is a fact about this repo — the type is defined in those terms — and
  not one upstream can drift.

  The oracle is then checked rather than asserted, by pointing `DOTNET_RUNTIME_SRC` at a
  doctored copy of those two files: the pinned tree is a read-only store path, so that is
  the only way to simulate drift. Four counterfactual upstream changes, each caught by
  the rows that should catch it, and an unmodified copy that passes. Note that
  `nix develop` sets `DOTNET_RUNTIME_SRC` itself, so it must be set *inside* the shell —
  exported around the invocation it is silently overridden, and the probe reads the real
  tree while claiming to read the doctored one.
* `delivered` against `ofReadiness` composed with the fold, over all 32 readiness levels
  rather than a sample, and the consequence a guest actually sees: no level delivers
  `SA_CLOSE`.
* `toInterest 0x18 = toInterest 0x00`, which is the collapse the three-field record
  asserts, pinned as its own row.
* The refusal row uses the literal `0x20` rather than `supported`, so that a `supported`
  widened by mutation cannot move the boundary the row is testing.
* `SocketEventDeliveryLinux.cs`, the Guest-tier fixture that asserts exact `Events` ints
  including the fold, is what covers `delivered` end to end; `TestSocketEventDelivery`
  operates upstream of the fold and cannot see it.

**What is left after 9g**: the four socket entry points' own moves into the library,
the two remaining PAL clusters (socket creation, and the managed-enum signal mapping),
and the packaging items.

#### Stage 9h: the socket-creation numbering leaves the library

**Dependencies**: none beyond stage 7's precedent. Shipped as two PRs, and the split
is the interesting part rather than a convenience.

This is the cluster the allowlist called "the mixed case": `socketCreation` held the
shim's three argument screens **and** this kernel's own declared protocol table in one
function, so splitting it was a design question rather than a move. It takes the
residue from seven definitions to two.

**Which side each piece belongs on**, by the question this whole extraction asks —
would a second client, not speaking .NET's PAL, need this fact? The `AF_*`/`SOCK_*`/
`PT_*` numbering, the three screens and their order, and `isTcpProtocolType` are the
shim's: no. The twelve measured rows, and their flavour dependence, are facts about the
simulated kernel: yes.

**What crosses the boundary.** The client screens, names its PAL triple in the
library's own vocabulary, and asks whether that shape is one the kernel creates. Two
alternatives were rejected. Handing over *platform* numbers is closer to what the real
shim does but would have the library re-derive a `SocketDomain` from an `int` it had
just been given. Moving `socketCreation` wholesale is cheapest and inverts exactly the
dependency this work exists to untangle: the twelve rows are the one part a second
client would have to re-derive.

**PR1 (#1224) made the table data and gave it an oracle**, changing no vocabulary. The
rows had *no library-side test at all* — `TestSocketCreation` lives in
`WoofWare.PawPrint.Test` — which is the gap 9g found in `reportedUnder`, one size up.
They became `SimulatedUnixPlatform.creatableSockets`, a set per flavour; data rather
than a predicate, because a caller wants to ask and a reader wants to enumerate, and
because a `bool` gate would be a classifier whose contract someone then has to keep
true. The oracle is the measurement rather than the data — a table checked against a
copy of itself catches a later typo and nothing else — which meant moving
`socketMatrix/{linux,darwin}.tsv` to `WoofWare.PosixKernel.Test` and linking it back,
so the two suites cannot drift onto different rows.

**PR2 moved the PAL half** into `WoofWare.PawPrint/Native/SocketArgumentsPal.fs`.
Named for the arguments rather than the shim because CoreLib has a managed
`System.Net.Sockets.SocketPal` of its own that comments in `NativeSystemNative` already
cite; a module called `SocketPal` there would have read as that one.

**The sub-decision, and why the obvious answer is wrong.** A PAL triple can pass every
screen and still name no `SocketDomain`, `SocketKind` or `SocketProtocol` —
`AF_UNSPEC`, `AF_PACKET`, `AF_CAN`, `SOCK_RDM` and every protocol but three. That is a
different failure from a shape the table omits, and the first draft of this plan split
`Unmodelled` into two union cases to keep the two crash instructions correct. Wrong:
the 330-row correspondence maps every `SYSCALL` row to one answer, and the measurement
cannot distinguish the two, so splitting the case forces that oracle either to accept
both answers — weakening the totality it exists for — or to re-derive vocabulary
membership itself. The benefit needs no type change: the refusal type is client-side
after the move, so the `failwith` branches its *message* on `SocketArgumentsPal.shapeOf`,
which is public exactly so a caller who cares can ask.

**A residue the checker cannot see, and nearly did not catch.** `SocketDomain`,
`SocketKind` and `SocketProtocol` were POSIX-named and *PAL-defined in their own
docstrings* — "`AF_INET`, PAL 2", and `SocketProtocol`'s "*not* as the platform numbers
it … it is the PAL value that `SystemNative_GetSocketType` will owe a caller".
Docstrings are comment-excluded from the residue scan, so retiring the five entries
without rewriting them would have turned the check green over a worse residue than the
one it reports — 9g's trap one level up. The case names stay (they are POSIX
conditions); the prose stops naming a client's numbering.

The same move weakens the checker in a way worth recording rather than discovering: the
library's PAL constants used to live in one `module private Pal`, so an adapter
essentially had to write `Pal.` to exist. That module has gone, so a re-accreted `AF_*`
table under an innocent name would now pass. The script's "what it does not see" list
says so.

**Correctness oracle**:
* The 330-row correspondence and the host `libSystem.Native` differential, unchanged
  and still passing: the screens moved, they did not change.
* `TestCreatableSockets` against the measurement's `OK` rows, which is what makes the
  library's own suite able to see a dropped row.
* A mutation battery run against **both** suites, whose split is itself the evidence:
  table mutants die in both, PAL-mapping mutants die only in PawPrint's, and the
  library's silence there is correct rather than a gap.

**What is left after 9h**: the four socket entry points' own moves into the library,
`Signal.{of,to}PosixSignalEnum` — the last two entries, which have an oracle the others
lacked, since `PosixSignal` is a managed enum the test can read from the running BCL —
and the packaging items.

#### Stage 9i: the last cluster, and the allowlist reaches zero

**Dependencies**: none. The smallest of the four moves, and the one that ends the
count: `scripts/pal-residue-allowlist.txt` is now empty.

`System.Runtime.InteropServices.PosixSignal` is ten negative integers naming the
signals .NET considers portable. It carries no kernel content whatever — a kernel knows
only signos — so the two conversions go to `Native/PosixSignalPal.fs` and the library
states the signo alone.

**One table instead of two matches.** The pair used to be two independent exhaustive
matches, so the round-trip was a property somebody had to keep true. They are now two
lookups over one list of pairs, which makes `toEnum >> ofEnum = id` true by construction
rather than by test. The cost is that `toEnum` loses exhaustiveness over `Signal`, and
that is the right trade: a new `Signal` case genuinely has no enum member, so answering
`PosixSignalInvalid` for it is correct and a compile error would be noise.

**The trap, for the third stage running.** `Signal.Other`'s docstring said it "carries
the raw managed `PosixSignal` enum value (negative for cross-platform signals…)". Wrong
twice: the checker cannot see it (union cases are an admitted blind spot, docstrings are
comment-excluded), and it was *false* — nothing can put a negative in `Other`, because
`ofPosixSignalEnum` sent every recognised negative to a named case and an unrecognised
one to `ofPlatformSigno`, which refuses everything `<= 0`.

Review then caught the replacement being born false in turn: the proposed wording
claimed a range invariant of `(0, linuxSignalMax]`, and the library's own tests
deliberately construct `Other -77`, `Other 0` and `Other 999` to pin that `toLinuxSigno`
stays a projection. The honest statement is what the *conversions* build, plus the
admission that the public case enforces nothing — and the test comment asserting the
same falsehood was fixed alongside.

Three instances in three stages says this is how residue comes back, so the script's
"what it does not see" list now says so, and says why it cannot be fixed there: the
check would have to tell "cites the PAL to explain a POSIX value", which its own header
blesses, from "defines this thing in the PAL's terms". That is a judgement, not a regex.

**What the check becomes at zero.** Kept, and rewritten as a ratchet: it used to record
how much of the extraction was owed and now records that the answer is none, so that
adding an entry is a visible act somebody has to argue for. Deleting it was the tempting
option and is wrong — its value from here is that it is cheap and fires on re-accretion.
Its module docstring, its guidance message and its success line all changed with it, as
did `flake.nix`'s comment and AGENTS.md's paragraph.

**Correctness oracle**:
* The enum itself, read from the running BCL. `PosixSignal` is public, unlike the three
  clusters before it, so no source-parsing is needed and no `Assert.Ignore` either. A
  pinned-source reader was considered and dropped: `expectedRuntimeVersion` pins the
  devshell's dotnet *and* the runtime source to the same version, so the two agree by
  construction and a disagreement could only mean pin drift, which two other checks
  already police.
* An exact member-count assertion, whose failure mode is specific: a new upstream member
  would reach `ofPlatformSigno`, be refused for being non-positive, and make PawPrint
  throw from `Register` where real .NET registers the signal happily.
* `toEnum` gets tests **for the first time** — it had none in either suite before this
  stage, and the battery had nothing to kill its mutants with until they were written.

**What is left after 9i**: the four socket entry points' own moves into the library,
and the packaging items — which are what finish the extraction.

#### Stage 9 packaging: the documents catch up

**Dependencies**: 9i, because most of what these documents were wrong about is what
9g–9i changed.

Stage 9's own text listed four packaging items. Two of them cost almost nothing, and what
was *checked* about them is worth recording so that nobody re-checks:

* **The packaging decision** was made and implemented in stage 1 — `WoofWare.PosixKernel`
  is `IsPackable` with its own `PackageId`, `version.json` and `README.md`, and entries in
  the `nuget-pack`, `expected-pack` and `github-release-dry-run` jobs. The forward
  reference here to "the open questions" pointed at a section that does not exist in this
  document; it was stale rather than outstanding.
* **`docs/divergences.md`** needed one line. Both of its pointers into the library
  (`UnixSystem.getcwd`, `UnixSystem.getsockname`) still resolve, but it pinned its
  directory-enumeration cases on `WoofWare.PawPrint.Test/TestDirectoryEnumeration.fs`, and
  that file moved to `WoofWare.PosixKernel.Test` during the extraction. Reading the
  document had not caught it; a path checker did, which is the argument for running one
  rather than trusting a careful read.
* **The skill's paths** were the item that genuinely needed nothing: its four
  `reference/*.md` files all exist. Its *content* is another matter — see below.

What cost more:

* **The package README** described the syscall layer as of stage 1: six syscalls, where
  `step` now dispatches nine, and no mention at all of the blocking model that 9a–9f
  built. A client evaluating this package would have read that it cannot express a
  blocking call. It now says that `step` answers `WouldBlock` with a `WakeCondition`, that
  `isSatisfied` is how a client's scheduler polls it, and that the state returned
  alongside is the state after whatever the call did before sleeping.
* **The skill had no section on the PAL boundary at all** — the rule stages 7 and 9g–9i
  established, and the one a future agent in this area is most likely to break, since
  writing the conversion in the library is the locally convenient thing to do. It now has
  one, naming the four `*Pal` modules and the flake check.
  It also carries the two blind spots, because those are what make the check weaker than
  it looks: it cannot see PAL vocabulary in *prose*, which three consecutive stages hit
  while retiring a cluster, and it can no longer lean on the library's PAL constants
  living in one `module private Pal`, that module having gone to `SocketArgumentsPal`.
  The skill's `description` gained `UnixSystem.fs`, `Signal.fs` and `Native/*Pal.fs`, so
  that it loads for the files this stage made important.

**Correctness oracle**: prose, so there is none beyond the suites being unchanged — with
one exception worth having. Every repo path these documents name was resolved
mechanically, which is what found the moved `TestDirectoryEnumeration.fs`; the remaining
reports are upstream CoreCLR filenames and a glob, and the two `docs/divergences.md` code
pointers were checked by hand. That check is a throwaway script rather than a committed
one: it needs a list of plausible roots to resolve project-relative paths and it cannot
tell an upstream filename from a repo one, so as a permanent check it would cry wolf.

**What is left of stage 9**: the four socket entry points' own moves into the library.

#### Stage 9j: `accept(2)` moves into the library

**Dependencies**: 9 packaging, only for the document this appends to. Nothing in the
code depends on it.

The first of the four socket entry points. `SystemNative_Accept` was 173 lines in
`Native/NativeSystemNative.fs`, and almost all of what it said is `accept(2)`'s own
ladder: the descriptor's kind, its phase, whether the description is non-blocking,
whether the queue has anything in it, and what a peer-address copy-out reports.

**Which of the four goes first.** Four candidates, and the argument is not size alone:

| entry point | handler lines | where its kernel ladder is |
| --- | --- | --- |
| `accept` | 173 | **inline in the handler**, plus `EmulatedKernel.acceptConnection` (80) |
| `connect` | 135 | already factored out, into `EmulatedKernel.connectSocket` (~370) |
| `poll` | 231 | inline, and it is the entry point that refuses to park |
| `WaitForSocketEvents` | 344 | inline, and it *does* park |

`connect` has the shortest handler but the largest move, because its ladder is a
separate 370-line function that has to travel with it. `poll` and
`WaitForSocketEvents` each carry the parking vocabulary, which is a second concern on
top of the move. `accept` is the only one whose kernel logic is genuinely interleaved
with guest-memory code in the handler body — which is the thing this stage exists to
separate — and it is the one for which every piece of library vocabulary it needs
already exists.

So `accept` first, and this slice invents exactly one new library concept: why an
`accept` can be refused.

**The precedent it follows.** `UnixSystem.getsockname`, from stage 8m. Both classify a
descriptor and then copy a `sockaddr_in` out under a caller-declared length that bounds
what is *written* and not what is *reported*. `accept` reuses that shape wholesale:
`(fd, destination : UserBuffer, declaredLength, system)`, a negative declared length
`failwith`n because a shim screens it before the cast to `socklen_t` that would
otherwise make the bound `SIZE_MAX`, and an answer carrying an `InternetEndpoint` plus
a reported length rather than an encoded blob.

**The encoding stays in PawPrint, deliberately.** `internetSockaddrBlob` and
`SockaddrOffsets` are `struct sockaddr_in`'s wire layout, which is a kernel fact and
not a .NET one, so by stage 9g–9i's rule they have a claim on the library. They stay
where they are for this slice anyway, because `getsockname` already decided it: the
same private helpers serve ten other PAL handlers (`SystemNative_GetPort`,
`SetIPv4Address`, and the rest) that involve no kernel at all, so moving the layout is
a separate concern with its own option set. Recorded here so the next person does not
have to re-derive that it was a choice rather than an oversight.

**Where the state transition lands.** `EmulatedKernel.acceptConnection` — dequeue the
head of the listener's queue, mint the accepted socket, hand back a descriptor — is
pure POSIX kernel state manipulation sitting on PawPrint's side, and it has to move.
The question is what happens to its callers, of which there are **ten**, all in
fixtures (`TestEmulatedKernelSockets`, `TestSocketEventDelivery`, `SocketFuzz`). They
want the state transition and not the entry point: each holds a `SocketId` rather than
a descriptor, and none has any use for a user buffer.

* **Option A: publish `UnixSystem.acceptConnection`; keep `EmulatedKernel.acceptConnection`
  as an adapter over it.** Zero churn at the ten sites.
* **Option B: publish it, delete the PawPrint one, retarget the ten sites.** Each
  becomes three lines rather than one: `EmulatedKernel.unix` in, the call, `withUnix`
  back out.
* **Option C: leave the transition in `EmulatedKernel`, and have `UnixSystem.accept`
  return a *description* of it for PawPrint to apply.**

C is wrong outright: it would make `accept` the only library entry point that does not
apply its own effect, against `opendir`, `read` and `write`, which all return the
system they produced.

B was the initial choice, on the argument that `TestEmulatedKernelInodeLifetime.fs`
already calls `UnixSystem.opendir (EmulatedKernel.unix kernel)` from a PawPrint
fixture, so the three-line shape is established rather than imposed — and that a
wrapper existing only to save lines in fixtures is the sort of thing this repository
deletes. Counting the sites is what overturned it: that precedent is *one* call site,
and this is ten across three files. Ten copies of the same three-line dance is the
adapter written out ten times, and the first thing anyone would do is factor it back
into a private helper per file — which is Option A, three times over.

So A, with the adapter's docstring saying why it exists rather than merely what it
does. It is not indirection for its own sake: `EmulatedKernel` already documents the
same convention for its `Process` and `Machine` reads, so that moving a field in or out
costs no call site.

Publishing `acceptConnection` publishes its precondition — the socket must be listening
with a non-empty queue — and that is stated on the function, as `UnixSystem.readdir`
states that a stream this kernel never issued is a caller bug rather than an errno.

**What the library refuses.** Four cases, each a `failwith` in the handler today:

* `UnmodelledDomain of socket * domain` — an IPv6 or Unix-domain socket. Today this
  comes out of `socketOfFd`, a *shared* PawPrint helper that `bind`, `listen` and
  `connect` also use; `accept` stops using it and the other three keep it until their
  own moves. PawPrint supplies the other half of the message, exactly as the
  `getsockname` handler already does: which CoreLib path could be holding such a socket
  is a fact about CoreLib rather than about any kernel.
* `UnmeasuredKind of socket * kind` — `SOCK_RAW` or `SOCK_SEQPACKET`, whose `accept(2)`
  nobody has measured.
* `WouldPark of listener` — a *blocking* listener with an empty queue. A real kernel
  sleeps; this kernel has no wake to end that sleep, because accept-side delivery is
  not modelled. It is a refusal rather than `SyscallOutcome.WouldBlock` for exactly
  that reason: `WouldBlock` promises a `WakeCondition`, and there is none to give.
* `UnmeasuredCopyOutFault of listener` — the connection would be dequeued and the peer
  address copied out, but the destination is unmapped, so that copy faults. Not
  a `BufferRefusal`, which names dead ends in how a buffer was *classified*: by the time
  the fault happens a connection has been taken off the queue, and whether a real kernel
  loses it or leaves it queued is unmeasured. This is where `accept` and `getsockname`
  part company — `getsockname` answers EFAULT, having nothing to lose.

  A destination that is `Opaque` or `Addressless` is a *fifth* refusal,
  `Buffer of BufferRefusal`, and the distinction is not decoration: there the kernel
  would have succeeded and dequeued, and it is the client that has run out of
  representation. That split is what review found; see below.

**What the library answers.** `Failed of error` (EBADF, ENOTSOCK, EOPNOTSUPP, EINVAL,
EAGAIN) and `Accepted of fd * peer * reportedLength`. PawPrint keeps every byte of the
guest-memory work: the three null-pointer screens the shim performs before it decodes
the descriptor, reading the caller's length cell, resolving the `acceptedSocket` cell
(which every failure writes -1 through, so it is resolved before the syscall rather
than after), encoding the blob, and writing the three results back.

One unreachable arm went with the move. The handler re-looked-up the descriptor to read
`NonBlocking` and `failwith`'d if it had vanished, having looked it up already inside
`socketOfFd`; the library does its own lookup once and reads the flag off the
description it already has.

**Correctness oracle**:

* The guest tier is unchanged and must stay so. `sourcesPure/SocketAccept.cs` covers
  success, EAGAIN, EINVAL, EOPNOTSUPP, ENOTSOCK, EBADF, all four EFAULT screens and the
  reported-versus-written length split; `sourcesImpure/SocketAccept{Linux,Darwin}.cs`
  cover the three errno numbers that are not portable.
* A new `WoofWare.PosixKernel.Test/TestAccept.fs`, 40 cases, driving `UnixSystem.accept`
  on a constructed system — which is the tier that reaches what a guest cannot: both
  flavours (a guest runs one), every refusal (a guest cannot hold a `SOCK_RAW` socket
  or ask for a copy-out through a buffer whose bytes nobody can produce), and the two
  orderings that matter (the domain screen before the phase screen, the queue check
  before the buffer screen).
* Two of those cases assert what a *refusal* leaves behind, which is the property that
  makes refusing safe: the connection is still queued, so the caller has lost nothing
  by asking. A refusal carries no system, so this is true by construction — and stating
  it is what would catch a future arm that refuses after mutating.

**A trap worth recording for whoever builds the next fixture.** An accept mints its
socket at `Machine.NextSocketId`, so a hand-built system whose sockets were inserted
without advancing that counter has the accepted socket *overwrite* the listener in the
table. The failure names the listener's phase and says nothing about ids, which makes
it look like an accept bug; `withSocket` in `TestAccept.fs` advances the counter, and
says why.

**What review found, which is the part worth reading.** Two things, both of which the
move *created the conditions to see* rather than introduced.

* **The accepted socket's `O_NONBLOCK` was the PAL's answer, not the kernel's.**
  `FileDescriptorRegistry.createSocket` always mints a blocking description, and while
  that was PawPrint-internal it was right: the guest goes through
  `SystemNative_Accept`, which clears the flag. Published as `UnixSystem.accept`, it
  became a claim about `accept(2)` — and the claim is false on Darwin. Measured with
  `accept-inherits-nonblock.c` (added beside the other probes in this plan's
  directory): on Linux 6.18.5 a non-blocking listener yields a **blocking** accepted
  socket, on Darwin 25.6.0 a **non-blocking** one, and a blocking listener yields a
  blocking one on both. So the flag is inherited rather than the platform being
  decisive, which is why the fixture asserts both halves.

  The rule went to `SimulatedUnixPlatform.acceptedSocketInheritsNonBlocking` and the
  normalisation stayed in `SystemNative_Accept`, mirroring upstream's
  `#if !defined(__linux__)` — applied unconditionally there rather than under a
  platform test, because on Linux the kernel never set the flag and clearing it is a
  no-op.

  It is inherited from the *description*, which is why `accept` applies it and
  `acceptConnection` does not: a `SocketId` names no description, so the state
  transition structurally cannot answer the question. A fixture pins that, because
  "scan the registry for a description naming this socket" is a plausible-looking
  wrong answer.

  **No guest can see any of this**, since the PAL erases it. `TestAccept` is the only
  tier that can, which is the second time this stage has found something reachable
  only from the library's own suite.

* **`UserBuffer.Opaque` was grouped with `Unmapped` under one refusal, and should not
  have been.** The plan above argued that all three unrepresentable destinations ask
  the same unanswerable question. They do not. `Unmapped` means the copy-out *faults*,
  after a connection has been taken off the queue, and what a kernel does then is
  genuinely unmeasured. `Opaque` and `Addressless` mean the kernel would have
  **succeeded**; it is the client that cannot represent the transfer. Calling that a
  copy-out fault tells a caller the kernel faulted when it did not. Split:
  `AcceptRefusal.Buffer of BufferRefusal` for the two representation dead ends —
  the vocabulary `getsockname` already uses — and `UnmeasuredCopyOutFault` reserved for
  the genuine one.

**Mutation battery**: twenty-two mutants.

Sixteen of `accept` and `acceptConnection` — every errno, both check orderings, the
dequeue, the inheritance of `SO_REUSEADDR`, the two addresses a connection carries, the
reported length — and every one is killed by the *library's own* suite, so PawPrint's
default suite was never consulted. That is the 9g failure mode not recurring: there,
`reportedUnder` was a library rule only the client's tests could kill.

Four of the `O_NONBLOCK` rule: the flavour answering `false` everywhere, `true`
everywhere, `accept` ignoring the listener's own flag, and `accept` ignoring the
flavour. All four die, and the first two only because `TestAccept` writes the two
answers out as literals rather than asking
`acceptedSocketInheritsNonBlocking` — which is the function under test, so a fixture
that consulted it would move with it and see nothing. That is a mirror oracle, and this
one was written as a mirror before it was rewritten.

Two in the handler, and these are the interesting ones because neither default suite
kills either:

* Replacing `EmulatedKernel.withUnix unix` with `id`, so the library's system is
  computed and thrown away, survives **both** default suites and one of the two guests
  that do a successful accept: `sourcesPure/SocketEventDelivery.cs` accepts and never
  uses the descriptor, so only `sourcesImpure/SocketEventDeliveryLinux.cs` kills it.
* Dropping the PAL's `O_NONBLOCK` clearing had **no killer at all** until this stage
  added one. It could not have had one: a Linux-flavour guest passes whether the
  clearing happens or not, since the kernel never set the flag, and no Darwin-flavour
  guest performed a successful accept. `sourcesImpure/SocketAcceptDarwin.cs` now makes
  a non-blocking listener, connects, accepts, and reads
  `SystemNative_FcntlGetIsNonBlocking` off the accepted descriptor; the mutant dies
  there with exit code 44 against the real macOS runtime's 0.

**What is left after 9j**: `connect`, `poll` and `WaitForSocketEvents`.

#### Stage 9k: `connect(2)`'s ladder moves, and nothing else does

**Dependencies**: 9j, for the adapter convention it settled.

The second socket entry point, split in two. This is the first half: 748 lines of
`connect(2)` ladder move from `EmulatedKernel.fs` to `WoofWare.PosixKernel`, and
**nothing about what any caller sees changes**. The second half (9l) adds the
`UnixSystem.connect` entry point and shrinks the handler.

**Why split at all**, when 9j moved `accept` in one PR: because the two moves are shaped
differently. `accept`'s ladder was *inline in the handler*, so extracting it and writing
the entry point were the same edit. `connect`'s ladder is already a separate function,
so the move and the entry point are two independent edits — and doing them together
would put a 750-line relocation and a redesign in one diff, where a reviewer cannot tell
which lines are which.

**What the move is held to.** `scripts/check-move-is-rename-only.sh` cannot answer here:
it compares whole files across a git rename, and this is a function moving between two
files that both still exist. So the branch carries its own one-off verifier, which takes
the block out of `EmulatedKernel.fs` at the base ref, applies the substitutions below,
and compares against `UnixSystem.fs` word for word (whitespace-normalised, because
fantomas re-wraps and re-wrapping is not content). It reports content-preserving for
both moved functions.

The substitutions are the whole of the change:

* the eight forwarding members `EmulatedKernel` has and `UnixSystem` does not
  (`kernel.Sockets` → `system.Machine.Sockets`, and seven more);
* `EmulatedKernel` → `UnixSystem<'Task, 'Handler>` in five type annotations, and the
  identifier `kernel` → `system` **in code only** — `kernel` is also an ordinary English
  word throughout this function's prose ("a real kernel", "this kernel's"), and renaming
  it there would be a meaning change smuggled into a move;
* three prose repairs, which are the part a move cannot avoid: every message prefixed
  `SystemNative_Connect:` now names `UnixSystem.connectSocket`, because a message must
  name the function it comes from and the shim's name would misdirect; and two
  references to types the library does not have (`EmulatedKernelDefect.SocketPhaseKindMismatch`,
  `KernelConfig.EphemeralPortRange`) would otherwise be dangling.

`ConnectOutcome`'s own docstring said "PAL SUCCESS" and "the wrapper maps it to a PAL
return" — PAL vocabulary in the library, and exactly the prose blind spot the residue
check cannot see. Reworded to say what the type means without naming a client's
encoding. `signalSocketDataReady` moved too: `connectSocket` was its only caller, and
it is nothing but library primitives.

**The narrative "PawPrint" mentions are deliberately left alone**, per the standing rule
for this extraction: they mark text not yet hand-reviewed for release, and a move is not
a review.

**What did not move.** `EmulatedKernel.connectSocket` survives as a six-line adapter,
for the reason `acceptConnection`'s exists: six fixtures call it holding an
`EmulatedKernel`. `ConnectOutcome` could not be adapted the same way — an F# type
abbreviation does not re-export union cases for qualified access — so the ~25
`EmulatedKernel.ConnectOutcome.X` references in fixtures became `ConnectOutcome.X`.

**Correctness oracle**: the move verifier, plus every existing test unchanged.
`TestEmulatedKernelSockets` alone carries about thirty `connect` rows, `SocketFuzz`
drives it, and the guest tier runs `SocketConnect{,Linux,Darwin}.cs`. All pass with no
edit beyond the qualified-name rename.

**The gap this leaves, stated rather than hidden**: `connectSocket` is now a library
rule with **no library-side test at all** — the 9g failure mode, where `reportedUnder`
could only be killed by the client's suite. It is deliberate for one PR: the exhaustive
rows live in a 1500-line PawPrint fixture that covers far more than `connect`, and
moving that is its own stage. 9l adds `WoofWare.PosixKernel.Test/TestConnect.fs`
alongside the entry point.

**What is left after 9k**: the `connect` entry point (9l), then `poll` and
`WaitForSocketEvents`.

#### Stage 9l: `connect(2)`'s entry point, and the test 9k owed

**Dependencies**: 9k, which is where the ladder went.

`UnixSystem.admitConnect` and `UnixSystem.connect` now carry every screen the handler
used to, and `SystemNative_Connect` keeps only the guest-memory work: the shim's two
null/negative screens, resolving the pointer, `requireBufferRoom`, reading the fields,
and the errno write.

**Two calls rather than one, and this is the decision worth recording.** The client
cannot know when to read the sockaddr without the kernel's copy rule, because whether
the kernel touches the buffer *at all* is a measured per-flavour fact: Darwin's
`getsockaddr` reads nothing at a length too short to reach `sa_family`, and Linux's
`move_addr_to_kernel` reads at any positive length. Three shapes were available:

* **The client keeps the rule** and passes `family`/`endpoint` as it does today. Cheapest
  edit; leaves a kernel rule in the client, and the client's copy would silently drift
  from the library's.
* **The library takes the sockaddr bytes** and decodes them itself. Cleanest boundary,
  and it settles where `struct sockaddr_in`'s layout lives — but the client would have
  to read the bytes *before* the library said whether they were needed, and reading
  through a pointer PawPrint cannot resolve is itself a refusal. It would trade a
  correct answer for a crash on inputs a real kernel never reads.
* **Two calls: admit, then connect.** The shape `admitWrite`/`write` already has in this
  library, for the same stated reason — a caller that cannot always produce the bytes is
  let off before it has to.

The third. `ConnectAdmission.Transfer` says how many bytes the copy takes and, as
`ConnectFields`, which of `sockaddr_in`'s fields it reaches — in the kernel's own
vocabulary rather than as two booleans, so the client maps each case to the reads it
makes and keeps no layout arithmetic of its own for this entry point.

**`connect` refuses to be handed a set of fields the admission did not ask for.** That is
not defensiveness: this kernel's answer for a field it *could not read* is measured and
different from its answer for a field the caller did not bother to read, so conflating
them would be a silent wrong answer rather than a crash. The guard is load-bearing in
practice, not only in the unit test — mutating the handler so that a `Family` copy also
reads an endpoint kills `sourcesPure/SocketConnect.cs` and `SocketConnectLinux.cs`
through this failwith, because those guests connect at a declared length of 4.

`SockaddrFamilyField.reachedBy` is the one arithmetic that both the kernel's copy rule
and the shim's own field screen need, so it lives in the library and PawPrint's
`sockaddrFamilyIsInBounds` delegates to it. The rest of `SockaddrOffsets` stays in
PawPrint, as 8m and 9j both decided: it serves ten PAL handlers that involve no kernel
at all. That question has now been deferred three stages running, which is itself the
argument for giving it a stage of its own rather than a paragraph in each.

**One behaviour change, and it is a relaxation.** The old handler resolved the address
pointer *before* deciding whether the kernel reads it, so a symbolic pointer at a
declared length of zero aborted the interpreter even though no byte moves. It is now
admitted, exactly as `getsockname`'s zero-length copy-out already was.

**Correctness oracle**: `WoofWare.PosixKernel.Test/TestConnect.fs`, 29 cases, which
closes the gap 9k opened and named. The admission's screens and their order; the
copy-extent table on both flavours, which is where they disagree at a length of 1; the
buffer arms; the field-mismatch refusal; and a floor under the ladder itself
(completion, EINPROGRESS, EISCONN, ECONNREFUSED) so that a client which is not PawPrint
no longer has the largest function in this library untested.

**Mutation battery**: fifteen mutants of `admitConnect`, `connect` and `reachedBy`, all
killed by the library's own suite. One of the first fifteen written was a no-op — both
arms of the mutated match answered EFAULT — and it read as a survivor until the mutation
itself was checked, which is the reason to read what a survivor actually changed before
writing a test for it.

**What is left after 9l**: `poll` and `WaitForSocketEvents`.

#### Stage 9m: `poll(2)` moves into the library

**Dependencies**: none beyond the vocabulary 9j-9l settled.

The third socket entry point, and the first of the two that carry a park.
`UnixSystem.poll` takes the caller's entries and its timeout and answers what each
reports and how many carry anything. `SystemNative_Poll` keeps the shim's two screens,
`struct PollEvent`'s layout, the int32 address-space bound, and the copy in and out.

**Nothing needed an admission here**, unlike `connect`. The reason is worth stating,
because the two entry points look alike: `poll`'s copy-in is unconditional — the C fills
its whole `struct pollfd` array before the syscall, at every length and on both
flavours — so there is no per-flavour rule the client would have to know before reading.
`connect` needed two calls precisely because whether the kernel reads at all is measured
and divergent.

**Three failwiths become named refusals**:

* `UnmodelledFlavour` — Darwin's poll readiness, which is a second model rather than an
  extra column (`ERR` and `HUP` are not output-only there, an idle stream socket presents
  nothing, file targets split by kind).
* `WouldPark of timeoutMilliseconds` — nothing ready and a non-zero timeout. A refusal
  rather than `SyscallOutcome.WouldBlock` for the reason `accept`'s is: `WakeCondition`
  has no case carrying a poll's captured entry set and its deadline, so a park here would
  never end.
* `UnmeasuredTarget of fd` — an entry naming a socket event port. **Reachable in a way
  epoll's equivalent is not**: `epoll_ctl` screens the targets it will accept, and
  `poll(2)` accepts any descriptor. It was a `failwith` inside the level function and is
  now a refusal from the entry point, which is where it belongs — the level function
  keeps a `failwith` for it, because `poll` screens it first.

`pollReadinessOfDescription` moves beside `SocketEventPort.epollReadinessOfDescription`,
the sibling its docstring was already written against. `SocketFuzz` was its only other
caller, and goes straight to the library.

**One ordering change, and it is between two refusals rather than two answers.** The
Darwin refusal used to precede the entry decode, so a Darwin-flavoured kernel with an
unresolvable `pollEvents` pointer refused about the flavour; it now decodes first and
refuses about the pointer. Both abort the interpreter, so no guest can tell — but the
coarseness the old message argued for is preserved, because the library refuses ahead of
the *entries*: a zero-entry Darwin poll still refuses rather than answering the one row
the flavours agree on.

**Correctness oracle**: `WoofWare.PosixKernel.Test/TestPoll.fs`, 15 cases. Three reach
what a guest cannot — the Darwin refusal (a guest runs one flavour), the port entry (no
managed caller polls one), the park refusal (a guest reaching it aborts). The rest are
per-entry rules a guest can reach only in combination: the negative descriptor that is
neither an error nor `NVAL`, `NVAL` reported unrequested, `HUP` output-only where `OUT`
is not, `PRI` askable and never reported, a regular file always-ready under every access
mode, and the count that is neither the entry total nor the condition total.

**Mutation battery**: ten mutants, and the first run left one alive —
`pollReadinessOfDescription`'s regular-file row, which no test polled, because the
fixture built sockets and standard streams and never opened a file. That row is measured
(`pollgaps.c`) and had come across the boundary untested. A case was added and the
mutant dies. A second mutant did not apply at all, its anchor matching the epoll sibling
as well as the poll one — which is the same file now holding both, and a reminder to
read what a `DID-NOT-APPLY` actually matched.

**What is left after 9m**: `WaitForSocketEvents`, the one that really parks.

#### Stage 9n: `WaitForSocketEvents`' screens move — stage 9 done

**Dependencies**: none beyond the admission vocabulary 9l settled.

The last of the four socket entry points. `UnixSystem.admitSocketWait` carries
`epoll_wait(2)`'s four screens and `kevent(2)`'s two, in the order each kernel applies
them. `SystemNative_WaitForSocketEvents` keeps the park machinery, and that division is
the interesting part of this slice.

**What stays in PawPrint, and why it is not a compromise.** The park is not a kernel
concept this library is missing; it is *how a client suspends its own execution*. The
re-entrant native frame, the scheduler status, the captured `ParkedSocketWait` consulted
in place of arguments the guest may have scribbled on, the wake driven by `Program`'s
readiness sweep — none of that is something a POSIX simulator can own, because a
simulator has no threads to suspend. The library's contribution is the vocabulary
(`ParkedSocketWait`, `WakeCondition.SocketEventDeliverable`, `SocketEventPort.drain`),
which stages 9c–9f had already moved. So this stage is the *screens*, and the screens are
all that was left.

Two more things did not move, deliberately:

* The sentinel written through `*count` on failure — 0 under epoll, -1 under kqueue — is
  `WaitForSocketEventsInner`'s, not either kernel's. A client that keeps no such cell
  writes nothing.
* The buffer's *classification*. The extent screen is the kernel's (`access_ok` over
  `maxevents * EventSize`), so the library performs it — but it takes a `UserBuffer`,
  because whether a pointer names storage at all is PawPrint's question.

**An admission rather than an answer**, for the reason `connect` has one: the call may
not return, and what it does instead is the client's business. Three outcomes — an
errno, Darwin's answer-zero-events-immediately row, and "reach the port with this
maxevents", which the handler turns into its existing drain-or-park.

**A helper fell out.** `NativeSystemNative.faultsBeforeOperation` lost its only
production caller, the library now performing that composition itself. Its three tests
did not lose their subject: they pin how a `BufferPointer` *maps* onto what the screen
sees, which is still a real claim about `toUserBuffer`. So the composition moved into
the fixture that tests it, rather than the tests being deleted along with the helper —
and, equally, rather than a production helper being kept alive by its tests.

**Correctness oracle**: `WoofWare.PosixKernel.Test/TestSocketWait.fs`, 15 cases. This is
the entry point that needed them most: **five of its eight measured rows differ between
the flavours**, and a guest runs one, so half the table had never had a test that could
reach it. The orderings are the point — descriptor before count before buffer before
object kind under epoll, descriptor before count under kqueue — and each adjacent pair
is separated by an input that provokes exactly one of the two, which is how they were
measured in the first place.

**Mutation battery**: eleven mutants, all killed by the library's own suite. Both
wrong-kind answers, both zero-count answers, the `EP_MAX_EVENTS` cap off by one, the
buffer screen skipped, its extent reduced to one element, all three epoll orderings
reversed one at a time, kqueue given a buffer screen it does not have, and the
addressless refusal turned into an answer.

**Stage 9 is done.** Every syscall the four socket entry points implement now lives in
`WoofWare.PosixKernel`, the PAL allowlist is empty, and the packaging is in place. What
PawPrint keeps at these four entry points is exactly guest memory and its own scheduler.

**What the extraction has not done**, and is worth writing down now rather than
rediscovering: `struct sockaddr_in`'s byte layout is still PawPrint's, deferred with an
argument in 8m, 9j and 9l. Three deferrals with the same reasoning is the case for
giving it a stage rather than a paragraph. The layout serves ten PAL handlers that
involve no kernel at all, so the question is not "move it" but "which of the two things
called `sockaddr_in` is which" — the kernel's struct, and the byte array CoreLib passes
around, which agree numerically today and need not.

### Stage 10: `struct sockaddr_in`'s layout moves to the library

**Dependencies**: stage 9, whose 8m, 9j and 9l deferrals this discharges.

#### The premise the deferrals assumed, and what measurement says

Stages 8m, 9j and 9l each left `SockaddrOffsets` in `WoofWare.PawPrint` with the same
argument: it serves ten PAL handlers that involve no kernel at all, so it looks like PAL
vocabulary. The natural next step reads as "map PawPrint's layout onto the kernel's".

There is no such mapping to write, because there is only one layout.

`docs/plans/2026-08-23-posix-kernel-extraction/sockaddr-layout.c`, run 2026-08-29 on
Linux 6.18.5 (container) and Darwin 25.6.0 (host):

| fact | Linux 6.18.5 | Darwin 25.6.0 |
| --- | --- | --- |
| `sa_family` | offset 0, width 2 | offset 1, width 1 |
| `sin_port` | 2 | 2 |
| `sin_addr` | 4 | 4 |
| `sin6_flowinfo` | 4 | 4 |
| `sin6_addr` (16 bytes) | 8 | 8 |
| `sin6_scope_id` | 24 | 24 |
| `sizeof` in / in6 / storage | 16 / 28 / 128 | 16 / 28 / 128 |

The only divergent field is `sa_family`, and the library **already** owns it
(`SockaddrFamilyField`), along with every size (`socketAddressSizes`). Everything
PawPrint still holds is flavour-invariant.

The other half is a fact about upstream's source rather than a measurement:
`SystemNative_SetIPv4Address` does
`struct sockaddr_in* inetSockAddr = (struct sockaddr_in*)sockAddr`, casting the caller's
byte array directly to the kernel's struct. The managed `SocketAddress` buffer *is* a
`sockaddr_in`. So `SockaddrOffsets` is a private transcription of the kernel's struct
sitting on the wrong side of the boundary, and nothing checks that the transcription is
right.

**One byte is not like the others, and the claim has to be stated carefully.** It would
be wrong to say the PAL exists so that managed code never needs the layout: managed code
knows one thing about it. `pal_networking.c` mentions `sa_len` zero times; the length
byte a guest sees at index 0 is written by `SocketAddress`'s own constructor, storing
`(byte) _size` unconditionally on every platform and relying on Linux's two-byte family
write to clobber it. `SockaddrFamilyField.OneByteAtOffsetOne`'s docstring already records
this. That is still an assumption about the *kernel's* layout rather than a second one,
so the "one layout" claim survives — but the two directions must be cross-referenced
once they share a file, because the encoder models the kernel filling `sa_len` on
copy-*out* while the docstring describes managed code writing it on the way *in*, and a
reader meeting both without a pointer between them will think one is wrong.

#### What is the kernel's and what is the PAL's

* **Kernel, and so the library's**: the field offsets and widths — including
  `InternetV6AddressLength`, which is `sizeof(struct in6_addr)` and therefore the
  `sin6_addr` descriptor's *width* rather than a constant of its own; the fields' byte
  orders; the `sa_len` byte a BSD kernel fills in on copy-out; and
  `internetSockaddrBlob`, which is wholly "encode an endpoint as this platform's
  `struct sockaddr_in`".
* **The PAL's, and so PawPrint's**: `IsInBounds`, the
  `socketAddressLen < sizeof(struct sockaddr_in)` screens, the
  EFAULT/EINVAL/EAFNOSUPPORT choices, and every handler's decision about *whether to
  swap bytes*.

That last item is a distinction worth keeping sharp. The fields' byte orders are kernel
ABI, but the swap decisions are the PAL's contract with its own out-parameters:
`SystemNative_GetPort` byte-swaps and `SystemNative_GetIPv4Address` does not — it copies
the address word verbatim, because both sides of that call hold it in network order.
**So the descriptors carry offset and width only, and byte order stays as prose on
each.** Making it data would invite an order-normalising accessor, and the first handler
to use one would silently acquire an `ntohl` that upstream does not have.

#### How the library states the layout

* **Option A: move the constants verbatim.** Cheapest. Hands callers arithmetic rather
  than answers, and a bare numeric table under an innocent name is precisely what
  `check-pal-residue.py` cannot see. It is *not* PAL residue — it came out of the C
  headers — but nothing in the code would tell a future reader that.
* **Option B: a field descriptor per struct**, in the shape `SockaddrFamilyField`
  already has. Callers ask where `sin_port` is rather than what 2 is, and
  `SockaddrField.reachedBy` subsumes both the family predicate 9l added and bind's
  hand-written `declaredLength >= SockaddrOffsets.InternetAddress + 4`.
* **Option C: codec functions only** — no offsets exposed at all. Strongest boundary;
  cannot serve the ten PAL handlers, which manipulate single fields of a partly-filled
  buffer (`SetPort` on a blob whose address is not set yet).

**Chosen: B, with C's encoder written in terms of it.** B serves the field handlers; the
copy-out path (`getsockname`, `accept`) is a whole-struct operation and gets the
encoder. Both live in `SimulatedUnixPlatform.fs`, beside the layout facts the library
already owns: `InternetEndpoint.fs` compiles before it, and the encoder needs both.

`sin_port` and `sin6_port` become **two descriptors with the same value** rather than the
one shared constant they are today. They are two fields of two structs that happen to
coincide, and the oracle below checks each against its own struct. The same goes for
`sin6_flowinfo` and `sin_addr`, which both sit at 4: a mutation swapping those two
remains a no-op, and the gain is that the use site now names which struct it means.

Offsets go in as **constants rather than `platform -> int` functions**, matching how
`internetAddressFamily` (a constant) sits beside `internetV6AddressFamily` (a function).
That distinction is already how this library records which facts were measured to agree.

#### The oracle

`IPEndPoint.Serialize()` returns the host PAL's own `sockaddr`, built by the same
managed and PAL writes a guest would perform. Measured 2026-08-29 on both:

```
Darwin 25.6.0
  v4 1.2.3.4:0x1234   size=16  10 02 12 34 01 02 03 04 00 …
  v6 ::1%7:0x1234     size=28  1c 1e 12 34 00000000 …0001 07 00 00 00
Linux 6.18.5 (container)
  v4 1.2.3.4:0x1234   size=16  02 00 12 34 01 02 03 04 00 …
  v6 ::1%7:0x1234     size=28  0a 00 12 34 00000000 …0001 07 00 00 00
```

Both rows were run rather than inferred. Every fact this stage moves is
flavour-invariant, so **both** hosts check all of them: the port at 2 in network order,
the address at 4, the sizes. What splits by column is only what the library already owns
— the family field (`10 02` against `02 00`) and AF_INET6's number (`1e` = 30 against
`0a` = 10), the latter being a genuinely new check.

The sizes are *not* new: `sourcesPure/SocketAddressScreens.cs` already asserts 16, 28 and
128 differentially against the real host runtime. What is new is a check in the
**library's own** suite, and a direct host-equality one rather than a guest's assertion.

Two things the test's prose must say, or a future failure is hard to read. `Serialize`
witnesses the *inbound* producer while `internetSockaddrBlob` models the kernel's
*copy-out*; they agree byte for byte, and the evidence for the copy-out `sa_len` remains
the measured comment that travels with the encoder. And the v6 row exercises
**descriptors only** — the library has no IPv6 endpoint type, so the encoder is v4-only
and the v6 expected bytes are hand-assembled from the descriptors.

#### Correctness oracle

* The host-equality test above, in `WoofWare.PosixKernel.Test`, which needs no PawPrint
  reference and so does not disturb `TestNoPawPrintReference`.
* The existing suites unchanged: the ten PAL handlers have guest coverage
  (`sourcesPure/SocketAddressScreens.cs`, `sourcesImpure/SocketAddress{Linux,Darwin}Bytes.cs`),
  and `getsockname`/`accept` have theirs.
* A mutation battery over the moved descriptors and the encoder, run against both
  suites. Recorded in advance and confirmed: swapping the two fields that share offset 4
  is a no-op mutant and cannot be killed. Eleven others die, every one in the library's
  own suite — each field moved, `sin6_addr` narrowed, `reachedBy` off by one, the
  encoder's two byte orders, its `sa_len`, and `AF_INET6`'s number.

  Two of those mutants can only be killed on a Linux host, and this machine is macOS, so
  they were run in a container against the real fixture: `AF_INET6` = 30 everywhere kills
  one row, and the Linux family field claiming Darwin's shape kills four. Both columns of
  the oracle are therefore load-bearing rather than one being carried by the other.
* `scripts/check-docstring-attachment.py` against the branch point, which this stage
  needs because it moves definitions between files.

#### What review found

Three things, and two of them are about the fixture rather than the move.

* **The skip could not run.** `hostFlavour` and the two serialized blobs were
  module-level bindings, so on a host with no preset F# forces them — and throws —
  before NUnit reaches the guard. Functions now, with the guard as a `SetUp` every test
  passes through. A guard that cannot run is worse than none, because it reads as
  handled.
* **The guard asked the wrong question.** It picked a preset by flavour alone, which
  assumes the preset's *machine* is this one; both presets are little-endian, and one row
  reads byte 0 of a Linux `sockaddr` as the family's low half, which on a big-endian
  Linux would be its high half. It now wants a little-endian Linux or macOS host. Only
  that one row actually breaks on big-endian — the others compare host-order writes
  against host-order reads and would agree — but a fixture comparing a preset against a
  machine should say which machines the preset describes.
* **`reachedBy` could wrap.** `Offset + Width <= declaredLength` was safe while its only
  inputs came from a closed DU, and is not now that `SockaddrField` is a public record:
  `{ Offset = Int32.MaxValue; Width = 1 }` wrapped onto a bound every length satisfies.
  It refuses a negative offset or width, and the comparison subtracts rather than adds —
  the same rearrangement, for the same reason, as
  `UserBufferCheck.faultsBeforeOperation`. This is the "publishing inherits private
  preconditions" shape: nothing about the arithmetic changed, only who can reach it.

  `TestSockaddrField.fs` covers the rewritten predicate, and its own battery of five
  mutants all die — including "the addition comes back", which the overflow row kills, so
  the rearrangement is load-bearing rather than decoration.

#### In scope, and easy to under-plan: the prose

`SockaddrOffsets`' docstrings are PAL-flavoured throughout — `FlowInfo` cites
`SystemNative_SetIPv6Address`, `InternetAddress` cites "the managed caller",
`InternetV6AddressLength` is named for upstream's `NUM_BYTES_IN_IPV6_ADDRESS`. Moved
verbatim, that is exactly the residue the check cannot see and that three consecutive
stages nearly shipped. This stage is a prose pass as much as a move.

#### What this does not overturn

8m argued that the blob "does not cross" the boundary, and 9j and 9l repeated it. That
reasoning was about the *entry points' signatures*, and it stands:
`UnixSystem.getsockname` and `UnixSystem.accept` still answer an `InternetEndpoint`, not
bytes. Only the pure encoding function relocates. This discharges the deferral without
reopening the question those stages actually settled.

#### Not proposed

Now that `connect` has an admission, the library *could* take the raw sockaddr bytes and
decode them itself; the ordering objection from 9l is gone. `ConnectFields` stays anyway:
it works, it is tested, and rewriting it buys only symmetry with a decoder the PAL
handlers still cannot use.

### Stage 11: `epoll_ctl(2)`'s ladder moves into the library

**Dependencies**: none beyond the vocabulary stage 9 settled.

Stage 9's closing note said "the four socket entry points", and that undercounted. Five
socket syscalls still held their kernel ladder in `WoofWare.PawPrint` after stage 10:
`bind` (379 handler lines), `listen` (152), `epoll_ctl` (141 plus an 82-line
`EmulatedKernel.changeSocketEventRegistration`), `socket` (102 plus 39), and the two
non-blocking `fcntl`s (86 and 58). This is the first of them, chosen because it is the
one whose ladder was measured to be entirely library-eligible: its body touches
`FileDescriptorRegistry`, `ReadinessLevel`, `SocketEventPort.epollReadinessOfDescription`
and the change DU, and **nothing** PawPrint-only.

#### What moved, and what stayed

* **The ladder itself**, verbatim apart from the aggregate rename, as
  `UnixSystem.changeSocketEventRegistration`.
* **The Darwin refusal.** kqueue's registration model is structurally different rather
  than differently numbered — per `(ident, filter)` state, a silently-replacing `ADD`, a
  regular file registering where epoll answers `EPERM`, a `DEL` of a dead target
  answering `ENOENT` where epoll answers `EBADF` — each measured only far enough to know
  that it diverges. That is a statement about what this kernel models, so it is the
  library's `SocketEventRegistrationRefusal.UnmodelledFlavour` rather than a `failwith`
  in a handler.
* **The errno mapping.** `SocketEventRegistrationError`'s six cases each already named
  their errno *in the docstring* — `EBADF`, `EPERM`, `EINVAL`, `EEXIST`, `ENOENT` — while
  the only code that mapped them was a hand-written `match` in PawPrint's handler.
  `SocketEventRegistrationError.toErrno` makes the library's own prose executable, and
  every client now answers the same numbers. Note it is not injective: two refusals share
  `EBADF`, so the case survives for a client that wants to know which.

Staying in PawPrint: the shim's `SupportedEvents` screen and equal-mask short-circuit
(already `SocketEventsPal`), the `data` decode from a `CliType`, and the derivation of
the op from the caller's *claims* about the current and new masks — that last is
`TryChangeSocketEventRegistrationInner`'s own arithmetic, not the kernel's.

#### The answer shape

`Result<SocketEventRegistrationAnswer * UnixSystem, SocketEventRegistrationRefusal>`,
where the answer is `Changed` or `Failed of reason`. The alternative was to keep the
existing `Result<UnixSystem, SocketEventRegistrationError>` and add the refusal as a
second `Result`, which nests; folding the kernel's own refusal into the answer keeps one
`Result` for "this library has nothing to say" and matches `accept` and `connect`.

#### Correctness oracle

`WoofWare.PosixKernel.Test/TestSocketEventRegistration.fs`, 8 cases covering what this
function adds over the registry's own ladder — which already has its rows. The flavour
refusal ahead of even the descriptor lookups; the ordinal that only an `Add` consumes and
that a refused change leaves alone; and the pending rule, including that a `Modify` of an
already-pending entry does not re-append it and that a target with nothing to report does
not become pending. That last row needs a target whose level is *empty under the
interest*, which an idle socket never is — `HUP` is reported unrequested — so it uses
standard output under a read-only interest.

Eleven fixture call sites in `TestSocketEventDelivery` and three in `SocketFuzz` move to
the new shape. The mechanical rewrite mis-attributed one block: a positional scan for
"the next result-handling after each call" ran past a call whose handler was
assertion-shaped rather than the common `| Ok kernel -> kernel`, and rewrote a `close`'s
handler instead. The compiler caught it, and the lesson is the ordinary one — a
positional scan needs to check what it landed on, not merely that it found something.

**Mutation battery**: ten mutants over the ladder and the errno mapping.

Seven of the ladder die in the library's own suite: the flavour arm, both directions of
the ordinal rule, the already-pending guard, the not-ready guard, and dropping the
interest from the readiness test. One (a `Remove` falling into the registration branch)
was written as invalid F# and never ran; it is recorded as not-run rather than as a
result.

The three over `toErrno` **survived both default suites on the first run**, which is the
9g failure mode and this time self-inflicted: the stage moved prose into code and gave
the code no test, leaving it reachable only from the guest tier. Three rows were added —
the six mappings as literals, the two `EBADF` refusals staying distinguishable, and the
count of distinct errnos — and all three mutants then die. The mapping being *stated* in
six docstrings is exactly why it was easy to skip: it looked tested.

**What is left after 11**: `bind` and `listen` (which share the bind-conflict relation and
probably want to travel together), `socket`, the two non-blocking `fcntl`s, and the
fixture relocation — 3893 lines across `TestEmulatedKernelSockets`, `TestSocketEventDelivery`
and `SocketFuzz` that test library behaviour from the client's suite, which is the 9g
failure mode at scale.

### Stage 12: the sockaddr copy-in admission is `bind`'s as well as `connect`'s

**Dependencies**: 9l, which built the admission; 11, only for the branch order.

Preparation for `bind`'s move, and separated from it so that `bind`'s diff is a move
rather than a move plus a refactor. No behaviour changes.

Reading `SystemNative_Bind`'s 379 lines against `UnixSystem.admitConnect` shows the same
screens in the same order: the descriptor to `EBADF`, the target to `ENOTSOCK`, the
domain to a refusal, `bindAddressLength` to its outright rejection, then the per-flavour
question of whether the kernel touches the buffer at all, then which fields the copy
contains. That is not a resemblance to be exploited; it is a measurement already recorded
in this library, which is why `SimulatedUnixPlatform.bindAddressLength` is named for
`bind` and called by `connect`.

So the admission is renamed for what it is rather than for its first caller:
`admitConnect` → `admitSockaddrCopy`, and `ConnectFields`, `ConnectAdmission` and
`ConnectRefusal` → `SockaddrCopyFields`, `SockaddrCopyAdmission` and
`SockaddrCopyRefusal`. `UnixSystem.connect` keeps its name and its signature apart from
the refusal type.

One simplification falls out. `ConnectAdmission.Answered` carried a `ConnectOutcome`,
which can be `Completed` — but every screen preceding the copy can only *refuse*, and no
arm ever built a `Completed`. The shared case carries a `UnixError`, so the type no
longer admits a value the function cannot produce, and `connect` does the one-line lift
into its own outcome.

**What `bind` will still need separately**: the length *verdict*, which it uses twice —
once for the outright rejection the admission already answers, and once as a member of
its fault set. It asks `bindAddressLength` again rather than the admission carrying a
third component; that is two callers of one pure function rather than a rule stated
twice.

**Correctness oracle**: the suites unchanged. This is a rename plus one type
simplification the compiler checks end to end, so the evidence is that 813 library tests,
3257 PawPrint tests and 1033 guests all pass with no edit beyond the renames — and, in
`TestConnect.fs`, unwrapping five `Answered (ConnectOutcome.Failed e)` patterns into
`Answered e`.

**A prose pass, as every one of these needs.** The shared types' docstrings all described
`connect` — "for a `connect(2)` whose sockaddr the kernel is about to copy in", "there is
no destination to connect to" — which the rename made false rather than merely narrow.
They now say what they are shared by, and where the sharing is measured.

**What is left after 12**: `bind`, then `listen` (which needs the bind-conflict relation
`bind`'s move will have taken across), `socket`, the two non-blocking `fcntl`s, and the
fixture relocation.

### Stage 13: `bind(2)` moves into the library

**Dependencies**: 12, which generalised the copy-in admission so that this is a move.

The largest of the socket handlers, 379 lines, and after 12 the move is mechanical: the
first half of it *is* `admitSockaddrCopy`, and the second half is a fault set, an
ordering, and an allocation, all of which are kernel facts.

`SystemNative_Bind` keeps the shim's null and negative-length screens, `requireBufferRoom`,
the field reads, `SocketArgumentsPal.isTcpProtocolType`, and the errno write.

**Two things the move had to reshape.**

* **The `SO_REUSEADDR` write outlives every failure**, so it cannot sit behind the
  admission: measured, after a bind that answered EFAULT the option still reads back set.
  `UnixSystem.bind` therefore resolves the descriptor itself, applies the write, and only
  then calls the admission — which resolves the descriptor again. That is a lookup
  repeated, not a rule; the same shape `connect` already has, where the entry point
  re-derives the admission it was given.

  It also means the handler cannot return early on `Answered`: an admission failure still
  has to go through `UnixSystem.bind` so that the write happens. The handler's two arms
  converge instead, with `family`/`endpoint` as `None` on the answered path.

* **`privilegedPortCeiling` moves** from `EmulatedKernel` to `SimulatedUnixPlatform`, as
  a constant rather than a function of the platform: measured 1024 on both. Review
  caught that the first attempt *copied* it rather than moving it, leaving two public
  definitions of 1024 that nothing forced to agree — the failure mode this whole
  extraction exists to prevent, committed while performing it.

**Two new refusals**, each a `failwith` in the handler today. `UnmodelledMulticast`, and
`EphemeralPortsExhausted` for a port-0 bind that finds the whole range taken — the
library refuses rather than inventing the `EADDRINUSE` a real kernel gives, which has not
been measured under this allocator.

`SockaddrCopyFields.checkSupplied` is extracted from `connect`, since `bind` is now a
second caller of the same contract and a wrong field set is just as silent there.

**Correctness oracle**: `WoofWare.PosixKernel.Test/TestBind.fs`, 29 cases. The screens
`bind` shares with `connect` are `TestConnect`'s; what is only here is what `bind` adds —
each fault on its own, the `SO_REUSEADDR` write surviving two different failures, the
ephemeral allocation and its exhaustion, and the fault *ordering*.

**Two mistakes worth recording, both mine and both caught by the fixture.**

* A row asserted that an already-bound socket asking for a multicast address answers
  EINVAL "because `AlreadyBound` outranks the address on both flavours". It does not:
  measured, Linux ranks `AddressNotLocal` *ahead* of `AlreadyBound` and Darwin the other
  way, so the same input is refused on one flavour and answered on the other. The
  corrected row is better than the one I meant to write — it is now the row that shows
  the multicast refusal is genuinely gated on the ordering, and so what "refused late"
  buys: a gap in the model that a higher-ranked fault can hide.
* Two ordering rows computed their expected errno by calling `firstBindFault`, which is
  the function under test — a mirror oracle that would have agreed with any order at
  all. They state the errnos as literals now.

**Mutation battery**: fifteen mutants, fourteen killed by the library's own suite —
both directions of the `SO_REUSEADDR` write, the port ceiling in three ways, the
per-transport namespaces, already-bound, the AF_UNSPEC split, the whole Darwin fault
order, the multicast refusal, the allocated port being reported rather than the requested
one, the address lock, the exhaustion refusal, and an errno swap.

The fifteenth — removing the `Port > 0us` guard on the address-in-use fault — **survived,
and could not have done otherwise**. `bindConflict` answers `false` outright when the
ports differ, and no bound socket ever holds port 0, since every port-0 request
allocates. So the guard restated a fact rather than enforcing one, and no test could
falsify it. It is deleted, with the reasoning kept as a comment: a guard nothing can
falsify is a guard nobody can maintain.

**What is left after 13**: `listen` (which needs the bind-conflict relation this move
took across), `socket`, the two non-blocking `fcntl`s, and the fixture relocation.

### Stage 14: `listen(2)` moves, and `socketOfFd` falls out

**Dependencies**: 13, whose move brought the bind-conflict relation across.

The shortest of the five, and the cleanest: `SystemNative_Listen` had, in its own words,
"no screens of its own: it is `listen(2)`". So the whole handler is now the errno write,
and `UnixSystem.listen` is the rest.

**The relation `bind` and `listen` share** — "does another socket's binding conflict with
mine" — was a closure inside each of them. It is one function now,
`UnixSystem.bindingConflicts`, because it is one kernel rule; the two callers differ only
in *when* they ask. `listen` asks it twice: once on the flavour whose `listen(2)` re-runs
the admission, and once per candidate port for the implicit bind an unbound `listen`
performs.

**Three `failwith`s become refusals** — the unmodelled domain, the unmeasured kinds, and
the unmeasured phases — plus `EphemeralPortsExhausted` for the implicit bind.

One shape had to change on the way. The handler screened the phase with a `unit`-typed
match whose other arm was a `failwith`, which types as anything and so could sit in the
middle of the fall-through. A refusal that is a *value* cannot: it has to be produced
before the rest of the function continues, so the phase check is now an
`option`-returning match followed by a match on that.

**`socketOfFd` is deleted.** It was the shared descriptor ladder for `bind`, `listen`,
`accept` and `connect`; all four have moved, and its last reference went with this stage.
Checked against the whole solution rather than one file, which is the lesson stage 9n's
`faultsBeforeOperation` taught: that one *looked* dead in `NativeSystemNative.fs` and had
three callers in a fixture.

**Correctness oracle**: `WoofWare.PosixKernel.Test/TestListen.fs`, 27 cases. Every row is
`listen(2)`, since nothing here belongs to a caller. Two are reachable only at this tier:
the binding re-screen, which Linux performs and Darwin does not, and the implicit bind's
exhaustion.

**A premise I got wrong, again in a fixture rather than in the code.** The re-screen row
first set up two reuse-carrying sockets both merely *bound*, and expected the second
`listen` to be EADDRINUSE on Linux. It is not: Linux's relaxation holds while nothing
listens, so a pair that has only bound does not conflict at all — it is the second
`listen`, with the first already listening, that finds one. The corrected row sets the
holder listening, and a new row asserts the complement: the *first* listener of such a
pair is admitted on both flavours. Without that second row the first would pass for a
rule that refused any duplicate binding.

**Mutation battery**: twelve mutants, all killed by the library's own suite — the
re-screen in three ways (on both flavours, on neither, and with the predicate inverted),
the implicit bind's address, its lock and its conflict check, the re-listen's queue, the
backlog clamp, the datagram answer, the kind/phase ordering, and both halves of the
shared conflict relation. Nothing here needed PawPrint's suite consulted.

**What is left after 14**: `socket`, the two non-blocking `fcntl`s, and the fixture
relocation. Also noted in passing: `UnmodelledDomain` now appears as a case in four
refusal DUs with the same shape and meaning (`accept`, `sockaddrCopy`, `listen`, and
`bind` through the first of those). A shared case would be tidier; it is deliberately not
done here, being churn across three shipped entry points for a naming gain.

### Stage 15: the last three socket state transitions

**Dependencies**: none beyond the vocabulary the stages above settled.

`socket(2)`'s state transition and the two non-blocking `fcntl`s — the last of the five
this audit found still holding kernel logic on PawPrint's side.

**`socket` is mostly not a move.** Its handler is nearly all PAL: the wrapper's null
screen, the `*createdSocket` store, `SocketArgumentsPal.socketCreation` and the three
conversion errnos are the shim's, and stay. What moves is
`EmulatedKernel.createSocket` — the "mint a socket and a descriptor onto it, agreeing"
transition — as `UnixSystem.createSocket`, with an adapter left behind for the nine
fixtures that call it.

**The two `fcntl`s are almost entirely kernel logic**, once the PAL return convention
(0, or -1-and-errno, or the odd `Error_EFAULT` enum on a null out-pointer) and the
argument decoding are taken off. `UnixSystem.setNonBlocking` and
`UnixSystem.isNonBlocking` carry the rest.

Three things about the setter are worth having in the library rather than in a handler:

* **The flag lands on the open file description**, where POSIX keeps the status flags, so
  a `dup` sees it.
* **A standard stream refuses to be set** — no modelled stream transfer consults the
  flag, so storing it would keep blocking semantics silently. *Clearing* it is answered,
  because `false` is what a stream already reads back: the refusal is about a divergence
  that clearing does not create.
* **An event port stores the flag whatever it answers.** Measured, the platforms agree
  that the bit toggles and disagree on the answer — Linux succeeds where Darwin reports a
  failure with the bit toggled anyway. That is why `SetNonBlockingAnswer.Failed` still
  hands back a system.

**Correctness oracle**: `WoofWare.PosixKernel.Test/TestNonBlocking.fs`, 11 cases. The
event-port split is reachable only here — a guest runs one flavour, and the managed
surface never sets the flag on an event port at all — and so is the `dup` sharing, which
no guest exercises through this entry point.

**Mutation battery**: ten mutants, all killed by the library's own suite — a fresh
socket's binding, reuse flag and identity counter; the standard-stream refusal in both
directions, including the one that would refuse a *clear*; the event port storing when it
fails and answering when it should not; the dead descriptor on both the setter and the
getter; and the getter reading the flag at all.

**What is left after 15**: the fixture relocation, and nothing else from the audit.
`TestEmulatedKernelSockets` (1903 lines), `TestSocketEventDelivery` (1088) and
`SocketFuzz` (902) test behaviour that is now almost entirely library code, from the
client's suite. That is the 9g failure mode at scale, and it is why every stage since 9j
has had to write a fresh fixture rather than move one.

### The fixture relocation is not one move

Stage 15 left one audit item: `TestEmulatedKernelSockets` (1903 lines),
`TestSocketEventDelivery` (1100) and `SocketFuzz` (905) test behaviour that is now
almost entirely library code, from the client's suite. Reading them, that item
decomposes, and one of its parts is a *production* move rather than a test move.

Measured on `5ee5301a`, the only `WoofWare.PawPrint` names the three fixtures use are:

| name | where it lives | uses |
| --- | --- | --- |
| `EmulatedKernel` (the record and its adapters) | `WoofWare.PawPrint/EmulatedKernel.fs` | 128 |
| `EmulatedKernel.checkInvariants` / `EmulatedKernelDefect` | ditto | 41 |
| `SocketEventsPal`, and one mention of `SocketPal` | `WoofWare.PawPrint/Native/` | 16 |
| `KernelSyscall` | `WoofWare.PawPrint.Test/KernelSyscall.fs` | 6 |

Everything else they name — `SocketEventPort`, `ReadinessLevel`,
`OpenFileDescriptionId`, `UnixMachineState`, `PollEvents`, `UnixTaskTable`,
`ParkedSocketWait` — is already the library's. So the relocation is four slices, not
one:

* **16**: the cross-table invariant checker moves into the library. This is the
  blocker: `TestEmulatedKernelSockets` asserts against `checkInvariants` in 30 places,
  and it cannot move while the checker is PawPrint's.
* **17**: the nine library fixtures' 45-line hand-rolled starting system becomes one
  shared definition. Test-only, and it stops each relocation adding a tenth copy.
* **18–20**: the three fixtures themselves, one per PR.

### Stage 16: the cross-table invariant checker moves into the library

**Dependencies**: none.

`EmulatedKernel.checkInvariants` is ~290 lines answering "every way this kernel's
tables disagree with each other". Of its `EmulatedKernelDefect` cases, **17 name only
library types** — `SocketId`, `ConnectionId`, `OpenFileDescriptionId`, `InodeNumber`,
`DirectoryStreamId`, `AbsoluteUnixPath`, `SocketKind`, `SocketPhase` — and exactly two
blocks name a PawPrint concept: the `DIR*` block map (`NativeMemoryBlockId`, a native
heap block id, three cases) and `checkTaskInvariants` (`ThreadId`/`ThreadStatus`, four
cases, and a separate function already).

That the docstring says "`EmulatedKernelDefect` is PawPrint's vocabulary" is true of
`checkTaskInvariants`, where the thread set is PawPrint's, and false of the other
seventeen.

It has **no production caller at all** — `TestTaskState.fs` says so in as many words
("`checkTaskInvariants` is a test-time oracle; nothing in the driver loop runs it"),
and the grep agrees. So the blast radius is entirely test-side, which is what makes
this cheap to get wrong and cheap to reverse.

#### Options for the two vocabularies

**(a) The library gets `UnixSystemDefect`; `EmulatedKernelDefect` shrinks to its seven
and gains `| System of UnixSystemDefect`.** One call, one list, one emptiness check;
the wrapper case names the boundary honestly. ~30 assertion sites in
`WoofWare.PawPrint.Test` gain `EmulatedKernelDefect.System (...)`, and most of those
sites are moving to the library in 18–20 anyway, where they lose the wrapper again.

**(b) `EmulatedKernel.checkInvariants` returns the two lists as a pair.** Keeps the
vocabularies separate with no wrapper case, but turns the common `shouldEqual []` into
`shouldEqual ([], [])` and lets a caller check one half and forget the other. That
silent-half failure is exactly what `KernelSyscall.fs`'s docstring says one shared
definition exists to prevent.

**(c) PawPrint does not compose at all**; callers pair
`UnixSystem.checkInvariants (EmulatedKernel.unix kernel)` with
`EmulatedKernel.checkInvariants kernel`, as the existing docstring already tells them
to pair with `VirtualFileSystem.checkInvariants`. Precedent exists, but it turns one
call into two at ~60 sites and a forgotten half is silent.

**(d) One DU stays in PawPrint; the library returns its own and PawPrint maps 17 cases
one-for-one.** Every existing call site is untouched, at the cost of two parallel
17-case DUs and a 17-arm identity mapping the compiler can check for exhaustiveness but
not for meaning. There is no encoding difference here to justify a conversion — unlike
the `*Pal` modules, where there genuinely is one.

**Decided: (a).** The seventeen cases are not PawPrint's vocabulary in any sense; they
name library types about library tables. (d) would keep them looking like PawPrint's
while the library computed them, which is the thing this whole extraction is trying to
stop.

**Correctness oracle**: the checker's own tests move with it. The forging helper the
socket cases need (`FileDescriptorRegistry.Unchecked.ofParts`) is already library-side,
which is what makes the moved tests a move rather than a rewrite.

### Stage 17: the constructor stage

**Dependencies**: none.

Not the test tidy-up the note after stage 16 predicted. Reading the ten fixtures
turned up the gap this plan already recorded when `TestUnixSystemStep` became the
second client — "the library exposes no constructor for either … a second client
cannot make a `UnixSystem` at all without transcribing those … It gets its own
stage." This is that stage, and the duplication was the symptom rather than the
disease.

**The duplication was hiding a live defect.** All ten fixtures parameterise over
`platform` and run both flavours, while hardcoding two fields the flavour fixes:

| field | Linux | Darwin | all ten wrote |
| --- | --- | --- | --- |
| `SoMaxConn` | 4096 | 128 | 4096 |
| `FileSystemType` | `Tmpfs` | `Apfs` | `Tmpfs` |

`EmulatedFileSystemType.isReportableUnder Darwin Tmpfs` is `false` — the library
states outright that a Darwin kernel never reports tmpfs — so every `macOsArm64`
row in the suite ran against a machine no real Darwin could be. The chokepoint that
prevents it already existed: `withUnixPlatformAndFileSystemType`, whose docstring
says the pair "are not independent … fused, it is unrepresentable rather than
merely checked". Writing the record literal is how all ten went around it.

Latent rather than failing, which is why it survived: `SoMaxConn` is read only by
`connectSocket`'s accept-queue capacity check, which needs 128 queued connections
to notice, and nothing reads `FileSystemType` at this tier.

#### Options for the constructor's shape

**(a) — taken.** `initial platform`, deriving the fields the platform fixes.
Smallest surface that closes the stated-goal gap; an impossible machine is
unrepresentable at construction rather than merely checkable; does not pre-empt a
richer `create : UnixSystemConfig -> _` if a second client ever wants one.

**(b)** A `UnixSystemConfig` record with optional fields, mirroring PawPrint's
`KernelConfig`. Every knob named, resolution in one place — but it either
duplicates `KernelConfig` or means moving it, and it decides the whole config
surface now rather than when a client needs it.

**(c)** A no-argument `initial` plus the existing fused setters. Adds one value and
no new decisions, but `withUnixPlatformAndFileSystemType` does not re-resolve
`SoMaxConn` (only `KernelConfig.applyTo` does, separately), so it would need
`SoMaxConn` folding into an already-published setter before it prevented anything.

#### What it deliberately does *not* derive

Two fields look derivable and their own docstrings say why they are not:
`UserAddressLimit` is "a property of the *machine* … rather than of the kernel …
which is why the simulated one is configuration rather than a constant derived from
the platform", and `EphemeralPortRange` is a sysctl either flavour can be set to
anything. `TestUnixSystemInitial` pins both, so a later helpful derivation is caught
rather than shipped.

Eleven POSIX defaults move with `initial`, because stating each value twice across
the boundary is how the two drift. Nine move byte-identical; `defaultUmask`'s
`parseOrFail` context names its new module, and `defaultEphemeralPortRange`'s prose
loses a clause this stage falsified — it called the range "configuration with one
default, in the way `FileSystemType` is", true only while nothing derived
`FileSystemType` either.

`EmulatedKernel.initial` becomes `UnixSystem.initial` plus the CoreCLR-shaped state
and three values PawPrint pins rather than inherits — the environment overlay and
the two PRNG seeds — each stated because a change to a library default must not
silently change what a recorded trace observes.

**Correctness oracle**: `TestUnixSystemInitial.fs`, 13 cases, with the `SoMaxConn`
and `FileSystemType` expectations written as literals rather than by calling the
same derivation the constructor calls — a row that asked `defaultSoMaxConn` what to
expect would agree with any constructor at all, including one that ignored the
platform.

**What is left after 17**: stages 18–20, the three fixtures, one PR each.

### Stages 18-20: the three fixtures

**Dependencies**: 16 (the checker) and 17 (the constructor). Neither fixture could
move before both: `TestEmulatedKernelSockets` asserts against `checkInvariants` in
twenty-two places, and none of the three could build a starting system without
transcribing thirty-one fields.

Each is a move, and each was verified as one rather than read: a script re-derives
the file from `origin/main` by applying exactly the substitutions the move is
allowed to make, and diffs the result. Reflow aside, all three reproduce.

| stage | fixture | lines | tests |
| --- | --- | --- | --- |
| 18 | `TestEmulatedKernelSockets` -> `TestSocketTable` | 1925 | 46 |
| 19 | `TestSocketEventDelivery` | 1106 | 28 |
| 20 | `SocketFuzz` + `TestSocketFuzz` + 5 resources | 1232 | 2 |

The substitutions are the same four kinds every time: the thin adapters, each of
which was `withUnix (f (unix kernel)) kernel`; the projection pair, which is the
identity once the value *is* the system; the forwarding members `EmulatedKernel`
has and `UnixSystem` does not; and `KernelSyscall.close`, whose whole content was
project-call-write-back.

**Every one of the three shed the PAL on the way across.** 18 and 19 replaced
`SocketEventsPal.toInterest` calls with `SocketEventInterest` literals. 20 could
not, because its masks are runtime values from the generated op — but that turned
out to be the interesting case:

**The fuzzer's op language was never the PAL's.** `harness.c` carries its own
`interest_to_epoll` mapping the same five bits onto epoll's, so the mask is a
*fuzzer* alphabet that both sides translate from. Routing the F# half through
`SocketEventsPal` left the two halves of one translation looking unrelated. Stage
20 gives the F# side `interestOfMask`, `interest_to_epoll`'s mirror, sitting in
the same file as the transcript format they share.

**The fuzzer belongs to the library, not to PawPrint.** Its emulated side is
`UnixSystem`; comparing that against a real Linux kernel is a claim about the
simulator rather than about its client.

#### The hazard `check-move-is-rename-only.sh` names and cannot see

`TestSocketFuzz` reads its corpus and its C harness as embedded resources, whose
logical names are `<assembly>.<directory>.<file>` — so they change with the
project. The corpus guards itself (an empty replay is refused as vacuous), but
`harness.c` is read only inside `runHarness`, which only the container-gated live
test reaches. A wrong name there would have gone unnoticed until someone had a
container.

Stage 20 adds the missing guard, and makes it guard the right thing: the name is
one binding used by both `runHarness` and the new row, because a second copy of
the literal would only have guarded itself. Mutating it to the old assembly name
fails the row, which is what says it is load-bearing.

The `PAWPRINT_SOCKET_FUZZ*` variable names are unchanged. They are the documented
interface, and renaming them would break every invocation anyone has recorded.

**The audit is closed** — and the audit turns out not to have been exhaustive;
the next section re-measures what it named.

### The audit named three fixtures; there are nine

Stage 20's PR body said `WoofWare.PawPrint.Test` "no longer holds any fixture whose
subject is library behaviour". That is false, and it is false because it repeated the
stage-15 audit's framing instead of re-measuring. The audit named the three biggest
fixtures; it never claimed to be exhaustive, and nothing checked.

Measured by sweeping every fixture in `WoofWare.PawPrint.Test` that
opens `WoofWare.PosixKernel` and listing the `WoofWare.PawPrint` names each actually
uses, seven fixtures totalling ~2,480 lines are library-subject:

| fixture | lines | subject | the PawPrint names it uses |
| --- | --- | --- | --- |
| `TestEmulatedKernelInodeLifetime` | 586 | when an inode stops existing | `mapUnix`/`unix`, and the `DIR*` block map |
| `TestFileSystemType` | 398 | `fstatfs`'s table, and mount/flavour coherence | `initial`, `mapMachine` |
| `TestSocketBinding` | 382 | the rules behind `bind`, and the port allocator | `initial`, `mapMachine` |
| `TestMonotonicTimestamp` | 361 | `UnixMachineState.monotonicTimestampNanos` | `initial`, `mapMachine`, `KernelConfig` |
| `TestSystemTimeAsTicks` | 281 | `UnixMachineState.systemTimeAsTicks` | `initial`, `mapMachine` |
| `TestEmulatedKernelCurrentDirectory` | 272 | the cwd-as-inode pair | `withFileSystemAndCurrentDirectory`, `KernelConfig` |
| `TestUserBufferCheckAgainstHost` | 197 | `UnixMachineState.userBufferCheck`, against the host | `initial`, `mapMachine` |

...on top of the two splits this document already records as open,
`TestAbsoluteUnixPath` (4 cases forge an `EmulatedKernel`) and `TestFileSystemSeed`
(8 call `RealRuntime.validateSeedForOracle`).

The same sweep separates out the fixtures that open the library and are nonetheless
correctly PawPrint's, so a later reader does not re-open them:
`TestEffectiveProcessorCount` (CoreCLR's `GetCurrentProcessCpuCount`),
`TestRetireStep`, `TestProcessPath`, `TestEnvironmentEntryInvariant`,
`TestDirectoryStreamId` (a `DIR*` is a native block address),
`TestUnixSystemProjection`, and the three `*Pal` fixtures.

The plan's third "Still open" bullet — where the `emulated-posix-kernel` skill should
live — is stale: its paths already name `WoofWare.PosixKernel` and its files.

#### How these were sliced

Two ways to stage this were considered.

**One PR for all seven.** They are mechanically similar and a single derivation script
could carry all of them, which is the argument: a reviewer reads one substitution list
instead of seven. Rejected because only three of the seven are *pure* moves. The other
four each leave something behind — a `KernelConfig` row, the `DIR*` rows, or a
production helper that has to move first — and a split is exactly the case where the
rename oracle detects no rename and the correspondence has to be stated by hand. Seven
correspondences in one PR is the shape nobody checks.

**One PR per fixture, as stages 18–20 did.** Rejected in the other direction: three of
these are a third the size of stage 20's, and two pairs of them share prose that
cross-references its partner, so moving one of a pair falsifies the other's docstring
in a separate commit from the one that fixes it.

So: grouped by what makes each move non-mechanical, and never splitting a pair whose
docstrings name each other.

* **21**: `TestSocketBinding`. A pure move, alone, because it lands beside `TestBind`
  and the ambiguity that creates has to be resolved in the same commit.
* **22**: the clock pair, `TestMonotonicTimestamp` and `TestSystemTimeAsTicks`. Each
  names the other; the first is also a split, since its `KernelConfig.applyTo` rows
  are about PawPrint's config plumbing.
* **23**: `TestUserBufferCheckAgainstHost`, which compares a library rule against a
  measurement of the machine the suite runs on. A pure move, and it takes
  `HostPlatform` — the shared answer to "which kernel is this test host" — with it.
* **24**: `TestFileSystemType`, split. It was paired with 23 above on the strength of
  measuring the same host, but one of its thirteen rows — `this host's own filesystem
  is one CoreCLR will lock` — is not about the library at all. It is an environmental
  premise for a guest, so it stays, and belongs with the other such premises in
  `TestOraclePolicy` rather than in a fixture whose remaining rows have all left.
* **25**: `EmulatedKernel.withFileSystemAndCurrentDirectory` becomes the library's.
  A production move, alone: it turned out to need an API decision about who states
  the remedy for a bad current directory, which does not belong in a commit that also
  rewrites a fixture.
* **25b**: `TestEmulatedKernelCurrentDirectory` follows it, splitting three ways: the
  resolution and invariant rows to the library, the message rows staying with the wrapper
  that formats them, and the `KernelConfig` orphan joining `TestKernelConfig`.
* **26**: `TestEmulatedKernelInodeLifetime`, split — one row stays, and the fixture dissolves.
* **27**: `TestAbsoluteUnixPath` — sixteen of seventeen rows cross; the one that drives
  the PawPrint wrapper joins `TestEmulatedKernelCurrentDirectory`.
* **28**: `TestFileSystemSeed` — eleven of thirteen rows cross; the two about
  `validateSeedForOracle` become `TestSeedForOracle`.

### Stage 21: the bind rules' fixture moves to the library

**Dependencies**: 13 (`bind` itself), 17 (`UnixSystem.initial`).

`TestSocketBinding` states `SimulatedUnixPlatform`'s bind rules directly —
`bindFaultOrder`, `firstBindFault`, `bindConflict`, `isBindableAddress`,
`bindAddressFaults`, `listenRescreensBinding` — and `UnixMachineState`'s ephemeral
port allocator. Every one of those is the library's, and has been since stage 13.

The move is mechanical: the fixture's only `WoofWare.PawPrint` uses are
`EmulatedKernel.initial.Machine` (four) and one `EmulatedKernel.mapMachine`. The
latter was never doing anything — it is a record update wrapped around the function
under test, and the exception the row asserts is `withEphemeralPortRange`'s own — so
it becomes a direct application, and the file names no PawPrint type at all.

`TestBind` is already in the library, testing the same syscall. They are not
duplicates: `TestBind` drives the entry point, and this states the rules underneath it
over their whole domain. The header now says so, because a reader who finds two bind
fixtures in one project needs to know which is which without reading both.

**Correctness oracle**: `verify21.py` re-derives the moved file from `origin/main` by
applying exactly the substitutions above, and compares whitespace-normalised (fantomas
reflows when `EmulatedKernel.initial.Machine` shortens to `initialSystem.Machine`).
The suite counts move exactly: library 1002 → 1016, PawPrint 3181 → 3167, fourteen
tests.
**The audit is closed.** Nothing from it remains.

### Stage 22: the clock pair moves, and the instruction cost stays

**Dependencies**: 17 (`UnixSystem.initial`).

`TestMonotonicTimestamp` (16 rows) and `TestSystemTimeAsTicks` (12) test
`UnixMachineState`'s two clock derivations. They move together because each names the
other: the monotonic module says the wall clock is the sibling's subject and asserts
the cross-entry-point agreement here, and both rest on the two being views of one
field. Moving one alone falsifies the other's docstring in a commit that does not fix
it.

Both had been asking for a `UnixMachineState` all along and reaching it through an
`EmulatedKernel` only because that is where `initial` lived. Every use was
`(kernelWith …).Machine` or `EmulatedKernel.initial.Machine`, so the helpers become
`machineWith` and `initialMachine` and the round trip disappears. `EmulatedKernel.mapMachine`
goes the same way it did in stage 21 — a record update wrapped around the function
under test, where every exception asserted is the writer's own.

**One row does not move.** `the instruction cost is configurable and validated` is
about `KernelConfig.applyTo`: that its default agrees with
`EmulatedKernel.defaultInstructionCostTicks`, that it reaches the field, and that it
rejects a cost below 1. The instruction cost is a charge of virtual time per retired
IL instruction, so it is PawPrint's in a way the clock derivations are not.

Leaving it behind in a fixture named `TestMonotonicTimestamp` would have made that
fixture's name a lie, so it goes to a new `TestKernelConfig`, whose subject is the
config layer itself rather than what any one field means. Stage 25 produces a second
orphan of exactly this kind (`KernelConfig applies the current directory whatever
else it sets`), which joins it there.

**The flavour the fixtures boot on.** `initialMachine` picks one arbitrarily, which is
only sound because both clocks boot at zero on either flavour. That was a claim in a
docstring, so `TestUnixSystemInitial` gains a row asserting it — mutation-verified:
booting `UnixSystem.initial` with a non-zero clock fails both its cases. Swapping the
two fixtures to `macOsArm64` and re-running leaves all 27 green, which is the same
claim measured from the other side.

**Correctness oracle**: `verify22.py` re-derives both moved files from `origin/main` by
applying exactly the stated substitutions, and separately asserts that the
instruction-cost row appears verbatim inside `TestKernelConfig`. Suite counts move
exactly: library 1016 → 1045, PawPrint 3167 → 3140 — 27 tests across the boundary,
one row staying on the PawPrint side, and two new cases in the library.

### Stage 23: the user-buffer host comparison moves, and takes `HostPlatform` with it

**Dependencies**: 17 (`UnixSystem.initial`).

`TestUserBufferCheckAgainstHost` measures how the *host* kernel screens a read buffer
— by bisecting for the address at which `read(2)` starts returning `EFAULT` — and
compares `UnixMachineState.userBufferCheck` against it. Both halves are the library's.

Its only `WoofWare.PawPrint` uses were `EmulatedKernel.initial` and two `mapMachine`s
around the setters under test, which collapse the way stages 21 and 22 collapsed
theirs. One docstring reference is corrected rather than carried: the field it names
as `EmulatedKernel.UserAddressLimit` has been `UnixMachineState`'s since stage 2.

#### `HostPlatform` moves with it

The fixture needs "which kernel is this test host, in the vocabulary the model uses",
which was `WoofWare.PawPrint.Test/HostPlatform.fs`. Three ways to give the library
test project that were considered.

**Duplicate a private `hostPlatform ()` into each moved fixture.** This is what
`WoofWare.PosixKernel.Test` already does *twice* — `TestVirtualFileSystemAgainstHost`
and `TestSockaddrLayoutAgainstHost` each carry their own — so it matches local
convention and needs no cross-project machinery. Rejected because those two copies are
three lines each, whereas this fixture also wants `onUnixHost`'s skip-or-run wrapper;
duplicating both into two fixtures is fifteen lines copied twice, and the copies would
be the definition of what a test measures against.

**Move it and leave PawPrint.Test its own copy.** Rejected outright: two divergent
answers to "what kernel is this" is precisely the drift the suites must not have.

**Move it, and have PawPrint.Test link the file.** Taken. `WoofWare.PawPrint.Test`
already links `socketMatrix\*.tsv` out of `WoofWare.PosixKernel.Test` for exactly this
reason, stated in that item's own comment: the two suites must not drift onto
different measurements. The same argument covers the flavour they measure *as*.

Five PawPrint-side fixtures consume it, of which three stay (`TestHarness`,
`TestOraclePolicy`, `TestPlatformSocketSupport` — the "is a differential comparison
meaningful here" use, which is PawPrint's). They gain an `open WoofWare.PosixKernel.Test`;
that is the whole blast radius. The type is compiled into both assemblies under that
namespace, which is harmless because neither references the other.

The two ad-hoc copies already in the library test project are *not* retargeted. They
`failwith` on an unmodelled host rather than skipping, deliberately — their fixtures
skip earlier, and the `failwith` is what says so. Unifying them would change a failure
mode in fixtures this stage otherwise does not touch.

**Correctness oracle**: `HostPlatform.fs` is a pure rename, and
`scripts/check-move-is-rename-only.sh` says so directly (`ok`). The fixture is checked
by `verify23.py`, which re-derives it from `origin/main` under exactly the stated
substitutions. Suite counts: library 1045 → 1048, PawPrint 3140 → 3137.

#### `TestFileSystemType` does not come along

The audit above called it a pure move, and it is not one. Twelve of its thirteen rows
are about `UnixMachineState`'s `fstatfs` table and the coherence between a mount and
the flavour that mounted it, which are the library's. The thirteenth, `this host's own
filesystem is one CoreCLR will lock`, is about neither the library nor its model: it is an
environmental premise for `sourcesPure/FlockContentionSeeded.cs`, which names
CoreCLR's `SafeFileHandle.CanLockTheFile` — if the suite ran with `/tmp` on a
filesystem CoreCLR will not lock, that guest's assertions would pass vacuously.

That row cannot travel, because the guest it protects is PawPrint's; it also cannot
stay in a fixture the rest of which has left. That is a relocation rather than a move,
so it gets its own stage rather than being smuggled into this one — stage 24 below.

### Stage 24: the filesystem-type fixture splits, twelve rows to one and one to the other

**Dependencies**: 17 (`UnixSystem.initial`), 23 (`HostPlatform` in the library test
project).

Twelve of `TestFileSystemType`'s thirteen rows are the library's: the magic-number
table `SystemNative_GetFileSystemType` answers from, the coherence rule between a
mount and the flavour that claims to have mounted it, and the host comparison that
manufactures a pipe, a socket and an anonymous inode on the real kernel and checks
the model against the real PAL. The four `EmulatedKernel.initial |>
EmulatedKernel.mapMachine` sites collapse the way stages 21 to 23 collapsed theirs.

The local binding keeps the name `kernel` even though its type is now
`UnixMachineState`. That is deliberate: this fixture's prose uses "kernel" for the
state object and "machine" for the thing being simulated, in the same sentence — *a
kernel carrying one of each would report a combination no machine could produce* — so
renaming it would collide with the other noun rather than clarify anything.

One comment is corrected rather than carried. It said `EmulatedKernel` is a public
record whose `{ x with UnixPlatform = ... }` bypasses the setter; that record has been
`UnixMachineState` since stage 2, and `EmulatedKernel` exposes the field as a
forwarding member which cannot be updated that way at all. It is also no longer in
scope, so this correction was not optional.

#### The thirteenth row is relocated, not moved

`this host's own filesystem is one CoreCLR will lock` is an environmental premise for
`sourcesPure/FlockContentionSeeded.cs` rather than a claim about the model. Three
places it could go:

**Leave it in a shrunken `TestFileSystemType` on the PawPrint side.** Cheapest, and
rejected for the reason stage 22 rejected the same shape: a fixture named for a
subject that has entirely left is a fixture whose name is a lie.

**Give it a new fixture** — `TestGuestEnvironmentPremises`, say. It names the category
exactly, and `TestOraclePolicy`'s `this host's shape is described by the presets` could
join it later. Rejected because that fixture would hold one row on the strength of a
second only hypothetically moving to it, which is the speculative half of the same
judgement stage 22 made in the other direction.

**Relocate it into `TestOraclePolicy`.** Taken, and on the merits rather than by
analogy: `FlockContentionSeeded.cs` lives in `sourcesPure`, where `runTest` refuses any
case not declaring `OraclePolicy.Always`, so it *is* a compared case, and this row is a
precondition for that comparison measuring anything. That is what `TestOraclePolicy` is
about, and it already carries `this host's shape is described by the presets`, whose own
comment makes the identical argument — "not a tautology restating the function: it says
that the machine running the suite is one the compared cases are actually compared on".
The fixture's docstring said two things were worth pinning; it now says three.

`hostGetFileSystemType` is *duplicated* rather than moved: the twelve rows that left
read it too, and a `private extern` cannot cross a project boundary.

**Correctness oracle**: `verify24.py` makes two claims, because the stage does two
things — that the moved file is exactly `origin/main`'s under `move24.py`, and that the
lifted row appears in `TestOraclePolicy` *verbatim*, so the relocation carried the row
rather than a retyping of it. Both halves were confirmed able to fail, by perturbing
each file in turn.

A duplicated `DllImport` in a second assembly is the one thing here that could bind
differently, and a row that no longer measured anything would stay green. Mutated to
fail on the *lockable* branch, it fails and reports this host's answer (`0x1A`, APFS),
so the measurement runs where it landed.

Suite counts: library 1053 → 1065, PawPrint 3137 → 3125. Twelve rows across the
boundary and one relocated within PawPrint, which is why the two do not sum.

### Stage 25: the current-directory setter becomes the library's, and grows a fault type

**Dependencies**: 17 (`UnixSystem.initial`).

`EmulatedKernel.withFileSystemAndCurrentDirectory` realises a seed as the guest's
filesystem and resolves the directory the simulated process starts in. Every one of
its dependencies was already the library's — `FileSystemSeed.toVirtualFileSystem`,
`VirtualFileSystem.resolveExisting`, `pathOfDirectory`, `SimulatedUnixPlatform.pathLimits`
— so nothing but the setter itself was holding it in PawPrint.

It is also the linchpin of what remains. Ten of `TestEmulatedKernelCurrentDirectory`'s
eleven rows reach it through one `seededAt` helper, and `TestEmulatedKernelInodeLifetime`
and `TestAbsoluteUnixPath` call it too, so stages 26 and 27 are blocked on it as well.
That is why it is worth settling the API rather than working around it.

It spans `Machine` (the filesystem) and `Process` (the current directory), so the
smallest library state it is about is `UnixSystem`. `EmulatedKernel.mapUnix` already
exists for exactly that shape — *"apply an operation that spans this kernel's whole
POSIX half"* — and `TestUnixSystemProjection` asserts the projection is total in both
directions, so the calling convention needed no invention.

#### What a failure says, and who says it

Every one of the five failures named PawPrint's config knobs: `EmulatedKernel.CurrentDirectory`
as the subject, `KernelConfig.FileSystem` and `KernelConfig.CurrentDirectory` as the
remedy. A library function cannot name `KernelConfig`, and one test row is named for
that property (`a current directory the seed does not contain names both knobs`).
Three ways to keep the remedy:

**Caller-supplied knob names**, threaded in as strings. This is the codebase's existing
idiom — `AbsoluteUnixPath.assertValid "EmulatedKernel.CurrentDirectory"`,
`VirtualFileSystem.assertInvariants "FileSystemSeed.toVirtualFileSystem"` — and leaves
every message and its test untouched. Not taken.

**Generic messages naming the function's own parameters.** Simplest signature, and
rejected: a PawPrint host would lose the sentence telling it which two fields to fix,
which is the reason those messages exist.

**The library states the fault; the caller states the remedy.** Taken. The library
returns `Result<_, CurrentDirectoryFault>`, and PawPrint's wrapper turns each case into
today's message — verified byte-identical to `origin/main`'s, modulo two interpolation
renames. A second client gets a value to match on rather than prose to regex, and the
library stops formatting advice about a config type it cannot see.

#### The fault type has three cases, not five

The obvious translation gives one case per `failwith`. Two of those five are not host
mistakes at all: "the walk answered an inode the filesystem does not contain" and "…a
directory it holds no path to" can only happen if `FileSystemSeed.toVirtualFileSystem`
produced a broken graph — and it asserts its own invariants before returning one. Their
old advice said so: *"Run VirtualFileSystem.checkInvariants"*, which is instruction to a
developer, not to a host.

So they crash in the library instead, alongside a third of the same kind that was
previously *lumped in with a host mistake*: a `Some (Symlink _)` from a walk run under
`SymlinkPolicy.Follow`, which never finishes on one. `chdir` says exactly this of the
same walk, so the treatment is now consistent between them.

That leaves three cases, each a mistake a host can fix, and each reachable through the
public API. `every fault case is reachable` asserts that by name against the union's own
case list rather than by count, so a fourth case added later fails there until something
produces it — mutation-verified by adding an unreachable case, which it names.

#### One case was named for the wrong limit

Review found that the `ENAMETOOLONG` arm claimed more than the walk knows. That errno has
two sources: a component past this flavour's `NAME_MAX`, and — on a flavour that
re-checks, which is Darwin alone — a symbolic link whose expansion carries the whole path
past `PATH_MAX`. Reporting the second as `NameTooLong` tells a host to shorten a
`CurrentDirectory` that has no overlong component, when what needs shortening is a
symlink target in its `FileSystem`. `origin/main` had the same defect in the message; the
type name would have entrenched it.

The library cannot separate them without `VirtualFileSystem.resolveExisting` reporting
which limit it hit, which every other caller of that walk would pay for — and a real
kernel conflates them too. So the case says only what is known: `TooLong of
SimulatedUnixFlavour`, documented with both sources, and PawPrint's message names both
remedies.

**Correctness oracle**: the PawPrint suite is a refactor except for one added row —
including `TestAbsoluteUnixPath`'s row pinning the `assertValid` context string, so the
wrapper still names its own knob before the library sees the value. Four of the five
messages are textually `origin/main`'s; the fifth is the one above.

`TestCurrentDirectoryFault` covers the three faults, the accepted control, and the
flavour-vs-argument distinction. Mutating the function to read
`system.Machine.UnixPlatform` instead of its argument fails exactly the row that names
that claim, and adding an unreachable case fails the reachability row by name.

The splice row is *not* the one that catches the review finding: the classification was
structurally identical before and after, so only the wording was wrong. The row that
catches it is PawPrint-side, asserting the message names `KernelConfig.FileSystem`, and
restoring the old wording kills it. Its seed is four hundred two-byte components rather
than one long one — the first attempt used a single 1100-byte target, which is an
overlong *component* and takes the `NAME_MAX` path without touching the splice at all.
The Linux half of the same row answers `DoesNotResolve`, which is what says the Darwin
half measures the re-check rather than a length.

#### A docstring was cut in half, and the oracle for that could not see it

Review also found that removing `currentDirectoryOf` left the first two paragraphs of
its docstring behind, fused onto `mapProcess` — so a note about resolving a directory and
about what `getcwd` owes was published as the contract of a function that takes no
directory. The paragraphs are now where they belong, on the library function.

`check-docstring-attachment.py` reported clean throughout, and that is not a bug in it so
much as an unlisted limit. Measured both ways: stranding the *whole* block fires MERGED
as designed, and stranding a prefix of it fires nothing, because blocks are keyed by
their whole text and a surviving prefix matches nothing at the base revision — it reads
as one block deleted and another added, neither of which is reportable. Removing a
definition is therefore the one edit where the remaining `///` lines have to be read by
hand, and the script's header now says so alongside its other stated limits.

#### Rebased onto the derived-path change

\#1255 landed while this was in review and deleted `UnixProcessState.CurrentDirectory`:
the process now holds only the inode, and `getcwd` derives the path from it. That is the
same judgement this stage made about `NoPathReachesIt`, arrived at independently — a
stored path and a stored inode are two copies of one fact, and the graph moves under the
copy.

So the setter is simpler than it was written: it records the inode alone, and its
`pathOfDirectory` call is now purely an assertion. \#1255 had already made that call an
assertion with a crash on failure, for the reason this stage crashes on it — *"the
alternative is a guest whose `getcwd` quietly reports ENOENT from its first
instruction"* — so the two agree and the arm is unchanged in substance.

#### Publishing it created a precondition it never had

Review's third finding, and the one that only exists because this is now a package's API.
In PawPrint the setter was called once, from `KernelConfig.applyTo`, on a kernel nothing
had opened anything on. As library API any client can call it at any moment — and it
replaces `Machine.FileSystem` while leaving `FileDescriptors` and `DirectoryStreams`
holding inode numbers of the graph that just went away. Measured before the guard
existed: open `/outer/file`, replace the filesystem, and `checkInvariants` reports
`DanglingOpenInode`. Worse than dangling is the case it *cannot* report — a stale handle
whose inode number the new graph has reissued to something unrelated.

So the operation states its precondition and crashes: this is boot-time, and the process
must hold no inode of the outgoing filesystem beyond its current directory, which is
precisely what is being replaced.

The guard counts **holders**, and two earlier drafts of it were wrong in instructive
ways.

The first wrote two guards, one for descriptions and one for streams, and left the
stream arm unreachable: `opendir` takes a descriptor too, so the description guard always
fired first. That is the same dead-arm defect this stage had just removed from the fault
type, reappearing thirty lines away.

The second asked `UnixProcessState.heldInodes` and subtracted `CurrentDirectoryInode`.
That reads well and is wrong, because `heldInodes` answers a **set of inode numbers** and
the exemption is about a **holder**. Standing at `/` and calling `opendir("/")` gives a
descriptor and a stream naming the very inode the current directory names, so subtracting
that value erased them and the guard saw an empty set. They would then have ridden into
the replacement filesystem and silently retargeted onto whatever the new graph gave the
root's number — which `checkInvariants` cannot see. `heldInodes` is not at fault: a set is
exactly right for the reaper's reachability question, and wrong for this one.

So both holder kinds are read here. The current directory is exempt because this
operation replaces it, not because its inode number is.

Four rows, each mutation-verified: subtracting the inode value again fails the
handle-onto-the-current-directory row; reading descriptions only fails the
descriptor-less-stream row; dropping the exemption altogether fails five rows including
the boot path PawPrint itself uses. The fourth row exists because the others would pass
against a guard that refused *every* system — a freshly booted process already holds
stdin, stdout and stderr, and those are not on the filesystem.

Library 1066 → 1077, PawPrint 3124 → 3125, re-measured at `f2a77fa0`.

**Deferred to stage 25b**: `TestEmulatedKernelCurrentDirectory` itself. Its rows split
three ways rather than two — the resolution behaviour to the library, the two rows that
assert PawPrint's *message* text staying with the wrapper that now formats it, and the
`KernelConfig` orphan joining `TestKernelConfig`. That is a fixture rewrite rather than a
move, and it does not belong in the same commit as an API change.

### Stage 25b: the current-directory fixture splits three ways

**Dependencies**: 25 (`UnixSystem.withFileSystemAndCurrentDirectory`).

Stage 25 deferred this because the rows do not split in two. Of eleven:

* **five cross to the library** — the resolution rows (`the held inode is the one the
  configured path names`, `a symlinked current directory is canonicalised`, `replacing the
  filesystem re-resolves the current directory`) and the two `checkInvariants` rows;
* **four stay**, because they assert the *messages*, which are the wrapper's own job now
  that the library answers a `CurrentDirectoryFault` saying nothing about `KernelConfig`;
* **one is rehomed within PawPrint**: `KernelConfig applies the current directory whatever
  else it sets` joins `TestKernelConfig`, the second orphan of the kind stage 22 created
  that fixture for. It is pasted verbatim rather than retyped.

#### Where the crossing rows land

The two `checkInvariants` rows go to `TestUnixSystemInvariants`, whose docstring already
said the rules tested from `WoofWare.PawPrint.Test` "move in their own stages" — this is
that stage for `CurrentDirectoryIsNotADirectory`. Its counts were **measured, and were
already stale**: it claimed five rules covered and twelve elsewhere, which summed to the
seventeen `UnixSystemDefect` cases that existed when it was written. #1255 deleted
`CurrentDirectoryPathDisagrees` without updating it, so sixteen cases had been five plus
eleven; after this stage they are six plus ten.

The three resolution rows go to the fixture stage 25 created, which is **renamed**
`TestCurrentDirectoryFault` → `TestWithFileSystemAndCurrentDirectory`. That name was
already drifting: the fixture held the accepted control, the platform-argument row and
three boot-time-guard rows, none of which is about the fault type. Naming it for the
function it tests makes room for these three rather than inventing a second fixture on one
function.

Three of the five crossing rows are renamed to their new home's vocabulary — a *kernel*
holding an inode becomes a *system* standing in one, and `checkInvariants rejects …`
becomes `… is a defect`, which is how every other row in that fixture reads.

#### One claim was split rather than moved

`the platform argument decides, not the one the kernel carries` stays, because it is about
the *wrapper* passing its argument down rather than reading `kernel.UnixPlatform`. But its
accepting direction was the only coverage of that direction anywhere, so the library row
gains it: boot Linux, pass Darwin, expect the wide name accepted **and**
`Machine.UnixPlatform` unchanged. Nothing else there would notice a function that
helpfully stored the platform it was handed.

**Correctness oracle**: the two suites' full test-name inventories, before and against
after. Every name is accounted for — 5382 distinct names on both sides, five crossing the
boundary, three of those renamed, none lost. Counted per suite with multiplicities rather
than as a set, since a set would let a vanished row hide behind a same-named row in
another fixture.

Mutation: each of the two moved invariant rows kills its own input and only its own
(a regular-file current directory, and an absent one), so they are not redundant; making
the setter stop following symlinks kills the moved canonicalisation row.

`check-docstring-attachment.py` earned its keep here. Pasting the `KernelConfig` row into
`TestKernelConfig` brought a helper and its docstring with it, and the original's users had
all left — F# does not warn on an unused private binding, the build was clean, and the
checker reported the duplicated prose. The dead helper is deleted.

Library 1077 → 1082, PawPrint 3125 → 3120.

### Stage 26: the inode-lifetime fixture becomes the library's, and dissolves

**Dependencies**: 25 (`UnixSystem.withFileSystemAndCurrentDirectory`).

`TestEmulatedKernelInodeLifetime`, 586 lines and nineteen rows, states when an inode stops
existing: `UnixProcessState.heldInodes`, `UnixSystem.pinnedInodes`, `forgetIfUnheld`, what
`close` does with them, and the cascade that frees an orphan's ancestors. Every one of
those is the library's. The fixture held an `EmulatedKernel` only as a container, reaching
into it with `unix`/`mapUnix` at almost every assertion, so the move is mostly the removal
of that round trip.

Eighteen rows cross. The nineteenth — `directoryStream refuses a block that names no
stream` — is about PawPrint's `DIR*`-to-stream binding, so it joins its sibling in
`TestDirectoryStreamId` and the fixture disappears entirely.

That sibling is `a DIR* this kernel never issued is refused loudly`, and the two are not
duplicates: one asks `directoryStreamId` about a block never bound, the other asks
`directoryStream` about one bound and then released. The moved row is renamed to say so,
since landing beside its sibling is what makes the distinction need stating.

#### Two rows that looked PawPrint's are not

`an open stream holds its directory even with its descriptor gone` and `a stream keeps an
rmdir'd directory alive, and closing it reaps` reached their subject through PawPrint's
`DIR*` block, but the rules they state — that `heldInodes` counts a stream, and that a
stream keeps an orphan alive until it goes — are the library's. Library-side they use the
`DirectoryStreamId` that `opendir` answers, and are shorter for it.

Forgetting a stream had to be written out rather than called: **the library has no
`closedir`.** `UnixSystem.opendir` mints a stream and nothing in the library removes one;
PawPrint's `withoutDirectoryStream` is the only operation that does. That is a real gap
rather than a consequence of this stage, and closing it is its own change.

#### One row was cut and then put back

`checkInvariants rejects a descriptor naming an inode the filesystem has forgotten` looked
like another rule for `TestUnixSystemInvariants`, which stage 25b fed. It is not: its own
comment says it brackets the reaping rule with `UnreachableFromRoot` — *"a `forget` that
fires too late is caught there and one that fires too early is caught here"* — which makes
it an inode-lifetime claim stated through the checker rather than an entry in the
checker's catalogue. It also uses this fixture's `unbound` and `opened`, which the other
would have had to grow copies of. So it stays, and 25b's count of six covered rules is
unchanged.

#### What the audit found before any code moved

Two of the nineteen looked like duplicates of `TestUnixSystemStep`'s direct `forgetIfUnheld`
rows, which reach the same rule through `unlink`. Neither is: the moving rows additionally
assert that the result is *sound* — `VirtualFileSystem.checkInvariants` against
`pinnedInodes` comes back clean — and that the pin is what excuses the unreachability,
`checkInvariants Set.empty` reporting `UnreachableFromRoot` for exactly the inode held.
The existing rows make neither claim. Both are kept, and land beside their counterparts.

**Correctness oracle**: `move26.py` states every substitution and asserts its own
occurrence count, so a rule that silently failed to apply is an error. Its counts are
*measured*: the first draft guessed them and was wrong in five places, including
`kernel.FileSystem` (twelve, not six). Where a count would have described an intermediate
state — the field accessors, which earlier structural rules also rewrite — the rule is
applied wholesale and the end-state assertions carry the weight instead: no `EmulatedKernel`
and no `KernelSyscall` may remain.

Beyond that, the two suites' full test-name inventories: 8086 rows before and after, 18
crossing, exactly one renamed, none lost.

Mutation, on the library rather than on the moved text: removing the directory-stream union
from `heldInodes` fails `an open stream holds its directory even with its descriptor gone`
and nothing else; stopping `pinnedInodes` climbing fails the two pinning rows and the
orphan-cascade row.

`check-docstring-attachment.py` reports the module rename as MOVED, which is what it is —
the fixture's own docstring now precedes `TestInodeLifetime`. Its every claim was checked
against the new home and still holds, including the `sourcesImpure/UnlinkReapSeeded.cs` it
names, which is still PawPrint's and still the only other check on any of this.

Library 1082 → 1100, PawPrint 3120 → 3102.

### Stage 27: the absolute-path fixture becomes the library's

**Dependencies**: none beyond 25, whose wrapper the one staying row drives.

`AbsoluteUnixPath` is `WoofWare.PosixKernel`'s own type, and sixteen of
`TestAbsoluteUnixPath`'s seventeen rows are about parsing, rendering and rejecting one.
They move as a file; the fixture keeps its name because the library has none.

The seventeenth, `The kernel rejects a forged current directory at configuration time`,
drives `EmulatedKernel.withFileSystemAndCurrentDirectory` and asserts the message names
`EmulatedKernel.CurrentDirectory`. That is the wrapper's job, so it joins
`TestEmulatedKernelCurrentDirectory`, whose subject stage 25b narrowed to exactly that.
It adopts that fixture's own `message` helper rather than bringing an `open System` with
it for `Assert.Throws<Exception>`.

#### The audit had this stage three rows too large

It said "split — 4 cases forge an `EmulatedKernel`". One does. The same error appears in
stage 28's line ("8 cases call `RealRuntime.validateSeedForOracle`" — two rows do, calling
it eight times) and in the stage-23 text that stage 24 corrected. All three counted *call
sites* and reported them as *rows*, which is why the audit has consistently overstated how
much stays behind.

#### The label two rows pass

`assertValid rejects the forged default value` and `parseOrFail names the offending knob`
pass a string naming the caller's boundary, and assert the failure echoes it. That string
was `KernelConfig.CurrentDirectory`.

Keeping it would have introduced a PawPrint type name into library code — which is a
different thing from the PawPrint names already in library *prose*, left deliberately as
markers for text not yet reviewed. `AbsoluteUnixPath.assertValid`'s own docstring
describes its callers generically rather than naming any, so the library's convention here
is already to avoid them.

Rather than invent a label, the rows now name a real library caller of the function they
test: `UnixSystem.withFileSystemAndCurrentDirectory`, which stage 25 gave that exact
`assertValid` call. The rows are truer than before — the string is now one this repository
can check exists.

**Correctness oracle**: the two suites' full test-name inventories are *identical* — 8086
rows before and after, nothing vanished, nothing new, sixteen crossing. This is the first
stage where no row needed renaming.

`scripts/check-move-is-rename-only.sh` prints residue that is exactly the label retarget
plus the cut. Mutation: dropping `context` from `AbsoluteUnixPath.parseOrFail`'s message
fails `parseOrFail names the offending knob` and nothing else, so the retargeted label is
load-bearing rather than decorative.

Library 1100 → 1116, PawPrint 3102 → 3086.

### Stage 28: the seed fixture becomes the library's, and the oracle's rule gets its own

**Dependencies**: none.

`FileSystemSeed` is the library's, and eleven of `TestFileSystemSeed`'s thirteen rows say
what realising a seed produces: the tree it describes, the parents, the creation instant,
the modes, and — by generator — that every declared path resolves to what was declared and
that realising is deterministic down to the inode numbers. Two more, about
`SimulatedUnixPlatform`'s release strings and what `stat` can ask of a flavour, are the
library's for the same reason.

The two that stay are `RealRuntime.validateSeedForOracle`'s: which seeds a *differential*
case may declare. A seed is the library's idea of a filesystem and the interpreted guest
gets exactly the one it describes, but the real runtime does not — the oracle materialises
the seed as a real directory on the host. A seed naming something a real directory cannot
hold, or carrying a mode a host `chmod` may silently drop, would leave the two runtimes
answering questions about different filesystems while the comparison still ran and still
looked like evidence.

#### Where the two go

**A shrunken `TestFileSystemSeed` on the PawPrint side.** Rejected for the reason stage 22
and stage 24 rejected it: the fixture would be named for a type that had entirely left.

**`TestRealRuntimeOracle`**, whose docstring is "tests for the differential oracle itself".
Broad enough on its face, but that docstring is a tight argument about `RunMain`'s
process-termination semantics, transcribed from the pinned runtime's `assembly.cpp`.
Widening it to cover seed shape would dilute a good piece of prose. Rejected.

**A new `TestSeedForOracle`.** Taken. Two rows with one subject is a fixture — the same
judgement stage 22 made creating `TestKernelConfig`, which began with one. Its docstring
states the division: `TestFileSystemSeed` says what a seed *is*, and these say which of
those a comparison may use.

#### A sentence that had never been true

The mode row's docstring opens *"Split out from the shape-validation test above"*. That
test is below it, and has been since `e36fa1c4` wrote the sentence — checked against the
file at that commit rather than assumed. This stage has to choose an order for the two
rows anyway, so putting the shape row first makes the sentence true at no cost. Not a
sweep: the prose is untouched.

**Correctness oracle**: the two suites' full test-name inventories are identical again —
8086 rows before and after, nothing vanished, nothing new, eleven crossing.

Mutation, one per side of the split: disabling the special-bit refusal in
`RealRuntime.validateSeedForOracle` fails `the oracle refuses mode bits a host chmod may
drop` in its new fixture, so the row still binds to the rule; realising every seed at the
epoch instead of the moment given fails `every seeded inode is created at the moment the
seed was realised` and nothing else.

`TestNoPawPrintReference` still passes, which is the standing check that none of this drags
a dependency across.

Library 1116 → 1127, PawPrint 3086 → 3075.

### The planned relocations are done

Stages 21 to 28 have moved every fixture the audit named. What remains is recorded above
and is not fixture relocation: the two ad-hoc `hostPlatform ()` copies that `HostPlatform`
should replace (stage 23, and #1253 has since added a third call site), and the library's
missing `closedir` (stage 26).
