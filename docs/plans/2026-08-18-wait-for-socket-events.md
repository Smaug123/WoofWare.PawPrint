# `SystemNative_WaitForSocketEvents`

Follow-on from #1047, which added `ThreadStatus.BlockedOnSocketEvents of port : OpenFileDescriptionId`
as representation only. This document plans the handler that constructs it.

## The finding that drives everything below

**`SystemNative_WaitForSocketEvents` cannot be implemented on its own.** Its body is
"screen the arguments, then consult the epoll instance the descriptor names". PawPrint has
no epoll instance: `OpenFileTarget` has exactly two cases (`StandardStream`, `File`), so no
descriptor in the registry can *be* a port, and `#1047` says so explicitly in the variant's
own docstring.

So every path through a handler written today either returns an error or crashes. The
blocking arm — the entire reason the wait reason exists — is unreachable, and a parked test
for it would be vacuous in exactly the way `parked-tests-must-not-be-vacuous` warns about.

That makes this a scoping decision before it is an implementation, and AGENTS.md's rule
("consider whether we can implement the dependency *first* … either way, stop and ask")
applies. The options are in §4.

## 0a. Review status

Reviewed by Fable against the pinned upstream source. It confirmed the central finding
(§"The finding"), found no fifth *structural* option, and endorsed Option B. Its substantive
corrections are folded in above and below: the `access_ok`-is-not-mappedness point (§2, row
7), Darwin's ordering putting row 6 behind a valid port (§2, §4 Option C), the undercounted
`WhatWeDid` blast radius (§2 Route 1), the `EP_MAX_EVENTS` row, the `SIGSEGV`-not-`EFAULT`
point about a garbage `count`, and the background-thread differential test (§6).

One of its corrections I have *not* taken, because measurement contradicts it: it predicted
`epoll_wait(badfd, evs, maxevents=0, …)` would answer `EINVAL` on Linux, reading the same
stale `do_epoll_wait` listing I had. Measured on 6.18.5 it is `EBADF`. See §1.

Two of its corrections were already fixed before it reported, by measuring rather than
asserting: `lseek` on an epoll fd succeeds returning 0 (it correctly predicted my source-free
guess of `ESPIPE` was wrong), and the `read`/`fstat` rows in §3.

## 0. Provenance of the measurements

Every row below marked "measured" was taken from a C probe run against a real kernel, not
read off a source tree:

- **Linux 6.18.5 aarch64**, via the `container` CLI (`docker.io/library/gcc:13`).
- **Darwin 25.6.0 arm64**, natively.

Neither is the exact target PawPrint claims to simulate (`LinuxX64`), so per
`kernel-source-version-vs-measured-host` the *rule shape* is what these establish; any
constant that could plausibly be architecture-dependent is called out where it appears. The
architecture-sensitive one here is `sizeof(struct epoll_event)`, handled in §1.

## 1. What upstream actually does

Managed extern (`Common/src/Interop/Unix/System.Native/Interop.SocketEvent.cs:48`):

```csharp
[LibraryImport(Libraries.SystemNative, EntryPoint = "SystemNative_WaitForSocketEvents")]
internal static unsafe partial Error WaitForSocketEvents(IntPtr port, SocketEvent* buffer, int* count);
```

Returns a PAL `Interop.Error`, not `-1`-and-errno — so unlike `SystemNative_Read` this
handler never touches `Kernel.LastSystemError`. `UnixError.palOfRawErrno` and the
`PalErrorReturn` active pattern in `NativeSystemNative.fs` already exist for this shape.

`SocketEvent` is `{ IntPtr Data; SocketEvents Events; int _padding; }` — 16 bytes on a
64-bit target. `SocketEventBufferElementSize` is `max(sizeof(struct epoll_event), sizeof(SocketEvent))`
under epoll and `sizeof(struct kevent)` under kqueue, and `WaitForSocketEventsInner` converts
in place down to a stride-16 `SocketEvent` array before returning, so the managed caller only
ever sees stride 16.

Measured: `sizeof(struct kevent)` is 32 on Darwin arm64, and `sizeof(struct epoll_event)` is
16 on Linux aarch64. On Linux **x86-64** — the flavour PawPrint defaults to — it is 12
instead, because `linux/eventpoll.h` defines `EPOLL_PACKED` as `__attribute__((packed))`
under `#ifdef __x86_64__` and empty otherwise, over `{ __poll_t events; __u64 data; }`
(read from the header, not recalled). Either way `max(12, 16) = max(16, 16) = 16`, so the
element size is 16 under epoll on both architectures and 32 under kqueue. This only matters
to `SystemNative_CreateSocketEventBuffer`, which is not in scope here.

Wrapper (`pal_networking.c:3492`):

```c
if (buffer == NULL || count == NULL || *count < 0) return Error_EFAULT;
int fd = ToFileDescriptor(port);
return WaitForSocketEventsInner(fd, buffer, count);
```

`ToFileDescriptor` is `assert(0 <= fd && fd < sysconf(_SC_OPEN_MAX)); return (int)fd;` — the
assert is compiled out of the shipped release build, so an out-of-range `port` truncates
rather than trapping. Same fact `SystemNative_Read`'s existing comment records.

### epoll (`pal_networking.c:3175`)

`while ((numEvents = epoll_wait(port, events, *count, -1)) < 0 && errno == EINTR);`

Infinite timeout, `EINTR` swallowed in the loop — so **signal delivery must not wake a
thread in this status**, which is what `ThreadStatus.BlockedOnSocketEvents`'s docstring
already promises. On failure `*count = 0` and the PAL error is returned.

The kernel's own ordering is worth pinning, because the wrapper's EFAULT screen does *not*
subsume it. **Measured, not read off the source** — the commonly-cited `do_epoll_wait`
ordering (maxevents, then `access_ok`, then `fdget`) is stale; current kernels look up the
descriptor first. On Linux 6.18.5 the order is:

| # | Check | Answer |
| --- | --- | --- |
| 1 | `epfd` not open | `EBADF` |
| 2 | `maxevents <= 0` (or `> EP_MAX_EVENTS`) | `EINVAL` |
| 3 | `events` buffer not writable | `EFAULT` |
| 4 | `epfd` open but not an epoll instance | `EINVAL` |

Each adjacent pair was disambiguated by an input that provokes exactly one of them
(`ordered-guards-need-a-disagreeing-input`): `epoll_wait(badfd, evs, 0, …)` → EBADF pins 1
before 2; `epoll_wait(ep, (void*)-1, 0, …)` → EINVAL pins 2 before 3; and
`epoll_wait(devnull, (void*)-1, 4, …)` → EFAULT pins 3 before 4.

Row 1 is the one that most invites being got wrong from reading: the widely-reproduced
`do_epoll_wait` listing checks `maxevents` and `access_ok` *before* `fdget`, which would make
`epoll_wait(badfd, evs, 0, …)` answer `EINVAL`. Measured on 6.18.5 it answers `EBADF`. The
kernel was restructured; the old listing is stale. Do not restore the source-derived order.

Note the interaction: the wrapper rejects `*count < 0` as EFAULT, so `*count == 0` is the
only value that reaches `epoll_wait` and comes back EINVAL. That is a reachable, testable row
*today*, with no port needing to exist.

### kqueue (`pal_networking.c:3348`)

`while ((numEvents = kevent(port, NULL, 0, events, GetKeventNchanges(*count), NULL)) < 0 && errno == EINTR);`

Trailing `NULL` is the `struct timespec *timeout`: also infinite. Three divergences from
epoll that a flavour-aware handler must carry, and which `SimulatedUnixPlatform.macOsArm64`
makes live rather than hypothetical:

1. **The error sentinel differs.** epoll writes `*count = 0`; kqueue writes `*count = -1`.
   (Read from the source; both are plainly visible and neither is conditional.)
2. **`*count == 0` does not block under kqueue.** Measured on Darwin 25.6.0:
   `kevent(kq, NULL, 0, evs, 0, NULL)` returns `0` immediately rather than blocking.
   `numEvents == 0` then trips only a debug `assert`, so the shipped release build falls
   through, writes `*count = 0`, and returns `Error_SUCCESS`. Under epoll the same input is
   `EINVAL`. This is the one input on which the two flavours disagree about whether the call
   blocks at all, and it is reachable without a port existing.
3. **A live descriptor that is not a port is `EBADF`, not `EINVAL`.** Measured:
   `kevent(open("/dev/null"), …)` is `EBADF` on Darwin, where `epoll_wait` on the same
   descriptor is `EINVAL` on Linux. kqueue has no "wrong kind of object" answer — it folds
   that into `EBADF`.

Darwin's ordering is correspondingly flatter: `kevent(kq, (void*)-1, nevents=0, NULL)`
returns `0`, so there is no buffer screen to order at all when `nevents` is zero.

Both flavours carry the same verbatim comment that a zero-event *success* is impossible with
an infinite timeout: the wait blocks until a descriptor is registered **and** an event occurs
on it. So a registration performed by another thread is itself a wake edge, not only data
arrival. That is a constraint on the eventual wake path, not on this change.

## 2. The handler, given a port exists

The contract PawPrint must reproduce is the *entry point's*, not the syscall's, because
PawPrint reimplements the whole shim. Composing the wrapper's screen with the measured
syscall orderings above gives this table. Rows marked ‡ differ between flavours.

| # | Input | Linux x64 | Darwin arm64 |
| --- | --- | --- | --- |
| 1 | `buffer == NULL` | `EFAULT`, `*count` untouched | same |
| 2 | `count == NULL` | `EFAULT` | same |
| 3 | `*count < 0` | `EFAULT`, `*count` untouched | same |
| 4 | descriptor not open | `EBADF`, `*count = 0` ‡ | `EBADF`, `*count = -1` ‡ |
| 5 | descriptor open, not a port | `EINVAL`, `*count = 0` ‡ | `EBADF`, `*count = -1` ‡ |
| 6 | valid port, `*count == 0` | `EINVAL`, `*count = 0` ‡ | `SUCCESS`, `*count = 0` ‡ |
| 7 | valid port, `*count > 0`, buffer range above `UserAddressLimit` | `EFAULT`, `*count = 0` ‡ | blocks ‡ |
| 8 | valid port, `*count > 0`, ready set empty | **blocks** | **blocks** |

Five of eight rows diverge by flavour, so the handler is flavour-branching throughout rather
than in one spot — the same shape `NativeSystemNative`'s existing handlers already have.

Three things about this table are easy to get wrong.

- **Row 7 is `access_ok`, not a mappedness check.** On 64-bit Linux `access_ok` only rejects
  ranges that reach into the kernel half; a *userspace* address that happens to be unmapped
  passes it, and `epoll_wait` then **blocks**, faulting only at delivery. So the handler must
  not eagerly validate that the buffer is real before parking. PawPrint already models
  exactly this and needs no new machinery: `UserBufferCheck.BeforeOperation` is a range test
  against `EmulatedKernel.UserAddressLimit`, and Darwin's `UserBufferCheck.AtCopyTime`
  returns `false` unconditionally — which is why row 7's Darwin cell is "blocks".
  Measured both ways: `epoll_wait(ep, (void*)-1, 4, 0)` is `EFAULT` while
  `epoll_wait(ep, NULL, 4, 0)` returns `0`.
- **Rows 4–6 are ordered differently per flavour, not merely valued differently.** On Linux
  the descriptor is resolved first, then `maxevents`, then the buffer, then is-it-epoll (§1).
  On Darwin `kevent` resolves the descriptor before its `nevents == 0` early return, so
  Darwin's row 6 is reachable *only with a valid port* — measured:
  `kevent(kq, …, nevents=0)` returns `0`, but `kevent(open("/dev/null"), …, nevents=0)` is
  `EBADF`. This is why row 6 sits below rows 4–5 rather than above them, and it matters for
  Option C (§4).
- **Row 3 is the wrapper's own deref.** A non-null but *garbage* `count` is dereferenced in
  the user-space PAL shim, so on a real system that is a `SIGSEGV`, not an errno. PawPrint's
  guest-memory read should fail loudly there rather than manufacture `EFAULT`.

Row 8 is the feature. Everything above it is reachable today; row 8 is not, because no
descriptor can be a port (§3). `*count > EP_MAX_EVENTS` (`INT_MAX / sizeof(struct epoll_event)`,
~1.79e8) is a further Linux `EINVAL` row, unreachable from the BCL but reachable from a
hand-rolled guest.

Parking is re-entrant, per #1047's decision (`parksPastTheBlockingCall = false`): leave the
native frame, do not advance the PC, and let the dispatcher re-enter the handler when the
port becomes ready. There are two routes to that, and **measurement picked the second**.

### Route 1 — mirror the `BlockedOnClassInit` pair

Add `NativeHandlerResult.BlockedOnSocketEvents of IlMachineState * port * StepEffect` *and*
`WhatWeDid.BlockedOnSocketEvents of port`, with `Scheduler.onStepOutcome` performing the
status transition exactly as it does at `Scheduler.fs:880`.

I estimated this at ~5 compile-forced sites. **Measured by adding the case and building: 11
in the library** — and that is itself an undercount, because the build failed in
`WoofWare.PawPrint` and so never reached `WoofWare.PawPrint.App`, which contributes at least
`DebuggerServer.fs:432`. Call it 12.

| Site | What it is |
| --- | --- |
| `Scheduler.fs:691` (`onWorkerSpawned`), `:844` (`onStepOutcome`) | the two that genuinely want an arm |
| `Program.fs:390`, `:1086`, `:1383` | driver logging / step classification |
| `IlMachineStateModel.fs:650` | the `WhatWeDid` → `NativeHandlerResult` forwarder |
| `NativeCustomAttribute.fs:475`, `NativeReflectionInvocation.fs:483`, `NativeRuntimeHelpers.fs:65`, `NativeRuntimeTypeQCall.fs:593`, `:1879` | five native handlers translating a *managed sub-call's* outcome |
| `App/DebuggerServer.fs:432` | not reached by the failing build; found by grep |

Those last five are the problem. Each asks "my managed sub-call reported X; what do I do?",
and for this variant the answer is "unreachable by construction — a managed sub-call cannot
report a socket wait". Five untestable arms, of exactly the kind
`check-predicate-conjuncts-are-falsifiable` says to prove dead at design time rather than
discover by mutation.

### Route 2 — the handler parks itself, as `Thread.Sleep` already does

Add only `NativeHandlerResult.BlockedRetainingFrame of IlMachineState * StepEffect`: the
dispatcher leaves the frame on the stack and reports `WhatWeDid.Executed`. The handler calls
a `Scheduler.blockOnSocketEvents` helper itself, alongside `blockOnJoin` and `blockOnSleep`.

Compile-forced blast radius: **1 site** (`AbstractMachine.dispatchNative`).

This is not a novel route: `ThreadNative_Sleep` already calls `Scheduler.blockOnSleep` and
then returns `NativeHandlerResult.completed`, so a thread reporting `WhatWeDid.Executed`
while already `Blocked` is established behaviour. Reporting `Executed` is also truthful — a
step *was* retired; the handler ran and decided to block. The only behavioural consequence is
that `onStepOutcome` runs `wakeClassInitWaiters`, which is correct for any retired step.

### Choice: Route 2

The asymmetry that decides it: `BlockedOnClassInit` *has* to travel by `WhatWeDid` because it
has many producers — IL ops raise it (`UnaryMetadataFieldOps.fs`, three sites) as well as
native handlers, so the transition must live somewhere all of them funnel through.
`BlockedOnSocketEvents` will have exactly one producer, ever. Paying an 11-site,
five-dead-arm tax to route a single-producer transition through a shared channel buys nothing.

Reversibility is good: if a second producer ever appears, promoting Route 2 to Route 1 is a
mechanical change the compiler drives.

The wake *mechanism* is not foreclosed by any of this, and the place it will go already
exists: `Program.advanceToDecision` runs `SignalDispatch.trySpawnHandler` as a preamble
before the scheduler picks a thread, precisely so a kernel-side event source can flip a
parked thread to Runnable between IL steps. A socket-readiness poll is the same shape and
the same call site. Waking by edge from the natives that mutate readiness is equally open.

**Nothing wakes the thread in this change.** That is faithful rather than a stub: with no
registration native, no descriptor is ever registered with the port, and upstream's own
comment says the wait blocks forever in exactly that situation. The hazard to guard against
is the `half-implementing-a-guarded-pair` shape — so `SystemNative_TryChangeSocketEventRegistration`
must stay *unimplemented and loud* in this change, and the plan must not add a wake function
that only flips the status (which would be wrong, since a real wake must also deliver the
event batch).

## 2a. What the only real caller does

Dumped from the shipped `System.Net.Sockets.dll` in the pinned linux-x64 framework pack
(`SocketAsyncEngine::EventLoop`), since that assembly is not in the sparse runtime checkout:

```
IL_0008: ldc.i4 1024          // count = 1024
IL_000E: ldarg.0; ldfld _port
IL_0016: call SocketEventHandler::get_Buffer
IL_001D: conv.u               // &count
IL_001E: call Interop.Sys::WaitForSocketEvents
IL_0025: brfalse.s +12        // Error.SUCCESS == 0 -> continue
IL_002D: newobj System.Net.InternalException; throw
IL_0036: call SocketEventHandler::HandleSocketEvents
IL_003B: brfalse.s -53        // -> IL_0008
IL_0043: br.s -61             // -> IL_0008
```

Three things follow.

- **`*count` is always 1024 on the real path**, reloaded each iteration. So the `*count == 0`
  flavour divergence is *not* on Kestrel's path; it is reachable only from a hand-rolled
  guest, which is where its test belongs.
- **Any non-`SUCCESS` return kills the process.** The engine throws `InternalException`, and
  the enclosing catch calls `Environment.FailFast`. So a wrong error row here is loud rather
  than silently divergent — welcome, and it means the error rows are worth getting exactly
  right before anything depends on them.
- **The loop never exits.** Confirms that "park forever, with no registration ever performed"
  is a faithful model of this thread rather than a stub: upstream's own comment says the wait
  blocks until a descriptor is registered *and* an event occurs on it.

## 3. What the dependency costs

The dependency is `OpenFileTarget.SocketEventPort` plus `SystemNative_CreateSocketEventPort`
and `SystemNative_CloseSocketEventPort`.

- `CreateSocketEventPort(intptr_t* port)` is `epoll_create1(EPOLL_CLOEXEC)` → allocate the
  lowest free fd naming a fresh description whose target is a port. `EFAULT` for a null
  out-pointer. `FileDescriptorRegistry` already has the allocation machinery.
- `CloseSocketEventPort(intptr_t port)` is literally `close(2)`, and `SystemNative_Close` is
  already target-agnostic (it removes the fd from the table and never inspects the target),
  so the registry work is done. Only the return convention differs: this entry point returns
  a PAL `Error` rather than `-1`-and-errno.

  One semantic question it raises, which belongs to whichever PR owns create/close: closing
  the last descriptor naming a port while another thread is parked
  `BlockedOnSocketEvents` on that description. On a real kernel the waiter holds a reference
  to the open file, so the close succeeds and the waiter is **not** woken — it blocks
  forever. PawPrint would have to either reproduce that (a description with no descriptors
  but a live waiter — which the registry's `UnreferencedDescription` invariant check
  currently treats as corruption) or fail loudly. Not a question this change has to answer,
  but it must not be answered accidentally.
- `OpenFileTarget` gains a case, which forces an answer at ~12 exhaustive match sites
  (`Read`, `PRead`, `PWrite`, `Write`, `LSeek`, `FStat`, `FLock`, `Dup`, `IsATty`, plus
  `OpenFileDescription.object` and the registry's own invariant checks).

That last bullet is the real cost, and I have now measured it rather than guessing. On
Linux 6.18.5 and Darwin 25.6.0, a port descriptor answers the ordinary file operations like
this — **every single row diverges**:

| operation on a port fd | Linux | Darwin |
| --- | --- | --- |
| `read` | `EINVAL` | `ENXIO` |
| `write` | `EINVAL` | `ENXIO` |
| `lseek` | succeeds, returns `0` for any whence/offset | `ESPIPE` |
| `flock(LOCK_EX)` | succeeds | `EOPNOTSUPP` |
| `ftruncate` | `EINVAL` | (not probed) |
| `isatty` | `0` | `0` |
| `dup` | succeeds | succeeds |
| `fstat` `st_mode` | `0600` — permission bits, **no file-type bits at all** | `010000` — `S_IFIFO`, **no permission bits** |
| `fstat` `st_blksize` | `4096` | `32` |
| `fstat` `st_nlink` / `st_dev` / `st_ino` | `1` / real dev / real anon-inode number | `0` / `0` / `0` |

Two consequences the plan has to own:

- This is not a rider on the wait. Ten divergent rows is a PR's worth of work on its own,
  and `fstat` in particular is not just an errno: PawPrint's `SystemNative_FStat` is
  inode-driven, and a port has no inode, so serving it means deciding what a *typeless*
  (Linux) or *type-only* (Darwin) stat buffer is in this model.
- `flock` succeeding on Linux forces a decision `OpenFileDescription.object` currently has no
  answer for. `flock` contention is decided by `OpenFileObject` equality, so two distinct
  `epoll_create1` instances must compare unequal while a `dup`'d pair compares equal. That
  needs a per-instance identity token in `OpenFileObject`, and reusing the creating
  `OpenFileDescriptionId` for it is a pun (an *identity* borrowed from a *description*) that
  wants deciding explicitly rather than by default.

The cheaper alternative is a precise `failwith` at each non-wait site ("read(2) on an epoll
port descriptor is not modelled"), defensible under
`prefer-crashing-over-documented-divergence`, and which no realistic guest hits —
`SocketAsyncEngine` never reads, seeks, stats or locks its epoll fd. But it is a decision to
take deliberately, not to drift into, and it trades away rows I have already measured.

## 4. The options

### Option A — one PR: port lifecycle + wait

`OpenFileTarget.SocketEventPort`, `CreateSocketEventPort`, `CloseSocketEventPort`,
`WaitForSocketEvents`, the two new dispatcher/scheduler cases. Non-port fd operations get
measured errno rows.

- Reachable end to end: a guest creates a port and waits; PawPrint reports `Deadlocked` with
  the thread located inside `WaitForSocketEvents`. A two-thread guest shows the waiter parked
  while the other thread runs to completion — which is precisely the Kestrel engine-thread
  shape, and the reason this is worth having before sockets exist.
- Cost: one PR doing two things, and the reviewer has to hold ten flavour-divergent
  fd-semantics rows, a new `fstat` shape, an `OpenFileObject` identity decision, and the
  scheduler plumbing in mind at once.

### Option B — two PRs: dependency first

PR 1: `OpenFileTarget.SocketEventPort` + create/close + the fd-semantics rows.
PR 2: `WaitForSocketEvents` + the scheduler plumbing.

- Matches AGENTS.md's stated preference exactly. PR 1 is independently testable (fresh fd
  numbering, `dup` shares the port, `close` frees it, each non-port operation's errno);
  PR 2 is then purely about the wait and the parking.
- Cost: PR 2 is stacked on PR 1, and `stacked-prs-get-no-ci` applies.

### Option C — one PR: wait only, no port

Implement only the argument-screening and no-such-port arms (rows 1–5 of §2), reachable today
through a hand-rolled guest `DllImport` and differentially testable against the real runtime.
The park arm `failwith`s "no epoll port kind exists".

- Smallest diff, and rows 4–5 carry a genuine flavour divergence (Linux `EINVAL` vs Darwin
  `EBADF` for a live non-port descriptor).
- **Weaker than it first looked.** I originally justified C partly on row 6's `*count == 0`
  split being reachable without a port. It is not: Darwin resolves the descriptor before the
  `nevents == 0` early return, so its `SUCCESS` arm needs a real kqueue. Without a port, C
  reaches one divergent row, not two.
- And it does not implement the feature. The thing #1047 exists for is still unreachable, and
  the PR title would overclaim.

### Option D — go wider: registration and wake in the same change

Add `TryChangeSocketEventRegistration` too, so the wake path is exercisable by registering
the standard-stream pipe descriptors and observing write-readiness.

- Only option in which `BlockedOnSocketEvents` is ever left.
- But it bakes in a readiness model (push ready-list vs pull recompute; edge-triggered
  bookkeeping) chosen against pipes when the actual consumer is sockets, and readiness for
  PawPrint's standard streams is a policy invention with no guest asking for it. Against the
  stated goal of a real network stack, this is the option most likely to be wrong.

### Option E — not yet: build sockets first, then the wait

Decline to implement the wait now, on the grounds that its only content is "consult the ready
set", and the ready set has no producers until sockets exist. Implement
`SystemNative_Socket`/`Bind`/`Listen`/`Accept` and the connection model first, then write the
wait against a readiness model that has a real consumer.

- The honest version of the objection to A/B/C: all three ship a `ThreadStatus` nothing can
  ever leave, and the design of the *wake* — which is the hard part — is deferred either way.
- Against it: the park is genuinely independent of where readiness comes from, and landing it
  early converts Kestrel's engine thread from "unimplemented native, run aborts" into "parked
  forever", which is what the real thread does when no connection ever arrives. That unblocks
  measurement of everything downstream of the engine thread. It also means the socket PRs
  never have to touch the scheduler.

## 5. Recommendation

**Option B**, and the §3 measurements are what decide it. Before measuring, "add an
`OpenFileTarget` case" looked like a rider on the wait; it is in fact ten flavour-divergent
rows, an `fstat` shape that PawPrint's inode-driven stat cannot currently express, and an
`OpenFileObject` identity question. That is a PR, and bundling it with the scheduler plumbing
(Option A) would put two unrelated review burdens in one diff — the exact shape
`repeated-review-findings-signal-wrong-altitude` warns about.

Within PR 1, prefer measured errno rows to `failwith`s, since the measurements are already in
hand and `dup`/`close`/`fstat` on a port descriptor are operations a real socket
implementation will exercise.

Option C is worth noting as a genuine fallback rather than a straw man: its rows 1–7 of the
§2 table are real, reachable, differentially testable content, and six of them diverge by
flavour. If the priority is landing something correct this week, C is defensible — but it
should then be *titled* as argument screening, not as implementing the wait.

Option D is the one to decline explicitly: it is the wide-blast-radius choice taken before
the consumer that would constrain it exists. Given the stated goal of a real network stack,
the readiness model should be designed against sockets, not retrofitted from pipes.

## 6. Tests

For the wait itself:

- Argument screening, as a hand-rolled `DllImport` guest in `sourcesPure` where the real
  runtime can serve as oracle: null buffer, null count, `*count < 0` (all `EFAULT`), bad fd
  (`EBADF`). Each row must be reached with a *valid* everything-else, or an earlier guard
  answers instead — `negative-test-may-fail-for-another-reason`.
- The flavour-divergent rows need the split verification pattern: a differential guest can
  only ever pin the *host's* column, so macOS runs pin Darwin and CI's Linux runs pin Linux,
  while both columns are asserted as data against the emulated platform preset. A
  `sourcesPure` guest that hard-codes one column would fail on the other host.
  Six of the eight rows in §2's table diverge, so this is the norm here, not an edge case.
- The park. The single-thread shape — guest creates a port and waits →
  `ProgramStepOutcome.Deadlocked`, stuck-thread description naming `WaitForSocketEvents` — is
  necessarily PawPrint-only, since real .NET would hang.

  The two-thread shape, however, **can** be differential, and I had wrongly written it off.
  Make the waiter a *background* thread: real .NET exits normally with that thread still
  blocked in `epoll_wait`, and PawPrint's driver exits as soon as the entry thread
  terminates. Same observable exit code from both runtimes, with the waiter genuinely parked
  in each. That needs create/close to exist, so it is a PR-2 test under Option B.
- Guests report via exit codes, never `Console` — a Console guest costs ~10 minutes under the
  interpreter against ~3 seconds for an exit-code one.
- A unit-level pin that the parked thread's frame stack still carries the native frame, which
  is what re-entrant parking means and what a resume-style regression would break.

Mutation: one per arm of the screening order, plus flipping the dispatcher arm to pop the
frame (must kill the frame-stack test), plus removing the parking transition (must kill the
deadlock test).

Note that "one mutant per arm" does **not** kill *reordering* mutants, which are the likely
bug here given §1's four-deep ladder. Each adjacent pair needs an input on which the two
orders disagree. The corrected flavour ladder supplies these naturally — e.g. `(bad fd,
*count == 0)` separates rows 1 and 2, and `(live non-port fd, unmappable buffer)` separates
rows 3 and 4 — so the reorder mutants are killable without inventing anything.
