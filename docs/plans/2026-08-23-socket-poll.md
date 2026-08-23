# `SystemNative_Poll`

## Why this slice, and how the frontier was established

Re-measured at main `828d07a4` (2026-08-23) by rebasing the `aspnet-spike`
ladder onto it and running rungs D and I:

```
guest            real       pawprint   elapsed
RungI            42         134        5.80s
    Unhandled exception ... Unimplemented native method
    (PInvokeImpl libSystem.Native!SystemNative_Poll)
    Guest was: thread 1 (BlockedOnSocketEvents) in Sys.WaitForSocketEvents;
               thread 2 (Runnable) in Sys.Poll
RungD            42         42         4.40s
```

So the wake (#1139) landed and rung I advanced exactly as the stubbing table
predicted: the engine thread is parked in `WaitForSocketEvents`, a pool thread
has been dispatched `TryCompleteConnect`, and that calls `Poll`.

`System.Net.Sockets.dll` (linux-x64 pack, 10.0.7) reaches the raw entry point
from four managed methods, read off the IL:

| caller | timeout | shape |
| --- | --- | --- |
| `SocketPal.TryCompleteConnect` | `0` | `Poll(POLLOUT, 0)`, then `GetSocketErrorOption` |
| `SocketPal.HasNonBlockingConnectCompleted` | `0` | one fd |
| `SocketPal.Poll` (`Socket.Poll`) | `microseconds == -1 ? -1 : microseconds / 1000` | one fd |
| `SocketPal.SelectViaPoll` (`Socket.Select`) | as above | an array of fds |

**The slice is `SystemNative_Poll` alone; `SystemNative_GetSocketErrorOption` is
deliberately left out.** Rung I will therefore stop one frame further on, at
`GetSocketErrorOption`, which is an honest new frontier rather than a silent
one. The two are separable because `Poll` has its own guest-reachable surface
that needs nothing else (`Socket.Poll` / `Socket.Select` are synchronous and
call no other unimplemented native), and because `GetSocketErrorOption` is not
a readiness question at all — measurement below shows it *mutates socket
state*, adding a phase, which is its own slice's design work.

## Measured tables

Probe `docs/plans/2026-08-23-socket-poll/pollmask.c`, run on Darwin 25.6.0
(host, arm64) and Linux 6.18.5 (arm64, `container run gcc:latest`). Every row
polls `POLLIN|POLLPRI|POLLOUT` with timeout 0, so what comes back is the level.

### Socket phases

| phase | Linux revents | Darwin revents |
| --- | --- | --- |
| fresh idle TCP, unbound | `OUT\|HUP` | `0` |
| bound idle TCP | `OUT\|HUP` | `0` |
| fresh idle UDP | `OUT` | `OUT` |
| listening, queue empty | `0` | `0` |
| listening, queue nonempty | `IN` | `IN` |
| established, no data | `OUT` | `OUT` |
| established, one byte readable | `IN\|OUT` | `IN\|OUT` |
| established, peer FIN, drained | `IN\|OUT` | `IN\|PRI\|HUP` |
| established, peer FIN, byte queued | `IN\|OUT` | `IN\|PRI\|HUP` |
| non-blocking connect in flight | `0` | `0` |
| connect refused, pending | `IN\|OUT\|ERR\|HUP` | `IN\|PRI\|HUP` |

**Linux poll's level is exactly the epoll level `EmulatedKernel.epollReadiness`
already models**, once the PAL's conversion is applied: `EPOLLRDHUP` has no
row in `Common_ConvertPollEventsPlatformToPal` and `poll(2)` reports
`POLLRDHUP` only when asked for it, which the PAL never does. So

- idle stream `Out=true; Hup=true` → `OUT|HUP` ✔
- datagram `Out=true` → `OUT` ✔
- listener `In = queue nonempty` ✔
- established, peer alive `Out=true` → `OUT` ✔
- established, peer gone `In|Out|RdHup` → `IN|OUT` ✔
- `RefusedPendingDelivery` `In|Out|RdHup|Hup|Err` → `IN|OUT|ERR|HUP` ✔

That agreement is the load-bearing fact behind decision D1 below. It is not a
coincidence: both `poll` and `epoll` derive their mask from the same
`->poll` handler on the socket.

### Non-socket descriptors

| target | Linux | Darwin |
| --- | --- | --- |
| `fd = -1` (and any negative) | ignored, `revents = 0`, not counted in `rv` | same |
| never-opened fd | `NVAL` | `NVAL` |
| a regular file | `IN\|OUT` | `IN\|PRI\|OUT` |
| a directory, `O_RDONLY` | `IN\|OUT` | `NVAL` |
| `/dev/null` (a character device) | `IN\|OUT` | `NVAL` |
| pipe read end, empty, writer alive | `0` | `0` |
| pipe write end, space, reader alive | `OUT` | `OUT` |
| pipe read end, writer closed | `HUP` | `IN\|PRI\|HUP` |

`epollReadinessOfDescription`'s standard-stream constants are the **write end
with a live reader** (`OUT`, stdout/stderr) and the **read end whose writer the
launcher closed** (`HUP`, stdin). Poll agrees with both on Linux. Note the
stdin one is a *diverging* row rather than an agreeing one — Darwin answers
`IN|PRI|HUP` — which costs nothing under D2(a) but means it cannot support a
portable test.

### Argument screens

| call | Linux | Darwin |
| --- | --- | --- |
| `poll(fds, nfds = 0, 0)` | `rv = 0`, errno untouched | same |
| `poll(NULL, 0, 0)` | `rv = 0`, errno untouched | same |
| **`SystemNative_Poll(NULL, 0, …)`** | **`EFAULT`** — see below | same |
| `poll(NULL, 1, 0)` | `rv = -1`, `EFAULT` | same |
| `poll(events = 0)` on idle TCP | `rv = 1`, `revents = HUP` | `rv = 0` |

The last row confirms that `POLLERR`/`POLLHUP`/`POLLNVAL` are output-only and
reported whatever the interest mask says — the same rule
`EpollReadiness.reportedUnder` already encodes for `ERR`/`HUP`.

**The first four rows are libc `poll(2)`, which is not the guest surface.**
`Common_Poll` (pal_io_common.h) sits on top and screens first:

```c
if (pollEvents == NULL || triggered == NULL) return Error_EFAULT;
if (milliseconds < -1)                       return Error_EINVAL;
```

Those run in user space, so they set no errno and touch no descriptor — and
the null check does **not** consult `eventCount`. So the one row where the
two layers disagree is `pollEvents == NULL` with `eventCount == 0`: libc
succeeds with `rv = 0`, `SystemNative_Poll` answers `EFAULT`. PawPrint must
follow the PAL, and the impure test pins exactly that row.

`Common_Poll` uses a 2048-byte stack buffer (256 `struct pollfd`) and
`calloc`s above that, answering `ENOMEM` if that allocation fails. Measured:
no `ENOMEM` at `nfds` of 256, 257 or 1024 on either kernel, because it needs
real allocator exhaustion. PawPrint allocates nothing here and so never
answers `ENOMEM` — a divergence only under host memory exhaustion, which
PawPrint does not model anywhere.

### `SO_ERROR`, measured here but *not* implemented in this slice

Recorded because it settles the next slice's shape, and because it retires the
"ECONNABORTED" note in the connect plan:

| | Linux | Darwin |
| --- | --- | --- |
| `SO_ERROR` after a completed non-blocking connect | `0`; the next `connect` still answers **SUCCESS once**, then `EISCONN` | `0`; next `connect` answers `EISCONN` |
| `SO_ERROR` after a refused connect | `ECONNREFUSED`, **and consumes it** | `ECONNREFUSED`, and consumes it |
| poll level after that consumption | drops `ERR`: `IN\|OUT\|ERR\|HUP` → `IN\|OUT\|HUP` | unchanged, `IN\|PRI\|HUP` |
| next `connect` after that consumption | `ECONNABORTED` | `EINVAL` (the existing `Dead` latch) |

So `GetSocketErrorOption` needs a *new phase* on Linux ("refused, error
consumed") with its own poll level and its own `connect` answer. That is a
socket-state change, not a readiness projection, and it is why it is a
separate PR. It does *not* change `EstablishedPendingReport`: the "SUCCESS
once" latch is `sock->state`, not `sk_err`, and survives an `SO_ERROR` read.

## Decisions

### D1. One readiness level, or a second one for poll?

**(a) Share.** `EmulatedKernel.epollReadiness` becomes *the* per-socket level,
and each waiter projects it: `EpollReadiness.reportedUnder` for epoll (already
exists), a new `PollEvents.ofLevel` for poll (drops `RdHup`, keeps `Err`/`Hup`
unconditionally, adds nothing).

**(b) A separate `pollReadiness`,** so the two waiters can drift
independently.

**Choosing (a).** The measured table above says the Linux level *is* one
function; (b) would duplicate the phase→level rules and let two copies of a
measured fact disagree. The projection differences are real but small and
belong at the boundary, which is where the PAL puts them too. The sharing is
kernel-true rather than a coincidence of the measured rows: on Linux `poll(2)`
and epoll's `ep_item_poll` both read the same `file->f_op->poll` mask, and
neither direction of the PAL's conversion has an `RDHUP` row, so "drop RdHup"
is a fact about the PAL boundary rather than a fudge. Blast radius is small —
`epollReadiness` has two callers today, `epollReadinessOfDescription` and the
lazy in the delivery path (`EmulatedKernel.fs:5019`) — and the reversal cost
is low: splitting later is a copy.

Naming: the type `EpollReadiness` and the function `epollReadiness` would then
both be serving `poll`. Sub-option **(a1)** rename to `ReadinessLevel` /
`socketReadinessLevel` (49 occurrences, mechanical); **(a2)** keep the names
and widen the docstrings. Proposing **(a1)**: the current docstring explicitly
justifies the name as "epoll's terms rather than the PAL's", and that
justification stops being true the moment a second waiter reads it.

**This is a breaking public-API change.** `WoofWare.PawPrint.fsproj` sets
`IsPackable=true`, `FileDescriptorRegistry.fs` has no `.fsi`, and both
`EpollReadiness` and `EpollReadiness.reportedUnder` are public — so the rename
is visible to package consumers. Flagging it rather than discovering it in
review; the same type keeps serving both the "level" and "reported subset"
roles after the rename, so its docstring must cover both.

### D2. Which flavour does poll answer for?

**(a) Linux only.** A Darwin-flavoured kernel refuses with a `failwith` naming
the missing rows.

**(b) Both,** using the Darwin column measured above.

**Choosing (a).** An earlier draft of this plan argued that D1(a) *forces*
D2(a) because a shared function would have to become flavour-branching. That
argument was wrong and is withdrawn: the real alternative is to keep the shared
function Linux-only — which is where sharing is kernel-true, since kqueue is
structurally different and there is no Darwin epoll level to share *with* — and
give Darwin poll its own function whenever Darwin readiness is modelled. D2 is
therefore a free choice, and these are the actual reasons:

- **No consumer.** Nothing in PawPrint or its tests polls under a Darwin
  flavour today, so (b) would be built entirely on spec.
- **The Darwin column is not clean enough to bake in.** It diverges on almost
  every row (idle TCP empty; regular files `IN|PRI|OUT` but directories and
  character devices `NVAL`; `ERR`/`HUP` *not* output-only — measured,
  `pollmulti.c`), so it is a second modelling job rather than an extra column,
  and it would want its own impure guest.
- **The refusal is loud**, and "refuse rather than invent" is the skill's rule
  for exactly this position.

The Darwin column is recorded above so that whoever does model Darwin readiness
does not re-measure it; `SocketPhase.Dead`'s Darwin level, which the earlier
draft listed as unmeasured, is now measured at `IN|PRI|HUP` (`pollmulti.c`).

Cost of being wrong: a Darwin-flavoured guest calling `Socket.Poll` aborts with
a named refusal instead of answering. No such guest exists today.

### D3. How much of the timeout is modelled?

**(a) `timeout == 0` only;** refuse anything else.

**(b) Full park/wake:** a new `ThreadStatus.BlockedOnPoll` carrying the
captured fd set, a deadline projection in `Program.fs` (unlike
`BlockedOnSocketEvents`, a positive poll timeout *does* have one), and a hook
into the readiness sweep.

**(c) Compute the level for any timeout; return it whenever it is non-empty,
or whenever `timeout == 0`; refuse only the "empty and would block" case.**

**Choosing (c).** Measured (`pollmulti.c`): a ready fd returns in 0.0ms at
timeout `-1` and at timeout `5000`, on both kernels. So every answer (c) gives
is the answer the real call gives. (a) would refuse calls whose answer is not
in doubt — including `Socket.Poll(-1, SelectWrite)` on a connected socket,
which is the ordinary blocking use. (b) is a second feature (a new blocked
status with deadline plumbing) and belongs in its own PR, with the parked-poll
wake sitting beside the parked-`epoll_wait` wake #1139 built.

**The ready predicate is the whole of this decision, and it must be stated
precisely, because the obvious reading is wrong.** "Ready" is *not* "the level
is non-empty". It is:

> at least one entry whose **post-projection `revents` is nonzero** — that is,
> after masking `IN`/`PRI`/`OUT` against that entry's own `events`, with
> `ERR`/`HUP` reported unconditionally, and counting an entry that answers
> `NVAL`.

Taking the raw level instead is a *wrong answer*, not an incomplete one.
Counterexample: `Socket.Poll(1_000_000, SelectMode.SelectRead)` on an
established socket with a live peer and no data. The raw level is `{Out}`,
which is non-empty — but `SelectRead` asks only `POLLIN`, so the real
`revents` is empty and real Linux blocks for a second, during which a peer's
send or FIN can flip the answer. Returning `false` immediately is different
guest control flow *and* different elapsed virtual time. `NVAL` must count
because a never-opened fd makes a real `poll` return at once even at timeout
`-1`.

With that predicate the refusal covers exactly "every entry masks to empty and
the call would block", which includes the all-negative-fd set and the
`eventCount = 0, timeout > 0` pure sleep. The refusal names the park, so the
failure mode is a loud "this needs the parked-poll slice", never a wrong answer
or a hang.

### D4. What a non-socket target answers

Linux rows only, per D2:

| `OpenFileTarget` | answer |
| --- | --- |
| `Socket` | the shared level (D1) |
| `File` | `IN\|OUT` (measured: regular file *and* directory) |
| `StandardStream` | the levels `epollReadinessOfDescription` already holds — stdin `HUP`, stdout/stderr `OUT` — which the pipe rows above confirm poll agrees with |
| `SocketEventPort` | **refuse.** Unmeasured, and no managed caller can reach it (CoreLib never polls an epoll fd) |
| fd not in the table | `NVAL`, and *not* an error return |
| fd `< 0` | ignored: `revents = 0`, not counted in `rv` |

The `StandardStream` row is reachable in practice, not hypothetically. Three
assemblies in the linux-x64 pack import `SystemNative_Poll`, not one:
`System.Net.Sockets.dll`, `System.Console.dll` and
`System.IO.FileSystem.Watcher.dll`. Console's caller is `ConsolePal.Write`,
which polls the *standard stream* handle — so refusing that row would be
wrong. (`FileSystemWatcher` polls an inotify descriptor, a kind PawPrint does
not model at all, and fails long before reaching here.)

Note `epollReadinessOfDescription` currently `failwith`s on `File` and
`SocketEventPort` because no epoll registration can name them. Poll can, so
this is a sibling dispatcher (`pollReadinessOfDescription`) over the shared
per-socket level, not a widening of the epoll one — the epoll refusals stay
exactly as strict as they are.

### D5. The `SocketKind` dimension, which epoll's registration hid

Poll widens a door. `socketReadinessLevel`'s `Raw`/`SeqPacket` arm currently
`failwith`s, and **an earlier draft of this section said poll makes it
reachable for the first time. That was wrong.** The registration path screens
only `OpenFileTarget.File` as not-pollable (`FileDescriptorRegistry.fs:1450`);
a socket of *any* kind is admitted, and `changeSocketEventRegistration` then
computes `readyNow` through `epollReadinessOfDescription`
(`EmulatedKernel.fs:5260`). So an epoll ADD of an `AF_UNIX` raw socket reaches
that arm today, before this slice — and `SimulatedUnixPlatform.socketCreation`
really does admit `AF_UNIX` + `SOCK_RAW` / `SOCK_SEQPACKET` on the Linux
flavour (`EmulatedKernel.fs:2891-2895`), so such a socket can exist. Poll adds
a second, wider door; it does not open the first.

What the slice *does* change is the arm's stated contract. The message does not
claim unreachability — it claims the readiness is **unmeasured**, and asks for
a measurement "before registering it delivers". After this slice that claim is
half-false, and the halves are asymmetric: poll's level is measured below,
epoll's is still only *inferred* from the two waiters sharing one `->poll`
handler.

Measured (`pollgaps.c`, Linux; Darwin refuses to create either, `EPROTONOSUPPORT`):

| socket | level |
| --- | --- |
| `AF_UNIX` `SOCK_RAW`, fresh | `OUT` |
| `AF_UNIX` `SOCK_SEQPACKET`, fresh | `OUT\|HUP` |
| `AF_UNIX` `SOCK_STREAM`, fresh | `OUT\|HUP` |
| `AF_UNIX` `SOCK_DGRAM`, fresh | `OUT` |

Two consequences.

**(1) The existing kind-based arms are vindicated, not merely unfalsified.**
`epollReadiness` branches on `Kind` alone, so it was already answering
`AF_UNIX` stream and datagram sockets with rows measured on `AF_INET` ones. The
last two rows say that is right on Linux. That was a latent assumption; it is
now a measurement.

**(2) `Raw`/`SeqPacket` still refuse in this slice, and the refusal message
must be rewritten.** Wiring the two measured rows in is four lines, but the
function is shared (D1), so epoll delivery would start answering on *poll-side
measurement plus inference* — and these would be the only two rows in the
shared function resting on inference alone, where every other row is measured
through both waiters. No current oracle could check them either: the fuzzer's
vocabulary has no raw/seqpacket ops. D1's sharing was ratified on measured
agreement across every row, so it must not be the thing that carries an
unmeasured row into a second waiter.

The cheap unlock for the follow-up is an `et.c`-style *epoll* probe on
`AF_UNIX` raw and seqpacket sockets; after that, the four lines plus an impure
epoll guest are honest. Meanwhile the message must name both doors and state
the asymmetry, rather than — as an earlier draft of it did — claiming poll is
the only way in.

`SocketPhase.Dead`'s arm is the one that genuinely *does* argue from
reachability — "readiness is only ever computed for registrations" — and it
stays unreachable, because D2(a) refuses the Darwin flavour before any level is
computed. But after this slice two doors refuse it rather than one, so its
message must name both.

This is the failure mode the repo already knows as *newly-reachable inputs
falsify error messages* — and the drafting history above is a live instance of
it in the other direction: reasoning about which door reaches an arm, without
reading the screen that guards it, produced a confident and wrong claim in both
this plan and the message it prescribed.

## Scope

In:

- `SystemNative_Poll` handler in `Native/NativeSystemNative.fs`: decode the
  `PollEvent[]` (8 bytes each: `int32 FileDescriptor`, `int16 Events`,
  `int16 TriggeredEvents`), compute each `revents`, write `TriggeredEvents`
  back through the caller's array, write `*triggered`, return the PAL `Error`.
- The `EFAULT` / `EINVAL` screens, in `Common_Poll`'s order, before any
  descriptor is decoded.
- `PollEvents` PAL flag type + `ofLevel` projection.
- `pollReadinessOfDescription` in `EmulatedKernel.fs`.
- D1's rename, if ratified.
- Rewriting the two `epollReadiness` refusal messages whose reachability
  arguments this slice falsifies (D5).

Out, each behind a named refusal:

- blocking poll with every entry masking to empty (D3).
- the Darwin flavour (D2).
- polling a socket event port (D4).
- polling an `AF_UNIX` raw or seqpacket socket (D5) — rows measured, wiring
  deferred because the function is shared with epoll delivery.
- `SystemNative_GetSocketErrorOption`.

## Tests

1. **`sourcesPure/SocketPoll.cs`** — differential against real .NET on the
   host, so only rows *both* kernels agree on (PawPrint's kernel is `LinuxX64`
   whatever the host is, and a macOS dev box runs the oracle on Darwin).
   **Verified rather than derived:** the guest *as committed* exits 0 on real
   .NET on macOS *and* on real .NET on Linux
   (`container run mcr.microsoft.com/dotnet/runtime:10.0`) — re-checked after
   every change to its rows, since portability is a property of the exact rows
   rather than of the design. Rows: listener with
   an empty queue / with a pending connection / drained again (`SelectRead`),
   both ends of an established pair `SelectWrite` true and `SelectRead` false,
   idle UDP `SelectWrite`, and `SelectError` false everywhere (the only
   guest-visible exercise of the PRI request path). **No `Socket.Select`** —
   see the findings section below: which PAL entry point it reaches is a
   CoreLib-flavour fact, so the multi-entry array path is covered by the impure
   guest calling `SystemNative_Poll` directly. An established pair is
   reachable today —
   `sourcesPure/SocketConnectManaged.cs` already does blocking
   `Socket.Connect`/`Socket.Accept`, and a blocking fd never sees EINPROGRESS,
   so this needs neither Poll nor `GetSocketErrorOption` to set up.

   Two requirements that are load-bearing rather than incidental, and must be
   written into the guest as comments so a later edit does not break them:

   - **Every expect-false row must use timeout 0.** With a positive timeout, a
     masked-empty entry is precisely the D3 refusal, so PawPrint would abort
     rather than answer. `Socket.Poll` divides microseconds by 1000, so
     anything under 1000µs is also 0 — that is why the `Poll(500, …)` row is
     safe, and it should say so.
   - **The pending-queue row needs a blocking connect**, or the SYN race makes
     the real-runtime oracle flake on the host.

2. **A positive-timeout-*ready* row**, in the same pure guest:
   `Socket.Poll(100_000, SelectMode.SelectWrite) == true` on an established
   socket. This is the only test that separates D3(c) from D3(a) — without it,
   an implementation that simply refused every nonzero timeout would pass the
   entire suite.

3. **`sourcesImpure/SocketPollLinux.cs`** — raw `DllImport`, Linux-flavour
   rows: idle TCP `OUT|HUP`; `events = 0` still reporting `HUP`; a never-opened
   fd `NVAL`; `fd = -1` ignored and *not counted*; `SystemNative_Poll(NULL, 0,
   …)` → `EFAULT` (the row where the PAL and libc disagree); `milliseconds =
   -2` → `EINVAL`; request bits outside the six PAL bits ignored rather than
   rejected (`Common_ConvertPollEventsPalToPlatform` has exactly six rows, so
   they never reach the kernel); and `*triggered` equal to the number of
   nonzero-`revents` entries over a mixed array.

   **Two of these rows must run at a *positive* timeout, not 0**, and that is
   the point of them rather than a detail. They are the only tests that pin
   the *ready predicate* rather than the revents computation:

   - idle TCP, `events` not including `OUT` (or `0`), timeout 5000 →
     immediate `rv = 1`, `revents = HUP`.
   - never-opened fd, timeout 5000 → immediate `rv = 1`, `revents = NVAL`.

   Both are measured, not inferred (`pollimmediate.c`): on Linux each returns
   in 0.0ms at timeout 5000 *and* at timeout −1. Without them, a mutant that
   computes the ready predicate as `level ∩ requested` — while still writing
   `ERR`/`HUP` into `revents` correctly — passes every other test here, because
   every other ready row either runs at timeout 0 (where the predicate is not
   consulted) or is ready via a *requested* bit.

4. **Socket fuzzer op.** `socketFuzzCorpus/handPickedPoll.txt` carries eleven
   rows measured through the harness on real Linux, one per projection rule,
   each naming the mutant it kills — so the CI replay covers the op with no
   container. (Without checked-in rows this oracle would be container-only:
   the per-op-kind coverage assertion lives inside the opt-in live test.) `SocketFuzz.fs` / `socketFuzz/harness.c` gain
   `Poll of slot : int * events : int`. This is the strongest available oracle
   for D1 — it asserts "PawPrint's level equals real Linux poll's level" over
   *generated* socket-state sequences rather than the hand-written rows above,
   and it is nearly free because the harness already builds the states.

   Two things the plan must pin, not leave to the implementer:

   - **Which alphabet `events` carries.** The fuzzer's existing masks are PAL
     `SocketEvents` bits (0x01–0x1F); poll's are PAL `PollEvents` bits
     (`IN` 0x1, `PRI` 0x2, `OUT` 0x4, `ERR` 0x8, `HUP` 0x10, `NVAL` 0x20).
     Different alphabets. The op carries `PollEvents`, and `harness.c` converts
     to platform bits.
   - **The generated masks must include 0 and the output-only bits in the
     *request*** (`ERR`/`HUP`/`NVAL`), or the masking rule — the one place this
     design can be subtly wrong — goes untested by the strongest oracle. This
     is the "generator alphabet can hide divergence" trap.

   The op's transcript line records the full `revents` mask, not just the
   inputs, since that mask is the thing being compared.

   Sequences whose transcripts agree are written to `socketFuzzCorpus/` as
   embedded resources, so the deterministic replay test exercises them in CI
   with no container — the same arrangement the existing ops use, and without
   it this oracle runs only on a machine that has `container`.

   What this oracle does *not* own, stated so no one assumes otherwise: the
   single-slot op cannot exercise the multi-entry loop or `*triggered` (test 3
   owns those), and the fuzzer vocabulary has no raw/seqpacket or send/recv
   ops, so it cannot reach D5's refusal or a data-readable socket.

5. **Mutation testing**, per the skill, on the projection and the count.
   Note the naive rationale is wrong: `*triggered = eventCount` is *not*
   survived by any single-fd test, because a single not-ready entry at timeout
   0 already distinguishes 0 from 1. The mutant that survives single-fd testing
   is `eventCount` **when every entry is ready** — which is exactly the
   `TryCompleteConnect` shape — so the mixed-readiness array in test 3 is the
   killer, and a single-fd *not-ready* row is worth adding beside it as the
   cheap complement. Also mutate: `RdHup` retained instead of dropped;
   `ERR`/`HUP` masked instead of unconditional; `TriggeredEvents` left
   unwritten for a not-ready entry (assert it is overwritten to 0 from a
   garbage-preloaded value); `*triggered` written on the screen paths
   (`Common_Poll` returns before touching it); and — the two D3-specific
   ones — the ready predicate computed as `level ∩ requested`, and `NVAL`
   omitted from it. The last two are killed only by the positive-timeout rows
   in test 3.

## What the tests turned out to be worth

Nine mutants, run against `sourcesPure/SocketPoll.cs` and
`sourcesImpure/SocketPollLinux.cs`. **Two survived the first battery**, and
both were real gaps rather than unjustified lines:

| mutant | outcome | killed by |
| --- | --- | --- |
| `In` reported unmasked | **survived**, then killed | impure: listener with a queued connection, asked for `OUT` alone |
| `Err` maskable instead of output-only | **survived**, then killed | impure: refused connect, asked for nothing |
| `Hup` maskable instead of output-only | killed | impure: idle TCP, `events = 0` |
| `Out` reported unmasked | killed | pure: established socket, `SelectRead` |
| `*triggered` = `eventCount` | killed | impure: mixed array, 3 of 4 ready |
| refuse *every* nonzero timeout (i.e. D3(a)) | killed | pure check 11 and impure checks 24-27 |
| ready predicate ignores `ERR`/`HUP`/`NVAL` | killed | impure: positive-timeout rows |
| `NVAL` never reported | killed | impure: never-opened fd |
| entry buffer resolved eagerly at `eventCount = 0` | killed | impure: `Poll((PollEvent*)1, 0, 0, …)` |
| `Pri` echoes the request back | killed | corpus `poll:1:2`; pure: `SelectError` rows |

Four of the projection mutants — `In` unmasked, `Err` maskable, `Hup` maskable,
`Pri` echoed — are additionally killed by the **corpus replay test alone**,
which runs in CI with no container. That was the point of checking the
hand-picked rows in: the live fuzzer is opt-in and container-gated, so an
oracle that existed only there would not defend the projection on any CI run.

The two survivors are worth recording, because each says something the plan
had wrong:

**`SocketPal.Poll` masks the result a second time.** `Socket.Poll(…,
SelectWrite)` asks for `POLLOUT` and then tests `revents & POLLOUT`, so an
implementation that leaked an unrequested `IN` bit is *invisible* through the
managed surface. The plan had assumed a managed row could pin the mask; it
cannot. Only a guest reading `TriggeredEvents` directly can, which is why that
row lives in the impure guest even though the underlying fact is portable.

**A zero-entry poll must not resolve the entry buffer at all** (found by
Codex). `Common_Poll`'s copy-in loop is the only thing that dereferences
`pollEvents`, and it does not run at `eventCount = 0`, so a non-null pointer
naming nothing is legal: the call succeeds and stores zero. Resolving the
buffer unconditionally aborted that. `SocketPal.Select` reaches exactly this
shape when every list it was given is empty. Verified against the real kernel
before fixing — `Poll((PollEvent*)1, 0, 0, &triggered)` really does return
SUCCESS on Linux.

**Nothing else in the suite produces a socket whose level carries `ERR`.**
`RefusedPendingDelivery` is the only phase that sets it, so pinning "ERR is
output-only" needs a refused connect built by hand — bind, listen, capture the
port, close the listener, then a non-blocking connect to it. Without that row
the `Err` line of the projection was never executed with `interest.Err` false.

## Two findings that changed the test plan

**`Socket.Select` cannot appear in `sourcesPure`, and the reason is the CoreLib
flavour rather than the kernel.** `SocketPal.Select` branches on
`SelectOverPollIsBroken`, which is `OperatingSystem.IsMacOS() || IsIOS() ||
IsTvOS() || IsMacCatalyst()`; `IsMacOS()` is `#if TARGET_OSX` in CoreLib. So a
macOS-flavour image routes `Socket.Select` to `SystemNative_Select` — a
different entry point, not implemented — and a Linux-flavour image routes it to
`SelectViaPoll`. A pure row using Select would pass in CI and fail on a macOS
dev box for a reason unrelated to what it was testing. The multi-entry array
path is covered by the impure guest calling the entry point directly, where no
such branch exists.

**`nfds` is bounded by `RLIMIT_NOFILE` on Linux** (measured, `pollnfds.c`:
EINVAL above it; Darwin refuses at 65536 despite a far larger rlimit). PawPrint
models no descriptor limit at all, which `FileDescriptorRegistry` already
states, so it answers as though the bound were unbounded. The only refusal is
an interpreter one: a count whose byte extent overflows `int32`.

## What the frontier becomes

Measured, not predicted — rung I re-run against this branch:

```
Unimplemented native method
  (PInvokeImpl libSystem.Native!SystemNative_GetSocketErrorOption)
Guest was: thread 1 (BlockedOnSocketEvents) in Sys.WaitForSocketEvents;
           thread 2 (Runnable) in Sys.GetSocketErrorOption
```

So `TryCompleteConnect`'s `Poll(POLLOUT, 0)` now answers, the socket reports
itself writable, and the call proceeds to the second half.
`SystemNative_GetSocketErrorOption` is the next slice, and the `SO_ERROR`
table above is its measured starting point: it needs a new Linux phase
("refused, error consumed") whose poll level is `IN|OUT|HUP` and whose
`connect(2)` answers `ECONNABORTED`.
