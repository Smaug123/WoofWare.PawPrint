# Connection establishment: SystemNative_Connect, and accept(2)'s success path

The entry point rung I of the ASP.NET ladder stops at, now that #1109's
registration table records the listener: thread 0's `ConnectAsync` reaches
`SystemNative_Connect`. Re-measure on post-#1109 main before starting.

## Scope

The TCP connection state machine over loopback: `connect(2)`'s full ladder,
the listening socket's accept queue that a completed connect writes, and
`accept(2)`'s dequeue that reads it — one slice because the queue is one
state machine whose producer and consumer are these two syscalls, and because
#1108's `accept` answers EAGAIN on the ground that "backlogs are empty by
construction", a premise Connect falsifies: landing Connect without the
dequeue would make that EAGAIN a lie on a nonempty queue. Also UDP `connect`
(a default-peer filter, no queue), because the entry point is family-generic
and the rows are measured.

**Not the wake.** `WaitForSocketEvents` delivery, `SystemNative_Poll`,
`SystemNative_GetSocketErrorOption`, `SystemNative_GetPeerName`, send/receive,
and TCP `AF_UNSPEC` dissolution all stay out (each named by a refusal or by
the existing unimplemented-native failwith). The managed async path measures
as: `TryStartConnect` → `Interop.Sys.Connect`, pend on PAL `EINPROGRESS`
(0x1001A); completion is `TryCompleteConnect` → `Interop.Sys.Poll`(POLLOUT,
timeout 0) then `Interop.Sys.GetSocketErrorOption` — two natives that are the
wake slice's natural companions, not this one's.

## Measured (Linux 6.18.5 via `container`, Darwin 25.6.0 host; probe
`connect_probe.c` / `backlog_probe.c`)

Rows where the flavours agree (pure-guest candidates):

| input | both kernels answer |
| --- | --- |
| blocking connect → listening loopback | success, synchronously |
| implicit bind at connect | local port nonzero |
| connect again after blocking success | EISCONN |
| non-blocking connect → listening | **EINPROGRESS** (even on loopback) |
| blocking connect → closed port | ECONNREFUSED |
| non-blocking connect → closed port | EINPROGRESS |
| dead fd | EBADF |
| pipe / event port | ENOTSOCK |
| addrlen 4 | EINVAL |
| wrong family (AF_APPLETALK) | EAFNOSUPPORT |
| ORDER dead fd + short len / + wrong family | EBADF |
| ORDER pipe + wrong family | ENOTSOCK |
| connect → 0.0.0.0:port | success (treated as loopback) |
| UDP connect → closed port; retarget | success; success |
| UDP wrong family | EAFNOSUPPORT |
| accept: FIFO order, peer = client's getsockname, accepted socket's own addr = listener's | yes |
| accepted fd through the PAL is blocking | Linux natively; Darwin because `SystemNative_Accept` resets it (`pal_networking.c:1739`) — observable via `FcntlGetIsNonBlocking` |
| close client before accept; accept | still returns the connection |

Divergent rows (impure guests, per-flavour arms):

| input | Linux | Darwin |
| --- | --- | --- |
| connect on the listening socket itself | EISCONN | EOPNOTSUPP |
| connect retry after async establishment | success (reports completion once), EISCONN thereafter | EISCONN |
| connect retry after async refusal | delivers ECONNREFUSED once, then *resets the socket*: the next connect is a fresh attempt (EINPROGRESS, and the alternation repeats) | delivers ECONNREFUSED once, then the socket is dead: EINVAL forever |
| non-blocking connect → bound-not-listening | EINPROGRESS, then refused (RST → SO_ERROR ECONNREFUSED) | EINPROGRESS, then **pends** (SYN dropped; blocking form measured ETIMEDOUT after the SYN-retry stall) |
| AF_UNSPEC on connected TCP | success (dissolves) | EISCONN |
| UDP AF_UNSPEC (dissolve) | success | EAFNOSUPPORT |
| accept-queue capacity with no accept | clamped backlog + 1, where a negative or over-`somaxconn` backlog clamps to `net.core.somaxconn` (measured with the sysctl at 3: listen(-1) and listen(INT_MAX) admit 4; defaults read 4096) | exactly the clamped backlog, where non-positive and over-`somaxconn` backlogs clamp to `kern.ipc.somaxconn` (measured at the default 128: listen(0), listen(-1), listen(INT_MAX) each admit 128) |
| a client bound to 0.0.0.0 then connected | source resolves to 127.0.0.1, port kept — in `getsockname` and in accept's reported peer, TCP and UDP alike | same |

The retry-after-refusal rows above are the *no-SO_ERROR-read* path
(`probe3.c`): reading `SO_ERROR` first consumes the pending error and changes
the retry answers (probe-measured ECONNABORTED on Linux, EINVAL on Darwin).
`GetSocketErrorOption` is not modelled this slice, so only the delivery path
is reachable; the consumed path becomes this table's business when that
native lands in the wake slice.

Timing-dependent rows that no deterministic model can honour and no
deterministic test can assert (kept out of guests, documented here):
immediate re-connect during the EINPROGRESS window (Linux answered success
because loopback had already completed; Darwin answered EALREADY because it
had not — a race on real hardware either way). PawPrint's model resolves the
handshake instantaneously at connect time, so EALREADY is unreachable: no
socket is ever mid-handshake between syscalls.

## Options considered

**1. EINPROGRESS fidelity.** (a) Resolve the connect synchronously and answer
its final result (success/ECONNREFUSED) from the non-blocking call; (b) answer
EINPROGRESS exactly as both real kernels do, while the *kernel state* resolves
instantly (established or refused-latched), so every later observation agrees
with a real kernel that has finished the handshake. Chose (b): (a) changes
guest control flow — `SocketPal.TryStartConnect` takes the completed branch
where real .NET pends — which is precisely the divergence this project exists
to avoid. The cost of (b) is that the outcome is observed via connect-retry
(measured, per-flavour) until Poll/GetSocketErrorOption land.

**2. Connection identity.** (a) A kernel `Connections : Map<ConnectionId,
TcpConnection>` — each connection an object with two ends; the client socket
references its end, the accept queue holds `ConnectionId`s, accept
materialises the server socket referencing the other end; (b) socket-pair
references (client socket stores the peer `SocketId`, the queue stores client
`SocketId`s). Chose (a): a queued connection outlives the client that opened
it (measured: close the client, accept still returns the connection), so (b)
either dangles or needs a liveness refcount that denormalises socket lifetime;
and the connection object is where the data buffers land in the send/receive
slice, so the identity is being minted where it will be needed rather than
migrated to later. UDP takes neither: its "connection" is a default-peer
*address* on the socket, no object.

**3. Edge-triggered soundness without event storage.** The PAL registers
everything `EPOLLET`; a backlog push onto a *registered* listener queues an
edge event a real kernel delivers later even if the queue is drained first —
state #1109's table cannot hold. (a) Add pending-edge state to the port now —
that is the wake slice, pulled forward; (b) refuse the two transitions that
would mint an unrecordable edge: a connect whose backlog push lands on a
listener registered with any event port, and an Add/Modify of a listening
socket whose backlog is already nonempty (both refusals name the wake slice).
Chose (b). With both refusals, a recorded listening registration has an
empty-queue history for its whole registered window, so no edge has ever
occurred, and `socketEventRegistrationCouldFire`'s "listening stream socket
cannot fire" answer stays truthful — its docstring premise moves from
"nothing can produce a backlog entry" to "the refusals above keep a
registered listener's queue empty". #1109's guards pass unchanged;
`SocketEventListenerWait.cs` still exercises the allow path (it never
connects).

**4. Accept-queue overflow.** (a) Model the pend (client SYN parked until
space); (b) refuse a push beyond the measured capacity (Linux backlog+1,
Darwin backlog), with Darwin `listen(0)` refused outright as unmeasured.
Chose (b): the pend's resolution is timing (SYN retries), which the model
cannot honour and no guest can deterministically observe; (a) would also need
the wake. The capacity itself is exact per flavour, so the refusal boundary
sits exactly where a real kernel stops completing handshakes; both error
directions of that boundary matter, so the capacity is stored per listener at
`listen(2)` time (which now records the backlog it was given — re-listen with
a new backlog is refused as unmeasured rather than guessed).

**5. Blocking accept on an empty queue.** (a) Park the thread and let a later
connect wake it (a rendezvous outside epoll); (b) refuse loudly. Chose (b)
for this slice: the park/wake pair is delivery machinery (wake-slice-shaped),
and a single-threaded guest never needs it — a blocking loopback connect
completes without a concurrent accept (measured), so connect-then-accept in
one thread exercises the whole path. Same refusal for the blocking client
sides that cannot resolve: Darwin bound-not-listening (pends), overflow
(above).

## What the state records

**Shape of the per-socket state** (a further option pair): (a) keep
`IsListening : bool` and add a separate connection-state field, with an
invariant forbidding a listening socket that is also connected; (b) merge
both into one `SocketPhase` DU — Idle / Listening (queue + the `listen(2)`
backlog) / EstablishedPendingReport (async success not yet reported: the
next connect answers SUCCESS once, then Established) / Established /
RefusedPendingDelivery (the next connect delivers ECONNREFUSED, then Linux
resets to Idle and Darwin latches Dead; a *blocking* refusal delivers inline
and takes the same per-flavour transition — `probe4.c`) / Dead (Darwin:
EINVAL forever) / DatagramPeer. Chose (b): listening and being connected are
mutually exclusive in the kernel being modelled, and (a) would represent
their conjunction only to forbid it by discipline. The cost is the
mechanical sweep over `IsListening`'s nine readers.

- Kernel gains `Connections : Map<ConnectionId, TcpConnection>`, each holding
  the two endpoints' addresses. A queued connection outlives the client that
  opened it (measured: close the client, accept still returns it), which is
  why the queue holds `ConnectionId`s rather than client `SocketId`s. No
  end-of-connection back-references: liveness at cleanup is computed by
  scanning the (small) socket table rather than denormalising it into the
  connection, and a connection is removed when no socket phase and no queue
  names it.
- Implicit bind at connect assigns an ephemeral port through the same
  allocator `bind(2)` port-0 uses, with the loopback source address.
- `listen(2)` now records its backlog (its capacity is the overflow-refusal
  boundary), where before it read and discarded it.

## Tests

- Pure guest `SocketConnect.cs`: the agreeing rows above — the full ladder
  through raw P/Invoke, connect/accept rendezvous single-threaded, FIFO and
  address equalities via `GetSockName`, accepted-fd-is-blocking via
  `FcntlGetIsNonBlocking`, close-before-accept. Oracle: exit 0 on real macOS
  and Linux .NET before implementation.
- `sourcesPure/SocketConnectManaged.cs` — the managed synchronous rendezvous
  (`Socket.Connect` + `Socket.Accept`, blocking, single-threaded), which
  measured as staying entirely inside implemented natives. Pins
  `SocketPal.Connect`'s completed-synchronously branch and the endpoint
  marshalling; oracle exit-0 on real macOS and Linux.
- Impure `SocketConnectLinux.cs`: divergent rows, Linux column (retry
  semantics, listening-socket EISCONN, bound-not-listening refusal via retry,
  UDP dissolve, capacity boundary backlog+1), validated on real Linux .NET.
- Impure `SocketConnectDarwin.cs` (KernelConfig macOS): Darwin columns that
  answer rather than refuse (EOPNOTSUPP, EISCONN/EINVAL retries, capacity =
  backlog), validated on real macOS .NET.
- Registry/kernel unit tests: queue FIFO and capacity per flavour, connection
  survival across client close, the two edge-refusal guards' classifier
  interplay, ephemeral allocation, invariants (queue entries name live
  connections; connected sockets' connections exist).
- Mutation battery over: every ladder arm and both measured orderings, queue
  discarded/not-FIFO/capacity off-by-one in each direction and per flavour,
  latched-failure state dropped, implicit bind skipped, EINPROGRESS answered
  as success, both edge-refusal guards deleted (via the classifier tests
  where the abort itself has no observer), UDP peer filter dropped.

## Measured after landing

Expect rung I to advance from `SystemNative_Connect` to either
`SystemNative_Poll` (if the engine polls before registering) or the
registration guard's refusal on the client socket (non-listening stream,
waiter parked) — either way naming the wake slice. Measure, don't assume.

## Codex review round 1 (both findings measured, then fixed)

- **P1: `Backlog + 1` overflowed on `Int32.MaxValue`** — the backlog a
  parameterless `Socket.Listen()` passes — refusing the first connect on a
  full-looking queue. The real rule was then measured rather than patched
  around: `probe5.c` with `net.core.somaxconn` set to 3 shows Linux clamping
  negative and oversized backlogs to the sysctl before the `+ 1`, and Darwin
  (at its default 128) clamping non-positive and oversized backlogs to
  exactly the sysctl — which also finally explains the earlier "listen(0)
  admitted at least twelve" mystery. `somaxconn` is machine configuration,
  so it is `KernelConfig.SoMaxConn` (an `option`, per-flavour default
  resolved beside the platform in `applyTo`, the `FileSystemType` pattern),
  and the two former refusals (negative backlog, Darwin listen(0)) became
  measured answers.
- **P2: a client bound to `0.0.0.0` kept the wildcard as its source.**
  Measured on both kernels, TCP and UDP: connect resolves the source to
  127.0.0.1 with the port kept, visibly in `getsockname` and in the accept
  side's reported peer. `ensureBound` now rewrites the *binding* (not merely
  the connection's record), refusing non-loopback destinations where the
  resolution is unmeasured.

Both fixes carry guest rows (pure wildcard-source rows on both protocols,
the managed guest's parameterless-`Listen()` rendezvous, listen(0)/(-1)
clamp rows in each impure guest), unit tests at the exact clamp boundaries
under a forged `SoMaxConn = 3`, and three mutants (clamp dropped per
flavour, source-resolution dropped) — all killed.

## Codex review round 2

One finding: `KernelConfig.SoMaxConn = Some Int32.MaxValue` — a value
`withSoMaxConn` accepts — wrapped the Linux `clamped + 1` and refused every
connect. Fixed test-first (the failing test names the wrap) by computing the
capacity in `int64`.

## Codex review round 3

One finding, measured before fixing (`probe6.c`/`probe7.c`): the refusal
paths skipped the implicit bind, where a real kernel binds before the SYN —
observable through `getsockname`. The measured picture is richer than the
finding: while the refusal pends, both flavours report the resolved loopback
source and a real port; the *delivery* then diverges — Darwin keeps the
resolved address (all three bind provenances), while Linux's reset reverts
the address to whatever the guest's own `bind(2)` locked (loopback stays;
an explicit wildcard reverts to the wildcard; no bind at all reverts to the
wildcard too), keeping the port. That lock is real kernel state
(SOCK_BINDADDR_LOCK), so `SocketBinding` now carries `LockedAddress`, the
refusal paths bind before latching, and the delivery applies the
per-flavour rule. Pinned by a 3-provenance × 2-flavour unit test, guest
rows in all three raw-P/Invoke guests, and two mutants (revert dropped,
pending bind dropped) — all killed.

## Codex review round 4

One finding, measured before fixing (`probe8.c`): the Linux `AF_UNSPEC`
dissolve kept the binding my earlier comment claimed "survives" — and the
measured truth is stronger than the finding: the dissolve drops the *port*
too, unlike TCP's reset. An implicitly bound socket, or one whose `bind(2)`
gave the wildcard, reads `0.0.0.0:0` afterwards — fully unbound, so the
model clears its `Binding` and the next connect binds afresh. A locked
concrete address was measured to survive with the port zeroed
(`127.0.0.1:0`), a half-bound state whose re-bind behaviour is unmeasured
for a `bind(2)`-chosen port, so that provenance is refused by name. Pinned
by the unit dissolve test (now asserting the unbind and the fresh re-bind),
a `getsockname`-after-dissolve guest row, and a mutant — killed.

## Codex review round 5

One finding, measured before fixing (`probe9.c`): the Darwin dropped-SYN
refusal fired for *any* bound stream socket at the destination, but the
measured drop is exclusively the bound-but-unconnected case — a port held
only by established ends (their pcbs are keyed by the full peer tuple; the
scenario is an accepted socket outliving its closed listener) or by a
refused socket answers RST on both kernels, refusing like a closed port.
The guard now requires `SocketPhase.Idle`; pinned by a three-arm unit test
(idle refuses loudly; established and refused-held ports take the ordinary
refusal path) and a mutant — killed.

## Codex review round 6

One finding, measured before fixing (`probe10.c`): established children now
flow into `bindConflict`, whose flavour rules were measured (#1105) against
idle and listening sockets only, and Darwin's exact-duplicate refusal
wrongly hit the server-restart shape — accept a connection, close the
listener, bind a reuse-carrying replacement at the same endpoint. Measured:
that rebind succeeds on both kernels (established pcbs are keyed by the
full peer tuple), the re-listen too, and a flagless rebind is EADDRINUSE on
both. `bindConflict` now takes the existing socket's `SocketPhase` and
Darwin's exact-duplicate arm exempts established sockets; Linux's existing
rule already answered both rows correctly (the child inherits the
listener's SO_REUSEADDR). Pinned by two new conflict-matrix rows, a
dedicated pure guest (`SocketRebind.cs`, oracle exit-0 on both), Darwin-
flavour end-to-end rows, and a mutant — killed.

## Known unmodelled: TIME_WAIT

Probing this slice's review rounds surfaced that a close of an established
socket leaves a real kernel's TIME_WAIT holding the endpoint for 2MSL —
refusing flagless rebinds and steering the ephemeral allocator away — while
PawPrint forgets the socket instantly. No current guest can see the
difference (none rebinds a closed established endpoint without SO_REUSEADDR
or asserts port-number reuse, which is nondeterministic on real kernels
anyway), and modelling the expiry needs the virtual-time machinery of a
later slice. Recorded so the divergence is a decision rather than a
surprise.

## Codex review round 7

Two findings, both four-tuple corners only a REUSEADDR-bound client can
engineer (no managed path reaches either — managed clients connect from
fresh ephemeral ports), and both real answers unmeasured. Bounded refusals
at the exact inputs, per the usual rule:

- a resolved source *equal to* the destination with a listener present (a
  wildcard listener at P beside a reuse-bound client at 127.0.0.1:P) —
  plausibly EINVAL on Darwin, a completed self-connect on Linux;
- a duplicate (source, destination) pair (two clients reuse-bound to one
  endpoint connecting to one listener) — a real kernel refuses duplicate
  established tuples, plausibly EADDRINUSE at connect.

Each pinned by an Assert.Throws unit arm and a guard-deletion mutant —
killed.

## Codex review round 8

Two findings, both consequences of round 7's duplicate-tuple refusal:

- the ephemeral allocator scanned only the socket table, so a wrapped
  cursor could re-offer a port whose four-tuple a *connection* still
  occupies (its client closed, the connection queued or accepted) — making
  an innocent fresh connect abort on the refusal. A real kernel's
  connect-time port selection skips occupied tuples, and the allocator's
  `acceptable` now does too, in either orientation. Pinned by a unit test
  that rewinds the cursor onto the occupied port (the first version of the
  test missed the hazard because the cursor had naturally moved on — the
  mutant survived and exposed it) and a mutant — killed.
- the duplicate-tuple refusal compared only the client-to-server
  orientation; a connection's endpoint pair occupies the tuple from both
  ends, so the reverse orientation now refuses too. Unit test + mutant —
  killed.
