# Socket readiness delivery: the wake out of `BlockedOnSocketEvents`

The third and last of the architectural items `2026-08-17-aspnet-critical-path.md`
names for Option B. #1109 gave the port an interest table (the registration half);
this is the delivery half — what makes a registered descriptor ready, and what moves
a thread out of `ThreadStatus.BlockedOnSocketEvents`.

**Prerequisite: `SystemNative_Connect`, which is PR #1122.** Connect is the only
producer of a readiness edge PawPrint can model, so this slice has nothing to deliver
until that lands. Its branch's base is an ancestor of `a4dae69d`; the cherry-pick onto
`a4dae69d` is clean (verified — the result was `c8ab636e`, which is what every
measurement below was taken against).

## Measured: where rung I stops, at each stage

`run-ladder.sh RungI`, host CoreLib flavour, four runs today:

| Tree | Rung I's first failure |
| --- | --- |
| `origin/main` @ `a4dae69d` | `SystemNative_Connect` unimplemented, 9 frames out; thread 1 parked in `BlockedOnSocketEvents (OpenFileDescriptionId 3L)` |
| + `socket-connect` | connect's registered-listener refusal ("…a real kernel would queue an edge-triggered readiness event…") |
| + all three refusals neutered | **deadlock**: thread 0 `BlockedOnSyncBlockWait` in `Monitor.Wait`, thread 1 `BlockedOnSocketEvents` in `WaitForSocketEvents` |
| + crude delivery **and** a crude unpark | `SystemNative_Poll`, on a **fourth** thread — thread 2 in `Sys.Poll`, thread 3 in `WaitHandle.WaitOneNoCheck` |

Three things that table settles, none of which was safe to assume:

- **Delivery is what rung I demands next.** Not `Poll`, not `GetSocketErrorOption`,
  not `Send`/`Receive`: with the refusals gone and no delivery, the guest deadlocks
  rather than reaching another native.
- **Delivery alone is not enough — the unpark is a separate missing piece.** With
  delivery stubbed but nothing flipping the status, rung I deadlocks in exactly the
  shape of row 3. The engine thread's first `WaitForSocketEvents` happens before any
  registration exists, so it parks with an empty table and never re-enters.
- **With both, the wake really does drive the engine.** Two new threads appear:
  `SocketAsyncEngine` handled the delivered batch and dispatched onto the thread
  pool, which reached `TryCompleteConnect`. So this slice's downstream rung is
  `Poll`, and the estimate that Poll and `GetSocketErrorOption` are "the wake slice's
  natural companions" is confirmed as *next*, not *included*.

## Measured: what edge-triggered epoll actually reports

Linux 6.18.5 `arm64` via `container`, probes `et.c` and `order.c` checked in beside
this document. Reproduce with
`container run --rm -v "$PWD/docs/plans/2026-08-21-socket-readiness-wake:/probe" gcc:14 bash -c 'gcc -O0 -o /tmp/et /probe/et.c && /tmp/et'`.
`EPOLLIN | EPOLLET` on a listening TCP socket, `epoll_wait` with timeout 0
throughout, so every row is about state rather than timing.

| # | Sequence | `epoll_wait` |
| --- | --- | --- |
| A | connect, accept (queue drained), then wait | **0 events** |
| B | connect, wait, wait again with the queue still nonempty | 1 event (`0x1`), then **0** |
| C | accept (drain) then connect (refill), both between two waits | **1 event** |
| D | a second connect onto an already-nonempty, already-reported queue | **1 event** |
| E | fresh port, `EPOLL_CTL_ADD` of an already-readable target | **1 event** |
| F | two listeners registered l1-then-l2, edges arriving l1-then-l2 | batch `[l1; l2]` |
| G | same registration order, edges arriving l2-then-l1 | batch `[l2; l1]` |

Each row falsifies a design that would otherwise look reasonable:

- **A** kills "stored edges, reported as recorded": the edge was queued and then the
  level went away, and epoll reports nothing. It re-polls the file at delivery.
- **B** kills plain level-triggered recompute: the level is still high on the second
  wait and nothing is reported.
- **C** kills "watermark of last-reported readiness mask" without a drop hook.
- **D** kills that watermark scheme *even with* a drop hook. The mask did not change
  — `READ` was already high and already reported — and epoll reports anyway. So the
  edge is "the driver signalled", not "the readiness mask transitioned".
- **E** is the premise #1109's guards already rest on, now measured directly.
- **F/G** say the batch is in **edge-arrival order**, not fd or registration order.
  A `Map`-fold over the interest table would order by `(fd, description)` and get
  this observably wrong.

So the faithful model is exactly epoll's: *an edge marks a registration as a
candidate; delivery reports the candidates that are still ready, in arrival order,
and drops the rest.*

## Measured: the per-phase readiness masks (2026-08-22, `masks.c`)

Level-triggered `epoll_wait` with timeout 0 reports the current level directly, so
each row registers LT with interest `IN|OUT|RDHUP` and reads the mask off. Linux
6.18.5 via `container`; Darwin is not measured because the Darwin registration arm
refuses, so no Darwin readiness is reachable through an event port.

| Phase | Mask |
| --- | --- |
| `Idle` stream (bound or not) | `EPOLLOUT\|EPOLLHUP` (0x14) |
| `Idle`/`DatagramPeer` datagram | `EPOLLOUT` — **no HUP**, unlike the stream case |
| `Listening`, queue empty (or drained) | nothing |
| `Listening`, queue nonempty | `EPOLLIN` |
| `EstablishedPendingReport` / `Established` (either end) | `EPOLLOUT` |
| `RefusedPendingDelivery` | `EPOLLIN\|EPOLLOUT\|EPOLLERR\|EPOLLHUP\|EPOLLRDHUP` (0x201d) |
| after the refusal delivery resets the socket | `EPOLLOUT\|EPOLLHUP`, i.e. exactly `Idle` again |
| established, peer closed | `EPOLLIN\|EPOLLOUT\|EPOLLRDHUP` (0x2005) — state this slice cannot represent; see the refusal below |
| pipe read end, empty, writer closed (PawPrint's stdin) | `EPOLLHUP` (`pipes.c`) |
| pipe write end, space, reader open (PawPrint's stdout/stderr) | `EPOLLOUT` (`pipes.c`) |

Interest filtering, measured on the same rows: the reported mask is
`level ∩ (interest ∪ {EPOLLERR, EPOLLHUP})` — an established socket registered
`IN`-only reports **nothing** (`OUT` filtered, no phantom bits), a pending refusal
registered with interest 0 still reports `ERR|HUP` (0x18), and `RDHUP` is maskable
(absent unless requested).

Two facts from the PAL sources (`pal_networking.c`, pinned at the devshell's
runtime version) that shape delivery:

- Registration always ORs in `EPOLLET`, and maps `SA_*` to epoll bits 1:1
  (`GetEPollEvents`), so every registration PawPrint holds is edge-triggered and
  the stored `SocketEventInterest` translates bijectively.
- Delivery (`ConvertEventEPollToSocketAsync`) **folds `EPOLLHUP` into
  `EPOLLIN|EPOLLOUT` and drops it** before converting to `SA_*` — so `SA_CLOSE`
  is unreachable on Linux, and an idle socket's `OUT|HUP` delivers as
  `SA_READ|SA_WRITE` (0x3). On success the inner function writes
  `*count = numEvents` and returns SUCCESS with `errno` untouched.

## Measured: what re-arms, what re-orders, what signals (`order2.c`–`order4.c`)

| # | Sequence | Result |
| --- | --- | --- |
| H | edges l2, l1, l2 (l2 re-signalled while pending) | batch `[l2; l1]` — a re-signal does **not** move an entry already pending |
| I | l2 made ready while unregistered, edge l1, then ADD l2 | batch `[l1; l2]` — an ADD-of-ready inserts at **ADD time**, not at the old edge's time |
| J | three ready, `maxevents = 2` | first two in order; the third delivers on the next wait; nothing after that — truncation preserves the remainder, delivery consumes |
| K | MOD (same interest) of a consumed, still-ready target | re-reports — MOD-of-ready re-arms |
| L | MOD of a target already pending | order unchanged — MOD does not move a pending entry |
| M | refusal delivery (the ECONNREFUSED-reporting connect, which resets the socket) | **signals**: a fresh `OUT\|HUP` edge |
| N | completion report (the SUCCESS-returning retry connect) | does **not** signal |
| O | UDP re-connect and `AF_UNSPEC` dissolve | do **not** signal |
| P | `bind(2)` on a registered socket | does **not** signal |
| Q | peer close on an established pair | **signals**, with mask 0x2005 |
| R | one edge, two registrations of the same socket (via `dup`) on one port | delivered in **reverse registration order** (the socket's wait-queue is LIFO), whichever fd is which |
| S | as R, with a MOD of the first-registered entry before the edge | order unchanged — MOD does not move a registration's place in the tie (`order5.c`) |
| T | an edge that misses a registration's interest entirely (IN at a WRITE-only entry), then a MOD to an interest the level meets | the missed edge queues **nothing**; the MOD enqueues fresh at MOD time, behind everything queued since (`order6.c`) — so the signal must filter by the registration's reported mask, not queue unconditionally |

Rows N/O/P answer the chokepoint question from option 1 in the negative twice
over: not only is there no single function through which `Sockets` writes pass
(there are eleven write sites), a blanket bump-on-any-write would be *wrong*,
because bind, the UDP re-target, and the completion report all mutate the socket
and must not signal. The producer set is an explicit enumeration, and in this
slice it is exactly three: the queue push onto a listener, the connect
resolution on the client (completion or refusal, blocking or not), and the
refusal delivery's reset (row M).

Row Q is a producer this slice does not model — nothing marks an `Established`
socket's peer as gone, and inventing the mask without the state would be a lie —
so `closeFd` must refuse when the description being destroyed is the last onto a
socket whose connection's *other* end is registered with any event port. With no
registration there is no observer (no receive path exists yet), so the sweep in
#1125's close path is otherwise unchanged.

Two facts a Codex review round surfaced, then settled by reasoning about the
in-flight syscall (each with its own oracle-validated observer):

- `epoll_wait` uses the `maxevents` it was *entered* with; a guest overwriting
  the count cell mid-park changes nothing. The park therefore captures the
  count (`EmulatedKernel.ParkedSocketWaitCounts`) and re-entry consumes it in
  place of a re-read — pinned by `SocketEventWaitCountCapture.cs`, which
  overwrites the cell with the fatal-if-re-read 0 while the waiter is parked.
- a real `close(2)` does not end an in-flight `epoll_wait`: the syscall holds
  a file reference, so the port and its registrations stay alive for it and a
  later edge can still complete the wait. PawPrint's close would sweep the
  description and strand the waiter, so destroying the last descriptor of a
  parked-on port refuses instead (retention is the unmodelled state).

## Options considered

### 1. Where the edge lives

**(a) An ordered ready list per port.** Each `OpenFileTarget.SocketEventPort` gains a
list of registration keys with an edge outstanding. Every operation that changes a
socket's readiness pushes that socket's key onto every port registered on it;
delivery pops, re-polls, reports the still-ready ones.

**(b) A kernel-global event stamp, watermarked per registration.** `EmulatedKernel`
gains `NextSocketEventStamp : int64`; each socket carries `LastEventStamp`, set from
the counter by any operation a real driver would signal. Each
`SocketEventRegistration` carries `SeenStamp`. Delivery is: the registrations whose
socket's `LastEventStamp` exceeds their `SeenStamp` **and** whose current readiness
mask is nonempty, ordered by stamp, truncated to `*count`; `SeenStamp` advances on
the delivered rows only.

**Chose (b)** (ratified 2026-08-21) — **and rows H, I, K and R falsify (b) as
written** (measured 2026-08-22, after the ratification). (b)'s delivery order is
"sorted by the socket's `LastEventStamp`", and that is observably wrong three ways:
a re-signal advances the stamp but does not move the pending entry (H); an
ADD-of-ready enters at ADD time, not at the socket's last edge's time (I, K); and
two registrations of one socket share every socket-side stamp yet have a measured
order (R). The measured semantics is exactly epoll's ready list: an entry is
appended when the driver signals a registration *not already pending* (LIFO across
same-signal ties, row R), or when an ADD/MOD finds the target ready and not
pending (E, I, K, L), and delivery walks the list in order, re-polls, drops or
reports, and keeps only what truncation spared (A, J).

Both options below reproduce every measured row; the choice is re-opened because
the ratified one cannot be kept as ratified. Producer call sites are identical in
both — one `signal` helper invoked at the three producing operations — so the
original locality argument for (b) no longer separates them; what remains is
where the pending state lives.

**(a′) The ready list, held per port.** `OpenFileTarget.SocketEventPort` carries
`Ready : (int * OpenFileDescriptionId) list` beside the registration table, in
delivery order. The signal helper finds every port registration naming the
changed socket (a scan of the description table — the kernel holds no reverse
index, and performance is not a goal) and appends the keys not already present,
newest-registered first (row R); ADD/MOD append their own key when the target is
ready and the key absent (E, I, K, L); delivery walks the prefix, re-polling each
entry (A); truncation leaves a suffix (J). Each registration records the counter
value at its ADD so that row R's tie order is data rather than `Map` iteration
order. This is structurally the thing epoll itself maintains, so every measured
row is reproduced by construction rather than by derivation.

**(b′) Signal stamps on the socket, watermarked per registration.** Each socket
keeps the ascending list of its signal stamps (not just the last); each
registration keeps `SeenStamp` plus an `AddedPending : int64 option` minted by an
ADD/MOD-of-ready; a registration is pending iff a signal exceeds `SeenStamp` or
`AddedPending` is set, ordered by the *earliest* such stamp; delivery consumes
both. Producers touch only the socket they hold, at the price of three
bookkeeping fields whose interaction reproduces the ready list indirectly, plus a
tie-break on the ADD-order counter for row R anyway.

**Chose (a′)** (ratified 2026-08-22). (b)'s surviving advantage was that producers write one
field on the socket they already hold, but both options now need the same
enumerated signal sites, (b′) needs an unbounded stamp list per socket *and* the
ADD-order counter that (a′) needs, and (a′)'s state answers "what does this port
deliver next?" by inspection where (b′) answers it by a min-search over three
fields. Drift risk is symmetrical: a wrong ready list and a wrong stamp set are
the same class of bug, and both are pinned by the same mutation battery.

### 2. What the readiness classifier becomes

`EmulatedKernel.socketEventRegistrationCouldFire : OpenFileDescriptionId -> bool`
exists to answer "can I prove this cannot fire?", and its three callers use the
answer to justify refusing. Replace it with the total function this slice needs —
`socketReadiness : OpenFileDescriptionId -> EmulatedKernel -> SocketEventInterest`
(the mask presented right now) — and let the old question become
`socketReadiness x <> empty`.

The alternative is to keep the boolean and add a mask function beside it. Rejected:
two functions answering one question is how the two drift, and the repo's own
guidance is that a classifier callers lean on stays truthful and load-bearing rather
than being shadowed.

### 3. What unparks the waiter

**(i) A state-based sweep.** `fireSocketReadiness`, run each tick in
`Program.advanceToDecision` beside `fireExpiredDeadlines`, flips a
`BlockedOnSocketEvents port` thread to `Runnable` iff that port has a deliverable
event. The handler re-enters and delivers from the caller's own frame.

**(ii) An explicit wake at each producing syscall.** `Connect`, `Accept`, `Send`,
`close`, `shutdown` each find the parked waiters on ports registered for the socket
they changed and unpark them.

**Chose (i).** #1060 made the park deliberately re-entrant — the frame stays, the PC
still names the call — precisely so that a wake need only flip a status, and (i) is
the shape that exploits it. (ii) re-introduces the producer-reaches-into-ports
problem from option 1(a), and its failure mode when a producer is forgotten is a
silent deadlock. (i) cannot forget: it asks the same question of the same state every
tick. Its cost is a per-tick scan over parked waiters × their registrations, and
performance is explicitly not a project goal.

**The property that makes (i) safe, and the one to get wrong:** the sweep's
"is anything deliverable?" and the handler's "what do I deliver?" must be *the same
function*, not two implementations of one idea. If the sweep is more permissive, the
handler re-parks and `advanceUntilRunnableOrQuiescent` spins forever instead of
reporting a deadlock; if it is less permissive, the waiter sleeps through an event.
One function, two callers.

### 4. Slice boundary: this, or this plus `Poll`

**(a) The wake alone** — readiness, the stamp model, delivery, the sweep, and the
removal of the three refusals. Rung I then advances to `SystemNative_Poll` (measured
above).

**(b) The wake plus `Poll` and `GetSocketErrorOption`**, so that rung I's connect
completion resolves and the rung advances to `Send`/`Receive`.

**Chose (a)** (ratified 2026-08-21). `Poll` shares the readiness predicate but has its own error ladder
and its own blocking-timeout behaviour, both of which want their own measured rows;
and this slice already carries the last architectural item, which is the part worth
reviewing on its own. (b) is defensible only if the readiness predicate turns out to
need Poll's synchronous observer to be testable at all — which the delivery guests
below suggest it does not.

## What the state records

- `OpenFileTarget.SocketEventPort` carries `Ready : (int * OpenFileDescriptionId) list`
  in delivery order beside its registration table, and each registration records
  the ADD-time ordinal (`RegisteredAt`, minted from a kernel counter) that breaks
  row R's same-signal ties, newest first. `EPOLL_CTL_MOD` preserves it — the
  wait-queue entry the tie order comes from is created at ADD and untouched by MOD.
- The three producers call one `signal` helper; ADD/MOD append their own key when
  the target is ready under the new interest and the key is absent (rows E, I, K, L).
- Delivery walks the `Ready` prefix, re-polling each entry against the current
  phase mask ∩ (interest ∪ {ERR, HUP}): an empty re-poll consumes silently
  (row A), a nonempty one delivers and consumes, and the walk stops when the
  batch is full, leaving the suffix pending (row J).
- No new `ThreadStatus`, no new wait reason, and no `FiredDeadline` case: the wake is
  not deadline-driven, which is why it is a sweep of its own rather than a row in
  `waitDeadline`.
- Delivery writes `SocketEvent { uintptr_t Data; int32_t Events; uint32_t Padding }`
  at `SimulatedUnixPlatform.socketEventBufferElementSize` stride (16 Linux, 32
  Darwin), `Data` verbatim from the registration, and stores the count through the
  caller's `count` cell. Both are already-modelled quantities; the buffer write is
  one `writeBytesThrough` of the whole batch.
- Three refusals die: connect's registered-listener refusal, the registration
  handler's nonempty-queue refusal, and the `park`-time loop. Their comments and
  `socketEventRegistrationCouldFire`'s docstring premise go with them.

## Tests

- Pure guest `SocketEventDelivery.cs`, raw P/Invoke, one check per row A–E plus F/G,
  each oracle-validated on real .NET (both flavours) before the handler exists. This
  is the observer the connect slice noted it lacked: the guards' removal now has a
  *positive* test, where previously deleting them would have passed the suite.
- `SocketEventsWaitBackgroundThread.cs` and `SocketEventListenerWait.cs` must keep
  passing unchanged. They are the no-spurious-wake observers: neither guest ever
  connects, so a sweep that unparks on anything less than a deliverable event turns
  both from "waiter stays parked, process exits" into a spin or an early return.
- A managed guest for the async rendezvous once `Poll` lands — not this slice; note
  it rather than park a test that cannot pass.
- Kernel unit tests: the `Ready` list across every measured row (A–R), truncation
  at `*count` leaving the suffix pending, the ADD-order tie-break, and the
  invariants that `Ready` names only registered keys and names each at most once.
- Mutation battery, one provoking input each: drop the re-poll (row A), report
  without a pending entry (row B), fail to consume a delivered entry (spin),
  consume the truncated suffix (row J), re-queue an already-pending entry on
  signal (row H), pend at signal time instead of ADD time (row I), order
  same-signal ties oldest-first (row R), make the sweep unconditional (the two
  waiting guests), and omit the signal from each producer in turn (deadlock).
- Re-run `run-ladder.sh RungI` and record the new frontier, expected `SystemNative_Poll`.
  **Done (2026-08-22, host flavour): rung I stops at `SystemNative_Poll` on thread 2**,
  with thread 1 back in `BlockedOnSocketEvents` — the engine woke, handled the batch,
  dispatched to the pool, and `TryCompleteConnect` is what reached Poll. Exactly the
  fourth row of the stubbing table above, now with nothing stubbed.

## Risks

- **A missed stamp bump is a deadlock**, and deadlock is the one failure this project
  most needs not to invent. Hence the chokepoint question in option 1, and the
  per-producer mutation row.
- **Sweep/handler disagreement is a spin**, which `run-ladder.sh` reports as neither
  a `GuestFailureException` nor a deadlock but as a run that does not finish. One
  shared predicate is the mitigation.
- The per-phase readiness masks are unmeasured. Everything above is structure; the
  numbers are not written yet, and should not be until both kernels have been asked.
