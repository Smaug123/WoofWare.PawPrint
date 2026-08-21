# The socket event port's interest table: SystemNative_TryChangeSocketEventRegistration

The entry point rung I of the ASP.NET ladder stops at, now that #1108's accept
answers EAGAIN: `SocketAsyncEngine.TryRegisterCore` registers the listening
socket with the event port, with thread 1 already parked in
`WaitForSocketEvents`. Measured on main `05d108f3` before this change: rung I
aborts on exactly this entry point.

## Scope

The registration table and the `epoll_ctl` contract over it. **Not the wake**:
no modelled operation can make a registered descriptor ready — `Connect` does
not exist, so no connection can arrive at the registered listener, and no data
can move — so firing an event, and the delivery half of `WaitForSocketEvents`,
have no producer and stay out. This is the same argument that kept the backlog
out of #1108, applied one layer up. What is *not* out is the table itself: unlike
a backlog, the table is read by the very syscall that writes it — a second `ADD`
answers `EEXIST` and a `MOD`/`DEL` of an unregistered target answers `ENOENT` —
so the state has both a producer and a guest-visible consumer inside this one
slice.

The managed surface is narrower than the entry point: dumped from the
interpreted image (System.Net.Sockets 10.0.7), `TryRegisterCore` issues the
*only* call — `(port, socket, None, Read|Write, (IntPtr)GlobalContextIndex)`, an
ADD with a small-integer `data` — and `UnregisterSocket` never calls the PAL
(deregistration is implicit at close). Raw-P/Invoke guests reach the rest.

## The wrapper (portable, pal_networking.c:3471)

- `currentEvents` or `newEvents` carrying bits outside
  `READ|WRITE|READCLOSE|CLOSE|ERROR` (0x1F) → `Error_EINVAL`.
- `currentEvents == newEvents` → `Error_SUCCESS`, no syscall, errno untouched.
- Both answers precede any *use* of the descriptor arguments:
  `ToFileDescriptor` is a cast, not a lookup, so a guest may pass a pointer as
  `port` alongside `current == new` and the real wrapper truncates it unread.
  PawPrint therefore decodes the fds only after the screens, exactly as
  `SystemNative_Accept` defers its decode behind the EFAULT screens.
- Past the screens the op is derived from the *claims*: `current == NONE` →
  ADD, `new == NONE` → DEL, else MOD. The claims are not checked against the
  table — the table's own answer (`EEXIST`/`ENOENT`) is what happens when a
  caller lies.

## The Linux kernel ladder (measured, 6.18.5 aarch64 via `container`)

Each adjacent pair is pinned by an input that provokes exactly one of the two:

| # | check | answer | pinning input |
| --- | --- | --- | --- |
| 1 | port fd dead | `EBADF` (9) | dead port + live target → EBADF |
| 2 | target fd dead | `EBADF` (9) | *non-epoll* port + dead target → EBADF, not EINVAL |
| 3 | target cannot poll (regular file) | `EPERM` (1) | port=file + target=that file → EPERM, not EINVAL |
| 4 | target *description* = port description, or port not an epoll instance | `EINVAL` (22) | port=file + target=socket → EINVAL, not EPERM; `ADD dup(ep)` into `ep` → EINVAL (description equality, not fd equality) |
| 5 | ADD: key already present | `EEXIST` (17) | second ADD of same fd |
| 5 | MOD/DEL: key absent | `ENOENT` (2) | MOD/DEL of never-registered target |

The registration key is the **(fd number, open file description)** pair, and
three measured rows pin each half:

- `ADD dup(sock)` after `ADD sock` succeeds (fd differs, description same);
- `DEL sock` then `DEL dup(sock)` removes two *separate* registrations;
- a `dup` of the **port** shares the table: ADD via `dup(ep)` then ADD via `ep`
  → EEXIST.

`EPERM` precedes the op check on both sides: MOD and DEL of a regular file are
EPERM, not ENOENT. The not-epoll EINVAL applies to all three ops (measured for
each). Pipes register fine (both ends), so a `StandardStream` target succeeds.
Another epoll instance as target succeeds in the simple case, but see the
refusal below. `errno` is set by the failed syscall (raw numbers above, all
flavour-independent), and untouched by the wrapper's own screens.

## Options considered

**Where the table lives.** (a) Payload on `OpenFileTarget.SocketEventPort`,
`Map<int * OpenFileDescriptionId, SocketEventRegistration>`; (b) a kernel-side
`Map<OpenFileDescriptionId, ...>` beside `Sockets`. Chose (a): the table is
per-description state of that target kind exactly as `File`'s offset is;
`dup(2)` of the port sharing the table falls out of the description sharing
(measured above); destroying the port's description destroys the table with no
cleanup step; and (b) would mint a denormalised pair of maps needing an
invariant that (a) makes unrepresentable. Cost of (a) is the mechanical
`SocketEventPort` → `SocketEventPort _` sweep over existing match arms.

**Darwin.** (a) Refuse at the kernel boundary, screens still answered; (b)
model kqueue. Chose (a), `SystemNative_FLock`'s precedent: kqueue's model is
*structurally* different — per-(ident, filter) registration, silent-replace
ADD (no EEXIST), separate READ/WRITE filter changes with a FreeBSD-shaped
two-call dance — and none of it is measured. A refusal names that; a guessed
model would answer wrongly on the first divergent row. The simulated platform
defaults to Linux, so the ladder and the managed engine are unaffected.

**ADD of another event port as target.** Measured, the simple case succeeds —
but the kernel's full-graph loop and reachable-path checks (`ELOOP`, depth
cap) are unmeasured, and a recorded nested port would then answer SUCCESS on
cycle inputs where Linux answers ELOOP. Refuse ADD-of-a-port loudly, naming the
unmeasured checks; MOD/DEL of a port target flow through the ladder honestly
(the table cannot hold one, so they answer ENOENT, which is what an
unregistered target answers). This bounds the undecidable input at its point of
use rather than refusing the whole entry point.

**Registration sweep on description destruction.** Linux removes a dead
description's registrations at file-release time (`eventpoll_release`). No
guest can observe the difference through `epoll_ctl` alone — the dead
description's key can never be probed again, since no fd names it — but a stale
entry is exactly what the wake slice must not deliver from, so the table should
stay truthful now rather than teaching the wake to filter corpses. It lives
*inside* `FileDescriptorRegistry.close` (the destroyed id and every port's
table are both in that module), pinned by unit tests since no guest can see it.

**Ops.** All three of ADD/MOD/DEL, not ADD-only: they are the three verbs of
the one table, sharing checks 1–4 wholesale, and refusing MOD/DEL would
failwith on inputs the ladder above already answers with measured rows.

## What a registration records

`{ Interest : SocketEventInterest ; Data : uint64 }` where `SocketEventInterest`
is five bools named after the PAL bits. The epoll mask is a bijection of those
bits (`GetEPollEvents`) plus an unconditional `EPOLLET` — a constant of the
wrapper, so not stored. `Data` is `uintptr_t`; the managed engine passes a
small integer index. **Nothing reads `Interest` or `Data` until the wake
slice**, so their stored *values* have no guest observer; registry unit tests
pin the write-back (a handler that recorded zeros would otherwise survive every
guest row — the state-writeback lesson). The *presence* of the entry is
guest-observable via EEXIST/ENOENT, which is what the guest batteries cover.

## The readiness guard (added after Codex review)

Recording a registration whose event a real kernel would deliver, while
`WaitForSocketEvents` parks forever, is a silent-deadlock divergence: an ADD of
a ready target queues an edge and wakes `epoll_wait` — an unconnected stream
socket is `EPOLLOUT|EPOLLHUP` the moment it is added, a datagram socket is
writable, a pipe end depends on peer state PawPrint does not model. The one
target whose events PawPrint can *prove* away is a **listening stream
socket**: its only read-readiness is a backlog entry (nothing can produce one
until `Connect`), it is never write-ready, and no modelled operation can put
it in an error or hangup state. That proof is
`EmulatedKernel.socketEventRegistrationCouldFire`, and two sites refuse on it:

- `WaitForSocketEvents` refuses to park while the port holds any registration
  that could fire (covers wait-after-register, whatever was registered);
- the registration handler refuses an Add/Modify that could fire while a
  thread is already parked on the port (covers register-after-wait, which is
  exactly rung I's order — the engine thread parks before the first
  registration arrives).

Registering a could-fire target with *no* waiter stays legal and recorded —
the ADD itself is faithful (the table really holds it), and any later wait
hits the park-time guard — which is what keeps the stdin rows of the impure
guest honest.

Observers: the classifier is pinned in both directions by unit tests
(listening/non-listening/datagram/stream/dangling);
`sourcesPure/SocketEventListenerWait.cs` pins the *allow* path in rung I's
exact shape (listener registered, waiter parked, second listener registered
past it — differential, exits 0 on real macOS and Linux), and the inverted
classifier (mutant V) aborts it through the park guard, proving that site
consults the classifier. **The refuse direction of the two sites has no
automated observer**: the refusal is an interpreter abort, which no exit-code
guest can assert, and the harness has no expected-deadlock mode that could
host a would-have-deadlocked guest. Deleting either guard outright therefore
survives the suite; the classifier unit tests and mutant V are the coverage
that exists, plus a one-off manual probe of the registration-site guard:
with the park guard disabled by hand and the classifier inverted,
`SocketEventListenerWait.cs` aborted with the *registration-site* message
("a thread is parked in SystemNative_WaitForSocketEvents on this port…") —
which also proves the guest's waiter genuinely parks before the second
registration, i.e. the parked-waiter path is exercised rather than vacuous.

## `data` is opaque until the registration commits (added after Codex review)

`epoll_ctl`'s failures never read the event payload, and DEL never stores it,
so an undecodable `data` — a provenance-tracked pointer PawPrint cannot
materialise to eight bytes — must not abort a call that answers
EBADF/EPERM/EEXIST/ENOENT, nor any Remove. The handler decodes leniently into
a `Result`, runs the ladder with a zero placeholder, and aborts only when an
Add/Modify is known to commit — the placeholder cannot survive, because the
abort precedes the table escaping. Raw pointer *bit patterns*
(`CliRuntimePointer.Verbatim`, a hand-rolled `void*` import) decode fine: they
are exact bits, which is all the kernel would hold. Pure guest rows 27–29 pin
the two orderings differentially.

## Tests

- `sourcesPure/SocketEventRegistration.cs` — differential rows on which the two
  real flavours' PAL agree (oracle validated exit-0 on the macOS host before
  implementation, so every row is a measured kqueue-agreement fact, and CI's
  Linux run pins the other column): the two screens (bits, current==new,
  including with dead fds and with a pointer as `port`), ADD/MOD/DEL success on
  a socket, ENOENT after full DEL and for a never-registered target, EBADF for
  dead port and dead target.
- `sourcesImpure/SocketEventRegistrationLinux.cs` — the epoll-only rows:
  EEXIST, both key-shape dup rows, the port-dup shared table, EPERM for a file
  (all three ops), EINVAL for a non-epoll port (all three ops), the two
  ordering rows of checks 2–4, `ADD dup(ep)` description-equality EINVAL, a
  stream target succeeding, and the raw errnos via `SetLastError`.
- No Darwin impure guest: the Darwin arm is a refusal, which no exit-code guest
  can assert; the screens above it are covered by the pure guest running under
  the default platform and by the macOS oracle run.
- `TestFileDescriptorRegistry.fs` — the sweep (close of the last target fd
  removes its registrations from every port; close of a dup removes nothing),
  table-shared-through-dup, stored `Interest`/`Data` round-trip, and a
  `checkInvariants` defect for a registration naming a dead description.

## Measured after landing

Rung I advances to `SystemNative_Connect`: thread 0's `AcceptAsync` registers
the listener and pends, `ConnectAsync` then reaches the missing entry point.
The readiness wake (registration → event → wake → delivery) becomes reachable
only once `Connect` can complete a connection, which is the slice after that.
