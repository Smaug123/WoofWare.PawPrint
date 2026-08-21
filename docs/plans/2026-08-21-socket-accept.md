# `SystemNative_Accept`

## Where this sits in the sockets plan

Re-measured on main at `8f53d77d` (the fcntl pair merged): rung D of the
ASP.NET ladder passes end-to-end, and rung I — the traffic rung — stops at
`SystemNative_Accept`, nine frames under `Socket.AcceptAsync`, with the
`SocketAsyncEngine` thread parked in `WaitForSocketEvents`. The earlier
stub-forward measurement says that once `Accept` answers `EAGAIN`, rung I
walks on to `SystemNative_TryChangeSocketEventRegistration` — the readiness
architecture proper.

## Scope: the error space is the whole slice

No modelled operation can put a connection into a listening socket's backlog:
`SystemNative_Connect` does not exist. So `accept(2)`'s *success* path is
unreachable in any PawPrint run today, and this slice deliberately does not
build it. Options considered for the backlog:

1. **No backlog state at all.** The empty backlog is a fact, not state:
   `IsListening && no pending connections` holds for every socket in every
   reachable kernel. Chosen — a `PendingConnections` field with no producer
   is dead state, and `EmulatedKernel.Sockets`'s docstring already names the
   backlog as the thing that will arrive with its producer.
2. **Add the backlog queue now, always empty.** Rejected: stored state must
   not outrun its writers any more than its readers; the queue's shape
   (SocketId list? connection records with peer addresses?) is decided by
   what `Connect` needs to put in it, which is exactly the next slice's
   design question.

The upstream shape (`pal_networking.c:1705`): screens
`socketAddress == NULL || socketAddressLen == NULL || acceptedSocket == NULL
|| *socketAddressLen < 0` to `Error_EFAULT` before touching the descriptor;
then `accept4(..., SOCK_CLOEXEC)` on Linux, and on macOS plain `accept` plus
`FD_CLOEXEC` plus an explicit clear of `O_NONBLOCK` on the accepted fd. Every
syscall failure stores `-1` through `acceptedSocket` and returns the PAL
conversion of errno, leaving the address buffer and its length untouched; the
EFAULT screen path stores nothing at all (the out-pointer may *be* the NULL).

## Measured error matrix

Probed on Linux 6.18.5 (Apple `container`, gcc image) and Darwin (macOS 26
host), 2026-08-21. The two kernels agree on every *classification*; only the
raw numbering differs.

| input | both kernels answer | raw Linux/Darwin |
| --- | --- | --- |
| listening + `O_NONBLOCK` + empty backlog | `EAGAIN` | 11 / 35 |
| …same through a `dup`, and with `*addrLen = 0` | `EAGAIN` | 11 / 35 |
| TCP socket not listening (bound or unbound) | `EINVAL` | 22 / 22 |
| datagram socket (bound or unbound) | `EOPNOTSUPP` | 95 / 102 |
| regular file, event port, either pipe end | `ENOTSOCK` | 88 / 38 |
| closed fd | `EBADF` | 9 / 9 |

Orderings that fall out of the disagreeing inputs: the wrapper's EFAULT
screens precede everything (code shape, upstream source); a dead fd beats
every socket-state answer; non-socket beats them too (a file is also "not
listening", and answers `ENOTSOCK`); the kind check beats the listening check
(a datagram socket is also "not listening", and answers `EOPNOTSUPP` on
*both* kernels — Darwin does not take the BSD `SO_ACCEPTCONN`-first shape
here); and not-listening beats `EAGAIN` (a bound non-blocking TCP socket
answers `EINVAL`).

`UnixError` gains `ENOTSOCK` (`Error_ENOTSOCK = 0x1003C`; raw 88 is
`EBADMACHO` on Darwin, raw 38 is `ENOSYS` on Linux, so the numbering is
platform-dependent).

## `socketOfFd` stops refusing non-sockets

The shared fd-to-socket helper refuses a non-socket descriptor with a
`failwith` whose own message says "a real kernel answers ENOTSOCK, which
PawPrint has not measured under this shim; measure it before answering". Now
measured — for **each** of its callers, not generalised from one: `bind(2)`,
`listen(2)` and `getsockname(2)` on a regular file, an event port and both
pipe ends all answer `ENOTSOCK` on both kernels, the same as `accept(2)`. So
the arm becomes `Error UnixError.ENOTSOCK` and all four entry points answer
it; the per-flavour raw numbers are pinned by the impure guests and the PAL
value by the pure one.

The IPv6 and Unix-domain `failwith`s in the same helper stay: those name
missing modelling, not a missing measurement.

## Blocking accept on an empty backlog

The one reachable input the matrix does not answer: a *blocking* listening
socket with nothing to accept, where a real kernel parks the caller until a
connection arrives — which no modelled operation can cause. Options:

1. **`failwith` naming the missing work.** Chosen. No BCL path reaches it —
   `SocketAsyncContext` switches the listener non-blocking before its first
   accept (that is what the fcntl slice landed), so only a hand-rolled
   P/Invoke or a synchronous `Socket.Accept()` can, and that guest deadlocks
   on real .NET too, so neither test tier could pin the alternative anyway.
2. **Park the thread on a new scheduler wait reason.** More faithful — the
   deadlock reporter would then name the stuck thread — but it mints a wait
   state whose wake does not exist, and the wake is `Connect`'s to build;
   `Scheduler.blockOnSocketEvents` earned its parking by having a real BCL
   caller that legitimately waits, which this does not yet have.

When `Connect` lands, the `failwith` is replaced by the real
dequeue-or-park, and option 2's wait reason arrives together with its wake.

## Tests

* `sourcesPure/SocketAccept.cs`: raw-P/Invoke differential guest, PAL return
  codes and out-pointer effects only (flavour-independent): the four EFAULT
  screens; EFAULT beating a closed fd; `EBADF`/`EINVAL` rows asserting errno
  too (raw 9 and 22 are portable); `EAGAIN`, `EOPNOTSUPP`, `ENOTSOCK` (via an
  event port) rows asserting the PAL code, the `-1` store through
  `acceptedSocket`, and that the address buffer and length survive
  unchanged; the dup row; `ENOTSOCK` from `Bind`/`Listen`/`GetSockName` too.
* `sourcesImpure/SocketAcceptLinux.cs` / `...Darwin.cs`: the raw errno
  numbers a `SetLastError=true` import observes for the platform-dependent
  rows — 11/35, 95/102, 88/38 — under each `KernelConfig.UnixPlatform`, plus
  the non-socket rows for the other three entry points.
* Re-run rung I and record the next frontier below.

## Measured after landing

Rung I now stops at `SystemNative_TryChangeSocketEventRegistration`, reached
from `SocketAsyncContext.Register` after `Accept` answers `EAGAIN` — with
thread 1 still parked in `WaitForSocketEvents`. The readiness slice
(registration, the interest set, and the wake) is next, and `Connect` after
that is what will make an accept *succeed* for the first time.
