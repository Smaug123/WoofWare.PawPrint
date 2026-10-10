# `shutdown(2)`, and `close(2)` under `SO_LINGER`, on TCP sockets

## Goal

Kestrel and `HttpClient`, as two PawPrint processes, talk over loopback and
then stop (`2026-10-07-multi-process-machine.md`, the last of "the socket
features rung M needs"). Stopping takes two calls the kernel does not model:

- **`shutdown(SHUT_RDWR)` on every connected socket, on both sides.** Kestrel's
  `SocketConnection.Shutdown` calls `Socket.Shutdown(SocketShutdown.Both)` and
  then `Dispose`. `HttpClient`'s `NetworkStream.Dispose` calls
  `InternalShutdown(SocketShutdown.Both)` and then `Close`. Neither side calls
  `SHUT_RD` or `SHUT_WR` alone.
- **`close` with `SO_LINGER` {1, 0} on Kestrel's listening socket.** Kestrel
  does not set the option itself. `SafeSocketHandle.CloseHandle` makes the
  close abortive when the close aborted a pending asynchronous operation, which
  the accept loop's pending `AcceptAsync` is, and `DoCloseHandle` then sets
  {1, 0} before it calls `close`. A connected socket on which `Shutdown(Send)`
  or `Shutdown(Both)` succeeded (or failed with `ENOTCONN`, which
  `SocketPal.Shutdown` ignores on a socket that was connected) has
  `_hasShutdownSend` set, so its close is never made abortive. So on the
  Kestrel path, linger {1, 0} reaches only the listener. A guest can still set
  it on a connected socket itself, and `.NET` does so on any socket closed
  from the finalizer.

This document is the measurement and design phase only. Nothing here changes
the kernel.

## 1. What exists

**On main.**

- `SO_LINGER` is stored (#1790): `SocketOptions.Linger`, a `SocketLinger` of
  `Enabled` and `Hundredths`. `accept` copies the listener's options onto the
  socket it returns. PawPrint handles `SystemNative_SetLingerOption` and
  `SystemNative_GetLingerOption` (`SocketOptionPal`).
- Two refusals stand in for a reset. Releasing the last reference to a
  connected socket whose linger is {1, 0}, while its connection is still
  referenced, is refused (`DescriptionReleaseRefusal.AbortiveClose`). So is an
  `accept` that would drop a dequeued connection (Linux, negative address
  length) while the listener's linger is {1, 0} and the client is open
  (`AcceptRefusal.AbortiveDrop`).
- Releasing the last reference to a listener whose accept queue holds a
  connection with an open client was refused
  (`DescriptionReleaseRefusal.ListenerWouldResetUnacceptedClient`), whatever
  the linger. Stage 2 replaced the refusal with the reset (3.3 (a)).
- There is no `shutdown` syscall in `WoofWare.PosixKernel`, and PawPrint has
  no handler for `SystemNative_Shutdown` or `SystemNative_Disconnect`.

**In the byte-transfer stack** (#1793, `tcp-4-nonblocking-transfer`, and
#1795, `tcp-5-blocking-transfer`, on `mp-4-cross-process-wakes`). The stack
branches from main before #1790, so it has neither `SocketLinger` nor the two
linger refusals. Its `ObjectLifetime.releaseDestroyed` replaces the code that
#1790 changed. Whoever rebases it first resolves that conflict, and keeps
`AbortiveClose` refused, since the stack does not call `abort` either.

- `TcpTransfer` (`TcpTransfer.fs`) holds a `TcpDirection` for each direction:
  the sender's send buffer (`Sending`), the receiver's receive buffer
  (`Receiving`), both capacities, and the receiving end's `TcpEndState`.
  That state is `Open`, `FinQueued` (the FIN waits behind bytes in
  `Sending`), `FinReceived`, `Reset of afterFin * errorPending`, or `Closed`.
- `TcpTransfer.close` is a FIN, or a reset if bytes are left unread.
  `TcpTransfer.abort` already exists: it is `closeWith` with `abortive =
  true`, which resets whatever is unread. Nothing calls it.
- A reset keeps what the survivor has in its receive buffer. It discards what
  the closer still had to send. What the survivor still had to send is
  discarded on Linux and stays counted on Darwin. A write after the peer's
  clean close is taken whole, and resets the connection: `EPIPE` pending on
  Linux, and `ECONNRESET` on Darwin. The take rules for the pending error are
  per flavour.
- A reset releases the survivor's port (`resetReleasedPort`, Linux only when
  the port was bound implicitly), the four-tuple (`resetReleasedTuple`), and
  the gone closer's endpoint (`orphanedConnectionOccupies`). After a FIN, all
  three stay held.
- Wakes: `TcpWake.DataArrived`, `SendSpace`, `PeerFinished` and `PeerReset`,
  which become `SocketWake`s. Parked reads and writes wake through
  `WakePrimitive.ConnectionReadable` and `ConnectionWritable`
  (`TcpTransfer.readAnswers` and `writeResumes`).
- Three places assume "a FIN or a reset reaches an end only because its peer
  has closed" (the `TcpEndState` docstring):
  - `violations` requires `FinQueued`, `FinReceived` and `Reset` to have a
    `Closed` peer;
  - `writeSpace` fails if the writer has sent its FIN;
  - `closeWith` fails if the closer's outbound direction is in a FIN or reset
    state.

  `shutdown` breaks all three.

## 2. Measured facts

**Probe.** `2026-10-08-tcp-shutdown-linger/tcp-shutdown.c`, with each
flavour's output beside it:

- `tcp-shutdown.linux-6.18.5-aarch64.txt`: Linux 6.18.5 aarch64 in Apple's
  `container` VM (Debian trixie, glibc 2.41), as root, default sysctls.
- `tcp-shutdown.darwin-27.0.txt`: Darwin 27.0.0 arm64 (macOS 27.0.1), uid 501.

The probe's header lists every section and what each line reports. `c` is the
connecting socket and `p` the accepted one. Two rules keep the recorded order
of events the order in the source:

- every call that touches a socket is a statement of its own, never an
  argument to another call, so that (say) a read that takes a pending error
  and an `SO_ERROR` that would otherwise take it run in a fixed order;
- every call that can put a segment on the wire (read, write, shutdown,
  close, connect) is followed by a 30 ms sleep, because Darwin's loopback
  delivers on another thread, and a reset its kernel sends in answer arrives
  after the call returns. Without the sleep, a Darwin write after the peer's
  ordinary close answers 100, then 100, with `SO_ERROR` 0; with it, 100,
  then `EPIPE` + `SIGPIPE`, with `ECONNRESET` pending.

A line ending `~counts` has byte counts or durations that depend on timing,
so only its answers and readiness bits are to be compared. A line ending
`~timing` is excluded from every replay. These are the lines whose outcome
waits on a TCP timer or on when the kernel advertises a window, where the
model's zero-latency delivery (3.6) gives a different but deliberate answer,
or whose state the model refuses:

| rows | flavour | why excluded |
|---|---|---|
| R `RD p-unsent`, R `RDWR p-unsent` | Linux | `c`'s reads make room the kernel does not advertise, so `p`'s bytes stay waiting (`EAGAIN`, no error); the model moves them at the first read, which on `RDWR` resets both ends (3.6) |
| R `RD p-unsent` | Darwin | the peer's reset waits on a timer, and the model refuses the state (3.6) |
| X, from the drain on | Linux | the bytes arrive on a zero-window probe within 250 ms; the model resets at the drain (3.6) |
| X, every line | Darwin | the model refuses the state (3.6) |
| S `cunsent`, the write line | both | whether `c`'s write finds room depends on how far the room `p`'s read made has been advertised (Linux) or drained (Darwin) |
| P `rd-then-fill`, the failed write | Darwin | the write races the reset its first bytes provoked |
| L `linger0 cqueued-pfin` | Darwin | the model refuses the close (3.2) |
| G `unsent` | both | the model refuses a close that would wait (3.4) |

The full probe ran twice on each flavour, and sections L, F and X three more
times each. Every line not marked `~` agreed between runs.

Poll and epoll were asked for IN|OUT|PRI|RDHUP, so the tables below never show
RDNORM or WRNORM. Darwin's kqueue is shown as `data/EOF/fflags`, and "-" means
not ready.

**Version and architecture.** Linux's rules all follow from
`inet_shutdown`, `tcp_shutdown`, `tcp_poll`, `tcp_disconnect` and the
`TCPABORTONDATA` branch of `tcp_rcv_state_process`. These have kept their
shape for many releases and read no architecture-dependent layout, so x86-64
should agree. The host-equality test proposed in stage 4 checks that on CI.
The fill totals depend on version (see the byte-transfer plan). Darwin's two
odd rules (data after `SHUT_RD` resets, and `SHUT_RDWR` is applied half by
half) are XNU implementation details. They are measured on 27.0 only.

### 2.1 What `shutdown` answers

| state of the socket | Linux | Darwin |
|---|---|---|
| connected, first call, any `how` | 0 | 0 |
| `how` already shut by an earlier call | 0 | `ENOTCONN` |
| `SHUT_RDWR` with only `RD` shut | 0 | `ENOTCONN`; **the WR half is not applied** (no FIN is sent) |
| `SHUT_RDWR` with only `WR` shut | 0 | `ENOTCONN`; the RD half **is** applied |
| peer's FIN received, `SHUT_RD` or `SHUT_RDWR` | 0 (RDWR sends the FIN) | `ENOTCONN`; nothing is applied, and no FIN is sent |
| peer's FIN received, `SHUT_WR` | 0 | 0 |
| reset, error pending or taken | `ENOTCONN`; the error stays pending | `ENOTCONN`; likewise |
| fresh, bound, refused, unconnected UDP | `ENOTCONN` | `ENOTCONN` |
| listening, `SHUT_RD` or `SHUT_RDWR` | 0, and see 2.6 | `ENOTCONN`, and nothing happens |
| listening, `SHUT_WR` | 0, and nothing happens | `ENOTCONN` |
| `how` = 3 or -1 | `EINVAL`, ahead of `ENOTCONN` | the same |
| not a socket, closed descriptor | `ENOTSOCK` (ahead of `EINVAL`), `EBADF` | the same |

The Darwin rows match XNU's `soshutdownlock`: the RD half fails with
`ENOTCONN` if `SS_CANTRCVMORE` is set, else it flushes; then the WR half fails
if `SS_CANTSENDMORE` is set. A received FIN sets `SS_CANTRCVMORE`. **This is
on the Kestrel path.** When the client's FIN arrives first, Kestrel's
`Shutdown(Both)` answers `ENOTCONN` on Darwin and sends nothing. `.NET`
ignores the error, and the FIN goes out at `close`.

### 2.2 What each end sees afterwards (sections S, T, P, R, X)

| | Linux | Darwin |
|---|---|---|
| shutter reads, after `RD` | the bytes already queued, then 0, never `EAGAIN` | 0: the receive buffer was **flushed** (FIONREAD 0) |
| shutter reads, after `WR` only | as before (bytes, or `EAGAIN`) | the same |
| shutter writes, after `WR`, including `write(0)` | `EPIPE` + `SIGPIPE` | the same |
| peer, after `WR` | the bytes, then 0 (the FIN waits behind unsent bytes, as `FinQueued` does) | the same |
| peer, after `RD` only | nothing: it may go on writing | nothing, until it writes (next row) |
| peer writes to a socket shut for reading | taken; the bytes queue and are readable; the peer can fill the buffers as before (4280576 bytes) | **reset at the first arrival**: the bytes are discarded, the peer reads `ECONNRESET`, and the shutter sees no error (reads 0, `SO_ERROR` 0, `EVFILT_WRITE` EOF) |
| peer writes to a socket shut both ways | **reset at the first arrival**: the shutter takes `ECONNRESET`; the peer, in `CLOSE_WAIT`, reads 0 and has `EPIPE` pending | reset at the first arrival, as for `RD` alone |
| shutter closes after `RD`/`RDWR` with bytes unread | reset (the bytes are still queued): after `RDWR` the peer reads 0 and writes `EPIPE`, since the FIN came first; after `RD` alone the peer reads `ECONNRESET` | FIN only (the flush left nothing unread) |
| `RD` while bytes wait in the peer's send buffer (R, p-unsent) | kept; the peer stays full, still after 5 s | **a timer decides**: no reset within 60 ms, and the peer reset (`ECONNRESET`) by 5 s; in one of four runs the peer took one more write first |
| `RDWR` while bytes wait in the peer's send buffer | the peer sees the FIN; the bytes stay waiting while the shutter reads 8192 bytes | the peer is reset at once; its in-flight bytes stay counted in its send buffer (`EVFILT_WRITE` data 0) |
| X: the sender shut writing with bytes waiting (its FIN queued), then the receiver shuts both ways and reads everything | nothing at once; **within 250 ms** a zero-window probe carries the waiting bytes in, and **both ends are reset with `ECONNRESET`**: the sender's own FIN had been made, so not `EPIPE` | the receiver's `SHUT_RD` had flushed what it held, nothing more arrives, and **nothing is reset for at least 15 s**; the sender's `EVFILT_WRITE` stays 0/EOF |

### 2.3 Readiness (sections S and T)

**Linux.** `poll` and level-triggered epoll agree on every row, and both
follow `tcp_poll`:

- IN and RDHUP once the receive side is shut, by the peer's FIN or by `SHUT_RD`;
- OUT once the send side is shut, whatever the buffer holds;
- HUP once **both** are shut;
- and the existing rules otherwise.

| shutter's state | shutter | peer |
|---|---|---|
| `RD`, idle | IN\|OUT\|RDHUP (0x2005) | OUT (0x4) |
| `WR`, idle | OUT (0x4) | IN\|OUT\|RDHUP (0x2005) |
| `RDWR`, idle | IN\|OUT\|HUP\|RDHUP (0x2015) | 0x2005 |
| `RD`, send buffer full | IN\|RDHUP (0x2001) | IN\|OUT |
| `WR`, send buffer full | OUT (0x4) | IN\|OUT, and no RDHUP until the bytes ahead of the FIN are read |
| `WR`, then the peer's FIN | 0x2015 | the peer, shut for writing and having received the FIN: 0x2015 |
| reset after `RDWR` | 0x201d (adds ERR) | 0x201d |

**Darwin.** The kqueue filters follow `filt_soread` and `filt_sowrite`.
`EVFILT_READ` reports EOF once the receive side is shut, with data equal to the
bytes unread. `EVFILT_WRITE` reports EOF once the send side is shut, with data
equal to the free space, **even below the low-water mark** (it reported
0/EOF while the buffer was full).

| shutter's state | shutter's poll | shutter's READ | shutter's WRITE | peer's READ |
|---|---|---|---|---|
| `RD` | IN\|PRI\|HUP (0x13) | 0/EOF/0 | 146988 (no EOF) | not ready |
| `WR` | **HUP alone (0x10)**; IN\|HUP once bytes arrive | not ready | 146988/EOF/0 | 0/EOF/0 |
| `RDWR` | 0x13 | 0/EOF/0 | 146988/EOF/0 | 0/EOF/0 |

`SHUT_WR` is the first state in which `EVFILT_WRITE` has EOF and
`EVFILT_READ` does not. Darwin's `poll` gives HUP and suppresses OUT, as it
already does for a reset.

### 2.4 Edges (section E)

| `how` | Linux, `EPOLLET` | Darwin, `EV_CLEAR` |
|---|---|---|
| `RD` | the shutter's registration reports again (0x2005); nothing at the peer | the shutter's READ fires (EOF) |
| `WR` | the shutter reports again (0x4: OUT, which it had already reported); the peer gets the FIN's edge (0x2005) | the shutter's WRITE fires (EOF); the peer's READ fires (EOF) |
| `RDWR` | the shutter 0x2015; the peer 0x2005 | the shutter's READ and WRITE; the peer's READ |
| `WR`, send buffer full | the shutter 0x4; nothing at the peer (the FIN is queued) | the shutter's WRITE (data 0 or 52, EOF); nothing at the peer |

On Linux the shutter's registration reports again for every `how`, including
`SHUT_WR`, where its level gained nothing. That is `inet_shutdown`'s
`sk_state_change`, a wake with no key, which queues every registration on the
socket. It is the shape `SocketWake.PeerReset` already has.

### 2.5 A thread asleep when `shutdown` comes (section B)

Both flavours agree except where stated.

| asleep in | `shutdown(c, RD)` | `shutdown(c, WR)` | `shutdown(c, RDWR)` |
|---|---|---|---|
| `read(c)` | returns 0 | sleeps on (returns the peer's next byte) | returns 0 |
| `read(p)`, the peer's | sleeps on | returns 0 (the FIN) | returns 0 |
| `write(c)`, nothing taken (buffer full) | sleeps on, and completes as `p` drains | `EPIPE` + `SIGPIPE` | as `WR` |
| `write(c, 8 MiB)`, part taken | sleeps on, and completes | **Linux: returns the count taken, no `SIGPIPE`. Darwin: `EPIPE` + `SIGPIPE`.** The taken bytes reach `p`, then the FIN. | as `WR` |
| `accept(l)` (`shutdown(l, ...)`) | **Linux: `EINVAL` at once. Darwin: `ENOTCONN` from the shutdown; the accept sleeps on** | both sleep on | as `RD` |

The write rows have the same shape as the stack's measured "a reset ends a
sleeping write" (`tcp-blocking.c`, W-reset).

### 2.6 A Linux listener, shut down (sections U, B, E, Q)

`shutdown(l, SHUT_RD)` or `SHUT_RDWR` on a Linux listener runs
`tcp_disconnect`:

- every queued connection's client is reset (0x201d, reads `ECONNRESET`, as in
  2.7);
- a sleeping `accept` returns `EINVAL`, and so does a later one;
- a new client's connect is refused;
- the socket keeps its port (`getsockname` unchanged), its poll is OUT|HUP
  (0x14), which is the model's idle stream socket, and `listen` succeeds again;
- an edge-triggered registration reports 0x14.

So the listener goes back to `SocketPhase.Idle` with its binding.

### 2.7 `close` with `SO_LINGER` {1, 0} (sections L and Q)

**A connected socket.** Whether linger {1, 0} resets depends on the two
FINs. "Made" means by `SHUT_WR` or by the close itself; "arrived" means the
FIN reached the peer, so that a FIN made behind unsent bytes is made but has
not arrived. Every row is section L; in the last four, the linger was set
before either FIN, because **Darwin answers `EINVAL` to setting `SO_LINGER`
once both directions are shut** (Linux takes it). `.NET` sets the linger at
close time and ignores `EINVAL` there, so on Darwin its abortive close of
such a socket is an ordinary close.

| closer's FIN | peer's FIN | Linux | Darwin |
|---|---|---|---|
| not made before the close (idle, unread, unsent, peer's unread data) | either | reset | reset |
| arrived | not made (`afterwr`) | reset | reset |
| queued behind bytes | not made (`cunsent-afterwr`) | reset | reset |
| arrived | arrived, either order (`bothfin-*`) | **no reset**: the peer sees only the FINs (0x2015, reads 0, `SO_ERROR` 0) | **no reset** (`EVFILT_READ` EOF, fflags 0) |
| queued, made after the peer's arrived (`pfin-cqueued`) | arrived | reset | reset |
| queued, made before the peer's arrived (`cqueued-pfin`) | arrived | reset | **no reset at the close** (poll IN\|HUP, no error); the peer reads what its receive buffer held, and then `ECONNRESET`, once its reads open the window towards a socket that has gone |

The Linux rows are `tcp_disconnect`: it resets in the states of
`tcp_need_reset`, and in `CLOSING` and `LAST_ACK` only while bytes are
unsent. So **once the exchange is complete, an abortive close is an ordinary
close**.

Where it resets, what the peer sees is exactly the stack's existing reset
rows:

- the peer keeps its receive buffer, reads it, and then reads the error;
- the closer's unsent bytes are discarded;
- on Linux, a write while the error is pending takes it (`ECONNRESET`, no
  `SIGPIPE`), and every later write answers `EPIPE` + `SIGPIPE`;
- on Darwin, every write answers `EPIPE` + `SIGPIPE`, and the error stays
  pending.

After the closer's `SHUT_WR`, Linux's peer, already in `CLOSE_WAIT`, reads
0, 0 and writes `EPIPE`, which (D) derives as the closer's FIN `Arrived` and
the peer's `NotSent`. Darwin's peer reads `ECONNRESET`. With the closer's FIN
still queued, Linux's peer takes `ECONNRESET`.

**The closer's endpoint after linger {1, 0}.** Free to a fresh bind at once
wherever the close resets, on both flavours: no `TIME_WAIT`. Where it does
not reset (both FINs arrived), Linux treats the close as ordinary for the
port too: free if the closer's FIN was passive, `EADDRINUSE` (`TIME_WAIT`)
if it was made first. Darwin frees it in both orders.

**After an ordinary FIN close, what decides is which FIN was made first, and
whether the closer's own FIN has arrived** (section F, `c` closes while `p`
stays open; the bind is of `c`'s endpoint, at once and 30 ms later, with the
same answer both times, on both flavours):

| order | bind |
|---|---|
| `p` `SHUT_WR`; `c` closes | free |
| `p` `SHUT_WR`; `c` `SHUT_WR`; `c` closes | free |
| `p` `SHUT_WR`; `c` fills; `c` closes (or `SHUT_WR`, then closes), so `c`'s FIN is passive but waits behind its bytes | **`EADDRINUSE` until `p` drains** the bytes; free at once after |
| `c` closes; `p` `SHUT_WR` | `EADDRINUSE` |
| `c` `SHUT_WR`; `p` `SHUT_WR`; `c` closes | `EADDRINUSE` |
| `c` fills; `c` `SHUT_WR` (its FIN queued behind the bytes); `p` `SHUT_WR`; `p` drains, so `c`'s FIN goes out after `p`'s arrived; `c` closes | `EADDRINUSE` |

So a FIN-closed end frees its endpoint exactly when **its socket has closed,
its own FIN was made after the peer's had arrived (passive), and its own FIN
has arrived**. The first two are fixed at the close and at the moment the
FIN is made: not when the FIN is sent, and not when the socket closes. That
is Linux's state machine (`SHUT_WR` moves to `FIN_WAIT1` whether or not the
FIN can go out, and a FIN received there leads to `CLOSING` and
`TIME_WAIT`; a passive end waits in `LAST_ACK` until its FIN is
acknowledged), and Darwin agrees on every row. The third can become true
after the close: an orphan whose passive FIN was queued releases its
endpoint when the peer's reads let the bytes and the FIN through. The stack
models neither case (see 3.5).

**A listener with queued connections** (Q). Closing it resets every queued
client: 0x201d on Linux, and READ and WRITE EOF/54 on Darwin. Each client
reads `ECONNRESET`, and its writes answer `EPIPE` + `SIGPIPE`. This happens
whether or not the client had written data, and **identically with linger off
and linger {1, 0}**: a listener's linger has no effect. The listener's port
is free to a fresh bind at once.

### 2.8 `close` with `SO_LINGER` {1, t > 0} (section G, t = 1 s)

| | Linux | Darwin |
|---|---|---|
| nothing unsent | returns 0 at once; FIN | the same |
| bytes unsent, blocking | returns 0 after ~1010 ms | returns 0 after ~1001 ms |
| bytes unsent, non-blocking | **blocks ~1020 ms**, returns 0 (`inet_release` ignores `O_NONBLOCK`) | returns 0 at once, **not** `EWOULDBLOCK` |
| afterwards | the peer reads every unsent byte, then 0 (no reset) | the same |

Kestrel never sets t > 0. `.NET`'s `DoCloseHandle` retries a close that answers
`EWOULDBLOCK`, which Darwin 27 never did here.

## 3. Design

### 3.1 How a shut direction is represented

Shutdown needs two facts per end that the stack cannot hold. One is the end's
own `SHUT_WR`, which must survive the peer's later close: after it, a write
must still answer `EPIPE` rather than reach a closed peer. The other is the
end's own `SHUT_RD`. Each flavour reads these differently: Linux keeps
receiving, and resets an arrival only once both halves are shut; Darwin
flushes, and resets the first arrival.

- **(A) A shutdown mask per end, kept beside `TcpEndState`.**
  `TcpDirection` gains the receiving end's own calls, `Shutdown : {
  Read : bool; Write : bool }` (or a four-case DU). `SHUT_WR`'s FIN is the
  existing `FinQueued`/`FinReceived` in the peer's direction, so every
  measured rule about a FIN followed by a reset (the stack's `afterFin`)
  applies unchanged, because it is the same FIN on the wire. These change:
  - `violations` allows a FIN state while the peer is open and has shut
    writing, and allows `Reset` at both ends;
  - `writeSpace` and `closeWith` handle those states;
  - a `Closed` end's mask must be empty, so equal states compare equal.
- **(B) Rewrite the end state as the kernels keep it.** Each end gets a
  receive-shut bit, a send-shut bit, whether the peer's FIN has arrived, and
  a pending error. That is `sk_shutdown`, `SOCK_DONE` and `sk_err`, or
  `SS_CANTRCVMORE`, `SS_CANTSENDMORE` and `so_error`. `FinQueued` becomes "the
  sender is shut and its bytes are still in `Sending`". Every measured row
  then reads as a transcription, and Darwin's `ENOTCONN` rules are literally
  "bit already set". But it replaces the type that #1793 and #1795 are built
  and property-tested on, and the bits admit combinations that the current
  DU rules out, which must then become invariants.
- **(C) Keep shutdown on the socket** (`SocketDescription`), and give
  `TcpTransfer` only "send a FIN". This is rejected. Darwin's reset on arrival,
  and Linux's when both halves are shut, are rules about a transfer. They would
  have to take the socket table, and `TcpTransfer.violations` could no
  longer check the states against each other.

- **(D) Split the end state along the fact boundary.** `TcpEndState` today
  holds two kinds of fact. The FIN travelling towards an end (none, queued
  behind `Sending`, arrived) is a fact about a *direction*. Whether the
  socket is open, reset or closed is a fact about an *end*. `SHUT_WR` is a
  direction fact that must outlive the peer's end fact, which is why (A)
  needs its `Write` bit. So separate them:

  ```fsharp
  /// The FIN in one direction: the sender's SHUT_WR or close. `passive`:
  /// the opposite FIN had already arrived when this one was made.
  type TcpFin =                                        // on TcpDirection
      | NotSent
      | Queued of passive : bool
      | Arrived of passive : bool
  type TcpEndState =                                   // per end
      | Open of receiveShut : bool
      | Reset of errorPending : bool
      | Closed
  ```

  - An end is write-shut exactly when its outbound direction's `Fin` is not
    `NotSent`. Nothing else holds that fact, and it survives the peer's close.
  - `Reset`'s `afterFin` is derived as the inbound FIN `Arrived` and the
    outbound FIN `NotSent`. That is `tcp_reset`'s own test: `EPIPE` only in
    `CLOSE_WAIT`, `ECONNRESET` in `CLOSING` and `LAST_ACK`.
  - `receiveShut` exists only on `Open`. A reset end is shut both ways
    anyway (`tcp_done` sets `SHUTDOWN_MASK`), and a closed one has no
    reader.
  - `passive` is recorded when the FIN is made, by `SHUT_WR` or by `close`,
    whether it then goes out or waits behind bytes. With the FIN's progress
    it decides whether the end keeps its endpoint once its socket has closed
    (2.7): the endpoint is released exactly when the end is `Closed` and
    its outbound FIN is `Arrived true`. An orphan whose FIN is `Queued true`
    releases it at the step that moves the FIN to `Arrived true`, which is
    a peer's read; `Arrived false` is `TIME_WAIT` and holds it. It cannot be
    derived later: once both FINs have arrived, nothing else records which
    was made first. Its invariant is that `passive` implies the opposite FIN
    is `Arrived`, since a FIN never un-arrives.
  - Only the byte-queue invariants stay in `violations`: `Queued` implies
    `Sending` is not empty while the receiver is `Open`; `Arrived` implies
    `Sending` is empty; `Reset` and `Closed` imply `Sending` is empty, on
    both flavours (see the next paragraph for what Darwin keeps instead).

  (A) admits a `Write` bit with no FIN opposite, a FIN opposite an end that
  has not shut writing (a check that must be weakened in exactly the case
  that motivates the bit), four representations of each `Reset` and `Closed`
  end, and a stored `afterFin` that disagrees with the FIN. (D) admits none
  of these. (B) admits all of them and more, and transcribing the kernels'
  `rcvShut` loses a distinction the model needs: both kernels set it on their
  own `SHUT_RD` *or* on the peer's FIN, while only Darwin's `SHUT_RD` flushes.

**Chosen: (D).** It replaces `TcpEndState` like (B), but each old case maps to
one new one (`Open` to `Open false`, `FinQueued` and `FinReceived` to the
direction's `Fin`, `Reset (a, e)` to `Reset e`, `Closed` to `Closed`). The
type is `internal`. It is stage 3's first commit, once #1793 and #1795 have
merged.

**What Darwin keeps of a dead direction's send buffer.** When a reset ends a
direction whose sender still has bytes waiting, Linux discards them and
Darwin keeps counting them in the sender's send buffer: R's `RDWR p-unsent`
row resets both ends and still reports `p`'s `EVFILT_WRITE` data as 0, and
the stack's "FIN, then written" row keeps the 100 bytes written (146888).
The stack holds those bytes in `Sending`, which contradicts "`Reset` implies
`Sending` is empty". Two ways out:

- **(i) A flavour-aware invariant.** On Darwin, `Sending` may be non-empty
  towards a reset or closed end. Every function that moves or counts bytes
  (`deliver`, `readable`, `readAnswers`, the arrival rules, `violations`)
  must then ask whether the direction is dead before treating `Sending` as
  bytes that will arrive.
- **(ii) Keep the count explicitly, as a Darwin-only fact.** `Sending` means
  bytes that can still be delivered, on both flavours. The bytes a reset
  strands are only a count against the sender's send space (they are never
  read), so they move out of `Sending` into the Darwin case of
  `TcpTransferRules`, `Darwin of stranded : Map<ConnectionEnd, int>`, keyed
  by the sender. That mirrors `Linux of spaceWakeArmed`, which already keeps
  Linux-only state where Darwin cannot construct it. An entry is removed when
  its sender closes, so equal states compare equal. `sendSpace` subtracts it,
  and `violations` checks that only a sender whose outbound direction is dead
  has an entry, and that it fits the send buffer.

**Chosen: (ii).** It keeps every invariant about `Sending` flavour-free and
every reader of `Sending` honest, it makes the Linux state unable to strand
anything, and it stores exactly what the measurements show is observable
(a count in `EVFILT_WRITE`'s data), not bytes nobody can read. The stack's
two Darwin paths that keep bytes in `Sending` (a reset by a close over unread
bytes, and a write after the peer's clean close) move to the count in the
same first commit.

Both kernels fold the peer's FIN into receive-shut (`tcp_fin` sets
`RCV_SHUTDOWN`; XNU calls `socantrcvmore`). The rules compute "receive-shut" once, as
`receiveShut` or the inbound FIN `Arrived`, and read that, rather than
restating it per row. That one fact explains Darwin's `ENOTCONN` to `SHUT_RD`
after a FIN, and Linux's HUP after `SHUT_WR` and then the peer's FIN.

The rules, as functions over (D), with each measured row as a test:

- **Read** with nothing queued: 0 once read-shut, on both. On Linux a pending
  error is answered first when no FIN has arrived (`tcp_recvmsg`'s order).
  Linux keeps the queued bytes, and Darwin's `SHUT_RD` empties `Receiving`.
- **Write** when write-shut: `EPIPE` + `SIGPIPE`, even for zero bytes. An end
  that is also reset keeps the stack's existing reset rules, which come first
  (on Linux, `sk_stream_error` answers a pending error before `EPIPE`).
- **Arrival** at a read-shut end: on Darwin, and on Linux once write-shut too,
  the arrival resets. The sender gets `Reset true`; whether its error reads
  as `EPIPE` or `ECONNRESET` follows from the FINs, as (D) derives it, so a
  sender that had itself shut writing gets `ECONNRESET` (measured on Linux,
  2.2's X row). The receiver gets `Reset true` on Linux, and
  `Reset false` on Darwin, whose shutter sees no error. Otherwise the
  arrival queues as now.
- **`shutdown` itself**: the 2.1 table as a pure function from (shut state,
  end state, `how`) to (answer, new state, wakes), including Darwin's
  half-by-half `SHUT_RDWR`.

### 3.2 Whether abortive close reuses the reset path

- **(a) Reuse `TcpTransfer.abort`.** `releaseDestroyed` calls `abort` instead
  of `close` when linger is {1, 0}, and so does the accept that drops a
  connection. `AbortiveClose` and `AbortiveDrop` are deleted. `closeWith`
  learns the new states:
  - a closer that shut writing first leaves the peer `Reset true`, whose
    error reads as `EPIPE` on Linux exactly when the closer's FIN had
    arrived, as (D) derives it;
  - a peer already reset just closes.

  Whether `abort` resets at all is a function of the two FINs (2.7), with
  the closer's outbound FIN `out` and its inbound FIN `in`:

  | `out` | `in` | `abort` |
  |---|---|---|
  | `Arrived _` | `Arrived _` | the exchange is complete: an ordinary `close`, no reset. On Darwin the closer's endpoint is released even if `out` is `Arrived false`; on Linux the ordinary rule holds. |
  | `Queued true` | `Arrived _` | reset |
  | `Queued false` | `Arrived _` | Linux: reset. Darwin: refused, because its reset waits on the peer's reads |
  | anything else | | reset |

  The two Darwin-specific facts belong to the flavour's rules, in the Darwin
  case of `TcpTransferRules`. Darwin's `EINVAL` to setting `SO_LINGER` on a
  socket shut both ways belongs to `setsockopt`, which already refuses
  Darwin's options after a reset for the same reason.

  Every resetting row of 2.7 is then an existing rule: what the peer keeps,
  what is discarded, the take rules, and the port and four-tuple release.
- **(b) A Linux-shaped `disconnect` operation that resets without closing**
  (`tcp_disconnect`), with an abortive close as "disconnect, then release".
  The same operation would serve Linux's listener `shutdown` and `connect`
  with `AF_UNSPEC` (`SystemNative_Disconnect`). But Darwin has no such
  operation (its `disconnectx` is a FIN), and nothing on the Kestrel path
  needs a socket to survive its own reset.
- **(c) Edit the connection directly in `ObjectLifetime`.** This is
  rejected: it would duplicate the reset rules that `TcpTransfer` keeps and
  checks.

**Recommend (a).** If `SystemNative_Disconnect` is ever modelled, (b) can be
built on top of `abort`.

### 3.3 How listener teardown with queued connections is modelled

- **(a) Abort each queued connection's server end as the listener goes.**
  This is `TcpTransfer.abort ConnectionEnd.Server`. The client gets
  `PeerReset`, and the queue goes. `ListenerWouldResetUnacceptedClient` is
  deleted. Linux's `shutdown(l, RD|RDWR)` reuses the same step and leaves the
  socket `Idle` with its binding, waking any parked `accept` with `EINVAL`.
  The listener's linger is ignored, as measured.
- **(b) Resolve the queued connections lazily**, leaving each one orphaned
  until its client next makes a call. No kernel does this. A client's
  registrations would see the reset late, or never.
- **(c) Keep refusing.** Kestrel's `StopAsync` closes the listener whatever
  its queue holds, so any schedule in which a client connects just before
  the stop would fail the run.

**Recommend (a).** The measurements give one outcome for linger off and on,
with or without data, on both flavours, and it is the outcome of the existing
reset rule.

One case stays refused. A queued client in `EstablishedPendingReport` is
reset before its connect is reported. Its later `connect` is already refused
(`ConnectRefusal.ResetBeforeReport`). Its `SO_ERROR` and `poll` answer the
reset, as they do for any reset.

### 3.4 `SO_LINGER` {1, t > 0}

- **(a) Refuse the close when it would wait**: on Linux whenever the closer
  has bytes in `Sending`, and on Darwin when it also has a blocking
  description. Otherwise, it is the ordinary close (2.8's first row).
- **(b) Model the wait**: park the close until the peer has read the bytes or
  the virtual clock passes the linger time. This needs a parked close, which
  nothing else has.
- **(c) Answer at once as if the peer had drained.** The bytes and the FIN
  come out the same, but a guest that times its close sees 0 instead of t.

**Recommend (a)**: it is out of scope for Kestrel, and refusing is honest.

### 3.5 Smaller choices

- **The syscall.** `UnixConnection.shutdown fd how` screens in the measured
  order: `EBADF`, `ENOTSOCK`, `EINVAL` (parsing `how` into a three-case DU),
  then the state. `SHUT_RD`, `SHUT_WR` and `SHUT_RDWR` are 0, 1 and 2 on both
  flavours, and so are the PAL's `SocketShutdown` values. Even so, the
  conversion goes in a `Native/*Pal.fs` adapter, because the shim answers
  `EINVAL` for any other value without making the call.
- **Wakes.** Add `TcpWake.ShutDown of shutter * how`. On Linux it becomes a
  wake with no key on the shutter's socket, as `PeerReset` does. On Darwin it
  activates READ for `RD` and WRITE for `WR`. The peer's FIN uses the existing
  `PeerFinished`, raised only when the FIN arrives.
- **Parked calls.**
  - `readAnswers` is true once read-shut.
  - `writeResumes` is true once write-shut, and the finish answers 2.5's row:
    on Linux the count taken, or `EPIPE` + `SIGPIPE` if none; on Darwin
    `EPIPE` + `SIGPIPE`.
  - A parked `accept` on a Linux listener that goes `Idle` finishes with
    `EINVAL`.
- **Readiness.** Linux's `socketReadinessLevel` takes the formula of 2.3.
  Darwin's `ofSocket` reports WRITE as EOF once write-shut, with the free space
  as data, however small. Darwin's existing `poll` derivation must give HUP
  alone for that, and a test row pins it.
- **The passive closer's port.** A FIN-closed end frees its endpoint when
  its socket has closed and its outbound FIN is `Arrived true`: made after
  the peer's had arrived, and itself arrived. Any other FIN-closed end holds
  it (2.7, section F). An orphan with a `Queued true` FIN releases its
  endpoint at the read by the peer that lets the FIN through, so
  `orphanedConnectionOccupies` reads the FIN's state each time rather than
  anything fixed at the close. Before `shutdown`, the difference could not
  be observed: the peer's FIN meant the peer had closed, and once both ends
  close the connection is gone. With `SHUT_WR`, the active closer stays
  open.

### 3.6 What the model deliberately does not reproduce

- **Darwin's `SHUT_RD`, or a `SHUT_RDWR` whose peer has shut writing, while
  the peer has bytes waiting** in its send buffer. Measured, `SHUT_RD` alone
  resets the peer only when a TCP timer fires (within 5 s, at a time that
  varied between runs; R `RD p-unsent`), and after `SHUT_RDWR` against a
  peer that had shut writing nothing happened for 15 s (X). Refused. `SHUT_RDWR` whose peer has not shut
  writing resets that peer at once, as measured (R `RDWR p-unsent`), and is
  modelled.
- **Linux's delay before a waiting byte reaches an end shut both ways.** The
  model moves waiting bytes as soon as there is room, so the reset comes at
  the read that makes room. The kernel advertises no window then, and the
  bytes arrive on a zero-window probe within about 250 ms (X), with the same
  outcome: both ends reset with `ECONNRESET`. Only a guest that reads after
  its own `SHUT_RDWR` while its peer's bytes wait, and watches the clock,
  can tell.
- **`SO_LINGER` {1, t > 0} with bytes unsent**: refused (3.4).
- **Darwin's linger {1, 0} close after its own FIN was made first and is
  still queued, once the peer's FIN has arrived.** No reset is sent at the
  close; the peer reads what it holds and then `ECONNRESET`, when its reads
  open the window towards a socket that has gone. The model has no
  "reset owed to whoever next reads". Refused.
- **`TIME_WAIT` after both ends have closed** stays unmodelled, as in the
  stack: the connection goes once nothing refers to it.
- **`SystemNative_Disconnect`** (Linux `connect` with `AF_UNSPEC`, which is
  abortive; Darwin `disconnectx`, which is a FIN). `.NET` calls it only from
  `TryUnblockSocket`, when a close races a synchronous call holding the
  handle. Kestrel's calls are asynchronous. Refused.

## 4. PR staging

Each stage is a PR, green on its own, stacked on #1795 after the stack has
been rebased onto main (with #1790).

1. **Abortive close of a connected socket.**
   - `releaseDestroyed` and the accept drop call `TcpTransfer.abort` under
     {1, 0}, and `AbortiveClose` and `AbortiveDrop` go.
   - `closeWith` handles a peer that is already reset. The shutdown states
     come in stage 3.
   - A close under {1, t > 0} that would wait is refused (3.4).
   - Tests: the reference model gains `abort`; L's rows without shutdown are
     replayed from the embedded probe output; a host-equality test covers L
     and G.
2. **Listener teardown.**
   - Releasing a listener aborts every queued connection, and
     `ListenerWouldResetUnacceptedClient` goes.
   - The same applies when a process ends with a listener open.
   - Tests: Q's rows, and a fuzz case that connects just before a close.

   Done. The resets go oldest first, which `listener-reset-order.c` (beside
   the probe) measured on both flavours. A Linux accept that held the
   listener's last reference releases it as it returns, resetting what is
   left queued, so `AcceptRefusal.Release` went too; and a process's end no
   longer holds its listeners back until its other releases are made, since a
   listener's release now raises wakes, in the order its descriptors close.
   Not done: section Q sets the listener's linger after its connections have
   queued, and `setsockopt` still refuses any option's change on a listener
   with connections queued (`SocketOptionRefusal.ListenerWithQueuedConnections`),
   because an accepted socket takes the options the listener had when its
   connection completed, and this kernel does not record them per
   connection. So Kestrel's {1, 0} close succeeds whatever is queued, but the
   `setsockopt` before it is refused when something is.
3. **Half-close in `TcpTransfer`, as pure functions** (3.1 (D)).
   - First commit: the split of `TcpEndState`, with behaviour unchanged.
   - Then the read, write and arrival rules, `shutdown`'s answer function,
     `closeWith` over the shut states, `abort` as 3.2's function of the two
     FINs, and `violations` reduced to the byte-queue invariants.
   - Tests: a property test against the reference model, extended with
     `shutdown`. S, T, R and P are replayed per flavour from the embedded output,
     as `TestTcpTransferMeasured` replays `tcp-transfer.c`, skipping every
     `~timing` line (section 2's table says why each is skipped) and
     comparing only answers and readiness bits on `~counts` lines.
   - No syscall uses any of it yet.
4. **`shutdown(2)` on connected sockets.**
   - The syscall and its screens, readiness, `TcpWake.ShutDown`, parked
     calls, and the passive closer's port.
   - Tests: a `TestShutdownAgainstHost` replays S, T, U (connected rows), R,
     P and E against the host's kernel, with the same exclusions, as `TestConnectedTransferAgainstHost`
     does, so that CI's x86-64 Linux checks the Linux column. B's rows go in
     kernel fixtures.
5. **`shutdown(2)` on a listener.** Linux returns the socket to `Idle`
   through stage 2's step and wakes parked accepts with `EINVAL`. Darwin
   answers `ENOTCONN`. Kestrel and `HttpClient` never make this call, so the
   stage can wait.
6. **PawPrint.**
   - `SystemNative_Shutdown` (the PAL `SocketShutdown` numbering through an
     adapter in `Native/`), and an explicit refusal of
     `SystemNative_Disconnect`.
   - `sourcesImpure` guests under both flavours, and then the Kestrel
     `StopAsync` and `HttpClient` disposal path between two processes.

Stages 1 and 2 need nothing from 3, and either can go first. Stage 6 can merge
with stage 4 if small.

## 5. Decisions

- **3.1: (D)**, the split of `TcpEndState` into a FIN per direction and an
  end state per end.
- **The X row** (2.2's last row): measured. On Linux X gets `ECONNRESET`,
  not `EPIPE`, as (D) derives. Darwin never delivers the bytes and never
  resets, so the model refuses that state (3.6).
- **Darwin's stranded send buffer (3.1):** (ii), a count in the Darwin rules
  case, not bytes in `Sending`.
- **Scope:** stage 5, `shutdown` on a listener, is deferred. Until it lands,
  that call is refused by name. Neither Kestrel nor `HttpClient` makes it.
- **The passive closer's port (3.5):** modelled in stage 4. A FIN-closed
  end's endpoint is released when it is `Closed` with its outbound FIN
  `Arrived true`, including later, when an orphan's queued passive FIN
  arrives. Not modelling it would answer `EADDRINUSE` to a `bind` that the
  kernel allows.
  **Still to measure, in stage 4:** Codex found on Darwin 27 that after
  `shutdown(p, SHUT_WR)` and then `shutdown(c, SHUT_WR)`, a fresh socket may
  bind `c`'s endpoint while both descriptors are still open. So the release
  may not need `Closed` at all, and may apply to a live socket's binding as
  well as to an orphan's. Stage 4 adds that ordering to section F, before
  either descriptor closes, measures it on both flavours, and states the rule
  from what it finds.
- **Abortive close (3.2):** a function of the two FINs. It is an ordinary
  close once both have arrived, and otherwise a reset, except that Darwin's
  close after an active FIN that is still queued and the peer's FIN has
  arrived is refused.
- **`SO_LINGER` {1, t > 0} (3.4):** refused when the close would wait.
