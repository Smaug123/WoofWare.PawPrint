# Byte transfer on connected TCP sockets

## Goal

Two processes on one simulated machine, a Kestrel server and an `HttpClient`
client, exchange bytes over a loopback TCP connection
(`2026-10-07-multi-process-machine.md`, "after that"). The throwaway
`rung-i-spike` branch (`57f6b4e0`, `e7d20741`) showed that the single-process
version runs if a connection carries bytes at all. This plan covers what the
spike skipped: buffer capacity, short writes and `EAGAIN`, blocking, the
write-space wake, readiness with data queued, end of file, resets, `EPIPE` and
`SIGPIPE`, and how `read` differs from `recv`.

This document is the design and measurement phase only. Nothing here changes
the kernel.

## 1. What exists

**The connection.** `TcpConnection` (`InternetEndpoint.fs`) holds only
`ClientAddress` and `ServerAddress`. A socket refers to it from
`SocketPhase.Established` or `EstablishedPendingReport`; a listener's accept
queue refers to it before `accept(2)` mints the server end. Nothing records
which end a socket is: the spike inferred it by comparing the socket's binding
with the two addresses. `UnixMachineState.peerOpen` derives "the other end is
open" by scanning the socket table. A connection is removed once nothing refers
to it.

**Closing.** `ObjectLifetime` releases a closed socket and signals
`SocketWake.PeerFin` to each survivor. Every close is a FIN, because no
connection holds data. A listener that closes with an accepted-but-unqueued
client is refused (`ListenerWouldResetUnacceptedClient`), since the reset is
not modelled.

**Readiness.** `UnixMachineState.socketReadinessLevel` answers an established
socket with OUT while the peer is open, and the measured half-closed level
IN|OUT|RDHUP once it is not. Its `SeqPacket` arm throws: see section 4.
`LinuxReadiness.ofDescription` adds RDNORM with IN and WRNORM with OUT, and no
WRBAND on TCP. `DarwinReadiness.ofSocket` reports READ only once the peer has
gone (`EV_EOF`, data 0), and WRITE always, with data
`DarwinReadiness.sendBufferSpace`: the machine's `TcpSendSpace` rounded up to
whole loopback segments, 146988 over IPv4 and 146808 over IPv6
(`kevent-write-data.c`). Its docstring says "This kernel has no send path, so
nothing is ever queued".

**`TcpSendSpace`.** It sits on `UnixMachineState`, with defaults of 131072 on
Darwin and 16384 on Linux. Only Darwin reads it: `withTcpSendSpace` refuses
`Some` on Linux, because nothing would read it.

**Wakes.** `SocketWake` has `AcceptQueuePush` (an epoll wake keyed
IN|PRI|RDNORM|RDBAND, and a kqueue READ activation), and `ConnectResolved`,
`RefusalReset` and `PeerFin` (unkeyed). The epoll model is edge-triggered only
(`EpollCtlRefusal.LevelTriggered`).

**read and write.** In `UnixReadWrite.fs`, a socket in `Idle` or `Listening` is
answered by `UnconnectedSocketRules`. `Established`, `EstablishedPendingReport`,
`DatagramPeer` and `Refused` are refused with `UnmodelledSocketPhase`. The pipe
path already has short writes (`WriteAdmission.Transfer`), blocking writes that
take part of the buffer and sleep for the rest (`TransferThenSleep`,
`writeThenSleep`, `finishWrite`), blocking reads (`ReadOutcome.WouldBlock` with
`WakeCondition.PipeHasBytes`), and `EPIPE` with `SIGPIPE`
(`BrokenWriteTarget`). `PipeBuffer.fs` holds `ByteQueue`, an immutable FIFO of
the chunks it was given, with equality by content.

**recv and send.** The kernel has no `recv` or `send` syscall. PawPrint has no
handler for `SystemNative_Receive`, `SystemNative_Send` or
`SystemNative_GetBytesAvailable` (which is `FIONREAD`, and so
`Socket.Available`). `SystemNative_Read` and `SystemNative_Write` refuse a
socket, noting that the BCL reaches sockets only through `Receive` and `Send`.
The shim's `Receive` and `Send` pass `MSG_PEEK`, `MSG_DONTWAIT`, `MSG_OOB`,
`MSG_TRUNC`, `MSG_CTRUNC`, `MSG_DONTROUTE` and `MSG_ERRQUEUE` through and
answer `ENOTSUP` for anything else, so `MSG_NOSIGNAL` never arrives from the
BCL. Both retry on `EINTR`, and Darwin's `Send` also retries `EPROTOTYPE` up to
three times. The runtime ignores `SIGPIPE`.

## 2. Measured facts

**Probe.** `2026-10-07-tcp-byte-transfer/tcp-transfer.c`, with each flavour's
output beside it:

- `tcp-transfer.linux-6.18.5-aarch64.txt`: Linux 6.18.5 aarch64 in Apple's
  `container` VM, run as root. The sysctls are at their defaults: `tcp_wmem`
  4096 16384 4194304, `tcp_rmem` 4096 131072 9042912, `tcp_autocorking` 1.
- `tcp-transfer.darwin-27.0.txt`: Darwin 27.0.0 arm64 (macOS 27.0.1), uid 501.
  `net.inet.tcp.sendspace` and `recvspace` are 131072, and `autorcvbufmax` and
  `autosndbufmax` are 4194304.

No x86-64 kernel was measured by hand. **`TestConnectedTransferAgainstHost`**
replays the S section (states × operations) against the host the suite runs on,
checking the answers and `poll`'s `revents` and `FIONREAD` before and after.
So Darwin's column is checked locally, and Linux's on CI's x86-64. It leaves
out the send-full state, which neither kernel holds still (below). Breaking one
recorded Darwin row was confirmed to fail it, in both the readiness check and
the answers check.

The Linux S section was identical across three runs, and so was the Darwin
section except for the send-full rows. The C section repeats every
configuration three times.

### 2.1 How much a writer can queue (section C)

The client writes non-blocking until `EAGAIN`, and the accepted end never reads.

**Linux, defaults.** `SO_SNDBUF` reads 3939840 straight after `connect`, not
16384, because the handshake autotunes it. During the fill it grows to 4194304
(`tcp_wmem[2]`). `SO_RCVBUF` reads 131072. The writer takes these totals before
`EAGAIN`:

| write size | 1 | 7 | 100 | 1000 | 1448 | 4096 | 16384 | 65536 | 1 MiB |
|---|---|---|---|---|---|---|---|---|---|
| bytes taken | 4023167–4036626 | 4025853 | 3915972 | 3924720 | 3905104 | 3959168 | 3931136 | 3948160 | 3910251 |
| reader's `FIONREAD` | ~127000 | 128253 | 80708 | 90864 | 66896 | 85120 | 86912 | 113024 | 127717 |

At these defaults the totals are deterministic across trials for writes of 7
bytes or more. (With `TCP_NODELAY`, writes of 7 and 100 bytes vary by under
1%.) They **depend on the write size**, by about 3% at these defaults and by up
to 36% with small buffers. With `SO_SNDBUF` set to 16384 (it reads 32768, and
autotuning stops), the totals are 80776, 91064, 66896, 85120, 86912 and 80229
for writes of 100, 1000, 1448, 4096, 16384 and 65536 bytes. `SO_MEMINFO` shows
why. The send queue is accounted in skb truesize (`wmem_queued`), which runs
about 2% above the payload at the defaults. It is much higher when the receive
window cuts the segments small: with the reader's `SO_RCVBUF` at 4096 (reads
8192), 2.81 MB of payload fills 3.94 MB of `wmem_queued`.

**Both buffers matter.** The writer's `SO_SNDBUF` bounds the send queue. The
reader's `SO_RCVBUF` bounds the receive queue, and it also changes how much
payload the send queue holds. **Time matters too**: 100 to 500 ms after
`EAGAIN`, with nobody reading, the Linux writer takes 37 KB to 380 KB more (the
`later(...)` column).

**Darwin, defaults.** `SO_SNDBUF` reads 146988, as `kevent-write-data.c`
found. The accepted socket's `SO_RCVBUF` reads 408300 (25 × 16332, unexplained).
The totals are **not repeatable**:

- three trials of 65536-byte writes took 319020, 629640 and 629640;
- writes of 100–1448 bytes took 540000–556000;
- 1-byte writes took 556546–749491.

After `EAGAIN`, more is taken within 10 ms without any read. With `SO_SNDBUF`
at 4096 or 16384 (65328 after the handshake), a large write takes exactly 65328
and then answers `EAGAIN`. Then **every 10 ms another 65328 drains**. Darwin's
loopback delivers on another thread, so the send buffer empties asynchronously.
No capacity on Darwin is a function of the writes alone. The typical small-write
total, about 550000, is close to 146988 + 408300 = 555288.

**Short writes.** Linux takes any positive remainder: the first short write was
6 of 7 bytes, and 72 of 100 (4 of 100 with a small receive buffer). Darwin never takes fewer than 2048 bytes (the send
low-water mark) unless that is the whole write. Writes of 1000 bytes or fewer
were never short, and the smallest short write was 3160. A 1 MiB write on a
fresh connection is whole on Linux (three of them are whole, and the fourth
takes 764523). On Darwin it is short, taking 490532–629224.

### 2.2 Readiness in each state (section S)

`s` is the connecting socket and `p` the accepted one. The states:

- **idle**: connected, nothing sent either way;
- **data in**: `p` wrote 1000 bytes;
- **send full**: `s` wrote to `EAGAIN`;
- **FIN**: `p` closed, with or without data for `s` to read first;
- **reset**: `p` closed with unread data, or with `SO_LINGER` {1, 0};
- **FIN, then written**: `p` closed, then `s` wrote 100 bytes, which `p`'s
  kernel answers with a reset.

**Linux** (`poll`'s `revents` and level-triggered `epoll` agree on every row):

| state of `s` | revents |
|---|---|
| idle | OUT\|WRNORM (0x104) |
| data in | IN\|OUT\|RDNORM\|WRNORM (0x145) |
| send full | 0 |
| FIN (unread data or none) | IN\|OUT\|RDNORM\|WRNORM\|RDHUP (0x2145), which is today's half-closed level |
| reset, error pending | 0x2145\|ERR\|HUP (0x215d) |
| reset, error taken | 0x2145\|HUP (0x2155) |
| FIN, then written | 0x215d, with `SO_ERROR` = **EPIPE** |

**Darwin.** Its `poll` is derived from these filters by the existing Darwin
`poll` machinery. That gives 0x104 idle and 0x145 with data. For FIN and reset
it gives 0xd3 (IN|PRI|HUP|RDNORM|RDBAND), because HUP suppresses OUT. The
kqueue filters, level-triggered:

| state of `s` | `EVFILT_READ` | `EVFILT_WRITE` |
|---|---|---|
| idle | not ready | data 146988 |
| data in | data = bytes unread | data 146988, since the bytes left the send buffer |
| send full | not ready | not repeatable: usually ready again within 20 ms |
| FIN | `EV_EOF`, data = bytes unread, fflags 0 | data 146988, no `EV_EOF` |
| reset | `EV_EOF`, data = unread, fflags `ECONNRESET` (54) until `SO_ERROR` takes it | `EV_EOF`, fflags 54 likewise |
| FIN, then written | as reset, `SO_ERROR` = **ECONNRESET** | `EV_EOF`, data 146888: the 100 bytes stay in the send buffer |

### 2.3 What each call answers (section S)

Each call was made three times in a row, 20 ms apart. "Then" means a later
call. "Takes" means the call clears the pending error.

| state | `read` / `recv` | `read(0)` | `recv(0)` | `recv(MSG_PEEK)` | `write` / `send` | `send(MSG_NOSIGNAL)` |
|---|---|---|---|---|---|---|
| idle | EAGAIN | 0 | **L EAGAIN, D 0** | EAGAIN | 100 | 100 |
| data in | 1000, then EAGAIN | 0 | 0 | 1000, and does not consume | 100 | 100 |
| send full | EAGAIN | 0 | L EAGAIN, D 0 | EAGAIN | L EAGAIN, then 100 after 20 ms; D not repeatable | as write |
| FIN, unread data | 1000, then 0 | 0 | 0 | 1000 | 100, then EPIPE + SIGPIPE | 100, then EPIPE, no signal |
| FIN, drained | 0 | 0 | 0 | 0 | as above | as above |
| reset | ECONNRESET (takes), then 0 | **L 0 (does not take); D ECONNRESET (takes)** | ECONNRESET, then 0 | **L ECONNRESET (takes), then 0; D ECONNRESET every time (does not take)** | **L ECONNRESET (takes, no signal), then EPIPE + SIGPIPE; D EPIPE + SIGPIPE every time (does not take)** | L ECONNRESET, then EPIPE; D EPIPE |
| reset, data unread | 1000, then ECONNRESET, then 0 | 0 (neither takes) | 0 | 1000, 1000 | as reset | as reset |
| FIN, then written | **L 0 every time (the earlier FIN wins, and the EPIPE stays pending); D ECONNRESET, then 0** | L 0; D ECONNRESET, then 0 | L 0; D ECONNRESET, then 0 | L 0; D ECONNRESET | EPIPE + SIGPIPE on both (L takes its EPIPE) | EPIPE |

The write rows hold for zero-length writes too, once there is an error. On both
flavours `write(0)` in a reset state answers like `write` (L ECONNRESET, then
EPIPE; D EPIPE), with `SIGPIPE` alike. Before any error, `write(0)` and
`send(0)` answer 0 and send nothing. In particular, they provoke no reset after
a FIN.

`MSG_DONTWAIT` on a blocking socket behaves as `O_NONBLOCK` does, on both:
`recv` answers EAGAIN, and a 65536-byte `send` is taken whole when idle. Darwin
honours `MSG_NOSIGNAL` (0x80000) as Linux does (0x4000).

Linux's rules all follow from `sk_stream_error`, from `sock_error` taking
`sk_err`, and from `tcp_recvmsg` testing `SOCK_DONE` before `sk_err`.
`tcp_reset` sets `sk_err` to EPIPE in `CLOSE_WAIT`, and to ECONNRESET
otherwise. Darwin's `soreceive` takes `so_error`, except under `MSG_PEEK`, and
its `sosend` answers EPIPE once `SS_CANTSENDMORE` is set, without reading
`so_error`.

### 2.4 Which transfers wake an edge-triggered waiter (section E)

**Linux** (`EPOLLET`):

- Every arrival of data queues an IN registration, even when bytes are already
  waiting. Its key is IN|PRI|RDNORM|RDBAND, as `AcceptQueuePush`'s is. A
  registration for IN|OUT is reported 0x5.
- Reading part of the queue wakes nothing.
- A writer that never met `EAGAIN` gets **no OUT edge**, either from its own
  write or from the peer's read.
- After `EAGAIN`, exactly one OUT edge comes. It arrives when the send queue
  falls to two thirds of `SO_SNDBUF`: in the W section it was not writable at
  `wmem_queued` 2853312 and writable at 2756672, of 4194304. This is
  `sk_stream_is_writeable`, armed by `SOCK_NOSPACE`, which a failed write or a
  `poll` that found the socket unwritable sets.

**Darwin** (`EV_CLEAR`):

- Every arrival queues READ, with data the bytes unread. Reading part of them
  queues nothing.
- WRITE fires **after the socket's own write**, once loopback has acknowledged
  it (data back to 146988). A peer's read that frees no send space queues
  nothing. After a fill, WRITE fires either within 20 ms on its own (one run)
  or after the peer had read 290816 bytes (another). The rule by XNU's source
  is: activated on each acknowledgement that frees space, and ready when
  `sbspace` is at least the low-water mark of 2048.

### 2.5 Blocking transfers (stage 5)

**Probe.** `2026-10-07-tcp-byte-transfer/tcp-blocking.c`, with each flavour's
output beside it, measured as `tcp-transfer.c` was, twice on each.

- **A sleeping read** returns what a read made then would: the bytes (it needs
  only one), 0 at a FIN, and ECONNRESET at a reset, which it takes. Several
  readers on one socket all wake and race (Linux returned the last to park
  first, mostly; Darwin the first, mostly; neither always).
- **A sleeping write** returns only once all its bytes are taken. Linux's
  writer, asleep on a full queue of 4194304, took nothing while the reader
  drained it to about two thirds (2765702), then refilled it: it is woken by
  `sk_stream_write_space`, as an edge-triggered waiter is, and then takes all
  the room. Darwin's took the room as each read made it.
- **A signal** ends a read with nothing to answer, and a write that has taken
  nothing, with EINTR, or restarts it under `SA_RESTART`; a write that has
  taken some returns that count, either way. On Linux, held off the CPU until
  both had happened, a reader with bytes answered them (40 of 40), and a writer
  with room took none of it and returned its count (40 of 40), whichever came
  first. Darwin answers whichever reached the sleeper first, which is refused.
- **A reset** ends a sleeping write: on Linux with the count taken, leaving
  ECONNRESET pending, or, with nothing taken, ECONNRESET, taken, and no
  SIGPIPE; on Darwin with EPIPE and SIGPIPE whatever was taken, leaving
  ECONNRESET pending.
- **Closing the descriptor** a sleeping call was made through ends it with
  EBADF on Darwin, whatever a write had taken, and raises nothing; on Linux the
  call sleeps on, holding the socket, and answers what arrives.
- `SO_RCVTIMEO` and `SO_SNDTIMEO`, which would bound the sleeps, cannot be set:
  `setsockopt` refuses every option it does not model.

What is not measured, and refused: a woken call that finds nothing to do
(beaten to the bytes or the room) through a description made non-blocking
while it slept, which Linux's source would leave asleep and Darwin's would
end; and a Darwin close of the descriptor of a call something has already
woken.

### 2.6 `recv` and `send` (stage 6)

**Probe.** `2026-10-07-tcp-byte-transfer/tcp-recv-send.c`, with each flavour's
output beside it, measured as `tcp-transfer.c` was, twice on each.

- **The flag word** is numbered per flavour, but for `MSG_OOB`, `MSG_PEEK` and
  `MSG_DONTROUTE`: `MSG_DONTWAIT` is 0x40 on Linux and 0x80 on Darwin, and
  `MSG_NOSIGNAL` 0x4000 and 0x80000.
- **Order.** Linux screens the buffer before it looks up the descriptor
  (`import_ubuf` comes first in `__sys_recvfrom` and `__sys_sendto`), so
  `(void*)-1` is EFAULT ahead of EBADF and ENOTSOCK, at length 0 too. Darwin
  screens nothing. A descriptor that is not a socket is ENOTSOCK on both.
- **`MSG_PEEK` blocks** like a read: it sleeps until there is something to
  answer, then answers it without taking the bytes; asleep at a FIN it is 0,
  and at a reset ECONNRESET, taken on Linux only, as 2.3 found for a peek
  that does not sleep.
- **`recv(0)` sleeps on Linux**: a blocking one with nothing queued returns 0
  only once bytes arrive (or a FIN). Darwin's returns 0 at once.
- **`MSG_DONTWAIT`** makes a `recv` non-blocking on both, and a Linux `send`,
  which also arms the send-space edge as `O_NONBLOCK` does. **Darwin's `send`
  ignores it**: on a full socket it sleeps, and returns only once its bytes
  are taken.
- **`MSG_NOSIGNAL`** suppresses Darwin's `SIGPIPE` from a `send` asleep when
  its connection is reset; Linux raises none there in any case. It changes
  nothing about a `recv`.
- **Darwin's `send` marks no description written** (`FWASWRITTEN`), where its
  `write` does.

The BCL reaches `SystemNative_Receive` only from an asynchronous receive and
from a receive on a socket whose `Blocking` is false: a synchronous receive on
a blocking socket goes through `SystemNative_ReceiveMessage` (`recvmsg`), which
is not part of this plan. `Send` reaches `SystemNative_Send` either way.

## 3. Design options

### 3.1 Which end of the connection a socket is

- **(a)** `SocketPhase.Established of ConnectionId * ConnectionEnd`, where
  `ConnectionEnd = Client | Server`. `connect` and `accept` know the end when
  they make the phase.
- **(b)** Infer it from the binding, as the spike did. This is wrong for a
  socket connected to its own address, and it fails loudly on any mismatch.
- **(c)** Record each end's `SocketId` on `TcpConnection`. `TcpConnection`'s
  docstring already rejects this, because the server end has no socket until
  `accept`, so the field would dangle or be `None` most of its life.

**Recommend (a).** The compiler then asks every site that builds the phase
which end it is. It is a mechanical change to every match on
`Established`/`EstablishedPendingReport`.

### 3.2 Where the bytes and the capacity live

Data must be storable **before `accept`**: `HttpClient` writes its request as
soon as `connect` returns, and Kestrel accepts later. That puts the bytes on
the connection, not on a socket.

- **(A) One queue per direction**, with one budget for the bytes the sender has
  written and the receiver has not read. This is simplest, but `FIONREAD`
  (`Socket.Available`) would then read up to the whole budget: up to 4 MB on
  Linux, where the kernel reports at most about 128 KB.
- **(B) Two stages per direction**: the sender's send queue and the receiver's
  receive queue, each with its own capacity. A write moves its bytes into the
  receive queue as far as that has room, and leaves the rest in the send queue.
  A read moves bytes on from the send queue as it makes room. Loopback has no
  latency, and nor does the model, so the move is immediate. This reproduces
  `FIONREAD` bounded by the receive buffer, `SO_SNDBUF` and `SO_RCVBUF` as
  separate knobs, and Darwin's WRITE activation "when the write is
  acknowledged", which is the moment bytes leave the send queue.
- **(C) Buffers on the sockets**, as the real kernels hold them. The
  pre-`accept` case rules this out, because there is no server socket to hold
  the receive queue.

**Recommend (B), held on the connection per direction.** Each direction is a
pair of `ByteQueue`s (moved out of `PipeBuffer.fs` into a file of its own, and
given a `peek`), plus the two capacities. The capacities are copied from the
flavour's defaults when the connection completes. A later `SO_SNDBUF` or
`SO_RCVBUF` (section 4) changes them in place. **Reversibility:** (A) is (B)
with a receive capacity of infinity, so if (B) proves needless the change is
internal to one module.

### 3.3 Bytes or segments

Linux's capacity depends on the write size, through skb truesize (section 2.1).

- **(A) Count bytes.** Choose the capacities so that the default totals land
  inside the measured range.
- **(B) Emulate Linux's skb accounting**: `size_goal`, tail coalescing, the
  truesize of each skb, and autocorking. This would reproduce the
  deterministic Linux rows for a fixed write sequence. But Linux also frees
  space on a timer (the `later` column), so even this would match only a
  prefix, and Darwin cannot be matched at all.
- **(C) Count bytes plus a fixed overhead per write.** This reproduces the
  direction of the effect, not the numbers.

**Recommend (A).** These are the proposed capacities, each derived from a named
configuration value rather than a magic number:

| flavour | send | receive | total | measured |
|---|---|---|---|---|
| Linux | 4194304, `tcp_wmem[2]`, which autotuning reaches | 131072, `tcp_rmem[1]` | 4325376 | 3.90–4.04 MB |
| Darwin | 146988, `TcpSendSpace` after the handshake's rounding, as `sendBufferSpace` already computes | 408300, measured | 555288 | typical ~550000, range 319020–749491 |

Linux's send capacity is then a new `KernelConfig` sysctl (the `tcp_wmem`
triple). The existing `TcpSendSpace`'s refusal on Linux is lifted, or the field
is split per flavour. That is a choice for the implementing PR.

**What the model deliberately does not reproduce, and how a guest could see it:**

- **The exact `EAGAIN` point and short-write sizes.** A guest writing until
  `EAGAIN` and printing the total sees 4325376 on Linux where the kernel gives
  3.90–4.04 MB. No portable program depends on this, since Darwin's total is
  not even repeatable.
- **Space freed by time alone.** On a real kernel, a writer that sleeps after
  `EAGAIN` can write again with no reader. The model frees space only when the
  peer reads. A guest that sleeps and retries would see `EAGAIN` for ever where
  a real kernel makes progress. That is a liveness difference, but the guest
  was relying on timer behaviour that no specification promises.
- **Darwin's asynchronous drain.** The model behaves as if the drain thread had
  finished at once. That is one of Darwin's possible schedules, so its answers
  are among Darwin's.
- **`FIONREAD` under small receive windows.** Linux's receive queue can sit
  below `SO_RCVBUF`, because the window, not the buffer, is the limit (35 KB
  with 1000-byte writes). The model fills it to the receive capacity.

Darwin's short-write rule *is* reproduced: take everything if it fits;
otherwise take the space if it is at least 2048; otherwise answer `EAGAIN`. So
is its WRITE readiness, "space ≥ 2048, data = space". Linux's writability is
"send queue ≤ ⅔ of send capacity" in bytes. That has the measured shape, though
the threshold falls at a different byte count.

### 3.4 How a short write is reported

- **(a)** Reuse `WriteAdmission.Transfer of count`, where `count` is the part
  taken, and `TransferThenSleep` for a blocking write that takes part of its
  buffer now and sleeps for the rest. This is exactly the pipe's contract.
- **(b)** A socket-specific admission type. It would duplicate the pipe's
  contract, with a different name for each client to learn.

**Recommend (a).** What is socket-specific is only the rule that decides the
count: Darwin's low-water mark, and Linux's "any positive remainder". The
blocking resume rule needs its own measurement: how much each wake writes, and
whether Darwin's `sosend` loops in low-water-mark-sized pieces. That
measurement belongs to the blocking-write PR.

### 3.5 How the write-space wake reaches waiters

- **(a)** Add `SocketWake` cases. `DataArrived` is keyed IN|PRI|RDNORM|RDBAND
  for epoll and activates READ for kqueue. `SendSpace` is keyed
  OUT|WRNORM|WRBAND (`sk_stream_write_space` passes those) and activates WRITE.
  Each transfer raises them by these rules:
  - **Linux** raises `DataArrived` whenever bytes enter a receive queue. It
    raises `SendSpace` only when the sender's direction carries a
    `SendNoSpace` flag (set by an `EAGAIN` write, a write that parks, and a
    `poll`, epoll `ADD` or `MOD` that found the socket unwritable) and the
    queue has fallen to ⅔. Raising it clears the flag.
  - **Darwin** raises `SendSpace` whenever bytes leave a send queue, and the
    activation then reports only if `space ≥ 2048`.
  - Parked blocking readers and writers wake through new `WakeCondition`
    cases, as pipes' do.
- **(b)** Recompute readiness for every registration after every transfer and
  signal on any change of level. This is simpler, but wrong on both flavours:
  Linux gives a new IN edge for each arrival even when the level does not
  change, and gives no OUT edge to a writer that never met `EAGAIN`.

**Recommend (a).** The `SendNoSpace` flag is per direction, so it lives beside
the queues. It is Linux-only state, so it should be an option or a DU that
Darwin cannot construct, not a `bool` that Darwin ignores.

### 3.6 How a close ends a direction

These follow from the measurements, given 3.2:

- **A close with nothing unread** in the closer's receive queue, and nothing in
  flight towards it, is a **FIN**. The closer's send queue keeps draining to
  the peer as the peer reads, and the peer reads end of file after the data.
- **A close with unread data** is a **reset**. The peer keeps what is already
  in its receive queue (Linux and Darwin agree: data, then ECONNRESET, then 0).
  Its in-flight send queue is discarded on Linux and stays counted on Darwin
  (the WRITE data of 146888). The peer gets a pending error and a
  can't-send-more mark.
- **A write after the peer's FIN** is taken whole and discarded. The direction
  then goes to reset, with pending error EPIPE on Linux and ECONNRESET on
  Darwin.
- **`SO_LINGER` {1, 0}** resets. It is out of scope until `SO_LINGER` exists;
  it belongs with `shutdown`.

The pending error is a slot per receiving end, like `RefusalError`, with the
take rules of 2.3. To keep illegal states out, the end's lifecycle is one DU:
open, FIN received, or reset (with the error pending or taken). The take rules
are a function per flavour, tested against the measured table.

## 4. The `SOCK_SEQPACKET` refusal (audit finding)

`socketReadinessLevel` throws for an `AF_UNIX SOCK_SEQPACKET` socket. Two
callers reach it from a guest: `epoll_ctl(ADD)`, whose screen admits any
socket, and Linux `poll`. Both crash the run. **Proposal:**

- add `EpollCtlRefusal.UnmeasuredSocketKind of targetFd * domain * kind`,
  screened at `ADD`;
- extend `PollRefusal.UnmodelledSocket` (today Darwin-only) to Linux for that
  kind.

The `SeqPacket` arm of `socketReadinessLevel` then becomes unreachable, and
says so, rather than being reachable with a crash. Its poll row is measured
(OUT|HUP|WRNORM|WRBAND). It could be answered once an epoll measurement exists,
but answering only `poll` would split the one level the two waiters share.
This is independent of everything else here, and can go first.

## 5. Staging

Each stage is a PR, green on its own. Stages 4 onwards touch `UnixWait.fs` and
`Readiness.fs`'s kqueue activation, which `mp-4-cross-process-wakes` is
changing, so they wait for that to merge.

1. **The `SOCK_SEQPACKET` refusal** (section 4).
2. **`ConnectionEnd` on the established phase** (3.1). Mechanical, no
   behaviour change.
3. **Buffers on the connection, and the transfer rules as pure functions.**
   Move `ByteQueue` and add `peek`. Add a `TcpDirection` with its two queues,
   its capacities, its end state and its pending error. Write the
   per-flavour rules (how much a write takes, what a read returns, which wakes
   fire, the take rules) as functions over that record. Property-test them
   against a naive reference (a `byte list` with integer budgets), and against
   every S row of section 2.3, replayed from the embedded probe output. No
   syscall uses them yet.
4. **Non-blocking `read` and `write` on an established socket**, with readiness
   (`socketReadinessLevel`, `DarwinReadiness.ofSocket`) and the
   `DataArrived`/`SendSpace` wakes. FIN and reset come from `close`, which
   becomes data-aware. `TestConnectedTransferAgainstHost` gains the kernel as a
   third column. A blocking read or write is still refused here.
5. **Blocking `read` and `write`**: `WakeCondition` cases, park and finish, and
   `TransferThenSleep` for sockets. Measure the resume rule first. Done:
   section 2.5 has the measurements.
6. **`recv` and `send` in the kernel**, with `MSG_PEEK`, `MSG_DONTWAIT` and
   `MSG_NOSIGNAL`. Every other flag is refused. This includes `recv(0)`'s
   divergence from `read(0)`. Done: section 2.6 has the measurements.
7. **PawPrint handlers**: `SystemNative_Receive` and `SystemNative_Send` (PAL
   flags through a `SocketFlagsPal` adapter in `Native/`),
   `SystemNative_GetBytesAvailable` for sockets, and socket arms for
   `SystemNative_Read` and `SystemNative_Write` (reached only by hand-rolled
   P/Invokes). Wiring guests go in `sourcesImpure`, under both flavours.
8. **`SO_SNDBUF` and `SO_RCVBUF`**, with Linux's doubling and lock, and
   Darwin's handshake rounding (already measured in `kevent-write-data.c`
   section B). They come last because nothing on the Kestrel path sets them.

`shutdown`, `SO_LINGER`, `TCP_NODELAY` and `getpeername` remain the separate
stages that the multi-process plan lists.

## 6. Decisions

- **Capacities (3.3): derived from the configuration that bounds them.** Linux's
  are 4194304 (`tcp_wmem[2]`) plus 131072 (`tcp_rmem[1]`), about 7% over the
  measured totals. An admin can change a sysctl, so by the
  `emulated-posix-kernel` skill's test it is machine configuration, not a
  platform fact. A measured constant such as 3948160 would match one write size
  and nothing else, while looking more exact than it is.
- **Time-freed space (3.3): not modelled.** The model frees send space only when
  the peer reads. This is a stated non-reproduction (see above). No
  specification promises the timer release, and modelling the TCP stack's
  zero-window probes and collapsing would be a design job of its own, for no
  guest on the Kestrel path.
- **Darwin's `SO_RCVBUF` of 408300.** Stage 3 explains it from XNU's source
  before relying on it, as `kevent-write-data.c` explained the send side. If it
  cannot be explained, stage 3 keeps it as a measured constant and says so
  beside it.
