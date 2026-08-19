# Plan: `SystemNative_Socket`

## Goal

Implement `int32_t SystemNative_Socket(int32_t addressFamily, int32_t socketType,
int32_t protocolType, intptr_t* createdSocket)` (`pal_networking.c:2812`), the
entry point behind `System.Net.Sockets.SocketPal.CreateSocket`, so that a guest
constructing a `System.Net.Sockets.Socket` gets a real descriptor.

Measured today (worktree `socket-create`, at `57584369`): a guest doing
`new Socket(InterNetwork, Stream, Tcp)` reaches `SystemNative_Socket` and stops
there. Thread 1 is already parked in `BlockedOnSocketEvents`, so the
`SocketAsyncEngine` startup path (PR #1057, #1060, #1064) is behind us; this is
the next thing on the ladder.

Scope is *creation only*. Bind/Listen/Connect/Accept/Send/Receive/GetSocketType
are separate entry points and separate PRs.

## Measurements

All rows below were measured, not read off source. Linux is 6.18.5 aarch64
(Apple `container`, `docker.io/library/gcc:13`), euid 1000 and euid 0; Darwin is
25.6.0 arm64, euid 501. The full sweep is checked into the repository as
`WoofWare.PawPrint.Test/socketMatrix/{linux,darwin}.tsv` and is the oracle
`TestSocketCreation.fs` asserts against; the descriptor-operation probes below
were run from this session's scratchpad (`sockops.c`, `waitsock.c`, `errnos.c`).

### 1. Which triples create a socket

Sweeping the full PAL matrix — 6 address families x 5 socket types x 11 protocols
= 330 rows per platform — through a mirror of the shim's three conversion
functions and then `socket(2)`:

Creatable on **both** Linux (euid 1000) and Darwin:

| family | type | protocol |
| --- | --- | --- |
| `AF_INET` | `SOCK_STREAM` | `PT_UNSPECIFIED`, `PT_TCP` |
| `AF_INET` | `SOCK_DGRAM` | `PT_UNSPECIFIED`, `PT_UDP` |
| `AF_INET6` | `SOCK_STREAM` | `PT_UNSPECIFIED`, `PT_TCP` |
| `AF_INET6` | `SOCK_DGRAM` | `PT_UNSPECIFIED`, `PT_UDP` |
| `AF_UNIX` | `SOCK_STREAM` | `PT_UNSPECIFIED` |
| `AF_UNIX` | `SOCK_DGRAM` | `PT_UNSPECIFIED` |

Creatable on **Linux only** (Darwin answers EPROTONOSUPPORT, raw 43):

| family | type | protocol |
| --- | --- | --- |
| `AF_UNIX` | `SOCK_RAW` | `PT_UNSPECIFIED` |
| `AF_UNIX` | `SOCK_SEQPACKET` | `PT_UNSPECIFIED` |

Nothing else in the matrix succeeds unprivileged on either platform. Three
further rows succeed on Darwin but not Linux — `AF_INET`/`SOCK_DGRAM`/`ICMP` and
the two IPv6 ICMP equivalents — and those are Linux's *ping socket* path, gated
by the `net.ipv4.ping_group_range` sysctl rather than by anything PawPrint
models. 70 Linux rows change answer between euid 1000 and euid 0 (every
`SOCK_RAW` row and every `AF_PACKET` row: `CAP_NET_RAW`).

That privilege- and sysctl-dependence is the reason this plan refuses rather
than reports for the non-creatable rows: their real answers are facts about the
host's configuration, which is exactly what a deterministic replay must not
depend on.

### 2. Everything else a socket descriptor can be asked

On a fresh, unbound, unconnected socket (`O_NONBLOCK` set so a would-block shows
as `EAGAIN` instead of hanging):

| op | Linux (euid 1000) | Darwin |
| --- | --- | --- |
| `lseek` whence 0-4, any offset | `ESPIPE` | `ESPIPE` |
| `lseek` whence 9 | `EINVAL` | `ESPIPE` |
| `pread` / `pwrite` | `ESPIPE` | `ESPIPE` |
| `flock(LOCK_EX\|LOCK_NB)` | succeeds | `ENOTSUP` (45) |
| `ftruncate` / `fsync` | `EINVAL` | `EINVAL` |
| `fstat` `st_mode` | `S_IFSOCK\|0777` | `S_IFSOCK\|0666` |
| `fstat` `st_dev`/`st_ino` | sockfs dev, one inode per socket | 0 / 0 (`AF_INET`), real (`AF_UNIX`) |
| `fstat` `st_blksize` | 4096 | 131072 (TCP), 9216 (UDP), 8192 (unix) |
| `epoll_wait`/`kevent` on it | `EINVAL` (after `EFAULT` buffer screen) | `EBADF` |

`read`/`write` are the messy ones, and they are messy *per domain and kind*:

| socket | `read` Linux | `read` Darwin | `write` Linux | `write` Darwin |
| --- | --- | --- | --- | --- |
| `AF_INET` stream | `ENOTCONN` (107) | `ENOTCONN` (57) | `EPIPE` + `SIGPIPE` | `ENOTCONN` (57) |
| `AF_INET` dgram | blocks (`EAGAIN`) | blocks (`EAGAIN`) | `EDESTADDRREQ` (89) | `EDESTADDRREQ` (39) |
| `AF_UNIX` stream | `EINVAL` (22) | `ENOTCONN` (57) | `ENOTCONN` (107) | `ENOTCONN` (57) |

### 3. `flock` contention

Two separate `socket(2)` calls get **distinct** sockfs inodes (measured 4127 and
4130, `st_dev` 8), and `flock(LOCK_EX|LOCK_NB)` succeeds on both — they do not
contend. This is the row [[openfileobject-is-the-flock-key]] predicted and is
the *opposite* of `OpenFileObject.AnonymousInode`, where every epoll port and
`eventfd` in the process shares one inode and therefore does contend.

So a socket needs a per-socket identity in `OpenFileObject`. A payload-free
`OpenFileObject.Socket` would be wrong in a way a guest can see.

## Design decisions

### D1. Where does a socket's state live?

**Option A — in the open file description.**

```fsharp
/// Not an inode number. Never guest-visible: its only jobs are to keep two
/// sockets from contending on `flock` and to be what a future socket table
/// keys on. Sibling of `OpenFileDescriptionId`.
type SocketId = SocketId of int64

type SocketDescription =
    { Id : SocketId ; Domain : SocketDomain ; Kind : SocketKind ; Protocol : SocketProtocol }

// FileDescriptorRegistry.fs
type OpenFileTarget = ... | Socket of SocketDescription
type OpenFileObject = ... | Socket of SocketId
```

**Option B — a socket table in `EmulatedKernel`**, `Map<SocketId, EmulatedSocket>`,
with `OpenFileTarget.Socket of SocketId` naming into it.

**Option C — sockets as VFS inodes**, `InodeContent.Socket`, reusing
`OpenFileTarget.File`. This is what Linux literally does (sockfs), and it would
give per-socket `flock` identity with no new `OpenFileObject` case at all.

**Choosing A.** Today the correspondence is exactly 1:1 — one `socket(2)` call
makes one description naming one socket, and `dup` shares both. That is not a
coincidence but a theorem of the surface PawPrint models: there is no
`/proc/self/fd` reopen and no `SCM_RIGHTS` (which shares the description
anyway), so nothing can produce two descriptions onto one socket. So B's table
buys nothing here, and costs a second lifetime to manage: closing the last
descriptor would have to remove the socket from the table by hand, and
`checkInvariants` would need a whole reachability clause for it. Under A the
socket dies with its description, which the registry already does correctly.

B becomes the right shape when a socket must exist *without* a descriptor — a
completed connection in a listening socket's backlog — or when two sockets share
mutable state, as a connected pair's receive queues do: under A, reaching a
peer's queue would mean scanning the description table for its `SocketId`. Both
arrive in the same `connect`/`accept` tranche, which is three or more rungs
further up the ladder (socket -> setsockopt -> bind/connect). It is a contained
refactor — move the non-identity fields out of the target, leave the id in place
— and it is that PR's to do, informed by what it actually needs. Anything `bind`
accrues in the description before then moves with them.

**Rejecting C**, despite its fidelity to Linux. `OpenFileTarget.File` is the
classifier every existing match site uses to justify treating a descriptor as a
file: `LSeek` would call a socket *seekable*, and `Read`/`Write` would walk into
`VirtualFileSystem.tryGet`. Every one of those arms would need a content-kind
re-check, which is exactly the "classifier stops being truthful for its callers"
failure the architecture guidelines forbid. It is also flavour-wrong at the
root: measured, a Darwin `AF_INET` socket has `st_dev` and `st_ino` of 0 — it
has no inode to be.

Blast radius if A is wrong: one DU case and one move of three fields.
Reversible.

Rejected outright: `OpenFileObject.Socket of OpenFileDescriptionId`. It would
produce the right contention behaviour today, but only because the 1:1 holds —
it keys contention on the description rather than on the thing the kernel
contends on, and would silently start lying the moment that stops being true.
[[identity-key-not-observability]]. Note that no test in this plan can
*discriminate* against it; the choice rests on the argument, not on a
measurement.

### D2. How are domain/kind/protocol represented?

Three small DUs restricted to values that can actually appear on a live socket
(`SocketDomain = InterNetwork | InterNetworkV6 | Unix`, `SocketKind = Stream |
Datagram | Raw | SeqPacket`, `SocketProtocol = Unspecified | Tcp | Udp`), with
the *set of creatable triples* expressed once, as the classifier that
`SystemNative_Socket` consults. The product type over-approximates; the
classifier is the truth, and it is flavour-dependent (D1's Linux-only rows).

The triple is stored even though nothing in this PR reads it back. It is not
speculative generality: it is the information the guest supplied, and
`SystemNative_Socket` is the only place it exists. Discarding it would mean
`GetSocketType`, `Bind` and `Connect` each re-plumbing it. Storing it is three
fields, not an abstraction.

The stored protocol is the *PAL* value that passed the screen — not the platform
value the conversion produces, which can differ (`AF_INET6` with `PT_ICMP`
converts to `IPPROTO_ICMPV6`, `pal_networking.c:2604`), and not the kernel's
resolved protocol either (Linux reports `IPPROTO_TCP` from `SO_PROTOCOL` for a
socket created with protocol 0). The PAL value is what `SystemNative_GetSocketType`
must eventually report back, so it is the one worth keeping. Resolution is that
entry point's question and wants its own measurement; this PR does not answer
it, and the representation does not pre-empt it.

Because nothing in this PR reads the triple back, storing it is exactly the
shape [[state-writeback-needs-its-own-test]] warns about: a handler that swapped
two fields, or stored `Unspecified` unconditionally, would pass every
guest-level test here. Test 1 therefore asserts the triple round-trips per
field.

### D3. What does the handler do?

Faithfully, in the C's order:

1. `createdSocket == NULL` -> `Error_EFAULT`, and **no** store. (Non-null but
   naming no storage -> `failwith`, exactly as `SystemNative_CreateSocketEventPort`
   already does: the real code would create the descriptor and then SIGSEGV.)
2. PAL address-family conversion fails -> store `-1`, `Error_EAFNOSUPPORT`.
   Flavour-dependent: `AF_PACKET` (65536) and `AF_CAN` (65537) convert under
   Linux and screen out under Darwin, the C's arms being `#ifdef`-guarded.
3. PAL socket-type conversion fails -> store `-1`, `Error_EPROTOTYPE`. All five
   PAL types convert on both flavours, so this fires only for out-of-range
   values — which a guest reaches with `(SocketType)99`.
4. PAL protocol conversion fails -> store `-1`, `Error_EPROTONOSUPPORT`.
   Per-family lists, straight from the C. One of those lists is behind a
   *build-configuration* flag rather than a platform one: the `AF_CAN` arm is
   `#if HAVE_LINUX_CAN_H` (`pal_networking.c:2547`), a `check_include_files`
   probe (`configure.cmake:970`). PawPrint models it as **present**, which is
   what an official linux-x64 shim build has and what the measured matrix
   corresponds to. With it, `AF_CAN` converts `PT_UNSPECIFIED` and `PT_RAW` and
   anything else is EPROTONOSUPPORT; without it, every `AF_CAN` protocol would
   be EPROTONOSUPPORT. Stated so the F# constant is a claim someone made.
5. Triple creatable for this flavour -> mint a `SocketDescription`, take the
   lowest free fd, store it, `Error_SUCCESS`.
6. Otherwise -> `failwith` naming the triple and the flavour.

Steps 1-4 are pure C in the shim PawPrint *is*, so they are exactly knowable and
are modelled rather than refused. Refusing them would be refusing to implement
the function. This is the same split `SystemNative_FLock` and `SystemNative_LSeek`
already draw: the shim's own logic is modelled, the kernel's is modelled only
where measured.

Step 6 is where the kernel's answer would be. **Three different reasons live
there, and the refusal message must not conflate them**, because the first
person to hit one wants a reason that applies to their input:

- *Privilege-dependent* — every `SOCK_RAW` row and every `AF_PACKET` row.
  Measured, 70 Linux rows change answer between euid 1000 and euid 0; the real
  gate is `CAP_NET_RAW`, which PawPrint does not model at all.
- *Sysctl-dependent* — Linux's ping sockets (`AF_INET`/`SOCK_DGRAM`/`PT_ICMP`
  and the two IPv6 equivalents), gated by `net.ipv4.ping_group_range`.
- *Deterministic but unmodelled* — everything else, including
  `AF_INET`/`SOCK_STREAM`/`PT_UDP`, `AF_UNSPEC` with any type, and every
  `SOCK_RDM`/`SOCK_SEQPACKET` IP row. These have one answer per flavour and it
  was measured this session. They are refused not because the answer is
  unknowable but because it is *unmodelled*: the creatable set is the emulated
  kernel's declared protocol table, and a row outside it is a socket PawPrint
  has not decided how to be. A refusal is recoverable — a later PR can report
  the measured errno — whereas an errno reported now on a guess is a silent lie
  forever. [[prefer-crashing-over-documented-divergence]]

Note that "host-dependent" cannot be the dividing line on its own: whether
`AF_INET6` rows succeed is equally a fact about the host's kernel
configuration, and those rows *are* modelled. The line is "measured, stable,
and needed", and it is drawn deliberately rather than derived.

The three Darwin-creatable ICMP rows are refused for the same reason even under
the Darwin flavour, where they are unprivileged, deterministic and measured: a
raw-ish ICMP socket is a dead end without send/receive, and `SocketProtocol`
does not represent `PT_ICMP` at all, so the DU encodes the decision. Recorded
here so the classifier and the measurement table do not disagree silently.

The handler **never touches `LastSystemError`**. Every modelled path here is the
C's own pre-syscall screening, which sets no errno; only the unmodelled
`socket(2)` failures would, and those crash. (This differs from the `FLock` and
`Read` handlers, which do set it — do not copy from the wrong template.)

### D4. Every existing match site

Enumerated **by hand**, not delegated to the compiler: `SystemNative_LSeek` has
two wildcard matches on the target (around lines 2649 and 2694) that no
exhaustiveness warning will flag. Both are benign for a socket — with
`NotSeekable` on each flavour the `ordered` ladder answers before either is
reachable — but they had to be checked rather than assumed.

Answers:

| site | answer | why |
| --- | --- | --- |
| `FileDescriptorRegistry.createSocket` | `AccessMode = FileAccessMode.ReadWrite`, `Flock = None` | **Not cosmetic.** `SystemNative_Read` and `SystemNative_Write` answer EBADF from `permitsRead`/`permitsWrite` *before* they match on the target (NativeSystemNative.fs:2383 and ~3684), so anything narrower would shadow the refusal below with an EBADF no kernel gives — measured, a socket is readable and writable, the refusals being ENOTCONN/EINVAL/would-block. `PRead`'s Darwin unreadability tie-break consults it too. Same reasoning as the port's own `ReadWrite` (`FileDescriptorRegistry.fs:530`) |
| `OpenFileDescription.object` | `OpenFileObject.Socket id` | one identity per socket, so two sockets never contend (measurement 3) |
| `FileDescriptorRegistry.setOffset` | `failwith` | a socket holds no offset; caller should have answered `ESPIPE` |
| `checkInvariants` negative-offset clause | `None` | no offset to be negative |
| `checkInvariants` (new clause) | distinct descriptions never share a `SocketId`; every `SocketId` below `NextSocketId` | mirrors `NextIdNotFresh` |
| `SystemNative_PRead` / `PWrite` | `ESPIPE` both flavours | measured |
| `SystemNative_LSeek` | `DescriptorFault.NotSeekable` both flavours | measured; the existing Linux-orders-whence-first machinery then reproduces the `EINVAL`-for-whence-9 row for free |
| `SystemNative_Read` / `Write` | `failwith` | measurement 2: the answer varies by domain *and* kind, and every stream answer is really a statement about connection state that does not exist yet, so any constant becomes a lie the moment `connect` lands. Nor is anyone waiting on it: the BCL never reaches these with a socket fd — Unix socket I/O goes through `SystemNative_Send`/`Receive`, and `SafeSocketHandle` is not a `SafeFileHandle` — so this arm is hand-rolled-P/Invoke territory. Two facts for whoever does model it: measurement 2 was taken with `O_NONBLOCK` set, so the true unconnected-datagram-read answer is a *block with no wake source* rather than EAGAIN; and the Linux stream-write row's SIGPIPE is invisible to a .NET guest, CoreCLR having installed `signal(SIGPIPE, SIG_IGN)` process-wide (`src/coreclr/pal/src/exception/signal.cpp:244`), leaving plain EPIPE |
| `SystemNative_FStat` | `failwith` | same as the streams and the port: `st_dev`/`st_nlink`/`st_blksize` would all be invented, Darwin's `st_blksize` varies per socket kind, and Linux's identity fields are host facts. References issue #956 |
| `SystemNative_FLock` | Linux: permit through the registry (contention keyed on `SocketId`, so two sockets never conflict). Darwin: `refuseDarwin` | measured `ENOTSUP` (45), same divergence the port already refuses |
| `SystemNative_WaitForSocketEvents` (both flavour arms) | join the existing `StandardStream \| File` grouping | measured: Linux `EINVAL` behind the `EFAULT` buffer screen, Darwin `EBADF` — identical to those cases |

`SystemNative_Close` and `Dup` need no change: neither inspects the target.

## Tests

*(Written after implementation, recording what the tests turned out to be.)*

1. **`TestSocketCreation.fs` — the classifier against the measurement.** All 330
   PAL triples, as swept on real Linux and real Darwin, checked in as
   `socketMatrix/{linux,darwin}.tsv` and asserted row by row against
   `SimulatedUnixPlatform.socketCreation`. The correspondence is total: each
   measured row maps onto exactly one classifier answer, so a slip anywhere in
   the per-family protocol tables surfaces as a row whose screen fires on one
   side and not the other. The Linux flavour agrees on all 330; Darwin agrees on
   327, and the three it does not — the ICMP datagram sockets Darwin hands to any
   user — are asserted as a *closed set* with the reason recorded, so a fourth
   divergence fails rather than passing quietly. A separate case asserts that
   Linux really does refuse those same three, so the exception is about Darwin
   rather than about ICMP sockets being unmodelled everywhere. Also the
   per-field round-trip of the created triple.

2. **`sourcesPure/SocketCreateScreens.cs` — differential, 21 checks.** The three
   screens, their order (each pinned by a pair supplying two bad arguments at
   once), the `-1` stored through the out-parameter, the null-out-parameter
   EFAULT, the six sockets both platforms make for an ordinary user, and
   `lseek`/`pread`/`pwrite` unseekability. Only rows both kernels agree on.

   Reached by hand-rolled P/Invoke rather than through
   `System.Net.Sockets.Socket`: the managed path turns the returned
   `Interop.Error` into a `SocketError` through an `EnumEqualityComparer`, which
   needs the `RuntimeHelpers.EnumEquals` JIT intrinsic PawPrint does not
   implement. That is the next rung and is not this entry point's contract, so
   no test is parked for it.

3. **`sourcesImpure/SocketCreate{Linux,Darwin}.cs` — the flavour-split rows.**
   Linux: the Unix-domain `SOCK_SEQPACKET` and `SOCK_RAW` sockets Darwin refuses,
   an IPv6 socket (kept out of the differential test because IPv6 is a property
   of the host kernel), `lseek`'s Linux screen order, and — the guest observer
   for the identity decision in D1 — two sockets each taking an exclusive
   `flock`, which a payload-free `OpenFileObject.Socket` would refuse. Darwin:
   `AF_PACKET` and `AF_CAN` refused by the address-family screen, each with a
   protocol its Linux arm would accept, so it is the family screen being
   observed; plus Darwin's opposite `lseek` screen order.

4. **`TestFileDescriptorRegistry.fs`** — two sockets are two descriptions *and*
   two `flock` objects (the row that separates a socket from `AnonymousInode`);
   `dup` names the same socket; a fresh description is `ReadWrite` with no lock
   and carries its triple per field; closing the last descriptor destroys the
   socket; and the two new `checkInvariants` defects, each with a negative
   control.

**Mutation-tested.** Three mutants, all killed, and the kills are informative:
disabling the address-family screen is caught by both the matrix oracle and the
guest; *swapping* the family and type screens is caught **only** by the guest's
ordering pair (no measured row has two bad arguments, so the matrix cannot see
order); and dropping the `*createdSocket = -1` store is caught only by the
guest's out-parameter assertion. Each test is therefore doing work the others
are not.

## Explicitly out of scope

`Bind`, `Listen`, `Connect`, `Accept`, `Shutdown`, `Send`/`Receive`,
`GetSocketType`, `SetSockOpt`/`GetSockOpt`, `TryChangeSocketEventRegistration`,
`GetSocketAddressSizes`, `GetDomainSocketSizes`, and any notion of a bound
address, a peer, or a receive queue.

Note `SocketPal.CreateSocket` calls `Interop.Sys.SetSockOpt(fd, IPPROTO_IPV6,
IPV6_V6ONLY, ...)` immediately after creating an `AF_INET6` socket that is not
`SOCK_RAW`. So an IPv6 guest will stop at `SystemNative_SetSockOpt` rather than
completing — expected, and one of the next rungs.

Measured after implementing: a guest doing `new Socket(InterNetwork, Stream,
Tcp)` now gets past `SystemNative_Socket` and stops in
`EnumEqualityComparer<Interop.Error>.Equals`, on the `RuntimeHelpers.EnumEquals`
JIT intrinsic — `SocketPal.GetSocketErrorForErrorCode`'s lookup table. That is
the immediate next blocker on the managed path, and it is not socket work.
