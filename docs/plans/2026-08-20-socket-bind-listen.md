# Plan: a socket acquires a local address — `Bind`, `Listen`, `GetSockName`

Revision 3, written after implementing it. Revision 1 was reviewed by Fable;
revision 2 folded in that review. This revision records what the implementation
itself forced, which was more than the plan anticipated.

## What implementing it changed

**The slice is IPv4 only.** No managed guest can create an `AF_INET6` socket
until `SystemNative_SetSockOpt` exists, so every IPv6 row was raw-P/Invoke-only
anyway; refusing IPv6 loudly also keeps out the cross-family conflict rows, which
are facts about `IPV6_V6ONLY = 0` that invert on Linux at 1.

**The order in which `bind(2)` reports its faults is per-flavour**, which the plan
did not know. Measured pairwise: Linux checks the declared length before it reads
the family and defers "already bound" until after it has validated the address;
Darwin reads the family first and rejects an already-bound socket before looking
at the address at all. So a rebind to a non-local address is `EADDRNOTAVAIL` on
Linux and `EINVAL` on Darwin, and a short `sockaddr_in6` on an IPv4 socket is
`EINVAL` on Linux and `EAFNOSUPPORT` on Darwin. Modelled as an ordered
`BindFault` list per flavour, so the divergence is one list rather than two code
paths.

**No managed guest can assert a refusal.** Raising a `SocketException` runs
`SystemNative_ConvertErrorPalToPlatform`, which is unimplemented, so a guest that
caught one aborts while constructing it — the wall `OpenMissingFile.cs` also
meets. `SocketBindListen.cs` therefore asserts only rows that succeed, and every
refusal moved to `SocketBindScreens.cs`, which reads the returned PAL error
through hand-rolled P/Invoke and never builds an exception. D5's reasoning still
holds; the tier changed.

**D4's shape was wrong, and a test caught it.** A list of *prefixes* cannot
express either rule: Darwin binds `127.0.0.1` and refuses `127.9.9.9`, so it needs
the assigned address, while Linux accepts both, so it needs the prefix. The
configuration records `Ipv4InterfaceAddress` — an assigned address *with* the
prefix length it was assigned with, `127.0.0.1/8` as `ifconfig` prints it — and
each flavour reads the half it uses. The first draft compared against the
prefix's network address and could not bind loopback under Darwin at all.

**The backlog is not stored** (D8, unchanged) and privileged ports are modelled
from `KernelConfig.UserId` against a 1024 ceiling (D7, as agreed).



## Goal

Implement the three entry points that give a socket a local address and let it
accept connections:

| entry point | `pal_networking.c` |
| --- | --- |
| `int32_t SystemNative_Bind(intptr_t socket, int32_t protocolType, uint8_t* socketAddress, int32_t socketAddressLen)` | 1760 |
| `int32_t SystemNative_Listen(intptr_t socket, int32_t backlog)` | 1892 |
| `int32_t SystemNative_GetSockName(intptr_t socket, uint8_t* socketAddress, int32_t* socketAddressLen)` | 1871 |

This is the first slice in which a socket has *mutable* state. Everything so far
— the creation triple, the event port, the address blob — is either fixed at
creation or lives in the guest's own memory.

## Where the frontier is

Measured at `4a0184e4` (this worktree, `LADDER_FLAVOUR=linux`), both socket rungs
stop at `SystemNative_Bind`, six frames out from the guest's own `Bind` call.
Stubbing forward from there:

| stubbed | RungD (`Socket`/`Bind`/`Listen`) | RungI (loopback TCP traffic) |
| --- | --- | --- |
| nothing | `SystemNative_Bind` | `SystemNative_Bind` |
| `Bind`, `Listen` | **exits 42** | `SystemNative_GetSockName` |
| `+ GetSockName` | | `SystemNative_FcntlSetIsNonBlocking` |
| `+ FcntlSetIsNonBlocking` | | `SystemNative_Accept` |
| `+ Accept` returning `EAGAIN` | | `SystemNative_TryChangeSocketEventRegistration` |

So these three entry points are exactly the layer between "a socket exists" and
"a socket can be waited on": one rung goes green, the other advances to `Accept`,
and the next thing after `Accept` is the readiness model, which this slice does
not touch.

## What a managed guest can reach at all

**No managed guest can create an IPv6 socket under PawPrint today.**
`SocketPal.CreateSocket` calls `Interop.Sys.SetSockOpt` to set `IPV6_V6ONLY` on
every non-raw `AF_INET6` socket, and `SystemNative_SetSockOpt` is unimplemented.
Measured: a guest whose whole body is
`new Socket (AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)`
aborts in `SetSockOpt`, three frames out.

Three consequences, all of which revision 1 got wrong:

- every IPv6 row in this slice is reachable only from a hand-rolled
  `[DllImport]` that calls `SystemNative_Socket` directly, so it belongs in the
  screens guest and never in a managed differential guest;
- the measured cross-family conflict rows (v6 `::` versus v4 `0.0.0.0`) are
  facts about `IPV6_V6ONLY = 0`, the kernel default the probes ran at. The
  common managed case once `SetSockOpt` lands is `V6ONLY = 1`, whose answers on
  Linux are the *opposite*. Baking the measured rows in as unconditional data
  would become a silent lie one slice later;
- so this slice models same-family conflicts only, and refuses (`failwith`) a
  conflict test between an IPv4 and an IPv6 binding, naming `IPV6_V6ONLY` as the
  input PawPrint does not yet model. Only two raw-P/Invoke sockets can reach
  that refusal; no managed guest can.

## Measurements

Linux 6.18.5 aarch64 (Apple `container`), euid 1000 and euid 0. Darwin 25.6.0
arm64, euid 501. C probes in `bindprobe/probe{1..5}.c`; managed probes are real
.NET guests run on the macOS host and in `mcr.microsoft.com/dotnet/runtime:10.0`
on Linux. Every row was run, not read off source.

### 1. Through the managed API — the oracle a differential guest faces

| row | Darwin | Linux |
| --- | --- | --- |
| `LocalEndPoint` before bind | `null` | `null` |
| bind loopback:0 | `127.0.0.1`, port ≠ 0 and ≥ 1024 | same |
| rebind the same socket | `InvalidArgument` | `InvalidArgument` |
| bind after `Listen` | `InvalidArgument` | `InvalidArgument` |
| `Listen` twice, backlog 0, backlog −1 | OK | OK |
| UDP binds the port a listening TCP socket holds | OK | OK |
| `Listen` on a UDP socket | `OperationNotSupported` | `OperationNotSupported` |
| bind 8.8.8.8:0 | `AddressNotAvailable` | `AddressNotAvailable` |
| bind loopback:1024 | OK | OK |
| bind on a closed socket | `ObjectDisposedException` | `ObjectDisposedException` |
| second socket binds a *listening* socket's exact address | `AddressAlreadyInUse` | `AddressAlreadyInUse` |
| `ProtocolType.Unspecified`, first socket bound only | `AddressAlreadyInUse` | `AddressAlreadyInUse` |
| `ProtocolType.Tcp`, first socket bound only | `AddressAlreadyInUse` | **OK** |
| `ProtocolType.Tcp`, first `0.0.0.0` listening, then loopback | **OK** | `AddressAlreadyInUse` |
| bind 127.9.9.9:0 | `AddressNotAvailable` | **OK** |
| bind loopback:1023 | `AccessDenied` | **OK as root**, `AccessDenied` otherwise |

The last four rows invert and are impure-only. `bind` on a closed socket never
reaches a syscall, so `EBADF` is raw-P/Invoke-only.

**`SO_REUSEADDR` is per-bind, not per-socket.** Rows 12 and 13 differ in exactly
one thing: the `ProtocolType` the constructor was given, which is what reaches
`SystemNative_Bind`'s `protocolType` argument and decides whether it issues
`setsockopt(SO_REUSEADDR)` (`pal_networking.c:1770`). Revision 1 claimed every
TCP socket carries the flag; that is false for `ProtocolType.Unspecified`, which
is an ordinary thing to write, and false again for a raw guest that passes
`PT_TCP` while binding a *UDP* socket. The flag is a fact about the bind call.

It is also *not* readable back: `pal_networking.c:2274` maps managed
`ReuseAddress` and `ExclusiveAddressUse` to `SO_REUSEPORT` where that exists, so
the flag `Bind` sets is invisible to `GetSocketOption`. Its whole observable
effect is the next bind's return code — which is precisely why revision 1's
"unobservable until `GetSockOpt` exists" was wrong.

### 2. Conflict matrix, by whether each bind set the flag

All rows same-family IPv4, same port, TCP unless stated.

| first | second | Darwin | Linux |
| --- | --- | --- | --- |
| exact address, bound only, flag on both | | `EADDRINUSE` | **OK** |
| exact address, bound only, flag on second only | | `EADDRINUSE` | `EADDRINUSE` |
| exact address, first listening, flag on both | | `EADDRINUSE` | `EADDRINUSE` |
| `0.0.0.0` listening, then loopback, flag on both | | **OK** | `EADDRINUSE` |
| loopback listening, then `0.0.0.0`, flag on both | | **OK** | `EADDRINUSE` |
| `0.0.0.0` bound only, then loopback, flag on both | | OK | OK |
| auto-bound listener (`listen` with no `bind`), then `0.0.0.0`, no flag | | `EADDRINUSE` | `EADDRINUSE` |
| auto-bound listener, then loopback, no flag | | `EADDRINUSE` | `EADDRINUSE` |
| auto-bound listener, then `0.0.0.0`, flag on second | | `EADDRINUSE` | `EADDRINUSE` |
| auto-bound listener, then loopback, flag on second | | **OK** | `EADDRINUSE` |
| two UDP sockets, exact address, no flag | | `EADDRINUSE` | `EADDRINUSE` |
| two UDP sockets, `0.0.0.0` then loopback, no flag | | `EADDRINUSE` | `EADDRINUSE` |
| TCP listening, then UDP same port | | OK | OK |

The auto-bound rows exist because `listen(2)` on an unbound socket binds it
*without going through `SystemNative_Bind`*, so such a listener never carries the
flag — a cell revision 1 did not know it was missing. Every UDP row agrees,
because no UDP bind through the shim sets the flag; every divergence in the table
is TCP-and-flag-specific.

### 3. Screens

| row | Darwin | Linux |
| --- | --- | --- |
| `bind` v4 blob, length 16 | OK | OK |
| `bind` v4 blob, length 17..32 | `EINVAL` | OK |
| `bind` v4 blob, greatest accepted length | 16 | **128** (129 is the least rejected) |
| `bind` v6 blob, length 26, 27 | `EINVAL` | OK |
| `bind` v6 blob, length 16 | `EINVAL` | `EINVAL` |
| `bind` length below the family's `sizeof` | `EINVAL` | `EINVAL` |
| `bind` v6 blob on a v4 socket | `EAFNOSUPPORT` | `EAFNOSUPPORT` |
| `bind` v4 blob on a v6 socket | `EAFNOSUPPORT` | `EINVAL` |
| `bind` all-zero blob, length 16, v4 socket | OK, binds `0.0.0.0`:ephemeral | same |
| `bind` all-zero blob, length 28, v4 socket | `EINVAL` | OK |
| `bind` `AF_UNSPEC` blob carrying 127.0.0.1:12345 | interprets it (bound, or `EADDRINUSE`) | `EAFNOSUPPORT` |
| `bind` `AF_UNSPEC` blob carrying 8.8.8.8 | `EADDRNOTAVAIL` | `EAFNOSUPPORT` |
| `bind` `255.255.255.255` / `224.0.0.1` | `EAFNOSUPPORT` | OK |
| `bind` `fe80::1` with scope 0 | `EADDRNOTAVAIL` | `EINVAL` |
| `bind` / `listen` / `getsockname` on a closed fd | `EBADF` | `EBADF` |
| `listen` on a UDP socket | `EOPNOTSUPP` | `EOPNOTSUPP` |
| `listen` backlog 0, −1, `INT_MAX` | OK | OK |
| `getsockname` on a fresh unbound socket | family only, zeros, `*len` 16 / 28 | same |

`AF_UNSPEC` is not one rule: Darwin reads the address and port out of the blob
and binds them, Linux accepts the blob only when the address is zero and answers
`EAFNOSUPPORT` otherwise. Revision 1 had measured only the all-zero case and
would have generalised it wrongly.

### 4. Three things reading the source gets wrong

**`GetSockName` reports the address's real length even when it truncated.** The C
asserts `addrLen <= (socklen_t)*socketAddressLen` and then stores `addrLen` back.
Measured on both platforms with declared lengths of 8 and 0: the call *succeeds*,
writes only as many bytes as were declared (none, for 0), and reports 16. The
assert is false on both platforms — it is compiled out of the shipped Release
build — and a caller believing the reported length would read uninitialised
bytes. PawPrint writes `min(declared, real)` bytes and reports the real length.

**Darwin's `bind` wants the length to be exactly the family's `sizeof`.** Not a
minimum: 16 succeeds for a v4 blob and every value from 17 to 32 fails `EINVAL`,
where Linux accepts 16 through 128 inclusive. Invisible through the managed API,
which always passes `SocketAddress.Size`.

**`sa_len` is input to nothing.** Darwin's `bind` accepts a v4 blob whose leading
`sa_len` byte says 0, 8, 24 or 32, given a correct length argument. The byte that
`SocketAddress..ctor` writes, and that the family accessors work around, is not
read by the kernel; only the explicit length argument counts.

## Decisions

### D1. Where a socket's mutable state lives — **recommendation changed**

`OpenFileTarget.Socket` carries the whole `SocketDescription`
(`FileDescriptorRegistry.fs:215`), and the registry mints its ids. That type's
docstring already flags the limit: *"A socket that must outlive or precede every
descriptor — a completed connection waiting in a listening socket's backlog —
would break that, and wants the table."*

**Option A — keep it in the open file description.** Add the binding and the
listening flag to `SocketDescription`; conflicts are answered by scanning
`registry.Descriptions`. The scan is complete today because the registry already
*enforces* one-description-per-socket (`DuplicateSocketId`,
`FileDescriptorRegistry.fs:429`).

**Option B — a socket table in the emulated kernel, keyed by `SocketId`.**
`OpenFileTarget.Socket` shrinks to the id; all socket state lives in the table.

**Option C — leave the socket where it is, add a kernel endpoint table** mapping
(protocol, address, port) to `SocketId` as the sole home of bindings.
`GetSockName` then reverse-scans it. Rejected: it splits one socket's state
across two homes and either denormalises the binding or makes `GetSockName` a
scan.

**Revision 1 recommended B. This revision recommends A**, for four reasons that
came out of review:

1. **B installs an invariant that `Accept` must immediately tear up.** B's
   lifetime rule is "closing the last description removes the table entry" —
   and a backlog connection is exactly a socket that no descriptor names. Doing
   B now means writing that invariant knowing it is wrong one slice later;
   doing it in the `Accept` slice means writing it once, correctly.
2. **The migration argument revision 1 gave does not apply.** "Finish your
   migrations" is about not leaving two versions of the truth coexisting.
   Option A leaves none: all socket state stays in one home. The honest argument
   for B-now is only that bind adds fields which later move — and they move
   wholesale with the record, so the enlargement is nearly free.
3. **The `2026-08-19-systemnative-socket.md` plan already assigned this migration
   to the `Accept` PR**, "informed by what it actually needs". Reversing a
   recorded decision needs a better reason than anticipation.
4. **B is not free, and none of its cost was costed in revision 1.**
   `FileDescriptorRegistry.close` (line 576) returns only the new registry, so
   the `SystemNative_Close` handler cannot learn that a socket died — its
   contract has to change. `checkInvariants` (line 901) lives in
   `FileDescriptorRegistry.fs`, which compiles *before* `EmulatedKernel.fs`, so
   the "every table entry has a description" rule cannot join the existing
   defect list and needs a new kernel-level checker. And `NextSocketId` is minted
   in the registry (line 358) while the socket it names would live in the
   kernel, so either the counter moves — killing `NextSocketIdNotFresh` as a
   registry defect — or two updates must be kept in step.

If you would rather have B, the right shape is a **separate, behaviour-free
preparatory PR** that moves the representation, changes `close`'s contract and
relocates the invariant, with this slice stacked on it. What revision 1 proposed
— representation migration *and* three entry points *and* two `KernelConfig`
knobs *and* a typed-address codec *and* a per-flavour conflict model — is
several features in one PR by this project's own standard.

### D2. What the stored address is

**Option A — store the guest's blob verbatim** and echo it from `GetSockName`.
Rejected: `bind(port 0)` has to rewrite the port, the conflict test would compare
flavour-shaped bytes, and the stored bytes would carry Darwin's `sa_len` into
kernel state with no business knowing about it.

**Option B — parse to a typed `TransportAddress`** at `Bind`, serialise at
`GetSockName`; the flavour-specific layout stays in the two converters, beside
the `SockaddrFamilyField` data PR #1086 added. The interior sees
`V4 of address * port` and `V6 of address * scope * port`, never a length or a
family number.

**Recommendation: B.** `flowinfo` is *not* preserved by either kernel (measured:
bound with `0x11223344`, reported back as 0), so the typed value has no flowinfo
field. The parser must also decide `AF_UNSPEC`, which §3 shows is two different
rules — so it is flavour data like everything else here, and an `AF_UNSPEC` blob
with a non-zero address is `EAFNOSUPPORT` on Linux and an ordinary v4 bind on
Darwin.

### D3. Ephemeral port selection

**Option A — a counter** over the configured range, skipping ports that conflict,
one full sweep before giving up.
**Option B — a draw from the kernel's seeded PRNG** over the same range, retried
on collision, which is Linux's own shape.

**Recommendation: A.** A trace whose ports read 32768, 32769, 32770 is far easier
to follow than one whose ports are random, and the value is unspecified either
way. Four things revision 1 left open:

- the range is new `KernelConfig` state. Real values are Linux 32768–60999 and
  Darwin 49152–65535, both *sysctl* settings, so by the reasoning that made
  filesystem type configuration rather than a flavour derivation, this is one
  configurable range with a single default, not per-flavour data;
- "a function of (state, seed) alone" was wrong for a counter — it has no seed.
  The property is that it is a function of the kernel state alone;
- "in use" must mean *conflicts with*, through the same relation `Bind` uses:
  a port a TCP socket holds is free to UDP (measured), so a naive
  "port is taken" set would refuse a legal bind;
- exhaustion: one full sweep of the range, then `failwith`. The real answer
  (`EADDRINUSE` from an implicit bind) is unmeasured, and inventing it would be
  a guess.

### D4. Which addresses are bindable — **shape changed**

Revision 1 proposed an interface table of (name, addresses). That is the wrong
shape for the row that motivates it: `127.9.9.9` is bindable on Linux because the
local route table holds `127.0.0.0/8`, not because any interface is assigned that
address.

**Option A — a per-flavour predicate over a configured set of local prefixes.**
Default `127.0.0.0/8` and `::1/128`; Linux accepts any address inside a local
prefix, plus broadcast and multicast; Darwin accepts only an address a prefix
*is* — matching `127.0.0.1` but not `127.9.9.9` — and answers `EAFNOSUPPORT`
rather than `EADDRNOTAVAIL` for broadcast and multicast.
**Option B — a hardcoded predicate** with no configuration at all.

**Recommendation: A**, with the caveat stated plainly: until
`SystemNative_EnumerateInterfaceAddresses` exists, nothing distinguishes A from
B except the ability to configure, and no test can tell them apart at the default.
A is preferred only because the divergent rows have to come from *somewhere* and
a prefix list is the honest home. The interface table proper — names, flags,
per-interface addresses — waits for the entry point that reads it.

### D5. Which entry points

**Option A — `Bind` and `Listen` only.** RungD goes green on this alone
(measured), but nothing can read back what `Bind` stored.
**Option B — plus `GetSockName`.** The stored address becomes observable, which
is what makes a differential test of it possible at all.
**Option C — plus `FcntlSetIsNonBlocking`, `GetPeerName`, `Shutdown`,
`SetSockOpt`.** Rejected: those are facts about a *connection*, or about the
description's flags, and belong with the slice that first needs them.

**Recommendation: B.**

### D6. How much of the conflict matrix

**Option A — exact-duplicate conflicts only**, refusing on anything else.
**Option B — the measured §2 matrix**, as per-flavour data keyed on both binds'
reuse flags and on whether either socket is listening.

**Recommendation: B**, restricted to same-family pairs, with a `failwith` for a
cross-family pair (see "What a managed guest can reach"). A guest binding
`0.0.0.0` and `127.0.0.1` on one port is ordinary, not exotic, and A's refusal
would fire on it.

### D7. Privileged ports — **new**

Measured `EACCES` below 1024 on both platforms for a non-root euid, and success
as root on Linux. `KernelConfig.UserId` already exists, defaulting to 1000.

**Option A — model it**: `UserId ≠ 0` and port below the threshold gives
`EACCES`. Linux's threshold is the `ip_unprivileged_port_start` sysctl and
Darwin's 1024 is fixed, so like D3's range this is configuration with one
default rather than flavour data.
**Option B — `failwith` on a privileged port**, on the grounds that the answer
depends on a uid model that sockets do not otherwise use.

**Recommendation: A.** It is two comparisons against state that already exists,
and B would crash a guest doing something as ordinary as binding port 443.
Because the answer depends on the *guest's* uid and the host test runner's uid
is not controlled, this row is impure-only.

### D8. The backlog — **new**

`Listen`'s backlog is not stored. Nothing this slice implements can read it —
its only observer is `Accept`'s queue depth — and a field written but never read
is state a test cannot cover, which is how a discarded write survives a whole
suite. `Accept`'s slice adds it together with the queue it bounds.

## Deliverables outside the three handlers

- `UnixError` gains `EADDRINUSE`, `EADDRNOTAVAIL` and `EOPNOTSUPP`; none of the
  three exists today. Each needs its PAL value and its per-flavour raw errno
  (98/48, 99/49, 95/102), measured, in the style `EAFNOSUPPORT` already uses.
- `KernelConfig` gains the ephemeral port range, the local prefix list and the
  privileged-port threshold.
- `TransportAddress` and its per-flavour codec.
- `AF_UNIX` sockets are creatable today, so `Bind`, `Listen` and `GetSockName`
  on one must `failwith` explicitly: a filesystem path is not a transport
  endpoint, and belongs with the filesystem work.

## Tests

The conflict matrix (§2) and the screen matrix (§3) are checked in as
machine-readable per-flavour tables and asserted row by row against the model's
classifier, the way `socketMatrix/{linux,darwin}.tsv` is asserted by
`TestSocketCreation.fs`. A checked-in table that nothing drives is documentation,
not an oracle.

- `sourcesPure/SocketBindListen.cs` — differential, the §1 rows that agree:
  `LocalEndPoint` null before bind; bind loopback:0 gives `127.0.0.1` and a
  non-zero port ≥ 1024 (**not** a range assertion — the two real ranges do not
  overlap); rebind and bind-after-listen throw `InvalidArgument`; `Listen` twice
  and with backlog 0 or −1 succeed; a UDP socket takes a listening TCP socket's
  port; `Listen` on a UDP socket throws `OperationNotSupported`; bind 8.8.8.8
  throws `AddressNotAvailable`; a second socket on a *listening* socket's exact
  address throws `AddressAlreadyInUse`; a `ProtocolType.Unspecified` pair throws
  it even when the first is only bound.
- `sourcesPure/SocketBindScreens.cs` — hand-rolled `[DllImport]`, for everything
  managed code cannot express: `EFAULT` for a null blob and a negative length,
  `EINVAL` for a short length, `EAFNOSUPPORT` for a v6 blob on a v4 socket,
  `EBADF` for a closed descriptor, every IPv6 row, the `GetSockName` truncation
  row (declare 8, get 8 bytes written and 16 reported), the fresh-unbound
  `GetSockName` row (family only, zeros) — which is the one case where the
  serialiser emits an address nothing ever parsed — and the implicit bind that
  `Listen` performs, which is invisible to `LocalEndPoint` and observable only
  here. Screen *ordering* rows are the point, as in the previous two slices: a
  null blob on a closed fd is `EFAULT`, not `EBADF`, because the C screens
  before `ToFileDescriptor`.
- `sourcesImpure/SocketBind{Linux,Darwin}.cs` — the §1–§3 divergences, one guest
  per flavour under the matching `KernelConfig.UnixPlatform`: the length rules,
  the four inverting conflict rows, `127.9.9.9`, `224.0.0.1`, the two `AF_UNSPEC`
  rules, and the privileged port. Expected values are obtained by **running each
  guest on the real platform** — macOS host and Linux container — rather than
  from the C probes, because the shim's screens and its `SO_REUSEADDR` insertion
  sit between a C probe and a guest, and an impure case's expectation is our
  claim rather than an oracle's answer.
- Property tests:
  - the port allocator returns a port that conflicts with nothing, always in
    range, and is a function of the kernel state alone; over a range narrowed to
    a handful of ports, it exhausts rather than looping;
  - `parse >> serialise` is the identity on every blob the serialiser emits and
    `serialise >> parse` on every `TransportAddress`, per flavour;
  - parse ignores exactly what the kernel ignores: mutating `sa_len` or
    `flowinfo` does not change the parse (§4);
  - closing a socket frees its endpoint: after close, a bind that conflicted
    succeeds.
- The ladder: RungD moves from `SystemNative_Bind` to green, RungI to
  `SystemNative_FcntlSetIsNonBlocking`.

## Out of scope

`Accept`, `Connect`, `Send`, `Receive` and the readiness model
(`TryChangeSocketEventRegistration`, marking a descriptor ready, and the
re-entrant wake out of `BlockedOnSocketEvents`) are the next slice and the one
genuinely architectural item left; the socket table (D1 option B) goes with them.
`SetSockOpt`/`GetSockOpt` are not implemented here, which keeps every IPv6 socket
out of managed reach and pins `IPV6_V6ONLY` at its kernel default for the raw
guests that can reach one.
