# The socket-address blob layer: `SocketAddressPal`

Measured at `b4a89a79`, against rungs D and I of
`docs/plans/2026-08-17-aspnet-critical-path/`, under `LADDER_FLAVOUR=linux`.

## Where the frontier was, and where it goes

With #1082's `EnumEquals` allowlisting, `new Socket(...)` constructs, and both socket rungs
advance from `SystemNative_Socket` to the same new site — which is not a socket call at all:

| Rung | First failure before | After |
| --- | --- | --- |
| D (`Socket`/`Bind`/`Listen`) | `SystemNative_GetSocketAddressSizes`, 7 frames out | `SystemNative_Bind` |
| I (loopback TCP traffic) | `SystemNative_GetSocketAddressSizes`, 7 frames out | `SystemNative_Bind` |

That site is `System.Net.Primitives`' `SocketAddressPal..cctor`, reached from
`IPEndPoint.Serialize()` inside `Bind`. It is *not*
`SystemNative_GetMaximumAddressSize`, which #1045 already serves: three separate entry points
report socket-address sizes and this is the second of them.

The slice was scoped by stubbing forward before writing anything. Stubbing the sizes moved both
rungs to `SetAddressFamily`; stubbing the setters too moved both to `SystemNative_Bind`. So the
layer is closed, and it is exactly the nine `Interop.Sys` entry points `SocketAddressPal` calls —
`GetSocketAddressSizes`, and `Get`/`Set` for `AddressFamily`, `Port`, `IPv4Address` and
`IPv6Address`. Nothing else calls them, and **none of the nine touches kernel state**: each is a
bounds-checked read or write of a `struct sockaddr_in`/`sockaddr_in6` in a buffer the guest owns.

A guest that creates *no socket at all* drives the whole set, which is what gives this layer a
differential oracle with no `Bind`, no socket state machine and no readiness model in existence.

## Measured layout, both flavours

`sizeof`/`offsetof` probes compiled on the Darwin host (arm64) and on Linux (aarch64, via the
`container` CLI). Arch-invariant — every member of these structs is fixed-width and the two
variable-length tails are sized from a constant — so the aarch64 reading stands for the pinned
linux-x64 pack.

| | Linux | Darwin |
| --- | --- | --- |
| `sizeof(sockaddr_in)` / `in6` / `storage` | 16 / 28 / 128 | 16 / 28 / 128 |
| `sizeof(sockaddr_un)` | **110** | **106** |
| `sizeof(sa_family_t)` | **2** | **1** |
| `offsetof(sockaddr, sa_family)` | **0** | **1** (byte 0 is `sa_len`) |
| `sin_port` / `sin_addr` | 2 / 4 | 2 / 4 |
| `sin6_port` / `flowinfo` / `addr` / `scope_id` | 2 / 4 / 8 / 24 | 2 / 4 / 8 / 24 |
| `AF_UNSPEC` / `AF_UNIX` / `AF_INET` | 0 / 1 / 2 | 0 / 1 / 2 |
| `AF_INET6` | **10** | **30** |
| `AF_PACKET` / `AF_CAN` | 17 / 29 | undefined |

Of the four sizes the class initialiser reports, three are flavour-free and only `sockaddr_un`
diverges. Every field past the family agrees, and not by luck: BSD gave `struct sockaddr` a
leading one-byte `sa_len` and narrowed `sa_family_t` to one byte to pay for it, so the two
layouts spend the same two leading bytes differently rather than in different amounts.

`SocketAddressPal`'s managed IL is **byte-identical** between the host macOS pack and the pinned
linux-x64 pack (diffed with `IlDump`), so all the flavour difference lives in the handlers, and
no linux-flavour variant of any guest is needed for this layer.

## Four findings that reading the source gets wrong

**Managed code writes BSD's `sa_len`, and it is what makes the family's width observable.**
Nothing in `pal_networking.c` mentions `sa_len`. `SocketAddress..ctor(AddressFamily, int)`
stores `(byte) _size` at index 0 before calling `SetAddressFamily`, unconditionally on every
platform. On Darwin the one-byte family lands at offset 1 and leaves that byte standing as
`sa_len`; on Linux the two-byte family overwrites it. Measured: a fresh
`SocketAddress(AddressFamily.InterNetwork)` is `10 02 …` on Darwin and `02 00 …` on Linux. So a
Linux handler that wrote only one byte would leave the size behind at offset 1, and
`GetAddressFamily` would then read `0x1002` and report `AF_UNKNOWN`.

**A negative length is EFAULT, not EAFNOSUPPORT.** `SystemNative_GetPort` screens only for a
null pointer before its `IsInBounds` check, and casts the length to `size_t` there — so reading
the source suggests a negative length sails past the bounds check and reaches the family switch,
which would answer EAFNOSUPPORT for an unrecognised family. Measured on both platforms, it
answers EFAULT: the cast makes the bound `SIZE_MAX`, so `baseAddr + len` wraps to *below* the
base and the comparison fails. Modelling it therefore needs no undefined-behaviour reasoning at
all — `offset + width <= socketAddressLen` is false for a negative length, which is the whole of
it.

**`SetIPv6Address` with an oversized `addressLen` stores zeroes and reports success.**
`ConvertByteArrayToIn6Addr` calls `memcpy_s(&sin6_addr, 16, address, addressLen)`, and the PAL's
own `memcpy_s` (`pal_safecrt.h:59`) will not copy into a destination it was told is too small: it
`memset`s that destination to zero and returns ERANGE. `ConvertByteArrayToIn6Addr` discards the
return, so the entry point reports success having stored the all-zeroes address. The
`assert(sizeInBytes >= count)` above it is compiled out of the shipped Release build. Measured on
both platforms with `addressLen` of 17 and 32. The getter is not symmetric — there `addressLen` is
the *destination* size, so a larger one is simply room to spare.

**`GetIPv6Address` reads `sin6_scope_id` after writing the address, and that is observable.**
`address` may legally point at byte 24 of the very blob being read: `sin6_addr` ends there, so
`memcpy_s`'s own overlap assertion passes. The copy then lands on `sin6_scope_id`, and the next
statement reads it back. Measured: a `fe80::` address aliased there reports a scope of 33022
rather than the one that was set, identically on both platforms.

Both of these were found by Codex against the first revision, which had read the C's two
statements as independent and treated every `addressLen >= 16` alike.

## What was built

**Flavour data in `SimulatedUnixPlatform`** (`EmulatedKernel.fs`), rather than a typed address
value parsed at each entry point. These nine are incremental mutators over a buffer the guest can
read back through `SocketAddress.Buffer`, each touching only its own field; re-serialising a
parsed value would write bytes the C never writes. A typed address belongs at the
`Bind`/`Connect`/`GetSockName` boundary, where it is what the kernel model stores — which keeps
identity and projection apart.

- `SocketAddressSizes`, the four `sizeof`s, whose `Storage` field *is*
  `maximumSocketAddressSize` rather than a second 128;
- `SockaddrFamilyField`, a two-case DU so no caller can pair an offset with the wrong width;
- `internetAddressFamily` / `internetV6AddressFamily`, the raw `AF_INET`/`AF_INET6` the accessors
  switch on, and the primitives `addressFamilyPalToPlatform` is defined in terms of;
- `addressFamilyPalToPlatform` / `addressFamilyPlatformToPal`. `socketCreation`'s own
  address-family screen now calls the first of these instead of carrying a second copy of the
  rule — the same C function screens `SystemNative_Socket`'s first argument and converts
  `SystemNative_SetAddressFamily`'s.

**Nine handlers** in `NativeSystemNative.fs`, over shared `sockaddrFieldAt` /
`readSockaddrFamily` / `writeSockaddrFamily` helpers and a `SockaddrOffsets` module. They reach
fields individually rather than transferring whole structs, because PawPrint's typed address
space aborts on a read that runs past the storage rather than inventing what follows it — so
touching only the bytes the C touches is enforced rather than merely intended.

**`NativeCall.uint16Argument` and `uint32Argument`.** A CLI `UInt16` lives in a two-byte cell of
its own rather than being widened to `Int32` the way a `UInt32` is, so `SetPort`'s `ushort`
parameter is rejected outright by `int32Argument`.

## Tests

- `sourcesPure/SocketAddressRoundTrip.cs` — differential, and restricted to claims that hold
  whichever Unix laid the bytes out: `Size`, `Family`, and `IPEndPoint.Create(ep.Serialize())`
  round-tripping address, port and scope id. Values chosen so a wrong answer inverts rather than
  perturbs — a port whose two bytes differ, an address that is not a palindrome, a scope id that
  is not zero and is the one field *not* in network order.
- `sourcesPure/SocketAddressScreens.cs` — the return codes, by hand-rolled P/Invoke, since the
  managed surface never passes a null out-parameter or a short length and discards the class
  initialiser's return value entirely. Three ordering rows are the point: a short blob is EFAULT
  while an unsupported family is EAFNOSUPPORT and the *same* blob through `GetIPv4Address` is
  EINVAL; and a negative length beats all of them. It also carries the two `memcpy_s` rows above,
  whose address buffer has to start out holding the address rather than zeroes — a zeroed buffer
  cannot tell "the destination was zeroed" from "the bytes were copied after all", and the mutant
  that copies them survived until it did. Every row was measured to answer identically on macOS
  and Linux before being asserted.
- `sourcesImpure/SocketAddressLinuxBytes.cs` and `SocketAddressDarwinBytes.cs` — the blob itself,
  byte for byte, one guest per flavour. A round trip is self-consistent and would pass against a
  setter and getter that agreed with each other on a wrong offset, width or byte order; these are
  what pin the layout to the platform's own. Both sets of expected bytes were measured by running
  the same guest on real .NET — on macOS for one, in a Linux container for the other — rather
  than derived, because an impure case's expectation is our claim rather than an oracle's answer.

## Out of scope

`SystemNative_Bind` and the socket state machine is the next slice, and the rungs now say so
themselves. The readiness model — an interest set on the event port, marking a registered
descriptor ready, and the re-entrant wake out of `BlockedOnSocketEvents` — remains the one
genuinely architectural item ahead, and no count of entry points bounds it.
`SystemNative_GetDomainSocketSizes`, the third of the three address-size entry points, keeps
refusing loudly: nothing on the Kestrel TCP path calls it.
