# `SystemNative_CreateSocketEventBuffer` / `SystemNative_FreeSocketEventBuffer`

Follow-on from #1060, which implemented `SystemNative_WaitForSocketEvents` and the park.
This is the next entry point on rung D of the ASP.NET ladder, chosen by measurement rather
than by working down the 63-entrypoint list.

## 0. Why this entry point

Rung D of `docs/plans/2026-08-17-aspnet-critical-path.md` — a guest that does
`new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)`, `Bind`,
`Listen` — is the row that tracks the socket stream. Re-measured at `d84638f7` (main, with
#1060 merged) with that document's `run-ladder.sh`:

```
guest            real       pawprint   elapsed
RungD            42         134        2.71s
    Unimplemented native method (PInvokeImpl libSystem.Native!SystemNative_CreateSocketEventBuffer)
    Guest was: thread 0 (Runnable) in System.Net.Sockets.Sys.CreateSocketEventBuffer at IL
    offset 0, called 7 frames out from RungD.Program.Main at IL offset 3
```

The previous blocker on that rung, `SystemNative_PlatformSupportsDualModeIPv4PacketInfo`,
was closed by #1058. So this is the measured head of the ladder, and the entry point
`SocketAsyncEngine`'s constructor reaches immediately after the
`CreateSocketEventPort` that #1057 implemented.

## 1. What upstream actually does

Managed externs (`Common/src/Interop/Unix/System.Native/Interop.SocketEvent.cs:36`):

```csharp
[LibraryImport(Libraries.SystemNative, EntryPoint = "SystemNative_CreateSocketEventBuffer")]
internal static unsafe partial Error CreateSocketEventBuffer(int count, SocketEvent** buffer);

[LibraryImport(Libraries.SystemNative, EntryPoint = "SystemNative_FreeSocketEventBuffer")]
internal static unsafe partial Error FreeSocketEventBuffer(SocketEvent* buffer);
```

Both return a PAL `Interop.Error`, so as with `CreateSocketEventPort` neither touches
`Kernel.LastSystemError`.

`pal_networking.c:3447`:

```c
int32_t SystemNative_CreateSocketEventBuffer(int32_t count, SocketEvent** buffer)
{
    if (buffer == NULL || count < 0)
    {
        return Error_EFAULT;
    }

    size_t bufferSize;
    if (!multiply_s(SocketEventBufferElementSize, (size_t)count, &bufferSize) ||
        (*buffer = (SocketEvent*)malloc(bufferSize)) == NULL)
    {
        return Error_ENOMEM;
    }

    return Error_SUCCESS;
}

int32_t SystemNative_FreeSocketEventBuffer(SocketEvent* buffer)
{
    free(buffer);
    return Error_SUCCESS;
}
```

Five details of that body which the handler has to reproduce and which are easy to skip.

- **The two EFAULT conditions are unordered by construction.** `buffer == NULL` and
  `count < 0` produce the *same* answer, so no input can tell which was checked first.
  There is therefore no adjacent-pair ordering test to write here, unlike #1060's four-deep
  ladder — and no reordering mutant to kill, because the reordered program is
  observationally identical. That is a fact to state, not a gap to apologise for.
- **`*buffer` is written on one of the two ENOMEM paths and not the other.** The `||`
  short-circuits: if `multiply_s` overflows, the store never runs and `*buffer` keeps
  whatever the caller left there. If `multiply_s` succeeds and `malloc` returns NULL, the
  store *does* run, and `*buffer` is NULL on return. §4 decides which of these PawPrint's
  own failure corresponds to.
- **`free(NULL)` is a documented no-op**, and `FreeSocketEventBuffer` has no screen at all —
  it returns `Error_SUCCESS` unconditionally, even for a pointer that was never allocated.
  The sole managed caller filters null itself (`FreeNativeResources` compares `_buffer`
  against `IntPtr.Zero` first), so the null row is reachable only from a hand-rolled guest.
- **A failed `malloc` moves `errno`; the wrapper's own EFAULT does not.** The return value is
  a PAL `Error`, which is what made this easy to miss — but `malloc` is libc, and libc sets
  `errno` on failure. Measured: `malloc(SIZE_MAX)` leaves `errno` 12 having been 7 before. So
  the ENOMEM path must write `Kernel.LastSystemError`, exactly as
  `SystemNative_CloseSocketEventPort` does on its EBADF row and for the same reason — a guest
  reading `Marshal.GetLastSystemError` after a raw P/Invoke must not see a stale value. The
  EFAULT rows run no libc call at all and so leave it alone. ENOMEM is raw 12 under both
  numberings, so no flavour decision arises. (Note the asymmetry with the `*buffer` store: the
  `multiply_s` route touches neither, the `malloc` route touches both.)
- **A non-null but garbage `buffer` is a `SIGSEGV`, not an errno.** The wrapper screens only
  `buffer == NULL`, then stores through it unconditionally. Same shape as
  `CreateSocketEventPort`'s `*port = fd`, and the same answer: `failwith`, per
  `prefer-crashing-over-documented-divergence`. Manufacturing EFAULT there would convert a
  process-killing fault into a plausible wrong answer.

### The element size

`SocketEventBufferElementSize` is a file-scope constant, defined once per backend:

| backend | definition | value |
| --- | --- | --- |
| epoll (`:3085`) | `max(sizeof(struct epoll_event), sizeof(SocketEvent))` | **16** |
| kqueue (`:3217`) | `sizeof(struct kevent)` | **32** |
| neither (`:3377`) | `0` | — |

Both measured on this host rather than recalled: `sizeof(struct kevent)` is 32 and
`sizeof(SocketEvent)` is 16 on Darwin arm64 (`SocketEvent` being
`{ intptr_t Data; int32_t Events; int32_t _padding; }`).

The epoll row is the interesting one, and it is where #1060's plan went wrong in the other
direction. `sizeof(struct epoll_event)` *is* architecture-dependent — 12 on x86-64 under
`EPOLL_PACKED`, 16 elsewhere — but the `max` against `sizeof(SocketEvent)` erases exactly
that difference, since `max(12, 16) = max(16, 16) = 16`. So:

- the **element size** is a total function of the *flavour* alone: Linux 16, Darwin 32;
- the **`epoll_wait` constants** #1060 added (`LinuxEpollLimits.EventSize` = 12,
  `MaxEvents` = `INT_MAX / 12`) are *not*, because neither goes through the `max`.

That is the whole content of #1060's §7 correction, seen from the other side, and it decides
where this constant lives.

## 2. Where the element size goes

Three genuinely different placements.

### Option 1 — an accessor on `SimulatedUnixPlatform`

`SimulatedUnixPlatform.socketEventBufferElementSize`, beside `rawErrnoNumbering` and
`reportsBirthTime`.

- The type's docstring promises every fact derived from it is a *total* function of the
  flavour, and this one is: both flavours have an answer, unlike `epollEventSize`.
- It is the same kind of fact as its neighbours: a compile-time property of the native shim
  that follows from which Unix is being impersonated.
- Cost: nothing. This is the placement the type exists for.

### Option 2 — a standalone `SocketEventBufferLimits` module, keyed on flavour

Mirror `LinuxEpollLimits`, taking a flavour parameter.

- Keeps every socket-event constant in one place, so a reader chasing "what does the event
  buffer look like" finds both sizes together.
- But it would be a module whose only content is a `match` on the flavour, which is what
  `SimulatedUnixPlatform`'s accessors already are — and it would make the *total* fact look
  like the *partial* one, which is precisely the distinction #1060 paid attention to.

### Option 3 — `KernelConfig` field

- Lets a host simulate a machine with a different stride.
- Rejected: no guest asks, and it is not host configuration — it is a property of the shim's
  compile-time target, which the flavour already names. `UserAddressLimit` is configuration
  because two machines running the same kernel differ in it; two machines running the same
  flavour cannot differ in this.

**Choice: Option 1.** The deciding argument is the one that put `epollEventSize` *outside*
this type: totality. `epoll_event`'s size has no Darwin answer, so it could not live here;
the element size has answers for both, so it should.

## 3. `count == 0`

`malloc(0)` is implementation-defined: C permits NULL. Measured on this host (Darwin 25.6.0
arm64):

```
malloc(0) = 0x102e7a1f0, second = 0x102e7a200, distinct = 1
```

so libmalloc returns a unique non-NULL pointer, and glibc documents the same behaviour. The
handler therefore answers SUCCESS with a distinct non-null pointer for `count == 0`, which
is what both hosts a differential test could run on actually do. The block is zero bytes
wide, so any dereference of it is out of bounds — which is true of the real thing too.

## 4. A size PawPrint cannot represent

`NativeMemoryPool.allocate` takes an `int` byte count, so the largest buffer PawPrint can
allocate is `Int32.MaxValue` bytes: `count <= 134_217_727` under Linux's stride of 16, and
`67_108_863` under Darwin's 32. Above that the interpreter has no block to hand back.

The real thing does not fail there. `multiply_s` cannot overflow — `size_t` is 64-bit and
`int32` × 32 is at most ~6.9×10^10 — and `malloc` of that much succeeds by overcommit.
Measured on this host:

```
malloc(16 * INT_MAX) = 0x7000000000
```

So whatever PawPrint answers for a huge count is a divergence from the host, and the choice
is which kind.

### Option 1 — report ENOMEM

- Consistent with the two allocators already in this file: `SystemNative_Malloc` and
  `SystemNative_Calloc` both return NULL for sizes "the interpreter cannot satisfy", with
  the stated reason that CoreLib then raises a catchable `OutOfMemoryException`.
- ENOMEM is one of the entry point's own two documented answers, so the guest is being told
  something the real entry point can say.
- The divergence band is `count` above ~1.3×10^8, where a host succeeds and PawPrint fails.
  Not differentially testable in either direction, so no test can be written that would
  notice — which is exactly why it has to be written down here.

### Option 2 — `failwith`

- The divergence becomes a crash naming the condition, and can never be mistaken for the
  real entry point's answer.
- But it is inconsistent with the two sibling allocators, which would leave PawPrint saying
  "allocation failed" for `Marshal.AllocHGlobal(3 GB)` and "the interpreter refuses" for
  `CreateSocketEventBuffer(2 GB / 16)`. A guest cannot see any principle behind that split.

**Choice: Option 1**, on consistency. The precedent is in the same file, three handlers up,
and it was chosen for the same reason: an allocation the interpreter cannot represent is
reported as an allocation failure. The divergence band is recorded in the handler's comment.

This also settles §1's second bullet. PawPrint's failure is the *`malloc` returned NULL*
arm, not the `multiply_s` arm — the product is perfectly representable, it is the block that
is not — so `*buffer` **is** written, as null, before ENOMEM is returned.

## 5. Writing `*buffer`

The value stored is a pointer, so unlike `CreateSocketEventPort`'s `*port = fd` this cannot
go through `writeBytesThrough`: synthesising a bit pattern for a pointer would discard the
provenance the guest's later `ldind.i` needs. `AppContextSeed.allocatePointerArray` already
records the right shape for a pointer *cell* in indirect memory —
`CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer target))`, because
indirect memory here is untyped and `stind.i`/`ldind.i` coerce to a native-int template.

The *routing* is `IlMachineState.writeIndirectPrimitiveStore`, which is what the guest's own
`stind.i` uses, and not `writeManagedByrefBytesOrTypedCell`. That distinction is not cosmetic:
the latter has typed-cell fast paths only for stack-memory, native-memory and array-element
roots, and falls through to `CliType.ToBytes` for everything else — which for a real caller,
whose out-parameter is a *local*, fails with "refusing to express pointer as bytes".
`writeIndirectPrimitiveStore` asks the whole-cell question per root kind, so a pointer reaches
a local's slot the same way the guest would have put it there.

The real caller's destination is `&_buffer`, a pinned pointer field of the
`SocketAsyncEngine` object (`ldflda _buffer; stloc.3; ldloc.3; conv.u`), so the byref names a
field of a managed object rather than native memory. That is why the write goes through the
byref writer rather than `NativeMemoryPool.writeCell` as `AppContextSeed` can.

## 6. The pair

`FreeSocketEventBuffer` is implemented in the same change as `CreateSocketEventBuffer`, not
after it. Implementing only the create half is the shape
`half-implementing-a-guarded-pair-hides-the-crash` warns about from the other direction: a
guest that allocated a buffer and released it would meet an unimplemented-native abort on a
path the real runtime completes, and `SocketAsyncEngine`'s own failure path
(`FreeNativeResources`, reached from the constructor's catch) does exactly that.

The free half is `free(2)` and nothing else, so it defers to the same
`NativeCall.tryResolveNativeHeapFreeTarget` that `SystemNative_Free` uses: null is a no-op,
a live block is released, a pointer naming no native block is a `failwith` rather than a
silent success. The last of those is a deliberate divergence from the C, which would return
SUCCESS after corrupting the heap; PawPrint reports the corruption instead.

## 7. Guest observability, honestly

| fact | observer |
| --- | --- |
| EFAULT for null buffer / negative count | differential guest |
| SUCCESS and a non-null, writable pointer for `count > 0` | differential guest |
| SUCCESS and a non-null pointer for `count == 0` | differential guest |
| distinct pointers from two calls | differential guest |
| `FreeSocketEventBuffer(NULL)` is SUCCESS | differential guest |
| allocate / free / allocate again all succeed | differential guest |
| ENOMEM, and `*buffer` nulled, for an unrepresentable count | PawPrint-only guest (§4) |
| the element size, *exactly* | no guest observer — unit test on the machine state |
| the element size, *indirectly* | PawPrint-only guest, via §4's boundary |

The last two rows are the ones that need care.

Directly, a guest can prove the block is *at least* `count × 16` bytes wide by writing and
reading that range — and that does kill an element-size-too-small mutation, because
PawPrint's write would leave the block and fail loudly. It cannot prove the block is not
*wider*: reading past the end aborts the run rather than returning a code the guest could
report, and the two flavours' answers are ordered, so the Linux stride is a prefix of the
Darwin one. So the exact width is pinned the way `layout-kind-gate-has-no-guest-observer`
prescribes: the guest stores the returned pointer in a static field, and the test reads that
static (as `TestBoxedAllocationParity` does), destructures it to a `NativeMemoryBlockId`, and
asserts `NativeMemoryPool.blockSize` is exactly `count × stride` under each preset. The same
test asserts the free half, via `NativeMemoryPool.liveBlockCount`, which is documented as the
only way to observe a native-heap leak.

Indirectly, though, §4's boundary *is* a guest observer of the stride, which this plan first
had wrong. The largest representable count is `Int32.MaxValue / stride`, so it differs between
the flavours: a request for 10^8 elements is 1.6×10^9 bytes under epoll and representable,
and 3.2×10^9 under kqueue and not. So one row asserted in both directions separates the two
strides from inside a guest, which is worth having as well as the unit test — it is the shape
a future flavour would break first.

## 8. Tests

- `sourcesPure/SocketEventBufferScreening.cs` — the differential rows of §7, by exit code.
  A negative-count row must pass a *valid* buffer, and vice versa, or the other conjunct
  answers (`negative-test-may-fail-for-another-reason`). The `*buffer`-untouched claim needs
  a non-null sentinel written first, so the test distinguishes "untouched" from "nulled".
- `sourcesImpure/SocketEventBufferOom.cs` — §4's ENOMEM row with `*buffer` nulled, which no
  real runtime reproduces.
- `TestSocketEventBuffer.fs` — the element size and the leak count, per §7, under both
  presets.
- Mutation: one mutant per arm — drop the null screen, drop the negative screen, widen the
  representability boundary, return SUCCESS from the ENOMEM path, leave `*buffer` untouched on
  ENOMEM, drop the buffer-room check, answer the unresolvable-buffer refusal with EFAULT,
  Linux's stride set to Darwin's and to one byte, Darwin's set to Linux's, free's null arm made
  a failure, and free made a no-op.

  §1 says the two EFAULT conjuncts admit no reordering mutant, and that stands. But there *is*
  one ordering to test: whether the out-parameter is resolved to storage before or after the
  count is screened. The C short-circuits, so `(count = -1, buffer = (byte**)123)` answers
  EFAULT having never dereferenced `buffer`, while resolving first meets a pointer that names
  no storage. That is the defect review caught in the sibling wait handler, so it gets both a
  differential row and a mutant that moves the resolution above the screen.

  Two arms turned out to have no test until one was written for them: the refusal of a
  non-null `buffer` naming no storage, and the refusal of a destination with less than eight
  bytes of room. Both are `failwith`s, so they are asserted by catching and matching the
  message rather than by an exit code.

  The `errno` side effect of §1's fifth bullet gets two mutants of its own — the write dropped,
  and the wrong error written — and both flavour guests assert the two directions from a
  sentinel set with `SystemNative_SetErrNo`. Those rows are PawPrint-only because the ENOMEM
  they hang off is (§4), not because errno itself is unobservable.

## 9. What this unblocks

The prediction was that `SocketAsyncEngine`'s constructor would complete, spawn its background
`.NET Sockets` thread, and that thread would park in the `WaitForSocketEvents` #1060
implemented, while the entry thread continued into whatever `new Socket(...)` needs next.
Measured, by re-running rung D:

```
RungD            42         134        2.41s
    Unimplemented native method (PInvokeImpl libSystem.Native!SystemNative_Socket)
    Guest was: thread 0 (Runnable) in System.Net.Sockets.Sys.Socket at IL offset 0, called
    3 frames out from RungD.Program.Main at IL offset 3;
    thread 1 (BlockedOnSocketEvents (OpenFileDescriptionId 3L)) in
    System.Net.Sockets.Sys.WaitForSocketEvents at IL offset 0
```

So the whole engine now constructs, and the second line is the first time a *real* BCL caller
has reached the park rather than a hand-rolled guest. Rung D's first failure moves from seven
frames out to three, and the next slice on this path is `SystemNative_Socket`.
