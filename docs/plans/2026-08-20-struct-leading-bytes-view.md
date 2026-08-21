# Narrow primitive loads through a pointer at a wider struct (`SetBitmapBit`)

## Symptom

`new StringContent("hello world")` reaches `Headers.ContentType`, whose parsing
initialises `SearchValues` state via `IndexOfAnyAsciiSearcher.TryComputeBitmap`,
which passes `(byte*)&state.Bitmap._lower` into `SetBitmapBit`. The first
`bitmap[lowNibble]` read with `lowNibble = 0` kills the run:

```
cannot view <Vector128<byte>> as a 1-byte value at offset 0: fields starting there: _lower (8 bytes)
  at CliValueType.DereferenceFieldAt
  at EvalStackValueModule.viewValueTypeAsPrimitiveWithVisited (EvalStack.fs:1053)
  at NullaryIlOpModule.executeLdind
```

## Measured shape-space (probe battery, boring types, all 42 on real .NET)

| shape | PawPrint |
| --- | --- |
| byte read/write at displacement ≥ 1 into a `ulong`-field struct (local, heap field, nested, `byte*` into `Vector128` at index 1) | passes |
| write-only `*p = 4` through a pointer at a struct (zero displacement) | passes |
| `*p` / `p[0]` **read** through a pointer at a struct whose leading field is wider than the read (`Vector128<byte>` local; own nested `struct{struct{struct{ulong}}}` via `&state.Bitmap._lower`) | **crashes as above** |

## Root cause

`executeLdind` routes a plain `ManagedPointer` load by asking
`ldindNeedsByteView target cell` (NullaryIlOp.fs:1279): "is the requested
primitive strictly narrower than the cell the byref names?" It answers by
recursing through primitive-like wrappers and comparing numeric widths — but
its `CliType.ValueType _` arm is a blanket `false`, so a narrow load over a
non-primitive-like struct is routed to the *typed cell*, and
`toCliTypeCoerced` → `viewValueTypeAsPrimitive` then has no sub-cell answer
and refuses. Displacement ≥ 1 pointers, `conv`'d (native-int-shaped) pointers,
and all `stind`s already route through the byte machinery
(`readManagedByrefBytesAs` / `writeIndirectPrimitiveStore`, the canonical
byref walk, `SymbolicBytesAt`-capable) — which is why every neighbouring
shape in the table already works.

## Fix (validated by running it: all probe stages go green, none regress)

Complete the routing predicate's missing arm:

```fsharp
| CliType.ValueType vt ->
    let targetSize = CliNumericType.SizeOf target

    targetSize < (CliValueType.SizeOf vt).Size
    && not (hasExactLeadingCell targetSize vt)
```

where `hasExactLeadingCell` asks whether a field of exactly the target's size
starts the struct, *recursively* so when that field is itself a value type —
mirroring the exact `DereferenceFieldAt 0 size` descent
`viewValueTypeAsPrimitive` performs at each level, so the byte route fires
precisely when that descent would have nothing to answer with.

The conjunct came out of Codex review of the first draft (which
routed *every* strictly-narrower load to bytes): a struct whose *leading field
exactly covers the window* — `struct { int* P; long Tail }` under `ldind.i` —
was previously served by the typed cell route, which preserves the pointer's
provenance; the byte walk refuses the struct outright for containing a
runtime pointer. Measured: the pointer-local spelling (`nint* np = (nint*)&s;
*np`) reaches the arm as a plain byref and passed before the draft and
crashed under it — a real regression, now prevented by the conjunct. (The
direct spelling `*(nint*)&s` arrives `conv.u`'d, bypasses the predicate at
NullaryIlOp.fs:1415, and crashes in the byte walk in *all* worlds — a
separate pre-existing gap.) With the conjunct, the byte view fires precisely
where the typed descent has nothing to answer with: an exact-size field at
offset 0 is what `DereferenceFieldAt 0 size` demands, so every load the old
code served keeps its route, and the byte route serves only loads that
previously crashed.

A second Codex round caught the conjunct's first, one-level spelling
under-approximating "the descent refuses": an exactly window-sized leading
field that is *itself* a decomposed struct (`Outer { Inner Head; uint Tail }`
with `Inner { ushort; ushort }`, under a `uint` load) kept the typed route
and then died one level down, where real .NET reads the spanning bytes. That
shape crashes identically on `origin/main`, so it was a pre-existing gap
rather than a regression — but it violated this plan's own governing
principle, hence the recursive spelling.

A strictly-narrower load over a struct cell is a byte view, exactly as a
strictly-narrower load over a numeric cell already is. The load then flows
through `readManagedByrefBytesAs` — the same function `p[1]` uses — so zero
and nonzero displacement share one byte machinery. (Not literally one branch:
zero-displacement plain byrefs take its `ValueNone` branch, which lacks the
trailing-view branch's cell-naming shortcuts, so the refusal *sets* at the
edges are not identical — but they agree on every shape in the probe battery,
and a single-byte template over an identity-modeled native int is served
symbolically on both.)

The comparison is strict `<`, and the edit splits `ValueType` out of the
grouped `false` arm, leaving `Bool`/`Char`/`ObjectRef`/`RuntimePointer` at
`false`. Strictness plus the exact-field conjunct give the minimal-diff
property: for equal-or-wider targets, and for windows an offset-0 field
covers exactly, the arm returns `false` exactly as the old blanket arm did —
so the byte route is entered only on shapes that previously crashed.

## Options considered

**Adopted: complete `ldindNeedsByteView` (above).** One arm, in the function
whose job is precisely this routing question; unifies `p[0]` with `p[1]` and
`ldind` with `stind` on one byte path; inherits `SymbolicBytesAt` semantics
and its refusal envelope rather than inventing a second one.

**Rejected: give `viewValueTypeAsPrimitive` a byte-image fallback
(`TryDereferenceFieldAt` + `CliValueType.BytesAt 0 size`).** This was the
first draft; review demoted it. It builds a *second, weaker* byte-read path in
EvalStack, so `p[0]` and `p[1]` on the same struct would take different
machinery with different refusal envelopes (plain `BytesAt` crashes on an
identity-modeled native int in range where the byref walk names its bytes);
it needs a `TryDereferenceFieldAt` refactor with a new RawBytes contract; and
its governing property is subtly false at reference-valued cells
(`ToBytes (ObjectRef None)` renders zeros where the view refuses).
`viewValueTypeAsPrimitive` keeps refusing for its non-pointer callers (boxed
receivers, plain coercions) — no guest has demonstrated a need there, and a
future one will fail loudly at the existing message.

**Rejected: representation change — store `Vector64/128/256` as RawBytes at
construction.** Type-keyed special-casing; forfeits field identity on every
other access; the probe battery shows the field-backed model already serves
every neighbouring shape.

## Scope guards

- Read routing only; `stind` and `writeIndirectPrimitiveStore` untouched
  (measured working).
- Equal-size and wider-than-struct targets keep today's behaviour by
  construction (`<` returns `false` there, as the old arm did). What that
  behaviour *is* varies by shape and is not all working: equal-size loads
  succeed for bare-numeric-backed fields (`struct { long }`), but a
  wrapper-backed field (`struct { IntPtr }`) dies today in the coercion's
  `TODO` arm at EvalStack.fs:806, and a pointer-shaped (`conv.u`'d) load over
  a pointer-field struct dies today in the byte walk's
  runtime-pointer refusal, because `NativeInt (ManagedPointer _)` pointers
  route to bytes unconditionally (NullaryIlOp.fs:1415) without consulting the
  predicate — both measured. Those are pre-existing, separate gaps; this PR
  neither fixes nor disturbs them.
- The `Bool`/`Char`/`ObjectRef`/`RuntimePointer` arms of the predicate are
  deliberately unchanged.
- `viewValueTypeAsPrimitive`, `DereferenceFieldAt`, `BytesAt` untouched.

## Tests

1. `sourcesPure/StructLeadingByteView.cs` (active, differential, distinct exit
   codes; observe red before the fix with today's exact message):
   - `*p` byte read at zero displacement over `struct { ulong }`;
   - `p[0] |=` read-modify-write (the `SetBitmapBit` shape);
   - the nested `&state.Bitmap._lower` pointer shape (own boring structs);
   - a 4-byte `*(uint*)p` read over the same struct (non-byte narrow width);
   - a 4-byte read spanning the two leading `ushort`s of
     `struct { ushort; ushort; uint }` (narrow width ≠ leading field width);
   - displacement-1 read and write-only `*p = x` as controls;
   - a narrow `ldind.i` over `struct { int* P; long Tail }` through a pointer
     local, whose loaded value is dereferenced — the exactly-covered-window
     regression guard: it passes only if the typed route (and its provenance)
     is retained;
   - a `uint` load over `Outer { Inner Head; uint Tail }` where
     `Inner { ushort; ushort }` — an exactly window-sized but decomposed
     leading field, which only the byte route can serve.
2. Unit test beside `TestByteViewCrossesContainer.fs`'s harness: drive
   `readManagedByrefBytesAs` directly with a zero-displacement byref at a
   hand-built nested struct cell (the hinge the fix leans on), for a 1-byte
   and a 4-byte template. The byref must be a plain Field-chain (no trailing
   `ReinterpretAs`), or the test exercises the trailing-view branch instead of
   the `ValueNone` branch the fix actually routes to.
3. Mutation battery (each named killer observed red):
   - arm reverted to `false` → guest 1;
   - comparison reversed (`>`) → guest 1;
   - exact-field conjunct dropped (i.e. the pre-review draft) → guest 1's
     pointer-struct stage, which then dies in the byte walk's runtime-pointer
     refusal;
   - conjunct recursion flattened (a nested exact-size field counted as
     answerable, i.e. the round-two draft) → guest 1's decomposed-head stage,
     which then dies in the second-level typed descent;
   - `SizeOf vt` replaced by the leading field's size → guest 1's spanning
     stage: for `struct { ushort; ushort; uint }` the mutant computes
     `4 < 2 = false`, routes to the cell, and dies in the exact-field descent,
     where the true predicate computes `4 < 8 = true` and answers from bytes.
     (A 4-byte read over `struct { uint; uint }` would NOT kill it: there the
     cell route finds the exact leading field and both routes agree.)
   - `<` → `<=`: **documented as guest-unobservable today**, not killed. The
     mutant changes routing only for equal-size loads over non-primitive-like
     structs reached through a *plain* `ManagedPointer`; every discriminating
     shape already crashes on a pre-existing gap before the two routes can
     disagree, measured twice: `struct { IntPtr }` (wrapper-backed) dies in
     EvalStack.fs:806 on the cell route in all worlds, and `*(int**)&p`
     pointer loads arrive `conv.u`'d and bypass the predicate entirely
     (NullaryIlOp.fs:1415), dying in the byte walk's runtime-pointer refusal
     in all worlds. Strict `<` is nevertheless the right constant because it
     is the no-behaviour-change choice for every non-narrow load (see scope
     guards); when the equal-size gaps are eventually filled, whichever guest
     fills them becomes this mutant's killer.
4. Full suite.

## Adjacent pre-existing gaps surfaced by review (out of scope, filed)

A third Codex round found two more shapes where the typed route crashes on a
load real .NET serves: `ldind.i` over `struct { nint Head; long Tail }`
(the leading cell is an `IntPtr` *wrapper*, and `toCliTypeCoerced`'s
native-int arm has no ValueType unwrap — the `TODO` at EvalStack.fs:806), and
an explicit-layout struct with two exact-size aliases at offset 0 (the
predicate's `exists` is satisfied by one alias while `DereferenceFieldAt`'s
winner logic hands the descent the other). Both live where this predicate
answers `false` — and wherever it answers `false`, routing is bit-identical
to the old blanket-`false` arm, so both crash identically on `origin/main`:
they are pre-existing coercion gaps, not behaviour this change introduces or
alters. They join two shapes this plan already measured in the same class
(the wrapper-backed equal-size load, and `conv.u`'d pointer loads bypassing
the predicate at NullaryIlOp.fs:1415 entirely). The class is filed as an
issue rather than widened into this change: the recurring theme — the
routing predicate must mirror the coercion's true reach — suggests the
eventual fix belongs in the coercion itself, not in ever-finer routing.

## Follow-on measurement (not in scope)

Measured after the fix: the `StringContent` probe gets past `SetBitmapBit` and
now stops at the `Vector256<T>.get_IsSupported` JIT intrinsic, reached through
`ThrowHelper.ThrowForUnsupportedIntrinsicsVector256BaseType` while
`IndexOfAnyAsciiSearcher.AsciiState` construction builds its `Vector256`
bitmap. That is a separate intrinsic gap (a `safeIntrinsics` candidate), left
for its own change.
