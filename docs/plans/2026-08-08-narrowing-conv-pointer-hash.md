# Plan: narrowing integer conversions synthesise pointer-hash bits

Date: 2026-08-08
Status: Implemented
Branch: `conv-i4-widened`
Builds on `docs/plans/2026-05-13-castcache-synthetic-hash-bits.md` and
`docs/plans/2026-05-14-pointer-hash-counter-strategy.md`.

## The gap, verified

`sourcesPure/MakeGenericMethodConstraintSatisfied.cs` fails with

```
TODO: Conv_I4 from widened native int <method ID 17> (truncating pointer-shaped int64 to int32)
  at EvalStackValueModule.convToInt32 (EvalStack.fs:358)
```

The guest stack at the failure (obtained by instrumenting the `Conv_I4` arm to
dump the frame chain) is:

```
Int64.GetHashCode @ IL 2
IntPtr.GetHashCode @ IL 11
RuntimeMethodInfo.GetHashCode @ IL 11
CerHashtable`2.GetHashCodeHelper @ IL 26
Table.Insert @ IL 6
CerHashtable`2.set_Item @ IL 61
RuntimeTypeCache.GetGenericMethodInfo @ IL 191
RuntimeType.GetMethodBase @ IL 431
RuntimeMethodInfo.MakeGenericMethod @ IL 188
Program.Main @ IL 35
```

So: reflection is inserting the freshly-bound `RuntimeMethodInfo` into a
hashtable keyed by its method handle. On 64-bit, `IntPtr.GetHashCode` is
`long value = _value; return value.GetHashCode()` (IntPtr.cs:90-97) and
`Int64.GetHashCode` is `unchecked((int)((long)m_value)) ^ (int)(m_value >> 32)`
(Int64.cs:106-109). The `>> 32` half already works — `Int64Source.shr`
materialises hash bits and tags them `OpaqueHashBits`, and `conv.i4` of
`OpaqueHashBits` is an allowed truncation. The `(int)l` half is the unimplemented
arm.

This is the same demand-site shape the counter-synthesis design was built for:
the pointer is being consumed as a number by something that only needs
distinctness and stability.

## The matrix

`EvalStack.fs`'s conversions already implement a coherent policy per destination
domain. Written out, with the gap marked:

| source | → int64 / native int | → int8/16/32 (unsigned variants alike) | → float |
| --- | --- | --- | --- |
| `Verbatim` | verbatim | truncate | convert |
| `NativeInt` pointer/handle shape | wrap as `WidenedNativeInt` (provenance kept) | **refuse** | refuse |
| `Int64Source.WidenedNativeInt` | unwrap back to the source | **TODO ← the gap** | refuse |
| `Int64Source.OpaqueHashBits` | keep the tag | truncate | refuse |
| byref (`ManagedPointer`) | wrap, provenance kept | `Int32Source.NarrowedManagedPointer` (i4/u4 only; refuse at 8/16) | refuse |
| `SyntheticCrossArrayOffset` | keep | refuse | refuse |

Two cells are wrong, and they are the same cell twice: narrowing a pointer-shaped
value to a sub-native integer width. Note that `(int)(long)ptr` and `(int)ptr`
are one guest operation spelled two ways — CoreLib itself picks between them by
`#if TARGET_64BIT` in this very method — so any answer that treats them
differently is wrong by construction.

The float column stays refusals: a float destination is not a bit-preserving
narrowing, so synthesised bits arriving there would be laundered into a domain
where no later operation can recognise them.

`conv.ovf.*` also stays refusing, including for `OpaqueHashBits` (NullaryIlOp.fs:563
and siblings). A *checked* narrowing asks whether the pointer's magnitude fits in
the destination; synthesised bits are small counters, so we would answer "fits"
where a real 64-bit address would overflow. That is a question we cannot answer,
so we should keep crashing rather than answer it wrongly.

## Options considered

### A. Thread `PointerHashCounters` into the narrowing conversions (recommended)

`convToInt8 / convToInt16 / convToInt32 / convToUInt8 / convToUInt16 / convToUInt32`
each take and return `PointerHashCounters`, exactly as `Int64Source.shr` /
`bitXor` / `negate` already do. The `WidenedNativeInt` arm becomes
materialise-then-truncate; the `NativeInt` pointer-shape arm switches from
refusal to the same materialisation.

Call sites: 6 op arms in `NullaryIlOp.fs`, 2 in `Intrinsics.fs` (`Interlocked`
`executeInt32`), all of which already hold `state`.

### B. Normalise at the op arm, keep the conversions pure

Add `materialiseForNarrowing : PointerHashCounters -> EvalStackValue -> EvalStackValue * PointerHashCounters`
which rewrites `WidenedNativeInt`/pointer-shaped `NativeInt` into
`OpaqueHashBits`, and call it in each op arm before the existing pure conversion.
The existing `OpaqueHashBits` arms then do the work unchanged, so `EvalStack.fs`'s
conversion family need not change at all.

Rejected, for two reasons:

1. It leaves the `WidenedNativeInt` arms in all six functions as unreachable
   `failwith "TODO"`s guarded only by caller discipline. A caller who forgets the
   normalisation gets a runtime failure claiming a gap that isn't one. Option A
   makes the same requirement a type error.
2. It cannot express the byref sub-case. `conv.i4` of a byref narrows to
   `Int32Source.NarrowedManagedPointer`, which preserves more than hash bits do
   (a mask against it is still answerable) and which `materialiseHashBits`
   deliberately refuses to synthesise. Only a function that knows its destination
   width can route `WidenedNativeInt (ManagedPointer p)` to `narrowByrefTo32` at
   32 bits and refuse it at 8 and 16. Under B that routing would have to be
   duplicated outside the conversion, or left broken.

### C. Make the hash an intrinsic

Implement `IntPtr.GetHashCode` (or `RuntimeMethodInfo.GetHashCode`) as a native
so the conversion is never reached. Rejected: AGENTS.md's rule is to implement
the primitive boundary rather than the managed method that happens to reach it,
and the primitive here is a conversion opcode. It would also leave every other
guest that truncates a handle still broken.

### D. Materialise eagerly at `conv.i8`

Have `conv.i8` / `conv.u8` of a pointer produce `OpaqueHashBits` immediately,
deleting `WidenedNativeInt` as a concept. Rejected: `WidenedNativeInt` exists so
that `conv.i8` followed by `conv.i` round-trips back to the original pointer
(`toNativeInt`, EvalStack.fs:290), which real guest code relies on. Eager
synthesis would break that round-trip, and would need the same counter plumbing
anyway — it is strictly more invasive for strictly less information preserved.

## Design (option A)

### 1. `materialiseHashBits` learns the placeholder case

`PointerHashSynthesis.materialiseHashBits` currently refuses every non-null
`ManagedPointer`, including `ManagedPointerSource.NativeIntPlaceholder bits` —
the `Unsafe.AsRef<T>((void*)bits)` form, which *is* a bit pattern rather than an
address. `Int64Source.widenedNativeInt` already treats it as one, and so does
`nativeIntBitsForIntegerConversion`. Add the arm so the three agree; no existing
test pins the refusal.

With that, `materialiseHashBits` is total on exactly the domain the narrowing
conversions need, and refuses on exactly the domain they must refuse (real
byrefs, cross-array offsets). The six `NativeInt` arms then call it directly
instead of `nativeIntBitsForIntegerConversion`.

### 2. Split the shared bits helper

`nativeIntBitsForIntegerConversion` is used by the six narrowing conversions and
by the three float ones. The float ones must keep refusing, so rename it
`nativeIntBitsForFloatConversion` and leave it to those three callers only. Its
name is currently a slight misnomer anyway.

### 3. The six conversions

```fsharp
let convToInt32 (value : EvalStackValue) (counters : PointerHashCounters) : EvalStackValue * PointerHashCounters
```

- `WidenedNativeInt (ManagedPointer ptr, _)` → `narrowByrefTo32 convI4FromInt64 ptr`
  (i4/u4 only), so `(int)(long)byref` and `(int)byref` agree. At 8/16 bits, refuse
  as today.
- `WidenedNativeInt (src, _)` → `materialiseHashBits` then the conversion's own
  truncation, yielding `Int32Source.Verbatim`.
- `NativeInt src` → same, replacing the refusal.

The result is `Int32Source.Verbatim`, matching the existing `OpaqueHashBits` arm.
There is deliberately no `Int32Source.OpaqueHashBits`: a 32-bit value can no
longer be mistaken for a pointer on a 64-bit interpreter, and adding a third
`Int32Source` case would touch every `Int32Source.value` consumer for no
protection gained.

### 4. The vestigial `option` stays, for now

`convToInt8 / convToInt16 / convToUInt8 / convToUInt16` return `int32 option`,
and every arm returns `Some` or throws. So they become
`(int32 * PointerHashCounters) option`, matching the shape `Int64Source.negate`
already has.

That `option` is dead, but it is dead across the whole family rather than in these
four functions: *no* conversion in `EvalStack.fs` returns `None` anywhere, so all
eleven `| None -> failwith "TODO: Conv_X conversion failure unimplemented"`
branches in `NullaryIlOp.fs` are unreachable. Nor can they become reachable —
unchecked `conv.*` has no failure outcome in ECMA-335 (out-of-range
float-to-integer is unspecified but still yields a value, implemented by the
inline-IL `convI4FromFloat` helpers), and the failure that does exist, overflow,
belongs to `conv.ovf.*`, which already models it as `Result<_, unit>`
(NullaryIlOp.fs:563). A PawPrint refusal is not a candidate either: the
convention throughout is a `failwith` naming the refused shape, which says
strictly more than `None` collapsing onto a generic TODO.

Removing it is therefore a separate, uniform, mechanical change over all eleven
call sites. Doing only these four here would leave the family internally
inconsistent.

## Non-goals

- `SyntheticCrossArrayOffset` narrowing stays a `failwith`. A cross-array offset
  is a difference of two addresses PawPrint does not model; it has no bits.
- `conv.ovf.*` stays refusing, per the magnitude argument above.
- Float conversions stay refusing.

## Tests

Property tests in `TestEvalStack.fs`, over a generator of pointer-shaped
`NativeIntSource`s (reusing the shapes `TestPointerHashSynthesis.fs` generates):

1. **The widened path is definitionally materialise-then-existing-path.** For
   every pointer-shaped `src`, every signedness, and each of the six widths,
   `convToIntN (Int64 (WidenedNativeInt (src, s))) counters` equals
   `convToIntN (Int64 (OpaqueHashBits bits)) counters'` where
   `bits, counters' = materialiseHashBits src counters`. This is an oracle for
   the whole change: it says the new arm adds no new truncation logic.
2. **Both spellings agree.** `convToIntN (NativeInt src)` equals
   `convToIntN (Int64 (WidenedNativeInt (src, true)))`, and both return the same
   counters. This is the property that made cell (`NativeInt`, narrowing) worth
   fixing in the same change.
3. **Stability.** Converting the same source twice with threaded counters yields
   the same result, and the second conversion assigns no new counter.
4. **Distinctness.** Distinct canonical keys give distinct `conv.i4` results
   (bounded by the counter range where `(n+1) <<< 2` cannot alias mod 2^32).
5. **Cross-width coherence.** The 8- and 16-bit results are the low bytes of the
   32-bit result.
6. **Refusals preserved.** A real byref refuses at 8/16 bits and narrows to
   `NarrowedManagedPointer` at 32; a cross-array offset refuses at every width;
   floats refuse for both `WidenedNativeInt` and `OpaqueHashBits`.

Unit test for the specific guest shape: `Int64.GetHashCode`'s
`(int)l ^ (int)(l >> 32)` applied to a `MethodHandlePtr`, asserting the two
halves derive from one materialisation (i.e. the xor is over the *same* bits).

Mutation checks, and what each one actually killed:

| mutant | killed by |
| --- | --- |
| return `0` instead of the materialised bits | oracle, distinctness, cross-width, the `GetHashCode` unit test |
| materialise from a fresh `PointerHashCounters.empty` each call | **distinctness and the oracle only** |
| drop the byref routing on the widened path | byref refusal |
| leave the `NativeInt` arm refusing | both-spellings, stability, distinctness, cross-width |
| op arm discards the returned counters | op-arm write-back, op-arm read |
| op arm reads `PointerHashCounters.empty` instead of the state's | op-arm read |

The second row is the instructive one: a fresh counter map per call is *stable* —
every source is always "first", so it always gets counter 0 and the same bits —
so the stability property does not catch it. Only distinctness does. Stability and
distinctness are therefore both load-bearing; neither implies the other.

The last two rows are why the op-arm tests exist at all. Discarding the returned
counters in `NullaryIlOp` was **not** caught by the whole suite including the
end-to-end case: a hashtable tolerates collisions, so the guest still produced the
right answer. That is exactly the failure mode
`docs/plans/2026-05-14-pointer-hash-counter-strategy.md` names as undetectable — a
collision makes two distinct pointers compare equal under `ceq` — so it needs a
test that watches the counter state rather than the guest's output.

## End-to-end effect, measured

Spiking the `conv.i4` arm alone (materialise-then-truncate, byref routed to
`narrowByrefTo32`) makes `MakeGenericMethodConstraintSatisfied.cs` pass. It is
un-parked by this change.

`SprintfBasic` is **not** un-parked: with the spike applied it advances past
`GetMethodBase` and stops at

```
TODO: Signature_Init method signature parsing is not implemented; got non-null Numeric (NativeInt (MethodHandlePtr 25L))
```

so its park comment moves to that blocker rather than being deleted. (The
previous comment predicted this conversion would un-park it; that prediction was
wrong, which is why the run was done before the plan.)
