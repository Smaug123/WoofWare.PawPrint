# Plan: synthesise hash bits from pointer-shaped widened int64s

Date: 2026-05-13
Author: Claude
Status: Proposed
Branch: `castcache-bit-twiddling-blocker`

## Context

`NullDereferenceTest.cs` enters
`System.Runtime.CompilerServices.CastCache.TryGet(int[], nuint, nuint)`
and trips on the first bit-twiddling step in `KeyToBucket`:

```
System.Exception : TODO: refusing to shl widened native int <type ID 184>
                   (bit-twiddling on pointer bits)
   at WoofWare.PawPrint.Int64SourceModule.shl (CliNumericType.fs:84)
   from NullaryIlOp.fs:978 (Shl)
```

The reaching IL is `KeyToBucket`
(`dotnet-runtime/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/CastCache.cs:87-101`):

```csharp
nuint hash = BitOperations.RotateLeft(source, (nuint.Size * 8) / 2) ^ target;
return (int)((hash * 11400714819323198485ul) >> hashShift);
```

`source` and `target` originate at `CastHelpers.cs:70,216,243,265,475,507`
as `(nuint)methodTablePtr`. In PawPrint these are
`NativeIntSource.MethodTablePtr (ConcreteTypeHandle)` — opaque
provenance-tracked handles, not bit patterns. After `conv.u8`
they live on the eval stack as
`Int64Source.WidenedNativeInt (MethodTablePtr _, signed)`.

`Int64Source.shl/shr/bitAnd/bitOr/bitXor/bitNot/negate` (CliNumericType.fs
lines 58-125) deliberately refuse to bit-twiddle `WidenedNativeInt`:
fabricating bits would either silently lose provenance or break the
"conv.i8 then conv.u" round-trip invariant that lets us deref a pointer
that has been widened to int64 and back.

An earlier draft (now removed) proposed treating `CastCache.TryGet`
as if it were a runtime intrinsic and short-circuiting it to
`MaybeCast`. That framing was misleading — `TryGet` carries no
`[Intrinsic]` attribute, it has a real managed body, and substituting
it at the call boundary tells a user-visible lie about which managed
code we executed. The user chose this plan instead, which faithfully
executes the cache's hashing while preserving the provenance discipline.

## Design

### The fundamental observation

`KeyToBucket`'s output flows to exactly one consumer: `Element(tableData, index)`,
where `index` is `(int)((hash * c) >> shift)` — a 32-bit array index.
The hash never round-trips back to a pointer; it is destroyed by
`conv.i4` and consumed as an integer index. The downstream entry
comparison `entrySource == source` does *not* compare hash bits: it
compares the *stored* entry (an nuint read out of the array) against
the live `source` argument, both as pointer-shaped values. The
provenance discipline only matters along the path that constructs
the hash.

So the design only needs to:

1. Let `conv.i8`/`conv.u8` of a pointer-shaped NativeInt continue to
   produce a `WidenedNativeInt` (unchanged — pointer round-trip stays
   honest).
2. When a bit-mixing operation (shl/shr/bitXor/bitAnd/bitOr/bitNot/mul)
   fires on a `WidenedNativeInt`, transition the value to a new tagged
   variant carrying deterministic synthesised bits. Further bit ops on
   the tagged value compute on those bits directly.
3. Allow `conv.i4` (and only `conv.i4`) to extract the low 32 bits of
   the tagged variant as a plain `Int32`. The result is a deterministic
   index with no provenance — which is fine: array indices don't need
   provenance.
4. **Reject** conversion of the tagged variant back into a `NativeInt`
   (e.g. via `conv.u`/`conv.i`), or any use as a managed pointer. The
   tag's job is to make "this came from a pointer hash, not a pointer"
   visible at runtime.

### New variant

```fsharp
// In Int64Source:
| OpaqueHashBits of bits : int64
```

`OpaqueHashBits 0xDEADBEEF` carries deterministic bits with no
provenance. Construction happens only via the materialisation pathway
from `WidenedNativeInt` (see below), or transitively from another
`OpaqueHashBits` via further bit ops.

### Materialisation: `WidenedNativeInt` → `OpaqueHashBits`

A new private helper in `Int64Source`:

```fsharp
let private materialiseHashBits (src : NativeIntSource) : int64 =
    // Synthesise a deterministic 64-bit pattern from the source's
    // identity. Must respect `typeHandleLowAddressBits`' contract:
    //   - MethodTable*       → low 2 bits clear (alignment)
    //   - TypeDesc-shaped    → low 2 bits = 0b10 (tag)
    // Anything that would otherwise be a plain pointer (ManagedPointer,
    // FunctionPointer, MethodTableAuxiliaryDataPtr, etc.) keeps low bits
    // clear since CoreCLR aligns those too. The upper bits are derived
    // from the source's hash to produce per-identity distinctness.
    match src with
    | NativeIntSource.Verbatim n -> n
    | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L
    | NativeIntSource.MethodTablePtr handle ->
        // Hash the handle id; clear low 2 bits to honour alignment.
        let h = int64 (hash handle) <<< 16
        h &&& ~~~3L
    | NativeIntSource.TypeHandlePtr target ->
        let low = NullaryIlOp.typeHandleLowAddressBits target
        let h = int64 (hash target) <<< 16
        (h &&& ~~~3L) ||| low
    | NativeIntSource.MethodTableAuxiliaryDataPtr _
    | NativeIntSource.FunctionPointer _
    | NativeIntSource.FieldHandlePtr _
    | NativeIntSource.MethodHandlePtr _
    | NativeIntSource.GcHandlePtr _
    | NativeIntSource.EventPipeProviderPtr _
    | NativeIntSource.EventPipeEventPtr _
    | NativeIntSource.AssemblyHandle _
    | NativeIntSource.ModuleHandle _
    | NativeIntSource.MetadataImportHandle _ ->
        let h = int64 (hash src) <<< 16
        h &&& ~~~3L
    | NativeIntSource.ManagedPointer _
    | NativeIntSource.SyntheticCrossArrayOffset _ ->
        failwith $"materialiseHashBits: provenance-preserving source %O{src} cannot be hashed"
```

Notes:
- `typeHandleLowAddressBits` currently lives `private` in `NullaryIlOp`.
  Promote it to a module-level helper (or duplicate the small DU match
  here) so `CliNumericType` can call it without a layering inversion.
  CliNumericType already references `NativeIntSource`, which is below
  it in compilation order; the only constraint is that
  `RuntimeTypeHandleTarget` is visible there. (Verify; if it isn't,
  inline the few-line match here.)
- The `<<< 16` shift is arbitrary: it ensures the synthesised bits
  exercise both halves of the int64 so `RotateLeft` produces a
  non-degenerate hash. Any deterministic non-trivial shape works.
- `ManagedPointer` (non-null) and `SyntheticCrossArrayOffset` are the
  cases where materialisation would actively destroy useful
  provenance — those should never reach `materialiseHashBits` because
  bit ops on `WidenedNativeInt (ManagedPointer _)` are handled by
  `BinaryArithmetic.execute` (offset arithmetic). If one ever does
  reach this helper, failing loudly is the right outcome.

### Operation semantics

Each bit op in `CliNumericType.fs` (`shl`, `shr`, `bitAnd`, `bitOr`,
`bitXor`, `bitNot`) gains:

```fsharp
| Int64Source.WidenedNativeInt (src, _) ->
    let bits = materialiseHashBits src
    op bits shift |> Int64Source.OpaqueHashBits
| Int64Source.OpaqueHashBits bits ->
    op bits shift |> Int64Source.OpaqueHashBits
```

Mixed `WidenedNativeInt × Verbatim` (e.g. `bitXor`) materialises the
widened side and computes against the verbatim bits — the result is
`OpaqueHashBits`. Mixed `OpaqueHashBits × Verbatim` likewise.

Multiplication of a widened value by a constant is *not* presently
handled by `Int64Source` (it goes through `BinaryArithmetic.execute`'s
`Int64 × Int64` path, which only handles `Verbatim × Verbatim`). Add
a new arm in `BinaryArithmetic.execute` (around lines 738-754) that
materialises a `WidenedNativeInt` or `OpaqueHashBits` operand into
its bits, runs `op.Int64Int64`, and returns `OpaqueHashBits`. The
existing `WidenedNativeInt(ManagedPointer _) × Verbatim` arm
(line 743) stays as is — that's managed-pointer offset arithmetic
and must not flip to OpaqueHashBits.

`negate` (line 58): never fires on hash bits in `KeyToBucket`, but for
completeness it should produce `OpaqueHashBits` for both
`WidenedNativeInt` and `OpaqueHashBits` inputs.

`add` (line 86): the existing `failwith` route for `WidenedNativeInt`
documents that pointer arithmetic must go through `BinaryArithmetic`,
not `Int64Source.add`. That stays. For `OpaqueHashBits + Verbatim`
and `OpaqueHashBits + OpaqueHashBits`, add arms returning
`OpaqueHashBits` (since these are computing on already-synthesised
bits, not real pointer arithmetic). Note `add` here uses `Checked`
arithmetic — switch to unchecked for the hash arms (matches what the
real `nuint *` would do on wraparound).

### Conversions

In `EvalStack.fs`:
- `convToInt8/16/32/64/UInt8/16/32` of `OpaqueHashBits`: produce the
  appropriate narrow `Some i`. For `convToInt32` specifically, return
  the low 32 bits as `Int32` — this is the path `KeyToBucket`'s
  `(int)` cast follows.
- `convToInt64`/`convToUInt64`: `OpaqueHashBits` round-trips as itself
  (already int64).
- `convToNativeInt` (line 244-249): **must fail** for `OpaqueHashBits`.
  The tag says "this came from a pointer hash" — converting back to a
  NativeInt would erase that lie. Match arm:
  ```fsharp
  | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits) ->
      failwith
        $"conv.i / conv.u of OpaqueHashBits %d{bits}: refusing to \
          synthesise a pointer from hashed pointer bits"
  ```
- `convToFloat`/`convR4`/`convR8`: also fail — hashes have no
  meaningful float interpretation. (None of CastCache exercises these,
  so failing loudly is safest.)

### Equality and ordering

`EvalStackValueComparisons.fs`:
- `cltSigned`/`cgtSigned` (lines 8, 45): `Int64Source.compareSigned`
  already governs these. `compareSigned` should treat `OpaqueHashBits`
  numerically (the bits are deterministic integers), so add an arm
  `Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> compare a b`
  and the mixed `OpaqueHashBits × Verbatim` arms. Likewise for
  `OpaqueHashBits × WidenedNativeInt` (materialise the widened side
  to compare).
- `cltUn`/`cgtUn` (lines 87-88, 159-160): the current code unwraps
  `WidenedNativeInt` and re-dispatches via `EvalStackValue.NativeInt`.
  For `OpaqueHashBits` we can compare as plain unsigned int64, so:
  ```fsharp
  | EvalStackValue.Int64 (Int64Source.OpaqueHashBits a),
    EvalStackValue.Int64 (Int64Source.OpaqueHashBits b) ->
      uint64 a < uint64 b
  | EvalStackValue.Int64 (Int64Source.OpaqueHashBits a),
    EvalStackValue.Int64 (Int64Source.Verbatim b) ->
      uint64 a < uint64 b
  ```
  (etc. with the Verbatim-left case).

`ceq` (somewhere else; need to grep — likely `EvalStackValue.equal` or
similar): same shape. Equal-by-bits for `OpaqueHashBits × OpaqueHashBits`
and `OpaqueHashBits × Verbatim`.

### Predicates

`Int64Source.isZero`: `OpaqueHashBits bits -> bits = 0L`.
`Int64Source.isNonnegative`: `OpaqueHashBits bits -> Some (bits >= 0L)`.

### Byte-addressability (CliType.fs)

`Int64Source.OpaqueHashBits` should be `Rejected` like `WidenedNativeInt`
and `SyntheticCrossArrayOffset` — these bits are a fiction, not a real
byte pattern. Add the arm to `int64Source` (line 73-81).

### `ToBytes`

`OpaqueHashBits` should fail to convert to bytes (mirroring
`SyntheticCrossArrayOffset`'s refusal at CliNumericType.fs:179). The
hash is computed for an in-register comparison; spilling it to memory
makes no sense and indicates a misuse.

### `ToString`

`Int64Source.OpaqueHashBits bits -> $"<opaque hash bits 0x%x{bits}>"`.

## Touch-point inventory

Files with `Int64Source` match arms that need a new `OpaqueHashBits`
case. Count is approximate (some matches are constructor-only):

| File | Approx. match arms |
| --- | --- |
| `WoofWare.PawPrint/CliNumericType.fs` | ~20 (definition + every helper) |
| `WoofWare.PawPrint/EvalStack.fs` | ~14 (all the `convTo*` helpers + `Choice2Of2`/`Choice1Of2` extractors at line 533) |
| `WoofWare.PawPrint/EvalStackValueComparisons.fs` | ~6 (cltSigned, cgtSigned, cltUn, cgtUn, possibly ceq) |
| `WoofWare.PawPrint/CliType.fs` | ~4 (byte-addressability + boxing/unboxing arms) |
| `WoofWare.PawPrint/NullaryIlOp.fs` | ~6 (locallocSizeBytes, divUnValues, fromUnsignedInt64, etc.) |
| `WoofWare.PawPrint/UnaryConstIlOp.fs` | (compareSigned + isZero only — handled centrally, likely zero additional arms) |
| `WoofWare.PawPrint/IntrinsicHelpers.fs` | 1 (line 540-545: checked-byte-count) |
| `WoofWare.PawPrint/BinaryArithmetic.fs` | 1 new arm + 1 expanded arm (`Int64 × Int64` multiplication) |
| `WoofWare.PawPrint/Intrinsics.fs` | 0–2 (only if any of the existing intrinsics destructure Int64Source directly) |
| `WoofWare.PawPrint/Native/*` | 0–4 — most natives use `Int64Source.Verbatim` constructors only, no matches |

Total: ~55–60 mechanical arms across ~7 core files. The vast majority
mirror the existing `Verbatim` arm (for ops that are numerically
well-defined on bits) or the existing `WidenedNativeInt` arm (for ops
that must reject — e.g. `ToBytes`, `convToNativeInt`).

The compiler will catch missed arms (warnings-as-errors with
incomplete pattern matches), so the inventory will be ground-truthed
during implementation.

## Test plan

### Unit-level (focused C# in `sourcesPure/`)

Add `WoofWare.PawPrint.Test/sourcesPure/CastCacheBucketing.cs` — small
C# exercising the bit-twiddling chain end-to-end through `isinst` and
`castclass` calls, without depending on NRE plumbing:

```csharp
interface IFoo { }
class Foo : IFoo { }

public static int Test1()
{
    // isinst — hits IsInstanceOfAny → CastCache.TryGet → KeyToBucket
    object o = new Foo();
    if (o is IFoo) return 0;
    return 1;
}

public static int Test2()
{
    // castclass — hits ChkCastAny → CastCache.TryGet → KeyToBucket
    object o = new Foo();
    IFoo f = (IFoo)o;
    return f is null ? 1 : 0;
}

public static int Test3()
{
    // isinst, negative branch
    object o = "hello";
    return (o is int[]) ? 1 : 0;
}
```

This is auto-discovered. If any variant trips a downstream blocker
in the cast slow paths, document it in `unimplemented` and trim the
variant. Keep the test focused on what *this* PR fixes.

### `NullDereferenceTest.cs`

The motivating test. After this PR, expected outcomes:
- Best case: it passes — promote out of `unimplemented`.
- Likely case: it trips the next blocker (entry comparison, exception
  unwinding, or somewhere in the cast slow paths). Document the new
  blocker in the `unimplemented` comment.

### Determinism check

Add (or extend) a property-style test asserting that hashing the same
`MethodTablePtr` twice produces the same `OpaqueHashBits`. This is the
core determinism contract — the cast cache lookup must be reproducible
across runs.

The simplest expression: two consecutive `isinst` operations on the
same `(o, T)` pair should reach `Element` with the same index. We can
verify this indirectly by checking that the test passes deterministically
under the existing harness (which already exercises trace-replay).

### Unit-level (F# unit tests)

Optional: a small NUnit test in `WoofWare.PawPrint.Test` that drives
`Int64Source.shl/shr/bitXor` directly on a `WidenedNativeInt(MethodTablePtr h, _)`
and asserts the resulting `OpaqueHashBits` is deterministic and is
rejected by `convToNativeInt`. This pins the contract in code rather
than relying on the C# integration tests to exercise every arm.

## Validation

1. `nix develop -c dotnet build` — clean.
2. `nix develop -c dotnet fantomas .` — formatted.
3. `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "Name~CastCacheBucketing"` — passes.
4. `nix develop -c dotnet test ... --filter "Name~NullDereferenceTest"` — pass or new-blocker.
5. Full suite: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal` — no regressions in the 689+ tests.
6. Commit on branch `castcache-bit-twiddling-blocker`. Push.
7. `codex review --base main`. Address findings.

## Risks

- **Touch-point miss.** The compiler catches missed match arms, so the
  risk is bounded to "build fails" rather than "wrong behaviour at
  runtime". Low.
- **`materialiseHashBits` determinism.** Uses `hash` (i.e.
  `Object.GetHashCode`) on the inner identity. `ConcreteTypeHandle` and
  the various handle DUs are structural records with structural hash;
  this is deterministic per-process. Across processes it would still
  be deterministic given identical assembly loads — which is precisely
  what PawPrint's determinism contract gives us. If we ever introduce
  identity types with reference-equality hashes, materialisation would
  silently differ between runs; flag this in the helper's doc comment.
- **`typeHandleLowAddressBits` cross-module access.** It's currently
  `private` in `NullaryIlOp`. Promoting it to a module-level helper
  (or inlining the few-line match into `materialiseHashBits`) is a
  small cleanup; no architectural risk.
- **Downstream blocker resurfaces.** After hashing works, the next
  step in `TryGet` is reading `pEntry._source` from the sentinel int[]
  and comparing it (as nuint) against the live pointer-shaped input.
  PawPrint may not currently support reading an nuint out of an int[]
  reinterpreted as `CastCacheEntry`. If so, that's the next plan, and
  it's exactly the kind of incremental progress AGENTS.md prescribes.
- **Performance.** `materialiseHashBits` runs once per cast (when the
  cache misses, which is every cast in PawPrint since we never write
  back). The hash is `O(handle structure)`; on a single-threaded
  interpreter this is invisible.

## Non-goals

- **Modelling cast-cache writes.** PawPrint runs no native JIT
  helpers, so the cache is intrinsically empty. Adding host-side
  memoisation would speed up casts but is orthogonal to correctness
  and adds hidden state that's tricky to reason about under
  deterministic replay.
- **General-purpose pointer-bit arithmetic outside CastCache.** This
  PR's design is targeted: bit ops on pointer-shaped widened ints
  produce hash bits, not real pointer bits. Code paths that *do* need
  real pointer-bit arithmetic (low-bit tagging schemes, alignment
  checks) should continue to use the existing `andManagedPointerAddressBits`
  / `typeHandleLowAddressBits` machinery in `NullaryIlOp`, which
  preserves provenance. If a future code path mixes "tag check" and
  "general hash", revisit then.
- **Removing `internCastCacheSentinelTable`.** The sentinel is kept
  so any caller that does `ldsfld CastHelpers::s_table` observes a
  non-null array. Removing it is a follow-up cleanup PR.
- **Round-tripping `OpaqueHashBits` back to a pointer.** Explicitly
  refused. The tag's load-bearing job is to reject this.

## Why this design over the alternatives

- **vs. call-boundary substitution of `TryGet` → `MaybeCast`:** the
  previous draft would have skipped `KeyToBucket` entirely. That's
  correct in the contract sense — `MaybeCast` means "consult the slow
  path" and managed code never writes to the cache — but it misleads
  about what code ran. A guest crash in a frame above `TryGet` would
  show a stack trace that elides the cache-lookup hop. Faithfully
  executing the bit-twiddling preserves the trace and validates more
  of the managed BCL under interpretation. The user chose this design
  on those grounds.
- **vs. materialising into `Verbatim` directly:** plain `Verbatim`
  bits could be converted back to a `NativeInt` (via `conv.u`) and
  then potentially dereferenced through `executeLdind` if a future
  code path tried. The tag explicitly closes that leak. The cost is
  ~60 mechanical match arms; the benefit is that "this int came from
  hashing a pointer" is a runtime-checked claim instead of an
  unenforced convention.
- **vs. promoting the cast helpers to intrinsics:** broader than
  necessary. The cast slow paths (`*_NoCacheLookup`) contain real
  cast-decision logic — variance, interface dispatch, type-equivalence
  fallbacks — that we want to run faithfully through the interpreter.
  The cache is the only artificial bit underneath them; this PR
  targets exactly the cache.
