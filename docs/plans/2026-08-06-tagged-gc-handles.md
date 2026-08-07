# Tag bits on GC handle pointers (`and`/`or` on `GcHandlePtr`)

## Problem

`System.WeakReference` and `System.WeakReference<T>` store their GC handle in an `nint` field
with the low bits used as tags, and strip those tags on every read.

From the host CoreLib IL (`FEATURE_COMWRAPPERS` **is** compiled in — the `ComAwareBit` branches
are present in the shipped IL, confirmed via `WoofWare.PawPrint.IlDump`):

| Site | IL | Meaning |
| --- | --- | --- |
| `WeakReference::Create` | `ldloc.0; ldc.i4.1; conv.i; or` | `h \| TracksResurrectionBit` |
| `WeakReference::IsTrackResurrection` | `ldfld; ldc.i4.1; conv.i; and` | `tagged & 1` |
| `WeakReference::get_WeakHandle` | `ldc.i4.2; conv.i; and` then `ldc.i4.3; conv.i; not; and` | `tagged & 2`, then `tagged & ~3` |
| `WeakReference::get_Target` | `ldc.i4.1; conv.i; not; and` then `ldc.i4.2; conv.i; and` | `tagged & ~1`, then `& 2` |
| `GCHandle..ctor` | `handle \|= 1` | pinned marker |
| `GCHandle::GetHandleValue` | `handle & ~1` | strip pinned marker |
| `GCHandle::IsPinned` | `handle & 1` | read pinned marker |

PawPrint models a GC handle as `NativeIntSource.GcHandlePtr of GcHandleAddress`, an opaque
registry index with *no numeric value at all*. Every one of the above operations therefore
fails today. Verified repro (new `sourcesPure/WeakReferenceBasic.cs`):

```
System.Exception : can't do binary operation on non-verbatim native int <GC handle <GC handle #1>>
  at NullaryIlOp.andNativeIntAddressBits (NullaryIlOp.fs:111)
```

Note that the failure is reached *after* `GCHandle.InternalAlloc` and
`ComAwareWeakReference.ComInfo.FromObject` have both run successfully, so tag arithmetic is
genuinely the only blocker on this path.

The constraint from the user, and from `docs/developer/pointers-and-byte-representations.md`:
**do not manufacture fake bits.** We must not give GC handles a synthetic numeric address just
so `and` falls out of integer arithmetic.

## What we can honestly state

A CoreCLR GC handle is a pointer into the handle table. CoreLib itself states the alignment
guarantee it relies on (`WeakReferenceHandleTags`):

> handles are at least 2-byte aligned, so we can use one bit for tagging
> … on COM-supporting platforms a handle is at least 4-byte aligned

So the honest model of a GC handle's numeric value is

```
value = base ||| tag        where  base &&& 3 = 0,  base <> 0,  tag ∈ [0, 4)
```

with `base` **unknown** (PawPrint never invents it) and `tag` **known**. This claims exactly
what alignment guarantees and nothing more. It is also already the model the rest of the
codebase assumes: `PointerHashSynthesis.lowBitsForKey` returns `0UL` for `CanonicalPointerKey.GcHandle`
and the counter scheme `((n + 1) <<< 2)` deliberately leaves the bottom two bits free
(`TestPointerHashSynthesis.fs:226`, ``low bits are clear for GcHandlePtr``).

Given that model, masking is decidable in exactly three ways. Split a mask `m` into
`mLow = m &&& 3` and `mHigh = m &&& ~3`:

- `mHigh = ~3` (every base bit kept) → the result **is still the same handle**, retagged to `tag &&& mLow`.
- `mHigh = 0` (every base bit cleared) → the result is **exactly the tag bits** `tag &&& mLow`, an ordinary integer.
- otherwise → the result depends on bits of `base` we do not model. **Fail loudly.**

`or` needs the same three-way split, but with the roles reversed, because `1` is OR's absorbing
element: splitting an operand `v` into `vLow`/`vHigh` the same way,

- `vHigh = 0` (no base bit forced) → `Retagged (tag ||| vLow)`.
- `vHigh = ~3` (every base bit forced to 1) → the result is base-independent:
  `TagBitsOnly (~3 ||| tag ||| vLow)`.
- otherwise → `NotStatable`.

`xor` has no absorbing element, so only the base-preserving case is statable: `vHigh = 0` →
`Retagged (tag ^^^ vLow)`; anything else is `NotStatable`.

In each case the rule is mechanical: the result is `Retagged` exactly when every base bit
survives unchanged, `TagBitsOnly` exactly when every base bit is forced to a constant, and
`NotStatable` otherwise. That is sound (each stated answer holds for *every* admissible base)
and complete (a partially-preserved high region genuinely differs between two admissible
bases, so no single answer exists).

Every operation in the table above lands in a statable case:

| Operation | mask | decision |
| --- | --- | --- |
| `tagged & 1` | `mHigh = 0` | `Verbatim (tag &&& 1)` |
| `tagged & 2` | `mHigh = 0` | `Verbatim (tag &&& 2)` |
| `tagged & ~1` (`-2`) | `mHigh = ~3` | `GcHandlePtr (h, tag &&& 2)` |
| `tagged & ~3` (`-4`) | `mHigh = ~3` | `GcHandlePtr (h, 0)` |
| `h \| 1` | `v &&& ~3 = 0` | `GcHandlePtr (h, tag ||| 1)` |

## Design options

### Option A (recommended): tag lives on the `GcHandlePtr` payload

`NativeIntSource.GcHandlePtr of GcHandleAddress` becomes
`NativeIntSource.GcHandlePtr of handle : GcHandleAddress * tag : int64`, and likewise
`CliRuntimePointer.GcHandlePtr`. `GcHandleAddress` itself is untouched — identity stays
identity; the tag is a *view* the guest has imposed on it.

- Blast radius: ~30 mechanical match sites, all in-repo, all compiler-caught.
- The tag travels with the value automatically wherever `IntPtr`s travel (fields, locals,
  eval stack, `CliRuntimePointer` slots), because it is part of the value.
- Reversible: if a general mechanism is later wanted, this collapses into it.
- Cost: `GcHandleRegistry` lookups must be handed the address, not the tagged value — enforced
  by making `Native/NativeCall.gcHandleAddressOfEvalStackValue` reject a non-zero tag.

### Option C: keep the tag as auxiliary state on `GcHandleCell` in `GcHandleRegistry`

Leave `NativeIntSource.GcHandlePtr` alone and record "this handle is currently tagged 1" beside
the handle's target in the registry.

**Rejected**, because a tag is a property of a *value the guest is holding*, not of the handle.
`GCHandle.GetHandleValue(handle)` and `WeakReference.get_Target` both produce an untagged
`nint` that coexists, at the same instant, with the tagged one still sitting in
`_taggedHandle`/`_handle`. One tag per registry cell cannot express two simultaneous views of
one handle. It also puts a per-value view into process-wide state, which is exactly the
identity-versus-projection confusion `AGENTS.md` warns against.

### Option B: a general `NativeIntSource.TaggedPointer of underlying : NativeIntSource * tag : int64`

A wrapper case usable for any pointer shape with known-clear low bits.

- Would also cover `TypeHandlePtr`'s TypeDesc bit (currently a special case, see below) and
  anything future.
- But: it admits states that should be unrepresentable — `TaggedPointer (Verbatim _, _)`,
  `TaggedPointer (TaggedPointer (_, _), _)`, `TaggedPointer (ManagedPointer _, _)` — each of
  which needs a normalisation rule and an equality rule.
- More fundamentally, the other pointer kind with interesting low bits (`TypeHandlePtr`) does
  not have a *tag* in this sense at all: its low bits are a pure function of the
  `RuntimeTypeHandleTarget` (CoreCLR sets them, CoreLib only reads them, the guest can never
  store a differently-tagged one). Only `GcHandlePtr` carries a guest-controlled, mutable tag.
  A single wrapper would be conflating two different things.
- Speculative generality: today exactly one pointer kind needs a mutable tag.

**Choice: Option A.** But the *decision procedure* above is factored into its own small,
kind-agnostic module (`TaggedPointerBits`, parameterised by tag width), so that if a second
pointer kind later needs tags we reuse the arithmetic without having reached for Option B's
representation. Small orthogonal core; per-kind representation.

Explicitly rejected: giving GC handles a synthetic numeric address (e.g. `index * 8`) so that
`and` becomes ordinary integer arithmetic. That is exactly the "manufacture fake bits" move
the project has avoided elsewhere, and it would make `handle == someInt` answerable with a
fabricated answer.

## Plan

### 1. `TaggedPointerBits` (new file, after `AbstractMachineDomain.fs`)

```fsharp
/// Result of a bitwise operation against the low tag region of a pointer whose
/// numeric value PawPrint does not model. The pointer's value is modelled as
/// `base ||| tag`, where `base` is unknown, non-zero, and has its low
/// `tagWidthBits` bits clear.
[<RequireQualifiedAccess>]
type TaggedPointerBitsResult =
    /// Every bit of the unknown base survived: the result is the same pointer,
    /// carrying this tag.
    | Retagged of tag : int64
    /// Every bit of the unknown base was cleared: the result is exactly these
    /// bits, and is no longer a pointer.
    | TagBitsOnly of bits : int64
    /// The result would depend on bits of the base that PawPrint does not model.
    | NotStatable

[<RequireQualifiedAccess>]
module TaggedPointerBits =
    /// CoreCLR GC handles are at least 4-byte aligned on COM-supporting
    /// platforms; CoreLib's `WeakReferenceHandleTags` relies on exactly this.
    let gcHandleTagWidthBits : int = 2

    val tagMask : tagWidthBits : int -> int64
    val bitAnd  : tagWidthBits : int -> tag : int64 -> mask : int64 -> TaggedPointerBitsResult
    val bitOr   : tagWidthBits : int -> tag : int64 -> operand : int64 -> TaggedPointerBitsResult
    val bitXor  : tagWidthBits : int -> tag : int64 -> operand : int64 -> TaggedPointerBitsResult
```

Total, no state, no `failwith`. Callers turn `NotStatable` into their own loud error with a
site-specific message.

### 2. Widen the representation (Option A)

- `NativeIntSource.GcHandlePtr of handle : GcHandleAddress * tag : int64`
- `CliRuntimePointer.GcHandlePtr of handle : GcHandleAddress * tag : int64`
- Update: `ToString` (render the tag only when non-zero), custom equality (equal iff same
  address **and** same tag), hash, `equalsForCli` arms, and the `EvalStack` conversions. All
  compiler-caught. `isZero`/`isNonnegative` and the `cgt.un`/`clt.un`-vs-zero arms in
  `EvalStackValueComparisons` match with a wildcard and stay correct unchanged: a tagged handle
  is still non-zero.
- A helper `NativeIntSource.gcHandlePtr (h : GcHandleAddress) : NativeIntSource` for the
  common untagged construction, so producer sites read unchanged, and a
  `NativeIntSource.gcHandlePtrTagged` that asserts `tag` is inside the tag region. The only
  way to obtain a non-zero tag is from a `TaggedPointerBits` result, which is in range by
  construction; the assertion catches any future site that bypasses it.

### 3. Consumers that require an untagged handle

- `Native/NativeCall.gcHandleAddressOfEvalStackValue`: accept only `tag = 0`; otherwise fail
  loudly ("expected an untagged GC handle, got …"). Real CoreLib always strips before calling
  `_InternalFree` / `InternalSet` / `InternalCompareExchange`.
- `NullaryIlOp` `Ldind_ref` on `GcHandlePtr` (this is release CoreLib's
  `GCHandle.InternalGet` = `*(object*)handle`): accept only `tag = 0`; a tagged deref is a
  misaligned read in reality and must not silently succeed.

### 4. `And` / `Or` / `Xor`

In `NullaryIlOp.fs`:

- `andNativeIntAddressBits` grows a `GcHandlePtr` arm driven by `TaggedPointerBits.bitAnd`.
- `Or` grows `GcHandlePtr` arms (against `Int32` and against `NativeInt (Verbatim _)`, both
  operand orders) driven by `TaggedPointerBits.bitOr`. Today `Or` rejects *every* non-verbatim
  native int.
- `Xor`'s `xorNativeIntSources` grows `GcHandlePtr` × `Verbatim` cases **in both operand
  orders** (the function matches positionally, so one order alone would leave the other
  silently falling through). This is not needed by `WeakReference`, but the current fallback
  routes a handle through `PointerHashSynthesis` and returns `OpaqueHashBits` — i.e. it
  **silently** discards handle identity for an operation that now has an exact answer. Adding
  the arms narrows an existing silently-lossy path rather than adding new capability.
- `Not` on a handle stays as it is (`~handle` genuinely depends on unknown base bits; the
  existing `OpaqueHashBits` behaviour and its doc comment already say so).

### 5. Not in scope: the `TypeHandlePtr` / `MethodTablePtr` arms of `andNativeIntAddressBits`

An earlier draft proposed tightening these in passing. They return `Verbatim (lowBits &&& mask)`
and `Verbatim (0L &&& mask)` respectively, which is right only when the mask clears every high
bit; for a base-preserving mask they silently answer `0` where the true answer is a pointer.
Confirmed by grep that no test and no reachable guest IL path exercises those arms with any
mask today (CoreCLR's `TypeHandle::AsMethodTable()` is native C++, not managed IL), so this is
a latent-but-unreachable issue about a different pointer kind. **Split out as follow-up work**
rather than mixed into this change.

### 6. `PointerHashSynthesis`

`CanonicalPointerKey.GcHandle` keeps carrying only the `GcHandleAddress` (identity, not view).
`materialiseHashBits` ORs the source's tag onto the counter-derived bits. Since
`lowBitsForKey (GcHandle _) = 0UL` and the counter scheme shifts left by 2, this is
non-colliding by construction and makes two differently-tagged views of one handle produce
bit patterns that differ exactly in the tag — which is what reality does.

### 7. Tests (written first)

1. **Property test**, `TestTaggedPointerBits.fs`, against a concrete-integer oracle. Generate a
   concrete `base` (non-zero, low `w` bits clear), a `tag`, and a `mask`/`operand`. The
   generator must *not* sample the operand uniformly: `Retagged`/`TagBitsOnly` fire only on the
   knife-edge conditions `high = 0` / `high = ~tagMask`, which a uniform `int64` essentially
   never hits, so the interesting branches would be tested vacuously. Build the operand as
   `low ||| oneOf [0L; ~tagMask; arbitrary high bits]`. Then:
   - `Retagged t'` ⟹ for **every** such base, `(base ||| tag) op v = base ||| t'`.
   - `TagBitsOnly b` ⟹ for **every** such base, `(base ||| tag) op v = b`.
   - `NotStatable` ⟹ *completeness*: exhibit two bases whose results are neither equal to each
     other (so no `TagBitsOnly` answer exists) nor of the form `base ||| t` for one fixed `t`
     (so no `Retagged` answer exists). i.e. we never refuse an answer we could have given.
   - Round-trip: `bitAnd w tag (tagMask w |> (~~~))` is always `Retagged 0`.
2. **Unit tests** at the `EvalStackValue` level (`TestNullaryIlOp.fs`) for each row of the table
   in "What we can honestly state", plus the loud refusals (`h & 4`, `h | 8`), plus
   `Ldind_ref`/`gcHandleAddressOfEvalStackValue` rejecting a tagged handle.
3. **Round-trip** in `TestEvalStack.fs`: a tagged handle survives
   `EvalStackValue ↔ CliType` in both directions with its tag.
4. **End-to-end** `sourcesPure/WeakReferenceBasic.cs` (already written and confirmed failing):
   covers `WeakReference` and `WeakReference<T>`, tracking and non-tracking, `Target` get/set,
   `TryGetTarget`, `SetTarget`, `IsAlive`, `TrackResurrection`. Every assertion it makes is
   also true on real .NET.
5. Update `docs/developer/pointers-and-byte-representations.md` — the "Low address bits" and
   "Extension Rules" sections — to record the tag-region model.

## Risks / open questions

- **Does the end-to-end test reach further gaps?** The repro shows the path gets past
  `InternalAlloc` and `ComInfo.FromObject`, and `DependentHandleBasic.cs` already exercises
  `InternalSet`/`InternalGet`/`InternalFree`, so the remaining surface is small — but if
  `WeakReference<T>` hits an unrelated blocker, per AGENTS.md the right move is to keep the
  unit and property coverage, park the end-to-end case in `unimplemented` with a precise note,
  and not chase the rabbit hole.
- **Tag width 2 vs 3.** 2 is what CoreLib itself claims and what `PointerHashSynthesis` already
  assumes. Claiming fewer bits is the conservative direction (it only ever makes us fail more
  loudly), so 2 it is.
- **PawPrint has no GC**, so a weak reference never dies. That is a pre-existing, documented
  property of the model (`GcHandleRegistry`), not something this change alters.
