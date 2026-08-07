# Tag Bits On Type Handles

*Authorship: LLM. Status: plan, awaiting a decision on Option 1 vs Option 2.*

Follow-up to `2026-08-06-tagged-gc-handles.md`, which taught `and`/`or`/`xor` to answer honestly
on a **GC handle**. The same class of bug exists on `NativeIntSource.TypeHandlePtr` and
`NativeIntSource.MethodTablePtr`. It is **not latent**: it is reachable from a public API and
produces a wrong value today.

## The bug

`NullaryIlOp.andNativeIntAddressBits` answers a mask against a type-handle-shaped pointer by
masking the *tag* and discarding the pointer:

```fsharp
| NativeIntSource.TypeHandlePtr target ->
    NativeIntSource.Verbatim (typeHandleLowAddressBits target &&& mask) |> EvalStackValue.NativeInt
| NativeIntSource.MethodTablePtr _ ->
    NativeIntSource.Verbatim (0L &&& mask) |> EvalStackValue.NativeInt
```

`typeHandleLowAddressBits` returns the CoreCLR tagged-pointer marker — `2` for TypeDesc-shaped
targets (byref / pointer / function-pointer / generic parameter), `0` otherwise. So the answer is
correct only when the mask forces the whole (unmodelled) base address to zero. For a
**base-preserving** mask the true answer is a pointer and PawPrint silently returns a small
integer, usually `0`.

`MethodTablePtr` is worse: its answer is the constant `0` for *every* mask, including `& -1`.

### Reproduction

`RuntimeTypeHandle.FromIntPtr` is public, and `RuntimeTypeHandle.ToIntPtr` is already an
allowlisted safe intrinsic that yields exactly a `TypeHandlePtr`:

```csharp
IntPtr raw = RuntimeTypeHandle.ToIntPtr(typeof(int*).TypeHandle);
RuntimeTypeHandle rth = RuntimeTypeHandle.FromIntPtr(raw);
Console.WriteLine(Type.GetTypeFromHandle(rth).Name);
```

Under PawPrint today:

```
TODO: ldflda System.Runtime.CompilerServices.TypeDesc::_exposedClassObject through native pointer 0
```

The path is `FromIntPtr` → `GetRuntimeTypeFromHandleMaybeNull` → `GetRuntimeTypeFromHandle`
(`src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs:50`), whose managed IL is:

```csharp
TypeHandle h = new((void*)handle);
return (h.IsTypeDesc
    ? h.AsTypeDesc()->ExposedClassObject
    : h.AsMethodTable()->AuxiliaryData->ExposedClassObject) ?? GetRuntimeTypeFromHandleSlow(handle);
```

- `IsTypeDesc` is `((nint)m_asTAddr & 2) != 0` — mask `2`, base forced to zero, answered
  correctly today (`Verbatim 2` → true).
- `AsTypeDesc()` is `(TypeDesc*)((nint)m_asTAddr & ~2)` — mask `-3`, **base-preserving**.
  Today: `Verbatim (2L &&& -3L)` = `Verbatim 0`, a null TypeDesc pointer.

It failed loudly only by luck: `ldflda` through native pointer `0` happens to be an unimplemented
TODO. A mask that left a nonzero residue (say `& ~1` on a TypeDesc handle → `Verbatim 2`) would
produce a plausible-looking non-null pointer and the failure would move somewhere much less
obvious.

`Type.GetTypeFromHandle(RuntimeTypeHandle)` does *not* reach this: modern `RuntimeTypeHandle`
wraps a `RuntimeType` object reference, so `typeof(int*)` alone is fine (verified — it prints
`Int32*`). The `IntPtr` overloads are the entry points that matter.

## Scope: only `and` is wrong

Checked all the bit operations on these two pointer kinds:

| Op | Behaviour on `TypeHandlePtr` / `MethodTablePtr` | Verdict |
|---|---|---|
| `and` | `Verbatim (tag &&& mask)` | **silently wrong** for a base-preserving mask |
| `or` | falls through to `failwith "can't do binary operation on non-verbatim native int"` | loud; unsupported, not wrong |
| `xor` | `PointerHashSynthesis.materialiseHashBits` → `OpaqueHashBits` | lossy **by documented contract**; not wrong |
| `not` | same hash-synthesis route, same documented contract | not wrong |

So the fix is confined to `andNativeIntAddressBits`. `or`/`xor` could later gain exact arms for
the same reason the GC handle did, but nothing needs them and they are not lying today.

## A second, smaller problem: the tag rule is duplicated

`NullaryIlOp.typeHandleLowAddressBits` and `PointerHashSynthesis.lowBitsForKey` are two copies of
the same "which targets carry bit 1" rule, kept in sync only by a comment that says *"Mirrors
`NullaryIlOp.typeHandleLowAddressBits`"*. Whichever option is chosen, hoist this into one
function so the tag rule has a single home. Cheap, and it is a prerequisite for stating the
model honestly in one place.

## The model

Identical in shape to the GC handle case, so `TaggedPointerBits` applies unchanged:

> a type handle's value is `base ||| tag`, where `base` is unknown but has its low bits clear
> (MethodTable and TypeDesc are both at least pointer-aligned, so bits 0–2 are provably clear —
> claiming a 2-bit region is conservative and true), and `tag` is known: `2` for TypeDesc-shaped
> targets, `0` otherwise.

The one structural difference from a GC handle, and the crux of the decision below:

> **A GC handle's tag is independent state** that managed code sets and clears at will.
> **A type handle's tag is a function of its target** — `IsTypeDesc` is determined by *what the
> handle points at*, not by anything the guest chose.

So `Retagged t` where `t` differs from the target's derived tag is not "the same handle, retagged";
it is *a different kind of pointer*. That is exactly what `AsTypeDesc()` produces, and it is the
only case the current representation cannot express.

Running `TaggedPointerBits.bitAnd 2 tag mask` gives, for the masks that actually occur:

| Source | mask | result | meaning |
|---|---|---|---|
| `TypeHandlePtr` (TypeDesc, tag 2) | `2` | `TagBitsOnly 2` | `IsTypeDesc` → true ✓ (unchanged) |
| `TypeHandlePtr` (MethodTable, tag 0) | `2` | `TagBitsOnly 0` | `IsTypeDesc` → false ✓ (unchanged) |
| `TypeHandlePtr` (MethodTable, tag 0) | `-3` | `Retagged 0` | same pointer — **wrong today (`0`)** |
| `TypeHandlePtr` (TypeDesc, tag 2) | `-3` | `Retagged 0` | the TypeDesc pointer — **wrong today (`0`)**, and not representable |
| `MethodTablePtr` (tag 0) | `-1` | `Retagged 0` | same pointer — **wrong today (`0`)** |
| `MethodTablePtr` (tag 0) | `3` | `TagBitsOnly 0` | alignment ✓ (right today, by accident) |
| either | `4` | `NotStatable` | refuse |

## Options

### Option 1 — refuse what cannot be represented

Route both arms through `TaggedPointerBits.bitAnd` with the derived tag, and:

- `TagBitsOnly bits` → `Verbatim bits`.
- `Retagged t` where `t` **equals** the derived tag → return the original source unchanged, so
  identity survives.
- `Retagged t` where `t` **differs** → `failwith`: PawPrint cannot represent this handle with
  its tag changed.
- `NotStatable` → `failwith`.

**Cost:** ~30 lines plus the `lowBitsForKey` deduplication. No new DU case, no ripple.

**Fixes:** `mt & -1`, `th & ~3` on MethodTable-shaped handles, and every mixed mask (now refused
rather than answered wrongly).

**Does not fix:** `AsTypeDesc()`. `RuntimeTypeHandle.FromIntPtr` on a TypeDesc-shaped handle
still fails — but at the actual cause, with a message naming it, instead of one step later
through a null pointer.

**Blast radius:** near zero. Fully reversible.

### Option 2 — Option 1, plus a `TypeDescPtr` identity

As Option 1, but add `NativeIntSource.TypeDescPtr of RuntimeTypeHandleTarget`, and let
`Retagged 0` on a TypeDesc-shaped `TypeHandlePtr target` produce `TypeDescPtr target`. That is
precisely what CoreCLR means by `AsTypeDesc()`: the same target, viewed as its TypeDesc rather
than as a tagged handle.

This turns out to be cheaper than it first appears, because the field projection it needs mostly
exists. `ByrefRoot.MethodTableExposedClassObject of declaringType : RuntimeTypeHandleTarget`
already models the cached `RuntimeType` cell for the MethodTable branch
(`AsMethodTable()->AuxiliaryData->ExposedClassObject`). `TypeDesc::_exposedClassObject` is the
same cached `RuntimeType` for the same target, so the TypeDesc branch can route to the same
byref root — renamed to drop the now-inaccurate `MethodTable` prefix.

**Cost:** a new `NativeIntSource` case ripples mechanically through `EvalStack`, `CliType`,
`CliNumericType`, `PointerHashSynthesis`, `EvalStackValueComparisons`, and the `ldfld`/`ldflda`
arms. The compiler finds every site.

**Fixes:** everything in Option 1, plus `RuntimeTypeHandle.FromIntPtr` end-to-end.

**Keeps the classifier truthful**, which Option 1 also does but a tempting third variant does
not — see below.

**Blast radius:** moderate but mechanical. Reversible with effort.

### Option 3 — store the tag on `TypeHandlePtr` (rejected)

Mirror the GC handle exactly: `TypeHandlePtr of RuntimeTypeHandleTarget * tag : int64`.

Rejected. The tag is a *function of the target*, so a stored tag can contradict it — an illegal
state made representable, which is the opposite of what the codebase asks for. And
`TypeHandlePtr (Pointer int, tag = 0)` is a TypeDesc pointer wearing a type-handle costume:
downstream code matching on `TypeHandlePtr` would treat it as a handle. Option 2 names the same
thing correctly.

### A variant worth naming and rejecting

One could make `& ~2` on a TypeDesc-shaped handle a **no-op on the representation** — return
`TypeHandlePtr target` unchanged, on the grounds that PawPrint never materialised the tag anyway,
and teach the `TypeDesc::_exposedClassObject` projection to accept a `TypeHandlePtr`. Very cheap,
and it would make the repro pass.

Rejected because it breaks the classifier: `IsTypeDesc` on the *result* of `AsTypeDesc()` would
re-derive tag `2` and still say true, where CoreCLR says false. AGENTS.md is explicit that a
classifier used to justify a later operation must stay truthful. It also collapses two distinct
identities into one case, so `TypeHandlePtr` would no longer mean one thing.

## Recommendation and the open question

Option 1 is unambiguously the bug fix, and I would do it either way — it is the part that stops
PawPrint asserting a wrong answer.

Option 2 is Option 1 **plus a new feature** (a TypeDesc pointer identity and its projection).
AGENTS.md is explicit that I should not fold a dependency feature into a bug fix without asking,
so this is the decision I want confirmed:

1. **Option 1 only** — land the honesty fix, leave `RuntimeTypeHandle.FromIntPtr` failing loudly
   with a clear message, and file the TypeDesc identity as its own piece of work. Smallest
   reviewable change; matches "one feature at a time" most strictly.
2. **Option 1 now, Option 2 immediately after** as a second PR stacked on it. Same end state,
   two reviewable chunks.
3. **Option 2 as one change** — if the `ExposedClassObject` reuse makes it small enough in
   practice that splitting is artificial.

My preference is **(2)**: the honesty fix stands on its own and is worth landing regardless, and
the TypeDesc identity then has a clean, self-contained PR with the end-to-end repro as its test.

## Testing plan (either option)

- **Property test**, extending `TestTaggedPointerBits.fs`: the existing bit-by-bit oracle already
  covers the arithmetic. Add the per-kind derived-tag rule as a property — for every
  `RuntimeTypeHandleTarget`, the tag is in `[0, 4)` and agrees between the (now single) tag-rule
  function and `PointerHashSynthesis`'s low bits.
- **Opcode-level tests** in `TestNullaryIlOp.fs` mirroring the real IL: `th & 2` (both tag
  values), `th & -3`, `mt & -1`, `mt & 3`, and a mixed mask refusing loudly. Each assertion to be
  broken deliberately once to confirm it can fail, as with the GC handle work.
- **End-to-end**: the `RuntimeTypeHandle.FromIntPtr` repro above as a `sourcesPure` case. Under
  Option 1 it stays in `unimplemented`; under Option 2 it passes. Its assertions hold on real
  .NET either way.
