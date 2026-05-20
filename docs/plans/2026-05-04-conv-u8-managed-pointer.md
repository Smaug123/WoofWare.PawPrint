# Plumb pointer provenance through `Conv.U8 / Conv.I8`

## Context

Re-enabling `WoofWare.PawPrint.Test/sourcesPure/InterfaceDispatch.cs` (commit on `main` as a remove from the `unimplemented` list) trips the failure

```
System.Exception : Conv_U8: refusing to convert managed pointer
  <<PE data System.Private.CoreLib ... managed resource System.Private.CoreLib.Strings.resources
    at 812996 size 192483> as System.Byte>
   at WoofWare.PawPrint.EvalStackValueModule.failReferenceConversion ... EvalStack.fs:61
   at WoofWare.PawPrint.EvalStackValueModule.convToUInt64 ... EvalStack.fs:310
   at WoofWare.PawPrint.NullaryIlOpModule.execute ... NullaryIlOp.fs:1258
```

The path: the test eventually loads a BCL string resource. `AssemblyNative_GetResource` (already implemented in `Native/NativeRuntimeAssembly.fs`) returns a `byte*` pointing into a `PeByteRange`. The BCL constructs an `UnmanagedMemoryStream` over that pointer, whose `Initialize(byte*, long, long, FileAccess)` runs a wraparound check (System.IO.UnmanagedMemoryStream.cs:160 in the .NET source):

```csharp
if (((byte*)((long)pointer + capacity)) < pointer)
    throw new ArgumentOutOfRangeException(nameof(capacity), ...);
```

The IL is, verbatim:

```
LdArg1            // byte* pointer (ManagedPointer with [ReinterpretAs Byte])
Conv_U8           // (long)pointer       <- we throw here
LdArg3            // long capacity
Add
Conv_U            // (byte*)(...)
LdArg1            // pointer again
Bge_un_s skip     // skip throw if (advanced >= original) unsigned
```

Recent provenance work (`f2712de`, "Track provenance for cross-array pointer deltas") put us in a position where pointer arithmetic is symbolic; this is the next gap in that story — preserving provenance across the int64-widening / native-int truncation round-trip.

## Why this is not just a special case

The `Conv.U8 → Add → Conv.U → Bge.un` sequence is the BCL's portable wraparound idiom; it appears anywhere a managed-pointer-shaped argument needs to be advanced by a `long`. Other examples in the BCL include various `Memory<T>` / `Span<T>` constructors and several offset checks in `Buffer`/`Marshal`. Special-casing the four-instruction sequence buys nothing — we'd just hit the same failure with a different stack trace from another caller.

## Architecture sensitivity

The BCL emits this idiom because on 32-bit it really does detect wraparound:

- 32-bit: `pointer` is 4 bytes. `Conv.U8` zero-extends 32→64. After `Add capacity`, the int64 may exceed `2^32`. `Conv.U` truncates back to 32 bits, wrapping mod `2^32`. `Bge.un` against the original 32-bit pointer fires when the truncation crossed `2^32`.
- 64-bit: `pointer` is 8 bytes. `Conv.U8` is bit-preserving — the int64 *is* the address. `Add capacity` then `Conv.U` is a no-op. `Bge.un` essentially can't fire (would need ~2^63 bytes of capacity).

PawPrint is committed to a 64-bit model (`NATIVE_INT_SIZE = 8` baked throughout). On that model the wraparound branch is statically vacuous and our job is just to keep provenance flowing.

A first cut considered adding `Int64Source.ManagedPointer of ManagedPointerSource`. That conflates "a managed pointer" with "the int64 result of `conv.u8` on a managed pointer", which coincide only on 64-bit. Burying the assumption in a variant name made it invisible. Below uses a wrapper that names the *operation* (a widening), which is honest about the architecture commitment and confines it to one site.

## Plan

### 1. Extend `Int64Source` with an explicit widening variant

`WoofWare.PawPrint/CliNumericType.fs:7` currently has `Int64Source = Verbatim | SyntheticCrossArrayOffset`. Add:

```fsharp
/// The result of `conv.i8` / `conv.u8` applied to a NativeInt (managed
/// pointer, function pointer, verbatim, …). On a 64-bit interpreter this
/// widening is a no-op at the bit level; on a 32-bit interpreter it would
/// zero/sign-extend 32→64. Operations on this variant must respect that
/// asymmetry: an int64 obtained this way can in principle outgrow the
/// native word.
| WidenedNativeInt of source : NativeIntSource * signed : bool
```

Normalise on construction: `WidenedNativeInt (NativeIntSource.Verbatim n, _) => Verbatim n`. The variant exists only to track non-verbatim provenance.

Update the existing `Int64Source` helpers (`isZero`, `negate`, `shr`, `shl`, `add`, `bitAnd/Or/Xor/Not`, `isNonnegative`):
- `isZero (WidenedNativeInt (src, _))` defers to `NativeIntSource.isZero src`.
- `add` is wired through step 3.
- Everything else `failwith` with a message that names the operation and explicitly says it's an unimplemented op on widened native bits — the next idiom that hits this gets a clear breadcrumb. *Do not* silently treat `WidenedNativeInt (NativeIntSource.ManagedPointer …, _)` as bit data; that would license bit-twiddling on byrefs.

### 2. `Conv.U8` / `Conv.I8`: keep the provenance

`EvalStack.fs:300` (`convToUInt64`) and `EvalStack.fs:271` (`convToInt64`) currently fail on `EvalStackValue.ManagedPointer _` and on `NativeInt (NativeIntSource.ManagedPointer _)` (except Null). Replace those arms with `Some (Int64Source.WidenedNativeInt (src, signed))` (where `src` is `NativeIntSource.ManagedPointer ptr` for the `EvalStackValue.ManagedPointer ptr` case). The Null special-case in `convToInt64` collapses naturally — `NativeIntSource.ManagedPointer Null` widens, then `Int64Source` operations on it reduce to "zero" via `NativeIntSource.isZero`. Same shape for `EvalStackValue.NativeInt src`.

### 3. `Conv.U` / `Conv.I`: round-trip back

`toUnsignedNativeInt` / `toNativeInt` already handle `EvalStackValue.Int64`. Extend the arms so:
- `Int64 (WidenedNativeInt (src, _))` → `UnsignedNativeIntSource.FromManagedPointer ptr` / `NativeIntSource.ManagedPointer ptr` when `src` is a `ManagedPointer`, else direct return of `src`.

The existing `Conv.U` / `Conv.I` handlers (`NullaryIlOp.fs:1089`, `:1187`) already wrap the result back into an `EvalStackValue.NativeInt`. On 64-bit this is a no-op; document that the truncation step is parameterised by the (currently-fixed) word size.

### 4. `Add` (and `Sub`) for `WidenedNativeInt`

`BinaryArithmetic.fs:692`. Mirror the existing `EvalStackValue.NativeInt (NativeIntSource.ManagedPointer …) + EvalStackValue.Int32` arms (`:717`, `:733`) for `EvalStackValue.Int64 (WidenedNativeInt (NativeIntSource.ManagedPointer …, _)) + EvalStackValue.Int64 (Verbatim …)` (and the symmetric form):

- For the byref case, reuse `addInt32ManagedPtr` (which already handles `ArithmeticTarget.ByteViewTarget`, `BinaryArithmetic.fs:235`). The result keeps the `WidenedNativeInt` shape.
- Use the existing fit-into-int32 policy (`nativeIntOffsetForPointerArithmetic`, `:679`); fail loudly on offsets that don't fit. The wraparound test case has capacity 192483, well within int32. Comment that this is the 64-bit assumption surfacing — on 32-bit, oversize offsets would force truncation behaviour we currently don't model.
- For `WidenedNativeInt (Verbatim n, _) + Verbatim m`: caught by step 1's normalisation; reduces to `Verbatim (n + m)`.

`Sub`: same shape. `Mul/Div/Rem/bit-ops` on `WidenedNativeInt (NativeIntSource.ManagedPointer …, _)`: fail loudly — those aren't pointer arithmetic.

### 5. Unsigned comparisons of two byref-derived values

After step 3, the wraparound `Bge.un` sees:
- top: `EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src')` (advanced by capacity)
- next: `EvalStackValue.ManagedPointer src` (original, freshly LdArg'd)

`cgtUn` / `cltUn` (`EvalStackValueComparisons.fs:80`, `:126`) — and via `not (…)`, `cgeUn`/`cleUn` — need:

- `NativeInt (NativeIntSource.ManagedPointer …)` vs `ManagedPointer …` (and symmetric): collapse to the all-NativeInt arm. `ceq` already does this (`EvalStackValueComparisons.fs:274`); mirror.
- Two byrefs sharing root and projection prefix, differing only in trailing `ByteOffset` under a trailing `ReinterpretAs`: compare the byte offsets numerically.
- Different roots: keep the current "I've banned this case" failure (`:114`). Comparing addresses across distinct storage containers has no defensible answer in our model.

Factor a new helper `ManagedPointerSource.tryByteOffsetWithinSameRoot : src -> src -> int64 option`. `Some n` ⇔ the second is the first advanced by `n` bytes (negative if behind); `None` otherwise. This generalises `tryStableAddressBits` (`ManagedPointerSource.fs:219`). Use it from the comparison arms above.

### 6. Tests

Property tests live in `WoofWare.PawPrint.Test/TestEvalStack.fs` or `TestNativeIntSource.fs`. For each kind of byte-addressable byref root (PE byte range, localloc, array element, string char):

- `Conv.U8 → Conv.U` round-trip: result `ceq`s the original.
- `Conv.U8 → ldc.i8 n → Add → Conv.U → ldarg orig → Bge.un` for `n ∈ [0, 2^31)`: never throws, branches taken.
- `Conv.U8 → ldc.i8 n → Add → Conv.U` followed by `ldelem`/`ldind` reaching the offset address: agrees with the direct `ldelema/ldflda` path (provenance survives).

Unit test the explicit `UnmanagedMemoryStream.Initialize`-style sequence end-to-end. Also assert the wraparound branch is statically not taken — this is the documentation that the BCL check is vacuous in our 64-bit model.

`InterfaceDispatch.cs` is the integration test (already removed from `unimplemented` on the working branch).

## Order of work

1. Step 1 (Int64Source variant + helpers) + step 2 (`Conv.U8`/`Conv.I8`) + step 3 (`Conv.U`/`Conv.I`). Property test the round-trip without arithmetic.
2. Step 4 (Add/Sub). Property test the round-trip with arithmetic.
3. Step 5 (comparisons). Property test the wraparound idiom.
4. Confirm `InterfaceDispatch.cs` passes; commit on a branch and run `codex review --base main`.

Each step is independently reviewable; the property test from each step prevents the next from regressing the previous.

## Risks / called out in PR description

- **Other consumers of `(ulong)pointer`.** Loose use of pointer-bit operations in the BCL (alignment masks `& 0x7`, hash combiners, shifts) won't go through this plan. Step 1's "fail loudly with op name" requirement means the next failing idiom yields a clear "TODO: bitAnd on widened byref bits" error rather than a silent bit-twiddle. Broaden as needed; do not pre-emptively wire bit-ops to `tryStableAddressBits` in this PR.
- **Provenance erosion.** Each new arm we add (Add/Sub, Conv.U/I/U8/I8, comparisons) is a place where dropping the `WidenedNativeInt` tag would silently lose provenance. The property test in step 6 enforces the contract.
- **Same-root comparison contract.** Once non-null managed pointers can compare, be careful never to allow comparisons across distinct roots. Keep the strict same-root requirement.
- **64-bit assumption is the wrapper's whole point.** `WidenedNativeInt`'s docstring should say so out loud, and step 4's offset-fits-int32 check should reference it. If anyone later wants 32-bit support, that's the single site to revisit.
- **Spec deviation in the `Bge.un` arm.** ECMA-335 says `Bge.un` on managed pointers compares unsigned bit patterns. We're substituting "same root → compare offsets, different root → fail." This is consistent with the existing `cgtUn` "banned" stance and is a deliberate strengthening; one-line comment at the site.
