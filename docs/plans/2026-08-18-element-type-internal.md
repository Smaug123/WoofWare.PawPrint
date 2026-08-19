# Plan: ELEMENT_TYPE_INTERNAL in Reflection.Emit signature blobs

Date: 2026-08-18
Status: Representation implemented (this branch). The signature decoders still refuse
`ELEMENT_TYPE_INTERNAL`, which is the next change.
Tracking issue: #849 ("Runtime IL emission"), stage 2 of its suggested staging.

## The gap, re-measured on `main` at `f6c60f79`

`SignatureHelper` builds a `DynamicMethod`'s signature with a null module, so it cannot
spell a type as a metadata token. For anything that is not a primitive, `object`, `string`,
`I`/`U`/`TYPEDBYREF` (`IsSimpleType`, SignatureHelper.cs:653) it takes the
`ELEMENT_TYPE_INTERNAL` branch (SignatureHelper.cs:449-452) and writes the raw bytes of the
type handle into the blob:

```csharp
AddElementType(CorElementType.ELEMENT_TYPE_INTERNAL);
IntPtr handle = type.TypeHandle.Value;
byte* phandle = (byte*)&handle;
for (int i = 0; i < sizeof(void*); i++)
    m_signature[m_currSig++] = phandle[i];
```

PawPrint's `RuntimeTypeHandle.Value` is `NativeIntSource.TypeHandlePtr target`, which has no
byte image, so the `ldind.u1` at `phandle[i]` is refused:

```
refusing byte view over value type containing non-byte-addressable field 30::_value:
native int with non-byte-addressable provenance <type ID 2> in plain byref ...
  Guest was: System.Private.CoreLib.SignatureHelper.InternalAddRuntimeType at IL offset 100
```

Two guest programs reach it today, both measured in this worktree:

1. A `DynamicMethod` whose signature or locals mention a user-defined type (the case #849
   predicted).
2. **`MethodBase.Invoke` called twice on one `MethodInfo`.** `MethodInvokerCommon` takes the
   interpreted `RuntimeMethodHandle.InvokeMethod` path only on the *first* invocation and
   builds a Reflection.Emit stub from the second onwards. #849 recorded this probe as stopping
   somewhere earlier, with no Emit assembly loaded; that earlier gap has since closed, and
   `InternalAddRuntimeType` is now its blocker. Whether anything follows it is unmeasured.

## Two halves

### Read side (not contentious)

`MethodSignatureDecoding.decode` and `LocalSignatureDecoding.decode` both drive
`System.Reflection.Metadata.SignatureDecoder`, which has no `ELEMENT_TYPE_INTERNAL` case
anywhere in the type tree — and 0x21 can appear nested (`SZARRAY INTERNAL …`,
`GENERICINST INTERNAL … n args`), so pre-scanning the blob is not an option and neither is
delegating whole types to `DecodeType`.

The alphabet a null-module `SignatureHelper` can emit is bounded and small
(SignatureHelper.cs:324-460): `VAR`/`MVAR` + position, `GENERICINST` + definition + count +
args, `BYREF`, `PTR`, `SZARRAY`, `ARRAY` + rank encoding, the simple types, and `INTERNAL`.
Notably it can emit **no metadata token at all** with a null module, so a hand-rolled walker
over exactly that alphabet needs no `MetadataReader`. That is the intended shape: a recursive
descent producing `TypeDefn`, refusing anything outside the alphabet by name.

Whatever the write side decides, the read side reverses it; that is the only coupling.

### Write side — the decision

The blob's 8 bytes have to come from somewhere. Three structurally different answers.

#### Option A — Bytes carry provenance (recommended)

Defer materialisation: a byte value can be "byte *i* of native-int source S", so nothing is ever
fabricated. The copy loop moves eight such bytes into `m_signature`; the decoder recognises
`0x21` followed by eight of them, of one source, in ascending index order, and recovers the
target exactly. A guest that scrambles or partially overwrites them gets a loud refusal rather
than a plausible wrong type.

This slots into machinery that already exists. PawPrint stores values as typed `CliType` cells
rather than as a flat byte array (`CellAwareMemOps`'s opening comment), so an array cell is a
full `CliType` and *can* carry provenance; and "a cell that exists but has no byte image" is
already a first-class concept with its own answer (`CliByteAddressability.Rejected`), today
covering object references and provenance-carrying native ints. The only new thing is a
one-byte-wide instance of it.

The pieces:

* `CliNumericType.UInt8 of uint8` widens to carry a source, exactly as `Int32Source` did for a
  narrowed byref, and for the reason that docstring gives: the compiler then visits every
  consumer of a byte, and none of them can get at a number without saying what to do when there
  isn't one.
* `Int32Source` gains the matching case. It has to: `ldind.u1` pushes an int32
  (`EvalStack.ofCliType`, EvalStack.fs:641) and `stelem.i1` narrows it back
  (EvalStack.fs:810-816, via `Int32Source.value "storing to a uint8 location"`), so the byte
  transits the evaluation stack between the load and the store.
* The byte view over the `IntPtr` local yields those bytes. The value is a field-backed struct
  whose single `_value` field is the handle, so this is the existing "bytes some field covers"
  half of the struct byte image producing a symbolic byte instead of refusing.
* `DynamicMethodBody`'s blob reader (DynamicMethodBody.fs:60-79) is the chokepoint where the
  guest array becomes a host `byte[]`; it returns a richer element type instead, and the
  signature walker consumes that.

Measured blast radius, by reading the sites rather than counting them: ~18 construction sites
that mechanically wrap in `Verbatim`, ~10 wildcard matches untouched, and about ten places that
genuinely destructure a byte to a number and so gain a refusal arm — `CliNumericType.ToBytes`,
`CellAwareMemOps.readByte`, `IntrinsicHelpers`, `NativeCall`, `NativeCustomAttribute`,
`NativeSystemNative`, `Intrinsics`, `EvalStack` both directions, `CliType.ofBytesLike`. Those
refusals are strictly better than today's, because they name the handle. The test suite carries
another ~95 mechanical `UInt8` sites.

Two mechanical assumptions checked before recommending this:

* `Buffer.BlockCopy` (which `SignatureHelper.ExpandArray` and the final trim both use) moves
  *cells*, not bytes: `CellAwareMemOps.tryWholeCellMoveAt` reads the whole cell with
  `readManagedByref` and writes it as a cell, so a symbolic byte survives a blob resize intact.
* Nothing between the write and the read inspects the blob numerically. `SignatureHelper`'s own
  `GetHashCode` is `m_module.GetHashCode() + m_currSig + m_sizeLoc`, and its `Equals` is
  reachable only for module-backed helpers.

#### Option B — Handles become byte-renderable, using the bits they already have

Permit a byte view over a handle-shaped `NativeIntSource`, and let its image be the eight
little-endian bytes of the value `PointerHashSynthesis` already hands out for that pointer —
the same bits the guest observes through `IntPtr.GetHashCode`, so PawPrint keeps one answer for
"the bits of this pointer". The decoder inverts through `PointerHashState.assigned` (stripping
the `TypeHandleTag` low bits, which are a view rather than part of the identity).

* One bit model, and no new encoding to specify.
* Assignment is *stateful* (first-touch counter) and `CliType.ToBytes`/`BytesAt` are pure, so the
  mint has to move to the byref byte-view entry points — which do take `IlMachineState`, but
  would then have to return one. A mutation reaching a layer that has none today.
* It shrinks `nonByteRenderableNativeIntSources`, a pinned property-test contract, and the
  consequence is silent: `memcpy` of a struct containing a `RuntimeTypeHandle` stops crashing at
  the copy and instead yields eight plain bytes, so the handle degrades to a number and the
  crash moves to the first *use*. Inverting in `ofBytesLike` too would fix the round trip but
  would promote any eight bytes colliding with an assigned counter into a handle.

#### Option C — An intrinsic for `SignatureHelper.InternalAddRuntimeType`

Implement the method natively: append `0x21` plus an opaque dense id per
`RuntimeTypeHandleTarget` from a table on `IlMachineState`, inverted exactly by the decoder.

* Smallest blast radius by a wide margin: nothing in the pointer or byte model changes.
* But it replaces a managed BCL method rather than implementing the primitive underneath it,
  which AGENTS.md discourages; it silently diverges if CoreLib's implementation changes shape;
  and it leaves the underlying gap open for the next guest that byte-views a handle.

## Recommendation

**Option A.** It invents nothing, it makes wrong uses fail loudly and precisely, and — measured
rather than estimated — it costs about ten real decisions in the library plus mechanical
wrapping, because the cell model and the "no byte image" concept it needs are both already
there. Option B is cheaper to write but buys that by fabricating bits, reaching into a pinned
replay contract, and turning a loud copy-time crash into a silent degradation. Option C is
cheapest of all and buys that by mocking a BCL method and leaving the gap in place.

## Why per-cell rather than per-array

The container form — an abstract array that records "cells 4..11 are the image of handle H" and
materialises only if the guest looks — was considered and rejected on two measurements.

Array *storage* is well encapsulated: `.Elements` is touched in six places, all inside
`ManagedHeap.fs`. But the array *API*'s currency is "a `CliType` at an index", and
`getArrayValue`/`setArrayValue`/`allocateArray` have 97 call sites. An abstract array still has
to answer `getArrayValue arr 5` with a `CliType`, so the per-cell symbolic byte has to exist
anyway; the container representation would sit on top of it rather than replace it. Widening
the array API to return "a cell, or a refusal" instead is those 97 sites, against about ten real
decisions for the `UInt8` widening.

Laziness is not the differentiator either. `UInt8 (HandleByte …)` never materialises a byte and
`ToBytes` refuses rather than inventing one, so the per-cell form is already exactly as deferred.

And the writes are ordinary `stelem` at computed indices, interleaved with plain bytes and
surviving a reallocation by `ExpandArray` mid-build, so a container-level extent would have to
be *inferred* from a run of eight stores and maintained across a `Buffer.BlockCopy` — the store
path pattern-matching on history. Per-cell, each store stores what it was handed and the
recognition happens once, in the decoder, which both designs need regardless. That check
("eight consecutive bytes, one source, ascending index") is what makes a guest that scrambles
the blob fail loudly rather than decode to a plausible wrong type.

## Scope of the first PR

The representation only. Concretely:

* `CliNumericType.UInt8` carries a `UInt8Source`; `Int32Source` gains the matching case so the
  byte survives the evaluation stack between `ldind.u1` and `stelem.i1`.
* `CliByteAddressability` gains a third answer alongside `ByteAddressable` and `Rejected`, so
  that *callers* state whether they accept symbolic bytes rather than the classifier deciding
  for them. The `ldind` byte-view path accepts; `CellAwareMemOps` keeps refusing exactly as it
  does today. Widening the bulk copy paths — which would make `memcpy` of a
  pointer-containing struct work exactly rather than crash — is a separate change with its own
  coverage, not a side effect of this one.
* Every site that destructures a byte to a number gains a refusal arm naming the handle.

The signature decoders keep refusing `ELEMENT_TYPE_INTERNAL`, so a guest reaches the decoder and
stops there. Measured after implementing the above, the two probes now stop at:

* the user-defined-signature `DynamicMethod`, in `ModuleHandle_GetDynamicMethod`, whose blob read
  reports the eight named bytes at blob indices 4..11 (index 3 is the `0x21` itself) and names
  what is missing;
* `MethodBase.Invoke` twice, no longer here at all — it clears this blocker and stops further on,
  in `ModuleHandle.ResolveMethod`, on a MethodDef declared on the open generic `System.Span<T>`.
  That is an unrelated reflection gap, so invoke-twice needs more than the decoder.

The hand-rolled walker and the guest cases follow.

## Later PR split


1. The write side (whichever option), pinned by unit tests over the byte view; the decoders
   still refuse `ELEMENT_TYPE_INTERNAL`, with the existing message. A guest gets further and
   fails at the decoder.
2. The read side: the hand-rolled walker over the null-module alphabet, unit-tested against
   blobs built by the real `SignatureHelper`, plus the guest cases (a `DynamicMethod` over a
   user-defined type; `MethodBase.Invoke` twice) as `sourcesImpure` registrations with the
   dynamic-code switch overridden.
