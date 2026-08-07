# Pointers And Byte Representations

*Authorship: LLM*

This document describes the model PawPrint currently uses for managed pointers, native-int-like pointer values, and byte views over managed storage.

## Object References And Byrefs

PawPrint keeps CLI object references and CLI managed pointers separate.

An object reference is a value whose identity is a heap object address. It is represented as `CliType.ObjectRef` and, on the evaluation stack, as `EvalStackValue.ObjectRef` or `EvalStackValue.NullObjectRef`. It is not a location that can be dereferenced with `ldind.*`.

A managed pointer, or byref, points at a storage location. It is represented by `ManagedPointerSource`:

- `ManagedPointerSource.Null` is the null byref.
- `ManagedPointerSource.Byref (root, projections)` identifies a root storage location and then applies zero or more navigation steps.

The root is a `ByrefRoot`: a local slot, argument slot, localloc byte, boxed value payload, object field, array element, PE byte range, static field, or string character. The projections are `ByrefProjection` values:

- `Field` navigates inside the current value.
- `ReinterpretAs` changes the type view while preserving the address.
- `ByteOffset` advances a byte cursor under a trailing reinterpret.

Do not introduce a bare "heap object byref" to stand in for an object reference. If code has an object reference, keep it as an object reference. If code has a byref into a heap object, say exactly which storage location it points at, such as `HeapObjectField`, `HeapValue`, or `ArrayElement`.

## Byte Cursors

A byte cursor is the trailing `ByteOffset` projection under a trailing `ReinterpretAs` projection. It means: "start at this typed storage cell, view it as bytes, and move this many bytes from the start of that cell."

For example, `Byref (ArrayElement (arr, 0), [ ReinterpretAs byte; ByteOffset 6 ])` is a byte view six bytes after `arr[0]`. If each array element is four bytes wide, normalisation rewrites this to `Byref (ArrayElement (arr, 1), [ ReinterpretAs byte; ByteOffset 2 ])`.

The invariant is that `ByteOffset` is only valid as the final projection, and only when immediately preceded by `ReinterpretAs`. Code that sees a `ByteOffset` elsewhere should treat that as an interpreter bug.

Normalisation is deliberately contextual:

- Array roots need the array element size, which requires heap/type state.
- String character roots have a fixed two-byte UTF-16 stride.
- Localloc byte roots have a fixed one-byte stride.

Use `ManagedPointerByteView.normalisationContextForPointer` or `normalisationContextForPointers` when heap state is available. APIs that compare byrefs structurally should require `NormalisedManagedPointerSource`, not raw `ManagedPointerSource`, so equivalent byte locations use one canonical representation.

## Reading And Writing Bytes

Typed byref reads and writes go through root/projection evaluation. Byte-view reads and writes go through `IlMachineState.readManagedByrefBytesAs` and `IlMachineState.writeManagedByrefBytes`.

The byte representation is not a process address. It is a deterministic reconstruction of storage bytes:

- Primitive `CliType` values use `CliType.ToBytes` and `CliType.ofBytesLike`.
- Byte-addressable value types use their materialised byte image. Field-backed value types preserve padding and trailing storage bytes explicitly; named field values are overlaid onto that preserved image, and byte-slice writes rebuild the value from the updated image.
- Arrays are treated as contiguous element cells. Byte reads and writes may span cells, and each touched element must be byte-addressable.
- Strings are treated as contiguous UTF-16 character cells. Byte writes reconstruct chars from two bytes.
- Localloc storage is already raw bytes.
- PE byte ranges expose read-only bytes from the loaded PE image.
- Boxed value-type byte views are allowed when the boxed payload is byte-addressable: value types containing object references or runtime pointers are rejected.
- Object references and runtime pointers are not ordinary byte payloads; reinterpret writes over them fail loudly.

Unrepresented storage and reference-containing values are intentionally not guessed. If a storage shape has bytes that PawPrint does not represent, the byte-view path should fail rather than silently inventing or dropping data. Padding bytes are usable only when they are part of the value type's preserved byte image.

## Pointer Arithmetic And Synthetic Distances

PawPrint does not model real machine addresses for managed objects, stack frames, or arrays. Pointer arithmetic therefore stays inside the byref model wherever possible.

For same-storage byte offsets, code computes deterministic byte positions from the storage identity and the in-storage byte offset. `ByteStorageIdentity` names the container: array, string, PE byte range, static field, localloc block, stack local, or stack argument.

For distinct storage containers there is no honest byte distance. `NativeIntSource.syntheticCrossStorageByteOffset` returns a deterministic tagged sentinel instead. It preserves anti-symmetry for `Unsafe.ByteOffset(a, b) = -Unsafe.ByteOffset(b, a)` and is large enough to avoid false overlap in `Memmove`-style unsigned checks, but downstream arithmetic refuses to compose it as if it were a real address.

Low address bits are only exposed for narrow cases where managed code masks alignment bits and the model can state a stable answer. Localloc and PE byte ranges can provide synthetic low bits directly; array elements and string characters can provide offsets from their container starts when their stride is known. Arbitrary byrefs and object fields cannot.

## Tag Bits On Opaque Handles

Managed code sometimes stores its own bits in a pointer's low, known-clear bits and strips them off again later. `System.WeakReference` keeps `handle | TracksResurrectionBit` in an `nint` field and masks the tag off on every read; `GCHandle` marks pinned handles with bit 0.

PawPrint answers those operations without inventing an address, by carrying the tag alongside the handle's identity: `NativeIntSource.GcHandlePtr` is a `GcHandleAddress` *plus* a tag. The value is modelled as `base ||| tag`, where `base` is unknown and non-zero but has its low bits clear by the runtime's own alignment guarantee, and `tag` is known.

`TaggedPointerBits` decides what such a model can say. A bitwise operation is answerable in exactly two shapes — the whole unknown base survives (so the result is the same handle with a new tag), or the whole unknown base is forced to a constant (so the result is a plain integer and no longer a pointer) — and is refused otherwise. Identity stays in `GcHandleAddress`; the tag is a view over it, so `PointerHashSynthesis` keys on the address alone and folds the tag into the low bits it already leaves free.

Consumers that dereference or free a handle require the tag to be zero. Real managed code always masks first, and a tagged dereference is a misaligned read.

The same model covers CoreCLR's `TypeHandle`, which wraps either a `MethodTable*` or a `TypeDesc*` and sets bit 1 in the latter case. `TypeHandleTag` is the single home for that rule, shared by the `and` arms in `NullaryIlOp` and the synthesised low bits in `PointerHashSynthesis`; the two must agree, because a tag is observable both by masking it directly and by comparing synthesised bits.

One difference matters. A GC handle's tag is *independent state* that managed code sets and clears at will, so any tag in the region is representable. A type handle's tag is a *function of its target* — `IsTypeDesc` is determined by what the handle points at — so a handle whose tag differs from its target's is not the same handle retagged, but a different kind of pointer. `TypeHandle.AsTypeDesc` (`handle & ~2`) is exactly that case: its result is the target's `TypeDesc*`. PawPrint refuses it rather than answering, because silently returning the masked tag bits would replace a pointer with a small integer, usually null.

## Extension Rules

When extending this area, keep the model honest:

- Represent identity as structured storage plus offset, not as fabricated host addresses. Where managed code imposes a *view* on an identity (tag bits on a handle), carry the view beside the identity rather than collapsing both into a bit pattern.
- Keep object references out of `ManagedPointerSource`.
- Add roots or projections when the storage relationship is real; do not coerce one identity into another just to reuse a lookup.
- Normalise byte cursors before structural equality or `ceq`-style byref comparison.
- Fail loudly for byte views over unrepresented storage, object references, runtime pointers, or unsupported field-layout cases. Preserved padding is represented storage; do not discard it.
- Extend the primitive boundary where the runtime primitive lives. Do not mock a higher-level managed method just because it happens to reach byte or pointer operations.
