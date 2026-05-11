# Plan: extend `RawData::Data` projection to arrays

Date: 2026-05-11
Author: Claude
Status: Proposed
Branch base: `rawdata-array-projection` (off `castcache-sentinel-init`)

## Context

The previous PR (`castcache-sentinel-init`) lazily installs an `int[18]`
sentinel for `CastHelpers::s_table` so that BCL code can call
`CastCache.TryGet` without dereferencing null. With that fix in place,
`NullDereferenceTest.cs` now progresses past the `FailFast` cascade and
trips the next blocker:

```
System.Exception : RawData::Data projection expected non-array heap object
                   at <object #134>, got array <unregistered concrete type 1[]>
   at WoofWare.PawPrint.RuntimeFieldProjection.requireNonArrayHeapObject
   at WoofWare.PawPrint.RuntimeFieldProjection.tryProjectRawDataFieldAddress
   at WoofWare.PawPrint.RuntimeFieldProjection.tryProjectFieldAddress
   at WoofWare.PawPrint.UnaryMetadataFieldOps.executeLdflda
```

The failing IL is `ldflda RawData::Data` against the sentinel array. The
BCL invokes it via:

```csharp
// CastCache.TableData
private static ref int TableData(int[] table)
{
    return ref Unsafe.As<byte, int>(
        ref Unsafe.AddByteOffset(ref table.GetRawData(), (nint)sizeof(nint)));
}

// RuntimeHelpers.GetRawData
internal static ref byte GetRawData(this object obj) =>
    ref Unsafe.As<RawData>(obj).Data;
```

`obj.GetRawData()` is just `Unsafe.As<RawData>(obj).Data`. CoreCLR's
documented layout (see `RuntimeHelpers.CoreCLR.cs:622-638`) is:

```
[ sync block || pMethodTable || num components || MD bounds || array data .. ]
                ^               ^                              ^
                |               |                              \-- &RawArrayData.Data
                \-- array       \-- &RawData.Data
```

So `RawData::Data` on an array points at the length field; adding
`sizeof(nint)` bytes skips length+padding and lands at element 0.

PawPrint currently rejects this whole path: `requireNonArrayHeapObject`
fails for arrays. To unblock `NullDereferenceTest.cs` (and 16 other tests
that travel through the cast cache during NRE message construction), we
need the projection to accept arrays and yield a byref that, after the
canonical `+sizeof(nint)` arithmetic, normalises to `&array[0]`.

## Approach

### Option matrix

| Option | Pros | Cons |
| --- | --- | --- |
| (a) Reuse `ByrefRoot.ArrayElement(arr, 0)` with `[ReinterpretAs byte; ByteOffset -sizeof(nint)]` | Tiny patch: only `RuntimeFieldProjection.fs` and the projection site. Leans on existing `ByteOffset n :: ByteOffset m -> ByteOffset (m+n)` collapse to wash out the negative offset when `+sizeof(nint)` is applied. | Intermediate byref is in a "before-element-0" state. If anything *reads* it without first adding sizeof(nint) (e.g., to access the length bytes), the existing array-byte normaliser folds to `ArrayElement(arr, -2)` and the read fails with a confusing index-out-of-bounds error. |
| (b) New `ByrefRoot.ArrayRawData(arr)` | Honest representation; reads in the length-header region produce a clear, targeted error message. | Requires adding match arms across `IlMachineManagedByref.fs`, `BinaryArithmetic.fs`, `Intrinsics.fs`, `IntrinsicHelpers.fs`, `NullaryIlOp.fs`, `ManagedPointerSource.fs`, `ManagedPointerByteView.fs`, and the diagnostic `ToString`. Each is mostly a `failwith` until a real consumer appears. |
| (c) Generalise `ByrefRoot.HeapValue` to accept arrays | Smallest DU surface. | Conflates two distinct memory-layout views: on non-array objects, `HeapValue` represents the start of instance data (no length prefix); on arrays, it would represent the start of the length-header (i.e. 8 bytes *before* element data on 64-bit). This blurs the contract and would silently mis-route consumers that previously assumed HeapValue points at the "first usable byte." |

### Recommendation: option (a)

Option (a) is the smallest faithful change that solves the problem.
Walk through the byref evolution for the CastCache pattern:

1. `ldflda RawData::Data` on array `arr` →
   `(ArrayElement(arr, 0), [ReinterpretAs byte; ByteOffset (-8)])`.
2. `Unsafe.AddByteOffset(rawData, 8)` calls `addByteOffsetToByteView 8`,
   which `appendProjection (ByteOffset 8)`. The existing collapse rule
   in `ManagedPointerSource.fs:399-403` matches `n = -m` and *removes*
   both the existing `-8` and the new `+8` offsets, leaving
   `(ArrayElement(arr, 0), [ReinterpretAs byte])`.
3. `Unsafe.As<byte, int>(...)` appends `ReinterpretAs int32`, which (per
   the existing rule at `ManagedPointerSource.fs:397-398`) replaces the
   trailing `ReinterpretAs byte`. Final byref:
   `(ArrayElement(arr, 0), [ReinterpretAs int32])` ≡ `ref array[0]` as
   `ref int`.
4. `HashShift(tableData) = ref tableData` reads `array[0]`. With the
   sentinel-layout fix below, this is `63` (the canonical sentinel
   hashShift on 64-bit).

The intermediate byref is only ever held on the eval stack between
`ldflda` and `Unsafe.AddByteOffset`. If a future BCL change reads the
length-header bytes via this byref, it will land at
`ArrayElement(arr, -1)` (or `-2`) and fail with PawPrint's standard
out-of-bounds error. To make that path discoverable when it eventually
fires, we will tighten the error message at `getArrayValue` /
`setArrayValue` to mention the likely cause when the index is negative.

This avoids touching the ~30-arm DU pattern-match graph that option (b)
would require.

### Latent sentinel-layout bug

While verifying the CastCache invariants, the
`internCastCacheSentinelTable` helper added in
`castcache-sentinel-init` was found to write the auxiliary header at
the **wrong array indices**.

`TableData(table)` returns `ref array[0]`, so per
`CastCache.cs:113-130`:

| Field | Definition | PawPrint index |
| --- | --- | --- |
| `HashShift(tableData)` | `ref tableData` | `array[0]` |
| `TableMask(tableData)` | `ref Unsafe.Add(ref tableData, 1)` | `array[1]` |
| `VictimCounter(tableData)` | `ref Unsafe.Add(ref tableData, 2)` | `array[2]` |

The current code writes them at `array[2]`, `array[3]`, `array[4]`. The
bug is latent — until this PR lands, the `RawData::Data` projection
fails before any consumer reads from those slots — but it would have
broken correctness as soon as the projection started working.

### Why this matters for `TryGet`

The sentinel needs `hashShift = 63` so
`KeyToBucket = (int)((hash * Magic) >> 63) ∈ {0, 1}` keeps the initial
bucket index within the int[18] table; otherwise `Element(tableData, k)`
for arbitrary `k` reads `array[(k+1)*6]` and out-of-bounds.
`tableMask = 1` keeps the reprobe `(index + i) & tableMask` in
`{0, 1}`. `victimCounter = 0` is just the default.

Once `version == 0` is read at the first probed element (every slot in
the sentinel is zero-initialised), the loop in `TryGet` breaks at the
`version == 0` check (`CastCache.cs:195-199`) and returns `MaybeCast` —
the intended sentinel behaviour.

## Concrete changes

### 1. `WoofWare.PawPrint/RuntimeFieldProjection.fs`

- `requireNonArrayHeapObject` becomes `requireHeapObject` (or is replaced
  in place): just verifies a heap object exists, no array rejection.
- `tryProjectRawDataFieldAddress` for `Data` projects to:
  - non-array: same as today.
  - array: `(ArrayElement(arr, 0), [ReinterpretAs byte; ByteOffset (-nativeIntSize)])`.
- Update the docstring above `tryProjectRawDataFieldAddress` to explain
  the new array case: byref is intentionally constructed in a
  "before-element-0" state; the canonical CastCache-style
  `+sizeof(nint)` arithmetic collapses the negative offset away. Reads
  at the raw byref position will surface as an out-of-bounds array
  index.

`nativeIntSize` should mirror the helper in
`Native/NativeRuntimeType.fs:32`:

```fsharp
let private nativeIntSize =
    CliType.sizeOf (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
```

Consider promoting the helper to a shared module if it ends up needed
in three or more places; for now, local duplication is fine.

### 2. `WoofWare.PawPrint/IlMachineRuntimeMetadata.fs`

`internCastCacheSentinelTable`: change the `setArrayValue` indices from
`2, 3, 4` to `0, 1, 2`.

```fsharp
state
|> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 63)) 0
|> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 1)) 1
|> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 0)) 2
```

Update the docstring "auxiliary header sits at element indices 2, 3, 4"
to "0, 1, 2", with a one-line derivation: `TableData(table)` returns
`ref array[0]` after the `+sizeof(nint)` skip, so the
`HashShift`/`TableMask`/`VictimCounter` accessors index from `array[0]`.

### 3. `WoofWare.PawPrint/IlMachineThreadState.fs` (`getArrayValue`/`setArrayValue`)

Tighten the out-of-bounds error message to flag the likely cause when
the index is negative:

> "Array index {i} is negative on array at {addr}. This typically
> indicates that a byref obtained via `RawData::Data` on an array was
> read without first applying the canonical `+sizeof(nint)` skip past
> the length-header region; consider `RawArrayData::Length` if you
> intended to read the length."

This change is defensive only — no current path produces a negative
array index — but it makes the failure mode of option (a) discoverable.

### 4. `WoofWare.PawPrint.Test/sourcesPure/RawDataArrayProjection.cs` (new)

Focused regression test: construct an `int[]`, take a byref via
`Unsafe.As<RawData>` + `+sizeof(nint)`, write/read element 0 through it,
assert the value round-trips. Mirrors the test added in
`castcache-sentinel-init` but for the projection itself rather than the
sentinel allocation. (The CastCache flow exercises the same path
end-to-end; the focused test is in case the BCL changes its
implementation.)

### 5. `WoofWare.PawPrint.Test/TestPureCases.fs`

Remove `"NullDereferenceTest.cs"` from `unimplemented` — it should now
pass. If other tests in the cluster (`CastClassInvalid.cs`,
`CastclassFailures.cs`, `ComplexTryCatch.cs`, `ArraySortHelperDefaultInt.cs`,
`GenericEdgeCases.cs`, `ThrowingCctorProperties.cs`) also pass with the
combined fix, promote them too; otherwise refresh their comments to
point at whatever the new blocker is.

### 6. `docs/runtime-initialised-statics.md`

No update needed — the Category B row already points at the
implementing helper for `s_table`. The CastCache layout fix is internal
correctness and doesn't change the runtime-initialised-statics surface.

## Risks and mitigations

- **Other consumers of `RawData::Data` on arrays.** Grep CoreLib for
  `Unsafe.As<RawData>` and `GetRawData()` to enumerate. Most hits are
  `RuntimeHelpers.GetRawData` (the helper itself) and CastCache. If a
  caller reads at offset 0 from this byref (length field), it will land
  at `ArrayElement(arr, -2)` and fail. We accept this as a
  defer-until-needed shape; the error message tightening in step 3
  surfaces the cause.
- **Negative-index normalisation collapsing prematurely.** The collapse
  rule at `ManagedPointerSource.fs:399-403` requires the negative
  `ByteOffset` to be the *trailing* projection. We construct the byref
  with `[ReinterpretAs byte; ByteOffset -nativeIntSize]` so that's
  satisfied. A subsequent `Unsafe.As<TFrom,TTo>` would replace the
  trailing `ReinterpretAs` and the existing rule at
  `ManagedPointerSource.fs:390-396` *preserves* the trailing
  `ByteOffset`. That's fine: a later `+nativeIntSize` still collapses
  the offset.
- **32-bit guests.** PawPrint targets 64-bit exclusively, so
  `nativeIntSize = 8`. Use the existing helper rather than hard-coded
  literals to keep the contract explicit.
- **Sentinel-layout fix is a behaviour change.** It's bundled with the
  projection fix because the projection fix is what makes the layout
  observable; shipping them together avoids leaving the tree in a state
  where the projection works but reads garbage.

## Validation

1. Build + fantomas clean.
2. Run focused test (step 4) — should pass.
3. Run `NullDereferenceTest.cs` (step 5) — should pass.
4. Run full suite: 687 (previous) + N (newly passing) tests should
   pass with no regressions.
5. Commit on branch `rawdata-array-projection` and request review per
   `CLAUDE.md`.
