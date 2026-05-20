# Plan: extend `RawData::Data` projection to arrays

Date: 2026-05-11
Author: Claude
Status: Landed (with deviations — see "Landed state" below)
Branch: `rawdata-array-projection` (merged from `castcache-sentinel-init`,
         currently rebased on `main`)

## Landed state

What actually shipped on this branch (vs. what this plan originally proposed):

- ✅ `RawData::Data` projection now accepts SZ arrays and emits
  `(ArrayElement(arr, 0), [ReinterpretAs byte; ByteOffset -nativeIntSize])`
  (`WoofWare.PawPrint/RuntimeFieldProjection.fs`).
- ✅ CastCache sentinel auxiliary header moved from indices `2, 3, 4` to
  `0, 1, 2` (`WoofWare.PawPrint/IlMachineRuntimeMetadata.fs`,
  `internCastCacheSentinelTable`). The sentinel-layout fix landed as a
  separate commit (`27fdaff`) after the projection change (`9416fdc`).
- ⚠️ **Deviation:** the projection is restricted to
  `ConcreteTypeHandle.OneDimArrayZero`. MD arrays
  (`ConcreteTypeHandle.Array (_, rank)`) hit a `TODO`-flavoured `failwith`
  with the rank in the message. `MethodTableProjection.baseSize` already
  models the `(3 + rank) * NATIVE_INT_SIZE` MD header, so the SZ-only
  arithmetic in this projection would have landed `2 * rank * sizeof(int32)`
  bytes inside the bounds region for an MD array — silently wrong. Failing
  loudly is the honest representation until a real MD-array caller needs it.
- ⚠️ **Deviation:** `getArrayValue` / `setArrayValue` error messages were
  **not** tightened for negative indices. The negative-index path isn't
  currently reachable through any test (the SZ-array byref always pairs
  with a `+sizeof(nint)` skip before it's dereferenced), so the
  diagnostic-tightening step (originally step 3) was deferred.
- ⚠️ **Deviation:** no `RawDataArrayProjection.cs` test was added under
  `sourcesPure/`. The CastCache end-to-end flow exercises the SZ-array
  path; an MD-array rejection unit test was added in
  `TestMethodTableProjection.fs` instead, covering the SZ-vs-MD split that
  the C# regression test would not have caught anyway.
- ⚠️ **Deviation:** `NullDereferenceTest.cs` was **not** promoted out of
  `unimplemented`. Past this PR's two fixes, it hits a new blocker —
  unimplemented JIT intrinsic
  `System.Runtime.CompilerServices.Unsafe.AddByteOffset(&, System.IntPtr)`
  inside `CastCache.TableData`. The `unimplemented` comment in
  `WoofWare.PawPrint.Test/TestPureCases.fs:26` was updated to reflect that
  new blocker; further sibling tests (`CastClassInvalid.cs`,
  `CastclassFailures.cs`, etc.) remain on their pre-existing blockers and
  were not touched.

The rest of this document is the original design rationale, preserved
because the option-matrix reasoning is what's worth keeping — not the
forward-looking task list.

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

### Latent sentinel-layout bug (fixed in this PR)

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

The pre-fix code wrote them at `array[2]`, `array[3]`, `array[4]`. The
bug was latent — until the projection in this PR landed, `RawData::Data`
failed before any consumer reads from those slots — but it would have
broken correctness as soon as the projection started working. The fix
landed alongside the projection change.

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
  - SZ array (`ConcreteTypeHandle.OneDimArrayZero`):
    `(ArrayElement(arr, 0), [ReinterpretAs byte; ByteOffset (-nativeIntSize)])`.
  - MD array (`ConcreteTypeHandle.Array (_, rank)`): fails with a
    `TODO`-flavoured `failwith` naming the rank and the `2 * rank` int32
    bounds entries that CoreCLR places between the length header and
    element data. The SZ-only arithmetic would land in the middle of
    that bounds region, so silent acceptance was rejected in favour of
    explicit failure until a real MD-array caller appears.
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

**Deferred — not landed.** Originally planned to tighten the out-of-bounds
error message to flag the likely cause when the index is negative:

> "Array index {i} is negative on array at {addr}. This typically
> indicates that a byref obtained via `RawData::Data` on an array was
> read without first applying the canonical `+sizeof(nint)` skip past
> the length-header region; consider `RawArrayData::Length` if you
> intended to read the length."

No current path produces a negative array index, and the diagnostic is
defensive only. Skipped to keep the PR focused; revisit if a real caller
ever lands on the failure mode.

### 4. `WoofWare.PawPrint.Test/sourcesPure/RawDataArrayProjection.cs` (new)

**Not landed.** The CastCache end-to-end flow already exercises the
SZ-array projection through the existing `unimplemented`-track tests,
and a focused unit test for the MD-array rejection path was added in
`TestMethodTableProjection.fs` (`RawData data projection rejects
multi-dimensional arrays`). A C# regression test could be added later
if the BCL changes its `CastCache.TableData` implementation in a way
that bypasses `Unsafe.As<RawData>`, but it isn't required today.

### 5. `WoofWare.PawPrint.Test/TestPureCases.fs`

**Did not promote `NullDereferenceTest.cs`.** With both fixes in place,
it advances past the `RawData::Data` blocker but immediately hits a new
one: the unimplemented JIT intrinsic
`System.Runtime.CompilerServices.Unsafe.AddByteOffset(&, System.IntPtr)`
inside `CastCache.TableData`. The `unimplemented` comment at
`TestPureCases.fs:26` was updated to point at that new blocker. The
sibling tests listed in the original plan
(`CastClassInvalid.cs`, `CastclassFailures.cs`, `ComplexTryCatch.cs`,
`ArraySortHelperDefaultInt.cs`, `GenericEdgeCases.cs`,
`ThrowingCctorProperties.cs`) all sit on pre-existing blockers further
along the BCL call graph and were not affected by this change; their
comments remain as-is.

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

## Validation (as landed)

1. Build + fantomas clean.
2. `TestMethodTableProjection`: existing SZ-array projection test still
   passes; new MD-array rejection test (`RawData data projection rejects
   multi-dimensional arrays`) asserts the `TODO`-flavoured failure.
3. `NullDereferenceTest.cs` advances past the projection blocker but
   stops at the next blocker (`Unsafe.AddByteOffset` JIT intrinsic);
   it remains in `unimplemented` with an updated comment.
4. Full `nix develop -c dotnet test` suite still passes with the new
   commits — no regressions.
5. Branch committed and reviewed per `CLAUDE.md`.
