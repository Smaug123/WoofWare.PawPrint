# Plan: Initialise `CastHelpers.s_table` so the BCL cast cache no longer NREs

## Context

A cluster of ~17 currently-`unimplemented` tests collapse onto a single blocker: the BCL's `SR.InternalGetResourceString` recursion guard escalates to `Environment.FailFast` with

> Encountered infinite recursion while looking up resource 'Arg_NullReferenceException' in System.Private.CoreLib.

The first NRE is legitimate (e.g. a guest `p._field` on `p = null`, the `NullDereferenceTest.cs` repro). Constructing the message string for that NRE calls `SR.GetResourceString` → `ResourceManager..ctor` → `ManifestBasedResourceGroveler.GetNeutralResourcesLanguage` → custom-attribute filtering → `RuntimeType.IsAssignableFrom` → `RuntimeTypeHandle.CanCastTo` → `TypeHandle.TryCanCastTo` → `CastCache.TryGet` → `CastCache.TableData(table)` → `RuntimeHelpers.GetRawData(table)` → `ldflda RawData::Data` on a null `obj` → second NRE → recursion guard trips.

The null is the static field `System.Runtime.CompilerServices.CastHelpers::s_table` (an `int[]?`). The managed code at `TypeHandle.TryCanCastTo+0x24` is `ldsfld CastHelpers::s_table`; PawPrint reads it as `None`/null because no managed code ever wrote to it.

In CoreCLR, `s_table` is populated by native code at EE startup: `appdomain.cpp:1072 → CastCache::Initialize()` (`castcache.cpp:112-138`) creates a 2-entry sentinel cache via `CreateCastCache(2)` and writes it into the field via `CoreLibBinder::GetField(FIELD__CASTCACHE__TABLE)->GetCurrentStaticAddress(); SetObjectReference(...)`. PawPrint has no equivalent EE startup hook.

This is the same *symptom class* as the recently-fixed `System.String::Empty`, but a different *mechanism*. There are three JIT-intrinsic fields whose `ldsfld` the real JIT replaces with a constant (`String.Empty`, `IntPtr.Zero`/`UIntPtr.Zero`, `BitConverter.IsLittleEndian`; see `getFieldIntrinsic` at `coreclr/vm/jitinterface.cpp:1145-1166`) — and `CastHelpers.s_table` is *not* one of them. It is a regular static slot that the EE writes during `SystemDomain::LoadBaseSystemClasses`. Searching `coreclr/vm` for the same pattern (`SetObjectReference((OBJECTREF*)…)` against a `FieldDesc`'s static address) finds exactly one such field, so this fix is expected to be a one-shot: there is no second native-init static lurking in CoreCLR.

(Notes on the JIT intrinsics: `IntPtr.Zero` / `UIntPtr.Zero` happen to coincide with `cliTypeZeroOf` of `nint`/`nuint` and need no fix; `BitConverter.IsLittleEndian` has a managed initialiser `= true` under `!BIGENDIAN`, so the ordinary `.cctor` does the right thing. `String.Empty` was the genuine omission and is already fixed in HEAD.)

## Approach

Mirror the `System.String::Empty` lazy-init pattern (`6b8bf34`). On `ldsfld`/`ldsflda` of `CastHelpers::s_table` whose backing store is `None`, allocate a managed `int[]` shaped as a 2-entry sentinel cast cache, write the aux header, install it in the static slot, and continue. Subsequent reads see the same array; subsequent BCL `CastCache.TryGet` calls go through the cache, observe `version == 0` on the first probe, return `CastResult.MaybeCast`, and the BCL falls through to the slow path — which on CoreCLR is the same QCall (`RuntimeTypeHandle::CanCastTo`) PawPrint already handles via its existing type-system implementation.

The sentinel never has anything written into it (`CastCache.TrySet` on the sentinel takes the `TableMask == 1` early-return at `CastCache.cs:267-274`), so we don't need to model `TrySet` updates — every cache lookup remains a miss-then-fallthrough. That is the *intended* behaviour of the native sentinel too.

### Sentinel `int[]` shape (size = 2)

Following managed `CastCache.CreateCastCache(2)` and `TableData` (`CastCache.cs:104-130`):

- `CastCacheEntry` has sequential layout `{ uint _version; nuint _source; nuint _targetAndResult; }`. On 64-bit, `sizeof(CastCacheEntry) == 24` bytes (uint padded to 8 for the nuint alignment).
- Array length in `int32` units = `(size + 1) * sizeof(CastCacheEntry) / sizeof(int32)` = `3 * 24 / 4` = **18 ints**.
- `TableData(table) = GetRawData(table) + sizeof(nint)`. `RuntimeHelpers.GetRawData` on an array returns a `ref byte` at the length field (`RawArrayData.Length`), so on 64-bit the 8-byte skip walks past `Length` (4 bytes) and the trailing padding (4 bytes) and the resulting pointer is `&table[0]`.
  - `table[0]` = `hashShift`
  - `table[1]` = `tableMask`
  - `table[2]` = `victimCounter` (treated as `uint`)
  - `table[3..5]` = the rest of the aux-slot CastCacheEntry, must be zero
  - `table[6..11]` = entry 0 (zero → `_version == 0`, immediate break in `TryGet`)
  - `table[12..17]` = entry 1 (zero)
- `tableMask = size - 1 = 1`.
- `hashShift = BitOperations.LeadingZeroCount((nuint)1)` — on a 64-bit guest, this is `63`. PawPrint targets 64-bit only, so we hard-code `63` and add a debug assert that `nuint.Size == 8` in the layout helper.

After installation, `KeyToBucket` shifts the 64-bit hash right by 63 and masks with `1`, returning an index in `{0, 1}`. `Element(ref tableData, 0)` and `Element(ref tableData, 1)` both see zero entries, hit the `version == 0` break, and return `CastResult.MaybeCast`.

### Where to do the detection

The `ldsfld`/`ldsflda` paths in `UnaryMetadataFieldOps.fs` already have a precedent block for `System.String::Empty` (the `None when isSystemStringEmptyField …` arm). Add a sibling arm for `s_table`:

```fsharp
let private isCastHelpersTableField
    (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
    : bool
    =
    field.Name = "s_table"
    && field.DeclaringType.Generics.IsEmpty
    && field.DeclaringType.Namespace = "System.Runtime.CompilerServices"
    && field.DeclaringType.Name = "CastHelpers"
    && field.DeclaringType.Assembly = baseClassTypes.Corelib.Identity  // or whatever the field exposes
```

`BaseClassTypes` does not carry `CastHelpers` (it's tightly scoped to types the runtime fundamentally needs). Match by namespace/name/assembly directly, the same way other corelib-only detections do — `CastHelpers` is sealed/internal and there is no risk of a guest type colliding under the same FQN.

### The lazy allocator

Add a sibling to `IlMachineRuntimeMetadata.internCanonicalEmptyString`:

```fsharp
/// Allocate a sentinel 2-entry CastCache backing array shaped to match the
/// native EE's CastCache::Initialize, and install it in CastHelpers::s_table.
/// The array is intentionally never updated: TryGet sees zero entries, returns
/// MaybeCast, and the BCL falls through to the slow path. This satisfies all
/// managed callers of CastCache.TryGet on this runtime, which has no JIT to
/// populate the cache for us anyway.
val internCastCacheSentinelTable :
    ILoggerFactory ->
    BaseClassTypes<DumpedAssembly> ->
    IlMachineState ->
    ManagedHeapAddress * IlMachineState
```

Implementation: build a `ConcreteTypeHandle.Array (int32Handle, 1)` via the existing concretisation API, then call `IlMachineState.allocateArray` with `len = 18`, zero-init, and post-allocate-write the three aux ints at positions 0, 1, 2. Use `ManagedHeap.setArrayElement` (or the existing array-mutation seam — see what `Array.Copy` already uses) to set those three entries. Cache the result on `IlMachineState` similarly to `InternedStrings` if/when we need a second caller, but for now a one-shot allocation tied to the field's storage slot is fine.

### Wiring into `ldsfld` / `ldsflda`

In the two `None when …` arms in `UnaryMetadataFieldOps.fs` (existing `executeLdsfld` and `executeLdsflda`), add the `s_table` case alongside `String::Empty`. Both return `CliType.ObjectRef (Some addr)` for the value and update the static via `IlMachineState.setStatic`, identical to the `String::Empty` precedent.

## Files to modify

- `WoofWare.PawPrint/IlMachineRuntimeMetadata.fs` — add `internCastCacheSentinelTable` next to `internCanonicalEmptyString` (~line 535).
- `WoofWare.PawPrint/IlMachineState.fs` — re-export the new helper (~line 80-95).
- `WoofWare.PawPrint/UnaryMetadataFieldOps.fs` — add `isCastHelpersTableField` predicate and the `None when isCastHelpersTableField …` arms in `executeLdsfld` and `executeLdsflda`.
- `WoofWare.PawPrint.Test/sourcesPure/CastHelpersTableInit.cs` — focused test (see below).
- `WoofWare.PawPrint.Test/TestPureCases.fs` — drop `NullDereferenceTest.cs` (and likely several siblings) from `unimplemented`, refresh comments on any that now hit a different blocker.

## Tests

### Focused new test: `CastHelpersTableInit.cs`

The point is to verify *only* that the cast cache field is non-null and shaped correctly enough to satisfy `CastCache.TryGet` without throwing. End-to-end behaviour (a real NRE catch surviving the resource lookup) is verified by promoting `NullDereferenceTest.cs` out of `unimplemented`.

```csharp
using System;
using System.Reflection;
using System.Runtime.CompilerServices;

class Program
{
    static int Main(string[] args)
    {
        // Reflectively read CastHelpers.s_table (internal).
        Type castHelpers = typeof(object).Assembly.GetType("System.Runtime.CompilerServices.CastHelpers", throwOnError: true)!;
        FieldInfo sTable = castHelpers.GetField("s_table", BindingFlags.NonPublic | BindingFlags.Static)!;

        // The first read after the runtime has initialised the field must be non-null
        // and an int[] (the sentinel cast cache).
        object? value = sTable.GetValue(null);
        if (value is not int[] arr)
        {
            return 1;
        }

        // Shape sanity: 18 ints for a size-2 sentinel.
        if (arr.Length != 18)
        {
            return 2;
        }

        // The aux header (tableMask) sits at element 1 and must be size - 1 = 1
        // for a 2-element sentinel; if tableMask were 0 or >=2 we'd see different
        // index arithmetic and a TrySet that doesn't take the sentinel early-return.
        if (arr[1] != 1)
        {
            return 3;
        }

        return 0;
    }
}
```

Pattern matches existing reflection-driven sanity tests in `sourcesPure/` and is auto-discovered.

### Promote `NullDereferenceTest.cs` out of `unimplemented`

The test asserts the guest exit code matches the real CLR's (0 on success). With `s_table` initialised, the inner NRE during message-string construction stops happening, the legitimate NRE is caught by the guest's `catch (NullReferenceException)`, and the test passes.

If it does *not* pass, expect a *different* blocker further along the resource-lookup chain — most likely an unimplemented QCall surfaced by `ResourceManager.CommonAssemblyInit` once it can get past `IsAssignableFrom`. Keep the test in `unimplemented` and refresh the comment to point at the new blocker, then split that out as a separate plan.

### Sibling tests likely to unblock

The `unimplemented` list currently has 17 entries whose comments end in *"blocked after Unsafe.IsNullRef by unimplemented QCall!AssemblyNative_GetResource"* or *"blocked downstream by ResourceManager hitting infinite recursion looking up 'Arg_NullReferenceException'"*. Don't preemptively un-mark all of them — change `NullDereferenceTest.cs` only, run the test suite, and *then* sweep the comments based on what actually now passes / what newly-surfaces. The cast-cache fix is one PR; the comment refresh is a follow-up.

## Edge cases & non-issues

- **`ldsflda` of `s_table`.** The BCL never takes the address of `s_table` (it's `int[]?`, a reference, and ECMA semantics for `ldsflda` of a `ref` field would be unusual). Adding the arm symmetrically with `String::Empty` is defensive but should not actually fire — guard with an assertion if convenient, otherwise just install identically.
- **Multiple writes to `s_table`.** The BCL's `CastCache.TrySet` only writes via the *instance* version, which is never invoked from managed code on CoreCLR (`CastCache.cs:208-209`: "in CoreClr the cache is only updated in the native code"). PawPrint has no native cache-writer, so the sentinel stays a sentinel forever, which is exactly what we want.
- **`MaybeReplaceCacheWithLarger`.** Only reached through `TrySet`, same gated path. Inert.
- **Sentinel identity (`s_sentinelTable` in the BCL).** The managed `CastCache` struct has its *own* private `s_sentinelTable` it allocates from within an instance ctor — that's for *instance* CastCaches, not for `CastHelpers.s_table`. We don't need to coordinate.
- **Endianness.** PawPrint targets 64-bit little-endian guests only; `hashShift = 63` is correct under that constraint. Assert it in the helper.
- **Type concretisation cost.** `ConcreteTypeHandle.Array (int32Handle, 1)` should already exist in `state.ConcreteTypes` after class init touches `int[]`. If not, concretise it lazily — `allocateArray` requires the handle, but cost is one-shot.

## Verification

1. Build: `nix develop -c dotnet build WoofWare.PawPrint.slnx`.
2. Format: `nix develop -c dotnet fantomas .`.
3. Focused new test: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --no-build --filter "Name~CastHelpersTableInit" --verbosity normal`.
4. The previously-`unimplemented` repro: `nix develop -c dotnet test … --filter "Name~NullDereferenceTest" …` — expect it to pass under "Standard tests".
5. Full suite for regressions: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`.
6. Branch + commit, then `codex review --base main` per the project workflow.

## Out of scope (intentional)

- Implementing `CastCache.TrySet` semantics or any cache *update* path. The sentinel is forever-empty by design.
- Generalising the lazy-init mechanism beyond `String::Empty` and `s_table`. We've now surveyed CoreCLR for native-EE-init managed statics and found these two; adding a generic registry would be speculative until a third one surfaces.
- Fixing the *other* unimplemented-test blockers (the four native-method blockers — `SystemNative_Malloc`, `SystemNative_LowLevelMonitor_Create`, `BulkMoveWithWriteBarrierInternal`, `GetDeclaringMethodForGenericParameter`). Those are independent.
- Refreshing comments on the 15 sibling `unimplemented` entries — do that *after* observing what newly passes.

## Documentation update

After landing, update the Category B row for `CastHelpers::s_table` in `docs/runtime-initialised-statics.md` from "Pending" to a pointer at the implementing helper (`IlMachineRuntimeMetadata.internCastCacheSentinelTable` and the `UnaryMetadataFieldOps.isCastHelpersTableField` predicate).
