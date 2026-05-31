# Runtime-Initialised Statics

*Authorship: LLM*

This document catalogues BCL static fields whose values come from the .NET *runtime* (the JIT or the EE) rather than from any managed `.cctor`. PawPrint has neither a JIT nor an EE startup hook, so it must fake these initialisations. If you are reading this because a guest program is failing with

> Encountered infinite recursion while looking up resource 'Arg_NullReferenceException' in System.Private.CoreLib

the most likely cause is a missing entry on this list: a spurious second `NullReferenceException` is being raised because PawPrint read one of these statics as `default(T)` (i.e. `null` for reference types) and the BCL then dereferenced it. Constructing the message for the first NRE re-enters resource lookup, hits `SR.InternalGetResourceString`'s recursion guard, and `FailFast`s.

Debugging recipe: temporarily add a logger to `IlMachineStateExecution.raiseRuntimeException` that walks `ts.ActiveMethodState` along `ReturnState.JumpTo` and emits each frame's `ExecutingMethod`/`IlOpIndex`. The second NRE's stack will name the BCL call site that dereferenced the unfaked static.

## Category A — JIT-intrinsic statics

The CoreCLR JIT replaces `ldsfld` on these three fields with a constant load. Defined exhaustively in `getFieldIntrinsic` at `dotnet-runtime/src/coreclr/vm/jitinterface.cpp` (search for `CORINFO_FIELD_INTRINSIC_EMPTY_STRING`):

| Field | Intrinsic | PawPrint status |
|---|---|---|
| `System.String::Empty` | `CORINFO_FIELD_INTRINSIC_EMPTY_STRING` | Faked. `UnaryMetadataFieldOps.isSystemStringEmptyField` + `IlMachineRuntimeMetadata.internCanonicalEmptyString` (commit `6b8bf34`). |
| `System.IntPtr::Zero` and `System.UIntPtr::Zero` | `CORINFO_FIELD_INTRINSIC_ZERO` | No fix needed. The fields are declared `static readonly nint Zero;` with no initialiser; `cliTypeZeroOf` of `nint`/`nuint` is `0`, which coincides with the intended value. |
| `System.BitConverter::IsLittleEndian` | `CORINFO_FIELD_INTRINSIC_ISLITTLEENDIAN` | No fix needed on our supported targets. The field has a managed source initialiser `= true` under `!BIGENDIAN` (see `BitConverter.cs`), so the ordinary `.cctor` runs and produces the right value. The intrinsic exists only to fold the `ldsfld` to a constant; correctness does not depend on it. |

## Category B — Native-EE-initialised statics

These are *not* JIT intrinsics. They are regular static slots that the native EE writes during startup via `CoreLibBinder::GetField(...)->GetCurrentStaticAddress()` + `SetObjectReference(...)`. Managed `ldsfld` on them reads whatever the EE installed.

| Field | Initialiser | PawPrint status |
|---|---|---|
| `System.Runtime.CompilerServices.CastHelpers::s_table` | `CastCache::Initialize()` in `coreclr/vm/castcache.cpp:112-138`, invoked from `appdomain.cpp:1072` inside `SystemDomain::LoadBaseSystemClasses`. Installs a 2-entry sentinel `int[]` shaped so that `CastCache.TryGet` always returns `MaybeCast` until the native cache writer (only reached from native fast paths) installs a real cache. | Faked. `UnaryMetadataFieldOps.isCastHelpersTableField` + `IlMachineRuntimeMetadata.internCastCacheSentinelTable`. Installs a sentinel `int[18]` on first read of the field, mirroring the `System.String::Empty` lazy-init. |

### Why we believe Category B is complete

Grep across `dotnet-runtime/src/coreclr/vm/` for `SetObjectReference((OBJECTREF *)...)` against a `FieldDesc`'s static address (the marker pattern for native-EE writes to a managed static slot). The only hits are:

- `castcache.cpp:92, 109, 137` — all three are `CastCache::Initialize` / `MaybeReplaceCacheWithLarger` / `FlushCurrentCache` writing to the same `s_table` field.
- `threadstatics.cpp:483` — writes to a *per-thread* TLS base pointer, not an `ldsfld`-visible managed static.

There are no other instances in CoreCLR. If a future BCL change adds another EE-initialised static, the symptom will again be a spurious NRE during BCL string-construction; the debugging recipe above will identify it, at which point add a row here and another lazy-init arm to `UnaryMetadataFieldOps`.

## Adding a new entry

1. Reproduce the spurious NRE and capture the inner stack with the `raiseRuntimeException` frame-walker.
2. Identify the `ldsfld`/`ldsflda` site where the unfaked static is read. The IL offset will land at the `Call`/`Ldflda` immediately after the `ldsfld`.
3. Decide whether `cliTypeZeroOf` already matches the intended value (Category A row 2 and 3) or whether the BCL truly requires a non-default value (everything else).
4. If a fake is required, add a predicate and a lazy-allocator following the `System.String::Empty` precedent (`UnaryMetadataFieldOps.fs` + `IlMachineRuntimeMetadata.fs`), plus a focused reflection-driven test under `WoofWare.PawPrint.Test/sourcesPure/`.
5. Update the table here with a one-line summary and a pointer to the implementing helper.
