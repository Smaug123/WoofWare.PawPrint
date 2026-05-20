# Plan: `CustomAttribute_CreateCustomAttributeInstance` QCall

## Context

`NullDereferenceTest.cs` (and `MakeGenericType{Struct,Class,New}Constraint.cs`, `ArithmeticOperations.cs`, and several others tagged in `TestPureCases.unimplemented`) is currently blocked by the unimplemented QCall

```
System.Reflection.CustomAttribute::CreateCustomAttributeInstance
```

(see `RuntimeCustomAttributeData.cs:1861` in the dotnet-runtime checkout; entry point string is `CustomAttribute_CreateCustomAttributeInstance`).

The path that lands here for `NullDereferenceTest`:

1. The test triggers a runtime-synthesised `NullReferenceException` (`ldfld` on null, `throw null`, `callvirt` on null, …).
2. `IlMachineStateExecution.raiseRuntimeException` (`IlMachineStateExecution.fs:1414`) allocates the exception via `ExceptionDispatching.allocateRuntimeException` and **calls the parameterless `.ctor`** — this is faithful to CLR's `EEException::CreateThrowable` (clrex.cpp:972, `CallDefaultConstructor(throwable)` at line 996).
3. `NullReferenceException()` calls `base(SR.Arg_NullReferenceException)` which initialises the CoreLib `ResourceManager`.
4. ResourceManager init triggers `RuntimeCustomAttributeData.AddCustomAttributes`, which iterates each custom-attribute record on the relevant metadata token and, for any ctor with parameters, calls `CreateCustomAttributeInstance` via the QCall.
5. The QCall has no handler in `NativeQCall.fs`, so dispatch falls through to `NativeCall.failUnimplemented` and the test aborts.

`AssemblyNative_GetResource` (referenced in several other `unimplemented` comments) is **already implemented** at `Native/NativeRuntimeAssembly.fs:135`; the stale comments on those tests will resolve to the same `CustomAttribute_CreateCustomAttributeInstance` blocker once retested.

## Why this is the right primitive to implement (not a workaround)

We earlier considered short-circuiting `raiseRuntimeException` to skip the parameterless ctor and write `_message` directly (mirroring `IlMachineRuntimeMetadata.synthesizeTypeInitializationException`). CLR source shows this is semantically wrong:

- **Opcode-synthesised exceptions** (`ldfld`/`stfld`/`callvirt` on null, overflow, etc.) ultimately route through `EEException::CreateThrowable` which **calls the default ctor** (`clrex.cpp:996`). Subclass-specific `.ctor` side effects (e.g., `NullReferenceException()` reading `SR.Arg_NullReferenceException` for the message) are observable.
- **`throw null`** routes through `IL_Throw` → `DispatchManagedException(kNullReferenceException)` / `COMPlusThrow(kNullReferenceException)` (`jithelpers.cpp:799,834`), which lands at the same `EEException::CreateThrowable` path — also calls the default ctor.

So bypassing the ctor would be a CLR-divergent shortcut, not a faithful primitive. `AGENTS.md` is explicit: "implement the primitive boundary itself rather than mocking or replacing a higher-level managed method that happens to call it."

### Aside: `synthesizeTypeInitializationException` is itself incorrect

While tracing the above, we confirmed that `IlMachineRuntimeMetadata.synthesizeTypeInitializationException` (`IlMachineRuntimeMetadata.fs:823`) is **not a faithful mirror of CLR**, despite the comment claiming so. CLR's `CreateTypeInitializationExceptionObject` (`src/coreclr/vm/excep.cpp:518`) allocates the TIE and then **calls `TypeInitializationException(string, Exception)`** (the `STR_EX_CTOR` MethodDesc) via `MethodDescCallSite.Call(args)` at `excep.cpp:593`. Our implementation skips the ctor and sets `_innerException`, `_typeName`, `_HResult` directly.

This is observable: the real ctor chains through `Exception(string, Exception)` which sets `_message`, `_innerException`, and `HResult` via the base-class ctors, and `TypeInitializationException` sets its own `_typeName`. Our shortcut sets the same final field values for the simple case but skips any side effects of the ctor chain (e.g., today none observable, but the same SR/ResourceManager init that gates the present plan would fire if a guest were to construct a TIE via reflection or catch one whose message is inspected through the normal getter path).

This should be fixed in the same direction as the present plan, but is **out of scope for this PR** — it is its own follow-up once `CustomAttribute_CreateCustomAttributeInstance` is in. Filed here so it is not forgotten:

> **Follow-up:** Replace `synthesizeTypeInitializationException`'s direct field writes with a real ctor call to `TypeInitializationException(string, Exception)` (mirror CLR's `CreateTypeInitializationExceptionObject`). The current implementation is a hack; the comment claiming CLR-fidelity is inaccurate.

## Approach

Implement the QCall in a new handler module `Native/NativeCustomAttribute.fs`, register it in `NativeQCall.fs`. Modelled on `Native/NativeRuntimeAssembly.fs`/`Native/NativeRuntimeType.fs` (pattern match on QCall name + signature, decode args via `NativeCall` helpers, mutate state, return `ExecutionResult.stepped`).

### Inputs (per `customattribute.cpp:900-1020` and `RuntimeCustomAttributeData.cs:1861`)

| Param | C# type | Native shape | Meaning |
| --- | --- | --- | --- |
| `pModule` | `QCallModule` | pointer to module ref | The decorated module owning the metadata token. Needed to resolve `TYPE`/`STRING` blob references. |
| `type` (in) | `ObjectHandleOnStack` (`RuntimeType`) | pointer to `RuntimeType` object | Attribute type to instantiate. |
| `pCtor` (in) | `ObjectHandleOnStack` (`IRuntimeMethodInfo`) | pointer to `RuntimeMethodInfo` | Attribute ctor MD reflection wrapper. |
| `ppBlob` (in/out) | `ref IntPtr` | pointer to pointer | Cursor into the attribute blob; advanced past the fixed-args section. |
| `pEndBlob` (in) | `IntPtr` | end pointer | Hard limit for blob reads. |
| `pcNamedArgs` (out) | `out int` | pointer to int | On success, the named-arg count read from the blob; the caller loops `cNamedArgs` times after the QCall returns. |
| `instance` (out) | `ObjectHandleOnStack` | pointer to object slot | The freshly-allocated, ctor-invoked attribute instance. |

### What the QCall does

1. Recover the ctor `MethodDesc` and attribute type from the two `ObjectHandleOnStack` args.
2. Allocate a zero-initialised instance of the attribute type (no-arg allocation; the ctor will run on this `this`).
3. Walk the fixed-arg section of the blob using the ctor's signature; for each fixed arg call `GetDataFromBlob` (one of `BOOLEAN/CHAR/I1..I8/U1..U8/R4/R8/STRING/TYPE/ENUM/TAGGED_OBJECT/SZARRAY`).
4. Push `this` + decoded args onto the eval stack of a new ctor frame, mark the frame so the return path produces the instance rather than a regular call result, and step into the ctor.
5. After the ctor returns, read the named-arg count (a `uint16` immediately after the fixed args), update `*ppBlob` to point at the first named-arg entry, write `instance` and `cNamedArgs` to their out slots.

### Implementation pieces required

#### A. Domain-side blob reader for `CustomAttrib` fixed args

`WoofWare.PawPrint.Domain/CustomAttribute.fs` already has `tryReadLeadingSerString` (for the `NeutralResourcesLanguageAttribute` shortcut elsewhere). It needs a generalised reader:

```fsharp
type CustomAttribFixedArg =
    | Bool of bool
    | Char of char
    | I1 of sbyte | U1 of byte
    | I2 of int16 | U2 of uint16
    | I4 of int32 | U4 of uint32
    | I8 of int64 | U8 of uint64
    | R4 of float32 | R8 of double
    | String of string option  // None for SerString null sentinel 0xFF
    | Type of string option    // SerString of assembly-qualified type name; None for null
    | Enum of underlyingType : CustomAttribFixedArg * declaredType : string
    | TaggedObject of CustomAttribFixedArg
    | SzArray of CustomAttribFixedArg list  // length read from blob; null = -1 elements
```

with a function

```fsharp
val readFixedArgs :
    ctorParamTypes : TypeDefn list ->
    blob : ImmutableArray<byte> ->
    startOffset : int ->
    Result<CustomAttribFixedArg list * int (* offset advanced past fixed args *), string>
```

mirroring CoreCLR's `GetDataFromBlob` (`customattribute.cpp` near line 200) and ECMA-335 II.23.3.

This belongs in `Domain` (not `Native`) because the parsing is pure metadata work. Keep it total: errors return `Result.Error`, never raise. The QCall handler will translate `Error` into the appropriate managed `CustomAttributeFormatException` (CoreCLR throws `kCustomAttributeFormatException`).

#### B. Eval-stack values for fixed args

Convert each `CustomAttribFixedArg` to a `CliType` matching the ctor parameter slot:

- Primitives map directly to `CliType.Numeric` / `CliType.Bool` / `CliType.Char`.
- `String s` → `Some s` becomes an allocated managed string via `allocateManagedString`; `None` is `CliType.ObjectRef None`.
- `Type t` → resolve type name to a `RuntimeType` (manually, the same way `RuntimeTypeHandle_ConstructName` etc. round-trip) and box. We probably need to share the resolution code with `Type.GetType` intrinsics.
- `Enum (underlying, declared)` → the boxed value of the underlying primitive in the declared enum type; allocate a boxed enum.
- `TaggedObject` → recursively decode the tagged value, box appropriately.
- `SzArray` → allocate an array of the right element type and write decoded entries.

Most attributes in the SR/ResourceManager init path have only primitive + `String` + `Type` fixed args, so the boxing/enum/array paths are not on the critical path but should be implemented for correctness — the project preference is "general correct solutions."

#### C. Ctor invocation through the dispatch loop

The QCall is synchronous in CLR (`ctorCallSite.CallWithValueTypes(args)`), but in PawPrint we can't synchronously run a managed ctor inside a single dispatch step. Two options here:

1. **Step-by-step**: push a new method frame for the ctor with the decoded args, return `WhatWeDid.Executed` immediately, and rely on the post-return path to write the `ObjectHandleOnStack` slot. This requires a new `MethodCallReturnDisposition` case (or extending `WhatWeDid`) that says "when this frame returns, write its `this` to `<address>` rather than pushing to caller's eval stack." This is the same shape as `dispatchAsExceptionOnReturn` already used in `raiseRuntimeException` for ctor-on-exception.
2. **In-line**: synthesise a small chain of pseudo-IL steps. Avoid — fragile.

Pick option 1. We already have precedent: `raiseRuntimeException` (`IlMachineStateExecution.fs:1486`) passes `dispatchAsExceptionOnReturn = true` to `callMethod` so the ctor's `Ret` triggers exception dispatch instead of pushing the result. Add an analogous boolean `writeThisToObjectHandleOnStack : ManagedPointerSource option` (or a small DU for return-disposition) that tells `returnStackFrame` to write the constructed `this` to the supplied native pointer slot.

#### D. Named-arg blob position

After the ctor frame returns, we still need to:

- Read the `uint16` named-arg count from the blob (the position immediately after the fixed args).
- Write `*ppBlob` = current blob cursor.
- Write `*pcNamedArgs` = the count.

These writes need to happen *after* the ctor completes. The natural place is the same return-disposition handler from (C): when the synthesised ctor frame returns, also perform these out-pointer writes.

This argues for the return disposition being a record:

```fsharp
type CustomAttributeCtorReturn =
    {
        InstanceSlot : ManagedPointerSource  // ObjectHandleOnStack target for `instance`
        BlobCursorOut : ManagedPointerSource // ref IntPtr ppBlob
        NamedArgCountOut : ManagedPointerSource // out int pcNamedArgs
        BlobCursorAfterFixedArgs : int
        BlobEnd : int
    }
```

stored on the frame; consumed at `Ret`.

#### E. Registration

Add `"CustomAttribute_CreateCustomAttributeInstance"` to the `NativeQCall.handlers` map, pointing at `NativeCustomAttribute.tryExecuteQCall`.

### What this PR does *not* include

- `CustomAttribute_CreatePropertyOrFieldData` QCall — also a `[LibraryImport(... CustomAttribute_CreatePropertyOrFieldData ...)]` in `RuntimeCustomAttributeData.cs`, called once per named arg after `CreateCustomAttributeInstance` returns. Some attributes in the resource-init path may have zero named args, in which case it isn't needed; if any do (e.g., `NeutralResourcesLanguageAttribute.Location` is a property), we'll hit it as the next blocker. **Address in a follow-up PR if it actually fires** — out of scope here so the present PR stays reviewable.
- Fixing `synthesizeTypeInitializationException` to actually call the ctor (see Aside above). Separate follow-up.
- Any work on `SR.InternalGetResourceString`'s recursion-detection / `Environment.FailFast` path. If our resource-blob parsing path works end-to-end after (A)–(E), the fallback never fires; if it does fire, that's a separate investigation.

### Likely next blockers after this PR lands

Once `CustomAttribute_CreateCustomAttributeInstance` works, `NullDereferenceTest` will resume execution further into ResourceManager init. The candidates are, in rough order of likelihood:

1. `CustomAttribute_CreatePropertyOrFieldData` (if any attribute on the path has named args).
2. Resource-blob parsing failures in `RuntimeResourceSet`/`ResourceReader` (all managed; should work, but possibly trips on an unimplemented IL opcode or InternalCall).
3. The `Arg_NullReferenceException`-lookup infinite-recursion `Environment.FailFast` (`SR.cs:71`) — would indicate our resource parsing returned `null` from `GetString`.

Per `AGENTS.md`, each becomes its own incremental PR. The `unimplemented` entry for `NullDereferenceTest.cs` stays in `TestPureCases.fs` with an updated comment after each step.

## Validation

- Property/unit test for the new domain-side blob reader in `WoofWare.PawPrint.Test`: generate random ctor signatures + corresponding well-formed blobs, assert round-trip; assert that truncated / malformed blobs produce `Result.Error`.
- A focused test that constructs a small attribute with a primitive + string fixed arg via the QCall path, checks the resulting instance's fields.
- After landing, retest `NullDereferenceTest.cs` (and the other entries tagged with the same blocker in `unimplemented`) and update their comments with the new next-blocker.
