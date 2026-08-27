# `AssemblyNative_GetModules`

## What is blocked

`Assembly.GetModules()`, `Assembly.GetModules(bool)` and `Assembly.GetLoadedModules(bool)` all
bottom out in this one QCall. Measured in this worktree with the playground guest:

```
Unhandled exception. WoofWare.PawPrint.GuestFailureException: Unimplemented native method
(PInvokeImpl QCall!AssemblyNative_GetModules): System.Private.CoreLib
System.Reflection.RuntimeAssembly::<GetModules>g____PInvoke|89_0(
    System.Runtime.CompilerServices.QCallAssembly, System.Int32, System.Int32,
    System.Runtime.CompilerServices.ObjectHandleOnStack) -> void.
  Guest was: ... called 5 frames out from CSharpExample.Program.Main at IL offset 7
```

So the QCall is the only blocker on the way in, and the signature to match on is
`[QCallAssembly; Int32; Int32; ObjectHandleOnStack] -> Void`. The two `Int32`s are the
`[MarshalAs(UnmanagedType.Bool)] bool` parameters after the `LibraryImport` stub has lowered
them; the stub's own IL is `brtrue`-based, so only 0 and 1 ever reach the handler.

## Upstream contract

`AssemblyNative_GetModules` (`coreclr/vm/assemblynative.cpp:696`):

```cpp
HENUMInternalHolder phEnum(pAssembly->GetMDImport());
phEnum.EnumInit(mdtFile, mdTokenNil);
InlineSArray<Module *, 8> modules;
modules.Append(pAssembly->GetModule());
mdFile mdFile;
while (pAssembly->GetMDImport()->EnumNext(&phEnum, &mdFile))
{
    if (fLoadIfNotFound)
    {
        Module* pModule = pAssembly->GetModule()->LoadModule(mdFile);
        modules.Append(pModule);
    }
}
orModules = (PTRARRAYREF)AllocateObjectArray(modules.GetCount(), CoreLibBinder::GetClass(CLASS__MODULE));
for (...) orModules->SetAt(i, pModule->GetExposedObject());
retModules.Set(orModules);
```

Four facts fall out of that, each of which the implementation has to reproduce:

1. **The manifest module is always element 0**, and it is the only element that can ever exist.
   `ModuleBase::LoadModule` (`vm/ceeload.cpp:2543-2575`) reads the row's name and then
   unconditionally `EEFileLoadException::Throw(name, COR_E_MULTIMODULEASSEMBLIESDIALLOWED)`. So the
   loop either appends nothing (no `File` rows, or `fLoadIfNotFound` false) or throws.

   That is airtight rather than merely true-so-far, but for two reasons a reader will not see from
   the call site, both worth writing down because a future grep will turn up something that looks
   like a counterexample. First, `LoadModule` *is* virtual (`vm/ceeload.h:539`) and there *is* an
   override that returns a module (`vm/readytoruninfo.cpp:1777`) — but it is `final` on
   `NativeManifestModule`, the synthetic metadata module of a composite ReadyToRun image, which is
   never what `pAssembly->GetModule()` returns; `class Module` itself has no override. Second,
   `ModuleBase::LoadModule` has an `mdtModuleRef` arm, but `EnumInit(mdtFile, mdTokenNil)` yields
   only `mdtFile` tokens, so that arm is dead from this call site.
2. **`fGetResourceModules` is dead.** It is not read anywhere in the function body. A resource-only
   `File` row is therefore treated exactly like a code module row.
3. **The array's element type is `System.Reflection.RuntimeModule`**, not `Module` —
   `DEFINE_CLASS(MODULE, Reflection, RuntimeModule)` at `vm/corelib.h:615`. The managed local is
   `RuntimeModule[]?` too (`RuntimeAssembly.cs:678-685`); only the public `GetModules(bool)`
   signature widens it to `Module[]`.
4. **The array is freshly allocated per call; the elements are not.** `GetExposedObject()` is
   cached on the `Module`, so `GetModules()[0]` is reference-equal to `ManifestModule` and to the
   element of any other `GetModules()` array.

### Measured, not assumed

Two probes, both run against real .NET 10 in this worktree.

Single-file assembly (`CSharpExample`, published osx-arm64):

```
count=1            same=True                scope=CSharpExample.dll
loadedCount=1      loadedSame=True          elemType=System.Reflection.RuntimeModule
```

Assembly with one `File` row, built with `<LinkResource Include="payload.bin" />` (scratchpad
project `linkres`):

```
assembly=LinkResProbe
GetModules() threw System.IO.FileLoadException hr=0x8013101E
  msg=Could not load file or assembly 'payload.bin'. The module cannot be loaded because only
      single file assemblies are supported. (0x8013101E)
  fileName=payload.bin
GetLoadedModules() ok, count=1
GetModules(true) threw System.IO.FileLoadException
```

That confirms points 1–4 directly: `getResourceModules` genuinely does not save you from a
resource-only `File` row; `GetLoadedModules` (`loadIfNotFound = false`) is total; and the thrown
exception carries a `FileName` taken from the **`File` row**, not from the assembly, plus
`HResult = 0x8013101E`.

## Decisions

### D1 — where the handler lives

**Chosen: a new arm in `NativeRuntimeAssembly.tryExecuteQCall`.** It is declared on
`RuntimeAssembly`, its handle argument is a `QCallAssembly`, and its entry point is in the
`AssemblyNative_*` family whose sixteen siblings are already in that match block.

Rejected: `NativeRuntimeModule.fs`. That file is scoped by its own docstring to "QCalls declared on
`System.Reflection.RuntimeModule`". Putting an `AssemblyNative_*` entry point there would falsify
that docstring, and the handler's work is assembly-shaped (resolve the handle, read the assembly's
`File` table) with the module only appearing at the end.

### D2 — what to do when the `File` table is non-empty and `loadIfNotFound` is true

This is the only genuinely contested decision, and the probe above makes it concrete rather than
hypothetical: a `csc /linkresource` assembly reaches it, and PawPrint can build one in a test.

- **Option 2a — refuse loudly (`failwith`) naming the offending `File` row.** Cost: near zero.
  Blast radius: the realistic guest that *catches* this exception is an assembly-scanning plugin
  loader — MEF-shaped code wraps `GetModules`/`GetTypes` in catch-and-skip as a matter of routine —
  and under 2a such a guest kills the interpreter where real .NET would have skipped the assembly
  and carried on. Reversible: entirely — replacing a `failwith` with a raise later is a local
  change. Information preserved: maximal; the failure names the row and the assembly.
- **Option 2b — `NativeHandlerResult.raiseExceptionWithMessage FileLoadException (Some "...")`.**
  Cost: near zero. But the existing raise machinery calls the *parameterless* ctor and then
  overwrites `_message` (`AbstractMachine.fs:146`), and there is no re-entry point at which a
  handler could set the other fields afterwards. Both of the fields that would then be wrong are
  guest-readable, and both ends of the divergence are measured rather than guessed:
  `new FileLoadException()` gives `HResult = 0x80131621` (`COR_E_FILELOAD`) and `FileName = null`,
  where the real throw gives `0x8013101E` and `FileName = "payload.bin"`. Buying guest-catchability
  at the price of two silently wrong values is exactly what
  [[prefer-crashing-over-documented-divergence]] declines.
- **Option 2c — extend the raise machinery to invoke a chosen ctor with arguments**, so
  `FileLoadException(string fileName, int hResult)` can be called the way
  `EEFileLoadException::CreateThrowable` calls it (`vm/clrex.cpp:1594-1620`). Faithful, and it
  would pay off for every future QCall whose exception carries more than a message. Cost: a change
  to `NativeHandlerResult.RaiseException` and its dispatcher, i.e. a mechanism change touching the
  whole native surface.

**Chosen: 2a for this PR.** 2c is a separate feature and AGENTS.md is explicit that a dependency
like that should land as its own PR first if we want it; 2b buys guest-catchability at the price of
two wrong field values, which is the trade this project declines. If Patrick would rather have the
real exception, the order is "2c as its own PR, then this one raising instead of refusing" — and
this plan should be re-cut that way before any code is written, not after.

Note that the *other* three cells of the grid need no policy at all: with `loadIfNotFound` false
the `File` rows are skipped, so `GetLoadedModules` stays total even on a linked-resource assembly.

### D3 — where the `File` table is read

**Chosen: in the handler, off `assembly.PeReader.GetMetadataReader()`.** That is established
practice for `Native/` handlers (`NativeSignature.fs:68`, `NativeEnum.fs:290`,
`NativeDelegate.fs:799`), and the handler needs only "is it empty, and if not, the first row's
name".

Rejected: a new member on `DumpedAssembly`. `WoofWare.PawPrint.Domain` is a published package
([[domain-is-a-published-package]]), so surface added there is surface we owe compatibility to, and
one caller does not earn it.

### D4 — module identity

**Chosen: `NativeRuntimeType.getOrAllocateRuntimeModule` keyed on `assembly.DefinitionFullName`**,
which is exactly what the `RuntimeAssembly.GetManifestModule` FCall already does
(`NativeRuntimeAssembly.fs:315-332`). Its cache is `state.RuntimeModuleObjects`, so element 0 comes
back reference-equal to `ManifestModule` — the `same=True` the probe measured. Allocating a fresh
`RuntimeModule` per call would be observably wrong.

Spelling it `assembly.DefinitionFullName` rather than reusing the name taken off the handle is
*only* a consistency choice, not a correctness one, and the plan should not pretend otherwise: the
two cannot differ past step 2. `IlMachineState.LoadedAssembly` is `TryByDefinitionName`, an exact
`ImmutableDictionary` lookup whose key is written as `assy.Name.FullName`
(`WoofWare.PawPrint.Domain/Assembly.fs:911`), and `DefinitionFullName` is the cached serialisation
of that same `AssemblyName`. So a handle naming anything else fails the lookup and takes the "is not
loaded" refusal instead. Write the sibling's expression because it is the sibling's expression.

### D5 — `getResourceModules`

**Chosen: ignore it, with a comment saying CoreCLR ignores it and pointing at the probe.** Adding a
refusal for `true` would break `Assembly.GetModules(true)`, which the probe shows returns normally.

## Implementation sketch

One arm in `NativeRuntimeAssembly.tryExecuteQCall`, registered in `NativeQCall.fs`'s handler map
(the test harness deliberately dispatches through `NativeQCall.tryExecute`, so a missing
registration fails a test rather than being silent):

```
| "AssemblyNative_GetModules",
  "System.Private.CoreLib", "System.Reflection", "RuntimeAssembly",
  [ CorelibType ... "QCallAssembly" ...
    ConcretePrimitive ... PrimitiveType.Int32
    ConcretePrimitive ... PrimitiveType.Int32
    CorelibType ... "ObjectHandleOnStack" ... ],
  MethodReturnType.Void when ... ->
```

Body:

1. Arity check (`instruction.Arguments.Length <> 4` → `failwith`), matching the family.
2. `NativeCall.qCallAssemblyToAssemblyFullName` → `state.LoadedAssembly` → `failwith "... is not
   loaded"` (family-consistent).
3. `NativeCall.int32Argument` for `loadIfNotFound`; `getResourceModules` read and discarded, with
   the D5 comment.
4. If `loadIfNotFound <> 0`, enumerate `metadataReader.AssemblyFiles`; if non-empty, `failwith`
   naming the first row's name in rid order (that is the row CoreCLR's `EnumNext` reaches first,
   and `LoadModule` throws on the first one it sees) and the assembly, and stating the condition:
   multi-module / linked-file assemblies are not supported.
5. `getOrAllocateRuntimeModule` for `assembly.DefinitionFullName`.
6. `concretizeNonGenericCorelibType ... "System.Reflection" "RuntimeModule"`, `allocateArray
   (ConcreteTypeHandle.OneDimArrayZero moduleHandle) ... 1`, `setArrayValue ... 0`.
7. `IlMachineState.writeManagedByrefWithBase` into the `ObjectHandleOnStack` target from
   `NativeCall.objectHandleOnStackTarget`. Unconditional — `GetModulesInternal` starts from `null`
   and returns `modules!`, so a skipped write is a guest `NullReferenceException`, the same shape
   `RuntimeModule_GetTypes` already documents.
8. `NativeHandlerResult.completed`.

## Test plan

### Unit — `WoofWare.PawPrint.Test/TestAssemblyNativeQCalls.fs`

A new `invokeGetModules` helper alongside `invokeGetFlags` / `invokeStringQCall`, taking
`loadIfNotFound` and `getResourceModules` and returning the address the handler wrote into the
`ObjectHandleOnStack` (the existing `objectHandleOnStackValue` gives the null-initialised slot).

1. **Shape**: length 1, and element 0 is the module object for that assembly.

   The oracle needs choosing explicitly, because the obvious one is not reachable from this
   fixture: `RuntimeAssembly.GetManifestModule` is an FCall, so it sits in
   `NativeRuntimeAssembly`'s internal-call block and `NativeQCall.tryExecute` cannot reach it, and
   nothing in the test project invokes it today. Two options. (a) Build an internal-call invoker —
   `NativeDispatch.tryExecute` is the entry point, precedent at `TestInternalAlloc.fs:264`, but it
   needs a `RuntimeAssembly` heap object as argument 0 and so is real harness work. (b) Use
   `getOrAllocateRuntimeModule` directly. **Take (b)**, and note what it costs: it is a mirror
   oracle sharing the implementation's own expression, so it proves identity but not that the
   identity is the one `ManifestModule` reports. Two consequences to respect — call the oracle
   *before* `invokeGetModules`, or the handler primes the cache and the "fresh `RuntimeModule` per
   call" mutant survives; and lean on the guest test for the genuine cross-surface fact, which is
   where `ReferenceEquals(GetModules()[0], ManifestModule)` earns its place
   ([[mirror-oracle-can-share-your-mistake]]).
2. **Element type**: the allocated array's element handle is `System.Reflection.RuntimeModule` —
   the fact the `elemType=System.Reflection.RuntimeModule` probe pins. Without this, `Module` or
   `Object` passes.
3. **Flag sweep on a single-file assembly**: all four `(loadIfNotFound, getResourceModules)`
   combinations give the same answer. Cheap, and it catches a handler that refuses outright on some
   flag value — but note what it *cannot* do: on a single-file assembly the four cells are
   indistinguishable by construction, so this test on its own says nothing about whether either
   flag is read correctly.
4. **Array freshness vs element stability**: two calls give different array addresses and the same
   element address. This is the pair that distinguishes CoreCLR's per-call `AllocateObjectArray`
   from its cached `GetExposedObject`.
5. **Unloaded handle**: an assembly full name that is not loaded gets the family's "is not loaded"
   refusal.
6. **`File`-row assembly, swept over the same 2×2** — built with
   `Roslyn.compileAssemblyWithResources` and the `linkedResource` `ResourceDescription` shape
   already used by `TestManifestResources.fs:22-33`. This is the *only* input on which the two
   flags are distinguishable at all, so it is here — not in test 3 — that
   [[sweep-a-parameter-dont-test-extremes]] applies:
   - `(loadIfNotFound = 1, getResourceModules = 0)` → refusal naming the linked file and the
     assembly;
   - `(1, 1)` → refusal;
   - `(0, 1)` → array of length 1, no refusal;
   - `(0, 0)` → array of length 1, no refusal.

   All four rows are load-bearing, and the pairs kill different mutants. Dropping the
   `loadIfNotFound = 0` rows lets a mutant that refuses on `File` rows unconditionally survive,
   which would make `GetLoadedModules` wrong on exactly the assemblies it is documented to work on.
   Dropping the `getResourceModules` asymmetry lets through two mutants that a reader of the
   *public API documentation* would plausibly write: an argument-position swap that gates the
   refusal on the wrong `Int32`, and "skip resource-only `File` rows when `getResourceModules` is
   false" — which is what the probe exists to refute, since a linked resource is precisely a
   resource-only `File` row and real .NET throws for it anyway.

### Guest — `WoofWare.PawPrint.Test/sourcesPure/AssemblyGetModules.cs`

Cross-runtime facts only ([[differential-tests-only-cross-runtime-facts]]); every assertion below
was measured on real .NET above. `GetModules().Length == 1`;
`ReferenceEquals(GetModules()[0], ManifestModule)`; `GetLoadedModules().Length == 1`;
`GetModules(true).Length == 1`; two `GetModules()` calls return different arrays.

`sourcesPure` is auto-discovered, so this needs no registration — but it does need the `Guest`
fixtures run (`--filter "TestCategory=Guest&FullyQualifiedName~TestPureCases"`). If it turns out to
hit a *downstream* blocker (`Module.ScopeName` is implemented; `Array.GetType().GetElementType()`
on a `RuntimeModule[]` is unverified), park it in `unimplemented` and say so rather than growing
this PR — the playground probe deliberately asked more of the runtime than the guest test will.

### Mutation — before claiming the tests cover anything

Per the `mutation-testing` skill, commit first, then break each of these and confirm which test
goes red:

| mutant | expected killer |
| --- | --- |
| element type `Module` instead of `RuntimeModule` | unit 2 |
| skip the `ObjectHandleOnStack` write | unit 1 (null slot), guest (NRE) |
| allocate a fresh `RuntimeModule` rather than `getOrAllocateRuntimeModule` | unit 1 and 4, guest |
| return the same array object on both calls | unit 4 |
| drop the `File`-table check | unit 6, `(1, 0)` |
| refuse on `File` rows regardless of `loadIfNotFound` | unit 6, `(0, 1)` and `(0, 0)` |
| gate the refusal on argument index 2 (`getResourceModules`) instead of 1 | unit 6, `(1, 0)` |
| skip resource-only `File` rows when `getResourceModules` is false | unit 6, `(1, 0)` |

Read which test each mutant actually killed rather than assuming
([[mutants-of-one-site-cannot-test-another]]).

**Measured outcome.** All eight mutants run were killed. Two of the killings are the ones worth
recording, because they are what the extra `getResourceModules` rows bought: the argument-position
swap and "skip resource-only `File` rows when `getResourceModules` is false" were each killed
*only* by the linked-resource fixture's `(loadIfNotFound = 1, getResourceModules = 0)` row. Every
other test in the file stayed green for both. Two further mutants — allocating a zero-length array,
and leaving the handle at null — were killed broadly, as expected.

One mutant deliberately **not** in that table: keying the module cache on the handle's name rather
than `assembly.DefinitionFullName`. Per D4's second paragraph the two are the same string past the
lookup, so that mutant is behaviourally equivalent and no test can kill it. It is listed here so
that the mutation session does not spend time rediscovering it and does not mistake its survival
for a gap in the tests.

## Housekeeping in the same commit

`TestAssemblyNativeQCalls.fs`'s fixture docstring currently opens "Tests for the QCalls behind
`Assembly.GetName()`". Adding the `GetModules` tests falsifies it, so it needs rewording in the same
commit rather than being left to drift.

## Out of scope

- `AssemblyNative_GetModule` (singular, by name) — a different entry point with a different
  contract; one feature at a time.
- `Assembly.GetFile` / `GetFiles` — these want a `FileStream` over the emulated filesystem.
- Linked-file manifest resources in `AssemblyNative_GetResource`, which still refuses
  (`NativeRuntimeAssembly.fs:411`). Related, but a separate gap.
- The faithful `FileLoadException` (D2 option 2c).
