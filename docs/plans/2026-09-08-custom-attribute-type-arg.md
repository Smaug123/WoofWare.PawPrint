# `System.Type`-valued custom-attribute constructor arguments (TYPE, 0x50)

## Problem

`CustomAttribute_CreateCustomAttributeInstance` refuses any constructor parameter that is neither a
primitive, an SZARRAY nor an enum:

    TODO: CustomAttribute.CreateCustomAttributeInstance: ctor parameter of type ... is neither a
    primitive, an SZARRAY, nor an enum, so the fixed-args grammar cannot decode it; TYPE (0x50) and
    TAGGED_OBJECT (0x51) fixed args in particular are not yet supported

`docs/plans/2026-08-21-nunit-test-binary/README.md` (on the `worktree-nunit-spike` branch) names
this as blocker 11: xUnit's `[RegisterXunitSerializer(typeof(…), params Type[])]` assembly
attributes stop RungE of the test-binary ladder, on `main` and behind every stub. The
`params Type[]` half means an SZARRAY of TYPE is on the same critical path as the scalar.

## What CoreCLR does

`GetDataFromBlob` (`vm/customattribute.cpp:763`) for `SERIALIZATION_TYPE_TYPE` calls
`GetTypeHandleFromBlob`, which reads a SerString and — for the null sentinel — answers a null
`TypeHandle`, so the ctor receives `null`; for a non-empty name it calls
`TypeName::GetTypeReferencedByCustomAttribute(szName, pModule->GetAssembly())`, where `pModule` is
the **decorated** module (the `QCallModule` argument), not the constructor's. An empty name is a
`CustomAttributeFormatException`.

`TypeName::GetTypeReferencedByCustomAttribute` is not a native resolver. `vm/typeparse.cpp:10`
`GetTypeHelper` calls **back into managed code**:
`TypeNameResolver.GetTypeHelper(char*, RuntimeAssembly, throwOnError: true,
requireAssemblyQualifiedName: false, unsafeAccessorMethod: 0)`, which parses with
`TypeName.Parse`, resolves through `RuntimeAssembly.GetTypeCore` / `InternalLoad` / the
`AssemblyLoadContext.TypeResolve` event, throws `TypeLoadException` or `FileNotFoundException` on
a miss, and finally calls the `RuntimeTypeHandle_RegisterCollectibleTypeDependency` QCall.

For an SZARRAY of TYPE, `ReadArray` allocates a `Type[]` and calls `GetDataFromBlob` per element,
so each occurrence of a name is resolved separately, in blob order.

## Options

**A. Resolve in F#.** Parse the name (hand-rolled, or `System.Reflection.Metadata.TypeName` from a
newer SRM package than the net8.0 in-box one) and resolve it against PawPrint's assembly tables:
assembly-qualified names by display name, unqualified ones against the requesting assembly and
then CoreLib, then nested names, then generic instantiation and array/pointer/byref suffixes, then
`getOrAllocateType`. Pure, no re-entry protocol. But it re-implements `TypeNameResolver` — several
hundred lines whose corner cases (binding policy, `TypeResolve` events, the exact exception and
message on a miss) are CoreLib's, and every one of them is a place to diverge from the guest's
own `Type.GetType`.

**B. Call the managed resolver, as CoreCLR does.** The handler issues a managed call to CoreLib's
`TypeNameResolver.GetTypeReferencedByCustomAttribute(string, RuntimeModule)` for each name, in
blob order, and resumes when each returns. Fidelity is by construction: the parse, the binding,
the events and the exceptions are the ones the guest would see from `Type.GetType`, and every
improvement to `GetTypeCore` or `InternalLoad` flows through. Cost: the handler's re-entry
protocol grows a phase, and one more QCall (`RegisterCollectibleTypeDependency`) must answer.

*Chosen: B.* It is the "implement the primitive boundary itself" rule applied literally — the
boundary CoreCLR draws is a call into managed code, so that is the boundary PawPrint draws.

The overload called is `GetTypeReferencedByCustomAttribute(string typeName, RuntimeModule scope)`
rather than the `char*` `GetTypeHelper` the VM calls. For a non-empty name the two are the same
computation: `throwOnError = true`, `requireAssemblyQualifiedName = false`, no unsafe-accessor
method, `_suppressContextualReflectionContext = true`, then `RegisterCollectibleTypeDependency`.
They differ only on the empty name, which `GetTypeHandleFromBlob` rejects before either is
reached; the handler refuses it the same way. The string overload takes the `RuntimeModule` the
QCall already receives, and needs no `ReadOnlySpan<char>` to be built by hand.

### How resolved types reach the lowering

The decoder stays pure: `CustomAttribFixedArg.Type of string option` is what the bytes said.
Resolution produces one heap object per non-null name, in blob order. Two ways to hand them to
`CustomAttribValueLowering.toCliType`:

* a `Map<string, address>` keyed by name — resolves each distinct name once, which is not what
  CoreCLR does (a `TypeResolve` handler would fire once rather than per occurrence);
* an ordered list consumed as the lowering walks the value, threading the remainder — one
  resolution per occurrence, and a traversal-order mismatch between "collect the names" and
  "consume the addresses" fails loudly (an address left over, or none left at a `Type`).

The ordered list is used. `CustomAttribFixedArg.typeNamesToResolve` is the one traversal that
collects, and `toCliType` consumes in the same order because both are the natural left-to-right
walk of the same value.

### The re-entry protocol

The QCall frame's eval stack is the state machine, as in `RuntimeMethodHandle_InvokeMethod`.
`EvalStack.Values` is top-first; the bottom slot tells the phases apart:

    []                                   first entry: parse; no names to resolve → proceed
    [tN; …; t1; NullObjectRef]           resolving: k of N names answered, bottom is the sentinel
    [ObjectRef instance]                 the ctor has returned; write `instance` out

While resolving, the handler re-parses the blob on each entry (nothing has been written back
yet, exactly as during a class-init suspension today), counts the answers above the sentinel, and
either issues the next call or — with all N answered — proceeds: `ensureTypeInitialised` for the
attribute type (idempotent, so a suspension here re-enters at the same shape and proceeds), pop
the sentinel and answers, write the cursor and named-arg count, allocate, push the marker, lower
the arguments consuming the answers, call the ctor. That is CoreCLR's order too: the names are
resolved in the argument loop before the ctor call triggers the attribute's `.cctor`.

The sentinel exists because a resolved type and the phase-2 marker are both one object
reference: with exactly one name, `[t1]` and `[instance]` would be the same shape.

## Not in scope

* Named arguments of type `System.Type` (`CustomAttribute_CreatePropertyOrFieldData`). The same
  resolver would serve, but that handler is single-phase today and would need its own protocol;
  its refusal message is updated to say what it now needs.
* `TAGGED_OBJECT` (0x51), whose value carries its own type tag and depends on this and on the
  SZARRAY named-arg case.
* Any name Roslyn writes assembly-qualified. Measured on the 10.0 compiler by dumping the blobs
  it emits for `sourcesPure/CustomAttributeTypeArg.cs`: only a type declared in the assembly
  being compiled is written bare (`Decorated`, `Outer+Inner`, `Other[]`); every other type is
  qualified, `typeof(int)` included — `System.Int32, System.Runtime, Version=10.0.0.0,
  Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a` against the reference pack, and the
  `System.Private.CoreLib` identity against the implementation assemblies the test harness
  compiles against. A generic instantiation qualifies its type arguments even when they are
  local. CoreLib's resolver binds a qualified name through `RuntimeAssembly.InternalLoad`, and
  `AssemblyNative_InternalLoad` is unimplemented; that is its own feature (blocker 6 of the
  test-binary ladder), and `sourcesPure/CustomAttributeTypeArgForeign.cs` is parked on it with
  the measured refusal. So what this change makes reachable is exactly the decorated assembly's
  own types, plus `null`.

## Tests

* Decoder (`TestCustomAttributeBlob`): the round-trip property gains `Type` — null sentinel,
  empty, non-ASCII names, arrays of them; a truncated name is an error.
* Lowering (`TestCustomAttribValueLowering`): a `Type` consumes one address; a null `Type`
  consumes none; an array consumes in element order; an exhausted or surplus list fails loudly.
* Handler (`TestNativeCustomAttribute`): a `Type` parameter makes the first entry push a call to
  `TypeNameResolver.GetTypeReferencedByCustomAttribute` with the name and the decorated module,
  and leaves the cursor and named-arg count unwritten; the `RegisterCollectibleTypeDependency`
  QCall completes without touching the state.
* Guest, differential (`sourcesPure/CustomAttributeTypeArg.cs`): unqualified CoreLib type,
  same-assembly type, nested type, array type, null, and `Type[]` with three, zero and null
  elements — each followed by an int tail to catch a mis-read length.
* Guest, cross-assembly (`TestCrossAssemblyTypeAttribute`): the attribute is declared in a
  library and applied in the entry assembly to one of the entry assembly's own types. The blob
  names that type unqualified, so it resolves only if the requesting assembly is the decorated
  one rather than the constructor's.

## Outcome

Implemented as option B. Measured rather than predicted:

* `sourcesPure/CustomAttributeTypeArg.cs` exits 0 under the interpreter and on real .NET;
  `TestCrossAssemblyTypeAttribute` passes, so the scope really is the decorated assembly.
* Six mutants, each applied to the committed tree and restored by copy: resolving against the
  ctor's assembly is killed only by the cross-assembly test (the same-assembly guest and the
  fixture survive it, as they must); reversing the answers is killed by the guest (exit 13);
  omitting the sentinel by the fixture's first-entry test and the guest; unregistering
  `RegisterCollectibleTypeDependency` by the guest naming that QCall; a decoder that does not
  advance past the name by four blob tests and the guest; a lowering that does not consume by
  three lowering tests and the guest's own "unconsumed" check.
* Default suite 3327 passed; Guest category 1107 passed.

Found by running it, not anticipated above: the two further resolver gaps behind a bare name,
`GetTypeCore`'s span-marshalling stub for a nested name and `RuntimeTypeHandle_MakeSZArray` for
an array name. Both reproduce with plain `Type.GetType` and are parked as
`CustomAttributeTypeArgNested.cs` and `CustomAttributeTypeArgArrayType.cs`.
