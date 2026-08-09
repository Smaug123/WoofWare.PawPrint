# `RuntimeTypeHandle_GetActivationInfo` QCall

Status: implemented. The plan below is as written before implementation, except for this
section, which records what actually happened.

## What implementation found that the plan did not predict

**A prerequisite gap in `stind`, unrelated to this QCall.** A pointer-typed field (`void*`,
`delegate*<...>`) is stored as a `CliType.RuntimePointer` cell, which has no byte image at
all, and `writeIndirectPrimitiveStore` sent every store into one down the byte-scatter path,
where it was refused. That made *any* `stind` through a `ref`/`out` parameter bound to a
pointer-typed field fail — including a store of plain zero. It has a standalone repro with no
reflection in it (`sourcesPure/PointerFieldIndirectStore.cs`) and fails identically on `main`,
so it is a pre-existing bug rather than something this feature introduced; but the CoreLib
shim assigns all five of its results into `ActivatorCache`'s fields exactly this way, so the
QCall cannot work without it. Fixed by widening the *destination-side* test — a new
`isDestinationWithoutByteImage`, distinct from the payload-side `isNumericProvenanceRejection`
— to route same-width stores into a pointer cell to the typed writer. It is a separate commit.

**The out-params' cell shapes are load-bearing.** Writing the MethodTable pointer as a
`NativeInt` rather than a `CliType.RuntimePointer` made the guest's own copy of that local into
`ActivatorCache`'s `void*` field take the byte path. The values written must match the shape
of the pointer-typed slots they land in.

**`MulticastDelegate` is not a delegate type**, so the delegate-vs-abstract ordering concern
resolves differently than expected: CoreCLR sets its `IsDelegate` flag only for types whose
*immediate* base is `MulticastDelegate`, so `typeof(Delegate)` and `typeof(MulticastDelegate)`
fall through to the abstract check and yield `MissingMethodException`, while a real delegate
type yields `ArgumentException`. Confirmed against .NET 10 and pinned by the differential test.

**`MarshalPtrToStructure.cs` un-parked**: it passes in full now.

## What is blocked today

`Activator.CreateInstance(typeof(T))` (and every overload that funnels through
`RuntimeType.CreateInstanceDefaultCtor`) dies immediately:

```
Unimplemented native method (PInvokeImpl QCall!RuntimeTypeHandle_GetActivationInfo):
  System.RuntimeTypeHandle::GetActivationInfo(ObjectHandleOnStack,
    *(fnptr(*(Void)->Object)), *(*(Void)), *(fnptr(Object->void)),
    *(fnptr(&(Byte)->void)), *(.BOOL)) -> void
```

Measured, not assumed: a guest of `Activator.CreateInstance(typeof(Widget))` where
`Widget` is a plain class reaches this QCall and nothing earlier gets in the way.
`WoofWare.PawPrint.Test/sourcesPure/MarshalPtrToStructure.cs` is parked on the same
gap (`Marshal.PtrToStructure(IntPtr, Type)` allocates via `Activator.CreateInstance`
before marshalling anything).

## What CoreCLR does

`reflectioninvocation.cpp:1565`. Given a `RuntimeType`, it validates the type is
instantiable and then hands back five out-values describing how to *activate it via
`calli`*:

| out-param | managed sig | CoreCLR value |
| --- | --- | --- |
| `ppfnAllocator` | `void* -> object` | `CEEJitInfo::getHelperFtnStatic(getNewHelperStatic(pMT))` — a **JIT helper**, not a managed method |
| `pvAllocatorFirstArg` | `void*` | `pMT` |
| `ppfnRefCtor` | `object -> void` | default ctor's **boxed** entry point (`forceBoxedEntryPoint = isValueType`), or null |
| `ppfnValueCtor` | `ref byte -> void` | default ctor's **unboxed** entry point (value types only), or null |
| `pfCtorIsPublic` | `BOOL` | `pMD->IsPublic()`, or TRUE when no ctor call is needed |

Three shapes:

* `Nullable<T>` — every out-param null/TRUE; `Activator.CreateInstance` returns null.
* Reference type — allocator + `pfnRefCtor` = the parameterless ctor; **no**
  parameterless ctor is a `MissingMethodException`.
* Value type — allocator; ctor pointers null unless the struct declares an explicit
  parameterless ctor.

`ValidateTypeAbleToBeInstantiated(typeHandle, allowByRefLike: true, fGetUninitializedObject: false)`
runs first. `ActivatorCache` is the only managed caller, and it unconditionally runs
`RuntimeType.CreateInstanceCheckThis` first (ActivatorCache.cs:48), which has *already*
rejected open generics, `void` and `ArgIterator`. So the reachable validations are, **in
CoreCLR's order** — the order is load-bearing, not just the set:

| # | condition | exception |
| --- | --- | --- |
| 1 | `void` | *unreachable* (`CreateInstanceCheckThis` threw `NotSupportedException`) |
| 2 | array, byref, pointer, function pointer (`IsTypeDesc \|\| IsArray`) | `MissingMethodException` |
| 3 | delegate | `ArgumentException` |
| 4 | `HasComponentSize` (string) | `MissingMethodException` |
| 5 | abstract / interface | `MissingMethodException` |
| 6 | `ContainsGenericVariables` | *unreachable* (`CreateInstanceCheckThis` threw `ArgumentException`) |
| 7 | `__Canon` | *unreachable* (PawPrint has no shared generics) |
| — | byref-like | **allowed through**: `allowByRefLike: true` here; managed `CreateInstanceDefaultCtor` throws `NotSupportedException` later, *after* the `CtorIsPublic` check |
| 8 | reference type with no parameterless ctor | `MissingMethodException` |

Delegate (3) is checked *before* abstract (5), so `typeof(Delegate)` and
`typeof(MulticastDelegate)` — which are both — must yield `ArgumentException`. A handler
that tests abstract first is silently wrong.

The parameterless-ctor lookup is **visibility-blind**: `HasDefaultConstructor` /
`GetDefaultConstructor` find a private ctor, and publicness is reported separately via
`pfCtorIsPublic = pMD->IsPublic()`. The `MissingMethodException` for a non-public ctor
under `publicOnly` is thrown managed-side in `CreateInstanceDefaultCtor`, not here.

Note the QCall explicitly does **not** run the type's `.cctor`; it only calls
`EnsureInstanceActive`. `ActivatorCache`'s comment spells out why that is
observable: for a value type, `Activator.CreateInstance` produces a boxed
`default(T)` with no ctor call, so the `.cctor` does not run at all.

`ActivatorCache` then substitutes managed no-op stubs (`ReturnNull`,
`RefCtorNoopStub`, `ValueRefCtorNoopStub` — all real local functions, so `ldftn`
on them gives PawPrint a genuine `MethodInfo`) for whichever pointers came back
null, and invokes all three via `calli`.

## The one hard problem

PawPrint's function-pointer value is `NativeIntSource.FunctionPointer of MethodInfo<…>`,
and `executeCalli` drives the whole call from that `MethodInfo` (it is the single
source of truth for argument popping). **The allocator has no managed `MethodInfo`** —
it is a JIT helper. That is the entire reason this QCall has stayed unimplemented.

The other two pointers are fine as plain `MethodInfo`s:

* ref-type `pfnRefCtor`: instance ctor, 1 slot; call site `delegate*<object, void>`,
  1 slot. Receiver is an `ObjectRef` — PawPrint's normal convention.
* value-type `pfnValueCtor`: instance ctor with byref `this`, 1 slot; call site
  `delegate*<ref byte, void>`, 1 slot. Also PawPrint's normal convention.

The one unrepresentable case is a **value type's boxed entry point** (`pfnRefCtor`
for a struct that declares an explicit parameterless ctor): same `MethodDesc`,
different entry point, and `NativeIntSource.FunctionPointer` carries no entry-point
flavour.

### Options considered for the allocator pointer

**Option 1 — new top-level `NativeIntSource` case.** Add
`| RuntimeAllocatorPtr` alongside `FunctionPointer`, and give `executeCalli` a
second dispatch mode.
*Cost:* ~35 new match arms across `NativeIntSource` consumers (`isZero`, `ceq`
aliasing, `ToBytes` refusals, `ToString`, …), each of which has to independently
re-derive the answer "this is a non-null opaque pointer".

**Option 2 — split the payload of the existing case.** Keep one
`FunctionPointer` case and make its payload a DU:

```fsharp
type FunctionPointerTarget =
    | Managed of MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    /// CoreCLR's `newobj` allocation JIT helper, managed sig `MethodTable* -> object`.
    | RuntimeAllocator
```

*Why this is better:* every site that merely *classifies* a function pointer
(`isZero` → fail; `ceq` → "distinct opaque handle kinds never alias"; refuse-to-
serialise; the `RuntimePointer` coercion that keeps a fnptr a `NativeInt`) is
already correct for both flavours and needs **no** edit. Only the ~8 sites that
actually consume the `MethodInfo` change, and each of those is a place that
genuinely must decide what a non-managed target means (`executeCalli`,
`dispatchDelegateInvoke`, `MethodInfo.NominallyEqual`, hashing, `ToString`).
It also states the truth: this *is* a function pointer, whose target happens not
to be a managed method.

**Option 3 — point the allocator at a real CoreLib method**
(`RuntimeTypeHandle.InternalAllocNoChecks`, sig `MethodTable* -> object`) and
implement the InternalCall + QCall underneath it. This is not absurd: CoreCLR
itself does exactly this in the COM branch, returning
`METHOD__RT_TYPE_HANDLE__ALLOCATECOMOBJECT` — a real managed method — as the
allocator (`reflectioninvocation.cpp:1637`).
*Rejected on identity grounds:* PawPrint compares function pointers nominally, so
a guest that `ldftn`s `InternalAllocNoChecks` and compares would get an equality
CoreCLR never reports. It also puts a stack frame in the guest that CoreCLR would
never show, and drags two more unrelated natives into this PR.

**Option 4 — synthesise a fake `MethodInfo`.**
*Rejected:* `MethodInfo` requires a `MethodDefinitionHandle` and a `DeclaringType`,
and there is no honest value for either. A nil handle would then flow into
`NominallyEqual`, hashing, stack traces and `RuntimeMethodHandle` conversion.

**Chosen: Option 2.**

## Plan

### 1. `NativeIntSource.FunctionPointer` gains a target DU

`NativeIntSource.fs`: introduce `FunctionPointerTarget` (`Managed` | `RuntimeAllocator`)
as above and repoint the case. Fix the consuming sites; the F# incomplete-match
warning (warnings-as-errors) enumerates them for us.

`ceq` semantics: `RuntimeAllocator = RuntimeAllocator` → true; `RuntimeAllocator` vs
`Managed _` → false; either vs a zero → false (the existing "distinct opaque handle
kinds never alias" block already gives this). The nullary shape is *more* faithful
than a handle-carrying one: CoreCLR hands most types the same
`CORINFO_HELP_NEWSFAST` address, so two types' allocators genuinely do compare equal
there, whereas `RuntimeAllocator of ConcreteTypeHandle` would diverge the other way.

A future boxed-entry-point flavour is then a local extension of this DU rather than
another top-level `NativeIntSource` case.

This step is a pure mechanical refactor with no behaviour change; it goes in as its
own commit so it is independently reviewable.

### 2. `executeCalli` learns the allocator

A new arm before the existing `FunctionPointer (Managed mi)` arm, and before the
`isZero` null check (which has no answer for function pointers). Semantics:

* validate the call site against the allocator's *exact* known signature
  (`void* -> object`), not merely "1 slot, returns a value" — the existing
  `Managed` arm does representation checks (`calliKindsConflict`) and this arm
  knows more than it does, so it should check more;
* pop the argument; require a closed `MethodTablePtr`/`TypeHandlePtr`
  (`NativeCall.methodTableOfEvalStackValue` is the existing honest reader — it
  already fails loudly on open generics and generic parameters);
* reject a `Nullable<T>` MethodTable explicitly. `ActivatorCache` guarantees the
  Nullable allocator is `ReturnNull`, and `executeBox` never boxes Nullable-as-
  itself — the unbox reader depends on that invariant, so assert it here rather
  than leaving it to be assumed;
* allocate an uninitialised instance — `allocateManagedObjectOfConcreteType`,
  which for a value type produces exactly the boxed shape `executeBox` writes
  (verified: both route the type's own non-static fields through the same
  `InlineArrayStorage.expand` and the same
  `CliValueType.OfFields … Layout … charset` into the same
  `IlMachineState.allocateManagedObject`, and `collectAllInstanceFields`'
  base-chain walk contributes nothing for a value type);
* **do not** run the `.cctor` — matching CoreCLR, and observable for a struct with
  a static ctor and no instance ctor. Concretely, this arm must never reach
  `loadClass`;
* push the `ObjectRef`, advance the PC. No frame is pushed: this is a synchronous
  runtime primitive, not a managed call, so it also never suspends and the
  peek-don't-pop retry dance the `Managed` arm needs does not apply.

The argument is *not* baked into the pointer value: CoreCLR's helper genuinely
takes the `MethodTable*` as its argument and the guest passes `_allocatorFirstArg`
separately, so reading it from the call site keeps one source of truth.

The subsequent ctor `calli` picks the `.cctor` up correctly and in CoreCLR's order:
`executeCalli`'s `Managed` arm runs `loadClass` on the *target method's declaring
type*, so a real ctor initialises T after allocation, while the ctorless-struct
path targets `RefCtorNoopStub`/`ValueRefCtorNoopStub` (declared on `ActivatorCache`)
and so never touches T at all.

### 3. The QCall handler

New arm in `Native/NativeRuntimeTypeQCall.fs`, registered in `NativeQCall.fs`.

**Argument shapes.** Only argument 0 is an `ObjectHandleOnStack` (read via
`objectHandleOnStackTarget` + `readManagedByref` +
`runtimeTypeHandleTargetOfRuntimeTypeRef`). The other five are *raw* out-pointers
(`delegate*<…>*`, `void**`, `Interop.BOOL*`) and decode via
`managedPointerOfPointerArgument`. All five are written with
`writeManagedByrefWithBase` on every non-throwing path.

**Encodings.** `Interop.BOOL` is int32-backed, so `pfCtorIsPublic` is written as
`CliType.Numeric (Int32 0|1)`, not a `Bool` byte. A null function pointer is written
as `NativeIntSource.Verbatim 0L` and never as a `FunctionPointer` — this is
load-bearing: `ActivatorCache`'s `if (_pfnRefCtor == null)` is a `ceq` against zero,
and a `FunctionPointer` hits the never-alias block and compares false, which would
silently skip the no-op-stub substitution.

Validation → `NativeHandlerResult.raiseException` with the CoreCLR exception type
(`MissingMethodException`, `ArgumentException`, `NotSupportedException` are all
already on `BaseClassTypes`). Message text will differ from CoreCLR's — PawPrint's
raise path only supports parameterless ctors — but `ActivatorCache` rewraps the
message anyway, so the guest-observable *type* is preserved. The three validations
the managed pre-check makes unreachable (`void`, `ContainsGenericVariables`,
`__Canon`) get a loud `failwith` naming the trigger, **not** a silent fall-through:
`System.Void` is an ordinary struct in PawPrint's model, so falling through would
hand back an allocator for boxed void.

Out-values:

* `Nullable<T>` → all five null/TRUE.
* value type with an explicit parameterless ctor → `failwith` at QCall time,
  writing nothing. `pfnRefCtor` would have to be the *boxed* entry point, and
  representing it as a plain `Managed` ctor is exactly the subtly-wrong-answer
  class this project forbids: `executeCalli` coercing an `ObjectRef` receiver into
  a value-type `this` risks constructing into a copy of the box's payload.
* value type without one → allocator; both ctor pointers null; `pfCtorIsPublic`
  TRUE.
* reference type → allocator; `pfnRefCtor` = the parameterless ctor found
  **visibility-blind**, with `pfCtorIsPublic` computed from its `MethodAttributes`;
  no parameterless ctor at all → `MissingMethodException`.

### 4. Tests

**Unit level.**

* A box-shape *property* test: over a corpus of value types (multi-field struct,
  bare primitive, enum, inline array, generic struct instantiation), the allocator
  arm's output must be structurally equal to `executeBox` applied to `default(T)`.
  This mechanically verifies the load-bearing parity claim in step 2 instead of
  resting on two readings of the `OfFields` call sites.
* The handler's classification: check order (delegate-before-abstract), the
  visibility-blind ctor lookup with its separate `pfCtorIsPublic`, the null
  encodings, and that the allocator does not initialise the type.

**Differential (`sourcesPure`, so cross-runtime facts only — classify caught
exceptions by *type* into the exit code and never print messages, since
`ActivatorCache` embeds the diverging inner message).**

* A table-driven battery as the primary test: one guest sweeping
  {class, struct, abstract, interface, delegate, enum, string, array, byref-like}
  × {implicit ctor, explicit public, private, none} × {cctor, none}, encoding each
  outcome (success / caught exception type) into the output. Finite alphabet, so
  it is cheap, and the real runtime is the oracle.
* The private-ctor pair specifically: `Activator.CreateInstance(t)` →
  `MissingMethodException`; `Activator.CreateInstance(t, nonPublic: true)` →
  success. Without this, an inverted or always-true `pfCtorIsPublic` passes
  everything else.
* `typeof(MulticastDelegate)` specifically, to pin the delegate-before-abstract
  order.
* `int?` → null.
* cctor-not-run for a ctorless struct, via a side channel (the struct's `.cctor`
  writes a static on a *different* class; the guest reads it after
  `CreateInstance`). An explicit `.cctor` makes the type non-`beforefieldinit`, so
  "did not run" is deterministic on both runtimes. Mutation-test it: make the
  allocator arm call `loadClass` once and confirm the test fails.
* Second activation of the same type, exercising `ActivatorCache`'s cached
  function pointers being re-fed to `calli` out of instance fields.
* A generic instantiation positive, since ctor concretization under generics is a
  distinct path in the handler's ctor lookup.
* Ctor throws → `TargetInvocationException`.
* A parked (`unimplemented`) guest for the value-type-with-explicit-parameterless-
  ctor case, run once un-parked to confirm it fails at the expected `failwith`
  rather than somewhere else.
* Attempt to un-park `MarshalPtrToStructure.cs`; if it is still blocked further
  down, leave it parked with the comment updated to name the *new* blocker.

### Explicitly out of scope

* The boxed entry point for value-type ctors (fails loudly).
* `FEATURE_COMINTEROP` (`__ComObject` / `AllocateComObject`) — not compiled into
  the CoreLibs we run.
* `RuntimeTypeHandle_InternalAllocNoChecks` and friends.
* Widening `Activator.CreateInstance<T>()`'s existing inline intrinsic; it is a
  separate path and stays as it is. **Endgame note:** once this QCall works, that
  intrinsic is a second implementation of the same semantics — the two-versions-of
  -the-truth situation to be avoided. The intended follow-up is to narrow or delete
  it in favour of the real path, and this should not be allowed to become
  permanent.
