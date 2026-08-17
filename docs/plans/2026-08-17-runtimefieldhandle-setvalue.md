# `RuntimeFieldHandle_SetValue`: the field route of custom-attribute named arguments

## What is blocked, and what is not

`sourcesPure/CustomAttributeNamedArgs.cs` is parked with a note saying it needs the later of
two primitives: `System.RuntimeMethodHandle::GetMethodDef` (the property route) and the
`RuntimeFieldHandle_SetValue` QCall (the field route). `GetMethodDef` landed in #1033, so
the note's first half is now stale.

Measured by un-parking the file on top of current `main`:

```
WoofWare.PawPrint.GuestFailureException : Unimplemented native method
(PInvokeImpl QCall!RuntimeFieldHandle_SetValue): System.Private.CoreLib
System.RuntimeFieldHandle::<SetValue>g____PInvoke|34_0(System.IntPtr,
ObjectHandleOnStack, ObjectHandleOnStack, QCallTypeHandle, QCallTypeHandle,
*(System.Int32)) -> void
```

A probe stub that reads the arguments, writes nothing, and reports
`isClassInitialized = false` runs the guest to completion and it returns **10** — the tenth
`Check`, `withHi.Count == 42`, which is the *first* field-route assertion. Checks 1–9
(attribute count, both applications found, the fixed ctor arg, both `Label` strings and both
boxed `Flag` bools) all pass. So the property route is genuinely working end to end and the
field write is the only missing behaviour.

## The shape of the managed caller, and why it forces a second decision

`FieldInfo.SetValue` is `RtFieldInfo.SetValue` → `FieldAccessor.SetValue`
(libraries/…/Reflection/FieldAccessor.cs). `FieldAccessor` is cached on the `RtFieldInfo`
(`m_fieldAccessor ??= new FieldAccessor(this)`, RtFieldInfo.cs:28) and `RuntimeType` caches
its field list, so both `[Marker(...)]` applications share one accessor.

The accessor starts in `FieldAccessorType.SlowPathUntilClassInitialized`, whose arm is:

```csharp
isClassInitialized = false;
RuntimeFieldHandle.SetValue(_fieldInfo, obj, value, fieldType, declaringType, ref isClassInitialized);
if (isClassInitialized) { Initialize(); }
```

`isClassInitialized` is bidirectional. As an *input* it means "skip the cctor check"; as an
*output* CoreCLR sets it to `pDeclMT->IsClassInited()` (invokeutil.cpp:790). Since
`MarkerAttribute` is initialised by the time we get here, CoreCLR answers `true`, and
`Initialize()` then switches the accessor to an *address-based* fast path:

```csharp
if (!RuntimeFieldHandle.IsFastPathSupported(_fieldInfo)) { _fieldAccessType = SlowPath; return; }
_addressOrOffset = RuntimeFieldHandle.GetInstanceFieldOffset(_fieldInfo);   // or GetStaticFieldAddress
```

after which sets are raw byte pokes:
`Volatile.Write(ref Unsafe.As<byte,int>(ref Unsafe.AddByteOffset(ref obj.GetRawData(), _addressOrOffset)), …)`.

Measured with the probe reporting `isClassInitialized = true`: the guest immediately dies at

```
Unimplemented native method (InternalCall): System.RuntimeFieldHandle::IsFastPathSupported(RtFieldInfo) -> Boolean
```

So implementing the QCall alone is *not* sufficient if we answer the out-param the way
CoreCLR does. That is the real decision this plan has to make.

## Option A (recommended) — one primitive; PawPrint declines the address-based fast path

Implement `RuntimeFieldHandle_SetValue` properly, report `isClassInitialized` truthfully from
the `TypeInitTable`, and implement `RuntimeFieldHandle.IsFastPathSupported` as `false`.

The accessor then goes `SlowPathUntilClassInitialized` → (QCall, we answer `true`) →
`Initialize()` → `IsFastPathSupported = false` → `SlowPath`, permanently. Every later set
falls through to the bottom of `SetValue`, which calls the same QCall with
`isClassInitialized = true`. One primitive services every field set in the process.

Why answering `false` is honest rather than expedient: `IsFastPathSupportedHelper`
(reflectioninvocation.cpp:1049) is
`!IsThreadStatic() && !IsEnCNew() && !(IsCollectible() && IsStatic())`, and those three
disjuncts are exactly CoreCLR's three cases where it cannot hand out a *stable raw
offset or address* for the field — per-thread storage, side-table storage, movable/collectible
base. Its callers use it for nothing else: `GetInstanceFieldOffset` and
`GetStaticFieldAddress` both `_ASSERTE(IsFastPathSupportedHelper(pFieldDesc))`. PawPrint
cannot hand out a raw offset or address for *any* field, because a `ManagedHeap` object is a
map from `FieldId` to `CliType` rather than a byte image. So `false` is the correct answer to
the question the predicate asks, and the comment should say which condition would let us
answer `true`.

Answering `false` also costs far less divergence than it first looks, because CoreCLR's own
fast-path `SetValue` switch is partial. It has arms only for `InstanceReferenceType`,
`InstanceValueTypeSize{1,2,4,8}`, `StaticReferenceType` and `StaticValueTypeSize{1,2,4,8}`
(FieldAccessor.cs:219–290). `InstanceValueType` (a non-primitive struct),
`InstancePointerType`, `StaticValueType`, `StaticValueTypeBoxed` and `StaticPointerType` have
no arm at all and fall out of the switch to the bottom slow-path QCall
(FieldAccessor.cs:319–331) *even on real .NET with the fast path fully supported*. So relative
to CoreCLR, Option A reroutes only primitive-, enum- and reference-typed field sets — exactly
the shapes step 3 implements. The handler needs the same coverage whichever answer we give.

Blast radius is one call site: `IsFastPathSupported` has exactly one managed consumer in the
whole framework, `FieldAccessor.Initialize` (FieldAccessor.cs:36). Reversibility is good too:
if PawPrint later byte-addresses heap objects, flip one boolean and add the two offset FCalls;
nothing else in this change has to move.

One knock-on worth stating rather than discovering later. `FieldAccessor.GetValue` has an
explicit `case FieldAccessorType.SlowPath` arm (FieldAccessor.cs:186) which likewise calls
`RuntimeFieldHandle.GetValue` with `isClassInitialized = true`, so under Option A
`FieldInfo.GetValue` can never take an address-based path either, and implementing the
`RuntimeFieldHandle_GetValue` QCall becomes the only route to it. That is not a regression:
`GetValue` is fully blocked today, because the fast path it would otherwise take needs the
same unimplemented `IsFastPathSupported`. Option A only changes *which* primitive its failure
names.

### Option B (rejected) — put the lie in the out-param instead

Report `isClassInitialized = false` always, so `Initialize()` is never called and
`IsFastPathSupported` is never reached. Smaller — one primitive, no second handler — and the
probe confirms it makes the test pass.

Rejected because the flag is a statement about a *type*, not about our capabilities: we would
be claiming `MarkerAttribute`'s initialiser has not run when it has. It is also silent —
nothing ever crashes, so if PawPrint later gains byte addressing no test goes red to tell us
managed code is being steered. And it pins the accessor in
`SlowPathUntilClassInitialized` forever, which is the one state in which `VerifyInitOnly`
deliberately *skips* the managed init-only check (FieldAccessor.cs:386) and delegates it to
the QCall — so the lie silently relocates a guest-visible exception for every field in the
program.

### Option C (later, separate) — implement the address-based fast path for real

`IsFastPathSupported = true`, plus `GetInstanceFieldOffset`, `GetStaticFieldAddress`, plus
`obj.GetRawData()` byte views over reference-containing heap objects. Faithful to CoreCLR and
would also serve `GetValue`.

Not this change: byte-addressing a reference-containing object is the capability already
parked behind `RuntimeHelpersBoxReferenceContainingStruct.cs`,
`ReinterpretCellUnderAliasedAncestor.cs` and `BulkMoveAcrossOverlappedStructPadding.cs`, and
it is a change to the heap model rather than to this QCall. `MarkerAttribute` has `string`
fields, so this test's own object is squarely in the parked shape. Option A's one boolean is
where Option C would land when it happens.

## Work in Option A

`Native/NativeRuntimeFieldHandle.fs`, registered in `NativeQCall.fs` (QCall) and already
dispatched for FCalls via `NativeDispatch`.

1. **Resolve the field.** Argument 0 is a `FieldDesc*` as an `IntPtr`;
   `NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal` already accepts both spellings
   (`FieldHandlePtr` / `FieldRegistryHandle`). Then
   `FieldHandleRegistry.resolveFieldFromId` + `FieldRvaData.fieldForHandle`, as the sibling
   `GetAttributes` arm does.
2. **Class initialisation.** If the incoming flag is `false`, call
   `IlMachineStateExecution.ensureTypeInitialised` and honour `SuspendedForClassInit` by
   returning `NativeHandlerResult.suspendedForClassInit` — the re-entrant pattern
   `ReflectionInvocation_RunClassConstructor` (NativeRuntimeHelpers.fs:57) already uses. Handle
   `BlockedOnClassInit` too, as that precedent does, so the arm set is exhaustive.

   The out-param is written **only when the incoming flag was false**, mirroring CoreCLR, whose
   whole write sits inside its `if (*pIsClassInitialized == FALSE)` block. Recomputing it
   unconditionally answers "not initialised" for a type the caller vouched for but which we
   never put in the `TypeInitTable` — a worse answer than the one it supplied. (The unit test
   below is what caught this; no guest can.)

   The value written must key on `TypeInitState.Initialized` **specifically**: CoreCLR writes
   `pDeclMT->IsClassInited()` (invokeutil.cpp:791), which is *false* while the cctor is still
   running, and step 4's gate reads the same predicate (invokeutil.cpp:811). That is what makes
   "reflectively set a static readonly from inside its own declaring type's cctor" legal on real
   .NET. PawPrint has the distinction — `TypeInitState.InProgress` vs `Initialized`
   (TypeInitialisation.fs:6–11), and `ensureTypeInitialised` returns `Executed` for same-thread
   `InProgress` (IlMachineStateExecution.fs:2395–2398) — so `InProgress` maps to
   "not initialised": out-param `false`, and step 4's gate does not fire.

   **The failing-cctor divergence.** `ReflectionInvocation_RunClassConstructor` is the wrong
   template on exactly this point. CoreCLR's `SetValidField` catches the cctor's failure
   (`EX_CATCH_THROWABLE`, invokeutil.cpp:786–794) and throws a *fresh*
   `TargetInvocationException` wrapping the `TypeInitializationException`
   (`CreateTargetExcept`, invokeutil.cpp:803); `ReflectionInvocation_RunClassConstructor`
   deliberately does not wrap (reflectioninvocation.cpp:1226–1231 lets
   `CheckRunClassInitThrowing` throw through). Copying the precedent's
   `ThrowingTypeInitializationException` arm verbatim would therefore diverge silently.

   Only half of this is interceptable. A declaring type already in `TypeInitState.Failed` can be
   caught by reading `TypeInitTable` *before* calling `ensureTypeInitialised` — that helper
   dispatches the cached TIE itself (IlMachineStateExecution.fs:2385–2391), so once it has
   returned the exception is already in flight and cannot be wrapped after the fact. So: refuse
   loudly on a pre-observed `Failed`. A cctor that fails *during* this call cannot be
   intercepted at all — we return `suspendedForClassInit`, the cctor frame throws, and the TIE
   propagates through the native frame without ever re-entering the handler. Measured, not
   assumed: `sourcesPure/ReflectionFieldSetValueFailingCctor.cs` exits 0 on real .NET and
   reports "threw unhandled exception" under PawPrint, because the bare
   `TypeInitializationException` does not match the guest's `catch (TargetInvocationException)`.
   It is parked, so the fixture's real-.NET side keeps asserting the answer, and recorded in
   `docs/divergences.md`. Wrapping it properly needs a native frame able to intercept an
   exception propagating through it, which is its own change.
3. **Write the value.** `instance` and `value` arrive as `ObjectHandleOnStack`;
   `NativeCall.objectHandleOnStackTarget` gives the byref, and `IlMachineState.readManagedByref`
   — the object-aware reader, not the byte-view one — gives the `ObjectRef`. Then, mirroring
   `InvokeUtil::SetValidField`'s split on the field's element type:
   - reference-typed field: store the `ObjectRef` as-is, including `null` (which does reach us:
     `FieldAccessor.CheckValue` leaves a null alone for a non-value-type field,
     FieldAccessor.cs:367–371);
   - value-type/primitive/enum field: unbox the payload and store it.

   Instance fields go through `ManagedHeap.setFieldById` (with
   `RuntimeFieldProjection.tryProjectFieldStore` first, as `executeStfld` does); static fields
   through `IlMachineState.setStatic`, whose `StaticOwner` derivation already routes
   `[ThreadStatic]` to the current thread's slot (StaticOwner.fs:33). Thread statics are worth a
   test precisely because they are the one field kind for which CoreCLR *also* answers
   `IsFastPathSupported = false`, so the two runtimes take the same managed path.

   Note **no** null-into-a-value-type-field arm. CoreCLR has one (`InitValueClass`,
   invokeutil.cpp:955), but it is unreachable from this QCall's only caller: `CheckValue` has
   already replaced a null destined for a value-type field with a default box
   (`AllocateValueType(this, value: null)`, RuntimeType.cs:1013). An arm nothing can provoke
   should not ship, so this becomes part of the refusal set below.

   A **boxed-struct target** is reachable and should work rather than be refused:
   `FieldInfo.SetValue(boxedStruct, v)` mutates the box (CoreCLR writes through
   `GetInstanceAddress`, invokeutil.cpp:952), and PawPrint's `executeBox` stores a genuine
   multi-field value type as its own fields, keyed by the same `FieldId`s, so
   `setFieldById` against the box addresses the right cells. It needs a test, not a guard.
4. **Static init-only.** CoreCLR throws `FieldAccessException` for a static `initonly` field
   once the class is inited, and managed `VerifyInitOnly` delegates to it on this path, so the
   check is load-bearing rather than decorative.
5. **`IsFastPathSupported`** as an FCall arm returning `false`.

### Refusals (loud, naming the field and both types)

- pointer / function-pointer fields: CoreCLR's `ELEMENT_TYPE_PTR`/`FNPTR` arms extract a raw
  pointer out of a box, which PawPrint's provenance model cannot synthesise — the same gap
  `PointerFieldAliasedWidthStore.cs` documents.
- a boxed value whose type the unbox relaxation does not accept. Note the relaxation itself is
  **load-bearing here, not belt-and-braces**: managed `CheckValue` does *not* always normalise
  the value to exactly the field type. `TryChangeTypeSpecial` converts only when the source and
  destination `CorElementType`s differ (RuntimeType.CoreCLR.cs:3787–3792), so a boxed `int`
  written into a `MyEnum : int` field, a boxed enum written into an `int` field, and a boxed
  same-underlying *other* enum all arrive unconverted. That is exactly the enum↔underlying
  clause `IlMachineState.unboxPermitted` already models for the `unbox` opcode, which is why
  reusing it rather than re-deriving it matters.
- `null` for a value-type field — unreachable from `FieldAccessor` (see step 3), so refuse
  rather than mirror `InitValueClass`.
- a declaring type that is `Nullable<T>` or an open generic definition. `FieldAccessor`'s ctor
  routes both to `NoInvoke` (FieldAccessor.cs:22–27), which throws before the QCall is reached,
  so this arm is genuinely unreachable from the only caller and `failwith` — not a guest
  `NotSupportedException` — is the honest spelling.

### Reuse question: where does "what a box holds" live?

Unboxing must not be re-derived here. `UnaryMetadataObjectOps` has exactly the right three
functions — `barePrimitiveBoxShape`, `unboxedContents`, `unboxTypeTest` (which carries the
enum↔underlying relaxation via `IlMachineState.unboxPermitted`) — but they are `let private`,
so `Native/` cannot see them even though it compiles later.

Decision: extract them into a small module compiled before `UnaryMetadataObjectOps.fs`, so
`unbox`, `unbox.any` and this QCall share one definition. A second copy is the drift that the
duplicated byref walk in `ByteOffset` already cost us once, and the enum↔underlying relaxation
is precisely the clause a re-derivation would get wrong.

## Tests

**Witness mechanism, for every guest here: direct field reads only.** `FieldInfo.GetValue` is
unreachable under Option A (it needs the unimplemented `RuntimeFieldHandle_GetValue`), so a
guest that reads its writes back through reflection fails for the wrong reason.

- Un-park `CustomAttributeNamedArgs.cs`. It covers an instance value-type field written twice,
  so the second call arrives with `isClassInitialized = true` and exercises the *input*
  direction of the flag, plus the parameterless-ctor attribute's second blob-cursor
  provenance. That "twice" claim rests on both applications sharing one `RtFieldInfo` — true in
  CoreCLR (`m_fieldAccessor ??=`, RtFieldInfo.cs:28, behind `RuntimeType`'s member cache) but a
  claim about PawPrint's type-identity invariants, so confirm it with a temporary `failwith` in
  the true-input arm rather than asserting it.
- A new pure guest driving `FieldInfo.SetValue` directly: instance value-type field, instance
  reference-typed field, `null` into a reference field, an **enum-typed field set from a boxed
  underlying value** (the unconverted shape above), a **boxed-struct target**, a `[ThreadStatic]`
  field, and a plain **static** field — the last being the only route to `setStatic` and, if the
  holder's cctor has not yet run, to the suspension path in step 2. That guest wants a witness
  distinguishing "the cctor ran *before* the set" from "after", not merely "the cctor ran".
- Step 4 is called load-bearing, so it needs tests, and specifically a *pair*: setting a static
  `readonly` field after its class is initialised must throw `FieldAccessException`, and
  setting one *from inside its own declaring type's cctor* must **succeed**. That pair is what
  pins `Initialized` against `InProgress`; either alone passes with the distinction collapsed.
- A parked guest for the failing-cctor case, whose real-.NET side records that
  `TargetInvocationException` is what should be thrown, against the bare
  `TypeInitializationException` step 2 documents.
- Unit tests driving the handler for shapes no guest reaches, and one per refusal arm.
- Mutation-test each arm, separately. Two notes on what is and is not killable:
  - The `isClassInitialized` write-back has **no guest-observable consequence** under Option A.
    Always answering `false` leaves the accessor in `SlowPathUntilClassInitialized` forever;
    the only differences are re-running an idempotent init check and throwing the init-only
    `FieldAccessException` from our gate rather than from managed `VerifyInitOnly` — the same
    exception type. So that mutation is killable only by a unit test asserting the out-cell's
    value directly, which is therefore required rather than optional.
  - The two mutations are coupled one way: "`IsFastPathSupported` returns `true`" is killed
    loudly by the un-parked guest (it reaches the unimplemented `GetInstanceFieldOffset`) *only
    if* the write-back is working. Run them one at a time, not together.
