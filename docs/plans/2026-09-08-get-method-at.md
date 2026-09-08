# `RuntimeTypeHandle_GetMethodAt`: the method occupying a MethodTable slot

Status: implemented on this branch; the recommendations under "Decisions" were confirmed, with one refinement to decision 2 recorded under "Outcome". Stacks on `module-gettoken`
(#1403), which stacks on `composite-target` (#1394).

## What is measured

Rung J of the ASP.NET ladder (`docs/plans/2026-08-17-aspnet-critical-path.md`), with #1394 and
#1403 in, stops 42 frames out from `Main` at

```
Unimplemented native method (PInvokeImpl QCall!RuntimeTypeHandle_GetMethodAt):
  System.RuntimeTypeHandle::GetMethodAt(MethodTable*, int) -> IntPtr
```

A probe printing the whole guest frame chain (thrown away) names the caller:

```
RuntimeTypeHandle.GetMethodAt
Associates.AssignAssociates
RuntimePropertyInfo..ctor
MemberInfoCache`1.PopulateProperties
RuntimeType.GetProperties
RuntimeCustomAttributeData..ctor
RuntimeCustomAttributeData.GetCustomAttributes
RuntimeModule.GetCustomAttributesData
NullabilityInfoContext.IsPublicOnly
NullabilityInfoContext.Create
Microsoft.AspNetCore.Http.Extensions.RequestDelegateFactory.IsOptionalParameter
```

`RequestDelegateFactory` asks `NullabilityInfoContext` whether a handler parameter is optional;
that reads the parameter's custom attribute data, which populates the attribute type's
*properties*, and `Associates.AssignAssociates` (Associates.cs:87-98) resolves each virtual
accessor to the override visible from the reflected type:

```csharp
int slot = RuntimeMethodHandle.GetSlot(associateMethodHandle);
associateMethodHandle = RuntimeTypeHandle.GetMethodAt(reflectedType, slot);
```

So the shape rung J needs is a **class** MethodTable and a slot below `GetNumVirtuals`. It is not
the interface-map path: `Type.GetInterfaceMap` (RuntimeType.CoreCLR.cs:2739) also loops over
`GetMethodAt`, but it first calls `RuntimeTypeHandle_GetNumVirtualsAndStaticVirtuals`, which
PawPrint has not implemented either, so that path stops one QCall earlier and is a later PR.

CoreLib's three callers, all of which first obtain a slot from `RuntimeMethodHandle.GetSlot`
(implemented) or iterate up to a virtual count:

| caller | receiver | slot source |
| --- | --- | --- |
| `Associates.AssignAssociates` (Associates.cs:98) | the reflected class | `GetSlot` of the accessor |
| `RuntimeMethodInfo.GetParentDefinition` / `GetBaseDefinition` (RuntimeMethodInfo.CoreCLR.cs:121, 352) | each base class in turn, guarded by `GetNumVirtuals` | `GetSlot` of the method |
| `RuntimeType.GetInterfaceMap` (RuntimeType.CoreCLR.cs:2771) | the interface | `0 .. GetNumVirtualsAndStaticVirtuals - 1` |

## What CoreCLR does

`RuntimeTypeHandle_GetMethodAt` (runtimehandles.cpp:399-455):

1. `slot < pMT->GetNumVirtuals()`: `pMT->GetMethodDescForSlot(slot)`. That reads the *code* in the
   slot and maps it back to its `MethodDesc` (methodtable.inl:399-421): it is the slot's current
   **content**, so a slot a MethodImpl retargeted to a differently-named body answers that body,
   not the declaration that owns the slot. For an interface it is the interface's own method
   (interfaces have no parent to inherit from, so their vtable is their own instance virtuals in
   declaration order; measured on the host, a closed `IList<int>` has 5 virtuals, its own five
   members and none of `ICollection<T>`'s).
2. Otherwise, only for an interface with virtual statics: a linear scan of the type's methods
   counting `IsVirtual && IsStatic`, so slot `numVirtuals + k` is the k-th static virtual in
   MethodDef order. Anything else throws `ArgumentException` (`Arg_ArgumentOutOfRangeException`).
3. An async-variant MethodDesc answers null. PawPrint has no async variants, so this clause has
   no arm.

The managed wrapper (RuntimeHandles.cs:463-476) throws `ArgumentException` for a TypeDesc and a
negative slot before the QCall, so the QCall sees a MethodTable-shaped target and `slot >= 0`.

## What PawPrint already has

- `VirtualSlotLayout.dispatchTableOfClosed`, memoised in `IlMachineState._VirtualSlotTables` and
  keyed on the definition: `Occupants : ImmutableArray<VtableSlot>` is what each slot *holds*,
  MethodImpl-aware, exactly `GetMethodDescForSlot`'s question. `contentVtableOfDefinition` is the
  same for an `OpenGenericTypeDefinition` receiver. Both are what `callvirt` already reads
  (IlMachineStateExecution.fs:781), so the QCall's answer and dispatch cannot disagree.
- `VtableSlot.DeclaredBy : SlotOwner` names the occupant's declaring type by identity plus a
  substitution rebased into the receiver's vocabulary, and `callvirt` turns that into the closed
  declaring handle by finding the identity on the receiver's concrete base chain
  (IlMachineStateExecution.fs:815-830).
- `RuntimeMethodHandle.GetSlot` (NativeRuntimeMethodHandle.fs:1414) is the inverse, over the
  *identity* table (`slotTableOfClosed` / `slotTableOfDefinition`), which is CoreCLR's
  `MethodDesc::GetSlot`. The two are not each other's inverse under a MethodImpl, and that
  asymmetry is CoreCLR's own.
- `VirtualSlotLayout.slotsBeyondVtableOfDefinition` places static virtuals past the vtable in
  MethodDef order, so the static-virtual tail is the beyond-vtable region filtered to
  `IsVirtual && IsStatic`.
- `MethodHandleRegistry.getOrAllocateInternalHandle` mints a handle from (assembly, declaring
  `RuntimeTypeHandleTarget`, `MethodInfo`), admitting only `Closed` and
  `OpenGenericTypeDefinition` declaring types. `NativeStackTrace.fs:242` shows the encoding for a
  `MethodDesc*` handed back as a raw `IntPtr` (`NativeIntSource.MethodHandlePtr id`), which
  `NativeCall.methodHandleIdOfRuntimeMethodHandleInternal` reads back.
- `RuntimeTypeHandle_GetFields` (NativeRuntimeTypeQCall.fs) decodes a raw `MethodTable*`
  argument through `NativeCall.runtimeTypeHandleTargetOfEvalStackValue`.

## Decisions

### 1. Which table answers a class slot

- **(A) Content** (`dispatchTableOfClosed.Occupants` / `contentVtableOfDefinition`). Matches
  `GetMethodDescForSlot`, is memoised, and is the table `callvirt` reads. **Recommended.**
- **(B) Identity** (`vtableOfClosed` / `vtableOfDefinition`), the declaration owning slot `i`.
  Cheaper to reach from `GetSlot`'s code, and wrong: `.override` of a differently-named body
  makes CoreCLR answer the body. Rejected; recorded because it is the tempting one.

### 2. What declaring type the minted handle carries

The occupant's `DeclaredBy.Identity`, instantiated as it appears on the receiver's chain, the
way `callvirt` does it. For a `Closed` receiver that is always a closed handle. For an
`OpenGenericTypeDefinition` receiver `D<T>` whose occupant is declared by a generic base
`B<T>`, the declaring type is the open construction `B<!0 of D>`, which the registry refuses to
mint a handle for. Two options:

- **(i) Refuse that one shape loudly**, naming it, and answer every other: occupant declared by
  the definition itself (`OpenGenericTypeDefinition D`), or by a non-generic base (`Closed`).
  **Recommended**: it is the same registry limit `MakeGenericMethodOpenArgument.cs` is parked
  on, and widening `MethodHandle`'s declaring type is its own change.
- **(ii) Widen the registry to `OpenConstructed` declaring types** in this PR. Reaches every
  `MethodHandle` consumer; not one feature.

### 3. Scope of this PR

- **(a) The whole QCall**: class and interface receivers, slot below `numVirtuals` from the
  content table, the static-virtual tail from the beyond-vtable region, `ArgumentException`
  (a raised guest exception, per the `raise-guest-exception` skill) past the end.
  **Recommended**: it is one QCall with one contract, and the interface half is the same walk.
- **(b) Class receivers only**, refusing interfaces with a TODO naming `GetInterfaceMap`.
  Smaller, but leaves a half-answered QCall whose other half is the same table.

`GetInterfaceMap`'s other two QCalls (`GetNumVirtualsAndStaticVirtuals`, a count over the same
tables; `GetInterfaceMethodImplementation`, which is `FindDispatchSlotForInterfaceMD`, i.e. the
interpreter's `tryResolveVirtualImplementation`) are separate PRs either way.

## Tests

Written before the implementation, and observed failing on the unimplemented native.

1. **Host-CLR oracle over corelib** (the shape of `TestVirtualMethodSlots.fs`): reflect onto the
   host's internal `RuntimeTypeHandle.GetMethodAt`, `GetNumVirtuals` and
   `RuntimeType.GetMethodBase` (all three reachable, measured with `dotnet fsi`), and for every
   type in the layout corpus and every slot below `numVirtuals` compare PawPrint's occupant
   (declaring type identity, MethodDef token) with the host's. Nothing about PawPrint's walk
   feeds the expected value. Interfaces are in the corpus, so the interface half is covered the
   same way; the static-virtual tail via `INumberBase<T>`, which is already in the corpus for
   its 41 static members.
2. **Differential guests** (`sourcesPure`), each exiting with the index of the first failing
   check:
   - `MethodInfo.GetBaseDefinition` and the accessor-association route: a virtual property on a
     base, overridden in a derived class, read through `typeof(Derived).GetProperty(...)`; the
     accessor's `DeclaringType` must be `Derived`. Same through a generic base `Base<T>`
     instantiated at `int`, and through a base in corelib.
   - a `.override` retarget to a differently-named body, which only IL can spell, so a
     fabricated-image test beside `TestFabricatedVtableLayout` rather than a C# guest:
     `GetBaseDefinition` of the retargeted slot must answer the body, pinning decision 1.
   - `GetBaseDefinition` on a method of an open definition (`typeof(D<>)`), for decision 2's
     answered shapes, and a parked case for the refused one.
3. **Mutation**: content table swapped for identity (dies on the MethodImpl fabricated test);
   declaring type minted as the receiver rather than `DeclaredBy` (dies on the derived
   accessor guest); static-virtual tail read from the vtable instead (dies on the corpus test's
   `INumberBase<T>` rows); out-of-range answered null instead of throwing (needs a guest that
   catches `ArgumentException`, which no public API reaches without `GetInterfaceMap`; record
   as a survivor if so).

## Ladder

After this, rung J's next stop is whatever `NullabilityInfoContext` reaches next, or
`GetNumVirtualsAndStaticVirtuals` if `GetInterfaceMap` is on the path; measure rather than
predict.

## Outcome

Implemented as recommended. Decision 2 needed one refinement, found by Codex's first review: the
first cut refused any *generic* ancestor of a definition receiver, which also refused the closed
ancestor of `D<T> : B<int>`. The content table names an inherited occupant's declarer in the
ancestor's own vocabulary, so the arm now takes the ancestor's arguments from the identity table
(which `placedSlotsOfDefinition` rebases into the definition's vocabulary), closes each one, and
concretises the ancestor; only an argument that is one of the definition's own formals is refused.

Codex's second round found two more shapes, both measured before being fixed: an array over a
type variable as the receiver (an array MethodTable; a guest reaches it, stopping one call
earlier at `GetNumVirtuals`, so that composite arm and `methodAt` now delegate to `System.Array`
and the guest pins it), and an ancestor argument spelled under a context carrying an unused
formal (`TwoStep<T> : Mid<int, T> : Base<int>`; a guest cannot reach it, because the definition's
`BaseType` stops first, so a Roslyn-compiled corpus under the host oracle pins it instead). Only
the context entries a spelling mentions are closed now. The declaring-type derivation moved out
of the QCall into `VirtualSlotLayout.declaringTypeAt` so the fixture could ask it directly.

Measured after the change: rung J runs the request and stops 15 frames out in
`RuntimeMethodHandle.InvokeMethod` on `Int32.TryParse`, whose `out` parameter needs reflection-
invoke byref copy-back. The out-of-range mutant the plan expected to survive was killed instead,
by the corpus test's one-past-the-end check.

