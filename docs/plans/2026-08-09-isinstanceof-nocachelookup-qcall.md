# `IsInstanceOf_NoCacheLookup` QCall

Status: implemented. The plan below is as written before implementation, except for this
section, which records what actually happened.

## What implementation found that the plan did not predict

**A wrong `ceq` classifier on the path *into* the QCall.** `CastHelpers.IsInstanceOfAny` opens
with `RuntimeHelpers.GetMethodTable(obj) != toTypeHnd`, comparing a `MethodTablePtr` against a
raw `void*` TypeHandle. `NativeIntSource`'s comparison already answered `false` for a
`Closed Byref`/`Pointer`/`FunctionPointer` TypeHandle — correctly, since those are tagged
TypeDesc pointers that can never equal a MethodTable address — but it `failwith`ed for a
*generic-parameter* target, on the stated grounds that the combination "should not arise from
any legitimate construction". It does: `typeof(List<>).GetGenericArguments()[0]` is an ordinary
`Type`, and passing it to `Type.IsInstanceOfType` reaches exactly that comparison. CoreCLR just
compares the two pointers and finds them unequal. Fixed by moving the generic-parameter target
into the same `false` arm as the other TypeDescs; the *MethodTablePtr* side keeps its
`failwith`, since there is genuinely no MethodTable whose address that could be. Without this
the TypeDesc branch of the QCall is unreachable from any guest.

**The `BOOL`s are plain `int32`.** Not `Interop.BOOL`, unlike the sibling
`TypeHandle_CanCastTo_NoCacheLookup`. Confirmed by dumping the CoreLib actually executed
rather than reading the C# declaration.

**`Array.SetValue` into an `int?[]` is still blocked**, one step past this QCall: the cast
question is answered (the Nullable branch says a boxed `int` is an `int?`), and managed code
then completes the store via `CastHelpers.Unbox_Nullable`, which needs a
`MethodTable::NullableValueAddrOffset` projection PawPrint does not have. The Nullable branch
is covered instead from `Type.IsInstanceOfType`, which needs no unboxing.
`RuntimeTypeHandle_MakeByRef` / `MakePointer` are likewise unimplemented, so the TypeDesc
branch is covered via a generic parameter rather than `MakeByRefType()`.

**No test can reach the `IDynamicInterfaceCastable` refusal**, because no guest in the suite
implements that interface. It is a refusal path, not an answer path, so this is expected.

**`TypeHandle::GetName` is a different renderer from `TypeString::AppendType`** — found by
Codex review, which demonstrated it differentially. The exception message was first built with
`runtimeTypeHandleName` (PawPrint's model of the reflection `ConstructName` path), and that
disagrees for *nested* types: `TypeHandle::GetName` delegates to
`MethodTable::_GetFullyQualifiedNameForClass` (`vm/class.cpp:2270`), which reads the TypeDef
row's own namespace and name and does not walk the nesting chain, so `Enclosing.Inner` renders
as bare `Inner`. (CoreCLR does have a nesting-aware sibling,
`_GetFullyQualifiedNameForClassNestedAware`, producing `Enclosing+Inner`; `GetName` does not
call it.) PawPrint's `TypeInfo.fullName` is nesting-aware and answered `Enclosing.Inner`.
Fixed by adding `NativeRuntimeTypeHelpers.typeHandleGetName`, modelling `TypeHandle::GetName`
directly and delegating only the *generic arguments* to the reflection renderer, which is what
`TypeString::AppendInst`'s default `FormatNamespace` does. `ArrayCopyCastCheckThrows.cs` gained
nested and namespaced cases; both are differentially checked, and reverting the renderer kills
the test. This also fixes the choice of diagnostic: two nested types sharing a simple name now
correctly reach the `IDS_EE_CANNOTCASTSAME` branch (which PawPrint refuses loudly) rather than
silently taking the ordinary message.

## What was blocked before this change

Any guest reaching `Type.IsInstanceOfType`, `Array.SetValue` on a reference-typed array, or the
casting arm of `Array.Copy` died with:

```
Unimplemented native method (PInvokeImpl QCall!IsInstanceOf_NoCacheLookup):
  System.Private.CoreLib System.Runtime.CompilerServices.CastHelpers::<IsInstanceOf_NoCacheLookup>g____PInvoke|4_0(
    *(System.Void), System.Int32, System.Runtime.CompilerServices.ObjectHandleOnStack) -> System.Int32
```

Measured, not assumed: a guest of `typeof(IFoo).IsInstanceOfType(new Foo())` reaches this QCall
and nothing earlier gets in the way.

PawPrint interprets `isinst`/`castclass`/`stelem.ref` natively, so the JIT-helper route into
`CastHelpers` never fires; the QCall is reached only because those three BCL methods call
`CastHelpers` from managed source. PawPrint's cast cache is a deliberate forever-empty sentinel
(`IlMachineRuntimeMetadata.internCastCacheSentinelTable`,
`docs/plans/2026-05-11-castcache-table-init.md`), so `CastCache.TryGet` always reports
`MaybeCast` and every such call falls through to the QCall unless the
exact-MethodTable-identity check short-circuits first.

## What CoreCLR does

`jithelpers.cpp:465` unwraps the `ObjectHandleOnStack` and delegates to `ObjIsInstanceOfCore`
(`jithelpers.cpp:385`). The branch order is load-bearing, not merely a fast path:

| # | condition | result |
| --- | --- | --- |
| 1 | `Nullable::IsNullableForType(toTypeHnd, pMT)` | `true` |
| 2 | `toTypeHnd.IsTypeDesc()` | `false` |
| 3 | `pMT->CanCastTo(toTypeHnd.AsMethodTable())` | `true` |
| 4 | target is an interface, and object is COM or `IDynamicInterfaceCastable` | dynamic |
| 5 | `!fCast && throwCastException` | throws `InvalidCastException` |

Step 1 is checked first and *never cached*, because object castability and type castability
disagree on `T -> Nullable<T>`: the two share a boxed representation, so a boxed `T` is a
`Nullable<T>` for this question but is not assignable to it structurally.

Step 2 matters for more than the answer: PawPrint's cast oracle
(`isRuntimeTypeHandleTargetAssignableTo`) refuses generic-parameter targets outright, but
CoreCLR never asks it about one, so taking the TypeDesc branch first is what makes this handler
total.

Step 5's message is `IDS_EE_CANNOTCAST` (`mscorrc.rc:406`), `Unable to cast object of type '%1'
to type '%2'.`, with both names from `TypeHandle::GetName`. When the two formatted names come
out equal, CoreCLR instead throws the `IDS_EE_CANNOTCASTSAME` form naming each type's assembly
and load context; PawPrint refuses that case loudly rather than emit the wrong message.

The `ObjectHandleOnStack` is read-only on this path: the `&obj` CoreCLR hands to
`DynamicInterfaceCastable::IsInstanceOf` is its own GC-protected local, not the caller's slot.
`pObject` is a hard non-null precondition, so a null behind the handle is a broken BCL contract
rather than a cast that should answer `false`.

## Design decisions

**A new `Native/NativeCastHelpers.fs`** rather than another arm on the already-1900-line
`NativeRuntimeTypeQCall.fs`: the declaring type is `CastHelpers`, and the per-BCL-area module
split is the established convention.

**Reproduce the branch order rather than delegate.** The sibling `TypeHandle_CanCastTo_NoCacheLookup`
hands straight to the cast oracle, which is right for *type* castability. Object castability
needs the Nullable and TypeDesc rules in front of it, and the second of those is what keeps the
handler total.

**`NativeHandlerResult.RaiseException` gained a `message` field**, wired to the pre-existing
`IlMachineStateExecution.raiseRuntimeExceptionWithMessage`. Without it a native handler could
only raise through a parameterless ctor, and the guest would see "Specified cast is not valid."
where CoreCLR formats a message naming both types. Blast radius is bounded: `raiseException`
remains as the `None`-supplying wrapper, so no existing handler changes behaviour. Note this
makes the QCall *more* faithful than PawPrint's own `castclass` opcode, which still raises a
message-less `InvalidCastException`; that inconsistency is pre-existing.

**`IDynamicInterfaceCastable` is detected and refused, not ignored.** Answering it requires
calling back into the guest's `IsInterfaceImplemented`, which PawPrint does not model, and a
silent `false` may be wrong. `BaseClassTypes` gained the interface so it can be identified
nominally. PawPrint's own `isinst`/`castclass` opcodes ignore the feature too — closing that
needs a managed callback out of the general cast path, so it is left as its own change.

**The Nullable predicate is now shared.** `NativeRuntimeTypeFCall`'s reflection cast path had
this rule inline as `nullableTargetMatchesSource`; it is the same predicate, so both now call
`IlMachineRuntimeMetadata.isNullableForType`, named after CoreCLR's `Nullable::IsNullableForType`.

## Tests

All four are `sourcesPure` cases, so each is differentially compared against real .NET.

| File | Covers |
| --- | --- |
| `IsInstanceOfTypeQCall.cs` | the answer matrix: interfaces, base classes, arrays, SZ-array implicit interfaces, generic variance, the Nullable branch, and an exact-identity control that short-circuits before the QCall |
| `IsInstanceOfTypeTypeDescTarget.cs` | the TypeDesc branch, via a bare generic parameter |
| `ArraySetValueCastCheck.cs` | the non-throwing arm through `Array.SetValue` |
| `ArrayCopyCastCheckThrows.cs` | the throwing arm through `Array.Copy`, asserting the exact `IDS_EE_CANNOTCAST` message text for top-level, nested and namespaced types |

Mutation-checked: disabling the Nullable branch, disabling the TypeDesc branch, never throwing,
altering the message text, and swapping `typeHandleGetName` back for the reflection renderer
are each killed by exactly one of the above, and by the expected one.
