# `RuntimeFieldHandle_GetValue`: the primitive under `FieldInfo.GetValue`

Issue #1434. The `RuntimeFieldHandle_SetValue` plan (`2026-08-17-runtimefieldhandle-setvalue.md`)
chose to answer `IsFastPathSupported = false`, and recorded the knock-on: `FieldAccessor.GetValue`
then never takes an address-based path either, so this QCall is the *only* route a reflective
field read can take under PawPrint. That decision is made; this plan is the read half of it.

## What is measured

`sourcesPure/ReflectionFieldGetValue.cs` exits 0 on real .NET 10. Before this change PawPrint stops
it at the first `GetValue`:

```
Unimplemented native method (PInvokeImpl QCall!RuntimeFieldHandle_GetValue)
```

Two further facts measured on real .NET 10 while writing the refusal set:

| guest | answer |
| --- | --- |
| `typeof(int?).GetField("value", NonPublic \| Instance).GetValue((object)(int?)3)` | `FieldAccessException` |
| the same field, `.SetValue(..., 5)` | `FieldAccessException` |
| `GetValue` on an `int*` field (instance or static) | a `System.Reflection.Pointer` |
| `GetValue` on a `delegate*<int>` field | a boxed `IntPtr` |

The first two are the managed `FieldAccessorType.NoInvoke` arm, which the accessor's constructor
selects for a `Nullable<T>` declaring type (FieldAccessor.cs:23-27) before any QCall runs, so the
QCall never sees a `Nullable<T>` declaring type from this caller. (Issue #1319 expects
`NotSupportedException` there; that is CoreCLR's answer inside the QCall, which this CoreLib does
not reach.)

## What CoreCLR does

`RuntimeFieldHandle_GetValue` (reflectioninvocation.cpp:28) is `InvokeUtil::GetFieldValue`
(invokeutil.cpp:972) once the arguments are unpacked. Its shape is `SetValidField`'s, read in
reverse:

1. refuse a `Nullable<T>` or generic-shared declaring type (`NotSupportedException`);
2. if `*pIsClassInitialized == FALSE`, run the initialiser and write `pDeclMT->IsClassInited()`
   back — the same block, with the same `TargetInvocationException` wrapping of a failing
   initialiser, as the write side;
3. switch on the field's element type:
   - primitives (`BOOLEAN` .. `U`): allocate a box of `fieldType` and copy the cell into it;
   - `OBJECT`/`CLASS`/`SZARRAY`/`ARRAY`/`VAR`: the `OBJECTREF` itself;
   - `VALUETYPE`: allocate a box, `CopyValueClass` into it, then `Nullable::NormalizeBox` —
     so a `Nullable<T>` field answers null or a boxed `T`, never a boxed `Nullable<T>`;
   - `FNPTR`: a boxed `IntPtr`; `PTR`: a `System.Reflection.Pointer` (`CreatePointer`).

The managed caller (`FieldAccessor.GetValue`, FieldAccessor.cs:171-193) has already done
`VerifyTarget` for an instance field, so a null or mistyped instance does not arrive.

## Decisions

### 1. What boxes the result?

**A. `Boxing.boxValue`**, the function under the `box` opcode, `constrained.` callvirt and the
`RuntimeMethodHandle_InvokeMethod` return. It already applies the `Nullable<T>` rule that
`Nullable::NormalizeBox` applies, and produces the heap shape `BoxedValue.contents` (which the
write side unboxes through) is the inverse of.

**B. Allocate the box directly from the cell** with `IlMachineState.allocateManagedObject`,
avoiding the eval-stack round trip.

**Chosen: A.** B would be a second statement of what `box` puts in a heap object, and the write
side's plan already paid for the lesson that a second copy of the box shape drifts. The cost of A
is `EvalStackValue.ofCliType` flattening a primitive-like cell (an enum, `IntPtr`) on the way in
and `boxValueType` rebuilding it from the type's fields on the way out — exactly what the `box`
opcode does with the same cell, so the two routes cannot disagree.

### 2. Where does the class-initialisation block live?

The read and write QCalls share, verbatim: resolving the handle to a field and a closed declaring
type; reading the `ref bool` as a four-byte cell; the pre-observed-`Failed` refusal;
`ensureTypeInitialised` and its early return; the `Initialized`-not-`InProgress` predicate; and
the write-back that happens only when the caller passed `false`.

**A. Duplicate it into the new arm.** Two copies of a block whose subtleties took a unit test
each to get right (the write-back gate, the `InProgress` distinction).

**B. Lift it into private helpers** in the same module, called from both arms.

**Chosen: B.** The SetValue arm's text moves into the helpers unchanged, comments included, so the
diff to that arm is a pure extraction.

### 3. The refusal set

Everything below is a `failwith` naming the field and the type, not a guest exception, because
each is either unreachable from the only managed caller or a capability PawPrint lacks:

- **pointer and function-pointer fields.** CoreCLR answers a `System.Reflection.Pointer` or a
  boxed `IntPtr` around a raw address. PawPrint's `RuntimePointer` cell carries provenance rather
  than bits, and the `box` opcode refuses a pointer type for the same reason (`executeBox`), so
  this refuses too and says what the answer would be.
- **an RVA-backed static.** The write side refused this because `setStatic` and `ldsflda` address
  different storage. The read side has the mirror problem: `ldsfld` reads the ordinary static
  slot, which for such a field is untouched zero, while `ldsflda` reads the PE bytes. Picking
  either would be picking a side in an asymmetry the opcodes themselves have not resolved.
- **a null instance for an instance field.** Managed `VerifyTarget` throws `TargetException`
  first.
- **a non-closed declaring type.** `FieldAccessor`'s constructor routes it to `NoInvoke`; reused
  from the write side.

Not refused: a **static that nothing has stored**. `getStatic` answers `None` for storage no
`stsfld` has touched, and the field's zero is the right cell, as `ldsfld` computes. The handler
does not write the zero back the way `ldsfld` does: a read that mutates static storage is a
surprise, and nothing can observe the difference.

## Tests

`sourcesPure/ReflectionFieldGetValue.cs` (differential): each primitive shape boxes as the field's
own type; a reference field hands back the same reference; a null reference; an `object` field
holding a box hands back *that* box; an enum boxes as the enum, not its underlying; a struct
field is a *copy* (a later write to the field does not reach the earlier box); a struct holding a
reference; a `Nullable<T>` field answers null then a boxed `T`; `IntPtr`; a private field; a
boxed-struct target; fields and per-instantiation statics of a closed generic type; the managed
`NoInvoke` refusal for an open definition; a precise-init static whose initialiser the read must
run *first* (7, not 0); statics nothing has written; `[ThreadStatic]`; static readonly; a second
read after a direct write (the accessor's permanent slow-path arm); a `SetValue`/`GetValue`
round trip.

`TestNativeRuntimeFieldHandleGetValue.fs` (unit): the out-cell, for the same reason the write
side's fixture exists — no guest can see it under `IsFastPathSupported = false`; the suspension
for a lazy initialiser leaves both the result slot and the out-cell untouched; one test per refusal
arm. The fixture helpers are shared with the SetValue fixture rather than copied.

Mutations, each against the Guest fixtures matching `Name~ReflectionField` plus the two unit
fixtures, recorded in the Outcome section once run.
