# `RuntimeTypeHandle_Instantiate` over a `fixed` array buffer

Rung J of the ASP.NET ladder (`docs/plans/2026-08-17-aspnet-critical-path.md`, local branch
`aspnet-spike`) now binds its route parameter and runs the handler, and stops serialising the
result to JSON. This is the plan for that stop.

## What is measured

Rung J under the linux CoreLib flavour, at main plus PR #1415, stops 33 frames out from `Main`
at

```
RuntimeTypeHandle.Instantiate: unsupported IntPtr result buffer pointer shape
<<element 0 of array <object #68509>> as System.IntPtr>
```

The full frame chain (throwaway `GuestLocation.ofState` probe, never committed):

```
RuntimeTypeHandle.Instantiate                          (the QCall)
RuntimeTypeHandle.Instantiate                          IL 62   (the Type[] overload)
RuntimeType.MakeGenericType                            IL 283
System.Text.Json.ObjectConverterFactory.CreateConverter  IL 236
JsonConverterFactory.GetConverterInternal
JsonSerializerOptions.ExpandConverterFactory
DefaultJsonTypeInfoResolver.GetConverterForType
DefaultJsonTypeInfoResolver.CreateJsonTypeInfo
DefaultJsonTypeInfoResolver.GetTypeInfo
JsonSerializerOptions.GetTypeInfoNoCaching / GetTypeInfoInternal / GetTypeInfo
Microsoft.AspNetCore.Http.HttpResultsHelper.WriteResultAsJsonAsync
Ok<T>.ExecuteAsync
RequestDelegateFactory.ExecuteResultWriteResponse (async state machine)
RuntimeMethodHandle.InvokeMethod ... MethodBase.Invoke   (the handler, reflection-invoked)
System.Linq.Expressions.MethodInfoCallInstruction.Run ... LightLambda.Run
RequestDelegateFactory.<Create>b__1
EndpointMiddleware.Invoke / EndpointRoutingMiddleware
RungJ.Program.Main
```

`CreateConverter` (IL dumped from the host's `System.Text.Json.dll`) closes
`SmallObjectWithParameterizedConstructorConverter<T, TArg0, TArg1, TArg2, TArg3>` — five
arguments, because the result type `Item(int Id, string Q)` is a record with a parameterised
constructor. `RuntimeType.MakeGenericType` (RuntimeType.CoreCLR.cs:3613) takes the
`Instantiate(RuntimeType)` fast path only for exactly one argument; with five it calls
`Instantiate(Type[])` (RuntimeHandles.cs:769), which is

```csharp
IntPtr[]? instHandles = CopyRuntimeTypeHandles(inst, out int instCount);
fixed (IntPtr* pInst = instHandles)
    Instantiate(new QCallTypeHandle(ref nativeHandle), pInst, instCount, ObjectHandleOnStack.Create(ref type));
```

Roslyn lowers the `fixed` to `ldelema System.IntPtr; conv.u`. Under PawPrint `ldelema` yields
the bare `Byref (ArrayElement (arr, 0), [])`, and `conv.u` then anchors a `ReinterpretAs IntPtr`
view on a plain array byref (NullaryIlOp.fs `Conv_I`/`Conv_U`: "crossing from byref-world to
native-pointer-world: subsequent pointer arithmetic must be byte-stride"), which is the shape in
the message.

The refusal is `NativeRuntimeTypeHelpers.nativeIntElementPointer` (NativeRuntimeTypeHelpers.fs:57),
which enumerates the pointer shapes an `IntPtr*` buffer argument has arrived in so far: a bare
`ArrayElement`, a bare `StackMemoryByte`, a `StackMemoryByte` under `ReinterpretAs IntPtr`
(the `Span<IntPtr>`-over-`stackalloc` route), and a single local or argument at index 0. Its
name says "result buffer" but `Instantiate` uses it for an *input* buffer: the closed
argument handles, read back as `NativeIntSource.TypeHandlePtr` cells by
`readTypeHandleInstantiationElement`. The other caller is `writeFieldHandleElement`
(`GetFields`), which writes.

No active guest takes this path: every `MakeGenericType` in `sourcesPure` passes exactly one
argument (`MakeGenericType{Struct,Class,New}Constraint.cs` and two others), so the array
overload is unexercised.

## Decisions

### 1. How the buffer pointer is strided

Two structurally different options.

**A. One more arm.** Add `ArrayElement (arr, base), [ReinterpretAs ty]` with the same
"`ty` must be native-int kind" guard the `StackMemoryByte`-under-reinterpret arm has, striding
the element index and keeping the projection. Smallest change; the enumeration grows by one and
the next buffer shape refuses the same way.

**C. Use the interpreter's own pointer arithmetic.** The QCall reads `pInst[i]`; a guest doing
`pInst + i` on this very pointer goes through `ManagedPointerByteView.addByteOffset`, whose
normalisation folds a byte offset under a reinterpret back into the root when the root has a
fixed cell size. `argumentByrefSlot` in `NativeReflectionInvocation.fs` already strides a
QCall's `void**` buffer this way. Replacing the enumeration with
`addByteOffset state <IntPtr view> (index * nativeIntSize) buffer` handles every shape a guest
pointer can have by the rule guests get, and the classifier stops being a list of shapes seen
so far.

The risk in C is specific and measurable: the cells hold provenance-bearing `TypeHandlePtr` /
`FieldHandlePtr` native ints with no byte image, so the strided pointer must come out as a
*cell* address (an `ArrayElement` at the advanced index under an address-preserving
`ReinterpretAs IntPtr`, or a `StackMemoryByte` at the advanced offset), not a byte cursor into
one. If normalisation does not fold to that for any of the existing shapes, the read refuses
loudly (there is no silent path) — and the existing `GetFields` and
`CreateInstanceForAnotherGenericParameter` guests, plus the new one, are the measurement.

**Recommendation: C, with A as the fallback if the measurement says the fold does not reach a
cell for some existing shape.** Either way the function's docstring stops calling it a result
buffer.

### 2. Scope

One QCall argument classifier and one new guest. `readTypeHandleInstantiationElement`'s
refusal of an open generic argument (`TODO ... with open generic type argument`) stays: the
STJ instantiation is closed, and a guest cannot reach it through `MakeGenericType`, whose
managed layer accepts open arguments only via `SignatureConstructedGenericType` /
`TypeBuilderInstantiation` before the QCall.

## Tests

`sourcesPure/MakeGenericTypeArrayArguments.cs` (differential):

1. `typeof(Dictionary<,>).MakeGenericType(typeof(string), typeof(int))` is reference-equal to
   `typeof(Dictionary<string, int>)` (the two-argument shape, the smallest that takes the array
   overload).
2. A five-parameter user generic closed over five distinct arguments, checked via
   `GetGenericArguments()` element by element, so a stride that read the wrong cell fails on a
   specific index (STJ's arity).
3. A constraint violation through the same overload (`where T : struct` given `string` in a
   two-parameter definition) is `ArgumentException`: the buffer is read before
   `validateConstraints`, so the read must work on the rejecting path too.
4. Instantiating the result (`Activator.CreateInstance`) and reading a field back, so the closed
   handle is a usable type and not merely a `Type` object.

Mutation: (m1) stride by index 0 for every element — check 2 dies on index 1; (m2) drop the
native-int-kind guard / stride by one byte instead of a cell — check 1 or 2 dies at the read;
(m3) under option C, pass the `Byte` view instead of `IntPtr` — the provenance-bearing cell read
refuses.

## Ladder

Measured on a throwaway merge of this branch with #1415 (rung J needs both). The converter
type instantiates, and rung J stops 39 frames out at the unimplemented InternalCall
`RuntimeFieldHandle::GetToken(IntPtr) -> int`: a field's metadata token, asked by
`RtFieldInfo.MetadataToken` from `CustomAttribute.GetCustomAttributes` on a field, while
System.Text.Json's `DefaultJsonTypeInfoResolver.AddMembersDeclaredBySuperType` looks for
attributes on the result type's fields. That is the next feature.

## Outcome

**#1417 landed first.** While this branch was being written, PR #1417 ("Read a multi-argument
`MakeGenericType`'s handle buffer as the pinned `IntPtr[]` it is", motivated by Fantomas 7's
`--version`, not the ladder) fixed the same stop with option A: one more arm for the
array-element-under-`IntPtr`-view shape, striding through `addByteOffsetToByteView`, plus a
two- and three-argument guest (`MakeGenericTypeMultipleArguments.cs`). So the rung J stop this
plan measured is already closed on main, and this branch, rebased over #1417, is now the
generalisation alone: the enumeration of buffer shapes (including #1417's arm) becomes the one
arithmetic rule, and the guest adds the five-argument, constraint-violation and
`Activator.CreateInstance` checks #1417's guest does not have. Whether that generalisation is
wanted as its own change is a judgement about the classifier, not about rung J.

Option C held and the fallback was not needed: with the enumeration replaced by
`addByteOffset` under the `IntPtr` view, the new guest passes and so does every guest through
the other users of the classifier (`GetFields`, `CreateInstanceForAnotherGenericParameter`, the
constraint-checking `MakeGenericType` guests), so the fold does reach a cell for every shape
that had an arm. The three mutants were all killed by the new guest. Codex was clean on the
first round.
