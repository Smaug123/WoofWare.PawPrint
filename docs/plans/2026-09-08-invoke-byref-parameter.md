# `RuntimeMethodHandle_InvokeMethod` over a byref parameter

Rung J of the ASP.NET ladder (`docs/plans/2026-08-17-aspnet-critical-path.md`, local branch
`aspnet-spike`) now executes a request and stops when route-parameter binding reflection-invokes
`Int32.TryParse(string, out int)`. This is the plan for that stop.

## What is measured

Rung J under the linux CoreLib flavour, at `origin/get-method-at` `db9bf070` (main `51474dd1`
plus PR #1405), stops 15 frames out from `Main` at

```
TODO: RuntimeMethodHandle.InvokeMethod on System.Int32::TryParse: parameter 3 is a byref,
whose copy-back semantics are not modelled
```

The refusal is `rejectParameterShape`'s `ConcreteTypeHandle.Byref` arm in
`WoofWare.PawPrint/Native/NativeReflectionInvocation.fs`. The full frame chain (a throwaway
`GuestLocation.ofState` probe that prints every frame, never committed):

```
RuntimeMethodHandle.InvokeMethod                       (the QCall)
RuntimeMethodHandle.InvokeMethod                       IL 36
MethodBaseInvoker.InterpretedInvoke_Method             IL 14
MethodBaseInvoker.InvokeDirectByRefWithFewArgs         IL 139
MethodBaseInvoker.InvokeWithFewArgs                    IL 198
RuntimeMethodInfo.Invoke                               IL 146
MethodBase.Invoke                                      IL 11
System.Linq.Expressions.ByRefMethodInfoCallInstruction.Run   IL 54
System.Linq.Expressions.Interpreter.Run                IL 31
System.Linq.Expressions.LightLambda.Run                IL 51
System.Linq.Expressions.DelegateHelpers.FuncThunk2     IL 30
RequestDelegateFactory.<Create>b__1                    IL 23
EndpointMiddleware.Invoke                              IL 181
EndpointRoutingMiddleware.SetRoutingAndContinue        IL 242
EndpointRoutingMiddleware.Invoke                       IL 96
RungJ.Program.Main                                     IL 243
```

So the route is the LINQ expression *interpreter*, not a direct `MethodInfo.Invoke` by
ASP.NET: `RequestDelegateFactory` builds an expression tree calling `TryParse` with a byref
argument, `Compile()` runs it under `System.Linq.Expressions.Interpreter` because PawPrint seeds
`IsDynamicCodeSupported` false, and the interpreter's `ByRefMethodInfoCallInstruction.Run` is
`MethodBase.Invoke(null, object[])` followed by `ByRefUpdater.Update` for each byref argument,
reading the updated value back out of the `object[]` (IL dumped from the host's
`System.Linq.Expressions.dll`, `Run` at IL_0031 and IL_00F8). The few-args route is the one
taken (two arguments), so the bare-address buffer shape is what rung J needs; the
`InvokeWithManyArgs` shape is covered because it is the same primitive.

Without #1405, rung J stops earlier at `GetMethodAt` (measured on `origin/main` alone), so
this PR stacks on `get-method-at` until that merges.

## What CoreCLR does

The QCall's part is one line. `InvokeUtil::CopyArg` (invokeutil.cpp:241):

```
case ELEMENT_TYPE_BYREF:
    *(PVOID *)pArgDst = argRef;
```

`args[i]` *is* the argument. The QCall neither reads through it before the call nor writes
anything back after it. Every word of "copy-back" happens in managed code, all of which PawPrint
interprets:

- `MethodInvokerCommon.Initialize` (MethodInvokerCommon.cs:42) strips the byref and classifies
  the *element*: `IsValueType` is set when the element is a pointer, a function pointer, or an
  actual value type, and `IsNullableOfT` for `Nullable<T>`. `needsByRefStrategy` forces the
  `InvokeDirectByRefWithFewArgs` / `InvokeWithManyArgs` routes.
- `CheckArguments` (MethodBaseInvoker.cs:347) sets `shouldCopyBack[i]` for every byref
  parameter, and decides what object sits in `copyOfArgs[i]`:
  - `TryByRefFastPath` (:391): an argument whose type *is* the element type and is a value type
    is **copied** (`RuntimeType.AllocateValueType`, which is `RuntimeHelpers.Box` over the
    source's raw data) "to prevent the boxed instance from being directly modified by the
    method". A reference-typed argument is passed as is.
  - `RuntimeType.TryChangeType` (RuntimeType.cs:946) for the rest: `null` becomes a `default`
    box via `AllocateValueType(elementType, null)` (`GetUninitializedObject`) for a value
    element and stays `null` for a reference element; a `Nullable<T>` element is reboxed as a
    true `Nullable<T>`; a byref-like element is `NotSupportedException`; a mismatched type is
    `ArgumentException`.
- `InvokeDirectByRefWithFewArgs` (:167) and `InvokeWithManyArgs` (:255) form the byref from the
  `IsValueType` flag: a value element's byref is `ref copyOfArgs[i]!.GetRawData()` (the box
  payload); a reference element's byref is `ref copyOfArgs[i]` (the `object?` slot itself).
- `CopyBack` (:324) then assigns `parameters[i] = copyOfArgs[i]` for every flagged slot. It runs
  after a normal return only, not in a `finally`: a callee that writes and then throws leaves the
  caller's `object[]` untouched.

So the caller's own box is never mutated, no two byref parameters can alias, and the callee's
partial writes are unobservable through `Invoke`. None of that is the QCall's doing.

## What PawPrint already has

- `readArgument` in `NativeReflectionInvocation.fs` already locates `args[i]` in both buffer
  shapes (`argumentByrefSlot`), reads the `ByReference` out of the slot, and re-imposes a type
  view on the byref it holds: the parameter type for a value type, `System.Object` for a
  reference type. It then *dereferences*. For a byref parameter the first three steps are wanted
  and the fourth is the bug.
- The byref arrives as `[...; ReinterpretAs System.Byte]` because `ByReference.Create<T>` goes
  through `Unsafe.As<T, byte>`. `ManagedPointerSource.reinterpretAs` collapses consecutive
  reinterpretations to the most recent one, so re-imposing the element view replaces the byte
  view rather than stacking under it.
- A byref argument on the eval stack is `EvalStackValue.ManagedPointer`; `callMethod` pops it
  as a `CliType.RuntimePointer (CliRuntimePointer.Managed _)` argument and `ldarg`/`ldind`/`stind`
  in the callee go through the byref model. Nothing new is needed on that side.
- `RuntimeHelpers.Box` and `GetUninitializedObject` are implemented, so `AllocateValueType`'s
  copy and default-fill both run. `ReboxToNullable` is refused
  (`NativeRuntimeTypeQCall.fs:1935`), so a `Nullable<T>` element stops before the QCall exactly
  as a `Nullable<T>` value parameter does today.

## Decisions

### 1. Where copy-back lives

Two structurally different options:

**A. Pass the byref through.** The argument the callee receives is the caller's own byref
re-viewed as the element type. The QCall does nothing after the call. This is `CopyArg`
verbatim, and the whole of copy-back is the managed code above, already interpreted.

**B. Copy in, copy out, in the QCall.** Read the value behind `args[i]` into a fresh cell,
pass a byref to the cell, and after the callee returns write the cell back through `args[i]`.

B is not observably different through `MethodBase.Invoke` today, because the managed layer has
already made the target a private copy. Its cost is in the re-entry protocol: the QCall's eval
stack is its state machine, and B must carry every (original byref, temporary) pair across the
suspended managed call, growing the marker from one slot to a variable number. It also
invents a second store for a value that already has one. A does not touch the protocol: the
callee writes into the storage CoreCLR would have handed it, and the marker stays as it is.

**Recommendation: A.**

### 2. Which type view the passed-through byref carries

The byref cannot be passed with its `System.Byte` view. Every `ldind`/`stind` in the callee
would then go through the bytewise path, which is refused for a struct with reference fields
(`readProjectedValue` and its write counterpart, and the "sub-cell byte view into a ref-holding
box" refusal). `readArgument`'s docstring already makes the point for reads: re-imposing the
type is the whole operation, and it is what `CopyArg` does with `th` for a value argument.

Two options for the view:

**(a) The same split `readArgument` uses:** the element type for a value-type element, and
`System.Object` for a reference-type element. This mirrors the managed layer's own
`IsValueType` split, which is the split that decided what the byref addresses (payload versus
slot). `System.Object` is also the only view available for an array-typed element (`ref int[]`
has no nominal `ConcreteType` to name).

**(b) The element type always.** Not available for arrays, and it buys nothing: a reference
slot reads and writes as an object reference under any reference view.

**Recommendation: (a)**, factored so the value/reference classification and the view it produces
are one function shared by the value-parameter and byref-parameter paths, so the two cannot
drift.

### 3. Scope

In: a byref parameter whose element is an actual value type other than `Nullable<T>`, or a
reference type. Both buffer shapes (at most four arguments, and `InvokeWithManyArgs` beyond).
`ref`, `out` and `in` are all `ELEMENT_TYPE_BYREF` at the QCall; the distinction is a
`ParameterInfo` attribute the QCall never sees.

Out, and refused by the same element-level checks the value-parameter path already applies:

- `ref Nullable<T>`: unreachable today (`ReboxToNullable` refuses first), refused here for the
  same reason the value-parameter arm is, so that landing `ReboxToNullable` cannot silently
  reach a read of a true boxed `Nullable<T>`.
- `ref T*` / `ref delegate*<...>`: the element's `IsValueType` flag makes the byref address a
  boxed `IntPtr` payload, the same shape the pointer-parameter refusal names.
- A byref *return* stays its own refusal (a different half of the QCall, and CoreCLR
  dereferences and boxes it; unrelated to arguments).
- `TypedReference` parameters are `ELEMENT_TYPE_TYPEDBYREF`, not `BYREF`, and are not touched.

The refusal message changes: "copy-back semantics are not modelled" named the wrong layer.

## Tests

All differential (`sourcesPure`), each `MethodInfo` invoked exactly once (the strategy switches
to Reflection.Emit on the host after the first call; see `ReflectionInvokeMethod.cs`).

`sourcesPure/ReflectionInvokeByRefParameter.cs`:

1. `int.TryParse("5", out int)` with a `null` slot: returns `true`, `args[1]` is a boxed 5. The
   rung J shape, on a corelib target.
2. `Increment(ref int)` given a boxed 41 named `o`: `args[0]` becomes 42 **and `o` still reads
   41** (the managed copy).
3. `ref string`: a `null` slot set by the callee, and a non-null one replaced.
4. `ref` to a struct with a reference field and an int field, both written by the callee: the
   case a byte view would refuse, so it pins decision 2.
5. Read-then-write through `ref` (callee adds to what it finds), so the byref carries the
   argument in as well as out.
6. Five parameters with byrefs at index 0 and 4: the `InvokeWithManyArgs` buffer, both the bare
   address and a cursor.
7. Callee writes through the byref then throws: `TargetInvocationException`, and the
   `object[]` slot still holds what the caller put there.
8. A wrong-typed argument for `ref int` is `ArgumentException` (managed; cheap to pin).
9. `in int` on the few-args path, read by the callee.

`sourcesPure/ExpressionInterpreterByRefCall.cs`: `Expression.Call` of `int.TryParse` with a
`ParameterExpression` in the `out` position, compiled and run, checking the variable was
updated. This is rung J's own route through `ByRefMethodInfoCallInstruction` and
`ByRefUpdater.Update`.

Mutation battery, each mutant killed by a named check:

- m1: dereference the byref as before (the old value path) for a byref parameter: check 1.
- m2: pass the byref with its `Byte` view intact: check 4.
- m3: view a value element as `System.Object`: check 4 (a struct payload under an object view).
- m4: skip the pointer-element refusal. Reachable only with a `System.Reflection.Pointer`
  argument (a plain `null` for `ref int*` faults in managed code at `GetRawData` before the
  QCall), and that family is parked in `ReflectionInvokePointerSignature.cs` on the sibling
  value-parameter refusal, so no active guest can kill it; recorded as such rather than given
  an invented test.

One risk the tests decide rather than this doc: check 2's managed copy is
`RuntimeHelpers.Box(ref GetRawData(box), handle)`, a box sourced from another box's payload,
and the existing `RuntimeHelpersBox.cs` guest only boxes from locals. If PawPrint's `Box`
refuses a heap-rooted source, that is a separate primitive and check 2 is parked on it rather
than this PR growing to cover it.

## Ladder

Re-measured with this in (linux flavour): rung J binds the route parameter, runs the handler,
and stops 33 frames out in JSON serialisation of the result, at

```
RuntimeTypeHandle.Instantiate: unsupported IntPtr result buffer pointer shape
<<element 0 of array <object #68509>> as System.IntPtr>
```

reached from `RuntimeType.MakeGenericType` inside
`System.Text.Json.ObjectConverterFactory.CreateConverter`, under `Ok<T>.ExecuteAsync`. The
`Instantiate` QCall's result buffer arrives as a byref into an array element, a shape its
handler does not yet accept. That is the next feature, not this one.

## Outcome

Decisions 1, 2 and 3 held as recommended. Two of the planned checks turned out to reach
pre-existing gaps *before* the QCall, and were parked rather than absorbed:

- A null `out int` slot: `TryChangeType` allocates the default box through
  `RuntimeHelpers.GetUninitializedObject`, whose QCall
  `ReflectionSerialization_GetCreateUninitializedObjectInfo` is unimplemented
  (`ReflectionInvokeOutNullSlot.cs`). Rung J does not need it: the interpreter's
  `ByRefUpdater` passes a boxed default, and the active guest does the same.
- A reference-holding struct argument: the managed copy is `RuntimeHelpers.Box` over the
  box's raw data, and the bytewise copy in `BoxCache.Box` refuses a box holding references —
  the same gap `RuntimeHelpersBoxReferenceContainingStruct.cs` is parked on
  (`ReflectionInvokeByRefReferenceStruct.cs`). The active guest uses a reference-free struct,
  which still needs the element view (a field cannot be selected under the byte view).

Mutation: m1–m3 killed by the new guest; m4 survived as predicted; m5 (the shared classifier
viewing a reference element as itself) survived the two new guests and was killed by the F#
`sprintf` guests, whose `CaptureFinalN` arguments include arrays; m6 (the byref arm alone
doing so) is killed by the `ref int[]` check added for it.
