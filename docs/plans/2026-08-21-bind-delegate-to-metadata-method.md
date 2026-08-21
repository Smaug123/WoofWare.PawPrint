# `Delegate_BindToMethodInfo` for a metadata method

## Where this came from

Rung E of the ASP.NET critical-path ladder is

```csharp
var p = Expression.Parameter (typeof (int), "x");
var lambda = Expression.Lambda<Func<int, int>> (Expression.Add (p, Expression.Constant (3)), p);
var f = lambda.Compile ();
return f (4) == 7 ? 0 : 1;
```

Measured at `origin/main` (046141b2), it stops at

```
TODO: Delegate_BindToMethodInfo was asked to bind a delegate to the metadata method
{ _Inner = System.Reflection.Metadata.MethodDefinitionHandle } in System.Linq.Expressions, …;
PawPrint implements this QCall only for a method minted by Reflection.Emit
```

## What the guest is actually asking for

The refusal message names no method, so a probe in that arm printed the resolved target and the
whole frame chain:

```
target: System.Dynamic.Utils.DelegateHelpers.FuncThunk1 (static=true, virtual=false,
        generics=2, params=2)  declaringType=Closed  flags=0x00000040  delegateType=Func<int,int>
 [0] System.Private.CoreLib!Delegate.<BindToMethodInfo>g____PInvoke|21_0
 [1] System.Private.CoreLib!Delegate.BindToMethodInfo
 [2] System.Private.CoreLib!Delegate.BindToMethodInfo
 [3] System.Private.CoreLib!Delegate.CreateDelegateInternal
 [4] System.Private.CoreLib!System.Reflection.RuntimeMethodInfo.CreateDelegateInternal
 [5] System.Private.CoreLib!System.Reflection.RuntimeMethodInfo.CreateDelegate
 [6] System.Linq.Expressions!System.Dynamic.Utils.DelegateHelpers.CreateObjectArrayDelegateRefEmit
 [7] System.Linq.Expressions!System.Dynamic.Utils.DelegateHelpers.CreateObjectArrayDelegate
 [8] System.Linq.Expressions!System.Linq.Expressions.Interpreter.LightLambda.MakeDelegate
 [9] System.Linq.Expressions!System.Linq.Expressions.Interpreter.LightDelegateCreator.CreateDelegate
[10] System.Linq.Expressions!System.Linq.Expressions.Interpreter.LightDelegateCreator.CreateDelegate
[11] System.Linq.Expressions!System.Linq.Expressions.Expression`1.Compile
[12] RungE!Program.Main
```

Two things about that chain are worth recording, because both contradict what the frame names
suggest.

`CreateObjectArrayDelegateRefEmit` emits nothing here. Its first act is
`DelegateHelpers.GetCSharpThunk`, which indexes the pre-baked `s_FuncThunks` table and
`MakeGenericMethod`s the entry — `FuncThunk1<int, int>`, an ordinary static generic method with a
MethodDef row in `System.Linq.Expressions`. Its `ILGenerator` path runs only when that table has no
entry of the right arity (`GetCSharpThunk` returns null), so `Expression.Compile` on a small lambda
reaches `Reflection.Emit` not at all. The `ldftn`/`MakeDelegate` reading of this path in an earlier
note was wrong: `LightLambda.MakeDelegate` calls `CreateObjectArrayDelegate`, not a `ldftn` +
`Func<object[], object>::.ctor` pair.

So the shape being asked for is a **closed delegate over a static, non-virtual, generic-instantiated
metadata method**: `FuncThunk1<int, int>` takes `(Func<object[], object>, int)` and returns `int`,
`Func<int, int>::Invoke` supplies one argument, and the bound `Func<object[], object>` is the
`target` argument.

## The flag sets that are reachable through this QCall

`NativeDelegate.isCompatible`'s docstring says four of `IsMethodDescCompatible`'s five flag filters
are unreachable, and that "whoever makes a second caller reachable should add their arms back with
the tests that exercise them". Enumerating the callers again, now that the metadata arm is in scope:

| caller | flags |
| --- | --- |
| `Delegate.CreateDelegateNoSecurityCheck` (`Delegate.CoreCLR.cs`:387) | `RelaxedSignature` |
| `Delegate.CreateDelegate(Type, object?, MethodInfo, bool)` (:350) | `RelaxedSignature` |
| `RuntimeMethodInfo.CreateDelegate(Type, object?)` (`RuntimeMethodInfo.CoreCLR.cs`:384) | `RelaxedSignature` |
| `Delegate.CreateDelegate(Type, MethodInfo, bool)` (:318) | `OpenDelegateOnly \|\| RelaxedSignature` |
| `RuntimeMethodInfo.CreateDelegate(Type)` (:371) | `OpenDelegateOnly \|\| RelaxedSignature` |

So exactly two combinations reach `Delegate_BindToMethodInfo`, and the newly reachable filter is
`DBF_OpenDelegateOnly` and only that. `StaticMethodOnly`, `InstanceMethodOnly`,
`ClosedDelegateOnly`, `NeverCloseOverNull` and `CaselessMatching` belong to `BindToMethodName`,
which is a different QCall and still unimplemented. The existing handler's `flags <>
relaxedSignature` refusal therefore widens to "one of those two", not to "anything".

## What the shapes actually do upstream

Measured on real .NET 10 rather than derived, because several of these contradict the obvious
reading. Every row exits cleanly, so each is a fact a `sourcesPure` guest can assert.

| shape | real .NET |
| --- | --- |
| `S3.ToString` (struct override) open over `delegate string D(ref S3)` | binds, runs the override |
| `S3.Go` (struct interface impl, `virtual final`) open over `ref S3` | binds, runs it |
| the same struct methods open over a *by-value* first parameter | `ArgumentException` |
| `int.ToString()` / `S3.ToString()` closed over a boxed receiver | binds, runs |
| `object.ToString` closed over a boxed `S3` | binds, runs `S3`'s override |
| `object.ToString` open over `Func<object, string>` | binds, dispatches per argument at invocation |
| instance method closed over `null` | binds, `Target == null`; runs, and only faults if the body touches `this` |
| `Delegate.CreateDelegate(t, genericMethodDefinition, throwOnBindFailure: false)` | throws `ArgumentException` — the QCall raises regardless of the flag |
| a `__arglist` target | `ArgumentException` |
| `List<int>.Add` open, then `d.Method` | answers `Add` on `List<int>` |

Two of those settle predicates the rest of this plan depends on.

**Roslyn does not mark struct overrides `final`** — measured, `S3.ToString` and `Int32.ToString` are
both `IsVirtual=True, IsFinal=False`, while a struct's *interface implementation* is `virtual final`.
So `MethodInfo.DispatchesVirtually` (`MethodInfo.fs`:715, `not IsStatic && IsVirtual && not IsFinal`)
is **true** for a struct override. CoreCLR's open path exempts value types explicitly —
`if (pTargetMethod->IsVirtual() && !pTargetMethod->GetMethodTable()->IsValueType())` takes the
virtual-call-stub branch, and everything else is treated "like non-virtual methods"
(comdelegate.cpp:1237-1272). A refusal keyed on `DispatchesVirtually` alone would therefore reject
the first row of that table.

**`IsMethodDescCompatible` never consults virtualness at all.** Every virtual decision lives in
`BindToMethod` (comdelegate.cpp:1184+). That matters for the decomposition below: the compatibility
routine needs `IsStatic` and the declaring type and nothing else, and the virtual arms belong in the
handler, where the whole `MethodInfo` is in hand — including raw `IsVirtual`, which
`DispatchesVirtually` cannot recover because it folds `not IsStatic` in.

## Options

### Option A (proposed): one `isCompatible`, generalised to describe its target

Give `isCompatible` a description of the target method rather than assuming the dynamic-method
answers, and transcribe every arm of `COMDelegate::IsMethodDescCompatible`
(comdelegate.cpp:2544-2762) that a metadata target makes reachable.

```fsharp
[<RequireQualifiedAccess>]
type private DelegateTarget =
    /// Minted by Reflection.Emit: always static, and declared on the synthetic per-module class,
    /// which is not a type an instance receiver could have.
    | Dynamic
    /// A method with a MethodDef row. `declaringType` is the `methodType` QCall argument, which is
    /// also where an instance target's first argument type comes from.
    | Metadata of isStatic : bool * declaringType : ConcreteTypeHandle
```

A DU rather than two parameters because a dynamic method has *no* declaring-type handle to supply,
so a flat `ConcreteTypeHandle` parameter would need a dummy value at that call site. This is the
shape #1110 settled on for `DeclaringTypeContext` and for the same reason: the existing path stays
correct by construction rather than by the caller passing the right filler. It deliberately carries
no virtualness — see above.

What changes inside `isCompatible`:

* `numTotalTargetArgs = numFixedTargetArgs + (isStatic ? 0 : 1)` (:2596), where today it is just the
  fixed count.
* the `DBF_OpenDelegateOnly` filter (:2611), as a second `bool` beside `relaxed` — keeping the
  handler as the place that validates the flag set, which is the contract the current docstring
  establishes.
* the instance-target first-argument rule (:2688-2707): `thFirstTargetArg = thExactMethodType`,
  byref-promoted when the delegate is open and the declaring type is a primitive or value type
  (`etFirstTargetArg <= R8 || VALUETYPE || I || U`). `ConcreteTypeHandle.Byref` expresses that, and
  `isLocationAssignable`'s byref arm then admits only an identical byref — which is exactly the
  measured answer (row 1 binds, row 3 does not).
* **the remaining-argument pairing** (:2725-2745). Upstream's static branch consumes the target's
  first fixed argument before the loop and its instance branch does not, so for an instance target
  the loop pairs the invoke tail against *all* of the target's fixed parameters. Today's
  `remainingTargetArgs = targetSignature.ParameterTypes.Tail` (`NativeDelegate.fs`:413) is right
  only for a static target.
* first-argument relaxation `!IsStatic || relaxed` (:2717-2721), transcribed rather than dropped even
  though it cannot currently differ (every reachable flag set contains `RelaxedSignature`).
* the vararg check (:2582) is unchanged and already correct, but its comment claiming no test can
  reach it becomes false: `static void M(__arglist)` compiles, and binding it is measured to fail.

`FindOrCreateAssociatedMethodDesc` (comdelegate.cpp:1147) runs before compatibility and does three
things. Instantiating the method against `pMethMT` is PawPrint's concretisation of the identity
against its `Closed` declaring handle, which this slice must do anyway to read the signature.
Requesting the *unboxing stub* for an instance method on a value type needs no analogue:
`callMethodWithCommitment` already converts an `ObjectRef` receiver into a byref into the box for a
value-type callee (`UnaryMetadataTokenOps.fs`:337-346), which is why row 4 of the table works, and
the open path's stub *reversal* (:1255-1266) is likewise implicit in pushing the byref invoke
argument straight through as `this`. Instantiating methods on generic interfaces is what
concretisation against the declaring handle already produces.

`HasUnmanagedCallersOnlyAttribute` is **not** an arm of this QCall — the only delegate-path check is
in `GetDelegateCtor` (comdelegate.cpp:2791-2795), on the JIT's `ldftn`/`newobj` path. Measured:
`CreateDelegate` over a `[UnmanagedCallersOnly]` method binds, and *invoking* it fail-fasts the
process uncatchably. So nothing to add at bind time, and a divergence to record: PawPrint would
interpret the body. Refusing it belongs in `dispatchDelegateInvoke`, not here, and cannot be a
`sourcesPure` check because the oracle run does not exit 0.

`BindToMethod` (comdelegate.cpp:1184) for a metadata target:

* **closed**: `_target := firstArg`, `_methodPtr := FunctionPointerTarget.Managed target`.
  Virtualise first when `DispatchesVirtually && targetAddr.IsSome && receiverType <> declaringType`
  — all three conjuncts from :1284-1286 — using
  `IlMachineStateExecution.tryResolveVirtualImplementation` with `walkBaseTypes = true`, which is
  what `executeLdvirtftn` already calls for the same stated reason ("CoreCLR also binds the target
  eagerly — `Delegate.Equals` compares the stored `_methodPtr`"). `DispatchesVirtually` in place of
  upstream's `IsVirtual()` is answer-preserving here: the delta is `final` methods, whose slot
  always resolves to themselves.
* **open**: `_target := null`, `_methodPtr := FunctionPointerTarget.Managed target`. Null rather
  than the delegate itself, which is what CoreCLR stores: `Delegate.GetTarget` is
  `_methodPtrAux == 0 ? _target : null`, PawPrint writes no `_methodPtrAux`, so null is what keeps
  `d.Target` truthful — and measured, an open reflection-built delegate is `Equals` to an
  `ldftn`-built one over the same method, with the same hash, which null also preserves.
* **refuse, open over a virtual target on a reference type**: `DispatchesVirtually && the declaring
  type is not a value type`. CoreCLR resolves this one at *invocation*, through a virtual call stub
  in `_methodPtrAux` with `_invocationCount` holding the `MethodDesc`;
  `AbstractMachine.dispatchDelegateInvoke` calls whatever `_methodPtr` names without virtualising,
  so binding the declared method would silently ignore an override. Measured reachable
  (`object.ToString` open over `Func<object, string>` dispatches per argument on real .NET), so this
  is a real gap, and it is #959's. The value-type exemption and the `final` exemption are both
  *served*, not refused, per the two findings above.
* **refuse, static virtual target** (a static abstract interface method): upstream's closed-path
  virtualisation condition tests `IsVirtual() && *pRefFirstArg != NULL` without excluding statics,
  which would virtualise on the *bound first parameter*. Not a shape to guess at. Decided from the
  `MethodInfo`'s raw `IsStatic && IsVirtual`, which is why the DU carries neither.
* **refuse, declaring type is an open generic definition**: `typeof(G<>).GetMethod("M")`. Binding
  needs a real instantiation to read the signature against; there is none.

`method->IsGenericMethodDefinition()` is a guest-visible `ArgumentException(Arg_DlgtTargMeth)` raised
*by the QCall*, not a FALSE return (comdelegate.cpp:1137-1139). Raise it with
`NativeHandlerResult.raiseExceptionWithMessage baseClassTypes.ArgumentException` and the resx text
("Cannot bind to the target method because its signature is not compatible with that of the delegate
type."). Keeping it in this slice is not optional: without it, `mi.CreateDelegate` on a generic
definition does not return a wrong answer, it host-crashes concretising a signature against an empty
method instantiation, on ordinary guest input.

### Fallout this slice owes elsewhere

`Delegate_FindMethodHandle`'s guard at `NativeDelegate.fs`:865 refuses an open instance delegate over
a *generic* declaring type, because `Delegate.GetMethodImpl` dereferences `_target` to walk the base
chain when `_methodPtrAux` is zero. Its justifying comment (:844-848) says the legal open shape
"needs either raw `ldnull; ldftn; newobj` IL, which the C# harness cannot emit, or
`Delegate.CreateDelegate(Type, MethodInfo)`, which `Delegate_BindToMethodInfo` refuses first". This
slice makes that second route live, so the comment becomes false and the guard becomes reachable.
Measured: real .NET answers `Add` on `List<int>` for exactly this shape. There is no narrow fix —
the field CoreLib branches on is `_methodPtrAux`, which PawPrint does not model — so the obligation
is to rewrite the comment to name the now-live route and park a test recording the crash, rather
than leave a guard whose stated reason for being unreachable is no longer true.

Three docstrings also become wrong and are part of the diff: `isCompatible`'s
dynamic-method-specialisation remarks, `isLocationAssignable`'s dead-branch justification (the
generic-variable half stays dead, but now because open generic definitions are refused and
instantiations are concretised, not because a `SignatureHelper` blob cannot spell a variable), and
the vararg comment above.

### Option B (rejected): a separate `isMetadataCompatible`

Leave `isCompatible` alone and write a second function for the metadata case. Zero risk to the
working dynamic path, and no DU to design.

Rejected because `IsMethodDescCompatible` is one upstream function whose arms interact — the arity
classification feeds the open/closed filters, which feed where each side's first argument comes
from, which feeds the remaining-argument pairing. Two transcriptions means a correction to one arm
can silently miss the other, and the arm most likely to need correcting
(`isLocationAssignable`'s enum and byref rules) is shared.

### Option C (rejected): guard narrowly to the shape rung E needs

Accept only static, non-virtual, closed metadata targets; refuse everything else.

Rejected because it is not smaller in the place that matters. `MethodInfo.CreateDelegate` over an
instance method and over an open static are the two most ordinary uses of this API, a guest reaches
them immediately after rung E, and serving them needs precisely the arms Option A adds.

### Option D (considered, rejected): split static and instance targets into two slices

Ship static targets first — enough for rung E and for the `OpenDelegateOnly` filter — then instance
targets, which is where the arity `+1`, the first-argument-from-declaring-type rule, the byref
promotion, the remaining-argument pairing, the bind-time virtualisation and the
`Delegate_FindMethodHandle` fallout all live.

Genuinely tempting, and it is the smaller first PR. Rejected because "static only" is not half the
surface of a feature, it is a half-transcribed function: `isCompatible` would carry the arity
classification and the flag filters generalised but the first-argument and pairing rules still
specialised, which is Option B's failure mode inside a single function rather than across two. It
would also ship a `failwith` on the single most common use of the API. The cost of not splitting is
that this is a larger diff than the last few slices, which the PR should say plainly.

## Tests

`sourcesPure/DelegateBindToMetadataMethod.cs`, so real .NET is the oracle for every check and the
exit code is the index of the first failure. Written and validated before implementing: 30 checks,
exit 0 on real .NET 10, and under PawPrint at `origin/main` it stops at check 1 with exactly the
`Delegate_BindToMethodInfo` refusal — nothing unrelated in the way.

Covered: closed and open over a static method; the rung E shape (a generic method *instantiation*);
both `CreateDelegate` overloads, so both reachable flag sets; closed and open over an instance
method; closed over a receiver that overrides the target, and over one that does not; an interface
method through an implementing receiver; closed over null for a static target; relaxed return-type
matching and the enum arm; `d.Target` and `d.Method` on both shapes; a generic *declaring* type,
closed and open; closed over a boxed value type; open over a value type with a `ref` first Invoke
parameter, with the by-value variant as its control; and five refusals — impossible arity, mismatched
argument types, `OpenDelegateOnly` against a closed pairing, a closed static whose first argument is
a value type, and a generic method definition.

Every refusal has a *positive control* that differs in exactly the rule under test, because an
`ArgumentException` proves only that some guard fired. The value-type refusal's control is the same
shape with an object-reference first argument; the `OpenDelegateOnly` refusal's control is the same
method and delegate type through the other overload; the byref promotion's control is the by-value
variant.

Still to add, from review:

* **struct override open over `ref`** — the shape that distinguishes `DispatchesVirtually` from
  `DispatchesVirtually && not value type`. Without it the value-type exemption is untested and the
  refusal predicate is unfalsifiable.
* **`object.ToString` closed over a boxed struct** — bind-time virtualisation where the receiver is a
  value type and the declaring type is not.
* **closed instance over null**, binding and invoking a body that does not touch `this`. The
  virtualisation step must skip when no receiver was supplied, and this is what kills a mutant that
  dereferences it.
* **`__arglist` target** → `ArgumentException`, killing a delete-the-calling-convention-check mutant.
* **open instance with more than one further parameter** — the discriminating input for the
  remaining-argument pairing; with a single-parameter target, both pairings agree.

Corrections to the mutation pairings:

* the `OpenDelegateOnly` mutant is killed only if the closed shape it degenerates to is *otherwise
  legal*, so the check's target must take a reference type first.
* the first-target-arg mutant (read the first fixed parameter instead of the declaring type) is
  killed only by an instance method with at least one parameter whose type cannot accept the
  receiver; a parameterless instance method makes the mutant crash on `.Head` instead, which is red
  for the wrong reason.
* **the generic-method-definition mutant (return FALSE instead of raising) survives** the check as
  first written: through `mi.CreateDelegate(Type)` a FALSE return produces the *identical*
  `ArgumentException(SR.Arg_DlgtTargMeth)` from managed code. The discriminating input is
  `Delegate.CreateDelegate(type, genericDefMi, throwOnBindFailure: false)` — measured, real .NET
  still throws, while the mutant returns null.
* the receiver-type-equals-declaring-type check kills no mutant on its own (both "always virtualise"
  and "never virtualise" agree there); it stays as regression, not as mutation coverage.

Parked, recording measured refusal text: open over a virtual target on a reference type; a declaring
type that is an open generic definition; and the `Delegate_FindMethodHandle` shape this slice makes
reachable (open instance over a generic declaring type, then `d.Method`).

## Outcome

Implemented as Option A. `isCompatible` now takes a `TargetFirstArgument` — where the target's first
argument comes from, which is the whole of what `IsMethodDescCompatible` consults about the target
beyond its signature — derived from a `BindTarget` that the handler's two arms produce, so the two
cannot disagree about whether the target is static. The dynamic path's behaviour is unchanged by
construction: `BindTarget.Dynamic` selects `FirstFixedParameter`, which is what the routine assumed
before.

Rung E is green. `Expression.Lambda<Func<int,int>>(...).Compile()` runs end to end and the compiled
delegate returns the right answer; `sourcesPure/ExpressionLambdaCompile.cs` pins five shapes of it
(one, two and zero arguments, and a reference-typed parameter and return).

`sourcesPure/DelegateBindToMetadataMethod.cs` is 36 checks, exit 0 on real .NET 10 and under
PawPrint. It gained a `step` counter and an attributing `Main`, because every bind failure surfaces
as an `ArgumentException` from `CreateDelegate`: without it a regression in any rule below would
report an unhandled exception and name no rule.

Every mutant is killed at a named check, which is what the counter bought:

| mutant | dies at |
| --- | --- |
| drop the implicit `this` from the target's argument count | check 5 |
| take an instance target's first argument from its first fixed parameter | check 5 |
| ignore `DBF_OpenDelegateOnly` | check 21 |
| skip bind-time virtualisation | check 7 |
| write the delegate itself into `_target` on the open path | check 14 |
| apply the closed-static objref constraint to instance targets too | check 28 |
| drop the byref promotion for an open value-type receiver | check 29 |
| pair the remaining arguments against the target's tail unconditionally | check 6, via the internal-error assertion |
| drop the vararg check | check 36 |
| answer a generic method definition with FALSE rather than raising | check 24 |
| remove the generic-method-definition guard entirely | host crash, "index outside the bounds of the array", concretising against an empty method instantiation — which is why the raise could not be deferred to a later slice |

One mutant survives and is documented in the code as unkillable: dropping the
`not IsStatic ||` disjunct from the instance first-argument relaxation. Both flag sets that reach
this QCall contain `DBF_RelaxedSignature`, so the disjunct cannot change the answer; it is
transcribed for fidelity rather than for behaviour.

Four shapes are parked, each with its refusal measured rather than predicted, and each with a served
control in the same file so the refusal is about the named rule:

* `DelegateBindOpenVirtual.cs` — open over a virtual method on a reference type (#959's
  `_methodPtrAux`). Controls: a non-virtual instance method, and a `sealed override`, both served.
* `DelegateBindStaticAbstractInterfaceMethod.cs` — the both-static-and-virtual shape. Real .NET
  binds it and raises `EntryPointNotFoundException` on invocation; measured.
* `DelegateBindOpenGenericDefinitionMethod.cs` — a declaring type that is an open generic
  definition. Real .NET's two directions fail differently, from `TryGetMultiCallableAddrOfCode`
  (method.cpp:2091) and from the compatibility check respectively.
* `DelegateFindMethodHandleOpenInstanceGeneric.cs` — the fallout this slice owed: the guard at
  `NativeDelegate.fs`:865 whose own comment called it unreachable "because
  `Delegate_BindToMethodInfo` refuses first". That comment is now rewritten to name the live route.

Full suite: 4101 tests, 4100 passed, 1 skipped, 0 failed.

One thing the review raised that this slice deliberately does not touch: `CreateDelegate` over a
`[UnmanagedCallersOnly]` method binds on real .NET and fail-fasts the process uncatchably when
invoked. There is no bind-time arm to add — CoreCLR checks it in `GetDelegateCtor`
(comdelegate.cpp:2791), on the JIT's `ldftn`/`newobj` path — so the refusal belongs in
`dispatchDelegateInvoke`, and it cannot be a `sourcesPure` check because the oracle run does not
exit 0.

## Fallout found by review after implementation

Codex found one shape this change made reachable and got wrong, and it is the same class of problem
as the `Delegate_FindMethodHandle` guard: a target that only the metadata path can name.

An **abstract** instance method closed over a **null** receiver. That is the one route to an abstract
target — a non-null receiver's runtime type is necessarily a subclass of the abstract declaring type,
so binding virtualises to a concrete override, and the open shape is refused — and it reached
`AbstractMachine.executeOneStep`'s internal `BUG: reached executeOneStep for abstract method`
invariant, from ordinary guest code.

Measured on real .NET 10: the binding *succeeds*, and invocation raises a catchable
`BadImageFormatException` with `HResult = 0x8007000B` (`COR_E_BADIMAGEFORMAT`). An interface member
behaves identically, being abstract for the same reason. A non-abstract virtual closed over null
runs normally, so the failure is about the absent body rather than the null receiver.

Fixed at the faithful place — the invocation, not the binding — in `dispatchDelegateInvoke`, reusing
the frame-pop-then-raise ordering the dynamic-target failure already establishes (a stub frame still
on the stack lands in the guest's trace, which `DelegateCctorFailureTraceHasNoStubFrame.cs` pins).
That ordering is now a named local rather than inline, so both failures share it.

`sourcesPure/DelegateToAbstractMethodOverNull.cs` pins it, with the two controls above. Its message
check asserts only the `0x8007000B` numeral: the prose around it is the CLR's localisable HRESULT
text, so a machine with a non-English UI culture would report different words for the same failure,
and a test on the whole string would depend on the machine that ran it. All three mutants die at a
named check — removing the abstract branch (check 2, via the `BUG:` invariant), raising the wrong
exception type (check 2), and using the parameterless constructor's message instead of the CLR's
(check 4).

Two comments were falsified by this and are part of the diff: `MethodBody.Abstract`'s docstring said
an abstract body is "reachable only via mis-resolved `callvirt`", and the `BUG:` message said virtual
dispatch was the only thing that should have prevented it.
