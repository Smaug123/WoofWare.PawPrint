# `Signature_Init` against a generic method definition

Plan for rung H's blocker: the last non-socket item on the ASP.NET ladder.

## The measured symptom

```
TODO: Signature_Init on generic method definition GetRequiredService: it declares 1 generic
parameter but the handle carries 0 generic argument(s); CoreCLR resolves the signature against
the typical instantiation, whose method generic parameters PawPrint's ConcreteTypeHandle
cannot represent
```

Rung H reaches it 19 frames out, through DI resolution during endpoint materialisation. But it
is not an ASP.NET shape at all. Measured: a non-generic static class with one generic method
reproduces it in 7 frames, with no framework involved beyond CoreLib.

```csharp
public static class Holder { public static T Echo<T> (T value) => value; }
// typeof(Holder).GetMethod("Echo").GetParameters() -> the refusal above
```

That guest exits 42 on real .NET. It belongs in `sourcesPure`, and it — not rung H — should be
the thing driven to green.

## What is actually missing

Not a representation. **The refusal is one arity check standing in front of a walk that already
handles the case.**

`NativeRuntimeTypeHelpers.reflectedTypeTarget`, added by #1110, resolves a signature element
against a `ReflectionTypeEnvironment` that already has *two* axes:

```fsharp
type ReflectionTypeEnvironment =
    { TypeVariables : ImmutableArray<RuntimeTypeHandleTarget>
      MethodVariables : ImmutableArray<RuntimeTypeHandleTarget> }
```

and it already has a `TypeDefn.GenericMethodParameter index` arm that reads `MethodVariables`.
`RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position)`
already exists and is documented as what reflection surfaces as a `RuntimeType` with
`DeclaringMethod` non-null. Nothing new needs to be *representable*.

What is missing is that `methodSignatureTypeContext` refuses before any of that runs, and that
`fillMethodSignature`'s closed-declaring-type branch calls `concretizeType` — which cannot
express a formal — rather than the walk.

### The previous revision of the ladder report got this wrong

`docs/plans/2026-08-17-aspnet-critical-path.md` files rung H and
`sourcesPure/MakeGenericMethodOpenArgument.cs` together, as one "method formals" change needing
`MethodHandle.MethodGenerics` widened beyond `ConcreteTypeHandle list`. That is right for
`MakeGenericMethodOpenArgument` and **wrong for rung H**. The two are different problems:

| | what the handle carries | what is needed |
| --- | --- | --- |
| rung H | `MethodGenerics = []` — already the encoding of the typical instantiation | derive the formals from the MethodDef row |
| `MakeGenericMethodOpenArgument.cs` | one *bound* argument that is not closed | a wider `MethodGenerics` |

Empty `MethodGenerics` is not a gap; it is load-bearing. `isGenericMethodDefinition` is exactly
`methodGenericParamCount > 0 && handleInstantiationCount = 0`, and it backs the guest-visible
`RuntimeMethodHandle.IsGenericMethodDefinition`. Storing the formals in `MethodGenerics` would
make that count 1 and flip the predicate to `false` for every generic method definition.

## Measured ground truth

`typeof(Box<>).GetMethod("Mix").GetGenericArguments()[0]` and
`typeof(Box<int>).GetMethod("Mix").GetGenericArguments()[0]` are **reference-equal** on real
.NET, both with `DeclaringType = Box<T>` and `DeclaringMethod.ReflectedType = Box<T>`. The
parameter types compare equal too.

So the target must be keyed on the **definition's** identity, never the instantiation's —
`methodInfo.TryDeclaringType.Identity`, not the closed declaring handle. `TypeHandleRegistry`
keys guest `Type` object identity on the target, so getting this wrong would hand the guest two
`Type` objects where .NET has one.

## The shape that constrains the design

`RuntimeTypeHandleTarget.openConstructed` **refuses an all-closed argument list** — such a type
must be a `Closed` handle, and the walk cannot mint one. So an element whose variables all
resolve closed must go through `concretizeType`, not the walk.

That never bit before, because both existing callers have an all-formal type axis (the
constraints path, and the signature path's `Definition` branch). The method axis introduces a
genuinely **mixed** environment for the first time:

```csharp
public class Box<T> { public U Mix<U> (List<T> a, U b) => b; }
// under Box<int>: `List<!0>` resolves fully closed, while `!!0` stays formal
```

`List<!0>` under `Box<int>` → arguments `[Closed int]` → all closed → `openConstructed` throws.
Measured on real .NET: that parameter reflects as exactly `typeof(List<int>)`.

Four shapes, all measured to exit 42 on real .NET
(`scratchpad/probe-genmeth`), and all reachable only behind shape 1 today:

1. non-generic declaring type + generic method definition — *rung H's shape*
2. **closed** generic declaring type + generic method definition, element closing fully
3. **open** generic declaring type + generic method definition — both axes formal
4. only the type axis varies, method axis still a definition

## Options

### Option 1 — decide per element at the call site

Add `MethodContext = Instantiation of ImmutableArray<ConcreteTypeHandle> | Definition`, mirroring
the existing `DeclaringTypeContext`. In `fillMethodSignature`'s `resolve`, pick a path per
element: if the element mentions no `!!i` and the type axis is closed, `concretizeType` as
today; otherwise build the environment and call the walk.

- **For**: no representation change; no dedup-key change; `isGenericMethodDefinition` untouched;
  diff confined to `NativeSignature.fs`.
- **Against**: the caller now owns a predicate ("does this element mention a method formal, and
  does every type variable it mentions resolve closed?") that must agree exactly with the walk's
  own internal fast path. Two places encoding one rule. Get it wrong and you get either an
  `openConstructed` crash or — worse — a non-canonical target that `assertWellFormed` may or may
  not catch.

### Option 2 — make the walk total (recommended)

Generalise `reflectedTypeTarget`'s existing fast path. It currently reads:

> if the element mentions no generic parameter at all, concretize it and wrap in `Closed`

which is the special case of the real rule:

> if every variable the element mentions resolves to a **`Closed`** target, concretize it against
> those closed handles and wrap in `Closed`; otherwise walk structurally

`concretizeType` already takes both a type-generic and a method-generic
`ImmutableArray<ConcreteTypeHandle>`, so when the rule fires the vectors are exactly the
environment's closed entries. The caller then always calls the walk, and `DeclaringTypeContext`'s
two arms collapse into one environment-building step.

- **For**: one place knows the closed-vs-open rule, and it is the place that already has to know
  it. The "which path" question disappears rather than gaining a third case. A future third axis
  does not re-open it. Matches the repo's "make walks total rather than adding projection
  helpers", and avoids the duplicated-classifier failure mode.
- **Against**: changes a function #1110 added three commits ago, with two callers. The
  constraints path never needs the collapse (its type axis is all-formal), so it must be checked
  for no behaviour change rather than assumed — its arguments never all resolve closed today,
  which is exactly the invariant the change would relax.

### Option 3 — widen `MethodHandle.MethodGenerics`

Carry the formals in the handle, as the ladder report currently proposes.

- **For**: one representation would serve both this and `MakeGenericMethodOpenArgument.cs`.
- **Against**: it solves a problem rung H does not have. It changes the dedup key of
  `MethodHandleToId` (so guest `RuntimeMethodHandle` identity moves), forces a rewrite of
  `isGenericMethodDefinition`'s encoding, and touches all eight `GetMethodGenerics` consumers —
  to store information that is *derivable* from the MethodDef row the handle already names.
  **Not recommended for this piece**; it remains the right change for
  `MakeGenericMethodOpenArgument.cs`, separately.

## Chosen: Option 2

Agreed with Patrick before any code was written, with the guest above as the driving test.

Option 1 and 2 produce identical guest-visible behaviour; they differ only in where the rule
lives. The reason to prefer 2 is that this cycle already demonstrated the cost of the rule living
in the caller: the caller's assumption ("a method declaring none cannot spell `!!i`") is written
down in `NativeSignature.fs` as a comment justifying an empty `MethodVariables`, and it is
exactly what the arity check upstream was silently guaranteeing.

## What was built

`sourcesPure/ReflectionGenericMethodDefinitionSignature.cs`, 46 checks over all four shapes,
verified to exit 0 on real .NET before any interpreter change and observed to fail at the
refusal first.

`ReflectionVariableBinding` splits each axis of `ReflectionTypeEnvironment` into `Bound` (an
instantiation's runtime types) and `Formal` (a definition's own variables). The walk's fast path
generalises from "mentions no variable" to "every variable it mentions is on a `Bound` axis", and
concretizes against those handles; `mentionedAxes` answers both axes from one walk so the two
cannot drift. `methodSignatureTypeContext` returns a `MethodGenericContext` in place of the arity
refusal, and `fillMethodSignature` takes one path for all four combinations.

### Mutation results

Three mutants, each killed by a different check, so the guest is not vacuous:

| mutant | killed at |
| --- | --- |
| method variable always answers position 0 | check 10 |
| method axis answers with a *type* variable (axes conflated) | check 3 |
| a `Bound` axis no longer counts as closed | crash: `openConstructed` all-closed refusal |

The third is the hazard the fast path exists to avoid, confirmed by running rather than by
reading.

### Rung H, re-measured

The motivating rung moves off this blocker entirely. At `619eece3` it failed at `Signature_Init`
19 frames out; with this change it reaches `LdToken` on a `MemberReference` token, 4 frames out —
an IL-decoding gap with nothing to do with generics. So the reflection half of rung H is done and
what remains there is a different piece of work.

One caution, learned the hard way here: the first rung-H run after this change appeared to crash
on `openConstructed`'s all-closed refusal, which looked exactly like a hole in the new fast path.
It was a stale binary. The mutation runs above rebuild the interpreter in place, and the last one
— "a `Bound` axis no longer counts as closed" — *produces that very crash*; restoring the source
does not rebuild it. Rebuild before drawing a conclusion from a run that follows a mutation.

### Correction found while mutating

An earlier draft of this plan said keying the method variable on the *instantiation* would mint
two `Type` objects. That mutant is unwritable: `ResolvedTypeIdentity` is (assembly, TypeDef row)
and cannot express an instantiation, so the two routes agree by construction. The comment in the
code says that now, rather than claiming a hazard it does not have.


## Out of scope

- `MakeGenericMethodOpenArgument.cs` and any widening of `MethodHandle.MethodGenerics`.
- Arrays/byrefs/pointers over a variable (`ReflectionOpenGenericDefinitionElementTypes.cs`),
  which the walk still refuses by naming the shape. A generic method whose parameter is `U[]`
  hits that refusal rather than this one.
- `Type.DeclaringMethod` on a method variable, which is what the guest would otherwise use to
  tell the two axes apart. Measured: it reaches
  `RuntimeTypeHandle.GetDeclaringMethodForGenericParameter`, which needs an `IRuntimeMethodInfo`
  for the declaring method and is unimplemented. The guest distinguishes the axes by reference
  equality against `GetGenericArguments()` instead.
- `Signature_GetCustomModifiersAtOffset` on a generic method definition. It concretizes a
  modifier and so wants runtime types, which the typical instantiation does not supply; it now
  refuses on the method axis exactly as it already did on the declaring-type axis. Not a
  regression: the arity check refused that whole path before.
