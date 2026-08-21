# `Signature_Init` under an open generic type definition

Status: plan. Slice selected by re-measuring the ASP.NET ladder's rung E
(`Expression.Lambda(...).Compile()`) against `main` at `f6c33fd1`.

## What is measured

Rung E still fails at

```
TODO: Signature_Init on a method declared by open generic type definition ...
  thread 0 in System.Private.CoreLib.Signature.Init, 17 frames out from Main
```

A probe printing the declaring type names it: **`System.Nullable`1..ctor`, with the
declaring type the open definition rather than an instantiation**. That is the shape
`sourcesPure/ReflectionOpenGenericNullableConstructor.cs` already names in a comment —
`System.Dynamic.Utils.TypeUtils`' static initialiser runs
`typeof(Nullable<>).GetConstructor(typeof(Nullable<>).GetGenericArguments())`, and
filtering candidate constructors by parameter type decodes the ctor's MethodSig under
the definition's formal type context. That file deliberately asks `GetConstructors()`
instead, so this gap is the one thing between it and the `GetConstructor(Type[])`
overload.

**Stubbing forward measures the rest of rung E**, rather than guessing at it. With the
type-formal context supplied (probe quality, thrown away), rung E walks past
`Signature_Init` with no further open-definition signature element reached at all, and
stops at three further blockers in order:

| # | blocker | shape |
| --- | --- | --- |
| 1 | `RuntimeTypeHandle_IsCollectible` QCall | unimplemented native |
| 2 | `RuntimeMethodHandle_GetIsCollectible` QCall | unimplemented native |
| 3 | `Delegate_BindToMethodInfo` for a *metadata* method in `System.Linq.Expressions` | implemented only for a `Reflection.Emit`-minted method |

So rung E is at least four changes deep and this is the first of them. Only the first
is in scope here; the other three are separate features, and #1 and #2 are separate
from each other only in the sense that they are separate entry points (they would make
one small PR).

## The gap

`NativeSignature.methodSignatureTypeContext` builds the `SigTypeContext` a
method-backed `Signature` resolves its blob against, and asks
`NativeRuntimeMethodHandle.requireClosedDeclaringType` for the declaring type. Under an
open generic definition there is no closed instantiation, so it refuses.

CoreCLR does not refuse: `Signature_Init` builds the context from
`declType.GetClassOrArrayInstantiation()`, and for the typical instantiation that is the
definition's own type variables. `Nullable<>..ctor(!0)` reflects its one parameter as
the `RuntimeType` for `T` — `IsGenericParameter`, `DeclaringType == typeof(Nullable<>)`,
`GenericParameterPosition == 0`.

PawPrint can already *name* that type: `RuntimeTypeHandleTarget.GenericParameter
(definition, position)` is exactly it, and `RuntimeTypeHandle_GetInstantiation` on an
open definition already hands those very targets back, so `typeof(Nullable<>)
.GetGenericArguments()[0]` and a reflected parameter type will be the same `Type`
object — which is what the binder's comparison needs. What is missing is a walk from a
signature element (`TypeDefn`) to a target under a formal context.

## Options

### Option A — resolve signature elements to `RuntimeTypeHandleTarget`, not `ConcreteTypeHandle`

Give `Signature_Init` a *reflection* type context: one `RuntimeTypeHandleTarget` per
generic parameter of the declaring type, `Closed h` under an instantiation and
`GenericParameter (definition, i)` under a definition. Walk each signature element
against it, producing a target, and allocate the guest `RuntimeType` from that.

This is the walk `NativeRuntimeTypeHelpers.genericParameterConstraintTargets` already
performs for generic-parameter *constraints* — `where T : IComparable<T>` has the same
problem and the same answer — including the `RuntimeTypeHandleTarget.openConstructed`
canonicalisation that keeps guest `Type` identity single-valued.

- Blast radius: `NativeSignature`'s method-backed path, plus wherever the shared walk
  is extracted to. `ConcreteTypeHandle` is untouched, so nothing about closed
  instantiations changes.
- Preserves information: a parameter typed `List<!0>` comes back as
  `OpenConstructed (List<>, [GenericParameter (D, 0)])` rather than being flattened.
- Reversible: additive; the closed path keeps taking the same code it takes today.

### Option B — widen `ConcreteTypeHandle` to admit formal variables

Make `AllConcreteTypes` able to hold a type variable, so `concretizeType` can be handed
a formal context directly and `Signature_Init` needs no new walk.

- Blast radius: every consumer of `ConcreteTypeHandle` — the interpreter binds calls,
  lays out fields and allocates objects against these. A handle that no longer denotes
  a runtime type would have to be refused at each of those, and the type system would
  not point at them.
- This is the line `SubstitutionArgument` was introduced *not* to cross: identity
  (`ConcreteTypeHandle` names a runtime type) versus projection (what reflection
  surfaces). Rejected.

### Option C — keep the walk private to `NativeSignature`

Option A's context, but with a second copy of the element walk rather than sharing one
with the constraints path.

- Smaller diff, no refactor of a tested path.
- But the walk's job is *canonicalisation*, and two canonicalisers are how one type
  comes to have two `Type` objects. `TypeHandleRegistry` keys object identity on the
  target, so a divergence between the two walks is directly guest-visible.

**Choice: Option A**, with the shared walk extracted into `NativeRuntimeTypeHelpers`
and `genericParameterConstraintTargets` refactored onto it in the same change, so there
is exactly one `TypeDefn -> RuntimeTypeHandleTarget` walk in the tree. The constraints
path has dedicated pure tests (`TypeGetGenericParameterConstraints*.cs`), so the
refactor is checked rather than asserted.

## Shape of the change

The context is a two-case DU rather than a vector of targets, and that is what makes
the closed path unchanged *by construction* rather than by care:

```fsharp
type private DeclaringTypeContext =
    /// The declaring type is an instantiation: `!i` is a runtime type.
    | Instantiation of typeGenerics : ImmutableArray<ConcreteTypeHandle>
    /// The declaring type is the definition itself -- CoreCLR's typical instantiation.
    /// `!i` is that definition's own `i`th variable, so the vector is derivable and is
    /// not carried.
    | Definition of ResolvedTypeIdentity
```

A mixed vector cannot arise: `MethodHandleRegistry` mints a declaring type only as
`Closed` or `OpenGenericTypeDefinition`. Carrying one would also break the commonest
closed shape there is — a parameter typed `List<!0>` under `G<int>` has *every*
argument closed once `!0` resolves, and `RuntimeTypeHandleTarget.openConstructed`
refuses an all-closed argument list outright (ManagedPointerSource.fs:214).

1. `NativeRuntimeTypeHelpers` gains a `TypeDefn -> RuntimeTypeHandleTarget` walk over a
   *formal* environment, extracted from the inner recursion of
   `genericParameterConstraintTargets` (`constraintTarget`, `embedsTypeParameter`,
   `resolveDefinitionIdentity`) and parameterised by:
   - a type-variable environment `int -> RuntimeTypeHandleTarget`, bounds-checked;
   - an optional method-variable environment (`None` on the signature path: `!!i` in the
     signature of a method carrying no method generics is illegal metadata, so the loud
     failure stays);
   - the resolving assembly, and the operation plus owner description for messages.

   The constraints-specific tail — filtering Roslyn's synthetic `System.ValueType` row
   and the `where T : unmanaged` append — stays *out* of the shared core; that is where a
   careless extraction would regress constraints.

   Walk rules: an element mentioning no variable is concretized under empty generics and
   wrapped `Closed`, as today; `TypeDefn.Modified` is stripped, matching both reflection
   and `concretizeType`, which strips it already (TypeConcretisation.fs:1151); `!i` /
   `!!i` come from the environment; a `GenericInstantiation` mentioning a variable
   becomes `RuntimeTypeHandleTarget.openConstructed` over recursively-resolved arguments;
   an array, pointer, byref or function pointer *over* a variable is refused, naming the
   shape.

   Real .NET does answer that last group — measured: `T[]`, `T&` and `T*` each reflect
   with `GetElementType()` reference-equal to `T` — so it is a documented refusal rather
   than agreement. Giving the target DU structural cases would touch 75 match arms across
   34 files, which is the recognised follow-on rather than part of this change.
2. `NativeSignature.methodSignatureTypeContext` stops calling
   `requireClosedDeclaringType` and returns the DU above. Its existing refusal of a
   *structural* declaring type — the array `Get`/`Set`/`Address` methods CoreCLR resolves
   against `GetClassOrArrayInstantiation` — is kept: that arm is reached from the
   `Closed` case, not from `requireClosedDeclaringType`.
3. `fillMethodSignature` destructures the DU. `Instantiation` runs today's code verbatim.
   `Definition` resolves the return type and each parameter type through the walk and
   allocates the `RuntimeType` from the resulting target.
4. `Signature_GetCustomModifiersAtOffset`, the other consumer, keeps today's code on the
   `Instantiation` arm and refuses `Definition` with a TODO naming the condition. Not a
   regression: it fails today one call earlier, inside `methodSignatureTypeContext`. The
   refusal does become newly *reachable*, because such `Signature` objects will now exist.
5. `requireClosedDeclaringType` keeps its remaining caller
   (`NativeReflectionInvocation`, where invoking a method of a definition is genuinely
   impossible). Its docstring stops claiming `Signature_Init` needs it, and its failwith
   message stops claiming `RuntimeTypeHandle.GetNumVirtuals` lacks this capability —
   `VirtualSlotLayout.numVirtuals` has answered for an open definition since
   `numVirtualsOfDefinition` landed, so that clause is stale.

Out of scope, and still refused: **method** formals `!!i`. A generic method *definition*
is refused a few lines further on by `methodSignatureTypeContext`'s arity check, which is
rung H's slice and needs `MethodHandle.MethodGenerics` widened. Note the error-path
reorder: that shape fails at the arity check after this change rather than at
`requireClosedDeclaringType` before it. The arity check's message is the more precise of
the two.

One consequence worth stating, because it bounds what this buys: `Signature_Init` decodes
a whole signature at once, and `GetConstructor(Type[])` / `GetMethod(name, Type[])` build
a `Signature` for *every* candidate rather than only the match. So a single `out T` or
`T[]` overload anywhere in a candidate set refuses the whole query —
`typeof(Dictionary<,>).GetMethod("TryGetValue")` is the nearest casualty.

## Tests

`sourcesPure/ReflectionOpenGenericDefinitionParameterTypes.cs` (new, differential; exits
0 on real .NET, verified). Beyond the rung E shape itself —
`typeof(Nullable<>).GetConstructor(typeof(Nullable<>).GetGenericArguments())` — it covers,
on a user-declared definition `SignatureDefinition<T, U>`, and each fact below is measured
on real .NET rather than assumed:

- a parameter typed `T`: reference-equal to `GetGenericArguments()[0]`,
  `IsGenericParameter`, position 0, `DeclaringType` the definition. Reference equality,
  not `==`, is the load-bearing form: a second equal-but-distinct `Type` would break the
  identity .NET guarantees, and `GetConstructor(Type[])` finds nothing without it;
- a parameter typed `int`, the closed control in the same signature;
- a parameter typed `U`, so an implementation that always answers with the zeroth
  variable fails;
- `List<T>`: not a variable, `ContainsGenericParameters`, *not* a definition, generic type
  definition `List<>`, argument reference-equal to `T`;
- `List<List<T>>`, so a walk that maps only top-level arguments through the environment
  fails;
- `Dictionary<U, int>`, whose arguments are `[U; int]` in that order, so a walk that maps
  arguments positionally onto the definition's own formals fails;
- `SignatureDefinition<T, U>` itself, which real .NET collapses to `typeof(...<,>)` —
  reference-equal, `IsGenericTypeDefinition` — so the typical-instantiation collapse in
  `openConstructed` is pinned on a *signature* element and not only at the registry's
  well-formedness assertion;
- the return type `T`, so the return path is not skipped;
- `GetMethod(name, Type[])` on the definition, so candidate filtering under a formal
  context is exercised on the user type as well as through the Nullable ctor;
- the closed instantiation `SignatureDefinition<int, string>`, whose answers are `int` and
  are *not* reference-equal to the formals.

`sourcesPure/ReflectionOpenGenericDefinitionElementTypes.cs` (new, parked in
`unimplemented`): `T[]`, `ref T` and the return-type array, with the closed instantiation
as control. Exits 0 on real .NET, verified. Its parking comment records the refusal that
is actually measured, not a prediction of when it lifts.

Mutation targets, checked red before claiming coverage:

- index-swap in the type-variable environment -> the `U` checks;
- return type resolved by the old path -> the `ReturnsFormal` check;
- collapse skipped in `openConstructed` -> the `SignatureDefinition<T, U>` check;
- instantiation arguments mapped positionally rather than resolved -> the
  `Dictionary<U, int>` check;
- a misrouted variable environment in the *shared* walk -> the existing
  `TypeGetGenericParameterConstraints*.cs` files, which are what make the extraction
  checked rather than asserted.

No FsCheck harness here: the canonicalisation invariant is already machine-enforced at the
single chokepoint (`RuntimeTypeHandleTarget.assertWellFormed`, called from
`TypeHandleRegistry.getOrAllocate`), and a host-level generator over `TypeDefn` trees would
need a loaded `IlMachineState` for what enumerated guest shapes already pin.

## Outcome

Implemented. Measured after the change rather than predicted:

- rung E advances past `Signature_Init` to `RuntimeTypeHandle_IsCollectible`, the blocker the
  stub-forward run named, at the same frame depth;
- the new differential file passes all 33 checks under the interpreter, and the parked one
  refuses with "the signature of TakesArray names arr[<type param 0>], which embeds a generic
  parameter beneath an array, pointer, byref or function-pointer shape";
- all five mutants died. Two of them died at a crash rather than at the check they were aimed
  at, which is worth recording: `List<List<T>>` resolved only at top level reaches
  `concretizeType` with an unbound variable, and a skipped typical-instantiation collapse is
  caught by `RuntimeTypeHandleTarget.assertWellFormed` at the registry chokepoint before the
  `TakesSelf` check runs. The `TakesSelf` check still earns its place: the assertion pins the
  internal invariant, the check pins the differential fact that real .NET collapses too;
- the full suite is green (4054 tests).
