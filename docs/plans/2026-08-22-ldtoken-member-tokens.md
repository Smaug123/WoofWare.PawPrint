# Plan: `ldtoken` of a member token (MemberRef, MethodSpec)

## The blocker, measured

Rung H of the ASP.NET ladder (`app.MapGet` + materialise endpoints) fails with:

```
Unexpected metadata token MemberReference ... in LdToken
  Guest was: thread 0 (Runnable) in Microsoft.AspNetCore.Http.Extensions.RequestDelegateFactory..cctor
  at IL offset 2329, called 4 frames out from RungH.Program.Main
```

`UnaryMetadataTokenOps.executeLdtoken` (`WoofWare.PawPrint/UnaryMetadataTokenOps.fs:445-548`) decodes
`FieldDefinition`, `MethodDef`, `TypeSpecification`, `TypeReference` and `TypeDefinition`, and drops
everything else into a catch-all `failwith` at line 544.

IL offset 2329 = 0x919 in that cctor is (dumped with `WoofWare.PawPrint.IlDump`):

```
IL_0917: Nullary LdNull
IL_0918: Nullary LdNull
IL_0919: Ldtoken   System.Threading.Tasks.Task::get_CompletedTask() : ref[System.Threading.Tasks.Task]
IL_091E: Call      System.Reflection.MethodBase::GetMethodFromHandle(ref[System.RuntimeMethodHandle])
IL_0923: Castclass System.Reflection.MethodInfo
IL_0928: Call      System.Linq.Expressions.Expression::Property(...)
```

so the token is a `MemberReference` naming a *method* on a *non-generic* type in *another* assembly.

Two facts worth having up front, both measured rather than assumed:

* The four earlier `ldtoken`s of methods in the same cctor (`RequestDelegateFactory/Log::ParameterBindingFailed`
  and three siblings, at IL_047A/053E/05D7/0689) are `MethodDef` tokens and already execute. The gap is
  specifically the *reference* form, not "ldtoken of a method".
* **The consumer already works.** A guest that obtains `typeof(Task).GetProperty("CompletedTask").GetGetMethod().MethodHandle`
  reflectively and round-trips it through the 1-arg `MethodBase.GetMethodFromHandle`, asserts
  `ReferenceEquals` with the original `MethodInfo`, and casts to `MethodInfo`, exits 42 on real .NET
  **and 42 under PawPrint today** (`$scratch/gmfh`). So this instruction is the only thing between
  rung H and the next unknown; the fix does not drag `GetMethodFromHandle` in with it.
  The 2-arg overloads C# emits for a constructed declaring type — `MethodBase.GetMethodFromHandle(h, th)`
  and `FieldInfo.GetFieldFromHandle(h, th)` — were measured separately over `Box<int>`, `List<int>::Add`
  and `string.Empty`, and also exit 42 under PawPrint today (`$scratch/gmfh2`).

## The token space

Roslyn probe (`$scratch/ldtokprobe`, one expression tree per shape, exits 42 on real .NET), dumped:

| token | parent | member | supported today |
| --- | --- | --- | --- |
| `FieldDefinition` | — | field | yes, if the declaring type is non-generic |
| `MethodDef` | — | method | yes, if neither the declaring type nor the method is generic |
| `MemberReference` | `TypeReference` | method | **no** ← rung H |
| `MemberReference` | `TypeReference` | field | **no** |
| `MemberReference` | `TypeSpecification` | method | **no** |
| `MemberReference` | `TypeSpecification` | field | **no** |
| `MethodSpecification`(`MethodDef`) | — | method | **no** |
| `MethodSpecification`(`MemberReference`) | — | method | **no** |

All six missing shapes come out of ordinary C#: an expression tree over a cross-assembly member,
a member of a constructed generic type, or a generic method call. The probe guest reaches the
first of them at IL offset 1 and dies there, so everything past it is currently unmeasured.

C# folds `int.MaxValue`, so a `MemberReference`/`TypeReference`/*field* did not appear in the probe;
it is reachable through a non-`const` static (`string.Empty`) and is included below because the
same arm answers it.

## Options

### Option A — add the arms inline to `executeLdtoken`

Add `MemberReference` and `MethodSpecification` cases to the existing `match`, calling
`IlMachineState.resolveMember` (which already returns `Choice<MethodInfo, FieldInfo>` and the
parent TypeSpec's extracted type arguments) and then `getOrAllocateMethod` / `getOrAllocateField`.

* Blast radius: one function. Nothing existing changes behaviour. Maximally reversible.
* Cost: `executeLdtoken` becomes a *fifth* in-tree decoder of "member token against a generic
  context". The other four are `resolveMethodPointerTarget` (`ldftn`/`ldvirtftn`, same module),
  `UnaryMetadataCallOps` (`call`/`callvirt`/`newobj`), `UnaryMetadataFieldOps.resolveFieldToken`
  (`ldfld`/`ldsfld`/…), and `NativeRuntimeTypeQCall.ResolveMethodHandle` (`Module.ResolveMethod`).

### Option B — parse the operand into an `LdtokenTarget`, then push  ← recommended

Introduce one internal DU naming the things a `Runtime*Handle` can stand for, and one total
function from operand to it:

```fsharp
[<RequireQualifiedAccess>]
type internal LdtokenTarget =
    | Type of RuntimeTypeHandleTarget
    | Method of MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    | Field of declaringType : RuntimeTypeHandleTarget * FieldDefinitionHandle
```

`executeLdtoken` becomes "classify, push, advance": three push helpers, one per arm, and every
refusal stated once in the classifier rather than scattered through the `match`.

* This is "parse, don't validate" applied to the operand. The token space is decoded exactly once,
  and what downstream code holds is a proof of which kind it got, not a token to re-inspect.
* It gives the two operand universes a single meeting point: `ResolvedLdtokenOperand.FromScope`
  already yields a `RuntimeTypeHandleTarget`, i.e. `LdtokenTarget.Type`.
* Blast radius is one function plus one internal type; `executeLdtoken` shrinks.
* It leaves the shape ready for Option C without paying for it now.

Note what the `Field` arm does **not** carry: an assembly name. See risk 1.

### Option C — unify now: one member-token resolver across all five call sites

* Largest information gain; rejected for now, and the reason is sharper than "it's big" — see the
  CoreCLR asymmetry under risk 2. Two of the five callers resolve `MethodDef` *without* a generic
  context and three resolve their tokens *with* one; a single resolver would have to re-encode that
  as a parameter, which is exactly the distinction most likely to be smeared by a merge. It also
  touches `UnaryMetadataCallOps` and a QCall, both heavily tested: more than one feature at a time.

### Option D (rejected) — narrow the operand in `UnaryMetadataIlOpContext`

`TypeOperand`/`FieldOperand` narrowing already lives there, so a `ResolvedLdtokenOperand` with
`MetadataType | MetadataMethod | MetadataField` arms looks symmetric.

Rejected, but *not* because the kind is unknowable without resolution — it is knowable.
`MemberReference.Signature` is eagerly parsed into `MemberSignature.Field | MemberSignature.Method`
from the blob's leading byte (`WoofWare.PawPrint.Domain/MemberReference.fs:7-30`), and
`resolveMemberWithGenerics` branches on it before resolving anything. The kind is a pure projection.

The real reason is that the kind is not what the push helpers need. They need the *resolved payload* —
which member, at which instantiation — and that needs `IlMachineState` threading, which the context
members (pure projections, no state) cannot do. So D buys a three-way label and leaves the resolution
in `executeLdtoken` anyway. B subsumes it: B's classifier is D's narrowing plus the payload.

## Scope

**In:** all six missing rows — `MemberReference` (method and field, `TypeReference` and
`TypeSpecification` parents) and `MethodSpecification` (over both parents).

`MethodSpecification` is in scope because it is the same feature, needs no new machinery
(`resolveMethodPointerTarget` already decodes both its parent forms for `ldftn`), and — decisively —
CoreCLR resolves it from the *same* generic context as `MemberReference` (risk 2). It is the same
change, not an adjacent one.

**Out, deliberately:** the existing `MethodDef` refusals for a generic declaring type and for a
generic method (`UnaryMetadataTokenOps.fs:496-503`), and the `FieldDefinition` arm's
generic-declaring-type refusal (`:466-473`). Also: any sharing with `UnaryMetadataCallOps` or
`NativeRuntimeTypeQCall` (Option C).

Worth noting for later: the generic-*method* half of the `MethodDef` refusal may now be
representable — `MethodHandle.MethodGenerics = []` beside a non-zero declared count is already the
typical-instantiation encoding (PR #1133). Separate change, own oracle work.

**Not an inconsistency:** the new `MemberReference`/`TypeSpecification` path resolves a declaring
type that is *closed* (the TypeSpec's arguments are substituted from the current frame, which is
always concretized), so it yields `RuntimeTypeHandleTarget.Closed`. The retained `MethodDef` refusals
are about the *open* / typical form. The two do not overlap, and for the forms both spellings support,
both routes key on the defining assembly's concretized method — the same registry key. So there is no
surface where one method has two handle identities depending on token spelling.

## Implementation sketch (Option B)

In `UnaryMetadataTokenOps`, above `executeLdtoken`:

* `LdtokenTarget` as above.
* `classifyLdtokenOperand : UnaryMetadataIlOpContext -> IlMachineState -> IlMachineState * LdtokenTarget`.
  * `FromScope target` → `Type target`.
  * `TypeDefinition` / `TypeReference` / `TypeSpecification` → `Type`, via the existing
    `runtimeTypeHandleTargetForTypeToken`, **including its `allowOpenGenericDefinition` flag exactly
    as it stands** (`false` for TypeSpec, `true` for the other two). That flag is not incidental: it
    is PawPrint's spelling of CoreCLR's `PermitUninstDefOrRef`, which `resolveToken` grants to
    `ldtoken` alone (`jitinterface.cpp:986-993`, and the `ClassLoader::PermitUninstDefOrRef` ternary
    at `:1000`).
  * `FieldDefinition` → `Field`, as today. `MethodDef` → `Method`, as today, refusals intact.
  * `MemberReference h` → `IlMachineState.resolveMember`, then branch on the `Choice`:
    * method → `ExecutionConcretization.concretizeMethodForExecution ... None (Some extractedTypeArgs)`,
      the same call `resolveMethodPointerTarget` makes for `ldftn`;
    * field → design this arm against `UnaryMetadataFieldOps.resolveFieldToken`, which already decodes
      `MemberReference → field` for `ldfld`/`ldsfld` and whose docstring is the codebase's canonical
      statement of the defining-vs-referencing assembly rule. Reuse it or diverge deliberately, but do
      not reimplement it in ignorance. `ExecutionConcretization.concretizeFieldForExecution` returns the
      declaring type's `ConcreteTypeHandle` directly.
  * `MethodSpecification h` → factor `resolveMethodPointerTarget`'s **`MemberReference` and
    `MethodSpecification` arms only** into a shared `resolveMethodToken`. The `MethodDef` arms stay
    separate — see risk 2. `ldftn`'s "tried to ldftn a field" refusal stays on the `ldftn` side, since
    `ldtoken` accepts what it refuses.
* `executeLdtoken` = classify, push, `advanceProgramCounter`.

## Risks to get right

1. **The field registry's assembly name is denormalised — remove it rather than test it.**
   `FieldHandle`'s key is `{ AssemblyFullName; DeclaringType : RuntimeTypeHandleTarget; FieldHandle }`
   (`FieldHandleRegistry.fs:7-21`), and `getOrAllocate` permits only `Closed` and
   `OpenGenericTypeDefinition` (`:88-101`). Both pin the defining assembly already: `Closed` through
   its `ConcreteTypeHandle` (the registry is handed `allConcreteTypes` and can look it up, exactly as
   `resolveFieldToken`'s `FromScope` arm does), `OpenGenericTypeDefinition` through the
   `ResolvedTypeIdentity` it carries. The string can therefore only agree with the target or be a bug.
   Contrast the method side, which takes no assembly parameter at all — `makeMethodHandle` derives it
   from the concretized `MethodInfo`, which is why this risk has no method-side twin.

   There are two production callers (`UnaryMetadataTokenOps.fs:489`, `NativeRuntimeTypeHelpers.fs:175`),
   so **derive the assembly inside `FieldHandleRegistry.getOrAllocate` and drop the parameter**, which
   also drops the `declaringAssembly` component from `LdtokenTarget.Field`. That turns "the single most
   likely bug in this change" into something unrepresentable rather than something a mutant must catch.
   Fallback if the lookup turns out to be partial in some arm: assert agreement at the same chokepoint.
   Separable from the rest of the change if review prefers it landed first.

2. **`ldftn` and `ldtoken` disagree on the `MethodDef` arm, so the shared factoring must not cover it.**
   Measured in the pinned source: `CEEInfo::resolveToken` resolves `mdtMethodDef` via
   `MemberLoader::GetMethodDescFromMethodDef(pModule, metaTOK, ...)` and `mdtFieldDef` via
   `GetFieldDescFromFieldDef` — **neither takes a `SigTypeContext`** (`jitinterface.cpp:1010-1026`).
   `mdtMemberRef`, `mdtMethodSpec` and `mdtTypeSpec` each call
   `GetTypeContext(pResolvedToken->tokenContext, &typeContext)` first (`:1028-1064`).

   So the enclosing frame's generics are the correct substitution source for exactly the three token
   kinds this change adds, and are *not* a correct source for the two arms it leaves alone. That is a
   much stronger justification for the scope boundary than "no C# compiler emits it", and it is the
   reason a shared `resolveMethodToken` must be scoped to MemberRef + MethodSpec. A factoring that
   let `ldtoken`'s `MethodDef` route through `ldftn`'s frame-substituting arm would produce a *wrong
   handle identity*, silently.

   Related, and worth one line in the code: `MethodSpec(MethodDef)` whose MethodDef has a generic
   declaring type falls back to the frame's `DeclaringTypeGenerics`. C# never emits that shape, and it
   is pre-existing in `ldftn` — but `ldtoken` newly exposes it, and it fails by being wrong rather than
   by throwing.

3. **Identity dedup.** `MethodHandleRegistry`'s key is
   `{ AssemblyFullName; DeclaringType; MethodDefinition; MethodGenerics }`, derived from the concretized
   `MethodInfo`. Two routes to one method (this `ldtoken`, and `GetMethod(...).MethodHandle`) must land
   on the same key. Assert it directly, not by inferring it from a guest passing.

4. **`ldtoken` must stay atomic, and must not initialise.** `executeLdtoken` advances the PC
   unconditionally, so a resolution path that could suspend would re-execute the instruction.
   `concretizeMethodForExecution`, `concretizeFieldForExecution` and `resolveMember` all return plain
   state with no `WhatWeDid`; `ldftn` already relies on this. On initialisation, measured: `CEE_LDTOKEN`
   in `importer.cpp:10264-10303` resolves the token and wraps it in the three `*_TO_STUB*` helpers with
   no `initClass`, unlike static-field access. (Module *activation* happens; type initialisation does
   not — and `jitinterface.cpp:1085` says the module part is "done for backward compatibility only".)

5. **`resolveMember`'s parent refusal.** `| parent -> failwith $"Unexpected: {parent}"` is newly
   reachable through `ldtoken` and names neither the opcode nor the shape. ECMA-335 II.22.25 permits
   `TypeDef`, `TypeRef`, `TypeSpec`, `ModuleRef` and `MethodDef` parents; the improved message should
   name all of them. Fold in one more unnamed failure while there: a MemberRef whose `TypeReference`
   parent names a generic type *definition* dies at `targetType.Generics.[par.SequenceNumber]` with a
   bare `IndexOutOfRange`.

## Tests, written before the fix

`sourcesPure` guests are differential and **exit 0 on success** (`TestPureCases.fs:953`,
`Option.defaultValue 0`), with the real runtime's code compared against PawPrint's. So: 0 = pass,
1..N = index of the first failing check, numbered from 1. (Not 42 — that convention belongs to the
standalone ladder guests, which are not run this way.)

**Property test — enumerate the whole space.** The token space here is finite and in hand: sweep
*every* `MemberReference` row of a compiled test assembly rather than five hand-picked tokens.
Classify each; for methods assert registry-key equality against the method concretized directly from
its defining assembly, for fields assert the recorded assembly is `field.DeclaringType.AssemblyFullName`,
and assert dedup on a second call. This kills the wrong-assembly and wrong-context mutants across the
whole space at once. It must **not** be phrased as "ldtoken agrees with ldftn": after the factoring
that is a mirror oracle sharing its own mistakes. Compare against the defining-assembly direct route.

**Unit (narrow, no expression-tree stack).** `TestMethodHandleRegistry.fs` already has
`loadAssemblyFromSource` / `installFrameForMethod` / direct `UnaryMetadataIlOp.execute`, and three
`Ldtoken` tests built on them. Locate the `MemberReferenceHandle` by name **and parent kind and
signature** — common names (`Add`, `get_Item`) have several rows, and a silently-wrong row makes the
test assert nothing. Cases beyond the sweep:

* **The differing-instantiation fixture.** For the TypeSpec-parent cases, install the frame on
  `G<string>` and have the token name a member of `G<int>`. With a non-generic frame the
  drop-`extractedTypeArgs` mutant dies by arity exception, which proves less; with a frame that
  happens to match the TypeSpec it survives entirely.
* **MethodSpec whose argument is a caller generic parameter**, with the frame installed for a generic
  method at a concrete instantiation — so a wrong context yields a wrong element rather than passing.
  "`MethodGenerics` is non-empty" is satisfiable by a literal-`int` spec that exercises no substitution.
* Two different members of one type yield different addresses, so the dedup assertion is not vacuous.

**Guest (`sourcesPure`).** One expression tree per row, asserting `ReferenceEquals` between the tree's
`.Method`/`.Member` and the same member obtained via `GetMethod`/`GetField`, plus `DeclaringType` for
the TypeSpec-parent rows. No `Compile()` — that pulls in Reflection.Emit and is a different feature.

The consumer side of these rows is **measured, not assumed**: a guest reaching the 2-arg
`MethodBase.GetMethodFromHandle(h, th)` and `FieldInfo.GetFieldFromHandle(h, th)` reflectively — the
overloads C# emits for every TypeSpec-parent row — over `Box<int>`, `List<int>::Add` and
`string.Empty` exits 42 on real .NET **and 42 under PawPrint today** (`$scratch/gmfh2`). So the
TypeSpec rows are not expected to park on their consumer.

**Mutation testing** (per the `mutation-testing` skill) on at least: the `extractedTypeArgs` passed to
`concretizeMethodForExecution` (drop it → the differing-instantiation cases must fail), the
`allowOpenGenericDefinition` flags carried over unchanged, and — if risk 1 is *not* made structural —
the defining-assembly argument.

**Re-measure rung H** after the fix, and report where it goes next.

---

# Outcome

## What the implementation actually did

Option B as planned, plus one thing the plan did not anticipate and one it got wrong.

**Anticipated and done:** `LdtokenTarget` classifier; `MemberReference` and `MethodSpecification`
decoding shared with `ldftn` and *not* extended to `MethodDef`; `FieldHandleRegistry` derives the
defining assembly instead of taking it (risk 1 made unrepresentable rather than tested).

**Not anticipated:** a *bare* `MemberReference` naming a generic method is the typical-instantiation
form, exactly as a bare `MethodDef` is, and needed its own refusal. Before it, that row failed inside
concretization with "Generic method parameter 0", naming neither token nor shape. The whole-space
sweep found it; none of the hand-picked cases did.

**Got wrong:** the plan proposed mutation-testing "the `allowOpenGenericDefinition` flags carried
over unchanged". Two of the three arms are killable; the third is not, for a reason worth recording
(below).

## Mutation results

Full suite (4286 tests) unless noted.

| # | mutation | outcome |
| --- | --- | --- |
| M1 | drop `extractedTypeArgs` on the MemberReference arm | killed — 4, incl. the TypeSpec-parent tests and the guest |
| M2 | `definingAssemblyOf` returns a wrong assembly | killed — 20 |
| M3 | MethodSpec: drop the spec's method generics | killed — 4, incl. both MethodSpec tests |
| M4 | MethodSpec: drop the parent TypeSpec's arguments | killed — the guest |
| M5 | `TypeSpecification` arm: allow open definitions | killed — `TypeOpenGenericDefinitionInGenericContext.cs` |
| M6 | `TypeReference` arm: forbid open definitions | **survived** — see below |
| M7 | accept a bare MemberReference to a generic method | killed — the sweep test |
| M8 | `TypeDefinition` arm: forbid open definitions | killed — 7 |

M5/M6/M8 were first run under a 51-test filter, where M5 *also* looked like a survivor. It is not;
the narrow filter was hiding its only killer. Numbers above are from the full suite.

## The surviving mutant is a pre-existing divergence

`allowOpenGenericDefinition` only decides anything once the token's placeholder arguments are bound
by the enclosing frame — unbound, `runtimeTypeHandleTargetForTypeToken` reaches the same target
through its `containsUnboundGenericParameter` path either way. Two wrong guesses were measured
before that was established: the checks were first written in `Main` (placeholders unbound) and then
in a generic *method* (still unbound — `typeof(List<>)` carries a *declaring-type* parameter, which
only a generic declaring type binds). Instrumenting the arm directly settled it.

With them bound, `typeof(List<>)` — cross-assembly, so a `TypeReference` token — comes back *closed
at the enclosing instantiation* rather than as the open definition. Real .NET disagrees, and `main`
diverges identically, so this is neither caused nor fixed here. Parked as
`sourcesPure/TypeOpenGenericDefinitionCrossAssembly.cs`, whose comment records that un-parking it is
what would make M6 killable.

The code comment on that arm now says the flag is *unobserved* rather than claiming it is
load-bearing. An earlier revision of this branch claimed the latter, which the mutant falsified.

## Rung H

Advances from 4 frames out to **20**, and now stops at `ModuleHandle.ResolveMethod` refusing a
`MethodDef` token on a generic declaring type — the sibling decoder named under Option C, and the
same open-generic `RuntimeMethodHandle` work this change scopes out.

## Guest scope, cut back as the plan allowed for

The `sourcesPure` guest carries no *field* shapes. The `ldtoken` half works and the unit tests assert
it, but every route from a `RuntimeFieldHandle` back to a `FieldInfo` runs `RuntimeType.GetFieldInfo`,
which reaches the unimplemented `RuntimeFieldHandle.AcquiresContextFromThis` whenever the field
handle is the first thing to materialise its declaring type. Measured to be independent of this
change: a `FieldDefinition` `ldtoken` of a same-assembly static stops in the same place on `main`.

## Review

Codex raised one finding (P2), which is the same latent hazard Fable named when reviewing the plan:
`MethodSpec(MethodDef)` whose MethodDef is declared on a *generic* type. The spec binds the method's
generics and nothing binds the declaring type's, so `resolveTargetTypeGenerics` fell back to the
executing frame's `DeclaringTypeGenerics` — producing a handle for *the caller's* instantiation where
the token means the typical one. A wrong answer rather than a failure, so it is now refused, exactly
as the adjacent bare-`MethodDef` arm is.

The plan recorded this as "worth one line in the code" and then did not act on it; two independent
reviewers finding it is what turned that into a fix. No compiler emits the shape — a generic method
on a generic type is referenced through a MemberReference with a TypeSpec parent (dumped) — so the
test builds the row directly, as hand-written IL would spell it, by pointing an existing MethodSpec
at the MethodDef. Building it required loading the fixture through a transform: `WithLoadedAssembly`
keeps whichever instance is already held for an identity, so handing a modified copy to a built state
is silently a no-op. Mutating the guard away (M9) kills exactly that test.
