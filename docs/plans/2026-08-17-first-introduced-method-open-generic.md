# `GetFirstIntroducedMethod` for open generic type definitions

## What is broken

`RuntimeTypeHandle.GetFirstIntroducedMethod` refuses when its `RuntimeType` resolves to
`RuntimeTypeHandleTarget.OpenGenericTypeDefinition`:

```
TODO: RuntimeTypeHandle.GetFirstIntroducedMethod for open generic type definition <identity>;
need to walk metadata-level methods on the open type
```

Narrowest guest that reaches it — no expression trees, no BCL generics, one boring type:

```csharp
public sealed class Box<T> { public T Item; public Box() {} public Box(T t) { Item = t; } }
Type open = typeof(Box<>);
var ctors = open.GetConstructors();   // refuses
```

## Measured: which reflection queries reach what

All four rows are runs of the guest above against `origin/main` at `a0a794b9`, differing only
in the query on the last line.

| Query on `typeof(Box<>)` | Outcome |
| --- | --- |
| `IsGenericTypeDefinition`, `GetGenericArguments()` | already works |
| `GetFields()` | already works |
| `GetConstructors()` | **refuses in `GetFirstIntroducedMethod`** |
| `GetMethods()`, `GetProperties()` | refuses earlier, in `GetNumVirtuals` |

And the control, which is what makes the scope defensible:

| Control | Outcome |
| --- | --- |
| `typeof(Box<int>).GetConstructors()` | **passes** |

Two things follow.

**`GetConstructors()` is the entry point that isolates this work.** `PopulateConstructors`
(`RuntimeType.CoreCLR.cs:753`) iterates `RuntimeTypeHandle.GetIntroducedMethods` and then only
asks for `GetUtf8Name`, `GetAttributes` and `GetStubIfNeeded` — no vtable, because constructors
are never virtual. It never calls `GetNumVirtuals`. `GetMethods()` does, via
`GetMethodCandidates` allocating its `bool[numVirtuals]` overrides map.

**`GetNumVirtuals` for an open definition is a different and much larger job, and must stay
refusing.** `numVirtualsOfClosed` is defined as the length of `vtableOfClosed`, and building
that vtable requires matching overrides against base slots by signature. The comment at
`NativeRuntimeTypeHelpers.fs:882-888` already names this: the missing capability is a
*symbolic*, definition-level signature comparison, which `ConcreteTypeHandle` cannot express,
and `sourcesPure/ReflectionGenericVirtualMethodOverrideSlots.cs` is its parked case. This plan
does not touch it. `GetMethods()` on an open definition stays parked.

Because the closed control passes, the whole downstream chain — `GetUtf8NameInternal`,
`GetAttributes`, `GetStubIfNeededInternal`/`GetStubIfNeededSlow` — already exists and works.
The only gap is naming an open definition as a method's declaring type.

## The actual obstacle: `MetadataMethodIdentity` cannot name an open definition

```fsharp
type MetadataMethodIdentity =
    private { AssemblyFullName : string
              DeclaringType : ConcreteTypeHandle
              MethodDefinition : ComparableMethodDefinitionHandle
              MethodGenerics : ConcreteTypeHandle list }
```

`MethodHandleRegistry.MethodHandleToId` keys on this, so it *is* the guest-visible identity of
a `RuntimeMethodHandleInternal`. And `ConcreteTypeHandle` structurally cannot denote `Box<T>`
un-substituted: `concretizeType` handles `TypeDefn.GenericTypeParameter index` by indexing into
the supplied `typeGenerics` array (`TypeConcretisation.fs:980`), so there is no handle for a
free type variable and therefore none for the typical instantiation.

This is an identity question, not an observability one. Standing a closed instantiation in for
the open definition would make `Box<>`'s `.ctor` and `Box<int>`'s `.ctor` collide on one
registry id, where CoreCLR keeps distinct `MethodDesc*` for the typical instantiation and each
instantiation. `RuntimeTypeHandleTarget`'s own docs make exactly this argument for
`DynamicMethodsClass` — "an identity collision rather than an approximation".

## Options for the declaring-type representation

### Option A — widen `DeclaringType` to `RuntimeTypeHandleTarget`

Reuse the existing type-identity DU wholesale. `Closed of ConcreteTypeHandle` already exists,
so every current construction site becomes `RuntimeTypeHandleTarget.Closed h`.

- One vocabulary for type identity across the codebase, rather than two.
- `RuntimeMethodHandle.GetDeclaringType` (`NativeRuntimeMethodHandle.fs:1328`) currently
  converts via `methodTableOfDeclaringType`; that conversion disappears.
- Against it: it admits states that cannot exist. A metadata method declared on a
  `GenericParameter`, on an `OpenConstructed` type, or on `DynamicMethodsClass` are all
  representable and all nonsense — the last doubly so, since `MethodHandle.FromDynamic` is how
  a dynamic method is named. Seven `GetDeclaringType ()` sites would each need a refusal arm
  for cases the type system could have excluded instead.

### Option B — a narrow DU with exactly the legal cases

```fsharp
[<RequireQualifiedAccess>]
type MethodDeclaringType =
    /// A closed instantiation, or a non-generic type.
    | Closed of ConcreteTypeHandle
    /// The typical instantiation of a generic type definition: CoreCLR's canonical
    /// MethodTable for `Box<>`, which carries the definition's own TypeDef token.
    | OpenGenericDefinition of ResolvedTypeIdentity
```

A metadata-backed method's declaring type is exactly "a MethodTable-backed type carrying a
TypeDef token", and those are the two shapes that can be.

- Illegal states unrepresentable; the compiler enumerates all seven consumers.
- Against it: a second type-identity vocabulary, plus a `MethodDeclaringType ->
  RuntimeTypeHandleTarget` conversion for the `GetDeclaringType` FCall (total, two cases).

### Option C — a separate id space for open-definition methods

Rejected, recorded for completeness: keeping `MetadataMethodIdentity` narrow and minting
open-definition handles from a side table gives one logical method two registry ids depending
on which path asked, and fragments the `MethodHandleToId` dedup that exists to stop exactly
that.

### Recommendation

**Option B.** The blast radius is the same seven sites either way and is compiler-enumerated in
both; the difference is whether four impossible states are representable. Reversibility is
symmetric — B can be widened to A later by replacing the DU, whereas A cannot be narrowed
without re-deriving which cases were reachable.

## Implementation

1. Add `MethodDeclaringType` (Option B) and change `MetadataMethodIdentity.DeclaringType` to
   it. `GetDeclaringType ()` returns it.
2. `MethodHandleRegistry.makeOpenMethodHandle` gains an overload (or a parameter) that takes a
   `MethodDeclaringType` rather than resolving a `ConcreteType` through
   `findExistingConcreteType`.
3. Split `introducedMethodsOfClosed` into a dispatcher over `MethodDeclaringType`:
   - `Closed` → today's behaviour, unchanged.
   - `OpenGenericDefinition identity` → `Assembly.resolveTypeIdentityDefinition` on the
     defining assembly, then that `TypeInfo.Methods`, in metadata order. No concretization, no
     base-chain walk (introduced methods are the type's own; `PopulateConstructors` does not
     inherit).
4. `GetFirstIntroducedMethod`: replace the `OpenGenericTypeDefinition` refusal with a call into
   (3), minting the handle with `OpenGenericDefinition` as the declaring type.
5. `GetNextIntroducedMethod`: already routes through the same helper; with (3) it works for
   both without further change. Verify rather than assume.
6. Consumers that genuinely cannot answer for an open definition must refuse *specifically*,
   naming the definition-level signature comparison as the blocker rather than emitting a
   generic TODO:
   - `RuntimeMethodHandle.GetSlot` (`slotTableOfClosed`) — needs the open vtable.
   - `NativeSignature.fs:486` and `NativeReflectionInvocation.fs:93` — signature decoding and
     reflective invocation under a formal type context; both are their own increments.
     (Invoking a constructor on an open definition throws in real .NET too, but
     `GetParameters()` on one does not, so this is a real gap, not an impossibility.)
   - `GetNumVirtuals` — message updated to point at the parked case, behaviour unchanged.

## Tests

An end-to-end guest alone cannot pin this, because the thing most likely to be wrong — handle
identity — has no guest observer on the reachable path. Both halves are needed.

1. `sourcesPure/ReflectionOpenGenericConstructors.cs`: `typeof(Box<>).GetConstructors()`,
   asserting the count and the public/static split (which comes from `MethodAttributes`, on the
   reachable path). Verified against real .NET by the `sourcesPure` oracle. The exact
   assertions get settled by running it, not predicted here.
2. A unit test on the registry that no guest can express: for one `Box<T>` with one `.ctor`,
   the handle minted for `OpenGenericDefinition` and the handle minted for `Closed Box<int>`
   must be **distinct ids**, and asking twice for either must dedup to the same id. This is the
   assertion that fails if someone later "simplifies" the open case into a closed stand-in.
3. The closed control (`typeof(Box<int>).GetConstructors()`) must stay green — it is currently
   passing and is the regression guard for step (1) of the implementation.
4. Mutation checks, one per claimed behaviour: return the base type's methods instead of the
   type's own; collapse `OpenGenericDefinition` to a closed handle; return the methods in
   reverse metadata order. Each must kill at least one test above.

## Explicitly out of scope

- `GetNumVirtuals` / `GetMethods()` / `GetProperties()` on an open definition.
- Signature decoding and reflective invocation for open-definition methods.
- The parked `ReflectionGenericVirtualMethodOverrideSlots.cs`.

Each of those wants the same missing capability — definition-level symbolic signature
comparison — and that is one coherent piece of work, separate from this one.
