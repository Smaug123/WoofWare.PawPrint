# Type Identity And Resolution Refactor Plan

Date: 2026-04-09

This document is a handoff-quality implementation plan for refactoring type identity and type resolution in `WoofWare.PawPrint`, with the following agreed constraints:

- Nested types must be supported correctly.
- Full metadata identity must be preserved.
- `ModuleRef` may remain unsupported for now, but the design must leave room for it.
- Performance is not a goal.
- The first landing should be domain-layer-first.
- Internal simplifications are acceptable only when they do not erase user-visible semantics.

This plan is written in the spirit of `gospel.md`:

- preserve local reasoning
- make illegal states unrepresentable where possible
- prefer a small orthogonal core
- use tests, including property-style tests where possible, to enforce invariants mechanically

## Problem Summary

The current domain layer mixes three different notions of "type":

1. A metadata token which refers to some type-like thing in some assembly.
2. A resolved nominal type definition after following type refs and forwards.
3. A runtime-concrete type with generic instantiation.

The current implementation partially collapses these layers, especially by reducing type identity to `(namespace, name)` in several places. That is not compatible with:

- nested types
- `TypeRefResolutionScope.TypeRef`
- full metadata identity
- robust forwarding and cross-assembly resolution
- making misuse difficult

The refactor should separate the three layers explicitly and make it hard for downstream code to skip the correct resolution path.

## Goals

### Primary goals

- Introduce canonical type identity in the domain layer.
- Make nested-type resolution correct by construction.
- Remove authoritative `(namespace, name)`-based resolution from the domain core.
- Make concretisation operate on resolved nominal identity rather than ad hoc lookup.
- Provide a migration path that allows the interpreter layer to adopt the new core incrementally.

### Non-goals for the first landing

- Full `ModuleRef` support.
- Interpreter-wide adoption in the same change.
- Dispatch refactor.
- Boxing/byref refactor.

Those later areas depend on this refactor, but are not part of the first landing.

## High-Level Design

Introduce three explicit layers and keep conversions one-way where possible.

### Layer 1: metadata reference identity

This is "a particular metadata token in a particular assembly".

Suggested type:

```fsharp
type MetadataTypeIdentifier =
    {
        ReferencedInAssembly : AssemblyName
        Token : MetadataToken
    }
```

Restrictions:

- `Token` must be one of `TypeDefinition`, `TypeReference`, or `TypeSpecification`.
- Construct this through smart constructors rather than by exposing the record directly.

Purpose:

- represent type-like metadata at the exact point it is encountered
- preserve enough information to resolve later in a scope-aware way

### Layer 2: resolved nominal identity

This is "the unique type definition this metadata eventually names".

Suggested type:

```fsharp
type ResolvedTypeIdentity =
    private
        {
            DefiningAssembly : AssemblyName
            Definition : ComparableTypeDefinitionHandle
        }
```

Important note:

- `ResolvedTypeIdentity` is intentionally only assembly plus type-definition handle.
- Nested chain is not stored redundantly.
- Nested-chain information must be recoverable from the defining assembly on demand.

This is acceptable because performance is a non-goal and because we want a small canonical core.

### Layer 3: concrete runtime identity

This is "the resolved nominal type, instantiated with these concrete generic arguments".

Suggested shape:

- keep `ConcreteTypeHandle` as the external runtime handle
- change the backing uniqueness key in `AllConcreteTypes` to use `ResolvedTypeIdentity + concrete generics`

This lets runtime identity be derived from nominal identity instead of re-deriving from assembly/namespace/name.

## Core Invariants

These should be explicit in code comments, tests, and helper APIs.

1. Authoritative type resolution never keys by `(namespace, name)` alone.
2. A `ResolvedTypeIdentity` can only be created by a resolver which has handled scope correctly.
3. Nested-type resolution always follows the enclosing-type path; there is no "shortcut" API that resolves nested types from `(namespace, simpleName)`.
4. `ConcreteTypeHandle` uniqueness is based on resolved nominal identity plus concrete generic arguments.
5. Friendly lookup helpers may exist for diagnostics or discovery, but not as canonical resolution logic.
6. If `ModuleRef` is encountered in a path that is not yet supported, the failure should be explicit and local, not silently collapsed into some other lookup path.

## Implementation Phases

## Phase 0: Add tests before changing the model

This is important. The existing test suite is too end-to-end and too exit-code-focused to protect this refactor.

Add domain-level tests first, even if they initially expose current failures.

### New test surface

Preferred approach:

- add focused tests to `WoofWare.PawPrint.Test`, but test domain-layer entry points directly
- do not go through the whole interpreter unless the scenario is specifically an integration test

If convenient, add a dedicated test module such as:

- `TestTypeResolution.fs`
- `TestTypeConcretisation.fs`

### Test fixture utilities to add

- compile C# snippets into in-memory assemblies as today
- compile multiple assemblies with references between them
- load `DumpedAssembly` directly from emitted PE bytes
- expose helpers to inspect type definitions, type refs, and call domain resolvers directly

### Test cases to add immediately

#### Example-based resolver tests

1. Top-level type resolves within same assembly.
2. Top-level type resolves across assembly reference.
3. Nested type resolves within same assembly.
4. Nested type resolves across assembly reference.
5. Nested type resolves through `TypeRefResolutionScope.TypeRef`.
6. Two nested types with the same simple name under different declaring types remain distinct.
7. Forwarded type resolves to target assembly.
8. Forwarded nested type resolves correctly if metadata shape permits it.
9. Unsupported `ModuleRef` path fails explicitly with a clear message.

#### Concretisation tests

10. Same nominal type with same generic arguments gives same `ConcreteTypeHandle`.
11. Same simple name but distinct metadata identity yields different `ConcreteTypeHandle`.
12. Nested generic type concretises distinctly from a top-level type of the same simple name.
13. Generic arguments from enclosing types propagate correctly into nested types.

#### Property-style tests where possible

If a property-testing library is already available or easy to add, use it. If not, write small exhaustive/generated tests manually.

Suggested properties:

- resolving the same `TypeReferenceHandle` twice is idempotent
- concretising the same `TypeDefn` twice is idempotent
- for every type definition in an assembly, resolving its self-identity and then looking it up returns the same definition handle
- for every nested type in test assemblies, recovered full name is consistent with the declaring-type chain

The point is not large random fuzzing yet; the point is mechanical enforcement of the invariants.

## Phase 1: Introduce canonical identity types in the domain layer

Create a new domain file, likely something like:

- `WoofWare.PawPrint.Domain/TypeIdentity.fs`

Suggested contents:

- `MetadataTypeIdentifier`
- `ResolvedTypeIdentity`
- smart constructors for each supported metadata token kind
- rendering/debug helpers
- helpers to recover the full nested chain from a `ResolvedTypeIdentity`

### API guidance

Follow the "hard to misuse" rule.

- make record constructors private where possible
- expose functions like:
  - `MetadataTypeIdentifier.ofTypeDef`
  - `MetadataTypeIdentifier.ofTypeRef`
  - `MetadataTypeIdentifier.ofTypeSpec`
  - `ResolvedTypeIdentity.make`

Do not expose an unrestricted public record constructor for `ResolvedTypeIdentity`.

### Full-name recovery helper

Add a helper along the lines of:

```fsharp
ResolvedTypeIdentity.fullName : DumpedAssembly -> ResolvedTypeIdentity -> string
```

or a function which returns the chain explicitly.

This keeps the canonical identity minimal while still making nested information available on demand.

## Phase 2: Add authoritative nested-aware assembly indexes

The existing `DumpedAssembly` fields already preserve handle-based metadata. Build on that instead of replacing it.

### Add helper indexes

Add indexes oriented around correct resolution:

- top-level type defs by `(namespace, name)` for only top-level types
- nested type defs by `(declaring type handle, simple name)`
- exported types in a form that does not collapse nested identity

Possible additions on `DumpedAssembly`:

- `TryGetTopLevelTypeDef : string -> string -> TypeInfo<...> option`
- `TryGetNestedTypeDef : TypeDefinitionHandle -> string -> TypeInfo<...> option`

These should be clearly named so callers cannot confuse them with canonical resolution.

### Important restriction

Do not offer a single helper like `TypeDef ns name` that silently spans top-level and nested cases.

That would preserve the current footgun.

### What to deprecate

The following should stop being authoritative:

- `BuildTypeRefsLookup`
- `BuildTypeDefsLookup`
- `DumpedAssembly.TypeRef`
- `DumpedAssembly.TypeDef`

They can either be:

- retained temporarily for diagnostics only, clearly documented as non-authoritative
- or removed in a later cleanup phase

## Phase 3: Rebuild the resolver around metadata scope chains

This is the core of the refactor.

### Replace `TypeResolutionResult`

Current `TypeResolutionResult` returns a resolved `TypeInfo` or indicates an assembly to load.

Replace or extend it so the canonical successful result carries nominal identity explicitly.

Suggested result:

```fsharp
type TypeResolutionResult =
    | FirstLoadAssy of AssemblyReference
    | Resolved of DumpedAssembly * ResolvedTypeIdentity * TypeInfo<TypeDefn, TypeDefn>
```

or, if you want less duplication:

```fsharp
type ResolvedType =
    {
        Assembly : DumpedAssembly
        Identity : ResolvedTypeIdentity
        Definition : TypeInfo<TypeDefn, TypeDefn>
    }
```

The key point is that successful resolution should carry the canonical nominal identity.

### Rewrite `Assembly.resolveTypeRef`

Algorithm sketch:

1. Inspect `target.ResolutionScope`.
2. If `Assembly`:
   - load referenced assembly if necessary
   - resolve as a top-level type in the target assembly
3. If `TypeRef`:
   - recursively resolve the parent type ref
   - use the resulting definition handle and defining assembly
   - find the child nested type by `(declaring type handle, simple child name)`
4. If `ModuleRef`:
   - return explicit unsupported failure for now

### Exported-type resolution is part of the core refactor

Do not treat exported types as a side concern.

Today, forwarded exports are effectively looked up by `(namespace, name)`, and nested exported types are represented via parent `ExportedTypeHandle` chains. That means the exported-type path must be made nested-aware at the same time as `TypeRef` resolution, or forwarded nested types will remain wrong even if plain `TypeRef` resolution is fixed.

Required adjustment:

- add an exported-type resolver that preserves parent-chain semantics
- make forwarded-type resolution return canonical nominal identity, not just a rediscovered name-based `TypeInfo`
- ensure nested exported types are resolved by walking their enclosing exported-type path, not by flattening to `(namespace, simpleName)`

### Generic substitution

Do not mix scope resolution and generic substitution more than necessary.

Recommended ordering:

1. resolve nominal identity first
2. obtain the target `TypeInfo`
3. only then apply generic substitution into the `TypeInfo<TypeDefn, TypeDefn>` view if required by the current API

That separation will make the resolver easier to reason about.

### Remove `resolveTypeFromName` as a canonical path

The existing `resolveTypeFromName` should not remain in the core resolver API, because it invites bypassing scope-aware resolution.

Possible replacements:

- restrict it to top-level type lookup only and rename it accordingly
- move it to a diagnostics/discovery module

The important thing is that downstream code should not be able to reach nested types through the wrong path.

### Compatibility adapter for current callers

Current domain callers often expect "resolved and already generic-substituted `TypeInfo<TypeDefn, TypeDefn>`".

To keep the first landing domain-layer-first rather than turning into a whole-codebase rewrite, introduce an explicit temporary adapter:

- canonical resolver returns nominal identity first
- compatibility helper takes that nominal identity, loads the corresponding `TypeInfo`, and performs the generic substitution required by the legacy API

Do not let the compatibility helper become the new authoritative API. It exists only to keep migration churn contained.

## Phase 4: Thread resolved nominal identity into concretisation

This is the second major phase.

### Update `ConcreteType<ConcreteTypeHandle>`

Today the concrete type record appears to carry redundant nominal information like assembly, namespace, name, and definition.

Change the canonical uniqueness basis so it uses:

- `ResolvedTypeIdentity`
- concrete generic arguments

It is acceptable to retain cached namespace/name on the record for debugging if useful, but those should be derived fields, not the thing equality depends on conceptually.

### Update `AllConcreteTypes.findExistingConcreteType`

Replace the current lookup-by-assembly/name/namespace/generics API with something centered on nominal identity:

```fsharp
findExistingConcreteType :
    AllConcreteTypes ->
    ResolvedTypeIdentity ->
    ImmutableArray<ConcreteTypeHandle> ->
    ConcreteTypeHandle option
```

Keep old helpers only as temporary adapters if necessary.

### Update concretisation entry points

The flow should become:

- metadata token / `TypeDefn`
- resolve nominal identity
- instantiate generic arguments
- look up or create concrete runtime type

In particular:

- `concretizeTypeReference` should resolve to `ResolvedTypeIdentity` before creating/finding a concrete type
- `concretizeTypeDefinition` should construct `ResolvedTypeIdentity` directly from the defining assembly plus type-def handle
- `TypeSpecification` paths should resolve their underlying nominal types through the same mechanism
- `concretizeGenericInstantiation` must stop rediscovering generic definitions by `(namespace, name)` or exported-type name lookup; it must reuse the canonical resolver path

### Temporary compatibility

If needed for incremental landing, add adapters that derive the old assembly/name/namespace fields from `ResolvedTypeIdentity` when constructing existing records.

That keeps interpreter churn low while still moving the domain core to the right model.

### Concrete-type identity migration note

There is an important mismatch in the current code:

- `ConcreteType` equality is already effectively based on defining assembly + type-def handle + generics
- `AllConcreteTypes.findExistingConcreteType` still uses `(assembly, namespace, name, generics)`

Do not migrate only one side of that split.

The following pieces need to move together:

- `ConcreteType`
- `AllConcreteTypes` lookup helpers
- concretisation cache keys / in-progress keys where nominal identity participates

Otherwise the refactor risks creating a half-migrated state where equality says two concrete types are the same but the lookup path still manufactures duplicates or fails to find them.

## Phase 5: Make downstream nominal-identity misuse harder

This phase is about preventing backsliding.

### Restrict APIs

After the new resolver exists:

- avoid exposing convenience methods on `DumpedAssembly` that look like canonical resolution but are not
- prefer helper names which force the caller to acknowledge top-level vs nested distinctions

Examples:

- good: `TryGetTopLevelTypeDef`
- good: `TryGetNestedTypeDef`
- bad: `TypeDef`

### Use narrow types

Where a function really needs a resolved nominal type, take `ResolvedTypeIdentity`, not `(AssemblyName * TypeDefinitionHandle)` and definitely not `(namespace * name)`.

That makes misuse a compile-time issue.

### Debug assertions

Where invariants cannot be encoded directly, add asserts:

- if a nested type is being looked up through a top-level-only helper, fail fast
- if a `ResolvedTypeIdentity` points at a handle not present in the given defining assembly, fail fast

## Phase 6: Clean up and delete obsolete code

Once the domain layer is migrated and tests pass:

- remove or demote old name-based authoritative lookup paths
- remove temporary adapters if possible
- simplify duplicated resolution logic in `TypeConcretisation.fs`

This should be done promptly. A half-migrated state here is dangerous because both resolution models would coexist.

## Detailed Work Breakdown

This is the order I recommend for actual implementation.

1. Add new domain tests that currently fail or are marked pending.
2. Add `TypeIdentity.fs` with canonical identity types.
3. Add nested-aware lookup helpers on `DumpedAssembly`.
4. Add nested-aware exported-type lookup/resolution helpers.
5. Rewrite `Assembly.resolveTypeRef`.
6. Replace `TypeResolutionResult` to carry `ResolvedTypeIdentity`.
7. Add an explicit compatibility adapter for current callers that still need substituted `TypeInfo`.
8. Update `TypeConcretisation.fs` to consume resolved nominal identity, including generic instantiation paths.
9. Update concrete-type uniqueness helpers and align them with `ConcreteType` equality.
10. Add more tests around concretisation uniqueness and nested generic cases.
11. Demote/remove old `(namespace, name)` resolution APIs.
12. Only after all of that, begin interpreter adoption in a separate change.

## Concrete Test Inventory

Below is a more explicit list suitable for implementation tickets.

### Resolver tests

#### `Nested type in same assembly`

C# shape:

```csharp
namespace N
{
    public class Outer
    {
        public class Inner { }
    }
}
```

Assert:

- metadata exposes a `TypeRef` or `TypeDef` path that names `Outer.Inner`
- resolver returns the `TypeDefinitionHandle` for `Inner`
- recovered full name is `N.Outer.Inner`

#### `Nested type across assemblies`

Assembly A defines `Outer.Inner`.

Assembly B references `Outer.Inner`.

Assert:

- the reference resolves into A
- the nominal identity points to A's `Inner` definition handle

#### `Same simple nested name under different parents`

Define:

- `A.X.Inner`
- `A.Y.Inner`

Reference both from another type.

Assert:

- the two refs resolve to distinct `ResolvedTypeIdentity` values

#### `Top-level and nested same simple name`

Define:

- top-level `A.Inner`
- nested `A.Outer.Inner`

Assert:

- resolution does not collapse these

#### `Explicit unsupported ModuleRef`

Craft or locate metadata containing `ModuleRef` if practical.

Assert:

- resolver fails with explicit unsupported error

If generating such metadata is awkward, this can be deferred, but the resolver branch should still be tested at unit level if possible.

### Concretisation tests

#### `Idempotent same type`

Concretise the same `TypeDefn` twice.

Assert:

- same `ConcreteTypeHandle`

#### `Distinct nested identities remain distinct`

Concretise:

- `Outer1.Inner`
- `Outer2.Inner`

Assert:

- different `ConcreteTypeHandle`s

#### `Nested generic identity`

Use a nested generic type where enclosing generics matter.

Assert:

- two different enclosing generic instantiations yield different concrete types
- same enclosing generic instantiation yields same concrete type

#### `Forwarded type concretisation`

Reference a forwarded type, concretise it, and assert:

- the concrete type is keyed by the target defining assembly and type-def handle, not the forwarding assembly

#### `Generic instantiation does not bypass canonical resolution`

Concretise a generic type whose definition is reached through:

- a nested type reference, or
- a forwarded type reference

Assert:

- the generic-definition lookup path goes through canonical nominal resolution
- concretisation succeeds without any `(namespace, name)`-only rediscovery logic

### Name-recovery tests

For a `ResolvedTypeIdentity` representing a nested type:

- recover its nested chain from the defining assembly
- assert the full-name renderer is correct

This protects the "store only assembly + typedef handle" choice.

## Design Risks And How To Avoid Them

### Risk: adding a new identity type but leaving old lookups authoritative

This creates dual truth.

Mitigation:

- move canonical resolution over early
- rename old helpers so it is obvious they are non-authoritative

### Risk: fixing `resolveTypeRef` but leaving `concretizeGenericInstantiation` on a separate lookup path

This creates a second resolver in practice, even if the main resolver is correct.

Mitigation:

- route generic-instantiation base-type discovery through the same canonical nominal resolver
- delete or demote any helper that "finds the generic definition again" by `(namespace, name)`

### Risk: handling forwarded top-level types but not forwarded nested exported types

This would give a false sense of completion while leaving an important metadata identity case broken.

Mitigation:

- include exported-type parent-chain resolution in the same landing
- add a specific test for forwarded nested types if metadata shape permits it
- if the metadata shape is awkward to synthesize, still make the resolver structure explicit and test the unit-level branches directly

### Risk: migrating concrete-type equality and lookup separately

This creates subtle duplicate-handle or failed-lookup behaviour.

Mitigation:

- update `ConcreteType` and `AllConcreteTypes` together
- add idempotence tests immediately around the migration

### Risk: underestimating legacy callers of substituted `TypeInfo`

The current domain API shape often expects resolution and substitution in one operation.

Mitigation:

- add a temporary compatibility adapter explicitly
- keep the canonical API nominal-identity-first
- remove the adapter promptly once downstream call sites are migrated

### Risk: storing too much redundant derived data in `ResolvedTypeIdentity`

That increases mismatch risk.

Mitigation:

- keep `ResolvedTypeIdentity` minimal
- recover nested chain and names from `DumpedAssembly` on demand

### Risk: allowing callers to construct `ResolvedTypeIdentity` directly

Then the enclosing-type path can be bypassed accidentally.

Mitigation:

- private constructor / private record
- expose only smart constructors from known-correct paths

### Risk: interleaving interpreter migration with domain refactor

That makes debugging much harder.

Mitigation:

- land domain-layer work first
- keep interpreter compatibility adapters temporary and explicit

## Suggested File Changes

Likely files touched in the domain-first landing:

- new: `WoofWare.PawPrint.Domain/TypeIdentity.fs`
- `WoofWare.PawPrint.Domain/Assembly.fs`
- `WoofWare.PawPrint.Domain/TypeRef.fs`
- `WoofWare.PawPrint.Domain/ExportedType.fs`
- `WoofWare.PawPrint.Domain/TypeConcretisation.fs`
- possibly `WoofWare.PawPrint.Domain/ConcreteType.fs`
- test files in `WoofWare.PawPrint.Test/`

Likely files not to touch yet unless required:

- interpreter execution files under `WoofWare.PawPrint/`
- dispatch code
- heap model

## Handoff Notes For The Implementer

### Priority judgement

Do not start by "just teaching `resolveTypeRef` about nested types". That is too local and will likely preserve the wrong API shape.

Start by introducing the identity types and tests. The point of the refactor is not just to make one resolver function smarter; it is to make the type system and API surface push callers toward the correct resolution path.

Do not leave exported-type resolution or generic-instantiation lookup for later cleanup. In the current code those are active alternate resolution paths, so leaving them behind would preserve dual truth.

### Success criteria

The domain-layer-first landing is successful when:

- nested-type and metadata-identity tests pass
- canonical resolution no longer depends on `(namespace, name)` alone
- concretisation uniqueness is driven by resolved nominal identity
- there is an explicit unsupported path for `ModuleRef`
- interpreter code can remain temporarily unchanged except for adapters, if needed

### Stop conditions

If the implementation finds that `ResolvedTypeIdentity = assembly + typedef handle` is insufficient, stop and reassess before widening the type casually.

The current expectation is that it is sufficient because:

- nested chain is recoverable from the defining assembly
- the important thing is that resolution of nested refs must go through the enclosing-type-aware algorithm before the identity is created

If that assumption fails in practice, the right answer is to revisit the identity model deliberately, not patch around it ad hoc.

