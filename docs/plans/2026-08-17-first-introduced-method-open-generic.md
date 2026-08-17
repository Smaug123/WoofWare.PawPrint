# `GetFirstIntroducedMethod` for open generic type definitions

Reviewed once (Fable) and revised: the recommendation flipped from a new narrow DU to reusing
`RuntimeTypeHandleTarget`, because the sibling field registry already shipped exactly that.

## What is broken

`RuntimeTypeHandle.GetFirstIntroducedMethod` refuses when its `RuntimeType` resolves to
`RuntimeTypeHandleTarget.OpenGenericTypeDefinition`
(`Native/NativeRuntimeTypeFCall.fs:900-902`).

Narrowest guest that reaches it — no expression trees, no BCL generics, one boring type:

```csharp
public sealed class Box<T> { public T Item; public Box() {} public Box(T t) { Item = t; } }
var ctors = typeof(Box<>).GetConstructors();   // refuses
```

## Measured: which reflection queries reach what

Runs of that guest against `origin/main` at `a0a794b9`, differing only in the final query.

| Query on `typeof(Box<>)` | Outcome |
| --- | --- |
| `IsGenericTypeDefinition`, `GetGenericArguments()`, `GetFields()` | already work |
| `GetConstructors()` | **refuses in `GetFirstIntroducedMethod`** |
| `GetMethods()`, `GetProperties()` | refuse earlier, in `GetNumVirtuals` |
| `typeof(Box<int>).GetConstructors()` (control) | **passes** |

`GetConstructors()` is the entry point that isolates this work. `PopulateConstructors`
(`RuntimeType.CoreCLR.cs:753` in the pinned source) iterates
`RuntimeTypeHandle.GetIntroducedMethods` and then asks only for `GetUtf8Name` (under a string
filter, which `GetConstructors()` does not set), `GetAttributes`, `GetMethodDef` (only under
metadata-update support, which PawPrint disables) and `GetStubIfNeeded`. No `GetNumVirtuals`,
no `GetSlot` — constructors are never virtual. `GetMethods()` does call `GetNumVirtuals`, via
`GetMethodCandidates` allocating its `bool[numVirtuals]` overrides map.

**`GetNumVirtuals` for an open definition stays refusing.** `numVirtualsOfClosed` is by
definition the length of `vtableOfClosed` (`NativeRuntimeTypeHelpers.fs:1637-1644`), and slot
matching concretises both signatures under the declaring type's `Generics`
(`concretiseSignatureForSlotMatch`, `:843-864`); an open definition has none, and
`TypeDefn.GenericTypeParameter` throws without a supplied instantiation
(`TypeConcretisation.fs:980-984`). `NativeRuntimeTypeHelpers.fs:882-903` already names the
missing capability — a *symbolic*, definition-level signature comparison — and
`sourcesPure/ReflectionGenericVirtualMethodOverrideSlots.cs` is its parked case, whose
`unimplemented` note says so verbatim. A definition-level walk could answer for the narrow
`Box<T> : object`-with-no-virtuals shape without symbolic comparison, but special-casing that
is against project policy; the whole thing stays parked.

## The obstacle: `MetadataMethodIdentity` cannot name an open definition

```fsharp
type MetadataMethodIdentity =
    private { AssemblyFullName : string
              DeclaringType : ConcreteTypeHandle
              ... }
```

`MethodHandleRegistry.MethodHandleToId` keys on this, so it *is* the guest-visible identity of
a `RuntimeMethodHandleInternal`. `ConcreteTypeHandle` structurally cannot denote `Box<T>`
un-substituted: both nominal concretizers refuse open definitions outright
(`concretizeTypeDefinition`, `TypeConcretisation.fs:893-898`; `concretizeTypeReference`,
`:918-923`), and `TypeDefn.GenericTypeParameter` is only ever resolved by indexing a supplied
instantiation (`:980-984`). There is no `__Canon`:
`NativeRuntimeMethodHandle.fs:160-163` states that PawPrint has no code sharing at all and
"every handle its registry can mint records an exact declaring type".

This is an identity question, not an observability one. A closed stand-in would collide
`Box<>`'s `.ctor` with `Box<int>`'s on one registry id, where CoreCLR keeps distinct
`MethodDesc*`. Measured on real .NET: the two handles compare unequal while their MethodDef
tokens are *identical* (`100663297` both), so the token alone cannot separate them.

## Options for the declaring-type representation

### Option A — widen `DeclaringType` to `RuntimeTypeHandleTarget` (recommended)

`Closed of ConcreteTypeHandle` already exists, so every current construction site becomes
`RuntimeTypeHandleTarget.Closed h`.

**The deciding fact is that the sibling registry already did this.**
`FieldHandle.DeclaringType` is already a `RuntimeTypeHandleTarget`
(`FieldHandleRegistry.fs:7-25`), and its docstring makes this plan's identity argument in so
many words: "a closed instantiation gets `Closed`; the open generic definition gets
`OpenGenericTypeDefinition`; and the two yield distinct registry ids". It keeps impossible
states out with a single mint-time chokepoint guard (`getOrAllocate`, `:88-101`) rather than
with the type, after which consumers treat the excluded arms as contract violations — the
pattern this codebase uses for `RuntimeTypeHandleTarget` matches throughout (e.g. `numVirtuals`
at `NativeRuntimeTypeHelpers.fs:1660-1668`).

Two further pieces of the design already anticipate the open case:
`stubDeclaringTypeOfTarget` has a working `OpenGenericTypeDefinition` arm returning
`HasInstantiation = true, IsGenericTypeDefinition = true`
(`NativeRuntimeMethodHandle.fs:425-429`), and `MethodTableStubFacts.HasInstantiation`'s
docstring says "True for both `Foo<int>` and the typical `Foo<>`". And
`methodTableOfDeclaringType` (`:1328`) exists only to convert `ConcreteTypeHandle` into a
`RuntimeTypeHandleTarget`; under A that conversion disappears at that site.

Cost: four states become representable that cannot occur (`GenericParameter`,
`MethodGenericParameter`, `OpenConstructed`, `DynamicMethodsClass` — the last doubly wrong,
since `MethodHandle.FromDynamic` is how a dynamic method is named). The mint-time guard is what
answers that, exactly as in the field registry.

### Option B — a new narrow DU (`Closed | OpenGenericDefinition`)

This was the previous recommendation. Rejected on review:

- It makes the two handle registries speak different declaring-type vocabularies for the same
  concept, for no gain the mint-time guard does not already provide.
- "Illegal states unrepresentable" is only partial anyway: `Closed of ConcreteTypeHandle` still
  represents a method declared on a byref, pointer, array or function pointer, since
  `findExistingConcreteType` only ever yields `Concrete` (`MethodHandleRegistry.fs:231, 283`).
  B removes four impossible states and keeps five, and the existing refusal arms for those five
  (`NativeSignature.fs:496-507`, `NativeReflectionInvocation.fs:103-114`,
  `methodTableOfDeclaringType`'s `Error` arms) survive under B unchanged.
- Its `MethodDeclaringType -> RuntimeTypeHandleTarget` conversion would not be total either:
  the `Closed` case still has to delegate to the fallible `methodTableOfDeclaringType`.

### Option C — widen the concretization system itself

Structurally different: add a `ConcreteTypeHandle` case (or an `AllConcreteTypes` entry) for
the typical instantiation — the `__Canon`-shaped route. This is the trajectory the repo's own
parked notes name as the eventual fix for the adjacent gaps (`TestPureCases.fs:30`;
`NativeSignature.fs:459-463`).

Rejected *here*, but not in general: every `AllConcreteTypes` consumer assumes its entries are
closed and layout-computable, and `ConcreteTypeHandle` is a public DU in the published
`WoofWare.PawPrint.Domain` package, so a new case is a breaking change for external exhaustive
matchers. If C ever lands it partially supersedes the declaring-type widening — which is an
argument for A over B, since A adds no new vocabulary to unwind.

### Option D — a separate id space for open-definition methods

Rejected: `IdToMethodHandle` resolution would need a third case regardless, so this buys
nothing over A while fragmenting the registry.

## Implementation

1. Change `MetadataMethodIdentity.DeclaringType` to `RuntimeTypeHandleTarget`;
   `GetDeclaringType ()` returns it. Add a mint-time chokepoint guard in
   `MethodHandleRegistry`, mirroring `FieldHandleRegistry.getOrAllocate:88-101`, admitting only
   `Closed` and `OpenGenericTypeDefinition`.
2. `makeOpenMethodHandle` takes the `RuntimeTypeHandleTarget` directly instead of resolving a
   `ConcreteType` through `findExistingConcreteType`. Note the name now collides in sense —
   "open" there means *open method generics*, not an open declaring type; add a docstring
   sentence distinguishing them.
3. Reshape `introducedMethodsOfClosed` into a dispatcher over `RuntimeTypeHandleTarget`. Its
   return type must change: it currently returns
   `(ConcreteType<ConcreteTypeHandle> * methods) option`, and the open case has no
   `ConcreteType`, so it returns the declaring `RuntimeTypeHandleTarget` alongside the methods.
   That signature is the interface between steps 3, 4 and 5.
   - `Closed` → today's behaviour, unchanged.
   - `OpenGenericTypeDefinition identity` → `Assembly.resolveTypeIdentityDefinition` on the
     defining assembly, then that `TypeInfo.Methods` in metadata order. No concretization and
     no base-chain walk: introduced methods are the type's own.
4. `GetFirstIntroducedMethod`: replace the refusal with a call into (3).
5. `GetNextIntroducedMethod`: routes through the same helper, so it should work for both.
   Verify by test rather than assume.
6. Consumers the compiler now forces:
   - `RuntimeMethodHandle.GetMethodTable` (`:1328`) must **answer**, not refuse:
     `OpenGenericTypeDefinition identity` is precisely CoreCLR's typical-instantiation
     MethodTable, per that function's own docstring.
   - `RuntimeMethodHandle.GetSlot` (`:1186`), `NativeSignature.fs:486`,
     `NativeReflectionInvocation.fs:93` must refuse *specifically*, naming the definition-level
     signature comparison as the blocker. Each is its own increment. (`GetParameters()` on an
     open-definition ctor works in real .NET, so this is a real gap, not an impossibility.)
7. Comments this change falsifies, which must be corrected in the same commit:
   - `NativeRuntimeTypeFCall.fs:978-980` ("the registry only stores handles whose declaring
     type was Concrete") and the error message at `:983-985`.
   - `NativeRuntimeMethodHandle.fs:991-1001` — see the struct hole below.

## The struct hole, found on review

For `struct Box<T>`, `GetStubIfNeeded`'s fast path fails on `IsValueType`, the slow QCall's
`stubOutcome` says `Rebind` (`NativeRuntimeMethodHandle.fs:349-354`) with an *empty*
instantiation, and `declaringTypeGenerics` then hits the `failwith` at `:1003`. That arm's
justifying comment (`:991-1001`) claims it is unreachable both because such lookups die earlier
at the `GetNumVirtuals` TODO and because it needs a non-empty instantiation. This change
falsifies both halves. Real .NET handles it — it is an unboxing stub.

So "the downstream chain already works" holds for *reference-type* definitions only. Plan:
after implementing, run a `struct Box<T>` guest. If it hits `:1003`, park a struct guest in
`unimplemented` with a note, and rewrite that comment to say what actually reaches it. Do not
leave the comment asserting unreachability.

## Tests

1. **Guest, `sourcesPure/ReflectionOpenGenericConstructors.cs`.** Handle identity *is* guest
   observable — verified, not assumed. On real .NET,
   `typeof(Box<>).GetConstructors()[i].MethodHandle` never equals
   `typeof(Box<int>).GetConstructors()[j].MethodHandle` for any pair, while the `MetadataToken`s
   coincide. And `MethodHandle.Equals`, `MetadataToken`, `IsPublic`/`IsStatic` were each
   confirmed already working under PawPrint on the closed control (both runtimes exit 0). So
   the guest asserts:
   - count is 2; all public; none static (from `MethodAttributes`);
   - no open handle equals any closed handle across every pair — this is the assertion that
     fails if the open case is ever "simplified" into a closed stand-in;
   - the tokens *are* shared, as a vacuity guard, so the previous check cannot pass trivially;
   - **metadata order**, via `MetadataToken` ascending — this is what kills the
     reverse-order mutant, which nothing in the previous draft did.

   Keep `int` as the closed control, deliberately: with a reference-type argument CoreCLR
   shares `__Canon` (`NativeRuntimeMethodHandle.fs:170-174`), and handle distinctness would no
   longer be a cross-runtime fact.

   Risk to verify by running: PawPrint's `ceq` over two `MethodRegistryHandle`-backed
   `IntPtr`s. If it refuses, fall back to the unit test and record why here.

2. **Unit test in `TestMethodHandleRegistry.fs`**, mirroring
   `TestFieldHandleRegistry.fs:440-478`: minting for `OpenGenericTypeDefinition` and for
   `Closed Box<int>` yields distinct ids; each resolves back to the declaring target it was
   minted with; minting twice with the same target dedups to one id. The guest cannot see
   registry-level dedup, so this is not redundant with (1).

3. **Property test with an outside oracle.** For every generic type definition in a corpus
   assembly, the open-definition dispatcher's method list equals the MethodDef rows
   `System.Reflection.Metadata` reports for that TypeDef, in the same order. The expectation
   comes from outside the graph under test, so a wrong-but-self-consistent walk fails. Plus
   injectivity: distinct `(declaringTarget, methodDef)` pairs never share a minted id.

4. **Regression guard**: the closed control `typeof(Box<int>).GetConstructors()` must stay
   green; it passes today and covers step 1 of the implementation.

5. **Mutation checks**, one per claimed behaviour, each required to kill a named test:
   - return the base type's methods instead of the type's own → killed by the count in (1),
     because `object` declares one ctor and `Box` two;
   - collapse `OpenGenericTypeDefinition` to a closed handle → killed by the cross-pair handle
     inequality in (1) and by (2);
   - reverse metadata order → killed by the token-ordering assertion in (1) and by (3);
   - drop the mint-time guard → killed by (2) if it asserts the refusal.

## Explicitly out of scope

`GetNumVirtuals` / `GetMethods()` / `GetProperties()` on an open definition; signature decoding
and reflective invocation for open-definition methods; the parked
`ReflectionGenericVirtualMethodOverrideSlots.cs`. Each needs the same missing capability —
definition-level symbolic signature comparison — which is one coherent piece of work, separate
from this one.
