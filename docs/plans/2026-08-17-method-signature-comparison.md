# Compare method signatures the way `MetaSig::CompareMethodSigs` does

## The bug

Two comparisons of *concretised* method signatures decide questions that CoreCLR decides on the
signature *blob*:

| site | question | CoreCLR |
|---|---|---|
| `IlMachineStateExecution.signatureMatchesTarget` | does this candidate fill the virtual slot being dispatched? | `MethodSignature::SignaturesEquivalent` → `MetaSig::CompareMethodSigs` |
| `IlMachineMemberResolution.resolveMemberWithGenerics` | which MethodDef does this MemberRef name? | `MemberLoader::FindMethod` → `MetaSig::CompareMethodSigs` |

Concretisation deliberately looks *through* `TypeDefn.Modified`, because runtime type identity and
storage shape follow the unmodified type. So both comparisons are blind to custom modifiers in every
position, and CoreCLR is not: `IgnoreCustomModifiers` is set only by `UnsafeAccessor` lookups
(`unsafeaccessors.cpp:620,749`).

Reachable from ordinary C#, because a *combined* unmanaged calling convention on a function pointer
is spelled as `modopt`s inside the function pointer's own signature (a single nameable convention
goes in the CallKind byte, which `ConcreteTypeHandle.FunctionPointer` preserves):

```csharp
public virtual int M (delegate* unmanaged[Cdecl, SuppressGCTransition]<void> f) => 1;
public virtual int M (delegate* unmanaged[Stdcall, SuppressGCTransition]<void> f) => 2;
```

| guest | real .NET | PawPrint on `main` |
|---|---|---|
| both overloads overridden in `Derived`, called through `Base` | 4 | `multiple options: M, M` |
| both overloads in a *referenced* assembly, called cross-assembly | 2 | `Multiple overloads matching signature for call to .Sink's Take!` |

Both are loud failures rather than silent wrong answers, and both predate #1049. The census in
[[custom-modifier-census]] missed this shape because it scanned MethodDef *return* columns only,
never inside a parameter's nested function-pointer signature.

Measured non-repros, so that the test set does not claim more than it covers:

- `in` on a **virtual** parameter does carry `modreq(InAttribute)`, so `Base.M(ref int)` and
  `Derived.M(in int)` differ only by a modifier — but the derived one is `newslot`, and
  `methodMatches` screens newslot candidates out. Correct on both runtimes.
- Roslyn never asks the runtime to match across a modifier difference for interface
  implementation: where the implementing method's signature would differ it emits an **explicit
  MethodImpl stub** carrying the modifier (measured: `Base::IFace.Q(modreq(In))` beside the public
  `Base::Q(byref int32)`).
- An interface-dispatch probe *does* diverge, but its no-modifier control diverges identically: that
  is the already-parked `InterfaceSlotHiddenByDerivedMethod.cs`, which needs a slot-to-implementation
  dispatch map. Modifier-awareness must not be allowed to "fix" it, or it masks that gap.

## The design

A structural comparison of two decoded signatures, mirroring `MetaSig::CompareMethodSigs`
(`siginfo.cpp:4549`) and `MetaSig::CompareElementType` (`:3781`). `TypeDefn` is a faithful mirror of
the blob, so the rules are expressible: `SignatureTypeKind` on `FromReference`/`FromDefinition`
separates `ELEMENT_TYPE_CLASS` from `ELEMENT_TYPE_VALUETYPE`, and `PrimitiveType.Object` is a
different case from `FromReference System.Object`.

Rules taken from the spec, each of which the current comparison gets wrong or cannot express:

- **Custom modifiers** are compared in blob order, `modreq`/`modopt` distinguished, and the modifier
  *type* compared as a type (`:4082-4100`, `CompareTypeDefOrRefOrSpec`).
- **Method generic parameters** are compared **positionally and symbolically** — `varNum1 ==
  varNum2` (`:4068-4077`). No substitution is ever applied to `MVAR`. This is what
  `concretiseSignatureForSlotMatch` currently refuses to do, and un-parks
  `ReflectionGenericVirtualMethodOverrideSlots.cs`.
- **Type generic parameters** *are* substituted (`:3842-3866`), and the comparison continues in the
  substituting module's context.
- **Function pointers** compare the CallKind byte exactly, then argument count, then the return and
  every argument recursively (`:4137-4200`) — which is what the repro turns on.
- **Encodings** are not interchangeable: differing element types fail, so `M(object)` and
  `M(class System.Object)` are different signatures.
- **Header**: calling convention and `hasThis` must match modulo the generic-instantiation bit, with
  the existing vararg sentinel rule.
- **Return type** is compared unless the caller asks to skip it. CoreCLR spells "allow covariant
  return" as `skipReturnTypeSig` (`SignaturesEquivalent` passes `allowCovariantReturn` straight into
  that parameter), which maps exactly onto the dispatch site's existing `retAssignable`: skip the
  structural return comparison there and keep the assignability check.

### The one place PawPrint cannot mirror the spec, and why

CoreCLR's `Substitution` holds a `SigPointer` into an instantiation *blob*, so a substituted generic
argument keeps being compared structurally. PawPrint's instantiations are `ConcreteTypeHandle`
arrays — every site that needs this comparison (`candidateTypeGenerics`, `slot.DeclaredBy.Generics`,
`concreteExtractedTypeArgs`) holds closed handles, not blobs.

Two ways to bridge that, and they are not equivalent:

1. **Handle-valued substitution.** On reaching `GenericTypeParameter i` on either side, concretise
   the other side at that position and compare `ConcreteTypeHandle`s. Inside such a subtree the
   comparison is by runtime type identity, so it is blind to modifiers and encodings there — i.e.
   *more permissive* than CoreCLR.
2. **Round-trip the instantiation back to `TypeDefn`** via `concreteHandleToTypeDefn` and keep
   comparing structurally. Rejected: the round-trip *synthesises* a spelling, so an `object`
   argument returns as `FromDefinition System.Object` and would compare unequal to a blob's
   `ELEMENT_TYPE_OBJECT` on the other side. That is wrongly *strict* — a wrong answer, where (1) is
   a bounded loss.

Taking (1). The divergence it leaves is statable and one-directional: two signatures that differ only
by a modifier or an encoding *inside a substituted generic argument* compare equal. Nothing narrows
it further without carrying instantiations as blobs, which is a change to how PawPrint represents
generic types rather than to signature comparison.

Nominal leaves are resolved by concretising them, which is `CompareTypeTokens`' resolve-and-compare
with the identity supplied by the existing registry. Note this primes base chains
(`TypeResolution.resolveTypeFromRef`), which `Signature_AreEqual` in #1050 had to avoid; here the
types are about to be used anyway, so it costs work rather than correctness, and can adopt
`resolveTypeRefIdentity` once that lands.

### Where it lives

`WoofWare.PawPrint.Domain`, beside `TypeConcretization.concretizeMethodSignature`, with a thin
state-threading wrapper in `IlMachineTypeResolution.fs` — the arrangement #1049 established. That
puts it above all three consumers in compile order (`IlMachineTypeResolution.fs` 76,
`IlMachineMemberResolution.fs` 79, `IlMachineStateExecution.fs` 98,
`Native/NativeRuntimeTypeHelpers.fs` 134) and makes it unit-testable without an `IlMachineState`.

### Recovering the blob at the dispatch site

`signatureMatchesTarget`'s target is `methodToCall : MethodInfo<ConcreteTypeHandle, ...>`, whose
signature has already been concretised. Its `TypeDefn` signature comes back from
`state.LoadedAssembly(methodToCall.DeclaringAssembly)`'s `Methods` table via the MethodDef handle.
A `MethodInfo.Synthesised` target has no MethodDef row and so no blob; that is a loud refusal, not a
fallback to the blind comparison, because every dispatch target reached from a token is metadata.

## What the engine replaced, and what it unblocked

`candidateFillsSlot` used `concretiseSignatureForSlotMatch`, which concretised each element and
carried the modifiers beside it in a parallel path-keyed list. That is gone (155 lines), because the
comparison now reads the decoded signature directly. Two consequences:

- It refused outright on a *generic* virtual method, because there is no `ConcreteTypeHandle`
  standing for "method generic parameter i". Comparing `MVAR` positionally needs no such thing, so
  `sourcesPure/ReflectionGenericVirtualMethodOverrideSlots.cs` is un-parked — ordinary
  `class Gb : Ga` overriding `virtual void M<T>(T)`, which previously poisoned every reflection query
  on the type because `numVirtualsOfClosed` is the vtable's length.
- Signature *encodings* are now compared, so that recorded limitation is gone. The other one is not:
  a single match can still be a substitution artifact, because `VAR` is still substituted (as CoreCLR
  substitutes it), and catching that needs the substitution chain CoreCLR carries down the `extends`
  clauses — a different thing from comparing two signatures.

The `genericMethodTypeArgs` parameter of `resolveMemberWithGenerics` went with it: nothing substitutes
method generics any more, so it was dead at all nine call sites.

## Mutation results

16 mutations, one per rule. 14 killed. Notable:

- `object-encoding-conflated` (pretend `ELEMENT_TYPE_OBJECT` and `class System.Object` are
  interchangeable) is killed by six `Monitor` guests as well as by its unit test, so the encoding rule
  is load-bearing for real BCL dispatch rather than a pedantic detail.
- `generic-count-ignored` and `callee-sentinel-ignored` survived the first pass, and each was a
  missing test rather than dead code. `void M<T>()` and `void M<U, V>()` share a header byte and an
  empty parameter list, and nothing screens arity ahead of this comparison any more.
- `memberref-candidate-token-space` was killed only incidentally, by `ResizeArray.cs`.
  `TestCrossAssemblyOverloadTokenSpace` makes it attributable, on the `DecoyLib` recipe
  `TestCrossAssemblyFieldScope` established, and was itself checked to fail under the mutation.

**Two survivors, both about the dispatch site's return column**, and both pre-existing:
`skipReturnType` may be flipped to `false`, and the `isAssignableFrom` behind it replaced with handle
equality, with the whole suite still green. So nothing in the suite has a dispatch case whose
candidate and target differ in their return types at all. The latitude is untested rather than known
to be unnecessary — Roslyn emits a covariant-return override as `newslot` plus a MethodImpl, which
`methodMatches` screens out before this comparison, so the shape that would need it may not be
reachable from C# at all. Left exactly as it was rather than tightened on the strength of a failure to
construct a case.
