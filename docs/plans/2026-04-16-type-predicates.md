# Plan: split `ResolvedBaseType` queries by intent

## Problem

`DumpedAssembly.resolveBaseType` walks up the inheritance chain and returns which of the four corelib "buckets" the type eventually lands in (`Enum | ValueType | Object | Delegate`). That's one specific question, but it's currently being used to answer several different ones:

- **family membership**: "does this type transitively inherit from `System.ValueType`?" (correct use)
- **exact identity**: "is this type itself one of the well-known corelib bases?" (wrong use — chain-walking produces wrong answers for exact `System.Enum`)
- **representation/ABI**: "is this type value-type-laid-out, or reference-type-laid-out?" (coincides with the family check for constructible types, but the semantics are different)
- **delegate dispatch**: "is this a delegate type?" (a narrow case of family membership)

Concrete bugs this exposes:
1. `typeof(System.Enum).IsValueType` returns `true` in our interpreter; real .NET returns `false`. `System.Enum`'s `BaseType` is `System.ValueType`, so chain-walking lands in `ResolvedBaseType.ValueType`, and `Intrinsics.fs:221-227` reports it as a value type.
2. At BCT construction (`Corelib.fs:145-196`) the loop iterates `bct.Enum` and `bct.ValueType` explicitly (lines 161, 162) and the match at `Corelib.fs:180-184` maps both to `SignatureTypeKind.ValueType`. Real CLR encodes `System.Enum` and `System.ValueType` as CLASS in signatures, so these are being recorded with the wrong kind.

The richer info needed to get either right (exact identity) is already present on `TypeInfo` via `Identity`/`TypeDefHandle`; it's simply being discarded at the query.

## Call-site survey

All uses of `DumpedAssembly.resolveBaseType`, classified by the question they're actually asking:

| Call site | Actual question | Current correctness |
|---|---|---|
| `Corelib.fs:180` | SignatureTypeKind | **BUG** — the iteration at `Corelib.fs:161-162` explicitly feeds `bct.Enum` and `bct.ValueType` through this match, and both land in `ResolvedBaseType.ValueType` and so get classified as `SignatureTypeKind.ValueType`. Real CLR encodes both as CLASS in signatures. |
| `IlMachineState.fs:776-785` (Newobj post-ctor) | is-value-type for push representation | ok (you can't `newobj` abstract base) |
| `IlMachineState.fs:1000-1007` | SignatureTypeKind | ok in practice |
| `IlMachineState.fs:1015-1024` | SignatureTypeKind | ok in practice |
| `UnaryMetadataIlOp.fs:284-358` (Newobj) | is-value-type for push representation | ok (abstract bases unconstructible) |
| `UnaryMetadataIlOp.fs:460-467` (Box) | is-value-type — needs boxing? | ok in practice |
| `UnaryMetadataIlOp.fs:1440-1453` | is-value-type — how to push? | ok in practice |
| `BasicCliType.fs:779-783` (zero of custom type) | is-value-type for layout | ok in practice |
| `Intrinsics.fs:57-68` (`containsRefType` recursion) | is-reference-type (to short-circuit) | ok |
| `Intrinsics.fs:221-227` (`Type.get_IsValueType`) | is-value-type for user-visible reflection | **BUG** for `typeof(System.Enum)` |
| `Intrinsics.fs:522-534` (`IsReferenceOrContainsReferences<T>`) | is-value-type for recursion | ok (user-chosen generic; could plausibly be `System.Enum`, mild risk) |
| `AbstractMachine.fs:32-49` | is-delegate (for invocation dispatch) | ok |

Two live bugs (`Intrinsics.fs:221-227` for user-visible `Type.IsValueType`, and `Corelib.fs:180` which misclassifies `bct.Enum` / `bct.ValueType`'s `SignatureTypeKind` during base-type-table construction). The rest are correct by coincidence because the inputs never reach the confused cases. That coincidence is exactly what the refactor exists to defend against.

## Target API

Introduce intent-named predicates in `WoofWare.PawPrint.Domain/TypeInfo.fs` (same module that already exports `isBaseType` and `resolveBaseType`). Callers should go through these; the two primitives stay as building blocks but stop being the idiom at call sites.

```fsharp
module TypeInfo =
    // --- existing primitives, kept ---
    val isBaseType      : ... -> ResolvedBaseType option  // "exactly one of the four corelib roots?"
    val resolveBaseType : ... -> ResolvedBaseType         // "walk chain, return bucket"

    // --- new intent-named predicates ---

    // ECMA "value type": inherits from System.ValueType (possibly via System.Enum),
    // but is NOT exactly System.ValueType or System.Enum themselves.
    // This is the correct predicate for Type.IsValueType, Box, zero-initialisation,
    // Newobj push kind, SignatureTypeKind selection, etc.
    val isValueType : ... -> bool

    // True iff the type is a delegate (inherits from System.Delegate, excluding
    // System.Delegate itself). Correct for delegate-invocation dispatch.
    val isDelegate : ... -> bool

    // Convenience: not a value type, not a delegate, not an interface.
    // (Or just: not isValueType. Pick whichever callers actually want.)
    val isReferenceType : ... -> bool

    // Metadata layout kind: ValueType for value types, Class otherwise.
    // Replaces the "match resolveBaseType with Enum|ValueType -> ValueType | _ -> Class" pattern.
    val signatureTypeKind : ... -> SignatureTypeKind
```

Each predicate composes `isBaseType` (for exact-identity exclusions) and `resolveBaseType` (for family membership), so the conflation lives in one tested place instead of being spelled out at every call site.

## Implementation stages

### Stage 1 — Failing test (write first, observe red)

**Harness constraint**: the pure-case harness (`WoofWare.PawPrint.Test/TestPureCases.fs:86-117`) does NOT compare printed output. It compiles the C# source, runs real .NET for an expected exit code, then checks the interpreter's top-of-stack `int` matches that exit code. `stdout` is discarded on both sides. So "print and diff" does not work — failures have to be encoded into the return value.

Add `WoofWare.PawPrint.Test/sourcesPure/IsValueType.cs` whose `Main` returns an `int` bitmask (or just the count of mismatches): for each hard-coded `(Type, expectedIsValueType)` pair, compare `t.IsValueType` to the expected literal and set a bit on mismatch. Real .NET will return `0` (assuming the expected literals are correct for real .NET); our interpreter will currently return non-zero because of `System.Enum`. Add the file to `customExitCodes` in `TestPureCases.fs` only if you want to assert a specific non-zero value; the simpler route is to leave `ExpectedReturnCode = 0` so the test naturally stays red until the bug is fixed. The list of cases:

- primitives (`int`, `double`) → `true`
- `string` → `false`
- user struct, user enum → `true`
- user class → `false`
- `System.Enum`, `System.ValueType`, `System.Object`, `System.Delegate`, `System.MulticastDelegate` → all `false`
- user delegate type → `false`

The `System.Enum` case is the one expected to fail before Stage 3f; all others should already be correct and serve as regression guards.

Run and confirm red before changing any production code. (If you'd rather extend the harness to compare stdout, that's a bigger change — scope it as a separate PR, because it affects every pure case and has its own design questions around newline/encoding normalisation.)

Consider a second source `IsValueTypeExhaustive.cs` using the same "bitmask of mismatches" trick over a hard-coded list of ~20 types — the oracle-style check the `property-based-testing` principle asks for in a setting where inputs aren't randomisable but are enumerable.

### Stage 2 — Introduce named predicates (no call-site changes yet)

In `WoofWare.PawPrint.Domain/TypeInfo.fs`:
- `isValueType` = "`resolveBaseType` is `Enum | ValueType`, AND `isBaseType` on the type's own identity is NOT `Some Enum` and NOT `Some ValueType`".
- `isDelegate` = "`resolveBaseType` is `Delegate`, AND not exactly `System.Delegate`" (i.e. a concrete delegate, including anything deriving from `MulticastDelegate`).
- `signatureTypeKind` = if `isValueType` then `ValueType` else `Class`.
- `isReferenceType` = `not isValueType` (or the stricter definition, depending on what callers need — decide during Stage 3).

Unit-test these directly on `BaseClassTypes` inputs: feed in System.Object/Enum/ValueType/Delegate/MulticastDelegate handles and assert the expected truth values. This is where the `System.Enum` bug gets pinned down by a local test, independent of the IL pipeline.

### Stage 3 — Migrate call sites

One PR per cluster, because each is mechanically trivial and reviewable on its own. Order by risk (lowest first; the `IsValueType` fix comes last so Stage 1's red test flips to green at the end, making the bisect story clean):

3a. **SignatureTypeKind callers** — `Corelib.fs:180-184`, `IlMachineState.fs:1000-1024`. Replace the `match resolveBaseType with ...` with `TypeInfo.signatureTypeKind`. Note: this is **not** a pure refactor at `Corelib.fs:180` — it changes the recorded `SignatureTypeKind` for `bct.Enum` and `bct.ValueType` from `ValueType` to `Class`, which is the correct CLR-signature encoding but may shake out downstream consumers that were compensating for the old wrong kind. Run the full suite after this stage and before moving on; any regressions here are bugs being uncovered, not introduced, but they still need addressing (possibly as precursor fixes).

3b. **Newobj / Box / push-kind callers** — `UnaryMetadataIlOp.fs:284-358`, `UnaryMetadataIlOp.fs:460-467`, `UnaryMetadataIlOp.fs:1440-1453`, `IlMachineState.fs:776-785`, `BasicCliType.fs:779-783`. Replace with `if TypeInfo.isValueType ...`. Pure refactor where the old code was correct.

3c. **Reference-type walker** — `Intrinsics.fs:57-68`. Replace with `TypeInfo.isReferenceType` / `isValueType`. Pure refactor.

3d. **Delegate dispatch** — `AbstractMachine.fs:32-49`. Replace with `TypeInfo.isDelegate`. Pure refactor.

3e. **`IsReferenceOrContainsReferences<T>`** — `Intrinsics.fs:522-534`. Replace with `isValueType`. Minor behaviour change only if caller passes `System.Enum` itself as T, which flips from "recurse into fields" to "return true" — and that's the right answer for reflection, though worth calling out in the PR description.

3f. **`Type.get_IsValueType`** — `Intrinsics.fs:221-227`. Replace with `isValueType`. Stage 1's test flips to green.

### Stage 4 — Tighten the primitives (optional)

Once no call site outside `TypeInfo.fs` pattern-matches on `resolveBaseType` directly, decide whether to:
- leave `resolveBaseType` public (no harm, but the footgun remains for new code), or
- make it `internal`/`private` to the module so new callers must go through the named predicates.

Recommend the latter — it's the Principle-2 move (make misuse a compile error). But it's a separate PR because it may flush out call sites not found in the initial survey, and keeping it separate makes the revert story simple.

## Risk & verification

- **Risk**: a call site classified as "correct by coincidence" is actually exercised with exact `System.Enum`/`System.ValueType` in some test not enumerated here. Full `dotnet test` run after each stage; pay attention to any test that names `Type`, `Enum`, or `ValueType`.
- **Risk**: `isDelegate` excludes exact `System.Delegate`, but `AbstractMachine.fs:32` is dispatching on the type of the currently executing method's declaring type — could that ever be `System.Delegate` itself (vs. a concrete delegate)? Methods on `System.Delegate` itself would count, and they'd be instance methods rather than `Invoke` dispatch. Worth checking the existing code path there before flipping; if `System.Delegate` shouldn't take the delegate-dispatch branch, `isDelegate`-excluding-exact is correct; if it should, use a looser predicate. This is the only stage-3 item with a genuine open question.
- **Verification at each stage**: `nix develop -c dotnet test`. Stage 1's new test fires red; stages 3a-3e leave it red but pass everything else; stage 3f flips it to green.
- **Formatting**: `nix develop -c dotnet fantomas .` after each stage.

## Out of scope

- Interfaces and generic-parameter types — `ResolvedBaseType` doesn't model them, and the current callers don't need to distinguish them. Leave for a future refactor if a real need emerges.
- Enriching `ResolvedBaseType` with an `IsExactly` bit. The commenter explicitly rejected this and the rationale stands: the right answer is to stop exposing `ResolvedBaseType` as the call-site-level abstraction, not to make it carry more information.
