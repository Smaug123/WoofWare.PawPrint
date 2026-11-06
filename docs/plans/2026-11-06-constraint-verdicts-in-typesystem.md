# Constraint verdicts in TypeSystem, stage 1: closed types

Status: proposed (2026-11-06). Nothing here is implemented.

## Why

The escape analysis does not report the exceptions CoreCLR raises when a generic instantiation violates its type parameters' constraints.
PR #2081 made it report them, but put about 2,400 lines of type-system logic in `WoofWare.PawPrint.Analysis/EscapeAnalysis.fs`: a walk over what a full type load brings, bounded by an invented nesting depth of 16; a substitution of `System.__Canon` for shared code; a private `Shape`, a third way of writing a type beside `TypeDefn` and `ConcreteTypeHandle`, with its own ancestor walk bounded at depth 8; and a second constraint checker for open types.
It called `ConstraintSatisfaction.satisfiesClosed` in two places only.
Each of fifteen review rounds found another edge case, and each got another special case, because nothing searched for the edge cases.
It was not merged.

Its test corpus was kept: `WoofWare.PawPrint.Test/TestEscapeAnalysisConstraintViolation.fs` (#2136), every case checked against real CoreCLR out of process, with what main does not yet report recorded in its `unreported` tables.

The redo has three stages:

1. TypeSystem, closed types (this plan): a constraint verdict for a closed type, covering everything its full load brings, and for a closed generic method instantiation. Analysis reads the verdicts.
2. TypeSystem, open types: the rules for an instantiation spelled over type variables, and for code shared over `__Canon`, in the same module, over TypeSystem's existing representations, including the rule that a type variable whose own constraints demand a requirement satisfies it.
3. Interpreter: `NativeRuntimeTypeHelpers.validateConstraintsOn` and its callers switch to TypeSystem's checker, leaving one version of the truth.

## What CoreCLR does

### Where it checks

- A type's constraints are checked as its load completes to `CLASS_LOADED`, at the end of `MethodTable::DoFullyLoad` (methodtable.cpp:4843-4852), through `SatisfiesClassConstraints` (typehandle.cpp:1259), which throws `TypeLoadException` (`IDS_EE_CLASS_CONSTRAINTS_VIOLATION`).
  It does not check the typical instantiation, nor an instantiation shared by generic code (`IsSharedByGenericInstantiations`, methodtable.cpp:4845).
- A method instantiation's constraints are checked as `InstantiatedMethodDesc::NewInstantiatedMethodDesc` first creates it (genmeth.cpp:434-441), through `SatisfiesMethodConstraintsForInstantiation` (genmeth.cpp:309), which throws `VerificationException` (`IDS_EE_METHOD_CONSTRAINTS_VIOLATION`, genmeth.cpp:371).
  It exempts an instantiation with a canonical argument, in the method's or the declaring type's instantiation.
- Each argument is checked by `TypeVarTypeDesc::SatisfiesConstraints` (typedesc.cpp:1491), which `ConstraintSatisfaction.satisfiesClosed` already follows for a closed argument (`TestConstraintSatisfaction` checks it against the host).
- `CEEInfo::GetMethodForSecurity` (jitinterface.cpp:2899) makes the JIT check a generic method's own instructions with the method's type variables as arguments. That is stage 2.

### What a full load brings, in order

`ClassLoader` pushes a type to `CLASS_LOADED` in two passes (`PushFinalLevels`, clsload.cpp:2964-3009): first everything the type reaches to `CLASS_DEPENDENCIES_LOADED`, then everything again to `CLASS_LOADED`, which is where the constraint checks are.
So every missing type in the closure is met before any constraint is checked.
That is why a load that misses raises `FileNotFoundException` or `TypeLoadException` for the miss and never the violation (the corpus's "a load that misses fails before the constraint it would check", which main passes, because the Analysis already runs `TypeLoad.loadMissFrom` first).

Within a pass, `MethodTable::DoFullyLoad` (methodtable.cpp:4522) visits, depth first:

1. an array's element type (4589);
2. the typical instantiation, for a type that is not one (4609);
3. the parent (4645);
4. each interface in the interface map, which holds inherited interfaces too (4664);
5. each generic argument (4684);
6. the canonical method table, for a type that is not canonical (4691);
7. the type of each value-type field with a `FieldDesc`, static and instance (4699), and of each generic static value-type field (4723), unless the module skips type validation (4600, ReadyToRun);
8. then, for this type, `SatisfiesClassConstraints`.

The check is post-order: a part's violation is raised before its owner's.
`TypeDesc::DoFullyLoad` (typedesc.cpp:583) visits a pointer's, byref's or array's element (`HasTypeParam`, typedesc.cpp:632), and nothing of a function pointer: a function pointer's signature is not brought to `CLASS_LOADED`, so a violation inside one is not raised by the full load.
(That is a reading of the source, to be confirmed by the property test below before the walk relies on it.)

Every failure of the second pass is a `TypeLoadException`: a violated constraint, a covariant return that does not validate (`ValidateMethodsWithCovariantReturnTypes`, 4565), an inaccessible parent or interface, circular constraints, an unimplemented static virtual.
So the order among the constraint checks of one full load does not decide what is raised, only its message.
The verdict needs to say whether some instantiation in the closure is violated, not which one CoreCLR meets first; it reports the first in post-order as a witness, for diagnostics.

### How it bounds the walk

No number bounds it.

- A type already on the current path is not walked again (`RecursionGraph::HasSeenType`, methodtable.cpp:4534); the caller is marked bailed, and its promotion to `CLASS_LOADED` is deferred to a pending list, promoted only when the whole closure has passed (clsload.cpp:2987-3008).
  The identity compared is the exact `TypeHandle`: an instantiation, not a definition.
- A type already at the level is not walked again (4540).
- A typical generic definition, as it reaches `CLASS_DEPENDENCIES_LOADED`, is checked for an *expanding cycle* in ECMA-335 II.9.2's sense (`RecursionGraph::CheckForIllegalRecursion`, methodtable.cpp:4637; generics.cpp:555): the graph whose nodes are type parameters, with an edge from `T` to `U` wherever `T` appears in the argument for `U` of a parent or interface, expanding where it appears strictly inside it.
  An expanding cycle throws `TypeLoadException` (`IDS_CLASSLOAD_GENERICTYPE_RECURSIVE`).
  The graph has edges only from parents and interfaces, and looks through arrays, pointers and byrefs (`HasTypeParam`, generics.cpp:674) but not into a function pointer (generics.cpp:686): `S<T> : I<S<delegate*<T, void>[]>>` has no edge, and loading `S<int>` overflows the stack. The corpus's growing-load test confirms that the host's process dies.
- Without an expanding cycle, the set of instantiations reachable through parents and interfaces is finite (ECMA-335 II.9.2). Generic arguments are subterms and add nothing new. Value-type fields are not in CoreCLR's graph. What it does with a field whose type grows, instance (`struct S<T> { S<S<T>> F; }`, an infinitely large struct, which C# refuses but IL can spell) or static (`struct S<T> { static S<S<T>> F; }`), is unmeasured (open question 2).

## Design options

All three options reuse `satisfiesClosed` for the per-instantiation check, and add a closure verdict on top. They differ in what the verdict is a function of, when it is computed, and where its walk lives.

```fsharp
[<RequireQualifiedAccess>]
type FullLoadVerdict =
    /// No instantiation the full load brings to `CLASS_LOADED` violates its constraints.
    | Satisfied
    /// This instantiation, which the full load brings, violates them: the first in CoreCLR's
    /// post-order, though any would raise the same `TypeLoadException`.
    | Violated of owner : ConcreteTypeHandle * ConstraintViolation
    /// CoreCLR's load of a typical definition the walk reaches finds an expanding cycle.
    | Recursive of definition : ResolvedTypeIdentity
    /// Not decided, and why (each case a reason, as data): an argument TypeSystem does not
    /// instantiate (a custom modifier); a static abstract interface constraint
    /// (`ConstraintVerdict.Undecided`); growth CoreCLR does not detect, which overflows its stack.
    | Undecided of FullLoadUndecided
```

### Option 1 (recommended): a lazy walk over `ConcreteTypeHandle`, memoised per handle

`ConstraintSatisfaction.fullLoadVerdict : ... -> TypeSystemState -> ConcreteTypeHandle -> TypeSystemState * Result<FullLoadVerdict, TypeBindingFailure>`.

- The walk follows the list above over concrete handles: the parts are `TypeLoad.fullLoadParts` of the handle's definition, concretised under the handle's arguments with `TypeSystemState.concretizeType`, plus the handle's own arguments and, for an array, pointer or byref, its element.
- Its identity is CoreCLR's: the exact handle. The visited set is a set of handles; a handle on the current path makes its ancestors' verdicts provisional, as CoreCLR's pending list does, and only a verdict computed without meeting the path is memoised.
- It is bounded by the structure, not a number. Before concretising a definition's parts, it decides once per definition whether the definition's parameter graph has an expanding cycle, over parents and interfaces as CoreCLR does (giving `Recursive`), and over the edges CoreCLR does not check but that grow (a function pointer's signature, a static value-type field; giving `Undecided`). Without either, the closure is finite, so the walk ends.
- The memo is a new `TypeSystemState` field, `_FullLoadVerdicts : Map<ConcreteTypeHandle, FullLoadVerdict>`, added through a `With...` method, like `_VirtualSlotTables`. The per-definition cycle verdict is a second memo keyed by `ResolvedTypeIdentity`.
- The method verdict is `ConstraintSatisfaction.methodInstantiationVerdict` over the exact declaring type's handle and the method's argument handles, which is `satisfiesClosed` with the method's parameters, after the declaring type's own full-load verdict.
- Consumers: Analysis asks wherever it already asks `TypeLoad.loadMissFrom` of a type the JIT loads in full, once that reports no miss and the spelling is closed in the instance; the interpreter can ask with handles it already has (stage 3).

For: one identity, the one CoreCLR uses and the interpreter already has; `satisfiesClosed` and `concretizeType` reused unchanged; the walk's termination follows from a decidable property of finitely many definitions; the memo is the existing pattern; a property test can compare a memoised verdict with one from a fresh state.
Against: it concretises every part it walks, so it registers handles CoreCLR also creates but PawPrint otherwise would not, which costs memory in a long analysis (measure: allocation per verdict over CoreLib's generic types); a part TypeSystem does not concretise (a modified argument) is `Undecided` rather than looked through.
Blast radius: new functions and one new memo in TypeSystem; nothing existing changes behaviour until a consumer calls it. Reversible by deleting the consumer's call.

### Option 2: a walk over spellings, folded into the Loader's load walk

Extend `TypeLoad`'s walk, which already visits the full load's parts in CoreCLR's order but tracks definitions (`ResolvedTypeIdentity`), so that it also carries each instantiation's arguments, and checks constraints at each instantiation's post-order point, in one walk that returns the first miss or the first violation.

For: one walk gives the order of misses and violations together; nothing is concretised that CoreCLR would not load; a modified argument can be looked through rather than refused.
Against: the Loader cannot see TypeSystem, and assignability needs it, so the walk would have to move up into TypeSystem or take the check as a function argument (a dependency injected into the Loader, against the repository's dependency rejection). Carrying arguments through the walk needs spellings substituted across assemblies, and a `TypeDefn` from one assembly cannot hold a reference resolved in another: that is exactly what led #2081 to invent `Shape`. It changes the order and memo of a walk the Analysis and the interpreter depend on today.
Blast radius: every caller of `TypeLoad.loadMissFrom`. Hard to reverse once a second representation exists.

### Option 3: a verdict recorded at the mint

Like the type-load validity rules (`docs/plans/2026-10-28-type-load-validity.md`, option A), `AllConcreteTypes.add` would compute the instantiation's own constraint verdict when it interns a handle, and the closure verdict would be a fold of the recorded verdicts.

For: no memo threaded by consumers; each instantiation checked once.
Against: CoreCLR creates handles for violating instantiations without failing (an approximate parent, a class-typed local the JIT does not load, every instantiation in shared code), so the verdict could only be recorded, never refused, which is option C of that plan with its discipline problem. And deciding a verdict needs assignability, which concretises more types, so minting would recurse into minting; every concretisation in the interpreter would pay for it.
Blast radius: every handle minted anywhere. Rejected.

### Recommendation

Option 1. It is the smallest change that keeps one identity, it can be wrong only in ways a host oracle can find (which instantiations the walk brings, and whether each is violated), and it is reversible.
The questions it leaves open are about facts, not structure (open questions 1-3), and the property test answers them.

## What #2081's Analysis code did, and where it goes

| #2081 (`EscapeAnalysis.fs`) | What it is | Where it goes |
|---|---|---|
| `closureConstraintFailure`, `closureNestingLimit` (16), `nestingOf` | the full-load walk over handles | stage 1, `fullLoadVerdict`, bounded by the expanding-cycle check instead of 16 |
| `methodInstantiationFailure` | a closed method instantiation's check, memoised | stage 1, `methodInstantiationVerdict` |
| `uninstantiableConstraints`, `constraintsConcretizable` | a constraint TypeSystem cannot concretise is undecided | stage 1, a `FullLoadUndecided` reason |
| `verdictFailure`, `failureOfBinding`, `BindFailure` | verdict to the exception CoreCLR raises, and how it is made | stays in Analysis (it maps a verdict to an `ExceptionMaking`) |
| `fullLoadConstraints`, `jitFullLoad`, `firstJitFullLoadFailure` | which types the JIT loads in full, then the verdict | stays in Analysis, which already decides what the JIT loads (`JitTypeLoads`); it calls stage 1 |
| `InstanceCheck`, `CheckReach`, `InstanceConstraintFacts`, `LocalFacts.InstanceChecks` | where in a body a check happens, and whether the method or its inliner raises it | stays in Analysis, simplified: in stage 1 every check is closed in the instance, so a check is a pending type or method verdict keyed by the instance |
| `canonHandle`, `containsCanon`, `canonicalArgument` | `__Canon` substitution for shared code | stage 2, over `CodeSharing` |
| `closedPartsFailure` | the closed parts of a spelling over type variables, checked in every instantiation | stage 2 |
| `Shape`, `shapeOf`, `ancestorShapes` (depth 8), `argumentSatisfies`, `instantiationMayBreak`, `shapeMayBreak`, `spellingMayBreakConstraint`, `specificationMayBreakConstraint`, `VariableScope`, `constrains` | the open-type checker, with the rule that a variable whose constraints demand a requirement satisfies it | stage 2, over `TypeDefn` with its generic context, or a handle for a type variable (the representation is stage 2's decision); the ancestor walk is TypeSystem's existing assignability, so no depth bound |
| `spellingConstraintFree`, `mayBeByRefLike` | a short cut where nothing is constrained | not needed unless measured: a memo per handle makes the common case one lookup |
| `namesBareGenericDefinition` | a type spec naming a generic definition bare | not needed: main's Loader refuses it (#2086) |
| the README paragraphs | the analysis's documentation | rewritten per stage, describing TypeSystem's verdict |

## The property-based test

Load the `property-based-testing` skill before writing it; `TestConstraintSatisfaction.fs` shows the FsCheck conventions (a `Tally` against vacuity, `Config.QuickThrowOnFailure.WithMaxTest`), and `TestEscapeAnalysisConstraintViolation.fs` the out-of-process driver.

### The generator

A *universe* is data, rendered to C#:

- `Lib`: up to three generic definitions (class, struct or interface; arity 1 or 2), each parameter with a subset of `class`, `struct`, `new()`, `allows ref struct` and up to two type constraints drawn from a few leaf types (an interface `IFoo`, a class `Base`, `IComparable`, `IEquatable<T>` over its own parameter, `IEnumerable<object>`). `Lib` is compiled twice, with its constraints and without (each parameter `allows ref struct`), as the corpus does.
- `Client`: up to four definitions with no constraints, each a class, struct or interface of arity 0 to 2, with a base (classes), up to two interfaces, and up to two value-type fields, static or instance, each spelled from a *type expression*: a leaf (`int`, `string`, `object`, `int?`, `DayOfWeek`, a struct `FooStruct : IFoo`, a class `Foo : Base, IFoo` with a public constructor, an abstract class, a `ref struct`), one of the definition's own parameters, an array of an expression, or an instantiation of a `Lib` or `Client` definition over expressions, nested to depth 3.
- Roots: up to four closed type expressions, each loaded by a method of `Client.Cases` as `typeof(root)` inside a `try`/`catch`, so that the host's JIT loads it in full before the body runs.

`Client` is compiled against the unconstrained `Lib` and run against the constrained one, so it can spell any instantiation.
The generator keeps only universes Roslyn compiles (it rejects, for instance, a struct field cycle) by discarding a failed compilation, and counts discards: more than a quarter discarded fails the run, since the generator is then mostly testing Roslyn.
Function pointers are not generated in the first version: growth through one kills the host process, which would lose a batch (see below for when to add them).

### The property

For every root `R` of every universe: TypeSystem's `fullLoadVerdict` of `R`'s handle agrees with the host.

- The host's outcome is `Runs`, `TypeLoadException` with the constraint-violation message, `TypeLoadException` with the recursive-generic message, another exception, or the process dying. The driver runs with invariant globalization and records the message, so the two `TypeLoadException`s are told apart.
- `Violated` and `Recursive` must match the host's `TypeLoadException` of that kind; `Satisfied` must match `Runs`; `Undecided` is allowed for any outcome, but each reason must be one the universe can produce (a static abstract constraint, which the generator does not make, never), and the tally fails the run if more than a tenth of verdicts are undecided.
- The tally also requires at least a tenth of decided roots to be `Violated` and a tenth `Satisfied`, so that neither direction passes vacuously.

A second property needs no host: the verdict from a state that has memoised other verdicts first equals the verdict from a fresh state (the memo is honest, checked as `_VirtualSlotTables`'s was), and the verdict of each root is the same whatever order the roots are asked in.

A third, end to end, once Analysis consumes the verdict (PR 2): the escape analysis of each root's `Cases` method names `TypeLoadException` exactly where the host raises it before the body runs.

### Shrinking

A failing universe shrinks by dropping roots, then definitions no root reaches, then fields, interfaces, bases and constraints, and by replacing a type expression with one of its subterms or with a leaf.
The shrinker is hand-written over the universe's data (FsCheck's derived shrinker does not know which definitions are reachable).
Each shrink step needs the host again; it is run only on failure, so its cost is acceptable, and the run prints the shrunk universe's C# so that it can become an example test.

### Cost, the 8 GiB cap and the machine's load

- Per batch: three Roslyn compilations of one source holding every universe of the batch in its own namespace, one driver process, and one `TypeSystemState` over CoreLib and the two assemblies. Universes are batched, 25 to a batch, because a compilation and a process cost seconds and a verdict milliseconds.
- One FsCheck case is a batch, so a failing batch shrinks first to its failing universe (a list shrink) and then within it.
- The default run is about 8 batches (200 universes, 800 roots), sequential: FsCheck's parallel mode slowed the suite by 40% when measured, and the cap is per process. A soak variant with more batches is `[<Explicit>]`.
- Memory is measured by allocation, not time: the test asserts nothing about it, but the PR reports the allocation of one batch (from `GC.GetAllocatedBytesForCurrentThread`) and the peak working set, which must be a small fraction of the 8 GiB cap. A `TypeSystemState` is dropped after each batch.
- The driver writes and flushes each root's outcome before running the next, so if a root kills the process, the root it was running is known (a stack overflow on .NET 10 mostly ends the process by SIGSEGV with nothing on stderr, measured for #2136, so the outcome file, not stderr, must say which root died): it is recorded as "the process dies", and the rest of the batch runs in a second process. With that, function-pointer growth can be generated too, and must then be `Undecided`.

## How stage 1 splits into PRs

Each PR has a test-only commit first, whose message quotes its failing output, and then the implementation. Each removes from the corpus's records only what its own run shows it now reports; the records' ratchet fails the run if a recorded case is reported and still listed.

1. **The full-load verdict in TypeSystem.** `FullLoadVerdict`, `fullLoadVerdict` and its memo, with the expanding-cycle check. Tests: the host property and the memo property above; example tests for what the property found while it was written. Analysis consumes nothing. Removes no record.
2. **Analysis reports a type's violation where the JIT loads it in full.** Wherever Analysis asks `TypeLoad.loadMissFrom` of a type the JIT loads in full (a token's type, an owner of a member a token names, a local, the signature types it loads, a field's type, a value type of a signature it prepares as a function pointer), it asks the verdict after the miss check: `Violated` and `Recursive` raise `TypeLoadException` made as CoreCLR makes it (to be read in `EEMessageException`, not assumed); `Undecided` is opaque, with a new `Opacity` case. Records it should remove: the positions `StaticCall`, `Newobj`, `Ldtoken`, `Isinst`, `Newarr`, `BaseClass`, `CalleeParameter`, `ValueField`; `MissingMemberOfViolatingParent` (both); `ModifiedArgument.PassesPair` and `.CallsGeneric`, `ModifiedByRefLike.TakesToken` (undecided, so "unknown"); `OpaqueArgument.CallsGeneric`; `ClosedPartThroughCycle.CallsA`/`CallsB` and `ParameterThroughCycle.CallsX`/`CallsY` (closed in the instance the case calls); the inlined address.
3. **The method instantiation verdict, and Analysis reports it.** `methodInstantiationVerdict` in TypeSystem, with its host property (the generator gains generic methods with constraints, called by method specifications), then Analysis at `call`, `callvirt`, `newobj`, `ldftn` and `ldvirtftn` of a closed method specification: `VerificationException`. Records it should remove: the positions `MethodSpecCall` and `MethodSpecLdftn`; `MethodAndReturnViolated.CallsM`; `RejectedCallee.CallsRun`; `OpaqueArgument.CallsGenericMethod`; `ModifiedByRefLike.TakesAddress` and `ModifiedMethodArgument.TakesConstrainedAddress` (undecided); `CallsClosedReturn`, `Plain::CallsMR`, `CallsPartlyClosedReturn`, `Plain::CallsMP`.
4. **README.** `WoofWare.PawPrint.Analysis/README.md` drops "A generic instantiation the caller spells that a constraint added since now rejects is not checked", and says what is; `WoofWare.PawPrint.TypeSystem/README.md` lists the verdict among what the package decides. (Each of 2 and 3 may carry its own README change instead; one PR keeps the wording consistent.)

What stays recorded after stage 1, for stage 2: the `Shared*` and `SharedClosedBase` positions and every generic method asked about by itself; `PartlyOpenMethod`, `PartlyOpenType`, `OpenParentClosedMethod`, `ClosedPartOfReturn`, `SubstitutedClosedPart`, `AbstractOverVariable` (shared code over `__Canon`); the inherited-base test (shared); and the IL C# does not write (`CallsRun`, `CallsRunMake`, `CallsPointing`, `CallsPointingType`, `CallsTypedReference` and their definitions), which are about typical instantiations and malformed instantiations over a type variable.
The lists above are predictions; the PR's run decides.

## Open questions

1. **Scope of stage 1.** Does a closed method instantiation (PR 3) belong in stage 1, as proposed, or wait for stage 2? It is closed, uses the same `satisfiesClosed`, and should remove about a dozen records, but the brief named closed *types*.
2. **Growth CoreCLR does not detect.** CoreCLR overflows its stack on growth through a function pointer; what it does with a growing value-type field, instance or static, is unmeasured, and PR 1 measures it first. Should the verdict be `Undecided` there (the analysis then answers "unknown"), or a distinct case saying the process dies, which the Analysis would report as `StackOverflowException`, which is what happens? The second is more precise and the corpus's growing-load test accepts either.
3. **Function pointers in the walk.** Reading typedesc.cpp:632, a function pointer's signature is not brought to `CLASS_LOADED`, so a violation inside one is not raised by the full load, while `TypeLoad`'s miss walk does look into function pointers (a miss is met at a lower level). The walk will follow the reading; the property test (with function pointers generated, once the driver survives a dying process) confirms or refutes it. Agree that this is measured rather than argued?
4. **Memory.** Option 1 concretises every part it walks. If a long analysis's allocation grows noticeably (to be measured in PR 1 over every generic type CoreLib defines), is a per-definition summary ("this definition's closure is satisfied for every argument that satisfies X") worth its complexity, or is the memo enough?
5. **ReadyToRun.** CoreCLR skips the value-type field part and the constraint checks of its own definition in an image that skips type validation (methodtable.cpp:4600). `TypeLoad` already notes it cannot tell such an image; the verdict inherits that, over-approximating. Keep it so?
