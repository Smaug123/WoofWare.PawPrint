# Type-load validity at concretization

Status: proposal, awaiting a choice.

## The rules

Three checks CoreCLR makes of every type it loads, whatever kind of type it is:

| Rule | CoreCLR | Exception |
|---|---|---|
| 1. No instantiation has a byref, `TypedReference`, `void`, pointer or function-pointer argument. Checking stops at the first type variable. | `Generics::CheckInstantiation` (generics.cpp:500), from `ClassLoader::LoadTypeHandleForTypeKey` (clsload.cpp:780) when it creates a new instantiation | `TypeLoadException` (`IDS_CLASSLOAD_INVALIDINSTANTIATION`) |
| 2. A `CLASS`/`VALUETYPE` spelling matches the definition it resolves to, including a `GENERICINST` head. | `SigPointer::GetTypeHandleThrowing` (siginfo.cpp:1766) and `GetGenericInstType` (siginfo.cpp:2045), only under `LoadTypes` | `TypeLoadException` (`BFA_CLASSLOAD_VALUETYPEMISMATCH`) |
| 3. `!n` / `!!n` is inside the context's instantiation. | `SigPointer::GetTypeVariableThrowing` (siginfo.cpp:2062) | `BadImageFormatException` |

PawPrint today: rule 1 is `ConstraintSatisfaction.malformedArgument`, called by nothing outside its test.
Rule 2 is not checked anywhere: `concretizeType` drops the `SignatureTypeKind` it is handed.
Rule 3 is an `IndexOutOfRangeException` thrown out of `TypeConcretization.concretizeType` (TypeConcretisation.fs:1026, 1032).

CoreCLR's order inside a `GENERICINST` is: the head (loaded, then spelling checked), then each argument in turn, then rule 1.
`concretizeGenericInstantiation` currently does the arguments before the head.
The order decides which failure a type with two faults reports.

## Where a handle is minted today

- `TypeConcretization.registerNominal`, reached from `concretizeType` for every nominal type.
- `concretizePrimitive` and the `Void` arm of `concretizeType`, which are non-generic and need no rule-1 check.
- Four callers of `AllConcreteTypes.add` that bypass `registerNominal`:
  `IlMachineTypeResolution.concretizeFieldDeclaringType` (which also skips the base-chain load),
  `ConcreteVirtualDispatch` (line 820), `DefaultInterfaceImplementation` (line 357) and `NativeCastHelpers` (line 90).

Rules 2 and 3 are facts about a *signature*, not about a handle: `List<valuetype String>` and `List<class String>` would intern as the same handle.
So rules 2 and 3 must be checked during the `TypeDefn` walk, whichever option below is chosen.
The options differ only in where rule 1 lives and in what a handle promises.

## Option A: refuse at the mint (recommended)

`AllConcreteTypes.add` becomes the only way to mint a `Concrete` handle, and it returns a `Result`.
It checks rule 1 against the argument handles before interning.
Every `Concrete` handle in the table is then proof that its instantiation passes rule 1.
The walk in `concretizeType` checks rules 2 and 3 at each node, reordered to match CoreCLR (head before arguments).
The four bypass sites take the `Result`.

- Each instantiation is checked once, as it is interned: O(arguments) per new handle, and no memo, because a shared subgraph is interned only once.
- The invariant is enforced by the type of `add`, not by callers remembering to check.
- Cost: `add` needs to recognise `TypedReference` and `Void`, so it takes their identities (or `BaseClassTypes`).
- Risk: PawPrint refuses wherever it concretizes, and it may concretize a type CoreCLR never loads.
  For example, the JIT does not load a `CLASS` local's type, and `CLASS_LOAD_APPROXPARENTS` replaces reference-type arguments of a parent with `__Canon` without reading them.
  The same is already true of `TypeAbsent`, a missing type: PawPrint fails to bind it where CoreCLR may never look.
  So this adds refusals of the existing kind, not a new kind of divergence.
  It is a divergence only if a consumer turns the refusal into a guest exception at a point where CoreCLR loads nothing (see "Representation").

## Option B: a separate validation pass that consumers call

Rule 1 moves to `TypeLoadValidity.check : TypeSystemState -> ConcreteTypeHandle -> Result`, memoised in `TypeSystemState` by a set of handles already validated.
It is called by the consumers that model a CoreCLR full load (field layout, enum normalization, JIT import, `ldtoken`).

- It can match CoreCLR's laziness position by position: a handle may exist without being "loaded".
- It relies on discipline. #2070 showed where that leads: the check landed in one consumer, so a struct and an enum in the same position were treated differently.
- It needs a memo in the state that every consumer must thread through.
- Rules 2 and 3 still go in the walk, so this option splits the three rules across two mechanisms.

## Option C: mint the handle, and record its verdict as data

`ConcreteType` gains a `LoadVerdict` (valid, or the `TypeLoadRefusal` below), computed once at the mint.
Concretization never fails on rule 1; a consumer that models a load reads the verdict and refuses.

- Loses no information, and never refuses more than CoreCLR does.
- A `List<int*>` handle exists in PawPrint where it cannot exist in CoreCLR, so every consumer of a handle has to remember to consult the verdict. That is the discipline problem of option B, with the rule moved into the data.

## Representation

`TypeBindingFailure.Unmodelled` says "PawPrint does not implement this", which is false for these rules.

### R1 (recommended): a new case holding the refusal as data

Add `TypeBindingFailure.Refused of TypeLoadRefusal`, in the Loader beside `TypeBindingFailure`:

```fsharp
[<RequireQualifiedAccess>]
type TypeLoadRefusal =
    /// Rule 1. TypeLoadException.
    | InvalidInstantiation of definition : ResolvedTypeIdentity * argument : int * MalformedArgument
    /// Rule 2. TypeLoadException.
    | KindMismatch of named : ResolvedTypeIdentity * spelled : SignatureTypeKind
    /// Rule 3, for `!index`. BadImageFormatException.
    | TypeParameterOutOfScope of index : int * inScope : int
    /// Rule 3, for `!!index`. BadImageFormatException.
    | MethodParameterOutOfScope of index : int * inScope : int
```

The case says "CoreCLR refuses to load this type, with this exception", not "CoreCLR throws here".
A consumer that knows CoreCLR fully loads this type at this point may turn it into a guest exception.
Every other consumer keeps doing what it does with any binding failure today, which in the interpreter is to stop the run.
Turning refusals into guest exceptions is a follow-up, consumer by consumer.

`MalformedArgument` moves from `ConstraintSatisfaction` to the Loader with it.

Two `Unmodelled` refusals already in `concretizeGenericInstantiation`/`concretizeType` say "CoreCLR refuses that with TypeLoadException, which is not modelled":
a wrong number of type arguments, and a generic definition named without arguments.
They are the same kind of fact, so I would move them onto `TypeLoadRefusal` in the same PR (`WrongArity`, `UninstantiatedDefinition`).
The Loader's `TypeResolution.resolveTypeFromDefn` has the same three `Unmodelled` refusals on its own walk; migrating those is a follow-up, unless you want it here.

### R2: a separate failure type for concretization

`concretizeType` returns `Result<_, ConcretizationFailure>`, where `ConcretizationFailure = Binding of TypeBindingFailure | Refused of TypeLoadRefusal`.
This keeps `TypeBindingFailure` about binding only.
But `concretizeType`'s `TypeBindingFailure` flows through about forty signatures across TypeSystem and the interpreter, and they would all change for the sake of the distinction.
The Loader's walk could not share the type either.

### R3: keep `Unmodelled` and reword the diagnostic

This is how the existing arity refusal works.
It is the smallest change, but the case keeps claiming something false, and a consumer cannot tell a refusal from a gap in PawPrint.

## Blast radius

- Every caller that matches `TypeBindingFailure` exhaustively gets a compile error to resolve. Callers using a wildcard pattern would silently swallow the new case, so I will audit every match on `TypeBindingFailure`.
- In the interpreter, almost every caller of a binding failure is `BindingStop.bound` or `failwith (TypeBindingFailure.describe ...)`. Those keep stopping the run, now with a true diagnostic.
- Real compilers never emit these shapes: C# cannot spell `List<int*>`, Roslyn spells kinds from the definition, and the scope of `!n` is checked. So the Guest suites should not change; I will run them to confirm.
- Rule 3 changes an exception escaping `concretizeType` into a `Result`. Anything that catches `IndexOutOfRangeException` would have relied on that; nothing in the repository does.

## Tests

The prior art in `enum-backing-type` at `99acc335c` (`TestEnumBackingType.fs`) emits raw metadata with `MetadataBuilder` and loads it on the host in a collectible `AssemblyLoadContext`.
I would reuse its emitter, with the struct or field type as the *position*, so one property covers a struct field, an enum backing field and a generic argument alike.

- **Oracle**: the host's `Type.GetType`/`MakeGenericType`/`RuntimeHelpers.RunClassConstructor`, or field layout via `Marshal.SizeOf`/`Unsafe.SizeOf` on a loaded type, giving "loads", `TypeLoadException` or `BadImageFormatException`.
- **Property**: for generated signatures (each of the three faults, nested to depth 0–5 under value-type and reference-type holders, sometimes two faults at once), PawPrint's concretization result agrees with the host: it binds if the host loads, and refuses with the matching `TypeLoadRefusal` if the host throws.
- Where the host is lazy (for example a reference-type argument at `APPROXPARENTS`), I measure which positions it loads, and generate only positions the host fully loads, saying so in the generator.
- Written first and observed failing, as a test-only commit.
