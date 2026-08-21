# Answering collectibility: the `IsCollectible` QCall family

Status: plan, awaiting a decision on the option set below. Slice selected as the next
rung E blocker after #1110 (`Signature_Init` under an open generic definition).

## Measured

**The family is four entry points, not the two rung E hits.** Found by grepping the
pinned runtime for every QCall `EntryPoint` naming collectibility, rather than by
following rung E:

| entry point | managed caller | reachable on `origin/main` today? |
| --- | --- | --- |
| `RuntimeTypeHandle_IsCollectible` | `RuntimeType.IsCollectible` (RuntimeType.CoreCLR.cs:3288) | **yes** |
| `RuntimeMethodHandle_GetIsCollectible` | `RuntimeMethodInfo.IsCollectible` (RuntimeMethodInfo.CoreCLR.cs:329) | **yes** |
| `AssemblyNative_GetIsCollectible` | `RuntimeAssembly.IsCollectible` (RuntimeAssembly.cs:258) | **yes** |
| `RuntimeTypeHandle_RegisterCollectibleTypeDependency` | `TypeNameResolver` (TypeNameResolver.CoreCLR.cs:128) | **no** |

**This slice does not depend on #1110.** A four-line guest — `typeof(int).IsCollectible`,
`typeof(Boring).IsCollectible`, a `MethodInfo`, an `Assembly` — reaches the first three
on plain `origin/main` at `3ba7ed8c`, in that order, with no generics and no expression
trees involved. So it branches from main rather than stacking, and the ladder is not the
only driver.

**The fourth is out of reach for a measured reason, not a guessed one.** `Type.GetType`
with an assembly-qualified name stops earlier, at `AssemblyNative_GetExecutingAssembly`,
so `RegisterCollectibleTypeDependency` cannot be exercised by any guest today. It is
excluded from this slice on that basis. (Its body is also a no-op unless the type's
loader allocator is collectible, so it will be a no-op handler whenever it does become
reachable.)

**Every answer is `false`, measured across 14 shapes on real .NET** — `int`, a user
class, an instance method, a generic method definition, a generic method instantiation,
a constructor, the user assembly, corelib, `List<int>`, `List<>`, `List<>`'s own type
variable, `int[]`, `int&`, `int*`. Not inferred from "PawPrint has no collectible ALC":
these are the answers a normally-loaded program actually gets, and structural types and
open definitions were included precisely because they are the shapes that would answer
oddly if collectibility were derived from something other than the loader allocator.

**`DynamicMethod.IsCollectible` is `true`, and that does not contradict the above.**
Measured. But `DynamicMethod` does not override `IsCollectible`, so it takes
`MemberInfo.IsCollectible => true` — the *managed* virtual default (MemberInfo.cs:40) —
and no QCall runs. CoreCLR's own LCG loader allocator is the *module's*
(`DynamicMethodTable::MakeMethodTable`, dynamicmethod.cpp:113), which in the default ALC
is non-collectible, so the QCall would answer `false` where the managed default answers
`true`. The two genuinely disagree for that shape and .NET's answer comes from the
managed side. This is what makes "always `false`" the right answer for the *QCall*
rather than a lucky guess: the one input that is collectible never arrives here.

**There is already precedent in the tree for this fact.**
`RuntimeMethodHandle.GetLoaderAllocatorInternal` (NativeRuntimeMethodHandle.fs:1517)
returns null and says why: `SetupManagedTracking` populates the exposed object only for
collectible loader allocators, and "non-collectible assemblies — i.e. everything PawPrint
currently loads — leave the handle null". So this slice is adding three more answers to a
question the tree has already answered once, which is what makes the sharing question
below worth asking rather than obvious.

## Should the ALC strategy be decided first?

Asked before choosing between the options, because if the answer were yes the options
would be the wrong question. It is no, and the census that settles it is worth recording
because it also changes *why* Option B is right.

**There is no ALC surface at all to be consistent with.** Every entry point is missing:
`GetLoadContextForAssembly`, `GetLoadedAssemblies`, `InitializeAssemblyLoadContext`,
`LoadFromPath`, `LoadFromStream`, `PrepareForAssemblyLoadContextRelease`, and
`AssemblyNative_InternalLoad`. So a guest cannot load an assembly at runtime by *any*
route — `Assembly.Load` included. Measured, not inferred: a guest asking
`AssemblyLoadContext.GetLoadContext(typeof(Program).Assembly)` stops at
`AssemblyNative_GetLoadContextForAssembly`. "One context" is therefore not an assumption
that might be silently wrong; it is a consequence of there being no way to make a second.

**Collectibility enters CoreCLR through exactly one door, and it is one of those missing
natives.** `m_IsCollectible` is assigned in one place (loaderallocator.cpp:80), and the
only caller that passes `true` is `AssemblyNative_InitializeAssemblyLoadContext`'s
`fIsCollectible` branch (assemblynative.cpp:1194-1197), which constructs the
`AssemblyLoaderAllocator`. Everything else shares the process's `GlobalLoaderAllocator`,
which is non-collectible. So `true` is unreachable *by construction* rather than by
accident, and that native is the natural trip-wire.

**Nothing wants multiple contexts.** No plan, no parked test, no roadmap item: the
ASP.NET critical path's Phases 0-4 mention ALCs, dynamic loading, plugins and Razor
exactly zero times, and rungs B/F/G pass without touching any of it. The 24 parked tests
in `unimplemented` contain no load-context entry. Deciding the strategy now would be
designing against no requirement.

**And the decision is far larger than these three handlers.** The real question is not
"how do we model contexts" but "may a type's identity include its load context". Today it
may not: `ResolvedTypeIdentity` is `(assembly display name, TypeDef handle)`
(TypeIdentity.fs:43-61), and the equality, hashing and ordering of *every* type in the
system are over that pair — so two contexts holding one identity would make two different
types compare equal. `LoadedAssemblies` is one flat `ImmutableDictionary<string,
DumpedAssembly>` keyed on that same string (Assembly.fs:799-806), with roughly **165
lookup sites across ~45 files**, and `_VirtualSlotTables` is keyed on
`ResolvedTypeIdentity` with a docstring resting on there being no unloading
(IlMachineStateModel.fs:62-63). Answering the ALC question means changing type identity
itself. That is not a decision taken to unblock three one-line handlers.

**The assumption is already machine-enforced, and loudly.**
`LoadedAssemblies.Canonicalise` (Assembly.fs:859-885) crashes if two distinct images
claim one definition identity — "Refusing to guess which one %s refers to". So the
failure mode of the current model is a crash rather than silent corruption, which is the
right way round and removes the urgency argument.

**What the instinct is right about.** PawPrint already asserts "no collectible loader
allocator" in six independent places — `GetLoaderAllocatorInternal`
(NativeRuntimeMethodHandle.fs:1526), `MethodHandleRegistry.fs:470`,
`NativeDelegate.fs:644`, `NativeRuntimeFieldHandle.fs:136`, and two plan docs — each
having worked the reasoning out again. That scattering is the real cost, and it is what
this slice should fix. So the up-front work worth doing is not an ALC design; it is
naming the fact once, which is Option B — chosen on this evidence rather than on taste.

What a future ALC decision will have to settle, recorded so the deferral is informed
rather than blank: whether a load context is part of type identity; whether binding
becomes per-context (today `TypeResolution.directoryLoader` binds by *simple name only*,
ignoring version, culture and public key token — TypeResolution.fs:22-27); whether
context identity is guest-visible (it is: `AssemblyLoadContext.Default` reference
identity, `.Name`, and `AssemblyLoadContext.All`, all measured); and what unloading means
for a deterministic replay.

## Options

`false` is the *correct* answer, not a documented divergence, so "refuse loudly" is not a
candidate — it would break a guest that PawPrint can serve correctly. Nor is a field on
`DumpedAssembly`: a loader allocator belongs to the load context, and one image can be
loaded into several contexts, so the image record is the wrong home for it (and
`WoofWare.PawPrint.Domain` is a published package, so it would be a public API change for
a value that does not belong to that entity). That leaves two.

### Option A — three independent handlers

Each of the three entry points gets a handler in its own module
(`NativeRuntimeTypeQCall`, `NativeRuntimeMethodHandle`, `NativeRuntimeAssembly`) pushing
`Interop.BOOL.FALSE`, each carrying its own comment explaining why.

- Minimal, and matches how every other unrelated QCall in these modules is written.
- No new abstraction to justify: the "classifier" would be a constant function today,
  which is the shape "no speculative generality" warns about.
- Cost: the same paragraph of reasoning is written three times, and a future
  collectible-ALC change has to find all three (plus `GetLoaderAllocatorInternal`)
  rather than one.

### Option B — one classifier the three handlers ask

A single function — `EmulatedRuntime.isCollectible` or similar — taking the thing whose
collectibility is being asked about, returning `false`, with one docstring holding the
fact. The three handlers become thin, and `GetLoaderAllocatorInternal`'s existing null
return is expressed in terms of it.

- The fact is single-sourced, and the future change is one edit with a compiler-checked
  set of callers rather than a grep.
- It takes a real argument, so it is a function whose *body* is a constant rather than a
  function that is definitionally constant — which is the difference between a stub and a
  speculative abstraction.
- Cost: one function that today ignores its argument, which is a fair thing to call
  premature.

**Recommendation: Option B**, on the grounds that four sites already share this one fact
and the reasoning is long enough that three copies of it will drift. But this is a
judgement call about when an abstraction has earned its place, and it is exactly the kind
of call the project asks be put to a human rather than assumed — so this plan stops here
for a decision.

## Tests, either way

`sourcesPure/ReflectionIsCollectible.cs` (new, differential): the 14 shapes measured
above, each asserted `false`, with the exit code naming the first failure. Worth the
breadth rather than one check per entry point, because a handler that answered from
something other than "nothing here is collectible" — say, from whether the type has
metadata — would pass a narrow file and fail on `int[]` or `List<>`.

Not satisfiable the wrong way: a mutant returning `true` fails at check 1, so the file
also has to pin that the *guest* really read the value rather than that nothing threw.
The `MemberInfo` default is what makes that possible — a shape whose answer is `true` on
real .NET is not reachable from a pure guest, so a "correct" reading is the only way for
every check to pass while the QCalls are actually being called. Mutation testing will
confirm that each of the three entry points is genuinely exercised, by refusing one at a
time and checking the file goes red each time; without that, one unimplemented handler
could hide behind another's earlier failure.

`DynamicMethod.IsCollectible` is deliberately absent: PawPrint reports dynamic code
unsupported, so a pure guest cannot build one, and the answer comes from managed code
anyway.

## After this

Rung E's remaining blockers, measured by stubbing forward on the #1110 branch:
`Delegate_BindToMethodInfo` for a *metadata* method in `System.Linq.Expressions`, which
is implemented only for a `Reflection.Emit`-minted method. That is the last one before
`Expression.Compile()` completes, and it is a bigger question than these three — it is
about what a delegate over an interpreter-resolved method means when the BCL expected a
dynamic one.
