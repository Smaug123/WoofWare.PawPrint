# Can PawPrint tell us what a method can throw?

An investigation. Everything numeric below was measured in this branch;
`docs/probes/exception-escape/` is the instrument and its `measured-*.txt` files are the outputs
quoted here. The runtime measured is the pinned managed linux-x64 pack at 10.0.7
(`$DOTNET_LINUX_FRAMEWORK_DIR`).

## The question

Could PawPrint grow an analyser that, given a method, reports the exceptions that can escape it?
The wider goal is a general analysis engine for .NET, of which escaping exceptions is the first
and most tractable question. Others named as targets: "did this change newly make
`OperationCanceledException` possible", "is this method pure", and "what files does this package
ever attempt to read".

The prior suspicion was that PawPrint's shape blocks this: the interpreter is a model of the CLR
that gets mutated, which is not a thing you can reason over, and the native-call registry is a list
of opaque functions, so the exceptions a native call can raise are not inspectable.

The second half of that is right, and worse than suspected. The first half is mostly wrong, for a
reason worth stating precisely: **escaping exceptions is a question about control flow and types,
not about values.** The parts of PawPrint that are hard to reason over are the parts that model
values and memory, and an escape analysis needs those only for precision, never for soundness.

## What already exists

| layer | size | can an analyser use it today? |
| --- | --- | --- |
| `WoofWare.PawPrint.Domain` | 14,567 lines | Yes. `WoofWare.PawPrint.IlDump` already depends on it alone and reads real assemblies with it. |
| `WoofWare.PawPrint`, below `IlMachineStateModel.fs` in compile order | 50 compile units, 21,586 lines | Yes in principle — the F# compile order already forbids these from seeing `IlMachineState`. |
| `WoofWare.PawPrint`, from `IlMachineStateModel.fs` on | 98 compile units, 79,900 lines | No, as written. |
| `WoofWare.PosixKernel` | 13,144 lines | Yes; it may not even reference PawPrint, and a test asserts it. |

Domain carries everything the *front half* of an analyser needs — every one of these was read by
the probe with no help from the interpreter:

* decoded IL, offset-indexed (`MethodInstructions`);
* exception regions with their clause kinds and catch types (`ExceptionRegion`);
* the type graph, including base types (`TypeInfo.BaseType`);
* signature blobs decoded to `TypeDefn`, so a `TypeSpec` can be walked to the nominal type at its
  root — which is what lets the probe tell "an instantiation of a local type" from "a call into
  another assembly" at all;
* and `MethodBody` already distinguishes `Il`, `InternalCall`, `PInvoke`, `RuntimeProvided` and
  `Abstract`. That is exactly the "which methods have no IL, and for which of two quite different
  reasons?" classification an analyser has to make, and it is already a total DU.

Reading CoreLib takes ~1.1s and yields 41,561 methods, 95.0% with IL bodies.

### The interpreter is less entangled than it looks

`IlMachineState` is mentioned 3,200 times across 98 of the main project's 144 source files, which
is the number that makes this look impossible. It is the wrong number.

Of the 91 files above the line that mention `IlMachineState`, **30 files and 14,712 lines never
name a guest-observable field at all** — not the heap, not thread state, not statics, not the
kernel, not the scheduler. They thread the state only because it is where the loader cache and the
concrete-type registry happen to live. The two largest are the two the analyser most needs:

* `VirtualSlotLayout.fs` (1,927 lines) — virtual dispatch. Mentions `IlMachineState` 50 times and
  reads **zero** guest-observable fields.
* `IlMachineTypeResolution.fs` (975 lines) — type resolution. Its functions are already thin
  adapters: `resolveTypeFromRef` calls `TypeResolution.resolveTypeFromRef` (which lives *below* the
  line, taking and returning a `LoadedAssemblies`) and then stores the result back into the record.

So the "program universe" layer that an analyser and the interpreter would share is not something
to be designed and migrated to. It already exists, scattered, and needs a name and a record to live
in. That is a mechanical refactor whose blast radius is a compile error at every call site.

(That census is a lower bound on purity, not a proof: a file could reach guest state through a
helper rather than by naming a field. Each file must be checked before it is lifted.)

## What an escaping-exception answer actually needs

The probe implements a real interprocedural fixpoint — local raises, filtered through the method's
own handlers with a handle-keyed subtype test, joined with each callee's escaping set, iterated to
stability. Over CoreLib it converges in 23 rounds and about 3 seconds. **Tractability is not the
problem.** Here is what is.

### It needs almost no IL semantics

97.2% of CoreLib's 8,704 `throw` sites are preceded *immediately* by the `newobj` that constructed
the thrown object. That adjacency may only be *used* where control cannot have arrived from
anywhere else — a `throw` that is a branch target may be shared by two arms constructing different
exceptions — so the probe checks the offset against the method's branch targets and handler entries
first, and 248 sites end up untyped rather than 244. Only 228 sites (2.6%) need to know a callee's
return type, and 11 in the whole of CoreLib need real dataflow. There are 41 `rethrow` sites and 33
`calli`.

Catching is rarer still: only 947 IL methods (2.4%) have any exception region — 796 `finally`, 396
named `catch`, 24 `filter`, 3 `fault`.

The semantics that *is* needed is the classification of which opcodes can raise what by themselves.
That is a table, not an interpreter.

PawPrint already knew this before the table existed, and knew it in more than one shape:
`BaseClassTypes` (`Corelib.fs`) names 23 exception types; `raiseRuntimeException` was referenced 73
times across 16 files, covering 7 distinct types from the opcode files; and `div`/`rem` did not use
that route at all, reaching the guest through `executeFaultingArithmetic`'s
`Error corelib.DivideByZeroException`. So the knowledge existed, in two mechanisms and sixteen
files, with no single list to hand an analyser.

That list is now `WoofWare.PawPrint.Semantics`' `OpcodeFaults`, which the interpreter and the probe
both consume — the probe's own copy is gone, which is the point: an analyser and the runtime
reading one table is the thing that stops them drifting.

### The walls it hits, and how their sizes flip by assembly

The probe widens to `Unknown` at eight named walls and counts each by site. The counts are not
stable across assemblies, and the difference is the most useful thing the probe measured:

| wall | CoreLib | System.Text.Json | what would remove it |
| --- | ---: | ---: | --- |
| generic instantiation (`MethodSpec`, or a MemberRef with a `TypeSpec` parent rooted locally) | 34,720 | 1,379 | resolving an instantiation to its definition |
| `callvirt` | 17,021 | 3,282 | devirtualisation, or a join over every override |
| callee in another assembly | **0** | 6,805 | cross-assembly member resolution |
| abstract method | 1,116 | 86 | the same dispatch machinery: join the overrides |
| native body (`InternalCall`/`PInvoke`/`RuntimeProvided`) | 979 | 6 | a declared summary per native target |
| untyped `throw` | 248 | 54 | a small stack-dataflow pass |
| `rethrow` | 41 | 16 | track the handler's clause type into its body |
| `calli` | 33 | 0 | function-pointer targets, or accept `Unknown` |

`ldvirtftn` is deliberately *not* a wall despite selecting a method: it pushes a pointer and invokes
nothing, so whatever uncertainty that pointer carries belongs to the `calli` that eventually uses
it. `jmp` is not one either — unlike `calli` it names its target by token, so it is followed like
any other call.

CoreLib references almost nothing outside itself, so **every one of its 19,588 MemberRef call sites
has a `TypeSpec` parent rooted in CoreLib** — an instantiation of a local type, not a foreign call.
Measuring "the cross-assembly wall" on CoreLib measures zero. An ordinary library inverts it:
System.Text.Json's largest wall is foreign callees at 41.8% of its call sites, with generics a
distant third.

The upshot is that generics and dispatch are the universal walls, cross-assembly resolution is the
wall for everything that is not the framework's root, and both must be dealt with.

Result: **74.3% of CoreLib methods and 82.4% of System.Text.Json's come back `Unknown`.** Of the
rest of CoreLib, 20.9% get an exact non-empty set and only 4.8% are proved to throw nothing — that
last figure being small precisely because the `.cctor` rule above puts
`TypeInitializationException` on almost every call. This is accepted as a starting point rather
than a problem: the walls are known, countable, and each has a named remedy.

### Can the `TypeInitializationException` load be discharged statically?

It is now the largest single entry in the table — 121,520 CoreLib sites — so it is the obvious
thing to ask about. The answer is yes, mostly, and the reason it is only *mostly* turns out not to
be about `.cctor`s at all.

**Only 275 of CoreLib's 2,759 types (10.0%) have a `.cctor`.** A call to a type without one cannot
raise `TypeInitializationException`, exactly and by metadata alone, with no analysis and no
approximation. That prunes **39.1% of CoreLib's 117,568 invoking sites**.

Of the rest, over CoreLib:

| | share of sites |
| --- | ---: |
| target type has no `.cctor` — pruned exactly | 39.1% |
| target not resolvable from here — the existing wall | 26.8% |
| target's `.cctor` may throw | 18.7% |
| target decided by dispatch, not by the token — not pruned | 14.5% |
| target's `.cctor` provably cannot throw — pruned | 1.0% |

The fourth row is a `callvirt`, whose token names where dispatch *starts*. Pruning on the statically
named type would be wrong in exactly the case that matters: an interface method has no `.cctor` at
all, so it would prune to "no initializer" even when the implementation is a value type whose
initializer can fail.

The third row is not irreducible either. Of the 275 `.cctor`s, **158 are `Unknown`** — blocked by
the same generic-instantiation, devirtualisation and cross-assembly walls as everything else, not by
anything specific to initialisers. So the ceiling on this prune is set by the four walls already
identified, and rises with them.

Two things were measured on the way that are worth recording.

**A `.cctor` inflates its own answer, and so does an absent one.** Two refinements, both sound,
both needed before the measurement means anything:

* A `.cctor` that writes its own type's statics picks up `TypeInitialization` from `stsfld`, which
  then propagates to every call site of its type — a cycle the analysis inflicts on itself, since
  the initializer it would supposedly trigger is the one already running. The CLI lets the
  initializing thread straight through (ECMA-335 I.8.9.5).
* The no-`.cctor` prune has to participate in the *fixpoint*, not be applied to its output
  afterwards. Applied afterwards, a `.cctor` that only calls harmless methods on types with no
  initializer still carries a synthetic `TypeInitializationException` and is counted as throwing.

Their combined effect is most of the story. Before either, *not one* of the 275 `.cctor`s was
provably harmless, and all 117 "throwing" ones carried `TypeInitialization` themselves. With both,
71 of the 110 that can throw carry **only** `OutOfMemoryException` — the self-propagation is gone,
and what is left is allocation.

**`beforefieldinit` is not available as a prune here**, though it looks like the obvious one.
ECMA-335 I.8.9.5 does not list method invocation as a trigger for such a type — only static field
access — and 270 of the 275 are marked. But PawPrint deliberately runs `.cctor`s eagerly
regardless of the flag (II.10.5.3.2 permits eager schedules; see `docs/divergences.md`), so an
analyser taking that prune would disagree with the interpreter that is meant to validate it. The
prune is available in principle to an analyser targeting CoreCLR's schedule; it is not available to
one whose oracle is PawPrint.

**What is left is mostly one thing.** Of the 110 `.cctor`s that can throw a named type, the
commonest shape by far is `OutOfMemoryException` alone — a `.cctor` that allocates. A sample of
what remains, from the run before resource exhaustion is filtered:

```text
System.Collections.Generic.List`1     OutOfMemory, Overflow, TypeInitialization
System.Convert                        TypeInitialization
System.DateTimeOffset                 NullReference, OutOfMemory, TypeInitialization
System.Decimal+DecCalc                ArrayTypeMismatch, IndexOutOfRange, NullReference, OutOfMemory, Overflow, TypeInitialization
System.Diagnostics.Stopwatch          DivideByZero, Overflow
```

`Stopwatch` is the one worth looking at: its initializer divides by the timer frequency, so
`DivideByZeroException` is a real fault a value domain could discharge and a syntactic analysis
cannot. `Convert` carries nothing but `TypeInitialization`, meaning it only calls into other types
— it is pure propagation.

**And filtering resource exhaustion is what makes this question answerable.** `OutOfMemoryException`
and `StackOverflowException` are `FaultKind.ResourceExhaustion`; `OpcodeFaults.excludingKind` drops
them from a report, deliberately unsoundly and saying so. Measured over CoreLib, hiding them takes
the initializers that are **provably harmless from 7 to 78** and those that can throw a named type
from 110 to 39. That is the difference between "a fifth of call sites carry an initializer risk you
cannot discharge" and "most of them do not".

It barely moves the *aggregate* method numbers — 15.1% to 15.4% provably-throws-nothing — because
those are dominated by the four walls, and a method that carries `OutOfMemoryException` usually
carries `Unknown` too. The filter's value is in the individual answer, not the summary statistic,
and it is worth being clear about which.

### It needs a value domain to be *useful*, as opposed to *sound*

A sound answer must include what the opcodes themselves can raise. In CoreLib that is 121,520 sites
that can raise `TypeInitializationException` — every instruction that invokes can trigger a
`.cctor` (ECMA-335 I.8.9.5), and the `.cctor` is not the callee the call edge names — 77,337 that
can raise `NullReferenceException`, 17,827 `OutOfMemoryException`, and smaller counts down to 343
`StackOverflowException` from `localloc`. **95.0%** of CoreLib's IL methods contain at least one,
and 97.1% of System.Text.Json's. So a sound analysis with no value domain reports "may throw
`NullReferenceException` or `TypeInitializationException`" for essentially the whole BCL, which is
true and worthless.

This is where sharing real semantics with the concrete interpreter genuinely bites: a nullness
domain is the first abstract domain, and every abstract domain wants a transfer function per
opcode, which must not drift from what the interpreter does.

It is not on the critical path, though, because three of the four target questions tolerate the
imprecision:

* **"what does this method throw?"** — needs the value domain, or needs to report structural
  exceptions in a separate bucket.
* **"did this change newly introduce `X`?"** — does not. Both sides of the diff are equally
  imprecise and it cancels. One honesty rule: if either side is `Unknown`, the comparison is
  inconclusive and must say so, never "no new exceptions".
* **"what files does this ever attempt to read?"** — does not. It is a reachability question over
  the same call graph, hitting the same walls, and falls out of the same fixpoint.
* **"is this method pure?"** — does not, for the *negative* answer. Proving impurity needs only
  reachability to a writing operation. Proving purity needs the walls down, because `Unknown` and
  "impure" are the same answer for a soundness-preserving analysis.

### The oracle

`docs/probes/exception-escape/Fixture/Cases.cs` is thirteen cases whose expected escaping set is
written in `Driver.fs` from first principles — what the instrument's stated envelope *should*
produce — and the driver exits non-zero on any mismatch. All thirteen hold: exact catch, unrelated
catch, `finally`-does-not-catch, base-class catch across an assembly boundary (which correctly
fails to absorb, the chain being unreadable), base-class catch within one assembly (which
correctly absorbs), two sources joined, an outer `catch (Exception)` absorbing even `Unknown`, a
leaf that cannot raise, recursion, and a `rethrow` in a handler outside its own protected region.

Checked to be falsifiable rather than assumed: making the universal-clause test return `false`
kills exactly one expectation, `CatchesBoth`, and no other.

## The one thing that does not exist at all

The suspicion about the native registry was that it is executable but not queryable. That is true —
`NativeDispatch.tryExecute` is `List.tryPick` over 23 handler functions, each a large tuple match —
but it understates the problem, and the understatement changes the plan.

**PawPrint's native handlers do not contain the answer to "what can this throw".** Across the whole
`Native/` tree there are **12** sites that raise a guest-visible managed exception
(`NativeHandlerResult.raiseException` / `raiseExceptionWithMessage`) and **952** `failwith` sites.
A handler models the reachable happy path and aborts the host process for everything else. So where
a real QCall raises `ArgumentException`, PawPrint frequently `failwith`s instead — deliberately, by
the repository's "prefer crashing over documented divergence" rule.

This has a sharp consequence. GPT-5.6's sketch argues that a native call's *body* should be the
source of its exception set, because a parallel annotation will drift from the implementation. That
assumes the body knows. It does not: extracting the exception set from a PawPrint handler by any
means — reification, symbolic execution of the F#, a human reading it — would produce PawPrint's
refusal set, not CoreCLR's exception set. The information has to be written down against upstream,
the way every other divergence in this repository is.

What stops it drifting is not that it is derived, but that it is *checked*: with only 12 raise
sites, routing them through a helper that asserts the raised type is in the entry's declared set
turns drift into a test failure.

### The vocabulary for this already exists, and is being built right now

`docs/plans/2026-08-23-posix-kernel-extraction.md` stage 7 introduced exactly the shape a queryable
native entry wants — `Syscall`, `SyscallAnswer` (`Completed` / `Failed of UnixError`), and a
per-syscall `…Refusal` DU naming what the library declines to model — and stage 8 is extending it
to buffer-carrying syscalls. `WoofWare.PosixKernel/UnixSystem.fs` has it working today for
`geteuid`, `dup`, `lseek`, `flock`, `ftruncate`, `close`, `read` and `write`.

`Native/NativeQCall.fs` is halfway there independently: a `Map<string, handler>` of 114 QCall entry
points keyed by name. Giving its values a record with a declared summary beside the handler is an
additive change.

The surface that would need entries has never been counted, because there is nothing to count it
with — which is itself an argument for a registry. Three lower bounds, measured by different greps
and not disjoint: 114 entries in the `NativeQCall` map, 75 distinct `SystemNative_*` entry-point
strings, and 147 tuple match arms elsewhere in `Native/`. CoreLib itself declares 979 methods that
would need one (`InternalCall` + `PInvoke` + `RuntimeProvided`), which is the demand side.

## Options for the architecture

Three genuinely different shapes.

### A. Reify CLR semantics into a first-order command language

GPT-5.6's recommendation: opcode and native semantics become inert `MachineOperation` graphs;
concrete and abstract interpreters both consume them; a project boundary forbids semantics code
from seeing `IlMachineState`.

The target shape is a good one. As a *plan* it has a specific problem this repository has already
written down: it produces a long-lived half-migrated system, and `gospel.md`'s migration section
says that is a tax on every future change. The migration is 79,900 lines of state-coupled
interpreter plus a few hundred native entry points, during which PawPrint's concrete fidelity — the
thing that makes it worth analysing with — is under continuous risk. And it would not, by itself,
produce the exception answer for a single native call, because the bodies do not know it.

### B. Analysis alongside, sharing exactly the three things that pay

Build `WoofWare.PawPrint.Analysis` on Domain plus a lifted program-universe layer, and share with
the concrete interpreter precisely three artefacts, each a data classification rather than
behaviour:

1. **The opcode fault table.** Which exceptions each opcode can raise by itself.
2. **A queryable native registry.** Identity, signature, handler, and a declared summary with
   `Unknown` as an explicit case.
3. **Resolution and dispatch answerable without a machine state.**

### C. Make the existing interpreter generic in its value domain

`IlMachineState<'value>`, run abstractly. Rejected for a measured reason: `CliType.fs` alone is
4,433 lines of concrete value representation, the state is mentioned 3,200 times, and the eval
stack, heap, byref surface and pointer model are all concrete-value-shaped. Largest possible diff,
least preserved information, and the abstract run would still have nothing to say about a native
call.

### The choice does not have to be made yet

B is my recommendation, but the decision is **deferrable**, because B's first two increments are
strict prerequisites of A as well:

* A needs the opcode fault table: under A, "which exceptions can this opcode raise" is part of the
  command language's semantics, and it has to be extracted from the same sixteen files either way.
* A needs resolution and dispatch answerable without a machine state: that is A's own stated
  constraint, that semantics code must not see `IlMachineState`.

So increments 1 and 3 are common ground, and the A-versus-B question only becomes live when the
native registry is reached — which, per the ordering below, is after the POSIX extraction finishes
anyway. Both are also worth doing on their own merits: a single fault table removes a scattered
duplication across two mechanisms, and a program-universe record makes the loader cache stop
masquerading as guest state.

## Ordering against the POSIX-kernel extraction

The extraction is at stage 8e of 8a–8h plus stage 9; the current branch is
`posix-kernel/stage-8e-read-socket-order`. Stage 9 then does the blocking syscalls and packaging.

The overlap is real but narrow, and entirely in the native-registry increment:

* **The native registry must wait.** It targets the same `Native/` tree the extraction is actively
  rewriting, and wants to extend the very vocabulary stage 8 is still settling (`SyscallAnswer`
  gains a `writes` component when the first buffer-carrying syscall lands; stage 9 replaces a
  stage-7 refusal with `WouldBlock of WakeCondition`).
* **The fault table and the program-universe rehoming do not.** They touch `Domain`, the opcode
  files, `VirtualSlotLayout.fs`, `IlMachineTypeResolution.fs` and their call sites. One caveat to
  check before committing: some `Native/` files are among the 30 guest-state-free ones and would
  move in the rehoming.
* **The analyser skeleton does not either.** It is a new project over Domain, as the probe already
  is.

## Plan

In order, each its own branch:

1. **Hoist the opcode fault table** out of the sixteen files that currently hold the knowledge, and
   make the interpreter's raise sites consume it. **Done**, as `WoofWare.PawPrint.Semantics`'
   `OpcodeFaults`. The anti-drift mechanism turned out not to be "derive it from the bodies" but
   "check it against them": `raiseOpcodeFault` reads the instruction from the raising thread's own
   frame and fails if the fault is not in that instruction's entry. That check found a real table
   error on its first run against the guest corpus — `calli` through a null function pointer —
   which is the evidence that it is load-bearing rather than decorative.
   `executeFaultingArithmetic`'s private `ArithmeticFaults` DU, a local restatement of the same
   table, is gone.
2. **Name the program universe.** A record holding `LoadedAssemblies`, `AllConcreteTypes` and the
   handle registries; `IlMachineState` holds one; `VirtualSlotLayout` and the resolution cluster
   take it instead of the whole state. Oracle: rename-only discipline plus the full suite. This is
   a refactor with no intended behaviour change, so any diff in guest behaviour is a bug.
3. **`WoofWare.PawPrint.Analysis`**, with the probe's fixpoint as its first real content, now able
   to resolve across assemblies, instantiate generics and devirtualise. Oracle: the `Cases.cs`
   fixture, extended, plus the inclusion property — every exception a concrete PawPrint run
   actually dispatches out of a method must be in that method's computed set. That last is the
   thing PawPrint uniquely offers, and it should be a property test over the existing guest corpus
   rather than a hand-written list.

Only then, and only after stage 9: the native registry.

## Settled

* **The target is a general analysis engine**, with escaping exceptions first. Purity and
  capability questions ("what files does this package attempt to read") are targets too; they share
  the call graph and the walls, so they are cheap once the walls come down.
* **`Unknown` at 74% is acceptable** as a starting point. The `failwith`s behind the native wall
  have to be addressed eventually regardless.
* **The A-versus-B architectural choice is deferred**, on the grounds above that the first two
  increments belong to both.
* **The `calli`-through-null divergence is reclassified.** Its entry justified PawPrint's
  catchable `NullReferenceException` by citing ECMA-335 III.3.20 as listing that exception. It does
  not: III.3.20's "Exceptions" clause lists `System.SecurityException` alone, where III.4.2 for
  `callvirt` says in as many words that "System.NullReferenceException is thrown if obj is null"
  (6th edition, June 2012). III.3.20's "Correctness" clause requires the pointer to hold a method
  address, so a null one is not correct CIL and the CLI does not say what happens to it. The
  behaviour is unchanged and still the right one; the status is now "unspecified, so both are
  permitted" rather than "strictly closer to the specification than CoreCLR", and the reason given
  is the one that actually holds.
* **Resource exhaustion is tagged, not dropped.** `OutOfMemoryException` and
  `StackOverflowException` are `FaultKind.ResourceExhaustion` and stay in the model, because they
  are genuinely possible and a model that hid them would be lying. What filters is the *report*:
  `OpcodeFaults.excludingKind` drops a kind, is documented as unsound in its own docstring, and
  returns `Unmodelled` unchanged — an unclassified instruction might raise a fault of any kind,
  including one the caller did not ask to drop, so there is nothing there that filtering could
  honestly remove. The interpreter's own check never uses it.

  One consequence to keep in view: `TypeInitializationException` is classified `Logic`, because
  whether it can arise is decided by the initializer's code. So an initializer that can fail *only*
  because it allocates still contributes one, and filtering resource exhaustion does not remove
  every fault whose ultimate cause is resource exhaustion. Whether that wants a third kind, or
  tracking of what causes a `TypeInitializationException`, is a question for when someone is
  reading real reports.

## Where the fault table lives

`WoofWare.PawPrint.Domain`'s package description says it holds "IL opcodes, the emulated type
system, and metadata-handle wrappers" — in effect, *what you need to read a DLL*. Which exceptions
an opcode can raise is not that: it is a fact about the CLI execution model, from ECMA-335 §III's
per-opcode "Exceptions" clauses, and it has no bearing on reading an image.

Three facts bear on the choice.

* **Domain already holds a classifier of exactly this shape.** `ContextSwitchPrior.fs` (457 lines)
  is an exhaustive per-opcode classifier — `ofNullary` / `ofUnaryConst` / `ofUnaryMetadata` /
  `ofUnaryString` / `ofIlOp`, a pure function of `IlOp` — whose own docstring says the bands are
  "coarse enough to assign by hand from CIL semantics". It reasons about the very facts the fault
  table would: it cites "`Add_ovf` and `Div` (trap only on specific inputs)" as its
  `RarelyGuestVisible` examples. So the "read a DLL" principle is right, and Domain already departs
  from it in one place, in the one place that matters here.
* **A new project referenced by `WoofWare.PawPrint` must be a published package.** A
  `ProjectReference` to an `IsPackable=false` project produces a nupkg naming a dependency that
  does not exist, with the assembly missing from `lib/`, and no warning from build or CI. So this
  is a fourth nupkg with its own `PackageId` and `version.json`, following the `Domain` and
  `PosixKernel` precedents.
* **Moving `ContextSwitchPrior` is nearly free inside the repo, and breaking outside it.** It
  declares `namespace WoofWare.PawPrint`, the same namespace a new project would use, so its five
  in-repo consumer files and 47-reference test file need no edit at all. The break is purely at the
  package boundary: a public type would leave a published package. The repo already has the tooling
  for a move commit — `scripts/check-move-is-rename-only.sh` and
  `scripts/check-docstring-attachment.py`.

The options:

| | where | cost | end state |
| --- | --- | --- | --- |
| **(a)** | fault table into Domain, beside `ContextSwitchPrior` | one file; widen Domain's stated scope by a clause | Domain drifts further from "read a DLL", and the native registry still cannot live there, so `Semantics` gets created later anyway — with more in it to move |
| **(b)** | new `WoofWare.PawPrint.Semantics`, fault table only | a fourth package | two per-opcode classifiers in two packages; the third one gets added to whichever the author happened to look at |
| **(c)** | new `WoofWare.PawPrint.Semantics`, and move `ContextSwitchPrior` into it | a fourth package, plus a public type leaving a published package | one home for CLI execution rules; the project graph enforces "semantics cannot see `IlMachineState`", which today only F# compile order does, and weakly |

**Decided: (c), and done.** The principle that Domain is for reading images is worth enforcing
rather than eroding, and (b) creates precisely the two-versions-of-the-truth split this
repository's own migration guidance warns against. (c) also gives the native registry a home it
will need, and upgrades the "semantics may not see the machine state" constraint from a
compile-order convention inside one project to a fact the project graph checks. Both outward-facing
costs were accepted: a fourth `PackageId`, and `ContextSwitchPrior` leaving the published
`WoofWare.PawPrint.Domain` surface.

Not moved: `AccessCheck.fs` or `VtableSlot.fs`. Both are rules for *interpreting metadata* —
visibility, slot layout — which is closer to reading an image than to running one.
`ContextSwitchPrior` was the clear outlier, being about execution and nothing else.

## Still open

* **How much does a `RuntimeProvided` body need modelling?** CoreLib has 284: 276 delegate
  `.ctor`/`Invoke`/`BeginInvoke`/`EndInvoke`, and 8 `[UnsafeAccessor]`s. Delegate `Invoke` in
  particular is a dispatch edge the call graph cannot see at all, and it is not obvious whether the
  answer is "summarise it" or "make delegate targets a first-class edge kind".
* **Does the fault table want to be an effect table from the start?** The general engine will
  eventually want "which opcodes write memory", "which allocate", "which can block". Adding
  dimensions later churns every caller; adding them now is speculative generality. My inclination
  is to ship faults alone but shape the lookup so a second dimension is an added field rather than
  a changed signature.
