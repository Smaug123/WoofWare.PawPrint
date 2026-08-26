# Can PawPrint tell us what a method can throw?

An investigation, not yet a commitment to build anything. Everything numeric below was measured
in this branch; `docs/probes/exception-escape/` is the instrument, and its two `measured-*.txt`
files are the outputs quoted here. The runtime measured is the pinned managed linux-x64 pack at
10.0.7 (`$DOTNET_LINUX_FRAMEWORK_DIR`).

## The question

Could PawPrint grow an analyser that, given a method, reports the exceptions that can escape it —
soundly enough that "this change newly makes `OperationCanceledException` possible" is a claim we
would stand behind?

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

Domain turns out to carry everything the *front half* of an analyser needs, and the probe proves it
by using nothing else:

* decoded IL, offset-indexed (`MethodInstructions`);
* exception regions with their clause kinds and catch types (`ExceptionRegion`);
* the type graph, including base types (`TypeInfo.BaseType`);
* and — this one matters more than it looks — `MethodBody` already distinguishes `Il`,
  `InternalCall`, `PInvoke`, `RuntimeProvided` and `Abstract`. That is exactly the "which methods
  have no IL and therefore need a declared summary?" classification an analyser has to make, and it
  is already a total DU rather than something to be rediscovered.

Reading CoreLib takes 974ms and yields 41,561 methods, 95.0% of them with IL bodies.

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
to be designed and migrated to. It is something that already exists, scattered, and needs a name
and a record to live in. That is a mechanical refactor whose blast radius is a compile error at
every call site, which the compiler will walk us through.

(That census is a lower bound on purity, not a proof: a file could reach guest state through a
helper rather than by naming a field. Each file must be checked before it is lifted.)

## What an escaping-exception answer actually needs

The probe implements a real interprocedural fixpoint — local raises, filtered through the method's
own handlers with a proper subtype test, joined with each callee's escaping set, iterated to
stability. Over CoreLib it converges in 23 rounds and 1.5 seconds. **Tractability is not the
problem.** Here is what is.

### It needs almost no IL semantics

97.2% of CoreLib's 8,704 `throw` sites are preceded *immediately* by the `newobj` that constructed
the thrown object, so the thrown type is available by looking one instruction back. Only 228 sites
(2.6%) need to know a callee's return type, and 11 sites in the whole of CoreLib need real
dataflow. There are 41 `rethrow` sites.

Catching is rarer still: only 947 IL methods (2.4%) have any exception region at all — 796
`finally`, 396 named `catch`, 24 `filter`, 3 `fault`.

The semantics that *is* needed is the classification of which opcodes can raise what by themselves.
That is a table, not an interpreter. PawPrint already knows it, and knows it in more than one shape:
`BaseClassTypes` (`Corelib.fs`) names 23 exception types; `raiseRuntimeException` is referenced 73
times across 16 files, covering 7 distinct types from the opcode files; and `div`/`rem` do not use
that route at all, reaching the guest through `executeFaultingArithmetic`'s
`Error corelib.DivideByZeroException` instead. So the knowledge exists, in at least two mechanisms
and sixteen files, and there is no single list to hand an analyser.

### It needs four things it does not have, and they are countable

The probe reports `Unknown` — its top element — at four named walls, and counts each. Over CoreLib:

| wall | sites | what would fix it |
| --- | --- | --- |
| callee in another assembly (`MemberRef`) | 19,588 (16.7% of call sites) | cross-assembly member resolution |
| generic instantiation (`MethodSpec`) | 15,132 (12.9%) | resolving a `MethodSpec` to its target, with the instantiation |
| `callvirt` | 13,760 | devirtualisation, or a class-hierarchy join over every override |
| method with no IL body | 2,095 in CoreLib alone | a declared summary per native/runtime method |
| untyped `throw` | 244 | a small stack-dataflow pass |
| `rethrow` | 41 | tracking the handler's caught type into the handler body |

The result: **74.3% of CoreLib methods come back `Unknown`.** 15.3% are proved to throw nothing;
10.4% get an exact non-empty set. A syntactic analysis is not a weaker version of the thing we
want; it is not the thing at all.

Three of those four walls are PawPrint's day job, and it does them well — but it does them in
`IlMachineMemberResolution.fs` and `VirtualSlotLayout.fs`, which is why the "lift the universe"
refactor above is the critical path rather than a tidy-up.

### It needs a value domain to be *useful*, as opposed to *sound*

A sound answer must include what the opcodes themselves can raise. In CoreLib that is 63,196 sites
that can raise `NullReferenceException`, 17,827 that can raise `OutOfMemoryException`, and 3,952
that can raise `TypeInitializationException`; 58.5% of IL methods contain at least one. So a sound
analysis with no value domain reports "may throw `NullReferenceException`" for most of the BCL,
which is true and worthless.

This is where the argument for sharing real semantics with the concrete interpreter genuinely
bites — a nullness domain is the first abstract domain, and every abstract domain wants a transfer
function per opcode, which is exactly the thing that must not drift from what the interpreter does.
It is also *not on the critical path*, because two of the three headline uses tolerate the
imprecision:

* **"what does this method throw?"** — needs the value domain, or needs to report structural
  exceptions in a separate bucket the reader can ignore.
* **"did this change newly introduce `X`?"** — does not. Both sides of the diff are equally
  imprecise, and the imprecision cancels. (With one honesty rule: if either side is `Unknown`, the
  comparison is inconclusive and must say so, never "no new exceptions".)
* **"can this method reach the filesystem / network / `Environment.Exit`?"** — does not. That is a
  reachability question over the same call graph, with the same four walls, and it falls out of the
  same fixpoint for nearly free.

### It has a working correctness oracle

`docs/probes/exception-escape/Fixture/Cases.cs` names the expected answer in each method's name.
The probe gets exact catch, unrelated catch, `finally`-does-not-catch, two sources joined, an
outer catch absorbing both, a leaf that cannot raise, and recursion — all correct. It reports
`CaughtByBase` wrongly (says `InvalidOperationException` escapes a `catch (SystemException)`)
because its subtype relation is built from one assembly's TypeDefs, and it reports the
same-assembly `CaughtByLocalBase` correctly. That contrast is the cross-assembly wall being
visible in a single test, which is the useful thing about having built the fixture.

## The one thing that does not exist at all

The suspicion about the native registry was that it is executable but not queryable. That is true —
`NativeDispatch.tryExecute` is `List.tryPick` over 23 handler functions, each a large tuple match —
but it understates the problem, and the understatement changes the plan.

**PawPrint's native handlers do not contain the answer to "what can this throw".** Across the whole
`Native/` tree there are **12** sites that raise a guest-visible managed exception
(`NativeHandlerResult.raiseException` / `raiseExceptionWithMessage`) and **952** `failwith` sites.
A handler models the reachable happy path and aborts the host process for everything else. So where
a real QCall raises `ArgumentException`, PawPrint frequently `failwith`s instead — deliberately and
correctly, by the repository's "prefer crashing over documented divergence" rule.

This has a sharp consequence for the design. GPT-5.6's sketch argues that a native call's *body*
should be the source of its exception set, because a parallel annotation will drift from the
implementation. That reasoning assumes the body knows. Measured, it does not: extracting the
exception set from a PawPrint handler by any means — reification, symbolic execution of the F#, a
human reading it — would produce PawPrint's refusal set, not CoreCLR's exception set. The
information has to be *written down against upstream*, the way every other divergence in this
repository is.

What stops it drifting is not that it is derived, but that it is *checked*: with only 12 raise
sites, routing them through a helper that asserts the raised type is in the entry's declared set
turns drift into a test failure. That is a small, self-contained piece of work.

### The vocabulary for this already exists, and is being built right now

`docs/plans/2026-08-23-posix-kernel-extraction.md` stage 7 introduced exactly the shape a queryable
native entry wants — `Syscall`, `SyscallAnswer` (`Completed` / `Failed of UnixError`), and a
per-syscall `…Refusal` DU naming what the library declines to model — and stage 8 is extending it
to buffer-carrying syscalls. `WoofWare.PosixKernel/UnixSystem.fs` has it working today for
`geteuid`, `dup`, `lseek`, `flock`, `ftruncate`, `close`, `read` and `write`.

`Native/NativeQCall.fs` is halfway there independently: a `Map<string, handler>` of 114 QCall entry
points, keyed by name. It is a registry that happens to store only behaviour; giving its values a
record with a declared summary beside the handler is an additive change.

So the "declarative native registry" is not a new idea to be introduced against the grain. It is
the idea the repository has already committed to, needing to be applied to a second dimension
(exceptions) and a wider surface. That surface has never been counted, because there is nothing to
count it with — which is itself the argument for a registry. Three lower bounds, measured by
different greps and not disjoint: 114 entries in the `NativeQCall` map, 75 distinct
`SystemNative_*` entry-point strings, and 147 tuple match arms elsewhere in `Native/` that end in a
`MethodReturnType` pattern. Call it "a few hundred", and note that the exact number becoming
knowable is one of the things the registry buys.

## Options for the architecture

Three genuinely different shapes, not variants of one.

### A. Reify CLR semantics into a first-order command language

GPT-5.6's recommendation: opcode and native semantics become inert `MachineOperation` graphs;
concrete and abstract interpreters both consume them; a project boundary forbids semantics code
from seeing `IlMachineState`.

The target shape is right and I would be happy to end up there. As a *plan*, it has a specific
problem this repository has already written down: it produces a long-lived half-migrated system,
and `gospel.md`'s migration section says a half-finished migration is a tax on every future change.
The migration is 79,900 lines of state-coupled interpreter and ~350 native entry points, during
which PawPrint's hard-won concrete fidelity — the thing that makes it worth analysing with — is
under continuous risk. And the measurement above says the reification would not, by itself, produce
the exception answer for a single native call, because the bodies do not know it.

### B. Analysis alongside, sharing exactly the three things that pay

Build `WoofWare.PawPrint.Analysis` on Domain plus a lifted program-universe layer, and share with
the concrete interpreter precisely three artefacts — each of which is a data classification, not
behaviour, so each is inspectable without any reification:

1. **The opcode raise table.** Which exceptions each opcode can raise by itself. Lives in Domain;
   the interpreter's 73 raise sites *consume* it rather than restating it, so a wrong entry becomes
   a guest bug that the existing differential suite catches.
2. **A queryable native registry.** Identity, signature, handler, and a declared summary with
   `Unknown` as an explicit case. Extends the `Syscall`/`SyscallAnswer` vocabulary stage 7 already
   established; enforced by requiring exactly one entry per native target.
3. **Resolution and dispatch answerable without a machine state.** The `VirtualSlotLayout` /
   `IlMachineTypeResolution` / `IlMachineMemberResolution` cluster rehomed onto a program-universe
   record. Measured above as a rehoming, not a redesign.

Each step ships value on its own; none of them leaves the tree in a half-migrated state; and the
end state is reachable from here to A later, because (1) and (2) are literally A's opcode semantics
and A's native registry, restricted to the exception dimension.

### C. Make the existing interpreter generic in its value domain

`IlMachineState<'value>`, run abstractly. Rejected, and for a measured reason rather than a
stylistic one: `CliType.fs` alone is 4,433 lines of concrete value representation, the state is
mentioned 3,200 times, and the eval stack, heap, byref surface and pointer model are all
concrete-value-shaped. This is the largest possible diff for the least preserved information, and
the abstract run would still have nothing to say about a native call.

### Recommendation

**B**, with A kept as the stated destination for the semantics of *values* if and when a nullness
domain earns it. B's first two increments are worth doing even if the analyser is never built:
a single opcode-raise table removes a scattered 22-type duplication, and a registry with one entry
per native target makes "which natives do we implement?" answerable, which it currently is not.

## Ordering against the POSIX-kernel extraction

The extraction is at stage 8e of 8a–8h plus stage 9; the current branch is
`posix-kernel/stage-8e-read-socket-order`. Stage 9 then does the blocking syscalls and packaging.
That is several PRs yet.

The overlap is real but narrow, and it is entirely in increment B2:

* **B2 must wait.** It targets the same `Native/` tree the extraction is actively rewriting, and it
  wants to extend the very vocabulary stage 8 is still settling (`SyscallAnswer` gains a `writes`
  component when the first buffer-carrying syscall lands; stage 9 replaces a stage-7 refusal with
  `WouldBlock of WakeCondition`). Starting B2 now means designing against a moving surface and
  conflicting with every extraction PR.
* **B1 and B3 do not.** The opcode raise table touches `Domain` and the 16 opcode files; the
  program-universe rehoming touches `VirtualSlotLayout.fs`, `IlMachineTypeResolution.fs` and their
  call sites. Neither is in `Native/` — with one caveat worth checking before committing to it,
  that some `Native/` files are among the 30 guest-state-free ones and would move in B3.
* **Building the analyser skeleton does not either.** It is a new project over Domain, as the probe
  already is.

So: this workstream can start, on B1 and B3, without waiting — and B2 should be scheduled after
stage 9, where it inherits a settled vocabulary instead of fighting one.

## What I would do first

In order, each its own branch:

1. **Hoist the opcode raise table into Domain**, and make the interpreter's raise sites consume it.
   Oracle: the existing guest suite, unchanged, plus a test that every entry is reachable. Care
   needed — hoisting a classifier makes previously-unreachable arms reachable, which has bitten this
   repository before.
2. **Name the program universe.** A record holding `LoadedAssemblies`, `AllConcreteTypes` and the
   handle registries; `IlMachineState` holds one; `VirtualSlotLayout` and the resolution cluster
   take it instead of the whole state. Oracle: rename-only discipline plus the full suite; this is
   a refactor with no intended behaviour change, so any diff in guest behaviour is a bug.
3. **`WoofWare.PawPrint.Analysis`**, with the probe's fixpoint as its first real content, now able
   to resolve across assemblies and devirtualise. Oracle: the `Cases.cs` fixture, extended, and the
   inclusion property — every exception a concrete PawPrint run actually dispatches out of a method
   must be in that method's computed set. That last one is the thing PawPrint uniquely offers, and
   it should be a property test over the existing guest corpus rather than a hand-written list.

Only then, and only after stage 9: the native registry.

## Questions I could not settle from the code

* **Which use case is the target?** "What can this method throw" and "did this diff newly introduce
  `X`" want different things: the first needs a value domain to be useful, the second does not.
  Committing to the second first is much cheaper and I would recommend it, but it is a product
  decision.
* **How much does a `RuntimeProvided` body need to be modelled?** CoreLib has 284 of them: 276
  delegate `.ctor`/`Invoke`/`BeginInvoke`/`EndInvoke`, and 8 `[UnsafeAccessor]`s. Delegate
  `Invoke` in particular is a dispatch edge the call graph cannot see at all, and it is not obvious
  whether the answer is "summarise it" or "make delegate targets a first-class edge kind".
* **Is `Unknown` acceptable at 74%?** It will fall a long way once the four walls come down, but I
  have not measured how far, and I would not want to promise a number before measuring it.
