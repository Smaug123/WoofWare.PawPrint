# `GCFrameRegistration::RegisterForGCReporting` / `UnregisterForGCReporting`

Status: implemented. The plan below is as written before implementation, except for this
section, which records what review and implementation changed.

## What review and implementation found that the plan did not predict

**The equivalence claim as first written was falsifiable, and is now scoped.** Reflection can
forge a pointer: `RuntimeType.CanValueSpecialCast` accepts a bare `IntPtr` for a pointer-typed
parameter, and reflection ignores `internal`, so a guest can call this InternalCall with
`IntPtr.Zero`. CoreCLR has no defined behaviour there — the `_ASSERTE` compiles out of a release
build and `Push` writes through the pointer — so the plan's "a guest cannot construct a
distinguishing experiment" was too strong, and its "CoreCLR's only failure mode is an `_ASSERTE`"
was wrong about release builds. The handler now refuses a null pointer loudly, and the doc
comment scopes the equivalence to registrations the managed constructor built.

**That refusal is unreachable today, measured rather than assumed.** A guest doing exactly the
forged-pointer call stops one level earlier, in `RuntimeMethodHandle_InvokeMethod`: "parameter 0
is a pointer or function pointer, whose argument buffer entry addresses a boxed IntPtr payload
rather than an object slot" — the gap the parked `ReflectionInvokePointerSignature.cs` is filed
against. So the null arm is an arm no test can currently kill (mutation 4 below survives). It is
kept deliberately: without it PawPrint would *silently succeed* where CoreCLR faults, and
un-parking that reflection gap would make it reachable without anyone revisiting this file.

**The chain has three readers upstream, not one.** The plan said `GCFrame::GcScanRoots` was the
sole consumer. `popGCFrames` (`vm/exceptionhandling.cpp:510-517`) also walks it during unwind,
and `Thread::GetGCFrame` carries a `_DEBUG_IMPL` stack-bounds assert (`vm/threads.h:1191-1205`).
Neither changes the conclusion — one only unlinks, the other is debug-only — but the doc comment
now argues all three rather than asserting one.

**The test's discrimination is narrower than the plan claimed.** "An implementation that did
anything observable to the guest's struct would break it" is false: after `Register` returns, no
managed code reads *any* field of the registration again, so an implementation that scribbled
over the whole struct would still pass. What the test actually discriminates is that both names
match, the argument shape is accepted, and the frame completes.

**The guest exercises the dual-registration branch**, which is better coverage than either the
plan or the review supposed. `DetermineStrategy_ObjSpanArgs` leaves `_invokeFunc_ObjSpanArgs`
null on a first invocation (`MethodInvokerCommon.cs:116-121`), so `InvokeWithManyArgs` takes the
`RefArgs` branch: `stackalloc IntPtr[3 * argCount]`, two `Register` calls and two `Unregister`
calls in reverse order, not one of each.

**Placement changed from the plan.** The handler lives in a new
`Native/NativeGcFrameRegistration.fs` rather than as an arm of `NativeGc.tryExecute`. The
justification is the deliverable here, and the house style for a justification of this weight is
a `///` doc at top level — which `NativeGc.fs` has no room for, both its entry points already
carrying method-specific docs. A separate module also leaves that file's prose untouched.

**The no-op is only sound for a caller that does not read the registration back, and that had
to be enforced rather than assumed.** Codex found the hole: PawPrint honours
`[IgnoresAccessChecksTo]` (`WoofWare.PawPrint.Domain/FriendAssemblies.fs`), so a guest assembly
can `call` this InternalCall directly on a `GCFrameRegistration` of its own and then read the
second native word — which CoreCLR's `Push` has set to the thread pointer and PawPrint would
leave at zero. That falsifies the plan's "nothing managed can read the difference out of the
struct", which was true of CoreLib and not of guests. Three options were put to the user: refuse a
non-CoreLib caller, model the chain in guest memory (needs a `Thread*` representation PawPrint
does not have, so it would have to invent a bit pattern), or document the divergence. The user
chose to refuse, which is also what "prefer crashing over documented divergence" points at. The
handler now reads the calling frame's assembly and fails loudly outside CoreLib.

Note that Codex did not demonstrate this end-to-end — its own repro died on an unrelated `Ldloc`
gap in the IL its rewriter emitted — but the mechanism is supported by design rather than
accidental, so it was treated as real.

**The caller's *assembly* was too coarse a proxy.** On the next review round Codex pointed out
that once pointer-parameter reflection lands, a guest reaching this through
`MethodInfo.Invoke` presents `RuntimeMethodHandle.InvokeMethod` as the immediate caller frame —
CoreLib's, even though the registration and the code inspecting it afterwards are both the
guest's. The check is now against an enumerated `permittedCallers` list of
`(declaring type, method name)` pairs: every method in .NET 10's CoreLib that contains a call to
`RegisterForGCReporting`. That is also a more honest statement of the property being relied on,
which is "the registration cannot escape this frame", not "the caller is CoreLib". A future
CoreLib caller not on the list gets the same loud refusal, which is the right failure mode.

**The dependency resolved itself.** #955 merged as `e5d8b93` while this was being written, and
its squash-merged tree is byte-identical to the branch tip this work started from, so the branch
is based on plain `main` and the "stacks on" plan below no longer applies.

**Newly reachable surface, beyond `MethodBase.Invoke`.** Everything routing through the four
invokers' many-argument paths unblocks together: `Delegate.DynamicInvoke`,
`ConstructorInfo.Invoke` and `Activator.CreateInstance(type, args)` with more than four
arguments, plus any BCL-internal reflection taking those paths. They will now fail *further
along* if they fail, which changes triage for anything parked behind them.

## What is blocked today

`MethodBase.Invoke` on a target taking more than `MethodBaseInvoker.MaxStackAllocArgCount`
(4) arguments routes through `MethodBaseInvoker.InvokeWithManyArgs` rather than
`InvokeDirectByRefWithFewArgs`, and dies immediately:

```
Unimplemented native method (InternalCall): System.Private.CoreLib
  System.Runtime.GCFrameRegistration::RegisterForGCReporting(*(System.Runtime.GCFrameRegistration))
  -> void
Guest was: thread 0 (Runnable) in System.Private.CoreLib.GCFrameRegistration.RegisterForGCReporting
```

Measured, by un-parking `sourcesPure/ReflectionInvokeMethodManyArguments.cs` (which
`union-byref-reinterpret` added, parked, for exactly this) and running it on that branch.

`UnregisterForGCReporting` is the same primitive's other half and runs in the `finally` of
the same `try`, so it is unconditionally reached by every caller that reaches `Register`.
Implementing one without the other leaves the guest dying three lines later; they land
together.

## What CoreCLR does

`GCFrameRegistration` (`System/Runtime/GCFrameRegistration.cs`) is a `[StructLayout(Sequential)]`
struct whose fields are laid out to be *punned* as the VM's `GCFrame` (`vm/frames.h:1865`):

| managed field | `GCFrame` member |
| --- | --- |
| `nuint _reserved1` | `PTR_GCFrame m_Next` |
| `nuint _reserved2` | `PTR_Thread m_pCurThread` |
| `void** _pObjRefs` | `PTR_OBJECTREF m_pObjRefs` |
| `uint _numObjRefs` | `UINT m_numObjRefs` |
| `int _maybeInterior` | `BOOL m_MaybeInterior` |

The managed constructor fills the last three and zeroes the two reserved words; the
InternalCalls are declared under `#if CORECLR` and bound in `vm/ecalllist.h:267-268` to
`GCReporting::Register` / `GCReporting::Unregister` (`vm/ecall.h:96`), whose bodies
(`vm/eetwain.cpp:1186-1203`) are, in full:

```cpp
FCIMPL1(void, GCReporting::Register, GCFrame* frame)   { _ASSERTE(frame != NULL); frame->Push(GetThread()); }
FCIMPL1(void, GCReporting::Unregister, GCFrame* frame) { _ASSERTE(frame != NULL); frame->Remove(); }
```

`Push` writes `m_pCurThread` and links the frame onto the head of the thread's GC frame
chain; `Remove` unlinks it from wherever in that chain it sits (explicitly *not* required to
be the head). The chain has exactly one consumer: `GCFrame::GcScanRoots`, called during a
collection, which promotes the `_numObjRefs` slots at `_pObjRefs` (as interior pointers when
`_maybeInterior`). The callers — `MethodBaseInvoker`, `MethodBaseInvoker.Constructor`,
`MethodInvoker`, `ConstructorInvoker` — all register a `stackalloc`'d block that the JIT's
own GC info cannot describe, because it holds `object` references and `ByReference`s at
runtime-computed offsets in what is statically an `IntPtr` block.

## What it must do in PawPrint

Nothing, and this is an exact behavioural match rather than a shortcut.

The registration's *entire* purpose is to make a block of stack slots visible to a garbage
collection. **PawPrint never collects and never moves an object** — the same fact
`NativeGc.tryExecuteQCall` already relies on when it says every allocation is permanently
pinned in the only sense `GC_ALLOC_PINNED_OBJECT_HEAP` can express, and that
`NativeGc.tryExecute` relies on when it reports every `last_gc_info` field as zero because
there has never been a GC of any kind.

So the set of guest-observable consequences of a registration on CoreCLR is
`{the referenced objects stay alive across the call}`, and PawPrint supplies that
unconditionally, for every object, registered or not. This is *observational equivalence*,
not a divergence, so it gets no entry in `docs/divergences.md`; a guest cannot construct an
experiment that distinguishes PawPrint's no-op from CoreCLR's chain manipulation.

Two supporting facts, both checked rather than assumed:

* Nothing managed ever reads `_reserved1` / `_reserved2`. They are `private`, are written
  only by the constructor (to zero), and are read only by the native `Push`/`Remove`. So a
  no-op `Register` leaves the struct in a state a no-op `Unregister` handles correctly, and
  no caller can observe the difference.
* No caller inspects a return value or an error: both methods return `void` and CoreCLR's
  only failure mode is an `_ASSERTE` on a null pointer.

## Options considered

**A. Validated no-op (chosen).** Match the exact signature; do nothing; complete the frame.
The signature match *is* the validation: a shape we have not read falls through to
`NativeDispatch.failUnimplemented`, which names the method and its signature.

**B. Interpreter-side registration table.** A per-thread list of
`{ pointer; numObjRefs; maybeInterior }` records, pushed by `Register` and removed by
`Unregister` (from any position, mirroring `Remove`), living beside `GcHandleRegistry` in
`IlMachineState`. Faithful to the chain, and hands a future collector an exact root set;
also detects an unbalanced or duplicate registration.

Rejected. Every byte of that table would be *write-only*: PawPrint has no collector to read
it, so no test can distinguish a correct table from a wrong one or from no table at all.
`union-byref-reinterpret` deleted a surviving mutant for exactly this reason ("carrying code
no test can kill is worse than the diff to remove it"), and a unit test asserting the table's
contents would be a test whose only subject is its own fixture. It is also not the cheap
option it looks: recording the fields means reading them back through a
`GCFrameRegistration*` that points at a guest stack local, so it takes a dependence on the
byref machinery that the no-op does not need — new failure modes in exchange for no
observable behaviour. If a collector is ever added, this table is what to add, and the
handler's doc comment says so.

**C. Faithful guest-memory chain.** Mimic `Push` by writing `_reserved1` (previous head) and
`_reserved2` (thread) into the guest's struct and keeping the head in thread state; `Remove`
unlinks. Rejected on the same write-only grounds as B, plus it needs a representation for
`Thread*` that PawPrint does not have and would have to invent for the sole purpose of
storing it somewhere nothing reads.

The cost of being wrong is low and symmetric: A → B is additive (the handler grows a body),
and nothing outside the handler depends on the choice.

## Design

One arm covering both methods, since the reason they are both no-ops is one reason and both
names are load-bearing (the guest calls both, so dropping either name from the pattern
reintroduces the failure — that is the mutation test):

```fsharp
| "System.Private.CoreLib",
  "System.Runtime",
  "GCFrameRegistration",
  ("RegisterForGCReporting" | "UnregisterForGCReporting"),
  [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                       "System.Runtime",
                                                       "GCFrameRegistration",
                                                       regGenerics)) ],
  MethodReturnType.Void when regGenerics.IsEmpty ->
    NativeHandlerResult.completed state |> Some
```

The argument is deliberately not inspected at all. In particular there is no null check
mirroring CoreCLR's `_ASSERTE(frame != NULL)`: all four callers pass `&someLocal`, so the
conjunct would be dead by construction, and a dead guard is worse than none — it reads as
though something checks it.

**Placement.** In `Native/NativeGc.fs`, extending the existing `tryExecute` (the name-based
InternalCall handler already registered in `NativeDispatch`), rather than a new
`NativeGcFrameRegistration.fs`. It is a GC-boundary InternalCall, it is a handful of lines,
and the module's existing "PawPrint never collects" reasoning is the reasoning this arm
needs — keeping them adjacent is the point. The `///` doc currently on `tryExecute` is
specific to `GC.GetMemoryInfo`; adding a second arm means giving `tryExecute` a short
summary doc and moving each arm's justification to a comment on the arm itself.

## Tests

`sourcesPure/ReflectionInvokeMethodManyArguments.cs` un-parks — it passes end-to-end with the
no-op in place (measured on `union-byref-reinterpret`, not predicted), and fails with a
message naming this exact InternalCall without it. That is the whole coverage, and it is not
vacuous: it is a differential test whose expectation comes from the real runtime, it exercises
both methods (`Unregister` runs in the `finally`), and an implementation that did anything
observable to the guest's struct or its arguments would break it.

Two edits follow from the un-parking:

* Remove the entry from `TestPureCases.unimplemented`.
* `sourcesPure/ReflectionInvokeMethodMultipleArguments.cs`'s header comment says the
  many-argument case is covered by the parked sibling; re-check its wording.

Mutations to run:

| Mutation | Expected killer |
| --- | --- |
| Drop `"RegisterForGCReporting"` from the arm | the many-args guest |
| Drop `"UnregisterForGCReporting"` from the arm | the many-args guest |
| Narrow the parameter pattern to `ConcreteByref` instead of `ConcretePointer` | the many-args guest (arm stops matching) |

## Explicitly out of scope

* `ConstructorInvoker` / `MethodInvoker` / `MethodBaseInvoker.Constructor`'s many-argument
  paths use the same primitive through different reflection machinery. A guest test for
  each would be testing that machinery, not this primitive, and would risk failing for
  unrelated reasons. Not added.
* Any root-set modelling (option B), unless and until PawPrint gains a collector.

## Dependency and base branch

Stacks on `union-byref-reinterpret` (PR #955), and the dependency is measured rather than
assumed: with this handler applied to plain `origin/main`, the same guest gets past the
InternalCall and then dies in the byref machinery with

```
cannot append ByteOffset 8 to projection list without a trailing ReinterpretAs:
  <byte 48 of <stack memory block #0> in method frame #1119 of thread 0>
```

which is the bug #955 fixes. Rebase onto `main` once #955 merges, and re-run before opening
the PR.
