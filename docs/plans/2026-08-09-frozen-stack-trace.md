# Plan: `ExceptionNative_GetFrozenStackTrace` (issue #754)

Status: **PR 1 implemented.** §3a describes what shipped; §3, §5 record how the decisions were
made. PRs 2-4 are still to do.

Two things changed between plan and implementation, both recorded below:
`ExceptionDispatchInfo.SetCurrentStackTrace` turned out not to be usable in the differential
test (it needs PR 2 *and* PR 4), and no `docs/divergences.md` entry is needed after all.

Reviewed by a second agent (Fable), whose argument changed the recommendation; §3 records
the disagreement and its resolution.

## 1. What was measured (not assumed)

All of the following was established by running PawPrint, not by reading.

### 1.1 The blocker reproduces without `Task` at all

```csharp
try { Boom(); } catch (Exception e) { caught = e; }
ExceptionDispatchInfo.Capture(caught);   // <-- fails here
```

fails with `Unimplemented native method (PInvokeImpl QCall!ExceptionNative_GetFrozenStackTrace)`.
So the feature has a *minimal* guest test that does not drag in the thread pool, the
scheduler, or `Task`.

### 1.2 Three natives — not one — gate the parked `Task` test

I stubbed each in turn and re-ran `sourcesPure/TaskRunThrowSetsFaulted.cs`'s program:

| # | Native | Kind | Reached from |
|---|--------|------|--------------|
| 1 | `ExceptionNative_GetFrozenStackTrace` | QCall | `EDI.Capture` → `Exception.CaptureDispatchState()` |
| 2 | `Exception::IsImmutableAgileException` | FCall (InternalCall) | `Exception.RestoreDispatchState` |
| 3 | `Exception::PrepareForForeignExceptionRaise` | FCall (InternalCall) | `Exception.RestoreDispatchState` |

With all three stubbed the parked test passes end to end (`aggregate inner:
System.InvalidOperationException`, exit 0), and so does the bare `t.IsFaulted` spin.

**Consequence for scoping: implementing this QCall alone does NOT un-park
`TaskRunThrowSetsFaulted.cs`.** The parked-test reason in `TestPureCases.fs:30` will need
rewriting to name blocker #2, not deleting. (Note #2 and #3 arrive via
`ExceptionDispatchInfo.Throw()`, which *is* on Task's own fault-recording path — the
thread-pool dispatch loop rethrows through an EDI — so a bare `IsFaulted` check reaches
them too.)

### 1.3 `_stackTrace` is a runtime-private opaque slot

Read of the pinned source (`Exception.CoreCLR.cs`, `Exception.cs`): **no managed code ever
type-tests, casts, indexes or serialises `_stackTrace`.** Its only managed uses are:

- null-check — `HasBeenThrown` (`Exception.cs:175`), consumed by `Exception.StackTrace`
  (`Exception.cs:220`), `Exception.Source` (`Exception.cs:98`) and
  `SerializationStackTraceString` (`Exception.cs:270`); and `CanSetRemoteStackTrace`
  (`Exception.CoreCLR.cs:252`);
- passed opaquely to the QCall `ExceptionNative_GetMethodFromStackTrace` (`TargetSite`);
- round-tripped through `DispatchState` / `RestoreDispatchState`.

`GetObjectData` writes `SerializationStackTraceString`, never the object.
`Diagnostics.StackTrace(exception)` does not read the field from managed either — it hands
the *Exception* to `StackTrace_GetStackFramesInternal` and the native side reads it.

So **PawPrint is free to choose any representation**, subject only to null-vs-non-null
fidelity and to its own natives being able to decode it.

### 1.4 Returning non-null is not free — measured

With a non-null opaque `byte[]` returned, `ex.Source` on a thrown exception stops being
silently `null` and instead routes `HasBeenThrown → TargetSite →
ExceptionNative_GetMethodFromStackTrace`, which **crashes** (`Unimplemented native
method`). Verified by running it. The `Task` path itself does not read `.Source`, so the
parked test still passes; but any guest that touches `.Source` or `.TargetSite` on a thrown
exception regresses from "quietly wrong" to "dead".

No file under `sourcesPure/` or `sourcesImpure/` reads `.Source` or `.TargetSite` today, so
this regression would ship green and only surface on a future guest.

### 1.5 What "return null" costs — measured

With the null stub plus #2/#3, PawPrint reports for the Task case:

```
   at System.Runtime.ExceptionServices.ExceptionDispatchInfo.Throw()
   at System.Threading.ExecutionContext.RunFromThreadPoolDispatchLoop(...)
   at System.Threading.Tasks.Task.ExecuteWithThreadLocal(...)
```

Real .NET (same program, run as an oracle) reports:

```
   at Program.<>c.<Main>b__1_0() in Program.cs:line 12
   at System.Threading.Tasks.Task`1.InnerInvoke()
   at System.Threading.ExecutionContext.RunFromThreadPoolDispatchLoop(...)
--- End of stack trace from previous location ---
   at System.Threading.ExecutionContext.RunFromThreadPoolDispatchLoop(...)
   at System.Threading.Tasks.Task.ExecuteWithThreadLocal(...)
```

i.e. the *original throw site is lost*. The mechanism is exactly
`RestoreDispatchState` setting `_stackTraceString = null` (`Exception.CoreCLR.cs:141`)
while PawPrint has nothing in `_stackTrace` to reconstruct from; PawPrint's unwinder then
re-renders `_stackTraceString` from the post-rethrow frames only.

### 1.6 Real-.NET facts a differential test can assert (all verified against the oracle)

- `Capture` does not mutate the source exception's `StackTrace`.
- `edi.SourceException` is reference-equal to the captured exception.
- `Capture` on a never-thrown exception leaves `StackTrace == null`.
- `ExceptionDispatchInfo.SetCurrentStackTrace` throws `InvalidOperationException` for an
  already-thrown exception and succeeds for a fresh one.

Exact trace *strings* are not comparable (`Roslyn.fs:23-30`), so the test must use these
structural facts, not string equality.

The last one is the sharp instrument. `SetCurrentStackTrace` → `CanSetRemoteStackTrace`
tests `_stackTrace != null` *from managed code*, so:

> **`Capture(fresh); SetCurrentStackTrace(fresh)` must succeed.**

kills the "always return a non-null object" mutant, which would make it throw
`InvalidOperationException`. Per the `mutation-test-new-end-to-end-cases` note, the new
test should be checked against exactly that mutant before the PR lands.

(For an already-*thrown* exception the probe is blunt: PawPrint sets `_stackTraceString`,
so `CanSetRemoteStackTrace` throws whatever `_stackTrace` holds. Only the fresh case
discriminates.)

## 2. The decision to make

What does PawPrint put in `Exception._stackTrace`?

### Option A — always `null`

`GetFrozenStackTrace` writes a null objectref. "PawPrint has no frozen stack-trace object."

- **Cost**: ~30 lines, one file, no new state. Nothing regresses (§1.4 shows this is the
  only option that regresses nothing).
- **Loses**: the original throw site across every `EDI.Throw()` (§1.5), permanently, until
  a later PR replaces it. Also keeps `Exception.Source` silently `null` on thrown
  exceptions (a pre-existing divergence, unchanged).
- **Reversibility**: total. Nothing outside the one handler depends on the choice.
- **Honesty**: this is the option the project's "prefer crashing over documented
  divergence" note argues *against* — it is a silent lie ("never thrown") rather than a
  crash. The counter-argument is that it is not a lie about anything PawPrint models: the
  field means "the runtime's private frozen frame array", and PawPrint has none.
- **Fatal objection (§3(i))**: nothing can test it. Every §1.6 fact is satisfied by a
  handler that writes null and does nothing else, and there is no state to inspect.

### Option B — token heap object + side table, minted at *unwind* time

(This is Fable's "A′", and the shape I now recommend.)

Extend the single existing projection helper
`IlMachineRuntimeMetadata.setExceptionStackTraceString` (`IlMachineRuntimeMetadata.fs:932`,
called from `ExceptionDispatching.fs:462, 500, 530, 570` and
`IlMachineStateExecution.fs:1832`) so that, alongside writing `_stackTraceString`, it:

- allocates an opaque token object (a zero-length `sbyte[]`, matching CoreCLR's `I1Array`
  type identity should anything ever look);
- records `token address → ExceptionStackFrame<...> list` in a new `IlMachineState` field;
- writes the token into the exception's `_stackTrace`.

`GetFrozenStackTrace` then becomes exactly what CoreCLR's is: read `_stackTrace`, write the
same reference back through the out handle (null → null). `MarkAsFrozen` needs no analogue:
the table's values are immutable F# lists and each dispatch mints a *fresh* token, so
CoreCLR's clone-on-append and cross-thread-clone semantics come for free.

- **Cost**: one new `IlMachineState` field, one changed helper, one allocation per exception
  dispatch. `GetFrozenStackTrace` itself becomes trivial.
- **Preserves**: everything — real `MethodInfo` values and IL offsets, which is exactly what
  `GetMethodFromStackTrace` and `StackTrace_GetStackFramesInternal` will need.
- **Reversibility**: total. Nothing outside PawPrint's own natives ever interprets the token.
- **Fixes a standing divergence**: PawPrint never writes `_stackTrace` today, so
  `Exception.HasBeenThrown` is `false` for *every* thrown exception, and `Exception.Source`
  silently returns `null` where real .NET returns the assembly name. Option B makes
  `HasBeenThrown` truthful.
- **Cost of that fix**: `.Source`/`.TargetSite` on a thrown exception go from silently-null
  to a **loud crash** at the unimplemented `GetMethodFromStackTrace` (§1.4). Per the
  project's "prefer crashing over documented divergence" stance that is the right
  direction — but it is a real behaviour change on a path no test covers.
- **Placement**: `IlMachineStateModel.fs` (fsproj line 60) is the home — it compiles after
  both `ManagedHeap.fs` (45) and `Exceptions.fs` (47), and already hosts `GcHandles`,
  `TypeHandles`, `MethodHandles`. `ManagedHeap` itself cannot host it without reordering.
- **Not a concern**: PawPrint has no collecting GC, and `ManagedHeap.FirstAvailableAddress`
  only ever increments (`ManagedHeap.fs:106, 170-176`) with no removal API, so addresses are
  never reused and an address-keyed table cannot go stale or need rooting. Address-keyed
  side tables are already the house pattern: `StringContents`, `StringDataOffsets`,
  `SyncBlocks` (`ManagedHeap.fs:116, 120, 135`). Leave a comment that a future collecting GC
  must weak-key this table.
- **Variant considered and rejected**: keying the table by *exception* address and minting
  the token only at capture time. That keeps `_stackTrace` null and `HasBeenThrown` wrong,
  needs two maps with two meanings, and lets a second throw of the same object retroactively
  rewrite what an earlier `Capture` saw.

### Option C — faithful `sbyte[]` `StackTraceArray` encoding

Encode CoreCLR's real layout: `ArrayHeader { uint32 size; uint32 keepAliveCount; Thread* }`
followed by `StackTraceElement` records (`object.h:1926`).

- **Rejected.** `StackTraceElement` holds a `MethodDesc*`. PawPrint has no such pointer,
  and synthesising one into a byte array is precisely the "bit-twiddling on
  provenance-tracked pointers" trap AGENTS.md calls out. It buys fidelity to a layout no
  PawPrint code and no guest code ever reads.

### Option D — store the rendered trace *string object*

Put the same string object PawPrint already writes to `_stackTraceString` into
`_stackTrace` too.

- **Cost**: tiny; no new state.
- **Preserves**: enough to make `EDI.Throw()` restore the original trace text later (a
  future `PrepareForForeignExceptionRaise` could re-concatenate it), which is the single
  most valuable thing lost under Option A.
- **Against**: it conflates identity with projection — the field's contract is
  "structured frames", and handing it a rendered string is exactly the "don't coerce a
  handle into a different identity to reuse machinery" anti-pattern in AGENTS.md. It also
  inherits Option B's `.Source`/`TargetSite` regression, for less information.

## 3. Recommendation, and where the review changed it

My first recommendation was **Option A**, on the grounds that §1.4 makes non-null the only
option that regresses something. Fable argued for **Option B**, and I now agree. Two points
decided it, both of which I had underweighted:

**(i) Option A is vacuously satisfiable, and therefore untestable.** Every differential fact
in §1.6 is satisfied by a handler that writes null and does nothing else — because after an
EDI restore PawPrint's unwinder re-renders `_stackTraceString` anyway, and nothing ever
decodes `_stackTrace`. There is no assertion that a correct Option-A implementation passes
and a broken one fails, and no state for a PawPrint-only test to inspect. Per the
`parked-tests-must-not-be-vacuous` and `mutation-test-new-end-to-end-cases` notes, that is
disqualifying on its own: we would be shipping a feature whose only test is "did not crash".

**(ii) Option A *adds* a silent divergence; Option B *removes* one.** I had framed the
`.Source` crash as Option B's regression. But the baseline is already wrong: PawPrint never
writes `_stackTrace`, so `HasBeenThrown` is false for every thrown exception and `.Source`
silently returns null where real .NET returns the assembly name. Option A entrenches that;
Option B converts it into a loud crash at a named unimplemented QCall, which is the
direction the project's own "prefer crashing over documented divergence" note points.

So: **ship Option B as PR 1**, minting the token in the existing unwind projection helper.

I record my one residual reservation: (ii) is a genuine behaviour change on a path no test
covers (§1.4), so guests that read `.Source` and run today will stop running. If you would
rather not take that, the fallback is Option B *plus* `ExceptionNative_GetMethodFromStackTrace`
in the same PR — which makes `.Source` correct instead of fatal, but is a second feature and
needs guest `IRuntimeMethodInfo` construction, so I would not assume it is small. See §4.

### PR sequence

1. **PR 1 — this issue.** Option B: token minted at unwind, `_stackTrace` set,
   `GetFrozenStackTrace` reads the field and writes it through the out handle. Decode paths
   (`GetMethodFromStackTrace`, `StackTrace_GetStackFramesInternal`) stay loud
   `failUnimplemented`. `TaskRunThrowSetsFaulted.cs` **stays parked**, its reason rewritten to
   name the verified next blocker.
2. **PR 2 —** `Exception::IsImmutableAgileException` → `false`, and
   `Exception::PrepareForForeignExceptionRaise` → no-op with a comment naming the divergence
   it leaves. Un-parks `TaskRunThrowSetsFaulted.cs` (§1.2 proves the three together reach
   exit 0). Real semantics of the first: "is this one of the runtime's preallocated singleton
   exception objects" (`comutilnative.cpp:49`); PawPrint allocates every exception freshly
   (`ExceptionDispatching.allocateRuntimeException`), so `false` is honest — but the cached
   `TypeInitializationException` path should be checked before asserting it.
3. **PR 3 — foreign-raise append.** A per-thread "raising foreign exception" flag on
   `ThreadState`, set by `PrepareForForeignExceptionRaise` and consumed by the throw path to
   *prefix* the restored token's frames plus a boundary marker instead of overwriting.
   Closes §1.5.
4. **PR 4 — `GetMethodFromStackTrace`**, making `.Source`/`.TargetSite` real.

## 3a. What PR 1 shipped

- `IlMachineStateModel.fs`: new `FrozenStackTraces : Map<ManagedHeapAddress,
  ExceptionStackFrame<...> list>` field; initialised in `IlMachineThreadState.initial`.
- `IlMachineRuntimeMetadata.fs`: three new functions —
  `stackTraceFieldId` (private; resolves `_stackTrace` against `System.Exception`, since it is
  declared there and not on the derived exception type), `frozenStackTraceToken` (reads the
  field, returning `None` for never-thrown and failing loudly on a token PawPrint did not
  mint), and `recordThrownStackTrace` (mints, registers, writes the field).
  `setExceptionStackTraceString` is **unchanged** — see §5 for why they are separate.
- `ExceptionDispatching.fs`: the four dispatch-conclusion sites now also call
  `recordThrownStackTrace`. `IlMachineStateExecution.fs:1832` deliberately does not; it passes
  a literal `[]` and does not mean "this is being dispatched" (§5).
- `Native/NativeException.fs`: new arm in `tryExecuteQCall`, reading the exception through
  argument 0 and writing its `_stackTrace` through argument 1. Fails on a null *exception*
  (as CoreCLR asserts, `comutilnative.cpp:88`) but not on a null *trace*, which is the legal
  never-thrown case.
- `Native/NativeQCall.fs`: one registration line.
- Tests: `sourcesPure/ExceptionDispatchInfoCapture.cs`,
  `sourcesImpure/ExceptionDispatchInfoCaptureState.cs` + its `TestImpureCases` registration.
- `TestPureCases.fs`: `TaskRunThrowSetsFaulted.cs` parked reason rewritten to name the
  measured next blocker.

**No `docs/divergences.md` entry, contrary to the plan.** That register is explicitly for
things that are *not* simply unimplemented. Under Option B nothing newly diverges silently:
`.Source`/`.TargetSite` now crash at a named unimplemented QCall, which is the ordinary
unimplemented state, and the `EDI.Throw` frame loss is not reachable until PR 2 lands. Revisit
when PR 2 makes it reachable.

The write-once invariant is asserted in `recordThrownStackTrace`: a freshly allocated token
that is already registered means heap addresses have been reused, and fails loudly.

## 3b. Tests

- **Differential (`sourcesPure/ExceptionDispatchInfoCapture.cs`)** — asserts §1.6. Observed
  failing before the change with the `failUnimplemented` crash; passes on both runtimes after.
  Includes the unthrown-capture case, which kills a `failwith`-on-null-trace implementation.

  Two §1.6 facts had to be dropped: both `ExceptionDispatchInfo.SetCurrentStackTrace`
  assertions. `CanSetRemoteStackTrace` calls the `IsImmutableAgileException` InternalCall
  (PR 2), and its success path builds a `new StackTrace(...)`, needing
  `StackTrace_GetStackFramesInternal` (PR 4). Neither is reachable yet, so the sharpest
  managed-side probe of `_stackTrace` nullness is unavailable until PR 4. Worth adding then.
- **PawPrint-only host test (`sourcesImpure/ExceptionDispatchInfoCaptureState.cs`) — the
  load-bearing one.** Walks `ExceptionDispatchInfo._dispatchState.StackTrace`, requires it to
  hold a token registered in `FrozenStackTraces`, and requires that token's frames to name the
  guest's `Thrower` and `Main`.
- **Not done: snapshot semantics.** A guest that captures at site A, rethrows the same object,
  and captures at site B, asserting two distinct tokens with different frame lists. Worth
  adding, but it wants `EDI.Throw` (PR 2) to be a natural rethrow vehicle; a plain `throw ex;`
  would work today. Left for PR 2.
- **Mutation check — run, both mutants killed.**
  1. Handler writes `ObjectRef None`: differential test stayed **green**, host test went
     **red** ("DispatchState.StackTrace is null after capturing a thrown exception"). This is
     the concrete confirmation of §3(i) — the differential test alone would have shipped a stub.
  2. Handler writes some other live object instead of the field's value: differential stayed
     **green**, host test went **red** ("not a token PawPrint minted").

  Full suite after restoring: 2442 passed, 0 failed.

## 3c. Review findings

An OpenAI Codex review of the branch raised one substantive point, and it is correct:
`tryFindAndEnterHandlerAtSearchPC` ignores its `_isFinally` flag, so the trace is snapshotted
on entry to *cleanup* handlers too — where `cliException.StackTrace` holds only the frames
unwound so far, because PawPrint interleaves handler search with cleanup instead of completing
a first pass first as CoreCLR does.

Verified, and **pre-existing**: reading the in-flight exception's `StackTrace` from inside a
`finally` gives `at Program.B()` under PawPrint against `B`/`A`/`Main` on real .NET, on `main`
as much as on this branch, because `setExceptionStackTraceString` already snapshots the same
partial list at the same point. This PR extends that same list to the frozen-trace token, which
has no decoder yet, so nothing newly observable diverges.

Not fixed here: the fix is to give dispatch a real two-pass structure, which reshapes
`ExceptionDispatching` rather than patching a call site. Filed as issue #865, and the call site
now carries a comment saying so — including why recording the partial trace still beats skipping
the write for cleanup handlers (`HasBeenThrown` would then be wrong, which is a worse answer to
a different question).

## 4. Open questions for the user

1. **Resolved (user, 2026-08-09): crashing on `.Source`/`.TargetSite` is accepted**, on the
   grounds that it is the more honest behaviour. `GetMethodFromStackTrace` stays in PR 4.
2. **Resolved (user, 2026-08-09): follow-ups stay separate.** PR 1 ships
   `GetFrozenStackTrace` alone; `IsImmutableAgileException` and
   `PrepareForForeignExceptionRaise` are PR 2, which is what un-parks
   `TaskRunThrowSetsFaulted.cs`.
3. **Resolved — see §5.** (Was: what to do about empty frame lists.)

## 5. The empty-frame-list question, and its answer

### What the question is

`setExceptionStackTraceString` opens with:

```fsharp
match stackTrace with
| [] -> state
| _ :: _ -> (* render, allocate the string, write `_stackTraceString` *)
```

Today that helper does exactly one job: project a frame list into a rendered string. The
early return means "no frames, nothing to render, leave the field alone".

Option B gives the same helper a *second* job: declare, by writing a non-null `_stackTrace`,
that this exception **has been thrown**. Those two facts have different truth conditions, and
the empty list is precisely where they come apart:

- "I have frames to render" — false for an empty list.
- "This exception has been thrown" — can perfectly well be true for an empty list.

So the question is only: does the early return keep covering both writes, or just the string?

### Where an empty list actually comes from

There are five call sites. Four (`ExceptionDispatching.fs:462, 500, 530, 570`) pass
`cliException.StackTrace`, which is seeded with one frame at the throw
(`ExceptionDispatching.fs:786-796`) and appended to on each unwind step (`:611-620`). The
synthesised `TypeInitializationException` / `TargetInvocationException` wrappers *are*
constructed with `StackTrace = []` (`:551, :587`), but control then falls through to the
pop-and-append at `:592-620` before any handler search, so those four sites see a non-empty
list in every path I traced.

The one site that passes a literal `[]` is `IlMachineStateExecution.fs:1832` — the
`Activator.CreateInstance<T>()` path where T's `.cctor` previously failed and its
`TypeInitializationException` was cached. **That call is a guaranteed no-op today**, because
the literal `[]` always takes the early return. It is dead code.

That matters, because the cached TIE was thrown on an earlier call, so it *already* carries a
`_stackTraceString` from that dispatch — and the early return is what preserves it. A version
of the helper that overwrote unconditionally would wipe a real stack trace here.

### Options

**(a) Early return keeps covering both writes.** Simplest; one code path. Under Option B the
cached TIE at `:1832` keeps the token it got from its original dispatch, so nothing is lost
there. The risk is residual: if any path ever does reach a dispatch site with an empty list,
that exception has genuinely been thrown but will report `HasBeenThrown = false` — the exact
divergence Option B exists to remove, surviving in a corner.

**(b) Always mint the token; keep only the string write conditional.** Honest about the two
facts being different. But at `:1832` it would mint a *fresh, empty* token over the real one
from the first dispatch — losing information, and violating the write-once invariant proposed
in §3a. Needs `:1832` special-cased, which is a smell.

**(c) Split the helper (recommended).** Leave `setExceptionStackTraceString` as a pure string
projection, early return and all. Add a separate function that mints the token and sets
`_stackTrace`, called only from the four dispatch sites that genuinely mean "this exception is
being dispatched". `:1832` keeps calling only the string version, where its no-op is correct.

(c) is right because the two writes answer different questions and have different callers. It
also matches the project's guidance on keeping a classifier's contract truthful and
load-bearing: "has this been thrown" should be decided by *being on the dispatch path*, not
inferred from whether a frame list happens to be non-empty.

**Follow-up worth noting but not doing here:** `:1832`'s call is dead. Either it was meant to
do something (in which case it is a latent bug) or it is vestigial (in which case it should
go). It should not be quietly repurposed by this PR; flag it separately.
