# Issue #876 — `ExceptionDispatchInfo.Throw` loses the captured stack trace

## What is wrong today

`Exception.PrepareForForeignExceptionRaise` is a no-op (`Native/NativeException.fs:209`). CoreCLR
uses it to set a one-shot per-thread flag that the *next* throw on that thread reads twice; PawPrint
models neither the flag nor either read, so a rethrow through `ExceptionDispatchInfo.Throw()`
restarts the trace at the rethrow site and emits no boundary annotation.

## Measured ground truth (.NET 10, host runtime)

Probe in `scratchpad/ediprobe`. Trace text after each rethrow:

| case | boundary annotations | frames before first boundary |
|---|---|---|
| single `Capture`/`Throw` | 1 | original throw frames |
| capture → throw → capture → throw | 2 | original throw frames |
| `Capture(new Exception())` (never thrown) → `Throw` | 0 | none |
| plain `throw ex` on a caught exception | 0 | none (trace reset) |
| `await` a faulted `Task.Run` | 2 | original throw frames |

Per-frame flags read reflectively out of `System.Diagnostics.StackFrame`
(`IsLastFrameFromForeignExceptionStackTrace`) for the nested-async case:

```
[0] foreign=False Probe.Boom
[1] foreign=False Probe+<>c.<Inner>b__8_0
[2] foreign=True  System.Threading.ExecutionContext.RunFromThreadPoolDispatchLoop
[3] foreign=False System.Runtime.ExceptionServices.ExceptionDispatchInfo.Throw
[4] foreign=False System.Threading.ExecutionContext.RunFromThreadPoolDispatchLoop
[5] foreign=True  System.Threading.Tasks.Task.ExecuteWithThreadLocal
[6] foreign=False System.Runtime.ExceptionServices.ExceptionDispatchInfo.Throw
[7] foreign=False System.Runtime.CompilerServices.TaskAwaiter.ThrowForNonSuccess
[8] foreign=False System.Runtime.CompilerServices.TaskAwaiter.HandleNonSuccessAndDebuggerNotification
[9] foreign=True  Probe+<Inner>d__8.MoveNext
```

Two facts fall out of that dump that reasoning alone would have missed:

* the flag is **per frame**, and several frames in one trace can carry it;
* frame `[9]` carries the flag but *no annotation is printed for it*, because
  `StackTrace.ToString` suppresses the annotation when the flag-carrying frame's declaring type is
  a compiler-generated async state machine (`StackTrace.cs:361`, `&& !isAsync`). Frames `[3]`,
  `[6]`, `[10]` are `[StackTraceHidden]` and are dropped by `ShowInStackTrace` before rendering.

## Design decisions

### (a) Where the flag lives — `ThreadState`

CoreCLR keeps it in `ThreadExceptionState` (`TEF_ForeignExceptionRaise`, `exstate.h:113`): a
per-thread runtime fact, not something the guest could learn by asking the OS. `ThreadState`
already carries exactly this kind of thing (`IsBackground`), and `EmulatedKernel` explicitly does
not: the kernel holds what the guest could ask the OS for. New field:

```fsharp
/// Set by `Exception.PrepareForForeignExceptionRaise`, consumed by the next throw on this thread.
IsRaisingForeignException : bool
```

Four construction sites (`ThreadState.New` plus three in `IlMachineThreadState.fs`) must state
`false`; the compiler asks each one.

Rejected: a `Set<ThreadId>` on `IlMachineState`. There is a truthful default for an absent key
here ("not raising"), so it is not the `Cpu`/`OsThreadId` situation — but it would put a
thread-local runtime fact somewhere other than the thread, for no gain.

### (b) How the boundary is represented — per-frame flag on `ExceptionStackFrame`

```fsharp
type ExceptionStackFrame<...> =
    {
        Method : MethodInfo<...>
        IlOffset : int
        /// This frame is the last one carried over from a previous throw of the same exception.
        IsLastFrameFromForeignExceptionStackTrace : bool
    }
```

Three genuinely different shapes were considered.

1. **Per-frame flag** (chosen). Mirrors `STEF_LAST_FRAME_FROM_FOREIGN_STACK_TRACE`
   (`clrex.h:26`).
2. **Segmented trace** — `StackTrace : ExceptionStackFrame list list`, one segment per throw
   episode, boundaries implied between segments.
3. **Sum-type element** — `Frame of ExceptionStackFrame | ForeignBoundary` in a flat list.

The argument for (2)/(3) is that a boundary is *between* frames, not a property *of* one, and (2)
additionally makes "boundary at the very start or end" and "two adjacent boundaries"
unrepresentable. That is a real point, and against a runtime with no other opinion it would win.

It loses here because the CLR does have an opinion, and it is guest-observable.
`System.Diagnostics.StackFrame.IsLastFrameFromForeignExceptionStackTrace` is a **per-frame
boolean**, materialised by `debugdebugger.cpp:475-477` into a `bool[]` parallel to the frame array
and handed to managed code. Anything PawPrint stores must be able to answer that question frame by
frame. Shape (1) *is* that answer; shapes (2) and (3) require a flatten at every read, and the
flatten is only well-defined because of an invariant they do not themselves enforce (a boundary
never leads). So the "illegal states" advantage is smaller than it looks: what (2)/(3) make
unrepresentable is a state the *producer* can already not construct, while what they make awkward
is the shape the *consumer* demands.

Blast radius also differs by an order of magnitude: (1) touches one record and its two construction
sites; (2)/(3) change the element type of every trace list, so `CliException.StackTrace`,
`IlMachineState.FrozenStackTraces`, both `@ [ stackFrame ]` appends and the renderer all move.
(1) → (2)/(3) later is a mechanical refactor if that judgement turns out wrong.

The renderer must stay total over the representation: it emits the annotation after a marked frame
wherever that frame sits, including last. PawPrint's producer never marks a last frame, but that is
the producer's invariant and encoding it in the renderer would put it in the wrong place —
`StackTrace.ToString` is likewise position-independent (`StackTrace.cs:361-366`).

### (c) Where the flag is consumed — `ExceptionDispatching.throwExceptionObject`

CoreCLR consumes it in two places (`IL_Throw` decides whether to clear `_stackTrace`; the first
`AppendElement` marks the last existing frame and resets the flag). PawPrint has no
clear-at-throw step at all — `recordThrownStackTrace` simply overwrites `_stackTrace` at dispatch
conclusion — so both halves collapse into one place: the point where a throw seeds its
`CliException`.

```
throwExceptionObject:
    if thread.IsRaisingForeignException then
        clear the flag
        restored = frames behind the exception's own _stackTrace token   (may be empty)
        seed = markLastAsForeign restored @ [ throwSiteFrame ]
    else
        seed = [ throwSiteFrame ]
```

`markLastAsForeign []` is the identity, which is exactly CoreCLR's `numCurrentFrames > 0` guard
(`excep.cpp:3093`) and gives the measured zero-annotation answer for an EDI over a never-thrown
exception, for free rather than by special case.

Reading the frames from the exception's *own* `_stackTrace` (rather than from anything the flag
carries) is what makes this faithful: `RestoreDispatchState` has already written the captured token
there, and if some other exception were somehow thrown while the flag was set, CoreCLR would
likewise splice *that* exception's existing frames.

All `throwExceptionObject` call sites consume the flag, not just the `throw` opcode. CoreCLR's
`AppendElement` reset is likewise unconditional — the flag belongs to the thread's next dispatch,
whatever raises it.

`Rethrow` (`NullaryIlOp.fs:2426`) calls `dispatchException` directly. `IL_Rethrow`
(`jithelpers.cpp:890`) never *sets* the flag, so a plain `rethrow` produces no annotation and
accumulates frames onto the existing trace, which is already what PawPrint does — the standing TODO
there, "record the rethrow site as a boundary", is an instruction to *introduce* a divergence and
is replaced with the reason not to.

But a rethrow does *consume* a flag left pending by someone else, and an earlier draft of this
plan missed that, on the reasoning that only `ExceptionDispatchInfo.Throw()` sets the flag and it
always throws in the same breath. Codex found the hole: the setter is reflectively invocable —
PawPrint already has a test doing exactly that to `IsImmutableAgileException` — so a guest can set
the flag inside a `catch` and then `rethrow`.

The first attempt at that fix consumed the flag at the `rethrow` *instruction*, and Codex's second
round showed that to be wrong too, in two ways. Both were then measured on .NET 10 rather than
argued:

| scenario | real .NET |
|---|---|
| flag set, `rethrow` handled by another clause in the **same method** | **0** boundaries, and the flag is still pending — the next ordinary `throw` shows 1 |
| flag set, `rethrow` after a nested throw of the **same object** from inside the handler | **2** boundaries |

The first says the consumption point is the *frame append*, not the raise: a rethrow that finds its
handler without unwinding appends nothing, so `AppendElement` never runs. The second says the frames
being marked must come from the exception object, because CoreCLR re-reads `_stackTrace` at every
append — the handler's own view of the trace was fixed when it was entered and misses what the
nested throw added.

So `consumeForeignExceptionRaise` is called from PawPrint's two frame-append sites — the throw-site
frame in `throwExceptionObject` and the caller frame in `unwindToCallerAndSearch` — and both read
the frames from the exception's `_stackTrace` token. Replacing the in-flight list wholesale is safe
exactly because the flag can only be consumed once, so this is always the raise's first append and
the list it replaces is the one the raise started with.

One trap worth recording, found by the new tests rather than by reading: `unwindToCallerAndSearch`
carries `threadState` as a *value* alongside `state`, and hands it to the handler-entry path, which
writes it back. A consume that only updates `state` is silently undone — visibly so, since the
boundary then appears *and* the flag survives to be spent again.

### The pass-one ordering, and `mayConsumeForeignRaise`

Codex's third round found that "consume at an append" is still not the whole rule. CoreCLR appends
every frame of a raise in **pass one**, before running any cleanup clause, so a flag set by guest
code in a `finally` cannot be consumed by the raise unwinding through it — that raise has no
appends left. Measured: 0 boundaries on the unwinding exception, then 1 on the next raise. PawPrint,
which interleaves search with cleanup instead of completing a search pass first, would otherwise hang
the boundary on the caller frame it appends *after* the `finally` — getting both halves wrong at
once, and regressing a case that was previously right.

The first attempt at *that* was a `mayConsumeForeignRaise` parameter on the three dispatch
functions, with every resume path passing `false`. Codex's fourth round produced the mirror
scenario: set the flag *before* a `rethrow`, and let that rethrow pass through a `finally` on its
way out. Real .NET consumes the flag when the rethrow appends its caller frame — pass one, before
the `finally` body runs — giving 1 boundary then 0, where the parameter version gave 0 then 1.

The two scenarios reach the *same* resume site and differ only in when the flag was set, so no
constant at that site can serve both. What distinguishes them is a property of the raise:
"was a flag already pending when I began?" So it lives on the raise, as
`CliException.MayConsumeForeignRaise`, and the parameter is gone. Both of PawPrint's suspension
points already carry a `CliException` — `ExceptionContinuation.PropagatingException` and
`ExceptionFilterContinuation` — so it survives cleanup for free, which is the property the resume
sites could not supply. It is set true only by `rethrow` (the one raise that appends no frame of
its own at initiation), and cleared at the first append.

The mutation pair is the proof: forcing the field `false` at the `endfinally` resume fails
`ForeignRaiseFlagPendingBeforeCleanup` at exit 5, forcing it `true` fails
`ForeignRaiseFlagSetInFinally` at exit 4. No resume-site rule passes both.

Codex's fifth round then found that `rethrow` was setting the field to a blanket `true` rather than
to the flag actually pending at that instant — so a rethrow that began with nothing pending would
still take a flag its own `finally` set. That is a one-token fix, and it makes the code say what
the field's doc comment already claimed; `ForeignRaiseFlagSetInFinallyDuringRethrow.cs` pins it,
and reverting to `true` fails it at exit 5.

### What is left, and why it is not fixed here

Two residuals, both needing pass structure rather than a better statement of the current model:

* **A flag set from an exception *filter*.** Filters run in pass one, so CoreCLR would let a later
  append of the same raise consume it, where PawPrint's raise began before the flag existed and so
  declines.
* **A flag *stolen* during cleanup.** The raise records that a flag was pending and re-reads the
  thread's bit when it finally appends; an exception thrown and caught inside an intervening
  `finally` consumes it first. Parked as `sourcesPure/ForeignRaiseFlagNotStolenByCleanup.cs`,
  measured exit 5 against 0.

Closing the second means *transferring* ownership of the flag at raise initiation and handing it
back if the raise turns out to append nothing — and "turns out to append nothing" is only
answerable once dispatch distinguishes a cleanup handler from a real one. That is the `_isFinally`
that `tryFindAndEnterHandlerAtSearchPC` deliberately ignores, i.e. issue #865's two-pass structure.
Both residuals are the same shape, and no approximation short of #865 would be honest about it.

A plain rethrow still carries the catch handler's snapshot rather than re-reading the token, so it
can report a staler trace than real .NET. That is pre-existing and independent of the flag; making
it read the token unconditionally would lose frames whenever the token is absent, because
`recordThrownStackTrace` declines to mint one for an empty frame list. Left as a follow-up.

The other half of `IL_Throw`'s flag branch, `SetStackTraceString(NULL)`, needs no PawPrint
counterpart: `RestoreDispatchState` assigns `_stackTraceString = null` in managed code
(`Exception.CoreCLR.cs:141`), which PawPrint interprets like any other store. CoreCLR's native
null is there for flag-setters that bypass `RestoreDispatchState`, of which the only one is
`IL_ThrowExact` (`jithelpers.cpp:937`) — a JIT helper with no managed caller in CoreLib, so
unreachable under IL interpretation.

**Multi-threading.** Placing the flag on the thread (decision (a)) is what makes concurrent guests
correct: a worker rethrowing a `Task` fault through an EDI sets and consumes its own flag, and two
threads rethrowing the same captured exception do not interfere. The residual race — thread B
overwriting the shared exception object's `_stackTrace` between A's `RestoreDispatchState` and A's
throw — exists in CoreCLR too (`AppendElement` re-reads the array from the object at every append);
PawPrint determinises one interleaving of it rather than introducing it.

**Explicitly not done here**: the `else` branch does *not* clear `_stackTrace`/`_stackTraceString`,
so PawPrint still diverges from `ClearStackTracePreservingRemoteStackTrace` on an ordinary
`throw ex` of an already-thrown exception. That is a pre-existing, separate divergence; adding it
to this change would alter behaviour for every ordinary throw, including the empty-frame-list case
that produced the #870 regression. Follow-up issue.

### (d) Rendering — emit unconditionally

`renderExceptionStackTrace` emits `--- End of stack trace from previous location ---` on its own
line after any frame whose flag is set. The literal is CoreLib's
`SR.Exception_EndStackTraceFromPreviousThrow` (`Strings.resx:2291`); PawPrint has no resource
pipeline, exactly as with `NativeException.messageForKind`.

The `&& !isAsync` suppression is *not* modelled. It is one of a family of display policies in
`StackTrace.ToString` that PawPrint's renderer implements none of — `ShowInStackTrace`'s
`[StackTraceHidden]` and `AggressiveInlining` filters (which is why PawPrint will show the
`ExceptionDispatchInfo.Throw` frames that real .NET hides), and `TryResolveStateMachineMethod`'s
rewrite of `<Inner>d__8.MoveNext` back to `Inner()`. Modelling the async suppression alone would be
half of one rule from that family. Better as one coherent follow-up covering the display filters
together; filed as its own issue and noted in `docs/divergences.md`.

This does mean a *new* line-level mismatch in async traces (PawPrint prints an annotation real .NET
suppresses) in exchange for fixing the non-async case. Stated in the follow-up rather than left
silent.

## Change list

* `Exceptions.fs` — new field on `ExceptionStackFrame`.
* `ThreadState.fs` — new field, `ThreadState.New` sets `false`.
* `IlMachineThreadState.fs` — three further construction sites set `false`.
* `IlMachineRuntimeMetadata.fs` — `renderExceptionStackTrace` emits the annotation; a
  `frozenStackTraceFrames` reader (token → frames) next to `frozenStackTraceToken`.
* `ExceptionDispatching.fs` — `throwExceptionObject` consumes the flag and splices; the two
  frame-construction sites set the flag `false`.
* `NullaryIlOp.fs` — the `Rethrow` TODO asking for a boundary at the rethrow site is replaced by
  the reason a rethrow of its own must not produce one, and by where the flag is handled instead.
* `Native/NativeException.fs` — `PrepareForForeignExceptionRaise` sets the flag; its comment stops
  describing a divergence.
* `docs/divergences.md` — delete the `ExceptionDispatchInfo.Throw` entry; add the renderer
  display-filter entry.
* `sourcesPure/ExceptionDispatchInfoThrow.cs` — comment no longer claims the trace is lost.

## Tests

`sourcesPure/ExceptionDispatchInfoThrowPreservesTrace.cs`, differential on exit code, positive
substring assertions only (trace text is not comparable across runtimes — real .NET appends
`in file:line`, PawPrint does not). Hand-rolled substring/count helpers, because
`string.Contains(string)` and the `StringComparison` overloads are unimplemented JIT intrinsics
under PawPrint.

1. single hop: annotation count is exactly 1, with the original throwing method's name *before* it
   and the rethrowing method's name *after* it.
2. double hop: annotation count is exactly 2.
3. EDI over a never-thrown exception: trace is non-null and annotation count is 0.
4. a plain `throw ex` of a previously-thrown exception, immediately after an `EDI.Throw()` on the
   same thread: annotation count is 0.

Counting exactly, rather than "contains", is what makes the test pin *placement* and not merely
presence; all four counts were measured on real .NET above.

Case 4 exists because of a mistake in an earlier draft of this plan, which claimed a leaked flag
would be caught by case 2. It would not: every `EDI.Throw()` re-sets the flag before splicing, so
cases 1–3 are blind to whether it was ever cleared, and a fresh exception thrown under a leaked
flag splices an empty list — the identity. The one observable discriminator is a plain `throw` of
an *already-thrown* exception under a leaked flag, which would splice and mark where real .NET
resets the trace. Both runtimes answer 0, so it is differential-safe.

Mutation results (each applied to a clean tree, then reverted from a scratchpad backup):

| mutation | outcome |
|---|---|
| never set the flag (pre-fix behaviour) | case 1 fails, exit 13 |
| mark every restored frame, not just the last | case 1 fails, exit 14 |
| never clear the flag after consuming | case 4 fails, exit 43 |
| assume a foreign raise always has restored frames (`None -> failwith`) | case 3 crashes |
| the append site does not consume the flag | `ForeignRaiseFlagConsumedByRethrow` fails, exit 4 |
| never clear the flag (again, against the rethrow test) | `ForeignRaiseFlagConsumedByRethrow` fails, exit 7 |
| consume when the raise begins rather than at the append | `ForeignRaiseFlagSurvivesFramelessRethrow` fails, exit 4 |
| mark the in-flight snapshot instead of the exception's token | `ForeignRaiseReadsCurrentExceptionTrace` fails, exit 4 |
| force the raise ineligible at the `endfinally` resume | `ForeignRaiseFlagPendingBeforeCleanup` fails, exit 5 |
| force the raise eligible at the `endfinally` resume | `ForeignRaiseFlagSetInFinally` fails, exit 4 |
| start every `rethrow` eligible instead of reading the flag | `ForeignRaiseFlagSetInFinallyDuringRethrow` fails, exit 5 |

Seven test files, six passing and one parked, each killed by a different mutation:

* `ExceptionDispatchInfoThrowPreservesTrace.cs` — the ordinary `EDI.Capture`/`Throw` round trip.
* `ForeignRaiseFlagConsumedByRethrow.cs` — a reflective set-then-`rethrow` that *does* unwind:
  one annotation, and the flag spent.
* `ForeignRaiseFlagSurvivesFramelessRethrow.cs` — the same, but handled in the same method: no
  annotation, and the flag *not* spent.
* `ForeignRaiseReadsCurrentExceptionTrace.cs` — a nested throw of the same object before the
  rethrow: two annotations, only reachable by reading the token.
* `ForeignRaiseFlagSetInFinally.cs` — the flag set from a `finally` during an unwind: no
  annotation on the unwinding exception, one on the next raise.
* `ForeignRaiseFlagPendingBeforeCleanup.cs` — its mirror: the flag set before a `rethrow` that
  then passes through a `finally`. One annotation, and nothing left pending.
* `ForeignRaiseFlagSetInFinallyDuringRethrow.cs` — the `rethrow` counterpart of
  `ForeignRaiseFlagSetInFinally.cs`: nothing pending when the rethrow starts, the `finally` sets a
  flag, and the rethrow must not take it.
* `ForeignRaiseFlagNotStolenByCleanup.cs` — **parked**: a raise inside a `finally` consuming the
  flag the outer raise had claimed. Needs #865.

Every one of these after the first exists because a Codex round named the shape; each was measured
on real .NET before its assertion was written, and each kills a mutation the others survive.

## Follow-ups (not this PR)

* PawPrint does not clear `_stackTrace`/`_stackTraceString` on an ordinary `throw ex`
  (`ClearStackTracePreservingRemoteStackTrace`).
* A plain `rethrow` extends the snapshot the catch handler was entered with, where CoreCLR
  re-reads the exception's current `_stackTrace`. Observable when the same object is thrown again
  from inside the handler.
* A foreign-raise flag set from an exception *filter* is treated as belonging to the next raise,
  where CoreCLR — for which filters run in pass one — would let the current raise consume it.
  Needs #865's two-pass dispatch.
* PawPrint's trace renderer implements none of `StackTrace.ToString`'s display policy:
  `[StackTraceHidden]`/`AggressiveInlining` filtering, async state-machine name resolution, and the
  `!isAsync` suppression of this very annotation.
* `renderExceptionStackTrace` joins with `System.Environment.NewLine` — a host read in a library
  that is meant to have none, so a replay of the same run on Windows would render `\r\n`.
