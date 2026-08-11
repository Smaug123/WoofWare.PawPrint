# Issue #865 — exception dispatch has no first pass

## What was wrong

CoreCLR dispatches in two passes. The **first** walks frames from the throw point outward,
running every `filter` and appending one stack-trace frame per frame boundary crossed, and stops
at the first `catch`/`filter` that accepts. The **second** then unwinds from the throw point to
that clause, running `finally` and `fault`. Cleanup never runs before the trace is complete.

PawPrint interleaved them. `unwindToCallerAndSearch` popped a frame, appended its trace entry,
and searched the caller for *any* covering region — cleanup included, because
`tryFindAndEnterHandlerAtSearchPC` was handed an `isCleanup` flag by
`findExceptionHandlerSkippingFilters` and ignored it. Every instance recorded on #865 followed
from that one ignored flag.

## Measured ground truth (.NET 10, host runtime)

Probe in `scratchpad/twopassprobe`, one mode per fact.

| mode | fact | PawPrint before |
|---|---|---|
| `trace` | a `finally` running during propagation reads a trace covering every frame up to *and including* the one owning the handler, and no further | only the frames unwound so far |
| `order` | an outer frame's `when` runs before an inner frame's `finally` — observed `L F C` | `F L C` |
| `unhandled` | an unhandled exception still unwinds and runs every `finally`, with the complete trace visible to it; banner first, then cleanup, then abort (134) | cleanup ran, but on a truncated trace |
| `exitfinally` | `Environment.Exit` from such a `finally` wins; process exits with its code | same |
| `filtertrace` | a `when` clause sees the trace so far, ending at its own frame | same (accidentally) |
| `abandoned` | an original displaced by a throwing `finally` keeps the *complete* trace its own search built | truncated at the displacing clause |

Two of these overturned decisions taken before measuring. The plan as first approved recorded
"unhandled exceptions run no cleanup" — `unhandled` shows they run all of it, which made the
change *smaller* than planned and turned a proposed test into one that would have asserted a
falsehood. And `filtertrace` showed that a filter's view of the trace is guest-observable
*during* the first pass, so projecting only at the pass's conclusion would have silently
regressed it from partial to `null`.

## The bounding invariant

**For a dispatch that concludes, the final frame list is unchanged. Only when its frames are
appended moves.**

The two append rules are carried over verbatim: the throw-site frame seeded by
`throwExceptionObject` (a `rethrow` seeds nothing), and one frame per caller-frame boundary
crossed, suppressed for a delegate-`Invoke` stub. Every pre-existing stack-trace test passes
unedited, which was the regression criterion.

The wording is narrower than "no trace ever changes", and deliberately: `abandoned` is a shape
where the *content* changes, because the displaced exception's dispatch no longer concludes at
the moment cleanup starts. That is a correction, and it has its own test.

## Shape

**First pass** (`firstPass`) walks the `ReturnState.JumpTo` chain and pops nothing. Per frame:
search for an accepting clause — `Catch` and `Filter` only, via `findAcceptingClause`, which
*cannot* return cleanup; then, if the frame is mid-filter, conclude `AbandonedAtFilter`; then, if
it has no caller, `NoHandler`; then, if its `ReturnState` carries a wrap flag, `WrappedAt`;
otherwise append the caller's frame and advance. A `Filter` suspends the walk in place, with
inner frames still live, parking the whole search on that frame's continuation.

Concluding projects the completed trace onto the exception object — for *every* outcome, not
only `CaughtAt`. That single move is what fixes the `trace`, `unhandled` and `abandoned` rows.

**Second pass** (`secondPass`) unwinds from the throw point to the frame the outcome names,
entering each covering cleanup clause and parking the remainder on its continuation. It carries
no itinerary: the first pass popped nothing, so the live frame chain *is* the itinerary and
cannot disagree with itself.

### Two decisions worth recording

**Cleanup selection is bounded by the target clause's entry offset.** In the target's frame a
cleanup clause runs only if its `try` covers the throw PC *and does not cover* the handler being
delivered to. Without that bound, a plain C# `try/catch/finally` runs its `finally` before its own
`catch` and again on the way out: Roslyn lowers that to `try { try { … } catch { … } } finally
{ … }`, so the outer `finally`'s `try` covers the throw point *and* the catch handler. The old
code was saved from this only by ranking catch and cleanup together and preferring catch at ties.
The predicate is the one `ExceptionHandling.finallyBlocksBetween` already implements for `leave`,
so it became a shared helper plus a `fault`-including sibling, `cleanupRegionsBetween`.

**"Is this frame mid-filter?" scans the continuation stack rather than reading its top.** Under
the old interleaved order a `finally` nested inside a filter body had already been entered and
popped by its `endfinally` before the question was asked, so the filter was always on top. The
first pass asks before running any cleanup, so a cleanup scope belonging to a superseded raise can
sit above the filter that still owns the frame; reading only the top would advance out of a frame
mid-filter, losing both the filter rejection and the parked outer search. Not reachable from C#
— a filter body containing its own `try`/`finally` needs hand-written IL — so it is
correct-by-construction with a loud failure rather than a tested path.

## What fell out

* **`CliException.MayConsumeForeignRaise` deleted.** It existed only to state an ordering the
  structure now has. Guest code can no longer run between a raise and its appends except inside a
  filter, which is exactly where CoreCLR would let a flag be consumed — so the read-and-reset
  becomes unconditional at every append, as `StackTraceInfo::AppendElement`'s is. Re-implementing
  "first append of this raise only" as a carried boolean would reinvent the field and leave #865's
  instance 4 wrong. `ForeignRaiseFlagNotStolenByCleanup.cs` is un-parked.
* **`findExceptionHandler` and its `isCleanup` flag deleted.** A classifier every caller ignored
  is the defect; `findAcceptingClause` cannot express the wrong answer.
* **`ExceptionDispatchResult.HandlerFound` renamed `Dispatched`.** Three of its four destinations
  are not handlers, and under two passes a `filter` can be entered before anything is known about
  whether the exception is caught at all.

## Verification

* 2655 pre-existing tests green, none edited for behaviour (`TestFaultHandlers` and
  `TestSchedulerYieldDebt` were edited only where they named deleted API).
* Five new differential cases plus the un-parked one, each measured on real .NET first.
* Each new case was run against the pre-restructure dispatcher, by reverting only the
  implementation files to `origin/main` and keeping the tests, and each failed at its intended
  assertion: `FilterRunsBeforeCalleeFinally` exit 2 (the `finally` ran first),
  `StackTraceInsideFinallyIsComplete` / `AbandonedOriginalHasCompleteTrace` /
  `UnhandledExceptionRunsFinally` exit 5 (the handler-owning frame missing from the trace),
  `ForeignRaiseFlagNotStolenByCleanup` exit 5 (the flag stolen by cleanup).
  `FilterSeesTraceSoFar` passed there, which is its documented role: it guards a behaviour the
  restructure had to avoid losing rather than one it introduced.
* The bounded-cleanup rule was mutation-checked separately by removing the boundary: four
  pre-existing tests fail, `ComplexTryCatch.cs` among them.

## Not in scope

* `CctorFailureTraceNamesTargetMethod.cs` — cause is `loadClass` running before the callee frame
  is pushed. Different bug; stays parked.
* `ActivatorCctorTypeInitializationTrace.cs` — needs a decision about which frame a synthesised
  wrapper's trace should *start* from. `WrappedAt` preserves the old answer deliberately, by
  seeding the next search at the caller's call site exactly where the old crossing append landed.
* `StackTrace.ToString` display policy (`docs/divergences.md`) — unrelated cause.
