# Plan: share the machine state up to the first fork point

Status: **all three PRs implemented.** §3.7 and §5.7 record where implementation contradicted the
plan. The remaining work is stage 4 — rewiring `TestConcurrencyBugs` / `TestRaces` to fan out from
a shared fork point — which this plan describes but does not yet cover in detail.

Goal: when sweeping many PCT seeds over one guest, compute the single-threaded prefix *once* and
fan every seed out from it.

Secondary goal (not required now, must not be designed out): re-seed from *any* fork point, so a
later harness can search the schedule space as a tree rather than as a flat list of whole-run
seeds.

Reviewed by a second agent (Fable). Its memo changed four things — the shared-predicate
formulation, the choice between `candidates` and `runnableThreads`, the module's home, and the
shape of the load-bearing test — and found one hazard I had missed (§4.3). §3.5 and §5 record the
disagreements and their resolution.

## 1. What was measured, not assumed

Temporary scaffolding (`WoofWare.PawPrint.Test/TestForkPointMeasurement.fs`, since removed)
prepared four `sourcesConcurrencyBugs` guests under `PctSeed = Some 0UL`, counted
`Kernel.StepCounter` at three points, and timed them.

| guest | steps at end of `prepare` | steps at first fork | total steps | ms to fork | ms total |
|---|---|---|---|---|---|
| SimultaneousCounter.cs | 3300 | 3544 | 3835 | 2445 | 2469 |
| LostUpdate.cs | 3300 | 3539 | 3785 | 218 | 224 |
| InvertedMonitorDeadlock.cs | 3307 | 3551 | 3902 | 203 | 210 |
| QueueIsNotThreadSafe.cs | 3314 | 3558 | 4761 | 136 | 161 |

(The first row's 2.4 s is JIT warmup of the test host, not guest work.)

* **74–94% of every run's simulated instructions are in the shared prefix**, and ~90% of the wall
  clock. `TestConcurrencyBugs` sweeps 4096 seeds per scenario at ~0.2 s each ≈ 14 minutes;
  roughly 12 of those minutes re-execute an identical prefix.
* **The prefix is overwhelmingly `Program.prepare`** — 3300 of ~3550 steps are startup, only ~240
  are the prologue of `Main` before the first `Thread.Start`. So most of the win is available
  from "share the prepared program" alone; but that simpler feature has *exactly* the same
  correctness obstacle (§2), and the fork-point framing is the one that generalises.

## 2. Why this is not just "hold on to the `IlMachineState`"

`IlMachineState` is an immutable F# record, so sharing a prefix costs nothing in memory and needs
no serialisation. The obstacle is entirely the **scheduler's own state**.

*Forced-prefix claim*: while the scheduling decision is forced, every policy makes the same
choice, so the guest-visible evolution of the prefix does not depend on the seed.

True (audited in §4), but PCT's *policy* state is nonetheless seed-dependent over the prefix,
because `Scheduler.chooseNext` draws whether or not the draw can change anything:

* one `NonCryptoRandom.nextDouble` per tick, unconditionally (`Scheduler.fs:267`, defended at
  `:262-266`);
* a lazy priority sample per newly-seen Runnable thread (`Scheduler.fs:237` →
  `SchedulerState.fs:88-95`) — so even the solo entry thread costs a draw at tick 1;
* a second draw on demotion (`Scheduler.fs:284`);
* one per `VoluntaryYield` in `chargeYieldDebt` (`Scheduler.fs:687`), again unconditional under
  `Pct` and again defended at `:676-679`.

After the ~3300-step startup, `PctState.Rng` has advanced ~3300+ times and `Priorities` holds a
sampled value for the entry thread. **Snapshotting the machine state and installing
`PctState.ofSeed s` would not reproduce the run seed `s` would have had from scratch** — it would
silently sweep a different schedule space from the one the tests were calibrated against, with
nothing to point at when it went wrong.

## 3. The architectural decision

### 3.1 Option A — keep the RNG discipline; record and replay the prefix's scheduler trace

Snapshot the machine state once, plus a per-tick recording of everything the policy reads (the
runnable-id list, the `ContextSwitchPrior` weight of the imminent op, whether the step yielded).
Per seed, replay only that fold — splitmix64 arithmetic, nanoseconds a tick — to derive `PctState`
at the fork.

* Preserves the existing seed→schedule mapping bit-for-bit; no existing test moves.
* Costs a **second implementation of `chooseNext`'s RNG consumption** that can drift from the
  real one silently: the "two versions of the truth" the project's principles warn about, with
  nothing but a differential test holding it honest. Avoiding the duplication means restructuring
  `chooseNext` to take a projection instead of `IlMachineState` — a bigger change than B.
* O(prefix) memory and O(prefix) work per seed.
* Does not generalise: at a later fork point the trace is itself seed-dependent, so every branch
  of a schedule tree needs its own recording.

### 3.2 Option B — PCT consumes randomness only where it can affect a decision

Rule: **the policy draws iff the imminent decision is contended.** Equivalently, `Scheduling`
changes only at genuine choice points.

* Over any forced prefix, `PctState` is invariantly `PctState.ofSeed s`, so the snapshot is an
  `IlMachineState` whose `Scheduling` field you overwrite.
* Better, the prefix can be run under `RoundRobin` — zero randomness by contract
  (`Scheduler.fs:670-674`) — and installing `Pct (ofSeed s)` at the fork gives a run
  **bit-identical** to a from-scratch `PctSeed = Some s` run. §4.4 proves this is sound.
* It gives the secondary goal an honest general contract: *resuming a fork point with scheduler
  state σ produces exactly the run that would have happened had the state been σ there.*
  First-fork exactness is a corollary rather than a special case.
* It makes `IlMachineState.withPctSeed`'s docstring precondition ("before any `chooseNext` call
  has observed threads", `IlMachineState.fs:277-280`) checkable rather than aspirational.
* Cost 1: the seed→schedule mapping moves for every existing run. One-time rebaseline; §6.
* Cost 2: the always-burn behaviour is defended in comments that must be rebutted, not overridden.
  See §3.4 — the rebuttal is stronger than I first thought.
* Cost 3: does skipping demotion during solo stretches degrade exploration? See §3.6.

### 3.3 Option C — serialise the snapshot to share it across processes

Rejected, and worth recording so it is not re-litigated. `IlMachineState` carries
`ILogger`/`ILoggerFactory` and `MetadataReader`-backed `DumpedAssembly` values
(`IlMachineStateModel.fs:13-20,54`), so a serialisation format is a large separate project — and
it is not where the cost is: the sweep is CPU-bound in one process. If a distributed sweep is
ever wanted, the thing to ship over the wire is `(image, config, seed)` plus a *decision trace*,
not a heap dump. Nobody should assume `IlMachineState` will ever serialise.

### 3.4 Option D — counter-based stateless draws

Fable raised a genuinely different third option: replace the threaded splitmix64 with stateless
`hash(seed, StepCounter, purpose)` draws, as the spurious-wakeup machinery already does
(`LowLevelMonitor.fs:490-501`). That makes the RNG position reconstructable at any tick, which is
attractive for fork-at-k. But it does *not* remove the prefix-dependence of `Priorities` — the
demotion history still depends on the weight sequence — so it does not subsume B, and it is a
larger algorithmic change. Deferred; B does not foreclose it.

### 3.5 Recommendation, and how Fable sharpened it

**Option B**, in Fable's sharpened form:

> Define the contention predicate **once**, in `Scheduler`, and have *both* the draw sites and the
> fork detector consume that one definition. Route every scheduler draw through a single private
> helper that takes the witnessing thread list and `failwith`s if it has fewer than two elements.

That last part is the piece Option A structurally cannot have: it turns "no draws while forced"
from a property maintained by discipline into one asserted by the machine, at the only place it
could ever be violated. It is also what stops the detector and the policy drifting apart — a
detector with its own re-derived predicate could disagree with the policy, and the disagreement
would surface as a sweep silently exploring the wrong space.

**Rebutting the always-burn comments honestly.** My first draft argued "predictable consumption
rate buys nothing observable". Fable's rebuttal is better and is the one to use: *the invariant
those comments defend is already false*. `Scheduler.fs:262-266` claims consumption of "one
`nextDouble` per `chooseNext` call", but actual consumption is 1 + (newly-seen threads) +
(demotions) + (yields), and the demotion count itself depends on the `ContextSwitchPrior` weights
of the ops encountered. Likewise `:676-679` claims independence from "how many threads happened to
be Runnable", but consumption already depends on the runnable history through `ensurePriorityFor`.
Option B does not discard a true invariant for convenience; it replaces a half-true informal one
with a fully-true machine-checked one.

### 3.6 Cost 3 was measured, not argued

Two edits to `Scheduler.fs` (skip the Pct branch when the decision is forced; skip the honour
draw when `others` is empty) were applied on a scratch basis and the mapping-sensitive suites
re-run:

```
TestRaces | TestConcurrencyBugs | TestSchedulerPct        →  25/25 passed (1.0 min)
  ReadWriteRace under PCT covers every legal outcome         passed
  NewobjCctorRace under PCT covers every legal outcome       passed
  PCT exhibits the bad interleaving(SimultaneousCounter)     passed
  ... JustABoolNotAMutex, InvertedMonitorDeadlock,
      LostUpdate, TwoCountersSeparated, QueueIsNotThreadSafe all passed

TestSchedulerYieldDebt | ...YieldFairness | ...SleepFairness
                       | ...VoluntaryYield                  →  20 passed, 2 failed
  Failed: Pct burns exactly one draw per yield, regardless of the Runnable set
  Failed: yielding spinners do not starve the worker under Pct
          305676 steps across 20 seeds against a 300000 budget (+1.9%)
```

So every 4096-seed sweep still finds its bug, both exact-coverage tests still enumerate exactly
the legal outcome set, and the only fairness regression is a 1.9% threshold overshoot — fairness
did not *degrade*, the budget was tuned to the old mapping's luck. The two failures are the
predicted ones, not surprises (§6). The prototype was reverted; this is evidence for the plan, not
a start on it.

Fable's theoretical worry is nonetheless real and worth recording: under always-burn, a thread
emerging from a long solo stretch arrives at the next contention having probably been resampled;
under plain B it arrives still holding the priority that won it the last contention, so it tends
to keep winning. The measurement above says this does not bite on any guest we have. **If a future
guest shows it biting, there is an exact fix that stays snapshot-compatible** (call it B′):
accumulate the survival product `∏(1 − wᵢ·P_BASE)` over the forced stretch as plain float
arithmetic in `PctState` — no draws — and at the next contended tick draw once against
`1 − product` to decide whether the incumbent gets one resample. Because repeated uniform
resampling is memoryless, that reproduces the always-burn distribution of the priority vector at
every contention *exactly*, and the accumulator over a forced prefix is seed-independent so it
still lives happily in the snapshot. Do not build B′ speculatively.

### 3.7 What implementation contradicted

Two predictions in this plan turned out to be wrong, both in the direction of the change being
cheaper than forecast. Recorded rather than quietly edited away, because the reasons are
informative.

**The fairness thresholds did not need re-baselining.** §3.6's prototype gated the draws on
`|candidates| = 1`; the shipped version gates on `|runnable| = 1` (§4.2's conclusion, which was
reached after the prototype had already been measured). Those two differ in exactly one regime —
two Runnable threads of which one is held out by a yield debt — and that regime is precisely what
`TestSchedulerYieldFairness` exercises. The shipped predicate keeps drawing there, and the
yielding-spinner budget lands inside its existing 300000 with no adjustment; the whole fairness
and voluntary-yield battery passes untouched. So the +1.9% overshoot was an artefact of the
prototype's over-broad gate, not of Option B, and it is independent evidence for choosing
`runnable` over `candidates`: the coupling argument and the measurement agree.

**P3 as specified is not writable.** The plan called for a unit test that the draw guard fails
loudly given a fewer-than-two-thread witness. The guard is private to `Scheduler` and the test
project has no `InternalsVisibleTo`, so there is nothing to call. Rather than widen the surface
for a test, the guard was made *structural*: `Contention.Contended` carries its first two members
as separate fields, so "at least two threads are Runnable" is a fact about the type, and
`chooseNext` puts the policy match *inside* that branch so the uncontended arms have no
`PctState` in scope to draw from at all. An edit that wants to draw on a forced decision has to
restructure the function to reach the state. The behavioural half of P3 is covered by P2, which
is what would actually fail — confirmed by mutation, below.

**Mutation results** (each applied to the shipped implementation, then reverted):

| mutant | killed by |
|---|---|
| draw a `nextDouble` in the `Forced` arm of `chooseNext` | `Pct consumes no randomness when the decision is forced`; `a run that never forks consumes no randomness at all` |
| toss the honour coin when `others` is empty | `Pct draws on a yield iff a peer is Runnable` |
| `classify` never returns `Contended` | `a run that forks does consume randomness` (plus five yield-debt tests, since that mutant also disables debt filtering — a coarser mutant than intended, but the target test fired) |

## 4. Auditing the forced-prefix claim

### 4.1 Channels from policy state to machine state

The only reads of `state.Scheduling` outside `Scheduler.fs`/`SchedulerState.fs` are the
`RoundRobin` initialisation (`IlMachineThreadState.fs:303`) and `withPctSeed`
(`IlMachineState.fs:287-290`). No `Native/` or `ExternImplementations/` handler touches it. So
influence flows only through `chooseNext`, `chargeYieldDebt` and `onThreadTerminated`:

* **Thread choice** — with one candidate both policies return it. Forced.
* **`Thread.Yield()`'s guest-visible bool** — the one real channel. `chargeYieldDebt` returns
  `(state, false)` whenever `others` is empty *regardless of the honour draw* (`Scheduler.fs:700`),
  so the guest observes `false` under every policy while the yielder is alone; only the RNG
  position diverges. `onStepOutcome` (`:759-786`) then pushes `0` identically.
* **`Thread.Sleep(0)`**, the second `VoluntaryYield` producer, returns void — nothing
  guest-visible, but it does burn the honour draw today.
* **Spurious wakeups** — keyed on the strategy's *own* seed and `StepCounter`
  (`EmulatedKernel.fs:65-87`), decoupled from `PctSeed`; and vacuous pre-fork anyway, since a
  thread can only sit in a `WaitQueue` if it has run.
* **Deadline firing and the jump-to-deadline loop** — deterministic folds; the loop deliberately
  uses `hasAnyRunnable` rather than `chooseNext` to avoid perturbing the RNG
  (`Program.fs:519-525`).
* **Signal dispatch** — deterministic, no RNG. It can *create* a fork point but does not depend on
  the policy.
* **The kernel PRNG** (guest `Random`, `Guid`, …) is a separate stream in `EmulatedKernel`,
  identical over an identical prefix.

### 4.2 The predicate: `runnableThreads`, not `candidates`

I first proposed `|candidates| ≥ 2`, on the grounds that `candidates` is what the policy chooses
among. Fable argued for `|runnable| ≥ 2` and is right, for a reason that is about coupling rather
than correctness: **the fork detector must use the same predicate that gates the draws**, and the
draws are gated by contention over the *unfiltered* runnable set — `ensurePriorityFor` samples
over `runnableThreads` while the argmax runs over `candidates` (`Scheduler.fs:216-222,237`).

For the first fork the two coincide anyway (§4.4 shows no yield debt can exist pre-fork, so
`candidates` is the identity there). The distinction only bites for the fork-at-k future, and
there `|runnable| ≥ 2` is also the better answer: a tick with two Runnable threads but one
eligible candidate still *consumes draws* whose effect is felt at later choices, so it is
genuinely part of the seed's identity and belongs in the tree.

### 4.3 The hazard Fable found: wake-then-charge

`onStepOutcome` wakes class-init waiters *before* charging the yield debt (`Scheduler.fs:762-764`,
"Wake first, then charge"). So `chargeYieldDebt` evaluates `others` against the *post-wake*
runnable set: a tick that was uncontended when `chooseNext` looked at it can be contended by the
time the honour draw is reached. On such a tick, gating the honour draw on "`others` is empty"
does **not** coincide with gating on "this tick's decision was forced" — the policy would draw
after the point at which the snapshot was deemed forced, and the guest-visible yield bool would
become seed-dependent.

This is provably unreachable before the *first* fork: a thread parked `BlockedOnClassInit` must
have executed a step to get there, and a cctor can only be `InProgress` on another thread, so two
threads have already run and contention already occurred. But that is a chain of facts about wake
paths, not a structural property, and a future change could break it silently.

**Therefore the fork-runner must `failwith` if a prefix step reports `VoluntaryYield` and the
post-step state is contended.** Crash rather than emit a snapshot that does not commute.

### 4.4 Why the prefix may be run under `RoundRobin`

`chargeYieldDebt` genuinely differs between the policies whenever `others` is non-empty
(`Scheduler.fs:683-698`: `RoundRobin` honours unconditionally, `Pct` draws). So running the prefix
under `RoundRobin` is only sound given:

Lemma: **during a forced prefix, at most one thread is ever Runnable.** Proof: consider the first
tick at which two threads are Runnable. A yield debt is only charged when `others` is non-empty,
i.e. at a tick with ≥ 2 Runnable threads, so no debt has been charged before this tick;
`candidates` therefore filters nothing and has two elements; so that tick is contended and the
prefix has already ended. ∎

Corollary: `others` is always empty during a forced prefix, so the two policies cannot diverge,
and (with §4.2) `candidates ≡ runnableThreads` there. The fork-runner should assert the lemma
rather than rely on it.

## 5. Proposed shape

### 5.1 Home

`Program.fs`. My first draft put this in a new file after `Program.fs`, but `Startup`'s
representation is private to the `Program` module (`Program.fs:115-122`) and a startup-phase fork
must be snapshottable (§5.2), so a sibling file would mean widening `Startup`'s surface for one
consumer. Fable's call; adopt it.

Note also that **`SchedulingDecision` from my first draft is dropped**: with one shared
`isContended` the detector reads the scheduler's own predicate directly, and a new public DU would
be speculative generality.

### 5.2 Snapshots are tick boundaries, and startup can fork

Contention can be created *inside* a tick, before `chooseNext` sees it: `trySpawnHandler` flipping
the dispatcher Runnable (`Program.fs:488-491`), a deadline fire (`:477-480`), a spurious wake
(`:437-444`), or the jump-to-deadline loop (`:519-547`). So the probe must run *after* that
pipeline — but the snapshot must be the **inter-tick** value from before it, because exposing a
mid-tick state would create a resumable value that the ordinary driver would double-run the
pipeline on, bumping `StepCounter` twice and shifting the spurious-wakeup schedule.

So: extract `Program.fs:437-547` into a private `preChoice : PreparedProgram -> PreparedProgram`
used both by `stepPrepared` and by the probe; snapshot the inter-tick value; resume re-runs the
pipeline, which §4.1 establishes is deterministic and RNG-free. The probe's extra execution of the
pipeline during the prefix is a few empty-map folds per tick — noise beside `executeOneStep`, and
paid once.

A `.cctor` may call `Thread.Start` (`StartupStepOutcome.WorkerTerminated` exists because this is
real), so the runner must drive `beginStartup`/`stepStartup` with the same probe and the snapshot
must be able to hold a mid-startup `Startup`.

### 5.3 API

```fsharp
[<RequireQualifiedAccess>]
module ScheduleFork =
    /// Witness that the wrapped state sits at an inter-tick boundary, was produced under
    /// RoundRobin (no randomness consumed), and that its next decision is contended.
    /// Constructing one is the only proof, hence the private representation.
    type Snapshot =
        private
        | DuringStartup of Startup
        | DuringMain of PreparedProgram

    type PrefixOutcome =
        | ForkPoint of Snapshot
        /// The guest ran to completion with no contended decision, so this outcome is the
        /// outcome of EVERY seed: a 4096-seed sweep is answered by one run.
        | NeverForked of RunOutcome
        | DeadlockedBeforeFork of stuckThreads : string

    val runToFork : ILoggerFactory -> string option -> Stream -> GuestConfig -> PrefixOutcome

    /// Install a policy and hand back ordinary driver state; `None` resumes RoundRobin.
    /// Rebinds the state's Logger/LoggerFactory to the factory given (see §5.5).
    val resume : ILoggerFactory -> uint64 option -> Snapshot -> Resumed
```

`resume` is deliberately no more than *reseed + logger rebind + unwrap*: afterwards the caller
drives with the existing `stepStartup`/`stepPrepared`/`pumpPrepared`, so the sweep harness's
`runOne` loop (`TestConcurrencyBugs.fs:213-221`) barely changes. `Snapshot`'s opacity guards
exactly one thing — the legality of overwriting `Scheduling` — and nothing else.

`NeverForked` is not a consolation prize; it is the freebie made explicit, and it is sound for
exactly the forced-prefix reason.

### 5.4 The "host config minus the seed" problem

`HostConfig`'s docstring says "Every field here is part of a run's replay contract"
(`HostConfig.fs:9-10`). The feature refines that: the seed belongs to the *continuation's* replay
contract, not the prefix's. `runToFork` must not be handed one.

1. take `HostConfig` and ignore `PctSeed` — rejected, it converts a caller's type error into a
   wrong experiment;
2. take `HostConfig` and `failwith` unless `PctSeed = None` — loud, zero churn, but validation
   where parsing is available;
3. **split**: `HostConfig = { Guest : GuestConfig ; PctSeed : uint64 option }`, `runToFork`
   taking `GuestConfig`.

**Decided: 3.** (This plan first called the inner record `PrefixConfig`. That name was wrong and
was changed during implementation: the record does not configure the *prefix*, it configures the
whole run, and it is only the seed that the prefix and its continuations differ in. Naming it
after one downstream consumer would also have exported a concept — the fork prefix — into a file
whose reader has no context for it. `GuestConfig` says what it is: everything describing the
simulated process, as against which of its schedules we are exploring.) `WoofWare.PawPrint` is `IsPackable=true` with `PackageId=WoofWare.PawPrint`
(`WoofWare.PawPrint.fsproj:6-7`), so `HostConfig` is a published surface and this is a breaking
change for external consumers — but the package is prerelease and the maintainer has confirmed
breaking it outright is fine, so the compile-time-correct factoring wins over any variant that
preserves the flat record. Do not soften it into `WithoutSeed`/`WithSeed` projections beside the
old shape: two records that must agree is precisely the drift risk the split exists to remove.

In-repo churn is 37 `HostConfig.Default` construction sites (9 setting `PctSeed`, 8 `Kernel`, 3
each `Argv`/`AppContext`), all mechanical — the compiler finds every one. `HostConfig.Default`
should keep taking the runtime dirs and returning a whole `HostConfig`, so the common
`{ HostConfig.Default dirs with PctSeed = ... }` shape survives; only sites that set a
`GuestConfig` field move a level down.

### 5.5 Logging and parallel fan-out

* **The snapshot pins the logger.** `IlMachineState` carries `Logger`/`LoggerFactory`
  (`IlMachineStateModel.fs:13-20`), documented as a sink nothing behavioural may depend on — so
  rebinding at resume is legitimate and `resume` should do it, otherwise every seed's log loses
  its `pct_seed`/`source_file` properties. Loggers captured in `Startup`'s closures
  (`Program.fs:961`) stay the prefix's; document that the prefix factory must outlive every
  resume. The sweeps' per-seed `use _loggerFactoryResource` (`TestConcurrencyBugs.fs:188`) means
  the prefix factory must be scoped to the whole sweep; disposal-after-share fails with
  `ObjectDisposedException`, which is loud enough.
* **Sharing across parallel resumes is not a new exposure class.** The sweeps use
  `Array.Parallel` (`TestConcurrencyBugs.fs:359`), and everything reachable from the snapshot is
  persistent F# data or `ImmutableDictionary`; `DumpedAssembly` values are *already* shared across
  parallel NUnit tests today through the process-wide `fileCache`
  (`WoofWare.PawPrint.Domain/Assembly.fs:1001`, a `ConcurrentDictionary<_, Lazy<DumpedAssembly>>`).
  The residual question is `MetadataReader`'s concurrent-read safety, and the answer should be a
  test (P5) rather than an assurance.

### 5.6 Fork-at-k

Nothing above needs rework. A mid-run snapshot's witness is strictly weaker — no
"no-randomness-consumed" claim, because re-seeding at k deliberately discards the old priorities
via `ofSeed`, which is the correct "re-randomise the future" semantics for tree search — so a
later `tryForkHere : PreparedProgram -> Snapshot option` built on the same probe slots in, and
`resume` is unchanged. Record now, build later: a mid-run snapshot's *cross-process* identity is
the decision trace that produced it, never the state.

### 5.7 What stage 3 shipped, and where it departed from §5.1–§5.3

* **No `ScheduleFork` module.** §5.1 wanted one inside `Program.fs`, to reach `Startup`'s private
  representation. A sibling module cannot see another module's `private` bindings, and the fork
  runner needs `advanceToDecision` and `stepDecided`, so the API lives directly in `Program`
  alongside `PreparedProgram`, `Startup` and `ProgramStartResult` — which is where the file's
  existing conventions put this kind of type anyway. Names: `Program.ForkSnapshot`,
  `Program.PrefixOutcome`, `runToFirstFork`, `runToNextFork`, `resumeFork`.
* **`isContended` became `tryContenders : IlMachineState -> ThreadId list option`.** PR 1's
  boolean forced the fork runner to derive the contending-thread list separately in order to
  record it, which is a second derivation of the predicate — exactly what §4.2 argues against.
  Returning the witness from the same evaluation removes the possibility of disagreement.
* **A startup fork is refused, not snapshotted.** §5.2 wanted `ForkSnapshot` to hold a mid-startup
  `Startup` from day one. It is detected — `runToFirstFork` probes startup with the same predicate
  — but reported as `PrefixOutcome.ForkedDuringStartup` rather than snapshotted, because resuming
  one means `resumeFork` returning a two-shape value and every caller driving both phases, for a
  case no guest in this repository exercises. Refusing loudly is correct-by-detection and the
  extension point is one DU arm. `ForkInCctor.cs` pins the refusal.
* `NeverForked`'s state carries the `RoundRobin` the prefix ran under, so its `Scheduling` differs
  from a from-scratch seeded run's. Nothing guest-visible depends on it; the DU case documents it.
* **A snapshot's `Contenders` are Runnable at the decision point, not necessarily in its `State`.**
  Fable caught the first draft of `ForkSnapshot`'s docstring claiming otherwise — falsified by
  this very PR's constructed preamble test, where a contender is `BlockedOnSleep` in the snapshot
  and woken by the tick's preamble. Organic first forks never show the shape, because there the
  second thread arrives via `Thread.Start`; `runToNextFork` from mid-run does. The docstring now
  says so, and the organic-versus-constructed distinction is asserted in both directions.

## 6. What has to be proved, and how

**P1 — the commuting square (load-bearing).** For each guest in a corpus and each seed, the
from-scratch `Pct s` run ≡ `runToFork` + `resume s`, compared on the **full post-fork decision
trace** (per tick: chosen thread and `WhatWeDid` discriminator) *and* the final classified
outcome. My first draft compared only endpoint state; Fable is right that trace equality is what
makes a one-tick boundary error or a straddled yield draw undeniable rather than usually-invisible.
Seeds must be generated across the full `uint64` range explicitly — FsCheck's default integer
generator is size-bounded and would test only tiny seeds. The corpus must include, by
construction: a guest whose solo prefix contains `Thread.Yield()` and `Thread.Sleep(0)`; a guest
that sleeps solo (exercising the deadline jump inside the prefix); a guest that forks *in a
cctor*; and a guest that never forks.

**P2 — prefix policy-independence sentinel.** Run the prefix to the fork under `RoundRobin` and
under `Pct s` for random `s`: the prefix step traces must be identical, and at the fork the Pct
run's `Scheduling` must be exactly `Pct (ofSeed s)` — `Rng` untouched, `Priorities` empty. This
catches both silent-drift modes: a draw reintroduced at a forced point, and machine evolution
acquiring a policy dependence outside the scheduler.

**P3 — the witness assertion is alive.** Unit-test the single draw helper: a witness list of fewer
than two threads must `failwith`. This is the guard that outlives every future refactor.

**P4 — degenerate resume.** `resume None` ≡ from-scratch `RoundRobin`, which also checks that
`LastRan` survived the snapshot.

**P5 — parallel ≡ serial fan-out.** Resume k seeds from one snapshot concurrently and serially;
the summaries must match. This is the machine check on §5.5's sharing claim, `MetadataReader`
included.

**P6 — `NeverForked` is not vacuous.** A guest calling `Thread.Start` must produce `ForkPoint`;
`NeverForked` must be reachable and must agree with `Program.run` under several seeds. Without
this, an implementation that reported `NeverForked` unconditionally would pass P1 trivially on the
single-threaded corpus.

**P7 — exploration is not degraded.** The existing sweeps must still find their bugs; record, per
scenario, the first seed that succeeds before and after, so the change is measured rather than
asserted. §3.6 is the first data point.

Per the project's mutation habit: break the implementation once per claimed mode — add a draw in
the solo branch; snapshot one tick late; skip the pipeline on resume; report `NeverForked`
unconditionally — and record in the PR which property killed which mutant.

### 6.1 The mutation that survived, and what it exposed

Stage 3's mutants: snapshot one tick late (killed by P1 on all four guests), resume with
`ofSeed (s + 1)` (killed by P1), drop the startup probe (killed by P6's `ForkInCctor` case) — and
**probe the inter-tick state instead of the post-preamble one, which survived the whole suite.**

It survived for a reason worth writing down. For a *first* fork the second Runnable thread always
arrives via the guest's own `Thread.Start`, which is a retired instruction and therefore already
visible on the inter-tick state; so the two probe points agree on every guest one can write.
The preamble can only create the first contention through the kernel's signal dispatcher, which
is the one thread that becomes Runnable without a guest instruction. Mid-run, a deadline expiry
does it easily — which is exactly what `runToNextFork` will meet once a schedule-tree search
exists.

So the guest corpus cannot kill it, and the plan's instinct to reach for a guest ("a guest that
sleeps solo") was wrong: `ForkAfterSoloSleep.cs` exercises the deadline *jump*, but with only one
thread alive, so the wake it produces is not contended. The test that kills it is constructed
instead — take a real fork point, park one of its two contenders on an already-expired sleep, and
require `runToNextFork` to report the fork at the very next tick, with no instruction retired in
between. That is the value of running the mutants rather than assuming coverage: the gap was in
the corpus, not in the code.

A second review pass (Fable, on the implementation this time) found a further pair of survivors:
garbling either `DeadlockedBeforeFork` arm, in the main phase or in startup. Neither was covered,
because a whole-program `Program.run` turns deadlock into a host `failwith` and so cannot reach
the case at all. Two guests that wedge while single-threaded — one in `Main`, one in a `.cctor`,
since `runToFirstFork` drives the two phases through different loops — now kill both.

One mutant survives deliberately: deleting the `checkYieldDidNotStraddle` calls. The hazard it
guards (§4.3) is unreachable today, so no test can distinguish the guard's presence. It is kept
as correct-by-detection for a future change to the wake paths, and its exhaustive `match` over
`WhatWeDid` is what will make a new outcome variant a compile error rather than a silent hole.

### The one test that encodes the old contract and must be inverted

`TestSchedulerYieldDebt.fs`, ``Pct burns exactly one draw per yield, regardless of the Runnable
set``, asserts that a yield with one Runnable and one blocked thread advances the RNG exactly as
much as a yield with two Runnable threads. That is the always-burn contract asserted directly, and
Option B inverts it. Changing an existing test deserves the corresponding scepticism, so the PR
must argue it: the old assertion pinned *consumption rate*, which §3.4 shows was never actually
invariant; the new one pins *"policy state changes only at choice points"*, which the sweep's
correctness depends on. The replacement must stay two-sided — assert that the contended case still
burns its draw — or a regression to never drawing would pass.

### Tests that were expected to need re-baselining, and did not

`TestSchedulerYieldFairness` / `TestSchedulerSleepFairness` aggregate step counts over fixed seed
sets against hand-tuned thresholds whose own comments record that per-seed numbers are
non-monotone, and `TestRaces` asserts *exact* outcome coverage over a fixed seed range. All were
expected to move. In the event none did — see §3.7 for why — so PR 1 changed no threshold and no
seed range. That is a happier outcome than planned, but it also means the aggregate claims were
re-validated by observation rather than re-derived, which is worth remembering if a future
`P_BASE`-scale change moves them for real.

## 7. Staging

Fable proposed two PRs, I proposed four; the difference was mostly the now-dropped
`SchedulingDecision` refactor. Three:

1. **Option B — done.** The shared `Contention`/`classify`/`isContended`, the structural draw
   witness, no draws on forced decisions. Adds P2 (both the frameless property over generated
   thread tables and the whole-run "a run that never forks consumes no randomness at all", which
   is the first time the claim is asserted through live frames), P6's two-sided half, and the
   inverted yield-draw test carrying the §3.4 argument. No threshold moved (§3.7). This is the
   seed-remapping commit, isolated so a bisect lands on it.
2. **`ScheduleFork` + the config split.** §5.2–§5.4 plus P1, P4, P5, P6. No consumer changes
   beyond the mechanical `HostConfig` re-shaping. Worth splitting the `HostConfig` re-shaping into
   its own commit *within* the PR: it touches 37 call sites and would otherwise bury the new
   logic.
3. **`ForkSnapshot` + the fork runner — done.** §5.2, §5.7 and properties P1, P4, P5, P6, plus the
   constructed preamble-contention test of §6.1.
4. **Rewire the sweeps.** `TestConcurrencyBugs` and `TestRaces` fan out from a shared fork point;
   report the measured speedup against §1. Still to do, and the only stage with a user-visible
   payoff — everything before it is the machinery that makes the payoff correct.

PR 1 is the one to review hardest.

## 8. Open questions

* Does anything want the prefix's `StepEffect` stream? A driver streaming guest output per step
  (`Program.fs:64-84`) sees only post-fork effects when resuming; the final state's `OutputLog` is
  still complete. Document at `resume`.
* Should `NeverForked` worry that a guest might fork only under some other schedule? It cannot: if
  no tick was ever contended, no policy had a choice anywhere. Worth an explicit comment at the
  definition, because it reads like an approximation and is not one.
