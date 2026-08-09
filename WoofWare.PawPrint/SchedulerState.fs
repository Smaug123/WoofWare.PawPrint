namespace WoofWare.PawPrint

/// State owned by the schedule-fuzzing PCT (Probabilistic Concurrency Testing)
/// policy: a deterministic splitmix64 RNG state plus the current
/// per-thread priority assignment. Burckhardt et al.'s original PCT
/// algorithm samples a fixed number of priority-change points from a known
/// total step count; PawPrint's runs are open-ended, so there is no such
/// count to sample from and demotion is instead a per-step Bernoulli at a
/// flat rate. That costs the paper's probabilistic bug-finding bound and
/// keeps everything else, demotion-below-everyone included.
///
/// Each thread is sampled uniformly into `[0, 1)` on the first
/// `chooseNext` call that sees it Runnable. Sampling lazily — rather
/// than via an eager hook at every thread-creation site (entry thread,
/// `Thread.StartInternal`, signal dispatcher dispatch) — keeps the
/// sampling rule in one place, so a future thread-creation path cannot
/// silently desynchronise PCT determinism by forgetting to call into
/// the scheduler. The cost is that `Priorities` is sparse rather than
/// dense in the alive-thread set; the `chooseNext` Pct branch always
/// runs `ensurePriorityFor` before consulting `Priorities`.
///
/// Demotion drops the running thread below every other thread; the next
/// chosen thread is whichever Runnable thread has the maximum priority.
/// `Map<ThreadId, double>` keeps this purely functional so that
/// schedule replay is bit-exact.
type PctState =
    {
        /// splitmix64 state, advanced via `NonCryptoRandom.step` /
        /// `NonCryptoRandom.nextDouble` / `NonCryptoRandom.nextInt32Below`
        /// whenever the scheduler needs a uniform draw. Threading the
        /// state through every decision is what keeps the run reproducible
        /// from a seed.
        Rng : uint64
        /// Priority assigned to each thread the scheduler has observed
        /// Runnable. Entries are inserted lazily by `chooseNext`'s
        /// `ensurePriorityFor` pass, removed on termination via
        /// `Scheduler.onThreadTerminated`, and lowered on demotion by
        /// `demoteToBottom`. Threads not present here are not currently
        /// Runnable from the scheduler's point of view (or have never
        /// been seen Runnable yet).
        ///
        /// Initial priorities are uniform on `[0, 1)`; demoted ones are the strictly negative
        /// values `DemotionFloor` hands out, so a demoted thread sorts below every thread that
        /// has not been demoted since, and a newly-created thread sorts above all of them —
        /// which is how Burckhardt et al.'s PCT treats late arrivals.
        Priorities : Map<ThreadId, double>
        /// The next priority `demoteToBottom` will assign, decreasing by one each time.
        ///
        /// Monotone by construction, and deliberately *not* recomputed as the minimum of
        /// `Priorities`: a demoted thread that then terminates would raise that minimum, so a
        /// later demotion could land above a thread demoted earlier and silently break the
        /// ordering the policy depends on.
        ///
        /// Drift is not a practical concern. Demotions happen at about `P_BASE` per step, so
        /// after `n` steps the floor is near `-n/100`, and `double` represents integers exactly
        /// to 2^53 — about 9e17 steps. The virtual clock's own horizon faults some five orders
        /// of magnitude earlier than that.
        DemotionFloor : double
    }

/// The scheduling policy in effect for the current run. `RoundRobin` is
/// the default and reproduces the legacy deterministic-ordering behaviour:
/// among the Runnable threads, prefer the lowest id strictly greater than
/// `lastRan`, wrapping to the lowest id overall. `Pct` carries the live
/// PCT state for schedule fuzzing and is selected by the harness via a
/// seed on `PreparedProgram`.
///
/// Encoded as a DU rather than a behavioural strategy object so that the
/// set of policies stays enumerable in one place: pattern matches on
/// `state.Scheduling` are exhaustive and the compiler reminds you to
/// extend every decision point when a new policy lands.
type SchedulerState =
    | RoundRobin
    | Pct of PctState

[<RequireQualifiedAccess>]
module PctState =
    /// Fresh PCT state from a seed: the splitmix64 RNG is initialised to
    /// `seed` and the priority map is empty. The first `chooseNext` call
    /// will lazily populate priorities for each Runnable thread it
    /// observes.
    let ofSeed (seed : uint64) : PctState =
        {
            Rng = seed
            Priorities = Map.empty
            DemotionFloor = 0.0
        }

    /// Sample a fresh uniform-on-`[0, 1)` priority for `thread`, advancing
    /// the Rng one step. Overwrites any existing entry. Used only for the lazy
    /// first-observation insert; demotion goes through `demoteToBottom`, which draws nothing.
    let resamplePriority (thread : ThreadId) (state : PctState) : PctState =
        let priority, rng = NonCryptoRandom.nextDouble state.Rng

        { state with
            Rng = rng
            Priorities = state.Priorities |> Map.add thread priority
        }

    /// Demote `thread` below every other thread the scheduler currently knows about, by handing
    /// it the next `DemotionFloor` value and lowering the floor.
    ///
    /// This is PCT's demotion as Burckhardt et al. specify it, and the distinction from "resample
    /// uniformly" is the whole point: a uniform resample lands above the other `n-1` threads
    /// about `1/n` of the time, so a demoted thread keeps winning the argmax and residency is a
    /// heavy-tailed random walk rather than a rotation. Measured on issue #844's guest, a single
    /// spinner took a sixth to a quarter of all steps under uniform resampling while a thread
    /// doing real work got 1%.
    ///
    /// Draws no random numbers: the demotion target is determined, and only the decision *to*
    /// demote is stochastic. That keeps `chooseNext`'s consumption at exactly one `nextDouble`
    /// per call.
    ///
    /// Known cost, deliberately accepted. Because the floor only ever descends, the priority
    /// order among permanently-runnable threads is a rotation fixed by the initial draw: a run
    /// with `n` such threads contains exactly `n` distinct hand-off pairs, so an interleaving
    /// like "A is preempted by B and resumes before C runs" is unreachable at *every* seed
    /// rather than merely rare. All pairs stay reachable across seeds, and blocking, waking and
    /// thread creation all perturb the cycle in real guests. Splitting the draw between demotion
    /// and a uniform resample was measured as a fix and rejected: over 2048 seeds of each of the
    /// six `TestConcurrencyBugs` scenarios it left hit density statistically unchanged (and
    /// slightly worse on `JustABoolNotAMutex.cs`) while costing 2.3x the sweep time. The real
    /// remedy is PCT's own change-point budget -- switch `d-1` times per run at pre-drawn step
    /// counts, instead of at a flat per-step rate forever -- which is tracked separately.
    let demoteToBottom (thread : ThreadId) (state : PctState) : PctState =
        let priority = state.DemotionFloor - 1.0

        { state with
            Priorities = state.Priorities |> Map.add thread priority
            DemotionFloor = priority
        }

    /// Ensure every thread in `threads` has a priority entry, sampling
    /// for any that don't. Threads are processed in input-list order, so
    /// the caller controls determinism by passing a sorted list — the
    /// scheduler enumerates Runnable threads in ascending `ThreadId`
    /// order to give a stable sampling sequence across runs with the
    /// same seed. Existing entries are left untouched (this is *not*
    /// `resamplePriority` applied to all).
    let ensurePriorityFor (threads : ThreadId list) (state : PctState) : PctState =
        (state, threads)
        ||> List.fold (fun acc tid ->
            if acc.Priorities |> Map.containsKey tid then
                acc
            else
                resamplePriority tid acc
        )

    /// Drop `thread`'s priority entry. Called from
    /// `Scheduler.onThreadTerminated` so a terminated thread's slot
    /// cannot leak into a later `argmax` (which would never happen in
    /// practice because terminated threads aren't Runnable, but
    /// keeping the map shape "domain = ever-seen-and-not-terminated"
    /// is easier to reason about than "domain = ever-seen").
    let removeThread (thread : ThreadId) (state : PctState) : PctState =
        { state with
            Priorities = state.Priorities |> Map.remove thread
        }
