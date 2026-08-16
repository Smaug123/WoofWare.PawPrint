namespace WoofWare.PawPrint

/// State owned by the schedule-fuzzing PCT (Probabilistic Concurrency Testing)
/// policy: a deterministic splitmix64 RNG state plus the current
/// per-thread priority assignment. Burckhardt et al.'s original PCT
/// algorithm uses a fixed switch budget `d`; we instead drive demotion
/// from per-step `ContextSwitchPrior` weights, so the priority map is
/// resampled lazily rather than constructed up-front.
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
/// Demotion resamples the running thread's priority; the next chosen
/// thread is whichever Runnable thread has the maximum priority.
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
        /// `Scheduler.onThreadTerminated`, and resampled in place on
        /// demotion. Threads not present here are not currently
        /// Runnable from the scheduler's point of view (or have never
        /// been seen Runnable yet).
        Priorities : Map<ThreadId, double>
    }

/// The scheduling policy in effect for the current run. `RoundRobin` is
/// the default, deterministic ordering:
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
        }

    /// Sample a fresh uniform-on-`[0, 1)` priority for `thread`, advancing
    /// the Rng one step. Overwrites any existing entry — used both for the
    /// lazy first-observation insert and for demotion (which resamples the
    /// running thread's priority).
    let resamplePriority (thread : ThreadId) (state : PctState) : PctState =
        let priority, rng = NonCryptoRandom.nextDouble state.Rng

        {
            Rng = rng
            Priorities = state.Priorities |> Map.add thread priority
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
