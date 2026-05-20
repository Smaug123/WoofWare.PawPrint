namespace WoofWare.PawPrint

/// State owned by the schedule-fuzzing PCT (Probabilistic Concurrency Testing)
/// policy: a deterministic splitmix64 RNG state plus the current
/// per-thread priority assignment. Burckhardt et al.'s original PCT
/// algorithm uses a fixed switch budget `d`; we instead drive demotion
/// from per-step `ContextSwitchPrior` weights, so the priority map is
/// resampled lazily rather than constructed up-front.
///
/// Each thread is sampled into `[0, 1)` on first observation. Demotion
/// resamples the running thread's priority; the next chosen thread is
/// whichever Runnable thread has the maximum priority. `Map<ThreadId, double>`
/// keeps this purely functional so that schedule replay is bit-exact.
type PctState =
    {
        /// splitmix64 state, advanced via `NonCryptoRandom.step` /
        /// `NonCryptoRandom.nextDouble` / `NonCryptoRandom.nextInt32Below`
        /// whenever the scheduler needs a uniform draw. Threading the
        /// state through every decision is what keeps the run reproducible
        /// from a seed.
        Rng : uint64
        /// Priority assigned to each thread the scheduler has observed.
        /// Entries are inserted lazily (on first scheduling decision that
        /// sees the thread) and removed on termination so that a terminated
        /// thread's slot cannot leak into a later choice.
        Priorities : Map<ThreadId, double>
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
