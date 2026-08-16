using System;
using System.Threading;

// Exercises `Thread.GetCurrentProcessorId()` end to end. On a Unix CoreLib it
// descends `ProcessorIdCache.GetCurrentProcessorId` -> the internal
// `Thread.GetCurrentProcessorNumber()` -> `Interop.Sys.SchedGetCpu` -> the
// `SystemNative_SchedGetCpu` PAL entry point.
//
// This is a *pure* case, so it is differentially compared against the real
// runtime running in-process, and may only assert facts that hold on both.
//
// In particular, do NOT add `id < Environment.ProcessorCount` here, tempting
// though it looks. `sched_getcpu` does not exist on macOS, where the PAL shim
// returns -1; CoreLib's `RefreshCurrentProcessorId` then substitutes
// `Environment.CurrentManagedThreadId`, a monotonically growing per-process
// counter with no relationship to the core count, which is easily >=
// ProcessorCount on a host that has already created plenty of threads
// in-process. That upper bound is a PawPrint-only fact and is asserted by the
// impure case `SchedGetCpuPlacement.cs` instead.
class Program
{
    static int Main(string[] args)
    {
        int id = Thread.GetCurrentProcessorId();

        // Non-negativity does hold everywhere: on a platform without
        // `sched_getcpu`, the raw -1 makes `ProcessorNumberSpeedCheck` return
        // false, which forces `GetCurrentProcessorId` onto the cached path, and
        // that path clamps a negative raw reading up to
        // `Environment.CurrentManagedThreadId`.
        if (id < 0) return 1;

        // Deliberately NOT asserted here: that two consecutive calls agree.
        // On Linux, if `sched_getcpu` is fast enough (plausible via vDSO),
        // `ProcessorNumberSpeedCheck` sets `s_isProcessorNumberReallyFast` and
        // `GetCurrentProcessorId` bypasses its ThreadStatic cache entirely, so
        // every call is a live kernel query and the OS may migrate
        // the thread between two of them on a busy CI runner. PawPrint would
        // satisfy such an assertion trivially (it never migrates), so it would
        // buy no coverage while importing a real-runtime flake.

        // A worker thread also observes a legal value. On PawPrint this is the
        // interesting half — it is the only way to see a *second* placement —
        // while on the real runtime it just re-checks the invariant above.
        int workerId = -1;
        Thread worker = new Thread(() => { workerId = Thread.GetCurrentProcessorId(); });
        worker.Start();
        worker.Join();

        if (workerId < 0) return 2;

        return 0;
    }
}
