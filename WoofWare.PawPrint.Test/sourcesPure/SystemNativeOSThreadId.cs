using System;
using System.Runtime.InteropServices;
using System.Threading;

// Exercises the SystemNative_TryGetUInt32OSThreadId and
// SystemNative_GetUInt64OSThreadId PawPrint handlers directly via P/Invoke
// stubs. The CLR would dispatch these to the real libSystem.Native shim;
// PawPrint intercepts them and answers from ThreadState.OsThreadId.
//
// Declaring the stubs here rather than reaching them through CoreLib is what
// lets one test cover *both* entry points on any host. CoreLib is #if-split per
// target -- `Lock.ThreadId.InitializeForCurrentThread` calls GetUInt64OSThreadId
// under TARGET_OSX and TryGetUInt32OSThreadId everywhere else, and each flavour
// declares only its own Interop.Sys stub -- so a test that went through
// System.Threading.Lock would exercise exactly one of the two, chosen by
// whichever machine happened to run it. This guest assembly is ours, so both
// are reachable everywhere. Upstream compiles both unconditionally on Unix
// (pal_threading.c has no platform #if around either), so the real runtime
// resolves both too.
//
// This is a *pure* case, so it is differentially compared against the real
// runtime running in-process, and may only assert facts that hold on both.
// In particular, do NOT assert any specific numeric id, nor that ids are small,
// contiguous, or increasing: real kernels give arbitrary ones. PawPrint's exact
// numbering is pinned in TestOsThreadId instead.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryGetUInt32OSThreadId")]
    static extern uint TryGetUInt32OSThreadId();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetUInt64OSThreadId")]
    static extern ulong GetUInt64OSThreadId();

    static int Main(string[] args)
    {
        uint id32 = TryGetUInt32OSThreadId();
        ulong id64 = GetUInt64OSThreadId();

        // Neither entry point can report 0 for a live thread. The 32-bit one
        // substitutes (uint)-1 when the truncated id is 0, and the 64-bit one
        // returns minipal_get_current_thread_id(), which no supported Unix
        // reports as 0 for a running thread.
        if (id32 == 0) return 1;
        if (id64 == 0) return 2;

        // (uint)-1 means "this platform does not know how to get a thread id".
        // Every platform minipal supports does know -- although note this is a
        // statement about the platform, not a mathematical certainty: the
        // 32-bit entry point truncates, so a macOS host whose 64-bit id had a
        // zero low word would legitimately report the sentinel for a live
        // thread. That is a ~2^-32 coincidence, not a flake worth guarding.
        if (id32 == unchecked((uint)-1)) return 3;

        // The 32-bit value is the truncation of the 64-bit one: upstream both
        // read the same minipal_get_current_thread_id(), with the 64-bit entry
        // point returning it verbatim. Asserted in the truncating direction,
        // which holds on every platform -- the reverse (that the 64-bit value
        // is the zero-extension of the 32-bit one) is true on Linux, where a
        // tid is a pid_t, but not structurally guaranteed on macOS, where
        // pthread_threadid_np is a system-wide 64-bit counter.
        if ((uint)id64 != id32) return 4;

        // Stable within a thread: an id is an identity, not a sample. This is
        // what System.Threading.Lock relies on to recognise its own owner.
        if (TryGetUInt32OSThreadId() != id32) return 5;
        if (GetUInt64OSThreadId() != id64) return 6;

        // Distinct across threads: the other half of "identity". A worker must
        // not be mistakable for the main thread, or Lock would treat a
        // contending acquire as a recursive one.
        uint workerId32 = 0;
        ulong workerId64 = 0;
        Thread worker = new Thread(() =>
        {
            workerId32 = TryGetUInt32OSThreadId();
            workerId64 = GetUInt64OSThreadId();
        });
        worker.Start();
        worker.Join();

        if (workerId32 == 0) return 7;
        if (workerId64 == 0) return 8;
        if (workerId32 == unchecked((uint)-1)) return 9;
        if ((uint)workerId64 != workerId32) return 10;

        if (workerId32 == id32) return 11;
        if (workerId64 == id64) return 12;

        return 0;
    }
}
