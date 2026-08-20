using System;
using System.Runtime.InteropServices;
using System.Threading;

// The last-error slot is per-thread: no thread can observe another's write to it. On Unix
// that slot *is* errno (CoreCLR's PAL stores its last-error there, "Reuse errno to store
// last error"), and errno is thread-local by POSIX; on Windows it is GetLastError, also
// per-thread. So this is a fact about any conforming runtime, not a PawPrint contract.
//
// Marshal.Set/GetLastSystemError are the canonical accessors for that slot. Using them
// rather than a failing syscall keeps the test free of fd, filesystem-seed and errno-
// numbering concerns: what is under test is the slot's isolation, not any error value.
//
// **Only isolation is asserted, never the value a thread reads.** Neither runtime keeps a
// thread's own value across `Thread.Start`: on real .NET, Start goes through a
// SetLastError=true import whose stub zeroes errno first, so the main thread reads 0 from
// there on. PawPrint has no such stub, so it reads what it last wrote. That difference is
// a separate matter (the SetLastError stub); it is deliberately not asserted here, which
// is why each check compares against the *other* thread's sentinel rather than its own.
//
// The sentinels are values no runtime produces on its own, so "did not see the other
// thread's write" is decidable regardless of what the synchronisation left behind.
class Program
{
    const int MainSentinel = 1111;
    const int WorkerSentinel = 2222;

    static int workerSawBeforeWriting;
    static int workerSawAfterWriting;

    static void Worker()
    {
        workerSawBeforeWriting = Marshal.GetLastSystemError();
        Marshal.SetLastSystemError(WorkerSentinel);
        workerSawAfterWriting = Marshal.GetLastSystemError();
    }

    static int Main()
    {
        Marshal.SetLastSystemError(MainSentinel);

        Thread t = new Thread(Worker);
        t.Start();
        t.Join();

        // Premise: a thread does see its own write.
        if (workerSawAfterWriting != WorkerSentinel) return 1;

        // The worker must never have seen the main thread's write.
        if (workerSawBeforeWriting == MainSentinel) return 2;

        // ...and the main thread must not see the worker's.
        if (Marshal.GetLastSystemError() == WorkerSentinel) return 3;

        return 0;
    }
}
