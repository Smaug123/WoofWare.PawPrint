using System;
using System.Runtime.InteropServices;
using System.Threading;

// SystemNative_SchedGetCpu under the Darwin flavour, and what
// `Thread.GetCurrentProcessorId()` makes of it.
//
// Darwin's libc has no `sched_getcpu`, so the shim is built without
// HAVE_SCHED_GETCPU and answers a hard -1 (pal_threading.c). Measured on
// Darwin 27.0.0 by P/Invoking the real libSystem.Native: -1 on every one of
// 1000 calls, and on a second thread.
//
// CoreLib reads -1 as "not supported": `ProcessorIdCache.RefreshCurrentProcessorId`
// substitutes `Environment.CurrentManagedThreadId`, and caches it per thread.
// So on Darwin a thread's first `GetCurrentProcessorId()` is its managed
// thread id, whatever the processor count.
//
// The Linux column, where the call reports a placement, is
// `SchedGetCpuPlacement.cs`.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SchedGetCpu")]
    static extern int SchedGetCpu();

    static int Main(string[] args)
    {
        if (SchedGetCpu() != -1) return 1;

        // The managed thread id, not a processor index: the entry thread is
        // on processor 0 of every placement, and its managed id is not 0.
        if (Thread.GetCurrentProcessorId() != Environment.CurrentManagedThreadId) return 2;

        int workerRaw = 0;
        int workerId = -1;
        int workerManagedId = -2;
        Thread worker = new Thread(() =>
        {
            workerRaw = SchedGetCpu();
            workerId = Thread.GetCurrentProcessorId();
            workerManagedId = Environment.CurrentManagedThreadId;
        });
        worker.Start();
        worker.Join();

        if (workerRaw != -1) return 3;
        if (workerId != workerManagedId) return 4;

        return 0;
    }
}
