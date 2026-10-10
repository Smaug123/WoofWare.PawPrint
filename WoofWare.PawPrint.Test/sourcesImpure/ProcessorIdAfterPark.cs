using System;
using System.Runtime.InteropServices;
using System.Threading;

// Run with KernelConfig.ProcessorCount = 2, under the Linux flavour.
//
// The entry thread is placed on processor 0, and the two workers on 1 and 0.
// The kernel answers `sched_getcpu` only for a thread it records as running, and
// a park in a syscall takes a thread off its processor, so each worker's read
// after a park checks that the driver put it back on its processor:
//
//   * the first worker blocks reading an empty pipe, and the entry thread writes
//     the byte that wakes it, so another thread runs between its park and its
//     wake;
//   * the second blocks in a `poll` that times out while the entry thread waits
//     in `Join`, so no other thread runs between its park and its next step,
//     which a driver that reported a thread only when it switched threads would
//     miss.
//
// Impure because the answer depends on the simulated machine's processor count.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    // `Thread.GetCurrentProcessorId` caches its answer for a number of calls,
    // so ask the shim directly, as the cache does when it refreshes.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SchedGetCpu")]
    static extern int SchedGetCpu();

    struct PollEvent
    {
        public int FileDescriptor;
        public short Events;
        public short TriggeredEvents;
    }

    const short POLLIN = 0x0001;

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Poll")]
    static extern unsafe int Poll(PollEvent* pollEvents, uint eventCount, int milliseconds, uint* triggered);

    static unsafe int Main(string[] args)
    {
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];

        if (SchedGetCpu() != 0) return 2;

        int before = -1;
        int after = -1;
        int read = -1;

        Thread worker = new Thread(() =>
        {
            before = SchedGetCpu();
            byte b;
            read = Read(readEnd, &b, 1);
            after = SchedGetCpu();
        });
        worker.Start();

        // Long enough that the worker is asleep in its read before the write.
        Thread.Sleep(50);
        if (SchedGetCpu() != 0) return 3;

        byte one = 1;
        if (Write(writeEnd, &one, 1) != 1) return 4;
        worker.Join();

        if (read != 1) return 5;
        if (before != 1) return 6;
        if (after != 1) return 7;
        if (SchedGetCpu() != 0) return 8;

        // The pipe is empty again, so the poll sleeps for its whole timeout.
        int pollBefore = -1;
        int pollAfter = -1;
        int polled = -1;

        Thread poller = new Thread(() =>
        {
            pollBefore = SchedGetCpu();
            PollEvent ev = new PollEvent { FileDescriptor = (int)readEnd, Events = POLLIN };
            uint triggered = 99;
            polled = Poll(&ev, 1, 10, &triggered);
            if (triggered != 0) polled = -2;
            pollAfter = SchedGetCpu();
        });
        poller.Start();
        poller.Join();

        if (polled != 0) return 9;
        if (pollBefore != 0) return 10;
        if (pollAfter != 0) return 11;
        return 0;
    }
}
