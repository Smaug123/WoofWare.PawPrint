using System;
using System.Runtime.InteropServices;
using System.Threading;

// SystemNative_WaitForSocketEvents screens `*count < 0` once, before the loop
// in WaitForSocketEventsInner that calls `epoll_wait(port, events, *count, -1)`
// again after a signal's EINTR. So a count another thread rewrites to -1 while
// the call sleeps is not the wrapper's EFAULT on the call made again: it reaches
// the kernel, whose epoll_wait answers a negative maxevents with EINVAL, and the
// inner function then writes 0 through `count`.
//
// Linux only: macOS's kevent answers a negative count with 0 events, and the
// call succeeds. The count cell lives in native memory so that another thread
// may legally write it while the call sleeps; 100 ms is far more than the main
// thread needs to enter the wait on the real runtime.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    const int PAL_SUCCESS = 0;
    const int PAL_EINVAL = 0x1001C;
    const int SIGTERM = 15;
    const int EventSize = 16;

    static int Handled;

    static unsafe int Main()
    {
        var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                Interlocked.Increment(ref Handled);
            });

        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 1;

        byte* buffer = (byte*)Marshal.AllocHGlobal(4 * EventSize);
        int* count = (int*)Marshal.AllocHGlobal(4);
        *count = 4;
        IntPtr countAddress = (IntPtr)count;

        Thread meddler = new Thread(() =>
        {
            Thread.Sleep(100);
            *(int*)countAddress = -1;
            Kill(Environment.ProcessId, SIGTERM);
        });
        meddler.Start();

        int rv = WaitForSocketEvents(port, buffer, count);
        meddler.Join();

        if (rv != PAL_EINVAL) return 2;
        if (*count != 0) return 3;

        GC.KeepAlive(registration);
        return 0;
    }
}
