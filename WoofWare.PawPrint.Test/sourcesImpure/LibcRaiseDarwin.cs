using System;
using System.Runtime.InteropServices;
using System.Threading;

// libc's raise(3) under the Darwin flavour, from the main thread and from
// another, on numbers that mean something different under Linux's numbering.
// Each row would end the run under Linux: 20 is SIGTSTP there, which would
// stop the process; and 29 is SIGIO, which terminates. Under Darwin's, 20 is
// SIGCHLD and 29 SIGINFO, both discarded by default; and 34, a signal on
// Linux, is past Darwin's last.
//
// Measured by docs/plans/2026-08-23-posix-kernel-extraction/raise-sweep.c.
class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int Rows()
    {
        if (Raise(20) != 0) return 1;
        if (Raise(29) != 0) return 2;
        if (Raise(34) != -1) return 3;
        if (Marshal.GetLastPInvokeError() != 22) return 4; // EINVAL
        return 0;
    }

    static int Main(string[] args)
    {
        int onMain = Rows();
        if (onMain != 0) return onMain;

        int onWorker = 99;
        var worker = new Thread(() => onWorker = Rows());
        worker.Start();
        worker.Join();
        return onWorker == 0 ? 0 : 10 + onWorker;
    }
}
