using System;
using System.Runtime.InteropServices;
using System.Threading;

// A process sends itself SIGILL, SIGABRT, SIGFPE and SIGSEGV with libc's
// kill(2), and survives each: the runtime's handler for a hardware-fault
// signal, sent one rather than taking a fault, restores the default and
// returns. The second SIGSEGV then meets that default, and kills the process.
//
// SIGILL, SIGABRT, SIGFPE and SIGSEGV are 4, 6, 8 and 11 on both Linux and
// Darwin. (SIGBUS is not: 7 on Linux, 10 on Darwin.)
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        if (Kill(pid, 4) != 0) return 1;
        if (Kill(pid, 6) != 0) return 2;
        if (Kill(pid, 8) != 0) return 3;
        if (Kill(pid, 11) != 0) return 4;

        if (Kill(pid, 11) != 0) return 5;

        // Unreachable if the default runs. A distinct code, so a runtime that
        // never ran it would be caught rather than exiting with something
        // plausible.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return 6;
    }
}
