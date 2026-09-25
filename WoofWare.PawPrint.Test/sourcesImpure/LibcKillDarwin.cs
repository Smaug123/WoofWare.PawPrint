using System;
using System.Runtime.InteropServices;

// libc's kill(2) sent by the process to itself under the Darwin flavour, with
// numbers that mean something different under Linux's numbering. Each row
// would end the run under Linux: 32 is a signal there, and terminates; 20 is
// SIGTSTP, which would stop the process; and 29 is SIGIO, which terminates.
//
// Measured by docs/plans/2026-08-23-posix-kernel-extraction/kill-arguments.c
// (32 past Darwin's NSIG) and by the defaults sweep in TestSignal (Darwin
// discards SIGCHLD and SIGINFO by default).
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        if (Kill(pid, 32) != -1) return 1;
        if (Marshal.GetLastPInvokeError() != 22) return 2; // EINVAL

        if (Kill(pid, 20) != 0) return 3; // SIGCHLD
        if (Kill(pid, 29) != 0) return 4; // SIGINFO

        return 0;
    }
}
