using System;
using System.Runtime.InteropServices;
using System.Threading;

// A process sends itself SIGTERM with libc's kill(2) while a
// PosixSignalRegistration handler for it is installed, and the handler does
// not cancel the default: once it returns, the process dies of SIGTERM.
//
// SIGTERM is 15 on both Linux and Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        using var registration = PosixSignalRegistration.Create(PosixSignal.SIGTERM, _ => { });

        if (Kill(Environment.ProcessId, 15) != 0) return 1;

        // Unreachable if the default runs. A distinct code, so a runtime that
        // never ran it would be caught rather than exiting with something
        // plausible.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return 3;
    }
}
