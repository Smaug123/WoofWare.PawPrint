using System;
using System.Runtime.InteropServices;
using System.Threading;

// A process sends itself SIGILL and SIGABRT with libc's kill(2), and survives
// each: the runtime's handler for a hardware-fault signal, sent one rather
// than taking a fault, restores the default and returns. The second SIGILL
// then meets that default, and kills the process.
//
// SIGILL and SIGABRT are 4 and 6 on both Linux and Darwin. They are the fault
// signals whose handler a real process does not need again once it has
// restored the default (see NativeLibc.screenSelfSignal), so PawPrint answers
// them on every platform.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        if (Kill(pid, 4) != 0) return 1;
        if (Kill(pid, 6) != 0) return 2;

        if (Kill(pid, 4) != 0) return 3;

        // Unreachable if the default runs. A distinct code, so a runtime that
        // never ran it would be caught rather than exiting with something
        // plausible.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return 4;
    }
}
