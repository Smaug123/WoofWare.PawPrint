using System;
using System.Runtime.InteropServices;
using System.Threading;

// The main thread raises SIGILL and SIGABRT with libc's raise(3), and the
// process survives each: the runtime's handler for a hardware-fault signal,
// raised rather than taken as a fault, restores the default and returns, as
// it does for one the process sends itself with kill(2). The second SIGILL
// then meets that default, and kills the process.
//
// SIGILL and SIGABRT are 4 and 6 on both Linux and Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int Main(string[] args)
    {
        if (Raise(4) != 0) return 1;
        if (Raise(6) != 0) return 2;

        if (Raise(4) != 0) return 3;

        // Unreachable if the default runs.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return 4;
    }
}
