using System;
using System.Diagnostics;

// Process.Kill on the calling process: SystemNative_Kill(getpid(), SIGKILL).
// The process never returns from the call, so the exit status is the signal's
// (128 + 9 as a shell or Process.ExitCode reports it), not Main's.
class Program
{
    static int Main(string[] args)
    {
        Process.GetCurrentProcess().Kill();
        // Unreachable. A distinct code, so a runtime that returned from Kill
        // would be caught rather than exiting with something plausible.
        return 3;
    }
}
