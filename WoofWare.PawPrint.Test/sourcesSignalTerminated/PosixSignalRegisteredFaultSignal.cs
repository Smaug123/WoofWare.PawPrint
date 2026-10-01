using System;
using System.Runtime.InteropServices;
using System.Threading;

// A process registers a handler for SIGILL through PosixSignalRegistration
// and sends itself SIGILL with libc's kill(2) twice. The first reaches the
// managed handler, and the process survives it; but System.Native's native
// handler first runs the runtime's handler it replaced, which restores the
// default over System.Native's own. The second SIGILL meets that default,
// and kills the process.
//
// SIGILL is 4 on both Linux and Darwin, and has no PosixSignal member.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;
        using var handled = new ManualResetEventSlim(false);
        using var registration = PosixSignalRegistration.Create((PosixSignal)4, _ => handled.Set());

        if (Kill(pid, 4) != 0) return 1;

        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 2;

        if (Kill(pid, 4) != 0) return 3;

        // Unreachable if the default runs. A distinct code, so a runtime that
        // never ran it would be caught rather than exiting with something
        // plausible.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return 4;
    }
}
