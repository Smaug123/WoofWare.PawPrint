using System;
using System.Runtime.InteropServices;
using System.Threading;

// A signal that is ignored when it is sent is discarded then: a handler
// registered afterwards never sees it, even when it is registered while the
// runtime's signal-handling thread is still busy with an earlier signal.
//
// SIGTERM is 15 and SIGWINCH 28 on both Linux and Darwin; SIGWINCH is
// discarded by default. Nothing else in the process generates SIGWINCH: the
// guest has no terminal whose window could change.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        using var terminated = new ManualResetEventSlim(false);
        using var terminateRegistration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                terminated.Set();
            });

        // Keep the signal-handling thread busy with SIGTERM...
        if (Kill(pid, 15) != 0) return 1;

        // ...while SIGWINCH is sent with nothing registered for it, and a
        // handler is then registered.
        if (Kill(pid, 28) != 0) return 2;

        using var resized = new ManualResetEventSlim(false);
        using var resizeRegistration = PosixSignalRegistration.Create(PosixSignal.SIGWINCH, _ => resized.Set());

        if (!terminated.Wait(TimeSpan.FromSeconds(30))) return 3;

        // Long enough for a handler to have run many times over, had anything
        // been left pending for it.
        if (resized.Wait(TimeSpan.FromSeconds(2))) return 4;

        return 0;
    }
}
