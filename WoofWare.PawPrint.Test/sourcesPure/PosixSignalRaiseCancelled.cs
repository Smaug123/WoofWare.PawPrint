using System;
using System.Runtime.InteropServices;
using System.Threading;

// The main thread raises SIGTERM with libc's raise(3) while a
// PosixSignalRegistration handler for it is installed, and the handler
// cancels the default: the process carries on, and exits normally.
//
// SIGTERM is 15 on both Linux and Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int Main(string[] args)
    {
        using var handled = new ManualResetEventSlim(false);
        int invocations = 0;

        using var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                if (context.Signal != PosixSignal.SIGTERM) Environment.Exit(4);
                Interlocked.Increment(ref invocations);
                context.Cancel = true;
                handled.Set();
            });

        if (Raise(15) != 0) return 1;

        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 2;

        // The handler has run, but the runtime decides on the default only
        // after it returns. Give it time to, so that ignoring the
        // cancellation would kill the process here rather than racing the
        // return from Main.
        Thread.Sleep(TimeSpan.FromSeconds(1));

        if (Volatile.Read(ref invocations) != 1) return 3;

        return 0;
    }
}
