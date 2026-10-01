using System;
using System.Runtime.InteropServices;
using System.Threading;

// Signals sent to the process while System.Native's dispatcher is still busy
// with an earlier one each reach a managed handler: the runtime's native
// handler takes every instance as it is delivered and writes it to the pipe
// the dispatcher reads, so neither a repeat nor a signal the kernel would
// take first is lost or merged.
//
// SIGTERM is 15 and SIGINT 2 on both Linux and Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int terms;
    static int interrupts;

    static bool WaitFor(Func<bool> condition)
    {
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(30);
        while (!condition())
        {
            if (DateTime.UtcNow > deadline) return false;
            Thread.Sleep(10);
        }
        return true;
    }

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        using var term = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                Interlocked.Increment(ref terms);
            });
        using var interrupt = PosixSignalRegistration.Create(
            PosixSignal.SIGINT,
            context =>
            {
                context.Cancel = true;
                Interlocked.Increment(ref interrupts);
            });

        // Back to back: the later ones are sent while the dispatcher is still
        // handing the first to managed code.
        if (Kill(pid, 15) != 0) return 1;
        if (Kill(pid, 15) != 0) return 2;
        if (Kill(pid, 2) != 0) return 3;
        if (Kill(pid, 15) != 0) return 4;

        if (!WaitFor(() => Volatile.Read(ref terms) == 3 && Volatile.Read(ref interrupts) == 1)) return 5;

        // No handler runs more often than it was sent for.
        Thread.Sleep(TimeSpan.FromSeconds(1));
        if (Volatile.Read(ref terms) != 3) return 6;
        if (Volatile.Read(ref interrupts) != 1) return 7;

        return 0;
    }
}
