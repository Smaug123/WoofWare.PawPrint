using System;
using System.Runtime.InteropServices;
using System.Threading;

// A process registers a handler for SIGILL that cancels it, and sends itself
// SIGILL once: the handler runs, but the runtime's own handler, which
// System.Native's runs first, has restored the default over System.Native's.
// Enabling SIGILL in System.Native again does nothing, because System.Native
// records that it installed its handler and has not restored it since; so the
// second SIGILL meets the default, and kills the process.
//
// SIGILL is 4 on both Linux and Darwin, and has no PosixSignal member. The
// BCL enables a signal only for its first registration, so the guest calls
// System.Native itself.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_EnablePosixSignalHandling")]
    static extern int EnablePosixSignalHandling(int signalCode);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;
        using var handled = new ManualResetEventSlim(false);
        using var registration = PosixSignalRegistration.Create(
            (PosixSignal)4,
            context =>
            {
                context.Cancel = true;
                handled.Set();
            });

        if (Kill(pid, 4) != 0) return 1;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 2;

        if (EnablePosixSignalHandling(4) != 1) return 3;

        if (Kill(pid, 4) != 0) return 4;

        // Unreachable if the default runs. A distinct code, so a runtime that
        // never ran it would be caught rather than exiting with something
        // plausible.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return 5;
    }
}
