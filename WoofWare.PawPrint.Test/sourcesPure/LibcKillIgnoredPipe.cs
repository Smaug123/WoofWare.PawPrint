using System;
using System.Runtime.InteropServices;
using System.Threading;

// SIGPIPE, sent by the process to itself with libc's kill(2), is discarded:
// the runtime sets it to SIG_IGN before Main. A PosixSignalRegistration for
// it leaves it ignored (System.Native respects an ignored signal), so its
// handler never runs, and disposing the registration puts back the ignore.
//
// SIGPIPE is 13 on both Linux and Darwin, and has no PosixSignal member.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        if (Kill(pid, 13) != 0) return 1;

        int invocations = 0;

        using (PosixSignalRegistration.Create((PosixSignal)13, _ => Interlocked.Increment(ref invocations)))
        {
            if (Kill(pid, 13) != 0) return 2;

            // Long enough for a handler to have run, had the signal been
            // delivered to one.
            Thread.Sleep(TimeSpan.FromSeconds(1));
        }

        if (Kill(pid, 13) != 0) return 3;

        if (Volatile.Read(ref invocations) != 0) return 4;

        return 0;
    }
}
