using System;
using System.Runtime.InteropServices;
using System.Threading;

// libc's sigsuspend(2) and pause(2) under the Darwin flavour. The main thread
// blocks SIGTERM and SIGHUP, and a PosixSignalRegistration handles SIGTERM.
//
// First, SIGTERM is raised at the main thread while it blocks it, so it is
// pending; sigsuspend with only SIGHUP blocked lets it through, and answers at
// once. Then another thread sends SIGTERM to the process while the main thread
// sleeps in sigsuspend. Each time the call fails with EINTR once the handler
// has run, and the mask is back to what it was before the call. Last, the main
// thread blocks only SIGHUP and calls pause, which another thread's SIGTERM
// ends the same way.
//
// Darwin numbers SIG_BLOCK and SIG_SETMASK 1 and 3, and its sigset_t is 32
// bits; SIGHUP is 1 (bit 0 of the
// set) and SIGTERM 15 (bit 14); EINTR is 4. The rows are
// docs/plans/2026-08-23-posix-kernel-extraction/sigsuspend-mask.c's.
unsafe class Program
{
    const int SIG_BLOCK = 1;
    const int SIG_SETMASK = 3;
    const uint Hup = 1U << 0;
    const uint Term = 1U << 14;
    const int EINTR = 4;

    [DllImport("libc", EntryPoint = "pthread_sigmask", SetLastError = true)]
    static extern int PthreadSigmask(int how, uint* set, uint* oldSet);

    [DllImport("libc", EntryPoint = "sigpending", SetLastError = true)]
    static extern int Sigpending(uint* set);

    [DllImport("libc", EntryPoint = "sigsuspend", SetLastError = true)]
    static extern int Sigsuspend(uint* mask);

    [DllImport("libc", EntryPoint = "pause", SetLastError = true)]
    static extern int Pause();

    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static uint Mask()
    {
        uint old = 0;
        if (PthreadSigmask(SIG_BLOCK, null, &old) != 0) throw new Exception("pthread_sigmask query failed");
        return old;
    }

    // Another thread sends SIGTERM to the process once the main thread is
    // asleep. The thread blocks SIGTERM itself, having inherited the main
    // thread's mask, so the main thread takes it.
    static Thread SendLater(int pid)
    {
        var sender = new Thread(() =>
        {
            Thread.Sleep(TimeSpan.FromMilliseconds(100));
            Kill(pid, 15);
        });
        sender.Start();
        return sender;
    }

    static int Main(string[] args)
    {
        using var handled = new SemaphoreSlim(0);
        int invocations = 0;

        using var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                Interlocked.Increment(ref invocations);
                context.Cancel = true;
                handled.Release();
            });

        int pid = Environment.ProcessId;
        uint blocked = Term | Hup;
        if (PthreadSigmask(SIG_SETMASK, &blocked, null) != 0) return 1;

        // Pending before the call, which lets it through: EINTR at once.
        if (Raise(15) != 0) return 2;
        uint pending = 0;
        if (Sigpending(&pending) != 0) return 3;
        if ((pending & Term) == 0) return 4;

        uint temporary = Hup;
        if (Sigsuspend(&temporary) != -1) return 5;
        if (Marshal.GetLastPInvokeError() != EINTR) return 6;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 7;
        if (Mask() != (Term | Hup)) return 8;

        // Sent to the process during the sleep.
        var sender = SendLater(pid);
        if (Sigsuspend(&temporary) != -1) return 9;
        if (Marshal.GetLastPInvokeError() != EINTR) return 10;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 11;
        if (Mask() != (Term | Hup)) return 12;
        sender.Join();

        // pause waits under the mask the thread has.
        blocked = Hup;
        if (PthreadSigmask(SIG_SETMASK, &blocked, null) != 0) return 13;
        sender = SendLater(pid);
        if (Pause() != -1) return 14;
        if (Marshal.GetLastPInvokeError() != EINTR) return 15;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 16;
        if (Mask() != Hup) return 17;
        sender.Join();

        if (Sigpending(&pending) != 0) return 18;
        if (pending != 0) return 19;

        // As PosixSignalRaiseCancelled: let the runtime decide on the default
        // after the handler has returned.
        Thread.Sleep(TimeSpan.FromSeconds(1));
        if (Volatile.Read(ref invocations) != 3) return 20;

        return 0;
    }
}
