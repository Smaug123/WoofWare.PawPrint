using System;
using System.Runtime.InteropServices;
using System.Threading;

// libc's pthread_sigmask(3), sigprocmask(2) and sigpending(2) under the Darwin
// flavour. The main thread blocks SIGTERM, raises it at itself, and sees it
// pending with no handler run; then it unblocks it with sigprocmask, and the
// PosixSignalRegistration handler runs and cancels the default.
//
// Darwin numbers SIG_BLOCK, SIG_UNBLOCK and SIG_SETMASK 1, 2 and 3, and its
// sigset_t is 32 bits; SIGTERM is 15 (bit 14), and bit 31 is signal number 32,
// which Darwin does not have and keeps in a mask all the same. The rows are
// docs/plans/2026-08-23-posix-kernel-extraction/sigprocmask-ops.c's and
// sigpending-scope.c's.
unsafe class Program
{
    const int SIG_BLOCK = 1;
    const int SIG_UNBLOCK = 2;
    const uint Term = 1u << 14;
    const uint Bit31 = 1u << 31;
    const uint Untouched = 0xA5A5A5A5u;

    [DllImport("libc", EntryPoint = "pthread_sigmask")]
    static extern int PthreadSigmask(int how, uint* set, uint* oldSet);

    [DllImport("libc", EntryPoint = "sigprocmask", SetLastError = true)]
    static extern int Sigprocmask(int how, uint* set, uint* oldSet);

    [DllImport("libc", EntryPoint = "sigpending", SetLastError = true)]
    static extern int Sigpending(uint* set);

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
                Interlocked.Increment(ref invocations);
                context.Cancel = true;
                handled.Set();
            });

        // An unnamed how with a set: pthread_sigmask returns EINVAL rather than
        // setting errno, and writes nothing to the old set.
        uint set = Term;
        uint old = Untouched;
        if (PthreadSigmask(100, &set, &old) != 22) return 1;
        if (old != Untouched) return 2;

        // With a NULL set, how is not looked at.
        if (PthreadSigmask(100, null, &old) != 0) return 3;
        if ((old & Term) != 0) return 4;

        // Bit 31 names no signal, and is kept.
        set = Term | Bit31;
        if (PthreadSigmask(SIG_BLOCK, &set, null) != 0) return 5;
        if (PthreadSigmask(SIG_BLOCK, null, &old) != 0) return 6;
        if ((old & (Term | Bit31)) != (Term | Bit31)) return 7;

        uint pending = Untouched;
        if (Sigpending(&pending) != 0) return 8;
        if ((pending & Term) != 0) return 9;

        // Raised at this thread while it blocks it: pending here, and no
        // handler runs.
        if (Raise(15) != 0) return 10;
        if (handled.Wait(TimeSpan.FromMilliseconds(200))) return 11;
        if (Sigpending(&pending) != 0) return 12;
        if ((pending & Term) == 0) return 13;

        // Unblocked, for every thread, by Darwin's sigprocmask: taken as the
        // call returns, and the handler runs.
        set = Term;
        if (Sigprocmask(SIG_UNBLOCK, &set, &old) != 0) return 14;
        if ((old & Term) == 0) return 15;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 16;
        if (Sigpending(&pending) != 0) return 17;
        if ((pending & Term) != 0) return 18;

        // As PosixSignalRaiseCancelled: let the runtime decide on the default
        // after the handler has returned.
        Thread.Sleep(TimeSpan.FromSeconds(1));
        if (Volatile.Read(ref invocations) != 1) return 19;

        // sigprocmask fails as a system call does: -1, with errno EINVAL.
        if (Sigprocmask(100, &set, null) != -1) return 20;
        if (Marshal.GetLastPInvokeError() != 22) return 21;

        return 0;
    }
}
