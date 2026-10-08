using System;
using System.Runtime.InteropServices;
using System.Threading;

// libc's pthread_sigmask(3), sigprocmask(2) and sigpending(2) under the Linux
// flavour. The main thread blocks SIGTERM, raises it at itself, and sees it
// pending with no handler run; then it unblocks it with sigprocmask, and the
// PosixSignalRegistration handler runs and cancels the default.
//
// Linux numbers SIG_BLOCK, SIG_UNBLOCK and SIG_SETMASK 0, 1 and 2; SIGTERM is
// 15 (bit 14 of the set), and the set's bit 31 is signal 32, glibc's own,
// which its wrappers take out of every set they are handed. The rows are
// docs/plans/2026-08-23-posix-kernel-extraction/sigprocmask-ops.c's and
// sigpending-scope.c's.
unsafe class Program
{
    const int SIG_BLOCK = 0;
    const int SIG_UNBLOCK = 1;
    const ulong Term = 1UL << 14;
    const ulong GlibcCancel = 1UL << 31;
    const ulong Untouched = 0xA5A5A5A5A5A5A5A5UL;

    [DllImport("libc", EntryPoint = "pthread_sigmask", SetLastError = true)]
    static extern int PthreadSigmask(int how, ulong* set, ulong* oldSet);

    [DllImport("libc", EntryPoint = "sigprocmask", SetLastError = true)]
    static extern int Sigprocmask(int how, ulong* set, ulong* oldSet);

    [DllImport("libc", EntryPoint = "sigpending", SetLastError = true)]
    static extern int Sigpending(ulong* set);

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

        // An unnamed how with a set: glibc's pthread_sigmask returns EINVAL and
        // leaves errno alone, and writes nothing to the old set.
        ulong set = Term;
        ulong old = Untouched;
        if (PthreadSigmask(100, &set, &old) != 22) return 1;
        if (old != Untouched) return 2;
        if (Marshal.GetLastPInvokeError() != 0) return 22;

        // With a NULL set, how is not looked at.
        if (PthreadSigmask(100, null, &old) != 0) return 3;
        if ((old & Term) != 0) return 4;

        // glibc takes its own 32 out of the set.
        set = Term | GlibcCancel;
        if (PthreadSigmask(SIG_BLOCK, &set, null) != 0) return 5;
        if (PthreadSigmask(SIG_BLOCK, null, &old) != 0) return 6;
        if ((old & (Term | GlibcCancel)) != Term) return 7;

        ulong pending = Untouched;
        if (Sigpending(&pending) != 0) return 8;
        if ((pending & Term) != 0) return 9;

        // Raised at this thread while it blocks it: pending here, and no
        // handler runs.
        if (Raise(15) != 0) return 10;
        if (handled.Wait(TimeSpan.FromMilliseconds(200))) return 11;
        if (Sigpending(&pending) != 0) return 12;
        if ((pending & Term) == 0) return 13;

        // Unblocked: taken as the call returns, and the handler runs.
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
