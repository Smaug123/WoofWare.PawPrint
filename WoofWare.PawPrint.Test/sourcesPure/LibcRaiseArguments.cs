using System;
using System.Runtime.InteropServices;

// libc's raise(3), with arguments whose answer is the same on Linux and
// Darwin: the null signal, numbers that are no signal on either, glibc's own
// 32 and 33 (which raise refuses, where kill(2) sends them), and a signal both
// discard by default.
//
// Measured by docs/plans/2026-08-23-posix-kernel-extraction/raise-sweep.c on
// Linux 6.18.5 (glibc 2.41) and Darwin 27.0.0.
class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    const int EINVAL = 22; // on both

    // 0 if raise(sig) failed with EINVAL; otherwise a code naming what it did.
    static int ExpectEinval(int sig, int code)
    {
        int result = Raise(sig);
        if (result != -1) return code;
        if (Marshal.GetLastPInvokeError() != EINVAL) return code + 1;
        return 0;
    }

    static int Main(string[] args)
    {
        // The null signal sends nothing.
        if (Raise(0) != 0) return 1;

        int failed;
        if ((failed = ExpectEinval(-1, 10)) != 0) return failed;
        if ((failed = ExpectEinval(32, 20)) != 0) return failed;
        if ((failed = ExpectEinval(33, 30)) != 0) return failed;
        if ((failed = ExpectEinval(65, 40)) != 0) return failed;
        if ((failed = ExpectEinval(1000, 50)) != 0) return failed;
        if ((failed = ExpectEinval(int.MinValue, 60)) != 0) return failed;
        if ((failed = ExpectEinval(int.MaxValue, 70)) != 0) return failed;

        // SIGWINCH is 28 on both, and discarded by default.
        if (Raise(28) != 0) return 80;

        return 0;
    }
}
