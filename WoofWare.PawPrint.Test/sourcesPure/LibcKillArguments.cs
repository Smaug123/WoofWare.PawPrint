using System;
using System.Runtime.InteropServices;

// libc's kill(2) sent by the process to itself, with arguments whose answer
// is the same on Linux and Darwin: the null signal, numbers that are no signal
// on either, and a signal both ignore by default.
//
// Measured by docs/plans/2026-08-23-posix-kernel-extraction/kill-arguments.c
// on Linux 6.18.5 and Darwin 25.6.0. 32 and 64 are left out: they are signals
// on Linux and not on Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    const int EINVAL = 22; // on both

    // 0 if kill(self, sig) failed with EINVAL; otherwise a code naming what it did.
    static int ExpectEinval(int sig, int code)
    {
        int result = Kill(Environment.ProcessId, sig);
        if (result != -1) return code;
        if (Marshal.GetLastPInvokeError() != EINVAL) return code + 1;
        return 0;
    }

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        // The null signal checks the target exists, and sends nothing.
        if (Kill(pid, 0) != 0) return 1;

        int failed;
        if ((failed = ExpectEinval(-1, 10)) != 0) return failed;
        if ((failed = ExpectEinval(65, 20)) != 0) return failed;
        if ((failed = ExpectEinval(1000, 30)) != 0) return failed;
        if ((failed = ExpectEinval(int.MinValue, 40)) != 0) return failed;
        if ((failed = ExpectEinval(int.MaxValue, 50)) != 0) return failed;

        // SIGWINCH is 28 on both, and discarded by default.
        if (Kill(pid, 28) != 0) return 60;

        return 0;
    }
}
