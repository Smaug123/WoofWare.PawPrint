using System;
using System.Runtime.InteropServices;

// The main thread blocks SIGABRT and raises it with libc's raise(3), which
// leaves it pending, and then fails fast. CoreCLR's abort restores SIGABRT's
// default and unblocks it, and the pending signal kills the process as the
// unblocking call returns.
//
// SIGABRT is 6 on both Linux and Darwin, so bit 5 of the set, which Darwin's
// four-byte sigset_t and Linux's eight-byte one both hold in their first byte.
// SIG_BLOCK is 0 on Linux and 1 on Darwin, where 0 is EINVAL. The kernel is
// asked rather than the CoreLib's OperatingSystem, which describes the
// platform the CoreLib was built for, not the one it runs on.
unsafe class Program
{
    const ulong Abrt = 1UL << 5;
    const int EINVAL = 22;

    [DllImport("libc", EntryPoint = "pthread_sigmask", SetLastError = true)]
    static extern int PthreadSigmask(int how, ulong* set, ulong* oldSet);

    [DllImport("libc", EntryPoint = "sigpending", SetLastError = true)]
    static extern int Sigpending(ulong* set);

    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int Main(string[] args)
    {
        ulong set = Abrt;
        int linuxBlock = PthreadSigmask(0, &set, null);
        if (linuxBlock == EINVAL)
        {
            if (PthreadSigmask(1, &set, null) != 0) return 1;
        }
        else if (linuxBlock != 0) return 2;

        if (Raise(6) != 0) return 3;

        ulong pending = 0;
        if (Sigpending(&pending) != 0) return 4;
        if ((pending & Abrt) == 0) return 5;

        Environment.FailFast("failing fast with SIGABRT pending");
        return 6;
    }
}
