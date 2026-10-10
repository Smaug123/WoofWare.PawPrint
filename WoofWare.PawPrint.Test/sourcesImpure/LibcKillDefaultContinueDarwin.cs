using System;
using System.Runtime.InteropServices;

// SIGCONT at its default disposition, sent by the process to itself with
// libc's kill(2) under the Darwin flavour. The process is not stopped, so
// there is nothing to resume, and the signal is consumed: the process carries
// on. Sent unblocked, and then sent while the main thread blocks it, when it
// stays pending until it is unblocked, and is then discarded.
//
// Nothing here initialises System.Native's signal handling, which would
// install its own SIGCONT handler; so SIGCONT keeps the default the runtime
// starts with.
//
// Darwin's SIGCONT is 19 (bit 18 of its 32-bit set), and it numbers SIG_BLOCK
// and SIG_UNBLOCK 1 and 2; under Linux's numbering 19 is SIGSTOP. The
// pending-while-blocked row is
// docs/plans/2026-08-23-posix-kernel-extraction/sigpending-scope.c's.
unsafe class Program
{
    const int SIGCONT = 19;
    const int SIG_BLOCK = 1;
    const int SIG_UNBLOCK = 2;
    const uint Cont = 1u << (SIGCONT - 1);

    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libc", EntryPoint = "sigprocmask", SetLastError = true)]
    static extern int Sigprocmask(int how, uint* set, uint* oldSet);

    [DllImport("libc", EntryPoint = "sigpending", SetLastError = true)]
    static extern int Sigpending(uint* set);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        if (Kill(pid, SIGCONT) != 0) return 1;
        if (Kill(pid, SIGCONT) != 0) return 2;

        uint pending = 0;
        if (Sigpending(&pending) != 0) return 3;
        if ((pending & Cont) != 0) return 4;

        uint set = Cont;
        if (Sigprocmask(SIG_BLOCK, &set, null) != 0) return 5;
        if (Kill(pid, SIGCONT) != 0) return 6;
        if (Sigpending(&pending) != 0) return 7;
        if ((pending & Cont) == 0) return 8;

        if (Sigprocmask(SIG_UNBLOCK, &set, null) != 0) return 9;
        if (Sigpending(&pending) != 0) return 10;
        if ((pending & Cont) != 0) return 11;

        return 42;
    }
}
