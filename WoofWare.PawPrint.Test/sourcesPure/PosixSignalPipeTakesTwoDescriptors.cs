using System;
using System.Runtime.InteropServices;

// Initialising System.Native's signal handling -- which registering the first
// PosixSignalRegistration does -- makes the shim's signal pipe with pipe(),
// and that takes the two lowest free descriptors. Every descriptor handed out
// afterwards is two higher than it would have been.
//
// The absolute numbers differ between runtimes (a real process has the
// runtime's own descriptors open), so this compares a pipe made before the
// registration with one made after, in the same process.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    static unsafe int Main(string[] args)
    {
        int* before = stackalloc int[2];
        if (Pipe(before, 0) != 0) return 1;
        if (before[1] != before[0] + 1) return 2;
        if (Close((IntPtr)before[0]) != 0) return 3;
        if (Close((IntPtr)before[1]) != 0) return 4;

        using var registration = PosixSignalRegistration.Create(PosixSignal.SIGWINCH, _ => { });

        int* after = stackalloc int[2];
        if (Pipe(after, 0) != 0) return 5;
        if (after[0] != before[0] + 2) return 6;
        if (after[1] != before[1] + 2) return 7;

        // A second registration makes no second signal pipe.
        using var another = PosixSignalRegistration.Create(PosixSignal.SIGCONT, _ => { });

        int* again = stackalloc int[2];
        if (Pipe(again, 0) != 0) return 8;
        if (again[0] != after[1] + 1) return 9;

        return 0;
    }
}
