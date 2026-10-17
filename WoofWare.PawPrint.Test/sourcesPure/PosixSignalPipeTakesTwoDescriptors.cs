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
        // The comparison needs the four descriptors above `before` to be free.
        // On the real runtime they may not be: during startup another runtime
        // thread can briefly hold low descriptors while the main thread opens
        // assemblies above them, leaving a hole below descriptors that stay
        // open. A pipe made into such a hole is followed by occupied
        // descriptors. So keep each pipe that lands in a hole open, filling it,
        // until one is followed by four free descriptors.
        const int maxFills = 16;
        int* before = stackalloc int[2];
        int* above = stackalloc int[4];
        int fills = 0;
        while (true)
        {
            if (Pipe(before, 0) != 0) return 1;
            if (Pipe(above, 0) != 0) return 1;
            if (Pipe(above + 2, 0) != 0) return 1;
            bool clear = before[1] == before[0] + 1;
            for (int i = 0; i < 4; i++)
            {
                if (above[i] != before[0] + 2 + i) clear = false;
                if (Close((IntPtr)above[i]) != 0) return 2;
            }
            if (clear) break;
            if (++fills == maxFills) return 10;
        }
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
