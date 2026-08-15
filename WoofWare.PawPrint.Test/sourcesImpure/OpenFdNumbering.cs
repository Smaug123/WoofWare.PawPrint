using System;
using System.Runtime.InteropServices;

// The descriptor-allocation rule: open(2) returns the lowest non-negative
// descriptor not currently in use.
//
// PawPrint-only, and necessarily so. The rule itself is POSIX and holds on both
// runtimes, but the *numbers* do not: PawPrint's simulated process holds
// exactly stdin, stdout and stderr, so its first open is fd 3, while the
// oracle's process has the runtime's own descriptors open -- the assembly it is
// executing, at minimum -- and the number it gets is unpredictable. A
// differential guest could only assert ">= 0", which no wrong allocator fails.
// The exact numbers are what distinguishes "lowest free" from "one more than
// the highest ever used", so they are asserted here against the emulated
// kernel, whose descriptor table is fully determined by KernelConfig.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f, g, h -- three regular files, so three
// descriptors can be live at once.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    const int O_RDONLY = 0x0000;

    static unsafe long OpenPath(string path)
    {
        byte* buf = stackalloc byte[64];

        for (int i = 0; i < path.Length; i++)
        {
            buf[i] = (byte)path[i];
        }

        buf[path.Length] = 0;
        return (long)Open(buf, O_RDONLY, 0);
    }

    static int Main(string[] args)
    {
        int check = 0;

        // stdin, stdout and stderr are 0, 1 and 2, so the first file opens at
        // 3. Nothing else in the simulated process holds a descriptor.
        check = 1;
        long a = OpenPath("f");
        if (a != 3) return check;

        check = 2;
        long b = OpenPath("g");
        if (b != 4) return check;

        check = 3;
        long c = OpenPath("h");
        if (c != 5) return check;

        // Freeing a descriptor in the middle leaves a gap, and the next open
        // fills the gap rather than continuing upwards. This is the check that
        // a max-plus-one allocator fails: it would answer 6.
        check = 4;
        if (Close((IntPtr)b) != 0) return check;
        check = 5;
        long d = OpenPath("g");
        if (d != 4) return check;

        // Two gaps: the lower one goes first.
        check = 6;
        if (Close((IntPtr)a) != 0) return check;
        check = 7;
        if (Close((IntPtr)d) != 0) return check;
        check = 8;
        if (OpenPath("f") != 3) return check;
        check = 9;
        if (OpenPath("g") != 4) return check;

        return 0;
    }
}
