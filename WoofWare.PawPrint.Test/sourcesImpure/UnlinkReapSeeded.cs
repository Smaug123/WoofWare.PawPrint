using System;
using System.Runtime.InteropServices;

// What happens to an *inode* whose last name goes away, in the two cases that
// differ: nothing holds it, and something does.
//
// PawPrint-only, because the fact under test is not one a guest can read. The
// guest below only sets the state up — that a descriptor still reads its bytes
// after the name has gone is asserted in sourcesPure/UnlinkSeeded.cs, against
// the real kernel — and the registration's `AssertTerminalState` inspects the
// emulated filesystem afterwards. That is the only place the reaping rule is
// visible at all: `EmulatedKernel.forgetIfUnheld` frees an inode nothing holds,
// and a real `close(2)` frees no memory a guest can see either.
//
// At exit exactly one inode besides the root must survive, and it must be the
// one this guest still has open.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): held ("payload"), kept ("kept-bytes"),
// plain ("never opened").
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Unlink", SetLastError = true)]
    static extern unsafe int Unlink(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    // Interop.Sys.OpenFlags, and the mode CoreLib's own OpenReadOnly passes.
    const int O_RDONLY = 0x0000;
    const int DefaultCreateMode = 438;

    static unsafe void Ascii(string s, byte* dest)
    {
        for (int i = 0; i < s.Length; i++)
        {
            dest[i] = (byte)s[i];
        }

        dest[s.Length] = 0;
    }

    static unsafe IntPtr OpenPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        return Open(buf, O_RDONLY, DefaultCreateMode);
    }

    static unsafe int UnlinkPath(string path)
    {
        byte* buf = stackalloc byte[256];
        Ascii(path, buf);
        return Unlink(buf);
    }

    static unsafe int Main()
    {
        int check = 0;

        // ---- opened, unlinked, read, and closed: nothing holds it at exit ----

        IntPtr held = OpenPath("held");

        check++;
        if (held == new IntPtr(-1)) return check;

        check++;
        if (UnlinkPath("held") != 0) return check;

        byte* buffer = stackalloc byte[16];

        check++;
        if (Read(held, buffer, 16) != 7) return check;

        check++;
        if (Close(held) != 0) return check;

        // ---- never opened at all: freed by the unlink itself ----

        // The case `close` cannot cover. Removing the last name of an inode
        // nothing holds must free it there and then; if only `close` reaped,
        // this one would linger for the rest of the run and nothing a guest
        // could read would say so.
        check++;
        if (UnlinkPath("plain") != 0) return check;

        // ---- opened and unlinked, and deliberately *not* closed ----

        IntPtr kept = OpenPath("kept");

        check++;
        if (kept == new IntPtr(-1)) return check;

        check++;
        if (UnlinkPath("kept") != 0) return check;

        // Read after the name has gone, so that the descriptor is demonstrably
        // still usable at the moment the run ends.
        check++;
        if (Read(kept, buffer, 16) != 10) return check;

        return 0;
    }
}
