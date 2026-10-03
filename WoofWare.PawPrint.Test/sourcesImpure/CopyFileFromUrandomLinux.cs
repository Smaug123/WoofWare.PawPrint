using System;
using System.Runtime.InteropServices;

// `SystemNative_CopyFile` from /dev/null and then from /dev/urandom, with the
// length hint 0 that `fstat` gives for a device, so the shim goes straight to
// its read/write loop. From /dev/null the loop ends at once. From /dev/urandom
// into a descriptor not open for writing it ends at the first write, with
// EBADF. From /dev/urandom into one that takes every write it never ends on a
// real Linux machine, and PawPrint refuses it rather than loop inside one
// native call. Run by TestImpureCases, which expects the rows before it to
// answer, and then, given an argument, the refusal.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CopyFile", SetLastError = true)]
    static extern int CopyFile(IntPtr source, IntPtr destination, long sourceLength);

    static unsafe IntPtr OpenPath(string path, int flags)
    {
        byte[] bytes = System.Text.Encoding.ASCII.GetBytes(path + "\0");
        fixed (byte* p = bytes) return Open(p, flags, 0);
    }

    static int Main(string[] args)
    {
        // PAL OpenFlags: O_RDONLY 0, O_WRONLY 1.
        IntPtr nul = OpenPath("/dev/null", 0);
        IntPtr sink = OpenPath("/dev/null", 1);
        IntPtr urandom = OpenPath("/dev/urandom", 0);
        if (nul == (IntPtr)(-1) || sink == (IntPtr)(-1) || urandom == (IntPtr)(-1)) return 1;

        if (CopyFile(nul, sink, 0) != 0) return 2;

        // A copy that fails at its first write ends, and answers its errno:
        // the source is readable, the destination not open for writing.
        if (CopyFile(urandom, nul, 0) != -1) return 3;
        if (Marshal.GetLastPInvokeError() != 9) return 4;

        // With no argument the run ends here, so that the rows above are seen
        // to answer rather than be refused.
        if (args.Length == 0) return 0;

        CopyFile(urandom, sink, 0);
        return 5;
    }
}
