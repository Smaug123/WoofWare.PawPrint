using System;
using System.Runtime.InteropServices;

// The rows a *Darwin*-flavoured kernel answers differently, which is the half of
// the write path `sourcesImpure/PWriteRawSeeded.cs` cannot reach: that case runs
// against the default Linux flavour, so the Darwin arms of the handlers have no
// other witness.
//
// Two distinct tie-breaks, both measured on macOS 26.6 and Linux 6.18.5:
//
//   1. **Unwritability versus unseekability**, for a descriptor that is neither.
//      PawPrint models the standard streams as pipes, so fd 0 is a pipe's read
//      end:
//
//        descriptor                        Linux    Darwin
//        pipe write end (unseekable)       ESPIPE   ESPIPE
//        pipe read end (also unwritable)   ESPIPE   EBADF
//
//   2. **The access mode versus a negative offset**, where `pread` and `pwrite`
//      differ *from each other* on Darwin and agree on Linux:
//
//        call                              Linux    Darwin
//        pread(wronly, buf, 4, -1)         EINVAL   EBADF
//        pwrite(rdonly, buf, 4, -1)        EINVAL   EINVAL
//
//      Darwin's `pread` resolves the descriptor, its seekability and its access
//      mode before it looks at the offset; its `pwrite` checks the offset first,
//      as Linux does for both. The second row is the control: without it, "the
//      offset is checked first" and "the offset is checked first only on Linux"
//      are indistinguishable.
//
// Every errno used here has the same number on both platforms (EBADF 9, EINVAL
// 22, ESPIPE 29), so the constants below need no flavour of their own — unlike
// ENAMETOOLONG, which is 36 on Linux and 63 on Darwin.
//
// Note this puts a Linux-flavoured CoreLib against a Darwin-claiming kernel when
// CI runs it, as `SpliceLengthSeeded.cs` already does. Nothing here reads a
// platform-split BCL path.
//
// errno is read via Marshal.GetLastSystemError, the slot the syscall itself
// writes, rather than the P/Invoke slot the SetLastError stub copies it into.
// The imports below declare that flag, so the two agree; what makes them agree
// is pinned by sourcesPure/PInvokeSetLastError.cs.
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestImpureCases): f = "hello".
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PWrite", SetLastError = true)]
    static extern unsafe int PWrite(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;

    const int EBADF = 9;
    const int EINVAL = 22;
    const int ESPIPE = 29;

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0);
    }

    static unsafe int Main(string[] args)
    {
        int check;
        byte* buf = stackalloc byte[8];
        buf[0] = (byte)'A';

        IntPtr wo = OpenPath("f", O_WRONLY);
        check = 1;
        if (wo == new IntPtr(-1)) return check;
        IntPtr ro = OpenPath("f", O_RDONLY);
        check = 2;
        if (ro == new IntPtr(-1)) return check;

        // --- 1: which of the two failures wins for a pipe ---

        // stdin is unwritable *and* unseekable, and Darwin lets unwritability
        // win. This is the row Linux answers ESPIPE for.
        check = 3;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(0), buf, 1, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // stdout is writable and merely unseekable, so only one test fails and
        // both platforms answer the same. The control: without it, "Darwin always
        // answers EBADF for a pipe" would pass too.
        check = 4;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(1), buf, 1, 0) != -1 || Marshal.GetLastSystemError() != ESPIPE) return check;

        // The mirror, on the read side: stdout is unreadable as well as
        // unseekable, so Darwin answers EBADF there and ESPIPE for stdin.
        check = 5;
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(1), buf, 1, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 6;
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(0), buf, 1, 0) != -1 || Marshal.GetLastSystemError() != ESPIPE) return check;

        // --- 2: the access mode versus a negative offset ---

        // Darwin's `pread` settles the descriptor's access mode before it looks
        // at the offset, so this is EBADF where Linux answers EINVAL.
        check = 7;
        Marshal.SetLastSystemError(0);
        if (PRead(wo, buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // Its `pwrite` does not: the offset comes first, as on Linux. This is the
        // row that makes the pair a genuine difference between the two syscalls
        // rather than a property of the platform.
        check = 8;
        Marshal.SetLastSystemError(0);
        if (PWrite(ro, buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // ...and likewise ahead of an unseekable descriptor, and of a bad one.
        check = 9;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(0), buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;
        check = 10;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(4242), buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // The single-fault controls, which are unanimous: the access mode alone,
        // and the offset alone.
        check = 11;
        Marshal.SetLastSystemError(0);
        if (PWrite(ro, buf, 4, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 12;
        Marshal.SetLastSystemError(0);
        if (PRead(wo, buf, 4, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 13;
        Marshal.SetLastSystemError(0);
        if (PWrite(wo, buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // And the write itself still works on a Darwin-flavoured kernel: the
        // platform decides errno ordering, not whether bytes move.
        check = 14;
        if (PWrite(wo, buf, 1, 0) != 1) return check;
        check = 15;
        if (Write(wo, buf, 1) != 1) return check;

        return 0;
    }
}
