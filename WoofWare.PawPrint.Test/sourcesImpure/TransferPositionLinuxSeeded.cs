using System;
using System.Runtime.InteropServices;

// A transfer that would carry the file position past INT64_MAX, under the Linux
// flavour, through the shim's own entry points.
//
// Linux answers EINVAL when position + count exceeds INT64_MAX, for `read` and
// `write` at the description's position as for `pread` and `pwrite` at the
// argument. It checks after the buffer's range and before anything the object
// does, and over the count as asked rather than as shortened to one call's
// worth (0x7FFFF000). Measured on Linux 6.18.5 aarch64 by
// docs/plans/2026-08-23-posix-kernel-extraction/transfer-counts-position.c.
// Darwin has no such check: `TransferPositionDarwinSeeded.cs` is the other half.
//
// Not differential: the host kernel is whichever the test runs on, and the two
// answer these rows differently.
//
// Every errno used here has the same number on both platforms (EBADF 9, EFAULT
// 14, EISDIR 21, EINVAL 22).
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestImpureCases): f = "hello", d an empty directory.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PWrite", SetLastError = true)]
    static extern unsafe int PWrite(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int O_RDONLY = 0x0000;
    const int O_RDWR = 0x0002;
    const int SEEK_SET = 0;
    const int SEEK_CUR = 1;

    const int EFAULT = 14;
    const int EISDIR = 21;
    const int EINVAL = 22;

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0);
    }

    static unsafe bool Rejected(int result, int errno)
    {
        return result == -1 && Marshal.GetLastSystemError() == errno;
    }

    static unsafe int Main(string[] args)
    {
        int check;
        byte* buf = stackalloc byte[16];
        const long nearTop = long.MaxValue - 10;

        IntPtr f = OpenPath("f", O_RDWR);
        check = 1;
        if (f == new IntPtr(-1)) return check;
        IntPtr d = OpenPath("d", O_RDONLY);
        check = 2;
        if (d == new IntPtr(-1)) return check;

        // --- pread: ten bytes fit below INT64_MAX, eleven do not ---
        check = 3;
        if (PRead(f, buf, 10, nearTop) != 0) return check;
        check = 4;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(f, buf, 11, nearTop), EINVAL)) return check;
        // A count of zero never overflows, even at INT64_MAX itself.
        check = 5;
        if (PRead(f, buf, 0, long.MaxValue) != 0) return check;
        check = 6;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(f, buf, 1, long.MaxValue), EINVAL)) return check;

        // Ahead of what the object does: a directory's EISDIR...
        check = 7;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(d, buf, 11, nearTop), EINVAL)) return check;
        check = 8;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(d, buf, 10, nearTop), EISDIR)) return check;
        // ...but behind the range screen, which (byte*)-1 fails at any count.
        check = 9;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(f, (byte*)(-1), 11, nearTop), EFAULT)) return check;
        // NULL passes the screen, and would fault only at the copy, which this
        // check precedes.
        check = 10;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(f, (byte*)0, 11, nearTop), EINVAL)) return check;

        // The count checked is the one asked for, not the 0x7FFFF000 one call
        // moves: both below would move the same, and only the second overflows.
        check = 11;
        if (PRead(f, buf, 0x7FFFF000, long.MaxValue - 0x7FFFF000) != 0) return check;
        check = 12;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PRead(f, buf, 0x7FFFF001, long.MaxValue - 0x7FFFF000), EINVAL)) return check;

        // --- read and write, at the description's position ---
        check = 13;
        if (LSeek(f, nearTop, SEEK_SET) != nearTop) return check;
        check = 14;
        if (Read(f, buf, 10) != 0) return check;
        check = 15;
        Marshal.SetLastSystemError(0);
        if (!Rejected(Read(f, buf, 11), EINVAL)) return check;
        check = 16;
        Marshal.SetLastSystemError(0);
        if (!Rejected(Write(f, buf, 11), EINVAL)) return check;
        // Neither moved the position.
        check = 17;
        if (LSeek(f, 0, SEEK_CUR) != nearTop) return check;

        // A directory's position, which lseek sets as freely.
        check = 18;
        if (LSeek(d, long.MaxValue - 1, SEEK_SET) != long.MaxValue - 1) return check;
        check = 19;
        Marshal.SetLastSystemError(0);
        if (!Rejected(Read(d, buf, 2), EINVAL)) return check;
        check = 20;
        Marshal.SetLastSystemError(0);
        if (!Rejected(Read(d, buf, 1), EISDIR)) return check;

        // --- pwrite, at the argument ---
        check = 21;
        Marshal.SetLastSystemError(0);
        if (!Rejected(PWrite(f, buf, 11, nearTop), EINVAL)) return check;
        check = 22;
        if (PWrite(f, buf, 0, long.MaxValue) != 0) return check;

        return 0;
    }
}
