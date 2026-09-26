using System;
using System.Runtime.InteropServices;

// The Darwin half of `TransferPositionLinuxSeeded.cs`: the same reads, which
// Darwin answers as it would at any other position past end-of-file. It has no
// check of position + count, so a read there is 0 however close to INT64_MAX
// it starts and however much it asks for, and a directory is EISDIR. Measured
// on Darwin 27.0.0 arm64 by
// docs/plans/2026-08-23-posix-kernel-extraction/transfer-counts-position.c.
//
// Not differential, for the reason the Linux half gives. No write is made:
// Darwin's writes at these positions are EFBIG, which this kernel does not
// model and refuses.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int O_RDONLY = 0x0000;
    const int SEEK_SET = 0;

    const int EISDIR = 21;

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
        byte* buf = stackalloc byte[16];
        const long nearTop = long.MaxValue - 10;

        IntPtr f = OpenPath("f", O_RDONLY);
        check = 1;
        if (f == new IntPtr(-1)) return check;
        IntPtr d = OpenPath("d", O_RDONLY);
        check = 2;
        if (d == new IntPtr(-1)) return check;

        check = 3;
        if (PRead(f, buf, 11, nearTop) != 0) return check;
        check = 4;
        if (PRead(f, buf, 1, long.MaxValue) != 0) return check;
        check = 5;
        if (PRead(f, buf, int.MaxValue, nearTop) != 0) return check;
        check = 6;
        if (PRead(f, buf, 0x7FFFF001, long.MaxValue - 0x7FFFF000) != 0) return check;
        check = 7;
        Marshal.SetLastSystemError(0);
        if (PRead(d, buf, 11, nearTop) != -1 || Marshal.GetLastSystemError() != EISDIR) return check;

        check = 8;
        if (LSeek(f, nearTop, SEEK_SET) != nearTop) return check;
        check = 9;
        if (Read(f, buf, 11) != 0) return check;

        return 0;
    }
}
