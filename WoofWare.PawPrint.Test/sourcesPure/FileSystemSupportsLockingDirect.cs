using System;
using System.Runtime.InteropServices;

// .NET 11's `SystemNative_FileSystemSupportsLocking` (pal_io.c:1720), called
// directly. CoreLib's `SafeFileHandle` asks it only about a file it has just
// opened, so what the shim answers for a descriptor `fstatfs(2)` refuses is
// reachable only like this: 0, with errno left as `fstatfs` set it.
//
// Differential: every row is a descriptor the process does not hold, which
// both Linux and macOS refuse with EBADF (9 on both), and the shim asks
// `fstatfs` only for a shared lock under write access, so every other row is
// answered without touching errno.
//
// errno is read via `Marshal.GetLastSystemError`, through an import without
// `SetLastError`, whose stub would otherwise zero errno on the way in and hide
// whether the shim left it alone (see sourcesImpure/GetFileSystemTypeLinux.cs).
//
// .NET 10's libSystem.Native has no such export, so there the first call
// raises `EntryPointNotFoundException`, and this exits 100.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FileSystemSupportsLocking")]
    static extern uint FileSystemSupportsLocking(IntPtr fd, int lockOperation, int accessWrite);

    // PAL_LOCK_SH and PAL_LOCK_EX (pal_io.h).
    const int LOCK_SH = 1;
    const int LOCK_EX = 2;

    const int EBADF = 9;
    const int Sentinel = 4242;

    // A descriptor number no process here holds.
    static readonly IntPtr Unheld = new IntPtr(4242);

    static int Main(string[] args)
    {
        int check;
        uint answer;

        // An exclusive lock is always supported, without asking `fstatfs`.
        check = 1;
        Marshal.SetLastSystemError(Sentinel);
        try
        {
            answer = FileSystemSupportsLocking(Unheld, LOCK_EX, 1);
        }
        catch (EntryPointNotFoundException)
        {
            return 100;
        }
        if (answer != 1) return check;
        check = 2;
        if (Marshal.GetLastSystemError() != Sentinel) return check;

        // So is a shared lock without write access.
        check = 3;
        Marshal.SetLastSystemError(Sentinel);
        answer = FileSystemSupportsLocking(Unheld, LOCK_SH, 0);
        if (answer != 1) return check;
        check = 4;
        if (Marshal.GetLastSystemError() != Sentinel) return check;

        // A shared lock under write access asks `fstatfs`, which refuses an
        // unheld descriptor: the shim answers 0 and leaves its errno.
        check = 5;
        Marshal.SetLastSystemError(Sentinel);
        answer = FileSystemSupportsLocking(Unheld, LOCK_SH, 1);
        if (answer != 0) return check;
        check = 6;
        if (Marshal.GetLastSystemError() != EBADF) return check;

        // Any nonzero `accessWrite` is write access: the C tests `== 0`.
        check = 7;
        Marshal.SetLastSystemError(Sentinel);
        answer = FileSystemSupportsLocking(Unheld, LOCK_SH, -1);
        if (answer != 0) return check;
        check = 8;
        if (Marshal.GetLastSystemError() != EBADF) return check;

        return 0;
    }
}
