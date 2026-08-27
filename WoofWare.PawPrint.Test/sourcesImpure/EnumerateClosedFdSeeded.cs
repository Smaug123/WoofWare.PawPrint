using System;
using System.Runtime.InteropServices;

// A guest that closes a directory stream's own file descriptor behind its back,
// and then removes the directory and closes the stream.
//
// **PawPrint only**, and not because of a flavour: `closedir(3)` on a stream
// whose descriptor has already been closed is undefined behaviour on a real
// libc -- it calls `close` on that number, which by then may belong to something
// else -- so there is no oracle to compare against and running it for real could
// take the process down.
//
// It is here because the *interpreter* must not be the thing that breaks. The
// descriptor `opendir` takes is an ordinary one, so its number is guessable, and
// a guest may legally close it. When that happens the stream is the only thing
// left holding the directory's inode; removing the stream at `CloseDir` drops
// that last hold, and if the reap does not happen there it happens nowhere --
// `close` reaps only the descriptor it actually closed, and here there is
// none. The result would be an inode no path reaches, which is PawPrint's
// bookkeeping at fault rather than the guest's: `AssertTerminalState` checks
// exactly that.
//
// The stream's descriptor number is *derived*, not assumed: fds are handed out
// lowest-free, so the one an `open` returns immediately afterwards is one above
// the stream's.
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestImpureCases.enumerateClosedFdSeed): gone/ (an empty directory)
// and f (a file).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_OpenDir", SetLastError = true)]
    static extern unsafe IntPtr OpenDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseDir", SetLastError = true)]
    static extern int CloseDir(IntPtr dir);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_RmDir", SetLastError = true)]
    static extern unsafe int RmDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    const int PAL_EBADF = 0x10008;
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

    static unsafe int Main()
    {
        byte* gone = stackalloc byte[16];
        Ascii("gone", gone);
        byte* file = stackalloc byte[16];
        Ascii("f", file);

        IntPtr stream = OpenDir(gone);
        if (stream == IntPtr.Zero) return 1;

        // One above the stream's, since descriptors are handed out lowest-free.
        IntPtr probe = Open(file, O_RDONLY, DefaultCreateMode);
        if (probe == new IntPtr(-1)) return 2;
        if (Close(probe) != 0) return 3;

        IntPtr streamFd = probe - 1;
        if (Close(streamFd) != 0) return 4;

        // Now nothing but the stream holds the directory, and this removes its
        // last name.
        if (RmDir(gone) != 0) return 5;

        // `closedir` calls `close` on a descriptor that is no longer open, so it
        // reports EBADF -- which is what a real one would do too, for whatever
        // that is worth on a path this far into undefined behaviour.
        Marshal.SetLastSystemError(0);
        if (CloseDir(stream) != -1) return 6;
        if (ConvertErrorPlatformToPal(Marshal.GetLastSystemError()) != PAL_EBADF) return 7;

        return 0;
    }
}
