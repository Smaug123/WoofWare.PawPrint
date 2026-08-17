using System;
using System.Runtime.InteropServices;

// The socket event port rows on which Darwin disagrees with Linux, under the
// macOS preset. The Linux column -- and the descriptor mechanics, which are
// flavour-independent -- are in the sibling SocketEventPortLinux.cs.
//
// Configured as macOS for the same reason WriteDarwinSeeded.cs is: on the
// default kernel these rows have different answers, so no single flavour
// exercises both files. Of the operations a port supports, only pread and pwrite
// agree across the two kernels, and they are asserted in the Linux file.
//
// `flock` is deliberately absent. Measured, Darwin refuses it on a kqueue with
// ENOTSUP where Linux takes the lock -- but PawPrint refuses the whole of
// Darwin's `flock` rather than modelling it (see SystemNative_FLock's
// `refuseDarwin`), so calling it here would abort the interpreter rather than
// return an errno. That refusal is asserted where the others are, not here.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int PAL_SUCCESS = 0;

    // Darwin's numbering, which is what the macOS preset reports.
    const int ENXIO = 6;
    const int ESPIPE = 29;

    static unsafe long Create()
    {
        IntPtr port;
        int error = CreateSocketEventPort(&port);
        return error == PAL_SUCCESS ? (long)port : -error;
    }

    static unsafe int Main(string[] args)
    {
        int check = 0;
        byte* buf = stackalloc byte[8];

        check = 1;
        long p = Create();
        if (p != 3) return check;

        // ---- read/write: ENXIO, where Linux answers EINVAL. Not EBADF, which
        // would mean the description lacked the access mode.
        check = 2;
        if (Read((IntPtr)p, buf, 8) != -1) return check;
        check = 3;
        if (Marshal.GetLastSystemError() != ENXIO) return check;

        check = 4;
        if (Write((IntPtr)p, buf, 8) != -1) return check;
        check = 5;
        if (Marshal.GetLastSystemError() != ENXIO) return check;

        // The wrong-object-kind answer precedes the buffer screen here too.
        check = 6;
        if (Read((IntPtr)p, (byte*)(-1), 8) != -1) return check;
        check = 7;
        if (Marshal.GetLastSystemError() != ENXIO) return check;

        // ---- lseek: a kqueue is simply not seekable, where an epoll
        // descriptor is seekable-but-inert. Every whence, including the ones
        // Linux answers with 0.
        check = 8;
        if (LSeek((IntPtr)p, 0, 0) != -1) return check;
        check = 9;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        check = 10;
        if (LSeek((IntPtr)p, 0, 2) != -1) return check;
        check = 11;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        // Unseekability is settled before `whence` is validated -- the ordering
        // Darwin already uses for pipes. An implementation that checked whence
        // first would answer EINVAL here.
        check = 12;
        if (LSeek((IntPtr)p, 0, 99) != -1) return check;
        check = 13;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        check = 14;
        if (LSeek((IntPtr)p, -1, 99) != -1) return check;
        check = 15;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        return 0;
    }
}
