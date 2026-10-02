using System;
using System.Runtime.InteropServices;
using System.Threading;

// The reader of a pipe leaves while the main thread is asleep in a write of
// more than the pipe holds, after 65536 bytes of it have gone in.
//
// Linux returns the count the write had put in; Darwin answers EPIPE, the
// count lost. Both raise SIGPIPE, which the runtime ignores. The exit code is
// 0 for the count and 100 for EPIPE, so the registration states each flavour's
// answer; any other value names the check that failed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, as `pal_errno.h` numbers it.
    const int PalEPIPE = 0x10043;
    const int Large = 200000;

    static unsafe int Main()
    {
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];

        int closed = -2;
        Thread leaver = new Thread(() =>
        {
            Thread.Sleep(50);
            closed = Close(readEnd);
        });
        leaver.Start();

        byte[] source = new byte[Large];
        int rv;
        fixed (byte* start = source)
        {
            rv = Write(writeEnd, start, Large);
        }
        int error = ConvertErrorPlatformToPal(Marshal.GetLastPInvokeError());

        leaver.Join();
        if (closed != 0) return 2;
        if (Close(writeEnd) != 0) return 3;

        if (rv == 65536) return 0;
        if (rv != -1) return 4;
        if (error != PalEPIPE) return 5;
        return 100;
    }
}
