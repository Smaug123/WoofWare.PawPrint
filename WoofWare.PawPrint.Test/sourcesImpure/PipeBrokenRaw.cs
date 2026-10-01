using System;
using System.Runtime.InteropServices;
using System.Threading;

// A pipe made through `SystemNative_Pipe` whose read end the guest closes, and
// what `SystemNative_Write` then answers through its write end: EPIPE, ahead of
// EAGAIN (a full non-blocking pipe) and EFAULT (a bad pointer), from the main
// thread and from another. Each write raises SIGPIPE too, which the runtime's
// startup ignores, so none ends the process.
//
// A write of no bytes is where the flavours part: Linux answers 0 and raises
// nothing, Darwin answers EPIPE. That check comes last, and its answer is the
// exit code: 0 for Linux's, 100 for Darwin's. The registration runs the guest
// under both flavours and expects each its own.
//
// Impure because which of the two it is depends on the kernel the registration
// simulates, not the host.
//
// Any other exit code names the first check that failed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking", SetLastError = true)]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    // Interop.Error, as `pal_errno.h` numbers it.
    const int PalEPIPE = 0x10043;

    static unsafe int Main()
    {
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];

        // Full, so that a non-blocking write would be EAGAIN if it had a
        // reader.
        if (SetIsNonBlocking(writeEnd, 1) != 0) return 2;
        byte[] block = new byte[70000];
        fixed (byte* start = block)
        {
            while (Write(writeEnd, start, 4096) > 0) { }
            while (Write(writeEnd, start, 1) > 0) { }
        }

        if (Close(readEnd) != 0) return 3;

        byte* one = stackalloc byte[1];
        if (Write(writeEnd, one, 1) != -1) return 10;
        if (LastPalError() != PalEPIPE) return 11;

        fixed (byte* start = block)
        {
            if (Write(writeEnd, start, block.Length) != -1) return 12;
            if (LastPalError() != PalEPIPE) return 13;
        }

        if (Write(writeEnd, (byte*)8, 5) != -1) return 14;
        if (LastPalError() != PalEPIPE) return 15;

        // Blocking makes no difference: nothing could ever read.
        if (SetIsNonBlocking(writeEnd, 0) != 0) return 16;
        if (Write(writeEnd, one, 1) != -1) return 17;
        if (LastPalError() != PalEPIPE) return 18;

        // From a thread other than the main one, whose SIGPIPE Linux aims at
        // that thread rather than at the process.
        int workerAnswer = 0;
        int workerError = 0;
        Thread worker = new Thread(() =>
        {
            byte* b = stackalloc byte[1];
            workerAnswer = Write(writeEnd, b, 1);
            workerError = LastPalError();
        });
        worker.Start();
        worker.Join();
        if (workerAnswer != -1) return 20;
        if (workerError != PalEPIPE) return 21;

        int zero = Write(writeEnd, one, 0);
        int zeroError = LastPalError();

        if (Close(writeEnd) != 0) return 30;

        if (zero == 0) return 0;
        if (zero == -1 && zeroError == PalEPIPE) return 100;
        return 31;
    }
}
