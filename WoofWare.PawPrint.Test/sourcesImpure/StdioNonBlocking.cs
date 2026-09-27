using System;
using System.Runtime.InteropServices;

// `O_NONBLOCK` on the three standard streams, through the entry points that
// carry it (`SystemNative_FcntlSetIsNonBlocking`, pal_io.c:655, and
// `SystemNative_FcntlGetIsNonBlocking`, pal_io.c:677), and what the flag
// changes about a stream's reads and writes.
//
// Every row was measured on Linux 6.18.5 and Darwin 27.0.0 by
// docs/plans/2026-08-23-posix-kernel-extraction/stdio-nonblock.c, under the
// launch shape the emulated kernel models: three distinct pipes, stdin's writer
// closed before the process runs, stdout and stderr drained by the launcher.
//
//   * setting the flag on each stream answers 0 and reads back;
//   * the three streams are three descriptions, so flagging one leaves the
//     others alone, while a `dup` shares its original's flag both ways;
//   * a non-blocking read of stdin is 0, end of file, as a blocking one is,
//     because the writer is gone;
//   * a short non-blocking write to stdout or stderr is taken whole.
//
// Impure because the registration reads back the bytes the writes delivered
// from the kernel's output log. The registration runs it under both flavours.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetIsNonBlocking")]
    static extern unsafe int GetIsNonBlocking(IntPtr fd, int* isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read")]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int FlagOf(int fd)
    {
        int flag = 7;
        if (GetIsNonBlocking((IntPtr)fd, &flag) != 0) return -1;
        return flag;
    }

    static unsafe int Main()
    {
        // Each stream in turn: set, read back, and see that the other two are
        // untouched.
        for (int fd = 0; fd <= 2; fd++)
        {
            if (FlagOf(fd) != 0) return 1 + fd;
            if (SetIsNonBlocking((IntPtr)fd, 1) != 0) return 4 + fd;
            if (FlagOf(fd) != 1) return 7 + fd;
            for (int other = 0; other <= 2; other++)
            {
                if (other != fd && FlagOf(other) != 0) return 10 + fd;
            }
            if (SetIsNonBlocking((IntPtr)fd, 0) != 0) return 13 + fd;
            if (FlagOf(fd) != 0) return 16 + fd;
        }

        // The flag is on the description: a dup of stdout shares it both ways.
        IntPtr duplicate = Dup((IntPtr)1);
        if (duplicate == (IntPtr)(-1)) return 20;
        if (SetIsNonBlocking((IntPtr)1, 1) != 0) return 21;
        if (FlagOf((int)duplicate) != 1) return 22;
        if (SetIsNonBlocking(duplicate, 0) != 0) return 23;
        if (FlagOf(1) != 0) return 24;
        if (Close(duplicate) != 0) return 25;

        // A non-blocking read of stdin is end of file, with and without a
        // count.
        if (SetIsNonBlocking((IntPtr)0, 1) != 0) return 30;
        byte* buffer = stackalloc byte[16];
        if (Read((IntPtr)0, buffer, 16) != 0) return 31;
        if (Read((IntPtr)0, buffer, 0) != 0) return 32;

        // A short non-blocking write to each output stream is taken whole.
        if (SetIsNonBlocking((IntPtr)1, 1) != 0) return 40;
        if (SetIsNonBlocking((IntPtr)2, 1) != 0) return 41;
        byte* out_ = stackalloc byte[3] { (byte)'o', (byte)'u', (byte)'t' };
        if (Write((IntPtr)1, out_, 3) != 3) return 42;
        byte* err = stackalloc byte[3] { (byte)'e', (byte)'r', (byte)'r' };
        if (Write((IntPtr)2, err, 3) != 3) return 43;

        // Still set afterwards: nothing above cleared it as a side effect.
        if (FlagOf(0) != 1 || FlagOf(1) != 1 || FlagOf(2) != 1) return 44;

        return 0;
    }
}
