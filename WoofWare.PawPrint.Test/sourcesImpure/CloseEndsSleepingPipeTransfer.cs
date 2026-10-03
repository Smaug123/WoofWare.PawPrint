using System;
using System.Runtime.InteropServices;
using System.Threading;

// A thread asleep in a pipe transfer, and another closing the descriptor the
// transfer was made through 50 ms in, the only one onto its end.
//
// First a read of an empty pipe: the closer then writes 3 bytes. Darwin ends the
// read at the close with end of file, and the read end is gone by the time the
// close returns, so the write answers EPIPE; Linux leaves the read asleep on the read
// end, which it holds, and the write's bytes complete it.
//
// Then a write of 200000 bytes into an empty pipe, asleep with 65536 of them in: the
// closer then reads until end of file. Darwin ends the write at the close with
// EPIPE, the bytes it had put in left in the pipe, so the reader takes 65536 and then
// end of file; Linux leaves the write asleep on the write end, which it holds, and
// it completes as the reader makes room.
//
// Every SIGPIPE raised is ignored, as the runtime ignores it. The exit code is 0 for
// Linux's answers and 100 for Darwin's, so the registration states each flavour's;
// any other value names the check that failed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

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
        // --- A sleeping read ---
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr readEnd = (IntPtr)fds[0];
        IntPtr writeEnd = (IntPtr)fds[1];

        int closedRead = -2;
        int wrote = -2;
        int writeError = 0;

        Thread readCloser = new Thread(() =>
        {
            Thread.Sleep(50);
            closedRead = Close(readEnd);
            Thread.Sleep(50);
            byte* three = stackalloc byte[3];
            three[0] = 1;
            three[1] = 2;
            three[2] = 3;
            wrote = Write(writeEnd, three, 3);
            writeError = ConvertErrorPlatformToPal(Marshal.GetLastPInvokeError());
        });
        readCloser.Start();

        byte* buffer = stackalloc byte[16];
        int read = Read(readEnd, buffer, 16);

        readCloser.Join();
        if (closedRead != 0) return 2;
        if (Close(writeEnd) != 0) return 3;

        bool readEndedByClose;

        if (read == 3)
        {
            if (wrote != 3) return 4;
            if (buffer[0] != 1 || buffer[2] != 3) return 5;
            readEndedByClose = false;
        }
        else
        {
            if (read != 0) return 6;
            if (wrote != -1) return 7;
            if (writeError != PalEPIPE) return 8;
            readEndedByClose = true;
        }

        // --- A sleeping write, part of it in ---
        if (Pipe(fds, 0) != 0) return 9;
        readEnd = (IntPtr)fds[0];
        writeEnd = (IntPtr)fds[1];

        int closedWrite = -2;
        long drained = 0;
        int drainFailed = 0;

        Thread writeCloser = new Thread(() =>
        {
            Thread.Sleep(50);
            closedWrite = Close(writeEnd);
            Thread.Sleep(50);
            byte[] sink = new byte[4096];
            fixed (byte* into = sink)
            {
                while (true)
                {
                    int n = Read(readEnd, into, sink.Length);
                    if (n == 0) break;
                    if (n < 0)
                    {
                        drainFailed = 1;
                        break;
                    }
                    drained += n;
                }
            }
        });
        writeCloser.Start();

        byte[] source = new byte[Large];
        int written;
        fixed (byte* start = source)
        {
            written = Write(writeEnd, start, Large);
        }
        int error = ConvertErrorPlatformToPal(Marshal.GetLastPInvokeError());

        writeCloser.Join();
        if (closedWrite != 0) return 10;
        if (drainFailed != 0) return 11;
        if (Close(readEnd) != 0) return 12;

        bool writeEndedByClose;

        if (written == Large)
        {
            if (drained != Large) return 13;
            writeEndedByClose = false;
        }
        else
        {
            if (written != -1) return 14;
            if (error != PalEPIPE) return 15;
            if (drained != 65536) return 16;
            writeEndedByClose = true;
        }

        if (readEndedByClose != writeEndedByClose) return 17;
        return readEndedByClose ? 100 : 0;
    }
}
