using System;
using System.Runtime.InteropServices;
using System.Threading;

// Two threads over a pipe made with `SystemNative_Pipe`, its ends blocking:
//
//   * a thread reads the empty pipe while the main thread sleeps, then writes
//     "abc" and "def": the reader sleeps until bytes arrive, and reads all six
//     in order, however the writes and reads interleave;
//   * a thread writes 200000 bytes, more than a pipe holds, in one call, while
//     the main thread sleeps and then drains it: the write sleeps until there
//     is room and returns the whole count, and every byte arrives in order;
//   * a thread reads the empty pipe while the main thread sleeps, then closes
//     the write end: the reader wakes at end of file.
//
// Each holds on both kernels whichever thread runs when, so the real runtime
// is the oracle.
//
// The exit code names the first check that failed; 0 means all passed.
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

    const int Large = 200000;

    static unsafe bool MakePipe(out IntPtr readEnd, out IntPtr writeEnd)
    {
        int* fds = stackalloc int[2];
        readEnd = IntPtr.Zero;
        writeEnd = IntPtr.Zero;
        if (Pipe(fds, 0) != 0) return false;
        readEnd = (IntPtr)fds[0];
        writeEnd = (IntPtr)fds[1];
        return true;
    }

    static byte Pattern(int i) => (byte)(i % 251);

    static unsafe int ReaderWakesOnBytes()
    {
        if (!MakePipe(out IntPtr readEnd, out IntPtr writeEnd)) return 1;

        byte[] received = new byte[6];
        int got = 0;
        int failed = 0;

        Thread reader = new Thread(() =>
        {
            fixed (byte* start = received)
            {
                while (got < 6)
                {
                    int n = Read(readEnd, start + got, 6 - got);
                    if (n <= 0)
                    {
                        failed = n == 0 ? 1 : 2;
                        return;
                    }
                    got += n;
                }
            }
        });
        reader.Start();

        Thread.Sleep(50);
        byte* abc = stackalloc byte[3] { (byte)'a', (byte)'b', (byte)'c' };
        if (Write(writeEnd, abc, 3) != 3) return 2;
        Thread.Sleep(50);
        byte* def = stackalloc byte[3] { (byte)'d', (byte)'e', (byte)'f' };
        if (Write(writeEnd, def, 3) != 3) return 3;

        reader.Join();
        if (failed != 0) return 3 + failed;
        if (got != 6) return 6;
        string text = System.Text.Encoding.ASCII.GetString(received);
        if (text != "abcdef") return 7;

        if (Close(writeEnd) != 0) return 8;
        if (Close(readEnd) != 0) return 9;
        return 0;
    }

    static unsafe int WriterWaitsForRoom()
    {
        if (!MakePipe(out IntPtr readEnd, out IntPtr writeEnd)) return 1;

        byte[] source = new byte[Large];
        for (int i = 0; i < Large; i++) source[i] = Pattern(i);
        int written = -2;

        Thread writer = new Thread(() =>
        {
            fixed (byte* start = source)
            {
                written = Write(writeEnd, start, Large);
            }
        });
        writer.Start();

        Thread.Sleep(50);
        byte[] chunk = new byte[10000];
        int total = 0;
        fixed (byte* start = chunk)
        {
            while (total < Large)
            {
                int n = Read(readEnd, start, chunk.Length);
                if (n <= 0) return 2;
                for (int i = 0; i < n; i++)
                {
                    if (chunk[i] != Pattern(total + i)) return 3;
                }
                total += n;
            }
        }

        writer.Join();
        if (written != Large) return 4;
        if (total != Large) return 5;

        if (Close(writeEnd) != 0) return 6;
        if (Close(readEnd) != 0) return 7;
        return 0;
    }

    static unsafe int ReaderWakesAtEndOfFile()
    {
        if (!MakePipe(out IntPtr readEnd, out IntPtr writeEnd)) return 1;

        int result = -2;
        Thread reader = new Thread(() =>
        {
            byte* buffer = stackalloc byte[8];
            result = Read(readEnd, buffer, 8);
        });
        reader.Start();

        Thread.Sleep(50);
        if (Close(writeEnd) != 0) return 2;

        reader.Join();
        if (result != 0) return 3;
        if (Close(readEnd) != 0) return 4;
        return 0;
    }

    static int Main()
    {
        int r = ReaderWakesOnBytes();
        if (r != 0) return 10 + r;
        r = WriterWaitsForRoom();
        if (r != 0) return 30 + r;
        r = ReaderWakesAtEndOfFile();
        if (r != 0) return 50 + r;
        return 0;
    }
}
