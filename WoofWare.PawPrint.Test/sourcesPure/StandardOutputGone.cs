using System;
using System.Runtime.InteropServices;

// Launched with standard output on a pipe whose reader the launcher closed
// before the guest started (TestPureCases' `standardStreamsCases`): nothing will
// ever read what the guest writes there.
//
// Console.Out swallows the EPIPE each write gets, as ConsolePal.Unix's write
// loop does ("Broken pipe... pretend we were successful"), so the guest carries
// on. A write through System.Native itself sees the EPIPE. SIGPIPE is raised by
// every one of those writes, and the runtime ignores it, so none ends the
// process.
//
// Each write is longer than a pipe holds, so a launcher whose close of the
// reader raced the guest's first write still leaves the guest meeting EPIPE:
// the write blocks on the full pipe until the reader goes, and the next one
// fails.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int EPIPE = 32; // the same on Linux and Darwin

    static unsafe int Main(string[] args)
    {
        Console.Out.Write(new string('x', 70000));
        Console.Out.WriteLine("a line after it");
        Console.Out.Flush();
        Console.WriteLine("and another, through Console");

        byte[] block = new byte[70000];
        fixed (byte* p = block)
        {
            for (int i = 0; ; i++)
            {
                if (i == 10) return 1;
                int written = Write((IntPtr)1, p, block.Length);
                if (written < 0)
                {
                    if (Marshal.GetLastPInvokeError() != EPIPE) return 2;
                    break;
                }
            }
        }

        // Still swallowed after the raw write saw it.
        Console.Out.WriteLine("one more");

        // Standard error's reader is still there.
        Console.Error.WriteLine("standard error is still read");

        return 0;
    }
}
