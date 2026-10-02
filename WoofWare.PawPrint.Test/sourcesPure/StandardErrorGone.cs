using System;
using System.Runtime.InteropServices;

// Launched with standard error on a pipe whose reader the launcher closed
// before the guest started (TestPureCases' `standardStreamsCases`), and
// standard output read as usual. StandardOutputGone.cs is the same guest the
// other way round; see there for why each write is longer than a pipe holds.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int EPIPE = 32; // the same on Linux and Darwin

    static unsafe int Main(string[] args)
    {
        Console.Error.Write(new string('x', 70000));
        Console.Error.WriteLine("a line after it");
        Console.Error.Flush();

        byte[] block = new byte[70000];
        fixed (byte* p = block)
        {
            for (int i = 0; ; i++)
            {
                if (i == 10) return 1;
                int written = Write((IntPtr)2, p, block.Length);
                if (written < 0)
                {
                    if (Marshal.GetLastPInvokeError() != EPIPE) return 2;
                    break;
                }
            }
        }

        Console.Error.WriteLine("one more");

        // Standard output's reader is still there.
        Console.Out.WriteLine("standard output is still read");

        return 0;
    }
}
