using System;
using System.IO;
using Microsoft.Win32.SafeHandles;

// A FileStream over descriptor 0, reading the 100000 bytes TestPureCases
// supplies for this case: more than the 64 KiB a pipe holds on Linux and on
// Darwin, so the launcher's write is still putting bytes in while the guest
// reads, and the guest must see every one, in order, before end of file.
//
// `new SafeFileHandle(0, false)` was not opened by the BCL, so nothing has
// classified it: CanSeek asks lseek, which answers ESPIPE for a pipe, and every
// read is then a read(2) of descriptor 0.
//
// The bytes are byte(i) = (i * 131 + (i >> 8) * 7) & 0xff, as the kernel
// library's supplied-pipe corpus uses.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    const int Length = 100000;

    static byte Expected(long i) => (byte)((i * 131 + (i >> 8) * 7) & 0xff);

    static int Main(string[] args)
    {
        // ownsHandle: false, so disposing the stream does not close fd 0.
        SafeFileHandle stdin = new SafeFileHandle((IntPtr)0, false);
        // bufferSize 0: no FileStream buffer, so each Read below is one read(2)
        // of the count asked for.
        using (FileStream fs = new FileStream(stdin, FileAccess.Read, 0))
        {
            if (fs.CanSeek) return 1;

            // Read sizes cycling through small, odd and larger than the pipe.
            int[] sizes = { 1, 7, 4096, 513, 65536, 100000, 3 };
            byte[] buffer = new byte[100000];
            long position = 0;
            int reads = 0;
            while (true)
            {
                int size = sizes[reads % sizes.Length];
                reads++;
                if (reads > 10000) return 2;

                int got = fs.Read(buffer, 0, size);
                if (got < 0 || got > size) return 3;
                if (got == 0) break;
                // A read returns what the pipe holds, never nothing while bytes
                // are still to come.
                for (int i = 0; i < got; i++)
                {
                    if (buffer[i] != Expected(position + i)) return 4;
                }
                position += got;
            }

            if (position != Length) return 5;
            // End of file again, and again.
            if (fs.Read(buffer, 0, 10) != 0) return 6;
            if (fs.Read(buffer, 0, 10) != 0) return 7;
        }

        return 0;
    }
}
