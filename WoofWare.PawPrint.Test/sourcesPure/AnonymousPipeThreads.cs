using System;
using System.IO;
using System.IO.Pipes;
using System.Threading;

// `AnonymousPipeServerStream` and its client in one process, used from two
// threads. On Unix the BCL makes the pipe with `SystemNative_Pipe` and moves
// bytes through a `Socket` wrapped round each end, whose `Receive` and `Send`
// fall back to `read` and `write` on a descriptor that is no socket, so these
// are blocking pipe transfers:
//
//   * a thread reads the client end while the main thread sleeps, then writes
//     two 3-byte chunks to the server end: the read sleeps until bytes arrive;
//   * a thread writes 70000 bytes, more than a pipe holds, to the server end
//     while the main thread sleeps and then reads the client end: the write
//     sleeps until there is room, and every byte arrives in order.
//
// Each holds whichever thread runs when, so the real runtime is the oracle.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    const int Large = 70000;

    static byte Pattern(int i) => (byte)(i % 251);

    static int Main()
    {
        using (var server = new AnonymousPipeServerStream(PipeDirection.Out))
        using (var client = new AnonymousPipeClientStream(PipeDirection.In, server.ClientSafePipeHandle))
        {
            byte[] got = new byte[6];
            int total = 0;
            var reader = new Thread(() =>
            {
                while (total < 6)
                {
                    int n = client.Read(got, total, 6 - total);
                    if (n <= 0) return;
                    total += n;
                }
            });
            reader.Start();
            Thread.Sleep(50);
            server.Write(new byte[] { 1, 2, 3 }, 0, 3);
            server.Write(new byte[] { 4, 5, 6 }, 0, 3);
            reader.Join();
            if (total != 6) return 1;
            for (int i = 0; i < 6; i++)
            {
                if (got[i] != i + 1) return 2;
            }
        }

        using (var server = new AnonymousPipeServerStream(PipeDirection.Out))
        using (var client = new AnonymousPipeClientStream(PipeDirection.In, server.ClientSafePipeHandle))
        {
            byte[] source = new byte[Large];
            for (int i = 0; i < Large; i++) source[i] = Pattern(i);
            var writer = new Thread(() => server.Write(source, 0, Large));
            writer.Start();
            Thread.Sleep(50);

            byte[] chunk = new byte[16384];
            int total = 0;
            while (total < Large)
            {
                int n = client.Read(chunk, 0, chunk.Length);
                if (n <= 0) return 3;
                for (int i = 0; i < n; i++)
                {
                    if (chunk[i] != Pattern(total + i)) return 4;
                }
                total += n;
            }

            writer.Join();
            if (total != Large) return 5;
        }

        return 0;
    }
}
