using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;

// Bytes both ways over a loopback connection through NetworkStream's ReadAsync
// and WriteAsync, which reach SystemNative_Receive and SystemNative_Send on
// sockets the SocketAsyncEngine has made non-blocking and registered with its
// epoll port or kqueue. Each read is issued before the bytes it waits for are
// written, so that it waits for the engine's wake rather than finding them;
// and one direction carries more than the registration's buffers hold, so
// that the writer waits for the reader to make room. Then the writer's socket
// closes, and the reader reads end of file.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
public static class Program
{
    public static async Task<int> Main(string[] args)
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(1);

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        var accepting = listener.AcceptAsync();
        await client.ConnectAsync((IPEndPoint)listener.LocalEndPoint!);
        using var server = await accepting;

        // The streams do not own their sockets: a stream that does shuts its
        // socket down as it closes, and `shutdown(2)` is not modelled.
        var clientStream = new NetworkStream(client, ownsSocket: false);
        var serverStream = new NetworkStream(server, ownsSocket: false);

        // A few bytes, the read waiting for them.
        var small = new byte[5];
        var reading = ReadExactly(serverStream, small);
        await clientStream.WriteAsync(new byte[] { 3, 1, 4, 1, 5 });
        if (!await reading) return 1;
        if (small[0] != 3 || small[1] != 1 || small[2] != 4 || small[3] != 1 || small[4] != 5) return 2;

        // More than the connection's buffers hold, the other way.
        const int Length = 200000;
        var payload = new byte[Length];
        for (int i = 0; i < Length; i++) payload[i] = (byte)(i * 7);

        var large = new byte[Length];
        var readingLarge = ReadExactly(clientStream, large);
        await serverStream.WriteAsync(payload);
        if (!await readingLarge) return 3;

        for (int i = 0; i < Length; i++)
        {
            if (large[i] != payload[i]) return 4;
        }

        // The writer's socket closes: the reader, waiting, reads end of file.
        var eof = clientStream.ReadAsync(new byte[1]).AsTask();
        server.Dispose();
        if (await eof != 0) return 5;

        return 0;
    }

    // False means the peer closed before the whole buffer arrived.
    private static async Task<bool> ReadExactly(Stream stream, byte[] buffer)
    {
        int read = 0;
        while (read < buffer.Length)
        {
            int n = await stream.ReadAsync(buffer.AsMemory(read));
            if (n == 0) return false;
            read += n;
        }

        return true;
    }
}
