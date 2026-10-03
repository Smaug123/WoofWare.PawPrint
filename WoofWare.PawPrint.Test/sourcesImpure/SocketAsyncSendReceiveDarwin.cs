using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;

// A managed asynchronous connection over loopback on a Darwin process, carrying two
// bytes: connect, accept, send and receive, all through the async APIs, which drive
// registration with SocketAsyncEngine's kqueue, its readiness reports, and the wake.
// The receive is issued before the send, so that it has to wait for the data rather
// than find it already there. Configured as macOS, and compared with real .NET on a
// macOS host.
//
// The exit code is the index of the first check that failed; 0 means all passed.
public static class Program
{
    public static async Task<int> Main(string[] args)
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(1);
        var endpoint = (IPEndPoint)listener.LocalEndPoint!;

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        var accepting = listener.AcceptAsync();
        await client.ConnectAsync(endpoint);
        using var accepted = await accepting;

        var buffer = new byte[2];
        var receiving = ReceiveExactly(accepted, buffer);

        if (!await SendAll(client, new byte[] { 7, 35 })) return 1;
        if (!await receiving) return 2;

        // Compared in order: a sum would accept [35, 7].
        if (buffer[0] != 7 || buffer[1] != 35) return 3;

        return 0;
    }

    // A send or receive may legally move fewer bytes than asked for, so both loop.
    // False means the send made no progress, which a real send never does for a
    // non-empty buffer; without the bail-out an emulated one that did would spin.
    private static async Task<bool> SendAll(Socket socket, byte[] payload)
    {
        int sent = 0;
        while (sent < payload.Length)
        {
            int n = await socket.SendAsync(new ArraySegment<byte>(payload, sent, payload.Length - sent), SocketFlags.None);
            if (n <= 0) return false;
            sent += n;
        }

        return true;
    }

    // False means the peer closed before the whole buffer arrived.
    private static async Task<bool> ReceiveExactly(Socket socket, byte[] buffer)
    {
        int read = 0;
        while (read < buffer.Length)
        {
            int n = await socket.ReceiveAsync(new ArraySegment<byte>(buffer, read, buffer.Length - read), SocketFlags.None);
            if (n == 0) return false;
            read += n;
        }

        return true;
    }
}
