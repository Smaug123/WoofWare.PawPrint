using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;

// A managed asynchronous connect over loopback on a Darwin process, with no data
// carried: one that completes, accepted through `AcceptAsync`, and one refused.
// Darwin's `ConnectAsync` registers the socket with SocketAsyncEngine's kqueue and,
// once its WRITE filter fires, completes through `SocketPal.TryCompleteConnect`,
// which polls the socket for POLLOUT before reading SO_ERROR: a completed connect
// answers OUT, a refused one HUP alone. Configured as macOS, and compared with real
// .NET on a macOS host.
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
        if (!client.Connected) return 1;

        using var accepted = await accepting;
        if (!accepted.Connected) return 2;

        // The accepted socket is on the listener's port, and the client on another.
        if (((IPEndPoint)accepted.LocalEndPoint!).Port != endpoint.Port) return 3;
        if (((IPEndPoint)client.LocalEndPoint!).Port == endpoint.Port) return 4;

        // A port with nothing listening on it.
        int deadPort;
        using (var dead = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            dead.Bind(new IPEndPoint(IPAddress.Loopback, 0));
            deadPort = ((IPEndPoint)dead.LocalEndPoint!).Port;
        }

        using var refused = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        try
        {
            await refused.ConnectAsync(new IPEndPoint(IPAddress.Loopback, deadPort));
            return 5;
        }
        catch (SocketException e)
        {
            if (e.SocketErrorCode != SocketError.ConnectionRefused) return 6;
        }

        if (refused.Connected) return 7;

        return 0;
    }
}
