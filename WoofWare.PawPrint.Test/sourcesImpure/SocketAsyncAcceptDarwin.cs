using System;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;

// A managed asynchronous accept over loopback, on a Darwin process, whose socket
// event port is a kqueue. Socket.AcceptAsync finds nothing queued, so it registers
// the listener with SocketAsyncEngine's kqueue (EV_ADD|EV_CLEAR|EV_RECEIPT on
// EVFILT_READ and EVFILT_WRITE) and waits for the listener's READ to report the
// connection a blocking connect then queues. Configured as macOS, and compared with
// real .NET on a macOS host.
//
// The connects are blocking on purpose: an asynchronous one completes through
// SocketPal.TryCompleteConnect, which polls the socket, and this kernel models no
// Darwin poll(2) (`SocketAsyncSendReceiveDarwin.cs`, parked, stops there).
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

        // Issued first, so that it has to wait for the connection rather than find it
        // already queued.
        var accepting = listener.AcceptAsync();
        if (accepting.IsCompleted) return 1;

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect(endpoint);

        using var accepted = await accepting;
        if (!accepted.Connected) return 2;

        var clientLocal = (IPEndPoint)client.LocalEndPoint!;
        var acceptedRemote = (IPEndPoint)accepted.RemoteEndPoint!;
        if (clientLocal.Port != acceptedRemote.Port) return 3;
        if (!acceptedRemote.Address.Equals(IPAddress.Loopback)) return 4;
        if (((IPEndPoint)accepted.LocalEndPoint!).Port != endpoint.Port) return 5;

        // A second round on the same listener, which is registered already: the accept
        // waits on the registration the first one made, whose READ has reported and
        // been cleared.
        var acceptingAgain = listener.AcceptAsync();
        if (acceptingAgain.IsCompleted) return 6;

        using var second = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        second.Connect(endpoint);
        using var acceptedAgain = await acceptingAgain;
        if (((IPEndPoint)acceptedAgain.RemoteEndPoint!).Port != ((IPEndPoint)second.LocalEndPoint!).Port) return 7;

        return 0;
    }
}
