using System;
using System.Net;
using System.Net.Sockets;

// An abortive close of a loopback connection to the guest's own listener, as
// both flavours' real runtimes agree on it: a client lingering for no time
// resets the connection when it closes, rather than shutting it down in
// order.
//
// First pair: the client sends 100 bytes, sets `LingerState` to {1, 0} and
// closes. The server reads the 100 bytes, then its receive throws with
// SocketError.ConnectionReset, and a later one answers 0; its send throws. A
// fresh socket binds the client's endpoint at once, since the reset freed it.
// Second pair: `Close(0)`, which closes abortively whatever the linger, does
// the same to a server that has read nothing.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    static (Socket client, Socket server) Pair(Socket listener)
    {
        var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect((IPEndPoint)listener.LocalEndPoint!);
        var server = listener.Accept();
        server.Blocking = false;
        return (client, server);
    }

    // Waits for something to read, as a real Darwin kernel's loopback
    // delivers on a thread of its own.
    static bool Readable(Socket socket) => socket.Poll(5_000_000, SelectMode.SelectRead);

    static bool ReceiveThrowsReset(Socket socket, byte[] buffer)
    {
        try
        {
            socket.Receive(buffer);
            return false;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.ConnectionReset)
        {
            return true;
        }
    }

    static bool SendThrows(Socket socket)
    {
        try
        {
            socket.Send(new byte[] { 1, 2, 3 });
            return false;
        }
        catch (SocketException)
        {
            return true;
        }
    }

    static int Main()
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        var buffer = new byte[4096];

        var (client, server) = Pair(listener);
        var clientEndPoint = (IPEndPoint)client.LocalEndPoint!;
        if (client.Send(new byte[100]) != 100) return 1;
        client.LingerState = new LingerOption(true, 0);
        client.Close();

        if (!Readable(server)) return 2;
        if (server.Receive(buffer) != 100) return 3;
        if (!ReceiveThrowsReset(server, buffer)) return 4;
        if (server.Receive(buffer) != 0) return 5;
        if (!SendThrows(server)) return 6;

        using (var fresh = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            try
            {
                fresh.Bind(clientEndPoint);
            }
            catch (SocketException)
            {
                return 7;
            }
        }

        server.Close();

        (client, server) = Pair(listener);
        client.Close(0);

        if (!Readable(server)) return 8;
        if (!ReceiveThrowsReset(server, buffer)) return 9;
        if (server.Receive(buffer) != 0) return 10;
        server.Close();

        return 0;
    }
}
