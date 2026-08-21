using System;
using System.Net;
using System.Net.Sockets;

// The managed synchronous rendezvous: `Socket.Connect` and `Socket.Accept`
// over loopback, single-threaded — a blocking loopback connect completes
// without a concurrent accept on both kernels, so no second thread is needed.
//
// `SocketConnect.cs` pins the PAL contract through raw P/Invoke; this guest
// pins the managed plumbing above it: `SocketPal.Connect`'s
// completed-synchronously branch (a blocking fd never sees EINPROGRESS),
// `SocketPal.Accept`, and the endpoint marshalling on both sides of the
// connection. Everything asserted is flavour-independent.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketConnectManaged
{
    static int Main()
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        var port = ((IPEndPoint)listener.LocalEndPoint).Port;
        if (port == 0) return 1;

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect(new IPEndPoint(IPAddress.Loopback, port));
        if (!client.Connected) return 2;
        var clientLocal = (IPEndPoint)client.LocalEndPoint;
        if (clientLocal.Port == 0) return 3;
        // No `client.RemoteEndPoint` here: on this BCL it reaches
        // `SystemNative_GetPeerName`, which is not modelled yet. The accepted
        // side's endpoints come from `accept(2)`'s own out-address and
        // `getsockname(2)`, both modelled.

        using var accepted = listener.Accept();
        if (((IPEndPoint)accepted.RemoteEndPoint).Port != clientLocal.Port) return 4;
        if (((IPEndPoint)accepted.LocalEndPoint).Port != port) return 5;

        // The parameterless Listen passes Int32.MaxValue as the backlog,
        // which the kernel clamps to somaxconn — a rendezvous must still
        // work through it.
        using var listener2 = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener2.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener2.Listen();
        var port2 = ((IPEndPoint)listener2.LocalEndPoint).Port;
        using var client2 = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client2.Connect(new IPEndPoint(IPAddress.Loopback, port2));
        if (!client2.Connected) return 6;
        using var accepted2 = listener2.Accept();
        if (((IPEndPoint)accepted2.RemoteEndPoint).Port != ((IPEndPoint)client2.LocalEndPoint).Port) return 7;

        return 0;
    }
}
