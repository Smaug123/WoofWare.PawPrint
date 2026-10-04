using System.Net;
using System.Net.Sockets;

// `Socket.Connect` to destinations that are not a listener's plain loopback
// address: the wildcard and port 0 on a datagram socket, and a multicast group
// on a stream socket. Measured on Linux 6.18.5 and Darwin 27.0.0
// (`docs/plans/2026-08-23-posix-kernel-extraction/sockaddr-dgram-connect.c`,
// section D):
//   * a datagram connect to 0.0.0.0 at a port connects to 127.0.0.1 there,
//     binding the socket to 127.0.0.1, on both;
//   * a datagram connect to 127.0.0.1 at port 0 succeeds on Linux and is
//     EADDRNOTAVAIL on Darwin;
//   * a stream connect to 224.0.0.1 is ENETUNREACH on Linux and EAFNOSUPPORT
//     on Darwin.
//
// Exits 0 for Linux's answers, 100 for Darwin's, and the index of the first
// other answer otherwise.
class SocketConnectDestinations
{
    static SocketError Connect(Socket socket, IPAddress address, int port)
    {
        try
        {
            socket.Connect(new IPEndPoint(address, port));
            return SocketError.Success;
        }
        catch (SocketException e)
        {
            return e.SocketErrorCode;
        }
    }

    static int Main()
    {
        using var peer = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
        peer.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        int peerPort = ((IPEndPoint)peer.LocalEndPoint!).Port;

        using (var wildcard = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp))
        {
            if (Connect(wildcard, IPAddress.Any, peerPort) != SocketError.Success) return 1;
            var local = (IPEndPoint)wildcard.LocalEndPoint!;
            if (!local.Address.Equals(IPAddress.Loopback)) return 2;
            if (local.Port == 0) return 3;
        }

        SocketError portZero;
        using (var socket = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp))
            portZero = Connect(socket, IPAddress.Loopback, 0);

        SocketError group;
        using (var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
            group = Connect(socket, IPAddress.Parse("224.0.0.1"), peerPort);

        if (portZero == SocketError.Success && group == SocketError.NetworkUnreachable) return 0;
        if (portZero == SocketError.AddressNotAvailable && group == SocketError.AddressFamilyNotSupported) return 100;
        return portZero == SocketError.Success || portZero == SocketError.AddressNotAvailable ? 5 : 4;
    }
}
