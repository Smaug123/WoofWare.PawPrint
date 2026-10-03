using System.Net;
using System.Net.Sockets;

// `Socket.Bind` to a multicast address and to the broadcast address, under
// Darwin, which reaches `bind(2)` with an AF_INET sockaddr naming each.
//
// Measured on Darwin 27.0.0
// (`docs/plans/2026-08-23-posix-kernel-extraction/sockaddr-bind-ladder.c`,
// sections M and Z): a stream socket answers EAFNOSUPPORT for both, and a
// datagram socket EADDRNOTAVAIL for the broadcast address. Linux binds all
// three, which the kernel library refuses to record, so this guest is Darwin's
// alone.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketBindGroupAddressDarwin
{
    static SocketError Bind(SocketType type, ProtocolType protocol, IPAddress address)
    {
        using var socket = new Socket(AddressFamily.InterNetwork, type, protocol);

        try
        {
            socket.Bind(new IPEndPoint(address, 0));
            return SocketError.Success;
        }
        catch (SocketException e)
        {
            return e.SocketErrorCode;
        }
    }

    static int Main()
    {
        var multicast = IPAddress.Parse("224.0.0.1");

        if (Bind(SocketType.Stream, ProtocolType.Tcp, multicast) != SocketError.AddressFamilyNotSupported) return 1;
        if (Bind(SocketType.Stream, ProtocolType.Tcp, IPAddress.Broadcast) != SocketError.AddressFamilyNotSupported) return 2;
        if (Bind(SocketType.Dgram, ProtocolType.Udp, IPAddress.Broadcast) != SocketError.AddressNotAvailable) return 3;

        return 0;
    }
}
