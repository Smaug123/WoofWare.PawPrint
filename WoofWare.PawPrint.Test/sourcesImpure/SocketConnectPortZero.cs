using System;
using System.Net;
using System.Net.Sockets;

// `Socket.Connect` to port 0 on 127.0.0.1 and on the wildcard, which reaches
// `connect(2)` with an AF_INET sockaddr whose port is 0.
//
// The flavours disagree, so the exit code says which one answered. Measured on
// Linux 6.18.5 and Darwin 27.0.0
// (`docs/plans/2026-08-23-posix-kernel-extraction/sockaddr-connect-ladder.c`,
// section Z): Linux tries the port and is refused, ECONNREFUSED; Darwin
// refuses a port of 0 before it looks at the address at all, EADDRNOTAVAIL.
//
// Exits 0 for Linux's answer to both connects, 100 for Darwin's, and the index
// of the first other answer otherwise.
class SocketConnectPortZero
{
    static int Answer(IPAddress address)
    {
        using var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

        try
        {
            socket.Connect(new IPEndPoint(address, 0));
            return 1;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.ConnectionRefused)
        {
            return 0;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.AddressNotAvailable)
        {
            return 100;
        }
        catch (SocketException)
        {
            return 2;
        }
    }

    static int Main()
    {
        int loopback = Answer(IPAddress.Loopback);
        int wildcard = Answer(IPAddress.Any);
        if (loopback != wildcard) return 3;
        return loopback;
    }
}
