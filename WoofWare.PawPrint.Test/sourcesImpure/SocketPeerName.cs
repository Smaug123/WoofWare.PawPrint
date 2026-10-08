using System;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;

// `getpeername(2)` through `SystemNative_GetPeerName`: by the managed
// `Socket.RemoteEndPoint` of a connected stream client and a connected datagram
// socket, and by hand on sockets with no peer.
//
// Measured on Linux 6.18.5 and Darwin 27.0.0
// (`docs/probes/getpeername/getpeername.c`): a socket never connected, and a
// listener, answer ENOTCONN on both; a socket whose connect was refused answers
// ENOTCONN on Linux and EINVAL on Darwin.
//
// Exits 0 for Linux's answer to the refused socket, 100 for Darwin's, and the
// index of the first check that failed otherwise.
class SocketPeerName
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPeerName")]
    static extern unsafe int GetPeerName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOTCONN = 0x10038;

    static unsafe int PeerOf(Socket socket, int declared, out int cell)
    {
        byte* address = stackalloc byte[128];
        int length = declared;
        int result = GetPeerName(socket.Handle, address, &length);
        cell = length;
        return result;
    }

    static unsafe int Main()
    {
        var loopback = new IPEndPoint(IPAddress.Loopback, 0);

        // --- a stream client reports the listener; the accepted end the client ---
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(loopback);
        listener.Listen(4);
        var listening = (IPEndPoint)listener.LocalEndPoint!;

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect(listening);
        if (!listening.Equals(client.RemoteEndPoint)) return 1;

        using var accepted = listener.Accept();
        if (!client.LocalEndPoint!.Equals(accepted.RemoteEndPoint)) return 2;

        // By hand, on the accepted end: the length reported is the whole
        // sockaddr_in whatever was declared.
        if (PeerOf(accepted, 128, out int cell) != PAL_SUCCESS) return 3;
        if (cell != 16) return 4;
        if (PeerOf(accepted, 8, out cell) != PAL_SUCCESS) return 5;
        if (cell != 16) return 6;

        // The shim's own screen of a negative length.
        if (PeerOf(accepted, -1, out cell) != PAL_EFAULT) return 7;

        // --- a connected datagram socket reports its default peer ---
        using var receiver = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
        receiver.Bind(loopback);
        var receiving = (IPEndPoint)receiver.LocalEndPoint!;
        using var datagram = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
        datagram.Connect(receiving);
        if (!receiving.Equals(datagram.RemoteEndPoint)) return 8;

        // --- no peer: never connected, and listening ---
        using var fresh = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        if (PeerOf(fresh, 16, out cell) != PAL_ENOTCONN) return 9;
        if (cell != 16) return 10;
        if (PeerOf(listener, 16, out cell) != PAL_ENOTCONN) return 11;
        using var freshDatagram = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
        if (PeerOf(freshDatagram, 16, out cell) != PAL_ENOTCONN) return 12;

        // --- a refused connect: the flavours disagree ---
        int deadPort;
        using (var placeholder = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            placeholder.Bind(loopback);
            deadPort = ((IPEndPoint)placeholder.LocalEndPoint!).Port;
        }

        using var refused = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        try
        {
            refused.Connect(new IPEndPoint(IPAddress.Loopback, deadPort));
            return 13;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.ConnectionRefused)
        {
        }

        if (refused.RemoteEndPoint != null) return 14;

        switch (PeerOf(refused, 16, out cell))
        {
            case PAL_ENOTCONN:
                return cell == 16 ? 0 : 15;
            case PAL_EINVAL:
                return cell == 16 ? 100 : 16;
            default:
                return 17;
        }
    }
}
