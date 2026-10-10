using System;
using System.Net;
using System.Net.Sockets;

// A dual-mode IPv6 TCP socket talking to an IPv4 listener in the same process,
// through the managed `Socket` API: `new Socket(SocketType.Stream,
// ProtocolType.Tcp)` makes an AF_INET6 socket with IPV6_V6ONLY off, its
// connect to 127.0.0.1 goes out as `::ffff:127.0.0.1` in a 28-byte
// `sockaddr_in6`, and its endpoints read back v4-mapped.
//
// Measured on Linux 6.18.5 and Darwin 27.0.0 (`docs/probes/dual-mode/`): with
// IPV6_V6ONLY on, a connect to a v4-mapped address is ENETUNREACH on Linux and
// EAFNOSUPPORT on Darwin; and a dual-mode socket bound to `::ffff:127.0.0.1`
// shares its port with an IPv4 socket at 127.0.0.1 on Linux when both set
// SO_REUSEADDR, which the shim's bind sets on every TCP socket, and not on
// Darwin.
//
// Exits 0 for Linux's answers, 100 for Darwin's, and the index of the first
// check that failed otherwise.
class SocketDualMode
{
    static int Main()
    {
        var mapped = IPAddress.Parse("::ffff:127.0.0.1");

        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        var listening = (IPEndPoint)listener.LocalEndPoint!;

        // --- a dual-mode client, connected to the IPv4 listener ---
        using var client = new Socket(SocketType.Stream, ProtocolType.Tcp);
        if (client.AddressFamily != AddressFamily.InterNetworkV6) return 1;
        if (!client.DualMode) return 2;

        client.Connect(new IPEndPoint(IPAddress.Loopback, listening.Port));

        var remote = (IPEndPoint)client.RemoteEndPoint!;
        if (!remote.Address.Equals(mapped)) return 3;
        if (remote.Port != listening.Port) return 4;
        if (!remote.Address.IsIPv4MappedToIPv6) return 5;

        var local = (IPEndPoint)client.LocalEndPoint!;
        if (!local.Address.Equals(mapped)) return 6;
        if (local.Port == 0) return 7;

        // The listener sees an IPv4 peer: the transport is IPv4.
        using var accepted = listener.Accept();
        if (!new IPEndPoint(IPAddress.Loopback, local.Port).Equals(accepted.RemoteEndPoint)) return 8;
        if (!listening.Equals(accepted.LocalEndPoint)) return 9;

        // IPV6_V6ONLY can no longer change, and reads back off.
        try
        {
            client.DualMode = false;
            return 10;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.InvalidArgument)
        {
        }

        if (!client.DualMode) return 11;

        // --- an IPv6-only socket reaches no IPv4 peer ---
        using var v6only = new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp);
        if (v6only.DualMode) return 12;

        SocketError v6onlyAnswer;
        try
        {
            v6only.Connect(new IPEndPoint(mapped, listening.Port));
            return 13;
        }
        catch (SocketException e)
        {
            v6onlyAnswer = e.SocketErrorCode;
        }

        // --- a dual-mode socket bound to ::ffff:127.0.0.1, beside an IPv4 one ---
        using var bound = new Socket(SocketType.Stream, ProtocolType.Tcp);
        bound.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        var boundAt = (IPEndPoint)bound.LocalEndPoint!;
        if (!boundAt.Address.Equals(mapped)) return 14;
        if (boundAt.Port == 0) return 15;

        bool shared;
        using var beside = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        try
        {
            beside.Bind(new IPEndPoint(IPAddress.Loopback, boundAt.Port));
            shared = true;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.AddressAlreadyInUse)
        {
            shared = false;
        }

        if (v6onlyAnswer == SocketError.NetworkUnreachable && shared) return 0;
        if (v6onlyAnswer == SocketError.AddressFamilyNotSupported && !shared) return 100;
        return 16;
    }
}
