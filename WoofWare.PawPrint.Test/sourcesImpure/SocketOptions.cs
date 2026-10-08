using System;
using System.Net;
using System.Net.Sockets;

// `TCP_NODELAY`, `IPV6_V6ONLY` and `SO_LINGER` through the managed `Socket`
// API: `NoDelay`, `DualMode`, `LingerState`, and `Get`/`SetSocketOption`, which
// reach `SystemNative_SetSockOpt`, `SystemNative_GetSockOpt`,
// `SystemNative_SetLingerOption` and `SystemNative_GetLingerOption`.
//
// Measured on Linux 6.18.5 and Darwin 27.0.0 (`docs/probes/sockopt-options/`).
// The flavours disagree in seven places, and the exit code says which answered
// all seven: 0 for Linux's answers, 100 for Darwin's, and the index of the first
// other answer otherwise.
//
//   * TCP_NODELAY reads back 1 on Linux and 4, its flag's bit, on Darwin;
//   * IPV6_V6ONLY on an IPv4 socket is EOPNOTSUPP on Linux and EINVAL on Darwin;
//   * TCP_NODELAY on a UDP socket is ENOPROTOOPT on Linux and EINVAL on Darwin;
//   * turning lingering off keeps the earlier linger time on Linux, and stores
//     the new one on Darwin;
//   * SO_LINGER set through the generic option path is the kernel's own, in
//     seconds on Linux and hundredths of one on Darwin, where LingerState reads
//     SO_LINGER_SEC;
//   * the shim refuses a linger time above 327 seconds on Darwin, where the
//     kernel keeps it in sixteen bits of hundredths, and above 65535 elsewhere;
//   * on a socket whose connect was refused, Linux takes SO_LINGER and Darwin
//     answers EINVAL, which the shim reports as success.
class SocketOptions
{
    const int Linux = 0;
    const int Darwin = 100;

    static int Main()
    {
        // --- TCP_NODELAY ---
        using var tcp = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        if (tcp.NoDelay) return 1;
        tcp.NoDelay = true;
        if (!tcp.NoDelay) return 2;

        int noDelay = (int)tcp.GetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.NoDelay)!;
        int noDelayFlavour = noDelay switch { 1 => Linux, 4 => Darwin, _ => -1 };
        if (noDelayFlavour < 0) return 3;

        // Asked for eight bytes, the kernel reports the four of an int.
        if (tcp.GetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.NoDelay, 8).Length != 4) return 25;

        tcp.NoDelay = false;
        if (tcp.NoDelay) return 4;

        // --- IPV6_V6ONLY ---
        using var tcp6 = new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp);
        // CreateSocket turns IPV6_V6ONLY on for every IPv6 socket.
        if (tcp6.DualMode) return 5;
        tcp6.DualMode = true;
        if (!tcp6.DualMode) return 6;
        if ((int)tcp6.GetSocketOption(SocketOptionLevel.IPv6, SocketOptionName.IPv6Only)! != 0) return 7;
        tcp6.SetSocketOption(SocketOptionLevel.IPv6, SocketOptionName.IPv6Only, 2);
        if ((int)tcp6.GetSocketOption(SocketOptionLevel.IPv6, SocketOptionName.IPv6Only)! != 1) return 8;

        // --- an option the socket does not have ---
        int wrongDomainFlavour;
        try
        {
            tcp.GetSocketOption(SocketOptionLevel.IPv6, SocketOptionName.IPv6Only);
            return 9;
        }
        catch (SocketException e)
        {
            wrongDomainFlavour = e.SocketErrorCode switch
            {
                SocketError.OperationNotSupported => Linux,
                SocketError.InvalidArgument => Darwin,
                _ => -1,
            };
        }
        if (wrongDomainFlavour < 0) return 10;

        using var udp = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
        int wrongKindFlavour;
        try
        {
            udp.NoDelay = true;
            return 11;
        }
        catch (SocketException e)
        {
            wrongKindFlavour = e.SocketErrorCode switch
            {
                SocketError.ProtocolOption => Linux,
                SocketError.InvalidArgument => Darwin,
                _ => -1,
            };
        }
        if (wrongKindFlavour < 0) return 12;

        // --- SO_LINGER ---
        using var lingering = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        var initial = lingering.LingerState!;
        if (initial.Enabled || initial.LingerTime != 0) return 13;

        lingering.LingerState = new LingerOption(true, 5);
        var five = lingering.LingerState!;
        if (!five.Enabled || five.LingerTime != 5) return 14;

        lingering.LingerState = new LingerOption(true, 0);
        var zero = lingering.LingerState!;
        if (!zero.Enabled || zero.LingerTime != 0) return 15;

        lingering.LingerState = new LingerOption(true, 9);
        lingering.LingerState = new LingerOption(false, 4);
        var off = lingering.LingerState!;
        if (off.Enabled) return 16;
        int offFlavour = off.LingerTime switch { 9 => Linux, 4 => Darwin, _ => -1 };
        if (offFlavour < 0) return 17;

        // SO_LINGER through the generic option path is the kernel's own
        // SO_LINGER, in hundredths of a second on Darwin, where LingerState
        // reads SO_LINGER_SEC.
        var raw = new byte[8];
        BitConverter.GetBytes(1).CopyTo(raw, 0);
        BitConverter.GetBytes(500).CopyTo(raw, 4);
        lingering.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.Linger, raw);
        var viaRaw = lingering.LingerState!;
        if (!viaRaw.Enabled) return 23;
        int rawFlavour = viaRaw.LingerTime switch { 500 => Linux, 5 => Darwin, _ => -1 };
        if (rawFlavour < 0) return 24;

        int longFlavour;
        try
        {
            lingering.LingerState = new LingerOption(true, 400);
            var four = lingering.LingerState!;
            if (!four.Enabled || four.LingerTime != 400) return 18;
            longFlavour = Linux;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.InvalidArgument)
        {
            longFlavour = Darwin;
        }

        // --- a refused socket: Darwin's kernel refuses every option there,
        //     and the shim reports success for SO_LINGER's EINVAL ---
        int deadPort;
        using (var placeholder = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            placeholder.Bind(new IPEndPoint(IPAddress.Loopback, 0));
            deadPort = ((IPEndPoint)placeholder.LocalEndPoint!).Port;
        }

        using var refused = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        try
        {
            refused.Connect(new IPEndPoint(IPAddress.Loopback, deadPort));
            return 26;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.ConnectionRefused)
        {
        }

        refused.LingerState = new LingerOption(true, 1);
        var afterRefusal = refused.LingerState!;
        int refusedFlavour = (afterRefusal.Enabled, afterRefusal.LingerTime) switch
        {
            (true, 1) => Linux,
            (false, 0) => Darwin,
            _ => -1,
        };
        if (refusedFlavour < 0) return 27;

        // --- an accepted socket has its listener's options ---
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        listener.NoDelay = true;
        listener.LingerState = new LingerOption(true, 3);

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect(listener.LocalEndPoint!);
        using var accepted = listener.Accept();
        if (!accepted.NoDelay) return 19;
        var inherited = accepted.LingerState!;
        if (!inherited.Enabled || inherited.LingerTime != 3) return 20;
        if (client.NoDelay) return 21;

        int[] flavours = { noDelayFlavour, wrongDomainFlavour, wrongKindFlavour, offFlavour, rawFlavour, longFlavour, refusedFlavour };
        foreach (var f in flavours)
        {
            if (f != flavours[0]) return 22;
        }

        return flavours[0];
    }
}
