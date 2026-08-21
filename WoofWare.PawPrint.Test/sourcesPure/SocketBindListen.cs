using System.Net;
using System.Net.Sockets;

// `Socket.Bind`, `Socket.Listen` and `Socket.LocalEndPoint`: the three entry
// points that give a socket a local address, through the managed surface.
//
// Differential, so every row here was measured to answer identically on macOS
// 26.6 and on Linux 6.x before being asserted. That rules out a good deal of
// what these calls do:
//
//   * binding a second socket to an address a *non-listening* TCP socket holds
//     succeeds on Linux and is EADDRINUSE on Darwin, because `SystemNative_Bind`
//     sets SO_REUSEADDR when its `protocolType` argument is PT_TCP and the two
//     kernels relax it in opposite directions. The same pair with
//     `ProtocolType.Unspecified` -- which reaches `Bind` as PT_UNSPECIFIED, so no
//     SO_REUSEADDR -- agrees, and is asserted here instead;
//   * `127.9.9.9` is bindable on Linux (127.0.0.0/8 is a local prefix there) and
//     EADDRNOTAVAIL on Darwin, which assigns loopback exactly one address;
//   * a privileged port is EACCES only for a non-root caller, and the suite's own
//     uid is not controlled, so it cannot be asserted here at all.
//
// Those live in `SocketBindLinux.cs` / `SocketBindDarwin.cs`, under PawPrint
// alone where the flavour and the uid are known.
//
// Nor does any row here expect a *throw*. Raising a `SocketException` runs
// `SystemNative_ConvertErrorPalToPlatform`, which is unimplemented, so a guest
// that caught one would abort while constructing it -- the same wall
// `OpenMissingFile.cs` meets. Every refusal this slice models is asserted in
// `SocketBindScreens.cs` instead, which reads the PAL error the entry point
// returns and never builds an exception.
//
// No IPv6 socket is created: `SocketPal.CreateSocket` sets IPV6_V6ONLY on every
// non-raw AF_INET6 socket, and `SystemNative_SetSockOpt` is unimplemented, so a
// guest that constructed one would abort before reaching `Bind`.
public static class SocketBindListen
{
    static Socket Tcp () => new Socket (AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
    static Socket Udp () => new Socket (AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);

    public static int Main ()
    {
        using (Socket s = Tcp ())
        {
            // No syscall yet: managed code answers null until an explicit Bind
            // has given it an endpoint to remember.
            if (s.LocalEndPoint != null) return 1;

            s.Bind (new IPEndPoint (IPAddress.Loopback, 0));

            IPEndPoint bound = (IPEndPoint) s.LocalEndPoint;
            if (bound == null) return 2;
            if (!bound.Address.Equals (IPAddress.Loopback)) return 3;

            // Which port an ephemeral bind picks is unspecified, and the two real
            // kernels do not agree (Linux draws from 32768-60999, Darwin from
            // 49152-65535), so only these two facts about it are assertable.
            if (bound.Port == 0) return 4;
            if (bound.Port < 1024) return 5;

            s.Listen (16);

            // Listening does not move the address the socket already had.
            IPEndPoint afterListen = (IPEndPoint) s.LocalEndPoint;
            if (afterListen.Port != bound.Port) return 7;
            if (!afterListen.Address.Equals (IPAddress.Loopback)) return 8;

            // A second listen is not an error, and neither is any backlog: the
            // value is a hint, and even a negative one is accepted. Called
            // directly rather than guarded -- a throw here propagates, which
            // fails the case just as a wrong return would.
            s.Listen (4);
            s.Listen (0);
            s.Listen (-1);

            // A UDP socket may take the port a listening TCP socket holds: the
            // two transports have separate port namespaces, measured on both.
            using (Socket datagram = Udp ())
            {
                datagram.Bind (new IPEndPoint (IPAddress.Loopback, bound.Port));

                if (((IPEndPoint) datagram.LocalEndPoint).Port != bound.Port) return 14;
            }
        }

        // The wildcard binds, and reports itself back rather than a resolved
        // address.
        using (Socket s = Tcp ())
        {
            s.Bind (new IPEndPoint (IPAddress.Any, 0));
            IPEndPoint wildcard = (IPEndPoint) s.LocalEndPoint;
            if (!wildcard.Address.Equals (IPAddress.Any)) return 19;
            if (wildcard.Port == 0) return 20;
        }

        return 0;
    }
}
