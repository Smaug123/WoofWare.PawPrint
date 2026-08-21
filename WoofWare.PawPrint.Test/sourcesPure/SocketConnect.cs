using System;
using System.Runtime.InteropServices;

// `SystemNative_Connect` (pal_networking.c:1785) and the success path of
// `SystemNative_Accept`, reached by hand-rolled P/Invoke: the loopback
// connection state machine, single-threaded — a blocking loopback connect
// completes without a concurrent accept on both kernels, so the rendezvous
// needs no second thread.
//
// Differential, so only rows measured to answer identically on macOS 26 and
// Linux 6.18 (probes `connect_probe.c` / `probe2.c`, 2026-08-21; see
// docs/plans/2026-08-21-socket-connect.md). The flavour-divergent rows —
// connect on the listening socket itself, connect-retry after an async
// outcome, bound-not-listening destinations, AF_UNSPEC, oversized addrlen,
// accept-queue capacity — live in `SocketConnectLinux.cs` /
// `SocketConnectDarwin.cs` under PawPrint alone, where the flavour is known.
//
// The facts pinned deliberately:
//
//   * the wrapper's EFAULT screens (NULL address, negative declared length)
//     precede everything, the descriptor decode included;
//   * a *blocking* connect to a listening loopback socket succeeds
//     synchronously, and gives the client a nonzero implicit-bind port; a
//     second connect answers EISCONN;
//   * a *non-blocking* connect answers EINPROGRESS even on loopback — on
//     both kernels — whether the destination is listening or closed;
//   * a blocking connect to a closed port answers ECONNREFUSED;
//   * the fd checks precede the argument checks: a dead fd answers EBADF and
//     a non-socket answers ENOTSOCK even alongside a short or wrong-family
//     address;
//   * a destination of 0.0.0.0 means loopback on both kernels;
//   * UDP connect is a peer filter: it succeeds with nothing at the
//     destination, and re-connect re-targets;
//   * accept dequeues in connect order, reports the peer's own
//     `getsockname` address, gives the accepted socket the listener's local
//     address, and hands back a *blocking* descriptor (Linux natively,
//     Darwin because the PAL resets it — pal_networking.c:1739);
//   * a connection whose client has already been closed is still accepted.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketConnect
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetIsNonBlocking")]
    static extern unsafe int GetIsNonBlocking(IntPtr fd, int* isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern unsafe int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetIPv4Address")]
    static extern unsafe int GetIPv4Address(byte* socketAddress, int socketAddressLen, uint* address);

    const int PAL_SUCCESS = 0;
    const int PAL_EAGAIN = 0x10006;
    const int PAL_EAFNOSUPPORT = 0x10005;
    const int PAL_EBADF = 0x10008;
    const int PAL_ECONNREFUSED = 0x1000E;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINPROGRESS = 0x1001A;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_EISCONN = 0x1001E;
    const int PAL_ENOTSOCK = 0x1003C;

    // PAL numbering, which is not any platform's.
    const int AF_UNIX = 1;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_TCP = 6;
    const int PT_UDP = 17;

    const int V4Size = 16;

    // `INADDR_LOOPBACK` in network order, as `SetIPv4Address` takes it.
    const uint Loopback = 0x0100007F;
    const uint AnyAddress = 0x00000000;

    static unsafe IntPtr Make(int type, int protocol)
    {
        IntPtr fd;
        if (Socket(AF_INET, type, protocol, &fd) != PAL_SUCCESS) return (IntPtr)(-1);
        return fd;
    }

    /// Fills `blob` with an IPv4 sockaddr for `address`:`port`, in whichever
    /// layout this runtime uses.
    static unsafe bool Address(byte* blob, uint address, ushort port)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, address) == PAL_SUCCESS;
    }

    /// The socket's own bound port, or 0 on any failure.
    static unsafe ushort PortOf(IntPtr fd)
    {
        byte* blob = stackalloc byte[V4Size];
        int len = V4Size;
        if (GetSockName(fd, blob, &len) != PAL_SUCCESS) return 0;
        ushort port;
        if (GetPort(blob, len, &port) != PAL_SUCCESS) return 0;
        return port;
    }

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];
        byte* outAddr = stackalloc byte[V4Size];

        // --- a blocking listener ---
        IntPtr lst = Make(SOCK_STREAM, PT_TCP);
        if (lst == (IntPtr)(-1)) return 1;
        if (!Address(blob, Loopback, 0)) return 2;
        if (Bind(lst, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 3;
        if (Listen(lst, 8) != PAL_SUCCESS) return 4;
        ushort listenPort = PortOf(lst);
        if (listenPort == 0) return 5;
        byte* dst = stackalloc byte[V4Size];
        if (!Address(dst, Loopback, listenPort)) return 6;

        // --- a port with nothing behind it: bind, remember, close ---
        IntPtr tmp = Make(SOCK_STREAM, PT_TCP);
        if (tmp == (IntPtr)(-1)) return 7;
        if (!Address(blob, Loopback, 0)) return 8;
        if (Bind(tmp, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 9;
        ushort deadPort = PortOf(tmp);
        if (deadPort == 0) return 10;
        if (Close(tmp) != 0) return 11;
        byte* deadDst = stackalloc byte[V4Size];
        if (!Address(deadDst, Loopback, deadPort)) return 12;

        // --- the wrapper's EFAULT screens, which precede everything ---
        IntPtr c0 = Make(SOCK_STREAM, PT_TCP);
        if (c0 == (IntPtr)(-1)) return 13;
        if (Connect(c0, null, V4Size) != PAL_EFAULT) return 14;
        if (Connect(c0, dst, -1) != PAL_EFAULT) return 15;
        // A pointer masquerading as the fd never reaches the decode.
        int scratch = 0;
        if (Connect((IntPtr)(&scratch), null, V4Size) != PAL_EFAULT) return 16;

        // --- blocking connect to the listener: synchronous success ---
        IntPtr c1 = c0;
        if (Connect(c1, dst, V4Size) != PAL_SUCCESS) return 17;
        ushort q1 = PortOf(c1);
        if (q1 == 0) return 18;
        if (Connect(c1, dst, V4Size) != PAL_EISCONN) return 19;

        // --- non-blocking connect to the listener: EINPROGRESS even on
        //     loopback, with the implicit bind already done ---
        IntPtr c2 = Make(SOCK_STREAM, PT_TCP);
        if (c2 == (IntPtr)(-1)) return 20;
        if (SetIsNonBlocking(c2, 1) != 0) return 21;
        if (Connect(c2, dst, V4Size) != PAL_EINPROGRESS) return 22;
        ushort q2 = PortOf(c2);
        if (q2 == 0) return 23;

        // --- connects to the port with nothing behind it ---
        IntPtr c3 = Make(SOCK_STREAM, PT_TCP);
        if (c3 == (IntPtr)(-1)) return 24;
        if (Connect(c3, deadDst, V4Size) != PAL_ECONNREFUSED) return 25;
        if (Close(c3) != 0) return 26;
        IntPtr c4 = Make(SOCK_STREAM, PT_TCP);
        if (c4 == (IntPtr)(-1)) return 27;
        if (SetIsNonBlocking(c4, 1) != 0) return 28;
        if (Connect(c4, deadDst, V4Size) != PAL_EINPROGRESS) return 29;
        // The attempt bound the socket before the SYN: while the refusal is
        // pending, getsockname reports resolved loopback and a real port.
        int preLen = V4Size;
        if (GetSockName(c4, outAddr, &preLen) != PAL_SUCCESS) return 30;
        uint pendingSource = 0;
        if (GetIPv4Address(outAddr, preLen, &pendingSource) != PAL_SUCCESS) return 31;
        if (pendingSource != Loopback) return 32;
        ushort pendingPort = 0;
        if (GetPort(outAddr, preLen, &pendingPort) != PAL_SUCCESS) return 33;
        if (pendingPort == 0) return 34;
        if (Close(c4) != 0) return 35;

        // --- descriptor checks, and their place in the ladder ---
        IntPtr dead = Make(SOCK_STREAM, PT_TCP);
        if (dead == (IntPtr)(-1)) return 36;
        if (Close(dead) != 0) return 37;
        if (Connect(dead, dst, V4Size) != PAL_EBADF) return 38;

        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 39;
        if (Connect(port, dst, V4Size) != PAL_ENOTSOCK) return 40;

        // --- argument checks on a live socket ---
        IntPtr c7 = Make(SOCK_STREAM, PT_TCP);
        if (c7 == (IntPtr)(-1)) return 41;
        if (Connect(c7, dst, 4) != PAL_EINVAL) return 42;
        if (Connect(c7, dst, 8) != PAL_EINVAL) return 43;
        if (Connect(c7, dst, V4Size - 1) != PAL_EINVAL) return 44;
        byte* unixDst = stackalloc byte[V4Size];
        for (int i = 0; i < V4Size; i++) unixDst[i] = 0;
        if (SetAddressFamily(unixDst, V4Size, AF_UNIX) != PAL_SUCCESS) return 45;
        if (Connect(c7, unixDst, V4Size) != PAL_EAFNOSUPPORT) return 46;
        if (Close(c7) != 0) return 47;

        // The fd checks beat the argument checks. A fresh dead fd, with no
        // allocation between its close and its use: descriptor numbers are
        // reused, so the one from check 33 may name a live object by now.
        IntPtr dead2 = Make(SOCK_STREAM, PT_TCP);
        if (dead2 == (IntPtr)(-1)) return 48;
        if (Close(dead2) != 0) return 49;
        if (Connect(dead2, dst, 4) != PAL_EBADF) return 50;
        if (Connect(dead2, unixDst, V4Size) != PAL_EBADF) return 51;
        if (Connect(port, unixDst, V4Size) != PAL_ENOTSOCK) return 52;
        if (Close(port) != 0) return 53;

        // --- 0.0.0.0 as destination means loopback ---
        IntPtr c5 = Make(SOCK_STREAM, PT_TCP);
        if (c5 == (IntPtr)(-1)) return 54;
        byte* zeroDst = stackalloc byte[V4Size];
        if (!Address(zeroDst, AnyAddress, listenPort)) return 55;
        if (Connect(c5, zeroDst, V4Size) != PAL_SUCCESS) return 56;
        ushort q5 = PortOf(c5);
        if (q5 == 0) return 57;

        // --- UDP connect is a peer filter, not a handshake ---
        IntPtr u = Make(SOCK_DGRAM, PT_UDP);
        if (u == (IntPtr)(-1)) return 58;
        if (Connect(u, deadDst, V4Size) != PAL_SUCCESS) return 59;
        if (Connect(u, dst, V4Size) != PAL_SUCCESS) return 60;
        if (Connect(u, unixDst, V4Size) != PAL_EAFNOSUPPORT) return 61;
        if (Close(u) != 0) return 62;

        // --- accept dequeues in connect order: c1, c2, c5 ---
        int len = V4Size;
        IntPtr a1;
        if (Accept(lst, outAddr, &len, &a1) != PAL_SUCCESS) return 63;
        if (len != V4Size) return 64;
        ushort peer;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 65;
        if (peer != q1) return 66;
        // The accepted socket's own address is the listener's...
        if (PortOf(a1) != listenPort) return 67;
        // ...and its descriptor is blocking through the PAL on both kernels.
        int nb = 7;
        if (GetIsNonBlocking(a1, &nb) != 0) return 68;
        if (nb != 0) return 69;
        if (Close(a1) != 0) return 70;

        len = V4Size;
        IntPtr a2;
        if (Accept(lst, outAddr, &len, &a2) != PAL_SUCCESS) return 71;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 72;
        if (peer != q2) return 73;
        if (Close(a2) != 0) return 74;

        len = V4Size;
        IntPtr a3;
        if (Accept(lst, outAddr, &len, &a3) != PAL_SUCCESS) return 75;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 76;
        if (peer != q5) return 77;
        if (Close(a3) != 0) return 78;

        // Queue drained: the non-blocking listener answers EAGAIN.
        if (SetIsNonBlocking(lst, 1) != 0) return 79;
        len = V4Size;
        IntPtr a4;
        if (Accept(lst, outAddr, &len, &a4) != PAL_EAGAIN) return 80;

        // --- a connection outlives the client that opened it ---
        IntPtr c6 = Make(SOCK_STREAM, PT_TCP);
        if (c6 == (IntPtr)(-1)) return 81;
        if (Connect(c6, dst, V4Size) != PAL_SUCCESS) return 82;
        ushort q6 = PortOf(c6);
        if (q6 == 0) return 83;
        if (Close(c6) != 0) return 84;
        len = V4Size;
        IntPtr a5;
        if (Accept(lst, outAddr, &len, &a5) != PAL_SUCCESS) return 85;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 86;
        if (peer != q6) return 87;
        if (Close(a5) != 0) return 88;

        // A declared length larger than the sockaddr: the kernel writes the
        // 16 bytes it has and *reports* 16, so the out-length visibly shrinks.
        IntPtr c8 = Make(SOCK_STREAM, PT_TCP);
        if (c8 == (IntPtr)(-1)) return 89;
        if (Connect(c8, dst, V4Size) != PAL_SUCCESS) return 90;
        ushort q8 = PortOf(c8);
        if (q8 == 0) return 91;
        byte* wideAddr = stackalloc byte[64];
        len = 64;
        IntPtr a6;
        if (SetIsNonBlocking(lst, 0) != 0) return 92;
        if (Accept(lst, wideAddr, &len, &a6) != PAL_SUCCESS) return 93;
        if (len != V4Size) return 94;
        if (GetPort(wideAddr, len, &peer) != PAL_SUCCESS) return 95;
        if (peer != q8) return 96;
        if (Close(a6) != 0) return 97;
        if (Close(c8) != 0) return 98;

        // --- a client bound to the wildcard gets a concrete source at
        //     connect: 127.0.0.1, with the port kept ---
        IntPtr c9 = Make(SOCK_STREAM, PT_TCP);
        if (c9 == (IntPtr)(-1)) return 99;
        if (!Address(blob, AnyAddress, 0)) return 100;
        if (Bind(c9, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 101;
        ushort q9 = PortOf(c9);
        if (q9 == 0) return 102;
        if (Connect(c9, dst, V4Size) != PAL_SUCCESS) return 103;
        len = V4Size;
        if (GetSockName(c9, outAddr, &len) != PAL_SUCCESS) return 104;
        uint sourceAddress = 0;
        if (GetIPv4Address(outAddr, len, &sourceAddress) != PAL_SUCCESS) return 105;
        if (sourceAddress != Loopback) return 106;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 107;
        if (peer != q9) return 108;
        // ...and the accept side reports that resolved source as the peer.
        len = V4Size;
        IntPtr a7;
        if (Accept(lst, outAddr, &len, &a7) != PAL_SUCCESS) return 109;
        if (GetIPv4Address(outAddr, len, &sourceAddress) != PAL_SUCCESS) return 110;
        if (sourceAddress != Loopback) return 111;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 112;
        if (peer != q9) return 113;
        if (Close(a7) != 0) return 114;
        if (Close(c9) != 0) return 115;

        // The same resolution for a wildcard-bound datagram socket.
        IntPtr u2 = Make(SOCK_DGRAM, PT_UDP);
        if (u2 == (IntPtr)(-1)) return 116;
        if (!Address(blob, AnyAddress, 0)) return 117;
        if (Bind(u2, PT_UDP, blob, V4Size) != PAL_SUCCESS) return 118;
        if (Connect(u2, dst, V4Size) != PAL_SUCCESS) return 119;
        len = V4Size;
        if (GetSockName(u2, outAddr, &len) != PAL_SUCCESS) return 120;
        if (GetIPv4Address(outAddr, len, &sourceAddress) != PAL_SUCCESS) return 121;
        if (sourceAddress != Loopback) return 122;
        if (Close(u2) != 0) return 123;

        if (Close(c1) != 0) return 124;
        if (Close(c2) != 0) return 125;
        if (Close(c5) != 0) return 126;
        if (Close(lst) != 0) return 127;

        return 0;
    }
}
