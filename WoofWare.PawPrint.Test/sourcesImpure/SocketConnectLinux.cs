using System;
using System.Runtime.InteropServices;
using System.Threading;

// `SystemNative_Connect`'s flavour-divergent rows under the Linux flavour,
// plus the raw errno numbers its failures leave for a `SetLastError = true`
// import. The flavour-independent rows live differentially in
// `SocketConnect.cs`; every row here was measured on Linux 6.18 (probes
// `connect_probe.c` / `probe2.c`, 2026-08-21) and this guest's expectations
// were confirmed by running it on real Linux .NET before the handler existed.
//
// The Linux-only facts:
//
//   * connect on the listening socket itself answers EISCONN (Darwin:
//     EOPNOTSUPP);
//   * a connect retry after an async establishment answers SUCCESS once —
//     reporting the completion — and EISCONN thereafter (Darwin: EISCONN
//     immediately);
//   * a connect retry after an async refusal delivers the pending error —
//     ECONNREFUSED — exactly once, and then *resets* the socket, so the
//     connect after that is a fresh attempt answering EINPROGRESS (Darwin
//     delivers ECONNREFUSED once too, but then latches the socket dead:
//     EINVAL thereafter). Reading SO_ERROR first would consume the pending
//     error and change these answers (probe-measured ECONNABORTED), but
//     `GetSocketErrorOption` is not modelled yet, so only this path is
//     reachable. A bound-but-not-listening destination refuses exactly like
//     a closed port, because Linux answers a SYN to either with RST (Darwin
//     drops the SYN and the connect pends, which PawPrint's Darwin arm
//     refuses to model);
//   * AF_UNSPEC on an unconnected TCP socket is an accepted no-op, and on a
//     UDP socket dissolves the peer filter (Darwin refuses both);
//   * an oversized sockaddr is fine: the kernel reads the prefix it needs
//     (Darwin: EINVAL);
//   * the accept queue admits backlog + 1 connections (Darwin: backlog).
//
// The Thread.Sleep calls exist for the real-.NET confirmation run: on a real
// kernel the loopback RST or handshake completion lands microseconds after
// EINPROGRESS, and the sleep keeps the retry on the settled side of that
// race. Under PawPrint the outcome is latched at connect time and the sleep
// merely advances the virtual clock.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketConnectLinux
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect", SetLastError = true)]
    static extern unsafe int ConnectReportingErrno(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept", SetLastError = true)]
    static extern unsafe int AcceptReportingErrno(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

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
    const int PAL_EINPROGRESS = 0x1001A;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_EISCONN = 0x1001E;
    const int PAL_ENOTSOCK = 0x1003C;

    // Linux's numbering.
    const int EBADF = 9;
    const int EAGAIN = 11;
    const int EINVAL = 22;
    const int ENOTSOCK = 88;
    const int EAFNOSUPPORT = 97;
    const int EISCONN = 106;
    const int ECONNREFUSED = 111;
    const int EINPROGRESS = 115;

    // PAL numbering, which is not any platform's.
    const int AF_UNIX = 1;
    const int AF_INET = 2;
    const int AF_UNSPEC = 0;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_TCP = 6;
    const int PT_UDP = 17;

    const int V4Size = 16;
    const uint Loopback = 0x0100007F;

    static unsafe IntPtr Make(int type, int protocol)
    {
        IntPtr fd;
        if (Socket(AF_INET, type, protocol, &fd) != PAL_SUCCESS) return (IntPtr)(-1);
        return fd;
    }

    static unsafe bool Address(byte* blob, int size, uint address, ushort port)
    {
        for (int i = 0; i < size; i++) blob[i] = 0;

        return SetAddressFamily(blob, size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, size, address) == PAL_SUCCESS;
    }

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

        // --- a listener, a closed port, and a bound-but-not-listening port ---
        IntPtr lst = Make(SOCK_STREAM, PT_TCP);
        if (lst == (IntPtr)(-1)) return 1;
        if (!Address(blob, V4Size, Loopback, 0)) return 2;
        if (Bind(lst, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 3;
        if (Listen(lst, 8) != PAL_SUCCESS) return 4;
        ushort listenPort = PortOf(lst);
        if (listenPort == 0) return 5;
        byte* dst = stackalloc byte[V4Size];
        if (!Address(dst, V4Size, Loopback, listenPort)) return 6;

        IntPtr tmp = Make(SOCK_STREAM, PT_TCP);
        if (tmp == (IntPtr)(-1)) return 7;
        if (!Address(blob, V4Size, Loopback, 0)) return 8;
        if (Bind(tmp, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 9;
        ushort deadPort = PortOf(tmp);
        if (deadPort == 0) return 10;
        if (Close(tmp) != 0) return 11;
        byte* deadDst = stackalloc byte[V4Size];
        if (!Address(deadDst, V4Size, Loopback, deadPort)) return 12;

        IntPtr bnl = Make(SOCK_STREAM, PT_TCP);
        if (bnl == (IntPtr)(-1)) return 13;
        if (!Address(blob, V4Size, Loopback, 0)) return 14;
        if (Bind(bnl, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 15;
        ushort bnlPort = PortOf(bnl);
        if (bnlPort == 0) return 16;
        byte* bnlDst = stackalloc byte[V4Size];
        if (!Address(bnlDst, V4Size, Loopback, bnlPort)) return 17;

        // --- connect on the listening socket itself: EISCONN here,
        //     EOPNOTSUPP on Darwin ---
        if (ConnectReportingErrno(lst, dst, V4Size) != PAL_EISCONN) return 18;
        if (Marshal.GetLastSystemError() != EISCONN) return 19;

        // --- retry after an async establishment: SUCCESS once, then EISCONN ---
        IntPtr c2 = Make(SOCK_STREAM, PT_TCP);
        if (c2 == (IntPtr)(-1)) return 20;
        if (SetIsNonBlocking(c2, 1) != 0) return 21;
        if (ConnectReportingErrno(c2, dst, V4Size) != PAL_EINPROGRESS) return 22;
        if (Marshal.GetLastSystemError() != EINPROGRESS) return 23;
        Thread.Sleep(100);
        if (Connect(c2, dst, V4Size) != PAL_SUCCESS) return 24;
        if (ConnectReportingErrno(c2, dst, V4Size) != PAL_EISCONN) return 25;
        if (Marshal.GetLastSystemError() != EISCONN) return 26;

        // --- retry after an async refusal: the pending error is delivered
        //     once as ECONNREFUSED, and the socket then resets — the next
        //     connect is a *fresh attempt* (Darwin instead latches the
        //     socket dead: EINVAL thereafter) ---
        IntPtr c4 = Make(SOCK_STREAM, PT_TCP);
        if (c4 == (IntPtr)(-1)) return 27;
        if (SetIsNonBlocking(c4, 1) != 0) return 28;
        if (Connect(c4, deadDst, V4Size) != PAL_EINPROGRESS) return 29;
        Thread.Sleep(100);
        // Before the delivery, the pending attempt's resolved source and
        // port are visible; the delivery's reset then reverts the address to
        // the wildcard (nothing was locked by bind) but keeps the port.
        int preLen = V4Size;
        if (GetSockName(c4, outAddr, &preLen) != PAL_SUCCESS) return 30;
        ushort prePort = 0;
        if (GetPort(outAddr, preLen, &prePort) != PAL_SUCCESS) return 30;
        if (prePort == 0) return 30;
        if (ConnectReportingErrno(c4, deadDst, V4Size) != PAL_ECONNREFUSED) return 31;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 32;
        preLen = V4Size;
        if (GetSockName(c4, outAddr, &preLen) != PAL_SUCCESS) return 33;
        uint postSource = 1;
        if (GetIPv4Address(outAddr, preLen, &postSource) != PAL_SUCCESS) return 33;
        if (postSource != 0) return 33;
        ushort postPort = 0;
        if (GetPort(outAddr, preLen, &postPort) != PAL_SUCCESS) return 33;
        if (postPort != prePort) return 33;
        if (ConnectReportingErrno(c4, deadDst, V4Size) != PAL_EINPROGRESS) return 34;
        if (Marshal.GetLastSystemError() != EINPROGRESS) return 35;
        Thread.Sleep(100);
        if (ConnectReportingErrno(c4, deadDst, V4Size) != PAL_ECONNREFUSED) return 36;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 37;
        if (Close(c4) != 0) return 38;

        // --- a bound-but-not-listening destination refuses like a closed
        //     port: Linux answers the SYN with RST ---
        IntPtr c6 = Make(SOCK_STREAM, PT_TCP);
        if (c6 == (IntPtr)(-1)) return 39;
        if (SetIsNonBlocking(c6, 1) != 0) return 40;
        if (Connect(c6, bnlDst, V4Size) != PAL_EINPROGRESS) return 41;
        Thread.Sleep(100);
        if (ConnectReportingErrno(c6, bnlDst, V4Size) != PAL_ECONNREFUSED) return 42;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 43;
        if (Close(c6) != 0) return 44;
        // The blocking form answers the refusal directly.
        IntPtr c9 = Make(SOCK_STREAM, PT_TCP);
        if (c9 == (IntPtr)(-1)) return 45;
        if (ConnectReportingErrno(c9, bnlDst, V4Size) != PAL_ECONNREFUSED) return 46;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 47;
        if (Close(c9) != 0) return 48;

        // --- AF_UNSPEC: a no-op on an unconnected TCP socket, and the
        //     socket stays usable ---
        IntPtr c7 = Make(SOCK_STREAM, PT_TCP);
        if (c7 == (IntPtr)(-1)) return 49;
        byte* unspecDst = stackalloc byte[V4Size];
        for (int i = 0; i < V4Size; i++) unspecDst[i] = 0;
        if (SetAddressFamily(unspecDst, V4Size, AF_UNSPEC) != PAL_SUCCESS) return 50;
        if (Connect(c7, unspecDst, V4Size) != PAL_SUCCESS) return 51;
        if (Connect(c7, dst, V4Size) != PAL_SUCCESS) return 52;

        // --- UDP: AF_UNSPEC dissolves the peer filter, and the socket
        //     re-targets afterwards ---
        IntPtr u = Make(SOCK_DGRAM, PT_UDP);
        if (u == (IntPtr)(-1)) return 53;
        if (Connect(u, dst, V4Size) != PAL_SUCCESS) return 54;
        if (Connect(u, unspecDst, V4Size) != PAL_SUCCESS) return 55;
        // The dissolve unbinds entirely — unlike TCP's reset, the port is
        // dropped too.
        int uLen = V4Size;
        if (GetSockName(u, outAddr, &uLen) != PAL_SUCCESS) return 56;
        uint uSource = 1;
        if (GetIPv4Address(outAddr, uLen, &uSource) != PAL_SUCCESS) return 56;
        if (uSource != 0) return 56;
        ushort uPort = 1;
        if (GetPort(outAddr, uLen, &uPort) != PAL_SUCCESS) return 56;
        if (uPort != 0) return 56;
        if (Connect(u, deadDst, V4Size) != PAL_SUCCESS) return 57;
        if (Close(u) != 0) return 58;

        // --- an oversized sockaddr: the kernel reads the prefix it needs ---
        IntPtr c8 = Make(SOCK_STREAM, PT_TCP);
        if (c8 == (IntPtr)(-1)) return 59;
        byte* big = stackalloc byte[64];
        if (!Address(big, 64, Loopback, listenPort)) return 60;
        if (Connect(c8, big, 64) != PAL_SUCCESS) return 61;

        // --- the raw numbers of the flavour-independent failures ---
        IntPtr dead = Make(SOCK_STREAM, PT_TCP);
        if (dead == (IntPtr)(-1)) return 62;
        if (Close(dead) != 0) return 63;
        if (ConnectReportingErrno(dead, dst, V4Size) != PAL_EBADF) return 64;
        if (Marshal.GetLastSystemError() != EBADF) return 65;
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 66;
        if (ConnectReportingErrno(port, dst, V4Size) != PAL_ENOTSOCK) return 67;
        if (Marshal.GetLastSystemError() != ENOTSOCK) return 68;
        if (Close(port) != 0) return 69;
        IntPtr c10 = Make(SOCK_STREAM, PT_TCP);
        if (c10 == (IntPtr)(-1)) return 70;
        if (ConnectReportingErrno(c10, dst, 4) != PAL_EINVAL) return 71;
        if (Marshal.GetLastSystemError() != EINVAL) return 72;
        byte* unixDst = stackalloc byte[V4Size];
        for (int i = 0; i < V4Size; i++) unixDst[i] = 0;
        if (SetAddressFamily(unixDst, V4Size, AF_UNIX) != PAL_SUCCESS) return 73;
        if (ConnectReportingErrno(c10, unixDst, V4Size) != PAL_EAFNOSUPPORT) return 74;
        if (Marshal.GetLastSystemError() != EAFNOSUPPORT) return 75;
        IntPtr c11 = Make(SOCK_STREAM, PT_TCP);
        if (c11 == (IntPtr)(-1)) return 76;
        if (ConnectReportingErrno(c11, deadDst, V4Size) != PAL_ECONNREFUSED) return 77;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 78;
        // A blocking refusal delivers inline and resets the socket too: the
        // retry is a fresh attempt — refused again by the same closed port,
        // and completing against a live listener.
        if (ConnectReportingErrno(c11, deadDst, V4Size) != PAL_ECONNREFUSED) return 79;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 80;
        if (Connect(c11, dst, V4Size) != PAL_SUCCESS) return 81;
        if (Close(c11) != 0) return 82;

        // --- the accept queue admits backlog + 1 ---
        IntPtr lst2 = Make(SOCK_STREAM, PT_TCP);
        if (lst2 == (IntPtr)(-1)) return 83;
        if (!Address(blob, V4Size, Loopback, 0)) return 84;
        if (Bind(lst2, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 85;
        if (Listen(lst2, 1) != PAL_SUCCESS) return 86;
        ushort listen2Port = PortOf(lst2);
        if (listen2Port == 0) return 87;
        byte* dst2 = stackalloc byte[V4Size];
        if (!Address(dst2, V4Size, Loopback, listen2Port)) return 88;
        IntPtr k1 = Make(SOCK_STREAM, PT_TCP);
        if (k1 == (IntPtr)(-1)) return 89;
        if (SetIsNonBlocking(k1, 1) != 0) return 90;
        if (Connect(k1, dst2, V4Size) != PAL_EINPROGRESS) return 91;
        ushort k1Port = PortOf(k1);
        if (k1Port == 0) return 92;
        IntPtr k2 = Make(SOCK_STREAM, PT_TCP);
        if (k2 == (IntPtr)(-1)) return 93;
        if (SetIsNonBlocking(k2, 1) != 0) return 94;
        if (Connect(k2, dst2, V4Size) != PAL_EINPROGRESS) return 95;
        ushort k2Port = PortOf(k2);
        if (k2Port == 0) return 96;
        Thread.Sleep(100);
        int len = V4Size;
        IntPtr a1;
        if (Accept(lst2, outAddr, &len, &a1) != PAL_SUCCESS) return 97;
        ushort peer;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 98;
        if (peer != k1Port) return 99;
        if (Close(a1) != 0) return 100;
        len = V4Size;
        IntPtr a2;
        if (Accept(lst2, outAddr, &len, &a2) != PAL_SUCCESS) return 101;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 102;
        if (peer != k2Port) return 103;
        if (Close(a2) != 0) return 104;
        if (SetIsNonBlocking(lst2, 1) != 0) return 105;

        // A negative backlog is compared unsigned and clamps to somaxconn:
        // the listener still admits connections.
        IntPtr lst3 = Make(SOCK_STREAM, PT_TCP);
        if (lst3 == (IntPtr)(-1)) return 106;
        if (!Address(blob, V4Size, Loopback, 0)) return 107;
        if (Bind(lst3, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 108;
        if (Listen(lst3, -1) != PAL_SUCCESS) return 109;
        ushort listen3Port = PortOf(lst3);
        if (listen3Port == 0) return 110;
        byte* dst3 = stackalloc byte[V4Size];
        if (!Address(dst3, V4Size, Loopback, listen3Port)) return 111;
        IntPtr k3 = Make(SOCK_STREAM, PT_TCP);
        if (k3 == (IntPtr)(-1)) return 112;
        if (Connect(k3, dst3, V4Size) != PAL_SUCCESS) return 113;
        len = V4Size;
        IntPtr a4;
        if (Accept(lst3, outAddr, &len, &a4) != PAL_SUCCESS) return 114;
        if (Close(a4) != 0) return 115;
        if (Close(k3) != 0) return 116;
        if (Close(lst3) != 0) return 117;
        len = V4Size;
        IntPtr a3;
        if (AcceptReportingErrno(lst2, outAddr, &len, &a3) != PAL_EAGAIN) return 118;
        if (Marshal.GetLastSystemError() != EAGAIN) return 119;

        return 0;
    }
}
