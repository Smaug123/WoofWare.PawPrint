using System;
using System.Runtime.InteropServices;
using System.Threading;

// `SystemNative_Connect`'s flavour-divergent rows under the Darwin flavour,
// plus the raw errno numbers its failures leave for a `SetLastError = true`
// import. The flavour-independent rows live differentially in
// `SocketConnect.cs`; every row here was measured on macOS 26 (probes
// `connect_probe.c` / `probe2.c` / `probe3.c`, 2026-08-21) and this guest's
// expectations were confirmed by running it on real macOS .NET before the
// handler existed.
//
// The Darwin-only facts:
//
//   * connect on the listening socket itself answers EOPNOTSUPP (Linux:
//     EISCONN);
//   * a connect retry after an async establishment answers EISCONN (Linux
//     reports the completion with one SUCCESS first). The measured EALREADY
//     answer exists only inside the real kernel's handshake window, which
//     PawPrint's instantaneous model has no equivalent of — the sleep below
//     keeps the real-.NET confirmation run on the settled side of it;
//   * a connect retry after an async refusal delivers the pending error —
//     ECONNREFUSED — exactly once, and then latches the socket dead: every
//     later connect answers EINVAL (Linux instead resets the socket for a
//     fresh attempt);
//   * AF_UNSPEC is refused everywhere: EADDRNOTAVAIL on an unconnected TCP
//     socket (which stays usable), EISCONN on a connected one, and
//     EAFNOSUPPORT on a UDP socket whether or not a peer filter is set
//     (Linux accepts all three);
//   * an oversized sockaddr answers EINVAL (Linux reads the prefix);
//   * the accept queue admits exactly the clamped backlog (Linux: clamped
//     backlog + 1), where a non-positive or over-large backlog clamps to
//     the kern.ipc.somaxconn sysctl — measured at its default of 128:
//     listen(0), listen(-1) and listen(INT_MAX) each admitted exactly 128.
//
// There is no bound-but-not-listening row: Darwin drops the SYN and the
// connect pends on the real kernel's retry schedule, which PawPrint's
// Darwin arm refuses to model.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketConnectDarwin
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
    const int PAL_EADDRINUSE = 0x10003;
    const int PAL_EAGAIN = 0x10006;
    const int PAL_EAFNOSUPPORT = 0x10005;
    const int PAL_EADDRNOTAVAIL = 0x10004;
    const int PAL_ECONNREFUSED = 0x1000E;
    const int PAL_EINPROGRESS = 0x1001A;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_EISCONN = 0x1001E;
    const int PAL_EOPNOTSUPP = 0x1003D;

    // Darwin's numbering.
    const int EINVAL = 22;
    const int EAGAIN = 35;
    const int EINPROGRESS = 36;
    const int EAFNOSUPPORT = 47;
    const int EADDRNOTAVAIL = 49;
    const int EISCONN = 56;
    const int ECONNREFUSED = 61;
    const int EOPNOTSUPP = 102;

    // PAL numbering, which is not any platform's.
    const int AF_UNSPEC = 0;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_TCP = 6;
    const int PT_UNSPECIFIED = 0;
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

        // --- a listener and a closed port ---
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

        // --- connect on the listening socket itself: EOPNOTSUPP here,
        //     EISCONN on Linux ---
        if (ConnectReportingErrno(lst, dst, V4Size) != PAL_EOPNOTSUPP) return 13;
        if (Marshal.GetLastSystemError() != EOPNOTSUPP) return 14;

        // --- AF_UNSPEC on a connected socket: EISCONN ---
        IntPtr c1 = Make(SOCK_STREAM, PT_TCP);
        if (c1 == (IntPtr)(-1)) return 15;
        if (Connect(c1, dst, V4Size) != PAL_SUCCESS) return 16;
        byte* unspecDst = stackalloc byte[V4Size];
        for (int i = 0; i < V4Size; i++) unspecDst[i] = 0;
        if (SetAddressFamily(unspecDst, V4Size, AF_UNSPEC) != PAL_SUCCESS) return 17;
        if (ConnectReportingErrno(c1, unspecDst, V4Size) != PAL_EISCONN) return 18;
        if (Marshal.GetLastSystemError() != EISCONN) return 19;

        // --- retry after an async establishment: EISCONN, with no
        //     completion-reporting SUCCESS first ---
        IntPtr c2 = Make(SOCK_STREAM, PT_TCP);
        if (c2 == (IntPtr)(-1)) return 20;
        if (SetIsNonBlocking(c2, 1) != 0) return 21;
        if (ConnectReportingErrno(c2, dst, V4Size) != PAL_EINPROGRESS) return 22;
        if (Marshal.GetLastSystemError() != EINPROGRESS) return 23;
        Thread.Sleep(200);
        if (ConnectReportingErrno(c2, dst, V4Size) != PAL_EISCONN) return 24;
        if (Marshal.GetLastSystemError() != EISCONN) return 25;

        // --- retry after an async refusal: ECONNREFUSED delivered once,
        //     then the socket is dead ---
        IntPtr c4 = Make(SOCK_STREAM, PT_TCP);
        if (c4 == (IntPtr)(-1)) return 26;
        if (SetIsNonBlocking(c4, 1) != 0) return 27;
        if (Connect(c4, deadDst, V4Size) != PAL_EINPROGRESS) return 28;
        Thread.Sleep(200);
        if (ConnectReportingErrno(c4, deadDst, V4Size) != PAL_ECONNREFUSED) return 29;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 30;
        // Darwin keeps the resolved source through the delivery (Linux
        // reverts it to what bind locked).
        int postLen = V4Size;
        if (GetSockName(c4, outAddr, &postLen) != PAL_SUCCESS) return 31;
        uint postSource = 0;
        if (GetIPv4Address(outAddr, postLen, &postSource) != PAL_SUCCESS) return 32;
        if (postSource != Loopback) return 33;
        ushort postPort = 0;
        if (GetPort(outAddr, postLen, &postPort) != PAL_SUCCESS) return 34;
        if (postPort == 0) return 35;
        if (ConnectReportingErrno(c4, deadDst, V4Size) != PAL_EINVAL) return 36;
        if (Marshal.GetLastSystemError() != EINVAL) return 37;
        if (ConnectReportingErrno(c4, dst, V4Size) != PAL_EINVAL) return 38;
        if (Marshal.GetLastSystemError() != EINVAL) return 39;
        if (Close(c4) != 0) return 40;

        // --- a *blocking* refusal delivers inline and latches the socket
        //     dead just the same: EINVAL even toward a live listener ---
        IntPtr c5 = Make(SOCK_STREAM, PT_TCP);
        if (c5 == (IntPtr)(-1)) return 41;
        if (ConnectReportingErrno(c5, deadDst, V4Size) != PAL_ECONNREFUSED) return 42;
        if (Marshal.GetLastSystemError() != ECONNREFUSED) return 43;
        if (ConnectReportingErrno(c5, dst, V4Size) != PAL_EINVAL) return 44;
        if (Marshal.GetLastSystemError() != EINVAL) return 45;
        if (Close(c5) != 0) return 46;

        // --- AF_UNSPEC on an unconnected TCP socket: EADDRNOTAVAIL, and
        //     the socket stays usable ---
        IntPtr c7 = Make(SOCK_STREAM, PT_TCP);
        if (c7 == (IntPtr)(-1)) return 47;
        if (ConnectReportingErrno(c7, unspecDst, V4Size) != PAL_EADDRNOTAVAIL) return 48;
        if (Marshal.GetLastSystemError() != EADDRNOTAVAIL) return 49;
        if (Connect(c7, dst, V4Size) != PAL_SUCCESS) return 50;

        // --- AF_UNSPEC on a UDP socket: EAFNOSUPPORT, peer filter or not ---
        IntPtr u = Make(SOCK_DGRAM, PT_UDP);
        if (u == (IntPtr)(-1)) return 51;
        if (ConnectReportingErrno(u, unspecDst, V4Size) != PAL_EAFNOSUPPORT) return 52;
        if (Marshal.GetLastSystemError() != EAFNOSUPPORT) return 53;
        if (Connect(u, dst, V4Size) != PAL_SUCCESS) return 54;
        if (ConnectReportingErrno(u, unspecDst, V4Size) != PAL_EAFNOSUPPORT) return 55;
        if (Marshal.GetLastSystemError() != EAFNOSUPPORT) return 56;
        if (Close(u) != 0) return 57;

        // --- an oversized sockaddr: EINVAL here, accepted on Linux ---
        IntPtr c8 = Make(SOCK_STREAM, PT_TCP);
        if (c8 == (IntPtr)(-1)) return 58;
        byte* big = stackalloc byte[64];
        if (!Address(big, 64, Loopback, listenPort)) return 59;
        if (ConnectReportingErrno(c8, big, 64) != PAL_EINVAL) return 60;
        if (Marshal.GetLastSystemError() != EINVAL) return 61;
        if (Close(c8) != 0) return 62;

        // --- the accept queue admits exactly backlog connections ---
        IntPtr lst2 = Make(SOCK_STREAM, PT_TCP);
        if (lst2 == (IntPtr)(-1)) return 63;
        if (!Address(blob, V4Size, Loopback, 0)) return 64;
        if (Bind(lst2, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 65;
        if (Listen(lst2, 1) != PAL_SUCCESS) return 66;
        ushort listen2Port = PortOf(lst2);
        if (listen2Port == 0) return 67;
        byte* dst2 = stackalloc byte[V4Size];
        if (!Address(dst2, V4Size, Loopback, listen2Port)) return 68;
        IntPtr k1 = Make(SOCK_STREAM, PT_TCP);
        if (k1 == (IntPtr)(-1)) return 69;
        if (SetIsNonBlocking(k1, 1) != 0) return 70;
        if (Connect(k1, dst2, V4Size) != PAL_EINPROGRESS) return 71;
        ushort k1Port = PortOf(k1);
        if (k1Port == 0) return 72;
        Thread.Sleep(200);
        int len = V4Size;
        IntPtr a1;
        if (Accept(lst2, outAddr, &len, &a1) != PAL_SUCCESS) return 73;
        ushort peer;
        if (GetPort(outAddr, len, &peer) != PAL_SUCCESS) return 74;
        if (peer != k1Port) return 75;
        if (Close(a1) != 0) return 76;
        if (SetIsNonBlocking(lst2, 1) != 0) return 77;
        len = V4Size;
        IntPtr a2;
        if (AcceptReportingErrno(lst2, outAddr, &len, &a2) != PAL_EAGAIN) return 78;
        if (Marshal.GetLastSystemError() != EAGAIN) return 79;

        // A non-positive backlog clamps to somaxconn: listen(0) and
        // listen(-1) still admit connections.
        IntPtr lst3 = Make(SOCK_STREAM, PT_TCP);
        if (lst3 == (IntPtr)(-1)) return 80;
        if (!Address(blob, V4Size, Loopback, 0)) return 81;
        if (Bind(lst3, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 82;
        if (Listen(lst3, 0) != PAL_SUCCESS) return 83;
        ushort listen3Port = PortOf(lst3);
        if (listen3Port == 0) return 84;
        byte* dst3 = stackalloc byte[V4Size];
        if (!Address(dst3, V4Size, Loopback, listen3Port)) return 85;
        IntPtr k3 = Make(SOCK_STREAM, PT_TCP);
        if (k3 == (IntPtr)(-1)) return 86;
        if (Connect(k3, dst3, V4Size) != PAL_SUCCESS) return 87;
        len = V4Size;
        IntPtr a3;
        if (Accept(lst3, outAddr, &len, &a3) != PAL_SUCCESS) return 88;
        if (Close(a3) != 0) return 89;
        if (Close(k3) != 0) return 90;
        if (Close(lst3) != 0) return 91;

        IntPtr lst4 = Make(SOCK_STREAM, PT_TCP);
        if (lst4 == (IntPtr)(-1)) return 92;
        if (!Address(blob, V4Size, Loopback, 0)) return 93;
        if (Bind(lst4, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 94;
        if (Listen(lst4, -1) != PAL_SUCCESS) return 95;
        ushort listen4Port = PortOf(lst4);
        if (listen4Port == 0) return 96;
        byte* dst4 = stackalloc byte[V4Size];
        if (!Address(dst4, V4Size, Loopback, listen4Port)) return 97;
        IntPtr k4 = Make(SOCK_STREAM, PT_TCP);
        if (k4 == (IntPtr)(-1)) return 98;
        if (Connect(k4, dst4, V4Size) != PAL_SUCCESS) return 99;
        len = V4Size;
        IntPtr a4;
        if (Accept(lst4, outAddr, &len, &a4) != PAL_SUCCESS) return 100;
        if (Close(a4) != 0) return 101;
        if (Close(k4) != 0) return 102;
        if (Close(lst4) != 0) return 103;

        // A replacement listener binds over established children when its
        // bind carries SO_REUSEADDR (every PT_TCP bind does), because their
        // pcbs are keyed by the full peer tuple; a flagless PT_UNSPECIFIED
        // bind is EADDRINUSE. Darwin's exact-duplicate refusal exempts
        // exactly the established phase.
        IntPtr lst5 = Make(SOCK_STREAM, PT_TCP);
        if (lst5 == (IntPtr)(-1)) return 104;
        if (!Address(blob, V4Size, Loopback, 0)) return 104;
        if (Bind(lst5, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 104;
        if (Listen(lst5, 8) != PAL_SUCCESS) return 104;
        ushort listen5Port = PortOf(lst5);
        if (listen5Port == 0) return 104;
        byte* dst5 = stackalloc byte[V4Size];
        if (!Address(dst5, V4Size, Loopback, listen5Port)) return 104;
        IntPtr k5 = Make(SOCK_STREAM, PT_TCP);
        if (k5 == (IntPtr)(-1)) return 104;
        if (Connect(k5, dst5, V4Size) != PAL_SUCCESS) return 104;
        len = V4Size;
        IntPtr a5;
        if (Accept(lst5, outAddr, &len, &a5) != PAL_SUCCESS) return 104;
        if (Close(lst5) != 0) return 104;
        IntPtr bare = Make(SOCK_STREAM, PT_TCP);
        if (bare == (IntPtr)(-1)) return 105;
        if (Bind(bare, PT_UNSPECIFIED, dst5, V4Size) != PAL_EADDRINUSE) return 106;
        if (Close(bare) != 0) return 106;
        IntPtr replacement = Make(SOCK_STREAM, PT_TCP);
        if (replacement == (IntPtr)(-1)) return 107;
        if (Bind(replacement, PT_TCP, dst5, V4Size) != PAL_SUCCESS) return 108;
        if (Listen(replacement, 8) != PAL_SUCCESS) return 109;
        if (Close(replacement) != 0) return 110;
        if (Close(a5) != 0) return 110;
        if (Close(k5) != 0) return 110;

        return 0;
    }
}
