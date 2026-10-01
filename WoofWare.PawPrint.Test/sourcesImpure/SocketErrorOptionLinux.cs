using System;
using System.Runtime.InteropServices;
using System.Threading;

// `SystemNative_GetSocketErrorOption` under the Linux flavour: the shim's
// `getsockopt(SOL_SOCKET, SO_ERROR)` through its own stack buffers, with the
// error it reads converted to the PAL's numbering. Measured on Linux 6.18.5
// with docs/probes/so-error/.
//
//   * a pending refusal reads as ECONNREFUSED once, and the read takes it;
//   * a connect after that answers ECONNABORTED and resets the socket, so the
//     connect after *that* is a fresh attempt answering EINPROGRESS;
//   * a completed connect reads as SUCCESS, and the read leaves the
//     completion for the next connect to report;
//   * the shim answers a NULL out-pointer with EFAULT before it looks at the
//     descriptor, and a failed `getsockopt` with its errno.
//
// The Thread.Sleep calls exist for the real-.NET run: on a real kernel the
// loopback RST or handshake lands just after EINPROGRESS. Under PawPrint the
// outcome is latched at connect time and the sleep only advances the clock.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketErrorOptionLinux
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSocketErrorOption")]
    static extern unsafe int GetSocketErrorOption(IntPtr socket, int* error);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSocketErrorOption", SetLastError = true)]
    static extern unsafe int GetSocketErrorOptionReportingErrno(IntPtr socket, int* error);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

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

    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_ECONNABORTED = 0x1000D;
    const int PAL_ECONNREFUSED = 0x1000E;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINPROGRESS = 0x1001A;
    const int PAL_EISCONN = 0x1001E;
    const int PAL_ENOTSOCK = 0x1003C;

    // Linux's numbering.
    const int EBADF = 9;
    const int ENOTSOCK = 88;

    // PAL numbering, which is not any platform's.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_TCP = 6;
    const int PT_UDP = 17;

    const int V4Size = 16;
    const uint Loopback = 0x0100007F;

    // What the shim leaves in the out cell when it writes nothing.
    const int Untouched = 0x5A5A5A5A;

    static unsafe IntPtr Make(int type, int protocol)
    {
        IntPtr fd;
        if (Socket(AF_INET, type, protocol, &fd) != PAL_SUCCESS) return (IntPtr)(-1);
        return fd;
    }

    static unsafe bool Address(byte* blob, uint address, ushort port)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, address) == PAL_SUCCESS;
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

    // The error `GetSocketErrorOption` stores, or -1 if the call itself failed.
    static unsafe int ErrorOf(IntPtr fd)
    {
        int error = Untouched;
        if (GetSocketErrorOption(fd, &error) != PAL_SUCCESS) return -1;
        return error;
    }

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];

        // --- a listener, and a closed port ---
        IntPtr lst = Make(SOCK_STREAM, PT_TCP);
        if (lst == (IntPtr)(-1)) return 1;
        if (!Address(blob, Loopback, 0)) return 2;
        if (Bind(lst, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 3;
        if (Listen(lst, 8) != PAL_SUCCESS) return 4;
        ushort listenPort = PortOf(lst);
        if (listenPort == 0) return 5;
        byte* dst = stackalloc byte[V4Size];
        if (!Address(dst, Loopback, listenPort)) return 6;

        IntPtr tmp = Make(SOCK_STREAM, PT_TCP);
        if (tmp == (IntPtr)(-1)) return 7;
        if (!Address(blob, Loopback, 0)) return 8;
        if (Bind(tmp, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 9;
        ushort deadPort = PortOf(tmp);
        if (deadPort == 0) return 10;
        if (Close(tmp) != 0) return 11;
        byte* deadDst = stackalloc byte[V4Size];
        if (!Address(deadDst, Loopback, deadPort)) return 12;

        // --- nothing pending reads as SUCCESS: a fresh socket, a listener,
        //     a fresh datagram socket ---
        IntPtr fresh = Make(SOCK_STREAM, PT_TCP);
        if (fresh == (IntPtr)(-1)) return 13;
        if (ErrorOf(fresh) != PAL_SUCCESS) return 14;
        if (ErrorOf(lst) != PAL_SUCCESS) return 15;
        IntPtr udp = Make(SOCK_DGRAM, PT_UDP);
        if (udp == (IntPtr)(-1)) return 16;
        if (ErrorOf(udp) != PAL_SUCCESS) return 17;

        // --- a pending refusal reads once, and the read takes it ---
        IntPtr c1 = Make(SOCK_STREAM, PT_TCP);
        if (c1 == (IntPtr)(-1)) return 18;
        if (SetIsNonBlocking(c1, 1) != 0) return 19;
        if (Connect(c1, deadDst, V4Size) != PAL_EINPROGRESS) return 20;
        Thread.Sleep(100);
        if (ErrorOf(c1) != PAL_ECONNREFUSED) return 21;
        if (ErrorOf(c1) != PAL_SUCCESS) return 22;

        // --- so the next connect aborts and resets, and the one after is a
        //     fresh attempt, refused afresh ---
        if (Connect(c1, deadDst, V4Size) != PAL_ECONNABORTED) return 23;
        if (Connect(c1, deadDst, V4Size) != PAL_EINPROGRESS) return 24;
        Thread.Sleep(100);
        if (ErrorOf(c1) != PAL_ECONNREFUSED) return 25;
        if (Close(c1) != 0) return 26;

        // --- the abort is aimed nowhere in particular: a connect to the live
        //     listener aborts too ---
        IntPtr c2 = Make(SOCK_STREAM, PT_TCP);
        if (c2 == (IntPtr)(-1)) return 27;
        if (SetIsNonBlocking(c2, 1) != 0) return 28;
        if (Connect(c2, deadDst, V4Size) != PAL_EINPROGRESS) return 29;
        Thread.Sleep(100);
        if (ErrorOf(c2) != PAL_ECONNREFUSED) return 30;
        if (Connect(c2, dst, V4Size) != PAL_ECONNABORTED) return 31;
        if (Close(c2) != 0) return 32;

        // --- a completed connect reads as SUCCESS, and the completion is
        //     still there for the next connect to report ---
        IntPtr c3 = Make(SOCK_STREAM, PT_TCP);
        if (c3 == (IntPtr)(-1)) return 33;
        if (SetIsNonBlocking(c3, 1) != 0) return 34;
        if (Connect(c3, dst, V4Size) != PAL_EINPROGRESS) return 35;
        Thread.Sleep(100);
        if (ErrorOf(c3) != PAL_SUCCESS) return 36;
        if (Connect(c3, dst, V4Size) != PAL_SUCCESS) return 37;
        if (Connect(c3, dst, V4Size) != PAL_EISCONN) return 38;
        if (ErrorOf(c3) != PAL_SUCCESS) return 39;

        // --- the shim's NULL screen comes before the descriptor, and sets
        //     no errno ---
        if (GetSocketErrorOption((IntPtr)(-1), null) != PAL_EFAULT) return 40;
        if (GetSocketErrorOption(fresh, null) != PAL_EFAULT) return 41;

        // --- a failed getsockopt answers its errno, and leaves the out cell
        //     alone ---
        IntPtr dead = Make(SOCK_STREAM, PT_TCP);
        if (dead == (IntPtr)(-1)) return 42;
        if (Close(dead) != 0) return 43;
        int cell = Untouched;
        if (GetSocketErrorOptionReportingErrno(dead, &cell) != PAL_EBADF) return 44;
        if (Marshal.GetLastSystemError() != EBADF) return 45;
        if (cell != Untouched) return 46;
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 47;
        if (GetSocketErrorOptionReportingErrno(port, &cell) != PAL_ENOTSOCK) return 48;
        if (Marshal.GetLastSystemError() != ENOTSOCK) return 49;
        if (cell != Untouched) return 50;
        if (Close(port) != 0) return 51;

        return 0;
    }
}
