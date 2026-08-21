using System;
using System.Runtime.InteropServices;

// The raw errno numbers `SystemNative_Accept`'s failures leave for a
// `SetLastError = true` import, under the Darwin flavour. The classification
// itself is flavour-independent -- measured, both kernels agree on every row
// -- so `SocketAccept.cs` carries the PAL return codes differentially, and
// this pair exists because three of the numbers are not portable: EAGAIN is
// 35 here and 11 on Linux, ENOTSOCK is 38 against 88, EOPNOTSUPP 102
// against 95.
//
// The ENOTSOCK rows also cover `Bind`, `Listen` and `GetSockName`: the
// answer was measured per entry point (regular file, event port and both
// pipe ends, on both kernels) rather than generalised from `accept(2)`.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketAcceptDarwin
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind", SetLastError = true)]
    static extern unsafe int BindReportingErrno(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen", SetLastError = true)]
    static extern int ListenReportingErrno(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName", SetLastError = true)]
    static extern unsafe int GetSockNameReportingErrno(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept", SetLastError = true)]
    static extern unsafe int AcceptReportingErrno(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

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

    const int PAL_SUCCESS = 0;
    const int PAL_EAGAIN = 0x10006;
    const int PAL_EBADF = 0x10008;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOTSOCK = 0x1003C;
    const int PAL_EOPNOTSUPP = 0x1003D;

    // Darwin's numbering.
    const int EBADF = 9;
    const int EAGAIN = 35;
    const int EINVAL = 22;
    const int ENOTSOCK = 38;
    const int EOPNOTSUPP = 102;

    const int AF_INET = 2;
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

    static unsafe bool Address(byte* blob)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, 0) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, Loopback) == PAL_SUCCESS;
    }

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];
        byte* outAddr = stackalloc byte[V4Size];
        int len;
        IntPtr acc;

        // --- EAGAIN's number, from a non-blocking listener ---
        IntPtr s = Make(SOCK_STREAM, PT_TCP);
        if (s == (IntPtr)(-1)) return 1;
        if (!Address(blob)) return 2;
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 3;
        if (Listen(s, 8) != PAL_SUCCESS) return 4;
        if (SetIsNonBlocking(s, 1) != 0) return 5;

        len = V4Size;
        if (AcceptReportingErrno(s, outAddr, &len, &acc) != PAL_EAGAIN) return 6;
        if (Marshal.GetLastSystemError() != EAGAIN) return 7;

        // --- EINVAL's number, from a bound non-listening socket ---
        IntPtr t = Make(SOCK_STREAM, PT_TCP);
        if (t == (IntPtr)(-1)) return 8;
        if (!Address(blob)) return 9;
        if (Bind(t, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 10;
        len = V4Size;
        if (AcceptReportingErrno(t, outAddr, &len, &acc) != PAL_EINVAL) return 11;
        if (Marshal.GetLastSystemError() != EINVAL) return 12;
        if (Close(t) != 0) return 13;

        // --- EOPNOTSUPP's number, from a datagram socket ---
        IntPtr u = Make(SOCK_DGRAM, PT_UDP);
        if (u == (IntPtr)(-1)) return 14;
        len = V4Size;
        if (AcceptReportingErrno(u, outAddr, &len, &acc) != PAL_EOPNOTSUPP) return 15;
        if (Marshal.GetLastSystemError() != EOPNOTSUPP) return 16;
        if (Close(u) != 0) return 17;

        // --- ENOTSOCK's number, from each of the four entry points ---
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 18;

        len = V4Size;
        if (AcceptReportingErrno(port, outAddr, &len, &acc) != PAL_ENOTSOCK) return 19;
        if (Marshal.GetLastSystemError() != ENOTSOCK) return 20;

        if (!Address(blob)) return 21;
        if (BindReportingErrno(port, PT_TCP, blob, V4Size) != PAL_ENOTSOCK) return 22;
        if (Marshal.GetLastSystemError() != ENOTSOCK) return 23;

        if (ListenReportingErrno(port, 8) != PAL_ENOTSOCK) return 24;
        if (Marshal.GetLastSystemError() != ENOTSOCK) return 25;

        len = V4Size;
        if (GetSockNameReportingErrno(port, outAddr, &len) != PAL_ENOTSOCK) return 26;
        if (Marshal.GetLastSystemError() != ENOTSOCK) return 27;

        if (Close(port) != 0) return 28;

        // --- EBADF's number, from the closed listener ---
        if (Close(s) != 0) return 29;
        len = V4Size;
        if (AcceptReportingErrno(s, outAddr, &len, &acc) != PAL_EBADF) return 30;
        if (Marshal.GetLastSystemError() != EBADF) return 31;

        return 0;
    }
}
