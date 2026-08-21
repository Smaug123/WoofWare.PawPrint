using System;
using System.Runtime.InteropServices;

// Rebinding a listener's endpoint while accepted connections stay open — the
// server-restart shape. Measured on both kernels (probe10.c, 2026-08-21):
// with SO_REUSEADDR on the replacement (which `SystemNative_Bind` sets for
// every PT_TCP bind), the bind and the re-listen both succeed even though
// established children still hold the endpoint, because their pcbs are keyed
// by the full peer tuple; without the flag (a PT_UNSPECIFIED bind), both
// kernels answer EADDRINUSE.
//
// Differential: every row agrees on macOS 26 and Linux 6.18.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketRebind
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern unsafe int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    const int PAL_SUCCESS = 0;
    const int PAL_EADDRINUSE = 0x10003;

    // PAL numbering, which is not any platform's.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;
    const int PT_UNSPECIFIED = 0;

    const int V4Size = 16;
    const uint Loopback = 0x0100007F;

    static unsafe IntPtr Make()
    {
        IntPtr fd;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &fd) != PAL_SUCCESS) return (IntPtr)(-1);
        return fd;
    }

    static unsafe bool Address(byte* blob, ushort port)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, Loopback) == PAL_SUCCESS;
    }

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];
        byte* outAddr = stackalloc byte[V4Size];

        // A listener with an established child: bind (PT_TCP, so the PAL sets
        // SO_REUSEADDR), listen, connect, accept — then close the listener.
        IntPtr lst = Make();
        if (lst == (IntPtr)(-1)) return 1;
        if (!Address(blob, 0)) return 2;
        if (Bind(lst, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 3;
        if (Listen(lst, 8) != PAL_SUCCESS) return 4;
        int len = V4Size;
        if (GetSockName(lst, outAddr, &len) != PAL_SUCCESS) return 5;
        ushort port;
        if (GetPort(outAddr, len, &port) != PAL_SUCCESS) return 6;
        if (port == 0) return 7;
        byte* dst = stackalloc byte[V4Size];
        if (!Address(dst, port)) return 8;
        for (int i = 0; i < V4Size; i++) dst[i] = blob[i];
        if (SetPort(dst, V4Size, port) != PAL_SUCCESS) return 9;

        IntPtr client = Make();
        if (client == (IntPtr)(-1)) return 10;
        if (Connect(client, dst, V4Size) != PAL_SUCCESS) return 11;
        len = V4Size;
        IntPtr accepted;
        if (Accept(lst, outAddr, &len, &accepted) != PAL_SUCCESS) return 12;
        if (Close(lst) != 0) return 13;

        // A PT_UNSPECIFIED bind carries no SO_REUSEADDR, and the established
        // children refuse it on both kernels.
        IntPtr bare = Make();
        if (bare == (IntPtr)(-1)) return 14;
        if (Bind(bare, PT_UNSPECIFIED, dst, V4Size) != PAL_EADDRINUSE) return 15;
        if (Close(bare) != 0) return 16;

        // The PT_TCP replacement carries the flag, and binds and listens over
        // the established children on both kernels.
        IntPtr replacement = Make();
        if (replacement == (IntPtr)(-1)) return 17;
        if (Bind(replacement, PT_TCP, dst, V4Size) != PAL_SUCCESS) return 18;
        if (Listen(replacement, 8) != PAL_SUCCESS) return 19;

        if (Close(replacement) != 0) return 20;
        if (Close(accepted) != 0) return 21;
        if (Close(client) != 0) return 22;

        return 0;
    }
}
