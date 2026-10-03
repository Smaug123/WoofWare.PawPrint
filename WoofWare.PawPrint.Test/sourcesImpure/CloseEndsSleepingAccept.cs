using System;
using System.Runtime.InteropServices;
using System.Threading;

// A thread asleep in a blocking `accept` on a listener, and another closing the
// listener's only descriptor under it 50 ms in, then connecting to its port 50 ms
// after that.
//
// Darwin ends the accept at the close with ECONNABORTED, and the listener is gone by
// the time the close returns, so the connect is refused. Linux leaves the accept
// asleep on the listener, which it holds, and the connect completes it. The exit
// code is 0 for Linux's answer and 100 for Darwin's, so the registration states each
// flavour's; any other value names the check that failed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept", SetLastError = true)]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    // Interop.Error, as `pal_errno.h` numbers it.
    const int PalSuccess = 0;
    const int PalEConnAborted = 0x1000D;
    const int PalEConnRefused = 0x1000E;

    // The shim's own numbering of the family, type and protocol.
    const int AfInet = 2;
    const int SockStream = 1;
    const int PtTcp = 6;

    const int V4Size = 16;
    const uint Loopback = 0x0100007F;

    static unsafe IntPtr MakeStreamSocket()
    {
        IntPtr fd;
        if (Socket(AfInet, SockStream, PtTcp, &fd) != PalSuccess) return (IntPtr)(-1);
        return fd;
    }

    static unsafe int Main()
    {
        byte[] address = new byte[V4Size];
        IntPtr listener = MakeStreamSocket();
        if (listener == (IntPtr)(-1)) return 1;

        fixed (byte* blob = address)
        {
            if (SetAddressFamily(blob, V4Size, AfInet) != PalSuccess) return 2;
            if (SetPort(blob, V4Size, 0) != PalSuccess) return 3;
            if (SetIPv4Address(blob, V4Size, Loopback) != PalSuccess) return 4;
            if (Bind(listener, PtTcp, blob, V4Size) != PalSuccess) return 5;
            if (Listen(listener, 8) != PalSuccess) return 6;

            // The port the kernel chose.
            int length = V4Size;
            if (GetSockName(listener, blob, &length) != PalSuccess) return 7;
        }

        int closed = -2;
        int connected = -2;
        IntPtr client = (IntPtr)(-1);

        Thread closer = new Thread(() =>
        {
            Thread.Sleep(50);
            closed = Close(listener);
            Thread.Sleep(50);
            client = MakeStreamSocket();
            fixed (byte* blob = address)
            {
                connected = Connect(client, blob, V4Size);
            }
        });
        closer.Start();

        byte* peer = stackalloc byte[V4Size];
        int peerLength = V4Size;
        IntPtr accepted;
        int rv = Accept(listener, peer, &peerLength, &accepted);
        int error = ConvertErrorPlatformToPal(Marshal.GetLastPInvokeError());

        closer.Join();
        if (closed != PalSuccess) return 8;
        if (client == (IntPtr)(-1)) return 9;

        if (rv == PalSuccess)
        {
            if (connected != PalSuccess) return 10;
            if (Close(accepted) != 0) return 11;
            if (Close(client) != 0) return 12;
            return 0;
        }

        if (rv != PalEConnAborted) return 13;
        if (error != PalEConnAborted) return 14;
        if (connected != PalEConnRefused) return 15;
        if (Close(client) != 0) return 16;
        return 100;
    }
}
