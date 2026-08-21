using System;
using System.Runtime.InteropServices;

// `SystemNative_Bind`, `SystemNative_Listen` and `SystemNative_GetSockName`
// reached by hand-rolled P/Invoke, for the half of their contract the managed
// surface cannot express.
//
// Two things force this. Raising a `SocketException` runs
// `SystemNative_ConvertErrorPalToPlatform`, which is unimplemented, so a managed
// guest aborts while *constructing* the exception rather than reporting the
// error -- every refusal below is unreachable from `SocketBindListen.cs`. And
// managed code never passes a null out-parameter, a negative length, or a
// declared length shorter than the address, so three of the screens have no
// managed caller at all.
//
// The blobs are built with the `SocketAddressPal` setters rather than by writing
// bytes, which keeps this guest differential: a `sockaddr_in`'s family is two
// bytes at offset 0 on Linux and one at offset 1 on Darwin, so a guest that laid
// out its own bytes would be asserting the host's layout against PawPrint's
// emulated one. The setters answer in whichever layout is in force.
//
// Every row was measured to answer identically on macOS 26.6 and Linux 6.x.
// Rows that do *not* agree are excluded, and there are more of them than the
// shared screens suggest: the order in which `bind(2)` reports two simultaneous
// faults is itself per-flavour (Linux checks the declared length before the
// family and defers "already bound" until after the address is validated; Darwin
// does the opposite of both), so no row here presents two faults at once.
// `SocketBindLinux.cs` and `SocketBindDarwin.cs` carry those.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class SocketBindScreens
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern unsafe int Listen(IntPtr socket, int backlog);

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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetIPv4Address")]
    static extern unsafe int GetIPv4Address(byte* socketAddress, int socketAddressLen, uint* address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetAddressFamily")]
    static extern unsafe int GetAddressFamily(byte* socketAddress, int socketAddressLen, int* addressFamily);

    const int PAL_SUCCESS = 0;
    const int PAL_EADDRINUSE = 0x10003;
    const int PAL_EADDRNOTAVAIL = 0x10004;
    const int PAL_EBADF = 0x10008;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_EOPNOTSUPP = 0x1003D;

    // PAL numbering, which is not any platform's.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_UNSPECIFIED = 0;
    const int PT_TCP = 6;
    const int PT_UDP = 17;

    const int V4Size = 16;

    // `INADDR_LOOPBACK` and a non-local address, both in network order as
    // `SetIPv4Address` takes them.
    const uint Loopback = 0x0100007F;
    const uint Foreign = 0x08080808;

    static unsafe IntPtr Make(int type, int protocol)
    {
        IntPtr fd;
        if (Socket(AF_INET, type, protocol, &fd) != PAL_SUCCESS) return (IntPtr) (-1);
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

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];
        byte* readBack = stackalloc byte[V4Size];

        // --- the shim's own screens, which precede ToFileDescriptor ---
        IntPtr s = Make(SOCK_STREAM, PT_TCP);
        if (s == (IntPtr) (-1)) return 1;

        if (Bind(s, PT_TCP, null, V4Size) != PAL_EFAULT) return 2;

        if (!Address(blob, Loopback, 0)) return 3;
        if (Bind(s, PT_TCP, blob, -1) != PAL_EFAULT) return 4;

        // A closed descriptor is EBADF -- but only once the blob has passed, so
        // a null blob on a closed descriptor is still EFAULT.
        IntPtr dead = Make(SOCK_STREAM, PT_TCP);
        if (dead == (IntPtr) (-1)) return 5;
        Close(dead);

        if (Bind(dead, PT_TCP, null, V4Size) != PAL_EFAULT) return 6;
        if (Bind(dead, PT_TCP, blob, V4Size) != PAL_EBADF) return 7;
        if (Listen(dead, 16) != PAL_EBADF) return 8;

        int deadLen = V4Size;
        if (GetSockName(dead, readBack, &deadLen) != PAL_EBADF) return 9;

        // --- a pointer that is not null but names nothing ---
        // The wrapper passes the blob straight to `bind(2)` without touching it,
        // so the fault is the kernel's and comes back as EFAULT rather than
        // killing the process. `getsockname(2)`'s output buffer is the same.
        // Measured with `(struct sockaddr *) 1` on both platforms.
        if (Bind(s, PT_TCP, (byte*) 1, V4Size) != PAL_EFAULT) return 63;

        int strayLen = V4Size;
        if (GetSockName(s, (byte*) 1, &strayLen) != PAL_EFAULT) return 64;

        // ...but only when bytes actually move. `bind(2)` copies the caller's
        // `len` bytes *before* judging whether `len` is a legal sockaddr length,
        // so a zero-length call never reads the pointer and falls through to the
        // length fault; and `getsockname` with a declared length of zero copies
        // nothing, succeeds, and still reports the real length. Both measured on
        // macOS and Linux.
        if (Bind(s, PT_TCP, (byte*) 1, 0) != PAL_EINVAL) return 65;

        int zeroStray = 0;
        if (GetSockName(s, (byte*) 1, &zeroStray) != PAL_SUCCESS) return 66;
        if (zeroStray != V4Size) return 67;

        // --- a length the struct does not fit in ---
        if (Bind(s, PT_TCP, blob, 8) != PAL_EINVAL) return 10;

        // --- an address no interface holds ---
        if (!Address(blob, Foreign, 0)) return 11;
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_EADDRNOTAVAIL) return 12;

        // --- getsockname on a socket that has never been bound ---
        int len = V4Size;
        for (int i = 0; i < V4Size; i++) readBack[i] = 0xEE;
        if (GetSockName(s, readBack, &len) != PAL_SUCCESS) return 13;
        if (len != V4Size) return 14;

        int family = -7;
        if (GetAddressFamily(readBack, V4Size, &family) != PAL_SUCCESS) return 15;
        if (family != AF_INET) return 16;

        ushort unboundPort = 7;
        if (GetPort(readBack, V4Size, &unboundPort) != PAL_SUCCESS) return 17;
        if (unboundPort != 0) return 18;

        uint unboundAddress = 7;
        if (GetIPv4Address(readBack, V4Size, &unboundAddress) != PAL_SUCCESS) return 19;
        if (unboundAddress != 0) return 20;

        // --- a successful bind, and what getsockname then reports ---
        if (!Address(blob, Loopback, 0)) return 21;
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 22;

        len = V4Size;
        if (GetSockName(s, readBack, &len) != PAL_SUCCESS) return 23;

        ushort boundPort = 0;
        if (GetPort(readBack, V4Size, &boundPort) != PAL_SUCCESS) return 24;
        if (boundPort == 0) return 25;
        if (boundPort < 1024) return 26;

        uint boundAddress = 0;
        if (GetIPv4Address(readBack, V4Size, &boundAddress) != PAL_SUCCESS) return 27;
        if (boundAddress != Loopback) return 28;

        // --- binding a second time ---
        if (!Address(blob, Loopback, 0)) return 29;
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_EINVAL) return 30;

        // --- getsockname reports the real length even when it truncated ---
        // The C asserts `addrLen <= *socketAddressLen` and then stores the real
        // length back regardless; the assert is false on both platforms and is
        // compiled out of the shipped build.
        for (int i = 0; i < V4Size; i++) readBack[i] = 0xEE;
        int shortLen = 8;
        if (GetSockName(s, readBack, &shortLen) != PAL_SUCCESS) return 31;
        if (shortLen != V4Size) return 32;
        // The bytes past the declared length were not touched...
        for (int i = 8; i < V4Size; i++) { if (readBack[i] != 0xEE) return 33; }
        // ...and the port, which lives at offset 2, was.
        ushort truncatedPort = 0;
        for (int i = 8; i < V4Size; i++) readBack[i] = 0;
        if (GetPort(readBack, V4Size, &truncatedPort) != PAL_SUCCESS) return 34;
        if (truncatedPort != boundPort) return 35;

        // A declared length of zero writes nothing at all, and still reports 16.
        for (int i = 0; i < V4Size; i++) readBack[i] = 0xEE;
        int zeroLen = 0;
        if (GetSockName(s, readBack, &zeroLen) != PAL_SUCCESS) return 36;
        if (zeroLen != V4Size) return 37;
        for (int i = 0; i < V4Size; i++) { if (readBack[i] != 0xEE) return 38; }

        // --- listen, and binding after it ---
        if (Listen(s, 16) != PAL_SUCCESS) return 39;
        if (!Address(blob, Loopback, 0)) return 40;
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_EINVAL) return 41;

        // --- a second socket on a listening socket's exact address ---
        IntPtr rival = Make(SOCK_STREAM, PT_TCP);
        if (rival == (IntPtr) (-1)) return 42;
        if (!Address(blob, Loopback, boundPort)) return 43;
        if (Bind(rival, PT_TCP, blob, V4Size) != PAL_EADDRINUSE) return 44;

        // ...while a UDP socket takes that port happily: separate namespaces.
        IntPtr datagram = Make(SOCK_DGRAM, PT_UDP);
        if (datagram == (IntPtr) (-1)) return 45;
        if (Bind(datagram, PT_UDP, blob, V4Size) != PAL_SUCCESS) return 46;

        // --- listen refuses a datagram socket ---
        if (Listen(datagram, 16) != PAL_EOPNOTSUPP) return 47;

        // --- two PT_UNSPECIFIED sockets contend, because neither bind set
        //     SO_REUSEADDR: the flag is a property of the bind call, not of the
        //     socket, and this is the row that shows it.
        IntPtr plainFirst = Make(SOCK_STREAM, PT_UNSPECIFIED);
        if (plainFirst == (IntPtr) (-1)) return 48;
        if (!Address(blob, Loopback, 0)) return 49;
        if (Bind(plainFirst, PT_UNSPECIFIED, blob, V4Size) != PAL_SUCCESS) return 50;

        len = V4Size;
        if (GetSockName(plainFirst, readBack, &len) != PAL_SUCCESS) return 51;
        ushort plainPort = 0;
        if (GetPort(readBack, V4Size, &plainPort) != PAL_SUCCESS) return 52;

        IntPtr plainSecond = Make(SOCK_STREAM, PT_UNSPECIFIED);
        if (plainSecond == (IntPtr) (-1)) return 53;
        if (!Address(blob, Loopback, plainPort)) return 54;
        if (Bind(plainSecond, PT_UNSPECIFIED, blob, V4Size) != PAL_EADDRINUSE) return 55;

        // --- listen(2) binds an unbound socket implicitly, to the wildcard ---
        IntPtr implicitly = Make(SOCK_STREAM, PT_TCP);
        if (implicitly == (IntPtr) (-1)) return 56;
        if (Listen(implicitly, 8) != PAL_SUCCESS) return 57;

        len = V4Size;
        if (GetSockName(implicitly, readBack, &len) != PAL_SUCCESS) return 58;

        uint implicitAddress = 7;
        if (GetIPv4Address(readBack, V4Size, &implicitAddress) != PAL_SUCCESS) return 59;
        if (implicitAddress != 0) return 60;

        ushort implicitPort = 0;
        if (GetPort(readBack, V4Size, &implicitPort) != PAL_SUCCESS) return 61;
        if (implicitPort == 0) return 62;

        Close(s);
        Close(rival);
        Close(datagram);
        Close(plainFirst);
        Close(plainSecond);
        Close(implicitly);
        return 0;
    }
}
