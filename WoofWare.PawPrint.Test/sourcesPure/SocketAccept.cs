using System;
using System.Runtime.InteropServices;

// `SystemNative_Accept` (pal_networking.c:1705) reached by hand-rolled
// P/Invoke. No modelled operation can put a connection into a backlog --
// `SystemNative_Connect` does not exist -- so every row here is an error
// answer; the success path is exercised nowhere until `Connect` lands.
//
// Differential, so PAL return codes and out-pointer effects only: every row
// was measured to answer identically on macOS 26 and Linux 6.18 (the raw
// errno numbers differ -- 11/35 for EAGAIN, 88/38 for ENOTSOCK, 95/102 for
// EOPNOTSUPP -- and those live in `SocketAcceptLinux.cs` /
// `SocketAcceptDarwin.cs`, under PawPrint alone where the flavour is known).
//
// The facts pinned deliberately:
//
//   * the wrapper's EFAULT screens -- either address NULL, the out-pointer
//     NULL, or a negative declared length -- precede everything, the
//     descriptor lookup included, and store nothing;
//   * every *syscall* failure stores -1 through `acceptedSocket` before
//     returning the PAL error, and touches neither the address buffer nor
//     the declared length;
//   * the kind check beats the listening check (a datagram socket is also
//     "not listening", and answers EOPNOTSUPP on both kernels);
//   * the listening check beats blocking behaviour (a *blocking*
//     non-listening socket answers EINVAL immediately rather than parking);
//   * `O_NONBLOCK` is read from the open file description, so an accept
//     through a `dup` of the non-blocking listener answers EAGAIN too;
//   * a non-socket descriptor answers ENOTSOCK from `Accept` -- and from
//     `Bind`, `Listen` and `GetSockName`, measured per entry point.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketAccept
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

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
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOTSOCK = 0x1003C;
    const int PAL_EOPNOTSUPP = 0x1003D;

    // PAL numbering, which is not any platform's.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int PT_TCP = 6;
    const int PT_UDP = 17;

    const int V4Size = 16;

    // `INADDR_LOOPBACK` in network order, as `SetIPv4Address` takes it.
    const uint Loopback = 0x0100007F;

    // A value no descriptor allocation can hand back, so a surviving sentinel
    // is proof the callee did not store.
    static readonly IntPtr Sentinel = (IntPtr)77;

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

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];
        byte* outAddr = stackalloc byte[V4Size];

        // --- a non-blocking listener with an empty backlog ---
        IntPtr s = Make(SOCK_STREAM, PT_TCP);
        if (s == (IntPtr)(-1)) return 1;
        if (!Address(blob, Loopback, 0)) return 2;
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 3;
        if (Listen(s, 8) != PAL_SUCCESS) return 4;
        if (SetIsNonBlocking(s, 1) != 0) return 5;

        // --- the wrapper's EFAULT screens, which precede everything and
        //     store nothing ---
        int len = V4Size;
        IntPtr acc = Sentinel;
        if (Accept(s, null, &len, &acc) != PAL_EFAULT) return 6;
        if (acc != Sentinel) return 7;
        if (Accept(s, outAddr, null, &acc) != PAL_EFAULT) return 8;
        if (Accept(s, outAddr, &len, null) != PAL_EFAULT) return 9;

        len = -5;
        if (Accept(s, outAddr, &len, &acc) != PAL_EFAULT) return 10;
        if (len != -5) return 11;
        if (acc != Sentinel) return 12;

        // The screens also precede any look at the descriptor: a pointer
        // masquerading as the fd never reaches the lookup.
        len = V4Size;
        if (Accept((IntPtr)(&len), null, &len, &acc) != PAL_EFAULT) return 46;

        // --- EAGAIN, and what a failure leaves untouched ---
        for (int i = 0; i < V4Size; i++) outAddr[i] = 0xEE;
        len = V4Size;
        acc = Sentinel;
        if (Accept(s, outAddr, &len, &acc) != PAL_EAGAIN) return 13;
        if (acc != (IntPtr)(-1)) return 14;
        if (len != V4Size) return 15;
        for (int i = 0; i < V4Size; i++) { if (outAddr[i] != 0xEE) return 16; }

        // A declared length of zero changes nothing: the address is only
        // examined on success, and there is no success to have.
        len = 0;
        if (Accept(s, outAddr, &len, &acc) != PAL_EAGAIN) return 17;
        if (len != 0) return 18;

        // `O_NONBLOCK` lives on the open file description, so the dup answers
        // EAGAIN too.
        IntPtr d = Dup(s);
        if (d == (IntPtr)(-1)) return 19;
        len = V4Size;
        if (Accept(d, outAddr, &len, &acc) != PAL_EAGAIN) return 20;
        if (Close(d) != 0) return 21;

        // --- not listening: EINVAL, before any blocking behaviour ---
        IntPtr t = Make(SOCK_STREAM, PT_TCP);
        if (t == (IntPtr)(-1)) return 22;

        // Unbound and *blocking*: the kernel answers rather than parking.
        len = V4Size;
        acc = Sentinel;
        if (Accept(t, outAddr, &len, &acc) != PAL_EINVAL) return 23;
        if (acc != (IntPtr)(-1)) return 24;

        // Bound and non-blocking: still EINVAL, not EAGAIN.
        if (!Address(blob, Loopback, 0)) return 25;
        if (Bind(t, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 26;
        if (SetIsNonBlocking(t, 1) != 0) return 27;
        if (Accept(t, outAddr, &len, &acc) != PAL_EINVAL) return 28;
        if (Close(t) != 0) return 29;

        // --- a datagram socket: EOPNOTSUPP, and the kind check beats both
        //     the listening check and blocking behaviour (the socket is
        //     blocking here) ---
        IntPtr u = Make(SOCK_DGRAM, PT_UDP);
        if (u == (IntPtr)(-1)) return 30;
        len = V4Size;
        acc = Sentinel;
        if (Accept(u, outAddr, &len, &acc) != PAL_EOPNOTSUPP) return 31;
        if (acc != (IntPtr)(-1)) return 32;
        if (Close(u) != 0) return 33;

        // --- a non-socket descriptor: ENOTSOCK, from all four entry points ---
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 34;
        len = V4Size;
        acc = Sentinel;
        if (Accept(port, outAddr, &len, &acc) != PAL_ENOTSOCK) return 35;
        if (acc != (IntPtr)(-1)) return 36;
        if (!Address(blob, Loopback, 0)) return 37;
        if (Bind(port, PT_TCP, blob, V4Size) != PAL_ENOTSOCK) return 38;
        if (Listen(port, 8) != PAL_ENOTSOCK) return 39;
        int portLen = V4Size;
        if (GetSockName(port, outAddr, &portLen) != PAL_ENOTSOCK) return 40;
        if (Close(port) != 0) return 41;

        // --- a dead descriptor: EBADF, with the EFAULT screens still first ---
        if (Close(s) != 0) return 42;
        len = V4Size;
        acc = Sentinel;
        if (Accept(s, outAddr, &len, &acc) != PAL_EBADF) return 43;
        if (acc != (IntPtr)(-1)) return 44;
        if (Accept(s, null, &len, &acc) != PAL_EFAULT) return 45;

        return 0;
    }
}
