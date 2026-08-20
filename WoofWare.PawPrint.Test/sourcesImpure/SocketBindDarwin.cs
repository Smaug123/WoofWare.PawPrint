using System;
using System.Runtime.InteropServices;

// The `bind(2)` rows where the two flavours answer differently, under PawPrint
// alone, where the flavour is known. `SocketBindScreens.cs` carries everything
// they agree on; nothing here could go in a differential guest, because the
// oracle would be whichever platform the suite happens to run on.
//
// Three kinds of divergence appear, and they are not the same kind:
//
//   1. **Which addresses are local.** Linux gives `lo` the whole of
//      `127.0.0.0/8`, so `127.9.9.9` binds there; Darwin assigns loopback
//      exactly one address and answers EADDRNOTAVAIL.
//   2. **Which declared lengths are accepted.** Linux takes anything from the
//      family's own `sizeof` up to `sizeof(struct sockaddr_storage)`; Darwin
//      insists on exactly 16 for an IPv4 blob.
//   3. **Which of two simultaneous faults is reported.** This is the subtle one:
//      the flavours check in different orders, so the *same* call answers
//      differently without either kernel disagreeing about any individual rule.
//      A rebind to a non-local address is EADDRNOTAVAIL on Linux (address checked
//      first) and EINVAL on Darwin (already-bound checked first).
//
// ...and one that is about `SO_REUSEADDR` rather than about addresses at all:
// with the flag set on both sockets, Linux permits a duplicate until one of them
// listens, while Darwin permits the wildcard and a specific address to coexist
// but never an exact duplicate. These are close to opposites.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class SocketBindDarwin
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

    const int PAL_SUCCESS = 0;
    const int PAL_EADDRINUSE = 0x10003;
    const int PAL_EADDRNOTAVAIL = 0x10004;
    const int PAL_EACCES = 0x10002;
    const int PAL_EAFNOSUPPORT = 0x10005;
    const int PAL_EINVAL = 0x1001C;

    const int AF_INET = 2;
    const int AF_INET6 = 23;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;

    const int V4Size = 16;
    const int V6Size = 28;

    const uint Loopback = 0x0100007F;

    static unsafe IntPtr Make()
    {
        IntPtr fd;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &fd) != PAL_SUCCESS) return (IntPtr) (-1);
        return fd;
    }

    static unsafe bool Address(byte* blob, uint address, ushort port)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, address) == PAL_SUCCESS;
    }

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[64];
        byte* readBack = stackalloc byte[V4Size];

        // 1. An address inside the loopback prefix but not the loopback address.
        {
            IntPtr s = Make();
            if (s == (IntPtr) (-1)) return 1;
            // 127.9.9.9: the bytes 7F 09 09 09, as SetIPv4Address takes them.
            uint alias = 0x0909097Fu;
            if (!Address(blob, alias, 0)) return 2;
            if (Bind(s, PT_TCP, blob, V4Size) != PAL_EADDRNOTAVAIL) return 3;
            Close(s);
        }

        // 2. A declared length one byte longer than the struct.
        {
            IntPtr s = Make();
            if (s == (IntPtr) (-1)) return 4;
            if (!Address(blob, Loopback, 0)) return 5;
            if (Bind(s, PT_TCP, blob, 17) != PAL_EINVAL) return 6;
            Close(s);
        }

        // 3. Fault order: a rebind to a non-local address. Both faults hold, and
        //    the flavours report different ones.
        {
            IntPtr s = Make();
            if (s == (IntPtr) (-1)) return 7;
            if (!Address(blob, Loopback, 0)) return 8;
            if (Bind(s, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 9;
            // 8.8.8.8, which no flavour holds.
            if (!Address(blob, 0x08080808u, 0)) return 10;
            if (Bind(s, PT_TCP, blob, V4Size) != PAL_EINVAL) return 11;
            Close(s);
        }

        // 4. Fault order: a blob whose family is wrong *and* whose declared
        //    length is short. Linux reports the length, Darwin the family.
        {
            IntPtr s = Make();
            if (s == (IntPtr) (-1)) return 12;
            for (int i = 0; i < V6Size; i++) blob[i] = 0;
            if (SetAddressFamily(blob, V6Size, AF_INET6) != PAL_SUCCESS) return 13;
            if (Bind(s, PT_TCP, blob, 8) != PAL_EAFNOSUPPORT) return 14;
            Close(s);
        }

        // 5. SO_REUSEADDR: two TCP binds to the exact same address, neither
        //    listening. Linux permits it; Darwin refuses the duplicate.
        {
            IntPtr first = Make();
            if (first == (IntPtr) (-1)) return 15;
            if (!Address(blob, Loopback, 0)) return 16;
            if (Bind(first, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 17;

            int len = V4Size;
            if (GetSockName(first, readBack, &len) != PAL_SUCCESS) return 18;
            ushort port = 0;
            if (GetPort(readBack, V4Size, &port) != PAL_SUCCESS) return 19;

            IntPtr second = Make();
            if (second == (IntPtr) (-1)) return 20;
            if (!Address(blob, Loopback, port)) return 21;
            if (Bind(second, PT_TCP, blob, V4Size) != PAL_EADDRINUSE) return 22;
            Close(first);
            Close(second);
        }

        // 6. SO_REUSEADDR: a listening wildcard, then a specific address on the
        //    same port. Linux refuses; Darwin permits.
        {
            IntPtr wildcard = Make();
            if (wildcard == (IntPtr) (-1)) return 23;
            if (!Address(blob, 0u, 0)) return 24;
            if (Bind(wildcard, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 25;

            int len = V4Size;
            if (GetSockName(wildcard, readBack, &len) != PAL_SUCCESS) return 26;
            ushort port = 0;
            if (GetPort(readBack, V4Size, &port) != PAL_SUCCESS) return 27;
            if (Listen(wildcard, 8) != PAL_SUCCESS) return 28;

            IntPtr specific = Make();
            if (specific == (IntPtr) (-1)) return 29;
            if (!Address(blob, Loopback, port)) return 30;
            if (Bind(specific, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 31;
            Close(wildcard);
            Close(specific);
        }

        // 7. A privileged port, as the default non-root uid. The two flavours
        //    agree, but the *host* running a differential guest need not be
        //    non-root, so this can only be asserted here.
        {
            IntPtr s = Make();
            if (s == (IntPtr) (-1)) return 32;
            if (!Address(blob, Loopback, 80)) return 33;
            if (Bind(s, PT_TCP, blob, V4Size) != PAL_EACCES) return 34;
            // ...and 1024 is the first port that is not privileged.
            if (!Address(blob, Loopback, 1024)) return 35;
            if (Bind(s, PT_TCP, blob, V4Size) != PAL_SUCCESS) return 36;
            Close(s);
        }

        return 0;
    }
}
