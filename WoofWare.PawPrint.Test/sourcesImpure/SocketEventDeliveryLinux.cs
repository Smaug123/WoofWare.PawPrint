using System;
using System.Runtime.InteropServices;

// The Linux-flavour rows of the socket readiness delivery: exact `Events`
// masks (shaped by the PAL's EPOLLHUP fold, pal_networking.c's
// ConvertEventEPollToSocketAsync), batch order, and truncation. Every row is
// measured on Linux 6.18.5 (probes in docs/plans/2026-08-21-socket-readiness-
// wake/) and validated by running this guest on real Linux .NET, exit 0,
// before the implementation existed; macOS's kqueue path differs in both
// element size and mask shape, so this lives under PawPrint's Linux flavour
// alone.
//
// The facts pinned deliberately:
//
//   * an idle stream socket's level is OUT|HUP, and the fold delivers it as
//     READ|WRITE (0x3);
//   * a pending refusal's level is IN|OUT|ERR|HUP|RDHUP; under full interest
//     the fold delivers 0x17 (READ|WRITE|READCLOSE|ERROR), and under
//     READ-only interest 0x13 — RDHUP is maskable, ERR and HUP are not;
//   * the refusal's delivering connect resets the socket and re-signals: the
//     next wait reports the idle 0x3;
//   * the batch is in edge-arrival order; a re-signal does not move a pending
//     entry; an ADD of an already-ready target enters at ADD time; one edge
//     reaching two registrations of the same socket (via dup) delivers
//     newest-registered first;
//   * an interest the level does not meet delivers nothing for that entry
//     while its neighbours still deliver;
//   * `*count` truncates the batch, the remainder arrives on the next wait in
//     order, and the buffer beyond the delivered events is untouched;
//   * a MOD of a consumed, still-ready target re-arms it at MOD time.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketEventDeliveryLinux
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration")]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldfd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    const int PAL_SUCCESS = 0;
    const int PAL_ECONNREFUSED = 0x1000E;
    const int PAL_EINPROGRESS = 0x1001A;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;
    const int SA_READ = 0x01;
    const int SA_WRITE = 0x02;
    const int SA_READCLOSE = 0x04;
    const int SA_CLOSE = 0x08;
    const int SA_ERROR = 0x10;
    const int SA_ALL = 0x1F;

    const uint LoopbackNetworkOrder = 0x0100007F;
    const int EventSize = 16;

    static unsafe IntPtr MakeListener(byte* addr)
    {
        IntPtr listener;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &listener) != PAL_SUCCESS) return (IntPtr)(-1);
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, AF_INET);
        SetIPv4Address(addr, 16, LoopbackNetworkOrder);
        SetPort(addr, 16, 0);
        if (Bind(listener, PT_TCP, addr, 16) != PAL_SUCCESS) return (IntPtr)(-1);
        if (Listen(listener, 8) != PAL_SUCCESS) return (IntPtr)(-1);
        int len = 16;
        if (GetSockName(listener, addr, &len) != PAL_SUCCESS) return (IntPtr)(-1);
        return listener;
    }

    static unsafe IntPtr ConnectOne(byte* addr)
    {
        IntPtr client;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return (IntPtr)(-1);
        if (Connect(client, addr, 16) != PAL_SUCCESS) return (IntPtr)(-1);
        return client;
    }

    // A loopback port with nothing behind it: bind, read the name, close.
    static unsafe bool DeadPort(byte* addr)
    {
        IntPtr tmp;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &tmp) != PAL_SUCCESS) return false;
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, AF_INET);
        SetIPv4Address(addr, 16, LoopbackNetworkOrder);
        SetPort(addr, 16, 0);
        if (Bind(tmp, PT_TCP, addr, 16) != PAL_SUCCESS) return false;
        int len = 16;
        if (GetSockName(tmp, addr, &len) != PAL_SUCCESS) return false;
        return Close(tmp) == PAL_SUCCESS;
    }

    static unsafe ulong DataAt(byte* buffer, int i) => *(ulong*)(buffer + i * EventSize);
    static unsafe int EventsAt(byte* buffer, int i) => *(int*)(buffer + i * EventSize + 8);

    static unsafe int Main()
    {
        byte* buffer = stackalloc byte[8 * EventSize];

        // --- the idle fold: OUT|HUP delivers as READ|WRITE ---
        {
            IntPtr port;
            if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 1;
            IntPtr idle;
            if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &idle) != PAL_SUCCESS) return 2;
            if (TryChange(port, idle, 0, SA_ALL, (IntPtr)1) != PAL_SUCCESS) return 3;
            int count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS) return 4;
            if (count != 1) return 5;
            if (EventsAt(buffer, 0) != (SA_READ | SA_WRITE)) return 6;
            if (Close(idle) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 7;
        }

        // --- the refusal lifecycle, full interest ---
        {
            IntPtr port;
            if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 8;
            byte* dead = stackalloc byte[16];
            if (!DeadPort(dead)) return 9;
            IntPtr client;
            if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return 10;
            if (SetIsNonBlocking(client, 1) != PAL_SUCCESS) return 11;
            if (TryChange(port, client, 0, SA_ALL, (IntPtr)2) != PAL_SUCCESS) return 12;
            int count = 8;
            // Consume the idle edge the ADD-of-ready queued.
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 13;
            if (EventsAt(buffer, 0) != (SA_READ | SA_WRITE)) return 14;

            if (Connect(client, dead, 16) != PAL_EINPROGRESS) return 15;
            count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 16;
            if (EventsAt(buffer, 0) != (SA_READ | SA_WRITE | SA_READCLOSE | SA_ERROR)) return 17;

            // The delivering connect resets the socket, and the reset is a
            // fresh edge whose level is idle again.
            if (Connect(client, dead, 16) != PAL_ECONNREFUSED) return 18;
            count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 19;
            if (EventsAt(buffer, 0) != (SA_READ | SA_WRITE)) return 20;
            if (Close(client) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 21;
        }

        // --- the refusal under READ-only interest: RDHUP is maskable, ERR
        // and HUP are not (and HUP folds) ---
        {
            IntPtr port;
            if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 22;
            byte* dead = stackalloc byte[16];
            if (!DeadPort(dead)) return 23;
            IntPtr client;
            if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return 24;
            if (SetIsNonBlocking(client, 1) != PAL_SUCCESS) return 25;
            if (Connect(client, dead, 16) != PAL_EINPROGRESS) return 26;
            // Registered after the refusal latched: the ADD finds it ready.
            if (TryChange(port, client, 0, SA_READ, (IntPtr)3) != PAL_SUCCESS) return 27;
            int count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 28;
            if (EventsAt(buffer, 0) != (SA_READ | SA_WRITE | SA_ERROR)) return 29;
            if (Close(client) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 30;
        }

        // --- batch order: edge arrival, re-signal immobility, ADD-time entry ---
        {
            // F/G: arrival order, both ways round.
            for (int firstIsL1 = 0; firstIsL1 < 2; firstIsL1++)
            {
                IntPtr port;
                if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 31;
                byte* a1 = stackalloc byte[16];
                byte* a2 = stackalloc byte[16];
                IntPtr l1 = MakeListener(a1);
                IntPtr l2 = MakeListener(a2);
                if (l1 == (IntPtr)(-1) || l2 == (IntPtr)(-1)) return 32;
                if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 33;
                if (TryChange(port, l2, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 34;
                byte* first = firstIsL1 == 1 ? a1 : a2;
                byte* second = firstIsL1 == 1 ? a2 : a1;
                if (ConnectOne(first) == (IntPtr)(-1)) return 35;
                if (ConnectOne(second) == (IntPtr)(-1)) return 36;
                int count = 8;
                if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 2) return 37;
                ulong expectedFirst = firstIsL1 == 1 ? 1UL : 2UL;
                ulong expectedSecond = firstIsL1 == 1 ? 2UL : 1UL;
                if (DataAt(buffer, 0) != expectedFirst) return 38;
                if (DataAt(buffer, 1) != expectedSecond) return 39;
                if (Close(l1) != PAL_SUCCESS || Close(l2) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 40;
            }

            // H: a re-signal of a pending entry does not move it.
            {
                IntPtr port;
                if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 41;
                byte* a1 = stackalloc byte[16];
                byte* a2 = stackalloc byte[16];
                IntPtr l1 = MakeListener(a1);
                IntPtr l2 = MakeListener(a2);
                if (l1 == (IntPtr)(-1) || l2 == (IntPtr)(-1)) return 42;
                if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 43;
                if (TryChange(port, l2, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 44;
                if (ConnectOne(a2) == (IntPtr)(-1)) return 45;
                if (ConnectOne(a1) == (IntPtr)(-1)) return 46;
                if (ConnectOne(a2) == (IntPtr)(-1)) return 47;
                int count = 8;
                if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 2) return 48;
                if (DataAt(buffer, 0) != 2UL) return 49;
                if (DataAt(buffer, 1) != 1UL) return 50;
                if (Close(l1) != PAL_SUCCESS || Close(l2) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 51;
            }

            // I: an ADD of an already-ready target enters at ADD time.
            {
                IntPtr port;
                if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 52;
                byte* a1 = stackalloc byte[16];
                byte* a2 = stackalloc byte[16];
                IntPtr l1 = MakeListener(a1);
                IntPtr l2 = MakeListener(a2);
                if (l1 == (IntPtr)(-1) || l2 == (IntPtr)(-1)) return 53;
                if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 54;
                if (ConnectOne(a2) == (IntPtr)(-1)) return 55;
                if (ConnectOne(a1) == (IntPtr)(-1)) return 56;
                if (TryChange(port, l2, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 57;
                int count = 8;
                if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 2) return 58;
                if (DataAt(buffer, 0) != 1UL) return 59;
                if (DataAt(buffer, 1) != 2UL) return 60;
                if (Close(l1) != PAL_SUCCESS || Close(l2) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 61;
            }

            // R: one edge, two registrations of the same socket via dup,
            // newest-registered first — both registration orders.
            for (int originalFirst = 0; originalFirst < 2; originalFirst++)
            {
                IntPtr port;
                if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 62;
                byte* a1 = stackalloc byte[16];
                IntPtr l1 = MakeListener(a1);
                if (l1 == (IntPtr)(-1)) return 63;
                IntPtr d1 = Dup(l1);
                if ((long)d1 < 0) return 64;
                if (originalFirst == 1)
                {
                    if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 65;
                    if (TryChange(port, d1, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 66;
                }
                else
                {
                    if (TryChange(port, d1, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 65;
                    if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 66;
                }
                if (ConnectOne(a1) == (IntPtr)(-1)) return 67;
                int count = 8;
                if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 2) return 68;
                ulong newest = originalFirst == 1 ? 2UL : 1UL;
                ulong oldest = originalFirst == 1 ? 1UL : 2UL;
                if (DataAt(buffer, 0) != newest) return 69;
                if (DataAt(buffer, 1) != oldest) return 70;
                if (Close(l1) != PAL_SUCCESS || Close(d1) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 71;
            }
        }

        // --- an unmet interest delivers nothing for that entry while its
        // neighbour still delivers ---
        {
            IntPtr port;
            if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 72;
            byte* a1 = stackalloc byte[16];
            byte* a2 = stackalloc byte[16];
            IntPtr l1 = MakeListener(a1);
            IntPtr l2 = MakeListener(a2);
            if (l1 == (IntPtr)(-1) || l2 == (IntPtr)(-1)) return 73;
            // l1 watches WRITE alone: a queued connection raises only IN, and
            // a listener reports no ERR/HUP, so l1's entry is silently
            // dropped at delivery.
            if (TryChange(port, l1, 0, SA_WRITE, (IntPtr)1) != PAL_SUCCESS) return 74;
            if (TryChange(port, l2, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 75;
            if (ConnectOne(a1) == (IntPtr)(-1)) return 76;
            if (ConnectOne(a2) == (IntPtr)(-1)) return 77;
            int count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 78;
            if (DataAt(buffer, 0) != 2UL) return 79;
            if (Close(l1) != PAL_SUCCESS || Close(l2) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 80;
        }

        // --- truncation, and the buffer beyond the batch ---
        {
            IntPtr port;
            if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 81;
            byte* a1 = stackalloc byte[16];
            byte* a2 = stackalloc byte[16];
            byte* a3 = stackalloc byte[16];
            IntPtr l1 = MakeListener(a1);
            IntPtr l2 = MakeListener(a2);
            IntPtr l3 = MakeListener(a3);
            if (l1 == (IntPtr)(-1) || l2 == (IntPtr)(-1) || l3 == (IntPtr)(-1)) return 82;
            if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 83;
            if (TryChange(port, l2, 0, SA_READ, (IntPtr)2) != PAL_SUCCESS) return 84;
            if (TryChange(port, l3, 0, SA_READ, (IntPtr)3) != PAL_SUCCESS) return 85;
            if (ConnectOne(a1) == (IntPtr)(-1)) return 86;
            if (ConnectOne(a2) == (IntPtr)(-1)) return 87;
            if (ConnectOne(a3) == (IntPtr)(-1)) return 88;

            for (int i = 0; i < 8 * EventSize; i++) buffer[i] = 0xEE;
            int count = 2;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 2) return 89;
            if (DataAt(buffer, 0) != 1UL) return 90;
            if (DataAt(buffer, 1) != 2UL) return 91;
            // Only the delivered events' bytes moved.
            for (int i = 2 * EventSize; i < 8 * EventSize; i++)
                if (buffer[i] != 0xEE) return 92;

            count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 93;
            if (DataAt(buffer, 0) != 3UL) return 94;
            if (Close(l1) != PAL_SUCCESS || Close(l2) != PAL_SUCCESS || Close(l3) != PAL_SUCCESS) return 95;
            if (Close(port) != PAL_SUCCESS) return 96;
        }

        // --- MOD of a consumed, still-ready target re-arms it, at MOD time ---
        {
            IntPtr port;
            if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 97;
            byte* a1 = stackalloc byte[16];
            IntPtr l1 = MakeListener(a1);
            if (l1 == (IntPtr)(-1)) return 98;
            if (TryChange(port, l1, 0, SA_READ, (IntPtr)1) != PAL_SUCCESS) return 99;
            if (ConnectOne(a1) == (IntPtr)(-1)) return 100;
            int count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 101;
            // MOD with a widened interest: the still-readable listener
            // re-arms and reports again.
            if (TryChange(port, l1, SA_READ, SA_READ | SA_WRITE, (IntPtr)1) != PAL_SUCCESS) return 102;
            count = 8;
            if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS || count != 1) return 103;
            if (DataAt(buffer, 0) != 1UL) return 104;
            if (EventsAt(buffer, 0) != SA_READ) return 105;
            if (Close(l1) != PAL_SUCCESS || Close(port) != PAL_SUCCESS) return 106;
        }

        return 0;
    }
}
