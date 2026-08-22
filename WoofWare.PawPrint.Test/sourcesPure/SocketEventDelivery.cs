using System;
using System.Runtime.InteropServices;
using System.Threading;

// The socket readiness delivery, reached by hand-rolled P/Invoke: a registered
// listener's accept-queue push is what makes `SystemNative_WaitForSocketEvents`
// return, and the batch carries the registration's `data` verbatim.
//
// Differential, so only rows on which the two real implementations (edge-
// triggered epoll on Linux, EV_CLEAR kqueue on macOS) agree through the PAL:
// each wait here is entered with exactly one event deliverable, so batch
// *order*, the exact `Events` masks (which epoll's EPOLLHUP fold shapes), and
// truncation live in `SocketEventDeliveryLinux.cs` under PawPrint alone, where
// the flavour is known. `Events` is only ever tested for the READ bit, which
// both kernels report for a listener with a queued connection.
//
// The facts pinned deliberately:
//
//   * an ADD of an already-readable listener delivers without any edge
//     arriving after the registration — the wait returns rather than parking;
//   * `data` comes back verbatim in `SocketEvent.Data`, and `*count` comes
//     back as the number of events delivered;
//   * a delivered edge is consumed: the next wait does not re-report it, so a
//     drain-and-refill (a fresh connect) is what makes the next wait return;
//   * a further connect onto a queue that is already nonempty and already
//     reported is a fresh edge (the mask never changed; the signal is what
//     reports);
//   * a waiter parked *before* the edge exists is woken by a connect from
//     another thread — the wake half of the readiness model, which is what
//     `SocketAsyncEngine`'s engine thread does for a living.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketEventDelivery
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    const int PAL_SUCCESS = 0;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;
    const int SA_READ = 0x01;
    const int SA_WRITE = 0x02;

    // 127.0.0.1 in the network order SetIPv4Address expects.
    const uint LoopbackNetworkOrder = 0x0100007F;

    // One SocketEvent is 16 bytes on a 64-bit target: uintptr Data, int32
    // Events, uint32 padding. The buffer is sized in these.
    const int EventSize = 16;

    static unsafe IntPtr MakeListener(byte* addr, out int addrLen)
    {
        addrLen = 16;
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

    static IntPtr WakePort;
    static volatile int WakeResult;

    static unsafe void WakeWaiter()
    {
        byte* buffer = stackalloc byte[8 * EventSize];
        int count = 8;
        if (WaitForSocketEvents(WakePort, buffer, &count) != PAL_SUCCESS) { WakeResult = 1; return; }
        if (count != 1) { WakeResult = 2; return; }
        if (*(UIntPtr*)buffer != (UIntPtr)0x51UL) { WakeResult = 3; return; }
        if ((*(int*)(buffer + 8) & SA_READ) == 0) { WakeResult = 4; return; }
        WakeResult = 5;
    }

    static unsafe int Main()
    {
        byte* addr = stackalloc byte[16];
        int addrLen;
        IntPtr listener = MakeListener(addr, out addrLen);
        if (listener == (IntPtr)(-1)) return 1;

        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 2;

        // Queue a connection *before* the registration exists: the ADD of an
        // already-readable listener delivers with no later edge.
        if (ConnectOne(addr) == (IntPtr)(-1)) return 3;
        if (TryChange(port, listener, 0, SA_READ | SA_WRITE, (IntPtr)0x1234) != PAL_SUCCESS) return 4;

        byte* buffer = stackalloc byte[8 * EventSize];
        int count = 8;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS) return 5;
        if (count != 1) return 6;
        if (*(UIntPtr*)buffer != (UIntPtr)0x1234UL) return 7;
        if ((*(int*)(buffer + 8) & SA_READ) == 0) return 8;

        // The edge is consumed; drain the queue and refill it, and the next
        // wait reports the refill.
        IntPtr accepted;
        int peerLen = 16;
        byte* peer = stackalloc byte[16];
        if (Accept(listener, peer, &peerLen, &accepted) != PAL_SUCCESS) return 9;
        if (ConnectOne(addr) == (IntPtr)(-1)) return 10;
        count = 8;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS) return 11;
        if (count != 1) return 12;
        if (*(UIntPtr*)buffer != (UIntPtr)0x1234UL) return 13;

        // A further connect onto the already-nonempty, already-reported queue
        // is a fresh edge even though the readable level never moved.
        if (ConnectOne(addr) == (IntPtr)(-1)) return 14;
        count = 8;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS) return 15;
        if (count != 1) return 16;

        // The wake: a fresh port and a fresh, empty listener, a waiter parked
        // on the port, and a connect from this thread ending the wait. The
        // registration precedes the waiter's start, so whether the connect
        // lands before or after the waiter reaches the wait, the edge is
        // pending for it — there is no order this can miss.
        byte* addr2 = stackalloc byte[16];
        int addr2Len;
        IntPtr listener2 = MakeListener(addr2, out addr2Len);
        if (listener2 == (IntPtr)(-1)) return 17;
        IntPtr port2;
        if (CreateSocketEventPort(&port2) != PAL_SUCCESS) return 18;
        if (TryChange(port2, listener2, 0, SA_READ | SA_WRITE, (IntPtr)0x51) != PAL_SUCCESS) return 19;

        WakePort = port2;
        Thread waiter = new Thread(WakeWaiter);
        waiter.Start();

        if (ConnectOne(addr2) == (IntPtr)(-1)) return 20;

        // Generous on the real runtime; virtual-clock time under PawPrint.
        if (!waiter.Join(10_000)) return 21;
        if (WakeResult != 5) return 22;

        return 0;
    }
}
