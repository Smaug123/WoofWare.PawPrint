using System;
using System.Runtime.InteropServices;
using System.Threading;

// `epoll_wait` uses the `maxevents` it was *entered* with: overwriting the
// count cell while the wait is parked changes nothing, because the value was
// passed by the time the syscall began. The overwrite here would be fatal if
// re-read — 0 is rejected before any wait — so a runtime that re-evaluated the
// cell at wake-up would answer EINVAL instead of delivering the event.
//
// Differential: the same holds of kevent's `nevents` on macOS, so this runs on
// both real flavours. The count cell lives in native memory so the entry
// thread can legally overwrite it while the waiter is parked; the Volatile
// flag plus the sleep is what keeps the overwrite after the park on the real
// runtime (the flag is set just before the wait is entered, and 100 ms is far
// more than the gap between the two), and under PawPrint the sleep yields to
// the waiter deterministically.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketEventWaitCountCapture
{
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
    const uint LoopbackNetworkOrder = 0x0100007F;
    const int EventSize = 16;

    static IntPtr Port;
    static IntPtr CountCell;
    static volatile int AboutToWait;
    static volatile int WaitOutcome;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[8 * EventSize];
        AboutToWait = 1;
        int rc = WaitForSocketEvents(Port, buffer, (int*)CountCell);
        if (rc != PAL_SUCCESS) { WaitOutcome = 1; return; }
        if (*(int*)CountCell != 1) { WaitOutcome = 2; return; }
        if (*(UIntPtr*)buffer != (UIntPtr)0x77UL) { WaitOutcome = 3; return; }
        WaitOutcome = 4;
    }

    static unsafe int Main()
    {
        IntPtr listener;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &listener) != PAL_SUCCESS) return 1;
        byte* addr = stackalloc byte[16];
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, AF_INET);
        SetIPv4Address(addr, 16, LoopbackNetworkOrder);
        SetPort(addr, 16, 0);
        if (Bind(listener, PT_TCP, addr, 16) != PAL_SUCCESS) return 2;
        if (Listen(listener, 8) != PAL_SUCCESS) return 3;
        int len = 16;
        if (GetSockName(listener, addr, &len) != PAL_SUCCESS) return 4;

        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 5;
        Port = port;
        if (TryChange(port, listener, 0, SA_READ | SA_WRITE, (IntPtr)0x77) != PAL_SUCCESS) return 6;

        CountCell = Marshal.AllocHGlobal(4);
        *(int*)CountCell = 8;

        Thread waiter = new Thread(Waiter);
        waiter.Start();

        while (AboutToWait == 0) Thread.Sleep(10);
        Thread.Sleep(100);

        // Fatal if re-read: a zero maxevents never waits.
        *(int*)CountCell = 0;

        IntPtr client;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return 7;
        if (Connect(client, addr, 16) != PAL_SUCCESS) return 8;

        if (!waiter.Join(10_000)) return 9;
        return WaitOutcome == 4 ? 0 : 10 + WaitOutcome;
    }
}
