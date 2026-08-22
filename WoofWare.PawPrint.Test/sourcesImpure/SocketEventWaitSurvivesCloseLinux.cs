using System;
using System.Runtime.InteropServices;
using System.Threading;

// A blocked `epoll_wait` holds the port by file reference, not by descriptor:
// closing the fd the wait was called through — while a dup keeps the
// description alive — changes nothing, and the wait still completes when the
// edge arrives. A runtime that re-resolved the fd at wake-up would answer
// EBADF instead of delivering.
//
// Linux-flavour only, and that is measured rather than assumed: this same
// guest run on real macOS exits 13 — kevent *does* end the wait with an error
// when the fd it was entered through closes — so the kernels diverge here and
// the Darwin-flavoured kernel refuses such a close instead. Validated on real
// Linux .NET, exit 0. The Volatile flag plus the sleep keeps the close after
// the park on the real runtime, and under PawPrint the sleep yields to the
// waiter deterministically.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketEventWaitSurvivesCloseLinux
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldfd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

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

    static IntPtr Alias;
    static volatile int AboutToWait;
    static volatile int WaitOutcome;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[8 * EventSize];
        int count = 8;
        AboutToWait = 1;
        int rc = WaitForSocketEvents(Alias, buffer, &count);
        if (rc != PAL_SUCCESS) { WaitOutcome = 1; return; }
        if (count != 1) { WaitOutcome = 2; return; }
        if (*(UIntPtr*)buffer != (UIntPtr)0x99UL) { WaitOutcome = 3; return; }
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
        if (TryChange(port, listener, 0, SA_READ | SA_WRITE, (IntPtr)0x99) != PAL_SUCCESS) return 6;

        IntPtr alias = Dup(port);
        if ((long)alias < 0) return 7;
        Alias = alias;

        Thread waiter = new Thread(Waiter);
        waiter.Start();

        while (AboutToWait == 0) Thread.Sleep(10);
        Thread.Sleep(100);

        // The description survives through `port`; only the fd the waiter
        // passed dies.
        if (Close(alias) != PAL_SUCCESS) return 8;

        IntPtr client;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return 9;
        if (Connect(client, addr, 16) != PAL_SUCCESS) return 10;

        if (!waiter.Join(10_000)) return 11;
        return WaitOutcome == 4 ? 0 : 12 + WaitOutcome;
    }
}
