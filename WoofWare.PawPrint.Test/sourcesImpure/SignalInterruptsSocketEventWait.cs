using System;
using System.Runtime.InteropServices;
using System.Threading;

// A signal interrupts the main thread's wait for socket events.
//
// The kernel delivers a signal sent to the process to its main thread, which
// here is asleep in SystemNative_WaitForSocketEvents: epoll_wait on Linux,
// kevent on macOS. Both fail with EINTR when a handler interrupts them, even
// under the SA_RESTART System.Native installs its handler with, and the shim
// calls them again in a loop. So the call still returns the event that ends the
// wait, and the only trace the interruption leaves is errno: EINTR (4 on both
// kernels) from the interrupted attempt, which the attempt that succeeds leaves
// alone, and which the P/Invoke stub then reports as the last P/Invoke error.
// A wait that was never interrupted reports 0, the stub having cleared errno
// before the call.
//
// The event comes from the signal handler itself: a thread sends SIGTERM every
// 100 ms, and the third handler run connects to the listener the port watches.
// So by the time there is an event, the main thread has been signalled while
// waiting, however late it started waiting.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents", SetLastError = true)]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern unsafe int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    const int PAL_SUCCESS = 0;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;
    const int SA_READ = 0x01;
    const int SIGTERM = 15;
    const int EINTR = 4;
    const uint LoopbackNetworkOrder = 0x0100007F;
    const int EventSize = 16;

    static ushort ListenerPort;
    static int Handled;

    static unsafe void Loopback(byte* addr, ushort port)
    {
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, AF_INET);
        SetIPv4Address(addr, 16, LoopbackNetworkOrder);
        SetPort(addr, 16, port);
    }

    // Run by the runtime's signal-handling thread, for each SIGTERM.
    static unsafe void OnSigTerm(PosixSignalContext context)
    {
        context.Cancel = true;
        if (Interlocked.Increment(ref Handled) == 3)
        {
            byte* addr = stackalloc byte[16];
            Loopback(addr, ListenerPort);
            IntPtr client;
            if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return;
            Connect(client, addr, 16);
        }
    }

    static void SendUntilHandled()
    {
        int pid = Environment.ProcessId;
        for (int i = 0; i < 50 && Volatile.Read(ref Handled) < 3; i++)
        {
            Thread.Sleep(100);
            if (Volatile.Read(ref Handled) >= 3) break;
            Kill(pid, SIGTERM);
        }
    }

    static unsafe int Main()
    {
        byte* addr = stackalloc byte[16];
        IntPtr listener;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &listener) != PAL_SUCCESS) return 1;
        Loopback(addr, 0);
        if (Bind(listener, PT_TCP, addr, 16) != PAL_SUCCESS) return 2;
        if (Listen(listener, 8) != PAL_SUCCESS) return 3;
        int len = 16;
        if (GetSockName(listener, addr, &len) != PAL_SUCCESS) return 4;
        ushort port;
        if (GetPort(addr, 16, &port) != PAL_SUCCESS) return 5;
        ListenerPort = port;

        IntPtr eventPort;
        if (CreateSocketEventPort(&eventPort) != PAL_SUCCESS) return 6;
        if (TryChange(eventPort, listener, 0, SA_READ, (IntPtr)7) != PAL_SUCCESS) return 7;

        // Kept for the life of the process, so that a signal the sender sends
        // after the wait has ended is handled too, rather than taking SIGTERM's
        // default.
        var registration = PosixSignalRegistration.Create(PosixSignal.SIGTERM, OnSigTerm);

        Thread sender = new Thread(SendUntilHandled);
        sender.Start();

        byte* buffer = stackalloc byte[4 * EventSize];
        int count = 4;
        int rv = WaitForSocketEvents(eventPort, buffer, &count);
        int lastError = Marshal.GetLastPInvokeError();

        sender.Join();
        GC.KeepAlive(registration);

        if (rv != PAL_SUCCESS) return 8;
        if (count != 1) return 9;
        if (*(ulong*)buffer != 7UL) return 10;
        if (lastError != EINTR) return 11;
        return 0;
    }
}
