using System;
using System.Runtime.InteropServices;
using System.Threading;

// The shape SocketAsyncEngine produces: a thread parked in
// `SystemNative_WaitForSocketEvents` while listening sockets are registered
// with the port — before the wait, and again after it. No event can be
// delivered (nothing can reach a listening socket's backlog until
// `SystemNative_Connect` exists), so on both runtimes the waiter simply stays
// parked and the process exits when Main returns; the waiter thread is
// IsBackground for exactly that reason.
//
// This pins the *allow* half of PawPrint's readiness guard: a listening
// stream socket is the one registration whose events PawPrint can rule out,
// so both the park (registrations present before the wait) and the
// registration-past-a-parked-waiter must proceed. The refuse half — a
// registration whose readiness cannot be ruled out — aborts the interpreter,
// which no exit-code guest can observe.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketEventListenerWait
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

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
    const int SA_READ = 0x01;
    const int SA_WRITE = 0x02;
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;
    const int V4Size = 16;
    const uint Loopback = 0x0100007F;

    static IntPtr EventPort;

    static unsafe void Waiter()
    {
        // Parks forever: no event can arrive. A 32-byte buffer covers one
        // event under either backend's stride.
        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(EventPort, buffer, &count);
    }

    static unsafe IntPtr Listener(byte* blob)
    {
        IntPtr s;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &s) != PAL_SUCCESS) return (IntPtr)(-1);
        for (int i = 0; i < V4Size; i++) blob[i] = 0;
        if (SetAddressFamily(blob, V4Size, AF_INET) != PAL_SUCCESS) return (IntPtr)(-1);
        if (SetPort(blob, V4Size, 0) != PAL_SUCCESS) return (IntPtr)(-1);
        if (SetIPv4Address(blob, V4Size, Loopback) != PAL_SUCCESS) return (IntPtr)(-1);
        if (Bind(s, PT_TCP, blob, V4Size) != PAL_SUCCESS) return (IntPtr)(-1);
        if (Listen(s, 8) != PAL_SUCCESS) return (IntPtr)(-1);
        return s;
    }

    static unsafe int Main(string[] args)
    {
        byte* blob = stackalloc byte[V4Size];

        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 1;
        EventPort = port;

        // A listener registered before anything waits.
        IntPtr first = Listener(blob);
        if (first == (IntPtr)(-1)) return 2;
        if (TryChange(port, first, 0, SA_READ | SA_WRITE, (IntPtr)1) != PAL_SUCCESS) return 3;

        var waiter = new Thread(Waiter);
        waiter.IsBackground = true;
        waiter.Start();

        // Long enough for the waiter to reach its park on both runtimes; the
        // registration below exercises the parked-waiter path only if it has,
        // and is the same answer either way.
        Thread.Sleep(300);

        // A second listener registered past the (by now parked) waiter.
        IntPtr second = Listener(blob);
        if (second == (IntPtr)(-1)) return 4;
        if (TryChange(port, second, 0, SA_READ | SA_WRITE, (IntPtr)2) != PAL_SUCCESS) return 5;

        return 0;
    }
}
