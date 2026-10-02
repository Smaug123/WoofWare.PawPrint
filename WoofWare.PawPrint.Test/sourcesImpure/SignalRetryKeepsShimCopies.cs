using System;
using System.Runtime.InteropServices;
using System.Threading;

// A shim function that a signal interrupts calls its syscall again with what it
// copied out of the caller's memory before its loop, not with what that memory
// holds by then.
//
// SystemNative_Accept copies `*socketAddressLen` into its own `addrLen` before
// its `accept4` loop, and Common_Poll (SystemNative_Poll) converts the caller's
// `PollEvent`s into its own `struct pollfd`s before its `poll` loop. Here
// another thread rewrites each while the main thread sleeps in the call, and
// then sends SIGTERM, which the kernel delivers to the main thread: accept
// restarts (System.Native installs its handler with SA_RESTART) and poll fails
// with EINTR and is called again. Read afresh, the rewritten length (-1) would
// fail the accept with EFAULT, and the rewritten descriptor (-1) would make the
// poll ignore the listener and time out; with the copies, a connection made
// after the signal is accepted, and reported to the poll.
//
// The cells live in native memory so that another thread may legally write
// them while the call sleeps. On the real runtime the 100 ms before the rewrite
// is far more than the main thread needs to enter the call.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Poll")]
    static extern unsafe int Poll(byte* pollEvents, uint eventCount, int milliseconds, uint* triggered);

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
    const short PAL_POLLIN = 0x0001;
    const int SIGTERM = 15;
    const uint LoopbackNetworkOrder = 0x0100007F;

    static int Handled;

    static unsafe void Loopback(byte* addr, ushort port)
    {
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, AF_INET);
        SetIPv4Address(addr, 16, LoopbackNetworkOrder);
        SetPort(addr, 16, port);
    }

    static unsafe IntPtr MakeListener(out ushort port)
    {
        port = 0;
        byte* addr = stackalloc byte[16];
        IntPtr listener;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &listener) != PAL_SUCCESS) return (IntPtr)(-1);
        Loopback(addr, 0);
        if (Bind(listener, PT_TCP, addr, 16) != PAL_SUCCESS) return (IntPtr)(-1);
        if (Listen(listener, 8) != PAL_SUCCESS) return (IntPtr)(-1);
        int len = 16;
        if (GetSockName(listener, addr, &len) != PAL_SUCCESS) return (IntPtr)(-1);
        ushort p;
        if (GetPort(addr, 16, &p) != PAL_SUCCESS) return (IntPtr)(-1);
        port = p;
        return listener;
    }

    static unsafe void ConnectTo(ushort port)
    {
        byte* addr = stackalloc byte[16];
        Loopback(addr, port);
        IntPtr client;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &client) != PAL_SUCCESS) return;
        Connect(client, addr, 16);
    }

    // While the main thread sleeps: rewrite `cell`, signal the process, wait
    // for the handler, give the main thread time to call again, and connect.
    static unsafe Thread Meddle(int* cell, int handledBefore, ushort port)
    {
        IntPtr cellAddress = (IntPtr)cell;
        Thread t = new Thread(() =>
        {
            Thread.Sleep(100);
            *(int*)cellAddress = -1;
            Kill(Environment.ProcessId, SIGTERM);
            for (int i = 0; i < 3000 && Volatile.Read(ref Handled) <= handledBefore; i++) Thread.Sleep(10);
            Thread.Sleep(100);
            ConnectTo(port);
        });
        t.Start();
        return t;
    }

    static unsafe int Main()
    {
        var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                Interlocked.Increment(ref Handled);
            });

        // The accept, its length cell rewritten to -1.
        IntPtr acceptListener = MakeListener(out ushort acceptPort);
        if (acceptListener == (IntPtr)(-1)) return 1;
        byte* peer = (byte*)Marshal.AllocHGlobal(16);
        int* lengthCell = (int*)Marshal.AllocHGlobal(4);
        IntPtr* acceptedCell = (IntPtr*)Marshal.AllocHGlobal(8);
        *lengthCell = 16;
        Thread meddler = Meddle(lengthCell, 0, acceptPort);
        int accepted = Accept(acceptListener, peer, lengthCell, acceptedCell);
        meddler.Join();
        if (accepted != PAL_SUCCESS) return 2;
        if (*lengthCell != 16) return 3;

        // The poll, its descriptor rewritten to -1.
        IntPtr pollListener = MakeListener(out ushort pollPort);
        if (pollListener == (IntPtr)(-1)) return 4;
        byte* pollEvent = (byte*)Marshal.AllocHGlobal(8);
        *(int*)pollEvent = (int)pollListener;
        *(short*)(pollEvent + 4) = PAL_POLLIN;
        *(short*)(pollEvent + 6) = 0;
        uint* triggered = (uint*)Marshal.AllocHGlobal(4);
        *triggered = 99;
        meddler = Meddle((int*)pollEvent, 1, pollPort);
        int polled = Poll(pollEvent, 1, 5000, triggered);
        meddler.Join();
        if (polled != PAL_SUCCESS) return 5;
        if (*triggered != 1) return 6;
        if ((*(short*)(pollEvent + 6) & PAL_POLLIN) == 0) return 7;

        GC.KeepAlive(registration);
        return 0;
    }
}
