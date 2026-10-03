using System;
using System.Runtime.InteropServices;
using System.Threading;

// `SystemNative_Poll`'s Darwin-flavour rows, the counterpart of `SocketPollLinux.cs`.
//
// Darwin builds `poll(2)` over kqueue: each entry registers a filter per group of
// requested bits (EVFILT_READ for IN/PRI/HUP, EVFILT_WRITE for OUT), and the call
// translates the filters' reports back into revents. So nothing is reported that was
// not asked for -- not even HUP or NVAL -- one descriptor named twice reports into
// the later entry alone, and a reported HUP suppresses OUT. Every expectation is
// measured on Darwin 27.0.0 (docs/plans/2026-08-23-posix-kernel-extraction,
// `poll-darwin.c` and `poll-entry-interplay.c`), and the guest is compared with real
// .NET on a macOS host.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class SocketPollDarwin
{
    [StructLayout(LayoutKind.Sequential)]
    struct PollEvent
    {
        public int FileDescriptor;
        public short Events;
        public short TriggeredEvents;
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Poll")]
    static extern unsafe int Poll(PollEvent* pollEvents, uint eventCount, int milliseconds, uint* triggered);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern unsafe int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_EINPROGRESS = 0x1001A;

    const int PAL_AF_INET = 2;
    const int PAL_SOCK_STREAM = 1;
    const int PAL_PT_TCP = 6;

    const int V4Size = 16;

    // `INADDR_LOOPBACK` in network order, as `SetIPv4Address` takes it.
    const uint Loopback = 0x0100007F;

    const short POLLIN = 0x0001;
    const short POLLPRI = 0x0002;
    const short POLLOUT = 0x0004;
    const short POLLERR = 0x0008;
    const short POLLHUP = 0x0010;
    const short POLLNVAL = 0x0020;

    // Well past anything the fd table hands out, and never opened.
    const int NeverOpened = 4096;

    static unsafe bool Address(byte* blob, uint address, ushort port)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, PAL_AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, address) == PAL_SUCCESS;
    }

    static unsafe ushort PortOf(IntPtr socket)
    {
        byte* blob = stackalloc byte[V4Size];
        int len = V4Size;
        if (GetSockName(socket, blob, &len) != PAL_SUCCESS) return 0;
        ushort port;
        if (GetPort(blob, V4Size, &port) != PAL_SUCCESS) return 0;
        return port;
    }

    static unsafe IntPtr NewSocket()
    {
        IntPtr created;
        if (Socket(PAL_AF_INET, PAL_SOCK_STREAM, PAL_PT_TCP, &created) != PAL_SUCCESS) return IntPtr.Zero;
        return created;
    }

    // A listener on a loopback port of the kernel's choosing, or zero.
    static unsafe IntPtr NewListener(out ushort port)
    {
        port = 0;
        IntPtr listener = NewSocket();
        if (listener == IntPtr.Zero) return IntPtr.Zero;
        byte* address = stackalloc byte[V4Size];
        if (!Address(address, Loopback, 0)) return IntPtr.Zero;
        if (Bind(listener, PAL_PT_TCP, address, V4Size) != PAL_SUCCESS) return IntPtr.Zero;
        if (Listen(listener, 4) != PAL_SUCCESS) return IntPtr.Zero;
        port = PortOf(listener);
        return port == 0 ? IntPtr.Zero : listener;
    }

    // Connect `socket` to loopback `port`, answering the PAL result.
    static unsafe int ConnectTo(IntPtr socket, ushort port)
    {
        byte* address = stackalloc byte[V4Size];
        if (!Address(address, Loopback, port)) return -1;
        return Connect(socket, address, V4Size);
    }

    // One entry polled with `events` at `timeout`: its revents, or -1 if the call
    // failed or `*triggered` did not count it as revents says it should.
    static unsafe int PollOne(IntPtr fd, short events, int timeout)
    {
        PollEvent one = new PollEvent { FileDescriptor = (int)fd, Events = events };
        uint triggered;
        if (Poll(&one, 1, timeout, &triggered) != PAL_SUCCESS) return -1;
        if (triggered != (one.TriggeredEvents != 0 ? 1u : 0u)) return -1;
        return one.TriggeredEvents;
    }

    static ushort s_lateConnectPort;

    // Connects to `s_lateConnectPort` after the main thread has gone to sleep in a
    // poll of the listener.
    static void LateConnect()
    {
        Thread.Sleep(50);
        IntPtr client = NewSocket();
        if (client != IntPtr.Zero) ConnectTo(client, s_lateConnectPort);
    }

    static unsafe int Main()
    {
        uint triggered;
        PollEvent one;

        // 1-2: the wrapper's own screens, answered in user space, as on Linux.
        triggered = 12345;
        if (Poll(null, 0, 0, &triggered) != PAL_EFAULT) return 1;
        if (triggered != 12345) return 2;

        IntPtr idle = NewSocket();
        if (idle == IntPtr.Zero) return 3;

        // 4: `milliseconds < -1` is the wrapper's EINVAL.
        one = new PollEvent { FileDescriptor = (int)idle, Events = POLLIN };
        if (Poll(&one, 1, -2, &triggered) != PAL_EINVAL) return 4;

        // 5: an idle TCP socket presents nothing to either filter, where Linux's
        // presents OUT|HUP.
        if (PollOne(idle, POLLIN | POLLOUT, 0) != 0) return 5;

        // 6-7: nothing is reported that was not asked for: a request of nothing
        // registers no filter, so even a descriptor that is not open reports
        // nothing, and is not counted. Asked for IN, it fails to register, and
        // answers NVAL.
        if (PollOne((IntPtr)NeverOpened, 0, 0) != 0) return 6;
        if (PollOne((IntPtr)NeverOpened, POLLIN, 0) != POLLNVAL) return 7;

        // 8-9: a listener with a connection queued: its READ is ready, its WRITE
        // never is.
        IntPtr listener = NewListener(out ushort listenerPort);
        if (listener == IntPtr.Zero) return 8;
        IntPtr client = NewSocket();
        if (client == IntPtr.Zero) return 9;
        if (ConnectTo(client, listenerPort) != PAL_SUCCESS) return 10;
        if (PollOne(listener, POLLOUT, 0) != 0) return 11;
        if (PollOne(listener, POLLIN, 0) != POLLIN) return 12;
        if (PollOne(listener, POLLIN | POLLOUT, 5000) != POLLIN) return 13;

        // 14-17: one descriptor named by two entries reports into the later one
        // alone, and is counted once; a negative descriptor beside them is
        // ignored.
        PollEvent* many = stackalloc PollEvent[3];
        many[0] = new PollEvent { FileDescriptor = (int)listener, Events = POLLIN };
        many[1] = new PollEvent { FileDescriptor = -1, Events = POLLIN };
        many[2] = new PollEvent { FileDescriptor = (int)listener, Events = POLLIN };
        if (Poll(many, 3, 0, &triggered) != PAL_SUCCESS) return 14;
        if (triggered != 1) return 15;
        if (many[0].TriggeredEvents != 0 || many[1].TriggeredEvents != 0) return 16;
        if (many[2].TriggeredEvents != POLLIN) return 17;

        // 18-23: the connected pair: WRITE ready, READ not. Then the peer closes,
        // and a poll for IN sleeps until its FIN arrives: the READ reports EOF,
        // which is HUP. Asked for IN|OUT, the HUP the READ reports first
        // suppresses OUT; asked for OUT alone, the same socket answers OUT.
        IntPtr accepted;
        byte* peer = stackalloc byte[V4Size];
        int peerLength = V4Size;
        if (Accept(listener, peer, &peerLength, &accepted) != PAL_SUCCESS) return 18;
        if (PollOne(client, POLLIN | POLLOUT, 0) != POLLOUT) return 19;
        if (Close(accepted) != PAL_SUCCESS) return 20;
        if (PollOne(client, POLLIN, 5000) != (POLLIN | POLLHUP)) return 21;
        if (PollOne(client, POLLIN | POLLOUT, 0) != (POLLIN | POLLHUP)) return 42;
        if (PollOne(client, POLLOUT, 0) != POLLOUT) return 22;
        if (PollOne(client, POLLPRI, 0) != (POLLPRI | POLLHUP)) return 23;

        // 24-28: a refused connect. Both filters report EOF, so OUT alone -- what
        // `SocketPal.TryCompleteConnect` asks -- answers HUP and no OUT, and a
        // request of nothing answers nothing, where Linux reports ERR|HUP unasked.
        IntPtr dead = NewListener(out ushort deadPort);
        if (dead == IntPtr.Zero) return 24;
        if (Close(dead) != PAL_SUCCESS) return 25;
        IntPtr refused = NewSocket();
        if (refused == IntPtr.Zero) return 26;
        if (SetIsNonBlocking(refused, 1) != PAL_SUCCESS) return 27;
        if (ConnectTo(refused, deadPort) != PAL_EINPROGRESS) return 28;
        // Sleeps until the refusal arrives, which activates the WRITE filter.
        if (PollOne(refused, POLLOUT, 5000) != POLLHUP) return 29;
        if (PollOne(refused, POLLIN | POLLOUT, 0) != (POLLIN | POLLHUP)) return 30;
        if (PollOne(refused, 0, 0) != 0) return 31;
        if (PollOne(refused, POLLERR, 0) != 0) return 32;

        // 33: a sleeping poll, woken by a connection another thread makes.
        IntPtr sleeper = NewListener(out ushort sleeperPort);
        if (sleeper == IntPtr.Zero) return 33;
        s_lateConnectPort = sleeperPort;
        var connector = new Thread(LateConnect);
        connector.Start();
        if (PollOne(sleeper, POLLIN, 5000) != POLLIN) return 34;
        connector.Join();

        // 35: a sleeping poll that nothing wakes returns 0 at its timeout.
        if (PollOne(idle, POLLIN | POLLOUT, 20) != 0) return 35;

        // 36-37: more entries than OPEN_MAX (10240) is the kernel's EINVAL,
        // whatever they hold.
        PollEvent[] tooMany = new PollEvent[10241];
        for (int i = 0; i < tooMany.Length; i++) tooMany[i] = new PollEvent { FileDescriptor = -1, Events = POLLIN };
        fixed (PollEvent* entries = tooMany)
        {
            if (Poll(entries, (uint)tooMany.Length, 0, &triggered) != PAL_EINVAL) return 36;
            if (Poll(entries, 1024, 0, &triggered) != PAL_SUCCESS || triggered != 0) return 37;
        }

        if (Close(refused) != PAL_SUCCESS) return 38;
        if (Close(client) != PAL_SUCCESS) return 39;
        if (Close(listener) != PAL_SUCCESS) return 40;
        if (Close(idle) != PAL_SUCCESS) return 41;

        return 0;
    }
}
