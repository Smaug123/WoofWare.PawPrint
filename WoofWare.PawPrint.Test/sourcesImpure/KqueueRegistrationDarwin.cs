using System;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;

// Registrations on a Darwin process's socket event port, which is a kqueue, driven
// through the System.Native shim the way SocketAsyncEngine drives it:
// TryChangeSocketEventRegistration registers a socket for SA_READ and SA_WRITE, which
// is EV_ADD|EV_CLEAR|EV_RECEIPT on EVFILT_READ and EVFILT_WRITE, and
// WaitForSocketEvents takes what the kqueue reports, converting each event to the
// shim's SocketEvents. Configured as macOS, and compared with real .NET on a macOS
// host. The sockets themselves are made through System.Net.Sockets, blocking.
//
// What it pins, each measured on Darwin 27.0.0 by kevent-register.c:
//
//   * a listener holding a connection reports READ, as SA_READ;
//   * a connected socket reports WRITE as soon as it is registered, and READ not
//     at all while its peer is open;
//   * a refused socket reports READ and then WRITE, in changelist order, both with
//     EV_EOF: SA_READ|SA_READCLOSE and SA_WRITE|SA_READ;
//   * the peer closing makes READ report EV_EOF, SA_READ|SA_READCLOSE;
//   * each event carries the data its registration was given;
//   * past the converted SocketEvents, the buffer holds what kevent wrote there
//     (the shim converts each 32-byte struct kevent in place into a 16-byte
//     SocketEvent), apart from an EVFILT_WRITE event's data, the send buffer's
//     free space, which is not asserted;
//   * removing a registration that is not there is ENOENT, and a change that fails
//     leaves the one before it in the same call applied.
//
// Every wait is made only once what it reports is certain to be queued, or about to
// be: a wait with nothing coming would sleep for ever.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents", SetLastError = true)]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration", SetLastError = true)]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    const int PAL_SUCCESS = 0;
    const int PAL_ENOENT = 0x1002D;
    const int ENOENT = 2;

    const int SA_READ = 0x01;
    const int SA_WRITE = 0x02;
    const int SA_READCLOSE = 0x04;

    // The shim lets kevent write one 32-byte struct kevent per event, then converts
    // each in place to a 16-byte SocketEvent: the data, then the SocketEvents mask.
    const int KeventSize = 32;
    const int SocketEventSize = 16;

    static unsafe int Wait(IntPtr port, byte* buffer, long[] data, int[] events)
    {
        // Filled first, so that what the call leaves alone is told apart from
        // what it writes.
        for (int i = 0; i < 4 * KeventSize; i++) buffer[i] = 0xEE;
        int count = 4;
        int result = WaitForSocketEvents(port, buffer, &count);
        if (result != PAL_SUCCESS) return -1000 - result;
        for (int i = 0; i < count; i++)
        {
            data[i] = *(long*)(buffer + i * SocketEventSize);
            events[i] = *(int*)(buffer + i * SocketEventSize + 8);
        }

        return count;
    }

    static unsafe int Main()
    {
        int check;
        byte* buffer = stackalloc byte[4 * KeventSize];
        long[] data = new long[4];
        int[] events = new int[4];

        IntPtr port;
        check = 1;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;

        // Never disposed: a check that fails returns while the listener may still hold
        // a connection, and the exit code should say which check that was.
        var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        var endpoint = (IPEndPoint)listener.LocalEndPoint!;

        // ---- A listener holding a connection reports READ.
        check = 2;
        if (TryChange(port, listener.Handle, 0, SA_READ | SA_WRITE, (IntPtr)7) != PAL_SUCCESS) return check;

        var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect(endpoint);

        check = 3;
        if (Wait(port, buffer, data, events) != 1) return check;
        check = 4;
        if (data[0] != 7 || events[0] != SA_READ) return check;
        // Past the one SocketEvent is the second half of the struct kevent the
        // conversion did not overwrite: its data (the one connection queued) and
        // its udata.
        check = 25;
        if (*(long*)(buffer + 16) != 1 || *(long*)(buffer + 24) != 7) return check;

        // ---- A connected socket reports WRITE as soon as it is registered.
        check = 5;
        if (TryChange(port, client.Handle, 0, SA_READ | SA_WRITE, (IntPtr)8) != PAL_SUCCESS) return check;
        check = 6;
        if (Wait(port, buffer, data, events) != 1) return check;
        check = 7;
        if (data[0] != 8 || events[0] != SA_WRITE) return check;

        // ---- A refused socket reports READ, then WRITE, both at end of file.
        int closedPort;
        using (var probe = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            probe.Bind(new IPEndPoint(IPAddress.Loopback, 0));
            closedPort = ((IPEndPoint)probe.LocalEndPoint!).Port;
        }

        using var refused = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        check = 8;
        try
        {
            refused.Connect(new IPEndPoint(IPAddress.Loopback, closedPort));
            return check;
        }
        catch (SocketException e) when (e.SocketErrorCode == SocketError.ConnectionRefused)
        {
        }

        check = 9;
        if (TryChange(port, refused.Handle, 0, SA_READ | SA_WRITE, (IntPtr)9) != PAL_SUCCESS) return check;
        check = 10;
        if (Wait(port, buffer, data, events) != 2) return check;
        check = 11;
        if (data[0] != 9 || events[0] != (SA_READ | SA_READCLOSE)) return check;
        check = 12;
        if (data[1] != 9 || events[1] != (SA_WRITE | SA_READ)) return check;
        // The two SocketEvents cover the first struct kevent, and the second, the
        // WRITE's, survives whole: its filter, its flags (EV_ADD|EV_CLEAR|
        // EV_RECEIPT with EV_EOF), no pending error (the blocking connect took it),
        // and its udata. Its data, the send buffer's free space, is not asserted.
        check = 26;
        if (*(short*)(buffer + 40) != -2 || *(ushort*)(buffer + 42) != 0x8061) return check;
        check = 27;
        if (*(uint*)(buffer + 44) != 0 || *(long*)(buffer + 56) != 9) return check;

        // ---- Removing a registration, and then removing it again.
        check = 13;
        if (TryChange(port, refused.Handle, SA_READ | SA_WRITE, SA_WRITE, (IntPtr)9) != PAL_SUCCESS) return check;
        check = 14;
        if (TryChange(port, refused.Handle, SA_READ, 0, (IntPtr)9) != PAL_ENOENT) return check;
        check = 15;
        if (Marshal.GetLastPInvokeError() != ENOENT) return check;

        // ---- A change that fails leaves the change before it applied: the shim's
        // changelist is READ then WRITE, and with no room in the eventlist the
        // WRITE's failure ends the call after the READ's ADD has registered it.
        check = 16;
        if (TryChange(port, refused.Handle, SA_WRITE, 0, (IntPtr)9) != PAL_SUCCESS) return check;
        check = 17;
        if (TryChange(port, refused.Handle, SA_WRITE, SA_READ, (IntPtr)11) != PAL_ENOENT) return check;
        check = 18;
        if (Wait(port, buffer, data, events) != 1) return check;
        check = 19;
        if (data[0] != 11 || events[0] != (SA_READ | SA_READCLOSE)) return check;

        // ---- The peer closing makes READ report end of file.
        using var accepted = listener.Accept();
        check = 20;
        if (TryChange(port, accepted.Handle, 0, SA_READ | SA_WRITE, (IntPtr)10) != PAL_SUCCESS) return check;
        check = 21;
        if (Wait(port, buffer, data, events) != 1) return check;
        check = 22;
        if (data[0] != 10 || events[0] != SA_WRITE) return check;

        client.Dispose();

        check = 23;
        if (Wait(port, buffer, data, events) != 1) return check;
        check = 24;
        if (data[0] != 10 || events[0] != (SA_READ | SA_READCLOSE)) return check;

        return 0;
    }
}
