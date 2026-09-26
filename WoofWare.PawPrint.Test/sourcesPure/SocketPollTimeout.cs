using System;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Threading;

// `Socket.Poll` with a positive or infinite timeout on a descriptor that is not
// ready, which reaches `SystemNative_Poll` and sleeps in `poll(2)`.
//
// The descriptor is a listening socket, whose read-readiness is "the accept
// queue is nonempty" on both kernels (see `SocketPoll.cs`), so every row here
// holds on either flavour. Measured on Linux 6.18.5 and Darwin 27.0.0
// (`docs/plans/2026-08-23-posix-kernel-extraction/poll-timeout.c`): a wait that
// times out returns nothing and never before its timeout, and a descriptor that
// becomes ready ends the wait at once.
//
// Under PawPrint the waits are on the virtual clock: with every thread asleep,
// the clock jumps to the poll's deadline, or to the connecting thread's sleep.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketPollTimeout
{
    static int Main()
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        var endpoint = new IPEndPoint(IPAddress.Loopback, ((IPEndPoint)listener.LocalEndPoint).Port);

        // Nothing connects, so the wait times out: false, and not before 200ms.
        var started = Stopwatch.GetTimestamp();
        if (listener.Poll(200_000, SelectMode.SelectRead)) return 1;
        if (Stopwatch.GetElapsedTime(started).TotalMilliseconds < 200) return 2;

        // A connection 50ms into a ten-second wait ends it long before its
        // deadline.
        using var early = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        var connector = new Thread(() =>
        {
            Thread.Sleep(50);
            early.Connect(endpoint);
        });
        started = Stopwatch.GetTimestamp();
        connector.Start();
        if (!listener.Poll(10_000_000, SelectMode.SelectRead)) return 3;
        if (Stopwatch.GetElapsedTime(started).TotalMilliseconds >= 5_000) return 4;
        connector.Join();

        using var firstAccepted = listener.Accept();
        if (listener.Poll(0, SelectMode.SelectRead)) return 5;

        // An infinite wait (-1) ends when a connection arrives.
        using var late = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        connector = new Thread(() =>
        {
            Thread.Sleep(50);
            late.Connect(endpoint);
        });
        connector.Start();
        if (!listener.Poll(-1, SelectMode.SelectRead)) return 6;
        connector.Join();

        using var secondAccepted = listener.Accept();
        return 0;
    }
}
