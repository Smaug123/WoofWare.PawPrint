using System;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Threading;

// `Socket.Accept` on a blocking listener with nothing queued, which reaches
// `SystemNative_Accept` and sleeps in `accept(2)` until another thread
// connects.
//
// On a fresh listener the managed `Accept` tries the accept once before it
// arms any wait of its own, and the descriptor is still blocking then, so the
// kernel's own accept is what sleeps. Measured on Linux 6.18.5 and Darwin
// 27.0.0 (`docs/plans/2026-08-23-posix-kernel-extraction/blocking-accept.c`,
// section A): a connection ends the wait at once, with the client's address as
// the peer. Every row here holds on either flavour.
//
// Under PawPrint the connecting thread's sleep is on the virtual clock: with
// the accepting thread asleep in the kernel, the clock jumps to the end of the
// sleep.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketAcceptBlocking
{
    static int Main()
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        if (!listener.Blocking) return 1;
        var endpoint = new IPEndPoint(IPAddress.Loopback, ((IPEndPoint)listener.LocalEndPoint).Port);

        // A connection 50ms after the accept has begun ends it, and not before.
        using var first = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        var connector = new Thread(() =>
        {
            Thread.Sleep(50);
            first.Connect(endpoint);
        });
        var started = Stopwatch.GetTimestamp();
        connector.Start();
        using var firstAccepted = listener.Accept();
        if (Stopwatch.GetElapsedTime(started).TotalMilliseconds < 50) return 2;
        connector.Join();
        if (!first.Connected) return 3;
        if (((IPEndPoint)firstAccepted.RemoteEndPoint).Port != ((IPEndPoint)first.LocalEndPoint).Port) return 4;
        if (((IPEndPoint)firstAccepted.LocalEndPoint).Port != endpoint.Port) return 5;
        if (!firstAccepted.Blocking) return 6;

        // The listener sleeps again for a second connection: an accept that
        // has slept once is an ordinary accept the next time.
        using var second = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        connector = new Thread(() =>
        {
            Thread.Sleep(50);
            second.Connect(endpoint);
        });
        connector.Start();
        using var secondAccepted = listener.Accept();
        connector.Join();
        if (((IPEndPoint)secondAccepted.RemoteEndPoint).Port != ((IPEndPoint)second.LocalEndPoint).Port) return 7;

        return 0;
    }
}
