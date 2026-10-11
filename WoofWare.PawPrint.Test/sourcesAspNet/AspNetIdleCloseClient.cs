using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Net.Sockets;
using System.Threading;
using System.Threading.Tasks;

public record Connections (int Accepted, int Ended);

// The client of TestAspNetTwoProcess's idle-close case: an HttpClient whose pool would keep an
// idle connection for ten minutes, against a server that closes one after a second. Its connect
// callback is the default one's (a dual-mode socket with Nagle off, connected to the request's
// DnsEndPoint), except that it keeps each socket it makes.
//
// It waits for the server's address in the file named by its one argument, and asks the server
// how many connections it has accepted. With that connection idle in the pool, it waits until its
// socket is readable, which on an idle HTTP/1.1 connection is the server's FIN, and peeks to
// check it is at end of file rather than reset. Then it asks again, expecting the pool to notice
// the end of file and connect afresh, and finally asks the server to stop. Exits 42 only if each
// answer was the expected one.
public static class Program
{
    public static async Task<int> Main (string[] args)
    {
        if (args.Length != 1) return Fail (2, $"expected one argument, the handoff file's name; got {args.Length}");
        var handoff = args[0];

        using var timeout = new CancellationTokenSource (TimeSpan.FromSeconds (60));

        // The server renames a complete file into place once it is listening, so once the name
        // exists it holds the whole address. The wait between looks doubles up to a second, so a
        // server that never writes the file costs few looks before the timeout.
        var wait = TimeSpan.FromMilliseconds (10);
        while (!File.Exists (handoff))
        {
            await Task.Delay (wait, timeout.Token);
            wait = TimeSpan.FromTicks (Math.Min (2 * wait.Ticks, TimeSpan.TicksPerSecond));
        }
        var address = File.ReadAllText (handoff);
        Console.WriteLine ("client: found the server");

        var sockets = new List<Socket> ();
        var handler = new SocketsHttpHandler
        {
            PooledConnectionIdleTimeout = TimeSpan.FromMinutes (10),
            ConnectCallback = async (context, cancellationToken) =>
            {
                var socket = new Socket (SocketType.Stream, ProtocolType.Tcp) { NoDelay = true };
                try
                {
                    await socket.ConnectAsync (context.DnsEndPoint, cancellationToken);
                }
                catch
                {
                    socket.Dispose ();
                    throw;
                }
                lock (sockets) sockets.Add (socket);
                return new NetworkStream (socket, ownsSocket: true);
            },
        };
        using var client = new HttpClient (handler) { BaseAddress = new Uri (address) };

        var first = await client.GetFromJsonAsync<Connections> ("/connections", timeout.Token);
        if (first != new Connections (1, 0)) return Fail (3, $"first count answered {first}");
        Console.WriteLine ("client: the server has accepted one connection");

        Socket pooled;
        lock (sockets)
        {
            if (sockets.Count != 1) return Fail (4, $"made {sockets.Count} sockets for one request");
            pooled = sockets[0];
        }

        // Nothing else uses the connection while it is idle in the pool: the pool's own check on
        // an idle connection runs every PooledConnectionIdleTimeout / 4, two and a half minutes
        // here, long after the server's keep-alive timeout of a second. So the socket becomes
        // readable only by the server's FIN (or a reset).
        if (!pooled.Poll (TimeSpan.FromSeconds (30), SelectMode.SelectRead)) return Fail (5, "the server never closed the idle connection");
        // A peek leaves the end of file for the pool to read, and tells a FIN, which it answers
        // with no bytes, from a reset, which it throws. Asynchronous, as the pool's own reads
        // are, so that on a TCP socket it is a recv: a synchronous receive is a recvmsg, which
        // PawPrint does not implement.
        var peeked = await pooled.ReceiveAsync (new byte[1], SocketFlags.Peek, timeout.Token);
        if (peeked != 0) return Fail (10, $"the idle connection became readable with {peeked} bytes, not at end of file");
        Console.WriteLine ("client: the server closed the idle connection");

        var second = await client.GetFromJsonAsync<Connections> ("/connections", timeout.Token);
        if (second != new Connections (2, 1)) return Fail (6, $"second count answered {second}");
        lock (sockets)
        {
            if (sockets.Count != 2) return Fail (7, $"made {sockets.Count} sockets for two requests across a closed connection");
        }
        Console.WriteLine ("client: the second request went over a new connection");

        using var stop = await client.PostAsync ("/shutdown", null, timeout.Token);
        if (stop.StatusCode != HttpStatusCode.OK) return Fail (8, $"shutdown status {stop.StatusCode}");
        lock (sockets)
        {
            if (sockets.Count != 2) return Fail (9, $"made {sockets.Count} sockets for three requests, the last two over one live connection");
        }
        Console.WriteLine ("client: asked the server to stop");

        return 42;
    }

    private static int Fail (int code, string message)
    {
        Console.Error.WriteLine ("client: " + message);
        return code;
    }
}
