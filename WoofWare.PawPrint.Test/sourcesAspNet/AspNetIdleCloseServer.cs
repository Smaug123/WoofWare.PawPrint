using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Connections;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Hosting.Server;
using Microsoft.AspNetCore.Hosting.Server.Features;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

public record Connections (int Accepted, int Ended);

// The server of TestAspNetTwoProcess's idle-close case: a minimal-API server on Kestrel's socket
// transport whose keep-alive timeout is one second, so that it closes a connection the client
// leaves idle. Connection middleware counts the connections Kestrel accepts and the ones whose
// processing has ended; GET /connections answers both counts. It hands its address to the client
// in a file named by its one argument, serves until the client asks it to stop, and then checks
// that it accepted exactly two connections, and that the first had ended before the second was
// accepted. Exits 42 only if all of that held.
public static class Program
{
    public static async Task<int> Main (string[] args)
    {
        if (args.Length != 1) return Fail (2, $"expected one argument, the handoff file's name; got {args.Length}");
        var handoff = args[0];

        int accepted = 0;
        int ended = 0;
        // How many connections had ended as each one was accepted, in the order they were accepted.
        var endedAtAccept = new System.Collections.Concurrent.ConcurrentQueue<int> ();

        var builder = WebApplication.CreateSlimBuilder ();
        builder.Logging.ClearProviders ();
        builder.WebHost.ConfigureKestrel (o =>
        {
            o.Limits.KeepAliveTimeout = TimeSpan.FromSeconds (1);
            o.Listen (IPAddress.Loopback, 0, listen => listen.Use (next => async connection =>
            {
                endedAtAccept.Enqueue (Volatile.Read (ref ended));
                Interlocked.Increment (ref accepted);
                try
                {
                    await next (connection);
                }
                finally
                {
                    // Kestrel shuts the socket down and closes it only once this returns, so a
                    // client that has seen the connection end sees this count already raised.
                    Interlocked.Increment (ref ended);
                }
            }));
        });

        var app = builder.Build ();
        app.MapGet ("/connections", () => Results.Ok (new Connections (Volatile.Read (ref accepted), Volatile.Read (ref ended))));
        app.MapPost ("/shutdown", (IHostApplicationLifetime lifetime) =>
        {
            lifetime.StopApplication ();
            return Results.Ok ();
        });

        var stopRequested = new TaskCompletionSource (TaskCreationOptions.RunContinuationsAsynchronously);
        app.Lifetime.ApplicationStopping.Register (() => stopRequested.TrySetResult ());

        using var timeout = new CancellationTokenSource (TimeSpan.FromSeconds (60));
        await app.StartAsync (timeout.Token);
        Console.WriteLine ("server: listening");

        var address = app.Services.GetRequiredService<IServer> ().Features.Get<IServerAddressesFeature> ().Addresses.Single ();
        // Written under another name and then renamed into place, so that the client, which waits
        // for the name to exist, never reads a partly written address.
        File.WriteAllText (handoff + ".tmp", address);
        File.Move (handoff + ".tmp", handoff);

        var stopped = await Task.WhenAny (stopRequested.Task, Task.Delay (Timeout.Infinite, timeout.Token));
        if (stopped != stopRequested.Task) return Fail (3, "the client never asked the server to stop");

        await app.StopAsync (timeout.Token);

        var counts = string.Join (",", endedAtAccept);
        if (counts != "0,1") return Fail (4, $"as each connection was accepted, this many had ended: [{counts}]; expected [0,1]");

        Console.WriteLine ("server: stopped after accepting a second connection once the first had ended");
        return 42;
    }

    private static int Fail (int code, string message)
    {
        Console.Error.WriteLine ("server: " + message);
        return code;
    }
}
