using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Hosting.Server;
using Microsoft.AspNetCore.Hosting.Server.Features;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

public record Item (int Id, string Q);

// The server of TestAspNetTwoProcess: an idiomatic minimal-API server on Kestrel's socket
// transport, listening on a port the kernel chooses. It hands its address to the client in a
// file named by its one argument, serves until the client asks it to stop, and then checks that
// it served both item requests. Exits 42 only if all of that held.
public static class Program
{
    public static async Task<int> Main (string[] args)
    {
        if (args.Length != 1) return Fail (2, $"expected one argument, the handoff file's name; got {args.Length}");
        var handoff = args[0];

        var builder = WebApplication.CreateSlimBuilder ();
        builder.Logging.ClearProviders ();
        builder.WebHost.ConfigureKestrel (o => o.Listen (IPAddress.Loopback, 0));

        var app = builder.Build ();
        int served = 0;
        app.MapGet ("/items/{id:int}", (int id, string q) =>
        {
            Interlocked.Increment (ref served);
            return Results.Ok (new Item (id, q));
        });
        app.MapPost ("/items", (Item item) =>
        {
            Interlocked.Increment (ref served);
            return Results.Ok (new Item (item.Id * 2, item.Q.ToUpperInvariant ()));
        });
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
        if (served != 2) return Fail (4, $"served {served} item requests, expected 2");

        Console.WriteLine ("server: stopped after serving both item requests");
        return 42;
    }

    private static int Fail (int code, string message)
    {
        Console.Error.WriteLine ("server: " + message);
        return code;
    }
}
