using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Threading;
using System.Threading.Tasks;

public record Item (int Id, string Q);

// The client of TestAspNetTwoProcess: an idiomatic HttpClient. It waits for the server's address
// to appear in the file named by its one argument, then makes a GET with a query, a POST with a
// JSON body and a request that must 404, and finally asks the server to stop. Exits 42 only if
// every answer was the expected one.
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

        using var client = new HttpClient { BaseAddress = new Uri (address) };

        var got = await client.GetFromJsonAsync<Item> ("/items/5?q=hi", timeout.Token);
        if (got == null || got.Id != 5 || got.Q != "hi") return Fail (3, $"GET answered {got}");
        Console.WriteLine ("client: GET answered");

        using var posted = await client.PostAsJsonAsync ("/items", new Item (7, "posted"), timeout.Token);
        if (posted.StatusCode != HttpStatusCode.OK) return Fail (4, $"POST status {posted.StatusCode}");
        var echoed = await posted.Content.ReadFromJsonAsync<Item> (timeout.Token);
        if (echoed == null || echoed.Id != 14 || echoed.Q != "POSTED") return Fail (5, $"POST answered {echoed}");
        Console.WriteLine ("client: POST answered");

        using var missing = await client.GetAsync ("/items/five", timeout.Token);
        if (missing.StatusCode != HttpStatusCode.NotFound) return Fail (6, $"missing item status {missing.StatusCode}");
        Console.WriteLine ("client: missing item answered 404");

        using var stop = await client.PostAsync ("/shutdown", null, timeout.Token);
        if (stop.StatusCode != HttpStatusCode.OK) return Fail (7, $"shutdown status {stop.StatusCode}");
        Console.WriteLine ("client: asked the server to stop");

        return 42;
    }

    private static int Fail (int code, string message)
    {
        Console.Error.WriteLine ("client: " + message);
        return code;
    }
}
