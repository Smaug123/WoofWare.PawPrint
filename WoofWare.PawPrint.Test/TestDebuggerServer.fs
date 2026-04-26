namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text.Json
open System.Threading.Tasks
open FsUnitTyped
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

[<TestFixture>]
module TestDebuggerServer =
    let private token = "test-debugger-token"

    let private simpleSource =
        """
class Program
{
    static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private infiniteSource =
        """
class Program
{
    static int Main(string[] args)
    {
        int i = 0;

        while (true)
        {
            i++;

            if (i == int.MaxValue)
            {
                i = 0;
            }
        }
    }
}
"""

    let private objectOnStackSource =
        """
class Program
{
    static int Main(string[] args)
    {
        object value = new object();
        return value == null ? 1 : 0;
    }
}
"""

    type private RunningServer =
        {
            App : WebApplication
            StopCts : System.Threading.CancellationTokenSource
            LoggerFactory : IDisposable
            BaseUrl : string
            TempDir : string
        }

        interface IDisposable with
            member this.Dispose () =
                try
                    this.App.StopAsync().GetAwaiter().GetResult ()
                with _ ->
                    ()

                this.App.DisposeAsync().AsTask().GetAwaiter().GetResult ()
                this.StopCts.Dispose ()
                this.LoggerFactory.Dispose ()

                try
                    Directory.Delete (this.TempDir, true)
                with _ ->
                    ()

    let private compileToTempDll (source : string) : string * string =
        let suffix = Guid.NewGuid().ToString "N"

        let tempDir =
            Path.Combine (Path.GetTempPath (), $"pawprint-debugger-test-%s{suffix}")

        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>
        let dllPath = Path.Combine (tempDir, "DebuggerHttpTest.dll")
        File.WriteAllBytes (dllPath, Roslyn.compile [ source ])
        tempDir, dllPath

    let private startServer (source : string) : RunningServer =
        let tempDir, dllPath = compileToTempDll source

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll typeof<RunResult>.Assembly.Location
            |> ImmutableArray.CreateRange

        let impls = NativeImpls.PassThru ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let app, stopCts =
            DebuggerServer.createApp
                loggerFactory
                dllPath
                dotnetRuntimes
                impls
                []
                token
                DebuggerServer.configureLoopbackEphemeralPort

        app.Start ()

        {
            App = app
            StopCts = stopCts
            LoggerFactory = loggerFactory
            BaseUrl = DebuggerServer.baseUrl app
            TempDir = tempDir
        }

    let private client (server : RunningServer) (token : string option) : HttpClient =
        let client = new HttpClient ()
        client.BaseAddress <- Uri server.BaseUrl

        match token with
        | Some token -> client.DefaultRequestHeaders.Authorization <- AuthenticationHeaderValue ("Bearer", token)
        | None -> ()

        client

    let private emptyContent () : HttpContent = new StringContent ("") :> HttpContent

    let private jsonDocument (response : HttpResponseMessage) : Task<JsonDocument> =
        task {
            let! body = response.Content.ReadAsStringAsync ()
            return JsonDocument.Parse body
        }

    let private activeFrame (thread : JsonElement) : JsonElement =
        thread.GetProperty("frames").EnumerateArray ()
        |> Seq.find (fun frame -> frame.GetProperty("active").GetBoolean ())

    let private tryObjectAddress (value : JsonElement) : int option =
        let mutable address = Unchecked.defaultof<JsonElement>

        if
            value.TryGetProperty ("objectAddress", &address)
            && address.ValueKind = JsonValueKind.Number
        then
            Some (address.GetInt32 ())
        else
            None

    [<Test>]
    let ``Debugger HTTP requires the bearer token before serving state`` () : Task =
        task {
            use server = startServer simpleSource
            use missingTokenClient = client server None
            let! missingToken = missingTokenClient.GetAsync "state"
            missingToken.StatusCode |> shouldEqual HttpStatusCode.Unauthorized
            missingToken.Headers.WwwAuthenticate.ToString () |> shouldEqual "Bearer"

            use badTokenClient = client server (Some "wrong-token")
            let! badToken = badTokenClient.GetAsync "state"
            badToken.StatusCode |> shouldEqual HttpStatusCode.Unauthorized

            use goodTokenClient = client server (Some token)
            let! ok = goodTokenClient.GetAsync "state"
            ok.StatusCode |> shouldEqual HttpStatusCode.OK

            use! json = jsonDocument ok

            json.RootElement.GetProperty("session").GetProperty("status").GetString ()
            |> shouldEqual "running"
        }

    [<Test>]
    let ``Debugger HTTP exposes basic route contracts`` () : Task =
        task {
            use server = startServer simpleSource
            use client = client server (Some token)

            let! badThread = client.GetAsync "thread/not-an-int"
            badThread.StatusCode |> shouldEqual HttpStatusCode.BadRequest

            let! missingRoute = client.GetAsync "not-a-route"
            missingRoute.StatusCode |> shouldEqual HttpStatusCode.NotFound

            let! step = client.PostAsync ("step?count=1", emptyContent ())
            step.StatusCode |> shouldEqual HttpStatusCode.OK

            use! stepJson = jsonDocument step
            stepJson.RootElement.GetProperty("requestedSteps").GetInt32 () |> shouldEqual 1
            stepJson.RootElement.GetProperty("cancelled").GetBoolean () |> shouldEqual false
            stepJson.RootElement.GetProperty("events").GetArrayLength () |> shouldEqual 1

            let! reset = client.PostAsync ("reset", emptyContent ())
            reset.StatusCode |> shouldEqual HttpStatusCode.OK

            use! resetJson = jsonDocument reset
            resetJson.RootElement.GetProperty("status").GetString () |> shouldEqual "reset"

            resetJson.RootElement.GetProperty("session").GetProperty("stepsExecuted").GetInt64 ()
            |> shouldEqual 0L
        }

    [<Test>]
    let ``Debugger HTTP exposes structured heap addresses from frame values`` () : Task =
        task {
            use server = startServer objectOnStackSource
            use client = client server (Some token)

            let mutable objectAddress = None

            for _ = 1 to 20 do
                if objectAddress.IsNone then
                    let! step = client.PostAsync ("step?count=1", emptyContent ())
                    step.StatusCode |> shouldEqual HttpStatusCode.OK

                    let! thread = client.GetAsync "thread/0"
                    thread.StatusCode |> shouldEqual HttpStatusCode.OK

                    use! threadJson = jsonDocument thread
                    let frame = activeFrame threadJson.RootElement

                    objectAddress <- frame.GetProperty("evalStack").EnumerateArray () |> Seq.tryPick tryObjectAddress

            let objectAddress =
                match objectAddress with
                | Some objectAddress -> objectAddress
                | None -> failwith "Did not observe an object reference on the eval stack within 20 steps"

            let! heap = client.GetAsync $"heap/%d{objectAddress}"
            heap.StatusCode |> shouldEqual HttpStatusCode.OK

            use! heapJson = jsonDocument heap

            heapJson.RootElement.GetProperty("address").GetInt32 ()
            |> shouldEqual objectAddress

            heapJson.RootElement.GetProperty("kind").GetString () |> shouldEqual "object"
        }

    [<Test>]
    let ``Debugger stop cancels an active run and lets it report cancellation`` () : Task =
        task {
            use server = startServer infiniteSource
            use client = client server (Some token)

            do! Task.Delay 50

            let run = client.PostAsync ("run?maxSteps=1000000", emptyContent ())
            do! Task.Delay 100

            let! stop = client.PostAsync ("stop", emptyContent ())
            stop.StatusCode |> shouldEqual HttpStatusCode.OK

            let! completed = Task.WhenAny (run, Task.Delay (TimeSpan.FromSeconds 10.0))
            Object.ReferenceEquals (completed, run) |> shouldEqual true

            let! runResponse = run
            runResponse.StatusCode |> shouldEqual HttpStatusCode.OK

            use! runJson = jsonDocument runResponse
            runJson.RootElement.GetProperty("maxSteps").GetInt32 () |> shouldEqual 1000000

            runJson.RootElement.GetProperty("stepsRun").GetInt32 () < 1000000
            |> shouldEqual true

            runJson.RootElement.GetProperty("cancelled").GetBoolean () |> shouldEqual true

            runJson.RootElement.GetProperty("session").GetProperty("status").GetString ()
            |> shouldEqual "running"
        }
