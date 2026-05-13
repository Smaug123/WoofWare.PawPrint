namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.Json
open System.Threading.Tasks
open FsCheck
open FsCheck.FSharp
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

    let private recursiveSource =
        """
class Program
{
    static int Recurse(int remaining)
    {
        if (remaining == 0)
        {
            while (true)
            {
            }
        }

        return Recurse(remaining - 1);
    }

    static int Main(string[] args)
    {
        return Recurse(4);
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

    let private jsonContent (json : string) : HttpContent =
        new StringContent (json, Encoding.UTF8, "application/json") :> HttpContent

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

    let private instructionTexts (body : JsonElement) : string list =
        body.GetProperty("instructions").EnumerateArray ()
        |> Seq.map (fun instruction -> instruction.GetProperty("text").GetString ())
        |> Seq.choose Option.ofObj
        |> Seq.toList

    type private GeneratedPredicateTree =
        | Leaf of bool
        | And of GeneratedPredicateTree * GeneratedPredicateTree
        | Or of GeneratedPredicateTree * GeneratedPredicateTree
        | Not of GeneratedPredicateTree

    let private runUntilPropertyConfig : Config =
        Config.QuickThrowOnFailure.WithMaxTest 100

    let private genPredicateTree : Gen<GeneratedPredicateTree> =
        let rec go (depth : int) : Gen<GeneratedPredicateTree> =
            if depth = 0 then
                Gen.elements [ GeneratedPredicateTree.Leaf true ; GeneratedPredicateTree.Leaf false ]
            else
                gen {
                    let! choice = Gen.choose (0, 4)

                    match choice with
                    | 0 -> return GeneratedPredicateTree.Leaf true
                    | 1 -> return GeneratedPredicateTree.Leaf false
                    | 2 ->
                        let! left = go (depth - 1)
                        let! right = go (depth - 1)
                        return GeneratedPredicateTree.And (left, right)
                    | 3 ->
                        let! left = go (depth - 1)
                        let! right = go (depth - 1)
                        return GeneratedPredicateTree.Or (left, right)
                    | _ ->
                        let! child = go (depth - 1)
                        return GeneratedPredicateTree.Not child
                }

        Gen.sized (fun size -> go (min 4 size))

    let rec private referencePredicateValue (predicate : GeneratedPredicateTree) : bool =
        match predicate with
        | GeneratedPredicateTree.Leaf value -> value
        | GeneratedPredicateTree.And (left, right) -> referencePredicateValue left && referencePredicateValue right
        | GeneratedPredicateTree.Or (left, right) -> referencePredicateValue left || referencePredicateValue right
        | GeneratedPredicateTree.Not child -> not (referencePredicateValue child)

    let rec private toRunUntilPredicate (predicate : GeneratedPredicateTree) : DebuggerRunUntil.RunUntilPredicate =
        match predicate with
        | GeneratedPredicateTree.Leaf true -> DebuggerRunUntil.RunUntilPredicate.SessionStatusChanged
        | GeneratedPredicateTree.Leaf false ->
            DebuggerRunUntil.RunUntilPredicate.Not DebuggerRunUntil.RunUntilPredicate.SessionStatusChanged
        | GeneratedPredicateTree.And (left, right) ->
            DebuggerRunUntil.RunUntilPredicate.And [ toRunUntilPredicate left ; toRunUntilPredicate right ]
        | GeneratedPredicateTree.Or (left, right) ->
            DebuggerRunUntil.RunUntilPredicate.Or [ toRunUntilPredicate left ; toRunUntilPredicate right ]
        | GeneratedPredicateTree.Not child -> DebuggerRunUntil.RunUntilPredicate.Not (toRunUntilPredicate child)

    let private runUntilSnapshot (status : DebuggerRunUntil.DebuggerSessionStatus) : DebuggerRunUntil.SessionSnapshot =
        {
            Status = status
            Threads = Map.empty
        }

    [<Test>]
    let ``Run-until evaluator boolean trees agree with reference`` () : unit =
        let mutable positiveCount = 0
        let mutable negativeCount = 0

        let property (predicate : GeneratedPredicateTree) : unit =
            let expected = referencePredicateValue predicate

            if expected then
                positiveCount <- positiveCount + 1
            else
                negativeCount <- negativeCount + 1

            let state =
                runUntilSnapshot DebuggerRunUntil.DebuggerSessionStatus.Running
                |> DebuggerRunUntil.initialEvaluationState

            let actual =
                DebuggerRunUntil.evaluate
                    state
                    (runUntilSnapshot DebuggerRunUntil.DebuggerSessionStatus.Finished)
                    (toRunUntilPredicate predicate)

            (actual.Matches |> List.isEmpty |> not) |> shouldEqual expected

        Check.One (runUntilPropertyConfig, Prop.forAll (Arb.fromGen genPredicateTree) property)

        positiveCount > 0 |> shouldEqual true
        negativeCount > 0 |> shouldEqual true

    [<Test>]
    let ``Run-until parser accepts scoped condition trees`` () : unit =
        let request =
            """
{
  "maxSteps": 10,
  "recordLimit": 3,
  "until": {
    "kind": "anyThread",
    "condition": {
      "kind": "or",
      "conditions": [
        { "kind": "frameDepthAtLeast", "depth": 7 },
        {
          "kind": "activeMethodMatches",
          "match": { "kind": "contains", "value": "AdvSimd.get_IsSupported" }
        }
      ]
    }
  }
}
"""

        match DebuggerRunUntil.parseRequestJson request with
        | Error errors -> failwith $"Expected valid run-until request, got %A{errors}"
        | Ok request ->
            request.MaxSteps |> shouldEqual 10
            request.RecordLimit |> shouldEqual 3

            match request.Predicate with
            | DebuggerRunUntil.RunUntilPredicate.AnyThread (DebuggerRunUntil.RunUntilPredicate.Or conditions) ->
                conditions.Length |> shouldEqual 2
            | other -> failwith $"Unexpected parsed predicate: %A{other}"

    [<Test>]
    let ``Run-until parser accepts not condition trees`` () : unit =
        let request =
            """
{
  "until": {
    "kind": "not",
    "condition": { "kind": "sessionStatusChanged" }
  }
}
"""

        match DebuggerRunUntil.parseRequestJson request with
        | Error errors -> failwith $"Expected valid run-until request, got %A{errors}"
        | Ok request ->
            match request.Predicate with
            | DebuggerRunUntil.RunUntilPredicate.Not DebuggerRunUntil.RunUntilPredicate.SessionStatusChanged -> ()
            | other -> failwith $"Unexpected parsed predicate: %A{other}"

    [<Test>]
    let ``Run-until parser rejects thread predicates outside a thread scope`` () : unit =
        let request =
            """
{
  "until": {
    "kind": "activeMethodMatches",
    "match": { "kind": "contains", "value": "Main" }
  }
}
"""

        match DebuggerRunUntil.parseRequestJson request with
        | Ok request -> failwith $"Expected invalid run-until request, got %A{request}"
        | Error errors ->
            errors
            |> List.exists (fun error -> error.Message.Contains ("thread or anyThread", StringComparison.Ordinal))
            |> shouldEqual true

    [<Test>]
    let ``Run-until parser accepts explicit thread zero`` () : unit =
        let request =
            """
{
  "until": {
    "kind": "thread",
    "thread": 0,
    "condition": { "kind": "frameDepthAtLeast", "depth": 2 }
  }
}
"""

        match DebuggerRunUntil.parseRequestJson request with
        | Error errors -> failwith $"Expected valid run-until request, got %A{errors}"
        | Ok request ->
            match request.Predicate with
            | DebuggerRunUntil.RunUntilPredicate.Thread (ThreadId.ThreadId 0, _) -> ()
            | other -> failwith $"Unexpected parsed predicate: %A{other}"

    [<Test>]
    let ``Run-until parser rejects removed repeated active location predicate`` () : unit =
        let request =
            """
{
  "until": {
    "kind": "anyThread",
    "condition": { "kind": "repeatedActiveLocation", "repeatCount": 2 }
  }
}
"""

        match DebuggerRunUntil.parseRequestJson request with
        | Ok request -> failwith $"Expected invalid run-until request, got %A{request}"
        | Error errors ->
            errors
            |> List.exists (fun error ->
                error.Path = "$.until.condition.kind"
                && error.Message.Contains ("unknown predicate kind 'repeatedActiveLocation'", StringComparison.Ordinal)
            )
            |> shouldEqual true

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
    let ``Debugger HTTP exposes compact stack summary`` () : Task =
        task {
            use server = startServer recursiveSource
            use client = client server (Some token)

            let! run = client.PostAsync ("run?maxSteps=80", emptyContent ())
            run.StatusCode |> shouldEqual HttpStatusCode.OK

            let! summary = client.GetAsync "thread/0/stack-summary?edgeFrames=2&topMethods=1"
            summary.StatusCode |> shouldEqual HttpStatusCode.OK

            use! summaryJson = jsonDocument summary
            let root = summaryJson.RootElement

            root.GetProperty("id").GetInt32 () |> shouldEqual 0

            let frameCount = root.GetProperty("frameCount").GetInt32 ()
            frameCount > 2 |> shouldEqual true

            root.GetProperty("firstFrames").GetArrayLength () |> shouldEqual 2
            root.GetProperty("lastFrames").GetArrayLength () |> shouldEqual 2

            let activeFrame = root.GetProperty("activeFrame").GetInt32 ()

            root.GetProperty("activeFrameSummary").GetProperty("id").GetInt32 ()
            |> shouldEqual activeFrame

            let topMethods = root.GetProperty ("topMethods")
            topMethods.GetArrayLength () |> shouldEqual 1

            let topMethod = topMethods.EnumerateArray () |> Seq.exactlyOne

            topMethod.GetProperty("method").GetString ()
            |> shouldEqual "PawPrintTestAssembly.Program.Recurse"

            topMethod.GetProperty("count").GetInt32 () > 1 |> shouldEqual true
        }

    [<Test>]
    let ``Debugger HTTP stack summary validates thread id`` () : Task =
        task {
            use server = startServer simpleSource
            use client = client server (Some token)

            let! badThread = client.GetAsync "thread/not-an-int/stack-summary"
            badThread.StatusCode |> shouldEqual HttpStatusCode.BadRequest

            let! missingThread = client.GetAsync "thread/999/stack-summary"
            missingThread.StatusCode |> shouldEqual HttpStatusCode.NotFound
        }

    [<Test>]
    let ``Debugger HTTP run-until matches the initial state before stepping`` () : Task =
        task {
            use server = startServer simpleSource
            use client = client server (Some token)

            let request =
                """
{
  "maxSteps": 20,
  "recordLimit": 4,
  "until": {
    "kind": "anyThread",
    "condition": {
      "kind": "activeMethodMatches",
      "match": { "kind": "contains", "value": "Program.Main" }
    }
  }
}
"""

            let! response = client.PostAsync ("run-until", jsonContent request)
            response.StatusCode |> shouldEqual HttpStatusCode.OK

            use! json = jsonDocument response
            let root = json.RootElement

            root.GetProperty("maxSteps").GetInt32 () |> shouldEqual 20
            root.GetProperty("recordLimit").GetInt32 () |> shouldEqual 4
            root.GetProperty("stepsRun").GetInt32 () |> shouldEqual 0
            root.GetProperty("recentEvents").GetArrayLength () |> shouldEqual 0

            let stopReason = root.GetProperty "stopReason"
            stopReason.GetProperty("kind").GetString () |> shouldEqual "predicateMatched"

            let matchResult =
                stopReason.GetProperty("matches").EnumerateArray () |> Seq.exactlyOne

            matchResult.GetProperty("kind").GetString ()
            |> shouldEqual "activeMethodMatches"

            matchResult.GetProperty("thread").GetInt32 () |> shouldEqual 0

            matchResult.GetProperty("method").GetString ()
            |> shouldEqual "PawPrintTestAssembly.Program.Main"
        }

    [<Test>]
    let ``Debugger HTTP run-until reports max steps and validation errors`` () : Task =
        task {
            use server = startServer infiniteSource
            use client = client server (Some token)

            let noMatch =
                """
{
  "maxSteps": 3,
  "until": {
    "kind": "anyThread",
    "condition": {
      "kind": "activeMethodMatches",
      "match": { "kind": "exact", "value": "Not.A.Real.Method" }
    }
  }
}
"""

            let! maxSteps = client.PostAsync ("run-until", jsonContent noMatch)
            maxSteps.StatusCode |> shouldEqual HttpStatusCode.OK

            use! maxStepsJson = jsonDocument maxSteps
            maxStepsJson.RootElement.GetProperty("stepsRun").GetInt32 () |> shouldEqual 3

            maxStepsJson.RootElement.GetProperty("stopReason").GetProperty("kind").GetString ()
            |> shouldEqual "maxStepsReached"

            let invalid =
                """
{
  "until": { "kind": "frameDepthAtLeast", "depth": 2 }
}
"""

            let! bad = client.PostAsync ("run-until", jsonContent invalid)
            bad.StatusCode |> shouldEqual HttpStatusCode.BadRequest

            use! badJson = jsonDocument bad

            badJson.RootElement.GetProperty("error").GetString ()
            |> shouldEqual "invalid run-until request"

            badJson.RootElement.GetProperty("errors").GetArrayLength () > 0
            |> shouldEqual true
        }

    [<Test>]
    let ``Debugger HTTP exposes active method IL`` () : Task =
        task {
            use server = startServer simpleSource
            use client = client server (Some token)

            let! il = client.GetAsync "thread/0/active-method/il"
            il.StatusCode |> shouldEqual HttpStatusCode.OK

            use! ilJson = jsonDocument il
            let root = ilJson.RootElement

            root.GetProperty("thread").GetInt32 () |> shouldEqual 0

            root.GetProperty("method").GetString ()
            |> shouldEqual "PawPrintTestAssembly.Program.Main"

            root.GetProperty("activeIlOffset").GetInt32 () |> shouldEqual 0
            root.GetProperty("hasBody").GetBoolean () |> shouldEqual true
            root.GetProperty("truncatedBefore").GetBoolean () |> shouldEqual false
            root.GetProperty("truncatedAfter").GetBoolean () |> shouldEqual false

            let activeInstructions =
                root.GetProperty("instructions").EnumerateArray ()
                |> Seq.filter (fun instruction -> instruction.GetProperty("active").GetBoolean ())
                |> Seq.toList

            activeInstructions.Length |> shouldEqual 1

            let activeInstruction = activeInstructions |> List.exactlyOne

            activeInstruction.GetProperty("offset").GetInt32 () |> shouldEqual 0

            activeInstruction.GetProperty("text").GetString().StartsWith ("IL_0000", StringComparison.Ordinal)
            |> shouldEqual true
        }

    [<Test>]
    let ``Debugger HTTP active method IL supports context windows and resolved tokens`` () : Task =
        task {
            use server = startServer objectOnStackSource
            use client = client server (Some token)

            let! context = client.GetAsync "thread/0/active-method/il?context=1"
            context.StatusCode |> shouldEqual HttpStatusCode.OK

            use! contextJson = jsonDocument context
            let contextRoot = contextJson.RootElement

            contextRoot.GetProperty("instructions").GetArrayLength () |> shouldEqual 2
            contextRoot.GetProperty("truncatedBefore").GetBoolean () |> shouldEqual false
            contextRoot.GetProperty("truncatedAfter").GetBoolean () |> shouldEqual true

            let! full = client.GetAsync "thread/0/active-method/il"
            full.StatusCode |> shouldEqual HttpStatusCode.OK

            use! fullJson = jsonDocument full

            fullJson.RootElement
            |> instructionTexts
            |> List.exists (fun text -> text.Contains ("System.Object::.ctor", StringComparison.Ordinal))
            |> shouldEqual true
        }

    [<Test>]
    let ``Debugger HTTP active method IL validates thread id`` () : Task =
        task {
            use server = startServer simpleSource
            use client = client server (Some token)

            let! badThread = client.GetAsync "thread/not-an-int/active-method/il"
            badThread.StatusCode |> shouldEqual HttpStatusCode.BadRequest

            let! missingThread = client.GetAsync "thread/999/active-method/il"
            missingThread.StatusCode |> shouldEqual HttpStatusCode.NotFound
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
