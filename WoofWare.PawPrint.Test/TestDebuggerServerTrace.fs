namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text.Json
open System.Threading.Tasks
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open NUnit.Framework
open WoofWare.PawPrint

/// `POST /trace` records a run as a base snapshot plus per-step deltas. The oracle throughout is
/// single-stepping a second server through `/step`, `/state` and `/thread/{id}`: a trace is correct
/// exactly when replaying it reconstructs, at every step, what those endpoints report.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDebuggerServerTrace =
    let private token = "test-debugger-trace-token"

    /// Two threads contending one lock, so the scheduler interleaves them and a thread is seen
    /// blocked on the other's lock; plus a write, so a step carries output.
    let private contendedLockSource =
        """
using System;
using System.Threading;

class Program
{
    static readonly object Gate = new object();
    static int counter;

    static int Twice(int n)
    {
        return n + n;
    }

    static void Work()
    {
        for (int i = 0; i < 2; i++)
        {
            lock (Gate)
            {
                counter += Twice(1);
            }
        }
    }

    static int Main(string[] args)
    {
        var t = new Thread(Work);
        t.Start();
        Work();
        t.Join();
        Console.Out.Write("done");
        return counter;
    }
}
"""

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

    /// Runs a few ordinary steps, then calls a P/Invoke no one could implement, so a trace page
    /// fails partway through.
    let private unimplementedNativeSource =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("PawPrintNonexistentNativeLibrary")]
    private static extern int PawPrintNoSuchNativeFunction();

    static int Main(string[] args)
    {
        int before = 41;
        before += 1;
        return before + PawPrintNoSuchNativeFunction();
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

    let private startServerWith (compile : string list -> byte[]) (source : string) : RunningServer =
        let suffix = Guid.NewGuid().ToString "N"

        let tempDir =
            Path.Combine (Path.GetTempPath (), $"pawprint-debugger-trace-test-%s{suffix}")

        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>
        let dllPath = Path.Combine (tempDir, "DebuggerTraceTest.dll")
        File.WriteAllBytes (dllPath, compile [ source ])

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let app, stopCts =
            DebuggerServer.createApp
                loggerFactory
                dllPath
                dotnetRuntimes
                KernelConfig.Default
                None
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

    let private client (server : RunningServer) : HttpClient =
        let client = new HttpClient ()
        client.BaseAddress <- Uri server.BaseUrl
        client.Timeout <- TimeSpan.FromMinutes 10.0
        client.DefaultRequestHeaders.Authorization <- AuthenticationHeaderValue ("Bearer", token)
        client

    let private emptyContent () : HttpContent = new StringContent ("") :> HttpContent

    let private jsonDocument (response : HttpResponseMessage) : Task<JsonDocument> =
        task {
            let! body = response.Content.ReadAsStringAsync ()
            return JsonDocument.Parse body
        }

    let private tryProperty (name : string) (element : JsonElement) : JsonElement option =
        match element.TryGetProperty name with
        | true, value -> Some value
        | false, _ -> None

    let private optionalInt (element : JsonElement) : int option =
        if element.ValueKind = JsonValueKind.Null then
            None
        else
            Some (element.GetInt32 ())

    /// JSON text with every object's keys sorted and all insignificant whitespace removed, so two
    /// renderings of one value compare equal whatever their key order or indentation.
    let rec private canonical (element : JsonElement) : string =
        match element.ValueKind with
        | JsonValueKind.Object ->
            element.EnumerateObject ()
            |> Seq.sortWith (fun a b -> String.CompareOrdinal (a.Name, b.Name))
            |> Seq.map (fun p -> JsonSerializer.Serialize p.Name + ":" + canonical p.Value)
            |> String.concat ","
            |> sprintf "{%s}"
        | JsonValueKind.Array ->
            element.EnumerateArray ()
            |> Seq.map canonical
            |> String.concat ","
            |> sprintf "[%s]"
        | _ -> element.GetRawText ()

    let private canonicalWithout (drop : string list) (element : JsonElement) : string =
        element.EnumerateObject ()
        |> Seq.filter (fun p -> not (List.contains p.Name drop))
        |> Seq.sortWith (fun a b -> String.CompareOrdinal (a.Name, b.Name))
        |> Seq.map (fun p -> JsonSerializer.Serialize p.Name + ":" + canonical p.Value)
        |> String.concat ","
        |> sprintf "{%s}"

    type private Frame =
        {
            Id : int
            Method : string
            IlOffset : int
            /// Canonical JSON, `null` included.
            SourceLocation : string
        }

    type private FrameValues =
        {
            EvalStack : string list
            Arguments : string list
            Locals : string list
        }

    type private Thread =
        {
            Status : string
            ActiveAssembly : string option
            ActiveFrame : int option
            Frames : Frame list
            /// The active frame's values; `None` when they were not asked for or there is no
            /// active frame.
            Values : FrameValues option
        }

    type private Snapshot =
        {
            Threads : Map<int, Thread>
            Heap : int list
        }

    type private Step =
        {
            /// Canonical JSON of the step's event, without its step number and output.
            Event : string
            Output : string option
            After : Snapshot
        }

    type private Page =
        {
            MaxSteps : int
            FirstStep : int64
            StepsRun : int
            StoppedBecause : string
            Base : Snapshot
            Steps : Step list
            Session : string
        }

    let private withoutValues (snapshot : Snapshot) : Snapshot =
        { snapshot with
            Threads =
                snapshot.Threads
                |> Map.map (fun _ thread ->
                    { thread with
                        Values = None
                    }
                )
        }

    /// Replays one page: its base is a delta from nothing, and each step a delta from the one
    /// before. Every structural rule a client relies on is asserted rather than assumed.
    let private decodePage (root : JsonElement) : Page =
        let strings =
            root.GetProperty("strings").EnumerateArray ()
            |> Seq.map (fun e -> e.GetString ())
            |> Seq.toArray

        let table (name : string) : string array =
            root.GetProperty(name).EnumerateArray () |> Seq.map canonical |> Seq.toArray

        let statuses = table "statuses"
        let locations = table "locations"
        let events = table "events"
        let values = table "frameValues"

        let valuesRecorded =
            match root.GetProperty("values").GetString () with
            | "none" -> false
            | "active" -> true
            | other -> failwith $"unknown values mode %s{other}"

        let location (element : JsonElement) : string =
            match optionalInt element with
            | None -> "null"
            | Some i -> locations.[i]

        let valueList (element : JsonElement) : string list =
            element.EnumerateArray ()
            |> Seq.map (fun i -> values.[i.GetInt32 ()])
            |> Seq.toList

        let applyThread (previous : Thread option) (delta : JsonElement) : Thread =
            let start =
                match previous with
                | Some thread -> thread
                | None ->
                    for key in [ "s" ; "a" ; "f" ] do
                        if (tryProperty key delta).IsNone then
                            failwith $"a thread's first delta must carry %s{key}: %s{delta.GetRawText ()}"

                    {
                        Status = ""
                        ActiveAssembly = None
                        ActiveFrame = None
                        Frames = []
                        Values = None
                    }

            let status =
                match tryProperty "s" delta with
                | Some s -> statuses.[s.GetInt32 ()]
                | None -> start.Status

            let assembly =
                match tryProperty "a" delta with
                | Some a -> optionalInt a |> Option.map (fun i -> strings.[i])
                | None -> start.ActiveAssembly

            let activeFrameChanged, activeFrame =
                match tryProperty "f" delta with
                | Some f -> true, optionalInt f
                | None -> false, start.ActiveFrame

            let popped =
                match tryProperty "pop" delta with
                | Some p ->
                    let p = p.GetInt32 ()

                    if p <= 0 || p > start.Frames.Length then
                        failwith $"pop %d{p} of %d{start.Frames.Length} frames"

                    List.take (start.Frames.Length - p) start.Frames
                | None -> start.Frames

            let set =
                match tryProperty "set" delta with
                | Some s ->
                    s.EnumerateArray ()
                    |> Seq.map (fun e -> e.[0].GetInt32 (), (e.[1].GetInt32 (), location e.[2]))
                    |> Map.ofSeq
                | None -> Map.empty

            for KeyValue (id, _) in set do
                if not (popped |> List.exists (fun f -> f.Id = id)) then
                    failwith $"set names frame %d{id}, which is not retained"

            let updated =
                popped
                |> List.map (fun frame ->
                    match Map.tryFind frame.Id set with
                    | Some (ilOffset, sourceLocation) ->
                        { frame with
                            IlOffset = ilOffset
                            SourceLocation = sourceLocation
                        }
                    | None -> frame
                )

            let pushed =
                match tryProperty "push" delta with
                | Some p ->
                    p.EnumerateArray ()
                    |> Seq.map (fun e ->
                        {
                            Id = e.[0].GetInt32 ()
                            Method = strings.[e.[1].GetInt32 ()]
                            IlOffset = e.[2].GetInt32 ()
                            SourceLocation = location e.[3]
                        }
                    )
                    |> Seq.toList
                | None -> []

            let frameValues =
                match activeFrame, tryProperty "v" delta with
                | None, None -> None
                | None, Some v -> failwith $"values for a thread with no active frame: %s{v.GetRawText ()}"
                | Some _, _ when not valuesRecorded ->
                    if (tryProperty "v" delta).IsSome then
                        failwith "values in a page that did not ask for them"

                    None
                | Some _, None ->
                    if activeFrameChanged || start.Values.IsNone then
                        failwith "the active frame changed but its values were not stated"

                    start.Values
                | Some _, Some v ->
                    let prior = if activeFrameChanged then None else start.Values

                    let part (name : string) (select : FrameValues -> string list) : string list =
                        match tryProperty name v, prior with
                        | Some c, _ -> valueList c
                        | None, Some prior -> select prior
                        | None, None -> failwith $"values for a new active frame must state %s{name}"

                    Some
                        {
                            EvalStack = part "e" (fun p -> p.EvalStack)
                            Arguments = part "a" (fun p -> p.Arguments)
                            Locals = part "l" (fun p -> p.Locals)
                        }

            {
                Status = status
                ActiveAssembly = assembly
                ActiveFrame = activeFrame
                Frames = updated @ pushed
                Values = frameValues
            }

        let applySnapshot (previous : Snapshot) (record : JsonElement) : Snapshot =
            let threads =
                match tryProperty "th" record with
                | Some deltas ->
                    deltas.EnumerateArray ()
                    |> Seq.fold
                        (fun threads delta ->
                            let id = delta.GetProperty("id").GetInt32 ()
                            Map.add id (applyThread (Map.tryFind id threads) delta) threads
                        )
                        previous.Threads
                | None -> previous.Threads

            let threads =
                match tryProperty "gone" record with
                | Some gone ->
                    gone.EnumerateArray ()
                    |> Seq.fold (fun m id -> Map.remove (id.GetInt32 ()) m) threads
                | None -> threads

            let heap =
                match tryProperty "hp" record with
                | Some hp -> hp.EnumerateArray () |> Seq.map (fun e -> e.GetInt32 ()) |> Seq.toList
                | None -> previous.Heap

            {
                Threads = threads
                Heap = heap
            }

        let baseRecord = root.GetProperty "base"

        (tryProperty "hp" baseRecord).IsSome |> shouldEqual true

        let baseSnapshot =
            applySnapshot
                {
                    Threads = Map.empty
                    Heap = []
                }
                baseRecord

        let steps =
            root.GetProperty("steps").EnumerateArray ()
            |> Seq.scan
                (fun (previous : Step) record ->
                    {
                        Event = events.[record.GetProperty("e").GetInt32 ()]
                        Output = tryProperty "o" record |> Option.map canonical
                        After = applySnapshot previous.After record
                    }
                )
                {
                    Event = ""
                    Output = None
                    After = baseSnapshot
                }
            |> Seq.skip 1
            |> Seq.toList

        {
            MaxSteps = root.GetProperty("maxSteps").GetInt32 ()
            FirstStep = root.GetProperty("firstStep").GetInt64 ()
            StepsRun = root.GetProperty("stepsRun").GetInt32 ()
            StoppedBecause = root.GetProperty("stoppedBecause").GetString ()
            Base = baseSnapshot
            Steps = steps
            Session = canonical (root.GetProperty "session")
        }

    let private tracePage (client : HttpClient) (query : string) : Task<Page> =
        task {
            let! response = client.PostAsync ($"trace?%s{query}", emptyContent ())
            let! body = response.Content.ReadAsStringAsync ()

            if response.StatusCode <> HttpStatusCode.OK then
                failwith $"trace?%s{query} returned %O{response.StatusCode}: %s{body}"

            use json = JsonDocument.Parse body
            return decodePage json.RootElement
        }

    /// A whole run's trace, from the first page's base to the end, with the page boundaries
    /// checked and then erased: each page must resume exactly where the last left off.
    type private Recording =
        {
            Initial : Snapshot
            Steps : Step list
            Session : string
        }

    let private joinPages (pages : Page list) : Recording =
        let rec check (pages : Page list) : unit =
            match pages with
            | previous :: (next :: _ as rest) ->
                next.FirstStep |> shouldEqual (previous.FirstStep + int64 previous.StepsRun)

                let previousEnd =
                    match List.tryLast previous.Steps with
                    | Some step -> step.After
                    | None -> previous.Base

                next.Base |> shouldEqual previousEnd
                check rest
            | _ -> ()

        for page in pages do
            page.Steps.Length |> shouldEqual page.StepsRun

        check pages

        {
            Initial = (List.head pages).Base
            Steps = pages |> List.collect (fun page -> page.Steps)
            Session = (List.last pages).Session
        }

    /// Pages through the whole run, taking each page's query from `nextQuery` in turn.
    let private traceWholeRun (client : HttpClient) (nextQuery : int -> string) : Task<Recording> =
        task {
            let pages = ResizeArray<Page> ()
            let mutable finished = false

            while not finished do
                if pages.Count > 100000 then
                    failwith "the trace did not reach the end of the run within 100000 pages"

                let! page = tracePage client (nextQuery pages.Count)
                pages.Add page
                finished <- page.StoppedBecause = "sessionEnded"

            return joinPages (List.ofSeq pages)
        }

    /// What the existing endpoints report about every thread, given the `session` object of a
    /// `/state` or `/step` response.
    let private observe (client : HttpClient) (session : JsonElement) : Task<Snapshot> =
        task {
            let mutable threads = Map.empty

            for summary in session.GetProperty("threads").EnumerateArray () do
                let id = summary.GetProperty("id").GetInt32 ()
                let! response = client.GetAsync $"thread/%d{id}"
                response.StatusCode |> shouldEqual HttpStatusCode.OK
                use! json = jsonDocument response
                let thread = json.RootElement
                let frames = thread.GetProperty("frames").EnumerateArray () |> Seq.toList

                let active =
                    frames |> List.tryFind (fun frame -> frame.GetProperty("active").GetBoolean ())

                let valueStrings (frame : JsonElement) (name : string) : string list =
                    frame.GetProperty(name).EnumerateArray () |> Seq.map canonical |> Seq.toList

                let observed =
                    {
                        Status = canonical (thread.GetProperty "status")
                        ActiveAssembly =
                            match thread.GetProperty "activeAssembly" with
                            | a when a.ValueKind = JsonValueKind.Null -> None
                            | a -> Some (a.GetString ())
                        ActiveFrame = optionalInt (thread.GetProperty "activeFrame")
                        Frames =
                            frames
                            |> List.map (fun frame ->
                                {
                                    Id = frame.GetProperty("id").GetInt32 ()
                                    Method = frame.GetProperty("method").GetString ()
                                    IlOffset = frame.GetProperty("ilOffset").GetInt32 ()
                                    SourceLocation = canonical (frame.GetProperty "sourceLocation")
                                }
                            )
                        Values =
                            active
                            |> Option.map (fun frame ->
                                {
                                    EvalStack = valueStrings frame "evalStack"
                                    Arguments = valueStrings frame "arguments"
                                    Locals = valueStrings frame "locals"
                                }
                            )
                    }

                threads <- Map.add id observed threads

            let heap = session.GetProperty "heap"

            return
                {
                    Threads = threads
                    Heap =
                        [
                            heap.GetProperty("nonArrayObjects").GetInt32 ()
                            heap.GetProperty("arrays").GetInt32 ()
                            heap.GetProperty("stringContents").GetInt32 ()
                        ]
                }
        }

    /// The oracle: the run as observed by single-stepping and asking the other endpoints after
    /// each step.
    let private singleStepWholeRun (client : HttpClient) : Task<Recording> =
        task {
            let! state = client.GetAsync "state"
            state.StatusCode |> shouldEqual HttpStatusCode.OK
            use! stateJson = jsonDocument state
            let initialSession = stateJson.RootElement.GetProperty "session"
            let! initial = observe client initialSession

            let steps = ResizeArray<Step> ()
            let mutable session = canonical initialSession
            let mutable running = initialSession.GetProperty("status").GetString () = "running"

            while running do
                if steps.Count > 200000 then
                    failwith "the guest did not finish within 200000 single steps"

                let! response = client.PostAsync ("step?count=1", emptyContent ())
                response.StatusCode |> shouldEqual HttpStatusCode.OK
                use! json = jsonDocument response

                let event =
                    json.RootElement.GetProperty("events").EnumerateArray () |> Seq.exactlyOne

                let stepSession = json.RootElement.GetProperty "session"
                session <- canonical stepSession
                running <- stepSession.GetProperty("status").GetString () = "running"

                // Detecting a deadlock executes nothing, so it is not a step.
                if event.GetProperty("kind").GetString () <> "deadlocked" then
                    let! after = observe client stepSession

                    let output =
                        match event.GetProperty "output" with
                        | o when o.ValueKind = JsonValueKind.Null -> None
                        | o -> Some (canonical o)

                    steps.Add
                        {
                            Event = canonicalWithout [ "step" ; "output" ] event
                            Output = output
                            After = after
                        }

            return
                {
                    Initial = initial
                    Steps = List.ofSeq steps
                    Session = session
                }
        }

    /// Fails at the first step on which the two disagree, rather than dumping two whole runs.
    let private assertSameRun (expected : Recording) (actual : Recording) : unit =
        actual.Initial |> shouldEqual expected.Initial

        for i, (e, a) in Seq.zip expected.Steps actual.Steps |> Seq.indexed do
            if e <> a then
                failwith $"step %d{i + 1} differs.\nexpected: %A{e}\nactual: %A{a}"

        actual.Steps.Length |> shouldEqual expected.Steps.Length
        actual.Session |> shouldEqual expected.Session

    let private stripValues (recording : Recording) : Recording =
        {
            Initial = withoutValues recording.Initial
            Steps =
                recording.Steps
                |> List.map (fun step ->
                    { step with
                        After = withoutValues step.After
                    }
                )
            Session = recording.Session
        }

    let private isLockWait (status : string) : bool =
        status.Contains ("blockedOnSyncBlockAcquire", StringComparison.Ordinal)
        || status.Contains ("blockedOnMonitorAcquire", StringComparison.Ordinal)

    [<Test>]
    let ``replaying a trace reconstructs what single-stepping observes at every step`` () : Task =
        task {
            use tracedServer = startServerWith Roslyn.compileWithSymbols contendedLockSource
            use tracedClient = client tracedServer
            use steppedServer = startServerWith Roslyn.compileWithSymbols contendedLockSource
            use steppedClient = client steppedServer

            // An odd page size, so page boundaries fall at arbitrary points in the run.
            let! traced = traceWholeRun tracedClient (fun _ -> "maxSteps=97&values=active")
            let! stepped = singleStepWholeRun steppedClient

            assertSameRun stepped traced

            // The comparison is only as strong as what the run exercised.
            let eventThreads =
                traced.Steps
                |> List.map (fun step ->
                    use doc = JsonDocument.Parse step.Event
                    doc.RootElement.GetProperty("thread").GetRawText ()
                )
                |> List.distinct

            eventThreads.Length >= 2 |> shouldEqual true

            traced.Steps
            |> List.exists (fun step -> step.After.Threads |> Map.exists (fun _ t -> isLockWait t.Status))
            |> shouldEqual true

            traced.Steps
            |> List.exists (fun step ->
                step.After.Threads
                |> Map.exists (fun _ t -> t.Frames |> List.exists (fun f -> f.SourceLocation <> "null"))
            )
            |> shouldEqual true

            traced.Steps |> List.exists (fun step -> step.Output.IsSome) |> shouldEqual true

            traced.Steps
            |> List.exists (fun step ->
                step.After.Threads
                |> Map.exists (fun _ t ->
                    match t.Values with
                    | Some v -> not v.EvalStack.IsEmpty && not v.Locals.IsEmpty
                    | None -> false
                )
            )
            |> shouldEqual true
        }

    [<Test>]
    let ``a trace without values is the trace with values, less the values`` () : Task =
        task {
            use server = startServerWith Roslyn.compile contendedLockSource
            use client = client server

            let! withValues = traceWholeRun client (fun _ -> "maxSteps=100000&values=active")

            let! reset = client.PostAsync ("reset", emptyContent ())
            reset.StatusCode |> shouldEqual HttpStatusCode.OK

            let! withoutValues = traceWholeRun client (fun _ -> "maxSteps=100000")

            withoutValues.Steps
            |> List.forall (fun step -> step.After.Threads |> Map.forall (fun _ t -> t.Values.IsNone))
            |> shouldEqual true

            assertSameRun (stripValues withValues) withoutValues
        }

    /// How a client asks for one page: by steps, and optionally by a byte budget too.
    type private PageRequest =
        | Steps of int
        | StepsAndBytes of int * int

    let private queryOf (request : PageRequest) : string =
        match request with
        | PageRequest.Steps steps -> $"maxSteps=%d{steps}&values=active"
        | PageRequest.StepsAndBytes (steps, bytes) -> $"maxSteps=%d{steps}&maxBytes=%d{bytes}&values=active"

    [<Test>]
    let ``pages concatenate to the same run whatever their sizes`` () : unit =
        use server = startServerWith Roslyn.compile contendedLockSource
        use client = client server

        let reference =
            (traceWholeRun client (fun _ -> "maxSteps=100000&values=active")).GetAwaiter().GetResult ()

        let requestGen : Gen<PageRequest> =
            Gen.oneof
                [
                    Gen.choose (1, 1500) |> Gen.map PageRequest.Steps
                    Gen.zip (Gen.choose (1, 1500)) (Gen.choose (1, 20000))
                    |> Gen.map PageRequest.StepsAndBytes
                ]

        let requestsGen = Gen.nonEmptyListOf requestGen

        let property (requests : PageRequest list) : bool =
            let reset = client.PostAsync("reset", emptyContent ()).GetAwaiter().GetResult ()
            reset.StatusCode |> shouldEqual HttpStatusCode.OK

            let requests = Array.ofList requests

            let paged =
                (traceWholeRun client (fun i -> queryOf requests.[i % requests.Length])).GetAwaiter().GetResult ()

            assertSameRun reference paged
            true

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 8, Prop.forAll (Arb.fromGen requestsGen) property)

    [<Test>]
    let ``a trace page reports its bounds and rejects malformed parameters`` () : Task =
        task {
            use server = startServerWith Roslyn.compile simpleSource
            use client = client server

            for bad in
                [
                    "maxSteps=abc"
                    "maxSteps=0"
                    "maxSteps=-3"
                    "maxBytes=0"
                    "maxBytes=lots"
                    "values=everything"
                ] do
                let! response = client.PostAsync ($"trace?%s{bad}", emptyContent ())
                response.StatusCode |> shouldEqual HttpStatusCode.BadRequest

            let! run = client.PostAsync ("run?maxSteps=1000000", emptyContent ())
            run.StatusCode |> shouldEqual HttpStatusCode.OK

            // The session is over, so the page is empty; the step cap is still echoed as applied.
            let! page = tracePage client "maxSteps=2000000000"
            page.MaxSteps |> shouldEqual 100000
            page.StepsRun |> shouldEqual 0
            page.Steps |> shouldEqual []
            page.StoppedBecause |> shouldEqual "sessionEnded"

            let! help = client.GetStringAsync "help"
            help.Contains ("POST /trace", StringComparison.Ordinal) |> shouldEqual true
        }

    [<Test>]
    let ``a trace page stops at its byte budget`` () : Task =
        task {
            use server = startServerWith Roslyn.compile infiniteSource
            use client = client server

            let! page = tracePage client "maxSteps=100000&maxBytes=4096"
            page.StoppedBecause |> shouldEqual "byteBudget"
            page.StepsRun > 0 |> shouldEqual true
            page.StepsRun < 4096 |> shouldEqual true
        }

    [<Test>]
    let ``stop cancels an active trace and it reports the steps it took`` () : Task =
        task {
            use server = startServerWith Roslyn.compile infiniteSource
            use client = client server

            let trace = client.PostAsync ("trace?maxSteps=100000", emptyContent ())
            do! Task.Delay 200

            let! stop = client.PostAsync ("stop", emptyContent ())
            stop.StatusCode |> shouldEqual HttpStatusCode.OK

            let! completed = Task.WhenAny (trace, Task.Delay (TimeSpan.FromSeconds 10.0))
            Object.ReferenceEquals (completed, trace) |> shouldEqual true

            let! response = trace
            response.StatusCode |> shouldEqual HttpStatusCode.OK
            use! json = jsonDocument response
            let page = decodePage json.RootElement

            page.StoppedBecause |> shouldEqual "cancelled"
            page.StepsRun < 100000 |> shouldEqual true
            page.Steps.Length |> shouldEqual page.StepsRun
        }

    [<Test>]
    let ``a host primitive failure keeps the page recorded before it`` () : Task =
        task {
            use server = startServerWith Roslyn.compile unimplementedNativeSource
            use client = client server

            let! response = client.PostAsync ("trace?maxSteps=100000", emptyContent ())
            response.StatusCode |> shouldEqual HttpStatusCode.InternalServerError
            use! json = jsonDocument response
            let root = json.RootElement

            // The same fields every other failed request reports...
            root.GetProperty("operation").GetString () |> shouldEqual "trace"

            root.GetProperty("error").GetString().Contains ("PawPrintNoSuchNativeFunction", StringComparison.Ordinal)
            |> shouldEqual true

            root.GetProperty("exceptionType").GetString().EndsWith ("Exception", StringComparison.Ordinal)
            |> shouldEqual true

            // ...and the page up to the failure, which is a complete page in its own right.
            let page = decodePage root
            page.StoppedBecause |> shouldEqual "hostFailure"
            page.StepsRun > 0 |> shouldEqual true
            page.Steps.Length |> shouldEqual page.StepsRun

            let stepsExecuted =
                root.GetProperty("session").GetProperty("stepsExecuted").GetInt64 ()

            stepsExecuted |> shouldEqual (page.FirstStep + int64 page.StepsRun)

            let! state = client.GetAsync "state"
            use! stateJson = jsonDocument state

            stateJson.RootElement.GetProperty("session").GetProperty("stepsExecuted").GetInt64 ()
            |> shouldEqual stepsExecuted
        }
