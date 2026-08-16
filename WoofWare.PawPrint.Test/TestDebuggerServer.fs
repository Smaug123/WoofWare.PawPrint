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

    /// Guest that executes a handful of ordinary IL steps and then calls a
    /// P/Invoke PawPrint cannot possibly implement, so the interpreter throws
    /// from `NativeDispatch.failUnimplemented` partway through the run. Used by
    /// the tests below that assert the debugger server preserves the steps it
    /// already completed when a host primitive fails.
    ///
    /// The import deliberately names a library that does not exist rather than
    /// a real-but-not-yet-implemented BCL primitive: this test is about the
    /// server's failure reporting, and pinning it to a genuine gap would make
    /// it start passing vacuously (or fail outright) the day that gap is
    /// closed.
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

    /// Substring the interpreter's unimplemented-native failure is expected to
    /// carry. Matched as a substring rather than compared whole because
    /// `NativeCall.failUnimplemented` formats the full signature into its
    /// message, and that formatting is not this test's contract.
    let private unimplementedNativeMarker = "PawPrintNoSuchNativeFunction"

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

    /// As `recursiveSource`, but with the lines the source-location assertions care about
    /// marked, so those tests derive the expected line from the text rather than counting.
    ///
    /// Compiled with symbols. Every other guest in this file is deliberately PDB-less — which is
    /// itself the subject of a test below, since a null `sourceLocation` is the ordinary case for
    /// anything the compiler did not emit debug information for.
    let private recursiveSourceWithSymbols =
        """
class Program
{
    static int Recurse(int remaining)
    {
        if (remaining == 0)
        {
            while (true) { } // SPIN
        }

        return Recurse(remaining - 1); // RECURSE
    }

    static int Main(string[] args)
    {
        return Recurse(4); // ENTRY
    }
}
"""

    /// The line of the single source line containing `marker`, 1-based as a PDB counts lines.
    let private lineContaining (marker : string) (source : string) : int =
        let lines = source.Replace("\r\n", "\n").Split '\n'

        match
            lines
            |> Array.mapi (fun i l -> i, l)
            |> Array.filter (fun (_, l) -> l.Contains marker)
        with
        | [| (i, _) |] -> i + 1
        | [||] -> failwith $"guest source contains no line matching %s{marker}; the test's oracle is broken"
        | many ->
            failwith $"guest source has %d{many.Length} lines matching %s{marker}, so the expected line is ambiguous"

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

    let private compileToTempDllWith (compile : string list -> byte[]) (source : string) : string * string =
        let suffix = Guid.NewGuid().ToString "N"

        let tempDir =
            Path.Combine (Path.GetTempPath (), $"pawprint-debugger-test-%s{suffix}")

        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>
        let dllPath = Path.Combine (tempDir, "DebuggerHttpTest.dll")
        File.WriteAllBytes (dllPath, compile [ source ])
        tempDir, dllPath

    let private startServerWith (compile : string list -> byte[]) (source : string) : RunningServer =
        let tempDir, dllPath = compileToTempDllWith compile source

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll typeof<RunResult>.Assembly.Location
            |> ImmutableArray.CreateRange

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

    let private startServer (source : string) : RunningServer = startServerWith Roslyn.compile source

    /// As `startServer`, but the guest image carries an embedded portable PDB, so its frames
    /// resolve to source. Opt-in per case: see `Roslyn.DebugSymbols` for why symbols are not the
    /// default.
    let private startServerWithSymbols (source : string) : RunningServer =
        startServerWith Roslyn.compileWithSymbols source

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

    let private instructionTexts (body : JsonElement) : string list =
        body.GetProperty("instructions").EnumerateArray ()
        |> Seq.map (fun instruction -> instruction.GetProperty("text").GetString ())
        |> Seq.choose Option.ofObj
        |> Seq.toList

    let private assertDebuggerFailure (operation : string) (response : HttpResponseMessage) : Task<int64> =
        task {
            response.StatusCode |> shouldEqual HttpStatusCode.InternalServerError

            response.Content.Headers.ContentType.MediaType |> shouldEqual "application/json"

            use! failureJson = jsonDocument response
            let failureRoot = failureJson.RootElement

            failureRoot.GetProperty("operation").GetString () |> shouldEqual operation

            failureRoot.GetProperty("error").GetString().Contains (unimplementedNativeMarker, StringComparison.Ordinal)
            |> shouldEqual true

            let exceptionType = failureRoot.GetProperty("exceptionType").GetString ()

            exceptionType.EndsWith ("Exception", StringComparison.Ordinal)
            |> shouldEqual true

            return failureRoot.GetProperty("session").GetProperty("stepsExecuted").GetInt64 ()
        }

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
    let ``Debugger HTTP run keeps completed steps when a host primitive fails`` () : Task =
        task {
            use server = startServer unimplementedNativeSource

            use client = client server (Some token)

            let! failedRun = client.PostAsync ("run?maxSteps=1000", emptyContent ())
            let! failureSteps = assertDebuggerFailure "run" failedRun

            failureSteps > 0L |> shouldEqual true

            let! state = client.GetAsync "state"
            state.StatusCode |> shouldEqual HttpStatusCode.OK

            use! stateJson = jsonDocument state

            stateJson.RootElement.GetProperty("session").GetProperty("stepsExecuted").GetInt64 ()
            |> shouldEqual failureSteps
        }

    [<Test>]
    let ``Debugger HTTP step keeps completed steps when a host primitive fails`` () : Task =
        task {
            use server = startServer unimplementedNativeSource

            use client = client server (Some token)

            let mutable failureSteps = None
            let mutable remaining = 100

            while failureSteps.IsNone && remaining > 0 do
                remaining <- remaining - 1
                let! response = client.PostAsync ("step?count=1", emptyContent ())

                if response.StatusCode = HttpStatusCode.InternalServerError then
                    let! steps = assertDebuggerFailure "step" response
                    failureSteps <- Some steps
                else
                    response.StatusCode |> shouldEqual HttpStatusCode.OK

            let failureSteps =
                match failureSteps with
                | Some failureSteps -> failureSteps
                | None -> failwith "The unimplemented-native guest did not fail within 100 debugger steps"

            failureSteps > 0L |> shouldEqual true

            let! state = client.GetAsync "state"
            state.StatusCode |> shouldEqual HttpStatusCode.OK

            use! stateJson = jsonDocument state

            stateJson.RootElement.GetProperty("session").GetProperty("stepsExecuted").GetInt64 ()
            |> shouldEqual failureSteps
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

    /// The frame a thread is actually executing must name the line it is on. Without this the
    /// debugger reports `Assembly.Type.Method` plus an IL offset, which does not tell a reader
    /// which of a method's statements they are looking at.
    [<Test>]
    let ``a frame carries the source line it is executing`` () : Task =
        task {
            use server = startServerWithSymbols recursiveSourceWithSymbols
            use client = client server (Some token)

            let! run = client.PostAsync ("run?maxSteps=200", emptyContent ())
            run.StatusCode |> shouldEqual HttpStatusCode.OK

            let! thread = client.GetAsync "thread/0"
            thread.StatusCode |> shouldEqual HttpStatusCode.OK

            use! threadJson = jsonDocument thread
            let active = activeFrame threadJson.RootElement
            let location = active.GetProperty "sourceLocation"

            location.ValueKind |> shouldNotEqual JsonValueKind.Null

            location.GetProperty("documentPath").GetString () |> shouldEqual "File0.cs"

            location.GetProperty("startLine").GetInt32 ()
            |> shouldEqual (lineContaining "// SPIN" recursiveSourceWithSymbols)
        }

    /// The decision this field's shape exists to make unambiguous. A caller's `ilOffset` is its
    /// *resume* point — the instruction after the call — so attributing it there reports every
    /// frame but the innermost one statement late. `sourceLocation` is instead resolved at the
    /// call site, and carries the offset it used so the two can never be confused.
    [<Test>]
    let ``a caller frame is attributed to its call site, not its resume point`` () : Task =
        task {
            use server = startServerWithSymbols recursiveSourceWithSymbols
            use client = client server (Some token)

            let! run = client.PostAsync ("run?maxSteps=200", emptyContent ())
            run.StatusCode |> shouldEqual HttpStatusCode.OK

            let! thread = client.GetAsync "thread/0"
            thread.StatusCode |> shouldEqual HttpStatusCode.OK

            use! threadJson = jsonDocument thread

            let frames =
                threadJson.RootElement.GetProperty("frames").EnumerateArray () |> Seq.toList

            let callers =
                frames
                |> List.filter (fun frame -> not (frame.GetProperty("active").GetBoolean ()))

            // `Recurse(4)` plus `Main`, so there is something to be a caller.
            callers |> List.isEmpty |> shouldEqual false

            let expectedFor (method : string) : int =
                if method.EndsWith "Main" then
                    lineContaining "// ENTRY" recursiveSourceWithSymbols
                else
                    lineContaining "// RECURSE" recursiveSourceWithSymbols

            for caller in callers do
                let location = caller.GetProperty "sourceLocation"
                location.ValueKind |> shouldNotEqual JsonValueKind.Null

                location.GetProperty("startLine").GetInt32 ()
                |> shouldEqual (expectedFor (caller.GetProperty("method").GetString ()))

                // The discriminator, and the reason the offset is emitted: in a debug build the
                // call site and the resume point often share a line — the compiler puts a `nop`
                // after the call carrying the call's own sequence point — so equal lines would
                // not prove the attribution happened. Differing offsets do.
                location.GetProperty("ilOffset").GetInt32 ()
                |> shouldNotEqual (caller.GetProperty("ilOffset").GetInt32 ())
        }

    /// A null location is the ordinary case, not a defect: the entire shared framework ships
    /// without PDBs. The key must still be present, so a consumer can tell "not known here" from
    /// "this server does not report locations".
    [<Test>]
    let ``a frame from an assembly without symbols reports a null source location`` () : Task =
        task {
            use server = startServer recursiveSource
            use client = client server (Some token)

            let! run = client.PostAsync ("run?maxSteps=200", emptyContent ())
            run.StatusCode |> shouldEqual HttpStatusCode.OK

            let! thread = client.GetAsync "thread/0"
            thread.StatusCode |> shouldEqual HttpStatusCode.OK

            use! threadJson = jsonDocument thread
            let active = activeFrame threadJson.RootElement

            active.GetProperty("sourceLocation").ValueKind |> shouldEqual JsonValueKind.Null
        }

    /// The field must reach every response that renders a frame, not only `GET /thread/{id}`.
    /// They go through one writer precisely so this cannot drift apart.
    [<Test>]
    let ``the stack summary and state responses carry source locations too`` () : Task =
        task {
            use server = startServerWithSymbols recursiveSourceWithSymbols
            use client = client server (Some token)

            let! run = client.PostAsync ("run?maxSteps=200", emptyContent ())
            run.StatusCode |> shouldEqual HttpStatusCode.OK

            let spin = lineContaining "// SPIN" recursiveSourceWithSymbols

            let! summary = client.GetAsync "thread/0/stack-summary?edgeFrames=2&topMethods=1"
            summary.StatusCode |> shouldEqual HttpStatusCode.OK
            use! summaryJson = jsonDocument summary

            summaryJson.RootElement
                .GetProperty("activeFrameSummary")
                .GetProperty("sourceLocation")
                .GetProperty("startLine")
                .GetInt32 ()
            |> shouldEqual spin

            let! state = client.GetAsync "state"
            state.StatusCode |> shouldEqual HttpStatusCode.OK
            use! stateJson = jsonDocument state

            // `heap` and `threads` sit inside the `session` object, not at the root.
            let thread =
                stateJson.RootElement.GetProperty("session").GetProperty("threads").EnumerateArray ()
                |> Seq.head

            thread.GetProperty("activeFrameSummary").GetProperty("sourceLocation").GetProperty("startLine").GetInt32 ()
            |> shouldEqual spin
        }

    /// Before `Main` is installed the entry thread carries a placeholder frame: shaped like the
    /// entry point so that everything reading a frame sees something sensible, but with a bare
    /// `ret` body, because `Main` has not run.
    ///
    /// It must therefore report no source location. Resolving its offsets against the real
    /// `Main`'s debug information would name a line of code that has not executed — and this is
    /// reachable, not theoretical: a static initialiser that throws leaves the guest here, and
    /// `/state` is the first call the debugging workflow tells you to make.
    [<Test>]
    let ``the pre-Main placeholder frame reports no source location`` () : Task =
        task {
            let source =
                """
class Program
{
    static readonly int Boom = int.Parse("not a number");

    static int Main(string[] args)
    {
        return Boom;
    }
}
"""

            use server = startServerWithSymbols source
            use client = client server (Some token)

            let! state = client.GetAsync "state"
            state.StatusCode |> shouldEqual HttpStatusCode.OK
            use! stateJson = jsonDocument state

            let thread =
                stateJson.RootElement.GetProperty("session").GetProperty("threads").EnumerateArray ()
                |> Seq.head

            let frame = thread.GetProperty "activeFrameSummary"

            // Asserted so the test cannot pass by the frame having become unrecognisable: it is
            // still the entry-point-shaped placeholder, sitting at offset 0 of its `ret` body.
            frame.GetProperty("method").GetString ()
            |> shouldEqual "PawPrintTestAssembly.Program.Main"

            frame.GetProperty("ilOffset").GetInt32 () |> shouldEqual 0

            frame.GetProperty("sourceLocation").ValueKind |> shouldEqual JsonValueKind.Null
        }
