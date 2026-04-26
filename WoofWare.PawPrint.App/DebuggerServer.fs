namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Net
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Hosting.Server
open Microsoft.AspNetCore.Hosting.Server.Features
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers
open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module DebuggerServer =
    let private threadIdValue (threadId : ThreadId) : int =
        match threadId with
        | ThreadId.ThreadId i -> i

    let private frameIdValue (frameId : FrameId) : int =
        match frameId with
        | FrameId.FrameId i -> i

    let private heapAddressValue (address : ManagedHeapAddress) : int =
        match address with
        | ManagedHeapAddress.ManagedHeapAddress i -> i

    let private pathSegments (path : string) : string list =
        path.TrimEnd('/').Split ('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map WebUtility.UrlDecode
        |> Array.toList

    let private currentInstruction (frame : MethodState) : string option =
        match frame.ExecutingMethod.Instructions with
        | None -> None
        | Some instructions -> instructions.Locations |> Map.tryFind frame.IlOpIndex |> Option.map string

    let private writeOptionalString (writer : Utf8JsonWriter) (name : string) (value : string option) : unit =
        writer.WritePropertyName name

        match value with
        | Some value -> writer.WriteStringValue value
        | None -> writer.WriteNullValue ()

    let private writeOptionalInt (writer : Utf8JsonWriter) (name : string) (value : int option) : unit =
        writer.WritePropertyName name

        match value with
        | Some value -> writer.WriteNumberValue value
        | None -> writer.WriteNullValue ()

    let private writeOptionalHeapAddress
        (writer : Utf8JsonWriter)
        (name : string)
        (value : ManagedHeapAddress option)
        : unit
        =
        writer.WritePropertyName name

        match value with
        | Some value -> writer.WriteNumberValue (heapAddressValue value)
        | None -> writer.WriteNullValue ()

    let private writeThreadStatus (writer : Utf8JsonWriter) (status : ThreadStatus) : unit =
        match status with
        | ThreadStatus.Runnable -> writer.WriteStringValue "runnable"
        | ThreadStatus.BlockedOnJoin target ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnJoin")
            writer.WriteNumber ("targetThread", threadIdValue target)
            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnClassInit blocker ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnClassInit")
            writer.WriteNumber ("blockerThread", threadIdValue blocker)
            writer.WriteEndObject ()
        | ThreadStatus.Terminated -> writer.WriteStringValue "terminated"

    let private writeFrameProperties
        (writer : Utf8JsonWriter)
        (includeActive : bool)
        (activeFrame : FrameId)
        (frameId : FrameId)
        (frame : MethodState)
        : unit
        =
        writer.WriteNumber ("id", frameIdValue frameId)

        if includeActive then
            writer.WriteBoolean ("active", (frameId = activeFrame))

        writer.WriteString ("method", string frame.ExecutingMethod)
        writer.WriteNumber ("ilOffset", frame.IlOpIndex)
        writeOptionalString writer "instruction" (currentInstruction frame)
        writer.WriteNumber ("evalStackDepth", frame.EvaluationStack.Values.Length)
        writer.WriteNumber ("argumentCount", frame.Arguments.Length)
        writer.WriteNumber ("localCount", frame.LocalVariables.Length)

    let private writeFrameSummary
        (writer : Utf8JsonWriter)
        (activeFrame : FrameId)
        (frameId : FrameId)
        (frame : MethodState)
        : unit
        =
        writer.WriteStartObject ()
        writeFrameProperties writer false activeFrame frameId frame
        writer.WriteEndObject ()

    let private writeThreadSummary (writer : Utf8JsonWriter) (threadId : ThreadId) (threadState : ThreadState) : unit =
        writer.WriteStartObject ()
        writer.WriteNumber ("id", threadIdValue threadId)
        writer.WritePropertyName "status"
        writeThreadStatus writer threadState.Status
        writer.WriteString ("activeAssembly", threadState.ActiveAssembly.FullName)
        writer.WriteNumber ("activeFrame", frameIdValue threadState.ActiveMethodState)
        writer.WritePropertyName "activeFrameSummary"
        writeFrameSummary writer threadState.ActiveMethodState threadState.ActiveMethodState threadState.MethodState
        writer.WriteEndObject ()

    let private writeValueArray<'a>
        (writer : Utf8JsonWriter)
        (name : string)
        (values : seq<'a>)
        (writeOne : Utf8JsonWriter -> 'a -> unit)
        : unit
        =
        writer.WritePropertyName name
        writer.WriteStartArray ()

        for value in values do
            writeOne writer value

        writer.WriteEndArray ()

    let private writeEvalStackValue (writer : Utf8JsonWriter) (value : EvalStackValue) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("value", string value)

        match value with
        | EvalStackValue.ObjectRef address -> writer.WriteNumber ("objectAddress", heapAddressValue address)
        | EvalStackValue.NullObjectRef -> writer.WriteNull "objectAddress"
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.NativeInt _
        | EvalStackValue.Float _
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.UserDefinedValueType _ -> ()

        writer.WriteEndObject ()

    let private writeCliType (writer : Utf8JsonWriter) (value : CliType) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("value", string value)

        match value with
        | CliType.ObjectRef address -> writeOptionalHeapAddress writer "objectAddress" address
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _
        | CliType.RuntimePointer _
        | CliType.ValueType _ -> ()

        writer.WriteEndObject ()

    let private writeFrameDetails
        (writer : Utf8JsonWriter)
        (activeFrame : FrameId)
        (frameId : FrameId)
        (frame : MethodState)
        : unit
        =
        writer.WriteStartObject ()
        writeFrameProperties writer true activeFrame frameId frame
        writeValueArray writer "evalStack" frame.EvaluationStack.Values writeEvalStackValue
        writeValueArray writer "arguments" frame.Arguments writeCliType
        writeValueArray writer "locals" frame.LocalVariables writeCliType
        writer.WriteEndObject ()

    let private tryExitCode (state : IlMachineState) (thread : ThreadId) : int option =
        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
        | EvalStackValue.Int32 i :: _ -> Some i
        | _ -> None

    let private writeRunOutcome (writer : Utf8JsonWriter) (outcome : RunOutcome) : unit =
        writer.WritePropertyName "outcome"
        writer.WriteStartObject ()

        match outcome with
        | RunOutcome.NormalExit (state, thread) ->
            writer.WriteString ("kind", "normalExit")
            writer.WriteNumber ("thread", threadIdValue thread)
            writeOptionalInt writer "exitCode" (tryExitCode state thread)
        | RunOutcome.ProcessExit (state, thread) ->
            writer.WriteString ("kind", "processExit")
            writer.WriteNumber ("thread", threadIdValue thread)
            writeOptionalInt writer "exitCode" (tryExitCode state thread)
        | RunOutcome.GuestUnhandledException (_, thread, exn) ->
            writer.WriteString ("kind", "guestUnhandledException")
            writer.WriteNumber ("thread", threadIdValue thread)
            writer.WriteString ("exceptionObject", string exn.ExceptionObject)
            writer.WriteNumber ("exceptionObjectAddress", heapAddressValue exn.ExceptionObject)

        writer.WriteEndObject ()

    type private SessionState =
        | Running of Program.PreparedProgram * stepsExecuted : int64
        | Finished of RunOutcome * stepsExecuted : int64
        | Deadlocked of Program.PreparedProgram * stuckThreads : string * stepsExecuted : int64

    type private DebugEvent =
        {
            StepNumber : int64
            Kind : string
            Thread : int option
            Detail : string
            BlockedOnClassInitThread : int option
        }

    let private sessionState (session : SessionState) : IlMachineState =
        match session with
        | SessionState.Running (prepared, _) -> prepared.State
        | SessionState.Finished (RunOutcome.NormalExit (state, _), _)
        | SessionState.Finished (RunOutcome.ProcessExit (state, _), _)
        | SessionState.Finished (RunOutcome.GuestUnhandledException (state, _, _), _) -> state
        | SessionState.Deadlocked (prepared, _, _) -> prepared.State

    let private prepareSession
        (loggerFactory : ILoggerFactory)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (impls : NativeImpls)
        (argv : string list)
        : SessionState
        =
        use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

        match Program.prepare loggerFactory (Some dllPath) fileStream dotnetRuntimeDirs impls argv with
        | Program.ProgramStartResult.Ready prepared -> SessionState.Running (prepared, 0L)
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> SessionState.Finished (outcome, 0L)

    let private eventOfStepOutcome (stepNumber : int64) (outcome : Program.ProgramStepOutcome) : DebugEvent =
        match outcome with
        | Program.ProgramStepOutcome.InstructionStepped (_, thread, whatWeDid) ->
            let blockedOnClassInitThread =
                match whatWeDid with
                | WhatWeDid.BlockedOnClassInit blocker -> Some (threadIdValue blocker)
                | WhatWeDid.Executed
                | WhatWeDid.SuspendedForClassInit
                | WhatWeDid.ThrowingTypeInitializationException -> None

            {
                StepNumber = stepNumber
                Kind = "instruction"
                Thread = Some (threadIdValue thread)
                Detail = string whatWeDid
                BlockedOnClassInitThread = blockedOnClassInitThread
            }
        | Program.ProgramStepOutcome.WorkerTerminated (_, thread) ->
            {
                StepNumber = stepNumber
                Kind = "workerTerminated"
                Thread = Some (threadIdValue thread)
                Detail = "thread terminated"
                BlockedOnClassInitThread = None
            }
        | Program.ProgramStepOutcome.Completed outcome ->
            let detail =
                match outcome with
                | RunOutcome.NormalExit _ -> "normal exit"
                | RunOutcome.ProcessExit _ -> "process exit"
                | RunOutcome.GuestUnhandledException _ -> "guest unhandled exception"

            {
                StepNumber = stepNumber
                Kind = "completed"
                Thread = None
                Detail = detail
                BlockedOnClassInitThread = None
            }
        | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
            {
                StepNumber = stepNumber
                Kind = "deadlocked"
                Thread = None
                Detail = stuck
                BlockedOnClassInitThread = None
            }

    let private stepSession
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (impls : NativeImpls)
        (session : SessionState)
        : SessionState * DebugEvent * bool
        =
        match session with
        | SessionState.Running (prepared, steps) ->
            let outcome = Program.stepPrepared loggerFactory logger impls prepared

            match outcome with
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _)
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) ->
                let steps = steps + 1L
                SessionState.Running (prepared, steps), eventOfStepOutcome steps outcome, true
            | Program.ProgramStepOutcome.Completed runOutcome ->
                let steps = steps + 1L
                SessionState.Finished (runOutcome, steps), eventOfStepOutcome steps outcome, true
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) ->
                SessionState.Deadlocked (prepared, stuck, steps), eventOfStepOutcome steps outcome, false
        | SessionState.Finished (_, steps) ->
            session,
            {
                StepNumber = steps
                Kind = "alreadyFinished"
                Thread = None
                Detail = "program has already finished"
                BlockedOnClassInitThread = None
            },
            false
        | SessionState.Deadlocked (_, stuck, steps) ->
            session,
            {
                StepNumber = steps
                Kind = "alreadyDeadlocked"
                Thread = None
                Detail = stuck
                BlockedOnClassInitThread = None
            },
            false

    let private writeEvent (writer : Utf8JsonWriter) (event : DebugEvent) : unit =
        writer.WriteStartObject ()
        writer.WriteNumber ("step", event.StepNumber)
        writer.WriteString ("kind", event.Kind)
        writeOptionalInt writer "thread" event.Thread
        writer.WriteString ("detail", event.Detail)

        match event.BlockedOnClassInitThread with
        | Some blocker -> writer.WriteNumber ("blockedOnClassInitThread", blocker)
        | None -> ()

        writer.WriteEndObject ()

    let private writeSessionSummary (writer : Utf8JsonWriter) (session : SessionState) : unit =
        let state = sessionState session

        writer.WritePropertyName "session"
        writer.WriteStartObject ()

        match session with
        | SessionState.Running (prepared, steps) ->
            writer.WriteString ("status", "running")
            writer.WriteNumber ("stepsExecuted", steps)
            writer.WriteNumber ("entryThread", threadIdValue prepared.EntryThread)
            writer.WriteNumber ("lastRan", threadIdValue prepared.LastRan)
        | SessionState.Finished (outcome, steps) ->
            writer.WriteString ("status", "finished")
            writer.WriteNumber ("stepsExecuted", steps)
            writeRunOutcome writer outcome
        | SessionState.Deadlocked (prepared, stuck, steps) ->
            writer.WriteString ("status", "deadlocked")
            writer.WriteNumber ("stepsExecuted", steps)
            writer.WriteNumber ("entryThread", threadIdValue prepared.EntryThread)
            writer.WriteNumber ("lastRan", threadIdValue prepared.LastRan)
            writer.WriteString ("stuckThreads", stuck)

        writer.WritePropertyName "heap"
        writer.WriteStartObject ()
        writer.WriteNumber ("nonArrayObjects", state.ManagedHeap.NonArrayObjects.Count)
        writer.WriteNumber ("arrays", state.ManagedHeap.Arrays.Count)
        writer.WriteNumber ("stringContents", state.ManagedHeap.StringContents.Count)
        writer.WriteEndObject ()

        writeValueArray
            writer
            "loadedAssemblies"
            state._LoadedAssemblies.Keys
            (fun writer assemblyName -> writer.WriteStringValue assemblyName)

        writeValueArray
            writer
            "threads"
            (state.ThreadState |> Map.toSeq)
            (fun writer (threadId, threadState) -> writeThreadSummary writer threadId threadState)

        writer.WriteEndObject ()

    let private writeStateResponse (writer : Utf8JsonWriter) (session : SessionState) : unit =
        writer.WriteStartObject ()
        writeSessionSummary writer session
        writer.WriteEndObject ()

    let private writeThreadResponse (writer : Utf8JsonWriter) (session : SessionState) (threadId : ThreadId) : unit =
        let state = sessionState session

        match state.ThreadState |> Map.tryFind threadId with
        | None ->
            writer.WriteStartObject ()
            writer.WriteString ("error", $"thread %d{threadIdValue threadId} does not exist")
            writer.WriteEndObject ()
        | Some threadState ->
            writer.WriteStartObject ()
            writer.WriteNumber ("id", threadIdValue threadId)
            writer.WritePropertyName "status"
            writeThreadStatus writer threadState.Status
            writer.WriteString ("activeAssembly", threadState.ActiveAssembly.FullName)
            writer.WriteNumber ("activeFrame", frameIdValue threadState.ActiveMethodState)

            writeValueArray
                writer
                "frames"
                (threadState.MethodStates |> Map.toSeq)
                (fun writer (frameId, frame) -> writeFrameDetails writer threadState.ActiveMethodState frameId frame)

            writer.WriteEndObject ()

    let private hasThread (session : SessionState) (threadId : ThreadId) : bool =
        let state = sessionState session
        state.ThreadState |> Map.containsKey threadId

    let private writeHeapObjectResponse
        (writer : Utf8JsonWriter)
        (session : SessionState)
        (address : ManagedHeapAddress)
        : unit
        =
        let state = sessionState session
        writer.WriteStartObject ()
        writer.WriteNumber ("address", heapAddressValue address)

        match state.ManagedHeap.NonArrayObjects |> Map.tryFind address with
        | Some object ->
            writer.WriteString ("kind", "object")
            writer.WriteString ("concreteType", string object.ConcreteType)
            writer.WriteString ("contents", string object.Contents)
            writeOptionalString writer "string" (ManagedHeap.getStringContents address state.ManagedHeap)
            writer.WriteString ("syncBlock", string object.SyncBlock)
        | None ->
            match state.ManagedHeap.Arrays |> Map.tryFind address with
            | Some array ->
                writer.WriteString ("kind", "array")
                writer.WriteString ("concreteType", string array.ConcreteType)
                writer.WriteNumber ("length", array.Length)
                writeValueArray writer "elements" array.Elements writeCliType
            | None ->
                writer.WriteString ("kind", "missing")
                writer.WriteString ("error", $"heap address %d{heapAddressValue address} does not exist")

        writer.WriteEndObject ()

    let private hasHeapAddress (session : SessionState) (address : ManagedHeapAddress) : bool =
        let state = sessionState session

        (state.ManagedHeap.NonArrayObjects |> Map.containsKey address)
        || (state.ManagedHeap.Arrays |> Map.containsKey address)

    type private DebuggerHttpResponse =
        {
            StatusCode : int
            ContentType : string
            Body : byte[]
            ExtraHeaders : (string * string) list
        }

    type private HandlerResult =
        {
            Response : DebuggerHttpResponse
            StopAfterResponse : bool
            ReleaseActiveStepRequestAfterResponse : bool
        }

    let private jsonResponse (statusCode : int) (write : Utf8JsonWriter -> unit) : DebuggerHttpResponse =
        use stream = new MemoryStream ()

        let options = JsonWriterOptions (Indented = true)

        use writer = new Utf8JsonWriter (stream, options)
        write writer
        writer.Flush ()

        {
            StatusCode = statusCode
            ContentType = "application/json; charset=utf-8"
            Body = stream.ToArray ()
            ExtraHeaders = []
        }

    let private textResponse (statusCode : int) (text : string) : DebuggerHttpResponse =
        {
            StatusCode = statusCode
            ContentType = "text/plain; charset=utf-8"
            Body = Encoding.UTF8.GetBytes text
            ExtraHeaders = []
        }

    let private unauthorisedResponse : DebuggerHttpResponse =
        { textResponse 401 "Unauthorized" with
            ExtraHeaders = [ "WWW-Authenticate", "Bearer" ]
        }

    let private writeResponse (context : HttpContext) (response : DebuggerHttpResponse) : Task =
        task {
            context.Response.StatusCode <- response.StatusCode
            context.Response.ContentType <- response.ContentType
            context.Response.ContentLength <- Nullable<int64> (int64 response.Body.Length)
            context.Response.Headers.[HeaderNames.CacheControl] <- StringValues "no-store"

            for name, value in response.ExtraHeaders do
                context.Response.Headers.[name] <- StringValues value

            do! context.Response.Body.WriteAsync (response.Body, 0, response.Body.Length)
        }

    let private generateBearerToken () : string =
        let bytes = RandomNumberGenerator.GetBytes 32
        Convert.ToHexString bytes

    let private fixedTimeEquals (expected : string) (actual : string) : bool =
        // The generated bearer token has a fixed-length hex encoding, so rejecting
        // different-length headers before the fixed-time loop does not leak which
        // prefix, if any, matched.
        if expected.Length <> actual.Length then
            false
        else
            let mutable diff = 0

            for i = 0 to expected.Length - 1 do
                diff <- diff ||| (int expected.[i] ^^^ int actual.[i])

            diff = 0

    let private isAuthorised (token : string) (context : HttpContext) : bool =
        let header = context.Request.Headers.Authorization.ToString ()
        fixedTimeEquals $"Bearer %s{token}" header

    let private parsePositiveInt (name : string) (defaultValue : int) (maximum : int) (query : IQueryCollection) : int =
        let raw = query.[name].ToString ()

        if String.IsNullOrWhiteSpace raw then
            defaultValue
        else
            match Int32.TryParse raw with
            | true, value when value > 0 -> min value maximum
            | _ -> defaultValue

    type private RunStepsResult =
        {
            Session : SessionState
            StepsRun : int
            Events : DebugEvent list
            Cancelled : bool
        }

    let private runSteps
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (impls : NativeImpls)
        (cancellationToken : System.Threading.CancellationToken)
        (recordLimit : int)
        (maxSteps : int)
        (session : SessionState)
        : RunStepsResult
        =
        let events = Queue<DebugEvent> ()
        let mutable session = session
        let mutable stepsRun = 0
        let mutable keepGoing = true

        while not cancellationToken.IsCancellationRequested
              && keepGoing
              && stepsRun < maxSteps do
            let nextSession, event, countedStep = stepSession loggerFactory logger impls session
            session <- nextSession

            if events.Count = recordLimit then
                events.Dequeue () |> ignore<DebugEvent>

            events.Enqueue event

            if countedStep then
                stepsRun <- stepsRun + 1

            match session with
            | SessionState.Running _ when countedStep -> ()
            | _ -> keepGoing <- false

        {
            Session = session
            StepsRun = stepsRun
            Events = events |> Seq.toList
            Cancelled = cancellationToken.IsCancellationRequested && keepGoing && stepsRun < maxSteps
        }

    let private helpText (baseUrl : string) : string =
        String.concat
            Environment.NewLine
            [
                $"PawPrint debugger server at %s{baseUrl}"
                "GET  /state"
                "POST /step?count=1"
                "POST /run?maxSteps=10000"
                "GET  /thread/{id}"
                "GET  /heap/{address}"
                "POST /reset"
                "POST /stop"
            ]

    let private requestBaseUrl (context : HttpContext) : string =
        let pathBase = context.Request.PathBase.ToString ()
        $"{context.Request.Scheme}://{context.Request.Host}%s{pathBase}/"

    let internal configureLoopbackEphemeralPort (webHost : IWebHostBuilder) : unit =
        webHost.ConfigureKestrel (fun options -> options.Listen (IPAddress.Loopback, 0))
        |> ignore<IWebHostBuilder>

    let internal baseUrl (app : WebApplication) : string =
        let server = app.Services.GetRequiredService<IServer> ()
        let addresses = server.Features.Get<IServerAddressesFeature> ()

        if isNull addresses || addresses.Addresses.Count <> 1 then
            failwith $"Expected exactly one debugger server address, got %O{addresses}"

        let address = addresses.Addresses |> Seq.exactlyOne

        if address.EndsWith ("/", StringComparison.Ordinal) then
            address
        else
            address + "/"

    let internal createApp
        (loggerFactory : ILoggerFactory)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (impls : NativeImpls)
        (argv : string list)
        (token : string)
        (configureWebHost : IWebHostBuilder -> unit)
        : WebApplication * System.Threading.CancellationTokenSource
        =
        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App.DebuggerServer"

        let mutable session =
            prepareSession loggerFactory dllPath dotnetRuntimeDirs impls argv

        let sessionLock = obj ()
        let stopStateLock = obj ()
        let mutable stopRequested = false
        let mutable activeStepRequests = 0

        let builder = WebApplication.CreateBuilder [||]
        builder.Logging.ClearProviders () |> ignore<ILoggingBuilder>

        configureWebHost builder.WebHost

        let app = builder.Build ()
        let stopCts = new System.Threading.CancellationTokenSource ()

        let requestStop () : unit =
            lock
                stopStateLock
                (fun () ->
                    stopRequested <- true
                    stopCts.Cancel ()
                )

        let isStopRequested () : bool =
            lock stopStateLock (fun () -> stopRequested)

        app.Use (fun (context : HttpContext) (next : RequestDelegate) ->
            task {
                if isAuthorised token context then
                    return! next.Invoke context
                else
                    do! writeResponse context unauthorisedResponse
            }
            :> Task
        )
        |> ignore<IApplicationBuilder>

        let responseOnly (response : DebuggerHttpResponse) : HandlerResult =
            {
                Response = response
                StopAfterResponse = false
                ReleaseActiveStepRequestAfterResponse = false
            }

        let stoppingResponse (response : DebuggerHttpResponse) : HandlerResult =
            {
                Response = response
                StopAfterResponse = true
                ReleaseActiveStepRequestAfterResponse = false
            }

        let stepResponse (response : DebuggerHttpResponse) : HandlerResult =
            {
                Response = response
                StopAfterResponse = false
                ReleaseActiveStepRequestAfterResponse = true
            }

        app.Run (fun context ->
            task {
                let method = context.Request.Method.ToUpperInvariant ()

                let segments =
                    match context.Request.Path.Value with
                    | null -> []
                    | path -> pathSegments path

                let isStopRequest =
                    match method, segments with
                    | "POST", [ "stop" ] -> true
                    | _ -> false

                let result =
                    if isStopRequest then
                        requestStop ()

                        if System.Threading.Volatile.Read (&activeStepRequests) = 0 then
                            stoppingResponse (textResponse 200 "stopping")
                        else
                            responseOnly (textResponse 200 "stopping")
                    else
                        try
                            lock
                                sessionLock
                                (fun () ->
                                    match method, segments with
                                    | "GET", []
                                    | "GET", [ "help" ] ->
                                        responseOnly (textResponse 200 (helpText (requestBaseUrl context)))
                                    | "GET", [ "state" ] ->
                                        responseOnly (
                                            jsonResponse 200 (fun writer -> writeStateResponse writer session)
                                        )
                                    | "POST", [ "step" ] ->
                                        let count = parsePositiveInt "count" 1 1000 context.Request.Query

                                        System.Threading.Interlocked.Increment (&activeStepRequests) |> ignore<int>

                                        let mutable releaseAfterResponse = false

                                        try
                                            let result =
                                                runSteps loggerFactory logger impls stopCts.Token count count session

                                            session <- result.Session

                                            let response =
                                                jsonResponse
                                                    200
                                                    (fun writer ->
                                                        writer.WriteStartObject ()
                                                        writer.WriteNumber ("requestedSteps", count)
                                                        writer.WriteNumber ("stepsRun", result.StepsRun)
                                                        writer.WriteBoolean ("cancelled", result.Cancelled)
                                                        writeValueArray writer "events" result.Events writeEvent
                                                        writeSessionSummary writer session
                                                        writer.WriteEndObject ()
                                                    )

                                            releaseAfterResponse <- true
                                            stepResponse response
                                        finally
                                            if not releaseAfterResponse then
                                                System.Threading.Interlocked.Decrement (&activeStepRequests)
                                                |> ignore<int>
                                    | "POST", [ "run" ] ->
                                        let maxSteps = parsePositiveInt "maxSteps" 10000 1000000 context.Request.Query

                                        System.Threading.Interlocked.Increment (&activeStepRequests) |> ignore<int>

                                        let mutable releaseAfterResponse = false

                                        try
                                            let result =
                                                runSteps loggerFactory logger impls stopCts.Token 20 maxSteps session

                                            session <- result.Session

                                            let response =
                                                jsonResponse
                                                    200
                                                    (fun writer ->
                                                        writer.WriteStartObject ()
                                                        writer.WriteNumber ("maxSteps", maxSteps)
                                                        writer.WriteNumber ("stepsRun", result.StepsRun)
                                                        writer.WriteBoolean ("cancelled", result.Cancelled)

                                                        writeValueArray
                                                            writer
                                                            "recentEvents"
                                                            result.Events
                                                            writeEvent

                                                        writeSessionSummary writer session
                                                        writer.WriteEndObject ()
                                                    )

                                            releaseAfterResponse <- true
                                            stepResponse response
                                        finally
                                            if not releaseAfterResponse then
                                                System.Threading.Interlocked.Decrement (&activeStepRequests)
                                                |> ignore<int>
                                    | "GET", [ "thread" ; rawThread ] ->
                                        match Int32.TryParse rawThread with
                                        | true, thread ->
                                            let threadId = ThreadId.ThreadId thread
                                            let statusCode = if hasThread session threadId then 200 else 404

                                            jsonResponse
                                                statusCode
                                                (fun writer -> writeThreadResponse writer session threadId)
                                            |> responseOnly
                                        | _ -> responseOnly (textResponse 400 $"Invalid thread id: %s{rawThread}")
                                    | "GET", [ "heap" ; rawAddress ] ->
                                        match Int32.TryParse rawAddress with
                                        | true, address ->
                                            let address = ManagedHeapAddress.ManagedHeapAddress address
                                            let statusCode = if hasHeapAddress session address then 200 else 404

                                            jsonResponse
                                                statusCode
                                                (fun writer -> writeHeapObjectResponse writer session address)
                                            |> responseOnly
                                        | _ -> responseOnly (textResponse 400 $"Invalid heap address: %s{rawAddress}")
                                    | "POST", [ "reset" ] ->
                                        session <- prepareSession loggerFactory dllPath dotnetRuntimeDirs impls argv

                                        jsonResponse
                                            200
                                            (fun writer ->
                                                writer.WriteStartObject ()
                                                writer.WriteString ("status", "reset")
                                                writeSessionSummary writer session
                                                writer.WriteEndObject ()
                                            )
                                        |> responseOnly
                                    | _ -> responseOnly (textResponse 404 (helpText (requestBaseUrl context)))
                                )
                        with ex ->
                            logger.LogError (ex, "Debugger request failed")
                            responseOnly (textResponse 500 ex.Message)

                do! writeResponse context result.Response

                let mutable remainingActiveStepRequests =
                    System.Threading.Volatile.Read (&activeStepRequests)

                if result.ReleaseActiveStepRequestAfterResponse then
                    remainingActiveStepRequests <- System.Threading.Interlocked.Decrement (&activeStepRequests)

                if
                    result.StopAfterResponse
                    || (result.ReleaseActiveStepRequestAfterResponse
                        && remainingActiveStepRequests = 0
                        && isStopRequested ())
                then
                    context.RequestServices.GetRequiredService<IHostApplicationLifetime>().StopApplication ()
            }
            :> Task
        )

        app, stopCts

    let run
        (loggerFactory : ILoggerFactory)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (impls : NativeImpls)
        (argv : string list)
        : int
        =
        let token = generateBearerToken ()

        let app, stopCts =
            createApp loggerFactory dllPath dotnetRuntimeDirs impls argv token configureLoopbackEphemeralPort

        use _stopCts = stopCts

        app.Start ()

        let baseUrl = baseUrl app

        printfn "PawPrint debugger listening on %s" baseUrl
        printfn "PawPrint debugger bearer token: %s" token
        printfn "Try: curl -H 'Authorization: Bearer %s' %sstate" token baseUrl

        app.WaitForShutdown ()
        0
