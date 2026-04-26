namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Net
open System.Text
open System.Text.Json
open Microsoft.Extensions.Logging
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

    let private normalizePrefix (prefix : string) : string =
        let withScheme =
            if
                prefix.StartsWith ("http://", StringComparison.OrdinalIgnoreCase)
                || prefix.StartsWith ("https://", StringComparison.OrdinalIgnoreCase)
            then
                prefix
            else
                $"http://%s{prefix}"

        if withScheme.EndsWith ("/", StringComparison.Ordinal) then
            withScheme
        else
            withScheme + "/"

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
        writer.WriteStringValue (string value)

    let private writeCliType (writer : Utf8JsonWriter) (value : CliType) : unit = writer.WriteStringValue (string value)

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
            {
                StepNumber = stepNumber
                Kind = "instruction"
                Thread = Some (threadIdValue thread)
                Detail = string whatWeDid
            }
        | Program.ProgramStepOutcome.WorkerTerminated (_, thread) ->
            {
                StepNumber = stepNumber
                Kind = "workerTerminated"
                Thread = Some (threadIdValue thread)
                Detail = "thread terminated"
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
            }
        | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
            {
                StepNumber = stepNumber
                Kind = "deadlocked"
                Thread = None
                Detail = stuck
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
            },
            false
        | SessionState.Deadlocked (_, stuck, steps) ->
            session,
            {
                StepNumber = steps
                Kind = "alreadyDeadlocked"
                Thread = None
                Detail = stuck
            },
            false

    let private writeEvent (writer : Utf8JsonWriter) (event : DebugEvent) : unit =
        writer.WriteStartObject ()
        writer.WriteNumber ("step", event.StepNumber)
        writer.WriteString ("kind", event.Kind)
        writeOptionalInt writer "thread" event.Thread
        writer.WriteString ("detail", event.Detail)
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
                (threadState.MethodStates |> Seq.mapi (fun i frame -> FrameId.FrameId i, frame))
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

    let private writeJson (response : HttpListenerResponse) (statusCode : int) (write : Utf8JsonWriter -> unit) : unit =
        use stream = new MemoryStream ()

        let options = JsonWriterOptions (Indented = true)

        use writer = new Utf8JsonWriter (stream, options)
        write writer
        writer.Flush ()

        let bytes = stream.ToArray ()
        response.StatusCode <- statusCode
        response.ContentType <- "application/json; charset=utf-8"
        response.ContentLength64 <- int64 bytes.Length
        response.OutputStream.Write (bytes, 0, bytes.Length)

    let private writeText (response : HttpListenerResponse) (statusCode : int) (text : string) : unit =
        let bytes = Encoding.UTF8.GetBytes text
        response.StatusCode <- statusCode
        response.ContentType <- "text/plain; charset=utf-8"
        response.ContentLength64 <- int64 bytes.Length
        response.OutputStream.Write (bytes, 0, bytes.Length)

    let private parsePositiveInt
        (name : string)
        (defaultValue : int)
        (maximum : int)
        (request : HttpListenerRequest)
        : int
        =
        let raw = request.QueryString.[name]

        if String.IsNullOrWhiteSpace raw then
            defaultValue
        else
            match Int32.TryParse raw with
            | true, value when value > 0 -> min value maximum
            | _ -> defaultValue

    let private runSteps
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (impls : NativeImpls)
        (recordLimit : int)
        (maxSteps : int)
        (session : SessionState)
        : SessionState * int * DebugEvent list
        =
        let events = ResizeArray<DebugEvent> ()
        let mutable session = session
        let mutable stepsRun = 0
        let mutable keepGoing = true

        while keepGoing && stepsRun < maxSteps do
            let nextSession, event, countedStep = stepSession loggerFactory logger impls session
            session <- nextSession

            if events.Count = recordLimit then
                events.RemoveAt 0

            events.Add event

            if countedStep then
                stepsRun <- stepsRun + 1

            match session with
            | SessionState.Running _ when countedStep -> ()
            | _ -> keepGoing <- false

        session, stepsRun, events |> Seq.toList

    let private helpText (prefix : string) : string =
        String.concat
            Environment.NewLine
            [
                $"PawPrint debugger server at %s{prefix}"
                "GET  /state"
                "POST /step?count=1"
                "POST /run?maxSteps=10000"
                "GET  /thread/{id}"
                "GET  /heap/{address}"
                "POST /reset"
                "POST /stop"
            ]

    let run
        (loggerFactory : ILoggerFactory)
        (prefix : string)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (impls : NativeImpls)
        (argv : string list)
        : int
        =
        let prefix = normalizePrefix prefix
        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App.DebuggerServer"

        let mutable session =
            prepareSession loggerFactory dllPath dotnetRuntimeDirs impls argv

        let mutable shouldStop = false

        use listener = new HttpListener ()
        listener.Prefixes.Add prefix
        listener.Start ()

        printfn "PawPrint debugger listening on %s" prefix
        printfn "Try GET %sstate" prefix

        while not shouldStop do
            let context = listener.GetContext ()
            let request = context.Request
            let response = context.Response
            let path = request.Url.AbsolutePath.TrimEnd '/'
            let segments = path.Split ('/', StringSplitOptions.RemoveEmptyEntries)

            try
                try
                    match request.HttpMethod.ToUpperInvariant (), segments |> Array.toList with
                    | "GET", []
                    | "GET", [ "help" ] -> writeText response 200 (helpText prefix)
                    | "GET", [ "state" ] -> writeJson response 200 (fun writer -> writeStateResponse writer session)
                    | "POST", [ "step" ] ->
                        let count = parsePositiveInt "count" 1 1000 request

                        let nextSession, stepsRun, events =
                            runSteps loggerFactory logger impls count count session

                        session <- nextSession

                        writeJson
                            response
                            200
                            (fun writer ->
                                writer.WriteStartObject ()
                                writer.WriteNumber ("requestedSteps", count)
                                writer.WriteNumber ("stepsRun", stepsRun)
                                writeValueArray writer "events" events writeEvent
                                writeSessionSummary writer session
                                writer.WriteEndObject ()
                            )
                    | "POST", [ "run" ] ->
                        let maxSteps = parsePositiveInt "maxSteps" 10000 1000000 request

                        let nextSession, stepsRun, events =
                            runSteps loggerFactory logger impls 20 maxSteps session

                        session <- nextSession

                        writeJson
                            response
                            200
                            (fun writer ->
                                writer.WriteStartObject ()
                                writer.WriteNumber ("maxSteps", maxSteps)
                                writer.WriteNumber ("stepsRun", stepsRun)
                                writeValueArray writer "recentEvents" events writeEvent
                                writeSessionSummary writer session
                                writer.WriteEndObject ()
                            )
                    | "GET", [ "thread" ; rawThread ] ->
                        match Int32.TryParse rawThread with
                        | true, thread ->
                            let threadId = ThreadId.ThreadId thread
                            let statusCode = if hasThread session threadId then 200 else 404
                            writeJson response statusCode (fun writer -> writeThreadResponse writer session threadId)
                        | _ -> writeText response 400 $"Invalid thread id: %s{rawThread}"
                    | "GET", [ "heap" ; rawAddress ] ->
                        match Int32.TryParse rawAddress with
                        | true, address ->
                            let address = ManagedHeapAddress.ManagedHeapAddress address
                            let statusCode = if hasHeapAddress session address then 200 else 404

                            writeJson response statusCode (fun writer -> writeHeapObjectResponse writer session address)
                        | _ -> writeText response 400 $"Invalid heap address: %s{rawAddress}"
                    | "POST", [ "reset" ] ->
                        session <- prepareSession loggerFactory dllPath dotnetRuntimeDirs impls argv

                        writeJson
                            response
                            200
                            (fun writer ->
                                writer.WriteStartObject ()
                                writer.WriteString ("status", "reset")
                                writeSessionSummary writer session
                                writer.WriteEndObject ()
                            )
                    | "POST", [ "stop" ] ->
                        shouldStop <- true
                        writeText response 200 "stopping"
                    | _ -> writeText response 404 (helpText prefix)
                with ex ->
                    logger.LogError (ex, "Debugger request failed")

                    try
                        writeText response 500 ex.Message
                    with writeError ->
                        logger.LogError (writeError, "Could not write debugger error response")
            finally
                try
                    response.OutputStream.Close ()
                with closeError ->
                    logger.LogDebug (closeError, "Could not close debugger response")

        listener.Stop ()
        0
