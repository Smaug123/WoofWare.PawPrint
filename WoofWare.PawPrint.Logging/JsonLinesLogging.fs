namespace WoofWare.PawPrint.Logging

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open Microsoft.Extensions.Logging

type JsonLinesLogSink
    internal
    (
        filePath : string,
        minimumLevel : LogLevel,
        topLevelProperties : IReadOnlyDictionary<string, string>,
        staticProperties : IReadOnlyDictionary<string, string>
    )
    =
    let gate = obj ()
    let mutable disposed = false
    let utf8NoBom = new UTF8Encoding (encoderShouldEmitUTF8Identifier = false)

    do
        // Construction claims this sink's GUID-suffixed path. Each sink must own its file;
        // the instance lock below only serializes writers using this sink instance.
        use _ =
            new FileStream (filePath, FileMode.CreateNew, FileAccess.Write, FileShare.Read)

        ()

    let reportWriteFailure (failure : exn) : unit =
        try
            Console.Error.WriteLine (
                $"PawPrint JSONL log sink failed to write %s{filePath}: %s{failure.GetType().FullName}: %s{failure.Message}"
            )
        with _ ->
            ()

    let writeValue (json : Utf8JsonWriter) (value : obj) : unit =
        match value with
        | null -> json.WriteNullValue ()
        | :? string as value -> json.WriteStringValue value
        | :? bool as value -> json.WriteBooleanValue value
        | :? byte as value -> json.WriteNumberValue value
        | :? sbyte as value -> json.WriteNumberValue value
        | :? int16 as value -> json.WriteNumberValue value
        | :? uint16 as value -> json.WriteNumberValue value
        | :? int as value -> json.WriteNumberValue value
        | :? uint32 as value -> json.WriteNumberValue value
        | :? int64 as value -> json.WriteNumberValue value
        | :? uint64 as value -> json.WriteNumberValue value
        | :? decimal as value -> json.WriteNumberValue value
        | :? float as value when not (Double.IsNaN value || Double.IsInfinity value) -> json.WriteNumberValue value
        | :? float32 as value when not (Single.IsNaN value || Single.IsInfinity value) -> json.WriteNumberValue value
        | :? DateTime as value -> json.WriteStringValue value
        | :? DateTimeOffset as value -> json.WriteStringValue value
        // Logging must prefer a lossy representation over throwing from ILogger.Log.
        | _ -> json.WriteStringValue (string value)

    let structuredStateProperties (state : 'state) : KeyValuePair<string, obj> list =
        match box state with
        | :? IEnumerable<KeyValuePair<string, obj>> as values -> values |> Seq.toList
        | _ -> []

    member _.FilePath : string = filePath

    member _.IsEnabled (logLevel : LogLevel) : bool =
        logLevel <> LogLevel.None && logLevel >= minimumLevel

    member this.Write<'state>
        (
            logLevel : LogLevel,
            category : string,
            eventId : EventId,
            state : 'state,
            ex : exn,
            formatter : Func<'state, exn, string>
        )
        : unit
        =
        try
            if this.IsEnabled logLevel then
                let message =
                    try
                        formatter.Invoke (state, ex)
                    with _ ->
                        "<formatter threw>"

                let stateProperties = structuredStateProperties state

                let messageTemplate =
                    stateProperties
                    |> List.tryPick (fun kv ->
                        if kv.Key = "{OriginalFormat}" then
                            Some (string kv.Value)
                        else
                            None
                    )

                let payload =
                    use stream = new MemoryStream ()
                    use json = new Utf8JsonWriter (stream)

                    json.WriteStartObject ()
                    json.WriteString ("timestamp", DateTimeOffset.UtcNow)
                    json.WriteString ("level", logLevel.ToString ())
                    json.WriteString ("logger", category)
                    json.WriteNumber ("event_id", eventId.Id)

                    if not (String.IsNullOrWhiteSpace eventId.Name) then
                        json.WriteString ("event_name", eventId.Name)

                    json.WriteString ("message", message)

                    match messageTemplate with
                    | Some messageTemplate -> json.WriteString ("message_template", messageTemplate)
                    | None -> ()

                    for KeyValue (key, value) in topLevelProperties do
                        if not (String.IsNullOrWhiteSpace key) then
                            json.WritePropertyName key
                            json.WriteStringValue value

                    if staticProperties.Count > 0 then
                        json.WriteStartObject "properties"

                        for KeyValue (key, value) in staticProperties do
                            if not (String.IsNullOrWhiteSpace key) then
                                json.WritePropertyName key
                                json.WriteStringValue value

                        json.WriteEndObject ()

                    match ex with
                    | null -> ()
                    | ex ->
                        json.WriteStartObject "exception"
                        json.WriteString ("type", ex.GetType().FullName)
                        json.WriteString ("message", ex.Message)

                        if not (String.IsNullOrWhiteSpace ex.StackTrace) then
                            json.WriteString ("stack_trace", ex.StackTrace)

                        json.WriteEndObject ()

                    json.WriteStartObject "fields"

                    for KeyValue (key, value) in stateProperties do
                        if key <> "{OriginalFormat}" && not (String.IsNullOrWhiteSpace key) then
                            json.WritePropertyName key
                            writeValue json value

                    json.WriteEndObject ()
                    json.WriteEndObject ()
                    json.Flush ()
                    Encoding.UTF8.GetString (stream.ToArray ())

                lock
                    gate
                    (fun () ->
                        if not disposed then
                            use fileStream =
                                new FileStream (filePath, FileMode.Append, FileAccess.Write, FileShare.Read)

                            use writer = new StreamWriter (fileStream, utf8NoBom)
                            writer.WriteLine payload
                    )
        with ex ->
            reportWriteFailure ex

    interface IDisposable with
        member _.Dispose () =
            lock
                gate
                (fun () ->
                    if not disposed then
                        // This sink has no buffered data; dispose only marks future writes as dropped.
                        disposed <- true
                )

type private JsonLinesLogger (sink : JsonLinesLogSink, category : string) =
    interface ILogger with
        member _.BeginScope (_state : 'state) : IDisposable =
            // Scopes are intentionally ignored; PawPrint sends query context as static properties.
            { new IDisposable with
                member _.Dispose () = ()
            }

        member _.IsEnabled (logLevel : LogLevel) : bool = sink.IsEnabled logLevel

        member _.Log<'state>
            (logLevel : LogLevel, eventId : EventId, state : 'state, ex : exn, formatter : Func<'state, exn, string>)
            : unit
            =
            sink.Write (logLevel, category, eventId, state, ex, formatter)

type JsonLinesLoggerProvider internal (sink : JsonLinesLogSink) =
    interface ILoggerProvider with
        member _.CreateLogger (categoryName : string) : ILogger =
            JsonLinesLogger (sink, categoryName) :> ILogger

        member _.Dispose () = (sink :> IDisposable).Dispose ()

/// Destination configuration for JSONL file logging. Construct via `LoggingConfig.fromEnv` at the
/// I/O boundary (typically the composition root), or via `LoggingConfig.forRunDirectory` in tests
/// that want explicit control over where logs go. The type is deliberately a plain value: callers
/// thread it down to whoever creates sinks rather than having the library read process-wide state.
type LoggingConfig =
    {
        /// Stamped onto every event as the `component` top-level property.
        ComponentName : string
        /// Fully-qualified, already-existing directory under which this run's `.jsonl` files live.
        RunDirectory : string
        /// Minimum level the produced sinks will emit.
        MinimumLevel : LogLevel
        /// Optional caller-supplied run ID (recorded on events as `user_run_id`, not used in paths).
        UserRunId : string option
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LoggingConfig =
    let private logDirectoryEnvVar : string = "PAWPRINT_LOG_DIR"
    let private logLevelEnvVar : string = "PAWPRINT_LOG_LEVEL"
    let private userRunIdEnvVar : string = "PAWPRINT_LOG_RUN_ID"

    let private tryGetNonWhiteSpaceEnvironmentVariable (name : string) : string option =
        let value = Environment.GetEnvironmentVariable name

        if String.IsNullOrWhiteSpace value then None else Some value

    /// Reads `PAWPRINT_LOG_LEVEL`, defaulting to `LogLevel.Information`. Independent of file
    /// logging: callers can set a console-only minimum level without enabling `PAWPRINT_LOG_DIR`.
    let minimumLevelFromEnvironment () : LogLevel =
        match tryGetNonWhiteSpaceEnvironmentVariable logLevelEnvVar with
        | None -> LogLevel.Information
        | Some raw ->
            let ok, parsed = Enum.TryParse<LogLevel> (raw, true)

            if ok then
                parsed
            else
                failwith
                    $"Invalid %s{logLevelEnvVar} value %s{raw}; expected a Microsoft.Extensions.Logging.LogLevel name"

    let private createRunDirectory (rootDirectory : string) : string =
        let rootDirectory = Path.GetFullPath rootDirectory
        Directory.CreateDirectory rootDirectory |> ignore<DirectoryInfo>

        let timestamp =
            DateTimeOffset.UtcNow.ToString ("yyyyMMddTHHmmss.fffffffZ", CultureInfo.InvariantCulture)

        let guid = Guid.NewGuid().ToString "N"
        let runDirectoryName = $"%s{timestamp}-pid%d{Environment.ProcessId}-%s{guid}"
        let runDirectory = Path.Combine (rootDirectory, runDirectoryName)
        Directory.CreateDirectory runDirectory |> ignore<DirectoryInfo>
        runDirectory

    /// Returns `None` when `PAWPRINT_LOG_DIR` is unset. When it is set, creates a fresh
    /// timestamped run directory under that root and returns a config pointing at it; throws
    /// if `PAWPRINT_LOG_LEVEL` is malformed or the root directory cannot be created.
    let fromEnv (componentName : string) : LoggingConfig option =
        tryGetNonWhiteSpaceEnvironmentVariable logDirectoryEnvVar
        |> Option.map (fun rootDirectory ->
            {
                ComponentName = componentName
                RunDirectory = createRunDirectory rootDirectory
                MinimumLevel = minimumLevelFromEnvironment ()
                UserRunId = tryGetNonWhiteSpaceEnvironmentVariable userRunIdEnvVar
            }
        )

    /// Build a config pointed at an already-existing run directory. Performs no I/O and reads
    /// no process-wide state; intended for tests and for library callers that want to control
    /// where logs land without going through the environment.
    let forRunDirectory (componentName : string) (runDirectory : string) (minimumLevel : LogLevel) : LoggingConfig =
        {
            ComponentName = componentName
            RunDirectory = Path.GetFullPath runDirectory
            MinimumLevel = minimumLevel
            UserRunId = None
        }

[<RequireQualifiedAccess>]
module PawPrintLogging =
    let private invalidFileNameChars : Set<char> =
        Path.GetInvalidFileNameChars () |> Seq.append [ '/' ; '\\' ; ':' ] |> Set.ofSeq

    let private sanitizeFileNameStem (value : string) : string =
        let chars =
            value.ToCharArray ()
            |> Array.map (fun c ->
                if invalidFileNameChars.Contains c || Char.IsControl c then
                    '_'
                else
                    c
            )

        let sanitized = String chars

        let sanitized =
            if String.IsNullOrWhiteSpace sanitized then
                "pawprint"
            else
                sanitized.Trim ()

        if sanitized.Length > 120 then
            sanitized.Substring (0, 120)
        else
            sanitized

    let createSink
        (config : LoggingConfig)
        (fileNameStem : string)
        (callerStaticProperties : seq<string * string>)
        : JsonLinesLogSink
        =
        let fileNameStem = sanitizeFileNameStem fileNameStem
        let guid = Guid.NewGuid().ToString "N"
        let fileName = $"%s{fileNameStem}-%s{guid}.jsonl"
        let filePath = Path.Combine (config.RunDirectory, fileName)

        let topLevelProperties = Dictionary<string, string> (StringComparer.Ordinal)
        let staticProperties = Dictionary<string, string> (StringComparer.Ordinal)

        for key, value in callerStaticProperties do
            if not (String.IsNullOrWhiteSpace key) then
                staticProperties.[key] <- value

        topLevelProperties.["component"] <- config.ComponentName
        topLevelProperties.["run_id"] <- Path.GetFileName config.RunDirectory
        topLevelProperties.["run_directory"] <- config.RunDirectory

        match config.UserRunId with
        | Some userRunId -> topLevelProperties.["user_run_id"] <- userRunId
        | None -> ()

        new JsonLinesLogSink (filePath, config.MinimumLevel, topLevelProperties, staticProperties)

    let createProvider
        (config : LoggingConfig)
        (fileNameStem : string)
        (staticProperties : seq<string * string>)
        : ILoggerProvider
        =
        new JsonLinesLoggerProvider (createSink config fileNameStem staticProperties) :> ILoggerProvider
