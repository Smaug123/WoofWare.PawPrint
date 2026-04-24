namespace WoofWare.PawPrint.Logging

open System
open System.Collections.Concurrent
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
        use _ =
            new FileStream (filePath, FileMode.CreateNew, FileAccess.Write, FileShare.ReadWrite)

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
                            new FileStream (filePath, FileMode.Append, FileAccess.Write, FileShare.ReadWrite)

                        use writer = new StreamWriter (fileStream, utf8NoBom)
                        writer.WriteLine payload
                )

    interface IDisposable with
        member _.Dispose () =
            lock
                gate
                (fun () ->
                    if not disposed then
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

[<RequireQualifiedAccess>]
module PawPrintLogging =
    let private logDirectoryEnvVar : string = "PAWPRINT_LOG_DIR"
    let private logLevelEnvVar : string = "PAWPRINT_LOG_LEVEL"
    let private userRunIdEnvVar : string = "PAWPRINT_LOG_RUN_ID"

    let private runDirectories =
        ConcurrentDictionary<string, Lazy<string>> (StringComparer.Ordinal)

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

    let private tryGetNonWhiteSpaceEnvironmentVariable (name : string) : string option =
        let value = Environment.GetEnvironmentVariable name

        if String.IsNullOrWhiteSpace value then None else Some value

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

    let private runDirectoryForRoot (rootDirectory : string) : string =
        let rootDirectory = Path.GetFullPath rootDirectory

        let runDirectory =
            runDirectories.GetOrAdd (
                rootDirectory,
                Func<string, Lazy<string>> (fun rootDirectory -> lazy (createRunDirectory rootDirectory))
            )

        runDirectory.Value

    let tryCreateSinkFromEnvironment
        (componentName : string)
        (fileNameStem : string)
        (callerStaticProperties : seq<string * string>)
        : JsonLinesLogSink option
        =
        match tryGetNonWhiteSpaceEnvironmentVariable logDirectoryEnvVar with
        | None -> None
        | Some rootDirectory ->
            let runDirectory = runDirectoryForRoot rootDirectory
            let generatedRunId = Path.GetFileName runDirectory
            let fileNameStem = sanitizeFileNameStem fileNameStem
            let guid = Guid.NewGuid().ToString "N"
            let fileName = $"%s{fileNameStem}-%s{guid}.jsonl"
            let filePath = Path.Combine (runDirectory, fileName)

            let topLevelProperties = Dictionary<string, string> (StringComparer.Ordinal)
            let staticProperties = Dictionary<string, string> (StringComparer.Ordinal)

            for key, value in callerStaticProperties do
                if not (String.IsNullOrWhiteSpace key) then
                    staticProperties.[key] <- value

            topLevelProperties.["component"] <- componentName
            topLevelProperties.["run_id"] <- generatedRunId
            topLevelProperties.["run_directory"] <- runDirectory

            match tryGetNonWhiteSpaceEnvironmentVariable userRunIdEnvVar with
            | Some userRunId -> topLevelProperties.["user_run_id"] <- userRunId
            | None -> ()

            new JsonLinesLogSink (filePath, minimumLevelFromEnvironment (), topLevelProperties, staticProperties)
            |> Some

    let tryCreateProviderFromEnvironment
        (componentName : string)
        (fileNameStem : string)
        (staticProperties : seq<string * string>)
        : ILoggerProvider option
        =
        tryCreateSinkFromEnvironment componentName fileNameStem staticProperties
        |> Option.map (fun sink -> new JsonLinesLoggerProvider (sink) :> ILoggerProvider)
