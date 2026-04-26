namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint.Logging

[<TestFixture>]
[<Parallelizable(ParallelScope.Children)>]
module TestJsonLinesLogging =
    type private GeneratedLogValue =
        | StringValue of string
        | BoolValue of bool
        | IntValue of int
        | NullValue

    let private withCapturedConsoleError (action : unit -> 'result) : 'result * string =
        let original = Console.Error
        use writer = new StringWriter ()

        try
            Console.SetError writer
            let result = action ()
            result, writer.ToString ()
        finally
            Console.SetError original

    let private withTempRunDirectory (action : string -> 'result) : 'result =
        let guid = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-logging-test-%s{guid}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            action root
        finally
            try
                if Directory.Exists root then
                    Directory.Delete (root, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    let private withEnvironmentVariable (name : string) (value : string option) (action : unit -> 'result) : 'result =
        let previous = Environment.GetEnvironmentVariable name

        try
            let value =
                match value with
                | Some value -> value
                | None -> null

            Environment.SetEnvironmentVariable (name, value)
            action ()
        finally
            Environment.SetEnvironmentVariable (name, previous)

    let private configFor (runDirectory : string) : LoggingConfig =
        { LoggingConfig.forRunDirectory "test" runDirectory LogLevel.Debug with
            UserRunId = Some "user-supplied-run"
        }

    let private asObject (value : GeneratedLogValue) : obj =
        match value with
        | GeneratedLogValue.StringValue value -> value :> obj
        | GeneratedLogValue.BoolValue value -> value :> obj
        | GeneratedLogValue.IntValue value -> value :> obj
        | GeneratedLogValue.NullValue -> null

    let private assertJsonValue (expected : GeneratedLogValue) (actual : JsonElement) : unit =
        match expected with
        | GeneratedLogValue.StringValue expected ->
            actual.ValueKind |> shouldEqual JsonValueKind.String
            actual.GetString () |> shouldEqual expected
        | GeneratedLogValue.BoolValue expected ->
            actual.ValueKind
            |> shouldEqual (if expected then JsonValueKind.True else JsonValueKind.False)

            actual.GetBoolean () |> shouldEqual expected
        | GeneratedLogValue.IntValue expected ->
            actual.ValueKind |> shouldEqual JsonValueKind.Number
            actual.GetInt32 () |> shouldEqual expected
        | GeneratedLogValue.NullValue -> actual.ValueKind |> shouldEqual JsonValueKind.Null

    let private genDistinctFromPool<'a when 'a : equality> (count : int) (pool : 'a list) : Gen<'a list> =
        let rec go (count : int) (pool : 'a list) : Gen<'a list> =
            if count = 0 then
                Gen.constant []
            else
                gen {
                    let! next = Gen.elements pool
                    let remaining = pool |> List.filter ((<>) next)
                    let! rest = go (count - 1) remaining
                    return next :: rest
                }

        go count pool

    let private genLogValue : Gen<GeneratedLogValue> =
        Gen.oneof
            [
                Gen.elements [ "" ; "plain" ; "quote \" value" ; "slash \\ value" ; "line\nbreak" ]
                |> Gen.map GeneratedLogValue.StringValue
                ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map GeneratedLogValue.BoolValue
                Gen.choose (-100000, 100000) |> Gen.map GeneratedLogValue.IntValue
                Gen.constant GeneratedLogValue.NullValue
            ]

    let private genLogFields : Gen<(string * GeneratedLogValue) list> =
        let fieldNamePool =
            [
                "alpha"
                "beta"
                "event_id"
                "level"
                "logger"
                "message"
                "run_id"
                "with space"
            ]

        gen {
            let! count = Gen.choose (0, fieldNamePool.Length)
            let! names = genDistinctFromPool count fieldNamePool
            let! values = Gen.listOfLength count genLogValue
            return List.zip names values
        }

    let private logFieldsRoundTrip (config : LoggingConfig) (fields : (string * GeneratedLogValue) list) : unit =
        let sink = PawPrintLogging.createSink config "round-trip-test" Seq.empty

        use _sinkResource = sink

        let state =
            fields
            |> List.map (fun (key, value) -> KeyValuePair<string, obj> (key, asObject value))

        sink.Write (
            LogLevel.Information,
            "RoundTripLogger",
            EventId (17, "RoundTrip"),
            state,
            null,
            Func<_, _, _> (fun _ _ -> "round-trip")
        )

        let lines = File.ReadAllLines sink.FilePath
        lines.Length |> shouldEqual 1

        use document = JsonDocument.Parse lines.[0]
        let fieldsElement = document.RootElement.GetProperty "fields"

        for fieldName, expected in fields do
            fieldsElement.TryGetProperty fieldName |> fst |> shouldEqual true
            assertJsonValue expected (fieldsElement.GetProperty fieldName)

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 100

    [<Test>]
    let ``JSONL sink preserves structured log fields`` () : unit =
        withTempRunDirectory (fun runDirectory ->
            let config = configFor runDirectory

            let provider =
                PawPrintLogging.createProvider config "structured-field-test" [ "source_file", "LoggingCase.cs" ]

            use _providerResource = provider
            let logger = provider.CreateLogger "StructuredLogger"
            logger.LogInformation ("Saw value {Value}", 42)

            let files = Directory.GetFiles (runDirectory, "*.jsonl")
            files.Length |> shouldEqual 1
            let lines = File.ReadAllLines files.[0]
            lines.Length |> shouldEqual 1

            use document = JsonDocument.Parse lines.[0]
            let rootElement = document.RootElement

            rootElement.GetProperty("component").GetString () |> shouldEqual "test"

            rootElement.GetProperty("properties").GetProperty("source_file").GetString ()
            |> shouldEqual "LoggingCase.cs"

            rootElement.GetProperty("user_run_id").GetString ()
            |> shouldEqual "user-supplied-run"

            rootElement.GetProperty("logger").GetString () |> shouldEqual "StructuredLogger"

            rootElement.GetProperty("message_template").GetString ()
            |> shouldEqual "Saw value {Value}"

            rootElement.GetProperty("fields").GetProperty("Value").GetInt32 ()
            |> shouldEqual 42
        )

    [<Test>]
    let ``Static properties are nested so reserved top-level fields cannot collide`` () : unit =
        withTempRunDirectory (fun runDirectory ->
            let config = configFor runDirectory

            let sink =
                PawPrintLogging.createSink
                    config
                    "collision-test"
                    [ "level", "caller-level" ; "message", "caller-message" ]

            use _sinkResource = sink

            sink.Write (
                LogLevel.Information,
                "CollisionLogger",
                EventId (0),
                ([] : KeyValuePair<string, obj> list),
                null,
                Func<KeyValuePair<string, obj> list, exn, string> (fun _ _ -> "real message")
            )

            let lines = File.ReadAllLines sink.FilePath
            lines.Length |> shouldEqual 1

            use document = JsonDocument.Parse lines.[0]
            let rootElement = document.RootElement

            rootElement.GetProperty("level").GetString () |> shouldEqual "Information"
            rootElement.GetProperty("message").GetString () |> shouldEqual "real message"

            rootElement.GetProperty("properties").GetProperty("level").GetString ()
            |> shouldEqual "caller-level"

            rootElement.GetProperty("properties").GetProperty("message").GetString ()
            |> shouldEqual "caller-message"
        )

    // Mutates `Console.Error`, which is process-global; isolate from the rest of the fixture.
    [<Test>]
    [<NonParallelizable>]
    let ``Write failures do not escape ILogger callers`` () : unit =
        withTempRunDirectory (fun runDirectory ->
            let config = configFor runDirectory

            let _, stderr =
                withCapturedConsoleError (fun () ->
                    let sink = PawPrintLogging.createSink config "write-failure-test" Seq.empty

                    File.Delete sink.FilePath
                    Directory.CreateDirectory sink.FilePath |> ignore<DirectoryInfo>

                    use _sinkResource = sink

                    sink.Write (
                        LogLevel.Information,
                        "WriteFailureLogger",
                        EventId (0),
                        ([] : KeyValuePair<string, obj> list),
                        null,
                        Func<KeyValuePair<string, obj> list, exn, string> (fun _ _ -> "will not be written")
                    )
                )

            stderr.Contains "PawPrint JSONL log sink failed to write" |> shouldEqual true
        )

    [<Test>]
    let ``Multiple sinks built from one config use distinct files in its run directory`` () : unit =
        withTempRunDirectory (fun runDirectory ->
            let config = configFor runDirectory
            let sink1 = PawPrintLogging.createSink config "same-test-name" Seq.empty
            let sink2 = PawPrintLogging.createSink config "same-test-name" Seq.empty

            use _sink1Resource = sink1
            use _sink2Resource = sink2

            sink1.FilePath |> shouldNotEqual sink2.FilePath
            File.Exists sink1.FilePath |> shouldEqual true
            File.Exists sink2.FilePath |> shouldEqual true

            Path.GetDirectoryName sink1.FilePath |> shouldEqual config.RunDirectory
            Path.GetDirectoryName sink2.FilePath |> shouldEqual config.RunDirectory
        )

    [<Test>]
    let ``Concurrent sinks sharing a config land in its run directory with distinct files`` () : unit =
        withTempRunDirectory (fun runDirectory ->
            let config = configFor runDirectory

            let sinkPaths =
                Array.Parallel.init
                    16
                    (fun _ ->
                        let sink =
                            PawPrintLogging.createSink config "parallel-test-name" Seq.empty<string * string>

                        use _sinkResource = sink
                        sink.FilePath
                    )

            sinkPaths |> Array.distinct |> Array.length |> shouldEqual sinkPaths.Length
            sinkPaths |> Array.iter (fun path -> File.Exists path |> shouldEqual true)

            for sinkPath in sinkPaths do
                Path.GetDirectoryName sinkPath |> shouldEqual config.RunDirectory
        )

    [<Test>]
    let ``Generated structured state fields round-trip through JSON`` () : unit =
        withTempRunDirectory (fun runDirectory ->
            let config = configFor runDirectory
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen genLogFields) (logFieldsRoundTrip config))
        )

    [<Test>]
    let ``LoggingConfig.fromEnv returns None when PAWPRINT_LOG_DIR is unset`` () : unit =
        // Sanity check on the single remaining env-reading boundary. We do not mutate any env
        // variable here; we only assert the "unset" branch behaves. (Tests that actually need
        // file logging go through `forRunDirectory` above, no environment required.)
        let previous = Environment.GetEnvironmentVariable "PAWPRINT_LOG_DIR"

        match previous with
        | null
        | "" -> LoggingConfig.fromEnv "test" |> shouldEqual None
        | _ ->
            // The ambient process has file logging on; don't fight it, just assert fromEnv produced
            // *something* consistent with that setting.
            match LoggingConfig.fromEnv "test" with
            | None -> failwith "PAWPRINT_LOG_DIR was set, so fromEnv should have returned Some"
            | Some config -> config.ComponentName |> shouldEqual "test"

    [<Test>]
    [<NonParallelizable>]
    let ``Console log level is quiet by default when file logging is enabled`` () : unit =
        withEnvironmentVariable
            "PAWPRINT_LOG_DIR"
            (Some "/tmp/pawprint-test-logs")
            (fun () ->
                withEnvironmentVariable
                    "PAWPRINT_LOG_LEVEL"
                    (Some "Trace")
                    (fun () ->
                        withEnvironmentVariable
                            "PAWPRINT_CONSOLE_LOG_LEVEL"
                            None
                            (fun () ->
                                LoggingConfig.consoleMinimumLevelFromEnvironment ()
                                |> shouldEqual LogLevel.Warning
                            )
                    )
            )

        withEnvironmentVariable
            "PAWPRINT_LOG_DIR"
            (Some "/tmp/pawprint-test-logs")
            (fun () ->
                withEnvironmentVariable
                    "PAWPRINT_CONSOLE_LOG_LEVEL"
                    (Some "Debug")
                    (fun () ->
                        LoggingConfig.consoleMinimumLevelFromEnvironment ()
                        |> shouldEqual LogLevel.Debug
                    )
            )

        withEnvironmentVariable
            "PAWPRINT_LOG_DIR"
            None
            (fun () ->
                withEnvironmentVariable
                    "PAWPRINT_LOG_LEVEL"
                    (Some "Debug")
                    (fun () ->
                        withEnvironmentVariable
                            "PAWPRINT_CONSOLE_LOG_LEVEL"
                            None
                            (fun () ->
                                LoggingConfig.consoleMinimumLevelFromEnvironment ()
                                |> shouldEqual LogLevel.Debug
                            )
                    )
            )
