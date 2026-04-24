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
[<NonParallelizable>]
module TestJsonLinesLogging =
    // These tests intentionally mutate PAWPRINT_LOG_* process environment variables and Console.Error.
    // The NonParallelizable attribute is load-bearing.
    type private GeneratedLogValue =
        | StringValue of string
        | BoolValue of bool
        | IntValue of int
        | NullValue

    let private withEnvironment (values : (string * string option) list) (action : unit -> 'result) : 'result =
        let previous =
            values
            |> List.map (fun (name, _) -> name, Environment.GetEnvironmentVariable name)

        try
            for name, value in values do
                Environment.SetEnvironmentVariable (name, Option.toObj value)

            action ()
        finally
            for name, value in previous do
                Environment.SetEnvironmentVariable (name, value)

    let private withCapturedConsoleError (action : unit -> 'result) : 'result * string =
        let original = Console.Error
        use writer = new StringWriter ()

        try
            Console.SetError writer
            let result = action ()
            result, writer.ToString ()
        finally
            Console.SetError original

    let private withTempRoot (action : string -> 'result) : 'result =
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

    let private withJsonLoggingEnvironment (root : string) (action : unit -> 'result) : 'result =
        withEnvironment
            [
                "PAWPRINT_LOG_DIR", Some root
                "PAWPRINT_LOG_LEVEL", Some "Debug"
                "PAWPRINT_LOG_RUN_ID", Some "user-supplied-run"
            ]
            action

    let private getOnlyLogFile (root : string) : string =
        let runDirectories = Directory.GetDirectories root
        runDirectories.Length |> shouldEqual 1

        let files = Directory.GetFiles (runDirectories.[0], "*.jsonl")
        files.Length |> shouldEqual 1
        files.[0]

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

    let private logFieldsRoundTrip (root : string) (fields : (string * GeneratedLogValue) list) : unit =
        withJsonLoggingEnvironment
            root
            (fun () ->
                let sink =
                    PawPrintLogging.tryCreateSinkFromEnvironment "test" "round-trip-test" Seq.empty
                    |> Option.defaultWith (fun () -> failwith "Expected sink")

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
            )

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 100

    [<Test>]
    let ``JSONL sink preserves structured log fields`` () : unit =
        withTempRoot (fun root ->
            withJsonLoggingEnvironment
                root
                (fun () ->
                    let provider =
                        PawPrintLogging.tryCreateProviderFromEnvironment
                            "test"
                            "structured-field-test"
                            [ "source_file", "LoggingCase.cs" ]
                        |> Option.defaultWith (fun () -> failwith "Expected PAWPRINT_LOG_DIR to create a provider")

                    use _providerResource = provider
                    let logger = provider.CreateLogger "StructuredLogger"
                    logger.LogInformation ("Saw value {Value}", 42)
                )

            let file = getOnlyLogFile root
            let lines = File.ReadAllLines file
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
        withTempRoot (fun root ->
            withJsonLoggingEnvironment
                root
                (fun () ->
                    let sink =
                        PawPrintLogging.tryCreateSinkFromEnvironment
                            "test"
                            "collision-test"
                            [ "level", "caller-level" ; "message", "caller-message" ]
                        |> Option.defaultWith (fun () -> failwith "Expected sink")

                    use _sinkResource = sink

                    sink.Write (
                        LogLevel.Information,
                        "CollisionLogger",
                        EventId (0),
                        ([] : KeyValuePair<string, obj> list),
                        null,
                        Func<KeyValuePair<string, obj> list, exn, string> (fun _ _ -> "real message")
                    )
                )

            let file = getOnlyLogFile root
            let lines = File.ReadAllLines file
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

    [<Test>]
    let ``Write failures do not escape ILogger callers`` () : unit =
        withTempRoot (fun root ->
            let _, stderr =
                withCapturedConsoleError (fun () ->
                    withJsonLoggingEnvironment
                        root
                        (fun () ->
                            let sink =
                                PawPrintLogging.tryCreateSinkFromEnvironment "test" "write-failure-test" Seq.empty
                                |> Option.defaultWith (fun () -> failwith "Expected sink")

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
                )

            stderr.Contains "PawPrint JSONL log sink failed to write" |> shouldEqual true
        )

    [<Test>]
    let ``Multiple sinks under one root use distinct files`` () : unit =
        withTempRoot (fun root ->
            let sinkPaths =
                withJsonLoggingEnvironment
                    root
                    (fun () ->
                        let sink1 =
                            PawPrintLogging.tryCreateSinkFromEnvironment "test" "same-test-name" Seq.empty
                            |> Option.defaultWith (fun () -> failwith "Expected first sink")

                        let sink2 =
                            PawPrintLogging.tryCreateSinkFromEnvironment "test" "same-test-name" Seq.empty
                            |> Option.defaultWith (fun () -> failwith "Expected second sink")

                        use _sink1Resource = sink1
                        use _sink2Resource = sink2
                        sink1.FilePath, sink2.FilePath
                    )

            let sink1Path, sink2Path = sinkPaths
            sink1Path |> shouldNotEqual sink2Path
            File.Exists sink1Path |> shouldEqual true
            File.Exists sink2Path |> shouldEqual true

            let runDirectories = Directory.GetDirectories root
            runDirectories.Length |> shouldEqual 1
            Path.GetDirectoryName sink1Path |> shouldEqual runDirectories.[0]
            Path.GetDirectoryName sink2Path |> shouldEqual runDirectories.[0]
        )

    [<Test>]
    let ``Concurrent sinks under one root share one run directory and distinct files`` () : unit =
        withTempRoot (fun root ->
            let sinkPaths =
                withJsonLoggingEnvironment
                    root
                    (fun () ->
                        Array.Parallel.init
                            16
                            (fun _ ->
                                let sink =
                                    PawPrintLogging.tryCreateSinkFromEnvironment
                                        "test"
                                        "parallel-test-name"
                                        Seq.empty<string * string>
                                    |> Option.defaultWith (fun () -> failwith "Expected sink")

                                use _sinkResource = sink
                                sink.FilePath
                            )
                    )

            sinkPaths |> Array.distinct |> Array.length |> shouldEqual sinkPaths.Length
            sinkPaths |> Array.iter (fun path -> File.Exists path |> shouldEqual true)

            let runDirectories = Directory.GetDirectories root
            runDirectories.Length |> shouldEqual 1

            for sinkPath in sinkPaths do
                Path.GetDirectoryName sinkPath |> shouldEqual runDirectories.[0]
        )

    [<Test>]
    let ``Generated structured state fields round-trip through JSON`` () : unit =
        withTempRoot (fun root ->
            Check.One (propertyConfig, Prop.forAll (Arb.fromGen genLogFields) (logFieldsRoundTrip root))
        )
