namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.Json
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint.Logging

[<TestFixture>]
[<NonParallelizable>]
module TestJsonLinesLogging =
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

    [<Test>]
    let ``JSONL sink preserves structured log fields`` () : unit =
        let guid = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-logging-test-%s{guid}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>

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

        rootElement.GetProperty("source_file").GetString ()
        |> shouldEqual "LoggingCase.cs"

        rootElement.GetProperty("user_run_id").GetString ()
        |> shouldEqual "user-supplied-run"

        rootElement.GetProperty("logger").GetString () |> shouldEqual "StructuredLogger"

        rootElement.GetProperty("message_template").GetString ()
        |> shouldEqual "Saw value {Value}"

        rootElement.GetProperty("fields").GetProperty("Value").GetInt32 ()
        |> shouldEqual 42

    [<Test>]
    let ``Multiple sinks under one root use distinct files`` () : unit =
        let guid = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-logging-test-%s{guid}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>

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

    [<Test>]
    let ``Concurrent sinks under one root share one run directory and distinct files`` () : unit =
        let guid = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-logging-test-%s{guid}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>

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
