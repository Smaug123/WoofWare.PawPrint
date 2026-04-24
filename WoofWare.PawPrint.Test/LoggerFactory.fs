namespace WoofWare.PawPrint.Test

open System
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint.Logging

type LogLine =
    {
        Level : LogLevel
        LoggerName : string
        Message : string
    }

    /// Human-readable representation of this log line.
    override this.ToString () =
        $"%s{this.LoggerName} [%O{this.Level}]: %s{this.Message}"

/// Very small, in-memory implementation of `ILoggerFactory` for unit tests.
[<RequireQualifiedAccess>]
module LoggerFactory =
    let private currentTestProperties () : (string * string) list =
        try
            let test = TestContext.CurrentContext.Test

            [
                if not (String.IsNullOrWhiteSpace test.FullName) then
                    "test_name", test.FullName

                if not (String.IsNullOrWhiteSpace test.ID) then
                    "test_id", test.ID
            ]
        with _ ->
            []

    let private fileNameStem (staticProperties : (string * string) list) : string =
        staticProperties
        |> List.tryPick (fun (key, value) ->
            if key = "source_file" || key = "test_name" then
                Some value
            else
                None
        )
        |> Option.defaultValue "pawprint-test"

    // End-to-end tests share one run directory for the test process — built lazily on first
    // demand and reused, so all `makeTestWithProperties` callers in a single test run drop
    // their `.jsonl` files into the same timestamped directory.
    let private endToEndProcessConfig : Lazy<LoggingConfig option> =
        lazy (LoggingConfig.fromEnv "test")

    /// Core factory. `fileConfig = Some cfg` mirrors events into a JSONL file under `cfg`'s run
    /// directory; `None` disables file logging entirely (events are still captured in the
    /// in-memory `LogLine` buffer the caller gets back).
    let private makeWithConfig
        (fileConfig : LoggingConfig option)
        (staticProperties : (string * string) list)
        : (unit -> LogLine list) * ILoggerFactory
        =
        let sink = ResizeArray ()
        let staticProperties = currentTestProperties () @ staticProperties

        let fileSink =
            fileConfig
            |> Option.map (fun config ->
                PawPrintLogging.createSink config (fileNameStem staticProperties) staticProperties
            )

        let minimumLevel =
            match fileConfig with
            | Some config -> config.MinimumLevel
            | None -> LogLevel.Information

        let isEnabled (logLevel : LogLevel) : bool =
            logLevel <> LogLevel.None && logLevel >= minimumLevel

        let createLogger (category : string) : ILogger =
            { new ILogger with
                member _.BeginScope _state =
                    { new IDisposable with
                        member _.Dispose () = ()
                    }

                member _.IsEnabled l = isEnabled l

                member _.Log (logLevel, eventId, state, ex, formatter) =
                    if not (isEnabled logLevel) then
                        ()
                    else
                        match fileSink with
                        | Some fileSink -> fileSink.Write (logLevel, category, eventId, state, ex, formatter)
                        | None -> ()

                        let message =
                            try
                                formatter.Invoke (state, ex)
                            with _ ->
                                "<formatter threw>"

                        lock
                            sink
                            (fun () ->
                                {
                                    Level = logLevel
                                    LoggerName = category
                                    Message = message
                                }
                                |> sink.Add
                            )
            }

        // Minimal `ILoggerFactory` that just hands out instances of the above.
        let factory =
            { new ILoggerFactory with
                member _.CreateLogger categoryName = createLogger categoryName

                member _.AddProvider _provider = ()

                member _.Dispose () =
                    match fileSink with
                    | Some fileSink -> (fileSink :> IDisposable).Dispose ()
                    | None -> ()
            }

        // Expose accessor that snapshots the current sink contents.
        let getLogs () = lock sink (fun () -> List.ofSeq sink)

        getLogs, factory

    /// In-memory test factory with no file logging. Appropriate for unit tests that only need
    /// to assert on captured log lines; these tests deliberately do not respect
    /// `PAWPRINT_LOG_DIR` so they can run in parallel without racing on file-system state.
    let makeTest () : (unit -> LogLine list) * ILoggerFactory = makeWithConfig None []

    /// Test factory that mirrors events into a JSONL file when `PAWPRINT_LOG_DIR` is set in
    /// the process environment. Intended for end-to-end tests whose traces we want to keep.
    let makeTestWithProperties (staticProperties : (string * string) list) : (unit -> LogLine list) * ILoggerFactory =
        makeWithConfig endToEndProcessConfig.Value staticProperties
