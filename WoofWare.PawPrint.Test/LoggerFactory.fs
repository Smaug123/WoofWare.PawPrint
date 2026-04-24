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

    /// Returns a pair: `(getLogs, loggerFactory)` where `getLogs ()` retrieves all
    /// log messages emitted so far, in chronological order.
    let makeTestWithProperties (staticProperties : (string * string) list) : (unit -> LogLine list) * ILoggerFactory =
        // Shared sink for all loggers created by the factory.
        let sink = ResizeArray ()

        let staticProperties = currentTestProperties () @ staticProperties

        let fileSink =
            PawPrintLogging.tryCreateSinkFromEnvironment "test" (fileNameStem staticProperties) staticProperties

        let minimumLevel = PawPrintLogging.minimumLevelFromEnvironment ()

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

    /// Returns a pair: `(getLogs, loggerFactory)` where `getLogs ()` retrieves all
    /// log messages emitted so far, in chronological order.
    let makeTest () : (unit -> LogLine list) * ILoggerFactory = makeTestWithProperties []
