namespace WoofWare.PawPrint.Test

open System
open Microsoft.Extensions.Logging

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

    /// Returns a pair: `(getLogs, loggerFactory)` where `getLogs ()` retrieves all
    /// log messages emitted so far, in chronological order.
    let makeTest () : (unit -> LogLine list) * ILoggerFactory =
        // Shared sink for all loggers created by the factory.
        let sink = ResizeArray ()

        let createLogger (category : string) : ILogger =
            { new ILogger with
                member _.BeginScope _state =
                    { new IDisposable with
                        member _.Dispose () = ()
                    }

                member _.IsEnabled logLevel = logLevel > LogLevel.Trace

                member _.Log (logLevel, eventId, state, ex, formatter) =
                    if logLevel <= LogLevel.Trace then
                        ()
                    else

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

                member _.Dispose () = ()
            }

        // Expose accessor that snapshots the current sink contents.
        let getLogs () = lock sink (fun () -> List.ofSeq sink)

        getLogs, factory
