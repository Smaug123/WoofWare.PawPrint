namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open WoofWare.DotnetRuntimeLocator

module Program =
    let reallyMain (argv : string[]) : int =
        let loggerFactory =
            LoggerFactory.Create (fun builder ->
                builder
                    .SetMinimumLevel(LogLevel.Information)
                    .AddConsole (fun options -> options.LogToStandardErrorThreshold <- LogLevel.Trace)
                |> ignore<ILoggingBuilder>
            )

        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App"

        match argv |> Array.toList with
        | dllPath :: args ->
            let dotnetRuntimes =
                DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

            let terminalState = Program.run loggerFactory fileStream dotnetRuntimes args

            0
        | _ ->
            logger.LogCritical "Supply exactly one DLL path"
            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
