namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.Logging
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint.ExternImplementations

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

            let impls = NativeImpls.PassThru ()

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

            match Program.run loggerFactory (Some dllPath) fileStream dotnetRuntimes impls args with
            | RunOutcome.NormalExit _
            | RunOutcome.ProcessExit _ -> 0
            | RunOutcome.GuestUnhandledException (state, _thread, exn) ->
                let exceptionTypeName =
                    match state.ManagedHeap.NonArrayObjects |> Map.tryFind exn.ExceptionObject with
                    | Some obj ->
                        match AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes with
                        | Some ti -> $"{ti.Namespace}.{ti.Name}"
                        | None -> $"<unknown type %O{obj.ConcreteType}>"
                    | None -> $"<heap address %O{exn.ExceptionObject}>"

                logger.LogCritical $"Unhandled exception in guest program: {exceptionTypeName}"

                // On Windows the .NET runtime exits with 0xE0434352 (SEH);
                // on Unix it aborts with SIGABRT (exit code 128 + 6 = 134).
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    -532462766
                else
                    134
        | _ ->
            logger.LogCritical "Supply exactly one DLL path"
            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
