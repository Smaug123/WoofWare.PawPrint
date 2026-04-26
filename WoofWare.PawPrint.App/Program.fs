namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint.Logging
open WoofWare.PawPrint.ExternImplementations

module Program =
    let private usage =
        "Usage: WoofWare.PawPrint.App [--debug-server] <dll-path> [args...]"

    [<RequireQualifiedAccess>]
    type private AppMode =
        | RunGuest of dllPath : string * args : string list
        | DebugServer of dllPath : string * args : string list
        | InvalidArgs of message : string

    let private parseMode (argv : string list) : AppMode =
        match argv with
        | "--debug-server" :: "--listen" :: _ ->
            AppMode.InvalidArgs "--debug-server always binds 127.0.0.1 on an ephemeral port; --listen is not supported"
        | "--debug-server" :: dllPath :: args -> AppMode.DebugServer (dllPath, args)
        | "--debug-server" :: [] -> AppMode.InvalidArgs "--debug-server requires a DLL path"
        | dllPath :: args -> AppMode.RunGuest (dllPath, args)
        | [] -> AppMode.InvalidArgs "Supply a DLL path"

    let private dllPathFromMode (mode : AppMode) : string option =
        match mode with
        | AppMode.RunGuest (dllPath, _)
        | AppMode.DebugServer (dllPath, _) -> Some dllPath
        | AppMode.InvalidArgs _ -> None

    let reallyMain (argv : string[]) : int =
        let mode = argv |> Array.toList |> parseMode

        let appStaticProperties =
            match dllPathFromMode mode with
            | Some dllPath -> [ "guest_dll", Path.GetFullPath dllPath ]
            | None -> []

        let loggingConfig = LoggingConfig.fromEnv "app"
        let consoleMinimumLevel = LoggingConfig.consoleMinimumLevelFromEnvironment ()

        let globalMinimumLevel =
            match loggingConfig with
            | None -> consoleMinimumLevel
            | Some config ->
                if int consoleMinimumLevel < int config.MinimumLevel then
                    consoleMinimumLevel
                else
                    config.MinimumLevel

        use loggerFactory =
            LoggerFactory.Create (fun builder ->
                builder.SetMinimumLevel (globalMinimumLevel) |> ignore<ILoggingBuilder>

                builder.AddFilter<ConsoleLoggerProvider> (
                    Func<LogLevel, bool> (fun logLevel -> logLevel >= consoleMinimumLevel)
                )
                |> ignore<ILoggingBuilder>

                builder.AddConsole (fun options -> options.LogToStandardErrorThreshold <- LogLevel.Trace)
                |> ignore<ILoggingBuilder>

                match loggingConfig with
                | Some config ->
                    builder.AddProvider (PawPrintLogging.createProvider config "pawprint-app" appStaticProperties)
                    |> ignore<ILoggingBuilder>
                | None -> ()
            )

        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App"

        let runNormal (dllPath : string) (args : string list) : int =
            let dotnetRuntimes =
                DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

            let impls = NativeImpls.PassThru ()

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

            let exitCodeFromStack (state : IlMachineState) (thread : ThreadId) : int =
                // Main returned an int32, or Environment.Exit(n) pushed the code on the
                // caller's eval stack before terminating; either way the top-of-stack
                // value is the guest's requested process exit code. If anything else is
                // there (or the stack is empty) we fail loud rather than silently
                // reporting 0 — shell callers depend on the exit code being meaningful.
                match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                | EvalStackValue.Int32 i :: _ -> i
                | [] -> failwith "Exiting thread returned void; expected an int32 exit code"
                | other :: _ -> failwith $"Exiting thread had unexpected eval-stack top %O{other}; expected int32"

            match Program.run loggerFactory (Some dllPath) fileStream dotnetRuntimes impls args with
            | RunOutcome.NormalExit (state, thread)
            | RunOutcome.ProcessExit (state, thread) -> exitCodeFromStack state thread
            | RunOutcome.GuestUnhandledException (state, _thread, exn) ->
                let exceptionTypeName =
                    match state.ManagedHeap.NonArrayObjects |> Map.tryFind exn.ExceptionObject with
                    | Some obj ->
                        match AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes with
                        | Some ti -> $"{ti.Namespace}.{ti.Name}"
                        | None -> $"<unknown type %O{obj.ConcreteType}>"
                    | None -> $"<heap address %O{exn.ExceptionObject}>"

                logger.LogCritical ("Unhandled exception in guest program: {ExceptionTypeName}", exceptionTypeName)

                // On Windows the .NET runtime exits with 0xE0434352 (SEH);
                // on Unix it aborts with SIGABRT (exit code 128 + 6 = 134).
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    -532462766
                else
                    134

        let runDebugger (dllPath : string) (args : string list) : int =
            let dotnetRuntimes =
                DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

            let impls = NativeImpls.PassThru ()

            DebuggerServer.run loggerFactory dllPath dotnetRuntimes impls args

        match mode with
        | AppMode.RunGuest (dllPath, args) -> runNormal dllPath args
        | AppMode.DebugServer (dllPath, args) -> runDebugger dllPath args
        | AppMode.InvalidArgs message ->
            logger.LogCritical ("{Message}\n{Usage}", message, usage)

            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
