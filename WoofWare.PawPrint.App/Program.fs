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

module AppProgram =
    let private usage =
        "Usage: WoofWare.PawPrint.App [--debug-server] <dll-path> [args...]"

    [<RequireQualifiedAccess>]
    type private AppMode =
        | RunGuest of dllPath : string * args : string list
        | DebugServer of dllPath : string * args : string list
        | InvalidArgs of message : string

    let private parseMode (argv : string list) : AppMode =
        match argv with
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

            let drainStandardStreams (state : IlMachineState) : unit =
                // The interpreter never writes to host stdout/stderr during
                // execution; instead `SystemNative_Write` appends each call
                // as an `OutputLogEntry` to `state.Kernel.OutputLog` (and
                // emits a `StepEffect.WroteToFd` for any consumer that wants
                // to stream). Here, at the end of the run, drain that log
                // to the host's real standard streams so a user invoking
                // `WoofWare.PawPrint.App` sees the guest's output in the
                // exact write order the guest produced — including across
                // stdout/stderr — so a `Write(2,…)` followed by a
                // `Write(1,…)` is replayed as `err`-then-`out`, matching
                // what a real shell sees under `2>&1`.
                //
                // Streaming during execution would couple the functional
                // core to an imperative sink; leaving it until the end keeps
                // the interpreter deterministic and replayable. Programs
                // that crash partway will lose nothing because the log is
                // what's drained regardless of which `RunOutcome`
                // terminates the run.
                let log = state.Kernel.OutputLog

                if log.Length > 0 then
                    use out = System.Console.OpenStandardOutput ()
                    use err = System.Console.OpenStandardError ()

                    for entry in log do
                        match entry.Role with
                        | FileDescriptorRole.StandardOutput -> out.Write (entry.Bytes.AsSpan ())
                        | FileDescriptorRole.StandardError -> err.Write (entry.Bytes.AsSpan ())
                        | FileDescriptorRole.StandardInput ->
                            // Unreachable: `SystemNative_Write` rejects stdin
                            // with EBADF before appending. Exhaustiveness is
                            // load-bearing here — a future writable role
                            // (e.g. a regular file) will fail to compile
                            // until its drain destination is decided.
                            failwith
                                "drainStandardStreams: OutputLog contains StandardInput entry (this is an interpreter bug)"

                    out.Flush ()
                    err.Flush ()

            match Program.run loggerFactory (Some dllPath) fileStream dotnetRuntimes impls args with
            | RunOutcome.NormalExit (state, thread)
            | RunOutcome.ProcessExit (state, thread) ->
                drainStandardStreams state
                exitCodeFromStack state thread
            | RunOutcome.FailFast (state, _thread, message) ->
                // CoreCLR's Environment_FailFast calls HandleFatalError(COR_E_FAILFAST)
                // and on Windows terminates with 0x80131623; on Unix it aborts via
                // SIGABRT (exit code 128 + 6 = 134).
                drainStandardStreams state
                let msg = message |> Option.defaultValue "<no message>"
                logger.LogCritical ("Guest called Environment.FailFast: {FailFastMessage}", msg)

                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    -2146232797
                else
                    134
            | RunOutcome.GuestUnhandledException (state, _thread, exn) ->
                drainStandardStreams state

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
