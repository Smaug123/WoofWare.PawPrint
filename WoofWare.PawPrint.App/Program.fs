namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint.Logging

module AppProgram =
    let private usage =
        "Usage: WoofWare.PawPrint.App [--debug-server] [--seed <decimal-or-0xHEX>] <dll-path> [args...]"

    [<RequireQualifiedAccess>]
    type private AppMode =
        | RunGuest of dllPath : string * pctSeed : uint64 option * args : string list
        | DebugServer of dllPath : string * pctSeed : uint64 option * args : string list
        | InvalidArgs of message : string

    /// Parse a PCT seed in decimal (`12345`) or hex (`0xDEADBEEF`/`0XDEADBEEF`).
    /// Hex is supported because writing 16 raw hex digits is the natural shape
    /// for a 64-bit seed, and the CLI is a place where humans want to copy/paste
    /// reproduction seeds from log output. Returns the parsed value or an error
    /// message; failure cases are reported via `AppMode.InvalidArgs` so the
    /// existing usage-print path handles them uniformly.
    let private parseSeed (s : string) : Result<uint64, string> =
        let trimmed = s.Trim ()

        if trimmed.StartsWith ("0x", System.StringComparison.OrdinalIgnoreCase) then
            let digits = trimmed.Substring 2

            match
                System.UInt64.TryParse (
                    digits,
                    System.Globalization.NumberStyles.HexNumber,
                    System.Globalization.CultureInfo.InvariantCulture
                )
            with
            | true, v -> Result.Ok v
            | false, _ -> Result.Error $"--seed: '%s{s}' is not a valid 64-bit hex literal"
        else
            match
                System.UInt64.TryParse (
                    trimmed,
                    System.Globalization.NumberStyles.Integer,
                    System.Globalization.CultureInfo.InvariantCulture
                )
            with
            | true, v -> Result.Ok v
            | false, _ ->
                Result.Error $"--seed: '%s{s}' is not a valid unsigned 64-bit decimal or 0x-prefixed hex literal"

    let private parseMode (argv : string list) : AppMode =
        // Two flags (`--debug-server`, `--seed N`) are accepted in either order
        // before the DLL path. Implemented as a fold rather than a fixed-order
        // pattern match so callers don't have to memorise the prefix order, and
        // so a future third flag stays additive instead of multiplying cases.
        let rec go (debugServer : bool) (seed : uint64 option) (rest : string list) : AppMode =
            match rest with
            | "--debug-server" :: tail -> go true seed tail
            | "--seed" :: value :: tail ->
                match parseSeed value with
                | Result.Ok v -> go debugServer (Some v) tail
                | Result.Error msg -> AppMode.InvalidArgs msg
            | "--seed" :: [] -> AppMode.InvalidArgs "--seed requires a value (decimal or 0xHEX)"
            | dllPath :: args ->
                if debugServer then
                    AppMode.DebugServer (dllPath, seed, args)
                else
                    AppMode.RunGuest (dllPath, seed, args)
            | [] ->
                if debugServer then
                    AppMode.InvalidArgs "--debug-server requires a DLL path"
                else
                    AppMode.InvalidArgs "Supply a DLL path"

        go false None argv

    let private dllPathFromMode (mode : AppMode) : string option =
        match mode with
        | AppMode.RunGuest (dllPath, _, _)
        | AppMode.DebugServer (dllPath, _, _) -> Some dllPath
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

        // Snapshot the host process's environment once at startup. Layered on top
        // of `EmulatedKernel.defaultEnvironment`, so any host-set value wins over
        // the seeded `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` default, while
        // unset keys still get the default. This is the production analogue of
        // tests passing an explicit env map to `Program.run`.
        //
        // Environment variables are the *only* thing the CLI takes from the host:
        // the rest of `KernelConfig` keeps its defaults. In particular the guest's
        // `Environment.ProcessorCount` stays at the deterministic default rather
        // than reporting this machine's core count, and its `DateTime.UtcNow`
        // starts at the Unix epoch rather than at this machine's clock, so a run
        // recorded here replays identically elsewhere — and at the same *times*.
        // Env vars are a deliberate exception because the
        // guest's whole reason to run under the CLI is to see the invoker's
        // configuration — and unlike the core count they are visible in, and
        // reproducible from, the recorded kernel state.
        let kernelConfig : KernelConfig =
            let dict = System.Environment.GetEnvironmentVariables ()

            let mutable acc = Map.empty

            for entry in dict do
                let entry = entry :?> System.Collections.DictionaryEntry
                acc <- Map.add (entry.Key :?> string) (entry.Value :?> string) acc

            { KernelConfig.Default with
                Environment = acc
            }

        let runNormal (dllPath : string) (pctSeed : uint64 option) (args : string list) : int =
            // Echo the active seed to stderr (so it doesn't pollute the
            // guest's stdout) before any guest output. Hex is the canonical
            // form for copy/paste reproduction even when the seed was given
            // in decimal — paste-back via `--seed 0x...` always parses.
            match pctSeed with
            | Some seed -> eprintfn "PCT seed: 0x%016X" seed
            | None -> ()

            let dotnetRuntimes =
                DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

            let exitCodeFromStack (state : IlMachineState) (thread : ThreadId) : int =
                // Main returned an int32, or Environment.Exit(n) pushed the code on the
                // caller's eval stack before terminating; either way the top-of-stack
                // value is the guest's requested process exit code. If anything else is
                // there (or the stack is empty) we fail loud rather than silently
                // reporting 0 — shell callers depend on the exit code being meaningful.
                match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
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

            match
                Program.run
                    loggerFactory
                    (Some dllPath)
                    fileStream
                    { HostConfig.Default dotnetRuntimes with
                        Kernel = kernelConfig
                        PctSeed = pctSeed
                        Argv = args
                    }
            with
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
            | RunOutcome.SignalTerminated (state, signal) ->
                // pal_signal.c's Terminate branch restores the original
                // sigaction and calls `kill(g_pid, signalCode)`, so the
                // host shell observes the process as having exited with
                // the POSIX-conventional code `128 + signo`. Mirror that
                // here so guests that install a signal handler and
                // forward to the default disposition observe the same
                // shell-level exit code as a real .NET process.
                drainStandardStreams state
                let signo = Signal.toLinuxSigno signal

                logger.LogInformation (
                    "Guest terminated by POSIX signal {SignalName} (signo {Signo}); exiting with code {ExitCode}",
                    sprintf "%O" signal,
                    signo,
                    128 + signo
                )

                128 + signo
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

        let runDebugger (dllPath : string) (pctSeed : uint64 option) (args : string list) : int =
            let dotnetRuntimes =
                DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

            match pctSeed with
            | Some seed -> eprintfn "PCT seed: 0x%016X" seed
            | None -> ()

            DebuggerServer.run loggerFactory dllPath dotnetRuntimes kernelConfig pctSeed args

        match mode with
        | AppMode.RunGuest (dllPath, pctSeed, args) -> runNormal dllPath pctSeed args
        | AppMode.DebugServer (dllPath, pctSeed, args) -> runDebugger dllPath pctSeed args
        | AppMode.InvalidArgs message ->
            logger.LogCritical ("{Message}\n{Usage}", message, usage)

            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
