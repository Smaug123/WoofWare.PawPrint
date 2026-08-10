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

            // Host-side sink for guest writes. `SystemNative_Write` appends each guest
            // write to `EmulatedKernel.OutputLog` *and* reports it as a
            // `StepEffect.WroteToFd`; we consume the effect so output reaches the host
            // as the guest produces it.
            //
            // Streaming rather than draining the log once at the end is what makes
            // output survive a run that never yields a `RunOutcome`: a livelocked guest,
            // one killed from outside, or one this interpreter reports as `Deadlocked`.
            // Draining only on a `RunOutcome` silently discarded everything such a guest
            // had printed, which is precisely when the output is most wanted. That holds
            // for everything the stepping loop below drives — see the `prepare` call site
            // for the startup-phase writes it does not yet cover.
            //
            // It costs no determinism: the interpreter is untouched and still never
            // performs I/O, the effect is a value it hands back, and this shell decides
            // what to do with it.
            use hostOut = System.Console.OpenStandardOutput ()
            use hostErr = System.Console.OpenStandardError ()

            // Count of `OutputLog` entries already written to the host. The invariant is
            // that entries `[0, written)` have reached a real stream; `writeEntry`
            // advances it and `drainRemaining` closes any gap. One `WroteToFd` effect is
            // exactly one log entry, so consuming effects keeps the two in step.
            let mutable written = 0

            let writeEntry (role : FileDescriptorRole) (bytes : ImmutableArray<byte>) : unit =
                let destination =
                    match role with
                    | FileDescriptorRole.StandardOutput -> hostOut
                    | FileDescriptorRole.StandardError -> hostErr
                    | FileDescriptorRole.StandardInput ->
                        // Unreachable: `SystemNative_Write` rejects stdin with EBADF before
                        // appending. Exhaustiveness is load-bearing here — a future writable
                        // role (e.g. a regular file) will fail to compile until its
                        // destination is decided.
                        failwith "guest OutputLog contains a StandardInput entry (this is an interpreter bug)"

                destination.Write (bytes.AsSpan ())

                // Flush per guest write rather than at end of run. Without this the whole
                // point is lost: a guest that prints and then hangs leaves its output in the
                // host's stream buffer, and a `kill` discards it. Guest writes arrive at
                // whatever granularity the guest's own `TextWriter` flushes (a line at a time
                // for `Console.WriteLine`), not per byte, so this is cheap relative to
                // interpreting the IL that produced them.
                //
                // The two streams are written in guest write order, so a `Write(2,…)`
                // followed by a `Write(1,…)` reaches the host as `err`-then-`out`, which is
                // what a real shell sees under `2>&1`.
                destination.Flush ()

                written <- written + 1

            // Write any log entries that never passed through `writeEntry` — any path that
            // appends to the log without surfacing a `StepEffect`. Normally a no-op, since
            // both startup and `Main` are stepped and every guest write reports an effect.
            let drainRemaining (state : IlMachineState) : unit =
                let log = state.Kernel.OutputLog

                // The range is evaluated once, from `written` as it stands on entry, and
                // `writeEntry` advances `written` by exactly one per entry — so this writes
                // precisely the entries it indexes and leaves the invariant intact.
                for i in written .. log.Length - 1 do
                    writeEntry log.[i].Role log.[i].Bytes

            let onOutcome (outcome : RunOutcome) : int =
                match outcome with
                | RunOutcome.NormalExit (state, thread)
                | RunOutcome.ProcessExit (state, thread) ->
                    drainRemaining state
                    exitCodeFromStack state thread
                | RunOutcome.FailFast (state, _thread, message) ->
                    // CoreCLR's Environment_FailFast calls HandleFatalError(COR_E_FAILFAST)
                    // and on Windows terminates with 0x80131623; on Unix it aborts via
                    // SIGABRT (exit code 128 + 6 = 134).
                    drainRemaining state
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
                    drainRemaining state
                    let signo = Signal.toLinuxSigno signal

                    logger.LogInformation (
                        "Guest terminated by POSIX signal {SignalName} (signo {Signo}); exiting with code {ExitCode}",
                        sprintf "%O" signal,
                        signo,
                        128 + signo
                    )

                    128 + signo
                | RunOutcome.GuestUnhandledException (state, _thread, exn) ->
                    drainRemaining state

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

            // The stepping loop is driven here rather than by calling `Program.run`,
            // because `Program.run` hands back only a terminal `RunOutcome` and the
            // per-step `StepEffect`s are what carry guest output. Consuming them here is
            // the whole point: output reaches the host while the guest is still running.
            //
            // `Program.pumpPrepared` (which `Program.run` uses) additionally raises on
            // `Deadlocked` rather than returning, so a deadlocked guest never reached the
            // end-of-run drain at all and lost everything it had printed.
            let logger = loggerFactory.CreateLogger "Program"

            // Every thread is blocked and the run has not finished, so no further step is
            // possible. Report it as a diagnostic rather than letting it escape as an unhandled
            // host exception: the guest's own output has already been streamed, and a stack
            // trace through the interpreter says nothing about why the *guest* is stuck.
            let reportDeadlock (during : string) (state : IlMachineState) (stuck : string) : int =
                drainRemaining state

                logger.LogCritical (
                    "Guest deadlocked during {Phase}: no runnable threads, and it has not finished. Stuck: {StuckThreads}",
                    during,
                    stuck
                )

                // Same code the escaping host exception produced, so shell callers observe no
                // change. That is per-platform: an unhandled .NET exception terminates with
                // 0xE0434352 on Windows and SIGABRT (128 + 6) on Unix, which is the same split
                // the guest-unhandled-exception arm above uses.
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    -532462766
                else
                    134

            let consume (effect : StepEffect) : unit =
                match effect with
                | StepEffect.WroteToFd (role, bytes) -> writeEntry role bytes
                | StepEffect.NoEffect -> ()

            let rec pump (prepared : Program.PreparedProgram) : int =
                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.InstructionStepped (prepared, _ranThread, _whatWeDid, effect) ->
                    consume effect
                    pump prepared
                | Program.ProgramStepOutcome.WorkerTerminated (prepared, _terminatingThread) -> pump prepared
                | Program.ProgramStepOutcome.Completed outcome -> onOutcome outcome
                | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) ->
                    reportDeadlock "execution" prepared.State stuck

            // Startup runs guest code too — the AppContext seed, then class initialisers — so
            // it is stepped for exactly the same reason `Main` is: a static initialiser that
            // prints and then wedges must still deliver what it printed, and must be reported
            // rather than escaping as a host exception.
            let rec pumpStartup (startup : Program.Startup) : int =
                match Program.stepStartup loggerFactory logger startup with
                | Program.StartupStepOutcome.Stepped (startup, _ranThread, _whatWeDid, effect) ->
                    consume effect
                    pumpStartup startup
                | Program.StartupStepOutcome.WorkerTerminated (startup, _terminatingThread) -> pumpStartup startup
                | Program.StartupStepOutcome.PhaseAdvanced startup -> pumpStartup startup
                | Program.StartupStepOutcome.Completed (Program.ProgramStartResult.CompletedBeforeMain outcome) ->
                    onOutcome outcome
                | Program.StartupStepOutcome.Completed (Program.ProgramStartResult.Ready prepared) ->
                    // Startup's own bookkeeping (installing the `Main` frame, allocating argv)
                    // performs no guest writes, so this is normally a no-op; it keeps the
                    // "everything in the log has reached a stream" invariant true regardless.
                    drainRemaining prepared.State
                    pump prepared
                | Program.StartupStepOutcome.Deadlocked (startup, stuck) -> reportDeadlock "startup" startup.State stuck

            Program.beginStartup
                loggerFactory
                (Some dllPath)
                fileStream
                { HostConfig.Default dotnetRuntimes with
                    Kernel = kernelConfig
                    PctSeed = pctSeed
                    Argv = args
                    AppContext = HostRuntimeConfig.forAssembly dllPath
                }
            |> pumpStartup

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
