namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestImpureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented : EndToEndTestCase list = []

    let cases : EndToEndTestCase list =
        [
            {
                // `Console.WriteLine("Hello, world!")` exercises the full
                // BCL stdio stack end-to-end: `Console::get_Out` descends
                // `ConsolePal::OpenStandardOutput → Interop.Sys.Dup`, then
                // the `StreamWriter` flush descends `Interop.Sys.Write`.
                // Both shims are intercepted by PawPrint's
                // FileDescriptorRegistry / EmulatedKernel. We assert on
                // the bytes the guest actually appended to the stdout
                // log, not just the exit code — a regression in the
                // encoder, the StreamWriter buffer, or the SystemNative
                // pointer decode would not change the exit code (the
                // `return 1;` runs unconditionally) but would corrupt
                // these bytes.
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState =
                    Some (fun state ->
                        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                        |> Seq.toArray
                        |> shouldEqual (System.Text.Encoding.UTF8.GetBytes "Hello, world!\n")

                        OutputLogEntry.bytesFor FileDescriptorRole.StandardError state.Kernel.OutputLog
                        |> Seq.length
                        |> shouldEqual 0
                    )
            }
            {
                // A host-configured `KernelConfig.ProcessorCount` must actually
                // reach the guest, and must do so before the entry type's
                // `.cctor` runs — CoreLib latches `Environment.ProcessorCount`
                // into a static on first read, so applying the configuration any
                // later than `Program.prepare` does would leave a guest that
                // reads it during static initialisation observing the default.
                // 4 rather than 1 so that a regression to "always the default"
                // is a failure rather than a coincidence.
                FileName = "ProcessorCountConfigured.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        ProcessorCount = 4
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The wall clock the guest observes through `DateTime.UtcNow`
                // boots at the Unix epoch by default. That is a replay-contract
                // value rather than an implementation detail, and the pure test
                // cannot pin it: pure cases are cross-checked against the real
                // runtime, which reports today's date.
                FileName = "DateTimeUtcNowEpochDefault.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The monotonic clock the guest observes through `Stopwatch`
                // boots at zero and moves in whole milliseconds, and is the same
                // clock `Environment.TickCount64` reads. Those are
                // replay-contract facts the pure `StopwatchElapsed.cs` cannot
                // pin: it is cross-checked against the real runtime, whose
                // CLOCK_MONOTONIC counts from an unspecified origin at
                // nanosecond resolution. `TestMonotonicTimestamp` covers the
                // scaling arithmetic itself; this covers the chain from
                // `SystemNative_GetTimestamp` out to guest-visible `Stopwatch`.
                FileName = "StopwatchTimestampGranularity.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Same guest observation, but with the host moving the boot
                // instant to 2023-11-14T00:00:00Z. Covers the whole chain
                // (`KernelConfig.WallClockEpochMs` -> `withWallClockEpochMs` ->
                // `systemTimeAsTicks` -> `SystemNative_GetSystemTimeAsTicks`),
                // where `TestSystemTimeAsTicks` covers the tick arithmetic
                // itself.
                FileName = "DateTimeUtcNowEpochConfigured.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        WallClockEpochMs = 1_699_920_000_000L
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Same guest, reached the other way: the count comes from the
                // guest-visible `DOTNET_PROCESSOR_COUNT` knob rather than from
                // `KernelConfig.ProcessorCount`, which stays at its default.
                // Covers the whole chain (env overlay -> kernel table ->
                // `effectiveProcessorCount` -> the native handler), where
                // `TestEffectiveProcessorCount` covers the resolution rules
                // themselves.
                FileName = "ProcessorCountConfigured.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        Environment = Map.ofList [ "DOTNET_PROCESSOR_COUNT", "4" ]
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `Environment.Exit` from the entry thread. Exercises the same
                // `ProcessExit` path as `ExitFromWorker.cs` below, but with the
                // caller being the thread whose return would otherwise have
                // supplied the exit code: `Main` goes on to `return 100`, so a
                // regression that let the guest keep running past `_Exit` would
                // surface as exit code 100 instead of 1.
                FileName = "InstaQuit.cs"
                ExpectedReturnCode = 1
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises Environment.Exit called from a worker thread: the whole process
                // must terminate with the worker's exit code, not just that worker thread.
                FileName = "ExitFromWorker.cs"
                ExpectedReturnCode = 7
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises the SystemNative_Write success path: a guest that
                // DllImports SystemNative_Write directly and pushes a few
                // bytes at stdout. The pure-source test only covers the
                // error paths (negative size, bad fd, zero size); the
                // success path is impure because it appends to the
                // interpreter's `OutputLog` and we want to assert directly
                // on those bytes rather than try to capture the test
                // runner's real stdout. The guest returns 0 on success
                // (positive return from `Write`), so a regression in the
                // handler's return value or pointer decoding also surfaces
                // as a wrong exit code.
                FileName = "SystemNativeWriteSuccess.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState =
                    Some (fun state ->
                        // The guest writes the literal "hi\n" (3 bytes) to
                        // fd 1. If the handler decoded the pointer wrong,
                        // we'd see garbage or fewer bytes here.
                        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                        |> Seq.toArray
                        |> shouldEqual [| 0x68uy ; 0x69uy ; 0x0Auy |]

                        OutputLogEntry.bytesFor FileDescriptorRole.StandardError state.Kernel.OutputLog
                        |> fun bytes -> bytes.Length
                        |> shouldEqual 0
                    )
            }
            {
                // Exercises the SystemNative_Close / SystemNative_Dup handler
                // pair end-to-end against the PawPrint FileDescriptorRegistry:
                // close of an invalid fd, close of a freshly-duped fd, the
                // double-close EBADF path, and the lowest-free gap-fill after
                // a close. This used to live in sourcesPure for cross-runtime
                // validation, but the real CLR's multi-threaded fd activity
                // races our close + dup window in the NUnit test process, so
                // it now runs as an impure (PawPrint-only) test where the
                // interpreter's deterministic single-threaded fd table makes
                // the assertions stable. The registry-level invariants are
                // still independently covered by TestFileDescriptorRegistry's
                // property tests; this test verifies the wiring from the
                // P/Invoke handler through to the registry.
                FileName = "SystemNativeClose.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises the SystemNative_IsATty PawPrint handler against
                // standard fds, a freshly-duped fd, and a closed fd. Lives in
                // sourcesImpure because the real CLR's IsATty answer depends
                // on whether the test process happens to have a TTY attached
                // to its standard streams, which races with how a developer
                // happens to run NUnit; PawPrint's headless-process model
                // makes the answer stable by construction.
                FileName = "SystemNativeIsATty.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
        ]

    let runTest (case : EndToEndTestCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", case.FileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let terminalState, terminatingThread =
                match
                    Program.run loggerFactory (Some case.FileName) peImage dotnetRuntimes case.KernelConfig None []
                with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"Guest called Environment.FailFast: %s{m}"
                | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"
                | RunOutcome.NormalExit (state, thread) -> state, thread
                | RunOutcome.ProcessExit (state, thread) -> state, thread

            let exitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return a value, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 i -> i
                    | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

            exitCode |> shouldEqual case.ExpectedReturnCode

            match case.AssertTerminalState with
            | None -> ()
            | Some assertion -> assertion terminalState
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (case : EndToEndTestCase) = runTest case

    [<TestCaseSource(nameof cases)>]
    let ``Can evaluate C# files`` (case : EndToEndTestCase) = runTest case
