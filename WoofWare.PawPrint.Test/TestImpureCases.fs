namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestImpureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            // `Console.WriteLine("Hello, world!")` triggers lazy initialisation of `Console.Out`,
            // which descends Console::get_Out → ConsolePal::OpenStandardOutput → Interop+Sys::Dup,
            // then ConsolePal::EnsureInitializedCore → Interop.Sys.InitializeTerminalAndSignalHandling.
            // PawPrint now intercepts SystemNative_Dup via FileDescriptorRegistry, SystemNative_Write
            // via the same handler family, and SystemNative_InitializeTerminalAndSignalHandling as a
            // no-op success (matching the WASI variant — we model neither termios nor signals). The
            // WriteLine flow now blocks downstream on the unimplemented libSystem.Native P/Invoke
            // `SystemNative_IsATty`, called from `Console.IsOutputRedirected` to decide whether to
            // open a SyncTextWriter around stdout or a plain StreamWriter; until it lands the guest
            // cannot fall through to the byte-emitting `Write` call this test is meant to exercise.
            {
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                NativeImpls = NativeImpls.PassThru ()
                Environment = Map.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
        ]

    let cases : EndToEndTestCase list =
        [
            {
                FileName = "InstaQuit.cs"
                ExpectedReturnCode = 1
                Environment = Map.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
                NativeImpls =
                    let mock = MockEnv.make ()
                    let env = mock.System_Environment

                    { mock with
                        System_Environment =
                            { System_EnvironmentMock.Empty with
                                GetProcessorCount =
                                    fun thread state ->
                                        let state =
                                            state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) thread

                                        (state, WhatWeDid.Executed) |> ExecutionResult.stepped
                                GetCurrentManagedThreadId = env.GetCurrentManagedThreadId
                                _Exit =
                                    fun thread state ->
                                        let state = state |> IlMachineState.loadArgument thread 0
                                        ExecutionResult.Terminated (state, thread)
                            }
                    }
            }
            {
                // Exercises Environment.Exit called from a worker thread: the whole process
                // must terminate with the worker's exit code, not just that worker thread.
                FileName = "ExitFromWorker.cs"
                ExpectedReturnCode = 7
                Environment = Map.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
                NativeImpls =
                    let mock = MockEnv.make ()

                    { mock with
                        System_Environment = System_Environment.passThru
                    }
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
                Environment = Map.empty
                ExpectsUnhandledException = false
                NativeImpls = NativeImpls.PassThru ()
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
                    Program.run
                        loggerFactory
                        (Some case.FileName)
                        peImage
                        dotnetRuntimes
                        case.NativeImpls
                        case.Environment
                        []
                with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"Guest called Environment.FailFast: %s{m}"
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
