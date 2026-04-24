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
            {
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                NativeImpls = NativeImpls.PassThru ()
                ExpectsUnhandledException = false
            }
        ]

    let cases : EndToEndTestCase list =
        [
            {
                FileName = "InstaQuit.cs"
                ExpectedReturnCode = 1
                ExpectsUnhandledException = false
                NativeImpls =
                    let mock = MockEnv.make ()

                    { mock with
                        System_Environment =
                            { System_EnvironmentMock.Empty with
                                GetProcessorCount =
                                    fun thread state ->
                                        let state =
                                            state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) thread

                                        (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
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
                ExpectsUnhandledException = false
                NativeImpls =
                    let mock = MockEnv.make ()

                    { mock with
                        System_Environment = System_Environment.passThru
                        System_Threading_Monitor = System_Threading_Monitor.passThru
                    }
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
                match Program.run loggerFactory (Some case.FileName) peImage dotnetRuntimes case.NativeImpls [] with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
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
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (case : EndToEndTestCase) = runTest case

    [<TestCaseSource(nameof cases)>]
    let ``Can evaluate C# files`` (case : EndToEndTestCase) = runTest case
