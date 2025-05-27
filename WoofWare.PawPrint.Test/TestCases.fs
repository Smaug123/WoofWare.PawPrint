namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test

[<TestFixture>]
module TestCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            {
                FileName = "Threads.cs"
                ExpectedReturnCode = 3
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "BasicException.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "BasicLock.cs"
                ExpectedReturnCode = 10
                NativeImpls =
                    let mock = MockEnv.make ()

                    { mock with
                        System_Threading_Monitor = System_Threading_Monitor.passThru
                    }
            }
            {
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
            }
        ]

    let cases : TestCase list =
        [
            {
                FileName = "ExceptionWithNoOpCatch.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "ExceptionWithNoOpFinally.cs"
                ExpectedReturnCode = 3
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TryCatchWithThrowInBody.cs"
                ExpectedReturnCode = 4
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "NoOp.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TriangleNumber.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "InstaQuit.cs"
                ExpectedReturnCode = 1
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
        ]

    [<TestCaseSource(nameof cases)>]
    let ``Can evaluate C# files`` (case : TestCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let terminalState, terminatingThread =
                Program.run loggerFactory (Some case.FileName) peImage dotnetRuntimes case.NativeImpls []

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
    [<Explicit "not yet implemented">]
    let ``Can evaluate C# files, unimplemented`` (case : TestCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let terminalState, terminatingThread =
                Program.run loggerFactory (Some case.FileName) peImage dotnetRuntimes case.NativeImpls []

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
