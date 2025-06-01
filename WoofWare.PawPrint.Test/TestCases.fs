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
                LocalVariablesOfMain = []
            }
            {
                FileName = "ExceptionWithNoOpCatch.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = []
            }
            {
                FileName = "TryCatchWithThrowInBody.cs"
                ExpectedReturnCode = 4
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain =
                    [
                        // one variable is x, one variable is the return value which also happens to have the same value
                        4
                        4
                    ]
                    |> List.map (fun i -> CliType.Numeric (CliNumericType.Int32 i))
            }
            {
                FileName = "ComplexTryCatch.cs"
                ExpectedReturnCode = 14
                NativeImpls = NativeImpls.PassThru ()
                LocalVariablesOfMain =
                    [
                        4
                        20
                        115
                        12
                        1
                        10
                        2
                        112
                        12
                        1111
                        42
                        99
                        25
                        50
                        123
                        20
                        35
                        5
                        11111
                        100001
                    ]
                    |> List.map (fun i -> CliType.Numeric (CliNumericType.Int32 i))
            }
            {
                FileName = "BasicLock.cs"
                ExpectedReturnCode = 10
                NativeImpls =
                    let mock = MockEnv.make ()

                    { mock with
                        System_Threading_Monitor = System_Threading_Monitor.passThru
                    }
                LocalVariablesOfMain = []
            }
            {
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                NativeImpls = NativeImpls.PassThru ()
                LocalVariablesOfMain = []
            }
        ]

    let cases : TestCase list =
        [
            {
                FileName = "NoOp.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = [ CliType.Numeric (CliNumericType.Int32 1) ]
            }
            {
                FileName = "TriangleNumber.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain =
                    [
                        // answer
                        CliType.Numeric (CliNumericType.Int32 10)
                        // i
                        CliType.Numeric (CliNumericType.Int32 5)
                        // End-loop condition
                        CliType.OfBool false
                        // Ret
                        CliType.Numeric (CliNumericType.Int32 10)
                    ]
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
                LocalVariablesOfMain = []
            }
            {
                FileName = "ExceptionWithNoOpFinally.cs"
                ExpectedReturnCode = 3
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain =
                    [
                        // Variable 1 is `x`, variable 2 is the implicit return value
                        4
                        3
                    ]
                    |> List.map (fun i -> CliType.Numeric (CliNumericType.Int32 i))
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

            let finalVariables =
                terminalState.ThreadState.[terminatingThread].MethodState.LocalVariables
                |> Seq.toList

            finalVariables |> shouldEqual case.LocalVariablesOfMain

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
