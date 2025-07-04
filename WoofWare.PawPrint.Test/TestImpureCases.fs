namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test

module TestImpureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            {
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                NativeImpls = NativeImpls.PassThru ()
                LocalVariablesOfMain = [] |> Some
            }
        ]

    let cases : TestCase list =
        [
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
                LocalVariablesOfMain = [] |> Some
            }
        ]

    let runTest (case : TestCase) : unit =
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

            match case.LocalVariablesOfMain with
            | None -> ()
            | Some expected -> finalVariables |> shouldEqual expected

        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    open Expecto

    [<Tests>]
    let tests =
        testList
            "Impure cases"
            [
                testList
                    "Can evaluate C# files"
                    [
                        for case in cases do
                            testCase case.FileName (fun () -> runTest case)
                    ]
                ptestList
                    "Can evaluate C# files (unimplemented)"
                    [
                        for case in unimplemented do
                            testCase case.FileName (fun () -> runTest case)
                    ]
            ]
