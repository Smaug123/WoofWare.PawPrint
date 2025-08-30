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
module TestPureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            "CrossAssemblyTypes.cs"
            "OverlappingStructs.cs"
            "AdvancedStructLayout.cs"
            "InitializeArray.cs"
            "Threads.cs"
            "ComplexTryCatch.cs"
            "ResizeArray.cs"
            "LdtokenField.cs"
            "GenericEdgeCases.cs"
            "UnsafeAs.cs"
        ]
        |> Set.ofList

    let requiresMocks =
        let empty = MockEnv.make ()

        [
            "BasicLock.cs",
            (1,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
        ]
        |> Map.ofList

    let customExitCodes =
        [
            "NoOp.cs", 1
            "CustomDelegate.cs", 8
            "ExceptionWithNoOpFinally.cs", 3
            "ExceptionWithNoOpCatch.cs", 10
            "TryCatchWithThrowInBody.cs", 4
            "ResizeArray.cs", 114
            "Threads.cs", 3
            "TriangleNumber.cs", 10
        ]
        |> Map.ofList

    let allPure =
        assy.GetManifestResourceNames ()
        |> Seq.choose (fun res ->
            let s = "WoofWare.PawPrint.Test.sourcesPure."

            if res.StartsWith (s, StringComparison.OrdinalIgnoreCase) then
                res.Substring s.Length |> Some
            else
                None
        )
        |> Set.ofSeq

    let simpleCases : string list =
        allPure
        |> Seq.filter (fun s ->
            (customExitCodes.ContainsKey s
             || requiresMocks.ContainsKey s
             || unimplemented.Contains s)
            |> not
        )
        |> Seq.toList

    let runTest (case : EndToEndTestCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let realResult = RealRuntime.executeWithRealRuntime [||] image
            realResult.ExitCode |> shouldEqual case.ExpectedReturnCode

            let terminalState, terminatingThread =
                Program.run loggerFactory (Some case.FileName) peImage dotnetRuntimes case.NativeImpls []

            let exitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return a value, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 i -> i
                    | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

            exitCode |> shouldEqual realResult.ExitCode

        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<TestCaseSource(nameof simpleCases)>]
    let ``Standard tests`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> runTest

    [<TestCaseSource(nameof customExitCodes)>]
    let ``Custom exit code tests`` (KeyValue (fileName : string, exitCode : int)) =
        if unimplemented.Contains fileName then
            Assert.Inconclusive ()

        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            NativeImpls = MockEnv.make ()
        }
        |> runTest

    [<TestCaseSource(nameof requiresMocks)>]
    let ``Tests which require mocks`` (KeyValue (fileName : string, (exitCode : int, mock : NativeImpls))) =
        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            NativeImpls = mock
        }
        |> runTest


    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> runTest
