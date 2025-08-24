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
[<Parallelizable(ParallelScope.All)>]
module TestPureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            {
                FileName = "CrossAssemblyTypes.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "OverlappingStructs.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "AdvancedStructLayout.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "InitializeArray.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "Threads.cs"
                ExpectedReturnCode = 3
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "ComplexTryCatch.cs"
                ExpectedReturnCode = 14
                NativeImpls = NativeImpls.PassThru ()
            }
            {
                FileName = "ResizeArray.cs"
                ExpectedReturnCode = 114
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "Sizeof.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "LdtokenField.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
        ]

    let cases : EndToEndTestCase list =
        [
            {
                FileName = "NoOp.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "UnsafeAs.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "Initobj.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "GenericEdgeCases.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TestShl.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TestShr.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "StaticVariables.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "Ldind.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "CustomDelegate.cs"
                ExpectedReturnCode = 8
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "ArgumentOrdering.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "BasicLock.cs"
                ExpectedReturnCode = 1
                NativeImpls =
                    let mock = MockEnv.make ()

                    { mock with
                        System_Threading_Monitor = System_Threading_Monitor.passThru
                    }
            }
            {
                FileName = "TriangleNumber.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "ExceptionWithNoOpFinally.cs"
                ExpectedReturnCode = 3
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "ExceptionWithNoOpCatch.cs"
                ExpectedReturnCode = 10
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "Floats.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TryCatchWithThrowInBody.cs"
                ExpectedReturnCode = 4
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "Ldelema.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TypeConcretization.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "TestOr.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "InterfaceDispatch.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
        ]

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

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (case : EndToEndTestCase) = runTest case

    [<TestCaseSource(nameof cases)>]
    let ``Can evaluate C# files`` (case : EndToEndTestCase) = runTest case
