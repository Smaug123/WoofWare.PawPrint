namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations
open WoofWare.PawPrint.Test

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
                FileName = "InitializeArray.cs"
                ExpectedReturnCode = 0
                NativeImpls = MockEnv.make ()
            }
            {
                FileName = "GenericEdgeCases.cs"
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
                ExpectedReturnCode = 109
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

    let cases : TestCase list =
        [
            {
                FileName = "NoOp.cs"
                ExpectedReturnCode = 1
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
                FileName = "CastClassSimpleInheritance.cs"
                ExpectedReturnCode = 5
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsInstSimpleInheritance.cs"
                ExpectedReturnCode = 42
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassNull.cs"
                ExpectedReturnCode = 42
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassArrayCovariance.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassToObject.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstPatternMatching.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassMultipleInterfaces.cs"
                ExpectedReturnCode = 42
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassCrossAssembly.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassNestedTypes.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassGenerics.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassEnum.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassBoxing.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstBoxing.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassArray.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstArray.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstNull.cs"
                ExpectedReturnCode = 42
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassInvalid.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstFailed.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstFailedInterface.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "CastClassInterface.cs"
                ExpectedReturnCode = 1
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
            }
            {
                FileName = "IsinstInterface.cs"
                ExpectedReturnCode = 42
                NativeImpls = MockEnv.make ()
                LocalVariablesOfMain = None
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

            let realResult = RealRuntime.executeWithRealRuntime [||] image

            let exitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return a value, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 i -> i
                    | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

            exitCode |> shouldEqual realResult.ExitCode

            exitCode |> shouldEqual case.ExpectedReturnCode
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    open Expecto

    [<Tests>]
    let tests =
        testList
            "Pure cases"
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
