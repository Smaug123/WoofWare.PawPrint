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
            "EnumSemantics.cs" // Constrained works; blocked on System.Object.GetType JIT intrinsic reached from Object.ToString
            "OverlappingStructs.cs" // blocked on Marshal.SizeOfHelper PInvoke boundary
            "AdvancedStructLayout.cs" // "TODO: couldn't identify field at offset"
            "Threads.cs" // "TODO: Constrained unimplemented"
            "LdtokenField.cs" // needs re-triage after RuntimeTypeHandle.GetGCHandle support
            "GenericEdgeCases.cs" // string byrefs now work; current Number..cctor hits the later large SpanHelpers.Memmove path
            "UnsafeAs.cs" // Unsafe.As primitive reinterprets work; blocked on struct/object byte views in readManagedByref
            "CastClassCrossAssembly.cs" // MethodTable/RawArrayData projections now work; blocked downstream on SpanHelpers.Memmove
            "CrossAssemblyTypes.cs" // Vector acceleration query works; blocked downstream on Monitor.ReliableEnter during CoreLib resource-string construction
            "InitializeArrayBoxedFieldHandle.cs" // Modified byref generic parameter concretization works; needs re-triage after ReadOnlySpan<T>.get_Item support
            "ConstrainedCallvirtStructOverload.cs" // constrained. prefix correctly boxes for case 3; blocked downstream on Object.GetType intrinsic reached from ValueType.Equals
            "ConstrainedCallvirtStructNewToString.cs" // constrained. prefix correctly boxes for case 3; blocked downstream on Object.GetType intrinsic reached from ValueType.ToString
            "ExceptionContinuationNestedFinally.cs" // nested EH inside a propagating finally overwrites the single-slot ExceptionContinuation; filters are not required to hit this
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
            "MonitorEnterRefBool.cs",
            (1,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
            "ProcessorCount.cs",
            (0,
             { empty with
                 System_Environment = System_Environment.passThru
             })
            "EnvironmentCurrentManagedThreadId.cs",
            (0,
             { empty with
                 System_Environment = System_Environment.passThru
             })
            "EnvironmentCurrentManagedThreadIdThread.cs",
            (0,
             { empty with
                 System_Environment = System_Environment.passThru
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
            // Thread.Start() takes the `lock (this)` in StartCore, so we need Monitor.Enter/Exit
            // implemented for the test to progress past StartCore into StartInternal.
            "ThreadStartJoin.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
            // Joining an already-Terminated thread: the second t.Join() must not block,
            // which exercises the Terminated branch of the Join intrinsic.
            "ThreadJoinTwice.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
            // ParameterizedThreadStart: t.Start(marker) must thread the argument through
            // StartHelper._startArg into the worker's single parameter.
            "ThreadParameterizedStart.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
            // Thread.Join(0) must be a non-blocking poll: false before the worker has
            // terminated, true once it has.
            "ThreadJoinZeroTimeout.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })
        ]
        |> Map.ofList

    let unimplementedMockTests : Map<string, int * NativeImpls> =
        let empty = MockEnv.make ()

        [
            "InterfaceDispatch.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

            // GetGCHandle now works; this next blocks on RuntimeTypeHandle.GetModule
            // while constructing the NullReferenceException message.
            "NullDereferenceTest.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

            // Was blocked on RuntimeTypeHandle.GetGCHandle; needs re-triage.
            "CastClassInvalid.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

            // Was blocked on RuntimeTypeHandle.GetGCHandle; needs re-triage.
            "CastclassFailures.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

            // Was blocked on RuntimeTypeHandle.GetGCHandle; needs re-triage.
            "ComplexTryCatch.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

            // Was blocked on RuntimeTypeHandle.GetGCHandle; needs re-triage.
            "ThrowingCctorProperties.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

            // CurrentManagedThreadId now works; blocked downstream on TypeConcretization
            // generic method parameter 0 from CollectionsMarshal.AsSpan initobj.
            "ResizeArray.cs",
            (0,
             { empty with
                 System_Environment = System_Environment.passThru
             })

            // Was blocked on RuntimeTypeHandle.GetGCHandle; needs re-triage.
            "ThrowingCctorStackTrace.cs",
            (0,
             { empty with
                 System_Threading_Monitor = System_Threading_Monitor.passThru
             })

        ]
        |> Map.ofList

    let expectsUnhandledException = [ "UnhandledException.cs" ] |> Set.ofList

    let customExitCodes =
        [
            "NoOp.cs", 1
            "ExceptionWithNoOpFinally.cs", 3
            "ExceptionWithNoOpCatch.cs", 10
            "Threads.cs", 3
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
             || unimplementedMockTests.ContainsKey s
             || unimplemented.Contains s
             || expectsUnhandledException.Contains s)
            |> not
        )
        |> Seq.toList

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
            let realResult = RealRuntime.executeWithRealRuntime [||] image

            let pawPrintResult =
                Program.run loggerFactory (Some case.FileName) peImage dotnetRuntimes case.NativeImpls []

            // NormalExit and ProcessExit both represent a clean process termination with
            // an exit code on the terminating thread's eval stack; the only difference is
            // whether the guest returned from Main or called Environment.Exit. The real
            // runtime surfaces both as RealRuntimeResult.NormalExit, so normalise here.
            let normalisedPawPrint =
                match pawPrintResult with
                | RunOutcome.ProcessExit (s, t) -> RunOutcome.NormalExit (s, t)
                | other -> other

            match realResult, normalisedPawPrint with
            | RealRuntimeResult.NormalExit exitCode, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                exitCode |> shouldEqual case.ExpectedReturnCode

                let pawPrintExitCode =
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | [] -> failwith "expected program to return a value, but it returned void"
                    | head :: _ ->
                        match head with
                        | EvalStackValue.Int32 i -> i
                        | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

                pawPrintExitCode |> shouldEqual exitCode
            | RealRuntimeResult.UnhandledException _, RunOutcome.GuestUnhandledException _ ->
                if not case.ExpectsUnhandledException then
                    failwith
                        $"Both runtimes threw unhandled exceptions for %s{case.FileName}, but this test was not expected to throw. Add to expectsUnhandledException if intentional."
            | RealRuntimeResult.NormalExit exitCode, RunOutcome.GuestUnhandledException (_, _, exn) ->
                failwith
                    $"Real runtime exited normally with code %d{exitCode}, but PawPrint threw unhandled exception: %O{exn.ExceptionObject}"
            | RealRuntimeResult.UnhandledException realExn, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                let pawPrintExitCode =
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | [] -> None
                    | EvalStackValue.Int32 i :: _ -> Some i
                    | _ -> None

                failwith
                    $"Real runtime threw unhandled %s{realExn.GetType().Name}, but PawPrint exited normally (code: %O{pawPrintExitCode})"
            | _, RunOutcome.ProcessExit _ -> failwith "unreachable: normalised away above"

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
            ExpectsUnhandledException = false
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
            ExpectsUnhandledException = false
        }
        |> runTest

    [<TestCaseSource(nameof requiresMocks)>]
    let ``Tests which require mocks`` (KeyValue (fileName : string, (exitCode : int, mock : NativeImpls))) =
        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            NativeImpls = mock
            ExpectsUnhandledException = false
        }
        |> runTest


    [<TestCaseSource(nameof expectsUnhandledException)>]
    let ``Tests which throw unhandled exceptions`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0 // not checked; both runtimes are expected to throw
            NativeImpls = MockEnv.make ()
            ExpectsUnhandledException = true
        }
        |> runTest

    [<TestCaseSource(nameof unimplemented)>]
    let ``Unimplemented tests have correct real-runtime behaviour`` (fileName : string) =
        let source = Assembly.getEmbeddedResourceAsString fileName assy
        let image = Roslyn.compile [ source ]

        let expectedExitCode =
            customExitCodes |> Map.tryFind fileName |> Option.defaultValue 0

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual expectedExitCode
        | RealRuntimeResult.UnhandledException exn ->
            failwith $"Real runtime threw unhandled %s{exn.GetType().Name} for %s{fileName}: %s{exn.Message}"

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
            ExpectsUnhandledException = false
        }
        |> runTest

    [<TestCaseSource(nameof unimplementedMockTests)>]
    let ``Unimplemented mock tests have correct real-runtime behaviour``
        (KeyValue (fileName : string, (exitCode : int, _mock : NativeImpls)))
        =
        let source = Assembly.getEmbeddedResourceAsString fileName assy
        let image = Roslyn.compile [ source ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit actualExitCode -> actualExitCode |> shouldEqual exitCode
        | RealRuntimeResult.UnhandledException exn ->
            failwith $"Real runtime threw unhandled %s{exn.GetType().Name} for %s{fileName}: %s{exn.Message}"

    [<TestCaseSource(nameof unimplementedMockTests)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented mock tests``
        (KeyValue (fileName : string, (exitCode : int, mock : NativeImpls)))
        =
        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            NativeImpls = mock
            ExpectsUnhandledException = false
        }
        |> runTest
