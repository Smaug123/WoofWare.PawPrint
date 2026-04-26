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
            "EnumSemantics.cs" // blocked on Object.GetType intrinsic reached from ValueType.ToString
            "OverlappingStructs.cs" // blocked after Marshal.SizeOfHelper on field-backed struct reconstruction from raw bytes
            "AdvancedStructLayout.cs" // "TODO: couldn't identify field at offset"
            "Threads.cs" // infinite loop, apparently? test doesn't terminate
            "LdtokenField.cs" // Unimplemented RuntimeTypeHandle::GetModule
            "GenericEdgeCases.cs" // TODO: Unsafe.ByteOffset on unsupported byref: Pointer(<<RVA data...>>)
            "UnsafeAs.cs" // TODO: read through `ReinterpretAs` as non-primitive type .FourBytes
            "CastClassCrossAssembly.cs" // IndexOutOfRangeException in substituteGenericsInTypeDefn
            "CrossAssemblyTypes.cs" // TODO: byref element offset on non-array byref without a trailing byte-view ReinterpretAs projection
            "InitializeArrayBoxedFieldHandle.cs" // BUG: reached extern dispatch for System.Numerics.IMinMaxValue::getMaxValue
            "ConstrainedCallvirtStructOverload.cs" // blocked on Object.GetType intrinsic reached from ValueType.ToString
            "ConstrainedCallvirtStructNewToString.cs" // blocked on Object.GetType intrinsic reached from ValueType.ToString
            "InterfaceDispatch.cs" // still does not terminate after GetNamespace/static abstract interface progress
            "NullDereferenceTest.cs" // blocks on RuntimeTypeHandle.GetModule while constructing the NullReferenceException message
            "CastClassInvalid.cs" // Unimplemented RuntimeTypeHandle::GetModule
            "CastclassFailures.cs" // Unimplemented RuntimeTypeHandle::GetModule
            "ComplexTryCatch.cs" // Unimplemented RuntimeTypeHandle::GetModule
            "ThrowingCctorProperties.cs" // Unimplemented RuntimeTypeHandle::GetModule
        ]
        |> Set.ofList

    let requiresMocks =
        let empty = MockEnv.make ()

        [
            "ProcessorCount.cs",
            { empty with
                System_Environment = System_Environment.passThru
            }
            "EnvironmentCurrentManagedThreadId.cs",
            { empty with
                System_Environment = System_Environment.passThru
            }
            "EnvironmentCurrentManagedThreadIdThread.cs",
            { empty with
                System_Environment = System_Environment.passThru
            }
        ]
        |> Map.ofList

    let unimplementedMockTests : Map<string, NativeImpls> =
        let empty = MockEnv.make ()

        [
            // CurrentManagedThreadId now works; blocked downstream on TypeConcretization
            // generic method parameter 0 from CollectionsMarshal.AsSpan initobj.
            "ResizeArray.cs",
            { empty with
                System_Environment = System_Environment.passThru
            }
        ]
        |> Map.ofList

    let expectsUnhandledException = [ "UnhandledException.cs" ] |> Set.ofList

    let customExitCodes =
        [
            "NoOp.cs", 1
            "BasicLock.cs", 1
            "MonitorEnterRefBool.cs", 1
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

    let runPawPrintSource
        (sourceName : string)
        (source : string)
        (nativeImpls : NativeImpls)
        (assertResult : byte array -> RunOutcome -> unit)
        : unit
        =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let pawPrintResult =
                Program.run loggerFactory (Some sourceName) peImage dotnetRuntimes nativeImpls []

            assertResult image pawPrintResult
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let runTest (case : EndToEndTestCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy

        runPawPrintSource
            case.FileName
            source
            case.NativeImpls
            (fun image pawPrintResult ->
                let realResult = RealRuntime.executeWithRealRuntime [||] image

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
                | RealRuntimeResult.UnhandledException realExn,
                  RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    let pawPrintExitCode =
                        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                        | [] -> None
                        | EvalStackValue.Int32 i :: _ -> Some i
                        | _ -> None

                    failwith
                        $"Real runtime threw unhandled %s{realExn.GetType().Name}, but PawPrint exited normally (code: %O{pawPrintExitCode})"
                | _, RunOutcome.ProcessExit _ -> failwith "unreachable: normalised away above"
            )

    [<Test>]
    let ``Unhandled rethrow preserves original throw stack frame`` () =
        let source =
            """
using System;

class StackTraceSentinelException : Exception
{
}

class Program
{
    static void Blow()
    {
        throw new StackTraceSentinelException();
    }

    static int Main(string[] args)
    {
        try
        {
            Blow();
        }
        catch
        {
            throw;
        }

        return 1;
    }
}
"""

        runPawPrintSource
            "RethrowStackTrace.cs"
            source
            (MockEnv.make ())
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    match exn.StackTrace with
                    | firstFrame :: _ -> firstFrame.Method.Name |> shouldEqual "Blow"
                    | [] -> failwith "Expected an unhandled rethrow to keep the original throw stack frame"
                | outcome -> failwith $"Expected an unhandled rethrow, got %O{outcome}"
            )

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
    let ``Tests which require mocks`` (KeyValue (fileName : string, mock : NativeImpls)) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
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
        (KeyValue (fileName : string, _mock : NativeImpls))
        =
        let source = Assembly.getEmbeddedResourceAsString fileName assy
        let image = Roslyn.compile [ source ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit actualExitCode -> actualExitCode |> shouldEqual 0
        | RealRuntimeResult.UnhandledException exn ->
            failwith $"Real runtime threw unhandled %s{exn.GetType().Name} for %s{fileName}: %s{exn.Message}"

    [<TestCaseSource(nameof unimplementedMockTests)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented mock tests`` (KeyValue (fileName : string, mock : NativeImpls)) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            NativeImpls = mock
            ExpectsUnhandledException = false
        }
        |> runTest
