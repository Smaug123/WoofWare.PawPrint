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
            "AdvancedStructLayout.cs" // past fixed-buffer pointer arithmetic and MarshalNative_SizeOfHelper; now blocked by unimplemented PInvoke libSystem.Native!SystemNative_Malloc
            "LdtokenField.cs" // past Volatile.Write of an object reference; still blocked by unimplemented InternalCall System.Buffer::BulkMoveWithWriteBarrierInternal during reflection-cache update
            "RuntimeFieldHandleGetUtf8Name.cs" // exercises RuntimeFieldHandle::GetUtf8NameInternal, RuntimeTypeHandle::GetInterfaces, and Volatile.Write of object refs; still blocked by unimplemented InternalCall System.Buffer::BulkMoveWithWriteBarrierInternal
            "GenericEdgeCases.cs" // past BitOperations.Log2; still blocked by unimplemented JIT intrinsic System.Runtime.CompilerServices.Unsafe.CopyBlockUnaligned (reached via int.ToString -> Number.UInt32ToDecStr)
            "RethrowStackTraceBoundary.cs" // stack trace rendering returns exit code 11 because frames lack parameter signatures (the test asserts presence of "RethrowStackTraceBoundary.Thrower(String value)")
            "RuntimeTypeHandleGetInstantiationOpenGeneric.cs" // still blocked by unimplemented QCall RuntimeTypeHandle::GetDeclaringMethodForGenericParameter
            "Threads.cs" // past pointer arithmetic over the generated Data field; now blocked by unimplemented PInvoke libSystem.Native!SystemNative_LowLevelMonitor_Create
            // The remaining tests share a symptom rather than a blocker: an upstream
            // NullReferenceException is raised somewhere in the BCL while running the test,
            // and when SR.GetResourceString tries to look up the "Arg_NullReferenceException"
            // string to format the exception's message, the resource-loading path itself
            // throws another NRE. SR's recursion guard detects the loop and calls
            // Environment.FailFast(...). Real CoreCLR does not FailFast for these scenarios,
            // so each entry indicates a genuine PawPrint defect upstream of the FailFast.
            //   For RuntimeTypeGetInterfacesEmpty/Inherited the upstream NRE is reproducible
            //   and traces to `Ldsfld System.String::Empty` returning null: that field is
            //   marked `[Intrinsic]` in CoreLib and is populated by the CLR's EE startup
            //   rather than by a static cctor, so PawPrint's `cliTypeZeroOf` fallback hands
            //   out a zero-initialised reference (null) which then NREs in
            //   `MemberInfoCache<T>.GetListByName` at `name.Length`.
            // Other entries are likely upstream-NRE bugs of their own kind; they are grouped
            // here only because they share the SR.GetResourceString recursion-guard failure
            // mode once the test fixture wires FailFast as ExecutionResult.FailFast.
            "ArraySortHelperDefaultInt.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "CastClassInvalid.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "CastclassFailures.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "ComplexTryCatch.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "CrossAssemblyTypes.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "EnumSemantics.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "InitializeArrayBoxedFieldHandle.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "InterfaceDispatch.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "IsAssignableToBasic.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "LdelemaArrayTypeMismatch.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "MakeGenericTypeClassConstraint.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "MakeGenericTypeNewConstraint.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "MakeGenericTypeStructConstraint.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "MetadataImportGetSigOfMethodDef.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "NullDereferenceTest.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "RuntimeTypeGetInterfacesEmpty.cs" // upstream NRE in MemberInfoCache.GetListByName because `String::Empty` is uninitialised; SR.GetResourceString's recursion guard then FailFasts (see group comment above)
            "RuntimeTypeGetInterfacesInherited.cs" // upstream NRE in MemberInfoCache.GetListByName because `String::Empty` is uninitialised; SR.GetResourceString's recursion guard then FailFasts (see group comment above)
            "ThrowingCctorProperties.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
            "TypeDefCustomAttributeEnum.cs" // upstream NRE -> SR.GetResourceString recursion guard -> Environment.FailFast (see group comment above)
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
                | _, RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"

                    failwith $"PawPrint guest called Environment.FailFast for %s{case.FileName}: %s{m}"
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

    [<Test>]
    let ``Mock environment exposes invariant globalization switch`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        return Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") == "1" ? 0 : 1;
    }
}
"""

        runPawPrintSource
            "MockEnvironmentInvariantGlobalization.cs"
            source
            (MockEnv.make ())
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 exitCode :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Mock environment returns configured variables and null for missing variables`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("PAWPRINT_TEST_VARIABLE") != "configured")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") != "1")
        {
            return 5;
        }

        string missing = Environment.GetEnvironmentVariable("PAWPRINT_MISSING_VARIABLE");

        if (missing == "configured")
        {
            return 2;
        }

        if (missing == "")
        {
            return 3;
        }

        if (missing != null)
        {
            return 4;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "MockEnvironmentConfiguredVariables.cs"
            source
            (MockEnv.makeWithEnvironment ([ "PAWPRINT_TEST_VARIABLE", "configured" ] |> Map.ofList))
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 exitCode :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Mock environment preserves missing variable last PInvoke error`` () =
        let source =
            """
using System;
using System.Runtime.InteropServices;

class Program
{
    static int Main(string[] args)
    {
        Marshal.SetLastPInvokeError(0);

        string missing = Environment.GetEnvironmentVariable("PAWPRINT_MISSING_VARIABLE");

        if (missing != null)
        {
            return 1;
        }

        return Marshal.GetLastPInvokeError() == 203 ? 0 : 2;
    }
}
"""

        runPawPrintSource
            "MockEnvironmentMissingVariableLastPInvokeError.cs"
            source
            (MockEnv.make ())
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 exitCode :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Environment.FailFast aborts execution`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        Environment.FailFast("boom");
        return 0;
    }
}
"""

        let nativeImpls =
            let empty = MockEnv.make ()

            { empty with
                System_Environment = System_Environment.passThru
            }

        runPawPrintSource
            "EnvironmentFailFast.cs"
            source
            nativeImpls
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.FailFast (_, _, message) -> message |> shouldEqual (Some "boom")
                | RunOutcome.NormalExit _ -> failwith "expected FailFast, got normal exit"
                | RunOutcome.ProcessExit _ -> failwith "expected FailFast, got process exit"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"expected FailFast, got guest unhandled exception: %O{exn.ExceptionObject}"
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
