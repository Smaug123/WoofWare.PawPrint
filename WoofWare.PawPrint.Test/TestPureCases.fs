namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            "GCMemoryInfoSpanProperties.cs" // `GC.GetGCMemoryInfo()` itself works (see GCGetMemoryInfo.cs), but its two span-valued properties don't past index 0. CoreLib builds them with `MemoryMarshal.CreateReadOnlySpan(ref _generationInfo0, 5)` / `(ref _pauseDuration0, 2)`: a byref to the *first* of a run of sibling fields, walked forward by sizeof(element), relying on `[StructLayout(Sequential)]` having laid them out contiguously. PawPrint models a heap object as named field cells, not a byte block, so `GenerationInfo[1]` becomes a byte-view read at offset 48 into `_generationInfo0`'s own 32-byte cell and fails in `IlMachineManagedByref.resolveCell` (IlMachineManagedByref.fs:1086) with "does not fit in single primitive cell". `Length` and index 0 both work. This is the general "sibling fields are not contiguous" gap rather than anything GC-specific; tracked as issue #729.
            "ContravariantImplicitInterfaceDispatch.cs" // Pre-existing (reproducible on main): `in`-variance dispatch onto an *implicit* interface implementation fails to resolve, so we reach the abstract `ISink\`1::Accept` with no body. The explicit-MethodImpl form of the same shape works (ContravariantExplicitMethodImpl.cs), so the implicit-implementation match is presumably still comparing parameter types invariantly.
            "MarshalPtrToStructure.cs" // blocked at the unimplemented RuntimeTypeHandle_GetActivationInfo QCall. The non-generic `Marshal.PtrToStructure(IntPtr, Type)` allocates its result via `Activator.CreateInstance(Type, bool)` (Marshal.cs:572) before marshalling anything, which reaches `RuntimeType.CreateInstanceDefaultCtor` -> `RuntimeType.ActivatorCache` -> the QCall. PawPrint only intercepts the generic `Activator.CreateInstance<T>()` (IlMachineStateExecution.fs:875). Landing the QCall alone is not sufficient: `ActivatorCache` invokes the returned allocator/ctor addresses via `calli` (UnaryMetadataIlOp.fs:65, unimplemented), and CoreCLR's allocator is a JIT helper (reflectioninvocation.cpp:1565) with no managed MethodInfo to put in `NativeIntSource.FunctionPointer`. Split out of AdvancedStructLayout.cs, which now passes in full.
            "MarshalStructureToPtrDateTimeField.cs" // MarshalNative_TryGetStructMarshalStub doesn't yet synthesise an IL stub for has-layout-non-blittable structs; CoreCLR writes an 8-byte OADate (`MARSHAL_TYPE_DATE`) for a `DateTime` field, but PawPrint currently rejects the struct loudly rather than memmove-ing the managed `_dateData` bytes. Real implementation needs the OADate-conversion stub.
            "MarshalStructureToPtrDecimalField.cs" // MarshalNative_TryGetStructMarshalStub now rejects Decimal fields (CoreCLR routes them through `NFT_DECIMAL` stub synthesis because native `DECIMAL` is 8-byte aligned while managed `Decimal` is 4-byte aligned). Two follow-on gaps remain before this test can pass: (1) `Marshal.SizeOf<{int; decimal}>()` returns 20 instead of 24 because the marshal-size walk in `CliValueType.TryComputeMarshalSize` doesn't bump Decimal's field alignment to 8, and (2) the actual Decimal-marshal stub that writes a 16-byte native `DECIMAL` at the 8-byte-aligned offset.
            "MarshalStructureToPtrIntPtrField.cs" // Classifier+Memmove widening now lets `Marshal.StructureToPtr` reach the byte copy for `IntPtr`/`UIntPtr` fields, but the subsequent `Marshal.ReadInt32(ptr, ofs)` (Marshal.cs:332) does `(int)addr & 0x3` as an alignment check, hitting `Conv_I4` of a managed pointer to a native memory block. Needs alignment-aware `Conv_I4`/`Conv_U4` on managed pointers (or equivalent) to land.
            "ArrayGetInterfacesPointerElement.cs" // `RuntimeTypeHandle.GetInterfaces` handles a pointer-element array correctly (it is an ordinary SZ array handle, and `PopulateInterfaces` declines to add the implicit generic five because `arrayType.IsPointer`), but the test can't reach that far: materialising the `RuntimeType` for the *element* `int*` fails at `TODO: ldflda System.Runtime.CompilerServices.TypeDesc::_exposedClassObject through native pointer 0` (`UnaryMetadataFieldOps.fs:448`). Pre-existing and unrelated to interfaces — `typeof(int*).Name` alone hits it too, while `typeof(int*)` itself is fine. Needs a `RuntimeType` to be allocatable for a pointer TypeDesc.
            "MdUtf8StringEqualsCaseInsensitiveUnicode.cs" // The `MdUtf8String_EqualsCaseInsensitive` QCall itself handles these cases (see the unit and property tests in `TestNativeMdUtf8String.fs`), but a non-ASCII member name can't reach it yet: `Encoding.UTF8.GetByteCount` on the requested name walks the string with a `clt.un` between two byrefs rooted at `ByrefRoot.StringCharAt` of the same string, and `ManagedPointerSource.tryByteAddressDeltaSign` only orders differing indices for `ByrefRoot.ArrayElement`, not `StringCharAt`. The ASCII half of the coverage lives in the sibling `MdUtf8StringEqualsCaseInsensitive.cs` and passes.
        ]
        |> Set.ofList

    let expectsUnhandledException =
        [
            "UnhandledException.cs"
            "EnumHasFlagMismatchUnhandled.cs"
            "EnumHasFlagNullFlagUnhandled.cs"
            "ArrayGetLengthOutOfRangeUnhandled.cs"
        ]
        |> Set.ofList

    let customExitCodes = [ "ExceptionWithNoOpFinally.cs", 3 ] |> Map.ofList

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
             || unimplemented.Contains s
             || expectsUnhandledException.Contains s)
            |> not
        )
        |> Seq.toList

    let runPawPrintSource
        (sourceName : string)
        (source : string)
        (kernelConfig : KernelConfig)
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
                Program.run loggerFactory (Some sourceName) peImage dotnetRuntimes kernelConfig None []

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
            case.KernelConfig
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
                | _, RunOutcome.SignalTerminated (_, signal) ->
                    failwith
                        $"PawPrint guest was terminated by POSIX signal %O{signal} for %s{case.FileName}; this test does not exercise signal-driven termination"
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
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    match exn.StackTrace with
                    | firstFrame :: _ -> firstFrame.Method.Name |> shouldEqual "Blow"
                    | [] -> failwith "Expected an unhandled rethrow to keep the original throw stack frame"
                | outcome -> failwith $"Expected an unhandled rethrow, got %O{outcome}"
            )

    [<Test>]
    let ``Emulated environment exposes invariant globalization switch`` () =
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
            "EmulatedEnvironmentInvariantGlobalization.cs"
            source
            KernelConfig.Default
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
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Emulated environment returns configured variables and null for missing variables`` () =
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
            "EmulatedEnvironmentConfiguredVariables.cs"
            source
            { KernelConfig.Default with
                Environment = [ "PAWPRINT_TEST_VARIABLE", "configured" ] |> Map.ofList
            }
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
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``GetEnvironmentVariableW lookup is case-sensitive`` () =
        // CoreCLR's Unix PAL implements the `kernel32!GetEnvironmentVariableW`
        // import with exact name comparison (see pal/src/misc/environ.cpp
        // `FindEnvVarValue`), so the QCall shim must do exact-string lookup
        // against the kernel env map even though the *Windows* kernel32 entry
        // would be case-insensitive: PawPrint is baselined against the host
        // runtime, which is the Unix PAL on the hosts this repo runs on.
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("PaWpRiNt_MiXeD_CaSe_KeY") != "found")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("pawprint_mixed_case_key") != null)
        {
            return 2;
        }

        if (Environment.GetEnvironmentVariable("PAWPRINT_MIXED_CASE_KEY") != null)
        {
            return 3;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentCaseSensitiveLookup.cs"
            source
            { KernelConfig.Default with
                Environment = [ "PaWpRiNt_MiXeD_CaSe_KeY", "found" ] |> Map.ofList
            }
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
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Emulated environment preserves missing variable last PInvoke error`` () =
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
            "EmulatedEnvironmentMissingVariableLastPInvokeError.cs"
            source
            KernelConfig.Default
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
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
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

        runPawPrintSource
            "EnvironmentFailFast.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.FailFast (_, _, message) -> message |> shouldEqual (Some "boom")
                | RunOutcome.NormalExit _ -> failwith "expected FailFast, got normal exit"
                | RunOutcome.ProcessExit _ -> failwith "expected FailFast, got process exit"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected FailFast, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"expected FailFast, got guest unhandled exception: %O{exn.ExceptionObject}"
            )

    [<TestCaseSource(nameof simpleCases)>]
    let ``Standard tests`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig = KernelConfig.Default
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof customExitCodes)>]
    let ``Custom exit code tests`` (KeyValue (fileName : string, exitCode : int)) =
        if unimplemented.Contains fileName then
            Assert.Inconclusive ()

        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            KernelConfig = KernelConfig.Default
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof expectsUnhandledException)>]
    let ``Tests which throw unhandled exceptions`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0 // not checked; both runtimes are expected to throw
            KernelConfig = KernelConfig.Default
            ExpectsUnhandledException = true
            AssertTerminalState = None
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
            KernelConfig = KernelConfig.Default
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest
