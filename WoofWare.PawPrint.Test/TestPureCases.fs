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
            "MultiDimensionalArrays.cs" // `int[,]` lowers to calls on the synthetic instance methods `Set(int,int,int)`/`Get(int,int)`/`Address(int,int)`/`.ctor(int,int)` of the array type, which the runtime does not yet synthesise
            "AdvancedStructLayout.cs" // past MarshalNative_SizeOfHelper for ByValTStr and SystemNative_Malloc / SystemNative_Free / Marshal.AllocHGlobal / FreeHGlobal; now blocked downstream at the unimplemented MarshalNative_TryGetStructMarshalStub QCall (CoreLib's Marshal.StructureToPtr path)
            "MarshalSizeOfAutoLayoutStruct.cs" // marshal walker correctly rejects `[StructLayout(LayoutKind.Auto)]` at top level (see CliValueType.IsAutoLayout), but `MarshalNative_SizeOfHelper` in `Native/NativeMarshal.fs` currently turns the `Result.Error` into a host `failwith` rather than raising a managed `ArgumentException`. Needs the QCall handler to translate marshal-size failures into a guest-side ArgumentException throw (CoreCLR `marshalnative.cpp:169` `COMPlusThrow(kArgumentException, IDS_CANNOT_MARSHAL, ...)`).
            "LdtokenField.cs" // past InternalCall System.Buffer::BulkMoveWithWriteBarrierInternal; now blocked during reflection-cache update because Buffer's byte-wise copy refuses to byte-view object-reference array cells (`validateByteAddressableCell` rejects ObjectRef storage)
            "RuntimeFieldHandleGetUtf8Name.cs" // exercises RuntimeFieldHandle::GetUtf8NameInternal, RuntimeTypeHandle::GetInterfaces, and Volatile.Write of object refs successfully; now blocked at the next step in the reflection-cache copy by `validateByteAddressableCell` refusing to byte-view object-reference cells inside Buffer::BulkMoveWithWriteBarrierInternal
            "RuntimeTypeGetInterfacesInherited.cs" // exercises the QCall's inherited-base + transitive-interface walk; now blocked during the reflection-cache update by `validateByteAddressableCell` refusing to byte-view object-reference cells inside Buffer::BulkMoveWithWriteBarrierInternal
            "NullDereferenceTest.cs" // past InternalCall RuntimeMethodHandle::GetLoaderAllocatorInternal; now blocked by unimplemented QCall RuntimeMethodHandle::IsCAVisibleFromDecoratedType during custom-attribute filtering inside the ArgumentException ctor / ResourceManager init path
            "IsAssignableFromOpenGenericDefinition.cs" // TypeHandle::CanCastTo_NoCacheLookup handler currently TODO-fails on non-Closed RuntimeTypeHandleTargets (open generic definitions, generic parameters, method generic parameters); needs TypeDesc::CanCastTo modelling to return CoreCLR's answer rather than throwing
            "CrossAssemblyTypes.cs" // past MethodTable::ParentMethodTable projection; now blocked by unimplemented QCall EventPipeInternal_CreateProvider during static init of System.Diagnostics.Tracing.EventPipeInternal
            "InterfaceDispatch.cs" // past MetadataImport::GetCustomAttributeProps; now blocked by unimplemented InternalCall MetadataImport::GetParentToken
            "CastClassInvalid.cs" // blocked after Unsafe.IsNullRef by unimplemented QCall!AssemblyNative_GetResource
            "CastclassFailures.cs" // blocked after Unsafe.IsNullRef by unimplemented QCall!AssemblyNative_GetResource
            "ComplexTryCatch.cs" // blocked after Unsafe.IsNullRef by unimplemented QCall!AssemblyNative_GetResource
            "TypeDefCustomAttributeEnum.cs" // past DependentHandle InternalCalls (ConditionalWeakTable static init); now blocked by unimplemented JIT intrinsic System.Threading.Interlocked.Exchange(&, System.Boolean)
            "RethrowStackTraceBoundary.cs" // stack trace rendering lacks CLR inner-exception boundary and parameterised frames
            "ThrowingCctorProperties.cs" // blocked after Unsafe.IsNullRef by unimplemented QCall!AssemblyNative_GetResource
            "Threads.cs" // blocked by pointer arithmetic over a generated Data field after Interlocked.CompareExchange
            "MetadataImportGetSigOfMethodDef.cs" // exercises MetadataImport::GetSigOfMethodDef successfully; now blocked at the next step by unimplemented QCall RuntimeMethodHandle::IsCAVisibleFromDecoratedType
            "LdelemaArrayTypeMismatch.cs" // ArrayTypeMismatchException is raised correctly, but its ctor walks past MetadataImport::GetCustomAttributeProps and now reaches unimplemented MetadataImport::GetParentToken while constructing the message
            "MakeGenericTypeStructConstraint.cs" // past InternalCall RuntimeMethodHandle::GetLoaderAllocatorInternal; now blocked by unimplemented QCall RuntimeMethodHandle::IsCAVisibleFromDecoratedType during custom-attribute filtering inside the ArgumentException ctor / ResourceManager init path
            "MakeGenericTypeClassConstraint.cs" // past InternalCall RuntimeMethodHandle::GetLoaderAllocatorInternal; now blocked by unimplemented QCall RuntimeMethodHandle::IsCAVisibleFromDecoratedType during custom-attribute filtering inside the ArgumentException ctor / ResourceManager init path
            "MakeGenericTypeNewConstraint.cs" // past InternalCall RuntimeMethodHandle::GetLoaderAllocatorInternal; now blocked by unimplemented QCall RuntimeMethodHandle::IsCAVisibleFromDecoratedType during custom-attribute filtering inside the ArgumentException ctor / ResourceManager init path
            "EnumSemantics.cs" // blocked by unimplemented QCall RuntimeTypeHandle::GetDeclaringTypeHandle
            "GetDeclaringTypeNestedGeneric.cs" // past MethodTable::AuxiliaryData projection for OpenGenericTypeDefinition; now blocked by ldflda through synthetic MethodTableAuxiliaryData::ExposedClassObjectRaw field address (same blocker as GetElementTypeBasic.cs)
            "IsAssignableToBasic.cs" // blocked by unimplemented QCall RuntimeTypeHandle::GetDeclaringTypeHandle
            "RuntimeTypeHandleGetInstantiationOpenGeneric.cs" // blocked by unimplemented QCall RuntimeTypeHandle::GetDeclaringMethodForGenericParameter
            "InitializeArrayBoxedFieldHandle.cs" // past String::FastAllocateString(MethodTable*, nint); now blocked by unimplemented MethodTable field projection for ParentMethodTable
            "ArraySortHelperDefaultInt.cs" // past Environment_FailFast QCall (now wired up); now blocked downstream by ResourceManager hitting infinite recursion looking up 'Arg_NullReferenceException' in System.Private.CoreLib, which the BCL escalates to Environment.FailFast
            "GenericEdgeCases.cs" // past Unsafe.CopyBlockUnaligned JIT intrinsic; now blocked downstream by ResourceManager hitting infinite recursion looking up 'Arg_NullReferenceException' in System.Private.CoreLib, which the BCL escalates to Environment.FailFast (same blocker as ArraySortHelperDefaultInt.cs)
            "ActivatorCreateInstanceThrowingCtor.cs" // Activator.CreateInstance<T>() does not wrap the ctor's exception in TargetInvocationException. CoreCLR's RuntimeType.CreateInstanceOfT (RuntimeType.CoreCLR.cs:4045-4048) wraps; the PawPrint intrinsic in `tryHandleActivatorCreateInstance` just recurses into callMethod and lets the raw exception propagate. Fix needs a ctor-frame marker so the exception dispatcher can rethrow wrapped, plus a host helper to construct the TargetInvocationException.
            "IndirectMemoryOperations.cs" // TestIndirectNativeInt now passes (Conv_U/Conv_I anchor a ReinterpretAs T projection on plain array byrefs so subsequent pointer arithmetic is byte-stride per ECMA-335 §III.1.5); now blocked at TestIndirectInt8 by the pre-existing `Ldelem_i1` TODO at NullaryIlOp.fs (`arr[2]` access on `sbyte[]` after pinned-pointer writes)
            "ArithmeticOperations.cs" // Add_ovf/Mul_ovf/Div now raise managed OverflowException via raiseRuntimeException; now blocked downstream by unimplemented QCall RuntimeMethodHandle::IsCAVisibleFromDecoratedType during custom-attribute filtering inside the OverflowException ctor / ResourceManager init path
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
            "ContendedMonitorEnter.cs", 99
            "MonitorPulseWait.cs", 42
            "MonitorWaitReentrant.cs", 7
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
        (env : Map<string, string>)
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
                Program.run loggerFactory (Some sourceName) peImage dotnetRuntimes nativeImpls env []

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
            case.Environment
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
            Map.empty
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
            Map.empty
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
            (MockEnv.make ())
            ([ "PAWPRINT_TEST_VARIABLE", "configured" ] |> Map.ofList)
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
    let ``Kernel32 GetEnvironmentVariableW lookup is case-insensitive`` () =
        // `kernel32!GetEnvironmentVariableW` is case-insensitive on Windows
        // (the PEB env block is keyed by case-folded names). PawPrint stores
        // the guest env as a plain F# `Map<string,string>`, so the QCall shim
        // must walk it with an ordinal-ignore-case comparison instead of
        // `Map.tryFind`; this test would have caught the eager `Map.tryFind`
        // implementation that regressed Windows semantics.
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("pawprint_mixed_case_key") != "found")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("PAWPRINT_MIXED_CASE_KEY") != "found")
        {
            return 2;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "MockEnvironmentCaseInsensitiveLookup.cs"
            source
            (MockEnv.make ())
            ([ "PaWpRiNt_MiXeD_CaSe_KeY", "found" ] |> Map.ofList)
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
    let ``Caller-supplied env overlay wins over seeded default under case-insensitive collision`` () =
        // The seeded `EmulatedKernel.defaultEnvironment` carries
        // `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1`. A caller that passes a
        // lower-case overlay for the same logical name must replace the seed
        // (Windows env-block semantics), not coexist with it — otherwise the
        // case-insensitive `GetEnvironmentVariableW` would walk the map and
        // could return either the seed or the overlay depending on Map
        // ordering, which is not deterministic from the caller's perspective.
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        return Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") == "0" ? 0 : 1;
    }
}
"""

        runPawPrintSource
            "MockEnvironmentCaseInsensitiveOverlayWins.cs"
            source
            (MockEnv.make ())
            ([ "dotnet_system_globalization_invariant", "0" ] |> Map.ofList)
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
            Map.empty
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
            Map.empty
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
            Environment = Map.empty
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
            NativeImpls = MockEnv.make ()
            Environment = Map.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof requiresMocks)>]
    let ``Tests which require mocks`` (KeyValue (fileName : string, mock : NativeImpls)) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            NativeImpls = mock
            Environment = Map.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest


    [<TestCaseSource(nameof expectsUnhandledException)>]
    let ``Tests which throw unhandled exceptions`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0 // not checked; both runtimes are expected to throw
            NativeImpls = MockEnv.make ()
            Environment = Map.empty
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
            NativeImpls = MockEnv.make ()
            Environment = Map.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
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
            Environment = Map.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest
