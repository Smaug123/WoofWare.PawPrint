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
            "InterfaceSlotHiddenByDerivedMethod.cs" // PawPrint does not model interface slot ownership — which type's method implements a given interface-map entry's slot. `findClassImplementation` starts at the receiver and takes the first name/signature match, and `methodMatches` skips its non-virtual/`newslot` guard whenever the call target is an interface, so any same-signature method on the way down wins. The file covers both directions: a derived type that must *not* take an inherited slot (reproducible with no variance at all, and failing identically on `main`), and one that *must* take a slot it re-declares. Fixing it needs a real slot-to-implementation dispatch map, which changes ordinary non-variant interface dispatch too and so wants its own change; `VariantInterfaceSlotOwnership.cs` covers the cases the interface map alone can get right.
            "MarshalPtrToStructure.cs" // blocked at the unimplemented RuntimeTypeHandle_GetActivationInfo QCall. The non-generic `Marshal.PtrToStructure(IntPtr, Type)` allocates its result via `Activator.CreateInstance(Type, bool)` (Marshal.cs:572) before marshalling anything, which reaches `RuntimeType.CreateInstanceDefaultCtor` -> `RuntimeType.ActivatorCache` -> the QCall. PawPrint only intercepts the generic `Activator.CreateInstance<T>()` (IlMachineStateExecution.fs:875). Landing the QCall alone is not sufficient: `ActivatorCache` invokes the returned allocator/ctor addresses via `calli` (UnaryMetadataIlOp.fs:65, unimplemented), and CoreCLR's allocator is a JIT helper (reflectioninvocation.cpp:1565) with no managed MethodInfo to put in `NativeIntSource.FunctionPointer`. Split out of AdvancedStructLayout.cs, which now passes in full.
            "MarshalStructureToPtrDateTimeField.cs" // MarshalNative_TryGetStructMarshalStub doesn't yet synthesise an IL stub for has-layout-non-blittable structs; CoreCLR writes an 8-byte OADate (`MARSHAL_TYPE_DATE`) for a `DateTime` field, but PawPrint currently rejects the struct loudly rather than memmove-ing the managed `_dateData` bytes. Real implementation needs the OADate-conversion stub.
            "MarshalStructureToPtrDecimalField.cs" // MarshalNative_TryGetStructMarshalStub now rejects Decimal fields (CoreCLR routes them through `NFT_DECIMAL` stub synthesis because native `DECIMAL` is 8-byte aligned while managed `Decimal` is 4-byte aligned). Two follow-on gaps remain before this test can pass: (1) `Marshal.SizeOf<{int; decimal}>()` returns 20 instead of 24 because the marshal-size walk in `CliValueType.TryComputeMarshalSize` doesn't bump Decimal's field alignment to 8, and (2) the actual Decimal-marshal stub that writes a 16-byte native `DECIMAL` at the 8-byte-aligned offset.
            "StructLayoutAutoWithoutReferences.cs" // CoreCLR reaches `HandleAutoLayout` either because the type declares `LayoutKind.Auto` or because it holds GC references and is promoted (`PlaceInstanceFields`, methodtablebuilder.cpp:8212). PawPrint implements the promotion route only, and cannot implement the other: `Layout` (TypeInfo.fs:47) is built from the `ClassLayout` table, which carries only `Pack` and `Size`, while the LayoutKind lives in `TypeAttributes.LayoutMask` and is discarded — so a reference-free type declared `LayoutKind.Auto` is indistinguishable from a sequential one where fields are laid out. The file's sequential controls pass, which is what makes this a LayoutKind gap rather than a bucketing one. Closing it means widening `Layout` to carry the kind and threading it through every construction site, so it is its own change.
            "StructLayoutInt128Alignment.cs" // `Int128`/`UInt128` carry a nominal 16-byte alignment requirement that CoreCLR stores on the type (`MethodTable::GetFieldAlignmentRequirement`, methodtable.cpp:8853, fed by the `IsInt128OrHasInt128Fields` flag) rather than deriving from the fields. PawPrint derives a value type's alignment structurally, so `Int128` — two `ulong`s — comes out 8-aligned and every type embedding it is under-sized. This is orthogonal to the GC auto-layout rule: the first case in the file holds no reference at all and diverges identically. Fixing it needs a nominal required-alignment concept covering `Int128`, `UInt128` and the `Vector` family, which reaches non-GC sequential layout too and so wants its own change.
            "MdUtf8StringEqualsCaseInsensitiveUnicode.cs" // The `MdUtf8String_EqualsCaseInsensitive` QCall itself handles these cases (see the unit and property tests in `TestNativeMdUtf8String.fs`), but a non-ASCII member name can't reach it yet: `Encoding.UTF8.GetByteCount` on the requested name walks the string with a `clt.un` between two byrefs rooted at `ByrefRoot.StringCharAt` of the same string, and `ManagedPointerSource.tryByteAddressDeltaSign` only orders differing indices for `ByrefRoot.ArrayElement`, not `StringCharAt`. The ASCII half of the coverage lives in the sibling `MdUtf8StringEqualsCaseInsensitive.cs` and passes.
            "MakeGenericMethodConstraintSatisfied.cs" // The mirror of the passing MakeGenericMethodConstraintViolation.cs: arguments that *satisfy* a generic method's constraint must bind rather than throw, which is what stops an always-violating constraint check from passing the violation case. Binding does succeed, but reflection then continues into `RuntimeType.GetMethodBase` and reaches the unimplemented `RuntimeMethodHandle.IsDynamicMethod` InternalCall — an unrelated gap. Un-park when that lands.
            "MakeGenericMethodOpenArgument.cs" //`RuntimeMethodHandle_GetStubIfNeededSlow` (issue #743) handles `MakeGenericMethod` with closed type arguments, which is what every reachable path needs, but an argument that still contains generic parameters — `MakeGenericMethod(typeof(G<>))` or `MakeGenericMethod(someTypeParameter)` — cannot be represented. Both are legal: real .NET returns a MethodInfo with `ContainsGenericParameters = true`, inspectable but not invokable. PawPrint's `MethodHandle.MethodGenerics` is a `ConcreteTypeHandle list`, and `ConcreteTypeHandle` indexes `AllConcreteTypes`, whose entries carry only *closed* generic arguments, so the QCall fails with a precise TODO. Widening that representation reaches concretization and every other MethodHandle consumer, so it is its own change rather than part of the QCall.
            "TaskRunThrowSetsFaulted.cs" // Found while decomposing issue #713. The blocker this was originally parked for (an `ldsflda` through a `MemberReference` token, issue #723) landed in #740, so Task's fault-capture path now gets further and stops at the next primitive: the unimplemented QCall `ExceptionNative_GetFrozenStackTrace` (`System.Private.CoreLib System.Exception::GetFrozenStackTrace(ObjectHandleOnStack, ObjectHandleOnStack) -> void`), via `NativeCall.failUnimplemented` (Native/NativeCall.fs:683), filed as issue #754. Still reproduces from the minimal `t.IsFaulted` spin-check alone (no `.Wait()`/`.Exception` access needed), so this remains inside Task's own fault-recording rather than anything downstream that user code touches.
            "ComparerDefaultEnumCompare.cs" // `Comparer<TEnum>.Default` *selection* works and is asserted by the sibling `ComparerDefault.cs`; what is parked here is calling the comparer it returns. `EnumComparer<T>.Compare` delegates to `RuntimeHelpers.EnumCompareTo<T>` (Comparer.CoreCLR.cs:19), a distinct [Intrinsic] which PawPrint has not reviewed for the safe-intrinsic allowlist, so it stops at the `TODO: implement JIT intrinsic` failure in `callMethod` (IlMachineStateExecution.fs:1931). An allowlist entry alone is not enough: its IL body is `ldarga.s 0; ldarg.1; box T; constrained. T; callvirt Enum::CompareTo(object); ret`, so un-parking needs `Enum.CompareTo(object)` to be reachable under a `constrained.` callvirt on a boxed enum.
            "StringCtorArgumentValidation.cs" // The throwing half of the nine `String` constructors; the non-throwing half passes across five sibling files. Every case here reaches an `ArgumentOutOfRangeException`/`ArgumentNullException` throw helper whose message goes through `SR.Format(string, object?, object?)`, which builds its argument span with `new TwoObjects(arg0, arg1)` (System/InlineArrays.cs:9) — an `[InlineArray(2)]` over `object?`. Its constructor body is `this[0] = arg0; this[1] = arg1`, and the second of those is the inline-array tail gap already parked as `InlineArraySecondSlot.cs`, differing only in that a reference element fails in `reinterpretStorageBytes` ("write through `ReinterpretAs` over byte-unaddressable storage") rather than in the primitive-cell check. Confirmed by direct repro: an `[InlineArray(2)]` struct of `object?` accepts `this[0]` and fails on `this[1]`. Nothing here is string-specific — un-park when inline arrays get a real N-slot layout.
            "InlineArraySecondSlot.cs" // Indexing an `[InlineArray(N)]` struct past element 0, which is a third gap distinct from the two reference-storage siblings above and uses a *primitive* element to keep it that way. `buffer[1]` lowers to `Unsafe.Add(ref Unsafe.As<TBuffer, TElement>(ref buffer), 1)`, i.e. `[ReinterpretAs TElement; ByteOffset <element width>]`. PawPrint does not model `InlineArrayAttribute` at all: the struct's storage is just its one declared field, the repeat count `N` lives only in the attribute, and there is no second slot for that offset to land in — so this fails with "byte-view write at offset 4 for 4 bytes does not fit in single primitive cell of size 4". Fixing it means giving inline arrays a real N-slot layout, which is a type-system change rather than an extension of the byref reinterpret classifier the two sibling tests need.
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
                            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
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
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> Some i
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
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
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
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
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
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
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
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
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
