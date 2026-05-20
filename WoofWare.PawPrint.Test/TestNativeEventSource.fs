namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

/// Direct-call tests for the `XplatEventLogger` QCall handlers in
/// `NativeEventSource`. The handlers are reached from Linux-built CoreLib
/// only (FEATURE_EVENTSOURCE_XPLAT-gated), so we can't rely on the host
/// CoreLib exposing `XplatEventLogger` at all — instead we drive the
/// dispatcher synthetically: a real concretized method has its
/// `Signature` overridden to the shape the matcher expects, and we hand a
/// record-updated `TypeInfo` to `NativeCallContext` so the namespace/name
/// arms fire.
///
/// What this pins down:
///   * `tryParseClrConfigDword` matches CoreCLR's hex-default DWORD
///     parsing (radix 16, optional `0x` prefix tolerated).
///   * `lookupClrConfigString` honours the `DOTNET_*` → `COMPlus_*`
///     priority and discards empty values, mirroring
///     `clrconfig.cpp`:`EnvGetString`.
///   * The `IsEventSourceLoggingEnabled` arm of `tryExecuteQCall` returns
///     Int32 0/1 according to the env-honouring rules.
///   * The `LogEventSource` arm fails loud (so a guest with
///     `DOTNET_EnableEventLog=1` that actually emits an EventSource event
///     surfaces a clear diagnostic, rather than silently losing the
///     event).
[<TestFixture>]
module TestNativeEventSource =

    let private trivialSource : string =
        """
public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private prepareProgram (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : Program.PreparedProgram =
        let image =
            Roslyn.compileAssemblyWithResources
                "NativeEventSourceTest"
                OutputKind.ConsoleApplication
                []
                []
                [ trivialSource ]

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare
                loggerFactory
                (Some "NativeEventSourceTest.cs")
                peImage
                dotnetRuntimes
                (MockEnv.make ())
                Map.empty
                []
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected program to be ready before Main, got %O{outcome}"

    /// Resolve and concretize `Thread.YieldInternal` — used purely as a
    /// donor `MethodInfo<ConcreteTypeHandle, _, _>`. Its `Signature` is
    /// overridden to whatever shape each test needs; only the surrounding
    /// MethodInfo fields (DeclaringType, Body, etc.) are reused. The
    /// dispatcher reads neither, so the donor identity does not leak.
    let private donorConcretizedMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let threadType =
            baseClassTypes.Corelib.TryGetTopLevelTypeDef "System.Threading" "Thread"
            |> Option.defaultWith (fun () -> failwith "System.Threading.Thread not found in CoreLib")

        let rawMethod =
            threadType.Methods
            |> List.filter (fun method ->
                match method.NativeImport with
                | Some import ->
                    import.ModuleName = "QCall"
                    && import.EntryPointName = "ThreadNative_YieldThread"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | _ -> failwith "donor method ThreadNative_YieldThread not found on System.Threading.Thread"

        let state, method, _declaringType =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, method

    /// Build the synthetic `XplatEventLogger` target type. We start from a
    /// real CoreLib type (so all the metadata-derived fields are
    /// well-formed) and override only `Namespace` and `Name` to make the
    /// dispatcher's namespace/name arms fire.
    let private xplatTargetType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        { baseClassTypes.UInt16 with
            Namespace = "System.Diagnostics.Tracing"
            Name = "XplatEventLogger"
        }

    let private buildContext
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : NativeCallContext
        =
        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = method
                Arguments = ImmutableArray.Empty
            }

        {
            LoggerFactory = loggerFactory
            Implementations = MockEnv.make ()
            BaseClassTypes = prepared.BaseClassTypes
            Thread = prepared.EntryThread
            State = state
            Instruction = instruction
            TargetAssembly = prepared.BaseClassTypes.Corelib
            TargetType = xplatTargetType prepared.BaseClassTypes
        }

    let private withEnvironment (env : (string * string) list) (state : IlMachineState) : IlMachineState =
        state.MapKernel (EmulatedKernel.withEnvironment (Map.ofList env))

    // ---------- Pure helper tests ----------

    [<Test>]
    let ``tryParseClrConfigDword: empty and whitespace return None`` () : unit =
        NativeEventSource.tryParseClrConfigDword "" |> shouldEqual None
        NativeEventSource.tryParseClrConfigDword "   " |> shouldEqual None
        NativeEventSource.tryParseClrConfigDword "\t" |> shouldEqual None

    [<Test>]
    let ``tryParseClrConfigDword: hex by default (matches CoreCLR's GetConfigDWORD default radix)`` () : unit =
        // No `ParseIntegerAsBase10` flag on EnableEventLog ⇒ radix 16.
        NativeEventSource.tryParseClrConfigDword "1" |> shouldEqual (Some 1u)
        NativeEventSource.tryParseClrConfigDword "0" |> shouldEqual (Some 0u)
        NativeEventSource.tryParseClrConfigDword "10" |> shouldEqual (Some 16u)
        NativeEventSource.tryParseClrConfigDword "ff" |> shouldEqual (Some 255u)
        NativeEventSource.tryParseClrConfigDword "FF" |> shouldEqual (Some 255u)

        NativeEventSource.tryParseClrConfigDword "deadbeef"
        |> shouldEqual (Some 0xdeadbeefu)

    [<Test>]
    let ``tryParseClrConfigDword: 0x prefix is tolerated (matches wcstoul base-16)`` () : unit =
        NativeEventSource.tryParseClrConfigDword "0x1" |> shouldEqual (Some 1u)
        NativeEventSource.tryParseClrConfigDword "0X10" |> shouldEqual (Some 16u)

        NativeEventSource.tryParseClrConfigDword "0xDEADBEEF"
        |> shouldEqual (Some 0xdeadbeefu)

    [<Test>]
    let ``tryParseClrConfigDword: malformed input returns None (not crash)`` () : unit =
        NativeEventSource.tryParseClrConfigDword "garbage" |> shouldEqual None
        NativeEventSource.tryParseClrConfigDword "0xZZ" |> shouldEqual None
        NativeEventSource.tryParseClrConfigDword "0x" |> shouldEqual None
        // Larger than uint32.MaxValue — TryParse rejects, we surface None
        // (CoreCLR would silently overflow inside wcstoul; we err on the
        // side of treating implausible values as unset).
        NativeEventSource.tryParseClrConfigDword "100000000" |> shouldEqual None

    [<Test>]
    let ``tryParseClrConfigDword: surrounding whitespace is trimmed`` () : unit =
        NativeEventSource.tryParseClrConfigDword "  1  " |> shouldEqual (Some 1u)
        NativeEventSource.tryParseClrConfigDword "\t0x10\n" |> shouldEqual (Some 16u)

    [<Test>]
    let ``lookupClrConfigString: DOTNET_ wins over COMPlus_ when both set`` () : unit =
        let env =
            Map.ofList [ "DOTNET_TestKnob", "dotnet-value" ; "COMPlus_TestKnob", "complus-value" ]

        NativeEventSource.lookupClrConfigString env "TestKnob"
        |> shouldEqual (Some "dotnet-value")

    [<Test>]
    let ``lookupClrConfigString: COMPlus_ is used when DOTNET_ is unset`` () : unit =
        let env = Map.ofList [ "COMPlus_TestKnob", "complus-value" ]

        NativeEventSource.lookupClrConfigString env "TestKnob"
        |> shouldEqual (Some "complus-value")

    [<Test>]
    let ``lookupClrConfigString: empty value is treated as unset (matches CoreCLR's '*ret != W('\0')' filter)``
        ()
        : unit
        =
        // DOTNET_ is present but empty: skip past it.
        let env = Map.ofList [ "DOTNET_TestKnob", "" ; "COMPlus_TestKnob", "fallback" ]

        NativeEventSource.lookupClrConfigString env "TestKnob"
        |> shouldEqual (Some "fallback")

        // Both empty: None
        let env = Map.ofList [ "DOTNET_TestKnob", "" ; "COMPlus_TestKnob", "" ]

        NativeEventSource.lookupClrConfigString env "TestKnob" |> shouldEqual None

    [<Test>]
    let ``lookupClrConfigString: nothing set returns None`` () : unit =
        NativeEventSource.lookupClrConfigString Map.empty "EnableEventLog"
        |> shouldEqual None

    // ---------- Dispatcher tests ----------

    /// Override the donor method's signature to `() -> int32`, then drive
    /// the dispatcher and read back the value pushed onto the eval stack.
    let private dispatchIsEnabled
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (donor : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : EvalStackValue
        =
        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes prepared.BaseClassTypes.Int32

        let signature =
            { donor.Signature with
                ParameterTypes = []
                ReturnType = MethodReturnType.Returns int32Handle
            }

        let method =
            { donor with
                Signature = signature
            }

        let ctx = buildContext loggerFactory prepared state method

        match NativeEventSource.tryExecuteQCall "IsEventSourceLoggingEnabled" ctx with
        | Some (NativeHandlerResult.Completed (stateAfter, effect)) ->
            effect |> shouldEqual StepEffect.NoEffect
            let v, _ = IlMachineState.popEvalStack prepared.EntryThread stateAfter
            v
        | other -> failwith $"unexpected IsEventSourceLoggingEnabled result: %O{other}"

    [<Test>]
    let ``IsEventSourceLoggingEnabled: returns 0 when DOTNET_EnableEventLog is unset`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State

        dispatchIsEnabled loggerFactory prepared state donor
        |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``IsEventSourceLoggingEnabled: returns 1 when DOTNET_EnableEventLog=1`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State

        let state = withEnvironment [ "DOTNET_EnableEventLog", "1" ] state

        dispatchIsEnabled loggerFactory prepared state donor
        |> shouldEqual (EvalStackValue.Int32 1)

    [<Test>]
    let ``IsEventSourceLoggingEnabled: returns 0 when DOTNET_EnableEventLog=0`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State

        let state = withEnvironment [ "DOTNET_EnableEventLog", "0" ] state

        dispatchIsEnabled loggerFactory prepared state donor
        |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``IsEventSourceLoggingEnabled: hex 10 means 16 (nonzero, so TRUE)`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State

        let state = withEnvironment [ "DOTNET_EnableEventLog", "10" ] state

        dispatchIsEnabled loggerFactory prepared state donor
        |> shouldEqual (EvalStackValue.Int32 1)

    [<Test>]
    let ``IsEventSourceLoggingEnabled: COMPlus_ fallback wires up`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State
        // No DOTNET_ key — only the legacy COMPlus_ knob is set.
        let state = withEnvironment [ "COMPlus_EnableEventLog", "1" ] state

        dispatchIsEnabled loggerFactory prepared state donor
        |> shouldEqual (EvalStackValue.Int32 1)

    [<Test>]
    let ``IsEventSourceLoggingEnabled: malformed value defaults to FALSE`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State

        let state = withEnvironment [ "DOTNET_EnableEventLog", "garbage" ] state

        dispatchIsEnabled loggerFactory prepared state donor
        |> shouldEqual (EvalStackValue.Int32 0)

    [<Test>]
    let ``LogEventSource: dispatch fires loud failwith (the gate that protects against silent event loss)`` () : unit =
        // This is the test the user explicitly asked for: flip
        // `DOTNET_EnableEventLog=1`, drive the dispatcher into the
        // `LogEventSource` arm, and observe that PawPrint raises rather
        // than silently no-opping. Even on macOS where the host CoreLib
        // omits `XplatEventLogger`, the synthetic context still exercises
        // the dispatcher arm that a Linux guest would reach when
        // `DOTNET_EnableEventLog=1` causes `InitializePersistentListener`
        // to build the LTTng-forwarding listener.
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let state, donor =
            donorConcretizedMethod loggerFactory prepared.BaseClassTypes prepared.State

        let state = withEnvironment [ "DOTNET_EnableEventLog", "1" ] state

        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes prepared.BaseClassTypes.Int32

        let uint16Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes prepared.BaseClassTypes.UInt16

        let charPtrHandle = ConcreteTypeHandle.Pointer uint16Handle

        let signature =
            { donor.Signature with
                ParameterTypes = [ int32Handle ; charPtrHandle ; charPtrHandle ; charPtrHandle ]
                ReturnType = MethodReturnType.Void
            }

        let method =
            { donor with
                Signature = signature
            }

        let ctx = buildContext loggerFactory prepared state method

        let ex =
            Assert.Throws (fun () ->
                NativeEventSource.tryExecuteQCall "LogEventSource" ctx
                |> ignore<NativeHandlerResult option>
            )

        ex.Message |> shouldContainText "LogEventSource"
        ex.Message |> shouldContainText "DOTNET_EnableEventLog=0"
