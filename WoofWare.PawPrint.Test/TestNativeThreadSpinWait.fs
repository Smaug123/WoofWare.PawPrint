namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Direct-call tests for the two halves of Thread's spin-wait surface:
///
/// * `ThreadNative_SpinWait` (the QCall backing `Thread.SpinWait(int)`,
///   reached via either `SpinWaitInternal` or `LongSpinWaitInternal`
///   depending on the guest's iteration count).
/// * `get_OptimalMaxSpinWaitsPerSpinIteration` (the InternalCall backing the
///   internal `Thread.OptimalMaxSpinWaitsPerSpinIteration` property, which
///   ordinary guest code can only reach indirectly through
///   `SpinWait.SpinOnce()` — and only on a simulated multi-processor kernel,
///   since `SpinOnceCore` always takes the yield/sleep branch on a
///   single-processor `Environment.IsSingleProcessor`, PawPrint's default).
///
/// Both handlers are exercised directly here (rather than solely via an
/// end-to-end `sourcesPure` differential test) because
/// `OptimalMaxSpinWaitsPerSpinIteration` is `internal`: no ordinary C# test
/// program can read it, and even if it could, the real CoreCLR value is a
/// host-CPU timing measurement that is not safe to assert exact equality
/// against across machines/CI runs. See `ThreadSpinWait.cs` in `sourcesPure`
/// for the differential coverage of the public `Thread.SpinWait(int)` half,
/// which has no such problem.
[<TestFixture>]
module TestNativeThreadSpinWait =

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

    let private prepareProgram
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (kernelConfig : KernelConfig)
        : Program.PreparedProgram
        =
        let image =
            Roslyn.compileAssemblyWithResources
                "ThreadSpinWaitTest"
                OutputKind.ConsoleApplication
                []
                []
                [ trivialSource ]

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "ThreadSpinWaitTest.cs") peImage dotnetRuntimes kernelConfig None []
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected program to be ready before Main, got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    /// Locate a `System.Threading.Thread` method and concretize it, so a test
    /// can build a `NativeCallContext` around it without going through the
    /// ordinary IL-dispatch path.
    let private concretizedThreadMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (chooseMethod :
            MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list
                -> MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let threadType =
            requiredTopLevelType baseClassTypes.Corelib "System.Threading" "Thread"

        let rawMethod = chooseMethod threadType.Methods

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

        state, threadType, method

    let private spinWaitInternalMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        concretizedThreadMethod
            loggerFactory
            baseClassTypes
            state
            (fun methods ->
                methods
                |> List.filter (fun method ->
                    match method.NativeImport with
                    | Some import -> import.ModuleName = "QCall" && import.EntryPointName = "ThreadNative_SpinWait"
                    | None -> false
                )
                |> function
                    | [] -> failwith "QCall entry point ThreadNative_SpinWait not found on System.Threading.Thread"
                    | methods ->
                        // Two managed methods legitimately share this one QCall entry
                        // point: `SpinWaitInternal` and `LongSpinWaitInternal` (see the
                        // long comment on the QCall handler in `NativeThreading.fs`).
                        // The handler dispatches on entry point + signature shape, not on
                        // which of the two names reached it, so either is a valid stand-in
                        // for this test; pin on the one whose managed name matches the
                        // `[SuppressGCTransition]` fast path so a future test run is
                        // reproducible rather than dependent on list ordering.
                        methods
                        |> List.filter (fun method -> method.Name = "SpinWaitInternal")
                        |> function
                            | [ method ] -> method
                            | _ ->
                                failwith
                                    "expected exactly one SpinWaitInternal method routing to the ThreadNative_SpinWait QCall entry point"
            )

    let private optimalMaxSpinWaitsPerSpinIterationMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        concretizedThreadMethod
            loggerFactory
            baseClassTypes
            state
            (fun methods ->
                methods
                |> List.filter (fun method -> method.Name = "get_OptimalMaxSpinWaitsPerSpinIteration")
                |> function
                    | [ method ] -> method
                    | [] -> failwith "get_OptimalMaxSpinWaitsPerSpinIteration not found on System.Threading.Thread"
                    | methods ->
                        failwith
                            $"get_OptimalMaxSpinWaitsPerSpinIteration was ambiguous on System.Threading.Thread: %d{methods.Length} matches"
            )

    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(100)>]
    [<TestCase(1024)>]
    [<TestCase(-5)>]
    let ``ThreadNative_SpinWait completes as a pure no-op for any iteration count`` (iterations : int) : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory KernelConfig.Default
        let baseClassTypes = prepared.BaseClassTypes

        let state, threadType, qCallMethod =
            spinWaitInternalMethod loggerFactory baseClassTypes prepared.State

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = qCallMethod
                Arguments = ImmutableArray.Create<CliType> (CliType.Numeric (CliNumericType.Int32 iterations))
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = threadType
            }

        // `Thread.SpinWait` returns void and, per the extensive rationale in
        // `NativeThreading.fs`, has no managed-visible side effect at all: the
        // handler must hand back the exact same state it was given, having
        // neither pushed anything to the eval stack nor mutated anything else.
        match NativeThreading.tryExecuteQCall "ThreadNative_SpinWait" ctx with
        | Some (NativeHandlerResult.Completed (stateAfter, effect)) ->
            effect |> shouldEqual StepEffect.NoEffect
            System.Object.ReferenceEquals (stateAfter, state) |> shouldEqual true
        | Some other -> failwith $"unexpected ThreadNative_SpinWait execution result: %O{other}"
        | None -> failwith "ThreadNative_SpinWait QCall did not match"

    [<Test>]
    let ``get_OptimalMaxSpinWaitsPerSpinIteration reports the default kernel value`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory KernelConfig.Default
        let baseClassTypes = prepared.BaseClassTypes

        let state, threadType, internalCallMethod =
            optimalMaxSpinWaitsPerSpinIterationMethod loggerFactory baseClassTypes prepared.State

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = internalCallMethod
                Arguments = ImmutableArray.Empty
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = threadType
            }

        let stateAfter =
            match NativeThreading.tryExecute ctx with
            | Some (NativeHandlerResult.Completed (state, effect)) ->
                effect |> shouldEqual StepEffect.NoEffect
                state
            | Some other -> failwith $"unexpected get_OptimalMaxSpinWaitsPerSpinIteration execution result: %O{other}"
            | None -> failwith "get_OptimalMaxSpinWaitsPerSpinIteration InternalCall did not match"

        let returnValue, _ = IlMachineState.popEvalStack prepared.EntryThread stateAfter

        returnValue
        |> shouldEqual (
            EvalStackValue.Int32 (Int32Source.Verbatim EmulatedKernel.defaultOptimalMaxSpinWaitsPerSpinIteration)
        )

    [<Test>]
    let ``get_OptimalMaxSpinWaitsPerSpinIteration round-trips a host-configured kernel value`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let kernelConfig =
            { KernelConfig.Default with
                OptimalMaxSpinWaitsPerSpinIteration = 3
            }

        let prepared = prepareProgram loggerFactory kernelConfig
        let baseClassTypes = prepared.BaseClassTypes

        let state, threadType, internalCallMethod =
            optimalMaxSpinWaitsPerSpinIterationMethod loggerFactory baseClassTypes prepared.State

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = internalCallMethod
                Arguments = ImmutableArray.Empty
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = threadType
            }

        let stateAfter =
            match NativeThreading.tryExecute ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some other -> failwith $"unexpected get_OptimalMaxSpinWaitsPerSpinIteration execution result: %O{other}"
            | None -> failwith "get_OptimalMaxSpinWaitsPerSpinIteration InternalCall did not match"

        let returnValue, _ = IlMachineState.popEvalStack prepared.EntryThread stateAfter

        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 3))
