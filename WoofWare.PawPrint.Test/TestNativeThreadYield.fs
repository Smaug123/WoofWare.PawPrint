namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Direct-call test for the `ThreadNative_YieldThread` QCall handler. Pins the
/// outcome shape promised by the `WhatWeDid.VoluntaryYield` design: the handler
/// must report `NativeHandlerResult.Yielded` (so the dispatcher reports
/// `WhatWeDid.VoluntaryYield`) and must push Int32 0 (Interop.BOOL.FALSE) as
/// the return value. See `NativeThreading.fs` for the reasoning behind FALSE
/// over TRUE under the current `chooseNext` contract.
[<TestFixture>]
module TestNativeThreadYield =

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
            Roslyn.compileAssemblyWithResources "ThreadYieldTest" OutputKind.ConsoleApplication [] [] [ trivialSource ]

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some "ThreadYieldTest.cs") peImage (HostConfig.Default dotnetRuntimes) with
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

    let private yieldInternalMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let threadType =
            requiredTopLevelType baseClassTypes.Corelib "System.Threading" "Thread"

        let rawMethod =
            threadType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import ->
                    import.ModuleName = "QCall"
                    && import.EntryPointName = "ThreadNative_YieldThread"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point ThreadNative_YieldThread not found on System.Threading.Thread"
                | methods ->
                    failwith
                        $"QCall entry point ThreadNative_YieldThread was ambiguous on System.Threading.Thread: %d{methods.Length} matches"

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

    [<Test>]
    let ``ThreadNative_YieldThread reports Yielded and pushes Interop.BOOL.FALSE`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory
        let baseClassTypes = prepared.BaseClassTypes

        let state, threadType, qCallMethod =
            yieldInternalMethod loggerFactory baseClassTypes prepared.State

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = qCallMethod
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
            match NativeThreading.tryExecuteQCall "ThreadNative_YieldThread" ctx with
            | Some (NativeHandlerResult.Yielded (state, reportsSwitch, effect)) ->
                // The handler must not emit any externally-observable effect — Thread.Yield is
                // a scheduler hint, not an I/O operation. The dispatcher will translate
                // `Yielded` into `ExecutionResult.Stepped (_, WhatWeDid.VoluntaryYield _, effect)`,
                // so any non-`NoEffect` here would leak through to the driver.
                effect |> shouldEqual StepEffect.NoEffect
                // `Thread.Yield()` returns Interop.BOOL, so the handler owes the scheduler an
                // optimistic slot to rewrite with the real answer. Asserting the flag here
                // pins the handler's half of the contract; `Scheduler.onStepOutcome` owns the
                // rewrite.
                reportsSwitch |> shouldEqual true
                state
            | Some other -> failwith $"unexpected ThreadNative_YieldThread execution result: %O{other}"
            | None -> failwith "ThreadNative_YieldThread QCall did not match"

        let returnValue, _ = IlMachineState.popEvalStack prepared.EntryThread stateAfter

        // Interop.BOOL.FALSE is Int32 0, so the IL caller's
        // `YieldInternal() != Interop.BOOL.FALSE` evaluates to `false`.
        //
        // This is the handler's *optimistic* push, not the final answer: the handler cannot
        // know whether a switch will happen, so it pushes FALSE and
        // `Scheduler.onStepOutcome` rewrites the slot to TRUE iff it charges a yield debt.
        // Here that rewrite has deliberately not run — we invoked the QCall directly rather
        // than going through the driver — so FALSE is what we must see. It is also the
        // correct *final* answer for this state, since the entry thread is the only Runnable
        // thread and a yield with nobody to yield to switches to nobody.
        // `TestSchedulerYieldDebt` covers the TRUE case, where a peer is Runnable.
        returnValue |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 0))
