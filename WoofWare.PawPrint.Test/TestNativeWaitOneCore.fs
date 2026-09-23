namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Direct-call test for the `WaitHandle_WaitOneCore` QCall handler's third argument,
/// `useTrivialWaits`. CoreCLR waits with `WaitMode_None` rather than `WaitMode_Alertable`
/// when it is set, and no guest can yet observe the difference except through
/// `Thread.ThreadState`, so this pins the decoding directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeWaitOneCore =

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
            Roslyn.compileAssemblyWithResources "WaitOneCoreTest" OutputKind.ConsoleApplication [] [] [ trivialSource ]

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some "WaitOneCoreTest.cs") peImage (HostConfig.Default dotnetRuntimes) with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected program to be ready before Main, got %O{outcome}"

    let private waitOneCoreMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let waitHandleType =
            baseClassTypes.Corelib.TryGetTopLevelTypeDef "System.Threading" "WaitHandle"
            |> Option.defaultWith (fun () -> failwith "type System.Threading.WaitHandle not found")

        let rawMethod =
            waitHandleType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = "WaitHandle_WaitOneCore"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point WaitHandle_WaitOneCore not found on System.Threading.WaitHandle"
                | methods ->
                    failwith
                        $"QCall entry point WaitHandle_WaitOneCore was ambiguous on System.Threading.WaitHandle: %d{methods.Length} matches"

        let state, method, _declaringType =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        state, waitHandleType, method

    /// Park the entry thread in an infinite wait on an unsignalled event, passing
    /// `useTrivialWaits`, and report the status it parks in.
    let private parkedStatus (useTrivialWaits : int) : WaitHandleId * ThreadStatus =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory
        let baseClassTypes = prepared.BaseClassTypes

        let state, waitHandleType, qCallMethod =
            waitOneCoreMethod loggerFactory baseClassTypes prepared.State

        let id, state = WaitHandle.createEvent false EventResetMode.Auto state

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = qCallMethod
                Arguments =
                    ImmutableArray.Create (
                        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.WaitHandlePtr id)),
                        CliType.Numeric (CliNumericType.Int32 System.Threading.Timeout.Infinite),
                        CliType.Numeric (CliNumericType.Int32 useTrivialWaits)
                    )
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = waitHandleType
            }

        match NativeWaitHandle.tryExecuteQCall "WaitHandle_WaitOneCore" ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> id, state.ThreadState.[prepared.EntryThread].Status
        | Some other -> failwith $"unexpected WaitHandle_WaitOneCore execution result: %O{other}"
        | None -> failwith "WaitHandle_WaitOneCore QCall did not match"

    [<Test>]
    let ``An ordinary wait parks alertably`` () : unit =
        let id, status = parkedStatus 0

        status
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None, WaitAlertability.Alertable))

    [<Test>]
    let ``A trivial wait parks non-alertably`` () : unit =
        let id, status = parkedStatus 1

        status
        |> shouldEqual (ThreadStatus.BlockedOnWaitHandle (id, None, WaitAlertability.NonAlertable))
