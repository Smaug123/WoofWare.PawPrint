namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Direct calls of .NET 11's `ThreadNative_SetWaitSleepJoinState` and
/// `ThreadNative_ClearWaitSleepJoinState`, read back through `ThreadNative_GetThreadState`, the
/// QCall behind `Thread.ThreadState`. Called directly, the pair can be driven through any
/// sequence, which a guest's waits never produce; `ThreadStateObserved.cs` checks the bit against
/// real .NET 11 from a guest that watches another thread wait.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeWaitSleepJoinState =

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
                "WaitSleepJoinStateTest"
                OutputKind.ConsoleApplication
                []
                []
                [ trivialSource ]

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "WaitSleepJoinStateTest.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected program to be ready before Main, got %O{outcome}"

    /// The `System.Threading.Thread` method whose QCall entry point is `entryPoint`, concretised.
    let private threadQCall
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (entryPoint : string)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let threadType =
            baseClassTypes.Corelib.TryGetTopLevelTypeDef "System.Threading" "Thread"
            |> Option.defaultWith (fun () -> failwith "type System.Threading.Thread not found")

        let rawMethod =
            threadType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
                | None -> false
            )
            |> function
                | [ method ] -> method
                | methods ->
                    failwith
                        $"expected one System.Threading.Thread method routing to the %s{entryPoint} QCall, found %d{methods.Length}"

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

        state, threadType, method

    /// Run the QCall `entryPoint` on the entry thread, passing the entry thread's own
    /// `ThreadHandle`, and return the state it completes with.
    let private call
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (threadAddr : ManagedHeapAddress)
        (entryPoint : string)
        (state : IlMachineState)
        : IlMachineState
        =
        let state, threadType, method =
            threadQCall loggerFactory prepared.BaseClassTypes entryPoint state

        let (ManagedHeapAddress addr) = threadAddr

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = prepared.BaseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction =
                    { state.ThreadState.[prepared.EntryThread].MethodState with
                        ExecutingMethod = method
                        Arguments =
                            ImmutableArray.Create<CliType> (
                                CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64<int> addr)))
                            )
                    }
                TargetAssembly = prepared.BaseClassTypes.Corelib
                TargetType = threadType
            }

        match NativeThreading.tryExecuteQCall entryPoint ctx with
        | Some (NativeHandlerResult.Completed (state, effect)) ->
            effect |> shouldEqual StepEffect.NoEffect
            state
        | Some other -> failwith $"unexpected %s{entryPoint} execution result: %O{other}"
        | None -> failwith $"%s{entryPoint} QCall did not match"

    [<Test>]
    let ``Thread.ThreadState reports WaitSleepJoin exactly while the last of Set and Clear was Set`` () : unit =
        if FrameworkUnderTest.runtime () <> EmulatedRuntime.Net11 then
            Assert.Ignore "Only .NET 11's CoreLib declares ThreadNative_SetWaitSleepJoinState and its Clear"

        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareProgram loggerFactory

        let threadAddr, initial =
            IlMachineState.getOrAllocateManagedThreadObject
                loggerFactory
                prepared.BaseClassTypes
                prepared.EntryThread
                prepared.State

        let waitSleepJoin (state : IlMachineState) : bool =
            let state =
                call loggerFactory prepared threadAddr "ThreadNative_GetThreadState" state

            match IlMachineState.popEvalStack prepared.EntryThread state |> fst with
            | EvalStackValue.Int32 (Int32Source.Verbatim bits) ->
                enum<System.Threading.ThreadState> bits
                &&& System.Threading.ThreadState.WaitSleepJoin = System.Threading.ThreadState.WaitSleepJoin
            | other -> failwith $"ThreadNative_GetThreadState returned %O{other}"

        waitSleepJoin initial |> shouldEqual false

        // `true` is a Set, `false` a Clear. Each is idempotent, as CoreCLR's `SetThreadState` and
        // `ResetThreadState` are.
        let property (calls : bool list) : unit =
            calls
            |> List.fold
                (fun state isSet ->
                    let entryPoint =
                        if isSet then
                            "ThreadNative_SetWaitSleepJoinState"
                        else
                            "ThreadNative_ClearWaitSleepJoinState"

                    let state = call loggerFactory prepared threadAddr entryPoint state
                    waitSleepJoin state |> shouldEqual isSet
                    state
                )
                initial
            |> ignore

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 50,
            Prop.forAll (Arb.fromGen (ArbMap.defaults |> ArbMap.generate<bool list>)) property
        )
