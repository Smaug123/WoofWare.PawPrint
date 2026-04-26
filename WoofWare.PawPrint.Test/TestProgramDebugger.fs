namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestProgramDebugger =
    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2

    let private genExitCode : Gen<int> = Gen.elements [ -2 ; -1 ; 0 ; 1 ; 2 ; 3 ]

    [<RequireQualifiedAccess>]
    type private OutcomeSignature =
        | ExitCode of int
        | GuestUnhandledException

    let private sourceForExitCode (exitCode : int) : string =
        $"""
class Program
{{
    static int Main(string[] args)
    {{
        return %d{exitCode};
    }}
}}
"""

    let private sourceForWorkerExitCode (exitCode : int) : string =
        $"""
using System.Threading;

class Program
{{
    static int result;

    static void Worker()
    {{
        result = %d{exitCode};
    }}

    static int Main(string[] args)
    {{
        Thread thread = new Thread(Worker);
        thread.Start();
        thread.Join();
        return result;
    }}
}}
"""

    let private exceptionSource =
        """
using System;

class Program
{
    static int Main(string[] args)
    {
        throw new InvalidOperationException("boom");
    }
}
"""

    let private outcomeSignature (outcome : RunOutcome) : OutcomeSignature =
        match outcome with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 i :: _ -> OutcomeSignature.ExitCode i
            | other -> failwith $"Expected int32 exit stack, got %O{other}"
        | RunOutcome.GuestUnhandledException _ -> OutcomeSignature.GuestUnhandledException

    let private stepToCompletion
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (impls : NativeImpls)
        (prepared : Program.PreparedProgram)
        : RunOutcome
        =
        let rec loop (remainingSteps : int) (prepared : Program.PreparedProgram) : RunOutcome =
            if remainingSteps = 0 then
                failwith "Prepared debugger stepper did not finish within the step limit"

            match Program.stepPrepared loggerFactory logger impls prepared with
            | Program.ProgramStepOutcome.Completed outcome -> outcome
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
                failwith $"Prepared debugger stepper deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _)
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> loop (remainingSteps - 1) prepared

        loop 5000 prepared

    [<Test>]
    let ``Prepared debugger stepping agrees with normal run`` () : unit =
        let mutable directReturnCount = 0
        let mutable workerThreadCount = 0
        let mutable unhandledExceptionCount = 0

        let assertDebuggerMatchesNormalRun (source : string) : unit =
            let image = Roslyn.compile [ source ]

            let dotnetRuntimes =
                DotnetRuntime.SelectForDll typeof<RunResult>.Assembly.Location
                |> ImmutableArray.CreateRange

            let impls = MockEnv.make ()

            let _, normalLoggerFactory = LoggerFactory.makeTest ()
            use _normalLoggerFactoryResource = normalLoggerFactory

            let normalOutcome =
                use stream = new MemoryStream (image)
                Program.run normalLoggerFactory (Some "DebuggerProperty.cs") stream dotnetRuntimes impls []

            let _, debuggerLoggerFactory = LoggerFactory.makeTest ()
            use _debuggerLoggerFactoryResource = debuggerLoggerFactory
            let logger = debuggerLoggerFactory.CreateLogger "TestProgramDebugger"

            let debuggerOutcome =
                use stream = new MemoryStream (image)

                match
                    Program.prepare debuggerLoggerFactory (Some "DebuggerProperty.cs") stream dotnetRuntimes impls []
                with
                | Program.ProgramStartResult.Ready prepared ->
                    stepToCompletion debuggerLoggerFactory logger impls prepared
                | Program.ProgramStartResult.CompletedBeforeMain outcome -> outcome

            outcomeSignature debuggerOutcome |> shouldEqual (outcomeSignature normalOutcome)

        let property (expectedExitCode : int) : unit =
            directReturnCount <- directReturnCount + 1
            assertDebuggerMatchesNormalRun (sourceForExitCode expectedExitCode)

            workerThreadCount <- workerThreadCount + 1
            assertDebuggerMatchesNormalRun (sourceForWorkerExitCode expectedExitCode)

        Check.One (config, Prop.forAll (Arb.fromGen genExitCode) property)

        unhandledExceptionCount <- unhandledExceptionCount + 1
        assertDebuggerMatchesNormalRun exceptionSource

        directReturnCount > 0 |> shouldEqual true
        workerThreadCount > 0 |> shouldEqual true
        unhandledExceptionCount > 0 |> shouldEqual true
