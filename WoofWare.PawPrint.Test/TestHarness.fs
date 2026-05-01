namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

/// Result of executing (some steps of) the program under PawPrint.
type RunResult =
    {
        /// Value that was left on the evaluation stack when execution stopped, **if**
        /// the program executed a `ret` that produced a value and PawPrint
        /// subsequently pushed it onto the stack.  This is only an early-stage
        /// approximation: once PawPrint supports a proper process-exit story we
        /// can promote this to a real exit–code.
        ExitCode : int option

        /// Final interpreter state after we stopped executing.
        FinalState : IlMachineState
    }

[<RequireQualifiedAccess>]
module MockEnv =
    let private invariantGlobalizationEnv : Map<string, string> =
        [ "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT", "1" ] |> Map.ofList

    let makeWithEnvironment (environment : Map<string, string>) : NativeImpls =
        let environment =
            // Tests may intentionally override the invariant-globalization default
            // when they need full control of the guest environment.
            (invariantGlobalizationEnv, environment)
            ||> Map.fold (fun acc variable value -> acc |> Map.add variable value)

        let tryGetEnvironmentVariable (variable : string) : string option = environment |> Map.tryFind variable

        {
            System_Environment =
                { System_EnvironmentMock.Empty with
                    GetProcessorCount =
                        fun thread state ->
                            state
                            |> IlMachineThreadState.pushToEvalStack' (EvalStackValue.Int32 1) thread
                            |> Tuple.withRight WhatWeDid.Executed
                            |> ExecutionResult.Stepped
                    GetCurrentManagedThreadId =
                        fun thread state ->
                            state
                            |> IlMachineThreadState.pushToEvalStack'
                                (EvalStackValue.Int32 (IlMachineRuntimeMetadata.getCurrentManagedThreadId thread state))
                                thread
                            |> Tuple.withRight WhatWeDid.Executed
                            |> ExecutionResult.Stepped
                    TryGetEnvironmentVariable = tryGetEnvironmentVariable
                }
        }

    let make () : NativeImpls = makeWithEnvironment Map.empty

type EndToEndTestCase =
    {
        FileName : string
        ExpectedReturnCode : int
        NativeImpls : NativeImpls
        ExpectsUnhandledException : bool
    }
