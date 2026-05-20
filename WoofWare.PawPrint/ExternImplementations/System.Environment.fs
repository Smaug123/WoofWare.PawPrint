namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint

type ISystem_Environment =
    /// The expected side-effect is to push an Int32 to the stack.
    abstract GetProcessorCount : ThreadId -> IlMachineState -> ExecutionResult
    /// The expected side-effect is to push an Int32 to the stack.
    abstract GetCurrentManagedThreadId : ThreadId -> IlMachineState -> ExecutionResult
    /// The expected side effect is to terminate execution.
    abstract _Exit : ThreadId -> IlMachineState -> ExecutionResult

    /// Environment.FailFast lowering. The expected side effect is to abort execution
    /// (`ExecutionResult.FailFast`). `message` and `errorSource` are the guest-supplied
    /// diagnostic strings (each may be absent if the guest passed a null pointer).
    abstract FailFast :
        ThreadId -> message : string option -> errorSource : string option -> IlMachineState -> ExecutionResult

[<RequireQualifiedAccess>]
module System_Environment =
    let passThru : ISystem_Environment =
        { new ISystem_Environment with
            member _.GetProcessorCount currentThread state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 System.Environment.ProcessorCount)
                    currentThread
                    state
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped

            member _.GetCurrentManagedThreadId currentThread state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (IlMachineState.getCurrentManagedThreadId currentThread state))
                    currentThread
                    state
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped

            member _._Exit currentThread state =
                // Push the exit code (arg 0) onto the eval stack so the scheduler can report
                // it as the final exit code, then tear the whole process down.
                let state = state |> IlMachineState.loadArgument currentThread 0
                ExecutionResult.ProcessExit (state, currentThread)

            member _.FailFast currentThread message _errorSource state =
                // FailFast aborts the process. We don't load the StackCrawlMark / exception
                // / errorSource arguments onto the eval stack because the caller never
                // returns — the run-loop converts ExecutionResult.FailFast directly to
                // RunOutcome.FailFast for the host to surface.
                ExecutionResult.FailFast (state, currentThread, message)
        }

type ISystem_Environment_Env =
    abstract System_Environment : ISystem_Environment

[<RequireQualifiedAccess>]
module ISystem_Environment_Env =
    let inline get (env : ISystem_Environment_Env) : ISystem_Environment = env.System_Environment
