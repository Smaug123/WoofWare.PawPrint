namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint

type ISystem_Environment =
    /// The expected side-effect is to push an Int32 to the stack.
    abstract GetProcessorCount : ThreadId -> IlMachineState -> ExecutionResult
    /// The expected side effect is to terminate execution.
    abstract _Exit : ThreadId -> IlMachineState -> ExecutionResult

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
                |> ExecutionResult.Stepped

            member _._Exit currentThread state =
                ExecutionResult.Terminated (state, currentThread)
        }

type ISystem_Environment_Env =
    abstract System_Environment : ISystem_Environment

[<RequireQualifiedAccess>]
module ISystem_Environment_Env =
    let inline get (env : ISystem_Environment_Env) : ISystem_Environment = env.System_Environment
