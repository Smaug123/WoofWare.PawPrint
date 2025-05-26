namespace WoofWare.PawPrint.ExternImplementations

open WoofWare.PawPrint

type ISystem_Threading_Monitor =
    /// Signature: (PrimitiveType Object, Byref (PrimitiveType Boolean)) -> Void
    abstract ReliableEnter : ThreadId -> IlMachineState -> ExecutionResult

[<RequireQualifiedAccess>]
module System_Threading_Monitor =
    let passThru : ISystem_Threading_Monitor =
        { new ISystem_Threading_Monitor with
            member _.ReliableEnter currentThread state =
                failwith "TODO" |> Tuple.withRight WhatWeDid.Executed |> ExecutionResult.Stepped
        }

type ISystem_Threading_Monitor_Env =
    abstract System_Threading_Monitor : ISystem_Threading_Monitor

[<RequireQualifiedAccess>]
module ISystem_Threading_Monitor_Env =
    let inline get (env : ISystem_Threading_Monitor_Env) : ISystem_Threading_Monitor = env.System_Threading_Monitor
