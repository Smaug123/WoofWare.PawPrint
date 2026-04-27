namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "GetEnvironmentVariableW",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            let state =
                { state with
                    LastPInvokeError = errorEnvVarNotFound
                }

            state
            |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 0u) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
            |> Some
        | _ -> None
