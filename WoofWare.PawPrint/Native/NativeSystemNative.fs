namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeSystemNative =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.NativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    let private pushInt32 (value : int) (ctx : NativeCallContext) : ExecutionResult =
        ctx.State
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) ctx.Thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            trySystemNativeEntryPoint ctx,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some ("SystemNative_LChflagsCanSetHiddenFlag" | "SystemNative_CanGetHiddenFlag"),
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // PawPrint does not model Unix file flags. Report that hidden flags
            // are unsupported so CoreLib follows the portable attribute path.
            pushInt32 0 ctx |> Some
        | Some "SystemNative_GetErrNo",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pushInt32 state.LastSystemError ctx |> Some
        | Some "SystemNative_SetErrNo",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[0] with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"SystemNative_SetErrNo: expected Int32 error, got %O{other}"

            ({ state with
                LastSystemError = error
             },
             WhatWeDid.Executed)
            |> ExecutionResult.Stepped
            |> Some
        | _ -> None
