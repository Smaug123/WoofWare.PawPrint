namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeDebugger =
    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "DebugDebugger_IsManagedDebuggerAttached",
          "System.Private.CoreLib",
          "System.Diagnostics",
          "Debugger",
          "IsManagedDebuggerAttached",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // .NET 10 QCall backing Debugger.get_IsAttached. Returns nonzero when a managed
            // debugger is attached; PawPrint never has one attached.
            let isAttached = if DebuggerState.isAttached state.Debugger then 1 else 0

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 isAttached)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Diagnostics",
          "Debugger",
          "get_IsAttached",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // Pre-.NET 10 InternalCall path. .NET 10 routes the same observation through the
            // DebugDebugger_IsManagedDebuggerAttached QCall above.
            let isAttached = DebuggerState.isAttached state.Debugger

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool isAttached) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
