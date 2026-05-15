namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeMarshal =
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
          "System.Runtime.InteropServices",
          "Marshal",
          "GetLastPInvokeError",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 state.Kernel.LastPInvokeError) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "GetLastSystemError",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 state.Kernel.LastSystemError) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "SetLastPInvokeError",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "Marshal.SetLastPInvokeError" instruction.Arguments.[0]

            (state.MapKernel (fun kernel ->
                { kernel with
                    LastPInvokeError = error
                }
             ),
             WhatWeDid.Executed)
            |> ExecutionResult.stepped
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "SetLastSystemError",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "Marshal.SetLastSystemError" instruction.Arguments.[0]

            (state.MapKernel (fun kernel ->
                { kernel with
                    LastSystemError = error
                }
             ),
             WhatWeDid.Executed)
            |> ExecutionResult.stepped
            |> Some
        | _ -> None

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "MarshalNative_SizeOfHelper",
          "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            qCallGenerics.IsEmpty
            ->
            let operation = "MarshalNative_SizeOfHelper"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle operation state qCallHandle

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes typeHandle

            let throwIfNotMarshalable =
                match instruction.Arguments.[1] |> EvalStackValue.ofCliType with
                | EvalStackValue.Int32 0 -> false
                | EvalStackValue.Int32 _ -> true
                | other -> failwith $"%s{operation}: expected throwIfNotMarshalable as Int32, got %O{other}"

            match CliType.TryComputeMarshalSize state.ConcreteTypes state._LoadedAssemblies ctx.BaseClassTypes zero with
            | Result.Error reason ->
                failwith
                    $"%s{operation}: refusing to compute unmanaged marshalled size because %s{reason} (throwIfNotMarshalable=%b{throwIfNotMarshalable})"
            | Result.Ok size ->
                let state =
                    IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size.Size)) ctx.Thread state

                (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None
