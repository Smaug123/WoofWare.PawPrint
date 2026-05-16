namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeMarshal =
    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
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
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "GetLastSystemError",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 state.Kernel.LastSystemError) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "SetLastPInvokeError",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "Marshal.SetLastPInvokeError" instruction.Arguments.[0]

            state.MapKernel (fun kernel ->
                { kernel with
                    LastPInvokeError = error
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "SetLastSystemError",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "Marshal.SetLastSystemError" instruction.Arguments.[0]

            state.MapKernel (fun kernel ->
                { kernel with
                    LastSystemError = error
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
            | Result.Error (MarshalSizeError.NotMarshalable _) when throwIfNotMarshalable ->
                // CoreCLR's `MarshalNative_SizeOfHelper` (marshalnative.cpp:150) throws
                // `ArgumentException` (resource `IDS_CANNOT_MARSHAL`) for types it can't
                // marshal as unmanaged structures when `throwIfNotMarshalable` is set.
                // Mirror that with a guest exception so the caller's `try/catch` can handle it.
                NativeHandlerResult.raiseException ctx.BaseClassTypes.ArgumentException state
                |> Some
            | Result.Error (MarshalSizeError.NotMarshalable reason) ->
                // `throwIfNotMarshalable=false` path: CoreCLR falls through to
                // `MethodTable::GetNativeSize` and returns whatever the type loader recorded.
                // PawPrint doesn't compute that value yet, so surface a host failure with a
                // clear TODO until a real caller forces us to model it.
                failwith
                    $"TODO %s{operation}: throwIfNotMarshalable=false fall-through to GetNativeSize is not implemented; type rejected because %s{reason}"
            | Result.Error (MarshalSizeError.NotImplemented reason) ->
                // PawPrint hasn't implemented this marshalling case; CoreCLR would compute a
                // size successfully. Surface as a host TODO so the missing case is visible.
                failwith
                    $"TODO %s{operation}: unimplemented marshalling case (throwIfNotMarshalable=%b{throwIfNotMarshalable}): %s{reason}"
            | Result.Ok size ->
                let state =
                    IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size.Size)) ctx.Thread state

                NativeHandlerResult.completed state |> Some
        | _ -> None
