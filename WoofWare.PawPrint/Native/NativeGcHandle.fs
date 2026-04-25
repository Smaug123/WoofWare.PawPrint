namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeGcHandle =
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
        | "QCall_GetGCHandleForTypeHandle",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.InteropServices",
                                              "GCHandleType",
                                              gcHandleTypeGenerics) ],
          ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr when
            qCallGenerics.IsEmpty && gcHandleTypeGenerics.IsEmpty
            ->
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let gcHandleType = instruction.Arguments.[1] |> EvalStackValue.ofCliType

            let typeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle "QCall_GetGCHandleForTypeHandle" qCallHandle

            let kind =
                NativeCall.gcHandleKindOfEvalStackValue "QCall_GetGCHandleForTypeHandle" gcHandleType

            let handle, gcHandles =
                state.GcHandles
                |> GcHandleRegistry.allocate kind (GcHandleOwner.TypeAssociated typeHandle) None

            let state =
                { state with
                    GcHandles = gcHandles
                }

            let state = NativeCall.pushGcHandleAddress handle ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "QCall_FreeGCHandleForTypeHandle",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          returnType when qCallGenerics.IsEmpty ->
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let objHandle = instruction.Arguments.[1] |> EvalStackValue.ofCliType

            // Extract this for validation. CoreCLR uses the type's loader allocator to
            // unregister the handle before destroying it; PawPrint has one process-wide
            // handle registry, but keeping the type association visible makes a future
            // collector/loader model easier to add.
            NativeCall.qCallTypeHandleToConcreteTypeHandle "QCall_FreeGCHandleForTypeHandle" qCallHandle
            |> ignore

            let handle =
                NativeCall.gcHandleAddressOfEvalStackValue "QCall_FreeGCHandleForTypeHandle" objHandle

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                }

            match returnType with
            | ConcreteVoid state.ConcreteTypes -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            | ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)) ctx.Thread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.Stepped
                |> Some
            | other -> failwith $"QCall_FreeGCHandleForTypeHandle: unexpected return type %O{other}"
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
          "System.Runtime.InteropServices",
          "GCHandle",
          "_InternalAlloc",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.InteropServices",
                                              "GCHandleType",
                                              gcHandleTypeGenerics) ],
          ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr when gcHandleTypeGenerics.IsEmpty ->
            let target =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue "GCHandle._InternalAlloc"

            let kind =
                instruction.Arguments.[1]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleKindOfEvalStackValue "GCHandle._InternalAlloc"

            let handle, gcHandles =
                state.GcHandles
                |> GcHandleRegistry.allocate kind GcHandleOwner.GuestAllocated target

            let state =
                { state with
                    GcHandles = gcHandles
                }

            let state = NativeCall.pushGcHandleAddress handle ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "GCHandle",
          "_InternalFree",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "GCHandle._InternalFree"

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                }

            let state = IlMachineState.pushToEvalStack (CliType.ofBool true) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "GCHandle",
          "_InternalFreeWithGCTransition",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          ConcreteVoid state.ConcreteTypes ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "GCHandle._InternalFreeWithGCTransition"

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                }

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "GCHandle",
          "InternalSet",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          ConcreteVoid state.ConcreteTypes ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "GCHandle.InternalSet"

            let target =
                instruction.Arguments.[1]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue "GCHandle.InternalSet"

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.setTarget handle target
                }

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "GCHandle",
          "InternalCompareExchange",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "GCHandle.InternalCompareExchange"

            let value =
                instruction.Arguments.[1]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue "GCHandle.InternalCompareExchange"

            let comparand =
                instruction.Arguments.[2]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue "GCHandle.InternalCompareExchange"

            let oldTarget, gcHandles =
                state.GcHandles |> GcHandleRegistry.compareExchangeTarget handle value comparand

            let state =
                { state with
                    GcHandles = gcHandles
                }

            let state = NativeCall.pushObjectTarget oldTarget ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
