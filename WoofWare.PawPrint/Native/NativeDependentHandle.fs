namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeDependentHandle =
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
          "System.Runtime",
          "DependentHandle",
          "InternalAlloc",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let target =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue "DependentHandle.InternalAlloc"

            let dependent =
                instruction.Arguments.[1]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue "DependentHandle.InternalAlloc"

            let handle, gcHandles =
                state.GcHandles
                |> GcHandleRegistry.allocateDependent GcHandleOwner.GuestAllocated target dependent

            let state =
                { state with
                    GcHandles = gcHandles
                }

            let state = NativeCall.pushGcHandleAddress handle ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime",
          "DependentHandle",
          "InternalGetTarget",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Object) ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "DependentHandle.InternalGetTarget"

            let target = state.GcHandles |> GcHandleRegistry.target handle

            let state = NativeCall.pushObjectTarget target ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime",
          "DependentHandle",
          "InternalGetDependent",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Object) ->
            let operation = "DependentHandle.InternalGetDependent"

            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue operation

            let cell = state.GcHandles |> GcHandleRegistry.get handle

            // CoreCLR's InternalGetDependent returns the dependent only when the target is
            // non-null; once the target has been cleared (or was never set), the dependent
            // is unobservable through this getter. Mirror that here so guests cannot read
            // a stale dependent through the direct accessor either.
            let dependent =
                match cell.Kind with
                | GcHandleKind.Dependent ->
                    match cell.Target with
                    | Some _ -> cell.Dependent
                    | None -> None
                | other -> failwith $"%s{operation}: handle %O{handle} is %O{other}, not Dependent"

            let state = NativeCall.pushObjectTarget dependent ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime",
          "DependentHandle",
          "InternalGetTargetAndDependent",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Object) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Object) ->
            let operation = "DependentHandle.InternalGetTargetAndDependent"

            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue operation

            let dependentOut =
                NativeCall.managedPointerOfPointerArgument operation "out dependent" instruction.Arguments.[1]

            let cell = state.GcHandles |> GcHandleRegistry.get handle

            let target, dependent =
                match cell.Kind with
                | GcHandleKind.Dependent ->
                    // CoreCLR's InternalGetTargetAndDependent returns (target, dependent) atomically;
                    // if the target has been cleared, both come back as null. PawPrint has no GC,
                    // so there is no race to defend against, but we preserve the "null target ⇒
                    // null dependent" projection so guests cannot observe a dangling pair.
                    match cell.Target with
                    | Some _ -> cell.Target, cell.Dependent
                    | None -> None, None
                | other -> failwith $"%s{operation}: handle %O{handle} is %O{other}, not Dependent"

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    dependentOut
                    (CliType.ObjectRef dependent)

            let state = NativeCall.pushObjectTarget target ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime",
          "DependentHandle",
          "InternalSetDependent",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Void ->
            let operation = "DependentHandle.InternalSetDependent"

            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue operation

            let dependent =
                instruction.Arguments.[1]
                |> EvalStackValue.ofCliType
                |> NativeCall.objectTargetOfEvalStackValue operation

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.setDependent handle dependent
                }

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime",
          "DependentHandle",
          "InternalSetTargetToNull",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "DependentHandle.InternalSetTargetToNull"

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.setTarget handle None
                }

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime",
          "DependentHandle",
          "InternalFree",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            let handle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue "DependentHandle.InternalFree"

            let state =
                { state with
                    GcHandles = state.GcHandles |> GcHandleRegistry.free handle
                }

            // CoreCLR returns false if the handle could not be freed without a GC transition.
            // PawPrint has no GC and free always succeeds, so we report true.
            let state = IlMachineState.pushToEvalStack (CliType.ofBool true) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None
