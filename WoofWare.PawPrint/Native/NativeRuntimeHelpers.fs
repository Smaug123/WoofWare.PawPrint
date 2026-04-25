namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeHelpers =
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
        | "ReflectionInvocation_RunClassConstructor",
          "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "RuntimeHelpers",
          "RunClassConstructor",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              generics) ],
          ConcreteVoid state.ConcreteTypes when generics.IsEmpty ->
            let operation = "ReflectionInvocation_RunClassConstructor"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle operation qCallHandle

            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // Pointer, byref, and array type descriptors have no .cctor; CoreCLR treats this
                // as a no-op. Return immediately.
                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            | ConcreteTypeHandle.Concrete _ ->
                let state, typeInit =
                    IlMachineStateExecution.ensureTypeInitialised
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.Thread
                        typeHandle
                        state

                match typeInit with
                | WhatWeDid.Executed -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
                | WhatWeDid.SuspendedForClassInit ->
                    // The cctor was pushed as a new frame. We must NOT go through the normal
                    // returnStackFrame path (which would pop the cctor frame we just pushed).
                    // Instead, return Stepped directly so the dispatch loop runs the cctor.
                    // When the cctor finishes, returnStackFrame pops it, bringing us back to
                    // this native method frame. executeOneStep re-enters here and
                    // ensureTypeInitialised will return Executed.
                    ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit) |> Some
                | WhatWeDid.ThrowingTypeInitializationException ->
                    (state, WhatWeDid.ThrowingTypeInitializationException)
                    |> ExecutionResult.Stepped
                    |> Some
                | WhatWeDid.BlockedOnClassInit blockedBy ->
                    // Another thread owns this type's .cctor lock. Yield so the scheduler
                    // can run that thread to completion before re-entering.
                    ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy) |> Some
        | _ -> None
