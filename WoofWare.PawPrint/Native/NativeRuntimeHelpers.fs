namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeHelpers =
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
          "System.Runtime.CompilerServices",
          "RuntimeHelpers",
          "RunClassConstructor",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              generics) ],
          ConcreteVoid state.ConcreteTypes when generics.IsEmpty ->
            // QCall: triggers the .cctor for the type identified by the QCallTypeHandle argument.
            // Extract the ConcreteTypeHandle from the QCallTypeHandle's _handle field, then
            // ensure the type is initialised.
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let arg, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                match arg with
                | EvalStackValue.UserDefinedValueType vt ->
                    // QCallTypeHandle._handle is typed as IntPtr (a primitive-like wrapper),
                    // so the dereferenced field contents are wrapped; unwrap to the inner NativeInt.
                    match CliValueType.DereferenceField "_handle" vt |> CliType.unwrapPrimitiveLike with
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
                    | other -> failwith $"RunClassConstructor: expected TypeHandlePtr in _handle field, got %O{other}"
                | other -> failwith $"RunClassConstructor: expected QCallTypeHandle value type, got %O{other}"

            match typeHandleTarget with
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                failwith
                    $"TODO: RuntimeHelpers.RunClassConstructor for open generic type definition %O{typeHandleTarget}"
            | RuntimeTypeHandleTarget.Closed concreteTypeHandle ->
                match concreteTypeHandle with
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
                            concreteTypeHandle
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
