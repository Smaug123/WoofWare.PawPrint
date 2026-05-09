namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeException =
    /// CoreCLR's `ExceptionNative_GetMessageFromNativeResources` looks the
    /// message up in the runtime's localised resource table, falling back to
    /// the literal English strings below when the lookup fails. PawPrint has
    /// no resource pipeline, so we always return the fallback string. These
    /// values are byte-for-byte the runtime's own English fallbacks
    /// (`comutilnative.cpp`), so they remain a faithful default.
    let private messageForKind (operation : string) (kind : int) : string =
        match kind with
        | 1 -> "Thread was being aborted."
        | 2 -> "Thread was interrupted from a waiting state."
        | 3 -> "Insufficient memory to continue the execution of the program."
        | other ->
            failwith
                $"%s{operation}: unknown ExceptionMessageKind value %d{other} (expected 1=ThreadAbort, 2=ThreadInterrupted, 3=OutOfMemory)"

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
        | "ExceptionNative_GetMessageFromNativeResources",
          "System.Private.CoreLib",
          "System",
          "Exception",
          "GetMessageFromNativeResources",
          [ _kindType
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "StringHandleOnStack",
                                              stringHandleGenerics) ],
          MethodReturnType.Void when stringHandleGenerics.IsEmpty ->
            let operation = "ExceptionNative_GetMessageFromNativeResources"

            if instruction.Arguments.Length <> 2 then
                failwith
                    $"%s{operation}: expected two native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let kind = NativeCall.int32Argument operation instruction.Arguments.[0]

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retMesg" instruction.Arguments.[1]

            let message = messageForKind operation kind

            let messageAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes message state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some messageAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
