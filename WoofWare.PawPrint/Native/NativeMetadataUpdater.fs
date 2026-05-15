namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeMetadataUpdater =
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
        | "AssemblyNative_IsApplyUpdateSupported",
          "System.Private.CoreLib",
          "System.Reflection.Metadata",
          "MetadataUpdater",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // CoreCLR (assemblynative.cpp) returns CORDebuggerAttached() ||
            // ForceEnc || DebugAssembliesModifiable. PawPrint never enables hot
            // reload, so the answer is always false (BOOL is marshalled as int32
            // by the LibraryImport-generated stub).
            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None
