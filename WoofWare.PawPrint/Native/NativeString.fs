namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module NativeString =
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
          "System",
          "String",
          "FastAllocateString",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.String) ->
            if instruction.Arguments.Length <> 1 then
                failwith
                    $"String.FastAllocateString: expected one native argument after matching signature, got %d{instruction.Arguments.Length}"

            let length =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[0] with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"String.FastAllocateString: expected int32 length, got %O{other}"

            if length < 0 then
                failwith "TODO: String.FastAllocateString with negative length should throw OutOfMemoryException"

            let contents = String (char 0, length)

            let addr, state =
                IlMachineRuntimeMetadata.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

            state
            |> IlMachineThreadState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread
            |> fun state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
            |> Some
        | _ -> None
