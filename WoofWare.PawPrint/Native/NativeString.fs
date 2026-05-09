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
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

            state
            |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread
            |> fun state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
            |> Some
        | "System.Private.CoreLib",
          "System",
          "String",
          ".ctor",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char) ],
          MethodReturnType.Void ->
            let operation = "String..ctor(char*)"

            // Newobj-driven constructor frames carry `this` as Arguments.[0]
            // (the placeholder allocated by executeNewobj) and the user-visible
            // char* as Arguments.[1].
            if instruction.Arguments.Length <> 2 then
                failwith
                    $"%s{operation}: expected 2 arguments (this, char*) after matching signature, got %d{instruction.Arguments.Length}"

            let ptr =
                NativeCall.managedPointerOfPointerArgument operation "value" instruction.Arguments.[1]

            // CoreCLR's String.Ctor(char*) returns String.Empty when ptr == null,
            // rather than throwing ArgumentNullException.
            let contents =
                match ptr with
                | ManagedPointerSource.Null -> ""
                | _ -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state ptr

            let newAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

            // Redirect the pending newobj result to our freshly-allocated string;
            // the placeholder allocated by executeNewobj is left as garbage.
            state
            |> IlMachineState.withReplacedConstructedObject newAddr ctx.Thread
            |> fun state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
            |> Some
        | _ -> None
