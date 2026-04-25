namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeEnvironment =
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
          "Environment",
          "GetProcessorCount",
          [],
          ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ->
            let env = ISystem_Environment_Env.get ctx.Implementations
            env.GetProcessorCount ctx.Thread state |> Some
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "get_CurrentManagedThreadId",
          [],
          ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ->
            let env = ISystem_Environment_Env.get ctx.Implementations
            env.GetCurrentManagedThreadId ctx.Thread state |> Some
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "_Exit",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          ConcreteVoid state.ConcreteTypes ->
            let env = ISystem_Environment_Env.get ctx.Implementations
            env._Exit ctx.Thread state |> Some
        | _ -> None
