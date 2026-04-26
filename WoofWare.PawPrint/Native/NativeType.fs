namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeType =
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
          "Type",
          "GetField",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.String ; ty ],
          MethodReturnType.Returns ret ->
            let ty = AllConcreteTypes.lookup ty state.ConcreteTypes |> Option.get
            let ret = AllConcreteTypes.lookup ret state.ConcreteTypes |> Option.get

            match ty.Namespace, ty.Name, ty.Generics.IsEmpty, ret.Namespace, ret.Name, ret.Generics.IsEmpty with
            | "System.Reflection", "BindingFlags", true, "System.Reflection", "FieldInfo", true ->
                failwith "TODO: GetField"
            | _ -> failwith "unexpected signature for Type.GetField"
        | _ -> None
