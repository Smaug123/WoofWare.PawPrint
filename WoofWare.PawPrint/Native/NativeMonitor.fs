namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeMonitor =
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
          "System.Threading",
          "Monitor",
          "ReliableEnter",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ],
          MethodReturnType.Void -> System_Threading_Monitor.ReliableEnter ctx.Thread state |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "Exit",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Void -> System_Threading_Monitor.Exit ctx.Thread state |> Some
        | _ -> None
