namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeMonitor =
    /// Match Monitor's nested int32 enums (`EnterHelperResult`, `LeaveHelperAction`).
    /// They live in CoreLib with empty namespace (nested types) and matching simple name.
    let private (|MonitorNestedEnum|_|) (concreteTypes : AllConcreteTypes) (enumName : string) handle =
        match handle with
        | ConcreteType concreteTypes (asm, "", name, generics) when
            asm = "System.Private.CoreLib" && name = enumName && generics.IsEmpty
            ->
            Some ()
        | _ -> None

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
          "TryEnter_FastPath",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            System_Threading_Monitor.TryEnter_FastPath ctx.BaseClassTypes ctx.Thread state
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "TryEnter_FastPath_WithTimeout",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (MonitorNestedEnum state.ConcreteTypes "EnterHelperResult") ->
            System_Threading_Monitor.TryEnter_FastPath_WithTimeout ctx.BaseClassTypes ctx.Thread state
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "Exit_FastPath",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (MonitorNestedEnum state.ConcreteTypes "LeaveHelperAction") ->
            System_Threading_Monitor.Exit_FastPath ctx.BaseClassTypes ctx.Thread state
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "IsEnteredNative",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            System_Threading_Monitor.IsEnteredNative ctx.BaseClassTypes ctx.Thread state
            |> Some
        | _ -> None
