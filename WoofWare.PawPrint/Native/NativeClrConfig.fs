namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeClrConfig =
    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "ClrConfig_GetConfigBoolValue",
          "System.Private.CoreLib",
          "CLRConfig",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          // CoreLib declares `[return: MarshalAs(UnmanagedType.Bool)] bool`; the QCall
          // PInvoke stub presents this to us as a 4-byte Int32 (Win32 BOOL).
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "ClrConfig_GetConfigBoolValue"

            // Read the LPCWSTR knob name for diagnostics / future env-routing, even
            // though the hard-coded "never set" answer doesn't depend on it. Forces
            // a UTF-16 walk at the boundary so malformed pointers fail loudly here
            // rather than later.
            let namePtr =
                NativeCall.managedPointerOfPointerArgument operation "name" instruction.Arguments.[0]

            let _name =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            // CoreCLR `clrconfignative.cpp:11` initialises both *exist and the return
            // value to FALSE, and only flips them to TRUE when
            // `Configuration::GetKnobStringValue(name)` finds an override (DOTNET_<name>
            // env var, legacy COMPlus_<name>, or runtimeconfig). PawPrint reports "no
            // knob is set" for every name in its deterministic CPU model. The only
            // caller today, `AutoreleasePool.CheckEnableAutoreleasePool`, short-circuits
            // to `false` on `exist=false`. See issue #609 for the gaps a future
            // widening would close.
            let existOutPtr =
                NativeCall.managedPointerOfPointerArgument operation "exist" instruction.Arguments.[1]

            let int32Zero = CliType.Numeric (CliNumericType.Int32 0)

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state existOutPtr int32Zero

            state
            |> IlMachineState.pushToEvalStack int32Zero ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None
