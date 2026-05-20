namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeEnvironment =
    /// Read a UTF-16 char* argument, returning None for a null pointer and Some <string> otherwise.
    let private tryReadOptionalUtf16
        (operation : string)
        (argName : string)
        (ctx : NativeCallContext)
        (arg : CliType)
        : string option
        =
        let ptr = NativeCall.managedPointerOfPointerArgument operation argName arg

        match ptr with
        | ManagedPointerSource.Null -> None
        | _ ->
            NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes ctx.State ptr
            |> Some

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
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
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let env = ISystem_Environment_Env.get ctx.Implementations

            env.GetProcessorCount ctx.Thread state
            |> NativeHandlerResult.ofExecutionResult
            |> Some
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "get_CurrentManagedThreadId",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let env = ISystem_Environment_Env.get ctx.Implementations

            env.GetCurrentManagedThreadId ctx.Thread state
            |> NativeHandlerResult.ofExecutionResult
            |> Some
        | "System.Private.CoreLib",
          "System",
          "Environment",
          "_Exit",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let env = ISystem_Environment_Env.get ctx.Implementations
            env._Exit ctx.Thread state |> NativeHandlerResult.ofExecutionResult |> Some
        | "System.Private.CoreLib", "System", "Environment", _, _, _ when
            NativeCall.tryQCallEntryPoint ctx = Some "Environment_FailFast"
            ->
            // QCall lowering of Environment.FailFast(string?, Exception?, string?). The
            // C# source uses LibraryImport with non-blittable string args, so Roslyn
            // emits a marshalling stub whose synthesized name (e.g.
            // `<FailFast>g____PInvoke|N_M`) carries source-generator counters and is
            // not stable across runtime/source-generator versions. Match on the QCall
            // entry-point name (`Environment_FailFast`) instead, then verify the
            // signature shape before reading args.
            //
            // The StackCrawlMarkHandle and ObjectHandleOnStack args are diagnostic-only
            // on the native side (used by CoreCLR to walk the managed stack and capture
            // the exception object); PawPrint surfaces FailFast as an abort outcome and
            // does not yet inspect either, so they're ignored here. `message` and
            // `errorSource` are UTF-16 char* pointers (possibly null).
            let operation = "Environment_FailFast"

            match
                instruction.ExecutingMethod.Signature.ParameterTypes, instruction.ExecutingMethod.Signature.ReturnType
            with
            | [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                  "System.Runtime.CompilerServices",
                                                  "StackCrawlMarkHandle",
                                                  stackMarkGenerics)
                ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
                ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                  "System.Runtime.CompilerServices",
                                                  "ObjectHandleOnStack",
                                                  objHandleGenerics)
                ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16) ],
              MethodReturnType.Void when stackMarkGenerics.IsEmpty && objHandleGenerics.IsEmpty ->
                if instruction.Arguments.Length <> 4 then
                    failwith
                        $"%s{operation}: expected four native arguments after matching signature, got %d{instruction.Arguments.Length}"

                let message = tryReadOptionalUtf16 operation "message" ctx instruction.Arguments.[1]

                let errorSource =
                    tryReadOptionalUtf16 operation "errorSource" ctx instruction.Arguments.[3]

                let env = ISystem_Environment_Env.get ctx.Implementations

                env.FailFast ctx.Thread message errorSource state
                |> NativeHandlerResult.ofExecutionResult
                |> Some
            | paramTypes, returnType ->
                failwith
                    $"%s{operation}: matched QCall entry point but signature unexpected: params=%A{paramTypes}, return=%A{returnType}"
        | _ -> None
