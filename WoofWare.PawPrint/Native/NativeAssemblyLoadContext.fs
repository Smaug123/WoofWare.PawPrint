namespace WoofWare.PawPrint

/// QCalls declared on `System.Runtime.Loader.AssemblyLoadContext`, which connect a managed load
/// context to the binder that resolves assembly references on its behalf.
[<RequireQualifiedAccess>]
module NativeAssemblyLoadContext =
    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
        | "AssemblyNative_InitializeAssemblyLoadContext",
          "System.Private.CoreLib",
          "System.Runtime.Loader",
          "AssemblyLoadContext",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "AssemblyNative_InitializeAssemblyLoadContext"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            // The context's constructor passes a GC handle to itself, which CoreCLR's binder keeps so
            // that it can raise the context's `Resolving` events. PawPrint's binding never calls back
            // into the guest, so the handle is only resolved, so that a malformed one fails here.
            let contextHandle =
                instruction.Arguments.[0]
                |> EvalStackValue.ofCliType
                |> NativeCall.gcHandleAddressOfEvalStackValue operation

            match GcHandleRegistry.target contextHandle state.GcHandles with
            | Some _ -> ()
            | None -> failwith $"%s{operation}: the context's GC handle %O{contextHandle} has no target"

            // Both flags are marshalled as a Win32 `BOOL`: zero is false, anything else true.
            let representsTpaLoadContext =
                NativeCall.int32Argument operation instruction.Arguments.[1] <> 0

            let isCollectible =
                NativeCall.int32Argument operation instruction.Arguments.[2] <> 0

            let binder =
                match representsTpaLoadContext, isCollectible with
                | true, false -> AssemblyBinder.Default
                // CoreCLR's TPA branch never reads the collectible flag, so this would report a
                // collectible context over the non-collectible default binder. Only CoreLib's
                // `DefaultAssemblyLoadContext` can ask for the TPA binder, and it passes false.
                | true, true ->
                    failwith
                        $"%s{operation}: a context asked for the default binder and to be collectible; only DefaultAssemblyLoadContext can ask for the default binder, and it is not collectible"
                // `LoaderAllocator.isCollectible` assumes one arena; this is where a second begins.
                | false, true ->
                    failwith
                        $"TODO: %s{operation}: a collectible AssemblyLoadContext needs a second loader allocator, and PawPrint has one"
                | false, false ->
                    failwith
                        $"TODO: %s{operation}: a custom AssemblyLoadContext needs a second assembly binder, and PawPrint binds every assembly in the default context"

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.AssemblyBinderPtr binder))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "AssemblyNative_GetLoadContextForAssembly",
          "System.Private.CoreLib",
          "System.Runtime.Loader",
          "AssemblyLoadContext",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "QCallAssembly",
                                                        qCallAssemblyGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.IntPtr) when
            qCallAssemblyGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetLoadContextForAssembly"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            state.LoadedAssembly assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")
            |> ignore<DumpedAssembly>

            // PawPrint binds every assembly, dynamic ones included, in the default binder:
            // `AssemblyNative_InitializeAssemblyLoadContext` refuses to make any other.
            let binder = AssemblyBinder.Default

            // CoreCLR answers the managed context's GC handle for any other binder, and 0 for the
            // default one, which `GetLoadContext` maps to `AssemblyLoadContext.Default`.
            let loadContext =
                match binder with
                | AssemblyBinder.Default -> NativeIntSource.Verbatim 0L

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt loadContext) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None
