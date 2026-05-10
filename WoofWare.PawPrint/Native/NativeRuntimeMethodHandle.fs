namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeMethodHandle =
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
          "RuntimeMethodHandle",
          "GetUtf8NameInternal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              generics) ],
          MethodReturnType.Returns (ConcretePointer (ConcreteVoid state.ConcreteTypes)) when generics.IsEmpty ->
            // CoreCLR's RuntimeMethodHandle.GetUtf8NameInternal returns a raw pointer into
            // metadata; the managed wrapper RuntimeMethodHandle.GetUtf8Name(...) wraps the
            // result in MdUtf8String, which calls string.strlen on the pointer to discover
            // the byte length. PawPrint materialises the method's metadata name as a
            // freshly-allocated null-terminated UTF-8 byte[] and returns a byref to it; the
            // managed strlen path then walks the array as expected.
            let operation = "RuntimeMethodHandle.GetUtf8NameInternal"

            let methodHandleId =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation instruction.Arguments.[0]
                |> Option.defaultWith (fun () ->
                    // CoreCLR's GetUtf8NameInternal is an FCall that dereferences the
                    // MethodDesc* directly; the managed wrapper would surface a null
                    // result as BadImageFormatException. None of PawPrint's current
                    // callers (the introduced-method iterator) yield a null handle, so
                    // reaching here would indicate a contract violation upstream.
                    failwith $"%s{operation}: null RuntimeMethodHandleInternal"
                )

            let methodHandle =
                MethodHandleRegistry.resolveMethodFromId methodHandleId state.MethodHandles
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: registry id %d{methodHandleId} did not resolve to a known MethodHandle"
                )

            let assemblyFullName = methodHandle.GetAssemblyFullName ()

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let methodDefHandle = methodHandle.GetMethodDefinitionHandle().Get

            let mutable methodInfo =
                Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

            if not (assembly.Methods.TryGetValue (methodDefHandle, &methodInfo)) then
                failwith $"%s{operation}: MethodDef %O{methodDefHandle} not found in assembly %s{assemblyFullName}"

            let namePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes methodInfo.Name state

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer namePtr) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
