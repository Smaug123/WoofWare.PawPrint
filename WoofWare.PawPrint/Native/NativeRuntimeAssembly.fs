namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeAssembly =
    let private assemblyHandleOfRuntimeAssemblyRef
        (operation : string)
        (state : IlMachineState)
        (runtimeAssemblyRef : EvalStackValue)
        : string
        =
        let runtimeAssemblyAddr =
            match runtimeAssemblyRef with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith $"TODO: %s{operation} on null RuntimeAssembly should throw NRE"
            | other -> failwith $"%s{operation}: expected ObjectRef for RuntimeAssembly argument, got %O{other}"

        let heapObj = ManagedHeap.get runtimeAssemblyAddr state.ManagedHeap

        let assemblyField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_assembly"

        match
            AllocatedNonArrayObject.DereferenceFieldById assemblyField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)) -> assemblyFullName
        | other -> failwith $"%s{operation}: expected AssemblyHandle in RuntimeAssembly.m_assembly, got %O{other}"

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
          "System.Reflection",
          "RuntimeAssembly",
          ("GetToken" | "GetTokenInternal"),
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Reflection",
                                              "RuntimeAssembly",
                                              runtimeAssemblyGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeAssemblyGenerics.IsEmpty
            ->
            let operation = "RuntimeAssembly.GetToken"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeAssemblyRef, state = IlMachineState.popEvalStack ctx.Thread state

            assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef
            |> ignore

            // Every assembly manifest has a single Assembly metadata row.
            let mdAssemblyToken = 0x20000001

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 mdAssemblyToken)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          "GetManifestModule",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Reflection",
                                              "RuntimeAssembly",
                                              runtimeAssemblyGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeModule",
                                                                      runtimeModuleGenerics)) when
            runtimeAssemblyGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            let operation = "RuntimeAssembly.GetManifestModule"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeAssemblyRef, state = IlMachineState.popEvalStack ctx.Thread state

            let assemblyFullName =
                assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded"
                )

            let runtimeModuleAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeModule
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    assembly.Name
                    state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some runtimeModuleAddr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
