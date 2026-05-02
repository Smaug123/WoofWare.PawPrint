namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeAssembly =
    let private writeLength
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (lengthOut : ManagedPointerSource)
        (length : uint32)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state lengthOut (NativeCall.cliUInt32 length)

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
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)) ->
            assemblyFullName
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

            assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef |> ignore

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
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let runtimeModuleAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeModule ctx.LoggerFactory ctx.BaseClassTypes assembly.Name state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some runtimeModuleAddr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
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
        | "AssemblyNative_GetResource",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallAssembly",
                                              qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ],
          MethodReturnType.Returns (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) when
            qCallAssemblyGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetResource"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let resourceNamePtr =
                NativeCall.managedPointerOfPointerArgument operation "resourceName" instruction.Arguments.[1]

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length" instruction.Arguments.[2]

            let resourceName =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state resourceNamePtr

            if resourceName.Length = 0 then
                failwith $"TODO: %s{operation} with empty resource name should throw ArgumentException"

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let state =
                match AssemblyApi.findManifestResource assembly resourceName with
                | ManifestResourceLookupResult.NotFound ->
                    let state = writeLength ctx state lengthOut 0u

                    IlMachineState.pushToEvalStack'
                        (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
                        ctx.Thread
                        state
                | ManifestResourceLookupResult.Embedded resource ->
                    let state = writeLength ctx state lengthOut (uint32 resource.PayloadLength)
                    let peByteRange = IlMachineState.peByteRangeForEmbeddedManifestResource resource

                    // Return a pointer even when PayloadLength is zero: null
                    // means "resource not found", while a zero-sized PE range
                    // means "resource exists and is empty".
                    let state, dataPtr =
                        IlMachineState.peByteRangePointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

                    IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer dataPtr) ctx.Thread state
                | ManifestResourceLookupResult.ExternalFile resource ->
                    // Deliberately fail loudly until linked-file resources are
                    // implemented. CoreCLR returns null for this case.
                    failwith
                        $"TODO: %s{operation} does not support external-file manifest resource %s{resource.Name} in %s{resource.AssemblyFullName} from %s{resource.FileName}"
                | ManifestResourceLookupResult.ReferencedAssembly (actualResourceName, assemblyReference) ->
                    // Deliberately fail loudly until forwarded resources are
                    // implemented. CoreCLR follows the AssemblyRef chain.
                    failwith
                        $"TODO: %s{operation} does not support assembly-forwarded manifest resource %s{actualResourceName} in %s{assemblyFullName} forwarded to %s{assemblyReference.Name.FullName}"

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
