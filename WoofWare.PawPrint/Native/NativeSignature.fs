namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeSignature =
    let private signatureObjectAddress (operation : string) (arg : CliType) : ManagedHeapAddress =
        match arg with
        | CliType.ObjectRef (Some addr) -> addr
        | CliType.ObjectRef None -> failwith $"TODO: %s{operation} on null Signature should throw NRE"
        | other -> failwith $"%s{operation}: expected Signature object reference, got %O{other}"

    let private setSignatureField
        (state : IlMachineState)
        (signatureAddr : ManagedHeapAddress)
        (fieldName : string)
        (value : CliType)
        : IlMachineState
        =
        let signatureObj = ManagedHeap.get signatureAddr state.ManagedHeap

        let field =
            IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType fieldName

        let signatureObj = AllocatedNonArrayObject.SetFieldById field value signatureObj

        { state with
            ManagedHeap = ManagedHeap.set signatureAddr signatureObj state.ManagedHeap
        }

    let private runtimeTypeForField
        (ctx : NativeCallContext)
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assembly, fieldInfo =
            NativeRuntimeFieldHandle.getFieldForFieldHandle operation fieldHandle state

        let declaringTypeHandle = fieldHandle.GetDeclaringTypeHandle ()

        let typeGenerics =
            match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
            | Some declaringType -> declaringType.Generics
            | None ->
                failwith
                    $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so field signature cannot be resolved"

        let state, fieldType =
            IlMachineState.concretizeType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                assembly.Name
                typeGenerics
                ImmutableArray.Empty
                fieldInfo.Signature

        IlMachineState.getOrAllocateType
            ctx.LoggerFactory
            ctx.BaseClassTypes
            (RuntimeTypeHandleTarget.Closed fieldType)
            state

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
          "Signature",
          "GetSignature",
          [ ConcretePointer (ConcreteVoid state.ConcreteTypes)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeFieldHandleInternal",
                                              fieldHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "IRuntimeMethodInfo",
                                              methodInfoGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Void when
            fieldHandleGenerics.IsEmpty
            && methodInfoGenerics.IsEmpty
            && runtimeTypeGenerics.IsEmpty
            ->
            let operation = "Signature.GetSignature"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected this plus five arguments, got %d{instruction.Arguments.Length}"

            let signatureAddr = signatureObjectAddress operation instruction.Arguments.[0]

            let fieldHandle =
                NativeRuntimeFieldHandle.fieldHandleOfRuntimeFieldHandleInternal
                    operation
                    state
                    instruction.Arguments.[3]
                |> Option.defaultWith (fun () ->
                    failwith $"TODO: %s{operation} without a field handle is not implemented"
                )

            let fieldTypeAddr, state = runtimeTypeForField ctx operation fieldHandle state

            let declaringType =
                match instruction.Arguments.[5] with
                | CliType.ObjectRef _ as value -> value
                | other -> failwith $"%s{operation}: expected declaring RuntimeType object ref, got %O{other}"

            let state = setSignatureField state signatureAddr "m_declaringType" declaringType

            let state =
                setSignatureField state signatureAddr "m_returnTypeORfieldType" (CliType.ObjectRef (Some fieldTypeAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
