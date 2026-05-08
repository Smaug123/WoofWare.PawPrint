namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeSignature =
    let private signatureObjectAddress (operation : string) (arg : CliType) : ManagedHeapAddress =
        match arg with
        | CliType.ObjectRef (Some addr) -> addr
        | CliType.ObjectRef None ->
            failwith $"TODO: %s{operation} on null Signature should throw NullReferenceException"
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

    let private requireNullCorSig (operation : string) (pCorSig : CliType) (cCorSig : CliType) : unit =
        match CliType.unwrapPrimitiveLikeDeep pCorSig with
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> ()
        | other -> failwith $"TODO: %s{operation} pCorSig blob parsing is not implemented; got non-null %O{other}"

        let cCorSig = NativeCall.int32Argument operation cCorSig

        if cCorSig <> 0 then
            failwith $"TODO: %s{operation} pCorSig blob parsing is not implemented; got cCorSig %d{cCorSig}"

    let private requireNullMethodHandle (operation : string) (methodHandle : CliType) : unit =
        match CliType.unwrapPrimitiveLikeDeep methodHandle with
        | CliType.ObjectRef None
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> ()
        | other -> failwith $"TODO: %s{operation} method signature parsing is not implemented; got non-null %O{other}"

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

        // FieldHandle is the metadata identity paired with the closed declaring type seen by the
        // reflection caller; use that closed type for generic substitution rather than trusting the
        // separate declaringType argument that CoreLib passes through unchanged.
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
                                              methodHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", declaringTypeGenerics) ],
          MethodReturnType.Void when
            fieldHandleGenerics.IsEmpty
            && methodHandleGenerics.IsEmpty
            && declaringTypeGenerics.IsEmpty
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
                    failwith
                        $"TODO: %s{operation} non-field signature parsing is not implemented; fieldHandle was null, pCorSig=%O{instruction.Arguments.[1]}, cCorSig=%O{instruction.Arguments.[2]}, methodHandle=%O{instruction.Arguments.[4]}"
                )

            requireNullCorSig operation instruction.Arguments.[1] instruction.Arguments.[2]
            requireNullMethodHandle operation instruction.Arguments.[4]

            let fieldTypeAddr, state = runtimeTypeForField ctx operation fieldHandle state

            // This field-backed slice preserves the caller-provided declaringType exactly.
            // RuntimeFieldInfo supplies a non-null RuntimeType; CoreCLR's fallback from null
            // declaringType to the field's declaring type is outside this slice.
            let declaringType =
                match instruction.Arguments.[5] with
                | CliType.ObjectRef _ as value -> value
                | other -> failwith $"%s{operation}: expected declaring RuntimeType object reference, got %O{other}"

            let state = setSignatureField state signatureAddr "m_declaringType" declaringType

            let state =
                setSignatureField state signatureAddr "m_returnTypeORfieldType" (CliType.ObjectRef (Some fieldTypeAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
