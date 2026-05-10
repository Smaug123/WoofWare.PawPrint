namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeSignature =
    /// ECMA II.23.2.4 calling-convention byte for a field signature blob.
    let private callingConventionField : int = 0x6

    /// Deliberately bogus pointer value installed as the field-signature blob.
    /// PawPrint does not yet serialise field signatures back to a COR-sig byte
    /// stream. Anything that tries to read this pointer via the managed-pointer
    /// boundary helpers will fail loudly through their catch-all rather than
    /// silently treating it as null and producing an empty parse.
    let private fieldSignatureBlobSentinel : int64 = 0xDEAD_BEEF_DEAD_BEEFL

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

    /// Populate the Signature object's `_returnTypeORfieldType`, `_sig`, and calling-convention
    /// fields for the field-backed path. The constructor caller supplies `_declaringType`
    /// directly, so this helper only needs to fill in the runtime-derived fields. Returns the
    /// updated machine state.
    let private fillFieldSignature
        (ctx : NativeCallContext)
        (operation : string)
        (signatureAddr : ManagedHeapAddress)
        (fieldHandle : FieldHandle)
        (returnTypeFieldName : string)
        (callingConventionFieldName : string)
        (sigFieldName : string)
        (state : IlMachineState)
        : IlMachineState
        =
        let fieldTypeAddr, state = runtimeTypeForField ctx operation fieldHandle state

        let state =
            setSignatureField state signatureAddr returnTypeFieldName (CliType.ObjectRef (Some fieldTypeAddr))

        let state =
            setSignatureField
                state
                signatureAddr
                callingConventionFieldName
                (CliType.Numeric (CliNumericType.Int32 callingConventionField))

        let state =
            setSignatureField
                state
                signatureAddr
                sigFieldName
                (CliType.RuntimePointer (CliRuntimePointer.Verbatim fieldSignatureBlobSentinel))

        state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "Signature_Init",
          "System.Private.CoreLib",
          "System",
          "Signature",
          "Init",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics)
            ConcretePointer (ConcreteVoid state.ConcreteTypes)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeFieldHandleInternal",
                                              fieldHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              methodHandleGenerics) ],
          MethodReturnType.Void when
            objectHandleGenerics.IsEmpty
            && fieldHandleGenerics.IsEmpty
            && methodHandleGenerics.IsEmpty
            ->
            // .NET 10 reshaped Signature.GetSignature into the Signature_Init QCall: the
            // declaringType is now set by the managed constructor before this call, so we
            // only populate the runtime-derived fields. Field names lost their `m_` prefix
            // (`m_returnTypeORfieldType` -> `_returnTypeORfieldType`, etc.).
            let operation = "Signature_Init"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let signaturePtr =
                NativeCall.objectHandleOnStackTarget operation state "_this" instruction.Arguments.[0]

            // ObjectHandleOnStack carries a managed byref to a slot that holds an object
            // reference; use the object-aware reader rather than the byte-view variant
            // (which rejects object references as not byte-addressable).
            let signatureValue = IlMachineState.readManagedByref state signaturePtr

            let signatureAddr =
                match signatureValue with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    failwith $"%s{operation}: ObjectHandleOnStack pointed to a null Signature reference"
                | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

            requireNullCorSig operation instruction.Arguments.[1] instruction.Arguments.[2]
            requireNullMethodHandle operation instruction.Arguments.[4]

            let fieldHandle =
                NativeRuntimeFieldHandle.fieldHandleOfRuntimeFieldHandleInternal
                    operation
                    state
                    instruction.Arguments.[3]
                |> Option.defaultWith (fun () ->
                    failwith
                        $"TODO: %s{operation} non-field signature parsing is not implemented; fieldHandle was null, pCorSig=%O{instruction.Arguments.[1]}, cCorSig=%O{instruction.Arguments.[2]}, methodHandle=%O{instruction.Arguments.[4]}"
                )

            let state =
                fillFieldSignature
                    ctx
                    operation
                    signatureAddr
                    fieldHandle
                    "_returnTypeORfieldType"
                    "_managedCallingConventionAndArgIteratorFlags"
                    "_sig"
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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
            // Pre-.NET 10 InternalCall path. .NET 10 routes the same field-signature population
            // through the Signature_Init QCall above (with `_declaringType` set by the managed
            // constructor before the QCall fires).
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

            // This slice covers only the field-backed path with null methodHandle.
            // CoreCLR's SignatureNative::GetSignature only tolerates a null declaringType
            // when methodHandle is a dynamic method (it then falls back to pMethod's
            // declaring type); with no method handle there is no fallback, and the field
            // caller (RuntimeFieldInfo.GetSignature) always supplies a non-null RuntimeType.
            // Reject null here rather than silently storing it into m_declaringType.
            let declaringType =
                match instruction.Arguments.[5] with
                | CliType.ObjectRef (Some _) as value -> value
                | CliType.ObjectRef None ->
                    failwith
                        $"%s{operation}: declaringType was null; the field-backed slice has no fallback for null declaring types"
                | other -> failwith $"%s{operation}: expected declaring RuntimeType object reference, got %O{other}"

            let state = setSignatureField state signatureAddr "m_declaringType" declaringType

            let state =
                fillFieldSignature
                    ctx
                    operation
                    signatureAddr
                    fieldHandle
                    "m_returnTypeORfieldType"
                    "m_managedCallingConventionAndArgIteratorFlags"
                    "m_sig"
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
