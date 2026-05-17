namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module NativeSignature =
    /// ECMA II.23.2.4 calling-convention byte for a field signature blob.
    let private callingConventionField : int = 0x6

    /// ECMA II.23.2.3 low-nibble mask for the calling-convention byte; the
    /// upper bits are the HASTHIS / EXPLICITTHIS / GENERIC / VARARG flags.
    let private callingConventionMask : int = 0xF

    /// Resolve a Signature `_sig` argument to the owning assembly plus the
    /// COR signature `BlobHandle` it points at. PawPrint installs `_sig` as a
    /// managed byref over a field's PE-metadata signature blob; this helper
    /// unwraps that byref. Callers that only need the raw bytes can use
    /// `resolveSignatureBlob`; callers that need to seek with a `BlobReader`
    /// (e.g. token-aware parsers) acquire a fresh reader from the returned
    /// handle.
    let private resolveSignatureBlobHandle
        (operation : string)
        (state : IlMachineState)
        (sigArg : CliType)
        : DumpedAssembly * BlobHandle
        =
        let peByteRange =
            match CliType.unwrapPrimitiveLikeDeep sigArg with
            | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange,
                                                                                             _))) -> peByteRange
            | other -> failwith $"%s{operation}: expected managed pointer over a PE byte range for sig, got %O{other}"

        match peByteRange.Source with
        | PeByteRangePointerSource.FieldSignatureBlob field ->
            let assembly =
                state.LoadedAssembly' peByteRange.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: signature blob references unloaded assembly %s{peByteRange.AssemblyFullName}"
                )

            let mdReader = assembly.PeReader.GetMetadataReader ()
            let fieldDef = mdReader.GetFieldDefinition field.Get
            assembly, fieldDef.Signature
        | other ->
            failwith
                $"%s{operation}: signature `_sig` byref points at non-signature PE byte range %O{other}; only FieldSignatureBlob is currently supported"

    /// Resolve a Signature `_sig` argument to the COR signature blob bytes it
    /// points at. Built on top of `resolveSignatureBlobHandle`.
    let private resolveSignatureBlob (operation : string) (state : IlMachineState) (sigArg : CliType) : byte[] =
        let assembly, blobHandle = resolveSignatureBlobHandle operation state sigArg
        let mdReader = assembly.PeReader.GetMetadataReader ()
        mdReader.GetBlobBytes blobHandle

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

        // FieldHandle's declaring type is canonicalised per CoreCLR's per-canonical
        // FieldDesc model: `Closed` for non-generic declaring types,
        // `OpenGenericTypeDefinition` for generic ones. For closed declaring types
        // we have a real generic-argument vector to substitute into the field
        // signature; for the open form we don't, and a field whose signature
        // depends on a type generic parameter cannot be concretised to a single
        // `Closed` runtime type. The signature concretisation path therefore only
        // succeeds when either the declaring type is `Closed` or the field's type
        // does not reference its declaring type's generics. We pass empty
        // generics in the open case and let `concretizeType` fault with its own
        // diagnostic if the field signature actually needs them.
        let typeGenerics =
            match fieldHandle.GetDeclaringTypeHandle () with
            | RuntimeTypeHandleTarget.Closed declaringTypeHandle ->
                match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                | Some declaringType -> declaringType.Generics
                | None ->
                    failwith
                        $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so field signature cannot be resolved"
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ImmutableArray.Empty
            | other ->
                failwith
                    $"%s{operation}: field declaring type %O{other} cannot host a field; expected Closed or OpenGenericTypeDefinition"

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

    /// Populate the Signature object's `_returnTypeORfieldType`, `_sig`, `_csig`,
    /// and calling-convention fields for the field-backed path. The constructor
    /// caller supplies `_declaringType` directly, so this helper only needs to
    /// fill in the runtime-derived fields. `_sig` is set to a managed byref over
    /// the field's COR signature blob bytes in the assembly metadata, and
    /// `_csig` to the blob length, mirroring CoreCLR's
    /// `pFieldDesc->GetSig(&_sig, &_csig)`. Returns the updated machine state.
    let private fillFieldSignature
        (ctx : NativeCallContext)
        (operation : string)
        (signatureAddr : ManagedHeapAddress)
        (fieldHandle : FieldHandle)
        (returnTypeFieldName : string)
        (callingConventionFieldName : string)
        (sigFieldName : string)
        (csigFieldName : string)
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

        let assembly, _fieldInfo =
            NativeRuntimeFieldHandle.getFieldForFieldHandle operation fieldHandle state

        let peByteRange =
            IlMachineState.peByteRangeForFieldSignatureBlob assembly (fieldHandle.GetFieldDefinitionHandle().Get)

        let state, sigPointer =
            IlMachineState.peByteRangePointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

        let state =
            setSignatureField
                state
                signatureAddr
                sigFieldName
                (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))

        let state =
            setSignatureField
                state
                signatureAddr
                csigFieldName
                (CliType.Numeric (CliNumericType.Int32 peByteRange.Size))

        state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
            let signatureValue =
                IlMachineState.readManagedByref ctx.BaseClassTypes state signaturePtr

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
                    "_csig"
                    state

            NativeHandlerResult.completed state |> Some
        | "Signature_GetCustomModifiersAtOffset",
          "System.Private.CoreLib",
          "System",
          "Signature",
          "GetCustomModifiersAtOffset",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              sigObjGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "BOOL", boolGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              resultGenerics) ],
          MethodReturnType.Void when sigObjGenerics.IsEmpty && boolGenerics.IsEmpty && resultGenerics.IsEmpty ->
            // CoreCLR's Signature_GetCustomModifiersAtOffset (runtimehandles.cpp:1461)
            // walks the field/method signature blob from `offset`, collecting
            // CMOD_REQD / CMOD_OPT prefixes whose `required` flag matches the caller's
            // request, resolves each TypeDefOrRefOrSpec token under the Signature's
            // type context, allocates a fresh `Type[]` of exactly cMods entries, and
            // writes it back through the `result` ObjectHandleOnStack. The managed
            // caller `Signature.GetCustomModifiersAtOffset` asserts the result is
            // non-null even when cMods = 0, so we always allocate.
            //
            // Today PawPrint's `_sig` byref is only populated for field-shaped
            // signatures (see `Signature_Init` / `GetSignature` above), so we use
            // the declaring type's instantiation as the type context.
            // CMOD_INTERNAL (0x21) carries a `void*` that points at a runtime-only
            // TypeHandle; CoreCLR's own metadata writer never emits it from PE bytes,
            // so we fail loudly if we encounter one.
            let operation = "Signature.GetCustomModifiersAtOffset"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let sigObjPtr =
                NativeCall.objectHandleOnStackTarget operation state "sigObj" instruction.Arguments.[0]

            let offset = NativeCall.int32Argument operation instruction.Arguments.[1]
            let requiredFlag = NativeCall.int32Argument operation instruction.Arguments.[2]
            let fRequired = requiredFlag <> 0

            let resultPtr =
                NativeCall.objectHandleOnStackTarget operation state "result" instruction.Arguments.[3]

            let signatureValue =
                IlMachineState.readManagedByref ctx.BaseClassTypes state sigObjPtr

            let signatureAddr =
                match signatureValue with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    failwith $"%s{operation}: ObjectHandleOnStack pointed to a null Signature reference"
                | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

            let signatureObj = ManagedHeap.get signatureAddr state.ManagedHeap

            let sigFieldId =
                IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_sig"

            let sigCliValue =
                AllocatedNonArrayObject.DereferenceFieldById sigFieldId signatureObj

            let csigFieldId =
                IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_csig"

            let csig =
                match
                    AllocatedNonArrayObject.DereferenceFieldById csigFieldId signatureObj
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.Int32 v) -> v
                | other -> failwith $"%s{operation}: expected Int32 in Signature._csig, got %O{other}"

            let declaringTypeFieldId =
                IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_declaringType"

            let declaringTypeAddr =
                match AllocatedNonArrayObject.DereferenceFieldById declaringTypeFieldId signatureObj with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    failwith
                        $"%s{operation}: Signature._declaringType was null; the field-backed slice always carries a declaring RuntimeType"
                | other ->
                    failwith $"%s{operation}: expected RuntimeType ObjectRef in Signature._declaringType, got %O{other}"

            let declaringTypeObj = ManagedHeap.get declaringTypeAddr state.ManagedHeap

            let handleFieldId =
                IlMachineState.requiredOwnInstanceFieldId state declaringTypeObj.ConcreteType "m_handle"

            let declaringTarget =
                match
                    AllocatedNonArrayObject.DereferenceFieldById handleFieldId declaringTypeObj
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
                | other -> failwith $"%s{operation}: expected TypeHandlePtr in RuntimeType.m_handle, got %O{other}"

            let typeGenerics =
                match declaringTarget with
                | RuntimeTypeHandleTarget.Closed handle ->
                    match AllConcreteTypes.lookup handle state.ConcreteTypes with
                    | Some ct -> ct.Generics
                    | None ->
                        failwith
                            $"%s{operation}: declaring type handle %O{handle} was not concretized, so custom modifiers cannot be resolved"
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ImmutableArray.Empty
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith
                        $"%s{operation}: declaring type %O{declaringTarget} is a generic parameter; the field-backed slice expects a real declaring type"

            let assembly, blobHandle = resolveSignatureBlobHandle operation state sigCliValue
            let mdReader = assembly.PeReader.GetMetadataReader ()
            let mutable blobReader = mdReader.GetBlobReader blobHandle

            if blobReader.Length <> csig then
                failwith
                    $"%s{operation}: Signature._csig %d{csig} does not match the actual blob length %d{blobReader.Length}"

            if offset < 0 || offset > csig then
                failwith $"%s{operation}: offset %d{offset} is out of range for blob of length %d{csig}"

            blobReader.Offset <- offset

            // ECMA II.23.1.16 ELEMENT_TYPE_* constants for custom-modifier prefixes.
            let CMOD_REQD : byte = 0x1Fuy
            let CMOD_OPT : byte = 0x20uy
            let CMOD_INTERNAL : byte = 0x21uy
            let SENTINEL : byte = 0x41uy

            let modifierHandles = ResizeArray<EntityHandle> ()
            let mutable continueLoop = true

            while continueLoop do
                if blobReader.RemainingBytes <= 0 then
                    failwith
                        $"%s{operation}: signature blob ran out at offset %d{blobReader.Offset} while scanning for custom modifiers"

                let data = blobReader.ReadByte ()

                if data = CMOD_REQD || data = CMOD_OPT then
                    let handle = blobReader.ReadTypeHandle ()
                    let isRequired = (data = CMOD_REQD)

                    if isRequired = fRequired then
                        modifierHandles.Add handle
                elif data = CMOD_INTERNAL then
                    failwith
                        $"TODO: %s{operation} encountered CMOD_INTERNAL (0x21) at offset %d{blobReader.Offset - 1}; not yet supported (only produced by runtime-only signatures)"
                elif data <> SENTINEL then
                    continueLoop <- false

            let state, _, typeElementHandle =
                NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    "System"
                    "Type"

            let arrayAddr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero typeElementHandle)
                    (fun () -> CliType.ObjectRef None)
                    modifierHandles.Count
                    state

            // CoreCLR fills the result array via `SetAt(--cMods, ...)`, counting
            // down from `count - 1`, so the first matching modifier in scan order
            // lands at the last index and the last at index 0. Mirror that
            // ordering exactly: reflection callers comparing array contents
            // against real .NET expect the modifiers in reverse-of-scan order.
            let state =
                ((state, modifierHandles.Count - 1), modifierHandles)
                ||> Seq.fold (fun (state, index) eh ->
                    let token = MetadataToken.ofEntityHandle eh

                    let state, typeDefn, resolvedAssy =
                        IlMachineState.resolveTypeMetadataToken
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            state
                            assembly
                            typeGenerics
                            token

                    let state, concreteHandle =
                        IlMachineState.concretizeType
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            state
                            resolvedAssy.Name
                            typeGenerics
                            ImmutableArray.Empty
                            typeDefn

                    let runtimeTypeAddr, state =
                        IlMachineState.getOrAllocateType
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            (RuntimeTypeHandleTarget.Closed concreteHandle)
                            state

                    let state =
                        IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some runtimeTypeAddr)) index state

                    state, index - 1
                )
                |> fst

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    resultPtr
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        | _ -> None

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
          "Signature",
          "GetParameterOffsetInternal",
          [ ConcretePointer (ConcreteVoid state.ConcreteTypes)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Static InternalCall: `int GetParameterOffsetInternal(void* sig, int csig, int parameterIndex)`.
            // Mirrors CoreCLR's `SignatureNative::GetParameterOffsetInternal`: for the
            // FIELD calling convention (0x06) the only valid parameter index is 0 and
            // the byte offset to the parameter type is exactly 1 (just past the
            // single calling-conv byte). Method-shaped calling conventions are not
            // yet covered.
            let operation = "Signature.GetParameterOffsetInternal"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three arguments, got %d{instruction.Arguments.Length}"

            let csig = NativeCall.int32Argument operation instruction.Arguments.[1]
            let parameterIndex = NativeCall.int32Argument operation instruction.Arguments.[2]

            if csig <= 0 then
                failwith $"%s{operation}: csig must be positive, got %d{csig}"

            let bytes = resolveSignatureBlob operation state instruction.Arguments.[0]

            if bytes.Length <> csig then
                failwith $"%s{operation}: csig %d{csig} does not match the actual blob length %d{bytes.Length}"

            let callConv = int bytes.[0] &&& callingConventionMask

            let offset =
                if callConv = callingConventionField then
                    if parameterIndex <> 0 then
                        failwith $"%s{operation}: FIELD signature only has parameterIndex 0, got %d{parameterIndex}"

                    1
                else
                    failwith
                        $"TODO: %s{operation} non-FIELD calling convention 0x%X{callConv} is not yet implemented (csig=%d{csig}, parameterIndex=%d{parameterIndex})"

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 offset)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
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
                    "m_csig"
                    state

            NativeHandlerResult.completed state |> Some
        | _ -> None
