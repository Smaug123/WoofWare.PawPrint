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

    /// Classify a `void*` COR signature pointer argument as null or as a pointer over a PE byte
    /// range, rejecting anything else.
    ///
    /// Two spellings of each case are listed on purpose. `CliType.unwrapPrimitiveLikeDeep` unwraps
    /// primitive-like value-type wrappers but does *not* canonicalise
    /// `Numeric (NativeInt (ManagedPointer p))` into `RuntimePointer (Managed p)`, so the two are
    /// the same pointer in different encodings and a classifier that lists one of them throws on
    /// the other. The live route today — `ConstArray.m_constArray` (an `IntPtr` field) through
    /// `MdFieldInfo.FieldType` — produces only the `RuntimePointer` spelling; the `NativeInt` arm
    /// is a guard, matching how `requireNullCorSig` below lists every spelling of a null pointer.
    /// That guard is not idle caution: recognising only one encoding of an argument is how
    /// `Signature_Init` broke once already, while every unit test still passed (see
    /// `nullFieldHandleSpellings` in `TestNativeSignature.fs`).
    let private corSigPeByteRange (operation : string) (sigArg : CliType) : PeByteRangePointer option =
        match CliType.unwrapPrimitiveLikeDeep sigArg with
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange,
                                                                                         _)))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange,
                                                                                                                 _)))) ->
            Some peByteRange
        | other ->
            failwith
                $"%s{operation}: expected a null COR signature pointer or a managed pointer over a PE byte range, got %O{other}"

    /// Resolve a Signature `_sig` argument to the owning assembly plus the
    /// COR signature `BlobHandle` it points at. PawPrint installs `_sig` as a
    /// managed byref over a field's or a method's PE-metadata signature blob
    /// (see `fillFieldSignature` / `fillMethodSignature`); this helper unwraps
    /// that byref. Callers that only need the raw bytes can use
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
            corSigPeByteRange operation sigArg
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: COR signature pointer was null")

        let assembly () : DumpedAssembly =
            state.LoadedAssembly' peByteRange.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: signature blob references unloaded assembly %s{peByteRange.AssemblyFullName}"
            )

        match peByteRange.Source with
        | PeByteRangePointerSource.FieldSignatureBlob field ->
            let assembly = assembly ()
            let mdReader = assembly.PeReader.GetMetadataReader ()
            let fieldDef = mdReader.GetFieldDefinition field.Get
            assembly, fieldDef.Signature
        | PeByteRangePointerSource.MethodSignatureBlob method ->
            let assembly = assembly ()
            let mdReader = assembly.PeReader.GetMetadataReader ()
            let methodDef = mdReader.GetMethodDefinition method.Get
            assembly, methodDef.Signature
        | PeByteRangePointerSource.FieldRva _
        | PeByteRangePointerSource.ManagedResource _
        | PeByteRangePointerSource.ConstantBlob _ ->
            failwith $"%s{operation}: signature `_sig` byref points at non-signature PE byte range %O{peByteRange}"

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
        match corSigPeByteRange operation pCorSig with
        | None -> ()
        | Some peByteRange ->
            // CoreCLR overwrites the caller's blob with the handle's own signature when a handle is
            // supplied, so a caller passing both is not a shape any managed constructor produces.
            failwith
                $"%s{operation}: a handle-backed signature was given a non-null pCorSig (%O{peByteRange}); no managed Signature constructor passes both"

        let cCorSig = NativeCall.int32Argument operation cCorSig

        if cCorSig <> 0 then
            failwith $"%s{operation}: a handle-backed signature was given cCorSig %d{cCorSig}, expected 0"

    let private requireNullMethodHandle (operation : string) (methodHandle : CliType) : unit =
        match CliType.unwrapPrimitiveLikeDeep methodHandle with
        | CliType.ObjectRef None
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> ()
        | other -> failwith $"TODO: %s{operation} method signature parsing is not implemented; got non-null %O{other}"

    /// Concretize a field's declared type under <paramref name="typeGenerics"/> and hand back the
    /// `RuntimeType` for it. Shared by the handle-backed and raw-blob field paths so that the type a
    /// reflected field reports cannot depend on which constructor built its `Signature`.
    let private runtimeTypeOfFieldSignature
        (ctx : NativeCallContext)
        (assembly : DumpedAssembly)
        (fieldInfo : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (typeGenerics : ConcreteTypeHandle ImmutableArray)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
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

    /// The generic-argument vector a `Signature` resolves its blob against when no method handle is
    /// involved: CoreCLR builds the `SigTypeContext` from `_declaringType`
    /// (`SigTypeContext::InitTypeContext(declType)`, runtimehandles.cpp), *not* from the definition
    /// that owns the blob — so a literal on `Foo&lt;int&gt;` sees `int` for `VAR 0`.
    let private declaringTypeGenericsOfSignature
        (operation : string)
        (state : IlMachineState)
        (signatureObj : AllocatedNonArrayObject)
        : ConcreteTypeHandle ImmutableArray
        =
        let declaringTypeFieldId =
            IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_declaringType"

        let declaringTypeAddr =
            match AllocatedNonArrayObject.DereferenceFieldById declaringTypeFieldId signatureObj with
            | CliType.ObjectRef (Some addr) -> addr
            | CliType.ObjectRef None ->
                // CoreCLR asserts `!declType.IsNull()`: every managed constructor sets
                // `_declaringType` before calling in.
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

        match declaringTarget with
        | RuntimeTypeHandleTarget.Closed handle ->
            match AllConcreteTypes.lookup handle state.ConcreteTypes with
            | Some ct -> ct.Generics
            | None ->
                failwith
                    $"%s{operation}: declaring type handle %O{handle} was not concretized, so the signature cannot be resolved"
        // An open generic definition has no instantiation to substitute. Pass empty generics and let
        // `concretizeType` fault with its own diagnostic if the signature actually needs them.
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ImmutableArray.Empty
        | RuntimeTypeHandleTarget.OpenConstructed (definition, _) ->
            // An open constructed type does have an instantiation, but its arguments are targets
            // rather than `ConcreteTypeHandle`s, which is what this slice needs to substitute.
            failwith
                $"TODO: %s{operation}: declaring type is the open constructed type %O{definition.TypeDefinition.Get}; substituting its arguments needs them as concrete handles, which an open instantiation has not got"
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            failwith
                $"%s{operation}: declaring type %O{declaringTarget} is a generic parameter; the field-backed slice expects a real declaring type"

    let private runtimeTypeForField
        (ctx : NativeCallContext)
        (operation : string)
        (fieldHandle : FieldHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assembly, fieldInfo = FieldRvaData.fieldForHandle operation fieldHandle state

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

        runtimeTypeOfFieldSignature ctx assembly fieldInfo typeGenerics state

    /// Populate the Signature object's `_returnTypeORfieldType`, `_sig` and `_csig`
    /// for the field-backed path. The constructor caller supplies `_declaringType`
    /// directly, so this helper only needs to fill in the runtime-derived fields.
    /// `_sig` is set to a managed byref over the field's COR signature blob bytes
    /// in the assembly metadata, and `_csig` to the blob length, mirroring CoreCLR's
    /// `pFieldDesc->GetSig(&_sig, &_csig)`. Returns the updated machine state.
    ///
    /// The calling-convention field is deliberately left alone. CoreCLR's FIELD arm
    /// (`Signature_Init`, runtimehandles.cpp) is `msig.NextArgNormalized(); SetReturnType(...)` —
    /// only the `else` branch, for method-shaped blobs, calls `SetCallingConvention`. A
    /// field-backed `Signature` therefore keeps the zero its freshly-allocated object was born
    /// with, and the handle-backed and raw-blob paths (which share CoreCLR's common tail) agree.
    let private fillFieldSignature
        (ctx : NativeCallContext)
        (operation : string)
        (signatureAddr : ManagedHeapAddress)
        (fieldHandle : FieldHandle)
        (returnTypeFieldName : string)
        (sigFieldName : string)
        (csigFieldName : string)
        (state : IlMachineState)
        : IlMachineState
        =
        let fieldTypeAddr, state = runtimeTypeForField ctx operation fieldHandle state

        let state =
            setSignatureField state signatureAddr returnTypeFieldName (CliType.ObjectRef (Some fieldTypeAddr))

        let assembly, _fieldInfo = FieldRvaData.fieldForHandle operation fieldHandle state

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

    /// Populate a Signature built from a raw COR signature blob pointer with no field or method
    /// handle — CoreCLR's `Signature_Init` with `pMethodDesc == NULL && pFieldDesc == NULL`, which
    /// the `new Signature(void*, int, RuntimeType)` constructor produces. `MdFieldInfo.FieldType` is
    /// the only reachable caller: a literal field has no `FieldDesc`, so it is reflected over from
    /// metadata tokens alone.
    ///
    /// The blob is not re-parsed. `pCorSig` carries its own provenance — it is the pointer
    /// `MetadataImport.GetSigOfFieldDef` handed back, a PE byte range naming the FieldDef — so the
    /// field's type comes from the `FieldInfo.Signature` PawPrint already parsed, concretized under
    /// the generics of `_declaringType`. That is the same source the interpreter binds field access
    /// against, so a reflected field type cannot disagree with the one the machine uses; a second,
    /// byte-level decoder could. CoreCLR reaches the same answer by a different route:
    /// `MetaSig::NextArgNormalized` computes a *normalised* element type but the FIELD arm discards
    /// it, taking the type from `GetLastTypeHandleThrowing()` on the raw signature position, so
    /// (for instance) an enum-typed field reports the enum, not its underlying type.
    let private fillRawFieldBlobSignature
        (ctx : NativeCallContext)
        (operation : string)
        (signatureAddr : ManagedHeapAddress)
        (peByteRange : PeByteRangePointer)
        (pCorSig : CliType)
        (cCorSig : int)
        (returnTypeFieldName : string)
        (sigFieldName : string)
        (csigFieldName : string)
        (state : IlMachineState)
        : IlMachineState
        =
        let fieldHandle =
            match peByteRange.Source with
            | PeByteRangePointerSource.FieldSignatureBlob field -> field.Get
            | other ->
                // A method-signature blob cannot arrive here: every managed `Signature` constructor
                // that has a method passes the *handle*, and CoreCLR overwrites the blob from it.
                failwith
                    $"TODO: %s{operation} on a raw %O{other} blob is not implemented; only FieldDef signature blobs reach the handle-less constructor today"

        // CoreCLR trusts the caller's cCorSig, but every caller derives it from the same
        // `ConstArray` whose pointer this is, so a disagreement means a PawPrint bug rather than a
        // malformed image. `Signature_GetCustomModifiersAtOffset` already refuses a `_csig` that
        // does not match its blob; agreeing here keeps the two from disagreeing about one Signature.
        if cCorSig <> peByteRange.Size then
            failwith
                $"%s{operation}: cCorSig %d{cCorSig} does not match the %d{peByteRange.Size}-byte signature blob it points at (%O{peByteRange})"

        let assembly =
            state.LoadedAssembly' peByteRange.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: signature blob references unloaded assembly %s{peByteRange.AssemblyFullName}"
            )

        let mutable fieldInfo =
            Unchecked.defaultof<FieldInfo<GenericParamFromMetadata, TypeDefn>>

        if not (assembly.Fields.TryGetValue (fieldHandle, &fieldInfo)) then
            failwith $"%s{operation}: FieldDef %O{fieldHandle} was not present in %s{peByteRange.AssemblyFullName}"

        let signatureObj = ManagedHeap.get signatureAddr state.ManagedHeap

        // CoreCLR skips the whole type-derivation block when `_returnTypeORfieldType` is already
        // set, which serves the `DynamicMethod` constructor's pre-filled Signature. PawPrint
        // declares dynamic code unsupported, so that shape cannot arrive; refuse it rather than
        // silently overwrite what a caller had put there.
        let returnTypeFieldId =
            IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType returnTypeFieldName

        match AllocatedNonArrayObject.DereferenceFieldById returnTypeFieldId signatureObj with
        | CliType.ObjectRef None -> ()
        | already ->
            failwith
                $"%s{operation}: Signature.%s{returnTypeFieldName} was already %O{already}; CoreCLR's pre-filled path serves DynamicMethod, which PawPrint does not support"

        let typeGenerics = declaringTypeGenericsOfSignature operation state signatureObj

        let fieldTypeAddr, state =
            runtimeTypeOfFieldSignature ctx assembly fieldInfo typeGenerics state

        let state =
            setSignatureField state signatureAddr returnTypeFieldName (CliType.ObjectRef (Some fieldTypeAddr))

        // `_sig` and `_csig` are the caller's own arguments, verbatim, as CoreCLR assigns them. That
        // keeps the blob's provenance intact for the later byte-level readers
        // (`GetParameterOffsetInternal`, `Signature_GetCustomModifiersAtOffset`).
        let state = setSignatureField state signatureAddr sigFieldName pCorSig

        setSignatureField state signatureAddr csigFieldName (CliType.Numeric (CliNumericType.Int32 cCorSig))

    /// ECMA II.23.2.3 calling-convention low nibble values that a *method* signature can carry.
    let private callingConventionVarArg : int = 0x5

    /// The managed `CallingConventions` bits CoreCLR's `SignatureNative::SetCallingConvention`
    /// (runtimehandles.h:455) derives from the raw ECMA calling-convention byte. Note that this
    /// is a translation, not the raw byte: `CallingConventions.Standard` is 0x1 while
    /// `IMAGE_CEE_CS_CALLCONV_DEFAULT` is 0x0.
    let private callConvStandard : int = 0x1
    let private callConvVarArgs : int = 0x2
    let private callConvHasThis : int = 0x20
    let private callConvExplicitThis : int = 0x40

    let private managedCallingConventionOfHeader (operation : string) (header : SignatureHeader) : int =
        // CoreCLR dispatches on the blob's own calling convention, not on which handle it was
        // given, so a FIELD-shaped blob reached from a method handle would take the *field* arm
        // there. No MethodDef can carry one; assert rather than silently reporting Standard.
        match header.Kind with
        | SignatureKind.Method -> ()
        | other ->
            failwith
                $"%s{operation}: method signature blob has signature kind %O{other}, not Method; a MethodDef cannot carry a field or local-variable calling convention"

        let baseBits =
            if int header.CallingConvention = callingConventionVarArg then
                callConvVarArgs
            else
                callConvStandard

        let withThis =
            if header.IsInstance then
                baseBits ||| callConvHasThis
            else
                baseBits

        if header.HasExplicitThis then
            withThis ||| callConvExplicitThis
        else
            withThis

    /// The `SigTypeContext` a method-backed `Signature` resolves its blob against: CoreCLR builds
    /// it from the declaring type's class instantiation plus
    /// `pMethodDesc->LoadMethodInstantiation()` (`Signature_Init`, runtimehandles.cpp:1622, and
    /// `SignatureNative::GetTypeContext`, runtimehandles.h:388). Returns the defining assembly too,
    /// since token resolution against the blob needs it.
    ///
    /// A generic method *definition* has no representable context: the handle the introduced-method
    /// iterator mints carries empty `MethodGenerics` while the method declares type parameters, and
    /// CoreCLR resolves against the typical instantiation, whose method generic parameters a
    /// `ConcreteTypeHandle` cannot name (the same limit that parks
    /// `sourcesPure/MakeGenericMethodOpenArgument.cs`). Fail loudly rather than substitute.
    let private methodSignatureTypeContext
        (operation : string)
        (state : IlMachineState)
        (methodHandleArg : CliType)
        : DumpedAssembly *
          MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
          MetadataMethodIdentity *
          ImmutableArray<ConcreteTypeHandle> *
          ImmutableArray<ConcreteTypeHandle>
        =
        let identity =
            NativeRuntimeMethodHandle.resolveMetadataIdentityFromArg operation state methodHandleArg

        let methodInfo =
            NativeRuntimeMethodHandle.methodInfoOfMetadataIdentity operation state identity

        let assemblyFullName = identity.GetAssemblyFullName ()

        let assembly =
            state.LoadedAssembly' assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

        let declaringTypeHandle = identity.GetDeclaringType ()

        let typeGenerics =
            match declaringTypeHandle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                | Some declaringType -> declaringType.Generics
                | None ->
                    failwith
                        $"%s{operation}: declaring type handle %O{declaringTypeHandle} was not concretized, so the method signature cannot be resolved"
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // CoreCLR reaches this via `declType.GetClassOrArrayInstantiation()`, which for an
                // array type yields its element type as a one-element instantiation, so the
                // runtime-generated array methods (Get/Set/Address/.ctor) resolve their signatures
                // against it. PawPrint stores array element types structurally in the handle rather
                // than as a generic argument vector, so that projection does not exist here.
                failwith
                    $"TODO: %s{operation} on a method whose declaring type is the structural type %O{declaringTypeHandle}; CoreCLR resolves such a signature against GetClassOrArrayInstantiation, which PawPrint does not model"

        let methodGenerics = identity.GetMethodGenerics () |> ImmutableArray.CreateRange

        if methodInfo.Generics.Length <> methodGenerics.Length then
            let plural =
                if methodInfo.Generics.Length = 1 then
                    "generic parameter"
                else
                    "generic parameters"

            failwith
                $"TODO: %s{operation} on generic method definition %s{methodInfo.Name}: it declares %d{methodInfo.Generics.Length} %s{plural} but the handle carries %d{methodGenerics.Length} generic argument(s); CoreCLR resolves the signature against the typical instantiation, whose method generic parameters PawPrint's ConcreteTypeHandle cannot represent"

        assembly, methodInfo, identity, typeGenerics, methodGenerics

    /// Populate the Signature object's `_returnTypeORfieldType`, `_arguments`, `_sig`, `_csig`,
    /// `_pMethod` and calling-convention fields for the method-backed path, mirroring CoreCLR's
    /// `Signature_Init` (runtimehandles.cpp:1585) when `pMethodDesc != NULL`. `_declaringType` is
    /// supplied by the managed constructor, so this helper only fills the runtime-derived fields.
    ///
    /// The types come from PawPrint's already-parsed `MethodInfo.Signature` rather than from a
    /// second parse of the COR blob, so a method's reflected parameter types cannot disagree with
    /// the types the interpreter binds calls against. `_sig`/`_csig` still point at the blob,
    /// because the byte-level readers (`GetParameterOffsetInternal`,
    /// `Signature_GetCustomModifiersAtOffset`, `Signature_AreEqual`) work from the bytes.
    let private fillMethodSignature
        (ctx : NativeCallContext)
        (operation : string)
        (signatureAddr : ManagedHeapAddress)
        (methodHandleArg : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        let assembly, methodInfo, identity, typeGenerics, methodGenerics =
            methodSignatureTypeContext operation state methodHandleArg

        let concretize (state : IlMachineState) (defn : TypeDefn) : IlMachineState * ConcreteTypeHandle =
            IlMachineState.concretizeType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                assembly.Name
                typeGenerics
                methodGenerics
                defn

        // CoreCLR takes the return type from `msig.GetRetTypeHandleThrowing()`, which for a void
        // return is `System.Void`'s TypeHandle rather than a null one.
        let state, returnTypeHandle =
            match methodInfo.Signature.ReturnType with
            | MethodReturnType.Void ->
                let state, _, voidHandle =
                    NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        "System"
                        "Void"

                state, voidHandle
            | MethodReturnType.Returns ret -> concretize state ret

        let returnTypeAddr, state =
            IlMachineState.getOrAllocateType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed returnTypeHandle)
                state

        let state =
            setSignatureField state signatureAddr "_returnTypeORfieldType" (CliType.ObjectRef (Some returnTypeAddr))

        let state =
            setSignatureField
                state
                signatureAddr
                "_managedCallingConventionAndArgIteratorFlags"
                (CliType.Numeric (
                    CliNumericType.Int32 (managedCallingConventionOfHeader operation methodInfo.Signature.Header.Get)
                ))

        // CoreCLR allocates the `_arguments` array unconditionally -- `AllocateSzArray(arrayHandle,
        // nArgs)` with nArgs possibly 0 -- and the managed `Signature.Arguments` getter asserts it
        // is non-null, so a nullary method must still get an empty array rather than null.
        let state, _, runtimeTypeElementHandle =
            NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                "System"
                "RuntimeType"

        // CoreCLR sizes `_arguments` with `msig.NumFixedArgs()`, i.e. the parameters before any
        // VARARG sentinel. A MethodDef signature never carries a sentinel -- only a call site's
        // MemberRef does -- so the two counts coincide here; assert it rather than assume it,
        // because if they ever diverge we would silently publish the optional parameters as fixed.
        let parameterTypes = methodInfo.Signature.ParameterTypes

        if List.length parameterTypes <> methodInfo.Signature.RequiredParameterCount then
            failwith
                $"%s{operation}: method %s{methodInfo.Name} has %d{List.length parameterTypes} parameter types but %d{methodInfo.Signature.RequiredParameterCount} required parameters; a MethodDef signature was not expected to carry a VARARG sentinel"

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero runtimeTypeElementHandle)
                (fun () -> CliType.ObjectRef None)
                (List.length parameterTypes)
                state

        let state =
            ((state, 0), parameterTypes)
            ||> List.fold (fun (state, index) parameterType ->
                let state, handle = concretize state parameterType

                let addr, state =
                    IlMachineState.getOrAllocateType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        (RuntimeTypeHandleTarget.Closed handle)
                        state

                let state =
                    IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some addr)) index state

                state, index + 1
            )
            |> fst

        let state =
            setSignatureField state signatureAddr "_arguments" (CliType.ObjectRef (Some arrayAddr))

        let peByteRange =
            IlMachineState.peByteRangeForMethodSignatureBlob assembly (identity.GetMethodDefinitionHandle().Get)

        let state, sigPointer =
            IlMachineState.peByteRangePointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

        let state =
            setSignatureField state signatureAddr "_sig" (CliType.RuntimePointer (CliRuntimePointer.Managed sigPointer))

        let state =
            setSignatureField state signatureAddr "_csig" (CliType.Numeric (CliNumericType.Int32 peByteRange.Size))

        setSignatureField state signatureAddr "_pMethod" methodHandleArg

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

            // CoreCLR's precedence is `if (pMethodDesc != NULL) ... else if (pFieldDesc != NULL)`,
            // then a caller-supplied raw blob. PawPrint classifies the inputs explicitly instead,
            // and refuses both-non-null: no managed `Signature` constructor passes both, so a value
            // arriving that way would be a PawPrint bug, and silently preferring one would hide it.
            let methodHandle =
                NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation instruction.Arguments.[4]

            let fieldHandle =
                NativeRuntimeFieldHandle.fieldHandleOfRuntimeFieldHandleInternal
                    operation
                    state
                    instruction.Arguments.[3]

            let state =
                match methodHandle, fieldHandle with
                | Some _, Some fieldHandle ->
                    failwith
                        $"%s{operation}: got both a field handle and a method handle (field %O{fieldHandle}, methodHandle=%O{instruction.Arguments.[4]}); no managed Signature constructor supplies both"
                | Some _, None ->
                    // CoreCLR overwrites the caller's blob from the handle, so a handle-backed call
                    // must not also carry one.
                    requireNullCorSig operation instruction.Arguments.[1] instruction.Arguments.[2]
                    fillMethodSignature ctx operation signatureAddr instruction.Arguments.[4] state
                | None, Some fieldHandle ->
                    requireNullCorSig operation instruction.Arguments.[1] instruction.Arguments.[2]

                    fillFieldSignature
                        ctx
                        operation
                        signatureAddr
                        fieldHandle
                        "_returnTypeORfieldType"
                        "_sig"
                        "_csig"
                        state
                | None, None ->
                    // Handle-less: the blob itself is the input. This is
                    // `new Signature(void*, int, RuntimeType)`, i.e. `MdFieldInfo.FieldType`.
                    match corSigPeByteRange operation instruction.Arguments.[1] with
                    | Some peByteRange ->
                        fillRawFieldBlobSignature
                            ctx
                            operation
                            signatureAddr
                            peByteRange
                            instruction.Arguments.[1]
                            (NativeCall.int32Argument operation instruction.Arguments.[2])
                            "_returnTypeORfieldType"
                            "_sig"
                            "_csig"
                            state
                    | None ->
                        // CoreCLR asserts `pCorSig != NULL && cCorSig > 0` once both handles are
                        // null, so there is nothing left to derive a signature from.
                        failwith
                            $"%s{operation}: no field handle, no method handle, and a null pCorSig (cCorSig=%O{instruction.Arguments.[2]}); there is nothing to build a signature from"

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
            // The type context follows `SignatureNative::GetTypeContext`
            // (runtimehandles.h:388): when `_pMethod` is non-null it is the method's
            // context (declaring-class instantiation plus method instantiation), and
            // only otherwise is it the declaring type's alone. Both shapes of `_sig`
            // reach here -- `Signature_Init` populates it for field-backed and
            // method-backed signatures alike.
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

            let pMethodFieldId =
                IlMachineState.requiredOwnInstanceFieldId state signatureObj.ConcreteType "_pMethod"

            let pMethod =
                AllocatedNonArrayObject.DereferenceFieldById pMethodFieldId signatureObj

            let typeGenerics, methodGenerics =
                match NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation pMethod with
                | Some _ ->
                    let _, _, _, typeGenerics, methodGenerics =
                        methodSignatureTypeContext operation state pMethod

                    typeGenerics, methodGenerics
                | None -> declaringTypeGenericsOfSignature operation state signatureObj, ImmutableArray.Empty

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
                            methodGenerics
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
                    "m_sig"
                    "m_csig"
                    state

            NativeHandlerResult.completed state |> Some
        | _ -> None
