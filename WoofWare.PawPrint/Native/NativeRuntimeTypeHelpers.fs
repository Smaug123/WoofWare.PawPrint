namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

module NativeRuntimeTypeHelpers =
    let primitiveCorElementType (primitive : PrimitiveType) : int32 =
        match primitive with
        | PrimitiveType.Boolean -> 0x02
        | PrimitiveType.Char -> 0x03
        | PrimitiveType.SByte -> 0x04
        | PrimitiveType.Byte -> 0x05
        | PrimitiveType.Int16 -> 0x06
        | PrimitiveType.UInt16 -> 0x07
        | PrimitiveType.Int32 -> 0x08
        | PrimitiveType.UInt32 -> 0x09
        | PrimitiveType.Int64 -> 0x0A
        | PrimitiveType.UInt64 -> 0x0B
        | PrimitiveType.Single -> 0x0C
        | PrimitiveType.Double -> 0x0D
        // String and Object are NOT TruePrimitive in CoreCLR; their MethodTable falls into
        // MethodTable::GetSignatureCorElementType's default branch and reports ELEMENT_TYPE_CLASS.
        // The ELEMENT_TYPE_STRING (0x0E) / ELEMENT_TYPE_OBJECT (0x1C) shorthands only appear in
        // metadata signature blobs, never at the runtime handle level.
        | PrimitiveType.String -> 0x12
        | PrimitiveType.TypedReference -> 0x16
        | PrimitiveType.IntPtr -> 0x18
        | PrimitiveType.UIntPtr -> 0x19
        | PrimitiveType.Object -> 0x12

    let nativeIntSize : int =
        CliType.sizeOf (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

    let int32AtPointer
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : int
        =
        match
            IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 0))
        with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwith $"%s{operation}: expected Int32 at pointer, got %O{other}"

    let writeInt32AtPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : int)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 value))

    let nativeIntElementPointer
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (buffer : ManagedPointerSource)
        (index : int)
        : ManagedPointerSource
        =
        match buffer with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex), []) ->
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex + index), [])
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
            ManagedPointerSource.Byref (
                ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset + (index * nativeIntSize)),
                []
            )
        // Span<IntPtr> pinned over a `stackalloc IntPtr[N]` buffer: the Span(void*, int)
        // constructor appends `ReinterpretAs IntPtr` to the localloc byte byref when
        // storing it into `_reference`, and the LibraryImport stub for the QCall pulls
        // that reference back out. The reinterpret is address-preserving, so striding
        // by nativeIntSize bytes and preserving the projection keeps the typed view.
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset),
                                      [ ByrefProjection.ReinterpretAs reinterpretTy as proj ]) ->
            // The QCall signature mandates `Span<IntPtr>`; any other reinterpret type would mean
            // the buffer was constructed from a different element type and `nativeIntSize` striding
            // would be wrong, so surface the mismatch loudly rather than silently mis-striding.
            if InternalTypeKind.kind baseClassTypes reinterpretTy <> InternalTypeKind.NativeInt then
                failwith
                    $"%s{operation}: expected IntPtr-reinterpret on localloc buffer, got %s{reinterpretTy.Namespace}.%s{reinterpretTy.Name} in %O{buffer}"

            ManagedPointerSource.Byref (
                ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset + (index * nativeIntSize)),
                [ proj ]
            )
        // The 1-arg overload of CreateInstanceForAnotherGenericParameter takes the
        // address of a single IntPtr local (`&typeHandle`), so element 0 *is* the
        // buffer itself. We cannot stride past it without escaping the local.
        | ManagedPointerSource.Byref (ByrefRoot.LocalVariable _, []) when index = 0 -> buffer
        | ManagedPointerSource.Byref (ByrefRoot.Argument _, []) when index = 0 -> buffer
        // Buffers are currently reached through GetFields' stackalloc/array path
        // (either as a bare byte byref or with a trailing `ReinterpretAs IntPtr` when
        // the buffer was wrapped in a Span<IntPtr>), or through a single-IntPtr local
        // taken by `&` for the 1-arg overload of CreateInstanceForAnotherGenericParameter.
        // Other shapes should fail with their structure intact.
        | _ -> failwith $"%s{operation}: unsupported IntPtr result buffer pointer shape %O{buffer}"

    let writeFieldHandleElement
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        (value : int64)
        : IlMachineState
        =
        let ptr = nativeIntElementPointer operation baseClassTypes buffer index

        IlMachineState.writeManagedByrefWithBase
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr value)))

    let runtimeFieldInfoStubAddress
        (operation : string)
        (state : IlMachineState)
        (runtimeFieldHandle : CliType)
        : ManagedHeapAddress
        =
        match runtimeFieldHandle with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state vt.Declared "m_ptr"

            match CliValueType.DereferenceFieldById ptrField vt |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.ObjectRef (Some addr) -> addr
            | CliType.ObjectRef None ->
                failwith $"%s{operation}: RuntimeFieldHandle.m_ptr was null after field handle allocation"
            | other -> failwith $"%s{operation}: expected RuntimeFieldHandle.m_ptr object ref, got %O{other}"
        | other -> failwith $"%s{operation}: expected RuntimeFieldHandle value type, got %O{other}"

    /// Enumerate the non-literal fields declared by a `(assembly, typeDef)` pair,
    /// materialising each as a field-handle registry id (the int64 the BCL writes
    /// into the buffer). Instance fields are listed before statics; literals are
    /// excluded. `declaringTarget` is the `RuntimeTypeHandleTarget` to record on
    /// each allocated `FieldHandle`: callers walking a closed type pass the
    /// `Closed` handle of that instantiation; callers walking an open generic
    /// typedef pass `OpenGenericTypeDefinition`. CoreCLR observably distinguishes
    /// these (`typeof(G<int>).GetField(...).FieldHandle` is incompatible with
    /// `typeof(G<>)`'s) so the two cases produce distinct registry ids.
    let walkFieldsOfTypeDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (declaringAssemblyName : AssemblyName)
        (declaringTypeDefinition : System.Reflection.Metadata.TypeDefinitionHandle)
        (declaringTarget : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : IlMachineState * int64 list
        =
        let assembly =
            state.LoadedAssembly declaringAssemblyName
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: assembly for declaring type is not loaded: %s{declaringAssemblyName.FullName}"
            )

        let typeInfo = assembly.TypeDefs.[declaringTypeDefinition]

        let fields =
            typeInfo.Fields
            |> List.filter (fun field -> not (field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal))

        let instanceFields, staticFields =
            fields |> List.partition (fun field -> not field.IsStatic)

        let fields = instanceFields @ staticFields

        ((state, []), fields)
        ||> List.fold (fun (state, ids) field ->
            let runtimeFieldHandle, state =
                IlMachineState.getOrAllocateField
                    loggerFactory
                    baseClassTypes
                    declaringAssemblyName
                    declaringTarget
                    field.Handle
                    state

            let stubAddress = runtimeFieldInfoStubAddress operation state runtimeFieldHandle

            let fieldHandleId =
                FieldHandleRegistry.resolveFieldIdFromAddress stubAddress state.FieldHandles
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: RuntimeFieldInfoStub %O{stubAddress} was not present in the field handle registry"
                )

            state, fieldHandleId :: ids
        )
        |> fun (state, ids) -> state, List.rev ids

    /// Enumerate the non-literal fields of a closed runtime type handle, materialising
    /// each as a field-handle registry id (the int64 the BCL writes into the buffer).
    /// Mirrors CoreCLR's `RuntimeTypeHandle::GetFields` walk on a `MethodTable*`: instance
    /// fields first, then statics; literals are excluded. Handles remain metadata
    /// identities; consumers that need closed signatures must substitute against the
    /// closed concrete type themselves.
    let walkClosedTypeHandleFields
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (typeHandle : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * int64 list
        =
        match typeHandle with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> state, []
        | ConcreteTypeHandle.Concrete _ ->
            let concreteType =
                AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                )

            walkFieldsOfTypeDefinition
                loggerFactory
                baseClassTypes
                operation
                concreteType.Assembly
                concreteType.Definition.Get
                (RuntimeTypeHandleTarget.Closed typeHandle)
                state

    let nominalCorElementType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<_, _>)
        : int32
        =
        if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
            0x11
        else
            0x12

    let corElementType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : int32
        =
        match typeHandleTarget with
        // An open constructed type is not a TypeDesc, so CoreCLR's
        // `TypeHandle::GetSignatureCorElementType` (typehandle.cpp:1160) routes it to
        // `MethodTable::GetSignatureCorElementType`, which reports the EEClass's internal
        // element type — the same CLASS/VALUETYPE the definition reports.
        // `CreateMinimalMethodTable` calls `SetInternalCorElementType(ELEMENT_TYPE_CLASS)`
        // (methodtable.cpp:703). Stored on the MethodTable itself, so no row is needed to read it.
        | RuntimeTypeHandleTarget.DynamicMethodsClass _ -> 0x12
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            nominalCorElementType baseClassTypes state typeInfo
        | RuntimeTypeHandleTarget.GenericParameter _ ->
            // ELEMENT_TYPE_VAR — see corhdr.h. CoreCLR's TypeHandle::GetSignatureCorElementType
            // delegates to TypeDesc::GetInternalCorElementType, which for a TypeVarTypeDesc is
            // the constant assigned at construction time: VAR for type-level generic parameters.
            0x13
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // ELEMENT_TYPE_MVAR — the method-level counterpart of VAR.
            0x1E
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteVoid state.ConcreteTypes -> 0x01
            | ConcretePrimitive state.ConcreteTypes primitive -> primitiveCorElementType primitive
            | ConcreteTypeHandle.Byref _ -> 0x10
            | ConcreteTypeHandle.Pointer _ -> 0x0F
            | ConcreteTypeHandle.FunctionPointer _ -> 0x1B
            | ConcreteTypeHandle.OneDimArrayZero _ -> 0x1D
            | ConcreteTypeHandle.Array _ -> 0x14
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                nominalCorElementType baseClassTypes state typeInfo

    let enumUnderlyingPrimitive (operation : string) (typeInfo : TypeInfo<_, TypeDefn>) : PrimitiveType option =
        let instanceFields =
            typeInfo.Fields |> List.filter (fun field -> not field.IsStatic)

        match instanceFields with
        | [ field ] when field.Name = "value__" ->
            match field.Signature with
            | TypeDefn.PrimitiveType primitive -> Some primitive
            | other -> failwith $"%s{operation}: enum value__ field had non-primitive signature %O{other}"
        | _ -> None

    let primitiveMethodTableCorElementType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : int32
        =
        match methodTableFor with
        | ConcretePrimitive state.ConcreteTypes primitive -> primitiveCorElementType primitive
        | ConcreteTypeHandle.Concrete _ ->
            let concreteType =
                AllConcreteTypes.lookup methodTableFor state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{methodTableFor}"
                )

            let assembly =
                state.LoadedAssembly concreteType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                )

            let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

            match enumUnderlyingPrimitive operation typeInfo with
            // CoreCLR debug-builds assert IsPrimitive for GetPrimitiveCorElementType, which excludes
            // enums. Release builds still fall through to the underlying primitive element type; match
            // that observable behaviour because managed enum code can reach this helper.
            | Some primitive -> primitiveCorElementType primitive
            | None ->
                failwith
                    $"%s{operation}: expected primitive or enum MethodTable, got %s{typeInfo.Namespace}.%s{typeInfo.Name}"
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith $"%s{operation}: expected primitive or enum MethodTable, got %O{methodTableFor}"

    let requiredValueTypeMethod
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (name : string)
        (parameterCount : int)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        baseClassTypes.ValueType.Methods
        |> List.filter (fun methodInfo ->
            methodInfo.Name = name
            && MethodInfo.arity methodInfo = parameterCount
            && not methodInfo.IsStatic
        )
        |> function
            | [ methodInfo ] -> methodInfo
            | [] -> failwith $"%s{operation}: could not find System.ValueType::%s{name}"
            | methods ->
                let signatures =
                    methods
                    |> List.map (fun methodInfo -> $"%s{methodInfo.Name}/%i{MethodInfo.arity methodInfo}")
                    |> String.concat ", "

                failwith $"%s{operation}: ambiguous System.ValueType::%s{name} candidates: %s{signatures}"

    let overridesValueTypeMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodTableFor : ConcreteTypeHandle)
        (valueTypeMethod : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let state, concretizedMethod, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                valueTypeMethod
                ImmutableArray.Empty
                state

        let state, directImplementation =
            IlMachineStateExecution.tryResolveVirtualImplementation
                loggerFactory
                baseClassTypes
                thread
                ImmutableArray.Empty
                concretizedMethod
                methodTableFor
                false
                state

        state, Option.isSome directImplementation

    let rec fieldAllowsFastCompare
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (valueTypeEquals : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (valueTypeGetHashCode :
            WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (seen : Set<ConcreteTypeHandle>)
        (field : CliField)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let rec canCompareValueType
            (seen : Set<ConcreteTypeHandle>)
            (methodTableFor : ConcreteTypeHandle)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            canCompareBitsOrUseFastGetHashCodeImpl
                loggerFactory
                baseClassTypes
                thread
                valueTypeEquals
                valueTypeGetHashCode
                seen
                methodTableFor
                state

        match CliType.unwrapPrimitiveLikeDeep field.Contents with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Float32 _
            | CliNumericType.Float64 _
            | CliNumericType.NativeFloat _
            | CliNumericType.NativeInt _ -> state, false
            | CliNumericType.Int32 _
            | CliNumericType.Int64 _
            | CliNumericType.Int8 _
            | CliNumericType.Int16 _
            | CliNumericType.UInt8 _
            | CliNumericType.UInt16 _ -> state, true
        | CliType.Bool _
        | CliType.Char _ -> state, true
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> state, false
        | CliType.ValueType _ -> canCompareValueType seen field.Type state

    and canCompareBitsOrUseFastGetHashCodeImpl
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (valueTypeEquals : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (valueTypeGetHashCode :
            WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (seen : Set<ConcreteTypeHandle>)
        (methodTableFor : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        if Set.contains methodTableFor seen then
            failwith
                $"MethodTable_CanCompareBitsOrUseFastGetHashCode: recursive value-type layout for %O{methodTableFor}"

        match methodTableFor with
        | ConcreteTypeHandle.Concrete _ ->
            let _, typeInfo =
                match IlMachineState.tryGetConcreteTypeInfo state methodTableFor with
                | Some result -> result
                | None ->
                    failwith
                        $"MethodTable_CanCompareBitsOrUseFastGetHashCode: concrete type handle was not registered: %O{methodTableFor}"

            if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo) then
                failwith
                    $"MethodTable_CanCompareBitsOrUseFastGetHashCode: expected value-type MethodTable, got %s{typeInfo.Namespace}.%s{typeInfo.Name}"

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes methodTableFor

            let fieldLayoutIsTightlyPacked =
                match zero with
                | CliType.Numeric (CliNumericType.Float32 _)
                | CliType.Numeric (CliNumericType.Float64 _)
                | CliType.Numeric (CliNumericType.NativeFloat _)
                | CliType.Numeric (CliNumericType.NativeInt _)
                | CliType.ObjectRef _
                | CliType.RuntimePointer _ -> false
                | CliType.Numeric _
                | CliType.Bool _
                | CliType.Char _ -> true
                | CliType.ValueType vt -> CliValueType.IsTightlyPacked vt

            if not fieldLayoutIsTightlyPacked || CliType.containsObjectReferences zero then
                state, false
            else

            let state, overridesEquals =
                overridesValueTypeMethod loggerFactory baseClassTypes thread methodTableFor valueTypeEquals state

            let state, overridesGetHashCode =
                overridesValueTypeMethod loggerFactory baseClassTypes thread methodTableFor valueTypeGetHashCode state

            if overridesEquals || overridesGetHashCode then
                state, false
            else

            let state, fields =
                IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state methodTableFor

            let seen = Set.add methodTableFor seen

            ((state, true), fields)
            ||> List.fold (fun (state, canCompare) field ->
                if not canCompare then
                    state, false
                else
                    fieldAllowsFastCompare
                        loggerFactory
                        baseClassTypes
                        thread
                        valueTypeEquals
                        valueTypeGetHashCode
                        seen
                        field
                        state
            )
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith
                $"MethodTable_CanCompareBitsOrUseFastGetHashCode: expected value-type MethodTable, got %O{methodTableFor}"

    let canCompareBitsOrUseFastGetHashCode
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodTableFor : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let operation = "MethodTable_CanCompareBitsOrUseFastGetHashCode"

        let valueTypeEquals = requiredValueTypeMethod operation baseClassTypes "Equals" 1

        let valueTypeGetHashCode =
            requiredValueTypeMethod operation baseClassTypes "GetHashCode" 0

        canCompareBitsOrUseFastGetHashCodeImpl
            loggerFactory
            baseClassTypes
            thread
            valueTypeEquals
            valueTypeGetHashCode
            Set.empty
            methodTableFor
            state

    let mdTypeDefNil : int32 = 0x02000000

    let typeDefinitionToken (handle : System.Reflection.Metadata.TypeDefinitionHandle) : int32 =
        let handle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit handle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

    let typeDefinitionTokenOfRuntimeTypeHandleTarget
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : int32
        =
        match typeHandleTarget with
        // An instantiation carries no metadata row of its own; `Type.MetadataToken` reports
        // the generic definition's TypeDef row for both the open and the closed forms.
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) -> typeDefinitionToken identity.TypeDefinition.Get
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            // ECMA-335 §II.22.20: GenericParam table tag 0x2A. The parameter's
            // GenericParameterHandle is owned by the declaring type's metadata
            // reader, which we reach via the declaring type's loaded assembly.
            let assembly =
                state.LoadedAssembly declaringType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for generic parameter declaring type is not loaded: %s{declaringType.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

            if position >= typeInfo.Generics.Length then
                failwith
                    $"%s{operation}: generic parameter position %i{position} out of range for %s{typeInfo.Namespace}.%s{typeInfo.Name} (has %i{typeInfo.Generics.Length} generics)"

            let param, _md = typeInfo.Generics.[position]
            MetadataToken.toInt (MetadataToken.GenericParameter param.Handle.Get)
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            // ECMA-335 §II.22.20: GenericParam table tag 0x2A. For method-level
            // generic parameters the owner is the declaring method rather than a type.
            let assembly =
                state.LoadedAssembly declaringType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for method generic parameter declaring type is not loaded: %s{declaringType.AssemblyFullName}"
                )

            let methodInfo = assembly.Methods.[declaringMethod.Get]

            if position >= methodInfo.Generics.Length then
                failwith
                    $"%s{operation}: method generic parameter position %i{position} out of range for method %O{declaringMethod.Get} (has %i{methodInfo.Generics.Length} method generics)"

            let param, _md = methodInfo.Generics.[position]
            MetadataToken.toInt (MetadataToken.GenericParameter param.Handle.Get)
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                    )

                typeDefinitionToken concreteType.Definition.Get
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> mdTypeDefNil

    let containsGenericVariables
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : bool
        =
        MethodTableProjection.targetContainsGenericVariables operation state typeHandleTarget

    /// One entry of a type's instance vtable: the method currently occupying the slot, together
    /// with the closed type it was read from. That type's generic arguments are the substitution
    /// context its signature must be concretised in, and the base chain's entries carry a
    /// different context from the derived type's -- which is the whole difficulty of matching an
    /// override against the slot it fills.
    type VtableSlot =
        {
            Method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
            DeclaredBy : ConcreteType<ConcreteTypeHandle>
        }

    /// Does this signature type mention a generic parameter of its *declaring type* anywhere? Used
    /// only to decide whether substituting that type's generic arguments could have changed a
    /// comparison.
    ///
    /// A generic *method* parameter deliberately does not count. `signaturesEquivalent` compares
    /// those positionally and never substitutes them, exactly as `MetaSig::CompareElementType` does,
    /// so no instantiation can make two of them coincide — and counting them would call every tie
    /// between two generic methods an artifact, which rejects ordinary C#: `A` declaring
    /// `virtual M&lt;T&gt;()`, `B` hiding it with a `new virtual M&lt;T&gt;()`, and `C` overriding `B`'s
    /// leaves two identically-signed slots on the chain for `C`'s to match.
    let rec private mentionsGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericTypeParameter _ -> true
        | TypeDefn.GenericMethodParameter _ -> false
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> mentionsGenericParameter element
        | TypeDefn.Modified m -> mentionsGenericParameter m.Unmodified || mentionsGenericParameter m.Modifier
        | TypeDefn.GenericInstantiation (generic, args) ->
            mentionsGenericParameter generic
            || (args |> Seq.exists mentionsGenericParameter)
        | TypeDefn.FunctionPointer signature ->
            (match signature.ReturnType with
             | MethodReturnType.Void -> false
             | MethodReturnType.Returns ret -> mentionsGenericParameter ret)
            || (signature.ParameterTypes |> List.exists mentionsGenericParameter)
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void -> false

    /// The raw, unsubstituted signature of a slot's occupant, for asking whether two slots were
    /// already identical *before* their declaring types' generic arguments were substituted in.
    let private rawSignature (slot : VtableSlot) : TypeDefn list * MethodReturnType<TypeDefn> =
        slot.Method.Signature.ParameterTypes, slot.Method.Signature.ReturnType

    /// Could substituting the declaring types' generic arguments have created this tie, rather than
    /// the candidate and the slots it matched being genuinely the same at the generic-definition
    /// level?
    ///
    /// Answered conservatively, because the exact question is not decidable from what a closed
    /// walk carries. Syntactic equality of the raw signatures does not prove the tie genuine: a
    /// raw `!0` is scoped to the type that wrote it, and two types' `!0` need
    /// not denote the same thing at a shared instantiation. Measured: with `Ka<T>.M(T)`,
    /// `Kb&lt;T&gt; : Ka&lt;string&gt;` declaring `M(T)`, and `Kc&lt;T&gt; : Kb&lt;T&gt;` overriding `M(string)`, both
    /// inherited signatures are raw `[!0]` yet .NET replaces Ka's slot and reports `Kc`/`Kb`;
    /// trusting the syntactic equality picks Kb's slot and reports `Kc`/`Ka`.
    ///
    /// So: if no signature involved mentions a generic parameter at all, no substitution can have
    /// changed anything and the tie is genuine (any raw difference is then just the same type
    /// spelled as a TypeDef in one assembly and a TypeRef in another). Otherwise, refuse to guess.
    ///
    /// The *candidate's* own signature is swept alongside the slots it matched, not just the slots
    /// against each other. Without it the guard is weaker than the rule above: let non-generic `A`
    /// declare `virtual string M(string)` and `B : A` re-declare it `newslot`, so that two slots
    /// hold identical parameter-free signatures; then let `C&lt;T&gt; : B` declare a *non-newslot*
    /// `virtual string M(!0)`. At `T = string` the candidate matches both slots, yet neither
    /// slot's raw signature mentions a parameter, so sweeping only the slots calls the tie genuine
    /// and answers `B`'s slot -- where CoreCLR, comparing at the definition level, matches neither
    /// (`!0` is not `string`) and allocates a fresh slot. Only the candidate carries the evidence
    /// that a substitution happened at all.
    let private tieCouldBeSubstitutionArtifact (slots : VtableSlot list) : bool =
        let types (slot : VtableSlot) =
            let parameters, ret = rawSignature slot

            match ret with
            | MethodReturnType.Void -> parameters
            | MethodReturnType.Returns ty -> ty :: parameters

        slots |> List.collect types |> List.exists mentionsGenericParameter

    // One limitation remains in comparing a candidate against a slot, and it is about substitution
    // rather than about what the comparison reads: a single match can itself be a substitution
    // artifact. `A.M(string)` with `C&lt;T&gt;` declaring a non-newslot `M(T)` is not an override at the
    // generic-definition level, so CoreCLR gives it a fresh slot; inspected as `C&lt;string&gt;` the two
    // signatures coincide and this walk overwrites A's slot. The multi-match guard above cannot
    // catch it because there is only one match, and the obvious "does a generic parameter appear"
    // screen would reject every ordinary override of a generic base (`G1&lt;string&gt;.Id(T)` overridden
    // by `G2.Id(string)`), which is common C#. It needs the substitution chain that CoreCLR carries
    // down the `extends` clauses, which is a different thing from comparing two signatures.
    //
    // Roslyn does not emit that shape -- it needs a hand-written non-newslot override -- but it is
    // legal IL, so a guest built by another toolchain could reach it.

    /// Does `candidate`, a non-newslot instance virtual declared on some derived type, fill the
    /// vtable slot currently occupied by `slot`?
    ///
    /// This is CoreCLR's *layout* rule (`MethodTableBuilder::LoaderFindMethodInParentClass`): same
    /// name, and an exact signature match under substitution -- return type included. It is
    /// deliberately stricter than PawPrint's *dispatch* rule in
    /// `IlMachineStateExecution.tryResolveVirtualImplementationForSlot`, which accepts an
    /// assignable return type and has variance carve-outs. That difference is not an oversight on
    /// either side: a covariant-return override is a genuinely new slot in CoreCLR (Roslyn emits it
    /// `newslot` plus a MethodImpl), so folding it into the base slot by return-assignability would
    /// make `GetMethods` report one method where .NET reports two.
    let private candidateFillsSlot
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (candidate : VtableSlot)
        (slot : VtableSlot)
        : IlMachineState * bool
        =
        if candidate.Method.Name <> slot.Method.Name then
            // The one rejection worth making before the signature comparison, because it is the one
            // that discards nearly every (candidate, slot) pair. Everything else the layout rule
            // requires -- the calling convention and `hasThis` in the header, the generic arity, the
            // parameter count -- `signaturesEquivalent` compares first, in that order.
            state, false
        else

        let comparand (slot : VtableSlot) : TypeConcretization.SignatureComparand =
            {
                Signature = slot.Method.Signature
                Assembly = slot.DeclaredBy.Assembly
                // A slot's occupant is read through the closed type it was found on, and the base
                // chain's entries carry a different instantiation from the derived type's. That is
                // the substitution the comparison needs.
                DeclaringTypeGenerics = slot.DeclaredBy.Generics
            }

        IlMachineState.signaturesEquivalent
            loggerFactory
            baseClassTypes
            state
            false
            (comparand candidate)
            (comparand slot)

    /// One side of the constraint comparison CoreCLR runs once it has chosen which parent slot a
    /// generic override fills.
    let private constraintComparand (slot : VtableSlot) : TypeConcretization.ConstraintComparand =
        {
            Parameters = slot.Method.Generics |> Seq.map snd |> List.ofSeq
            Assembly = slot.DeclaredBy.Assembly
            DeclaringTypeGenerics = slot.DeclaredBy.Generics
        }

    /// The methods of a type that CoreCLR's `DeclaredMethodIterator` ranges over, paired with their
    /// metadata facts. Both halves of the method table are laid out from this one list, so that
    /// neither can disagree with the other about what the type declares.
    ///
    /// Two kinds of row are absent from it.
    ///
    /// A *synthesised* method has no MethodDef row, so it is not a declared method at all. The
    /// vtable walk excludes them only incidentally (a synthesised method is never `IsVirtual`);
    /// beyond the vtable, placing one would shift every later method's slot number by one. No test
    /// can cover the filter: nothing today puts a synthesised method into a `TypeInfo` (the
    /// construction sites in `Program.buildStartupFrame` and `StructMarshalStub` both build one for
    /// immediate execution), but `TypeInfo.Methods` is typed to hold either kind.
    ///
    /// A COM *vtable-gap marker* names empty slots in the COM interface vtable rather than declaring
    /// a method. `EnumerateClassMethods` recognises it by `IsMdRTSpecialName` plus a `_VtblGap` name
    /// prefix (methodtablebuilder.cpp:2749, corhdr.h:265-270) and `continue`s before it reaches
    /// `rgDeclaredMethods` (:2852-2921), recording the run length in a `SparseVTableMap` that only
    /// `FEATURE_COMINTEROP` reads -- so it occupies no slot in the CLR method table, virtual or
    /// otherwise. Dropping it here rather than in one walk alone is the point: tlbimp emits these as
    /// `virtual abstract` members of an interface, so a filter applied only past the vtable would
    /// leave the *vtable* inflated by one slot per gap, which moves `GetNumVirtuals` and with it the
    /// origin of everything after it.
    ///
    /// The name grammar is `_VtblGap` + optional digits + optionally `_` and at least one digit, and
    /// CoreCLR refuses to load the type for anything else (:2865-2907) rather than treating it as an
    /// ordinary method -- so a prefix match alone would accept images the runtime rejects. Upstream
    /// raises that as `COR_E_BADIMAGEFORMAT` with `IDS_CLASSLOAD_BADSPECIALMETHOD`, but what a guest
    /// (and the fabricated test) observes is a `TypeLoadException`.
    let private declaredMethodsOf
        (operation : string)
        (concreteTypeInfo : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : (MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * MetadataMethodFacts) list
        =
        // Exactly `void`, no custom modifier: a *blob* comparison, matching `ExactlyEqual`.
        let hasNullaryVoidSignature (method : MethodInfo<_, _, _>) : bool =
            method.Signature.ParameterTypes.IsEmpty
            && not (
                method.Signature.Header.Get.Attributes.HasFlag System.Reflection.Metadata.SignatureAttributes.Generic
            )
            && method.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.Default
            && method.Signature.ReturnType = MethodReturnType.Void

        // `void` once custom modifiers are looked through, which is the other question CoreCLR asks;
        // see `slotsBeyondVtableOfClosed` for why the two must stay separate.
        let returnsVoidThroughModifiers (method : MethodInfo<_, _, _>) : bool =
            match method.Signature.ReturnType with
            | MethodReturnType.Void -> true
            | MethodReturnType.Returns ty -> TypeDefn.stripCustomModifiers ty = TypeDefn.Void

        // `_VtblGap`, then the optional-number/count grammar upstream parses.
        let isWellFormedGapName (name : string) : bool =
            let suffix = name.Substring "_VtblGap".Length
            let afterLeadingDigits = suffix.TrimStart [| '0' .. '9' |]

            if afterLeadingDigits = "" then
                // "_VtblGap" or "_VtblGap<n>": a single empty slot, or the count-less form.
                true
            elif afterLeadingDigits.[0] <> '_' then
                false
            else
                let count = afterLeadingDigits.Substring 1
                count <> "" && count |> Seq.forall System.Char.IsAsciiDigit

        typeInfo.Methods
        |> List.choose (fun method ->
            match method.TryMetadata with
            | None -> None
            | Some facts ->
                if
                    facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName
                    && method.Name.StartsWith ("_VtblGap", System.StringComparison.Ordinal)
                then
                    if not (isWellFormedGapName method.Name) then
                        failwith
                            $"%s{operation}: method %s{method.Name} on %O{concreteTypeInfo} is marked RTSpecialName and begins `_VtblGap`, but the rest of the name is not the vtable-gap count grammar; CoreCLR rejects the type at load time (methodtablebuilder.cpp:2865-2907) rather than laying out a method table for it"

                    None
                else

                // The load-time rejections. They live here, rather than beside the placement that
                // needs them, so that they run for *every* type this walk touches -- including each
                // ancestor, since `vtableOfClosed` recurses through the base chain and asks each one
                // for its declared methods. A type whose base CoreCLR refuses to load cannot itself
                // be loaded, because building a MethodTable begins by building the parent's, so
                // validating only the leaf would let `GetSlot` answer for a derived type that cannot
                // exist.
                //
                // The scope is exactly the type and its base chain.
                // Those are the declarations that *contribute slots to the layout being computed*,
                // so a rejection anywhere in them means the numbers this function returns describe a
                // MethodTable that cannot exist. An implemented interface is a different matter:
                // CoreCLR does load one while building the type (`ResolveInterfaces`) and would
                // refuse the implementor if the interface were malformed, but no interface method
                // enters this slot table, so nothing computed here depends on it. Chasing that
                // dependency has no natural stopping point short of the whole type-load closure --
                // field types, generic constraints, and so on -- which is a different feature from
                // laying out a method table. A guest that asks about the malformed interface itself
                // is still refused, because this same function is what answers for it.
                //
                // The classification below keys *on* the
                // RTSpecialName flag, and that is only unambiguous because CoreCLR refuses to load
                // the shapes that would make it ambiguous. Same reason `vtableOfClosed` refuses a
                // non-newslot virtual that matches a `final` parent slot.

                // A `static virtual` is legal only on an interface: on a class or value type
                // `ValidateMethods` throws `IDS_CLASSLOAD_STATICVIRTUAL`
                // (methodtablebuilder.cpp:5124-5131). Only the `!IsInterface()` half is enforced
                // there -- upstream's comment beside it also says such methods "must be abstract",
                // but nothing checks that, and static virtuals with bodies have been legal since
                // .NET 7. Without this the method would simply be placed past the vtable, since
                // `PlaceVirtualMethods` skips it for being static.
                if method.IsStatic && method.IsVirtual && not typeInfo.IsInterface then
                    failwith
                        $"%s{operation}: method %s{method.Name} on %O{concreteTypeInfo} is both static and virtual, which is legal only on an interface; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5124-5131) rather than laying out a method table for it"

                if facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName then
                    if method.IsVirtual then
                        failwith
                            $"%s{operation}: method %s{method.Name} on %O{concreteTypeInfo} is marked RTSpecialName and virtual; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5001-5004) rather than laying out a method table for it"

                    if method.IsStatic then
                        if method.Name <> ".cctor" || not (hasNullaryVoidSignature method) then
                            failwith
                                $"%s{operation}: static method %s{method.Name} on %O{concreteTypeInfo} is marked RTSpecialName but is not exactly `static void .cctor()`; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5011-5019) rather than laying out a method table for it"
                    else if method.Name <> ".ctor" then
                        failwith
                            $"%s{operation}: instance method %s{method.Name} on %O{concreteTypeInfo} is marked RTSpecialName but is not named `.ctor`; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5023-5026) rather than laying out a method table for it"
                    elif not (returnsVoidThroughModifiers method) then
                        failwith
                            $"%s{operation}: constructor on %O{concreteTypeInfo} does not return void; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5028-5037) rather than laying out a method table for it"

                Some (method, facts)
        )

    /// The instance vtable of a closed type, base-first: index `i` is the method that currently
    /// occupies slot `i`. A type inherits its base's layout, replaces the entries its own
    /// non-newslot virtuals override, and appends a slot for each `newslot` virtual it introduces.
    ///
    /// This is the single definition of "which slot" in PawPrint: `GetSlot` is an index into this
    /// list and `GetNumVirtuals` is its length, so the two cannot disagree -- which matters,
    /// because the BCL *compares* them (`isVirtual = slot &lt; GetNumVirtuals(declaringType)`,
    /// RuntimeType.CoreCLR.cs:685-686).
    ///
    /// Note that MethodImpls are deliberately not consulted. A MethodImpl overwrites a slot's
    /// implementation but not the slot number its body was declared at
    /// (`MethodTableBuilder::SetVirtualMethodImpl` changes the Impl and not the Decl), so it
    /// belongs to slot *content* -- dispatch, and one day `GetMethodAt` -- rather than to slot
    /// identity.
    ///
    /// This is recomputed on every `GetSlot`/`GetNumVirtuals` query, and `PopulateMethods` issues
    /// one query per virtual method: the walk is not memoised, so populating a type is quadratic in
    /// its virtual count before counting the concretisation each signature comparison performs.
    let rec vtableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * VtableSlot list
        =
        // The un-memoised quadratic walk is affordable at this interpreter's speed and has not
        // been measured as a bottleneck. If it ever is, note that the cache key must be the
        // `ConcreteTypeHandle` and not the underlying type definition: `List<int>` and
        // `List<string>` share a definition, and the whole point of this walk is that it compares
        // *substituted* signatures, so a definition-keyed cache would serve one instantiation's
        // layout for another.
        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // Byrefs, pointers, and function pointers are TypeDescs in CoreCLR with no
            // MethodTable, so they have no vtable at all.
            state, []
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Synthesised array MethodTables inherit their virtual slots from System.Array (and
            // through it, System.Object); the structural array handle itself introduces none.
            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

            match baseHandle with
            | None -> state, []
            | Some bh -> vtableOfClosed loggerFactory baseClassTypes operation state bh
        | ConcreteTypeHandle.Concrete _ ->
            let concreteTypeInfo, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

            let state, baseSlots =
                match baseHandle with
                | None -> state, []
                | Some bh -> vtableOfClosed loggerFactory baseClassTypes operation state bh

            // Shared with the walk past the vtable, so that the two cannot disagree about what the
            // type declares -- see `declaredMethodsOf` for what it drops and why. Upstream's
            // `PlaceVirtualMethods` takes exactly the declared *instance* virtuals from that same
            // list; a `static virtual` is placed past the vtable instead.
            let instanceVirtuals =
                declaredMethodsOf operation concreteTypeInfo typeInfo
                |> List.filter (fun (method, _) -> not method.IsStatic && method.IsVirtual)
                |> List.map fst

            // Upstream is a single pass over the declared methods in MethodDef row order
            // (`DeclaredMethodIterator` over the array `EnumerateClassMethods` fills in row order),
            // and each method either replaces a parent slot or takes the next free one
            // (`MethodTableBuilder::PlaceVirtualMethods`, methodtablebuilder.cpp:5405-5482). So
            // overrides and fresh slots interleave, and it is *declaration* order -- not NewSlot --
            // that decides the order of the fresh ones. Partitioning on NewSlot and appending the
            // groups separately would agree only while fresh slots were the exclusive preserve of
            // NewSlot methods, which stopped being true the moment the unmatched case below started
            // allocating one. Measured on a fabricated type declaring an unmatched NewSlot virtual
            // before an unmatched non-NewSlot one: the host CLR gives the NewSlot method the lower
            // slot, and a NewSlot-grouped layout gets it backwards (TestFabricatedVtableLayout).
            //
            // Only the *parent's* slots are candidates for a match: upstream searches
            // `bmtParent->pParentMethodHash`, built once from the parent MethodTable
            // (methodtablebuilder.cpp:174-193) and never extended as this type's own methods are
            // placed. A slot appended by an earlier method of *this* type is therefore not something
            // a later one can land on.
            //
            // That is why the fold below carries the inherited slots and the fresh ones as two
            // values rather than one growing list. `inherited` only ever has entries *replaced*, so
            // it stays exactly the parent's vtable and the search cannot reach a fresh slot however
            // the search is written. Threading one list and capping the search at the parent's length
            // would compute the same answer, but this way the invariant is a property of the shape
            // rather than of remembering to cap; it also keeps appending O(1) rather than copying the
            // accumulated vtable per method, which for an interface -- where every member appends --
            // is the difference between a linear layout and a quadratic one.
            //
            // The restriction bites on legal metadata, not only on corrupt images. ECMA-335
            // II.22.26 stops a type repeating a method blob-for-blob, but `candidateFillsSlot`
            // compares *concretised* signatures -- which is what lets an ordinary override of a
            // generic base match at all -- and that conflates blobs which genuinely differ. The
            // worked example is `GenericConflation`1` in TestFabricatedVtableLayout: it declares
            // `Conflated(!0)` as NewSlot and `Conflated(string)` without it, and closing it at
            // `T = string` makes the second match the slot the first was just appended to. CoreCLR
            // lays slots out on the generic definition, where the two are distinct, and gives each
            // its own; a search that could see fresh slots would have the second replace the first
            // and the vtable would come out a slot short.
            let state, inherited, freshReversed =
                ((state, baseSlots, []), instanceVirtuals)
                ||> List.fold (fun (state, slots, fresh) method ->
                    let candidate =
                        {
                            VtableSlot.Method = method
                            VtableSlot.DeclaredBy = concreteTypeInfo
                        }

                    let state, matched =
                        if method.IsNewSlot then
                            // "If the member is marked with a new slot we do not need to find it in
                            // the parent" -- it is asking for a slot of its own by construction.
                            state, []
                        else
                            // An interface reaches here with no inherited slots, so the search is
                            // empty and every method it declares appends -- which is exactly what
                            // upstream's `IsInterface` arm does, an interface having no parent whose
                            // slots it could reuse. That arm needs no special case here, but it does
                            // need the unmatched case below to allocate rather than fail: corelib's
                            // `INumberBase<T>` declares `System.IUtf8SpanFormattable.TryFormat` as
                            // `Private, Final, Virtual, HideBySig` with no NewSlot -- measured, the
                            // only such method in corelib -- and it takes this path.
                            ((state, []), List.indexed slots)
                            ||> List.fold (fun (state, acc) (i, slot) ->
                                let state, fills =
                                    candidateFillsSlot loggerFactory baseClassTypes state candidate slot

                                state, (if fills then i :: acc else acc)
                            )

                    // More than one slot can legitimately match: `A` declares `virtual M()`, `B :
                    // A` declares `new virtual M()` with the identical signature, and `C : B`
                    // overrides it. CoreCLR resolves this in `LoaderFindMethodInParentClass`, and
                    // the tie-break lives in how that lookup's index is built rather than in the
                    // lookup itself: `CreateMethodChainHash` walks the *parent's* slot table in
                    // ascending slot order and inserts each slot's occupant at the **head** of its
                    // name bucket, and `Lookup` returns the first entry in the bucket. So the entry
                    // returned is the one inserted last, i.e. the occupant of the highest matching
                    // slot -- the most-derived declaration, which is also C#'s meaning, since
                    // `C.M` overrides the `M` that `B` introduced and leaves `A`'s alone. Slots are
                    // appended as the walk descends, so that is the matching slot with the largest
                    // index; the fold above prepends, so `matched` is already in descending index
                    // order.
                    // A candidate legitimately matching several slots is the `new virtual` case
                    // above, where the tie is real and most-derived is the answer. But a tie can
                    // also be an *artifact* of matching closed signatures: CoreCLR lays slots out on
                    // the generic definition, where `A<T>.M(T)` and `B<T>.M(string)` are distinct
                    // methods, whereas concretising at `T = string` first makes them identical. In
                    // that case most-derived silently picks the wrong slot -- measured: .NET reports
                    // `C<string>.M` and `B.M`, and matching closed signatures yields `A.M` instead.
                    //
                    // The two are separable. A tie can only be an artifact if some generic
                    // substitution actually happened, so when several slots match and any type
                    // involved is generic, fail rather than guess. A single match is safe *whenever
                    // the method overrides at all*: the definition-level match is then still among
                    // the candidates, so if only one slot matches, it is that one. That proviso
                    // matters -- substitution can manufacture a lone match
                    // for a method that overrides nothing, which is the first limitation recorded
                    // above (non-newslot `C<T>.M(T)` over `A.M(string)`: one closed match, zero
                    // definition-level matches, and CoreCLR allocates a fresh slot). Detecting that
                    // needs the generic-definition layout this walk does not have, so it is refused
                    // there rather than answered here.
                    if List.length matched > 1 then
                        let matchedSlots = matched |> List.map (fun i -> List.item i slots)

                        let artifact = tieCouldBeSubstitutionArtifact (candidate :: matchedSlots)

                        if artifact then
                            failwith
                                $"TODO: %s{operation}: virtual method %s{method.Name} on %O{concreteTypeInfo} matches %i{List.length matched} base vtable slots that are only identical once the declaring types' generic arguments are substituted; CoreCLR lays out slots on the generic definition, which PawPrint cannot yet walk (see the open-generic TODO in `numVirtuals`)"

                        // Same-type ties are not orderable by derivation either: one type declaring
                        // two virtuals this candidate cannot be told apart from is illegal metadata
                        // unless the signatures differ somewhere the normalisation cannot see.
                        let distinctOwners =
                            matched
                            |> List.map (fun i -> (List.item i slots).DeclaredBy.Identity)
                            |> List.distinct
                            |> List.length

                        if distinctOwners <> List.length matched then
                            failwith
                                $"%s{operation}: virtual method %s{method.Name} on %O{concreteTypeInfo} matches %i{List.length matched} base vtable slots held by the same type, so the most-derived rule cannot order them; the signatures must differ somewhere PawPrint's slot-matching normalisation does not distinguish"

                    match matched with
                    | mostDerived :: _ ->
                        // CoreCLR refuses to load a type whose non-newslot virtual matches a
                        // `final` parent slot: having picked the override candidate out of the
                        // parent chain, `MethodTableBuilder::PlaceVirtualMethods` throws
                        // `IDS_CLASSLOAD_MI_FINAL_DECL` when `IsMdFinal(dwParentAttrs)`
                        // (methodtablebuilder.cpp:5445-5448). The check is against the single method
                        // the lookup returned, which is the most-derived match -- the same slot the
                        // tie-break above selects -- so testing the chosen occupant is upstream's
                        // rule and not an approximation of it.
                        //
                        // Filling the slot anyway would hand out a vtable layout for a type the real
                        // runtime would refuse to load, and every slot number derived from it would
                        // then be answering a question about a type that cannot exist. Roslyn cannot
                        // emit this shape, but -- like the unmatched-override case below -- assembly
                        // version skew can, by sealing a virtual in a base that a derived assembly
                        // was already compiled against.
                        let occupant = List.item mostDerived slots

                        if occupant.Method.IsFinal then
                            failwith
                                $"%s{operation}: virtual method %s{method.Name} on %O{concreteTypeInfo} is not marked newslot and matches vtable slot %i{mostDerived}, which is occupied by the final method %s{occupant.Method.Name} declared by %O{occupant.DeclaredBy}; CoreCLR rejects this type at load time with a TypeLoadException rather than laying out a vtable for it"

                        // Matching signatures are not the whole of the layout rule for a *generic*
                        // method: CoreCLR compares the type parameters' constraints too, and refuses
                        // to load the type if the override demands more of a type argument than the
                        // method it overrides did (`MetaSig::CompareMethodConstraints`,
                        // methodtablebuilder.cpp:5449-5459).
                        //
                        // Like the `final` check above, this belongs *after* the most-derived match
                        // is chosen rather than inside the predicate that finds matches. A base
                        // chain may hold several slots this candidate matches by signature -- `A`
                        // declaring `virtual M<T>()`, `B` hiding it with a `new virtual M<T>()` that
                        // adds a constraint, `C` overriding `B`'s -- and only the one it actually
                        // fills has any say. Comparing against the others would reject ordinary C#.
                        //
                        // Roslyn copies a base method's constraints verbatim onto an override, so a
                        // genuine override always agrees here; assembly version skew and
                        // hand-authored IL are what can disagree.
                        let state, constraintsMatch =
                            if candidate.Method.Generics.IsEmpty then
                                state, true
                            else
                                IlMachineState.methodConstraintsMatch
                                    loggerFactory
                                    baseClassTypes
                                    state
                                    (constraintComparand candidate)
                                    (constraintComparand occupant)

                        if not constraintsMatch then
                            failwith
                                $"%s{operation}: generic method %s{method.Name} on %O{concreteTypeInfo} fills vtable slot %i{mostDerived}, held by %s{occupant.Method.Name} declared by %O{occupant.DeclaredBy}, but its type parameters' constraints do not permit it to override that slot; CoreCLR rejects this type at load time with a TypeLoadException rather than laying out a vtable for it"

                        state, (slots |> List.mapi (fun j slot -> if j = mostDerived then candidate else slot)), fresh
                    | [] ->
                        // "Else, place the method in the next available empty vtable slot"
                        // (methodtablebuilder.cpp:5401). Both kinds of method arrive here: one
                        // marked NewSlot, which skipped the search and is asking for a slot of its
                        // own, and one *not* marked NewSlot whose search came up empty. Upstream
                        // makes no distinction between them -- both go to `AddVirtualMethod` -- and
                        // neither does this.
                        //
                        // The second kind is what F# emits constantly: the structural equality and
                        // comparison members of a union or record are `Public, Final, Virtual,
                        // HideBySig` with no NewSlot, so `Equals(T)` and `CompareTo(object,
                        // IComparer)` match nothing on `Object` and land here. Roslyn never emits
                        // it -- 0 of corelib's 1470 non-generic classes trigger it, measured.
                        //
                        // Appending is the whole of the rule, but it costs a diagnostic: a gap in
                        // `candidateFillsSlot` shows up as a spurious extra slot rather than a
                        // failure here, so what catches one is the slot-by-slot comparison against
                        // the host CLR's own `GetSlot` in TestVirtualMethodSlots -- a check on the
                        // layout rather than merely on its length, because a walk that appends one
                        // slot too many while dropping a real one has the right length.
                        state, slots, candidate :: fresh
                )

            // The fresh slots were accumulated head-first, so undo that once here rather than
            // copying the accumulated vtable on every append.
            let slots = inherited @ List.rev freshReversed

            state, slots

    /// The occupants of the region of a type's method table that follows its vtable, in slot order,
    /// so that the method at index `i` holds slot `numVirtuals + i`.
    ///
    /// This is `MethodTableBuilder::PlaceNonVirtualMethods` (methodtablebuilder.cpp:5255-5359).
    /// Slot numbers come from one monotonic counter shared with the vtable
    /// (`AddNonVirtualMethod` sets the index to `pSlotTable->GetSlotCount()`,
    /// methodtablebuilder.h:1532-1541), and only the parent's *virtual* slots are inherited --
    /// `CopyParentVtable` (methodtablebuilder.cpp:1143) stops at the parent's `GetNumVirtuals()` --
    /// so this region begins at exactly the type's own `GetNumVirtuals()`, however many slots its
    /// base had beyond its vtable. Upstream machine-checks that premise: `PlaceNonVirtualMethods`
    /// opens with `INDEBUG(bmtVT->SealVirtualSlotSection())` and every subsequent add re-seals, so
    /// a debug build asserts that nothing appends to the vtable once this has begun.
    ///
    /// Nothing renumbers a declared method afterwards. `PlaceInterfaceMethods` runs later but adds
    /// no slots -- it only fills in `bmtInterfaceSlotImpl` and the dispatch map. Do not be misled by
    /// the comment above its call site (methodtablebuilder.cpp:1676), which still describes
    /// creating "duplicate slots ... starting at dwCurrentDuplicateVtableSlot": that variable no
    /// longer exists anywhere in the file. The one later addition, `AddUnboxedMethod` for a value
    /// type's unboxed entrypoints (:7178), appends after everything placed from metadata.
    ///
    /// Two assumptions about what the metadata contains, both currently true and neither checked
    /// here. Runtime-async (`g_pConfig->RuntimeAsync()`, off by default) makes
    /// `EnumerateClassMethods` synthesise a second `bmtMDMethod` per Task-returning method, and
    /// those consume slots alongside the declared ones; and EnC adds MethodDescs entirely outside
    /// this file, which PawPrint may ignore because it does not support dynamic code at all (#853).
    ///
    /// The order is *not* MethodDef row order, and every step below is observable.
    /// Verified against the host CLR's own `RuntimeMethodHandle.GetSlot` for every method reflection
    /// can reach: 31064 methods over 2336 corelib types, 5499 over 1153 FSharp.Core types, and 352
    /// over closed generic instantiations, with no disagreement.
    let private slotsBeyondVtableOfClosed
        (operation : string)
        (concreteTypeInfo : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : VtableSlot list
        =
        // The same list the vtable walk is laid out from, so the two cannot disagree about what the
        // type declares; `declaredMethodsOf` documents which rows it drops and why. In particular a
        // vtable-gap marker never reaches the validation below, which would otherwise see a
        // runtime-special-named method that is not a constructor and wrongly report that CoreCLR
        // rejects the type.
        let declared = declaredMethodsOf operation concreteTypeInfo typeInfo

        // `PlaceVirtualMethods` places exactly the declared *instance* virtuals, so everything else
        // is still unplaced when `PlaceNonVirtualMethods` runs. A `static virtual` -- an interface
        // static abstract -- is therefore placed here, which is what upstream's
        // `AddNonVirtualMethod` assertion `!IsMdVirtual(...) || IsMdStatic(...)` asserts. Writing
        // this filter as "not virtual" would silently drop all 41 of `INumberBase<T>`'s static
        // members, which is why that interface is in the layout corpus.
        let unplaced =
            declared
            |> List.filter (fun (method, _) -> not (method.IsVirtual && not method.IsStatic))

        // CoreCLR recognises the two constructors by `IsMdRTSpecialName` *plus* an `ExactlyEqual`
        // match -- name and raw signature blob both -- against hard-coded `static void .cctor()` and
        // `instance void .ctor()` signatures. `declaredMethodsOf` has already refused any
        // runtime-special-named method that is neither, since CoreCLR refuses to load such a type.
        //
        // The flag is not implied by the name: a method merely *named* `.ctor`
        // without it skips that block entirely and is placed in the ordinary pass below.
        // `FakeCtorSecond` in TestFabricatedVtableLayout pins that against the host CLR. ECMA-335
        // II.10.5.1 requires constructors to carry `rtspecialname`, so such an image is invalid --
        // but CoreCLR loads it anyway, and CoreCLR is what this emulates.
        let isRuntimeSpecialName (facts : MetadataMethodFacts) : bool =
            facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName

        // "The signature carries `IMAGE_CEE_CS_CALLCONV_GENERIC`", which is the bit
        // `EnumerateClassMethods` reads to decide `hasGenericMethodArgs`
        // (methodtablebuilder.cpp:2794) -- *not* "the encoded generic arity is positive". ECMA-335
        // requires that arity to be at least 1 when the bit is set, so the two agree on every valid
        // image and no test here distinguishes them; on an invalid-but-loadable one with the bit set
        // and a count of zero, CoreCLR goes by the bit, and the method's pass below would differ.
        let isGenericSignature (method : MethodInfo<_, _, _>) : bool =
            method.Signature.Header.Get.Attributes.HasFlag System.Reflection.Metadata.SignatureAttributes.Generic

        // CoreCLR asks two different questions about a constructor's return type, and the answers
        // come apart on exactly one shape. `ValidateMethods` rejects a ctor whose return is not void
        // using `MetaSig::GetReturnType()`, which reaches `SigParser::PeekElemTypeClosed` and calls
        // `SkipCustomModifiers()` first (sigparser.h:225) -- so `modopt(X) void` *is* void there, and
        // such a type loads happily; measured on the host, which instantiates one. That question is
        // `declaredMethodsOf`'s, since it decides whether the type loads at all.
        //
        // This one is the other: `pDefaultCtor` is set by `ExactlyEqual` against the hard-coded
        // `instance void ()`, a raw *blob* comparison, in which a modifier makes the signature
        // different -- so the same constructor does not get the priority slot. `ModoptVoidCtor` in
        // TestFabricatedVtableLayout needs both, and collapsing either into the other kills it.
        //
        // Matching the blob also means the calling convention and generic arity are part of the
        // test, not just the arity: a vararg or (illegal-but-loadable) generic `.ctor()` is not the
        // default constructor either.
        let hasNullaryVoidSignature (method : MethodInfo<_, _, _>) : bool =
            method.Signature.ParameterTypes.IsEmpty
            && not (isGenericSignature method)
            && method.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.Default
            && method.Signature.ReturnType = MethodReturnType.Void

        let isClassConstructor ((method, facts) : MethodInfo<_, _, _> * MetadataMethodFacts) : bool =
            isRuntimeSpecialName facts
            && method.IsStatic
            && method.Name = ".cctor"
            && hasNullaryVoidSignature method

        let isDefaultConstructor ((method, facts) : MethodInfo<_, _, _> * MetadataMethodFacts) : bool =
            isRuntimeSpecialName facts
            && not method.IsStatic
            && method.Name = ".ctor"
            && hasNullaryVoidSignature method

        // Steps 1 and 2: the class constructor, then the parameterless instance constructor, ahead
        // of everything else whatever their MethodDef rows say. Upstream places them first because
        // `MethodTable::GetCCtorSlot` and `GetDefaultCtorSlot` are *defined* as those two positions.
        // `System.Type` is the corpus witness for both halves at once: it declares its `.cctor` at
        // row 2639, its default ctor at row 2438, and other methods from row 2431, so it
        // discriminates cctor-before-ctor *and* ctor-before-row-order. `Lazy`1` is the witness that
        // the rule still holds on a generic type, where every other method is placed in the first
        // pass below and could otherwise have swallowed the ctors with it.
        //
        // At most *one* row is hoisted for each. `ValidateMethods` records them by plain assignment
        // inside its loop -- `bmtVT->pCCtor = *it` (methodtablebuilder.cpp:5019) and
        // `bmtVT->pDefaultCtor = *it` (:5042) -- so when a type declares the same constructor twice,
        // which ECMA-335 II.22.26 forbids but CoreCLR loads anyway, the *last* matching row wins and
        // the earlier ones are placed in the ordinary pass like any other method. Measured: a type
        // with `Plain` then two identical `.ctor()` rows gives the last `.ctor` slot 4 and leaves the
        // earlier one at slot 6, *after* `Plain`. Hoisting both would move everything after them.
        let lastMatching (predicate : MethodInfo<_, _, _> * MetadataMethodFacts -> bool) =
            unplaced |> List.filter predicate |> List.tryLast

        let placedFirst =
            [ lastMatching isClassConstructor ; lastMatching isDefaultConstructor ]
            |> List.choose id

        let hoisted = placedFirst |> List.map (fun (method, _) -> method.IdentityKey)

        let stillUnplaced =
            unplaced
            |> List.filter (fun (method, _) -> not (hoisted |> List.contains method.IdentityKey))

        // Steps 3 and 4: two passes, each in row order. Upstream's vocabulary for them is worth
        // knowing, because it cuts across the name of this function: the first pass places methods
        // that need a *real vtable slot* and freezes `bmtVT->cVtableSlots` after itself, so only
        // pass-2 methods are what CoreCLR calls "non-vtable slots". Both regions are past
        // `GetNumVirtuals` and both are returned here. The boundary between them is deliberately
        // not exposed -- nothing PawPrint models reads `cVtableSlots` -- and the split is modelled
        // only because it decides the numbering.
        //
        // `fCanHaveNonVtableSlots` is false for a generic type and for an interface, so both place
        // everything in the first pass and leave the second empty. `mcInstantiated` is exactly "the
        // signature carries `IMAGE_CEE_CS_CALLCONV_GENERIC`" (methodtablebuilder.cpp:2794, 3235-3238):
        // the delegate and P/Invoke arms are tried first, but a generic method reaching one of them
        // is rejected outright by the `BFA_GENERIC_METHODS_INST` guard at :3273, so on a loadable
        // image the two coincide. `GenericParameterCount` is read from the same signature blob
        // rather than from the GenericParam rows, so this is that predicate and not a proxy for it.
        //
        // So on a non-generic class a generic method is numbered *ahead* of a non-generic one
        // declared earlier: `System.Version` puts its four generic methods at slots 12-15 and starts
        // everything else at 16, though its lowest-numbered row is among the latter.
        let canHaveNonVtableSlots =
            concreteTypeInfo.Generics.IsEmpty && not typeInfo.IsInterface

        let needsRealSlot ((method, _) : MethodInfo<_, _, _> * MetadataMethodFacts) : bool =
            not canHaveNonVtableSlots || isGenericSignature method

        let realSlots, rest = stillUnplaced |> List.partition needsRealSlot

        placedFirst @ realSlots @ rest
        |> List.map (fun (method, _) ->
            {
                VtableSlot.Method = method
                // Slots beyond the vtable are never inherited, so the declaring type is always this
                // one -- unlike a vtable slot, which routinely still holds a base type's method.
                VtableSlot.DeclaredBy = concreteTypeInfo
            }
        )

    /// A closed type's whole method table, as CoreCLR's `bmtVT->pSlotTable`: the vtable proper,
    /// followed by the region `PlaceNonVirtualMethods` fills. Slot numbers run across the two
    /// without a break, and `cVirtualSlots` -- `MethodTable::GetNumVirtuals()` -- is the length of
    /// the first.
    ///
    /// Kept as two lists rather than one, with `slotIndexInTable` owning the arithmetic that joins
    /// them, because the two halves answer different questions and the BCL asks both: `GetSlot`
    /// indexes the concatenation while `GetNumVirtuals` is the prefix length, and
    /// `PopulateProperties` *compares* the two to decide whether an accessor is virtual. A single
    /// flat list would lose the boundary the comparison is about; making the caller add an offset
    /// would scatter that arithmetic across call sites.
    ///
    /// The second field is named for the boundary rather than for virtualness on purpose: it holds
    /// every `static virtual` the type declares, those being placed outside the vtable, so calling
    /// it "non-virtual" would be false of its contents.
    type MethodSlotTable =
        {
            /// Slots `0 .. Vtable.Length - 1`. This length is `MethodTable::GetNumVirtuals()`.
            Vtable : VtableSlot list
            /// Slots `Vtable.Length` upwards.
            BeyondVtable : VtableSlot list
        }

    let slotTableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * MethodSlotTable
        =
        // Only the vtable walk recurses through the base chain; the region beyond it is this type's
        // alone, so it is computed once here rather than once per ancestor and discarded.
        let state, virtualSlots =
            vtableOfClosed loggerFactory baseClassTypes operation state concreteType

        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // TypeDescs with no MethodTable, so no slots of either kind -- the same
            // reason `vtableOfClosed` gives them an empty vtable.
            state,
            {
                MethodSlotTable.Vtable = virtualSlots
                MethodSlotTable.BeyondVtable = []
            }
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // A synthesised array MethodTable really does carry slots beyond its vtable, for the
            // intrinsic Get/Set/Address and the ctor, and PawPrint models none of them --
            // `introducedMethodsOfClosed` refuses the same question for the same reason. Answering
            // "none" would be a wrong answer rather than an absent one, so refuse. Unreachable from
            // `GetSlot` today: a method handle always resolves to a `Concrete` declaring type, there
            // being no way to mint one naming an array intrinsic.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{concreteType}; the array intrinsic methods (Get/Set/Address/.ctor) occupy slots beyond the vtable that PawPrint does not model"
        | ConcreteTypeHandle.Concrete _ ->
            let concreteTypeInfo, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            state,
            {
                MethodSlotTable.Vtable = virtualSlots
                MethodSlotTable.BeyondVtable = slotsBeyondVtableOfClosed operation concreteTypeInfo typeInfo
            }

    /// What identifies a vtable slot's occupant well enough to find it again: the full name of the
    /// assembly that declares the method, paired with the method's within-assembly identity.
    ///
    /// The assembly is not decoration. `MethodInfo.IdentityKey` is a MethodDef *row number*, which
    /// is unique only within its own module, and a vtable routinely spans assemblies -- a guest type
    /// deriving from `System.Object` has corelib's rows sitting underneath its own. Row 6 of the
    /// guest and row 6 of corelib are different methods that compare equal on `IdentityKey` alone.
    let slotIdentity
        (slot : VtableSlot)
        : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option)
        =
        slot.DeclaredBy.Assembly.FullName, slot.Method.IdentityKey

    /// The index of the slot occupied by the method with the given identity, or `None`.
    let slotIndexOfIdentity
        (target : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option))
        (slotIdentities :
            (string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option)) list)
        : int option
        =
        slotIdentities |> List.tryFindIndex (fun identity -> identity = target)

    /// The slot CoreCLR assigns a method in its declaring type's method table -- `MethodDesc::GetSlot`
    /// -- or `None` if the method holds no slot there at all.
    ///
    /// The one place the two halves of a `MethodSlotTable` are joined into a single numbering, which
    /// is the point of routing every query through here rather than letting callers add the offset.
    ///
    /// `None` is not "not virtual": every method a type declares in metadata occupies a slot, in one
    /// half or the other. It means the method is not this type's at all -- a synthesised method,
    /// which has no MethodDef row and so is never placed, or a lookup against the wrong type.
    let slotIndexInTable
        (target : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option))
        (table : MethodSlotTable)
        : int option
        =
        match slotIndexOfIdentity target (table.Vtable |> List.map slotIdentity) with
        | Some index -> Some index
        | None ->
            slotIndexOfIdentity target (table.BeyondVtable |> List.map slotIdentity)
            |> Option.map (fun index -> List.length table.Vtable + index)

    /// The size of the instance vtable for a closed type, matching CoreCLR's
    /// `MethodTable::GetNumVirtuals()`.
    let numVirtualsOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * int
        =
        // The length of `vtableOfClosed` by definition rather than an independently-computed sum,
        // because `PopulateMethods` compares it against `RuntimeMethodHandle.GetSlot`'s answer:
        // two walks that had to agree by discipline would disagree silently, and the symptom
        // would be a wrong `isVirtual` rather than a crash.
        let state, slots =
            vtableOfClosed loggerFactory baseClassTypes operation state concreteType

        state, List.length slots

    let numVirtuals
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : IlMachineState * int
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            // CoreCLR's GetNumVirtuals asserts !typeHandle.IsGenericVariable(); the BCL's
            // RuntimeType.GetMethodCandidates strips generic variables before calling.
            // Reaching here means a managed-side invariant was violated.
            failwith
                $"%s{operation}: invoked on type-generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before calling"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"%s{operation}: invoked on method-generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before calling"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            failwith
                $"TODO: %s{operation} for open generic type definition %O{identity}; need to walk the metadata-level method list and base-type chain without concretising"
        | RuntimeTypeHandleTarget.Closed handle ->
            numVirtualsOfClosed loggerFactory baseClassTypes operation state handle

    /// The methods a declaring type introduces, as CoreCLR's `IntroducedMethodIterator` walks
    /// them: the type's own MethodDef rows in metadata order, never an inherited one.
    ///
    /// Returns the defining assembly and the declaring target alongside them, because those are
    /// what `MethodHandleRegistry.getOrAllocateInternalHandle` needs to mint a handle and they
    /// differ between the closed and open-definition cases.
    ///
    /// `None` means "this type has no MethodTable, so it introduces nothing" — byref, pointer and
    /// function-pointer TypeDescs. Callers should emit the null sentinel so the managed
    /// `IntroducedMethodEnumerator` terminates immediately.
    let introducedMethodsOf
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : (string *
          RuntimeTypeHandleTarget *
          MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list) option
        =
        match target with
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            let concreteType, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state handle
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                )

            Some (concreteType.Assembly.FullName, target, typeInfo.Methods)
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            // CoreCLR's typical instantiation of `G<>` is a MethodTable carrying the definition's
            // own TypeDef token, and its MethodDescChunks hold the definition's MethodDefs. So the
            // answer is the metadata method list read straight off the typedef: no instantiation is
            // needed, which is what makes this answerable where `numVirtuals` is not — that needs
            // to *match* signatures across the base chain, and this only needs to list them.
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: assembly %s{identity.AssemblyFullName} is not loaded"
                )

            let typeInfo = Assembly.resolveTypeIdentityDefinition assembly identity

            Some (identity.AssemblyFullName, target, typeInfo.Methods)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _) ->
            // CoreCLR's IntroducedMethodIterator runs on a MethodTable; byrefs/pointers/function-
            // pointers are TypeDescs with no MethodTable, so GetFirstIntroducedMethod returns null
            // and the managed enumerator terminates without iterating.
            None
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // Also TypeVarTypeDescs, and CoreCLR agrees they introduce nothing:
            // `PopulateConstructors` returns an empty array for `IsGenericParameter`
            // (RuntimeType.CoreCLR.cs:755) rather than iterating.
            None
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _ as handle)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _ as handle) ->
            // Synthesised array MethodTables have a small fixed set of introduced methods (Get/Set/
            // Address/the parameterless ctor). PawPrint does not yet model these; no test exercises
            // this path, so fail loudly to flag the gap rather than silently reporting zero.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{handle}; need to surface the array's intrinsic Get/Set/Address methods"
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"

    let getOrAllocateNonGenericRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ManagedHeapAddress * IlMachineState
        =
        if not typeInfo.Generics.IsEmpty then
            failwith
                $"getOrAllocateNonGenericRuntimeType: expected non-generic runtime type for %s{typeInfo.Name}, but metadata has %i{typeInfo.Generics.Length} generic parameters"

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeInfo

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                typeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, stk))

        IlMachineState.getOrAllocateType loggerFactory baseClassTypes (RuntimeTypeHandleTarget.Closed typeHandle) state

    let declaringTypeInfo
        (operation : string)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        if not typeInfo.IsNested then
            None
        else
            let assembly =
                state.LoadedAssembly typeInfo.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: declaring assembly is not loaded: %s{typeInfo.Assembly.FullName}"
                )

            Some assembly.TypeDefs.[typeInfo.DeclaringType]

    /// Compute the declaring type's `RuntimeTypeHandleTarget` for a given typedef.
    /// Returns `None` if the typedef is not a nested type. For nested types whose
    /// declaring type is non-generic, the result is a `Closed` target (the typedef
    /// is concretized into the registry on the way out, mutating state). For nested
    /// types whose declaring type is generic, the result is the `OpenGenericTypeDefinition`
    /// — matching CoreCLR's behaviour of returning the parent typedef rather than a
    /// closed instantiation (e.g. `Outer<int>.Inner`'s DeclaringType is `Outer<>`).
    let declaringTypeHandleTargetForTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : RuntimeTypeHandleTarget option * IlMachineState
        =
        match declaringTypeInfo "RuntimeTypeHandle.GetDeclaringType" state typeInfo with
        | None -> None, state
        | Some declaringTypeInfo when declaringTypeInfo.Generics.IsEmpty ->
            let stk =
                DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies declaringTypeInfo

            let state, typeHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    declaringTypeInfo.Assembly
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (declaringTypeInfo.Identity, stk))

            Some (RuntimeTypeHandleTarget.Closed typeHandle), state
        | Some declaringTypeInfo ->
            Some (RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringTypeInfo.Identity), state

    let getOrAllocateDeclaringRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ManagedHeapAddress option * IlMachineState
        =
        match declaringTypeHandleTargetForTypeInfo loggerFactory baseClassTypes state typeInfo with
        | None, state -> None, state
        | Some target, state ->
            let addr, state =
                IlMachineState.getOrAllocateType loggerFactory baseClassTypes target state

            Some addr, state

    let declaringRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : ManagedHeapAddress option * IlMachineState
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery "RuntimeTypeHandle.declaringRuntimeType" scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"RuntimeTypeHandle.GetDeclaringType: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            getOrAllocateDeclaringRuntimeType loggerFactory baseClassTypes state typeInfo
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, _) ->
            // The DeclaringType of a type-generic parameter is the open generic type
            // that declares it, not that type's enclosing type. CoreCLR exposes the
            // same RuntimeType you would get from typeof(...): going through the
            // structural-equality registry preserves reference equality with the
            // existing OpenGenericTypeDefinition allocation.
            let addr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringType)
                    state

            Some addr, state
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, _declaringMethod, _) ->
            // The DeclaringType of a method-generic parameter is the type that
            // declares the method. CoreCLR's TypeVarTypeDesc::GetDeclaringType
            // returns the owning method's declaring type for method-level params.
            // When the declaring type itself is non-generic, allocate it as a
            // closed RuntimeType rather than OpenGenericTypeDefinition, because
            // OpenGenericTypeDefinition would incorrectly report IsGenericType=true.
            let assembly =
                state.LoadedAssembly declaringType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"RuntimeTypeHandle.GetDeclaringType: assembly for method generic parameter declaring type is not loaded: %s{declaringType.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

            if typeInfo.Generics.IsEmpty then
                let addr, state =
                    getOrAllocateNonGenericRuntimeType loggerFactory baseClassTypes state typeInfo

                Some addr, state
            else
                let addr, state =
                    IlMachineState.getOrAllocateType
                        loggerFactory
                        baseClassTypes
                        (RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringType)
                        state

                Some addr, state
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> None, state
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"RuntimeTypeHandle.GetDeclaringType: concrete type handle was not registered: %O{typeHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"RuntimeTypeHandle.GetDeclaringType: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                getOrAllocateDeclaringRuntimeType loggerFactory baseClassTypes state typeInfo

    let baseRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : ManagedHeapAddress option * IlMachineState
        =
        let baseHandle, state =
            match typeHandleTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery "RuntimeTypeHandle.baseRuntimeType" scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                let assembly =
                    state.LoadedAssembly identity.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"RuntimeTypeHandle.GetBaseType: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]

                match typeInfo.BaseType with
                | None -> None, state
                | Some baseTypeInfo ->
                    let state, baseAssembly, baseTypeDefn =
                        IlMachineState.resolveBaseTypeInfo loggerFactory baseClassTypes state assembly baseTypeInfo

                    let state, baseHandle =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            baseTypeDefn

                    Some baseHandle, state
            | RuntimeTypeHandleTarget.Closed typeHandle ->
                match typeHandle with
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ -> None, state
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    let state, baseHandle =
                        IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state typeHandle

                    baseHandle, state
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                failwith
                    $"TODO: RuntimeTypeHandle.GetBaseType for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"TODO: RuntimeTypeHandle.GetBaseType for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

        match baseHandle with
        | None -> None, state
        | Some baseHandle ->
            let addr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.Closed baseHandle)
                    state

            Some addr, state

    /// Returns the element handle of an array/byref/pointer wrapper, or None for
    /// concrete types and open generic type definitions. This mirrors the .NET
    /// rule that Type.GetElementType() returns null for anything that is not
    /// an array, pointer, or by-ref type.
    let elementRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : ManagedHeapAddress option * IlMachineState
        =
        let elementHandle =
            match typeHandleTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery "RuntimeTypeHandle.elementRuntimeType" scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> None
            // A generic parameter is not an array/pointer/byref, so GetElementType returns null.
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> None
            | RuntimeTypeHandleTarget.Closed typeHandle ->
                match typeHandle with
                | ConcreteTypeHandle.Concrete _ -> None
                // Function pointers expose no element type: there's no single referenced
                // element type to surface (they're parametrised by a whole signature),
                // and CoreCLR's IsFunctionPointer/GetFunctionPointerXxx APIs are the
                // proper way to inspect them. Mirror Type.GetElementType() == null.
                | ConcreteTypeHandle.FunctionPointer _ -> None
                | ConcreteTypeHandle.Byref inner
                | ConcreteTypeHandle.Pointer inner
                | ConcreteTypeHandle.OneDimArrayZero inner -> Some inner
                // Multi-dim arrays drop the rank: typeof(int[,]).GetElementType()
                // returns typeof(int), not typeof(int[]).
                | ConcreteTypeHandle.Array (inner, _) -> Some inner

        match elementHandle with
        | None -> None, state
        | Some inner ->
            let addr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.Closed inner)
                    state

            Some addr, state

    let findCorelibType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (``namespace`` : string)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        baseClassTypes.Corelib.TypeDefs
        |> Seq.choose (fun (KeyValue (_, typeInfo)) ->
            if typeInfo.Namespace = ``namespace`` && typeInfo.Name = name then
                Some typeInfo
            else
                None
        )
        |> Seq.exactlyOne

    let concretizeNonGenericCorelibType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (``namespace`` : string)
        (name : string)
        : IlMachineState * TypeInfo<GenericParamFromMetadata, TypeDefn> * ConcreteTypeHandle
        =
        let typeInfo = findCorelibType baseClassTypes ``namespace`` name

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeInfo

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, stk))

        state, typeInfo, typeHandle

    /// Render a method for a diagnostic: its declaring type and name, plus its MethodDef token, so
    /// that overloads sharing a name stay distinguishable.
    let private describeMethodDefinition
        (assembly : DumpedAssembly)
        (declaringTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (handle : System.Reflection.Metadata.MethodDefinitionHandle)
        (methodInfo : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let token =
            let handle : System.Reflection.Metadata.EntityHandle =
                System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle

            System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

        let declaringTypeName =
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) declaringTypeInfo

        $"%s{declaringTypeName}::%s{methodInfo.Name} (MethodDef 0x%08x{token})"

    /// The types a generic parameter is constrained to be assignable to, in metadata row order.
    /// This is exactly what <c>RuntimeType.GetGenericParameterConstraints</c> reports, and the list
    /// <c>RuntimeType.GetBaseType</c> picks a type variable's base type out of.
    ///
    /// <paramref name="target"/> must name a generic parameter, of a type or of a method. What any
    /// other shape means is the caller's to decide: for a QCall that is a fact about its managed
    /// wrapper's guards, not about constraints.
    ///
    /// A constraint that mentions a type variable cannot be concretised, so it comes back as a
    /// parameter target (<c>where T2 : T1</c>) or as an open constructed type whose arguments are
    /// themselves targets (<c>where T : IComparable&lt;T&gt;</c>), recursively.
    let genericParameterConstraintTargets
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * RuntimeTypeHandleTarget list
        =
        let declaringType =
            match target with
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, _)
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, _, _) -> declaringType
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.DynamicMethodsClass _ ->
                failwith
                    $"%s{operation}: genericParameterConstraintTargets requires a generic-parameter target, got %O{target}"

        let assembly =
            state.LoadedAssembly declaringType.Assembly
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: assembly for the declaring type of %O{target} is not loaded: %s{declaringType.AssemblyFullName}"
            )

        let declaringTypeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

        let declaringTypeName =
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) declaringTypeInfo

        // `!!n` inside a constraint signature names the owning *method*'s n-th formal, so it can be
        // given a target only under a method owner; `methodVariable` is `None` for a type owner.
        // ECMA-335 §II.10.1.7 scopes a type parameter's constraints to the type, so no signature
        // the metadata model permits spells `!!n` there, and the `None` case stays a loud failure.
        let ownerDescription, parameterMetadata, methodVariable =
            match target with
            | RuntimeTypeHandleTarget.GenericParameter (_, position) ->
                if position < 0 || position >= declaringTypeInfo.Generics.Length then
                    failwith
                        $"%s{operation}: generic parameter position %d{position} is out of range for %s{declaringTypeName}, which declares %d{declaringTypeInfo.Generics.Length} parameter(s)"

                let description = $"type-generic parameter #%d{position} of %s{declaringTypeName}"

                description, snd declaringTypeInfo.Generics.[position], None
            | RuntimeTypeHandleTarget.MethodGenericParameter (_, declaringMethod, position) ->
                let methodInfo = assembly.Methods.[declaringMethod.Get]

                let methodDescription =
                    describeMethodDefinition assembly declaringTypeInfo declaringMethod.Get methodInfo

                // The MethodDef row is read out of the *declaring type's* assembly, and a
                // constraint's `!n` is resolved against that same type's formals. A target pairing
                // a method with a type that does not declare it would therefore answer about some
                // other method's parameter list rather than fail.
                match methodInfo.TryDeclaringType with
                | Some owner when owner.Identity = declaringType -> ()
                | Some owner ->
                    failwith
                        $"%s{operation}: %s{methodDescription} is declared on %O{owner.Identity}, but %O{target} names %s{declaringTypeName} as its declaring type"
                | None ->
                    failwith
                        $"%s{operation}: %s{methodDescription} has no declaring type, so it cannot be a method of %s{declaringTypeName} as %O{target} claims"

                if position < 0 || position >= methodInfo.Generics.Length then
                    failwith
                        $"%s{operation}: method-generic parameter position %d{position} is out of range for %s{methodDescription}, which declares %d{methodInfo.Generics.Length} parameter(s)"

                let description = $"method-generic parameter #%d{position} of %s{methodDescription}"

                let methodVariable (index : int) : RuntimeTypeHandleTarget =
                    if index < 0 || index >= methodInfo.Generics.Length then
                        failwith
                            $"%s{operation}: a constraint on %s{description} names method-generic parameter !!%d{index}, but that method declares only %d{methodInfo.Generics.Length} parameter(s)"

                    RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, index)

                description, snd methodInfo.Generics.[position], Some methodVariable
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.DynamicMethodsClass _ ->
                failwith
                    $"logic error: %s{operation}: %O{target} is not a generic-parameter target, which binding `declaringType` above has already refused"

        // Detect constraints that *embed* a generic parameter inside a structural shape
        // (e.g. `where T : IEnumerable<T>` decoded as `GenericInstantiation(IEnumerable,
        // [GenericTypeParameter 0])`). Concretizing such a shape would require binding
        // parameters to parameter targets, which our concretization machinery doesn't
        // model. Detect up front and fail with a pointed TODO rather than letting
        // concretizeType raise IndexOutOfRangeException from deep in the resolver.
        let rec embedsTypeParameter (ty : TypeDefn) : bool =
            match ty with
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _ -> true
            | TypeDefn.Array (element, _)
            | TypeDefn.Pinned element
            | TypeDefn.Pointer element
            | TypeDefn.Byref element
            | TypeDefn.OneDimensionalArrayLowerBoundZero element -> embedsTypeParameter element
            | TypeDefn.Modified m -> embedsTypeParameter m.Unmodified || embedsTypeParameter m.Modifier
            | TypeDefn.GenericInstantiation (generic, args) ->
                embedsTypeParameter generic || (args |> Seq.exists embedsTypeParameter)
            | TypeDefn.FunctionPointer signature ->
                let returnContains =
                    match signature.ReturnType with
                    | MethodReturnType.Void -> false
                    | MethodReturnType.Returns ret -> embedsTypeParameter ret

                returnContains || (signature.ParameterTypes |> List.exists embedsTypeParameter)
            | TypeDefn.PrimitiveType _
            | TypeDefn.FromReference _
            | TypeDefn.FromDefinition _
            | TypeDefn.Void -> false

        // Resolve the head of a generic instantiation to the canonical identity of its
        // definition. Identity, not spelling: the same definition reached via
        // `FromDefinition` and via a `FromReference` in some other assembly must produce
        // one `ResolvedTypeIdentity`, because `TypeHandleRegistry` keys guest `Type`
        // object identity on the resulting target.
        let resolveDefinitionIdentity
            (state : IlMachineState)
            (genericDef : TypeDefn)
            : IlMachineState * ResolvedTypeIdentity
            =
            match genericDef with
            | TypeDefn.FromDefinition (identity, _) -> state, identity
            | _ ->
                let state, _, resolved =
                    IlMachineState.resolveTypeFromDefn
                        loggerFactory
                        baseClassTypes
                        genericDef
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        assembly
                        state

                state, resolved.Identity

        // Map one constraint signature to the target that names it. A constraint that
        // mentions no generic parameter is an ordinary closed type; one that does is an
        // open constructed type, whose arguments are themselves targets — recursively,
        // since `where T : IComparable<List<T>>` is legal.
        let rec constraintTarget (state : IlMachineState) (ty : TypeDefn) : IlMachineState * RuntimeTypeHandleTarget =
            match ty with
            | TypeDefn.GenericTypeParameter idx ->
                // `!n` names the *declaring type's* n-th formal under either owner: a method
                // parameter's constraint may mention the enclosing type's parameters, as in
                // `class C<TOuter> { void M<T>() where T : TOuter {} }`.
                if idx < 0 || idx >= declaringTypeInfo.Generics.Length then
                    failwith
                        $"%s{operation}: a constraint on %s{ownerDescription} names type-generic parameter !%d{idx}, but %s{declaringTypeName} declares only %d{declaringTypeInfo.Generics.Length} parameter(s)"

                state, RuntimeTypeHandleTarget.GenericParameter (declaringType, idx)
            | TypeDefn.GenericMethodParameter idx ->
                match methodVariable with
                | Some mapMethodVariable -> state, mapMethodVariable idx
                | None ->
                    failwith
                        $"%s{operation}: %s{ownerDescription} declares a method-generic parameter constraint !!%d{idx}; impossible without a method context"
            | TypeDefn.GenericInstantiation (genericDef, args) when embedsTypeParameter ty ->
                let state, definition = resolveDefinitionIdentity state genericDef

                let state, argumentTargets =
                    ((state, []), args)
                    ||> Seq.fold (fun (state, acc) arg ->
                        let state, target = constraintTarget state arg
                        state, target :: acc
                    )

                // `openConstructed` is what keeps this canonical: it collapses the
                // typical instantiation (the CRTP `where T : ISelf<T>`) back to the bare
                // definition, exactly as CoreCLR's class loader does, so the guest sees
                // one `Type` object rather than two.
                state, RuntimeTypeHandleTarget.openConstructed definition (List.rev argumentTargets)
            | _ when embedsTypeParameter ty ->
                failwith
                    $"TODO: %s{operation}: constraint %O{ty} on %s{ownerDescription} embeds a generic parameter beneath an array, pointer, byref or function-pointer shape; only generic instantiations are represented today (`RuntimeTypeHandleTarget.OpenConstructed`)"
            | _ ->
                let state, handle =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        assembly.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty

                state, RuntimeTypeHandleTarget.Closed handle

        // No variance validation happens here. CoreCLR's `TypeVarTypeDesc::LoadConstraints` runs
        // `EEClass::CheckVarianceInSig` over each TypeSpec constraint of a method declared on a
        // variant type, and throws TypeLoadException on violation; PawPrint validates variance
        // nowhere, and C# rejects the violating shape, so only hand-written IL could tell.
        // Closed (non-parameter) constraints are concretized against the declaring
        // assembly with no generic context: a constraint like `where T : List<int>`
        // resolves to the closed type. Constraints that reference a generic
        // parameter (e.g. `where T2 : T1`) are surfaced as parameter targets directly,
        // because concretizeType cannot bind a parameter back to a parameter target.
        let baseTargets, state =
            ((List.empty, state), parameterMetadata.Constraints)
            ||> Seq.fold (fun (acc, state) ty ->
                match ty with
                | TypeDefn.GenericTypeParameter _
                | TypeDefn.GenericMethodParameter _
                | TypeDefn.GenericInstantiation _ ->
                    let state, target = constraintTarget state ty
                    target :: acc, state
                | _ when embedsTypeParameter ty ->
                    let state, target = constraintTarget state ty
                    target :: acc, state
                | _ ->
                    let state, handle =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            ty

                    RuntimeTypeHandleTarget.Closed handle :: acc, state
            )

        let baseTargets = List.rev baseTargets

        // GenericParameter.fs filters out the synthetic System.ValueType row that Roslyn
        // emits alongside the NotNullableValueTypeConstraint flag for `where T : struct`,
        // but only the TypeRef/TypeDef forms — a `where T : unmanaged` constraint encodes
        // ValueType as a TypeSpec wrapped in an `IsUnmanaged` modreq, which the filter
        // doesn't recognise. Append the synthetic row only when no existing entry already
        // resolves to System.ValueType, matching reflection's behaviour of returning
        // exactly one ValueType for both `struct` and `unmanaged` constraints.
        //
        // Appending at the *end* is what Roslyn's row order calls for: the synthetic row it
        // filters out is emitted last, while the `unmanaged` TypeSpec — which survives the
        // filter and so keeps its own position — is emitted first.
        match parameterMetadata.Constraint with
        | Some GenericConstraint.NonNullableValue ->
            let state, _, valueTypeHandle =
                concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System" "ValueType"

            let alreadyHasValueType =
                baseTargets
                |> List.exists (fun t ->
                    match t with
                    | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                        RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
                    | RuntimeTypeHandleTarget.Closed h -> h = valueTypeHandle
                    // An open constructed type is never System.ValueType, which is
                    // non-generic.
                    | RuntimeTypeHandleTarget.OpenConstructed _
                    | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                    | RuntimeTypeHandleTarget.GenericParameter _
                    | RuntimeTypeHandleTarget.MethodGenericParameter _ -> false
                )

            if alreadyHasValueType then
                state, baseTargets
            else
                state, baseTargets @ [ RuntimeTypeHandleTarget.Closed valueTypeHandle ]
        | Some GenericConstraint.Reference
        | None -> state, baseTargets

    let allocateManagedObjectOfConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (typeHandle : ConcreteTypeHandle)
        : ManagedHeapAddress * IlMachineState
        =
        // `typeInfo` is redundant with `typeHandle` — it is the TypeDef the handle resolves
        // to — but callers here have it to hand already, so it stays in the signature and is
        // checked against the handle rather than trusted.
        let resolved =
            AllConcreteTypes.lookup typeHandle state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith
                    $"allocateManagedObjectOfConcreteType: ConcreteTypeHandle %O{typeHandle} not found in AllConcreteTypes"
            )

        // Full identity, not just the TypeDef row: row numbers collide freely across assemblies,
        // so comparing only the handle would let a `typeInfo` from an unrelated assembly through
        // the check this claims to make.
        if resolved.Identity <> typeInfo.Identity then
            failwith
                $"allocateManagedObjectOfConcreteType: handle %O{typeHandle} resolves to a different TypeDef than the supplied %s{typeInfo.Namespace}.%s{typeInfo.Name}"

        IlMachineState.allocateUninitialisedInstance loggerFactory baseClassTypes typeHandle state

    /// Read one element of a `TypeHandle*` instantiation buffer and return the
    /// closed `ConcreteTypeHandle` it points to. Open generic type-parameter
    /// references aren't yet representable here and fail loudly.
    let readTypeHandleInstantiationElement
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        : ConcreteTypeHandle
        =
        let ptr = nativeIntElementPointer operation baseClassTypes buffer index

        match
            IlMachineState.readManagedByref baseClassTypes state ptr
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle))) ->
            handle
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity))) ->
            failwith $"TODO: %s{operation} with open generic type argument %O{identity}"
        | other -> failwith $"%s{operation}: expected TypeHandlePtr in instantiation buffer, got %O{other}"

    /// Instantiate `genericDefinition` with `genericArguments`, producing a fresh
    /// closed `ConcreteTypeHandle`. Mirrors CoreCLR's `Instantiate(...)` step:
    /// canonicalise to the open generic definition first, then re-instantiate.
    let instantiateOpenGenericTypeDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (genericDefinition : ResolvedTypeIdentity)
        (genericArguments : ConcreteTypeHandle list)
        : ConcreteTypeHandle * IlMachineState
        =
        let assembly =
            state.LoadedAssembly genericDefinition.Assembly
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: assembly for open generic type definition is not loaded: %s{genericDefinition.AssemblyFullName}"
            )

        let typeInfo = assembly.TypeDefs.[genericDefinition.TypeDefinition.Get]

        if typeInfo.Generics.Length <> genericArguments.Length then
            failwith
                $"%s{operation}: generic arity mismatch for %s{typeInfo.Namespace}.%s{typeInfo.Name}; definition has %i{typeInfo.Generics.Length} parameters, but call supplied %i{genericArguments.Length} arguments"

        let signatureTypeKind =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeInfo

        let genericDefn = TypeDefn.FromDefinition (genericDefinition, signatureTypeKind)

        let genericArgDefns =
            genericArguments
            |> List.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, instantiatedHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                genericDefinition.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.GenericInstantiation (genericDefn, genericArgDefns))

        instantiatedHandle, state

    /// Re-instantiate a `RuntimeTypeHandleTarget` with the given closed generic
    /// arguments. Closed concrete handles are canonicalised to their open generic
    /// definition before re-instantiation (matching CoreCLR's
    /// `genericType.GetCanonicalMethodTable().Instantiate(...)` step). Structural
    /// wrappers (byref / pointer / array) have no instantiation and so fail loudly.
    let instantiateGenericRuntimeTypeTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        (genericArguments : ConcreteTypeHandle list)
        : ConcreteTypeHandle * IlMachineState
        =
        match target with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            instantiateOpenGenericTypeDefinition loggerFactory baseClassTypes operation state identity genericArguments
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as typeHandle) ->
            let concreteType =
                AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                )

            instantiateOpenGenericTypeDefinition
                loggerFactory
                baseClassTypes
                operation
                state
                concreteType.Identity
                genericArguments
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _) ->
            failwith $"TODO: %s{operation} for structural RuntimeTypeHandleTarget %O{target}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            // A generic parameter is not itself a generic type definition, so it cannot be
            // instantiated. Real CoreCLR throws ArgumentException; here we surface the misuse
            // with a TODO until the caller path is exercised.
            failwith
                $"TODO: %s{operation}: cannot instantiate generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: %s{operation}: cannot instantiate method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

    /// Open-generic type-definition behind a `RuntimeTypeHandleTarget`, used by the
    /// constraint validator to look up the type definition's `Generics` (which carry
    /// the constraint metadata). Closed concrete handles are canonicalised to their
    /// open generic identity, mirroring the canonicalisation in
    /// `instantiateGenericRuntimeTypeTarget`.
    let openGenericTypeInfoForValidation
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        let lookupFromIdentity (identity : ResolvedTypeIdentity) =
            match state.LoadedAssembly identity.Assembly with
            | None -> None
            | Some assembly -> Some assembly.TypeDefs.[identity.TypeDefinition.Get]

        match target with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery
                "RuntimeTypeHandle.openGenericTypeInfoForValidation"
                scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> lookupFromIdentity identity
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            match AllConcreteTypes.lookup handle state.ConcreteTypes with
            | None -> None
            | Some concreteType -> lookupFromIdentity concreteType.Identity
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _) ->
            // Structural targets carry no open-generic definition. Downstream
            // `instantiateGenericRuntimeTypeTarget` rejects them with `failwith`,
            // so we leave validation to that path.
            None
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // A generic parameter is not itself a generic type definition.
            // Downstream code rejects this with `failwith`; no constraint to check.
            None

    /// Resolves a single closed generic argument to its underlying nominal `TypeInfo`,
    /// or `None` if the argument is a structural shape (array, byref, pointer) that
    /// has no nominal definition.
    let nominalTypeInfoOfArgument
        (state : IlMachineState)
        (arg : ConcreteTypeHandle)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        match arg with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup arg state.ConcreteTypes with
            | None -> None
            | Some concreteType ->
                match state.LoadedAssembly concreteType.Assembly with
                | None -> None
                | Some assembly -> Some assembly.TypeDefs.[concreteType.Definition.Get]
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> None

    /// True iff `arg` resolves to a value type (i.e., a struct or enum).
    let argumentIsValueType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : ConcreteTypeHandle)
        : bool
        =
        match nominalTypeInfoOfArgument state arg with
        | None -> false // arrays / byref / pointer are not value types
        | Some typeInfo -> DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo

    /// True iff `arg` is the corelib's `System.Nullable\`1` definition. Roslyn emits
    /// the value-type constraint for `where T : struct` as the
    /// `NotNullableValueTypeConstraint` flag, which forbids `Nullable<T>` even though
    /// `Nullable<T>` is itself a value type.
    let argumentIsNullable
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : ConcreteTypeHandle)
        : bool
        =
        match nominalTypeInfoOfArgument state arg with
        | None -> false
        | Some typeInfo -> TypeInfo.NominallyEqual typeInfo baseClassTypes.Nullable

    /// True iff `arg` satisfies the `where T : new()` constraint:
    /// - value types implicitly satisfy it (every value type has a parameterless ctor);
    /// - reference types must be non-abstract, non-interface, and define a public
    ///   parameterless instance ctor;
    /// - structural shapes (array / byref / pointer) never satisfy it.
    let argumentSatisfiesNewConstraint
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : ConcreteTypeHandle)
        : bool
        =
        match nominalTypeInfoOfArgument state arg with
        | None -> false
        | Some typeInfo ->
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                true
            elif typeInfo.IsInterface then
                false
            elif typeInfo.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Abstract then
                false
            else
                typeInfo.Methods
                |> List.exists (fun m -> m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0 && m.IsPublic)

    /// True iff `arg` is a byref-like type (a C# `ref struct`), which may not be used as a generic
    /// argument unless the parameter carries `allows ref struct`. CoreCLR's `TypeHandle::IsByRefLike`
    /// (typehandle.cpp:1061) answers `false` for every TypeDesc, so a structural shape — for which
    /// `nominalTypeInfoOfArgument` returns `None` — is never byref-like whatever its element type is.
    let argumentIsByRefLike
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : ConcreteTypeHandle)
        : bool
        =
        match nominalTypeInfoOfArgument state arg with
        | None -> false
        | Some typeInfo -> DumpedAssembly.isByRefLike baseClassTypes state._LoadedAssemblies typeInfo

    /// A display name for a constraint type, for the diagnostic message only.
    let private constraintDisplayName (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        match nominalTypeInfoOfArgument state handle with
        | Some typeInfo -> $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"
        | None -> string<ConcreteTypeHandle> handle

    /// Validate the generic-parameter constraints declared by a *generic parameter list*, whoever
    /// owns it: a generic type's, via `validateConstraints` below, or a generic method's, via
    /// `RuntimeMethodHandle_GetStubIfNeededSlow`. Both owners reach the same CoreCLR check while
    /// binding (`TypeVarTypeDesc::SatisfiesConstraints`, typedesc.cpp:1491).
    ///
    /// Per parameter, in CoreCLR's order:
    /// 1. the flag-style special constraints (`NotNullableValueTypeConstraint` /
    ///    `ReferenceTypeConstraint` / `DefaultConstructorConstraint`, i.e. `where T : struct` /
    ///    `class` / `new()`);
    /// 2. the byref-like rejection: a `ref struct` argument is refused unless the parameter carries
    ///    `gpAllowByRefLike` (`allows ref struct`) — typedesc.cpp:1606;
    /// 3. the general "must be assignable to" constraints from the GenericParamConstraint table
    ///    (ECMA-335 §II.22.21), i.e. base-class and interface requirements.
    ///
    /// Each general constraint is concretized in the *caller's* substitution context —
    /// `declaringAssembly` / `typeGenerics` / `methodGenerics` — before the assignability check,
    /// exactly as CoreCLR loads it under `pTypeContextOfConstraintDeclarer` rather than deferring
    /// to `CanCastTo` on a typical instantiation. The comment at typedesc.cpp:1565-1580 gives the
    /// motivating example: verifying `S : A&lt;R&gt;` against `U : A&lt;T&gt;` requires substituting
    /// to `A&lt;int&gt;`, and the same is what makes `where T : IComparable&lt;T&gt;` satisfiable at
    /// all.
    ///
    /// CoreCLR additionally walks the *constraining chain* of the argument when the argument is
    /// itself a type variable (`GatherConstraintsRecursive`), because `class A&lt;S, T&gt; where S : T`
    /// may be instantiated as `A&lt;U, U&gt;`. That branch is unreachable here and cannot be
    /// silently mishandled: an argument is a `ConcreteTypeHandle`, which indexes `AllConcreteTypes`
    /// and is therefore closed by construction, so no open argument is representable to begin with.
    /// The static-virtual-method refinement at typedesc.cpp:1686 (an abstract argument against an
    /// interface constraint carrying unimplemented virtual statics) is likewise not implemented:
    /// it needs `ResolveVirtualStaticMethod`, which PawPrint does not have.
    ///
    /// Returns `Some message` describing the first violation, or `None` if every constraint is
    /// satisfied. `ownerDisplayName` appears only in that diagnostic; callers currently use the
    /// `Some`/`None` as a predicate and raise a message-less guest exception.
    ///
    /// That message is a PawPrint diagnostic, not the CLR's. It shares only the leading
    /// `GenericArguments[i]`: the CLR emits one shape for every kind of violation, naming the
    /// *argument* and then the *parameter* ("GenericArguments[0], 'System.Object', on 'C`1[T]'
    /// violates the constraint of type 'T'."), whereas this names which constraint was violated,
    /// which is far more useful when debugging the interpreter. It must therefore not be handed to
    /// a guest verbatim without first being rewritten into the CLR's shape.
    ///
    /// CoreCLR throws either `ArgumentException` or `VerificationException` depending on the
    /// call path; we always raise `ArgumentException`, matching the most commonly observed
    /// user-facing exception from `RuntimeType.MakeGenericType`. TODO: revisit if a different
    /// surface (e.g. a guest path that goes through verification rather than reflection) needs
    /// the other exception type.
    let validateConstraintsOn
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ownerDisplayName : string)
        (declaringAssembly : System.Reflection.AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (generics : GenericParamFromMetadata ImmutableArray)
        (genericArguments : ConcreteTypeHandle list)
        : IlMachineState * string option
        =
        if generics.Length <> List.length genericArguments then
            // Arity mismatch: defer to downstream to surface a more specific error. (`Seq.zip`
            // below would silently truncate without this guard.)
            state, None
        else

        let violation (param : GenericParameter) (constraintName : string) : string =
            $"GenericArguments[%i{param.SequenceNumber}], '%s{param.Name}', on '%s{ownerDisplayName}', violates the constraint of type '%s{constraintName}'."

        /// The flag-style constraints plus the byref-like rejection. None of these need to load
        /// anything, so they stay off the state-threading path.
        let specialViolationFor
            (param : GenericParameter)
            (paramMd : GenericParamMetadata)
            (arg : ConcreteTypeHandle)
            : string option
            =
            let isValue = argumentIsValueType baseClassTypes state arg

            let valueTypeViolation () =
                if paramMd.Constraint = Some GenericConstraint.NonNullableValue then
                    if not isValue || argumentIsNullable baseClassTypes state arg then
                        Some (violation param "System.ValueType")
                    else
                        None
                else
                    None

            let referenceTypeViolation () =
                if paramMd.Constraint = Some GenericConstraint.Reference && isValue then
                    Some (violation param "class")
                else
                    None

            let newConstraintViolation () =
                if
                    paramMd.RequiresParameterlessConstructor
                    && not (argumentSatisfiesNewConstraint baseClassTypes state arg)
                then
                    Some (violation param "new()")
                else
                    None

            let byRefLikeViolation () =
                if not paramMd.AllowsByRefLike && argumentIsByRefLike baseClassTypes state arg then
                    Some (violation param "allows ref struct")
                else
                    None

            valueTypeViolation ()
            |> Option.orElseWith referenceTypeViolation
            |> Option.orElseWith newConstraintViolation
            |> Option.orElseWith byRefLikeViolation

        /// The general "must be assignable to" constraints. Concretizing them can load assemblies
        /// and register types, hence the state threading.
        let generalViolationFor
            (state : IlMachineState)
            (param : GenericParameter)
            (paramMd : GenericParamMetadata)
            (arg : ConcreteTypeHandle)
            : IlMachineState * string option
            =
            ((state, None), paramMd.Constraints)
            ||> Seq.fold (fun (state, found) constraintTypeDefn ->
                match found with
                | Some _ -> state, found
                | None ->

                let state, constraintHandle =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        declaringAssembly
                        typeGenerics
                        methodGenerics
                        constraintTypeDefn

                // "System.Object constraint will be always satisfied" (typedesc.cpp:1637).
                // Untested: C# cannot spell `where T : object`, so no Roslyn-compiled corpus
                // reaches this branch. It keeps the verdict independent of whether the cast
                // relation grants object-assignability to every shape an argument can take.
                let isObjectConstraint =
                    match nominalTypeInfoOfArgument state constraintHandle with
                    | Some typeInfo -> TypeInfo.NominallyEqual typeInfo baseClassTypes.Object
                    | None -> false

                if isObjectConstraint then
                    state, None
                else

                let state, satisfied =
                    IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state arg constraintHandle

                if satisfied then
                    state, None
                else
                    state, Some (violation param (constraintDisplayName state constraintHandle))
            )

        ((state, None), Seq.zip generics genericArguments)
        ||> Seq.fold (fun (state, found) ((param, paramMd), arg) ->
            match found with
            | Some _ -> state, found
            | None ->

            match specialViolationFor param paramMd arg with
            | Some message -> state, Some message
            | None -> generalViolationFor state param paramMd arg
        )

    /// `validateConstraintsOn` for a generic *type*'s parameter list. The type's own generic
    /// arguments are the substitution context for its parameters' constraints.
    let validateConstraints
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (genericArguments : ConcreteTypeHandle list)
        : IlMachineState * string option
        =
        validateConstraintsOn
            loggerFactory
            baseClassTypes
            state
            $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"
            typeInfo.Assembly
            (ImmutableArray.CreateRange genericArguments)
            ImmutableArray.Empty
            typeInfo.Generics
            genericArguments

    let getOrAllocateRuntimeAssembly
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assemblyFullName = assemblyName.FullName

        match state.RuntimeAssemblyObjects.TryGetValue assemblyFullName with
        | true, cachedAddr -> cachedAddr, state
        | false, _ ->
            let state, runtimeAssemblyTypeInfo, runtimeAssemblyTypeHandle =
                concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System.Reflection" "RuntimeAssembly"

            let addr, state =
                allocateManagedObjectOfConcreteType
                    loggerFactory
                    baseClassTypes
                    state
                    runtimeAssemblyTypeInfo
                    runtimeAssemblyTypeHandle

            // Set the m_assembly field to a tagged native pointer so downstream native
            // calls can map back to the PawPrint DumpedAssembly.
            let assemblyField =
                FieldIdentity.requiredOwnInstanceField runtimeAssemblyTypeInfo "m_assembly"
                |> FieldIdentity.fieldId runtimeAssemblyTypeHandle

            let updatedObj =
                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetFieldById
                    assemblyField
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)))

            let state =
                { state with
                    ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                    RuntimeAssemblyObjects = state.RuntimeAssemblyObjects.Add (assemblyFullName, addr)
                }

            addr, state

    let getOrAllocateModuleRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assembly =
            state.LoadedAssembly assemblyName
            |> Option.defaultWith (fun () ->
                failwith
                    $"RuntimeTypeHandle.GetModule: assembly %s{assemblyName.FullName} for module type is not loaded"
            )

        let moduleTypeInfo =
            assembly.TypeDefs.Values
            |> Seq.tryFind (fun typeInfo -> typeInfo.Namespace = "" && typeInfo.Name = "<Module>")
            |> Option.defaultWith (fun () ->
                failwith $"RuntimeTypeHandle.GetModule: assembly %s{assemblyName.FullName} has no <Module> type"
            )

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies moduleTypeInfo

        let state, moduleTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                moduleTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (moduleTypeInfo.Identity, stk))

        IlMachineState.getOrAllocateType
            loggerFactory
            baseClassTypes
            (RuntimeTypeHandleTarget.Closed moduleTypeHandle)
            state

    let getOrAllocateRuntimeModule
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assemblyFullName = assemblyName.FullName

        match state.RuntimeModuleObjects.TryGetValue assemblyFullName with
        | true, cachedAddr -> cachedAddr, state
        | false, _ ->
            let runtimeAssemblyAddr, state =
                getOrAllocateRuntimeAssembly loggerFactory baseClassTypes assemblyName state

            let moduleRuntimeTypeAddr, state =
                getOrAllocateModuleRuntimeType loggerFactory baseClassTypes assemblyName state

            let state, runtimeModuleTypeInfo, runtimeModuleTypeHandle =
                concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System.Reflection" "RuntimeModule"

            let addr, state =
                allocateManagedObjectOfConcreteType
                    loggerFactory
                    baseClassTypes
                    state
                    runtimeModuleTypeInfo
                    runtimeModuleTypeHandle

            let updatedObj =
                let runtimeAssemblyField =
                    FieldIdentity.requiredOwnInstanceField runtimeModuleTypeInfo "m_runtimeAssembly"
                    |> FieldIdentity.fieldId runtimeModuleTypeHandle

                let runtimeTypeField =
                    FieldIdentity.requiredOwnInstanceField runtimeModuleTypeInfo "m_runtimeType"
                    |> FieldIdentity.fieldId runtimeModuleTypeHandle

                let pDataField =
                    FieldIdentity.requiredOwnInstanceField runtimeModuleTypeInfo "m_pData"
                    |> FieldIdentity.fieldId runtimeModuleTypeHandle

                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetFieldById
                    runtimeAssemblyField
                    (CliType.ObjectRef (Some runtimeAssemblyAddr))
                |> AllocatedNonArrayObject.SetFieldById
                    runtimeTypeField
                    (CliType.ObjectRef (Some moduleRuntimeTypeAddr))
                |> AllocatedNonArrayObject.SetFieldById
                    pDataField
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)))

            let state =
                { state with
                    ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                    RuntimeModuleObjects = state.RuntimeModuleObjects.Add (assemblyFullName, addr)
                }

            addr, state

    let formatNamespaceFlag : int32 = 0x00000001
    let formatFullInstFlag : int32 = 0x00000002
    let formatAssemblyFlag : int32 = 0x00000004
    let formatNoVersionFlag : int32 = 0x00000010

    let hasFormatFlag (flag : int32) (flags : int32) : bool = flags &&& flag <> 0

    let typeInfoDisplayName
        (includeNamespace : bool)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string
        =
        if includeNamespace then
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) typeInfo
        else
            typeInfo.Name

    let assemblyDisplayName (noVersion : bool) (assemblyName : System.Reflection.AssemblyName) : string =
        if noVersion then
            assemblyName.Name
        else
            assemblyName.FullName

    let runtimeTypeHandleName
        (operation : string)
        (state : IlMachineState)
        (flags : int32)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : string
        =
        let includeNamespace = hasFormatFlag formatNamespaceFlag flags
        let includeAssembly = hasFormatFlag formatAssemblyFlag flags
        let noVersion = hasFormatFlag formatNoVersionFlag flags

        let rec concreteTypeHandleName (typeHandle : ConcreteTypeHandle) : string =
            match typeHandle with
            | ConcreteTypeHandle.Byref inner -> $"%s{concreteTypeHandleName inner}&"
            | ConcreteTypeHandle.Pointer inner -> $"%s{concreteTypeHandleName inner}*"
            | ConcreteTypeHandle.FunctionPointer signature ->
                // CoreCLR's TypeString::AppendType for FnPtrType (vm/typestring.cpp ~791) only
                // emits the signature when FormatNamespace is set; otherwise it emits the empty
                // string. This matches user-visible reflection: typeof(delegate*<void>).Name is
                // "" (FormatBasic), .ToString() is "System.Void()" (FormatNamespace), and
                // .FullName is null (gated to null in the BCL before reaching ConstructName).
                if not includeNamespace then
                    ""
                else
                    let argStr =
                        signature.ParameterTypes |> Seq.map concreteTypeHandleName |> String.concat ", "

                    let retStr =
                        match signature.ReturnType with
                        // Void has no metadata-driven concrete handle to recurse through, so
                        // emit the qualified BCL name directly. CoreCLR recurses into the void
                        // type's metadata and gets the namespace via the same FormatNamespace path.
                        | MethodReturnType.Void -> "System.Void"
                        | MethodReturnType.Returns ret -> concreteTypeHandleName ret

                    $"%s{retStr}(%s{argStr})"
            | ConcreteTypeHandle.OneDimArrayZero inner -> $"%s{concreteTypeHandleName inner}[]"
            | ConcreteTypeHandle.Array (inner, rank) ->
                let dims = if rank <= 1 then "*" else System.String (',', rank - 1)
                $"%s{concreteTypeHandleName inner}[%s{dims}]"
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                let name = typeInfoDisplayName includeNamespace assembly typeInfo

                let name =
                    // CoreCLR's TypeString::AppendType (vm/typestring.cpp ~1170) appends the
                    // instantiation whenever FormatNamespace or FormatAssembly is set, regardless
                    // of FormatFullInst. FormatFullInst only changes how the instantiation
                    // arguments themselves are rendered (full namespace+assembly vs minimal).
                    let appendInstantiation =
                        (includeNamespace || includeAssembly) && not concreteType.Generics.IsEmpty

                    if appendInstantiation then
                        let args =
                            concreteType.Generics |> Seq.map concreteTypeHandleName |> String.concat ","

                        $"%s{name}[%s{args}]"
                    else
                        name

                if includeAssembly then
                    $"%s{name}, %s{assemblyDisplayName noVersion concreteType.Assembly}"
                else
                    name

        let rec targetName (target : RuntimeTypeHandleTarget) : string =
            match target with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.Closed typeHandle -> concreteTypeHandleName typeHandle
            | RuntimeTypeHandleTarget.OpenConstructed (definition, arguments) ->
                // CoreCLR's `TypeString::AppendType` emits the definition's name followed by the
                // instantiation in brackets, exactly as for a closed one; the arguments simply
                // happen to include type variables, which print as their bare parameter names.
                let assembly =
                    state.LoadedAssembly definition.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for open constructed type is not loaded: %s{definition.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[definition.TypeDefinition.Get]
                let name = typeInfoDisplayName includeNamespace assembly typeInfo

                let name =
                    // Same gate as the closed case just above: CoreCLR's
                    // `TypeString::AppendType` appends the instantiation only when
                    // FormatNamespace or FormatAssembly is set. `Type.Name` asks for neither, so
                    // it is "IComparable`1" rather than "IComparable`1[T]"; `ToString()` asks for
                    // FormatNamespace and so gets the brackets.
                    if includeNamespace || includeAssembly then
                        // Comma with no space, as the closed-handle path above does and as
                        // CoreCLR's `TypeString` emits: `IDictionary\`2[A,B]`.
                        let args = arguments |> List.map targetName |> String.concat ","
                        $"%s{name}[%s{args}]"
                    else
                        name

                if includeAssembly then
                    $"%s{name}, %s{assemblyDisplayName noVersion definition.Assembly}"
                else
                    name
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> nonConstructedName target

        and nonConstructedName (typeHandleTarget : RuntimeTypeHandleTarget) : string =
            match typeHandleTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.Closed typeHandle -> concreteTypeHandleName typeHandle
            | RuntimeTypeHandleTarget.OpenConstructed _ -> targetName typeHandleTarget
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                let assembly =
                    state.LoadedAssembly identity.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
                let name = typeInfoDisplayName includeNamespace assembly typeInfo

                let name =
                    // A generic definition renders its own formal parameters for `ToString()`
                    // (FormatNamespace alone): `typeof(List<>).ToString()` is
                    // "System.Collections.Generic.List`1[T]", while `.Name` is "List`1".
                    //
                    // But *not* for `FullName` or `AssemblyQualifiedName`, which CoreLib asks for
                    // with FormatFullInst set: those are "System.Collections.Generic.List`1",
                    // because a full name has to stay parseable and `List`1[T]` is not. This is
                    // the opposite of the closed case above, where FormatFullInst is precisely
                    // what makes the instantiation render in full.
                    //
                    // The formals also matter in argument position, since the
                    // typical-instantiation collapse puts definitions there —
                    // `IWrap<INested<T>>` renders its argument as "INested`1[T]".
                    let fullInst = hasFormatFlag formatFullInstFlag flags

                    if includeNamespace && not fullInst && not typeInfo.Generics.IsEmpty then
                        let formals =
                            typeInfo.Generics |> Seq.map (fun (p, _) -> p.Name) |> String.concat ","

                        $"%s{name}[%s{formals}]"
                    else
                        name

                if includeAssembly then
                    $"%s{name}, %s{assemblyDisplayName noVersion identity.Assembly}"
                else
                    name
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                // CoreCLR's TypeString::AppendType for a generic parameter emits only the
                // parameter name regardless of the FormatNamespace / FormatAssembly /
                // FormatGenericParameters bits: parameters have no namespace, no owning
                // assembly suffix, and no instantiation of their own.
                let assembly =
                    state.LoadedAssembly declaringType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for declaring type of generic parameter is not loaded: %s{declaringType.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

                if position < 0 || position >= typeInfo.Generics.Length then
                    failwith
                        $"%s{operation}: generic parameter position %d{position} is out of range for %O{declaringType.TypeDefinition.Get} (declares %d{typeInfo.Generics.Length} parameters)"

                let parameter, _ = typeInfo.Generics.[position]
                parameter.Name
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                // Same as type-generic parameters: CoreCLR emits only the parameter name.
                let assembly =
                    state.LoadedAssembly declaringType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for declaring type of method generic parameter is not loaded: %s{declaringType.AssemblyFullName}"
                    )

                let methodInfo = assembly.Methods.[declaringMethod.Get]

                if position < 0 || position >= methodInfo.Generics.Length then
                    failwith
                        $"%s{operation}: method generic parameter position %d{position} is out of range for method %O{declaringMethod.Get} (declares %d{methodInfo.Generics.Length} method generics)"

                let parameter, _ = methodInfo.Generics.[position]
                parameter.Name

        targetName typeHandleTarget

    /// CoreCLR's `TypeHandle::GetName` (`vm/typehandle.cpp:659`) — a *different* renderer from
    /// `runtimeTypeHandleName` above, which models `TypeString::AppendType` (the reflection
    /// `ConstructName` path). Use this one only where the EE itself formats a diagnostic,
    /// notably `COMPlusThrowInvalidCastException`'s `IDS_EE_CANNOTCAST`.
    ///
    /// The two disagree on nested types, which is the whole reason this exists.
    /// `TypeHandle::GetName` delegates to `MethodTable::_GetFullyQualifiedNameForClass`
    /// (`vm/class.cpp:2270`), which reads the TypeDef row's *own* namespace and name and does
    /// not walk the nesting chain — a nested `Outer.A` renders as bare `A`, because a nested
    /// TypeDef row carries no namespace of its own. (Contrast the sibling
    /// `_GetFullyQualifiedNameForClassNestedAware`, which builds `Outer+A`; `GetName` does not
    /// call it.) `TypeInfo.fullName`, which `runtimeTypeHandleName` uses, is nesting-aware,
    /// so it would answer `Outer.A` here and diverge from the real runtime's exception text.
    ///
    /// The *generic arguments*, by contrast, are rendered by `TypeString::AppendInst` with its
    /// default `FormatNamespace` (`vm/typestring.h:169`), i.e. by the ordinary reflection
    /// renderer — so those do delegate to `runtimeTypeHandleName`.
    let rec typeHandleGetName
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : string
        =
        let getNameOfHandle (handle : ConcreteTypeHandle) : string =
            typeHandleGetName operation state (RuntimeTypeHandleTarget.Closed handle)

        match target with
        // `TypeDesc::GetName` -> `TypeDesc::ConstructName` (`vm/typedesc.cpp:190`).
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.GenericParameter (_, position) -> $"!%d{position}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (_, _, position) -> $"!!%d{position}"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            // The canonical MethodTable of an open definition has an instantiation of type
            // variables, so `GetName` would append something like `[!0]`. Nothing reachable
            // formats one today, and guessing the spelling would be a silent divergence.
            failwith
                $"TODO: %s{operation}: TypeHandle::GetName of the open generic definition %O{identity.TypeDefinition.Get} is not modelled; its canonical MethodTable's instantiation is the type's own generic parameters, whose rendering is unverified"
        | RuntimeTypeHandleTarget.OpenConstructed (definition, _) ->
            // Same problem as the open definition above, one step further along: the
            // instantiation contains type variables, which `AppendInst` renders through
            // `TypeDesc::ConstructName` as `!0` / `!!0` rather than by name. The exact
            // interleaving with the enclosing name is unverified, so refuse rather than guess.
            failwith
                $"TODO: %s{operation}: TypeHandle::GetName of an open constructed instantiation of %O{definition.TypeDefinition.Get} is not modelled; its instantiation contains type variables whose rendering in this position is unverified"
        | RuntimeTypeHandleTarget.Closed handle ->
            match handle with
            | ConcreteTypeHandle.Byref inner -> $"%s{getNameOfHandle inner}&"
            | ConcreteTypeHandle.Pointer inner -> $"%s{getNameOfHandle inner}*"
            // `ConstructName` emits this literal for ELEMENT_TYPE_FNPTR, with no signature.
            | ConcreteTypeHandle.FunctionPointer _ -> "FNPTR"
            // An array is a MethodTable, and `_GetFullyQualifiedNameForClass` routes arrays
            // back through `TypeDesc::ConstructName` with the element type and rank.
            | ConcreteTypeHandle.OneDimArrayZero inner -> $"%s{getNameOfHandle inner}[]"
            | ConcreteTypeHandle.Array (inner, rank) ->
                let dims = if rank = 1 then "*" else System.String (',', rank - 1)
                $"%s{getNameOfHandle inner}[%s{dims}]"
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup handle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

                // `ns::MakePath(szNamespace, szName)` from the TypeDef row. A nested row's
                // namespace is empty, which is what collapses `Outer.A` to `A`.
                let name =
                    if System.String.IsNullOrEmpty typeInfo.Namespace then
                        typeInfo.Name
                    else
                        $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"

                if concreteType.Generics.IsEmpty then
                    name
                else
                    let args =
                        concreteType.Generics
                        |> Seq.map (fun arg ->
                            runtimeTypeHandleName
                                operation
                                state
                                formatNamespaceFlag
                                (RuntimeTypeHandleTarget.Closed arg)
                        )
                        |> String.concat ","

                    $"%s{name}[%s{args}]"

    /// PawPrint's rendering of CoreCLR's `CopyRuntimeTypeHandles` (runtimehandles.cpp:561), the
    /// single helper behind every QCall that hands a type list back through an
    /// `ObjectHandleOnStack`: `RuntimeTypeHandle_GetInstantiation`,
    /// `RuntimeTypeHandle_GetConstraints`, and `RuntimeMethodHandle_GetMethodInstantiation`.
    ///
    /// `asRuntimeTypeArray` is the upstream `BinderClassID` choice: `CLASS__CLASS`
    /// (`RuntimeType[]`) when true, `CLASS__TYPE` (`Type[]`) when false. Every element is the same
    /// `RuntimeType` object either way; only the array's element type differs, and the BCL casts
    /// the result to whichever it asked for.
    ///
    /// When `targets` is empty, CoreCLR returns NULL rather than a zero-length array
    /// (runtimehandles.cpp:573), so we leave `destination` untouched: every managed caller
    /// initialises its local to `null` before the QCall, and either tolerates that null or
    /// launders it through `?? Type.EmptyTypes`.
    let copyRuntimeTypeHandles
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (asRuntimeTypeArray : bool)
        (destination : ManagedPointerSource)
        (targets : RuntimeTypeHandleTarget list)
        : IlMachineState
        =
        match targets with
        | [] -> state
        | _ ->

        let elementTypeName = if asRuntimeTypeArray then "RuntimeType" else "Type"

        let state, _, elementTypeHandle =
            concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System" elementTypeName

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero elementTypeHandle)
                (fun () -> CliType.ObjectRef None)
                (List.length targets)
                state

        let state =
            ((state, 0), targets)
            ||> List.fold (fun (state, index) target ->
                let runtimeTypeAddr, state =
                    IlMachineState.getOrAllocateType loggerFactory baseClassTypes target state

                let state =
                    IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some runtimeTypeAddr)) index state

                state, index + 1
            )
            |> fst

        IlMachineState.writeManagedByrefWithBase baseClassTypes state destination (CliType.ObjectRef (Some arrayAddr))

    /// The inverse of <see cref="copyRuntimeTypeHandles"/>: read a managed <c>RuntimeType[]</c>
    /// back *in* through an <c>ObjectHandleOnStack</c>, as CoreCLR does when a QCall receives an
    /// instantiation from managed code (e.g. `RuntimeMethodHandle_GetStubIfNeededSlow`,
    /// runtimehandles.cpp:1936-1953).
    ///
    /// Returns <c>None</c> for a null array reference, keeping that distinct from an
    /// allocated-but-empty array. CoreCLR collapses the two here -- it guards on
    /// <c>methodInstantiation.Get() != NULL</c> and an empty array yields <c>ntypars = 0</c> -- but
    /// the distinction is the caller's to discard, not this helper's to hide.
    ///
    /// Each element is a <c>RuntimeType</c>, so the result is a <c>RuntimeTypeHandleTarget</c> per
    /// element; narrowing to the closed types a particular caller can use is likewise the caller's
    /// job.
    let readRuntimeTypeHandleArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (argName : string)
        (state : IlMachineState)
        (source : ManagedPointerSource)
        : RuntimeTypeHandleTarget list option
        =
        match IlMachineState.readManagedByref baseClassTypes state source with
        | CliType.ObjectRef None -> None
        | CliType.ObjectRef (Some arrayAddr) ->
            let array =
                match ManagedHeap.tryGetArrayShape arrayAddr state.ManagedHeap with
                | Some array -> array
                | None ->
                    failwith
                        $"%s{operation}: %s{argName} points at %O{arrayAddr}, which is not an array on the managed heap"

            [
                for index in 0 .. array.Length - 1 do
                    match IlMachineState.getArrayValue arrayAddr index state with
                    | CliType.ObjectRef None ->
                        // CoreCLR throws ArgumentNullException("inst", "ArgumentNull_ArrayElement")
                        // (runtimehandles.cpp:1948-1949). Unreachable from the public API, whose
                        // managed wrappers null-check each element before building the array (e.g.
                        // RuntimeMethodInfo.MakeGenericMethod, RuntimeMethodInfo.CoreCLR.cs:420-421),
                        // so surface the condition rather than inventing a guest exception path.
                        failwith
                            $"TODO: %s{operation}: %s{argName}[%d{index}] is null; CoreCLR throws ArgumentNullException(\"inst\", \"ArgumentNull_ArrayElement\")"
                    | element ->
                        yield
                            NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                                operation
                                state
                                (EvalStackValue.ofCliType element)
            ]
            |> Some
        | other -> failwith $"%s{operation}: expected %s{argName} to hold an array reference, got %O{other}"

/// Why `RuntimeTypeHandle_GetActivationInfo` refused to describe a type. Each case names the
/// check in CoreCLR's `ValidateTypeAbleToBeInstantiated` (reflectioninvocation.cpp) that it
/// corresponds to, and carries the guest exception CoreCLR throws for it. Message text is not
/// carried: PawPrint's runtime-exception path constructs through a parameterless ctor, and
/// `RuntimeType.ActivatorCache` rewraps the message with its own text regardless, so the
/// guest-observable fact is the exception *type*.
[<RequireQualifiedAccess>]
type ActivationRejection =
    /// Arrays and TypeDescs (byref, pointer, function pointer). CoreCLR:
    /// `typeHandle.IsTypeDesc() || typeHandle.IsArray()` -> `MissingMethodException`.
    | UnsupportedShape of ConcreteTypeHandle
    /// `pMT->IsDelegate()` -> `ArgumentException`. True exactly of types whose *immediate*
    /// base is `System.MulticastDelegate`, so `System.Delegate` and `MulticastDelegate`
    /// itself fall through to the abstract check instead.
    | Delegate
    /// `pMT->HasComponentSize()` -> `MissingMethodException`. `System.String` is the only
    /// variable-length type that reaches here; arrays were already rejected above.
    | VariableLength
    /// `pMT->IsAbstract()` and `pMT->IsInterface()` -> `MissingMethodException`.
    | Interface
    /// `pMT->IsAbstract()` -> `MissingMethodException`.
    | AbstractClass
    /// A reference type with no parameterless instance constructor at all -> CoreCLR's
    /// `MissingMethodException(W("Arg_NoDefCTorWithoutTypeName"))`. Note this is *not* the
    /// non-public-ctor case: that one is reported through `ctorIsPublic` and thrown by
    /// managed `CreateInstanceDefaultCtor`.
    | NoDefaultConstructor

/// What `RuntimeTypeHandle_GetActivationInfo` should hand back for a type.
[<RequireQualifiedAccess>]
type ActivationInfo =
    | Rejected of ActivationRejection
    /// `Nullable<T>`: CoreCLR writes a null allocator and null ctors, and `ActivatorCache`
    /// substitutes a stub that returns null. `Activator.CreateInstance(typeof(int?))` is null.
    | Nullable
    /// A value type with no explicit parameterless constructor. Allocation produces a boxed
    /// `default(T)` and no constructor call is needed, so CoreCLR reports the (absent) ctor as
    /// public.
    | ValueTypeWithoutConstructor of methodTable : ConcreteTypeHandle
    /// A type with a parameterless instance constructor. `isValueType` distinguishes the two
    /// entry points CoreCLR would hand back: a reference type needs only the `object`-receiver
    /// one, whereas a value type needs both a boxed and an unboxed entry point.
    | WithConstructor of
        methodTable : ConcreteTypeHandle *
        ctor : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
        isPublic : bool *
        isValueType : bool

[<RequireQualifiedAccess>]
module ActivationInfo =
    /// Reproduce CoreCLR's `RuntimeTypeHandle_GetActivationInfo` classification
    /// (reflectioninvocation.cpp), including the order of `ValidateTypeAbleToBeInstantiated`'s
    /// checks — the order matters where a type trips more than one, because CoreCLR throws
    /// different exception types for different checks.
    ///
    /// Two of `ValidateTypeAbleToBeInstantiated`'s checks are unreachable here, because the only
    /// managed caller (`RuntimeType.ActivatorCache`) runs `RuntimeType.CreateInstanceCheckThis`
    /// first and that throws for them already: `void` (`NotSupportedException`) and open
    /// generics / generic variables (`ArgumentException`). Both fail loudly here rather than
    /// silently falling through. A third check, for generics instantiated over `__Canon`, has no
    /// analogue at all: PawPrint has no shared generic instantiations.
    ///
    /// `ArgIterator` is *not* one of `ValidateTypeAbleToBeInstantiated`'s checks — it is rejected
    /// only by managed `CreateInstanceCheckThis`, which PawPrint interprets rather than
    /// reimplements, so there is nothing to reproduce here. (It is also byref-like, so even a
    /// hypothetical bypass would end at `CreateInstanceDefaultCtor`'s `IsByRefLike` guard rather
    /// than in a boxed `ArgIterator`.)
    ///
    /// Note that byref-like types are *permitted* here: CoreCLR passes `allowByRefLike: true`,
    /// and the `NotSupportedException` for a ref struct is thrown later, by managed
    /// `CreateInstanceDefaultCtor`, after it has consulted `CtorIsPublic`.
    let classify
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (target : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : IlMachineState * ActivationInfo
        =
        let handle =
            match target with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.Closed handle -> handle
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                failwith
                    $"%s{operation}: reached for %O{target}, which contains generic variables; RuntimeType.CreateInstanceCheckThis should have thrown ArgumentException (Acc_CreateGenericEx) before the QCall"

        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            state, ActivationInfo.Rejected (ActivationRejection.UnsupportedShape handle)
        | ConcreteTypeHandle.Concrete _ ->

        let ct, typeInfo =
            AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes handle
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: ConcreteTypeHandle %O{handle} not found in AllConcreteTypes"
            )

        if TypeInfo.NominallyEqual typeInfo baseClassTypes.Void then
            failwith
                $"%s{operation}: reached for System.Void; RuntimeType.CreateInstanceCheckThis should have thrown NotSupportedException (Acc_CreateVoid) before the QCall"

        // `pMT->IsDelegate()`, before the abstract check, so a type that is both reports
        // CoreCLR's ArgumentException rather than MissingMethodException. CoreCLR sets the flag
        // exactly when the immediate parent is MulticastDelegate.
        let state, directBase =
            IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state handle

        let isDelegate =
            match directBase with
            | None -> false
            | Some baseHandle ->
                match AllConcreteTypes.lookup baseHandle state.ConcreteTypes with
                | None -> false
                | Some baseCt -> baseCt.Identity = baseClassTypes.MulticastDelegateType.Identity

        if isDelegate then
            state, ActivationInfo.Rejected ActivationRejection.Delegate
        // `pMT->HasComponentSize()`: variable-length instances. Arrays were rejected above, so
        // String is the only remaining one.
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.String then
            state, ActivationInfo.Rejected ActivationRejection.VariableLength
        elif typeInfo.TypeAttributes.HasFlag TypeAttributes.Abstract then
            if typeInfo.TypeAttributes.HasFlag TypeAttributes.Interface then
                state, ActivationInfo.Rejected ActivationRejection.Interface
            else
                state, ActivationInfo.Rejected ActivationRejection.AbstractClass
        else

        match InternalTypeKind.kind baseClassTypes ct with
        | InternalTypeKind.Nullable -> state, ActivationInfo.Nullable
        | InternalTypeKind.Ordinary
        | InternalTypeKind.NativeInt
        | InternalTypeKind.NativeUInt ->

        let isValueType =
            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo

        // Visibility-blind, matching CoreCLR's `HasDefaultConstructor` / `GetDefaultConstructor`:
        // a private parameterless ctor *is* found, and its publicness is reported separately so
        // that managed `CreateInstanceDefaultCtor` can throw for `publicOnly`.
        let ctor =
            typeInfo.Methods
            |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && MethodInfo.arity m = 0)

        match ctor with
        | Some ctor -> state, ActivationInfo.WithConstructor (handle, ctor, ctor.IsPublic, isValueType)
        | None ->
            if isValueType then
                state, ActivationInfo.ValueTypeWithoutConstructor handle
            else
                state, ActivationInfo.Rejected ActivationRejection.NoDefaultConstructor

/// Why `ReflectionInvocation_GetBoxInfo` refused to describe a type, and which guest exception
/// CoreCLR throws for it. As with `ActivationRejection`, only the exception *type* is carried:
/// PawPrint's runtime-exception path constructs through a parameterless ctor.
///
/// These are the only two of `ValidateTypeAbleToBeInstantiated`'s checks a guest can reach through
/// this QCall; see `BoxInfo.classify` for why the rest cannot.
[<RequireQualifiedAccess>]
type BoxRejection =
    /// `typeHandle.GetSignatureCorElementType() == ELEMENT_TYPE_VOID` ->
    /// `ArgumentException`. Reachable: `System.Void` is a value type with a MethodTable, so
    /// `RuntimeHelpers.Box(ref b, typeof(void).TypeHandle)` passes every check `BoxCache`'s
    /// constructor makes and lands here.
    | Void
    /// `pMT->IsByRefLike()` under `allowRefLike: false` -> `NotSupportedException`. Reachable
    /// the same way, via any `ref struct`'s type handle.
    | ByRefLike

/// What `ReflectionInvocation_GetBoxInfo` should hand back for a type: enough for
/// `RuntimeType.BoxCache.Box` to allocate a box by `calli` and copy the payload into it.
type BoxDescription =
    {
        /// CoreCLR's `pvAllocatorFirstArg`: the MethodTable to allocate. For a `Nullable<T>` this
        /// is *`T`'s*, not the nullable's, because a boxed `Nullable<T>` is a boxed `T`.
        MethodTable : ConcreteTypeHandle
        /// CoreCLR's `pValueOffset`: `Nullable::GetValueAddrOffset` for a `Nullable<T>`, else 0.
        /// The guest adds this to the source byref before copying, and separately uses
        /// `!= 0` as its "is this a nullable" test.
        ValueOffset : int32
        /// CoreCLR's `pValueSize`: `GetNumInstanceFieldBytes` of `MethodTable` — i.e. of the
        /// substituted `T` for a nullable, not of the `Nullable<T>`.
        ValueSize : uint32
    }

[<RequireQualifiedAccess>]
type BoxInfo =
    | Rejected of BoxRejection
    | Describes of BoxDescription

[<RequireQualifiedAccess>]
module BoxInfo =
    /// Reproduce CoreCLR's `ReflectionInvocation_GetBoxInfo` (reflectioninvocation.cpp:1909),
    /// including its `ValidateTypeAbleToBeInstantiated(type, allowRefLike: false,
    /// fGetUninitializedObject: true)` prologue.
    ///
    /// Note that the exception kinds differ from `ActivationInfo.classify`'s even where the check
    /// is the same one: `fGetUninitializedObject` is `true` here and `false` there, which swaps
    /// `ArgumentException` for `MissingMethodException` on the shape checks and
    /// `MemberAccessException` for `MissingMethodException` on the abstract ones. Do not read
    /// across from the sibling classifier without re-reading the C++.
    ///
    /// Most of that prologue is unreachable, because the sole managed caller —
    /// `RuntimeType.BoxCache`'s constructor (RuntimeType.BoxCache.cs:42-55) — filters ahead of it:
    /// it throws `ArgumentException` itself for `IsTypeDesc` and for `ContainsGenericVariables`,
    /// and it calls this QCall *only* when `_pMT->IsValueType`. That leaves `void` and byref-like
    /// as the two live rejections, and rules out the array, delegate, variable-length (String),
    /// abstract and interface checks — none of which can hold of a value type emitted by any real
    /// compiler. Rather than write five dead predicates, this asserts the caller's own
    /// `IsValueType` gate, which is exactly what CoreCLR does immediately after the validation
    /// (`_ASSERTE(pMT->IsValueType() || ...)`, reflectioninvocation.cpp:1936). A sixth check,
    /// `IsSharedByGenericInstantiations`, has no analogue at all: PawPrint has no `__Canon`.
    ///
    /// Beware the shape of the sibling `RuntimeTypeHandle_GetActivationInfo` handler, which writes
    /// *all-null* pointers for `Nullable<T>` because `Activator.CreateInstance(typeof(int?))` is
    /// null. That is the opposite of what is wanted here: this QCall must describe a real
    /// allocator, offset and size for the underlying `T`.
    let classify
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (target : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : IlMachineState * BoxInfo
        =
        let handle =
            match target with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeRuntimeTypeHelpers.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.Closed handle -> handle
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                failwith
                    $"%s{operation}: reached for %O{target}, which contains generic variables; RuntimeType.BoxCache's constructor should have thrown ArgumentException (Arg_TypeNotSupported) before the QCall"

        match handle with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // CoreCLR's `typeHandle.IsTypeDesc()`, which `BoxCache`'s constructor already
            // rejected with ArgumentException.
            failwith
                $"%s{operation}: reached for the TypeDesc %O{handle}; RuntimeType.BoxCache's constructor should have thrown ArgumentException (Arg_TypeNotSupported) before the QCall"
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // CoreCLR's `typeHandle.IsArray()`. An array is not a value type, so `BoxCache`'s
            // `_pMT->IsValueType` gate is what excludes it.
            failwith
                $"%s{operation}: reached for the array type %O{handle}, which is not a value type; RuntimeType.BoxCache calls this QCall only when MethodTable::IsValueType"
        | ConcreteTypeHandle.Concrete _ ->

        let ct, typeInfo =
            AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes handle
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: ConcreteTypeHandle %O{handle} not found in AllConcreteTypes"
            )

        // The void check comes first in `ValidateTypeAbleToBeInstantiated`, and must stay ahead of
        // the value-type assertion below: `System.Void` *is* a value type, so the assertion would
        // wave it through.
        if TypeInfo.NominallyEqual typeInfo baseClassTypes.Void then
            state, BoxInfo.Rejected BoxRejection.Void
        elif not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo) then
            failwith
                $"%s{operation}: reached for the reference type %s{typeInfo.Namespace}.%s{typeInfo.Name}; RuntimeType.BoxCache calls this QCall only when MethodTable::IsValueType"
        elif DumpedAssembly.isByRefLike baseClassTypes state._LoadedAssemblies typeInfo then
            state, BoxInfo.Rejected BoxRejection.ByRefLike
        else

        // The `Nullable<T>` substitution: CoreCLR replaces the MethodTable with `T`'s and reports
        // where `T` starts inside the nullable, so the allocator produces a boxed `T` and the copy
        // skips `hasValue`.
        let state, methodTable, valueOffset =
            match InternalTypeKind.kind baseClassTypes ct with
            | InternalTypeKind.Ordinary
            | InternalTypeKind.NativeInt
            | InternalTypeKind.NativeUInt -> state, handle, 0
            | InternalTypeKind.Nullable ->

            let underlying =
                if ct.Generics.IsEmpty then
                    failwith
                        $"%s{operation}: System.Nullable`1 instantiation %O{handle} unexpectedly has no generic arguments"
                else
                    ct.Generics.[0]

            // The offset comes from PawPrint's own layout of this `Nullable<T>` rather than from
            // CoreCLR's, because it is PawPrint's byref model that has to resolve the
            // `Unsafe.Add(ref source, _nullableValueOffset)` the guest goes on to perform. A
            // constant taken from CoreCLR would be right only where the two layouts agree.
            let valueField = IlMachineState.requiredOwnInstanceFieldId state handle "value"

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

            let offset, _size = CliType.getFieldLayoutById valueField zero

            if offset <= 0 then
                // `hasValue` occupies offset 0 in CoreCLR and in every layout PawPrint computes
                // (it is a `bool`, so auto-layout places it in a primitive bucket ahead of the
                // `value` field's value-class bucket). This matters beyond tidiness: managed
                // `Box` uses `_nullableValueOffset != 0` as its *only* test for whether the
                // source is a nullable at all (RuntimeType.BoxCache.cs:79), so a zero offset
                // would silently make it copy a `Nullable<T>` as though it were a `T`.
                failwith
                    $"%s{operation}: laid out System.Nullable`1's 'value' field at offset %d{offset} in %O{handle}; CoreCLR places it after 'hasValue', and managed BoxCache.Box reads a zero offset as 'not a nullable'"

            state, underlying, offset

        let valueSize, state =
            MethodTableProjection.numInstanceFieldBytes baseClassTypes state methodTable

        state,
        BoxInfo.Describes
            {
                MethodTable = methodTable
                ValueOffset = valueOffset
                ValueSize = valueSize
            }
