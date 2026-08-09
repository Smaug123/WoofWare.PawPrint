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
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
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
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> typeDefinitionToken identity.TypeDefinition.Get
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

    /// Counts the instance virtual methods declared on this type that introduce a new vtable slot.
    /// Methods marked `Virtual` without `NewSlot` reuse a parent slot (override) and do not contribute
    /// here; static virtual methods (default interface methods) live outside the instance vtable.
    let numVirtualsOwn (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : int =
        typeInfo.Methods
        |> List.filter (fun method -> not method.IsStatic && method.IsVirtual && method.IsNewSlot)
        |> List.length

    /// Walks the type's inheritance chain (from the given handle up to the root, typically
    /// System.Object) summing the new instance vtable slots introduced at each level. The result
    /// is the size of the instance vtable for the type, matching CoreCLR's
    /// `MethodTable::GetNumVirtuals()`.
    let rec numVirtualsOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * int
        =
        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // Byrefs, pointers, and function pointers are TypeDescs in CoreCLR with no
            // MethodTable, so GetNumVirtuals returns 0 for them.
            state, 0
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Synthesised array MethodTables inherit their virtual slots from System.Array
            // (and through it, System.Object); the structural array handle itself adds none.
            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

            match baseHandle with
            | None -> state, 0
            | Some bh -> numVirtualsOfClosed loggerFactory baseClassTypes state bh
        | ConcreteTypeHandle.Concrete _ ->
            let _, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith
                        $"RuntimeTypeHandle.GetNumVirtuals: concrete type handle was not registered: %O{concreteType}"
                )

            let ownCount = numVirtualsOwn typeInfo

            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

            match baseHandle with
            | None -> state, ownCount
            | Some bh ->
                let state, baseCount = numVirtualsOfClosed loggerFactory baseClassTypes state bh
                state, ownCount + baseCount

    let numVirtuals
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : IlMachineState * int
        =
        match typeHandleTarget with
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
        | RuntimeTypeHandleTarget.Closed handle -> numVirtualsOfClosed loggerFactory baseClassTypes state handle

    /// Resolve the closed declaring type's `(ConcreteType, Methods)` pair. Returns `None` for
    /// handles whose CoreCLR equivalent has no MethodTable and therefore introduces no methods
    /// (byref/pointer/function-pointer TypeDescs); callers should emit the null sentinel so the
    /// managed `IntroducedMethodEnumerator` terminates immediately. Fails for synthesised array
    /// handles, because PawPrint does not yet model the array intrinsic methods and silent
    /// under-reporting would hide that gap.
    let introducedMethodsOfClosed
        (operation : string)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> *
          MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list) option
        =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            let concreteType, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state handle
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                )

            Some (concreteType, typeInfo.Methods)
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // CoreCLR's IntroducedMethodIterator runs on a MethodTable; byrefs/pointers/function-
            // pointers are TypeDescs with no MethodTable, so GetFirstIntroducedMethod returns null
            // and the managed enumerator terminates without iterating.
            None
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Synthesised array MethodTables have a small fixed set of introduced methods (Get/Set/
            // Address/the parameterless ctor). PawPrint does not yet model these; no test exercises
            // this path, so fail loudly to flag the gap rather than silently reporting zero.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{handle}; need to surface the array's intrinsic Get/Set/Address methods"

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
            // below would silently truncate, so this guard is load-bearing rather than defensive.)
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
                // Deliberately untested: C# cannot spell `where T : object`, so no Roslyn-compiled
                // corpus reaches this branch, and hand-written IL would be its own fixture. It is
                // here for parity, and because it keeps the verdict independent of whether the cast
                // relation grants object-assignability to every shape an argument can take. Today
                // removing it would change no answer: every argument that reaches this point walks
                // its base chain to Object anyway.
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

        match typeHandleTarget with
        | RuntimeTypeHandleTarget.Closed typeHandle -> concreteTypeHandleName typeHandle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            let name = typeInfoDisplayName includeNamespace assembly typeInfo

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
                match state.ManagedHeap.Arrays.TryGetValue arrayAddr with
                | true, array -> array
                | false, _ ->
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
    /// checks — the order is load-bearing where a type trips more than one, and CoreCLR throws
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

        let ct =
            AllConcreteTypes.lookup handle state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: ConcreteTypeHandle %O{handle} not found in AllConcreteTypes"
            )

        let typeInfo =
            state._LoadedAssemblies
                .ByDefinitionName(ct.Identity.AssemblyFullName)
                .TypeDefs.[ct.Identity.TypeDefinition.Get]

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
