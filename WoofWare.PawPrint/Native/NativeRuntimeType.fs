namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeRuntimeType =
    let private primitiveCorElementType (primitive : PrimitiveType) : int32 =
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
        | PrimitiveType.String -> 0x12
        | PrimitiveType.TypedReference -> 0x16
        | PrimitiveType.IntPtr -> 0x18
        | PrimitiveType.UIntPtr -> 0x19
        | PrimitiveType.Object -> 0x12

    let private nativeIntSize : int =
        CliType.sizeOf (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

    let private int32AtPointer (operation : string) (state : IlMachineState) (ptr : ManagedPointerSource) : int =
        match IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.Int32 0)) with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwith $"%s{operation}: expected Int32 at pointer, got %O{other}"

    let private writeInt32AtPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : int)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 value))

    let private nativeIntElementPointer
        (operation : string)
        (buffer : ManagedPointerSource)
        (index : int)
        : ManagedPointerSource
        =
        match buffer with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex), []) ->
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex + index), [])
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset), []) ->
            ManagedPointerSource.Byref (
                ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset + (index * nativeIntSize)),
                []
            )
        // The 1-arg overload of CreateInstanceForAnotherGenericParameter takes the
        // address of a single IntPtr local (`&typeHandle`), so element 0 *is* the
        // buffer itself. We cannot stride past it without escaping the local.
        | ManagedPointerSource.Byref (ByrefRoot.LocalVariable _, []) when index = 0 -> buffer
        | ManagedPointerSource.Byref (ByrefRoot.Argument _, []) when index = 0 -> buffer
        // Buffers are currently reached through GetFields' stackalloc/array path,
        // or through a single-IntPtr local taken by `&` for the 1-arg overload of
        // CreateInstanceForAnotherGenericParameter. Other shapes should fail with
        // their structure intact.
        | _ -> failwith $"%s{operation}: unsupported IntPtr result buffer pointer shape %O{buffer}"

    let private writeFieldHandleElement
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        (value : int64)
        : IlMachineState
        =
        let ptr = nativeIntElementPointer operation buffer index

        IlMachineState.writeManagedByrefWithBase
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr value)))

    let private runtimeFieldInfoStubAddress
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

    let private nominalCorElementType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<_, _>)
        : int32
        =
        if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
            0x11
        else
            0x12

    let private corElementType
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
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith $"TODO: %s{operation} for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
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

    let private enumUnderlyingPrimitive (operation : string) (typeInfo : TypeInfo<_, TypeDefn>) : PrimitiveType option =
        let instanceFields =
            typeInfo.Fields |> List.filter (fun field -> not field.IsStatic)

        match instanceFields with
        | [ field ] when field.Name = "value__" ->
            match field.Signature with
            | TypeDefn.PrimitiveType primitive -> Some primitive
            | other -> failwith $"%s{operation}: enum value__ field had non-primitive signature %O{other}"
        | _ -> None

    let private primitiveMethodTableCorElementType
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

    let private requiredValueTypeMethod
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (name : string)
        (parameterCount : int)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        baseClassTypes.ValueType.Methods
        |> List.filter (fun methodInfo ->
            methodInfo.Name = name
            && methodInfo.Parameters.Length = parameterCount
            && not methodInfo.IsStatic
        )
        |> function
            | [ methodInfo ] -> methodInfo
            | [] -> failwith $"%s{operation}: could not find System.ValueType::%s{name}"
            | methods ->
                let signatures =
                    methods
                    |> List.map (fun methodInfo -> $"%s{methodInfo.Name}/%i{methodInfo.Parameters.Length}")
                    |> String.concat ", "

                failwith $"%s{operation}: ambiguous System.ValueType::%s{name} candidates: %s{signatures}"

    let private overridesValueTypeMethod
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

    let rec private fieldAllowsFastCompare
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

    and private canCompareBitsOrUseFastGetHashCodeImpl
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

    let private canCompareBitsOrUseFastGetHashCode
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

    let private mdTypeDefNil : int32 = 0x02000000

    let private typeDefinitionToken (handle : System.Reflection.Metadata.TypeDefinitionHandle) : int32 =
        let handle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit handle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

    let private typeDefinitionTokenOfRuntimeTypeHandleTarget
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

    let private containsGenericVariables
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : bool
        =
        MethodTableProjection.targetContainsGenericVariables operation state typeHandleTarget

    /// Counts the instance virtual methods declared on this type that introduce a new vtable slot.
    /// Methods marked `Virtual` without `NewSlot` reuse a parent slot (override) and do not contribute
    /// here; static virtual methods (default interface methods) live outside the instance vtable.
    let private numVirtualsOwn (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : int =
        typeInfo.Methods
        |> List.filter (fun method ->
            not method.IsStatic
            && method.MethodAttributes.HasFlag System.Reflection.MethodAttributes.Virtual
            && method.MethodAttributes.HasFlag System.Reflection.MethodAttributes.NewSlot
        )
        |> List.length

    /// Walks the type's inheritance chain (from the given handle up to the root, typically
    /// System.Object) summing the new instance vtable slots introduced at each level. The result
    /// is the size of the instance vtable for the type, matching CoreCLR's
    /// `MethodTable::GetNumVirtuals()`.
    let rec private numVirtualsOfClosed
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

    let private numVirtuals
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
    let private introducedMethodsOfClosed
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

    let private declaringTypeInfo
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

    let private getOrAllocateDeclaringRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ManagedHeapAddress option * IlMachineState
        =
        match declaringTypeInfo "RuntimeTypeHandle.GetDeclaringType" state typeInfo with
        | None -> None, state
        | Some declaringTypeInfo when declaringTypeInfo.Generics.IsEmpty ->
            let addr, state =
                getOrAllocateNonGenericRuntimeType loggerFactory baseClassTypes state declaringTypeInfo

            Some addr, state
        | Some declaringTypeInfo ->
            let addr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringTypeInfo.Identity)
                    state

            Some addr, state

    let private declaringRuntimeType
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

    let private baseRuntimeType
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
    let private elementRuntimeType
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

    let private requireEmptyInterfaceMap
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : IlMachineState
        =
        let rec walkClosedType
            (state : IlMachineState)
            (visited : Set<ConcreteTypeHandle>)
            (typeHandle : ConcreteTypeHandle)
            : IlMachineState
            =
            if visited.Contains typeHandle then
                state
            else
                let visited = visited.Add typeHandle

                match typeHandle with
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ ->
                    // CoreCLR treats these TypeDesc shapes as having no MethodTable interface map.
                    state
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    failwith
                        $"TODO: %s{operation} for array type %O{typeHandle}; arrays expose runtime-provided interfaces"
                | ConcreteTypeHandle.Concrete _ ->
                    let _, typeInfo =
                        IlMachineState.tryGetConcreteTypeInfo state typeHandle
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                        )

                    if not typeInfo.ImplementedInterfaces.IsEmpty then
                        failwith
                            $"TODO: %s{operation} for %s{typeInfo.Namespace}.%s{typeInfo.Name}; type metadata has %i{typeInfo.ImplementedInterfaces.Length} implemented interfaces"

                    let state, baseType =
                        IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state typeHandle

                    match baseType with
                    | None -> state
                    | Some baseType -> walkClosedType state visited baseType

        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            failwith $"TODO: %s{operation} for open generic type definition %O{identity}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith $"TODO: %s{operation} for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.Closed typeHandle -> walkClosedType state Set.empty typeHandle

    let private findCorelibType
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

    let private concretizeNonGenericCorelibType
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

    let private allocateEmptyTypeArray
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, _, typeHandle =
            concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System" "Type"

        IlMachineState.allocateArray
            (ConcreteTypeHandle.OneDimArrayZero typeHandle)
            (fun () -> CliType.ObjectRef None)
            0
            state

    let private allocateManagedObjectOfConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (typeHandle : ConcreteTypeHandle)
        : ManagedHeapAddress * IlMachineState
        =
        let state, allFields =
            IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state typeHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes typeHandle typeInfo.Layout allFields

        IlMachineState.allocateManagedObject typeHandle fields state

    /// Read one element of a `TypeHandle*` instantiation buffer and return the
    /// closed `ConcreteTypeHandle` it points to. Open generic type-parameter
    /// references aren't yet representable here and fail loudly.
    let private readTypeHandleInstantiationElement
        (operation : string)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (index : int)
        : ConcreteTypeHandle
        =
        let ptr = nativeIntElementPointer operation buffer index

        match IlMachineState.readManagedByref state ptr |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle))) ->
            handle
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity))) ->
            failwith $"TODO: %s{operation} with open generic type argument %O{identity}"
        | other -> failwith $"%s{operation}: expected TypeHandlePtr in instantiation buffer, got %O{other}"

    /// Instantiate `genericDefinition` with `genericArguments`, producing a fresh
    /// closed `ConcreteTypeHandle`. Mirrors CoreCLR's `Instantiate(...)` step:
    /// canonicalise to the open generic definition first, then re-instantiate.
    let private instantiateOpenGenericTypeDefinition
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
    let private instantiateGenericRuntimeTypeTarget
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
    /// the constraint metadata read by Stage B1). Closed concrete handles are
    /// canonicalised to their open generic identity, mirroring the canonicalisation
    /// in `instantiateGenericRuntimeTypeTarget`.
    let private openGenericTypeInfoForValidation
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
    let private nominalTypeInfoOfArgument
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
    let private argumentIsValueType
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
    let private argumentIsNullable
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : ConcreteTypeHandle)
        : bool
        =
        match nominalTypeInfoOfArgument state arg with
        | None -> false
        | Some typeInfo ->
            typeInfo.Namespace = "System"
            && typeInfo.Name = "Nullable`1"
            && typeInfo.Assembly.FullName = baseClassTypes.Corelib.Name.FullName

    /// True iff `arg` satisfies the `where T : new()` constraint:
    /// - value types implicitly satisfy it (every value type has a parameterless ctor);
    /// - reference types must be non-abstract, non-interface, and define a public
    ///   parameterless instance ctor;
    /// - structural shapes (array / byref / pointer) never satisfy it.
    let private argumentSatisfiesNewConstraint
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
                |> List.exists (fun m ->
                    m.Name = ".ctor"
                    && not m.IsStatic
                    && m.Parameters.IsEmpty
                    && (m.MethodAttributes &&& System.Reflection.MethodAttributes.MemberAccessMask) = System.Reflection.MethodAttributes.Public
                )

    /// Validate the special-constraint flags
    /// (`NotNullableValueTypeConstraint` / `ReferenceTypeConstraint` /
    /// `DefaultConstructorConstraint`) declared on `typeInfo.Generics` against the
    /// supplied closed `genericArguments`. Returns `Some message` describing the
    /// first violation (suitable for an `ArgumentException` message), or `None` if
    /// all flag-style constraints are satisfied.
    ///
    /// This does NOT validate base-type / interface (`Constraints`) requirements —
    /// those land in Stage B3.
    ///
    /// CoreCLR throws either `ArgumentException` or `VerificationException`
    /// depending on the call path; we always raise `ArgumentException`, matching
    /// the most commonly observed user-facing exception from
    /// `RuntimeType.MakeGenericType`. TODO: revisit if a different surface (e.g. a
    /// guest path that goes through verification rather than reflection) needs the
    /// other exception type.
    let private validateSpecialConstraints
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (genericArguments : ConcreteTypeHandle list)
        : string option
        =
        if typeInfo.Generics.Length <> List.length genericArguments then
            // Arity mismatch: defer to downstream to surface a more specific error.
            None
        else
            let violationFor (param : GenericParameter) (paramMd : GenericParamMetadata) (arg : ConcreteTypeHandle) =
                let isValue = argumentIsValueType baseClassTypes state arg

                let valueTypeViolation () =
                    if paramMd.Constraint = Some GenericConstraint.NonNullableValue then
                        if not isValue || argumentIsNullable baseClassTypes state arg then
                            Some
                                $"GenericArguments[%i{param.SequenceNumber}], '%s{param.Name}', on '%s{typeInfo.Namespace}.%s{typeInfo.Name}', violates the constraint of type 'System.ValueType'."
                        else
                            None
                    else
                        None

                let referenceTypeViolation () =
                    if paramMd.Constraint = Some GenericConstraint.Reference && isValue then
                        Some
                            $"GenericArguments[%i{param.SequenceNumber}], '%s{param.Name}', on '%s{typeInfo.Namespace}.%s{typeInfo.Name}', violates the constraint of type 'class'."
                    else
                        None

                let newConstraintViolation () =
                    if
                        paramMd.RequiresParameterlessConstructor
                        && not (argumentSatisfiesNewConstraint baseClassTypes state arg)
                    then
                        Some
                            $"GenericArguments[%i{param.SequenceNumber}], '%s{param.Name}', on '%s{typeInfo.Namespace}.%s{typeInfo.Name}', violates the constraint of type 'new()'."
                    else
                        None

                valueTypeViolation ()
                |> Option.orElseWith referenceTypeViolation
                |> Option.orElseWith newConstraintViolation

            Seq.zip typeInfo.Generics genericArguments
            |> Seq.tryPick (fun ((param, paramMd), arg) -> violationFor param paramMd arg)

    let private getOrAllocateRuntimeAssembly
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

    let private getOrAllocateModuleRuntimeType
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

    let private formatNamespaceFlag : int32 = 0x00000001
    let private formatFullInstFlag : int32 = 0x00000002
    let private formatAssemblyFlag : int32 = 0x00000004
    let private formatNoVersionFlag : int32 = 0x00000010

    let private hasFormatFlag (flag : int32) (flags : int32) : bool = flags &&& flag <> 0

    let private typeInfoDisplayName
        (includeNamespace : bool)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string
        =
        if includeNamespace then
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) typeInfo
        else
            typeInfo.Name

    let private assemblyDisplayName (noVersion : bool) (assemblyName : System.Reflection.AssemblyName) : string =
        if noVersion then
            assemblyName.Name
        else
            assemblyName.FullName

    let private runtimeTypeHandleName
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
        | "RuntimeTypeHandle_ConstructName",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "ConstructName",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "TypeNameFormatFlags", flagsGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "StringHandleOnStack",
                                              stringHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && flagsGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.ConstructName"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let flags =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 flags) -> flags
                | other -> failwith $"%s{operation}: expected TypeNameFormatFlags as Int32, got %O{other}"

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[2]

            let name = runtimeTypeHandleName operation state flags typeHandleTarget

            let nameAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes name state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some nameAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "MethodTable_CanCompareBitsOrUseFastGetHashCode",
          "System.Private.CoreLib",
          "System",
          "ValueType",
          _,
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
                                                               "MethodTable",
                                                               methodTableGenerics)) ],
          returnType when methodTableGenerics.IsEmpty ->
            let operation = "MethodTable_CanCompareBitsOrUseFastGetHashCode"

            match returnType with
            | MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean)
            | MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) -> ()
            | other -> failwith $"%s{operation}: unexpected QCall stub return type %O{other}"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let methodTableArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let state, canCompare =
                canCompareBitsOrUseFastGetHashCode ctx.LoggerFactory ctx.BaseClassTypes ctx.Thread methodTableFor state

            let state =
                let ret = if canCompare then 1 else 0

                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "RuntimeTypeHandle_Instantiate",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "Instantiate",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.Instantiate"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let instantiationPointer =
                NativeCall.managedPointerOfPointerArgument operation "pInst" instruction.Arguments.[1]

            let genericArgumentCount =
                NativeCall.int32Argument operation instruction.Arguments.[2]

            if genericArgumentCount < 0 then
                failwith $"%s{operation}: numGenericArgs must be non-negative, got %d{genericArgumentCount}"

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "type" instruction.Arguments.[3]

            let genericArguments =
                [
                    for index in 0 .. genericArgumentCount - 1 ->
                        readTypeHandleInstantiationElement operation state instantiationPointer index
                ]

            // Stage B2: validate the special-constraint flags
            // (NotNullableValueTypeConstraint / ReferenceTypeConstraint /
            // DefaultConstructorConstraint) before instantiating. Base-type and
            // interface (`Constraints` array) requirements are not yet validated;
            // those will land in Stage B3.
            let constraintViolation =
                openGenericTypeInfoForValidation state typeHandleTarget
                |> Option.bind (fun typeInfo ->
                    validateSpecialConstraints ctx.BaseClassTypes state typeInfo genericArguments
                )

            match constraintViolation with
            | Some _message ->
                // raiseRuntimeException pushes the ArgumentException ctor frame on top of
                // this native QCall frame and arms `dispatchAsExceptionOnReturn`, so when
                // the ctor finishes its `Ret` will dispatch.  From the QCall dispatch
                // loop's point of view we have set up a managed continuation, exactly the
                // shape described by `SuspendedForManagedCall`: the native frame must
                // stay on the stack while the ctor runs.  We override the
                // `WhatWeDid.Executed` that `raiseRuntimeException` returns (which is
                // the right answer for IL-handler callers, where the ctor frame becomes
                // the new active frame and no QCall return-frame logic runs over it).
                // Exception dispatch on the ctor's `Ret` will eventually unwind the
                // native QCall frame too, so we never re-enter this handler.
                let state, _ =
                    IlMachineStateExecution.raiseRuntimeException
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.BaseClassTypes.ArgumentException
                        ctx.Thread
                        state

                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForManagedCall) |> Some
            | None ->

            let instantiatedHandle, state =
                instantiateGenericRuntimeTypeTarget
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    operation
                    state
                    typeHandleTarget
                    genericArguments

            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    (RuntimeTypeHandleTarget.Closed instantiatedHandle)
                    state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retType
                    (CliType.ObjectRef (Some runtimeTypeAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "RuntimeTypeHandle_GetInstantiation",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetInstantiation",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics)
            _ ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetInstantiation"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let retTypes =
                NativeCall.objectHandleOnStackTarget operation state "retTypes" instruction.Arguments.[1]

            // Interop.BOOL is an int32-backed enum. TRUE selects RuntimeType[]; FALSE selects Type[].
            let asRuntimeTypeArray =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[2] with
                | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
                | other -> failwith $"%s{operation}: expected Interop.BOOL as Int32, got %O{other}"

            let genericArgumentTargets : ImmutableArray<RuntimeTypeHandleTarget> =
                match typeHandleTarget with
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Concrete _ ->
                        let concreteType =
                            AllConcreteTypes.lookup handle state.ConcreteTypes
                            |> Option.defaultWith (fun () ->
                                failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                            )

                        concreteType.Generics
                        |> Seq.map RuntimeTypeHandleTarget.Closed
                        |> ImmutableArray.CreateRange
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // Real .NET strips array/byref/pointer wrappers via GetRootElementType
                        // before reaching this QCall, but be defensive: these wrappers carry
                        // no generic instantiation of their own.
                        ImmutableArray.Empty
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    // Real .NET returns Type[] { typeof(T), ... } where each T is a generic
                    // type parameter. We surface each parameter as a RuntimeType backed by a
                    // GenericParameter target.
                    let assembly =
                        state.LoadedAssembly identity.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]

                    if typeInfo.Generics.IsEmpty then
                        failwith
                            $"%s{operation}: open generic type definition %O{identity} declares no generic parameters"

                    Seq.init
                        typeInfo.Generics.Length
                        (fun position -> RuntimeTypeHandleTarget.GenericParameter (identity, position))
                    |> ImmutableArray.CreateRange
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    // GetInstantiation on a generic parameter T returns Type.EmptyTypes in CoreCLR,
                    // because a parameter has no instantiation of its own.
                    ImmutableArray.Empty

            // Empty: leave the caller's local null. RuntimeType.GetGenericArguments handles
            // null via `?? EmptyTypes`, matching native CopyRuntimeTypeHandles for 0 args.
            if genericArgumentTargets.IsEmpty then
                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            else
                let elementTypeName = if asRuntimeTypeArray then "RuntimeType" else "Type"

                let state, _, elementTypeHandle =
                    concretizeNonGenericCorelibType ctx.LoggerFactory ctx.BaseClassTypes state "System" elementTypeName

                let arrayAddr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero elementTypeHandle)
                        (fun () -> CliType.ObjectRef None)
                        genericArgumentTargets.Length
                        state

                let state =
                    ((state, 0), genericArgumentTargets)
                    ||> Seq.fold (fun (state, index) target ->
                        let runtimeTypeAddr, state =
                            IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

                        let state =
                            IlMachineState.setArrayValue
                                arrayAddr
                                (CliType.ObjectRef (Some runtimeTypeAddr))
                                index
                                state

                        state, index + 1
                    )
                    |> fst

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        retTypes
                        (CliType.ObjectRef (Some arrayAddr))

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "RuntimeTypeHandle_GetConstraints",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetConstraints",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.GetConstraints"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let retTypes =
                NativeCall.objectHandleOnStackTarget operation state "retTypes" instruction.Arguments.[1]

            match typeHandleTarget with
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
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

                let _, metadata = typeInfo.Generics.[position]

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
                    | TypeDefn.Modified (original, modifier, _) ->
                        embedsTypeParameter original || embedsTypeParameter modifier
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

                // Closed (non-parameter) constraints are concretized against the declaring
                // assembly with no generic context: a constraint like `where T : List<int>`
                // resolves to the closed type. Constraints that reference another type-generic
                // parameter (e.g. `where T2 : T1`) are surfaced as parameter targets directly,
                // because concretizeType cannot bind a parameter back to a parameter target.
                let baseTargets, state =
                    ((List.empty, state), metadata.Constraints)
                    ||> Seq.fold (fun (acc, state) ty ->
                        match ty with
                        | TypeDefn.GenericTypeParameter idx ->
                            let target = RuntimeTypeHandleTarget.GenericParameter (declaringType, idx)
                            target :: acc, state
                        | TypeDefn.GenericMethodParameter idx ->
                            failwith
                                $"%s{operation}: type-generic parameter #%d{position} of %O{declaringType.TypeDefinition.Get} declares a method-generic parameter constraint !!%d{idx}; impossible without a method context"
                        | _ when embedsTypeParameter ty ->
                            failwith
                                $"TODO: %s{operation}: constraint %O{ty} on type-generic parameter #%d{position} of %O{declaringType.TypeDefinition.Get} embeds a generic-parameter reference; concretization needs to bind parameters to parameter targets"
                        | _ ->
                            let state, handle =
                                IlMachineState.concretizeType
                                    ctx.LoggerFactory
                                    ctx.BaseClassTypes
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
                let constraintTargets, state =
                    match metadata.Constraint with
                    | Some GenericConstraint.NonNullableValue ->
                        let state, _, valueTypeHandle =
                            concretizeNonGenericCorelibType
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                state
                                "System"
                                "ValueType"

                        let alreadyHasValueType =
                            baseTargets
                            |> List.exists (fun t ->
                                match t with
                                | RuntimeTypeHandleTarget.Closed h -> h = valueTypeHandle
                                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                                | RuntimeTypeHandleTarget.GenericParameter _
                                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> false
                            )

                        if alreadyHasValueType then
                            baseTargets, state
                        else
                            baseTargets @ [ RuntimeTypeHandleTarget.Closed valueTypeHandle ], state
                    | Some GenericConstraint.Reference
                    | None -> baseTargets, state

                if List.isEmpty constraintTargets then
                    // CopyRuntimeTypeHandles writes NULL when count = 0; the managed wrapper turns
                    // the resulting null into Type.EmptyTypes via `?? EmptyTypes`. Leave the
                    // caller's local null untouched.
                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
                else
                    // CopyRuntimeTypeHandles allocates Type[] (CLASS__TYPE) — not RuntimeType[].
                    let state, _, typeHandle =
                        concretizeNonGenericCorelibType ctx.LoggerFactory ctx.BaseClassTypes state "System" "Type"

                    let arrayAddr, state =
                        IlMachineState.allocateArray
                            (ConcreteTypeHandle.OneDimArrayZero typeHandle)
                            (fun () -> CliType.ObjectRef None)
                            (List.length constraintTargets)
                            state

                    let state =
                        ((state, 0), constraintTargets)
                        ||> List.fold (fun (state, index) target ->
                            let runtimeTypeAddr, state =
                                IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

                            let state =
                                IlMachineState.setArrayValue
                                    arrayAddr
                                    (CliType.ObjectRef (Some runtimeTypeAddr))
                                    index
                                    state

                            state, index + 1
                        )
                        |> fst

                    let state =
                        IlMachineState.writeManagedByrefWithBase
                            ctx.BaseClassTypes
                            state
                            retTypes
                            (CliType.ObjectRef (Some arrayAddr))

                    (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some

            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                // CoreCLR's QCall throws ArgumentException for non-generic-variable arguments,
                // but the only managed caller (RuntimeType.GetGenericParameterConstraints) gates
                // on IsGenericParameter, so we should never reach this branch in practice. Fail
                // loudly rather than silently writing Type.EmptyTypes, which would mask a bug.
                failwith $"%s{operation}: expected a generic-parameter type handle, got %O{typeHandleTarget}"
        | "RuntimeTypeHandle_CreateInstanceForAnotherGenericParameter",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "CreateInstanceForAnotherGenericParameter",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.CreateInstanceForAnotherGenericParameter"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let outHandle =
                NativeCall.objectHandleOnStackTarget operation state "instantiatedObject" instruction.Arguments.[3]

            // The handler runs in two phases connected by `WhatWeDid.SuspendedForManagedCall`:
            //   1. First entry — eval stack empty: instantiate, ensure cctor, allocate, push the
            //      allocated address as a re-entry marker beneath `this`, then push `this` and
            //      hand off to the default ctor via `callMethod`. We return SuspendedForManagedCall
            //      so the dispatch loop runs the ctor before re-entering us.
            //   2. Re-entry — eval stack holds the marker: pop it and write to OutHandle. Per
            //      CoreCLR's reflectioninvocation.cpp, OutHandle is set only after the ctor
            //      returns successfully; if the ctor throws, exception dispatch unwinds past us
            //      and the caller's pre-zeroed `instantiatedObject` local stays null.
            //
            // The cctor case is independent: ensureTypeInitialised may suspend with
            // SuspendedForClassInit on the first phase; the eval stack stays empty across that
            // suspension, so when we re-enter we re-run phase 1 and ensureTypeInitialised
            // returns Executed the second time.
            match instruction.EvaluationStack.Values with
            | [ marker ] ->
                let addr =
                    match marker with
                    | EvalStackValue.ObjectRef a -> a
                    | other ->
                        failwith
                            $"%s{operation}: expected re-entry marker (object ref to allocated instance) on eval stack, got %O{other}"

                let _, state = IlMachineState.popEvalStack ctx.Thread state

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        outHandle
                        (CliType.ObjectRef (Some addr))

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            | [] ->
                let typeHandleTarget =
                    NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                        operation
                        state
                        (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

                let pInstArray =
                    NativeCall.managedPointerOfPointerArgument operation "pTypeHandles" instruction.Arguments.[1]

                let cInstArray = NativeCall.int32Argument operation instruction.Arguments.[2]

                if cInstArray < 0 then
                    failwith $"%s{operation}: cTypeHandles must be non-negative, got %d{cInstArray}"

                let genericArguments =
                    [
                        for index in 0 .. cInstArray - 1 ->
                            readTypeHandleInstantiationElement operation state pInstArray index
                    ]

                let instantiatedHandle, state =
                    instantiateGenericRuntimeTypeTarget
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        operation
                        state
                        typeHandleTarget
                        genericArguments

                let state, typeInit =
                    IlMachineStateExecution.ensureTypeInitialised
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        ctx.Thread
                        instantiatedHandle
                        state

                match typeInit with
                | WhatWeDid.SuspendedForClassInit ->
                    ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit) |> Some
                | WhatWeDid.BlockedOnClassInit blockedBy ->
                    ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy) |> Some
                | WhatWeDid.ThrowingTypeInitializationException ->
                    ExecutionResult.Stepped (state, WhatWeDid.ThrowingTypeInitializationException)
                    |> Some
                | WhatWeDid.SuspendedForManagedCall ->
                    failwith "logic error: ensureTypeInitialised cannot suspend for an arbitrary managed call"
                | WhatWeDid.Executed ->

                let concreteType =
                    AllConcreteTypes.lookup instantiatedHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: instantiated handle was not registered: %O{instantiatedHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: assembly is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

                if DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies typeInfo then
                    // CoreCLR's QCall asserts !pVMT->IsByRefLike() and routes value types
                    // away from this path elsewhere; the only documented consumer
                    // (ArraySortHelper) instantiates reference types. If a value-type ever
                    // reaches us, calling the parameterless ctor with `this`-as-ObjectRef
                    // would silently boxsem the receiver, so reject it explicitly.
                    failwith $"TODO: %s{operation} for value type %s{typeInfo.Namespace}.%s{typeInfo.Name}"

                let objectAddr, state =
                    allocateManagedObjectOfConcreteType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        typeInfo
                        instantiatedHandle

                let ctor =
                    typeInfo.Methods
                    |> List.tryFind (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Parameters.IsEmpty)
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: no parameterless .ctor found on %s{typeInfo.Namespace}.%s{typeInfo.Name}"
                    )

                let state, concretizedCtor, _declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        concreteType.Generics
                        ctor
                        ImmutableArray.Empty
                        state

                // Push the allocated address as the re-entry marker. `callMethod` pops
                // only the ctor's `this` (which we push next), leaving the marker visible
                // to the re-entry branch above when the ctor returns.
                let state =
                    IlMachineState.pushToEvalStack (CliType.ObjectRef (Some objectAddr)) ctx.Thread state

                let state =
                    IlMachineState.pushToEvalStack (CliType.ObjectRef (Some objectAddr)) ctx.Thread state

                let threadState = state.ThreadState.[ctx.Thread]

                // wasConstructing = None: we're calling the ctor as a regular instance
                // method, not Newobj. We don't want returnStackFrame to push the
                // constructed value back — the marker is already there for us.
                // advanceProgramCounterOfCaller = false: the native frame has no IL.
                let state =
                    IlMachineStateExecution.callMethod
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        None
                        None
                        false
                        false
                        false
                        concretizedCtor.Generics
                        concretizedCtor
                        ctx.Thread
                        threadState
                        None
                        false
                        state

                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForManagedCall) |> Some
            | other ->
                failwith
                    $"%s{operation}: expected at most one re-entry marker on the eval stack, got %d{other.Length} value(s): %A{other}"
        | "ModuleHandle_ResolveType",
          "System.Private.CoreLib",
          "System",
          "ModuleHandle",
          "ResolveType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              qCallModuleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when qCallModuleGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "ModuleHandle.ResolveType"

            if instruction.Arguments.Length <> 7 then
                failwith $"%s{operation}: expected seven native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let typeToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            let typeInstArgsPtr =
                NativeCall.managedPointerOfPointerArgument operation "typeInstArgs" instruction.Arguments.[2]

            let typeInstCount = NativeCall.int32Argument operation instruction.Arguments.[3]

            if typeInstCount < 0 then
                failwith $"%s{operation}: typeInstCount must be non-negative, got %d{typeInstCount}"

            let methodInstArgsPtr =
                NativeCall.managedPointerOfPointerArgument operation "methodInstArgs" instruction.Arguments.[4]

            let methodInstCount = NativeCall.int32Argument operation instruction.Arguments.[5]

            if methodInstCount < 0 then
                failwith $"%s{operation}: methodInstCount must be non-negative, got %d{methodInstCount}"

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "type" instruction.Arguments.[6]

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: module's assembly %s{assemblyFullName} is not loaded"
                )

            // CoreCLR allows the caller to pass declaring-type / declaring-method generic
            // argument arrays as substitution context for tokens that reference generic
            // parameters (typically TypeSpecs); these arrays may also be supplied for tokens
            // that don't need them, in which case they are simply unused. Decode them up
            // front so we never reject a call whose token doesn't actually consume them.
            let typeInstantiation =
                ImmutableArray.CreateRange (
                    seq {
                        for index in 0 .. typeInstCount - 1 ->
                            readTypeHandleInstantiationElement operation state typeInstArgsPtr index
                    }
                )

            let methodInstantiation =
                ImmutableArray.CreateRange (
                    seq {
                        for index in 0 .. methodInstCount - 1 ->
                            readTypeHandleInstantiationElement operation state methodInstArgsPtr index
                    }
                )

            // The C# wrapper validates the token kind (TypeDef/TypeSpec/TypeRef, and not the
            // global TypeDef token) before reaching this QCall, so any other kind here is a
            // contract violation rather than user error.
            let state, target =
                match MetadataToken.ofInt typeToken with
                | MetadataToken.TypeDefinition h ->
                    let state, typeDefn =
                        IlMachineState.lookupTypeDefn ctx.BaseClassTypes state assembly h

                    IlMachineState.runtimeTypeHandleTargetForTypeToken
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        assembly
                        true
                        typeInstantiation
                        methodInstantiation
                        typeDefn
                        state
                | MetadataToken.TypeReference h ->
                    // Resolve the TypeRef itself with no caller-supplied generic context: the
                    // referenced type's own definition must not be substituted via the caller's
                    // type/method instantiation. Caller context is reserved for TypeSpec generic
                    // substitution, applied below by runtimeTypeHandleTargetForTypeToken.
                    let state, typeDefn, declaringAssembly =
                        IlMachineState.lookupTypeRef
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            state
                            assembly
                            ImmutableArray.Empty
                            h

                    IlMachineState.runtimeTypeHandleTargetForTypeToken
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        declaringAssembly
                        true
                        typeInstantiation
                        methodInstantiation
                        typeDefn
                        state
                | MetadataToken.TypeSpecification h ->
                    // Mirror executeLdtoken: feed the raw signature directly with
                    // allowOpenGenericDefinition=false. TypeSpecs already encode their
                    // structure, including any generic instantiations.
                    let typeDefn = assembly.TypeSpecs.[h].Signature

                    IlMachineState.runtimeTypeHandleTargetForTypeToken
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        assembly
                        false
                        typeInstantiation
                        methodInstantiation
                        typeDefn
                        state
                | other ->
                    failwith
                        $"%s{operation}: unexpected metadata token kind %O{other} from token 0x%08x{typeToken}; the managed wrapper should only forward TypeDef/TypeSpec/TypeRef"

            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retType
                    (CliType.ObjectRef (Some runtimeTypeAddr))

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
          "System.Runtime.CompilerServices",
          "MethodTable",
          "GetNumInstanceFieldBytes",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            let operation = "MethodTable.GetNumInstanceFieldBytes"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let methodTableArg, state = IlMachineState.popEvalStack ctx.Thread state
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let bytes, state =
                MethodTableProjection.numInstanceFieldBytes ctx.BaseClassTypes state methodTableFor

            let state =
                IlMachineState.pushToEvalStack (NativeCall.cliUInt32 bytes) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "MethodTable",
          "GetPrimitiveCorElementType",
          [],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "CorElementType",
                                                                      corElementTypeGenerics)) when
            corElementTypeGenerics.IsEmpty
            ->
            let operation = "MethodTable.GetPrimitiveCorElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let methodTableArg, state = IlMachineState.popEvalStack ctx.Thread state
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let elementType =
                primitiveMethodTableCorElementType operation ctx.BaseClassTypes state methodTableFor

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetFields",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetFields"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let resultBuffer =
                NativeCall.managedPointerOfPointerArgument operation "result buffer" instruction.Arguments.[1]

            let countPtr =
                NativeCall.managedPointerOfPointerArgument operation "count pointer" instruction.Arguments.[2]

            let capacity = int32AtPointer operation state countPtr

            let state, fieldHandleIds =
                match typeHandleTarget with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic type definition %O{identity}; expected behavior is to enumerate the canonical type's non-literal fields"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    // A generic parameter has no instance fields — its constraints can declare
                    // field-bearing types but the parameter itself is not one. Real CoreCLR
                    // returns an empty array for typeof(T).GetFields().
                    failwith
                        $"TODO: %s{operation} for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.Closed typeHandle ->
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

                        let assembly =
                            state.LoadedAssembly concreteType.Assembly
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                            )

                        let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

                        // Handles remain metadata identities here. Consumers that expose field
                        // signatures must substitute them against the closed concrete type.
                        let fields =
                            typeInfo.Fields
                            |> List.filter (fun field ->
                                not (field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal)
                            )

                        let instanceFields, staticFields =
                            fields |> List.partition (fun field -> not field.IsStatic)

                        let fields = instanceFields @ staticFields

                        ((state, []), fields)
                        ||> List.fold (fun (state, ids) field ->
                            let runtimeFieldHandle, state =
                                IlMachineState.getOrAllocateField
                                    ctx.LoggerFactory
                                    ctx.BaseClassTypes
                                    concreteType.Assembly
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

            let count = List.length fieldHandleIds

            let state =
                if count > capacity then
                    writeInt32AtPointer ctx.BaseClassTypes state countPtr count
                else
                    let state =
                        ((state, 0), fieldHandleIds)
                        ||> List.fold (fun (state, index) fieldHandleId ->
                            writeFieldHandleElement operation ctx.BaseClassTypes state resultBuffer index fieldHandleId,
                            index + 1
                        )
                        |> fst

                    writeInt32AtPointer ctx.BaseClassTypes state countPtr count

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool (count <= capacity)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetInterfaces",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteTypeHandle.OneDimArrayZero (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                                                          "System",
                                                                                                          "Type",
                                                                                                          returnTypeGenerics))) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetInterfaces"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let state =
                requireEmptyInterfaceMap ctx.LoggerFactory ctx.BaseClassTypes operation state typeHandleTarget

            let arrayAddr, state =
                allocateEmptyTypeArray ctx.LoggerFactory ctx.BaseClassTypes state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some arrayAddr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetCorElementType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "CorElementType",
                                                                      corElementTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && corElementTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetCorElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let elementType = corElementType operation ctx.BaseClassTypes state typeHandleTarget

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetToken",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetToken"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let token =
                typeDefinitionTokenOfRuntimeTypeHandleTarget operation state typeHandleTarget

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 token)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "IsGenericVariable",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.IsGenericVariable"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let isGenericVariable =
                match target with
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> true
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                | RuntimeTypeHandleTarget.Closed _ -> false

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool isGenericVariable) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetGenericVariableIndex",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            // CoreCLR's public RuntimeTypeHandle.GetGenericVariableIndex wrapper guards this
            // InternalCall with an IsGenericVariable check that throws InvalidOperationException
            // for non-parameter targets. Reaching here on a non-parameter target means the
            // wrapper's invariant was violated, so fail loudly.
            let operation = "RuntimeTypeHandle.GetGenericVariableIndex"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let index =
                match target with
                | RuntimeTypeHandleTarget.GenericParameter (_, position)
                | RuntimeTypeHandleTarget.MethodGenericParameter (_, _, position) -> position
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                | RuntimeTypeHandleTarget.Closed _ ->
                    failwith
                        $"%s{operation} called on non-parameter target %O{target}: managed wrapper should have rejected this"

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 index)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringMethod",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "IRuntimeMethodInfo",
                                                                      methodInfoGenerics)) when
            runtimeTypeGenerics.IsEmpty && methodInfoGenerics.IsEmpty
            ->
            // GetDeclaringMethod returns null for type-level generic parameters and
            // non-parameter targets, and the declaring IRuntimeMethodInfo for
            // method-level generic parameters.
            let operation = "RuntimeTypeHandle.GetDeclaringMethod"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            match target with
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.Closed _ ->
                // Type-level generic parameters and non-parameter targets return null.
                let state = NativeCall.pushObjectTarget None ctx.Thread state
                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; need to allocate/return IRuntimeMethodInfo"
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetDeclaringType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let declaringTypeAddr, state =
                declaringRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget declaringTypeAddr ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "ContainsGenericVariables",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.ContainsGenericVariables"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let result = containsGenericVariables operation state typeHandleTarget

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetBaseType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetBaseType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let baseTypeAddr, state =
                baseRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget baseTypeAddr ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetElementType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let elementTypeAddr, state =
                elementRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget elementTypeAddr ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetAssembly",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeAssembly",
                                                                      runtimeAssemblyGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeAssemblyGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetAssembly"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName =
                NativeCall.typeAssemblyName operation ctx.BaseClassTypes state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeAssembly ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetModule",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeModule",
                                                                      runtimeModuleGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetModule"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName =
                NativeCall.typeAssemblyName operation ctx.BaseClassTypes state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeModule ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "CanCastTo",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", sourceGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", targetGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            sourceGenerics.IsEmpty && targetGenerics.IsEmpty
            ->
            // RuntimeTypeHandle.CanCastTo is the InternalCall boundary that backs
            // RuntimeType.IsAssignableFrom (and therefore Type.IsAssignableTo) on .NET 9.
            // Delegate to the existing concrete-type cast oracle.
            let operation = "RuntimeTypeHandle.CanCastTo"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let sourceRef, state = IlMachineState.popEvalStack ctx.Thread state
            let state = IlMachineState.loadArgument ctx.Thread 1 state
            let targetRef, state = IlMachineState.popEvalStack ctx.Thread state

            let sourceTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state sourceRef

            let targetTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state targetRef

            let sourceHandle =
                match sourceTarget with
                | RuntimeTypeHandleTarget.Closed handle -> handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic source type definition %O{identity}; need to model variance/identity rules for unbound generics"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    failwith
                        $"TODO: %s{operation} for generic parameter source #%i{position} of %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: %s{operation} for method generic parameter source #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            let targetHandle =
                match targetTarget with
                | RuntimeTypeHandleTarget.Closed handle -> handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic target type definition %O{identity}; need to model variance/identity rules for unbound generics"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    failwith
                        $"TODO: %s{operation} for generic parameter target #%i{position} of %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: %s{operation} for method generic parameter target #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            // Reflection-only rule from CanCastToWorker(nullableCast: true): T is assignable
            // to Nullable<T> when queried via reflection, even though the runtime IL cast
            // disagrees. The asymmetric direction (Nullable<T> -> T) does not hold and is
            // left to the standard cast oracle.
            let nullableTargetMatchesSource =
                match targetHandle with
                | ConcreteTypeHandle.Concrete _ ->
                    match AllConcreteTypes.lookup targetHandle state.ConcreteTypes with
                    | Some targetCt when
                        targetCt.Namespace = "System"
                        && targetCt.Name = "Nullable`1"
                        && targetCt.Assembly.FullName = ctx.BaseClassTypes.Corelib.Name.FullName
                        && targetCt.Generics.Length = 1
                        ->
                        targetCt.Generics.[0] = sourceHandle
                    | _ -> false
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> false

            let state, isAssignable =
                if nullableTargetMatchesSource then
                    state, true
                else
                    IlMachineState.isConcreteTypeAssignableTo
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        sourceHandle
                        targetHandle

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool isAssignable) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetAttributes",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "TypeAttributes",
                                                                      typeAttributesGenerics)) when
            runtimeTypeGenerics.IsEmpty && typeAttributesGenerics.IsEmpty
            ->
            // RuntimeTypeHandle.GetAttributes is the InternalCall boundary backing
            // RuntimeType.GetAttributeFlagsImpl, which is what Type.Attributes calls.
            // CoreCLR's implementation (runtimehandles.cpp ::GetAttributes) returns
            // tdPublic (1) for any TypeDesc — generic variables, byrefs, pointers,
            // function pointers — and otherwise returns the MethodTable's TypeAttributes.
            // Arrays are not TypeDesc in CoreCLR; their synthesized MethodTable carries
            // Public | Sealed | Serializable.
            let operation = "RuntimeTypeHandle.GetAttributes"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let attributes : int32 =
                match target with
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> int System.Reflection.TypeAttributes.Public
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    let assembly =
                        state.LoadedAssembly identity.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
                    int typeInfo.TypeAttributes
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ -> int System.Reflection.TypeAttributes.Public
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // tdPublic | tdSealed | tdSerializable. The Serializable enum
                        // member is deprecated for new managed code, but the bit is the
                        // documented runtime convention for synthesized array MethodTables.
                        int (
                            System.Reflection.TypeAttributes.Public
                            ||| System.Reflection.TypeAttributes.Sealed
                        )
                        ||| 0x2000
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
                        int typeInfo.TypeAttributes

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 attributes)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetNumVirtuals",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            // RuntimeType.GetMethodCandidates allocates a `bool[numVirtuals]` overrides
            // map, so this number must be the size of the instance vtable for the type:
            // sum of (Virtual + NewSlot, instance) methods declared on the type and on
            // every ancestor up to System.Object. CoreCLR's runtimehandles.cpp returns
            // pMT->GetNumVirtuals() (or 0 when there is no MethodTable, e.g. byrefs and
            // pointers).
            let operation = "RuntimeTypeHandle.GetNumVirtuals"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let state, count =
                numVirtuals ctx.LoggerFactory ctx.BaseClassTypes operation state target

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 count)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetFirstIntroducedMethod",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeMethodHandleInternal",
                                                                      returnGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnGenerics.IsEmpty
            ->
            // First half of the IntroducedMethodEnumerator pair: returns the bare
            // RuntimeMethodHandleInternal pointing at the first method declared by `type`'s
            // MethodTable, or zero if there are none. The BCL pairs this with
            // GetNextIntroducedMethod to walk every introduced slot in metadata order
            // (RuntimeHandles.cs:347-390). Inherited methods are NOT surfaced; callers walk the
            // base-type chain themselves (see RuntimeType.GetMethodCandidates).
            let operation = "RuntimeTypeHandle.GetFirstIntroducedMethod"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let handle =
                match target with
                | RuntimeTypeHandleTarget.Closed handle -> handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic type definition %O{identity}; need to walk metadata-level methods on the open type"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    // CoreCLR's GetMethodCandidates strips generic variables via GetBaseType
                    // before iterating; reaching here means a managed-side invariant was violated.
                    failwith
                        $"%s{operation}: invoked on type-generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before iterating"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"%s{operation}: invoked on method-generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            let returnValue, state =
                match introducedMethodsOfClosed operation state handle with
                | None
                | Some (_, []) ->
                    let zero =
                        MethodHandleRegistry.zeroInternalHandle ctx.BaseClassTypes state.ConcreteTypes

                    zero, state
                | Some (declaringType, first :: _) ->
                    let value, reg =
                        MethodHandleRegistry.getOrAllocateInternalHandle
                            ctx.BaseClassTypes
                            state.ConcreteTypes
                            declaringType
                            first
                            state.MethodHandles

                    let state =
                        { state with
                            MethodHandles = reg
                        }

                    value, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ValueType returnValue) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetNextIntroducedMethod",
          [ ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System",
                                                             "RuntimeMethodHandleInternal",
                                                             refGenerics)) ],
          MethodReturnType.Void when refGenerics.IsEmpty ->
            // Second half of the IntroducedMethodEnumerator pair. Reads the byref'd handle,
            // advances to the next introduced method on the same declaring type (in metadata
            // order), and writes the new handle through the byref. A null/zero handle is written
            // when the iteration is exhausted (RuntimeHandles.cs:359-370).
            let operation = "RuntimeTypeHandle.GetNextIntroducedMethod"

            let methodPtr =
                NativeCall.managedPointerOfPointerArgument operation "method" instruction.Arguments.[0]

            let currentValue = IlMachineState.readManagedByref state methodPtr

            // RuntimeMethodHandleInternal wraps a single IntPtr-shaped m_handle. The byref came
            // from a managed local of struct type, so primitive-like rewrapping during the
            // write/read round-trip can surface the registry id either as a runtime pointer (the
            // form GetFirst returns) or as a NativeInt with a MethodHandlePtr source (the form
            // produced after passing through an IntPtr field). The shared helper accepts both.
            let currentId =
                match NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation currentValue with
                | Some id -> id
                | None -> 0L

            if currentId = 0L then
                failwith
                    $"%s{operation}: byref already held a null RuntimeMethodHandleInternal; the BCL's IntroducedMethodEnumerator only calls GetNextIntroducedMethod when the current handle is non-null"

            let methodHandle =
                MethodHandleRegistry.resolveMethodFromId currentId state.MethodHandles
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: registry id %d{currentId} did not resolve to a known MethodHandle"
                )

            // The registry only stores handles whose declaring type was Concrete (GetFirst emits
            // the null sentinel for TypeDesc handles), so `None` here would mean the iterator was
            // resumed against a handle whose declaring type can no longer produce methods.
            let declaringType, methods =
                introducedMethodsOfClosed operation state methodHandle.DeclaringType
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: registry handle %d{currentId} resolves to declaring type %O{methodHandle.DeclaringType}, which does not enumerate introduced methods"
                )

            let currentMetadataHandle = methodHandle.GetMethodDefinitionHandle ()

            let nextValue, state =
                let rec findNext (xs : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list) =
                    match xs with
                    | [] ->
                        failwith
                            $"%s{operation}: current method (token %O{currentMetadataHandle}) was not found in declaring type's introduced-methods list"
                    | head :: tail ->
                        if ComparableMethodDefinitionHandle.Make head.Handle = currentMetadataHandle then
                            tail
                        else
                            findNext tail

                match findNext methods with
                | [] ->
                    let zero =
                        MethodHandleRegistry.zeroInternalHandle ctx.BaseClassTypes state.ConcreteTypes

                    zero, state
                | nextMethod :: _ ->
                    let value, reg =
                        MethodHandleRegistry.getOrAllocateInternalHandle
                            ctx.BaseClassTypes
                            state.ConcreteTypes
                            declaringType
                            nextMethod
                            state.MethodHandles

                    let state =
                        { state with
                            MethodHandles = reg
                        }

                    value, state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    methodPtr
                    (CliType.ValueType nextValue)

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
