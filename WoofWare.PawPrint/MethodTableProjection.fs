namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module internal MethodTableProjection =
    let private hasComponentSizeFlag : int32 = Int32.MinValue
    let private containsGcPointersFlag : int32 = 0x01000000

    let private categoryInterface : int32 = 0x000C0000
    let private categoryValueType : int32 = 0x00040000
    let private categoryNullable : int32 = 0x00050000
    let private categoryTruePrimitive : int32 = 0x00070000
    let private categoryArray : int32 = 0x00080000

    // PawPrint carries CLI uint32 fields as Int32 while preserving the low 32 bits; see PrimitiveType.UInt32.
    let private uint32Field (value : uint32) : CliType =
        CliType.Numeric (CliNumericType.Int32 (int32 value))

    let private isMethodTableField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        field.DeclaringType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName
        && field.DeclaringType.Namespace = "System.Runtime.CompilerServices"
        && field.DeclaringType.Name = "MethodTable"
        && field.DeclaringType.Generics.IsEmpty

    let private tryArrayElement (handle : ConcreteTypeHandle) : (ConcreteTypeHandle * int option) option =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero element -> Some (element, None)
        | ConcreteTypeHandle.Array (element, rank) ->
            if rank <= 0 then
                failwith $"MethodTable projection cannot model array rank %i{rank} for %O{handle}"

            Some (element, Some rank)
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> None

    let private isStringType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> false
        | Some concreteType when concreteType.Generics.IsEmpty -> concreteType.Identity = baseClassTypes.String.Identity
        | Some _ -> false

    let private isTruePrimitive
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : bool
        =
        [
            baseClassTypes.Boolean
            baseClassTypes.Char
            baseClassTypes.SByte
            baseClassTypes.Byte
            baseClassTypes.Int16
            baseClassTypes.UInt16
            baseClassTypes.Int32
            baseClassTypes.UInt32
            baseClassTypes.Int64
            baseClassTypes.UInt64
            baseClassTypes.Single
            baseClassTypes.Double
            baseClassTypes.IntPtr
            baseClassTypes.UIntPtr
        ]
        |> List.exists (fun primitive -> TypeInfo.NominallyEqual typeInfo primitive)

    let private tryConcreteTypeInfo
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>) option
        =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> None
        | Some concreteType ->
            let assembly = state._LoadedAssemblies.[concreteType.Identity.AssemblyFullName]

            Some (concreteType, assembly.TypeDefs.[concreteType.Identity.TypeDefinition.Get])

    let private concreteTypeInfoOrFail
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match tryConcreteTypeInfo state handle with
        | Some result -> result
        | None -> failwith $"Concrete MethodTable handle %O{handle} was not registered in AllConcreteTypes"

    let private tryPrimitiveSize
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : int option
        =
        if TypeInfo.NominallyEqual typeInfo baseClassTypes.Boolean then
            Some 1
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.SByte then
            Some 1
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Byte then
            Some 1
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Char then
            Some 2
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Int16 then
            Some 2
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.UInt16 then
            Some 2
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Int32 then
            Some 4
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.UInt32 then
            Some 4
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Single then
            Some 4
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Int64 then
            Some 8
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.UInt64 then
            Some 8
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.Double then
            Some 8
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.IntPtr then
            Some NATIVE_INT_SIZE
        elif TypeInfo.NominallyEqual typeInfo baseClassTypes.UIntPtr then
            Some NATIVE_INT_SIZE
        else
            None

    let private tryFastStorageSize
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : int option
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> Some NATIVE_INT_SIZE
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> Some NATIVE_INT_SIZE
        | ConcreteTypeHandle.Concrete _ ->
            match tryConcreteTypeInfo state handle with
            | None -> None
            | Some (_, typeInfo) ->
                match tryPrimitiveSize baseClassTypes typeInfo with
                | Some size -> Some size
                | None ->
                    if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                        None
                    else
                        Some NATIVE_INT_SIZE

    let private tryFastContainsGcPointers
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : bool option
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> Some true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> Some false
        | ConcreteTypeHandle.Concrete _ ->
            match tryConcreteTypeInfo state handle with
            | None -> None
            | Some (_, typeInfo) ->
                if isTruePrimitive baseClassTypes typeInfo then
                    Some false
                elif DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                    None
                else
                    Some true

    let private categoryFlags
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : int32
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> categoryArray
        | ConcreteTypeHandle.Concrete _ ->
            let _, typeInfo = concreteTypeInfoOrFail state handle

            if typeInfo.IsInterface then
                categoryInterface
            elif
                typeInfo.Assembly.FullName = baseClassTypes.Corelib.Name.FullName
                && typeInfo.Namespace = "System"
                && typeInfo.Name = "Nullable`1"
            then
                categoryNullable
            elif DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                if isTruePrimitive baseClassTypes typeInfo then
                    categoryTruePrimitive
                else
                    categoryValueType
            else
                0
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> 0

    let private componentSize
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : uint16 * IlMachineState
        =
        match tryArrayElement methodTableFor with
        | Some (element, _) ->
            let size, state =
                match tryFastStorageSize baseClassTypes state element with
                | Some size -> size, state
                | None ->
                    let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes element
                    CliType.sizeOf zero, state

            if size < 0 || size > int UInt16.MaxValue then
                failwith $"MethodTable::ComponentSize for %O{methodTableFor} does not fit in UInt16: %i{size}"

            uint16 size, state
        | None when isStringType baseClassTypes state methodTableFor -> 2us, state
        | None -> failwith $"TODO: MethodTable::ComponentSize projection for non-component type %O{methodTableFor}"

    let private baseSize (methodTableFor : ConcreteTypeHandle) : int32 =
        match tryArrayElement methodTableFor with
        | Some (_, None) -> 3 * NATIVE_INT_SIZE
        | Some (_, Some rank) -> (3 + rank) * NATIVE_INT_SIZE
        | None -> failwith $"TODO: MethodTable::BaseSize projection for non-array type %O{methodTableFor}"

    let private containsGcPointersForHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (containsForHandle : ConcreteTypeHandle)
        : bool * IlMachineState
        =
        match containsForHandle with
        | ConcreteTypeHandle.Concrete _ ->
            let _, typeInfo = concreteTypeInfoOrFail state containsForHandle

            if isTruePrimitive baseClassTypes typeInfo then
                false, state
            elif DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                let zero, state =
                    IlMachineState.cliTypeZeroOfHandle state baseClassTypes containsForHandle

                CliType.containsObjectReferences zero, state
            else
                failwith
                    $"TODO: MethodTable::Flags ContainsGCPointers projection for non-array reference type %O{containsForHandle}"
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> false, state
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> failwith $"unreachable: array MethodTable %O{containsForHandle} handled above"

    let private containsGcPointers
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : bool * IlMachineState
        =
        match tryArrayElement methodTableFor with
        | Some (element, _) ->
            match tryFastContainsGcPointers baseClassTypes state element with
            | Some result -> result, state
            | None -> containsGcPointersForHandle baseClassTypes state element
        | None when isStringType baseClassTypes state methodTableFor -> false, state
        | None -> containsGcPointersForHandle baseClassTypes state methodTableFor

    let private flags
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : int32 * IlMachineState
        =
        let hasComponentSize =
            Option.isSome (tryArrayElement methodTableFor)
            || isStringType baseClassTypes state methodTableFor

        let containsGcPointers, state =
            containsGcPointers baseClassTypes state methodTableFor

        let componentSizeBits, state =
            if hasComponentSize then
                // CoreCLR overlaps ComponentSize with the low 16 bits of Flags for component MethodTables.
                let componentSize, state = componentSize baseClassTypes state methodTableFor
                int32<uint16> componentSize, state
            else
                0, state

        let flags =
            categoryFlags baseClassTypes state methodTableFor
            ||| componentSizeBits
            ||| (if hasComponentSize then hasComponentSizeFlag else 0)
            ||| (if containsGcPointers then containsGcPointersFlag else 0)

        flags, state

    let numInstanceFieldBytes
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : uint32 * IlMachineState
        =
        // Incremental projection for RuntimeHelpers.GetSpanDataFrom: primitive
        // element types are enough for RVA-backed ReadOnlySpan<T> initializers.
        // Non-primitive value types need the full instance-field byte layout.
        match methodTableFor with
        | ConcreteTypeHandle.Concrete _ ->
            let _, typeInfo = concreteTypeInfoOrFail state methodTableFor

            match tryPrimitiveSize baseClassTypes typeInfo with
            | Some size -> uint32 size, state
            | None ->
                failwith
                    $"TODO: MethodTable::GetNumInstanceFieldBytes projection for non-primitive type %O{methodTableFor}"
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> uint32 NATIVE_INT_SIZE, state
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith $"TODO: MethodTable::GetNumInstanceFieldBytes projection for array type %O{methodTableFor}"

    let tryProjectField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (methodTableFor : ConcreteTypeHandle)
        (state : IlMachineState)
        : (CliType * IlMachineState) option
        =
        if not (isMethodTableField baseClassTypes field) then
            None
        else
            match field.Name with
            | "Flags" ->
                let flags, state = flags baseClassTypes state methodTableFor
                Some (uint32Field (uint32 flags), state)
            | "BaseSize" -> Some (uint32Field (uint32 (baseSize methodTableFor)), state)
            | "ComponentSize" ->
                let componentSize, state = componentSize baseClassTypes state methodTableFor
                Some (CliType.Numeric (CliNumericType.UInt16 componentSize), state)
            | "ElementType" ->
                match tryArrayElement methodTableFor with
                | Some (element, _) -> Some (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr element), state)
                | None -> failwith $"TODO: MethodTable::ElementType projection for non-array type %O{methodTableFor}"
            | _ ->
                failwith
                    $"TODO: MethodTable field projection for System.Runtime.CompilerServices.MethodTable::{field.Name} on %O{methodTableFor}"
