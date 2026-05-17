namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal MethodTableProjection =
    let private hasComponentSizeFlag : int32 = Int32.MinValue
    let private containsGcPointersFlag : int32 = 0x01000000
    let private containsGenericVariablesFlag : int32 = 0x20000000

    let private genericsMaskNonGeneric : int32 = 0x00000000
    let private genericsMaskGenericInst : int32 = 0x00000010
    let private genericsMaskTypicalInst : int32 = 0x00000030

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

    let private isMethodTableAuxiliaryDataField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        field.DeclaringType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName
        && field.DeclaringType.Namespace = "System.Runtime.CompilerServices"
        && field.DeclaringType.Name = "MethodTableAuxiliaryData"
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
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None

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

    let private typeInfoForIdentityOrFail
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match state.LoadedAssembly identity.Assembly with
        | Some assembly -> assembly, assembly.TypeDefs.[identity.TypeDefinition.Get]
        | None -> failwith $"Open generic MethodTable target assembly was not loaded: %s{identity.AssemblyFullName}"

    let private openGenericTypeInfoOrFail
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        typeInfoForIdentityOrFail state identity |> snd

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
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> Some NATIVE_INT_SIZE
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
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> Some false
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

    let private categoryFlagsForTypeInfo
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : int32
        =
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
            categoryFlagsForTypeInfo baseClassTypes state typeInfo
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> 0

    let private categoryFlagsForRuntimeTypeHandleTarget
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : RuntimeTypeHandleTarget)
        : int32
        =
        match methodTableFor with
        | RuntimeTypeHandleTarget.Closed handle -> categoryFlags baseClassTypes state handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            openGenericTypeInfoOrFail state identity
            |> categoryFlagsForTypeInfo baseClassTypes state
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"TODO: categoryFlagsForRuntimeTypeHandleTarget for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: categoryFlagsForRuntimeTypeHandleTarget for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

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
        (loggerFactory : ILoggerFactory)
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
                // Reference-type zeros are object references, so inspect their instance layout instead.
                let state, fields =
                    IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state containsForHandle

                let containsGcPointers =
                    fields
                    |> List.exists (fun field -> CliType.containsObjectReferences field.Contents)

                containsGcPointers, state
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false, state
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> failwith $"unreachable: array MethodTable %O{containsForHandle} handled above"

    let private containsGcPointers
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : bool * IlMachineState
        =
        match tryArrayElement methodTableFor with
        | Some (element, _) ->
            match tryFastContainsGcPointers baseClassTypes state element with
            | Some result -> result, state
            | None -> containsGcPointersForHandle loggerFactory baseClassTypes state element
        | None when isStringType baseClassTypes state methodTableFor -> false, state
        | None -> containsGcPointersForHandle loggerFactory baseClassTypes state methodTableFor

    let rec private typeDefnFieldMayContainGcPointers
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (visited : Set<ResolvedTypeIdentity>)
        (fieldType : TypeDefn)
        : bool * IlMachineState
        =
        let contains =
            typeDefnFieldMayContainGcPointers loggerFactory baseClassTypes state currentAssembly typeGenericArgs visited

        match fieldType with
        | TypeDefn.PrimitiveType primitiveType ->
            let result =
                match primitiveType with
                | PrimitiveType.Boolean
                | PrimitiveType.Char
                | PrimitiveType.SByte
                | PrimitiveType.Byte
                | PrimitiveType.Int16
                | PrimitiveType.UInt16
                | PrimitiveType.Int32
                | PrimitiveType.UInt32
                | PrimitiveType.Int64
                | PrimitiveType.UInt64
                | PrimitiveType.Single
                | PrimitiveType.Double
                | PrimitiveType.IntPtr
                | PrimitiveType.UIntPtr -> false
                | PrimitiveType.String
                | PrimitiveType.TypedReference
                | PrimitiveType.Object -> true

            result, state
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> false, state
        | TypeDefn.Pinned inner
        | TypeDefn.Modified (inner, _, _) -> contains inner
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> true, state
        | TypeDefn.GenericTypeParameter index ->
            if index < typeGenericArgs.Length then
                match typeGenericArgs.[index] with
                | TypeDefn.GenericTypeParameter _
                | TypeDefn.GenericMethodParameter _ -> true, state
                | genericArg -> contains genericArg
            else
                true, state
        | TypeDefn.GenericMethodParameter _ -> true, state
        | TypeDefn.FromReference (_, SignatureTypeKind.Class)
        | TypeDefn.FromDefinition (_, SignatureTypeKind.Class) -> true, state
        | TypeDefn.FromReference (_, SignatureTypeKind.ValueType)
        | TypeDefn.FromDefinition (_, SignatureTypeKind.ValueType) ->
            typeDefnInstanceFieldsMayContainGcPointers
                loggerFactory
                baseClassTypes
                state
                currentAssembly
                typeGenericArgs
                visited
                fieldType
        | TypeDefn.FromReference (_, SignatureTypeKind.Unknown)
        | TypeDefn.FromDefinition (_, SignatureTypeKind.Unknown) ->
            let state, assembly, typeInfo =
                resolveTypeInfoForTypeDefn loggerFactory baseClassTypes state currentAssembly typeGenericArgs fieldType

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                typeInfoInstanceFieldsMayContainGcPointers
                    loggerFactory
                    baseClassTypes
                    state
                    assembly
                    typeInfo.Generics
                    visited
                    typeInfo.Identity
                    typeInfo.BaseType
                    typeInfo.Fields
            else
                true, state
        | TypeDefn.FromReference (_, other)
        | TypeDefn.FromDefinition (_, other) ->
            failwith $"TODO: MethodTable::Flags GC pointer projection for SignatureTypeKind %O{other}"
        | TypeDefn.GenericInstantiation (generic, _) ->
            match generic with
            | TypeDefn.FromReference (_, SignatureTypeKind.Class)
            | TypeDefn.FromDefinition (_, SignatureTypeKind.Class) -> true, state
            | TypeDefn.FromReference (_, SignatureTypeKind.ValueType)
            | TypeDefn.FromDefinition (_, SignatureTypeKind.ValueType) ->
                typeDefnInstanceFieldsMayContainGcPointers
                    loggerFactory
                    baseClassTypes
                    state
                    currentAssembly
                    typeGenericArgs
                    visited
                    fieldType
            | TypeDefn.FromReference (_, SignatureTypeKind.Unknown)
            | TypeDefn.FromDefinition (_, SignatureTypeKind.Unknown) ->
                let state, assembly, typeInfo =
                    resolveTypeInfoForTypeDefn
                        loggerFactory
                        baseClassTypes
                        state
                        currentAssembly
                        typeGenericArgs
                        fieldType

                if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                    typeInfoInstanceFieldsMayContainGcPointers
                        loggerFactory
                        baseClassTypes
                        state
                        assembly
                        typeInfo.Generics
                        visited
                        typeInfo.Identity
                        typeInfo.BaseType
                        typeInfo.Fields
                else
                    true, state
            | TypeDefn.FromReference (_, other)
            | TypeDefn.FromDefinition (_, other) ->
                failwith $"TODO: MethodTable::Flags GC pointer projection for generic SignatureTypeKind %O{other}"
            | TypeDefn.PrimitiveType _
            | TypeDefn.Array _
            | TypeDefn.Pinned _
            | TypeDefn.Pointer _
            | TypeDefn.Byref _
            | TypeDefn.OneDimensionalArrayLowerBoundZero _
            | TypeDefn.Modified _
            | TypeDefn.GenericInstantiation _
            | TypeDefn.FunctionPointer _
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _
            | TypeDefn.Void -> contains generic

    and private resolveTypeInfoForTypeDefn
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (fieldType : TypeDefn)
        : IlMachineState * DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>
        =
        match fieldType with
        | TypeDefn.FromDefinition (identity, _) ->
            let assembly, typeInfo = typeInfoForIdentityOrFail state identity

            let typeInfo =
                typeInfo
                |> TypeInfo.mapGeneric (fun (param, _) ->
                    if param.SequenceNumber < typeGenericArgs.Length then
                        typeGenericArgs.[param.SequenceNumber]
                    else
                        TypeDefn.GenericTypeParameter param.SequenceNumber
                )

            state, assembly, typeInfo
        | TypeDefn.FromReference (typeRef, _) ->
            IlMachineTypeResolution.resolveTypeFromRef loggerFactory currentAssembly typeRef typeGenericArgs state
        | TypeDefn.GenericInstantiation (generic, args) ->
            // The instantiation owns the target type's generic arguments here. If one of those arguments
            // is still an unbound outer parameter, the field walker treats it conservatively as maybe-GC.
            resolveTypeInfoForTypeDefn loggerFactory baseClassTypes state currentAssembly args generic
        | TypeDefn.PrimitiveType primitiveType ->
            let typeInfo =
                match primitiveType with
                | PrimitiveType.Boolean -> baseClassTypes.Boolean
                | PrimitiveType.Char -> baseClassTypes.Char
                | PrimitiveType.SByte -> baseClassTypes.SByte
                | PrimitiveType.Byte -> baseClassTypes.Byte
                | PrimitiveType.Int16 -> baseClassTypes.Int16
                | PrimitiveType.UInt16 -> baseClassTypes.UInt16
                | PrimitiveType.Int32 -> baseClassTypes.Int32
                | PrimitiveType.UInt32 -> baseClassTypes.UInt32
                | PrimitiveType.Int64 -> baseClassTypes.Int64
                | PrimitiveType.UInt64 -> baseClassTypes.UInt64
                | PrimitiveType.Single -> baseClassTypes.Single
                | PrimitiveType.Double -> baseClassTypes.Double
                | PrimitiveType.String -> baseClassTypes.String
                | PrimitiveType.TypedReference -> baseClassTypes.TypedReference
                | PrimitiveType.IntPtr -> baseClassTypes.IntPtr
                | PrimitiveType.UIntPtr -> baseClassTypes.UIntPtr
                | PrimitiveType.Object -> baseClassTypes.Object
                |> TypeInfo.mapGeneric (fun _ -> failwith "primitive MethodTable target unexpectedly had generics")

            state, baseClassTypes.Corelib, typeInfo
        | TypeDefn.Array _
        | TypeDefn.Pinned _
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Modified _
        | TypeDefn.FunctionPointer _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> failwith $"TODO: MethodTable::Flags type-info resolution for %O{fieldType}"

    and private typeDefnInstanceFieldsMayContainGcPointers
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (visited : Set<ResolvedTypeIdentity>)
        (fieldType : TypeDefn)
        : bool * IlMachineState
        =
        match fieldType with
        | TypeDefn.PrimitiveType primitiveType ->
            match primitiveType with
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> false, state
            | PrimitiveType.String
            | PrimitiveType.TypedReference
            | PrimitiveType.Object -> true, state
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.GenericInstantiation _ ->
            let state, assembly, typeInfo =
                resolveTypeInfoForTypeDefn loggerFactory baseClassTypes state currentAssembly typeGenericArgs fieldType

            typeInfoInstanceFieldsMayContainGcPointers
                loggerFactory
                baseClassTypes
                state
                assembly
                typeInfo.Generics
                visited
                typeInfo.Identity
                typeInfo.BaseType
                typeInfo.Fields
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> true, state
        | TypeDefn.Pinned inner
        | TypeDefn.Modified (inner, _, _) ->
            typeDefnInstanceFieldsMayContainGcPointers
                loggerFactory
                baseClassTypes
                state
                currentAssembly
                typeGenericArgs
                visited
                inner
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void ->
            failwith
                $"typeDefnInstanceFieldsMayContainGcPointers: caller passed unresolved generic parameter or void as a value-type signature: %O{fieldType}"

    and private typeInfoInstanceFieldsMayContainGcPointers
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (visited : Set<ResolvedTypeIdentity>)
        (identity : ResolvedTypeIdentity)
        (baseType : BaseTypeInfo option)
        (fields : FieldInfo<GenericParamFromMetadata, TypeDefn> list)
        : bool * IlMachineState
        =
        if visited.Contains identity then
            false, state
        else
            let visited = visited.Add identity

            let state, inheritedContainsGcPointers =
                match baseType with
                | None -> state, false
                | Some baseTypeInfo ->
                    let state, baseAssembly, baseTypeDefn =
                        IlMachineState.resolveBaseTypeInfo
                            loggerFactory
                            baseClassTypes
                            state
                            currentAssembly
                            baseTypeInfo

                    let inheritedContainsGcPointers, state =
                        typeDefnInstanceFieldsMayContainGcPointers
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssembly
                            typeGenericArgs
                            visited
                            baseTypeDefn

                    state, inheritedContainsGcPointers

            if inheritedContainsGcPointers then
                true, state
            else
                ((false, state), fields)
                ||> List.fold (fun (containsGcPointers, state) field ->
                    if containsGcPointers || field.IsStatic then
                        containsGcPointers, state
                    else
                        typeDefnFieldMayContainGcPointers
                            loggerFactory
                            baseClassTypes
                            state
                            currentAssembly
                            typeGenericArgs
                            visited
                            field.Signature
                )

    let private openGenericContainsGcPointers
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : bool * IlMachineState
        =
        let assembly, typeInfo = typeInfoForIdentityOrFail state identity

        typeInfoInstanceFieldsMayContainGcPointers
            loggerFactory
            baseClassTypes
            state
            assembly
            ImmutableArray.Empty
            Set.empty
            typeInfo.Identity
            typeInfo.BaseType
            typeInfo.Fields

    let private containsGcPointersForRuntimeTypeHandleTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : RuntimeTypeHandleTarget)
        : bool * IlMachineState
        =
        match methodTableFor with
        | RuntimeTypeHandleTarget.Closed handle -> containsGcPointers loggerFactory baseClassTypes state handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            openGenericContainsGcPointers loggerFactory baseClassTypes state identity
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"TODO: containsGcPointersForRuntimeTypeHandleTarget for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: containsGcPointersForRuntimeTypeHandleTarget for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

    let private genericsFlags (state : IlMachineState) (methodTableFor : RuntimeTypeHandleTarget) : int32 =
        match methodTableFor with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> genericsMaskTypicalInst
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith $"TODO: genericsFlags for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: genericsFlags for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.Closed handle ->
            match tryArrayElement handle with
            | Some _ -> genericsMaskNonGeneric
            | None ->
                match tryConcreteTypeInfo state handle with
                | Some (concreteType, _) when not concreteType.Generics.IsEmpty -> genericsMaskGenericInst
                | Some _
                | None -> genericsMaskNonGeneric

    /// Whether a MethodTable target represents a type that contains unbound generic variables.
    /// Closed (`ConcreteTypeHandle`) targets always return `false` because `ConcreteTypeHandle`
    /// represents only fully-constructed types; open generic type definitions are required by
    /// invariant to have a non-empty generic parameter list.
    let targetContainsGenericVariables
        (operation : string)
        (state : IlMachineState)
        (methodTableFor : RuntimeTypeHandleTarget)
        : bool
        =
        match methodTableFor with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let typeInfo = openGenericTypeInfoOrFail state identity

            if typeInfo.Generics.IsEmpty then
                failwith $"%s{operation}: open generic MethodTable target had no generic parameters: %O{identity}"
            else
                true
        | RuntimeTypeHandleTarget.Closed _ -> false
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // A generic parameter T is itself an unbound variable, so its MethodTable contains
            // generic variables. Treating this is conservatively correct for the flag's intent.
            true

    let private containsGenericVariablesFlags
        (state : IlMachineState)
        (methodTableFor : RuntimeTypeHandleTarget)
        : int32
        =
        if targetContainsGenericVariables "MethodTable::Flags" state methodTableFor then
            containsGenericVariablesFlag
        else
            0

    let private flagsForRuntimeTypeHandleTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : RuntimeTypeHandleTarget)
        : int32 * IlMachineState
        =
        let hasComponentSize =
            match methodTableFor with
            | RuntimeTypeHandleTarget.Closed handle ->
                Option.isSome (tryArrayElement handle)
                || isStringType baseClassTypes state handle
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> false
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> false

        let containsGcPointers, state =
            containsGcPointersForRuntimeTypeHandleTarget loggerFactory baseClassTypes state methodTableFor

        let componentSizeBits, state =
            match methodTableFor with
            | RuntimeTypeHandleTarget.Closed handle when hasComponentSize ->
                // CoreCLR overlaps ComponentSize with the low 16 bits of Flags for component MethodTables.
                let componentSize, state = componentSize baseClassTypes state handle
                int32<uint16> componentSize, state
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> 0, state
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> 0, state

        let flags =
            categoryFlagsForRuntimeTypeHandleTarget baseClassTypes state methodTableFor
            ||| componentSizeBits
            ||| (if hasComponentSize then hasComponentSizeFlag else 0)
            ||| (if containsGcPointers then containsGcPointersFlag else 0)
            ||| (if hasComponentSize then
                     0
                 else
                     genericsFlags state methodTableFor)
            ||| containsGenericVariablesFlags state methodTableFor

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
                if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                    let zero, state =
                        IlMachineState.cliTypeZeroOfHandle state baseClassTypes methodTableFor

                    uint32 (CliType.sizeOf zero), state
                else
                    failwith
                        $"TODO: MethodTable::GetNumInstanceFieldBytes projection for non-value type %O{methodTableFor}"
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> uint32 NATIVE_INT_SIZE, state
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith $"TODO: MethodTable::GetNumInstanceFieldBytes projection for array type %O{methodTableFor}"

    let tryProjectFieldForRuntimeTypeHandleTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (methodTableFor : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : (CliType * IlMachineState) option
        =
        if not (isMethodTableField baseClassTypes field) then
            None
        else
            match field.Name with
            | "Flags" ->
                let flags, state =
                    flagsForRuntimeTypeHandleTarget loggerFactory baseClassTypes state methodTableFor

                Some (uint32Field (uint32 flags), state)
            | "BaseSize" ->
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed handle -> Some (uint32Field (uint32 (baseSize handle)), state)
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith $"TODO: MethodTable::BaseSize projection for %O{methodTableFor}"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith $"TODO: MethodTable::BaseSize projection for %O{methodTableFor}"
            | "ComponentSize" ->
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed handle ->
                    let componentSize, state = componentSize baseClassTypes state handle
                    Some (CliType.Numeric (CliNumericType.UInt16 componentSize), state)
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith $"TODO: MethodTable::ComponentSize projection for %O{methodTableFor}"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith $"TODO: MethodTable::ComponentSize projection for %O{methodTableFor}"
            | "ElementType" ->
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed handle ->
                    match tryArrayElement handle with
                    | Some (element, _) ->
                        Some (
                            CliType.RuntimePointer (
                                CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed element)
                            ),
                            state
                        )
                    | None -> failwith $"TODO: MethodTable::ElementType projection for non-array type %O{handle}"
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith $"TODO: MethodTable::ElementType projection for %O{methodTableFor}"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith $"TODO: MethodTable::ElementType projection for %O{methodTableFor}"
            | "AuxiliaryData" ->
                // CoreCLR represents generic parameters (TypeVarTypeDesc) as TypeDesc handles, which
                // have no MethodTable and therefore no AuxiliaryData. Keep the projection honest:
                // only Closed and OpenGenericTypeDefinition targets carry a MethodTable.
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed _
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    Some (CliType.RuntimePointer (CliRuntimePointer.MethodTableAuxiliaryDataPtr methodTableFor), state)
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith
                        $"MethodTable::AuxiliaryData projection refused for TypeDesc target %O{methodTableFor}: generic parameters have no MethodTable in CoreCLR"
            | "ParentMethodTable" ->
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed handle ->
                    let state, parent =
                        IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state handle

                    let result =
                        match parent with
                        | Some parentHandle ->
                            CliType.RuntimePointer (
                                CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed parentHandle)
                            )
                        | None ->
                            // CoreCLR sets ParentMethodTable to null at System.Object; the cast-walk
                            // loops in CastHelpers (e.g. IsInstanceOfClass, ChkCastClassSpecial) check
                            // for null before the next dereference.
                            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

                    Some (result, state)
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith $"TODO: MethodTable::ParentMethodTable projection for %O{methodTableFor}"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith
                        $"MethodTable::ParentMethodTable projection refused for TypeDesc target %O{methodTableFor}: generic parameters have no MethodTable in CoreCLR"
            | "PerInstInfo" ->
                // ElementType and PerInstInfo share a FieldOffset on the
                // CoreCLR struct: only one is meaningful per MethodTable.
                // PerInstInfo holds `MethodTable***` indexed by dictionary
                // slot. For a type with `N` per-instance dictionaries the
                // *first* slot contains the *base* type's dictionary and the
                // *last* slot contains the type's own — PawPrint only walks
                // the synthetic chain for types with a single dictionary
                // whose own generic args occupy that one slot. The current
                // call site is `CastHelpers.IsNullableForType`, so we gate
                // strictly to `System.Nullable\`1` to keep the deref correct
                // by construction; other generic instantiations would need
                // explicit dictionary-index modelling before this can be
                // broadened.
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Concrete _ ->
                        let concreteType, _ = concreteTypeInfoOrFail state handle

                        let isNullable =
                            concreteType.Namespace = "System"
                            && concreteType.Name = "Nullable`1"
                            && concreteType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName

                        if not isNullable then
                            failwith
                                $"MethodTable::PerInstInfo projection refused for %O{handle}: PawPrint only models the synthetic PerInstInfo dictionary chain for System.Nullable`1 today; broader support requires explicit dictionary-index modelling because the first PerInstInfo slot holds the base type's dictionary in inherited generic chains"
                        elif concreteType.Generics.IsEmpty then
                            failwith
                                $"MethodTable::PerInstInfo projection refused for %O{handle}: System.Nullable`1 instantiation unexpectedly has no generic arguments"
                        else
                            Some (CliType.RuntimePointer (CliRuntimePointer.PerInstInfoPtr handle), state)
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        failwith
                            $"MethodTable::PerInstInfo projection refused for array %O{handle}: arrays carry ElementType in this union slot, not PerInstInfo"
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ ->
                        failwith
                            $"MethodTable::PerInstInfo projection refused for TypeDesc-shaped handle %O{handle}: TypeDescs have no MethodTable in CoreCLR"
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    failwith
                        $"MethodTable::PerInstInfo projection refused for open generic definition %O{methodTableFor}: PerInstInfo is meaningful only for closed instantiations"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    failwith
                        $"MethodTable::PerInstInfo projection refused for TypeDesc target %O{methodTableFor}: generic parameters have no MethodTable in CoreCLR"
            | _ ->
                failwith
                    $"TODO: MethodTable field projection for System.Runtime.CompilerServices.MethodTable::{field.Name} on %O{methodTableFor}"

    let tryProjectField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (methodTableFor : ConcreteTypeHandle)
        (state : IlMachineState)
        : (CliType * IlMachineState) option
        =
        tryProjectFieldForRuntimeTypeHandleTarget
            loggerFactory
            baseClassTypes
            field
            (RuntimeTypeHandleTarget.Closed methodTableFor)
            state

    let tryProjectAuxiliaryDataField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (methodTableFor : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : (CliType * IlMachineState) option
        =
        if not (isMethodTableAuxiliaryDataField baseClassTypes field) then
            None
        else
            match field.Name with
            | "Flags" ->
                // Start with the cache bits unset. Managed CoreLib will call the QCall helper,
                // which computes the answer against PawPrint's structured type information.
                Some (uint32Field 0u, state)
            | _ ->
                failwith
                    $"TODO: MethodTableAuxiliaryData field projection for System.Runtime.CompilerServices.MethodTableAuxiliaryData::{field.Name} on %O{methodTableFor}"

    /// Address-side counterpart of `tryProjectAuxiliaryDataField`: produce the
    /// synthetic byref returned by `ldflda` on a MethodTableAuxiliaryData field
    /// whose layout we model directly. The `ExposedClassObjectRaw` cell is the
    /// CoreCLR-side cache of the type's `RuntimeType`; managed code reads it via
    /// `ldflda → Unsafe.AsPointer → ldind.ref` and treats it as a `RuntimeType*`.
    /// We pre-allocate the canonical `RuntimeType` so that subsequent reads of
    /// the byref are pure registry lookups.
    let tryProjectAuxiliaryDataFieldAddress
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (methodTableFor : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : (ManagedPointerSource * IlMachineState) option
        =
        if not (isMethodTableAuxiliaryDataField baseClassTypes field) then
            None
        else
            match field.Name with
            | "ExposedClassObjectRaw" ->
                match methodTableFor with
                | RuntimeTypeHandleTarget.Closed _
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
                    // Both Closed instantiations and open generic type definitions
                    // have a real MethodTable in CoreCLR, so this auxiliary cell is
                    // well-defined. Pre-allocate the canonical RuntimeType so the
                    // read through the byref is a pure registry lookup.
                    let _addr, state =
                        IlMachineRuntimeMetadata.getOrAllocateType loggerFactory baseClassTypes methodTableFor state

                    let ptr =
                        ManagedPointerSource.Byref (ByrefRoot.MethodTableExposedClassObject methodTableFor, [])

                    Some (ptr, state)
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    // Generic-parameter handles are TypeDescs in CoreCLR; the BCL
                    // reads `h.AsTypeDesc()->ExposedClassObject` (a different field
                    // on a different runtime structure) rather than going through
                    // MethodTableAuxiliaryData. Returning None makes the call site
                    // fail loudly if a future path mistakenly reaches here.
                    None
            | _ -> None
