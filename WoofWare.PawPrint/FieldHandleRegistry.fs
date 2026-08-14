namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type FieldHandle =
    private
        {
            AssemblyFullName : string
            /// The declaring type observed at allocation time. CoreCLR's
            /// `typeof(G&lt;int&gt;).GetField(...).FieldHandle` is observably *not*
            /// interchangeable with `typeof(G&lt;&gt;).GetField(...).FieldHandle` —
            /// each carries its own instantiation context and `FieldInfo.GetFieldFromHandle`
            /// rejects a mismatched declaring `RuntimeTypeHandle`. Mirror that here by
            /// keying on the full `RuntimeTypeHandleTarget` the caller supplied: a closed
            /// instantiation gets `Closed`; the open generic definition gets
            /// `OpenGenericTypeDefinition`; and the two yield distinct registry ids.
            DeclaringType : RuntimeTypeHandleTarget
            FieldHandle : ComparableFieldDefinitionHandle
        }

    member this.GetAssemblyFullName () : string = this.AssemblyFullName
    member this.GetDeclaringTypeHandle () : RuntimeTypeHandleTarget = this.DeclaringType
    member this.GetFieldDefinitionHandle () : ComparableFieldDefinitionHandle = this.FieldHandle

type FieldHandleRegistry =
    private
        {
            // The registry is authoritative for id/address/field resolution. The managed
            // RuntimeFieldInfoStub mirrors the id for guest-visible RuntimeFieldHandle state.
            FieldHandleIdToField : Map<int64, FieldHandle>
            FieldHandleAddressToId : Map<ManagedHeapAddress, int64>
            FieldHandleToField : Map<ManagedHeapAddress, FieldHandle>
            FieldToHandle : Map<FieldHandle, ManagedHeapAddress>
            NextHandle : int64
        }

[<RequireQualifiedAccess>]
module FieldHandleRegistry =
    let empty () =
        {
            FieldHandleAddressToId = Map.empty
            FieldHandleToField = Map.empty
            FieldToHandle = Map.empty
            FieldHandleIdToField = Map.empty
            NextHandle = 1L
        }

    let rec private isReferenceShaped (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | TypeDefn.PrimitiveType PrimitiveType.Object
        | TypeDefn.PrimitiveType PrimitiveType.String
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.Class)
        | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.Class) -> true
        | TypeDefn.GenericInstantiation (generic, _) -> isReferenceShaped generic
        | TypeDefn.Modified m -> isReferenceShaped m.Unmodified
        | TypeDefn.PrimitiveType _
        | TypeDefn.Pinned _
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.FunctionPointer _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> false

    /// Returns a (struct) System.RuntimeFieldHandle, with its contents (reference type) freshly allocated if necessary.
    /// `declaringType` must be either `Closed` (a fully concrete declaring type — non-generic or
    /// a particular closed instantiation) or `OpenGenericTypeDefinition` (the open generic typedef,
    /// e.g. for `typeof(Foo&lt;&gt;).GetField`). Distinct targets allocate distinct registry ids,
    /// matching CoreCLR's per-instantiation `RuntimeFieldHandle` identity. Type-parameter targets
    /// cannot own a field and are rejected.
    let getOrAllocate
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (declaringAssy : AssemblyName)
        (declaringType : RuntimeTypeHandleTarget)
        (handle : FieldDefinitionHandle)
        (reg : FieldHandleRegistry)
        : CliType * FieldHandleRegistry * 'allocState
        =
        match declaringType with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery "FieldHandleRegistry.getOrAllocate" scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at FieldHandleRegistry.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.Closed _
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ()
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // Generic-parameter declaring types cannot own a field: fields live on the
            // type that mentions the parameter, not on the parameter itself.
            failwith
                $"FieldHandleRegistry.getOrAllocate: declaring type must be Closed or OpenGenericTypeDefinition, got %O{declaringType}"

        let runtimeFieldHandle (runtimeFieldInfoStub : ManagedHeapAddress) =
            // RuntimeFieldHandle is a struct; it contains one field, an IRuntimeFieldInfo
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1048
            // In practice we expect to use RuntimeFieldInfoStub for that IRuntimeFieldInfo:
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1157
            let runtimeFieldHandleType = baseClassTypes.RuntimeFieldHandle
            let field = runtimeFieldHandleType.Fields |> List.exactlyOne

            if field.Name <> "m_ptr" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeFieldHandle"

            FieldIdentity.cliField
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandle)
                field
                (CliType.ofManagedObject runtimeFieldInfoStub)
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldInfoStub)
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandle)
                Layout.Default
                (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeFieldHandle.TypeAttributes)
            |> CliType.ValueType

        let handle =
            {
                AssemblyFullName = declaringAssy.FullName
                FieldHandle = ComparableFieldDefinitionHandle.Make handle
                DeclaringType = declaringType
            }

        match Map.tryFind handle reg.FieldToHandle with
        | Some v -> runtimeFieldHandle v, reg, allocState
        | None ->

        let newHandle = reg.NextHandle

        let runtimeFieldHandleInternal =
            let field = baseClassTypes.RuntimeFieldHandleInternal.Fields |> List.exactlyOne

            if field.Name <> "m_handle" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeFieldHandleInternal"

            match field.Signature with
            | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> ()
            | s -> failwith $"bad sig: {s}"

            // https://github.com/dotnet/runtime/blob/2b21c73fa2c32fa0195e4a411a435dda185efd08/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1380
            FieldIdentity.cliField
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandleInternal)
                field
                (CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle newHandle))
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.IntPtr)
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandleInternal)
                Layout.Default
                (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeFieldHandleInternal.TypeAttributes)
            |> CliType.ValueType

        // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1074
        let runtimeFieldInfoStub =
            let objType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.Object

            let intType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.Int32

            let runtimeFieldInfoStubHandle =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldInfoStub

            let runtimeFieldHandleInternalType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandleInternal

            let fieldHandleField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.RuntimeFieldInfoStub "m_fieldHandle"

            // LayoutKind.Sequential
            baseClassTypes.RuntimeFieldInfoStub.Fields
            |> List.filter (fun field -> not field.IsStatic)
            |> List.map (fun field ->
                if field.Handle = fieldHandleField.Handle then
                    FieldIdentity.cliField
                        runtimeFieldInfoStubHandle
                        field
                        runtimeFieldHandleInternal
                        runtimeFieldHandleInternalType
                else
                    match field.Signature with
                    | TypeDefn.PrimitiveType PrimitiveType.Int32 ->
                        FieldIdentity.cliField
                            runtimeFieldInfoStubHandle
                            field
                            (CliType.Numeric (CliNumericType.Int32 0))
                            intType
                    | signature when isReferenceShaped signature ->
                        FieldIdentity.cliField runtimeFieldInfoStubHandle field (CliType.ObjectRef None) objType
                    | signature ->
                        failwith
                            $"RuntimeFieldInfoStub field %s{field.Name} was expected to be reference-shaped or int32, got %O{signature}"
            )
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                runtimeFieldInfoStubHandle
                Layout.Default
                (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeFieldInfoStub.TypeAttributes) // explicitly sequential but no custom packing size

        let alloc, state = allocate runtimeFieldInfoStub allocState

        let reg =
            {
                FieldHandleAddressToId = reg.FieldHandleAddressToId |> Map.add alloc newHandle
                FieldHandleToField = reg.FieldHandleToField |> Map.add alloc handle
                FieldToHandle = reg.FieldToHandle |> Map.add handle alloc
                FieldHandleIdToField = reg.FieldHandleIdToField |> Map.add newHandle handle
                NextHandle = reg.NextHandle + 1L
            }

        runtimeFieldHandle alloc, reg, state

    /// Given the ManagedHeapAddress of a RuntimeFieldInfoStub, resolve it to the FieldHandle.
    let resolveFieldFromAddress (addr : ManagedHeapAddress) (reg : FieldHandleRegistry) : FieldHandle option =
        Map.tryFind addr reg.FieldHandleToField

    /// Given the ManagedHeapAddress of a RuntimeFieldInfoStub, resolve it to the integer payload
    /// used by RuntimeFieldHandleInternal / FieldDesc-like native pointers.
    let resolveFieldIdFromAddress (addr : ManagedHeapAddress) (reg : FieldHandleRegistry) : int64 option =
        Map.tryFind addr reg.FieldHandleAddressToId

    /// Given the integer payload of a RuntimeFieldHandleInternal, resolve it to the FieldHandle.
    let resolveFieldFromId (id : int64) (reg : FieldHandleRegistry) : FieldHandle option =
        Map.tryFind id reg.FieldHandleIdToField
