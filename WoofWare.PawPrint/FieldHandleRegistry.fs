namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type FieldHandle =
    private
        {
            /// The assembly that *defines* the field, which is the only assembly whose tables
            /// <see cref="FieldHandle"/> indexes. Derived from <c>DeclaringType</c> at allocation
            /// rather than supplied: see <c>FieldHandleRegistry.definingAssemblyOf</c>.
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

    /// The assembly that defines the field a handle with this declaring type would name.
    ///
    /// Derived rather than supplied by the caller, because a `FieldDefinitionHandle` indexes the
    /// tables of the assembly that *defines* the field and both declaring-type arms `getOrAllocate`
    /// permits already pin that assembly. A separately-supplied name could therefore only agree
    /// with this or be a bug — and the wrong one is easy to reach, since a caller resolving a
    /// `MemberReference` holds the *referencing* assembly, which is a different one. Supplying that
    /// would mint a second registry id for a field the guest already holds a handle to, breaking
    /// `ReferenceEquals(FieldInfo.GetFieldFromHandle(h), fi)`.
    let private definingAssemblyOf
        (allConcreteTypes : AllConcreteTypes)
        (declaringType : RuntimeTypeHandleTarget)
        : string
        =
        match declaringType with
        | RuntimeTypeHandleTarget.Closed handle ->
            match AllConcreteTypes.lookup handle allConcreteTypes with
            | Some concrete -> concrete.AssemblyFullName
            | None ->
                // `lookup` answers `None` for the structural handles — byref, pointer, array — none
                // of which declares a field with a `FieldDefinitionHandle`. Anything else is a
                // handle that was never registered.
                failwith
                    $"FieldHandleRegistry: declaring type %O{handle} has no registered ConcreteType, so the field's defining assembly cannot be determined"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> identity.AssemblyFullName
        | other ->
            // `getOrAllocate` refuses every other arm before reaching this.
            failwith
                $"BUG: FieldHandleRegistry.definingAssemblyOf reached %O{other}, which getOrAllocate is supposed to have refused"

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
        | RuntimeTypeHandleTarget.Composite _
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            // A byref, pointer or function pointer is a TypeDesc with no fields, and an array's
            // fields are synthesised rather than declared, so no FieldDefinitionHandle names one.
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
                (DeclaredTypeFacts.ofCorelibType baseClassTypes baseClassTypes.RuntimeFieldHandle)
            |> CliType.ValueType

        let handle =
            {
                AssemblyFullName = definingAssemblyOf allConcreteTypes declaringType
                FieldHandle = ComparableFieldDefinitionHandle.Make handle
                DeclaringType = declaringType
            }

        match Map.tryFind handle reg.FieldToHandle with
        | Some v -> runtimeFieldHandle v, reg, allocState
        | None ->

        let newHandle = reg.NextHandle

        let runtimeFieldInfoStub =
            RuntimeFieldInfoStubLayout.build baseClassTypes allConcreteTypes newHandle

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
