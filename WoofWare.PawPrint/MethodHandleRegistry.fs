namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

type MethodHandle =
    private
        {
            AssemblyFullName : string
            DeclaringType : ConcreteTypeHandle
            MethodHandle : ComparableMethodDefinitionHandle
            MethodGenerics : ConcreteTypeHandle list
        }

    member this.GetAssemblyFullName () : string = this.AssemblyFullName
    member this.GetMethodDefinitionHandle () : ComparableMethodDefinitionHandle = this.MethodHandle
    member this.GetMethodGenerics () : ConcreteTypeHandle list = this.MethodGenerics

type MethodHandleRegistry =
    private
        {
            MethodHandleToId : Map<MethodHandle, int64>
            /// Reverse of `MethodHandleToId`. Used by callers (e.g., the introduced-method
            /// iterator on `RuntimeTypeHandle`) that hold a bare `RuntimeMethodHandleInternal`
            /// id and need to recover the underlying `MethodHandle`.
            IdToMethodHandle : Map<int64, MethodHandle>
            MethodHandleToMethod : Map<ManagedHeapAddress, MethodHandle>
            MethodToHandle : Map<MethodHandle, ManagedHeapAddress>
            NextHandle : int64
        }

[<RequireQualifiedAccess>]
module MethodHandleRegistry =
    let empty () =
        {
            MethodHandleToMethod = Map.empty
            MethodToHandle = Map.empty
            MethodHandleToId = Map.empty
            IdToMethodHandle = Map.empty
            NextHandle = 1L
        }

    /// Build a `MethodHandle` describing the canonical identity of a concretised method.
    let private makeMethodHandle
        (allConcreteTypes : AllConcreteTypes)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : MethodHandle
        =
        {
            AssemblyFullName = method.DeclaringType.Assembly.FullName
            MethodHandle = ComparableMethodDefinitionHandle.Make method.Handle
            DeclaringType =
                AllConcreteTypes.findExistingConcreteType
                    allConcreteTypes
                    method.DeclaringType.Identity
                    method.DeclaringType.Generics
                |> Option.defaultWith (fun () ->
                    failwith $"declaring type for method %O{method} was not found in ConcreteTypes"
                )
            MethodGenerics = method.Generics |> Seq.toList
        }

    /// Build a CliValueType representing a `System.RuntimeMethodHandleInternal` whose `m_handle`
    /// field is the given registry id (encoded as a `MethodRegistryHandle` runtime pointer).
    let private buildRuntimeMethodHandleInternal
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (registryId : int64)
        : CliValueType
        =
        let field = baseClassTypes.RuntimeMethodHandleInternal.Fields |> List.exactlyOne

        if field.Name <> "m_handle" then
            failwith $"unexpected field name %s{field.Name} for BCL type RuntimeMethodHandleInternal"

        match field.Signature with
        | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> ()
        | s -> failwith $"bad RuntimeMethodHandleInternal.m_handle signature: {s}"

        FieldIdentity.cliField
            (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandleInternal)
            field
            (CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle registryId))
            (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.IntPtr)
        |> List.singleton
        |> CliValueType.OfFields
            baseClassTypes
            allConcreteTypes
            (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandleInternal)
            Layout.Default

    /// Returns a bare `System.RuntimeMethodHandleInternal` value type for the given method, allocating
    /// a fresh registry id if necessary. No managed-heap allocation is performed; this is the
    /// representation used by `RuntimeTypeHandle.GetFirstIntroducedMethod` /
    /// `GetNextIntroducedMethod`, which surface raw method-table pointers rather than full handles.
    /// The returned value's `m_handle` is a `MethodRegistryHandle` referring to the registry id.
    let getOrAllocateInternalHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (reg : MethodHandleRegistry)
        : CliValueType * MethodHandleRegistry
        =
        let handle = makeMethodHandle allConcreteTypes method

        match Map.tryFind handle reg.MethodHandleToId with
        | Some existingId -> buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes existingId, reg
        | None ->
            let newId = reg.NextHandle

            let reg =
                { reg with
                    MethodHandleToId = reg.MethodHandleToId |> Map.add handle newId
                    IdToMethodHandle = reg.IdToMethodHandle |> Map.add newId handle
                    NextHandle = reg.NextHandle + 1L
                }

            buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes newId, reg

    /// Build a zero-valued `RuntimeMethodHandleInternal`. Matches the BCL's `IsNullHandle()`
    /// sentinel used to terminate `IntroducedMethodEnumerator`.
    let zeroInternalHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        : CliValueType
        =
        buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes 0L

    /// Resolve a `RuntimeMethodHandleInternal` registry id back to its underlying `MethodHandle`,
    /// or return `None` if the id is unknown (including the zero/null id).
    let resolveMethodFromId (id : int64) (reg : MethodHandleRegistry) : MethodHandle option =
        if id = 0L then
            None
        else
            Map.tryFind id reg.IdToMethodHandle

    let rec private isReferenceShaped (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | TypeDefn.PrimitiveType PrimitiveType.Object
        | TypeDefn.PrimitiveType PrimitiveType.String
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.Class)
        | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.Class) -> true
        | TypeDefn.GenericInstantiation (generic, _) -> isReferenceShaped generic
        | TypeDefn.Modified (original, _, _) -> isReferenceShaped original
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

    /// Returns a (struct) System.RuntimeMethodHandle, with its contents (reference type) freshly allocated if necessary.
    let getOrAllocate
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (reg : MethodHandleRegistry)
        : CliType * MethodHandleRegistry * 'allocState
        =

        let runtimeMethodHandle (runtimeMethodInfoStub : ManagedHeapAddress) =
            // RuntimeMethodHandle is a struct; it contains one field, an IRuntimeMethodInfo.
            // In practice we expect to use RuntimeMethodInfoStub for that IRuntimeMethodInfo:
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L802
            let runtimeMethodHandleType = baseClassTypes.RuntimeMethodHandle
            let field = runtimeMethodHandleType.Fields |> List.exactlyOne

            if field.Name <> "m_value" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeMethodHandle"

            FieldIdentity.cliField
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandle)
                field
                (CliType.ofManagedObject runtimeMethodInfoStub)
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodInfoStub)
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandle)
                Layout.Default
            |> CliType.ValueType

        let handle = makeMethodHandle allConcreteTypes method

        match Map.tryFind handle reg.MethodToHandle with
        | Some v -> runtimeMethodHandle v, reg, allocState
        | None ->

        // Reuse an existing registry id for this method if one was minted earlier (e.g., via
        // `getOrAllocateInternalHandle` while iterating introduced methods); otherwise mint a new one.
        let registryId, reg =
            match Map.tryFind handle reg.MethodHandleToId with
            | Some existing -> existing, reg
            | None ->
                let newId = reg.NextHandle

                let reg =
                    { reg with
                        MethodHandleToId = reg.MethodHandleToId |> Map.add handle newId
                        IdToMethodHandle = reg.IdToMethodHandle |> Map.add newId handle
                        NextHandle = reg.NextHandle + 1L
                    }

                newId, reg

        let runtimeMethodHandleInternal =
            buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes registryId
            |> CliType.ValueType

        let runtimeMethodInfoStub =
            let objType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.Object

            let runtimeMethodHandleInternalType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandleInternal

            let runtimeMethodInfoStubHandle =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodInfoStub

            let valueField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.RuntimeMethodInfoStub "m_value"

            let fields =
                baseClassTypes.RuntimeMethodInfoStub.Fields
                |> List.filter (fun field -> not field.IsStatic)
                |> List.map (fun field ->
                    if field.Handle = valueField.Handle then
                        FieldIdentity.cliField
                            runtimeMethodInfoStubHandle
                            field
                            runtimeMethodHandleInternal
                            runtimeMethodHandleInternalType
                    else
                        if not (isReferenceShaped field.Signature) then
                            failwith
                                $"RuntimeMethodInfoStub field %s{field.Name} was expected to be reference-shaped, got %O{field.Signature}"

                        FieldIdentity.cliField runtimeMethodInfoStubHandle field (CliType.ObjectRef None) objType
                )

            if
                fields
                |> List.exists (fun field ->
                    FieldId.exactlyEqual field.Id (FieldIdentity.fieldId runtimeMethodInfoStubHandle valueField)
                )
                |> not
            then
                failwith "RuntimeMethodInfoStub did not contain the expected m_value field"

            fields
            |> CliValueType.OfFields baseClassTypes allConcreteTypes runtimeMethodInfoStubHandle Layout.Default

        let alloc, state = allocate runtimeMethodInfoStub allocState

        let reg =
            { reg with
                MethodHandleToMethod = reg.MethodHandleToMethod |> Map.add alloc handle
                MethodToHandle = reg.MethodToHandle |> Map.add handle alloc
            }

        runtimeMethodHandle alloc, reg, state

    /// Given the ManagedHeapAddress of a RuntimeMethodInfoStub, resolve it to the MethodHandle.
    let resolveMethodFromAddress (addr : ManagedHeapAddress) (reg : MethodHandleRegistry) : MethodHandle option =
        Map.tryFind addr reg.MethodHandleToMethod
