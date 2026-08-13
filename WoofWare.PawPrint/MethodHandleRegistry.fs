namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

/// The identity of a method that exists in some assembly's metadata: a MethodDef token, the
/// assembly it was read from, and the type/method instantiations bound to this particular handle.
/// This is the analogue of a CoreCLR `MethodDesc*` that answers `false` to `IsNoMetadata()`.
type MetadataMethodIdentity =
    private
        {
            AssemblyFullName : string
            DeclaringType : ConcreteTypeHandle
            MethodDefinition : ComparableMethodDefinitionHandle
            MethodGenerics : ConcreteTypeHandle list
        }

    member this.GetAssemblyFullName () : string = this.AssemblyFullName
    member this.GetDeclaringType () : ConcreteTypeHandle = this.DeclaringType
    member this.GetMethodDefinitionHandle () : ComparableMethodDefinitionHandle = this.MethodDefinition
    member this.GetMethodGenerics () : ConcreteTypeHandle list = this.MethodGenerics

/// The identity of a method minted at runtime by `Reflection.Emit` rather than read out of any
/// assembly's metadata: CoreCLR's `DynamicMethodDesc`, which is exactly what
/// `MethodDesc::IsNoMetadata()` answers `true` for.
///
/// The payload is deliberately nothing but the registry id. Two `DynamicMethod`s built with the
/// same name, the same signature and the same module are *different methods*, and CoreCLR's
/// identity for one is its `DynamicMethodDesc`'s address: distinct for any two methods that are
/// live at once, though `DynamicMethodTable::GetDynamicMethod` (dynamicmethod.cpp:218) recycles a
/// desc off its free list once the method it belonged to has died, so an address can be reused
/// across generations. PawPrint never frees, so a monotone counter is the faithful projection of
/// that. Either way nothing descriptive may participate in equality here, or two distinct dynamic
/// methods would collide in the registry's maps. What the method is called, and what its signature
/// says, live in `MethodHandleRegistry.DynamicMethods` keyed by this.
type DynamicMethodHandle =
    private
        {
            RegistryId : int64
        }

    member this.GetRegistryId () : int64 = this.RegistryId

    override this.ToString () : string = $"dynamic method #%d{this.RegistryId}"

/// Everything `ModuleHandle_GetDynamicMethod` (runtimehandles.cpp:2388) was told about a dynamic
/// method: the name and signature blob CoreCLR copies onto the loader heap beside the fresh
/// `DynamicMethodDesc`, the module the method is scoped to, and the managed `DynamicResolver` it
/// attaches to the `LCGMethodResolver`.
///
/// The resolver is held here as a plain address, where CoreCLR holds a *long weak* handle
/// (`AppDomain::GetCurrentDomain()->CreateLongWeakHandle`) so that the runtime does not keep the
/// resolver alive. That difference is unobservable in PawPrint: its interpreter never performs a
/// garbage collection, of any kind, ever (see `Native/NativeGc.fs`), so nothing a weak handle
/// would permit to be collected ever is, and the guest cannot ask after the handle's strength --
/// `GetLCGMethodResolver` is reachable only from native code. Should a collector ever land, this
/// is one of the places that has to grow a real weak reference.
type DynamicMethodDefinition =
    private
        {
            Name : string
            Signature : ImmutableArray<byte>
            ScopeAssemblyFullName : string
            Resolver : ManagedHeapAddress option
            Body : MethodInstructions<TypeDefn>
        }

    member this.GetName () : string = this.Name
    member this.GetSignature () : ImmutableArray<byte> = this.Signature
    member this.GetScopeAssemblyFullName () : string = this.ScopeAssemblyFullName
    member this.GetResolver () : ManagedHeapAddress option = this.Resolver

    /// The IL this method will execute, read out of `Resolver` at the moment the method was
    /// minted (see `DynamicMethodBody`).
    ///
    /// Recorded eagerly and not as an option, so that a minted dynamic method is never in a
    /// half-built state where it has an identity but no body. That will have to give when a
    /// dynamic method is allowed to carry a token for *itself*: CoreCLR installs the method and
    /// assigns `_methodHandle` before anything resolves its tokens, precisely so that the
    /// self-reference has something to resolve *to*, and matching that needs the identity minted
    /// first and the body attached second. Bodies carrying tokens at all are refused today, so
    /// the cycle is unreachable and the simpler shape is the honest one.
    member this.GetBody () : MethodInstructions<TypeDefn> = this.Body

/// What a `RuntimeMethodHandleInternal` registry id can name.
///
/// The cases are public (the payloads are not) specifically so that consumers must match, and so
/// that adding a case forces every site that cares to be revisited by the compiler rather than by
/// a reader. CoreCLR's `MethodDesc` covers both metadata-backed methods and "no metadata" ones --
/// `DynamicMethod`/LCG stubs, which have no MethodDef token and no defining assembly, and which
/// `RuntimeMethodHandle.IsDynamicMethod` (`MethodDesc::IsNoMetadata()`) exists to distinguish.
type MethodHandle =
    | FromMetadata of MetadataMethodIdentity
    | FromDynamic of DynamicMethodHandle

type MethodHandleRegistry =
    private
        {
            /// Dedup index over *structural* method identities, so that asking twice for the
            /// handle of the same method yields the same id. Only `FromMetadata` handles appear
            /// here: a dynamic method has no structural identity to dedup on (see
            /// `DynamicMethodHandle`), so `mintDynamicMethod` deliberately does not populate it.
            MethodHandleToId : Map<MethodHandle, int64>
            /// Every id this registry has minted, mapped to what it names. A superset of the
            /// reverse of `MethodHandleToId`: it is the only *handle-resolution* map a dynamic
            /// method appears in, since the two dedup indices are keyed on a structural identity
            /// it does not have. Used by callers (e.g., the introduced-method iterator on
            /// `RuntimeTypeHandle`) that hold a bare `RuntimeMethodHandleInternal` id and need
            /// to recover the underlying `MethodHandle`.
            IdToMethodHandle : Map<int64, MethodHandle>
            /// What each minted dynamic method was built from. Keyed by the same id that
            /// `IdToMethodHandle` uses, because for a dynamic method the registry id *is* the
            /// identity, exactly as the `DynamicMethodDesc*` is in CoreCLR.
            DynamicMethods : Map<DynamicMethodHandle, DynamicMethodDefinition>
            /// Dedup cache for `getOrAllocate`: when an F# caller asks for the stub address
            /// of a method we've previously allocated through this same path, return that
            /// existing address rather than minting a fresh stub. Note that this dedup is
            /// scoped to F#-side allocations — stubs the BCL constructs in managed code (via
            /// `new RuntimeMethodInfoStub(...)`) bypass this registry entirely. Like
            /// `MethodHandleToId`, this holds only `FromMetadata` handles: CoreCLR allocates a
            /// fresh stub per `ModuleHandle_GetDynamicMethod` call, and the managed lock in
            /// `DynamicMethod.GetMethodDescriptor` means there is exactly one such call per
            /// `DynamicMethod`, so there is nothing to reuse.
            MethodToHandle : Map<MethodHandle, ManagedHeapAddress>
            NextHandle : int64
        }

[<RequireQualifiedAccess>]
module MethodHandleRegistry =
    let empty () =
        {
            MethodToHandle = Map.empty
            MethodHandleToId = Map.empty
            IdToMethodHandle = Map.empty
            DynamicMethods = Map.empty
            NextHandle = 1L
        }

    /// A `MethodHandle` is the identity the guest sees through `RuntimeMethodHandle`, so it is
    /// keyed on a MethodDef token. A synthesised method has no such token, and no reflection
    /// surface either — nothing can name it, so nothing can ask for its handle. Reaching here
    /// with one means some path handed a runtime-supplied method to reflection, which is a bug in
    /// that path rather than something to paper over with a fabricated token.
    let private requireDeclaredMethod (method : MethodInfo<'tyGen, 'methGen, 'vars>) : MethodDefinitionHandle =
        match method.TryMetadata with
        | Some facts -> facts.Handle
        | None ->
            failwith
                $"cannot mint a RuntimeMethodHandle for %O{method}: it is synthesised by the runtime and has no MethodDef token"

    /// Build a `MethodHandle` describing the canonical identity of a concretised method.
    let private makeMethodHandle
        (allConcreteTypes : AllConcreteTypes)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : MethodHandle
        =
        {
            AssemblyFullName = method.DeclaringType.Assembly.FullName
            MethodDefinition = ComparableMethodDefinitionHandle.Make (requireDeclaredMethod method)
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
        |> MethodHandle.FromMetadata

    /// Build a CliValueType representing a `System.RuntimeMethodHandleInternal` whose `m_handle`
    /// field carries the given verbatim CliType. Callers pass either a `MethodRegistryHandle id`
    /// runtime pointer (for live methods) or a verbatim zero `NativeInt` (for the null sentinel
    /// recognised by `RuntimeMethodHandleInternal.IsNullHandle()`, which compares m_handle to
    /// `IntPtr.Zero`).
    let private buildRuntimeMethodHandleInternal
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (mHandleValue : CliType)
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
            mHandleValue
            (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.IntPtr)
        |> List.singleton
        |> CliValueType.OfFields
            baseClassTypes
            allConcreteTypes
            (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandleInternal)
            Layout.Default
            (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeMethodHandleInternal.TypeAttributes)

    /// Construct the `MethodHandle` that identifies an open method declared on `declaringType`.
    /// Callers in the introduced-method iterator path use this rather than going through
    /// `concretizeMethod`, since the BCL's enumerator surfaces method-table slots (i.e., method
    /// definitions) and a generic-method definition cannot be expressed with empty
    /// `MethodGenerics` via the normal concretization path.
    let private makeOpenMethodHandle
        (allConcreteTypes : AllConcreteTypes)
        (declaringType : ConcreteType<ConcreteTypeHandle>)
        (method : MethodInfo<'tyGen, GenericParamFromMetadata, TypeDefn>)
        : MethodHandle
        =
        let declaringHandle =
            AllConcreteTypes.findExistingConcreteType allConcreteTypes declaringType.Identity declaringType.Generics
            |> Option.defaultWith (fun () ->
                failwith $"declaring type %O{declaringType} was not registered in ConcreteTypes"
            )

        {
            AssemblyFullName = declaringType.Assembly.FullName
            DeclaringType = declaringHandle
            MethodDefinition = ComparableMethodDefinitionHandle.Make (requireDeclaredMethod method)
            MethodGenerics = []
        }
        |> MethodHandle.FromMetadata

    /// Returns a bare `System.RuntimeMethodHandleInternal` value type identifying the given method
    /// declared on `declaringType`, allocating a fresh registry id if necessary. No managed-heap
    /// allocation is performed; this is the representation used by
    /// `RuntimeTypeHandle.GetFirstIntroducedMethod` / `GetNextIntroducedMethod`, which surface raw
    /// method-table slots rather than full handles. Method-generic parameters of the input
    /// `method` are intentionally NOT instantiated: the iterator returns the method definition
    /// (analogous to a CoreCLR open `MethodDesc*`), so the registered handle has empty
    /// `MethodGenerics`.
    let getOrAllocateInternalHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (declaringType : ConcreteType<ConcreteTypeHandle>)
        (method : MethodInfo<'tyGen, GenericParamFromMetadata, TypeDefn>)
        (reg : MethodHandleRegistry)
        : CliValueType * MethodHandleRegistry
        =
        let handle = makeOpenMethodHandle allConcreteTypes declaringType method

        let registryId, reg =
            match Map.tryFind handle reg.MethodHandleToId with
            | Some existingId -> existingId, reg
            | None ->
                let newId = reg.NextHandle

                let reg =
                    { reg with
                        MethodHandleToId = reg.MethodHandleToId |> Map.add handle newId
                        IdToMethodHandle = reg.IdToMethodHandle |> Map.add newId handle
                        NextHandle = reg.NextHandle + 1L
                    }

                newId, reg

        let mHandle =
            CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle registryId)

        buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes mHandle, reg

    /// Build a zero-valued `RuntimeMethodHandleInternal`. Matches the BCL's `IsNullHandle()`
    /// sentinel used to terminate `IntroducedMethodEnumerator`: `m_handle` is a verbatim
    /// `IntPtr.Zero`, so managed `m_handle == IntPtr.Zero` checks see it as null.
    let zeroInternalHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        : CliValueType
        =
        let zero = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

        buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes zero

    /// Build a `RuntimeMethodHandleInternal` value type whose `m_handle` field carries the given
    /// registry id. Rejects the zero id (which is the BCL's null sentinel and must be constructed
    /// via `zeroInternalHandle` so the resulting struct's `IsNullHandle()` check sees `IntPtr.Zero`
    /// rather than a non-null `MethodRegistryHandle 0L`). The id is otherwise assumed live: a
    /// non-zero id that was never allocated will yield a struct that resolves to `None` from
    /// `resolveMethodFromId`.
    let internalHandleFromId
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (id : int64)
        : CliValueType
        =
        if id = 0L then
            failwith
                "MethodHandleRegistry.internalHandleFromId: refusing to wrap zero id as a live RuntimeMethodHandleInternal; use zeroInternalHandle for the null sentinel"

        let mHandle = CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id)
        buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes mHandle

    /// Resolve a `RuntimeMethodHandleInternal` registry id back to its underlying `MethodHandle`,
    /// or return `None` if the id is unknown (including the zero/null id).
    let resolveMethodFromId (id : int64) (reg : MethodHandleRegistry) : MethodHandle option =
        if id = 0L then
            None
        else
            Map.tryFind id reg.IdToMethodHandle

    /// Mint (or reuse) a registry id for the given fully-concretised method and return a
    /// `RuntimeMethodHandleInternal` value type referencing it. Unlike `getOrAllocate`, this
    /// does not also allocate a `RuntimeMethodInfoStub` on the managed heap: callers that
    /// only need the bare `RuntimeMethodHandleInternal` (e.g. the `ModuleHandle.ResolveMethod`
    /// QCall, whose C# wrapper allocates the stub itself) can avoid the unnecessary heap object.
    let getOrAllocateConcreteInternalHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (reg : MethodHandleRegistry)
        : CliValueType * MethodHandleRegistry
        =
        let handle = makeMethodHandle allConcreteTypes method

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

        let mHandle =
            CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle registryId)

        buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes mHandle, reg

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

    /// Build the `RuntimeMethodInfoStub` value CoreCLR's `MethodDesc::AllocateStubMethodInfo`
    /// (method.cpp:3809) hands back: an object whose `m_value` is the given
    /// `RuntimeMethodHandleInternal` and whose every other field is null.
    ///
    /// CoreCLR's only other assignment there is `m_keepalive`, which it sets to the
    /// LoaderAllocator's exposed object when — and only when — that allocator is collectible.
    /// PawPrint models no collectible loader allocator, so leaving it null is faithful rather
    /// than a simplification.
    let private buildRuntimeMethodInfoStub
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (runtimeMethodHandleInternal : CliType)
        : CliValueType
        =
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
        |> CliValueType.OfFields
            baseClassTypes
            allConcreteTypes
            runtimeMethodInfoStubHandle
            Layout.Default
            (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeMethodInfoStub.TypeAttributes)

    /// Mint a fresh no-metadata method — CoreCLR's `DynamicMethodTable::GetDynamicMethod`
    /// followed by `AllocateStubMethodInfo` — and return the address of the
    /// `RuntimeMethodInfoStub` naming it, which is what `ModuleHandle_GetDynamicMethod` writes
    /// through its `result` handle.
    ///
    /// Unconditionally fresh: there is no dedup step and none is wanted. Each call to the QCall
    /// produces a method distinct from every other one alive, so two dynamic methods that happen
    /// to agree on name, signature and scope must not collapse into one here. Correspondingly
    /// this touches neither `MethodHandleToId` nor `MethodToHandle`, both of which are dedup
    /// indices keyed on a structural identity that a dynamic method does not have.
    let mintDynamicMethod
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (name : string)
        (signature : ImmutableArray<byte>)
        (scopeAssemblyFullName : string)
        (resolver : ManagedHeapAddress option)
        (body : MethodInstructions<TypeDefn>)
        (reg : MethodHandleRegistry)
        : ManagedHeapAddress * MethodHandleRegistry * 'allocState
        =
        let registryId = reg.NextHandle

        let dynamicHandle =
            {
                RegistryId = registryId
            }
            : DynamicMethodHandle

        let definition =
            {
                Name = name
                Signature = signature
                ScopeAssemblyFullName = scopeAssemblyFullName
                Resolver = resolver
                Body = body
            }
            : DynamicMethodDefinition

        let reg =
            { reg with
                IdToMethodHandle =
                    reg.IdToMethodHandle
                    |> Map.add registryId (MethodHandle.FromDynamic dynamicHandle)
                DynamicMethods = reg.DynamicMethods |> Map.add dynamicHandle definition
                NextHandle = registryId + 1L
            }

        let stub =
            CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle registryId)
            |> buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes
            |> CliType.ValueType
            |> buildRuntimeMethodInfoStub baseClassTypes allConcreteTypes

        let address, allocState = allocate stub allocState

        address, reg, allocState

    /// What the given dynamic method was built from, or `None` if this registry never minted it.
    let resolveDynamicMethod
        (handle : DynamicMethodHandle)
        (reg : MethodHandleRegistry)
        : DynamicMethodDefinition option
        =
        Map.tryFind handle reg.DynamicMethods

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
                (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeMethodHandle.TypeAttributes)
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
            let mHandle =
                CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle registryId)

            buildRuntimeMethodHandleInternal baseClassTypes allConcreteTypes mHandle
            |> CliType.ValueType

        let runtimeMethodInfoStub =
            buildRuntimeMethodInfoStub baseClassTypes allConcreteTypes runtimeMethodHandleInternal

        let alloc, state = allocate runtimeMethodInfoStub allocState

        let reg =
            { reg with
                MethodToHandle = reg.MethodToHandle |> Map.add handle alloc
            }

        runtimeMethodHandle alloc, reg, state
