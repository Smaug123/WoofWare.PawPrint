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
            /// The declaring type as the handle was minted, which is the identity the guest
            /// sees: a closed instantiation gets `Closed`, and a generic type definition gets
            /// `OpenGenericTypeDefinition`, so `typeof(G&lt;int&gt;)`'s method and
            /// `typeof(G&lt;&gt;)`'s share a MethodDef row but never a registry id. CoreCLR keeps
            /// their `MethodDesc*` distinct for the same reason. Only those two arms can occur;
            /// `getOrAllocate*` refuses the rest.
            DeclaringType : RuntimeTypeHandleTarget
            MethodDefinition : ComparableMethodDefinitionHandle
            MethodGenerics : ConcreteTypeHandle list
        }

    member this.GetAssemblyFullName () : string = this.AssemblyFullName
    member this.GetDeclaringType () : RuntimeTypeHandleTarget = this.DeclaringType
    member this.GetMethodDefinitionHandle () : ComparableMethodDefinitionHandle = this.MethodDefinition
    member this.GetMethodGenerics () : ConcreteTypeHandle list = this.MethodGenerics

/// A dynamic method's body as it is known at the moment the method is minted: everything a
/// <see cref="MethodInstructions{T}"/> holds *except* <c>LocalsInit</c>.
///
/// The omission is the point. `DynamicMethod.InitLocals` has a public setter that never latches
/// (`DynamicMethod.cs`, `set => _initLocals = value;`), and CoreCLR reads it late — the managed
/// `DynamicResolver.GetCodeInfo` returns `m_method.InitLocals` at call time
/// (`DynamicILGenerator.cs:729`), and the native `LCGMethodResolver::GetCodeInfo`
/// (`vm/dynamicmethod.cpp`) calls that during the *first JIT* of the method and caches the answer
/// under `if (!m_Code)`. So the flag is not known when the method is minted, and a type
/// that carried a field for it here would have to put something untrue in it.
///
/// Everything else `GetCodeInfo` reports is frozen at resolver construction (bake time), so this
/// is the whole of what is not yet decided. `MethodHandleRegistry.latchPreparation` decides it, at
/// first execution, and `withLocalsInit` is the only way to get a `MethodInstructions` back out.
///
/// A `catch` clause's *type* is undecided at mint too, but it is not a gap in this type: the clause
/// carries a `DynamicScope` index, which is frozen at bake like everything else here, and only the
/// type that index names is settled later. That answer lives on `PreparedDynamicMethod` beside
/// `initLocals`, because it is a property of the method's preparation rather than of its body.
type MintedDynamicMethodBody =
    {
        Instructions : (IlOp * int) list
        Locations : Map<int, IlOp>
        LocalVars : ImmutableArray<TypeDefn> option
        ExceptionRegions : ImmutableArray<WoofWare.PawPrint.ExceptionRegion>
    }

[<RequireQualifiedAccess>]
module MintedDynamicMethodBody =
    /// Everything about a dynamic method's body that reading its `DynamicResolver` establishes.
    let make
        (instructions : (IlOp * int) list)
        (localVars : ImmutableArray<TypeDefn> option)
        (exceptionRegions : ImmutableArray<WoofWare.PawPrint.ExceptionRegion>)
        : MintedDynamicMethodBody
        =
        {
            Instructions = instructions
            Locations = instructions |> List.map (fun (a, b) -> b, a) |> Map.ofList
            LocalVars = localVars
            ExceptionRegions = exceptionRegions
        }

    /// Complete the body with the `initLocals` that was latched at first execution.
    let withLocalsInit (localsInit : bool) (body : MintedDynamicMethodBody) : MethodInstructions<TypeDefn> =
        {
            Instructions = body.Instructions
            Locations = body.Locations
            LocalsInit = localsInit
            LocalVars = body.LocalVars
            ExceptionRegions = body.ExceptionRegions
        }

/// Everything about a dynamic method that is settled when it is first prepared for execution,
/// rather than when it is minted: CoreCLR resolves all of this during the method's first JIT and
/// caches it, so a guest's later edits change nothing.
///
/// One record rather than two fields, so that the two cannot latch apart. Measured on real .NET: a
/// first invocation that *fails* to compile — an open-generic `catch` clause, say — latches
/// nothing, and a second invocation after the guest repairs the scope compiles and runs. So a
/// preparation that gets part-way must leave the method exactly as unprepared as it found it, and a
/// type that could hold "initLocals latched, clause types not" would let a caller do otherwise.
type PreparedDynamicMethod =
    {
        /// `DynamicMethod.InitLocals` as it read at first preparation. Its setter goes on working
        /// afterwards and is then ignored, exactly as CoreCLR's `if (!m_Code)` guard arranges.
        LocalsInit : bool
        /// The type each `catch` clause of this body names, keyed by the `DynamicScope` index the
        /// clause carries. Keyed on the scope index rather than on the clause's position so that
        /// two clauses naming one entry share one resolution, as CoreCLR's single `ResolveToken`
        /// per token does.
        CatchTypes : Map<int, ConcreteTypeHandle>
    }

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
/// is one of the places that has to grow a real weak reference -- and `Resolver` is read *late*
/// as well as at mint, by `latchPreparation`, so it is live for longer than it looks.
type DynamicMethodDefinition =
    private
        {
            Name : string
            Signature : ImmutableArray<byte>
            ScopeAssemblyFullName : string
            Resolver : ManagedHeapAddress option
            Body : MintedDynamicMethodBody
            /// What this method was compiled with, once something has executed it; `None` until
            /// then. See `MintedDynamicMethodBody` for why it cannot be known earlier, and
            /// `latchPreparation` for why it never changes once set.
            Prepared : PreparedDynamicMethod option
        }

    member this.GetName () : string = this.Name
    member this.GetSignature () : ImmutableArray<byte> = this.Signature
    member this.GetScopeAssemblyFullName () : string = this.ScopeAssemblyFullName
    member this.GetResolver () : ManagedHeapAddress option = this.Resolver

    /// The IL this method will execute, read out of `Resolver` at the moment the method was
    /// minted (see `DynamicMethodBody`).
    ///
    /// Recorded eagerly and not as an option, so that a minted dynamic method is never in a
    /// half-built state where it has an identity but no body. A dynamic method carrying a token for
    /// *itself* does not disturb this: the cycle is broken by *when* a scope entry is read
    /// rather than by minting the identity before the body. Decoding classifies the self-entry by its type alone and reads
    /// nothing out of it, and the read that needs `_methodHandle` happens when the `call` executes —
    /// by which point the executing method has necessarily been minted. So there is no moment at
    /// which a body must exist before its own identity does.
    ///
    /// This is *not* the whole body: `initLocals` is not known at mint. See
    /// <see cref="MintedDynamicMethodBody"/>.
    member this.GetBody () : MintedDynamicMethodBody = this.Body

    /// What was latched at first execution, or `None` if nothing has executed this method yet.
    /// Exposed so that a test can tell "not yet decided" from "decided as false"; the interpreter
    /// goes through `latchPreparation` instead.
    member this.GetPreparation () : PreparedDynamicMethod option = this.Prepared

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
            /// existing address rather than minting a fresh stub. This dedup is
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
            AssemblyFullName = method.DeclaringAssembly.FullName
            MethodDefinition = ComparableMethodDefinitionHandle.Make (requireDeclaredMethod method)
            DeclaringType =
                // `requireDeclaredMethod` has already refused anything without a MethodDef row,
                // and only a dynamic method lacks a declaring type, so this cannot fire.
                let declaringType =
                    MethodOwner.requireDeclaringType "minting a RuntimeMethodHandle" method.Owner

                AllConcreteTypes.findExistingConcreteType allConcreteTypes declaringType.Identity declaringType.Generics
                |> Option.defaultWith (fun () ->
                    failwith $"declaring type for method %O{method} was not found in ConcreteTypes"
                )
                |> RuntimeTypeHandleTarget.Closed
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
            (DeclaredTypeFacts.ofCorelibType baseClassTypes baseClassTypes.RuntimeMethodHandleInternal)

    /// Refuse the declaring-type shapes that cannot own a metadata-backed method, so that
    /// consumers matching on `MetadataMethodIdentity.GetDeclaringType ()` may treat those arms as
    /// contract violations rather than as cases to serve. Mirrors
    /// `FieldHandleRegistry.getOrAllocate`.
    let private requireMethodBearingDeclaringType
        (operation : string)
        (declaringType : RuntimeTypeHandleTarget)
        : unit
        =
        match declaringType with
        | RuntimeTypeHandleTarget.Closed _
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> ()
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            // A method on this class is a Reflection.Emit method, whose identity is
            // `MethodHandle.FromDynamic`; minting it as metadata-backed would give it a MethodDef
            // row it does not have.
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at MethodHandleRegistry.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // A generic parameter is a TypeVarTypeDesc: methods live on the type that mentions
            // the parameter, never on the parameter itself.
            failwith $"%s{operation}: declaring type must be Closed or OpenGenericTypeDefinition, got %O{declaringType}"

    /// Construct the `MethodHandle` identifying `method` as declared by `declaringType`, with no
    /// method-generic arguments bound.
    ///
    /// "Open" here is about the *method's* generics, not the declaring type's: the BCL's
    /// enumerator surfaces method-table slots, i.e. method definitions, and a generic-method
    /// definition cannot be expressed with empty `MethodGenerics` through `concretizeMethod`.
    /// The declaring type may independently be closed or a generic type definition.
    let private makeOpenMethodHandle
        (operation : string)
        (assemblyFullName : string)
        (declaringType : RuntimeTypeHandleTarget)
        (method : MethodInfo<'tyGen, GenericParamFromMetadata, TypeDefn>)
        : MethodHandle
        =
        requireMethodBearingDeclaringType operation declaringType

        {
            AssemblyFullName = assemblyFullName
            DeclaringType = declaringType
            MethodDefinition = ComparableMethodDefinitionHandle.Make (requireDeclaredMethod method)
            MethodGenerics = []
        }
        |> MethodHandle.FromMetadata

    /// Returns a bare `System.RuntimeMethodHandleInternal` value type identifying the given method
    /// declared on `declaringType`, allocating a fresh registry id if necessary. No managed-heap
    /// allocation is performed; this is the representation `RuntimeTypeHandle.GetFirstIntroducedMethod`
    /// / `GetNextIntroducedMethod` surface, being raw method-table slots rather than full handles,
    /// and the one `RuntimeMethodHandle_GetStubIfNeededSlow` rebinds onto when the declaring type is
    /// a generic type *definition*. Method-generic parameters of the input `method` are intentionally
    /// NOT instantiated, so the registered handle has empty `MethodGenerics`: that is the method
    /// definition, analogous to a CoreCLR open `MethodDesc*`, which is what both callers want.
    let getOrAllocateInternalHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (assemblyFullName : string)
        (declaringType : RuntimeTypeHandleTarget)
        (method : MethodInfo<'tyGen, GenericParamFromMetadata, TypeDefn>)
        (reg : MethodHandleRegistry)
        : CliValueType * MethodHandleRegistry
        =
        let handle =
            makeOpenMethodHandle
                "MethodHandleRegistry.getOrAllocateInternalHandle"
                assemblyFullName
                declaringType
                method

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
            (DeclaredTypeFacts.ofCorelibType baseClassTypes baseClassTypes.RuntimeMethodInfoStub)

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
        (body : MintedDynamicMethodBody)
        (reg : MethodHandleRegistry)
        : ManagedHeapAddress * MethodHandleRegistry * 'allocState
        =
        let registryId = reg.NextHandle

        let dynamicHandle = DynamicMethodHandle.ofRegistryId registryId

        let definition =
            {
                Name = name
                Signature = signature
                ScopeAssemblyFullName = scopeAssemblyFullName
                Resolver = resolver
                Body = body
                // Not read here, deliberately: see `MintedDynamicMethodBody`.
                Prepared = None
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

    /// <summary>
    /// Fix what this dynamic method is compiled with at <paramref name="observed" /> if nothing has
    /// fixed it already, and return whatever is now in effect.
    /// </summary>
    /// <remarks>
    /// <para>
    /// First write wins, and that is the semantics rather than mere tolerance of a double call.
    /// CoreCLR's <c>LCGMethodResolver::GetCodeInfo</c> computes <c>m_Options</c> only under
    /// <c>if (!m_Code)</c>, so what the first JIT saw is what the method is compiled with forever;
    /// a guest that assigns <c>InitLocals</c> afterwards -- which the property's setter cheerfully
    /// permits, at any time -- changes nothing, and neither does one that rewrites a scope slot a
    /// <c>catch</c> clause named. Re-reading on each invocation would be wrong in exactly the
    /// opposite direction from reading at mint.
    /// </para>
    /// <para>
    /// Returning what is in effect, rather than leaving the caller to read it back, is what stops a
    /// caller from compiling a frame with a value the registry did not latch.
    /// </para>
    /// </remarks>
    let latchPreparation
        (handle : DynamicMethodHandle)
        (observed : PreparedDynamicMethod)
        (reg : MethodHandleRegistry)
        : PreparedDynamicMethod * MethodHandleRegistry
        =
        match Map.tryFind handle reg.DynamicMethods with
        | None -> failwith $"cannot latch the preparation of %O{handle}: this registry never minted it"
        | Some definition ->
            match definition.Prepared with
            | Some alreadyLatched -> alreadyLatched, reg
            | None ->
                let definition =
                    { definition with
                        Prepared = Some observed
                    }

                observed,
                { reg with
                    DynamicMethods = reg.DynamicMethods |> Map.add handle definition
                }

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
                (DeclaredTypeFacts.ofCorelibType baseClassTypes baseClassTypes.RuntimeMethodHandle)
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
