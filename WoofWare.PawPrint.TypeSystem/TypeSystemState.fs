namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// What `TypeSystemState` memoises a MemberRef resolution under: the row, and the generic
/// context the frame reading it was executing in. Two frames of the same method body with
/// different instantiations read the same row to different members, so the context is part of
/// the key.
type MemberResolutionKey =
    {
        /// Definition identity of the assembly whose MemberRef table holds the row.
        Assembly : string
        /// The MemberRef row number.
        MemberRow : int
        /// The executing method's declaring type's generic arguments.
        DeclaringTypeGenerics : ConcreteTypeHandle list
        /// The executing method's own generic arguments.
        MethodGenerics : ConcreteTypeHandle list
    }

/// What a MemberRef row resolves to: `IlMachineMemberResolution.resolveMember`'s answer, kept
/// so the row need not be resolved again on the next instruction that names it.
type ResolvedMemberReference =
    {
        /// The assembly the member is declared in.
        DeclaringAssembly : AssemblyName
        Member :
            Choice<
                WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
                WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
             >
        /// The generic arguments of the type the row's parent named, as the row spells them.
        TargetTypeGenerics : ImmutableArray<TypeDefn>
    }

/// What `TypeSystemState` memoises a method concretisation under: the definition, and the
/// concrete generic arguments it is instantiated at.
type ConcreteMethodKey =
    {
        /// Identity of the definition that declares the method.
        DeclaringType : ResolvedTypeIdentity
        /// Which method of that definition: its MethodDef row and its synthesised kind, one of
        /// which is set, exactly as `MethodInfo.IdentityKey` pairs them.
        MethodRow : int option
        Synthesised : SynthesisedMethod option
        /// The declaring type's generic arguments.
        TypeGenerics : ConcreteTypeHandle list
        /// The method's own generic arguments.
        MethodGenerics : ConcreteTypeHandle list
    }

/// `ExecutionConcretization.concretizeMethodWithAllGenerics`'s answer for a `ConcreteMethodKey`.
type ConcretisedMethod =
    {
        Method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        DeclaringTypeHandle : ConcreteTypeHandle
    }

/// The type system over one set of loaded assemblies: the assemblies, the concrete types
/// instantiated over them so far, and the memos of the walks that read them. Immutable; every
/// operation returns the state to carry on with.
type TypeSystemState =
    {
        /// The assemblies we have loaded, keyed by their own AssemblyDefinition identity, plus the
        /// record of which AssemblyReferences have been bound to which of them. An assembly's
        /// reference identity routinely differs from its definition identity (the .NET Framework
        /// compatibility facades reference implementation assemblies as `Version=0.0.0.0`), so the
        /// two must not be conflated; see `LoadedAssemblies`.
        _LoadedAssemblies : LoadedAssemblies
        /// Memo of `ConcreteMethodTable.dispatchTableOfClosed`, keyed on the *definition* whose method
        /// table it is, because every instantiation of a definition shares one table.
        ///
        /// A memo rather than state: the walk is a pure function of metadata that never changes once
        /// loaded, so a hit and a miss agree by construction. The walk does mutate `TypeSystemState` on
        /// the way -- registering concrete types, binding assembly references -- but those are
        /// idempotent and persist from the miss that performed them, so skipping them on a hit changes
        /// nothing. PawPrint supports neither assembly unloading nor EnC, so nothing invalidates an
        /// entry.
        ///
        /// It exists because dispatch reads it per `callvirt`. Measured on the dispatch-saturated
        /// benchmark guest: rebuilding every time cost 255.2ms and 735.2MB where the signature-matching
        /// walk this replaced cost 184.7ms and 552.7MB, and memoising recovers nearly all of it. Run
        /// back to back against that walk, memoised is 188.5ms (StdDev 4.7, median 185.6) and 565.6MB
        /// against 180.1ms (StdDev 3.1) and 552.7MB -- +4.7% and +2.3%, the time within noise.
        ///
        /// That the memo tells the truth is checked by forcing every lookup to miss: the suite's 4019
        /// tests then recompute each table and agree exactly. Worth checking rather than arguing,
        /// because a wrong entry is invisible -- nothing downstream has a second opinion to disagree
        /// with.
        ///
        /// Add through `WithVirtualSlotTable`, never by assignment: an entry that disagreed with the
        /// walk would be undetectable, every later read taking the memo's word for it.
        _VirtualSlotTables : Map<ResolvedTypeIdentity, DispatchTable>
        /// Memo of `ConcreteInterfaceDispatch.ownDispatchMapOf`: the interface dispatch entries each type
        /// contributes, keyed on the type's *instantiation*, because which instantiation of an
        /// interface an entry names depends on it.
        ///
        /// A memo and not state, for the reason `_VirtualSlotTables` gives: the walk is a pure function
        /// of metadata, and the registrations it performs on the way are idempotent. It exists because
        /// interface dispatch consults it once per `callvirt` for every type on the receiver's chain.
        ///
        /// Add through `WithInterfaceDispatchMap`, never by assignment.
        _InterfaceDispatchMaps : Map<ConcreteTypeHandle, InterfaceDispatchMap>
        /// Memo of `IlMachineMemberResolution.resolveMember`, keyed on the row and the generic
        /// context it is read in.
        ///
        /// Every `ldfld`, `stfld`, `call` and `callvirt` whose token is a MemberRef resolves it
        /// afresh otherwise, and resolving means rebuilding the parent type's `TypeInfo` under
        /// the row's instantiation and comparing signatures against each same-named member.
        /// Measured on a `NonBacktracking` regex construction: 675,675 resolutions over 1,141
        /// distinct keys, 15% of everything the run allocated.
        ///
        /// A hit and a miss agree for the reason `_VirtualSlotTables` gives: the answer is a
        /// function of metadata and of which assemblies are loaded, and a miss's side effects on
        /// the state (assemblies bound, concrete types registered) are idempotent and persist,
        /// so a hit that skips them changes nothing. Nothing invalidates an entry: no unloading,
        /// no EnC.
        ///
        /// Add through `WithMemberResolution`, never by assignment.
        _MemberResolutions : Map<MemberResolutionKey, ResolvedMemberReference>
        /// Memo of `ExecutionConcretization.concretizeMethodWithAllGenerics`, keyed on the
        /// definition and the concrete generic arguments.
        ///
        /// Every call concretises its callee otherwise: concretising the declaring type, the
        /// signature, the locals and the method generics against the instantiation. Measured on
        /// a `NonBacktracking` regex construction: 1,021,532 frames over 1,268 distinct concrete
        /// methods, with the concretisation 7% of everything the run allocated.
        ///
        /// A hit and a miss agree for the reason `_VirtualSlotTables` gives: the answer is a
        /// function of the definition's metadata and the handles, a miss's side effects on the
        /// state (assemblies loaded, concrete types registered) are idempotent and persist, so a
        /// hit that skips them changes nothing, and nothing invalidates an entry.
        ///
        /// Add through `WithConcretisedMethod`, never by assignment.
        _ConcretisedMethods : Map<ConcreteMethodKey, ConcretisedMethod>
        /// Every concrete type instantiated so far, each identified by its handle.
        ConcreteTypes : AllConcreteTypes
    }

    /// No assemblies loaded, no concrete types, nothing memoised.
    static member Empty : TypeSystemState =
        {
            _LoadedAssemblies = LoadedAssemblies.empty
            _VirtualSlotTables = Map.empty
            _InterfaceDispatchMaps = Map.empty
            _MemberResolutions = Map.empty
            _ConcretisedMethods = Map.empty
            ConcreteTypes = AllConcreteTypes.Empty
        }

    member this.WithVirtualSlotTable (definition : ResolvedTypeIdentity) (table : DispatchTable) =
        { this with
            _VirtualSlotTables = this._VirtualSlotTables |> Map.add definition table
        }

    member this.WithInterfaceDispatchMap (owner : ConcreteTypeHandle) (map : InterfaceDispatchMap) =
        { this with
            _InterfaceDispatchMaps = this._InterfaceDispatchMaps |> Map.add owner map
        }

    member this.WithMemberResolution (key : MemberResolutionKey) (resolved : ResolvedMemberReference) =
        { this with
            _MemberResolutions = this._MemberResolutions |> Map.add key resolved
        }

    member this.WithConcretisedMethod (key : ConcreteMethodKey) (concretised : ConcretisedMethod) =
        { this with
            _ConcretisedMethods = this._ConcretisedMethods |> Map.add key concretised
        }

    /// Register an assembly under its own definition identity. Idempotent: if an assembly with
    /// that identity is already loaded, the existing instance is kept.
    member this.WithLoadedAssembly (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.WithLoadedAssembly value
        }

    /// Register a dynamic assembly; see `LoadedAssemblies.WithDynamicAssembly`, whose error this
    /// passes on.
    member this.WithDynamicAssembly (value : DumpedAssembly) : Result<TypeSystemState, DumpedAssembly> =
        this._LoadedAssemblies.WithDynamicAssembly value
        |> Result.map (fun loaded ->
            { this with
                _LoadedAssemblies = loaded
            }
        )

    /// The loaded assembly with this definition identity, if it is loaded.
    member this.LoadedAssembly (definitionFullName : string) : DumpedAssembly option =
        this._LoadedAssemblies.TryByDefinitionName definitionFullName

/// The type system's questions over a `TypeSystemState`: instantiating types and signatures, comparing
/// signatures and instantiations as CoreCLR does, resolving type tokens and base types, and reading a
/// concrete type's row. Whatever loads an assembly or registers a concrete type on the way returns the
/// state it leaves behind; `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module TypeSystemState =
    let concretizeType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (declaringAssemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (ty : TypeDefn)
        : TypeSystemState * ConcreteTypeHandle
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let handle, ctx =
            TypeConcretization.concretizeType
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                declaringAssemblyFullName
                typeGenerics
                methodGenerics
                ty

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, handle

    /// Concretise a decoded method signature: its parameter types, and its return column.
    ///
    /// This is the only way to turn a `TypeMethodSignature&lt;TypeDefn&gt;` into a
    /// `TypeMethodSignature&lt;ConcreteTypeHandle&gt;`, and going through it is what makes two such
    /// signatures comparable — several callers concretise one signature here and compare it against
    /// another that arrived via `Concretization.concretizeMethod`, so a caller that mapped the types
    /// itself could disagree with them about the return shape of a method whose return carries a
    /// custom modifier.
    let concretizeMethodSignature
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (declaringAssemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (signature : TypeMethodSignature<TypeDefn>)
        : TypeSystemState * TypeMethodSignature<ConcreteTypeHandle>
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let signature, ctx =
            TypeConcretization.concretizeMethodSignature
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                declaringAssemblyFullName
                typeGenerics
                methodGenerics
                signature

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, signature

    /// Concretise a method's return column alone. Use this rather than folding a
    /// `MethodReturnType&lt;TypeDefn&gt;` by hand: a `void` under custom modifiers returns no value, and
    /// two consumers that decide that separately can disagree.
    let concretizeReturnColumn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (declaringAssemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (returnType : MethodReturnType<TypeDefn>)
        : TypeSystemState * MethodReturnType<ConcreteTypeHandle>
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let ctx, returnType =
            TypeConcretization.concretizeReturnColumn
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                declaringAssemblyFullName
                typeGenerics
                methodGenerics
                returnType

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, returnType

    /// Do the constraints on a generic method's type parameters permit `impl` to override `decl`?
    /// CoreCLR asks this only once the signatures already match, and a mismatch means the type does
    /// not load at all rather than that the method gets a slot of its own.
    let methodConstraintsMatch
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (impl : TypeConcretization.ConstraintComparand)
        (decl : TypeConcretization.ConstraintComparand)
        : TypeSystemState * bool
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let matches, ctx =
            TypeConcretization.methodConstraintsMatch
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                impl
                decl

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, matches

    /// Do these two method signatures name the same signature, in the sense of CoreCLR's
    /// `MetaSig::CompareMethodSigs`? Use this, and not equality of two concretised signatures, for
    /// any question CoreCLR answers off the signature blob — which virtual slot a method fills, and
    /// which MethodDef a MemberRef names. Concretisation deliberately looks through custom modifiers
    /// and normalises away the choice of encoding, both of which are part of the signature to those
    /// questions.
    ///
    /// `skipReturnType` omits the return column, which is how CoreCLR expresses "a covariant return
    /// is acceptable"; the caller then applies its own rule to the return types.
    ///
    /// `caller` is the side whose vararg sentinel bounds the comparison, where the two differ in
    /// parameter count.
    let signaturesEquivalent
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (skipReturnType : bool)
        (caller : TypeConcretization.SignatureComparand)
        (callee : TypeConcretization.SignatureComparand)
        : TypeSystemState * bool
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let equivalent, ctx =
            TypeConcretization.signaturesEquivalent
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                skipReturnType
                caller
                callee

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, equivalent

    /// `TypeConcretization.signaturesEquivalentWithoutSubstitution`: the comparison `[UnsafeAccessor]`
    /// matching makes, in which a type variable on either side is compared by its index alone.
    let signaturesEquivalentWithoutSubstitution
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (skipReturnType : bool)
        (caller : TypeConcretization.UnsubstitutedComparand)
        (callee : TypeConcretization.UnsubstitutedComparand)
        : TypeSystemState * bool
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let equivalent, ctx =
            TypeConcretization.signaturesEquivalentWithoutSubstitution
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                skipReturnType
                caller
                callee

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, equivalent

    /// Do these two instantiations of one generic definition name the same type, in the sense of
    /// CoreCLR's `MetaSig::CompareTypeDefsUnderSubstitutions`? See
    /// `TypeConcretization.substitutionsEquivalent`.
    let substitutionsEquivalent
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (left : TypeConcretization.SubstitutionContext)
        (right : TypeConcretization.SubstitutionContext)
        : TypeSystemState * bool
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let equivalent, ctx =
            TypeConcretization.substitutionsEquivalent
                ctx
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                left
                right

        let state =
            { state with
                _LoadedAssemblies = ctx.LoadedAssemblies
                ConcreteTypes = ctx.ConcreteTypes
            }

        state, equivalent

    let resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (state : TypeSystemState)
        : TypeSystemState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let assemblies, resolvedAssy, typeInfo =
            TypeResolution.resolveTypeFromRef
                loggerFactory
                dotnetRuntimeDirs
                referencedInAssembly
                target
                typeGenericArgs
                state._LoadedAssemblies

        { state with
            _LoadedAssemblies = assemblies
        },
        resolvedAssy,
        typeInfo

    let lookupTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (activeAssy : DumpedAssembly)
        (typeDef : TypeDefinitionHandle)
        : TypeSystemState * TypeDefn
        =
        let defn = activeAssy.TypeDefs.[typeDef]
        state, LoadedTypeInfo.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies defn

    /// Resolve a `TypeReference` token to the type it names.
    ///
    /// No generic context is taken, and none may be: a `TypeReference` row names a type and carries
    /// no type arguments, so there is nothing for a caller to instantiate it with. `resolveTypeRef`
    /// substitutes whatever it is handed into the *referenced type's own* formal parameters,
    /// positionally (`Assembly.applyGenericArgs`), so passing the executing frame's generics binds
    /// them into an unrelated type's slots whenever the arities happen to line up: `ldtoken List`1`
    /// from a frame on `Holder<string>` came back as `List<string>`.
    ///
    /// A caller that does have arguments for the type is looking at a `TypeSpecification`, whose
    /// signature spells them out and which resolves by a different route. Callers that need the
    /// frame's context apply it downstream, when concretizing the `TypeDefn` this returns.
    let lookupTypeRef
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (activeAssy : DumpedAssembly)
        (ref : TypeReferenceHandle)
        : TypeSystemState * TypeDefn * DumpedAssembly
        =
        let ref = activeAssy.TypeRefs.[ref]

        let state, assy, resolved =
            resolveTypeFromRef loggerFactory dotnetRuntimeDirs activeAssy ref ImmutableArray.Empty state

        state, LoadedTypeInfo.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved, assy

    /// Resolve a MetadataToken (TypeDefinition, TypeReference, or TypeSpecification) to a TypeDefn,
    /// together with the assembly the type was resolved in.
    ///
    /// Takes no generic context, for the reason `lookupTypeRef` gives: none of the three token
    /// kinds carries one. A `TypeSpecification`'s signature is returned verbatim, `!0` and all,
    /// for the caller to concretize against whatever context it means.
    let resolveTypeMetadataToken
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (activeAssy : DumpedAssembly)
        (token : MetadataToken)
        : TypeSystemState * TypeDefn * DumpedAssembly
        =
        match token with
        | MetadataToken.TypeDefinition h ->
            let state, ty = lookupTypeDefn baseClassTypes state activeAssy h
            state, ty, activeAssy
        | MetadataToken.TypeReference ref ->
            lookupTypeRef loggerFactory dotnetRuntimeDirs baseClassTypes state activeAssy ref
        | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
        | m -> failwith $"unexpected type metadata token {m}"

    /// Resolve a BaseTypeInfo to the assembly and TypeDefn of the base type.
    let resolveBaseTypeInfo
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (currentAssembly : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo)
        : TypeSystemState * DumpedAssembly * TypeDefn
        =
        match baseTypeInfo with
        | BaseTypeInfo.TypeDef handle ->
            let typeInfo = currentAssembly.TypeDefs.[handle]

            let typeDefn =
                LoadedTypeInfo.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, currentAssembly, typeDefn
        | BaseTypeInfo.TypeRef handle ->
            let state, assy, resolved =
                resolveTypeFromRef
                    loggerFactory
                    dotnetRuntimeDirs
                    currentAssembly
                    (currentAssembly.TypeRefs.[handle])
                    ImmutableArray.Empty
                    state

            let typeDefn =
                LoadedTypeInfo.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved

            state, assy, typeDefn
        | BaseTypeInfo.TypeSpec handle ->
            let signature = currentAssembly.TypeSpecs.[handle].Signature
            state, currentAssembly, signature

    /// Given a ConcreteTypeHandle, resolve and return its base type as a ConcreteTypeHandle.
    /// Returns None for types without a base type (System.Object).
    let resolveBaseConcreteType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (concreteType : ConcreteTypeHandle)
        : TypeSystemState * ConcreteTypeHandle option
        =
        match concreteType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Structural array handles keep their own runtime identity; their base type is System.Array.
            let state, arrayHandle =
                LoadedTypeInfo.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Array
                |> concretizeType
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            state, Some arrayHandle
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: resolveBaseConcreteType: function pointer types (%O{concreteType}) not yet supported; the runtime base type is System.ValueType but the lookup path needs adjusting"
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ ->

            match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | Some ct ->
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName

                let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

                match typeInfo.BaseType with
                | None -> state, None
                | Some baseTypeInfo ->
                    let state, baseAssy, baseTypeDefn =
                        resolveBaseTypeInfo loggerFactory dotnetRuntimeDirs baseClassTypes state assy baseTypeInfo

                    let state, baseHandle =
                        concretizeType
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            baseAssy.DefinitionFullName
                            ct.Generics
                            ImmutableArray.Empty
                            baseTypeDefn

                    state, Some baseHandle

    /// Get the metadata row directly represented by this concrete handle.
    /// Structural arrays, byrefs, and pointers have no direct TypeDef row; callers that are walking
    /// inheritance should ask for their base type explicitly.
    let tryGetConcreteTypeInfo
        (state : TypeSystemState)
        (concreteType : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>) option
        =
        // Deliberately not just `AllConcreteTypes.tryTypeInfo`: this distinguishes the two
        // reasons that returns `None`. A structural handle is an ordinary answer of "no nominal
        // type here", but a `Concrete` handle with no row is a broken invariant and is raised.
        match concreteType with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes concreteType with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | resolved -> resolved
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None

    /// Does this handle denote a reference type (as opposed to a value type)?
    ///
    /// The structural handles answer without any metadata: arrays of every rank are reference
    /// types, while byrefs, pointers and function pointers are not (they are neither, strictly,
    /// but every caller asks this question to decide whether reference-type rules — covariance,
    /// array-store checks, atomic reference exchange — apply, and for those the answer is "no").
    /// Nominal handles defer to the TypeDef row.
    ///
    /// `context` names the caller in the diagnostic raised when a nominal handle has no TypeDef
    /// row, which would be a bug in whatever produced the handle.
    let isReferenceTypeHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (context : string)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false
        | ConcreteTypeHandle.Concrete _ ->
            match tryGetConcreteTypeInfo state handle with
            | Some (_, typeInfo) -> LoadedTypeInfo.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
            | None -> failwith $"%s{context}: concrete type handle %O{handle} has no TypeDef row"

    /// Returns true if `handle` is a CLR enum value type — a nominal type whose immediate runtime
    /// base is `System.Enum`.
    let isEnumValueType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> state, false
        | ConcreteTypeHandle.Concrete _ ->
            let state, baseHandle =
                resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state handle

            match baseHandle with
            | None -> state, false
            | Some bh ->
                match AllConcreteTypes.lookup bh state.ConcreteTypes with
                | Some baseTy -> state, baseTy.Identity = baseClassTypes.Enum.Identity
                | None -> state, false

    /// For an enum `ConcreteTypeHandle`, return the `ConcreteTypeHandle` of its underlying integer
    /// type by concretising the signature of its sole instance field (`value__`, the CLR-reserved
    /// name for the integer slot of an enum; ECMA-335 §II.14.3). Returns `None` if `handle` is not
    /// an enum, has no TypeDef row, or — defensively — has a malformed Fields list. The caller is
    /// expected to have first verified enum-ness via `isEnumValueType`; this helper does the
    /// metadata read.
    let enumUnderlyingHandle
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : (TypeSystemState * ConcreteTypeHandle) option
        =
        match tryGetConcreteTypeInfo state handle with
        | None -> None
        | Some (ct, typeInfo) ->
            let instanceFields =
                typeInfo.Fields
                |> List.filter (fun f -> not (f.Attributes.HasFlag FieldAttributes.Static))

            match instanceFields with
            | [ valueField ] when valueField.Name = "value__" ->
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName

                let state, underlying =
                    concretizeType
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        assy.DefinitionFullName
                        ct.Generics
                        ImmutableArray.Empty
                        valueField.Signature

                Some (state, underlying)
            | _ -> None
