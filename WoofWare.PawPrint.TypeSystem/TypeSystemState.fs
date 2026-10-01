namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

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
        /// Memo of `VirtualSlotLayout.dispatchTableOfClosed`, keyed on the *definition* whose method
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
        /// Memo of `InterfaceDispatch.ownDispatchMapOf`: the interface dispatch entries each type
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
