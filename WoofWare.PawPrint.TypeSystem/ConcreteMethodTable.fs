namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// The method table of a concrete type, over a `TypeSystemState`: a nominal type's is its definition's, as
/// `MethodTableLayout` lays it out, an array's is `System.Array`'s, and a byref, pointer or function
/// pointer has none. Also `MethodTableLayout`'s walks over a definition, run against a `TypeSystemState`
/// rather than a bare load context, so that the assemblies they bind and the types they register are kept.
[<RequireQualifiedAccess>]
module ConcreteMethodTable =

    /// Run one of `MethodTableLayout`'s walks against a type system's load context, keeping the
    /// assemblies it bound and the concrete types it registered.
    let private inContext
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (walk :
            TypeConcretization.ConcretizationContext<DumpedAssembly>
                -> TypeConcretization.ConcretizationContext<DumpedAssembly> * 'a)
        : TypeSystemState * 'a
        =
        let ctx, result =
            walk
                {
                    TypeConcretization.ConcretizationContext.ConcreteTypes = state.ConcreteTypes
                    TypeConcretization.ConcretizationContext.LoadedAssemblies = state._LoadedAssemblies
                    TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
                }

        { state with
            _LoadedAssemblies = ctx.LoadedAssemblies
            ConcreteTypes = ctx.ConcreteTypes
        },
        result

    /// `MethodTableLayout.definitionMetadata` against a type system's loaded assemblies.
    let internal definitionMetadata
        (operation : string)
        (state : TypeSystemState)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        MethodTableLayout.definitionMetadata operation state._LoadedAssemblies identity

    /// `MethodTableLayout.ownerOfDefinition` against a type system's loaded assemblies.
    let ownerOfDefinition (operation : string) (state : TypeSystemState) (identity : ResolvedTypeIdentity) : SlotOwner =
        MethodTableLayout.ownerOfDefinition operation state._LoadedAssemblies identity

    /// `MethodTableLayout.nominalIdentityOfSpelling` against a type system's load context.
    let internal nominalIdentityOfSpelling
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (assembly : DumpedAssembly)
        (ty : TypeDefn)
        : TypeSystemState * ResolvedTypeIdentity
        =
        inContext
            baseClassTypes
            state
            (fun ctx ->
                MethodTableLayout.nominalIdentityOfSpelling loggerFactory dotnetRuntimeDirs operation ctx assembly ty
            )

    /// `MethodTableLayout.baseOfDefinition` against a type system's load context.
    let internal baseOfDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (owner : SlotOwner)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * (ResolvedTypeIdentity * ImmutableArray<TypeConcretization.SubstitutionArgument>) option
        =
        inContext
            baseClassTypes
            state
            (fun ctx -> MethodTableLayout.baseOfDefinition loggerFactory dotnetRuntimeDirs operation ctx owner typeInfo)

    /// `MethodTableLayout.vtableOfDefinition` against a type system's load context.
    let vtableOfDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (identity : ResolvedTypeIdentity)
        : TypeSystemState * VtableSlot list
        =
        inContext
            baseClassTypes
            state
            (fun ctx -> MethodTableLayout.vtableOfDefinition loggerFactory dotnetRuntimeDirs operation ctx identity)

    /// `MethodTableLayout.contentVtableOfDefinition` against a type system's load context.
    let contentVtableOfDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (identity : ResolvedTypeIdentity)
        : TypeSystemState * VtableSlot list
        =
        inContext
            baseClassTypes
            state
            (fun ctx ->
                MethodTableLayout.contentVtableOfDefinition loggerFactory dotnetRuntimeDirs operation ctx identity
            )

    /// `MethodTableLayout.placedSlotsOfDefinition` against a type system's load context.
    let placedSlotsOfDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (identity : ResolvedTypeIdentity)
        : TypeSystemState * (VtableSlot * int) list
        =
        inContext
            baseClassTypes
            state
            (fun ctx ->
                MethodTableLayout.placedSlotsOfDefinition loggerFactory dotnetRuntimeDirs operation ctx identity
            )

    /// `MethodTableLayout.slotTableOfDefinition` against a type system's load context.
    let slotTableOfDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (identity : ResolvedTypeIdentity)
        : TypeSystemState * MethodTableLayout.MethodSlotTable
        =
        inContext
            baseClassTypes
            state
            (fun ctx -> MethodTableLayout.slotTableOfDefinition loggerFactory dotnetRuntimeDirs operation ctx identity)

    /// `MethodTableLayout.numVirtualsOfDefinition` against a type system's load context.
    let numVirtualsOfDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (identity : ResolvedTypeIdentity)
        : TypeSystemState * int
        =
        inContext
            baseClassTypes
            state
            (fun ctx ->
                MethodTableLayout.numVirtualsOfDefinition loggerFactory dotnetRuntimeDirs operation ctx identity
            )

    /// Both halves of what answering a `callvirt` on a receiver of this runtime type needs, from one
    /// walk: which slot each declaration in the receiver's chain owns, and what each slot of the
    /// receiver holds.
    ///
    /// Dispatch needs the slot of a declaration on some *ancestor* and the content of that slot on the
    /// *receiver*, and it is tempting to ask two questions of two types. That doubles the work for
    /// nothing: slot numbers are prefix-stable -- `CopyParentVtable` copies the parent's slots at the
    /// same indices -- so the receiver's own placement list already names every ancestor's declaration
    /// at the very index the ancestor gave it. Measured on the dispatch-saturated benchmark guest:
    /// asking separately cost 275.4ms and asking once costs 255.2ms, against 184.7ms for the
    /// signature-matching walk this replaced.
    ///
    /// `None` means the handle has no method table to read: byrefs, pointers and function pointers are
    /// TypeDescs. A synthesised array delegates to `System.Array`, whose slots are the ones it has.
    let rec dispatchTableOfClosed
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (concreteType : ConcreteTypeHandle)
        : TypeSystemState * DispatchTable option
        =
        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // TypeDescs with no method table: no slots, so nothing to dispatch through.
            state, None
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // A synthesised array's virtual slots are `System.Array`'s, as in `vtableOfClosed`.
            let state, baseHandle =
                TypeSystemState.resolveBaseConcreteType
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    concreteType

            match baseHandle with
            | None -> state, None
            | Some baseHandle ->
                dispatchTableOfClosed loggerFactory dotnetRuntimeDirs baseClassTypes operation state baseHandle
        | ConcreteTypeHandle.Concrete _ ->
            let concreteTypeInfo, _ =
                TypeSystemState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            // Keyed on the definition, which every instantiation of it shares -- the same reason the
            // walk itself is defined on the definition. See `TypeSystemState._VirtualSlotTables` for why a memo is
            // sound here and why it has to live on the state rather than beside this walk.
            match Map.tryFind concreteTypeInfo.Identity state._VirtualSlotTables with
            | Some cached -> state, Some cached
            | None ->

            let state, (table, content) =
                inContext
                    baseClassTypes
                    state
                    (fun ctx ->
                        let ctx, table =
                            MethodTableLayout.ownerOfDefinition
                                operation
                                ctx.LoadedAssemblies
                                concreteTypeInfo.Identity
                            |> MethodTableLayout.placeVirtualMethodsOfDefinitionOwner
                                loggerFactory
                                dotnetRuntimeDirs
                                operation
                                ctx

                        let ctx, content =
                            MethodTableLayout.contentOfDefinitionOwner
                                loggerFactory
                                dotnetRuntimeDirs
                                operation
                                ctx
                                table

                        ctx, (table, content)
                    )

            // Indexed here rather than at each use: the memo is built once per definition and read
            // once per `callvirt`, so the cost belongs on the build.
            //
            // A declaration appears at most once, every method being placed by exactly one type, so
            // `Add` cannot collide -- and if it somehow did, throwing beats silently keeping one.
            let byDeclaration =
                (ImmutableDictionary.CreateBuilder<_, _> (), table.Placed)
                ||> List.fold (fun acc (slot, index) ->
                    acc.Add ((slot.DeclaredBy.AssemblyFullName, slot.Method.IdentityKey), index)
                    acc
                )

            let computed =
                {
                    DispatchTable.SlotOfDeclaration = byDeclaration.ToImmutable ()
                    DispatchTable.Occupants =
                        content |> List.map (fun entry -> entry.Occupant) |> ImmutableArray.CreateRange
                }

            state.WithVirtualSlotTable concreteTypeInfo.Identity computed, Some computed


    /// The instance vtable of a runtime type, base-first: index `i` is the method that occupies slot
    /// `i`.
    ///
    /// For a nominal type this is its *definition's* vtable, which is the same list for every
    /// instantiation -- see `vtableOfDefinition`, where the rule lives. A structural handle has no
    /// definition to ask: byrefs, pointers and function pointers are TypeDescs with no method table,
    /// and a synthesised array's slots are `System.Array`'s.
    let rec vtableOfClosed
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (concreteType : ConcreteTypeHandle)
        : TypeSystemState * VtableSlot list
        =
        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // Byrefs, pointers, and function pointers are TypeDescs in CoreCLR with no
            // MethodTable, so they have no vtable at all.
            state, []
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Synthesised array MethodTables inherit their virtual slots from System.Array (and
            // through it, System.Object); the structural array handle itself introduces none.
            let state, baseHandle =
                TypeSystemState.resolveBaseConcreteType
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    concreteType

            match baseHandle with
            | None -> state, []
            | Some bh -> vtableOfClosed loggerFactory dotnetRuntimeDirs baseClassTypes operation state bh
        | ConcreteTypeHandle.Concrete _ ->
            // A nominal type's own generic arguments say nothing about which slot anything occupies:
            // CoreCLR hands the method-table builder its parent as a raw `SigPointer` into the
            // extends-clause blob with no substitution for the type being built
            // (methodtablebuilder.cpp:1330-1337), and clones the canonical method table for every
            // instantiation that shares code
            // (`Generics::CreateTypeHandleForNonCanonicalGenericInstantiation`, generics.cpp:159-495).
            // Measured over corelib, System.Linq, System.Text.Json, System.Collections.Concurrent and
            // System.Private.Uri: 2683 (definition, instantiation) pairs agree on the whole layout.
            let concreteTypeInfo, _ =
                TypeSystemState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            vtableOfDefinition loggerFactory dotnetRuntimeDirs baseClassTypes operation state concreteTypeInfo.Identity

    let slotTableOfClosed
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (concreteType : ConcreteTypeHandle)
        : TypeSystemState * MethodTableLayout.MethodSlotTable
        =
        // Only the vtable walk recurses through the base chain; the region beyond it is this type's
        // alone, so it is computed once here rather than once per ancestor and discarded.
        let state, virtualSlots =
            vtableOfClosed loggerFactory dotnetRuntimeDirs baseClassTypes operation state concreteType

        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // TypeDescs with no MethodTable, so no slots of either kind -- the same
            // reason `vtableOfClosed` gives them an empty vtable.
            state,
            {
                MethodTableLayout.MethodSlotTable.Vtable = virtualSlots
                MethodTableLayout.MethodSlotTable.BeyondVtable = []
            }
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // A synthesised array MethodTable really does carry slots beyond its vtable, for the
            // intrinsic Get/Set/Address and the ctor, and PawPrint models none of them --
            // `introducedMethodsOfClosed` refuses the same question for the same reason. Answering
            // "none" would be a wrong answer rather than an absent one, so refuse. Unreachable from
            // `GetSlot` today: a method handle always resolves to a `Concrete` declaring type, there
            // being no way to mint one naming an array intrinsic.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{concreteType}; the array intrinsic methods (Get/Set/Address/.ctor) occupy slots beyond the vtable that PawPrint does not model"
        | ConcreteTypeHandle.Concrete _ ->
            let concreteTypeInfo, _ =
                TypeSystemState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            let owner = ownerOfDefinition operation state concreteTypeInfo.Identity
            let _, typeInfo = definitionMetadata operation state concreteTypeInfo.Identity

            state,
            {
                MethodTableLayout.MethodSlotTable.Vtable = virtualSlots
                MethodTableLayout.MethodSlotTable.BeyondVtable =
                    MethodTableLayout.slotsBeyondVtableOfDefinition operation owner typeInfo
            }

    /// The size of the instance vtable for a closed type, matching CoreCLR's
    /// `MethodTable::GetNumVirtuals()`.
    let numVirtualsOfClosed
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (concreteType : ConcreteTypeHandle)
        : TypeSystemState * int
        =
        // The length of `vtableOfClosed` by definition rather than an independently-computed sum,
        // because `PopulateMethods` compares it against `RuntimeMethodHandle.GetSlot`'s answer:
        // two walks that had to agree by discipline would disagree silently, and the symptom
        // would be a wrong `isVirtual` rather than a crash.
        let state, slots =
            vtableOfClosed loggerFactory dotnetRuntimeDirs baseClassTypes operation state concreteType

        state, List.length slots
