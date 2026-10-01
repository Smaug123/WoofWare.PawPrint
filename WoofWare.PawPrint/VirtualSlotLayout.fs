namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

/// Where a method sits in a type's method table: which slot each declaration owns, and how many of
/// those slots form the vtable. This is slot *identity*; a MethodImpl changes what a slot holds
/// without changing which slot its body was declared at, so slot *content* is a separate question.
///
/// The walks are `ConcreteMethodTable`'s, lifted here to the machine state. What this module adds is
/// the reflection QCalls' questions about a `RuntimeTypeHandleTarget`, which can name an open type.
///
/// Kept apart from `NativeRuntimeTypeHelpers`, where the QCalls that ask these questions live,
/// because virtual dispatch asks them too and compiles well before those QCalls do.
module VirtualSlotLayout =

    /// `SlotOwner` and `VtableSlot` were nested in this module before they moved to the namespace, so
    /// that `IlMachineState` -- which compiles well before this file and memoises the walks --
    /// could name them. Both are public API of a shipped package, so these keep
    /// `VirtualSlotLayout.SlotOwner` and `VirtualSlotLayout.VtableSlot` resolving for existing source.
    ///
    /// An abbreviation is not a distinct CLR type, so this restores *source* compatibility only: a
    /// consumer compiled against the previous package binds to nested types that no longer exist and
    /// must be recompiled.
    type SlotOwner = WoofWare.PawPrint.SlotOwner

    type VtableSlot = WoofWare.PawPrint.VtableSlot

    /// The owner of a slot read from a closed type.
    let private slotOwnerOfClosed (concreteType : ConcreteType<ConcreteTypeHandle>) : SlotOwner =
        {
            SlotOwner.AssemblyFullName = concreteType.AssemblyFullName
            SlotOwner.Identity = concreteType.Identity
            SlotOwner.Substitution = TypeConcretization.SubstitutionContext.ofClosed concreteType.Generics
            SlotOwner.Description = string concreteType
        }

    /// `ConcreteMethodTable.definitionMetadata` against the machine's type system.
    let internal definitionMetadata
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        ConcreteMethodTable.definitionMetadata operation state.TypeSystem identity

    /// `ConcreteMethodTable.ownerOfDefinition` against the machine's type system.
    let ownerOfDefinition (operation : string) (state : IlMachineState) (identity : ResolvedTypeIdentity) : SlotOwner =
        ConcreteMethodTable.ownerOfDefinition operation state.TypeSystem identity

    /// `ConcreteMethodTable.nominalIdentityOfSpelling` against the machine's type system.
    let internal nominalIdentityOfSpelling
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (assembly : DumpedAssembly)
        (ty : TypeDefn)
        : IlMachineState * ResolvedTypeIdentity
        =
        let typeSystem, result =
            ConcreteMethodTable.nominalIdentityOfSpelling
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                assembly
                ty

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.baseOfDefinition` against the machine's type system.
    let internal baseOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (owner : SlotOwner)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * (ResolvedTypeIdentity * ImmutableArray<TypeConcretization.SubstitutionArgument>) option
        =
        let typeSystem, result =
            ConcreteMethodTable.baseOfDefinition
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                owner
                typeInfo

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.vtableOfDefinition` against the machine's type system.
    let vtableOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * VtableSlot list
        =
        let typeSystem, result =
            ConcreteMethodTable.vtableOfDefinition
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                identity

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.contentVtableOfDefinition` against the machine's type system.
    let contentVtableOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * VtableSlot list
        =
        let typeSystem, result =
            ConcreteMethodTable.contentVtableOfDefinition
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                identity

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.placedSlotsOfDefinition` against the machine's type system.
    let placedSlotsOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * (VtableSlot * int) list
        =
        let typeSystem, result =
            ConcreteMethodTable.placedSlotsOfDefinition
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                identity

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.slotTableOfDefinition` against the machine's type system.
    let slotTableOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * MethodTableLayout.MethodSlotTable
        =
        let typeSystem, result =
            ConcreteMethodTable.slotTableOfDefinition
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                identity

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.numVirtualsOfDefinition` against the machine's type system.
    let numVirtualsOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * int
        =
        let typeSystem, result =
            ConcreteMethodTable.numVirtualsOfDefinition
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                identity

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.dispatchTableOfClosed` against the machine's type system.
    let dispatchTableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * DispatchTable option
        =
        let typeSystem, result =
            ConcreteMethodTable.dispatchTableOfClosed
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                concreteType

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.vtableOfClosed` against the machine's type system.
    let vtableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * VtableSlot list
        =
        let typeSystem, result =
            ConcreteMethodTable.vtableOfClosed
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                concreteType

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.slotTableOfClosed` against the machine's type system.
    let slotTableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * MethodTableLayout.MethodSlotTable
        =
        let typeSystem, result =
            ConcreteMethodTable.slotTableOfClosed
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                concreteType

        state.WithTypeSystem typeSystem, result

    /// `ConcreteMethodTable.numVirtualsOfClosed` against the machine's type system.
    let numVirtualsOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * int
        =
        let typeSystem, result =
            ConcreteMethodTable.numVirtualsOfClosed
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                concreteType

        state.WithTypeSystem typeSystem, result

    let numVirtuals
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : IlMachineState * int
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            // CoreCLR's GetNumVirtuals asserts !typeHandle.IsGenericVariable(); the BCL's
            // RuntimeType.GetMethodCandidates strips generic variables before calling.
            // Reaching here means a managed-side invariant was violated.
            failwith
                $"%s{operation}: invoked on type-generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before calling"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"%s{operation}: invoked on method-generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before calling"
        // An array over a variable is an array MethodTable whose slots are `System.Array`'s, as a
        // closed array's are (`vtableOfClosed`); the element plays no part.
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) ->
            let state, arrayType =
                IlMachineState.resolveBaseRuntimeTypeHandleTarget loggerFactory baseClassTypes state typeHandleTarget

            match arrayType with
            | Some (RuntimeTypeHandleTarget.Closed arrayHandle) ->
                numVirtualsOfClosed loggerFactory baseClassTypes operation state arrayHandle
            | other ->
                failwith
                    $"%s{operation}: expected the closed System.Array as the base of %O{typeHandleTarget}, got %O{other}"
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            RuntimeTypeHandleTarget.refuseComposite operation typeHandleTarget
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
            // Slot layout is a property of the generic definition: `MethodTableBuilder` places
            // virtuals from the definition's own metadata, so every instantiation ends up with the
            // same numbering -- and for a reference type CoreCLR does not even recompute it, taking
            // `SetNumVirtuals` from the canonical instantiation and sharing its vtable chunks
            // (`Generics::CreateTypeHandleForNonCanonicalGenericInstantiation`, generics.cpp:205 and
            // :327-334). So this is the same number `numVirtualsOfClosed` answers for any `G<...>`,
            // and asking the definition is the only way to get it when the guest named no closed
            // instantiation: neither the typical instantiation nor an open construction such as
            // `Base<T>` over a deriving definition's `T` has arguments to concretise.
            numVirtualsOfDefinition loggerFactory baseClassTypes operation state identity
        | RuntimeTypeHandleTarget.Closed handle ->
            numVirtualsOfClosed loggerFactory baseClassTypes operation state handle

    /// What `RuntimeTypeHandle_GetMethodAt` finds at a slot of a type's method table.
    [<RequireQualifiedAccess>]
    type MethodAtSlot =
        /// The method the slot holds: its content, together with the type that declared it.
        | Method of VtableSlot
        /// No such slot. CoreCLR throws `ArgumentException` (`Arg_ArgumentOutOfRangeException`).
        | OutOfRange

    /// The method at slot <paramref name="slot"/> of the method table of <paramref name="target"/>,
    /// as CoreCLR's `RuntimeTypeHandle_GetMethodAt` (runtimehandles.cpp:399) reads it.
    ///
    /// Below `GetNumVirtuals` that is `MethodTable::GetMethodDescForSlot`: the slot's *content*,
    /// so a slot a MethodImpl retargeted to another body answers that body rather than the
    /// declaration owning the slot -- the same table `callvirt` dispatches through. At and past
    /// `GetNumVirtuals`, only an interface has anything to find: its static virtuals, numbered on
    /// from the vtable in MethodDef order (`GetNumVirtualsAndStaticVirtuals` counts them the same
    /// way). Everything else past the end is `OutOfRange`.
    ///
    /// <paramref name="target"/> must carry a method table: a closed type, an array (whose slots
    /// are `System.Array`'s), a generic definition or an open construction (whose slots are its
    /// definition's). The managed wrapper throws
    /// `ArgumentException` for a TypeDesc before the QCall, so one reaching here is a contract
    /// violation and this fails rather than answering.
    let rec methodAt
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        (slot : int)
        : IlMachineState * MethodAtSlot
        =
        if slot < 0 then
            // The managed wrapper (RuntimeHandles.cs:471) throws before the QCall; CoreCLR asserts.
            failwith
                $"%s{operation}: slot %d{slot} is negative; the managed wrapper throws ArgumentException before this point"

        // `MethodTable::HasVirtualStaticMethods() && IsInterface()`: the tail past the vtable is
        // searched only for an interface, and only its static virtuals are counted along it.
        let staticVirtualsOf (isInterface : bool) (beyondVtable : VtableSlot list) : VtableSlot list =
            if isInterface then
                beyondVtable
                |> List.filter (fun candidate -> candidate.Method.IsVirtual && candidate.Method.IsStatic)
            else
                []

        let answer (vtable : VtableSlot list) (staticVirtuals : VtableSlot list) : MethodAtSlot =
            let numVirtuals = List.length vtable

            if slot < numVirtuals then
                MethodAtSlot.Method vtable.[slot]
            else
                match List.tryItem (slot - numVirtuals) staticVirtuals with
                | Some found -> MethodAtSlot.Method found
                | None -> MethodAtSlot.OutOfRange

        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            let state, table =
                dispatchTableOfClosed loggerFactory baseClassTypes operation state handle

            match table with
            | None ->
                failwith
                    $"%s{operation}: %O{target} is a TypeDesc with no method table; the managed wrapper throws ArgumentException before this point"
            | Some table ->

            let vtable = List.ofSeq table.Occupants

            // Only an interface has a static-virtual tail, and only a nominal type can be one: an
            // array's slots are `System.Array`'s, which is a class.
            let state, staticVirtuals =
                match handle with
                | ConcreteTypeHandle.Concrete _ ->
                    match IlMachineState.tryGetConcreteTypeInfo state handle with
                    | Some (_, typeInfo) when typeInfo.IsInterface ->
                        let state, slotTable =
                            slotTableOfClosed loggerFactory baseClassTypes operation state handle

                        state, staticVirtualsOf true slotTable.BeyondVtable
                    | _ -> state, []
                | _ -> state, []

            state, answer vtable staticVirtuals
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
            // An open construction's slots are its definition's, as `numVirtuals` says; only the
            // declaring type `declaringTypeAt` names for an occupant depends on the arguments.
            let state, vtable =
                contentVtableOfDefinition loggerFactory baseClassTypes operation state identity

            let _, typeInfo = definitionMetadata operation state identity

            let state, staticVirtuals =
                if typeInfo.IsInterface then
                    let state, slotTable =
                        slotTableOfDefinition loggerFactory baseClassTypes operation state identity

                    state, staticVirtualsOf true slotTable.BeyondVtable
                else
                    state, []

            state, answer vtable staticVirtuals
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            failwith
                $"%s{operation}: %O{target} is a TypeDesc with no method table; the managed wrapper throws ArgumentException before this point"
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) ->
            // An array MethodTable whose slots are `System.Array`'s, whatever the element -- the
            // same delegation `dispatchTableOfClosed` makes for a closed array.
            let state, arrayType =
                IlMachineState.resolveBaseRuntimeTypeHandleTarget loggerFactory baseClassTypes state target

            match arrayType with
            | Some (RuntimeTypeHandleTarget.Closed _ as arrayTarget) ->
                methodAt loggerFactory baseClassTypes operation state arrayTarget slot
            | other ->
                failwith $"%s{operation}: expected the closed System.Array as the base of %O{target}, got %O{other}"

    /// The declaring type of <paramref name="occupant"/>, found at slot <paramref name="slot"/> of
    /// <paramref name="receiver"/>'s method table by `methodAt`, as the receiver's chain
    /// instantiates it -- the type `GetBaseDefinition` and the accessor association report as the
    /// method's `DeclaringType`, and the declaring type of the handle `GetMethodAt` mints.
    ///
    /// That is the first type on the receiver's class chain whose definition declares the occupant,
    /// with the chain walked exactly as `Type.BaseType` reports it
    /// (`resolveBaseRuntimeTypeHandleTarget`), so the two cannot disagree. An array's chain starts
    /// at `System.Array`. On the chain of a definition or of an open construction an ancestor can
    /// itself be an open construction, `Base<T>` over the definition's own `T`, and is answered as
    /// one; an ancestor all of whose arguments come out closed is the closed type.
    let declaringTypeAt
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (receiver : RuntimeTypeHandleTarget)
        (slot : int)
        (occupant : VtableSlot)
        : IlMachineState * RuntimeTypeHandleTarget
        =
        let declaringIdentity = occupant.DeclaredBy.Identity

        let rec walk
            (state : IlMachineState)
            (target : RuntimeTypeHandleTarget)
            : IlMachineState * RuntimeTypeHandleTarget
            =
            let identity =
                match target with
                | RuntimeTypeHandleTarget.Closed handle ->
                    // `None` for a closed array, whose chain continues at `System.Array`.
                    IlMachineState.tryGetConcreteTypeInfo state handle
                    |> Option.map (fun (concreteType, _) -> concreteType.Identity)
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
                | RuntimeTypeHandleTarget.OpenConstructed (identity, _) -> Some identity
                | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) ->
                    None
                | RuntimeTypeHandleTarget.DynamicMethodsClass _
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _
                | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
                | RuntimeTypeHandleTarget.FunctionPointer _ ->
                    failwith
                        $"%s{operation}: %O{target} appeared on the class chain of %O{receiver}, but only a type with a method table can be anything's base"

            if identity = Some declaringIdentity then
                state, target
            else
                let state, parent =
                    IlMachineState.resolveBaseRuntimeTypeHandleTarget loggerFactory baseClassTypes state target

                match parent with
                | Some parent -> walk state parent
                | None ->
                    failwith
                        $"%s{operation}: slot %d{slot} of %O{receiver} is held by %s{occupant.Method.Name}, declared by %s{occupant.DeclaredBy.Description}, which is not on the receiver's class chain"

        match receiver with
        | RuntimeTypeHandleTarget.Closed _
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
        | RuntimeTypeHandleTarget.OpenConstructed _
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) ->
            walk state receiver
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            // `methodAt` answers for none of these, so no occupant of theirs exists to ask about.
            failwith
                $"%s{operation}: %O{receiver} has no method table, so it has no slot for %s{occupant.Method.Name} to occupy"

    /// The methods a declaring type introduces, as CoreCLR's `IntroducedMethodIterator` walks
    /// them: the type's own MethodDef rows in metadata order, never an inherited one.
    ///
    /// This is the same list the slot table is laid out from, so a row `declaredMethodsOf` drops
    /// (a COM vtable-gap marker, which has no MethodDesc for the iterator to reach) is absent here
    /// too. `PopulateMethods` asks `GetSlot` about every virtual this walk yields, and the table
    /// can only answer for rows it placed.
    ///
    /// Returns the defining assembly and the declaring target alongside them, because those are
    /// what `MethodHandleRegistry.getOrAllocateInternalHandle` needs to mint a handle and they
    /// differ between the closed and open-definition cases.
    ///
    /// `None` means "this type has no MethodTable, so it introduces nothing" — byref, pointer and
    /// function-pointer TypeDescs. Callers should emit the null sentinel so the managed
    /// `IntroducedMethodEnumerator` terminates immediately.
    let introducedMethodsOf
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : (string *
          RuntimeTypeHandleTarget *
          MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list) option
        =
        match target with
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            let concreteType, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state handle
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                )

            let declared =
                MethodTableLayout.declaredMethodsOf operation (slotOwnerOfClosed concreteType) typeInfo
                |> List.map fst

            Some (concreteType.AssemblyFullName, target, declared)
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _) ->
            // CoreCLR's typical instantiation of `G<>` is a MethodTable carrying the definition's
            // own TypeDef token, and its MethodDescChunks hold the definition's MethodDefs; so does
            // an open construction such as `Base<T>` over a deriving definition's `T`, whose
            // MethodDescs are its own (measured: `typeof(Derived<>).BaseType.GetMethod("M")`'s
            // `MethodHandle` differs from `typeof(Base<>).GetMethod("M")`'s). So the answer is the
            // metadata method list read straight off the typedef, with the target itself as the
            // declaring type: no instantiation is needed, because this only lists the methods.
            let _, typeInfo = definitionMetadata operation state identity

            let declared =
                MethodTableLayout.declaredMethodsOf operation (ownerOfDefinition operation state identity) typeInfo
                |> List.map fst

            Some (identity.AssemblyFullName, target, declared)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _)
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.Byref | CompositeShape.Pointer), _)
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            // CoreCLR's IntroducedMethodIterator runs on a MethodTable; byrefs/pointers/function-
            // pointers are TypeDescs with no MethodTable, so GetFirstIntroducedMethod returns null
            // and the managed enumerator terminates without iterating.
            None
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // Also TypeVarTypeDescs, and CoreCLR agrees they introduce nothing:
            // `PopulateConstructors` returns an empty array for `IsGenericParameter`
            // (RuntimeType.CoreCLR.cs:755) rather than iterating.
            None
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _)
        | RuntimeTypeHandleTarget.Composite ((CompositeShape.OneDimArrayZero | CompositeShape.Array _), _) ->
            // Synthesised array MethodTables have a small fixed set of introduced methods (Get/Set/
            // Address/the parameterless ctor). PawPrint does not yet model these; no test exercises
            // this path, so fail loudly to flag the gap rather than silently reporting zero.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{target}; need to surface the array's intrinsic Get/Set/Address methods"
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
