namespace WoofWare.PawPrint

open System.Collections.Generic
open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Which method implements an interface method on a receiver whose type supplies one: CoreCLR's
/// interface dispatch map.
///
/// The question has two halves, and they belong to different types. *Which slot* implements an
/// interface method is decided by the type whose level of the chain mapped it -- at type load, by
/// `MethodTableBuilder::PlaceInterfaceMethods` and `PlaceMethodImpls` -- and that type records only
/// the entries it contributes, not what it inherits. *What that slot holds* is then read from the
/// receiver's own vtable, so that an override further down the chain is what runs. Dispatch walks
/// from the receiver upwards and takes the first level that maps the method
/// (`MethodTable::FindDispatchEntry`, methodtable.cpp).
///
/// A default interface body is not in the map: a receiver no level of which maps the method falls
/// through to `FindDefaultInterfaceImplementation`, which is the caller's business.
[<RequireQualifiedAccess>]
module InterfaceDispatch =

    /// Resolve one entry of `ownerTy`'s `ImplementedInterfaces` list to the concrete interface
    /// it names, registering that instantiation in the ConcreteTypes registry if it is not
    /// already there.
    let internal resolveImplementedInterface
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ownerTy : ConcreteType<ConcreteTypeHandle>)
        (impl : WoofWare.PawPrint.InterfaceImplementation)
        (state : IlMachineState)
        : IlMachineState *
          ConcreteTypeHandle *
          ConcreteType<ConcreteTypeHandle> *
          TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let ownerAssy =
            state._LoadedAssemblies.ByDefinitionName ownerTy.Identity.AssemblyFullName

        let implAssy =
            match state.LoadedAssembly impl.RelativeToAssembly.FullName with
            | Some assy -> assy
            | None -> ownerAssy

        let state, implTypeDefn, implResolvedAssy =
            IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state implAssy impl.InterfaceHandle

        let state, implHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                implResolvedAssy.DefinitionFullName
                ownerTy.Generics
                ImmutableArray.Empty
                implTypeDefn

        match IlMachineState.tryGetConcreteTypeInfo state implHandle with
        | Some (implTy, typeInfo) -> state, implHandle, implTy, typeInfo
        | None -> failwith $"Interface implementation handle %O{implHandle} was not registered or has no TypeDef row"

    /// One entry of a type's interface map (`bmtInterfaceEntry`), with the two flags
    /// `PlaceInterfaceMethods` decides placement by.
    type internal InterfaceMapEntry =
        {
            /// The interface instantiation, as the type sees it.
            Interface : ConcreteTypeHandle
            Type : ConcreteType<ConcreteTypeHandle>
            TypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>
            /// The same instantiation read in the vocabulary of the *generic definition* of the type
            /// whose map this is, which is where CoreCLR compares an interface method's signature
            /// against the type's candidates. Closing both sides first would let two distinct
            /// declarations coincide: `C<T> : I<int>` declaring `M(T)` and `M(int)` must bind
            /// `I<int>.M(int)` to `M(int)` alone, even in `C<int>`.
            Substitution : TypeConcretization.SubstitutionContext
            /// The type lists this interface in its own InterfaceImpl rows, rather than reaching it
            /// only through its parent or through another interface. A declared interface the parent
            /// also implements is *re-implemented*: its slots are matched afresh against the type's
            /// own methods.
            DeclaredOnType : bool
            /// The parent's interface map already holds this instantiation.
            ImplementedByParent : bool
        }

    /// The substitution an InterfaceImpl row's own spelling denotes, read in `spellingContext`.
    let private spelledInterfaceSubstitution
        (operation : string)
        (spellingAssembly : DumpedAssembly)
        (spellingContext : TypeConcretization.SubstitutionContext)
        (implementedTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (token : MetadataToken)
        : TypeConcretization.SubstitutionContext
        =
        let arguments =
            match token with
            | MetadataToken.TypeDefinition _
            | MetadataToken.TypeReference _ -> ImmutableArray.Empty
            | MetadataToken.TypeSpecification handle ->
                match spellingAssembly.TypeSpecs.[handle].Signature with
                | TypeDefn.GenericInstantiation (_, arguments) -> ImmutableArray.CreateRange arguments
                | _ -> ImmutableArray.Empty
            | other ->
                failwith
                    $"%s{operation}: an InterfaceImpl row in %s{spellingAssembly.Name.Name} names its interface with %O{other}, which is not a TypeDefOrRefOrSpec"

        // The row supplies one argument per variable the interface declares, or the image would not
        // load. Checked because a decomposition bug above would otherwise surface as a signature
        // comparison reading past the end of a substitution.
        if arguments.Length <> implementedTypeInfo.Generics.Length then
            failwith
                $"%s{operation}: an InterfaceImpl row in %s{spellingAssembly.Name.Name} names %s{implementedTypeInfo.Namespace}.%s{implementedTypeInfo.Name}, which declares %d{implementedTypeInfo.Generics.Length} generic parameter(s), with %d{arguments.Length} argument(s)"

        TypeConcretization.SubstitutionContext.forBase spellingAssembly.DefinitionFullName arguments spellingContext

    /// The interface map of a class or value type, in CoreCLR's order: the parent's entries first,
    /// then each interface the type declares followed by its own parents, depth-first, each
    /// instantiation appearing once (`MethodTableBuilder::ExpandApproxInheritedInterfaces` and
    /// `ExpandApproxDeclaredInterfaces`, methodtablebuilder.cpp).
    ///
    /// Instantiations are told apart as CoreCLR tells them apart, on the generic definition, so a
    /// type naming two instantiations that its own arguments make coincide -- `C<T, U> : I<T>, I<U>`
    /// at `C<int, int>` -- has two entries with one closed handle.
    let rec internal interfaceMapOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandle : ConcreteTypeHandle)
        : IlMachineState * InterfaceMapEntry list
        =
        let ty, typeInfo =
            match IlMachineState.tryGetConcreteTypeInfo state typeHandle with
            | Some found -> found
            | None -> failwith $"%s{operation}: %O{typeHandle} has no TypeDef row, so it has no interface map"

        let owner = VirtualSlotLayout.ownerOfDefinition operation state ty.Identity

        let assembly, _ = VirtualSlotLayout.definitionMetadata operation state ty.Identity

        let state, parentHandle =
            IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state typeHandle

        let state, inherited =
            match parentHandle with
            | None -> state, []
            | Some parentHandle ->
                let state, parentMap =
                    interfaceMapOf loggerFactory baseClassTypes operation state parentHandle

                let state, baseOfDefinition =
                    VirtualSlotLayout.baseOfDefinition loggerFactory baseClassTypes operation state owner typeInfo

                match baseOfDefinition with
                | None ->
                    failwith
                        $"%s{operation}: %s{owner.Description} has a base type as an instantiation but none as a definition"
                | Some (baseIdentity, arguments) ->

                let inherited =
                    parentMap
                    |> List.map (fun entry ->
                        { entry with
                            Substitution =
                                TypeConcretization.SubstitutionContext.rebase baseIdentity arguments entry.Substitution
                            DeclaredOnType = false
                            ImplementedByParent = true
                        }
                    )

                state, inherited

        // "Checking for further expanded interfaces isn't necessary for the system module, as we can
        // rely on the C# compiler to have found all of the interfaces that the type implements"
        // (methodtablebuilder.cpp, `ExpandApproxInterface`): for a value type declared in CoreLib, an
        // interface's own parents are not expanded where it is declared. Classes are unaffected.
        let expandParents =
            not (
                ty.Identity.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
                && DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo
            )

        let rec expand
            (state : IlMachineState, map : InterfaceMapEntry list)
            (declared : bool)
            (spellingAssembly : DumpedAssembly)
            (spellingContext : TypeConcretization.SubstitutionContext)
            (spellingOwner : ConcreteType<ConcreteTypeHandle>)
            (row : WoofWare.PawPrint.InterfaceImplementation)
            : IlMachineState * InterfaceMapEntry list
            =
            let state, handle, interfaceTy, interfaceTypeInfo =
                resolveImplementedInterface loggerFactory baseClassTypes spellingOwner row state

            let substitution =
                spelledInterfaceSubstitution
                    operation
                    spellingAssembly
                    spellingContext
                    interfaceTypeInfo
                    row.InterfaceHandle

            // The same instantiation *of the definition*, which a matching closed handle is necessary
            // but not sufficient for: `C<T> : B<T>, I<int>` over `B<T> : I<T>` holds `I<!0>` and
            // `I<int>` as two entries, and at `C<int>` both close to `I<int>`.
            let rec findExisting (state : IlMachineState) (i : int) (entries : InterfaceMapEntry list) =
                match entries with
                | [] -> state, None
                | entry :: rest when entry.Interface <> handle -> findExisting state (i + 1) rest
                | entry :: rest ->
                    let state, same =
                        IlMachineState.substitutionsEquivalent
                            loggerFactory
                            baseClassTypes
                            state
                            entry.Substitution
                            substitution

                    if same then
                        state, Some i
                    else
                        findExisting state (i + 1) rest

            let state, existing = findExisting state 0 map

            match existing with
            | Some existing ->
                // Already present: an entry is added once, and its parents with it. Re-declaring it
                // on this type is recorded, since that is what makes it re-implemented here.
                if declared then
                    state,
                    map
                    |> List.mapi (fun i entry ->
                        if i = existing then
                            { entry with
                                DeclaredOnType = true
                            }
                        else
                            entry
                    )
                else
                    state, map
            | None ->

            let entry =
                {
                    Interface = handle
                    Type = interfaceTy
                    TypeInfo = interfaceTypeInfo
                    Substitution = substitution
                    DeclaredOnType = declared
                    ImplementedByParent = false
                }

            let map = map @ [ entry ]

            if not expandParents then
                state, map
            else

            let interfaceAssembly, _ =
                VirtualSlotLayout.definitionMetadata operation state interfaceTy.Identity

            ((state, map), interfaceTypeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun acc parentRow -> expand acc false interfaceAssembly substitution interfaceTy parentRow)

        ((state, inherited), typeInfo.ImplementedInterfaces)
        ||> Seq.fold (fun acc row -> expand acc true assembly owner.Substitution ty row)

    /// The interface method a MethodImpl declaration names, together with the interface instantiation
    /// it names it on; `None` when the declaration is not an interface instance method, which makes
    /// the row a vtable write (see `VirtualSlotLayout.contentVtableOfDefinition`) or a static virtual
    /// implementation (resolved through MethodImpl rows by the caller, not through this map).
    let private interfaceMethodImplDeclaration
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (ownerTy : ConcreteType<ConcreteTypeHandle>)
        (ownerDescription : string)
        (assembly : DumpedAssembly)
        (declaration : MetadataToken)
        : IlMachineState * (ConcreteTypeHandle * SlotIdentity) option
        =
        let concretizeParent (state : IlMachineState) (parent : MetadataToken) =
            let state, parentTypeDefn, parentAssembly =
                IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state assembly parent

            let state, parentHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    parentAssembly.DefinitionFullName
                    ownerTy.Generics
                    ImmutableArray.Empty
                    parentTypeDefn

            match IlMachineState.tryGetConcreteTypeInfo state parentHandle with
            | Some (parentTy, parentTypeInfo) -> state, parentHandle, parentTy, parentTypeInfo
            | None ->
                failwith
                    $"%s{operation}: a MethodImpl on %s{ownerDescription} names its declaration on %O{parentHandle}, which has no TypeDef row"

        match declaration with
        | MetadataToken.MethodDef handle ->
            let method = assembly.Methods.[handle]

            if method.IsStatic then
                state, None
            else

            let declaringTypeInfo =
                assembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

            if not declaringTypeInfo.IsInterface then
                state, None
            elif not declaringTypeInfo.Generics.IsEmpty then
                // A MethodDef token carries no instantiation, so it can name a generic interface only
                // at its typical instantiation, which no class implements.
                failwith
                    $"TODO: %s{operation}: a MethodImpl on %s{ownerDescription} names %s{method.Name} on the generic interface %s{declaringTypeInfo.Name} by MethodDef, which carries no instantiation"
            else
                let state, interfaceHandle, _, _ =
                    concretizeParent state (MetadataToken.TypeDefinition method.RequiredDeclaringType.Definition.Get)

                state, Some (interfaceHandle, (assembly.DefinitionFullName, method.IdentityKey))
        | MetadataToken.MemberReference handle ->
            let memberRef = assembly.Members.[handle]

            match memberRef.Signature with
            | MemberSignature.Field _ ->
                failwith $"%s{operation}: a MethodImpl on %s{ownerDescription} names a field as its declaration"
            | MemberSignature.Method signature ->

            if not signature.Header.Get.IsInstance then
                state, None
            else

            match memberRef.Parent with
            | MetadataToken.TypeDefinition _
            | MetadataToken.TypeReference _
            | MetadataToken.TypeSpecification _ ->
                let state, interfaceHandle, interfaceTy, interfaceTypeInfo =
                    concretizeParent state memberRef.Parent

                if not interfaceTypeInfo.IsInterface then
                    state, None
                else

                // Compared in the interface's own open vocabulary, as `FindDeclMethodOnClassInHierarchy`
                // compares the named type: neither side substituted, nominal tokens resolved against
                // whichever assembly spelled them.
                let openContext =
                    TypeConcretization.SubstitutionContext.forDefinition
                        interfaceTy.Identity
                        interfaceTypeInfo.Generics.Length

                let state, matches =
                    ((state, []), interfaceTypeInfo.Methods)
                    ||> List.fold (fun (state, acc) candidate ->
                        if candidate.Name <> memberRef.PrettyName || candidate.IsStatic then
                            state, acc
                        else

                        let state, equivalent =
                            IlMachineState.signaturesEquivalent
                                loggerFactory
                                baseClassTypes
                                state
                                false
                                {
                                    Signature = signature
                                    AssemblyFullName = assembly.DefinitionFullName
                                    DeclaringTypeGenerics = openContext
                                }
                                {
                                    Signature = candidate.Signature
                                    AssemblyFullName = candidate.DeclaringAssemblyFullName
                                    DeclaringTypeGenerics = openContext
                                }

                        state, (if equivalent then candidate :: acc else acc)
                    )

                match matches with
                | [ interfaceMethod ] ->
                    state,
                    Some (interfaceHandle, (interfaceMethod.DeclaringAssemblyFullName, interfaceMethod.IdentityKey))
                | [] ->
                    failwith
                        $"%s{operation}: a MethodImpl on %s{ownerDescription} declares an implementation of %s{memberRef.PrettyName} on %s{interfaceTypeInfo.Namespace}.%s{interfaceTypeInfo.Name}, which declares no instance method of that name and signature; CoreCLR rejects this type at load time"
                | _ ->
                    failwith
                        $"%s{operation}: a MethodImpl on %s{ownerDescription} declares an implementation of %s{memberRef.PrettyName} on %s{interfaceTypeInfo.Namespace}.%s{interfaceTypeInfo.Name}, which declares that name and signature more than once; ECMA-335 II.22.26 forbids a type repeating a method signature"
            | _ ->
                // A ModuleRef or MethodDef parent (the vararg case) cannot name an interface method.
                state, None
        | other ->
            failwith
                $"%s{operation}: a MethodImpl on %s{ownerDescription} names its declaration with the token %O{other}, which is neither a MethodDef nor a MemberRef; ECMA-335 II.22.27 permits only those two"

    /// The interface dispatch entries `typeHandle` itself contributes, excluding everything it
    /// inherits: `MethodTableBuilder::PlaceInterfaceMethods` followed by the interface half of
    /// `PlaceMethodImpls`.
    ///
    /// For each interface-map entry the type declares, or reaches for the first time through one it
    /// declares, each instance method of the interface is matched by name and exact signature against
    /// the public virtual methods the type itself declares, in declaration order. Only when that
    /// fails, and only for an entry the parent does not already implement, are the parent's vtable
    /// slots searched too, most-derived first. An entry the type merely inherits contributes nothing
    /// here: the parent's own map already answers for it, and dispatch reaches that map by walking up.
    /// The exception is an *abstract* parent: an inherited interface's methods that no class on the
    /// parent's chain implements -- those left to a default body -- are matched against this type's
    /// own public virtuals too.
    ///
    /// A MethodImpl naming an interface method then maps it to its body, whatever placement found.
    let rec ownDispatchMapOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandle : ConcreteTypeHandle)
        : IlMachineState * InterfaceDispatchMap
        =
        match Map.tryFind typeHandle state._InterfaceDispatchMaps with
        | Some cached -> state, cached
        | None ->

        let ty, typeInfo =
            match IlMachineState.tryGetConcreteTypeInfo state typeHandle with
            | Some found -> found
            | None -> failwith $"%s{operation}: %O{typeHandle} has no TypeDef row, so it has no dispatch map"

        let owner = VirtualSlotLayout.ownerOfDefinition operation state ty.Identity

        let assembly, _ = VirtualSlotLayout.definitionMetadata operation state ty.Identity

        let state, interfaceMap =
            interfaceMapOf loggerFactory baseClassTypes operation state typeHandle

        let state, parentHandle =
            IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state typeHandle

        let parentIsAbstract =
            match parentHandle with
            | None -> false
            | Some parentHandle ->
                match IlMachineState.tryGetConcreteTypeInfo state parentHandle with
                | Some (_, parentTypeInfo) ->
                    parentTypeInfo.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Abstract
                | None -> false

        let state, table =
            match VirtualSlotLayout.dispatchTableOfClosed loggerFactory baseClassTypes operation state typeHandle with
            | state, Some table -> state, table
            | _, None -> failwith $"%s{operation}: %s{owner.Description} has no method table"

        let slotOfOwnMethod (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) : int =
            match table.SlotOfDeclaration.TryGetValue ((method.DeclaringAssemblyFullName, method.IdentityKey)) with
            | true, slot -> slot
            | false, _ ->
                // `VirtualSlotLayout` documents the one way a declared virtual can own no slot: two of
                // the type's own virtuals coinciding once the base's arguments are substituted.
                failwith
                    $"%s{operation}: %s{owner.Description} declares the virtual method %s{method.Name}, which owns no slot in its method table"

        // The candidates `PlaceInterfaceMethods` collects from `DeclaredMethodIterator`: "non-publics
        // and statics can legally be exposed via an interface, but only through methodImpls".
        let ownCandidates =
            VirtualSlotLayout.declaredMethodsOf operation owner typeInfo
            |> List.map fst
            |> List.filter (fun method -> method.IsVirtual && method.IsPublic)

        // The parent's slots as `bmtParent` holds them -- each slot's declaration, not its content --
        // read in this type's vocabulary. Only needed for an interface the parent does not implement,
        // so it is not built until one asks.
        let parentSlots (state : IlMachineState) : IlMachineState * VtableSlot list =
            let state, baseOfDefinition =
                VirtualSlotLayout.baseOfDefinition loggerFactory baseClassTypes operation state owner typeInfo

            match baseOfDefinition with
            | None -> state, []
            | Some (baseIdentity, arguments) ->
                let state, parentVtable =
                    VirtualSlotLayout.vtableOfDefinition loggerFactory baseClassTypes operation state baseIdentity

                state,
                parentVtable
                |> List.map (fun slot ->
                    { slot with
                        VtableSlot.DeclaredBy =
                            { slot.DeclaredBy with
                                SlotOwner.Substitution =
                                    TypeConcretization.SubstitutionContext.rebase
                                        baseIdentity
                                        arguments
                                        slot.DeclaredBy.Substitution
                            }
                    }
                )

        let equivalent
            (state : IlMachineState)
            (interfaceComparand : TypeConcretization.SignatureComparand)
            (interfaceMethodName : string)
            (candidateName : string)
            (candidateComparand : TypeConcretization.SignatureComparand)
            : IlMachineState * bool
            =
            // `MethodSignature::Equivalent`: the names, then the whole signature, return type included.
            if interfaceMethodName <> candidateName then
                state, false
            else
                IlMachineState.signaturesEquivalent
                    loggerFactory
                    baseClassTypes
                    state
                    false
                    interfaceComparand
                    candidateComparand

        let rec firstEquivalent
            (state : IlMachineState)
            (interfaceComparand : TypeConcretization.SignatureComparand)
            (interfaceMethodName : string)
            (candidates : ('candidate * string * TypeConcretization.SignatureComparand) list)
            : IlMachineState * 'candidate option
            =
            match candidates with
            | [] -> state, None
            | (candidate, name, comparand) :: rest ->
                let state, matches =
                    equivalent state interfaceComparand interfaceMethodName name comparand

                if matches then
                    state, Some candidate
                else
                    firstEquivalent state interfaceComparand interfaceMethodName rest

        let ownComparands =
            ownCandidates
            |> List.map (fun method ->
                method,
                method.Name,
                {
                    TypeConcretization.SignatureComparand.Signature = method.Signature
                    TypeConcretization.SignatureComparand.AssemblyFullName = owner.AssemblyFullName
                    TypeConcretization.SignatureComparand.DeclaringTypeGenerics = owner.Substitution
                }
            )

        // Implicit placement, keyed on (interface-map index, interface method).
        let state, _, placed =
            ((state, None, ImmutableDictionary.Empty), List.indexed interfaceMap)
            ||> List.fold (fun (state, parentSlotsIfBuilt, placed) (index, entry) ->
                // Inherited and not re-declared, so the parent's map answers -- unless the parent is
                // abstract and so may have left a method to a default body.
                let inheritedOnly = entry.ImplementedByParent && not entry.DeclaredOnType

                if inheritedOnly && not parentIsAbstract then
                    state, parentSlotsIfBuilt, placed
                else

                let interfaceMethods =
                    entry.TypeInfo.Methods
                    |> List.filter (fun method -> method.IsVirtual && not method.IsStatic)

                ((state, parentSlotsIfBuilt, placed), interfaceMethods)
                ||> List.fold (fun (state, parentSlotsIfBuilt, placed) interfaceMethod ->
                    let interfaceComparand : TypeConcretization.SignatureComparand =
                        {
                            Signature = interfaceMethod.Signature
                            AssemblyFullName = interfaceMethod.DeclaringAssemblyFullName
                            DeclaringTypeGenerics = entry.Substitution
                        }

                    let interfaceMethodKey : SlotIdentity =
                        interfaceMethod.DeclaringAssemblyFullName, interfaceMethod.IdentityKey

                    let key = index, interfaceMethodKey

                    let state, implementedAbove =
                        match parentHandle with
                        | Some parentHandle when inheritedOnly ->
                            let state, slot =
                                findSlotOnChain
                                    loggerFactory
                                    baseClassTypes
                                    operation
                                    state
                                    parentHandle
                                    true
                                    false
                                    entry.Interface
                                    interfaceMethodKey

                            state, slot.IsSome
                        | _ -> state, false

                    if implementedAbove then
                        state, parentSlotsIfBuilt, placed
                    else

                    let state, own =
                        firstEquivalent state interfaceComparand interfaceMethod.Name ownComparands

                    match own with
                    | Some method -> state, parentSlotsIfBuilt, placed.SetItem (key, slotOfOwnMethod method)
                    | None when entry.ImplementedByParent ->
                        // "Explicit re-declaration of an inherited interface will try to match only
                        // newslot methods [of this type]": an unmatched slot keeps the parent's mapping,
                        // which dispatch reaches by walking up.
                        state, parentSlotsIfBuilt, placed
                    | None ->

                    let state, slots =
                        match parentSlotsIfBuilt with
                        | Some slots -> state, slots
                        | None -> parentSlots state

                    // Backwards through the parent's slots, "to find the most derived method".
                    let candidates =
                        slots
                        |> List.indexed
                        |> List.rev
                        |> List.filter (fun (_, slot) -> slot.Method.IsVirtual && slot.Method.IsPublic)
                        |> List.map (fun (i, slot) ->
                            i,
                            slot.Method.Name,
                            {
                                TypeConcretization.SignatureComparand.Signature = slot.Method.Signature
                                TypeConcretization.SignatureComparand.AssemblyFullName =
                                    slot.DeclaredBy.AssemblyFullName
                                TypeConcretization.SignatureComparand.DeclaringTypeGenerics =
                                    slot.DeclaredBy.Substitution
                            }
                        )

                    let state, inheritedSlot =
                        firstEquivalent state interfaceComparand interfaceMethod.Name candidates

                    match inheritedSlot with
                    | Some slot -> state, Some slots, placed.SetItem (key, slot)
                    | None -> state, Some slots, placed
                )
            )

        // `PlaceMethodImpls` runs after `PlaceInterfaceMethods`, so a MethodImpl replaces whatever
        // placement mapped (`AddMethodImplDispatchMapping`). It maps every interface-map entry at the
        // named instantiation.
        let methodImpls =
            typeInfo.MethodImpls
            |> Seq.sortBy (fun (KeyValue (handle, _)) ->
                MetadataToken.toInt (MetadataToken.MethodImplementation handle)
            )
            |> Seq.map (fun (KeyValue (_, impl)) -> impl)
            |> List.ofSeq

        let state, placed, _ =
            ((state, placed, ImmutableHashSet.Empty), methodImpls)
            ||> List.fold (fun (state, placed, alreadyImplemented) impl ->
                let state, declaration =
                    interfaceMethodImplDeclaration
                        loggerFactory
                        baseClassTypes
                        operation
                        state
                        ty
                        owner.Description
                        assembly
                        impl.Declaration

                match declaration with
                | None -> state, placed, alreadyImplemented
                | Some (interfaceHandle, interfaceMethod) ->

                let body =
                    match impl.Body with
                    | MetadataToken.MethodDef body -> assembly.Methods.[body]
                    | other ->
                        failwith
                            $"TODO: %s{operation}: a MethodImpl on %s{owner.Description} names its body with %O{other} rather than a MethodDef of the type itself"

                let indices =
                    interfaceMap
                    |> List.indexed
                    |> List.choose (fun (index, entry) ->
                        if entry.Interface = interfaceHandle then
                            Some index
                        else
                            None
                    )

                if indices.IsEmpty then
                    failwith
                        $"%s{operation}: a MethodImpl on %s{owner.Description} implements a method of %O{interfaceHandle}, which is not in its interface map; CoreCLR rejects this type at load time"

                if alreadyImplemented.Contains ((interfaceHandle, interfaceMethod)) then
                    failwith
                        $"%s{operation}: two MethodImpls on %s{owner.Description} implement the same method of %O{interfaceHandle}; CoreCLR rejects this type at load time (IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES)"

                let slot = slotOfOwnMethod body

                let placed =
                    (placed, indices)
                    ||> List.fold (fun placed index -> placed.SetItem ((index, interfaceMethod), slot))

                state, placed, alreadyImplemented.Add ((interfaceHandle, interfaceMethod))
            )

        let byInterfaceMethod =
            placed
            |> Seq.map (fun (KeyValue (key, slot)) -> key, slot)
            |> Seq.groupBy (fun ((_, interfaceMethod), _) -> interfaceMethod)
            |> Seq.map (fun (interfaceMethod, entries) ->
                let entries =
                    entries
                    |> Seq.map (fun ((index, _), slot) ->
                        {
                            InterfaceDispatchEntry.Interface = interfaceMap.[index].Interface
                            InterfaceDispatchEntry.InterfaceMapIndex = index
                            InterfaceDispatchEntry.ImplementationSlot = slot
                        }
                    )
                    |> Seq.sortBy _.InterfaceMapIndex
                    |> List.ofSeq

                KeyValuePair.Create (interfaceMethod, entries)
            )
            |> ImmutableDictionary.CreateRange

        let computed =
            {
                InterfaceDispatchMap.ByInterfaceMethod = byInterfaceMethod
            }

        state.WithInterfaceDispatchMap typeHandle computed, computed

    /// `tryFindImplementationSlot`, with the variance pass optional: placement asks whether a parent
    /// already implements an instantiation *exactly*.
    and private findSlotOnChain
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (receiver : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (allowVariance : bool)
        (target : ConcreteTypeHandle)
        (interfaceMethod : SlotIdentity)
        : IlMachineState * int option
        =
        let targetHasVariance =
            allowVariance
            && (
                match IlMachineState.tryGetConcreteTypeInfo state target with
                | Some (_, targetTypeInfo) ->
                    targetTypeInfo.Generics
                    |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)
                | None -> failwith $"%s{operation}: interface %O{target} has no TypeDef row"
            )

        let rec firstCompatible (state : IlMachineState) (entries : InterfaceDispatchEntry list) =
            match entries with
            | [] -> state, None
            | entry :: rest ->
                let state, compatible =
                    IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state entry.Interface target

                if compatible then
                    state, Some entry.ImplementationSlot
                else
                    firstCompatible state rest

        let rec atLevel (state : IlMachineState) (level : ConcreteTypeHandle) : IlMachineState * int option =
            match level with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, None
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->

            let state, found =
                match IlMachineState.tryGetConcreteTypeInfo state level with
                | None ->
                    // A synthesised array has no TypeDef row and declares nothing; its interfaces
                    // come from `System.Array` above it, or from the SZ-array carve-out.
                    state, None
                | Some _ ->
                    let state, own = ownDispatchMapOf loggerFactory baseClassTypes operation state level

                    match own.ByInterfaceMethod.TryGetValue interfaceMethod with
                    | false, _ -> state, None
                    | true, entries ->
                        match entries |> List.tryFind (fun entry -> entry.Interface = target) with
                        | Some entry -> state, Some entry.ImplementationSlot
                        | None when not targetHasVariance -> state, None
                        | None -> firstCompatible state entries

            match found with
            | Some _ -> state, found
            | None when not walkBaseTypes -> state, None
            | None ->
                let state, baseType =
                    IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state level

                match baseType with
                | None -> state, None
                | Some baseType -> atLevel state baseType

        atLevel state receiver

    /// The vtable slot whose content implements `interfaceMethod` of the interface instantiation
    /// `target` on a receiver of runtime type `receiver`, or `None` if no type on the receiver's chain
    /// maps it -- in which case only a default interface body can answer.
    ///
    /// Each level is asked for an entry at `target` itself and then, if the interface is variant, for
    /// the first entry in its interface-map order that is variance-compatible with it
    /// (`MethodTable::FindEncodedMapDispatchEntry`). The passes run per level, so a variance-compatible
    /// entry on a more-derived type beats an exact one on its base.
    ///
    /// `walkBaseTypes` false asks the receiver's own level alone: the `constrained.` probe of whether
    /// a value type supplies the method itself.
    let tryFindImplementationSlot
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (receiver : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (target : ConcreteTypeHandle)
        (interfaceMethod : SlotIdentity)
        : IlMachineState * int option
        =
        findSlotOnChain loggerFactory baseClassTypes operation state receiver walkBaseTypes true target interfaceMethod
