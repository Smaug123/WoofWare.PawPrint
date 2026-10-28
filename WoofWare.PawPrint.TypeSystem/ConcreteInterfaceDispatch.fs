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
module ConcreteInterfaceDispatch =

    /// Resolve one entry of `ownerTy`'s `ImplementedInterfaces` list to the concrete interface
    /// it names, registering that instantiation in the ConcreteTypes registry if it is not
    /// already there. An interface that does not bind is the failure.
    let internal resolveImplementedInterface
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ownerTy : ConcreteType<ConcreteTypeHandle>)
        (impl : WoofWare.PawPrint.InterfaceImplementation)
        (state : TypeSystemState)
        : TypeSystemState *
          Result<
              ConcreteTypeHandle * ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>,
              TypeBindingFailure
           >
        =
        let ownerAssy =
            state._LoadedAssemblies.ByDefinitionName ownerTy.Identity.AssemblyFullName

        let implAssy =
            match state.LoadedAssembly impl.RelativeToAssembly.FullName with
            | Some assy -> assy
            | None -> ownerAssy

        let state, implTypeDefnResult =
            TypeSystemState.resolveTypeMetadataToken
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                state
                implAssy
                impl.InterfaceHandle

        match implTypeDefnResult with
        | Error failure -> state, Error failure
        | Ok (implTypeDefn, implResolvedAssy) ->

        match
            TypeSystemState.concretizeType
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                state
                implResolvedAssy.DefinitionFullName
                ownerTy.Generics
                ImmutableArray.Empty
                implTypeDefn
        with
        | state, Error failure -> state, Error failure
        | state, Ok implHandle ->

        match TypeSystemState.tryGetConcreteTypeInfo state implHandle with
        | Some (implTy, typeInfo) -> state, Ok (implHandle, implTy, typeInfo)
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
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (typeHandle : ConcreteTypeHandle)
        : TypeSystemState * Result<InterfaceMapEntry list, TypeBindingFailure>
        =
        let ty, typeInfo =
            match TypeSystemState.tryGetConcreteTypeInfo state typeHandle with
            | Some found -> found
            | None -> failwith $"%s{operation}: %O{typeHandle} has no TypeDef row, so it has no interface map"

        let owner = ConcreteMethodTable.ownerOfDefinition operation state ty.Identity

        let assembly, _ = ConcreteMethodTable.definitionMetadata operation state ty.Identity

        let state, parentHandle =
            TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state typeHandle

        match parentHandle with
        | Error failure -> state, Error failure
        | Ok parentHandle ->

        let state, inherited =
            match parentHandle with
            | None -> state, Ok []
            | Some parentHandle ->
                match interfaceMapOf loggerFactory dotnetRuntimeDirs baseClassTypes operation state parentHandle with
                | state, Error failure -> state, Error failure
                | state, Ok parentMap ->

                let state, baseOfDefinition =
                    ConcreteMethodTable.baseOfDefinition
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        owner
                        typeInfo

                match baseOfDefinition with
                | Error failure -> state, Error failure
                | Ok None ->
                    failwith
                        $"%s{operation}: %s{owner.Description} has a base type as an instantiation but none as a definition"
                | Ok (Some (baseIdentity, arguments)) ->

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

                state, Ok inherited

        match inherited with
        | Error failure -> state, Error failure
        | Ok inherited ->

        // "Checking for further expanded interfaces isn't necessary for the system module, as we can
        // rely on the C# compiler to have found all of the interfaces that the type implements"
        // (methodtablebuilder.cpp, `ExpandApproxInterface`): for a value type declared in CoreLib, an
        // interface's own parents are not expanded where it is declared. Classes are unaffected.
        let expandParents =
            not (
                ty.Identity.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
                && LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies typeInfo
            )

        let rec expand
            (declared : bool)
            (spellingAssembly : DumpedAssembly)
            (spellingContext : TypeConcretization.SubstitutionContext)
            (spellingOwner : ConcreteType<ConcreteTypeHandle>)
            (state : TypeSystemState)
            (map : InterfaceMapEntry list)
            (row : WoofWare.PawPrint.InterfaceImplementation)
            : TypeSystemState * Result<InterfaceMapEntry list, TypeBindingFailure>
            =
            match
                resolveImplementedInterface loggerFactory dotnetRuntimeDirs baseClassTypes spellingOwner row state
            with
            | state, Error failure -> state, Error failure
            | state, Ok (handle, interfaceTy, interfaceTypeInfo) ->

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
            let rec findExisting
                (state : TypeSystemState)
                (i : int)
                (entries : InterfaceMapEntry list)
                : TypeSystemState * Result<int option, TypeBindingFailure>
                =
                match entries with
                | [] -> state, Ok None
                | entry :: rest when entry.Interface <> handle -> findExisting state (i + 1) rest
                | entry :: rest ->
                    match
                        TypeSystemState.substitutionsEquivalent
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            entry.Substitution
                            substitution
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok true -> state, Ok (Some i)
                    | state, Ok false -> findExisting state (i + 1) rest

            match findExisting state 0 map with
            | state, Error failure -> state, Error failure
            | state, Ok (Some existing) ->
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
                    |> Ok
                else
                    state, Ok map
            | state, Ok None ->

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
                state, Ok map
            else

            let interfaceAssembly, _ =
                ConcreteMethodTable.definitionMetadata operation state interfaceTy.Identity

            (state, map, interfaceTypeInfo.ImplementedInterfaces)
            |||> StateResult.fold (expand false interfaceAssembly substitution interfaceTy)

        (state, inherited, typeInfo.ImplementedInterfaces)
        |||> StateResult.fold (expand true assembly owner.Substitution ty)

    /// The interface method a MethodImpl declaration names, together with the interface instantiation
    /// it names it on -- closed, and read in the vocabulary of the owner's generic definition, whose
    /// substitution is `ownerSubstitution`. Only static interface methods are named when `statics`
    /// is set, and only instance ones when not; `None` when the declaration is not an interface method
    /// of that kind. An instance declaration that is not an interface method makes the row a vtable
    /// write (see `ConcreteMethodTable.contentVtableOfDefinition`); a static one implements a static
    /// virtual, which `StaticVirtualDispatch` reads the rows for, and which is not in this map. A type
    /// the declaration names that does not bind is the failure, as is a declaration naming a method
    /// its interface does not declare (`MethodImplDeclarationAbsent`), and one naming a generic
    /// interface's method by MethodDef, which is not modelled.
    let internal interfaceMethodImplDeclaration
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (statics : bool)
        (state : TypeSystemState)
        (ownerTy : ConcreteType<ConcreteTypeHandle>)
        (ownerDescription : string)
        (ownerSubstitution : TypeConcretization.SubstitutionContext)
        (assembly : DumpedAssembly)
        (declaration : MetadataToken)
        : TypeSystemState *
          Result<(ConcreteTypeHandle * TypeConcretization.SubstitutionContext * SlotIdentity) option, TypeBindingFailure>
        =
        let concretizeParent
            (state : TypeSystemState)
            (parent : MetadataToken)
            : TypeSystemState *
              Result<
                  ConcreteTypeHandle * ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>,
                  TypeBindingFailure
               >
            =
            let state, parentTypeDefnResult =
                TypeSystemState.resolveTypeMetadataToken
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    assembly
                    parent

            match parentTypeDefnResult with
            | Error failure -> state, Error failure
            | Ok (parentTypeDefn, parentAssembly) ->

            match
                TypeSystemState.concretizeType
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    parentAssembly.DefinitionFullName
                    ownerTy.Generics
                    ImmutableArray.Empty
                    parentTypeDefn
            with
            | state, Error failure -> state, Error failure
            | state, Ok parentHandle ->

            match TypeSystemState.tryGetConcreteTypeInfo state parentHandle with
            | Some (parentTy, parentTypeInfo) -> state, Ok (parentHandle, parentTy, parentTypeInfo)
            | None ->
                failwith
                    $"%s{operation}: a MethodImpl on %s{ownerDescription} names its declaration on %O{parentHandle}, which has no TypeDef row"

        match declaration with
        | MetadataToken.MethodDef handle ->
            let method = assembly.Methods.[handle]

            if method.IsStatic <> statics then
                state, Ok None
            else

            let declaringTypeInfo =
                assembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

            if not declaringTypeInfo.IsInterface then
                state, Ok None
            elif not declaringTypeInfo.Generics.IsEmpty then
                // A MethodDef token carries no instantiation, so it can name a generic interface only
                // at its typical instantiation, which no class implements.
                state,
                Error (
                    TypeBindingFailure.Unmodelled
                        {
                            Diagnostic =
                                $"%s{operation}: a MethodImpl on %s{ownerDescription} names %s{method.Name} on the generic interface %s{declaringTypeInfo.Name} by MethodDef, which carries no instantiation; that is not modelled"
                        }
                )
            else
                match
                    concretizeParent state (MetadataToken.TypeDefinition method.RequiredDeclaringType.Definition.Get)
                with
                | state, Error failure -> state, Error failure
                | state, Ok (interfaceHandle, _, _) ->

                state,
                Ok (
                    Some (
                        interfaceHandle,
                        {
                            TypeConcretization.SubstitutionContext.Arguments = ImmutableArray.Empty
                        },
                        (assembly.DefinitionFullName, method.IdentityKey)
                    )
                )
        | MetadataToken.MemberReference handle ->
            let memberRef = assembly.Members.[handle]

            match memberRef.Signature with
            | MemberSignature.Field _ ->
                failwith $"%s{operation}: a MethodImpl on %s{ownerDescription} names a field as its declaration"
            | MemberSignature.Method signature ->

            if signature.Header.Get.IsInstance = statics then
                state, Ok None
            else

            match memberRef.Parent with
            | MetadataToken.TypeDefinition _
            | MetadataToken.TypeReference _
            | MetadataToken.TypeSpecification _ ->
                match concretizeParent state memberRef.Parent with
                | state, Error failure -> state, Error failure
                | state, Ok (interfaceHandle, interfaceTy, interfaceTypeInfo) ->

                if not interfaceTypeInfo.IsInterface then
                    state, Ok None
                else

                // Compared in the interface's own open vocabulary, as `FindDeclMethodOnClassInHierarchy`
                // compares the named type: neither side substituted, nominal tokens resolved against
                // whichever assembly spelled them.
                let openContext =
                    TypeConcretization.SubstitutionContext.forDefinition
                        interfaceTy.Identity
                        interfaceTypeInfo.Generics.Length

                let state, matches =
                    (state, [], interfaceTypeInfo.Methods)
                    |||> StateResult.fold (fun state acc candidate ->
                        if candidate.Name <> memberRef.PrettyName || candidate.IsStatic <> statics then
                            state, Ok acc
                        else

                        match
                            TypeSystemState.signaturesEquivalent
                                loggerFactory
                                dotnetRuntimeDirs
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
                        with
                        | state, Error failure -> state, Error failure
                        | state, Ok equivalent -> state, Ok (if equivalent then acc @ [ candidate ] else acc)
                    )

                match matches with
                | Error failure -> state, Error failure
                | Ok matches ->

                // The first in declaration order (`FindDeclMethodOnInterfaceEntry`). Two can match
                // legitimately: overloads spelled against two assemblies become one signature once
                // one assembly forwards the type to the other.
                match matches with
                | interfaceMethod :: _ ->
                    let substitution =
                        spelledInterfaceSubstitution
                            operation
                            assembly
                            ownerSubstitution
                            interfaceTypeInfo
                            memberRef.Parent

                    state,
                    Ok (
                        Some (
                            interfaceHandle,
                            substitution,
                            (interfaceMethod.DeclaringAssemblyFullName, interfaceMethod.IdentityKey)
                        )
                    )
                | [] ->
                    // `FindDeclMethodOnInterfaceEntry` searches the named interface alone, and its
                    // finding nothing is a `MissingMethodException` (methodtablebuilder.cpp:5939-5968).
                    let miss : MethodImplDeclarationMiss =
                        {
                            Type = ownerTy.Identity
                            Declaration = handle
                            Name = memberRef.PrettyName
                        }

                    state, Error (TypeBindingFailure.MethodImplDeclarationAbsent miss)
            | _ ->
                // A ModuleRef or MethodDef parent (the vararg case) cannot name an interface method.
                state, Ok None
        | other ->
            failwith
                $"%s{operation}: a MethodImpl on %s{ownerDescription} names its declaration with the token %O{other}, which is neither a MethodDef nor a MemberRef; ECMA-335 II.22.27 permits only those two"

    /// The method a MethodImpl body names, which must be one the owner itself declares. A MethodDef
    /// row is that method; a MemberRef is resolved against the owner's own methods by name and
    /// signature in its open vocabulary, as `EnumerateMethodImpls` normalises one. A type on the way
    /// that does not bind is the failure, and a body that names anything else is an unmodelled one:
    /// CoreCLR refuses to load the owner (`IDS_CLASSLOAD_MI_ILLEGAL_BODY`).
    let internal methodImplBody
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (owner : SlotOwner)
        (ownerTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (assembly : DumpedAssembly)
        (body : MetadataToken)
        : TypeSystemState *
          Result<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>, TypeBindingFailure>
        =
        let refuse (state : TypeSystemState) (detail : string) : TypeSystemState * Result<'a, TypeBindingFailure> =
            state,
            Error (
                TypeBindingFailure.Unmodelled
                    {
                        Diagnostic =
                            $"%s{operation}: a MethodImpl on %s{owner.Description} names its body with %O{body}, %s{detail}; ECMA-335 II.22.27 requires the body to be a method of the type itself, and CoreCLR rejects this type at load time with a TypeLoadException (IDS_CLASSLOAD_MI_ILLEGAL_BODY), which is not modelled"
                    }
            )

        match body with
        | MetadataToken.MethodDef handle ->
            let method = assembly.Methods.[handle]

            if method.RequiredDeclaringType.Identity <> owner.Identity then
                refuse state "a method of another type"
            else
                state, Ok method
        | MetadataToken.MemberReference handle ->
            let memberRef = assembly.Members.[handle]

            match memberRef.Signature with
            | MemberSignature.Field _ -> refuse state "which names a field"
            | MemberSignature.Method signature ->

            let state, parentIdentity =
                match memberRef.Parent with
                | MetadataToken.TypeDefinition parent ->
                    state, Ok (ResolvedTypeIdentity.ofDefinitionInAssembly assembly.DefinitionFullName parent)
                | MetadataToken.TypeReference parent ->
                    match
                        TypeSystemState.resolveTypeFromRef
                            loggerFactory
                            dotnetRuntimeDirs
                            assembly
                            assembly.TypeRefs.[parent]
                            ImmutableArray.Empty
                            state
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok (_, resolved) ->
                        state,
                        Ok (
                            ResolvedTypeIdentity.ofDefinitionInAssembly resolved.AssemblyFullName resolved.TypeDefHandle
                        )
                | MetadataToken.TypeSpecification parent ->
                    let spelling =
                        match assembly.TypeSpecs.[parent].Signature with
                        | TypeDefn.GenericInstantiation (generic, _) -> generic
                        | nominal -> nominal

                    ConcreteMethodTable.nominalIdentityOfSpelling
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        assembly
                        spelling
                | _ -> refuse state "whose parent is not a type"

            match parentIdentity with
            | Error failure -> state, Error failure
            | Ok parentIdentity when parentIdentity <> owner.Identity -> refuse state "a method of another type"
            | Ok _ ->

            let rec first
                (state : TypeSystemState)
                candidates
                : TypeSystemState *
                  Result<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>, TypeBindingFailure>
                =
                match candidates with
                | [] -> refuse state "which names no method the type declares"
                | (candidate : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) :: rest ->
                    if candidate.Name <> memberRef.PrettyName then
                        first state rest
                    else

                    match
                        TypeSystemState.signaturesEquivalent
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            state
                            false
                            {
                                Signature = signature
                                AssemblyFullName = assembly.DefinitionFullName
                                DeclaringTypeGenerics = owner.Substitution
                            }
                            {
                                Signature = candidate.Signature
                                AssemblyFullName = owner.AssemblyFullName
                                DeclaringTypeGenerics = owner.Substitution
                            }
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok true -> state, Ok candidate
                    | state, Ok false -> first state rest

            first state ownerTypeInfo.Methods
        | other ->
            failwith
                $"BUG: %s{operation}: a MethodImpl on %s{owner.Description} names its body with %O{other}, but a MethodImpl body is a MethodDefOrRef coded index"

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
    ///
    /// A type the map reaches that does not bind is the failure, returned with what loaded before it;
    /// only a computed map is memoised.
    let rec ownDispatchMapOf
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (typeHandle : ConcreteTypeHandle)
        : TypeSystemState * Result<InterfaceDispatchMap, TypeBindingFailure>
        =
        match Map.tryFind typeHandle state._InterfaceDispatchMaps with
        | Some cached -> state, Ok cached
        | None ->

        let ty, typeInfo =
            match TypeSystemState.tryGetConcreteTypeInfo state typeHandle with
            | Some found -> found
            | None -> failwith $"%s{operation}: %O{typeHandle} has no TypeDef row, so it has no dispatch map"

        let owner = ConcreteMethodTable.ownerOfDefinition operation state ty.Identity

        let assembly, _ = ConcreteMethodTable.definitionMetadata operation state ty.Identity

        match interfaceMapOf loggerFactory dotnetRuntimeDirs baseClassTypes operation state typeHandle with
        | state, Error failure -> state, Error failure
        | state, Ok interfaceMap ->

        let state, parentHandle =
            TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state typeHandle

        match parentHandle with
        | Error failure -> state, Error failure
        | Ok parentHandle ->

        let parentIsAbstract =
            match parentHandle with
            | None -> false
            | Some parentHandle ->
                match TypeSystemState.tryGetConcreteTypeInfo state parentHandle with
                | Some (_, parentTypeInfo) ->
                    parentTypeInfo.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Abstract
                | None -> false

        match
            ConcreteMethodTable.dispatchTableOfClosed
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                operation
                state
                typeHandle
        with
        | state, Error failure -> state, Error failure
        | _, Ok None -> failwith $"%s{operation}: %s{owner.Description} has no method table"
        | state, Ok (Some table) ->

        let slotOfOwnMethod (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) : int =
            match table.SlotOfDeclaration.TryGetValue ((method.DeclaringAssemblyFullName, method.IdentityKey)) with
            | true, slot -> slot
            | false, _ ->
                // `MethodTableLayout.PlacedVtable` documents the one way a declared virtual can own no slot: two of
                // the type's own virtuals coinciding once the base's arguments are substituted.
                failwith
                    $"%s{operation}: %s{owner.Description} declares the virtual method %s{method.Name}, which owns no slot in its method table"

        // The candidates `PlaceInterfaceMethods` collects from `DeclaredMethodIterator`: "non-publics
        // and statics can legally be exposed via an interface, but only through methodImpls".
        let ownCandidates =
            MethodTableLayout.declaredMethodsOf operation owner typeInfo
            |> List.map fst
            |> List.filter (fun method -> method.IsVirtual && method.IsPublic)

        // The parent's slots as `bmtParent` holds them -- each slot's declaration, not its content --
        // read in this type's vocabulary. Only needed for an interface the parent does not implement,
        // so it is not built until one asks.
        let parentSlots (state : TypeSystemState) : TypeSystemState * Result<VtableSlot list, TypeBindingFailure> =
            let state, baseOfDefinition =
                ConcreteMethodTable.baseOfDefinition
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    state
                    owner
                    typeInfo

            match baseOfDefinition with
            | Error failure -> state, Error failure
            | Ok None -> state, Ok []
            | Ok (Some (baseIdentity, arguments)) ->
                let state, parentVtable =
                    ConcreteMethodTable.vtableOfDefinition
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        baseIdentity

                state,
                parentVtable
                |> Result.map (
                    List.map (fun slot ->
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
                )

        let equivalent
            (state : TypeSystemState)
            (interfaceComparand : TypeConcretization.SignatureComparand)
            (interfaceMethodName : string)
            (candidateName : string)
            (candidateComparand : TypeConcretization.SignatureComparand)
            : TypeSystemState * Result<bool, TypeBindingFailure>
            =
            // `MethodSignature::Equivalent`: the names, then the whole signature, return type included.
            if interfaceMethodName <> candidateName then
                state, Ok false
            else
                match
                    TypeSystemState.signaturesEquivalent
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        false
                        interfaceComparand
                        candidateComparand
                with
                | state, Error failure -> state, Error failure
                | state, Ok equivalent -> state, Ok equivalent

        let rec firstEquivalent
            (state : TypeSystemState)
            (interfaceComparand : TypeConcretization.SignatureComparand)
            (interfaceMethodName : string)
            (candidates : ('candidate * string * TypeConcretization.SignatureComparand) list)
            : TypeSystemState * Result<'candidate option, TypeBindingFailure>
            =
            match candidates with
            | [] -> state, Ok None
            | (candidate, name, comparand) :: rest ->
                match equivalent state interfaceComparand interfaceMethodName name comparand with
                | state, Error failure -> state, Error failure
                | state, Ok true -> state, Ok (Some candidate)
                | state, Ok false -> firstEquivalent state interfaceComparand interfaceMethodName rest

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
        let state, placed =
            (state, (None, ImmutableDictionary.Empty), List.indexed interfaceMap)
            |||> StateResult.fold (fun state (parentSlotsIfBuilt, placed) (index, entry) ->
                // Inherited and not re-declared, so the parent's map answers -- unless the parent is
                // abstract and so may have left a method to a default body.
                let inheritedOnly = entry.ImplementedByParent && not entry.DeclaredOnType

                if inheritedOnly && not parentIsAbstract then
                    state, Ok (parentSlotsIfBuilt, placed)
                else

                let interfaceMethods =
                    entry.TypeInfo.Methods
                    |> List.filter (fun method -> method.IsVirtual && not method.IsStatic)

                (state, (parentSlotsIfBuilt, placed), interfaceMethods)
                |||> StateResult.fold (fun state (parentSlotsIfBuilt, placed) interfaceMethod ->
                    let interfaceComparand : TypeConcretization.SignatureComparand =
                        {
                            Signature = interfaceMethod.Signature
                            AssemblyFullName = interfaceMethod.DeclaringAssemblyFullName
                            DeclaringTypeGenerics = entry.Substitution
                        }

                    let interfaceMethodKey : SlotIdentity =
                        interfaceMethod.DeclaringAssemblyFullName, interfaceMethod.IdentityKey

                    let key = index, interfaceMethodKey

                    // Asked of the entry by its position, not its closed handle: an ancestor's map is a
                    // prefix of this one, so the position names the same entry at every level, whereas
                    // two distinct entries can close to one handle.
                    let state, implementedAbove =
                        match parentHandle with
                        | Some parentHandle when inheritedOnly ->
                            isMappedOnChain
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                operation
                                state
                                parentHandle
                                index
                                interfaceMethodKey
                        | _ -> state, Ok false

                    match implementedAbove with
                    | Error failure -> state, Error failure
                    | Ok true -> state, Ok (parentSlotsIfBuilt, placed)
                    | Ok false ->

                    match firstEquivalent state interfaceComparand interfaceMethod.Name ownComparands with
                    | state, Error failure -> state, Error failure
                    | state, Ok (Some method) ->
                        state, Ok (parentSlotsIfBuilt, placed.SetItem (key, slotOfOwnMethod method))
                    | state, Ok None when entry.ImplementedByParent ->
                        // "Explicit re-declaration of an inherited interface will try to match only
                        // newslot methods [of this type]": an unmatched slot keeps the parent's mapping,
                        // which dispatch reaches by walking up.
                        state, Ok (parentSlotsIfBuilt, placed)
                    | state, Ok None ->

                    let state, slots =
                        match parentSlotsIfBuilt with
                        | Some slots -> state, Ok slots
                        | None -> parentSlots state

                    match slots with
                    | Error failure -> state, Error failure
                    | Ok slots ->

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

                    match firstEquivalent state interfaceComparand interfaceMethod.Name candidates with
                    | state, Error failure -> state, Error failure
                    | state, Ok (Some slot) -> state, Ok (Some slots, placed.SetItem (key, slot))
                    | state, Ok None -> state, Ok (Some slots, placed)
                )
            )

        match placed with
        | Error failure -> state, Error failure
        | Ok (_, placed) ->

        // `PlaceMethodImpls` runs after `PlaceInterfaceMethods`, so a MethodImpl replaces whatever
        // placement mapped (`AddMethodImplDispatchMapping`).
        let methodImpls =
            typeInfo.MethodImpls
            |> Seq.sortBy (fun (KeyValue (handle, _)) ->
                MetadataToken.toInt (MetadataToken.MethodImplementation handle)
            )
            |> Seq.map (fun (KeyValue (_, impl)) -> impl)
            |> List.ofSeq

        let state, placed =
            (state, (placed, ImmutableDictionary.Empty), methodImpls)
            |||> StateResult.fold (fun state (placed, alreadyImplemented) impl ->
                match
                    interfaceMethodImplDeclaration
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        false
                        state
                        ty
                        owner.Description
                        owner.Substitution
                        assembly
                        impl.Declaration
                with
                | state, Error failure -> state, Error failure
                | state, Ok None -> state, Ok (placed, alreadyImplemented)
                | state, Ok (Some (interfaceHandle, declaredSubstitution, interfaceMethod)) ->

                match
                    methodImplBody
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        owner
                        typeInfo
                        assembly
                        impl.Body
                with
                | state, Error failure -> state, Error failure
                | state, Ok body ->

                // The entry the declaration names, identified on the generic definition as the interface
                // map itself is: the first that is the same instantiation there
                // (`ComputeDispatchMapTypeIDs`), not every entry that happens to close to the same type.
                let rec namedEntry
                    (state : TypeSystemState)
                    (entries : (int * InterfaceMapEntry) list)
                    : TypeSystemState * Result<int option, TypeBindingFailure>
                    =
                    match entries with
                    | [] -> state, Ok None
                    | (_, entry) :: rest when entry.Interface <> interfaceHandle -> namedEntry state rest
                    | (index, entry) :: rest ->
                        match
                            TypeSystemState.substitutionsEquivalent
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                entry.Substitution
                                declaredSubstitution
                        with
                        | state, Error failure -> state, Error failure
                        | state, Ok true -> state, Ok (Some index)
                        | state, Ok false -> namedEntry state rest

                match namedEntry state (List.indexed interfaceMap) with
                | state, Error failure -> state, Error failure
                | state, Ok index ->

                let index =
                    match index with
                    | Some index -> index
                    | None ->
                        failwith
                            $"%s{operation}: a MethodImpl on %s{owner.Description} implements a method of %O{interfaceHandle}, which is not in its interface map; CoreCLR rejects this type at load time"

                // A second row naming the same body is the same mapping; only a different body is a
                // conflict (`AddMethodImplDispatchMapping`).
                match alreadyImplemented.TryGetValue ((index, interfaceMethod)) with
                | true, previous when previous <> body.IdentityKey ->
                    failwith
                        $"%s{operation}: two MethodImpls on %s{owner.Description} implement the same method of %O{interfaceHandle} with different bodies; CoreCLR rejects this type at load time (IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES)"
                | _ -> ()

                let slot = slotOfOwnMethod body

                state,
                Ok (
                    placed.SetItem ((index, interfaceMethod), slot),
                    alreadyImplemented.SetItem ((index, interfaceMethod), body.IdentityKey)
                )
            )

        match placed with
        | Error failure -> state, Error failure
        | Ok (placed, _) ->

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

        state.WithInterfaceDispatchMap typeHandle computed, Ok computed

    /// Does `level` or any type above it map `interfaceMethod` through the interface-map entry at
    /// `index`? Positions agree down a chain, because each type's interface map begins with its
    /// parent's.
    and private isMappedOnChain
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (level : ConcreteTypeHandle)
        (index : int)
        (interfaceMethod : SlotIdentity)
        : TypeSystemState * Result<bool, TypeBindingFailure>
        =
        match ownDispatchMapOf loggerFactory dotnetRuntimeDirs baseClassTypes operation state level with
        | state, Error failure -> state, Error failure
        | state, Ok own ->

        let mapped =
            match own.ByInterfaceMethod.TryGetValue interfaceMethod with
            | true, entries -> entries |> List.exists (fun entry -> entry.InterfaceMapIndex = index)
            | false, _ -> false

        if mapped then
            state, Ok true
        else
            let state, baseType =
                TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state level

            match baseType with
            | Error failure -> state, Error failure
            | Ok baseType ->

            match baseType with
            | None -> state, Ok false
            | Some baseType ->
                isMappedOnChain
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    state
                    baseType
                    index
                    interfaceMethod

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
    ///
    /// A type the search reaches that does not bind is the failure, returned with what loaded before it.
    let tryFindImplementationSlot
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : TypeSystemState)
        (receiver : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (target : ConcreteTypeHandle)
        (interfaceMethod : SlotIdentity)
        : TypeSystemState * Result<int option, TypeBindingFailure>
        =
        let targetHasVariance =
            match TypeSystemState.tryGetConcreteTypeInfo state target with
            | Some (_, targetTypeInfo) ->
                targetTypeInfo.Generics
                |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)
            | None -> failwith $"%s{operation}: interface %O{target} has no TypeDef row"

        let rec firstCompatible
            (state : TypeSystemState)
            (entries : InterfaceDispatchEntry list)
            : TypeSystemState * Result<int option, TypeBindingFailure>
            =
            match entries with
            | [] -> state, Ok None
            | entry :: rest ->
                match
                    TypeAssignability.isConcreteTypeAssignableTo
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        entry.Interface
                        target
                with
                | state, Error failure -> state, Error failure
                | state, Ok true -> state, Ok (Some entry.ImplementationSlot)
                | state, Ok false -> firstCompatible state rest

        let rec atLevel
            (state : TypeSystemState)
            (level : ConcreteTypeHandle)
            : TypeSystemState * Result<int option, TypeBindingFailure>
            =
            match level with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, Ok None
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->

            let state, found =
                match TypeSystemState.tryGetConcreteTypeInfo state level with
                | None ->
                    // A synthesised array has no TypeDef row and declares nothing; its interfaces
                    // come from `System.Array` above it, or from the SZ-array carve-out.
                    state, Ok None
                | Some _ ->
                    match ownDispatchMapOf loggerFactory dotnetRuntimeDirs baseClassTypes operation state level with
                    | state, Error failure -> state, Error failure
                    | state, Ok own ->

                    match own.ByInterfaceMethod.TryGetValue interfaceMethod with
                    | false, _ -> state, Ok None
                    | true, entries ->
                        match entries |> List.tryFind (fun entry -> entry.Interface = target) with
                        | Some entry -> state, Ok (Some entry.ImplementationSlot)
                        | None when not targetHasVariance -> state, Ok None
                        | None -> firstCompatible state entries

            match found with
            | Error failure -> state, Error failure
            | Ok (Some slot) -> state, Ok (Some slot)
            | Ok None when not walkBaseTypes -> state, Ok None
            | Ok None ->
                let state, baseType =
                    TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state level

                match baseType with
                | Error failure -> state, Error failure
                | Ok baseType ->

                match baseType with
                | None -> state, Ok None
                | Some baseType -> atLevel state baseType

        atLevel state receiver
