namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// What a MemberRef's parent token names, as <c>MemberLoader::GetDescFromMemberRef</c> reads it
/// before looking for the member.
/// </summary>
[<RequireQualifiedAccess>]
type MemberReferenceParent =
    /// A type definition, with any instantiation, custom modifier or primitive spelling read
    /// through to the definition it names.
    | Nominal of ResolvedTypeIdentity
    /// An array type, whose members the runtime supplies. Custom modifiers are stripped.
    | Array of TypeDefn
    /// A type variable of the context using the reference, which only an instantiation decides.
    | TypeVariable
    /// A MethodDef of this module: a vararg call site naming its definition.
    | VarArgDefinition of MethodDefinitionHandle
    /// CoreCLR's load of the parent reaches a type reference that names no type in the assembly it
    /// is scoped to, so binding the member throws <c>TypeLoadException</c>, whatever the member. The
    /// miss is the first such reference that load reaches.
    | Unresolved of TypeResolutionMiss

[<RequireQualifiedAccess>]
module MemberReferenceParent =

    /// The definition a type spelling in `spellingAssembly` names, looking through custom modifiers
    /// and instantiations, and reading a primitive as its CoreLib type.
    let private identityOfSpelling
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<ResolvedTypeIdentity, TypeResolutionMiss>
        =
        let ofReference (typeRef : TypeRef) =
            TypeResolution.resolveTypeRefIdentity loggerFactory dotnetRuntimeDirs spellingAssembly typeRef assemblies

        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, _) ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromDefinition (identity, _) -> assemblies, Ok identity
            | TypeDefn.FromReference (typeRef, _) -> ofReference typeRef
            | other ->
                failwith
                    $"An instantiation in %s{spellingAssembly.DefinitionFullName} applies arguments to %O{other}, which names no generic definition"
        | TypeDefn.FromDefinition (identity, _) -> assemblies, Ok identity
        | TypeDefn.FromReference (typeRef, _) -> ofReference typeRef
        | TypeDefn.PrimitiveType primitive ->
            assemblies, Ok (BaseClassTypes.ofPrimitive baseClassTypes primitive).Identity
        | other ->
            failwith $"Expected a type with members in %s{spellingAssembly.DefinitionFullName}, but got %O{other}"

    /// The CoreCLR load levels a walk distinguishes.
    [<RequireQualifiedAccess>]
    type private LoadLevel =
        /// `CLASS_LOAD_APPROXPARENTS`.
        | Approximate
        /// `CLASS_LOAD_EXACTPARENTS`.
        | ExactParents
        /// `CLASS_LOADED`, through `MethodTable::DoFullyLoad`.
        | Full

    /// The type definitions a walk of a parent's load has brought to each of CoreCLR's load levels,
    /// and the assemblies loaded on the way.
    type private LoadWalk =
        {
            Assemblies : LoadedAssemblies
            /// Brought to `CLASS_LOAD_APPROXPARENTS`.
            Approximate : Set<ResolvedTypeIdentity>
            /// Brought to `CLASS_LOAD_EXACTPARENTS`.
            Exact : Set<ResolvedTypeIdentity>
            /// Through `MethodTable::DoFullyLoad`.
            Full : Set<ResolvedTypeIdentity>
        }

    /// Whether a signature element is `ELEMENT_TYPE_VALUETYPE`, or an instantiation of one: what
    /// decides whether `MethodTableBuilder` and `DoFullyLoad` load a field's type, and whether an
    /// approximate load keeps a generic argument rather than replacing it by `Object`. Custom
    /// modifiers are looked through.
    let rec private isValueTypeSpelling (spelling : TypeDefn) : bool =
        match spelling with
        | TypeDefn.Modified modified -> isValueTypeSpelling modified.Unmodified
        | TypeDefn.FromReference (_, SignatureTypeKind.ValueType)
        | TypeDefn.FromDefinition (_, SignatureTypeKind.ValueType) -> true
        | TypeDefn.GenericInstantiation (root, _) -> isValueTypeSpelling root
        | _ -> false

    /// The first of `steps` to find a type that cannot be loaded, running each on the walk the one
    /// before it left.
    let rec private firstMiss
        (steps : (LoadWalk -> LoadWalk * TypeResolutionMiss option) list)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        match steps with
        | [] -> walk, None
        | step :: rest ->
            match step walk with
            | walk, None -> firstMiss rest walk
            | walk, Some miss -> walk, Some miss

    /// Run `step`, unless an earlier one found a miss.
    let private andThen
        (step : LoadWalk -> LoadWalk * TypeResolutionMiss option)
        (walk : LoadWalk, miss : TypeResolutionMiss option)
        : LoadWalk * TypeResolutionMiss option
        =
        match miss with
        | Some miss -> walk, Some miss
        | None -> step walk

    /// The definition a nominal spelling in `spellingAssembly` names, or why there is none.
    let private definitionOf
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * Result<ResolvedTypeIdentity, TypeResolutionMiss>
        =
        match spelling with
        | TypeDefn.FromDefinition (identity, _) -> walk, Ok identity
        | TypeDefn.FromReference (typeRef, _) ->
            let assemblies, result =
                TypeResolution.resolveTypeRefIdentity
                    loggerFactory
                    dotnetRuntimeDirs
                    spellingAssembly
                    typeRef
                    walk.Assemblies

            { walk with
                Assemblies = assemblies
            },
            result
        | other -> failwith $"BUG: %O{other} is not a nominal spelling"

    /// A type definition's base, as a spelling in its own assembly.
    let private baseSpelling
        (definedIn : DumpedAssembly)
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeDefn option
        =
        match definition.BaseType with
        | None -> None
        | Some (BaseTypeInfo.TypeDef handle) ->
            Some (TypeDefn.FromDefinition (definedIn.TypeDefs.[handle].Identity, SignatureTypeKind.Class))
        | Some (BaseTypeInfo.TypeRef handle) ->
            Some (TypeDefn.FromReference (definedIn.TypeRefs.[handle], SignatureTypeKind.Class))
        | Some (BaseTypeInfo.TypeSpec handle) -> Some definedIn.TypeSpecs.[handle].Signature

    /// A type definition's interface implementations, in metadata order, as spellings in its own
    /// assembly.
    let private interfaceSpellings
        (definedIn : DumpedAssembly)
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeDefn list
        =
        definition.ImplementedInterfaces
        |> Seq.map (fun implementation ->
            match implementation.InterfaceHandle with
            | MetadataToken.TypeDefinition handle ->
                TypeDefn.FromDefinition (definedIn.TypeDefs.[handle].Identity, SignatureTypeKind.Class)
            | MetadataToken.TypeReference handle ->
                TypeDefn.FromReference (definedIn.TypeRefs.[handle], SignatureTypeKind.Class)
            | MetadataToken.TypeSpecification handle -> definedIn.TypeSpecs.[handle].Signature
            | other ->
                failwith
                    $"%s{definition.Namespace}.%s{definition.Name} in %s{definedIn.DefinitionFullName} implements %O{other}, which ECMA-335 does not permit as an interface"
        )
        |> List.ofSeq

    /// The types of a type definition's value-type fields that have a `FieldDesc` (a literal has
    /// none), in `FieldDesc` order: its instance fields, then, when `withStatics`, its ordinary
    /// static fields, then its `[ThreadStatic]` ones, each in metadata order.
    let private valueTypeFields
        (withStatics : bool)
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeDefn list
        =
        let fields =
            definition.Fields
            |> List.filter (fun field ->
                not (field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal)
                && isValueTypeSpelling field.Signature
            )

        let isStatic (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) =
            field.Attributes.HasFlag System.Reflection.FieldAttributes.Static

        let instance = fields |> List.filter (isStatic >> not)

        let statics =
            if withStatics then
                let statics = fields |> List.filter isStatic

                (statics |> List.filter (fun field -> not field.IsThreadStatic))
                @ (statics |> List.filter (fun field -> field.IsThreadStatic))
            else
                []

        instance @ statics |> List.map (fun field -> field.Signature)

    /// The element types a constructed spelling is loaded through: an array's or pointer's element,
    /// or a function pointer's return and parameter types. `None` for any other spelling.
    let private constructedFrom (spelling : TypeDefn) : TypeDefn list option =
        match spelling with
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> Some [ element ]
        | TypeDefn.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> []
                | MethodReturnType.Returns ret -> [ ret ]

            Some (returned @ signature.ParameterTypes)
        | _ -> None

    /// One step of a load level over a spelling: `atDefinition` for the definition it names, or
    /// `onPart` for each part it is built from (an instantiation's generic definition, then, when
    /// `withArguments`, its arguments; a constructed type's elements). `onPart` is the level's own
    /// entry point, so that each part is brought through the levels before it first, as CoreCLR
    /// brings every type it reaches.
    let private walkSpelling
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (withArguments : bool)
        (atDefinition : ResolvedTypeIdentity -> LoadWalk -> LoadWalk * TypeResolutionMiss option)
        (onPart : TypeDefn -> LoadWalk -> LoadWalk * TypeResolutionMiss option)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        let recurse = onPart

        match spelling with
        | TypeDefn.Modified modified -> recurse modified.Unmodified walk
        | TypeDefn.GenericInstantiation (root, args) ->
            let parts = if withArguments then root :: List.ofSeq args else [ root ]
            firstMiss (List.map recurse parts) walk
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ ->
            match definitionOf loggerFactory dotnetRuntimeDirs spellingAssembly spelling walk with
            | walk, Error miss -> walk, Some miss
            | walk, Ok identity -> atDefinition identity walk
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> walk, None
        | constructed ->
            match constructedFrom constructed with
            | Some elements -> firstMiss (List.map recurse elements) walk
            | None -> failwith $"BUG: no load walk for %O{constructed}"

    /// `ClassLoader::LoadApproxTypeThrowing`, by which `CLASS_LOAD_APPROXPARENTS` loads a base or an
    /// interface: for an instantiation, its generic definition, and then, unless that is an
    /// interface, the arguments that are value types (the others stand as `Object`, unloaded).
    let rec private approximateParent
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, args) ->
            match
                definitionOf loggerFactory dotnetRuntimeDirs spellingAssembly (TypeDefn.stripCustomModifiers root) walk
            with
            | walk, Error miss -> walk, Some miss
            | walk, Ok identity ->

            match approximateDefinition loggerFactory dotnetRuntimeDirs identity walk with
            | walk, Some miss -> walk, Some miss
            | walk, None ->

            let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName

            if definedIn.TypeDefs.[identity.TypeDefinition.Get].IsInterface then
                walk, None
            else
                firstMiss
                    (args
                     |> Seq.filter isValueTypeSpelling
                     |> Seq.map (approximateArgument loggerFactory dotnetRuntimeDirs spellingAssembly)
                     |> List.ofSeq)
                    walk
        | other -> approximate loggerFactory dotnetRuntimeDirs spellingAssembly other walk

    /// A value-type argument of an approximately loaded instantiation, or the type of a value-type
    /// instance field. The replacement of reference types by `Object` reaches into it: an
    /// instantiation's generic definition is loaded, and of its arguments only the value types.
    and private approximateArgument
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, args) ->
            approximate loggerFactory dotnetRuntimeDirs spellingAssembly (TypeDefn.stripCustomModifiers root) walk
            |> andThen (fun walk ->
                firstMiss
                    (args
                     |> Seq.filter isValueTypeSpelling
                     |> Seq.map (approximateArgument loggerFactory dotnetRuntimeDirs spellingAssembly)
                     |> List.ofSeq)
                    walk
            )
        | other -> approximate loggerFactory dotnetRuntimeDirs spellingAssembly other walk

    /// `CLASS_LOAD_APPROXPARENTS` for a type spelled in `spellingAssembly`.
    and private approximate
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        walkSpelling
            loggerFactory
            dotnetRuntimeDirs
            true
            (approximateDefinition loggerFactory dotnetRuntimeDirs)
            (approximate loggerFactory dotnetRuntimeDirs spellingAssembly)
            spellingAssembly
            spelling
            walk

    /// `MethodTableBuilder` for a type definition: its approximate base and interfaces, and the
    /// value types of its instance fields, which its layout needs, loaded as approximately as a
    /// value-type argument is.
    and private approximateDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (identity : ResolvedTypeIdentity)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        if walk.Approximate.Contains identity then
            walk, None
        else

        let walk =
            { walk with
                Approximate = walk.Approximate.Add identity
            }

        let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName
        let definition = definedIn.TypeDefs.[identity.TypeDefinition.Get]
        let parent = approximateParent loggerFactory dotnetRuntimeDirs definedIn
        let field = approximateArgument loggerFactory dotnetRuntimeDirs definedIn

        firstMiss
            (List.map
                parent
                (Option.toList (baseSpelling definedIn definition)
                 @ interfaceSpellings definedIn definition)
             @ List.map field (valueTypeFields false definition))
            walk

    /// `CLASS_LOAD_EXACTPARENTS` for a type spelled in `spellingAssembly`, after its approximate
    /// load. An instantiation's arguments stay at the approximate level until `DoFullyLoad`.
    let rec private exact
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        approximate loggerFactory dotnetRuntimeDirs spellingAssembly spelling walk
        |> andThen (
            walkSpelling
                loggerFactory
                dotnetRuntimeDirs
                false
                (exactDefinition loggerFactory dotnetRuntimeDirs)
                (exact loggerFactory dotnetRuntimeDirs spellingAssembly)
                spellingAssembly
                spelling
        )

    /// `ClassLoader::LoadExactParentAndInterfacesTransitively`: a type definition's exact base, then
    /// its exact interfaces.
    and private exactDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (identity : ResolvedTypeIdentity)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        if walk.Exact.Contains identity then
            walk, None
        else

        let walk =
            { walk with
                Exact = walk.Exact.Add identity
            }

        let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName
        let definition = definedIn.TypeDefs.[identity.TypeDefinition.Get]

        firstMiss
            (List.map
                (exact loggerFactory dotnetRuntimeDirs definedIn)
                (Option.toList (baseSpelling definedIn definition)
                 @ interfaceSpellings definedIn definition))
            walk

    /// Whether a field of this type has a primitive `FieldDesc` rather than a value-type one, which
    /// `MethodTableBuilder` decides from metadata without loading the type: its definition (an
    /// instantiation's generic definition, its arguments unloaded) derives directly from
    /// `System.Enum`. A type that cannot be found is not one.
    let private isEnumField
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * bool
        =
        let nominal =
            match TypeDefn.stripCustomModifiers spelling with
            | TypeDefn.GenericInstantiation (root, _) -> TypeDefn.stripCustomModifiers root
            | other -> other

        match nominal with
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ ->
            match definitionOf loggerFactory dotnetRuntimeDirs spellingAssembly nominal walk with
            | walk, Error _ -> walk, false
            | walk, Ok identity ->
                let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName
                let definition = definedIn.TypeDefs.[identity.TypeDefinition.Get]

                match baseSpelling definedIn definition with
                | None -> walk, false
                | Some baseType ->
                    match definitionOf loggerFactory dotnetRuntimeDirs definedIn baseType walk with
                    | walk, Ok baseIdentity -> walk, baseIdentity = baseClassTypes.Enum.Identity
                    | walk, Error _ -> walk, false
        | _ -> walk, false

    /// `MethodTable::DoFullyLoad` for a type already created, spelled in `spellingAssembly`, after
    /// its exact-parents load: an instantiation's generic definition, then its arguments. This is how
    /// a base or an interface is fully loaded.
    let rec private fullyLoad
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        exact loggerFactory dotnetRuntimeDirs spellingAssembly spelling walk
        |> andThen (
            walkSpelling
                loggerFactory
                dotnetRuntimeDirs
                true
                (fullyLoadDefinition loggerFactory dotnetRuntimeDirs baseClassTypes)
                (fullyLoad loggerFactory dotnetRuntimeDirs baseClassTypes spellingAssembly)
                spellingAssembly
                spelling
        )

    /// A type definition's base, its interfaces, and the value types of its fields, instance then
    /// static.
    and private fullyLoadDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (identity : ResolvedTypeIdentity)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        if walk.Full.Contains identity then
            walk, None
        else

        let walk =
            { walk with
                Full = walk.Full.Add identity
            }

        let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName
        let definition = definedIn.TypeDefs.[identity.TypeDefinition.Get]

        let load = fullyLoad loggerFactory dotnetRuntimeDirs baseClassTypes definedIn

        // A field of an enum type has a primitive `FieldDesc`, which `DoFullyLoad` does not load.
        let loadField (spelling : TypeDefn) (walk : LoadWalk) =
            match isEnumField loggerFactory dotnetRuntimeDirs baseClassTypes definedIn spelling walk with
            | walk, true -> walk, None
            | walk, false ->
                // `GetFieldTypeHandleThrowing` brings the type to the level below the one
                // `DoFullyLoad` is completing, which on its first pass is `CLASS_LOAD_EXACTPARENTS`,
                // and then `DoFullyLoad` completes it.
                loadFresh loggerFactory dotnetRuntimeDirs baseClassTypes LoadLevel.ExactParents definedIn spelling walk
                |> andThen (load spelling)

        firstMiss
            (List.map
                load
                (Option.toList (baseSpelling definedIn definition)
                 @ interfaceSpellings definedIn definition)
             @ List.map loadField (valueTypeFields true definition))
            walk

    /// `SigPointer::GetTypeHandleThrowing` to the given level, for a type spelled in
    /// `spellingAssembly` that nothing has created yet: an instantiation's generic definition
    /// approximately, then its arguments to that level, then the instantiation to it; a constructed
    /// type's elements to it; a definition through every level up to it.
    and private loadFresh
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (level : LoadLevel)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * TypeResolutionMiss option
        =
        let recurse =
            loadFresh loggerFactory dotnetRuntimeDirs baseClassTypes level spellingAssembly

        // A definition (an instantiation's, its arguments already loaded) through the levels up to
        // `level`, after its approximate load.
        let complete (identity : ResolvedTypeIdentity) (walk : LoadWalk) =
            match level with
            | LoadLevel.Approximate -> walk, None
            | LoadLevel.ExactParents -> exactDefinition loggerFactory dotnetRuntimeDirs identity walk
            | LoadLevel.Full ->
                exactDefinition loggerFactory dotnetRuntimeDirs identity walk
                |> andThen (fullyLoadDefinition loggerFactory dotnetRuntimeDirs baseClassTypes identity)

        match spelling with
        | TypeDefn.Modified modified -> recurse modified.Unmodified walk
        | TypeDefn.GenericInstantiation (root, args) ->
            match
                definitionOf loggerFactory dotnetRuntimeDirs spellingAssembly (TypeDefn.stripCustomModifiers root) walk
            with
            | walk, Error miss -> walk, Some miss
            | walk, Ok identity ->
                approximateDefinition loggerFactory dotnetRuntimeDirs identity walk
                |> andThen (firstMiss (args |> Seq.map recurse |> List.ofSeq))
                |> andThen (complete identity)
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ ->
            match definitionOf loggerFactory dotnetRuntimeDirs spellingAssembly spelling walk with
            | walk, Error miss -> walk, Some miss
            | walk, Ok identity ->
                approximateDefinition loggerFactory dotnetRuntimeDirs identity walk
                |> andThen (complete identity)
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> walk, None
        | constructed ->
            match constructedFrom constructed with
            | Some elements -> firstMiss (List.map recurse elements) walk
            | None -> failwith $"BUG: no load walk for %O{constructed}"

    /// The first type reference that names no type, among those CoreCLR's load of `spelling` (in
    /// `spellingAssembly`) reaches, in the order it reaches them. Loads whatever assemblies those
    /// references take.
    let private loadMiss
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * TypeResolutionMiss option
        =
        let walk =
            {
                Assemblies = assemblies
                Approximate = Set.empty
                Exact = Set.empty
                Full = Set.empty
            }

        let walk, miss =
            loadFresh loggerFactory dotnetRuntimeDirs baseClassTypes LoadLevel.Full spellingAssembly spelling walk

        walk.Assemblies, miss

    /// Read the parent token of one of `referencingAssembly`'s MemberRefs. Loads whatever assemblies
    /// naming it takes.
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (referencingAssembly : DumpedAssembly)
        (reference : MemberReferenceHandle)
        : LoadedAssemblies * MemberReferenceParent
        =
        let row = referencingAssembly.Members.[reference]

        let nominal (assemblies, result) =
            match result with
            | Ok identity -> assemblies, MemberReferenceParent.Nominal identity
            | Error miss -> assemblies, MemberReferenceParent.Unresolved miss

        // CoreCLR loads the whole parent before it looks for the member, so a parent whose load
        // reaches a type reference that names nothing is `Unresolved` whatever the member.
        let loaded (spelling : TypeDefn) (answer : LoadedAssemblies -> LoadedAssemblies * MemberReferenceParent) =
            match loadMiss loggerFactory dotnetRuntimeDirs baseClassTypes referencingAssembly spelling assemblies with
            | assemblies, Some miss -> assemblies, MemberReferenceParent.Unresolved miss
            | assemblies, None -> answer assemblies

        match row.Parent with
        | MetadataToken.MethodDef handle -> assemblies, MemberReferenceParent.VarArgDefinition handle
        | MetadataToken.TypeDefinition handle ->
            let identity = referencingAssembly.TypeDefs.[handle].Identity

            loaded
                (TypeDefn.FromDefinition (identity, SignatureTypeKind.Unknown))
                (fun assemblies -> assemblies, MemberReferenceParent.Nominal identity)
        | MetadataToken.TypeReference handle ->
            let typeRef = referencingAssembly.TypeRefs.[handle]

            loaded
                (TypeDefn.FromReference (typeRef, SignatureTypeKind.Unknown))
                (fun assemblies ->
                    TypeResolution.resolveTypeRefIdentity
                        loggerFactory
                        dotnetRuntimeDirs
                        referencingAssembly
                        typeRef
                        assemblies
                    |> nominal
                )
        | MetadataToken.TypeSpecification handle ->
            let spelling = referencingAssembly.TypeSpecs.[handle].Signature

            loaded spelling
            <| fun assemblies ->

                match TypeDefn.stripCustomModifiers spelling with
                | TypeDefn.OneDimensionalArrayLowerBoundZero _
                | TypeDefn.Array _ as arrayType -> assemblies, MemberReferenceParent.Array arrayType
                | TypeDefn.GenericTypeParameter _
                | TypeDefn.GenericMethodParameter _ -> assemblies, MemberReferenceParent.TypeVariable
                | _ ->
                    identityOfSpelling
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        referencingAssembly
                        spelling
                        assemblies
                    |> nominal
        | MetadataToken.ModuleReference _ ->
            failwith
                $"TODO: MemberRef %s{row.PrettyName} in %s{referencingAssembly.DefinitionFullName} names a global member of another module, which is not modelled"
        | other ->
            failwith
                $"MemberRef %s{row.PrettyName} in %s{referencingAssembly.DefinitionFullName} has parent %O{other}, which ECMA-335 does not permit"
