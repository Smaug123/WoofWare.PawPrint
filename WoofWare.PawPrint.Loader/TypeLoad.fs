namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// CoreCLR's load of a type to <c>CLASS_LOADED</c>, the level at which the JIT asks for every type a
/// method names, followed level by level, and the first type reference it reaches that names
/// nothing, which fails the load.
/// </summary>
/// <remarks>
/// <para>
/// The load brings the type's base class and interfaces approximately, then exactly, then in full,
/// each transitively; the types of its value-type fields, static and instance, except an enum's,
/// which has a primitive <c>FieldDesc</c>, though telling that it is an enum binds the reference to
/// it and to its base; and every type each of those spells as a generic argument, an array's
/// element, a pointer's target or a function pointer's signature. It does not load the
/// type that encloses a nested one, the type of a field that is a class, an array or a pointer, the
/// type of a <c>const</c> field, or anything a method's signature names.
/// </para>
/// <para>
/// Once every type the load reaches has been through those levels (`CLASS_DEPENDENCIES_LOADED`),
/// it loads, for each type definition outside CoreLib it has brought, the constraints of the
/// definition's type parameters, each to that level: the constraint types and what they reach, but
/// not their own constraints. It does not check that any instantiation satisfies its constraints.
/// </para>
/// <para>
/// Not followed: the constraints of a definition's generic methods, which
/// <c>MethodTable::DoFullyLoad</c> loads with its type parameters'. A reference only one of those
/// names can fail a load that this reports as meeting no miss. CoreCLR also loads no constraints in
/// a ReadyToRun image that skips type validation, which this does not tell from any other image
/// outside CoreLib: such a load may meet a miss that CoreCLR never meets.
/// </para>
/// <para>
/// No miss does not mean the load succeeds: CoreCLR's class loader fails a load for many other
/// reasons, none of which this models (<c>ParentLoadVouching</c> says when none can arise).
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module TypeLoad =

    /// Why a type that a load reaches cannot be loaded.
    [<RequireQualifiedAccess>]
    type LoadMiss =
        /// A type reference names no type in the assembly it is scoped to.
        | TypeAbsent of TypeResolutionMiss
        /// A type reference is to an assembly no runtime directory supplies.
        | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference
        /// The load reaches a shape PawPrint does not implement loading.
        | Unmodelled of UnmodelledBinding

    /// A type reference's definition, or why it has none.
    let internal ofIdentity (identity : TypeReferenceIdentity) : Result<ResolvedTypeIdentity, LoadMiss> =
        match identity with
        | TypeReferenceIdentity.Resolved identity -> Ok identity
        | TypeReferenceIdentity.TypeAbsent miss -> Error (LoadMiss.TypeAbsent miss)
        | TypeReferenceIdentity.AssemblyUnavailable reference -> Error (LoadMiss.AssemblyUnavailable reference)
        | TypeReferenceIdentity.Unmodelled unmodelled -> Error (LoadMiss.Unmodelled unmodelled)

    /// The CoreCLR load levels a walk distinguishes.
    [<RequireQualifiedAccess>]
    type private LoadLevel =
        /// `CLASS_LOAD_APPROXPARENTS`.
        | Approximate
        /// `CLASS_LOAD_EXACTPARENTS`.
        | ExactParents
        /// `CLASS_DEPENDENCIES_LOADED`, through `MethodTable::DoFullyLoad`.
        | Dependencies
        /// `CLASS_LOADED`: `CLASS_DEPENDENCIES_LOADED`, then the checks `DoFullyLoad` makes there.
        | Full

    /// The type definitions a walk of a load has brought to each of CoreCLR's load levels,
    /// and the assemblies loaded on the way.
    type private LoadWalk =
        {
            Assemblies : LoadedAssemblies
            /// Brought to `CLASS_LOAD_APPROXPARENTS`.
            Approximate : Set<ResolvedTypeIdentity>
            /// Brought to `CLASS_LOAD_EXACTPARENTS`.
            Exact : Set<ResolvedTypeIdentity>
            /// Brought to `CLASS_DEPENDENCIES_LOADED`.
            Dependencies : Set<ResolvedTypeIdentity>
            /// Brought to `CLASS_LOADED`: what `CLASS_DEPENDENCIES_LOADED` brings, and its
            /// constraints.
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
        (steps : (LoadWalk -> LoadWalk * LoadMiss option) list)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
        =
        match steps with
        | [] -> walk, None
        | step :: rest ->
            match step walk with
            | walk, None -> firstMiss rest walk
            | walk, Some miss -> walk, Some miss

    /// Run `step`, unless an earlier one found a miss.
    let private andThen
        (step : LoadWalk -> LoadWalk * LoadMiss option)
        (walk : LoadWalk, miss : LoadMiss option)
        : LoadWalk * LoadMiss option
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
        : LoadWalk * Result<ResolvedTypeIdentity, LoadMiss>
        =
        match spelling with
        | TypeDefn.FromDefinition (identity, _) -> walk, Ok identity
        | TypeDefn.FromReference (typeRef, _) ->
            let assemblies, identity =
                TypeResolution.tryResolveTypeRefIdentity
                    loggerFactory
                    dotnetRuntimeDirs
                    spellingAssembly
                    typeRef
                    walk.Assemblies

            { walk with
                Assemblies = assemblies
            },
            ofIdentity identity
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
    /// assembly; one ECMA-335 does not permit is unmodelled.
    let private interfaceSpellings
        (definedIn : DumpedAssembly)
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : Result<TypeDefn, UnmodelledBinding> list
        =
        definition.ImplementedInterfaces
        |> Seq.map (fun implementation ->
            match implementation.InterfaceHandle with
            | MetadataToken.TypeDefinition handle ->
                Ok (TypeDefn.FromDefinition (definedIn.TypeDefs.[handle].Identity, SignatureTypeKind.Class))
            | MetadataToken.TypeReference handle ->
                Ok (TypeDefn.FromReference (definedIn.TypeRefs.[handle], SignatureTypeKind.Class))
            | MetadataToken.TypeSpecification handle -> Ok definedIn.TypeSpecs.[handle].Signature
            | other ->
                Error
                    {
                        Diagnostic =
                            $"%s{definition.Namespace}.%s{definition.Name} in %s{definedIn.DefinitionFullName} implements %O{other}, which ECMA-335 does not permit as an interface"
                    }
        )
        |> List.ofSeq

    /// `step` over a type definition's base, then over each of its interfaces in metadata order: the
    /// order CoreCLR loads a type's parents in. An interface spelled in a way PawPrint does not
    /// model is a step that misses.
    let private parentSteps
        (step : TypeDefn -> LoadWalk -> LoadWalk * LoadMiss option)
        (definedIn : DumpedAssembly)
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : (LoadWalk -> LoadWalk * LoadMiss option) list
        =
        let interfaces =
            interfaceSpellings definedIn definition
            |> List.map (fun spelling ->
                match spelling with
                | Ok spelling -> step spelling
                | Error unmodelled -> fun walk -> walk, Some (LoadMiss.Unmodelled unmodelled)
            )

        (baseSpelling definedIn definition |> Option.toList |> List.map step)
        @ interfaces

    /// The types of a type definition's value-type fields that have a `FieldDesc` (a literal has
    /// none), in `FieldDesc` order: its instance fields, then its ordinary static fields, then its
    /// `[ThreadStatic]` ones, each in metadata order.
    let private valueTypeFields (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>) : TypeDefn list =
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
            let statics = fields |> List.filter isStatic

            (statics |> List.filter (fun field -> not field.IsThreadStatic))
            @ (statics |> List.filter (fun field -> field.IsThreadStatic))

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
        (atDefinition : ResolvedTypeIdentity -> LoadWalk -> LoadWalk * LoadMiss option)
        (onPart : TypeDefn -> LoadWalk -> LoadWalk * LoadMiss option)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
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

    /// `MethodTableBuilder::GetCorElementTypeOfTypeDefOrRefForStaticField` for the type of a static
    /// value-type field, spelled in `spellingAssembly`: whether it names an enum, decided from
    /// metadata without loading it. That resolves the reference naming its definition (an
    /// instantiation's generic definition), and a base class referred to by the name `System.Enum`,
    /// and throws where either needs an assembly no runtime directory supplies. A type the assembly
    /// lacks it leaves to `DoFullyLoad`. The approximate load it tries first swallows every failure,
    /// so it reaches no miss.
    let private staticFieldReference
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
        =
        let unavailable (walk : LoadWalk, result : Result<ResolvedTypeIdentity, LoadMiss>) =
            match result with
            | Error (LoadMiss.AssemblyUnavailable _ as miss)
            | Error (LoadMiss.Unmodelled _ as miss) -> walk, Some miss
            | Error (LoadMiss.TypeAbsent _)
            | Ok _ -> walk, None

        let nominal =
            match TypeDefn.stripCustomModifiers spelling with
            | TypeDefn.GenericInstantiation (root, _) -> TypeDefn.stripCustomModifiers root
            | other -> other

        match definitionOf loggerFactory dotnetRuntimeDirs spellingAssembly nominal walk with
        | walk, Error miss -> unavailable (walk, Error miss)
        | walk, Ok identity ->
            let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName

            match definedIn.TypeDefs.[identity.TypeDefinition.Get].BaseType with
            | Some (BaseTypeInfo.TypeRef handle) when
                definedIn.TypeRefs.[handle].Namespace = "System"
                && definedIn.TypeRefs.[handle].Name = "Enum"
                ->
                definitionOf
                    loggerFactory
                    dotnetRuntimeDirs
                    definedIn
                    (TypeDefn.FromReference (definedIn.TypeRefs.[handle], SignatureTypeKind.Unknown))
                    walk
                |> unavailable
            | _ -> walk, None

    /// `ClassLoader::LoadApproxTypeThrowing`, by which `CLASS_LOAD_APPROXPARENTS` loads a base or an
    /// interface: for an instantiation, its generic definition, and then, unless that is an
    /// interface, the arguments that are value types (the others stand as `Object`, unloaded).
    let rec private approximateParent
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
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
        : LoadWalk * LoadMiss option
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
        : LoadWalk * LoadMiss option
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

    /// `MethodTableBuilder` for a type definition: its approximate base and interfaces, then its
    /// value-type fields in metadata order (`InitializeFieldDescs`): the type of an instance one,
    /// which its layout needs, loaded as approximately as a value-type argument is, and the
    /// reference naming a static one's (`staticFieldReference`).
    and private approximateDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (identity : ResolvedTypeIdentity)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
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

        let field (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) =
            if field.Attributes.HasFlag System.Reflection.FieldAttributes.Static then
                staticFieldReference loggerFactory dotnetRuntimeDirs definedIn field.Signature
            else
                approximateArgument loggerFactory dotnetRuntimeDirs definedIn field.Signature

        let fields =
            definition.Fields
            |> List.filter (fun field ->
                not (field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal)
                && isValueTypeSpelling field.Signature
            )

        firstMiss (parentSteps parent definedIn definition @ List.map field fields) walk

    /// `CLASS_LOAD_EXACTPARENTS` for a type spelled in `spellingAssembly`, after its approximate
    /// load. An instantiation's arguments stay at the approximate level until `DoFullyLoad`.
    let rec private exact
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
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
        : LoadWalk * LoadMiss option
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

        firstMiss (parentSteps (exact loggerFactory dotnetRuntimeDirs definedIn) definedIn definition) walk

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

    /// The types `DoAccessibilityCheckForConstraintSignature` loads of a constraint's signature, each
    /// without its instantiation, in the order the signature names them: a generic type, then each
    /// of its arguments; a vector's, pointer's or byref's element; a function pointer's return type,
    /// then its parameters; past a custom modifier, the type it modifies. The walk ends at a part
    /// CoreCLR refuses as malformed, or misreads, which is unmodelled.
    let rec private constraintSignatureTypes (spelling : TypeDefn) : Result<TypeDefn, UnmodelledBinding> list =
        let all (spellings : TypeDefn list) : Result<TypeDefn, UnmodelledBinding> list =
            let rec go (walked : Result<TypeDefn, UnmodelledBinding> list) (spellings : TypeDefn list) =
                match spellings with
                | [] -> walked
                | spelling :: rest ->
                    let walked = walked @ constraintSignatureTypes spelling

                    if walked |> List.exists Result.isError then
                        walked
                    else
                        go walked rest

            go [] spellings

        match spelling with
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ -> [ Ok spelling ]
        | TypeDefn.GenericInstantiation (root, args) -> all (root :: List.ofSeq args)
        // `SigParser::GetElemType` skips a custom modifier without loading its type.
        | TypeDefn.Modified modified -> constraintSignatureTypes modified.Unmodified
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element -> constraintSignatureTypes element
        | TypeDefn.Array _ ->
            [
                Error
                    {
                        Diagnostic =
                            $"the constraint signature %O{spelling} has a multidimensional array, whose element type CoreCLR's DoAccessibilityCheckForConstraintSignature reads past before it walks the rest, which PawPrint does not follow"
                    }
            ]
        | TypeDefn.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> []
                | MethodReturnType.Returns ret -> [ ret ]

            all (returned @ signature.ParameterTypes)
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> []
        | TypeDefn.Pinned _ ->
            [
                Error
                    {
                        Diagnostic =
                            $"the constraint signature %O{spelling} has a pinned type, for which CoreCLR's DoAccessibilityCheckForConstraintSignature throws BadImageFormatException"
                    }
            ]

    /// A load the constraint checks of a definition's type parameters make, in the order
    /// `MethodTable::DoFullyLoad` makes them.
    [<RequireQualifiedAccess>]
    type private ConstraintLoad =
        /// `TypeVarTypeDesc::LoadConstraints` for the type parameter at this index: each constraint,
        /// except an interface instantiation, of which only the generic definition.
        | Constraints of parameter : int
        /// `DoAccessibilityCheckForConstraints` for the type parameter at this index: each type the
        /// signature of its first constraint names, if that is an interface instantiation.
        | Signatures of parameter : int

    /// The order of the loads `DoFullyLoad` makes for the type parameters of a definition, each with
    /// the constraints `constraints` gives: for each in turn, `Bounded` loads its constraints, and
    /// then those of each type parameter it is constrained to, transitively, each once; then
    /// `DoAccessibilityCheckForConstraints` walks the signature of its first constraint.
    let private constraintLoadOrder (constraints : TypeDefn list list) : ConstraintLoad list =
        let parameters = constraints.Length

        // `Bounded`, to `depth`: it gives up at depth 0, which only constraints among the
        // parameters that form a cycle reach.
        let rec bounded
            (loaded : Set<int>, order : ConstraintLoad list)
            (parameter : int)
            (depth : int)
            : Set<int> * ConstraintLoad list
            =
            if depth = 0 then
                loaded, order
            else

            let loaded, order =
                if loaded.Contains parameter then
                    loaded, order
                else
                    loaded.Add parameter, ConstraintLoad.Constraints parameter :: order

            ((loaded, order), constraints.[parameter])
            ||> List.fold (fun soFar bound ->
                match TypeDefn.stripCustomModifiers bound with
                | TypeDefn.GenericTypeParameter other -> bounded soFar other (depth - 1)
                | _ -> soFar
            )

        ((Set.empty, []), List.init parameters id)
        ||> List.fold (fun soFar parameter ->
            let loaded, order = bounded soFar parameter parameters
            loaded, ConstraintLoad.Signatures parameter :: order
        )
        |> snd
        |> List.rev

    /// `MethodTable::DoFullyLoad` to `CLASS_DEPENDENCIES_LOADED` for a type already created, spelled
    /// in `spellingAssembly`, after its exact-parents load: an instantiation's generic definition,
    /// then its arguments. This is how a base or an interface is brought to that level.
    let rec private dependencies
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
        =
        exact loggerFactory dotnetRuntimeDirs spellingAssembly spelling walk
        |> andThen (
            walkSpelling
                loggerFactory
                dotnetRuntimeDirs
                true
                (dependenciesDefinition loggerFactory dotnetRuntimeDirs baseClassTypes)
                (dependencies loggerFactory dotnetRuntimeDirs baseClassTypes spellingAssembly)
                spellingAssembly
                spelling
        )

    /// A type definition's base, its interfaces, and the value types of its fields, instance then
    /// static.
    and private dependenciesDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (identity : ResolvedTypeIdentity)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
        =
        if walk.Dependencies.Contains identity then
            walk, None
        else

        let walk =
            { walk with
                Dependencies = walk.Dependencies.Add identity
            }

        let definedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName
        let definition = definedIn.TypeDefs.[identity.TypeDefinition.Get]

        let load = dependencies loggerFactory dotnetRuntimeDirs baseClassTypes definedIn

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
            (parentSteps load definedIn definition
             @ List.map loadField (valueTypeFields definition))
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
        : LoadWalk * LoadMiss option
        =
        let recurse =
            loadFresh loggerFactory dotnetRuntimeDirs baseClassTypes level spellingAssembly

        // A definition (an instantiation's, its arguments already loaded) through the levels up to
        // `level`, after its approximate load.
        let complete (identity : ResolvedTypeIdentity) (walk : LoadWalk) =
            match level with
            | LoadLevel.Approximate -> walk, None
            | LoadLevel.ExactParents -> exactDefinition loggerFactory dotnetRuntimeDirs identity walk
            | LoadLevel.Dependencies ->
                exactDefinition loggerFactory dotnetRuntimeDirs identity walk
                |> andThen (dependenciesDefinition loggerFactory dotnetRuntimeDirs baseClassTypes identity)
            | LoadLevel.Full ->
                exactDefinition loggerFactory dotnetRuntimeDirs identity walk
                |> andThen (dependenciesDefinition loggerFactory dotnetRuntimeDirs baseClassTypes identity)
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

    /// The checks `MethodTable::DoFullyLoad` makes of a type definition's type parameters at
    /// `CLASS_LOADED`, as far as they load types: each constraint, spelled in the definition's
    /// assembly, to `CLASS_DEPENDENCIES_LOADED`. An interface instantiation is not loaded: its generic
    /// definition is, and later, if it is the type parameter's first constraint, each type its
    /// signature names.
    and private typeParameterConstraintSteps
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (definedIn : DumpedAssembly)
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : (LoadWalk -> LoadWalk * LoadMiss option) list
        =
        let constraints =
            definition.Generics
            |> Seq.map (fun (_, metadata) -> List.ofSeq metadata.Constraints)
            |> List.ofSeq

        // `DoAccessibilityCheckForConstraints` finds the metadata row of an interface instantiation
        // it did not load by counting rows up to its index, but never advances the count, so it
        // finds the row only of a parameter's first constraint. That row is the first of
        // `Constraints` unless it is the `System.ValueType` row that `struct` comes with, which
        // `Constraints` leaves out.
        let firstRowKept =
            let metadata = definedIn.PeReader.GetMetadataReader ()

            definition.Generics
            |> Seq.map (fun (parameter, parameterMetadata) ->
                let rows = metadata.GetGenericParameter(parameter.Handle.Get).GetConstraints ()

                if rows.Count = 0 then
                    false
                else

                let first = metadata.GetGenericParameterConstraint(rows.[0]).Type

                let namedValueType (ns : StringHandle) (name : StringHandle) =
                    metadata.GetString ns = "System" && metadata.GetString name = "ValueType"

                let isValueType =
                    match first.Kind with
                    | HandleKind.TypeReference ->
                        let reference = metadata.GetTypeReference (TypeReferenceHandle.op_Explicit first)
                        namedValueType reference.Namespace reference.Name
                    | HandleKind.TypeDefinition ->
                        let definition = metadata.GetTypeDefinition (TypeDefinitionHandle.op_Explicit first)
                        namedValueType definition.Namespace definition.Name
                    | _ -> false

                not (
                    isValueType
                    && parameterMetadata.Constraint = Some GenericConstraint.NonNullableValue
                )
            )
            |> List.ofSeq

        let load (spelling : TypeDefn) =
            loadFresh loggerFactory dotnetRuntimeDirs baseClassTypes LoadLevel.Dependencies definedIn spelling

        // Whether a constraint's generic definition, already loaded, is an interface.
        let isInterface (root : TypeDefn) (walk : LoadWalk) : LoadWalk * bool =
            match definitionOf loggerFactory dotnetRuntimeDirs definedIn (TypeDefn.stripCustomModifiers root) walk with
            | walk, Ok identity ->
                let rootDefinedIn = walk.Assemblies.ByDefinitionName identity.AssemblyFullName
                walk, rootDefinedIn.TypeDefs.[identity.TypeDefinition.Get].IsInterface
            | _, Error miss -> failwith $"BUG: a constraint's generic definition was loaded, but now binds to %O{miss}"

        // `LoadConstraints` and `DoAccessibilityCheckForConstraints` classify a constraint by the
        // element type `SigParser::GetElemType` reads, which skips custom modifiers.
        let loadConstraint (bound : TypeDefn) (walk : LoadWalk) : LoadWalk * LoadMiss option =
            match TypeDefn.stripCustomModifiers bound with
            | TypeDefn.GenericInstantiation (root, _) ->
                load root walk
                |> andThen (fun walk ->
                    match isInterface root walk with
                    | walk, true -> walk, None
                    | walk, false -> load bound walk
                )
            | _ -> load bound walk

        let walkSignature (bound : TypeDefn) (walk : LoadWalk) : LoadWalk * LoadMiss option =
            match TypeDefn.stripCustomModifiers bound with
            | TypeDefn.GenericInstantiation (root, _) ->
                match isInterface root walk with
                | walk, false -> walk, None
                | walk, true ->
                    let step (part : Result<TypeDefn, UnmodelledBinding>) =
                        match part with
                        | Ok spelling -> load spelling
                        | Error unmodelled -> fun (walk : LoadWalk) -> walk, Some (LoadMiss.Unmodelled unmodelled)

                    firstMiss (constraintSignatureTypes bound |> List.map step) walk
            | _ -> walk, None

        constraintLoadOrder constraints
        |> List.map (fun step (walk : LoadWalk) ->
            match step with
            | ConstraintLoad.Constraints parameter -> firstMiss (List.map loadConstraint constraints.[parameter]) walk
            | ConstraintLoad.Signatures parameter ->
                match constraints.[parameter] with
                | first :: _ when firstRowKept.[parameter] -> walkSignature first walk
                | _ -> walk, None
        )

    /// `MethodTable::DoFullyLoad` to `CLASS_LOADED` for a type spelled in `spellingAssembly`, once
    /// every type its load reaches has been brought to `CLASS_DEPENDENCIES_LOADED`: as that walk,
    /// each type definition after what it reaches, with the checks each definition makes at the end,
    /// but not a function pointer's signature.
    and private fullyLoad
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
        =
        match spelling with
        // `TypeDesc::DoFullyLoad` completes a function pointer without its signature's types, which
        // stay at the level creating it brought them to.
        | TypeDefn.FunctionPointer _ -> walk, None
        | _ ->
            walkSpelling
                loggerFactory
                dotnetRuntimeDirs
                true
                (fullyLoadDefinition loggerFactory dotnetRuntimeDirs baseClassTypes)
                (fullyLoad loggerFactory dotnetRuntimeDirs baseClassTypes spellingAssembly)
                spellingAssembly
                spelling
                walk

    /// A type definition's base, its interfaces and the value types of its fields, instance then
    /// static, each to `CLASS_LOADED`; then, outside CoreLib, which skips the checks, its type
    /// parameters' constraints.
    and private fullyLoadDefinition
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (identity : ResolvedTypeIdentity)
        (walk : LoadWalk)
        : LoadWalk * LoadMiss option
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

        let loadField (spelling : TypeDefn) (walk : LoadWalk) =
            match isEnumField loggerFactory dotnetRuntimeDirs baseClassTypes definedIn spelling walk with
            | walk, true -> walk, None
            | walk, false -> load spelling walk

        // `Module::SkipTypeValidation`, which CoreLib always sets.
        let constraints =
            if identity.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName then
                []
            else
                typeParameterConstraintSteps loggerFactory dotnetRuntimeDirs baseClassTypes definedIn definition

        firstMiss
            (parentSteps load definedIn definition
             @ List.map loadField (valueTypeFields definition)
             @ constraints)
            walk

    /// The type definitions that loads have brought in full without meeting a miss
    /// (`loadMissFrom`). A load from these passes over them at every level, as CoreCLR passes over a
    /// type already loaded: none can lead it to a miss, since a definition's load reaches the same
    /// references wherever it is spelled. That holds only over the assemblies those loads
    /// were over, or more of them, each reference bound as it was.
    [<NoEquality ; NoComparison>]
    type LoadedDefinitions = private | LoadedDefinitions of Set<ResolvedTypeIdentity>

    /// No definitions loaded: a load from these walks everything it reaches.
    let nothingLoaded : LoadedDefinitions = LoadedDefinitions Set.empty

    /// <summary>
    /// The first type reference that names no type, among those CoreCLR's full load of
    /// <paramref name="spelling" />, a type <paramref name="spellingAssembly" /> spells, reaches, in
    /// the order it reaches them, passing over the definitions <paramref name="loaded" /> holds.
    /// Loads, from <paramref name="dotnetRuntimeDirs" />, whatever assemblies those references
    /// take, and the returned <c>LoadedAssemblies</c> carries them. Where it meets no miss, the
    /// returned <c>LoadedDefinitions</c> holds the definitions it loaded as well; otherwise it is
    /// <paramref name="loaded" />.
    /// </summary>
    /// <remarks>
    /// A type variable is not a type the load reaches: a spelling of one, or of a generic
    /// definition's own instantiation over its type parameters, leaves the types an instantiation
    /// supplies for them to wherever that instantiation is spelled.
    /// </remarks>
    let loadMissFrom
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (loaded : LoadedDefinitions)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * LoadedDefinitions * LoadMiss option
        =
        let (LoadedDefinitions full) = loaded

        let walk =
            {
                Assemblies = assemblies
                Approximate = full
                Exact = full
                Dependencies = full
                Full = full
            }

        match
            loadFresh loggerFactory dotnetRuntimeDirs baseClassTypes LoadLevel.Full spellingAssembly spelling walk
        with
        | walk, None -> walk.Assemblies, LoadedDefinitions walk.Full, None
        // A walk stops at its first miss, so a definition it marked may have parts it never reached.
        | walk, Some miss -> walk.Assemblies, loaded, Some miss

    /// <summary>
    /// The first type reference that names no type, among those CoreCLR's full load of
    /// <paramref name="spelling" />, a type <paramref name="spellingAssembly" /> spells, reaches, in
    /// the order it reaches them: <c>loadMissFrom</c> with nothing loaded.
    /// </summary>
    let loadMiss
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * LoadMiss option
        =
        let assemblies, _, miss =
            loadMissFrom
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                spellingAssembly
                spelling
                nothingLoaded
                assemblies

        assemblies, miss

    /// <summary>
    /// The first type reference that names no type, among those CoreCLR's approximate load of
    /// <paramref name="spelling" />, a value type or a type variable <paramref name="spellingAssembly" /> spells,
    /// reaches, in the order it reaches them, passing over the definitions <paramref name="loaded" />
    /// holds. This is how CoreCLR loads a value type that a signature names in order to lay out the
    /// arguments (<c>SigPointer::PeekElemTypeNormalized</c>): to <c>CLASS_LOAD_APPROXPARENTS</c>, where an
    /// instantiation loads its generic definition and, of its arguments, only the value types, each
    /// as approximately, and a definition loads its base and interfaces approximately and the types
    /// of its value-type fields, but nothing a later level loads. Loads, from
    /// <paramref name="dotnetRuntimeDirs" />, whatever assemblies those references take, and the
    /// returned <c>LoadedAssemblies</c> carries them.
    /// </summary>
    /// <remarks>A type variable is not a type the load reaches, as for <c>loadMissFrom</c>.</remarks>
    let approximateLoadMissFrom
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (loaded : LoadedDefinitions)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * LoadMiss option
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> ()
        | spelling when isValueTypeSpelling spelling -> ()
        | _ ->
            failwith
                $"BUG: TypeLoad.approximateLoadMissFrom was given %O{spelling}, which is spelled as neither a value type nor a type variable"

        let (LoadedDefinitions full) = loaded

        let walk =
            {
                Assemblies = assemblies
                Approximate = full
                Exact = full
                Dependencies = full
                Full = full
            }

        let walk, miss =
            approximateArgument loggerFactory dotnetRuntimeDirs spellingAssembly spelling walk

        walk.Assemblies, miss
