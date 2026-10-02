namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

/// A MemberRef row resolved as CoreCLR binds it, and instantiated for one generic context: which
/// method or field it names, and the arguments of the type that declares it. Whatever loads an
/// assembly or registers a concrete type on the way returns the state it leaves behind;
/// `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module MemberReferenceInstantiation =
    /// The name `MethodTable::_GetFullyQualifiedNameForClass` gives a type definition: its
    /// namespace and name, and for a nested type (whose metadata namespace is empty) its bare name.
    let private definitionName (state : TypeSystemState) (identity : ResolvedTypeIdentity) : string =
        let declaring = state._LoadedAssemblies.ByDefinitionName identity.AssemblyFullName
        let definition = declaring.TypeDefs.[identity.TypeDefinition.Get]

        if System.String.IsNullOrEmpty definition.Namespace then
            definition.Name
        else
            $"%s{definition.Namespace}.%s{definition.Name}"

    /// The name `MemberLoader::ThrowMissingFieldException` gives an array parent:
    /// `TypeDesc::ConstructName` over the element's `TypeHandle::GetName`.
    let rec private arrayParentName
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (state : TypeSystemState)
        : TypeSystemState * string
        =
        let recurse =
            arrayParentName loggerFactory dotnetRuntimeDirs baseClassTypes spellingAssembly

        match spelling with
        | TypeDefn.Modified modified -> recurse modified.Unmodified state
        | TypeDefn.PrimitiveType primitive ->
            state, definitionName state (BaseClassTypes.ofPrimitive baseClassTypes primitive).Identity
        | TypeDefn.FromDefinition (identity, _) -> state, definitionName state identity
        | TypeDefn.FromReference (typeRef, _) ->
            let state, _, resolved =
                TypeSystemState.resolveTypeFromRef
                    loggerFactory
                    dotnetRuntimeDirs
                    spellingAssembly
                    typeRef
                    ImmutableArray.Empty
                    state

            state, definitionName state resolved.Identity
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            let state, element = recurse element state
            state, $"%s{element}[]"
        | TypeDefn.Array (element, rank) ->
            let state, element = recurse element state
            let dimensions = if rank = 1 then "*" else System.String (',', rank - 1)
            state, $"%s{element}[%s{dimensions}]"
        | TypeDefn.Pointer element ->
            let state, element = recurse element state
            state, $"%s{element}*"
        | other ->
            // `TypeHandle::GetName` appends an instantiation with `TypeString::AppendInst`, and a
            // function pointer or a type variable has a rendering of its own; none is measured.
            failwith
                $"TODO: name the array element %O{other} as MissingFieldException's message would; the rendering has not been measured"

    /// <summary>
    /// The member a MemberRef row of <paramref name="assy" /> names, as CoreCLR binds it
    /// (<c>MethodReferenceResolution</c>, <c>FieldReferenceResolution</c>), with the generic
    /// arguments of the type that declares it: the reference's parent as the frame whose generic
    /// context is <paramref name="typeGenerics" /> and <paramref name="methodGenerics" />
    /// instantiates it, or the ancestor of the parent that declares the method.
    /// </summary>
    /// <remarks>
    /// A field reference CoreCLR fails to bind is <c>FieldReferenceBinding.Fails</c>, saying why:
    /// CoreCLR throws while compiling the method that uses it, and the caller throws the same
    /// exception where the reference is used. Refuses a method reference CoreCLR would fail to
    /// bind, one whose target depends on how a type variable of the
    /// referencing context is instantiated, and one naming a method the runtime supplies on an array
    /// type, which callers handle before resolving.
    /// </remarks>
    let resolveMemberWithGenerics
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (typeGenerics : ImmutableArray<TypeDefn>)
        (methodGenerics : ImmutableArray<TypeDefn>)
        (m : MemberReferenceHandle)
        (state : TypeSystemState)
        : TypeSystemState *
          AssemblyName *
          Choice<WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>, FieldReferenceBinding> *
          TypeDefn ImmutableArray
        =
        let mem = assy.Members.[m]
        let memberName : string = assy.Strings mem.Name

        let refuse (outcome : string) : 'a =
            failwith
                $"MemberRef %s{memberName} (row %d{MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit m : EntityHandle)}) of %s{assy.DefinitionFullName} %s{outcome}"

        let withAssemblies (assemblies : LoadedAssemblies) (state : TypeSystemState) : TypeSystemState =
            { state with
                _LoadedAssemblies = assemblies
            }

        // The parent as the frame instantiates it, and the arguments the row spells for it. A
        // TypeReference or TypeDefinition spells none: naming a generic definition that way names its
        // typical instantiation, which callers recognise by the empty arguments and refuse.
        let resolveParent
            (state : TypeSystemState)
            : TypeSystemState * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn> * ImmutableArray<TypeDefn>
            =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                let state, _, targetType =
                    TypeSystemState.resolveType loggerFactory dotnetRuntimeDirs parent ImmutableArray.Empty assy state

                state, targetType, ImmutableArray.Empty
            | MetadataToken.TypeDefinition parent ->
                // The bare definition, as a TypeReference parent resolves to it (base chain loaded):
                // its formals stand, rather than being substituted from an environment the parent
                // supplies none of.
                let assemblies, _, targetType =
                    TypeResolution.resolveTypeFromDefn
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        (TypeDefn.FromDefinition (assy.TypeDefs.[parent].Identity, SignatureTypeKind.Unknown))
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        assy
                        state._LoadedAssemblies

                withAssemblies assemblies state, targetType, ImmutableArray.Empty
            | MetadataToken.TypeSpecification parent ->
                let state, _, targetType =
                    TypeSystemState.resolveTypeFromSpec
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        parent
                        assy
                        typeGenerics
                        methodGenerics
                        state

                state, targetType, targetType.Generics
            | parent -> refuse $"has a parent %O{parent} that names no type with members"

        // Whether a substitution still mentions a type variable of the parent, which only a generic
        // definition named without an instantiation leaves standing.
        let rec mentionsTypeVariable (arguments : ImmutableArray<TypeConcretization.SubstitutionArgument>) : bool =
            arguments
            |> Seq.exists (fun argument ->
                match argument with
                | TypeConcretization.SubstitutionArgument.Formal _ -> true
                | TypeConcretization.SubstitutionArgument.Closed _ -> false
                | TypeConcretization.SubstitutionArgument.Spelled (_, _, context) -> mentionsTypeVariable context
            )

        match mem.Signature with
        | MemberSignature.Field _ ->
            let assemblies, target =
                FieldReferenceResolution.resolve
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state._LoadedAssemblies
                    assy
                    m

            let state = withAssemblies assemblies state

            // Which exception binding throws is the first failure of the parent's load, and a missing
            // field is not a failure until the parent has loaded; so either answer needs every type
            // that load reaches to be one that cannot fail some other way.
            let refuseUnvouched () : unit =
                match ParentLoadVouching.vouch baseClassTypes state._LoadedAssemblies assy m with
                | ParentLoadVouch.Vouched -> ()
                | ParentLoadVouch.Unvouched reason ->
                    refuse
                        $"binds to no field, and PawPrint cannot vouch for every type CoreCLR's load of its parent reaches (%O{reason}), so cannot tell which exception binding throws; TODO: that is not modelled"

            match target with
            | FieldReferenceTarget.Defined (declaringAssembly, field) ->
                // Fields are not inherited, so the parent is the declaring type.
                let state, targetType, spelledArguments = resolveParent state

                let field =
                    declaringAssembly.Fields.[field]
                    |> FieldInfo.mapTypeGenerics (fun _ (par, _) -> targetType.Generics.[par.SequenceNumber])

                state, declaringAssembly.Name, Choice2Of2 (FieldReferenceBinding.Bound field), spelledArguments
            | FieldReferenceTarget.Missing ->
                refuseUnvouched ()

                let assemblies, parent =
                    MemberReferenceParent.resolve
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state._LoadedAssemblies
                        assy
                        m

                let state = withAssemblies assemblies state

                let state, parentName =
                    match parent with
                    | MemberReferenceParent.Nominal identity -> state, definitionName state identity
                    | MemberReferenceParent.Array arrayType ->
                        arrayParentName loggerFactory dotnetRuntimeDirs baseClassTypes assy arrayType state
                    | other ->
                        refuse
                            $"binds to no field, but its parent %O{other} is not one FieldReferenceResolution searches"

                let failure = FieldReferenceFailure.MissingField (parentName, memberName)
                state, assy.Name, Choice2Of2 (FieldReferenceBinding.Fails failure), ImmutableArray.Empty
            | FieldReferenceTarget.ParentTypeMissing miss ->
                refuseUnvouched ()

                // `ClassLoader::ThrowTypeLoadException` names the missing TypeRef row by its own
                // namespace and name: a nested row is named alone, without the type it is nested in.
                let typeName, searchedIn =
                    match miss with
                    | TypeResolutionMiss.TopLevelTypeAbsent (searchedIn, ns, name) ->
                        match ns with
                        | None
                        | Some "" -> name, searchedIn
                        | Some ns -> $"%s{ns}.%s{name}", searchedIn
                    | TypeResolutionMiss.NestedTypeAbsent (searchedIn, _declaringType, name) -> name, searchedIn

                let failure = FieldReferenceFailure.ParentTypeMissing (typeName, searchedIn)
                state, assy.Name, Choice2Of2 (FieldReferenceBinding.Fails failure), ImmutableArray.Empty
            | FieldReferenceTarget.ParentAssemblyUnavailable reference ->
                refuse
                    $"has a parent whose load needs %s{reference.FullName}, which no runtime directory supplies, so CoreCLR throws FileNotFoundException; that is not modelled"
            | FieldReferenceTarget.DependsOnInstantiation ->
                refuse
                    "has a type variable for its parent, whose instantiation resolution does not yet take into account"

        | MemberSignature.Method _ ->
            let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                {
                    ConcreteTypes = state.ConcreteTypes
                    LoadedAssemblies = state._LoadedAssemblies
                    BaseTypes = baseClassTypes
                }

            let ctx, target =
                MethodReferenceResolution.resolve loggerFactory dotnetRuntimeDirs ctx assy m

            let state =
                { state with
                    ConcreteTypes = ctx.ConcreteTypes
                    _LoadedAssemblies = ctx.LoadedAssemblies
                }

            match target with
            | MethodReferenceTarget.Defined (declaringAssembly, method, declaringTypeArguments) when
                (match mem.Parent with
                 | MetadataToken.TypeReference _
                 | MetadataToken.TypeDefinition _ -> true
                 | _ -> false)
                && mentionsTypeVariable declaringTypeArguments.Arguments
                ->
                // A generic definition named without an instantiation: its typical instantiation.
                let state, parent, spelledArguments = resolveParent state

                let declaredByParent =
                    declaringTypeArguments.Arguments
                    |> Seq.forall (fun argument ->
                        match argument with
                        | TypeConcretization.SubstitutionArgument.Formal (owner, _) -> owner = parent.Identity
                        | TypeConcretization.SubstitutionArgument.Closed _
                        | TypeConcretization.SubstitutionArgument.Spelled _ -> false
                    )

                if not declaredByParent then
                    refuse
                        $"names generic type definition %O{parent} without an instantiation, and an ancestor of it declares the method; the typical instantiation of a generic declaring type is not supported"

                let method =
                    declaringAssembly.Methods.[method]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> parent.Generics.[par.SequenceNumber])

                state, declaringAssembly.Name, Choice1Of2 method, spelledArguments
            | MethodReferenceTarget.Defined (declaringAssembly, method, declaringTypeArguments) ->
                let state, parent, spelledArguments = resolveParent state

                // The parent's own arguments, concretised the first time an ancestor's extends
                // clause turns out to spell one of the declaring type's arguments in terms of them.
                let parentHandles (state : TypeSystemState) : TypeSystemState * ImmutableArray<ConcreteTypeHandle> =
                    ((state, ImmutableArray.CreateBuilder spelledArguments.Length), spelledArguments)
                    ||> Seq.fold (fun (state, handles) ty ->
                        let state, handle =
                            TypeSystemState.concretizeType
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                parent.AssemblyFullName
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                ty

                        handles.Add handle
                        state, handles
                    )
                    |> fun (state, handles) -> state, handles.ToImmutable ()

                // One of the declaring type's arguments, which the parent's arguments close.
                let closeArgument
                    (state : TypeSystemState)
                    (handles : ImmutableArray<ConcreteTypeHandle>)
                    (argument : TypeConcretization.SubstitutionArgument)
                    : TypeSystemState * TypeDefn
                    =
                    let closedArguments =
                        TypeConcretization.SubstitutionContext.rebase
                            parent.Identity
                            (handles |> ImmutableArray.map TypeConcretization.SubstitutionArgument.Closed)
                            {
                                TypeConcretization.SubstitutionContext.Arguments = ImmutableArray.Create argument
                            }

                    let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                        {
                            ConcreteTypes = state.ConcreteTypes
                            LoadedAssemblies = state._LoadedAssemblies
                            BaseTypes = baseClassTypes
                        }

                    let closed, ctx =
                        TypeConcretization.concretizeSubstitution
                            ctx
                            (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                            closedArguments

                    let state =
                        { state with
                            ConcreteTypes = ctx.ConcreteTypes
                            _LoadedAssemblies = ctx.LoadedAssemblies
                        }

                    state,
                    Concretization.concreteHandleToTypeDefn
                        baseClassTypes
                        closed.[0]
                        state.ConcreteTypes
                        state._LoadedAssemblies

                let state, _, declaringTypeGenerics =
                    ((state, None, ImmutableArray.CreateBuilder declaringTypeArguments.Arguments.Length),
                     declaringTypeArguments.Arguments)
                    ||> Seq.fold (fun (state, handles, acc) argument ->
                        match argument with
                        | TypeConcretization.SubstitutionArgument.Formal (owner, index) when owner = parent.Identity ->
                            acc.Add spelledArguments.[index]
                            state, handles, acc
                        | TypeConcretization.SubstitutionArgument.Formal (owner, index) ->
                            failwith
                                $"%s{memberName}: the declaring type's arguments mention variable !%d{index} of %O{owner}, not of the parent %O{parent}"
                        | TypeConcretization.SubstitutionArgument.Closed _
                        | TypeConcretization.SubstitutionArgument.Spelled _ ->
                            let state, handles =
                                match handles with
                                | Some handles -> state, handles
                                | None -> parentHandles state

                            let state, closed = closeArgument state handles argument
                            acc.Add closed
                            state, Some handles, acc
                    )
                    |> fun (state, handles, acc) -> state, handles, acc.ToImmutable ()

                let method =
                    declaringAssembly.Methods.[method]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> declaringTypeGenerics.[par.SequenceNumber])

                state, declaringAssembly.Name, Choice1Of2 method, declaringTypeGenerics
            | MethodReferenceTarget.ArrayMethod (arrayType, accessor) ->
                refuse
                    $"names %A{accessor} of the array type %O{arrayType}, which the runtime supplies; callers handle these before resolving a MemberRef"
            | MethodReferenceTarget.Missing ->
                refuse "binds to no method, so CoreCLR throws MissingMethodException; that is not modelled"
            | MethodReferenceTarget.ParentTypeMissing miss ->
                refuse
                    $"has a parent that names no type (%O{miss}), so CoreCLR throws TypeLoadException; that is not modelled"
            | MethodReferenceTarget.ParentAssemblyUnavailable reference ->
                refuse
                    $"has a parent whose load needs %s{reference.FullName}, which no runtime directory supplies, so CoreCLR throws FileNotFoundException; that is not modelled"
            | MethodReferenceTarget.DependsOnInstantiation ->
                refuse
                    "names a method that depends on how a type variable of the referencing context is instantiated, which resolution does not yet take into account"

    /// `resolveMemberWithGenerics` for a reference read in the generic context of a method whose
    /// declaring type and own generic parameters are instantiated at `declaringTypeGenerics` and
    /// `methodGenerics`, memoised in `TypeSystemState._MemberResolutions` on the row and that
    /// context.
    let resolveMember
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (declaringTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (m : MemberReferenceHandle)
        (state : TypeSystemState)
        : TypeSystemState *
          AssemblyName *
          Choice<WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>, FieldReferenceBinding> *
          TypeDefn ImmutableArray
        =
        let key : MemberResolutionKey =
            {
                Assembly = assy.DefinitionFullName
                MemberRow = MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit m : EntityHandle)
                DeclaringTypeGenerics = List.ofSeq declaringTypeGenerics
                MethodGenerics = List.ofSeq methodGenerics
            }

        match Map.tryFind key state._MemberResolutions with
        | Some resolved -> state, resolved.DeclaringAssembly, resolved.Member, resolved.TargetTypeGenerics
        | None ->

        let toTypeDefn (handle : ConcreteTypeHandle) : TypeDefn =
            Concretization.concreteHandleToTypeDefn baseClassTypes handle state.ConcreteTypes state._LoadedAssemblies

        let typeGenericDefns =
            declaringTypeGenerics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        let methodGenericDefns =
            methodGenerics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        let state, declaringAssembly, member', targetTypeGenerics =
            resolveMemberWithGenerics
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                assy
                typeGenericDefns
                methodGenericDefns
                m
                state

        let resolved : ResolvedMemberReference =
            {
                DeclaringAssembly = declaringAssembly
                Member = member'
                TargetTypeGenerics = targetTypeGenerics
            }

        state.WithMemberResolution key resolved, declaringAssembly, member', targetTypeGenerics
