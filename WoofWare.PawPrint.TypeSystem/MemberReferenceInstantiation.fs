namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

/// Why a MemberRef row binds to no member, where binding it is not a `FieldReferenceBinding.Fails`.
[<RequireQualifiedAccess>]
type MemberBindingFailure =
    /// The row is method-shaped, and nothing of its name and signature is where CoreCLR looks, so
    /// binding it throws `MissingMethodException`.
    | NoSuchMethod
    /// A type the binding reaches does not bind: the parent, a type its load reaches, or one a
    /// signature compared on the way names.
    | TypeUnbound of TypeBindingFailure
    /// A shape PawPrint does not implement binding.
    | Unmodelled of UnmodelledBinding

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
        : TypeSystemState * Result<string, MemberBindingFailure>
        =
        let recurse =
            arrayParentName loggerFactory dotnetRuntimeDirs baseClassTypes spellingAssembly

        let decorated (element : TypeDefn) (decoration : string -> string) =
            match recurse element state with
            | state, Ok element -> state, Ok (decoration element)
            | failed -> failed

        match spelling with
        | TypeDefn.Modified modified -> recurse modified.Unmodified state
        | TypeDefn.PrimitiveType primitive ->
            state, Ok (definitionName state (BaseClassTypes.ofPrimitive baseClassTypes primitive).Identity)
        | TypeDefn.FromDefinition (identity, _) -> state, Ok (definitionName state identity)
        | TypeDefn.FromReference (typeRef, _) ->
            match
                TypeSystemState.resolveTypeFromRef
                    loggerFactory
                    dotnetRuntimeDirs
                    spellingAssembly
                    typeRef
                    ImmutableArray.Empty
                    state
            with
            | state, Ok (_, resolved) -> state, Ok (definitionName state resolved.Identity)
            | state, Error failure -> state, Error (MemberBindingFailure.TypeUnbound failure)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> decorated element (fun element -> $"%s{element}[]")
        | TypeDefn.Array (element, rank) ->
            let dimensions = if rank = 1 then "*" else System.String (',', rank - 1)
            decorated element (fun element -> $"%s{element}[%s{dimensions}]")
        | TypeDefn.Pointer element -> decorated element (fun element -> $"%s{element}*")
        | other ->
            // `TypeHandle::GetName` appends an instantiation with `TypeString::AppendInst`, and a
            // function pointer or a type variable has a rendering of its own; none is measured.
            state,
            Error (
                MemberBindingFailure.Unmodelled
                    {
                        Diagnostic =
                            $"binds to no field, and its parent is an array of %O{other}; TODO: name the array element as MissingFieldException's message would; the rendering has not been measured"
                    }
            )

    /// The row, as a diagnostic names it.
    /// The MemberRef row `m` of `assy`, described for a run that stops on it.
    let rowDescription (assy : DumpedAssembly) (m : MemberReferenceHandle) : string =
        let memberName = assy.Strings assy.Members.[m].Name

        let row =
            MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit m : EntityHandle)

        $"MemberRef %s{memberName} (row %d{row}) of %s{assy.DefinitionFullName}"

    /// The failure, described for a run that stops on it: which row of `assy` failed, and why.
    let describeFailure (assy : DumpedAssembly) (m : MemberReferenceHandle) (failure : MemberBindingFailure) : string =
        let prefix = rowDescription assy m

        match failure with
        | MemberBindingFailure.NoSuchMethod ->
            $"%s{prefix} binds to no method, so CoreCLR throws MissingMethodException; that is not modelled"
        | MemberBindingFailure.TypeUnbound failure ->
            $"%s{prefix} reaches a type that does not bind, so CoreCLR throws while compiling the method that uses it; that is not modelled: %s{TypeBindingFailure.describe failure}"
        | MemberBindingFailure.Unmodelled unmodelled -> $"%s{prefix} %s{unmodelled.Diagnostic}"

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
    /// exception where the reference is used. Any other reference that binds no member is the
    /// result's failure: a method reference CoreCLR fails to bind, one whose target depends on how a
    /// type variable of the referencing context is instantiated, and a shape PawPrint does not
    /// implement binding. Fails on a reference naming a method the runtime supplies on an array
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
        : TypeSystemState * Result<ResolvedMemberReference, MemberBindingFailure>
        =
        let mem = assy.Members.[m]
        let memberName : string = assy.Strings mem.Name

        let fails (state : TypeSystemState) (failure : MemberBindingFailure) : TypeSystemState * Result<'a, _> =
            state, Error failure

        let unmodelled (state : TypeSystemState) (diagnostic : string) : TypeSystemState * Result<'a, _> =
            fails
                state
                (MemberBindingFailure.Unmodelled
                    {
                        Diagnostic = diagnostic
                    })

        let bound
            (state : TypeSystemState)
            (declaringAssembly : AssemblyName)
            (member' :
                Choice<WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>, FieldReferenceBinding>)
            (targetTypeGenerics : TypeDefn ImmutableArray)
            =
            state,
            Ok
                {
                    DeclaringAssembly = declaringAssembly
                    Member = member'
                    TargetTypeGenerics = targetTypeGenerics
                }

        let withAssemblies (assemblies : LoadedAssemblies) (state : TypeSystemState) : TypeSystemState =
            { state with
                _LoadedAssemblies = assemblies
            }

        // The parent as the frame instantiates it, and the arguments the row spells for it. A
        // TypeReference or TypeDefinition spells none: naming a generic definition that way names its
        // typical instantiation, which callers recognise by the empty arguments and refuse.
        let resolveParent
            (state : TypeSystemState)
            : TypeSystemState *
              Result<WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn> * ImmutableArray<TypeDefn>, MemberBindingFailure>
            =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                match
                    TypeSystemState.resolveType loggerFactory dotnetRuntimeDirs parent ImmutableArray.Empty assy state
                with
                | state, Ok (_, targetType) -> state, Ok (targetType, ImmutableArray.Empty)
                | state, Error failure -> fails state (MemberBindingFailure.TypeUnbound failure)
            | MetadataToken.TypeDefinition parent ->
                // The bare definition, as a TypeReference parent resolves to it (base chain loaded):
                // its formals stand, rather than being substituted from an environment the parent
                // supplies none of.
                match
                    TypeResolution.resolveTypeFromDefn
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        (TypeDefn.FromDefinition (assy.TypeDefs.[parent].Identity, SignatureTypeKind.Unknown))
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        assy
                        state._LoadedAssemblies
                with
                | assemblies, Ok (_, targetType) ->
                    withAssemblies assemblies state, Ok (targetType, ImmutableArray.Empty)
                | assemblies, Error failure ->
                    fails (withAssemblies assemblies state) (MemberBindingFailure.TypeUnbound failure)
            | MetadataToken.TypeSpecification parent ->
                match
                    TypeSystemState.resolveTypeFromSpec
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        parent
                        assy
                        typeGenerics
                        methodGenerics
                        state
                with
                | state, Ok (_, targetType) -> state, Ok (targetType, targetType.Generics)
                | state, Error failure -> fails state (MemberBindingFailure.TypeUnbound failure)
            | parent -> unmodelled state $"has a parent %O{parent} that names no type with members"

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
            let unvouched () : string option =
                match ParentLoadVouching.vouch baseClassTypes state._LoadedAssemblies assy m with
                | ParentLoadVouch.Vouched -> None
                | ParentLoadVouch.Unvouched reason ->
                    Some
                        $"binds to no field, and PawPrint cannot vouch for every type CoreCLR's load of its parent reaches (%O{reason}), so cannot tell which exception binding throws; TODO: that is not modelled"

            match target with
            | FieldReferenceTarget.Defined (declaringAssembly, field) ->
                // Fields are not inherited, so the parent is the declaring type.
                match resolveParent state with
                | state, Error failure -> fails state failure
                | state, Ok (targetType, spelledArguments) ->

                let field =
                    declaringAssembly.Fields.[field]
                    |> FieldInfo.mapTypeGenerics (fun _ (par, _) -> targetType.Generics.[par.SequenceNumber])

                bound state declaringAssembly.Name (Choice2Of2 (FieldReferenceBinding.Bound field)) spelledArguments
            | FieldReferenceTarget.Missing ->
                match unvouched () with
                | Some diagnostic -> unmodelled state diagnostic
                | None ->

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
                    | MemberReferenceParent.Nominal identity -> state, Ok (definitionName state identity)
                    | MemberReferenceParent.Array arrayType ->
                        arrayParentName loggerFactory dotnetRuntimeDirs baseClassTypes assy arrayType state
                    | other ->
                        state,
                        Error (
                            MemberBindingFailure.Unmodelled
                                {
                                    Diagnostic =
                                        $"binds to no field, but its parent %O{other} is not one FieldReferenceResolution searches"
                                }
                        )

                match parentName with
                | Error failure -> fails state failure
                | Ok parentName ->

                let failure = FieldReferenceFailure.MissingField (parentName, memberName)
                bound state assy.Name (Choice2Of2 (FieldReferenceBinding.Fails failure)) ImmutableArray.Empty
            | FieldReferenceTarget.ParentTypeMissing miss ->
                match unvouched () with
                | Some diagnostic -> unmodelled state diagnostic
                | None ->

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
                bound state assy.Name (Choice2Of2 (FieldReferenceBinding.Fails failure)) ImmutableArray.Empty
            | FieldReferenceTarget.ParentAssemblyUnavailable reference
            | FieldReferenceTarget.SignatureAssemblyUnavailable reference ->
                fails state (MemberBindingFailure.TypeUnbound (TypeBindingFailure.AssemblyUnavailable reference))
            | FieldReferenceTarget.DependsOnInstantiation ->
                unmodelled
                    state
                    "has a type variable for its parent, whose instantiation resolution does not yet take into account"
            | FieldReferenceTarget.Unmodelled unmodelled -> fails state (MemberBindingFailure.Unmodelled unmodelled)

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
                match resolveParent state with
                | state, Error failure -> fails state failure
                | state, Ok (parent, spelledArguments) ->

                let declaredByParent =
                    declaringTypeArguments.Arguments
                    |> Seq.forall (fun argument ->
                        match argument with
                        | TypeConcretization.SubstitutionArgument.Formal (owner, _) -> owner = parent.Identity
                        | TypeConcretization.SubstitutionArgument.Closed _
                        | TypeConcretization.SubstitutionArgument.Spelled _ -> false
                    )

                if not declaredByParent then
                    unmodelled
                        state
                        $"names generic type definition %O{parent} without an instantiation, and an ancestor of it declares the method; the typical instantiation of a generic declaring type is not supported"
                else

                let method =
                    declaringAssembly.Methods.[method]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> parent.Generics.[par.SequenceNumber])

                bound state declaringAssembly.Name (Choice1Of2 method) spelledArguments
            | MethodReferenceTarget.Defined (declaringAssembly, method, declaringTypeArguments) ->
                match resolveParent state with
                | state, Error failure -> fails state failure
                | state, Ok (parent, spelledArguments) ->

                // The parent's own arguments, concretised the first time an ancestor's extends
                // clause turns out to spell one of the declaring type's arguments in terms of them.
                let rec parentHandles
                    (state : TypeSystemState)
                    (handles : ConcreteTypeHandle list)
                    (spelled : TypeDefn list)
                    : TypeSystemState * Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure>
                    =
                    match spelled with
                    | [] -> state, Ok (handles |> List.rev |> ImmutableArray.CreateRange)
                    | ty :: rest ->
                        match
                            TypeSystemState.concretizeType
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                state
                                parent.AssemblyFullName
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                ty
                        with
                        | state, Ok handle -> parentHandles state (handle :: handles) rest
                        | state, Error failure -> state, Error failure

                // One of the declaring type's arguments, which the parent's arguments close.
                let closeArgument
                    (state : TypeSystemState)
                    (handles : ImmutableArray<ConcreteTypeHandle>)
                    (argument : TypeConcretization.SubstitutionArgument)
                    : TypeSystemState * Result<TypeDefn, TypeBindingFailure>
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
                    closed
                    |> Result.map (fun closed ->
                        Concretization.concreteHandleToTypeDefn
                            baseClassTypes
                            closed.[0]
                            state.ConcreteTypes
                            state._LoadedAssemblies
                    )

                // Each of the declaring type's arguments in turn, stopping at the first that does not
                // bind.
                let rec declaringTypeGenerics
                    (state : TypeSystemState)
                    (handles : ImmutableArray<ConcreteTypeHandle> option)
                    (acc : TypeDefn list)
                    (arguments : TypeConcretization.SubstitutionArgument list)
                    : TypeSystemState * Result<ImmutableArray<TypeDefn>, TypeBindingFailure>
                    =
                    match arguments with
                    | [] -> state, Ok (acc |> List.rev |> ImmutableArray.CreateRange)
                    | argument :: rest ->
                        match argument with
                        | TypeConcretization.SubstitutionArgument.Formal (owner, index) when owner = parent.Identity ->
                            declaringTypeGenerics state handles (spelledArguments.[index] :: acc) rest
                        | TypeConcretization.SubstitutionArgument.Formal (owner, index) ->
                            failwith
                                $"%s{memberName}: the declaring type's arguments mention variable !%d{index} of %O{owner}, not of the parent %O{parent}"
                        | TypeConcretization.SubstitutionArgument.Closed _
                        | TypeConcretization.SubstitutionArgument.Spelled _ ->
                            let state, parentArguments =
                                match handles with
                                | Some handles -> state, Ok handles
                                | None -> parentHandles state [] (List.ofSeq spelledArguments)

                            match parentArguments with
                            | Error failure -> state, Error failure
                            | Ok parentArguments ->

                            match closeArgument state parentArguments argument with
                            | state, Ok closed ->
                                declaringTypeGenerics state (Some parentArguments) (closed :: acc) rest
                            | state, Error failure -> state, Error failure

                match declaringTypeGenerics state None [] (List.ofSeq declaringTypeArguments.Arguments) with
                | state, Error failure -> fails state (MemberBindingFailure.TypeUnbound failure)
                | state, Ok declaringTypeGenerics ->

                let method =
                    declaringAssembly.Methods.[method]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> declaringTypeGenerics.[par.SequenceNumber])

                bound state declaringAssembly.Name (Choice1Of2 method) declaringTypeGenerics
            | MethodReferenceTarget.ArrayMethod (arrayType, accessor) ->
                failwith
                    $"%s{rowDescription assy m} names %A{accessor} of the array type %O{arrayType}, which the runtime supplies; callers handle these before resolving a MemberRef"
            | MethodReferenceTarget.Missing -> fails state MemberBindingFailure.NoSuchMethod
            | MethodReferenceTarget.ParentTypeMissing miss ->
                fails state (MemberBindingFailure.TypeUnbound (TypeBindingFailure.TypeAbsent miss))
            | MethodReferenceTarget.ParentAssemblyUnavailable reference ->
                fails state (MemberBindingFailure.TypeUnbound (TypeBindingFailure.AssemblyUnavailable reference))
            | MethodReferenceTarget.Unbound failure -> fails state (MemberBindingFailure.TypeUnbound failure)
            | MethodReferenceTarget.DependsOnInstantiation ->
                unmodelled
                    state
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
        : TypeSystemState * Result<ResolvedMemberReference, MemberBindingFailure>
        =
        let key : MemberResolutionKey =
            {
                Assembly = assy.DefinitionFullName
                MemberRow = MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit m : EntityHandle)
                DeclaringTypeGenerics = List.ofSeq declaringTypeGenerics
                MethodGenerics = List.ofSeq methodGenerics
            }

        match Map.tryFind key state._MemberResolutions with
        | Some resolved -> state, Ok resolved
        | None ->

        let toTypeDefn (handle : ConcreteTypeHandle) : TypeDefn =
            Concretization.concreteHandleToTypeDefn baseClassTypes handle state.ConcreteTypes state._LoadedAssemblies

        let typeGenericDefns =
            declaringTypeGenerics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        let methodGenericDefns =
            methodGenerics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        // A failure is not memoised: binding the row again fails the same way.
        match
            resolveMemberWithGenerics
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                assy
                typeGenericDefns
                methodGenericDefns
                m
                state
        with
        | state, Ok resolved -> state.WithMemberResolution key resolved, Ok resolved
        | state, Error failure -> state, Error failure
