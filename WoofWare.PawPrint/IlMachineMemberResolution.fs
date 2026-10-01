namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineMemberResolution =
    /// <summary>
    /// The member a MemberRef row of <paramref name="assy" /> names, as CoreCLR binds it
    /// (<c>MethodReferenceResolution</c>, <c>FieldReferenceResolution</c>), with the generic
    /// arguments of the type that declares it: the reference's parent as the frame whose generic
    /// context is <paramref name="typeGenerics" /> and <paramref name="methodGenerics" />
    /// instantiates it, or the ancestor of the parent that declares the method.
    /// </summary>
    /// <remarks>
    /// Refuses a reference CoreCLR would fail to bind (it throws while compiling the method that
    /// uses it, which is not modelled), one whose target depends on how a type variable of the
    /// referencing context is instantiated, and one naming a method the runtime supplies on an array
    /// type, which callers handle before resolving.
    /// </remarks>
    let resolveMemberWithGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (typeGenerics : ImmutableArray<TypeDefn>)
        (methodGenerics : ImmutableArray<TypeDefn>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]
        let memberName : string = assy.Strings mem.Name

        let refuse (outcome : string) : 'a =
            failwith
                $"MemberRef %s{memberName} (row %d{MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit m : EntityHandle)}) of %s{assy.DefinitionFullName} %s{outcome}"

        let withAssemblies (assemblies : LoadedAssemblies) (state : IlMachineState) : IlMachineState =
            { state with
                TypeSystem =
                    { state.TypeSystem with
                        _LoadedAssemblies = assemblies
                    }
            }

        // The parent as the frame instantiates it, and the arguments the row spells for it. A
        // TypeReference spells none: naming a generic definition that way names its typical
        // instantiation, which callers recognise by the empty arguments and refuse.
        let resolveParent
            (state : IlMachineState)
            : IlMachineState * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn> * ImmutableArray<TypeDefn>
            =
            match mem.Parent with
            | MetadataToken.TypeReference parent ->
                let state, _, targetType =
                    IlMachineTypeResolution.resolveType loggerFactory parent ImmutableArray.Empty assy state

                state, targetType, ImmutableArray.Empty
            | MetadataToken.TypeSpecification parent ->
                let state, _, targetType =
                    IlMachineTypeResolution.resolveTypeFromSpec
                        loggerFactory
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
                    state.DotnetRuntimeDirs
                    baseClassTypes
                    state.TypeSystem._LoadedAssemblies
                    assy
                    m

            let state = withAssemblies assemblies state

            match target with
            | FieldReferenceTarget.Defined (declaringAssembly, field) ->
                // Fields are not inherited, so the parent is the declaring type.
                let state, targetType, spelledArguments = resolveParent state

                let field =
                    declaringAssembly.Fields.[field]
                    |> FieldInfo.mapTypeGenerics (fun _ (par, _) -> targetType.Generics.[par.SequenceNumber])

                state, declaringAssembly.Name, Choice2Of2 field, spelledArguments
            | FieldReferenceTarget.Missing ->
                refuse "binds to no field, so CoreCLR throws MissingFieldException; that is not modelled"
            | FieldReferenceTarget.ParentTypeMissing miss ->
                refuse
                    $"has a parent that names no type (%O{miss}), so CoreCLR throws TypeLoadException; that is not modelled"
            | FieldReferenceTarget.DependsOnInstantiation ->
                refuse
                    "has a type variable for its parent, whose instantiation resolution does not yet take into account"

        | MemberSignature.Method _ ->
            let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                {
                    ConcreteTypes = state.TypeSystem.ConcreteTypes
                    LoadedAssemblies = state.TypeSystem._LoadedAssemblies
                    BaseTypes = baseClassTypes
                }

            let ctx, target =
                MethodReferenceResolution.resolve loggerFactory state.DotnetRuntimeDirs ctx assy m

            let state =
                { state with
                    TypeSystem =
                        { state.TypeSystem with
                            ConcreteTypes = ctx.ConcreteTypes
                            _LoadedAssemblies = ctx.LoadedAssemblies
                        }
                }

            match target with
            | MethodReferenceTarget.Defined (declaringAssembly, method, declaringTypeArguments) when
                (match mem.Parent with
                 | MetadataToken.TypeReference _ -> true
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
                let parentHandles (state : IlMachineState) : IlMachineState * ImmutableArray<ConcreteTypeHandle> =
                    ((state, ImmutableArray.CreateBuilder spelledArguments.Length), spelledArguments)
                    ||> Seq.fold (fun (state, handles) ty ->
                        let state, handle =
                            IlMachineTypeResolution.concretizeType
                                loggerFactory
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
                    (state : IlMachineState)
                    (handles : ImmutableArray<ConcreteTypeHandle>)
                    (argument : TypeConcretization.SubstitutionArgument)
                    : IlMachineState * TypeDefn
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
                            ConcreteTypes = state.TypeSystem.ConcreteTypes
                            LoadedAssemblies = state.TypeSystem._LoadedAssemblies
                            BaseTypes = baseClassTypes
                        }

                    let closed, ctx =
                        TypeConcretization.concretizeSubstitution
                            ctx
                            (IlMachineTypeResolution.loader loggerFactory state)
                            closedArguments

                    let state =
                        { state with
                            TypeSystem =
                                { state.TypeSystem with
                                    ConcreteTypes = ctx.ConcreteTypes
                                    _LoadedAssemblies = ctx.LoadedAssemblies
                                }
                        }

                    state,
                    Concretization.concreteHandleToTypeDefn
                        baseClassTypes
                        closed.[0]
                        state.TypeSystem.ConcreteTypes
                        state.TypeSystem._LoadedAssemblies

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
            | MethodReferenceTarget.DependsOnInstantiation ->
                refuse
                    "names a method that depends on how a type variable of the referencing context is instantiated, which resolution does not yet take into account"

    let resolveMember
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod

        let key : MemberResolutionKey =
            {
                Assembly = assy.DefinitionFullName
                MemberRow = MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit m : EntityHandle)
                DeclaringTypeGenerics = List.ofSeq executing.DeclaringTypeGenerics
                MethodGenerics = List.ofSeq executing.Generics
            }

        match Map.tryFind key state.TypeSystem._MemberResolutions with
        | Some resolved -> state, resolved.DeclaringAssembly, resolved.Member, resolved.TargetTypeGenerics
        | None ->

        let toTypeDefn (handle : ConcreteTypeHandle) : TypeDefn =
            Concretization.concreteHandleToTypeDefn
                baseClassTypes
                handle
                state.TypeSystem.ConcreteTypes
                state.TypeSystem._LoadedAssemblies

        let typeGenerics =
            executing.DeclaringTypeGenerics
            |> Seq.map toTypeDefn
            |> ImmutableArray.CreateRange

        let methodGenerics =
            executing.Generics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

        let state, declaringAssembly, member', targetTypeGenerics =
            resolveMemberWithGenerics
                loggerFactory
                baseClassTypes
                currentThread
                assy
                typeGenerics
                methodGenerics
                m
                state

        let resolved : ResolvedMemberReference =
            {
                DeclaringAssembly = declaringAssembly
                Member = member'
                TargetTypeGenerics = targetTypeGenerics
            }

        state.WithMemberResolution key resolved, declaringAssembly, member', targetTypeGenerics
