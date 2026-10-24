namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

/// What an instruction does with the member a MemberRef names, which decides whether CoreCLR lets
/// the reference's parent name a generic definition without an instantiation.
[<RequireQualifiedAccess>]
type MemberReferenceUse =
    /// `ldtoken`, which `CEEInfo::resolveToken` lets name a member of a generic definition's typical
    /// instantiation (`PermitUninstDefOrRef`).
    | Ldtoken
    /// Any other instruction, named for diagnostics, for which `resolveToken` refuses such a parent
    /// (`FailIfUninstDefOrRef`).
    | Other of opName : string

/// What `IlMachineMemberResolution.resolveMember` found for a MemberRef row.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type MemberReferenceResolution =
    /// The member the row names, the assembly declaring it, and the declaring type's generic
    /// arguments, read from the row's parent.
    | Resolved of
        declaringAssembly : AssemblyName *
        member' :
            Choice<WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>, FieldReferenceBinding> *
        declaringTypeArguments : TypeDefn ImmutableArray
    /// The row names its member through a type CoreCLR refuses a token of (`TypeOperand.ofDefinition`),
    /// so the instruction raises `InvalidProgramException`, with that type's default message, rather
    /// than reaching the member. Never for `ldtoken`, which CoreCLR lets name it.
    | RefusedParent of TypeOperandRefusal

[<RequireQualifiedAccess>]
module IlMachineMemberResolution =
    /// <summary>
    /// Refuse to execute a member that MemberRef row <paramref name="m" /> of
    /// <paramref name="assy" /> names through a generic definition with no instantiation: a
    /// TypeReference or TypeDefinition parent naming a generic type, whichever type declares the
    /// member. CoreCLR's <c>CEEInfo::resolveToken</c> loads such a parent with
    /// <c>FailIfUninstDefOrRef</c> for every instruction but <c>ldtoken</c>, so it throws
    /// <c>TypeLoadException</c>; that is not modelled.
    /// </summary>
    /// <remarks>
    /// The row must already have been resolved, so that the parent's assembly is loaded. Without
    /// this, the caller would instantiate the definition from the executing frame's own generic
    /// arguments, which is a type the reference does not name.
    /// </remarks>
    let private refuseUninstantiatedParent
        (opName : string)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : unit
        =
        let parent =
            match assy.Members.[m].Parent with
            | MetadataToken.TypeDefinition handle -> Some assy.TypeDefs.[handle]
            | MetadataToken.TypeReference handle ->
                match
                    LoadedTypeResolution.resolveTypeRef
                        state.TypeSystem._LoadedAssemblies
                        assy
                        ImmutableArray.Empty
                        assy.TypeRefs.[handle]
                with
                | TypeResolutionResult.Resolved (definedIn, _, resolved) ->
                    Some definedIn.TypeDefs.[resolved.TypeDefHandle]
                | other ->
                    failwith
                        $"BUG: %s{opName}: the TypeReference parent of a MemberRef in %s{assy.DefinitionFullName} resolved when the row was resolved, but not now: %O{other}"
            | _ -> None

        match parent with
        | Some definition when not definition.Generics.IsEmpty ->
            failwith
                $"TODO: raise TypeLoadException: %s{opName} names a member through generic type definition %s{definition.Namespace}.%s{definition.Name} with no instantiation, which CoreCLR refuses for every instruction but ldtoken; that is not modelled"
        | _ -> ()

    /// <summary>
    /// Why CoreCLR refuses an instruction whose token is MemberRef row <paramref name="m" /> of
    /// <paramref name="assy" /> for the type the row's parent names, or <c>None</c>: that type is
    /// the token's in <c>CEEInfo::resolveToken</c>, which refuses it as <c>TypeOperand.ofDefinition</c>
    /// says, after it has found the member.
    /// </summary>
    /// <remarks>
    /// The row must already have been resolved, so that the parent's assembly is loaded.
    /// </remarks>
    let private parentRefusal
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : TypeOperandRefusal option
        =
        let ofReference (typeRef : TypeRef) : TypeOperandRefusal option =
            match
                LoadedTypeResolution.resolveTypeRef state.TypeSystem._LoadedAssemblies assy ImmutableArray.Empty typeRef
            with
            | TypeResolutionResult.Resolved (definedIn, _, resolved) ->
                TypeOperand.ofDefinition baseClassTypes definedIn.TypeDefs.[resolved.TypeDefHandle].Identity
            | other ->
                failwith
                    $"BUG: the parent of a MemberRef in %s{assy.DefinitionFullName} resolved when the row was resolved, but not now: %O{other}"

        // A TypeSpec parent may spell `void` itself, or name it as a value type. Custom modifiers do
        // not change the type a signature names. Any other spelling names a type that is not
        // `System.Void`; a byref, which `resolveToken` would refuse too, has no members to find.
        let rec ofSpelling (spelling : TypeDefn) : TypeOperandRefusal option =
            match spelling with
            | TypeDefn.Void -> Some TypeOperandRefusal.Void
            | TypeDefn.Modified modified -> ofSpelling modified.Unmodified
            | TypeDefn.FromDefinition (identity, _) -> TypeOperand.ofDefinition baseClassTypes identity
            | TypeDefn.FromReference (typeRef, _) -> ofReference typeRef
            | TypeDefn.PrimitiveType _
            | TypeDefn.GenericInstantiation _
            | TypeDefn.Array _
            | TypeDefn.OneDimensionalArrayLowerBoundZero _
            | TypeDefn.Pointer _
            | TypeDefn.Byref _
            | TypeDefn.Pinned _
            | TypeDefn.FunctionPointer _
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _ -> None

        match assy.Members.[m].Parent with
        | MetadataToken.TypeDefinition handle -> TypeOperand.ofDefinition baseClassTypes assy.TypeDefs.[handle].Identity
        | MetadataToken.TypeReference handle -> ofReference assy.TypeRefs.[handle]
        | MetadataToken.TypeSpecification handle -> ofSpelling assy.TypeSpecs.[handle].Signature
        | _ -> None

    /// `TypeLoadException._resourceId` for a type reference that names nothing:
    /// `IDS_CLASSLOAD_GENERAL` (mscorrc/resource.h), "Could not load type '%1' from assembly '%2'."
    [<Literal>]
    let private IdsClassLoadGeneral : int = 0x80131522

    /// The exception binding a field reference throws, and the fields CoreCLR's EE sets on it:
    /// `MemberLoader::ThrowMissingFieldException`'s message, or `ClassLoader::ThrowTypeLoadException`'s
    /// message, type name, assembly name and resource id, which is where `TypeLoadException.TypeName`
    /// comes from.
    let bindingFailureException
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (failure : FieldReferenceFailure)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> * RuntimeExceptionField list
        =
        match failure with
        | FieldReferenceFailure.MissingField (parent, name) ->
            baseClassTypes.MissingFieldException,
            [ RuntimeExceptionField.Message $"Field not found: '%s{parent}.%s{name}'." ]
        | FieldReferenceFailure.ParentTypeMissing (typeName, searchedIn) ->
            baseClassTypes.TypeLoadException,
            [
                RuntimeExceptionField.Message $"Could not load type '%s{typeName}' from assembly '%s{searchedIn}'."
                RuntimeExceptionField.TypeLoadClassName typeName
                RuntimeExceptionField.TypeLoadAssemblyName searchedIn
                RuntimeExceptionField.TypeLoadResourceId IdsClassLoadGeneral
            ]

    /// The generic definition a TypeSpec spelling in `assy` names without an instantiation, if any: a
    /// nominal type, other than an instantiation's own generic definition, whose definition has
    /// generic parameters. No signature may spell one; CoreCLR's `SigPointer::GetTypeHandleThrowing`
    /// cannot load it. A type reference that does not resolve is not this rule's concern.
    let rec private uninstantiatedDefinitionIn
        (state : IlMachineState)
        (assy : DumpedAssembly)
        (spelling : TypeDefn)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        let recurse = uninstantiatedDefinitionIn state assy

        let ifGeneric (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
            if definition.Generics.IsEmpty then
                None
            else
                Some definition

        match spelling with
        | TypeDefn.Modified modified -> recurse modified.Unmodified
        | TypeDefn.GenericInstantiation (_, args) -> args |> Seq.tryPick recurse
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> recurse element
        | TypeDefn.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> []
                | MethodReturnType.Returns ret -> [ ret ]

            returned @ signature.ParameterTypes |> List.tryPick recurse
        | TypeDefn.FromDefinition (identity, _) ->
            state.TypeSystem._LoadedAssemblies
                .ByDefinitionName(identity.AssemblyFullName)
                .TypeDefs.[identity.TypeDefinition.Get]
            |> ifGeneric
        | TypeDefn.FromReference (typeRef, _) ->
            match
                LoadedTypeResolution.resolveTypeRef state.TypeSystem._LoadedAssemblies assy ImmutableArray.Empty typeRef
            with
            | TypeResolutionResult.Resolved (definedIn, _, resolved) ->
                ifGeneric definedIn.TypeDefs.[resolved.TypeDefHandle]
            | TypeResolutionResult.NotFound _
            | TypeResolutionResult.FirstLoadAssy _ -> None
            | TypeResolutionResult.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> None

    /// `MemberReferenceInstantiation.resolveMemberWithGenerics` against the machine's type system.
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
          Choice<WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>, FieldReferenceBinding> *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let typeSystem, declaringAssembly, member', targetTypeGenerics =
            MemberReferenceInstantiation.resolveMemberWithGenerics
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                assy
                typeGenerics
                methodGenerics
                m
                state.TypeSystem

        state.WithTypeSystem typeSystem, declaringAssembly, member', targetTypeGenerics

    /// `MemberReferenceInstantiation.resolveMember` against the machine's type system, in the
    /// generic context of the method `currentThread` is executing. For any use but `ldtoken`,
    /// refuses a row whose parent names a generic definition without an instantiation, as CoreCLR
    /// refuses it, and answers `RefusedParent` for a row whose member CoreCLR finds but whose parent
    /// it refuses the instruction over.
    let resolveMember
        (use' : MemberReferenceUse)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * MemberReferenceResolution
        =
        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod

        let typeSystem, declaringAssembly, member', targetTypeGenerics =
            MemberReferenceInstantiation.resolveMember
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                assy
                executing.DeclaringTypeGenerics
                executing.Generics
                m
                state.TypeSystem

        let state = state.WithTypeSystem typeSystem

        let resolved =
            state, MemberReferenceResolution.Resolved (declaringAssembly, member', targetTypeGenerics)

        match member' with
        // A parent CoreCLR cannot load is the failure the binding already reports.
        | Choice2Of2 (FieldReferenceBinding.Fails (FieldReferenceFailure.ParentTypeMissing _)) -> resolved
        | _ ->

        let opName =
            match use' with
            | MemberReferenceUse.Ldtoken -> "ldtoken"
            | MemberReferenceUse.Other opName -> opName

        // A parent signature naming a generic definition with no instantiation cannot be loaded,
        // whatever the instruction.
        match assy.Members.[m].Parent with
        | MetadataToken.TypeSpecification handle ->
            match uninstantiatedDefinitionIn state assy assy.TypeSpecs.[handle].Signature with
            | Some definition ->
                failwith
                    $"TODO: raise TypeLoadException: %s{opName} names a member of a type whose signature spells generic type definition %s{definition.Namespace}.%s{definition.Name} with no instantiation, which CoreCLR cannot load; that is not modelled"
            | None -> ()
        | _ -> ()

        match use' with
        | MemberReferenceUse.Ldtoken -> resolved
        | MemberReferenceUse.Other opName ->
            refuseUninstantiatedParent opName assy m state

            match member' with
            // A field that binds nothing raises its own failure first, as CoreCLR finds no member
            // to refuse the parent over.
            | Choice2Of2 (FieldReferenceBinding.Fails _) -> resolved
            | Choice1Of2 _
            | Choice2Of2 (FieldReferenceBinding.Bound _) ->
                match parentRefusal baseClassTypes assy m state with
                | Some refusal -> state, MemberReferenceResolution.RefusedParent refusal
                | None -> resolved
