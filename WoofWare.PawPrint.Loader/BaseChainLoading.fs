namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// Loading the assemblies a type definition's base-type chain runs through.
[<RequireQualifiedAccess>]
module BaseChainLoading =

    let rec private ensureTypeRefResolved
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (sourceAssembly : DumpedAssembly)
        (typeRef : TypeRef)
        : LoadedAssemblies * Result<DumpedAssembly * TypeDefinitionHandle, BaseChainFailure>
        =
        match LoadedTypeResolution.resolveTypeRef assemblies sourceAssembly ImmutableArray.Empty typeRef with
        | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
            assemblies, Ok (resolvedAssembly, resolvedType.TypeDefHandle)
        | TypeResolutionResult.NotFound miss -> assemblies, Error (BaseChainFailure.BaseTypeAbsent miss)
        | TypeResolutionResult.FirstLoadAssy assemblyRef ->
            let handle, referencedIn = assemblyRef.Handle

            match loadAssembly.TryLoadAssembly assemblies referencedIn handle with
            | Error failure -> assemblies, Error (BaseChainFailure.LoadFailed failure)
            | Ok (newAssemblies, _) ->

            let newAssemblies =
                LoadedAssemblies.assertReferenceBound $"base type reference %s{typeRef.Name}" assemblyRef newAssemblies

            let refreshedSourceAssembly = newAssemblies.[sourceAssembly.Name]
            ensureTypeRefResolved loadAssembly newAssemblies refreshedSourceAssembly typeRef

    let rec private ensureTypeDefnResolved
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (sourceAssembly : DumpedAssembly)
        (ty : TypeDefn)
        : LoadedAssemblies * Result<DumpedAssembly * TypeDefinitionHandle, BaseChainFailure>
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, _) ->
            ensureTypeDefnResolved loadAssembly assemblies sourceAssembly generic
        // A custom modifier annotates the signature; the type definition being named is the
        // unmodified one. Stepping into `Modifier` would resolve `InAttribute`/`IsVolatile`/etc.
        | TypeDefn.Modified m -> ensureTypeDefnResolved loadAssembly assemblies sourceAssembly m.Unmodified
        | TypeDefn.FromDefinition (identity, _) ->
            let resolvedAssembly = assemblies.ByDefinitionName identity.AssemblyFullName
            assemblies, Ok (resolvedAssembly, identity.TypeDefinition.Get)
        | TypeDefn.FromReference (typeRef, _) -> ensureTypeRefResolved loadAssembly assemblies sourceAssembly typeRef
        | unexpected ->
            failwithf
                "Unexpected TypeDefn shape while resolving base type from %s: %O"
                sourceAssembly.DefinitionFullName
                unexpected

    /// <remarks>
    /// This threads the <c>DumpedAssembly</c> itself rather than its <c>AssemblyName</c>, and
    /// deliberately so: <c>LoadedAssemblies</c> is keyed by definition <em>full name</em>, and
    /// <c>AssemblyName.FullName</c> re-formats that string from its components on every single
    /// access. This walk runs on the type-resolution hot path, so a lookup per link is not free.
    /// Each step already holds the assembly it needs — for a TypeDef link it is the same one, and
    /// for a TypeRef/TypeSpec link the resolver hands back the canonical instance.
    /// </remarks>
    let rec private ensureBaseTypeAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (assy : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo option)
        : LoadedAssemblies * BaseChainFailure option
        =
        match baseTypeInfo with
        | None -> assemblies, None
        | Some (BaseTypeInfo.TypeDef handle) ->
            let baseType = assy.TypeDefs.[handle]
            ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy baseType.BaseType
        | Some (BaseTypeInfo.TypeRef handle) ->
            let typeRef = assy.TypeRefs.[handle]

            match ensureTypeRefResolved loadAssembly assemblies assy typeRef with
            | newAssemblies, Error failure -> newAssemblies, Some failure
            | newAssemblies, Ok (resolvedAssembly, resolvedHandle) ->

            let resolvedType = resolvedAssembly.TypeDefs.[resolvedHandle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies resolvedAssembly resolvedType.BaseType
        | Some (BaseTypeInfo.TypeSpec handle) ->
            let typeSpec = assy.TypeSpecs.[handle].Signature

            match ensureTypeDefnResolved loadAssembly assemblies assy typeSpec with
            | newAssemblies, Error failure -> newAssemblies, Some failure
            | newAssemblies, Ok (resolvedAssembly, resolvedHandle) ->

            let resolvedType = resolvedAssembly.TypeDefs.[resolvedHandle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies resolvedAssembly resolvedType.BaseType

    /// <summary>
    /// Load every assembly reachable from the base-type chain of the given type definition, or
    /// report the first reference that would not bind.
    /// </summary>
    /// <remarks>
    /// <para><paramref name="assy"/> must be the canonical instance for the assembly which defines
    /// it.</para>
    /// <para>The returned load context carries every load the walk managed before it stopped, so a
    /// caller that adopts it on the failure path does not lose an assembly that really was read —
    /// which a guest can observe, since loaded assemblies are enumerable.</para>
    /// </remarks>
    let tryEnsureTypeDefinitionBaseAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (assy : DumpedAssembly)
        (typeDefinitionHandle : TypeDefinitionHandle)
        : LoadedAssemblies * BaseChainFailure option
        =
        let typeDef = assy.TypeDefs.[typeDefinitionHandle]
        ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy typeDef.BaseType

    /// As <see cref="tryEnsureTypeDefinitionBaseAssembliesLoaded"/>, for the callers that have
    /// nowhere to put a failed bind.
    let ensureTypeDefinitionBaseAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (assy : DumpedAssembly)
        (typeDefinitionHandle : TypeDefinitionHandle)
        : LoadedAssemblies
        =
        match tryEnsureTypeDefinitionBaseAssembliesLoaded loadAssembly assemblies assy typeDefinitionHandle with
        | assemblies, None -> assemblies
        | _, Some failure -> failwith (string<BaseChainFailure> failure)
