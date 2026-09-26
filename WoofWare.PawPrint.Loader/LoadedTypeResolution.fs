namespace WoofWare.PawPrint

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

/// <summary>
/// Resolve a type name to its definition using only the assemblies already loaded.
/// </summary>
/// <remarks>
/// Where the answer lies in an assembly that has not been loaded, the result is
/// <c>TypeResolutionResult.FirstLoadAssy</c> naming the reference to bind, and the caller loads it
/// and asks again; <c>TypeResolution</c> is that loop. Type forwarders are followed.
/// </remarks>
[<RequireQualifiedAccess>]
module LoadedTypeResolution =
    let private resolveDefinedType
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeResolutionResult
        =
        TypeResolutionResult.Resolved (
            assy,
            ResolvedTypeIdentity.ofDefinitionInAssembly assy.DefinitionFullName ty.TypeDefHandle,
            TypeInfo.applyGenericArgs genericArgs ty
        )

    let rec private resolveTopLevelTypeInAssembly
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (ns : string option)
        (name : string)
        : TypeResolutionResult
        =
        let nsString = ns |> Option.defaultValue ""

        match assy.TryGetTopLevelTypeDef nsString name with
        | Some typeDef -> resolveDefinedType genericArgs assy typeDef
        | None ->
            match assy.TryGetTopLevelExportedType ns name with
            | Some export -> resolveTypeFromExport assy assemblies genericArgs export
            | None ->
                TypeResolutionMiss.TopLevelTypeAbsent (assy.DefinitionFullName, ns, name)
                |> TypeResolutionResult.NotFound

    // No exported-type fallback is needed here (unlike resolveTopLevelTypeInAssembly).
    // This function is only reached after the parent TypeRef has been fully resolved through
    // any forwarding chains, so `assy` is the assembly that *defines* the declaring type.
    // ECMA-335 requires nested types to reside in the same assembly as their declaring type,
    // so the child must be a local TypeDef here.
    // Nested *forwarded* types (ExportedTypeData.ParentExportedType) are handled by a
    // separate code path: resolveTypeFromExport -> resolveExportedTypeByChain.
    and private resolveNestedTypeInAssembly
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (declaringType : ResolvedTypeIdentity)
        (childName : string)
        : TypeResolutionResult
        =
        match assy.TryGetNestedTypeDef declaringType.TypeDefinition.Get childName with
        | Some typeDef -> resolveDefinedType genericArgs assy typeDef
        | None ->
            TypeResolutionMiss.NestedTypeAbsent (
                assy.DefinitionFullName,
                Assembly.fullName assy declaringType,
                childName
            )
            |> TypeResolutionResult.NotFound

    and private resolveTypeRefInAssembly
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (referencedInAssembly : DumpedAssembly)
        (typeRefHandle : TypeReferenceHandle)
        : TypeResolutionResult
        =
        let target = referencedInAssembly.TypeRefs.[typeRefHandle]
        resolveTypeRef assemblies referencedInAssembly genericArgs target

    and private resolveExportedTypeByChain
        (targetAssembly : DumpedAssembly)
        (resolvedParent : ResolvedTypeIdentity option)
        (exportedType : WoofWare.PawPrint.ExportedType)
        : Result<ResolvedTypeIdentity, TypeResolutionMiss>
        =
        match resolvedParent with
        | Some parent ->
            match targetAssembly.TryGetNestedTypeDef parent.TypeDefinition.Get exportedType.Name with
            | Some nested ->
                ResolvedTypeIdentity.ofDefinitionInAssembly targetAssembly.DefinitionFullName nested.TypeDefHandle
                |> Ok
            | None ->
                TypeResolutionMiss.NestedTypeAbsent (
                    targetAssembly.DefinitionFullName,
                    Assembly.fullName targetAssembly parent,
                    exportedType.Name
                )
                |> Error
        | None ->
            let nsString = exportedType.Namespace |> Option.defaultValue ""

            match targetAssembly.TryGetTopLevelTypeDef nsString exportedType.Name with
            | Some topLevel ->
                ResolvedTypeIdentity.ofDefinitionInAssembly targetAssembly.DefinitionFullName topLevel.TypeDefHandle
                |> Ok
            | None ->
                TypeResolutionMiss.TopLevelTypeAbsent (
                    targetAssembly.DefinitionFullName,
                    exportedType.Namespace,
                    exportedType.Name
                )
                |> Error

    and resolveTypeFromExport
        (fromAssembly : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        (genericArgs : ImmutableArray<TypeDefn>)
        (ty : WoofWare.PawPrint.ExportedType)
        : TypeResolutionResult
        =
        match ty.Data with
        | ExportedTypeData.ForwardsTo assyRef ->
            let assyRef = fromAssembly.AssemblyReferences.[assyRef]

            match assemblies.TryResolveReference assyRef with
            | None -> TypeResolutionResult.FirstLoadAssy assyRef
            | Some toAssy -> resolveTopLevelTypeInAssembly assemblies genericArgs toAssy ty.Namespace ty.Name
        | ExportedTypeData.ParentExportedType parentExport ->
            let parent = fromAssembly.ExportedTypes.[parentExport]

            // As with a TypeRef's declaring type, only the parent's identity is wanted.
            match resolveTypeFromExport fromAssembly assemblies ImmutableArray.Empty parent with
            | TypeResolutionResult.FirstLoadAssy assyRef -> TypeResolutionResult.FirstLoadAssy assyRef
            | TypeResolutionResult.NotFound miss -> TypeResolutionResult.NotFound miss
            | TypeResolutionResult.Resolved (targetAssembly, parentIdentity, _) ->

            match resolveExportedTypeByChain targetAssembly (Some parentIdentity) ty with
            | Error miss -> TypeResolutionResult.NotFound miss
            | Ok identity ->
                let typeDef = Assembly.resolveTypeIdentityDefinition targetAssembly identity
                TypeResolutionResult.Resolved (targetAssembly, identity, TypeInfo.applyGenericArgs genericArgs typeDef)
        | ExportedTypeData.AssemblyFile _ ->
            failwithf
                "AssemblyFile exported types are not yet supported while resolving %A from %s"
                ty.Handle
                fromAssembly.DefinitionFullName

    and resolveTypeRef
        (assemblies : LoadedAssemblies)
        (referencedInAssembly : DumpedAssembly)
        (genericArgs : ImmutableArray<TypeDefn>)
        (target : TypeRef)
        : TypeResolutionResult
        =
        match target.ResolutionScope with
        | TypeRefResolutionScope.Assembly r ->
            match referencedInAssembly.AssemblyReferences.TryGetValue r with
            | false, _ ->
                failwithf
                    "AssemblyReferenceHandle %A not found in assembly %s. Available references: %A"
                    r
                    referencedInAssembly.DefinitionFullName
                    (referencedInAssembly.AssemblyReferences.Keys |> Seq.toList)
            | true, assemblyRef ->

            match assemblies.TryResolveReference assemblyRef with
            | None -> TypeResolutionResult.FirstLoadAssy assemblyRef
            | Some assy -> resolveTopLevelTypeInAssembly assemblies genericArgs assy (Some target.Namespace) target.Name
        | TypeRefResolutionScope.TypeRef parent ->
            // Only the declaring type's identity is wanted, so it is resolved open: the arguments
            // belong to the nested type, whose parameter list includes the declaring type's.
            match resolveTypeRefInAssembly assemblies ImmutableArray.Empty referencedInAssembly parent with
            | TypeResolutionResult.FirstLoadAssy assyRef -> TypeResolutionResult.FirstLoadAssy assyRef
            | TypeResolutionResult.NotFound miss -> TypeResolutionResult.NotFound miss
            | TypeResolutionResult.Resolved (targetAssembly, parentIdentity, _) ->
                resolveNestedTypeInAssembly assemblies genericArgs targetAssembly parentIdentity target.Name
        | TypeRefResolutionScope.ModuleDef _ ->
            // The type is defined in the current module.
            resolveTopLevelTypeInAssembly
                assemblies
                genericArgs
                referencedInAssembly
                (Some target.Namespace)
                target.Name
        | TypeRefResolutionScope.ModuleRef moduleRef ->
            failwithf
                "ModuleRef type resolution is not yet supported for type %s.%s in assembly %s via module ref %A"
                target.Namespace
                target.Name
                referencedInAssembly.DefinitionFullName
                moduleRef

    and resolveTopLevelTypeFromName
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        resolveTopLevelTypeInAssembly assemblies genericArgs assy ns name
