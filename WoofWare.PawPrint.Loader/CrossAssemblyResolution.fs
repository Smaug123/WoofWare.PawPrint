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

[<RequireQualifiedAccess>]
module Assembly =
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

[<RequireQualifiedAccess>]
module DumpedAssembly =
    let private getName (a : DumpedAssembly) : string = a.DefinitionFullName

    let private getTypeDef (a : DumpedAssembly) (h : TypeDefinitionHandle) : TypeInfo<TypeDefn, TypeDefn> =
        a.TypeDefs.[h]
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

    let private getTypeRef
        (loadedAssemblies : LoadedAssemblies)
        (a : DumpedAssembly)
        (h : TypeReferenceHandle)
        : DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeRef loadedAssemblies a ImmutableArray.Empty a.TypeRefs.[h] with
        | TypeResolutionResult.Resolved (resultAssy, _, typeInfo) -> resultAssy, typeInfo
        | TypeResolutionResult.FirstLoadAssy _ ->
            failwith "seems pretty unlikely that we could have constructed this object without loading its base type"
        | TypeResolutionResult.NotFound miss ->
            failwithf "Base type reference from %s does not resolve: %O" a.DefinitionFullName miss

    let private getTypeSpec
        (loadedAssemblies : LoadedAssemblies)
        (a : DumpedAssembly)
        (h : TypeSpecificationHandle)
        : DumpedAssembly * TypeDefinitionHandle
        =
        let signature = a.TypeSpecs.[h].Signature

        let rec go (currentAssembly : DumpedAssembly) (ty : TypeDefn) =
            match ty with
            | TypeDefn.GenericInstantiation (generic, _) -> go currentAssembly generic
            // A custom modifier annotates the signature; the type definition being named is the
            // unmodified one. Stepping into `Modifier` would resolve `InAttribute`/`IsVolatile`/etc.
            | TypeDefn.Modified m -> go currentAssembly m.Unmodified
            | TypeDefn.FromDefinition (identity, _) ->
                let resolvedAssembly = loadedAssemblies.ByDefinitionName identity.AssemblyFullName
                let resolvedType = resolvedAssembly.TypeDefs.[identity.TypeDefinition.Get]
                resolvedAssembly, resolvedType.TypeDefHandle
            | TypeDefn.FromReference (typeRef, _) ->
                match Assembly.resolveTypeRef loadedAssemblies currentAssembly ImmutableArray.Empty typeRef with
                | TypeResolutionResult.FirstLoadAssy assyRef ->
                    failwithf
                        "Base type traversal unexpectedly needed to load assembly %s while resolving %O from %s"
                        assyRef.FullName
                        signature
                        a.DefinitionFullName
                | TypeResolutionResult.NotFound miss ->
                    failwithf "Base type traversal could not resolve %O from %s: %O" signature a.DefinitionFullName miss
                | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
                    resolvedAssembly, resolvedType.TypeDefHandle
            | unexpected ->
                failwithf
                    "Unexpected TypeSpec base type shape while resolving %O from %s: %O"
                    signature
                    a.DefinitionFullName
                    unexpected

        go a signature

    let private assemblies (loadedAssemblies : LoadedAssemblies) (identity : string) : DumpedAssembly =
        loadedAssemblies.ByDefinitionName identity

    /// ECMA "value type": transitively inherits from System.ValueType (possibly via System.Enum),
    /// but is NOT exactly System.ValueType or System.Enum themselves.
    let isValueType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isValueType
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// CoreCLR's `MethodTable::IsEnum`: derives from System.Enum, and is not System.Enum itself.
    /// See <see cref="TypeInfo.isEnum"/> for why this decides the MethodTable category.
    let isEnum
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isEnum
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// Convenience: not a value type.
    let isReferenceType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isReferenceType
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// ECMA "byref-like": a value type that may not appear on the heap (a C# <c>ref struct</c>).
    ///
    /// CoreCLR derives this from the <c>IsByRefLikeAttribute</c> application, but only for types it
    /// has already classified as value classes: the attribute read sits inside the
    /// <c>fIsValueClass = true</c> branch of <c>MethodTableBuilder::BuildMethodTableThrowing</c>
    /// (methodtablebuilder.cpp:1449). So a *class* carrying the attribute — which C# cannot emit but
    /// IL can, since <c>AttributeUsage</c> binds only the compiler — is not byref-like, and this
    /// gate is what makes <see cref="TypeInfo.HasIsByRefLikeAttribute"/> into the classification.
    ///
    /// Note this is a question about a *nominal* type. CoreCLR's <c>TypeHandle::IsByRefLike</c>
    /// (typehandle.cpp:1061) answers <c>false</c> for every TypeDesc, so byrefs, pointers, function
    /// pointers and arrays are never byref-like however their element type is declared.
    let isByRefLike
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        ty.HasIsByRefLikeAttribute && isValueType bct loadedAssemblies ty

    /// Metadata layout kind: ValueType for value types, Class otherwise. Note that System.Enum and
    /// System.ValueType themselves encode as Class, matching real CLR signature encoding.
    let signatureTypeKind
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : SignatureTypeKind
        =
        TypeInfo.signatureTypeKind
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    let typeInfoToTypeDefn
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ti : TypeInfo<TypeDefn, TypeDefn>)
        : TypeDefn
        =
        TypeInfo.toTypeDefn
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ti

    let typeInfoToTypeDefn'
        (bct : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        =
        ti
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)
        |> typeInfoToTypeDefn bct assemblies

[<RequireQualifiedAccess>]
module AssemblyApi =
    let resolveTypeRef = Assembly.resolveTypeRef
    let resolveTopLevelTypeFromName = Assembly.resolveTopLevelTypeFromName

    let resolveTypeFromExport = Assembly.resolveTypeFromExport
