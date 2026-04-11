namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

/// <summary>
/// Represents a .NET assembly definition.
/// This is a strongly-typed representation of AssemblyDefinition from System.Reflection.Metadata.
/// </summary>
type AssemblyDefinition =
    {
        /// <summary>
        /// The fully specified name of the assembly, including name, version, culture, and public key token.
        /// </summary>
        Name : AssemblyName
    }

[<RequireQualifiedAccess>]
module AssemblyDefinition =
    let make (assy : System.Reflection.Metadata.AssemblyDefinition) : AssemblyDefinition =
        {
            Name = assy.GetAssemblyName ()
        }

/// <summary>
/// Represents a fully parsed .NET assembly with all its metadata components.
/// This serves as the main container for accessing assembly information in the PawPrint library.
/// </summary>
type DumpedAssembly =
    {
        OriginalPath : string option

        /// <summary>Logger for recording information about this assembly.</summary>
        Logger : ILogger

        /// <summary>
        /// Dictionary of all type definitions in this assembly, keyed by their handle.
        /// </summary>
        TypeDefs :
            IReadOnlyDictionary<TypeDefinitionHandle, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>

        /// <summary>
        /// Dictionary of all type references in this assembly, keyed by their handle.
        /// </summary>
        TypeRefs : IReadOnlyDictionary<TypeReferenceHandle, WoofWare.PawPrint.TypeRef>

        /// <summary>
        /// Dictionary of all type specifications in this assembly, keyed by their handle.
        /// Type specifications represent complex types like generic instantiations.
        /// </summary>
        TypeSpecs : IReadOnlyDictionary<TypeSpecificationHandle, WoofWare.PawPrint.TypeSpec>

        /// <summary>
        /// Dictionary of all method definitions in this assembly, keyed by their handle.
        /// </summary>
        Methods :
            IReadOnlyDictionary<
                MethodDefinitionHandle,
                WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
             >

        /// <summary>
        /// Dictionary of all member references in this assembly, keyed by their handle.
        /// </summary>
        Members : IReadOnlyDictionary<MemberReferenceHandle, WoofWare.PawPrint.MemberReference<MetadataToken>>

        /// <summary>
        /// Dictionary of all field definitions in this assembly, keyed by their handle.
        /// </summary>
        Fields :
            IReadOnlyDictionary<FieldDefinitionHandle, WoofWare.PawPrint.FieldInfo<GenericParamFromMetadata, TypeDefn>>

        /// <summary>
        /// The entry point method of the assembly, if one exists.
        /// </summary>
        MainMethod : MethodDefinitionHandle option

        /// <summary>
        /// Dictionary of all method specifications in this assembly, keyed by their handle.
        /// Method specifications typically represent generic method instantiations.
        /// </summary>
        MethodSpecs : ImmutableDictionary<MethodSpecificationHandle, MethodSpec>

        /// <summary>
        /// Function to resolve string tokens to their actual string values.
        /// </summary>
        Strings : StringToken -> string

        /// <summary>
        /// Dictionary of all assembly references in this assembly, keyed by their handle.
        /// </summary>
        AssemblyReferences : ImmutableDictionary<AssemblyReferenceHandle, WoofWare.PawPrint.AssemblyReference>

        /// <summary>
        /// Information about this assembly.
        /// </summary>
        ThisAssemblyDefinition : AssemblyDefinition

        /// <summary>
        /// The root namespace of this assembly.
        /// </summary>
        RootNamespace : Namespace

        /// <summary>
        /// Dictionary of all non-root namespaces in this assembly, keyed by their name components.
        /// </summary>
        NonRootNamespaces : ImmutableDictionary<string list, Namespace>

        /// <summary>
        /// The PE reader for the underlying assembly file.
        /// TODO: work out how to render all the strings up front, then drop this.
        /// </summary>
        PeReader : PEReader

        /// <summary>
        /// Dictionary of all custom attributes in this assembly, keyed by their handle.
        /// </summary>
        Attributes : ImmutableDictionary<CustomAttributeHandle, WoofWare.PawPrint.CustomAttribute>

        /// <summary>
        /// Dictionary of all exported types in this assembly, keyed by their handle.
        /// </summary>
        ExportedTypes : ImmutableDictionary<ExportedTypeHandle, WoofWare.PawPrint.ExportedType>

        /// <summary>
        /// Internal lookup for top-level type definitions by namespace and name.
        /// </summary>
        _TopLevelTypeDefsLookup :
            ImmutableDictionary<string * string, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>

        /// <summary>
        /// Internal lookup for nested type definitions by declaring type and simple name.
        /// </summary>
        _NestedTypeDefsLookup :
            ImmutableDictionary<
                ComparableTypeDefinitionHandle * string,
                WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>
             >

        /// <summary>
        /// Internal lookup for top-level exported types by namespace and name.
        /// </summary>
        _TopLevelExportedTypesLookup : ImmutableDictionary<string option * string, WoofWare.PawPrint.ExportedType>

        /// <summary>
        /// Internal lookup for nested exported types by parent export and simple name.
        /// </summary>
        _NestedExportedTypesLookup : ImmutableDictionary<ExportedTypeHandle * string, WoofWare.PawPrint.ExportedType>
    }

    static member internal BuildTopLevelTypeDefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeDefs : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> seq)
        : ImmutableDictionary<string * string, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeDefs do
            if not ty.IsNested then
                let key = ty.Namespace, ty.Name

                if keys.Add key then
                    result.Add (key, ty)
                else
                    logger.LogDebug (
                        "Duplicate top-level type defs from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                        name,
                        ty.Namespace,
                        ty.Name
                    )

        result.ToImmutable ()

    static member internal BuildNestedTypeDefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeDefs : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeDefs do
            if ty.IsNested then
                let key = (ComparableTypeDefinitionHandle.Make ty.DeclaringType, ty.Name)

                if keys.Add key then
                    result.Add (key, ty)
                else
                    logger.LogDebug (
                        "Duplicate nested type defs from assembly {ThisAssemblyName}: parent {DeclaringTypeHandle}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                        name,
                        ty.DeclaringType,
                        ty.Name
                    )

        result.ToImmutable ()

    static member internal BuildTopLevelExportedTypesLookup
        (logger : ILogger)
        (name : AssemblyName)
        (types : WoofWare.PawPrint.ExportedType seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in types do
            match ty.Data with
            | ExportedTypeData.ParentExportedType _ -> ()
            | _ ->
                let key = ty.Namespace, ty.Name

                if keys.Add key then
                    result.Add (key, ty)
                else
                    logger.LogDebug (
                        "Duplicate top-level exported types from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                        name,
                        ty.Namespace,
                        ty.Name
                    )

        result.ToImmutable ()

    static member internal BuildNestedExportedTypesLookup
        (logger : ILogger)
        (name : AssemblyName)
        (types : WoofWare.PawPrint.ExportedType seq)
        : ImmutableDictionary<ExportedTypeHandle * string, WoofWare.PawPrint.ExportedType>
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in types do
            match ty.Data with
            | ExportedTypeData.ParentExportedType parent ->
                let key = (parent, ty.Name)

                if keys.Add key then
                    result.Add (key, ty)
                else
                    logger.LogDebug (
                        "Duplicate nested exported types from assembly {ThisAssemblyName}: parent {ParentExportedType}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                        name,
                        parent,
                        ty.Name
                    )
            | _ -> ()

        result.ToImmutable ()

    member this.Name = this.ThisAssemblyDefinition.Name

    member this.TryGetTopLevelTypeDef
        (``namespace`` : string)
        (name : string)
        : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        match this._TopLevelTypeDefsLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TryGetNestedTypeDef
        (declaringType : TypeDefinitionHandle)
        (name : string)
        : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        match this._NestedTypeDefsLookup.TryGetValue ((ComparableTypeDefinitionHandle.Make declaringType, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TryGetTopLevelExportedType
        (``namespace`` : string option)
        (name : string)
        : WoofWare.PawPrint.ExportedType option
        =
        match this._TopLevelExportedTypesLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TryGetNestedExportedType
        (parent : ExportedTypeHandle)
        (name : string)
        : WoofWare.PawPrint.ExportedType option
        =
        match this._NestedExportedTypesLookup.TryGetValue ((parent, name)) with
        | false, _ -> None
        | true, v -> Some v

    interface IDisposable with
        member this.Dispose () = this.PeReader.Dispose ()


type TypeResolutionResult =
    | FirstLoadAssy of WoofWare.PawPrint.AssemblyReference
    | Resolved of DumpedAssembly * ResolvedTypeIdentity * TypeInfo<TypeDefn, TypeDefn>

    override this.ToString () : string =
        match this with
        | TypeResolutionResult.FirstLoadAssy a -> $"FirstLoadAssy(%s{a.Name.FullName})"
        | TypeResolutionResult.Resolved (assy, identity, ty) ->
            $"Resolved(%s{assy.Name.FullName}: %O{identity} {string<TypeInfo<TypeDefn, TypeDefn>> ty})"

[<RequireQualifiedAccess>]
module Assembly =
    let read (loggerFactory : ILoggerFactory) (originalPath : string option) (dllBytes : Stream) : DumpedAssembly =
        let peReader = new PEReader (dllBytes)
        let metadataReader = peReader.GetMetadataReader ()

        let assy = metadataReader.GetAssemblyDefinition () |> AssemblyDefinition.make

        let entryPoint =
            peReader.PEHeaders.CorHeader.EntryPointTokenOrRelativeVirtualAddress
            |> fun x -> if x = 0 then None else Some x

        let entryPointMethod =
            entryPoint |> Option.map MetadataTokens.MethodDefinitionHandle

        let assemblyRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ref in metadataReader.AssemblyReferences do
                builder.Add (ref, AssemblyReference.make (ref, assy.Name) (metadataReader.GetAssemblyReference ref))

            builder.ToImmutable ()

        let typeRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeReferences do
                builder.Add (ty, TypeRef.make metadataReader ty)

            builder.ToImmutable ()

        let typeDefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeDefinitions do
                builder.Add (ty, TypeInfo.read loggerFactory peReader assy.Name metadataReader ty)

            builder.ToImmutable ()

        // TODO: this probably misses any methods out which aren't associated with a type definition?
        let methods =
            typeDefs
            |> Seq.collect (fun (KeyValue (_, ty)) -> ty.Methods |> List.map (fun mi -> KeyValuePair (mi.Handle, mi)))
            |> ImmutableDictionary.CreateRange

        let methodSpecs =
            Seq.init
                (metadataReader.GetTableRowCount TableIndex.MethodSpec)
                (fun i ->
                    let i = i + 1
                    let handle = MetadataTokens.MethodSpecificationHandle i
                    KeyValuePair (handle, MethodSpec.make assy.Name (metadataReader.GetMethodSpecification handle))
                )
            |> ImmutableDictionary.CreateRange

        let typeSpecs =
            let result = ImmutableDictionary.CreateBuilder ()

            for i = 1 to metadataReader.GetTableRowCount TableIndex.TypeSpec do
                let handle = MetadataTokens.TypeSpecificationHandle i
                result.Add (handle, metadataReader.GetTypeSpecification handle |> TypeSpec.make assy.Name handle)

            result.ToImmutable ()

        let memberReferences =
            let builder = ImmutableDictionary.CreateBuilder ()

            for c in metadataReader.MemberReferences do
                builder.Add (
                    c,
                    MemberReference.make<MetadataToken>
                        metadataReader.GetBlobReader
                        metadataReader.GetString
                        MetadataToken.ofEntityHandle
                        assy.Name
                        (metadataReader.GetMemberReference c)
                )

            builder.ToImmutable ()

        // TODO: render all this up front
        let strings (token : StringToken) =
            match token with
            | StringToken.String s -> metadataReader.GetString s
            | StringToken.UserString s -> metadataReader.GetUserString s

        let rootNamespace, nonRootNamespaces =
            metadataReader.GetNamespaceDefinitionRoot ()
            |> Namespace.make metadataReader.GetString metadataReader.GetNamespaceDefinition

        let fields =
            let result = ImmutableDictionary.CreateBuilder ()

            for field in metadataReader.FieldDefinitions do
                let fieldDefn =
                    metadataReader.GetFieldDefinition field
                    |> FieldInfo.make metadataReader assy.Name field

                result.Add (field, fieldDefn)

            result.ToImmutable ()

        let exportedTypes =
            let result = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.ExportedTypes do
                result.Add (ty, ExportedType.make metadataReader.GetString ty (metadataReader.GetExportedType ty))

            result.ToImmutable ()

        let attrs =
            let result = ImmutableDictionary.CreateBuilder ()

            for field in metadataReader.CustomAttributes do
                let fieldDefn =
                    metadataReader.GetCustomAttribute field |> CustomAttribute.make field

                result.Add (field, fieldDefn)

            result.ToImmutable ()

        let logger = loggerFactory.CreateLogger assy.Name.Name

        {
            Logger = logger
            OriginalPath = originalPath
            TypeDefs = typeDefs
            TypeRefs = typeRefs
            TypeSpecs = typeSpecs
            MainMethod = entryPointMethod
            Methods = methods
            MethodSpecs = methodSpecs
            Members = memberReferences
            Strings = strings
            Fields = fields
            AssemblyReferences = assemblyRefs
            ThisAssemblyDefinition = assy
            RootNamespace = rootNamespace
            NonRootNamespaces = nonRootNamespaces
            PeReader = peReader
            Attributes = attrs
            ExportedTypes = exportedTypes
            _TopLevelTypeDefsLookup = DumpedAssembly.BuildTopLevelTypeDefsLookup logger assy.Name typeDefs.Values
            _NestedTypeDefsLookup = DumpedAssembly.BuildNestedTypeDefsLookup logger assy.Name typeDefs.Values
            _TopLevelExportedTypesLookup =
                DumpedAssembly.BuildTopLevelExportedTypesLookup logger assy.Name exportedTypes.Values
            _NestedExportedTypesLookup =
                DumpedAssembly.BuildNestedExportedTypesLookup logger assy.Name exportedTypes.Values
        }

    let print (main : MethodDefinitionHandle) (dumped : DumpedAssembly) : unit =
        for KeyValue (_, typ) in dumped.TypeDefs do
            Console.WriteLine $"\nType: %s{typ.Namespace}.%s{typ.Name}"

            for method in typ.Methods do
                if method.Handle = main then
                    Console.WriteLine "Entry point!"

                Console.WriteLine $"\nMethod: %s{method.Name}"

                match method.Instructions with
                | None -> Console.WriteLine "<no IL instructions>"
                | Some instructions ->
                    instructions.Instructions
                    |> List.map (fun (op, index) -> IlOp.Format op index)
                    |> List.iter Console.WriteLine

    let private applyGenericArgs
        (genericArgs : ImmutableArray<TypeDefn>)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeInfo<TypeDefn, TypeDefn>
        =
        ty
        |> TypeInfo.mapGeneric (fun (param, _) ->
            if param.SequenceNumber < genericArgs.Length then
                genericArgs.[param.SequenceNumber]
            else
                TypeDefn.GenericTypeParameter param.SequenceNumber
        )

    let private resolveDefinedType
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeResolutionResult
        =
        TypeResolutionResult.Resolved (
            assy,
            ResolvedTypeIdentity.ofTypeDefinition assy.Name ty.TypeDefHandle,
            applyGenericArgs genericArgs ty
        )

    let resolveTypeIdentityDefinition
        (assy : DumpedAssembly)
        (identity : ResolvedTypeIdentity)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        if assy.Name.FullName <> identity.Assembly.FullName then
            failwithf
                "ResolvedTypeIdentity points at assembly %s but attempted lookup used assembly %s"
                identity.Assembly.FullName
                assy.Name.FullName

        match assy.TypeDefs.TryGetValue identity.TypeDefinition.Get with
        | true, defn -> defn
        | false, _ ->
            failwithf
                "ResolvedTypeIdentity points at missing type definition handle %A in assembly %s"
                identity.TypeDefinition.Get
                assy.Name.FullName

    let rec fullName (assy : DumpedAssembly) (identity : ResolvedTypeIdentity) : string =
        resolveTypeIdentityDefinition assy identity
        |> TypeInfo.fullName (fun h -> assy.TypeDefs.[h])

    let rec private resolveTopLevelTypeInAssembly
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (ns : string option)
        (name : string)
        : TypeResolutionResult
        =
        match ns with
        | None ->
            failwithf
                "Top-level type resolution requires an explicit namespace when resolving %s in %s"
                name
                assy.Name.FullName
        | Some ns ->
            match assy.TryGetTopLevelTypeDef ns name with
            | Some typeDef -> resolveDefinedType genericArgs assy typeDef
            | None ->
                match assy.TryGetTopLevelExportedType (Some ns) name with
                | Some export -> resolveTypeFromExport assy assemblies genericArgs export
                | None ->
                    failwith $"TODO: top-level type resolution unimplemented for {ns} {name} in {assy.Name.FullName}"

    // No exported-type fallback is needed here (unlike resolveTopLevelTypeInAssembly).
    // This function is only reached after the parent TypeRef has been fully resolved through
    // any forwarding chains, so `assy` is the assembly that *defines* the declaring type.
    // ECMA-335 requires nested types to reside in the same assembly as their declaring type,
    // so the child must be a local TypeDef here.
    // Nested *forwarded* types (ExportedTypeData.ParentExportedType) are handled by a
    // separate code path: resolveTypeFromExport -> resolveExportedTypeByChain.
    and private resolveNestedTypeInAssembly
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (declaringType : ResolvedTypeIdentity)
        (childName : string)
        : TypeResolutionResult
        =
        match assy.TryGetNestedTypeDef declaringType.TypeDefinition.Get childName with
        | Some typeDef -> resolveDefinedType genericArgs assy typeDef
        | None ->
            failwithf
                "Failed to resolve nested type %s inside %s in assembly %s"
                childName
                (fullName assy declaringType)
                assy.Name.FullName

    and private resolveTypeRefInAssembly
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
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
        : ResolvedTypeIdentity
        =
        match resolvedParent with
        | Some parent ->
            match targetAssembly.TryGetNestedTypeDef parent.TypeDefinition.Get exportedType.Name with
            | Some nested -> ResolvedTypeIdentity.ofTypeDefinition targetAssembly.Name nested.TypeDefHandle
            | None ->
                failwithf
                    "Failed to resolve nested exported type %s under %s in assembly %s"
                    exportedType.Name
                    (fullName targetAssembly parent)
                    targetAssembly.Name.FullName
        | None ->
            match exportedType.Namespace with
            | None ->
                failwithf
                    "Top-level exported type %s in assembly %s did not carry a namespace"
                    exportedType.Name
                    targetAssembly.Name.FullName
            | Some ns ->
                match targetAssembly.TryGetTopLevelTypeDef ns exportedType.Name with
                | Some topLevel -> ResolvedTypeIdentity.ofTypeDefinition targetAssembly.Name topLevel.TypeDefHandle
                | None ->
                    failwithf
                        "Failed to resolve top-level exported type %s.%s in assembly %s"
                        ns
                        exportedType.Name
                        targetAssembly.Name.FullName

    and resolveTypeFromExport
        (fromAssembly : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (genericArgs : ImmutableArray<TypeDefn>)
        (ty : WoofWare.PawPrint.ExportedType)
        : TypeResolutionResult
        =
        match ty.Data with
        | ExportedTypeData.ForwardsTo assyRef ->
            let assyRef = fromAssembly.AssemblyReferences.[assyRef]

            match assemblies.TryGetValue assyRef.Name.FullName with
            | false, _ -> TypeResolutionResult.FirstLoadAssy assyRef
            | true, toAssy -> resolveTopLevelTypeInAssembly assemblies genericArgs toAssy ty.Namespace ty.Name
        | ExportedTypeData.ParentExportedType parentExport ->
            let parent = fromAssembly.ExportedTypes.[parentExport]

            match resolveTypeFromExport fromAssembly assemblies genericArgs parent with
            | TypeResolutionResult.FirstLoadAssy assyRef -> TypeResolutionResult.FirstLoadAssy assyRef
            | TypeResolutionResult.Resolved (targetAssembly, parentIdentity, _) ->
                let identity = resolveExportedTypeByChain targetAssembly (Some parentIdentity) ty
                let typeDef = resolveTypeIdentityDefinition targetAssembly identity
                TypeResolutionResult.Resolved (targetAssembly, identity, applyGenericArgs genericArgs typeDef)
        | ExportedTypeData.AssemblyFile _ ->
            failwithf
                "AssemblyFile exported types are not yet supported while resolving %A from %s"
                ty.Handle
                fromAssembly.Name.FullName

    and resolveTypeRef
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
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
                    referencedInAssembly.Name.FullName
                    (referencedInAssembly.AssemblyReferences.Keys |> Seq.toList)
            | true, assemblyRef ->

            let assemblyName = assemblyRef.Name

            match assemblies.TryGetValue assemblyName.FullName with
            | false, _ -> TypeResolutionResult.FirstLoadAssy assemblyRef
            | true, assy ->
                resolveTopLevelTypeInAssembly assemblies genericArgs assy (Some target.Namespace) target.Name
        | TypeRefResolutionScope.TypeRef parent ->
            match resolveTypeRefInAssembly assemblies genericArgs referencedInAssembly parent with
            | TypeResolutionResult.FirstLoadAssy assyRef -> TypeResolutionResult.FirstLoadAssy assyRef
            | TypeResolutionResult.Resolved (targetAssembly, parentIdentity, _) ->
                resolveNestedTypeInAssembly assemblies genericArgs targetAssembly parentIdentity target.Name
        | TypeRefResolutionScope.ModuleRef moduleRef ->
            failwithf
                "ModuleRef type resolution is not yet supported for type %s.%s in assembly %s via module ref %A"
                target.Namespace
                target.Name
                referencedInAssembly.Name.FullName
                moduleRef

    and resolveTopLevelTypeFromName
        (assy : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        resolveTopLevelTypeInAssembly assemblies genericArgs assy ns name

    [<Obsolete("Use resolveTopLevelTypeFromName for top-level discovery only, or resolveTypeRef / resolveTypeFromExport for scope-aware resolution.")>]
    let resolveTypeFromName
        (assy : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        resolveTopLevelTypeFromName assy assemblies ns name genericArgs

[<RequireQualifiedAccess>]
module DumpedAssembly =
    let resolveBaseType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : ImmutableDictionary<string, DumpedAssembly>)
        (source : AssemblyName)
        (baseTypeInfo : BaseTypeInfo option)
        : ResolvedBaseType
        =
        let rec go (source : AssemblyName) (baseType : BaseTypeInfo option) =
            match baseType with
            | Some (BaseTypeInfo.TypeRef r) ->
                let assy = loadedAssemblies.[source.FullName]
                // TODO: generics
                match Assembly.resolveTypeRef loadedAssemblies assy ImmutableArray.Empty assy.TypeRefs.[r] with
                | TypeResolutionResult.FirstLoadAssy _ ->
                    failwith
                        "seems pretty unlikely that we could have constructed this object without loading its base type"
                | TypeResolutionResult.Resolved (assy, _, typeInfo) ->
                    match TypeInfo.isBaseType bct _.Name assy.Name typeInfo.TypeDefHandle with
                    | Some v -> v
                    | None -> go assy.Name typeInfo.BaseType
            | Some (BaseTypeInfo.ForeignAssemblyType (assy, ty)) ->
                let assy = loadedAssemblies.[assy.FullName]

                match TypeInfo.isBaseType bct _.Name assy.Name ty with
                | Some v -> v
                | None ->
                    let ty = assy.TypeDefs.[ty]
                    go assy.Name ty.BaseType
            | Some (BaseTypeInfo.TypeSpec _) -> failwith "TODO"
            | Some (BaseTypeInfo.TypeDef h) ->
                let assy = loadedAssemblies.[source.FullName]

                match TypeInfo.isBaseType bct _.Name assy.Name h with
                | Some v -> v
                | None ->
                    let ty = assy.TypeDefs.[h]
                    go assy.Name ty.BaseType
            | None -> ResolvedBaseType.Object

        go source baseTypeInfo

    let typeInfoToTypeDefn
        (bct : BaseClassTypes<DumpedAssembly>)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ti : TypeInfo<TypeDefn, TypeDefn>)
        : TypeDefn
        =
        ti
        |> TypeInfo.toTypeDefn
            bct
            (fun n -> assemblies.[n.FullName])
            _.Name
            (fun x y -> x.TypeDefs.[y])
            (fun x y ->
                let r = x.TypeRefs.[y] |> Assembly.resolveTypeRef assemblies x ImmutableArray.Empty

                match r with
                | TypeResolutionResult.FirstLoadAssy assemblyReference -> failwith "todo"
                | TypeResolutionResult.Resolved (dumpedAssembly, _, typeInfo) ->
                    let result =
                        typeInfo |> TypeInfo.mapGeneric (fun typeDef -> failwith "TODO: generics")

                    dumpedAssembly, result
            )

    let typeInfoToTypeDefn'
        (bct : BaseClassTypes<DumpedAssembly>)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        =
        ti
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)
        |> typeInfoToTypeDefn bct assemblies

[<RequireQualifiedAccess>]
module AssemblyApi =
    let read = Assembly.read
    let resolveTypeRef = Assembly.resolveTypeRef
    let resolveTopLevelTypeFromName = Assembly.resolveTopLevelTypeFromName

    [<Obsolete("Use resolveTopLevelTypeFromName for top-level discovery only, or resolveTypeRef / resolveTypeFromExport for scope-aware resolution.")>]
    let resolveTypeFromName
        (assy : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        Assembly.resolveTopLevelTypeFromName assy assemblies ns name genericArgs

    let resolveTypeFromExport = Assembly.resolveTypeFromExport
    let resolveTypeIdentityDefinition = Assembly.resolveTypeIdentityDefinition
    let fullName = Assembly.fullName
