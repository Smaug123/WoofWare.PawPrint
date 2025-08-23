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
        /// Internal lookup for exported types by namespace and name.
        /// </summary>
        _ExportedTypesLookup : ImmutableDictionary<string option * string, WoofWare.PawPrint.ExportedType>

        /// <summary>
        /// Internal lookup for type references by namespace and name.
        /// </summary>
        _TypeRefsLookup : ImmutableDictionary<string * string, WoofWare.PawPrint.TypeRef>

        /// <summary>
        /// Internal lookup for type definitions by namespace and name.
        /// </summary>
        _TypeDefsLookup :
            ImmutableDictionary<string * string, WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>>
    }

    static member internal BuildExportedTypesLookup
        (logger : ILogger)
        (name : AssemblyName)
        (types : WoofWare.PawPrint.ExportedType seq)
        : ImmutableDictionary<string option * string, WoofWare.PawPrint.ExportedType>
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in types do
            let key = ty.Namespace, ty.Name

            if keys.Add key then
                result.Add (key, ty)
            else
                logger.LogDebug (
                    "Duplicate types exported from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                    name,
                    ty.Namespace,
                    ty.Name
                )

                result.Remove key |> ignore<bool>

        result.ToImmutable ()

    static member internal BuildTypeRefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeRefs : WoofWare.PawPrint.TypeRef seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeRefs do
            let key = (ty.Namespace, ty.Name)

            if keys.Add key then
                result.Add (key, ty)
            else
                // TODO: this is all very dubious, the ResolutionScope is supposed to tell us how to disambiguate these
                logger.LogDebug (
                    "Duplicate type refs from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                    name,
                    ty.Namespace,
                    ty.Name
                )

        result.ToImmutable ()

    static member internal BuildTypeDefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeDefs : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeDefs do
            let key = (ty.Namespace, ty.Name)

            if keys.Add key then
                result.Add (key, ty)
            else
                // TODO: this is all very dubious, the ResolutionScope is supposed to tell us how to disambiguate these
                logger.LogDebug (
                    "Duplicate type defs from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                    name,
                    ty.Namespace,
                    ty.Name
                )

        result.ToImmutable ()

    member this.Name = this.ThisAssemblyDefinition.Name

    member this.TypeRef (``namespace`` : string) (name : string) : WoofWare.PawPrint.TypeRef option =
        match this._TypeRefsLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.TypeDef
        (``namespace`` : string)
        (name : string)
        : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        match this._TypeDefsLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.ExportedType (``namespace`` : string option) (name : string) : WoofWare.PawPrint.ExportedType option =
        match this._ExportedTypesLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    interface IDisposable with
        member this.Dispose () = this.PeReader.Dispose ()


type TypeResolutionResult =
    | FirstLoadAssy of WoofWare.PawPrint.AssemblyReference
    | Resolved of DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>

    override this.ToString () : string =
        match this with
        | TypeResolutionResult.FirstLoadAssy a -> $"FirstLoadAssy(%s{a.Name.FullName})"
        | TypeResolutionResult.Resolved (assy, ty) ->
            $"Resolved(%s{assy.Name.FullName}: {string<TypeInfo<TypeDefn, TypeDefn>> ty})"

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
            _ExportedTypesLookup = DumpedAssembly.BuildExportedTypesLookup logger assy.Name exportedTypes.Values
            _TypeRefsLookup = DumpedAssembly.BuildTypeRefsLookup logger assy.Name typeRefs.Values
            _TypeDefsLookup = DumpedAssembly.BuildTypeDefsLookup logger assy.Name typeDefs.Values
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

    let rec resolveTypeRef
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (genericArgs : ImmutableArray<TypeDefn>)
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

                let nsPath = target.Namespace.Split '.' |> Array.toList

                let targetNs = assy.NonRootNamespaces.[nsPath]

                let targetType =
                    targetNs.TypeDefinitions
                    |> Seq.choose (fun td ->
                        let ty = assy.TypeDefs.[td]

                        if ty.Name = target.Name && ty.Namespace = target.Namespace then
                            Some ty
                        else
                            None
                    )
                    |> Seq.toList

                match targetType with
                | [ t ] ->
                    let t =
                        t
                        |> TypeInfo.mapGeneric (fun _ (param, md) -> genericArgs.[param.SequenceNumber])

                    TypeResolutionResult.Resolved (assy, t)
                | _ :: _ :: _ -> failwith $"Multiple matching type definitions! {nsPath} {target.Name}"
                | [] ->
                    match assy.ExportedType (Some target.Namespace) target.Name with
                    | None -> failwith $"Failed to find type {nsPath} {target.Name} in {assy.Name.FullName}!"
                    | Some ty -> resolveTypeFromExport assy assemblies ty genericArgs
        | k -> failwith $"Unexpected: {k}"

    and resolveTypeFromName
        (assy : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        match ns with
        | None -> failwith "what are the semantics here"
        | Some ns ->

        match assy.TypeDef ns name with
        | Some typeDef ->
            let typeDef =
                typeDef
                |> TypeInfo.mapGeneric (fun _ (param, md) -> genericArgs.[param.SequenceNumber])

            TypeResolutionResult.Resolved (assy, typeDef)
        | None ->

        match assy.TypeRef ns name with
        | Some typeRef -> resolveTypeRef assemblies assy typeRef genericArgs
        | None ->

        match assy.ExportedType (Some ns) name with
        | Some export -> resolveTypeFromExport assy assemblies export genericArgs
        | None -> failwith $"TODO: type resolution unimplemented for {ns} {name}"

    and resolveTypeFromExport
        (fromAssembly : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        : TypeResolutionResult
        =
        match ty.Data with
        | NonForwarded _ -> failwith "Somehow didn't find type definition but it is exported"
        | ForwardsTo assy ->
            let assy = fromAssembly.AssemblyReferences.[assy]

            match assemblies.TryGetValue assy.Name.FullName with
            | false, _ -> TypeResolutionResult.FirstLoadAssy assy
            | true, toAssy -> resolveTypeFromName toAssy assemblies ty.Namespace ty.Name genericArgs

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
                match Assembly.resolveTypeRef loadedAssemblies assy assy.TypeRefs.[r] ImmutableArray.Empty with
                | TypeResolutionResult.FirstLoadAssy _ ->
                    failwith
                        "seems pretty unlikely that we could have constructed this object without loading its base type"
                | TypeResolutionResult.Resolved (assy, typeInfo) ->
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
