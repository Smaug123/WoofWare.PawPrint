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
            IReadOnlyDictionary<
                TypeDefinitionHandle,
                WoofWare.PawPrint.TypeInfo<WoofWare.PawPrint.GenericParameter, WoofWare.PawPrint.TypeDefn>
             >

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
                WoofWare.PawPrint.MethodInfo<FakeUnit, WoofWare.PawPrint.GenericParameter, TypeDefn>
             >

        /// <summary>
        /// Dictionary of all member references in this assembly, keyed by their handle.
        /// </summary>
        Members : IReadOnlyDictionary<MemberReferenceHandle, WoofWare.PawPrint.MemberReference<MetadataToken>>

        /// <summary>
        /// Dictionary of all field definitions in this assembly, keyed by their handle.
        /// </summary>
        Fields :
            IReadOnlyDictionary<FieldDefinitionHandle, WoofWare.PawPrint.FieldInfo<FakeUnit, WoofWare.PawPrint.TypeDefn>>

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
            ImmutableDictionary<
                string * string,
                WoofWare.PawPrint.TypeInfo<WoofWare.PawPrint.GenericParameter, WoofWare.PawPrint.TypeDefn>
             >
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
        (typeDefs : WoofWare.PawPrint.TypeInfo<WoofWare.PawPrint.GenericParameter, WoofWare.PawPrint.TypeDefn> seq)
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
        : WoofWare.PawPrint.TypeInfo<WoofWare.PawPrint.GenericParameter, WoofWare.PawPrint.TypeDefn> option
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


type TypeResolutionResult<'generic> =
    | FirstLoadAssy of WoofWare.PawPrint.AssemblyReference
    | Resolved of DumpedAssembly * TypeInfo<'generic, WoofWare.PawPrint.TypeDefn>

    override this.ToString () : string =
        match this with
        | TypeResolutionResult.FirstLoadAssy a -> $"FirstLoadAssy(%s{a.Name.FullName})"
        | TypeResolutionResult.Resolved (assy, ty) -> $"Resolved(%s{assy.Name.FullName}: {ty})"

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
        (genericArgs : ImmutableArray<'generic>)
        : TypeResolutionResult<'generic>
        =
        match target.ResolutionScope with
        | TypeRefResolutionScope.Assembly r ->
            let assemblyRef = referencedInAssembly.AssemblyReferences.[r]
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
                let t = t |> TypeInfo.mapGeneric (fun _ param -> genericArgs.[param.SequenceNumber])

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
        (genericArgs : ImmutableArray<'generic>)
        : TypeResolutionResult<'generic>
        =
        match ns with
        | None -> failwith "what are the semantics here"
        | Some ns ->

        match assy.TypeDef ns name with
        | Some typeDef ->
            let typeDef =
                typeDef
                |> TypeInfo.mapGeneric (fun _ param -> genericArgs.[param.SequenceNumber])

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
        (genericArgs : ImmutableArray<'generic>)
        : TypeResolutionResult<'generic>
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
                match Assembly.resolveTypeRef loadedAssemblies assy assy.TypeRefs.[r] ImmutableArray<unit>.Empty with
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

    // TODO: this is in totally the wrong place, it was just convenient to put it here
    /// Returns a mapping whose keys are assembly full name * type definition.
    let concretiseType
        (mapping : AllConcreteTypes)
        (baseTypes : BaseClassTypes<'corelib>)
        (getCorelibAssembly : 'corelib -> AssemblyName)
        (getTypeInfo :
            ComparableTypeDefinitionHandle
                -> AssemblyName
                -> TypeInfo<WoofWare.PawPrint.GenericParameter, WoofWare.PawPrint.TypeDefn>)
        (resolveTypeRef : TypeRef -> TypeResolutionResult<'a>)
        (typeGenerics : (AssemblyName * TypeDefn) ImmutableArray)
        (methodGenerics : (AssemblyName * TypeDefn) ImmutableArray)
        (defn : AssemblyName * TypeDefn)
        : Map<string * TypeDefn, ConcreteTypeHandle> * AllConcreteTypes
        =
        // Track types currently being processed to detect cycles
        let rec concretiseTypeRec
            (inProgress : Map<string * TypeDefn, ConcreteTypeHandle>)
            (newlyCreated : Map<string * TypeDefn, ConcreteTypeHandle>)
            (mapping : AllConcreteTypes)
            (assy : AssemblyName)
            (typeDefn : TypeDefn)
            : ConcreteTypeHandle * Map<string * TypeDefn, ConcreteTypeHandle> * AllConcreteTypes
            =

            // First check if we're already processing this type (cycle)
            match inProgress |> Map.tryFind (assy.FullName, typeDefn) with
            | Some handle -> handle, newlyCreated, mapping
            | None ->

            // Check if already concretised in this session
            match newlyCreated |> Map.tryFind (assy.FullName, typeDefn) with
            | Some handle -> handle, newlyCreated, mapping
            | None ->

            match typeDefn with
            | PrimitiveType primitiveType ->
                let typeInfo =
                    match primitiveType with
                    | PrimitiveType.Boolean -> baseTypes.Boolean
                    | PrimitiveType.Char -> baseTypes.Char
                    | PrimitiveType.SByte -> baseTypes.SByte
                    | PrimitiveType.Byte -> baseTypes.Byte
                    | PrimitiveType.Int16 -> baseTypes.Int16
                    | PrimitiveType.UInt16 -> baseTypes.UInt16
                    | PrimitiveType.Int32 -> baseTypes.Int32
                    | PrimitiveType.UInt32 -> baseTypes.UInt32
                    | PrimitiveType.Int64 -> baseTypes.Int64
                    | PrimitiveType.UInt64 -> baseTypes.UInt64
                    | PrimitiveType.Single -> baseTypes.Single
                    | PrimitiveType.Double -> baseTypes.Double
                    | PrimitiveType.String -> baseTypes.String
                    | PrimitiveType.TypedReference -> failwith "TypedReference not supported in BaseClassTypes"
                    | PrimitiveType.IntPtr -> failwith "IntPtr not supported in BaseClassTypes"
                    | PrimitiveType.UIntPtr -> failwith "UIntPtr not supported in BaseClassTypes"
                    | PrimitiveType.Object -> baseTypes.Object

                let cth, concreteType, mapping =
                    ConcreteType.make
                        mapping
                        typeInfo.Assembly
                        typeInfo.Namespace
                        typeInfo.Name
                        typeInfo.TypeDefHandle
                        []

                let handle, mapping = mapping |> AllConcreteTypes.add concreteType

                handle,
                newlyCreated
                |> Map.add ((getCorelibAssembly baseTypes.Corelib).FullName, typeDefn) handle,
                mapping

            | Void -> failwith "Void is not a real type and cannot be concretised"

            | Array (elt, shape) ->
                let eltHandle, newlyCreated, mapping =
                    concretiseTypeRec inProgress newlyCreated mapping elt

                let arrayTypeInfo = baseTypes.Array

                let cth, concreteType, mapping =
                    ConcreteType.make
                        mapping
                        arrayTypeInfo.Assembly
                        arrayTypeInfo.Namespace
                        arrayTypeInfo.Name
                        arrayTypeInfo.TypeDefHandle
                        [ eltHandle ]

                let handle, mapping = mapping |> AllConcreteTypes.add concreteType
                handle, newlyCreated |> Map.add typeDefn handle, mapping

            | OneDimensionalArrayLowerBoundZero elements ->
                let eltHandle, newlyCreated, mapping =
                    concretiseTypeRec inProgress newlyCreated mapping elements

                let arrayTypeInfo = baseTypes.Array

                let cth, concreteType, mapping =
                    ConcreteType.make
                        mapping
                        arrayTypeInfo.Assembly
                        arrayTypeInfo.Namespace
                        arrayTypeInfo.Name
                        arrayTypeInfo.TypeDefHandle
                        [ eltHandle ]

                let handle, mapping = mapping |> AllConcreteTypes.add concreteType

                handle,
                newlyCreated
                |> Map.add ((getCorelibAssembly baseTypes.Corelib).FullName, typeDefn) handle,
                mapping

            | Pointer inner -> failwith "Pointer types require special handling - no TypeDefinition available"

            | Byref inner -> failwith "Byref types require special handling - no TypeDefinition available"

            | Pinned inner -> failwith "Pinned types require special handling - no TypeDefinition available"

            | GenericTypeParameter index ->
                if index < typeGenerics.Length then
                    concretiseTypeRec inProgress newlyCreated mapping typeGenerics.[index]
                else
                    failwithf "Generic type parameter index %d out of range" index

            | GenericMethodParameter index ->
                if index < methodGenerics.Length then
                    concretiseTypeRec inProgress newlyCreated mapping methodGenerics.[index]
                else
                    failwithf "Generic method parameter index %d out of range" index

            | FromDefinition (typeDefHandle, assemblyFullName, _) ->
                let assemblyName = AssemblyName assemblyFullName
                let typeInfo = getTypeInfo typeDefHandle assemblyName

                let cth, concreteType, mapping =
                    ConcreteType.make mapping assemblyName typeInfo.Namespace typeInfo.Name typeDefHandle.Get []

                let handle, mapping = mapping |> AllConcreteTypes.add concreteType
                handle, newlyCreated |> Map.add (assemblyFullName, typeDefn) handle, mapping

            | FromReference (typeRef, sigKind) ->
                match resolveTypeRef typeRef with
                | TypeResolutionResult.FirstLoadAssy assy -> failwith "TODO"
                | TypeResolutionResult.Resolved (resolvedAssy, typeInfo) ->
                    let cth, concreteType, mapping =
                        ConcreteType.make
                            mapping
                            resolvedAssy.Name
                            typeInfo.Namespace
                            typeInfo.Name
                            typeInfo.TypeDefHandle
                            []

                    let handle, mapping = mapping |> AllConcreteTypes.add concreteType
                    handle, newlyCreated |> Map.add typeDefn handle, mapping

            | GenericInstantiation (genericDef, args) ->
                // This is the tricky case - we might have self-reference
                // First, allocate a handle for this type
                let tempHandle = ConcreteTypeHandle mapping.NextHandle

                let mapping =
                    { mapping with
                        NextHandle = mapping.NextHandle + 1
                    }

                let inProgress = inProgress |> Map.add typeDefn tempHandle

                // Concretise all type arguments first
                let rec concretiseArgs
                    (acc : ConcreteTypeHandle list)
                    (newlyCreated : Map<TypeDefn, ConcreteTypeHandle>)
                    (mapping : AllConcreteTypes)
                    (args : TypeDefn list)
                    : ConcreteTypeHandle list * Map<TypeDefn, ConcreteTypeHandle> * AllConcreteTypes
                    =
                    match args with
                    | [] -> List.rev acc, newlyCreated, mapping
                    | arg :: rest ->
                        let argHandle, newlyCreated, mapping =
                            concretiseTypeRec inProgress newlyCreated mapping arg

                        concretiseArgs (argHandle :: acc) newlyCreated mapping rest

                let argHandles, newlyCreated, mapping =
                    concretiseArgs [] newlyCreated mapping (args |> Seq.toList)

                // Now extract the definition from the generic def
                match genericDef with
                | FromDefinition (typeDefHandle, assemblyFullName, _) ->
                    let assemblyName = AssemblyName (assemblyFullName)
                    let typeInfo = getTypeInfo typeDefHandle assemblyName

                    let cth, concreteType, mapping =
                        ConcreteType.make
                            mapping
                            assemblyName
                            typeInfo.Namespace
                            typeInfo.Name
                            typeDefHandle.Get
                            argHandles
                    // Update the pre-allocated entry
                    let mapping =
                        { mapping with
                            Mapping = mapping.Mapping |> Map.add tempHandle concreteType
                        }

                    tempHandle, newlyCreated |> Map.add typeDefn tempHandle, mapping

                | FromReference (typeRef, _) ->
                    match resolveTypeRef typeRef with
                    | TypeResolutionResult.FirstLoadAssy _ -> failwith "TODO"
                    | TypeResolutionResult.Resolved (resolvedAssy, typeInfo) ->

                    let cth, concreteType, mapping =
                        ConcreteType.make
                            mapping
                            resolvedAssy.Name
                            typeInfo.Namespace
                            typeInfo.Name
                            typeInfo.TypeDefHandle
                            argHandles

                    let mapping =
                        { mapping with
                            Mapping = mapping.Mapping |> Map.add tempHandle concreteType
                        }

                    tempHandle, newlyCreated |> Map.add typeDefn tempHandle, mapping

                | _ -> failwithf "Generic instantiation of non-definition type: %A" genericDef

            | Modified (original, afterMod, required) ->
                failwith "Modified types require special handling - not yet implemented"

            | FunctionPointer _ -> failwith "Function pointer concretisation not implemented"

        let _, newlyCreated, finalMapping =
            concretiseTypeRec Map.empty Map.empty mapping (fst defn) (snd defn)

        newlyCreated, finalMapping
