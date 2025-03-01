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

type AssemblyDefinition =
    {
        Name : AssemblyName
    }

[<RequireQualifiedAccess>]
module AssemblyDefinition =
    let make (assy : System.Reflection.Metadata.AssemblyDefinition) : AssemblyDefinition =
        {
            Name = assy.GetAssemblyName ()
        }

type DumpedAssembly =
    {
        Logger : ILogger
        TypeDefs : IReadOnlyDictionary<TypeDefinitionHandle, WoofWare.PawPrint.TypeInfo>
        TypeRefs : IReadOnlyDictionary<TypeReferenceHandle, WoofWare.PawPrint.TypeRef>
        TypeSpecs : IReadOnlyDictionary<TypeSpecificationHandle, WoofWare.PawPrint.TypeSpec>
        Methods : IReadOnlyDictionary<MethodDefinitionHandle, WoofWare.PawPrint.MethodInfo>
        Members : IReadOnlyDictionary<MemberReferenceHandle, WoofWare.PawPrint.MemberReference<MetadataToken>>
        Fields : IReadOnlyDictionary<FieldDefinitionHandle, WoofWare.PawPrint.FieldInfo>
        MainMethod : MethodDefinitionHandle option
        /// Map of four-byte int token to metadata
        MethodDefinitions : ImmutableDictionary<int, MethodDefinition>
        MethodSpecs : ImmutableDictionary<MethodSpecificationHandle, MethodSpec>
        Strings : StringToken -> string
        AssemblyReferences : ImmutableDictionary<AssemblyReferenceHandle, WoofWare.PawPrint.AssemblyReference>
        ThisAssemblyDefinition : AssemblyDefinition
        RootNamespace : Namespace
        NonRootNamespaces : ImmutableDictionary<string list, Namespace>
        // TODO: work out how to render all the strings up front, then drop this
        PeReader : PEReader
        Attributes : ImmutableDictionary<CustomAttributeHandle, WoofWare.PawPrint.CustomAttribute>
        ExportedTypes : ImmutableDictionary<ExportedTypeHandle, WoofWare.PawPrint.ExportedType>
        _ExportedTypesLookup : ImmutableDictionary<string option * string, WoofWare.PawPrint.ExportedType>
        _TypeRefsLookup : ImmutableDictionary<string * string, WoofWare.PawPrint.TypeRef>
        _TypeDefsLookup : ImmutableDictionary<string * string, WoofWare.PawPrint.TypeInfo>
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
                logger.LogWarning (
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
                logger.LogWarning (
                    "Duplicate type refs from assembly {ThisAssemblyName}: namespace {DuplicatedTypeNamespace}, type {DuplicatedTypeName}. Ignoring the duplicate.",
                    name,
                    ty.Namespace,
                    ty.Name
                )

        result.ToImmutable ()

    static member internal BuildTypeDefsLookup
        (logger : ILogger)
        (name : AssemblyName)
        (typeDefs : WoofWare.PawPrint.TypeInfo seq)
        =
        let result = ImmutableDictionary.CreateBuilder ()
        let keys = HashSet ()

        for ty in typeDefs do
            let key = (ty.Namespace, ty.Name)

            if keys.Add key then
                result.Add (key, ty)
            else
                // TODO: this is all very dubious, the ResolutionScope is supposed to tell us how to disambiguate these
                logger.LogWarning (
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

    member this.TypeDef (``namespace`` : string) (name : string) : WoofWare.PawPrint.TypeInfo option =
        match this._TypeDefsLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    member this.ExportedType (``namespace`` : string option) (name : string) : WoofWare.PawPrint.ExportedType option =
        match this._ExportedTypesLookup.TryGetValue ((``namespace``, name)) with
        | false, _ -> None
        | true, v -> Some v

    interface IDisposable with
        member this.Dispose () = this.PeReader.Dispose ()

[<RequireQualifiedAccess>]
module Assembly =
    let read (loggerFactory : ILoggerFactory) (dllBytes : Stream) : DumpedAssembly =
        let peReader = new PEReader (dllBytes)
        let metadataReader = peReader.GetMetadataReader ()

        let entryPoint =
            peReader.PEHeaders.CorHeader.EntryPointTokenOrRelativeVirtualAddress
            |> fun x -> if x = 0 then None else Some x

        let entryPointMethod =
            entryPoint |> Option.map MetadataTokens.MethodDefinitionHandle

        let assemblyRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ref in metadataReader.AssemblyReferences do
                builder.Add (ref, AssemblyReference.make (metadataReader.GetAssemblyReference ref))

            builder.ToImmutable ()

        let typeRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeReferences do
                builder.Add (ty, TypeRef.make metadataReader ty)

            builder.ToImmutable ()

        let typeDefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeDefinitions do
                builder.Add (ty, TypeInfo.read loggerFactory peReader metadataReader ty)

            builder.ToImmutable ()

        // TODO: this probably misses any methods out which aren't associated with a type definition?
        let methods =
            typeDefs
            |> Seq.collect (fun (KeyValue (_, ty)) -> ty.Methods |> List.map (fun mi -> KeyValuePair (mi.Handle, mi)))
            |> ImmutableDictionary.CreateRange

        let methodDefnMetadata =
            let result = ImmutableDictionary.CreateBuilder ()

            for mh in metadataReader.MethodDefinitions do
                let def = metadataReader.GetMethodDefinition mh
                let eh : EntityHandle = MethodDefinitionHandle.op_Implicit mh
                let token = MetadataTokens.GetToken eh
                result.Add (token, def)

            result.ToImmutable ()

        let methodSpecs =
            Seq.init
                (metadataReader.GetTableRowCount TableIndex.MethodSpec)
                (fun i ->
                    let i = i + 1
                    let handle = MetadataTokens.MethodSpecificationHandle i
                    KeyValuePair (handle, MethodSpec.make (metadataReader.GetMethodSpecification handle))
                )
            |> ImmutableDictionary.CreateRange

        let typeSpecs =
            let result = ImmutableDictionary.CreateBuilder ()

            for i = 1 to metadataReader.GetTableRowCount TableIndex.TypeSpec do
                let handle = MetadataTokens.TypeSpecificationHandle i
                result.Add (handle, metadataReader.GetTypeSpecification handle |> TypeSpec.make handle)

            result.ToImmutable ()

        let memberReferences =
            let builder = ImmutableDictionary.CreateBuilder ()

            for c in metadataReader.MemberReferences do
                builder.Add (
                    c,
                    MemberReference.make<MetadataToken>
                        metadataReader.GetString
                        MetadataToken.ofEntityHandle
                        (metadataReader.GetMemberReference c)
                )

            builder.ToImmutable ()

        // TODO: render all this up front
        let strings (token : StringToken) =
            match token with
            | StringToken.String s -> metadataReader.GetString s
            | StringToken.UserString s -> metadataReader.GetUserString s

        let assy = metadataReader.GetAssemblyDefinition () |> AssemblyDefinition.make

        let rootNamespace, nonRootNamespaces =
            metadataReader.GetNamespaceDefinitionRoot ()
            |> Namespace.make metadataReader.GetString metadataReader.GetNamespaceDefinition

        let fields =
            let result = ImmutableDictionary.CreateBuilder ()

            for field in metadataReader.FieldDefinitions do
                let fieldDefn =
                    metadataReader.GetFieldDefinition field
                    |> FieldInfo.make metadataReader.GetString field

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
            TypeDefs = typeDefs
            TypeRefs = typeRefs
            TypeSpecs = typeSpecs
            MainMethod = entryPointMethod
            Methods = methods
            MethodDefinitions = methodDefnMetadata
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
            printfn "\nType: %s.%s" typ.Namespace typ.Name

            for method in typ.Methods do
                if method.Handle = main then
                    printfn "Entry point!"

                printfn "\nMethod: %s" method.Name

                method.Instructions
                |> List.map (fun (op, index) -> IlOp.Format op index)
                |> List.iter Console.WriteLine
