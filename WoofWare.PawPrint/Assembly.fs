namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.FSharp.Core

type AssemblyDefinition =
    {
        Name : AssemblyName
    }

type Namespace =
    {
        Name : StringToken
        PrettyName : string
        Parent : NamespaceDefinitionHandle
        TypeDefinitions : ImmutableArray<TypeDefinitionHandle>
        ExportedTypes : ImmutableArray<ExportedTypeHandle>
    }

[<RequireQualifiedAccess>]
module Namespace =
    /// Returns also the children.
    let make
        (getString : StringHandle -> string)
        (getNamespace : NamespaceDefinitionHandle -> NamespaceDefinition)
        (ns : NamespaceDefinition)
        : Namespace * ImmutableDictionary<string list, Namespace>
        =
        let children = ImmutableDictionary.CreateBuilder ()

        let rec inner (path : string list) (ns : NamespaceDefinition) : Namespace =
            for child in ns.NamespaceDefinitions do
                let rendered = getNamespace child
                let location = getString rendered.Name :: path
                children.Add (List.rev location, inner location rendered)

            {
                Name = StringToken.String ns.Name
                PrettyName = getString ns.Name
                Parent = ns.Parent
                TypeDefinitions = ns.TypeDefinitions
                ExportedTypes = ns.ExportedTypes
            }

        let result = inner [] ns
        result, children.ToImmutable ()

[<RequireQualifiedAccess>]
module AssemblyDefinition =
    let make (assy : System.Reflection.Metadata.AssemblyDefinition) : AssemblyDefinition =
        {
            Name = assy.GetAssemblyName ()
        }

type DumpedAssembly =
    {
        TypeDefs : IReadOnlyDictionary<TypeDefinitionHandle, WoofWare.PawPrint.TypeInfo>
        TypeRefs : IReadOnlyDictionary<TypeReferenceHandle, WoofWare.PawPrint.TypeRef>
        Methods : IReadOnlyDictionary<MethodDefinitionHandle, WoofWare.PawPrint.MethodInfo>
        Members : IReadOnlyDictionary<MemberReferenceHandle, WoofWare.PawPrint.MemberReference<MetadataToken>>
        Fields : IReadOnlyDictionary<FieldDefinitionHandle, WoofWare.PawPrint.FieldInfo>
        MainMethod : MethodDefinitionHandle option
        /// Map of four-byte int token to metadata
        MethodDefinitions : Map<int, MethodDefinition>
        MethodSpecs : ImmutableDictionary<MethodSpecificationHandle, MethodSpec>
        Strings : StringToken -> string
        AssemblyReferences : ImmutableDictionary<AssemblyReferenceHandle, WoofWare.PawPrint.AssemblyReference>
        ThisAssemblyDefinition : AssemblyDefinition
        RootNamespace : Namespace
        NonRootNamespaces : ImmutableDictionary<string list, Namespace>
        // TODO: work out how to render all the strings up front, then drop this
        PeReader : PEReader
    }

    interface IDisposable with
        member this.Dispose () = this.PeReader.Dispose ()

[<RequireQualifiedAccess>]
module Assembly =
    let read (dllBytes : Stream) : DumpedAssembly =
        let peReader = new PEReader (dllBytes)
        let metadataReader = peReader.GetMetadataReader ()

        let entryPoint =
            peReader.PEHeaders.CorHeader.EntryPointTokenOrRelativeVirtualAddress
            |> fun x -> if x = 0 then None else Some x

        let entryPointMethod =
            entryPoint |> Option.map MetadataTokens.MethodDefinitionHandle

        let typeRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeReferences do
                let typeRef = metadataReader.GetTypeReference ty

                let result =
                    {
                        Name = StringToken.String typeRef.Name
                        PrettyName = metadataReader.GetString typeRef.Name
                        Namespace = StringToken.String typeRef.Namespace
                        PrettyNamespace = metadataReader.GetString typeRef.Namespace
                        ResolutionScope = MetadataToken.ofEntityHandle typeRef.ResolutionScope
                    }

                builder.Add (ty, result)

            builder.ToImmutable ()

        let typeDefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ty in metadataReader.TypeDefinitions do
                builder.Add (ty, TypeInfo.read peReader metadataReader ty)

            builder.ToImmutable ()

        // TODO: this probably misses any methods out which aren't associated with a type definition?
        let methods =
            typeDefs
            |> Seq.collect (fun (KeyValue (_, ty)) -> ty.Methods |> List.map (fun mi -> KeyValuePair (mi.Handle, mi)))
            |> ImmutableDictionary.CreateRange

        let methodDefnMetadata =
            metadataReader.MethodDefinitions
            |> Seq.map (fun mh ->
                let def = metadataReader.GetMethodDefinition mh
                let eh : EntityHandle = MethodDefinitionHandle.op_Implicit mh
                let token = MetadataTokens.GetToken eh
                token, def
            )
            |> Map.ofSeq

        let methodSpecs =
            Seq.init
                (metadataReader.GetTableRowCount TableIndex.MethodSpec)
                (fun i ->
                    let i = i + 1
                    let handle = MetadataTokens.MethodSpecificationHandle i
                    KeyValuePair (handle, MethodSpec.make (metadataReader.GetMethodSpecification handle))
                )
            |> ImmutableDictionary.CreateRange

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

        let assemblyRefs =
            let builder = ImmutableDictionary.CreateBuilder ()

            for ref in metadataReader.AssemblyReferences do
                builder.Add (ref, AssemblyReference.make (metadataReader.GetAssemblyReference ref))

            builder.ToImmutable ()

        let assy = metadataReader.GetAssemblyDefinition () |> AssemblyDefinition.make

        let rootNamespace, nonRootNamespaces =
            metadataReader.GetNamespaceDefinitionRoot ()
            |> Namespace.make metadataReader.GetString metadataReader.GetNamespaceDefinition

        let fields =
            let result = ImmutableDictionary.CreateBuilder ()

            for field in metadataReader.FieldDefinitions do
                let fieldDefn =
                    metadataReader.GetFieldDefinition field |> FieldInfo.make strings field

                result.Add (field, fieldDefn)

            result.ToImmutable ()

        {
            TypeDefs = typeDefs
            TypeRefs = typeRefs
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
