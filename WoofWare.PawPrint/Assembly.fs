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
        TypeDefs : IReadOnlyDictionary<TypeDefinitionHandle, WoofWare.PawPrint.TypeInfo>
        TypeRefs : IReadOnlyDictionary<TypeReferenceHandle, WoofWare.PawPrint.TypeRef>
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
    }

    member this.Name = this.ThisAssemblyDefinition.Name

    member private this.ExportedTypesLookup =
        lazy
            let result = ImmutableDictionary.CreateBuilder ()

            for KeyValue (_, ty) in this.ExportedTypes do
                try
                    result.Add ((ty.Namespace, ty.Name), ty)
                with _ ->
                    let clash = result.[ty.Namespace, ty.Name]

                    let newOneForwardsTo =
                        match ty.Data with
                        | ForwardsTo assemblyReferenceHandle -> failwith "todo"
                        | NonForwarded eth -> eth

                    let existingOneForwardsTo =
                        match clash.Data with
                        | ForwardsTo assemblyReferenceHandle -> failwith "todo"
                        | NonForwarded eth -> eth

                    let resolvedNew =
                        result
                        |> Seq.tryPick (fun (KeyValue (_, v)) -> if v.Handle = newOneForwardsTo then Some v else None)
                        |> Option.get

                    let resolvedOld =
                        result
                        |> Seq.tryPick (fun (KeyValue (_, v)) ->
                            if v.Handle = existingOneForwardsTo then Some v else None
                        )
                        |> Option.get

                    reraise ()

            result.ToImmutable ()

    member this.ExportedType (``namespace`` : string option) (name : string) : WoofWare.PawPrint.ExportedType option =
        let types = this.ExportedTypesLookup.Force ()

        match types.TryGetValue ((``namespace``, name)) with
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
                let typeRef = metadataReader.GetTypeReference ty
                let prettyName = metadataReader.GetString typeRef.Name
                let prettyNamespace = metadataReader.GetString typeRef.Namespace
                let resolutionScope = MetadataToken.ofEntityHandle typeRef.ResolutionScope

                let result =
                    {
                        Name = prettyName
                        Namespace = prettyNamespace
                        ResolutionScope = resolutionScope
                    }

                builder.Add (ty, result)

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
            Attributes = attrs
            ExportedTypes = exportedTypes
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
