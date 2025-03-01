namespace WoofWare.PawPrint

open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type MethodSpec =
    {
        Method : MetadataToken
    }

[<RequireQualifiedAccess>]
module MethodSpec =
    let make (p : MethodSpecification) : MethodSpec =
        {
            // Horrible abuse to get this as an int
            Method = MetadataToken.ofInt (p.Method.GetHashCode ())
        }

type BaseTypeInfo =
    | TypeDef of TypeDefinitionHandle
    | TypeRef of TypeReferenceHandle
    | ForeignAssemblyType of assemblyName : AssemblyName * TypeDefinitionHandle

type MethodImplParsed =
    | MethodImplementation of MethodImplementationHandle
    | MethodDefinition of MethodDefinitionHandle

type TypeInfo =
    {
        Namespace : string
        Name : string
        Methods : WoofWare.PawPrint.MethodInfo list
        MethodImpls : ImmutableDictionary<MethodImplementationHandle, MethodImplParsed>
        Fields : WoofWare.PawPrint.FieldInfo list
        BaseType : BaseTypeInfo option
        TypeAttributes : TypeAttributes
        Attributes : WoofWare.PawPrint.CustomAttribute list
        TypeDefHandle : TypeDefinitionHandle
    }

[<RequireQualifiedAccess>]
module TypeInfo =
    let internal read
        (loggerFactory : ILoggerFactory)
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (typeHandle : TypeDefinitionHandle)
        : TypeInfo
        =
        let typeDef = metadataReader.GetTypeDefinition typeHandle
        let methods = typeDef.GetMethods ()

        let methodImpls =
            typeDef.GetMethodImplementations ()
            |> Seq.map (fun handle ->
                let m = metadataReader.GetMethodImplementation handle
                let methodBody = MetadataToken.ofEntityHandle m.MethodBody

                match methodBody with
                | MetadataToken.MethodImplementation t ->
                    KeyValuePair (handle, MethodImplParsed.MethodImplementation t)
                | MetadataToken.MethodDef t -> KeyValuePair (handle, MethodImplParsed.MethodDefinition t)
                | k -> failwith $"unexpected kind: {k}"

            )
            |> ImmutableDictionary.CreateRange

        let fields =
            metadataReader.FieldDefinitions
            |> Seq.map (fun h -> FieldInfo.make metadataReader.GetString h (metadataReader.GetFieldDefinition h))
            |> Seq.toList

        let baseType =
            match MetadataToken.ofEntityHandle typeDef.BaseType with
            | TypeReference typeReferenceHandle -> Some (BaseTypeInfo.TypeRef typeReferenceHandle)
            | TypeDefinition typeDefinitionHandle -> Some (BaseTypeInfo.TypeDef typeDefinitionHandle)
            | t -> failwith $"Unrecognised base-type entity identifier: %O{t}"

        let name = metadataReader.GetString typeDef.Name
        let ns = metadataReader.GetString typeDef.Namespace
        let typeAttrs = typeDef.Attributes

        let attrs =
            typeDef.GetCustomAttributes ()
            |> Seq.map (fun h -> CustomAttribute.make h (metadataReader.GetCustomAttribute h))
            |> Seq.toList

        {
            Namespace = ns
            Name = name
            Methods =
                methods
                |> Seq.choose (fun m ->
                    let result = MethodInfo.read loggerFactory peReader metadataReader m

                    match result with
                    | None -> None
                    | Some x -> Some x
                )
                |> Seq.toList
            MethodImpls = methodImpls
            Fields = fields
            BaseType = baseType
            TypeAttributes = typeAttrs
            Attributes = attrs
            TypeDefHandle = typeHandle
        }
