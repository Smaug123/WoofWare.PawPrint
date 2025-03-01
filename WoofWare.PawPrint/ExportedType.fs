namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

type ExportedTypeData =
    | ForwardsTo of AssemblyReferenceHandle
    | NonForwarded of ExportedTypeHandle

type ExportedType =
    {
        Handle : ExportedTypeHandle
        Name : string
        Namespace : string option
        NamespaceDefn : NamespaceDefinitionHandle
        TypeAttrs : TypeAttributes
        Data : ExportedTypeData
    }

[<RequireQualifiedAccess>]
module ExportedType =
    let make
        (getString : StringHandle -> string)
        (handle : ExportedTypeHandle)
        (ty : System.Reflection.Metadata.ExportedType)
        : ExportedType
        =
        let name = getString ty.Name
        let ns = getString ty.Namespace
        let impl = MetadataToken.ofEntityHandle ty.Implementation
        let nsDef = ty.NamespaceDefinition

        let data =
            if ty.IsForwarder then
                match impl with
                | MetadataToken.AssemblyReference e -> ExportedTypeData.ForwardsTo e
                | _ -> failwith $"Expected forwarder type to have an assembly reference: {impl}"
            else
                match impl with
                | MetadataToken.ExportedType impl -> ExportedTypeData.NonForwarded impl
                | _ -> failwith $"Expected ExportedType implementation but got {impl}"

        {
            Handle = handle
            Name = name
            Namespace = if nsDef.IsNil then None else Some ns
            NamespaceDefn = nsDef
            TypeAttrs = ty.Attributes
            Data = data
        }
