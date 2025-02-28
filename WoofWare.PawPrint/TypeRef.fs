namespace WoofWare.PawPrint

open System.Reflection.Metadata

type TypeRef =
    {
        Name : string
        Namespace : string
        ResolutionScope : WoofWare.PawPrint.MetadataToken
    }

[<RequireQualifiedAccess>]
module TypeRef =
    let make (metadataReader : MetadataReader) (ty : TypeReferenceHandle) : TypeRef =
        let typeRef = metadataReader.GetTypeReference ty
        let prettyName = metadataReader.GetString typeRef.Name
        let prettyNamespace = metadataReader.GetString typeRef.Namespace
        let resolutionScope = MetadataToken.ofEntityHandle typeRef.ResolutionScope

        {
            Name = prettyName
            Namespace = prettyNamespace
            ResolutionScope = resolutionScope
        }
