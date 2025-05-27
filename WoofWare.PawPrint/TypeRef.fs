namespace WoofWare.PawPrint

open System.Reflection.Metadata

type TypeRefResolutionScope =
    | Assembly of AssemblyReferenceHandle
    | ModuleRef of ModuleReferenceHandle
    | TypeRef of TypeReferenceHandle

/// <summary>
/// Represents a type reference in a .NET assembly metadata.
/// This corresponds to a TypeReferenceHandle in System.Reflection.Metadata.
/// </summary>
type TypeRef =
    {
        /// <summary>The simple name of the referenced type (without namespace).</summary>
        Name : string

        /// <summary>The namespace of the referenced type, or empty string for nested types.</summary>
        Namespace : string

        /// <summary>
        /// The scope of the type reference: where to find the type.
        /// </summary>
        ResolutionScope : TypeRefResolutionScope
    }

[<RequireQualifiedAccess>]
module TypeRef =
    let make (metadataReader : MetadataReader) (ty : TypeReferenceHandle) : TypeRef =
        let typeRef = metadataReader.GetTypeReference ty
        let prettyName = metadataReader.GetString typeRef.Name
        let prettyNamespace = metadataReader.GetString typeRef.Namespace

        let resolutionScope =
            match MetadataToken.ofEntityHandle typeRef.ResolutionScope with
            | MetadataToken.AssemblyReference ref -> TypeRefResolutionScope.Assembly ref
            | MetadataToken.ModuleReference ref -> TypeRefResolutionScope.ModuleRef ref
            | MetadataToken.TypeReference ref -> TypeRefResolutionScope.TypeRef ref
            | handle -> failwith $"Unexpected TypeRef resolution scope: {handle}"

        {
            Name = prettyName
            Namespace = prettyNamespace
            ResolutionScope = resolutionScope
        }
