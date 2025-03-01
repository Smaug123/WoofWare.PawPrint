namespace WoofWare.PawPrint

open System.Reflection.Metadata

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
        /// The scope of the type reference. This can be:
        /// - AssemblyReference token: When the type is defined in another assembly
        /// - ModuleReference token: When the type is defined in another module of the same assembly
        /// - TypeReference token: When the type is a nested type
        /// - ModuleDefinition token: When the type is defined in the current module
        /// </summary>
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
