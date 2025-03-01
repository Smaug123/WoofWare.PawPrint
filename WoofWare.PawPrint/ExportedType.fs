namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

/// <summary>
/// Represents the implementation details of an exported type.
/// This discriminated union indicates whether the type is forwarded to another assembly
/// or references another exported type.
/// </summary>
type ExportedTypeData =
    /// <summary>
    /// Indicates the type is forwarded to another assembly.
    /// Type forwarders are used to redirect type references to implementations in other assemblies.
    /// </summary>
    | ForwardsTo of AssemblyReferenceHandle

    /// <summary>
    /// Indicates the type references another exported type within the assembly.
    /// This is often used for nested types.
    /// </summary>
    | NonForwarded of ExportedTypeHandle

/// <summary>
/// Represents a type exported from an assembly.
/// Exported types are types that are defined in one module but made visible
/// at the assembly level, or types that are forwarded to another assembly.
/// </summary>
type ExportedType =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this exported type.
        /// </summary>
        Handle : ExportedTypeHandle

        /// <summary>
        /// The name of the exported type.
        /// </summary>
        Name : string

        /// <summary>
        /// The namespace containing the exported type, if any.
        /// None if the type is not in a namespace.
        /// </summary>
        Namespace : string option

        /// <summary>
        /// The metadata handle for the namespace definition containing this type.
        /// </summary>
        NamespaceDefn : NamespaceDefinitionHandle

        /// <summary>
        /// The type attributes (visibility, inheritance characteristics, etc.) for this exported type.
        /// </summary>
        TypeAttrs : TypeAttributes

        /// <summary>
        /// The implementation details of this exported type, indicating whether it forwards
        /// to another assembly or references another exported type.
        /// </summary>
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
