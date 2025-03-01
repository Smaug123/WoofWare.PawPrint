namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// <summary>
/// Represents a type specification in assembly metadata.
/// Type specifications describe complex types like generic instantiations,
/// arrays, pointers, and other composite types.
/// </summary>
type TypeSpec =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this type specification.
        /// </summary>
        Handle : TypeSpecificationHandle

        /// <summary>
        /// The full type definition/signature of this type specification.
        /// This contains all the details about the composite type structure.
        /// </summary>
        Signature : TypeDefn
    }

[<RequireQualifiedAccess>]
module TypeSpec =
    let make (handle : TypeSpecificationHandle) (r : TypeSpecification) : TypeSpec =
        let spec = r.DecodeSignature (TypeDefn.typeProvider, ())

        {
            Handle = handle
            Signature = spec
        }
