namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

/// <summary>
/// Represents a method specification, which provides information about a method,
/// particularly for generic method instantiations.
/// </summary>
type MethodSpec =
    {
        /// <summary>
        /// The token that identifies the method being specialized.
        /// </summary>
        Method : MetadataToken

        Signature : TypeDefn ImmutableArray
    }

[<RequireQualifiedAccess>]
module MethodSpec =
    let make (assemblyName : AssemblyName) (p : MethodSpecification) : MethodSpec =
        let signature = p.DecodeSignature (TypeDefn.typeProvider assemblyName, ())

        {
            // Horrible abuse to get this as an int
            Method = MetadataToken.ofInt (p.Method.GetHashCode ())
            Signature = signature
        }
