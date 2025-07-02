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

        /// <summary>
        /// The actual type arguments for generic instantiation.
        /// </summary>
        /// <example>
        /// For <c>Volatile.Read&lt;System.IO.TextWriter&gt;</c>, the <c>Signature</c> is <c>[System.IO.TextWriter]</c>.
        /// </example>
        /// <remarks>
        /// The contents might themselves be <c>TypeDefn.GenericMethodParameter</c>, for example.
        /// This happens when the method is itself being called from within a generic method, and the generic parameters
        /// of the spec are being instantiated with generic parameters from the caller.
        /// </remarks>
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
