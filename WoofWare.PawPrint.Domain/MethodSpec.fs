namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

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
        ///
        /// It is empty when the blob names no argument, which a well-formed image never does, and
        /// which is still an instantiation (of the wrong arity, unless the method is not generic).
        /// </remarks>
        Signature : TypeDefn ImmutableArray
    }

[<RequireQualifiedAccess>]
module MethodSpec =
    /// <summary>
    /// Read a MethodSpec row. An instantiation of no arguments is read as an empty
    /// <c>Signature</c>, which is not the absence of an instantiation: binding it to a generic
    /// method is refused for its arity, and binding it to a non-generic method names that method.
    /// </summary>
    /// <exception cref="System.BadImageFormatException">
    /// The blob's first byte is not GENERICINST, or an argument cannot be decoded.
    /// </exception>
    let make (assemblyName : AssemblyName) (metadataReader : MetadataReader) (p : MethodSpecification) : MethodSpec =
        // Not `MethodSpecification.DecodeSignature`, which refuses a count of zero. CoreCLR reads
        // the count and loads that many arguments (memberload.cpp, GetMethodDescFromMethodSpec),
        // and only the arity check in FindOrCreateAssociatedMethodDesc, when the MethodSpec is
        // used, can refuse it.
        let mutable reader = metadataReader.GetBlobReader p.Signature
        let header = reader.ReadSignatureHeader ()

        // The whole byte, as CoreCLR compares it: GENERICINST with any flag set is refused too.
        if header.RawValue <> byte SignatureKind.MethodSpecification then
            raise (
                System.BadImageFormatException (
                    $"a MethodSpec's signature blob opens with the byte 0x%02x{header.RawValue}, not GENERICINST (0x0a)"
                )
            )

        let count = reader.ReadCompressedInteger ()

        let decoder =
            SignatureDecoder<TypeDefn, unit> (TypeDefn.typeProvider assemblyName, metadataReader, ())

        let signature = ImmutableArray.CreateBuilder<TypeDefn> ()

        for _ in 1..count do
            signature.Add (decoder.DecodeType (&reader, false))

        {
            Method = MetadataToken.ofEntityHandle p.Method
            Signature = signature.ToImmutable ()
        }
