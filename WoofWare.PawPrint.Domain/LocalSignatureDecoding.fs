namespace WoofWare.PawPrint

// `fixed`, to pin the signature bytes for the BlobReader below.
#nowarn "9"

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// <summary>
/// Decoding of a method body's local-variable signature (ECMA-335 II.23.2.6) from its raw blob
/// bytes.
/// </summary>
/// <remarks>
/// A body read out of a PE image reaches its locals through a <c>StandaloneSignatureHandle</c>,
/// which is a row in the metadata that owns it. A body minted by <c>Reflection.Emit</c> has no
/// such row: <c>DynamicResolver.GetLocalsSignature</c> hands back the bytes
/// <c>SignatureHelper.InternalGetSignatureArray</c> produced, and there is nowhere to look them
/// up. Hence this entry point, which takes the blob directly.
/// </remarks>
[<RequireQualifiedAccess>]
module LocalSignatureDecoding =

    /// <summary>
    /// Decode a LocalVarSig blob into one <see cref="TypeDefn"/> per local, in declaration order.
    /// </summary>
    /// <param name="assembly">
    /// The token universe any <c>TypeDef</c>/<c>TypeRef</c>/<c>TypeSpec</c> in the blob is drawn
    /// from, and the assembly the resulting <see cref="TypeDefn"/>s name themselves against.
    /// </param>
    /// <param name="metadataReader">
    /// The reader those same tokens are resolved against. Must be the reader of
    /// <paramref name="assembly" />: a signature decoded against a second image's tables would
    /// silently name whichever rows happened to sit at those indices.
    /// </param>
    let decode (assembly : AssemblyName) (metadataReader : MetadataReader) (blob : byte[]) : ImmutableArray<TypeDefn> =
        // `BlobReader` accepts a null pointer precisely when the length is zero, which is what
        // `fixed` yields for an empty array; but an empty blob is not a valid LocalVarSig at all,
        // and letting it through would decode to "no locals" rather than being reported.
        if blob.Length = 0 then
            failwith
                "local variable signature blob is empty; every LocalVarSig carries at least the 0x07 calling-convention byte"

        use bytes = fixed blob

        // ECMA-335 II.23.2.6 spells a LocalVarSig as `LOCAL_SIG Count Type+`, and `MetadataReader`
        // enforces the `+`: `DecodeLocalSignature` on a zero-count blob throws "Signature type
        // sequence must have at least one element". A dynamic method with no locals produces
        // exactly that blob, though — `DynamicILGenerator` builds `m_localSignature` from
        // `SignatureHelper.GetLocalVarSigHelper()` whether or not `DeclareLocal` is ever called,
        // and `InternalGetSignatureArray` then writes a count of zero — so it is the common case
        // rather than a malformed one. Read the count first and answer it here.
        let localCount =
            let mutable probe : BlobReader = BlobReader (bytes, blob.Length)
            let header = probe.ReadSignatureHeader ()

            if header.Kind <> SignatureKind.LocalVariables then
                failwith
                    $"expected a local variable signature (LOCAL_SIG, 0x07), but the blob's calling convention is %O{header.Kind}"

            probe.ReadCompressedInteger ()

        if localCount = 0 then
            ImmutableArray.Empty
        else

        let mutable reader : BlobReader = BlobReader (bytes, blob.Length)

        let decoder =
            SignatureDecoder<TypeDefn, unit> (TypeDefn.typeProvider assembly, metadataReader, ())

        try
            decoder.DecodeLocalSignature &reader
        with :? BadImageFormatException as e ->
            // The overwhelmingly likely cause, and the one worth naming, is a signature built by
            // `SignatureHelper` with no module to spell types against: `AddOneArgTypeHelper` then
            // takes its `m_module == null` branch and emits `ELEMENT_TYPE_INTERNAL` (0x21)
            // followed by the raw bytes of the type's handle, which is not a signature element
            // `MetadataReader` knows how to read. That encoding is a separate piece of work; see
            // "Prerequisites that are not emit" in the Reflection.Emit tracking issue.
            //
            // Deliberately not narrowed to "the blob contains a 0x21 byte": 0x21 is also a
            // perfectly ordinary payload byte inside a compressed integer or a type name, so such
            // a test would be wrong in both directions. A genuinely malformed blob gets this
            // message too, which is why it is phrased as the likely cause rather than the finding.
            raise (
                Exception (
                    "could not decode a local variable signature blob. If it came from a DynamicMethod, the likely cause is ELEMENT_TYPE_INTERNAL (0x21), which SignatureHelper emits for any type that is not one of the primitives, object or string, and which PawPrint cannot yet read.",
                    e
                )
            )
