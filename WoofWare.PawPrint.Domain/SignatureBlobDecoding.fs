namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

/// <summary>
/// Shared machinery for decoding a raw signature blob that arrived without a metadata row to own
/// it.
/// </summary>
/// <remarks>
/// A signature read out of a PE image is reached through a handle into the tables that produced
/// it, so <c>MetadataReader</c> can find its bytes. A signature minted by <c>Reflection.Emit</c>
/// has no such row: it is a bare <c>byte[]</c> handed across the runtime boundary, so it has to be
/// decoded from the bytes directly. <see cref="LocalSignatureDecoding" /> and
/// <see cref="MethodSignatureDecoding" /> are the two entry points that do so, and they fail in
/// the same way for the same reason; this is that shared failure.
/// </remarks>
[<RequireQualifiedAccess>]
module SignatureBlobDecoding =

    /// <summary>
    /// Re-raise a <see cref="System.BadImageFormatException" /> from <c>SignatureDecoder</c> as an
    /// exception naming the cause a <c>Reflection.Emit</c>-produced blob is overwhelmingly likely
    /// to have.
    /// </summary>
    /// <param name="what">
    /// What was being decoded, phrased to complete "could not decode a ...": for example
    /// <c>"local variable signature blob"</c>.
    /// </param>
    /// <remarks>
    /// Deliberately not narrowed to "the blob contains a 0x21 byte": 0x21 is also a perfectly
    /// ordinary payload byte inside a compressed integer or a type name, so such a test would be
    /// wrong in both directions. A genuinely malformed blob gets this message too, which is why it
    /// is phrased as the likely cause rather than as the finding.
    /// </remarks>
    let reraiseAsUndecodable (what : string) (e : BadImageFormatException) : 'a =
        // The overwhelmingly likely cause, and the one worth naming, is a signature built by
        // `SignatureHelper` with no module to spell types against: `AddOneArgTypeHelper` then
        // takes its `m_module == null` branch and emits `ELEMENT_TYPE_INTERNAL` (0x21) followed by
        // the raw bytes of the type's handle, which is not a signature element `MetadataReader`
        // knows how to read. That encoding is a separate piece of work; see "Prerequisites that
        // are not emit" in the Reflection.Emit tracking issue.
        raise (
            Exception (
                $"could not decode a %s{what}. If it came from a DynamicMethod, the likely cause is ELEMENT_TYPE_INTERNAL (0x21), which SignatureHelper emits for any type that is not one of the primitives, object or string, and which PawPrint cannot yet read.",
                e
            )
        )
