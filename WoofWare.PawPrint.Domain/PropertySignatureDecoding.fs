namespace WoofWare.PawPrint

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// Decoding of ECMA-335 II.23.2.5 PropertySig blobs.
[<RequireQualifiedAccess>]
module PropertySignatureDecoding =
    /// The property's own type and its index parameters, decoded from the Property row's signature
    /// blob. The result is a <c>MethodSignature</c> because a PropertySig has exactly a
    /// MethodDefSig's shape — <c>PROPERTY [|HASTHIS] ParamCount Type Param*</c> — with the
    /// property's type occupying the return type's position. That is not a coincidence PawPrint
    /// relies on privately: CoreCLR's <c>Signature_Init</c> reads a PROPERTY blob through the same
    /// <c>MetaSig</c> path it uses for methods, taking the property's type from
    /// <c>GetRetTypeHandleThrowing</c> and its index parameters from <c>NumFixedArgs</c>.
    ///
    /// Custom modifiers are preserved here, as <c>TypeDefn.Modified</c>; callers that report a type
    /// to a guest must strip them, because <c>MetaSig</c> does.
    let decode
        (assembly : AssemblyName)
        (metadataReader : MetadataReader)
        (signature : BlobHandle)
        : MethodSignature<TypeDefn>
        =
        // Check the calling convention before decoding rather than after. `DecodeMethodSignature`
        // rejects a FieldSig, but it accepts a *method* signature quite happily, and a MethodDefSig
        // read as a property would produce a confident wrong answer rather than an error: a generic
        // method's arity would be consumed as the parameter count.
        let headerKind =
            let mutable probe = metadataReader.GetBlobReader signature

            if probe.Length = 0 then
                failwith
                    "property signature blob is empty; every PropertySig carries at least a calling-convention byte"

            (probe.ReadSignatureHeader ()).Kind

        if headerKind <> SignatureKind.Property then
            failwith
                $"expected a property signature (the PROPERTY calling convention), but the blob's calling convention is %O{headerKind}"

        let mutable reader = metadataReader.GetBlobReader signature

        let decoder =
            SignatureDecoder<TypeDefn, unit> (TypeDefn.typeProvider assembly, metadataReader, ())

        try
            decoder.DecodeMethodSignature &reader
        with :? BadImageFormatException as e ->
            // Deliberately not `SignatureBlobDecoding.reraiseAsUndecodable`: that reports the
            // likely cause as an `ELEMENT_TYPE_INTERNAL` from a `DynamicMethod`-built blob, which
            // cannot be what happened here. These bytes come from a Property row in a PE image, so
            // a decode failure means the image is malformed.
            raise (
                BadImageFormatException (
                    $"could not decode the PropertySig at %O{signature} in %O{assembly}; the Property row's signature blob is malformed",
                    e
                )
            )
