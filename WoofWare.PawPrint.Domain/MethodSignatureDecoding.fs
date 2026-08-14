namespace WoofWare.PawPrint

// `fixed`, to pin the signature bytes for the BlobReader below.
#nowarn "9"

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// <summary>
/// Decoding of a MethodDefSig (ECMA-335 II.23.2.1) from its raw blob bytes.
/// </summary>
/// <remarks>
/// <para>
/// A method defined in a PE image reaches its signature through its <c>MethodDefinition</c> row,
/// so <c>MethodDefinition.DecodeSignature</c> suffices and this module has nothing to say about
/// it. A method minted by <c>Reflection.Emit</c> has no such row: <c>DynamicILGenerator</c> passes
/// <c>ModuleHandle.GetDynamicMethod</c> the bytes <c>SignatureHelper</c> produced
/// (DynamicILGenerator.cs:24-31), PawPrint records them verbatim on the
/// <c>DynamicMethodDefinition</c>, and there is nowhere to look them up. Hence this entry point,
/// which takes the blob directly. It is the method-signature counterpart of
/// <see cref="LocalSignatureDecoding" />, which exists for the same reason.
/// </para>
/// <para>
/// The alphabet a dynamic method's signature can be spelled in is narrower than metadata's,
/// because <c>SignatureHelper</c> builds it with a null module. It classifies a type by
/// <c>RuntimeType.GetCorElementType()</c> and takes the "simple" branch for anything up to
/// <c>ELEMENT_TYPE_STRING</c> plus <c>TYPEDBYREF</c>/<c>I</c>/<c>U</c>/<c>OBJECT</c>
/// (SignatureHelper.cs:653-659). Anything it cannot spell that way takes the
/// <c>ELEMENT_TYPE_INTERNAL</c> branch (SignatureHelper.cs:449-452) -- a raw type-handle pointer,
/// which is not something <c>MetadataReader</c> can read, and which is refused loudly here rather
/// than decoded to something plausible.
/// </para>
/// <para>
/// The alphabet is wider than that list, though, because it is closed under three constructors:
/// <c>IsByRef</c>, <c>IsPointer</c> and <c>IsArray</c> are each tested *before* the simple-type
/// check and recurse into the element type (SignatureHelper.cs:392-424). So <c>int[]</c>,
/// <c>ref int</c> and <c>int**</c> are all perfectly spellable -- which matters, since emitting a
/// dynamic method over an array is an entirely ordinary thing to do, and reading this as "simple
/// types only" would wrongly say it could not be bound. An array of an unspellable element is
/// still unspellable.
/// </para>
/// <para>
/// An enum is <em>not</em> an exception to that, despite <c>IsSimpleType</c> classifying by
/// element type: measured against the real encoder, a parameter typed as an enum is spelled
/// <c>ELEMENT_TYPE_INTERNAL</c> like any other value type, whatever its underlying integer.
/// <c>TestMethodSignatureDecoding</c> pins that for two enums of different underlying width, so
/// this claim cannot rot into a lie -- and it is worth pinning, because the natural guess is the
/// opposite.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module MethodSignatureDecoding =

    /// <summary>
    /// Decode a MethodDefSig blob into its return type and parameter types.
    /// </summary>
    /// <param name="assembly">
    /// The token universe any <c>TypeDef</c>/<c>TypeRef</c>/<c>TypeSpec</c> in the blob is drawn
    /// from, and the assembly the resulting <see cref="TypeDefn" />s name themselves against.
    /// </param>
    /// <param name="metadataReader">
    /// The reader those same tokens are resolved against. Must be the reader of
    /// <paramref name="assembly" />: a signature decoded against a second image's tables would
    /// silently name whichever rows happened to sit at those indices.
    /// </param>
    /// <remarks>
    /// <para>
    /// The result is faithful, not filtered: a vararg signature comes back with
    /// <c>Header.CallingConvention</c> saying so and <c>RequiredParameterCount</c> distinguishing
    /// the fixed parameters from the rest, and a generic method's arity comes back in
    /// <c>GenericParameterCount</c>. Neither is rejected here. Whether PawPrint can *do* anything
    /// with such a method is a question for whoever is asking -- binding a delegate to it, say --
    /// and that consumer has the context to say why not; a decoder that refused them would be
    /// discarding information the returned type already carries perfectly well, and would be
    /// answering a question it was not asked.
    /// </para>
    /// <para>
    /// What is refused is a blob this cannot decode *honestly*: one that is not a method signature
    /// at all, one whose types are spelled in an encoding PawPrint cannot read, one whose declared
    /// parameter count exceeds the bytes available to spell those parameters in, and one with
    /// bytes left over.
    /// </para>
    /// <para>
    /// Those last two are corruption checks, and it is worth being precise about how corrupt a
    /// blob can actually get here, because that bounds how much validation is worth doing. No
    /// guest supplies these bytes directly: <c>ModuleHandle.GetDynamicMethod</c> is internal, and
    /// every route to it (<c>DynamicMethod.GetILGenerator</c>, <c>GetDynamicILInfo</c>) builds the
    /// signature with <c>SignatureHelper</c> from the <c>Type</c>s passed to the
    /// <c>DynamicMethod</c> constructor. So this decodes <c>SignatureHelper</c> output in
    /// practice, and these checks exist to turn interpreter-level corruption into a diagnosable
    /// failure rather than to defend a hostile boundary.
    /// </para>
    /// <para>
    /// That is also why there is no check that the decoded *types* are ones <c>SignatureHelper</c>
    /// could have produced. A hand-crafted blob can spell <c>ELEMENT_TYPE_FNPTR</c>, for which
    /// <c>SignatureHelper</c> has no branch at all -- a function-pointer type fails
    /// <c>IsSimpleType</c> and so would take the <c>ELEMENT_TYPE_INTERNAL</c> branch -- so such a
    /// signature cannot have come from a real dynamic method. It nonetheless decodes into a
    /// well-formed <c>TypeDefn.FunctionPointer</c>, and consumers that cannot handle one already
    /// refuse it by name. Screening the decoded shape against the encoder's reachable output would
    /// mean a whitelist walk over every <see cref="TypeDefn" /> case; that is the right change if
    /// this entry point ever becomes reachable with guest-chosen bytes, and disproportionate
    /// until then.
    /// </para>
    /// </remarks>
    let decode (assembly : AssemblyName) (metadataReader : MetadataReader) (blob : byte[]) : MethodSignature<TypeDefn> =
        // `BlobReader` accepts a null pointer precisely when the length is zero, which is what
        // `fixed` yields for an empty array. An empty blob is not a valid MethodDefSig at all, and
        // CoreCLR agrees: `DynamicMethodTable::GetDynamicMethod` carries `PRECONDITION(sigSize >
        // 0)` (dynamicmethod.cpp:229). Report it rather than letting `BlobReader` decide.
        if blob.Length = 0 then
            failwith "method signature blob is empty; every MethodDefSig carries at least a calling-convention byte"

        use bytes = fixed blob

        // Check the calling convention before decoding rather than after. `DecodeMethodSignature`
        // reads the header and then keeps going, so handing it a LocalVarSig or a FieldSig
        // produces a confident, wrong answer (a field signature's type would be read as a
        // parameter count) instead of an error.
        let headerKind, declaredParameters, bytesAfterCounts =
            let mutable probe : BlobReader = BlobReader (bytes, blob.Length)
            let header = probe.ReadSignatureHeader ()

            if header.Kind <> SignatureKind.Method then
                // Read nothing further: the counts below are only where they are because this is
                // a method signature.
                header.Kind, 0, 0
            else

            // A generic method's arity precedes the parameter count.
            if header.IsGeneric then
                probe.ReadCompressedInteger () |> ignore

            let declaredParameters = probe.ReadCompressedInteger ()
            header.Kind, declaredParameters, probe.RemainingBytes

        if headerKind <> SignatureKind.Method then
            failwith
                $"expected a method signature (one of the METHOD calling conventions), but the blob's calling convention is %O{headerKind}"

        // This is a diagnosability check, not a safety one, and it is worth saying which because
        // the obvious reason to write it is wrong. `SignatureDecoder` does *not* blow up on an
        // absurd declared count: measured, a blob declaring half a billion parameters is refused
        // in under a millisecond with no large allocation attempted. What it is refused *with* is
        // the problem -- a bare `BadImageFormatException`, which this module's catch then reports
        // as the likely `ELEMENT_TYPE_INTERNAL` cause, sending whoever reads it hunting for a
        // missing encoding when the blob is merely truncated.
        //
        // Deciding it here instead is exact rather than a magic cap: every parameter occupies at
        // least one byte, and so does the return type, so a signature declaring N parameters needs
        // strictly more than N bytes after its counts. That is decidable without reading them.
        if declaredParameters >= bytesAfterCounts then
            failwith
                $"method signature blob declares %d{declaredParameters} parameter(s) but has only %d{bytesAfterCounts} byte(s) left to spell them and the return type in; it is truncated or corrupt"

        let mutable reader : BlobReader = BlobReader (bytes, blob.Length)

        let decoder =
            SignatureDecoder<TypeDefn, unit> (TypeDefn.typeProvider assembly, metadataReader, ())

        let decoded =
            try
                decoder.DecodeMethodSignature &reader
            with :? BadImageFormatException as e ->
                SignatureBlobDecoding.reraiseAsUndecodable "method signature blob" e

        // `DecodeMethodSignature` stops as soon as it has read the declared number of parameters,
        // so a blob that is a valid signature followed by anything at all decodes "successfully"
        // and the tail is silently dropped. That is precisely the shape a corrupted or
        // concatenated blob has, and these bytes come from guest memory, so say so instead.
        //
        // One trailing byte is expected and legal, though, and it is what every real dynamic
        // method carries: `DynamicMethod.GetILGenerator` builds the blob with
        // `SignatureHelper.GetSignature(true)` (DynamicMethod.CoreCLR.cs:161-162), whose `true`
        // means "append ELEMENT_TYPE_END" (SignatureHelper.cs:870-881). ECMA-335 II.23.2.1 does
        // not put that byte in a MethodDefSig, so a blob without it is equally valid and is what
        // the public `SignatureHelper.GetSignature()` produces; accept either.
        match reader.RemainingBytes with
        | 0 -> ()
        | 1 ->
            let trailing = reader.ReadByte ()

            if trailing <> 0uy then
                failwith
                    $"method signature blob has one byte left over after its %d{decoded.ParameterTypes.Length} declared parameter(s), and it is 0x%02x{trailing} rather than the ELEMENT_TYPE_END (0x00) that SignatureHelper appends"
        | remaining ->
            failwith
                $"method signature blob has %d{remaining} bytes left over after its %d{decoded.ParameterTypes.Length} declared parameter(s); at most one trailing ELEMENT_TYPE_END is expected, so this blob is truncated, corrupt, or several signatures concatenated"

        decoded
