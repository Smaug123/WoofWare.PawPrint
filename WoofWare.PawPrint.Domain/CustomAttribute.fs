namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// <summary>
/// Represents a custom attribute applied to a type, method, field, or other metadata entity.
/// This is a strongly-typed representation of CustomAttribute from System.Reflection.Metadata.
/// </summary>
type CustomAttribute =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this custom attribute in the assembly.
        /// </summary>
        Handle : CustomAttributeHandle

        /// <summary>
        /// The metadata entity (TypeDef, MethodDef, FieldDef, Assembly, etc.) to which this
        /// custom attribute is applied.
        /// </summary>
        Parent : MetadataToken

        /// <summary>
        /// The constructor method used to create this custom attribute instance.
        /// This token references the method that constructs the attribute.
        /// </summary>
        Constructor : MetadataToken

        /// <summary>
        /// The encoded constructor-argument blob for this custom attribute (the bytes of
        /// the <c>Value</c> blob in ECMA-335 II.23.3). The CoreCLR <c>MetadataImport</c>
        /// returns this as a <c>ConstArray</c> from <c>GetCustomAttributeProps</c>.
        /// </summary>
        Value : ImmutableArray<byte>
    }

[<RequireQualifiedAccess>]
module CustomAttribute =
    let make
        (metadataReader : MetadataReader)
        (handle : CustomAttributeHandle)
        (attr : System.Reflection.Metadata.CustomAttribute)
        : CustomAttribute
        =
        let parent = attr.Parent |> MetadataToken.ofEntityHandle
        let ctor = attr.Constructor |> MetadataToken.ofEntityHandle

        let value : ImmutableArray<byte> =
            if attr.Value.IsNil then
                ImmutableArray.Empty
            else
                ImmutableArray.Create<byte> (metadataReader.GetBlobBytes attr.Value)

        {
            Handle = handle
            Parent = parent
            Constructor = ctor
            Value = value
        }

    /// <summary>
    /// Decode the leading <c>SerString</c> from a <c>CustomAttrib</c> blob
    /// (ECMA-335 II.23.3). The blob must start with the two-byte prolog
    /// <c>0x0001</c> followed by a <c>SerString</c> as the first fixed argument.
    /// </summary>
    /// <returns>
    /// <c>Ok None</c> if the leading <c>SerString</c> is the null sentinel
    /// (<c>0xFF</c>); <c>Ok (Some s)</c> for an empty or non-empty UTF-8 string;
    /// <c>Error msg</c> if the blob is malformed for this purpose.
    /// </returns>
    /// <remarks>
    /// Trailing bytes (subsequent fixed args, the <c>NumNamed</c> count, and any
    /// named args) are ignored. Mirrors CoreCLR's
    /// <c>CustomAttributeParser::GetString</c> in <c>caparser.h</c>.
    /// </remarks>
    let tryReadLeadingSerString (blob : ImmutableArray<byte>) : Result<string option, string> =
        let len = blob.Length

        if len < 2 then
            Error "CustomAttrib blob is shorter than the 2-byte prolog"
        else
            let prolog = uint16 blob.[0] ||| (uint16 blob.[1] <<< 8)

            if prolog <> 0x0001us then
                Error (sprintf "CustomAttrib blob has unexpected prolog 0x%04X (expected 0x0001)" prolog)
            elif len = 2 then
                Error "CustomAttrib blob ends after the prolog; expected a SerString"
            else
                let first = blob.[2]

                if first = 0xFFuy then
                    Ok None
                else
                    // PackedLen per ECMA-335 II.23.2.
                    let lengthResult : Result<int * int, string> =
                        if first &&& 0x80uy = 0uy then
                            Ok (int first, 3)
                        elif first &&& 0xC0uy = 0x80uy then
                            if len < 4 then
                                Error "CustomAttrib blob: truncated 2-byte PackedLen"
                            else
                                let length = ((int first &&& 0x3F) <<< 8) ||| int blob.[3]
                                Ok (length, 4)
                        elif first &&& 0xE0uy = 0xC0uy then
                            if len < 6 then
                                Error "CustomAttrib blob: truncated 4-byte PackedLen"
                            else
                                let length =
                                    ((int first &&& 0x1F) <<< 24)
                                    ||| (int blob.[3] <<< 16)
                                    ||| (int blob.[4] <<< 8)
                                    ||| int blob.[5]

                                Ok (length, 6)
                        else
                            Error (sprintf "CustomAttrib blob: invalid PackedLen leading byte 0x%02X" first)

                    match lengthResult with
                    | Error e -> Error e
                    | Ok (length, bodyStart) ->
                        if bodyStart + length > len then
                            Error (
                                sprintf
                                    "CustomAttrib blob: SerString body truncated (declared %d bytes, %d available)"
                                    length
                                    (len - bodyStart)
                            )
                        else
                            let bytes = Array.zeroCreate<byte> length
                            blob.CopyTo (bodyStart, bytes, 0, length)
                            let s = System.Text.Encoding.UTF8.GetString bytes
                            Ok (Some s)
