namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// <summary>
/// One decoded fixed argument from a <c>CustomAttrib</c> blob (ECMA-335 II.23.3).
/// Each variant corresponds to a <c>CorSerializationType</c> the metadata blob can carry.
/// </summary>
/// <remarks>
/// Not all <c>CorSerializationType</c> values are represented yet:
/// <c>TYPE</c> (0x50), <c>TAGGED_OBJECT</c> (0x51), <c>ENUM</c> (0x55), and
/// <c>SZARRAY</c> (0x1d) will be added when the QCall handler needs them.
/// The current set covers attributes whose ctors take only primitives and strings,
/// which is the dominant shape on the ResourceManager-init code path.
/// </remarks>
[<RequireQualifiedAccess>]
type CustomAttribFixedArg =
    | Bool of bool
    | Char of char
    | I1 of sbyte
    | U1 of byte
    | I2 of int16
    | U2 of uint16
    | I4 of int32
    | U4 of uint32
    | I8 of int64
    | U8 of uint64
    | R4 of float32
    | R8 of double
    /// <c>None</c> for the SerString null sentinel (a single <c>0xFF</c> byte);
    /// <c>Some ""</c> for the empty string; <c>Some s</c> for a non-empty UTF-8 string.
    | String of string option

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

    /// <summary>
    /// Read a <c>PackedLen</c> (ECMA-335 II.23.2) starting at the given offset.
    /// Returns the decoded length and the offset of the first byte after the
    /// PackedLen header.
    /// </summary>
    let internal readPackedLen (blob : ImmutableArray<byte>) (offset : int) : Result<int * int, string> =
        let len = blob.Length

        if offset >= len then
            Error (sprintf "CustomAttrib blob: PackedLen begins at offset %d but blob has only %d bytes" offset len)
        else
            let first = blob.[offset]

            if first &&& 0x80uy = 0uy then
                Ok (int first, offset + 1)
            elif first &&& 0xC0uy = 0x80uy then
                if offset + 2 > len then
                    Error "CustomAttrib blob: truncated 2-byte PackedLen"
                else
                    let length = ((int first &&& 0x3F) <<< 8) ||| int blob.[offset + 1]
                    Ok (length, offset + 2)
            elif first &&& 0xE0uy = 0xC0uy then
                if offset + 4 > len then
                    Error "CustomAttrib blob: truncated 4-byte PackedLen"
                else
                    let length =
                        ((int first &&& 0x1F) <<< 24)
                        ||| (int blob.[offset + 1] <<< 16)
                        ||| (int blob.[offset + 2] <<< 8)
                        ||| int blob.[offset + 3]

                    Ok (length, offset + 4)
            else
                Error (sprintf "CustomAttrib blob: invalid PackedLen leading byte 0x%02X" first)

    /// <summary>
    /// Read a <c>SerString</c> (ECMA-335 II.23.3) starting at the given offset.
    /// Returns <c>None</c> for the null sentinel (a single <c>0xFF</c> byte) and
    /// <c>Some s</c> for an empty or non-empty UTF-8 string, paired with the
    /// offset of the first byte after the SerString.
    /// </summary>
    let internal readSerString (blob : ImmutableArray<byte>) (offset : int) : Result<string option * int, string> =
        let len = blob.Length

        if offset >= len then
            Error (sprintf "CustomAttrib blob: SerString begins at offset %d but blob has only %d bytes" offset len)
        elif blob.[offset] = 0xFFuy then
            Ok (None, offset + 1)
        else
            match readPackedLen blob offset with
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
                    Ok (Some s, bodyStart + length)

    /// <summary>
    /// Decode the fixed-args section of a <c>CustomAttrib</c> blob (ECMA-335 II.23.3).
    /// The blob must start with the two-byte prolog <c>0x0001</c>, followed by one
    /// fixed-arg value for each entry in <paramref name="paramTypes"/> (in declared order),
    /// encoded per ECMA-335 II.23.3 / <c>CorSerializationType</c>.
    /// </summary>
    /// <param name="paramTypes">The constructor's parameter types in declaration order.</param>
    /// <param name="blob">The raw <c>CustomAttrib</c> blob.</param>
    /// <returns>
    /// On success: the decoded fixed-arg values in declaration order, and the offset of the
    /// first byte after the fixed-args section (i.e. where the <c>NumNamed</c> count, if any,
    /// begins). On failure: a diagnostic message.
    /// </returns>
    /// <remarks>
    /// Mirrors the per-arg loop in CoreCLR's <c>CustomAttribute_CreateCustomAttributeInstance</c>
    /// (<c>customattribute.cpp:900</c>), which dispatches via <c>GetDataFromBlob</c>.
    /// </remarks>
    let readFixedArgs
        (paramTypes : TypeDefn list)
        (blob : ImmutableArray<byte>)
        : Result<CustomAttribFixedArg list * int, string>
        =
        let len = blob.Length

        if len < 2 then
            Error "CustomAttrib blob is shorter than the 2-byte prolog"
        else

        let prolog = uint16 blob.[0] ||| (uint16 blob.[1] <<< 8)

        if prolog <> 0x0001us then
            Error (sprintf "CustomAttrib blob has unexpected prolog 0x%04X (expected 0x0001)" prolog)
        else

        let readPrimitive
            (size : int)
            (offset : int)
            (build : ImmutableArray<byte> -> int -> CustomAttribFixedArg)
            : Result<CustomAttribFixedArg * int, string>
            =
            if offset + size > len then
                Error (
                    sprintf
                        "CustomAttrib blob: primitive of size %d at offset %d would overrun blob length %d"
                        size
                        offset
                        len
                )
            else
                Ok (build blob offset, offset + size)

        let readOne (paramType : TypeDefn) (offset : int) : Result<CustomAttribFixedArg * int, string> =
            match paramType with
            | TypeDefn.PrimitiveType pt ->
                match pt with
                | PrimitiveType.Boolean -> readPrimitive 1 offset (fun b o -> CustomAttribFixedArg.Bool (b.[o] <> 0uy))
                | PrimitiveType.Char ->
                    readPrimitive
                        2
                        offset
                        (fun b o ->
                            let v = uint16 b.[o] ||| (uint16 b.[o + 1] <<< 8)
                            CustomAttribFixedArg.Char (char v)
                        )
                | PrimitiveType.SByte -> readPrimitive 1 offset (fun b o -> CustomAttribFixedArg.I1 (sbyte b.[o]))
                | PrimitiveType.Byte -> readPrimitive 1 offset (fun b o -> CustomAttribFixedArg.U1 b.[o])
                | PrimitiveType.Int16 ->
                    readPrimitive
                        2
                        offset
                        (fun b o ->
                            let v = uint16 b.[o] ||| (uint16 b.[o + 1] <<< 8)
                            CustomAttribFixedArg.I2 (int16 v)
                        )
                | PrimitiveType.UInt16 ->
                    readPrimitive
                        2
                        offset
                        (fun b o ->
                            let v = uint16 b.[o] ||| (uint16 b.[o + 1] <<< 8)
                            CustomAttribFixedArg.U2 v
                        )
                | PrimitiveType.Int32 ->
                    readPrimitive
                        4
                        offset
                        (fun b o ->
                            let v =
                                uint32 b.[o]
                                ||| (uint32 b.[o + 1] <<< 8)
                                ||| (uint32 b.[o + 2] <<< 16)
                                ||| (uint32 b.[o + 3] <<< 24)

                            CustomAttribFixedArg.I4 (int32 v)
                        )
                | PrimitiveType.UInt32 ->
                    readPrimitive
                        4
                        offset
                        (fun b o ->
                            let v =
                                uint32 b.[o]
                                ||| (uint32 b.[o + 1] <<< 8)
                                ||| (uint32 b.[o + 2] <<< 16)
                                ||| (uint32 b.[o + 3] <<< 24)

                            CustomAttribFixedArg.U4 v
                        )
                | PrimitiveType.Int64 ->
                    readPrimitive
                        8
                        offset
                        (fun b o ->
                            let lo =
                                uint32 b.[o]
                                ||| (uint32 b.[o + 1] <<< 8)
                                ||| (uint32 b.[o + 2] <<< 16)
                                ||| (uint32 b.[o + 3] <<< 24)

                            let hi =
                                uint32 b.[o + 4]
                                ||| (uint32 b.[o + 5] <<< 8)
                                ||| (uint32 b.[o + 6] <<< 16)
                                ||| (uint32 b.[o + 7] <<< 24)

                            CustomAttribFixedArg.I8 (int64 (uint64 lo ||| (uint64 hi <<< 32)))
                        )
                | PrimitiveType.UInt64 ->
                    readPrimitive
                        8
                        offset
                        (fun b o ->
                            let lo =
                                uint32 b.[o]
                                ||| (uint32 b.[o + 1] <<< 8)
                                ||| (uint32 b.[o + 2] <<< 16)
                                ||| (uint32 b.[o + 3] <<< 24)

                            let hi =
                                uint32 b.[o + 4]
                                ||| (uint32 b.[o + 5] <<< 8)
                                ||| (uint32 b.[o + 6] <<< 16)
                                ||| (uint32 b.[o + 7] <<< 24)

                            CustomAttribFixedArg.U8 (uint64 lo ||| (uint64 hi <<< 32))
                        )
                | PrimitiveType.Single ->
                    readPrimitive
                        4
                        offset
                        (fun b o ->
                            let bits =
                                uint32 b.[o]
                                ||| (uint32 b.[o + 1] <<< 8)
                                ||| (uint32 b.[o + 2] <<< 16)
                                ||| (uint32 b.[o + 3] <<< 24)

                            CustomAttribFixedArg.R4 (System.BitConverter.Int32BitsToSingle (int32 bits))
                        )
                | PrimitiveType.Double ->
                    readPrimitive
                        8
                        offset
                        (fun b o ->
                            let lo =
                                uint32 b.[o]
                                ||| (uint32 b.[o + 1] <<< 8)
                                ||| (uint32 b.[o + 2] <<< 16)
                                ||| (uint32 b.[o + 3] <<< 24)

                            let hi =
                                uint32 b.[o + 4]
                                ||| (uint32 b.[o + 5] <<< 8)
                                ||| (uint32 b.[o + 6] <<< 16)
                                ||| (uint32 b.[o + 7] <<< 24)

                            let bits = uint64 lo ||| (uint64 hi <<< 32)
                            CustomAttribFixedArg.R8 (System.BitConverter.Int64BitsToDouble (int64 bits))
                        )
                | PrimitiveType.String ->
                    match readSerString blob offset with
                    | Error e -> Error e
                    | Ok (s, next) -> Ok (CustomAttribFixedArg.String s, next)
                | other ->
                    Error (
                        sprintf
                            "CustomAttrib blob: TODO: primitive type %O is not yet supported as a CustomAttrib fixed-arg"
                            other
                    )
            | other ->
                Error (
                    sprintf
                        "CustomAttrib blob: TODO: non-primitive parameter type %O is not yet supported as a CustomAttrib fixed-arg (need TYPE/ENUM/SZARRAY/TAGGED_OBJECT decoders)"
                        other
                )

        let rec loop
            (remaining : TypeDefn list)
            (offset : int)
            (acc : CustomAttribFixedArg list)
            : Result<CustomAttribFixedArg list * int, string>
            =
            match remaining with
            | [] -> Ok (List.rev acc, offset)
            | head :: tail ->
                match readOne head offset with
                | Error e -> Error e
                | Ok (value, next) -> loop tail next (value :: acc)

        loop paramTypes 2 []
