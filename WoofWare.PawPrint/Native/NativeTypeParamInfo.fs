namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Where a length-prefixed string sits inside a MarshalSpec blob: the offset of its first byte
/// (just past its ECMA-335 II.23.2 length prefix) and its length in bytes, both relative to the
/// start of the blob.
type MarshalSpecString =
    {
        Offset : int
        Length : int
    }

/// What CoreCLR's `ParseNativeTypeInfo` (mlinfo.cpp) extracts from a MarshalSpec blob (ECMA-335
/// II.23.4), as the `MetaDataImport::GetMarshalAs` FCall (managedmdimport.cpp) sees it: starting
/// from a zeroed `NativeTypeParamInfo` rather than from that struct's constructor defaults, on a
/// build without `FEATURE_COMINTEROP`. That is every non-Windows CoreCLR, and so the platform
/// PawPrint emulates; on such a build the COM shapes (`NATIVE_TYPE_INTF`, `IUNKNOWN`, `IDISPATCH`,
/// `SAFEARRAY`) carry nothing beyond their leading byte, and the FCall reports
/// `SafeArraySubType = VT_EMPTY`, a null `SafeArrayUserDefinedSubType` and `IidParameterIndex = 0`.
type NativeTypeParamInfo =
    {
        /// The blob's leading `NATIVE_TYPE_*` byte; `MarshalAsAttribute.Value`.
        NativeType : byte
        /// `MarshalAsAttribute.ArraySubType`. Zero unless the blob is `NATIVE_TYPE_FIXEDARRAY` or
        /// `NATIVE_TYPE_ARRAY` and carries an element type.
        ArrayElementType : uint32
        /// `MarshalAsAttribute.SizeParamIndex`. CoreCLR stores it in a `UINT16`, so a larger
        /// compressed integer in the blob arrives here truncated to its low 16 bits.
        CountParamIndex : uint16
        /// `MarshalAsAttribute.SizeConst`: the `NATIVE_TYPE_FIXEDSYSSTRING` / `NATIVE_TYPE_FIXEDARRAY`
        /// size, or the `NATIVE_TYPE_ARRAY` additive.
        Additive : uint32
        /// `NATIVE_TYPE_CUSTOMMARSHALER`'s marshaler type name, and `None` for every other shape.
        MarshalerTypeName : MarshalSpecString option
        /// `NATIVE_TYPE_CUSTOMMARSHALER`'s cookie, and `None` for every other shape.
        Cookie : MarshalSpecString option
    }

[<RequireQualifiedAccess>]
module NativeTypeParamInfo =
    [<Literal>]
    let NativeTypeFixedSysString = 0x17uy

    [<Literal>]
    let NativeTypeFixedArray = 0x1Euy

    [<Literal>]
    let NativeTypeArray = 0x2Auy

    [<Literal>]
    let NativeTypeCustomMarshaler = 0x2Cuy

    /// `CheckForCompressedData` (mlinfo.cpp): S_FALSE, S_OK or a failure HRESULT.
    [<RequireQualifiedAccess>]
    type private CompressedData =
        | AtEnd
        /// A well-formed compressed integer of this many bytes starts here.
        | Present of width : int
        | Corrupt

    /// `CPackedLen::SafeGetLength` (stgpooli.cpp) at `offset`, bounded by the end of the blob. The
    /// lead byte announces a 1-, 2- or 4-byte integer; one announcing more bytes than remain, or
    /// with its top three bits all set, is refused.
    let private compressedWidth (blob : ImmutableArray<byte>) (offset : int) : int option =
        let available = blob.Length - offset

        if available < 1 then
            None
        else

        let lead = blob.[offset]

        if lead &&& 0x80uy = 0uy then Some 1
        elif available < 2 then None
        elif lead &&& 0xC0uy = 0x80uy then Some 2
        elif available < 4 then None
        elif lead &&& 0xE0uy = 0xC0uy then Some 4
        else None

    let private checkForCompressedData (blob : ImmutableArray<byte>) (offset : int) : CompressedData =
        if offset = blob.Length then
            CompressedData.AtEnd
        else
            match compressedWidth blob offset with
            | Some width -> CompressedData.Present width
            | None -> CompressedData.Corrupt

    /// `CorSigUncompressData` (cor.h) of an integer `checkForCompressedData` has already vetted.
    let private uncompress (blob : ImmutableArray<byte>) (offset : int) (width : int) : uint32 =
        match width with
        | 1 -> uint32 blob.[offset]
        | 2 -> ((uint32 blob.[offset] &&& 0x3Fu) <<< 8) ||| uint32 blob.[offset + 1]
        | 4 ->
            ((uint32 blob.[offset] &&& 0x1Fu) <<< 24)
            ||| (uint32 blob.[offset + 1] <<< 16)
            ||| (uint32 blob.[offset + 2] <<< 8)
            ||| uint32 blob.[offset + 3]
        | _ -> failwith $"NativeTypeParamInfo: compressed integer width %d{width} is not 1, 2 or 4"

    /// One `CheckForCompressedData` + `CPackedLen::SafeGetData` step of the
    /// `NATIVE_TYPE_CUSTOMMARSHALER` arm: the string starting at `offset`, if its length prefix is
    /// well-formed and its bytes fit before the end of the blob. `CheckForCompressedData`'s own
    /// failure and `SafeGetData`'s are both `return FALSE` there, so they are one `None` here.
    let private packedString (blob : ImmutableArray<byte>) (offset : int) : MarshalSpecString option =
        match checkForCompressedData blob offset with
        | CompressedData.AtEnd
        | CompressedData.Corrupt -> None
        | CompressedData.Present width ->
            let length = uncompress blob offset width
            let dataOffset = offset + width

            // `SafeGetData` compares in pointer width, so a length near 2^29 cannot wrap; neither
            // can it here, in 64 bits.
            if int64 dataOffset + int64 length > int64 blob.Length then
                None
            else
                Some
                    {
                        Offset = dataOffset
                        Length = int length
                    }

    let private zero (nativeType : byte) : NativeTypeParamInfo =
        {
            NativeType = nativeType
            ArrayElementType = 0u
            CountParamIndex = 0us
            Additive = 0u
            MarshalerTypeName = None
            Cookie = None
        }

    /// CoreCLR's `ParseNativeTypeInfo` over one MarshalSpec blob, or `None` where it returns
    /// `FALSE` — which the managed `MetadataImport.GetMarshalAs` turns into a
    /// `BadImageFormatException`.
    ///
    /// The quirks are CoreCLR's, reproduced deliberately: an empty blob fails; trailing bytes after
    /// the last item a shape reads are ignored; a `NATIVE_TYPE_FIXEDARRAY` whose element type is
    /// malformed *succeeds*, reporting element type 0; and a `NATIVE_TYPE_CUSTOMMARSHALER` fails
    /// unless all four of its strings are present and in bounds.
    let parse (blob : ImmutableArray<byte>) : NativeTypeParamInfo option =
        if blob.Length = 0 then
            None
        else

        let nativeType = blob.[0]
        let info = zero nativeType

        match nativeType with
        | NativeTypeFixedArray ->
            match checkForCompressedData blob 1 with
            | CompressedData.AtEnd
            | CompressedData.Corrupt -> None
            | CompressedData.Present width ->
                let info =
                    { info with
                        Additive = uncompress blob 1 width
                    }

                let offset = 1 + width

                match checkForCompressedData blob offset with
                // mlinfo.cpp's `return TRUE` on this failure, where every other arm returns FALSE.
                | CompressedData.AtEnd
                | CompressedData.Corrupt -> Some info
                | CompressedData.Present width ->
                    Some
                        { info with
                            ArrayElementType = uncompress blob offset width
                        }
        | NativeTypeFixedSysString ->
            match checkForCompressedData blob 1 with
            | CompressedData.AtEnd
            | CompressedData.Corrupt -> None
            | CompressedData.Present width ->
                Some
                    { info with
                        Additive = uncompress blob 1 width
                    }
        | NativeTypeArray ->
            // Each item is optional, but only as a suffix: an absent item leaves the cursor where
            // it was, so every later check reports the same absence. Only a malformed item fails.
            // The fourth item is a flags word, which changes only `m_Multiplier`, and the FCall
            // does not report that.
            let rec items (offset : int) (remaining : int) (acc : uint32 list) : uint32 list option =
                if remaining = 0 then
                    Some (List.rev acc)
                else
                    match checkForCompressedData blob offset with
                    | CompressedData.Corrupt -> None
                    | CompressedData.AtEnd -> Some (List.rev acc)
                    | CompressedData.Present width ->
                        items (offset + width) (remaining - 1) (uncompress blob offset width :: acc)

            match items 1 4 [] with
            | None -> None
            | Some values ->
                let item (index : int) : uint32 option = List.tryItem index values

                Some
                    { info with
                        ArrayElementType = item 0 |> Option.defaultValue 0u
                        CountParamIndex =
                            item 1
                            |> Option.map (fun index -> uint16 (index &&& 0xFFFFu))
                            |> Option.defaultValue 0us
                        Additive = item 2 |> Option.defaultValue 0u
                    }
        | NativeTypeCustomMarshaler ->
            // The typelib GUID and the native type name are skipped; the marshaler type name and
            // the cookie are reported.
            packedString blob 1
            |> Option.bind (fun guid -> packedString blob (guid.Offset + guid.Length))
            |> Option.bind (fun nativeTypeName -> packedString blob (nativeTypeName.Offset + nativeTypeName.Length))
            |> Option.bind (fun marshalerTypeName ->
                packedString blob (marshalerTypeName.Offset + marshalerTypeName.Length)
                |> Option.map (fun cookie ->
                    { info with
                        MarshalerTypeName = Some marshalerTypeName
                        Cookie = Some cookie
                    }
                )
            )
        | _ -> Some info
