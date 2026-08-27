namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// <summary>
/// One decoded fixed argument from a <c>CustomAttrib</c> blob (ECMA-335 II.23.3).
/// Each variant corresponds to a <c>CorSerializationType</c> the metadata blob can carry.
/// </summary>
/// <remarks>
/// Not all <c>CorSerializationType</c> values are represented yet:
/// <c>TYPE</c> (0x50) and <c>TAGGED_OBJECT</c> (0x51) will be added when a caller
/// needs them. The current set covers attributes whose ctors take primitives,
/// strings, enums, and SZARRAYs of those.
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
    /// SZARRAY (ECMA-335 II.23.3): <c>None</c> when <c>NumElem = 0xFFFFFFFF</c>
    /// (the null-array sentinel); <c>Some []</c> for the empty array;
    /// <c>Some [e1; e2; ...]</c> for a non-null array in declaration order.
    /// The element type is fixed by the ctor parameter and is not redundantly
    /// recorded here.
    | Array of CustomAttribFixedArg list option
    /// An enum-typed argument, carrying the decoded value of the enum's underlying type.
    ///
    /// The payload is always one of the integral variants above. The decoder reaches this case
    /// only via <c>CustomAttribArgShape.Enum</c>, whose payload is an
    /// <see cref="T:WoofWare.PawPrint.EnumUnderlyingType"/> and therefore cannot name a
    /// non-integral type; so <c>Enum (String _)</c> and <c>Enum (Enum _)</c>, while expressible,
    /// are not decodable.
    ///
    /// Which enum it was is *not* recorded: that is the caller's own
    /// <c>CustomAttribArgShape</c> input, so a caller that wants the enum's identity (to render
    /// a member name, say) zips the decoded args against the shapes it supplied.
    | Enum of underlying : CustomAttribFixedArg

/// <summary>
/// The types ECMA-335 II.14.3 admits as an enum's underlying type: "the underlying type shall be
/// a built-in integer type". This exists so that <c>CustomAttribArgShape.Enum</c> cannot name a
/// width that no enum can have — the decoder's enum arm is then total, with no error branch for
/// a case the caller could not have constructed.
/// </summary>
[<RequireQualifiedAccess>]
type EnumUnderlyingType =
    | Boolean
    | Char
    | SByte
    | Byte
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64

[<RequireQualifiedAccess>]
module EnumUnderlyingType =
    /// <c>None</c> for any primitive that cannot underlie an enum. Callers reading a real
    /// <c>value__</c> field out of metadata should fail loudly on <c>None</c>: only hand-crafted
    /// IL can produce such a type, and the CLR type loader would not admit it either.
    let ofPrimitive (primitive : PrimitiveType) : EnumUnderlyingType option =
        match primitive with
        | PrimitiveType.Boolean -> Some EnumUnderlyingType.Boolean
        | PrimitiveType.Char -> Some EnumUnderlyingType.Char
        | PrimitiveType.SByte -> Some EnumUnderlyingType.SByte
        | PrimitiveType.Byte -> Some EnumUnderlyingType.Byte
        | PrimitiveType.Int16 -> Some EnumUnderlyingType.Int16
        | PrimitiveType.UInt16 -> Some EnumUnderlyingType.UInt16
        | PrimitiveType.Int32 -> Some EnumUnderlyingType.Int32
        | PrimitiveType.UInt32 -> Some EnumUnderlyingType.UInt32
        | PrimitiveType.Int64 -> Some EnumUnderlyingType.Int64
        | PrimitiveType.UInt64 -> Some EnumUnderlyingType.UInt64
        | PrimitiveType.Single
        | PrimitiveType.Double
        | PrimitiveType.String
        | PrimitiveType.TypedReference
        | PrimitiveType.IntPtr
        | PrimitiveType.UIntPtr
        | PrimitiveType.Object -> None

    let toPrimitive (underlying : EnumUnderlyingType) : PrimitiveType =
        match underlying with
        | EnumUnderlyingType.Boolean -> PrimitiveType.Boolean
        | EnumUnderlyingType.Char -> PrimitiveType.Char
        | EnumUnderlyingType.SByte -> PrimitiveType.SByte
        | EnumUnderlyingType.Byte -> PrimitiveType.Byte
        | EnumUnderlyingType.Int16 -> PrimitiveType.Int16
        | EnumUnderlyingType.UInt16 -> PrimitiveType.UInt16
        | EnumUnderlyingType.Int32 -> PrimitiveType.Int32
        | EnumUnderlyingType.UInt32 -> PrimitiveType.UInt32
        | EnumUnderlyingType.Int64 -> PrimitiveType.Int64
        | EnumUnderlyingType.UInt64 -> PrimitiveType.UInt64

/// <summary>
/// How to decode one fixed argument from a <c>CustomAttrib</c> blob: the ctor's declared parameter
/// type, resolved to the point where the bytes can be read.
/// </summary>
/// <remarks>
/// The blob is not self-describing in its fixed-args section (ECMA-335 II.23.3): an enum argument
/// is encoded as a bare value of its underlying type, with nothing to say how wide that is. So the
/// decoder cannot work from the ctor's <c>TypeDefn</c>s alone — resolving those needs assembly
/// lookup, which the parser deliberately does not have. Callers resolve first and hand the decoder
/// this plan instead, which is decodable by construction.
///
/// <c>TYPE</c> (0x50) and <c>TAGGED_OBJECT</c> (0x51) arguments have no variant here yet; a caller
/// that meets one fails when building the plan, which is where the diagnostic belongs.
/// </remarks>
[<RequireQualifiedAccess>]
type CustomAttribArgShape =
    | Primitive of PrimitiveType
    | Enum of underlying : EnumUnderlyingType
    | SzArray of elements : CustomAttribArgShape

/// <summary>
/// One serialization type as it appears in a <c>CustomAttrib</c> blob (ECMA-335 II.23.3), with any
/// <c>SZARRAY</c> prefix already stripped: see
/// <see cref="T:WoofWare.PawPrint.CustomAttribFieldOrPropType"/>, which pairs this with that
/// prefix.
/// </summary>
/// <remarks>
/// <c>Primitive</c> over-admits: <c>IntPtr</c>, <c>Object</c> and <c>TypedReference</c> have no
/// <c>CorSerializationType</c> byte, so <c>readSerializationType</c> never produces them, and
/// <c>readElem</c> errors if one is supplied by hand.
/// </remarks>
[<RequireQualifiedAccess>]
type CustomAttribSerializationType =
    /// <c>BOOLEAN</c> (0x02) .. <c>R8</c> (0x0D), and <c>STRING</c> (0x0E).
    | Primitive of PrimitiveType
    /// <c>ENUM</c> (0x55), followed by a <c>SerString</c> naming the enum type.
    /// <c>None</c> is the SerString null sentinel, which is malformed here but is what the bytes said.
    | Enum of typeName : string option
    /// <c>TYPE</c> (0x50): an argument of type <c>System.Type</c>.
    | Type
    /// <c>TAGGED_OBJECT</c> (0x51): an <c>object</c>-typed argument, whose value carries its own
    /// <c>FieldOrPropType</c> in front of it.
    | TaggedObject

/// <summary>
/// ECMA-335 II.23.3 <c>FieldOrPropType</c>: the serialization type a *named* argument carries in
/// the blob itself, as opposed to a fixed argument's type, which comes from the constructor
/// signature.
/// </summary>
/// <remarks>
/// This is deliberately not <see cref="T:WoofWare.PawPrint.CustomAttribArgShape"/>: it is what the
/// bytes say, before any type resolution. The two differ exactly at <c>Enum</c> — the blob names
/// the enum by a reflection string, but decoding its value needs the *width* of that enum's
/// underlying type, which only assembly resolution can supply. Callers therefore resolve this into
/// a <c>CustomAttribArgShape</c> before reading the value, the same two-step the fixed-arg path
/// already uses.
///
/// An array's element is a <see cref="T:WoofWare.PawPrint.CustomAttribSerializationType"/> rather
/// than another <c>FieldOrPropType</c>, so arrays cannot nest. Both of CoreCLR's parsers read the
/// type that way — one tag, and one more tag if the first was <c>SZARRAY</c>
/// (<c>customattribute.cpp:1060-1070</c>, and <c>ParseEncodedType</c> in
/// <c>md/compiler/custattr_emit.cpp</c>, whose <c>CaType</c> is this same pair of fields). Reading
/// ECMA-335's "0x1D followed by the <c>FieldOrPropType</c> of the element type" as a recursive
/// grammar instead would admit blobs neither parser accepts, would take the member name from a
/// different offset than CoreCLR takes it from, and would put the decoder's stack depth under the
/// control of the bytes being decoded.
/// </remarks>
[<RequireQualifiedAccess>]
type CustomAttribFieldOrPropType =
    /// The argument's own serialization type: a single tag, plus an enum's name where it has one.
    | Scalar of CustomAttribSerializationType
    /// <c>SZARRAY</c> (0x1D), followed by exactly one further serialization type: the element's.
    | SzArray of elements : CustomAttribSerializationType

/// Whether a named argument sets a field (<c>0x53</c>) or a property (<c>0x54</c>).
[<RequireQualifiedAccess>]
type CustomAttribNamedArgKind =
    | Field
    | Property

/// <summary>
/// Everything a <c>NamedArg</c> (ECMA-335 II.23.3) carries *before* its value: which member kind it
/// sets, that member's serialization type, and its name.
/// </summary>
/// <remarks>
/// The value is not part of this record because it cannot always be decoded from the blob alone —
/// see <see cref="T:WoofWare.PawPrint.CustomAttribFieldOrPropType"/>. Callers resolve
/// <c>ElemType</c> to a <c>CustomAttribArgShape</c> and then call <c>readElem</c> at the offset
/// this decode returned.
/// </remarks>
type CustomAttribNamedArgHeader =
    {
        Kind : CustomAttribNamedArgKind
        ElemType : CustomAttribFieldOrPropType
        /// <c>None</c> for the <c>SerString</c> null sentinel. CoreCLR permits this and hands the
        /// caller a null name, which then throws out of <c>GetProperty(null)</c>; we reproduce the
        /// null rather than inventing a name.
        Name : string option
    }

/// <summary>
/// The three values CoreCLR's <c>CustomAttribute_ParseAttributeUsageAttribute</c> writes out for
/// one <c>[AttributeUsage]</c> application.
/// </summary>
type AttributeUsageBlob =
    {
        /// The sole fixed constructor argument, an <c>AttributeTargets</c>. CoreCLR reads these
        /// bits unsigned into a <c>ULONG*</c> and the managed caller casts the same 32 bits to
        /// <c>AttributeTargets</c>, so the signedness is never observable.
        ///
        /// Nothing in CoreLib reads the resulting <c>AttributeUsageAttribute.ValidOn</c> — a guest
        /// that wants an attribute's targets goes through the general attribute-instance decoder
        /// instead, never this parse — so no guest can observe this field. It is decoded because it
        /// is what the primitive returns, not because a caller consults it.
        ValidOn : int32
        AllowMultiple : bool
        Inherited : bool
    }

/// <summary>
/// The outcome of parsing one <c>[AttributeUsage]</c> blob.
/// </summary>
/// <remarks>
/// Three cases rather than two because CoreCLR's parser writes its out-params as it goes: it fills
/// <c>*pTargets</c> as soon as the fixed argument decodes, and only then attempts the named
/// arguments. A blob whose named arguments are malformed therefore returns <c>FALSE</c> with the
/// targets slot already overwritten and the two flag slots untouched, which a caller holding those
/// three slots can tell apart from a blob that failed earlier.
/// </remarks>
[<RequireQualifiedAccess>]
type AttributeUsageParse =
    /// The prolog or the fixed argument was malformed; nothing has been written.
    | Malformed of reason : string
    /// The fixed argument decoded, but a named argument did not; the targets are written and the
    /// two flags are not.
    | ValidOnOnly of validOn : int32 * reason : string
    /// Everything decoded; all three are written.
    | Parsed of AttributeUsageBlob

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

    /// Namespace and name of the type declaring the constructor a custom attribute invokes, or
    /// `None` when the parent provably cannot be a non-generic well-known attribute.
    ///
    /// This walk deliberately uses raw `MetadataReader` calls only: it runs while metadata is being
    /// parsed, before any cross-assembly resolution (or even this assembly's method dictionary)
    /// exists. `TypeRef` rows carry `Namespace`/`Name` directly, so no resolution is needed. The
    /// match this feeds is therefore on namespace+name strings and does not verify that the type
    /// resolves to corelib's copy of the attribute.
    ///
    /// A `TypeSpecification` parent denotes a member of a *generic instantiation* (`[MyAttr&lt;int&gt;]`,
    /// legal since C# 11). No non-generic well-known attribute can be reached that way, so `None`
    /// is a correct answer rather than the silent `("", "")` false negative the shape of this walk
    /// otherwise invites. ECMA-335 II.22.10 admits only `MethodDef` and `MemberRef` for a
    /// `CustomAttribute`'s `Type` column, so every other shape is malformed metadata and fails
    /// loudly; `describeTarget` names the entity carrying the attribute in that message.
    let constructorParentName
        (mr : MetadataReader)
        (describeTarget : unit -> string)
        (constructor : EntityHandle)
        : (string * string) option
        =
        match constructor.Kind with
        | HandleKind.MemberReference ->
            let memberRef =
                mr.GetMemberReference (MemberReferenceHandle.op_Explicit constructor)

            match memberRef.Parent.Kind with
            | HandleKind.TypeReference ->
                let typeRef = mr.GetTypeReference (TypeReferenceHandle.op_Explicit memberRef.Parent)
                Some (mr.GetString typeRef.Namespace, mr.GetString typeRef.Name)
            | HandleKind.TypeDefinition ->
                let typeDef =
                    mr.GetTypeDefinition (TypeDefinitionHandle.op_Explicit memberRef.Parent)

                Some (mr.GetString typeDef.Namespace, mr.GetString typeDef.Name)
            | HandleKind.TypeSpecification ->
                // A generic attribute instantiation; cannot be a non-generic well-known attribute.
                None
            | parentKind ->
                failwith
                    $"custom attribute on %s{describeTarget ()}: constructor MemberReference has unsupported parent kind %O{parentKind}, so we cannot identify the attribute type"
        | HandleKind.MethodDefinition ->
            let methodDef =
                mr.GetMethodDefinition (MethodDefinitionHandle.op_Explicit constructor)

            let typeDef = mr.GetTypeDefinition (methodDef.GetDeclaringType ())
            Some (mr.GetString typeDef.Namespace, mr.GetString typeDef.Name)
        | constructorKind ->
            failwith
                $"custom attribute on %s{describeTarget ()}: constructor has unsupported handle kind %O{constructorKind}, so we cannot identify the attribute type"

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
    /// The part of shape resolution that needs no type resolution at all: primitives, and SZARRAYs
    /// whose element type is itself resolvable this way. <c>None</c> for any parameter whose shape
    /// depends on identifying a named type — an enum, a <c>System.Type</c>, an <c>object</c>.
    /// </summary>
    /// <remarks>
    /// Callers that can resolve types use this first and fall back to resolution only on
    /// <c>None</c>. Callers that cannot resolve types (the IL dumper) treat <c>None</c> as
    /// "cannot decode this blob".
    /// </remarks>
    let rec tryShapeWithoutResolution (paramType : TypeDefn) : CustomAttribArgShape option =
        match paramType with
        | TypeDefn.PrimitiveType pt -> Some (CustomAttribArgShape.Primitive pt)
        | TypeDefn.OneDimensionalArrayLowerBoundZero elt ->
            tryShapeWithoutResolution elt |> Option.map CustomAttribArgShape.SzArray
        | _ -> None

    /// <summary>
    /// Decode a single <c>Elem</c> (ECMA-335 II.23.3) at <paramref name="offset"/>, returning the
    /// value and the offset of the first byte after it.
    /// </summary>
    /// <remarks>
    /// Fixed args and named args encode their values identically — both are <c>Elem</c> — so this
    /// serves both. What differs is only where the <see cref="T:WoofWare.PawPrint.CustomAttribArgShape"/>
    /// comes from: a fixed arg takes it from the ctor signature, a named arg from the
    /// <c>FieldOrPropType</c> in the blob. Mirrors CoreCLR's <c>GetDataFromBlob</c>, which the
    /// native side likewise shares between the two.
    /// </remarks>
    let rec readElem
        (shape : CustomAttribArgShape)
        (blob : ImmutableArray<byte>)
        (offset : int)
        : Result<CustomAttribFixedArg * int, string>
        =
        let len = blob.Length

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

        let readUInt32 (offset : int) : Result<uint32 * int, string> =
            if offset + 4 > len then
                Error (sprintf "CustomAttrib blob: uint32 at offset %d would overrun blob length %d" offset len)
            else
                let v =
                    uint32 blob.[offset]
                    ||| (uint32 blob.[offset + 1] <<< 8)
                    ||| (uint32 blob.[offset + 2] <<< 16)
                    ||| (uint32 blob.[offset + 3] <<< 24)

                Ok (v, offset + 4)

        let readPrimitiveValue (pt : PrimitiveType) (offset : int) : Result<CustomAttribFixedArg * int, string> =
            match pt with
            // Divergence: CoreCLR boxes the raw blob byte, so a hand-crafted blob can produce a
            // `System.Boolean` holding 2, whereas this normalises to an F# `bool` and hence to 0/1.
            // Only hand-written IL can produce such a blob.
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
                        "CustomAttrib blob: TODO: primitive type %O is not yet supported as a CustomAttrib Elem"
                        other
                )

        match shape with
        | CustomAttribArgShape.Primitive pt -> readPrimitiveValue pt offset
        | CustomAttribArgShape.Enum underlying ->
            // ECMA-335 II.23.3: "if the parameter kind is an enum, ... the value is stored
            // using the underlying type of the enum". There is no tag; the width comes
            // entirely from `underlying`, which is why the caller has to resolve it.
            match readPrimitiveValue (EnumUnderlyingType.toPrimitive underlying) offset with
            | Error e -> Error e
            | Ok (value, next) -> Ok (CustomAttribFixedArg.Enum value, next)
        | CustomAttribArgShape.SzArray eltShape ->
            match readUInt32 offset with
            | Error e -> Error e
            | Ok (0xFFFFFFFFu, next) -> Ok (CustomAttribFixedArg.Array None, next)
            | Ok (numElem, next) ->
                let rec readElems
                    (remaining : int)
                    (cursor : int)
                    (acc : CustomAttribFixedArg list)
                    : Result<CustomAttribFixedArg list * int, string>
                    =
                    if remaining = 0 then
                        Ok (List.rev acc, cursor)
                    else
                        match readElem eltShape blob cursor with
                        | Error e -> Error e
                        | Ok (value, after) -> readElems (remaining - 1) after (value :: acc)

                match readElems (int numElem) next [] with
                | Error e -> Error e
                | Ok (elts, after) -> Ok (CustomAttribFixedArg.Array (Some elts), after)

    /// <summary>
    /// Decode the fixed-args section of a <c>CustomAttrib</c> blob (ECMA-335 II.23.3).
    /// The blob must start with the two-byte prolog <c>0x0001</c>, followed by one
    /// fixed-arg value for each entry in <paramref name="paramShapes"/> (in declared order),
    /// encoded per ECMA-335 II.23.3 / <c>CorSerializationType</c>.
    /// </summary>
    /// <param name="paramShapes">
    /// The constructor's parameter types in declaration order, already resolved to
    /// <see cref="T:WoofWare.PawPrint.CustomAttribArgShape"/>. See that type for why the raw
    /// <c>TypeDefn</c>s are not enough.
    /// </param>
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
        (paramShapes : CustomAttribArgShape list)
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

        let rec loop
            (remaining : CustomAttribArgShape list)
            (offset : int)
            (acc : CustomAttribFixedArg list)
            : Result<CustomAttribFixedArg list * int, string>
            =
            match remaining with
            | [] -> Ok (List.rev acc, offset)
            | head :: tail ->
                match readElem head blob offset with
                | Error e -> Error e
                | Ok (value, next) -> loop tail next (value :: acc)

        loop paramShapes 2 []

    /// <summary>
    /// Read one <c>CorSerializationType</c> at <paramref name="offset"/> — the tag byte, plus an
    /// enum's type name where it has one — returning it and the offset of the first byte after it.
    /// <c>SZARRAY</c> is not one of these; see <c>readFieldOrPropType</c>.
    /// </summary>
    /// <remarks>
    /// Total over the tags a named argument's type can take, including the ones no caller can yet
    /// lower to a runtime value. The partiality lives in the resolution step instead, which is also
    /// where it lives for fixed args. Byte values are <c>CorSerializationType</c>
    /// (<c>corhdr.h</c>), which aliases <c>CorElementType</c> for <c>BOOLEAN</c>..<c>R8</c> and
    /// <c>STRING</c>.
    /// </remarks>
    let readSerializationType
        (blob : ImmutableArray<byte>)
        (offset : int)
        : Result<CustomAttribSerializationType * int, string>
        =
        if offset >= blob.Length then
            Error (
                sprintf
                    "CustomAttrib blob: a serialization type begins at offset %d but blob has only %d bytes"
                    offset
                    blob.Length
            )
        else

        let primitive (pt : PrimitiveType) =
            Ok (CustomAttribSerializationType.Primitive pt, offset + 1)

        match blob.[offset] with
        | 0x02uy -> primitive PrimitiveType.Boolean
        | 0x03uy -> primitive PrimitiveType.Char
        | 0x04uy -> primitive PrimitiveType.SByte
        | 0x05uy -> primitive PrimitiveType.Byte
        | 0x06uy -> primitive PrimitiveType.Int16
        | 0x07uy -> primitive PrimitiveType.UInt16
        | 0x08uy -> primitive PrimitiveType.Int32
        | 0x09uy -> primitive PrimitiveType.UInt32
        | 0x0Auy -> primitive PrimitiveType.Int64
        | 0x0Buy -> primitive PrimitiveType.UInt64
        | 0x0Cuy -> primitive PrimitiveType.Single
        | 0x0Duy -> primitive PrimitiveType.Double
        | 0x0Euy -> primitive PrimitiveType.String
        | 0x50uy -> Ok (CustomAttribSerializationType.Type, offset + 1)
        | 0x51uy -> Ok (CustomAttribSerializationType.TaggedObject, offset + 1)
        | 0x55uy ->
            match readSerString blob (offset + 1) with
            | Error e -> Error e
            | Ok (typeName, next) -> Ok (CustomAttribSerializationType.Enum typeName, next)
        | other ->
            Error (
                sprintf
                    "CustomAttrib blob: byte 0x%02X at offset %d is not a valid serialization type (ECMA-335 II.23.3)"
                    other
                    offset
            )

    /// <summary>
    /// Read a <c>FieldOrPropType</c> (ECMA-335 II.23.3) at <paramref name="offset"/>, returning it
    /// and the offset of the first byte after it.
    /// </summary>
    /// <remarks>
    /// A leading <c>SZARRAY</c> (0x1D) consumes exactly one further serialization type, so this
    /// reads at most two tag bytes and never calls itself: see
    /// <see cref="T:WoofWare.PawPrint.CustomAttribFieldOrPropType"/> for why the grammar is that
    /// shape rather than a recursive one. A second <c>SZARRAY</c> is therefore rejected here, at the
    /// offset it appears at.
    /// </remarks>
    let readFieldOrPropType
        (blob : ImmutableArray<byte>)
        (offset : int)
        : Result<CustomAttribFieldOrPropType * int, string>
        =
        if offset >= blob.Length then
            Error (
                sprintf
                    "CustomAttrib blob: FieldOrPropType begins at offset %d but blob has only %d bytes"
                    offset
                    blob.Length
            )
        elif blob.[offset] = 0x1Duy then
            match readSerializationType blob (offset + 1) with
            | Error e -> Error e
            | Ok (elt, next) -> Ok (CustomAttribFieldOrPropType.SzArray elt, next)
        else

        match readSerializationType blob offset with
        | Error e -> Error e
        | Ok (elt, next) -> Ok (CustomAttribFieldOrPropType.Scalar elt, next)

    /// <summary>
    /// Read the leading part of a <c>NamedArg</c> (ECMA-335 II.23.3) at <paramref name="offset"/>:
    /// its field/property tag, its <c>FieldOrPropType</c>, and its member name. Returns the header
    /// and the offset of its value, which the caller decodes with <c>readElem</c> once it has
    /// resolved the type.
    /// </summary>
    /// <remarks>
    /// Field order mirrors CoreCLR's <c>CustomAttribute_CreatePropertyOrFieldData</c>
    /// (<c>customattribute.cpp:1050-1096</c>): the type — <em>including an enum's type name</em> —
    /// comes before the member name.
    /// </remarks>
    let readNamedArgHeader
        (blob : ImmutableArray<byte>)
        (offset : int)
        : Result<CustomAttribNamedArgHeader * int, string>
        =
        if offset >= blob.Length then
            Error (
                sprintf "CustomAttrib blob: NamedArg begins at offset %d but blob has only %d bytes" offset blob.Length
            )
        else

        let kindResult =
            match blob.[offset] with
            | 0x53uy -> Ok CustomAttribNamedArgKind.Field
            | 0x54uy -> Ok CustomAttribNamedArgKind.Property
            | other ->
                Error (
                    sprintf
                        "CustomAttrib blob: byte 0x%02X at offset %d is neither FIELD (0x53) nor PROPERTY (0x54)"
                        other
                        offset
                )

        match kindResult with
        | Error e -> Error e
        | Ok kind ->

        match readFieldOrPropType blob (offset + 1) with
        | Error e -> Error e
        | Ok (elemType, afterType) ->

        match readSerString blob afterType with
        | Error e -> Error e
        | Ok (name, afterName) ->

        let header =
            {
                Kind = kind
                ElemType = elemType
                Name = name
            }

        Ok (header, afterName)

    /// <summary>
    /// Decode a <c>CustomAttrib</c> blob as an application of <c>[AttributeUsage]</c>: one
    /// <c>AttributeTargets</c> fixed argument, plus the optional <c>AllowMultiple</c> and
    /// <c>Inherited</c> named arguments. Anything but <c>Parsed</c> is an input on which CoreCLR's
    /// parser returns <c>FALSE</c>, which its managed caller turns into a
    /// <c>CustomAttributeFormatException</c>; the two failure cases differ in how many of the
    /// parser's out-params it had already written, which
    /// <see cref="T:WoofWare.PawPrint.AttributeUsageParse"/> explains.
    /// </summary>
    /// <remarks>
    /// The contract here is "what CoreCLR's parser does", not "what ECMA-335 II.23.3 says" — this
    /// exists to be that primitive, so where the two disagree this follows the parser. The
    /// divergences from the grammar are marked at the code that makes them.
    ///
    /// The parse is <c>::ParseKnownCaArgs</c> and <c>::ParseKnownCaNamedArgs</c>
    /// (<c>md/compiler/custattr_emit.cpp</c>), reached from
    /// <c>CustomAttribute_ParseAttributeUsageAttribute</c> (<c>vm/customattribute.cpp</c>). Note
    /// that <c>customattribute.cpp</c> also defines a VM-local <c>ParseCaNamedArgs</c> which is
    /// *not* what this QCall uses and which behaves differently; the <c>::</c> qualification at the
    /// call site is what distinguishes them.
    ///
    /// The diagnostic on the two failure cases has no counterpart in CoreCLR, whose <c>BOOL</c> discards
    /// which rule fired. The native handler discards it too, having only <c>FALSE</c> to report,
    /// but it lets a test pin *why* a blob was rejected rather than merely that it was.
    /// </remarks>
    let parseAttributeUsage (blob : ImmutableArray<byte>) : AttributeUsageParse =
        // `args[0].InitEnum(SERIALIZATION_TYPE_I4)`: the AttributeTargets argument is an enum whose
        // width the parser hardcodes rather than resolving, so the blob's 4 bytes are read directly.
        // `readFixedArgs` performs the 0x0001 prolog check that `ValidateProlog` does.
        match readFixedArgs [ CustomAttribArgShape.Enum EnumUnderlyingType.Int32 ] blob with
        | Error e -> AttributeUsageParse.Malformed e
        | Ok (fixedArgs, afterFixed) ->

        let validOn =
            match fixedArgs with
            | [ CustomAttribFixedArg.Enum (CustomAttribFixedArg.I4 v) ] -> v
            | other ->
                // `readElem` on `Enum Int32` can only produce `Enum (I4 _)`, so this is a logic
                // error in the decoder rather than anything the blob could have said.
                failwith
                    $"logic error: reading one Int32-underlain enum fixed arg produced %A{other} rather than a single Enum(I4 _)"

        // The named-arg count is read with `GetI2`, and a *failed* read yields 0 rather than an
        // error (custattr_emit.cpp, commented "Everett behavior"). `GetI2` fails whenever fewer
        // than two bytes remain, so a blob that ends after the fixed arg and one that has a single
        // stray byte left both land here with "no named arguments".
        let remaining = blob.Length - afterFixed

        let namedCount, afterCount =
            if remaining < 2 then
                0s, afterFixed
            else
                let count =
                    int16 (uint16 blob.[afterFixed] ||| (uint16 blob.[afterFixed + 1] <<< 8))

                count, afterFixed + 2

        // The count is a *signed* int16 compared against a widened int32 loop counter, so a count
        // with its high bit set runs no iterations at all and the parse succeeds with both
        // defaults. Mirrored rather than refused: this is the primitive's behaviour, and real .NET
        // accepts such a blob.
        let rec loop
            (remainingArgs : int)
            (cursor : int)
            (allowMultiple : bool option)
            (inherited : bool option)
            : AttributeUsageParse
            =
            if remainingArgs <= 0 then
                // Nothing checks that the blob was fully consumed, so trailing bytes are ignored.
                AttributeUsageParse.Parsed
                    {
                        ValidOn = validOn
                        // The descriptor table's starting values, which stand for any argument the
                        // blob did not name.
                        AllowMultiple = Option.defaultValue false allowMultiple
                        Inherited = Option.defaultValue true inherited
                    }
            else

            // The FIELD (0x53) / PROPERTY (0x54) tag, then the serialization type, then the name,
            // in that order — the order `ParseKnownCaNamedArgs` reads them, so a blob that is
            // malformed in more than one way is rejected for the same reason CoreCLR rejects it.
            // No tag other than those two is admitted, and `ParseEncodedType` validates nothing
            // beyond that, so an unrecognised *type* tag is not refused here: it is carried to
            // matching and refused there, as an argument naming no known descriptor.
            let kindResult =
                if cursor >= blob.Length then
                    Error (
                        sprintf
                            "CustomAttrib blob: a named arg was expected at offset %d but the blob has only %d bytes"
                            cursor
                            blob.Length
                    )
                elif blob.[cursor] = 0x53uy || blob.[cursor] = 0x54uy then
                    Ok ()
                else
                    Error (
                        sprintf
                            "CustomAttrib blob: byte 0x%02X at offset %d is neither FIELD (0x53) nor PROPERTY (0x54)"
                            blob.[cursor]
                            cursor
                    )

            match kindResult with
            | Error e -> AttributeUsageParse.ValidOnOnly (validOn, e)
            | Ok () ->

            // `ParseEncodedType` reads the argument's whole serialization type here — a further tag
            // if this one is SZARRAY, an enum's name if the result is ENUM. None of that is
            // observable to *this* parse: both descriptors `[AttributeUsage]` declares are BOOLEAN,
            // so an argument whose tag is anything else matches neither and rejects the blob, and
            // it does so whether CoreCLR rejected it while decoding the type (a truncated element
            // tag, a null enum name) or afterwards while matching. One tag byte therefore decides
            // it, and what follows a non-BOOLEAN one is never read.
            let typeTagResult =
                if cursor + 1 >= blob.Length then
                    Error (
                        sprintf
                            "CustomAttrib blob: a serialization type tag was expected at offset %d but the blob has only %d bytes"
                            (cursor + 1)
                            blob.Length
                    )
                elif blob.[cursor + 1] = 0x02uy then
                    Ok ()
                else
                    Error (
                        sprintf
                            "CustomAttrib blob: named arg at offset %d has serialization type 0x%02X, and [AttributeUsage] declares only BOOLEAN arguments"
                            cursor
                            blob.[cursor + 1]
                    )

            match typeTagResult with
            | Error e -> AttributeUsageParse.ValidOnOnly (validOn, e)
            | Ok () ->

            match readSerString blob (cursor + 2) with
            | Error e -> AttributeUsageParse.ValidOnOnly (validOn, e)
            | Ok (declaredName, valueOffset) ->

            // `GetNonEmptyString` rejects the null sentinel and the empty string alike.
            match declaredName with
            | None ->
                AttributeUsageParse.ValidOnOnly (
                    validOn,
                    $"CustomAttrib blob: named arg at offset %d{cursor} has the null name sentinel"
                )
            | Some "" ->
                AttributeUsageParse.ValidOnOnly (
                    validOn,
                    $"CustomAttrib blob: named arg at offset %d{cursor} has an empty name"
                )
            | Some name ->

            // Matching compares the serialization type and the name, and *not* whether the blob
            // said FIELD or PROPERTY — so a field named `AllowMultiple` sets the property. Mirrored
            // for the same reason as the signed count above.
            let matched =
                match name with
                | "AllowMultiple" -> Some (true, allowMultiple)
                | "Inherited" -> Some (false, inherited)
                | _ -> None

            match matched with
            | None ->
                AttributeUsageParse.ValidOnOnly (
                    validOn,
                    $"CustomAttrib blob: named arg '%s{name}' at offset %d{cursor} matches no argument of [AttributeUsage]"
                )
            | Some (_, Some _) ->
                AttributeUsageParse.ValidOnOnly (
                    validOn,
                    $"CustomAttrib blob: named arg '%s{name}' at offset %d{cursor} appears more than once"
                )
            | Some (isAllowMultiple, None) ->

            match readElem (CustomAttribArgShape.Primitive PrimitiveType.Boolean) blob valueOffset with
            | Error e -> AttributeUsageParse.ValidOnOnly (validOn, e)
            | Ok (CustomAttribFixedArg.Bool value, next) ->
                if isAllowMultiple then
                    loop (remainingArgs - 1) next (Some value) inherited
                else
                    loop (remainingArgs - 1) next allowMultiple (Some value)
            | Ok (other, _) ->
                failwith $"logic error: reading a Boolean-shaped named arg value produced %A{other} rather than a Bool"

        loop (int namedCount) afterCount None None
