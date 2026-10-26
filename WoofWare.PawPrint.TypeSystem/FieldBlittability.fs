namespace WoofWare.PawPrint

/// CoreCLR's per-field blittability rules, after the field's type has been classified.
/// Type resolution, enum validation and computing a nested value type's facts belong to the caller.
[<RequireQualifiedAccess>]
module internal FieldBlittability =

    /// The field's internal element type, carrying the caller's facts for a
    /// non-primitive value type. Enum fields use their validated underlying primitive; this
    /// classification does not establish that a type is a valid enum backing field.
    [<RequireQualifiedAccess>]
    type Element<'nestedFacts> =
        | Boolean
        | Char
        | OneByte
        | TwoBytes
        | FourBytes
        | EightBytes
        | Single
        | Double
        | NativeInteger
        | Void
        | TypedReference
        | Pointer
        | FunctionPointer
        | Byref
        /// An object reference: a class, an interface, an array, `string` or `object`.
        | Reference
        /// Whether the type is CoreLib's `System.Decimal`, and the caller's nested-type facts.
        | ValueType of isDecimal : bool * facts : 'nestedFacts

    /// Transform the facts of a non-primitive value type, preserving the field's classification.
    let mapValueType (map : 'a -> 'b) (element : Element<'a>) : Element<'b> =
        match element with
        | Element.Boolean -> Element.Boolean
        | Element.Char -> Element.Char
        | Element.OneByte -> Element.OneByte
        | Element.TwoBytes -> Element.TwoBytes
        | Element.FourBytes -> Element.FourBytes
        | Element.EightBytes -> Element.EightBytes
        | Element.Single -> Element.Single
        | Element.Double -> Element.Double
        | Element.NativeInteger -> Element.NativeInteger
        | Element.Void -> Element.Void
        | Element.TypedReference -> Element.TypedReference
        | Element.Pointer -> Element.Pointer
        | Element.FunctionPointer -> Element.FunctionPointer
        | Element.Byref -> Element.Byref
        | Element.Reference -> Element.Reference
        | Element.ValueType (isDecimal, facts) -> Element.ValueType (isDecimal, map facts)

    // `NATIVE_TYPE_*` (corhdr.h): the leading byte of a field's marshalling blob.
    [<Literal>]
    let private NativeTypeI1 = 0x03uy

    [<Literal>]
    let private NativeTypeU1 = 0x04uy

    [<Literal>]
    let private NativeTypeI2 = 0x05uy

    [<Literal>]
    let private NativeTypeU2 = 0x06uy

    [<Literal>]
    let private NativeTypeI4 = 0x07uy

    [<Literal>]
    let private NativeTypeU4 = 0x08uy

    [<Literal>]
    let private NativeTypeI8 = 0x09uy

    [<Literal>]
    let private NativeTypeU8 = 0x0Auy

    [<Literal>]
    let private NativeTypeR4 = 0x0Buy

    [<Literal>]
    let private NativeTypeR8 = 0x0Cuy

    [<Literal>]
    let private NativeTypeStruct = 0x1Buy

    [<Literal>]
    let private NativeTypeInt = 0x1Fuy

    [<Literal>]
    let private NativeTypeUInt = 0x20uy

    [<Literal>]
    let private NativeTypeFunc = 0x26uy

    [<Literal>]
    let private NativeTypeError = 0x2Duy

    /// The leading byte of a field's marshalling blob, which is all `IsFieldBlittable` reads:
    /// `NATIVE_TYPE_DEFAULT` for no blob, and for an empty one.
    let private leadingNativeType (descriptor : FieldMarshalDescriptor option) : byte =
        match descriptor with
        | None
        | Some FieldMarshalDescriptor.Empty -> NativeTypeParamInfo.NativeTypeDefault
        | Some (FieldMarshalDescriptor.ByValTStr _) -> NativeTypeParamInfo.NativeTypeFixedSysString
        | Some (FieldMarshalDescriptor.ByValArray _) -> NativeTypeParamInfo.NativeTypeFixedArray
        | Some (FieldMarshalDescriptor.Other unmanagedType) -> byte unmanagedType
        | Some (FieldMarshalDescriptor.Malformed nativeType) -> nativeType

    /// CoreCLR's `IsFieldBlittable` (fieldmarshaler.cpp) for an already classified field. `ansi`
    /// is the declaring type's charset: true for ANSI and, on Unix, Auto; false for Unicode.
    /// Only the leading byte of `descriptor` matters; an empty blob means no descriptor.
    /// A non-primitive value type carries its `MethodTable::IsBlittable` as the payload.
    let isBlittable (ansi : bool) (descriptor : FieldMarshalDescriptor option) (element : Element<bool>) : bool =
        let nativeType = leadingNativeType descriptor
        let isDefault = nativeType = NativeTypeParamInfo.NativeTypeDefault

        match element with
        | Element.Char ->
            (isDefault && not ansi)
            || nativeType = NativeTypeI2
            || nativeType = NativeTypeU2
        | Element.OneByte -> isDefault || nativeType = NativeTypeI1 || nativeType = NativeTypeU1
        | Element.TwoBytes -> isDefault || nativeType = NativeTypeI2 || nativeType = NativeTypeU2
        | Element.FourBytes ->
            isDefault
            || nativeType = NativeTypeI4
            || nativeType = NativeTypeU4
            || nativeType = NativeTypeError
        | Element.EightBytes -> isDefault || nativeType = NativeTypeI8 || nativeType = NativeTypeU8
        | Element.Single -> isDefault || nativeType = NativeTypeR4
        | Element.Double -> isDefault || nativeType = NativeTypeR8
        | Element.NativeInteger -> isDefault || nativeType = NativeTypeInt || nativeType = NativeTypeUInt
        | Element.Pointer -> isDefault
        | Element.FunctionPointer -> isDefault || nativeType = NativeTypeFunc
        | Element.ValueType (isDecimal, isBlittable) ->
            (isDefault || nativeType = NativeTypeStruct) && not isDecimal && isBlittable
        | Element.Boolean
        | Element.Void
        | Element.TypedReference
        | Element.Byref
        | Element.Reference -> false
