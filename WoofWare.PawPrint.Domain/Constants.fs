namespace WoofWare.PawPrint

[<AutoOpen>]
module Constants =

    [<Literal>]
    let SIZEOF_INT = 4

    [<Literal>]
    let SIZEOF_OBJ = 8

    /// The ceiling a type's `[StructLayout(Pack = ...)]` puts on each field's alignment, when the
    /// type requests no packing of its own. CoreCLR's `DEFAULT_PACKING_SIZE` (fieldmarshaler.h:27),
    /// used both when the `ClassLayout` table has no row for the type and when its `Pack` is 0
    /// (methodtablebuilder.cpp:12590, classlayoutinfo.cpp:904).
    ///
    /// This is a *ceiling*, not an alignment — the name it replaced (`DEFAULT_STRUCT_ALIGNMENT`,
    /// value 8) read as though it were the latter, and the two coincided only because no PawPrint
    /// type demanded more than pointer alignment. `Int128` demands 16, so a ceiling of 8 would
    /// silently cap it back down to 8 and undo the nominal stamp
    /// (`DeclaredTypeFacts.nominalAlignment`).
    [<Literal>]
    let DEFAULT_PACKING_SIZE = 64

    [<Literal>]
    let NATIVE_INT_SIZE = 8

    /// Base-2 log of `NATIVE_INT_SIZE`. CoreCLR's auto layout buckets fields by size class,
    /// indexing arrays by the base-2 log of the field size; this is the largest such index,
    /// CoreCLR's `MAX_LOG2_PRIMITIVE_FIELD_SIZE` (class.h:56) and `LOG2SLOT`.
    [<Literal>]
    let LOG2_NATIVE_INT_SIZE = 3

    /// The largest byte offset a real instance field may have, and hence the largest instance size
    /// a value type may reach. CoreCLR's `FIELD_OFFSET_LAST_REAL_OFFSET` (field.h:27), which is
    /// `FIELD_OFFSET_MAX - 7` for the seven sentinel offsets reserved above it, with
    /// `FIELD_OFFSET_MAX = (1 <<< 27) - 1` (field.h:16). Exceeding it is a `TypeLoadException`
    /// there (`IDS_CLASSLOAD_FIELDTOOLARGE`).
    ///
    /// `int64` because callers reach it by *multiplying* sizes, and the point of the check is to
    /// catch a product that would overflow an `int`.
    [<Literal>]
    let FIELD_OFFSET_LAST_REAL_OFFSET = 134217720L

    /// Round `value` up to the next multiple of `alignment`, which must be positive.
    let roundUpToAlignment (alignment : int) (value : int) : int =
        let error = value % alignment

        if error = 0 then value else value + (alignment - error)
