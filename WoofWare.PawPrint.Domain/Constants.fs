namespace WoofWare.PawPrint

[<AutoOpen>]
module Constants =

    [<Literal>]
    let SIZEOF_INT = 4

    [<Literal>]
    let SIZEOF_OBJ = 8

    [<Literal>]
    let DEFAULT_STRUCT_ALIGNMENT = 8

    [<Literal>]
    let NATIVE_INT_SIZE = 8

    /// Base-2 log of `NATIVE_INT_SIZE`. CoreCLR's auto layout buckets fields by size class,
    /// indexing arrays by the base-2 log of the field size; this is the largest such index,
    /// CoreCLR's `MAX_LOG2_PRIMITIVE_FIELD_SIZE` (class.h:56) and `LOG2SLOT`.
    [<Literal>]
    let LOG2_NATIVE_INT_SIZE = 3

    /// Round `value` up to the next multiple of `alignment`, which must be positive.
    let roundUpToAlignment (alignment : int) (value : int) : int =
        let error = value % alignment

        if error = 0 then value else value + (alignment - error)
