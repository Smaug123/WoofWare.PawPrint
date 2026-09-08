namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Why CoreCLR's `AllocateArrayEx` (`vm/gchelpers.cpp:836-895`) refuses a multi-dimensional
/// array's dimension lengths. Each case is a guest-visible exception rather than an interpreter
/// failure, and they do not all raise the same exception, so this stays a DU rather than a bool.
[<RequireQualifiedAccess>]
type MultiDimArrayLengthError =
    /// `length < 0` for this dimension: `COMPlusThrow(kOverflowException)`, raised as soon as the
    /// loop reaches the dimension.
    | Negative of dimension : int * length : int
    /// `(SIZE_T)length > MaxArrayLength()` for this dimension. CoreCLR records this in a flag and
    /// raises it only after every dimension has been walked ("Throw this exception only after
    /// everything else was validated for backward compatibility"), so it loses to a `Negative`
    /// in any dimension and to a `TotalElementsOverflow` at any point.
    | DimensionExceedsMaxLength of dimension : int * length : int
    /// The running product overflowed `S_UINT32` at this dimension. Checked inside the loop, so a
    /// later zero dimension cannot rescue it.
    | TotalElementsOverflow of dimension : int
    /// The product fits in `UInt32` but not in `Int32`, so PawPrint cannot index the backing
    /// store it would need. CoreCLR has no such rule — it hands the count to the GC, which fails
    /// the allocation — so both reach `OutOfMemoryException`, by different routes.
    | TotalElementsExceedsInt32

/// The dimension-length policy shared by every route into a multi-dimensional array allocation:
/// the `newobj T[,]` constructor and the `Array_CreateInstance` QCall both bottom out in
/// CoreCLR's `AllocateArrayEx`, so they must agree on the limits and on what violating them
/// raises.
///
/// Applied *at the boundary* where guest-supplied ints become dimension lengths, for the reason
/// `SzArrayAllocation` gives for the single-dimensional case: only the caller can turn the answer
/// into a guest exception. `IlMachineThreadState.allocateMultiDimArray` asserts the resulting
/// precondition rather than re-deriving it.
[<RequireQualifiedAccess>]
module MultiDimArrayAllocation =
    /// The total element count CoreCLR would allocate, or the first reason it refuses.
    ///
    /// The walk is the one `AllocateArrayEx` performs, in its order, because the order is
    /// guest-visible: a negative length in a later dimension beats an over-long length in an
    /// earlier one, and those raise different exceptions. The two rules that both raise
    /// `OutOfMemoryException` cannot be told apart by their order, but they are still evaluated
    /// where CoreCLR evaluates them so that the classifier reads as the source does.
    ///
    /// The running product is accumulated in `uint32` exactly as CoreCLR's `S_UINT32` is, so a
    /// transient prefix above `Int32.MaxValue` that a later zero dimension brings back down is
    /// allowed: `new int[50000, 50000, 0]` is an empty array, while `new int[65536, 65536, 0]`
    /// overflows at the second multiply and is refused regardless of the trailing zero.
    let totalElements (dimensionLengths : ImmutableArray<int>) : Result<int, MultiDimArrayLengthError> =
        let mutable running : uint32 = 1u
        let mutable pendingMaxLength : MultiDimArrayLengthError option = None
        let mutable failure : MultiDimArrayLengthError option = None
        let mutable i = 0

        while failure.IsNone && i < dimensionLengths.Length do
            let length = dimensionLengths.[i]

            if length < 0 then
                failure <- Some (MultiDimArrayLengthError.Negative (i, length))
            else

            if length > SzArrayAllocation.maxLength && pendingMaxLength.IsNone then
                pendingMaxLength <- Some (MultiDimArrayLengthError.DimensionExceedsMaxLength (i, length))

            let lengthU = uint32 length

            // Multiplying by zero cannot overflow; it just zeroes the running product.
            if lengthU <> 0u && running > System.UInt32.MaxValue / lengthU then
                failure <- Some (MultiDimArrayLengthError.TotalElementsOverflow i)
            else
                running <- running * lengthU
                i <- i + 1

        match failure, pendingMaxLength with
        | Some failure, _ -> Error failure
        | None, Some pending -> Error pending
        | None, None ->
            if running > uint32 System.Int32.MaxValue then
                Error MultiDimArrayLengthError.TotalElementsExceedsInt32
            else
                Ok (int running)

    /// The exception CoreCLR raises for this rejection, and the message it carries (`None`
    /// meaning the parameterless constructor's own default, which is what a
    /// `COMPlusThrow(kOverflowException)` or a plain `ThrowOutOfMemory()` produces).
    ///
    /// Names the exception through `BaseClassTypes` rather than an `OpcodeFault`, because the
    /// caller this serves is a QCall rather than an instruction; `newobj`'s own faults are
    /// reachable through its `OpcodeFaults` entry when that caller comes to raise them.
    let exceptionFor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (error : MultiDimArrayLengthError)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> * string option
        =
        match error with
        | MultiDimArrayLengthError.Negative _ -> baseClassTypes.OverflowException, None
        | MultiDimArrayLengthError.DimensionExceedsMaxLength _
        | MultiDimArrayLengthError.TotalElementsOverflow _ ->
            baseClassTypes.OutOfMemoryException, Some SzArrayAllocation.dimensionsExceededMessage
        // The GC's own refusal, which carries no dimensions-exceeded message.
        | MultiDimArrayLengthError.TotalElementsExceedsInt32 -> baseClassTypes.OutOfMemoryException, None

    /// A description of the violation for interpreter-facing diagnostics — an
    /// `allocateMultiDimArray` precondition failure, not a guest exception.
    let describe (error : MultiDimArrayLengthError) : string =
        match error with
        | MultiDimArrayLengthError.Negative (dimension, length) ->
            $"dimension %d{dimension} has negative length %d{length}"
        | MultiDimArrayLengthError.DimensionExceedsMaxLength (dimension, length) ->
            $"dimension %d{dimension} has length %d{length}, above MaxArrayLength() = %d{SzArrayAllocation.maxLength}"
        | MultiDimArrayLengthError.TotalElementsOverflow dimension ->
            $"the running element count overflows UInt32 at dimension %d{dimension}"
        | MultiDimArrayLengthError.TotalElementsExceedsInt32 ->
            "the total element count exceeds Int32.MaxValue, which PawPrint's backing store cannot index"
