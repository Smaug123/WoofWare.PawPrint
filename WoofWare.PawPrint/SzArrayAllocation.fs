namespace WoofWare.PawPrint

/// Why CoreCLR's `AllocateSzArray` (`vm/gchelpers.cpp:624-641`) refuses a single-dimensional
/// array length outright, before attempting any allocation. Both rejections are guest-visible
/// exceptions rather than interpreter failures, and each has its own exception type, so this
/// stays a two-case DU rather than a bool: a caller that has one of these in hand still has to
/// know which it is.
[<RequireQualifiedAccess>]
type SzArrayLengthError =
    /// `cElements < 0`. ECMA-335 III.4.13 ("newarr") specifies this for the opcode too.
    | Negative
    /// `(SIZE_T)cElements > MaxArrayLength()`.
    | ExceedsMaxLength

/// The length policy shared by every route into a single-dimensional array allocation: the
/// `newarr` opcode and the `GCInterface_AllocateNewArray` QCall both bottom out in CoreCLR's
/// `AllocateSzArray`, so they must agree on the limit and on what violating it raises.
///
/// The classifier is applied *at the boundary* where an untrusted guest int becomes a length,
/// rather than inside `IlMachineThreadState.allocateArray`: the caller is the only party that
/// can turn the answer into a guest exception, and every other caller of `allocateArray`
/// supplies a length it computed itself. `allocateArray` asserts the resulting precondition
/// rather than re-deriving it.
[<RequireQualifiedAccess>]
module SzArrayAllocation =
    /// `MaxArrayLength()` (`gchelpers.cpp:604-609`), which upstream keeps in sync with the
    /// managed `Array.MaxLength`.
    let maxLength : int = 0x7FFFFFC7

    /// The English text of `IDS_EE_ARRAY_DIMENSIONS_EXCEEDED` (`mscorrc.rc:455`), the native
    /// resource string CoreCLR's `ThrowOutOfMemoryDimensionsExceeded` (`gchelpers.cpp:767-778`)
    /// attaches to the `OutOfMemoryException` it raises. PawPrint has no resource pipeline, so
    /// the literal is reproduced here byte-for-byte, exactly as `NativeException.messageForKind`
    /// does for the sibling `mscorrc` strings.
    ///
    /// This message is the `HOST_64BIT` answer: a 32-bit CoreCLR falls through to a
    /// plain `ThrowOutOfMemory()` with the default message instead. PawPrint models a 64-bit
    /// target throughout (`SimulatedUnixPlatform` defaults to `LinuxX64`, and `NativeInt` is
    /// 64 bits wide), so the 64-bit arm is the faithful one here.
    let dimensionsExceededMessage : string =
        "Array dimensions exceeded supported range."

    /// `None` if an SZ array of this many elements is one CoreCLR would go on to allocate.
    /// Note the boundary is exclusive: `maxLength` itself is allocatable.
    let checkLength (length : int) : SzArrayLengthError option =
        // `gchelpers.cpp:637-638` then `:640-641`, in that order — though the two cases are
        // disjoint, so the order is not observable.
        if length < 0 then
            Some SzArrayLengthError.Negative
        elif length > maxLength then
            Some SzArrayLengthError.ExceedsMaxLength
        else
            None

    /// The fault CoreCLR raises for this rejection, plus a message override (`None` meaning the
    /// parameterless constructor's own default message, which is what
    /// `COMPlusThrow(kOverflowException)` produces).
    ///
    /// Names the fault rather than resolving it, because the two callers must raise it by
    /// different routes: `newarr` is an instruction and goes through the `OpcodeFaults`-checked
    /// path, while `NativeGc`'s allocation helper is not and cannot. Both faults are in `newarr`'s
    /// table entry, which is what makes the checked route available to it at all.
    let faultFor (error : SzArrayLengthError) : OpcodeFault * string option =
        match error with
        | SzArrayLengthError.Negative ->
            // `COMPlusThrow(kOverflowException)`, with no message.
            OpcodeFault.Overflow, None
        | SzArrayLengthError.ExceedsMaxLength -> OpcodeFault.OutOfMemory, Some dimensionsExceededMessage

    /// A description of the violation for interpreter-facing diagnostics — an
    /// `allocateArray` precondition failure, not a guest exception.
    let describe (error : SzArrayLengthError) : string =
        match error with
        | SzArrayLengthError.Negative -> "must be non-negative"
        | SzArrayLengthError.ExceedsMaxLength -> $"must be at most MaxArrayLength() = %d{maxLength}"
