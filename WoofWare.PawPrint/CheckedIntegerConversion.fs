namespace WoofWare.PawPrint

open System

/// The integer type a checked conversion (`conv.ovf.<to type>` or `conv.ovf.<to type>.un`)
/// produces. PawPrint's native int is 64 bits wide, so `NativeInt` and `UNativeInt` have the
/// ranges of `Int64` and `UInt64`.
[<RequireQualifiedAccess>]
type CheckedConversionTarget =
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | NativeInt
    | UNativeInt

/// How a checked conversion reads an integer source: `conv.ovf.<to type>` as signed, and
/// `conv.ovf.<to type>.un` as unsigned, so that the int32 `-1` is 4294967295 to the latter. A
/// float is signed by construction and has no other reading, so both read it the same way.
[<RequireQualifiedAccess>]
type CheckedConversionReading =
    | Signed
    | Unsigned

/// The numeric rule shared by every checked integer conversion: read the source as an exact
/// integer, truncating a float toward zero, and overflow unless the target's range contains it.
/// Comparing exact integers is what keeps the float bounds right without deriving them per
/// target: `conv.ovf.u2` overflows at 65536.0 above but only at -1.0 below, since everything
/// strictly between -1.0 and 0.0 truncates to zero, and an exact comparison of the truncated
/// value needs neither fact stated.
[<RequireQualifiedAccess>]
module CheckedIntegerConversion =

    /// The inclusive range of values `target` can hold.
    let range (target : CheckedConversionTarget) : bigint * bigint =
        match target with
        | CheckedConversionTarget.Int8 -> bigint SByte.MinValue, bigint SByte.MaxValue
        | CheckedConversionTarget.UInt8 -> 0I, bigint Byte.MaxValue
        | CheckedConversionTarget.Int16 -> bigint Int16.MinValue, bigint Int16.MaxValue
        | CheckedConversionTarget.UInt16 -> 0I, bigint UInt16.MaxValue
        | CheckedConversionTarget.Int32 -> bigint Int32.MinValue, bigint Int32.MaxValue
        | CheckedConversionTarget.UInt32 -> 0I, bigint UInt32.MaxValue
        | CheckedConversionTarget.Int64
        | CheckedConversionTarget.NativeInt -> bigint Int64.MinValue, bigint Int64.MaxValue
        | CheckedConversionTarget.UInt64
        | CheckedConversionTarget.UNativeInt -> 0I, bigint UInt64.MaxValue

    /// `value`, pushed as `target`'s stack type, or `Error ()` if `target` cannot hold it. The
    /// narrow targets push an int32, sign- or zero-extended as the target's own signedness says,
    /// and `UInt32` pushes its bits; the 64-bit and native targets push their bits likewise.
    let convert (target : CheckedConversionTarget) (value : bigint) : Result<EvalStackValue, unit> =
        let lower, upper = range target

        if value < lower || value > upper then
            Error ()
        else
            match target with
            | CheckedConversionTarget.Int8
            | CheckedConversionTarget.UInt8
            | CheckedConversionTarget.Int16
            | CheckedConversionTarget.UInt16
            | CheckedConversionTarget.Int32 -> int32 value |> Int32Source.Verbatim |> EvalStackValue.Int32
            | CheckedConversionTarget.UInt32 ->
                int32<uint32> (uint32 value) |> Int32Source.Verbatim |> EvalStackValue.Int32
            | CheckedConversionTarget.Int64 -> int64 value |> Int64Source.Verbatim |> EvalStackValue.Int64
            | CheckedConversionTarget.UInt64 ->
                int64<uint64> (uint64 value) |> Int64Source.Verbatim |> EvalStackValue.Int64
            | CheckedConversionTarget.NativeInt -> int64 value |> NativeIntSource.Verbatim |> EvalStackValue.NativeInt
            | CheckedConversionTarget.UNativeInt ->
                int64<uint64> (uint64 value)
                |> NativeIntSource.Verbatim
                |> EvalStackValue.NativeInt
            |> Ok

    let private ofInt32 (reading : CheckedConversionReading) (value : int32) : bigint =
        match reading with
        | CheckedConversionReading.Signed -> bigint value
        | CheckedConversionReading.Unsigned -> bigint (uint32<int32> value)

    let private ofInt64 (reading : CheckedConversionReading) (value : int64) : bigint =
        match reading with
        | CheckedConversionReading.Signed -> bigint value
        | CheckedConversionReading.Unsigned -> bigint (uint64<int64> value)

    /// The checked conversion of a plain number, or `None` if `value` is not one: a source carrying
    /// provenance (a byref, a handle, synthesised hash bits, a cross-array offset) has no bits
    /// PawPrint can vouch for, and what to do with it is each opcode's own policy. A native int
    /// that is a null byref counts as the number zero, because the guest has already asked for it
    /// as a number. `operation` names the opcode in the refusal of an int32 that is not a number.
    let ofNumber
        (operation : string)
        (target : CheckedConversionTarget)
        (reading : CheckedConversionReading)
        (value : EvalStackValue)
        : Result<EvalStackValue, unit> option
        =
        match value with
        | EvalStackValue.Int32 source -> Int32Source.value operation source |> ofInt32 reading |> convert target |> Some
        | EvalStackValue.Int64 (Int64Source.Verbatim i)
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> ofInt64 reading i |> convert target |> Some
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) ->
            convert target 0I |> Some
        | EvalStackValue.Float f ->
            let f = EvalStackFloat.toDouble f

            // NaN and the infinities are held by no integer target. Every finite double is an
            // exact rational, so its truncation is an exact integer.
            if Double.IsFinite f then
                bigint (Math.Truncate f) |> convert target
            else
                Error ()
            |> Some
        | EvalStackValue.Int64 _
        | EvalStackValue.NativeInt _
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ -> None
        | EvalStackValue.Undefined u -> UndefinedValue.failUnobserved operation u
