namespace WoofWare.PawPrint

open System
open Checked

[<RequireQualifiedAccess>]
type Int64Source =
    | Verbatim of int64
    | SyntheticCrossArrayOffset of SyntheticCrossArrayOffset
    /// The result of `conv.i8` / `conv.u8` applied to a NativeInt (managed
    /// pointer, function pointer, type handle, …). On a 64-bit interpreter
    /// this widening is bit-preserving; on a 32-bit interpreter it would
    /// zero/sign-extend 32→64. PawPrint is a 64-bit interpreter
    /// (`NATIVE_INT_SIZE = 8`), so the int64 obtained this way carries the
    /// same provenance as the underlying NativeIntSource. Operations on this
    /// variant must respect that an int64 obtained this way could in
    /// principle outgrow the native word — but on 64-bit we never observe
    /// this, so most numeric ops fail loudly to keep the round-trip
    /// inversion contract honest.
    ///
    /// Always construct via `Int64Source.widenedNativeInt`, which normalises
    /// `Verbatim` and `SyntheticCrossArrayOffset` cases to the corresponding
    /// `Int64Source` variants. Unnormalised forms violate invariants of
    /// pattern matches that assume `Verbatim` / `SyntheticCrossArrayOffset`
    /// cover all "purely numeric" int64 values.
    | WidenedNativeInt of source : NativeIntSource * signed : bool

    override this.ToString () =
        match this with
        | Int64Source.Verbatim i -> $"%i{i}"
        | Int64Source.SyntheticCrossArrayOffset _ -> "<synthetic cross-array offset>"
        | Int64Source.WidenedNativeInt (src, signed) ->
            let conv = if signed then "conv.i8" else "conv.u8"
            $"<%s{conv} %O{src}>"

[<RequireQualifiedAccess>]
module Int64Source =

    /// Smart constructor for `Int64Source.WidenedNativeInt`. Normalises the
    /// `Verbatim` and `SyntheticCrossArrayOffset` cases of the underlying
    /// `NativeIntSource` so they round-trip back into the canonical
    /// `Int64Source` shapes; non-numeric sources are wrapped to preserve
    /// provenance through `conv.i8` / `conv.u8`.
    let widenedNativeInt (src : NativeIntSource) (signed : bool) : Int64Source =
        match src with
        | NativeIntSource.Verbatim n -> Int64Source.Verbatim n
        | NativeIntSource.SyntheticCrossArrayOffset s -> Int64Source.SyntheticCrossArrayOffset s
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> Int64Source.Verbatim 0L
        | _ -> Int64Source.WidenedNativeInt (src, signed)

    let isZero (i : Int64Source) : bool =
        match i with
        | Int64Source.Verbatim i -> i = 0L
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: is SyntheticCrossArrayOffset zero?"
        | Int64Source.WidenedNativeInt (src, _) -> NativeIntSource.isZero src

    /// Returns None if the input was Int64.MinValue.
    let negate (i : Int64Source) : Int64Source option =
        match i with
        | Int64Source.Verbatim i ->
            if i = Int64.MinValue then
                None
            else
                Int64Source.Verbatim (0L - i) |> Some
        | Int64Source.SyntheticCrossArrayOffset i ->
            SyntheticCrossArrayOffset.negate i
            |> Int64Source.SyntheticCrossArrayOffset
            |> Some
        | Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to negate widened native int %O{src} (would lose provenance)"

    let shr (i : Int64Source) (shift : int) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> i >>> shift |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to shr widened native int %O{src} (bit-twiddling on pointer bits)"

    let shl (i : Int64Source) (shift : int) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> i <<< shift |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to shl widened native int %O{src} (bit-twiddling on pointer bits)"

    let add (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 + i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src, _), _
        | _, Int64Source.WidenedNativeInt (src, _) ->
            // Pointer-shaped int64 arithmetic is handled by BinaryArithmetic.execute
            // (which dispatches on EvalStackValue pairs), not via this generic helper.
            failwith $"TODO: Int64Source.add on widened native int %O{src} should be routed through BinaryArithmetic"
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitNot (i : Int64Source) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> Int64Source.Verbatim ~~~i
        | Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to bitNot widened native int %O{src} (bit-twiddling on pointer bits)"
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitAnd (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 &&& i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src, _), _
        | _, Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to bitAnd widened native int %O{src} (bit-twiddling on pointer bits)"
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitOr (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 ||| i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src, _), _
        | _, Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to bitOr widened native int %O{src} (bit-twiddling on pointer bits)"
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitXor (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 ^^^ i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src, _), _
        | _, Int64Source.WidenedNativeInt (src, _) ->
            failwith $"TODO: refusing to bitXor widened native int %O{src} (bit-twiddling on pointer bits)"
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    /// Returns None if we can't decide whether this number is nonnegative.
    let isNonnegative (i : Int64Source) : bool option =
        match i with
        | Int64Source.Verbatim i -> Some (i >= 0L)
        | Int64Source.WidenedNativeInt (src, _) -> Some (NativeIntSource.isNonnegative src)
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

    /// Numerically compare two `Int64Source` values, treating them as their
    /// underlying int64 bits. `Int64Source` no longer supports structural
    /// comparison (it now contains a `NativeIntSource`, which is
    /// `[<NoComparison>]`), so callers must funnel through this helper.
    /// Non-`Verbatim` variants don't have a meaningful numeric ordering and
    /// fail loudly — provenance-tracked offsets and widened pointer bits
    /// shouldn't be compared as plain integers.
    let compareBits (i1 : Int64Source) (i2 : Int64Source) : int =
        match i1, i2 with
        | Int64Source.Verbatim a, Int64Source.Verbatim b -> compare a b
        | _, _ -> failwith $"TODO: refusing to compare Int64Source values numerically: %O{i1} vs %O{i2}"

/// Defined in III.1.1.1
type CliNumericType =
    | Int32 of int32
    | Int64 of Int64Source
    /// The real CLR just represents these as native ints, but we track their provenance.
    | NativeInt of NativeIntSource
    | NativeFloat of float
    | Int8 of int8
    | Int16 of int16
    | UInt8 of uint8
    | UInt16 of uint16
    | Float32 of float32
    | Float64 of float

    static member SizeOf (t : CliNumericType) : int =
        match t with
        | CliNumericType.Int32 _ -> 4
        | CliNumericType.Int64 _ -> 8
        | CliNumericType.NativeInt _ -> 8
        | CliNumericType.NativeFloat _ -> 8
        | CliNumericType.Int8 _ -> 1
        | CliNumericType.Int16 _ -> 2
        | CliNumericType.UInt8 _ -> 1
        | CliNumericType.UInt16 _ -> 2
        | CliNumericType.Float32 _ -> 4
        | CliNumericType.Float64 _ -> 8

    static member ToBytes (t : CliNumericType) : byte[] =
        match t with
        | CliNumericType.Int32 i -> BitConverter.GetBytes i
        | CliNumericType.Int64 (Int64Source.Verbatim i) -> BitConverter.GetBytes i
        | CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset _) ->
            failwith "refusing to convert cross-array offset to bytes"
        | CliNumericType.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            failwith $"refusing to convert widened native int %O{src} to bytes"
        | CliNumericType.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim i -> BitConverter.GetBytes i
            | NativeIntSource.SyntheticCrossArrayOffset _ -> failwith "refusing to convert cross-array offset to bytes"
            | NativeIntSource.ManagedPointer src ->
                match src with
                | ManagedPointerSource.Null -> BitConverter.GetBytes 0L
                | _ -> failwith "refusing to express pointer as bytes"
            | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to express FieldHandlePtr as bytes"
            | NativeIntSource.MethodHandlePtr _ -> failwith "refusing to express MethodHandlePtr as bytes"
            | NativeIntSource.FunctionPointer _ -> failwith "refusing to express FunctionPointer as bytes"
            | NativeIntSource.TypeHandlePtr _ -> failwith "refusing to express TypeHandlePtr as bytes"
            | NativeIntSource.MethodTablePtr _ -> failwith "refusing to express MethodTablePtr as bytes"
            | NativeIntSource.MethodTableAuxiliaryDataPtr _ ->
                failwith "refusing to express MethodTableAuxiliaryDataPtr as bytes"
            | NativeIntSource.GcHandlePtr _ -> failwith "refusing to express GcHandlePtr as bytes"
            | NativeIntSource.AssemblyHandle _ -> failwith "refusing to express AssemblyHandle as bytes"
            | NativeIntSource.ModuleHandle _ -> failwith "refusing to express ModuleHandle as bytes"
            | NativeIntSource.MetadataImportHandle _ -> failwith "refusing to express MetadataImportHandle as bytes"
        | CliNumericType.NativeFloat f -> BitConverter.GetBytes f
        // Overload resolution for sbyte/byte silently picks
        // `BitConverter.GetBytes(System.Half)` (2 bytes) in net8/net9; build
        // the single-byte result explicitly to stay faithful to CLR layout.
        // Route a negative sbyte through int32 + mask to preserve bit
        // pattern without hitting the checked-conversion throw.
        | CliNumericType.Int8 i -> [| byte (int i &&& 0xFF) |]
        | CliNumericType.Int16 i -> BitConverter.GetBytes i
        | CliNumericType.UInt8 i -> [| i |]
        | CliNumericType.UInt16 i -> BitConverter.GetBytes i
        | CliNumericType.Float32 i -> BitConverter.GetBytes i
        | CliNumericType.Float64 i -> BitConverter.GetBytes i
