namespace WoofWare.PawPrint

open System
open Checked

[<RequireQualifiedAccess>]
type Int64Source =
    | Verbatim of int64
    | SyntheticCrossArrayOffset of SyntheticCrossArrayOffset

    override this.ToString () =
        match this with
        | Int64Source.Verbatim i -> $"%i{i}"
        | Int64Source.SyntheticCrossArrayOffset _ -> "<synthetic cross-array offset>"

[<RequireQualifiedAccess>]
module Int64Source =

    let isZero (i : Int64Source) : bool =
        match i with
        | Int64Source.Verbatim i -> i = 0L
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: is SyntheticCrossArrayOffset zero?"

    let negate (i : Int64Source) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> Int64Source.Verbatim (0L - i)
        | Int64Source.SyntheticCrossArrayOffset i ->
            SyntheticCrossArrayOffset.negate i |> Int64Source.SyntheticCrossArrayOffset

    let shr (i : Int64Source) (shift : int) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> i >>> shift |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let shl (i : Int64Source) (shift : int) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> i <<< shift |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let add (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 + i2 |> Int64Source.Verbatim
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitNot (i : Int64Source) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> Int64Source.Verbatim ~~~i
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitAnd (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 &&& i2 |> Int64Source.Verbatim
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitOr (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 ||| i2 |> Int64Source.Verbatim
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitXor (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 ^^^ i2 |> Int64Source.Verbatim
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    /// Returns None if we can't decide whether this number is nonnegative.
    let isNonnegative (i : Int64Source) : bool option =
        match i with
        | Int64Source.Verbatim i -> Some (i >= 0L)
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

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
