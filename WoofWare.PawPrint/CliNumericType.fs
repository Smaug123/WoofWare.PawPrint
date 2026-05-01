namespace WoofWare.PawPrint

open System
open Checked

/// Defined in III.1.1.1
type CliNumericType =
    | Int32 of int32
    | Int64 of int64
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
        | CliNumericType.Int64 i -> BitConverter.GetBytes i
        | CliNumericType.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim i -> BitConverter.GetBytes i
            | NativeIntSource.SyntheticCrossArrayOffset i -> BitConverter.GetBytes i
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
