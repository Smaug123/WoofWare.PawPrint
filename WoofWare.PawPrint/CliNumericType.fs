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
    /// Deterministic synthesised bits produced when a bit-mixing operation
    /// (`shl` / `shr` / `shrUn` / `bitAnd` / `bitOr` / `bitXor` / `bitNot` /
    /// `negate` / `add`) fires on a `WidenedNativeInt` whose underlying source is a pointer
    /// shape (`MethodTablePtr`, `TypeHandlePtr`, etc.). The hash bits derive
    /// from the source's identity (see `materialiseHashBits`) and respect
    /// the low-bit contract used elsewhere in the interpreter
    /// (MethodTable* → low 2 bits clear; TypeDesc-shaped → low 2 bits set
    /// to `0b10`). Once a value has this tag, further bit ops compute on
    /// the bits directly and the result keeps the same tag.
    ///
    /// The tag's load-bearing job: an `OpaqueHashBits` value MUST NOT be
    /// converted back to a `NativeInt` (via `conv.u` / `conv.i`); doing so
    /// would let a synthesised non-pointer be used as a real pointer and
    /// silently dereferenced. `conv.i4` is allowed — that path is how the
    /// cast-cache hash becomes an array index. See
    /// `docs/plans/2026-05-13-castcache-synthetic-hash-bits.md`.
    | OpaqueHashBits of int64

    override this.ToString () =
        match this with
        | Int64Source.Verbatim i -> $"%i{i}"
        | Int64Source.SyntheticCrossArrayOffset _ -> "<synthetic cross-array offset>"
        | Int64Source.WidenedNativeInt (src, signed) ->
            let conv = if signed then "conv.i8" else "conv.u8"
            $"<%s{conv} %O{src}>"
        | Int64Source.OpaqueHashBits bits -> $"<opaque hash bits 0x%x{bits}>"

[<RequireQualifiedAccess>]
module Int64Source =

    /// Low-bit contract for pointer-shaped handles. Mirrors
    /// `NullaryIlOp.typeHandleLowAddressBits`, kept in sync so that
    /// `materialiseHashBits` honours the same alignment / tagging convention
    /// when synthesising bits for hashing. Centralising this would require
    /// promoting types — for now the contract is small enough to duplicate.
    let private typeHandleLowAddressBitsForHash (target : RuntimeTypeHandleTarget) : int64 =
        match target with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> 0L
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ -> 2L
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> 2L
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> 0L

    /// Synthesise a deterministic 64-bit pattern from a pointer-shaped
    /// `NativeIntSource`. Used only when a bit-mixing operation fires on a
    /// `WidenedNativeInt`; the resulting bits get tagged as
    /// `Int64Source.OpaqueHashBits` so subsequent ops compute on the bits
    /// directly and the value cannot be round-tripped back to a real
    /// pointer.
    ///
    /// Determinism: derives bits from `GetHashCode` on the structural DU
    /// payload. PawPrint identity types use structural hashing, so two
    /// processes loading the same assemblies produce identical hash bits
    /// for the same `MethodTablePtr`/`TypeHandlePtr`. The cast-cache
    /// hashing only needs determinism *within* a process anyway.
    ///
    /// Low-bit contract: matches `typeHandleLowAddressBitsForHash` so the
    /// synthesised bits agree with the existing `and`-mask path. The
    /// upper bits are shifted to ensure `RotateLeft(_, 32)` produces a
    /// non-degenerate hash mix.
    let private materialiseHashBits (src : NativeIntSource) : int64 =
        match src with
        | NativeIntSource.Verbatim n -> n
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L
        | NativeIntSource.MethodTablePtr _ ->
            let h = int64 (hash src) <<< 16
            h &&& ~~~3L
        | NativeIntSource.TypeHandlePtr target ->
            let low = typeHandleLowAddressBitsForHash target
            let h = int64 (hash src) <<< 16
            (h &&& ~~~3L) ||| low
        | NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.FunctionPointer _
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.MethodHandlePtr _
        | NativeIntSource.GcHandlePtr _
        | NativeIntSource.EventPipeProviderPtr _
        | NativeIntSource.EventPipeEventPtr _
        | NativeIntSource.AssemblyHandle _
        | NativeIntSource.ModuleHandle _
        | NativeIntSource.MetadataImportHandle _ ->
            let h = int64 (hash src) <<< 16
            h &&& ~~~3L
        | NativeIntSource.ManagedPointer _ ->
            // Non-null managed pointers carry real provenance (byref roots,
            // heap addresses). Hashing them as plain bits would forget the
            // root identity. Bit ops on a `WidenedNativeInt (ManagedPointer
            // _, _)` should be routed via BinaryArithmetic (offset arithmetic),
            // never through this helper.
            failwith
                $"materialiseHashBits: refusing to synthesise bits for managed pointer %O{src} (would erase byref provenance)"
        | NativeIntSource.SyntheticCrossArrayOffset _ ->
            // Cross-array offsets are explicitly non-numeric; the
            // `widenedNativeInt` smart constructor normalises these into
            // `Int64Source.SyntheticCrossArrayOffset`, so a
            // `WidenedNativeInt (SyntheticCrossArrayOffset _, _)` shouldn't
            // exist on the eval stack. Fail loudly if one ever does.
            failwith $"materialiseHashBits: refusing to synthesise bits for synthetic cross-array offset %O{src}"

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
        | Int64Source.OpaqueHashBits bits -> bits = 0L

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
            let bits = materialiseHashBits src
            // Wraparound at Int64.MinValue is acceptable here: hash bits
            // are an intermediate in the bit-mixing pipeline, not a genuine
            // signed-int value where overflow matters. Use unchecked
            // subtraction explicitly (the file opens `Checked`).
            Some (Operators.(-) 0L bits |> Int64Source.OpaqueHashBits)
        | Int64Source.OpaqueHashBits bits -> Some (Operators.(-) 0L bits |> Int64Source.OpaqueHashBits)

    let shr (i : Int64Source) (shift : int) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> i >>> shift |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits = materialiseHashBits src
            bits >>> shift |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits bits -> bits >>> shift |> Int64Source.OpaqueHashBits

    let shrUn (i : Int64Source) (shift : int) : Int64Source =
        // `open Checked` shadows `uint64` / `int64` with their overflow-checking
        // versions; an unsigned right shift needs the unchecked tag-flip, since a
        // negative int64 has the sign bit set and `Checked.uint64` rejects that.
        let unsignedShift (bits : int64) : int64 =
            Operators.uint64 bits >>> shift |> Operators.int64

        match i with
        | Int64Source.Verbatim i -> unsignedShift i |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits = materialiseHashBits src
            unsignedShift bits |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits bits -> unsignedShift bits |> Int64Source.OpaqueHashBits

    let shl (i : Int64Source) (shift : int) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> i <<< shift |> Int64Source.Verbatim
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits = materialiseHashBits src
            bits <<< shift |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits bits -> bits <<< shift |> Int64Source.OpaqueHashBits

    let add (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 + i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src, _), _
        | _, Int64Source.WidenedNativeInt (src, _) ->
            // Pointer-shaped int64 arithmetic is handled by BinaryArithmetic.execute
            // (which dispatches on EvalStackValue pairs), not via this generic helper.
            failwith $"TODO: Int64Source.add on widened native int %O{src} should be routed through BinaryArithmetic"
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b ->
            // Adding two synthesised hash values is a bit-mixing operation,
            // not pointer arithmetic — keep the tag and wrap on overflow.
            (Operators.(+) a b) |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> (Operators.(+) a b) |> Int64Source.OpaqueHashBits
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitNot (i : Int64Source) : Int64Source =
        match i with
        | Int64Source.Verbatim i -> Int64Source.Verbatim ~~~i
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits = materialiseHashBits src
            ~~~bits |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits bits -> ~~~bits |> Int64Source.OpaqueHashBits
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitAnd (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 &&& i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src1, _), Int64Source.WidenedNativeInt (src2, _) ->
            (materialiseHashBits src1) &&& (materialiseHashBits src2)
            |> Int64Source.OpaqueHashBits
        | Int64Source.WidenedNativeInt (src, _), Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.WidenedNativeInt (src, _) ->
            (materialiseHashBits src) &&& b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> a &&& b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> a &&& b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.WidenedNativeInt (src, _)
        | Int64Source.WidenedNativeInt (src, _), Int64Source.OpaqueHashBits a ->
            a &&& materialiseHashBits src |> Int64Source.OpaqueHashBits
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitOr (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 ||| i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src1, _), Int64Source.WidenedNativeInt (src2, _) ->
            (materialiseHashBits src1) ||| (materialiseHashBits src2)
            |> Int64Source.OpaqueHashBits
        | Int64Source.WidenedNativeInt (src, _), Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.WidenedNativeInt (src, _) ->
            (materialiseHashBits src) ||| b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> a ||| b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> a ||| b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.WidenedNativeInt (src, _)
        | Int64Source.WidenedNativeInt (src, _), Int64Source.OpaqueHashBits a ->
            a ||| materialiseHashBits src |> Int64Source.OpaqueHashBits
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    let bitXor (i1 : Int64Source) (i2 : Int64Source) : Int64Source =
        match i1, i2 with
        | Int64Source.Verbatim i1, Int64Source.Verbatim i2 -> i1 ^^^ i2 |> Int64Source.Verbatim
        | Int64Source.WidenedNativeInt (src1, _), Int64Source.WidenedNativeInt (src2, _) ->
            (materialiseHashBits src1) ^^^ (materialiseHashBits src2)
            |> Int64Source.OpaqueHashBits
        | Int64Source.WidenedNativeInt (src, _), Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.WidenedNativeInt (src, _) ->
            (materialiseHashBits src) ^^^ b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> a ^^^ b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> a ^^^ b |> Int64Source.OpaqueHashBits
        | Int64Source.OpaqueHashBits a, Int64Source.WidenedNativeInt (src, _)
        | Int64Source.WidenedNativeInt (src, _), Int64Source.OpaqueHashBits a ->
            a ^^^ materialiseHashBits src |> Int64Source.OpaqueHashBits
        | _, _ -> failwith "TODO: SyntheticCrossArrayOffset"

    /// Returns None if we can't decide whether this number is nonnegative.
    let isNonnegative (i : Int64Source) : bool option =
        match i with
        | Int64Source.Verbatim i -> Some (i >= 0L)
        | Int64Source.WidenedNativeInt (src, _) -> Some (NativeIntSource.isNonnegative src)
        | Int64Source.OpaqueHashBits bits -> Some (bits >= 0L)
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

    /// Signed comparison of two `Int64Source` values, treating each as the
    /// signed int64 it represents. Returns negative / zero / positive in the
    /// `compare` convention. `Int64Source` no longer supports structural
    /// comparison (it now contains a `NativeIntSource`, which is
    /// `[<NoComparison>]`), so callers must funnel through this helper.
    /// Non-`Verbatim` numeric variants (`OpaqueHashBits`) compare on their
    /// synthesised bits; `WidenedNativeInt` and `SyntheticCrossArrayOffset`
    /// don't have a meaningful numeric ordering and fail loudly — pointer
    /// provenance and cross-storage offsets shouldn't be compared as plain
    /// integers. For unsigned comparison see the `cgt.un` / `clt.un` paths
    /// in `EvalStackValueComparisons`.
    let compareSigned (i1 : Int64Source) (i2 : Int64Source) : int =
        match i1, i2 with
        | Int64Source.Verbatim a, Int64Source.Verbatim b -> compare a b
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> compare a b
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> compare a b
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
        | CliNumericType.Int64 (Int64Source.OpaqueHashBits bits) ->
            failwith $"refusing to convert synthesised pointer-hash bits 0x%x{bits} to bytes"
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
            | NativeIntSource.EventPipeProviderPtr _ -> failwith "refusing to express EventPipeProviderPtr as bytes"
            | NativeIntSource.EventPipeEventPtr _ -> failwith "refusing to express EventPipeEventPtr as bytes"
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
