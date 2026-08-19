namespace WoofWare.PawPrint

open System
open Checked

/// The provenance of one byte of a value's materialised byte image.
///
/// Almost every byte is an ordinary number. The exception is a byte of a native int whose bits
/// PawPrint does not model — a type handle, a method table pointer, a byref — which has no
/// numeric value to give. `SignatureHelper.InternalAddRuntimeType` is what makes that reachable:
/// with no module to spell a type against, it copies the eight bytes of `type.TypeHandle.Value`
/// into a `Reflection.Emit` signature blob one at a time and hands the blob straight back to the
/// runtime, so the bytes are only ever moved, never inspected.
///
/// Naming the byte rather than materialising one is what lets that round trip be exact: the
/// decoder recovers the type from eight consecutive bytes of one source in ascending index
/// order, and a guest that scrambles or partially overwrites them is refused rather than
/// decoded to a plausible wrong type. Making it a case *here*, rather than filling the byte
/// with synthesised address bits, is what makes the compiler visit every consumer of a byte:
/// none of them can treat one of these as a number by accident, because none of them can get at
/// a number without saying what to do when there isn't one.
[<RequireQualifiedAccess>]
type UInt8Source =
    | Verbatim of uint8
    /// Byte `index` (0-based, little-endian) of the eight-byte image of a native int PawPrint
    /// models as an identity rather than as an address.
    | NativeIntByte of source : NativeIntSource * index : int

    override this.ToString () : string =
        match this with
        | UInt8Source.Verbatim b -> $"%i{b}"
        | UInt8Source.NativeIntByte (source, index) -> $"<byte %i{index} of %O{source}>"

[<RequireQualifiedAccess>]
module UInt8Source =

    /// The numeric value of a byte.
    ///
    /// A byte of an unmodelled native int has none: it names a position in an identity PawPrint
    /// carries instead of an address. `operation` names the consumer, so a guest that reaches one
    /// fails precisely rather than somewhere downstream.
    /// `ValueSome` for a byte that has a numeric value, `ValueNone` for one that names a native
    /// int instead.
    let tryValue (src : UInt8Source) : uint8 voption =
        match src with
        | UInt8Source.Verbatim b -> ValueSome b
        | UInt8Source.NativeIntByte _ -> ValueNone

    /// `ValueSome` iff every byte in the image has a numeric value, so the image is an ordinary
    /// `byte[]`.
    let tryValues (bytes : UInt8Source[]) : byte[] voption =
        let result = Array.zeroCreate<byte> bytes.Length
        let mutable i = 0
        let mutable ok = true

        while ok && i < bytes.Length do
            match tryValue bytes.[i] with
            | ValueSome b ->
                result.[i] <- b
                i <- i + 1
            | ValueNone -> ok <- false

        if ok then ValueSome result else ValueNone

    let value (operation : string) (src : UInt8Source) : uint8 =
        match src with
        | UInt8Source.Verbatim b -> b
        | UInt8Source.NativeIntByte (source, index) ->
            failwith
                $"%s{operation}: refusing to use byte %i{index} of %O{source} as a number; PawPrint models that native int as an identity rather than as an address, so it has no byte value"

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
    /// `negate`) fires on a `WidenedNativeInt` whose underlying source is a
    /// pointer shape (`MethodTablePtr`, `TypeHandlePtr`, etc.), or when an
    /// arithmetic opcode (`Add`, `Mul`, …) runs through `BinaryArithmetic`
    /// with at least one OpaqueHashBits operand. The hash bits derive from
    /// the source's identity (see `materialiseHashBits`) and respect the
    /// low-bit contract used elsewhere in the interpreter (MethodTable* →
    /// low 2 bits clear; TypeDesc-shaped → low 2 bits set to `0b10`). Once
    /// a value has this tag, further bit ops compute on the bits directly
    /// and the result keeps the same tag.
    ///
    /// An `OpaqueHashBits` value MUST NOT be
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
        | NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
            // `Unsafe.AsRef<T>((void*)bits)` placeholders ARE bit patterns;
            // widening to int64 is a verbatim move, not a provenance-carrying
            // wrap. Should normally be canonicalised before reaching here, but
            // defending in depth keeps the invariant local.
            Int64Source.Verbatim bits
        | NativeIntSource.OpaqueHashBits bits -> Int64Source.OpaqueHashBits bits
        | _ -> Int64Source.WidenedNativeInt (src, signed)

    let isZero (i : Int64Source) : bool =
        match i with
        | Int64Source.Verbatim i -> i = 0L
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith "TODO: is SyntheticCrossArrayOffset zero?"
        | Int64Source.WidenedNativeInt (src, _) -> NativeIntSource.isZero src
        | Int64Source.OpaqueHashBits bits -> bits = 0L

    /// Negate an `Int64Source`. Returns `None` only when the input is the
    /// genuine `Int64.MinValue` whose negation overflows; for synthesised
    /// pointer-hash bits the wraparound at `Int64.MinValue` is acceptable
    /// because the hash domain isn't a genuine signed-int value. Threads
    /// `PointerHashState` because materialising a `WidenedNativeInt`
    /// may register a new pointer.
    let negate
        (reason : string)
        (i : Int64Source)
        (counters : PointerHashState)
        : (Int64Source * PointerHashState) option
        =
        match i with
        | Int64Source.Verbatim i ->
            if i = Int64.MinValue then
                None
            else
                Some (Int64Source.Verbatim (0L - i), counters)
        | Int64Source.SyntheticCrossArrayOffset i ->
            Some (SyntheticCrossArrayOffset.negate i |> Int64Source.SyntheticCrossArrayOffset, counters)
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            // Wraparound at Int64.MinValue is acceptable here: hash bits
            // are an intermediate in the bit-mixing pipeline, not a genuine
            // signed-int value where overflow matters. Use unchecked
            // subtraction explicitly (the file opens `Checked`).
            Some (Operators.(-) 0L bits |> Int64Source.OpaqueHashBits, counters)
        | Int64Source.OpaqueHashBits bits -> Some (Operators.(-) 0L bits |> Int64Source.OpaqueHashBits, counters)

    let shr
        (reason : string)
        (i : Int64Source)
        (shift : int)
        (counters : PointerHashState)
        : Int64Source * PointerHashState
        =
        match i with
        | Int64Source.Verbatim i -> i >>> shift |> Int64Source.Verbatim, counters
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith $"TODO: shr (%s{reason}) on SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            bits >>> shift |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits bits -> bits >>> shift |> Int64Source.OpaqueHashBits, counters

    let shrUn
        (reason : string)
        (i : Int64Source)
        (shift : int)
        (counters : PointerHashState)
        : Int64Source * PointerHashState
        =
        // `open Checked` shadows `uint64` / `int64` with their overflow-checking
        // versions; an unsigned right shift needs the unchecked tag-flip, since a
        // negative int64 has the sign bit set and `Checked.uint64` rejects that.
        let unsignedShift (bits : int64) : int64 =
            Operators.uint64 bits >>> shift |> Operators.int64

        match i with
        | Int64Source.Verbatim i -> unsignedShift i |> Int64Source.Verbatim, counters
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith $"TODO: shrUn (%s{reason}) on SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            unsignedShift bits |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits bits -> unsignedShift bits |> Int64Source.OpaqueHashBits, counters

    let shl
        (reason : string)
        (i : Int64Source)
        (shift : int)
        (counters : PointerHashState)
        : Int64Source * PointerHashState
        =
        match i with
        | Int64Source.Verbatim i -> i <<< shift |> Int64Source.Verbatim, counters
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith $"TODO: shl (%s{reason}) on SyntheticCrossArrayOffset"
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            bits <<< shift |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits bits -> bits <<< shift |> Int64Source.OpaqueHashBits, counters

    let bitNot (reason : string) (i : Int64Source) (counters : PointerHashState) : Int64Source * PointerHashState =
        match i with
        | Int64Source.Verbatim i -> Int64Source.Verbatim ~~~i, counters
        | Int64Source.WidenedNativeInt (src, _) ->
            let bits, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            ~~~bits |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits bits -> ~~~bits |> Int64Source.OpaqueHashBits, counters
        | Int64Source.SyntheticCrossArrayOffset _ -> failwith $"TODO: bitNot (%s{reason}) on SyntheticCrossArrayOffset"

    let bitAnd
        (reason : string)
        (i1 : Int64Source)
        (i2 : Int64Source)
        (counters : PointerHashState)
        : Int64Source * PointerHashState
        =
        match i1, i2 with
        | Int64Source.Verbatim a, Int64Source.Verbatim b -> a &&& b |> Int64Source.Verbatim, counters
        | Int64Source.WidenedNativeInt (src1, _), Int64Source.WidenedNativeInt (src2, _) ->
            let a, counters = PointerHashSynthesis.materialiseHashBits reason src1 counters
            let b, counters = PointerHashSynthesis.materialiseHashBits reason src2 counters
            a &&& b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.WidenedNativeInt (src, _), Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.WidenedNativeInt (src, _) ->
            let a, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            a &&& b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> a &&& b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> a &&& b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.WidenedNativeInt (src, _)
        | Int64Source.WidenedNativeInt (src, _), Int64Source.OpaqueHashBits a ->
            let b, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            a &&& b |> Int64Source.OpaqueHashBits, counters
        | _, _ -> failwith $"TODO: bitAnd (%s{reason}) on SyntheticCrossArrayOffset"

    let bitOr
        (reason : string)
        (i1 : Int64Source)
        (i2 : Int64Source)
        (counters : PointerHashState)
        : Int64Source * PointerHashState
        =
        match i1, i2 with
        | Int64Source.Verbatim a, Int64Source.Verbatim b -> a ||| b |> Int64Source.Verbatim, counters
        | Int64Source.WidenedNativeInt (src1, _), Int64Source.WidenedNativeInt (src2, _) ->
            let a, counters = PointerHashSynthesis.materialiseHashBits reason src1 counters
            let b, counters = PointerHashSynthesis.materialiseHashBits reason src2 counters
            a ||| b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.WidenedNativeInt (src, _), Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.WidenedNativeInt (src, _) ->
            let a, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            a ||| b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> a ||| b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> a ||| b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.WidenedNativeInt (src, _)
        | Int64Source.WidenedNativeInt (src, _), Int64Source.OpaqueHashBits a ->
            let b, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            a ||| b |> Int64Source.OpaqueHashBits, counters
        | _, _ -> failwith $"TODO: bitOr (%s{reason}) on SyntheticCrossArrayOffset"

    let bitXor
        (reason : string)
        (i1 : Int64Source)
        (i2 : Int64Source)
        (counters : PointerHashState)
        : Int64Source * PointerHashState
        =
        match i1, i2 with
        | Int64Source.Verbatim a, Int64Source.Verbatim b -> a ^^^ b |> Int64Source.Verbatim, counters
        | Int64Source.WidenedNativeInt (src1, _), Int64Source.WidenedNativeInt (src2, _) ->
            let a, counters = PointerHashSynthesis.materialiseHashBits reason src1 counters
            let b, counters = PointerHashSynthesis.materialiseHashBits reason src2 counters
            a ^^^ b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.WidenedNativeInt (src, _), Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.WidenedNativeInt (src, _) ->
            let a, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            a ^^^ b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.OpaqueHashBits b -> a ^^^ b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b
        | Int64Source.Verbatim b, Int64Source.OpaqueHashBits a -> a ^^^ b |> Int64Source.OpaqueHashBits, counters
        | Int64Source.OpaqueHashBits a, Int64Source.WidenedNativeInt (src, _)
        | Int64Source.WidenedNativeInt (src, _), Int64Source.OpaqueHashBits a ->
            let b, counters = PointerHashSynthesis.materialiseHashBits reason src counters
            a ^^^ b |> Int64Source.OpaqueHashBits, counters
        | _, _ -> failwith $"TODO: bitXor (%s{reason}) on SyntheticCrossArrayOffset"

    /// Returns None if we can't decide whether this number is nonnegative.
    let isNonnegative (i : Int64Source) : bool option =
        match i with
        | Int64Source.Verbatim i -> Some (i >= 0L)
        | Int64Source.WidenedNativeInt (src, _) -> Some (NativeIntSource.isNonnegative src)
        | Int64Source.OpaqueHashBits bits -> Some (bits >= 0L)
        | _ -> failwith "TODO: SyntheticCrossArrayOffset"

    /// Signed comparison of two `Int64Source` values, treating each as the
    /// signed int64 it represents. Returns negative / zero / positive in the
    /// `compare` convention. `Int64Source` does not support structural
    /// comparison (it contains a `NativeIntSource`, which is
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
        | Int64Source.OpaqueHashBits a, Int64Source.Verbatim b -> compare a b
        | Int64Source.Verbatim a, Int64Source.OpaqueHashBits b -> compare a b
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
    | UInt8 of UInt8Source
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

    /// `true` iff two numerics are the same CLR numeric kind, ignoring the values they hold.
    ///
    /// Not "the same width": `Int32` and `Float32` are both four bytes and are not interchangeable,
    /// and `Int64`/`NativeInt` differ in provenance tracking even though both are eight. Callers
    /// use this to decide whether a storage cell may stand in for a value without a bytewise
    /// reinterpret, so anything short of same-kind would silently change what a cell claims to
    /// hold.
    static member SameKind (a : CliNumericType) (b : CliNumericType) : bool =
        match a, b with
        | CliNumericType.Int32 _, CliNumericType.Int32 _
        | CliNumericType.Int64 _, CliNumericType.Int64 _
        | CliNumericType.NativeInt _, CliNumericType.NativeInt _
        | CliNumericType.NativeFloat _, CliNumericType.NativeFloat _
        | CliNumericType.Int8 _, CliNumericType.Int8 _
        | CliNumericType.Int16 _, CliNumericType.Int16 _
        | CliNumericType.UInt8 _, CliNumericType.UInt8 _
        | CliNumericType.UInt16 _, CliNumericType.UInt16 _
        | CliNumericType.Float32 _, CliNumericType.Float32 _
        | CliNumericType.Float64 _, CliNumericType.Float64 _ -> true
        | CliNumericType.Int32 _, _
        | CliNumericType.Int64 _, _
        | CliNumericType.NativeInt _, _
        | CliNumericType.NativeFloat _, _
        | CliNumericType.Int8 _, _
        | CliNumericType.Int16 _, _
        | CliNumericType.UInt8 _, _
        | CliNumericType.UInt16 _, _
        | CliNumericType.Float32 _, _
        | CliNumericType.Float64 _, _ -> false

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
            | NativeIntSource.TypeDescPtr _ -> failwith "refusing to express TypeDescPtr as bytes"
            | NativeIntSource.MethodTablePtr _ -> failwith "refusing to express MethodTablePtr as bytes"
            | NativeIntSource.MethodTableAuxiliaryDataPtr _ ->
                failwith "refusing to express MethodTableAuxiliaryDataPtr as bytes"
            | NativeIntSource.PerInstInfoPtr _ -> failwith "refusing to express PerInstInfoPtr as bytes"
            | NativeIntSource.PerInstDictPtr _ -> failwith "refusing to express PerInstDictPtr as bytes"
            | NativeIntSource.GcHandlePtr _ -> failwith "refusing to express GcHandlePtr as bytes"
            | NativeIntSource.EventPipeProviderPtr _ -> failwith "refusing to express EventPipeProviderPtr as bytes"
            | NativeIntSource.EventPipeEventPtr _ -> failwith "refusing to express EventPipeEventPtr as bytes"
            | NativeIntSource.LowLevelMonitorPtr _ -> failwith "refusing to express LowLevelMonitorPtr as bytes"
            | NativeIntSource.WaitHandlePtr _ -> failwith "refusing to express WaitHandlePtr as bytes"
            | NativeIntSource.AssemblyHandle _ -> failwith "refusing to express AssemblyHandle as bytes"
            | NativeIntSource.ModuleHandle _ -> failwith "refusing to express ModuleHandle as bytes"
            | NativeIntSource.MetadataImportHandle _ -> failwith "refusing to express MetadataImportHandle as bytes"
            | NativeIntSource.OpaqueHashBits bits ->
                failwith $"refusing to convert synthesised pointer-hash bits 0x%x{bits} (native int) to bytes"
        | CliNumericType.NativeFloat f -> BitConverter.GetBytes f
        // Overload resolution for sbyte/byte silently picks
        // `BitConverter.GetBytes(System.Half)` (2 bytes) in net8/net9; build
        // the single-byte result explicitly to stay faithful to CLR layout.
        // Route a negative sbyte through int32 + mask to preserve bit
        // pattern without hitting the checked-conversion throw.
        | CliNumericType.Int8 i -> [| byte (int i &&& 0xFF) |]
        | CliNumericType.Int16 i -> BitConverter.GetBytes i
        | CliNumericType.UInt8 i -> [| UInt8Source.value "CliNumericType.ToBytes" i |]
        | CliNumericType.UInt16 i -> BitConverter.GetBytes i
        | CliNumericType.Float32 i -> BitConverter.GetBytes i
        | CliNumericType.Float64 i -> BitConverter.GetBytes i
