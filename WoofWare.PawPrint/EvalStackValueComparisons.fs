namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module EvalStackValueComparisons =

    // `Unsafe.AsRef<T>((void*)bits)` placeholders carry a literal bit pattern
    // in a byref-shaped wrapper. CIL treats pointers and `nativeint` as the
    // same stack type, so C# emits no `conv.i`/`conv.u` for casts like
    // `(nint)(byte*)p` — the placeholder ManagedPointer arrives at
    // numeric-shape operations (clt/cgt/clt_un/cgt_un/ceq) verbatim. For
    // those operations the placeholder *is* its bits, so normalise both
    // possible shapes (`ManagedPointer placeholder` and the
    // post-`conv.u` form `NativeInt (ManagedPointer placeholder)`) to
    // `NativeInt (Verbatim bits)` before dispatching. The byref-shape
    // arms downstream stay honest: they only see real byrefs.
    let private unwrapPlaceholderForBitComparison (v : EvalStackValue) : EvalStackValue =
        match v with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)) ->
            EvalStackValue.NativeInt (NativeIntSource.Verbatim bits)
        | _ -> v

    let clt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match var1, var2 with
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> Int64Source.compareSigned var1 var2 < 0
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 < var2
        | EvalStackValue.NullObjectRef, _
        | _, EvalStackValue.NullObjectRef ->
            failwith $"Clt instruction invalid for comparing object refs, {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            failwith $"Clt instruction invalid for comparing object refs, {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, other -> failwith $"invalid comparison, ref %O{var1} vs %O{other}"
        | other, EvalStackValue.ObjectRef var2 -> failwith $"invalid comparison, %O{other} vs ref %O{var2}"
        | EvalStackValue.Float i, other -> failwith $"invalid comparison, float %f{i} vs %O{other}"
        | other, EvalStackValue.Float i -> failwith $"invalid comparison, %O{other} vs float %f{i}"
        | EvalStackValue.Int64 i, other -> failwith $"invalid comparison, int64 %O{i} vs %O{other}"
        | other, EvalStackValue.Int64 i -> failwith $"invalid comparison, %O{other} vs int64 %O{i}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 < var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: Clt Int32 vs NativeInt comparison unimplemented"
        | EvalStackValue.Int32 i, other -> failwith $"invalid comparison, int32 %i{i} vs %O{other}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: Clt NativeInt vs Int32 comparison unimplemented"
        | other, EvalStackValue.Int32 var2 -> failwith $"invalid comparison, {other} vs int32 {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> NativeIntSource.isLess var1 var2
        | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
        | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
            failwith "TODO: Clt ManagedPointer vs NativeInt comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
            failwith "TODO: Clt ManagedPointer vs ManagedPointer comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType _ ->
            failwith "TODO: Clt ManagedPointer vs UserDefinedValueType comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, NativeInt int64 ->
            failwith "TODO: Clt UserDefinedValueType vs NativeInt comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, ManagedPointer managedPointerSource ->
            failwith "TODO: Clt UserDefinedValueType vs ManagedPointer comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, UserDefinedValueType _ ->
            failwith "TODO: Clt UserDefinedValueType vs UserDefinedValueType comparison unimplemented"

    let cgt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match var1, var2 with
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> Int64Source.compareSigned var1 var2 > 0
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 > var2
        | EvalStackValue.NullObjectRef, _
        | _, EvalStackValue.NullObjectRef ->
            failwith $"Cgt instruction invalid for comparing object refs, {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            failwith $"Cgt instruction invalid for comparing object refs, {var1} vs {var2}"
        | EvalStackValue.ObjectRef var1, other -> failwith $"invalid comparison, ref %O{var1} vs %O{other}"
        | other, EvalStackValue.ObjectRef var2 -> failwith $"invalid comparison, %O{other} vs ref %O{var2}"
        | EvalStackValue.Float i, other -> failwith $"invalid comparison, float %f{i} vs %O{other}"
        | other, EvalStackValue.Float i -> failwith $"invalid comparison, %O{other} vs float %f{i}"
        | EvalStackValue.Int64 i, other -> failwith $"invalid comparison, int64 %O{i} vs %O{other}"
        | other, EvalStackValue.Int64 i -> failwith $"invalid comparison, %O{other} vs int64 %O{i}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 > var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: Cgt Int32 vs NativeInt comparison unimplemented"
        | EvalStackValue.Int32 i, other -> failwith $"invalid comparison, int32 %i{i} vs %O{other}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: Cgt NativeInt vs Int32 comparison unimplemented"
        | other, EvalStackValue.Int32 var2 -> failwith $"invalid comparison, {other} vs int32 {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 -> NativeIntSource.isLess var2 var1
        | EvalStackValue.NativeInt var1, other -> failwith $"invalid comparison, nativeint {var1} vs %O{other}"
        | EvalStackValue.ManagedPointer managedPointerSource, NativeInt int64 ->
            failwith "TODO: Cgt ManagedPointer vs NativeInt comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, ManagedPointer pointerSource ->
            failwith "TODO: Cgt ManagedPointer vs ManagedPointer comparison unimplemented"
        | EvalStackValue.ManagedPointer managedPointerSource, UserDefinedValueType _ ->
            failwith "TODO: Cgt ManagedPointer vs UserDefinedValueType comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, NativeInt int64 ->
            failwith "TODO: Cgt UserDefinedValueType vs NativeInt comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, ManagedPointer managedPointerSource ->
            failwith "TODO: Cgt UserDefinedValueType vs ManagedPointer comparison unimplemented"
        | EvalStackValue.UserDefinedValueType _, UserDefinedValueType _ ->
            failwith "TODO: Cgt UserDefinedValueType vs UserDefinedValueType comparison unimplemented"

    let rec cgtUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match var1, var2 with
        // A WidenedNativeInt is the int64 bit pattern of a NativeInt under our
        // 64-bit assumption, so unsigned comparison agrees with comparing the
        // underlying NativeInt directly. Rewriting here lets the NativeInt
        // arms (including the byref-vs-byref same-root case) handle every
        // mixed combination uniformly.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)), _ -> cgtUn (EvalStackValue.NativeInt src) var2
        | _, EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) -> cgtUn var1 (EvalStackValue.NativeInt src)
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> uint32 var1 > uint32 var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.Verbatim var2) ->
            uint64 var1 > uint64 var2
        // OpaqueHashBits carries an unambiguous int64 bit pattern, so unsigned
        // comparison is well-defined against any other unambiguous bit-pattern
        // source. The cast-cache bucket selection compares hashes/bucket indices
        // exactly this way.
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            uint64 var1 > uint64 var2
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.Verbatim var2)
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            uint64 var1 > uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.Verbatim var1, NativeIntSource.Verbatim var2 -> uint64 var1 > uint64 var2
            // OpaqueHashBits carries an unambiguous int64 bit pattern; unsigned
            // comparison against any other unambiguous bit-pattern source is
            // well-defined. Mirrors the Int64 OpaqueHashBits cgt.un arms above.
            | NativeIntSource.OpaqueHashBits var1, NativeIntSource.OpaqueHashBits var2 -> uint64 var1 > uint64 var2
            | NativeIntSource.OpaqueHashBits var1, NativeIntSource.Verbatim var2
            | NativeIntSource.Verbatim var1, NativeIntSource.OpaqueHashBits var2 -> uint64 var1 > uint64 var2
            // `ManagedPointer Null` is the value 0 (cf. the Verbatim-vs-Null
            // arms below), so unsigned comparison against OpaqueHashBits
            // reduces to `bits != 0` in the bits-on-the-left direction and
            // `false` in the Null-on-the-left direction.
            | NativeIntSource.OpaqueHashBits bits, NativeIntSource.ManagedPointer ManagedPointerSource.Null ->
                bits <> 0L
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.OpaqueHashBits _ -> false
            | NativeIntSource.Verbatim var1, NativeIntSource.SyntheticCrossArrayOffset var2 ->
                if var1 >= 0L then
                    SyntheticCrossArrayOffset.cltVerbatim var2 var1
                else
                    failwith "TODO: didn't want to think about negative ints yet"
            | NativeIntSource.SyntheticCrossArrayOffset var1, NativeIntSource.Verbatim var2 ->
                if var2 >= 0L then
                    SyntheticCrossArrayOffset.cgtVerbatim var1 var2
                else
                    failwith "TODO: didn't want to think about negative ints yet"
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null,
              NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.ManagedPointer _ -> false
            | NativeIntSource.ManagedPointer _, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> true
            | NativeIntSource.ManagedPointer p1, NativeIntSource.ManagedPointer p2 ->
                // Spec III.3.4: cgt.un on managed pointers is unsigned address
                // comparison. We strengthen this to "must share a storage
                // container": within the same storage the address ordering is
                // well-defined; across distinct containers there is no
                // defensible answer in our model. The helper returns the
                // sign of `addr(p2) - addr(p1)`, so `var1 > var2` corresponds
                // to a negative delta.
                match ManagedPointerSource.tryByteAddressDeltaSign p1 p2 with
                | Some sign -> sign < 0
                | None -> failwith $"refusing to cgt.un byrefs without a common root: %O{p1} vs %O{p2}"
            // GC handle addresses are minted from 1 upwards (see
            // GcHandleRegistry.empty), so a GcHandlePtr is never zero. The
            // common idiom emitting cgt.un against zero is a non-null check on
            // the handle, which must report true; the symmetric direction is
            // never strictly greater.
            | NativeIntSource.GcHandlePtr _, NativeIntSource.Verbatim 0L -> true
            | NativeIntSource.Verbatim 0L, NativeIntSource.GcHandlePtr _ -> false
            // `ManagedPointer Null` denotes the value 0 — it's the
            // representation `cliTypeZeroOf` plants for `IntPtr.Zero` /
            // `UIntPtr.Zero` (and any other zero-initialised nint slot). Under
            // unsigned comparison 0 is the minimum, so `Null > v` is always
            // false, and `v > Null` is just `v != 0` (which holds for both
            // strictly positive and negative-reinterpreted-as-huge-unsigned
            // values).
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.Verbatim _ -> false
            | NativeIntSource.Verbatim v, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> v <> 0L
            // A non-null managed pointer is a live address. We don't know its
            // numeric value, but it's strictly non-zero (cf. the GcHandlePtr
            // arms above): the comparison against `Verbatim 0L` is well-defined
            // even though the symmetric comparison against arbitrary non-zero
            // Verbatims is not.
            | NativeIntSource.ManagedPointer _, NativeIntSource.Verbatim 0L -> true
            | NativeIntSource.Verbatim 0L, NativeIntSource.ManagedPointer _ -> false
            | _ -> failwith $"TODO: cgt.un on non-Verbatim nativeints: %O{var1} vs %O{var2}"
        | EvalStackValue.NativeInt _, EvalStackValue.ManagedPointer var2 ->
            cgtUn var1 (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: comparison of unsigned nativeint with int32"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 <= var2)
        | EvalStackValue.Float _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt _ ->
            cgtUn (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1)) var2
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 ->
            cgtUn
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1))
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
        | EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _ -> false
        | EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> true
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            // According to the spec, cgt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct comparison between two object refs is not specified, so we treat it as a pointer comparison.
            failwith "TODO"
        | EvalStackValue.NullObjectRef, other -> failwith $"Cgt.un invalid for comparing NullObjectRef with {other}"
        | EvalStackValue.ObjectRef _, other -> failwith $"Cgt.un invalid for comparing ObjectRef with {other}"
        | other1, other2 -> failwith $"Cgt.un instruction invalid for comparing {other1} vs {other2}"

    let rec cltUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match var1, var2 with
        // See cgtUn: WidenedNativeInt collapses to NativeInt for unsigned
        // comparison under the 64-bit assumption.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)), _ -> cltUn (EvalStackValue.NativeInt src) var2
        | _, EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) -> cltUn var1 (EvalStackValue.NativeInt src)
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> uint32 var1 < uint32 var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.Verbatim var2) ->
            uint64 var1 < uint64 var2
        // See cgtUn: OpaqueHashBits is bit-pattern unambiguous and can be
        // compared unsigned against any other unambiguous Int64 source.
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            uint64 var1 < uint64 var2
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.Verbatim var2)
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            uint64 var1 < uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.Verbatim var1, NativeIntSource.Verbatim var2 -> uint64 var1 < uint64 var2
            // See cgt.un: OpaqueHashBits is bit-pattern unambiguous and can be
            // compared unsigned against any other unambiguous nativeint source.
            | NativeIntSource.OpaqueHashBits var1, NativeIntSource.OpaqueHashBits var2 -> uint64 var1 < uint64 var2
            | NativeIntSource.OpaqueHashBits var1, NativeIntSource.Verbatim var2
            | NativeIntSource.Verbatim var1, NativeIntSource.OpaqueHashBits var2 -> uint64 var1 < uint64 var2
            // See cgt.un: `ManagedPointer Null` is the value 0 under unsigned
            // comparison, so `bits < Null` is always false and `Null < bits`
            // is `bits != 0`.
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.OpaqueHashBits bits ->
                bits <> 0L
            | NativeIntSource.Verbatim var1, NativeIntSource.SyntheticCrossArrayOffset var2 ->
                if var1 >= 0L then
                    SyntheticCrossArrayOffset.cgtVerbatim var2 var1
                else
                    failwith "TODO: didn't want to think about negative ints yet"
            | NativeIntSource.SyntheticCrossArrayOffset var1, NativeIntSource.Verbatim var2 ->
                if var2 >= 0L then
                    SyntheticCrossArrayOffset.cltVerbatim var1 var2
                else
                    failwith "TODO: didn't want to think about negative ints yet"
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null,
              NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.ManagedPointer _ -> true
            | NativeIntSource.ManagedPointer _, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer p1, NativeIntSource.ManagedPointer p2 ->
                // See cgt.un for rationale; this is the symmetric case.
                // Helper returns sign of `addr(p2) - addr(p1)`; `var1 < var2`
                // corresponds to a positive delta.
                match ManagedPointerSource.tryByteAddressDeltaSign p1 p2 with
                | Some sign -> sign > 0
                | None -> failwith $"refusing to clt.un byrefs without a common root: %O{p1} vs %O{p2}"
            // Mirror of the cgt.un arms: GC handles are minted from 1 upwards,
            // so they are never zero. This makes `bge.un handle, 0`
            // (lowered through `cgeUn = not cltUn`) and direct `0 < handle`
            // checks answer truthfully instead of crashing.
            | NativeIntSource.GcHandlePtr _, NativeIntSource.Verbatim 0L -> false
            | NativeIntSource.Verbatim 0L, NativeIntSource.GcHandlePtr _ -> true
            // Mirror of the cgt.un arms: `ManagedPointer Null` is the value 0,
            // so `Null < v` is `v != 0` (unsigned), `v < Null` is always false,
            // and a non-null managed pointer is strictly greater than 0.
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.Verbatim v -> v <> 0L
            | NativeIntSource.Verbatim _, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer _, NativeIntSource.Verbatim 0L -> false
            | NativeIntSource.Verbatim 0L, NativeIntSource.ManagedPointer _ -> true
            | _, _ -> failwith $"TODO: clt.un on non-Verbatim nativeints: %O{var1} vs %O{var2}"
        | EvalStackValue.NativeInt _, EvalStackValue.ManagedPointer var2 ->
            cltUn var1 (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 ->
            failwith "TODO: comparison of unsigned nativeint with int32"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 >= var2)
        | EvalStackValue.Float _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt _ ->
            cltUn (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1)) var2
        | EvalStackValue.ManagedPointer var1, EvalStackValue.ManagedPointer var2 ->
            cltUn
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1))
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
        | EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _ -> true
        | EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.ObjectRef var1, EvalStackValue.ObjectRef var2 ->
            // According to the spec, cgt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct comparison between two object refs is not specified, so we treat it as a pointer comparison.
            failwith "TODO"
        | EvalStackValue.NullObjectRef, other -> failwith $"Clt.un invalid for comparing NullObjectRef with {other}"
        | EvalStackValue.ObjectRef _, other -> failwith $"Clt.un invalid for comparing ObjectRef with {other}"
        | other1, other2 -> failwith $"Cgt.un instruction invalid for comparing {other1} vs {other2}"

    let cgeUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 < var2)
        | EvalStackValue.Float _, _ -> failwith $"Bge.un invalid for comparing %O{var1} with %O{var2}"
        | _, EvalStackValue.Float _ -> failwith $"Bge.un invalid for comparing %O{var1} with %O{var2}"
        | _ -> not (cltUn var1 var2)

    let cleUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 > var2)
        | EvalStackValue.Float _, _ -> failwith $"Ble.un invalid for comparing %O{var1} with %O{var2}"
        | _, EvalStackValue.Float _ -> failwith $"Ble.un invalid for comparing %O{var1} with %O{var2}"
        | _ -> not (cgtUn var1 var2)

    let private ceqNormalisedManagedPointers
        (context : string)
        (p1 : NormalisedManagedPointerSource)
        (p2 : NormalisedManagedPointerSource)
        : bool
        =
        if
            ManagedPointerSource.hasNonTrailingReinterpret p1
            || ManagedPointerSource.hasNonTrailingReinterpret p2
        then
            failwith
                $"TODO (CEQ): %s{context} with `ReinterpretAs` followed by `Field` needs a bytewise layout comparison; got %O{NormalisedManagedPointerSource.value p1} vs %O{NormalisedManagedPointerSource.value p2}"

        ManagedPointerSource.stripTrailingReinterprets p1 = ManagedPointerSource.stripTrailingReinterprets p2

    let rec ceq (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2
        // Table III.4
        // Primitive-like wrappers AND enums are flattened on push (see EvalStackValue.ofCliType),
        // so UserDefinedValueType here is always a genuine user struct. ECMA leaves ceq between
        // user-defined value types unspecified, so we fail loud.
        match var1, var2 with
        | EvalStackValue.UserDefinedValueType var1, v ->
            failwith $"ceq is not specified for UserDefinedValueType: %O{var1} vs %O{v}"
        | u, EvalStackValue.UserDefinedValueType var2 ->
            failwith $"ceq is not specified for UserDefinedValueType: %O{u} vs %O{var2}"
        | EvalStackValue.Int32 var1, EvalStackValue.Int32 var2 -> var1 = var2
        | EvalStackValue.Int32 var1, EvalStackValue.NativeInt var2 -> failwith "TODO: int32 CEQ nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"bad ceq: Int32 vs {var2}"
        // WidenedNativeInt × WidenedNativeInt: route both sides through the
        // NativeInt arms so pointer-identity (TypeHandlePtr, MethodTablePtr,
        // function-pointer, …) is decided structurally.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src1, _)),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src2, _)) ->
            ceq (EvalStackValue.NativeInt src1) (EvalStackValue.NativeInt src2)
        // WidenedNativeInt × Verbatim n: the underlying source is a non-null
        // pointer shape (Null is normalised to `Verbatim 0L` by the
        // `widenedNativeInt` smart constructor), so it can't equal 0. For
        // non-zero `n` we don't know the pointer's actual numeric address —
        // the safe and previously-structurally-correct answer is still
        // `false`, but we keep that arm explicit so we can revisit if a real
        // need to compare against a known pointer value arises.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt _), EvalStackValue.Int64 (Int64Source.Verbatim _)
        | EvalStackValue.Int64 (Int64Source.Verbatim _), EvalStackValue.Int64 (Int64Source.WidenedNativeInt _) -> false
        // WidenedNativeInt × SyntheticCrossArrayOffset: a real pointer can't
        // equal a cross-storage delta; cross-array offsets are synthetic
        // markers for unrepresentable address deltas, not pointer values.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt _),
          EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _)
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt _) -> false
        // WidenedNativeInt × OpaqueHashBits is genuinely ambiguous: under the
        // counter-based synthesis scheme an identity bit op such as `x ^ 0UL`
        // or `x & ulong.MaxValue` materialises the WidenedNativeInt's bits
        // into an OpaqueHashBits carrier whose bit pattern is *exactly* what
        // the WidenedNativeInt would synthesise to — so the answer here is
        // "equal iff WidenedNativeInt's materialised bits equal the
        // OpaqueHashBits value". Producing the right answer requires reading
        // the `PointerHashCounters` map, which `ceq` does not thread today.
        // Fail loudly rather than silently returning false (which would have
        // been wrong under identity ops) or true (which would be wrong when
        // bits genuinely differ).
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt _), EvalStackValue.Int64 (Int64Source.OpaqueHashBits _)
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits _), EvalStackValue.Int64 (Int64Source.WidenedNativeInt _) ->
            failwith
                $"TODO: ceq of WidenedNativeInt vs OpaqueHashBits requires looking up the pointer's materialised hash bits via PointerHashCounters; thread state through ceq to resolve. Got %O{var1} vs %O{var2}"
        // Verbatim and OpaqueHashBits both carry unambiguous int64 bit patterns,
        // so equality is bit-pattern equality regardless of how the bits were
        // produced. Structural DU equality would incorrectly treat
        // `Verbatim 0xABCD` and `OpaqueHashBits 0xABCD` as unequal.
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.Verbatim var2) ->
            var1 = var2
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            var1 = var2
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2)
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.Verbatim var2) ->
            var1 = var2
        // SyntheticCrossArrayOffset values are equal iff they reference the
        // same source/target roots at the same offsets — that's structural
        // equality on the record. Cross-shape (offset vs verbatim/hash bits)
        // can't sensibly equal: the offset is a marker for an unrepresentable
        // address delta, not a number we can pin to a verbatim bit pattern.
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset s1),
          EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset s2) -> s1 = s2
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _), EvalStackValue.Int64 (Int64Source.Verbatim _)
        | EvalStackValue.Int64 (Int64Source.Verbatim _), EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _)
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _),
          EvalStackValue.Int64 (Int64Source.OpaqueHashBits _)
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits _),
          EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _) -> false
        | EvalStackValue.Int64 _, _ -> failwith $"bad ceq: Int64 vs {var2}"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 = var2
        | EvalStackValue.Float _, _ -> failwith $"bad ceq: Float vs {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.FunctionPointer f1, NativeIntSource.FunctionPointer f2 -> MethodInfo.NominallyEqual f1 f2
            | NativeIntSource.TypeHandlePtr f1, NativeIntSource.TypeHandlePtr f2 -> f1 = f2
            | NativeIntSource.MethodTablePtr f1, NativeIntSource.MethodTablePtr f2 -> f1 = f2
            | NativeIntSource.MethodTableAuxiliaryDataPtr f1, NativeIntSource.MethodTableAuxiliaryDataPtr f2 -> f1 = f2
            | NativeIntSource.MethodHandlePtr f1, NativeIntSource.MethodHandlePtr f2 -> f1 = f2
            | NativeIntSource.FieldHandlePtr f1, NativeIntSource.FieldHandlePtr f2 -> f1 = f2
            | NativeIntSource.AssemblyHandle f1, NativeIntSource.AssemblyHandle f2 -> f1 = f2
            | NativeIntSource.ModuleHandle f1, NativeIntSource.ModuleHandle f2 -> f1 = f2
            | NativeIntSource.MetadataImportHandle f1, NativeIntSource.MetadataImportHandle f2 -> f1 = f2
            | NativeIntSource.GcHandlePtr f1, NativeIntSource.GcHandlePtr f2 -> f1 = f2
            | NativeIntSource.EventPipeProviderPtr f1, NativeIntSource.EventPipeProviderPtr f2 -> f1 = f2
            | NativeIntSource.EventPipeEventPtr f1, NativeIntSource.EventPipeEventPtr f2 -> f1 = f2
            | NativeIntSource.LowLevelMonitorPtr f1, NativeIntSource.LowLevelMonitorPtr f2 -> f1 = f2
            | NativeIntSource.Verbatim f1, NativeIntSource.Verbatim f2 -> f1 = f2
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.SyntheticCrossArrayOffset _
            | NativeIntSource.Verbatim _, NativeIntSource.SyntheticCrossArrayOffset _
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.Verbatim _ -> failwith "TODO: ceq"
            // Synthesised pointer-hash bits compare as raw int64 bit patterns:
            // they're deterministic numeric content, so structural equality on
            // the bits is correct. Across-tag (vs Verbatim) the same applies.
            | NativeIntSource.OpaqueHashBits b1, NativeIntSource.OpaqueHashBits b2 -> b1 = b2
            | NativeIntSource.OpaqueHashBits b, NativeIntSource.Verbatim v
            | NativeIntSource.Verbatim v, NativeIntSource.OpaqueHashBits b -> b = v
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.SyntheticCrossArrayOffset _
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.OpaqueHashBits _ ->
                failwith "TODO: ceq of synthesised hash bits against cross-array offset"
            // OpaqueHashBits vs a real handle pointer is genuinely ambiguous:
            // an identity bit op such as `((ulong)h) ^ 0UL` or `((ulong)h) | 0UL`
            // round-trips the handle's materialised bits into an
            // OpaqueHashBits carrier, so the answer depends on whether those
            // bits equal the handle's synthesised address. Resolving correctly
            // requires reading the `PointerHashCounters` map, which `ceq` does
            // not thread today. Fail loudly rather than fall through to the
            // handle-kind catch-all (which would return a fixed `false` even
            // for the same handle). Mirrors the Int64
            // WidenedNativeInt × OpaqueHashBits case above.
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.FunctionPointer _
            | NativeIntSource.FunctionPointer _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.TypeHandlePtr _
            | NativeIntSource.TypeHandlePtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.MethodTablePtr _
            | NativeIntSource.MethodTablePtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.MethodTableAuxiliaryDataPtr _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.MethodHandlePtr _
            | NativeIntSource.MethodHandlePtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.FieldHandlePtr _
            | NativeIntSource.FieldHandlePtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.AssemblyHandle _
            | NativeIntSource.AssemblyHandle _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.ModuleHandle _
            | NativeIntSource.ModuleHandle _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.MetadataImportHandle _
            | NativeIntSource.MetadataImportHandle _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.GcHandlePtr _
            | NativeIntSource.GcHandlePtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.EventPipeProviderPtr _
            | NativeIntSource.EventPipeProviderPtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.EventPipeEventPtr _
            | NativeIntSource.EventPipeEventPtr _, NativeIntSource.OpaqueHashBits _
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.LowLevelMonitorPtr _
            | NativeIntSource.LowLevelMonitorPtr _, NativeIntSource.OpaqueHashBits _ ->
                failwith
                    $"TODO (CEQ): synthesised hash bits vs handle pointer requires materialising the handle's bits through PointerHashCounters; got {var1} vs {var2}"
            // CoreCLR's TypeHandle wraps either a MethodTable* (when !IsTypeDesc) or a tagged
            // TypeDesc*; for non-TypeDesc handles the inner pointer IS the MethodTable address.
            // Patterns like `RuntimeHelpers.GetMethodTable(obj) == TypeHandleOf<T>().AsMethodTable()`
            // (CastHelpers, RuntimeType.IsEnum/IsDelegate) require the two encodings to compare
            // equal when they reference the same concrete type. Only Concrete and array handles
            // have MethodTables in CoreCLR; Byref/Pointer/FunctionPointer are TypeDescs and never
            // alias a MethodTablePtr (otherwise e.g. `typeof(int*)` would compare equal to a
            // MethodTablePtr synthesised for the same handle).
            | NativeIntSource.MethodTablePtr h1, NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed h2)
            | NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed h2), NativeIntSource.MethodTablePtr h1 ->
                match h2 with
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> h1 = h2
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ -> false
            | NativeIntSource.ManagedPointer f1, NativeIntSource.ManagedPointer f2 ->
                // Match the `EvalStackValue.ManagedPointer` vs `ManagedPointer`
                // arm below: trailing `ReinterpretAs` projections are address-
                // preserving, so a byref converted to a native int via
                // `conv.u` / `Unsafe.AsPointer` must compare equal to the same
                // byref whose type view was changed by an `Unsafe.As`. Refuse
                // the comparison on non-trailing `ReinterpretAs` for the same
                // reason as the direct byref-ceq arm.
                ceqNormalisedManagedPointers
                    "native-int-wrapped byref"
                    (ManagedPointerSource.unsafeAssumeNormalisedForComparison f1)
                    (ManagedPointerSource.unsafeAssumeNormalisedForComparison f2)
            | NativeIntSource.Verbatim _, NativeIntSource.ManagedPointer _
            | NativeIntSource.ManagedPointer _, NativeIntSource.Verbatim _
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.ManagedPointer _
            | NativeIntSource.ManagedPointer _, NativeIntSource.SyntheticCrossArrayOffset _ ->
                let z1 = NativeIntSource.isZero var1
                let z2 = NativeIntSource.isZero var2

                if z1 && z2 then
                    true
                elif z1 <> z2 then
                    false
                else
                    failwith $"TODO (CEQ): mixed nativeint representations, {var1} vs {var2}"
            // Distinct opaque handle kinds have distinct non-null bit patterns, so never alias.
            | NativeIntSource.FunctionPointer _, _
            | _, NativeIntSource.FunctionPointer _
            | NativeIntSource.TypeHandlePtr _, _
            | _, NativeIntSource.TypeHandlePtr _
            | NativeIntSource.MethodTablePtr _, _
            | _, NativeIntSource.MethodTablePtr _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _, _
            | _, NativeIntSource.MethodTableAuxiliaryDataPtr _
            | NativeIntSource.MethodHandlePtr _, _
            | _, NativeIntSource.MethodHandlePtr _
            | NativeIntSource.FieldHandlePtr _, _
            | _, NativeIntSource.FieldHandlePtr _
            | NativeIntSource.AssemblyHandle _, _
            | _, NativeIntSource.AssemblyHandle _
            | NativeIntSource.ModuleHandle _, _
            | _, NativeIntSource.ModuleHandle _
            | NativeIntSource.MetadataImportHandle _, _
            | _, NativeIntSource.MetadataImportHandle _
            | NativeIntSource.GcHandlePtr _, _
            | _, NativeIntSource.GcHandlePtr _
            | NativeIntSource.EventPipeProviderPtr _, _
            | _, NativeIntSource.EventPipeProviderPtr _
            | NativeIntSource.EventPipeEventPtr _, _
            | _, NativeIntSource.EventPipeEventPtr _
            | NativeIntSource.LowLevelMonitorPtr _, _
            | _, NativeIntSource.LowLevelMonitorPtr _ -> false
            // OpaqueHashBits vs ManagedPointer: every other OpaqueHashBits
            // pairing is handled above (vs Verbatim/OpaqueHashBits, vs
            // SyntheticCrossArrayOffset, and vs the various handle kinds);
            // this is the remaining case. Hash bits equal a byref iff both
            // are null; non-zero hash bits vs a non-null byref is genuinely
            // ambiguous (we don't know the byref's numeric address), so
            // fail loudly rather than silently returning a fixed answer.
            // Mirrors the Verbatim × ManagedPointer arm above.
            | NativeIntSource.OpaqueHashBits _, NativeIntSource.ManagedPointer _
            | NativeIntSource.ManagedPointer _, NativeIntSource.OpaqueHashBits _ ->
                let z1 = NativeIntSource.isZero var1
                let z2 = NativeIntSource.isZero var2

                if z1 && z2 then
                    true
                elif z1 <> z2 then
                    false
                else
                    failwith $"TODO (CEQ): synthesised hash bits vs managed pointer, both non-null: {var1} vs {var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 var2 -> failwith $"TODO (CEQ): nativeint vs int32"
        | EvalStackValue.NativeInt var1, EvalStackValue.ManagedPointer var2 ->
            ceq (EvalStackValue.NativeInt var1) (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
        | EvalStackValue.NativeInt _, _ -> failwith $"bad ceq: NativeInt vs {var2}"
        | EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> true
        | EvalStackValue.ObjectRef addr1, EvalStackValue.ObjectRef addr2 -> addr1 = addr2
        | EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _
        | EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> false
        | EvalStackValue.ManagedPointer p1, EvalStackValue.ManagedPointer p2 ->
            // `ceq` on byrefs is address equality; trailing `ReinterpretAs`
            // projections are address-preserving type-view changes, so strip
            // them from both sides before comparison. A `ReinterpretAs`
            // followed by a `Field` would need a bytewise layout comparison
            // (fields at the same offset under different type views still
            // alias); we don't model that yet, so refuse rather than risk a
            // silent false negative.
            ceqNormalisedManagedPointers
                "byref"
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison p1)
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison p2)
        | EvalStackValue.ManagedPointer _, EvalStackValue.NullObjectRef
        | EvalStackValue.NullObjectRef, EvalStackValue.ManagedPointer _
        | EvalStackValue.ManagedPointer _, EvalStackValue.ObjectRef _
        | EvalStackValue.ObjectRef _, EvalStackValue.ManagedPointer _ ->
            // In CLI, ceq between O and & types is unspecified.
            // If this fires, investigate the upstream IL.
            failwith "ceq between managed pointer and object reference"
        | EvalStackValue.NullObjectRef, _ -> failwith $"bad ceq: NullObjectRef vs {var2}"
        | EvalStackValue.ObjectRef _, _ -> failwith $"bad ceq: ObjectRef vs {var2}"
        | EvalStackValue.ManagedPointer var1, EvalStackValue.NativeInt var2 ->
            ceq (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1)) (EvalStackValue.NativeInt var2)
        | EvalStackValue.ManagedPointer _, _ -> failwith $"bad ceq: ManagedPointer vs {var2}"
