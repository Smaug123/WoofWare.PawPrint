namespace WoofWare.PawPrint

/// Which order an unsigned comparison of two byrefs asks about.
[<RequireQualifiedAccess>]
type ByrefOrderQuestion =
    /// `cgt.un`: is the left operand's address strictly above the right's?
    | LeftAbove
    /// `clt.un`: is the left operand's address strictly below the right's?
    | LeftBelow

/// What `cgt.un` or `clt.un` concluded. `Decided` is an answer; `NeedsByteLocation` is the
/// byref pair whose order structural comparison could not fix (see
/// `ByteAddressDeltaSign`), together with the `question` asked of it, for a caller with
/// `IlMachineState` to settle by byte coordinates (`StorageLocation.resolveOrder`). A caller
/// that cannot fails with `diagnostic`.
[<RequireQualifiedAccess>]
[<NoComparison>]
type UnsignedOrderOutcome =
    | Decided of bool
    | NeedsByteLocation of
        left : ManagedPointerSource *
        right : ManagedPointerSource *
        question : ByrefOrderQuestion *
        diagnostic : string

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

    /// Ask `question` of two byrefs. Spec III.3.4: `cgt.un` and `clt.un` on managed pointers
    /// are unsigned address comparison. We strengthen this to "must share a storage
    /// container": within the same storage the address ordering is well-defined; across
    /// distinct containers there is no defensible answer in our model. Structural comparison
    /// gives the sign of `addr(p2) - addr(p1)` where the byrefs' structure fixes it, so
    /// `LeftAbove` is a negative delta and `LeftBelow` a positive one; where it does not, the
    /// pair is handed on for byte-coordinate resolution.
    let private orderByrefs
        (question : ByrefOrderQuestion)
        (p1 : ManagedPointerSource)
        (p2 : ManagedPointerSource)
        : UnsignedOrderOutcome
        =
        match ManagedPointerSource.byteAddressDeltaSign p1 p2 with
        | ByteAddressDeltaSign.Decided sign ->
            match question with
            | ByrefOrderQuestion.LeftAbove -> UnsignedOrderOutcome.Decided (sign < 0)
            | ByrefOrderQuestion.LeftBelow -> UnsignedOrderOutcome.Decided (sign > 0)
        | ByteAddressDeltaSign.NeedsByteLocation (left, right, diagnostic) ->
            UnsignedOrderOutcome.NeedsByteLocation (left, right, question, diagnostic)

    /// `orderByrefs` for the stateless comparisons, which have nothing to resolve a deferral
    /// with and so refuse it.
    let private orderByrefsOrFail
        (opcode : string)
        (question : ByrefOrderQuestion)
        (p1 : ManagedPointerSource)
        (p2 : ManagedPointerSource)
        : bool
        =
        match orderByrefs question p1 p2 with
        | UnsignedOrderOutcome.Decided answer -> answer
        | UnsignedOrderOutcome.NeedsByteLocation (_, _, _, diagnostic) ->
            failwith $"refusing %s{opcode}: %s{diagnostic}"

    let clt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match var1, var2 with
        // A byref that `conv.i4` truncated has no numeric value to order: only a
        // mask can say anything about it (see `Int32Source`).
        | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer _), _
        | _, EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer _) ->
            failwith $"Clt instruction invalid for ordering a truncated managed pointer, %O{var1} vs %O{var2}"
        // Likewise a byte of a native int PawPrint models as an identity: it names a position in
        // that identity rather than holding a number, so there is nothing to order.
        | EvalStackValue.Int32 (Int32Source.NativeIntByte _), _
        | _, EvalStackValue.Int32 (Int32Source.NativeIntByte _) ->
            failwith $"Clt instruction invalid for ordering a byte of an unmodelled native int, %O{var1} vs %O{var2}"
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
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            var1 < var2
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.NativeInt var2 ->
            failwith "TODO: Clt Int32 vs NativeInt comparison unimplemented"
        | EvalStackValue.Int32 (Int32Source.Verbatim i), other ->
            failwith $"invalid comparison, int32 %i{i} vs %O{other}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            failwith "TODO: Clt NativeInt vs Int32 comparison unimplemented"
        | other, EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            failwith $"invalid comparison, {other} vs int32 {var2}"
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
        // A byref that `conv.i4` truncated has no numeric value to order: only a
        // mask can say anything about it (see `Int32Source`).
        | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer _), _
        | _, EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer _) ->
            failwith $"Cgt instruction invalid for ordering a truncated managed pointer, %O{var1} vs %O{var2}"
        // Likewise a byte of a native int PawPrint models as an identity: it names a position in
        // that identity rather than holding a number, so there is nothing to order.
        | EvalStackValue.Int32 (Int32Source.NativeIntByte _), _
        | _, EvalStackValue.Int32 (Int32Source.NativeIntByte _) ->
            failwith $"Cgt instruction invalid for ordering a byte of an unmodelled native int, %O{var1} vs %O{var2}"
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
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            var1 > var2
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.NativeInt var2 ->
            failwith "TODO: Cgt Int32 vs NativeInt comparison unimplemented"
        | EvalStackValue.Int32 (Int32Source.Verbatim i), other ->
            failwith $"invalid comparison, int32 %i{i} vs %O{other}"
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            failwith "TODO: Cgt NativeInt vs Int32 comparison unimplemented"
        | other, EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            failwith $"invalid comparison, {other} vs int32 {var2}"
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

    /// Ordered "less than or equal". For floats this is IEEE `<=` (NaN ⇒ false), which is the
    /// correct ECMA-335 ordered ble semantics.
    let cle (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        // Non-float types defer to `not (cgt v1 v2)` — note that for floats this would be
        // wrong (`not cgt` is the *unordered* ble, since `cgt(NaN, _)` is false), so the
        // Float × Float arm overrides explicitly. Cross-type (Float vs Int / NativeInt) is
        // inherited from `cgt`'s "invalid comparison" failwith.
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 <= var2
        | _ -> not (cgt var1 var2)

    /// Ordered "greater than or equal". Float × Float uses IEEE `>=` (NaN ⇒ false).
    let cge (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match var1, var2 with
        // Mirrors `cle`: other types defer to `not (clt v1 v2)`, with the Float × Float
        // arm overriding explicitly. Cross-type guards are inherited from `clt`.
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> var1 >= var2
        | _ -> not (clt var1 var2)

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
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            uint32 var1 > uint32 var2
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.NativeInt var2 ->
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
                // var1 >u var2  ⟺  var2 <u var1
                SyntheticCrossArrayOffset.cltUnVerbatim var2 var1
            | NativeIntSource.SyntheticCrossArrayOffset var1, NativeIntSource.Verbatim var2 ->
                SyntheticCrossArrayOffset.cgtUnVerbatim var1 var2
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null,
              NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.ManagedPointer _ -> false
            | NativeIntSource.ManagedPointer _, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> true
            | NativeIntSource.ManagedPointer p1, NativeIntSource.ManagedPointer p2 ->
                orderByrefsOrFail "cgt.un" ByrefOrderQuestion.LeftAbove p1 p2
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
        | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
            failwith $"TODO: cgt.un comparing a native int with an int32: %O{var1} vs %O{var2}"
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
            // A direct ordering of two object refs is unspecified, so no answer is implemented.
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
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            uint32 var1 < uint32 var2
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.NativeInt var2 ->
            failwith "TODO: comparison of unsigned int32 with nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"Clt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.Verbatim var2) ->
            uint64 var1 < uint64 var2
        // See cgtUn: OpaqueHashBits is bit-pattern unambiguous and can be
        // compared unsigned against any other unambiguous Int64 source.
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            uint64 var1 < uint64 var2
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits var1), EvalStackValue.Int64 (Int64Source.Verbatim var2)
        | EvalStackValue.Int64 (Int64Source.Verbatim var1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits var2) ->
            uint64 var1 < uint64 var2
        | EvalStackValue.Int64 _, _ -> failwith $"Clt.un invalid for comparing %O{var1} with %O{var2}"
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
                // var1 <u var2  ⟺  var2 >u var1
                SyntheticCrossArrayOffset.cgtUnVerbatim var2 var1
            | NativeIntSource.SyntheticCrossArrayOffset var1, NativeIntSource.Verbatim var2 ->
                SyntheticCrossArrayOffset.cltUnVerbatim var1 var2
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null,
              NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.ManagedPointer _ -> true
            | NativeIntSource.ManagedPointer _, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> false
            | NativeIntSource.ManagedPointer p1, NativeIntSource.ManagedPointer p2 ->
                orderByrefsOrFail "clt.un" ByrefOrderQuestion.LeftBelow p1 p2
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
        | EvalStackValue.NativeInt _, EvalStackValue.Int32 _ ->
            failwith $"TODO: clt.un comparing a native int with an int32: %O{var1} vs %O{var2}"
        | EvalStackValue.Float var1, EvalStackValue.Float var2 -> not (var1 >= var2)
        | EvalStackValue.Float _, _ -> failwith $"Clt.un invalid for comparing %O{var1} with %O{var2}"
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
            // According to the spec, clt.un is verifiable on ObjectRefs and is used to compare with null.
            // A direct ordering of two object refs is unspecified, so no answer is implemented.
            failwith "TODO"
        | EvalStackValue.NullObjectRef, other -> failwith $"Clt.un invalid for comparing NullObjectRef with {other}"
        | EvalStackValue.ObjectRef _, other -> failwith $"Clt.un invalid for comparing ObjectRef with {other}"
        | other1, other2 -> failwith $"Clt.un instruction invalid for comparing {other1} vs {other2}"

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

    let rec ceq (counters : PointerHashState) (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
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
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            var1 = var2
        | EvalStackValue.Int32 (Int32Source.Verbatim var1), EvalStackValue.NativeInt var2 ->
            failwith "TODO: int32 CEQ nativeint"
        | EvalStackValue.Int32 _, _ -> failwith $"bad ceq: Int32 vs {var2}"
        // WidenedNativeInt × WidenedNativeInt: route both sides through the
        // NativeInt arms so pointer-identity (TypeHandlePtr, MethodTablePtr,
        // function-pointer, …) is decided structurally.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src1, _)),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src2, _)) ->
            ceq counters (EvalStackValue.NativeInt src1) (EvalStackValue.NativeInt src2)
        // WidenedNativeInt × Verbatim n: the underlying source is a non-null
        // pointer shape (Null is normalised to `Verbatim 0L` by the
        // `widenedNativeInt` smart constructor), so it can't equal 0. For
        // non-zero `n` the answer is *deferred*, not unknowable: the same
        // `tryExistingHashBits` lookup the OpaqueHashBits arm below performs
        // would decide it. It is left at `false` because an untagged integer
        // cannot be told apart from a number the guest simply computed, and
        // that policy belongs with the change that first makes a handle
        // reachable as untagged full-width bits (giving handles a byte
        // image). Nothing can produce such a value today.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt _), EvalStackValue.Int64 (Int64Source.Verbatim _)
        | EvalStackValue.Int64 (Int64Source.Verbatim _), EvalStackValue.Int64 (Int64Source.WidenedNativeInt _) -> false
        // WidenedNativeInt × SyntheticCrossArrayOffset: a real pointer can't
        // equal a cross-storage delta; cross-array offsets are synthetic
        // markers for unrepresentable address deltas, not pointer values.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt _),
          EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _)
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset _),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt _) -> false
        // WidenedNativeInt × OpaqueHashBits: under the counter-based synthesis scheme an
        // identity bit op such as `x ^ 0UL` or `x & ulong.MaxValue` materialises the
        // WidenedNativeInt's bits into an OpaqueHashBits carrier whose bit pattern is
        // *exactly* what the WidenedNativeInt would synthesise to. So the answer is "equal
        // iff those bits are this pointer's synthesised address", which `counters` can say
        // exactly — by lookup, never by assigning, so a comparison still cannot perturb the
        // numbering (which is what `ContextSwitchPrior` bands `Ceq` on). Delegated so that
        // this and the native-int-width form give one answer.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)),
          EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits)
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            NativeIntSourceComparison.equalsForCli counters (NativeIntSource.OpaqueHashBits bits) src
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
            NativeIntSourceComparison.equalsForCli counters var1 var2
        | EvalStackValue.NativeInt var1, EvalStackValue.Int32 (Int32Source.Verbatim var2) ->
            failwith $"TODO (CEQ): nativeint vs int32"
        | EvalStackValue.NativeInt var1, EvalStackValue.ManagedPointer var2 ->
            ceq
                counters
                (EvalStackValue.NativeInt var1)
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var2))
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
            ManagedPointerSource.ceqNormalised
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
            ceq
                counters
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer var1))
                (EvalStackValue.NativeInt var2)
        | EvalStackValue.ManagedPointer _, _ -> failwith $"bad ceq: ManagedPointer vs {var2}"

    /// `ceq`, but returning the byref case's deferral rather than failing on it, so a caller
    /// with `IlMachineState` can resolve it to byte coordinates (`StorageLocation.resolveCeq`).
    ///
    /// Only the byref-vs-byref arm defers. The limit: a byref compared against a
    /// `NativeInt` re-enters `ceq` proper and so still fails loudly on an undecidable pair,
    /// because that recursion routes through `NativeIntSourceComparison` rather than through
    /// this function. Widening that is its own change.
    let ceqDeferred (counters : PointerHashState) (var1 : EvalStackValue) (var2 : EvalStackValue) : CeqOutcome =
        // Must precede the match, exactly as in `ceq`. A `ManagedPointer (NativeIntPlaceholder
        // bits)` is a bit pattern wearing a byref's clothes; comparing it *structurally* against
        // a live symbolic byref would answer `false` on the shape of the two representations,
        // when the literal bits may well be that byref's actual address. Unwrapping sends the
        // pair down the native-int path, which refuses the indeterminate mixed comparison.
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match var1, var2 with
        | EvalStackValue.ManagedPointer p1, EvalStackValue.ManagedPointer p2 ->
            ManagedPointerSource.ceqNormalisedDeferred
                "byref"
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison p1)
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison p2)
        | _ -> CeqOutcome.Decided (ceq counters var1 var2)

    /// The byref a comparison operand carries, if it carries one. A byref reaches `cgt.un` or
    /// `clt.un` as itself, as the native int `conv.u` or `conv.i` made of it (the shape C#
    /// pointer comparison produces), or as the int64 `conv.u8` or `conv.i8` made of that. These
    /// are exactly the operands the comparisons route to `orderByrefs`; a bit-pattern
    /// placeholder is not one, and the caller has already unwrapped it to its bits.
    let private tryByrefOperand (v : EvalStackValue) : ManagedPointerSource option =
        match v with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref _ as p)
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer (ManagedPointerSource.Byref _ as p))
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer (ManagedPointerSource.Byref _ as p),
                                                              _)) -> Some p
        | _ -> None

    /// `compare` (one of `cgtUn`, `cltUn`), but returning the byref pair's deferral rather than
    /// failing on it, so a caller with `IlMachineState` can resolve it to byte coordinates
    /// (`StorageLocation.resolveOrder`). Every other operand pair is decided by `compare`
    /// exactly as before.
    let private unsignedOrderDeferred
        (question : ByrefOrderQuestion)
        (compare : EvalStackValue -> EvalStackValue -> bool)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : UnsignedOrderOutcome
        =
        let var1 = unwrapPlaceholderForBitComparison var1
        let var2 = unwrapPlaceholderForBitComparison var2

        match tryByrefOperand var1, tryByrefOperand var2 with
        | Some p1, Some p2 -> orderByrefs question p1 p2
        | _ -> UnsignedOrderOutcome.Decided (compare var1 var2)

    /// `cgtUn`, deferring a byref pair it cannot order structurally: see `unsignedOrderDeferred`.
    let cgtUnDeferred (var1 : EvalStackValue) (var2 : EvalStackValue) : UnsignedOrderOutcome =
        unsignedOrderDeferred ByrefOrderQuestion.LeftAbove cgtUn var1 var2

    /// `cltUn`, deferring a byref pair it cannot order structurally: see `unsignedOrderDeferred`.
    let cltUnDeferred (var1 : EvalStackValue) (var2 : EvalStackValue) : UnsignedOrderOutcome =
        unsignedOrderDeferred ByrefOrderQuestion.LeftBelow cltUn var1 var2
