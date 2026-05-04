namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module EvalStackValueComparisons =

    let clt (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
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
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.Verbatim var1, NativeIntSource.Verbatim var2 -> uint64 var1 > uint64 var2
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
        | EvalStackValue.Int64 _, _ -> failwith $"Cgt.un invalid for comparing %O{var1} with %O{var2}"
        | EvalStackValue.NativeInt var1, EvalStackValue.NativeInt var2 ->
            match var1, var2 with
            | NativeIntSource.Verbatim var1, NativeIntSource.Verbatim var2 -> uint64 var1 < uint64 var2
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
        | EvalStackValue.Int64 var1, EvalStackValue.Int64 var2 -> var1 = var2
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
            | NativeIntSource.Verbatim f1, NativeIntSource.Verbatim f2 -> f1 = f2
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.SyntheticCrossArrayOffset _
            | NativeIntSource.Verbatim _, NativeIntSource.SyntheticCrossArrayOffset _
            | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.Verbatim _ -> failwith "TODO: ceq"
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
            | _, NativeIntSource.GcHandlePtr _ -> false
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
