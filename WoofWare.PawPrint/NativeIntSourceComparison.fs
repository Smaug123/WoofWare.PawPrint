namespace WoofWare.PawPrint

/// CEQ semantics on `NativeIntSource`, split out from `NativeIntSource` itself because
/// answering "are these synthesised hash bits this handle's address?" needs
/// `PointerHashState`, whose `canonicalKey` pattern-matches `NativeIntSource` and so cannot
/// be defined before it. One canonical implementation lives here rather than each caller
/// resolving the pairing before delegating: `ceq` and the `Interlocked.CompareExchange` CAS
/// must agree, and a function that crashed on a pairing its callers had quietly resolved
/// would be lying about its own contract.
[<RequireQualifiedAccess>]
module NativeIntSourceComparison =

    /// Are `bits` — synthesised hash bits, produced by some earlier bit-mixing operation —
    /// the address this state has assigned to `handle`?
    ///
    /// `None` from the lookup answers `false`. That is an envelope statement rather than a
    /// theorem: guest arithmetic can manufacture a number that a *later* assignment will
    /// hand out, and under the counter scheme (`(n + 1) <<< 2`) the assigned addresses are
    /// dense with stride 4, so `((ulong)hA + 4) == (ulong)hB` is exactly such a case and
    /// answers `false` here while the same comparison after `hB` has been materialised
    /// answers `true` by bit equality. The alternative — refusing — would abort on the
    /// overwhelmingly common case of comparing genuinely unrelated values, and the
    /// bit-equality arms above already have the same exposure. The honest summary is that
    /// PawPrint's synthesised addresses are small where real addresses are not, which is
    /// what makes a manufactured collision reachable at all.
    let private hashBitsEqualHandle (counters : PointerHashState) (bits : int64) (handle : NativeIntSource) : bool =
        match PointerHashSynthesis.tryExistingHashBits counters handle with
        | Some assigned -> bits = assigned
        | None -> false

    /// CEQ semantics on `NativeIntSource` pairs: matches the
    /// `native int × native int` arm of ECMA Table III.4. Distinct from the
    /// type's structural `Equals` because (a) `Verbatim 0L` and
    /// `ManagedPointer Null` are both the value zero so must compare equal
    /// despite structural inequality, and (b) `MethodTablePtr` and
    /// `TypeHandlePtr` for the same handle alias (CoreCLR encodes them as the
    /// same address for non-TypeDesc types). Used by `ceq` on eval-stack
    /// native ints and by the `Interlocked.CompareExchange(ref IntPtr, …)`
    /// intrinsic, which compares the slot's contents under CEQ semantics.
    let equalsForCli (counters : PointerHashState) (a : NativeIntSource) (b : NativeIntSource) : bool =
        // `Unsafe.AsRef<T>((void*)bits)` synthesises a placeholder byref
        // carrying a literal bit pattern. C# casts between native-int and
        // pointer shapes emit no `conv.i`/`conv.u`, so a slot containing the
        // bits as a plain `Verbatim` and a value freshly produced by
        // `Unsafe.AsPointer(ref Unsafe.AsRef<T>((void*)bits))` (which arrives
        // as `ManagedPointer (NativeIntPlaceholder bits)`) must compare equal:
        // they're the same numeric value, just routed through different
        // shapes. Mirrors `unwrapPlaceholderForBitComparison` in
        // `EvalStackValueComparisons`, but at the `NativeIntSource` layer so
        // every caller (eval-stack `ceq`, Interlocked CAS, …) sees the same
        // normalised view.
        let unwrapPlaceholder (n : NativeIntSource) : NativeIntSource =
            match n with
            | NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
                NativeIntSource.Verbatim bits
            | _ -> n

        let a = unwrapPlaceholder a
        let b = unwrapPlaceholder b

        match a, b with
        | NativeIntSource.FunctionPointer f1, NativeIntSource.FunctionPointer f2 -> f1 = f2
        | NativeIntSource.TypeHandlePtr f1, NativeIntSource.TypeHandlePtr f2 -> f1 = f2
        // A `TypeDescPtr` is the same base address as the `TypeHandlePtr` it was
        // masked from, but with the tag bit clear, so it must NOT alias one: in
        // CoreCLR the two differ numerically by exactly that bit. It aliases
        // nothing else either — only TypeDesc-shaped targets have one, and those
        // have no MethodTable.
        | NativeIntSource.TypeDescPtr f1, NativeIntSource.TypeDescPtr f2 -> f1 = f2
        | NativeIntSource.MethodTablePtr f1, NativeIntSource.MethodTablePtr f2 -> f1 = f2
        | NativeIntSource.MethodTableAuxiliaryDataPtr f1, NativeIntSource.MethodTableAuxiliaryDataPtr f2 -> f1 = f2
        | NativeIntSource.PerInstInfoPtr f1, NativeIntSource.PerInstInfoPtr f2 -> f1 = f2
        | NativeIntSource.PerInstDictPtr f1, NativeIntSource.PerInstDictPtr f2 -> f1 = f2
        | NativeIntSource.MethodHandlePtr f1, NativeIntSource.MethodHandlePtr f2 -> f1 = f2
        | NativeIntSource.FieldHandlePtr f1, NativeIntSource.FieldHandlePtr f2 -> f1 = f2
        | NativeIntSource.AssemblyHandle f1, NativeIntSource.AssemblyHandle f2 -> f1 = f2
        | NativeIntSource.ModuleHandle f1, NativeIntSource.ModuleHandle f2 -> f1 = f2
        | NativeIntSource.MetadataImportHandle f1, NativeIntSource.MetadataImportHandle f2 -> f1 = f2
        // Two views of one handle are the same value only if they carry the same
        // tag: CoreLib's `GCHandle.Equals` compares the raw tagged `IntPtr`, so a
        // pinned handle does not equal the same handle with its pin marker
        // stripped.
        | NativeIntSource.GcHandlePtr (h1, tag1), NativeIntSource.GcHandlePtr (h2, tag2) -> h1 = h2 && tag1 = tag2
        | NativeIntSource.EventPipeProviderPtr f1, NativeIntSource.EventPipeProviderPtr f2 -> f1 = f2
        | NativeIntSource.EventPipeEventPtr f1, NativeIntSource.EventPipeEventPtr f2 -> f1 = f2
        | NativeIntSource.LowLevelMonitorPtr f1, NativeIntSource.LowLevelMonitorPtr f2 -> f1 = f2
        | NativeIntSource.WaitHandlePtr f1, NativeIntSource.WaitHandlePtr f2 -> f1 = f2
        | NativeIntSource.Verbatim f1, NativeIntSource.Verbatim f2 -> f1 = f2
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.SyntheticCrossArrayOffset _
        | NativeIntSource.Verbatim _, NativeIntSource.SyntheticCrossArrayOffset _
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.Verbatim _ -> failwith "TODO: ceq"
        // Synthesised pointer-hash bits compare as raw int64 bit patterns:
        // they're deterministic numeric content, so structural equality on
        // the bits is correct. Across-tag (vs Verbatim) the same applies.
        | NativeIntSource.OpaqueHashBits b1, NativeIntSource.OpaqueHashBits b2 -> b1 = b2
        | NativeIntSource.OpaqueHashBits bits, NativeIntSource.Verbatim v
        | NativeIntSource.Verbatim v, NativeIntSource.OpaqueHashBits bits -> bits = v
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.SyntheticCrossArrayOffset _
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.OpaqueHashBits _ ->
            failwith "TODO: ceq of synthesised hash bits against cross-array offset"
        // OpaqueHashBits vs a real handle pointer: an identity bit op such as
        // `((ulong)h) ^ 0UL` or `((ulong)h) | 0UL` round-trips the handle's materialised
        // bits into an OpaqueHashBits carrier, so the answer is "equal iff those bits are
        // this handle's synthesised address" — which `counters` can say exactly, without
        // assigning anything. Mirrors the Int64 WidenedNativeInt × OpaqueHashBits case in
        // `EvalStackValueComparisons.ceq`, which must agree with this one.
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.FunctionPointer _ as handle)
        | (NativeIntSource.FunctionPointer _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.TypeHandlePtr _ as handle)
        | (NativeIntSource.TypeHandlePtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.TypeDescPtr _ as handle)
        | (NativeIntSource.TypeDescPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.MethodTablePtr _ as handle)
        | (NativeIntSource.MethodTablePtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.MethodTableAuxiliaryDataPtr _ as handle)
        | (NativeIntSource.MethodTableAuxiliaryDataPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.PerInstInfoPtr _ as handle)
        | (NativeIntSource.PerInstInfoPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.PerInstDictPtr _ as handle)
        | (NativeIntSource.PerInstDictPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.MethodHandlePtr _ as handle)
        | (NativeIntSource.MethodHandlePtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.FieldHandlePtr _ as handle)
        | (NativeIntSource.FieldHandlePtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.AssemblyHandle _ as handle)
        | (NativeIntSource.AssemblyHandle _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.ModuleHandle _ as handle)
        | (NativeIntSource.ModuleHandle _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.MetadataImportHandle _ as handle)
        | (NativeIntSource.MetadataImportHandle _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.GcHandlePtr _ as handle)
        | (NativeIntSource.GcHandlePtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.EventPipeProviderPtr _ as handle)
        | (NativeIntSource.EventPipeProviderPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.EventPipeEventPtr _ as handle)
        | (NativeIntSource.EventPipeEventPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.LowLevelMonitorPtr _ as handle)
        | (NativeIntSource.LowLevelMonitorPtr _ as handle), NativeIntSource.OpaqueHashBits bits
        | NativeIntSource.OpaqueHashBits bits, (NativeIntSource.WaitHandlePtr _ as handle)
        | (NativeIntSource.WaitHandlePtr _ as handle), NativeIntSource.OpaqueHashBits bits ->
            hashBitsEqualHandle counters bits handle
        // CoreCLR's TypeHandle wraps either a MethodTable* (when !IsTypeDesc) or a tagged
        // TypeDesc*; for non-TypeDesc handles the inner pointer IS the MethodTable address.
        // Patterns like `RuntimeHelpers.GetMethodTable(obj) == TypeHandleOf<T>().AsMethodTable()`
        // (CastHelpers, RuntimeType.IsEnum/IsDelegate) require the two encodings to compare
        // equal when they reference the same concrete type. Only Concrete and array handles
        // have MethodTables in CoreCLR; Byref/Pointer/FunctionPointer are TypeDescs and never
        // alias a MethodTablePtr (otherwise e.g. `typeof(int*)` would compare equal to a
        // MethodTablePtr synthesised for the same handle). The OpenGenericTypeDefinition
        // case aliases the typedef's canonical MethodTable address with the same TypeHandle.
        | NativeIntSource.MethodTablePtr t1, NativeIntSource.TypeHandlePtr t2
        | NativeIntSource.TypeHandlePtr t2, NativeIntSource.MethodTablePtr t1 ->
            match t1, t2 with
            | RuntimeTypeHandleTarget.Closed h1, RuntimeTypeHandleTarget.Closed h2 ->
                match h2 with
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> h1 = h2
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ -> false
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition i1, RuntimeTypeHandleTarget.OpenGenericTypeDefinition i2 ->
                i1 = i2
            | RuntimeTypeHandleTarget.Closed _, RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _, RuntimeTypeHandleTarget.Closed _ ->
                // The closed instantiation has its own MT distinct from the typedef's canonical MT.
                false
            // An open constructed type has its own MethodTable too, distinct from both the
            // definition's canonical MT and any closed instantiation's, so it aliases only
            // itself. Canonicalisation in `RuntimeTypeHandleTarget.openConstructed` is what
            // makes this structural equality an identity test rather than a spelling test.
            | RuntimeTypeHandleTarget.OpenConstructed (d1, a1), RuntimeTypeHandleTarget.OpenConstructed (d2, a2) ->
                d1 = d2 && a1 = a2
            | RuntimeTypeHandleTarget.OpenConstructed _,
              (RuntimeTypeHandleTarget.Closed _ | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _)
            | (RuntimeTypeHandleTarget.Closed _ | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _),
              RuntimeTypeHandleTarget.OpenConstructed _ -> false
            // The dynamic-methods class has a MethodTable of its own, distinct from every type any
            // assembly declares — including the `<Module>` type of the very assembly it is keyed
            // by. One per scope assembly, so structural equality on that name is the identity test.
            | RuntimeTypeHandleTarget.DynamicMethodsClass a1, RuntimeTypeHandleTarget.DynamicMethodsClass a2 -> a1 = a2
            | RuntimeTypeHandleTarget.DynamicMethodsClass _,
              (RuntimeTypeHandleTarget.Closed _ | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ | RuntimeTypeHandleTarget.OpenConstructed _)
            | (RuntimeTypeHandleTarget.Closed _ | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ | RuntimeTypeHandleTarget.OpenConstructed _),
              RuntimeTypeHandleTarget.DynamicMethodsClass _ -> false
            | RuntimeTypeHandleTarget.GenericParameter _, _
            | RuntimeTypeHandleTarget.MethodGenericParameter _, _ ->
                // The *MethodTablePtr* side cannot legitimately name a generic parameter:
                // there is no MethodTable to have taken the address of. Checked ahead of the
                // TypeHandlePtr-side arm below so that a violation here is reported even when
                // both sides are generic parameters.
                failwith
                    $"CEQ: MethodTablePtr with generic-parameter target has no MethodTable identity: %O{t1} vs %O{t2}"
            | _, RuntimeTypeHandleTarget.GenericParameter _
            | _, RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                // A bare generic parameter is a TypeVarTypeDesc, so the same rule as the
                // Byref/Pointer/FunctionPointer arm above applies: it is a tagged TypeDesc
                // pointer and can never equal a MethodTable address. This is a legitimate
                // construction, not a contract violation — `CastHelpers.IsInstanceOfAny`
                // opens with `RuntimeHelpers.GetMethodTable(obj) != toTypeHnd` against a raw
                // `void*` TypeHandle that the caller is free to have obtained from
                // `typeof(List<>).GetGenericArguments()[0]`, and CoreCLR simply compares the
                // two pointers and finds them unequal.
                false
        | NativeIntSource.ManagedPointer f1, NativeIntSource.ManagedPointer f2 ->
            // Match the `EvalStackValue.ManagedPointer` vs `ManagedPointer`
            // arm below: trailing `ReinterpretAs` projections are address-
            // preserving, so a byref converted to a native int via
            // `conv.u` / `Unsafe.AsPointer` must compare equal to the same
            // byref whose type view was changed by an `Unsafe.As`. Refuse
            // the comparison on non-trailing `ReinterpretAs` for the same
            // reason as the direct byref-ceq arm.
            ManagedPointerSource.ceqNormalised
                "native-int-wrapped byref"
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison f1)
                (ManagedPointerSource.unsafeAssumeNormalisedForComparison f2)
        | NativeIntSource.Verbatim _, NativeIntSource.ManagedPointer _
        | NativeIntSource.ManagedPointer _, NativeIntSource.Verbatim _
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.ManagedPointer _
        | NativeIntSource.ManagedPointer _, NativeIntSource.SyntheticCrossArrayOffset _ ->
            let z1 = NativeIntSource.isZero a
            let z2 = NativeIntSource.isZero b

            if z1 && z2 then
                true
            elif z1 <> z2 then
                false
            else
                failwith $"TODO (CEQ): mixed nativeint representations, {a} vs {b}"
        // Distinct opaque handle kinds have distinct non-null bit patterns, so never alias.
        | NativeIntSource.FunctionPointer _, _
        | _, NativeIntSource.FunctionPointer _
        | NativeIntSource.TypeHandlePtr _, _
        | _, NativeIntSource.TypeHandlePtr _
        | NativeIntSource.TypeDescPtr _, _
        | _, NativeIntSource.TypeDescPtr _
        | NativeIntSource.MethodTablePtr _, _
        | _, NativeIntSource.MethodTablePtr _
        | NativeIntSource.MethodTableAuxiliaryDataPtr _, _
        | _, NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.PerInstInfoPtr _, _
        | _, NativeIntSource.PerInstInfoPtr _
        | NativeIntSource.PerInstDictPtr _, _
        | _, NativeIntSource.PerInstDictPtr _
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
        | _, NativeIntSource.LowLevelMonitorPtr _
        | NativeIntSource.WaitHandlePtr _, _
        | _, NativeIntSource.WaitHandlePtr _ -> false
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
            let z1 = NativeIntSource.isZero a
            let z2 = NativeIntSource.isZero b

            if z1 && z2 then
                true
            elif z1 <> z2 then
                false
            else
                failwith $"TODO (CEQ): synthesised hash bits vs managed pointer, both non-null: {a} vs {b}"
