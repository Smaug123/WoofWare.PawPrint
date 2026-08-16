namespace WoofWare.PawPrint

/// Canonical identity for a pointer that may need synthesised hash bits.
/// `NativeIntSource` is projected onto this DU before counter assignment so
/// that aliasing encodings (`MethodTablePtr h` and the `TypeHandlePtr` form
/// for the same Concrete/Array shape) collapse to a single key. Verbatim
/// numeric values and managed pointers never reach this DU — they are
/// handled directly by `materialiseHashBits`.
[<RequireQualifiedAccess>]
type CanonicalPointerKey =
    /// Concrete / OneDimArrayZero / Array shapes share their MethodTable*
    /// with their TypeHandle, so both encodings collapse here. CoreCLR
    /// returns the same address from either path for these shapes; the
    /// canonical key reflects that aliasing.
    | MethodTable of ConcreteTypeHandle
    /// TypeDesc-shaped TypeHandles (Byref / Pointer / FunctionPointer) and
    /// non-Closed targets (OpenGenericTypeDefinition / GenericParameter /
    /// MethodGenericParameter) keep distinct keys.
    | TypeHandle of RuntimeTypeHandleTarget
    | MethodTableAuxiliaryData of RuntimeTypeHandleTarget
    /// A pointer to a managed method (`FunctionPointerTarget.Managed`). Projected onto
    /// comparable components because `MethodInfo` itself is neither comparable nor
    /// structurally equatable.
    | FunctionPointer of
        declaringTypeIdentity : ResolvedTypeIdentity *
        declaringTypeGenerics : ConcreteTypeHandle list *
        methodHandle : ComparableMethodDefinitionHandle *
        methodGenerics : ConcreteTypeHandle list
    /// A pointer to the runtime's `newobj` allocation helper
    /// (`FunctionPointerTarget.RuntimeAllocator`). Nullary, matching the source: the
    /// helper carries no per-type identity, so every occurrence is the same address.
    | RuntimeAllocatorFunctionPointer
    | MethodHandle of int64
    | FieldHandle of int64
    | GcHandle of GcHandleAddress
    | EventPipeProvider of int64
    | EventPipeEvent of int64
    | LowLevelMonitor of LowLevelMonitorId
    | WaitHandle of WaitHandleId
    | AssemblyHandle of string
    | ModuleHandle of string
    | MetadataImportHandle of string

/// The rule by which a canonical pointer key acquires synthesised address bits,
/// together with whatever state that rule needs. Each case owns its own state, so
/// a run cannot be carrying bookkeeping that belongs to a rule it is not using.
///
/// Which rule to use is a *choice* PawPrint has made, not a fact about CoreCLR. It
/// is a DU so that the choice is visible where it is made, and so that adding a
/// second case cannot compile until every site that has to decide between them has
/// been updated. There is exactly one case today.
///
/// The known alternative is a keyed scheme — derive the bits by hashing the
/// `CanonicalPointerKey` itself, with no counter, no assignment order and no
/// memo table at all. Its advantage is that it is immune to first-touch-order
/// desync: under `SequentialFirstTouch`, a change that materialises one extra key
/// early in a run shifts the bits of every key materialised after it, so two
/// nearly-identical runs diverge in every synthesised pointer value rather than in
/// the one that actually changed. That is a real cost when diffing runs (mutation
/// testing, delta debugging). It is not the default because CoreCLR's
/// cast-cache pipeline hashes real pointer bits whose ordering is an artefact of
/// allocation order, and `SequentialFirstTouch` reproduces that shape where a hash
/// of the identity would not. A keyed scheme would also have to establish
/// collision-freedom explicitly, which the counter gets by construction.
///
/// Whichever rule is in force is part of a run's replay contract: changing it
/// changes every synthesised pointer value the guest observes.
[<RequireQualifiedAccess>]
type PointerHashState =
    /// The nth distinct key to be materialised is assigned counter `n` and stores
    /// address bits `(n + 1) <<< 2`, so bits follow first-touch order and distinct
    /// keys cannot collide. Subsequent materialisations of the same key return the
    /// same stored bits, which is what `assigned` memoises — the bits are a
    /// function of assignment order, so they cannot be recomputed from the key.
    ///
    /// Tag bits are NOT stored: they are a view over the identity, so
    /// `materialiseHashBits` OR-s them on per source, which is what lets two
    /// differently-tagged views of one identity (a `TypeHandlePtr` and the
    /// `TypeDescPtr` masked out of it; an untagged and a tagged GC handle) share
    /// address bits and differ only in the low region. The counter scheme leaves
    /// the low 2 bits clear for exactly this, so no-collision is preserved.
    ///
    /// Distinct keys get distinct bit patterns by construction, and the assignment
    /// order is deterministic given a fixed program and scheduler; that is what
    /// makes synthesised hash bits a faithful guest-observable surrogate for real
    /// pointer bits in the CoreCLR cast-cache pipeline.
    | SequentialFirstTouch of nextCounter : uint64 * assigned : Map<CanonicalPointerKey, uint64>

[<RequireQualifiedAccess>]
module PointerHashState =
    /// A fresh fixture, assigning bits by first-touch order.
    let empty : PointerHashState =
        PointerHashState.SequentialFirstTouch (0UL, Map.empty)

[<RequireQualifiedAccess>]
module PointerHashSynthesis =

    /// Project a `NativeIntSource` into its canonical pointer key. The
    /// Concrete/OneDimArrayZero/Array alias between `MethodTablePtr` and
    /// `TypeHandlePtr (Closed _)` collapses here, so the two encodings
    /// produce identical synthesised bits — that contract is required by
    /// the `ceq`/`cgtUn`/`cltUn` paths in `EvalStackValueComparisons` and
    /// by the cast-cache hash pipeline. The same aliasing rule applies for
    /// `OpenGenericTypeDefinition`: the canonical MethodTable for the
    /// typedef is the same address as the typedef's `TypeHandle`.
    let private canonicalKey (src : NativeIntSource) : CanonicalPointerKey =
        match src with
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle) ->
            CanonicalPointerKey.MethodTable handle
        // Same reasoning as `OpenGenericTypeDefinition`: an open constructed type is a real
        // MethodTable (see `TypeHandleTag.forTarget`), so its `MethodTablePtr` and
        // `TypeHandlePtr` are one address and must share a key — the `ceq` arm in
        // `NativeIntSourceComparison.equalsForCli` says they are equal, and the synthesised bits have
        // to agree with that.
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ as target)
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.OpenConstructed _ as target)
        // Same again: the dynamic-methods class is a real MethodTable, so its `MethodTablePtr` and
        // `TypeHandlePtr` are one address and must share a key.
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.DynamicMethodsClass _ as target) ->
            CanonicalPointerKey.TypeHandle target
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.GenericParameter _ as target)
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.MethodGenericParameter _ as target) ->
            failwith
                $"PointerHashSynthesis.canonicalKey: MethodTablePtr(%O{target}) has no MethodTable identity (generic parameters are TypeDescs in CoreCLR)"
        | NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle as target) ->
            match handle with
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> CanonicalPointerKey.MethodTable handle
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> CanonicalPointerKey.TypeHandle target
        | NativeIntSource.TypeHandlePtr target -> CanonicalPointerKey.TypeHandle target
        // A `TypeDescPtr` shares its base address with the `TypeHandlePtr` it was
        // masked from — in CoreCLR they differ only in the tag bit — so it shares
        // the identity too. `lowBitsForSource` is what separates their bit
        // patterns. Only TypeDesc-shaped targets have a `TypeDescPtr`, and those
        // never collapse to a `MethodTable` key.
        | NativeIntSource.TypeDescPtr target -> CanonicalPointerKey.TypeHandle target
        | NativeIntSource.MethodTableAuxiliaryDataPtr target -> CanonicalPointerKey.MethodTableAuxiliaryData target
        | NativeIntSource.FunctionPointer (FunctionPointerTarget.Managed methodInfo) ->
            // The key is a MethodDef token, so only a declared method has one. A function pointer
            // to a synthesised method is possible — a struct-marshal stub is exactly that — but
            // nothing hashes one: the guest only compares it against null and calls through it.
            // Widening the key to admit synthesised methods is work for whichever consumer first
            // needs it, so refuse rather than mint bits that would then have to stay stable.
            match methodInfo.TryMetadata with
            | None ->
                failwith
                    $"PointerHashSynthesis.canonicalKey: %O{methodInfo} is synthesised by the runtime, so it has no MethodDef token to key on"
            | Some facts ->

            // The `Some facts` branch: a metadata method, so it is declared by a type.
            let declaringType =
                MethodOwner.requireDeclaringType "synthesising hash bits for a function pointer" methodInfo.Owner

            CanonicalPointerKey.FunctionPointer (
                declaringType.Identity,
                List.ofSeq declaringType.Generics,
                ComparableMethodDefinitionHandle.Make facts.Handle,
                List.ofSeq methodInfo.Generics
            )
        | NativeIntSource.FunctionPointer FunctionPointerTarget.RuntimeAllocator ->
            CanonicalPointerKey.RuntimeAllocatorFunctionPointer
        | NativeIntSource.FunctionPointer (FunctionPointerTarget.Dynamic handle) ->
            // A dynamic method has an identity to key on (its registry id), but nothing consumes
            // synthesised bits for one. The only guest path that reads a bound dynamic method's
            // `_methodPtr` is `Delegate.Equals` (Delegate.CoreCLR.cs:96), whose
            // `_methodPtr == d._methodPtr` is a `ceq` over native ints, which
            // `NativeIntSourceComparison.equalsForCli` answers structurally through
            // `FunctionPointerTarget.Equals` without materialising any bits at all. Minting bits
            // now would commit to keeping them stable for a consumer that does not exist.
            failwith
                $"PointerHashSynthesis.canonicalKey: %O{handle} has no synthesised address bits; nothing hashes or does arithmetic on a dynamic method's code address today, so widening CanonicalPointerKey is work for whichever consumer first needs it"
        | NativeIntSource.MethodHandlePtr id -> CanonicalPointerKey.MethodHandle id
        | NativeIntSource.FieldHandlePtr id -> CanonicalPointerKey.FieldHandle id
        // The canonical key is the handle's *identity*; its tag bits are a view
        // the guest imposed, and are folded back in by `materialiseHashBits` so
        // that two differently-tagged views of one handle differ exactly in their
        // low bits, as they would in reality.
        | NativeIntSource.GcHandlePtr (handle, _) -> CanonicalPointerKey.GcHandle handle
        | NativeIntSource.EventPipeProviderPtr id -> CanonicalPointerKey.EventPipeProvider id
        | NativeIntSource.EventPipeEventPtr id -> CanonicalPointerKey.EventPipeEvent id
        | NativeIntSource.LowLevelMonitorPtr id -> CanonicalPointerKey.LowLevelMonitor id
        | NativeIntSource.WaitHandlePtr id -> CanonicalPointerKey.WaitHandle id
        | NativeIntSource.AssemblyHandle name -> CanonicalPointerKey.AssemblyHandle name
        | NativeIntSource.ModuleHandle name -> CanonicalPointerKey.ModuleHandle name
        | NativeIntSource.MetadataImportHandle name -> CanonicalPointerKey.MetadataImportHandle name
        | NativeIntSource.Verbatim _
        | NativeIntSource.ManagedPointer _
        | NativeIntSource.SyntheticCrossArrayOffset _
        | NativeIntSource.OpaqueHashBits _
        | NativeIntSource.PerInstInfoPtr _
        | NativeIntSource.PerInstDictPtr _ ->
            failwith
                $"PointerHashSynthesis.canonicalKey: %O{src} is not a canonicalisable pointer shape; verbatim / managed-pointer / cross-array / already-synthesised values / PerInstInfo chain intermediates must be handled before reaching this function"

    /// Tag bits a pointer carries in the low, known-clear region that
    /// `canonicalKey` deliberately does not distinguish.
    ///
    /// Every low bit is a *view* over an identity, never part of the identity
    /// itself, so it is computed from the source rather than from the key. Two
    /// values sharing a key therefore get the same address bits and differ in
    /// exactly their tags — which is what makes a `TypeHandlePtr` and the
    /// `TypeDescPtr` masked out of it differ by precisely bit 1, as they do in
    /// CoreCLR. A `MethodTable*` is aligned and untagged, and every other handle
    /// kind is conventionally aligned, so both contribute nothing.
    ///
    /// The type-handle rule lives in `TypeHandleTag.forTarget`, shared with the
    /// `and` arms in `NullaryIlOp`: the two must agree, because a tag is
    /// observable both by masking it directly and by comparing synthesised bits.
    let private lowBitsForSource (src : NativeIntSource) : uint64 =
        match src with
        | NativeIntSource.GcHandlePtr (_, tag) -> Operators.uint64 tag
        | NativeIntSource.TypeHandlePtr target -> TypeHandleTag.forTarget target |> Operators.uint64
        // `AsTypeDesc` has cleared the tag; that is the whole point of it.
        | NativeIntSource.TypeDescPtr _ -> 0UL
        | _ -> 0UL

    /// For a *pointer-shaped* `src`, the bits `materialiseHashBits` would return if this state
    /// has already assigned them; `None` if it has not. Never assigns, so a caller that only
    /// needs to *recognise* an address — equality, rather than arithmetic — can ask without
    /// perturbing the numbering that every later synthesised value depends on. The signature
    /// is what guarantees that: there is no state to hand back.
    ///
    /// Shares `canonicalKey` and `lowBitsForSource` with the minting path rather than
    /// re-deriving them, which is what makes a tagged view answer with the bits the guest
    /// would actually have observed: a `TypeDescPtr` masked out of a `TypeHandlePtr` differs
    /// from it in exactly bit 1, and a tagged GC handle in its own low bits.
    ///
    /// The domain is the canonicalisable pointer shapes only. `Verbatim`, `OpaqueHashBits`,
    /// managed pointers and cross-array offsets are values whose bits are known (or knowably
    /// absent) without any assignment, so asking this question about them is a category
    /// error; `canonicalKey` refuses them loudly and that refusal is the contract. The same
    /// goes for the pointer shapes `canonicalKey` itself declines — PerInstInfo chain
    /// intermediates, a `MethodTablePtr` over a generic parameter, a function pointer to a
    /// synthesised method — which crash here with `canonicalKey`'s diagnostic rather than a
    /// comparison-flavoured one.
    let tryExistingHashBits (counters : PointerHashState) (src : NativeIntSource) : int64 option =
        let key = canonicalKey src
        let tagBits = lowBitsForSource src

        match counters with
        | PointerHashState.SequentialFirstTouch (_, assigned) ->
            Map.tryFind key assigned
            |> Option.map (fun bits -> Operators.int64 (bits ||| tagBits))

    /// Synthesise deterministic 64-bit hash bits for a `NativeIntSource`.
    /// This is the single named site at which synthesised bits come into
    /// existence; every bit-mixing or arithmetic op that lifts a pointer
    /// shape into the numeric domain routes through here.
    ///
    /// `reason` is woven into the diagnostic if a non-canonicalisable
    /// source (non-null managed pointer, cross-array offset) reaches this
    /// helper — those should have been handled by the caller before they
    /// got here.
    ///
    /// Determinism: under `PointerHashState.SequentialFirstTouch`, bits
    /// depend only on the canonical key and the order in which keys are first
    /// registered. The result is stable for a given execution trace and
    /// reproducible across runs with the same scheduler.
    ///
    /// Returned as `int64` to match `Int64Source.OpaqueHashBits` storage;
    /// the conversion from the uint64 bit pattern is an unchecked
    /// reinterpret (bit-preserving).
    let materialiseHashBits
        (reason : string)
        (src : NativeIntSource)
        (counters : PointerHashState)
        : int64 * PointerHashState
        =
        match src with
        | NativeIntSource.Verbatim n -> n, counters
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L, counters
        | NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
            // An `Unsafe.AsRef<T>((void*)bits)` placeholder IS a bit pattern rather
            // than an address, so its bits are known exactly and no counter is
            // needed. `Int64Source.widenedNativeInt` and
            // `nativeIntBitsForFloatConversion` treat it the same way; all three
            // must agree, because a placeholder can reach synthesis either
            // already-widened or straight from the native-int slot.
            bits, counters
        | NativeIntSource.ManagedPointer _ ->
            failwith
                $"PointerHashSynthesis.materialiseHashBits (%s{reason}): refusing to synthesise bits for managed pointer %O{src} (would erase byref provenance)"
        | NativeIntSource.SyntheticCrossArrayOffset _ ->
            failwith
                $"PointerHashSynthesis.materialiseHashBits (%s{reason}): refusing to synthesise bits for synthetic cross-array offset %O{src}"
        | NativeIntSource.OpaqueHashBits bits ->
            // Already-synthesised bits round-trip back as themselves; no new
            // counter is assigned because the bits were produced by the same
            // pipeline and stored in a native-int slot via `conv.u` / `conv.i`.
            bits, counters
        | _ ->
            let key = canonicalKey src

            // Low bits are a view over an identity, not part of it, so the memo
            // stores address bits alone and the tag is OR-ed on at the end. The
            // counter scheme leaves the low 2 bits clear for exactly this, so the
            // no-collision property is preserved.
            let tagBits = lowBitsForSource src

            match counters with
            | PointerHashState.SequentialFirstTouch (nextCounter, assigned) ->
                match Map.tryFind key assigned with
                | Some bits -> Operators.int64 (bits ||| tagBits), counters
                | None ->
                    let bits = (nextCounter + 1UL) <<< 2

                    let counters' =
                        PointerHashState.SequentialFirstTouch (nextCounter + 1UL, Map.add key bits assigned)

                    Operators.int64 (bits ||| tagBits), counters'
