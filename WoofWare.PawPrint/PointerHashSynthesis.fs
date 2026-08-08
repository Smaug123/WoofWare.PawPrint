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
    | FunctionPointer of
        declaringTypeIdentity : ResolvedTypeIdentity *
        declaringTypeGenerics : ConcreteTypeHandle list *
        methodHandle : ComparableMethodDefinitionHandle *
        methodGenerics : ConcreteTypeHandle list
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

/// Counter-based assignment of synthesised bits to canonical pointer keys.
/// The first time a key is materialised, it is assigned counter `n` and stores
/// address bits `(n + 1) <<< 2`. Subsequent materialisations of the same key
/// return those same stored bits. Tag bits are NOT stored: they are a view over
/// the identity, so `materialiseHashBits` OR-s them on per source, which is what
/// lets two differently-tagged views of one identity (a `TypeHandlePtr` and the
/// `TypeDescPtr` masked out of it; an untagged and a tagged GC handle) share
/// address bits and differ only in the low region.
///
/// Distinct keys get distinct bit patterns by construction (no collisions),
/// and the assignment order is deterministic given a fixed program and
/// scheduler. This is the load-bearing property that makes synthesised hash
/// bits a faithful guest-observable surrogate for real pointer bits in the
/// CoreCLR cast-cache pipeline.
type PointerHashCounters =
    {
        NextCounter : uint64
        Assigned : Map<CanonicalPointerKey, uint64>
    }

[<RequireQualifiedAccess>]
module PointerHashCounters =
    let empty : PointerHashCounters =
        {
            NextCounter = 0UL
            Assigned = Map.empty
        }

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
        | NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ as target) ->
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
        | NativeIntSource.FunctionPointer methodInfo ->
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

            CanonicalPointerKey.FunctionPointer (
                methodInfo.DeclaringType.Identity,
                List.ofSeq methodInfo.DeclaringType.Generics,
                ComparableMethodDefinitionHandle.Make facts.Handle,
                List.ofSeq methodInfo.Generics
            )
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
    /// Determinism: bits depend only on the canonical key and the order
    /// in which keys are first registered. The result is stable for a
    /// given execution trace and reproducible across runs with the same
    /// scheduler.
    ///
    /// Returned as `int64` to match `Int64Source.OpaqueHashBits` storage;
    /// the conversion from the uint64 bit pattern is an unchecked
    /// reinterpret (bit-preserving).
    let materialiseHashBits
        (reason : string)
        (src : NativeIntSource)
        (counters : PointerHashCounters)
        : int64 * PointerHashCounters
        =
        match src with
        | NativeIntSource.Verbatim n -> n, counters
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L, counters
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

            // Low bits are a view over an identity, not part of it, so the map
            // stores address bits alone and the tag is OR-ed on at the end. The
            // counter scheme leaves the low 2 bits clear for exactly this, so the
            // no-collision property is preserved.
            let tagBits = lowBitsForSource src

            match Map.tryFind key counters.Assigned with
            | Some bits -> Operators.int64 (bits ||| tagBits), counters
            | None ->
                let n = counters.NextCounter
                let bits = (n + 1UL) <<< 2

                let counters' =
                    {
                        NextCounter = n + 1UL
                        Assigned = Map.add key bits counters.Assigned
                    }

                Operators.int64 (bits ||| tagBits), counters'
