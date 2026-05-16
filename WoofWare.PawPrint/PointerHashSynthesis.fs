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
/// The first time a key is materialised, it is assigned counter `n` and
/// produces bit pattern `((n + 1) <<< 2) ||| lowBitsForKey(key)`. Subsequent
/// materialisations of the same key return the previously-assigned bits.
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
    /// by the cast-cache hash pipeline.
    let private canonicalKey (src : NativeIntSource) : CanonicalPointerKey =
        match src with
        | NativeIntSource.MethodTablePtr h -> CanonicalPointerKey.MethodTable h
        | NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle as target) ->
            match handle with
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> CanonicalPointerKey.MethodTable handle
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> CanonicalPointerKey.TypeHandle target
        | NativeIntSource.TypeHandlePtr target -> CanonicalPointerKey.TypeHandle target
        | NativeIntSource.MethodTableAuxiliaryDataPtr target -> CanonicalPointerKey.MethodTableAuxiliaryData target
        | NativeIntSource.FunctionPointer methodInfo ->
            CanonicalPointerKey.FunctionPointer (
                methodInfo.DeclaringType.Identity,
                List.ofSeq methodInfo.DeclaringType.Generics,
                ComparableMethodDefinitionHandle.Make methodInfo.Handle,
                List.ofSeq methodInfo.Generics
            )
        | NativeIntSource.MethodHandlePtr id -> CanonicalPointerKey.MethodHandle id
        | NativeIntSource.FieldHandlePtr id -> CanonicalPointerKey.FieldHandle id
        | NativeIntSource.GcHandlePtr handle -> CanonicalPointerKey.GcHandle handle
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

    /// Low-bit pattern required for a canonical key. Mirrors
    /// `NullaryIlOp.typeHandleLowAddressBits`: MethodTable* is aligned
    /// (low 2 bits clear); TypeDesc-shaped handles (Byref / Pointer /
    /// FunctionPointer / generic parameters) carry bit 1 set as the
    /// tagged-pointer marker; other handles are conventionally aligned.
    let private lowBitsForKey (key : CanonicalPointerKey) : uint64 =
        match key with
        | CanonicalPointerKey.MethodTable _ -> 0UL
        | CanonicalPointerKey.TypeHandle target ->
            match target with
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> 0UL
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> 2UL
            | RuntimeTypeHandleTarget.Closed handle ->
                match handle with
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ -> 2UL
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    failwith
                        $"PointerHashSynthesis.lowBitsForKey: TypeHandle(Closed(%O{handle})) should have been collapsed to MethodTable by canonicalKey; this is an interpreter bug"
        | CanonicalPointerKey.MethodTableAuxiliaryData _
        | CanonicalPointerKey.FunctionPointer _
        | CanonicalPointerKey.MethodHandle _
        | CanonicalPointerKey.FieldHandle _
        | CanonicalPointerKey.GcHandle _
        | CanonicalPointerKey.EventPipeProvider _
        | CanonicalPointerKey.EventPipeEvent _
        | CanonicalPointerKey.LowLevelMonitor _
        | CanonicalPointerKey.WaitHandle _
        | CanonicalPointerKey.AssemblyHandle _
        | CanonicalPointerKey.ModuleHandle _
        | CanonicalPointerKey.MetadataImportHandle _ -> 0UL

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

            match Map.tryFind key counters.Assigned with
            | Some bits -> Operators.int64 bits, counters
            | None ->
                let n = counters.NextCounter
                let bits = ((n + 1UL) <<< 2) ||| lowBitsForKey key

                let counters' =
                    {
                        NextCounter = n + 1UL
                        Assigned = Map.add key bits counters.Assigned
                    }

                Operators.int64 bits, counters'
