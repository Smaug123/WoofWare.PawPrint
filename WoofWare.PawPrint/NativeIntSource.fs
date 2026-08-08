namespace WoofWare.PawPrint

open System
open System.Diagnostics
open Checked

/// The delta between the addresses of two locations in memory that aren't within the same ByteStorageIdentity.
type SyntheticCrossArrayOffset =
    private
        {
            _TargetRoot : ByteStorageIdentity
            _TargetOffset : int64
            _SourceRoot : ByteStorageIdentity
            _SourceOffset : int64
        }

module SyntheticCrossArrayOffset =
    let make
        (targetRoot : ByteStorageIdentity)
        (targetOffset : int64)
        (sourceRoot : ByteStorageIdentity)
        (sourceOffset : int64)
        =
        if targetRoot = sourceRoot then
            failwith "not a cross-array offset"

        {
            _TargetRoot = targetRoot
            _TargetOffset = targetOffset
            _SourceRoot = sourceRoot
            _SourceOffset = sourceOffset
        }

    let negate (s : SyntheticCrossArrayOffset) =
        {
            _TargetRoot = s._SourceRoot
            _TargetOffset = s._SourceOffset
            _SourceRoot = s._TargetRoot
            _SourceOffset = s._TargetOffset
        }

    let targetRoot (s : SyntheticCrossArrayOffset) : ByteStorageIdentity = s._TargetRoot
    let targetOffset (s : SyntheticCrossArrayOffset) : int64 = s._TargetOffset
    let sourceRoot (s : SyntheticCrossArrayOffset) : ByteStorageIdentity = s._SourceRoot
    let sourceOffset (s : SyntheticCrossArrayOffset) : int64 = s._SourceOffset

    /// A SyntheticCrossArrayOffset is semantically a difference between memory addresses, so it is a native int.
    /// Various parts of the BCL ask to compare it against integers.
    /// For example, Memmove asks whether the source and dest overlap, by asking whether dest - source < len.
    /// PawPrint doesn't really model the address space as an array of bytes at all, but it *can* reply to the question
    /// "is this delta small", and that's what this function does.
    let internal cltVerbatim (_ : SyntheticCrossArrayOffset) (positiveComparand : int64) =
        if positiveComparand < 0L then
            failwith "cltVerbatim arg must be nonnegative"

        if positiveComparand >= (1L <<< 40) then
            failwith $"cltVerbatim can only compare with small deltas, got %i{positiveComparand}"
        // TODO: it *is* possible for people to do arithmetic on addresses e.g. a PE image.
        // I really hope nobody does that.
        false

    /// A SyntheticCrossArrayOffset is semantically a difference between memory addresses, so it is a native int.
    /// Various parts of the BCL ask to compare it against integers.
    /// For example, Memmove asks whether the source and dest overlap, by asking whether dest - source < len.
    /// PawPrint doesn't really model the address space as an array of bytes at all, but it *can* reply to the question
    /// "is this delta small", and that's what this function does.
    let internal cgtVerbatim (_ : SyntheticCrossArrayOffset) (positiveComparand : int64) =
        if positiveComparand < 0L then
            failwith "cgtVerbatim arg must be nonnegative"

        if positiveComparand >= (1L <<< 40) then
            failwith $"cgtVerbatim can only compare with small deltas, got %i{positiveComparand}"
        // TODO: it *is* possible for people to do arithmetic on addresses e.g. a PE image.
        // I really hope nobody does that.
        true

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource =
    | Verbatim of uint64
    | FromManagedPointer of ManagedPointerSource
    | FromSyntheticCrossArrayStorage of SyntheticCrossArrayOffset
    /// Synthesised pointer-hash bits arriving from `Int64Source.OpaqueHashBits`
    /// via `conv.u`. Round-trips back into a `NativeIntSource.OpaqueHashBits`
    /// in `Conv_U`; that variant inherits the "this is not a real pointer"
    /// contract — it must never be dereferenced. See
    /// `docs/plans/2026-05-13-castcache-synthetic-hash-bits.md`.
    | FromOpaqueHashBits of int64

[<RequireQualifiedAccess>]
[<CustomEquality>]
[<NoComparison>]
type NativeIntSource =
    | Verbatim of int64
    | ManagedPointer of ManagedPointerSource
    | FunctionPointer of MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    | TypeHandlePtr of RuntimeTypeHandleTarget
    /// `TypeDesc*` for a runtime type: the untagged half of a TypeDesc-shaped
    /// `TypeHandlePtr`, produced by CoreCLR's `TypeHandle.AsTypeDesc`
    /// (`handle & ~2`). It addresses the same base as the `TypeHandlePtr` it came
    /// from — the two differ only in the tag bit — but is a distinct identity:
    /// `IsTypeDesc` is false of it, and it is the pointer through which
    /// `TypeDesc`'s own fields are read.
    ///
    /// Only TypeDesc-shaped targets (byref / pointer / function-pointer /
    /// generic-parameter) have one; `TypeHandleTag.forTarget` is the classifier,
    /// and producer sites must not mint this for a MethodTable-shaped target.
    | TypeDescPtr of RuntimeTypeHandleTarget
    /// `MethodTable*` for a runtime type. The payload widens to
    /// `RuntimeTypeHandleTarget` so the open-generic typedef's canonical
    /// MethodTable (`OpenGenericTypeDefinition`) can be expressed alongside
    /// the closed instantiation MethodTable (`Closed`). TypeDesc cases
    /// (`GenericParameter` / `MethodGenericParameter`) never own a real
    /// MethodTable and must be rejected at producer sites; consumers that
    /// require a closed `ConcreteTypeHandle` (e.g. fixed-instantiation
    /// reflection paths) match `Closed` explicitly and fail loudly otherwise.
    | MethodTablePtr of RuntimeTypeHandleTarget
    | MethodTableAuxiliaryDataPtr of RuntimeTypeHandleTarget
    /// Synthetic `MethodTable*** PerInstInfo` pointer for a generic instantiation.
    /// First `ldind` step yields `PerInstDictPtr` of the same handle (the
    /// pointer to the first per-instance dictionary); a second `ldind` step
    /// yields `MethodTablePtr` of the type's first generic argument. Only the
    /// `**PerInstInfo` chain used by `CastHelpers.IsNullableForType` is
    /// modelled today; richer indexing into the dictionary table is not.
    | PerInstInfoPtr of ConcreteTypeHandle
    /// Synthetic `MethodTable**` pointing at the first per-instance dictionary
    /// of the named generic instantiation. Produced by `ldind` on
    /// `PerInstInfoPtr`; one further `ldind` step yields `MethodTablePtr` of
    /// the type's first generic argument.
    | PerInstDictPtr of ConcreteTypeHandle
    | MethodHandlePtr of int64
    | FieldHandlePtr of int64
    | AssemblyHandle of string
    | ModuleHandle of string
    | MetadataImportHandle of string
    /// A GC handle, plus whatever tag bits managed code has stuffed into its low
    /// bits. `GcHandleAddress` is the handle's identity; `tag` is a view the guest
    /// has imposed on it, and is always inside
    /// `TaggedPointerBits.tagMask TaggedPointerBits.gcHandleTagWidthBits`.
    ///
    /// CoreLib genuinely does this: `System.WeakReference` stores
    /// `handle | TracksResurrectionBit` and masks the bit off again on every read,
    /// and `GCHandle` marks pinned handles with bit 0. PawPrint models neither the
    /// handle's numeric address nor a fake stand-in for it, so the tag has to
    /// travel alongside the identity — see `TaggedPointerBits` for the arithmetic
    /// this admits, and `docs/plans/2026-08-06-tagged-gc-handles.md` for why.
    ///
    /// Consumers that dereference or free the handle (`ldind.ref`, the
    /// `GCHandle`/`DependentHandle` InternalCalls) require `tag = 0`: real managed
    /// code always strips the tag first, and a tagged dereference is a misaligned
    /// read.
    | GcHandlePtr of handle : GcHandleAddress * tag : int64
    /// Opaque handle returned by `EventPipeInternal_CreateProvider` /
    /// `EventPipeInternal_GetProvider`. PawPrint never opens a tracing
    /// session, so the payload is just a monotonically increasing ID; the
    /// tagged variant exists so a foreign IntPtr cannot be mistaken for a
    /// PawPrint-minted EventPipe provider handle.
    | EventPipeProviderPtr of int64
    /// Opaque handle returned by `EventPipeInternal_DefineEvent`. Same role
    /// as `EventPipeProviderPtr` but distinguished by tag so an event handle
    /// cannot be passed where a provider handle is expected.
    | EventPipeEventPtr of int64
    /// Opaque handle returned by `SystemNative_LowLevelMonitor_Create`. The
    /// guest stores it in `LowLevelMonitor._nativeMonitor` (an `IntPtr` slot)
    /// and round-trips it through the other six `SystemNative_LowLevelMonitor_*`
    /// QCalls. The handle is distinguished by tag so a foreign `IntPtr` cannot
    /// be mistaken for a PawPrint-minted monitor, and never compares equal to
    /// `Verbatim 0L` so the BCL's allocation-failure check (`if _nativeMonitor
    /// == IntPtr.Zero throw new OutOfMemoryException()`) does not fire.
    | LowLevelMonitorPtr of LowLevelMonitorId
    /// Opaque handle returned by `CreateSemaphoreExW` (and, in future PRs,
    /// `CreateEventExW` / `CreateMutexExW`). Round-trips through the guest as
    /// an `IntPtr` wrapped in a `SafeWaitHandle`, and is fed back into
    /// `WaitHandle_WaitOneCore`, `ReleaseSemaphore`, and `CloseHandle`. The
    /// handle is distinguished by tag so a foreign `IntPtr` cannot be mistaken
    /// for a PawPrint-minted wait handle; never compares equal to
    /// `Verbatim 0L`, so the BCL's "create failed → throw" check does not fire
    /// for a successfully-minted handle.
    | WaitHandlePtr of WaitHandleId
    /// Returned by `Unsafe.ByteOffset` or managed-pointer subtraction for two byrefs into distinct byte-addressed
    /// storage containers.
    | SyntheticCrossArrayOffset of SyntheticCrossArrayOffset
    /// Synthesised pointer-hash bits living in a native-int slot. Produced
    /// when `conv.u` / `conv.i` narrows an `Int64Source.OpaqueHashBits` back
    /// to native-int width (e.g. `BitOperations.RotateLeft(nuint, int)`
    /// inlines `(nuint)RotateLeft((ulong)value, offset)` — the final `(nuint)`
    /// cast is exactly this round-trip). Carries the same load-bearing
    /// contract as `Int64Source.OpaqueHashBits`: the bits are deterministic
    /// and bit-mixing safe, but they must NOT be used as a real pointer —
    /// `ldind` / `stind` / dereference must reject them, and the `conv.i8`
    /// / `conv.u8` round-trip normalises back to `Int64Source.OpaqueHashBits`
    /// via `Int64Source.widenedNativeInt`. See
    /// `docs/plans/2026-05-13-castcache-synthetic-hash-bits.md`.
    | OpaqueHashBits of int64

    override this.ToString () : string =
        match this with
        | NativeIntSource.Verbatim int64 -> $"%i{int64}"
        | NativeIntSource.ManagedPointer ptr -> $"<managed pointer {ptr}>"
        | NativeIntSource.FunctionPointer methodDefinition ->
            $"<pointer to {methodDefinition.Name} in {methodDefinition.DeclaringType.Assembly.Name}>"
        | NativeIntSource.TypeHandlePtr ptr -> $"<type ID %O{ptr}>"
        | NativeIntSource.TypeDescPtr ptr -> $"<TypeDesc of %O{ptr}>"
        | NativeIntSource.MethodTablePtr ptr -> $"<method table for type %O{ptr}>"
        | NativeIntSource.MethodTableAuxiliaryDataPtr ptr -> $"<method table auxiliary data for type %O{ptr}>"
        | NativeIntSource.PerInstInfoPtr ptr -> $"<PerInstInfo for type %O{ptr}>"
        | NativeIntSource.PerInstDictPtr ptr -> $"<PerInstInfo first dictionary for type %O{ptr}>"
        | NativeIntSource.MethodHandlePtr ptr -> $"<method ID %O{ptr}>"
        | NativeIntSource.FieldHandlePtr ptr -> $"<field ID %O{ptr}>"
        | NativeIntSource.AssemblyHandle name -> $"<assembly %s{name}>"
        | NativeIntSource.ModuleHandle name -> $"<module %s{name}>"
        | NativeIntSource.MetadataImportHandle name -> $"<metadata import for %s{name}>"
        | NativeIntSource.GcHandlePtr (handle, 0L) -> $"<GC handle %O{handle}>"
        | NativeIntSource.GcHandlePtr (handle, tag) -> $"<GC handle %O{handle}, tagged 0x%x{tag}>"
        | NativeIntSource.EventPipeProviderPtr id -> $"<EventPipe provider #%i{id}>"
        | NativeIntSource.EventPipeEventPtr id -> $"<EventPipe event #%i{id}>"
        | NativeIntSource.LowLevelMonitorPtr id -> $"%O{id}"
        | NativeIntSource.WaitHandlePtr id -> $"%O{id}"
        | NativeIntSource.SyntheticCrossArrayOffset _ -> "<synthetic cross-storage byte offset>"
        | NativeIntSource.OpaqueHashBits bits -> $"<opaque hash bits (native int) 0x%x{bits}>"

    override this.Equals (other : obj) : bool =
        match other with
        | :? NativeIntSource as other ->
            match this, other with
            | NativeIntSource.Verbatim left, NativeIntSource.Verbatim right -> left = right
            | NativeIntSource.ManagedPointer left, NativeIntSource.ManagedPointer right -> left = right
            | NativeIntSource.FunctionPointer left, NativeIntSource.FunctionPointer right ->
                MethodInfo.NominallyEqual left right
            | NativeIntSource.TypeHandlePtr left, NativeIntSource.TypeHandlePtr right -> left = right
            | NativeIntSource.TypeDescPtr left, NativeIntSource.TypeDescPtr right -> left = right
            | NativeIntSource.MethodTablePtr left, NativeIntSource.MethodTablePtr right -> left = right
            | NativeIntSource.MethodTableAuxiliaryDataPtr left, NativeIntSource.MethodTableAuxiliaryDataPtr right ->
                left = right
            | NativeIntSource.PerInstInfoPtr left, NativeIntSource.PerInstInfoPtr right -> left = right
            | NativeIntSource.PerInstDictPtr left, NativeIntSource.PerInstDictPtr right -> left = right
            | NativeIntSource.MethodHandlePtr left, NativeIntSource.MethodHandlePtr right -> left = right
            | NativeIntSource.FieldHandlePtr left, NativeIntSource.FieldHandlePtr right -> left = right
            | NativeIntSource.AssemblyHandle left, NativeIntSource.AssemblyHandle right -> left = right
            | NativeIntSource.ModuleHandle left, NativeIntSource.ModuleHandle right -> left = right
            | NativeIntSource.MetadataImportHandle left, NativeIntSource.MetadataImportHandle right -> left = right
            | NativeIntSource.GcHandlePtr (leftHandle, leftTag), NativeIntSource.GcHandlePtr (rightHandle, rightTag) ->
                leftHandle = rightHandle && leftTag = rightTag
            | NativeIntSource.EventPipeProviderPtr left, NativeIntSource.EventPipeProviderPtr right -> left = right
            | NativeIntSource.EventPipeEventPtr left, NativeIntSource.EventPipeEventPtr right -> left = right
            | NativeIntSource.LowLevelMonitorPtr left, NativeIntSource.LowLevelMonitorPtr right -> left = right
            | NativeIntSource.WaitHandlePtr left, NativeIntSource.WaitHandlePtr right -> left = right
            | NativeIntSource.SyntheticCrossArrayOffset left, NativeIntSource.SyntheticCrossArrayOffset right ->
                left = right
            | NativeIntSource.OpaqueHashBits left, NativeIntSource.OpaqueHashBits right -> left = right
            | NativeIntSource.Verbatim _, _
            | NativeIntSource.ManagedPointer _, _
            | NativeIntSource.FunctionPointer _, _
            | NativeIntSource.TypeHandlePtr _, _
            | NativeIntSource.TypeDescPtr _, _
            | NativeIntSource.MethodTablePtr _, _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _, _
            | NativeIntSource.PerInstInfoPtr _, _
            | NativeIntSource.PerInstDictPtr _, _
            | NativeIntSource.MethodHandlePtr _, _
            | NativeIntSource.FieldHandlePtr _, _
            | NativeIntSource.AssemblyHandle _, _
            | NativeIntSource.ModuleHandle _, _
            | NativeIntSource.MetadataImportHandle _, _
            | NativeIntSource.GcHandlePtr _, _
            | NativeIntSource.EventPipeProviderPtr _, _
            | NativeIntSource.EventPipeEventPtr _, _
            | NativeIntSource.LowLevelMonitorPtr _, _
            | NativeIntSource.WaitHandlePtr _, _
            | NativeIntSource.SyntheticCrossArrayOffset _, _
            | NativeIntSource.OpaqueHashBits _, _ -> false
        | _ -> false

    override this.GetHashCode () : int =
        match this with
        | NativeIntSource.Verbatim int64 -> HashCode.Combine (0, int64)
        | NativeIntSource.ManagedPointer ptr -> HashCode.Combine (1, ptr)
        | NativeIntSource.FunctionPointer methodDefinition ->
            HashCode.Combine (
                2,
                methodDefinition.DeclaringType.Identity,
                methodDefinition.DeclaringType.Generics,
                methodDefinition.IdentityKey,
                methodDefinition.Generics
            )
        | NativeIntSource.TypeHandlePtr ptr -> HashCode.Combine (3, ptr)
        | NativeIntSource.TypeDescPtr ptr -> HashCode.Combine (20, ptr)
        | NativeIntSource.MethodTablePtr ptr -> HashCode.Combine (4, ptr)
        | NativeIntSource.MethodTableAuxiliaryDataPtr ptr -> HashCode.Combine (5, ptr)
        | NativeIntSource.PerInstInfoPtr ptr -> HashCode.Combine (18, ptr)
        | NativeIntSource.PerInstDictPtr ptr -> HashCode.Combine (19, ptr)
        | NativeIntSource.MethodHandlePtr ptr -> HashCode.Combine (6, ptr)
        | NativeIntSource.FieldHandlePtr ptr -> HashCode.Combine (7, ptr)
        | NativeIntSource.AssemblyHandle name -> HashCode.Combine (8, name)
        | NativeIntSource.ModuleHandle name -> HashCode.Combine (9, name)
        | NativeIntSource.MetadataImportHandle name -> HashCode.Combine (10, name)
        | NativeIntSource.GcHandlePtr (handle, tag) -> HashCode.Combine (11, handle, tag)
        | NativeIntSource.EventPipeProviderPtr id -> HashCode.Combine (12, id)
        | NativeIntSource.EventPipeEventPtr id -> HashCode.Combine (13, id)
        | NativeIntSource.SyntheticCrossArrayOffset s -> HashCode.Combine (14, hash s)
        | NativeIntSource.OpaqueHashBits bits -> HashCode.Combine (15, bits)
        | NativeIntSource.LowLevelMonitorPtr id -> HashCode.Combine (16, id)
        | NativeIntSource.WaitHandlePtr id -> HashCode.Combine (17, id)

/// CoreCLR's `TypeHandle` is a tagged pointer: it wraps either a `MethodTable*`
/// or a `TypeDesc*`, and distinguishes them by setting bit 1 in the TypeDesc
/// case. The managed `TypeHandle` struct in
/// src/coreclr/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.CoreCLR.cs
/// both reads that tag (`IsTypeDesc` is `((nint)m_asTAddr & 2) != 0`) and strips
/// it (`AsTypeDesc` is `(TypeDesc*)((nint)m_asTAddr & ~2)`).
///
/// This is the single home for that rule. It has two consumers — the `and`
/// arms in `NullaryIlOp` and the synthesised low bits in
/// `PointerHashSynthesis` — which must agree, because a handle's tag is
/// observable both by masking it directly and by comparing synthesised bits.
[<RequireQualifiedAccess>]
module TypeHandleTag =
    /// Width of the region whose bits are known. Both a `MethodTable` and a
    /// `TypeDesc` are at least pointer-aligned, so bits 0-2 of either address are
    /// provably clear; claiming two bits is conservative and true, and covers the
    /// only tag bit CoreCLR actually uses.
    let widthBits : int = 2

    /// The tag carried by a handle to `target`. Unlike a GC handle's tag — which
    /// is independent state that managed code sets and clears — this is a
    /// *function of the target*: `IsTypeDesc` is determined by what the handle
    /// points at. A handle whose tag differs from this is therefore a different
    /// kind of pointer, not the same handle retagged.
    let forTarget (target : RuntimeTypeHandleTarget) : int64 =
        match target with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> 0L
        // Generic parameters in CoreCLR are TypeVarTypeDesc, a TypeDesc subclass, so the
        // tagged-pointer encoding sets the second-lowest bit. Reflection paths such as
        // `RuntimeType.get_IsInterface` rely on `TypeHandle.IsTypeDesc` to short-circuit
        // before dereferencing a non-existent MethodTable; honour that contract.
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

[<RequireQualifiedAccess>]
module NativeIntSource =
    let syntheticCrossStorageByteOffset
        (originStorage : ByteStorageIdentity)
        (originByteOffset : int64)
        (targetStorage : ByteStorageIdentity)
        (targetByteOffset : int64)
        : NativeIntSource
        =
        SyntheticCrossArrayOffset.make targetStorage targetByteOffset originStorage originByteOffset
        |> NativeIntSource.SyntheticCrossArrayOffset

    /// A freshly-minted GC handle, as the runtime hands it to managed code:
    /// untagged. Managed code is what adds tag bits, via `and`/`or`/`xor`.
    let gcHandlePtr (handle : GcHandleAddress) : NativeIntSource =
        NativeIntSource.GcHandlePtr (handle, 0L)

    /// A GC handle carrying tag bits managed code has put there. The tag must lie
    /// inside the tag region, which is guaranteed when it comes from
    /// `TaggedPointerBits`; the assertion catches any site that bypasses that.
    let gcHandlePtrTagged (handle : GcHandleAddress) (tag : int64) : NativeIntSource =
        Debug.Assert (
            tag &&& ~~~(TaggedPointerBits.tagMask TaggedPointerBits.gcHandleTagWidthBits) = 0L,
            $"tag 0x%x{tag} escapes the GC handle tag region; tags must come from TaggedPointerBits"
        )

        NativeIntSource.GcHandlePtr (handle, tag)

    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.SyntheticCrossArrayOffset s -> SyntheticCrossArrayOffset.cltVerbatim s 1L
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.MethodHandlePtr _
        | NativeIntSource.TypeHandlePtr _
        | NativeIntSource.TypeDescPtr _
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.PerInstInfoPtr _
        | NativeIntSource.PerInstDictPtr _
        | NativeIntSource.GcHandlePtr _
        | NativeIntSource.EventPipeProviderPtr _
        | NativeIntSource.EventPipeEventPtr _
        | NativeIntSource.LowLevelMonitorPtr _
        | NativeIntSource.WaitHandlePtr _
        | NativeIntSource.AssemblyHandle _
        | NativeIntSource.MetadataImportHandle _
        | NativeIntSource.ModuleHandle _ -> false
        | NativeIntSource.OpaqueHashBits bits -> bits = 0L
        // A function pointer is never null. `ldftn` cannot produce one, and neither can the
        // runtime synthesising a stub — CoreLib branches on `structMarshalStub != null` to choose
        // between the stub and the blittable memmove path, so answering anything else here would
        // silently send a non-blittable struct down the memmove path.
        | NativeIntSource.FunctionPointer _ -> false
        | NativeIntSource.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> true
            | _ -> false

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L
        | NativeIntSource.SyntheticCrossArrayOffset _ ->
            failwith "Most isNonnegative of cross-array offsets are not meaningful"
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.MethodHandlePtr _
        | NativeIntSource.TypeHandlePtr _
        | NativeIntSource.TypeDescPtr _
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.PerInstInfoPtr _
        | NativeIntSource.PerInstDictPtr _
        | NativeIntSource.GcHandlePtr _
        | NativeIntSource.EventPipeProviderPtr _
        | NativeIntSource.EventPipeEventPtr _
        | NativeIntSource.LowLevelMonitorPtr _
        | NativeIntSource.WaitHandlePtr _
        | NativeIntSource.AssemblyHandle _
        | NativeIntSource.MetadataImportHandle _
        | NativeIntSource.ModuleHandle _ -> true
        | NativeIntSource.OpaqueHashBits bits -> bits >= 0L
        | NativeIntSource.ManagedPointer _ -> true

    /// CEQ semantics on `NativeIntSource` pairs: matches the
    /// `native int × native int` arm of ECMA Table III.4. Distinct from the
    /// type's structural `Equals` because (a) `Verbatim 0L` and
    /// `ManagedPointer Null` are both the value zero so must compare equal
    /// despite structural inequality, and (b) `MethodTablePtr` and
    /// `TypeHandlePtr` for the same handle alias (CoreCLR encodes them as the
    /// same address for non-TypeDesc types). Used by `ceq` on eval-stack
    /// native ints and by the `Interlocked.CompareExchange(ref IntPtr, …)`
    /// intrinsic, which compares the slot's contents under CEQ semantics.
    let equalsForCli (a : NativeIntSource) (b : NativeIntSource) : bool =
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
        | NativeIntSource.FunctionPointer f1, NativeIntSource.FunctionPointer f2 -> MethodInfo.NominallyEqual f1 f2
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
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.TypeDescPtr _
        | NativeIntSource.TypeDescPtr _, NativeIntSource.OpaqueHashBits _
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.MethodTablePtr _
        | NativeIntSource.MethodTablePtr _, NativeIntSource.OpaqueHashBits _
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.MethodTableAuxiliaryDataPtr _, NativeIntSource.OpaqueHashBits _
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.PerInstInfoPtr _
        | NativeIntSource.PerInstInfoPtr _, NativeIntSource.OpaqueHashBits _
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.PerInstDictPtr _
        | NativeIntSource.PerInstDictPtr _, NativeIntSource.OpaqueHashBits _
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
        | NativeIntSource.LowLevelMonitorPtr _, NativeIntSource.OpaqueHashBits _
        | NativeIntSource.OpaqueHashBits _, NativeIntSource.WaitHandlePtr _
        | NativeIntSource.WaitHandlePtr _, NativeIntSource.OpaqueHashBits _ ->
            failwith
                $"TODO (CEQ): synthesised hash bits vs handle pointer requires materialising the handle's bits through PointerHashCounters; got {a} vs {b}"
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
            | RuntimeTypeHandleTarget.GenericParameter _, _
            | RuntimeTypeHandleTarget.MethodGenericParameter _, _
            | _, RuntimeTypeHandleTarget.GenericParameter _
            | _, RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                // A bare generic parameter has no MethodTable; this combination should
                // not arise from any legitimate construction.
                failwith
                    $"CEQ: MethodTablePtr/TypeHandlePtr with generic-parameter target has no MethodTable identity: %O{t1} vs %O{t2}"
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
            let z1 = isZero a
            let z2 = isZero b

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
            let z1 = isZero a
            let z2 = isZero b

            if z1 && z2 then
                true
            elif z1 <> z2 then
                false
            else
                failwith $"TODO (CEQ): synthesised hash bits vs managed pointer, both non-null: {a} vs {b}"

    /// True if a < b.
    let isLess (a : NativeIntSource) (b : NativeIntSource) : bool =
        match a, b with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> a < b
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.Verbatim _
        | NativeIntSource.Verbatim _, NativeIntSource.SyntheticCrossArrayOffset _ ->
            failwith "TODO: cross-array offsets hopefully aren't meaningfully compared with ints"
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.SyntheticCrossArrayOffset _ -> failwith "TODO"
        // OpaqueHashBits carries an unambiguous int64 bit pattern, so signed
        // comparison is well-defined against any other unambiguous bit-pattern
        // source (mirrors `Int64Source.compareSigned`).
        | NativeIntSource.OpaqueHashBits a, NativeIntSource.OpaqueHashBits b -> a < b
        | NativeIntSource.OpaqueHashBits a, NativeIntSource.Verbatim b
        | NativeIntSource.Verbatim a, NativeIntSource.OpaqueHashBits b -> a < b
        // `ManagedPointer Null` is the value 0 (cf. `cliTypeZeroOf` planting
        // it for `IntPtr.Zero`/`UIntPtr.Zero`). Signed comparison against
        // OpaqueHashBits therefore reduces to comparing the bits against 0L.
        | NativeIntSource.OpaqueHashBits a, NativeIntSource.ManagedPointer ManagedPointerSource.Null -> a < 0L
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null, NativeIntSource.OpaqueHashBits b -> 0L < b
        | _, _ -> failwith $"TODO: NativeIntSource.isLess on non-Verbatim sources: %O{a} vs %O{b}"

type CliRuntimePointer =
    | Verbatim of int64
    | TypeHandlePtr of RuntimeTypeHandleTarget
    /// See `NativeIntSource.TypeDescPtr` for the contract; the
    /// eval-stack-flattened counterpart is `NativeIntSource.TypeDescPtr`.
    | TypeDescPtr of RuntimeTypeHandleTarget
    | FieldRegistryHandle of int64
    | MethodRegistryHandle of int64
    /// See `NativeIntSource.MethodTablePtr` for the contract; the
    /// eval-stack-flattened counterpart is `NativeIntSource.MethodTablePtr`.
    | MethodTablePtr of RuntimeTypeHandleTarget
    | MethodTableAuxiliaryDataPtr of RuntimeTypeHandleTarget
    /// See `NativeIntSource.PerInstInfoPtr`. The eval-stack-flattened
    /// counterpart is `NativeIntSource.PerInstInfoPtr`.
    | PerInstInfoPtr of ConcreteTypeHandle
    /// See `NativeIntSource.PerInstDictPtr`. The eval-stack-flattened
    /// counterpart is `NativeIntSource.PerInstDictPtr`.
    | PerInstDictPtr of ConcreteTypeHandle
    | Managed of ManagedPointerSource
    /// A GC handle stored in a typed-pointer slot (e.g. `void*`, `T*`), plus any
    /// tag bits managed code has put in its low bits (see
    /// `NativeIntSource.GcHandlePtr`, whose contract this mirrors). Arithmetic
    /// and comparison operations on this case must go through eval-stack
    /// flattening (`EvalStack.ofCliType`) into `NativeIntSource.GcHandlePtr`;
    /// helpers like `NativeIntSource.isZero`/`isNonnegative` and conv ops only
    /// match the `NativeIntSource` form.
    | GcHandlePtr of handle : GcHandleAddress * tag : int64

/// The provenance of a value in an int32 evaluation-stack slot.
///
/// Almost every int32 on the stack is an ordinary number. The exception is a
/// byref that `conv.i4` / `conv.u4` truncated: CoreLib asks whether a pointer is
/// aligned by narrowing it and masking — `SpanHelpers.IndexOfNullCharacter`, which
/// `String.wcslen` and hence `new string(char*)` runs first, opens with
/// `((int)searchSpace & 1) != 0`. The mask is answerable (see `TaggedPointerBits`,
/// and `ManagedPointerSource.tryContainerAlignmentBits` for the guarantee it rests
/// on), but only if the byref survives the narrowing, and PawPrint must not invent
/// the address bits an `int32` would need (see
/// `docs/developer/pointers-and-byte-representations.md`).
///
/// This DU is why the int32 stack slot carries provenance at all. Making it a case
/// *here*, rather than smuggling the narrowed pointer into the wider native-int
/// slot, is what makes the compiler visit every site that consumes an int32: none
/// of them can treat a narrowed byref as a number by accident, because none of them
/// can get at a number without saying what to do when there isn't one.
[<RequireQualifiedAccess>]
type Int32Source =
    | Verbatim of int32
    /// A byref that `conv.i4` / `conv.u4` truncated to 32 bits, whose address
    /// PawPrint does not model. Only `and` against a mask can say anything about
    /// it; everything else must refuse.
    ///
    /// Always construct via `Int32Source.narrowManagedPointer`, which sends byrefs
    /// with an exactly-known bit pattern (`Null`, and the `NativeIntPlaceholder`
    /// produced by `Unsafe.AsRef<T>((void*)bits)`) to `Verbatim` instead: those are
    /// values, not addresses, and truncating them is ordinary truncation.
    | NarrowedManagedPointer of source : ManagedPointerSource

    override this.ToString () : string =
        match this with
        | Int32Source.Verbatim i -> $"%i{i}"
        | Int32Source.NarrowedManagedPointer ptr -> $"<managed pointer %O{ptr}, truncated to 32 bits>"

[<RequireQualifiedAccess>]
module Int32Source =

    /// Smart constructor for `Int32Source.NarrowedManagedPointer`: the result of
    /// `conv.i4` / `conv.u4` on a byref. `truncate` is the conversion's own
    /// narrowing, applied to byrefs whose bit pattern is exactly known.
    let narrowManagedPointer (truncate : int64 -> int32) (src : ManagedPointerSource) : Int32Source =
        match ManagedPointerSource.tryBitPatternBits src with
        | ValueSome bits -> Int32Source.Verbatim (truncate bits)
        | ValueNone -> Int32Source.NarrowedManagedPointer src

    /// The numeric value of an int32 stack slot.
    ///
    /// A narrowed byref has no numeric value PawPrint can state: `conv.i4` kept the
    /// low half of an address that was never modelled. `operation` names the
    /// consumer, so a guest that reaches one says exactly which opcode or helper
    /// wanted a number it cannot have.
    let value (operation : string) (src : Int32Source) : int32 =
        match src with
        | Int32Source.Verbatim i -> i
        | Int32Source.NarrowedManagedPointer ptr ->
            failwith
                $"%s{operation}: refusing to use managed pointer %O{ptr}, truncated to 32 bits, as a number; its value depends on the container's address, which PawPrint does not model"
