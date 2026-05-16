namespace WoofWare.PawPrint

open System
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
    | MethodTablePtr of ConcreteTypeHandle
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
    | GcHandlePtr of GcHandleAddress
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
        | NativeIntSource.MethodTablePtr ptr -> $"<method table for type %O{ptr}>"
        | NativeIntSource.MethodTableAuxiliaryDataPtr ptr -> $"<method table auxiliary data for type %O{ptr}>"
        | NativeIntSource.PerInstInfoPtr ptr -> $"<PerInstInfo for type %O{ptr}>"
        | NativeIntSource.PerInstDictPtr ptr -> $"<PerInstInfo first dictionary for type %O{ptr}>"
        | NativeIntSource.MethodHandlePtr ptr -> $"<method ID %O{ptr}>"
        | NativeIntSource.FieldHandlePtr ptr -> $"<field ID %O{ptr}>"
        | NativeIntSource.AssemblyHandle name -> $"<assembly %s{name}>"
        | NativeIntSource.ModuleHandle name -> $"<module %s{name}>"
        | NativeIntSource.MetadataImportHandle name -> $"<metadata import for %s{name}>"
        | NativeIntSource.GcHandlePtr handle -> $"<GC handle %O{handle}>"
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
            | NativeIntSource.GcHandlePtr left, NativeIntSource.GcHandlePtr right -> left = right
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
                methodDefinition.Handle,
                methodDefinition.Generics
            )
        | NativeIntSource.TypeHandlePtr ptr -> HashCode.Combine (3, ptr)
        | NativeIntSource.MethodTablePtr ptr -> HashCode.Combine (4, ptr)
        | NativeIntSource.MethodTableAuxiliaryDataPtr ptr -> HashCode.Combine (5, ptr)
        | NativeIntSource.PerInstInfoPtr ptr -> HashCode.Combine (18, ptr)
        | NativeIntSource.PerInstDictPtr ptr -> HashCode.Combine (19, ptr)
        | NativeIntSource.MethodHandlePtr ptr -> HashCode.Combine (6, ptr)
        | NativeIntSource.FieldHandlePtr ptr -> HashCode.Combine (7, ptr)
        | NativeIntSource.AssemblyHandle name -> HashCode.Combine (8, name)
        | NativeIntSource.ModuleHandle name -> HashCode.Combine (9, name)
        | NativeIntSource.MetadataImportHandle name -> HashCode.Combine (10, name)
        | NativeIntSource.GcHandlePtr handle -> HashCode.Combine (11, handle)
        | NativeIntSource.EventPipeProviderPtr id -> HashCode.Combine (12, id)
        | NativeIntSource.EventPipeEventPtr id -> HashCode.Combine (13, id)
        | NativeIntSource.SyntheticCrossArrayOffset s -> HashCode.Combine (14, hash s)
        | NativeIntSource.OpaqueHashBits bits -> HashCode.Combine (15, bits)
        | NativeIntSource.LowLevelMonitorPtr id -> HashCode.Combine (16, id)
        | NativeIntSource.WaitHandlePtr id -> HashCode.Combine (17, id)

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

    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.SyntheticCrossArrayOffset s -> SyntheticCrossArrayOffset.cltVerbatim s 1L
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.MethodHandlePtr _
        | NativeIntSource.TypeHandlePtr _
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
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
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
    | FieldRegistryHandle of int64
    | MethodRegistryHandle of int64
    | MethodTablePtr of ConcreteTypeHandle
    | MethodTableAuxiliaryDataPtr of RuntimeTypeHandleTarget
    /// See `NativeIntSource.PerInstInfoPtr`. The eval-stack-flattened
    /// counterpart is `NativeIntSource.PerInstInfoPtr`.
    | PerInstInfoPtr of ConcreteTypeHandle
    /// See `NativeIntSource.PerInstDictPtr`. The eval-stack-flattened
    /// counterpart is `NativeIntSource.PerInstDictPtr`.
    | PerInstDictPtr of ConcreteTypeHandle
    | Managed of ManagedPointerSource
    /// A GC handle stored in a typed-pointer slot (e.g. `void*`, `T*`). Arithmetic
    /// and comparison operations on this case must go through eval-stack
    /// flattening (`EvalStack.ofCliType`) into `NativeIntSource.GcHandlePtr`;
    /// helpers like `NativeIntSource.isZero`/`isNonnegative` and conv ops only
    /// match the `NativeIntSource` form.
    | GcHandlePtr of GcHandleAddress
