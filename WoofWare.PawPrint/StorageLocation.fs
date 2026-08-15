namespace WoofWare.PawPrint

/// Resolution of a byref to the storage it names.
///
/// Two questions live here, and they are deliberately different in strength.
/// `byteLocation` is the precise one: it folds a projection chain into a
/// container identity plus a flat byte offset, so two byrefs resolved this way
/// can be compared by arithmetic. `sharedStorageKey` is the coarse fallback,
/// answering only "could these two possibly share storage at all" for the
/// chains `byteLocation` cannot resolve.
///
/// The coarse answer exists because the precise one cannot be made total.
/// Reference- and pointer-containing values have no byte image, and explicit
/// layout can place two distinct fields at one address, so there is no
/// injective map from a byref to a byte coordinate. A consumer that treats
/// the coarse answer as if it were precise turns a loud refusal into a silent
/// wrong answer; see `docs/plans/2026-08-14-storage-location-identity.md` §2.
[<RequireQualifiedAccess>]
module internal StorageLocation =
    /// Lazy resolution of the root's `CliType` template. Used by
    /// `tryProjectionByteOffset` only when a `Field` projection appears in
    /// the chain. Variants whose `Field`-projection layout cannot be
    /// resolved (e.g. `PeByteRange`, `ExposedClassObject`) or
    /// where no typed cell starts at the root byte offset
    /// (`StackMemoryByte` / `NativeMemoryByte`) raise — the caller wraps
    /// in try/with and degrades to the coarse `SharedStorageKey` path.
    let private rootTemplate (state : IlMachineState) (root : ByrefRoot) : CliType =
        match root with
        | ByrefRoot.LocalVariable (thread, frame, local) ->
            (IlMachineThreadState.getFrame thread frame state).LocalVariables.[int<uint16> local]
        | ByrefRoot.Argument (thread, frame, arg) ->
            (IlMachineThreadState.getFrame thread frame state).Arguments.[int<uint16> arg]
        | ByrefRoot.HeapValue addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | ByrefRoot.HeapObjectField (addr, field) ->
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById field
        | ByrefRoot.ArrayElement (arr, index) -> IlMachineThreadState.getArrayValue arr index state
        | ByrefRoot.StaticField (ty, field, owner) ->
            match IlMachineManagedByref.getStatic owner ty field state with
            | Some value -> value
            | None ->
                failwith
                    $"rootTemplate: static field byref %O{field.Get} on %O{ty} in %O{owner} was read before its static slot was initialised"
        | ByrefRoot.StringCharAt _ ->
            // A char is a single UTF-16 unit; no `Field` projection makes
            // sense on it. The caller's try/with degrades to `None`.
            failwith "rootTemplate: StringCharAt root has no Field-projectable template"
        | ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset) ->
            let pool = IlMachineThreadState.getStackMemoryPool thread frame state

            match StackMemoryPool.tryFindCellCovering block byteOffset pool with
            | Some (cellOffset, cell) when cellOffset = byteOffset -> cell
            | _ ->
                failwith
                    $"rootTemplate: no typed cell starts at byte offset %d{byteOffset} of stack memory block %O{block}"
        | ByrefRoot.NativeMemoryByte (block, byteOffset) ->
            match NativeMemoryPool.tryFindCellCovering block byteOffset state.Kernel.NativeMemoryPool with
            | Some (cellOffset, cell) when cellOffset = byteOffset -> cell
            | _ ->
                failwith
                    $"rootTemplate: no typed cell starts at byte offset %d{byteOffset} of native memory block %O{block}"
        | ByrefRoot.PeByteRange _ -> failwith "rootTemplate: PeByteRange root has no Field-projectable template"
        | ByrefRoot.ExposedClassObject _ -> failwith "rootTemplate: ExposedClassObject is a single object reference"

    /// Fold a projection chain to a byte offset relative to the root's
    /// storage origin. Returns `None` for any chain whose offset can't be
    /// computed (missing template, unsupported projection shape, missing
    /// concrete type for a `ReinterpretAs` target, etc.); the caller
    /// degrades to the coarse `SharedStorageKey` path.
    ///
    /// The walk's coordinate is `int64` and is taken as-is. It is not an
    /// access offset — nothing here dereferences either byref — so a
    /// coordinate beyond `int32` is a perfectly good answer, and one that
    /// `Unsafe.ByteOffset` reports to the guest. Narrowing it (as this used to)
    /// wrapped `ref s.B` displaced by `Int32.MaxValue` onto `ref s.A`
    /// displaced by `Int32.MinValue`: issue #993.
    let private tryProjectionByteOffset
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (root : ByrefRoot)
        (projs : ByrefProjection list)
        : int64 option
        =
        let templateFor (ty : ConcreteType<ConcreteTypeHandle>) : CliType =
            IlMachineManagedByref.zeroForConcreteType baseClassTypes state ty

        let rootTemplateThunk () = rootTemplate state root

        try
            Some (IlMachineManagedByref.walkProjectionByteOffset templateFor rootTemplateThunk projs)
        with _ ->
            None

    let private byteLocation
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : (ByteStorageIdentity * int64) option
        =
        match ptr with
        | ManagedPointerSource.Null -> None
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.Array arr, ManagedPointerByteView.arrayBytePosition state arr index byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.String str, int64 charIndex * 2L + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.PeByteRange peByteRange, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.StackMemory (thread, frame, block), int64 rootByteOffset + byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.NativeMemory block, int64 rootByteOffset + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (thread, frame, local) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackLocal (thread, frame, local), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, arg) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackArgument (thread, frame, arg), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringType, field, owner) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StaticField (declaringType, field, owner), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.HeapObject addr, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, field), projs) ->
            // One heap object is one storage container; a field root is a
            // *view* into it at the field's layout offset, not its own
            // container. Under `[StructLayout(LayoutKind.Explicit)]` on a
            // class, two distinct fields can overlap, so a per-field identity
            // would falsely assert disjointness — measured as a live
            // wrong-direction `Memmove` (forward loop re-reading bytes it had
            // overwritten) in
            // `SpanMemmoveOverlappingExplicitLayoutClassFields.cs`.
            //
            // Resolving as if the byref were rooted at the whole object with
            // a leading `Field` projection folds the field's offset within
            // the object into the flat byte coordinate. The projection walk
            // is unchanged in what it computes: `rootTemplate` for
            // `HeapValue` yields the object's `Contents`, and the `Field`
            // step's `getFieldById` is exactly the `DereferenceFieldById`
            // the per-field root used to start from, so this resolves
            // whenever the old shape did.
            tryProjectionByteOffset
                baseClassTypes
                state
                (ByrefRoot.HeapValue addr)
                (ByrefProjection.Field field :: projs)
            |> Option.map (fun byteOffset -> ByteStorageIdentity.HeapObject addr, byteOffset)
        // The MethodTable auxiliary cell is a single object reference;
        // `Field` projections are nonsensical on it, and overlap reasoning
        // through it has no flat byte coordinate.
        | ManagedPointerSource.Byref (ByrefRoot.ExposedClassObject _, _) -> None

    /// Coarse storage discriminator used purely to decide whether two byrefs
    /// *could* share underlying storage when `byteLocation` cannot derive a
    /// flat byte offset (e.g. an unresolved concrete-type for a
    /// `ReinterpretAs` target, or a `StackMemoryByte` whose root offset has
    /// no covering typed cell). `byteLocation` now folds `Field` projections
    /// into a precise byte offset whenever the root template is available,
    /// so the fallback path here is reached only when that resolution
    /// fails; equal keys then mean an overlapping `Memmove` is undecidable
    /// from the byref shape alone and the analyser must fail loud.
    ///
    /// Indexed flat roots (array element, string char) carry their index so
    /// that disjoint cross-element copies like `arr[0].A` ↔ `arr[1].A` get
    /// distinct keys. `HeapObjectField` deliberately does *not* carry its
    /// `FieldId`: two distinct fields of one object can genuinely share bytes
    /// under `[StructLayout(LayoutKind.Explicit)]` on a class, so "could
    /// these share storage" is answered per-object, and only the precise
    /// byte offsets (which consult field layout) may prove two fields of one
    /// object disjoint. `HeapValue` (a whole boxed value) is its own bucket
    /// keyed by address; a boxed value and a class-instance field byref
    /// cannot share an address (each heap allocation has a single object
    /// kind), so the two heap kinds never need reconciling.
    [<RequireQualifiedAccess>]
    type SharedStorageKey =
        | ArrayCell of arr : ManagedHeapAddress * index : int
        | StringChar of str : ManagedHeapAddress * charIndex : int
        | Flat of ByteStorageIdentity
        | HeapValue of ManagedHeapAddress
        | HeapObjectField of obj : ManagedHeapAddress
        | RuntimeTypeAux of RuntimeTypeHandleTarget

    let private sharedStorageKeyOfRoot (root : ByrefRoot) : SharedStorageKey =
        match root with
        | ByrefRoot.ArrayElement (arr, index) -> SharedStorageKey.ArrayCell (arr, index)
        | ByrefRoot.StringCharAt (str, charIndex) -> SharedStorageKey.StringChar (str, charIndex)
        | ByrefRoot.PeByteRange peByteRange -> SharedStorageKey.Flat (ByteStorageIdentity.PeByteRange peByteRange)
        | ByrefRoot.StackMemoryByte (thread, frame, block, _) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StackMemory (thread, frame, block))
        | ByrefRoot.NativeMemoryByte (block, _) -> SharedStorageKey.Flat (ByteStorageIdentity.NativeMemory block)
        | ByrefRoot.LocalVariable (thread, frame, local) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StackLocal (thread, frame, local))
        | ByrefRoot.Argument (thread, frame, arg) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StackArgument (thread, frame, arg))
        | ByrefRoot.StaticField (declaringType, field, owner) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StaticField (declaringType, field, owner))
        | ByrefRoot.HeapValue addr -> SharedStorageKey.HeapValue addr
        | ByrefRoot.HeapObjectField (addr, _) -> SharedStorageKey.HeapObjectField addr
        | ByrefRoot.ExposedClassObject decl -> SharedStorageKey.RuntimeTypeAux decl

    /// Storage discriminator of a byref. Returns `None` for non-byref pointers
    /// (`Null`, `NativeIntPlaceholder`) which cannot participate in shared
    /// storage with another byref under PawPrint's model.
    let private sharedStorageKey (ptr : ManagedPointerSource) : SharedStorageKey option =
        match ptr with
        | ManagedPointerSource.Byref (root, _) -> Some (sharedStorageKeyOfRoot root)
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _ -> None

    /// What is known about the storage a pointer names.
    ///
    /// The coarse key and the precise coordinate are deliberately *both* carried on
    /// `Located`, rather than being alternatives. Consumers compare two resolutions, and
    /// they degrade **pairwise**: if either side lacks a precise offset, both sides fall
    /// back to the coarse key. A representation that dropped the coarse key once a precise
    /// one was available would make such a pair incomparable — `ByteStorageIdentity.Array
    /// arr` has lost the element index that `SharedStorageKey.ArrayCell (arr, index)`
    /// carries — leaving the consumer to either call a possibly-aliasing pair disjoint or
    /// reject an unrelated one.
    [<RequireQualifiedAccess>]
    type LocationResolution =
        /// Not a byref, so it shares storage with nothing.
        | Unrelatable
        /// A byref whose container is known. `precise` is its flat byte coordinate, present
        /// only when the projection chain resolves to one.
        | Located of coarse : SharedStorageKey * precise : (ByteStorageIdentity * int64) option

    /// Resolve a pointer to the storage it names. This is the only way in: `byteLocation`
    /// and `sharedStorageKey` are private precisely so that a caller cannot obtain a precise
    /// coordinate without also holding the coarse key it degrades to.
    let resolve
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : LocationResolution
        =
        match sharedStorageKey ptr with
        | None -> LocationResolution.Unrelatable
        | Some coarse -> LocationResolution.Located (coarse, byteLocation baseClassTypes state ptr)

    /// Which direction a byte-range copy between two resolved pointers must run.
    [<RequireQualifiedAccess>]
    type OverlapVerdict =
        /// Either provably disjoint, or overlapping with `dest` at or before `src`: a
        /// forward loop cannot clobber a byte it has yet to read.
        | CopyForwards
        /// `src` strictly precedes `dest` inside one storage and the ranges overlap, so the
        /// loop must walk backwards.
        | CopyBackwards
        /// The two may share storage, but no flat coordinate is available on at least one
        /// side, so the direction cannot be derived. Callers must fail loud rather than
        /// assume forwards. Carries the shared key for the diagnostic.
        | Undecidable of sharedStorage : SharedStorageKey

    /// Decide copy direction from two resolutions. Pure, and total over the resolution
    /// type — the partiality that `byteLocation` cannot avoid surfaces as `Undecidable`
    /// rather than as a wrong answer.
    let overlapVerdict (src : LocationResolution) (dest : LocationResolution) (byteCount : int) : OverlapVerdict =
        match src, dest with
        | LocationResolution.Located (_, Some (srcStorage, srcOffset)),
          LocationResolution.Located (_, Some (destStorage, destOffset)) ->
            if
                srcStorage = destStorage
                && srcOffset < destOffset
                && destOffset < srcOffset + int64 byteCount
            then
                OverlapVerdict.CopyBackwards
            else
                // Either distinct byte storages, which cannot overlap, or one storage in
                // which `dest` does not start strictly inside `src`'s range.
                OverlapVerdict.CopyForwards
        | LocationResolution.Located (srcCoarse, _), LocationResolution.Located (destCoarse, _) when
            srcCoarse = destCoarse
            ->
            OverlapVerdict.Undecidable srcCoarse
        | _ ->
            // Distinct coarse keys, or a non-byref endpoint: no shared storage is possible
            // under the model, so overlap is not either.
            OverlapVerdict.CopyForwards
