namespace WoofWare.PawPrint

/// Resolution of a byref to the storage it names.
///
/// Two questions of position live here, and they are deliberately different in
/// strength. `byteLocation` is the precise one: it folds a projection chain into
/// a container identity plus a flat byte offset, so two byrefs resolved this way
/// can be compared by arithmetic. `sharedStorageKey` is the coarse fallback,
/// answering only "could these two possibly share storage at all" for the
/// chains `byteLocation` cannot resolve. `byteExtent` then answers how far the
/// container reaches, which is what turns a position into a bound.
///
/// The coarse answer exists because the precise one cannot be made total.
/// Reference- and pointer-containing values have no byte image, and explicit
/// layout can place two distinct fields at one address, so there is no
/// injective map from a byref to a byte coordinate. A consumer that treats
/// the coarse answer as if it were precise turns a loud refusal into a silent
/// wrong answer; see `docs/plans/2026-08-14-storage-location-identity.md` §2.
[<RequireQualifiedAccess>]
module internal StorageLocation =
    /// Lazy resolution of the root's `CliType` template. Consumed by
    /// `byteLocation` (via `IlMachineManagedByref.walkProjectionByteOffset`)
    /// only when a `Field` projection appears in the chain. Variants whose `Field`-projection layout cannot be
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

    /// Where a byref lands, as a container plus a flat byte coordinate within it: the root's
    /// own offset inside its container, plus the offset its projection chain walks from
    /// there.
    ///
    /// Returns `None` for any byref whose coordinate can't be computed — a root with no
    /// byte-addressable container at all (`ExposedClassObject`), or a chain the walk cannot
    /// fold (missing template, unsupported projection shape, missing concrete type for a
    /// `ReinterpretAs` target). The caller degrades to the coarse `SharedStorageKey` path.
    /// The `try` spans the container lookup as well as the walk, because a view root's
    /// offset needs a heap lookup that can fail for the same kinds of reason.
    ///
    /// The coordinate is `int64` and is taken as-is. It is not an access offset — nothing
    /// here dereferences either byref — so a coordinate beyond `int32` is a perfectly good
    /// answer, and one that `Unsafe.ByteOffset` reports to the guest. Narrowing it to int32
    /// wraps `ref s.B` displaced by `Int32.MaxValue` onto `ref s.A` displaced by
    /// `Int32.MinValue`: issue #993.
    let private byteLocation
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : (ByteStorageIdentity * int64) option
        =
        match ptr with
        | ManagedPointerSource.Null -> None
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        | ManagedPointerSource.Byref (root, projs) ->
            let templateFor (ty : ConcreteType<ConcreteTypeHandle>) : CliType =
                IlMachineManagedByref.zeroForConcreteType baseClassTypes state ty

            let rootTemplateThunk () = rootTemplate state root

            try
                match ByrefContainer.tryOfRoot state.ManagedHeap root with
                | None -> None
                | Some (container, rootOffset) ->
                    let projectionOffset =
                        IlMachineManagedByref.walkProjectionByteOffset templateFor rootTemplateThunk projs

                    Some (container, rootOffset + projectionOffset)
            with _ ->
                None

    /// Coarse storage discriminator used purely to decide whether two byrefs
    /// *could* share underlying storage when `byteLocation` cannot derive a
    /// flat byte offset (e.g. an unresolved concrete-type for a
    /// `ReinterpretAs` target, or a `StackMemoryByte` whose root offset has
    /// no covering typed cell). `byteLocation` folds `Field` projections
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

    /// How many bytes `identity` spans, in the same flat coordinate space `resolve` reports
    /// offsets in.
    ///
    /// `None` when the storage is gone — a freed native block, a frame that has returned —
    /// or when its size was never recorded. An access to such a container fails at the
    /// access with a diagnostic about *that*, which is the more accurate report, so there is
    /// nothing to be gained by guessing a size here.
    let private byteExtent (state : IlMachineState) (identity : ByteStorageIdentity) : int64 option =
        try
            match identity with
            | ByteStorageIdentity.Array arr ->
                // Shape facts rather than cells, so this is not a read of guest memory:
                // both are recorded at allocation and neither can change afterwards.
                ManagedHeap.tryGetArrayShape arr state.ManagedHeap
                |> Option.map (fun shape -> int64 shape.Length * int64 shape.ElementStride)
            | ByteStorageIdentity.String str ->
                // One past the last character is the null terminator CoreCLR's string layout
                // reserves and `ManagedHeap.getStringChar` addresses, so it belongs to the
                // container. `None` for a string allocated off the standard path, which has
                // no recorded content to measure.
                ManagedHeap.getStringContents str state.ManagedHeap
                |> Option.map (fun contents -> (int64 contents.Length + 1L) * 2L)
            | ByteStorageIdentity.PeByteRange range -> Some (int64 range.Size)
            | ByteStorageIdentity.StaticField (declaringType, field, owner) ->
                IlMachineManagedByref.getStatic owner declaringType field state
                |> Option.map (fun value -> int64 (CliType.sizeOf value))
            | ByteStorageIdentity.StackMemory (thread, frame, block) ->
                IlMachineThreadState.getStackMemoryPool thread frame state
                |> StackMemoryPool.blockSize block
                |> int64
                |> Some
            | ByteStorageIdentity.StackLocal (thread, frame, local) ->
                (IlMachineThreadState.getFrame thread frame state).LocalVariables.[int<uint16> local]
                |> CliType.sizeOf
                |> int64
                |> Some
            | ByteStorageIdentity.StackArgument (thread, frame, arg) ->
                (IlMachineThreadState.getFrame thread frame state).Arguments.[int<uint16> arg]
                |> CliType.sizeOf
                |> int64
                |> Some
            | ByteStorageIdentity.NativeMemory block ->
                NativeMemoryPool.blockSize block state.Kernel.NativeMemoryPool |> int64 |> Some
            | ByteStorageIdentity.HeapObject addr ->
                // Covers a boxed value and a class instance alike: the instance size is the
                // whole flattened field storage, which is the space the field offsets
                // `byteLocation` folds are measured in.
                ManagedHeap.tryGet addr state.ManagedHeap
                |> Option.map (fun allocated -> int64 (CliValueType.SizeOf allocated.Contents).Size)
        with _ ->
            None

    /// Whether a byte range lies within the storage its start pointer names.
    [<RequireQualifiedAccess>]
    type ByteRangeFit =
        /// Every byte of the range is inside the storage.
        | Fits
        /// The range begins before the storage does or ends after it does. Carries the
        /// storage, the range's start coordinate within it, and the storage's extent.
        | Escapes of storage : ByteStorageIdentity * offset : int64 * extent : int64
        /// The start coordinate, the extent, or both could not be derived, so containment is
        /// not decided either way. Distinct from `Fits`: a caller that refuses `Escapes`
        /// lets this past, and so keeps refusing only what it has actually established.
        | Undecided

    /// How the `byteCount` bytes starting where `ptr` points sit inside the storage `ptr`
    /// names.
    let byteRangeFit
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (byteCount : int)
        : ByteRangeFit
        =
        System.Diagnostics.Debug.Assert (
            byteCount >= 0,
            "StorageLocation.byteRangeFit: a negative byte count is not a range"
        )

        match resolve baseClassTypes state ptr with
        | LocationResolution.Unrelatable
        | LocationResolution.Located (_, None) -> ByteRangeFit.Undecided
        | LocationResolution.Located (_, Some (storage, offset)) ->
            match byteExtent state storage with
            | None -> ByteRangeFit.Undecided
            | Some extent ->
                // Rearranged to subtract rather than add, so that a coordinate near the top
                // of the `int64` range is a refusal instead of wrapping onto a low one the
                // comparison would accept. Both operands of the subtraction are
                // non-negative, so it cannot underflow.
                if offset < 0L || offset > extent - int64 byteCount then
                    ByteRangeFit.Escapes (storage, offset, extent)
                else
                    ByteRangeFit.Fits

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

    /// Answer a byref comparison that structural comparison deferred, by resolving both
    /// sides to flat byte coordinates.
    ///
    /// **This decides only what it can positively prove**, and refuses everything else by
    /// re-raising the deferral's own diagnostic. In particular it does *not* infer inequality
    /// from two byrefs landing in different containers, even though that rule would decide
    /// more pairs; counterexamples: two fields of one explicit-layout object overlap; a byref
    /// displaced past its root's extent lands in another root, and ECMA-335 promises no
    /// relative placement between independently declared locals, so `local0 + 1000` may *be*
    /// `local1`; and a `Field` resolved against a reinterpreted larger type can sit outside
    /// the original slot while `mayLeaveRootExtent` still reports it in-extent. The
    /// distinct-container case is
    /// simply not decided here — it is exactly the set of pairs `ceqNormalised` already
    /// answers correctly on its own, so declining costs nothing.
    let resolveCeq
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (outcome : CeqOutcome)
        : bool
        =
        match outcome with
        | CeqOutcome.Decided answer -> answer
        | CeqOutcome.NeedsByteLocation (left, right, diagnostic) ->
            match resolve baseClassTypes state left, resolve baseClassTypes state right with
            | LocationResolution.Located (_, Some (leftStorage, leftOffset)),
              LocationResolution.Located (_, Some (rightStorage, rightOffset)) when leftStorage = rightStorage ->
                // One container means one flat coordinate system, which is the whole content
                // of `ByteStorageIdentity`, so equal coordinates are the same address and
                // unequal ones are not. Field offsets are consulted here (via
                // `walkProjectionByteOffset`), which is precisely what the structural
                // comparison lacked.
                leftOffset = rightOffset
            | _ -> failwith diagnostic

    /// Answer a byref ordering (`cgt.un`, `clt.un`) that structural comparison deferred, by
    /// resolving both sides to flat byte coordinates.
    ///
    /// As with `resolveCeq`, this decides only what one container's coordinate system proves:
    /// two byrefs with a precise coordinate in the same storage are ordered by those
    /// coordinates, which fold the field offsets the structural comparison lacked. Everything
    /// else is refused with the deferral's own diagnostic — two byrefs into different
    /// containers have no relative placement in the model (one local against another, one
    /// array against another), and a byref without a precise coordinate cannot be placed
    /// within its own.
    let resolveOrder
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (outcome : UnsignedOrderOutcome)
        : bool
        =
        match outcome with
        | UnsignedOrderOutcome.Decided answer -> answer
        | UnsignedOrderOutcome.NeedsByteLocation (left, right, question, diagnostic) ->
            match resolve baseClassTypes state left, resolve baseClassTypes state right with
            | LocationResolution.Located (_, Some (leftStorage, leftOffset)),
              LocationResolution.Located (_, Some (rightStorage, rightOffset)) when leftStorage = rightStorage ->
                match question with
                | ByrefOrderQuestion.LeftAbove -> leftOffset > rightOffset
                | ByrefOrderQuestion.LeftBelow -> leftOffset < rightOffset
            | _ -> failwith diagnostic
