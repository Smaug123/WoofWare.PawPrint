namespace WoofWare.PawPrint

/// Direction policy for the byte-range copy driven by `CellAwareMemOps.copy`.
///
/// `Memmove` mirrors `Buffer.Memmove` semantics: when src and dest alias the
/// same flat byte storage with src strictly before dest, the loop walks
/// backwards so writes do not clobber later reads.
///
/// `CpblkForward` mirrors `cpblk` (ECMA-335 III.3.30): the loop always walks
/// forwards, and behaviour on overlap is the caller's problem. `Unsafe.CopyBlock`
/// / `Unsafe.CopyBlockUnaligned` lower to `cpblk`, so they take this policy.
type internal CellAwareCopyPolicy =
    | Memmove
    | CpblkForward

/// Bulk byte-range operations over PawPrint's typed-cell storage model.
///
/// PawPrint stores values as typed `CliType` cells rather than as a flat byte
/// array, so the BCL's byte-oriented bulk primitives (`Buffer.Memmove`,
/// `SpanHelpers.Memmove`, `SpanHelpers.ClearWithoutReferences`, `cpblk`, ...)
/// cannot simply be replayed byte by byte: cells that are not byte-addressable
/// (object references, runtime pointers, non-`Verbatim` `NativeIntSource`
/// provenance) have no byte rendering to walk. Every operation here therefore
/// prefers structural steps when the endpoints anchor cell-aware roots, and
/// falls back to the byte walk for genuinely flat storage.
///
/// A struct's bytes split in two: those some field covers, which
/// `tryWholeCellMoveAt` moves as a typed cell, and those none does — alignment
/// filler — which `tryPaddingMoveAt` moves as bytes out of the preserved image.
/// Together they are total over a struct, which is what lets a move whose range
/// starts or ends *inside* a reference-containing element make progress at all.
[<RequireQualifiedAccess>]
module internal CellAwareMemOps =
    let private byteTemplate : CliType = CliType.Numeric (CliNumericType.UInt8 0uy)

    let private byteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Byte is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Byte handle %O{handle} not found")

    let private readByte
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : byte
        =
        match IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr byteTemplate with
        | CliType.Numeric (CliNumericType.UInt8 b) -> b
        | other -> failwith $"%s{operation}: byte-view read returned non-byte value %O{other}"

    let private writeByte
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : byte)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefBytesOrTypedCell
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.UInt8 value))

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
            Some (int64 (IlMachineManagedByref.walkProjectionByteOffset templateFor rootTemplateThunk projs))
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
                ByteStorageIdentity.Array arr,
                ManagedPointerByteView.arrayBytePosition baseClassTypes state arr index byteOffset
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
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, field) as root, projs) ->
            tryProjectionByteOffset baseClassTypes state root projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.HeapObjectField (addr, field), byteOffset)
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
    /// distinct keys. `HeapObjectField` carries its `FieldId` for the same
    /// reason. `HeapValue` (a whole boxed value) is its own bucket keyed by
    /// address; a boxed value and a class-instance field byref cannot share
    /// an address (each heap allocation has a single object kind).
    [<RequireQualifiedAccess>]
    type private SharedStorageKey =
        | ArrayCell of arr : ManagedHeapAddress * index : int
        | StringChar of str : ManagedHeapAddress * charIndex : int
        | Flat of ByteStorageIdentity
        | HeapValue of ManagedHeapAddress
        | HeapObjectField of obj : ManagedHeapAddress * field : FieldId
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
        | ByrefRoot.HeapObjectField (addr, field) -> SharedStorageKey.HeapObjectField (addr, field)
        | ByrefRoot.ExposedClassObject decl -> SharedStorageKey.RuntimeTypeAux decl

    /// Storage discriminator of a byref. Returns `None` for non-byref pointers
    /// (`Null`, `NativeIntPlaceholder`) which cannot participate in shared
    /// storage with another byref under PawPrint's model.
    let private sharedStorageKey (ptr : ManagedPointerSource) : SharedStorageKey option =
        match ptr with
        | ManagedPointerSource.Byref (root, _) -> Some (sharedStorageKeyOfRoot root)
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _ -> None

    let private shouldCopyBackwards
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (byteCount : int)
        : bool
        =
        match byteLocation baseClassTypes state src, byteLocation baseClassTypes state dest with
        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) when srcStorage = destStorage ->
            srcOffset < destOffset && destOffset < srcOffset + int64 byteCount
        | Some _, Some _ ->
            // Distinct flat byte storages — disjoint, no overlap is possible.
            false
        | _ ->
            // `byteLocation` could not compute a precise flat byte offset for
            // at least one side (e.g. residual `Field` projections, or a
            // heap-rooted byref). If the byrefs nonetheless share root
            // storage (flat, heap-allocated, or MT auxiliary cell), we
            // cannot determine the safe direction for `Memmove` semantics,
            // so we must fail loud rather than silently picking a forward
            // loop that could corrupt overlapping writes.
            match sharedStorageKey src, sharedStorageKey dest with
            | Some s, Some d when s = d ->
                failwith
                    $"%s{operation}: cannot determine overlap direction for byrefs sharing storage %A{s} (residual projections lack a flat byte offset). src=%O{src}, dest=%O{dest}, byteCount=%d{byteCount}"
            | _ ->
                // Distinct storage discriminators or a non-byref endpoint —
                // overlap is impossible under the model.
                false

    /// All residual projections must be plain `Field` projections. The
    /// fast-path uses `readManagedByref` on the residual, which dispatches
    /// through `readProjectedValue`: that helper supports `Field` cleanly,
    /// short-circuits `ReinterpretAs` only for same-width primitive families
    /// (and throws otherwise), and unconditionally throws on `ByteOffset`.
    /// Allowing interior `ReinterpretAs` here would turn the fast path into
    /// a host failure for shapes the byte-walk fallback would otherwise
    /// service (e.g. a byte view built from a non-trailing `ReinterpretAs`
    /// such as `Unsafe.As<int, S>(ref arr[0]).B`), so the fast path
    /// declines and lets the byte-walk peel the projection.
    let private isPlainResidual (projs : ByrefProjection list) : bool =
        projs
        |> List.forall (fun proj ->
            match proj with
            | ByrefProjection.Field _ -> true
            | ByrefProjection.ReinterpretAs _
            | ByrefProjection.ByteOffset _ -> false
        )

    /// Strip a trailing byte-view suffix (`[..., ReinterpretAs _]` or
    /// `[..., ReinterpretAs _; ByteOffset n]`) and return the residual byref
    /// together with the intra-cell byte offset. Returns `None` for non-Byref
    /// pointers, for byrefs whose trailing projections are not a clean
    /// byte-view suffix (e.g. a trailing `Field` or `ByteOffset` without a
    /// preceding `ReinterpretAs` — the latter would already be an
    /// interpreter-bug shape), or for byrefs whose residual after stripping
    /// would still contain an interior `ReinterpretAs` / `ByteOffset`
    /// projection (see `isPlainResidual` for why those must fall back to the
    /// byte-walk).
    let private inCellOffsetAndStripByteView (ptr : ManagedPointerSource) : (ManagedPointerSource * int) option =
        let tryReturn (root : ByrefRoot) (residualRev : ByrefProjection list) (inCellOffset : int) =
            let residual = List.rev residualRev

            if isPlainResidual residual then
                Some (ManagedPointerSource.Byref (root, residual), inCellOffset)
            else
                None

        match ptr with
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | [] -> Some (ptr, 0)
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revRest -> tryReturn root revRest n
            | ByrefProjection.ReinterpretAs _ :: revRest -> tryReturn root revRest 0
            | _ -> None

    /// True when copying the src cell wholesale into the dest cell would
    /// preserve the dest cell's CLI shape: same primitive constructor for
    /// primitives/refs/pointers, same declared concrete type for
    /// value-type structs. This matters because the typed write paths
    /// (`writeRootValue` for `ArrayElement`/`HeapObjectField`/`HeapValue`)
    /// overwrite wholesale and would silently rewrite the cell's shape on
    /// a mismatch.
    ///
    /// No test distinguishes this, and that is measured rather than assumed:
    /// `TestBulkMoveCellAccess` drives a copy on which the guard demonstrably
    /// fires (`long` cells into `double` cells via `MemoryMarshal.Cast`), and
    /// removing it still changes no answer, because the read path carries the
    /// payload through either cell shape. The shapes it refuses are ones the
    /// real runtime accepts, so the difference cannot be asserted
    /// differentially at all — the same position `isCellIdentityCompatible`
    /// records one layer up. It is a deliberate choice of the safe direction;
    /// treat it as load-bearing regardless of what mutating it does to the
    /// suite.
    let private cellsHaveCompatibleShape (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Bool _, CliType.Bool _
        | CliType.Char _, CliType.Char _
        | CliType.ObjectRef _, CliType.ObjectRef _
        | CliType.RuntimePointer _, CliType.RuntimePointer _ -> true
        | CliType.Numeric a, CliType.Numeric b ->
            match a, b with
            | CliNumericType.Int8 _, CliNumericType.Int8 _
            | CliNumericType.UInt8 _, CliNumericType.UInt8 _
            | CliNumericType.Int16 _, CliNumericType.Int16 _
            | CliNumericType.UInt16 _, CliNumericType.UInt16 _
            | CliNumericType.Int32 _, CliNumericType.Int32 _
            | CliNumericType.Int64 _, CliNumericType.Int64 _
            | CliNumericType.NativeInt _, CliNumericType.NativeInt _
            | CliNumericType.NativeFloat _, CliNumericType.NativeFloat _
            | CliNumericType.Float32 _, CliNumericType.Float32 _
            | CliNumericType.Float64 _, CliNumericType.Float64 _ -> true
            | _ -> false
        | CliType.ValueType a, CliType.ValueType b -> a.Declared = b.Declared
        | _ -> false

    /// True for byref roots that designate a typed cell, as opposed to a flat pool of bytes.
    /// Stripping such a byref to a `[]` residual and handing it to `readManagedByref` yields that
    /// cell; doing the same to a byte-pool root fails, because those carry no typed cell at an
    /// arbitrary byte offset. That difference is the whole content of this predicate.
    ///
    /// The byte-walk path through `readManagedByrefBytesAs` serves byte-addressable cells under
    /// either kind of root, but fails outright on cells that have no byte image (`ObjectRef`,
    /// `RuntimePointer`, value types containing those); for those the typed step is the only
    /// correct route. Where both routes are defined they agree, so the typed step is taken
    /// whenever it applies rather than only when bytes would have failed.
    ///
    /// Stack and static slots belong on the typed side and used to be listed with the byte pools,
    /// justified as "the byte-walk path is the modelled access shape for them". That is a policy
    /// dressed as a structural fact, and it cost every bulk move through a local its only route
    /// into reference-containing storage. Gating them on the *contents* being byte-unaddressable
    /// would be worse than either honest answer: the same local would flip between routes
    /// depending on what it happened to hold at the time, making a predicate named for the root's
    /// kind quietly mean "and bytes would not have worked".
    let private rootIsCellAware (root : ByrefRoot) : bool =
        match root with
        | ByrefRoot.ArrayElement _
        | ByrefRoot.HeapValue _
        | ByrefRoot.HeapObjectField _
        | ByrefRoot.ExposedClassObject _
        | ByrefRoot.LocalVariable _
        | ByrefRoot.Argument _
        | ByrefRoot.StaticField _ -> true
        | ByrefRoot.StackMemoryByte _
        | ByrefRoot.NativeMemoryByte _
        | ByrefRoot.PeByteRange _
        | ByrefRoot.StringCharAt _ -> false

    /// True if the residual byref (after stripping its byte-view suffix)
    /// is anchored on a cell-aware root. Used to decide whether to attempt
    /// a whole-cell typed read; for byte-storage roots the byte-walk path
    /// is the modelled access shape and we must not bypass it.
    let private byrefAnchorsCellAwareRoot (ptr : ManagedPointerSource) : bool =
        match ptr with
        | ManagedPointerSource.Byref (root, _) -> rootIsCellAware root
        | _ -> false

    /// Attempt to move one whole storage cell across, with the copy cursor sitting at byte `i` of
    /// the requested range. Returns `Some (state, width)` when a move happened, in which case the
    /// caller advances the cursor by `width`; `None` when no cell could be named, in which case the
    /// caller falls back to a single byte step.
    ///
    /// Each endpoint is considered on its own terms. A byref strips to a typed residual plus an
    /// intra-cell byte offset, and the cell the move should take is whichever one the range
    /// anchored at that offset names — which need not be the residual cell itself, and need not be
    /// at the same offset on both sides. `[InlineArray(8)] struct { object _item; }` is the shape
    /// that forces both points: a `Span<object>` over such a local is one indivisible 64-byte cell,
    /// so copying a single element out of it is a strict sub-range on the source side while the
    /// destination is a whole 8-byte array cell, and once the cursor passes the first element the
    /// two offsets diverge (the array side canonicalises into `arr[k]` at offset 0, the buffer side
    /// walks up through one cell). Object references have no byte image, so there is no bytewise
    /// route to fall back to: naming the cell is the only way to serve this at all.
    ///
    /// Widths are proposed from the *source* by `CliType.CandidateCellExtentsContainingByte` and
    /// validated on both sides by `CliType.CellPathsExactlyCovering`, which stays the authority on
    /// whether a range names a cell. Proposing from one side suffices because a width the
    /// destination can name and the source cannot is not a width we could move anyway, and because
    /// the generator is complete for the source (argued at its definition).
    ///
    /// Three properties this relies on, none of them accidental:
    ///
    /// - **Largest width first is a preference, not a correctness question.** Every validated
    ///   candidate at a given width covers the identical byte range on both sides, so the choices
    ///   differ only in which level of a nesting chain is replaced wholesale, and a same-`Declared`
    ///   value-type replacement over the same extent is not observable. Across widths, one step of
    ///   `n` bytes and `n / k` steps of `k` bytes move the same bytes.
    /// - **Variable widths keep `shouldCopyBackwards`'s overlap guarantee**, which was written when
    ///   every step was one whole cell. A step reads its entire source range before writing its
    ///   destination range, and the cursor then advances by exactly the width moved. So for a
    ///   forward loop (used when the destination is at or before the source in shared storage), a
    ///   step's writes land at or before the bytes it just read, and every later step reads from
    ///   strictly beyond the cursor — untouched. The backward loop is the mirror image. The
    ///   argument never mentions the width, so letting it vary changes nothing.
    /// - **A step always advances.** Widths are positive and at most `cap`, so the caller cannot
    ///   spin. A zero-sized cell would return a zero-width "success" and loop forever, so it is
    ///   rejected here rather than being left to the byte step. No value type should now be able
    ///   to present that shape — `CliValueType.SizeOfFieldStorage` models CoreCLR's floor of one
    ///   byte per value class — so this is a guard on the termination invariant rather than a
    ///   live case, and it stays cheap enough to be worth keeping as one.
    ///
    /// Cells must additionally have compatible CLI shape (`cellsHaveCompatibleShape`), so that the
    /// wholesale typed write does not silently rewrite what the destination cell claims to hold.
    let private tryWholeCellMoveAt
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (bytesRemaining : int)
        (backwards : bool)
        : (IlMachineState * int) option
        =
        if not (byrefAnchorsCellAwareRoot src && byrefAnchorsCellAwareRoot dest) then
            None
        else

        match inCellOffsetAndStripByteView src, inCellOffsetAndStripByteView dest with
        | Some (srcPlain, srcInCell), Some (destPlain, destInCell) ->
            let srcCell = IlMachineState.readManagedByref baseClassTypes state srcPlain
            let destCell = IlMachineState.readManagedByref baseClassTypes state destPlain
            let srcCellSize = CliType.sizeOf srcCell
            let destCellSize = CliType.sizeOf destCell

            // Bytes the cursor can consume within its own cell: forwards, to the cell's end;
            // backwards, back to the cell's start (the cursor byte is the move's *last*).
            let available (inCell : int) (cellSize : int) : int =
                if backwards then inCell + 1 else cellSize - inCell

            let cap =
                List.min
                    [
                        available srcInCell srcCellSize
                        available destInCell destCellSize
                        bytesRemaining
                    ]

            let cursorsAreInsideTheirCells =
                srcInCell >= 0
                && srcInCell < srcCellSize
                && destInCell >= 0
                && destInCell < destCellSize

            if cap <= 0 || not cursorsAreInsideTheirCells then
                None
            else

            // Where a move of `width` bytes anchored at this cursor begins.
            let startOf (inCell : int) (width : int) : int =
                if backwards then inCell - width + 1 else inCell

            /// The cells of `cell` whose extent is exactly the `width`-byte range anchored at
            /// `inCell`, outermost first, as the `FieldId` path to reach each and its contents.
            /// `CellPathsExactlyCovering` reports *fields*, so the case where the range is the
            /// whole of `cell` is this function's own base case rather than something it returns.
            let namedCells (cell : CliType) (inCell : int) (width : int) : (FieldId list * CliType) list =
                let start = startOf inCell width

                let whole =
                    if start = 0 && width = CliType.sizeOf cell then
                        [ [], cell ]
                    else
                        []

                whole @ CliType.CellPathsExactlyCovering start width cell

            let candidateWidths : int list =
                CliType.CandidateCellExtentsContainingByte srcInCell srcCell
                |> List.choose (fun (offset, width) ->
                    let anchoredAtCursor =
                        if backwards then
                            offset + width = srcInCell + 1
                        else
                            offset = srcInCell

                    if anchoredAtCursor && width > 0 && width <= cap then
                        Some width
                    else
                        None
                )

            // Outermost-first at every level, so the first hit is the widest move whose two ends
            // both name a cell and agree on shape.
            let move =
                candidateWidths
                |> List.tryPick (fun width ->
                    namedCells srcCell srcInCell width
                    |> List.tryPick (fun (_, srcContents) ->
                        namedCells destCell destInCell width
                        |> List.tryPick (fun (destPath, destContents) ->
                            if cellsHaveCompatibleShape srcContents destContents then
                                Some (srcContents, destPath, width)
                            else
                                None
                        )
                    )
                )

            match move with
            | None -> None
            | Some (srcContents, destPath, width) ->
                if width <= 0 then
                    failwith
                        $"tryWholeCellMoveAt: chose a non-advancing move of width %d{width}; the caller's cursor would not progress (this is an interpreter bug)"

                let destByref =
                    match destPlain with
                    | ManagedPointerSource.Byref (root, projs) ->
                        ManagedPointerSource.Byref (root, projs @ List.map ByrefProjection.Field destPath)
                    | other ->
                        failwith
                            $"tryWholeCellMoveAt: byte-view stripping returned a non-Byref pointer %O{other} (this is an interpreter bug)"

                let newState =
                    IlMachineState.writeManagedByrefWithBase baseClassTypes state destByref srcContents

                Some (newState, width)
        | _ -> None

    /// Attempt to move a run of *padding* bytes across, with the copy cursor sitting at byte `i` of
    /// the requested range. The same contract as `tryWholeCellMoveAt`: `Some (state, width)` means
    /// the caller advances by `width`, `None` means fall back to a single byte step.
    ///
    /// Padding is the bytes a value type's fields do not cover — alignment filler between them and
    /// at the tail. It belongs to no cell, so `tryWholeCellMoveAt` can propose no width anchored on
    /// it; and inside a value type holding object references it has no byte rendering either, since
    /// `BytesAt` refuses the whole of such a value. A cursor there has nowhere to go, and the copy
    /// stops with "refusing byte view over value type containing object references".
    ///
    /// That state is only reachable when a move starts or ends *inside* an element, because a walk
    /// beginning on an element boundary takes the whole element as one cell and carries its interior
    /// padding along untouched. `Buffer.BulkMoveWithWriteBarrier` produces exactly that: above
    /// 16384 bytes it splits the move into 16384-byte chunks, which for any element size that does
    /// not divide 16384 leaves every chunk after the first starting mid-element.
    ///
    /// The bytes are *copied*, not skipped. For storage that is byte-addressable the single-byte
    /// fallback copies them today, so skipping would be an outright regression there; and
    /// `PreservedBytes` is already the authoritative home for bytes no field covers, so carrying
    /// them costs nothing but the read and the write.
    ///
    /// Both endpoints must land on padding. When only one does — the source's filler lines up with
    /// a live field of the destination, say — the honest move is a byte write into that field, which
    /// is the byte path's job; taking a padding step there would silently write the destination's
    /// *filler* instead and lose the bytes. Declining leaves the byte path to serve it, or to fail
    /// loudly if it cannot, which is the right outcome for a shape this odd.
    ///
    /// The overlap-safety argument documented on `tryWholeCellMoveAt` carries over verbatim: this
    /// step also reads its whole source range before writing its destination range, and advances
    /// the cursor by exactly the width moved, so the direction the driver chose still holds.
    let private tryPaddingMoveAt
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (bytesRemaining : int)
        (backwards : bool)
        : (IlMachineState * int) option
        =
        if not (byrefAnchorsCellAwareRoot src && byrefAnchorsCellAwareRoot dest) then
            None
        else

        match inCellOffsetAndStripByteView src, inCellOffsetAndStripByteView dest with
        | Some (srcPlain, srcInCell), Some (destPlain, destInCell) ->
            let srcCell = IlMachineState.readManagedByref baseClassTypes state srcPlain
            let destCell = IlMachineState.readManagedByref baseClassTypes state destPlain

            match CliType.TryPaddingRunAt srcInCell srcCell, CliType.TryPaddingRunAt destInCell destCell with
            | Some (srcStart, srcLength), Some (destStart, destLength) ->
                // Bytes the cursor can consume within its own run: forwards, to the run's end;
                // backwards, back to the run's start (the cursor byte is the move's *last*).
                let available (inCell : int) (start : int) (length : int) : int =
                    if backwards then
                        inCell - start + 1
                    else
                        start + length - inCell

                let width =
                    List.min
                        [
                            available srcInCell srcStart srcLength
                            available destInCell destStart destLength
                            bytesRemaining
                        ]

                // A run contains the byte it was asked about, so each `available` is at least one,
                // and the driver never calls with fewer than one byte left. A zero would return a
                // non-advancing "success" and spin the caller's loop forever.
                if width <= 0 then
                    failwith
                        $"tryPaddingMoveAt: chose a non-advancing move of width %d{width} at src offset %d{srcInCell} of run [%d{srcStart}, %d{srcStart + srcLength}), dest offset %d{destInCell} of run [%d{destStart}, %d{destStart + destLength}), with %d{bytesRemaining} byte(s) remaining (this is an interpreter bug)"

                // Where a move of `width` bytes anchored at this cursor begins.
                let startOf (inCell : int) : int =
                    if backwards then inCell - width + 1 else inCell

                let bytes = CliType.PaddingBytesAt (startOf srcInCell) width srcCell

                let state =
                    match CliType.WithPaddingBytesAtIfChanged (startOf destInCell) bytes destCell with
                    | None -> state
                    | Some updated ->
                        // Written through the *outer* pointer with no field path appended, because
                        // padding is reachable by no field path. Only bytes no field covers differ,
                        // so the destination cell's CLI shape — and every cell named within it — is
                        // exactly what it was; there is no shape to check compatibility of, which is
                        // why this needs no analogue of `cellsHaveCompatibleShape`.
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state destPlain updated

                Some (state, width)
            | _ -> None
        | _ -> None

    /// One step of the copy loop: prefer a whole typed cell, then a padding run, and let the caller
    /// fall back to a single byte. The two structural steps are disjoint by construction — a byte is
    /// either covered by some field or it is not — so their order is a matter of which question is
    /// cheaper to ask, not of which answer wins.
    let private tryStructuralMoveAt
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (bytesRemaining : int)
        (backwards : bool)
        : (IlMachineState * int) option
        =
        match tryWholeCellMoveAt baseClassTypes state src dest bytesRemaining backwards with
        | Some result -> Some result
        | None -> tryPaddingMoveAt baseClassTypes state src dest bytesRemaining backwards

    /// Attempt to zero a single whole cell, starting at byte offset `i` in the
    /// buffer. Returns `Some (newState, cellSize)` when the cell-aware path
    /// applied and the caller must advance `i` by `cellSize` bytes; `None` when
    /// it did not, and the caller must fall back to a single byte step.
    ///
    /// The single-endpoint analogue of `tryWholeCellMoveAt`, with the same
    /// preconditions minus everything that is about relating two endpoints: the
    /// byref must anchor a cell-aware root, strip to a typed residual at
    /// intra-cell offset 0, and its cell must fit in the bytes remaining. No
    /// shape-compatibility check is needed because `CliType.ZeroLike` derives
    /// the written value from the destination cell itself, so the cell's CLI
    /// shape is preserved by construction.
    ///
    /// As on the copy path, this is not merely an optimisation: a cell that is
    /// not byte-addressable (a runtime pointer, or a `NativeInt` carrying
    /// non-`Verbatim` provenance) has no byte rendering, so the byte-walk
    /// fallback's read-modify-write would fail loudly on it.
    let private tryWholeCellZeroAt
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (dest : ManagedPointerSource)
        (bytesRemaining : int)
        : (IlMachineState * int) option
        =
        if not (byrefAnchorsCellAwareRoot dest) then
            None
        else

        match inCellOffsetAndStripByteView dest with
        | Some (destPlain, 0) ->
            let cell = IlMachineState.readManagedByref baseClassTypes state destPlain
            let cellSize = CliType.sizeOf cell

            // `cellSize <= 0` is rejected for the same reason as on the copy
            // path: a zero-sized cell would advance the caller's cursor by
            // zero and spin forever. As there, no value type should be able to
            // present that shape now that `SizeOfFieldStorage` models CoreCLR's
            // one-byte floor; the guard remains for the termination invariant.
            if cellSize <= 0 || cellSize > bytesRemaining then
                None
            else
                let newState =
                    IlMachineState.writeManagedByrefWithBase baseClassTypes state destPlain (CliType.ZeroLike cell)

                Some (newState, cellSize)
        | Some _
        | None -> None

    /// Zero `byteCount` bytes starting at `dest`, preferring whole-cell typed
    /// writes through `tryWholeCellZeroAt` and falling back to byte-by-byte
    /// stepping otherwise.
    ///
    /// Always walks forwards. Unlike `copy` there is no source to alias
    /// against, so no direction policy is required: every byte in the range
    /// ends up zero regardless of the order in which it is written.
    let clear
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (dest : ManagedPointerSource)
        (byteCount : int)
        : IlMachineState
        =
        let byteConcreteType = byteType operation baseClassTypes state
        let mutable state = state
        let mutable i = 0

        while i < byteCount do
            let destAtI =
                ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

            match tryWholeCellZeroAt baseClassTypes state destAtI (byteCount - i) with
            | Some (newState, cellSize) ->
                state <- newState
                i <- i + cellSize
            | None ->
                state <- writeByte baseClassTypes state destAtI 0uy
                i <- i + 1

        state

    /// Copy `byteCount` bytes from `src` to `dest`, preferring the structural
    /// steps through `tryStructuralMoveAt` — a whole typed cell, or a run of a
    /// struct's alignment filler — and falling back to byte-by-byte stepping
    /// otherwise. The structural path is the only correct option for
    /// non-byte-addressable storage (object references, runtime pointers,
    /// value-types containing those) and preserves the dest cell's CLI shape
    /// and provenance.
    ///
    /// `policy` controls direction: `Memmove` walks backwards when src/dest
    /// alias the same flat byte storage with src strictly before dest, so
    /// writes don't clobber later reads. `CpblkForward` always walks
    /// forwards; cpblk is undefined for overlap (ECMA-335 III.3.30) and
    /// callers (`Unsafe.CopyBlock`, `Unsafe.CopyBlockUnaligned`) inherit
    /// that undefinedness.
    let copy
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (policy : CellAwareCopyPolicy)
        (state : IlMachineState)
        (dest : ManagedPointerSource)
        (src : ManagedPointerSource)
        (byteCount : int)
        : IlMachineState
        =
        let byteConcreteType = byteType operation baseClassTypes state
        let mutable state = state

        let backwards =
            match policy with
            | CellAwareCopyPolicy.CpblkForward -> false
            | CellAwareCopyPolicy.Memmove -> shouldCopyBackwards operation baseClassTypes state src dest byteCount

        if backwards then
            let mutable i = byteCount - 1

            while i >= 0 do
                let srcAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i src

                let destAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

                // For a backward step we are at byte `i`, which must be the
                // *last* byte of its cell (intra-cell offset `cellSize - 1`)
                // for a whole-cell move to be safe; the move covers bytes
                // `[i - cellSize + 1, i]` and advances `i` backwards by
                // `cellSize`.
                match tryStructuralMoveAt baseClassTypes state srcAtI destAtI (i + 1) true with
                | Some (newState, cellSize) ->
                    state <- newState
                    i <- i - cellSize
                | None ->
                    let value = readByte operation baseClassTypes state srcAtI
                    state <- writeByte baseClassTypes state destAtI value
                    i <- i - 1
        else
            let mutable i = 0

            while i < byteCount do
                let srcAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i src

                let destAtI =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

                match tryStructuralMoveAt baseClassTypes state srcAtI destAtI (byteCount - i) false with
                | Some (newState, cellSize) ->
                    state <- newState
                    i <- i + cellSize
                | None ->
                    let value = readByte operation baseClassTypes state srcAtI
                    state <- writeByte baseClassTypes state destAtI value
                    i <- i + 1

        state
