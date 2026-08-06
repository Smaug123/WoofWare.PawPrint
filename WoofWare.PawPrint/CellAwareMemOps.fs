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
/// prefers whole-cell typed steps when the endpoints anchor cell-aware roots,
/// and falls back to the byte walk for genuinely flat storage.
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
    /// resolved (e.g. `PeByteRange`, `MethodTableExposedClassObject`) or
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
        | ByrefRoot.MethodTableExposedClassObject _ ->
            failwith "rootTemplate: MethodTableExposedClassObject is a single object reference"

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
        | ManagedPointerSource.Byref (ByrefRoot.MethodTableExposedClassObject _, _) -> None

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
        | ByrefRoot.MethodTableExposedClassObject decl -> SharedStorageKey.RuntimeTypeAux decl

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

    /// True for byref roots whose cell shape is non-trivial (each root
    /// designates a typed cell, possibly non-byte-addressable). The byte-walk
    /// path through `readManagedByrefBytesAs` works for byte-addressable
    /// cells under these roots, but fails for non-byte-addressable cells
    /// (ObjectRef, RuntimePointer, value types containing those); the
    /// cell-aware fast path handles both. Byte-storage roots
    /// (`StackMemoryByte`, `NativeMemoryByte`, `PeByteRange`,
    /// `StringCharAt`) and stack-slot roots (`LocalVariable`, `Argument`,
    /// `StaticField`) are not considered here — the byte-walk path is the
    /// modelled access shape for them, and stripping to a `[]` residual
    /// for a `readManagedByref` call into a byte-storage root would fail
    /// because the root carries no typed cell at arbitrary byte offsets.
    let private rootIsCellAware (root : ByrefRoot) : bool =
        match root with
        | ByrefRoot.ArrayElement _
        | ByrefRoot.HeapValue _
        | ByrefRoot.HeapObjectField _
        | ByrefRoot.MethodTableExposedClassObject _ -> true
        | ByrefRoot.LocalVariable _
        | ByrefRoot.Argument _
        | ByrefRoot.StackMemoryByte _
        | ByrefRoot.NativeMemoryByte _
        | ByrefRoot.PeByteRange _
        | ByrefRoot.StaticField _
        | ByrefRoot.StringCharAt _ -> false

    /// True if the residual byref (after stripping its byte-view suffix)
    /// is anchored on a cell-aware root. Used to decide whether to attempt
    /// a whole-cell typed read; for byte-storage roots the byte-walk path
    /// is the modelled access shape and we must not bypass it.
    let private byrefAnchorsCellAwareRoot (ptr : ManagedPointerSource) : bool =
        match ptr with
        | ManagedPointerSource.Byref (root, _) -> rootIsCellAware root
        | _ -> false

    /// Attempt to copy a single whole cell, starting at byte offset `i` in
    /// the buffer. Returns `Some cellSize` when the move succeeded; in that
    /// case `state` has been updated and the caller must advance `i` by
    /// `cellSize` bytes. Returns `None` when the cell-aware path is not
    /// applicable; the caller must then fall back to a single byte step.
    ///
    /// The path is taken iff both src and dest byrefs are anchored on
    /// cell-aware roots (see `rootIsCellAware`), strip to a typed
    /// residual at the same intra-cell byte offset (which must be 0 for a
    /// forward step, or `cellSize - 1` for a backward step), the residual
    /// cells have compatible CLI shape (so the wholesale typed write does
    /// not silently change the dest's shape), and there are at least
    /// `cellSize` bytes remaining in the requested copy. The path is the
    /// only correct option for non-byte-addressable cells (`ObjectRef`,
    /// `RuntimePointer`, value-type cells containing those); for
    /// byte-addressable cells under cell-aware roots it remains correct
    /// (and faster than the byte-by-byte loop), so we take it
    /// unconditionally when the preconditions hold rather than
    /// restricting to non-byte-addressable cells only.
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
        | Some (srcPlain, srcInCell), Some (destPlain, destInCell) when srcInCell = destInCell ->
            let srcCell = IlMachineState.readManagedByref baseClassTypes state srcPlain
            let cellSize = CliType.sizeOf srcCell

            let aligned =
                if backwards then
                    srcInCell = cellSize - 1
                else
                    srcInCell = 0

            // Reject `cellSize <= 0`: a zero-sized value-type cell (PawPrint
            // gives fieldless default-layout structs `sizeOf = 0`) would
            // return `Some (newState, 0)`, and the caller would advance `i`
            // by zero — spinning the copy loop forever for any positive
            // requested byte count. Falling back to the byte step is also
            // wrong here (we'd read a byte off a non-existent cell), so the
            // caller's byte step would itself fail loudly — which is the
            // intended behaviour, since a positive byte copy against a
            // zero-sized cell anchor is an interpreter-bug shape.
            if not aligned || cellSize <= 0 || cellSize > bytesRemaining then
                None
            else
                let destCell = IlMachineState.readManagedByref baseClassTypes state destPlain

                if CliType.sizeOf destCell <> cellSize then
                    None
                elif not (cellsHaveCompatibleShape srcCell destCell) then
                    None
                else
                    let newState =
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state destPlain srcCell

                    Some (newState, cellSize)
        | _ -> None

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
            // zero and spin forever.
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

    /// Copy `byteCount` bytes from `src` to `dest`, preferring whole-cell
    /// typed moves through `tryWholeCellMoveAt` and falling back to
    /// byte-by-byte stepping otherwise. The whole-cell path is the only
    /// correct option for non-byte-addressable cells (object references,
    /// runtime pointers, value-types containing those) and preserves the
    /// dest cell's CLI shape and provenance.
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
                match tryWholeCellMoveAt baseClassTypes state srcAtI destAtI (i + 1) true with
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

                match tryWholeCellMoveAt baseClassTypes state srcAtI destAtI (byteCount - i) false with
                | Some (newState, cellSize) ->
                    state <- newState
                    i <- i + cellSize
                | None ->
                    let value = readByte operation baseClassTypes state srcAtI
                    state <- writeByte baseClassTypes state destAtI value
                    i <- i + 1

        state
