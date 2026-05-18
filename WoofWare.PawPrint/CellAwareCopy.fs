namespace WoofWare.PawPrint

/// Direction policy for the byte-range copy driven by `CellAwareCopy.copy`.
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

[<RequireQualifiedAccess>]
module internal CellAwareCopy =
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

    let private projectionByteOffset (projs : ByrefProjection list) : int64 option =
        let rec loop (byteOffset : int64) (projs : ByrefProjection list) : int64 option =
            match projs with
            | [] -> Some byteOffset
            | ByrefProjection.ReinterpretAs _ :: rest -> loop byteOffset rest
            | ByrefProjection.ByteOffset offset :: rest -> loop (byteOffset + int64 offset) rest
            | ByrefProjection.Field _ :: _ -> None

        loop 0L projs

    let private byteLocation
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : (ByteStorageIdentity * int64) option
        =
        match ptr with
        | ManagedPointerSource.Null -> None
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.Array arr,
                ManagedPointerByteView.arrayBytePosition baseClassTypes state arr index byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.String str, int64 charIndex * 2L + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.PeByteRange peByteRange, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.StackMemory (thread, frame, block), int64 rootByteOffset + byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.NativeMemory block, int64 rootByteOffset + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (thread, frame, local), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackLocal (thread, frame, local), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, arg), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackArgument (thread, frame, arg), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringType, field), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StaticField (declaringType, field), byteOffset)
        // These roots do not expose a stable flat byte coordinate here. The
        // supported Buffer_MemMove overlap paths are flat byte-storage-backed;
        // if aliased overlap on these roots appears, extend this model rather
        // than guessing a projection.
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue _, _)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField _, _)
        | ManagedPointerSource.Byref (ByrefRoot.MethodTableExposedClassObject _, _) -> None

    /// Coarse storage discriminator used purely to decide whether two byrefs
    /// *could* share underlying storage when `byteLocation` cannot derive a
    /// flat byte offset (e.g. `Field`-projected residuals on either flat or
    /// heap-rooted byrefs). Heap-backed roots are bucketed by their heap
    /// address so an overlapping `Memmove` over a boxed value or a class's
    /// struct field doesn't slip through to the silent forward path.
    [<RequireQualifiedAccess>]
    type private SharedStorageKey =
        | Flat of ByteStorageIdentity
        | Heap of ManagedHeapAddress
        | RuntimeTypeAux of RuntimeTypeHandleTarget

    let private sharedStorageKeyOfRoot (root : ByrefRoot) : SharedStorageKey =
        match root with
        | ByrefRoot.ArrayElement (arr, _) -> SharedStorageKey.Flat (ByteStorageIdentity.Array arr)
        | ByrefRoot.StringCharAt (str, _) -> SharedStorageKey.Flat (ByteStorageIdentity.String str)
        | ByrefRoot.PeByteRange peByteRange -> SharedStorageKey.Flat (ByteStorageIdentity.PeByteRange peByteRange)
        | ByrefRoot.StackMemoryByte (thread, frame, block, _) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StackMemory (thread, frame, block))
        | ByrefRoot.NativeMemoryByte (block, _) -> SharedStorageKey.Flat (ByteStorageIdentity.NativeMemory block)
        | ByrefRoot.LocalVariable (thread, frame, local) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StackLocal (thread, frame, local))
        | ByrefRoot.Argument (thread, frame, arg) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StackArgument (thread, frame, arg))
        | ByrefRoot.StaticField (declaringType, field) ->
            SharedStorageKey.Flat (ByteStorageIdentity.StaticField (declaringType, field))
        // A boxed value and a field of the same heap object share the same
        // heap allocation; either kind of byref to the same address can
        // alias the other's bytes.
        | ByrefRoot.HeapValue addr -> SharedStorageKey.Heap addr
        | ByrefRoot.HeapObjectField (addr, _) -> SharedStorageKey.Heap addr
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
