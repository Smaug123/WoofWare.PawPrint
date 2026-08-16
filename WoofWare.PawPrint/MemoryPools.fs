namespace WoofWare.PawPrint

/// Whether a new `localloc` block should expose zeroed bytes immediately.
[<RequireQualifiedAccess>]
type MemoryBlockInitialization =
    | ZeroInitialized
    | Uninitialized

/// Frame-owned typed-cell storage for `localloc`. A block stores typed CliType
/// "cells" at known offsets together with a sparse byte overlay for raw byte
/// writes that don't correspond to a typed cell. Bytes that are not covered by
/// either representation default to the block's `Initialization`.
///
/// Cells preserve provenance — for example, `NativeIntSource.FieldHandlePtr`
/// stays a tagged pointer rather than collapsing to its bit pattern. A byte
/// view of a non-byte-addressable cell intentionally fails through the
/// `CliType` byte helpers; that fail-fast is what catches accidental
/// provenance loss.
///
/// Pointers into a block are valid only while the owning method frame is live;
/// if one escapes, later deref fails visibly because the frame-local pool is
/// gone.
type MemoryBlock =
    {
        Size : int
        Initialization : MemoryBlockInitialization
        /// Offset -> typed cell. Cells must not overlap each other; cell ranges
        /// are also disjoint from `Bytes` keys.
        Cells : Map<int, CliType>
        /// Offset -> raw byte. Keys must not lie inside any cell range.
        Bytes : Map<int, byte>
    }

type StackMemoryPool =
    {
        NextBlockId : int
        Blocks : Map<StackMemoryBlockId, MemoryBlock>
    }

/// Native-heap-backed analogue of `StackMemoryPool`: an explicit free list of
/// `MemoryBlock`s scoped to the whole machine state rather than a method frame.
/// Allocations come from `Marshal.AllocHGlobal` / `NativeMemory.Alloc`; the
/// caller is responsible for freeing them via `Marshal.FreeHGlobal` /
/// `NativeMemory.Free`. Freeing deletes the block; subsequent dereferences of
/// any retained byref fail loudly, matching the deterministic-simulator
/// principle of catching use-after-free at the offending access.
type NativeMemoryPool =
    {
        NextBlockId : int
        Blocks : Map<NativeMemoryBlockId, MemoryBlock>
    }

/// A primitive byte read from a memory block, classified by where the byte
/// came from. Callers walking byte ranges use the `Cell` arm to amortise per-
/// cell processing; the byte arms describe a single byte each.
[<RequireQualifiedAccess>]
type MemoryByteSource =
    /// Byte lies inside a typed cell. The cell starts at `cellOffset` and
    /// reading byte `n` of it requires the existing byte helpers
    /// (`CliType.BytesAt`); callers must respect the cell's byte
    /// addressability.
    | Cell of cellOffset : int * cell : CliType
    /// Byte was written through the raw byte overlay.
    | Overlay of byte
    /// Byte was never written but the block was zero-initialised.
    | DefaultZero
    /// Byte was never written and the block is uninitialised. Reads must
    /// fail visibly.
    | Uninitialized

/// Block-level primitives shared by `StackMemoryPool` and `NativeMemoryPool`.
/// Both pools wrap an identical `MemoryBlock` shape; the only difference is
/// where the map lives and how blocks are added/removed. Keeping the cell-and-
/// byte logic here means the two pools never drift.
[<RequireQualifiedAccess>]
module MemoryBlock =
    let empty (initialization : MemoryBlockInitialization) (byteCount : int) : MemoryBlock =
        {
            Size = byteCount
            Initialization = initialization
            Cells = Map.empty
            Bytes = Map.empty
        }

    let internal checkRange
        (operation : string)
        (containerDesc : string)
        (blockLength : int)
        (byteOffset : int)
        (byteCount : int)
        : unit
        =
        if byteOffset < 0 then
            failwith $"%s{operation}: negative byte offset %d{byteOffset} in %s{containerDesc}"

        if byteCount < 0 then
            failwith $"%s{operation}: negative byte count %d{byteCount} in %s{containerDesc}"

        let rangeEnd = int64 byteOffset + int64 byteCount

        if rangeEnd > int64 blockLength then
            failwith
                $"%s{operation}: byte range [%d{byteOffset}, %d{rangeEnd}) is outside %s{containerDesc} of length %d{blockLength}"

    let private rangesIntersect (aOffset : int) (aSize : int) (bOffset : int) (bSize : int) : bool =
        aOffset < bOffset + bSize && bOffset < aOffset + aSize

    /// Find the unique cell whose covered range contains `offset`, if any.
    /// Cells don't overlap, so at most one matches.
    let private tryFindCellCoveringRaw (offset : int) (block : MemoryBlock) : (int * CliType) option =
        // F# `Map` iterates entries in key order, so we can stop as soon as a
        // cell starts past `offset` (no later cell can contain `offset`) or as
        // soon as we find a cover.
        use enumerator =
            (block.Cells :> System.Collections.Generic.IEnumerable<_>).GetEnumerator ()

        let mutable result = None
        let mutable continueScan = true

        while continueScan && enumerator.MoveNext () do
            let kvp = enumerator.Current
            let cellOffset = kvp.Key
            let cell = kvp.Value

            if cellOffset > offset then
                continueScan <- false
            else
                let cellSize = CliType.sizeOf cell

                if offset < cellOffset + cellSize then
                    result <- Some (cellOffset, cell)
                    continueScan <- false

        result

    /// Find the unique cell whose covered range contains `offset`, if any.
    /// Returns `None` for an in-range offset that no cell covers, and also for
    /// an out-of-range `offset` (consistent with the `try` prefix).
    let tryFindCellCovering (offset : int) (block : MemoryBlock) : (int * CliType) option =
        if offset < 0 || offset >= block.Size then
            None
        else
            tryFindCellCoveringRaw offset block

    /// Return the cell that begins at exactly `offset`, if any.
    let tryReadCell (offset : int) (block : MemoryBlock) : CliType option = Map.tryFind offset block.Cells

    /// Classify a single byte position. Callers walking byte ranges use the
    /// `Cell` arm to dispatch through the existing typed-cell byte helpers.
    let private readByteSource (containerDesc : string) (offset : int) (block : MemoryBlock) : MemoryByteSource =
        checkRange "MemoryBlock.readByteSource" containerDesc block.Size offset 1

        match tryFindCellCoveringRaw offset block with
        | Some (cellOffset, cell) -> MemoryByteSource.Cell (cellOffset, cell)
        | None ->
            match Map.tryFind offset block.Bytes with
            | Some b -> MemoryByteSource.Overlay b
            | None ->
                match block.Initialization with
                | MemoryBlockInitialization.ZeroInitialized -> MemoryByteSource.DefaultZero
                | MemoryBlockInitialization.Uninitialized -> MemoryByteSource.Uninitialized

    /// Remove any cells or byte-overlay entries intersecting
    /// `[offset, offset + count)`. Cells are removed wholesale even if they
    /// only partially overlap the requested range.
    let private evictRange (offset : int) (count : int) (block : MemoryBlock) : MemoryBlock =
        if count <= 0 then
            block
        else
            let cells =
                block.Cells
                |> Map.filter (fun cellOffset cell ->
                    not (rangesIntersect cellOffset (CliType.sizeOf cell) offset count)
                )

            let bytes =
                block.Bytes
                |> Map.filter (fun byteOffset _ -> byteOffset < offset || byteOffset >= offset + count)

            if
                Map.count cells = Map.count block.Cells
                && Map.count bytes = Map.count block.Bytes
            then
                block
            else
                { block with
                    Cells = cells
                    Bytes = bytes
                }

    /// Insert a typed cell at `offset`, evicting any cells/bytes whose range
    /// intersects the new cell. The caller is responsible for ensuring the
    /// value is the intended typed view; provenance carried by the value
    /// (such as `NativeIntSource.FieldHandlePtr`) is preserved.
    let writeCell (containerDesc : string) (offset : int) (value : CliType) (block : MemoryBlock) : MemoryBlock =
        let size = CliType.sizeOf value
        checkRange "MemoryBlock.writeCell" containerDesc block.Size offset size

        let evicted = evictRange offset size block

        { evicted with
            Cells = evicted.Cells |> Map.add offset value
        }

    /// Replace an existing cell at `cellOffset` whose new size matches the
    /// existing cell. Used by the byte-write path to install an updated cell
    /// produced by `CliType.WithBytesAtIfChanged`. Throws if no cell exists at
    /// `cellOffset` or the new value's size differs from the existing cell.
    let private replaceCell
        (containerDesc : string)
        (cellOffset : int)
        (updated : CliType)
        (block : MemoryBlock)
        : MemoryBlock
        =
        match Map.tryFind cellOffset block.Cells with
        | None ->
            failwith
                $"MemoryBlock.replaceCell: no cell at offset %d{cellOffset} in %s{containerDesc} (this is an interpreter bug)"
        | Some existing ->
            let existingSize = CliType.sizeOf existing
            let updatedSize = CliType.sizeOf updated

            if existingSize <> updatedSize then
                failwith
                    $"MemoryBlock.replaceCell: refusing to change cell size at offset %d{cellOffset} in %s{containerDesc} (was %d{existingSize}, would be %d{updatedSize})"

            { block with
                Cells = block.Cells |> Map.add cellOffset updated
            }

    /// Write a single byte through the byte overlay. Caller must ensure
    /// `offset` does not lie inside any cell — typically by walking the cell
    /// covering check first and routing cell-resident writes through
    /// `replaceCell` instead.
    let private writeOverlayByte
        (containerDesc : string)
        (offset : int)
        (value : byte)
        (block : MemoryBlock)
        : MemoryBlock
        =
        checkRange "MemoryBlock.writeOverlayByte" containerDesc block.Size offset 1

        match tryFindCellCoveringRaw offset block with
        | Some (cellOffset, _) ->
            failwith
                $"MemoryBlock.writeOverlayByte: byte offset %d{offset} lies inside cell at %d{cellOffset} in %s{containerDesc} (this is an interpreter bug)"
        | None ->
            { block with
                Bytes = block.Bytes |> Map.add offset value
            }

    /// Read `count` bytes starting at `offset`, returning `ValueNone` when any
    /// byte in the range is uninitialised or lies inside a cell whose typed
    /// view is not byte-addressable. Used by writers that want to short-circuit
    /// a write when the bytes already match.
    let tryReadBytes (containerDesc : string) (offset : int) (count : int) (block : MemoryBlock) : byte[] voption =
        let rangeEnd = int64 offset + int64 count

        if offset < 0 || count < 0 || rangeEnd > int64 block.Size then
            ValueNone
        else
            let result = Array.zeroCreate<byte> count
            let mutable readable = true
            let mutable i = 0

            while readable && i < count do
                match readByteSource containerDesc (offset + i) block with
                | MemoryByteSource.Cell (cellOffset, cell) ->
                    match CliType.ByteAddressability cell with
                    | CliByteAddressability.ByteAddressable ->
                        let inCellOffset = offset + i - cellOffset
                        let cellSize = CliType.sizeOf cell
                        let take = min (cellSize - inCellOffset) (count - i)
                        let bytes = CliType.BytesAt inCellOffset take cell
                        Array.blit bytes 0 result i take
                        i <- i + take
                    | CliByteAddressability.Rejected _ -> readable <- false
                | MemoryByteSource.Overlay b ->
                    result.[i] <- b
                    i <- i + 1
                | MemoryByteSource.DefaultZero ->
                    result.[i] <- 0uy
                    i <- i + 1
                | MemoryByteSource.Uninitialized -> readable <- false

            if readable then ValueSome result else ValueNone

    /// Read `count` bytes starting at `offset`. Throws if the range is out of
    /// bounds, contains uninitialised bytes, or crosses a cell whose typed
    /// view is not byte-addressable (a tagged-pointer cell, for instance).
    let readBytes (containerDesc : string) (offset : int) (count : int) (block : MemoryBlock) : byte[] =
        checkRange "MemoryBlock.readBytes" containerDesc block.Size offset count

        let result = Array.zeroCreate<byte> count
        let mutable i = 0

        while i < count do
            let pos = offset + i

            match readByteSource containerDesc pos block with
            | MemoryByteSource.Cell (cellOffset, cell) ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.ByteAddressable ->
                    let inCellOffset = pos - cellOffset
                    let cellSize = CliType.sizeOf cell
                    let take = min (cellSize - inCellOffset) (count - i)
                    let bytes = CliType.BytesAt inCellOffset take cell
                    Array.blit bytes 0 result i take
                    i <- i + take
                | CliByteAddressability.Rejected rejection ->
                    failwith
                        $"MemoryBlock.readBytes: refusing byte view over %s{rejection.Description} at offset %d{cellOffset} in %s{containerDesc}"
            | MemoryByteSource.Overlay b ->
                result.[i] <- b
                i <- i + 1
            | MemoryByteSource.DefaultZero ->
                result.[i] <- 0uy
                i <- i + 1
            | MemoryByteSource.Uninitialized ->
                failwith $"MemoryBlock.readBytes: byte at offset %d{pos} in %s{containerDesc} is uninitialised"

        result

    /// Scatter `bytes` into the block starting at `offset`. Bytes that fall
    /// inside an existing cell are merged through `CliType.WithBytesAtIfChanged`
    /// (preserving the cell's typed shape) and replace the cell in place via
    /// `replaceCell`. Bytes outside any cell are written through the byte
    /// overlay via `writeOverlayByte`. Throws if the range is out of bounds or
    /// crosses a non-byte-addressable cell.
    let writeBytes (containerDesc : string) (offset : int) (bytes : byte[]) (block : MemoryBlock) : MemoryBlock =
        checkRange "MemoryBlock.writeBytes" containerDesc block.Size offset bytes.Length

        let mutable block = block
        let mutable filled = 0

        while filled < bytes.Length do
            let pos = offset + filled

            match tryFindCellCovering pos block with
            | Some (cellOffset, cell) ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.ByteAddressable ->
                    let inCellOffset = pos - cellOffset
                    let cellSize = CliType.sizeOf cell
                    let canTake = cellSize - inCellOffset
                    let take = min canTake (bytes.Length - filled)
                    let cellBytes = bytes.[filled .. filled + take - 1]

                    match CliType.WithBytesAtIfChanged inCellOffset cellBytes cell with
                    | None -> ()
                    | Some updatedCell -> block <- replaceCell containerDesc cellOffset updatedCell block

                    filled <- filled + take
                | CliByteAddressability.Rejected rejection ->
                    failwith
                        $"MemoryBlock.writeBytes: refusing byte view over %s{rejection.Description} at offset %d{cellOffset} in %s{containerDesc}"
            | None ->
                block <- writeOverlayByte containerDesc pos bytes.[filled] block
                filled <- filled + 1

        block

[<RequireQualifiedAccess>]
module StackMemoryPool =
    let empty : StackMemoryPool =
        {
            NextBlockId = 0
            Blocks = Map.empty
        }

    let allocate
        (initialization : MemoryBlockInitialization)
        (byteCount : int)
        (pool : StackMemoryPool)
        : StackMemoryBlockId * StackMemoryPool
        =
        if byteCount < 0 then
            failwith $"StackMemoryPool.allocate: negative byte count %d{byteCount}"

        let blockId = StackMemoryBlockId pool.NextBlockId

        blockId,
        { pool with
            NextBlockId = pool.NextBlockId + 1
            Blocks = pool.Blocks |> Map.add blockId (MemoryBlock.empty initialization byteCount)
        }

    let private getBlock (blockId : StackMemoryBlockId) (pool : StackMemoryPool) : MemoryBlock =
        match pool.Blocks |> Map.tryFind blockId with
        | Some block -> block
        | None -> failwith $"Local memory block %O{blockId} is not live in this method frame"

    /// The byte length of `blockId`, fixed when it was allocated.
    ///
    /// A property of the allocation rather than of anything stored in it, which is why it is
    /// answerable without handing the caller the block: callers want it to bounds-check a
    /// byte-view access before performing one.
    let blockSize (blockId : StackMemoryBlockId) (pool : StackMemoryPool) : int = (getBlock blockId pool).Size

    let private setBlock
        (blockId : StackMemoryBlockId)
        (block : MemoryBlock)
        (pool : StackMemoryPool)
        : StackMemoryPool
        =
        { pool with
            Blocks = pool.Blocks |> Map.add blockId block
        }

    let tryFindCellCovering
        (blockId : StackMemoryBlockId)
        (offset : int)
        (pool : StackMemoryPool)
        : (int * CliType) option
        =
        MemoryBlock.tryFindCellCovering offset (getBlock blockId pool)

    let tryReadCell (blockId : StackMemoryBlockId) (offset : int) (pool : StackMemoryPool) : CliType option =
        MemoryBlock.tryReadCell offset (getBlock blockId pool)

    let writeCell
        (blockId : StackMemoryBlockId)
        (offset : int)
        (value : CliType)
        (pool : StackMemoryPool)
        : StackMemoryPool
        =
        let block = getBlock blockId pool
        let updated = MemoryBlock.writeCell (string blockId) offset value block
        setBlock blockId updated pool

    let tryReadBytes
        (blockId : StackMemoryBlockId)
        (offset : int)
        (count : int)
        (pool : StackMemoryPool)
        : byte[] voption
        =
        MemoryBlock.tryReadBytes (string blockId) offset count (getBlock blockId pool)

    let readBytes (blockId : StackMemoryBlockId) (offset : int) (count : int) (pool : StackMemoryPool) : byte[] =
        MemoryBlock.readBytes (string blockId) offset count (getBlock blockId pool)

    let writeBytes
        (blockId : StackMemoryBlockId)
        (offset : int)
        (bytes : byte[])
        (pool : StackMemoryPool)
        : StackMemoryPool
        =
        let block = getBlock blockId pool
        let updated = MemoryBlock.writeBytes (string blockId) offset bytes block
        setBlock blockId updated pool

[<RequireQualifiedAccess>]
module NativeMemoryPool =
    let empty : NativeMemoryPool =
        {
            NextBlockId = 0
            Blocks = Map.empty
        }

    let allocate
        (initialization : MemoryBlockInitialization)
        (byteCount : int)
        (pool : NativeMemoryPool)
        : NativeMemoryBlockId * NativeMemoryPool
        =
        if byteCount < 0 then
            failwith $"NativeMemoryPool.allocate: negative byte count %d{byteCount}"

        let blockId = NativeMemoryBlockId pool.NextBlockId

        blockId,
        { pool with
            NextBlockId = pool.NextBlockId + 1
            Blocks = pool.Blocks |> Map.add blockId (MemoryBlock.empty initialization byteCount)
        }

    /// Release a native-heap block. Subsequent reads or writes through any
    /// retained byref will fail at `getBlock` with a use-after-free message.
    /// Throws when `blockId` does not name a live block — that is either a
    /// double-free or a free of an address PawPrint never allocated.
    let free (blockId : NativeMemoryBlockId) (pool : NativeMemoryPool) : NativeMemoryPool =
        if not (Map.containsKey blockId pool.Blocks) then
            failwith
                $"NativeMemoryPool.free: %O{blockId} is not a live native-heap block (double free, or free of an address PawPrint never allocated)"

        { pool with
            Blocks = pool.Blocks |> Map.remove blockId
        }

    let isLive (blockId : NativeMemoryBlockId) (pool : NativeMemoryPool) : bool = Map.containsKey blockId pool.Blocks

    let private getBlock (blockId : NativeMemoryBlockId) (pool : NativeMemoryPool) : MemoryBlock =
        match pool.Blocks |> Map.tryFind blockId with
        | Some block -> block
        | None ->
            failwith
                $"Use-after-free: native-heap block %O{blockId} was accessed after `NativeMemory.Free` / `Marshal.FreeHGlobal`"

    /// The byte length of `blockId`, fixed when it was allocated. As for the stack pool, a
    /// property of the allocation rather than of its contents. Reports the same
    /// use-after-free failure as any other access to a freed block.
    let blockSize (blockId : NativeMemoryBlockId) (pool : NativeMemoryPool) : int = (getBlock blockId pool).Size

    let private setBlock
        (blockId : NativeMemoryBlockId)
        (block : MemoryBlock)
        (pool : NativeMemoryPool)
        : NativeMemoryPool
        =
        if not (Map.containsKey blockId pool.Blocks) then
            failwith
                $"NativeMemoryPool.setBlock: %O{blockId} is not a live native-heap block (this is an interpreter bug; the caller should have called `getBlock` first)"

        { pool with
            Blocks = pool.Blocks |> Map.add blockId block
        }

    let tryFindCellCovering
        (blockId : NativeMemoryBlockId)
        (offset : int)
        (pool : NativeMemoryPool)
        : (int * CliType) option
        =
        MemoryBlock.tryFindCellCovering offset (getBlock blockId pool)

    let tryReadCell (blockId : NativeMemoryBlockId) (offset : int) (pool : NativeMemoryPool) : CliType option =
        MemoryBlock.tryReadCell offset (getBlock blockId pool)

    let writeCell
        (blockId : NativeMemoryBlockId)
        (offset : int)
        (value : CliType)
        (pool : NativeMemoryPool)
        : NativeMemoryPool
        =
        let block = getBlock blockId pool
        let updated = MemoryBlock.writeCell (string blockId) offset value block
        setBlock blockId updated pool

    let tryReadBytes
        (blockId : NativeMemoryBlockId)
        (offset : int)
        (count : int)
        (pool : NativeMemoryPool)
        : byte[] voption
        =
        MemoryBlock.tryReadBytes (string blockId) offset count (getBlock blockId pool)

    let readBytes (blockId : NativeMemoryBlockId) (offset : int) (count : int) (pool : NativeMemoryPool) : byte[] =
        MemoryBlock.readBytes (string blockId) offset count (getBlock blockId pool)

    let writeBytes
        (blockId : NativeMemoryBlockId)
        (offset : int)
        (bytes : byte[])
        (pool : NativeMemoryPool)
        : NativeMemoryPool
        =
        let block = getBlock blockId pool
        let updated = MemoryBlock.writeBytes (string blockId) offset bytes block
        setBlock blockId updated pool
