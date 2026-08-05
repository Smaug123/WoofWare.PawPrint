namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Accumulates IL prefix opcodes (ECMA-335 III.2) that have been executed but not yet
/// consumed by their target instruction. Multiple prefixes can stack on a single
/// target instruction (e.g. `volatile. unaligned. ldfld`), so this is a record of
/// independent slots rather than a single-slot DU.
///
/// Prefixes apply to the immediately-following instruction; after the target instruction
/// consumes the state, callers must reset it back to `PrefixState.empty`.
type PrefixState =
    {
        /// `constrained. T` (III.2.1) — applies to the next `callvirt`.
        Constrained : ConcreteTypeHandle option
        /// `volatile.` (III.2.5) — applies to the next ldind/stind/ldfld/stfld/ldobj/stobj/initblk/cpblk.
        Volatile : bool
        /// `tail.` (III.2.4) — applies to the next call/callvirt/calli.
        /// PawPrint never sets this: `tail.` executes as a no-op (see `NullaryIlOp.execute`),
        /// so there is nothing for the following call to consume. It exists for a future
        /// implementation that actually releases the caller's frame.
        Tail : bool
        /// `unaligned. alignment` (III.2.3) — applies to the next ldind/stind/ldfld/stfld/ldobj/stobj/initblk/cpblk.
        Unaligned : uint8 option
        /// `readonly.` (III.2.2) — applies to the next ldelema.
        Readonly : bool
    }

[<RequireQualifiedAccess>]
module PrefixState =
    let empty : PrefixState =
        {
            Constrained = None
            Volatile = false
            Tail = false
            Unaligned = None
            Readonly = false
        }

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
/// `CliType` byte helpers; that fail-fast is the seam that catches accidental
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
    /// Cells don't overlap, so at most one matches. F# `Map` iterates entries
    /// in key order, so we can stop as soon as a cell starts past `offset`
    /// (no later cell can contain `offset`) or as soon as we find a cover.
    let private tryFindCellCoveringRaw (offset : int) (block : MemoryBlock) : (int * CliType) option =
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

    let getBlock (blockId : StackMemoryBlockId) (pool : StackMemoryPool) : MemoryBlock =
        match pool.Blocks |> Map.tryFind blockId with
        | Some block -> block
        | None -> failwith $"Local memory block %O{blockId} is not live in this method frame"

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

    let getBlock (blockId : NativeMemoryBlockId) (pool : NativeMemoryPool) : MemoryBlock =
        match pool.Blocks |> Map.tryFind blockId with
        | Some block -> block
        | None ->
            failwith
                $"Use-after-free: native-heap block %O{blockId} was accessed after `NativeMemory.Free` / `Marshal.FreeHGlobal`"

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

/// Whether a frame was entered by a `newobj` in its caller, and if so under which of
/// the CLI's two object-construction calling conventions. On return, this decides what
/// (if anything) gets pushed onto the caller's evaluation stack.
type ConstructionState =
    /// The frame was entered by an ordinary `call`/`callvirt`, not a `newobj`. Whatever
    /// the method's signature says it returns is what gets pushed.
    | NotConstructing
    /// Fixed-size object: `newobj` allocated the object *before* the constructor ran and
    /// passed its address as `this`, so the address is known up front. On return we push
    /// that address (or, for value types, the object's now-complete contents).
    | Constructing of ManagedHeapAddress
    /// Variable-size object, i.e. one whose instance size depends on its constructor
    /// arguments. CoreCLR flags these `CORINFO_FLG_VAROBJSIZE` (set whenever the
    /// MethodTable `HasComponentSize`; see `jitinterface.cpp`), and both the JIT and the
    /// CoreCLR interpreter special-case `newobj` on them: nothing is allocated up front
    /// and *no `this` is passed* — the constructor allocates the object itself and
    /// effectively returns it, despite a `void` signature. See `importer.cpp`
    /// ("At present this can only be String", `newObjThisPtr = nullptr`) and
    /// `interpreter/compiler.cpp` (`doCallInsteadOfNew = true`).
    ///
    /// Arrays are the CLI's other variable-size case, but they never reach here: array
    /// `newobj` is diverted to the multi-dim allocation path in `executeNewobj`, and
    /// szarrays go through `newarr`. So in practice this case means `System.String`.
    ///
    /// A frame in this state has not yet nominated its object. The constructor must call
    /// `IlMachineState.withSuppliedConstructedObject`, which moves it to `Constructing`;
    /// `returnStackFrame` fails loudly on a frame that returns still in this state.
    | ConstructingVariableSize

/// What `returnStackFrame` should do with the object a constructor frame was constructing,
/// once that constructor returns.
[<RequireQualifiedAccess>]
type ConstructedObjectDisposition =
    /// The ordinary `newobj` convention: push the constructed object (or, for value types,
    /// its now-complete contents) onto the caller's evaluation stack.
    | PushToCaller
    /// The runtime synthesised this exception and pushed its ctor frame itself (see
    /// `IlMachineStateExecution.raiseRuntimeException`). Dispatch the constructed object as
    /// a managed exception instead of pushing it.
    ///
    /// `message`, when present, overwrites `_message` *after* the ctor has run — it must be
    /// applied post-ctor, because the parameterless ctor sets `_message` to the type's
    /// default resource string and would otherwise clobber it. Use it where the CLR would
    /// have called a message-taking ctor overload that PawPrint cannot yet invoke (e.g.
    /// `IndexOutOfRangeException(SR.IndexOutOfRange_ArrayRankIndex)`); leave it `None` to
    /// accept the parameterless ctor's default, which is what the CLR produces when it
    /// throws the exception with no argument.
    | DispatchAsException of message : string option

type MethodReturnState =
    {
        /// Handle to the caller's frame
        JumpTo : FrameId
        WasInitialisingType : ConcreteTypeHandle option
        /// Whether a Newobj instruction in the caller is awaiting an object reference to be
        /// pushed immediately after Ret, and under which construction calling convention.
        Constructing : ConstructionState
        /// The IL offset of the call/callvirt/newobj instruction in the caller that created
        /// this frame. Exception dispatch must use this (not the caller's resumed IlOpIndex)
        /// so that handler lookup sees the call site inside the protected region, even when
        /// the advanced resume PC falls outside it.
        CallSiteIlOpIndex : int
        /// What to do with the constructed object (see `Constructing`) when this frame
        /// returns. Anything other than `PushToCaller` is set by `raiseRuntimeException`,
        /// which runs exception ctors via the dispatch loop.
        ConstructedObjectDisposition : ConstructedObjectDisposition
        /// When true, an exception escaping this frame is wrapped in a fresh
        /// `System.Reflection.TargetInvocationException` whose `_innerException` points at the
        /// original exception object. Used by the `Activator.CreateInstance<T>()` intrinsic to
        /// reproduce CoreCLR's `RuntimeType.CreateInstanceOfT` `try { ctor } catch (Exception e)
        /// { throw new TargetInvocationException(e); }` wrap without synthesising a trampoline
        /// frame. The wrap fires only on unwind across this frame's boundary, so a `try`/`catch`
        /// *inside* the ctor that handles the exception is unaffected.
        WrapExceptionInTargetInvocation : bool
    }

and MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliType ImmutableArray
        /// Index into the stream of IL bytes.
        _IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliType ImmutableArray
        ExecutingMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        StackMemoryPool : StackMemoryPool
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodReturnState option
        Generics : ImmutableArray<ConcreteTypeHandle>
        /// Track which exception regions are currently active (innermost first)
        ActiveExceptionRegions : ExceptionRegion list
        /// When executing finally/fault/filter bodies, we need to know how to resume.
        /// Nested EH inside those bodies pushes a new continuation over the outer one.
        ExceptionContinuations :
            ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        /// Active catch/filter handler body -> caught exception.
        /// TODO: replace with a push/pop active-catch stack so escaped handlers cannot leave stale entries.
        CatchExceptions : Map<ExceptionOffset, CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>>
        /// Prefix opcodes (constrained./volatile./tail./unaligned./readonly.) executed but
        /// not yet consumed by the following instruction. Reset to `PrefixState.empty` after
        /// consumption.
        PendingPrefix : PrefixState
    }

    member this.IlOpIndex = this._IlOpIndex

    member this.ExceptionContinuation =
        this.ExceptionContinuations |> List.tryHead |> Option.map _.Continuation

    /// Set the program counter to an absolute byte offset from the start of the method.
    static member setProgramCounter (absoluteOffset : int) (state : MethodState) =
        let jumped =
            { state with
                _IlOpIndex = absoluteOffset
            }

        let newActiveRegions =
            ExceptionHandling.getActiveRegionsAtOffset jumped.IlOpIndex state.ExecutingMethod

        { jumped with
            ActiveExceptionRegions = newActiveRegions
        }


    static member jumpProgramCounter (bytes : int) (state : MethodState) =
        MethodState.setProgramCounter (state._IlOpIndex + bytes) state

    static member advanceProgramCounter (state : MethodState) =
        let instruction =
            match state.ExecutingMethod.Body with
            | MethodBody.Il instr -> instr.Locations.[state.IlOpIndex]
            | other ->
                failwith
                    $"advanceProgramCounter: executing method %O{state.ExecutingMethod} has no IL body (Body=%A{other})"

        MethodState.jumpProgramCounter (IlOp.NumberOfBytes instruction) state

    static member peekEvalStack (state : MethodState) : EvalStackValue option = EvalStack.Peek state.EvaluationStack

    static member clearEvalStack (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Empty
        }

    static member pushExceptionContinuation
        (scope : ExceptionContinuationScope)
        (cont : ExceptionContinuation<_, _, _>)
        (state : MethodState)
        : MethodState
        =
        match scope, cont with
        | ExceptionContinuationScope.FilterHandler _, ExceptionContinuation.ResumeAfterFilter _
        | ExceptionContinuationScope.FinallyHandler _, ExceptionContinuation.ResumeAfterFinally _
        | ExceptionContinuationScope.FinallyHandler _, ExceptionContinuation.PropagatingException _
        | ExceptionContinuationScope.FaultHandler _, ExceptionContinuation.PropagatingException _ -> ()
        | _ -> failwith $"Exception continuation scope %O{scope} does not match continuation %O{cont}"

        { state with
            ExceptionContinuations =
                {
                    Scope = scope
                    Continuation = cont
                }
                :: state.ExceptionContinuations
        }

    static member popExceptionContinuation
        (state : MethodState)
        : ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option * MethodState
        =
        match state.ExceptionContinuations with
        | [] -> None, state
        | head :: tail ->
            Some head,
            { state with
                ExceptionContinuations = tail
            }

    /// Store the full caught exception for `rethrow`, which must preserve the original
    /// stack trace rather than creating a fresh throw record from the eval-stack object.
    static member setCatchException
        (offset : ExceptionOffset)
        (exn : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : MethodState)
        : MethodState
        =
        { state with
            CatchExceptions = state.CatchExceptions |> Map.add offset exn
        }

    static member clearCatchException (offset : ExceptionOffset) (state : MethodState) : MethodState =
        { state with
            CatchExceptions = state.CatchExceptions |> Map.remove offset
        }

    /// Clear any pending prefix opcodes. Must be called whenever the PC is set to a
    /// non-sequential target (exception handler entry, finally entry) so that a prefix
    /// set before the transfer cannot be consumed by an unrelated instruction in the handler.
    static member clearPendingPrefix (state : MethodState) : MethodState =
        { state with
            PendingPrefix = PrefixState.empty
        }

    static member pushToEvalStack' (e : EvalStackValue) (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Push' e state.EvaluationStack
        }

    static member pushToEvalStack (o : CliType) (state : MethodState) : MethodState =
        { state with
            EvaluationStack = EvalStack.Push o state.EvaluationStack
        }

    /// Pop the eval stack into the given argument slot.
    static member popFromStackToArg (index : int) (state : MethodState) : MethodState =
        let popped, state = MethodState.popFromStack state

        let arg =
            if index < state.Arguments.Length then
                state.Arguments.[index]
            else
                failwith
                    $"Tried to get element {index} of the args list for method {state.ExecutingMethod.Name}, which has only {state.Arguments.Length} elements"

        let popped = EvalStackValue.toCliTypeCoerced arg popped

        { state with
            Arguments = state.Arguments.SetItem (index, popped)
        }

    static member loadArgument (index : int) (state : MethodState) : MethodState =
        // Correct CIL guarantees that we are loading an argument from an index that exists.
        MethodState.pushToEvalStack state.Arguments.[index] state

    static member popFromStack (state : MethodState) : EvalStackValue * MethodState =
        let popped, newStack = EvalStack.Pop state.EvaluationStack

        let state =
            { state with
                EvaluationStack = newStack
            }

        popped, state

    static member popFromStackToVariable (localVariableIndex : int) (state : MethodState) : MethodState =
        if localVariableIndex >= state.LocalVariables.Length then
            failwith
                $"Tried to access zero-indexed local variable %i{localVariableIndex} but only %i{state.LocalVariables.Length} exist"

        if localVariableIndex < 0 || localVariableIndex >= 65535 then
            failwith $"Incorrect CIL encountered: local variable index has value %i{localVariableIndex}"

        let popped, state = MethodState.popFromStack state

        let desiredValue =
            EvalStackValue.toCliTypeCoerced state.LocalVariables.[localVariableIndex] popped

        { state with
            LocalVariables = state.LocalVariables.SetItem (localVariableIndex, desiredValue)
        }

    /// `args` must be populated with entries of the right type.
    /// If `method` is an instance method, `args` must be of length 1+numParams.
    /// If `method` is static, `args` must be of length numParams.
    /// The exception is a frame entered under the variable-size newobj convention
    /// (`ConstructionState.ConstructingVariableSize`), which receives no `this` despite
    /// the constructor being an instance method, and so takes numParams entries.
    static member Empty
        (concreteTypes : AllConcreteTypes)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (containingAssembly : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (args : ImmutableArray<CliType>)
        (returnState : MethodReturnState option)
        : Result<MethodState, WoofWare.PawPrint.AssemblyReference list>
        =
        do
            // A frame entered under the variable-size (CORINFO_FLG_VAROBJSIZE) newobj
            // convention gets no `this` slot even though the constructor is an instance
            // method: CoreCLR calls it with a null this-pointer and the constructor
            // allocates the object itself. See `ConstructionState.ConstructingVariableSize`.
            let isVariableSizeCtorFrame =
                match returnState with
                | None -> false
                | Some returnState ->
                    match returnState.Constructing with
                    | ConstructionState.ConstructingVariableSize -> true
                    | ConstructionState.Constructing _
                    | ConstructionState.NotConstructing -> false

            let expectsThis = not method.IsStatic && not isVariableSizeCtorFrame

            let expected = MethodInfo.arity method + (if expectsThis then 1 else 0)

            if args.Length <> expected then
                let shape =
                    if method.IsStatic then
                        "Static method"
                    elif isVariableSizeCtorFrame then
                        "Variable-size constructor"
                    else
                        "Non-static method"

                failwith
                    $"%s{shape} {method.Name} should have had %i{expected} parameters, but was given %i{args.Length}"

        let localVariableSig =
            match MethodInfo.tryIlBody method with
            | None -> ImmutableArray.Empty
            | Some instr ->
                match instr.LocalVars with
                | None -> ImmutableArray.Empty
                | Some vars -> vars
        // I think valid code should remain valid if we unconditionally localsInit - it should be undefined
        // to use an uninitialised value? Not checked this; TODO.

        let localVars =
            let result = ImmutableArray.CreateBuilder ()

            for var in localVariableSig do
                // Note: This assumes all types have already been concretized
                // If this fails with "ConcreteTypeHandle not found", it means
                // we need to ensure types are concretized before creating the MethodState
                let zero, _ = CliType.zeroOf concreteTypes loadedAssemblies baseClassTypes var
                result.Add zero

            result.ToImmutable ()

        let activeRegions = ExceptionHandling.getActiveRegionsAtOffset 0 method

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
            _IlOpIndex = 0
            Arguments = args
            ExecutingMethod = method
            StackMemoryPool = StackMemoryPool.empty
            ReturnState = returnState
            Generics = methodGenerics
            ActiveExceptionRegions = activeRegions
            ExceptionContinuations = []
            CatchExceptions = Map.empty
            PendingPrefix = PrefixState.empty
        }
        |> Ok
