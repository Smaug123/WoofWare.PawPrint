namespace WoofWare.PawPrint

/// Whether a new `localloc` block should expose zeroed bytes immediately.
[<RequireQualifiedAccess>]
type MemoryBlockInitialization =
    | ZeroInitialized
    | Uninitialized

/// Frame-owned typed-cell storage for `localloc`: one block per `localloc`, keyed by
/// `StackMemoryBlockId`, living in the owning method frame.
///
/// A block stores typed `CliType` cells at known offsets together with a sparse byte overlay
/// for raw writes that do not correspond to a cell; bytes covered by neither default to the
/// block's `MemoryBlockInitialization`. Cells preserve provenance — a
/// `NativeIntSource.FieldHandlePtr` stays a tagged pointer rather than collapsing to its bit
/// pattern — and a byte view of a non-byte-addressable cell fails through the `CliType` byte
/// helpers, which is what catches accidental provenance loss.
///
/// The representation is hidden, and the invariant that motivates hiding it is that cells
/// never overlap one another and never overlap a byte-overlay key. Every function below
/// maintains that; a block built any other way could not. Pointers into a block are valid
/// only while the owning frame is live, so an escaped one fails visibly at the next
/// dereference rather than reading a neighbouring frame.
[<Sealed>]
type StackMemoryPool

/// Native-heap analogue of `StackMemoryPool`, scoped to the whole machine state rather than
/// to a method frame: an explicit free list backing `Marshal.AllocHGlobal` /
/// `NativeMemory.Alloc`, whose blocks the guest is responsible for releasing via
/// `Marshal.FreeHGlobal` / `NativeMemory.Free`.
///
/// Freeing deletes the block outright, so a retained byref into it fails at the offending
/// access rather than reading whatever later occupied the storage. Same hidden
/// representation, same non-overlap invariant, as `StackMemoryPool`.
[<Sealed>]
type NativeMemoryPool

[<RequireQualifiedAccess>]
module StackMemoryPool =
    /// A pool owning no blocks. One of these belongs to each method frame.
    val empty : StackMemoryPool

    /// Reserve `byteCount` bytes and return the id addressing them. Ids are unique within a
    /// pool and are never reused, so a stale id fails rather than silently addressing a
    /// later allocation.
    val allocate :
        initialization : MemoryBlockInitialization ->
        byteCount : int ->
        pool : StackMemoryPool ->
            StackMemoryBlockId * StackMemoryPool

    /// The byte length of `blockId`, fixed when it was allocated.
    ///
    /// A property of the allocation rather than of anything stored in it, which is why it is
    /// answerable without handing the caller the block: callers want it to bounds-check a
    /// byte-view access before performing one.
    val blockSize : blockId : StackMemoryBlockId -> pool : StackMemoryPool -> int

    /// The cell covering `offset` together with the offset it starts at, or `None` if no
    /// cell covers it. Unlike `tryReadCell`, `offset` need not be where the cell begins.
    val tryFindCellCovering :
        blockId : StackMemoryBlockId -> offset : int -> pool : StackMemoryPool -> (int * CliType) option

    /// The cell beginning at exactly `offset`, or `None`. A byte written through the overlay
    /// is not a cell, so this also answers "was this offset written as a typed value".
    val tryReadCell : blockId : StackMemoryBlockId -> offset : int -> pool : StackMemoryPool -> CliType option

    /// Fail unless `blockId`'s representation satisfies the non-overlap invariant described
    /// on the type. For tests: every operation below maintains the invariant, and this is
    /// how a test asserts that it did without seeing the representation.
    val internal checkInvariants : blockId : StackMemoryBlockId -> pool : StackMemoryPool -> unit

    /// Store `value` as a typed cell at `offset`, evicting whatever the new cell's byte range
    /// overlaps: every overlay byte within the range, and every intersecting cell. A cell the
    /// new one only partly covers keeps its uncovered bytes, so the block reads back as if the
    /// new cell's bytes had simply been written over the old ones; that needs the old cell to
    /// have a byte image, and partly covering a cell without one (a tagged pointer, say) fails
    /// rather than dropping the rest of it. The eviction is what keeps the non-overlap
    /// invariant true, and it is why a stale byte cannot resurface once a covering cell is
    /// later displaced.
    val writeCell :
        blockId : StackMemoryBlockId -> offset : int -> value : CliType -> pool : StackMemoryPool -> StackMemoryPool

    /// The `count` bytes at `offset`, or `ValueNone` if any of them was never written in a
    /// block that was not zero-initialised. Reading an uninitialised byte is a guest bug, so
    /// callers that cannot handle it should use `readBytes` and fail.
    val tryReadBytes :
        blockId : StackMemoryBlockId -> offset : int -> count : int -> pool : StackMemoryPool -> byte[] voption

    /// The `count` bytes at `offset`, failing if any was never written in a block that was
    /// not zero-initialised.
    val readBytes : blockId : StackMemoryBlockId -> offset : int -> count : int -> pool : StackMemoryPool -> byte[]

    /// `readBytes`, for a caller that can carry a byte naming a native int PawPrint models as an
    /// identity rather than as an address (see `UInt8Source`) instead of demanding a number for
    /// it. `readBytes` is this, with every byte required to be a number.
    val readNamedBytes :
        blockId : StackMemoryBlockId -> offset : int -> count : int -> pool : StackMemoryPool -> UInt8Source[]

    /// Overwrite the `bytes.Length` bytes at `offset`.
    ///
    /// Unlike `writeCell`, this evicts nothing. A byte landing inside a byte-addressable
    /// cell edits that cell in place, so the cell — and its provenance — survives a partial
    /// overwrite and stays visible to `tryReadCell`; a byte landing outside every cell goes
    /// to the raw overlay. A byte landing inside a cell that is *not* byte-addressable, such
    /// as a tagged `NativeIntSource`, fails rather than silently collapsing the tag to its
    /// bit pattern.
    val writeBytes :
        blockId : StackMemoryBlockId -> offset : int -> bytes : byte[] -> pool : StackMemoryPool -> StackMemoryPool

[<RequireQualifiedAccess>]
module NativeMemoryPool =
    /// A pool owning no blocks. The machine state holds exactly one.
    val empty : NativeMemoryPool

    /// Reserve `byteCount` bytes and return the id addressing them. As for the stack pool,
    /// ids are never reused, so a use-after-free fails rather than addressing whatever was
    /// allocated next.
    val allocate :
        initialization : MemoryBlockInitialization ->
        byteCount : int ->
        pool : NativeMemoryPool ->
            NativeMemoryBlockId * NativeMemoryPool

    /// Release `blockId`. Fails on a double free. Every subsequent access to the block fails
    /// too, which is the point: use-after-free is reported at the offending access.
    val free : blockId : NativeMemoryBlockId -> pool : NativeMemoryPool -> NativeMemoryPool

    /// Whether `blockId` is still allocated. For callers that must answer the question
    /// without provoking the use-after-free failure every other accessor raises.
    val isLive : blockId : NativeMemoryBlockId -> pool : NativeMemoryPool -> bool

    /// How many blocks the pool still owns. Ids are never reused, so this is a leak count
    /// rather than a high-water mark: a guest that released everything it allocated leaves
    /// zero, whatever it did in between. That is the only way to observe a native-heap leak,
    /// since a leaked block is invisible to the guest itself.
    val liveBlockCount : pool : NativeMemoryPool -> int

    /// The byte length of `blockId`, fixed when it was allocated. As for the stack pool, a
    /// property of the allocation rather than of its contents. Reports the same
    /// use-after-free failure as any other access to a freed block.
    val blockSize : blockId : NativeMemoryBlockId -> pool : NativeMemoryPool -> int

    /// The cell covering `offset` together with the offset it starts at, or `None` if no
    /// cell covers it. Unlike `tryReadCell`, `offset` need not be where the cell begins.
    val tryFindCellCovering :
        blockId : NativeMemoryBlockId -> offset : int -> pool : NativeMemoryPool -> (int * CliType) option

    /// The cell beginning at exactly `offset`, or `None`. A byte written through the overlay
    /// is not a cell, so this also answers "was this offset written as a typed value".
    val tryReadCell : blockId : NativeMemoryBlockId -> offset : int -> pool : NativeMemoryPool -> CliType option

    /// Fail unless `blockId`'s representation satisfies the non-overlap invariant described
    /// on `StackMemoryPool`. For tests, as there.
    val internal checkInvariants : blockId : NativeMemoryBlockId -> pool : NativeMemoryPool -> unit

    /// Store `value` as a typed cell at `offset`, evicting whatever the new cell's byte range
    /// overlaps, as for the stack pool: a partly covered cell keeps its uncovered bytes, and
    /// partly covering a cell with no byte image fails.
    val writeCell :
        blockId : NativeMemoryBlockId -> offset : int -> value : CliType -> pool : NativeMemoryPool -> NativeMemoryPool

    /// The `count` bytes at `offset`, or `ValueNone` if any of them was never written in a
    /// block that was not zero-initialised.
    val tryReadBytes :
        blockId : NativeMemoryBlockId -> offset : int -> count : int -> pool : NativeMemoryPool -> byte[] voption

    /// The `count` bytes at `offset`, failing if any was never written in a block that was
    /// not zero-initialised.
    val readBytes : blockId : NativeMemoryBlockId -> offset : int -> count : int -> pool : NativeMemoryPool -> byte[]

    /// `readBytes`, for a caller that can carry a byte naming a native int PawPrint models as an
    /// identity rather than as an address (see `UInt8Source`) instead of demanding a number for
    /// it. `readBytes` is this, with every byte required to be a number.
    val readNamedBytes :
        blockId : NativeMemoryBlockId -> offset : int -> count : int -> pool : NativeMemoryPool -> UInt8Source[]

    /// Overwrite the `bytes.Length` bytes at `offset`. As for the stack pool, this evicts
    /// nothing: a byte inside a byte-addressable cell edits that cell in place, a byte
    /// outside every cell goes to the raw overlay, and a byte inside a non-byte-addressable
    /// cell fails rather than discarding the tag.
    val writeBytes :
        blockId : NativeMemoryBlockId -> offset : int -> bytes : byte[] -> pool : NativeMemoryPool -> NativeMemoryPool
