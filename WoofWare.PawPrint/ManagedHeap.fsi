namespace WoofWare.PawPrint

/// The guest's managed memory: every live object, array and string, together with the
/// object headers that carry their monitors.
///
/// The representation is deliberately hidden. Every read of guest memory must be
/// attributable to a caller and a reason, because a race detector has to distinguish the
/// guest touching a field from the interpreter answering a question about a type, and from
/// a debugger dumping state on the side. A caller that reached into the maps directly would
/// be invisible to that machinery, so the type system removes the option: `ManagedHeap`
/// below is the interpreter's surface, and `HeapObserver` is the outside-the-guest one.
///
/// The payload records the accessors hand back (`AllocatedArray`, `ArrayShape`,
/// `AllocatedNonArrayObject`, `SyncBlock`) stay transparent — the constraint is on reaching
/// the heap, not on inspecting what it gave you.
[<Sealed>]
type ManagedHeap

/// Accessors for the *interpreter*: the emulated CLR acting on the guest's behalf.
///
/// This is where guest-visible memory accesses come from, and so where any future
/// instrumentation of them belongs. Contrast `HeapObserver`.
[<RequireQualifiedAccess>]
module ManagedHeap =
    /// A heap with nothing allocated. The first address handed out is 1, so 0 is never a
    /// live address.
    val empty : ManagedHeap

    /// The object header of the object at `addr`. Every live heap object has one, whatever
    /// its payload kind; fails for an address that is not a live allocation.
    val getSyncBlock : addr : ManagedHeapAddress -> heap : ManagedHeap -> SyncBlock

    /// Overwrite the object header of the object at `addr`. Fails for an address that is
    /// not a live allocation rather than conjuring a header for it.
    val setSyncBlock : addr : ManagedHeapAddress -> syncValue : SyncBlock -> heap : ManagedHeap -> ManagedHeap

    /// Every object whose monitor is currently `Held` by `thread`, with the ownership
    /// state, in ascending address order.
    val syncBlocksHeldBy : thread : ThreadId -> heap : ManagedHeap -> (ManagedHeapAddress * LockedSyncBlock) list

    /// Every (object, thread) pair where `thread` is parked in the object's `WaitQueue`
    /// from a `Monitor.Wait`, in ascending address order and then in wait-queue (FIFO)
    /// order within each object. Threads merely contending for the lock do not appear.
    ///
    /// The ordering is part of the contract: callers fold scheduling decisions over this
    /// list, so a different order is a different interleaving.
    val syncBlockWaiters : heap : ManagedHeap -> (ManagedHeapAddress * ThreadId) list

    /// Allocate `ty` at a fresh address, registering an empty object header for it.
    ///
    /// This is the single chokepoint through which every array reaches the heap, and it
    /// checks the denormalised facts on `ty.Shape` rather than trusting them: the stride is
    /// positive, the stride agrees with `ElementZero` and with cell 0, the length agrees
    /// with the cell count, and the length agrees with the product of the per-dimension
    /// lengths. Callers read those facts instead of measuring a cell, which would be
    /// worthless if the recorded values could be wrong.
    val allocateArray : ty : AllocatedArray -> heap : ManagedHeap -> ManagedHeapAddress * ManagedHeap

    /// Allocate `ty` at a fresh address, registering an empty object header for it.
    val allocateNonArray : ty : AllocatedNonArrayObject -> heap : ManagedHeap -> ManagedHeapAddress * ManagedHeap

    /// Allocate a fresh array that is a shallow copy of the array at `source`, as
    /// `System.Array.Clone` promises. The clone gets its own empty object header, so it
    /// never inherits the source's monitor ownership.
    val cloneArray : source : ManagedHeapAddress -> heap : ManagedHeap -> ManagedHeapAddress * ManagedHeap

    /// The concrete type of the object at `alloc`, whichever payload kind it has, or None
    /// if `alloc` is not live.
    val tryGetObjectConcreteType : alloc : ManagedHeapAddress -> heap : ManagedHeap -> ConcreteTypeHandle option

    /// The concrete type of the object at `alloc`, whichever payload kind it has. Fails if
    /// `alloc` is not live.
    val getObjectConcreteType : alloc : ManagedHeapAddress -> heap : ManagedHeap -> ConcreteTypeHandle

    /// Reserve `len + 1` characters of string storage — the trailing slot is the null
    /// terminator CoreCLR's string layout requires — and return the index at which it
    /// starts.
    val allocateString : len : int -> heap : ManagedHeap -> int * ManagedHeap

    /// Blit `contents` into string storage starting at index `addr`. Does not touch the
    /// null terminator, and does not update any `StringContents` registration.
    val setStringData : addr : int -> contents : string -> heap : ManagedHeap -> ManagedHeap

    /// Record the full character content of the string object at `addr`, so that
    /// string-level operations (equality, hashing) can read it back.
    val recordStringContents : addr : ManagedHeapAddress -> contents : string -> heap : ManagedHeap -> ManagedHeap

    /// Record where the string object at `addr` has its UTF-16 data in string storage.
    val recordStringDataOffset : addr : ManagedHeapAddress -> dataOffset : int -> heap : ManagedHeap -> ManagedHeap

    /// The character content of the string object at `addr`, or None if none was recorded
    /// — a string allocated off the standard path, or an address that is not a string.
    val getStringContents : addr : ManagedHeapAddress -> heap : ManagedHeap -> string option

    /// The index in string storage of the first character of the string at `addr`, or None
    /// if none was recorded.
    val tryGetStringDataOffset : addr : ManagedHeapAddress -> heap : ManagedHeap -> int option

    /// The index in string storage of the first character of the string at `addr`. Fails if
    /// none was recorded.
    val getStringDataOffset : addr : ManagedHeapAddress -> heap : ManagedHeap -> int

    /// Update one character of the string at `addr`. `charIndex` equal to the string length
    /// addresses the null terminator, which updates the character storage but not the
    /// logical string value; beyond that is rejected.
    val setStringChar :
        addr : ManagedHeapAddress -> charIndex : int -> value : char -> heap : ManagedHeap -> ManagedHeap

    /// Read one character of the string at `addr`. `charIndex` equal to the string length
    /// addresses the null terminator; beyond that is rejected, since it would walk into
    /// unrelated string storage.
    val getStringChar : addr : ManagedHeapAddress -> charIndex : int -> heap : ManagedHeap -> char

    /// Value equality between two managed strings, with the semantics of
    /// `System.String.Equals(string, string)`. Fails if the contents are genuinely needed
    /// and either address is not a registered string.
    val stringsEqual : a1 : ManagedHeapAddress -> a2 : ManagedHeapAddress -> heap : ManagedHeap -> bool

    /// Whether `addr` is a live array. False both for a live non-array object and for an
    /// address that was never allocated; `tryGetObjectConcreteType` tells those apart.
    val isArray : addr : ManagedHeapAddress -> heap : ManagedHeap -> bool

    /// Whether `addr` is a live heap allocation of either kind.
    val isLive : addr : ManagedHeapAddress -> heap : ManagedHeap -> bool

    /// The dimensions and element type of the array at `addr`, or None if `addr` is not a
    /// live array. Carries no cells, so this is not a read of guest memory.
    val tryGetArrayShape : addr : ManagedHeapAddress -> heap : ManagedHeap -> ArrayShape option

    /// The dimensions and element type of the array at `addr`. Carries no cells, so this is
    /// not a read of guest memory. Reports "not an array" and "not allocated" distinctly.
    val getArrayShape : addr : ManagedHeapAddress -> heap : ManagedHeap -> ArrayShape

    /// The byte distance between consecutive cells of the array at `addr`. Well defined for
    /// an empty array, and never a read of a cell.
    val getArrayElementStride : addr : ManagedHeapAddress -> heap : ManagedHeap -> int

    /// The zero value of the element type of the array at `addr`: the witness for questions
    /// about what shape a cell of this array has. Well defined for an empty array, and
    /// never a read of a cell — which is the point. Use `getArrayValue` to learn what a
    /// cell actually holds.
    val getArrayElementZero : addr : ManagedHeapAddress -> heap : ManagedHeap -> CliType

    /// The value in cell `offset` of the array at `alloc`, bounds-checked against the
    /// array's length. A read of guest memory.
    val getArrayValue : alloc : ManagedHeapAddress -> offset : int -> heap : ManagedHeap -> CliType

    /// Store `v` into cell `offset` of the array at `alloc`, bounds-checked against the
    /// array's length. A write to guest memory.
    val setArrayValue : alloc : ManagedHeapAddress -> offset : int -> v : CliType -> heap : ManagedHeap -> ManagedHeap

    /// The non-array object at `alloc`, or None if there is no live non-array object there
    /// — including when `alloc` is a live *array*, which has no such payload.
    val tryGet : alloc : ManagedHeapAddress -> heap : ManagedHeap -> AllocatedNonArrayObject option

    /// The non-array object at `alloc`. Reports "is an array" and "not allocated"
    /// distinctly.
    val get : alloc : ManagedHeapAddress -> heap : ManagedHeap -> AllocatedNonArrayObject

    /// Replace the payload of the non-array object at `alloc`. Fails if `alloc` is not a
    /// live non-array object.
    val set : alloc : ManagedHeapAddress -> v : AllocatedNonArrayObject -> heap : ManagedHeap -> ManagedHeap

    /// Store `value` into field `field` of the non-array object at `alloc`. Reports "is an
    /// array" and "not allocated" distinctly.
    val setFieldById :
        alloc : ManagedHeapAddress -> field : FieldId -> value : CliType -> heap : ManagedHeap -> ManagedHeap

/// Read-only introspection of the heap by code that is *not the running guest*: the
/// debugger server, crash reporting, and tests.
///
/// The split from `ManagedHeap` is by *caller identity*, not by what is read. A debugger
/// dumping an array reads exactly the cells `ManagedHeap.getArrayValue` reads; the
/// difference is that the guest did not ask for them, and so a race detector must not treat
/// them as an access by any thread. Keeping the two in separate modules makes that boundary
/// visible at every call site: when heap accesses come to emit events, `ManagedHeap`'s
/// functions emit and `HeapObserver`'s deliberately do not.
///
/// Consequently these must stay pure reads. Anything the interpreter proper needs belongs
/// in `ManagedHeap`, even if a test is its only current caller.
[<RequireQualifiedAccess>]
module HeapObserver =
    /// The number of live non-array objects, arrays excluded.
    val nonArrayObjectCount : heap : ManagedHeap -> int

    /// The number of live arrays.
    val arrayCount : heap : ManagedHeap -> int

    /// The number of objects with recorded string content. Not the number of live
    /// `System.String` instances as such.
    val stringContentCount : heap : ManagedHeap -> int

    /// The address the next allocation will be given, and hence an address guaranteed not
    /// to be live.
    val nextAddress : heap : ManagedHeap -> int

    /// The whole array at `addr`, cells included, or None if `addr` is not a live array.
    /// The only accessor that hands out every cell at once.
    val tryGetArray : addr : ManagedHeapAddress -> heap : ManagedHeap -> AllocatedArray option

    /// Every live non-array object with its payload, in ascending address order.
    val nonArrayObjects : heap : ManagedHeap -> (ManagedHeapAddress * AllocatedNonArrayObject) list

    /// The addresses of all live allocations, arrays and non-arrays alike. Computed from
    /// the payload tables, independently of `syncBlockAddresses`.
    val liveAddresses : heap : ManagedHeap -> Set<ManagedHeapAddress>

    /// The addresses that have an object header. Computed from the header table,
    /// independently of `liveAddresses`; the two agreeing is an invariant worth testing
    /// rather than assuming.
    val syncBlockAddresses : heap : ManagedHeap -> Set<ManagedHeapAddress>

    /// Whether `addr` has an object header. Computed from the header table alone; see
    /// `syncBlockAddresses`.
    val hasSyncBlock : addr : ManagedHeapAddress -> heap : ManagedHeap -> bool

    /// Every object header, keyed by address.
    val syncBlocks : heap : ManagedHeap -> Map<ManagedHeapAddress, SyncBlock>
