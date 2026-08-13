namespace WoofWare.PawPrint

open System.Collections.Immutable

/// State carried when an object's monitor is `Held` by some thread.
/// `AcquireQueue` is the FIFO list of (thread, optional re-entry depth) pairs
/// parked in `BlockedOnSyncBlockAcquire` waiting for ownership to be transferred
/// to them when `LockingThread` calls `Monitor.Exit`. FIFO order is load-bearing
/// for fairness: switching to LIFO or arbitrary order would change the
/// observable interleaving for guests that race multiple threads into the same
/// `lock` block.
///
/// The `int option` snapshot on each `AcquireQueue` entry distinguishes the
/// two flavours of waiter:
///   * `None` — a fresh entrant from `Monitor.Enter`. On ownership transfer it
///     becomes the new owner with `ReentrancyCount = 1`.
///   * `Some depth` — a waiter that was woken from `Monitor.Wait` by
///     `Monitor.Pulse` / `PulseAll` (or a spurious wake). `Wait` snapshots its
///     prior `ReentrancyCount` so that on re-acquire the depth it had before
///     parking is restored verbatim. Storing the depth inline next to the
///     thread keeps the "what's waiting and what depth do they need" coupling
///     visible at every read site; a separate map would let a transition lose
///     the pairing silently.
///
/// `ReentrancyCount` is the depth of nested `Monitor.Enter` calls by
/// `LockingThread` and must reach exactly zero before ownership can transfer.
type LockedSyncBlock =
    {
        LockingThread : ThreadId
        ReentrancyCount : int
        AcquireQueue : (ThreadId * int option) list
    }

/// Ownership state of an object's monitor — distinct from its `WaitQueue`
/// because `Monitor.Wait` fully releases the lock (`Free`) while leaving its
/// caller parked in the SyncBlock's `WaitQueue`.
type SyncBlockLock =
    | Free
    | Held of LockedSyncBlock

/// Per-object monitor metadata. `Lock` describes ownership and FIFO of
/// `Monitor.Enter` contenders. `WaitQueue` is the FIFO list of (thread,
/// snapshot depth) pairs currently parked in `BlockedOnSyncBlockWait` from a
/// `Monitor.Wait` call; they do NOT contend for the lock until a `Pulse` /
/// `PulseAll` moves them onto `AcquireQueue` (FIFO tail), at which point they
/// re-enter via the normal ownership-transfer path. The two fields are
/// orthogonal: a non-empty `WaitQueue` can coexist with `Lock = Free` (the
/// owner called `Wait`, releasing the lock, but the waiter is still parked).
/// Pulse on an empty wait queue is a documented no-op (matches CoreCLR's
/// `SyncBlock`).
type SyncBlock =
    {
        Lock : SyncBlockLock
        WaitQueue : (ThreadId * int) list
    }

    /// Initial state for a freshly-allocated object: lock free, no waiters.
    static member Empty : SyncBlock =
        {
            Lock = SyncBlockLock.Free
            WaitQueue = []
        }

type AllocatedNonArrayObject =
    {
        // TODO: this is a slightly odd domain; the same type for value types as class types!
        Contents : CliValueType
        ConcreteType : ConcreteTypeHandle
    }

    static member DereferenceField (name : string) (f : AllocatedNonArrayObject) : CliType =
        CliValueType.DereferenceField name f.Contents

    static member DereferenceFieldById (field : FieldId) (f : AllocatedNonArrayObject) : CliType =
        CliValueType.DereferenceFieldById field f.Contents

    static member SetField (name : string) (v : CliType) (f : AllocatedNonArrayObject) : AllocatedNonArrayObject =
        { f with
            Contents = CliValueType.WithFieldSet name v f.Contents
        }

    static member SetFieldById (field : FieldId) (v : CliType) (f : AllocatedNonArrayObject) : AllocatedNonArrayObject =
        { f with
            Contents = CliValueType.WithFieldSetById field v f.Contents
        }

/// Everything about an array except its contents: the element type, the total element
/// count, the per-dimension lengths, and the two element-type facts the access paths need —
/// the byte stride between cells and the element's zero value. All of them are fixed when
/// the array is allocated and never change afterwards, so reading them is not a
/// guest-visible memory access — unlike reading a cell, which is.
///
/// The absence of an `Elements` field is the point. A caller that needs only the rank or
/// the length holds a value from which no cell can be reached, so a shape query cannot
/// silently become a data read as the code around it changes. Cell reads go through
/// `ManagedHeap.getArrayValue`.
type ArrayShape =
    {
        ConcreteType : ConcreteTypeHandle
        /// Total element count, equal to the product of `Lengths`.
        Length : int
        /// Per-dimension lengths in row-major order; length 1 for szarrays, else the rank.
        Lengths : ImmutableArray<int>
        /// The byte distance between consecutive cells.
        ///
        /// A property of the *element type*, not of any stored value: CoreCLR fixes it when
        /// the array type is laid out, and no store into a cell can change it. It is recorded
        /// here at allocation, from the element zero the allocator was handed, rather than
        /// recovered later by measuring a cell — measuring a cell would be a read of guest
        /// memory to answer a question about a type, showing up as an access with no
        /// counterpart in the program under test, and it has no answer at all for an empty
        /// array.
        ///
        /// Always strictly positive: every CLI type occupies at least one byte, a fieldless
        /// struct included (CoreCLR pads it to 1, and `CliValueType.SizeOfFieldStorage`
        /// follows). Consumers divide by it — see `floorDivRem` — so a zero here would be a
        /// silent wrong answer rather than a loud one.
        ///
        /// `ManagedHeap.allocateArray` checks positivity, checks the value against
        /// `ElementZero`, and checks it against cell 0 of every non-empty allocation, so it
        /// can never drift from either.
        ElementStride : int
        /// The zero value of the element type: what every cell held immediately after
        /// allocation, and the canonical witness for "what shape is a cell of this array".
        ///
        /// Like `ElementStride`, a property of the element type rather than of any stored
        /// value, recorded from the zero factory the allocator was handed. Callers asking a
        /// *type* question — is this store whole-cell-shaped, what template should this
        /// decoded value take — read this instead of sampling cell 0. Sampling cell 0 is
        /// wrong in three separate ways: it is a guest-visible read performed to answer a
        /// question about a type, it has no answer for an empty array, and cell 0 is only a
        /// sample, so a store to cell 5 ends up validated against whatever provenance cell 0
        /// happens to be carrying.
        ///
        /// Not a substitute for reading a cell. A cell legitimately drifts from this shape —
        /// an `IntPtr[]` slot holding a `TypeHandlePtr` after a typed store through a fixed
        /// pointer — so anything that cares what is *actually stored* must still go through
        /// `getArrayValue`.
        ///
        /// `ElementStride` is exactly `CliType.sizeOf ElementZero`, checked at
        /// `allocateArray`. It is stored rather than recomputed because `CliType.sizeOf`
        /// walks a value type's whole field tree, and the stride is read on every byte-view
        /// array access.
        ElementZero : CliType
    }

type AllocatedArray =
    {
        /// Identity and dimensions, fixed at allocation. Held as a nested record rather
        /// than inlined so that a caller wanting only the shape can be handed a value
        /// from which no cell is reachable; see `ArrayShape`.
        Shape : ArrayShape
        /// Backing store in row-major order. For multi-dim arrays the element at
        /// `(i_0, ..., i_{n-1})` lives at flat offset
        /// `((((i_0)*d_1)+i_1)*d_2 + i_2)*...*d_{n-1} + i_{n-1}`, where `d_k = Lengths.[k]`.
        Elements : ImmutableArray<CliType>
    }
