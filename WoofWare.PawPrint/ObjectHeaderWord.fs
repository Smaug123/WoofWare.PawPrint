namespace WoofWare.PawPrint

/// How a byte access below an object's raw data relates to its header word
/// (`ManagedHeap.getHeaderWord`).
[<RequireQualifiedAccess>]
type HeaderWordAccess =
    /// The access is exactly the word.
    | Word
    /// The access does not touch the word.
    | Clear
    /// The access covers part of the word, or the word and bytes beside it.
    | Straddles

/// Where an object's header word sits in the byte containers PawPrint keeps object storage in.
///
/// CoreCLR lays a 64-bit object out as 4 bytes of padding, the 4-byte header word, the
/// method-table pointer, and then the object's raw data, so .NET 11's thin lock reaches the
/// word as `(int*)&obj.GetMethodTableRef() - 1`, 12 bytes before `RuntimeHelpers.GetRawData`.
/// An SZ array's raw data begins with its length and padding, one `nint` before element 0.
/// PawPrint targets 64-bit guests only.
[<RequireQualifiedAccess>]
module internal ObjectHeaderWord =
    /// The word's width: `ObjHeader::m_SyncBlockValue` is a `DWORD`.
    let size : int = 4

    /// The word's offset in a non-array object's `ByteStorageIdentity.HeapObject` container,
    /// whose offset 0 is the first byte of raw data: one `nint` method-table pointer, then the
    /// word.
    let nonArrayStart : int64 = -12L

    /// The word's offset in an SZ array's `ByteStorageIdentity.Array` container, whose offset 0
    /// is element 0: the length and its padding, the method-table pointer, then the word.
    let szArrayStart : int64 = -20L

    /// How the access of `width` bytes at `start` relates to a word at `wordStart`.
    let classify (wordStart : int64) (start : int64) (width : int) : HeaderWordAccess =
        let finish = start + int64<int> width
        let wordFinish = wordStart + int64<int> size

        if start = wordStart && width = size then
            HeaderWordAccess.Word
        elif finish <= wordStart || start >= wordFinish then
            HeaderWordAccess.Clear
        else
            HeaderWordAccess.Straddles

    /// The refusal for an access `classify` calls `Straddles`.
    let refuseStraddle (addr : ManagedHeapAddress) (start : int64) (width : int) : 'a =
        failwith
            $"TODO: a %d{width}-byte access at offset %d{start} overlaps the object header word of %O{addr} without being exactly it; CoreLib reads and compare-exchanges that word only as a whole int32, so an access to part of it, or across it into the padding or the method-table pointer, is not modelled"

    /// The bit CoreCLR sets in a header word that holds a hash code or a sync block index
    /// (`BIT_SBLK_IS_HASH_OR_SYNCBLKINDEX`). While it is clear, the word holds a thin lock.
    let isHashOrSyncBlockIndexBit : int32 = 0x08000000

    /// With `isHashOrSyncBlockIndexBit`, marks the rest of the word as a hash code rather than a
    /// sync block index (`BIT_SBLK_IS_HASHCODE`).
    let isHashCodeBit : int32 = 0x04000000

    /// The bit the VM holds while it changes the index in the word (`BIT_SBLK_SPIN_LOCK`).
    let spinLockBit : int32 = 0x10000000

    /// The bits of a sync block index (`MASK_SYNCBLOCKINDEX`).
    let syncBlockIndexMask : int32 = (1 <<< 26) - 1

    /// The thin lock's owning thread's managed id (`SBLK_MASK_LOCK_THREADID`).
    let thinLockThreadIdMask : int32 = 0x0000FFFF

    /// The thin lock's recursion level, shifted by `thinLockRecursionShift` (`SBLK_MASK_LOCK_RECLEVEL`).
    let thinLockRecursionMask : int32 = 0x003F0000

    let thinLockRecursionShift : int = 16

/// The thread holding an object's thin lock, as its header word records it.
type ThinLockOwner =
    {
        /// The owner's `Environment.CurrentManagedThreadId`; never 0, which means unowned.
        ManagedThreadId : int
        /// How many times the owner has re-entered beyond the first.
        RecursionLevel : int
    }

/// What CoreCLR's `ObjHeader::GetSyncBlock` does with an object's header word when it gives the
/// object a sync block, which is how .NET 11's monitor inflates a thin lock to a `Lock`.
[<RequireQualifiedAccess>]
type HeaderWordInflation =
    /// The word held a thin lock, with `owner` holding it if any; `word` is the word once the
    /// sync block's index is installed and the thin lock's bits are cleared, which the sync block
    /// now carries.
    | FromThinLock of owner : ThinLockOwner option * word : int32
    /// The word holds bits PawPrint never writes there, which `ObjectHeaderInflation.inflate` does
    /// not model: a hash code, a sync block index, or the VM's spin lock.
    | Unmodelled of word : int32

[<RequireQualifiedAccess>]
module internal ObjectHeaderInflation =

    /// `word` as `ObjHeader::GetSyncBlock` leaves it on installing the sync block numbered
    /// `index`, which must be a positive sync block index.
    let inflate (index : int) (word : int32) : HeaderWordInflation =
        if index <= 0 || index > ObjectHeaderWord.syncBlockIndexMask then
            failwith $"ObjectHeaderInflation.inflate: %d{index} is not a sync block index"

        if
            word
            &&& (ObjectHeaderWord.isHashOrSyncBlockIndexBit ||| ObjectHeaderWord.spinLockBit)
            <> 0
        then
            HeaderWordInflation.Unmodelled word
        else
            let threadId = word &&& ObjectHeaderWord.thinLockThreadIdMask

            let recursion =
                (word &&& ObjectHeaderWord.thinLockRecursionMask)
                >>> ObjectHeaderWord.thinLockRecursionShift

            let owner =
                if threadId = 0 then
                    if recursion <> 0 then
                        failwith
                            $"ObjectHeaderInflation.inflate: header word 0x%08x{word} records a recursion level with no owner, which the thin lock never writes"

                    None
                else
                    Some
                        {
                            ManagedThreadId = threadId
                            RecursionLevel = recursion
                        }

            // `ObjHeader::SetIndex` keeps every bit but the index, the hash-code bit and the
            // index-or-hash bit, and the index carries the latter.
            let kept =
                word
                &&& ~~~(ObjectHeaderWord.isHashOrSyncBlockIndexBit
                        ||| ObjectHeaderWord.isHashCodeBit
                        ||| ObjectHeaderWord.syncBlockIndexMask)

            HeaderWordInflation.FromThinLock (owner, kept ||| ObjectHeaderWord.isHashOrSyncBlockIndexBit ||| index)
