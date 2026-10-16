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
