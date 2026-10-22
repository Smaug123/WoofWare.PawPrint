namespace WoofWare.PawPrint

/// The alignment a native-heap block's base must have: a power of two, in bytes, and never
/// less than `NativeBlockAlignment.minimum`.
[<Sealed>]
type NativeBlockAlignment =
    /// The alignment in bytes.
    member Bytes : int

[<RequireQualifiedAccess>]
module NativeBlockAlignment =
    /// Pointer size, 8 bytes: the least alignment any native allocation has, since `malloc`,
    /// `aligned_alloc` as CoreLib calls it, and a pointer-sized slot are all at least
    /// pointer-aligned. `ManagedPointerSource.tryContainerBase` claims this of every
    /// native-heap block when it answers an alignment mask.
    val minimum : NativeBlockAlignment

    /// What `malloc` guarantees on the 64-bit targets PawPrint models: 16 bytes, from glibc and
    /// from Darwin's libmalloc alike. `calloc`, and every allocation CoreLib makes through
    /// `NativeMemory.Alloc` or `Marshal.AllocHGlobal`, gets the same.
    val malloc : NativeBlockAlignment

    /// `bytes` as an alignment. Fails unless it is a power of two no less than `minimum`.
    val ofBytes : bytes : int -> NativeBlockAlignment

/// Where each live native-heap block lies in the process's address space: a base address per
/// block, which is what a pointer into the block is an offset from.
///
/// A block's base is fixed for as long as the block is live, is aligned as its allocation
/// asked, and lies in `[NativeAddressSpace.lowest, NativeAddressSpace.limit)`, and no two live
/// blocks overlap. A block of no bytes still occupies one, so that its base is unique too.
/// Nothing else is promised about where a block goes: which address satisfying those is
/// chosen is this module's business alone, and may change.
[<Sealed>]
type NativeAddressSpace

[<RequireQualifiedAccess>]
module NativeAddressSpace =
    /// The lowest address a block may be placed at, 2^32: no base fits in 32 bits, so an
    /// address truncated to 32 bits anywhere names no block.
    val lowest : uint64

    /// One past the highest address a block may occupy, 2^46: below the user address limit of
    /// every machine the kernel has observed, so the kernel takes any block's address for
    /// user memory.
    val limit : uint64

    /// An address space holding no blocks.
    val empty : NativeAddressSpace

    /// Choose a base for `blockId`, a block of `byteCount` bytes, and record it. Fails if
    /// `blockId` already has one, if `byteCount` is negative, or if no address satisfying the
    /// type's promises is left.
    val place :
        blockId : NativeMemoryBlockId ->
        byteCount : int ->
        alignment : NativeBlockAlignment ->
        space : NativeAddressSpace ->
            uint64 * NativeAddressSpace

    /// Forget `blockId`'s base. Fails if it has none.
    val release : blockId : NativeMemoryBlockId -> space : NativeAddressSpace -> NativeAddressSpace

    /// `blockId`'s base, or `None` if it has none (it was never placed, or has been released).
    val tryBase : blockId : NativeMemoryBlockId -> space : NativeAddressSpace -> uint64 option

    /// Fail unless every live block is aligned as it asked, lies within the space's bounds,
    /// and overlaps no other. `place` checks the first two of each block as it places it;
    /// this checks all three of every block at once, for tests.
    val internal checkInvariants : space : NativeAddressSpace -> unit
