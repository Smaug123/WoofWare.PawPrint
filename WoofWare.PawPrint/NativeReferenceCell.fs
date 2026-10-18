namespace WoofWare.PawPrint

/// A fresh native block of one object-reference cell: a protected local of the VM's, whose
/// address it passes to a CoreLib `[UnmanagedCallersOnly]` entry point as a `T*`, such as the
/// `Exception*` out-slot in which such an entry point stores any exception it catches, for the
/// VM to rethrow once the call has returned. Made by `NativeReferenceCell.allocate`, and read by
/// `NativeReferenceCell.read`.
type NativeReferenceCell =
    private
        {
            Block : NativeMemoryBlockId
        }

    /// The pointer to pass as the callee's `T*` argument.
    member this.Pointer : ManagedPointerSource =
        ManagedPointerSource.Byref
            {
                Root = ByrefRoot.NativeMemoryByte (this.Block, 0)
                Projections = []
            }

[<RequireQualifiedAccess>]
module NativeReferenceCell =

    /// Bytes in the cell: an object reference is pointer-sized.
    [<Literal>]
    let private PointerSize = 8

    /// A fresh cell holding `contents`, as the VM's protected local does before the call: null for
    /// an out-slot.
    ///
    /// The cell is written rather than left to the zeroed block, because a typed read of a
    /// native block finds only the cells written into it; a callee's `stind.ref` then replaces a
    /// cell of the type it stores.
    let allocate
        (contents : ManagedHeapAddress option)
        (pool : NativeMemoryPool)
        : NativeReferenceCell * NativeMemoryPool
        =
        let block, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized PointerSize pool

        let pool = NativeMemoryPool.writeCell block 0 (CliType.ObjectRef contents) pool

        {
            Block = block
        },
        pool

    /// What `cell` holds: `None` if it holds null. Fails if it holds anything but an object
    /// reference, which no CoreLib entry point stores there.
    let read (cell : NativeReferenceCell) (pool : NativeMemoryPool) : ManagedHeapAddress option =
        match NativeMemoryPool.tryReadCell cell.Block 0 pool with
        | Some (CliType.ObjectRef stored) -> stored
        | Some other ->
            failwith
                $"The native reference cell %O{cell.Block} holds %O{other}, which is not an object reference; the callee wrote something other than an object reference into it."
        | None ->
            failwith
                $"logic error: the native reference cell %O{cell.Block} has no cell at byte 0, though NativeReferenceCell.allocate writes one"

    /// Release `cell`, as the VM's local dies when the native function holding it returns. Any
    /// later read of it fails.
    let free (cell : NativeReferenceCell) (pool : NativeMemoryPool) : NativeMemoryPool =
        NativeMemoryPool.free cell.Block pool
