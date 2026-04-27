namespace WoofWare.PawPrint

type ThreadId =
    | ThreadId of int

    override this.ToString () =
        match this with
        | ThreadId.ThreadId i -> $"%i{i}"

/// Currently this is just an opaque handle; it can't be treated as a pointer.
type ManagedHeapAddress =
    | ManagedHeapAddress of int

    override this.ToString () : string =
        match this with
        | ManagedHeapAddress.ManagedHeapAddress i -> $"<object #%i{i}>"

/// Opaque address for PawPrint's emulated GC handle table.
type GcHandleAddress =
    | GcHandleAddress of int

    override this.ToString () : string =
        match this with
        | GcHandleAddress.GcHandleAddress i -> $"<GC handle #%i{i}>"

type FrameId =
    | FrameId of int

    override this.ToString () =
        match this with
        | FrameId.FrameId i -> $"<frame #%i{i}>"

/// Opaque handle for a localloc block owned by a single method frame.
type LocallocBlockId =
    | LocallocBlockId of int

    override this.ToString () =
        match this with
        | LocallocBlockId.LocallocBlockId i -> $"<localloc block #%i{i}>"
