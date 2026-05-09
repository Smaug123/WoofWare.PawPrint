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

/// Opaque handle minted by `EventPipeInternal_CreateProvider`. The int64 is monotonically
/// increasing within a run and starts at 1 so that the IntPtr handed back to the guest is
/// non-zero (real .NET callers throw OOM if `CreateProvider` returns zero).
[<Struct>]
type EventPipeProviderHandle =
    | EventPipeProviderHandle of int64

    override this.ToString () : string =
        match this with
        | EventPipeProviderHandle.EventPipeProviderHandle i -> $"<EventPipe provider #%i{i}>"

/// Opaque handle minted by `EventPipeInternal_DefineEvent`.
[<Struct>]
type EventPipeEventHandle =
    | EventPipeEventHandle of int64

    override this.ToString () : string =
        match this with
        | EventPipeEventHandle.EventPipeEventHandle i -> $"<EventPipe event #%i{i}>"

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
