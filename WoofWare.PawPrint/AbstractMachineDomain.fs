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
type StackMemoryBlockId =
    | StackMemoryBlockId of int

    override this.ToString () =
        match this with
        | StackMemoryBlockId.StackMemoryBlockId i -> $"<stack memory block #%i{i}>"

/// Opaque handle for a native-heap block allocated by `Marshal.AllocHGlobal` /
/// `NativeMemory.Alloc` and freed by `Marshal.FreeHGlobal` / `NativeMemory.Free`.
/// Globally scoped within the IlMachineState (not frame-local). Freeing deletes
/// the block from the pool so subsequent reads or writes through any retained
/// byref fail loudly — the deterministic simulator catches use-after-free at the
/// site that exercises the dangling pointer.
type NativeMemoryBlockId =
    | NativeMemoryBlockId of int

    override this.ToString () =
        match this with
        | NativeMemoryBlockId.NativeMemoryBlockId i -> $"<native memory block #%i{i}>"

/// Opaque handle for a `System.Threading.LowLevelMonitor` allocated by the
/// `SystemNative_LowLevelMonitor_Create` QCall. Round-trips through guest code
/// as an `IntPtr` (see `NativeIntSource.LowLevelMonitorPtr`). Globally scoped
/// within the `IlMachineState`; never reused after Destroy so that
/// use-after-free is caught at the use site.
type LowLevelMonitorId =
    | LowLevelMonitorId of int

    override this.ToString () =
        match this with
        | LowLevelMonitorId.LowLevelMonitorId i -> $"<low-level monitor #%i{i}>"
