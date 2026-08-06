namespace WoofWare.PawPrint

type ThreadId =
    | ThreadId of int

    override this.ToString () =
        match this with
        | ThreadId.ThreadId i -> $"%i{i}"

/// Index of one of the simulated process's logical processors, as reported to
/// the guest by `sched_getcpu(3)` (`SystemNative_SchedGetCpu`, and hence
/// `Thread.GetCurrentProcessorId()`).
///
/// Always in `[0, EmulatedKernel.effectiveProcessorCount)`: `sched_getcpu`
/// names a processor the guest also counts through
/// `Environment.ProcessorCount`, and BCL callers use the value to index
/// per-CPU shards sized off that count. The invariant is established by
/// construction rather than re-checked at each read. There are exactly two
/// producers: `EmulatedKernel.cpuForRotation`, which places every
/// guest-visible thread and is in range because it is a remainder modulo a
/// count that is at least 1; and `IlMachineState.allocateParkedThread`, which
/// hard-codes core 0 for PawPrint-internal auxiliary threads no guest can
/// name — trivially in range for the same reason.
///
/// A distinct type from `ThreadId` and friends because the two are trivially
/// confusable at the `sched_getcpu` boundary (both are small ints naming a
/// scheduling entity) and swapping them would produce a plausible-looking wrong
/// answer rather than a crash.
type CpuId =
    | CpuId of int

    override this.ToString () =
        match this with
        | CpuId.CpuId i -> $"<cpu #%i{i}>"

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

/// Opaque handle for a Win32-shaped wait handle (semaphore today; event/mutex
/// in future PRs) minted by the `CreateSemaphoreExW` / `CreateEventExW` /
/// `CreateMutexExW` QCalls. Round-trips through guest code as an `IntPtr` (see
/// `NativeIntSource.WaitHandlePtr`), wrapped by the BCL in a `SafeWaitHandle`.
/// Globally scoped within the `IlMachineState`; never reused after `CloseHandle`
/// so that use-after-free is caught at the use site.
type WaitHandleId =
    | WaitHandleId of int

    override this.ToString () =
        match this with
        | WaitHandleId.WaitHandleId i -> $"<wait handle #%i{i}>"
