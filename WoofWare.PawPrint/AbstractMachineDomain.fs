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

/// The operating-system thread identifier the simulated kernel reports for a
/// thread: what `gettid(2)` returns on Linux, and what
/// `pthread_threadid_np(3)` returns on macOS. Reaches the guest through the
/// `SystemNative_TryGetUInt32OSThreadId` / `SystemNative_GetUInt64OSThreadId`
/// PAL entry points, which are two width-projections of this one value rather
/// than two independent quantities (upstream both read the same
/// `minipal_get_current_thread_id()`).
///
/// Held as a `uint32` because the two sentinel values that must be dodged are
/// both 32-bit facts. `TryGetUInt32OSThreadId` returns `(uint32)-1` to mean
/// "this platform does not know how to get a thread id", and CoreLib's
/// `Lock.ThreadId.InitializeForCurrentThread` (Lock.NonNativeAot.cs) maps a
/// zero id to `0xFFFF_FFFF` by decrement — so a `0` or `0xFFFF_FFFF` id would
/// silently become a *shared* id across every thread that produced one. The
/// 64-bit entry point reports the zero-extension.
///
/// The load-bearing property is **uniqueness**, not the specific numbers:
/// `System.Threading.Lock` uses this value as its owner identity, for both
/// mutual exclusion and recursive re-entry detection, so two live threads
/// sharing an id is a silent correctness failure rather than a crash. There
/// are exactly two producers, and they mint into disjoint halves of the range
/// so that uniqueness holds *between* them and not merely within each:
/// `EmulatedKernel.osThreadIdForGuest` mints odd ids for guest-visible
/// threads, and `EmulatedKernel.osThreadIdForInternal` mints even ids for
/// PawPrint-internal auxiliary threads (currently just the signal dispatcher,
/// which does run guest handler code and therefore does need a real, distinct
/// id — unlike its `CpuId`, which may safely alias a guest thread's, because a
/// processor index is a shared-resource key whereas a thread id is an
/// identity).
///
/// A distinct type from `ThreadId` and `CpuId` because all three are small
/// integers naming a scheduling entity, and swapping them would produce a
/// plausible-looking wrong answer rather than a crash. In particular this is
/// *not* `ThreadId`: `ThreadId` is PawPrint's interpreter-private allocation
/// counter, which internal threads also consume, and deriving a guest-visible
/// id from it directly would let an interpreter-internal allocation shift
/// every subsequent guest thread's id.
type OsThreadId =
    | OsThreadId of uint32

    override this.ToString () =
        match this with
        | OsThreadId.OsThreadId i -> $"<os thread #%i{i}>"

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
