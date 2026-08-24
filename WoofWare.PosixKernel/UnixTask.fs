namespace WoofWare.PosixKernel

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
/// Nothing cares about specific numbers, only uniqueness.
/// (`System.Threading.Lock` uses this value as its owner identity, for both
/// mutual exclusion and recursive re-entry detection.) There is exactly one
/// producer, `EmulatedKernel.osThreadId`, which derives the id from the
/// thread's `ThreadId`; uniqueness is therefore inherited from `ThreadId`
/// rather than being an invariant this type has to defend.
///
/// Still a distinct type from `ThreadId`, though currently in bijection with
/// it, because the two answer different questions and are confusable at
/// exactly the boundary where confusing them is expensive. A `ThreadId` is
/// interpreter-private: it keys `IlMachineState.ThreadState` and the scheduler
/// picks by it. An `OsThreadId` is a value the *guest* reads and stores —
/// notably in `Lock._owningThreadId`, where a wrong-but-plausible number is
/// silently mistaken for an owner rather than crashing.
type OsThreadId =
    | OsThreadId of uint32

    override this.ToString () =
        match this with
        | OsThreadId.OsThreadId i -> $"<os thread #%i{i}>"

/// One thread's in-flight `SystemNative_WaitForSocketEvents` call: the state
/// the syscall captured when it was entered, which outlives anything the
/// guest does to its arguments afterwards. The port is held by *description
/// identity*, exactly as the real syscall holds a file reference — closing
/// the fd the wait was called through changes nothing, because the fd is
/// never consulted again.
type ParkedSocketWait =
    {
        /// The open file description of the port being waited on.
        Port : OpenFileDescriptionId
        /// The `*count` read at entry. A real `epoll_wait` keeps using the
        /// maxevents it was passed even if the guest overwrites the cell
        /// mid-wait.
        MaxEvents : int
    }
