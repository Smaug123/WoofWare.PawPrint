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

/// One task's in-flight `flock` acquisition: which open file description it is
/// waiting to lock, and how.
///
/// Exactly the payload of the `WakeCondition` that parked it, and derived from
/// that condition rather than built beside it (`UnixSystem.parkFlock`), so that
/// a record disagreeing with the condition a client is polling is unwritable
/// rather than merely unwritten.
type ParkedFlock =
    {
        /// The open file description whose lock is being waited for.
        Requester : OpenFileDescriptionId
        /// The lock that was asked for. A conversion has already dropped
        /// whatever this description held, so this is what it will hold if the
        /// acquisition ever completes, not what it holds now.
        Mode : FlockMode
    }

/// What the emulated kernel knows about one task — one scheduling entity, what
/// `gettid(2)` names.
///
/// Every live thread has exactly one of these, minted at thread creation;
/// `IlMachineState.checkInvariants` refuses a state where the two sets differ.
/// That is what makes the record total: `Cpu` and `OsThreadId` were fields on
/// `ThreadState` precisely because a `Map` has no truthful default for an absent
/// key, and the answer is that there is never an absent key rather than that a
/// default exists.
///
/// The per-thread errno is *not* here. On a real Unix errno lives in libc, not
/// in the kernel: the kernel returns an error code and the syscall wrapper
/// stores it. PawPrint's `LastSystemError` is that wrapper's slot — CoreCLR
/// reuses it for Windows last-error too, and `NativeWaitHandle` really does put
/// Win32 numbers in it — so it stays on `EmulatedKernel` with
/// `LastPInvokeError`.
type UnixTaskState =
    {
        /// The simulated logical processor this task is pinned to: what
        /// `sched_getcpu(3)` reports while it runs.
        ///
        /// Assigned once, at thread creation, by `cpuForRotation`. PawPrint's
        /// scheduler runs one task at a time and never migrates one between
        /// cores, so "pinned to" and "currently executing on" coincide. This is
        /// the seat a future core-aware scheduler would rewrite.
        Cpu : CpuId
        /// The OS thread identifier this task reports to the guest, and which
        /// `System.Threading.Lock` uses as its owner identity.
        ///
        /// Assigned once, at thread creation, and never reused: real kernels
        /// recycle thread ids, but a recycled one here would let a stale
        /// `Lock._owningThreadId` be mistaken for a live owner.
        OsThreadId : OsThreadId
        /// The socket wait this task is blocked in, if it is blocked in one.
        ///
        /// A real kernel holds a blocked task's in-flight syscall arguments on
        /// its stack; this is that, for the one syscall PawPrint parks in. The
        /// re-entry consults it rather than the guest's argument cells, which
        /// the guest may have written since.
        ParkedSocketWait : ParkedSocketWait option
        /// The `flock` acquisition this task is blocked in, if it is blocked in
        /// one.
        ///
        /// Held by *description identity*, exactly as `ParkedSocketWait` holds
        /// its port and for the same reason: a sleeping task keeps the kernel
        /// object rather than the descriptor number, and descriptor numbers are
        /// reused as soon as they are free. The re-entry finishes the
        /// acquisition against this rather than against the number the call was
        /// made through.
        ///
        /// It is also what lets `close` refuse to destroy a description
        /// something is waiting on: a rule about kernel objects, which this
        /// library can only apply to a park it can see.
        ParkedFlock : ParkedFlock option
    }

/// The tasks a simulated process owns, by whatever a client uses to name one.
///
/// Generic in the task name for the same reason `SignalState` is: the identity
/// of a scheduling entity is the client's, not this library's. WoofWare.PawPrint
/// names them by its interpreter-private `ThreadId`; anything else would do.
[<RequireQualifiedAccess>]
module UnixTaskTable =

    /// The task `name` is.
    ///
    /// Total, and loudly partial rather than an option: every live task is
    /// registered when it is created, so a name that resolves to nothing is a
    /// client bug rather than anything a guest did.
    let get<'Task when 'Task : comparison> (name : 'Task) (tasks : Map<'Task, UnixTaskState>) : UnixTaskState =
        match Map.tryFind name tasks with
        | Some task -> task
        | None ->
            failwith
                $"UnixTaskTable.get: %O{name} names no task. Every task is registered with `UnixTaskTable.register` when it is created, so this one was built without that (this is a bug in the client)."

    /// Mint the task for a newly-created scheduling entity.
    ///
    /// The one route by which a task enters the table, so that "exactly the live
    /// tasks" is maintained at the single place a task comes into being.
    let register<'Task when 'Task : comparison>
        (name : 'Task)
        (cpu : CpuId)
        (osThreadId : OsThreadId)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        if Map.containsKey name tasks then
            failwith
                $"UnixTaskTable.register: %O{name} already names a task. A task is created once, and re-registering would silently discard whatever the first registration recorded (this is a bug in the client)."

        Map.add
            name
            {
                Cpu = cpu
                OsThreadId = osThreadId
                ParkedSocketWait = None
                ParkedFlock = None
            }
            tasks

    /// The logical processor `name` runs on, as `sched_getcpu` reports it.
    let cpuOf<'Task when 'Task : comparison> (name : 'Task) (tasks : Map<'Task, UnixTaskState>) : CpuId =
        (get name tasks).Cpu

    /// The OS thread id `name` reports to the guest.
    let osThreadIdOf<'Task when 'Task : comparison> (name : 'Task) (tasks : Map<'Task, UnixTaskState>) : OsThreadId =
        (get name tasks).OsThreadId

    /// The socket wait `name` is blocked in, if any.
    let parkedSocketWaitFor<'Task when 'Task : comparison>
        (name : 'Task)
        (tasks : Map<'Task, UnixTaskState>)
        : ParkedSocketWait option
        =
        (get name tasks).ParkedSocketWait

    /// Record that `name` has parked in a socket wait, or (with `None`) that it
    /// is no longer in one.
    let withParkedSocketWait<'Task when 'Task : comparison>
        (name : 'Task)
        (wait : ParkedSocketWait option)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        let existing = get name tasks

        Map.add
            name
            { existing with
                ParkedSocketWait = wait
            }
            tasks

    /// The `flock` acquisition `name` is blocked in, if any.
    let parkedFlockFor<'Task when 'Task : comparison>
        (name : 'Task)
        (tasks : Map<'Task, UnixTaskState>)
        : ParkedFlock option
        =
        (get name tasks).ParkedFlock

    /// Record that `name` has parked in an `flock`, or (with `None`) that it is
    /// no longer in one.
    ///
    /// Clients park through `UnixSystem.parkFlock` rather than through this,
    /// which is what ties the record to the condition that produced it; this is
    /// how the record is cleared, and how `parkFlock` writes it.
    let withParkedFlock<'Task when 'Task : comparison>
        (name : 'Task)
        (parked : ParkedFlock option)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        let existing = get name tasks

        Map.add
            name
            { existing with
                ParkedFlock = parked
            }
            tasks

    /// Compare the table against the tasks a client believes are live,
    /// answering those it has no entry for and those it has an entry for but
    /// the client does not.
    ///
    /// Answers both sets rather than raising, so the client reports them in its
    /// own vocabulary: this library has no opinion on what a defect is called.
    let reconcile<'Task when 'Task : comparison>
        (live : Set<'Task>)
        (tasks : Map<'Task, UnixTaskState>)
        : 'Task list * 'Task list
        =
        let named = tasks |> Map.toList |> List.map fst |> Set.ofList
        Set.difference live named |> Set.toList, Set.difference named live |> Set.toList
