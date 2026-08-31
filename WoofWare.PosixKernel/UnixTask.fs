namespace WoofWare.PosixKernel

/// <summary>
/// Index of one of the simulated process's logical processors.
/// </summary>
/// <example>
/// As reported to the guest by <c>sched_getcpu(3)</c>.
/// </example>
type CpuId =
    | CpuId of int

    /// <summary>
    /// A human-readable description of the CPU ID.
    /// </summary>
    override this.ToString () =
        match this with
        | CpuId.CpuId i -> $"<cpu #%i{i}>"

/// <summary>
/// The operating-system thread identifier which the simulated kernel reports for a
/// thread.
/// </summary>
/// <example>
/// This is what <c>gettid(2)</c> returns on Linux, and what
/// <c>pthread_threadid_np(3)</c> returns on Darwin.
/// </example>
type OsThreadId =
    | OsThreadId of uint32

    /// <summary>
    /// A human-readable description of the thread ID.
    /// </summary>
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
        /// <summary>
        /// The open file description of the port being waited on.
        /// </summary>
        Port : OpenFileDescriptionId
        /// <summary>
        /// The <c>*count</c> read at entry.
        /// </summary>
        /// <remarks>
        /// A real <c>epoll_wait</c> keeps using the maxevents it was passed,
        /// even if the guest overwrites the cell mid-wait.
        /// </remarks>
        MaxEvents : int
    }

/// <summary>
/// One task's in-flight <c>flock</c> acquisition: which open file description it is
/// waiting to lock, and how.
/// </summary>
/// <remarks>
/// This is exactly the payload of the <c>WakeCondition</c> that parked it.
/// </remarks>
type ParkedFlock =
    {
        /// <summary>
        /// The open file description whose lock is being waited for.
        /// </summary>
        Requester : OpenFileDescriptionId
        /// <summary>
        /// The lock that the guest asked for.
        /// </summary>
        /// <remarks>
        /// This is <i>not</i> necessarily a lock which is currently held.
        /// In the case of a "conversion" (<c>flock(2)</c>'s term for re-acquiring a lock in a different
        /// mode on a description which already holds a lock), the kernel may already have dropped whatever lock
        /// this description held; Linux models conversion as "drop-then-reacquire".
        /// So this is what it will hold if the acquisition ever completes, not necessarily what it holds
        /// now.
        /// (This is Linux-specific behaviour; Darwin keeps the lock which a failed conversion would have dropped.)
        /// </remarks>
        Mode : FlockMode
    }

/// <summary>
/// The syscall a task is blocked in, if it is blocked in one.
/// </summary>
/// <remarks>
/// One case per parking syscall.
///
/// This is stored as a single field on the task holding it, because
/// a task blocks in at most one syscall at a time.
///
/// Each case's payload is its own syscall's business: what parks a
/// task is generally arbitrary state specific to that syscall.
/// </remarks>
[<RequireQualifiedAccess>]
type ParkedSyscall =
    | SocketWait of ParkedSocketWait
    | Flock of ParkedFlock

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
        /// The syscall this task is blocked in, if it is blocked in one.
        ///
        /// A real kernel holds a blocked task's in-flight syscall arguments on
        /// its stack; this is that. Three readers, and they must agree, which is
        /// why there is one of it: the re-entry consults it rather than the
        /// guest's argument cells, which the guest may have written since;
        /// whatever a client polls to decide the call can be finished reads it
        /// to learn what the call is waiting for; and `close` reads it to refuse
        /// destroying a description something is waiting on — a rule about
        /// kernel objects, which this library can only apply to a park it can
        /// see.
        ///
        /// Every payload holds kernel objects by *identity*, never by descriptor
        /// number: a sleeping task keeps the object rather than the number, and
        /// descriptor numbers are reused as soon as they are free.
        Parked : ParkedSyscall option
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
                Parked = None
            }
            tasks

    /// The logical processor `name` runs on, as `sched_getcpu` reports it.
    let cpuOf<'Task when 'Task : comparison> (name : 'Task) (tasks : Map<'Task, UnixTaskState>) : CpuId =
        (get name tasks).Cpu

    /// The OS thread id `name` reports to the guest.
    let osThreadIdOf<'Task when 'Task : comparison> (name : 'Task) (tasks : Map<'Task, UnixTaskState>) : OsThreadId =
        (get name tasks).OsThreadId

    /// The syscall `name` is blocked in, if any.
    let parkedFor<'Task when 'Task : comparison>
        (name : 'Task)
        (tasks : Map<'Task, UnixTaskState>)
        : ParkedSyscall option
        =
        (get name tasks).Parked

    /// Record that `name` has parked in a syscall, or (with `None`) that it is
    /// no longer in one.
    ///
    /// Refuses to replace a park of one syscall with a park of another. A task
    /// runs no guest code between a wake and its re-entry into the syscall it
    /// woke from, so the only lawful writes are onto an absent record and onto a
    /// park of the same syscall — the re-park a beaten waiter performs. Anything
    /// else means a completion path failed to clear its record, and without this
    /// the next park would quietly overwrite the evidence: the invariant check is
    /// a test-time oracle, so this is where a live run gets told.
    ///
    /// Equality is deliberately not required of a same-syscall re-park. A
    /// re-parking call may lawfully revise its own re-entry state — a timeout
    /// with less of itself left to run is the obvious future instance — and that
    /// is the syscall's business rather than this table's.
    ///
    /// Clients park in an `flock` through `UnixSystem.parkFlock` rather than
    /// through this, which is what ties that record to the condition that
    /// produced it; this is how any park is cleared, and how `parkFlock` writes.
    let withParked<'Task when 'Task : comparison>
        (name : 'Task)
        (parked : ParkedSyscall option)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        let existing = get name tasks

        match existing.Parked, parked with
        | Some (ParkedSyscall.SocketWait _), Some (ParkedSyscall.Flock _)
        | Some (ParkedSyscall.Flock _), Some (ParkedSyscall.SocketWait _) ->
            failwith
                $"UnixTaskTable.withParked: task %O{name} is parked in %A{existing.Parked} and something is parking it in %A{parked} without clearing the first. A task blocks in one syscall at a time, so the earlier park's completion failed to clear its record."
        | _ -> ()

        Map.add
            name
            { existing with
                Parked = parked
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
