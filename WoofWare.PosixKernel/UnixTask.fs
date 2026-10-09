namespace WoofWare.PosixKernel

/// <summary>
/// Index of one of the simulated process's logical processors.
/// </summary>
/// <example>
/// As reported to a process by <c>sched_getcpu(3)</c>.
/// </example>
type CpuId =
    | CpuId of int

    /// <summary>
    /// A human-readable description of the CPU ID.
    /// </summary>
    override this.ToString () =
        match this with
        | CpuId.CpuId i -> $"<cpu #%i{i}>"

/// One thread's in-flight `epoll_wait`: the state the syscall captured when it
/// was entered, which outlives anything the process does to its arguments
/// afterwards. The epoll instance is held by *description identity*, exactly as
/// the real syscall holds a file reference: the fd the wait was called through
/// is never consulted again, and the instance lives at least as long as the
/// wait (`ParkedSyscall.descriptions`).
type ParkedEpollWait =
    {
        /// <summary>
        /// The open file description of the epoll instance being waited on.
        /// </summary>
        Epoll : OpenFileDescriptionId
        /// <summary>
        /// The <c>maxevents</c> the call was made with.
        /// </summary>
        MaxEvents : int
        /// The buffer the call was given to copy events out to, as the caller
        /// classified it when the call was entered.
        Buffer : UserBuffer
        /// The instant, in nanoseconds since boot, at which the wait stops and
        /// returns no events; or `None` for a wait that lasts until an event is
        /// deliverable.
        Deadline : int64 option
    }

/// One task's in-flight `kevent(2)` wait on a kqueue: the state the call
/// captured when it was entered.
type ParkedKevent =
    {
        /// The open file description of the kqueue being waited on.
        ///
        /// Held as the real call holds a file reference: the kqueue lives at
        /// least as long as the wait (`ParkedSyscall.descriptions`), so a close
        /// of `Fd` drains it rather than destroying it under the wait.
        Kqueue : OpenFileDescriptionId
        /// The descriptor the call was made through.
        ///
        /// Kept beside the description because a close of this descriptor, and
        /// of no other, ends the wait (see `KqueueState.Drained`).
        Fd : int
        /// The `nevents` the call was made with: positive, since a call with
        /// none to take returns at once.
        MaxEvents : int
        /// The eventlist the call was given to copy events out to, as the
        /// caller classified it when the call was entered.
        Buffer : UserBuffer
        /// The instant, in nanoseconds since boot, at which the wait stops and
        /// returns no events; or `None` for a wait with a null timeout.
        Deadline : int64 option
    }

/// <summary>
/// One task's in-flight <c>flock</c> acquisition: which open file description it is
/// waiting to lock, and how.
/// </summary>
/// <remarks>
/// This is exactly the payload of the <c>WakePrimitive.FlockGrantable</c> that parked it.
/// </remarks>
type ParkedFlock =
    {
        /// <summary>
        /// The open file description whose lock is being waited for.
        /// </summary>
        Requester : OpenFileDescriptionId
        /// <summary>
        /// The lock that the caller asked for.
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

/// One entry of a `poll(2)` call, as its caller supplied it: `struct pollfd`'s
/// `fd` and `events`, without the `revents` the kernel writes back.
type PollEntry =
    {
        /// The descriptor to poll. A negative one is not an error: measured on
        /// both kernels, it is ignored, reports nothing, and does not count
        /// towards the return value.
        Fd : int
        /// What the caller asked about: `events`, as raw bits in the simulated
        /// flavour's own `<poll.h>` numbering.
        ///
        /// Under Linux, `POLLERR`, `POLLHUP` and `POLLNVAL` are reported whether
        /// or not they appear here. Under Darwin, a request names kqueue filters
        /// rather than conditions, and a request of none of the bits that
        /// register one (`ERR`, `NVAL` and the bits above `POLLWRITE` among
        /// them) reports nothing at all, even for a descriptor that is not open.
        Events : int16
    }

/// One entry of a parked Linux-flavoured `poll(2)`, as the call captured it
/// when it went to sleep.
[<RequireQualifiedAccess>]
type ParkedPollEntry =
    /// A negative descriptor, which `poll` ignores: it reports nothing and
    /// waits on nothing.
    | Ignored of fd : int
    /// The descriptor `fd`, asked for `events` (raw bits in the flavour's own
    /// `<poll.h>` numbering), and the open file description `fd` named when the
    /// call went to sleep.
    ///
    /// Both halves are kept because a real poll uses both: it sleeps on the
    /// description it found, which it holds, and when it wakes it looks `fd` up
    /// again. `close` refuses to close `fd` while this entry waits, so the two
    /// stay in step.
    | Watched of fd : int * description : OpenFileDescriptionId * events : int16

/// One task's in-flight Linux-flavoured `poll(2)`: every entry the call was made with, in order,
/// and when it times out.
type ParkedPoll =
    {
        /// The entries, in the caller's order, which is the order the finishing
        /// call reports `revents` in.
        Entries : ParkedPollEntry list
        /// The instant, in nanoseconds since boot, at which the call stops
        /// waiting and returns 0; or `None` for a call that waits until a
        /// descriptor is ready.
        Deadline : int64 option
    }

/// One filter a Darwin-flavoured `poll(2)` registered in the kqueue it makes
/// for its own use, keyed beside it by the descriptor number and the filter.
///
/// Darwin's `poll` registers, for each entry, one filter per group of the bits
/// it asks for, each added once only (`EV_ONESHOT`): the first report it makes
/// removes it, whether or not that report adds anything to the entry.
type PollRegistration =
    {
        /// The index, in the call's entries, of the entry the filter reports
        /// into: the last entry naming this descriptor and filter, since a
        /// later entry's registration of the same pair replaces the earlier's
        /// `udata` rather than adding a filter (measured).
        Entry : int
        /// Whether the entry that first registered the pair asked for
        /// `POLLPRI` or `POLLRDBAND`, which registers `EVFILT_READ` with
        /// `EV_OOBAND`. A later entry's registration keeps the first one's
        /// flags (measured). A socket's filter ignores the flag; every other
        /// target's reports it back, and `poll` then answers `POLLPRI` and
        /// `POLLRDBAND` for a ready read.
        OutOfBand : bool
        /// Where the pair's first registration stands in the order the call
        /// made them. One event that activates several registrations of a
        /// socket's filter, made through different descriptors onto it,
        /// queues the latest-made first, as it does in a kqueue `kevent` fills.
        RegisteredAt : int
        /// For a socket's filter, the socket the descriptor named when the
        /// call registered it: what the filter is attached to, so that an
        /// event on the socket reaches it whichever process's call caused the
        /// event, without reading any descriptor table. `None` for a pipe's or
        /// a regular file's filter, which nothing activates (see
        /// `PollQueue.Active`).
        Socket : SocketId option
    }

/// The identity of the kqueue a Darwin `poll(2)` made for its own use
/// (`PollQueue`). Minted from a counter the machine keeps, and never reused.
type PollQueueId =
    | PollQueueId of int64

    /// A human-readable description of the identity.
    override this.ToString () =
        match this with
        | PollQueueId.PollQueueId i -> $"<poll queue #%i{i}>"

/// The kqueue a Darwin `poll(2)` made for its own use, while the call sleeps.
///
/// Darwin builds `poll` over kqueue: the call registers a filter per group of
/// requested bits in a kqueue that has no descriptor, and translates what the
/// filters report back into `revents`. A sleeping call keeps that kqueue, and
/// what it reports when it wakes depends on what has been activated in it, and
/// in which order -- not only on what its descriptors present then.
///
/// Held on the machine rather than in the call's park, because a socket event
/// in any process's call activates its filters, as one does a kqueue's
/// (`KqueueRegistration.Socket`), and no process's view holds another's tasks.
/// It lives exactly as long as the park naming it (`ParkedKqueuePoll.Queue`):
/// the park's end destroys it.
type PollQueue =
    {
        /// The process whose `poll` made the queue, whose descriptor table its
        /// registrations' descriptor numbers are read in.
        Owner : ProcessId
        /// The filters still registered: those that have not yet reported, keyed
        /// by descriptor number and filter. Closing a descriptor removes every
        /// one made through it, as it does from a kqueue a process holds.
        Registrations : Map<int * KqueueFilter, PollRegistration>
        /// The registrations of sockets' filters something has activated since
        /// the call last scanned, in the order they were activated: the order
        /// a scan reports them in, which decides whether a reported `POLLHUP`
        /// suppresses a socket's `POLLOUT`. Always a subset of
        /// `Registrations`, with no duplicates, naming sockets alone.
        ///
        /// A pipe's filters are not listed: every operation that makes one
        /// ready activates it (measured), and the order a pipe entry's two
        /// filters report in cannot change what the entry answers, so a scan
        /// reports a pipe's registration exactly while its filter is ready.
        Active : (int * KqueueFilter) list
    }

/// One task's in-flight Darwin-flavoured `poll(2)`: its entries, the kqueue
/// the call made for its own use, and when it times out.
type ParkedKqueuePoll =
    {
        /// The entries, in the caller's order, which is the order the
        /// finishing call reports `revents` in.
        Entries : PollEntry list
        /// The kqueue the call made for its own use, held on the machine. The
        /// park holds it: it is destroyed when the park ends.
        Queue : PollQueueId
        /// The instant, in nanoseconds since boot, at which the call stops
        /// waiting and returns 0; or `None` for a call that waits until
        /// something is reported.
        Deadline : int64 option
    }

/// What a sleeping call that waits on one open file description waits on, or
/// that a close has ended it: the state of a blocking `accept(2)`, or of a
/// blocking `read(2)` or `write(2)` of a pipe or a connected stream socket.
///
/// `'Object` names the kernel object the call waited on, by the machine's own
/// identity for it (a `SocketId` or a `PipeId`), for a call that no longer
/// holds a description to name it by.
[<RequireQualifiedAccess>]
type SleepTarget<'Object> =
    /// The call waits on the open file description `description`, which it
    /// holds, having been made through the descriptor `fd`.
    ///
    /// Held by description rather than by descriptor: a `dup` of the descriptor
    /// names the same description, and the call keeps it whatever is closed.
    /// Under Darwin, `fd` names `description` for as long as the call waits,
    /// since a close of it ends the call (`EndedByClose`). Under Linux a close
    /// of `fd` leaves the call waiting, so the number can be freed, and taken
    /// by a later open, while the call sleeps; nothing reads it there.
    | Waiting of description : OpenFileDescriptionId * fd : int
    /// Under Darwin, a close has ended the call, which waited on `object`. It
    /// holds nothing, and its finishing call answers what the close left it.
    ///
    /// A pipe or connection transfer is ended by a close of the descriptor it
    /// was made through; an accept by a close of the descriptor any accept on
    /// the same listener was made through (see `ListenState.Drained`).
    | EndedByClose of object : 'Object

[<RequireQualifiedAccess>]
module SleepTarget =
    /// The open file description the call waits on and holds, or `None` once a
    /// close has ended it.
    let description<'Object> (target : SleepTarget<'Object>) : OpenFileDescriptionId option =
        match target with
        | SleepTarget.Waiting (description, _) -> Some description
        | SleepTarget.EndedByClose _ -> None

/// One task's in-flight blocking `accept(2)`: the listening socket it waits on
/// for a connection, and where the connection's peer address goes when one
/// arrives.
type ParkedAccept =
    {
        /// The open file description of the listening socket the call was made
        /// through, and the descriptor it was made through; or the listening
        /// socket, once a close has ended the call.
        ///
        /// Under Linux the descriptor the call came through can be closed while
        /// it sleeps, the last one included, leaving the listener to the call.
        Listener : SleepTarget<SocketId>
        /// Where the peer address is to be copied out to, as the caller
        /// classified it when the call was entered.
        Destination : UserBuffer
        /// How many bytes of the peer address the caller's length cell allows
        /// to be written: the 32-bit word in the cell, which Linux reads as an
        /// `int` and Darwin as a `socklen_t` (see `UnixConnection.accept`).
        ///
        /// This is the length the call was entered with. Darwin reads the
        /// caller's cell then; Linux reads it only when it copies the address
        /// out, after the wait, so under Linux this is right for a caller whose
        /// cell nothing writes while the call sleeps.
        DeclaredLength : uint32
    }

/// One task's in-flight blocking `read(2)` of a pipe that held nothing while a
/// write end was open.
type ParkedPipeRead =
    {
        /// The open file description of the pipe's read end the call was made
        /// through, and the descriptor it was made through; or the pipe, once a
        /// close has ended the call.
        Reader : SleepTarget<PipeId>
        /// Where the bytes are to be copied out to, as the caller classified it
        /// when the call was entered. Nothing is copied before the call sleeps,
        /// so a buffer naming no storage faults only once there is something to
        /// copy.
        Buffer : UserBuffer
        /// The most bytes the call takes: its count, after the platform's limit
        /// on one call's transfer. Never zero, since a read of nothing returns
        /// at once.
        Count : int
    }

/// One task's in-flight blocking `write(2)` into a pipe that had no room for all
/// of it.
///
/// Holds no bytes. A sleeping write copies from the caller's buffer as room
/// appears, so bytes a process writes into its buffer while the call sleeps
/// are the bytes that reach the pipe; the call that finishes the write asks
/// the caller for them then.
type ParkedPipeWrite =
    {
        /// The open file description of the pipe's write end the call was made
        /// through, and the descriptor it was made through; or the pipe, once a
        /// close has ended the call.
        Writer : SleepTarget<PipeId>
        /// Where the bytes come from, as the caller classified it when the call
        /// was entered.
        Buffer : UserBuffer
        /// How many bytes the call writes in all: its count, after the
        /// platform's limit on one call's transfer.
        Count : int
        /// How many of them, from the start, are already in the pipe: less than
        /// `Count`.
        Written : int
        /// The pipe's `PipeState.Reads` when the write went to sleep: a read
        /// since then is one that woke it, on Darwin.
        ReadsSeen : int64
    }

/// One task's in-flight blocking `read(2)` of a connected stream socket that
/// had nothing to answer: no bytes, no FIN and no reset.
type ParkedConnectionRead =
    {
        /// The open file description of the socket the call was made through,
        /// and the descriptor it was made through; or the socket, once a close
        /// has ended the call.
        Socket : SleepTarget<SocketId>
        /// Where the bytes are to be copied out to, as the caller classified it
        /// when the call was entered. Nothing is copied before the call sleeps.
        Buffer : UserBuffer
        /// The most bytes the call takes: its count, after the platform's limit
        /// on one call's transfer. Never zero, since a read of nothing returns
        /// at once.
        Count : int
    }

/// One task's in-flight blocking `write(2)` to a connected stream socket whose
/// buffers had no room for all of it.
///
/// Holds no bytes, as `ParkedPipeWrite` holds none: the call that finishes the
/// write asks the caller for the next of them as room appears.
type ParkedConnectionWrite =
    {
        /// The open file description of the socket the call was made through,
        /// and the descriptor it was made through; or the socket, once a close
        /// has ended the call.
        Socket : SleepTarget<SocketId>
        /// Where the bytes come from, as the caller classified it when the call
        /// was entered.
        Buffer : UserBuffer
        /// How many bytes the call writes in all: its count, after the
        /// platform's limit on one call's transfer.
        Count : int
        /// How many of them, from the start, the connection has taken already:
        /// less than `Count`.
        Written : int
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
    | EpollWait of ParkedEpollWait
    | Kevent of ParkedKevent
    | Flock of ParkedFlock
    | Poll of ParkedPoll
    | KqueuePoll of ParkedKqueuePoll
    | Accept of ParkedAccept
    | PipeRead of ParkedPipeRead
    | PipeWrite of ParkedPipeWrite
    | ConnectionRead of ParkedConnectionRead
    | ConnectionWrite of ParkedConnectionWrite
    /// `sigsuspend(2)`, or `pause(2)`, which is `sigsuspend` with the mask the
    /// task already has (`UnixSignal.pause`). Carries nothing: the temporary
    /// mask is the task's mask while it sleeps, and the mask the call replaced
    /// is the signal state's (`SignalState.maskToRestore`), because it outlives
    /// the park, until the task returns to user mode.
    | SigSuspend

[<RequireQualifiedAccess>]
module ParkedSyscall =
    /// The open file descriptions the call holds while it is in flight, as a
    /// real syscall holds a reference to each file it found: each stays alive
    /// until the call returns, whatever descriptors are closed meanwhile.
    ///
    /// An accept, or a pipe or connection transfer, a Darwin close has ended
    /// (`SleepTarget.EndedByClose`) holds none: it returned, as far as the
    /// kernel is concerned, before the close did.
    ///
    /// A Linux `poll` holds every description it watches, as Linux's holds each
    /// file whose wait queue it sleeps on; a Darwin one holds none.
    let descriptions (parked : ParkedSyscall) : OpenFileDescriptionId list =
        match parked with
        | ParkedSyscall.EpollWait wait -> [ wait.Epoll ]
        | ParkedSyscall.Kevent wait -> [ wait.Kqueue ]
        | ParkedSyscall.Flock parked -> [ parked.Requester ]
        | ParkedSyscall.Poll poll ->
            poll.Entries
            |> List.choose (fun entry ->
                match entry with
                | ParkedPollEntry.Ignored _ -> None
                | ParkedPollEntry.Watched (_, description, _) -> Some description
            )
        // XNU's poll holds no file across its sleep: a filter it registered
        // goes when the descriptor it was registered through closes
        // (`knote_fdclose`), and the file with it if that was the last
        // reference. Measured (`poll-timeout.c` section E): a datagram sent to
        // the address of a socket closed under a sleeping Darwin poll wakes
        // nothing. The kqueue it made is its own (`ParkedKqueuePoll.Queue`).
        | ParkedSyscall.KqueuePoll _ -> []
        // A call a close has ended holds nothing: Darwin's close does not
        // return until the call has, and the call's reference goes as it does.
        | ParkedSyscall.Accept accept -> SleepTarget.description accept.Listener |> Option.toList
        | ParkedSyscall.PipeRead read -> SleepTarget.description read.Reader |> Option.toList
        | ParkedSyscall.PipeWrite write -> SleepTarget.description write.Writer |> Option.toList
        | ParkedSyscall.ConnectionRead read -> SleepTarget.description read.Socket |> Option.toList
        | ParkedSyscall.ConnectionWrite write -> SleepTarget.description write.Socket |> Option.toList
        | ParkedSyscall.SigSuspend -> []

/// Where one park stands in the order every park on this machine was made in.
///
/// Minted from a counter the machine keeps as each task parks, so of any two
/// tasks parked at once, the one parked earlier holds the smaller ordinal.
/// A kernel that wakes one waiter of several reads this to decide which.
type ParkOrdinal =
    | ParkOrdinal of int64

    /// A human-readable description of the ordinal.
    override this.ToString () =
        match this with
        | ParkOrdinal.ParkOrdinal i -> $"<park #%i{i}>"

/// One task's park: the syscall it is blocked in, and where the park stands in
/// the order parks were made in.
type TaskPark =
    {
        /// The syscall the task is blocked in, with the state it was entered with.
        Syscall : ParkedSyscall
        /// When the task parked, relative to every other park.
        ///
        /// A re-park (a woken waiter that finds its condition gone and sleeps
        /// again) mints a fresh one, as a real kernel queues such a waiter
        /// afresh.
        Ordinal : ParkOrdinal
    }

/// What the emulated kernel knows about one task — one scheduling entity, what
/// `gettid(2)` names.
///
/// A process starts with one (`UnixSystem.initial`), every thread it creates adds
/// one (`UnixTaskLifecycle.spawn`), and the thread's exit removes it
/// (`UnixTaskLifecycle.exitThread`), so the table holds a task for each thread
/// from its creation until its exit. That is what makes the record total:
/// `Cpu` and `OsThreadId` have no truthful default, so a `Map` of each would
/// have no honest answer for an absent key, and the answer is that there is
/// never an absent key rather than that a default exists.
///
/// The per-thread errno is *not* here. On a real Unix errno lives in libc, not
/// in the kernel: the kernel returns an error code and the syscall wrapper
/// stores it, so the slot belongs to the client.
type UnixTaskState =
    internal
        {
            /// The simulated logical processor this task is pinned to: what
            /// `sched_getcpu(3)` reports while it runs.
            ///
            /// Assigned once, when the task is created: the processor its creator
            /// names to `UnixTaskLifecycle.spawn`. This library has no scheduler:
            /// under a client that runs one task at a time and never migrates one
            /// between cores, "pinned to" and "currently executing on" coincide, and
            /// a core-aware client would rewrite this.
            Cpu : CpuId
            /// The OS thread identifier this task reports, as `gettid(2)` does.
            ///
            /// Minted by the machine's `ThreadIdAllocator` when the task is created,
            /// and fixed from then on. No two live tasks share one. On Linux an exited
            /// task's id comes back once the counter wraps at `pid_max`, so a stale
            /// owner identity recorded by a user-space lock can then be mistaken for a
            /// live owner, as it can on a real Linux.
            OsThreadId : OsThreadId
            /// The syscall this task is blocked in, and where that park stands in park
            /// order, if it is blocked in one.
            ///
            /// A real kernel holds a blocked task's in-flight syscall arguments on
            /// its stack; this is that. Three readers, and they must agree, which is
            /// why there is one of it: the re-entry consults it rather than the
            /// caller's argument cells, which the process may have written since;
            /// whatever a client polls to decide the call can be finished reads it
            /// to learn what the call is waiting for; and the machine's open file
            /// table counts a hold on each description it names
            /// (`ParkedSyscall.descriptions`, `OpenFileTable.holdCount`), so the
            /// description lives until the call returns.
            ///
            /// Every payload holds kernel objects by *identity*, never by descriptor
            /// number: a sleeping task keeps the object rather than the number, and
            /// descriptor numbers are reused as soon as they are free.
            Parked : TaskPark option
        }

/// Reading one task.
///
/// A client gets a task's state by looking its name up in `UnixSystem.tasks`,
/// which holds no entry for a name the process has no task by, so each reader
/// here answers for every state it can be given.
[<RequireQualifiedAccess>]
module UnixTaskState =

    /// The logical processor `task` runs on, as `sched_getcpu(3)` reports it:
    /// the one its creator named when it was created.
    let cpu (task : UnixTaskState) : CpuId = task.Cpu

    /// The OS thread ID `task` reports, as `gettid(2)` does: fixed when the
    /// task is created, and held by no other live task on the machine.
    let osThreadId (task : UnixTaskState) : OsThreadId = task.OsThreadId

    /// The syscall `task` is blocked in, and where that park stands in park
    /// order, if it is blocked in one. The park holds kernel objects by
    /// identity, never by descriptor number: a sleeping task keeps the object
    /// rather than the number, and descriptor numbers are reused as soon as
    /// they are free.
    let park (task : UnixTaskState) : TaskPark option = task.Parked

    /// The syscall `task` is blocked in, if it is blocked in one: `park`
    /// without its place in park order.
    let parkedIn (task : UnixTaskState) : ParkedSyscall option =
        task.Parked |> Option.map (fun park -> park.Syscall)

/// The tasks a simulated process owns, by whatever a client uses to name one.
///
/// Generic in the task name for the same reason `SignalState` is: the identity
/// of a scheduling entity is the client's, not this library's.
[<RequireQualifiedAccess>]
module UnixTaskTable =

    /// The task `name` is.
    ///
    /// Total, and loudly partial rather than an option: every task is added
    /// when it is created and removed only when it exits, so a name that
    /// resolves to nothing is a client bug rather than anything a process did.
    let internal get<'Task when 'Task : comparison> (name : 'Task) (tasks : Map<'Task, UnixTaskState>) : UnixTaskState =
        match Map.tryFind name tasks with
        | Some task -> task
        | None ->
            failwith
                $"UnixTaskTable.get: %O{name} names no task. Every task enters the table when its thread is created, by `UnixSystem.initial` or `UnixTaskLifecycle.spawn`, and leaves it when the thread exits, so this one was never created or has already exited (this is a bug in the client)."

    /// Add the task for a newly-created scheduling entity.
    ///
    /// Internal so that the only routes by which a task enters the table are
    /// `UnixSystem.initial` and `UnixTaskLifecycle.spawn`, which mint its id.
    let internal add<'Task when 'Task : comparison>
        (name : 'Task)
        (cpu : CpuId)
        (osThreadId : OsThreadId)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        if Map.containsKey name tasks then
            failwith
                $"UnixTaskTable.add: %O{name} already names a task. A task is created once, and adding it again would silently discard whatever the first creation recorded (this is a bug in the client)."

        Map.add
            name
            {
                Cpu = cpu
                OsThreadId = osThreadId
                Parked = None
            }
            tasks

    /// The syscall `name` is blocked in, if any. Fails loudly, as `get` does,
    /// for a name that is not a task; a client reads `UnixTaskState.parkedIn`
    /// of the state it looked up instead.
    let internal parkedFor<'Task when 'Task : comparison>
        (name : 'Task)
        (tasks : Map<'Task, UnixTaskState>)
        : ParkedSyscall option
        =
        UnixTaskState.parkedIn (get name tasks)

    /// The park `name` is in, with its place in park order, if it is parked.
    let internal parkOf<'Task when 'Task : comparison>
        (name : 'Task)
        (tasks : Map<'Task, UnixTaskState>)
        : TaskPark option
        =
        (get name tasks).Parked

    /// Record that `name` is in `park`.
    ///
    /// Refuses to replace a park of one syscall with a park of another. A task
    /// runs no user code between a wake and its re-entry into the syscall it
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
    /// The table alone: it moves no hold on an open file description. Internal
    /// so that `UnixWait.park`, which mints the ordinal, and `UnixParkState`,
    /// which moves the holds, are the ways a park is written.
    let internal withPark<'Task when 'Task : comparison>
        (name : 'Task)
        (park : TaskPark)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        let existing = get name tasks

        // Which syscall a park is of, and nothing else about it.
        let kind (parked : ParkedSyscall) : int =
            match parked with
            | ParkedSyscall.EpollWait _ -> 0
            | ParkedSyscall.Flock _ -> 1
            | ParkedSyscall.Poll _ -> 2
            | ParkedSyscall.Accept _ -> 3
            | ParkedSyscall.PipeRead _ -> 4
            | ParkedSyscall.PipeWrite _ -> 5
            | ParkedSyscall.Kevent _ -> 6
            | ParkedSyscall.KqueuePoll _ -> 7
            | ParkedSyscall.ConnectionRead _ -> 8
            | ParkedSyscall.ConnectionWrite _ -> 9
            | ParkedSyscall.SigSuspend -> 10

        let sameSyscall =
            match existing.Parked with
            | None -> true
            | Some existing -> kind existing.Syscall = kind park.Syscall

        if not sameSyscall then
            failwith
                $"UnixTaskTable.withPark: task %O{name} is parked in %A{existing.Parked} and something is parking it in %A{park} without clearing the first. A task blocks in one syscall at a time, so the earlier park's completion failed to clear its record."

        Map.add
            name
            { existing with
                Parked = Some park
            }
            tasks

    /// Record that `name` is no longer parked: its syscall has finished.
    ///
    /// The table alone: the holds the park took stay recorded in the open file
    /// table, where `UnixSystem.checkInvariants` reports them
    /// (`HoldCountMismatch`). `UnixParkState.unpark` ends a park and lets go of
    /// its holds.
    let internal unpark<'Task when 'Task : comparison>
        (name : 'Task)
        (tasks : Map<'Task, UnixTaskState>)
        : Map<'Task, UnixTaskState>
        =
        let existing = get name tasks

        Map.add
            name
            { existing with
                Parked = None
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
