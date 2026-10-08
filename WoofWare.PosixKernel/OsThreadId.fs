namespace WoofWare.PosixKernel

open System

/// <summary>
/// The operating-system thread identifier which the simulated kernel reports for a
/// thread.
/// </summary>
/// <remarks>
/// Only this library mints one: <c>UnixSystem.initial</c> for a process's first
/// task, and <c>UnixTaskLifecycle.spawn</c> for every later one.
/// </remarks>
/// <example>
/// This is what <c>gettid(2)</c> returns on Linux, and what
/// <c>pthread_threadid_np(3)</c> returns on Darwin.
/// </example>
[<Struct>]
type OsThreadId =
    private
    | OsThreadId of uint64

    /// <summary>
    /// A human-readable description of the thread ID.
    /// </summary>
    override this.ToString () =
        match this with
        | OsThreadId i -> $"<os thread #%i{i}>"

[<RequireQualifiedAccess>]
module OsThreadId =

    /// The id as an unsigned 64-bit number: the width of Darwin's
    /// `pthread_threadid_np`. A Linux tid is a positive `pid_t`, so it is the same
    /// number there too.
    let toUInt64 (id : OsThreadId) : uint64 =
        match id with
        | OsThreadId i -> i

/// Each flavour's own counter a machine hands out thread ids from.
///
/// Linux takes a thread's id from the counter it takes process ids from; the
/// process's first task's id is the process id. Darwin takes it from one 64-bit
/// counter shared by every process on the machine, unrelated to the process id.
[<RequireQualifiedAccess>]
type internal ThreadIdCounter =
    internal
    /// `cursor` is where the next search for a free id starts; the ids it hands
    /// out are below `pidMax`, which may since have been lowered beneath ids
    /// handed out earlier.
    | Linux of cursor : int32 * pidMax : int32
    /// `next` is the id the next thread gets.
    | Darwin of next : uint64

/// How a machine hands out thread ids: its flavour's counter, and the ids of
/// every live task on the machine, in every process, which it never hands out
/// again while they live.
///
/// The machine's rather than a process's, as a real kernel's pid allocator is,
/// so that no process can be handed an id another process's task holds.
type ThreadIdAllocator =
    internal
        {
            Counter : ThreadIdCounter
            /// The ids of every live task on the machine. `spawn` adds the new
            /// task's, and a task's exit removes its own.
            Live : Set<OsThreadId>
        }

/// What `ThreadIdAllocator.allocate` did.
[<RequireQualifiedAccess>]
type internal ThreadIdAllocation =
    /// `id` is handed out, and is live in `allocator` from then on.
    | Issued of id : OsThreadId * allocator : ThreadIdAllocator
    /// Every id Linux would hand out is held, so the thread's creation fails
    /// with `error`.
    | Failed of error : UnixError
    /// Darwin's counter has reached the top of its 64-bit range, and what
    /// Darwin does next has not been measured.
    | DarwinCounterExhausted

[<RequireQualifiedAccess>]
module ThreadIdAllocator =

    // Linux's `RESERVED_PIDS`: once the counter has passed it, a search that runs
    // off the top of the range starts again here, never lower.
    let private reservedPids : int32 = 300

    /// The least value Linux's `pid_max` takes.
    [<Literal>]
    let linuxPidMaxFloor : int32 = 301

    /// The greatest value Linux's `pid_max` takes on a 64-bit kernel.
    [<Literal>]
    let linuxPidMaxCeiling : int32 = 4194304

    // Measured on Linux 6.18.5 aarch64 and x86-64 by
    // `docs/plans/2026-08-23-posix-kernel-extraction/pid-allocation.c`: writing
    // `/proc/sys/kernel/pid_max` accepts exactly 301..4194304, and answers EINVAL for
    // every other value swept.
    let private assertPidMax (context : string) (pidMax : int32) : unit =
        if pidMax < linuxPidMaxFloor || pidMax > linuxPidMaxCeiling then
            failwith
                $"%s{context}: %d{pidMax} is not a pid_max Linux accepts; it must be between %d{linuxPidMaxFloor} and %d{linuxPidMaxCeiling} (the sysctl answers EINVAL outside that range)."

    /// A Linux allocator whose process has id `pid`, below `pidMax`, and has
    /// just been started: the process's first task's id, which is `pid`, and the
    /// allocator after it, with that id its one live id.
    let internal startLinux (context : string) (pidMax : int32) (pid : ProcessId) : OsThreadId * ThreadIdAllocator =
        assertPidMax context pidMax
        let pid = ProcessId.toInt32 (ProcessId.assertValid context pid)

        if pid >= pidMax then
            failwith
                $"%s{context}: process ID %d{pid} is not below pid_max %d{pidMax}, so a Linux kernel could not have handed it out."

        let leader = OsThreadId (uint64 pid)

        leader,
        {
            Counter = ThreadIdCounter.Linux (pid + 1, pidMax)
            Live = Set.singleton leader
        }

    /// A Darwin allocator whose first id is `first`: the process's first task's
    /// id, and the allocator after it, with that id its one live id.
    let internal startDarwin (context : string) (first : uint64) : OsThreadId * ThreadIdAllocator =
        // Neither end has been observed. 0 is not refused because a kernel was
        // seen not to report it, but because nothing says one would, and
        // `UInt64.MaxValue` leaves no id for a second thread.
        if first = 0UL || first = UInt64.MaxValue then
            failwith
                $"%s{context}: %d{first} is not a thread ID this library will start a Darwin counter at; it must be between 1 and %d{UInt64.MaxValue - 1UL}."

        let leader = OsThreadId first

        leader,
        {
            Counter = ThreadIdCounter.Darwin (first + 1UL)
            Live = Set.singleton leader
        }

    /// The Linux allocator `allocator` is, with its `pid_max` set to `pidMax`.
    ///
    /// Throws for a Darwin allocator, which has no `pid_max`, and for a value
    /// Linux does not accept.
    let internal withPidMax (context : string) (pidMax : int32) (allocator : ThreadIdAllocator) : ThreadIdAllocator =
        match allocator.Counter with
        | ThreadIdCounter.Linux (cursor, _) ->
            assertPidMax context pidMax

            { allocator with
                Counter = ThreadIdCounter.Linux (cursor, pidMax)
            }
        | ThreadIdCounter.Darwin _ ->
            failwith
                $"%s{context}: Darwin has no pid_max; its thread IDs come from a 64-bit counter that no setting bounds."

    /// Whether `id` is one `allocator` could have handed out: on Linux, one below
    /// the greatest `pid_max` Linux takes, and on Darwin, one below the counter.
    let internal couldHaveMinted (id : OsThreadId) (allocator : ThreadIdAllocator) : bool =
        let id = OsThreadId.toUInt64 id

        match allocator.Counter with
        // Not the `pid_max` now in force: a write may lower it beneath a live id,
        // which keeps that id (`pid-max-below-live.c`), so the counter may have
        // handed out anything below the greatest value it could once have had.
        | ThreadIdCounter.Linux _ -> id >= 1UL && id < uint64 linuxPidMaxCeiling
        | ThreadIdCounter.Darwin next -> id >= 1UL && id < next

    /// The ids of every live task on the machine, which `allocate` will not
    /// hand out.
    let internal live (allocator : ThreadIdAllocator) : Set<OsThreadId> = allocator.Live

    /// Hand out the next id, which is live from then on, skipping every id a
    /// live task holds; or EAGAIN if every id Linux would hand out is held; or
    /// nothing, if Darwin's counter has reached the top of its range.
    let internal allocate (allocator : ThreadIdAllocator) : ThreadIdAllocation =
        let held = allocator.Live

        let issued (id : OsThreadId) (counter : ThreadIdCounter) =
            ThreadIdAllocation.Issued (
                id,
                {
                    Counter = counter
                    Live = Set.add id held
                }
            )

        match allocator.Counter with
        | ThreadIdCounter.Linux (cursor, pidMax) ->
            let firstFree (low : int32) : int32 option =
                seq { low .. pidMax - 1 }
                |> Seq.tryFind (fun id -> not (Set.contains (OsThreadId (uint64 id)) held))

            // Measured on Linux 6.18.5 aarch64 by
            // `docs/plans/2026-08-23-posix-kernel-extraction/pid-allocation.c`: ids
            // go up from the last one handed out, never back to a freed one, until
            // they reach pid_max; the search then starts again at 300 and skips
            // every live id; and once every id from 300 up is live, thread
            // creation fails with EAGAIN.
            let found =
                match firstFree cursor with
                | Some id -> Some id
                | None ->
                    // The search from the cursor can only fail once the counter has
                    // passed 300: before then, every id it has handed out is below the
                    // cursor, and pid_max is above 300. Past it, Linux's search starts
                    // again at 300; below it, Linux would start at 1, which is
                    // unmeasured and which this counter cannot reach.
                    if cursor <= reservedPids then
                        failwith
                            $"ThreadIdAllocator.allocate: no free id from the cursor %d{cursor} up to pid_max %d{pidMax}, although the counter has not passed %d{reservedPids}. Every id it has handed out is below the cursor, so this is a bug in this library."

                    firstFree reservedPids

            match found with
            | Some id -> issued (OsThreadId (uint64 id)) (ThreadIdCounter.Linux (id + 1, pidMax))
            | None -> ThreadIdAllocation.Failed UnixError.EAGAIN
        | ThreadIdCounter.Darwin next ->
            // A 64-bit counter never wraps in practice, and what Darwin does if it
            // did has not been measured.
            if next = UInt64.MaxValue then
                ThreadIdAllocation.DarwinCounterExhausted
            else
                issued (OsThreadId next) (ThreadIdCounter.Darwin (next + 1UL))

    /// The task holding `id` has exited, so `id` is no longer live. Loudly
    /// partial on an id that is not live: every live task's id is.
    let internal release (id : OsThreadId) (allocator : ThreadIdAllocator) : ThreadIdAllocator =
        if not (Set.contains id allocator.Live) then
            failwith
                $"ThreadIdAllocator.release: %O{id} is not a live task's thread ID, so no task holding it can exit (this is a bug in this library)."

        { allocator with
            Live = Set.remove id allocator.Live
        }
