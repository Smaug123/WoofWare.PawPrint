namespace WoofWare.PosixKernel

/// Where a process's view of a `SimulatedMachine` was taken from: the machine
/// and the process's own state as they stood, which `SimulatedMachine.unfocus`
/// requires to be the machine's still, so that a view whose copy of the
/// machine is stale, or of another history of it, is refused rather than
/// written back over a change it never saw.
///
/// Compared by identity rather than by content, both by `unfocus` and as part
/// of a `UnixSystem`'s equality: two views are views of the same state when
/// they were taken from the same values. A system no machine has focused is
/// `NotFocused`, so equality between such systems is unaffected.
[<CustomEquality ; NoComparison>]
type internal FocusOrigin<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    | NotFocused
    | FocusedFrom of
        machine : UnixMachineState *
        proc : UnixProcessState<'Task, 'Handler> *
        tasks : Map<'Task, UnixTaskState>

    override this.Equals (other : obj) : bool =
        match other with
        | :? FocusOrigin<'Task, 'Handler> as other ->
            match this, other with
            | NotFocused, NotFocused -> true
            | FocusedFrom (machine, proc, tasks), FocusedFrom (machine', proc', tasks') ->
                obj.ReferenceEquals (machine, machine')
                && obj.ReferenceEquals (proc, proc')
                && obj.ReferenceEquals (tasks, tasks')
            | NotFocused, FocusedFrom _
            | FocusedFrom _, NotFocused -> false
        | _ -> false

    override this.GetHashCode () : int =
        match this with
        | NotFocused -> 0
        | FocusedFrom _ -> 1

/// Everything one simulated POSIX process is, as a syscall sees it: the machine
/// it runs on, its own per-process state, and its tasks.
///
/// On a `SimulatedMachine` holding several processes, this is one process's
/// view of it (`SimulatedMachine.focus`): it holds no other process, so no
/// syscall can reach one.
///
/// Generic in what names a task and what a signal handler is, for the same
/// reason `SignalState` is: those are the client's identities and this library
/// never learns them.
///
/// Opaque: a client reads it through the queries in the `UnixSystem` module
/// and changes it only through syscalls, `UnixSystem.advanceClock` and
/// `UnixSystem.writePidMaxSysctl`.
type UnixSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            Machine : UnixMachineState
            Process : UnixProcessState<'Task, 'Handler>
            Tasks : Map<'Task, UnixTaskState>
            /// The process's first task, which it started with: its thread-group
            /// leader.
            ///
            /// Always in `Tasks`. It cannot exit while another task lives
            /// (`UnixTaskLifecycle.exitThread` refuses that), so it is a task for as
            /// long as the process is running. On Linux its thread ID is the process ID.
            Leader : 'Task
            /// Where this view was taken from, if a `SimulatedMachine` focused
            /// it; see `FocusOrigin`.
            Origin : FocusOrigin<'Task, 'Handler>
        }

/// Reading and writing a `UnixSystem`'s process's descriptor table together
/// with the machine's open file descriptions it names.
[<RequireQualifiedAccess>]
module internal UnixSystemState =
    /// Whether the process's descriptor table is every descriptor on the
    /// machine: whether no other process is on it.
    let census<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : DescriptorCensus
        =
        let others =
            Set.remove system.Process.ProcessId (ProcessIdTable.live system.Machine.ProcessIds)

        if Set.isEmpty others then
            DescriptorCensus.Complete
        else
            DescriptorCensus.OneProcessOf system.Process.ProcessId

    /// The process's descriptor table, read against the machine's open file
    /// descriptions.
    let fileDescriptors<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : FileDescriptorRegistry
        =
        FileDescriptorRegistry.ofTables (census system) system.Process.FileDescriptors system.Machine.OpenFiles

    /// `system` with `registry`'s descriptor table as the process's, and its open
    /// file descriptions as the machine's.
    let withFileDescriptors<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine =
                { system.Machine with
                    OpenFiles = FileDescriptorRegistry.openFiles registry
                }
            Process =
                { system.Process with
                    FileDescriptors = FileDescriptorRegistry.descriptorTable registry
                }
        }

    /// `system` with its machine's open file descriptions rewritten by `f`.
    let mapOpenFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (f : OpenFileTable -> OpenFileTable)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine =
                { system.Machine with
                    OpenFiles = f system.Machine.OpenFiles
                }
        }

/// Writing a task's park, together with the holds it takes on open file
/// descriptions.
[<RequireQualifiedAccess>]
module internal UnixParkState =
    /// The machine's open file table with the holds of `released` let go of and
    /// those of `taken` taken: one per time a park names a description
    /// (`ParkedSyscall.descriptions`).
    let private moveHolds
        (released : TaskPark option)
        (taken : TaskPark option)
        (openFiles : OpenFileTable)
        : OpenFileTable
        =
        let descriptions (park : TaskPark option) : OpenFileDescriptionId list =
            match park with
            | None -> []
            | Some park -> ParkedSyscall.descriptions park.Syscall

        let openFiles =
            (openFiles, descriptions released)
            ||> List.fold (fun openFiles id -> OpenFileTable.releaseHold id openFiles)

        (openFiles, descriptions taken)
        ||> List.fold (fun openFiles id -> OpenFileTable.hold id openFiles)

    /// `system` with `task` parked in `park`, which replaces any park it was in,
    /// and with the holds the machine records moved to match: the old park's
    /// let go of, and the new park's taken. Every write of a park goes through
    /// here or `unpark`, so the holds the open file table records are always
    /// those the parks name.
    ///
    /// Refuses to replace a park of one syscall with a park of another, as
    /// `UnixTaskTable.withPark` does. Writes `park` as it stands, ordinal and
    /// all: `UnixWait.park` is what mints a fresh ordinal.
    let setPark<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (park : TaskPark)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let previous = UnixTaskTable.parkOf task system.Tasks

        { system with
            Machine =
                { system.Machine with
                    OpenFiles = moveHolds previous (Some park) system.Machine.OpenFiles
                }
            Tasks = UnixTaskTable.withPark task park system.Tasks
        }

    /// `system` with `task` no longer parked, its call having returned or been
    /// ended, and the holds its park took let go of. Destroys nothing: a
    /// description only the park held stays in the table until
    /// `ObjectLifetime.releaseUnreferenced` is asked about it.
    let unpark<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let previous = UnixTaskTable.parkOf task system.Tasks

        { system with
            Machine =
                { system.Machine with
                    OpenFiles = moveHolds previous None system.Machine.OpenFiles
                }
            Tasks = UnixTaskTable.unpark task system.Tasks
        }

/// A simulated machine before it has booted, with the ID its first process
/// will have: what `UnixSystem.initial` makes, which the setters in
/// `UnixBootImage` configure and `UnixBootImage.boot` turns, by launching that
/// first process (`ProcessLaunch`), into the `UnixSystem` that syscalls take.
///
/// Generic in the task and handler types of the system it boots into.
///
/// Opaque, so that the only way to a system from it is `boot`, after which no
/// setter applies.
type UnixBootImage<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            Machine : UnixMachineState
            /// The ID the first process gets. The machine's thread ID allocator
            /// already holds its leader's thread ID as its one live ID.
            ProcessId : ProcessId
        }

/// What the entry point returns, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type SyscallAnswer =
    /// The entry point returns this.
    | Completed of answer : int64
    /// The entry point returns its failure sentinel, and the client stores
    /// `error` wherever its libc keeps errno.
    ///
    /// A failure still changes the system in general: `flock` advances the
    /// descriptor table before it can discover the conflict that fails it.
    | Failed of error : UnixError
