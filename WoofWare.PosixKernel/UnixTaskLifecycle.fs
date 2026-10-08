namespace WoofWare.PosixKernel

/// A process that has ended, and how.
///
/// Not a `UnixSystem`: it has no tasks, so nothing can make a syscall in it, and no
/// function that answers one accepts it.
type EndedProcess<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    {
        /// How the process ended, which is what its parent's `wait` reads.
        Termination : ProcessTermination
        /// The machine the process ran on, as the process's end left it: its
        /// tasks' thread IDs are no longer live, the holds its tasks' calls
        /// in flight had on open file descriptions are let go of, so is the
        /// hold its current directory had on its inode, and its process ID is
        /// no live process's (no `wait` is modelled, so nothing keeps it).
        ///
        /// Its descriptors are not closed, so every description they name is
        /// still counted as named, and a description no descriptor named,
        /// which only a call held, is still in the table. On a
        /// `SimulatedMachine`, `SimulatedMachine.endProcess` closes them, as a
        /// real kernel does at exit, where another process can see it.
        Machine : UnixMachineState
        /// The process's own state as it stood when it ended. It holds nothing for
        /// any task: no signal mask, and no signal pending on one task alone.
        FinalProcess : UnixProcessState<'Task, 'Handler>
        /// The process as the call that ended it found it, tasks and all: what
        /// `SimulatedMachine.endProcess` checks against the machine it ends the
        /// process on, as `SimulatedMachine.unfocus` checks a view, and reads
        /// the calls the process's tasks had in flight from.
        EndedIn : UnixSystem<'Task, 'Handler>
    }

/// What a syscall that can end the whole process did.
[<RequireQualifiedAccess>]
type TaskOutcome<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// The process carries on, as this system.
    | Continues of UnixSystem<'Task, 'Handler>
    /// The process has ended.
    | ProcessEnded of EndedProcess<'Task, 'Handler>

/// Why this library will not answer a task's thread exit: a case it does not
/// model, rather than an error a kernel would report, because the thread-exit
/// syscall cannot fail.
[<RequireQualifiedAccess>]
type ThreadExitRefusal<'Task> =
    /// The task is blocked in a syscall, so it cannot be making another one.
    | Parked of task : 'Task * park : TaskPark
    /// The task is the process's leader, and other tasks live. On both flavours
    /// the process carries on, with a leader that has gone (a zombie task on
    /// Linux); this library keeps the leader until the process ends.
    | LeaderBeforeOthers of task : 'Task
    /// The task is the last one on Darwin, where what its thread exit does has not
    /// been measured.
    | LastTaskOnDarwin of task : 'Task

[<RequireQualifiedAccess>]
module ThreadExitRefusal =

    /// A human-readable account of why the exit was refused.
    let describe<'Task> (refusal : ThreadExitRefusal<'Task>) : string =
        match refusal with
        | ThreadExitRefusal.Parked (task, park) ->
            $"task %O{task} is parked in %A{park.Syscall}, so it cannot be making the thread-exit syscall; whatever ended the thread skipped finishing or abandoning its park"
        | ThreadExitRefusal.LeaderBeforeOthers task ->
            $"task %O{task} is the process's leader, and other tasks are still running; this library does not model a process whose leader has exited before them"
        | ThreadExitRefusal.LastTaskOnDarwin task ->
            $"task %O{task} is the process's last task, and what Darwin does when the last task makes the thread-exit syscall has not been measured"

/// What `clone(2)` with `CLONE_THREAD` answered, for a request this library
/// could answer.
[<RequireQualifiedAccess>]
type SpawnAnswer =
    /// The new thread exists, and its task reports `threadId`.
    | Spawned of threadId : OsThreadId
    /// The call returned -1 with this errno. No task was created, and no thread
    /// ID was handed out.
    | Failed of error : UnixError

/// Why this library will not answer a `clone(2)` with `CLONE_THREAD`: a case it
/// does not model or has not measured, rather than an error a kernel would
/// report.
[<RequireQualifiedAccess>]
type SpawnRefusal<'Task> =
    /// Unmodelled. `parent` is running a signal handler, and `mask`, the mask in
    /// force while the handler runs, blocks at least one signal. A new thread
    /// starts with its creator's mask, but this library holds a mask only as a
    /// task's handler frames, and a new task has none, so it cannot give the new
    /// task that mask.
    | InheritedHandlerMask of parent : 'Task * mask : SignalMask
    /// Unmeasured. Darwin's 64-bit thread ID counter has reached the top of its
    /// range, and what Darwin does when a thread is created then has not been
    /// measured.
    | ThreadIdCounterExhausted

[<RequireQualifiedAccess>]
module SpawnRefusal =

    /// A human-readable account of why the thread's creation was refused.
    let describe<'Task> (refusal : SpawnRefusal<'Task>) : string =
        match refusal with
        | SpawnRefusal.InheritedHandlerMask (parent, mask) ->
            $"task %O{parent} creates a thread from inside a signal handler, whose mask (%O{mask}) the new thread would inherit; this library holds a mask only as a task's handler frames, so it cannot give the new thread one"
        | SpawnRefusal.ThreadIdCounterExhausted ->
            "Darwin's thread ID counter has reached the top of its 64-bit range, and what Darwin does when a thread is created then has not been measured"

/// How tasks join and leave a process, and how a process ends.
[<RequireQualifiedAccess>]
module UnixTaskLifecycle =

    /// End the process `system` is, as `termination` says: every task goes, and
    /// with each everything the process and the machine held for that task
    /// alone: its signal mask and the signals pending on it alone, its thread
    /// ID, and the holds its call in flight, if any, had. The machine lets go
    /// of the process's current directory and its process ID.
    let internal endProcess<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (termination : ProcessTermination)
        (system : UnixSystem<'Task, 'Handler>)
        : EndedProcess<'Task, 'Handler>
        =
        let signals =
            (system.Process.Signals, Map.keys system.Tasks)
            ||> Seq.fold (fun signals task -> SignalState.forgetTask task signals)

        let unparked =
            (system, Map.keys system.Tasks)
            ||> Seq.fold (fun system task -> UnixParkState.unpark task system)

        let threadIds =
            (unparked.Machine.ThreadIds, unparked.Tasks)
            ||> Map.fold (fun threadIds _ state -> ThreadIdAllocator.release state.OsThreadId threadIds)

        let cwd = system.Process.CurrentDirectoryInode

        // A directory the process had removed while standing in it is free once
        // nothing else stands in it.
        let released =
            { unparked with
                Machine =
                    { unparked.Machine with
                        ThreadIds = threadIds
                        ProcessIds = ProcessIdTable.remove system.Process.ProcessId unparked.Machine.ProcessIds
                    }
                    |> UnixMachineState.releaseCurrentDirectory cwd
            }
            |> ObjectLifetime.forgetIfUnheld cwd

        {
            Termination = termination
            Machine = released.Machine
            FinalProcess =
                { system.Process with
                    Signals = signals
                }
            EndedIn = system
        }

    /// The thread-exit syscall: `task` ends. This is `SYS_exit` on Linux, which is
    /// what `pthread_exit` and a thread's return from its start routine end in; it
    /// is not `exit(3)` or `exit_group(2)` (see `exitGroup`).
    ///
    /// Removes `task` from the table, and with it everything the process holds for
    /// that task alone: its signal mask, and the signals pending on it alone, which
    /// are discarded rather than passed on to another task. Its thread ID is no
    /// longer live, so the machine may hand it out again. The process carries on
    /// unless `task` was its last, in which case the process ends, on Linux, having
    /// exited with `status`. `status` is otherwise ignored.
    ///
    /// Refuses a task that is parked in a syscall, the process's leader while any
    /// other task lives, and the process's last task on Darwin.
    ///
    /// Fails loudly if `task` names no task, which is a bug in the client.
    let exitThread<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (status : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<TaskOutcome<'Task, 'Handler>, ThreadExitRefusal<'Task>>
        =
        let state = UnixTaskTable.get task system.Tasks

        match state.Parked with
        | Some park -> Error (ThreadExitRefusal.Parked (task, park))
        | None ->

        // Measured on Linux 6.18.5 and Darwin 27.0.0 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/leader-exits-first.c`: the
        // process carries on without its leader, which Linux keeps as a zombie
        // task. Nothing here models a process in that state.
        if task = system.Leader && system.Tasks.Count > 1 then
            Error (ThreadExitRefusal.LeaderBeforeOthers task)
        elif system.Tasks.Count = 1 then
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux ->
                // The last task's own status is the process's, whatever an earlier
                // task exited with, and it is kept as `exit_group`'s is: measured on
                // Linux 6.18.5 (aarch64 and x86-64) by
                // `docs/plans/2026-08-23-posix-kernel-extraction/last-thread-exit-status.c`,
                // with the leader last, with a worker last, and with one thread.
                let termination =
                    ProcessTermination.Exited (ExitStatus.ofExitArgument SimulatedUnixFlavour.Linux status)

                Ok (TaskOutcome.ProcessEnded (endProcess termination system))
            | SimulatedUnixFlavour.Darwin -> Error (ThreadExitRefusal.LastTaskOnDarwin task)
        else

        Ok (
            TaskOutcome.Continues
                { system with
                    Machine =
                        { system.Machine with
                            ThreadIds = ThreadIdAllocator.release state.OsThreadId system.Machine.ThreadIds
                        }
                    Process =
                        { system.Process with
                            Signals = SignalState.forgetTask task system.Process.Signals
                        }
                    Tasks = Map.remove task system.Tasks
                }
        )

    /// `clone(2)` with `CLONE_THREAD`, made by `parent`: a new thread in the process,
    /// the task `child`, running on the logical processor `cpu`. This is what
    /// `pthread_create(3)` ends in.
    ///
    /// Answers the thread ID the machine's allocator hands the new task, one no
    /// live task on the machine holds, which starts with a copy of `parent`'s
    /// signal mask and with no signal pending on it alone. On Linux, answers
    /// EAGAIN instead once every thread ID from 300 up to the machine's
    /// `pid_max` is in use, leaving the system as it was.
    ///
    /// Refuses a `parent` inside a signal handler whose mask blocks anything
    /// (`SpawnRefusal.InheritedHandlerMask`), and a spawn on Darwin once the
    /// machine's thread ID counter has reached the top of its range
    /// (`SpawnRefusal.ThreadIdCounterExhausted`).
    ///
    /// Fails loudly if `parent` names no task or is parked in a syscall, or if
    /// `child` already names a task: each is a bug in the client.
    let spawn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (parent : 'Task)
        (child : 'Task)
        (cpu : CpuId)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SpawnAnswer * UnixSystem<'Task, 'Handler>, SpawnRefusal<'Task>>
        =
        match (UnixTaskTable.get parent system.Tasks).Parked with
        | Some park ->
            failwith
                $"UnixTaskLifecycle.spawn: task %O{parent} is parked in %A{park.Syscall}, so it cannot be making the clone syscall"
        | None ->

        if Map.containsKey child system.Tasks then
            failwith
                $"UnixTaskLifecycle.spawn: %O{child} already names a task. A task is created once, and creating it again would silently discard whatever the first creation recorded (this is a bug in the client)."

        // Measured on Linux 6.18.5 (aarch64 and x86-64) and Darwin 27.0.0 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/thread-spawn-mask.c`: a new
        // thread's mask is exactly its creator's, and nothing pending on the
        // creator alone is pending on it. A task's mask is its handler frames'
        // here, and a new task has none, so it starts with the empty mask its
        // creator has when that has no frame.
        let mask = SignalState.maskOf parent system.Process.Signals

        if not (SignalMask.isEmpty mask) then
            Error (SpawnRefusal.InheritedHandlerMask (parent, mask))
        else

        match ThreadIdAllocator.allocate system.Machine.ThreadIds with
        | ThreadIdAllocation.Failed error -> Ok (SpawnAnswer.Failed error, system)
        | ThreadIdAllocation.DarwinCounterExhausted -> Error SpawnRefusal.ThreadIdCounterExhausted
        | ThreadIdAllocation.Issued (id, threadIds) ->

        Ok (
            SpawnAnswer.Spawned id,
            { system with
                Machine =
                    { system.Machine with
                        ThreadIds = threadIds
                    }
                Tasks = UnixTaskTable.add child cpu id system.Tasks
            }
        )

    /// `exit_group(2)`, made by `task`: every task ends at once, parked ones
    /// included, and the process exits with `status`. This is what `exit(3)` and
    /// `_exit(2)` end in; Darwin's `_exit(2)` is the same call under another name.
    ///
    /// Fails loudly if `task` names no task, or is parked in a syscall: a task
    /// blocked in one is not making another, so either is a bug in the client.
    let exitGroup<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (status : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : EndedProcess<'Task, 'Handler>
        =
        match (UnixTaskTable.get task system.Tasks).Parked with
        | Some park ->
            failwith
                $"UnixTaskLifecycle.exitGroup: task %O{task} is parked in %A{park.Syscall}, so it cannot be making the exit_group syscall"
        | None ->

        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
        endProcess (ProcessTermination.Exited (ExitStatus.ofExitArgument flavour status)) system
