namespace WoofWare.PosixKernel

/// Why this library will not answer a task's thread exit: a case it does not
/// model, rather than an error a kernel would report, because the thread-exit
/// syscall cannot fail.
[<RequireQualifiedAccess>]
type ThreadExitRefusal<'Task> =
    /// The task is blocked in a syscall, so it cannot be making another one.
    | Parked of task : 'Task * park : TaskPark
    /// The task is the last one on Linux. There, the last task's exit ends the
    /// process, and this library does not yet represent a process that has ended.
    | LastTaskOnLinux of task : 'Task
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
        | ThreadExitRefusal.LastTaskOnLinux task ->
            $"task %O{task} is the process's last task, and on Linux its exit ends the process, which this library does not yet represent"
        | ThreadExitRefusal.LastTaskOnDarwin task ->
            $"task %O{task} is the process's last task, and what Darwin does when the last task makes the thread-exit syscall has not been measured"

/// How tasks leave a process.
[<RequireQualifiedAccess>]
module UnixTaskLifecycle =

    /// The thread-exit syscall: `task` ends, and the process carries on. This is
    /// `SYS_exit` on Linux, which is what `pthread_exit` and a thread's return
    /// from its start routine end in; it is not `exit(3)` or `exit_group(2)`.
    ///
    /// Removes `task` from the table, and with it everything the process holds for
    /// that task alone: its signal mask, and the signals pending on it alone, which
    /// are discarded rather than passed on to another task.
    ///
    /// Refuses a task that is parked in a syscall, and the process's last task,
    /// whose exit would end the process.
    ///
    /// The process's leader exiting while other tasks live is not refused yet: that
    /// refusal arrives with `UnixSystem.Leader`. Until then a leader that exits first
    /// is removed like any other task.
    ///
    /// Fails loudly if `task` names no task, which is a bug in the client.
    let exitThread<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixSystem<'Task, 'Handler>, ThreadExitRefusal<'Task>>
        =
        // No exit status is taken yet: only the last task's is ever read (the process
        // ends with it), and the last task's exit is refused below.
        let state = UnixTaskTable.get task system.Tasks

        match state.Parked with
        | Some park -> Error (ThreadExitRefusal.Parked (task, park))
        | None ->

        if system.Tasks.Count = 1 then
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> Error (ThreadExitRefusal.LastTaskOnLinux task)
            | SimulatedUnixFlavour.Darwin -> Error (ThreadExitRefusal.LastTaskOnDarwin task)
        else

        Ok
            { system with
                Process =
                    { system.Process with
                        Signals = SignalState.forgetTask task system.Process.Signals
                    }
                Tasks = Map.remove task system.Tasks
            }
