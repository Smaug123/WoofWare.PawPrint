namespace WoofWare.PosixKernel

/// One process on a `SimulatedMachine`: everything a view of the machine from
/// that process (`UnixSystem`) holds besides the machine itself.
type ProcessSlot<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            Process : UnixProcessState<'Task, 'Handler>
            /// The process's tasks. A task is named by the client, separately in
            /// each process: two processes may each have a task the client calls
            /// by the same name.
            Tasks : Map<'Task, UnixTaskState>
            /// See `UnixSystem.Leader`.
            Leader : 'Task
        }

/// Several simulated processes on one machine.
///
/// A syscall is made in one process's view of the machine, a `UnixSystem`,
/// which `focus` takes and `unfocus` writes back. The view holds the machine
/// and that one process, and no other, so no syscall can read or change
/// another process; everything one process's syscall can do to another goes
/// through the machine they share.
///
/// Opaque: a client reads it through the queries in the `SimulatedMachine`
/// module and through the views it focuses.
type SimulatedMachine<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            Machine : UnixMachineState
            /// Every process on the machine, by its process ID.
            Processes : Map<ProcessId, ProcessSlot<'Task, 'Handler>>
        }

/// A way a `SimulatedMachine` fails to be a machine any kernel could be in.
/// `SimulatedMachine.checkInvariants` returns these.
[<RequireQualifiedAccess>]
type SimulatedMachineDefect<'Task> =
    /// One of `UnixSystem.checkMachineInvariants`'s defects, read against every
    /// process on the machine.
    | Machine of UnixSystemDefect<'Task>
    /// One of `UnixSystem.checkViewInvariants`'s defects, in the view of the
    /// process `processId`.
    | View of processId : ProcessId * defect : UnixSystemDefect<'Task>
    /// One of `FileDescriptorRegistry.checkDescriptorTableInvariants`'s
    /// defects, in the descriptor table of the process `processId`.
    | DescriptorTable of processId : ProcessId * defect : FileDescriptorRegistryDefect
    /// One of `OpenFileTable.checkInvariants`'s defects, read against every
    /// process's descriptor table: among them, a description whose count of
    /// the descriptors naming it is not the number of descriptors in every
    /// process that do.
    | OpenFiles of defect : FileDescriptorRegistryDefect
    /// The process held under `key` has the process ID `recorded`. A view is
    /// focused by the key and written back by the process ID it records, so
    /// the two must agree.
    | SlotUnderAnotherProcessId of key : ProcessId * recorded : ProcessId
    /// More than one process has the process ID `processId`.
    | DuplicateProcessId of processId : ProcessId

/// Why `SimulatedMachine.launch` will not start a process.
[<RequireQualifiedAccess>]
type ProcessCreationRefusal =
    /// The launch itself is refused; see `LaunchRefusal`.
    | Launch of LaunchRefusal
    /// On Linux, every ID the machine would hand out, from 300 up to its
    /// `pid_max`, is a live thread's, so `fork(2)` would answer EAGAIN.
    | NoFreeProcessId
    /// On Darwin, the process ID counter has reached `pidMax`
    /// (`ProcessIdTable.darwinPidMax`). xnu wraps it back to 100, skipping IDs
    /// in use; that has not been measured, so this library does not wrap.
    | DarwinPidMaxReached of pidMax : int32

[<RequireQualifiedAccess>]
module ProcessCreationRefusal =
    /// What this library knows about why it would not start the process, for
    /// a client composing a diagnostic that names its own knob.
    let describe (refusal : ProcessCreationRefusal) : string =
        match refusal with
        | ProcessCreationRefusal.Launch refusal -> LaunchRefusal.describe refusal
        | ProcessCreationRefusal.NoFreeProcessId ->
            "every process ID the machine would hand out is a live thread's, so fork(2) would answer EAGAIN."
        | ProcessCreationRefusal.DarwinPidMaxReached pidMax ->
            $"the machine's process ID counter has reached Darwin's PID_MAX %d{pidMax}; what Darwin does next (xnu's source wraps to 100) has not been measured."

/// Why `SimulatedMachine.endProcess` will not end a process: what closing
/// its descriptors would do has not been measured.
[<RequireQualifiedAccess>]
type ProcessEndRefusal =
    /// Closing the process's descriptors releases an open file description
    /// this library will not release (`DescriptionReleaseRefusal`): so far,
    /// the last reference to a listener holding a connection another
    /// process's open socket made, which a real kernel resets.
    | Release of DescriptionReleaseRefusal

[<RequireQualifiedAccess>]
module ProcessEndRefusal =
    /// What this library knows about why it will not end the process, for a
    /// client composing a diagnostic that names its own knob.
    let describe (refusal : ProcessEndRefusal) : string =
        match refusal with
        | ProcessEndRefusal.Release refusal ->
            $"the process's end closes its descriptors, and %s{DescriptionReleaseRefusal.describe refusal}"

/// Moving between a `SimulatedMachine` and the views of it its processes'
/// syscalls are made in.
[<RequireQualifiedAccess>]
module SimulatedMachine =

    let private viewOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (machine : SimulatedMachine<'Task, 'Handler>)
        (slot : ProcessSlot<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        {
            Machine = machine.Machine
            Process = slot.Process
            Tasks = slot.Tasks
            Leader = slot.Leader
            Origin = FocusOrigin.FocusedFrom (machine.Machine, slot.Process, slot.Tasks)
        }

    /// The machine `system` runs on, holding `system`'s process as its only
    /// one. `system` is itself a view of the result, which `unfocus` accepts
    /// while it is unchanged.
    ///
    /// Fails loudly if `system` is a view of a machine holding other processes
    /// besides, which a machine without them would not be one any kernel could
    /// be in.
    let ofSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SimulatedMachine<'Task, 'Handler>
        =
        let others =
            Set.remove system.Process.ProcessId (ProcessIdTable.live system.Machine.ProcessIds)

        if not others.IsEmpty then
            failwith
                $"SimulatedMachine.ofSystem: the machine holds the processes %A{Set.toList others} besides process %O{system.Process.ProcessId}, so the system is a view of a machine holding other processes besides."

        {
            Machine = system.Machine
            Processes =
                Map.empty
                |> Map.add
                    system.Process.ProcessId
                    {
                        Process = system.Process
                        Tasks = system.Tasks
                        Leader = system.Leader
                    }
        }

    /// Start a new process on the machine, as `launch` describes it: the
    /// process ID the kernel chose for it, and the machine with it on.
    ///
    /// On Linux the process's ID is its leader's thread ID, the next the
    /// machine's thread ID counter hands out, so it is no live process's or
    /// thread's. On Darwin it is the next from a process ID counter of its own,
    /// which started one past the first process's ID, and the leader's thread
    /// ID is the next from the 64-bit thread ID counter.
    ///
    /// Refuses a launch `LaunchRefusal` describes, and a machine with no
    /// process ID left to hand out.
    let launch<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (launch : ProcessLaunch<'Task>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : Result<ProcessId * SimulatedMachine<'Task, 'Handler>, ProcessCreationRefusal>
        =
        let current = machine.Machine

        let allocated =
            match current.ProcessIds.Counter with
            | ProcessIdCounter.ThreadIds ->
                match ThreadIdAllocator.allocate current.ThreadIds with
                | Error _ -> Error ProcessCreationRefusal.NoFreeProcessId
                | Ok (leader, threadIds) ->
                    let pid =
                        ProcessId.parseOrFail "SimulatedMachine.launch" (int32 (OsThreadId.toUInt64 leader))

                    Ok (
                        pid,
                        leader,
                        { current with
                            ThreadIds = threadIds
                        }
                    )
            | ProcessIdCounter.Darwin _ ->
                match ProcessIdTable.nextDarwin current.ProcessIds with
                | None -> Error (ProcessCreationRefusal.DarwinPidMaxReached ProcessIdTable.darwinPidMax)
                | Some (pid, processIds) ->
                    match ThreadIdAllocator.allocate current.ThreadIds with
                    | Error error ->
                        failwith
                            $"SimulatedMachine.launch: Darwin's thread ID counter answered %O{error}, which it never does (this is a bug in this library)."
                    | Ok (leader, threadIds) ->
                        Ok (
                            pid,
                            leader,
                            { current with
                                ThreadIds = threadIds
                                ProcessIds = processIds
                            }
                        )

        match allocated with
        | Error refusal -> Error refusal
        | Ok (pid, leaderThreadId, allocatedMachine) ->

        if Map.containsKey pid machine.Processes then
            failwith
                $"SimulatedMachine.launch: the kernel chose process ID %O{pid}, which a process on the machine already has (this is a bug in this library)."

        match ProcessLaunch.launchOnto launch pid leaderThreadId allocatedMachine with
        | Error refusal -> Error (ProcessCreationRefusal.Launch refusal)
        | Ok (launched, proc, tasks) ->
            Ok (
                pid,
                {
                    Machine = launched
                    Processes =
                        Map.add
                            pid
                            {
                                Process = proc
                                Tasks = tasks
                                Leader = ProcessLaunch.leader launch
                            }
                            machine.Processes
                }
            )

    /// The process ID of every process on the machine.
    let processIds<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (machine : SimulatedMachine<'Task, 'Handler>)
        : Set<ProcessId>
        =
        machine.Processes |> Map.keys |> Set.ofSeq

    /// The view of the machine from the process `processId`, in which that
    /// process's syscalls are made; or `None` if no process on the machine has
    /// that ID.
    ///
    /// The view holds a copy of the machine. After a syscall changes it, the
    /// change reaches the machine only through `unfocus`, which refuses the
    /// view once any other change has been written back: focus again to make
    /// another call after that.
    let focus<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (processId : ProcessId)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler> option
        =
        Map.tryFind processId machine.Processes |> Option.map (viewOf machine)

    /// Fail loudly, naming `operation`, unless `view` is a view of `machine`
    /// as it stands; see `unfocus`.
    let private assertCurrent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (operation : string)
        (view : UnixSystem<'Task, 'Handler>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : unit
        =
        let processId = view.Process.ProcessId

        if not (Map.containsKey processId machine.Processes) then
            failwith
                $"%s{operation}: no process on the machine has ID %O{processId}, so the view is not of this machine (this is a bug in the client)."

        let slot = machine.Processes.[processId]

        let holds
            (machineState : UnixMachineState)
            (proc : UnixProcessState<'Task, 'Handler>)
            (tasks : Map<'Task, UnixTaskState>)
            : bool
            =
            obj.ReferenceEquals (machineState, machine.Machine)
            && obj.ReferenceEquals (proc, slot.Process)
            && obj.ReferenceEquals (tasks, slot.Tasks)

        // A view the machine already holds exactly, wherever it was focused
        // from, writes nothing back; as the machine `ofSystem` made of a
        // system holds that system.
        let unchanged = holds view.Machine view.Process view.Tasks

        let current =
            match view.Origin with
            | FocusOrigin.FocusedFrom (originMachine, originProcess, originTasks) ->
                unchanged || holds originMachine originProcess originTasks
            | FocusOrigin.NotFocused -> unchanged

        if not current then
            failwith
                $"%s{operation}: the view of process %O{processId} was not focused from the machine as it stands: another view has been written back since, or the machine has otherwise changed, or the view was focused from another history of it. Writing the view back would undo a change it never saw; focus the process again and repeat its call (this is a bug in the client)."

    /// Write `view` back into `machine`: the view's machine replaces
    /// `machine`'s, and the view's process replaces the process with its ID.
    ///
    /// Fails loudly if `view` is not a view of `machine` as it stands: if no
    /// process on it has the view's process ID, or if the machine or the
    /// view's own process is not the one the view was focused from (another
    /// view was written back since, the machine changed some other way, or the
    /// view was focused from another history of the machine), so that writing
    /// the view back would undo a change it never saw. Each is a bug in the
    /// client. A view the machine already holds exactly is accepted wherever
    /// it came from, since writing it back changes nothing: so `ofSystem` of a
    /// system accepts that system unchanged.
    ///
    /// What a syscall that ends the process answers is an `EndedProcess`,
    /// which is not a view and so cannot be written back: `endProcess` ends
    /// the process on the machine instead.
    let unfocus<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (view : UnixSystem<'Task, 'Handler>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : SimulatedMachine<'Task, 'Handler>
        =
        assertCurrent "SimulatedMachine.unfocus" view machine
        let processId = view.Process.ProcessId

        {
            Machine = view.Machine
            Processes =
                Map.add
                    processId
                    {
                        Process = view.Process
                        Tasks = view.Tasks
                        Leader = view.Leader
                    }
                    machine.Processes
        }

    /// Run `f` in the view of the machine from the process `processId`, and
    /// write the view it returns back: `focus`, then `unfocus`. `None` if no
    /// process on the machine has that ID.
    let inView<'Task, 'Handler, 'Result when 'Task : comparison and 'Handler : equality>
        (processId : ProcessId)
        (f : UnixSystem<'Task, 'Handler> -> 'Result * UnixSystem<'Task, 'Handler>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : ('Result * SimulatedMachine<'Task, 'Handler>) option
        =
        focus processId machine
        |> Option.map (fun view ->
            let result, view = f view
            result, unfocus view machine
        )

    /// `UnixSystem.step` of `call`, made by `task` of the process `processId`.
    /// A refused call changes nothing, and answers `machine` itself, so a view
    /// focused from it can still be written back. `None` if no process on the
    /// machine has that ID.
    let step<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (processId : ProcessId)
        (task : 'Task)
        (call : Syscall)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : (Result<SyscallOutcome, SyscallRefusal<'Task>> * SimulatedMachine<'Task, 'Handler>) option
        =
        focus processId machine
        |> Option.map (fun view ->
            match UnixSystem.step task call view with
            | Ok (outcome, view) -> Ok outcome, unfocus view machine
            | Error refusal -> Error refusal, machine
        )

    /// End the process `ended` is on `machine`, as the call that ended it left
    /// it (`UnixTaskLifecycle.exitGroup`, the last thread's exit, or a signal
    /// whose default ends the process): how it ended, and the machine without
    /// it.
    ///
    /// What a real kernel does at exit, where another process can see it.
    /// The process's tasks are gone already (`EndedProcess.Machine`), and with
    /// them the holds their calls in flight had; any description only those
    /// calls held, or that the call which ended the process let go of as it
    /// returned, is released now. Then every descriptor of the process is
    /// closed, in the order each flavour measurably closes them
    /// (`exit-close-order.c`, Linux 6.18.5 and Darwin 27.0.0, which a peer
    /// watching several connections sees as the order of their FINs): Linux
    /// drops the descriptors lowest first and then releases what that let go
    /// of, the last first; Darwin drops them highest first, releasing each as
    /// it goes. Each release does everything a `close(2)` of the last
    /// descriptor onto the description does: a connected peer gets its FIN,
    /// a lock is let go of, a pipe end closes, an epoll instance, a kqueue or a
    /// listener goes, and every registration made through the descriptor with
    /// it. A listener's release signals nothing, so it is made after every
    /// other, whether a descriptor or a call held it last: a connection the
    /// process's own socket left unaccepted in it has gone first, as it goes
    /// with the process. The process ID, thread IDs and
    /// current directory are let go of with the tasks.
    ///
    /// Refuses (`ProcessEndRefusal`) where a close would be refused: a listener
    /// holding a connection from another process's socket that is still open,
    /// which a real kernel resets, is not released. The machine is then as it
    /// was.
    ///
    /// Fails loudly, as `unfocus` does, if the view the process ended in
    /// (`EndedProcess.EndedIn`) is not a view of `machine` as it stands.
    let endProcess<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (ended : EndedProcess<'Task, 'Handler>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : Result<ProcessTermination * SimulatedMachine<'Task, 'Handler>, ProcessEndRefusal>
        =
        let view = ended.EndedIn
        assertCurrent "SimulatedMachine.endProcess" view machine
        let processId = view.Process.ProcessId

        // The process with no task left. Releasing a description reads and
        // writes the machine alone, and dropping a descriptor the process's
        // table.
        let dead =
            { view with
                Machine = ended.Machine
                Process = ended.FinalProcess
                Tasks = Map.empty
                Origin = FocusOrigin.NotFocused
            }

        let isListener (description : OpenFileDescription) (machine : UnixMachineState) : bool =
            match description.Target with
            | OpenFileTarget.Socket socketId ->
                match (UnixMachineState.socket socketId machine).Phase with
                | SocketPhase.Listening _ -> true
                | SocketPhase.Idle
                | SocketPhase.Established _
                | SocketPhase.EstablishedPendingReport _
                | SocketPhase.Refused _
                | SocketPhase.DatagramPeer _ -> false
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Epoll _
            | OpenFileTarget.Kqueue _ -> false

        // A listener's release signals nothing, so it waits for every other
        // release; any other goes at once.
        let release
            (state : Result<UnixSystem<'Task, 'Handler> * OpenFileDescription list, DescriptionReleaseRefusal>)
            (destroyed : OpenFileDescription)
            : Result<UnixSystem<'Task, 'Handler> * OpenFileDescription list, DescriptionReleaseRefusal>
            =
            match state with
            | Error refusal -> Error refusal
            | Ok (dead, listeners) ->
                if isListener destroyed dead.Machine then
                    Ok (dead, destroyed :: listeners)
                else
                    ObjectLifetime.releaseDestroyed destroyed dead
                    |> Result.map (fun dead -> dead, listeners)

        // First what nothing references any more: what the process's calls in
        // flight held, whose holds went with its tasks, and what the call that
        // ended the process let go of as it returned. These go as the calls
        // return when their tasks die, before the process's files are closed;
        // the order between the two has not been measured. On a machine no
        // process's end has left so, no description is unreferenced.
        let unreferenced =
            OpenFileTable.descriptions dead.Machine.OpenFiles
            |> Map.keys
            |> Seq.filter (fun id ->
                OpenFileTable.descriptorCount id dead.Machine.OpenFiles = Some 0
                && OpenFileTable.holdCount id dead.Machine.OpenFiles = Some 0
            )
            |> Seq.toList

        let released =
            ((Ok (dead, []) : Result<UnixSystem<'Task, 'Handler> * OpenFileDescription list, DescriptionReleaseRefusal>),
             unreferenced)
            ||> List.fold (fun state id ->
                match state with
                | Error refusal -> Error refusal
                | Ok (dead, listeners) ->
                    match OpenFileTable.destroyIfUnreferenced id dead.Machine.OpenFiles with
                    | _, None ->
                        failwith
                            $"SimulatedMachine.endProcess: open file description %O{id} was unreferenced a moment ago, and destroying it destroyed nothing (this is a bug in this library)."
                    | openFiles, Some destroyed ->
                        release (Ok (UnixSystemState.mapOpenFiles (fun _ -> openFiles) dead, listeners)) destroyed
            )

        // Then every descriptor. Measured on Linux 6.18.5 and Darwin 27.0.0
        // (`exit-close-order.c`, sections O and D): Linux drops them lowest
        // first and releases each description the drop let go of only after
        // every drop, the last let go of first, as its deferred final `fput`s
        // run; Darwin drops them highest first, releasing each as it goes.
        let drop
            (fd : int)
            (dead : UnixSystem<'Task, 'Handler>)
            : UnixSystem<'Task, 'Handler> * OpenFileDescription option
            =
            match FileDescriptorRegistry.dropDescriptor processId fd (UnixSystemState.fileDescriptors dead) with
            | Error FileDescriptorCloseError.BadFd ->
                failwith
                    $"SimulatedMachine.endProcess: process %O{processId}'s table held descriptor %d{fd} a moment ago, and closing it answered EBADF (this is a bug in this library)."
            | Ok (registry, destroyed) -> UnixSystemState.withFileDescriptors registry dead, destroyed

        let closed =
            match released with
            | Error refusal -> Error refusal
            | Ok (dead, listeners) ->
                let ascending =
                    FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors dead)
                    |> Map.keys
                    |> Seq.sort
                    |> Seq.toList

                match SimulatedUnixPlatform.flavour dead.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux ->
                    // Newest first.
                    let dead, destroyed =
                        ((dead, []), ascending)
                        ||> List.fold (fun (dead, destroyed) fd ->
                            match drop fd dead with
                            | dead, None -> dead, destroyed
                            | dead, Some description -> dead, description :: destroyed
                        )

                    (Ok (dead, listeners), destroyed) ||> List.fold release
                | SimulatedUnixFlavour.Darwin ->
                    (Ok (dead, listeners), List.rev ascending)
                    ||> List.fold (fun state fd ->
                        match state with
                        | Error refusal -> Error refusal
                        | Ok (dead, listeners) ->

                        match drop fd dead with
                        | dead, None -> Ok (dead, listeners)
                        | dead, Some destroyed -> release (Ok (dead, listeners)) destroyed
                    )

        // Then the listeners, in the order they were let go of.
        let finished =
            match closed with
            | Error refusal -> Error refusal
            | Ok (dead, listeners) ->
                (Ok dead, List.rev listeners)
                ||> List.fold (fun state listener ->
                    match state with
                    | Error refusal -> Error refusal
                    | Ok dead -> ObjectLifetime.releaseDestroyed listener dead
                )

        match finished with
        | Error refusal -> Error (ProcessEndRefusal.Release refusal)
        | Ok dead ->
            Ok (
                ended.Termination,
                {
                    Machine = dead.Machine
                    Processes = Map.remove processId machine.Processes
                }
            )

    /// The tasks `asleep` names that the machine wakes now, each with its
    /// process and the primitives of its wake condition which hold, in the
    /// order they parked: `UnixWait.wakes` of every process at once.
    ///
    /// `asleep` holds, for each process, the tasks of it the client is holding
    /// asleep in a syscall. A parked task it leaves out, in a process it names
    /// or one it does not, is one the client has woken and whose call has not
    /// yet finished.
    ///
    /// Each task's condition is asked of its own process's view, so a
    /// condition another process's call has made true (a connection queued on
    /// a listener, a peer's FIN, a lock let go of) wakes it here. Where a
    /// kernel wakes one waiter of a queue at a time (`UnixWait.wakes` says
    /// which queues, and which waiter), the choice is made across every
    /// process, by the order of the machine's parks, which every process's
    /// parks share. A woken task's call is finished in its own process's view
    /// (`inView`), by the finishing call of the syscall it is asleep in.
    ///
    /// Fails loudly if `asleep` names a process the machine does not hold.
    let wakes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (asleep : Map<ProcessId, Set<'Task>>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : ((ProcessId * 'Task) * Set<WakePrimitive>) list
        =
        for KeyValue (processId, _) in asleep do
            if not (Map.containsKey processId machine.Processes) then
                failwith
                    $"SimulatedMachine.wakes: no process on the machine has ID %O{processId}, but the client holds tasks of it asleep (this is a bug in the client)."

        let views =
            machine.Processes
            |> Map.toList
            |> List.map (fun (processId, slot) ->
                processId, Map.tryFind processId asleep |> Option.defaultValue Set.empty, viewOf machine slot
            )

        UnixWait.wakesAmong views machine.Machine
        |> List.map (fun (processId, task, fired) -> (processId, task), fired)

    /// Every way `machine` fails to be a machine any kernel could be in: the
    /// machine's clauses (`UnixSystem.checkMachineInvariants`) read against
    /// every process on it; each process's view's clauses
    /// (`UnixSystem.checkViewInvariants`) and its descriptor table's
    /// (`FileDescriptorRegistry.checkDescriptorTableInvariants`); the open
    /// file table's (`OpenFileTable.checkInvariants`) read against every
    /// process's descriptor table; and the clauses relating processes to one
    /// another: each is held under its own process ID, and no two have one ID.
    ///
    /// The filesystem's own rules are `VirtualFileSystem.checkInvariants`'s,
    /// and are not repeated here.
    let checkInvariants<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (machine : SimulatedMachine<'Task, 'Handler>)
        : SimulatedMachineDefect<'Task> list
        =
        let slots = Map.toList machine.Processes

        let machineDefects =
            UnixSystem.machineDefects
                true
                (slots |> List.map (fun (_, slot) -> slot.Process, slot.Tasks))
                machine.Machine
            |> List.map SimulatedMachineDefect.Machine

        let viewDefects =
            slots
            |> List.collect (fun (processId, slot) ->
                UnixSystem.checkViewInvariants (viewOf machine slot)
                |> List.map (fun defect -> SimulatedMachineDefect.View (processId, defect))
            )

        let tableDefects =
            slots
            |> List.collect (fun (processId, slot) ->
                FileDescriptorRegistry.ofTables
                    (DescriptorCensus.OneProcessOf processId)
                    slot.Process.FileDescriptors
                    machine.Machine.OpenFiles
                |> FileDescriptorRegistry.checkDescriptorTableInvariants slot.Process.ProcessId
                |> List.map (fun defect -> SimulatedMachineDefect.DescriptorTable (processId, defect))
            )

        let openFileDefects =
            OpenFileTable.checkInvariants
                (slots |> List.map (fun (_, slot) -> slot.Process.FileDescriptors))
                machine.Machine.OpenFiles
            |> List.map SimulatedMachineDefect.OpenFiles

        let misplaced =
            slots
            |> List.choose (fun (key, slot) ->
                if slot.Process.ProcessId = key then
                    None
                else
                    Some (SimulatedMachineDefect.SlotUnderAnotherProcessId (key, slot.Process.ProcessId))
            )

        let duplicates =
            slots
            |> List.countBy (fun (_, slot) -> slot.Process.ProcessId)
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fst >> SimulatedMachineDefect.DuplicateProcessId)

        machineDefects
        @ viewDefects
        @ tableDefects
        @ openFileDefects
        @ misplaced
        @ duplicates
