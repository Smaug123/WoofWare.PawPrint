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
            /// Advanced by every change to the machine, so that `unfocus` can
            /// refuse a view taken before one.
            Generation : MachineGeneration
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
    /// A kqueue is owned (`KqueueState.Owner`) by `owner`, which is no process
    /// on the machine. Its registrations name descriptors in that process's
    /// table, which no longer exists.
    | KqueueOwnerNotOnMachine of kqueue : OpenFileDescriptionId * owner : ProcessId

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
            Generation = machine.Generation
        }

    /// The machine `system` runs on, holding `system`'s process as its only
    /// one. `system` is itself a view of the result, which `unfocus` accepts.
    ///
    /// Fails loudly if `system` is a view of a machine holding other processes
    /// besides: the machine records a thread ID as live that none of `system`'s
    /// tasks holds, so some other process's task holds it, and a machine
    /// without that process would not be one any kernel could be in.
    let ofSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SimulatedMachine<'Task, 'Handler>
        =
        let held =
            system.Tasks
            |> Map.toSeq
            |> Seq.map (fun (_, state) -> state.OsThreadId)
            |> Set.ofSeq

        let live = ThreadIdAllocator.live system.Machine.ThreadIds

        if live <> held then
            failwith
                $"SimulatedMachine.ofSystem: the machine records the thread IDs %A{Set.toList (Set.difference live held)} as live, which none of process %O{system.Process.ProcessId}'s tasks holds, so the system is a view of a machine holding other processes besides."

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
            Generation = system.Generation
        }

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

    /// Write `view` back into `machine`: the view's machine replaces
    /// `machine`'s, and the view's process replaces the process with its ID.
    ///
    /// Fails loudly if `view` is not a view of `machine` as it stands: if no
    /// process on it has the view's process ID, or if a change has been
    /// written back since the view was focused (`unfocus` of another view, or
    /// another change to the machine), whose effects writing this view's copy
    /// of the machine back would undo. Each is a bug in the client.
    ///
    /// A process cannot end on a `SimulatedMachine`: what a syscall that ends
    /// one answers is an `EndedProcess`, which is not a view and so cannot be
    /// written back.
    let unfocus<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (view : UnixSystem<'Task, 'Handler>)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : SimulatedMachine<'Task, 'Handler>
        =
        let processId = view.Process.ProcessId

        if not (Map.containsKey processId machine.Processes) then
            failwith
                $"SimulatedMachine.unfocus: no process on the machine has ID %O{processId}, so the view is not of this machine (this is a bug in the client)."

        if view.Generation <> machine.Generation then
            failwith
                $"SimulatedMachine.unfocus: the view of process %O{processId} was focused at %A{view.Generation}, but the machine has since changed and is at %A{machine.Generation}. Writing the view back would undo that change; focus the process again and repeat its call (this is a bug in the client)."

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
            Generation = MachineGeneration.next machine.Generation
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
    /// A refused call changes nothing. `None` if no process on the machine has
    /// that ID.
    let step<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (processId : ProcessId)
        (task : 'Task)
        (call : Syscall)
        (machine : SimulatedMachine<'Task, 'Handler>)
        : (Result<SyscallOutcome, SyscallRefusal<'Task>> * SimulatedMachine<'Task, 'Handler>) option
        =
        inView
            processId
            (fun view ->
                match UnixSystem.step task call view with
                | Ok (outcome, view) -> Ok outcome, view
                | Error refusal -> Error refusal, view
            )
            machine

    /// Every way `machine` fails to be a machine any kernel could be in: the
    /// machine's clauses (`UnixSystem.checkMachineInvariants`) read against
    /// every process on it; each process's view's clauses
    /// (`UnixSystem.checkViewInvariants`) and its descriptor table's
    /// (`FileDescriptorRegistry.checkDescriptorTableInvariants`); the open
    /// file table's (`OpenFileTable.checkInvariants`) read against every
    /// process's descriptor table; and the clauses relating processes to one
    /// another: each is held under its own process ID, no two have one ID,
    /// and every kqueue's owner is a process on the machine.
    ///
    /// The filesystem's own rules are `VirtualFileSystem.checkInvariants`'s,
    /// and are not repeated here.
    let checkInvariants<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (machine : SimulatedMachine<'Task, 'Handler>)
        : SimulatedMachineDefect<'Task> list
        =
        let slots = Map.toList machine.Processes

        let machineDefects =
            UnixSystem.checkMachineInvariants
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
                FileDescriptorRegistry.ofTables slot.Process.FileDescriptors machine.Machine.OpenFiles
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

        let kqueueOwners =
            OpenFileTable.toSeq machine.Machine.OpenFiles
            |> Seq.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue state when not (Map.containsKey state.Owner machine.Processes) ->
                    Some (SimulatedMachineDefect.KqueueOwnerNotOnMachine (id, state.Owner))
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> None
            )
            |> Seq.toList

        machineDefects
        @ viewDefects
        @ tableDefects
        @ openFileDefects
        @ misplaced
        @ duplicates
        @ kqueueOwners
