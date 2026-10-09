namespace WoofWare.PosixKernel

/// One of the machine's NUMA nodes, as `getcpu(2)` reports it.
type NumaNode =
    | NumaNode of int

    /// <summary>
    /// A human-readable description of the NUMA node.
    /// </summary>
    override this.ToString () =
        match this with
        | NumaNode.NumaNode i -> $"<numa node #%i{i}>"

/// What `getcpu(2)` answers: where the calling task is running.
type GetCpuAnswer =
    {
        /// The logical processor the task is running on.
        Cpu : CpuId
        /// The NUMA node that processor belongs to. This library models a
        /// machine with one node, so it is always node 0, which is what every
        /// `getcpu` answered on Linux 6.18.5 at two and at five processors
        /// (`docs/plans/2026-08-23-posix-kernel-extraction/cpu-placement.c`).
        Node : NumaNode
    }

/// Why this kernel will not answer a `getcpu(2)`.
[<RequireQualifiedAccess>]
type GetCpuRefusal =
    /// This kernel is not Linux-flavoured, and only Linux has `getcpu`.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module GetCpuRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which of its entry points asked.
    let describe (refusal : GetCpuRefusal) : string =
        match refusal with
        | GetCpuRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and getcpu exists on Linux only. Darwin's libc has neither sched_getcpu nor getcpu."

/// Which task runs on which of the machine's logical processors.
///
/// The client chooses which task runs where, and reports each choice with
/// `dispatch`; this library records it, and answers `getcpu` from the record.
/// A task's processor (`UnixTaskState.cpu`) is the one it last ran on, or,
/// before it first runs, the one its creator named. Each processor of the
/// machine runs at most one task, whichever process it belongs to, and a task
/// is running exactly when it is its processor's (`runningOn`).
///
/// A task stops running when the client dispatches another to its processor,
/// when it parks in a syscall, and when it exits or its process ends. A parked
/// task may be dispatched: that is how a woken task gets back onto a processor
/// to finish its call.
[<RequireQualifiedAccess>]
module UnixScheduling =

    /// `task` is now running on the logical processor `cpu`: the client's
    /// report of a dispatch.
    ///
    /// `task`'s processor becomes `cpu`. Whichever task `cpu` was running,
    /// in this process or another on the machine, stops running there and
    /// keeps `cpu` as its processor, as a preempted task does. If `task` was
    /// running on another processor, that one is now idle. Dispatching a task
    /// to the processor it is already running on changes nothing, and answers
    /// `system` itself.
    ///
    /// A parked task may be dispatched. Whether it was woken is the client's
    /// record (`UnixWait.wakes`), so this library does not refuse one that was
    /// not.
    ///
    /// Fails loudly if `task` names no task, or if `cpu` is not one of the
    /// machine's processors (`UnixSystem.processorCount`): each is a bug in
    /// the client.
    let dispatch<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (cpu : CpuId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let state = UnixTaskTable.get task system.Tasks

        if not (UnixMachineState.hasProcessor cpu system.Machine) then
            failwith
                $"UnixScheduling.dispatch: %O{task} was dispatched to %O{cpu}, but the machine has %d{system.Machine.ProcessorCount} logical processors, numbered from 0 (this is a bug in the client)."

        let running = Map.tryFind cpu system.Machine.Occupants = Some state.OsThreadId

        if state.Cpu = cpu && running then
            system
        else

        let machine =
            let vacated = UnixMachineState.vacate state.Cpu state.OsThreadId system.Machine

            { vacated with
                Occupants = Map.add cpu state.OsThreadId vacated.Occupants
            }

        let tasks =
            if state.Cpu = cpu then
                system.Tasks
            else
                Map.add
                    task
                    { state with
                        Cpu = cpu
                    }
                    system.Tasks

        { system with
            Machine = machine
            Tasks = tasks
        }

    /// The logical processor `task` is running on: `None` if it is not
    /// running, because the client has not dispatched it since it was created
    /// or since it last parked, or has dispatched another task to its
    /// processor since, and `None` if `task` names no task.
    let runningOn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : CpuId option
        =
        match Map.tryFind task system.Tasks with
        | Some state when Map.tryFind state.Cpu system.Machine.Occupants = Some state.OsThreadId -> Some state.Cpu
        | Some _
        | None -> None

    /// `getcpu(2)`, made by `task`: the processor it is running on, and that
    /// processor's NUMA node. glibc's `sched_getcpu(3)` reads the same value.
    ///
    /// Refuses under Darwin, which has no `getcpu`.
    ///
    /// Fails loudly if `task` names no task, is parked in a syscall, or is not
    /// running (`runningOn`): a task making a syscall is running, so each
    /// means the client's record of what it runs disagrees with this
    /// library's, which is a bug in the client.
    let getcpu<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetCpuAnswer, GetCpuRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (GetCpuRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let state = UnixTaskTable.get task system.Tasks

        match state.Parked with
        | Some park ->
            failwith
                $"UnixScheduling.getcpu: task %O{task} is parked in %A{park.Syscall}, so it cannot be making the getcpu syscall (this is a bug in the client)."
        | None ->

        match Map.tryFind state.Cpu system.Machine.Occupants with
        | Some running when running = state.OsThreadId ->
            Ok
                {
                    Cpu = state.Cpu
                    Node = NumaNode 0
                }
        | occupant ->
            let occupied =
                match occupant with
                | Some other -> $"%O{state.Cpu} is running the task with thread ID %O{other}"
                | None -> $"%O{state.Cpu} is idle"

            failwith
                $"UnixScheduling.getcpu: task %O{task} is not running on any processor (%s{occupied}), so it cannot be making the getcpu syscall. Report each task the client runs with UnixScheduling.dispatch (this is a bug in the client)."
