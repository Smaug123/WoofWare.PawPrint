namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Tasks for a test that needs more than a process's leader.
[<RequireQualifiedAccess>]
module Tasks =

    /// `child`, created by the process's leader on processor 0, failing the test if
    /// the creation is refused.
    let spawn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (child : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        match UnixTaskLifecycle.spawn system.Leader child (CpuId 0) system with
        | Ok (SpawnAnswer.Spawned _, system) -> system
        | Ok (SpawnAnswer.Failed error, _) -> failwith $"spawning %O{child} failed with %O{error}"
        | Error refusal -> failwith $"spawning %O{child} was refused: %s{SpawnRefusal.describe refusal}"

    /// `system` with a task `name`: itself if `name` is already a task, and
    /// otherwise with `name` spawned by the leader.
    let ensure<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (name : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        if Map.containsKey name system.Tasks then
            system
        else
            spawn name system
