namespace WoofWare.PawPrint.Test

open WoofWare.PosixKernel

/// The one process of a machine that holds exactly one, as most tests build: so that a test
/// can name a call's caller by the thread alone.
[<RequireQualifiedAccess>]
module LoneProcess =

    /// The ID of `system`'s one process. Fails the test unless `system` holds exactly one.
    let pid<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : ProcessId
        =
        match Set.toList (UnixSystem.processIds system) with
        | [ pid ] -> pid
        | pids -> failwith $"expected a machine of one process, but it holds %O{pids}"

    /// `thread` of `system`'s one process, as the caller of a syscall.
    let caller<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (thread : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Caller<'Task>
        =
        Caller.make (pid system) thread

    /// The leader of `system`'s one process, as the caller of a syscall.
    let leader<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Caller<'Task>
        =
        let pid = pid system
        Caller.make pid (UnixSystem.leader pid system)
