namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// A `UnixConnection.connect`'s outcome, for a test about what a connect
/// answers rather than about whether it sleeps.
[<RequireQualifiedAccess>]
module internal ConnectOutcomes =

    /// A connect's result as the answer it returned, failing the test if the
    /// call slept or restarted. No connect sleeps in this version: the one that
    /// would, to a full listener, is refused.
    let answered<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (result : Result<SyscallOutcome * UnixSystem<'Task, 'Handler>, ConnectRefusal>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ConnectRefusal>
        =
        result
        |> Result.map (fun (outcome, after) ->
            match outcome with
            | SyscallOutcome.Answered answer -> answer, after
            | SyscallOutcome.WouldBlock _
            | SyscallOutcome.Restarts -> failwith $"expected a connect that returns, got %A{outcome}"
        )
