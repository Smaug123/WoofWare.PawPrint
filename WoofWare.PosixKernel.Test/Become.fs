namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Changing who a running process is, through the syscalls, as a privileged
/// program does it: the supplementary groups first, then the group IDs, then
/// the user IDs, since giving up the user ID is what gives up the privilege
/// the first two steps need.
///
/// Linux only: Darwin has no `setresuid` or `setresgid`.
[<RequireQualifiedAccess>]
module Become =

    let private completed<'Task, 'Handler, 'Refusal when 'Task : comparison and 'Handler : equality>
        (what : string)
        (result : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, 'Refusal>)
        : UnixSystem<'Task, 'Handler>
        =
        match result with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"Become: %s{what} did not complete: %A{other}"

    let private setGroups<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (groups : GroupId list)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        UnixCredentials.setgroups
            (List.length groups)
            (GroupListWords.Readable (groups |> List.map GroupId.toUInt32))
            system
        |> completed "setgroups"

    /// The process, which must be privileged, as `credentials` in full: all
    /// six IDs and the supplementary groups.
    let fully<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (credentials : Credentials)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        system
        |> setGroups credentials.SupplementaryGroups
        |> UnixCredentials.setresgid
            (Some credentials.RealGroup)
            (Some credentials.EffectiveGroup)
            (Some credentials.SavedGroup)
        |> completed "setresgid"
        |> UnixCredentials.setresuid
            (Some credentials.RealUser)
            (Some credentials.EffectiveUser)
            (Some credentials.SavedUser)
        |> completed "setresuid"

    /// The process, whose real and saved user IDs must be 0, acting as
    /// `credentials`: its groups, and its effective user ID, with the real and
    /// saved user IDs left at 0 so that `rootAgain` can take privilege back.
    let temporarily<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (credentials : Credentials)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        system
        |> setGroups credentials.SupplementaryGroups
        |> UnixCredentials.setresgid
            (Some credentials.RealGroup)
            (Some credentials.EffectiveGroup)
            (Some credentials.SavedGroup)
        |> completed "setresgid"
        |> UnixCredentials.setresuid None (Some credentials.EffectiveUser) None
        |> completed "setresuid"

    /// The process, whose real or saved user ID is 0, privileged again: its
    /// effective user ID set back to 0, and nothing else changed.
    let rootAgain<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        UnixCredentials.setresuid None (Some UserId.root) None system
        |> completed "setresuid"
