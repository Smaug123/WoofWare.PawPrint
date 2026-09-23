namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// `SO_REUSEADDR` set or cleared through `setsockopt(2)`, as a fixture that is
/// arranging a scenario rather than testing the call.
[<RequireQualifiedAccess>]
module ReuseAddress =

    /// `setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &value, sizeof(int))` through
    /// real storage, failing the test unless the option is set.
    let set<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : bool)
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.socketOptionLevel platform
        let optionName = SimulatedUnixPlatform.reuseAddressOption platform
        let optionLength = 4u

        let supplied =
            match UnixSocket.admitSetSockOpt fd level optionName UserBuffer.Mapped optionLength system with
            | Ok (SetSockOptAdmission.Transfer _) -> Some (if value then 1 else 0)
            | Ok (SetSockOptAdmission.Answered error) ->
                failwith $"ReuseAddress.set: fd %d{fd} answered %O{error} before the value was read"
            | Error refusal -> failwith $"ReuseAddress.set: %s{SocketOptionRefusal.describe refusal}"

        match UnixSocket.setsockopt fd level optionName UserBuffer.Mapped optionLength supplied system with
        | Ok (SetSockOptAnswer.Set, system) -> system
        | Ok (SetSockOptAnswer.Failed error, _) -> failwith $"ReuseAddress.set: fd %d{fd} answered %O{error}"
        | Error refusal -> failwith $"ReuseAddress.set: %s{SocketOptionRefusal.describe refusal}"
