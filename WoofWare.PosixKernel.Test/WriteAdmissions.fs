namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// `UnixReadWrite.admitWrite` for a write whose admission must change nothing:
/// every target but a Darwin pipe, whose timestamps a write moves whatever it
/// answers.
[<RequireQualifiedAccess>]
module WriteAdmissions =

    /// The admission, having checked that the system came back as it arrived.
    let unchanged<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission, WriteRefusal>
        =
        UnixReadWrite.admitWrite fd buffer count system
        |> Result.map (fun (admission, after) ->
            if after <> system then
                failwith
                    $"admitWrite on fd %d{fd} changed the system for an admission of %A{admission}, which only a write reaching a Darwin pipe may do"

            admission
        )
