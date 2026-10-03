namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// `UnixReadWrite.pread` of anything but a device that draws from the entropy
/// pool, which changes nothing: the answer alone, failing the test that made
/// the call if the system came back changed.
[<RequireQualifiedAccess>]
module internal PReadUnchanged =

    let pread<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer, BufferRefusal>
        =
        match UnixReadWrite.pread system.Leader fd buffer count offset system with
        | Error (PReadRefusal.Buffer refusal) -> Error refusal
        | Error (PReadRefusal.SignalAtPageBoundary _ as refusal) ->
            failwith $"pread(%d{fd}, %d{count}@%d{offset}) was refused, which only a device's can be: %A{refusal}"
        | Ok (answer, after) ->
            if after <> system then
                failwith $"pread(%d{fd}, %d{count}@%d{offset}) answered %A{answer} and changed the system"

            Ok answer
