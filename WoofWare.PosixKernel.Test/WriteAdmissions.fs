namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open WoofWare.PosixKernel

/// `UnixReadWrite.admitWrite` and `UnixReadWrite.write`, made by the process's
/// leader, for a test whose write must raise no signal: the answer and the
/// system, or the refusal. A write that raised one fails the test.
[<RequireQualifiedAccess>]
module WriteOutcomes =

    /// The answer and system of `outcome`, failing if the write raised a signal.
    let returned<'Answer, 'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (outcome : Result<WriteOutcome<'Answer, 'Task, 'Handler>, WriteRefusal>)
        : Result<'Answer * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        outcome
        |> Result.map (fun outcome ->
            match outcome with
            | WriteOutcome.Returns (answer, system) -> answer, system
            | WriteOutcome.ReturnsRaising _
            | WriteOutcome.ProcessEnded _ -> failwith $"expected a write that raises no signal, got %A{outcome}"
        )

    /// A whole `write(2)` by `task`: `UnixReadWrite.admitWrite`, and if it says
    /// to transfer, `UnixReadWrite.write` of the first that many of `bytes`.
    /// An admission's answer is reported as the write's.
    let admitThenWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        match UnixReadWrite.admitWrite task fd buffer (uint64 bytes.Length) system with
        | Error refusal -> Error refusal
        | Ok (WriteOutcome.Returns (WriteAdmission.Answered answer, after)) -> Ok (WriteOutcome.Returns (answer, after))
        | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Answered answer, signal, after)) ->
            Ok (WriteOutcome.ReturnsRaising (answer, signal, after))
        | Ok (WriteOutcome.ProcessEnded ended) -> Ok (WriteOutcome.ProcessEnded ended)
        | Ok (WriteOutcome.Returns (WriteAdmission.Transfer count, admitted)) ->
            if count > bytes.Length then
                failwith $"admitWrite asked for %d{count} of %d{bytes.Length} bytes"

            UnixReadWrite.write task fd (ImmutableArray.Create (bytes, 0, count)) admitted
        | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Transfer count, signal, _)) ->
            failwith $"admitWrite raised %A{signal} and still asked for %d{count} bytes"

    /// `UnixReadWrite.admitWrite` by the leader, through `returned`.
    let admitWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        UnixReadWrite.admitWrite system.Leader fd buffer count system |> returned

    /// `UnixReadWrite.write` by the leader, through `returned`.
    let write<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAnswer * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        UnixReadWrite.write system.Leader fd bytes system |> returned

/// `UnixReadWrite.admitWrite` for a write whose admission must change nothing:
/// every target but a Darwin pipe, whose timestamps a write moves whatever it
/// answers but `EPIPE`.
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
        WriteOutcomes.admitWrite fd buffer count system
        |> Result.map (fun (admission, after) ->
            if after <> system then
                failwith
                    $"admitWrite on fd %d{fd} changed the system for an admission of %A{admission}, which only a write reaching a Darwin pipe may do"

            admission
        )
