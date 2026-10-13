namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Whole writes and sends for tests, through the library's public protocol and
/// held to the internal steps it is made of; and the internal steps
/// themselves, made by the process's leader, for a test whose write must raise
/// no signal: the answer and the system, or the refusal. A write that raised
/// one fails the test.
[<RequireQualifiedAccess>]
module internal WriteOutcomes =

    /// The answer and system of `outcome`, failing if the write raised a signal.
    let returned<'Answer, 'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (outcome : Result<WriteStep<'Answer, 'Task, 'Handler>, WriteRefusal>)
        : Result<'Answer * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        outcome
        |> Result.map (fun outcome ->
            match outcome with
            | WriteStep.Returns (answer, system) -> answer, system
            | WriteStep.ReturnsRaising _
            | WriteStep.ProcessEnded _
            | WriteStep.WouldBlock _
            | WriteStep.Restarts _ -> failwith $"expected a write that returns and raises no signal, got %A{outcome}"
        )

    /// A whole `write(2)` by `task`, made a step at a time through the
    /// library's internal steps: `UnixReadWrite.tryAdmitWrite`, and if it says
    /// to transfer, `tryWriteBytes` or `tryWriteBytesThenSleep` of the first
    /// that many of `bytes`. An admission's answer is reported as the write's.
    /// The oracle `admitThenWrite` holds the public protocol to.
    let private stepsThenWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteStep<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        match UnixReadWrite.tryAdmitWrite task fd buffer (uint64 bytes.Length) system with
        | Error refusal -> Error refusal
        | Ok (WriteStep.Returns (WriteAdmission.Answered answer, after)) -> Ok (WriteStep.Returns (answer, after))
        | Ok (WriteStep.ReturnsRaising (WriteAdmission.Answered answer, signal, after)) ->
            Ok (WriteStep.ReturnsRaising (answer, signal, after))
        | Ok (WriteStep.ProcessEnded ended) -> Ok (WriteStep.ProcessEnded ended)
        | Ok (WriteStep.WouldBlock (condition, after)) -> Ok (WriteStep.WouldBlock (condition, after))
        | Ok (WriteStep.Restarts after) -> Ok (WriteStep.Restarts after)
        | Ok (WriteStep.Returns (WriteAdmission.Transfer count, admitted)) ->
            if count > bytes.Length then
                failwith $"admitWrite asked for %d{count} of %d{bytes.Length} bytes"

            UnixReadWrite.tryWriteBytes task fd (ImmutableArray.Create (bytes, 0, count)) admitted
        | Ok (WriteStep.Returns (WriteAdmission.TransferThenSleep (count, total), admitted)) ->
            if total <> bytes.Length || count > bytes.Length then
                failwith $"admitWrite asked for %d{count} of %d{total} bytes, given %d{bytes.Length}"

            UnixReadWrite.tryWriteBytesThenSleep task fd total (ImmutableArray.Create (bytes, 0, count)) admitted
        | Ok (WriteStep.ReturnsRaising (WriteAdmission.Transfer count, signal, _))
        | Ok (WriteStep.ReturnsRaising (WriteAdmission.TransferThenSleep (count, _), signal, _)) ->
            failwith $"admitWrite raised %A{signal} and still asked for %d{count} bytes"

    /// A whole `send(2)` by `task` with the raw flag word `flags`, made a step
    /// at a time as `stepsThenWrite` makes a `write(2)`: the oracle
    /// `admitThenSend` holds the public protocol to.
    let private stepsThenSend<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (bytes : ImmutableArray<byte>)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteStep<WriteAnswer, 'Task, 'Handler>, SendRefusal>
        =
        match UnixReadWrite.tryAdmitSend task fd buffer (uint64 bytes.Length) flags system with
        | Error refusal -> Error refusal
        | Ok (WriteStep.Returns (WriteAdmission.Answered answer, after)) -> Ok (WriteStep.Returns (answer, after))
        | Ok (WriteStep.ReturnsRaising (WriteAdmission.Answered answer, signal, after)) ->
            Ok (WriteStep.ReturnsRaising (answer, signal, after))
        | Ok (WriteStep.ProcessEnded ended) -> Ok (WriteStep.ProcessEnded ended)
        | Ok (WriteStep.WouldBlock (condition, after)) -> Ok (WriteStep.WouldBlock (condition, after))
        | Ok (WriteStep.Restarts after) -> Ok (WriteStep.Restarts after)
        | Ok (WriteStep.Returns (WriteAdmission.Transfer count, admitted)) ->
            if count > bytes.Length then
                failwith $"admitSend asked for %d{count} of %d{bytes.Length} bytes"

            UnixReadWrite.trySendBytes task fd (ImmutableArray.Create (bytes, 0, count)) flags admitted
        | Ok (WriteStep.Returns (WriteAdmission.TransferThenSleep (count, total), admitted)) ->
            if total <> bytes.Length || count > bytes.Length then
                failwith $"admitSend asked for %d{count} of %d{total} bytes, given %d{bytes.Length}"

            Ok (UnixReadWrite.sendBytesThenSleep task fd total (ImmutableArray.Create (bytes, 0, count)) flags admitted)
        | Ok (WriteStep.ReturnsRaising (WriteAdmission.Transfer count, signal, _))
        | Ok (WriteStep.ReturnsRaising (WriteAdmission.TransferThenSleep (count, _), signal, _)) ->
            failwith $"admitSend raised %A{signal} and still asked for %d{count} bytes"

    /// A paused write's or send's answer, in the shape of the internal steps
    /// the tests match on.
    let private stepOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fate : ProcessFate<WriteOutcome<'Task>, 'Task, 'Handler>)
        : WriteStep<WriteAnswer, 'Task, 'Handler>
        =
        match fate with
        | ProcessFate.ProcessEnded ended -> WriteStep.ProcessEnded ended
        | ProcessFate.Continues (WriteOutcome.Returns answer, system) -> WriteStep.Returns (answer, system)
        | ProcessFate.Continues (WriteOutcome.ReturnsRaising (answer, signal), system) ->
            WriteStep.ReturnsRaising (answer, signal, system)
        | ProcessFate.Continues (WriteOutcome.WouldBlock condition, system) -> WriteStep.WouldBlock (condition, system)
        | ProcessFate.Continues (WriteOutcome.Restarts, system) -> WriteStep.Restarts system

    /// The bytes of `bytes` a paused call's `range` names, failing unless the
    /// range lies in the buffer the call was made with, from its start.
    let private copied (call : string) (range : CopyRange) (bytes : ImmutableArray<byte>) : ImmutableArray<byte> =
        if range.FirstByte <> 0 || range.ByteCount < 0 || range.ByteCount > bytes.Length then
            failwith $"%s{call} asked for %A{range} of a buffer of %d{bytes.Length} bytes"

        ImmutableArray.Create (bytes, range.FirstByte, range.ByteCount)

    /// `actual`, having failed unless it is what the oracle `expected` says.
    let private agreeing<'Answer, 'Refusal when 'Answer : equality and 'Refusal : equality>
        (call : string)
        (expected : Result<'Answer, 'Refusal>)
        (actual : Result<'Answer, 'Refusal>)
        : Result<'Answer, 'Refusal>
        =
        if actual <> expected then
            failwith $"%s{call} through the paused call answered %A{actual}, and made a step at a time, %A{expected}"

        actual

    /// A whole `write(2)` by `task` of `bytes` from `buffer`, through the
    /// library's public protocol (`UnixReadWrite.write`, and if it stops at the
    /// copy, `writeWithBytes` of the bytes `PausedWrite.copyRange` names),
    /// held to `stepsThenWrite`, which makes the same write a step at a time.
    let admitThenWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteStep<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        let actual =
            match UnixReadWrite.tryWrite task fd buffer (uint64 bytes.Length) system with
            | Error refusal -> Error refusal
            | Ok (CallProgress.Answered fate) -> Ok (stepOf fate)
            | Ok (CallProgress.NeedsCopyIn paused) ->
                let bytes = copied "write" (PausedWrite.copyRange paused) bytes
                UnixReadWrite.tryWriteWithBytes bytes paused |> Result.map stepOf

        agreeing "write" (stepsThenWrite task fd buffer bytes system) actual

    /// A whole `send(2)` by `task` with the raw flag word `flags`, through the
    /// library's public protocol, as `admitThenWrite` makes a `write(2)`, held
    /// to `stepsThenSend`.
    let admitThenSend<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (bytes : ImmutableArray<byte>)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteStep<WriteAnswer, 'Task, 'Handler>, SendRefusal>
        =
        let actual =
            match UnixReadWrite.trySend task fd buffer (uint64 bytes.Length) flags system with
            | Error refusal -> Error refusal
            | Ok (CallProgress.Answered fate) -> Ok (stepOf fate)
            | Ok (CallProgress.NeedsCopyIn paused) ->
                let bytes = copied "send" (PausedSend.copyRange paused) bytes
                UnixReadWrite.trySendWithBytes bytes paused |> Result.map stepOf

        agreeing "send" (stepsThenSend task fd buffer bytes flags system) actual

    /// The woken `write` or `send` of `task`, finished a step at a time:
    /// `UnixReadWrite.tryAdmitFinishWrite`, and if it says to transfer,
    /// `tryFinishWriteBytes` of `bytesAt offset count`. The oracle
    /// `finishWrite` holds the public protocol to.
    let private stepsThenFinishWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (bytesAt : int -> int -> ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteStep<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        match UnixReadWrite.tryAdmitFinishWrite task system with
        | Error refusal -> Error refusal
        | Ok (WriteStep.Returns (WriteResumption.Transfer (offset, count), admitted)) ->
            UnixReadWrite.tryFinishWriteBytes task (bytesAt offset count) admitted
        | Ok (WriteStep.Returns (WriteResumption.Answered answer, after)) -> Ok (WriteStep.Returns (answer, after))
        | Ok (WriteStep.ReturnsRaising (WriteResumption.Answered answer, signal, after)) ->
            Ok (WriteStep.ReturnsRaising (answer, signal, after))
        | Ok (WriteStep.ReturnsRaising (WriteResumption.Transfer _ as resumption, signal, _)) ->
            failwith $"admitFinishWrite raised %A{signal} and still asked for %A{resumption}"
        | Ok (WriteStep.ProcessEnded ended) -> Ok (WriteStep.ProcessEnded ended)
        | Ok (WriteStep.WouldBlock (condition, after)) -> Ok (WriteStep.WouldBlock (condition, after))
        | Ok (WriteStep.Restarts after) -> Ok (WriteStep.Restarts after)

    /// The woken `write` or `send` of `task`, finished through the library's
    /// public protocol (`UnixReadWrite.finishWrite`, and if it stops at the
    /// copy, `writeWithBytes` of `bytesAt offset count` for the range
    /// `PausedWrite.copyRange` names), held to `stepsThenFinishWrite`.
    /// `bytesAt` must answer exactly `count` bytes.
    let finishWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (bytesAt : int -> int -> ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteStep<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        let actual =
            match UnixReadWrite.tryFinishWrite task system with
            | Error refusal -> Error refusal
            | Ok (CallProgress.Answered fate) -> Ok (stepOf fate)
            | Ok (CallProgress.NeedsCopyIn paused) ->
                let range = PausedWrite.copyRange paused

                if range.FirstByte < 0 || range.ByteCount <= 0 then
                    failwith $"finishWrite asked for %A{range}"

                UnixReadWrite.tryWriteWithBytes (bytesAt range.FirstByte range.ByteCount) paused
                |> Result.map stepOf

        agreeing "finishWrite" (stepsThenFinishWrite task bytesAt system) actual

    /// `UnixReadWrite.tryAdmitWrite` by the leader, through `returned`.
    let admitWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        UnixReadWrite.tryAdmitWrite system.Leader fd buffer count system |> returned

    /// `UnixReadWrite.tryWriteBytes` by the leader, through `returned`.
    let write<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAnswer * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        UnixReadWrite.tryWriteBytes system.Leader fd bytes system |> returned

/// `UnixReadWrite.tryAdmitWrite` for a write whose admission must change nothing:
/// every target but a Darwin pipe, whose timestamps a write moves whatever it
/// answers but `EPIPE`.
[<RequireQualifiedAccess>]
module internal WriteAdmissions =

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
