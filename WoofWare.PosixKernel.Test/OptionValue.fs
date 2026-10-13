namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open WoofWare.PosixKernel

/// A socket option's value as the bytes a caller's buffer holds: the presets
/// are little-endian machines, so a C `int` is `BitConverter`'s layout.
[<RequireQualifiedAccess>]
module OptionValue =

    /// The four bytes of a C `int`.
    let ofInt (value : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (BitConverter.GetBytes value)

    /// The eight bytes of a `struct linger`.
    let ofLinger (onOff : int) (linger : int) : ImmutableArray<byte> =
        ImmutableArray.CreateRange (Array.append (BitConverter.GetBytes onOff) (BitConverter.GetBytes linger))

    /// A whole C `int` copied out, or `None` for any other number of bytes.
    let (|Int|_|) (copied : ImmutableArray<byte>) : int option =
        if copied.Length = 4 then
            Some (BitConverter.ToInt32 (copied.AsSpan ()))
        else
            None

    /// The `int` a `getsockopt` reported whole, failing the test otherwise.
    let readInt (answer : GetSockOptAnswer) : int =
        match answer with
        | GetSockOptAnswer.Reported (Int value) -> value
        | other -> failwith $"expected a whole int, got %A{other}"

    /// What a `getsockopt` reports for an option whose value is `value`,
    /// through a buffer that took `length` bytes of it.
    let reported (value : int) (length : uint32) : GetSockOptAnswer =
        GetSockOptAnswer.Reported (ImmutableArray.Create (ofInt value, 0, int length))

/// `setsockopt(2)` and `getsockopt(2)` as a client makes them, through the
/// library's public protocol, and held to the internal steps they are made of:
/// the admission, then the call given what the copy took.
[<RequireQualifiedAccess>]
module internal SockOptCalls =

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

    /// A `setsockopt` of the value buffer `value`, declared `optionLength`
    /// bytes long, whose first `count` bytes are `contents count` wherever the
    /// kernel copies `count` of them.
    let set<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (optionLength : uint32)
        (contents : int -> ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SetSockOptAnswer * UnixSystem<'Task, 'Handler>, SocketOptionRefusal>
        =
        let expected =
            let supplied =
                match UnixSocket.tryAdmitSetSockOpt fd level optionName value optionLength system with
                | Ok (SetSockOptAdmission.Transfer count) -> Some (contents count)
                | Ok SetSockOptAdmission.NoCopy
                | Ok (SetSockOptAdmission.Answered _)
                | Error _ -> None

            UnixSocket.trySetsockoptBytes fd level optionName value optionLength supplied system

        let actual =
            match UnixSocket.trySetsockopt fd level optionName value optionLength system with
            | Error refusal -> Error refusal
            | Ok (CallProgress.Answered answer) -> Ok answer
            | Ok (CallProgress.NeedsCopyIn paused) ->
                let range = PausedSetSockOpt.copyRange paused

                if range.FirstByte <> 0 || range.ByteCount <= 0 then
                    failwith $"setsockopt asked for %A{range}"

                UnixSocket.trySetsockoptWithValue (contents range.ByteCount) paused

        agreeing "setsockopt" expected actual

    /// A `getsockopt` into the value buffer `value` whose length cell, at
    /// `length`, holds `cell`.
    let get<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (length : UserBuffer)
        (cell : uint32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetSockOptAnswer * UnixSystem<'Task, 'Handler>, SocketOptionRefusal>
        =
        let expected =
            let read =
                match UnixSocket.tryAdmitGetSockOpt fd level optionName value length system with
                | Ok GetSockOptAdmission.ReadLength -> Some cell
                | Ok GetSockOptAdmission.SkipLength
                | Ok (GetSockOptAdmission.Answered _)
                | Error _ -> None

            UnixSocket.tryGetsockoptLength fd level optionName value length read system

        let actual =
            match UnixSocket.tryGetsockopt fd level optionName value length system with
            | Error refusal -> Error refusal
            | Ok (CallProgress.Answered answer) -> Ok answer
            | Ok (CallProgress.NeedsCopyIn paused) -> UnixSocket.tryGetsockoptWithLength cell paused

        agreeing "getsockopt" expected actual
