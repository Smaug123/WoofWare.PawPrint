namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `PollEvents` encoding of `poll(2)`'s event bits, and the
/// conversions `SystemNative_Poll` performs across it.
///
/// This is PawPrint's half of the poll boundary, as `SocketEventsPal` is its
/// half of the epoll one. `PAL_POLLIN` .. `PAL_POLLNVAL` are .NET's own six-bit
/// encoding rather than any kernel's: the shim converts a caller's
/// `PollEvent.Events` to the platform's `<poll.h>` bits on the way into
/// `poll(2)`, and each `revents` back on the way out. Each direction has exactly
/// six rows, so every other bit is dropped both ways — a guest cannot ask for
/// `POLLRDNORM` or `POLLRDHUP` through this entry point, and never sees them
/// come back. The library holds the platform's numbering and never meets these
/// numbers.
///
/// The conversions are transcriptions, so the compiler cannot keep them
/// correct. Their oracle is upstream: `TestPollEventsPal` re-derives the six
/// PAL values and each conversion's rows from the pinned `pal_io_common.h`.
[<RequireQualifiedAccess>]
module PollEventsPal =

    /// Each row of `Common_ConvertPollEventsPalToPlatform` and of its inverse,
    /// in upstream's order: a `PAL_POLL*` value and the platform's `POLL*` of
    /// the same name.
    ///
    /// One table serves both flavours because their `<poll.h>` number these six
    /// alike, 0x01 through 0x20 (measured 2026-09-23 on Linux 6.18.5 and Darwin
    /// 25.6.0, `docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c`).
    /// The two headers part ways only above them, which neither conversion
    /// reaches.
    let private rows : (int16 * int16) list =
        [
            // PAL_POLLIN, POLLIN.
            0x0001s, 0x0001s
            // PAL_POLLPRI, POLLPRI.
            0x0002s, 0x0002s
            // PAL_POLLOUT, POLLOUT.
            0x0004s, 0x0004s
            // PAL_POLLERR, POLLERR.
            0x0008s, 0x0008s
            // PAL_POLLHUP, POLLHUP.
            0x0010s, 0x0010s
            // PAL_POLLNVAL, POLLNVAL.
            0x0020s, 0x0020s
        ]

    /// `Common_ConvertPollEventsPalToPlatform`: the `events` the shim hands
    /// `poll(2)` for a caller's `PollEvent.Events`. Total: a bit outside the six
    /// is dropped, not refused.
    let toPlatform (palEvents : int16) : int16 =
        rows
        |> List.fold (fun acc (pal, platform) -> if palEvents &&& pal <> 0s then acc ||| platform else acc) 0s

    /// `Common_ConvertPollEventsPlatformToPal`: the `PollEvent.TriggeredEvents`
    /// the shim writes for a kernel's `revents`. Total: a bit outside the six is
    /// dropped.
    let ofPlatform (platformEvents : int16) : int16 =
        rows
        |> List.fold
            (fun acc (pal, platform) ->
                if platformEvents &&& platform <> 0s then
                    acc ||| pal
                else
                    acc
            )
            0s

    /// `Common_Poll` between its argument screens and its copy-out: convert each
    /// entry's PAL `Events`, ask the kernel's `poll(2)`, and convert each
    /// `revents` back.
    ///
    /// `entries` pairs each `PollEvent.FileDescriptor` with its PAL `Events`.
    /// Answers the PAL `TriggeredEvents` for each entry, in order, and
    /// `poll(2)`'s own return value, which the shim stores through `triggered`
    /// unconverted.
    let poll<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (entries : (int * int16) list)
        (milliseconds : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int16 list * int, PollRefusal>
        =
        let platformEntries =
            entries
            |> List.map (fun (fd, palEvents) ->
                {
                    PollEntry.Fd = fd
                    Events = toPlatform palEvents
                }
            )

        match UnixPoll.poll platformEntries milliseconds system with
        | Error refusal -> Error refusal
        // The count is the kernel's, not a count of the converted reports, as
        // in the C. The two cannot differ here: a PAL request asks for none of
        // the bits `ofPlatform` drops, and `poll(2)` reports those only when
        // asked.
        | Ok (reported, triggered) -> Ok (List.map ofPlatform reported, triggered)
