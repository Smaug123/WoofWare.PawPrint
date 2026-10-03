namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `SocketEvents` encoding of the readiness conditions epoll
/// reports, and the conversions the socket event port's `SystemNative_*`
/// shims perform across it.
///
/// This is PawPrint's half of the socket-event boundary, as `UnixErrorPal` is
/// its half of the errno one. `SocketEvents` is .NET's own five-bit encoding
/// rather than any kernel's: the shim converts it to epoll's bits on the way
/// into `epoll_ctl` and back again on the way out of `epoll_wait`, so the
/// library holds epoll's vocabulary and never meets these numbers.
///
/// The conversions below are transcriptions, so the compiler cannot keep them
/// correct. Their oracle is upstream: `TestSocketEventsPal` re-derives all five
/// bit values from the pinned `pal_networking.h` and each conversion's rows
/// from `pal_networking.c`, and fails if this disagrees.
[<RequireQualifiedAccess>]
module SocketEventsPal =

    /// `SystemNative_TryChangeSocketEventRegistration`'s `SupportedEvents`:
    /// `SA_READ | SA_WRITE | SA_READCLOSE | SA_CLOSE | SA_ERROR`. A mask
    /// carrying any other bit is answered EINVAL in user space, before the
    /// wrapper has looked at either descriptor.
    [<Literal>]
    let supported : int = 0x1F

    /// Each condition `SocketEvents` names, in upstream's order: its bit, and
    /// the `<sys/epoll.h>` bit of the same condition. `GetEPollEvents` and the
    /// epoll `GetSocketEvents` are this table read in each direction.
    let private rows : (int * uint32) list =
        [
            // SA_READ, EPOLLIN.
            0x01, EpollEvents.In
            // SA_WRITE, EPOLLOUT.
            0x02, EpollEvents.Out
            // SA_READCLOSE, EPOLLRDHUP.
            0x04, EpollEvents.RdHup
            // SA_CLOSE, EPOLLHUP.
            0x08, EpollEvents.Hup
            // SA_ERROR, EPOLLERR.
            0x10, EpollEvents.Err
        ]

    /// `GetEPollEvents`: the epoll bits for a `SocketEvents` mask. Total: a bit
    /// outside the five is dropped, although the wrapper's `supported` screen
    /// means none reaches it.
    let toEpollEvents (bits : int) : uint32 =
        rows
        |> List.fold (fun acc (pal, epoll) -> if bits &&& pal <> 0 then acc ||| epoll else acc) 0u

    /// `GetSocketEvents`, the epoll build's: the `SocketEvents` mask naming the
    /// conditions among `events`. Total: every other epoll bit is dropped.
    let ofEpollEvents (events : uint32) : int =
        rows
        |> List.fold (fun acc (pal, epoll) -> if events &&& epoll <> 0u then acc ||| pal else acc) 0

    /// `ConvertEventEPollToSocketAsync`: the `SocketEvent.Events` the shim
    /// writes for one delivered epoll event.
    ///
    /// `EPOLLHUP` folds into `EPOLLIN|EPOLLOUT` and is dropped before the
    /// conversion — "epoll does not play well with disconnected
    /// connection-oriented sockets", pal_networking.c — so `SA_CLOSE` never
    /// reaches a guest through this entry point, and an idle socket's
    /// `OUT|HUP` arrives as `SA_READ|SA_WRITE`.
    let delivered (events : uint32) : int =
        if events &&& EpollEvents.Hup <> 0u then
            (events &&& ~~~EpollEvents.Hup) ||| EpollEvents.In ||| EpollEvents.Out
        else
            events
        |> ofEpollEvents

    /// The `op` `TryChangeSocketEventRegistrationInner` passes `epoll_ctl`,
    /// derived from the caller's *claimed* current mask and its new one:
    /// `EPOLL_CTL_ADD` (1) when the claimed current set is empty,
    /// `EPOLL_CTL_DEL` (2) when the new one is, and `EPOLL_CTL_MOD` (3)
    /// otherwise, in that order of precedence. The claim is never checked
    /// against the kernel's table; a wrong one is answered by `epoll_ctl`
    /// itself, with EEXIST or ENOENT.
    let epollCtlOperation (currentEvents : int) (newEvents : int) : int =
        if currentEvents = 0 then 1
        elif newEvents = 0 then 2
        else 3

    /// `TryChangeSocketEventRegistrationInner`, past the wrapper's two screens
    /// (the `supported` mask, and equal masks answering success unasked): the
    /// `epoll_ctl` the shim makes, with the operation `epollCtlOperation`
    /// derives, the new mask's epoll bits with `EPOLLET` added, and `data`
    /// verbatim.
    let tryChangeSocketEventRegistration<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (portFd : int)
        (targetFd : int)
        (currentEvents : int)
        (newEvents : int)
        (data : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<EpollCtlAnswer * UnixSystem<'Task, 'Handler>, EpollCtlRefusal>
        =
        let events = toEpollEvents newEvents ||| EpollEvents.EdgeTriggered

        UnixPoll.epollCtl
            portFd
            (epollCtlOperation currentEvents newEvents)
            targetFd
            (EpollEventArgument.Readable (events, data))
            system

    /// `TryChangeSocketEventRegistrationInner`, the kqueue build's, past the
    /// wrapper's two screens: the changelist the shim passes `kevent`. It
    /// changes `EVFILT_READ` when `SA_READ` differs between the two masks, and
    /// then `EVFILT_WRITE` when `SA_WRITE` does, each `EV_ADD|EV_CLEAR|EV_RECEIPT`
    /// when the new mask has the bit and `EV_DELETE|EV_RECEIPT` when it does
    /// not, naming the socket's descriptor and carrying `data`. Empty when
    /// neither bit changed: the shim then calls `kevent` with no changes at all.
    let keventChanges (socketFd : int) (currentEvents : int) (newEvents : int) (data : uint64) : Kevent list =
        let saRead = 0x01
        let saWrite = 0x02
        let changes = currentEvents ^^^ newEvents

        let change (filter : int16) (bit : int) : Kevent =
            {
                // `(uint64_t)socket`, from an `int32_t`: sign-extended.
                Ident = uint64 (int64 socketFd)
                Filter = filter
                Flags =
                    if newEvents &&& bit = 0 then
                        KeventFlags.Delete ||| KeventFlags.Receipt
                    else
                        KeventFlags.Add ||| KeventFlags.Clear ||| KeventFlags.Receipt
                FilterFlags = 0u
                Data = 0L
                UserData = data
            }

        [
            if changes &&& saRead <> 0 then
                change KeventFilter.Read saRead
            if changes &&& saWrite <> 0 then
                change KeventFilter.Write saWrite
        ]

    /// `GetSocketEvents`, the kqueue build's: the `SocketEvents` mask
    /// `SystemNative_WaitForSocketEvents` writes for one event `kevent`
    /// reported, from its `filter` and `flags`.
    ///
    /// `EVFILT_READ` is `SA_READ`, with `SA_READCLOSE` for `EV_EOF`;
    /// `EVFILT_WRITE` is `SA_WRITE`, with `SA_READ` for `EV_EOF` ("EOF may be
    /// handled as an EVFILT_READ | EVFILT_WRITE event", pal_networking.c); and
    /// `EV_ERROR` adds `SA_ERROR` to either. No other filter is ever
    /// registered, so none can be reported.
    let ofKevent (filter : int16) (flags : uint16) : int =
        let saRead = 0x01
        let saWrite = 0x02
        let saReadClose = 0x04
        let saError = 0x10
        let eof = flags &&& KeventFlags.Eof <> 0us

        let events =
            match filter with
            | KeventFilter.Read -> saRead ||| (if eof then saReadClose else 0)
            | KeventFilter.Write -> saWrite ||| (if eof then saRead else 0)
            | other ->
                failwith
                    $"SocketEventsPal.ofKevent: kevent reported filter %d{other}, but only EVFILT_READ and EVFILT_WRITE are ever registered through this shim (this is an interpreter bug)."

        if flags &&& KeventFlags.Error <> 0us then
            events ||| saError
        else
            events

    /// The stride of the event buffer `SystemNative_CreateSocketEventBuffer`
    /// allocates and `SystemNative_WaitForSocketEvents` fills, in bytes.
    ///
    /// A compile-time property of the native shim:
    /// `pal_networking.c` defines `SocketEventBufferElementSize` once per backend,
    /// as `max(sizeof(struct epoll_event), sizeof(SocketEvent))` under epoll and
    /// `sizeof(struct kevent)` under kqueue.
    ///
    /// Note what the epoll `max` does, because it is the reason this is a total
    /// function of the flavour where `LinuxEpollLimits.eventSize` is not.
    /// `sizeof(struct epoll_event)` is architecture-dependent — 12 on x86-64 under
    /// `EPOLL_PACKED`, 16 everywhere else — and the `max` against the 16-byte
    /// `SocketEvent` erases exactly that difference, since `max(12, 16)` and
    /// `max(16, 16)` are both 16. So the buffer stride follows the flavour alone,
    /// while the `epoll_wait` constants that skip the `max` do not.
    ///
    /// `sizeof(struct kevent)` is 32 on every 64-bit Darwin:
    /// `{ uintptr_t ident; int16_t filter; uint16_t flags; uint32_t fflags;
    /// intptr_t data; void* udata; }`, measured rather than recalled.
    let socketEventBufferElementSize (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 16
        | SimulatedUnixFlavour.Darwin -> 32
