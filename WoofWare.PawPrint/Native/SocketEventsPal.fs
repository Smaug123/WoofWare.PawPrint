namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `SocketEvents` encoding of the readiness conditions
/// `WoofWare.PosixKernel` speaks, and the conversions the socket event port's
/// `SystemNative_*` shims perform across it.
///
/// This is PawPrint's half of the socket-event boundary, as `UnixErrorPal` is
/// its half of the errno one. `SocketEvents` is .NET's own five-bit encoding
/// rather than any kernel's: the shim converts it to epoll's bits on the way
/// into `epoll_ctl` and back again on the way out of `epoll_wait`, so the
/// library holds epoll's vocabulary and never meets these numbers.
///
/// The conversions below are transcriptions, so the compiler cannot keep them
/// correct. Their oracle is upstream: `TestSocketEventsPal` re-derives all five
/// bit values from the pinned `pal_networking.h` and fails if this disagrees.
[<RequireQualifiedAccess>]
module SocketEventsPal =

    /// `SystemNative_TryChangeSocketEventRegistration`'s `SupportedEvents`:
    /// `SA_READ | SA_WRITE | SA_READCLOSE | SA_CLOSE | SA_ERROR`. A mask
    /// carrying any other bit is answered EINVAL in user space, before the
    /// wrapper has looked at either descriptor.
    [<Literal>]
    let supported : int = 0x1F

    /// `GetEPollEvents`, less the two bits `epoll_ctl` does not keep: the
    /// interest a registration whose `SocketEvents` mask is `bits` leaves
    /// behind.
    ///
    /// Lossy, and unavoidably so. `SA_CLOSE` and `SA_ERROR` become `EPOLLHUP`
    /// and `EPOLLERR`, which the kernel forces into every stored mask, so a
    /// caller that set them and one that did not have made the same
    /// registration — nothing downstream of here can tell them apart, and
    /// `SocketEventInterest` therefore does not carry them.
    ///
    /// Partial: `bits` must already have passed the `supported` screen, which
    /// the wrapper runs in user space before any registration is attempted, so
    /// an out-of-range mask arriving here is an interpreter bug rather than a
    /// guest error.
    let toInterest (context : string) (bits : int) : SocketEventInterest =
        if bits &&& ~~~supported <> 0 then
            failwith
                $"%s{context}: SocketEvents mask 0x%x{bits} has bits outside READ|WRITE|READCLOSE|CLOSE|ERROR (0x1F); the wrapper's EINVAL screen should have refused it before any registration was attempted (this is an interpreter bug)."

        {
            SocketEventInterest.In = bits &&& 0x01 <> 0
            Out = bits &&& 0x02 <> 0
            RdHup = bits &&& 0x04 <> 0
        }

    /// `GetSocketEvents`: the `SocketEvents` mask naming an epoll readiness
    /// set.
    ///
    /// All five rows, as upstream has them, even though `delivered` — the only
    /// caller — can never reach the `SA_CLOSE` one. Keeping the two functions
    /// apart is what lets the pinned source check each of upstream's on its
    /// own, so that a future divergence says which one moved.
    let ofReadiness (level : ReadinessLevel) : int =
        (if level.In then 0x01 else 0)
        ||| (if level.Out then 0x02 else 0)
        ||| (if level.RdHup then 0x04 else 0)
        ||| (if level.Hup then 0x08 else 0)
        ||| (if level.Err then 0x10 else 0)

    /// `ConvertEventEPollToSocketAsync`: the `SocketEvent.Events` the shim
    /// writes for one delivered epoll event.
    ///
    /// `EPOLLHUP` folds into `EPOLLIN|EPOLLOUT` and is dropped before the
    /// conversion — "epoll does not play well with disconnected
    /// connection-oriented sockets", pal_networking.c — so `SA_CLOSE` never
    /// reaches a guest through this entry point, and an idle socket's
    /// `OUT|HUP` arrives as `SA_READ|SA_WRITE`.
    let delivered (level : ReadinessLevel) : int =
        if level.Hup then
            { level with
                Hup = false
                In = true
                Out = true
            }
        else
            level
        |> ofReadiness
