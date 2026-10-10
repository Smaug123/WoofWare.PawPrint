namespace WoofWare.PosixKernel

/// One bit of the flag word `recv(2)` and `send(2)` take, named as the
/// flavour's `<sys/socket.h>` names it. The two flavours number the bits they
/// share differently, and each defines some the other does not.
[<RequireQualifiedAccess>]
type MessageFlag =
    /// `MSG_OOB`.
    | OutOfBand
    /// `MSG_PEEK`.
    | Peek
    /// `MSG_DONTROUTE`.
    | DontRoute
    /// `MSG_EOR`.
    | EndOfRecord
    /// `MSG_TRUNC`.
    | Truncate
    /// `MSG_CTRUNC`.
    | ControlTruncate
    /// `MSG_WAITALL`.
    | WaitAll
    /// `MSG_DONTWAIT`.
    | DontWait
    /// `MSG_NOSIGNAL`.
    | NoSignal
    /// Linux's `MSG_PROXY`.
    | Proxy
    /// Linux's `MSG_FIN`.
    | Fin
    /// Linux's `MSG_SYN`.
    | Syn
    /// Linux's `MSG_CONFIRM`.
    | Confirm
    /// Linux's `MSG_RST`.
    | Reset
    /// Linux's `MSG_ERRQUEUE`.
    | ErrorQueue
    /// Linux's `MSG_MORE`.
    | More
    /// Linux's `MSG_WAITFORONE`.
    | WaitForOne
    /// Linux's `MSG_BATCH`.
    | Batch
    /// Linux's `MSG_SOCK_DEVMEM`.
    | SocketDeviceMemory
    /// Linux's `MSG_ZEROCOPY`.
    | ZeroCopy
    /// Linux's `MSG_FASTOPEN`.
    | FastOpen
    /// Linux's `MSG_CMSG_CLOEXEC`.
    | ControlCloseOnExec
    /// Darwin's `MSG_EOF`.
    | EndOfFile
    /// Darwin's `MSG_WAITSTREAM`.
    | WaitStream
    /// Darwin's `MSG_FLUSH`.
    | Flush
    /// Darwin's `MSG_HOLD`.
    | Hold
    /// Darwin's `MSG_SEND`.
    | Send
    /// Darwin's `MSG_HAVEMORE`.
    | HaveMore
    /// Darwin's `MSG_RCVMORE`.
    | ReceiveMore
    /// Darwin's `MSG_NEEDSA`.
    | NeedSocketAddress
    /// A bit of the word to which the flavour's header gives no name.
    | Unnamed of bit : int

[<RequireQualifiedAccess>]
module MessageFlag =
    /// Every flag either flavour's header names.
    let named : MessageFlag list =
        [
            MessageFlag.OutOfBand
            MessageFlag.Peek
            MessageFlag.DontRoute
            MessageFlag.EndOfRecord
            MessageFlag.Truncate
            MessageFlag.ControlTruncate
            MessageFlag.WaitAll
            MessageFlag.DontWait
            MessageFlag.NoSignal
            MessageFlag.Proxy
            MessageFlag.Fin
            MessageFlag.Syn
            MessageFlag.Confirm
            MessageFlag.Reset
            MessageFlag.ErrorQueue
            MessageFlag.More
            MessageFlag.WaitForOne
            MessageFlag.Batch
            MessageFlag.SocketDeviceMemory
            MessageFlag.ZeroCopy
            MessageFlag.FastOpen
            MessageFlag.ControlCloseOnExec
            MessageFlag.EndOfFile
            MessageFlag.WaitStream
            MessageFlag.Flush
            MessageFlag.Hold
            MessageFlag.Send
            MessageFlag.HaveMore
            MessageFlag.ReceiveMore
            MessageFlag.NeedSocketAddress
        ]

    /// The bit `flavour`'s `<sys/socket.h>` gives `flag`, or `None` if it does
    /// not define it. Measured (`tcp-recv-send.c` section H, glibc 2.41 on
    /// Linux 6.18.5 and the macOS 27.0 SDK): Linux's numbering is the
    /// architecture-independent `<bits/socket.h>` one.
    let number (flavour : SimulatedUnixFlavour) (flag : MessageFlag) : int option =
        match flavour, flag with
        | _, MessageFlag.Unnamed _ -> None
        | _, MessageFlag.OutOfBand -> Some 0x1
        | _, MessageFlag.Peek -> Some 0x2
        | _, MessageFlag.DontRoute -> Some 0x4
        | SimulatedUnixFlavour.Linux, MessageFlag.ControlTruncate -> Some 0x8
        | SimulatedUnixFlavour.Linux, MessageFlag.Proxy -> Some 0x10
        | SimulatedUnixFlavour.Linux, MessageFlag.Truncate -> Some 0x20
        | SimulatedUnixFlavour.Linux, MessageFlag.DontWait -> Some 0x40
        | SimulatedUnixFlavour.Linux, MessageFlag.EndOfRecord -> Some 0x80
        | SimulatedUnixFlavour.Linux, MessageFlag.WaitAll -> Some 0x100
        | SimulatedUnixFlavour.Linux, MessageFlag.Fin -> Some 0x200
        | SimulatedUnixFlavour.Linux, MessageFlag.Syn -> Some 0x400
        | SimulatedUnixFlavour.Linux, MessageFlag.Confirm -> Some 0x800
        | SimulatedUnixFlavour.Linux, MessageFlag.Reset -> Some 0x1000
        | SimulatedUnixFlavour.Linux, MessageFlag.ErrorQueue -> Some 0x2000
        | SimulatedUnixFlavour.Linux, MessageFlag.NoSignal -> Some 0x4000
        | SimulatedUnixFlavour.Linux, MessageFlag.More -> Some 0x8000
        | SimulatedUnixFlavour.Linux, MessageFlag.WaitForOne -> Some 0x10000
        | SimulatedUnixFlavour.Linux, MessageFlag.Batch -> Some 0x40000
        | SimulatedUnixFlavour.Linux, MessageFlag.SocketDeviceMemory -> Some 0x2000000
        | SimulatedUnixFlavour.Linux, MessageFlag.ZeroCopy -> Some 0x4000000
        | SimulatedUnixFlavour.Linux, MessageFlag.FastOpen -> Some 0x20000000
        | SimulatedUnixFlavour.Linux, MessageFlag.ControlCloseOnExec -> Some 0x40000000
        | SimulatedUnixFlavour.Linux, MessageFlag.EndOfFile
        | SimulatedUnixFlavour.Linux, MessageFlag.WaitStream
        | SimulatedUnixFlavour.Linux, MessageFlag.Flush
        | SimulatedUnixFlavour.Linux, MessageFlag.Hold
        | SimulatedUnixFlavour.Linux, MessageFlag.Send
        | SimulatedUnixFlavour.Linux, MessageFlag.HaveMore
        | SimulatedUnixFlavour.Linux, MessageFlag.ReceiveMore
        | SimulatedUnixFlavour.Linux, MessageFlag.NeedSocketAddress -> None
        | SimulatedUnixFlavour.Darwin, MessageFlag.EndOfRecord -> Some 0x8
        | SimulatedUnixFlavour.Darwin, MessageFlag.Truncate -> Some 0x10
        | SimulatedUnixFlavour.Darwin, MessageFlag.ControlTruncate -> Some 0x20
        | SimulatedUnixFlavour.Darwin, MessageFlag.WaitAll -> Some 0x40
        | SimulatedUnixFlavour.Darwin, MessageFlag.DontWait -> Some 0x80
        | SimulatedUnixFlavour.Darwin, MessageFlag.EndOfFile -> Some 0x100
        | SimulatedUnixFlavour.Darwin, MessageFlag.WaitStream -> Some 0x200
        | SimulatedUnixFlavour.Darwin, MessageFlag.Flush -> Some 0x400
        | SimulatedUnixFlavour.Darwin, MessageFlag.Hold -> Some 0x800
        | SimulatedUnixFlavour.Darwin, MessageFlag.Send -> Some 0x1000
        | SimulatedUnixFlavour.Darwin, MessageFlag.HaveMore -> Some 0x2000
        | SimulatedUnixFlavour.Darwin, MessageFlag.ReceiveMore -> Some 0x4000
        | SimulatedUnixFlavour.Darwin, MessageFlag.NeedSocketAddress -> Some 0x10000
        | SimulatedUnixFlavour.Darwin, MessageFlag.NoSignal -> Some 0x80000
        | SimulatedUnixFlavour.Darwin, MessageFlag.Proxy
        | SimulatedUnixFlavour.Darwin, MessageFlag.Fin
        | SimulatedUnixFlavour.Darwin, MessageFlag.Syn
        | SimulatedUnixFlavour.Darwin, MessageFlag.Confirm
        | SimulatedUnixFlavour.Darwin, MessageFlag.Reset
        | SimulatedUnixFlavour.Darwin, MessageFlag.ErrorQueue
        | SimulatedUnixFlavour.Darwin, MessageFlag.More
        | SimulatedUnixFlavour.Darwin, MessageFlag.WaitForOne
        | SimulatedUnixFlavour.Darwin, MessageFlag.Batch
        | SimulatedUnixFlavour.Darwin, MessageFlag.SocketDeviceMemory
        | SimulatedUnixFlavour.Darwin, MessageFlag.ZeroCopy
        | SimulatedUnixFlavour.Darwin, MessageFlag.FastOpen
        | SimulatedUnixFlavour.Darwin, MessageFlag.ControlCloseOnExec -> None

    /// The name `<sys/socket.h>` gives `flag`, or the bit for an unnamed one.
    let describe (flag : MessageFlag) : string =
        match flag with
        | MessageFlag.OutOfBand -> "MSG_OOB"
        | MessageFlag.Peek -> "MSG_PEEK"
        | MessageFlag.DontRoute -> "MSG_DONTROUTE"
        | MessageFlag.EndOfRecord -> "MSG_EOR"
        | MessageFlag.Truncate -> "MSG_TRUNC"
        | MessageFlag.ControlTruncate -> "MSG_CTRUNC"
        | MessageFlag.WaitAll -> "MSG_WAITALL"
        | MessageFlag.DontWait -> "MSG_DONTWAIT"
        | MessageFlag.NoSignal -> "MSG_NOSIGNAL"
        | MessageFlag.Proxy -> "MSG_PROXY"
        | MessageFlag.Fin -> "MSG_FIN"
        | MessageFlag.Syn -> "MSG_SYN"
        | MessageFlag.Confirm -> "MSG_CONFIRM"
        | MessageFlag.Reset -> "MSG_RST"
        | MessageFlag.ErrorQueue -> "MSG_ERRQUEUE"
        | MessageFlag.More -> "MSG_MORE"
        | MessageFlag.WaitForOne -> "MSG_WAITFORONE"
        | MessageFlag.Batch -> "MSG_BATCH"
        | MessageFlag.SocketDeviceMemory -> "MSG_SOCK_DEVMEM"
        | MessageFlag.ZeroCopy -> "MSG_ZEROCOPY"
        | MessageFlag.FastOpen -> "MSG_FASTOPEN"
        | MessageFlag.ControlCloseOnExec -> "MSG_CMSG_CLOEXEC"
        | MessageFlag.EndOfFile -> "MSG_EOF"
        | MessageFlag.WaitStream -> "MSG_WAITSTREAM"
        | MessageFlag.Flush -> "MSG_FLUSH"
        | MessageFlag.Hold -> "MSG_HOLD"
        | MessageFlag.Send -> "MSG_SEND"
        | MessageFlag.HaveMore -> "MSG_HAVEMORE"
        | MessageFlag.ReceiveMore -> "MSG_RCVMORE"
        | MessageFlag.NeedSocketAddress -> "MSG_NEEDSA"
        | MessageFlag.Unnamed bit -> $"the unnamed bit 0x%x{bit}"

    /// Each bit set in `word`, from the lowest, as `flavour` names it.
    let decode (flavour : SimulatedUnixFlavour) (word : int) : MessageFlag list =
        [
            for i in 0..31 do
                let bit = 1 <<< i

                if word &&& bit <> 0 then
                    match named |> List.tryFind (fun flag -> number flavour flag = Some bit) with
                    | Some flag -> flag
                    | None -> MessageFlag.Unnamed bit
        ]

    /// The word with exactly `flags` set, in `flavour`'s numbering; `None` if
    /// `flavour` does not define one of them. An unnamed bit is its own number.
    let encode (flavour : SimulatedUnixFlavour) (flags : MessageFlag list) : int option =
        (Some 0, flags)
        ||> List.fold (fun word flag ->
            match word, flag with
            | None, _ -> None
            | Some word, MessageFlag.Unnamed bit -> Some (word ||| bit)
            | Some word, flag -> number flavour flag |> Option.map (fun bit -> word ||| bit)
        )

/// The flags of a `recv(2)` this kernel models.
type internal ReceiveFlags =
    {
        /// `MSG_PEEK`: the bytes stay queued.
        Peek : bool
        /// `MSG_DONTWAIT`: the call answers `EAGAIN` rather than sleeping,
        /// whatever the description says.
        DontWait : bool
    }

[<RequireQualifiedAccess>]
module internal ReceiveFlags =
    /// The flags of `word`, in `flavour`'s numbering; or every flag in it this
    /// kernel does not model for `recv(2)`. `MSG_NOSIGNAL` is taken and
    /// changes nothing: measured on both (`tcp-recv-send.c` section N-recv),
    /// a receive with it answers as one without.
    let decode (flavour : SimulatedUnixFlavour) (word : int) : Result<ReceiveFlags, MessageFlag list> =
        let flags = MessageFlag.decode flavour word

        let unmodelled =
            flags
            |> List.filter (fun flag ->
                match flag with
                | MessageFlag.Peek
                | MessageFlag.DontWait
                | MessageFlag.NoSignal -> false
                | _ -> true
            )

        if unmodelled.IsEmpty then
            Ok
                {
                    Peek = List.contains MessageFlag.Peek flags
                    DontWait = List.contains MessageFlag.DontWait flags
                }
        else
            Error unmodelled

/// The flags of a `send(2)` this kernel models.
type internal SendFlags =
    {
        /// `MSG_DONTWAIT`, which only Linux's `send(2)` honours.
        DontWait : bool
        /// `MSG_NOSIGNAL`: an `EPIPE` raises no `SIGPIPE`.
        NoSignal : bool
    }

[<RequireQualifiedAccess>]
module internal SendFlags =
    /// The flags of `word`, in `flavour`'s numbering; or every flag in it this
    /// kernel does not model for `send(2)`.
    let decode (flavour : SimulatedUnixFlavour) (word : int) : Result<SendFlags, MessageFlag list> =
        let flags = MessageFlag.decode flavour word

        let unmodelled =
            flags
            |> List.filter (fun flag ->
                match flag with
                | MessageFlag.DontWait
                | MessageFlag.NoSignal -> false
                | _ -> true
            )

        if unmodelled.IsEmpty then
            Ok
                {
                    DontWait = List.contains MessageFlag.DontWait flags
                    NoSignal = List.contains MessageFlag.NoSignal flags
                }
        else
            Error unmodelled
