namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// An error a call on a reset TCP connection fails with.
[<RequireQualifiedAccess>]
type internal TcpError =
    /// `ECONNRESET`.
    | ConnectionReset
    /// `EPIPE`. A write that fails with it raises `SIGPIPE` as well, unless
    /// the caller asked for no signal.
    | BrokenPipe

[<RequireQualifiedAccess>]
module internal TcpError =
    let toUnixError (error : TcpError) : UnixError =
        match error with
        | TcpError.ConnectionReset -> UnixError.ECONNRESET
        | TcpError.BrokenPipe -> UnixError.EPIPE

/// The FIN travelling in one direction of a TCP connection: the sending end's
/// `shutdown(SHUT_WR)`, or its close.
///
/// `passive` says whether the opposite FIN had already arrived when this one
/// was made, by the call or the close, whether it then went out or waited
/// behind bytes. Nothing else records which FIN was made first once both have
/// arrived, and with the FIN's progress it decides whether a closed end keeps
/// its endpoint.
[<RequireQualifiedAccess>]
type internal TcpFin =
    /// The sending end has not shut writing.
    | NotSent
    /// The sending end has shut writing, but its FIN waits in its send buffer
    /// behind bytes the receiving end's receive buffer has no room for yet.
    /// It arrives, and this becomes `Arrived`, when the last of them does.
    | Queued of passive : bool
    /// The FIN has arrived. Once the bytes already in the receive buffer are
    /// read, a read sees end of file.
    | Arrived of passive : bool

/// What one end of a TCP connection is: open, reset or closed. What it has
/// sent and received of the connection's FINs is the two directions'
/// `TcpFin`.
[<RequireQualifiedAccess>]
type internal TcpEndState =
    /// The end's socket is open and the connection has not been reset.
    /// `receiveShut` says whether the end's own `shutdown(SHUT_RD)` has been
    /// applied; the peer's FIN arriving shuts the receive side too, but is
    /// recorded in the inbound direction's `TcpFin`.
    | Open of receiveShut : bool
    /// The connection was reset. `errorPending` says whether the error still
    /// waits for a call to take it. Which error it is follows from the FINs:
    /// on Linux `EPIPE` when the peer's FIN had arrived and this end had made
    /// none (`tcp_reset` sets it in `CLOSE_WAIT`), and then reads see end of
    /// file rather than the error; `ECONNRESET` otherwise, and always on
    /// Darwin.
    | Reset of errorPending : bool
    /// This end's socket is closed: nothing reads here any more.
    | Closed

/// The bytes travelling towards one end of a TCP connection, and that end's
/// state.
///
/// The bytes are held in two stages, as two real kernels hold them: the
/// sender's send buffer and the receiver's receive buffer. Loopback has no
/// latency, so a byte moves on from the send buffer the moment the receive
/// buffer has room: bytes wait in `Sending` only while `Receiving` is full.
/// The exceptions are the states in which nothing receives any more, which
/// `TcpTransfer.violations` lists.
[<NoComparison>]
type internal TcpDirection =
    {
        /// Bytes the sending end has written that have not reached the
        /// receiving end's receive buffer: they are still in the sender's
        /// send buffer.
        Sending : ByteQueue
        /// Bytes in the receiving end's receive buffer, which a read takes
        /// from: what `FIONREAD` reports.
        Receiving : ByteQueue
        /// The sending end's send buffer, in bytes.
        SendCapacity : int
        /// The receiving end's receive buffer, in bytes.
        ReceiveCapacity : int
        /// The sending end's FIN.
        Fin : TcpFin
        /// The receiving end's state.
        Receiver : TcpEndState
    }

/// The state one flavour's transfer rules keep beyond the bytes.
[<RequireQualifiedAccess>]
type internal TcpTransferRules =
    /// Linux. `spaceWakeArmed` holds each end whose send buffer is marked
    /// out of space (`SOCK_NOSPACE`): a write met `EAGAIN` there, and when the
    /// send buffer next drains to two thirds full, the end gets one
    /// `TcpWake.SendSpace` and the mark clears.
    | Linux of spaceWakeArmed : Set<ConnectionEnd>
    /// Darwin, whose send-space wake keeps no state: every acknowledgement
    /// that frees send space raises it. `stranded` holds, for each sending
    /// end whose outbound direction a reset ended while bytes were still in
    /// its send buffer, how many: they are never delivered, but Darwin goes on
    /// counting them against the end's send space, where Linux discards them.
    /// An end's entry goes when it closes, and no entry is zero.
    | Darwin of stranded : Map<ConnectionEnd, int>

/// What travels over one TCP connection, in both directions, and the rules
/// of the flavour the connection was made under.
[<NoComparison>]
type internal TcpTransfer =
    internal
        {
            /// Bytes the server end has written towards the client end, and
            /// the client end's state.
            ToClient : TcpDirection
            /// Bytes the client end has written towards the server end, and
            /// the server end's state.
            ToServer : TcpDirection
            Rules : TcpTransferRules
        }

/// Which sides of its connection a `shutdown(2)` shuts: `SHUT_RD`, `SHUT_WR`
/// or `SHUT_RDWR`.
[<RequireQualifiedAccess>]
type internal TcpShutdownHow =
    /// `SHUT_RD`: the receive side.
    | Read
    /// `SHUT_WR`: the send side, which sends a FIN behind the bytes already
    /// written.
    | Write
    /// `SHUT_RDWR`: both.
    | Both

/// Something a transfer did that a waiter on one end could be woken by.
[<RequireQualifiedAccess>]
type internal TcpWake =
    /// Bytes entered `receiver`'s receive buffer. Raised on every arrival,
    /// even when bytes were already waiting.
    | DataArrived of receiver : ConnectionEnd
    /// `sender`'s send buffer gained space. On Darwin, raised whenever bytes
    /// leave it for the peer's receive buffer, which is when loopback
    /// acknowledges them; whether that makes the socket writable is the
    /// reader's question. On Linux, raised only once after a write met
    /// `EAGAIN`, when the send buffer has drained to two thirds full.
    | SendSpace of sender : ConnectionEnd
    /// `receiver`'s peer closed cleanly.
    | PeerFinished of receiver : ConnectionEnd
    /// `receiver`'s connection was reset.
    | PeerReset of receiver : ConnectionEnd
    /// `shutter`'s own `shutdown(2)` shut `how`: the sides it applied, which
    /// on Darwin may be fewer than it asked for. Linux raises it for every
    /// call on a connected socket, even one that changed nothing.
    | ShutDown of shutter : ConnectionEnd * how : TcpShutdownHow

/// What `shutdown(2)` on a connected TCP socket answers.
[<RequireQualifiedAccess>]
type internal TcpShutdownAnswer =
    /// 0.
    | Shut
    /// `ENOTCONN`.
    | NotConnected

/// A `shutdown(2)` this kernel refuses to answer, because what a real kernel
/// does next waits on a TCP timer.
[<RequireQualifiedAccess>]
type internal TcpShutdownRefusal =
    /// On Darwin, the call would shut `shutter`'s receive side while `unsent`
    /// bytes of its peer's still wait in the peer's send buffer, and it is not
    /// a `SHUT_RDWR` whose FIN goes out at once to a peer still able to send.
    /// Measured, the peer was reset only when a TCP timer fired, within 5 s
    /// but at a time that varied between runs (`tcp-shutdown.c`, section R),
    /// or, after `SHUT_RDWR` against a peer that had shut writing, not for 15
    /// s (section X). This kernel delivers bytes as soon as there is room, so
    /// it would reset at once.
    | DarwinReceiveShutBeforeUnsentBytes of shutter : ConnectionEnd * unsent : int

/// What a non-blocking read of a TCP connection answers.
[<RequireQualifiedAccess>]
type internal TcpReadAnswer =
    /// These bytes, which may be none: a request for zero bytes, or one on a
    /// Darwin connection that is open with nothing queued.
    | Bytes of bytes : ImmutableArray<byte>
    /// End of file: 0.
    | EndOfFile
    /// `EAGAIN`, or a blocking read's reason to sleep.
    | WouldBlock
    | Failed of error : TcpError

/// What a non-blocking write to a TCP connection answers.
[<RequireQualifiedAccess>]
type internal TcpWriteAnswer =
    /// The count the call returns.
    | Wrote of count : int
    /// `EAGAIN`, or a blocking write's reason to sleep.
    | WouldBlock
    | Failed of error : TcpError

/// Whether a write to a TCP connection reads the caller's buffer, which it
/// does only if bytes are taken: see `WriteAdmission`, whose contract this is.
[<RequireQualifiedAccess>]
type internal TcpWriteAdmission =
    /// Answered without the buffer being read.
    | Answered of answer : TcpWriteAnswer
    /// Pass exactly the first `count` bytes of the caller's buffer to
    /// `TcpTransfer.write`, which takes them all; the call returns `count`.
    | Take of count : int

/// The transfer rules of TCP connections over loopback, as pure functions over
/// a `TcpTransfer`. They count bytes, not segments, so where a real kernel
/// first answers `EAGAIN` depends here on the buffers' sizes alone; and a send
/// buffer drains only when the peer reads, never with time.
[<RequireQualifiedAccess>]
module internal TcpTransfer =

    /// The send buffer's low-water mark on Darwin, in bytes: a write that does
    /// not fit whole takes nothing unless at least this much fits (measured:
    /// writes of 1000 bytes or fewer were never short, and the smallest short
    /// write was 3160 bytes).
    let darwinSendLowWater : int = 2048

    let private otherEnd (connectionEnd : ConnectionEnd) : ConnectionEnd =
        match connectionEnd with
        | ConnectionEnd.Client -> ConnectionEnd.Server
        | ConnectionEnd.Server -> ConnectionEnd.Client

    /// A connection that has just completed under `flavour`: nothing queued
    /// either way, both ends open, and at each end a send buffer of
    /// `sendCapacity` bytes and a receive buffer of `receiveCapacity`.
    let create (flavour : SimulatedUnixFlavour) (sendCapacity : int) (receiveCapacity : int) : TcpTransfer =
        if sendCapacity <= 0 || receiveCapacity <= 0 then
            failwith
                $"TcpTransfer.create: a send buffer of %d{sendCapacity} bytes and a receive buffer of %d{receiveCapacity}, but each must hold at least one byte (this is a bug in this library)."

        let direction : TcpDirection =
            {
                Sending = ByteQueue.empty
                Receiving = ByteQueue.empty
                SendCapacity = sendCapacity
                ReceiveCapacity = receiveCapacity
                Fin = TcpFin.NotSent
                Receiver = TcpEndState.Open false
            }

        {
            ToClient = direction
            ToServer = direction
            Rules =
                match flavour with
                | SimulatedUnixFlavour.Linux -> TcpTransferRules.Linux Set.empty
                | SimulatedUnixFlavour.Darwin -> TcpTransferRules.Darwin Map.empty
        }

    /// The bytes travelling towards `receiver`, and its state.
    let towards (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpDirection =
        match receiver with
        | ConnectionEnd.Client -> transfer.ToClient
        | ConnectionEnd.Server -> transfer.ToServer

    /// Whether a reset has reached `receiver`, whether or not a call has since
    /// taken its error.
    let hasBeenReset (receiver : ConnectionEnd) (transfer : TcpTransfer) : bool =
        match (towards receiver transfer).Receiver with
        | TcpEndState.Reset _ -> true
        | TcpEndState.Open _
        | TcpEndState.Closed -> false

    let private withTowards
        (receiver : ConnectionEnd)
        (direction : TcpDirection)
        (transfer : TcpTransfer)
        : TcpTransfer
        =
        match receiver with
        | ConnectionEnd.Client ->
            { transfer with
                ToClient = direction
            }
        | ConnectionEnd.Server ->
            { transfer with
                ToServer = direction
            }

    /// How many bytes `receiver` could read now: what `FIONREAD` reports.
    let readable (receiver : ConnectionEnd) (transfer : TcpTransfer) : int =
        ByteQueue.length (towards receiver transfer).Receiving

    let private isLinux (transfer : TcpTransfer) : bool =
        match transfer.Rules with
        | TcpTransferRules.Linux _ -> true
        | TcpTransferRules.Darwin _ -> false

    /// How many bytes Darwin still counts in `sender`'s send buffer that a
    /// reset stranded there.
    let private strandedBy (sender : ConnectionEnd) (transfer : TcpTransfer) : int =
        match transfer.Rules with
        | TcpTransferRules.Linux _ -> 0
        | TcpTransferRules.Darwin stranded -> Map.tryFind sender stranded |> Option.defaultValue 0

    /// A reset strands `count` more bytes in `sender`'s send buffer: Darwin
    /// keeps counting them, and Linux discards them.
    let private strand (sender : ConnectionEnd) (count : int) (transfer : TcpTransfer) : TcpTransfer =
        match transfer.Rules with
        | TcpTransferRules.Linux _ -> transfer
        | TcpTransferRules.Darwin stranded ->
            if count = 0 then
                transfer
            else
                { transfer with
                    Rules = TcpTransferRules.Darwin (Map.add sender (strandedBy sender transfer + count) stranded)
                }

    /// `sender` has closed, so nothing counts its send buffer any more.
    let private forgetStranded (sender : ConnectionEnd) (transfer : TcpTransfer) : TcpTransfer =
        match transfer.Rules with
        | TcpTransferRules.Linux _ -> transfer
        | TcpTransferRules.Darwin stranded ->
            { transfer with
                Rules = TcpTransferRules.Darwin (Map.remove sender stranded)
            }

    /// Whether `receiver`'s receive side is shut: by its own
    /// `shutdown(SHUT_RD)`, or by its peer's FIN having arrived, as both
    /// kernels fold the two together (`RCV_SHUTDOWN`, `SS_CANTRCVMORE`). A
    /// reset end's is shut too, but the reset is the fact a caller reads
    /// there, so this answers only of an open end.
    let receiveShut (receiver : ConnectionEnd) (transfer : TcpTransfer) : bool =
        let inbound = towards receiver transfer

        match inbound.Receiver, inbound.Fin with
        | TcpEndState.Open true, _
        | TcpEndState.Open false, TcpFin.Arrived _ -> true
        | TcpEndState.Open false, TcpFin.NotSent
        | TcpEndState.Open false, TcpFin.Queued _
        | TcpEndState.Reset _, _
        | TcpEndState.Closed, _ -> false

    /// Whether `sender` has made its FIN, by `shutdown(SHUT_WR)` or by its
    /// close, whether or not the FIN has arrived: its send side is shut, and
    /// a write there answers `EPIPE`.
    let sendShut (sender : ConnectionEnd) (transfer : TcpTransfer) : bool =
        match (towards (otherEnd sender) transfer).Fin with
        | TcpFin.NotSent -> false
        | TcpFin.Queued _
        | TcpFin.Arrived _ -> true

    /// Whether a reset reaching `receiver` now finds the connection as
    /// Linux's `CLOSE_WAIT`: the peer's FIN has arrived, and `receiver` has
    /// made no FIN of its own.
    let private closeWaiting (receiver : ConnectionEnd) (transfer : TcpTransfer) : bool =
        match (towards receiver transfer).Fin with
        | TcpFin.Arrived _ -> not (sendShut receiver transfer)
        | TcpFin.NotSent
        | TcpFin.Queued _ -> false

    /// Whether both FINs have arrived: on Linux both ends are then in
    /// `TCP_CLOSE`, whether or not their sockets are open.
    let exchangeComplete (transfer : TcpTransfer) : bool =
        match transfer.ToClient.Fin, transfer.ToServer.Fin with
        | TcpFin.Arrived _, TcpFin.Arrived _ -> true
        | _ -> false

    /// Whether `sender`'s FIN was made after the peer's had arrived and has
    /// itself arrived: its end is done with the connection, and holds no
    /// `TIME_WAIT` (Linux's `LAST_ACK` ends in `TCP_CLOSE`). One made first
    /// waits in `TIME_WAIT` instead, and one still queued is not done.
    let passiveFinArrived (sender : ConnectionEnd) (transfer : TcpTransfer) : bool =
        match (towards (otherEnd sender) transfer).Fin with
        | TcpFin.Arrived true -> true
        | TcpFin.Arrived false
        | TcpFin.Queued _
        | TcpFin.NotSent -> false

    /// Every way `transfer` breaks the rules the functions here keep, as text;
    /// empty when it breaks none.
    let violations (transfer : TcpTransfer) : string list =
        let ofDirection (receiver : ConnectionEnd) : string list =
            let sender = otherEnd receiver
            let direction = towards receiver transfer
            let sending = ByteQueue.length direction.Sending
            let receiving = ByteQueue.length direction.Receiving
            let senderState = (towards sender transfer).Receiver

            [
                if direction.SendCapacity <= 0 || direction.ReceiveCapacity <= 0 then
                    $"towards %A{receiver}: capacities %d{direction.SendCapacity} and %d{direction.ReceiveCapacity} are not positive"
                if sending > direction.SendCapacity then
                    $"towards %A{receiver}: %d{sending} bytes in a send buffer of %d{direction.SendCapacity}"
                if receiving > direction.ReceiveCapacity then
                    $"towards %A{receiver}: %d{receiving} bytes in a receive buffer of %d{direction.ReceiveCapacity}"

                match direction.Receiver with
                | TcpEndState.Open receiveShut ->
                    if sending > 0 && receiving < direction.ReceiveCapacity then
                        $"towards %A{receiver}: %d{sending} bytes wait to be sent while the receive buffer has room"

                    // Darwin's `SHUT_RD` flushes the receive buffer, and any
                    // byte arriving after it resets the connection.
                    if receiveShut && not (isLinux transfer) && sending + receiving > 0 then
                        $"towards %A{receiver}: %d{sending + receiving} bytes on their way to a Darwin end that shut its receive side"
                | TcpEndState.Reset _ ->
                    if sending > 0 then
                        $"towards %A{receiver}: %d{sending} bytes in flight to an end that was reset"
                | TcpEndState.Closed ->
                    if receiving > 0 then
                        $"towards %A{receiver}: %d{receiving} bytes queued for a closed end"

                    if sending > 0 then
                        $"towards %A{receiver}: %d{sending} bytes kept in flight to a closed end"

                match direction.Fin, direction.Receiver with
                | TcpFin.Queued _, TcpEndState.Open _ ->
                    if sending = 0 then
                        $"towards %A{receiver}: a FIN is queued behind no bytes, so it has arrived"
                | TcpFin.Arrived _, _ ->
                    if sending > 0 then
                        $"towards %A{receiver}: a FIN has arrived ahead of %d{sending} bytes sent before it"
                | TcpFin.Queued _, TcpEndState.Reset _
                | TcpFin.Queued _, TcpEndState.Closed
                | TcpFin.NotSent, _ -> ()

                match direction.Fin with
                | TcpFin.Queued true
                | TcpFin.Arrived true ->
                    match (towards sender transfer).Fin with
                    | TcpFin.Arrived _ -> ()
                    | other ->
                        $"towards %A{receiver}: a FIN made after the opposite one had arrived, but that one is %A{other}"
                | TcpFin.Queued false
                | TcpFin.Arrived false
                | TcpFin.NotSent -> ()

                // A reset reaches both ends at once, unless one of them has
                // closed; and a close tells the peer, by a FIN or a reset.
                match direction.Receiver, senderState with
                | TcpEndState.Reset _, TcpEndState.Open _ -> $"%A{receiver} was reset while its peer is open"
                | TcpEndState.Closed, TcpEndState.Open _ when (towards sender transfer).Fin = TcpFin.NotSent ->
                    $"%A{receiver} is closed while its peer has not been told"
                | _ -> ()
            ]

        let ofRules : string list =
            match transfer.Rules with
            | TcpTransferRules.Linux armed ->
                [
                    for sender in armed do
                        match (towards sender transfer).Receiver with
                        | TcpEndState.Open _ -> ()
                        | other -> $"%A{sender}'s send-space wake is armed, but it is in %A{other} and cannot write"
                ]
            | TcpTransferRules.Darwin stranded ->
                [
                    for KeyValue (sender, count) in stranded do
                        let outbound = towards (otherEnd sender) transfer

                        if count <= 0 then
                            $"%A{sender} has %d{count} bytes stranded, but an entry is never kept for none"

                        if count > outbound.SendCapacity then
                            $"%A{sender} has %d{count} bytes stranded in a send buffer of %d{outbound.SendCapacity}"

                        match outbound.Receiver with
                        | TcpEndState.Reset _
                        | TcpEndState.Closed -> ()
                        | TcpEndState.Open _ ->
                            $"%A{sender} has %d{count} bytes stranded, but its peer is open to receive them"
                ]

        ofDirection ConnectionEnd.Client @ ofDirection ConnectionEnd.Server @ ofRules

    let private keepingRules (context : string) (transfer : TcpTransfer) : TcpTransfer =
        match violations transfer with
        | [] -> transfer
        | broken ->
            let broken = String.concat "; " broken

            failwith
                $"TcpTransfer.%s{context}: the transfer breaks its rules: %s{broken} (this is a bug in this library)."

    let private isArmed (sender : ConnectionEnd) (transfer : TcpTransfer) : bool =
        match transfer.Rules with
        | TcpTransferRules.Linux armed -> Set.contains sender armed
        | TcpTransferRules.Darwin _ -> false

    let private withArmed (sender : ConnectionEnd) (armed : bool) (transfer : TcpTransfer) : TcpTransfer =
        match transfer.Rules with
        | TcpTransferRules.Darwin _ -> transfer
        | TcpTransferRules.Linux set ->
            { transfer with
                Rules = TcpTransferRules.Linux (if armed then Set.add sender set else Set.remove sender set)
            }

    // `sk_stream_is_writeable`: the free space is at least half what is
    // queued, so the queue is at most two thirds of the buffer.
    let private linuxWritable (direction : TcpDirection) : bool =
        let queued = ByteQueue.length direction.Sending
        direction.SendCapacity - queued >= queued / 2

    /// The free space in `sender`'s send buffer, in bytes: what Darwin's
    /// `EVFILT_WRITE` reports as its event's `data`. Bytes a reset stranded
    /// there on Darwin still take space.
    let sendSpace (sender : ConnectionEnd) (transfer : TcpTransfer) : int =
        let outbound = towards (otherEnd sender) transfer

        outbound.SendCapacity
        - ByteQueue.length outbound.Sending
        - strandedBy sender transfer

    /// How many bytes `sender` has written that are still in its send buffer,
    /// because the peer's receive buffer has had no room for them yet. Bytes a
    /// reset stranded there, which will never be delivered, are not counted.
    let unsent (sender : ConnectionEnd) (transfer : TcpTransfer) : int =
        ByteQueue.length (towards (otherEnd sender) transfer).Sending

    /// Whether Linux's `tcp_poll` finds `sender` writable: its send buffer at
    /// most two thirds full (`sk_stream_is_writeable`). Says nothing of an end
    /// that was reset, which is writable whatever its buffer holds, since the
    /// write fails at once, nor of one that shut its send side.
    let linuxSendable (sender : ConnectionEnd) (transfer : TcpTransfer) : bool =
        linuxWritable (towards (otherEnd sender) transfer)

    /// The error a pending one would make a call by `receiver` fail with.
    let private resetError (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpError =
        if isLinux transfer && closeWaiting receiver transfer then
            TcpError.BrokenPipe
        else
            TcpError.ConnectionReset

    /// The error a call on `receiver`'s socket would take now, if one is
    /// pending.
    let pendingError (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpError option =
        match (towards receiver transfer).Receiver with
        | TcpEndState.Reset true -> Some (resetError receiver transfer)
        | TcpEndState.Reset false
        | TcpEndState.Open _
        | TcpEndState.Closed -> None

    /// A Linux `tcp_poll` of `sender`'s socket, by `poll(2)`, an epoll `ADD`
    /// or `MOD`, or `epoll_wait`'s re-poll of a pending entry: one that finds
    /// the socket open for sending but not writable marks it out of space
    /// (`SOCK_NOSPACE`), so that its send buffer draining to two thirds full
    /// raises `TcpWake.SendSpace`. A socket whose send side is shut is
    /// writable whatever its buffer holds, so its poll marks nothing. Darwin's
    /// poll keeps no such mark.
    let polled (sender : ConnectionEnd) (transfer : TcpTransfer) : TcpTransfer =
        match transfer.Rules, (towards sender transfer).Receiver with
        | TcpTransferRules.Darwin _, _ -> transfer
        | TcpTransferRules.Linux _, TcpEndState.Open _ ->
            if sendShut sender transfer || linuxSendable sender transfer then
                transfer
            else
                withArmed sender true transfer
        | TcpTransferRules.Linux _, TcpEndState.Reset _
        | TcpTransferRules.Linux _, TcpEndState.Closed -> transfer

    /// Whether bytes reaching `receiver` now make its kernel reset the
    /// connection rather than queue them: on Darwin once its receive side is
    /// shut, and on Linux once its send side is shut too
    /// (`tcp_rcv_state_process`'s `TCPABORTONDATA`, in `FIN_WAIT1` and
    /// `FIN_WAIT2`).
    let private resetsOnArrival (receiver : ConnectionEnd) (transfer : TcpTransfer) : bool =
        receiveShut receiver transfer
        && (not (isLinux transfer) || sendShut receiver transfer)

    /// Bytes reached `receiver`, whose kernel answered with a reset, which
    /// reaches both ends. What was in flight either way is discarded on Linux
    /// and stays counted in its sender's send buffer on Darwin, the bytes
    /// that provoked the reset included; what each end's receive buffer holds
    /// stays readable. The sender gets the error, and so does the receiver on
    /// Linux; Darwin's receiver sees none (measured, `tcp-shutdown.c`
    /// sections S and R). A sender that has closed stays closed: the reset
    /// only ends what it still had to send.
    let private resetByArrival (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
        let sender = otherEnd receiver
        let inbound = towards receiver transfer
        let outbound = towards sender transfer

        let senderClosed =
            match outbound.Receiver with
            | TcpEndState.Closed -> true
            | TcpEndState.Open _
            | TcpEndState.Reset _ -> false

        let transfer =
            transfer
            |> (if senderClosed then
                    id
                else
                    strand sender (ByteQueue.length inbound.Sending))
            |> strand receiver (ByteQueue.length outbound.Sending)
            |> withTowards
                receiver
                { inbound with
                    Sending = ByteQueue.empty
                    Receiver = TcpEndState.Reset (isLinux transfer)
                }
            |> withTowards
                sender
                { outbound with
                    Sending = ByteQueue.empty
                    Receiver =
                        if senderClosed then
                            TcpEndState.Closed
                        else
                            TcpEndState.Reset true
                }
            |> withArmed receiver false
            |> withArmed sender false

        let wakes =
            [
                if not senderClosed then
                    TcpWake.PeerReset sender
                TcpWake.PeerReset receiver
            ]

        wakes, transfer

    /// Bytes move on towards `receiver` from the send buffer into the receive
    /// buffer as far as it has room, unless they would reach an end that
    /// resets on their arrival, which then resets the connection. How many
    /// moved, the wakes a reset raises, and the transfer after.
    let private arrive (receiver : ConnectionEnd) (transfer : TcpTransfer) : int * TcpWake list * TcpTransfer =
        let direction = towards receiver transfer
        let room = direction.ReceiveCapacity - ByteQueue.length direction.Receiving
        let moved = min room (ByteQueue.length direction.Sending)

        if moved = 0 then
            0, [], transfer
        elif resetsOnArrival receiver transfer then
            let wakes, transfer = resetByArrival receiver transfer
            0, wakes, transfer
        else

        let bytes, sending = ByteQueue.take moved direction.Sending

        moved,
        [],
        withTowards
            receiver
            { direction with
                Sending = sending
                Receiving = ByteQueue.append bytes direction.Receiving
            }
            transfer

    /// The wakes owed to `sender` now that `moved` bytes have left its send
    /// buffer for the peer's receive buffer.
    let private spaceFreed
        (sender : ConnectionEnd)
        (moved : int)
        (transfer : TcpTransfer)
        : TcpWake list * TcpTransfer
        =
        match (towards sender transfer).Receiver with
        | TcpEndState.Closed
        | TcpEndState.Reset _ -> [], transfer
        | TcpEndState.Open _ ->

        match transfer.Rules with
        | TcpTransferRules.Darwin _ -> (if moved > 0 then [ TcpWake.SendSpace sender ] else []), transfer
        | TcpTransferRules.Linux _ ->
            if isArmed sender transfer && linuxWritable (towards (otherEnd sender) transfer) then
                [ TcpWake.SendSpace sender ], withArmed sender false transfer
            else
                [], transfer

    /// How many bytes a write of `count` takes when `space` bytes are free.
    let private taking (transfer : TcpTransfer) (count : int) (space : int64) : int option =
        match transfer.Rules with
        // Linux takes any positive remainder.
        | TcpTransferRules.Linux _ ->
            if space > 0L then
                Some (int (min (int64 count) space))
            else
                None
        | TcpTransferRules.Darwin _ ->
            if int64 count <= space then Some count
            elif space >= int64 darwinSendLowWater then Some (int space)
            else None

    /// The room a write by `writer`, whose end may still send, has: the free
    /// space in its send buffer, and in the peer's receive buffer while the
    /// peer is there to drain it.
    let private writeSpace (context : string) (writer : ConnectionEnd) (transfer : TcpTransfer) : int64 =
        let peer = otherEnd writer
        let outbound = towards peer transfer

        if sendShut writer transfer then
            failwith
                $"TcpTransfer.%s{context}: the %A{writer} end has shut its send side, but it is still writing (this is a bug in this library)."

        match outbound.Receiver with
        // A peer that answers an arrival with a reset acknowledges nothing,
        // so a write can fill the send buffer and no more.
        | TcpEndState.Open _ when resetsOnArrival peer transfer -> int64 (sendSpace writer transfer)
        | TcpEndState.Open _ ->
            int64 (outbound.ReceiveCapacity - ByteQueue.length outbound.Receiving)
            + int64 (sendSpace writer transfer)
        // The peer has gone, so nothing drains the send buffer.
        | TcpEndState.Closed -> int64 (sendSpace writer transfer)
        | TcpEndState.Reset _ ->
            failwith
                $"TcpTransfer.%s{context}: the %A{writer} end's peer was reset while the %A{writer} end was not, which TcpTransfer.violations forbids (this is a bug in this library)."

    /// Whether a read by `receiver`, asleep because nothing was there to
    /// answer it, has an answer now: bytes have arrived, or its receive side
    /// is shut (by its peer's FIN or its own `SHUT_RD`), or a reset. A read
    /// wakes for any of them, needing only one byte to return (measured,
    /// `tcp-blocking.c` sections R-data, R-fin and R-reset).
    let readAnswers (receiver : ConnectionEnd) (transfer : TcpTransfer) : bool =
        let direction = towards receiver transfer

        match direction.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.readAnswers: the %A{receiver} end is closed, so nothing can be asleep reading there (this is a bug in this library)."
        | TcpEndState.Reset _ -> true
        | TcpEndState.Open _ -> receiveShut receiver transfer || ByteQueue.length direction.Receiving > 0

    /// Whether a write by `writer`, asleep with `remaining` of its bytes not
    /// yet taken, is woken: by a reset at either flavour, or by its own
    /// end's send side being shut, and otherwise by room. On Linux a sleeping
    /// writer is woken only once its send buffer has drained to two thirds
    /// full (`sk_stream_write_space`), as an edge-triggered waiter is, and
    /// has room; on Darwin, at every acknowledgement that leaves room for a
    /// write of `remaining` to take something, by the low water mark
    /// `admitWrite` applies.
    ///
    /// Measured (`tcp-blocking.c` section W-resume): Linux's writer, asleep
    /// with 4194304 bytes queued, took nothing while the reader drained the
    /// queue to 2765702, and refilled it to the full as it fell to two
    /// thirds; Darwin's took the room as each read made it.
    let writeResumes (writer : ConnectionEnd) (remaining : int) (transfer : TcpTransfer) : bool =
        if remaining <= 0 then
            failwith
                $"TcpTransfer.writeResumes: a write asleep with %d{remaining} bytes left, but a write with nothing left has returned (this is a bug in this library)."

        match (towards writer transfer).Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.writeResumes: the %A{writer} end is closed, so nothing can be asleep writing there (this is a bug in this library)."
        | TcpEndState.Reset _ -> true
        | TcpEndState.Open _ when sendShut writer transfer -> true
        | TcpEndState.Open _ ->

        match transfer.Rules with
        // The woken writer goes on only if the buffer is not full
        // (`sk_stream_memory_free`), which a buffer of one byte holding one is
        // while two thirds full.
        | TcpTransferRules.Linux _ ->
            linuxWritable (towards (otherEnd writer) transfer)
            && writeSpace "writeResumes" writer transfer > 0L
        | TcpTransferRules.Darwin _ -> (taking transfer remaining (writeSpace "writeResumes" writer transfer)).IsSome

    /// What a write of `count` bytes by `writer` decides before the caller's
    /// buffer is read, and the transfer after: a failed write takes a pending
    /// error on Linux, and a Linux write that runs out of space, whether it
    /// meets `EAGAIN` or takes only part of its bytes, arms the writer's
    /// send-space wake (`SOCK_NOSPACE`).
    ///
    /// A reset end answers by the reset's rules. Otherwise an end whose send
    /// side is shut answers `EPIPE`, even to a write of nothing (measured on
    /// both, `tcp-shutdown.c` section S).
    let admitWrite (writer : ConnectionEnd) (count : int) (transfer : TcpTransfer) : TcpWriteAdmission * TcpTransfer =
        if count < 0 then
            failwith $"TcpTransfer.admitWrite: a write of %d{count} bytes (this is a bug in this library)."

        let own = towards writer transfer

        match own.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.admitWrite: the %A{writer} end is closed, so nothing can write there (this is a bug in this library)."
        | TcpEndState.Reset errorPending ->
            match transfer.Rules with
            | TcpTransferRules.Linux _ ->
                // `sk_stream_error`: the pending error if there is one, which
                // the write takes, and `EPIPE` once it is gone.
                if errorPending then
                    TcpWriteAdmission.Answered (TcpWriteAnswer.Failed (resetError writer transfer)),
                    withTowards
                        writer
                        { own with
                            Receiver = TcpEndState.Reset false
                        }
                        transfer
                else
                    TcpWriteAdmission.Answered (TcpWriteAnswer.Failed TcpError.BrokenPipe), transfer
            // `sosend` answers `EPIPE` once it cannot send more, without
            // reading the pending error.
            | TcpTransferRules.Darwin _ ->
                TcpWriteAdmission.Answered (TcpWriteAnswer.Failed TcpError.BrokenPipe), transfer
        | TcpEndState.Open _ when sendShut writer transfer ->
            TcpWriteAdmission.Answered (TcpWriteAnswer.Failed TcpError.BrokenPipe), transfer
        | TcpEndState.Open _ ->

        if count = 0 then
            TcpWriteAdmission.Answered (TcpWriteAnswer.Wrote 0), transfer
        else

        match taking transfer count (writeSpace "admitWrite" writer transfer) with
        // `tcp_sendmsg` marks the socket out of space before it returns a
        // short count, as before it answers `EAGAIN`.
        | Some taken when taken < count -> TcpWriteAdmission.Take taken, withArmed writer true transfer
        | Some taken -> TcpWriteAdmission.Take taken, transfer
        | None -> TcpWriteAdmission.Answered TcpWriteAnswer.WouldBlock, withArmed writer true transfer

    /// `writer` writes `bytes`, which must be exactly what `admitWrite`
    /// decided to take; the write returns their count. Returns the wakes the
    /// write raises, and the transfer after.
    ///
    /// When the peer has closed, the bytes are taken and never delivered, and
    /// the peer's kernel answers with a reset: Linux discards them, and Darwin
    /// keeps counting them in the writer's send buffer. When they reach a
    /// peer that resets on their arrival (`resetsOnArrival`), they are taken
    /// and the connection is reset in the same way.
    let write
        (writer : ConnectionEnd)
        (bytes : ImmutableArray<byte>)
        (transfer : TcpTransfer)
        : TcpWake list * TcpTransfer
        =
        match fst (admitWrite writer bytes.Length transfer) with
        | TcpWriteAdmission.Take count when count = bytes.Length -> ()
        | other ->
            failwith
                $"TcpTransfer.write: %d{bytes.Length} bytes passed where admitWrite decided %A{other} (this is a bug in the caller)."

        let own = towards writer transfer
        let peer = otherEnd writer
        let outbound = towards peer transfer

        match outbound.Receiver with
        | TcpEndState.Closed ->
            // The reset also ends what the closed peer still had in flight
            // to the writer; what already arrived stays readable.
            let transfer =
                transfer
                |> withTowards
                    writer
                    { own with
                        Sending = ByteQueue.empty
                        Receiver = TcpEndState.Reset true
                    }
                |> strand writer bytes.Length
                |> withArmed writer false

            [ TcpWake.PeerReset writer ], keepingRules "write" transfer
        | TcpEndState.Reset _ ->
            failwith
                $"TcpTransfer.write: the %A{writer} end's peer was reset while the %A{writer} end was not, which TcpTransfer.violations forbids (this is a bug in this library)."
        | TcpEndState.Open _ ->

        let transfer =
            withTowards
                peer
                { outbound with
                    Sending = ByteQueue.append bytes outbound.Sending
                }
                transfer

        let arrived, resetWakes, transfer = arrive peer transfer

        let wakes =
            [
                if arrived > 0 then
                    TcpWake.DataArrived peer

                    match transfer.Rules with
                    | TcpTransferRules.Darwin _ -> TcpWake.SendSpace writer
                    | TcpTransferRules.Linux _ -> ()
                yield! resetWakes
            ]

        wakes, keepingRules "write" transfer

    /// What a non-blocking `call` asking for `count` bytes from `receiver`
    /// answers, the wakes it raises, and the transfer after.
    ///
    /// With nothing queued, a read of an end whose receive side is shut
    /// answers end of file, never `EAGAIN`. Linux keeps what was queued when
    /// the end shut its receive side, and a read takes it first; Darwin's
    /// `SHUT_RD` discarded it (`shutdown`).
    let read
        (receiver : ConnectionEnd)
        (call : TcpReceiveCall)
        (count : int)
        (transfer : TcpTransfer)
        : TcpReadAnswer * TcpWake list * TcpTransfer
        =
        if count < 0 then
            failwith $"TcpTransfer.read: a read of %d{count} bytes (this is a bug in this library)."

        let direction = towards receiver transfer

        match direction.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.read: the %A{receiver} end is closed, so nothing can read there (this is a bug in this library)."
        | TcpEndState.Open _
        | TcpEndState.Reset _ -> ()

        let takeError () : TcpTransfer =
            match direction.Receiver with
            | TcpEndState.Reset true ->
                withTowards
                    receiver
                    { direction with
                        Receiver = TcpEndState.Reset false
                    }
                    transfer
            | TcpEndState.Reset false
            | TcpEndState.Open _
            | TcpEndState.Closed -> transfer

        let queued = ByteQueue.length direction.Receiving

        let unqueuedFin : string =
            $"TcpTransfer.read: the %A{receiver} end's FIN waits behind bytes in flight, but its receive buffer is empty, which TcpTransfer.violations forbids (this is a bug in this library)."

        // Linux's `read(2)` answers a zero-length request before it reaches
        // the socket.
        if isLinux transfer && count = 0 && call = TcpReceiveCall.Read then
            TcpReadAnswer.Bytes ImmutableArray.Empty, [], transfer
        elif queued > 0 then
            let taken = min count queued

            match call with
            | TcpReceiveCall.Peek -> TcpReadAnswer.Bytes (ByteQueue.peek taken direction.Receiving), [], transfer
            | TcpReceiveCall.Read
            | TcpReceiveCall.Receive ->
                let bytes, receiving = ByteQueue.take taken direction.Receiving

                let moved, resetWakes, transfer =
                    arrive
                        receiver
                        (withTowards
                            receiver
                            { direction with
                                Receiving = receiving
                            }
                            transfer)

                // A FIN queued behind the bytes arrives with the last of them.
                let direction = towards receiver transfer

                let transfer, finArrives =
                    match direction.Fin, direction.Receiver with
                    | TcpFin.Queued passive, TcpEndState.Open _ when ByteQueue.length direction.Sending = 0 ->
                        withTowards
                            receiver
                            { direction with
                                Fin = TcpFin.Arrived passive
                            }
                            transfer,
                        true
                    | _ -> transfer, false

                let sender = otherEnd receiver
                let spaceWakes, transfer = spaceFreed sender moved transfer

                let wakes =
                    [
                        if moved > 0 then
                            TcpWake.DataArrived receiver
                        if finArrives then
                            TcpWake.PeerFinished receiver
                        yield! spaceWakes
                        yield! resetWakes
                    ]

                TcpReadAnswer.Bytes bytes, wakes, keepingRules "read" transfer
        elif isLinux transfer then
            // `tcp_recvmsg` tests for a shut receive side (a received FIN, or
            // `SHUT_RD`) before the pending error, and `sock_error` takes the
            // error even under `MSG_PEEK`.
            match direction.Receiver, direction.Fin with
            | TcpEndState.Open _, _ when receiveShut receiver transfer -> TcpReadAnswer.EndOfFile, [], transfer
            | TcpEndState.Open _, TcpFin.Queued _ -> failwith unqueuedFin
            | TcpEndState.Open _, _ -> TcpReadAnswer.WouldBlock, [], transfer
            // `SOCK_DONE`, which the peer's FIN set, comes before the error
            // whatever this end has sent (`tcp-shutdown-exchange.c`, Q).
            | TcpEndState.Reset errorPending, fin ->
                let finArrived =
                    match fin with
                    | TcpFin.Arrived _ -> true
                    | TcpFin.NotSent
                    | TcpFin.Queued _ -> false

                if finArrived || not errorPending then
                    TcpReadAnswer.EndOfFile, [], transfer
                else
                    TcpReadAnswer.Failed (resetError receiver transfer), [], takeError ()
            | TcpEndState.Closed, _ -> failwith "TcpTransfer.read: unreachable, the closed end was refused above."
        else
            // `soreceive` takes the pending error unless peeking, and a
            // zero-length request with nothing queued answers 0 rather than
            // waiting.
            match direction.Receiver, direction.Fin with
            | TcpEndState.Reset true, _ ->
                let transfer =
                    match call with
                    | TcpReceiveCall.Peek -> transfer
                    | TcpReceiveCall.Read
                    | TcpReceiveCall.Receive -> takeError ()

                TcpReadAnswer.Failed (resetError receiver transfer), [], transfer
            | TcpEndState.Reset false, _ -> TcpReadAnswer.EndOfFile, [], transfer
            | TcpEndState.Open _, _ when receiveShut receiver transfer -> TcpReadAnswer.EndOfFile, [], transfer
            | TcpEndState.Open _, TcpFin.Queued _ -> failwith unqueuedFin
            | TcpEndState.Open _, _ ->
                if count = 0 then
                    TcpReadAnswer.Bytes ImmutableArray.Empty, [], transfer
                else
                    TcpReadAnswer.WouldBlock, [], transfer
            | TcpEndState.Closed, _ -> failwith "TcpTransfer.read: unreachable, the closed end was refused above."

    /// `getsockopt(SO_ERROR)` on `receiver`'s socket: the pending error, which
    /// the call takes, and the transfer after.
    let takeError (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpError option * TcpTransfer =
        let direction = towards receiver transfer

        match direction.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.takeError: the %A{receiver} end is closed, so it has no socket to ask (this is a bug in this library)."
        | TcpEndState.Reset true ->
            Some (resetError receiver transfer),
            withTowards
                receiver
                { direction with
                    Receiver = TcpEndState.Reset false
                }
                transfer
        | TcpEndState.Open _
        | TcpEndState.Reset false -> None, transfer

    /// `sender` makes its FIN, which follows whatever it still has to send,
    /// and the wakes its arrival raises. Its send side must not be shut yet.
    let private makeFin (sender : ConnectionEnd) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
        let peer = otherEnd sender
        let outbound = towards peer transfer

        if sendShut sender transfer then
            failwith
                $"TcpTransfer.makeFin: the %A{sender} end has made its FIN already (this is a bug in this library)."

        let passive =
            match (towards sender transfer).Fin with
            | TcpFin.Arrived _ -> true
            | TcpFin.NotSent
            | TcpFin.Queued _ -> false

        let fin =
            if ByteQueue.length outbound.Sending > 0 then
                TcpFin.Queued passive
            else
                TcpFin.Arrived passive

        let wakes =
            match fin, outbound.Receiver with
            | TcpFin.Arrived _, TcpEndState.Open _ -> [ TcpWake.PeerFinished peer ]
            | _ -> []

        wakes,
        withTowards
            peer
            { outbound with
                Fin = fin
            }
            transfer

    /// Whether `shutdown(2)` of `how` by `shutter` is one this kernel refuses
    /// (`TcpShutdownRefusal`), and why.
    let private shutdownRefusal
        (shutter : ConnectionEnd)
        (how : TcpShutdownHow)
        (transfer : TcpTransfer)
        : TcpShutdownRefusal option
        =
        let inbound = towards shutter transfer
        let waiting = ByteQueue.length inbound.Sending

        let receiveHalf =
            match how with
            | TcpShutdownHow.Read
            | TcpShutdownHow.Both -> true
            | TcpShutdownHow.Write -> false

        // A `SHUT_RDWR` whose FIN goes out at once, to a peer that has not
        // shut its send side, reset that peer at once (measured, section R,
        // `RDWR p-unsent`): the FIN carries the room the flush made.
        let resetsAtOnce =
            how = TcpShutdownHow.Both
            && not (sendShut shutter transfer)
            && unsent shutter transfer = 0
            && inbound.Fin = TcpFin.NotSent

        match transfer.Rules, inbound.Receiver with
        | TcpTransferRules.Darwin _, TcpEndState.Open _ when
            receiveHalf
            && not (receiveShut shutter transfer)
            && waiting > 0
            && not resetsAtOnce
            ->
            Some (TcpShutdownRefusal.DarwinReceiveShutBeforeUnsentBytes (shutter, waiting))
        | _ -> None

    /// `shutter`'s socket, an end of an established connection, calls
    /// `shutdown(how)`: what it answers, the wakes it raises, and the
    /// transfer after; or why this kernel refuses it.
    ///
    /// Linux applies every side it is asked to, again or not, and answers 0,
    /// unless the connection was reset, or both FINs have arrived, when it
    /// answers `ENOTCONN` (and a reset's error stays pending). Darwin applies `SHUT_RDWR` side by side, as XNU's
    /// `soshutdownlock` does: the receive side fails with `ENOTCONN` if it is
    /// already shut (by an earlier `SHUT_RD`, the peer's FIN, or a reset),
    /// and then nothing else is applied; the send side likewise, after the
    /// receive side has been applied (measured, `tcp-shutdown.c` sections S
    /// and T).
    ///
    /// Shutting the send side makes the FIN, behind whatever is still to be
    /// sent. Shutting the receive side leaves Linux receiving as before, but
    /// an arrival at an end shut both ways resets the connection; Darwin
    /// discards what is queued, and any arrival afterwards resets. Every call
    /// raises `TcpWake.ShutDown` for the sides it applied (on Linux, for every
    /// call), and the peer's wakes for what reached it.
    let shutdown
        (shutter : ConnectionEnd)
        (how : TcpShutdownHow)
        (transfer : TcpTransfer)
        : Result<TcpShutdownAnswer * TcpWake list * TcpTransfer, TcpShutdownRefusal>
        =
        let inbound = towards shutter transfer

        let receiveHalf, sendHalf =
            match how with
            | TcpShutdownHow.Read -> true, false
            | TcpShutdownHow.Write -> false, true
            | TcpShutdownHow.Both -> true, true

        match inbound.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.shutdown: the %A{shutter} end is closed, so it has no socket to shut (this is a bug in this library)."
        // `inet_shutdown` answers `ENOTCONN` in `TCP_CLOSE`, but still wakes
        // the socket's waiters; `soshutdownlock` finds both sides shut.
        | TcpEndState.Reset _ ->
            let wakes =
                match transfer.Rules with
                | TcpTransferRules.Linux _ -> [ TcpWake.ShutDown (shutter, how) ]
                | TcpTransferRules.Darwin _ -> []

            Ok (TcpShutdownAnswer.NotConnected, wakes, transfer)
        // Once both FINs have arrived, a Linux socket is in `TCP_CLOSE` too
        // (measured, `tcp-shutdown-exchange.c` section K). Darwin's answer
        // follows from its rule: both sides are already shut.
        | TcpEndState.Open _ when isLinux transfer && exchangeComplete transfer ->
            Ok (TcpShutdownAnswer.NotConnected, [ TcpWake.ShutDown (shutter, how) ], transfer)
        | TcpEndState.Open _ ->

        match shutdownRefusal shutter how transfer with
        | Some refusal -> Error refusal
        | None ->

        let shutReceive (flush : bool) (transfer : TcpTransfer) : TcpTransfer =
            let inbound = towards shutter transfer

            withTowards
                shutter
                { inbound with
                    Receiving = if flush then ByteQueue.empty else inbound.Receiving
                    Receiver = TcpEndState.Open true
                }
                transfer

        match transfer.Rules with
        | TcpTransferRules.Linux _ ->
            let transfer = if receiveHalf then shutReceive false transfer else transfer

            let finWakes, transfer =
                if sendHalf && not (sendShut shutter transfer) then
                    makeFin shutter transfer
                else
                    [], transfer

            Ok (TcpShutdownAnswer.Shut, TcpWake.ShutDown (shutter, how) :: finWakes, keepingRules "shutdown" transfer)
        | TcpTransferRules.Darwin _ ->

        if receiveHalf && receiveShut shutter transfer then
            Ok (TcpShutdownAnswer.NotConnected, [], transfer)
        else

        let transfer = if receiveHalf then shutReceive true transfer else transfer
        let sendFails = sendHalf && sendShut shutter transfer
        let sendApplied = sendHalf && not sendFails

        let finWakes, transfer =
            if sendApplied then
                makeFin shutter transfer
            else
                [], transfer
        // Bytes still on their way arrive at an end that cannot take them.
        let _, resetWakes, transfer = arrive shutter transfer

        let applied =
            match receiveHalf, sendApplied with
            | true, true -> [ TcpWake.ShutDown (shutter, TcpShutdownHow.Both) ]
            | true, false -> [ TcpWake.ShutDown (shutter, TcpShutdownHow.Read) ]
            | false, true -> [ TcpWake.ShutDown (shutter, TcpShutdownHow.Write) ]
            | false, false -> []

        let answer =
            if sendFails then
                TcpShutdownAnswer.NotConnected
            else
                TcpShutdownAnswer.Shut

        Ok (answer, applied @ finWakes @ resetWakes, keepingRules "shutdown" transfer)

    /// Whether this kernel refuses `closer`'s close under `SO_LINGER` {1, 0},
    /// which `abort` must not then be asked to make.
    ///
    /// On Darwin, when `closer` made its FIN before the peer's arrived, the
    /// FIN still waits behind its bytes, and the peer's has arrived, no reset
    /// is sent at the close: the peer reads what it holds and then
    /// `ECONNRESET`, once its reads open the window towards a socket that has
    /// gone (measured, `tcp-shutdown.c` section L, `cqueued-pfin`). This
    /// kernel has no reset owed to whoever next reads.
    let abortRefused (closer : ConnectionEnd) (transfer : TcpTransfer) : bool =
        let peer = otherEnd closer

        match transfer.Rules, (towards closer transfer).Receiver, (towards peer transfer).Receiver with
        | TcpTransferRules.Darwin _, TcpEndState.Open _, TcpEndState.Open _ ->
            match (towards peer transfer).Fin, (towards closer transfer).Fin with
            | TcpFin.Queued false, TcpFin.Arrived _ -> true
            | _ -> false
        | _ -> false

    /// Whether this kernel refuses `closer`'s ordinary close, which `close`
    /// must not then be asked to make.
    ///
    /// On Darwin, when `closer` leaves bytes unread, its own FIN still waits
    /// behind its bytes, and the peer's FIN has arrived, the close sends no
    /// reset, and neither the rest of its bytes nor its FIN reach the peer,
    /// which reads what it holds and then `EAGAIN`, still 2 s later
    /// (measured, `tcp-shutdown-exchange.c` section G): only a timer can end
    /// that.
    let closeRefused (closer : ConnectionEnd) (transfer : TcpTransfer) : bool =
        let peer = otherEnd closer
        let own = towards closer transfer

        match transfer.Rules, own.Receiver, (towards peer transfer).Receiver with
        | TcpTransferRules.Darwin _, TcpEndState.Open _, TcpEndState.Open _ ->
            match (towards peer transfer).Fin, own.Fin with
            | TcpFin.Queued _, TcpFin.Arrived _ -> ByteQueue.length own.Receiving > 0
            | _ -> false
        | _ -> false

    /// `closer`'s socket closes, sending a FIN unless it made one already, or
    /// a reset: if bytes are left unread (but not on Linux once both FINs
    /// have arrived, nor on Darwin once the closer's has arrived and the peer
    /// has made its own), or if `abortive` and the exchange of FINs is not
    /// complete.
    let private closeWith
        (context : string)
        (abortive : bool)
        (closer : ConnectionEnd)
        (transfer : TcpTransfer)
        : TcpWake list * TcpTransfer
        =
        let own = towards closer transfer
        let peer = otherEnd closer
        let outbound = towards peer transfer

        let transfer = transfer |> withArmed closer false |> forgetStranded closer

        let closedOwn : TcpDirection =
            { own with
                Sending = ByteQueue.empty
                Receiving = ByteQueue.empty
                Receiver = TcpEndState.Closed
            }

        match own.Receiver, outbound.Receiver with
        | TcpEndState.Closed, _ ->
            failwith $"TcpTransfer.%s{context}: the %A{closer} end is closed already (this is a bug in this library)."
        | _, TcpEndState.Closed ->
            // Both ends have now closed, and nothing is left to send or keep.
            let transfer =
                transfer
                |> withTowards closer closedOwn
                |> withTowards
                    peer
                    { outbound with
                        Sending = ByteQueue.empty
                    }

            [], keepingRules context transfer
        // The reset reached both ends, so there is nothing to tell the peer.
        | TcpEndState.Reset _, TcpEndState.Reset _ -> [], keepingRules context (withTowards closer closedOwn transfer)
        | TcpEndState.Reset _, TcpEndState.Open _
        | TcpEndState.Open _, TcpEndState.Reset _ ->
            failwith
                $"TcpTransfer.%s{context}: one end of the connection was reset and the other is open, which TcpTransfer.violations forbids (this is a bug in this library)."
        | TcpEndState.Open _, TcpEndState.Open _ ->

        if abortive && abortRefused closer transfer then
            failwith
                $"TcpTransfer.%s{context}: the %A{closer} end's close under linger zero is one TcpTransfer.abortRefused refuses (this is a bug in the caller)."

        if not abortive && closeRefused closer transfer then
            failwith
                $"TcpTransfer.%s{context}: the %A{closer} end's close is one TcpTransfer.closeRefused refuses (this is a bug in the caller)."

        // Bytes still on their way to the closer count as unread too, but
        // they wait only while its receive buffer is full, so that test
        // covers them.
        let unread = ByteQueue.length own.Receiving > 0

        // A close over unread bytes resets, except on Linux once both FINs
        // have arrived (`tcp_close` in `TCP_CLOSE`), and on Darwin once the
        // closer's FIN has arrived and the peer has made its own, arrived or
        // not (measured, `tcp-shutdown-exchange.c` sections U, Q, V, W and
        // G; on Darwin no reset came within 5 s).
        let closerFinArrived =
            match outbound.Fin with
            | TcpFin.Arrived _ -> true
            | TcpFin.NotSent
            | TcpFin.Queued _ -> false

        let unreadResets =
            unread
            && not (
                if isLinux transfer then
                    exchangeComplete transfer
                else
                    closerFinArrived && sendShut peer transfer
            )

        // Once both FINs have arrived, a close under linger zero is the
        // ordinary close (`tcp_disconnect` resets only before that).
        if unreadResets || (abortive && not (exchangeComplete transfer)) then
            // The peer keeps what is in its receive buffer, and loses what the
            // closer still had to send it. What the peer still had to send
            // the closer is discarded on Linux, and stays counted in the
            // peer's send buffer on Darwin. Whether the peer's error reads as
            // `EPIPE` on Linux follows from the FINs (`closeWaiting`).
            let transfer =
                transfer
                |> strand peer (ByteQueue.length own.Sending)
                |> withTowards closer closedOwn
                |> withTowards
                    peer
                    { outbound with
                        Sending = ByteQueue.empty
                        Receiver = TcpEndState.Reset true
                    }
                |> withArmed peer false

            [ TcpWake.PeerReset peer ], keepingRules context transfer
        else
            // What the closer had left to send keeps draining as the peer
            // reads, and the FIN follows it, unless the closer's
            // `shutdown(SHUT_WR)` made it already. What the peer still had
            // on its way to the closer will never arrive: Darwin goes on
            // counting it in the peer's send buffer.
            let wakes, transfer =
                if sendShut closer transfer then
                    [], transfer
                else
                    makeFin closer transfer

            let transfer =
                transfer
                |> strand peer (ByteQueue.length own.Sending)
                |> withTowards closer closedOwn

            wakes, keepingRules context transfer

    /// `closer`'s socket closes. With nothing unread, and nothing on its way
    /// to it, the close is a FIN, unless `shutdown(SHUT_WR)` made one
    /// already: the peer reads what was sent, then end of file. Otherwise it
    /// is a reset, and the peer gets a pending error; but once both FINs have
    /// arrived on Linux, or on Darwin once the closer's has arrived and the
    /// peer has made its own, bytes left unread are discarded without a
    /// reset. Not to be asked where `closeRefused` holds.
    let close (closer : ConnectionEnd) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
        closeWith "close" false closer transfer

    /// `closer`'s socket closes under `SO_LINGER` {1, 0}: a reset, unless
    /// both FINs have arrived, when it is the ordinary close (measured,
    /// `tcp-shutdown.c` section L). Not to be asked where `abortRefused`
    /// holds.
    let abort (closer : ConnectionEnd) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
        closeWith "abort" true closer transfer
