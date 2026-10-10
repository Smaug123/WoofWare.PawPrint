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

/// What one end of a TCP connection knows of the connection's ending.
///
/// The model has no `shutdown(2)`, so a FIN or a reset reaches an end only
/// because its peer has closed: either the peer closed cleanly, or it closed
/// with bytes unread, or this end wrote after the peer's clean close and the
/// peer's kernel answered with a reset.
[<RequireQualifiedAccess>]
type internal TcpEndState =
    /// The peer may still send.
    | Open
    /// The peer closed cleanly, but its FIN waits in its send buffer behind
    /// bytes this end's receive buffer has no room for yet. The FIN arrives,
    /// and this becomes `FinReceived`, when the last of them does.
    | FinQueued
    /// The peer closed cleanly and its FIN has arrived. Once the bytes
    /// already in the receive buffer are read, a read sees end of file.
    | FinReceived
    /// The connection was reset. `afterFin` says whether a FIN had arrived
    /// first, which on Linux decides both the pending error (`EPIPE` rather
    /// than `ECONNRESET`, since `tcp_reset` sets that in `CLOSE_WAIT`) and that
    /// reads see end of file rather than the error. `errorPending` says
    /// whether the error still waits for a call to take it.
    | Reset of afterFin : bool * errorPending : bool
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
    /// that frees send space raises it.
    | Darwin

/// What travels over one TCP connection, in both directions, and the rules
/// of the flavour the connection was made under.
[<NoComparison>]
type TcpTransfer =
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

/// Which call reads from a TCP connection.
[<RequireQualifiedAccess>]
type internal TcpReceiveCall =
    /// `read(2)`.
    | Read
    /// `recv(2)` without `MSG_PEEK`.
    | Receive
    /// `recv(2)` with `MSG_PEEK`: the bytes stay queued.
    | Peek

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
                Receiver = TcpEndState.Open
            }

        {
            ToClient = direction
            ToServer = direction
            Rules =
                match flavour with
                | SimulatedUnixFlavour.Linux -> TcpTransferRules.Linux Set.empty
                | SimulatedUnixFlavour.Darwin -> TcpTransferRules.Darwin
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
        | TcpEndState.Open
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived
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

    /// Every way `transfer` breaks the rules the functions here keep, as text;
    /// empty when it breaks none.
    let violations (transfer : TcpTransfer) : string list =
        let isLinux =
            match transfer.Rules with
            | TcpTransferRules.Linux _ -> true
            | TcpTransferRules.Darwin -> false

        let ofDirection (receiver : ConnectionEnd) : string list =
            let direction = towards receiver transfer
            let sending = ByteQueue.length direction.Sending
            let receiving = ByteQueue.length direction.Receiving
            let peerState = (towards (otherEnd receiver) transfer).Receiver

            [
                if direction.SendCapacity <= 0 || direction.ReceiveCapacity <= 0 then
                    $"towards %A{receiver}: capacities %d{direction.SendCapacity} and %d{direction.ReceiveCapacity} are not positive"
                if sending > direction.SendCapacity then
                    $"towards %A{receiver}: %d{sending} bytes in a send buffer of %d{direction.SendCapacity}"
                if receiving > direction.ReceiveCapacity then
                    $"towards %A{receiver}: %d{receiving} bytes in a receive buffer of %d{direction.ReceiveCapacity}"

                match direction.Receiver with
                | TcpEndState.Open
                | TcpEndState.FinQueued
                | TcpEndState.FinReceived ->
                    if sending > 0 && receiving < direction.ReceiveCapacity then
                        $"towards %A{receiver}: %d{sending} bytes wait to be sent while the receive buffer has room"
                | TcpEndState.Reset _ ->
                    if sending > 0 then
                        $"towards %A{receiver}: %d{sending} bytes in flight to an end that was reset"
                | TcpEndState.Closed ->
                    if receiving > 0 then
                        $"towards %A{receiver}: %d{receiving} bytes queued for a closed end"

                    if sending > 0 && isLinux then
                        $"towards %A{receiver}: %d{sending} bytes kept in flight to a closed end, which Linux discards"

                    if sending > 0 && not isLinux then
                        match peerState with
                        | TcpEndState.Reset _ -> ()
                        | other ->
                            $"towards %A{receiver}: %d{sending} bytes kept in flight to a closed end, by a sender in %A{other} rather than one that was reset"

                match direction.Receiver with
                | TcpEndState.FinQueued ->
                    if sending = 0 then
                        $"towards %A{receiver}: a FIN is queued behind no bytes, so it has arrived"
                | TcpEndState.FinReceived ->
                    if sending > 0 then
                        $"towards %A{receiver}: a FIN has arrived ahead of %d{sending} bytes sent before it"
                | TcpEndState.Open
                | TcpEndState.Reset _
                | TcpEndState.Closed -> ()

                match direction.Receiver, peerState with
                | TcpEndState.FinQueued, TcpEndState.Closed
                | TcpEndState.FinReceived, TcpEndState.Closed
                | TcpEndState.Reset _, TcpEndState.Closed -> ()
                | TcpEndState.FinQueued, other
                | TcpEndState.FinReceived, other
                | TcpEndState.Reset _, other ->
                    $"%A{receiver} is in %A{direction.Receiver} while its peer is in %A{other} rather than closed"
                | TcpEndState.Closed, TcpEndState.Open -> $"%A{receiver} is closed while its peer has not been told"
                | TcpEndState.Closed, _
                | TcpEndState.Open, _ -> ()
            ]

        let ofRules : string list =
            match transfer.Rules with
            | TcpTransferRules.Darwin -> []
            | TcpTransferRules.Linux armed ->
                [
                    for sender in armed do
                        match (towards sender transfer).Receiver with
                        | TcpEndState.Open
                        | TcpEndState.FinQueued
                        | TcpEndState.FinReceived -> ()
                        | other -> $"%A{sender}'s send-space wake is armed, but it is in %A{other} and cannot write"
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
        | TcpTransferRules.Darwin -> false

    let private withArmed (sender : ConnectionEnd) (armed : bool) (transfer : TcpTransfer) : TcpTransfer =
        match transfer.Rules with
        | TcpTransferRules.Darwin -> transfer
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
    /// `EVFILT_WRITE` reports as its event's `data`.
    let sendSpace (sender : ConnectionEnd) (transfer : TcpTransfer) : int =
        let outbound = towards (otherEnd sender) transfer
        outbound.SendCapacity - ByteQueue.length outbound.Sending

    /// How many bytes `sender` has written that are still in its send buffer,
    /// because the peer's receive buffer has had no room for them yet.
    let unsent (sender : ConnectionEnd) (transfer : TcpTransfer) : int =
        ByteQueue.length (towards (otherEnd sender) transfer).Sending

    /// Whether Linux's `tcp_poll` finds `sender` writable: its send buffer at
    /// most two thirds full (`sk_stream_is_writeable`). Says nothing of an end
    /// that was reset, which is writable whatever its buffer holds, since the
    /// write fails at once.
    let linuxSendable (sender : ConnectionEnd) (transfer : TcpTransfer) : bool =
        linuxWritable (towards (otherEnd sender) transfer)

    /// The error a call on `receiver`'s socket would take now, if one is
    /// pending.
    let pendingError (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpError option =
        match transfer.Rules, (towards receiver transfer).Receiver with
        | TcpTransferRules.Linux _, TcpEndState.Reset (true, true) -> Some TcpError.BrokenPipe
        | _, TcpEndState.Reset (_, true) -> Some TcpError.ConnectionReset
        | _, TcpEndState.Reset (_, false)
        | _, TcpEndState.Open
        | _, TcpEndState.FinQueued
        | _, TcpEndState.FinReceived
        | _, TcpEndState.Closed -> None

    /// A Linux `tcp_poll` of `sender`'s socket, by `poll(2)`, an epoll `ADD`
    /// or `MOD`, or `epoll_wait`'s re-poll of a pending entry: one that finds
    /// the socket open for sending but not writable marks it out of space
    /// (`SOCK_NOSPACE`), so that its send buffer draining to two thirds full
    /// raises `TcpWake.SendSpace`. Darwin's poll keeps no such mark.
    let polled (sender : ConnectionEnd) (transfer : TcpTransfer) : TcpTransfer =
        match transfer.Rules, (towards sender transfer).Receiver with
        | TcpTransferRules.Darwin, _ -> transfer
        | TcpTransferRules.Linux _, TcpEndState.Open
        | TcpTransferRules.Linux _, TcpEndState.FinQueued
        | TcpTransferRules.Linux _, TcpEndState.FinReceived ->
            if linuxSendable sender transfer then
                transfer
            else
                withArmed sender true transfer
        | TcpTransferRules.Linux _, TcpEndState.Reset _
        | TcpTransferRules.Linux _, TcpEndState.Closed -> transfer

    /// Move bytes on from the send buffer into the receive buffer as far as it
    /// has room: how many moved, and the direction after.
    let private deliver (direction : TcpDirection) : int * TcpDirection =
        let room = direction.ReceiveCapacity - ByteQueue.length direction.Receiving
        let moved = min room (ByteQueue.length direction.Sending)

        if moved = 0 then
            0, direction
        else

        let bytes, sending = ByteQueue.take moved direction.Sending

        moved,
        { direction with
            Sending = sending
            Receiving = ByteQueue.append bytes direction.Receiving
        }

    /// The wakes owed to `sender` now that `moved` bytes have left its send
    /// buffer for the peer's receive buffer.
    let private spaceFreed
        (sender : ConnectionEnd)
        (moved : int)
        (transfer : TcpTransfer)
        : TcpWake list * TcpTransfer
        =
        let senderState = (towards sender transfer).Receiver

        match senderState with
        | TcpEndState.Closed
        | TcpEndState.Reset _ -> [], transfer
        | TcpEndState.Open
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived ->

        match transfer.Rules with
        | TcpTransferRules.Darwin -> (if moved > 0 then [ TcpWake.SendSpace sender ] else []), transfer
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
        | TcpTransferRules.Darwin ->
            if int64 count <= space then Some count
            elif space >= int64 darwinSendLowWater then Some (int space)
            else None

    /// The room a write by `writer`, whose end may still send, has: the free
    /// space in its send buffer, and in the peer's receive buffer while the
    /// peer is there to drain it.
    let private writeSpace (context : string) (writer : ConnectionEnd) (transfer : TcpTransfer) : int64 =
        let outbound = towards (otherEnd writer) transfer

        match outbound.Receiver with
        | TcpEndState.Open ->
            int64 (outbound.ReceiveCapacity - ByteQueue.length outbound.Receiving)
            + int64 (outbound.SendCapacity - ByteQueue.length outbound.Sending)
        // The peer has gone, so nothing drains the send buffer.
        | TcpEndState.Closed -> int64 (outbound.SendCapacity - ByteQueue.length outbound.Sending)
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived
        | TcpEndState.Reset _ ->
            failwith
                $"TcpTransfer.%s{context}: the %A{writer} end has sent its FIN, which only its close does (this library models no shutdown), but it is still writing (this is a bug in this library)."

    /// Whether a read by `receiver`, asleep because nothing was there to
    /// answer it, has an answer now: bytes have arrived, or a FIN, or a reset.
    /// A read wakes for any of them, needing only one byte to return
    /// (measured, `tcp-blocking.c` sections R-data, R-fin and R-reset).
    let readAnswers (receiver : ConnectionEnd) (transfer : TcpTransfer) : bool =
        let direction = towards receiver transfer

        match direction.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.readAnswers: the %A{receiver} end is closed, so nothing can be asleep reading there (this is a bug in this library)."
        | TcpEndState.FinReceived
        | TcpEndState.Reset _ -> true
        | TcpEndState.Open
        | TcpEndState.FinQueued -> ByteQueue.length direction.Receiving > 0

    /// Whether a write by `writer`, asleep with `remaining` of its bytes not
    /// yet taken, is woken: by a reset at either flavour, and otherwise by
    /// room. On Linux a sleeping writer is woken only once its send buffer
    /// has drained to two thirds full (`sk_stream_write_space`), as an
    /// edge-triggered waiter is, and has room; on Darwin, at every acknowledgement that
    /// leaves room for a write of `remaining` to take something, by the low
    /// water mark `admitWrite` applies.
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
        | TcpEndState.Open
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived ->

        match transfer.Rules with
        // The woken writer goes on only if the buffer is not full
        // (`sk_stream_memory_free`), which a buffer of one byte holding one is
        // while two thirds full.
        | TcpTransferRules.Linux _ ->
            linuxWritable (towards (otherEnd writer) transfer)
            && writeSpace "writeResumes" writer transfer > 0L
        | TcpTransferRules.Darwin -> (taking transfer remaining (writeSpace "writeResumes" writer transfer)).IsSome

    /// What a write of `count` bytes by `writer` decides before the caller's
    /// buffer is read, and the transfer after: a failed write takes a pending
    /// error on Linux, and a Linux write that runs out of space, whether it
    /// meets `EAGAIN` or takes only part of its bytes, arms the writer's
    /// send-space wake (`SOCK_NOSPACE`).
    let admitWrite (writer : ConnectionEnd) (count : int) (transfer : TcpTransfer) : TcpWriteAdmission * TcpTransfer =
        if count < 0 then
            failwith $"TcpTransfer.admitWrite: a write of %d{count} bytes (this is a bug in this library)."

        let own = towards writer transfer

        match own.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.admitWrite: the %A{writer} end is closed, so nothing can write there (this is a bug in this library)."
        | TcpEndState.Reset (afterFin, errorPending) ->
            match transfer.Rules with
            | TcpTransferRules.Linux _ ->
                // `sk_stream_error`: the pending error if there is one, which
                // the write takes, and `EPIPE` once it is gone.
                if errorPending then
                    let error =
                        if afterFin then
                            TcpError.BrokenPipe
                        else
                            TcpError.ConnectionReset

                    TcpWriteAdmission.Answered (TcpWriteAnswer.Failed error),
                    withTowards
                        writer
                        { own with
                            Receiver = TcpEndState.Reset (afterFin, false)
                        }
                        transfer
                else
                    TcpWriteAdmission.Answered (TcpWriteAnswer.Failed TcpError.BrokenPipe), transfer
            // `sosend` answers `EPIPE` once it cannot send more, without
            // reading the pending error.
            | TcpTransferRules.Darwin ->
                TcpWriteAdmission.Answered (TcpWriteAnswer.Failed TcpError.BrokenPipe), transfer
        | TcpEndState.Open
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived ->

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
    /// keeps counting them in the writer's send buffer.
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
            let afterFin =
                match own.Receiver with
                | TcpEndState.FinReceived -> true
                | _ -> false

            let outbound =
                match transfer.Rules with
                | TcpTransferRules.Linux _ -> outbound
                | TcpTransferRules.Darwin ->
                    { outbound with
                        Sending = ByteQueue.append bytes outbound.Sending
                    }

            // The reset also ends what the closed peer still had in flight
            // to the writer; what already arrived stays readable.
            let transfer =
                transfer
                |> withTowards peer outbound
                |> withTowards
                    writer
                    { own with
                        Sending = ByteQueue.empty
                        Receiver = TcpEndState.Reset (afterFin, true)
                    }
                |> withArmed writer false

            [ TcpWake.PeerReset writer ], keepingRules "write" transfer
        | _ ->

        let queued =
            { outbound with
                Sending = ByteQueue.append bytes outbound.Sending
            }

        let arrived, outbound = deliver queued

        let wakes =
            [
                if arrived > 0 then
                    TcpWake.DataArrived peer

                    match transfer.Rules with
                    | TcpTransferRules.Darwin -> TcpWake.SendSpace writer
                    | TcpTransferRules.Linux _ -> ()
            ]

        wakes, keepingRules "write" (withTowards peer outbound transfer)

    /// The error a pending one would make a call by `receiver` fail with.
    let private resetError (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpError =
        match transfer.Rules, (towards receiver transfer).Receiver with
        | TcpTransferRules.Linux _, TcpEndState.Reset (true, _) -> TcpError.BrokenPipe
        | _ -> TcpError.ConnectionReset

    /// What a non-blocking `call` asking for `count` bytes from `receiver`
    /// answers, the wakes it raises, and the transfer after.
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
        | _ -> ()

        let takeError () : TcpTransfer =
            match direction.Receiver with
            | TcpEndState.Reset (afterFin, true) ->
                withTowards
                    receiver
                    { direction with
                        Receiver = TcpEndState.Reset (afterFin, false)
                    }
                    transfer
            | _ -> transfer

        let isLinux =
            match transfer.Rules with
            | TcpTransferRules.Linux _ -> true
            | TcpTransferRules.Darwin -> false

        let queued = ByteQueue.length direction.Receiving

        let unqueuedFin : string =
            $"TcpTransfer.read: the %A{receiver} end's FIN waits behind bytes in flight, but its receive buffer is empty, which TcpTransfer.violations forbids (this is a bug in this library)."

        // Linux's `read(2)` answers a zero-length request before it reaches
        // the socket.
        if isLinux && count = 0 && call = TcpReceiveCall.Read then
            TcpReadAnswer.Bytes ImmutableArray.Empty, [], transfer
        elif queued > 0 then
            let taken = min count queued

            match call with
            | TcpReceiveCall.Peek -> TcpReadAnswer.Bytes (ByteQueue.peek taken direction.Receiving), [], transfer
            | TcpReceiveCall.Read
            | TcpReceiveCall.Receive ->
                let bytes, receiving = ByteQueue.take taken direction.Receiving

                let moved, direction =
                    deliver
                        { direction with
                            Receiving = receiving
                        }

                // A FIN queued behind the bytes arrives with the last of them.
                let finArrives =
                    direction.Receiver = TcpEndState.FinQueued
                    && ByteQueue.length direction.Sending = 0

                let direction =
                    if finArrives then
                        { direction with
                            Receiver = TcpEndState.FinReceived
                        }
                    else
                        direction

                let transfer = withTowards receiver direction transfer
                let sender = otherEnd receiver
                let spaceWakes, transfer = spaceFreed sender moved transfer

                let wakes =
                    [
                        if moved > 0 then
                            TcpWake.DataArrived receiver
                        if finArrives then
                            TcpWake.PeerFinished receiver
                        yield! spaceWakes
                    ]

                TcpReadAnswer.Bytes bytes, wakes, keepingRules "read" transfer
        elif isLinux then
            // `tcp_recvmsg` tests for a received FIN before the pending error,
            // and `sock_error` takes the error even under `MSG_PEEK`.
            match direction.Receiver with
            | TcpEndState.Open -> TcpReadAnswer.WouldBlock, [], transfer
            | TcpEndState.FinReceived
            | TcpEndState.Reset (true, _)
            | TcpEndState.Reset (false, false) -> TcpReadAnswer.EndOfFile, [], transfer
            | TcpEndState.Reset (false, true) -> TcpReadAnswer.Failed (resetError receiver transfer), [], takeError ()
            | TcpEndState.FinQueued -> failwith unqueuedFin
            | TcpEndState.Closed -> failwith "TcpTransfer.read: unreachable, the closed end was refused above."
        else
            // `soreceive` takes the pending error unless peeking, and a
            // zero-length request with nothing queued answers 0 rather than
            // waiting.
            match direction.Receiver with
            | TcpEndState.Reset (_, true) ->
                let transfer =
                    match call with
                    | TcpReceiveCall.Peek -> transfer
                    | TcpReceiveCall.Read
                    | TcpReceiveCall.Receive -> takeError ()

                TcpReadAnswer.Failed (resetError receiver transfer), [], transfer
            | TcpEndState.Reset (_, false)
            | TcpEndState.FinReceived -> TcpReadAnswer.EndOfFile, [], transfer
            | TcpEndState.Open ->
                if count = 0 then
                    TcpReadAnswer.Bytes ImmutableArray.Empty, [], transfer
                else
                    TcpReadAnswer.WouldBlock, [], transfer
            | TcpEndState.FinQueued -> failwith unqueuedFin
            | TcpEndState.Closed -> failwith "TcpTransfer.read: unreachable, the closed end was refused above."

    /// `getsockopt(SO_ERROR)` on `receiver`'s socket: the pending error, which
    /// the call takes, and the transfer after.
    let takeError (receiver : ConnectionEnd) (transfer : TcpTransfer) : TcpError option * TcpTransfer =
        let direction = towards receiver transfer

        match direction.Receiver with
        | TcpEndState.Closed ->
            failwith
                $"TcpTransfer.takeError: the %A{receiver} end is closed, so it has no socket to ask (this is a bug in this library)."
        | TcpEndState.Reset (afterFin, true) ->
            Some (resetError receiver transfer),
            withTowards
                receiver
                { direction with
                    Receiver = TcpEndState.Reset (afterFin, false)
                }
                transfer
        | TcpEndState.Open
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived
        | TcpEndState.Reset (_, false) -> None, transfer

    /// `closer`'s socket closes, sending a FIN if `abortive` is false and
    /// nothing is left unread, and a reset otherwise.
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

        match own.Receiver with
        | TcpEndState.Closed ->
            failwith $"TcpTransfer.%s{context}: the %A{closer} end is closed already (this is a bug in this library)."
        | _ -> ()

        let transfer = transfer |> withArmed closer false

        let closedOwn (sending : ByteQueue) : TcpDirection =
            { own with
                Sending = sending
                Receiving = ByteQueue.empty
                Receiver = TcpEndState.Closed
            }

        match outbound.Receiver with
        | TcpEndState.Closed ->
            // Both ends have now closed, and nothing is left to send or keep.
            let transfer =
                transfer
                |> withTowards closer (closedOwn ByteQueue.empty)
                |> withTowards
                    peer
                    { outbound with
                        Sending = ByteQueue.empty
                    }

            [], keepingRules context transfer
        | TcpEndState.FinQueued
        | TcpEndState.FinReceived
        | TcpEndState.Reset _ ->
            failwith
                $"TcpTransfer.%s{context}: the %A{closer} end has sent its FIN or reset already, which only its close does (this library models no shutdown), but it is open (this is a bug in this library)."
        | TcpEndState.Open ->

        // Bytes still on their way to the closer count as unread too, but
        // they wait only while its receive buffer is full, so that test
        // covers them.
        let unread = ByteQueue.length own.Receiving > 0

        if abortive || unread then
            // The peer keeps what is in its receive buffer, and loses what the
            // closer still had to send it. What the peer still had to send
            // the closer is discarded on Linux, and stays counted in the
            // peer's send buffer on Darwin.
            let kept =
                match transfer.Rules with
                | TcpTransferRules.Linux _ -> ByteQueue.empty
                | TcpTransferRules.Darwin -> own.Sending

            let transfer =
                transfer
                |> withTowards closer (closedOwn kept)
                |> withTowards
                    peer
                    { outbound with
                        Sending = ByteQueue.empty
                        Receiver = TcpEndState.Reset (false, true)
                    }
                |> withArmed peer false

            [ TcpWake.PeerReset peer ], keepingRules context transfer
        else
            // What the closer had left to send keeps draining as the peer
            // reads, and the FIN follows it.
            let finQueued = ByteQueue.length outbound.Sending > 0

            let transfer =
                transfer
                |> withTowards closer (closedOwn ByteQueue.empty)
                |> withTowards
                    peer
                    { outbound with
                        Receiver =
                            if finQueued then
                                TcpEndState.FinQueued
                            else
                                TcpEndState.FinReceived
                    }

            (if finQueued then [] else [ TcpWake.PeerFinished peer ]), keepingRules context transfer

    /// `closer`'s socket closes. With nothing unread, and nothing on its way
    /// to it, the close is a FIN: the peer reads what was sent, then end of
    /// file. Otherwise it is a reset, and the peer gets a pending error.
    let close (closer : ConnectionEnd) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
        closeWith "close" false closer transfer

    /// `closer`'s socket closes with a reset whatever is unread, as a close
    /// under `SO_LINGER` {1, 0} does.
    let abort (closer : ConnectionEnd) (transfer : TcpTransfer) : TcpWake list * TcpTransfer =
        closeWith "abort" true closer transfer
