namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of the blocking-connection property: a call by a task, a
/// descriptor operation, or a step of the client's scheduler.
///
/// A task is named by its index, modulo their number, among the tasks the step
/// can be taken by: those making no call, for a call; those in a call, for a
/// signal; those woken, for a finish. A descriptor is named by its index among
/// the open ones.
[<RequireQualifiedAccess>]
type BlockingConnectionOp =
    | Read of task : int * fd : int * count : int
    | Write of task : int * fd : int * count : int
    /// `recv(2)`, with `MSG_PEEK` if `peek` and `MSG_DONTWAIT` if `dontWait`.
    | Receive of task : int * fd : int * count : int * peek : bool * dontWait : bool
    /// `send(2)`, with `MSG_NOSIGNAL` if `noSignal` and `MSG_DONTWAIT` if
    /// `dontWait`.
    | Send of task : int * fd : int * count : int * noSignal : bool * dontWait : bool
    | Close of fd : int
    | Dup of fd : int
    | SetNonBlocking of fd : int * value : bool
    /// `SO_LINGER` on `fd`: on or off, with a time of `seconds`.
    | Linger of fd : int * on : bool * seconds : int
    /// A caught `SIGUSR1` is sent to a task in a call.
    | Signal of task : int
    /// The client asks which sleepers the system wakes.
    | Wake
    /// The client finishes the call of a woken task.
    | Finish of task : int

/// Blocking `read(2)`, `recv(2)`, `write(2)` and `send(2)` on a connected TCP
/// socket, by several tasks, through `tcp-blocking.c`'s and
/// `tcp-recv-send.c`'s measurements (docs/plans/2026-10-07-tcp-byte-transfer):
/// what parks, who a transfer wakes, and how each woken call finishes.
///
/// The property holds the library to a reference written out again from those
/// measurements over `TcpTransferReference`'s transfer rules: a blocking
/// transfer that would wait sleeps, having taken what fits; every sleeper on a
/// socket wakes for what it waits on (section R-order); a sleeping writer
/// wakes on Linux once its send buffer has drained to two thirds, and on
/// Darwin once a write of what is left would take something (W-resume); a
/// signal ends a sleep as R-eintr, R-restart, W-partial and W-empty measured,
/// beats room on Linux and loses to bytes there (D-read, D-write); a reset
/// ends a write as W-reset measured; a Darwin close of the descriptor a
/// call was made through ends it with `EBADF` (R-close, W-close); and under
/// `SO_LINGER`, the close of a socket's last descriptor is refused while a
/// call the close does not end holds it, and otherwise where the socket's own
/// close would reset the connection or wait. A `recv` is
/// a read by its own call (`MSG_PEEK` leaves the bytes, and a Linux `recv`
/// of nothing sleeps: sections P and Z), `MSG_DONTWAIT` makes a `recv` and a
/// Linux `send` non-blocking (D), `MSG_NOSIGNAL` keeps an `EPIPE` from raising
/// `SIGPIPE` (N), and only a Darwin `write` marks its description written (W).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBlockingConnection =

    let private tasks : int list = [ 0 ; 1 ; 2 ; 3 ]

    let private port : uint16 = 6100us

    /// What a call came to, in a shape both the library and the reference can
    /// produce.
    [<RequireQualifiedAccess>]
    type private Seen =
        | ReadBytes of byte list
        | Wrote of int64
        | Failed of UnixError
        | Sleeps
        | Restarts
        /// The library will not say whether the call's own answer or a signal
        /// ends it.
        | RefusedSignal
        /// The call found nothing, through a description that had become
        /// non-blocking while it slept.
        | RefusedNonBlocking
        /// A Darwin close of the descriptor a woken call was made through.
        | RefusedClose

    // --- the reference ---

    type private Call =
        | Reading of connectionEnd : ConnectionEnd * count : int * call : TcpReceiveCall
        | Writing of connectionEnd : ConnectionEnd * payload : byte list * written : int * call : TcpSendCall

    type private Park =
        {
            Call : Call
            /// The descriptor the call was made through; `None` once a Darwin
            /// close of it has ended the call.
            Through : int option
            /// The client has woken it, and not yet finished it.
            Woken : bool
        }

    type private Reference =
        {
            Linux : bool
            Restart : bool
            Model : TcpTransferReference.Model
            /// Open descriptor to the end it names. Each end has one open file
            /// description, which every descriptor naming the end shares.
            Fds : Map<int, ConnectionEnd>
            NonBlocking : Map<ConnectionEnd, bool>
            Parks : Map<int, Park>
            Signalled : Set<int>
            Writes : int
            /// The ends whose description has gone, closing their socket.
            Gone : Set<ConnectionEnd>
            /// The ends whose description a `write` has returned having moved
            /// bytes through: Darwin's `FWASWRITTEN`, which no `send` sets.
            Marked : Set<ConnectionEnd>
            /// Each end's `SO_LINGER` time, while it is on.
            Linger : Map<ConnectionEnd, int>
        }

    let private endOf (call : Call) : ConnectionEnd =
        match call with
        | Call.Reading (connectionEnd, _, _)
        | Call.Writing (connectionEnd, _, _, _) -> connectionEnd

    let private payload (index : int) (count : int) : byte list =
        List.init count (fun i -> byte ((index * 37 + i) % 251))

    /// The ends something still references: a descriptor, or a call that has
    /// not returned and that no Darwin close has ended.
    let private live (r : Reference) : Set<ConnectionEnd> =
        let named = r.Fds |> Map.values |> Set.ofSeq

        let held =
            r.Parks
            |> Map.values
            |> Seq.filter (fun park -> park.Through.IsSome)
            |> Seq.map (fun park -> endOf park.Call)
            |> Set.ofSeq

        Set.union named held

    /// Close the socket of every end nothing references any more.
    let private releaseUnreferenced (r : Reference) : Reference =
        let live = live r

        ([ ConnectionEnd.Client ; ConnectionEnd.Server ], r)
        ||> List.foldBack (fun connectionEnd r ->
            if Set.contains connectionEnd live || Set.contains connectionEnd r.Gone then
                r
            else
                let _, model = TcpTransferReference.close false connectionEnd r.Model

                { r with
                    Model = model
                    Gone = Set.add connectionEnd r.Gone
                }
        )

    let private other (connectionEnd : ConnectionEnd) : ConnectionEnd =
        match connectionEnd with
        | ConnectionEnd.Client -> ConnectionEnd.Server
        | ConnectionEnd.Server -> ConnectionEnd.Client

    /// Whether a read by `connectionEnd` has something to answer now.
    let private readable (connectionEnd : ConnectionEnd) (r : Reference) : bool =
        match TcpTransferReference.read connectionEnd TcpReceiveCall.Read 1 r.Model with
        | TcpTransferSeen.WouldBlock, _, _ -> false
        | _ -> true

    /// The room a write by `connectionEnd` would have, as the measured rule
    /// counts it: the free space in its send buffer, and in the peer's
    /// receive buffer while the peer is there to drain it.
    let private room (connectionEnd : ConnectionEnd) (r : Reference) : int =
        let flow = r.Model.Flows.[other connectionEnd]
        let sendFree = flow.SendCap - List.length flow.InFlight

        if r.Model.Ends.[other connectionEnd].Closed then
            sendFree
        else
            sendFree + flow.RecvCap - List.length flow.Arrived

    /// How much of `remaining` bytes a write by `connectionEnd` takes now:
    /// any positive part on Linux, and on Darwin all of it or at least 2048.
    let private takes (connectionEnd : ConnectionEnd) (remaining : int) (r : Reference) : int option =
        let free = room connectionEnd r

        if r.Linux then
            (if free > 0 then Some (min remaining free) else None)
        elif remaining <= free then
            Some remaining
        elif free >= 2048 then
            Some free
        else
            None

    /// Whether a writer asleep at `connectionEnd` with `remaining` bytes left
    /// is woken by room: on Linux once the send buffer is at most two thirds
    /// full, in the kernel's integer arithmetic, with room; on Darwin once a
    /// write of `remaining` would take something.
    let private roomWakes (connectionEnd : ConnectionEnd) (remaining : int) (r : Reference) : bool =
        if r.Linux then
            let flow = r.Model.Flows.[other connectionEnd]
            let queued = List.length flow.InFlight
            flow.SendCap - queued >= queued / 2 && room connectionEnd r > 0
        else
            (takes connectionEnd remaining r).IsSome

    let private park (task : int) (fd : int) (call : Call) (r : Reference) : Reference =
        { r with
            Parks =
                Map.add
                    task
                    {
                        Call = call
                        Through = Some fd
                        Woken = false
                    }
                    r.Parks
        }

    /// The task's call answered: the signal it had pending is taken as it
    /// returns, and what it held goes if nothing else references it.
    let private answered (task : int) (r : Reference) : Reference =
        { r with
            Parks = Map.remove task r.Parks
            Signalled = Set.remove task r.Signalled
        }
        |> releaseUnreferenced

    let private ofRead (seen : TcpTransferSeen) : Seen =
        match seen with
        | TcpTransferSeen.Bytes bytes -> Seen.ReadBytes bytes
        | TcpTransferSeen.EndOfFile -> Seen.ReadBytes []
        | TcpTransferSeen.ReadFailed error -> Seen.Failed (TcpError.toUnixError error)
        | other -> failwith $"reference: a read came to %A{other}"

    let private ofFailedWrite (seen : TcpTransferSeen) : Seen =
        match seen with
        | TcpTransferSeen.WriteFailed error -> Seen.Failed (TcpError.toUnixError error)
        | other -> failwith $"reference: a write to a reset end came to %A{other}"

    /// A read by `call` of up to `count` bytes, non-blocking if `dontWait`
    /// whatever the description says.
    let private referenceRead
        (task : int)
        (fd : int)
        (count : int)
        (call : TcpReceiveCall)
        (dontWait : bool)
        (r : Reference)
        : Seen * Reference
        =
        let connectionEnd = r.Fds.[fd]

        match TcpTransferReference.read connectionEnd call count r.Model with
        | TcpTransferSeen.WouldBlock, _, model ->
            if r.NonBlocking.[connectionEnd] || dontWait then
                Seen.Failed UnixError.EAGAIN, r
            else
                Seen.Sleeps,
                park
                    task
                    fd
                    (Call.Reading (connectionEnd, count, call))
                    { r with
                        Model = model
                    }
        | seen, _, model ->
            ofRead seen,
            { r with
                Model = model
            }

    /// The end `connectionEnd`'s description marked written if `call` is a
    /// write that moved bytes, under Darwin.
    let private marking (connectionEnd : ConnectionEnd) (call : TcpSendCall) (moved : bool) (r : Reference) =
        match call with
        | TcpSendCall.Write when moved && not r.Linux ->
            { r with
                Marked = Set.add connectionEnd r.Marked
            }
        | TcpSendCall.Write
        | TcpSendCall.Send _ -> r

    /// A write by `call` of `count` bytes; `dontWait` makes a Linux `send`
    /// non-blocking whatever the description says.
    let private referenceWrite
        (task : int)
        (fd : int)
        (count : int)
        (call : TcpSendCall)
        (dontWait : bool)
        (r : Reference)
        : Seen * Reference
        =
        let connectionEnd = r.Fds.[fd]
        let bytes = payload r.Writes count

        let r =
            { r with
                Writes = r.Writes + 1
            }

        let blocking = not r.NonBlocking.[connectionEnd] && not (dontWait && r.Linux)

        match TcpTransferReference.write connectionEnd bytes r.Model with
        | TcpTransferSeen.WouldBlock, _, model ->
            let r =
                { r with
                    Model = model
                }

            if blocking then
                Seen.Sleeps, park task fd (Call.Writing (connectionEnd, bytes, 0, call)) r
            else
                Seen.Failed UnixError.EAGAIN, r
        | TcpTransferSeen.Wrote n, _, model ->
            let r =
                { r with
                    Model = model
                }

            if n < count && blocking then
                Seen.Sleeps, park task fd (Call.Writing (connectionEnd, bytes, n, call)) r
            else
                Seen.Wrote (int64 n), marking connectionEnd call (n > 0) r
        | seen, _, model ->
            ofFailedWrite seen,
            { r with
                Model = model
            }

    /// Whether the sleeping `task`'s call is woken.
    let private wakesNow (task : int) (r : Reference) : bool =
        let park = r.Parks.[task]

        park.Through.IsNone
        || Set.contains task r.Signalled
        || (
            match park.Call with
            | Call.Reading (connectionEnd, _, _) -> readable connectionEnd r
            | Call.Writing (connectionEnd, payload, written, _) ->
                r.Model.Ends.[connectionEnd].GotReset
                || roomWakes connectionEnd (List.length payload - written) r
        )

    /// The sleepers the client holds asleep that the reference wakes, in the
    /// order they parked: every one whose call has something to answer.
    let private referenceWakes (ordinals : Map<int, int64>) (r : Reference) : int list =
        r.Parks
        |> Map.filter (fun task park -> not park.Woken && wakesNow task r)
        |> Map.keys
        |> Seq.sortBy (fun task -> ordinals.[task])
        |> List.ofSeq

    let private interrupted (r : Reference) : Seen =
        if r.Restart then
            Seen.Restarts
        else
            Seen.Failed UnixError.EINTR

    let private referenceFinish (task : int) (r : Reference) : Seen * Reference =
        let park = r.Parks.[task]
        let signalled = Set.contains task r.Signalled

        let reparked (call : Call) (r : Reference) =
            { r with
                Parks =
                    Map.add
                        task
                        { park with
                            Call = call
                            Woken = false
                        }
                        r.Parks
            }

        match park.Call with
        | Call.Reading _
        | Call.Writing _ when park.Through.IsNone -> Seen.Failed UnixError.EBADF, answered task r
        | Call.Reading (connectionEnd, count, call) ->
            if readable connectionEnd r then
                if signalled && not r.Linux then
                    Seen.RefusedSignal, r
                else
                    let seen, _, model = TcpTransferReference.read connectionEnd call count r.Model

                    ofRead seen,
                    answered
                        task
                        { r with
                            Model = model
                        }
            elif signalled then
                interrupted r, answered task r
            elif r.NonBlocking.[connectionEnd] then
                Seen.RefusedNonBlocking, r
            else
                Seen.Sleeps, reparked park.Call r
        | Call.Writing (connectionEnd, payload, written, call) ->
            let count = List.length payload
            let rest = List.skip written payload
            // A call that returns having moved bytes, whatever it answers.
            let movedSome = marking connectionEnd call (written > 0)

            if r.Model.Ends.[connectionEnd].GotReset then
                if signalled && not r.Linux then
                    Seen.RefusedSignal, r
                elif r.Linux && written > 0 then
                    Seen.Wrote (int64 written), answered task r |> movedSome
                else
                    let seen, _, model = TcpTransferReference.write connectionEnd rest r.Model

                    ofFailedWrite seen,
                    answered
                        task
                        { r with
                            Model = model
                        }
                    |> movedSome
            elif signalled then
                if not r.Linux && (takes connectionEnd (count - written) r).IsSome then
                    Seen.RefusedSignal, r
                elif written > 0 then
                    Seen.Wrote (int64 written), answered task r |> movedSome
                else
                    interrupted r, answered task r
            else
                match takes connectionEnd (count - written) r with
                | None ->
                    if r.NonBlocking.[connectionEnd] then
                        Seen.RefusedNonBlocking, r
                    else
                        let _, _, model = TcpTransferReference.write connectionEnd rest r.Model

                        Seen.Sleeps,
                        reparked
                            park.Call
                            { r with
                                Model = model
                            }
                | Some taken ->
                    let seen, _, model =
                        TcpTransferReference.write connectionEnd (List.take taken rest) r.Model

                    match seen with
                    | TcpTransferSeen.Wrote n when n = taken -> ()
                    | other -> failwith $"reference: a resumed write of %d{taken} came to %A{other}"

                    let r =
                        { r with
                            Model = model
                        }

                    let written = written + taken

                    if written = count then
                        Seen.Wrote (int64 count), answered task r |> marking connectionEnd call true
                    elif r.NonBlocking.[connectionEnd] then
                        Seen.RefusedNonBlocking, r
                    else
                        Seen.Sleeps, reparked (Call.Writing (connectionEnd, payload, written, call)) r

    // --- the library, driven as a client drives it ---

    let private fromRead (outcome : Result<ReadOutcome * UnixSystem<int, string>, ReadRefusal>) =
        match outcome with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) -> Seen.ReadBytes (List.ofSeq bytes), Some after
        | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), after) -> Seen.Failed error, Some after
        | Ok (ReadOutcome.Answered (ReadAnswer.Drawn _), _) -> failwith "a read of a socket drew from the entropy pool"
        | Ok (ReadOutcome.WouldBlock _, after) -> Seen.Sleeps, Some after
        | Ok (ReadOutcome.Restarts, after) -> Seen.Restarts, Some after
        | Error (ReadRefusal.Interruption _) -> Seen.RefusedSignal, None
        | Error (ReadRefusal.ConnectionBecameNonBlocking _) -> Seen.RefusedNonBlocking, None
        | Error refusal -> failwith $"read refused: %s{ReadRefusal.describe refusal}"

    /// What a write came to, and whether it raised `SIGPIPE`.
    let private fromWriteRaising (outcome : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>) =
        match outcome with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, after)) -> Seen.Wrote n, false, Some after
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Completed n, signal, after)) ->
            failwith $"a write that answered %d{n} raised %A{signal}"
        | Ok (WriteOutcome.Returns (WriteAnswer.Failed error, after)) -> Seen.Failed error, false, Some after
        | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed error, signal, after)) ->
            signal.Signal |> shouldEqual Signal.SIGPIPE
            Seen.Failed error, true, Some after
        | Ok (WriteOutcome.WouldBlock (_, after)) -> Seen.Sleeps, false, Some after
        | Ok (WriteOutcome.Restarts after) -> Seen.Restarts, false, Some after
        | Ok (WriteOutcome.ProcessEnded _ as outcome) -> failwith $"a write ended the process: %A{outcome}"
        | Error (WriteRefusal.Interruption _) -> Seen.RefusedSignal, false, None
        | Error (WriteRefusal.ConnectionBecameNonBlocking _) -> Seen.RefusedNonBlocking, false, None
        | Error refusal -> failwith $"write refused: %s{WriteRefusal.describe refusal}"

    let private fromWrite (outcome : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>) =
        let seen, _, after = fromWriteRaising outcome
        seen, after

    /// `send`'s outcome as a write's: every refusal a `send` gives here is
    /// one a `write` gives too.
    let private ofSend
        (outcome : Result<WriteOutcome<WriteAnswer, int, string>, SendRefusal>)
        : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>
        =
        match outcome with
        | Ok outcome -> Ok outcome
        | Error refusal -> failwith $"send refused: %s{SendRefusal.describe refusal}"

    /// Whether a call by `call` answering `seen` raises `SIGPIPE`: an `EPIPE`
    /// does, unless the call is a `send` with `MSG_NOSIGNAL`.
    let private raises (call : TcpSendCall) (seen : Seen) : bool =
        match seen, call with
        | Seen.Failed UnixError.EPIPE, TcpSendCall.Send true -> false
        | Seen.Failed UnixError.EPIPE, _ -> true
        | _ -> false

    let private flagWord (platform : SimulatedUnixPlatform) (flags : MessageFlag list) : int =
        MessageFlag.encode (SimulatedUnixPlatform.flavour platform) flags |> Option.get

    /// The woken write of `task`, finished: admitted, and given the bytes of
    /// `payload` it asks for.
    let private libraryFinishWrite (task : int) (payload : byte list) (system : UnixSystem<int, string>) =
        match UnixReadWrite.admitFinishWrite task system with
        | Error refusal -> Error refusal
        | Ok (WriteOutcome.Returns (WriteResumption.Transfer (offset, count), admitted)) ->
            let bytes = payload |> List.skip offset |> List.take count
            UnixReadWrite.finishWrite task (ImmutableArray.CreateRange bytes) admitted
        | Ok (WriteOutcome.Returns (WriteResumption.Answered answer, after)) ->
            Ok (WriteOutcome.Returns (answer, after))
        | Ok (WriteOutcome.ReturnsRaising (WriteResumption.Answered answer, signal, after)) ->
            Ok (WriteOutcome.ReturnsRaising (answer, signal, after))
        | Ok (WriteOutcome.WouldBlock (condition, after)) -> Ok (WriteOutcome.WouldBlock (condition, after))
        | Ok (WriteOutcome.Restarts after) -> Ok (WriteOutcome.Restarts after)
        | Ok other -> failwith $"admitFinishWrite: %A{other}"

    /// `task` returns to user mode: every handler it takes runs and returns.
    let rec private returnToUser (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixSignal.onReturnToUser task system with
        | Ok (None, system) -> system
        | Ok (Some (SignalDelivery.RunHandlers frames), system) ->
            (system, frames)
            ||> List.fold (fun system frame -> UnixSignal.sigreturn task frame.Id system)
            |> returnToUser task
        | other -> failwith $"returning task %d{task} to user mode: %A{other}"

    /// `closes` weights the closes, which end the connection's useful life,
    /// against the 31 of everything else.
    let private opGen (closes : int) : Gen<BlockingConnectionOp> =
        let task = Gen.elements tasks
        let fd = Gen.choose (0, 7)

        // Weighted towards writes that outgrow even Darwin's buffers, which
        // are the larger, so that writers sleep under both flavours.
        let count =
            Gen.frequency
                [
                    3, Gen.elements [ 0 ; 1 ; 7 ; 100 ; 2047 ; 2048 ; 3000 ; 20000 ; 60000 ]
                    2, Gen.elements [ 100000 ; 200000 ]
                ]

        let flag = Gen.elements [ false ; true ]

        Gen.frequency
            [
                4, Gen.map3 (fun t f c -> BlockingConnectionOp.Read (t, f, c)) task fd count
                4, Gen.map3 (fun t f c -> BlockingConnectionOp.Write (t, f, c)) task fd count
                3,
                gen {
                    let! t = task
                    let! f = fd
                    // A Linux recv of nothing sleeps, so ask for it more often.
                    let! c = Gen.frequency [ 1, Gen.constant 0 ; 2, count ]
                    let! peek = flag
                    let! dontWait = Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ]
                    return BlockingConnectionOp.Receive (t, f, c, peek, dontWait)
                }
                3,
                gen {
                    let! t = task
                    let! f = fd
                    let! c = count
                    let! noSignal = flag
                    let! dontWait = flag
                    return BlockingConnectionOp.Send (t, f, c, noSignal, dontWait)
                }
                closes, Gen.map BlockingConnectionOp.Close fd
                1, Gen.map BlockingConnectionOp.Dup fd
                2, Gen.map2 (fun f v -> BlockingConnectionOp.SetNonBlocking (f, v)) fd (Gen.elements [ true ; false ])
                1,
                Gen.map3
                    (fun f on seconds -> BlockingConnectionOp.Linger (f, on, seconds))
                    fd
                    (Gen.elements [ true ; false ])
                    (Gen.elements [ 0 ; 1 ])
                2, Gen.map BlockingConnectionOp.Signal task
                6, Gen.constant BlockingConnectionOp.Wake
                8, Gen.map BlockingConnectionOp.Finish task
            ]

    /// Two tasks read one end and sleep; then `between` makes that end
    /// readable, a wake wakes both, and the client finishes them in turn.
    /// Each op names a task and a descriptor by index, as `opGen`'s do: the
    /// first idle task, and the first descriptor or the second.
    let private twoReaders (between : BlockingConnectionOp) : BlockingConnectionOp list =
        [
            BlockingConnectionOp.Read (0, 0, 100000)
            BlockingConnectionOp.Read (0, 0, 100000)
            between
            BlockingConnectionOp.Wake
            BlockingConnectionOp.Finish 0
            BlockingConnectionOp.Finish 0
        ]

    /// Two tasks write enough to one end that both sleep, the second having
    /// taken nothing; the other end closes over the bytes it has not read,
    /// which resets the connection; a third task reads the reset's error, and
    /// the client wakes and finishes the writers.
    let private resetUnderWriters : BlockingConnectionOp list =
        [
            BlockingConnectionOp.Write (0, 0, 200000)
            BlockingConnectionOp.Write (0, 0, 200000)
            BlockingConnectionOp.Close 1
            BlockingConnectionOp.Read (0, 0, 100)
            BlockingConnectionOp.Wake
            BlockingConnectionOp.Finish 0
            BlockingConnectionOp.Finish 0
        ]

    /// Openings that reach what a random run reaches only now and then: seven
    /// bytes for two readers, so that the second finds nothing and sleeps
    /// again; the other end's close under two readers, so that both read end
    /// of file; and a reset under two writers, whose read is `ECONNRESET` and
    /// whose second writer's finish, the error taken, is `EPIPE`.
    let private openingGen : Gen<BlockingConnectionOp list> =
        Gen.frequency
            [
                6, Gen.constant []
                1, Gen.constant (twoReaders (BlockingConnectionOp.Write (0, 1, 7)))
                1, Gen.constant (twoReaders (BlockingConnectionOp.Close 1))
                1, Gen.constant resetUnderWriters
            ]

    /// TCP buffers as small as each flavour admits.
    let private small (image : UnixBootImage<int, string>) : UnixBootImage<int, string> =
        match SimulatedUnixPlatform.flavour (UnixBootImage.platform image) with
        | SimulatedUnixFlavour.Linux ->
            image
            |> UnixBootImage.withTcpSendSpaceMax (Some 30000)
            |> Configured.expectOk TcpSendSpaceMaxRefusal.describe
            |> UnixBootImage.withTcpReceiveSpace (Some 10000)
            |> Configured.expectOk TcpReceiveSpaceRefusal.describe
        | SimulatedUnixFlavour.Darwin ->
            image
            |> UnixBootImage.withTcpSendSpace (Some UnixMachineState.darwinLoopbackSendPipe)
            |> Configured.expectOk TcpSendSpaceRefusal.describe
            |> UnixBootImage.withTcpReceiveSpace (Some UnixMachineState.darwinLoopbackReceivePipe)
            |> Configured.expectOk TcpReceiveSpaceRefusal.describe

    /// A system on `platform` with tasks 0 to 3, `SIGPIPE` ignored and
    /// `SIGUSR1` caught, its handler installed with `SA_RESTART` if `restart`,
    /// and TCP buffers as small as the flavour admits.
    let private systemOn (platform : SimulatedUnixPlatform) (restart : bool) : UnixSystem<int, string> =
        let system =
            UnixSystem.initial<int, string> platform
            |> small
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> fun system -> (tasks, system) ||> List.foldBack Tasks.ensure

        { system with
            Process =
                { system.Process with
                    Signals =
                        system.Process.Signals
                        |> SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore
                        |> SignalState.setDisposition
                            Signal.SIGUSR1
                            (SignalDisposition.Catch
                                { SignalCatch.ofHandler "h" with
                                    Restart = restart
                                })
                }
        }

    /// A connected pair on `system`, both ends blocking: the connecting
    /// socket's descriptor, then the accepted one's.
    let private pair (system : UnixSystem<int, string>) : int * int * UnixSystem<int, string> =
        let listener, system = KeventWorld.listenerAt port system
        let client, system = KeventWorld.stream false system

        let system =
            match KeventWorld.connect client port system with
            | ConnectOutcome.Completed, system -> system
            | other, _ -> failwith $"connect: %A{other}"

        let server, system = KeventWorld.accept listener system
        let system = KeventWorld.close listener system
        let _, system = UnixDescriptor.setNonBlocking server false system
        client, server, system

    let private connectionOf (fd : int) (system : UnixSystem<int, string>) : ConnectionId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) ->
            match SocketPhase.connectionEnd (UnixMachineState.socket socketId system.Machine).Phase with
            | Some (connection, _) -> connection
            | None -> failwith $"fd %d{fd} is not connected"
        | other -> failwith $"fd %d{fd} names %A{other}"

    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socketId) -> socketId
        | other -> failwith $"fd %d{fd} names %A{other}"

    /// `SO_LINGER` on `fd`: `l_onoff` of `onOff` and a time of `seconds`,
    /// through `SO_LINGER_SEC` where the flavour has it and `SO_LINGER`
    /// otherwise. `None` if the call fails with an errno, which changes
    /// nothing (Darwin's `EINVAL` once the connection has gone, for one).
    let private tryLinger
        (fd : int)
        (onOff : int)
        (seconds : int)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string> option
        =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.socketOptionLevel platform

        let name =
            match SimulatedUnixPlatform.lingerSecondsOption platform with
            | Some name -> name
            | None -> SimulatedUnixPlatform.lingerOption platform

        let value = OptionValue.ofLinger onOff seconds
        let length = uint32 value.Length

        match UnixSocket.admitSetSockOpt fd level name UserBuffer.Mapped length system with
        | Ok (SetSockOptAdmission.Answered _) -> None
        | Ok (SetSockOptAdmission.Transfer count) ->
            let supplied = Some (ImmutableArray.Create (value, 0, count))

            match UnixSocket.setsockopt fd level name UserBuffer.Mapped length supplied system with
            | Ok (SetSockOptAnswer.Set, system) -> Some system
            | Ok (SetSockOptAnswer.Failed _, _) -> None
            | other -> failwith $"setting SO_LINGER on fd %d{fd}: %A{other}"
        | other -> failwith $"admitting SO_LINGER on fd %d{fd}: %A{other}"

    /// `tryLinger`, which must succeed.
    let private lingerFor (fd : int) (onOff : int) (seconds : int) (system : UnixSystem<int, string>) =
        match tryLinger fd onOff seconds system with
        | Some system -> system
        | None -> failwith $"setting SO_LINGER on fd %d{fd} failed"

    let private label (flavour : string) (what : string) (seen : Seen) : string =
        let kind =
            match seen with
            | Seen.ReadBytes [] -> "end of file"
            | Seen.ReadBytes _ -> "bytes"
            | Seen.Wrote _ -> "wrote"
            | Seen.Failed error -> $"%O{error}"
            | Seen.Sleeps -> "sleeps"
            | Seen.Restarts -> "restarts"
            | Seen.RefusedSignal -> "refused beside a signal"
            | Seen.RefusedNonBlocking -> "refused, non-blocking now"
            | Seen.RefusedClose -> "close refused"

        $"%s{flavour} %s{what}: %s{kind}"

    [<Test>]
    let ``blocking connection transfers park, wake and finish as the reference says`` () : unit =
        let property
            (cover : string -> unit)
            (platform : SimulatedUnixPlatform, restart : bool, ops : BlockingConnectionOp list)
            : unit
            =
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
            let flavourName = if linux then "Linux" else "Darwin"
            let client, server, start = pair (systemOn platform restart)
            let connection = connectionOf client start
            let mutable system = start

            let transfer = (UnixMachineState.connection connection start.Machine).Transfer

            let mutable reference =
                {
                    Linux = linux
                    Restart = restart
                    Model =
                        TcpTransferReference.create
                            linux
                            transfer.ToServer.SendCapacity
                            transfer.ToServer.ReceiveCapacity
                    Fds = Map.ofList [ client, ConnectionEnd.Client ; server, ConnectionEnd.Server ]
                    NonBlocking = Map.ofList [ ConnectionEnd.Client, false ; ConnectionEnd.Server, false ]
                    Parks = Map.empty
                    Signalled = Set.empty
                    Writes = 0
                    Gone = Set.empty
                    Marked = Set.empty
                    Linger = Map.empty
                }

            // Both ends' capacities are the same, so one pair describes both.
            transfer.ToClient.SendCapacity |> shouldEqual transfer.ToServer.SendCapacity

            transfer.ToClient.ReceiveCapacity
            |> shouldEqual transfer.ToServer.ReceiveCapacity

            let mutable stopped = false

            let settle (task : int) (seen : Seen) (after : UnixSystem<int, string>) =
                match seen with
                | Seen.Sleeps -> after
                | Seen.ReadBytes _
                | Seen.Wrote _
                | Seen.Failed _
                | Seen.Restarts -> returnToUser task after
                | Seen.RefusedSignal
                | Seen.RefusedNonBlocking
                | Seen.RefusedClose -> failwith "a refusal leaves no system"

            for i, op in List.indexed ops |> Seq.takeWhile (fun _ -> not stopped) do
                let pick (eligible : int -> bool) (index : int) : int option =
                    match List.filter eligible tasks with
                    | [] -> None
                    | candidates -> Some candidates.[index % List.length candidates]

                let fdAt (index : int) : int option =
                    match Map.keys reference.Fds |> List.ofSeq with
                    | [] -> None
                    | fds -> Some fds.[index % fds.Length]

                let idle (task : int) =
                    not (Map.containsKey task reference.Parks)

                let inCall (task : int) = Map.containsKey task reference.Parks

                let woken (task : int) =
                    Map.tryFind task reference.Parks |> Option.exists (fun park -> park.Woken)

                let resolved =
                    match op with
                    | BlockingConnectionOp.Read (index, fd, count) ->
                        Option.map2 (fun t fd -> BlockingConnectionOp.Read (t, fd, count)) (pick idle index) (fdAt fd)
                    | BlockingConnectionOp.Write (index, fd, count) ->
                        Option.map2 (fun t fd -> BlockingConnectionOp.Write (t, fd, count)) (pick idle index) (fdAt fd)
                    | BlockingConnectionOp.Receive (index, fd, count, peek, dontWait) ->
                        Option.map2
                            (fun t fd -> BlockingConnectionOp.Receive (t, fd, count, peek, dontWait))
                            (pick idle index)
                            (fdAt fd)
                    | BlockingConnectionOp.Send (index, fd, count, noSignal, dontWait) ->
                        Option.map2
                            (fun t fd -> BlockingConnectionOp.Send (t, fd, count, noSignal, dontWait))
                            (pick idle index)
                            (fdAt fd)
                    | BlockingConnectionOp.Close fd -> fdAt fd |> Option.map BlockingConnectionOp.Close
                    | BlockingConnectionOp.Dup fd -> fdAt fd |> Option.map BlockingConnectionOp.Dup
                    | BlockingConnectionOp.SetNonBlocking (fd, value) ->
                        fdAt fd
                        |> Option.map (fun fd -> BlockingConnectionOp.SetNonBlocking (fd, value))
                    | BlockingConnectionOp.Linger (fd, on, seconds) ->
                        fdAt fd |> Option.map (fun fd -> BlockingConnectionOp.Linger (fd, on, seconds))
                    | BlockingConnectionOp.Signal index -> pick inCall index |> Option.map BlockingConnectionOp.Signal
                    | BlockingConnectionOp.Finish index -> pick woken index |> Option.map BlockingConnectionOp.Finish
                    | BlockingConnectionOp.Wake -> Some op

                match resolved with
                | None -> ()
                | Some op ->

                let where = $"%O{platform}, restart %b{restart}, op %d{i} (%A{op})"

                let compare (expected : Seen) (actual : Seen) =
                    if expected <> actual then
                        failwith $"%s{where}: expected %A{expected}, got %A{actual}"

                match op with
                | BlockingConnectionOp.Read (task, fd, count) ->
                    let expected, after =
                        referenceRead task fd count TcpReceiveCall.Read false reference

                    let seen, actual =
                        fromRead (UnixReadWrite.read task fd UserBuffer.Mapped (uint64 count) system)

                    compare expected seen
                    cover (label flavourName "read" seen)
                    system <- settle task seen (Option.get actual)
                    reference <- after
                | BlockingConnectionOp.Write (task, fd, count) ->
                    let bytes = payload reference.Writes count

                    let expected, after = referenceWrite task fd count TcpSendCall.Write false reference

                    let seen, raised, actual =
                        fromWriteRaising (
                            WriteOutcomes.admitThenWrite
                                task
                                fd
                                UserBuffer.Mapped
                                (ImmutableArray.CreateRange bytes)
                                system
                        )

                    compare expected seen

                    if raised <> raises TcpSendCall.Write seen then
                        failwith $"%s{where}: raised SIGPIPE %b{raised} answering %A{seen}"

                    match seen, Map.tryFind task after.Parks with
                    | Seen.Sleeps,
                      Some {
                               Call = Call.Writing (_, _, written, _)
                           } when written > 0 -> cover $"%s{flavourName} write: sleeps having taken some"
                    | _ -> cover (label flavourName "write" seen)

                    system <- settle task seen (Option.get actual)
                    reference <- after
                | BlockingConnectionOp.Receive (task, fd, count, peek, dontWait) ->
                    let call = if peek then TcpReceiveCall.Peek else TcpReceiveCall.Receive

                    let expected, after = referenceRead task fd count call dontWait reference

                    let flags =
                        flagWord
                            platform
                            [
                                if peek then
                                    MessageFlag.Peek
                                if dontWait then
                                    MessageFlag.DontWait
                            ]

                    let seen, actual =
                        match UnixReadWrite.recv task fd UserBuffer.Mapped (uint64 count) flags system with
                        | Error refusal -> failwith $"%s{where}: recv refused: %s{ReceiveRefusal.describe refusal}"
                        | Ok outcome -> fromRead (Ok outcome)

                    compare expected seen

                    let what =
                        match peek, dontWait with
                        | true, _ -> "peek"
                        | false, true -> "recv, MSG_DONTWAIT"
                        | false, false -> if count = 0 then "recv of nothing" else "recv"

                    cover (label flavourName what seen)
                    system <- settle task seen (Option.get actual)
                    reference <- after
                | BlockingConnectionOp.Send (task, fd, count, noSignal, dontWait) ->
                    let bytes = payload reference.Writes count
                    let call = TcpSendCall.Send noSignal
                    let expected, after = referenceWrite task fd count call dontWait reference

                    let flags =
                        flagWord
                            platform
                            [
                                if noSignal then
                                    MessageFlag.NoSignal
                                if dontWait then
                                    MessageFlag.DontWait
                            ]

                    let seen, raised, actual =
                        fromWriteRaising (
                            WriteOutcomes.admitThenSend
                                task
                                fd
                                UserBuffer.Mapped
                                (ImmutableArray.CreateRange bytes)
                                flags
                                system
                            |> ofSend
                        )

                    compare expected seen

                    if raised <> raises call seen then
                        failwith $"%s{where}: raised SIGPIPE %b{raised} answering %A{seen}"

                    let what =
                        match noSignal, dontWait with
                        | true, _ -> "send, MSG_NOSIGNAL"
                        | false, true -> "send, MSG_DONTWAIT"
                        | false, false -> "send"

                    cover (label flavourName what seen)
                    system <- settle task seen (Option.get actual)
                    reference <- after
                | BlockingConnectionOp.Close fd ->
                    // Darwin's close ends every call made through `fd`, unless
                    // something had already woken one, which is refused.
                    let through =
                        if linux then
                            []
                        else
                            reference.Parks
                            |> Map.filter (fun _ park -> park.Through = Some fd)
                            |> Map.keys
                            |> List.ofSeq

                    let refusedFor = through |> List.tryFind (fun task -> wakesNow task reference)

                    // Under `SO_LINGER`, the last descriptor's close: refused
                    // while a call the close does not end holds the socket,
                    // and otherwise refused where the socket's own close would
                    // reset the connection or wait.
                    let closing = reference.Fds.[fd]
                    let socket = socketOf fd system

                    let lingerRefusal =
                        match Map.tryFind closing reference.Linger with
                        | Some seconds when reference.Fds |> Map.filter (fun _ e -> e = closing) |> Map.count = 1 ->
                            let holder =
                                reference.Parks
                                |> Map.tryFindKey (fun task park ->
                                    endOf park.Call = closing
                                    && park.Through.IsSome
                                    && not (List.contains task through)
                                )

                            let unsent = List.length reference.Model.Flows.[other closing].InFlight

                            match holder with
                            | Some task -> Some (CloseRefusal.LingeringCloseDeferredToCall (socket, task))
                            | None when seconds = 0 && not (Set.contains (other closing) reference.Gone) ->
                                Some (
                                    CloseRefusal.Release (
                                        DescriptionReleaseRefusal.AbortiveClose (socket, connectionOf fd system)
                                    )
                                )
                            | None when seconds > 0 && unsent > 0 && (linux || not reference.NonBlocking.[closing]) ->
                                Some (
                                    CloseRefusal.Release (
                                        DescriptionReleaseRefusal.LingeringClose (
                                            socket,
                                            connectionOf fd system,
                                            unsent
                                        )
                                    )
                                )
                            | None -> None
                        | Some _
                        | None -> None

                    match refusedFor, lingerRefusal, UnixDescriptor.close fd system with
                    | Some task, _, Error (CloseRefusal.DarwinWokenTransfer (_, refused)) ->
                        refused |> shouldEqual task
                        cover (label flavourName "close" Seen.RefusedClose)
                    | None, Some expected, Error refusal ->
                        refusal |> shouldEqual expected

                        match refusal with
                        | CloseRefusal.LingeringCloseDeferredToCall _ ->
                            cover $"%s{flavourName} close: refused, deferred to a call under SO_LINGER"
                        | CloseRefusal.Release (DescriptionReleaseRefusal.AbortiveClose _) ->
                            cover $"%s{flavourName} close: refused, abortive"
                        | CloseRefusal.Release (DescriptionReleaseRefusal.LingeringClose _) ->
                            cover $"%s{flavourName} close: refused, lingering"
                        | _ -> ()

                        // A refusal: the client goes no further.
                        stopped <- true
                    | None, None, Ok (answer, after) ->
                        answer |> shouldEqual (SyscallAnswer.Completed 0L)

                        for task in through do
                            match reference.Parks.[task].Call with
                            | Call.Reading _ -> cover "Darwin close: ends a read"
                            | Call.Writing _ -> cover "Darwin close: ends a write"

                        if
                            linux
                            && reference.Parks
                               |> Map.exists (fun _ park -> endOf park.Call = reference.Fds.[fd])
                            && reference.Fds |> Map.filter (fun _ e -> e = reference.Fds.[fd]) |> Map.count = 1
                        then
                            cover "Linux close: the last descriptor onto a sleeping call's socket"

                        if
                            not (
                                Set.contains
                                    closing
                                    (live
                                        { reference with
                                            Fds = Map.remove fd reference.Fds
                                        })
                            )
                        then
                            cover $"%s{flavourName} close: closes the socket"

                        system <- after

                        // A write a Darwin close ends having moved bytes
                        // returns having written, which marks the description.
                        let marked =
                            (reference.Marked, through)
                            ||> List.fold (fun marked task ->
                                match reference.Parks.[task].Call with
                                | Call.Writing (connectionEnd, _, written, TcpSendCall.Write) when written > 0 ->
                                    Set.add connectionEnd marked
                                | Call.Writing _
                                | Call.Reading _ -> marked
                            )

                        reference <-
                            { reference with
                                Marked = marked
                                Fds = Map.remove fd reference.Fds
                                Parks =
                                    (reference.Parks, through)
                                    ||> List.fold (fun parks task ->
                                        Map.add
                                            task
                                            { parks.[task] with
                                                Through = None
                                            }
                                            parks
                                    )
                            }
                            |> releaseUnreferenced
                    | expected, lingering, other ->
                        failwith $"%s{where}: close expected refusal for %A{expected}, or %A{lingering}, got %A{other}"
                | BlockingConnectionOp.Dup fd ->
                    let copy, after = KeventWorld.dup fd system
                    system <- after

                    reference <-
                        { reference with
                            Fds = Map.add copy reference.Fds.[fd] reference.Fds
                        }
                | BlockingConnectionOp.Linger (fd, on, seconds) ->
                    match tryLinger fd (if on then 1 else 0) seconds system with
                    | None -> cover $"%s{flavourName} SO_LINGER: fails"
                    | Some after ->

                    system <- after

                    reference <-
                        { reference with
                            Linger =
                                if on then
                                    Map.add reference.Fds.[fd] seconds reference.Linger
                                else
                                    Map.remove reference.Fds.[fd] reference.Linger
                        }
                | BlockingConnectionOp.SetNonBlocking (fd, value) ->
                    let _, after = UnixDescriptor.setNonBlocking fd value system
                    system <- after

                    reference <-
                        { reference with
                            NonBlocking = Map.add reference.Fds.[fd] value reference.NonBlocking
                        }
                | BlockingConnectionOp.Signal task ->
                    system <-
                        { system with
                            Process =
                                { system.Process with
                                    Signals =
                                        SignalState.enqueue
                                            {
                                                Signal = Signal.SIGUSR1
                                                Target = ValueSome task
                                            }
                                            system.Process.Signals
                                }
                        }

                    reference <-
                        { reference with
                            Signalled = Set.add task reference.Signalled
                        }
                | BlockingConnectionOp.Wake ->
                    let asleep =
                        reference.Parks
                        |> Map.filter (fun _ park -> not park.Woken)
                        |> Map.keys
                        |> Set.ofSeq

                    let ordinals =
                        asleep
                        |> Seq.map (fun task ->
                            let (ParkOrdinal.ParkOrdinal ordinal) =
                                (UnixTaskTable.parkOf task system.Tasks |> Option.get).Ordinal

                            task, ordinal
                        )
                        |> Map.ofSeq

                    let woken = UnixWait.wakes asleep system |> List.map fst
                    let expected = referenceWakes ordinals reference

                    if woken <> expected then
                        failwith $"%s{where}: woke %A{woken}, expected %A{expected}"

                    if List.length woken > 1 then
                        cover $"%s{flavourName} wake: several"

                    for task in woken do
                        match reference.Parks.[task].Call with
                        | Call.Writing (connectionEnd, payload, written, _) when
                            not (Set.contains task reference.Signalled)
                            && not reference.Model.Ends.[connectionEnd].GotReset
                            && reference.Parks.[task].Through.IsSome
                            ->
                            if (takes connectionEnd (List.length payload - written) reference).IsSome then
                                cover $"%s{flavourName} wake: a writer, by room"
                        | _ -> ()

                    // A Linux writer with room that is not yet woken: the queue
                    // is above two thirds, and some of it free.
                    if linux then
                        for task in asleep do
                            match reference.Parks.[task].Call with
                            | Call.Writing (connectionEnd, payload, written, _) when
                                not (List.contains task woken)
                                && (takes connectionEnd (List.length payload - written) reference).IsSome
                                ->
                                cover "Linux wake: a writer with room left asleep"
                            | _ -> ()

                    reference <-
                        { reference with
                            Parks =
                                (reference.Parks, woken)
                                ||> List.fold (fun parks task ->
                                    Map.add
                                        task
                                        { parks.[task] with
                                            Woken = true
                                        }
                                        parks
                                )
                        }
                | BlockingConnectionOp.Finish task ->
                    let park = reference.Parks.[task]
                    let expected, after = referenceFinish task reference

                    let seen, actual =
                        match park.Call with
                        | Call.Reading _ -> fromRead (UnixReadWrite.finishRead task system)
                        | Call.Writing (_, payload, _, call) ->
                            let seen, raised, actual = fromWriteRaising (libraryFinishWrite task payload system)

                            // A close that ended the call raised nothing.
                            let raisedExpected = park.Through.IsSome && raises call seen

                            if raised <> raisedExpected then
                                failwith $"%s{where}: finishing raised SIGPIPE %b{raised} answering %A{seen}"

                            seen, actual

                    compare expected seen

                    // The read and write calls keep their labels; recv and
                    // send name themselves.
                    let finishing =
                        match park.Call with
                        | Call.Reading (_, _, TcpReceiveCall.Read) -> "finish read"
                        | Call.Reading (_, _, TcpReceiveCall.Receive) -> "finish recv"
                        | Call.Reading (_, _, TcpReceiveCall.Peek) -> "finish peek"
                        | Call.Writing (_, _, _, TcpSendCall.Write) -> "finish write"
                        | Call.Writing (_, _, _, TcpSendCall.Send false) -> "finish send"
                        | Call.Writing (_, _, _, TcpSendCall.Send true) -> "finish send, MSG_NOSIGNAL"

                    let what =
                        match park.Call, seen with
                        | _, _ when park.Through.IsNone -> $"%s{finishing} ended by a close"
                        | Call.Writing (_, payload, written, _), Seen.Wrote n when
                            int n < List.length payload && int n = written
                            ->
                            $"%s{finishing}, with the count already taken"
                        | Call.Writing (_, _, written, _), Seen.Sleeps when
                            (match Map.tryFind task after.Parks with
                             | Some {
                                        Call = Call.Writing (_, _, now, _)
                                    } -> now > written
                             | _ -> false)
                            ->
                            $"%s{finishing}, more taken"
                        | _ -> finishing

                    cover (label flavourName what seen)

                    if
                        Set.contains (endOf park.Call) (live reference)
                        && not (Set.contains (endOf park.Call) (live after))
                    then
                        cover $"%s{flavourName} finish: the call's return closes the socket"

                    match actual with
                    | None ->
                        // A refusal: the client goes no further.
                        stopped <- true
                    | Some actual -> system <- settle task seen actual

                    reference <- after

                if not stopped then
                    match UnixSystem.checkInvariants system with
                    | [] -> ()
                    | defects -> failwith $"%s{where}: %A{defects}"

                    for task in tasks do
                        let expected =
                            Map.tryFind task reference.Parks
                            |> Option.map (fun park ->
                                match park.Call with
                                | Call.Reading (_, count, call) -> 0, count, 0, $"%A{call}"
                                | Call.Writing (_, payload, written, call) ->
                                    1, List.length payload, written, $"%A{call}"
                            )

                        let actual =
                            UnixTaskTable.parkedFor task system.Tasks
                            |> Option.map (fun parked ->
                                match parked with
                                | ParkedSyscall.ConnectionRead read -> 0, read.Count, 0, $"%A{read.Call}"
                                | ParkedSyscall.ConnectionWrite write ->
                                    1, write.Count, write.Written, $"%A{write.Call}"
                                | other -> failwith $"%s{where}: task %d{task} parked in %A{other}"
                            )

                        if expected <> actual then
                            failwith $"%s{where}: task %d{task} parked as %A{actual}, expected %A{expected}"

                    // The socket of an end goes exactly when nothing references
                    // its description.
                    let socketsLeft =
                        system.Machine.Sockets
                        |> Map.filter (fun _ socket -> (SocketPhase.connectionEnd socket.Phase).IsSome)
                        |> Map.count

                    socketsLeft |> shouldEqual (2 - Set.count reference.Gone)

                    // Darwin's `FWASWRITTEN`, on each end a descriptor still
                    // names: set by a `write` that moved bytes, never by a
                    // `send`, and never on Linux.
                    for KeyValue (fd, connectionEnd) in reference.Fds do
                        let written =
                            match FileDescriptorRegistry.tryFindWithId fd (UnixSystemState.fileDescriptors system) with
                            | Some (_, description) -> description.Status.Written
                            | None -> failwith $"%s{where}: fd %d{fd} names no description"

                        if written <> Set.contains connectionEnd reference.Marked then
                            failwith $"%s{where}: fd %d{fd}'s description marked written %b{written}"

                        if written then
                            cover $"%s{flavourName}: a description marked written"

        let gen =
            gen {
                let! platform = Gen.elements Machines.platforms
                let! restart = Gen.elements [ true ; false ]
                let! closes = Gen.elements [ 0 ; 1 ; 2 ]
                let! opening = openingGen
                let! ops = Gen.listOfLength 80 (opGen closes)
                return platform, restart, opening @ ops
            }

        // The floors below are a claim about the generator, so they are
        // counted over a fixed sample: a run cannot miss one by chance.
        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 400) (Arb.fromGen gen) property

        // Each reached by the fixed sample, whose counts `CoverageSample.check`
        // prints; a change that loses one fails on every run, and wants
        // an opening in `openingGen` rather than another seed. The recv and
        // send rows' rarer finishes are `tcp-recv-send.c`'s rows, held one at
        // a time below. Darwin's buffers are the larger, so its writers sleep
        // less often, and those of its paths, like a finish's ECONNRESET and
        // the non-blocking refusals, are reached only now and then: the rows
        // below hold each of them, and the property checks every one it
        // reaches.
        for flavour in [ "Linux" ; "Darwin" ] do
            for what in
                [
                    "read: sleeps"
                    "read: bytes"
                    "read: end of file"
                    "read: ECONNRESET"
                    "read: EAGAIN"
                    "write: wrote"
                    "write: EAGAIN"
                    "write: EPIPE"
                    "write: sleeps"
                    "write: sleeps having taken some"
                    "finish read: bytes"
                    "finish read: end of file"
                    "finish read: EINTR"
                    "finish read: restarts"
                    "wake: several"
                    "close: closes the socket"
                    "recv: sleeps"
                    "recv: bytes"
                    "recv, MSG_DONTWAIT: EAGAIN"
                    "recv, MSG_DONTWAIT: bytes"
                    "recv of nothing: end of file"
                    "peek: sleeps"
                    "peek: bytes"
                    "finish recv: bytes"
                    "finish peek: bytes"
                    "send: wrote"
                    "send: sleeps"
                    "send: EPIPE"
                    "send, MSG_NOSIGNAL: EPIPE"
                    "send, MSG_DONTWAIT: wrote"
                ] do
                if coverage.Count $"%s{flavour} %s{what}" = 0 then
                    failwith $"never reached: %s{flavour} %s{what}"

        for label in
            [
                "Linux finish read: sleeps"
                "Linux finish write: sleeps"
                "Linux finish write, more taken: sleeps"
                "Linux finish write, with the count already taken: wrote"
                "Linux finish write: EINTR"
                "Linux finish write: restarts"
                "Linux finish write: EPIPE"
                "Linux finish write: refused, non-blocking now"
                "Linux write: ECONNRESET"
                "Linux wake: a writer, by room"
                "Linux wake: a writer with room left asleep"
                // A Darwin close ends the calls made through it, which then
                // hold nothing, so only a Linux call's return can close.
                "Linux finish: the call's return closes the socket"
                "Linux close: the last descriptor onto a sleeping call's socket"
                // The other refusals under SO_LINGER are reached only now
                // and then; `TestConnectedTransfer` holds their rows.
                "Linux close: refused, deferred to a call under SO_LINGER"
                "Darwin finish read: sleeps"
                "Darwin finish read: refused beside a signal"
                "Darwin close: ends a read"
                "Darwin close: close refused"
                "Darwin finish read ended by a close: EBADF"
                // A Linux recv of nothing sleeps; Darwin's answers at once.
                "Linux recv of nothing: sleeps"
                // MSG_DONTWAIT: Linux's send answers EAGAIN, Darwin's sleeps.
                "Linux send, MSG_DONTWAIT: EAGAIN"
                "Darwin send, MSG_DONTWAIT: sleeps"
                "Darwin: a description marked written"
            ] do
            if coverage.Count label = 0 then
                failwith $"never reached: %s{label}"

    // ------------------------------------------------------------------
    // The rows of `tcp-blocking.c`, one at a time
    // ------------------------------------------------------------------

    /// `task` reads up to `count` bytes through `fd`, and sleeps.
    let private asleepReading
        (task : int)
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match UnixReadWrite.read task fd UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadOutcome.WouldBlock _, system) -> system
        | other -> failwith $"expected task %d{task}'s read to sleep, got %A{other}"

    /// `task` writes `bytes` through `fd`, and sleeps.
    let private asleepWriting
        (task : int)
        (fd : int)
        (bytes : byte list)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match WriteOutcomes.admitThenWrite task fd UserBuffer.Mapped (ImmutableArray.CreateRange bytes) system with
        | Ok (WriteOutcome.WouldBlock (_, system)) -> system
        | other -> failwith $"expected task %d{task}'s write to sleep, got %A{other}"

    /// A write of `count` bytes through `fd` by task 0 that returns, and its
    /// count.
    let private wrote (fd : int) (count : int) (system : UnixSystem<int, string>) : int64 * UnixSystem<int, string> =
        match
            WriteOutcomes.admitThenWrite 0 fd UserBuffer.Mapped (ImmutableArray.CreateRange (payload 99 count)) system
        with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, system)) -> n, system
        | other -> failwith $"expected a write of %d{count} through fd %d{fd} to return, got %A{other}"

    /// A read of up to `count` bytes through `fd` by task 0, which has
    /// something to answer.
    let private readNow
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : ReadAnswer * UnixSystem<int, string>
        =
        match UnixReadWrite.read 0 fd UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadOutcome.Answered answer, system) -> answer, system
        | other -> failwith $"expected a read through fd %d{fd} to answer, got %A{other}"

    let private wokenAmong (asleep : int list) (system : UnixSystem<int, string>) : int list =
        UnixWait.wakes (Set.ofList asleep) system |> List.map fst

    /// The woken write of `task`, finished, through `libraryFinishWrite`.
    let private finishedWrite
        (task : int)
        (bytes : byte list)
        (system : UnixSystem<int, string>)
        : Result<WriteOutcome<WriteAnswer, int, string>, WriteRefusal>
        =
        libraryFinishWrite task bytes system

    let private signalled (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals =
                        SignalState.enqueue
                            {
                                Signal = Signal.SIGUSR1
                                Target = ValueSome task
                            }
                            system.Process.Signals
                }
        }

    /// The connection's bytes in the client's send buffer, still to reach the
    /// server's receive buffer.
    let private clientQueued (connection : ConnectionId) (system : UnixSystem<int, string>) : int =
        ByteQueue.length (UnixMachineState.connection connection system.Machine).Transfer.ToServer.Sending

    let private pendingError
        (connection : ConnectionId)
        (connectionEnd : ConnectionEnd)
        (system : UnixSystem<int, string>)
        : TcpError option
        =
        TcpTransfer.pendingError connectionEnd (UnixMachineState.connection connection system.Machine).Transfer

    /// Read through `fd` by task 0 until nothing is left to read.
    let rec private drained (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let connection = connectionOf fd system

        let connectionEnd =
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Socket socketId) ->
                SocketPhase.connectionEnd (UnixMachineState.socket socketId system.Machine).Phase
                |> Option.get
                |> snd
            | other -> failwith $"fd %d{fd} names %A{other}"

        if TcpTransfer.readable connectionEnd (UnixMachineState.connection connection system.Machine).Transfer = 0 then
            system
        else
            readNow fd 65536 system |> snd |> drained fd

    /// R-data, R-fin and R-reset: a read asleep on a connection wakes for
    /// bytes, a FIN and a reset, and answers each as a read made then would.
    [<Test>]
    let ``a sleeping read wakes for bytes, a FIN or a reset, and answers it`` () : unit =
        for platform in Machines.platforms do
            for arrival in [ "data" ; "fin" ; "reset" ] do
                let client, server, system = pair (systemOn platform false)

                // For the reset, the server leaves bytes from the client unread.
                let system =
                    if arrival = "reset" then
                        wrote client 100 system |> snd
                    else
                        system

                let system = asleepReading 1 client 4096 system
                wokenAmong [ 1 ] system |> shouldEqual []

                let system =
                    match arrival with
                    | "data" -> wrote server 100 system |> snd
                    | _ -> KeventWorld.close server system

                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                let expected =
                    match arrival with
                    | "data" -> ReadAnswer.Completed (ImmutableArray.CreateRange (payload 99 100))
                    | "fin" -> ReadAnswer.Completed ImmutableArray.Empty
                    | _ -> ReadAnswer.Failed UnixError.ECONNRESET

                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered answer, after) ->
                    answer |> shouldEqual expected
                    UnixTaskTable.parkOf 1 after.Tasks |> shouldEqual None
                    UnixSystem.checkInvariants after |> shouldEqual []

                    // Measured: after the reset's error, end of file.
                    if arrival = "reset" then
                        readNow client 4096 after
                        |> fst
                        |> shouldEqual (ReadAnswer.Completed ImmutableArray.Empty)
                | other -> failwith $"%O{platform} %s{arrival}: %A{other}"

    /// R-eintr and R-restart, W-empty: a signal ends a read with nothing to
    /// answer, and a write that took nothing, with EINTR, or restarts it under
    /// SA_RESTART.
    [<Test>]
    let ``a signal ends a sleeping transfer that has moved nothing with EINTR, or restarts it`` () : unit =
        for platform in Machines.platforms do
            for restart in [ false ; true ] do
                let client, server, system = pair (systemOn platform restart)
                let system = asleepReading 1 client 4096 system |> signalled 1
                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EINTR), _) when not restart -> ()
                | Ok (ReadOutcome.Restarts, after) when restart ->
                    UnixTaskTable.parkOf 1 after.Tasks |> shouldEqual None
                | other -> failwith $"%O{platform} restart %b{restart}: the read came to %A{other}"

                // The server's end full, so a write by it takes nothing.
                let _, system = UnixDescriptor.setNonBlocking server true system

                let rec fill (system : UnixSystem<int, string>) =
                    match
                        WriteOutcomes.admitThenWrite
                            0
                            server
                            UserBuffer.Mapped
                            (ImmutableArray.CreateRange (payload 1 65536))
                            system
                    with
                    | Ok (WriteOutcome.Returns (WriteAnswer.Completed _, system)) -> fill system
                    | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system)) -> system
                    | other -> failwith $"filling: %A{other}"

                let system = fill system
                let _, system = UnixDescriptor.setNonBlocking server false system
                let system = asleepWriting 2 server (payload 2 1000) system |> signalled 2
                wokenAmong [ 2 ] system |> shouldEqual [ 2 ]

                match finishedWrite 2 (payload 2 1000) system with
                | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EINTR, _)) when not restart -> ()
                | Ok (WriteOutcome.Restarts after) when restart ->
                    UnixTaskTable.parkOf 2 after.Tasks |> shouldEqual None
                | other -> failwith $"%O{platform} restart %b{restart}: the write came to %A{other}"

    /// A signal the sleeping task blocks neither wakes a sleeping transfer nor
    /// ends it: the read answers the bytes that wake it, and the signal stays
    /// pending.
    [<Test>]
    let ``a signal the task blocks leaves a sleeping transfer asleep, and pending`` () : unit =
        for platform in Machines.platforms do
            let numbering = SignalState.numbering (systemOn platform false).Process.Signals
            let usr1 = SignalMask.ofSignals numbering (Set.singleton Signal.SIGUSR1)

            let sigBlock =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> 0
                | SimulatedUnixFlavour.Darwin -> 1

            let blocking (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
                match UnixSignal.pthreadSigmask task sigBlock (Some usr1) system with
                | Ok (_, system) -> system
                | Error errno -> failwith $"%O{platform}: pthread_sigmask: %O{errno}"

            let client, server, system = pair (systemOn platform false)
            let system = system |> blocking 1 |> blocking 2

            let system = asleepReading 1 client 4096 system |> signalled 1
            wokenAmong [ 1 ] system |> shouldEqual []

            let system = wrote server 100 system |> snd
            wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

            let system =
                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                    List.ofSeq bytes |> shouldEqual (payload 99 100)
                    after
                | other -> failwith $"%O{platform}: the read came to %A{other}"

            UnixSignal.sigpending 1 system |> shouldEqual usr1

            // The server's end full, so a write by it takes nothing.
            let _, system = UnixDescriptor.setNonBlocking server true system

            let rec fill (system : UnixSystem<int, string>) =
                match
                    WriteOutcomes.admitThenWrite
                        0
                        server
                        UserBuffer.Mapped
                        (ImmutableArray.CreateRange (payload 1 65536))
                        system
                with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed _, system)) -> fill system
                | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system)) -> system
                | other -> failwith $"filling: %A{other}"

            let system = fill system
            let _, system = UnixDescriptor.setNonBlocking server false system
            let system = asleepWriting 2 server (payload 2 1000) system |> signalled 2
            wokenAmong [ 2 ] system |> shouldEqual []
            UnixSignal.sigpending 2 system |> shouldEqual usr1
            UnixSystem.checkInvariants system |> shouldEqual []

    /// R-order: three readers asleep on one socket all wake for one byte, on
    /// either flavour; the first to finish takes it, and the rest sleep again.
    [<Test>]
    let ``every reader asleep on a socket wakes, and those beaten to the bytes sleep again`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair (systemOn platform false)

            let system =
                system
                |> asleepReading 1 client 1
                |> asleepReading 2 client 1
                |> asleepReading 3 client 1

            let system = wrote server 1 system |> snd
            wokenAmong [ 1 ; 2 ; 3 ] system |> shouldEqual [ 1 ; 2 ; 3 ]

            let system =
                match UnixReadWrite.finishRead 2 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) when bytes.Length = 1 -> after
                | other -> failwith $"%O{platform}: %A{other}"

            for task in [ 1 ; 3 ] do
                match UnixReadWrite.finishRead task system with
                | Ok (ReadOutcome.WouldBlock _, _) -> ()
                | other -> failwith $"%O{platform}: task %d{task} came to %A{other}"

    /// W-resume, Linux: a writer asleep on a full send buffer is not woken
    /// until the buffer has drained to two thirds full, however much the
    /// reader frees before that; then it fills the room, and sleeps again.
    /// Darwin's is woken as soon as a write of what is left would take
    /// something.
    [<Test>]
    let ``a sleeping writer wakes by its flavour's rule, and refills the room`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair (systemOn platform false)
            let connection = connectionOf client system

            let transfer () =
                (UnixMachineState.connection connection system.Machine).Transfer

            let sendCapacity = (transfer ()).ToServer.SendCapacity
            let total = 600000
            let bytes = payload 5 total
            let system = asleepWriting 1 client bytes system
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            // Read 1000 at a time until the writer wakes, checking the rule
            // at every step.
            let rec untilWoken (reads : int) (system : UnixSystem<int, string>) =
                if reads > 10000 then
                    failwith $"%O{platform}: the writer never woke"

                let queued = clientQueued connection system
                let woke = not (List.isEmpty (wokenAmong [ 1 ] system))

                let expected =
                    if linux then
                        sendCapacity - queued >= queued / 2 && queued < sendCapacity
                    else
                        let flow = (UnixMachineState.connection connection system.Machine).Transfer.ToServer

                        let free =
                            sendCapacity - queued + flow.ReceiveCapacity - ByteQueue.length flow.Receiving

                        let remaining =
                            match UnixTaskTable.parkedFor 1 system.Tasks with
                            | Some (ParkedSyscall.ConnectionWrite write) -> write.Count - write.Written
                            | other -> failwith $"%A{other}"

                        free >= 2048 || free >= remaining

                if woke <> expected then
                    failwith $"%O{platform}: with %d{queued} of %d{sendCapacity} queued, woken %b{woke}"

                if woke then
                    system, reads
                else
                    untilWoken (reads + 1) (readNow server 1000 system |> snd)

            let system, reads = untilWoken 0 system

            if linux then
                // Woken only once a third of the buffer was free.
                reads |> shouldBeGreaterThan 3

            // The woken writer takes all the room there is, and sleeps for the
            // rest, its queue full again.
            let system =
                match finishedWrite 1 bytes system with
                | Ok (WriteOutcome.WouldBlock (_, after)) -> after
                | other -> failwith $"%O{platform}: %A{other}"

            clientQueued connection system |> shouldEqual sendCapacity

            // Drained to the end, the write returns every byte it was given,
            // and the reader has had them in order.
            let rec toTheEnd (got : byte list list) (system : UnixSystem<int, string>) =
                let got, system =
                    match readNow server 65536 system with
                    | ReadAnswer.Completed bytes, system -> List.ofSeq bytes :: got, system
                    | other, _ -> failwith $"%A{other}"

                match wokenAmong [ 1 ] system with
                | [] -> toTheEnd got system
                | _ ->
                    match finishedWrite 1 bytes system with
                    | Ok (WriteOutcome.WouldBlock (_, system)) -> toTheEnd got system
                    | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, system)) -> n, got, system
                    | other -> failwith $"%O{platform}: %A{other}"

            let n, got, system = toTheEnd [] system
            n |> shouldEqual (int64 total)

            let rec rest (got : byte list list) (system : UnixSystem<int, string>) =
                match readNow server 65536 system with
                | ReadAnswer.Completed bytes, _ when bytes.IsEmpty -> got
                | ReadAnswer.Completed bytes, system -> rest (List.ofSeq bytes :: got) system
                | other, _ -> failwith $"%A{other}"

            // Closing the writer's end lets the last read see end of file.
            let got = rest got (KeventWorld.close client system)
            let tail = got |> List.rev |> List.concat
            tail |> shouldEqual (List.skip (reads * 1000) bytes)

    /// W-partial: a writer asleep having taken part of its bytes returns
    /// their count when signalled, with SA_RESTART or without.
    [<Test>]
    let ``a signal ends a sleeping write that has taken some with its count`` () : unit =
        for platform in Machines.platforms do
            for restart in [ false ; true ] do
                let client, _, system = pair (systemOn platform restart)
                let connection = connectionOf client system
                let system = asleepWriting 1 client (payload 3 600000) system

                let taken =
                    match UnixTaskTable.parkedFor 1 system.Tasks with
                    | Some (ParkedSyscall.ConnectionWrite write) -> write.Written
                    | other -> failwith $"%A{other}"

                taken |> shouldBeGreaterThan 0
                let system = signalled 1 system

                match finishedWrite 1 (payload 3 600000) system with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, after)) ->
                    n |> shouldEqual (int64 taken)
                    clientQueued connection after |> shouldEqual (clientQueued connection system)
                | other -> failwith $"%O{platform} restart %b{restart}: %A{other}"

    /// D-write and D-read: room and a signal at once. Linux's writer takes
    /// none of the room and returns its count; its reader answers the bytes.
    /// Darwin answers whichever reached the sleeper first, which is refused.
    [<Test>]
    let ``room and a signal at once: Linux's signal wins a write and loses a read`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
            let client, server, system = pair (systemOn platform false)
            let bytes = payload 4 600000
            let system = asleepWriting 1 client bytes system

            let taken =
                match UnixTaskTable.parkedFor 1 system.Tasks with
                | Some (ParkedSyscall.ConnectionWrite write) -> write.Written
                | other -> failwith $"%A{other}"

            let rec untilWoken (system : UnixSystem<int, string>) =
                if List.isEmpty (wokenAmong [ 1 ] system) then
                    untilWoken (readNow server 65536 system |> snd)
                else
                    system

            let system = untilWoken system |> signalled 1

            match finishedWrite 1 bytes system, linux with
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, _)), true -> n |> shouldEqual (int64 taken)
            | Error (WriteRefusal.Interruption (SyscallInterruptionRefusal.SignalBesideCompletion _)), false -> ()
            | other, _ -> failwith $"%O{platform}: the write came to %A{other}"

            let client, server, system = pair (systemOn platform false)
            let system = asleepReading 2 client 4096 system
            let system = wrote server 10 system |> snd |> signalled 2

            match UnixReadWrite.finishRead 2 system, linux with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed got), _), true -> got.Length |> shouldEqual 10
            | Error (ReadRefusal.Interruption (SyscallInterruptionRefusal.SignalBesideCompletion _)), false -> ()
            | other, _ -> failwith $"%O{platform}: the read came to %A{other}"

    /// W-reset: a reset reaches a sleeping writer. Linux's returns the count
    /// it had taken, leaving ECONNRESET pending, or, having taken nothing,
    /// takes ECONNRESET, raising no signal; Darwin's answers EPIPE and raises
    /// SIGPIPE either way, leaving ECONNRESET pending.
    [<Test>]
    let ``a reset ends a sleeping write as each flavour measured`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            for tookSome in [ true ; false ] do
                let client, server, system = pair (systemOn platform false)
                let connection = connectionOf client system

                // The server has a byte of its own unread, so its close resets.
                let system = wrote client 1 system |> snd

                let system =
                    if tookSome then
                        system
                    else
                        let _, system = UnixDescriptor.setNonBlocking client true system

                        let rec fill (system : UnixSystem<int, string>) =
                            match
                                WriteOutcomes.admitThenWrite
                                    0
                                    client
                                    UserBuffer.Mapped
                                    (ImmutableArray.CreateRange (payload 1 65536))
                                    system
                            with
                            | Ok (WriteOutcome.Returns (WriteAnswer.Completed _, system)) -> fill system
                            | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system)) -> system
                            | other -> failwith $"filling: %A{other}"

                        UnixDescriptor.setNonBlocking client false (fill system) |> snd

                let bytes = payload 6 (if tookSome then 600000 else 1000)
                let system = asleepWriting 1 client bytes system

                let taken =
                    match UnixTaskTable.parkedFor 1 system.Tasks with
                    | Some (ParkedSyscall.ConnectionWrite write) -> write.Written
                    | other -> failwith $"%A{other}"

                (taken > 0) |> shouldEqual tookSome

                let system = KeventWorld.close server system
                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                match finishedWrite 1 bytes system, linux with
                | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, after)), true when tookSome ->
                    n |> shouldEqual (int64 taken)

                    pendingError connection ConnectionEnd.Client after
                    |> shouldEqual (Some TcpError.ConnectionReset)
                | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.ECONNRESET, after)), true ->
                    pendingError connection ConnectionEnd.Client after |> shouldEqual None
                | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, raised, after)), false ->
                    raised.Signal |> shouldEqual Signal.SIGPIPE

                    pendingError connection ConnectionEnd.Client after
                    |> shouldEqual (Some TcpError.ConnectionReset)
                | other, _ -> failwith $"%O{platform}, took some %b{tookSome}: %A{other}"

    /// R-close and W-close: closing the descriptor a sleeping read or write
    /// was made through. Darwin's close ends it with EBADF, whatever a write
    /// had taken, raising no signal; Linux's leaves it asleep, holding the
    /// socket, and it answers what arrives.
    [<Test>]
    let ``closing the descriptor of a sleeping transfer ends it on Darwin and not on Linux`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            // The read.
            let client, server, system = pair (systemOn platform false)
            let system = asleepReading 1 client 4096 system
            let system = KeventWorld.close client system
            UnixSystem.checkInvariants system |> shouldEqual []

            if linux then
                wokenAmong [ 1 ] system |> shouldEqual []
                let system = wrote server 100 system |> snd
                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                    bytes.Length |> shouldEqual 100
                    // The call's return let go of the socket's last reference.
                    after.Machine.Sockets.Count |> shouldEqual (system.Machine.Sockets.Count - 1)
                | other -> failwith $"%O{platform}: %A{other}"
            else
                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EBADF), _) -> ()
                | other -> failwith $"%O{platform}: %A{other}"

                // Measured: the peer's write after the close is taken.
                wrote server 100 system |> fst |> shouldEqual 100L

            // The write, having taken some.
            let client, server, system = pair (systemOn platform false)
            let bytes = payload 7 600000
            let system = asleepWriting 1 client bytes system
            let system = KeventWorld.close client system
            UnixSystem.checkInvariants system |> shouldEqual []

            if linux then
                wokenAmong [ 1 ] system |> shouldEqual []
            else
                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                match finishedWrite 1 bytes system with
                | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EBADF, _)) -> ()
                | other -> failwith $"%O{platform}: %A{other}"

            ignore server

    /// A Darwin close of the descriptor of a sleeping transfer something has
    /// already woken is refused: which of the two the kernel answers is
    /// unmeasured.
    [<Test>]
    let ``Darwin: closing the descriptor of a woken transfer is refused`` () : unit =
        let client, server, system = pair (systemOn SimulatedUnixPlatform.macOsArm64 false)
        let system = asleepReading 1 client 4096 system
        let system = wrote server 10 system |> snd

        match UnixDescriptor.close client system with
        | Error (CloseRefusal.DarwinWokenTransfer (_, task)) -> task |> shouldEqual 1
        | other -> failwith $"%A{other}"

    // ------------------------------------------------------------------
    // A close deferred to a sleeping call, under SO_LINGER
    // ------------------------------------------------------------------

    /// A sleeping transfer on `fd`, one of each kind: a read with nothing to
    /// read, and a write with its send buffer full behind a full peer, so that
    /// bytes are unsent.
    let private asleepIn (kind : string) (task : int) (fd : int) (system : UnixSystem<int, string>) =
        match kind with
        | "read" -> asleepReading task fd 4096 system
        | "write" -> asleepWriting task fd (payload 7 600000) system
        | other -> failwith $"no such transfer: %s{other}"

    /// Under Linux a close of the last descriptor onto a socket a call sleeps
    /// on leaves the call holding it (R-close, W-close), so the socket's own
    /// close happens when the call returns. Under `SO_LINGER`, whose either
    /// time this kernel refuses a close under in some state the call can
    /// change, that close is refused at the descriptor's close, through
    /// `close`, `dup2` and `dup3` alike, naming the socket and the call.
    [<Test>]
    let ``Linux: the last close of a socket a call sleeps on under SO_LINGER is refused`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64

        for kind in [ "read" ; "write" ] do
            for seconds in [ 0 ; 1 ] do
                for how in [ "close" ; "dup2" ; "dup3" ] do
                    let row = $"%s{kind}, linger {{1, %d{seconds}}}, %s{how}"
                    let client, _, system = pair (systemOn platform false)
                    let system = lingerFor client 1 seconds system
                    let system = asleepIn kind 1 client system
                    let socket = socketOf client system

                    let refusal =
                        match how with
                        | "close" ->
                            match UnixDescriptor.close client system with
                            | Error refusal -> Some refusal
                            | Ok other -> failwith $"%s{row}: the close answered %A{other}"
                        | "dup2" ->
                            match UnixDescriptor.dup2 0 client system with
                            | Error (Dup2Refusal.ClosingTarget refusal) -> Some refusal
                            | other -> failwith $"%s{row}: %A{other}"
                        | _ ->
                            match UnixDescriptor.dup3 0 client 0 system with
                            | Error (Dup3Refusal.ClosingTarget refusal) -> Some refusal
                            | other -> failwith $"%s{row}: %A{other}"

                    refusal
                    |> shouldEqual (Some (CloseRefusal.LingeringCloseDeferredToCall (socket, 1)))

    /// The refusal is of the last descriptor's close, whichever descriptor the
    /// call was made through: a `dup` keeps the description, and its close
    /// is the one refused.
    [<Test>]
    let ``Linux: under SO_LINGER a close that leaves a descriptor onto the sleeping call's socket is served``
        ()
        : unit
        =
        let client, _, system = pair (systemOn SimulatedUnixPlatform.linuxX64 false)
        let system = lingerFor client 1 0 system
        let copy, system = KeventWorld.dup client system
        let system = asleepReading 1 client 4096 system

        let system =
            match UnixDescriptor.close client system with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"%A{other}"

        UnixSystem.checkInvariants system |> shouldEqual []

        UnixDescriptor.close copy system
        |> shouldEqual (Error (CloseRefusal.LingeringCloseDeferredToCall (socketOf copy system, 1)))

    /// Darwin's close ends the call made through the descriptor, so nothing
    /// holds the socket past it, and the close is refused, if at all, as the
    /// ordinary close of the socket is.
    [<Test>]
    let ``Darwin: the last close of a socket a call sleeps on under SO_LINGER is the socket's own close`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64

        // {1, 0}: the abortive close, refused while the peer is open.
        let client, _, system = pair (systemOn platform false)
        let system = lingerFor client 1 0 system
        let system = asleepReading 1 client 4096 system
        let socket = socketOf client system

        match UnixDescriptor.close client system with
        | Error (CloseRefusal.Release (DescriptionReleaseRefusal.AbortiveClose (refused, _))) ->
            refused |> shouldEqual socket
        | other -> failwith $"{{1, 0}}: %A{other}"

        // {1, 1 s} with bytes unsent, through a blocking description: the
        // close that waits.
        let client, _, system = pair (systemOn platform false)
        let system = lingerFor client 1 1 system
        let system = asleepWriting 1 client (payload 7 600000) system
        let socket = socketOf client system

        match UnixDescriptor.close client system with
        | Error (CloseRefusal.Release (DescriptionReleaseRefusal.LingeringClose (refused, _, unsent))) ->
            refused |> shouldEqual socket
            unsent |> shouldBeGreaterThan 0
        | other -> failwith $"{{1, 1}}: %A{other}"

    /// With `SO_LINGER` off, whatever its time, the close is the ordinary one
    /// whenever it happens: Linux's call keeps the socket past the close of its
    /// last descriptor, answers what arrives, and releases it as it returns.
    [<Test>]
    let ``Linux: with SO_LINGER off, the last close of a socket a call sleeps on is deferred to the call`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64

        for seconds in [ 0 ; 1 ] do
            // The read.
            let client, server, system = pair (systemOn platform false)
            let system = lingerFor client 1 seconds system |> lingerFor client 0 seconds
            let system = asleepReading 1 client 4096 system

            let system =
                match UnixDescriptor.close client system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"read, time %d{seconds}: %A{other}"

            UnixSystem.checkInvariants system |> shouldEqual []
            let system = wrote server 100 system |> snd
            wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

            match UnixReadWrite.finishRead 1 system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                bytes.Length |> shouldEqual 100
                after.Machine.Sockets.Count |> shouldEqual (system.Machine.Sockets.Count - 1)
                UnixSystem.checkInvariants after |> shouldEqual []
            | other -> failwith $"read, time %d{seconds}: %A{other}"

            // The write, with bytes unsent: the peer drains until it wakes.
            let client, server, system = pair (systemOn platform false)
            let system = lingerFor client 1 seconds system |> lingerFor client 0 seconds
            let bytes = payload 7 600000
            let system = asleepWriting 1 client bytes system

            let system =
                match UnixDescriptor.close client system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"write, time %d{seconds}: %A{other}"

            UnixSystem.checkInvariants system |> shouldEqual []

            let rec untilWoken (system : UnixSystem<int, string>) =
                if List.isEmpty (wokenAmong [ 1 ] system) then
                    untilWoken (readNow server 65536 system |> snd)
                else
                    system

            match finishedWrite 1 bytes (untilWoken system) with
            | Ok (WriteOutcome.WouldBlock (_, after))
            | Ok (WriteOutcome.Returns (_, after)) -> UnixSystem.checkInvariants after |> shouldEqual []
            | other -> failwith $"write, time %d{seconds}: %A{other}"

    /// The state the refusal keeps out: a connected socket under `SO_LINGER`
    /// that only calls in flight hold.
    [<Test>]
    let ``checkInvariants reports a socket under SO_LINGER that only calls hold`` () : unit =
        let client, _, system = pair (systemOn SimulatedUnixPlatform.linuxX64 false)
        let system = asleepReading 1 client 4096 system
        let socket = socketOf client system

        let description =
            FileDescriptorRegistry.tryFindId client (UnixSystemState.fileDescriptors system)
            |> Option.get

        let system =
            match UnixDescriptor.close client system with
            | Ok (_, system) -> system
            | other -> failwith $"%A{other}"

        UnixSystem.checkInvariants system |> shouldEqual []

        for seconds in [ 0L ; 100L ] do
            let lingering =
                let held = UnixMachineState.socket socket system.Machine

                { system with
                    Machine =
                        { system.Machine with
                            Sockets =
                                Map.add
                                    socket
                                    { held with
                                        Options =
                                            { held.Options with
                                                Linger =
                                                    {
                                                        Enabled = true
                                                        Hundredths = seconds
                                                    }
                                            }
                                    }
                                    system.Machine.Sockets
                        }
                }

            UnixSystem.checkInvariants lingering
            |> shouldEqual [ UnixSystemDefect.LingeringSocketHeldOnlyByCalls (description, socket) ]

    /// A woken call that finds nothing, through a description made
    /// non-blocking while it slept, is refused: whether it answers or sleeps
    /// on is unmeasured.
    [<Test>]
    let ``a woken transfer that finds nothing through a description now non-blocking is refused`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair (systemOn platform false)
            let system = asleepReading 1 client 1 |> fun f -> f system
            let system = asleepReading 2 client 1 system
            let system = wrote server 1 system |> snd

            let system =
                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered _, after) -> after
                | other -> failwith $"%A{other}"

            let _, system = UnixDescriptor.setNonBlocking client true system

            match UnixReadWrite.finishRead 2 system with
            | Error (ReadRefusal.ConnectionBecameNonBlocking _) -> ()
            | other -> failwith $"%O{platform}: %A{other}"

            // A writer woken into room it fills with more left.
            let client, server, system = pair (systemOn platform false)
            let bytes = payload 8 600000
            let system = asleepWriting 1 client bytes system

            let rec untilWoken (system : UnixSystem<int, string>) =
                if List.isEmpty (wokenAmong [ 1 ] system) then
                    untilWoken (readNow server 65536 system |> snd)
                else
                    system

            let system = untilWoken system
            let _, system = UnixDescriptor.setNonBlocking client true system

            match finishedWrite 1 bytes system with
            | Error (WriteRefusal.ConnectionBecameNonBlocking _) -> ()
            | other -> failwith $"%O{platform}: %A{other}"

    /// `write` given the bytes without `admitWrite` sleeps as the pair does.
    [<Test>]
    let ``a write without its admission sleeps as one with it`` () : unit =
        for platform in Machines.platforms do
            let client, _, system = pair (systemOn platform false)
            let bytes = ImmutableArray.CreateRange (payload 9 600000)

            let viaAdmission =
                match WriteOutcomes.admitThenWrite 1 client UserBuffer.Mapped bytes system with
                | Ok (WriteOutcome.WouldBlock (condition, after)) -> condition, after
                | other -> failwith $"%A{other}"

            let direct =
                match UnixReadWrite.write 1 client bytes system with
                | Ok (WriteOutcome.WouldBlock (condition, after)) -> condition, after
                | other -> failwith $"%A{other}"

            direct |> shouldEqual viaAdmission

    /// `SO_SNDTIMEO` would bound a sleeping write, as `SO_RCVTIMEO` would a
    /// read (whose refusal `TestBlockingAccept` holds); a park has no deadline
    /// because neither can be set. The numbers are each flavour's
    /// `<sys/socket.h>`.
    [<Test>]
    let ``SO_SNDTIMEO, which would bound a sleeping write, cannot be set`` () : unit =
        for platform, option in
            [
                SimulatedUnixPlatform.linuxX64, 21
                SimulatedUnixPlatform.macOsArm64, 0x1005
            ] do
            let client, _, system = pair (systemOn platform false)
            let level = SimulatedUnixPlatform.socketOptionLevel platform

            let socketId =
                match FileDescriptorRegistry.tryFindTarget client (UnixSystemState.fileDescriptors system) with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"%A{other}"

            UnixSocket.admitSetSockOpt client level option UserBuffer.Mapped 16u system
            |> shouldEqual (Error (SocketOptionRefusal.UnmodelledOption (socketId, level, option)))

    /// The invariants a park on a connection keeps.
    [<Test>]
    let ``checkInvariants reports a connection park no call could make`` () : unit =
        let client, _, system = pair (systemOn SimulatedUnixPlatform.linuxX64 false)

        let description =
            FileDescriptorRegistry.tryFindId client (UnixSystemState.fileDescriptors system)
            |> Option.get

        let readBy (call : TcpReceiveCall) (count : int) (target : SleepTarget<SocketId>) =
            ParkedSyscall.ConnectionRead
                {
                    Socket = target
                    Buffer = UserBuffer.Mapped
                    Count = count
                    Call = call
                }

        let read = readBy TcpReceiveCall.Read

        UnixSystem.checkInvariants (ForgedPark.onAbsent 1 (read 0 (SleepTarget.Waiting (description, client))) system)
        |> shouldEqual [ UnixSystemDefect.ParkedConnectionTransferProgress (1, 0, 0) ]

        // A Linux recv of nothing sleeps until there is something to answer
        // (`tcp-recv-send.c` sections Z and P-zero), so it may be asleep
        // asking for nothing; a negative count is never a call's.
        for call in [ TcpReceiveCall.Receive ; TcpReceiveCall.Peek ] do
            UnixSystem.checkInvariants (
                ForgedPark.onAbsent 1 (readBy call 0 (SleepTarget.Waiting (description, client))) system
            )
            |> shouldEqual []

            UnixSystem.checkInvariants (
                ForgedPark.onAbsent 1 (readBy call -1 (SleepTarget.Waiting (description, client))) system
            )
            |> shouldEqual [ UnixSystemDefect.ParkedConnectionTransferProgress (1, -1, 0) ]

        let write =
            ParkedSyscall.ConnectionWrite
                {
                    Socket = SleepTarget.Waiting (description, client)
                    Buffer = UserBuffer.Mapped
                    Count = 10
                    Written = 10
                    Call = TcpSendCall.Write
                }

        UnixSystem.checkInvariants (ForgedPark.onAbsent 1 write system)
        |> shouldEqual [ UnixSystemDefect.ParkedConnectionTransferProgress (1, 10, 10) ]

        // A park on stdout, a pipe, which is no connection.
        let stdout =
            FileDescriptorRegistry.tryFindId 1 (UnixSystemState.fileDescriptors system)
            |> Option.get

        UnixSystem.checkInvariants (ForgedPark.onAbsent 1 (read 5 (SleepTarget.Waiting (stdout, 1))) system)
        |> shouldEqual [ UnixSystemDefect.ParkedConnectionTransferOnNonConnection (1, stdout) ]

        let socketId =
            match FileDescriptorRegistry.tryFindTarget client (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"%A{other}"

        UnixSystem.checkInvariants (ForgedPark.onAbsent 1 (read 5 (SleepTarget.EndedByClose socketId)) system)
        |> shouldEqual [ UnixSystemDefect.ParkedCallEndedByCloseUnderLinux 1 ]

        // Under Darwin a close of the descriptor a call was made through ends
        // it, so while it sleeps the descriptor names its description.
        let client, server, system = pair (systemOn SimulatedUnixPlatform.macOsArm64 false)

        let description =
            FileDescriptorRegistry.tryFindId client (UnixSystemState.fileDescriptors system)
            |> Option.get

        let serverDescription =
            FileDescriptorRegistry.tryFindId server (UnixSystemState.fileDescriptors system)
            |> Option.get

        UnixSystem.checkInvariants (ForgedPark.onAbsent 1 (read 5 (SleepTarget.Waiting (description, server))) system)
        |> shouldEqual
            [
                UnixSystemDefect.ParkedCallDescriptorRebound (1, server, description, Some serverDescription)
            ]

        // Darwin's recv of nothing returns at once, so none sleeps.
        UnixSystem.checkInvariants (
            ForgedPark.onAbsent 1 (readBy TcpReceiveCall.Receive 0 (SleepTarget.Waiting (description, client))) system
        )
        |> shouldEqual [ UnixSystemDefect.ParkedConnectionTransferProgress (1, 0, 0) ]

    /// A read asleep in one process wakes for bytes another process writes,
    /// and a write asleep in one for room another's read makes, through
    /// `SimulatedMachine.wakes`; each finishes in its own process's view.
    [<Test>]
    let ``transfers asleep in one process wake for another's`` () : unit =
        for platform in Machines.platforms do
            let pids, machine = Machines.withTasksOn small platform 2 5
            let a, b = pids.[0], pids.[1]
            let listener, machine = Machines.inProcess b (KeventWorld.listenerAt port) machine

            let client, machine =
                Machines.inProcess
                    a
                    (fun view ->
                        let fd, view = KeventWorld.stream false view

                        match KeventWorld.connect fd port view with
                        | ConnectOutcome.Completed, view -> fd, view
                        | other, _ -> failwith $"connect: %A{other}"
                    )
                    machine

            let accepted, machine =
                Machines.inProcess b (fun view -> KeventWorld.accept listener view) machine

            let machine = Machines.doIn a (asleepReading 1 client 4096) machine
            let asleep = Map.ofList [ a, Set.singleton 1 ]
            SimulatedMachine.wakes asleep machine |> shouldEqual []

            let machine = Machines.doIn b (fun view -> wrote accepted 100 view |> snd) machine
            Machines.assertClean machine
            SimulatedMachine.wakes asleep machine |> List.map fst |> shouldEqual [ a, 1 ]

            let answer, machine =
                Machines.inProcess
                    a
                    (fun view ->
                        match UnixReadWrite.finishRead 1 view with
                        | Ok (ReadOutcome.Answered answer, view) -> answer, view
                        | other -> failwith $"%A{other}"
                    )
                    machine

            answer
            |> shouldEqual (ReadAnswer.Completed (ImmutableArray.CreateRange (payload 99 100)))

            // Now a writer in `a`, which `b`'s reads wake.
            let bytes = payload 10 600000
            let machine = Machines.doIn a (asleepWriting 2 client bytes) machine
            let asleep = Map.ofList [ a, Set.singleton 2 ]
            SimulatedMachine.wakes asleep machine |> shouldEqual []

            let rec untilWoken (reads : int) (machine : SimulatedMachine<int, string>) =
                if reads > 1000 then
                    failwith "the writer never woke"

                match SimulatedMachine.wakes asleep machine with
                | [] -> untilWoken (reads + 1) (Machines.doIn b (fun view -> readNow accepted 4096 view |> snd) machine)
                | woken -> woken |> List.map fst, machine

            let woken, machine = untilWoken 0 machine
            woken |> shouldEqual [ a, 2 ]
            Machines.assertClean machine

            let machine =
                Machines.doIn
                    a
                    (fun view ->
                        match finishedWrite 2 bytes view with
                        | Ok (WriteOutcome.WouldBlock (_, view)) -> view
                        | other -> failwith $"%A{other}"
                    )
                    machine

            Machines.assertClean machine

    /// A Linux write that sleeps marks its socket out of space, as
    /// `sk_stream_wait_memory` sets `SOCK_NOSPACE`, so an edge-triggered
    /// `EPOLLOUT` registration gets its one edge when the send buffer drains
    /// to two thirds full, as after a write that met `EAGAIN`.
    [<Test>]
    let ``a Linux write that sleeps arms the send-space edge`` () : unit =
        let client, server, system = pair (systemOn SimulatedUnixPlatform.linuxX64 false)

        let epoll, system =
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok created) -> created
            | other -> failwith $"epoll_create1: %A{other}"

        let system =
            match
                UnixPoll.epollCtl
                    epoll
                    1
                    client
                    (EpollEventArgument.Readable (EpollEvents.Out ||| EpollEvents.EdgeTriggered, 0UL))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> system
            | other -> failwith $"epoll_ctl: %A{other}"

        let edges (system : UnixSystem<int, string>) : uint32 list * UnixSystem<int, string> =
            match UnixPoll.epollWait 3 epoll 2 UserBuffer.Mapped 0 system with
            | Ok (EpollWaitOutcome.Answered events, system) -> List.map snd events, system
            | other -> failwith $"epoll_wait: %A{other}"

        // The ADD's own edge, while the socket is writable.
        let added, system = edges system
        added |> shouldEqual [ EpollEvents.Out ]

        let system = asleepWriting 1 client (payload 11 600000) system

        let rec drain (reads : int) (system : UnixSystem<int, string>) =
            if reads > 100 then
                failwith "no edge came"

            let system = readNow server 1000 system |> snd

            match edges system with
            | [], system -> drain (reads + 1) system
            | events, _ -> events, wokenAmong [ 1 ] system

        let events, woken = drain 0 system
        events |> shouldEqual [ EpollEvents.Out ]
        // The edge comes as the sleeping writer is woken.
        woken |> shouldEqual [ 1 ]

    /// A Linux write that takes nothing and sleeps arms the edge too, with
    /// nothing before it having armed it: the buffers were filled by a write
    /// they took whole.
    [<Test>]
    let ``a Linux write that sleeps having taken nothing arms the send-space edge`` () : unit =
        let client, server, system = pair (systemOn SimulatedUnixPlatform.linuxX64 false)
        let connection = connectionOf client system

        let toServer =
            (UnixMachineState.connection connection system.Machine).Transfer.ToServer

        let fits = toServer.SendCapacity + toServer.ReceiveCapacity
        let written, system = wrote client fits system
        written |> shouldEqual (int64 fits)

        let epoll, system =
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok created) -> created
            | other -> failwith $"epoll_create1: %A{other}"

        // Registered while the socket is not writable, so with no edge of
        // its own.
        let system =
            match
                UnixPoll.epollCtl
                    epoll
                    1
                    client
                    (EpollEventArgument.Readable (EpollEvents.Out ||| EpollEvents.EdgeTriggered, 0UL))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> system
            | other -> failwith $"epoll_ctl: %A{other}"

        let edges (system : UnixSystem<int, string>) : uint32 list * UnixSystem<int, string> =
            match UnixPoll.epollWait 3 epoll 2 UserBuffer.Mapped 0 system with
            | Ok (EpollWaitOutcome.Answered events, system) -> List.map snd events, system
            | other -> failwith $"epoll_wait: %A{other}"

        let none, system = edges system
        none |> shouldEqual []

        // The ADD found the socket unwritable, which marks it too; drain to
        // the edge that mark gives, so that only the sleeping write can mark
        // it again.
        let rec drainToEdge (reads : int) (system : UnixSystem<int, string>) =
            if reads > 100 then
                failwith "no edge came for the ADD's mark"

            let system = readNow server 1000 system |> snd

            match edges system with
            | [], system -> drainToEdge (reads + 1) system
            | _, system -> system

        let system = drainToEdge 0 system

        let written, system =
            wrote
                client
                (toServer.SendCapacity + toServer.ReceiveCapacity
                 - clientQueued connection system
                 - TcpTransfer.readable
                     ConnectionEnd.Server
                     (UnixMachineState.connection connection system.Machine).Transfer)
                system

        written |> shouldBeGreaterThan 0L
        let system = asleepWriting 1 client (payload 12 1000) system

        match UnixTaskTable.parkedFor 1 system.Tasks with
        | Some (ParkedSyscall.ConnectionWrite write) -> write.Written |> shouldEqual 0
        | other -> failwith $"%A{other}"

        let rec drain (reads : int) (system : UnixSystem<int, string>) =
            if reads > 100 then
                failwith "no edge came for the sleeping write's mark"

            let system = readNow server 1000 system |> snd

            match edges system with
            | [], system -> drain (reads + 1) system
            | events, _ -> events

        drain 0 system |> shouldEqual [ EpollEvents.Out ]

    /// Darwin's sleeping writer with less than the low-water mark left wakes
    /// once there is room for all of what is left, though not for the mark.
    [<Test>]
    let ``Darwin: a writer with a short remainder wakes for room for all of it`` () : unit =
        let client, server, system = pair (systemOn SimulatedUnixPlatform.macOsArm64 false)
        let connection = connectionOf client system

        let toServer =
            (UnixMachineState.connection connection system.Machine).Transfer.ToServer

        let room = toServer.SendCapacity + toServer.ReceiveCapacity
        let system = asleepWriting 1 client (payload 13 (room + 100)) system

        match UnixTaskTable.parkedFor 1 system.Tasks with
        | Some (ParkedSyscall.ConnectionWrite write) -> write.Count - write.Written |> shouldEqual 100
        | other -> failwith $"%A{other}"

        let system = readNow server 50 system |> snd
        wokenAmong [ 1 ] system |> shouldEqual []
        let system = readNow server 50 system |> snd
        wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

        match finishedWrite 1 (payload 13 (room + 100)) system with
        | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, _)) -> n |> shouldEqual (int64 (room + 100))
        | other -> failwith $"%A{other}"

    /// A Linux send buffer of one byte, holding it, is two thirds full by the
    /// kernel's integer arithmetic and still full: the sleeping writer is not
    /// woken until a read makes room, or it would wake for ever and make no
    /// progress.
    [<Test>]
    let ``a Linux writer is not woken by a full buffer that is two thirds full`` () : unit =
        let tiny (image : UnixBootImage<int, string>) : UnixBootImage<int, string> =
            image
            |> UnixBootImage.withTcpSendSpaceMax (Some 1)
            |> Configured.expectOk TcpSendSpaceMaxRefusal.describe
            |> UnixBootImage.withTcpReceiveSpace (Some 1)
            |> Configured.expectOk TcpReceiveSpaceRefusal.describe

        let system =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64
            |> tiny
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> fun system -> (tasks, system) ||> List.foldBack Tasks.ensure

        let client, server, system = pair system
        let connection = connectionOf client system

        let toServer =
            (UnixMachineState.connection connection system.Machine).Transfer.ToServer

        let room = toServer.SendCapacity + toServer.ReceiveCapacity
        let system = asleepWriting 1 client (payload 14 (room + 1)) system
        clientQueued connection system |> shouldEqual toServer.SendCapacity
        wokenAmong [ 1 ] system |> shouldEqual []
        let system = readNow server 1 system |> snd
        wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

    // ------------------------------------------------------------------
    // The rows of `tcp-recv-send.c`, one at a time
    // ------------------------------------------------------------------

    /// A `recv` by `task` through `fd` of up to `count` bytes with `flags`.
    let private received
        (task : int)
        (fd : int)
        (count : int)
        (flags : MessageFlag list)
        (system : UnixSystem<int, string>)
        : ReadOutcome * UnixSystem<int, string>
        =
        match
            UnixReadWrite.recv
                task
                fd
                UserBuffer.Mapped
                (uint64 count)
                (flagWord system.Machine.UnixPlatform flags)
                system
        with
        | Ok outcome -> outcome
        | Error refusal -> failwith $"recv refused: %s{ReceiveRefusal.describe refusal}"

    /// A whole `send` by `task` through `fd` of `bytes` with `flags`.
    let private sent
        (task : int)
        (fd : int)
        (bytes : byte list)
        (flags : MessageFlag list)
        (system : UnixSystem<int, string>)
        : WriteOutcome<WriteAnswer, int, string>
        =
        match
            WriteOutcomes.admitThenSend
                task
                fd
                UserBuffer.Mapped
                (ImmutableArray.CreateRange bytes)
                (flagWord system.Machine.UnixPlatform flags)
                system
        with
        | Ok outcome -> outcome
        | Error refusal -> failwith $"send refused: %s{SendRefusal.describe refusal}"

    let private available (fd : int) (system : UnixSystem<int, string>) : int =
        match UnixDescriptor.bytesAvailable fd UserBuffer.Mapped system with
        | Ok (BytesAvailableAnswer.Reported count) -> count
        | other -> failwith $"FIONREAD of fd %d{fd}: %A{other}"

    /// The server end full, written to non-blocking until `EAGAIN`, and the
    /// description blocking again.
    let private filled (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let _, system = UnixDescriptor.setNonBlocking fd true system

        let rec fill (system : UnixSystem<int, string>) =
            match
                WriteOutcomes.admitThenWrite
                    0
                    fd
                    UserBuffer.Mapped
                    (ImmutableArray.CreateRange (payload 1 65536))
                    system
            with
            | Ok (WriteOutcome.Returns (WriteAnswer.Completed _, system)) -> fill system
            | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system)) -> system
            | other -> failwith $"filling fd %d{fd}: %A{other}"

        fill system |> UnixDescriptor.setNonBlocking fd false |> snd

    /// P-sleep: a blocking peek with nothing queued sleeps until bytes arrive,
    /// answers them, and leaves them queued.
    [<Test>]
    let ``a peek sleeps until bytes arrive, and answers them without taking them`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair (systemOn platform false)

            let system =
                match received 1 client 100 [ MessageFlag.Peek ] system with
                | ReadOutcome.WouldBlock _, system -> system
                | other -> failwith $"%O{platform}: the peek came to %A{other}"

            match UnixTaskTable.parkedFor 1 system.Tasks with
            | Some (ParkedSyscall.ConnectionRead read) -> read.Call |> shouldEqual TcpReceiveCall.Peek
            | other -> failwith $"%O{platform}: parked as %A{other}"

            wokenAmong [ 1 ] system |> shouldEqual []
            let system = wrote server 10 system |> snd
            wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

            let expected = ImmutableArray.CreateRange (payload 99 10)

            let system =
                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                    bytes |> shouldEqual expected
                    after
                | other -> failwith $"%O{platform}: %A{other}"

            available client system |> shouldEqual 10

            match received 0 client 100 [] system with
            | ReadOutcome.Answered (ReadAnswer.Completed bytes), system ->
                bytes |> shouldEqual expected
                available client system |> shouldEqual 0
            | other -> failwith $"%O{platform}: the recv came to %A{other}"

    /// P-zero and Z: a recv of nothing, peeking or not, is `EAGAIN` on an idle
    /// non-blocking Linux socket and sleeps on a blocking one until bytes
    /// arrive, then answers 0 and takes none of them; on Darwin it answers 0
    /// at once.
    [<Test>]
    let ``a recv of nothing sleeps on Linux until there is something to answer, and answers 0 at once on Darwin``
        ()
        : unit
        =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            for flags in [ [] ; [ MessageFlag.Peek ] ] do
                let client, server, system = pair (systemOn platform false)
                let _, system = UnixDescriptor.setNonBlocking client true system

                match received 1 client 0 flags system with
                | ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN), _ when linux -> ()
                | ReadOutcome.Answered (ReadAnswer.Completed bytes), _ when not linux ->
                    bytes.IsEmpty |> shouldEqual true
                | other -> failwith $"%O{platform} %A{flags}: non-blocking, %A{other}"

                let _, system = UnixDescriptor.setNonBlocking client false system

                match received 1 client 0 flags system with
                | ReadOutcome.Answered (ReadAnswer.Completed bytes), _ when not linux ->
                    bytes.IsEmpty |> shouldEqual true
                | ReadOutcome.WouldBlock _, system when linux ->
                    UnixSystem.checkInvariants system |> shouldEqual []
                    let system = wrote server 10 system |> snd
                    wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                    match UnixReadWrite.finishRead 1 system with
                    | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                        bytes.IsEmpty |> shouldEqual true
                        available client after |> shouldEqual 10
                    | other -> failwith $"%O{platform} %A{flags}: finished as %A{other}"
                | other -> failwith $"%O{platform} %A{flags}: blocking, %A{other}"

    /// P-eof: a peek answers what is queued ahead of a FIN, and then 0; and a
    /// peek asleep when the FIN arrives answers 0.
    [<Test>]
    let ``a peek at a FIN answers the bytes ahead of it, then 0`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair (systemOn platform false)
            let system = wrote server 1000 system |> snd |> KeventWorld.close server
            let all = ImmutableArray.CreateRange (payload 99 1000)

            let answers =
                ([ [ MessageFlag.Peek ] ; [] ; [ MessageFlag.Peek ] ], (system, []))
                ||> List.foldBack (fun flags (system, answers) ->
                    match received 0 client 4096 flags system with
                    | ReadOutcome.Answered answer, system -> system, answer :: answers
                    | other -> failwith $"%O{platform}: %A{other}"
                )
                |> snd
                |> List.rev

            answers
            |> shouldEqual
                [
                    ReadAnswer.Completed all
                    ReadAnswer.Completed all
                    ReadAnswer.Completed ImmutableArray.Empty
                ]

            let client, server, system = pair (systemOn platform false)

            let system =
                match received 1 client 100 [ MessageFlag.Peek ] system with
                | ReadOutcome.WouldBlock _, system -> system
                | other -> failwith $"%O{platform}: %A{other}"

            let system = KeventWorld.close server system
            wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

            match UnixReadWrite.finishRead 1 system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                bytes.IsEmpty |> shouldEqual true

                received 0 client 100 [ MessageFlag.Peek ] after
                |> fst
                |> shouldEqual (ReadOutcome.Answered (ReadAnswer.Completed ImmutableArray.Empty))
            | other -> failwith $"%O{platform}: %A{other}"

    /// P-reset: a peek asleep when its connection is reset answers
    /// `ECONNRESET`, which Linux's takes and Darwin's leaves pending.
    [<Test>]
    let ``a peek asleep at a reset answers ECONNRESET, which only Linux's takes`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
            let client, server, system = pair (systemOn platform false)
            let connection = connectionOf client system
            let system = wrote client 100 system |> snd

            let system =
                match received 1 client 100 [ MessageFlag.Peek ] system with
                | ReadOutcome.WouldBlock _, system -> system
                | other -> failwith $"%O{platform}: %A{other}"

            let system = KeventWorld.close server system
            wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

            match UnixReadWrite.finishRead 1 system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.ECONNRESET), after) ->
                pendingError connection ConnectionEnd.Client after
                |> shouldEqual (if linux then None else Some TcpError.ConnectionReset)
            | other -> failwith $"%O{platform}: %A{other}"

    /// P-eintr: a signal ends a sleeping peek with `EINTR`, or restarts it.
    [<Test>]
    let ``a signal ends a sleeping peek with EINTR, or restarts it`` () : unit =
        for platform in Machines.platforms do
            for restart in [ false ; true ] do
                let client, _, system = pair (systemOn platform restart)

                let system =
                    match received 1 client 100 [ MessageFlag.Peek ] system with
                    | ReadOutcome.WouldBlock _, system -> signalled 1 system
                    | other -> failwith $"%O{platform}: %A{other}"

                wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                match UnixReadWrite.finishRead 1 system with
                | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EINTR), _) when not restart -> ()
                | Ok (ReadOutcome.Restarts, _) when restart -> ()
                | other -> failwith $"%O{platform} restart %b{restart}: %A{other}"

    /// D: `MSG_DONTWAIT` on a blocking socket. A recv with nothing queued
    /// answers `EAGAIN` on both, and a send with no room answers `EAGAIN` on
    /// Linux, having armed the send-space edge as `O_NONBLOCK` does, where
    /// Darwin's sleeps until its bytes are taken. Neither sets `O_NONBLOCK`.
    [<Test>]
    let ``MSG_DONTWAIT makes a recv and a Linux send non-blocking, and Darwin's send ignores it`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
            let client, server, system = pair (systemOn platform false)
            let connection = connectionOf client system

            received 0 client 100 [ MessageFlag.DontWait ] system
            |> fst
            |> shouldEqual (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN))

            let system = filled client system
            let bytes = payload 7 65536

            let outcome = sent 1 client bytes [ MessageFlag.DontWait ] system

            let system =
                match outcome with
                | WriteOutcome.Returns (WriteAnswer.Failed UnixError.EAGAIN, system) when linux ->
                    match (UnixMachineState.connection connection system.Machine).Transfer.Rules with
                    | TcpTransferRules.Linux armed -> Set.contains ConnectionEnd.Client armed |> shouldEqual true
                    | TcpTransferRules.Darwin -> failwith "a Linux connection with Darwin's rules"

                    system
                | WriteOutcome.WouldBlock (_, system) when not linux ->
                    let system = drained server system
                    wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                    let rec finished (system : UnixSystem<int, string>) =
                        match libraryFinishWrite 1 bytes system with
                        | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, system)) ->
                            n |> shouldEqual 65536L
                            system
                        | Ok (WriteOutcome.WouldBlock (_, system)) -> drained server system |> finished
                        | other -> failwith $"%O{platform}: %A{other}"

                    finished system
                | other -> failwith $"%O{platform}: the send came to %A{other}"

            match FileDescriptorRegistry.tryFindWithId client (UnixSystemState.fileDescriptors system) with
            | Some (_, description) -> description.NonBlocking |> shouldEqual false
            | None -> failwith "the client's descriptor has gone"

    /// N-recv: `MSG_NOSIGNAL` changes nothing about a recv.
    [<Test>]
    let ``MSG_NOSIGNAL changes nothing about a recv`` () : unit =
        for platform in Machines.platforms do
            let client, server, system = pair (systemOn platform false)

            received 0 client 100 [ MessageFlag.NoSignal ; MessageFlag.DontWait ] system
            |> fst
            |> shouldEqual (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN))

            let system = wrote server 10 system |> snd

            received 0 client 100 [ MessageFlag.NoSignal ; MessageFlag.DontWait ] system
            |> fst
            |> shouldEqual (ReadOutcome.Answered (ReadAnswer.Completed (ImmutableArray.CreateRange (payload 99 10))))

    /// N-partial and N-empty: a send asleep when its connection is reset.
    /// Linux answers the count taken, or with nothing taken `ECONNRESET`, and
    /// raises nothing either way; Darwin answers `EPIPE` and raises `SIGPIPE`,
    /// unless the send had `MSG_NOSIGNAL`.
    [<Test>]
    let ``a send asleep at a reset raises SIGPIPE only where a write would, and never under MSG_NOSIGNAL`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            for noSignal in [ false ; true ] do
                for tookSome in [ false ; true ] do
                    let where = $"%O{platform} MSG_NOSIGNAL %b{noSignal}, took some %b{tookSome}"
                    let client, server, system = pair (systemOn platform false)
                    let connection = connectionOf client system
                    let system = if tookSome then system else filled client system

                    let toServer =
                        (UnixMachineState.connection connection system.Machine).Transfer.ToServer

                    let bytes =
                        payload
                            3
                            (if tookSome then
                                 toServer.SendCapacity + toServer.ReceiveCapacity + 5000
                             else
                                 1000)

                    let flags = if noSignal then [ MessageFlag.NoSignal ] else []

                    let system =
                        match sent 1 client bytes flags system with
                        | WriteOutcome.WouldBlock (_, system) -> system
                        | other -> failwith $"%s{where}: %A{other}"

                    let written =
                        match UnixTaskTable.parkedFor 1 system.Tasks with
                        | Some (ParkedSyscall.ConnectionWrite write) ->
                            write.Call |> shouldEqual (TcpSendCall.Send noSignal)
                            write.Written
                        | other -> failwith $"%s{where}: parked as %A{other}"

                    (written > 0) |> shouldEqual tookSome

                    // The server closes with the client's bytes unread: a reset.
                    let system = KeventWorld.close server system
                    wokenAmong [ 1 ] system |> shouldEqual [ 1 ]

                    match libraryFinishWrite 1 bytes system, linux with
                    | Ok (WriteOutcome.Returns (WriteAnswer.Completed n, _)), true when tookSome ->
                        n |> shouldEqual (int64 written)
                    | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.ECONNRESET, _)), true when not tookSome ->
                        ()
                    | Ok (WriteOutcome.Returns (WriteAnswer.Failed UnixError.EPIPE, _)), false when noSignal -> ()
                    | Ok (WriteOutcome.ReturnsRaising (WriteAnswer.Failed UnixError.EPIPE, raised, _)), false when
                        not noSignal
                        ->
                        raised.Signal |> shouldEqual Signal.SIGPIPE
                    | other, _ -> failwith $"%s{where}: %A{other}"

    /// W: Darwin's `write` that moves bytes marks its description written
    /// (`FWASWRITTEN`, which `F_GETFL` shows); its `send` does not, nor does a
    /// send of nothing. Linux marks nothing.
    [<Test>]
    let ``only a Darwin write marks its description written`` () : unit =
        for platform in Machines.platforms do
            let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

            let marked (fd : int) (system : UnixSystem<int, string>) : bool =
                match FileDescriptorRegistry.tryFindWithId fd (UnixSystemState.fileDescriptors system) with
                | Some (_, description) -> description.Status.Written
                | None -> failwith $"fd %d{fd} names no description"

            for call, count in [ "write", 10 ; "send", 10 ; "send", 0 ] do
                let client, _, system = pair (systemOn platform false)

                let system =
                    match call with
                    | "write" -> wrote client count system |> snd
                    | _ ->
                        match sent 0 client (payload 5 count) [] system with
                        | WriteOutcome.Returns (WriteAnswer.Completed n, system) when n = int64 count -> system
                        | other -> failwith $"%O{platform}: %A{other}"

                marked client system |> shouldEqual (call = "write" && not linux)

    /// A Darwin close that ends a sleeping send having taken some marks
    /// nothing, where one that ends such a write marks the description
    /// (`fcntl-dup.c`, WRITTEN rows).
    [<Test>]
    let ``Darwin: a close that ends a send having taken some marks nothing`` () : unit =
        for call in [ TcpSendCall.Write ; TcpSendCall.Send false ] do
            let client, _, system = pair (systemOn SimulatedUnixPlatform.macOsArm64 false)
            let connection = connectionOf client system
            let copy, system = KeventWorld.dup client system

            let toServer =
                (UnixMachineState.connection connection system.Machine).Transfer.ToServer

            let bytes = payload 4 (toServer.SendCapacity + toServer.ReceiveCapacity + 5000)

            let system =
                match call with
                | TcpSendCall.Write -> asleepWriting 1 client bytes system
                | TcpSendCall.Send _ ->
                    match sent 1 client bytes [] system with
                    | WriteOutcome.WouldBlock (_, system) -> system
                    | other -> failwith $"%A{other}"

            let system = KeventWorld.close client system

            match FileDescriptorRegistry.tryFindWithId copy (UnixSystemState.fileDescriptors system) with
            | Some (_, description) -> description.Status.Written |> shouldEqual (call = TcpSendCall.Write)
            | None -> failwith "the copy names no description"

    /// Every flag but those modelled is refused, naming it: `MSG_PEEK`,
    /// `MSG_DONTWAIT` and `MSG_NOSIGNAL` for recv, `MSG_DONTWAIT` and
    /// `MSG_NOSIGNAL` for send, and a bit the flavour names nothing is
    /// refused as unnamed.
    [<Test>]
    let ``recv and send refuse every flag they do not model, naming it`` () : unit =
        for platform in Machines.platforms do
            let flavour = SimulatedUnixPlatform.flavour platform
            let client, _, system = pair (systemOn platform false)

            let defined =
                MessageFlag.named
                |> List.filter (fun flag -> (MessageFlag.number flavour flag).IsSome)

            // A bit neither flavour names.
            let unnamed = MessageFlag.Unnamed 0x20000

            MessageFlag.decode flavour 0x20000 |> shouldEqual [ unnamed ]

            for flag in unnamed :: defined do
                let word = MessageFlag.encode flavour [ flag ] |> Option.get

                let receiveModelled =
                    List.contains flag [ MessageFlag.Peek ; MessageFlag.DontWait ; MessageFlag.NoSignal ]

                let sendModelled =
                    List.contains flag [ MessageFlag.DontWait ; MessageFlag.NoSignal ]

                match UnixReadWrite.recv 0 client UserBuffer.Mapped 10UL word system with
                | Error (ReceiveRefusal.UnmodelledFlags flags) when not receiveModelled -> flags |> shouldEqual [ flag ]
                | Ok _ when receiveModelled -> ()
                | other -> failwith $"%O{platform} recv with %A{flag}: %A{other}"

                match UnixReadWrite.admitSend 0 client UserBuffer.Mapped 10UL word system with
                | Error (SendRefusal.UnmodelledFlags flags) when not sendModelled -> flags |> shouldEqual [ flag ]
                | Ok _ when sendModelled -> ()
                | other -> failwith $"%O{platform} send with %A{flag}: %A{other}"

            // Every unmodelled flag of a word, from the lowest bit.
            let word =
                MessageFlag.encode flavour [ MessageFlag.Peek ; MessageFlag.WaitAll ; MessageFlag.OutOfBand ]
                |> Option.get

            match UnixReadWrite.recv 0 client UserBuffer.Mapped 10UL word system with
            | Error (ReceiveRefusal.UnmodelledFlags flags) ->
                flags |> shouldEqual [ MessageFlag.OutOfBand ; MessageFlag.WaitAll ]
            | other -> failwith $"%O{platform}: %A{other}"

    /// A count above `INT_MAX`, and a socket that is not an end of a
    /// connection, are refused rather than answered: neither is measured.
    [<Test>]
    let ``recv and send refuse a count above INT_MAX and a socket that is not connected`` () : unit =
        for platform in Machines.platforms do
            let client, _, system = pair (systemOn platform false)
            let huge = uint64 System.Int32.MaxValue + 1UL

            match UnixReadWrite.recv 0 client UserBuffer.Mapped huge 0 system with
            | Error (ReceiveRefusal.UnmeasuredCount count) -> count |> shouldEqual huge
            | other -> failwith $"%O{platform}: %A{other}"

            match UnixReadWrite.admitSend 0 client UserBuffer.Mapped huge 0 system with
            | Error (SendRefusal.UnmeasuredCount count) -> count |> shouldEqual huge
            | other -> failwith $"%O{platform}: %A{other}"

            let idle, system = KeventWorld.stream false system
            let listener, system = KeventWorld.listenerAt (port + 1us) system

            for fd in [ idle ; listener ] do
                match UnixReadWrite.recv 0 fd UserBuffer.Mapped 10UL 0 system with
                | Error (ReceiveRefusal.UnmodelledSocketPhase _) -> ()
                | other -> failwith $"%O{platform} fd %d{fd}: %A{other}"

                match UnixReadWrite.admitSend 0 fd UserBuffer.Mapped 10UL 0 system with
                | Error (SendRefusal.UnmodelledSocketPhase _) -> ()
                | other -> failwith $"%O{platform} fd %d{fd}: %A{other}"

                match UnixReadWrite.send 0 fd (ImmutableArray.CreateRange (payload 6 10)) 0 system with
                | Error (SendRefusal.UnmodelledSocketPhase _) -> ()
                | other -> failwith $"%O{platform} fd %d{fd}: %A{other}"
