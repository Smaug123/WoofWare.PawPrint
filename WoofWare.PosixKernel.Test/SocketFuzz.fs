namespace WoofWare.PosixKernel.Test

open System
open System.Text
open WoofWare.PosixKernel

/// One operation of the socket/epoll differential fuzzer's op language
/// (docs/plans/2026-08-22-socket-epoll-fuzzer.md). Slots name descriptors on
/// both sides of the comparison; each side keeps its own slot-to-fd map, and
/// raw fd numbers never appear in a transcript. `Add` and `Mod` carry the
/// .NET shim's `SocketEvents` bits (0..0x1F), which both sides translate 1:1 to
/// epoll bits and register edge-triggered, as the shim does; `EpollAdd` and
/// `EpollMod` carry raw `<sys/epoll.h>` events, which reach both kernels
/// unconverted.
[<RequireQualifiedAccess>]
type FuzzOp =
    | NewSocket of slot : int
    | Listen of slot : int
    | Connect of client : int * listener : int
    | ConnectDead of client : int
    | Accept of listener : int * newSlot : int
    | Close of slot : int
    | Dup of slot : int * newSlot : int
    | NewPort of slot : int
    | Add of port : int * target : int * mask : int
    | Mod of port : int * target : int * mask : int
    | Del of port : int * target : int
    /// `EPOLL_CTL_ADD` with raw `<sys/epoll.h>` events, any 32-bit value.
    | EpollAdd of port : int * target : int * events : uint32
    /// `EPOLL_CTL_MOD` with raw `<sys/epoll.h>` events, any 32-bit value.
    | EpollMod of port : int * target : int * events : uint32
    | Wait of port : int * maxEvents : int
    /// `poll(2)` over a single slot, with timeout 0. The `events` mask is
    /// Linux's own `<poll.h>` numbering, any value in 0..0xFFFF, and it reaches
    /// both kernels unconverted — a *different* alphabet from the
    /// `SocketEvents` bits `Add`/`Mod` carry, which number different
    /// conditions with the same small integers.
    | Poll of slot : int * events : int

/// How the emulated side answered one whole sequence.
[<RequireQualifiedAccess>]
type EmulatedRun =
    /// Every op answered; the transcript is comparable with the harness's.
    | Transcript of string
    /// The kernel refused an op with one of its typed refusals — the sequence
    /// is outside the modelled envelope, and the comparison skips it.
    | Refused of opIndex : int * message : string
    /// An exception out of the kernel or this driver, or a `checkInvariants`
    /// defect: reaching either through the public surface is a finding, never
    /// a skip.
    | Defect of opIndex : int * message : string

[<RequireQualifiedAccess>]
module SocketFuzz =

    /// The op language's interest mask, as the events the .NET shim passes
    /// `epoll_ctl`.
    ///
    /// The mirror of `harness.c`'s `interest_to_epoll`, which maps the same five
    /// bits onto `EPOLLIN|EPOLLOUT|EPOLLRDHUP|EPOLLHUP|EPOLLERR` and adds
    /// `EPOLLET`. Only three of them can be *asked* for -- the last two are what
    /// epoll reports unasked -- which is exactly the collapse the fuzzer wants
    /// to exercise.
    ///
    /// Screens the mask rather than ignoring stray bits: a mask outside 0..0x1F
    /// is a generator bug, and a fuzzer that quietly accepted one would compare
    /// two sides that had been asked different questions.
    ///
    /// Carries the `INTERPRETER-DRIVER BUG` marker every other generator-bug
    /// failure in this file carries, because that is what `executeEmulated`
    /// classifies on: without it the sequence would come back as
    /// `EmulatedRun.Refused`, which the live fuzzer *skips*, and a generator
    /// regression would be counted rather than reported.
    let private eventsOfMask (mask : int) : uint32 =
        if mask &&& ~~~0x1F <> 0 then
            failwith
                $"INTERPRETER-DRIVER BUG: interest mask 0x%x{mask} has bits outside the five the op language defines (0x1F); the generator should never have produced it."

        EpollEvents.EdgeTriggered
        ||| (if mask &&& 0x01 <> 0 then EpollEvents.In else 0u)
        ||| (if mask &&& 0x02 <> 0 then EpollEvents.Out else 0u)
        ||| (if mask &&& 0x04 <> 0 then EpollEvents.RdHup else 0u)
        ||| (if mask &&& 0x08 <> 0 then EpollEvents.Hup else 0u)
        ||| (if mask &&& 0x10 <> 0 then EpollEvents.Err else 0u)


    /// The model refused an op, by its own refusal type rather than by a
    /// message: raised inside `execOp` so that `executeEmulated` can tell a
    /// refusal from a defect without reading text.
    exception private ModelRefusal of string

    /// `close(2)`. A refusal is the model's, and is reported as one; an errno
    /// comes back, because that is an answer.
    let private closeFd (fd : int) (system : UnixSystem<int, string>) : Result<UnixSystem<int, string>, UnixError> =
        match UnixDescriptor.close fd system with
        | Error refusal -> raise (ModelRefusal $"close of fd %d{fd} refused: %s{CloseRefusal.describe refusal}")
        | Ok (SyscallAnswer.Failed error, _) -> Error error
        | Ok (SyscallAnswer.Completed _, system) -> Ok system

    let serializeOp (op : FuzzOp) : string =
        match op with
        | FuzzOp.NewSocket s -> $"sock:%d{s}"
        | FuzzOp.Listen s -> $"lstn:%d{s}"
        | FuzzOp.Connect (c, l) -> $"conn:%d{c}:%d{l}"
        | FuzzOp.ConnectDead c -> $"conndead:%d{c}"
        | FuzzOp.Accept (l, s) -> $"acpt:%d{l}:%d{s}"
        | FuzzOp.Close s -> $"close:%d{s}"
        | FuzzOp.Dup (s, s2) -> $"dup:%d{s}:%d{s2}"
        | FuzzOp.NewPort p -> $"port:%d{p}"
        | FuzzOp.Add (p, t, m) -> $"add:%d{p}:%d{t}:%d{m}"
        | FuzzOp.Mod (p, t, m) -> $"mod:%d{p}:%d{t}:%d{m}"
        | FuzzOp.Del (p, t) -> $"del:%d{p}:%d{t}"
        | FuzzOp.EpollAdd (p, t, e) -> $"eadd:%d{p}:%d{t}:%d{e}"
        | FuzzOp.EpollMod (p, t, e) -> $"emod:%d{p}:%d{t}:%d{e}"
        | FuzzOp.Wait (p, n) -> $"wait:%d{p}:%d{n}"
        | FuzzOp.Poll (s, e) -> $"poll:%d{s}:%d{e}"

    let serialize (ops : FuzzOp list) : string =
        ops |> List.map serializeOp |> String.concat " "

    /// Inverse of `serialize`; the corpus stores sequences in serialized form.
    let parseOp (token : string) : FuzzOp =
        let parts = token.Split ':'

        let arg (i : int) : int =
            match Int32.TryParse parts.[i] with
            | true, value -> value
            | false, _ -> failwith $"SocketFuzz.parseOp: op '%s{token}' has a non-integer argument."

        let events (i : int) : uint32 =
            match UInt32.TryParse parts.[i] with
            | true, value -> value
            | false, _ ->
                failwith $"SocketFuzz.parseOp: op '%s{token}' has events that are not a 32-bit unsigned integer."

        match parts.[0], parts.Length with
        | "sock", 2 -> FuzzOp.NewSocket (arg 1)
        | "lstn", 2 -> FuzzOp.Listen (arg 1)
        | "conn", 3 -> FuzzOp.Connect (arg 1, arg 2)
        | "conndead", 2 -> FuzzOp.ConnectDead (arg 1)
        | "acpt", 3 -> FuzzOp.Accept (arg 1, arg 2)
        | "close", 2 -> FuzzOp.Close (arg 1)
        | "dup", 3 -> FuzzOp.Dup (arg 1, arg 2)
        | "port", 2 -> FuzzOp.NewPort (arg 1)
        | "add", 4 -> FuzzOp.Add (arg 1, arg 2, arg 3)
        | "mod", 4 -> FuzzOp.Mod (arg 1, arg 2, arg 3)
        | "del", 3 -> FuzzOp.Del (arg 1, arg 2)
        | "eadd", 4 -> FuzzOp.EpollAdd (arg 1, arg 2, events 3)
        | "emod", 4 -> FuzzOp.EpollMod (arg 1, arg 2, events 3)
        | "wait", 3 -> FuzzOp.Wait (arg 1, arg 2)
        | "poll", 3 -> FuzzOp.Poll (arg 1, arg 2)
        | _ -> failwith $"SocketFuzz.parseOp: unrecognised op '%s{token}'."

    let parse (line : string) : FuzzOp list =
        line.Split (' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseOp
        |> List.ofSeq

    /// `epoll_wait`'s reported events, in the order `harness.c`'s
    /// `mask_string` prints them: the five conditions the .NET shim names
    /// first, in the order the corpus has always printed them, then the rest
    /// of Linux's named readiness bits.
    let private epollBitNames : (uint32 * string) list =
        [
            EpollEvents.In, "IN"
            EpollEvents.Out, "OUT"
            EpollEvents.RdHup, "RDHUP"
            EpollEvents.Hup, "HUP"
            EpollEvents.Err, "ERR"
            EpollEvents.Pri, "PRI"
            EpollEvents.RdNorm, "RDNORM"
            EpollEvents.RdBand, "RDBAND"
            EpollEvents.WrNorm, "WRNORM"
            EpollEvents.WrBand, "WRBAND"
            EpollEvents.Msg, "MSG"
        ]

    /// Canonical mask rendering, shared with the harness. Refuses a bit with
    /// no name rather than printing it, as the harness does.
    let private maskString (events : uint32) : string =
        let known = epollBitNames |> List.fold (fun acc (bit, _) -> acc ||| bit) 0u

        if events &&& ~~~known <> 0u then
            failwith $"INTERPRETER-DRIVER BUG: epoll_wait reported events 0x%08x{events}, outside Linux's named bits."

        epollBitNames
        |> List.choose (fun (bit, name) -> if events &&& bit <> 0u then Some name else None)
        |> String.concat "+"

    /// Linux's `<poll.h>` names, in the order `harness.c`'s
    /// `poll_mask_string` prints them.
    let private pollBitNames : (int16 * string) list =
        [
            0x0001s, "IN"
            0x0002s, "PRI"
            0x0004s, "OUT"
            0x0008s, "ERR"
            0x0010s, "HUP"
            0x0020s, "NVAL"
            0x0040s, "RDNORM"
            0x0080s, "RDBAND"
            0x0100s, "WRNORM"
            0x0200s, "WRBAND"
            0x0400s, "MSG"
            0x2000s, "RDHUP"
        ]

    /// `poll(2)`'s `revents`, in Linux's own numbering. Separate from
    /// `maskString` because the two alphabets differ: poll has `NVAL`, which
    /// is not a readiness condition at all, and the `*NORM`/`*BAND` bits epoll
    /// interest cannot ask for.
    ///
    /// Refuses a bit with no name rather than printing it, as the harness does:
    /// a kernel that reported one would be answering something neither side's
    /// transcript can say.
    let private pollMaskString (revents : int16) : string =
        let known = pollBitNames |> List.fold (fun acc (bit, _) -> acc ||| bit) 0s

        if revents &&& ~~~known <> 0s then
            failwith
                $"INTERPRETER-DRIVER BUG: poll reported revents 0x%04x{uint16 revents}, outside Linux's named bits."

        pollBitNames
        |> List.choose (fun (bit, name) -> if revents &&& bit <> 0s then Some name else None)
        |> String.concat "+"

    /// `UnixError` case names are errno names, which is also what the
    /// harness's `strerrorname_np` prints — one vocabulary by construction.
    let private errName (e : UnixError) : string = $"%A{e}"

    /// `EpollCtlError` back to the errno `epoll_ctl(2)` answers.
    let private registrationErrName (e : EpollCtlError) : string = errName (EpollCtlError.toErrno e)

    /// The listener ports the emulated side assigns, in `Listen` op order.
    /// Fixed and below `UnixSystem.defaultEphemeralPortRange` (32768+),
    /// so a client's implicit bind can never collide with one. The harness
    /// uses real ephemeral ports instead; port numbers are never compared.
    let private listenerPortBase : uint16 = 20000us

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    type private ExecState =
        {
            Kernel : UnixSystem<int, string>
            /// Slot to fd. Absent = never assigned, or closed.
            SlotFd : Map<int, int>
            NextListenerPort : uint16
        }

    let private slotFd (slot : int) (state : ExecState) : int =
        match Map.tryFind slot state.SlotFd with
        | Some fd -> fd
        | None ->
            failwith
                $"INTERPRETER-DRIVER BUG: op names slot %d{slot}, which holds no fd — the generator is supposed to be constructive."

    let private socketIdOfSlot (slot : int) (state : ExecState) : SocketId =
        match FileDescriptorRegistry.tryFind (slotFd slot state) state.Kernel.Process.FileDescriptors with
        | Some description ->
            match description.Target with
            | OpenFileTarget.Socket socketId -> socketId
            | other -> failwith $"INTERPRETER-DRIVER BUG: slot %d{slot} is %O{other}, not a socket."
        | None -> failwith $"INTERPRETER-DRIVER BUG: slot %d{slot}'s fd is not live."

    let private assignSlot (slot : int) (fd : int) (state : ExecState) : ExecState =
        if Map.containsKey slot state.SlotFd then
            failwith $"INTERPRETER-DRIVER BUG: slot %d{slot} assigned twice."

        { state with
            SlotFd = Map.add slot fd state.SlotFd
        }

    /// `epoll_ctl` of the target slot on the port slot, with `data` the target's
    /// slot number, as the harness passes it.
    ///
    /// A refusal is a skip only for raw events, which may ask for modes the
    /// model refuses. The `SocketEvents` ops register exactly what the .NET
    /// shim registers, which the model answers in full, so a refusal of one is
    /// a finding.
    let private epollCtl
        (rawEvents : bool)
        (port : int)
        (op : int)
        (target : int)
        (events : uint32)
        (state : ExecState)
        : string * ExecState
        =
        match
            UnixPoll.epollCtl
                (slotFd port state)
                op
                (slotFd target state)
                (EpollEventArgument.Readable (events, uint64 target))
                state.Kernel
        with
        | Ok (EpollCtlAnswer.Changed, kernel) ->
            "ok",
            { state with
                Kernel = kernel
            }
        | Ok (EpollCtlAnswer.Failed reason, _) -> registrationErrName reason, state
        | Error refusal when rawEvents ->
            raise (ModelRefusal $"epoll_ctl refused: %s{EpollCtlRefusal.describe refusal}")
        | Error refusal -> failwith $"INTERPRETER-DRIVER BUG: %s{EpollCtlRefusal.describe refusal}"

    /// One op against the emulated kernel: the transcript token, and the state
    /// after. Any `failwith` escaping this is the kernel refusing (or, if it
    /// says "interpreter bug", a finding); `executeEmulated` classifies.
    let private execOp (op : FuzzOp) (state : ExecState) : string * ExecState =
        match op with
        | FuzzOp.NewSocket slot ->
            let fd, kernel =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp state.Kernel

            "ok",
            assignSlot
                slot
                fd
                { state with
                    Kernel = kernel
                }
        | FuzzOp.Listen slot ->
            // The trivially-conflict-free composite bind+listen, constructed
            // directly: bind/listen semantics live in the native handler, not
            // in UnixSystem<int, string>, and are deliberately outside the fuzzed
            // vocabulary (see the plan doc's altitude option).
            let socketId = socketIdOfSlot slot state
            let sock = UnixMachineState.socket socketId state.Kernel.Machine

            match sock.Phase with
            | SocketPhase.Idle -> ()
            | phase ->
                failwith $"INTERPRETER-DRIVER BUG: lstn on a socket in %A{phase}; the generator listens only on Idle."

            let port = state.NextListenerPort

            let kernel =
                { state.Kernel with
                    Machine =
                        { state.Kernel.Machine with
                            Sockets =
                                Map.add
                                    socketId
                                    { sock with
                                        Binding =
                                            Some
                                                {
                                                    Endpoint =
                                                        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port
                                                    LockedAddress = None
                                                    LockedPort = false
                                                }
                                        Phase =
                                            SocketPhase.Listening
                                                {
                                                    Backlog = 8
                                                    Queue = []
                                                }
                                    }
                                    state.Kernel.Machine.Sockets
                        }
                }

            "ok",
            { state with
                Kernel = kernel
                NextListenerPort = port + 1us
            }
        | FuzzOp.Connect (client, listener) ->
            // The listening endpoint belongs to the socket, not to any one
            // slot of it, so ask the kernel at connect time — the harness
            // does the same with getsockname.
            let endpoint =
                match (UnixMachineState.socket (socketIdOfSlot listener state) state.Kernel.Machine).Binding with
                | Some binding when binding.Endpoint.Port <> 0us -> binding.Endpoint
                | _ -> failwith $"INTERPRETER-DRIVER BUG: conn targets slot %d{listener}, whose socket never listened."

            let socketId = socketIdOfSlot client state

            let outcome, kernel =
                match UnixConnection.connectSocket socketId true 16 inetFamily (Some endpoint) state.Kernel with
                | Ok answer -> answer
                | Error refusal -> raise (ModelRefusal (ConnectRefusal.describe refusal))

            let token =
                match outcome with
                | ConnectOutcome.Completed -> "ok"
                | ConnectOutcome.Failed e -> errName e

            token,
            { state with
                Kernel = kernel
            }
        | FuzzOp.ConnectDead client ->
            // Loopback port 1: privileged, and nothing in either world ever
            // listens there, so the connect is a deterministic refusal.
            let socketId = socketIdOfSlot client state

            let outcome, kernel =
                match
                    UnixConnection.connectSocket
                        socketId
                        true
                        16
                        inetFamily
                        (Some (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 1us))
                        state.Kernel
                with
                | Ok answer -> answer
                | Error refusal -> raise (ModelRefusal (ConnectRefusal.describe refusal))

            let token =
                match outcome with
                | ConnectOutcome.Completed -> "ok"
                | ConnectOutcome.Failed e -> errName e

            token,
            { state with
                Kernel = kernel
            }
        | FuzzOp.Accept (listener, newSlot) ->
            let socketId = socketIdOfSlot listener state

            match (UnixMachineState.socket socketId state.Kernel.Machine).Phase with
            | SocketPhase.Listening listenState when List.isEmpty listenState.Queue ->
                // Nonblocking accept of an empty queue, exactly accept4's
                // answer; `acceptConnection` requires a nonempty queue.
                "EAGAIN", state
            | _ ->

            let fd, _, kernel = UnixConnection.acceptConnection socketId state.Kernel

            "ok",
            assignSlot
                newSlot
                fd
                { state with
                    Kernel = kernel
                }
        | FuzzOp.Close slot ->
            let fd = slotFd slot state

            match closeFd fd state.Kernel with
            | Ok kernel ->
                "ok",
                { state with
                    Kernel = kernel
                    SlotFd = Map.remove slot state.SlotFd
                }
            | Error UnixError.EBADF ->
                // Unreachable, and known to be: the generator is constructive,
                // so it only ever closes a slot it knows holds a live fd (see
                // `slotFd`, which crashes rather than inventing one). Kept
                // because it is `close(2)`'s only errno and a generator that
                // learned to close twice should find this arm waiting rather
                // than a crash; measured by mutation, which turned EBADF into
                // success here and left the whole PawPrint suite green.
                "EBADF", state
            | Error error ->
                // EBADF is `close(2)`'s only errno; anything else means the
                // library grew a failure this generator does not know how to
                // shrink towards.
                failwith $"close of fd %d{fd} answered %O{error}, which is not EBADF"
        | FuzzOp.Dup (slot, newSlot) ->
            match FileDescriptorRegistry.dup (slotFd slot state) state.Kernel.Process.FileDescriptors with
            | Ok (fd, registry) ->
                "ok",
                assignSlot
                    newSlot
                    fd
                    { state with
                        Kernel =
                            { state.Kernel with
                                Process =
                                    { state.Kernel.Process with
                                        FileDescriptors = registry
                                    }
                            }
                    }
            | Error FileDescriptorDupError.BadFd -> "EBADF", state
        | FuzzOp.NewPort slot ->
            let fd, registry =
                FileDescriptorRegistry.createSocketEventPort state.Kernel.Process.FileDescriptors

            "ok",
            assignSlot
                slot
                fd
                { state with
                    Kernel =
                        { state.Kernel with
                            Process =
                                { state.Kernel.Process with
                                    FileDescriptors = registry
                                }
                        }
                }
        | FuzzOp.Add (port, target, mask) -> epollCtl false port 1 target (eventsOfMask mask) state
        | FuzzOp.Mod (port, target, mask) -> epollCtl false port 3 target (eventsOfMask mask) state
        | FuzzOp.EpollAdd (port, target, events) -> epollCtl true port 1 target events state
        | FuzzOp.EpollMod (port, target, events) -> epollCtl true port 3 target events state
        | FuzzOp.Del (port, target) -> epollCtl false port 2 target 0u state
        | FuzzOp.Wait (port, maxEvents) ->
            let portId =
                match FileDescriptorRegistry.tryFindId (slotFd port state) state.Kernel.Process.FileDescriptors with
                | Some id -> id
                | None -> failwith $"INTERPRETER-DRIVER BUG: wait's port slot %d{port} is not live."

            // The predicate a parked waiter is polled against and the drain its
            // woken handler performs read the same annotated walk, so they
            // cannot disagree; asked here because a generated sequence drives
            // the port through phases no hand-written row reaches.
            let system = state.Kernel
            let predicted = SocketEventPort.hasDeliverableEvent portId system
            let delivered, system = SocketEventPort.drain portId maxEvents system

            if List.isEmpty delivered = predicted then
                failwith
                    $"INTERPRETER-DRIVER BUG: SocketEventPort.hasDeliverableEvent answered %b{predicted} of port %O{portId}, but draining it reported %d{List.length delivered} events."

            let kernel = system

            let batch =
                delivered
                |> List.map (fun (data, mask) -> $"%d{data}/%s{maskString mask}")
                |> String.concat ","

            $"[%s{batch}]",
            { state with
                Kernel = kernel
            }
        | FuzzOp.Poll (slot, events) ->
            // The whole point of this op: it asks `poll(2)` the same question
            // the harness asks the real kernel, so a generated sequence that
            // drives a socket into any phase compares this library's answer
            // against the kernel's rather than against a hand-written row.
            // `poll(2)` mutates nothing, so the state passes through.
            if events < 0 || events > 0xFFFF then
                failwith $"INTERPRETER-DRIVER BUG: poll events %d{events} is not a 16-bit mask."

            let fd = slotFd slot state

            if
                FileDescriptorRegistry.tryFindId fd state.Kernel.Process.FileDescriptors
                |> Option.isNone
            then
                failwith $"INTERPRETER-DRIVER BUG: poll's slot %d{slot} is not live."

            let entry : PollEntry =
                {
                    Fd = fd
                    Events = int16 (uint16 events)
                }

            match UnixPoll.poll [ entry ] 0 state.Kernel with
            | Error refusal -> raise (ModelRefusal $"poll of fd %d{fd} refused: %s{PollRefusal.describe refusal}")
            | Ok ([ reported ], _) -> $"<%s{pollMaskString reported}>", state
            | Ok (reported, _) ->
                failwith $"INTERPRETER-DRIVER BUG: one poll entry was answered with %d{List.length reported} reports."

    /// Run one sequence against a fresh `UnixSystem.initial SimulatedUnixPlatform.linuxX64` (Linux
    /// flavour, matching the harness's kernel). Both invariant checkers run
    /// after every op — a generated sequence that corrupts the state is a
    /// finding even when every transcript token agrees.
    let executeEmulated (ops : FuzzOp list) : EmulatedRun =
        let mutable state =
            {
                Kernel = UnixSystem.initial SimulatedUnixPlatform.linuxX64
                SlotFd = Map.empty
                NextListenerPort = listenerPortBase
            }

        let transcript = StringBuilder ()
        let mutable result = None
        let mutable index = 0

        for op in ops do
            if Option.isNone result then
                // A refusal is the model saying, in its own type, that the
                // input is outside what it answers; any exception is a bug
                // in the model or in this driver.
                let outcome =
                    try
                        Ok (execOp op state)
                    with
                    | ModelRefusal message -> Error (EmulatedRun.Refused (index, message))
                    | Failure message -> Error (EmulatedRun.Defect (index, message))

                match outcome with
                | Error classified -> result <- Some classified
                | Ok (token, next) ->
                    let defects =
                        (UnixSystem.checkInvariants next.Kernel |> List.map (sprintf "%A"))
                        @ (FileDescriptorRegistry.checkInvariants next.Kernel.Process.FileDescriptors
                           |> List.map (sprintf "%A"))

                    match defects with
                    | [] ->
                        state <- next

                        if transcript.Length > 0 then
                            transcript.Append ' ' |> ignore

                        transcript.Append token |> ignore
                    | defects -> result <- Some (EmulatedRun.Defect (index, String.concat "; " defects))

            index <- index + 1

        match result with
        | Some r -> r
        | None -> EmulatedRun.Transcript (transcript.ToString ())

    // --- Generation ---

    /// The phase of one shadow *socket* (or event port). Slots alias sockets
    /// — a dup shares the socket — so the phase lives here and every slot of
    /// the socket sees a change at once.
    ///
    /// The shadow is tracked only closely enough to keep generation
    /// constructive (mostly-lawful ops chosen from what is applicable, rather
    /// than generate-and-filter). It is *not* a model: when it guesses wrong
    /// the sequence merely lands on a comparable errno row or a counted
    /// refusal skip, never on a harness abort — the one hard rule is that an
    /// op never names a closed or never-assigned slot.
    [<RequireQualifiedAccess>]
    type private Shadow =
        | Idle
        /// Listening, with the shadow's count of unaccepted queued connects.
        | Listening of queued : int
        /// A nonblocking connect toward a live listener is in flight; the
        /// next connect on this socket is the completion-reporting retry.
        | Connecting
        /// A conndead is in flight; the next connect delivers the refusal.
        | Refused
        | Established
        | Port

    type private GenState =
        {
            /// Live slot to the shadow socket it names. Dup'd slots share one.
            SlotSocket : Map<int, int>
            /// Shadow socket to its phase.
            SocketShadow : Map<int, Shadow>
            /// (port slot, target slot) pairs the shadow believes registered.
            Registrations : Set<int * int>
            NextSlot : int
            NextSocket : int
        }

    let private pick (rng : Random) (items : 'a list) : 'a = items.[rng.Next items.Length]

    /// An interest mask, biased toward the readable/writable bits: CLOSE and
    /// ERROR are unmaskable at delivery so their presence is rarely
    /// interesting, and interest-0 registrations are a measured edge worth
    /// visiting occasionally.
    let private randomMask (rng : Random) : int =
        match rng.Next 10 with
        | 0 -> 0x00
        | 1 -> 0x01 // READ
        | 2 -> 0x02 // WRITE
        | 3 -> 0x04 // READCLOSE
        | 4
        | 5 -> 0x07 // READ|WRITE|READCLOSE — what SocketAsyncEngine registers
        | 6 -> 0x03
        | 7 -> 0x05
        | 8 -> rng.Next 0x20 // anything, CLOSE/ERROR bits included
        | _ -> 0x1F

    /// Raw `<sys/epoll.h>` events for `EpollAdd` and `EpollMod`: the readiness
    /// bits the .NET shim never asks for, alone and together, and bits epoll
    /// names nothing for, which it stores and never reports. Edge-triggered
    /// but for one draw in 32, which asks for a mode the model refuses
    /// (level-triggering, `EPOLLEXCLUSIVE`, `EPOLLONESHOT` or `EPOLLWAKEUP`),
    /// so that the failures which precede a refusal are compared too.
    let private randomEpollEvents (rng : Random) : uint32 =
        let readiness =
            [
                EpollEvents.In
                EpollEvents.Pri
                EpollEvents.Out
                EpollEvents.Err
                EpollEvents.Hup
                EpollEvents.RdNorm
                EpollEvents.RdBand
                EpollEvents.WrNorm
                EpollEvents.WrBand
                EpollEvents.Msg
                EpollEvents.RdHup
            ]

        let body =
            match rng.Next 8 with
            | 0 -> 0u
            | 1
            | 2
            | 3
            | 4 -> pick rng readiness
            | 5 -> readiness |> List.filter (fun _ -> rng.Next 2 = 0) |> List.fold (|||) 0u
            | 6 -> uint32 (rng.Next 0x10000000)
            | _ -> pick rng [ 0x20u ; 0x800u ; 0x1000u ; 0x4000u ; 0x8000u ; 0x100000u ; 0x8000000u ]

        if rng.Next 32 = 0 then
            body
            ||| pick
                    rng
                    [
                        0u
                        EpollEvents.EdgeTriggered ||| EpollEvents.Exclusive
                        EpollEvents.EdgeTriggered ||| EpollEvents.OneShot
                        EpollEvents.EdgeTriggered ||| EpollEvents.WakeUp
                    ]
        else
            body ||| EpollEvents.EdgeTriggered

    /// A `poll(2)` request mask, in Linux's own `<poll.h>` numbering.
    ///
    /// 0 and the output-only bits are drawn deliberately, not as an
    /// afterthought: `ERR`, `HUP` and `NVAL` are reported whether or not they
    /// were asked for, so a generator that only ever asked for `IN`/`OUT`
    /// would never exercise the one rule this projection can get wrong. `PRI`
    /// is included for the same reason in the other direction — no modelled
    /// level sets it, so every draw of it must come back empty. The bits the
    /// .NET shim never asks for (`RDNORM`, `WRNORM`, `WRBAND`, `RDHUP`) are
    /// drawn alone as well as inside the whole 16-bit space, so each is asked
    /// of every phase a sequence reaches.
    let private randomPollMask (rng : Random) : int =
        match rng.Next 16 with
        | 0 -> 0x00 // ask for nothing; ERR/HUP/NVAL must still be reported
        | 1 -> 0x01 // IN
        | 2 -> 0x04 // OUT
        | 3
        | 4 -> 0x05 // IN|OUT — what SocketPal.SelectViaPoll asks for
        | 5 -> 0x02 // PRI alone
        | 6 -> 0x08 // ERR alone, an output-only bit in the request
        | 7 -> 0x10 // HUP alone, likewise
        | 8 -> 0x20 // NVAL alone, likewise
        | 9 -> 0x40 // RDNORM, which rides with IN
        | 10 -> 0x100 // WRNORM, which rides with OUT
        | 11 -> 0x200 // WRBAND, which TCP never presents
        | 12 -> 0x2000 // RDHUP, which the .NET shim never asks for
        | 13 -> 0xFFFF // everything: the level itself
        | 14 -> rng.Next 0x40 // anything inside the six bits the .NET shim asks for
        | _ -> rng.Next 0x10000 // anything at all, unnamed bits included

    /// One generated sequence. Constructive: every op names live slots and
    /// stays inside the modelled envelope where the shadow can tell — e.g. no
    /// slot of a listener with a nonempty shadow queue is ever closed (a
    /// modelled refusal), and a port is never a registration target (nested
    /// epoll is refused). The op-kind weights are themselves drawn from
    /// `rng`, so the distribution is fuzzed too.
    let generate (rng : Random) : FuzzOp list =
        let targetLength = 6 + rng.Next 11

        // Per-sequence biases: each sequence explores a differently-shaped
        // regime (connect-heavy, registration-heavy, churn-heavy, ...).
        let wNew = 1 + rng.Next 3
        let wConnect = 1 + rng.Next 4
        let wRegister = 1 + rng.Next 4
        let wWait = 1 + rng.Next 3
        let wPoll = 1 + rng.Next 3
        let wChurn = rng.Next 3

        let mutable state =
            {
                SlotSocket = Map.empty
                SocketShadow = Map.empty
                Registrations = Set.empty
                NextSlot = 0
                NextSocket = 0
            }

        let freshSlot (shadow : Shadow option) : int =
            let slot = state.NextSlot

            match shadow with
            | None ->
                // Burned: the op that would assign it is expected to fail.
                state <-
                    { state with
                        NextSlot = slot + 1
                    }
            | Some shadow ->
                let socket = state.NextSocket

                state <-
                    { state with
                        SlotSocket = Map.add slot socket state.SlotSocket
                        SocketShadow = Map.add socket shadow state.SocketShadow
                        NextSlot = slot + 1
                        NextSocket = socket + 1
                    }

            slot

        let shadowOfSlot (slot : int) : Shadow =
            Map.find (Map.find slot state.SlotSocket) state.SocketShadow

        let setShadowOfSlot (slot : int) (shadow : Shadow) : unit =
            state <-
                { state with
                    SocketShadow = Map.add (Map.find slot state.SlotSocket) shadow state.SocketShadow
                }

        let ops = ResizeArray<FuzzOp> ()

        // Give every sequence something to fuzz: a port and a socket exist
        // before the weighted walk starts.
        ops.Add (FuzzOp.NewPort (freshSlot (Some Shadow.Port)))
        ops.Add (FuzzOp.NewSocket (freshSlot (Some Shadow.Idle)))

        while ops.Count < targetLength do
            let slotsWhere (predicate : Shadow -> bool) : int list =
                state.SlotSocket
                |> Map.toList
                |> List.filter (fun (_, socket) -> predicate (Map.find socket state.SocketShadow))
                |> List.map fst

            let ports = slotsWhere ((=) Shadow.Port)
            let idle = slotsWhere ((=) Shadow.Idle)

            let listeners =
                slotsWhere (fun s ->
                    match s with
                    | Shadow.Listening _ -> true
                    | _ -> false
                )

            let queuedListeners =
                slotsWhere (fun s ->
                    match s with
                    | Shadow.Listening q -> q > 0
                    | _ -> false
                )

            let emptyListeners =
                slotsWhere (fun s ->
                    match s with
                    | Shadow.Listening q -> q = 0
                    | _ -> false
                )

            let connecting = slotsWhere ((=) Shadow.Connecting)
            let refused = slotsWhere ((=) Shadow.Refused)
            let allSockets = slotsWhere ((<>) Shadow.Port)

            // Weighted candidate thunks; each appends its op and updates the
            // shadow. Multiplicity in the list is the weight.
            let candidates = ResizeArray<unit -> unit> ()

            let addWeighted (weight : int) (action : unit -> unit) : unit =
                for _ in 1..weight do
                    candidates.Add action

            addWeighted wNew (fun () -> ops.Add (FuzzOp.NewSocket (freshSlot (Some Shadow.Idle))))

            if ports.Length < 2 then
                addWeighted 1 (fun () -> ops.Add (FuzzOp.NewPort (freshSlot (Some Shadow.Port))))

            if not (List.isEmpty idle) then
                addWeighted
                    wNew
                    (fun () ->
                        let slot = pick rng idle
                        ops.Add (FuzzOp.Listen slot)
                        setShadowOfSlot slot (Shadow.Listening 0)
                    )

                addWeighted
                    wConnect
                    (fun () ->
                        let client = pick rng idle
                        ops.Add (FuzzOp.ConnectDead client)
                        setShadowOfSlot client Shadow.Refused
                    )

                if not (List.isEmpty listeners) then
                    addWeighted
                        (wConnect * 2)
                        (fun () ->
                            let client = pick rng idle
                            let listener = pick rng listeners
                            ops.Add (FuzzOp.Connect (client, listener))
                            setShadowOfSlot client Shadow.Connecting

                            match shadowOfSlot listener with
                            | Shadow.Listening q -> setShadowOfSlot listener (Shadow.Listening (q + 1))
                            | _ -> ()
                        )

            // The completion-reporting retry (SUCCESS) and the
            // refusal-delivering retry (ECONNREFUSED, then the socket resets
            // to a connectable Idle): both measured rows, both producers.
            for retryable, after in [ connecting, Shadow.Established ; refused, Shadow.Idle ] do
                if not (List.isEmpty retryable) then
                    addWeighted
                        wConnect
                        (fun () ->
                            let client = pick rng retryable

                            if List.isEmpty listeners then
                                ops.Add (FuzzOp.ConnectDead client)
                            else
                                ops.Add (FuzzOp.Connect (client, pick rng listeners))

                            setShadowOfSlot client after
                        )

            if not (List.isEmpty queuedListeners) then
                addWeighted
                    (wConnect * 2)
                    (fun () ->
                        let listener = pick rng queuedListeners
                        ops.Add (FuzzOp.Accept (listener, freshSlot (Some Shadow.Established)))

                        match shadowOfSlot listener with
                        | Shadow.Listening q -> setShadowOfSlot listener (Shadow.Listening (q - 1))
                        | _ -> ()
                    )

            if not (List.isEmpty emptyListeners) then
                // The EAGAIN row: accept of a drained queue. The unused slot
                // number is deliberately burned — neither side assigns it.
                addWeighted
                    1
                    (fun () ->
                        let listener = pick rng emptyListeners
                        ops.Add (FuzzOp.Accept (listener, freshSlot None))
                    )

            let closable =
                slotsWhere (fun shadow ->
                    match shadow with
                    // Destroying a listener over a live queued client is a
                    // modelled refusal, and the shadow cannot tell which
                    // slot's close is the destroying one, so it keeps clear
                    // of every slot of such a listener.
                    | Shadow.Listening q -> q = 0
                    | _ -> true
                )

            if not (List.isEmpty closable) then
                addWeighted
                    wChurn
                    (fun () ->
                        let slot = pick rng closable
                        ops.Add (FuzzOp.Close slot)

                        state <-
                            { state with
                                SlotSocket = Map.remove slot state.SlotSocket
                                Registrations =
                                    state.Registrations |> Set.filter (fun (p, t) -> p <> slot && t <> slot)
                            }
                    )

            if not (List.isEmpty allSockets) && state.SlotSocket.Count < 24 then
                addWeighted
                    wChurn
                    (fun () ->
                        let slot = pick rng allSockets
                        let copy = freshSlot None
                        ops.Add (FuzzOp.Dup (slot, copy))

                        state <-
                            { state with
                                SlotSocket = Map.add copy (Map.find slot state.SlotSocket) state.SlotSocket
                            }
                    )

            if not (List.isEmpty ports) && not (List.isEmpty allSockets) then
                addWeighted
                    (wRegister * 2)
                    (fun () ->
                        let port = pick rng ports
                        let target = pick rng allSockets

                        if rng.Next 3 = 0 then
                            ops.Add (FuzzOp.EpollAdd (port, target, randomEpollEvents rng))
                        else
                            ops.Add (FuzzOp.Add (port, target, randomMask rng))
                        // A duplicate Add is the EEXIST row; the shadow set
                        // is unchanged either way.
                        state <-
                            { state with
                                Registrations = Set.add (port, target) state.Registrations
                            }
                    )

            let registered =
                state.Registrations
                |> Set.filter (fun (p, t) -> Map.containsKey p state.SlotSocket && Map.containsKey t state.SlotSocket)
                |> Set.toList

            if not (List.isEmpty registered) then
                addWeighted
                    wRegister
                    (fun () ->
                        let port, target = pick rng registered

                        if rng.Next 3 = 0 then
                            ops.Add (FuzzOp.EpollMod (port, target, randomEpollEvents rng))
                        else
                            ops.Add (FuzzOp.Mod (port, target, randomMask rng))
                    )

                addWeighted
                    wChurn
                    (fun () ->
                        let port, target = pick rng registered
                        ops.Add (FuzzOp.Del (port, target))

                        state <-
                            { state with
                                Registrations = Set.remove (port, target) state.Registrations
                            }
                    )

            if not (List.isEmpty ports) && not (List.isEmpty allSockets) then
                // The ENOENT rows: MOD/DEL of a pair that may never have been
                // registered.
                addWeighted
                    1
                    (fun () ->
                        let port = pick rng ports
                        let target = pick rng allSockets

                        if rng.Next 2 = 0 then
                            ops.Add (FuzzOp.Mod (port, target, randomMask rng))
                        else
                            ops.Add (FuzzOp.Del (port, target))

                        state <-
                            { state with
                                Registrations = Set.remove (port, target) state.Registrations
                            }
                    )

            if not (List.isEmpty allSockets) then
                addWeighted wPoll (fun () -> ops.Add (FuzzOp.Poll (pick rng allSockets, randomPollMask rng)))

            if not (List.isEmpty ports) then
                addWeighted
                    wWait
                    (fun () ->
                        let port = pick rng ports
                        let maxEvents = pick rng [ 1 ; 2 ; 8 ]
                        ops.Add (FuzzOp.Wait (port, maxEvents))
                    )

            (pick rng (List.ofSeq candidates)) ()

        // Drain every port so each sequence ends by observing whatever the
        // walk left pending.
        for slot, socket in Map.toList state.SlotSocket do
            if Map.find socket state.SocketShadow = Shadow.Port then
                ops.Add (FuzzOp.Wait (slot, 8))

        List.ofSeq ops
