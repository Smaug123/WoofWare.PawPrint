namespace WoofWare.PawPrint.Test

open System
open System.Text
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// One operation of the socket/epoll differential fuzzer's op language
/// (docs/plans/2026-08-22-socket-epoll-fuzzer.md). Slots name descriptors on
/// both sides of the comparison; each side keeps its own slot-to-fd map, and
/// raw fd numbers never appear in a transcript. Interest masks are the PAL's
/// `SocketEvents` bits (0..0x1F), which translate 1:1 to epoll bits.
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
    | Wait of port : int * maxEvents : int
    /// `poll(2)` over a single slot, with timeout 0. The `events` mask is in
    /// the PAL's `PollEvents` alphabet (`IN` 0x1, `PRI` 0x2, `OUT` 0x4,
    /// `ERR` 0x8, `HUP` 0x10, `NVAL` 0x20) — a *different* alphabet from the
    /// `SocketEvents` bits `Add`/`Mod` carry, which number different
    /// conditions with the same small integers.
    | Poll of slot : int * events : int

/// How the emulated side answered one whole sequence.
[<RequireQualifiedAccess>]
type EmulatedRun =
    /// Every op answered; the transcript is comparable with the harness's.
    | Transcript of string
    /// The kernel refused an op with a modelled `failwith` — the sequence is
    /// outside the modelled envelope, and the comparison skips it.
    | Refused of opIndex : int * message : string
    /// An "interpreter bug" refusal, or a `checkInvariants` defect: reaching
    /// either through the public surface is a finding, never a skip.
    | Defect of opIndex : int * message : string

[<RequireQualifiedAccess>]
module SocketFuzz =

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
        | "wait", 3 -> FuzzOp.Wait (arg 1, arg 2)
        | "poll", 3 -> FuzzOp.Poll (arg 1, arg 2)
        | _ -> failwith $"SocketFuzz.parseOp: unrecognised op '%s{token}'."

    let parse (line : string) : FuzzOp list =
        line.Split (' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseOp
        |> List.ofSeq

    /// Canonical mask rendering, shared with the harness: bits in
    /// IN,OUT,RDHUP,HUP,ERR order joined by '+'.
    let private maskString (r : ReadinessLevel) : string =
        [
            if r.In then
                "IN"
            if r.Out then
                "OUT"
            if r.RdHup then
                "RDHUP"
            if r.Hup then
                "HUP"
            if r.Err then
                "ERR"
        ]
        |> String.concat "+"

    /// `poll(2)`'s `revents`, in the PAL's alphabet. Separate from
    /// `maskString` because the two alphabets differ: poll has no `RDHUP` (the
    /// PAL never asks for it) and does have `NVAL`, which is not a readiness
    /// condition at all.
    let private pollMaskString (r : PollEvents) : string =
        [
            if r.In then
                "IN"
            if r.Pri then
                "PRI"
            if r.Out then
                "OUT"
            if r.Err then
                "ERR"
            if r.Hup then
                "HUP"
            if r.Nval then
                "NVAL"
        ]
        |> String.concat "+"

    /// `UnixError` case names are errno names, which is also what the
    /// harness's `strerrorname_np` prints — one vocabulary by construction.
    let private errName (e : UnixError) : string = $"%A{e}"

    /// `SocketEventRegistrationError` back to the errno `epoll_ctl(2)` answers
    /// (each case's docstring names it).
    let private registrationErrName (e : SocketEventRegistrationError) : string =
        match e with
        | SocketEventRegistrationError.BadPortFd -> "EBADF"
        | SocketEventRegistrationError.BadTargetFd -> "EBADF"
        | SocketEventRegistrationError.TargetNotPollable -> "EPERM"
        | SocketEventRegistrationError.NotAnEventPort -> "EINVAL"
        | SocketEventRegistrationError.AlreadyRegistered -> "EEXIST"
        | SocketEventRegistrationError.NotRegistered -> "ENOENT"

    /// The listener ports the emulated side assigns, in `Listen` op order.
    /// Fixed and below `EmulatedKernel.defaultEphemeralPortRange` (32768+),
    /// so a client's implicit bind can never collide with one. The harness
    /// uses real ephemeral ports instead; port numbers are never compared.
    let private listenerPortBase : uint16 = 20000us

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    type private ExecState =
        {
            Kernel : EmulatedKernel
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
        match FileDescriptorRegistry.tryFind (slotFd slot state) state.Kernel.FileDescriptors with
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

    /// One op against the emulated kernel: the transcript token, and the state
    /// after. Any `failwith` escaping this is the kernel refusing (or, if it
    /// says "interpreter bug", a finding); `executeEmulated` classifies.
    let private execOp (op : FuzzOp) (state : ExecState) : string * ExecState =
        match op with
        | FuzzOp.NewSocket slot ->
            let fd, kernel =
                EmulatedKernel.createSocket SocketDomain.InterNetwork SocketKind.Stream SocketProtocol.Tcp state.Kernel

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
            // in EmulatedKernel, and are deliberately outside the fuzzed
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
                                                }
                                        Phase =
                                            SocketPhase.Listening
                                                {
                                                    Backlog = 8
                                                    Queue = []
                                                }
                                    }
                                    state.Kernel.Sockets
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
                EmulatedKernel.connectSocket socketId true 16 inetFamily (Some endpoint) state.Kernel

            let token =
                match outcome with
                | EmulatedKernel.ConnectOutcome.Completed -> "ok"
                | EmulatedKernel.ConnectOutcome.Failed e -> errName e

            token,
            { state with
                Kernel = kernel
            }
        | FuzzOp.ConnectDead client ->
            // Loopback port 1: privileged, and nothing in either world ever
            // listens there, so the connect is a deterministic refusal.
            let socketId = socketIdOfSlot client state

            let outcome, kernel =
                EmulatedKernel.connectSocket
                    socketId
                    true
                    16
                    inetFamily
                    (Some (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 1us))
                    state.Kernel

            let token =
                match outcome with
                | EmulatedKernel.ConnectOutcome.Completed -> "ok"
                | EmulatedKernel.ConnectOutcome.Failed e -> errName e

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

            let fd, _, kernel = EmulatedKernel.acceptConnection socketId state.Kernel

            "ok",
            assignSlot
                newSlot
                fd
                { state with
                    Kernel = kernel
                }
        | FuzzOp.Close slot ->
            let fd = slotFd slot state

            match KernelSyscall.close fd state.Kernel with
            | Ok kernel ->
                "ok",
                { state with
                    Kernel = kernel
                    SlotFd = Map.remove slot state.SlotFd
                }
            | Error UnixError.EBADF -> "EBADF", state
            | Error error ->
                // EBADF is `close(2)`'s only errno; anything else means the
                // library grew a failure this generator does not know how to
                // shrink towards.
                failwith $"close of fd %d{fd} answered %O{error}, which is not EBADF"
        | FuzzOp.Dup (slot, newSlot) ->
            match FileDescriptorRegistry.dup (slotFd slot state) state.Kernel.FileDescriptors with
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
                FileDescriptorRegistry.createSocketEventPort state.Kernel.FileDescriptors

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
        | FuzzOp.Add (port, target, mask) ->
            let change =
                SocketEventRegistrationChange.Add (SocketEventInterest.ofBits "SocketFuzz" mask, uint64 target)

            match
                EmulatedKernel.changeSocketEventRegistration
                    (slotFd port state)
                    (slotFd target state)
                    change
                    state.Kernel
            with
            | Ok kernel ->
                "ok",
                { state with
                    Kernel = kernel
                }
            | Error e -> registrationErrName e, state
        | FuzzOp.Mod (port, target, mask) ->
            let change =
                SocketEventRegistrationChange.Modify (SocketEventInterest.ofBits "SocketFuzz" mask, uint64 target)

            match
                EmulatedKernel.changeSocketEventRegistration
                    (slotFd port state)
                    (slotFd target state)
                    change
                    state.Kernel
            with
            | Ok kernel ->
                "ok",
                { state with
                    Kernel = kernel
                }
            | Error e -> registrationErrName e, state
        | FuzzOp.Del (port, target) ->
            match
                EmulatedKernel.changeSocketEventRegistration
                    (slotFd port state)
                    (slotFd target state)
                    SocketEventRegistrationChange.Remove
                    state.Kernel
            with
            | Ok kernel ->
                "ok",
                { state with
                    Kernel = kernel
                }
            | Error e -> registrationErrName e, state
        | FuzzOp.Wait (port, maxEvents) ->
            let portId =
                match FileDescriptorRegistry.tryFindId (slotFd port state) state.Kernel.FileDescriptors with
                | Some id -> id
                | None -> failwith $"INTERPRETER-DRIVER BUG: wait's port slot %d{port} is not live."

            let delivered, kernel =
                EmulatedKernel.deliverSocketEvents portId maxEvents state.Kernel

            let batch =
                delivered
                |> List.map (fun (data, mask) -> $"%d{data}/%s{maskString mask}")
                |> String.concat ","

            $"[%s{batch}]",
            { state with
                Kernel = kernel
            }
        | FuzzOp.Poll (slot, events) ->
            // The whole point of this op: it asks the *shared* level function
            // the same question `poll(2)` asks the real kernel, so a generated
            // sequence that drives a socket into any phase compares PawPrint's
            // level against the kernel's rather than against a hand-written
            // row. `poll(2)` mutates nothing, so the state passes through.
            let reported =
                match FileDescriptorRegistry.tryFindId (slotFd slot state) state.Kernel.FileDescriptors with
                | Some descriptionId ->
                    EmulatedKernel.pollReadinessOfDescription descriptionId state.Kernel
                    |> PollEvents.ofLevel (PollEvents.ofBits (int16 events))
                | None -> failwith $"INTERPRETER-DRIVER BUG: poll's slot %d{slot} is not live."

            $"<%s{pollMaskString reported}>", state

    /// Run one sequence against a fresh `EmulatedKernel.initial` (Linux
    /// flavour, matching the harness's kernel). Both invariant checkers run
    /// after every op — a generated sequence that corrupts the state is a
    /// finding even when every transcript token agrees.
    let executeEmulated (ops : FuzzOp list) : EmulatedRun =
        let mutable state =
            {
                Kernel = EmulatedKernel.initial
                SlotFd = Map.empty
                NextListenerPort = listenerPortBase
            }

        let transcript = StringBuilder ()
        let mutable result = None
        let mutable index = 0

        for op in ops do
            if Option.isNone result then
                let outcome =
                    try
                        Ok (execOp op state)
                    with Failure message ->
                        Error message

                match outcome with
                | Error message ->
                    if message.Contains "interpreter bug" || message.Contains "INTERPRETER-DRIVER BUG" then
                        result <- Some (EmulatedRun.Defect (index, message))
                    else
                        result <- Some (EmulatedRun.Refused (index, message))
                | Ok (token, next) ->
                    let defects =
                        (EmulatedKernel.checkInvariants next.Kernel |> List.map (sprintf "%A"))
                        @ (FileDescriptorRegistry.checkInvariants next.Kernel.FileDescriptors
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

    /// A `poll(2)` request mask, in the PAL's `PollEvents` alphabet.
    ///
    /// 0 and the output-only bits are drawn deliberately, not as an
    /// afterthought: `ERR`, `HUP` and `NVAL` are reported whether or not they
    /// were asked for, so a generator that only ever asked for `IN`/`OUT`
    /// would never exercise the one rule this projection can get wrong. `PRI`
    /// is included for the same reason in the other direction — no modelled
    /// level sets it, so every draw of it must come back empty.
    let private randomPollMask (rng : Random) : int =
        match rng.Next 10 with
        | 0 -> 0x00 // ask for nothing; ERR/HUP/NVAL must still be reported
        | 1 -> 0x01 // IN
        | 2 -> 0x04 // OUT
        | 3
        | 4 -> 0x05 // IN|OUT — what SocketPal.SelectViaPoll asks for
        | 5 -> 0x02 // PRI alone
        | 6 -> 0x08 // ERR alone, an output-only bit in the request
        | 7 -> 0x10 // HUP alone, likewise
        | 8 -> 0x20 // NVAL alone, likewise
        | _ -> rng.Next 0x40 // anything inside the six the PAL knows

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
