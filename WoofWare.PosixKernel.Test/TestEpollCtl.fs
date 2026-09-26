namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixPoll.epollCtl`, held to what `epoll_ctl(2)` answered on Linux 6.18.5.
///
/// The measurements are `docs/plans/2026-08-23-posix-kernel-extraction/epoll-ctl.c`'s,
/// taken on 2026-09-26 under aarch64 and x86-64 userlands, which printed
/// identical rows. Its LADDER section is checked in verbatim as
/// `epollCtl/linux-ladder.txt`; its REPORT and EXCL sections are stated below
/// as the levels and the rule they measured.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEpollCtl =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    let private add : int = 1
    let private del : int = 2
    let private modify : int = 3

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }

    let private linux : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.linuxX64

    let private withRegistry
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private withPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        fd, withRegistry registry system

    let private withSocket
        (domain : SocketDomain)
        (kind : SocketKind)
        (phase : SocketPhase)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let socketId = system.Machine.NextSocketId
        let (SocketId raw) = socketId

        let socket =
            {
                Domain = domain
                Kind = kind
                Protocol =
                    match domain, kind with
                    | SocketDomain.Unix, _ -> SocketProtocol.Unspecified
                    | _, SocketKind.Stream -> SocketProtocol.Tcp
                    | _, _ -> SocketProtocol.Udp
                Binding = None
                ReuseAddress = false
                Phase = phase
            }

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        fd,
        { withRegistry registry system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
        }

    let private idleSocket (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        withSocket SocketDomain.InterNetwork SocketKind.Stream SocketPhase.Idle system

    let private withFile (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadWrite system.Process.FileDescriptors

        fd, withRegistry registry system

    let private dupOf (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match FileDescriptorRegistry.dup fd system.Process.FileDescriptors with
        | Ok (copy, registry) -> copy, withRegistry registry system
        | Error error -> failwith $"dup of fd %d{fd} failed: %O{error}"

    let private idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
        | Some id -> id
        | None -> failwith $"fd %d{fd} is not live"

    let private portOf (portFd : int) (system : UnixSystem<int, string>) : SocketEventPortState =
        match FileDescriptorRegistry.tryFindTarget portFd system.Process.FileDescriptors with
        | Some (OpenFileTarget.SocketEventPort portState) -> portState
        | other -> failwith $"expected an event port, got %A{other}"

    let private ready (portFd : int) (system : UnixSystem<int, string>) : (int * OpenFileDescriptionId) list =
        (portOf portFd system).Ready

    let private ctl
        (epfd : int)
        (op : int)
        (fd : int)
        (events : uint32)
        (system : UnixSystem<int, string>)
        : Result<EpollCtlAnswer * UnixSystem<int, string>, EpollCtlRefusal>
        =
        UnixPoll.epollCtl epfd op fd (EpollEventArgument.Readable (events, 42UL)) system

    let private applied
        (epfd : int)
        (op : int)
        (fd : int)
        (events : uint32)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match ctl epfd op fd events system with
        | Ok (EpollCtlAnswer.Changed, system) -> system
        | Ok (EpollCtlAnswer.Failed reason, _) -> failwith $"expected the change to apply, got %O{reason}"
        | Error refusal -> failwith $"expected the change to apply, got a refusal: %s{EpollCtlRefusal.describe refusal}"

    let private errnoName (error : UnixError) : string = $"%A{error}"

    let private edge : uint32 = EpollEvents.EdgeTriggered

    // ------------------------------------------------------------------
    // The errnos
    // ------------------------------------------------------------------

    /// The errno of each failure, stated as literals rather than read back off
    /// `toErrno`, which is the function under test.
    [<Test>]
    let ``each failure carries the errno epoll_ctl answers`` () : unit =
        [
            EpollCtlError.EventUnreadable, UnixError.EFAULT
            EpollCtlError.BadPortFd, UnixError.EBADF
            EpollCtlError.BadTargetFd, UnixError.EBADF
            EpollCtlError.TargetNotPollable, UnixError.EPERM
            EpollCtlError.NotAnEventPort, UnixError.EINVAL
            EpollCtlError.ExclusiveNotPermitted, UnixError.EINVAL
            EpollCtlError.AlreadyRegistered, UnixError.EEXIST
            EpollCtlError.NotRegistered, UnixError.ENOENT
            EpollCtlError.UnrecognisedOperation, UnixError.EINVAL
        ]
        |> List.iter (fun (reason, expected) -> EpollCtlError.toErrno reason |> shouldEqual expected)

    // ------------------------------------------------------------------
    // The measured ladder
    // ------------------------------------------------------------------

    /// The 36 event masks the probe tried in every ladder cell, in its order:
    /// each single bit, then 0, then three `EPOLLEXCLUSIVE` combinations.
    let private ladderEvents : uint32 list =
        [
            for b in 0..31 -> 1u <<< b
            yield 0u
            yield EpollEvents.Exclusive ||| EpollEvents.In
            yield 0xb000001du
            yield EpollEvents.Exclusive ||| EpollEvents.In ||| EpollEvents.RdHup
        ]

    type private LadderRow =
        {
            Epfd : string
            Target : string
            Op : int
            NullEvent : bool
            Registered : bool
            /// The measured answer for each of `ladderEvents`: "ok" or an errno
            /// name.
            Answers : string list
        }

    let private ladderRows : Lazy<LadderRow list> =
        lazy
            let assembly = Assembly.GetExecutingAssembly ()
            let name = "WoofWare.PosixKernel.Test.epollCtl.linux-ladder.txt"

            use stream =
                match assembly.GetManifestResourceStream name with
                | null -> failwith $"embedded resource %s{name} not found"
                | stream -> stream

            use reader = new StreamReader (stream)

            reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))
            |> List.map (fun line ->
                let cell, answers =
                    match line.Split (" : ", 2) with
                    | [| cell ; answers |] -> cell, answers
                    | _ -> failwith $"ladder row has no ' : ': %s{line}"

                let field (key : string) : string =
                    cell.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.pick (fun part ->
                        if part.StartsWith (key + "=", StringComparison.Ordinal) then
                            Some (part.Substring (key.Length + 1))
                        else
                            None
                    )

                let words = answers.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                let common = words.[0]

                let exceptions =
                    words.[1..]
                    |> Array.map (fun word ->
                        match word.Trim('[', ']').Split ':' with
                        | [| events ; answer |] -> Convert.ToUInt32 (events, 16), answer
                        | _ -> failwith $"ladder exception is not [events:answer]: %s{word}"
                    )
                    |> Map.ofArray

                {
                    Epfd = field "epfd"
                    Target = field "target"
                    Op = int (field "op")
                    NullEvent = field "ev" = "NULL"
                    Registered = field "reg" = "1"
                    Answers =
                        ladderEvents
                        |> List.map (fun events -> Map.tryFind events exceptions |> Option.defaultValue common)
                }
            )

    /// A descriptor of the probe's kind `kind`, made in `system`. `epfd` is the
    /// port descriptor already made, for the two kinds defined relative to it.
    let private make (kind : string) (epfd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match kind with
        | "closed" -> 50, system
        | "-1" -> -1, system
        | "file"
        | "dir" -> withFile system
        // The launch shape's standard streams are exactly the probe's pipes.
        | "stdin-pipe" -> 0, system
        | "stdout-pipe" -> 1, system
        | "tcp-idle" -> idleSocket system
        | "tcp-listen" ->
            withSocket
                SocketDomain.InterNetwork
                SocketKind.Stream
                (SocketPhase.Listening
                    {
                        Backlog = 64
                        Queue = []
                    })
                system
        | "udp" -> withSocket SocketDomain.InterNetwork SocketKind.Datagram SocketPhase.Idle system
        | "unix-stream" -> withSocket SocketDomain.Unix SocketKind.Stream SocketPhase.Idle system
        | "unix-dgram" -> withSocket SocketDomain.Unix SocketKind.Datagram SocketPhase.Idle system
        | "epoll" -> withPort system
        | "same-as-epfd" -> epfd, system
        // The probe's `dup` of an epfd that is not open failed, leaving -1.
        | "dup-of-epfd" ->
            match FileDescriptorRegistry.tryFindId epfd system.Process.FileDescriptors with
            | Some _ -> dupOf epfd system
            | None -> -1, system
        | other -> failwith $"ladder row names an unknown kind %s{other}"

    /// The refusal `epollCtl` owes a call that the kernel committed, by what the
    /// call asked for: nesting first, where Linux runs its loop check, then the
    /// modes this library does not model, in `EpollCtlRefusal`'s order.
    let private owedRefusal
        (op : int)
        (targetIsPort : bool)
        (targetFd : int)
        (events : uint32)
        : EpollCtlRefusal option
        =
        if op = add && targetIsPort then
            Some (EpollCtlRefusal.NestedPort targetFd)
        elif op = del then
            None
        elif events &&& EpollEvents.Exclusive <> 0u then
            Some EpollCtlRefusal.Exclusive
        elif events &&& EpollEvents.OneShot <> 0u then
            Some EpollCtlRefusal.OneShot
        elif events &&& EpollEvents.WakeUp <> 0u then
            Some EpollCtlRefusal.WakeUp
        elif events &&& EpollEvents.EdgeTriggered = 0u then
            Some EpollCtlRefusal.LevelTriggered
        else
            None

    /// Every cell of the probe's ladder -- seven kinds of `epfd`, fourteen kinds
    /// of target, nine operation values, a real and a null event pointer, and
    /// a registered target where the target can be registered -- under each of
    /// the 36 event masks. Each is answered with the errno
    /// the real kernel gave, or, where the kernel committed a change this
    /// library does not model, refused with the refusal the call is owed. The
    /// only cells left out are those that start from a registered epoll
    /// target, a nested port this library will not make.
    [<Test>]
    let ``every ladder cell answers as the measured Linux table says`` () : unit =
        let rows = ladderRows.Force ()
        rows |> List.length |> shouldEqual 1908

        let mutable answeredCommits = 0
        let mutable refusals = 0

        // A registered epoll target is a nested port, which this library
        // refuses to make, so those cells have no model state to start from.
        let reachable, unreachable =
            rows |> List.partition (fun row -> not (row.Registered && row.Target = "epoll"))

        unreachable |> List.length |> shouldEqual 18

        let mismatches =
            [
                for row in reachable do
                    for events, measured in List.zip ladderEvents row.Answers do
                        let epfd, system = make row.Epfd (-1) linux
                        let fd, system = make row.Target epfd system

                        let system =
                            if row.Registered then
                                applied epfd add fd (EpollEvents.In ||| edge) system
                            else
                                system

                        let argument =
                            if row.NullEvent then
                                EpollEventArgument.Unreadable
                            else
                                EpollEventArgument.Readable (events, 42UL)

                        let describe = $"%A{row} events 0x%08x{events}"

                        match UnixPoll.epollCtl epfd row.Op fd argument system with
                        | Ok (EpollCtlAnswer.Changed, _) ->
                            if row.Op <> del then
                                answeredCommits <- answeredCommits + 1

                            if measured <> "ok" then
                                yield $"%s{describe}: measured %s{measured}, answered ok"
                        | Ok (EpollCtlAnswer.Failed reason, after) ->
                            if after <> system then
                                yield $"%s{describe}: a failure changed the system"

                            let answered = errnoName (EpollCtlError.toErrno reason)

                            if measured <> answered then
                                yield $"%s{describe}: measured %s{measured}, answered %s{answered}"
                        | Error refusal ->
                            refusals <- refusals + 1
                            let owed = owedRefusal row.Op (row.Target = "epoll") fd events

                            if measured <> "ok" || owed <> Some refusal then
                                yield
                                    $"%s{describe}: measured %s{measured}, refused %s{EpollCtlRefusal.describe refusal} (owed %A{owed})"
            ]

        mismatches |> List.truncate 20 |> shouldEqual []

        // Not vacuous: both kinds of success appear, as well as the failures.
        answeredCommits |> shouldBeGreaterThan 0
        refusals |> shouldBeGreaterThan 0

    // ------------------------------------------------------------------
    // Every mask: the EPOLLEXCLUSIVE screen, and where the refusals go
    // ------------------------------------------------------------------

    /// A mask drawn from the whole 32-bit space, with each half uniform.
    let private maskGen : Gen<uint32> =
        gen {
            let! high = Gen.choose (0, 0xFFFF)
            let! low = Gen.choose (0, 0xFFFF)
            return (uint32 high <<< 16) ||| uint32 low
        }

    /// The bits Linux permits beside `EPOLLEXCLUSIVE` on an ADD.
    let private exclusivePermitted : uint32 =
        EpollEvents.In
        ||| EpollEvents.Out
        ||| EpollEvents.Err
        ||| EpollEvents.Hup
        ||| EpollEvents.WakeUp
        ||| EpollEvents.EdgeTriggered
        ||| EpollEvents.Exclusive

    /// The probe's EXCL section: for 20000 random masks in each of six rows it
    /// found `epoll_ctl` answering exactly as `predicted` below, with no
    /// exception. Here the same six rows, over masks FsCheck draws from the
    /// whole 32-bit space; a predicted success is answered, or refused with the
    /// refusal the mask is owed.
    [<Test>]
    let ``every mask is screened for EPOLLEXCLUSIVE as measured, and refused only where it would commit`` () : unit =
        let rows : (string * int * bool * bool) list =
            [
                "ADD, unregistered socket", add, false, false
                "ADD, registered socket", add, true, false
                "MOD, registered socket", modify, true, false
                "MOD, unregistered socket", modify, false, false
                "op 0, registered socket", 0, true, false
                "ADD, unregistered epoll target", add, false, true
            ]

        let property (mask : uint32) : unit =
            for label, op, registered, epollTarget in rows do
                let portFd, system = withPort linux

                let targetFd, system = if epollTarget then withPort system else idleSocket system

                let system =
                    if registered then
                        applied portFd add targetFd (EpollEvents.In ||| edge) system
                    else
                        system

                let exclusive = mask &&& EpollEvents.Exclusive <> 0u

                let predicted =
                    if op = add then
                        if exclusive && (epollTarget || mask &&& ~~~exclusivePermitted <> 0u) then
                            "EINVAL"
                        elif registered then
                            "EEXIST"
                        else
                            "ok"
                    elif op = modify then
                        if exclusive then "EINVAL"
                        elif registered then "ok"
                        else "ENOENT"
                    else
                        "EINVAL"

                let answered =
                    match ctl portFd op targetFd mask system with
                    | Ok (EpollCtlAnswer.Changed, _) -> Ok "ok"
                    | Ok (EpollCtlAnswer.Failed reason, _) -> Ok (errnoName (EpollCtlError.toErrno reason))
                    | Error refusal -> Error refusal

                match answered with
                | Ok answer when answer = predicted -> ()
                | Error refusal when predicted = "ok" && owedRefusal op epollTarget targetFd mask = Some refusal -> ()
                | other -> failwith $"%s{label}, mask 0x%08x{mask}: predicted %s{predicted}, got %A{other}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen maskGen) property)

    // ------------------------------------------------------------------
    // What a registration reports
    // ------------------------------------------------------------------

    /// One descriptor onto each readiness state this library models for
    /// epoll, with the mask the probe's REPORT section measured for it: what
    /// `epoll_wait` reported to a registration of `0x0FFFFFFF`. For every one,
    /// every one of 21024 request masks reported exactly this mask restricted
    /// to the request plus `EPOLLERR` and `EPOLLHUP`.
    let private measuredLevels : (string * int * uint32) list * UnixSystem<int, string> =
        let connection = ConnectionId 7L
        let orphan = ConnectionId 8L
        let queued = ConnectionId 9L

        let peer : InternetEndpoint =
            {
                Address = 0x7F000001u
                Port = 5555us
            }

        let rows, system =
            [
                "stdin: pipe read end, writer closed", (fun system -> 0, system), 0x0010u
                "stdout: pipe write end, reader alive", (fun system -> 1, system), 0x0104u
                "stderr: pipe write end, reader alive", (fun system -> 2, system), 0x0104u
                "IPv4 TCP, idle", withSocket SocketDomain.InterNetwork SocketKind.Stream SocketPhase.Idle, 0x0114u
                "IPv6 TCP, idle", withSocket SocketDomain.InterNetworkV6 SocketKind.Stream SocketPhase.Idle, 0x0114u
                "IPv4 TCP, listening, queue empty",
                withSocket
                    SocketDomain.InterNetwork
                    SocketKind.Stream
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = []
                        }),
                0x0000u
                "IPv4 TCP, listening, queue nonempty",
                withSocket
                    SocketDomain.InterNetwork
                    SocketKind.Stream
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = [ queued ]
                        }),
                0x0041u
                "IPv4 TCP, established, peer alive",
                withSocket SocketDomain.InterNetwork SocketKind.Stream (SocketPhase.Established connection),
                0x0104u
                "IPv4 TCP, established pending report, peer alive",
                withSocket SocketDomain.InterNetwork SocketKind.Stream (SocketPhase.EstablishedPendingReport connection),
                0x0104u
                "IPv4 TCP, established, peer closed",
                withSocket SocketDomain.InterNetwork SocketKind.Stream (SocketPhase.Established orphan),
                0x2145u
                "IPv4 TCP, refused, pending delivery",
                withSocket SocketDomain.InterNetwork SocketKind.Stream SocketPhase.RefusedPendingDelivery,
                0x215du
                "IPv4 UDP, idle", withSocket SocketDomain.InterNetwork SocketKind.Datagram SocketPhase.Idle, 0x0304u
                "IPv6 UDP, idle", withSocket SocketDomain.InterNetworkV6 SocketKind.Datagram SocketPhase.Idle, 0x0304u
                "IPv4 UDP, peer set",
                withSocket SocketDomain.InterNetwork SocketKind.Datagram (SocketPhase.DatagramPeer peer),
                0x0304u
                "Unix stream, idle", withSocket SocketDomain.Unix SocketKind.Stream SocketPhase.Idle, 0x0314u
                "Unix datagram, idle", withSocket SocketDomain.Unix SocketKind.Datagram SocketPhase.Idle, 0x0304u
            ]
            |> List.fold
                (fun (rows, system) (name, add, level) ->
                    let fd, system = add system
                    (name, fd, level) :: rows, system
                )
                ([], linux)

        // The peer of the "peer alive" rows: a second end on the same
        // connection. Not itself a row, because it duplicates one.
        let _, system =
            withSocket SocketDomain.InterNetwork SocketKind.Stream (SocketPhase.Established connection) system

        List.rev rows, system

    /// ADD every measured state to one fresh port with `mask`, each with its
    /// row's index as `data`, and drain it: what one `epoll_wait` reports.
    /// Asserts the report is the measured level masked by the stored mask, in
    /// ADD order, since each ADD of a ready target appends it.
    let private reportsAsMeasured (mask : uint32) : unit =
        let rows, system = measuredLevels
        let portFd, system = withPort system
        let portId = idOf portFd system

        let system =
            rows
            |> List.indexed
            |> List.fold
                (fun system (index, (name, fd, _)) ->
                    match
                        UnixPoll.epollCtl portFd add fd (EpollEventArgument.Readable (mask, uint64 index)) system
                    with
                    | Ok (EpollCtlAnswer.Changed, system) -> system
                    | other -> failwith $"%s{name}: ADD of mask 0x%08x{mask} answered %A{other}"
                )
                system

        let delivered, _ = SocketEventPort.drain portId 64 system

        let expected =
            rows
            |> List.mapi (fun index (_, _, level) ->
                uint64 index, level &&& (mask ||| EpollEvents.Err ||| EpollEvents.Hup)
            )
            |> List.filter (fun (_, reported) -> reported <> 0u)

        if delivered <> expected then
            failwith $"mask 0x%08x{mask}: expected %A{expected}, reported %A{delivered}"

    /// The events a mask is registered with here: edge-triggered, and without
    /// the modes this library refuses, so that every mask is answered.
    let private answerable (mask : uint32) : uint32 =
        (mask ||| edge)
        &&& ~~~(EpollEvents.Exclusive ||| EpollEvents.OneShot ||| EpollEvents.WakeUp)

    /// Every single bit and every pair of bits, as the probe swept them.
    [<Test>]
    let ``every single bit and pair of bits reports the measured level masked by the registration`` () : unit =
        for i in 0..31 do
            for j in 0..31 do
                reportsAsMeasured (answerable ((1u <<< i) ||| (1u <<< j)))

        reportsAsMeasured (answerable 0u)

    /// Masks from the whole 32-bit space.
    [<Test>]
    let ``every mask reports the measured level masked by the registration`` () : unit =
        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll (Arb.fromGen maskGen) (answerable >> reportsAsMeasured)
        )

    /// What the kernel stores is the caller's events with `EPOLLERR` and
    /// `EPOLLHUP` added (measured through `/proc/self/fdinfo`), and MOD
    /// replaces it wholesale along with the data.
    [<Test>]
    let ``the stored mask is the caller's events with ERR and HUP added`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let key = socketFd, idOf socketFd system

        let system =
            match
                UnixPoll.epollCtl
                    portFd
                    add
                    socketFd
                    (EpollEventArgument.Readable (EpollEvents.Pri ||| 0x800u ||| edge, 7UL))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> system
            | other -> failwith $"%A{other}"

        let stored = (portOf portFd system).Registrations.[key]

        stored.Events
        |> shouldEqual (EpollEvents.Pri ||| 0x800u ||| edge ||| EpollEvents.Err ||| EpollEvents.Hup)

        stored.Data |> shouldEqual 7UL

        let system =
            match
                UnixPoll.epollCtl
                    portFd
                    modify
                    socketFd
                    (EpollEventArgument.Readable (EpollEvents.In ||| edge, 9UL))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, system) -> system
            | other -> failwith $"%A{other}"

        let stored = (portOf portFd system).Registrations.[key]

        stored.Events
        |> shouldEqual (EpollEvents.In ||| edge ||| EpollEvents.Err ||| EpollEvents.Hup)

        stored.Data |> shouldEqual 9UL

    // ------------------------------------------------------------------
    // The event argument
    // ------------------------------------------------------------------

    /// DEL never reads its event, so an unreadable one is answered as if it
    /// were readable; every other operation value reads it first of all.
    [<Test>]
    let ``an unreadable event is EFAULT for everything but DEL, and DEL never reads it`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let registered = applied portFd add socketFd (EpollEvents.In ||| edge) system

        for op in [ add ; modify ; 0 ; 4 ; -1 ; Int32.MaxValue ; Int32.MinValue ] do
            // Ahead of even a descriptor that is not open.
            UnixPoll.epollCtl 99 op 98 EpollEventArgument.Unreadable registered
            |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.EventUnreadable, registered))

        UnixPoll.epollCtl 99 del 98 EpollEventArgument.Unreadable registered
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.BadPortFd, registered))

        match UnixPoll.epollCtl portFd del socketFd EpollEventArgument.Unreadable registered with
        | Ok (EpollCtlAnswer.Changed, after) -> (portOf portFd after).Registrations |> shouldEqual Map.empty
        | other -> failwith $"%A{other}"

    // ------------------------------------------------------------------
    // The refusals
    // ------------------------------------------------------------------

    /// Ahead of everything, including the copy-in and the descriptor lookups:
    /// kqueue's model is structurally different rather than differently
    /// numbered, so there is no row of it to answer even for inputs epoll would
    /// refuse.
    [<Test>]
    let ``a Darwin-flavoured kernel refuses every call`` () : unit =
        let darwin = systemOn SimulatedUnixPlatform.macOsArm64
        let portFd, darwin = withPort darwin
        let socketFd, darwin = idleSocket darwin

        let expected = Error (EpollCtlRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)

        for op in [ add ; del ; modify ; 0 ] do
            for argument in
                [
                    EpollEventArgument.Readable (EpollEvents.In ||| edge, 1UL)
                    EpollEventArgument.Unreadable
                ] do
                UnixPoll.epollCtl portFd op socketFd argument darwin |> shouldEqual expected
                UnixPoll.epollCtl 99 op 99 argument darwin |> shouldEqual expected

    /// A level-triggered request is refused only where it would commit: a
    /// failure that precedes the commit is still the kernel's answer.
    [<Test>]
    let ``a level-triggered request is refused at the commit, and answered where it would fail`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system

        ctl portFd add socketFd EpollEvents.In system
        |> shouldEqual (Error EpollCtlRefusal.LevelTriggered)

        ctl portFd modify socketFd EpollEvents.In system
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.NotRegistered, system))

        let registered = applied portFd add socketFd (EpollEvents.In ||| edge) system

        ctl portFd add socketFd EpollEvents.In registered
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.AlreadyRegistered, registered))

        ctl portFd modify socketFd EpollEvents.In registered
        |> shouldEqual (Error EpollCtlRefusal.LevelTriggered)

    /// Linux registers one epoll instance in another; this library refuses to,
    /// at the point in the ladder where Linux runs its loop check, which is
    /// behind the EPOLLEXCLUSIVE screen. MOD and DEL of a port target answer
    /// ENOENT, as they do on a table that holds no such registration.
    [<Test>]
    let ``an ADD of another port is refused, and MOD and DEL of one are answered`` () : unit =
        let portFd, system = withPort linux
        let innerFd, system = withPort system

        ctl portFd add innerFd (EpollEvents.In ||| edge) system
        |> shouldEqual (Error (EpollCtlRefusal.NestedPort innerFd))

        ctl portFd add innerFd (EpollEvents.In ||| EpollEvents.Exclusive ||| edge) system
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.ExclusiveNotPermitted, system))

        ctl portFd modify innerFd (EpollEvents.In ||| edge) system
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.NotRegistered, system))

        ctl portFd del innerFd 0u system
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.NotRegistered, system))

    // ------------------------------------------------------------------
    // The key
    // ------------------------------------------------------------------

    /// The registration key is the (fd, description) pair, exactly as epoll
    /// keys it: a `dup` of the target admits a second registration, and a
    /// `dup` of the port operates on the one shared table.
    [<Test>]
    let ``dup of the target is a second key; dup of the port is the same table`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let copyFd, system = dupOf socketFd system

        let system =
            system
            |> applied portFd add socketFd (EpollEvents.In ||| edge)
            |> applied portFd add copyFd (EpollEvents.In ||| edge)

        (portOf portFd system).Registrations.Count |> shouldEqual 2

        let portCopyFd, system = dupOf portFd system

        ctl portCopyFd add socketFd (EpollEvents.In ||| edge) system
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.AlreadyRegistered, system))

        let system = applied portCopyFd del socketFd 0u system
        (portOf portFd system).Registrations.Count |> shouldEqual 1

    // ------------------------------------------------------------------
    // The ordinal
    // ------------------------------------------------------------------

    /// Only an ADD mints a registration, so only an ADD consumes an ordinal.
    /// A MOD rebuilds an existing one and a DEL destroys it, and neither may
    /// shift the numbering a later ADD will get.
    [<Test>]
    let ``only an ADD consumes an ordinal`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 0L

        let system = applied portFd add socketFd (EpollEvents.In ||| edge) system
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 1L

        let system = applied portFd modify socketFd (EpollEvents.Out ||| edge) system
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 1L

        let system = applied portFd del socketFd 0u system
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 1L

        let system = applied portFd add socketFd (EpollEvents.In ||| edge) system
        system.Machine.NextSocketEventRegistrationOrdinal |> shouldEqual 2L

    /// A failure or a refusal consumes nothing: the ordinal is taken by the
    /// commit, which did not happen.
    [<Test>]
    let ``a failed or refused ADD changes nothing`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let system = applied portFd add socketFd (EpollEvents.In ||| edge) system

        ctl portFd add socketFd (EpollEvents.In ||| edge) system
        |> shouldEqual (Ok (EpollCtlAnswer.Failed EpollCtlError.AlreadyRegistered, system))

        let otherFd, system = idleSocket system

        ctl portFd add otherFd (EpollEvents.In ||| EpollEvents.OneShot ||| edge) system
        |> shouldEqual (Error EpollCtlRefusal.OneShot)

    // ------------------------------------------------------------------
    // Pending at registration time
    // ------------------------------------------------------------------

    /// An ADD whose target is already ready under the new mask becomes pending
    /// at that moment, rather than waiting for something to happen to the
    /// target.
    [<Test>]
    let ``an ADD of an already-ready target is pending at once`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        ready portFd system |> shouldEqual []

        let system =
            applied portFd add socketFd (EpollEvents.In ||| EpollEvents.Out ||| edge) system

        ready portFd system |> shouldEqual [ socketFd, idOf socketFd system ]

    /// `HUP` is reported whether it was asked for or not, so an idle stream
    /// socket is ready under a read-only mask too.
    [<Test>]
    let ``a mask that misses OUT still sees the unrequested HUP`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let system = applied portFd add socketFd (EpollEvents.In ||| edge) system
        ready portFd system |> List.length |> shouldEqual 1

    /// A MOD of an entry already pending leaves its place alone rather than
    /// appending it a second time.
    [<Test>]
    let ``a MOD of an already-pending entry does not re-append it`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let system = applied portFd add socketFd (EpollEvents.Out ||| edge) system
        let afterAdd = ready portFd system

        let system =
            applied portFd modify socketFd (EpollEvents.In ||| EpollEvents.Out ||| edge) system

        ready portFd system |> shouldEqual afterAdd

    /// A DEL takes the pending entry with it.
    [<Test>]
    let ``a DEL removes the pending entry`` () : unit =
        let portFd, system = withPort linux
        let socketFd, system = idleSocket system
        let system = applied portFd add socketFd (EpollEvents.Out ||| edge) system
        let system = applied portFd del socketFd 0u system
        ready portFd system |> shouldEqual []

    /// A target that is not ready under the mask does not become pending:
    /// registering it is not itself an event. Standard output presents
    /// `OUT|WRNORM` alone, so a mask of `IN|RDNORM|PRI` reports nothing, and
    /// a mask of `WRNORM` alone does report.
    [<Test>]
    let ``an ADD pends exactly when the mask meets the target's readiness`` () : unit =
        let portFd, system = withPort linux

        let quiet =
            applied portFd add 1 (EpollEvents.In ||| EpollEvents.RdNorm ||| EpollEvents.Pri ||| edge) system

        ready portFd quiet |> shouldEqual []

        let loud = applied portFd add 1 (EpollEvents.WrNorm ||| edge) system
        ready portFd loud |> shouldEqual [ 1, idOf 1 system ]
