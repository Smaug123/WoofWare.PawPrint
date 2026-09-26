namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `SocketEventsPal` transcribes four upstream functions, so nothing in the
/// type system keeps its numbers right. Its oracle is upstream rather than the
/// library: the five `SocketEvents` values are re-derived here from the pinned
/// `pal_networking.h`, and each conversion's rows from `pal_networking.c`. The
/// library has no opinion about them at all -- it holds epoll's own bits, and
/// never sees .NET's encoding of them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventsPal =

    let private runtimeSrc : string option =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" -> None
        | dir -> Some dir

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails.
    let private requireRuntimeSrc () : string =
        match runtimeSrc with
        | Some dir -> dir
        | None ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"

    let private palPath (leaf : string) : string =
        let path =
            Path.Combine (requireRuntimeSrc (), "src", "native", "libs", "System.Native", leaf)

        if not (File.Exists path) then
            failwith
                $"TestSocketEventsPal: expected the pinned PAL networking source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/System.Native, this transcription has lost its oracle."

        path

    /// `SocketEvents_SA_READ = 0x01,` and friends.
    let private palEntry : Regex =
        Regex (@"^\s+SocketEvents_(?<name>SA_[A-Z]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),", RegexOptions.Multiline)

    let private pinnedSocketEvents () : Map<string, int> =
        let text = File.ReadAllText (palPath "pal_networking.h")

        let values =
            palEntry.Matches text
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        // `SA_NONE` is in the enum too, so six rather than five. Its absence
        // would mean the regex had drifted rather than that upstream had.
        if values.Count <> 6 then
            failwith
                $"TestSocketEventsPal: read %d{values.Count} SocketEvents values from the pinned pal_networking.h, expected 6 (SA_NONE and the five conditions). The enum's shape has changed; teach this test to read it."

        values

    let private pinned (name : string) : int =
        match Map.tryFind name (pinnedSocketEvents ()) with
        | Some value -> value
        | None ->
            failwith
                $"TestSocketEventsPal: the pinned pal_networking.h has no SocketEvents_%s{name}. The enum has been renamed or reordered upstream."

    // ---------------------------------------------------------------------
    // The alphabet itself.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``the five condition bits are upstream's`` () : unit =
        pinned "SA_NONE" |> shouldEqual 0
        pinned "SA_READ" |> shouldEqual 0x01
        pinned "SA_WRITE" |> shouldEqual 0x02
        pinned "SA_READCLOSE" |> shouldEqual 0x04
        pinned "SA_CLOSE" |> shouldEqual 0x08
        pinned "SA_ERROR" |> shouldEqual 0x10

    /// The wrapper's screen is `SupportedEvents`, which upstream spells as the
    /// OR of exactly these five. Checked as that OR rather than as `0x1F`, so
    /// that a `supported` narrowed to some other constant cannot agree with a
    /// literal copied out of it.
    [<Test>]
    let ``supported is upstream's SupportedEvents`` () : unit =
        let expected =
            pinned "SA_READ"
            ||| pinned "SA_WRITE"
            ||| pinned "SA_READCLOSE"
            ||| pinned "SA_CLOSE"
            ||| pinned "SA_ERROR"

        SocketEventsPal.supported |> shouldEqual expected

        // And that upstream really names those five in the screen, rather than
        // some subset that happens to OR to the same number today.
        let source = File.ReadAllText (palPath "pal_networking.c")

        let declaration =
            Regex.Match (source, @"const int32_t SupportedEvents = (?<rhs>[^;]+);")

        if not declaration.Success then
            failwith
                "TestSocketEventsPal: the pinned pal_networking.c no longer declares `const int32_t SupportedEvents`, so the screen has lost its oracle."

        let named =
            Regex.Matches (declaration.Groups.["rhs"].Value, @"SocketEvents_(SA_[A-Z]+)")
            |> Seq.map (fun m -> m.Groups.[1].Value)
            |> Set.ofSeq

        named
        |> shouldEqual (Set.ofList [ "SA_READ" ; "SA_WRITE" ; "SA_READCLOSE" ; "SA_CLOSE" ; "SA_ERROR" ])

    // ---------------------------------------------------------------------
    // Which condition maps to which, read out of upstream's own function
    // bodies. The enum *values* above are only half an oracle: a runtime pin
    // that re-paired the rows without renumbering them would leave a test that
    // checked numbers alone entirely green.
    // ---------------------------------------------------------------------

    /// The body of a `static` function in a C file, from its signature to the
    /// closing brace in column 0.
    let private functionBody (source : string) (signature : string) : string =
        match source.IndexOf (signature, StringComparison.Ordinal) with
        | -1 ->
            failwith
                $"TestSocketEventsPal: the pinned pal_networking.c no longer declares `%s{signature}`. The conversion this transcribes has been renamed or resignatured upstream."
        | start ->

        let body = source.Substring start

        match body.IndexOf ("\n}", StringComparison.Ordinal) with
        | -1 -> failwith $"TestSocketEventsPal: `%s{signature}` has no closing brace in column 0."
        | finish -> body.Substring (0, finish)

    /// `((events & EPOLLIN) != 0) ? SocketEvents_SA_READ : 0` and friends: one
    /// row of a conversion, in whichever direction the function runs.
    let private conversionRow : Regex =
        Regex (@"\(\(events\s*&\s*(?<from>\w+)\)\s*!=\s*0\)\s*\?\s*(?<to>\w+)\s*:\s*0")

    let private conversionRows (signature : string) : Map<string, string> =
        let body = functionBody (File.ReadAllText (palPath "pal_networking.c")) signature

        // The `SocketEvents_` prefix is on whichever side of the row is the
        // PAL's, which is the `from` in one direction and the `to` in the
        // other; the names this answers with are bare either way.
        let bare (name : string) : string = name.Replace ("SocketEvents_", "")

        let rows =
            conversionRow.Matches body
            |> Seq.map (fun m -> bare m.Groups.["from"].Value, bare m.Groups.["to"].Value)
            |> Map.ofSeq

        if rows.Count <> 5 then
            failwith
                $"TestSocketEventsPal: read %d{rows.Count} conversion rows from `%s{signature}`, expected 5. The function's shape has changed; teach this test to read it."

        rows

    /// Each epoll bit upstream's conversions name, as Linux's `<sys/epoll.h>`
    /// numbers it: measured 2026-09-26 on Linux 6.18.5 by
    /// `docs/plans/2026-08-23-posix-kernel-extraction/epoll-ctl.c`, which
    /// printed the header's values. Literals rather than `EpollEvents`, which
    /// is the library's own transcription of the same header.
    let private epollBits : Map<string, uint32> =
        Map.ofList
            [
                "EPOLLIN", 0x0001u
                "EPOLLOUT", 0x0004u
                "EPOLLERR", 0x0008u
                "EPOLLHUP", 0x0010u
                "EPOLLRDHUP", 0x2000u
                "EPOLLET", 0x80000000u
            ]

    let private epollBit (name : string) : uint32 =
        match Map.tryFind name epollBits with
        | Some bit -> bit
        | None ->
            failwith $"TestSocketEventsPal: upstream names %s{name}, which is not one of the epoll bits measured above."

    [<Test>]
    let ``the library numbers the epoll bits as the header does`` () : unit =
        EpollEvents.In |> shouldEqual (epollBit "EPOLLIN")
        EpollEvents.Out |> shouldEqual (epollBit "EPOLLOUT")
        EpollEvents.Err |> shouldEqual (epollBit "EPOLLERR")
        EpollEvents.Hup |> shouldEqual (epollBit "EPOLLHUP")
        EpollEvents.RdHup |> shouldEqual (epollBit "EPOLLRDHUP")
        EpollEvents.EdgeTriggered |> shouldEqual (epollBit "EPOLLET")

    /// `GetSocketEvents`' rows: each epoll bit and the `SA_*` it becomes.
    let private getSocketEventsRows () : (uint32 * int) list =
        conversionRows "static int GetSocketEvents(uint32_t events)"
        |> Map.toList
        |> List.map (fun (epoll, sa) -> epollBit epoll, pinned sa)

    /// `GetEPollEvents`' rows: each `SA_*` bit and the epoll bit it becomes.
    let private getEPollEventsRows () : (int * uint32) list =
        conversionRows "static uint32_t GetEPollEvents(SocketEvents events)"
        |> Map.toList
        |> List.map (fun (sa, epoll) -> pinned sa, epollBit epoll)

    /// Masks from the whole 32-bit space, each half uniform.
    let private epollMaskGen : Gen<uint32> =
        gen {
            let! high = Gen.choose (0, 0xFFFF)
            let! low = Gen.choose (0, 0xFFFF)
            return (uint32 high <<< 16) ||| uint32 low
        }

    /// Upstream's `GetSocketEvents` is the union of its rows, and every other
    /// epoll bit is dropped: over every combination of the five it reads, and
    /// over masks from the whole 32-bit space.
    [<Test>]
    let ``ofEpollEvents is upstream's GetSocketEvents`` () : unit =
        let rows = getSocketEventsRows ()

        let expected (events : uint32) : int =
            rows
            |> List.fold (fun acc (epoll, sa) -> if events &&& epoll <> 0u then acc ||| sa else acc) 0

        let named = rows |> List.fold (fun acc (epoll, _) -> acc ||| epoll) 0u

        for subset in 0..31 do
            let events =
                rows
                |> List.indexed
                |> List.fold (fun acc (i, (epoll, _)) -> if subset &&& (1 <<< i) <> 0 then acc ||| epoll else acc) 0u

            SocketEventsPal.ofEpollEvents events |> shouldEqual (expected events)
            events &&& ~~~named |> shouldEqual 0u

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll
                (Arb.fromGen epollMaskGen)
                (fun events -> SocketEventsPal.ofEpollEvents events = expected events)
        )

    /// Upstream's `GetEPollEvents` is the union of its rows, over every mask
    /// the wrapper's screen admits.
    [<Test>]
    let ``toEpollEvents is upstream's GetEPollEvents`` () : unit =
        let rows = getEPollEventsRows ()
        rows |> List.length |> shouldEqual 5

        for bits in 0 .. SocketEventsPal.supported do
            let expected =
                rows
                |> List.fold (fun acc (sa, epoll) -> if bits &&& sa <> 0 then acc ||| epoll else acc) 0u

            SocketEventsPal.toEpollEvents bits |> shouldEqual expected

    // ---------------------------------------------------------------------
    // `ConvertEventEPollToSocketAsync`, which folds before converting.
    // ---------------------------------------------------------------------

    /// Upstream's fold is one statement, and this reads which bit it clears
    /// and which it sets rather than assuming. A pin that folded, say, `ERR`
    /// instead, or that stopped setting `OUT`, changes this text.
    [<Test>]
    let ``the delivery fold is upstream's`` () : unit =
        let body =
            functionBody
                (File.ReadAllText (palPath "pal_networking.c"))
                "static void ConvertEventEPollToSocketAsync(SocketEvent* sae, struct epoll_event* epoll)"

        let fold =
            Regex.Match (
                body,
                @"if\s*\(\(events\s*&\s*(?<tested>\w+)\)\s*!=\s*0\)\s*\{\s*events\s*=\s*\(events\s*&\s*\(\(uint32_t\)~(?<cleared>\w+)\)\)(?<set>(\s*\|\s*\w+)+);"
            )

        if not fold.Success then
            failwith
                $"TestSocketEventsPal: could not read the delivery fold out of ConvertEventEPollToSocketAsync. Its shape has changed upstream; read the body and teach this test, because `SocketEventsPal.delivered` transcribes exactly this statement.\n%s{body}"

        fold.Groups.["tested"].Value |> shouldEqual "EPOLLHUP"
        fold.Groups.["cleared"].Value |> shouldEqual "EPOLLHUP"

        Regex.Matches (fold.Groups.["set"].Value, @"\w+")
        |> Seq.map (fun m -> m.Value)
        |> Set.ofSeq
        |> shouldEqual (Set.ofList [ "EPOLLIN" ; "EPOLLOUT" ])

    [<Test>]
    let ``delivery folds HUP into READ and WRITE`` () : unit =
        let hup = epollBit "EPOLLHUP"

        let property (events : uint32) : bool =
            let folded =
                if events &&& hup <> 0u then
                    (events &&& ~~~hup) ||| epollBit "EPOLLIN" ||| epollBit "EPOLLOUT"
                else
                    events

            SocketEventsPal.delivered events = SocketEventsPal.ofEpollEvents folded

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen epollMaskGen) property)

    /// The consequence of that fold, and the reason a guest never sees
    /// `SA_CLOSE` on Linux however the socket is registered.
    [<Test>]
    let ``no event delivers SA_CLOSE`` () : unit =
        let saClose = pinned "SA_CLOSE"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll (Arb.fromGen epollMaskGen) (fun events -> SocketEventsPal.delivered events &&& saClose = 0)
        )

    /// An idle stream socket's report, which is the row that makes the fold
    /// visible rather than merely stated: `OUT|HUP` is not `SA_WRITE`.
    [<Test>]
    let ``an idle socket's OUT and HUP deliver as READ and WRITE`` () : unit =
        epollBit "EPOLLOUT" ||| epollBit "EPOLLHUP"
        |> SocketEventsPal.delivered
        |> shouldEqual (pinned "SA_READ" ||| pinned "SA_WRITE")

    // ---------------------------------------------------------------------
    // `TryChangeSocketEventRegistrationInner`: the op, and EPOLLET.
    // ---------------------------------------------------------------------

    let private changeInnerBody () : string =
        let source = File.ReadAllText (palPath "pal_networking.c")

        // Two definitions exist, one per backend; the epoll one is the first,
        // under `#if HAVE_EPOLL`.
        let epollSection =
            match source.IndexOf ("#if HAVE_EPOLL", StringComparison.Ordinal) with
            | -1 -> failwith "TestSocketEventsPal: the pinned pal_networking.c has no `#if HAVE_EPOLL` section."
            | start -> source.Substring start

        functionBody epollSection "static int32_t TryChangeSocketEventRegistrationInner("

    /// Upstream's derivation reads: MOD by default, ADD when the claimed current
    /// set is `SA_NONE`, else DEL when the new one is. Read out of the body, so
    /// a pin that reordered the precedence changes this text.
    [<Test>]
    let ``the operation is derived as upstream derives it`` () : unit =
        let body = changeInnerBody ()

        let derivation =
            Regex.Match (
                body,
                @"int op = (?<default>EPOLL_CTL_\w+);\s*if \(currentEvents == SocketEvents_SA_NONE\)\s*\{\s*op = (?<first>EPOLL_CTL_\w+);\s*\}\s*else if \(newEvents == SocketEvents_SA_NONE\)\s*\{\s*op = (?<second>EPOLL_CTL_\w+);"
            )

        if not derivation.Success then
            failwith
                $"TestSocketEventsPal: could not read the op derivation out of TryChangeSocketEventRegistrationInner. Read the body and teach this test.\n%s{body}"

        derivation.Groups.["default"].Value |> shouldEqual "EPOLL_CTL_MOD"
        derivation.Groups.["first"].Value |> shouldEqual "EPOLL_CTL_ADD"
        derivation.Groups.["second"].Value |> shouldEqual "EPOLL_CTL_DEL"

        // Linux's `EPOLL_CTL_ADD`, `_DEL` and `_MOD`.
        for current in 0 .. SocketEventsPal.supported do
            for next in 0 .. SocketEventsPal.supported do
                let expected =
                    if current = 0 then 1
                    elif next = 0 then 2
                    else 3

                SocketEventsPal.epollCtlOperation current next |> shouldEqual expected

    /// Every registration upstream makes is edge-triggered: the new mask's
    /// bits with `EPOLLET` ORed in.
    [<Test>]
    let ``every registration asks for EPOLLET`` () : unit =
        let body = changeInnerBody ()

        Regex.IsMatch (body, @"evt\.events = GetEPollEvents\(newEvents\) \| \(unsigned int\)EPOLLET;")
        |> shouldEqual true

    // ---------------------------------------------------------------------
    // The composition `SystemNative_TryChangeSocketEventRegistration` and
    // `SystemNative_WaitForSocketEvents` answer with, against what they
    // answered when the library stored the shim's three-bit interest.
    // ---------------------------------------------------------------------

    /// One call a guest can make through the two entry points, past the
    /// wrapper's screens: masks within `SocketEventsPal.supported`, and never
    /// equal (the wrapper answers equal masks itself).
    type private ShimCall =
        | Change of port : int * target : int * current : int * next : int * data : uint64
        | Wait of port : int * maxEvents : int

    /// What one registration held, in the shim's terms: the three conditions
    /// `epoll_ctl` keeps of a `SocketEvents` mask, and the data.
    type private OldRegistration =
        {
            Interest : int
            Data : uint64
        }

    /// The shim-shaped model the library used to be, stated independently of
    /// it: a table per port description keyed on (fd, description), and a
    /// ready list, with the ladder and the reporting rule written out in the
    /// shim's own terms. Socket phases are fixed for the whole sequence, so
    /// the only producer is registration itself.
    type private OldModel =
        {
            Tables : Map<OpenFileDescriptionId, Map<int * OpenFileDescriptionId, OldRegistration>>
            Ready : Map<OpenFileDescriptionId, (int * OpenFileDescriptionId) list>
        }

    let private linuxSystem : UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }

    let private withRegistry (registry : FileDescriptorRegistry) (system : UnixSystem<int, string>) =
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private addSocket
        (domain : SocketDomain)
        (kind : SocketKind)
        (phase : SocketPhase)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let socketId = system.Machine.NextSocketId
        let (SocketId raw) = socketId

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        fd,
        { withRegistry registry system with
            Machine =
                { system.Machine with
                    Sockets =
                        Map.add
                            socketId
                            {
                                Domain = domain
                                Kind = kind
                                Protocol =
                                    match domain, kind with
                                    | SocketDomain.Unix, _ -> SocketProtocol.Default
                                    | _, SocketKind.Stream -> SocketProtocol.Tcp
                                    | _, _ -> SocketProtocol.Udp
                                Binding = None
                                ReuseAddress = false
                                Phase = phase
                            }
                            system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
        }

    /// Two ports (one with a `dup`), sockets in every phase whose level the
    /// shim can see, the standard streams, a regular file, and a descriptor
    /// that is not open: the pool a sequence draws its fds from.
    let private pool : int list * UnixSystem<int, string> =
        let connection = ConnectionId 7L
        let system = linuxSystem

        let portA, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        let portB, registry = FileDescriptorRegistry.createSocketEventPort registry

        let portCopy, registry =
            match FileDescriptorRegistry.dup portA registry with
            | Ok result -> result
            | Error error -> failwith $"dup: %O{error}"

        let file, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadWrite registry

        let system = withRegistry registry system

        let adders =
            [
                addSocket SocketDomain.Inet SocketKind.Stream SocketPhase.Idle
                addSocket
                    SocketDomain.Inet
                    SocketKind.Stream
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = []
                        })
                addSocket
                    SocketDomain.Inet
                    SocketKind.Stream
                    (SocketPhase.Listening
                        {
                            Backlog = 8
                            Queue = [ ConnectionId 9L ]
                        })
                addSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established connection)
                addSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established connection)
                addSocket SocketDomain.Inet SocketKind.Stream (SocketPhase.Established (ConnectionId 8L))
                addSocket SocketDomain.Inet SocketKind.Stream SocketPhase.RefusedPendingDelivery
                addSocket SocketDomain.Inet SocketKind.Datagram SocketPhase.Idle
                addSocket SocketDomain.Unix SocketKind.Stream SocketPhase.Idle
            ]

        let socketFds, system =
            adders
            |> List.fold
                (fun (fds, system) add ->
                    let fd, system = add system
                    fd :: fds, system
                )
                ([], system)

        let firstSocket = List.last socketFds

        let socketCopy, registry =
            match FileDescriptorRegistry.dup firstSocket system.Process.FileDescriptors with
            | Ok result -> result
            | Error error -> failwith $"dup: %O{error}"

        let system = withRegistry registry system

        [ portA ; portB ; portCopy ; file ; 0 ; 1 ; 2 ; 60 ; socketCopy ]
        @ List.rev socketFds,
        system

    /// The epoll level of a target, in the five conditions the shim could see.
    let private oldLevel (targetId : OpenFileDescriptionId) (system : UnixSystem<int, string>) : ReadinessLevel =
        match Map.tryFind targetId (FileDescriptorRegistry.descriptions system.Process.FileDescriptors) with
        | None -> failwith $"oldLevel: %O{targetId} is not live"
        | Some description ->

        match description.Target with
        | OpenFileTarget.Socket socketId -> UnixMachineState.socketReadinessLevel socketId system.Machine
        | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput ->
            { ReadinessLevel.none with
                Hup = true
            }
        | OpenFileTarget.StandardStream _ ->
            { ReadinessLevel.none with
                Out = true
            }
        | other -> failwith $"oldLevel: %O{other} cannot be registered"

    /// What the old shim-shaped model reported for a registration with
    /// `interest` (the `SocketEvents` bits READ, WRITE and READCLOSE) on a
    /// target at `level`, as the `SocketEvents` a guest reads: `IN`, `OUT` and
    /// `RDHUP` when asked for, `HUP` and `ERR` always, and then the shim's fold
    /// of `HUP` into `READ|WRITE`. Zero exactly when nothing was reported.
    let private oldReport (interest : int) (level : ReadinessLevel) : int =
        let inBit = level.In && interest &&& 0x01 <> 0
        let outBit = level.Out && interest &&& 0x02 <> 0
        let rdHup = level.RdHup && interest &&& 0x04 <> 0
        let inBit, outBit = if level.Hup then true, true else inBit, outBit

        (if inBit then 0x01 else 0)
        ||| (if outBit then 0x02 else 0)
        ||| (if rdHup then 0x04 else 0)
        ||| (if level.Err then 0x10 else 0)

    /// One call against the old model: the transcript token, and the model
    /// after.
    let private oldStep (system : UnixSystem<int, string>) (model : OldModel) (call : ShimCall) : string * OldModel =
        let registry = system.Process.FileDescriptors

        match call with
        | Change (port, target, current, next, data) ->
            match FileDescriptorRegistry.tryFindWithId port registry with
            | None -> "EBADF", model
            | Some (portId, portDescription) ->

            match FileDescriptorRegistry.tryFindWithId target registry with
            | None -> "EBADF", model
            | Some (targetId, targetDescription) ->

            match targetDescription.Target with
            | OpenFileTarget.File _ -> "EPERM", model
            | _ ->

            match portDescription.Target with
            | OpenFileTarget.SocketEventPort _ when portId <> targetId ->
                let isAdd = current = 0
                let isDel = not isAdd && next = 0

                match targetDescription.Target with
                | OpenFileTarget.SocketEventPort _ when isAdd -> "refused", model
                | _ ->

                let table = Map.tryFind portId model.Tables |> Option.defaultValue Map.empty
                let ready = Map.tryFind portId model.Ready |> Option.defaultValue []
                let key = target, targetId
                let registered = Map.containsKey key table

                let commit (table : Map<_, OldRegistration>) (ready : (int * OpenFileDescriptionId) list) =
                    "ok",
                    { model with
                        Tables = Map.add portId table model.Tables
                        Ready = Map.add portId ready model.Ready
                    }

                let pend (interest : int) (ready : (int * OpenFileDescriptionId) list) =
                    if
                        not (List.contains key ready)
                        && oldReport interest (oldLevel targetId system) <> 0
                    then
                        ready @ [ key ]
                    else
                        ready

                let registration =
                    {
                        Interest = next &&& 0x07
                        Data = data
                    }

                if isAdd then
                    if registered then
                        "EEXIST", model
                    else
                        commit (Map.add key registration table) (pend registration.Interest ready)
                elif isDel then
                    if registered then
                        commit (Map.remove key table) (List.filter (fun k -> k <> key) ready)
                    else
                        "ENOENT", model
                elif registered then
                    commit (Map.add key registration table) (pend registration.Interest ready)
                else
                    "ENOENT", model
            | _ -> "EINVAL", model
        | Wait (port, maxEvents) ->
            let portId =
                match FileDescriptorRegistry.tryFindId port registry with
                | Some id -> id
                | None -> failwith "oldStep: waits are drawn only on live ports"

            let table = Map.tryFind portId model.Tables |> Option.defaultValue Map.empty
            let ready = Map.tryFind portId model.Ready |> Option.defaultValue []

            let rec walk delivered remaining =
                match remaining with
                | [] -> List.rev delivered, []
                | _ when List.length delivered = maxEvents -> List.rev delivered, remaining
                | (_, targetId as key) :: rest ->
                    let registration = table.[key]
                    let reported = oldReport registration.Interest (oldLevel targetId system)

                    if reported = 0 then
                        walk delivered rest
                    else
                        walk ((registration.Data, reported) :: delivered) rest

            let delivered, surviving = walk [] ready

            $"%A{delivered}",
            { model with
                Ready = Map.add portId surviving model.Ready
            }

    /// One call against the new composition: the same transcript token, and
    /// the system after.
    let private newStep (system : UnixSystem<int, string>) (call : ShimCall) : string * UnixSystem<int, string> =
        match call with
        | Change (port, target, current, next, data) ->
            match SocketEventsPal.tryChangeSocketEventRegistration port target current next data system with
            | Ok (EpollCtlAnswer.Changed, system) -> "ok", system
            | Ok (EpollCtlAnswer.Failed reason, after) ->
                if after <> system then
                    failwith $"a failed change moved the system: %A{call}"

                $"%A{EpollCtlError.toErrno reason}", system
            | Error (EpollCtlRefusal.NestedPort _) -> "refused", system
            | Error refusal -> failwith $"unexpected refusal of %A{call}: %s{EpollCtlRefusal.describe refusal}"
        | Wait (port, maxEvents) ->
            let portId =
                match FileDescriptorRegistry.tryFindId port system.Process.FileDescriptors with
                | Some id -> id
                | None -> failwith "newStep: waits are drawn only on live ports"

            let delivered, system = SocketEventPort.drain portId maxEvents system

            let delivered =
                delivered
                |> List.map (fun (data, events) -> data, SocketEventsPal.delivered events)

            $"%A{delivered}", system

    let private shimCallGen (fds : int list) : Gen<ShimCall> =
        let fdGen = Gen.elements fds

        let change =
            gen {
                let! port = fdGen
                let! target = fdGen
                let! current = Gen.choose (0, 0x1F)
                let! next = Gen.choose (0, 0x1F) |> Gen.filter (fun next -> next <> current)
                let! data = Gen.choose (0, 1000)
                return Change (port, target, current, next, uint64 data)
            }

        let wait =
            gen {
                let! port = Gen.elements (List.take 3 fds)
                let! maxEvents = Gen.elements [ 1 ; 2 ; 8 ]
                return Wait (port, maxEvents)
            }

        Gen.frequency [ 4, change ; 1, wait ]

    /// Random sequences of registration changes and waits, over two ports and
    /// every target kind the shim can name, answered by the composition exactly
    /// as the shim-shaped model answered them: the same errno or success for
    /// every change, and the same `SocketEvent`s, in the same order, for every
    /// wait.
    [<Test>]
    let ``the composition answers every call as the shim-shaped model did`` () : unit =
        let fds, system = pool

        let property (calls : ShimCall list) : unit =
            calls
            |> List.fold
                (fun (system, model) call ->
                    let expected, model = oldStep system model call
                    let answered, system = newStep system call

                    if answered <> expected then
                        failwith $"%A{call}: the shim-shaped model answered %s{expected}, the composition %s{answered}"

                    system, model
                )
                (system,
                 {
                     Tables = Map.empty
                     Ready = Map.empty
                 })
            |> ignore

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGen (Gen.listOf (shimCallGen fds))) property
        )
