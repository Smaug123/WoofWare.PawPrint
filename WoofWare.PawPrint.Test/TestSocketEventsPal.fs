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
    // `TryChangeSocketEventRegistrationInner`, the kqueue build's: the
    // changelist.
    // ---------------------------------------------------------------------

    let private keventChangeInnerBody () : string =
        let source = File.ReadAllText (palPath "pal_networking.c")

        // The kqueue backend's definitions follow its own `GetSocketEvents`,
        // whose signature the epoll backend's does not share.
        let kqueueSection =
            match
                source.IndexOf (
                    "static SocketEvents GetSocketEvents(int16_t filter, uint16_t flags)",
                    StringComparison.Ordinal
                )
            with
            | -1 ->
                failwith
                    "TestSocketEventsPal: the pinned pal_networking.c has no kqueue `GetSocketEvents(int16_t filter, uint16_t flags)`, which this test finds the kqueue backend by."
            | start -> source.Substring start

        functionBody kqueueSection "static int32_t TryChangeSocketEventRegistrationInner("

    /// Upstream builds at most two changes, `EVFILT_READ` first when `SA_READ`
    /// changed and then `EVFILT_WRITE` when `SA_WRITE` did, each an add or a
    /// delete by whether the new mask has the bit, with `EV_RECEIPT` on both
    /// wherever the header defines it, as Darwin's does. Read out of the body,
    /// then checked against `keventChanges` for every pair of masks.
    [<Test>]
    let ``the kqueue changelist is upstream's`` () : unit =
        let body = keventChangeInnerBody ()

        let expect (pattern : string) =
            if not (Regex.IsMatch (body, pattern)) then
                failwith
                    $"TestSocketEventsPal: the kqueue TryChangeSocketEventRegistrationInner no longer matches /%s{pattern}/. Read the body and teach this test.\n%s{body}"

        expect
            @"#ifdef EV_RECEIPT\s*const uint16_t AddFlags = EV_ADD \| EV_CLEAR \| EV_RECEIPT;\s*const uint16_t RemoveFlags = EV_DELETE \| EV_RECEIPT;"

        expect @"int8_t readChanged = \(changes & SocketEvents_SA_READ\) != 0;"
        expect @"int8_t writeChanged = \(changes & SocketEvents_SA_WRITE\) != 0;"
        expect @"int32_t changes = currentEvents \^ newEvents;"

        expect
            @"if \(readChanged\)\s*\{\s*EV_SET\(&events\[i\+\+\],\s*\(uint64_t\)socket,\s*EVFILT_READ,\s*\(newEvents & SocketEvents_SA_READ\) == 0 \? RemoveFlags : AddFlags,\s*0,\s*0,\s*GetKeventUdata\(data\)\);"

        expect
            @"if \(writeChanged\)\s*\{\s*EV_SET\(&events\[i\+\+\],\s*\(uint64_t\)socket,\s*EVFILT_WRITE,\s*\(newEvents & SocketEvents_SA_WRITE\) == 0 \? RemoveFlags : AddFlags,\s*0,\s*0,\s*GetKeventUdata\(data\)\);"

        expect @"kevent\(port, events, GetKeventNchanges\(i\), NULL, 0, NULL\)"

        // Darwin 27.0.0's <sys/event.h>, measured: EVFILT_READ -1, EVFILT_WRITE -2,
        // EV_ADD 0x1, EV_DELETE 0x2, EV_CLEAR 0x20, EV_RECEIPT 0x40.
        let add = 0x0001us ||| 0x0020us ||| 0x0040us
        let remove = 0x0002us ||| 0x0040us

        for current in 0 .. SocketEventsPal.supported do
            for next in 0 .. SocketEventsPal.supported do
                let change (filter : int16) (bit : int) : Kevent =
                    {
                        Ident = uint64 (int64 -7)
                        Filter = filter
                        Flags = if next &&& bit = 0 then remove else add
                        FilterFlags = 0u
                        Data = 0L
                        UserData = 0xC0FFEEUL
                    }

                let expected =
                    [
                        if (current ^^^ next) &&& 0x01 <> 0 then
                            change -1s 0x01
                        if (current ^^^ next) &&& 0x02 <> 0 then
                            change -2s 0x02
                    ]

                SocketEventsPal.keventChanges -7 current next 0xC0FFEEUL |> shouldEqual expected

    /// The kqueue build's `GetSocketEvents(int16_t filter, uint16_t flags)`, read out
    /// of the pinned source: `EVFILT_READ` is `SA_READ` with `SA_READCLOSE` for
    /// `EV_EOF`, `EVFILT_WRITE` is `SA_WRITE` with `SA_READ` for `EV_EOF`, and
    /// `EV_ERROR` adds `SA_ERROR`; then checked against `ofKevent` for every flag word
    /// of both filters.
    [<Test>]
    let ``ofKevent is upstream's kqueue GetSocketEvents`` () : unit =
        let body =
            functionBody
                (File.ReadAllText (palPath "pal_networking.c"))
                "static SocketEvents GetSocketEvents(int16_t filter, uint16_t flags)"

        let expect (pattern : string) =
            if not (Regex.IsMatch (body, pattern)) then
                failwith
                    $"TestSocketEventsPal: the kqueue GetSocketEvents no longer matches /%s{pattern}/. Read the body and teach this test.\n%s{body}"

        expect
            @"case EVFILT_READ:\s*events = SocketEvents_SA_READ;\s*if \(\(flags & EV_EOF\) != 0\)\s*\{\s*events \|= SocketEvents_SA_READCLOSE;\s*\}\s*break;"

        expect
            @"case EVFILT_WRITE:\s*events = SocketEvents_SA_WRITE;(\s*//[^\n]*)*\s*if \(\(flags & EV_EOF\) != 0\)\s*\{\s*events \|= SocketEvents_SA_READ;\s*\}\s*break;"

        expect @"if \(\(flags & EV_ERROR\) != 0\)\s*\{\s*events \|= SocketEvents_SA_ERROR;\s*\}"

        // Each `pinned` reads and parses the header, so resolve the four once
        // rather than in every one of the 65536 iterations.
        let read = pinned "SA_READ"
        let readClose = pinned "SA_READCLOSE"
        let write = pinned "SA_WRITE"
        let errorBit = pinned "SA_ERROR"

        // Darwin 27.0.0's <sys/event.h>, measured: EVFILT_READ -1, EVFILT_WRITE -2,
        // EV_ERROR 0x4000, EV_EOF 0x8000.
        for flags in 0..0xFFFF do
            let flags = uint16 flags
            let eof = flags &&& 0x8000us <> 0us
            let error = if flags &&& 0x4000us <> 0us then errorBit else 0

            SocketEventsPal.ofKevent -1s flags
            |> shouldEqual (read ||| (if eof then readClose else 0) ||| error)

            SocketEventsPal.ofKevent -2s flags
            |> shouldEqual (write ||| (if eof then read else 0) ||| error)

    // ---------------------------------------------------------------------
    // The composition `SystemNative_TryChangeSocketEventRegistration` and
    // `SystemNative_WaitForSocketEvents` answer with, against what they
    // answered when the library stored the shim's three-bit interest.
    // ---------------------------------------------------------------------

    /// Every combination of the five conditions a target's level states.
    let private everyLevel : ReadinessLevel list =
        [
            for bits in 0..31 ->
                {
                    In = bits &&& 0x01 <> 0
                    Out = bits &&& 0x02 <> 0
                    RdHup = bits &&& 0x04 <> 0
                    Hup = bits &&& 0x08 <> 0
                    Err = bits &&& 0x10 <> 0
                }
        ]

    let private epollLevel (level : ReadinessLevel) : uint32 =
        (if level.In then epollBit "EPOLLIN" else 0u)
        ||| (if level.Out then epollBit "EPOLLOUT" else 0u)
        ||| (if level.RdHup then epollBit "EPOLLRDHUP" else 0u)
        ||| (if level.Hup then epollBit "EPOLLHUP" else 0u)
        ||| (if level.Err then epollBit "EPOLLERR" else 0u)

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

    /// Every mask the wrapper admits, registered at every level: converted in
    /// as upstream registers it (`toEpollEvents`, with `EPOLLET`), reported as
    /// Linux's epoll reports (the level restricted to what was registered, plus
    /// `EPOLLERR` and `EPOLLHUP`; see `LinuxReadiness`), and converted out as
    /// upstream delivers it, a registration delivers exactly what the
    /// shim-shaped model delivered, and is on the ready list exactly when that
    /// model's was.
    [<Test>]
    let ``the composition delivers at every level what the shim-shaped model did`` () : unit =
        let alwaysReported = epollBit "EPOLLERR" ||| epollBit "EPOLLHUP"

        let mismatches =
            [
                for level in everyLevel do
                    for mask in 0 .. SocketEventsPal.supported do
                        let registered = SocketEventsPal.toEpollEvents mask ||| EpollEvents.EdgeTriggered
                        let kernel = epollLevel level &&& (registered ||| alwaysReported)
                        let expected = oldReport (mask &&& 0x07) level

                        if (kernel <> 0u) <> (expected <> 0) then
                            yield $"%A{level}, mask 0x%02x{mask}: the kernel reported 0x%08x{kernel}"
                        elif kernel <> 0u && SocketEventsPal.delivered kernel <> expected then
                            yield
                                $"%A{level}, mask 0x%02x{mask}: expected 0x%02x{expected}, delivered 0x%02x{SocketEventsPal.delivered kernel}"
            ]

        mismatches |> List.truncate 20 |> shouldEqual []
