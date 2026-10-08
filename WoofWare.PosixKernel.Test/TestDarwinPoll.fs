namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixPoll.poll` and `UnixPoll.finishPoll` under the Darwin flavour, held to
/// two oracles:
///
/// - a reference transcription of XNU's `poll_nocancel` and `poll_callback`,
///   written imperatively over a table of what each kind of descriptor's
///   filters report, and property-tested against the library over random lists
///   of entries; and
/// - the measurements of `poll-darwin.c`, `poll-entry-interplay.c` and
///   `pipe-activation.c` (docs/plans/2026-08-23-posix-kernel-extraction,
///   Darwin 27.0.0 arm64): the probe's own answers for every state the library
///   models, embedded and replayed, and its multi-entry and sleeping rows as
///   literals.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDarwinPoll =

    // Darwin's `<poll.h>`, as the probe printed it. Stated here rather than read
    // from the library, so that a library which renumbered a bit disagrees with
    // the table instead of with itself.
    let private pollIn : int16 = 0x0001s
    let private pollPri : int16 = 0x0002s
    let private pollOut : int16 = 0x0004s
    let private pollErr : int16 = 0x0008s
    let private pollHup : int16 = 0x0010s
    let private pollNval : int16 = 0x0020s
    let private pollRdNorm : int16 = 0x0040s
    let private pollRdBand : int16 = 0x0080s
    let private pollWrBand : int16 = 0x0100s
    let private pollExtend : int16 = 0x0200s
    let private pollAttrib : int16 = 0x0400s
    let private pollNLink : int16 = 0x0800s
    let private pollWrite : int16 = 0x1000s

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private entry (fd : int) (events : int16) : PollEntry =
        {
            Fd = fd
            Events = events
        }

    /// The task every poll here is made by; `KeventWorld.darwin` has it, and
    /// its helpers make their own calls as other tasks.
    let private poller : int = 2

    let private sound (system : UnixSystem<int, string>) : unit =
        UnixSystem.checkInvariants system |> shouldEqual []

    // ------------------------------------------------------------------
    // A world of descriptors in every state the library models
    // ------------------------------------------------------------------

    let private payload (count : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init count (fun i -> byte (i % 251)))

    let private write (fd : int) (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match
            WriteOutcomes.returned (
                WriteOutcomes.admitThenWrite system.Leader fd UserBuffer.Mapped (payload count) system
            )
        with
        | Ok (WriteAnswer.Completed written, system) when written = int64 count -> system
        | other -> failwith $"writing %d{count} bytes to fd %d{fd}: %A{other}"

    let private read (fd : int) (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match ReadOutcomes.read fd UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadAnswer.Completed _, system) -> system
        | other -> failwith $"reading %d{count} bytes from fd %d{fd}: %A{other}"

    let private pipe (system : UnixSystem<int, string>) : (int * int) * UnixSystem<int, string> =
        match UnixPipe.pipe2 0 UserBuffer.Mapped system with
        | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> (readFd, writeFd), system
        | other -> failwith $"pipe did not make a pipe: %A{other}"

    let private openFlags (directory : bool) : OpenFlags =
        {
            Access =
                if directory then
                    FileAccessMode.ReadOnly
                else
                    FileAccessMode.ReadWrite
            Create = not directory
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = directory
        }

    let private openPath
        (directory : bool)
        (path : string)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match Answered.openPath (openFlags directory) (UnixPath.parseOrFail "TestDarwinPoll" path) 0o644 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other, _ -> failwith $"opening %s{path}: %A{other}"

    /// What one filter of a descriptor reports, as `poll-darwin.c` observed it
    /// with a `kevent` of its own (section S's `filters` lines), and as the
    /// reference below consumes it.
    [<RequireQualifiedAccess>]
    type private Filter =
        /// The registration fails (EBADF or EINVAL).
        | Fails
        /// It registers, and is not ready.
        | Idle
        /// It registers, and is ready.
        | Ready
        /// It registers, and is ready with `EV_EOF`.
        | ReadyEof
        /// The library refuses to register it.
        | Refused

    type private Kind =
        {
            Read : Filter
            Write : Filter
            Vnode : Filter
            /// Whether a read filter reports `EV_OOBAND` back when registered
            /// with it: every target's but a socket's.
            KeepsOutOfBand : bool
            /// The state's name in `poll-darwin.c`'s output, where it printed
            /// one.
            Probe : string option
        }

    let private socketKind (read : Filter) (write : Filter) (probe : string) : Kind =
        {
            Read = read
            Write = write
            Vnode = Filter.Fails
            KeepsOutOfBand = false
            Probe = Some probe
        }

    let private pipeKind (read : Filter) (write : Filter) (probe : string) : Kind =
        {
            Read = read
            Write = write
            Vnode = Filter.Fails
            KeepsOutOfBand = true
            Probe = Some probe
        }

    /// A descriptor that is not open, chosen well above any the world opens.
    let private closedFd : int = 900

    /// A Darwin system holding a descriptor in every state the library models
    /// `poll` for, each with the filters `poll-darwin.c` measured for it.
    let private world : Lazy<(int * Kind) list * UnixSystem<int, string>> =
        lazy
            (let system = KeventWorld.darwin
             let unbound, system = KeventWorld.stream true system
             let bound, system = KeventWorld.stream true system
             let system = KeventWorld.bind bound 4000us system
             let emptyListener, system = KeventWorld.listenerAt 5001us system
             let queuedListener, system = KeventWorld.listenerAt 5002us system
             let _, system = KeventWorld.client 5002us system
             let connectedListener, system = KeventWorld.listenerAt 5003us system
             let connected, system = KeventWorld.client 5003us system
             let accepted, system = KeventWorld.accept connectedListener system
             let finListener, system = KeventWorld.listenerAt 5004us system
             let finClient, system = KeventWorld.client 5004us system
             let finAccepted, system = KeventWorld.accept finListener system
             let system = KeventWorld.close finAccepted system
             let refused, system = KeventWorld.stream true system
             let _, system = KeventWorld.connect refused 6000us system
             let taken, system = KeventWorld.stream true system
             let _, system = KeventWorld.connect taken 6000us system
             let _, system = KeventWorld.readSocketError taken system

             let (emptyRead, roomWrite), system = pipe system
             let (holdingRead, holdingWrite), system = pipe system
             let system = write holdingWrite 3 system
             let (heldGoneRead, heldGoneWrite), system = pipe system
             let system = write heldGoneWrite 3 system
             let system = KeventWorld.close heldGoneWrite system
             let (emptyGoneRead, emptyGoneWrite), system = pipe system
             let system = KeventWorld.close emptyGoneWrite system
             let (fullRead, fullWrite), system = pipe system
             let system = write fullWrite 65536 system
             let (readerGoneRead, readerGoneWrite), system = pipe system
             let system = KeventWorld.close readerGoneRead system

             let file, system = openPath false "/poll-darwin-file" system
             let directory, system = openPath true "/" system
             let kqueue, system = KeventWorld.kqueue system

             let kinds =
                 [
                     unbound, socketKind Filter.Idle Filter.Idle "tcpv4 unbound"
                     bound, socketKind Filter.Idle Filter.Idle "tcpv4 bound"
                     emptyListener, socketKind Filter.Idle Filter.Idle "tcpv4 listener empty"
                     queuedListener, socketKind Filter.Ready Filter.Idle "tcpv4 listener queued"
                     connected, socketKind Filter.Idle Filter.Ready "tcpv4 connected client"
                     accepted, socketKind Filter.Idle Filter.Ready "tcpv4 accepted"
                     finClient, socketKind Filter.ReadyEof Filter.Ready "tcpv4 peer closed"
                     refused, socketKind Filter.ReadyEof Filter.ReadyEof "tcpv4 refused, error pending"
                     taken, socketKind Filter.ReadyEof Filter.ReadyEof "tcpv4 refused, error taken"
                     emptyRead, pipeKind Filter.Idle Filter.Idle "pipe read end, empty"
                     roomWrite, pipeKind Filter.Idle Filter.Ready "pipe write end, room"
                     holdingRead, pipeKind Filter.Ready Filter.Idle "pipe read end, holding 3"
                     heldGoneRead, pipeKind Filter.ReadyEof Filter.ReadyEof "pipe read end, holding 3, writer closed"
                     emptyGoneRead, pipeKind Filter.ReadyEof Filter.ReadyEof "pipe read end, empty, writer closed"
                     fullWrite, pipeKind Filter.Idle Filter.Idle "pipe write end, full"
                     readerGoneWrite, pipeKind Filter.ReadyEof Filter.ReadyEof "pipe write end, reader closed"
                     file,
                     {
                         Read = Filter.Ready
                         Write = Filter.Ready
                         Vnode = Filter.Idle
                         KeepsOutOfBand = true
                         Probe = Some "regular file O_RDWR"
                     }
                     directory,
                     {
                         Read = Filter.Fails
                         Write = Filter.Fails
                         Vnode = Filter.Idle
                         KeepsOutOfBand = true
                         Probe = Some "directory"
                     }
                     // Measured idle while empty; refused, since a kqueue's own
                     // readiness is not modelled.
                     kqueue,
                     {
                         Read = Filter.Refused
                         Write = Filter.Fails
                         Vnode = Filter.Fails
                         KeepsOutOfBand = true
                         Probe = None
                     }
                     closedFd,
                     {
                         Read = Filter.Fails
                         Write = Filter.Fails
                         Vnode = Filter.Fails
                         KeepsOutOfBand = false
                         Probe = Some "closed descriptor"
                     }
                 ]

             sound system
             kinds, system)

    // ------------------------------------------------------------------
    // The reference: XNU's poll_nocancel and poll_callback, transcribed
    // ------------------------------------------------------------------

    /// What a timeout-0 `poll` of `entries` answers, by XNU's
    /// `bsd/kern/sys_generic.c`, given what each descriptor's filters report:
    /// each entry's `revents` and the return value, or the descriptor whose
    /// registration the library refuses.
    let private reference (kindOf : int -> Kind) (entries : PollEntry list) : Result<int16 list * int, int> =
        let entries = List.toArray entries
        let revents : int16 array = Array.zeroCreate entries.Length
        // (fd, isRead) -> (entry, out-of-band at first ADD)
        let knotes = Collections.Generic.Dictionary<int * bool, int * bool> ()
        let queue = Collections.Generic.List<int * bool> ()
        let mutable failed = 0
        let mutable refusal = None

        let has (events : int16) (bits : int16) = events &&& bits <> 0s

        for i in 0 .. entries.Length - 1 do
            let fd = entries.[i].Fd
            let events = entries.[i].Events

            if refusal.IsNone && fd >= 0 then
                let kind = kindOf fd
                let mutable error = false

                let register (isRead : bool) (filter : Filter) (outOfBand : bool) =
                    match filter with
                    | Filter.Refused -> refusal <- Some fd
                    | Filter.Fails -> error <- true
                    | Filter.Idle
                    | Filter.Ready
                    | Filter.ReadyEof ->
                        match knotes.TryGetValue ((fd, isRead)) with
                        | true, (_, firstOutOfBand) -> knotes.[(fd, isRead)] <- (i, firstOutOfBand)
                        | false, _ -> knotes.[(fd, isRead)] <- (i, outOfBand)

                        if filter <> Filter.Idle && not (queue.Contains ((fd, isRead))) then
                            queue.Add ((fd, isRead))

                if has events (pollIn ||| pollRdNorm ||| pollPri ||| pollRdBand ||| pollHup) then
                    register true kind.Read (has events (pollPri ||| pollRdBand))

                if refusal.IsNone && not error && has events (pollOut ||| pollWrBand) then
                    register false kind.Write false

                if
                    refusal.IsNone
                    && not error
                    && has events (pollExtend ||| pollAttrib ||| pollNLink ||| pollWrite)
                then
                    match kind.Vnode with
                    | Filter.Fails -> error <- true
                    | _ -> ()

                if error then
                    revents.[i] <- pollNval
                    failed <- failed + 1

        match refusal with
        | Some fd -> Error fd
        | None ->

        let mutable outputs = failed

        if not (entries.Length > 0 && failed = entries.Length) then
            for fd, isRead in queue do
                let index, firstOutOfBand = knotes.[(fd, isRead)]
                let kind = kindOf fd
                let filter = if isRead then kind.Read else kind.Write
                let events = entries.[index].Events
                let before = revents.[index]
                let mutable r = before

                if filter = Filter.ReadyEof then
                    r <- r ||| pollHup

                if isRead then
                    let mask =
                        if has r pollHup then
                            pollIn ||| pollRdNorm ||| pollPri ||| pollRdBand
                        elif firstOutOfBand && kind.KeepsOutOfBand then
                            pollIn ||| pollRdNorm ||| pollPri ||| pollRdBand
                        else
                            pollIn ||| pollRdNorm

                    r <- r ||| (events &&& mask)
                elif not (has r pollHup) then
                    r <- r ||| (events &&& (pollOut ||| pollWrBand))

                revents.[index] <- r

                if before = 0s && r <> 0s then
                    outputs <- outputs + 1

        Ok (List.ofArray revents, outputs)

    let private pollNow
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : Result<PollOutcome * UnixSystem<int, string>, PollRefusal>
        =
        UnixPoll.poll poller entries milliseconds system

    let private answered (entries : PollEntry list) (system : UnixSystem<int, string>) : int16 list * int =
        match pollNow entries 0 system with
        | Ok (PollOutcome.Answered (revents, count), after) ->
            after |> shouldEqual system
            revents, count
        | other -> failwith $"poll of %A{entries} did not answer: %A{other}"

    [<Test>]
    let ``every timeout-0 poll agrees with XNU's poll_nocancel over the measured filters`` () : unit =
        let kinds, system = world.Force ()

        let kindOf (fd : int) =
            kinds |> List.find (fun (candidate, _) -> candidate = fd) |> snd

        let fds = -1 :: (kinds |> List.map fst)

        let eventsGen : Gen<int16> =
            Gen.oneof
                [
                    Gen.choose (0, 0xFFFF) |> Gen.map (uint16 >> int16)
                    // Requests built from the bits that matter, more often than
                    // a uniform mask would.
                    Gen.subListOf
                        [
                            pollIn
                            pollPri
                            pollOut
                            pollErr
                            pollHup
                            pollNval
                            pollRdNorm
                            pollRdBand
                            pollWrBand
                            pollExtend
                            pollWrite
                        ]
                    |> Gen.map (List.fold (|||) 0s)
                ]

        let entryGen = Gen.map2 entry (Gen.elements fds) eventsGen

        let entriesGen =
            Gen.choose (0, 6) |> Gen.bind (fun n -> Gen.listOfLength n entryGen)

        let property (entries : PollEntry list) =
            let expected = reference kindOf entries

            match expected, pollNow entries 0 system with
            | Error fd, Error (PollRefusal.UnmodelledTarget refused) -> refused |> shouldEqual fd
            | Ok (revents, count), Ok (PollOutcome.Answered (actualRevents, actualCount), after) ->
                (actualRevents, actualCount) |> shouldEqual (revents, count)
                after |> shouldEqual system
            | expected, actual -> failwith $"%A{entries}: expected %A{expected}, got %A{actual}"

        Check.One (config, Prop.forAll (Arb.fromGen entriesGen) property)

    // ------------------------------------------------------------------
    // The probe's own answers, replayed
    // ------------------------------------------------------------------

    let private probeLines (resource : string) : Lazy<string list> =
        lazy
            (let assembly = Assembly.GetExecutingAssembly ()

             use stream =
                 match assembly.GetManifestResourceStream resource with
                 | null -> failwith $"embedded resource %s{resource} not found"
                 | stream -> stream

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
             |> Array.toList)

    let private pollDarwin =
        probeLines "WoofWare.PosixKernel.Test.pollDarwin.darwin.txt"

    /// The probe's names for the bits, as its `b()` prints them.
    let private bitNames : (string * int16) list =
        [
            "IN", pollIn
            "PRI", pollPri
            "OUT", pollOut
            "ERR", pollErr
            "HUP", pollHup
            "NVAL", pollNval
            "RDNORM", pollRdNorm
            "RDBAND", pollRdBand
            "WRBAND", pollWrBand
            "EXTEND", pollExtend
            "ATTRIB", pollAttrib
            "NLINK", pollNLink
            "WRITE", pollWrite
            "0x2000", 0x2000s
            "0x4000", 0x4000s
            "0x8000", int16 0x8000us
        ]

    let private parseBits (text : string) : int16 =
        if text = "0" then
            0s
        else
            text.Split '|'
            |> Array.fold
                (fun acc name ->
                    match List.tryFind (fun (candidate, _) -> candidate = name) bitNames with
                    | Some (_, bit) -> acc ||| bit
                    | None -> failwith $"the probe printed an unknown bit %s{name}"
                )
                0s

    /// The probe's `answers` line for `state`: each request it printed, what
    /// came back, and the return value.
    let private probeAnswers (state : string) : (int16 * int16 * int) list =
        let lines =
            pollDarwin.Force ()
            |> List.filter (fun line -> line.StartsWith ($"S\t%s{state}\tanswers ", StringComparison.Ordinal))

        match lines with
        | [ line ] ->
            line.Substring(($"S\t%s{state}\tanswers ").Length).Split ' '
            |> Array.toList
            |> List.map (fun pair ->
                let m =
                    System.Text.RegularExpressions.Regex.Match (
                        pair,
                        @"^(?<events>[^-]+)->(?<revents>[^(]+)\(rv=(?<rv>\d+)\)$"
                    )

                if not m.Success then
                    failwith $"unparseable answer %s{pair}"

                parseBits m.Groups.["events"].Value, parseBits m.Groups.["revents"].Value, int m.Groups.["rv"].Value
            )
        | lines -> failwith $"the probe printed %d{List.length lines} answers lines for %s{state}"

    [<Test>]
    let ``every modelled state answers the probe's own requests as the probe measured`` () : unit =
        let kinds, system = world.Force ()

        let states =
            kinds
            |> List.choose (fun (fd, kind) -> kind.Probe |> Option.map (fun probe -> fd, probe))

        // Two columns of probe states never left out by accident.
        List.length states |> shouldEqual 19

        for fd, state in states do
            for events, revents, rv in probeAnswers state do
                answered [ entry fd events ] system |> shouldEqual ([ revents ], rv)

        // The IPv6 states measured the same, row for row.
        for fd, state in states do
            if state.StartsWith ("tcpv4 ", StringComparison.Ordinal) then
                probeAnswers ("tcpv6 " + state.Substring 6) |> shouldEqual (probeAnswers state)

        // A negative descriptor reports nothing to anything, and is not counted.
        for events, revents, rv in probeAnswers "descriptor -1" do
            answered [ entry -1 events ] system |> shouldEqual ([ revents ], rv)

    /// The multi-entry rows, measured (`poll-darwin.c` section M and
    /// `poll-entry-interplay.c`), each against the descriptors of the world.
    [<Test>]
    let ``several entries in one call answer as measured`` () : unit =
        let kinds, system = world.Force ()

        let fdOf (probe : string) =
            kinds |> List.find (fun (_, kind) -> kind.Probe = Some probe) |> fst

        let queued = fdOf "tcpv4 listener queued"
        let idle = fdOf "tcpv4 unbound"
        let connected = fdOf "tcpv4 connected client"
        let finClient = fdOf "tcpv4 peer closed"
        let holding = fdOf "pipe read end, holding 3"
        let heldGone = fdOf "pipe read end, holding 3, writer closed"
        let file = fdOf "regular file O_RDWR"
        let queuedDup, withDup = KeventWorld.dup queued system

        let rows =
            [
                "M1", [ entry queued pollIn ; entry queued pollIn ], ([ 0s ; pollIn ], 1)
                "M3", [ entry queued (pollIn ||| pollExtend) ; entry idle pollIn ], ([ pollIn ||| pollNval ; 0s ], 1)
                "M4", [ entry queued (pollIn ||| pollExtend) ], ([ pollNval ], 1)
                "M5", [ entry queued (pollIn ||| pollExtend) ; entry -1 pollIn ], ([ pollIn ||| pollNval ; 0s ], 1)
                "M6", [ entry -1 pollIn ; entry closedFd pollIn ], ([ 0s ; pollNval ], 1)
                "M7", [ entry closedFd 0s ; entry closedFd pollIn ], ([ 0s ; pollNval ], 1)
                "M8", [ entry closedFd pollIn ; entry queued pollIn ], ([ pollNval ; pollIn ], 2)
                "M9", [ entry queued pollIn ; entry queued (pollIn ||| pollExtend) ], ([ 0s ; pollIn ||| pollNval ], 1)
                "M10", [ entry queued (pollIn ||| pollExtend) ; entry queued pollIn ], ([ pollNval ; pollIn ], 2)
                "M11", [ entry connected pollOut ; entry connected pollIn ], ([ pollOut ; 0s ], 1)
                "M12", [ entry connected pollOut ; entry connected pollOut ], ([ 0s ; pollOut ], 1)
                "M13",
                [ entry finClient (pollIn ||| pollOut) ; entry finClient pollIn ],
                ([ pollOut ; pollIn ||| pollHup ], 2)
                "M14",
                [ entry finClient (pollIn ||| pollOut) ; entry finClient pollOut ],
                ([ pollIn ||| pollHup ; pollOut ], 2)
                "M15", [ entry finClient pollOut ; entry finClient pollIn ], ([ pollOut ; pollIn ||| pollHup ], 2)
                "M16", [ entry finClient pollIn ; entry finClient pollOut ], ([ pollIn ||| pollHup ; pollOut ], 2)
                "M17", [ entry holding pollIn ; entry holding pollPri ], ([ 0s ; 0s ], 0)
                "M18", [ entry holding pollPri ; entry holding pollIn ], ([ 0s ; pollIn ], 1)
                "M19", [ entry holding pollIn ; entry holding (pollIn ||| pollPri) ], ([ 0s ; pollIn ], 1)
                "M20",
                [ entry holding pollPri ; entry holding (pollIn ||| pollRdNorm) ],
                ([ 0s ; pollIn ||| pollRdNorm ], 1)
                "M21", [ entry holding (pollIn ||| pollOut) ; entry holding pollOut ], ([ pollIn ; 0s ], 1)
                "M22", [ entry heldGone pollIn ; entry heldGone pollPri ], ([ 0s ; pollPri ||| pollHup ], 1)
                "M23", [ entry file pollIn ; entry file pollPri ], ([ 0s ; 0s ], 0)
                "M24", [ entry file pollPri ; entry file pollIn ], ([ 0s ; pollIn ], 1)
                "M25", [ entry file (pollIn ||| pollExtend) ; entry file pollExtend ], ([ pollIn ; 0s ], 1)
            ]

        for name, entries, expected in rows do
            (name, answered entries system) |> shouldEqual (name, expected)

        answered [ entry queued pollIn ; entry queuedDup pollIn ] withDup
        |> shouldEqual ([ pollIn ; pollIn ], 2)

    // ------------------------------------------------------------------
    // Sleeping
    // ------------------------------------------------------------------

    /// `poll` by `poller`, which must park.
    let private parks
        (entries : PollEntry list)
        (milliseconds : int)
        (system : UnixSystem<int, string>)
        : WakeCondition * UnixSystem<int, string>
        =
        match pollNow entries milliseconds system with
        | Ok (PollOutcome.WouldBlock condition, parked) ->
            sound parked
            condition, parked
        | other -> failwith $"poll of %A{entries} did not park: %A{other}"

    let private wakes (condition : WakeCondition) (system : UnixSystem<int, string>) : bool =
        not (Set.isEmpty (WakeCondition.satisfied poller condition system))

    let private finish (system : UnixSystem<int, string>) : Result<PollOutcome * UnixSystem<int, string>, PollRefusal> =
        UnixPoll.finishPoll poller system

    let private finishAnswer (system : UnixSystem<int, string>) : int16 list * int =
        match finish system with
        | Ok (PollOutcome.Answered (revents, count), after) ->
            UnixTaskTable.parkedFor poller after.Tasks |> shouldEqual None
            revents, count
        | other -> failwith $"the finish did not answer: %A{other}"

    let private advanceTo (nanoseconds : int64) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Machine =
                { system.Machine with
                    NanosecondsSinceBoot = nanoseconds
                }
        }

    let private deadlineOf (condition : WakeCondition) : int64 =
        match WakeCondition.deadlines condition with
        | [ deadline ] -> deadline
        | other -> failwith $"expected one deadline, got %A{other}"

    [<Test>]
    let ``each socket producer wakes a sleeping poll, which answers as measured`` () : unit =
        // W1: an empty listener asked for IN, then a connection.
        let listener, system = KeventWorld.listenerAt 5100us KeventWorld.darwin
        let condition, parked = parks [ entry listener pollIn ] 1000 system
        wakes condition parked |> shouldEqual false
        let _, connected = KeventWorld.client 5100us parked
        sound connected
        wakes condition connected |> shouldEqual true
        finishAnswer connected |> shouldEqual ([ pollIn ], 1)

        // W4 and W5: a connected client asked for IN, or for HUP alone, then the
        // peer closes.
        for asked, answer in [ pollIn, pollIn ||| pollHup ; pollHup, pollHup ] do
            let listener, system = KeventWorld.listenerAt 5101us KeventWorld.darwin
            let client, system = KeventWorld.client 5101us system
            let accepted, system = KeventWorld.accept listener system
            let condition, parked = parks [ entry client asked ] 1000 system
            let closed = KeventWorld.close accepted parked
            sound closed
            wakes condition closed |> shouldEqual true
            finishAnswer closed |> shouldEqual ([ answer ], 1)

        // W6c and W6d: an idle socket asked for IN|OUT, or OUT alone, then its
        // connect is refused.
        for asked, answer in [ pollIn ||| pollOut, pollIn ||| pollHup ; pollOut, pollHup ] do
            let socket, system = KeventWorld.stream true KeventWorld.darwin
            let condition, parked = parks [ entry socket asked ] 2000 system
            let _, refused = KeventWorld.connect socket 6000us parked
            sound refused
            wakes condition refused |> shouldEqual true
            finishAnswer refused |> shouldEqual ([ answer ], 1)

    /// W11: a connect completing activates the socket's WRITE, and the peer's
    /// close then its READ. A poller that finishes between the two answers
    /// OUT; one that finishes after both answers IN|OUT|HUP, which no poll made
    /// in the final state answers (IN|HUP) -- measured, 33 and 37 of 40 trials
    /// with the poller held off its CPU.
    [<Test>]
    let ``the order a socket's filters were activated in decides what a sleeper answers`` () : unit =
        let listener, system = KeventWorld.listenerAt 5102us KeventWorld.darwin
        let socket, system = KeventWorld.stream true system
        let condition, parked = parks [ entry socket (pollIn ||| pollOut) ] 2000 system
        let _, connected = KeventWorld.connect socket 5102us parked
        wakes condition connected |> shouldEqual true

        // The poller runs at once.
        finishAnswer connected |> shouldEqual ([ pollOut ], 1)

        // The poller runs only after the peer has closed too.
        let accepted, both = KeventWorld.accept listener connected
        let both = KeventWorld.close accepted both
        sound both
        finishAnswer both |> shouldEqual ([ pollIn ||| pollOut ||| pollHup ], 1)

        // A poll made in that final state.
        let fresh = UnixParkState.unpark poller both

        answered [ entry socket (pollIn ||| pollOut) ] fresh
        |> shouldEqual ([ pollIn ||| pollHup ], 1)

    [<Test>]
    let ``a wake that adds nothing does not end the sleep`` () : unit =
        // W2 and W3: an empty listener asked for HUP (and OUT), then a
        // connection: the activation adds nothing, and the poll sleeps on to
        // its deadline.
        for asked in [ pollHup ; pollHup ||| pollOut ] do
            let listener, system = KeventWorld.listenerAt 5103us KeventWorld.darwin
            let condition, parked = parks [ entry listener asked ] 300 system
            let _, connected = KeventWorld.client 5103us parked
            wakes condition connected |> shouldEqual false
            let late = advanceTo (deadlineOf condition) connected
            wakes condition late |> shouldEqual true
            finishAnswer late |> shouldEqual ([ 0s ], 0)

        // W7: HUP alone of a pipe that holds data, whose READ reported at
        // registration and was consumed: the writer's close wakes nothing.
        let (readFd, writeFd), system = pipe KeventWorld.darwin
        let system = write writeFd 3 system
        let condition, parked = parks [ entry readFd pollHup ] 300 system
        let closed = KeventWorld.close writeFd parked
        sound closed
        wakes condition closed |> shouldEqual false

        // The same poll of an empty pipe is woken by the close.
        let (readFd, writeFd), system = pipe KeventWorld.darwin
        let condition, parked = parks [ entry readFd pollHup ] 300 system
        let closed = KeventWorld.close writeFd parked
        wakes condition closed |> shouldEqual true
        finishAnswer closed |> shouldEqual ([ pollHup ], 1)

    /// Each pipe operation `pipe-activation.c` measured as activating a filter
    /// that becomes ready wakes a poll asleep on it.
    [<Test>]
    let ``pipe operations wake a sleeping poll as measured`` () : unit =
        // W8: a write into an empty pipe.
        let (readFd, writeFd), system = pipe KeventWorld.darwin
        let condition, parked = parks [ entry readFd pollIn ] 1000 system
        let written = write writeFd 1 parked
        sound written
        wakes condition written |> shouldEqual true
        finishAnswer written |> shouldEqual ([ pollIn ], 1)

        // A read that frees 512 bytes of a full pipe, and not one that frees 1.
        let (readFd, writeFd), system = pipe KeventWorld.darwin
        let system = write writeFd 65536 system
        let condition, parked = parks [ entry writeFd pollOut ] 1000 system
        let one = read readFd 1 parked
        wakes condition one |> shouldEqual false
        let more = read readFd 511 one
        sound more
        wakes condition more |> shouldEqual true
        finishAnswer more |> shouldEqual ([ pollOut ], 1)

        // The reader's close, under a write end that is full.
        let (readFd, writeFd), system = pipe KeventWorld.darwin
        let system = write writeFd 65536 system
        let condition, parked = parks [ entry writeFd (pollOut ||| pollIn) ] 1000 system
        let closed = KeventWorld.close readFd parked
        wakes condition closed |> shouldEqual true
        finishAnswer closed |> shouldEqual ([ pollIn ||| pollHup ], 1)

    [<Test>]
    let ``closing a watched descriptor drops its entry, and the poll sleeps to its timeout`` () : unit =
        let listener, system = KeventWorld.listenerAt 5104us KeventWorld.darwin
        let condition, parked = parks [ entry listener pollIn ] 300 system
        let closed = KeventWorld.close listener parked
        sound closed

        match UnixTaskTable.parkedFor poller closed.Tasks with
        | Some (ParkedSyscall.KqueuePoll poll) ->
            (UnixMachineState.pollQueue poll.Queue closed.Machine).Registrations
            |> shouldEqual Map.empty
        | other -> failwith $"expected a Darwin poll, got %A{other}"

        // A new listener takes the number, and is connected to: nothing wakes
        // the sleeper.
        let reused, again = KeventWorld.listenerAt 5105us closed
        reused |> shouldEqual listener
        let _, connected = KeventWorld.client 5105us again
        wakes condition connected |> shouldEqual false
        let late = advanceTo (deadlineOf condition) connected
        finishAnswer late |> shouldEqual ([ 0s ], 0)

    /// XNU's poll holds no file across its sleep, so closing the last
    /// descriptor onto a socket it watches releases the socket at once: its
    /// peer sees the FIN, which a poll holding the file would postpone until it
    /// returned.
    [<Test>]
    let ``a sleeping poll holds no description, so the last close releases what it watches`` () : unit =
        let listener, system = KeventWorld.listenerAt 5108us KeventWorld.darwin
        let client, system = KeventWorld.client 5108us system
        let accepted, system = KeventWorld.accept listener system
        let acceptedId = KeventWorld.idOf accepted system
        let condition, parked = parks [ entry accepted pollIn ] 300 system
        let closed = KeventWorld.close accepted parked
        sound closed

        OpenFileTable.descriptions closed.Machine.OpenFiles
        |> Map.containsKey acceptedId
        |> shouldEqual false

        let fresh = UnixParkState.unpark poller closed

        answered [ entry client pollIn ] fresh
        |> shouldEqual ([ pollIn ||| pollHup ], 1)

        wakes condition closed |> shouldEqual false

    [<Test>]
    let ``a poll that registers nothing sleeps to its timeout`` () : unit =
        // W9: a closed descriptor asked for nothing, and no entries at all.
        for entries in [ [ entry closedFd 0s ] ; [] ] do
            let condition, parked = parks entries 200 KeventWorld.darwin
            wakes condition parked |> shouldEqual false
            let late = advanceTo (deadlineOf condition) parked
            finishAnswer late |> shouldEqual (entries |> List.map (fun _ -> 0s), 0)

        // And -1 waits for ever.
        let condition, _ = parks [ entry closedFd 0s ] -1 KeventWorld.darwin
        WakeCondition.deadlines condition |> shouldEqual []

    [<Test>]
    let ``a deadline is milliseconds from now, and a signal ends the sleep with EINTR`` () : unit =
        let listener, system = KeventWorld.listenerAt 5106us KeventWorld.darwin
        let now = system.Machine.NanosecondsSinceBoot
        let condition, parked = parks [ entry listener pollIn ] 7 system
        deadlineOf condition |> shouldEqual (now + 7_000_000L)
        let early = advanceTo (now + 6_999_999L) parked
        wakes condition early |> shouldEqual false

        // Woken with nothing to report and no deadline passed, the poll sleeps
        // again.
        match finish early with
        | Ok (PollOutcome.WouldBlock _, again) ->
            UnixTaskTable.parkedFor poller again.Tasks |> Option.isSome |> shouldEqual true
        | other -> failwith $"expected a re-park, got %A{other}"

        // Something to report and an expired deadline at once: Darwin answers
        // whichever reached the sleeper first.
        let _, connected = KeventWorld.client 5106us (advanceTo (now + 7_000_000L) parked)
        finish connected |> shouldEqual (Error PollRefusal.EventsBesideDeadline)

    [<Test>]
    let ``what the call refuses rather than answers`` () : unit =
        let kinds, system = world.Force ()

        let fdOf (probe : string) =
            kinds |> List.find (fun (_, kind) -> kind.Probe = Some probe) |> fst

        let file = fdOf "regular file O_RDWR"
        let directory = fdOf "directory"
        let idle = fdOf "tcpv4 unbound"
        let queued = fdOf "tcpv4 listener queued"

        // A vnode filter is answered when the call need not sleep, and refused
        // when it would.
        answered [ entry file pollExtend ] system |> shouldEqual ([ 0s ], 0)

        answered [ entry file (pollIn ||| pollExtend) ] system
        |> shouldEqual ([ pollIn ], 1)

        for fd in [ file ; directory ] do
            pollNow [ entry fd pollExtend ] 10 system
            |> shouldEqual (Error (PollRefusal.UnmodelledVnodeWait fd))

        // A negative timeout other than -1 is refused only when the call would
        // sleep.
        pollNow [ entry idle pollIn ] -2 system
        |> shouldEqual (Error (PollRefusal.UnmeasuredNegativeTimeout -2))

        pollNow [ entry queued pollIn ] -2 system
        |> Result.map fst
        |> shouldEqual (Ok (PollOutcome.Answered ([ pollIn ], 1)))

        // A socket whose filters are not modelled, asked for a bit that
        // registers one; a vnode bit alone fails to register, which is answered.
        let udp, withUdp =
            NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

        pollNow [ entry udp pollOut ] 0 withUdp
        |> shouldEqual (Error (PollRefusal.UnmodelledSocket (udp, SocketDomain.Inet, SocketKind.Datagram)))

        answered [ entry udp pollExtend ] withUdp |> shouldEqual ([ pollNval ], 1)

    [<Test>]
    let ``the entry count is screened as measured`` () : unit =
        let system = KeventWorld.darwin
        let entries (n : int) = List.replicate n (entry -1 pollIn)

        pollNow (entries 10241) 0 system
        |> Result.map fst
        |> shouldEqual (Ok (PollOutcome.Failed UnixError.EINVAL))

        for n in [ 1025 ; 10240 ] do
            pollNow (entries n) 0 system
            |> shouldEqual (Error (PollRefusal.UnmodelledEntryCount n))

        answered (entries 1024) system |> shouldEqual (List.replicate 1024 0s, 0)

    [<Test>]
    let ``a sleeping poll whose socket filter is ready but not activated breaks the invariants`` () : unit =
        let listener, system = KeventWorld.listenerAt 5107us KeventWorld.darwin
        let _, parked = parks [ entry listener pollIn ] 1000 system
        let _, connected = KeventWorld.client 5107us parked
        sound connected

        // Undo the activation the connection made.
        let machine =
            { connected.Machine with
                PollQueues =
                    connected.Machine.PollQueues
                    |> Map.map (fun _ queue ->
                        { queue with
                            Active = []
                        }
                    )
            }

        UnixSystem.checkInvariants
            { connected with
                Machine = machine
            }
        |> shouldEqual
            [
                UnixSystemDefect.ParkedKqueuePollActivationMissed (poller, (listener, KqueueFilter.Read))
            ]

    [<Test>]
    let ``a sleeping poll's kqueue is held by its park alone, attached to the sockets its descriptors name`` () : unit =
        let listener, system = KeventWorld.listenerAt 5109us KeventWorld.darwin
        let _, parked = parks [ entry listener pollIn ] 1000 system
        sound parked

        let queueId, queue =
            match UnixTaskTable.parkedFor poller parked.Tasks with
            | Some (ParkedSyscall.KqueuePoll poll) -> poll.Queue, UnixMachineState.pollQueue poll.Queue parked.Machine
            | other -> failwith $"expected a Darwin poll, got %A{other}"

        let socket =
            match FileDescriptorRegistry.tryFindTarget listener (UnixSystemState.fileDescriptors parked) with
            | Some (OpenFileTarget.Socket socket) -> socket
            | other -> failwith $"the listener's descriptor names %A{other}"

        queue.Registrations
        |> Map.toList
        |> List.map (fun (key, registration) -> key, registration.Socket)
        |> shouldEqual [ (listener, KqueueFilter.Read), Some socket ]

        let withQueues (queues : Map<PollQueueId, PollQueue>) (next : PollQueueId) =
            { parked with
                Machine =
                    { parked.Machine with
                        PollQueues = queues
                        NextPollQueueId = next
                    }
            }

        let next = parked.Machine.NextPollQueueId

        // Gone from the machine while the park names it.
        UnixSystem.checkInvariants (withQueues Map.empty next)
        |> shouldEqual [ UnixSystemDefect.ParkedOnAbsentPollQueue (poller, queueId) ]

        // A second one no park names.
        let (PollQueueId n) = next

        UnixSystem.checkInvariants (withQueues (Map.add next queue parked.Machine.PollQueues) (PollQueueId (n + 1L)))
        |> shouldEqual [ UnixSystemDefect.PollQueueNotHeldOnce (next, 0) ]

        // Not below the counter.
        UnixSystem.checkInvariants (withQueues parked.Machine.PollQueues queueId)
        |> shouldEqual [ UnixSystemDefect.PollQueueIdNotFresh (queueId, queueId) ]

        // Another process's.
        let other = ProcessId.parseOrFail "test" 9

        UnixSystem.checkInvariants (
            withQueues
                (Map.add
                    queueId
                    { queue with
                        Owner = other
                    }
                    parked.Machine.PollQueues)
                next
        )
        |> shouldEqual [ UnixSystemDefect.PollQueueOfAnotherProcess (poller, queueId, other) ]

        // A socket's filter attached to no socket.
        let detached =
            { queue with
                Registrations =
                    queue.Registrations
                    |> Map.map (fun _ registration ->
                        { registration with
                            Socket = None
                        }
                    )
            }

        UnixSystem.checkInvariants (withQueues (Map.add queueId detached parked.Machine.PollQueues) next)
        |> shouldEqual
            [
                UnixSystemDefect.ParkedKqueuePollAttachedElsewhere (
                    poller,
                    (listener, KqueueFilter.Read),
                    None,
                    Some socket
                )
            ]

        // ...and the park's end lets it go.
        let finished = UnixParkState.unpark poller parked
        finished.Machine.PollQueues |> shouldEqual Map.empty
        sound finished
