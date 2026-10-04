namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixKqueue.kqueue`, `UnixKqueue.kevent` and `UnixKqueue.finishKevent`: the
/// descriptor `kqueue()` makes, `kevent`'s argument ladder, its timeout, and what
/// a close does to a task asleep in it.
///
/// The facts these rows hold the library to were measured by
/// `docs/plans/2026-08-23-posix-kernel-extraction/kqueue-kevent.c` on Darwin
/// 27.0.0 arm64 (2026-10-02), whose output is embedded and replayed line by line:
/// every one of its 5040 argument combinations, and every wait on a drained
/// kqueue.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKqueue =

    let private nanosecondsPerSecond : int64 = 1_000_000_000L

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    let private darwin : UnixSystem<int, string> =
        UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot
        |> fun system -> ([ 1..4 ], system) ||> List.foldBack Tasks.ensure

    let private linux : UnixSystem<int, string> =
        UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot
        |> Tasks.ensure 1

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

    let private idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
        | Some id -> id
        | None -> failwith $"fd %d{fd} names no description"

    let private createKqueue (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixKqueue.kqueue system with
        | Ok (fd, system) -> fd, system
        | Error refusal -> failwith $"expected a kqueue, got %s{KqueueRefusal.describe refusal}"

    let private dup (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match FileDescriptorRegistry.dup fd system.Process.FileDescriptors with
        | Ok (copy, registry) -> copy, withRegistry registry system
        | Error error -> failwith $"dup: %O{error}"

    let private close (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed 0L, closed) -> closed
        | other -> failwith $"expected the close of fd %d{fd} to succeed, got %A{other}"

    let private after (nanoseconds : int64) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Machine = UnixMachineState.advanceClock nanoseconds system.Machine
        }

    /// A wait for one event, with no changes.
    let private waits
        (task : int)
        (fd : int)
        (timeout : KeventTimeout)
        (system : UnixSystem<int, string>)
        : Result<KeventOutcome * UnixSystem<int, string>, KeventRefusal>
        =
        UnixKqueue.kevent task fd 0 [] 1 UserBuffer.Mapped timeout system

    let private parks (task : int) (fd : int) (timeout : KeventTimeout) (system : UnixSystem<int, string>) =
        match waits task fd timeout system with
        | Ok (KeventOutcome.WouldBlock condition, parked) -> condition, parked
        | other -> failwith $"expected task %d{task} to park, got %A{other}"

    let private finishes (task : int) (system : UnixSystem<int, string>) : KeventOutcome * UnixSystem<int, string> =
        match UnixKqueue.finishKevent task system with
        | Ok (outcome, finished) ->
            UnixTaskTable.parkedFor task finished.Tasks |> shouldEqual None
            outcome, finished
        | Error refusal -> failwith $"expected task %d{task}'s wait to finish, got %s{KeventRefusal.describe refusal}"

    let private woken (asleep : int list) (system : UnixSystem<int, string>) : (int * Set<WakePrimitive>) list =
        UnixWait.wakes (Set.ofList asleep) system

    /// `system` with a caught SIGUSR1 pending for `task`.
    let private signalled (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals =
                        system.Process.Signals
                        |> SignalState.setDisposition
                            Signal.SIGUSR1
                            (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
                        |> SignalState.enqueue
                            {
                                Signal = Signal.SIGUSR1
                                Target = ValueSome task
                            }
                }
        }

    /// A change a kqueue would apply: an `EV_ADD` of `EVFILT_READ` on standard input.
    let private someChange : Kevent =
        {
            Ident = 0UL
            Filter = KeventFilter.Read
            Flags = KeventFlags.Add ||| KeventFlags.Clear ||| KeventFlags.Receipt
            FilterFlags = 0u
            Data = 0L
            UserData = 42UL
        }

    // ------------------------------------------------------------------
    // kqueue()
    // ------------------------------------------------------------------

    [<Test>]
    let ``kqueue makes a fresh blocking kqueue on the lowest free descriptor`` () : unit =
        // Measured (section A): with 3 free and 4 open, kqueue() returned 3; O_RDWR, not
        // O_NONBLOCK. The property sweeps which descriptors above the standard streams are
        // open.
        let property (openMask : bool list) : unit =
            let opened, system =
                ((darwin, []), openMask)
                ||> List.fold (fun (system, opened) keep ->
                    let fd, system = dup 0 system
                    system, (fd, keep) :: opened
                )
                |> fun (system, opened) -> opened, system

            let system =
                (system, opened)
                ||> List.fold (fun system (fd, keep) -> if keep then system else close fd system)

            let expected =
                Seq.initInfinite id
                |> Seq.find (fun fd ->
                    FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors
                    |> Option.isNone
                )

            let fd, created = createKqueue system
            fd |> shouldEqual expected

            match FileDescriptorRegistry.tryFindWithId fd created.Process.FileDescriptors with
            | Some (id, description) ->
                description
                |> shouldEqual
                    {
                        Target =
                            OpenFileTarget.Kqueue
                                {
                                    Drained = false
                                    Registrations = Map.empty
                                    Active = []
                                }
                        AccessMode = FileAccessMode.ReadWrite
                        NonBlocking = false
                        Flock = None
                        Status = OpenFileStatus.none
                    }

                // A fresh description, not one any other descriptor shares.
                FileDescriptorRegistry.descriptions system.Process.FileDescriptors
                |> Map.containsKey id
                |> shouldEqual false
            | None -> failwith "the kqueue's descriptor names nothing"

            // Nothing but the descriptor table changed.
            { created with
                Process = system.Process
            }
            |> shouldEqual system

            UnixSystem.checkInvariants created |> shouldEqual []

        let masks =
            Gen.listOf (ArbMap.defaults |> ArbMap.generate<bool>)
            |> Gen.map (List.truncate 8)

        Check.One (config, Prop.forAll (Arb.fromGen masks) property)

    [<Test>]
    let ``Linux has no kqueue, so kqueue and kevent are refused`` () : unit =
        UnixKqueue.kqueue linux
        |> shouldEqual (Error (KqueueRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux))

        waits 1 3 KeventTimeout.Null linux
        |> shouldEqual (Error (KeventRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux))

    // ------------------------------------------------------------------
    // The measured sweep
    // ------------------------------------------------------------------

    /// The lines of the embedded probe output that start with `prefix`, split on tabs.
    let private probeLines (prefix : string) : string list list =
        let assembly = Assembly.GetExecutingAssembly ()
        let resource = "WoofWare.PosixKernel.Test.kqueueKevent.darwin.txt"

        use stream =
            match assembly.GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> line.StartsWith (prefix + "\t", StringComparison.Ordinal))
        |> List.map (fun line -> line.Split '\t' |> Array.toList |> List.tail)

    let private changesOf (column : string) : int * Kevent list =
        // Every changelist the probe passed is null or unreadable, so the copy-in finds no
        // readable change whatever the count.
        match column with
        | "0/NULL" -> 0, []
        | "-1/NULL"
        | "-1/unreadable" -> -1, []
        | "INT_MIN/NULL" -> Int32.MinValue, []
        | "1/unreadable" -> 1, []
        | other -> failwith $"unknown changelist column %s{other}"

    let private eventlistOf (column : string) : UserBuffer =
        match column with
        | "real" -> UserBuffer.Mapped
        | "NULL" -> UserBuffer.Unmapped 0UL
        | "unreadable" -> UserBuffer.Unmapped 8UL
        | other -> failwith $"unknown eventlist column %s{other}"

    let private timeoutOf (column : string) : KeventTimeout =
        let readable (seconds : int64) (nanoseconds : int64) =
            KeventTimeout.Readable (seconds, nanoseconds)

        match column with
        | "NULL" -> KeventTimeout.Null
        | "unreadable" -> KeventTimeout.Unreadable
        | "{0,0}" -> readable 0L 0L
        | "{0,20ms}" -> readable 0L 20_000_000L
        | "{0,-1}" -> readable 0L -1L
        | "{0,1e9}" -> readable 0L 1_000_000_000L
        | "{0,1e9+1}" -> readable 0L 1_000_000_001L
        | "{-1,0}" -> readable -1L 0L
        | "{-1,1}" -> readable -1L 1L
        | "{INT64_MAX,0}" -> readable Int64.MaxValue 0L
        | "{INT32_MAX,0}" -> readable (int64 Int32.MaxValue) 0L
        | "{INT32_MAX+1,0}" -> readable (int64 Int32.MaxValue + 1L) 0L
        | other -> failwith $"unknown timeout column %s{other}"

    let private errnoOf (name : string) : UnixError =
        match name with
        | "EBADF" -> UnixError.EBADF
        | "EINVAL" -> UnixError.EINVAL
        | "EFAULT" -> UnixError.EFAULT
        | other -> failwith $"unexpected errno %s{other} in the probe's output"

    /// The probe cut every wait short with a 40 ms timer: a sleeper it reports as
    /// interrupted waited at least that long, and one it reports as having returned 0
    /// after sleeping waited less.
    let private probeTimer : int64 = 40_000_000L

    /// Whether `outcome`, from a system whose clock reads `now`, is what the probe's
    /// line reported.
    let private agreesWith
        (now : int64)
        (rv : string)
        (errno : string)
        (slept : string)
        (outcome : KeventOutcome)
        (before : UnixSystem<int, string>)
        (after : UnixSystem<int, string>)
        (task : int)
        : unit
        =
        match rv, errno, slept, outcome with
        | "rv=-1", errno, "slept=0", KeventOutcome.Failed error ->
            error |> shouldEqual (errnoOf errno)
            after |> shouldEqual before
        | "rv=0", "-", "slept=0", KeventOutcome.Answered [] -> after |> shouldEqual before
        | "rv=0", "-", "slept=1", KeventOutcome.WouldBlock _ ->
            match UnixTaskTable.parkedFor task after.Tasks with
            | Some (ParkedSyscall.Kevent {
                                             Deadline = Some deadline
                                         }) -> deadline - now |> shouldBeSmallerThan probeTimer
            | other -> failwith $"expected a park with a deadline, got %A{other}"
        | "rv=-1", "EINTR", "slept=1", KeventOutcome.WouldBlock _ ->
            match UnixTaskTable.parkedFor task after.Tasks with
            | Some (ParkedSyscall.Kevent {
                                             Deadline = None
                                         }) -> ()
            | Some (ParkedSyscall.Kevent {
                                             Deadline = Some deadline
                                         }) -> deadline - now |> shouldBeGreaterThan (probeTimer - 1L)
            | other -> failwith $"expected a park, got %A{other}"
        | rv, errno, slept, outcome ->
            failwith $"the probe said %s{rv} %s{errno} %s{slept}, and kevent answered %A{outcome}"

    [<Test>]
    let ``every argument combination the probe swept is answered as Darwin answered it`` () : unit =
        // The descriptors the probe waited through, opened in a system whose clock is
        // somewhere past boot.
        let system = after 123_456_789L darwin

        let socketFd, system =
            let socket =
                {
                    Domain = SocketDomain.Inet
                    Kind = SocketKind.Datagram
                    Protocol = SocketProtocol.Udp
                    Binding = None
                    ReuseAddress = false
                    Phase = SocketPhase.Idle
                }

            let fd, registry =
                FileDescriptorRegistry.createSocket system.Machine.NextSocketId system.Process.FileDescriptors

            let (SocketId raw) = system.Machine.NextSocketId

            fd,
            { withRegistry registry system with
                Machine =
                    { system.Machine with
                        Sockets = Map.add system.Machine.NextSocketId socket system.Machine.Sockets
                        NextSocketId = SocketId (raw + 1L)
                    }
            }

        let fileFd, system =
            let fd, registry =
                FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

            fd, withRegistry registry system

        let kqueueFd, system = createKqueue system
        let dupFd, system = dup kqueueFd system

        let descriptor (column : string) : int =
            match column with
            | "closed" -> 500
            | "minus1" -> -1
            | "socket" -> socketFd
            | "file" -> fileFd
            // Standard input, the read end of the launch pipe.
            | "pipe" -> 0
            | "kqueue" -> kqueueFd
            | "kqueue-dup" -> dupFd
            | other -> failwith $"unknown descriptor column %s{other}"

        let lines = probeLines "B"
        List.length lines |> shouldEqual 5040

        for line in lines do
            match line with
            | [ changes ; fd ; nevents ; events ; timeout ; rv ; errno ; slept ; _elapsed ] ->
                let nchanges, readable = changesOf changes

                match
                    UnixKqueue.kevent
                        1
                        (descriptor fd)
                        nchanges
                        readable
                        (int nevents)
                        (eventlistOf events)
                        (timeoutOf timeout)
                        system
                with
                | Ok (outcome, after) ->
                    try
                        agreesWith system.Machine.NanosecondsSinceBoot rv errno slept outcome system after 1
                    with e ->
                        failwith $"line %A{line}: %s{e.Message}"
                | Error refusal -> failwith $"line %A{line}: refused, %s{KeventRefusal.describe refusal}"
            | other -> failwith $"malformed probe line %A{other}"

    /// A kqueue a close has drained, the descriptor that survived the close onto it, and
    /// the system: task 1 slept in `kevent` through the closed descriptor, and has
    /// finished.
    let private drained : int * UnixSystem<int, string> =
        let k, system = createKqueue darwin
        let d, system = dup k system
        let _, system = parks 1 k KeventTimeout.Null system
        let system = close k system

        match finishes 1 system with
        | KeventOutcome.Failed UnixError.EBADF, system -> d, system
        | other -> failwith $"expected the sleeper to fail with EBADF, got %A{other}"

    [<Test>]
    let ``every wait the probe made on a drained kqueue is answered as Darwin answered it`` () : unit =
        let d, system = drained

        let lines = probeLines "F" |> List.filter (fun line -> List.length line = 6)
        List.length lines |> shouldEqual 45

        for line in lines do
            match line with
            | [ changes ; nevents ; timeout ; rv ; errno ; slept ] ->
                let nchanges, readable = changesOf changes

                match
                    UnixKqueue.kevent 2 d nchanges readable (int nevents) UserBuffer.Mapped (timeoutOf timeout) system
                with
                | Ok (outcome, after) ->
                    try
                        agreesWith system.Machine.NanosecondsSinceBoot rv errno slept outcome system after 2
                    with e ->
                        failwith $"line %A{line}: %s{e.Message}"
                | Error refusal -> failwith $"line %A{line}: refused, %s{KeventRefusal.describe refusal}"
            | other -> failwith $"malformed probe line %A{other}"

    // ------------------------------------------------------------------
    // The ladder, beyond the probe's columns
    // ------------------------------------------------------------------

    /// What a kqueue in `system`, which holds no socket, makes of the readable
    /// `changes` of a changelist of `nchanges` entries with room for `room` entries:
    /// the entries echoed, or the errno that ends the call, or a refusal. None of them
    /// can change the system: an ADD names no socket, and a DELETE nothing registered.
    let private changesWithNoSocket
        (system : UnixSystem<int, string>)
        (nchanges : int)
        (changes : Kevent list)
        (room : int)
        : Result<Result<Kevent list, UnixError * Kevent list>, KeventRefusal>
        =
        let allowed = [ 0x01us ; 0x21us ; 0x41us ; 0x61us ; 0x02us ; 0x42us ]

        let rec go (echoed : Kevent list) (room : int) (remaining : Kevent list) =
            match remaining with
            | [] when nchanges > List.length changes -> Ok (Error (UnixError.EFAULT, List.rev echoed))
            | [] -> Ok (Ok (List.rev echoed))
            | change :: rest ->
                let outcome =
                    if not (List.contains change.Flags allowed) then
                        Error (KeventRefusal.UnmodelledFlags change)
                    elif change.Filter <> -1s && change.Filter <> -2s then
                        Error (KeventRefusal.UnmodelledFilter change)
                    elif change.FilterFlags <> 0u || change.Data <> 0L then
                        Error (KeventRefusal.UnmodelledFilterParameters change)
                    elif change.Flags &&& 0x02us <> 0us then
                        Ok UnixError.ENOENT
                    else
                        match
                            FileDescriptorRegistry.tryFindTarget (int change.Ident) system.Process.FileDescriptors
                        with
                        | None -> Ok UnixError.EBADF
                        | Some target -> Error (KeventRefusal.UnmodelledTarget (change, target))

                match outcome with
                | Error refusal -> Error refusal
                | Ok error when room > 0 ->
                    let entry =
                        { change with
                            Flags = change.Flags ||| 0x4000us
                            Data = int64 (UnixError.toRawErrnoUnder RawErrnoNumbering.Darwin error)
                        }

                    go (entry :: echoed) (room - 1) rest
                | Ok error -> Ok (Error (error, List.rev echoed))

        go [] room changes

    /// What the measured ladder answers for a call that gets past the timeout and the
    /// descriptor, on a kqueue in `system` that nothing has drained and that registers
    /// nothing: `None` for a call that waits.
    let private oracle
        (system : UnixSystem<int, string>)
        (nchanges : int)
        (readable : Kevent list)
        (nevents : int)
        (timeout : KeventTimeout)
        : Result<KeventOutcome option, KeventRefusal>
        =
        match timeout with
        | KeventTimeout.Unreadable -> Ok (Some (KeventOutcome.Failed UnixError.EFAULT))
        | KeventTimeout.Readable (seconds, nanoseconds) when
            seconds < 0L
            || seconds > 2147483647L
            || nanoseconds < 0L
            || nanoseconds > 1_000_000_000L
            ->
            Ok (Some (KeventOutcome.Failed UnixError.EINVAL))
        | _ ->

        match changesWithNoSocket system nchanges readable (max nevents 0) with
        | Error refusal -> Error refusal
        | Ok (Error (error, [])) -> Ok (Some (KeventOutcome.Failed error))
        | Ok (Error (error, echoed)) -> Ok (Some (KeventOutcome.FailedAfterEchoing (error, echoed)))
        | Ok (Ok (_ :: _ as echoed)) -> Ok (Some (KeventOutcome.Echoed echoed))
        | Ok (Ok []) ->

        if nevents <= 0 then
            Ok (Some (KeventOutcome.Answered []))
        else
            match timeout with
            | KeventTimeout.Readable (0L, 0L) -> Ok (Some (KeventOutcome.Answered []))
            | _ -> Ok None

    let private change : Gen<Kevent> =
        gen {
            let! ident = Gen.choose (0, 10)
            let! filter = Gen.elements [ KeventFilter.Read ; KeventFilter.Write ; -3s ]

            let! flags =
                Gen.elements
                    [
                        KeventFlags.Add ||| KeventFlags.Clear ||| KeventFlags.Receipt
                        KeventFlags.Delete ||| KeventFlags.Receipt
                        KeventFlags.Add
                    ]

            let! data = Gen.choose (0, 3)

            return
                {
                    Ident = uint64 ident
                    Filter = filter
                    Flags = flags
                    FilterFlags = 0u
                    Data = int64 data
                    UserData = uint64 (ident * 7)
                }
        }

    let private timeoutGen : Gen<KeventTimeout> =
        let seconds =
            Gen.oneof
                [
                    Gen.choose64 (-3L, 3L)
                    Gen.elements [ 2147483647L ; 2147483648L ; Int64.MaxValue ; Int64.MinValue ]
                ]

        let nanoseconds =
            Gen.oneof
                [
                    Gen.choose64 (-2L, 2L)
                    Gen.elements [ 999_999_999L ; 1_000_000_000L ; 1_000_000_001L ; Int64.MaxValue ]
                ]

        Gen.oneof
            [
                Gen.constant KeventTimeout.Null
                Gen.constant KeventTimeout.Unreadable
                Gen.map2 (fun s ns -> KeventTimeout.Readable (s, ns)) seconds nanoseconds
            ]

    [<Test>]
    let ``on a live kqueue every call is answered by the measured ladder`` () : unit =
        let fd, system = createKqueue darwin

        let gen =
            gen {
                let! nchanges = Gen.oneof [ Gen.choose (-2, 3) ; Gen.constant Int32.MinValue ]
                let! available = Gen.listOf change
                let readable = List.truncate (max nchanges 0) available
                let! nevents = Gen.oneof [ Gen.choose (-2, 3) ; Gen.elements [ Int32.MinValue ; Int32.MaxValue ] ]
                let! timeout = timeoutGen
                return nchanges, readable, nevents, timeout
            }

        let property (nchanges : int, readable : Kevent list, nevents : int, timeout : KeventTimeout) : unit =
            let actual =
                UnixKqueue.kevent 1 fd nchanges readable nevents UserBuffer.Mapped timeout system

            match oracle system nchanges readable nevents timeout, actual with
            | Error expected, actual -> actual |> shouldEqual (Error expected)
            | Ok (Some expected), actual -> actual |> shouldEqual (Ok (expected, system))
            | Ok None, Ok (KeventOutcome.WouldBlock _, parked) ->
                match UnixTaskTable.parkedFor 1 parked.Tasks with
                | Some (ParkedSyscall.Kevent wait) ->
                    wait.Kqueue |> shouldEqual (idOf fd system)
                    wait.Fd |> shouldEqual fd
                    wait.MaxEvents |> shouldEqual nevents
                | other -> failwith $"expected a kevent park, got %A{other}"

                UnixSystem.checkInvariants parked |> shouldEqual []
            | Ok None, other -> failwith $"expected the call to park, got %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a call through a descriptor that is not a kqueue is EBADF, after the timeout and before everything else``
        ()
        : unit
        =
        let property (fd : int, timeout : KeventTimeout, nchanges : int, nevents : int) : unit =
            let expected =
                match oracle darwin 0 [] 1 timeout with
                | Ok (Some (KeventOutcome.Failed error)) when error <> UnixError.EBADF -> KeventOutcome.Failed error
                | _ -> KeventOutcome.Failed UnixError.EBADF

            // Even a readable change, which a kqueue would apply or refuse, is never reached.
            let readable = if nchanges > 0 then [ someChange ] else []

            UnixKqueue.kevent 1 fd nchanges readable nevents UserBuffer.Mapped timeout darwin
            |> shouldEqual (Ok (expected, darwin))

        // Standard input, output and error are pipe ends; 7 and -1 are not open.
        let gen =
            Gen.zip
                (Gen.zip (Gen.elements [ 0 ; 1 ; 2 ; 7 ; -1 ]) timeoutGen)
                (Gen.zip (Gen.choose (-1, 2)) (Gen.choose (-1, 2)))
            |> Gen.map (fun ((fd, timeout), (nchanges, nevents)) -> fd, timeout, nchanges, nevents)

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------
    // The timeout
    // ------------------------------------------------------------------

    [<Test>]
    let ``a positive timeout fires exactly at its deadline, and answers no events`` () : unit =
        // Measured (section C): no wait returned before its timeout, and every one returned 0.
        let property (seconds : int64, nanoseconds : int64, now : int64) : unit =
            let fd, system = createKqueue (after now darwin)

            let condition, parked =
                parks 1 fd (KeventTimeout.Readable (seconds, nanoseconds)) system

            let deadline = now + seconds * nanosecondsPerSecond + nanoseconds

            condition
            |> shouldEqual (
                WakeCondition.AnyOf (
                    WakeCondition.Primitive (WakePrimitive.KqueueEventDeliverable (idOf fd system)),
                    [
                        WakeCondition.Primitive (WakePrimitive.KqueueDrained (idOf fd system))
                        WakeCondition.Primitive (WakePrimitive.DeadlinePassed deadline)
                        WakeCondition.Primitive WakePrimitive.SignalDeliverable
                    ]
                )
            )

            UnixWait.deadlines (Set.singleton 1) parked |> shouldEqual [ deadline ]

            // One nanosecond short: nothing wakes it, and a finish attempted anyway parks again
            // on the same deadline, behind every other park.
            let short = after (deadline - 1L - now) parked
            woken [ 1 ] short |> shouldEqual []

            let ordinalBefore = (UnixTaskTable.parkOf 1 parked.Tasks).Value.Ordinal

            match UnixKqueue.finishKevent 1 short with
            | Ok (KeventOutcome.WouldBlock _, reparked) ->
                UnixWait.deadlines (Set.singleton 1) reparked |> shouldEqual [ deadline ]

                (UnixTaskTable.parkOf 1 reparked.Tasks).Value.Ordinal
                |> shouldBeGreaterThan ordinalBefore
            | other -> failwith $"expected a re-park, got %A{other}"

            let expired = after (deadline - now) parked

            woken [ 1 ] expired
            |> shouldEqual [ 1, Set.singleton (WakePrimitive.DeadlinePassed deadline) ]

            finishes 1 expired |> fst |> shouldEqual (KeventOutcome.Answered [])

        let gen =
            gen {
                let! seconds = Gen.oneof [ Gen.choose64 (0L, 3L) ; Gen.constant 2147483647L ]
                let! nanoseconds = Gen.oneof [ Gen.choose64 (0L, 1_000_000_000L) ; Gen.choose64 (0L, 3L) ]
                let! now = Gen.choose64 (0L, 1_000_000_000_000L)
                return seconds, nanoseconds, now
            }
            |> Gen.filter (fun (seconds, nanoseconds, _) -> seconds > 0L || nanoseconds > 0L)

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a null timeout waits for an event, a drain or a signal, and no deadline`` () : unit =
        let fd, system = createKqueue darwin
        let condition, parked = parks 1 fd KeventTimeout.Null system

        condition
        |> shouldEqual (
            WakeCondition.AnyOf (
                WakeCondition.Primitive (WakePrimitive.KqueueEventDeliverable (idOf fd system)),
                [
                    WakeCondition.Primitive (WakePrimitive.KqueueDrained (idOf fd system))
                    WakeCondition.Primitive WakePrimitive.SignalDeliverable
                ]
            )
        )

        match UnixTaskTable.parkedFor 1 parked.Tasks with
        | Some (ParkedSyscall.Kevent wait) ->
            wait
            |> shouldEqual
                {
                    Kqueue = idOf fd system
                    Fd = fd
                    MaxEvents = 1
                    Buffer = UserBuffer.Mapped
                    Deadline = None
                }
        | other -> failwith $"expected a kevent park, got %A{other}"

        UnixWait.deadlines (Set.singleton 1) parked |> shouldEqual []
        woken [ 1 ] (after 1_000_000_000_000_000L parked) |> shouldEqual []

    [<Test>]
    let ``a deadline past the clock's range is refused`` () : unit =
        let fd, system = createKqueue (after (Int64.MaxValue - 5L) darwin)

        waits 1 fd (KeventTimeout.Readable (0L, 6L)) system
        |> shouldEqual (Error (KeventRefusal.DeadlineBeyondClock (Int64.MaxValue - 5L, 0L, 6L)))

        // Exactly the last nanosecond is representable.
        match waits 1 fd (KeventTimeout.Readable (0L, 5L)) system with
        | Ok (KeventOutcome.WouldBlock _, _) -> ()
        | other -> failwith $"expected a park, got %A{other}"

    // ------------------------------------------------------------------
    // Closing the kqueue under a sleeper
    // ------------------------------------------------------------------

    [<Test>]
    let ``closing the descriptor a sleeper entered through ends its wait with EBADF`` () : unit =
        // Measured (section E, rows E1, E6 and E7): with no other descriptor onto the kqueue,
        // and with or without a timeout.
        for timeout in [ KeventTimeout.Null ; KeventTimeout.Readable (0L, 300_000_000L) ] do
            let k, system = createKqueue darwin
            let kqueue = idOf k system
            let _, parked = parks 1 k timeout system
            let closed = close k parked

            // The number is free again, but the sleeping call holds the kqueue, which
            // outlives its last descriptor drained.
            FileDescriptorRegistry.tryFindId k closed.Process.FileDescriptors
            |> shouldEqual None

            FileDescriptorRegistry.descriptions closed.Process.FileDescriptors
            |> Map.tryFind kqueue
            |> Option.map _.Target
            |> shouldEqual (
                Some (
                    OpenFileTarget.Kqueue
                        {
                            Drained = true
                            Registrations = Map.empty
                            Active = []
                        }
                )
            )

            UnixSystem.checkInvariants closed |> shouldEqual []

            woken [ 1 ] closed
            |> shouldEqual [ 1, Set.singleton (WakePrimitive.KqueueDrained kqueue) ]

            // The kqueue goes as the call holding it returns.
            let outcome, finished = finishes 1 closed
            outcome |> shouldEqual (KeventOutcome.Failed UnixError.EBADF)

            FileDescriptorRegistry.descriptions finished.Process.FileDescriptors
            |> Map.containsKey kqueue
            |> shouldEqual false

            UnixSystem.checkInvariants finished |> shouldEqual []

            // E7: a new kqueue on the freed number is a different kqueue.
            let k', reopened = createKqueue closed
            k' |> shouldEqual k
            idOf k' reopened |> shouldNotEqual kqueue

    /// Which descriptor, of the kqueue's own `k` and its dup `d`, each sleeper entered
    /// through.
    type private Entered =
        | ThroughK
        | ThroughD

    [<Test>]
    let ``a close drains the kqueue exactly when some sleeper entered through the closed descriptor`` () : unit =
        // Measured (section E): closing K with a sleeper through K ends every sleeper's wait
        // with EBADF, a sleeper through D included (E4, E5); closing a descriptor no sleeper
        // entered through ends nothing (E2, E3); and the drain outlives the close (F).
        let property (sleepers : Entered list, closeK : bool) : unit =
            let k, system = createKqueue darwin
            let d, system = dup k system
            let kqueue = idOf k system
            let tasks = List.mapi (fun i _ -> i + 1) sleepers

            let parked =
                (system, List.zip tasks sleepers)
                ||> List.fold (fun system (task, entered) ->
                    let fd =
                        match entered with
                        | ThroughK -> k
                        | ThroughD -> d

                    parks task fd KeventTimeout.Null system |> snd
                )

            let closing, surviving, through = if closeK then k, d, ThroughK else d, k, ThroughD
            let closed = close closing parked
            UnixSystem.checkInvariants closed |> shouldEqual []

            let drains = List.contains through sleepers

            match FileDescriptorRegistry.tryFindTarget surviving closed.Process.FileDescriptors with
            | Some (OpenFileTarget.Kqueue state) -> state.Drained |> shouldEqual drains
            | other -> failwith $"expected the surviving kqueue, got %A{other}"

            if drains then
                woken tasks closed
                |> shouldEqual (
                    tasks
                    |> List.map (fun task -> task, Set.singleton (WakePrimitive.KqueueDrained kqueue))
                )

                let finished =
                    (closed, tasks)
                    ||> List.fold (fun system task ->
                        let outcome, system = finishes task system
                        outcome |> shouldEqual (KeventOutcome.Failed UnixError.EBADF)
                        system
                    )

                // F: every later wait through the survivor that reaches the kqueue is EBADF at
                // once, whatever its timeout.
                for timeout in
                    [
                        KeventTimeout.Null
                        KeventTimeout.Readable (0L, 0L)
                        KeventTimeout.Readable (1L, 0L)
                    ] do
                    waits 9 surviving timeout (Tasks.ensure 9 finished)
                    |> shouldEqual (Ok (KeventOutcome.Failed UnixError.EBADF, Tasks.ensure 9 finished))
            else
                woken tasks closed |> shouldEqual []

        let gen =
            Gen.zip
                (Gen.listOf (Gen.elements [ ThroughK ; ThroughD ]) |> Gen.map (List.truncate 4))
                (ArbMap.defaults |> ArbMap.generate<bool>)

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a drained kqueue outlives its last descriptor until the last call holding it returns`` () : unit =
        // Every sleeper holds the kqueue, so closing both its descriptors destroys nothing:
        // the drain ends every wait, and the kqueue goes as the last of them returns.
        let property (first : Entered, rest : Entered list) : unit =
            let sleepers = first :: rest
            let k, system = createKqueue darwin
            let d, system = dup k system
            let kqueue = idOf k system
            let tasks = List.mapi (fun i _ -> i + 1) sleepers

            let parked =
                (system, List.zip tasks sleepers)
                ||> List.fold (fun system (task, entered) ->
                    let fd =
                        match entered with
                        | ThroughK -> k
                        | ThroughD -> d

                    parks task fd KeventTimeout.Null system |> snd
                )

            let closed = parked |> close k |> close d
            UnixSystem.checkInvariants closed |> shouldEqual []

            FileDescriptorRegistry.descriptions closed.Process.FileDescriptors
            |> Map.tryFind kqueue
            |> Option.map _.Target
            |> shouldEqual (
                Some (
                    OpenFileTarget.Kqueue
                        {
                            Drained = true
                            Registrations = Map.empty
                            Active = []
                        }
                )
            )

            woken tasks closed
            |> shouldEqual (
                tasks
                |> List.map (fun task -> task, Set.singleton (WakePrimitive.KqueueDrained kqueue))
            )

            let finished =
                (closed, tasks)
                ||> List.fold (fun system task ->
                    let outcome, system = finishes task system
                    outcome |> shouldEqual (KeventOutcome.Failed UnixError.EBADF)
                    UnixSystem.checkInvariants system |> shouldEqual []

                    FileDescriptorRegistry.descriptions system.Process.FileDescriptors
                    |> Map.containsKey kqueue
                    |> shouldEqual (task < List.length tasks)

                    system
                )

            FileDescriptorRegistry.descriptions finished.Process.FileDescriptors
            |> Map.containsKey kqueue
            |> shouldEqual false

        let entered = Gen.elements [ ThroughK ; ThroughD ]

        let gen = Gen.zip entered (Gen.listOf entered |> Gen.map (List.truncate 3))

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a drain and an expired deadline together are refused, as is a signal beside either`` () : unit =
        // Darwin answers whichever reached the sleeper first, which nothing records.
        let k, system = createKqueue darwin
        let d, system = dup k system
        let kqueue = idOf k system
        let _, parked = parks 1 k (KeventTimeout.Readable (0L, 5L)) system

        let both = close k parked |> after 5L

        UnixKqueue.finishKevent 1 both
        |> shouldEqual (Error (KeventRefusal.DrainBesideDeadline kqueue))

        match UnixKqueue.finishKevent 1 (signalled 1 (close k parked)) with
        | Error (KeventRefusal.Interruption (SyscallInterruptionRefusal.SignalBesideCompletion SimulatedUnixFlavour.Darwin)) ->
            ()
        | other -> failwith $"expected the signal beside the drain to be refused, got %A{other}"

        match UnixKqueue.finishKevent 1 (signalled 1 (after 5L parked)) with
        | Error (KeventRefusal.Interruption (SyscallInterruptionRefusal.SignalBesideCompletion SimulatedUnixFlavour.Darwin)) ->
            ()
        | other -> failwith $"expected the signal beside the deadline to be refused, got %A{other}"

        ignore<int> d

    // ------------------------------------------------------------------
    // Signals
    // ------------------------------------------------------------------

    [<Test>]
    let ``a caught signal ends the wait with EINTR, timed or not`` () : unit =
        // Measured (section D): EINTR under SA_RESTART and without, NULL timeout and 2 s.
        for timeout in [ KeventTimeout.Null ; KeventTimeout.Readable (2L, 0L) ] do
            let fd, system = createKqueue darwin
            let _, parked = parks 1 fd timeout system
            let interrupted = signalled 1 parked

            woken [ 1 ] interrupted
            |> shouldEqual [ 1, Set.singleton WakePrimitive.SignalDeliverable ]

            finishes 1 interrupted
            |> fst
            |> shouldEqual (KeventOutcome.Failed UnixError.EINTR)

        SyscallInterruption.ruleOf (
            ParkedSyscall.Kevent
                {
                    Kqueue = OpenFileDescriptionId 0L
                    Fd = 3
                    MaxEvents = 1
                    Buffer = UserBuffer.Mapped
                    Deadline = None
                }
        )
        |> shouldEqual SignalRestartRule.FailsWithEintr

    // ------------------------------------------------------------------
    // Misuse, and forged states
    // ------------------------------------------------------------------

    [<Test>]
    let ``a parked task cannot wait again, and an unparked one cannot finish`` () : unit =
        let fd, system = createKqueue darwin
        let _, parked = parks 1 fd KeventTimeout.Null system

        let exn =
            Assert.Throws<exn> (fun () -> waits 1 fd KeventTimeout.Null parked |> ignore)

        exn.Message |> shouldContainText "is parked"

        let exn = Assert.Throws<exn> (fun () -> UnixKqueue.finishKevent 1 system |> ignore)
        exn.Message |> shouldContainText "is not parked"

        let exn =
            Assert.Throws<exn> (fun () ->
                UnixKqueue.kevent 1 fd 1 [ someChange ; someChange ] 1 UserBuffer.Mapped KeventTimeout.Null system
                |> ignore
            )

        exn.Message |> shouldContainText "at most nchanges"

    [<Test>]
    let ``checkInvariants rejects a kevent park no call could have made`` () : unit =
        let k, system = createKqueue darwin
        let kqueue = idOf k system

        let forged (park : ParkedKevent) (system : UnixSystem<int, string>) =
            UnixSystem.checkInvariants (UnixWait.park 1 (ParkedSyscall.Kevent park) system)

        let honest =
            {
                Kqueue = kqueue
                Fd = k
                MaxEvents = 1
                Buffer = UserBuffer.Mapped
                Deadline = None
            }

        forged honest system |> shouldEqual []

        forged
            { honest with
                MaxEvents = 0
            }
            system
        |> shouldEqual [ UnixSystemDefect.ParkedKeventCountNotPositive (1, 0) ]

        // Entered through stdin, which names a pipe end, not the kqueue.
        forged
            { honest with
                Fd = 0
            }
            system
        |> shouldEqual
            [
                UnixSystemDefect.ParkedKeventDescriptorRebound (1, 0, kqueue, Some (idOf 0 system))
            ]

        // ...which a drain excuses: the descriptor it entered through may have gone.
        let d, withDup = dup k system
        let drained = close k (UnixWait.park 1 (ParkedSyscall.Kevent honest) withDup)
        UnixSystem.checkInvariants drained |> shouldEqual []
        ignore<int> d

        forged
            { honest with
                Kqueue = idOf 0 system
                Fd = 0
            }
            system
        |> shouldEqual
            [
                UnixSystemDefect.ParkedKeventOnNonKqueue (
                    1,
                    idOf 0 system,
                    (FileDescriptorRegistry.tryFindTarget 0 system.Process.FileDescriptors).Value
                )
            ]

    [<Test>]
    let ``checkInvariants rejects an object the flavour's kernel does not have`` () : unit =
        let fd, registry = FileDescriptorRegistry.createKqueue linux.Process.FileDescriptors
        let withKqueue = withRegistry registry linux

        UnixSystem.checkInvariants withKqueue
        |> shouldEqual
            [
                UnixSystemDefect.DescriptionNotOfFlavour (
                    idOf fd withKqueue,
                    OpenFileTarget.Kqueue
                        {
                            Drained = false
                            Registrations = Map.empty
                            Active = []
                        },
                    SimulatedUnixFlavour.Linux
                )
            ]

        let fd, registry = FileDescriptorRegistry.createEpoll darwin.Process.FileDescriptors
        let withEpoll = withRegistry registry darwin

        match UnixSystem.checkInvariants withEpoll with
        | [ UnixSystemDefect.DescriptionNotOfFlavour (id, OpenFileTarget.Epoll _, SimulatedUnixFlavour.Darwin) ] ->
            id |> shouldEqual (idOf fd withEpoll)
        | other -> failwith $"expected the epoll instance to be reported, got %A{other}"
