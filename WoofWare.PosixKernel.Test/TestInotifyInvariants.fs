namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The inotify instance's invariants (`FileDescriptorRegistryDefect` and
/// `UnixSystemDefect`), each tripped by a forged state; the limits as boot
/// configuration, counted per user across processes; the Darwin refusals; and
/// `UnixSystem.checkInvariants` holding after every call of generated
/// sequences.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyInvariants =

    /// Fails unless `value` is at most `limit`.
    let private atMost<'a when 'a : comparison> (limit : 'a) (value : 'a) : unit =
        if value > limit then
            failwith $"%A{value} is above %A{limit}"

    let private context : string = "TestInotifyInvariants"

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private bits (mode : int) : PermissionBits = PermissionBits.parseOrFail context mode

    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList
            [
                name "d1", SeedEntry.Directory (Map.empty, bits 0o755, None)
                name "d2", SeedEntry.Directory (Map.empty, bits 0o755, None)
                name "f", SeedEntry.File (ImmutableArray.CreateRange "x"B, bits 0o644, None)
            ]

    let private bootWith
        (configure : UnixBootImage<int, string> -> UnixBootImage<int, string>)
        (platform : SimulatedUnixPlatform)
        : UnixSystem<int, string>
        =
        UnixSystem.initial<int, string> platform
        |> configure
        |> Launched.fileSystem
            (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
            {
                User = UnixSystem.defaultUserId (SimulatedUnixPlatform.flavour platform)
                Group = UnixSystem.defaultGroupId (SimulatedUnixPlatform.flavour platform)
            }
            seed
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> fun system -> ([ 1 ; 2 ], system) ||> List.foldBack Tasks.ensure

    let private linux : UnixSystem<int, string> =
        bootWith id SimulatedUnixPlatform.linuxX64

    let private withLimits (limits : InotifyLimits) (image : UnixBootImage<int, string>) : UnixBootImage<int, string> =
        UnixBootImage.tryWithInotifyLimits limits image
        |> Configured.expectOk InotifyLimitsRefusal.describe

    let private init1 (flags : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixInotify.tryInotifyInit1 flags system with
        | Ok (Ok created) -> created
        | other -> failwith $"inotify_init1: %A{other}"

    let private add (fd : int) (path : string) (mask : uint32) (system : UnixSystem<int, string>) =
        match UnixInotify.tryInotifyAddWatch fd (PathArg.ofText path) mask system with
        | Ok answered -> answered
        | Error refusal -> failwith $"add: %s{InotifyAddWatchRefusal.describe refusal}"

    let private idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
        | Some id -> id
        | None -> failwith $"fd %d{fd} names nothing"

    /// An instance at fd 3 watching `d1` (wd 1) for `IN_CREATE`.
    let private instance : UnixSystem<int, string> * OpenFileDescriptionId * InotifyState =
        let fd, system = init1 0x800 linux
        let _, system = add fd "d1" 0x100u system
        let id = idOf fd system
        system, id, OpenFileTable.inotifyState context id system.Machine.OpenFiles

    let private forge (state : InotifyState) : UnixSystem<int, string> =
        let system, id, _ = instance
        UnixSystemState.mapOpenFiles (OpenFileTable.setInotifyState context id state) system

    let private registryDefects (system : UnixSystem<int, string>) : FileDescriptorRegistryDefect list =
        FileDescriptorRegistry.checkInvariants (UnixSystemState.fileDescriptors system)

    [<Test>]
    let ``an instance made by the calls has no defect`` () : unit =
        let system, _, _ = instance
        registryDefects system |> shouldEqual []
        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``each of an instance's own rules is tripped by a state that breaks it alone`` () : unit =
        let _, id, state = instance
        let watch = WatchDescriptor 1
        let held = state.Watches.[watch]

        let cases : (string * InotifyState * FileDescriptorRegistryDefect list) list =
            [
                "next not positive",
                { state with
                    NextWatchDescriptor = 0
                    Watches = Map.empty
                },
                [ FileDescriptorRegistryDefect.InotifyWatchDescriptorNotFresh (id, 0, 0) ]

                "a watch at the next descriptor",
                { state with
                    NextWatchDescriptor = 1
                },
                [ FileDescriptorRegistryDefect.InotifyWatchDescriptorNotFresh (id, 1, 1) ]

                "a record past the next descriptor",
                { state with
                    Queue = InotifyQueue.ofList [ InotifyRecord.Ignored (WatchDescriptor 5) ]
                },
                [ FileDescriptorRegistryDefect.InotifyWatchDescriptorNotFresh (id, 2, 5) ]

                "an unmodelled event stored",
                { state with
                    Watches =
                        Map.add
                            watch
                            { held with
                                Mask =
                                    { held.Mask with
                                        Events = Set.add InotifyEvent.Open held.Mask.Events
                                    }
                            }
                            state.Watches
                },
                [
                    FileDescriptorRegistryDefect.InotifyUnmodelledEvent (id, watch, InotifyEvent.Open)
                ]

                "two overflow records",
                { state with
                    Queue = InotifyQueue.ofList [ InotifyRecord.QueueOverflow ; InotifyRecord.QueueOverflow ]
                },
                [ FileDescriptorRegistryDefect.InotifyOverflowRecordDuplicated (id, 2) ]

                "an IN_IGNORED for a live watch",
                { state with
                    Queue = InotifyQueue.ofList [ InotifyRecord.Ignored watch ]
                },
                [ FileDescriptorRegistryDefect.InotifyIgnoredLiveWatch (id, watch) ]

                "an event record of no event",
                { state with
                    Queue = InotifyQueue.ofList [ InotifyRecord.Event (watch, Set.empty, false, 0u, None) ]
                },
                [ FileDescriptorRegistryDefect.InotifyEventRecordEmpty (id, watch) ]

                "two watches of one inode",
                { state with
                    Watches = Map.add (WatchDescriptor 2) held state.Watches
                    NextWatchDescriptor = 3
                },
                [ FileDescriptorRegistryDefect.InotifyInodeWatchedTwice (id, held.Inode) ]
            ]

        for case, forged, expected in cases do
            (case, registryDefects (forge forged)) |> shouldEqual (case, expected)

    [<Test>]
    let ``a watch on an inode the filesystem no longer holds is a defect`` () : unit =
        let _, _, state = instance
        let held = state.Watches.[WatchDescriptor 1]

        let forged =
            forge
                { state with
                    Watches =
                        Map.add
                            (WatchDescriptor 2)
                            { held with
                                Inode = InodeNumber 999L
                            }
                            state.Watches
                    NextWatchDescriptor = 3
                }

        // The defect's cases are internal to the library, and named here by
        // its description, which renders the case.
        match UnixSystem.checkInvariants forged |> List.map UnixSystemDefect.describe with
        | [ described ] ->
            if
                not (described.Contains "InotifyWatchOfForgottenInode")
                || not (described.Contains "999")
            then
                failwith $"the defect reported is not the watch of a forgotten inode: %s{described}"
        | other -> failwith $"expected one defect, the watch of a forgotten inode; got %A{other}"

    [<Test>]
    let ``an instance open for writing is a defect`` () : unit =
        let system, id, _ = instance

        let forged =
            UnixSystemState.mapOpenFiles
                (OpenFileTable.mapDescription
                    context
                    id
                    (fun description ->
                        { description with
                            AccessMode = FileAccessMode.ReadWrite
                        }
                    ))
                system

        registryDefects forged
        |> shouldEqual [ FileDescriptorRegistryDefect.InotifyNotReadOnly id ]

    let private withMachineLimits
        (limits : InotifyLimits)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        { system with
            Machine =
                { system.Machine with
                    InotifyLimits = limits
                }
        }

    [<Test>]
    let ``the machine's limits are held to the instances and watches each user holds`` () : unit =
        let fd, system = init1 0x800 linux
        let _, system = add fd "d1" 0x100u system
        let _, system = add fd "d2" 0x100u system
        let _, system = init1 0 system
        let id = idOf fd system
        let user = system.Process.Credentials.EffectiveUser
        UnixSystem.checkInvariants system |> shouldEqual []
        let limits = system.Machine.InotifyLimits

        UnixSystem.checkInvariants (
            withMachineLimits
                { limits with
                    MaxUserInstances = 1
                }
                system
        )
        |> shouldEqual [ UnixSystemDefect.InotifyInstancesBeyondLimit (user, 2, 1) ]

        UnixSystem.checkInvariants (
            withMachineLimits
                { limits with
                    MaxUserWatches = 1
                }
                system
        )
        |> shouldEqual [ UnixSystemDefect.InotifyWatchesBeyondLimit (user, 2, 1) ]

        let notAdmissible =
            { limits with
                MaxQueuedEvents = 0
            }

        UnixSystem.checkInvariants (withMachineLimits notAdmissible system)
        |> shouldEqual [ UnixSystemDefect.InotifyLimitsNotAdmissible notAdmissible ]

        let state = OpenFileTable.inotifyState context id system.Machine.OpenFiles

        let overfull =
            UnixSystemState.mapOpenFiles
                (OpenFileTable.setInotifyState
                    context
                    id
                    { state with
                        Queue =
                            InotifyQueue.ofList
                                [
                                    for name in [ "a" ; "b" ; "c" ] ->
                                        InotifyRecord.Event (
                                            WatchDescriptor 1,
                                            Set.singleton InotifyEvent.Create,
                                            false,
                                            0u,
                                            Some (DirectoryEntryName.parseOrFail context name)
                                        )
                                ]
                    })
                system

        UnixSystem.checkInvariants (
            withMachineLimits
                { limits with
                    MaxQueuedEvents = 1
                }
                overfull
        )
        |> shouldEqual [ UnixSystemDefect.InotifyQueueBeyondLimit (id, 3, 1) ]

    [<Test>]
    let ``a read parked on something other than an instance is a defect`` () : unit =
        let fd, system =
            match UnixPoll.tryEpollCreate1 0 linux with
            | Ok (Ok created) -> created
            | other -> failwith $"%A{other}"

        let id = idOf fd system

        let parked =
            ForgedPark.onAbsent
                1
                (ParkedSyscall.InotifyRead
                    {
                        Instance = id
                        Buffer = UserBuffer.Mapped
                        Count = 64
                    })
                system

        let target = (OpenFileTable.get context id parked.Machine.OpenFiles).Target

        UnixSystem.checkInvariants parked
        |> shouldEqual [ UnixSystemDefect.ParkedInotifyReadOnNonInotify (1, id, target) ]

    [<Test>]
    let ``an instance under the Darwin flavour is a defect, and every call is refused`` () : unit =
        let darwin = bootWith id SimulatedUnixPlatform.macOsArm64

        UnixInotify.tryInotifyInit1 0 darwin
        |> shouldEqual (Error (InotifyRefusal.Flavour SimulatedUnixFlavour.Darwin))

        UnixInotify.tryInotifyAddWatch 0 (PathArg.ofText "d1") 0x100u darwin
        |> shouldEqual (Error (InotifyAddWatchRefusal.Flavour SimulatedUnixFlavour.Darwin))

        UnixInotify.tryInotifyRmWatch 0 1 darwin
        |> shouldEqual (Error (InotifyRefusal.Flavour SimulatedUnixFlavour.Darwin))

        match UnixSystem.tryStep 0 (Syscall.InotifyInit1 0) darwin with
        | Error (SyscallRefusal.Inotify (InotifyRefusal.Flavour SimulatedUnixFlavour.Darwin)) -> ()
        | other -> failwith $"%A{other}"

        UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64
        |> UnixBootImage.tryWithInotifyLimits UnixSystem.defaultInotifyLimits
        |> Result.map ignore
        |> shouldEqual (Error (InotifyLimitsRefusal.NotReadOn SimulatedUnixFlavour.Darwin))

        let fd, registry =
            FileDescriptorRegistry.createInotify UserId.root false (UnixSystemState.fileDescriptors darwin)

        let forged = UnixSystemState.withFileDescriptors registry darwin
        let id = idOf fd forged

        UnixSystem.checkInvariants forged
        |> shouldEqual
            [
                UnixSystemDefect.DescriptionNotOfFlavour (
                    id,
                    (OpenFileTable.get context id forged.Machine.OpenFiles).Target,
                    SimulatedUnixFlavour.Darwin
                )
            ]

    [<Test>]
    let ``the limits are boot configuration, each positive and the queue's countable`` () : unit =
        let image = UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64

        let limits =
            {
                MaxUserInstances = 128
                MaxUserWatches = 8192
                MaxQueuedEvents = 16384
            }

        UnixSystem.defaultInotifyLimits |> shouldEqual limits
        linux.Machine.InotifyLimits |> shouldEqual limits

        let refused (limits : InotifyLimits) =
            match UnixBootImage.tryWithInotifyLimits limits image with
            | Ok _ -> None
            | Error refusal -> Some refusal

        refused
            { limits with
                MaxUserInstances = 0
            }
        |> shouldEqual (Some (InotifyLimitsRefusal.MaxUserInstancesNotPositive 0))

        refused
            { limits with
                MaxUserWatches = -1
            }
        |> shouldEqual (Some (InotifyLimitsRefusal.MaxUserWatchesNotPositive -1))

        refused
            { limits with
                MaxQueuedEvents = 0
            }
        |> shouldEqual (Some (InotifyLimitsRefusal.MaxQueuedEventsNotPositive 0))

        refused
            { limits with
                MaxQueuedEvents = InotifyLimits.largestQueuedEvents
            }
        |> shouldEqual None

        refused
            { limits with
                MaxQueuedEvents = InotifyLimits.largestQueuedEvents + 1
            }
        |> shouldEqual (
            Some (
                InotifyLimitsRefusal.MaxQueuedEventsTooLarge (
                    InotifyLimits.largestQueuedEvents + 1,
                    InotifyLimits.largestQueuedEvents
                )
            )
        )

        // A full queue of the longest records, and its overflow record, fit
        // in what FIONREAD reports.
        int64 InotifyLimits.largestQueuedEvents * 272L + 16L
        |> atMost (int64 System.Int32.MaxValue)

        int64 (InotifyLimits.largestQueuedEvents + 1) * 272L + 16L
        |> shouldBeGreaterThan (int64 System.Int32.MaxValue)

        let configured =
            image
            |> withLimits
                {
                    MaxUserInstances = 2
                    MaxUserWatches = 1
                    MaxQueuedEvents = 3
                }
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        configured.Machine.InotifyLimits
        |> shouldEqual
            {
                MaxUserInstances = 2
                MaxUserWatches = 1
                MaxQueuedEvents = 3
            }

    [<Test>]
    let ``a user's instances and watches count against the limits in every process on the machine`` () : unit =
        let pids, machine =
            Machines.withTasksOn
                (withLimits
                    {
                        MaxUserInstances = 3
                        MaxUserWatches = 2
                        MaxQueuedEvents = 16
                    })
                SimulatedUnixPlatform.linuxX64
                2
                1

        let a, b =
            match pids with
            | [ a ; b ] -> a, b
            | other -> failwith $"%A{other}"

        let initIn (pid : ProcessId) (machine : SimulatedMachine<int, string>) =
            machine
            |> Machines.inProcess
                pid
                (fun view ->
                    match UnixInotify.tryInotifyInit1 0x800 view with
                    | Ok (Ok (fd, view)) -> Ok fd, view
                    | Ok (Error error) -> Error error, view
                    | Error refusal -> failwith $"%A{refusal}"
                )

        let fdA, machine = initIn a machine
        let _, machine = initIn a machine
        let fdB, machine = initIn b machine
        let refused, machine = initIn b machine
        refused |> shouldEqual (Error UnixError.EMFILE)

        let addIn (pid : ProcessId) (fd : int) (path : string) (machine : SimulatedMachine<int, string>) =
            machine
            |> Machines.inProcess
                pid
                (fun view ->
                    match UnixInotify.tryInotifyAddWatch fd (PathArg.ofText path) 0x100u view with
                    | Ok answered -> answered
                    | Error refusal -> failwith $"%A{refusal}"
                )

        let fdA =
            match fdA with
            | Ok fd -> fd
            | Error e -> failwith $"%A{e}"

        let fdB =
            match fdB with
            | Ok fd -> fd
            | Error e -> failwith $"%A{e}"

        let first, machine = addIn a fdA "/" machine
        first |> shouldEqual (SyscallAnswer.Completed 1L)
        let second, machine = addIn b fdB "/" machine
        second |> shouldEqual (SyscallAnswer.Completed 1L)
        let third, machine = addIn b fdB "/dev" machine
        third |> shouldEqual (SyscallAnswer.Failed UnixError.ENOSPC)
        // The refused add spent its descriptor.
        let again, machine = addIn a fdA "/dev" machine
        again |> shouldEqual (SyscallAnswer.Failed UnixError.ENOSPC)
        SimulatedMachine.checkInvariants machine |> shouldEqual []

        let state =
            match
                FileDescriptorRegistry.tryFindTarget fdB (UnixSystemState.fileDescriptors (Machines.viewOf b machine))
            with
            | Some (OpenFileTarget.Inotify state) -> state
            | other -> failwith $"%A{other}"

        state.NextWatchDescriptor |> shouldEqual 3

    [<Test>]
    let ``step answers inotify_init1 and inotify_rm_watch as the functions do`` () : unit =
        let viaStep =
            match UnixSystem.tryStep 0 (Syscall.InotifyInit1 0x800) linux with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed fd), system) -> int fd, system
            | other -> failwith $"%A{other}"

        viaStep |> shouldEqual (init1 0x800 linux)

        match UnixSystem.tryStep 0 (Syscall.InotifyInit1 0x1) linux with
        | Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EINVAL), system) -> system |> shouldEqual linux
        | other -> failwith $"%A{other}"

        let system, _, _ = instance

        match UnixSystem.tryStep 0 (Syscall.InotifyRmWatch (3, 1)) system, UnixInotify.tryInotifyRmWatch 3 1 system with
        | Ok (SyscallOutcome.Answered stepped, afterStep), Ok (answered, after) ->
            stepped |> shouldEqual answered
            afterStep |> shouldEqual after
        | other -> failwith $"%A{other}"

    /// One call of a generated sequence, on the instance in a slot.
    [<RequireQualifiedAccess>]
    type private Op =
        | Init of nonBlocking : bool
        | Add of slot : int * path : string * mask : uint32
        | Remove of slot : int * wd : int
        | Read of slot : int * count : int * mapped : bool
        | Dup of slot : int
        | Close of slot : int
        | Signal

    [<RequireQualifiedAccess>]
    type private Label =
        | Overflowed
        | ReaderSlept
        | ReaderWokenByRecord
        | ReaderInterrupted
        | ReadTookRecords
        | ClosedUnderSleepingReader

    [<Test>]
    let ``every call of a generated sequence leaves the system's invariants holding`` () : unit =
        let image =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64
            |> withLimits
                {
                    MaxUserInstances = 3
                    MaxUserWatches = 4
                    MaxQueuedEvents = 1
                }

        let start =
            image
            |> Launched.fileSystem
                (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                {
                    User = UnixSystem.defaultUserId SimulatedUnixFlavour.Linux
                    Group = UnixSystem.defaultGroupId SimulatedUnixFlavour.Linux
                }
                seed
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> Tasks.ensure 1
            |> fun system ->
                { system with
                    Process =
                        { system.Process with
                            Signals =
                                SignalState.setDisposition
                                    Signal.SIGUSR1
                                    (SignalDisposition.Catch (SignalCatch.ofHandler "h"))
                                    system.Process.Signals
                        }
                }

        let opGen : Gen<Op> =
            let slot = Gen.choose (0, 3)

            Gen.frequency
                [
                    2, Gen.elements [ true ; false ] |> Gen.map Op.Init
                    5,
                    Gen.map3
                        (fun s p m -> Op.Add (s, p, m))
                        slot
                        (Gen.elements [ "d1" ; "d2" ; "f" ; "/" ; "missing" ])
                        (Gen.elements
                            [
                                0x100u
                                0x2u ||| 0x4u
                                0x100u ||| 0x20000000u
                                0x100u ||| 0x10000000u
                                0u
                            ])
                    4, Gen.map2 (fun s w -> Op.Remove (s, w)) slot (Gen.choose (0, 4))
                    3,
                    Gen.map3
                        (fun s c m -> Op.Read (s, c, m))
                        slot
                        (Gen.elements [ 0 ; 16 ; 64 ])
                        (Gen.frequency [ 4, Gen.constant true ; 1, Gen.constant false ])
                    1, slot |> Gen.map Op.Dup
                    1, slot |> Gen.map Op.Close
                    1, Gen.constant Op.Signal
                ]

        let property (cover : Label -> unit) (ops : Op list) : unit =
            // Each slot: a descriptor of the process's leader, or none.
            let slots = Array.create 4 None
            let mutable system = start
            // The reader, task 1, asleep in a read if it is.
            let mutable asleep = false

            let check (op : Op) =
                match UnixSystem.checkInvariants system with
                | [] -> ()
                | defects -> failwith $"after %A{op}: %A{defects}"

            let finishReader () =
                match UnixWait.satisfied 1 system with
                | Some woken when not (Set.isEmpty woken) ->
                    match UnixReadWrite.tryFinishRead 1 system with
                    | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EINTR), after) ->
                        cover Label.ReaderInterrupted
                        system <- after
                        asleep <- false
                    | Ok (ReadOutcome.Answered _, after) ->
                        cover Label.ReaderWokenByRecord
                        system <- after
                        asleep <- false
                    | Ok (ReadOutcome.WouldBlock _, after) -> system <- after
                    | other -> failwith $"finishing the read: %A{other}"

                    // Its handler runs and returns, as the client would see to.
                    match UnixSignal.tryOnReturnToUser 1 system with
                    | Ok (ProcessFate.Continues (ReturnToUserOutcome.Resumes, after)) -> system <- after
                    | Ok (ProcessFate.Continues (ReturnToUserOutcome.RunHandlers frames, after)) ->
                        system <-
                            (after, frames)
                            ||> List.fold (fun system frame -> UnixSignal.sigreturn 1 frame.Id system)
                    | other -> failwith $"%A{other}"
                | _ -> ()

            for op in ops do
                match op with
                | Op.Init nonBlocking ->
                    match Array.tryFindIndex Option.isNone slots with
                    | None -> ()
                    | Some free ->
                        match UnixInotify.tryInotifyInit1 (if nonBlocking then 0x800 else 0) system with
                        | Ok (Ok (fd, after)) ->
                            system <- after
                            slots.[free] <- Some fd
                        | Ok (Error UnixError.EMFILE) -> ()
                        | other -> failwith $"%A{other}"
                | Op.Add (slot, path, mask) ->
                    let fd = slots.[slot] |> Option.defaultValue 99

                    match UnixInotify.tryInotifyAddWatch fd (PathArg.ofText path) mask system with
                    | Ok (_, after) -> system <- after
                    | Error refusal -> failwith $"%A{refusal}"
                | Op.Remove (slot, wd) ->
                    let fd = slots.[slot] |> Option.defaultValue 99

                    match UnixInotify.tryInotifyRmWatch fd wd system with
                    | Ok (_, after) ->
                        system <- after

                        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
                        | Some (OpenFileTarget.Inotify state) when
                            InotifyQueue.toList state.Queue |> List.contains InotifyRecord.QueueOverflow
                            ->
                            cover Label.Overflowed
                        | _ -> ()
                    | Error refusal -> failwith $"%A{refusal}"
                | Op.Read (slot, count, mapped) ->
                    let fd = slots.[slot] |> Option.defaultValue 99

                    let buffer =
                        if mapped then
                            UserBuffer.Mapped
                        else
                            UserBuffer.Unmapped 0UL
                    // The reader, task 1, reads, and sleeps if it must; while
                    // it sleeps, the leader reads only what it need not sleep
                    // for.
                    let leaderWouldSleep =
                        match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system) with
                        | Some {
                                   Target = OpenFileTarget.Inotify state
                                   NonBlocking = false
                               } -> InotifyQueue.isEmpty state.Queue
                        | _ -> false

                    if asleep && leaderWouldSleep then
                        ()
                    else

                    match UnixReadWrite.tryRead (if asleep then 0 else 1) fd buffer (uint64 count) system with
                    | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                        if bytes.Length > 0 then
                            cover Label.ReadTookRecords

                        system <- after
                    | Ok (ReadOutcome.Answered (ReadAnswer.Failed _), after) -> system <- after
                    | Ok (ReadOutcome.WouldBlock _, after) ->
                        if asleep then
                            failwith "the leader's read slept"

                        cover Label.ReaderSlept
                        asleep <- true
                        system <- after
                    | other -> failwith $"%A{other}"
                | Op.Dup slot ->
                    match slots.[slot], Array.tryFindIndex Option.isNone slots with
                    | Some fd, Some free ->
                        match UnixDescriptor.tryDup fd system with
                        | Ok (SyscallAnswer.Completed copy, after) ->
                            system <- after
                            slots.[free] <- Some (int copy)
                        | other -> failwith $"%A{other}"
                    | _ -> ()
                | Op.Close slot ->
                    match slots.[slot] with
                    | None -> ()
                    | Some fd ->
                        if asleep then
                            cover Label.ClosedUnderSleepingReader

                        match UnixDescriptor.tryClose fd system with
                        | Ok (_, after) ->
                            system <- after
                            slots.[slot] <- None
                        | Error refusal -> failwith $"%A{refusal}"
                | Op.Signal ->
                    if asleep then
                        system <-
                            { system with
                                Process =
                                    { system.Process with
                                        Signals =
                                            SignalState.enqueue
                                                {
                                                    Signal = Signal.SIGUSR1
                                                    Target = ValueSome 1
                                                }
                                                system.Process.Signals
                                    }
                            }

                check op
                finishReader ()
                check op

        let gen =
            Gen.listOf opGen |> Gen.resize 40 |> Gen.map (fun ops -> Op.Init false :: ops)

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 500) (Arb.fromGen gen) property

        for label in
            [
                Label.Overflowed
                Label.ReaderSlept
                Label.ReaderWokenByRecord
                Label.ReaderInterrupted
                Label.ReadTookRecords
                Label.ClosedUnderSleepingReader
            ] do
            if coverage.Count label < 10 then
                failwith $"the fixed sample reached %A{label} only %d{coverage.Count label} times"

    // ------------------------------------------------ the name a description was opened through

    /// A machine holding `d/f`, `d/g` and the directory `d/e`, with `d/f`
    /// opened through its name for reading and writing: the system, the
    /// description, and the inodes of `d` and `d/f`.
    let private openedThroughName () : UnixSystem<int, string> * OpenFileDescriptionId * InodeNumber * InodeNumber =
        let system =
            InotifyReplay.boot
                InotifyReplay.root
                [
                    "d",
                    InotifyReplay.directory
                        0o755
                        [
                            "f", InotifyReplay.file "x"
                            "g", InotifyReplay.file "y"
                            "e", InotifyReplay.directory 0o755 []
                        ]
                ]

        let fd, system =
            InotifyReplay.openFd (InotifyReplay.openFlags FileAccessMode.ReadWrite false false false) "d/f" 0 system

        system, idOf fd system, InotifyReplay.numberOf "d" system, InotifyReplay.numberOf "d/f" system

    /// `system` with the description `id` recorded as opened through `through`.
    let private openedThroughForged
        (id : OpenFileDescriptionId)
        (through : ReachedThrough option)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        UnixSystemState.withFileDescriptors
            (FileDescriptorRegistry.Unchecked.setOpenedThrough id through (UnixSystemState.fileDescriptors system))
            system

    [<Test>]
    let ``each way the name a description was opened through can disagree with the graph is a defect`` () : unit =
        let system, id, d, f = openedThroughName ()
        UnixSystem.checkInvariants system |> shouldEqual []

        let named (entry : string) (unlinked : bool) : ReachedThrough option =
            Some (
                ReachedThrough.Name (
                    {
                        Parent = d
                        Name = name entry
                    },
                    unlinked
                )
            )

        openedThroughForged id (named "g" false) system
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.OpenedThroughNameUnbound (id, f, d, name "g") ]

        openedThroughForged
            id
            (Some (
                ReachedThrough.Name (
                    {
                        Parent = InodeNumber 999L
                        Name = name "f"
                    },
                    true
                )
            ))
            system
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.OpenedThroughParentGone (id, InodeNumber 999L) ]

        openedThroughForged id (Some ReachedThrough.Unnamed) system
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.OpenedThroughNoNameNotRoot (id, f) ]

        openedThroughForged id (Some (ReachedThrough.RemovedDirectory d)) system
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.OpenedThroughRemovedDirectoryNamed (id, f, d) ]

        // An unlinked name the directory still binds to the inode is no defect:
        // the name may have been given again (`inotify-dentry-probe.c` section
        // L).
        openedThroughForged id (named "f" true) system
        |> UnixSystem.checkInvariants
        |> shouldEqual []

    [<Test>]
    let ``descriptions holding one name record it alike, and two recording one linked name hold one name`` () : unit =
        let system, first, d, f = openedThroughName ()
        let system = InotifyReplay.link "d/f" "d/h" system

        let open' (path : string) (system : UnixSystem<int, string>) =
            let fd, system =
                InotifyReplay.openFd (InotifyReplay.openFlags FileAccessMode.ReadOnly false false false) path 0 system

            idOf fd system, system

        let named (entry : string) : ReachedThrough option =
            Some (
                ReachedThrough.Name (
                    {
                        Parent = d
                        Name = name entry
                    },
                    false
                )
            )

        // Opened through `d/f` again: one name with the first.
        let again, shared = open' "d/f" system
        registryDefects shared |> shouldEqual []

        openedThroughForged again (named "h") shared
        |> registryDefects
        |> shouldEqual [ FileDescriptorRegistryDefect.OpenedThroughNameNotShared (first, again) ]

        // Opened through `d/h`, another name of the file.
        let other, distinct = open' "d/h" system
        registryDefects distinct |> shouldEqual []
        UnixSystem.checkInvariants distinct |> shouldEqual []

        openedThroughForged other (named "f") distinct
        |> registryDefects
        |> shouldEqual [ FileDescriptorRegistryDefect.OpenedThroughNameNotShared (first, other) ]

        ignore f

    [<Test>]
    let ``a description onto an inode without the name it was opened through, or another with one, is a defect``
        ()
        : unit
        =
        let system, id, _, f = openedThroughName ()

        openedThroughForged id None system
        |> registryDefects
        |> shouldEqual
            [
                FileDescriptorRegistryDefect.OpenedThroughMismatch (id, OpenFileTarget.File (f, 0L))
            ]

        let instance, system = init1 0x800 system
        let instanceId = idOf instance system
        let target = (OpenFileTable.get context instanceId system.Machine.OpenFiles).Target

        openedThroughForged instanceId (Some ReachedThrough.Unnamed) system
        |> registryDefects
        |> shouldEqual [ FileDescriptorRegistryDefect.OpenedThroughMismatch (instanceId, target) ]

    [<Test>]
    let ``a description is made onto an inode with the name it was opened through, and onto nothing else with one``
        ()
        : unit
        =
        let description (target : OpenFileTarget) : OpenFileDescription =
            {
                Target = target
                AccessMode = FileAccessMode.ReadOnly
                NonBlocking = false
                Flock = None
                Status = OpenFileStatus.none
            }

        Assert.Throws<exn> (fun () ->
            OpenFileTable.create (description (OpenFileTarget.File (InodeNumber 5L, 0L))) None OpenFileTable.empty
            |> ignore<OpenFileDescriptionId * OpenFileTable>
        )
        |> ignore<exn>

        Assert.Throws<exn> (fun () ->
            OpenFileTable.create
                (description (OpenFileTarget.Socket (SocketId 0L)))
                (Some ReachedThrough.Unnamed)
                OpenFileTable.empty
            |> ignore<OpenFileDescriptionId * OpenFileTable>
        )
        |> ignore<exn>

    [<Test>]
    let ``a name unlinked and linked again to the same inode leaves the description's name unlinked`` () : unit =
        // Measured (`inotify-dentry-probe.c` section L): the name given again is
        // another name, and a write through the description opened by the old
        // one is kept out of a watch with IN_EXCL_UNLINK.
        let system, id, d, _ = openedThroughName ()

        let system =
            system
            |> InotifyReplay.link "d/f" "d/h"
            |> InotifyReplay.unlink "d/f"
            |> InotifyReplay.link "d/h" "d/f"

        OpenFileTable.openedThrough id system.Machine.OpenFiles
        |> shouldEqual (
            Some (
                ReachedThrough.Name (
                    {
                        Parent = d
                        Name = name "f"
                    },
                    true
                )
            )
        )

        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``a rename moves the name a description was opened through, and a rmdir of a directory's name unlinks it``
        ()
        : unit
        =
        let system, id, d, _ = openedThroughName ()
        let e = InotifyReplay.numberOf "d/e" system

        let directoryFd, system =
            InotifyReplay.openFd (InotifyReplay.openFlags FileAccessMode.ReadOnly false false false) "d/e" 0 system

        let directoryId = idOf directoryFd system

        let system =
            system
            |> InotifyReplay.rename "d/f" "d/e/moved"
            |> InotifyReplay.rename "d/e" "d/e2"

        OpenFileTable.openedThrough id system.Machine.OpenFiles
        |> shouldEqual (
            Some (
                ReachedThrough.Name (
                    {
                        Parent = e
                        Name = name "moved"
                    },
                    false
                )
            )
        )

        let system =
            system |> InotifyReplay.unlink "d/e2/moved" |> InotifyReplay.rmdir "d/e2"

        OpenFileTable.openedThrough directoryId system.Machine.OpenFiles
        |> shouldEqual (
            Some (
                ReachedThrough.Name (
                    {
                        Parent = d
                        Name = name "e2"
                    },
                    true
                )
            )
        )

        UnixSystem.checkInvariants system |> shouldEqual []
