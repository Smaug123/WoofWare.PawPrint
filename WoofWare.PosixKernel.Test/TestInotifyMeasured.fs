namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixInotify` and the calls that reach an inotify instance, replayed against
/// what Linux 6.18.5 aarch64 answered in docs/plans/2026-10-11-inotify:
/// `inotify-probe.c` sections I, W, R, P, A, B and S, `inotify-instance-probe.c`
/// sections F, G, M, U, Q, T and X, and `inotify-descriptor-probe.c` sections D,
/// N and S. Each test drives the model through the probe's calls, renders each
/// row as the probe printed it, and compares the rows with the recorded ones.
///
/// Where the probe changed the filesystem to make a record, the replay queues
/// that record on the instance directly (`queue`), so that these rows measure
/// reading, readiness, waking and the queue alone; which change makes which
/// record is replayed in `TestInotifyDeliveryMeasured`. A watch that asked for
/// the events this kernel does not raise (`IN_ALL_EVENTS`) asks instead for the
/// ones it does; the rows record watch descriptors, not events.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyMeasured =

    let private recorded (logicalName : string) : (string * string * string) list =
        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream logicalName with
            | null -> failwith $"no embedded resource %s{logicalName}"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.choose (fun line ->
            match line.Split '\t' with
            | [| section ; case ; result |] -> Some (section, case, result)
            | _ -> None
        )

    let private probe = lazy recorded "WoofWare.PosixKernel.Test.inotify.linux.txt"

    let private instanceProbe =
        lazy recorded "WoofWare.PosixKernel.Test.inotifyInstance.linux.txt"

    let private descriptorProbe =
        lazy recorded "WoofWare.PosixKernel.Test.inotifyDescriptor.linux.txt"

    /// The rows of `section`, but those whose case is in `skipped`, each a
    /// case and what it reported.
    let private rows
        (file : Lazy<(string * string * string) list>)
        (section : string)
        (skipped : string list)
        : (string * string) list
        =
        let all =
            file.Value
            |> List.filter (fun (s, _, _) -> s = section)
            |> List.map (fun (_, case, result) -> case, result)

        // A skip names a row that exists, so a renamed row cannot pass by
        // being skipped.
        for case in skipped do
            if not (List.exists (fun (c, _) -> c = case) all) then
                failwith $"section %s{section} has no row %s{case} to skip"

        all |> List.filter (fun (case, _) -> not (List.contains case skipped))

    let private context : string = "TestInotifyMeasured"

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private bits (mode : int) : PermissionBits = PermissionBits.parseOrFail context mode
    let private group (gid : uint32) : GroupId = GroupId.parseOrFail context gid

    let private directory (mode : int) (entries : (string * SeedEntry) list) : SeedEntry =
        SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, bits mode, None)

    let private file (mode : int) : SeedEntry =
        SeedEntry.File (ImmutableArray.CreateRange "x"B, bits mode, None)

    let private symlink (target : string) : SeedEntry =
        SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

    let private rootOwner : InodeOwner =
        {
            User = UserId.root
            Group = group 0u
        }

    /// The probes ran as root.
    let private root : Credentials = Credentials.ofIds UserId.root (group 0u) []

    /// A machine like the one measured: Linux 6.18.5 aarch64, the measuring
    /// VM's `fs.inotify` sysctls, `seed` at the root, and a process of
    /// `credentials` with tasks 0 to 2.
    let private boot (credentials : Credentials) (seed : (string * SeedEntry) list) : UnixSystem<int, string> =
        UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxArm64
        |> UnixBootImage.tryWithInotifyLimits
            {
                MaxUserInstances = 128
                MaxUserWatches = 8982
                MaxQueuedEvents = 16384
            }
        |> Configured.expectOk InotifyLimitsRefusal.describe
        |> Launched.fileSystem
            (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
            rootOwner
            (seed |> List.map (fun (n, e) -> name n, e) |> Map.ofList)
        |> Launched.bootWith (Launched.credentials credentials) UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> fun system -> ([ 1 ; 2 ], system) ||> List.foldBack Tasks.ensure

    /// `IN_CLOEXEC` and `IN_NONBLOCK`.
    let private cloexec : int = 0x80000
    let private nonblock : int = 0x800

    let private create : uint32 = 0x100u
    let private delete : uint32 = 0x200u
    let private attrib : uint32 = 0x4u
    let private modify : uint32 = 0x2u
    let private onlyDir : uint32 = 0x01000000u
    let private dontFollow : uint32 = 0x02000000u
    let private maskAdd : uint32 = 0x20000000u
    let private maskCreate : uint32 = 0x10000000u

    /// `IN_ALL_EVENTS` without the events this kernel does not raise.
    let private modelledEvents : uint32 = 0xFC6u

    /// The probe's `rc`.
    let private rc (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed value -> $"%d{value}"
        | SyscallAnswer.Failed error -> $"-1 %O{error}"

    let private init1 (flags : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixInotify.tryInotifyInit1 flags system with
        | Ok (Ok created) -> created
        | other -> failwith $"inotify_init1(0x%x{flags}): %A{other}"

    let private initAnswer (flags : int) (system : UnixSystem<int, string>) : SyscallAnswer * UnixSystem<int, string> =
        match UnixInotify.tryInotifyInit1 flags system with
        | Ok (Ok (fd, system)) -> SyscallAnswer.Completed (int64 fd), system
        | Ok (Error error) -> SyscallAnswer.Failed error, system
        | Error refusal -> failwith $"inotify_init1(0x%x{flags}) refused: %s{InotifyRefusal.describe refusal}"

    let private addWatch
        (fd : int)
        (path : string)
        (mask : uint32)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixInotify.tryInotifyAddWatch fd (PathArg.ofText path) mask system with
        | Ok answered -> answered
        | Error refusal ->
            failwith
                $"inotify_add_watch(%d{fd}, %s{path}, 0x%x{mask}) refused: %s{InotifyAddWatchRefusal.describe refusal}"

    let private rmWatch
        (fd : int)
        (wd : int)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixInotify.tryInotifyRmWatch fd wd system with
        | Ok answered -> answered
        | Error refusal -> failwith $"inotify_rm_watch(%d{fd}, %d{wd}) refused: %s{InotifyRefusal.describe refusal}"

    let private close (fd : int) (system : UnixSystem<int, string>) : SyscallAnswer * UnixSystem<int, string> =
        match UnixDescriptor.tryClose fd system with
        | Ok answered -> answered
        | Error refusal -> failwith $"close(%d{fd}): %A{refusal}"

    let private idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
        | Some id -> id
        | None -> failwith $"fd %d{fd} names no description"

    /// The record the probe's filesystem change queued on the instance `fd`
    /// names, queued directly.
    let private queue (fd : int) (record : InotifyRecord) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixSystemState.mapOpenFiles
            (OpenFileTable.enqueueInotify system.Machine.InotifyLimits.MaxQueuedEvents (idOf fd system) record)
            system

    let private event (wd : int) (events : uint32) (entry : string) : InotifyRecord =
        let events =
            InotifyEvent.all
            |> List.filter (fun event -> InotifyEvent.bit event &&& events <> 0u)
            |> Set.ofList

        InotifyRecord.Event (WatchDescriptor wd, events, false, 0u, Some (name entry))

    /// The probe's `maskNames`.
    let private maskNames (mask : uint32) : string =
        let names =
            [
                0x1u, "ACCESS"
                0x2u, "MODIFY"
                0x4u, "ATTRIB"
                0x8u, "CLOSE_WRITE"
                0x10u, "CLOSE_NOWRITE"
                0x20u, "OPEN"
                0x40u, "MOVED_FROM"
                0x80u, "MOVED_TO"
                0x100u, "CREATE"
                0x200u, "DELETE"
                0x400u, "DELETE_SELF"
                0x800u, "MOVE_SELF"
                0x2000u, "UNMOUNT"
                0x4000u, "Q_OVERFLOW"
                0x8000u, "IGNORED"
                0x40000000u, "ISDIR"
            ]

        let named = names |> List.filter (fun (bit, _) -> mask &&& bit <> 0u)
        let seen = named |> List.fold (fun acc (bit, _) -> acc ||| bit) 0u
        let text = named |> List.map snd |> String.concat "+"

        if mask &&& ~~~seen <> 0u then
            failwith $"mask 0x%x{mask} has bits the probe would print raw"
        elif text = "" then
            "0"
        else
            text

    /// The probe's `renderEvents`: each record of `bytes`, as it printed one.
    let private render (bytes : ImmutableArray<byte>) : string list =
        let bytes = bytes.AsSpan().ToArray ()

        let rec go (pos : int) (acc : string list) =
            if pos + 16 > bytes.Length then
                if pos <> bytes.Length then
                    failwith $"%d{bytes.Length - pos} trailing bytes"

                List.rev acc
            else
                let wd = BinaryPrimitives.ReadInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos, 4))

                let mask =
                    BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos + 4, 4))

                let cookie =
                    BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos + 8, 4))

                let len =
                    int (BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos + 12, 4)))

                if cookie <> 0u then
                    failwith "a cookie these rows never record"

                let entry =
                    if len = 0 then
                        ""
                    else
                        let raw = bytes.[pos + 16 .. pos + 16 + len - 1]
                        let nul = Array.IndexOf (raw, 0uy)
                        Text.Encoding.UTF8.GetString (raw, 0, nul)

                go
                    (pos + 16 + len)
                    ($"wd=%d{wd} mask=%s{maskNames mask} cookie=0 len=%d{len} name=\"%s{entry}\""
                     :: acc)

        go 0 []

    let private read
        (task : int)
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<int, string>)
        : ReadOutcome * UnixSystem<int, string>
        =
        match UnixReadWrite.tryRead task fd buffer (uint64 count) system with
        | Ok outcome -> outcome
        | Error refusal -> failwith $"read(%d{fd}, %d{count}) refused: %s{ReadRefusal.describe refusal}"

    /// A read that returns, as the probe's `rc` prints it, and its records.
    let private readNow
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<int, string>)
        : string * string list * UnixSystem<int, string>
        =
        match read 0 fd buffer count system with
        | ReadOutcome.Answered (ReadAnswer.Completed bytes), system -> $"%d{bytes.Length}", render bytes, system
        | ReadOutcome.Answered (ReadAnswer.Failed error), system -> $"-1 %O{error}", [], system
        | other, _ -> failwith $"read(%d{fd}, %d{count}): %A{other}"

    /// The probe's `drain`.
    let private drain (fd : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        let rec go (acc : string list) (system : UnixSystem<int, string>) =
            match readNow fd UserBuffer.Mapped 65536 system with
            | "-1 EAGAIN", _, system -> acc, system
            | _, records, system -> go (acc @ records) system

        match go [] system with
        | [], system -> "(none)", system
        | records, system -> String.concat " | " records, system

    let private fionread (fd : int) (system : UnixSystem<int, string>) : string =
        match UnixDescriptor.tryBytesAvailable fd UserBuffer.Mapped system with
        | Ok (BytesAvailableAnswer.Reported count) -> $"%d{count}"
        | other -> failwith $"FIONREAD on %d{fd}: %A{other}"

    let private pollIn (fd : int) (events : int16) (system : UnixSystem<int, string>) : string =
        match
            UnixPoll.tryPoll
                0
                [
                    {
                        Fd = fd
                        Events = events
                    }
                ]
                0
                system
        with
        | Ok (PollOutcome.Answered ([ revents ], count), _) -> $"poll=%d{count} revents=0x%x{revents}"
        | other -> failwith $"poll(%d{fd}): %A{other}"

    let private pollAll : int16 = 0x1s ||| 0x4s ||| 0x2s ||| 0x2000s

    [<Test>]
    let ``section I: the descriptor`` () : unit =
        let mutable system = boot root []
        let a, s = init1 cloexec system
        let b, s = init1 (cloexec ||| nonblock) s

        let ep, s =
            match UnixPoll.tryEpollCreate1 0 s with
            | Ok (Ok created) -> created
            | other -> failwith $"%A{other}"

        system <- s

        let actual = ResizeArray<string * string> ()

        // fstat: refused, since this kernel holds no inode for an event queue;
        // what the row measures of identity is the flock object, the same for
        // both instances and the epoll instance (the flock rows below).
        for fd in [ a ; b ; ep ] do
            UnixPathResolution.tryFstat fd system
            |> shouldEqual (Error FStatRefusal.EventQueue)

        let objectOf (fd : int) =
            FileDescriptorRegistry.tryFindObject fd (UnixSystemState.fileDescriptors system)

        objectOf a |> shouldEqual (Some OpenFileObject.AnonymousInode)
        objectOf b |> shouldEqual (objectOf a)
        objectOf ep |> shouldEqual (objectOf a)

        let flock (fd : int) (operation : int) =
            match UnixDescriptor.tryFlock 0 fd operation system with
            | Ok (SyscallOutcome.Answered answer, s) ->
                system <- s
                rc answer
            | other -> failwith $"flock(%d{fd}, %d{operation}): %A{other}"

        let first = flock ep 6
        let second = flock a 6
        actual.Add ("flock", $"epoll LOCK_EX|LOCK_NB=%s{first}; then inotify LOCK_EX|LOCK_NB=%s{second}")
        flock ep 8 |> shouldEqual "0"
        let first = flock a 6
        let second = flock b 6

        actual.Add (
            "flock-after-unlock",
            $"inotify LOCK_EX|LOCK_NB=%s{first}; second inotify LOCK_EX|LOCK_NB=%s{second}"
        )

        let lseek (offset : int64) (whence : int) =
            match UnixDescriptor.tryLseek a offset whence system with
            | Ok (answer, _) -> rc answer
            | Error refusal -> failwith $"%A{refusal}"

        actual.Add (
            "lseek",
            $"SEEK_SET 0=%s{lseek 0L 0} SEEK_CUR 0=%s{lseek 0L 1} SEEK_END 0=%s{lseek 0L 2} SEEK_SET 5=%s{lseek 5L 0}"
        )

        let fcntl (fd : int) (command : int) : int64 =
            match FcntlWorld.fcntl fd command 0 system with
            | SyscallAnswer.Completed value, _ -> value
            | other -> failwith $"%A{other}"

        let fl = fcntl a 3
        let fdFlags = fcntl a 1
        let flb = fcntl b 3
        let octal (value : int64) = Convert.ToString (value, 8)

        actual.Add (
            "fcntl",
            $"IN_CLOEXEC: F_GETFL=0%s{octal fl} (O_NONBLOCK=%d{(if fl &&& 0x800L <> 0L then 1 else 0)} accmode=%d{fl &&& 3L}) F_GETFD=%d{fdFlags}; IN_CLOEXEC|IN_NONBLOCK: F_GETFL=0%s{octal flb} (O_NONBLOCK=%d{(if flb &&& 0x800L <> 0L then 1 else 0)})"
        )

        let polled =
            match
                UnixPoll.tryPoll
                    0
                    [
                        {
                            Fd = a
                            Events = pollAll
                        }
                    ]
                    0
                    system
            with
            | Ok (PollOutcome.Answered ([ revents ], count), _) -> $"%d{count} revents=0x%x{revents}"
            | other -> failwith $"%A{other}"

        actual.Add ("empty", $"poll(IN|OUT|PRI|RDHUP, 0)=%s{polled}; FIONREAD=0 avail=%s{fionread a system}")

        let answer, _, s = readNow b UserBuffer.Mapped 64 system
        system <- s
        actual.Add ("read-nonblocking-empty", $"read(64)=%s{answer}")

        let written =
            match
                WriteOutcomes.admitThenWrite 0 a UserBuffer.Mapped (ImmutableArray.CreateRange "x"B) system
                |> WriteOutcomes.returned
            with
            | Ok (WriteAnswer.Failed error, _) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        actual.Add ("write", $"write(inotify, 1 byte)=%s{written}")

        List.ofSeq actual |> shouldEqual (rows probe "I" [ "fstat" ; "fstat-values" ])

    [<Test>]
    let ``section W: watches`` () : unit =
        let seed =
            [
                "w",
                directory
                    0o755
                    [
                        "d1", directory 0o755 []
                        "d2", directory 0o755 []
                        "f", file 0o644
                        "l", symlink "d1"
                    ]
            ]

        let mutable system = boot root seed
        let fd, s = init1 nonblock system
        system <- s

        let add (path : string) (mask : uint32) : string =
            let answer, s = addWatch fd path mask system
            system <- s
            rc answer

        let actual = ResizeArray<string * string> ()
        let wd1 = add "w/d1" modelledEvents
        let wd2 = add "w/d2" modelledEvents
        let again = add "w/d1" modelledEvents
        let dotted = add "./w/d1/." modelledEvents
        actual.Add ("numbering", $"d1=%s{wd1} d2=%s{wd2} d1-again=%s{again} ./w/d1/.=%s{dotted}")
        let drained, s = drain fd system
        system <- s
        actual.Add ("events-from-adding", drained)
        actual.Add ("onlydir-file", add "w/f" (modelledEvents ||| onlyDir))
        actual.Add ("file", add "w/f" modelledEvents)
        actual.Add ("missing", add "w/missing" modelledEvents)
        actual.Add ("mask-0", add "w/d1" 0u)
        actual.Add ("mask-flags-only", $"""ONLYDIR alone=%s{add "w/d2" onlyDir}""")
        actual.Add ("symlink-followed", $"""add w/l (no DONT_FOLLOW)=%s{add "w/l" modelledEvents} (d1's wd=%s{wd1})""")
        let wl = add "w/l" (modelledEvents ||| dontFollow)
        actual.Add ("symlink-dont-follow", $"add w/l DONT_FOLLOW=%s{wl}")

        actual.Add (
            "symlink-dont-follow-onlydir",
            $"""add w/l DONT_FOLLOW|ONLYDIR=%s{add "w/l" (modelledEvents ||| dontFollow ||| onlyDir)}"""
        )

        let badFd =
            match UnixInotify.tryInotifyAddWatch 999 (PathArg.ofText "w/d1") modelledEvents system with
            | Ok (answer, _) -> rc answer
            | Error refusal -> failwith $"%A{refusal}"

        let regular, s =
            match UnixNamespace.tryOpenPath 0 0 (PathArg.ofText "w/f") 0 system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed fd), s) -> int fd, s
            | other -> failwith $"%A{other}"

        system <- s

        let onRegular =
            match UnixInotify.tryInotifyAddWatch regular (PathArg.ofText "w/d1") modelledEvents system with
            | Ok (answer, _) -> rc answer
            | Error refusal -> failwith $"%A{refusal}"

        actual.Add ("bad-fd", $"add on fd 999=%s{badFd}; add on a regular file's fd=%s{onRegular}")

        // The masks each add stores, checked here; and the changes the probe
        // made under each, of which the first two rows record a write's
        // `IN_MODIFY`, which this kernel refuses under a watch asking for it
        // (it is made through a descriptor), and the first an `IN_OPEN`, which
        // no watch here asks for.
        let maskOf (wd : int) =
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Inotify state) -> state.Watches.[WatchDescriptor wd].Mask.Events
            | other -> failwith $"%A{other}"

        add "w/d2" modify |> shouldEqual "2"
        maskOf 2 |> shouldEqual (Set.singleton InotifyEvent.Modify)

        system <-
            system
            |> InotifyReplay.writeFileRefused "w/d2/x" "1"
            |> InotifyReplay.chmod "w/d2/x" 0o600

        let _, s = drain fd system
        system <- s
        add "w/d2" (attrib ||| maskAdd) |> shouldEqual "2"

        maskOf 2
        |> shouldEqual (Set.ofList [ InotifyEvent.Modify ; InotifyEvent.Attrib ])

        system <-
            system
            |> InotifyReplay.chmod "w/d2/x" 0o644
            |> InotifyReplay.writeFileRefused "w/d2/x" "2"

        let _, s = drain fd system
        system <- s
        let m3 = add "w/d2" create
        maskOf 2 |> shouldEqual (Set.singleton InotifyEvent.Create)

        system <-
            system
            |> InotifyReplay.chmod "w/d2/x" 0o600
            |> InotifyReplay.writeFile "w/d2/y" "1"

        let drained, s = drain fd system
        system <- s
        actual.Add ("mask-replaced-again", $"wd=%s{m3}; chmod then create under CREATE: %s{drained}")

        actual.Add ("mask-create-existing", $"""add d2 with IN_MASK_CREATE=%s{add "w/d2" (create ||| maskCreate)}""")

        let rm (wd : int) =
            let answer, s = rmWatch fd wd system
            system <- s
            rc answer

        let first = rm 1
        let second = rm 1
        let large = rm 999
        let negative = rm -1
        actual.Add ("rm", $"rm wd1=%s{first}; rm wd1 again=%s{second}; rm 999=%s{large}; rm -1=%s{negative}")
        let drained, s = drain fd system
        system <- s
        actual.Add ("rm-events", drained)
        actual.Add ("re-add-after-rm", $"""d1=%s{add "w/d1" modelledEvents} (was %s{wd1}, highest so far %s{wl})""")
        let drained, s = drain fd system
        system <- s
        actual.Add ("re-add-events", drained)

        List.ofSeq actual
        |> shouldEqual (rows probe "W" [ "mask-replace" ; "mask-add" ])

    [<Test>]
    let ``IN_ALL_EVENTS is refused, naming the events this kernel does not raise, where the add would succeed``
        ()
        : unit
        =
        let system = boot root [ "w", directory 0o755 [ "d1", directory 0o755 [] ] ]
        let fd, system = init1 nonblock system

        UnixInotify.tryInotifyAddWatch fd (PathArg.ofText "w/d1") 0xFFFu system
        |> shouldEqual (
            Error (
                InotifyAddWatchRefusal.UnmodelledEvents
                    [
                        InotifyEvent.Access
                        InotifyEvent.CloseWrite
                        InotifyEvent.CloseNoWrite
                        InotifyEvent.Open
                    ]
            )
        )

        // An add that fails anyway answers its errno.
        match UnixInotify.tryInotifyAddWatch fd (PathArg.ofText "w/missing") 0xFFFu system with
        | Ok (SyscallAnswer.Failed UnixError.ENOENT, _) -> ()
        | other -> failwith $"%A{other}"

        UnixInotify.tryInotifyAddWatch fd (PathArg.ofText "w/d1") (create ||| 0x80000000u) system
        |> shouldEqual (Error InotifyAddWatchRefusal.OneShot)

        UnixInotify.tryInotifyAddWatch fd (PathArg.ofText "w/d1") (create ||| 0x2000u) system
        |> shouldEqual (Error (InotifyAddWatchRefusal.UnmodelledEvents [ InotifyEvent.Unmount ]))

    [<Test>]
    let ``section R: read`` () : unit =
        let mutable system = boot root [ "r", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        system <- s
        let _, s = addWatch fd "r" create system
        system <- queue fd (event 1 create "abc") s

        let readCount (buffer : UserBuffer) (count : int) =
            let answer, _, s = readNow fd buffer count system
            system <- s
            answer

        let actual = ResizeArray<string * string> ()
        actual.Add ("count-0-with-event-queued", $"read(0)=%s{readCount UserBuffer.Mapped 0}")
        actual.Add ("count-15", $"read(15)=%s{readCount UserBuffer.Mapped 15}")
        actual.Add ("count-16", $"read(16)=%s{readCount UserBuffer.Mapped 16}")
        actual.Add ("count-31", $"read(31)=%s{readCount UserBuffer.Mapped 31}")
        actual.Add ("fionread-one-event", fionread fd system)
        actual.Add ("count-32", $"read(32)=%s{readCount UserBuffer.Mapped 32}")
        system <- system |> queue fd (event 1 create "def") |> queue fd (event 1 create "ghi")
        actual.Add ("fionread-two-events", fionread fd system)
        actual.Add ("count-48-of-64", $"read(48)=%s{readCount UserBuffer.Mapped 48}")
        actual.Add ("count-1024-rest", $"read(1024)=%s{readCount UserBuffer.Mapped 1024}")
        actual.Add ("count-0-empty", $"read(0)=%s{readCount UserBuffer.Mapped 0}")
        actual.Add ("count-1024-empty", $"read(1024)=%s{readCount UserBuffer.Mapped 1024}")
        system <- queue fd (event 1 create "jkl") system
        actual.Add ("null-buffer", $"read(NULL, 1024)=%s{readCount (UserBuffer.Unmapped 0UL) 1024}")
        actual.Add ("then-valid", $"read(1024)=%s{readCount UserBuffer.Mapped 1024}")

        List.ofSeq actual |> shouldEqual (rows probe "R" [])

    [<Test>]
    let ``section P: poll and epoll`` () : unit =
        let mutable system = boot root [ "p", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let _, s = addWatch fd "p" create s

        let ep, s =
            match UnixPoll.tryEpollCreate1 0 s with
            | Ok (Ok created) -> created
            | other -> failwith $"%A{other}"

        system <- s
        let actual = ResizeArray<string * string> ()

        // The probe's registration is level-triggered, which this library's
        // epoll does not model (`EpollCtlRefusal`), so it is made
        // edge-triggered here; the rows below up to `epoll-level` read the
        // same either way.
        let added =
            match
                UnixPoll.tryEpollCtl
                    ep
                    1
                    fd
                    (EpollEventArgument.Readable (
                        EpollEvents.In
                        ||| EpollEvents.Out
                        ||| EpollEvents.RdHup
                        ||| EpollEvents.Pri
                        ||| EpollEvents.EdgeTriggered,
                        7UL
                    ))
                    system
            with
            | Ok (EpollCtlAnswer.Changed, s) ->
                system <- s
                "0"
            | other -> failwith $"%A{other}"

        actual.Add ("epoll-ctl-add", added)

        let wait () =
            match UnixPoll.tryEpollWait 0 ep 4 UserBuffer.Mapped 0 system with
            | Ok (EpollWaitOutcome.Answered events, s) ->
                system <- s
                events
            | other -> failwith $"%A{other}"

        actual.Add ("epoll-wait-empty", $"%d{List.length (wait ())}")
        system <- queue fd (event 1 create "x") system
        actual.Add ("poll-with-event", pollIn fd pollAll system)

        let events = wait ()

        let reported =
            match events with
            | (data, mask) :: _ -> $"n=%d{events.Length} events=0x%x{mask} data=%d{data}"
            | [] -> "n=0 events=0x0 data=0"

        actual.Add ("epoll-wait-with-event", reported)
        let _, s = drain fd system
        system <- s
        actual.Add ("poll-after-drain", pollIn fd pollAll system)
        actual.Add ("poll-out-only", pollIn fd 0x4s system)

        List.ofSeq actual |> shouldEqual (rows probe "P" [ "epoll-level" ])

    [<Test>]
    let ``section A: two instances and a dup`` () : unit =
        let mutable system = boot root [ "a", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let fd2, s = init1 nonblock s

        let d, s =
            match UnixDescriptor.tryDup fd s with
            | Ok (SyscallAnswer.Completed d, s) -> int d, s
            | other -> failwith $"%A{other}"

        let w1, s = addWatch fd "a" create s
        let w2, s = addWatch fd2 "a" create s
        let wd, s = addWatch d "a" (create ||| delete) s
        system <- s
        let actual = ResizeArray<string * string> ()
        actual.Add ("wds", $"instance1=%s{rc w1} instance2=%s{rc w2} via-dup=%s{rc wd}")

        system <-
            system
            |> queue fd (event 1 create "x")
            |> queue fd2 (event 1 create "x")
            |> queue fd (event 1 delete "x")

        let drained, s = drain d system
        system <- s
        actual.Add ("instance1-via-dup", drained)
        let drained, s = drain fd system
        system <- s
        actual.Add ("instance1-original-after-dup-drained", drained)
        let drained, s = drain fd2 system
        system <- s
        actual.Add ("instance2", drained)
        let _, s = close d system
        system <- queue fd (event 1 create "y") s
        let drained, s = drain fd system
        system <- s
        actual.Add ("after-dup-closed", drained)

        List.ofSeq actual |> shouldEqual (rows probe "A" [])

    /// SIGUSR1 caught by a handler, restarting or not.
    let private catching (restart : bool) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals =
                        SignalState.setDisposition
                            Signal.SIGUSR1
                            (SignalDisposition.Catch
                                { SignalCatch.ofHandler "h" with
                                    Restart = restart
                                })
                            system.Process.Signals
                }
        }

    let private signalTask (task : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
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

    let private returned (task : int) (system : UnixSystem<int, string>) : bool =
        match UnixWait.satisfied task system with
        | Some woken -> not (Set.isEmpty woken)
        | None -> failwith $"task %d{task} is not asleep"

    /// What the woken read of `task` answers, as the probe prints it.
    let private finish (task : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        match UnixReadWrite.tryFinishRead task system with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), s) ->
            $"""result=%d{bytes.Length}  events=%s{String.concat " | " (render bytes)}""", s
        | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), s) -> $"result=-1 %O{error}", s
        | Ok (ReadOutcome.Restarts, s) -> "restarts", s
        | other -> failwith $"%A{other}"

    let private sleeps (task : int) (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match read task fd UserBuffer.Mapped 4096 system with
        | ReadOutcome.WouldBlock _, system -> system
        | other, _ -> failwith $"expected the read to sleep, got %A{other}"

    [<Test>]
    let ``section B: a reader asleep`` () : unit =
        let reader = 1
        let mutable system = boot root [ "b", directory 0o755 [] ] |> catching false
        let fd, s = init1 cloexec system
        let _, s = addWatch fd "b" create s
        system <- sleeps reader fd s
        let actual = ResizeArray<string * string> ()

        actual.Add (
            "blocked-before-event",
            $"thread returned=%d{(if returned reader system then 1 else 0)} after 100 ms"
        )

        system <- queue fd (event 1 create "x") system
        let woken = returned reader system
        let answer, s = finish reader system
        system <- s
        actual.Add ("event", $"returned=%d{(if woken then 1 else 0)} %s{answer}")
        system <- sleeps reader fd system
        actual.Add ("blocked-again", $"returned=%d{(if returned reader system then 1 else 0)}")
        let _, s = rmWatch fd 1 system
        system <- s
        let woken = returned reader system
        let answer, s = finish reader system
        system <- s
        actual.Add ("rm-watch-wakes", $"returned=%d{(if woken then 1 else 0)} %s{answer}")
        system <- sleeps reader fd system
        actual.Add ("no-watches", $"returned after 150 ms=%d{(if returned reader system then 1 else 0)}")
        let closed, s = close fd system
        system <- s

        actual.Add (
            "close-under-read",
            $"close=%s{rc closed}; thread returned after 150 ms=%d{(if returned reader system then 1 else 0)}"
        )

        system <- signalTask reader system
        let woken = returned reader system
        let answer, s = finish reader system
        system <- s
        actual.Add ("signal", $"returned=%d{(if woken then 1 else 0)} %s{answer}")

        // Under SA_RESTART: the read restarts, and the client issues it again.
        system <- catching true system
        let fd, s = init1 cloexec system
        let _, s = addWatch fd "b" create s
        system <- sleeps reader fd s |> signalTask reader

        let restarted, s = finish reader system
        restarted |> shouldEqual "restarts"

        // The handler runs and returns, and the client issues the read again.
        let s =
            match UnixSignal.tryOnReturnToUser reader s with
            | Ok (ReturnToUserOutcome.RunHandlers (frames, s)) ->
                (s, frames)
                ||> List.fold (fun s frame -> UnixSignal.sigreturn reader frame.Id s)
            | other -> failwith $"%A{other}"

        system <- sleeps reader fd s
        actual.Add ("signal-sa-restart", $"returned=%d{(if returned reader system then 1 else 0)}")
        system <- queue fd (event 1 create "y") system
        let woken = returned reader system
        let answer, s = finish reader system
        system <- s
        actual.Add ("event-after-restart", $"""returned=%d{(if woken then 1 else 0)} %s{answer.Replace ("  ", " ")}""")

        List.ofSeq actual |> shouldEqual (rows probe "B" [])

    [<Test>]
    let ``section S: the per-user instance limit, one instance held elsewhere`` () : unit =
        // The measuring VM's init process held one instance, as root.
        let mutable system = boot root []
        let _, s = init1 0 system
        system <- s

        let rec count (n : int) =
            match initAnswer cloexec system with
            | SyscallAnswer.Completed _, s ->
                system <- s
                count (n + 1)
            | SyscallAnswer.Failed error, _ -> n, error

        let created, error = count 0

        [ "instances", $"created %d{created} before %O{error} (uid 0) ~boot" ]
        |> shouldEqual (rows probe "S" [ "watches" ])

    [<Test>]
    let ``section F: inotify_init1's flags`` () : unit =
        let system = boot root []

        let accepted =
            [ 0..31 ]
            |> List.choose (fun bit ->
                match UnixInotify.tryInotifyInit1 (1 <<< bit) system with
                | Ok (Ok _) -> Some $"0%s{Convert.ToString (1L <<< bit, 8)}"
                | Ok (Error UnixError.EINVAL) -> None
                | other -> failwith $"bit %d{bit}: %A{other}"
            )

        let fcntlOf (fd : int) (system : UnixSystem<int, string>) (command : int) =
            match FcntlWorld.fcntl fd command 0 system with
            | SyscallAnswer.Completed value, _ -> value
            | other -> failwith $"%A{other}"

        let zero, afterZero = init1 0 system

        [
            "single-bits-accepted", $"""%s{String.concat "," accepted} (others EINVAL)"""
            "cloexec-nonblock",
            (match initAnswer (cloexec ||| nonblock) system with
             | SyscallAnswer.Completed _, _ -> "ok"
             | answer, _ -> rc answer)
            "cloexec-plus-bit0", rc (fst (initAnswer (cloexec ||| 1) system))
            "zero", $"F_GETFL=0%s{Convert.ToString (fcntlOf zero afterZero 3, 8)} F_GETFD=%d{fcntlOf zero afterZero 1}"
            "negative-one", rc (fst (initAnswer -1 system))
        ]
        |> shouldEqual (rows instanceProbe "F" [])

    [<Test>]
    let ``section G: inotify_add_watch's screens and their order`` () : unit =
        let nobody = Credentials.ofIds (UserId.parseOrFail context 65534u) (group 65534u) []

        let seed =
            [
                "g",
                directory
                    0o777
                    [
                        "f", file 0o644
                        "perm",
                        directory
                            0o777
                            [
                                "r", directory 0o755 []
                                "x-only", directory 0o711 []
                                "none", directory 0o700 []
                                "w-only", directory 0o733 []
                                "file-0200", file 0o200
                            ]
                    ]
            ]

        let mutable system = boot root seed
        let fd, s = init1 nonblock system

        let regular, s =
            match UnixNamespace.tryOpenPath 0 0 (PathArg.ofText "g/f") 0 s with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed fd), s) -> int fd, s
            | other -> failwith $"%A{other}"

        let epoll, s =
            match UnixPoll.tryEpollCreate1 0 s with
            | Ok (Ok created) -> created
            | other -> failwith $"%A{other}"

        system <- s

        let add (on : int) (path : PathArgumentBytes) (mask : uint32) =
            match UnixInotify.tryInotifyAddWatch on path mask system with
            | Ok (answer, s) ->
                system <- s
                rc answer
            | Error refusal -> failwith $"%A{refusal}"

        let rm (on : int) (wd : int) =
            match UnixInotify.tryInotifyRmWatch on wd system with
            | Ok (answer, _) -> rc answer
            | Error refusal -> failwith $"%A{refusal}"

        let p = PathArg.ofText

        let actual =
            [
                "bad-fd-mask-0", $"""add(999, g, 0)=%s{add 999 (p "g") 0u}"""
                "bad-fd-add-and-create",
                $"""add(999, g, CREATE|MASK_ADD|MASK_CREATE)=%s{add 999 (p "g") (create ||| maskAdd ||| maskCreate)}"""
                "add-and-create",
                $"""add(fd, g, CREATE|MASK_ADD|MASK_CREATE)=%s{add fd (p "g") (create ||| maskAdd ||| maskCreate)}"""
                "bad-fd-missing-path", $"""add(999, missing, CREATE)=%s{add 999 (p "g/missing") create}"""
                "regular-fd-missing-path", $"""add(regular, missing, CREATE)=%s{add regular (p "g/missing") create}"""
                "regular-fd-mask-0", $"""add(regular, g, 0)=%s{add regular (p "g") 0u}"""
                "null-path", $"add(fd, NULL, CREATE)=%s{add fd PathArgumentBytes.Unreadable create}"
                "null-path-mask-0", $"add(fd, NULL, 0)=%s{add fd PathArgumentBytes.Unreadable 0u}"
                "empty-path", $"add(fd, \"\", CREATE)=%s{add fd (PathArg.ofBytes []) create}"
                "through-file", $"""add(fd, g/f/x, CREATE)=%s{add fd (p "g/f/x") create}"""
                "missing-mask-0", $"""add(fd, missing, 0)=%s{add fd (p "g/missing") 0u}"""
                "epoll-fd", $"""add(epoll, g, CREATE)=%s{add epoll (p "g") create}"""
                "rm-bad-fd", $"rm(999, 1)=%s{rm 999 1}"
                "rm-regular-fd", $"rm(regular, 1)=%s{rm regular 1}"
                "rm-zero", $"rm(fd, 0)=%s{rm fd 0}"
            ]

        // As uid 65534, in a fresh process of its own.
        let mutable child = boot nobody seed
        let cfd, s = init1 nonblock child
        child <- s

        let addAs (path : string) (mask : uint32) =
            match UnixInotify.tryInotifyAddWatch cfd (p path) mask child with
            | Ok (answer, s) ->
                child <- s
                rc answer
            | Error refusal -> failwith $"%A{refusal}"

        let permissions =
            [
                "perm-readable", $"""uid 65534 add(0755 dir)=%s{addAs "g/perm/r" create}"""
                "perm-search-only", $"""uid 65534 add(0711 dir)=%s{addAs "g/perm/x-only" create}"""
                "perm-none", $"""uid 65534 add(0700 dir)=%s{addAs "g/perm/none" create}"""
                "perm-write-search", $"""uid 65534 add(0733 dir)=%s{addAs "g/perm/w-only" create}"""
                "perm-write-only-file", $"""uid 65534 add(0200 file)=%s{addAs "g/perm/file-0200" create}"""
                "perm-none-mask-0", $"""uid 65534 add(0700 dir, 0)=%s{addAs "g/perm/none" 0u}"""
            ]

        actual @ permissions |> shouldEqual (rows instanceProbe "G" [])

    [<Test>]
    let ``section M: masks of flags and kernel bits`` () : unit =
        let masks =
            [
                "0x1000", 0x1000u
                "0x08000000", 0x08000000u
                "IGNORED", 0x8000u
                "ISDIR", 0x40000000u
                "Q_OVERFLOW", 0x4000u
                "UNMOUNT", 0x2000u
                "ONESHOT", 0x80000000u
                "EXCL_UNLINK", 0x04000000u
                "DONT_FOLLOW", 0x02000000u
                "MASK_ADD", 0x20000000u
                "MASK_CREATE", 0x10000000u
                "CREATE|0x1000", create ||| 0x1000u
                "CREATE|0x08000000", create ||| 0x08000000u
            ]

        let seed =
            [
                "m",
                directory
                    0o755
                    ((masks |> List.mapi (fun i _ -> $"%d{i}", directory 0o755 []))
                     @ [ "replace", directory 0o755 [] ])
            ]

        let mutable system = boot root seed
        let fd, s = init1 nonblock system
        system <- s
        let measured = rows instanceProbe "M" []

        // The two rows this kernel refuses take no watch descriptor, so each
        // accepted row's descriptor is one past the accepted rows before it
        // here, rather than the measured number.
        let mutable accepted = 0

        for i, (case, mask) in List.indexed masks do
            let expected = measured |> List.find (fun (c, _) -> c = case) |> snd

            match UnixInotify.tryInotifyAddWatch fd (PathArg.ofText $"m/%d{i}") mask system with
            | Error (InotifyAddWatchRefusal.UnmodelledEvents [ InotifyEvent.Unmount ]) when case = "UNMOUNT" -> ()
            | Error InotifyAddWatchRefusal.OneShot when case = "ONESHOT" -> ()
            | Ok (SyscallAnswer.Failed UnixError.EINVAL, _) -> expected |> shouldEqual "-1 EINVAL"
            | Ok (SyscallAnswer.Completed wd, s) ->
                system <- s
                accepted <- accepted + 1
                wd |> shouldEqual (int64 accepted)
                Int32.TryParse expected |> fst |> shouldEqual true
            | other -> failwith $"%s{case}: %A{other}"

        // Row replace-with-isdir-alone: a mask of a kernel-added bit alone,
        // re-added over a watch, answers the watch and leaves it asking for
        // nothing.
        let wd =
            match addWatch fd "m/replace" create system with
            | SyscallAnswer.Completed wd, s ->
                system <- s
                wd
            | other -> failwith $"%A{other}"

        match addWatch fd "m/replace" 0x40000000u system with
        | SyscallAnswer.Completed again, s ->
            again |> shouldEqual wd

            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors s) with
            | Some (OpenFileTarget.Inotify state) ->
                state.Watches.[WatchDescriptor (int wd)].Mask.Events |> shouldEqual Set.empty
            | other -> failwith $"%A{other}"
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``section U: the per-user limits, from a user holding nothing else`` () : unit =
        let nobody = Credentials.ofIds (UserId.parseOrFail context 65534u) (group 65534u) []

        let seed =
            [ "u", directory 0o777 [ for i in 0..8999 -> $"%d{i}", directory 0o755 [] ] ]

        let mutable system = boot nobody seed

        let rec instances (fds : int list) =
            match initAnswer cloexec system with
            | SyscallAnswer.Completed fd, s ->
                system <- s
                instances (int fd :: fds)
            | SyscallAnswer.Failed error, _ -> List.rev fds, error

        let fds, error = instances []
        let actual = ResizeArray<string * string> ()
        actual.Add ("instances", $"uid 65534 created %d{fds.Length} before %O{error}")

        for fd in List.tail fds do
            let _, s = close fd system
            system <- s

        let first = List.head fds

        let rec watches (n : int) =
            match addWatch first $"u/%d{n}" create system with
            | SyscallAnswer.Completed _, s ->
                system <- s
                watches (n + 1)
            | SyscallAnswer.Failed error, s ->
                system <- s
                n, error

        let added, error = watches 0
        actual.Add ("watches", $"uid 65534 added %d{added} before %O{error}")
        let second, s = init1 cloexec system
        system <- s
        let answer, s = addWatch second "u" create system
        system <- s
        actual.Add ("watch-on-second-instance", rc answer)
        let answer, s = addWatch first "u/0" delete system
        system <- s
        actual.Add ("re-add-at-limit", rc answer)
        let _, s = rmWatch first 1 system
        system <- s
        let answer, _ = addWatch second "u" create system
        actual.Add ("after-rm", rc answer)

        List.ofSeq actual |> shouldEqual (rows instanceProbe "U" [])

    /// `ATTRIB` of `entry` at watch 1.
    let private attribOf (entry : string) : InotifyRecord = event 1 attrib entry

    /// How many records reading `fd` to the end takes, how many are
    /// `IN_Q_OVERFLOW`, and the index of the first.
    let private countRecords (fd : int) (system : UnixSystem<int, string>) : int * int * int * UnixSystem<int, string> =
        let rec go (records : int) (overflows : int) (first : int) (system : UnixSystem<int, string>) =
            match read 0 fd UserBuffer.Mapped (1 <<< 20) system with
            | ReadOutcome.Answered (ReadAnswer.Completed bytes), system ->
                let rendered = render bytes

                let records, overflows, first =
                    ((records, overflows, first), rendered)
                    ||> List.fold (fun (records, overflows, first) record ->
                        if record.Contains "Q_OVERFLOW" then
                            records + 1, overflows + 1, (if first < 0 then records else first)
                        else
                            records + 1, overflows, first
                    )

                go records overflows first system
            | ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN), system -> records, overflows, first, system
            | other, _ -> failwith $"%A{other}"

        go 0 0 -1 system

    [<Test>]
    let ``section Q: the queue's limit and its repeats`` () : unit =
        let mutable system =
            boot root [ "q", directory 0o755 [] ; "q2", directory 0o755 [] ]

        let fd, s = init1 nonblock system
        let _, s = addWatch fd "q" attrib s
        system <- s
        let max = 16384

        let fill (count : int) =
            for i in 0 .. count - 1 do
                system <- queue fd (attribOf (if i % 2 = 0 then "f" else "g")) system

        let actual = ResizeArray<string * string> ()
        fill max
        actual.Add ("full", $"FIONREAD=%s{fionread fd system}")
        system <- queue fd (attribOf "g") system

        actual.Add (
            "identical-at-full",
            $"FIONREAD=%s{fionread fd system} (full was %d{max * 32}, +16 is an overflow record)"
        )

        let records, overflows, first, s = countRecords fd system
        system <- s

        actual.Add (
            "identical-at-full-drained",
            $"records=%d{records} overflows=%d{overflows} first-overflow=%d{first}"
        )

        fill (max + 10)
        let answer, _, s = readNow fd UserBuffer.Mapped 64 system
        system <- s
        actual.Add ("partial-read", $"read(64)=%s{answer}")
        system <- system |> queue fd (attribOf "h") |> queue fd (attribOf "f")
        let records, overflows, first, s = countRecords fd system
        system <- s

        actual.Add (
            "after-partial-read",
            $"records=%d{records} overflows=%d{overflows} first-overflow=%d{first} (two read before)"
        )

        fill (max + 10)
        let answer, _, s = readNow fd UserBuffer.Mapped 32 system
        system <- s
        actual.Add ("read-one", $"read(32)=%s{answer}")
        system <- queue fd (attribOf "h") system
        let records, overflows, first, s = countRecords fd system
        system <- s

        actual.Add (
            "after-read-one",
            $"records=%d{records} overflows=%d{overflows} first-overflow=%d{first} (one read before)"
        )

        // Two MOVED_FROM records of one name, differing only in their cookies.
        let fd2, s = init1 nonblock system
        let _, s = addWatch fd2 "q2" 0x40u s

        let movedFrom (cookie : uint32) =
            InotifyRecord.Event (
                WatchDescriptor 1,
                Set.singleton InotifyEvent.MovedFrom,
                false,
                cookie,
                Some (name "x")
            )

        system <- s |> queue fd2 (movedFrom 1u) |> queue fd2 (movedFrom 2u)

        let records =
            match FileDescriptorRegistry.tryFindTarget fd2 (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Inotify state) -> InotifyQueue.length state.Queue
            | other -> failwith $"%A{other}"

        actual.Add ("moved-from-twice", $"records=%d{records}")

        List.ofSeq actual |> shouldEqual (rows instanceProbe "Q" [])

    [<Test>]
    let ``section T: each record queued wakes an edge-triggered registration, and a repeat does not`` () : unit =
        let mutable system = boot root [ "t", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let _, s = addWatch fd "t" attrib s

        let ep, s =
            match UnixPoll.tryEpollCreate1 0 s with
            | Ok (Ok created) -> created
            | other -> failwith $"%A{other}"

        let s =
            match
                UnixPoll.tryEpollCtl
                    ep
                    1
                    fd
                    (EpollEventArgument.Readable (EpollEvents.In ||| EpollEvents.EdgeTriggered, 9UL))
                    s
            with
            | Ok (EpollCtlAnswer.Changed, s) -> s
            | other -> failwith $"%A{other}"

        system <- s

        let wait () =
            match UnixPoll.tryEpollWait 0 ep 4 UserBuffer.Mapped 0 system with
            | Ok (EpollWaitOutcome.Answered events, s) ->
                system <- s
                List.length events
            | other -> failwith $"%A{other}"

        system <- queue fd (attribOf "f") system
        let a = wait ()
        let b = wait ()
        system <- queue fd (attribOf "g") system
        let c = wait ()
        system <- queue fd (attribOf "g") system
        let d = wait ()

        [
            "edge", $"first record=%d{a} again=%d{b} second record=%d{c} merged record=%d{d}"
        ]
        |> shouldEqual (rows instanceProbe "T" [])

    [<Test>]
    let ``section X: a count of 0 with a null buffer, and a fault`` () : unit =
        let mutable system = boot root [ "x", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let _, s = addWatch fd "x" create s
        system <- queue fd (event 1 create "a") s
        let answer, _, s = readNow fd (UserBuffer.Unmapped 0UL) 0 system
        system <- s
        let afterZero = fionread fd system
        system <- queue fd (event 1 create "b") system
        let before = fionread fd system
        // The probe's buffer was mapped for the first record and not the
        // second, which `UserBuffer` cannot say; a buffer wholly unmapped
        // faults on the first record, which it loses, as row R null-buffer
        // measured.
        let faulted, _, s = readNow fd (UserBuffer.Unmapped 0x1000UL) 1024 system
        system <- s

        [
            "null-count-0", $"read(NULL, 0)=%s{answer}"
            "after-null-count-0", $"FIONREAD=%s{afterZero}"
        ]
        |> shouldEqual (rows instanceProbe "X" [ "partial-fault" ])

        before |> shouldEqual "64"
        faulted |> shouldEqual "-1 EFAULT"
        fionread fd system |> shouldEqual "32"

    [<Test>]
    let ``descriptor section D: the calls that are not inotify's own`` () : unit =
        let mutable system = boot root [ "d", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let other, s = init1 nonblock s
        let _, s = addWatch fd "d" create s
        system <- s

        let isatty =
            match UnixDescriptor.terminalAttributes fd system with
            | TerminalAttributesAnswer.NotATerminal error -> $"isatty=0 errno=%O{error}"

        let pread () =
            match UnixReadWrite.tryPread 0 fd UserBuffer.Mapped 256UL 0L system with
            | Ok (ReadAnswer.Failed error, _) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        let preadEmpty = pread ()
        system <- queue fd (event 1 create "a") system
        let preadWithRecord = pread ()
        let afterPread = $"FIONREAD=%s{fionread fd system}"

        let pwrite =
            match UnixReadWrite.tryAdmitPWrite 0 fd UserBuffer.Mapped 1UL 0L system with
            | Ok (PWriteAdmission.Answered (WriteAnswer.Failed error)) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        let write0 =
            match
                WriteOutcomes.admitThenWrite 0 fd (UserBuffer.Unmapped 0UL) ImmutableArray.Empty system
                |> WriteOutcomes.returned
            with
            | Ok (WriteAnswer.Failed error, _) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        let fionreadNull =
            match UnixDescriptor.tryBytesAvailable fd (UserBuffer.Unmapped 0UL) system with
            | Ok (BytesAvailableAnswer.Failed error) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        let fadvise =
            match UnixDescriptor.tryPosixFadvise fd 0L 0L 0 system with
            | Ok FileAdviceAnswer.Completed -> "0"
            | other -> failwith $"%A{other}"

        let ftruncate =
            match UnixDescriptor.tryFtruncate fd 0L system with
            | Ok (answer, _) -> rc answer
            | Error refusal -> failwith $"%A{refusal}"

        let ficlone =
            match UnixDescriptor.tryFileClone fd other system with
            | Ok error -> $"-1 %O{error}"
            | Error refusal -> failwith $"%A{refusal}"

        let getdents =
            match UnixNamespace.tryReadDirectoryEntry fd system with
            | Ok (ReadDirectoryAnswer.Failed error, _) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        let recv =
            match UnixReadWrite.tryRecv 0 fd UserBuffer.Mapped 256UL 0 system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), _) -> $"-1 %O{error}"
            | other -> failwith $"%A{other}"

        [
            "isatty", isatty
            "pread-empty", preadEmpty
            "pread-with-record", preadWithRecord
            "after-pread", afterPread
            "pwrite", pwrite
            "write-0", write0
            "fionread-null", fionreadNull
            "fadvise", fadvise
            "ftruncate", ftruncate
            "ficlone-two-instances", ficlone
            "getdents64", getdents
            "recv", recv
        ]
        |> shouldEqual (rows descriptorProbe "D" [ "fsync" ])

    [<Test>]
    let ``descriptor section N: F_SETFL O_NONBLOCK`` () : unit =
        let system = boot root [ "n", directory 0o755 [] ]
        let fd, system = init1 0 system
        let _, system = addWatch fd "n" create system
        let set, system = FcntlWorld.fcntl fd 4 0x800 system

        let getfl =
            match FcntlWorld.fcntl fd 3 0 system with
            | SyscallAnswer.Completed value, _ -> $"0%s{Convert.ToString (value, 8)}"
            | other -> failwith $"%A{other}"

        let answer, _, _ = readNow fd UserBuffer.Mapped 256 system

        [ "setfl-nonblock", rc set ; "getfl", getfl ; "read-empty", answer ]
        |> shouldEqual (rows descriptorProbe "N" [])

    [<Test>]
    let ``descriptor section S: sleeping readers woken to a record too large, and to a non-blocking description``
        ()
        : unit
        =
        let mutable system = boot root [ "s", directory 0o755 [] ]
        let fd, s = init1 0 system
        let _, s = addWatch fd "s" create s

        system <-
            match read 1 fd UserBuffer.Mapped 10 s with
            | ReadOutcome.WouldBlock _, s -> s
            | other, _ -> failwith $"%A{other}"

        let actual = ResizeArray<string * string> ()
        actual.Add ("small-count-asleep", $"returned=%d{(if returned 1 system then 1 else 0)}")
        system <- queue fd (event 1 create "a") system
        let woken = returned 1 system
        let answer, s = finish 1 system
        system <- s

        let result =
            match answer with
            | "result=-1 EINVAL" -> "EINVAL"
            | other -> other

        actual.Add ("small-count-woken", $"returned=%d{(if woken then 1 else 0)} result=%s{result}")
        actual.Add ("small-count-after", $"FIONREAD=%s{fionread fd system}")
        let _, _, s = readNow fd UserBuffer.Mapped 1024 system
        system <- s

        for reader in [ 1 ; 2 ] do
            system <- sleeps reader fd system

        let _, s = FcntlWorld.fcntl fd 4 0x800 system
        system <- s

        actual.Add (
            "after-setfl",
            $"returned=%d{(if returned 1 system then 1 else 0)},%d{(if returned 2 system then 1 else 0)}"
        )

        system <- queue fd (event 1 create "b") system
        let woken1 = returned 1 system
        let woken2 = returned 2 system
        let first, s = finish 1 system
        let second, s = finish 2 s
        system <- s
        let answers = [ first ; second ]

        let records =
            answers |> List.filter (fun a -> a.StartsWith "result=32") |> List.length

        let eagain = answers |> List.filter ((=) "result=-1 EAGAIN") |> List.length

        actual.Add (
            "two-sleepers-one-record",
            $"returned=%d{(if woken1 then 1 else 0)},%d{(if woken2 then 1 else 0)} record=%d{records} EAGAIN=%d{eagain} other=%d{2 - records - eagain}"
        )

        List.ofSeq actual |> shouldEqual (rows descriptorProbe "S" [])

    [<Test>]
    let ``descriptor section P: readiness asked with every bit`` () : unit =
        let mutable system = boot root [ "p", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let _, s = addWatch fd "p" create s
        system <- queue fd (event 1 create "x") s
        let polled = pollIn fd -1s system

        let ep, s =
            match UnixPoll.tryEpollCreate1 0 system with
            | Ok (Ok created) -> created
            | other -> failwith $"%A{other}"

        // The probe's registration is level-triggered, which this library's
        // epoll does not model; an edge-triggered one reports the same first.
        let every =
            0xffffffffu
            &&& ~~~(EpollEvents.Exclusive
                    ||| EpollEvents.WakeUp
                    ||| EpollEvents.OneShot
                    ||| EpollEvents.EdgeTriggered)

        let added, s =
            match
                UnixPoll.tryEpollCtl ep 1 fd (EpollEventArgument.Readable (every ||| EpollEvents.EdgeTriggered, 1UL)) s
            with
            | Ok (EpollCtlAnswer.Changed, s) -> "0", s
            | other -> failwith $"%A{other}"

        let waited =
            match UnixPoll.tryEpollWait 0 ep 2 UserBuffer.Mapped 0 s with
            | Ok (EpollWaitOutcome.Answered events, _) ->
                match events with
                | (_, mask) :: _ -> $"n=%d{events.Length} events=0x%x{mask}"
                | [] -> "n=0 events=0x0"
            | other -> failwith $"%A{other}"

        [
            "poll-every-bit", polled
            "epoll-ctl-every-bit", added
            "epoll-every-bit", waited
        ]
        |> shouldEqual (rows descriptorProbe "P" [])
