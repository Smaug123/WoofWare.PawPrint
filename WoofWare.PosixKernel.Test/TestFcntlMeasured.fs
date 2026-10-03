namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `fcntl(2)`, `dup2(2)` and `dup3(2)` against what `fcntl-dup.c` measured on
/// Darwin 27.0.0 arm64 and Linux 6.18.5 aarch64 (as root and as uid 1000),
/// replayed row by row on a system built like the probe's (`FcntlWorld`).
///
/// The rows a replay leaves out are counted and named: those whose answer
/// turns on `RLIMIT_NOFILE`, which this kernel does not model, and those of a
/// call it does not make (`accept4`, `send`, Darwin's `F_DUPFD_CLOFORK`).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFcntlMeasured =

    let private runs : FcntlWorld.Run list = FcntlWorld.runs

    let private field (prefix : string) (cell : string) : string =
        if not (cell.StartsWith prefix) then
            failwith $"expected a cell starting %s{prefix}, got %s{cell}"

        cell.Substring prefix.Length

    // ------------------------------------------------------------------ KIND

    /// Every kind the probe made, as this kernel makes it: `F_GETFL` and
    /// `F_GETFD` as measured.
    [<Test>]
    let ``a fresh descriptor of every kind reports the status and descriptor flags measured`` () : unit =
        for run in runs do
            let mutable replayed = 0

            let mismatches =
                [
                    for row in FcntlWorld.rows "KIND" run do
                        match row with
                        | [ kind ; getfl ; getfd ] ->
                            match FcntlWorld.make kind (FcntlWorld.system run) with
                            | None -> ()
                            | Some (fd, system) ->
                                replayed <- replayed + 1
                                let modelled = $"F_GETFL %s{FcntlWorld.statusFlags fd system}"
                                let modelledFd = $"F_GETFD %s{FcntlWorld.descriptorFlags fd system}"

                                if modelled <> getfl || modelledFd <> getfd then
                                    yield
                                        $"%s{run.Name} %s{kind}: measured %s{getfl} %s{getfd}, modelled %s{modelled} %s{modelledFd}"
                        | other -> failwith $"malformed KIND row %A{other}"
                ]

            mismatches |> shouldEqual []
            // Darwin has no SOCK_* flags, epoll, devices or accept4.
            replayed |> shouldBeGreaterThan 20

    // ------------------------------------------------------------------ SETFL

    /// The flags this kernel refuses to set (`FcntlRefusal.UnmodelledStatusFlags`),
    /// in `platform`'s numbering.
    let private unmodelledBits (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            OpenFlagNumbering.LinuxAppend
            ||| OpenFlagNumbering.LinuxAsynchronous
            ||| OpenFlagNumbering.linuxDirect (SimulatedUnixPlatform.architecture platform)
            ||| OpenFlagNumbering.LinuxNoAccessTime
        | SimulatedUnixFlavour.Darwin -> OpenFlagNumbering.DarwinAppend ||| OpenFlagNumbering.DarwinAsynchronous

    /// Every F_SETFL the probe made, each bit on its own beside the word
    /// `F_GETFL` reported and three whole words, on a fresh descriptor of each
    /// kind: what it answered, and what `F_GETFL` reported after it and after
    /// the word was given back. A word this kernel refuses must hold a flag it
    /// does not model, and the refusals must be exactly those.
    [<Test>]
    let ``F_SETFL answers and leaves the flags as measured, or refuses an unmodelled flag`` () : unit =
        for run in runs do
            let mutable replayed = 0
            let mutable refused = 0

            let mismatches =
                [
                    for row in FcntlWorld.rows "SETFL" run do
                        match row with
                        | [ kind ; label ; before ; set ; after ; clear ; after2 ] ->
                            match FcntlWorld.make kind (FcntlWorld.system run) with
                            | None -> ()
                            | Some (fd, system) ->

                            let measuredBefore = Convert.ToUInt32 (field "before 0x" before, 16) |> int
                            let word = Convert.ToUInt32 ((field "set 0x" set).Split(' ').[0], 16) |> int
                            let modelledBefore = FcntlWorld.statusFlags fd system

                            if modelledBefore <> $"ok 0x%x{measuredBefore}" then
                                yield
                                    $"%s{run.Name} %s{kind} %s{label}: before measured 0x%x{measuredBefore}, modelled %s{modelledBefore}"
                            else

                            let unmodelled = word &&& unmodelledBits run.Platform <> 0

                            match UnixDescriptor.fcntl fd FcntlWorld.SetFl word system with
                            | Error (FcntlRefusal.UnmodelledStatusFlags _) when unmodelled -> refused <- refused + 1
                            | Error refusal ->
                                yield $"%s{run.Name} %s{kind} %s{label}: refused (%s{FcntlRefusal.describe refusal})"
                            | Ok _ when unmodelled ->
                                yield $"%s{run.Name} %s{kind} %s{label}: answered a word holding an unmodelled flag"
                            | Ok (answer, afterSet) ->
                                replayed <- replayed + 1

                                let setAnswer = FcntlWorld.word answer
                                let afterSetFlags = FcntlWorld.statusFlags fd afterSet

                                let clearAnswer, afterClear =
                                    FcntlWorld.fcntl fd FcntlWorld.SetFl measuredBefore afterSet

                                let afterClearFlags = FcntlWorld.statusFlags fd afterClear

                                let modelled =
                                    $"set 0x%x{word} %s{setAnswer}|after %s{afterSetFlags}|clear %s{FcntlWorld.word clearAnswer}|after %s{afterClearFlags}"

                                let measured = $"%s{set}|%s{after}|%s{clear}|%s{after2}"

                                if modelled <> measured then
                                    yield
                                        $"%s{run.Name} %s{kind} %s{label}: measured %s{measured}, modelled %s{modelled}"
                        | other -> failwith $"malformed SETFL row %A{other}"
                ]

            mismatches |> List.truncate 20 |> shouldEqual []
            replayed |> shouldBeGreaterThan 250
            refused |> shouldBeGreaterThan 10

    // ------------------------------------------------------------------ SETFD

    /// F_SETFD with every bit, and with -1 and 0: what F_GETFD reports after.
    [<Test>]
    let ``F_SETFD keeps the bits measured and ignores the rest`` () : unit =
        for run in runs do
            let rows = FcntlWorld.rows "SETFD" run
            rows.Length |> shouldBeGreaterThan 33

            for row in rows do
                match row with
                | [ label ; set ; getfd ] ->
                    let fd, system =
                        FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadWrite) "f" (FcntlWorld.system run)

                    let fd, system, word =
                        match label with
                        | "clear from O_CLOEXEC" ->
                            let fd, system =
                                FcntlWorld.openWith
                                    { FcntlWorld.opening FileAccessMode.ReadWrite with
                                        CloseOnExec = true
                                    }
                                    "f"
                                    system

                            fd, system, 0
                        | "3 then 1" -> fd, snd (FcntlWorld.fcntl fd FcntlWorld.SetFd 3 system), 1
                        | hex -> fd, system, Convert.ToUInt32 (field "0x" hex, 16) |> int

                    let answer, after = FcntlWorld.fcntl fd FcntlWorld.SetFd word system

                    (label, $"set %s{FcntlWorld.word answer}", $"F_GETFD %s{FcntlWorld.descriptorFlags fd after}")
                    |> shouldEqual (label, set, getfd)
                | other -> failwith $"malformed SETFD row %A{other}"

    // ------------------------------------------------------------------ CLOEXEC

    /// Where the descriptor flags go when a descriptor is duplicated, by every
    /// call the probe duplicated with, from a descriptor carrying each
    /// combination of them.
    [<Test>]
    let ``every duplicate gets the descriptor flags measured, and the source keeps its own`` () : unit =
        for run in runs do
            let platform = run.Platform
            let mutable replayed = 0

            let mismatches =
                [
                    for row in FcntlWorld.rows "CLOEXEC" run do
                        match row with
                        | [ "F_SETFD on the dup of a pair" ; old ; created ] ->
                            let fd, system =
                                FcntlWorld.openWith
                                    (FcntlWorld.opening FileAccessMode.ReadWrite)
                                    "f"
                                    (FcntlWorld.system run)

                            let copy, system =
                                match UnixDescriptor.dup fd system with
                                | SyscallAnswer.Completed copy, system -> int copy, system
                                | other -> failwith $"dup: %A{other}"

                            let _, system = FcntlWorld.fcntl copy FcntlWorld.SetFd 1 system
                            replayed <- replayed + 1

                            let modelled =
                                $"old %s{FcntlWorld.descriptorFlags fd system}|new %s{FcntlWorld.descriptorFlags copy system}"

                            if modelled <> $"%s{old}|%s{created}" then
                                yield $"%s{run.Name} dup pair: measured %s{old} %s{created}, modelled %s{modelled}"
                        | [ setting ; operation ; old ; created ] ->
                            let flags = Convert.ToUInt32 (field "old 0x" setting, 16) |> int

                            let fd, system =
                                FcntlWorld.openWith
                                    (FcntlWorld.opening FileAccessMode.ReadWrite)
                                    "f"
                                    (FcntlWorld.system run)

                            let _, system = FcntlWorld.fcntl fd FcntlWorld.SetFd flags system

                            let duplicated : (int * UnixSystem<int, string>) option =
                                let ok (answer, system) =
                                    match answer with
                                    | SyscallAnswer.Completed copy -> Some (int copy, system)
                                    | SyscallAnswer.Failed error -> failwith $"%s{operation}: %A{error}"

                                if operation = "dup" then
                                    ok (UnixDescriptor.dup fd system)
                                elif operation = "F_DUPFD" then
                                    ok (FcntlWorld.fcntl fd FcntlWorld.DupFd 0 system)
                                elif operation = "F_DUPFD_CLOEXEC" then
                                    ok (FcntlWorld.fcntl fd (FcntlWorld.dupFdCloexec platform) 0 system)
                                elif operation = "F_DUPFD_CLOFORK" then
                                    // Darwin's 115 answered EBADF on an open
                                    // descriptor; this kernel models no such
                                    // command.
                                    match UnixDescriptor.fcntl fd 115 0 system with
                                    | Error (FcntlRefusal.UnmodelledCommand 115) -> None
                                    | other -> failwith $"F_DUPFD_CLOFORK: %A{other}"
                                elif operation = "dup2 onto free" then
                                    match UnixDescriptor.dup2 fd 40 system with
                                    | Ok answered -> ok answered
                                    | Error refusal -> failwith $"dup2: %s{Dup2Refusal.describe refusal}"
                                elif operation = "dup2 onto open (cloexec target)" then
                                    let target, system =
                                        FcntlWorld.openRaw (2 ||| FcntlWorld.closeOnExec platform) "f" system

                                    let system =
                                        match SimulatedUnixPlatform.flavour platform with
                                        | SimulatedUnixFlavour.Darwin ->
                                            snd (FcntlWorld.fcntl target FcntlWorld.SetFd 3 system)
                                        | SimulatedUnixFlavour.Linux -> system

                                    match UnixDescriptor.dup2 fd target system with
                                    | Ok answered -> ok answered
                                    | Error refusal -> failwith $"dup2: %s{Dup2Refusal.describe refusal}"
                                elif operation.StartsWith "dup2 onto itself" then
                                    match UnixDescriptor.dup2 fd fd system with
                                    | Ok answered ->
                                        (field "dup2 onto itself " operation)
                                        |> shouldEqual (FcntlWorld.number (fst answered))

                                        ok answered
                                    | Error refusal -> failwith $"dup2: %s{Dup2Refusal.describe refusal}"
                                elif operation.StartsWith "dup3" then
                                    let flags =
                                        if operation.StartsWith "dup3 O_CLOEXEC" then
                                            OpenFlagNumbering.LinuxCloseOnExec
                                        else
                                            0

                                    match UnixDescriptor.dup3 fd 40 flags system with
                                    | Ok answered -> ok answered
                                    | Error refusal -> failwith $"dup3: %s{Dup3Refusal.describe refusal}"
                                else
                                    failwith $"unknown CLOEXEC operation %s{operation}"

                            match duplicated with
                            | None -> ()
                            | Some (copy, system) ->
                                replayed <- replayed + 1

                                let modelled =
                                    $"old %s{FcntlWorld.descriptorFlags fd system}|new %s{FcntlWorld.descriptorFlags copy system}"

                                if modelled <> $"%s{old}|%s{created}" then
                                    yield
                                        $"%s{run.Name} %s{setting} %s{operation}: measured %s{old} %s{created}, modelled %s{modelled}"
                        | other -> failwith $"malformed CLOEXEC row %A{other}"
                ]

            mismatches |> shouldEqual []
            replayed |> shouldBeGreaterThan 15

    // ------------------------------------------------------------------ LIMIT

    /// The probe's table for the LIMIT and DUP2 rows: 3 to 12 open on `f`,
    /// then 5 and 9 closed.
    let private limitTable (run : FcntlWorld.Run) : UnixSystem<int, string> =
        let system =
            (FcntlWorld.system run, [ 3..12 ])
            ||> List.fold (fun system expected ->
                let fd, system =
                    FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

                fd |> shouldEqual expected
                system
            )

        [ 5 ; 9 ]
        |> List.fold
            (fun system fd ->
                match UnixDescriptor.close fd system with
                | Ok (SyscallAnswer.Completed 0L, system) -> system
                | other -> failwith $"close %d{fd}: %A{other}"
            )
            system

    /// `F_DUPFD` and `F_DUPFD_CLOEXEC` from an open, a closed and a negative
    /// descriptor, at each argument the probe tried below its soft limit of
    /// 64, and the four commands on closed descriptors: as measured. The rows
    /// at or above the limit are EINVAL on both kernels, which this kernel,
    /// modelling no `RLIMIT_NOFILE`, answers with a descriptor; they are
    /// counted, and must be exactly the rows that measured EINVAL for an open
    /// source at a non-negative argument.
    [<Test>]
    let ``F_DUPFD finds the lowest free descriptor at or above its argument, as measured below the limit`` () : unit =
        for run in runs do
            let mutable replayed = 0
            let mutable beyondLimit = 0

            for row in FcntlWorld.rows "LIMIT" run do
                match row with
                | [ command ; source ; argument ; measured ] when command.StartsWith "F_DUPFD" ->
                    let argument = int (field "arg " argument)

                    let fd =
                        match source with
                        | "from 3" -> 3
                        | "from closed 30" -> 30
                        | "from -1" -> -1
                        | other -> failwith $"unknown source %s{other}"

                    let command =
                        if command = "F_DUPFD" then
                            FcntlWorld.DupFd
                        else
                            FcntlWorld.dupFdCloexec run.Platform

                    if fd = 3 && argument >= 64 then
                        measured |> shouldEqual "EINVAL"
                        beyondLimit <- beyondLimit + 1
                    else
                        replayed <- replayed + 1

                        (source,
                         argument,
                         FcntlWorld.number (fst (FcntlWorld.fcntl fd command argument (limitTable run))))
                        |> shouldEqual (source, argument, measured)
                | [ closed ; getfl ; setfl ; getfd ; setfd ] when closed.StartsWith "closed " ->
                    let fd = int (field "closed " closed)
                    let system = limitTable run

                    let answer (command : int) =
                        FcntlWorld.word (fst (FcntlWorld.fcntl fd command 0 system))

                    replayed <- replayed + 1

                    [
                        $"F_GETFL %s{answer FcntlWorld.GetFl}"
                        $"F_SETFL %s{answer FcntlWorld.SetFl}"
                        $"F_GETFD %s{answer FcntlWorld.GetFd}"
                        $"F_SETFD %s{answer FcntlWorld.SetFd}"
                    ]
                    |> shouldEqual [ getfl ; setfl ; getfd ; setfd ]
                | [ "command 9999" ; onOpen ; onClosed ] ->
                    replayed <- replayed + 1
                    // EBADF ahead of the command, which this kernel refuses.
                    onClosed |> shouldEqual "closed EBADF"

                    UnixDescriptor.fcntl 60 9999 0 (limitTable run)
                    |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EBADF, limitTable run))

                    onOpen.StartsWith "open " |> shouldEqual true

                    match UnixDescriptor.fcntl 3 9999 0 (limitTable run) with
                    | Error (FcntlRefusal.UnmodelledCommand 9999) -> ()
                    | other -> failwith $"command 9999 on an open descriptor: %A{other}"
                | _ -> ()

            replayed |> shouldBeGreaterThan 50
            beyondLimit |> shouldEqual 8

    // ------------------------------------------------------------------ DUP2

    /// Every `dup2` and `dup3` the probe made whose answer does not turn on the
    /// soft limit: the answer, and whether the target is open after it. The
    /// rows targeting 64, 65 and `INT_MAX` from an open source with good flags
    /// are EBADF on both kernels by the limit; they are counted.
    [<Test>]
    let ``dup2 and dup3 answer as measured below the limit`` () : unit =
        for run in runs do
            let mutable replayed = 0
            let mutable beyondLimit = 0
            let reset () = FcntlWorld.system run

            let table () =
                let system = reset ()

                let a, system =
                    FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

                let b, system =
                    FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

                (a, b) |> shouldEqual (3, 4)
                system

            let isOpen (fd : int) (system : UnixSystem<int, string>) : string =
                match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                | Some _ -> "open"
                | None -> "closed"

            for row in FcntlWorld.rows "DUP2" run do
                match row with
                | [ "dup2" ; label ; arrow ; measured ; targetState ] ->
                    let parts = arrow.Split ' '
                    let oldFd = int parts.[0]
                    let newFd = int parts.[2]

                    if oldFd = 3 && newFd >= 64 then
                        measured |> shouldEqual "EBADF"
                        beyondLimit <- beyondLimit + 1
                    else
                        replayed <- replayed + 1
                        let system = table ()
                        let was = isOpen newFd system

                        match UnixDescriptor.dup2 oldFd newFd system with
                        | Ok (answer, after) ->
                            (label, FcntlWorld.number answer, $"target was %s{was}, is %s{isOpen newFd after}")
                            |> shouldEqual (label, measured, targetState)

                            UnixSystem.checkInvariants after |> shouldEqual []
                        | Error refusal -> failwith $"%s{label}: %s{Dup2Refusal.describe refusal}"
                | [ "dup3" ; label ; arrow ; measured ; targetState ] ->
                    let parts = arrow.Split ' '
                    let oldFd = int parts.[0]
                    let newFd = int parts.[2]
                    let flags = Convert.ToUInt32 (parts.[4].Substring 2, 16) |> int

                    if oldFd = 3 && newFd >= 64 && flags &&& ~~~OpenFlagNumbering.LinuxCloseOnExec = 0 then
                        measured |> shouldEqual "EBADF"
                        beyondLimit <- beyondLimit + 1
                    else
                        replayed <- replayed + 1
                        let system = table ()
                        let was = isOpen newFd system

                        match UnixDescriptor.dup3 oldFd newFd flags system with
                        | Ok (answer, after) ->
                            (label, FcntlWorld.number answer, $"target was %s{was}, is %s{isOpen newFd after}")
                            |> shouldEqual (label, measured, targetState)
                        | Error refusal -> failwith $"%s{label}: %s{Dup3Refusal.describe refusal}"
                | [ "dup3 onto open" ; _ ] -> ()
                | other -> failwith $"malformed DUP2 row %A{other}"

            match SimulatedUnixPlatform.flavour run.Platform with
            | SimulatedUnixFlavour.Linux ->
                replayed |> shouldBeGreaterThan 40
                beyondLimit |> shouldEqual 4
            | SimulatedUnixFlavour.Darwin ->
                replayed |> shouldBeGreaterThan 10
                beyondLimit |> shouldEqual 3

    /// Under Darwin there is no `dup3`.
    [<Test>]
    let ``dup3 is refused under Darwin`` () : unit =
        let system = FcntlWorld.system (runs |> List.find (fun run -> run.Name = "darwin"))

        match UnixDescriptor.dup3 0 20 0 system with
        | Error (Dup3Refusal.NotProvided SimulatedUnixFlavour.Darwin) -> ()
        | other -> failwith $"expected a refusal, got %A{other}"

    // ------------------------------------------------------------------ ONTO

    let private dup2OrFail (oldFd : int) (newFd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.dup2 oldFd newFd system with
        | Ok (SyscallAnswer.Completed answer, system) when answer = int64 newFd -> system
        | other -> failwith $"dup2(%d{oldFd}, %d{newFd}): %A{other}"

    let private lseekOrFail (fd : int) (offset : int64) (whence : int) (system : UnixSystem<int, string>) =
        match UnixDescriptor.lseek fd offset whence system with
        | Ok (SyscallAnswer.Completed position, system) -> position, system
        | other -> failwith $"lseek(%d{fd}): %A{other}"

    let private readNonBlocking (fd : int) (system : UnixSystem<int, string>) : string =
        match UnixReadWrite.read system.Leader fd UserBuffer.Mapped 4UL system with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), _) -> $"ok %d{bytes.Length}"
        | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), _) -> $"%A{error}"
        | other -> failwith $"read of %d{fd}: %A{other}"

    /// `dup2` onto an open descriptor closes it as `close` does: the two
    /// share an offset afterwards, the target's description goes if nothing
    /// else holds it (a pipe's last writer, a held lock), and a dup keeps it.
    [<Test>]
    let ``dup2 onto an open descriptor releases its description as measured`` () : unit =
        for run in runs do
            let rows = FcntlWorld.rows "ONTO" run |> List.map (String.concat "\t")

            let measured (prefix : string) =
                rows |> List.filter (fun row -> row.StartsWith prefix)

            let reading = FcntlWorld.opening FileAccessMode.ReadOnly

            // The offset.
            let x, system = FcntlWorld.openWith reading "f" (FcntlWorld.system run)
            let y, system = FcntlWorld.openWith reading "f" system
            let _, system = lseekOrFail x 5L 0 system
            let system = dup2OrFail x y system
            let atTarget, system = lseekOrFail y 0L 1 system
            let _, system = lseekOrFail y 2L 0 system
            let atSource, _ = lseekOrFail x 0L 1 system

            [
                $"offset\tdup2 ok %d{y}\ttarget at %d{atTarget}"
                $"offset\tsource at %d{atSource} after the target moved to 2"
            ]
            |> shouldEqual (measured "offset")

            // A pipe whose only writer is the target, with and without a dup of
            // it kept.
            let nonBlock = FcntlWorld.nonBlock run.Platform

            let lastWriter =
                let system = FcntlWorld.system run
                let (r, w), system = FcntlWorld.pipe 0 system
                let (_, w2), system = FcntlWorld.pipe 0 system
                let _, system = UnixDescriptor.setNonBlocking r true system
                let before = readNonBlocking r system
                let system = dup2OrFail w2 w system

                [
                    $"pipe last writer\tread before %s{before}"
                    $"pipe last writer\tdup2 ok %d{w}\tread after %s{readNonBlocking r system}"
                ]

            lastWriter |> shouldEqual (measured "pipe last writer")

            let keptWriter =
                let system = FcntlWorld.system run
                let (r, w), system = FcntlWorld.pipe nonBlock system
                let (_, w2), system = FcntlWorld.pipe 0 system

                let kept, system =
                    match UnixDescriptor.dup w system with
                    | SyscallAnswer.Completed kept, system -> int kept, system
                    | other -> failwith $"dup: %A{other}"

                let system = dup2OrFail w2 w system
                let afterDup2 = readNonBlocking r system

                let system =
                    match UnixDescriptor.close kept system with
                    | Ok (_, system) -> system
                    | Error refusal -> failwith $"close: %s{CloseRefusal.describe refusal}"

                [
                    $"pipe writer with a dup kept\tdup2 ok %d{w}\tread after %s{afterDup2}"
                    $"pipe writer with a dup kept\tread after the dup closes %s{readNonBlocking r system}"
                ]

            keptWriter |> shouldEqual (measured "pipe writer with a dup kept")

            // An flock held through the target.
            let flockRows =
                let system = FcntlWorld.system run
                let holder, system = FcntlWorld.openWith reading "f" system
                let other, system = FcntlWorld.openWith reading "f" system
                let source, system = FcntlWorld.openWith reading "f" system

                let flock (fd : int) (operation : int) (system : UnixSystem<int, string>) =
                    match UnixDescriptor.flock system.Leader fd operation system with
                    | Ok (SyscallOutcome.Answered answer, system) -> FcntlWorld.number answer, system
                    | other -> failwith $"flock: %A{other}"

                let _, system = flock holder (2 ||| 4) system
                let before, system = flock other (2 ||| 4) system
                let system = dup2OrFail source holder system
                let after, _ = flock other (2 ||| 4) system

                [
                    $"flock\tbefore, other's LOCK_EX|LOCK_NB %s{before}"
                    $"flock\tdup2 ok %d{holder}\tother's LOCK_EX|LOCK_NB %s{after}"
                ]

            // Darwin's EWOULDBLOCK is EAGAIN's number, which the probe printed.
            flockRows
            |> List.map (fun row -> row.Replace ("EWOULDBLOCK", "EAGAIN"))
            |> shouldEqual (measured "flock")

            // dup2 onto one of a dup pair, and of one of a pair onto the other.
            let pairRows =
                let system = FcntlWorld.system run
                let x, system = FcntlWorld.openWith reading "f" system

                let y, system =
                    match UnixDescriptor.dup x system with
                    | SyscallAnswer.Completed y, system -> int y, system
                    | other -> failwith $"dup: %A{other}"

                let z, system = FcntlWorld.openWith reading "f" system
                let _, system = lseekOrFail x 6L 0 system
                let system = dup2OrFail z y system
                let at, _ = lseekOrFail x 0L 1 system

                let sameRow =
                    let system = FcntlWorld.system run
                    let x, system = FcntlWorld.openWith reading "f" system

                    let y, system =
                        match UnixDescriptor.dup x system with
                        | SyscallAnswer.Completed y, system -> int y, system
                        | other -> failwith $"dup: %A{other}"

                    let _, system = FcntlWorld.fcntl y FcntlWorld.SetFd 1 system
                    let system = dup2OrFail x y system
                    $"same description\tdup2 ok %d{y}\ttarget F_GETFD %s{FcntlWorld.descriptorFlags y system}"

                [ $"dup pair\tdup2 ok %d{y}\tthe other of the pair still at %d{at}" ; sameRow ]

            pairRows |> shouldEqual (measured "dup pair" @ measured "same description")

    // ------------------------------------------------------------------ WRITTEN

    /// `system` with `SIGPIPE` ignored, as the probe had it.
    let private ignoringSigPipe (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Signals = SignalState.setDisposition Signal.SIGPIPE SignalDisposition.Ignore system.Process.Signals
                }
        }

    /// A write by the leader of `bytes` through `fd`, which must return.
    let private writeOf
        (fd : int)
        (bytes : byte[])
        (system : UnixSystem<int, string>)
        : string * UnixSystem<int, string>
        =
        match
            WriteOutcomes.admitThenWrite system.Leader fd UserBuffer.Mapped (ImmutableArray.CreateRange bytes) system
        with
        | Ok (WriteOutcome.Returns (answer, system))
        | Ok (WriteOutcome.ReturnsRaising (answer, _, system)) ->
            match answer with
            | WriteAnswer.Completed written -> $"ok %d{written}", system
            | WriteAnswer.Failed error -> $"%A{error}", system
        | other -> failwith $"write through %d{fd}: %A{other}"

    let private flockOf
        (fd : int)
        (operation : int)
        (system : UnixSystem<int, string>)
        : string * UnixSystem<int, string>
        =
        match UnixDescriptor.flock system.Leader fd operation system with
        | Ok (SyscallOutcome.Answered answer, system) -> FcntlWorld.number answer, system
        | other -> failwith $"flock(%d{fd}, %d{operation}): %A{other}"

    /// What moves the status bits F_SETFL cannot: each act the probe made on a
    /// fresh file, the description's F_GETFL before and after, as measured. A
    /// conversion of a Darwin lock is refused, as `flock` refuses every one.
    [<Test>]
    let ``what a write, a truncation or a lock leaves in F_GETFL is as measured`` () : unit =
        for run in runs do
            let mutable replayed = 0
            let mutable skipped : string list = []
            let platform = run.Platform
            let darwin = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Darwin
            let lockShared = 1
            let lockExclusive = 2
            let lockNonBlocking = 4
            let lockUnlock = 8

            for row in FcntlWorld.rows "WRITTEN" run do
                match row with
                | [ label ; before ; call ; after ] when before.StartsWith "before " ->
                    let mode =
                        if label.Contains "wronly" then FileAccessMode.WriteOnly
                        elif label.Contains "rdonly" then FileAccessMode.ReadOnly
                        else FileAccessMode.ReadWrite

                    let system = FcntlWorld.system run
                    let fd, system = FcntlWorld.openWith (FcntlWorld.opening mode) "f" system

                    let other, system =
                        FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadWrite) "f" system

                    let act : (string * UnixSystem<int, string>) option =
                        match label with
                        | "write rdwr"
                        | "write wronly"
                        | "write rdonly (refused)" -> Some (writeOf fd "x"B system)
                        | "write0 rdwr" -> Some (writeOf fd [||] system)
                        | "pwrite rdwr" ->
                            match UnixReadWrite.pwrite system.Leader fd (ImmutableArray.CreateRange "x"B) 3L system with
                            | Ok (WriteAnswer.Completed n, system) -> Some ($"ok %d{n}", system)
                            | other -> failwith $"pwrite: %A{other}"
                        | "ftruncate rdwr"
                        | "ftruncate to same length" ->
                            let length = if label = "ftruncate rdwr" then 2L else 8L

                            match UnixDescriptor.ftruncate fd length system with
                            | Ok (answer, system) -> Some (FcntlWorld.number answer, system)
                            | Error refusal -> failwith $"ftruncate: %A{refusal}"
                        | "read rdwr" ->
                            match UnixReadWrite.read system.Leader fd UserBuffer.Mapped 4UL system with
                            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->
                                Some ($"ok %d{bytes.Length}", system)
                            | other -> failwith $"read: %A{other}"
                        | "flock LOCK_SH" -> Some (flockOf fd (lockShared ||| lockNonBlocking) system)
                        | "flock LOCK_EX" -> Some (flockOf fd (lockExclusive ||| lockNonBlocking) system)
                        | "flock LOCK_UN unheld" -> Some (flockOf fd lockUnlock system)
                        | "flock LOCK_SH then LOCK_UN" ->
                            let _, system = flockOf fd (lockShared ||| lockNonBlocking) system
                            Some (flockOf fd lockUnlock system)
                        | "flock LOCK_SH then LOCK_EX" when darwin -> None
                        | "flock LOCK_SH then LOCK_EX" ->
                            let _, system = flockOf fd (lockShared ||| lockNonBlocking) system
                            Some (flockOf fd (lockExclusive ||| lockNonBlocking) system)
                        | "flock refused" ->
                            let _, system = flockOf other (lockExclusive ||| lockNonBlocking) system
                            Some (flockOf fd (lockShared ||| lockNonBlocking) system)
                        | "flock conversion refused" when darwin -> None
                        | "flock conversion refused" ->
                            let _, system = flockOf fd (lockShared ||| lockNonBlocking) system
                            let _, system = flockOf other (lockShared ||| lockNonBlocking) system
                            Some (flockOf fd (lockExclusive ||| lockNonBlocking) system)
                        | "flock through a dup" ->
                            match UnixDescriptor.dup fd system with
                            | SyscallAnswer.Completed copy, system ->
                                let answer, system = flockOf (int copy) (lockShared ||| lockNonBlocking) system

                                match UnixDescriptor.close (int copy) system with
                                | Ok (_, system) -> Some (answer, system)
                                | Error refusal -> failwith $"close: %s{CloseRefusal.describe refusal}"
                            | other -> failwith $"dup: %A{other}"
                        | other -> failwith $"unknown WRITTEN row %s{other}"

                    match act with
                    | None -> skipped <- label :: skipped
                    | Some (answer, after') ->
                        replayed <- replayed + 1
                        // Darwin answered EWOULDBLOCK, which is EAGAIN's number.
                        let answer = answer.Replace ("EWOULDBLOCK", "EAGAIN")

                        (label,
                         $"before 0x%s{(FcntlWorld.statusFlags fd system).Substring 5}",
                         $"call %s{answer}",
                         $"after 0x%s{(FcntlWorld.statusFlags fd after').Substring 5}")
                        |> shouldEqual (label, before, call, after)
                | [ "open O_TRUNC existing" ; after ] ->
                    replayed <- replayed + 1
                    let truncating = 2 ||| (OpenFlagWords.bitOf platform OpenFlagBit.Truncate)
                    let fd, system = FcntlWorld.openRaw truncating "f" (FcntlWorld.system run)
                    $"after %s{FcntlWorld.statusFlags fd system}" |> shouldEqual after
                | [ "open O_CREAT|O_TRUNC new" ; after ] ->
                    replayed <- replayed + 1

                    let creating =
                        2
                        ||| OpenFlagWords.bitOf platform OpenFlagBit.Create
                        ||| OpenFlagWords.bitOf platform OpenFlagBit.Truncate

                    let fd, system = FcntlWorld.openRaw creating "g" (FcntlWorld.system run)
                    $"after %s{FcntlWorld.statusFlags fd system}" |> shouldEqual after
                | [ "pipe write end after a write" ; after ] ->
                    replayed <- replayed + 1
                    let (_, w), system = FcntlWorld.pipe 0 (FcntlWorld.system run)
                    let _, system = writeOf w "x"B system
                    $"after %s{FcntlWorld.statusFlags w system}" |> shouldEqual after
                | [ "pipe write EPIPE" ; call ; after ] ->
                    replayed <- replayed + 1
                    let (r, w), system = FcntlWorld.pipe 0 (FcntlWorld.system run |> ignoringSigPipe)

                    let system =
                        match UnixDescriptor.close r system with
                        | Ok (_, system) -> system
                        | Error refusal -> failwith $"close: %s{CloseRefusal.describe refusal}"

                    let answer, system = writeOf w "x"B system

                    ($"call %s{answer}", $"after %s{FcntlWorld.statusFlags w system}")
                    |> shouldEqual (call, after)
                | [ "ftruncate -1" ; call ; after ]
                | [ "ftruncate through O_RDONLY" ; call ; after ]
                | [ "pwrite at -1" ; call ; after ]
                | [ "ftruncate of a pipe" ; call ; after ] ->
                    replayed <- replayed + 1
                    let label = List.head row
                    let system = FcntlWorld.system run

                    let fd, answer, system =
                        match label with
                        | "ftruncate -1" ->
                            let fd, system =
                                FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadWrite) "f" system

                            match UnixDescriptor.ftruncate fd -1L system with
                            | Ok (answer, system) -> fd, FcntlWorld.number answer, system
                            | Error refusal -> failwith $"ftruncate: %A{refusal}"
                        | "ftruncate through O_RDONLY" ->
                            let fd, system =
                                FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

                            match UnixDescriptor.ftruncate fd 0L system with
                            | Ok (answer, system) -> fd, FcntlWorld.number answer, system
                            | Error refusal -> failwith $"ftruncate: %A{refusal}"
                        | "ftruncate of a pipe" ->
                            let (_, w), system = FcntlWorld.pipe 0 system

                            match UnixDescriptor.ftruncate w 0L system with
                            | Ok (answer, system) -> w, FcntlWorld.number answer, system
                            | Error refusal -> failwith $"ftruncate: %A{refusal}"
                        | _ ->
                            let fd, system =
                                FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadWrite) "f" system

                            match UnixReadWrite.admitPWrite system.Leader fd UserBuffer.Mapped 1UL -1L system with
                            | Ok (PWriteAdmission.Answered (WriteAnswer.Failed error)) -> fd, $"%A{error}", system
                            | other -> failwith $"admitPWrite at -1: %A{other}"

                    ($"call %s{answer}", $"after %s{FcntlWorld.statusFlags fd system}")
                    |> shouldEqual (call, after)
                | other -> skipped <- List.head other :: skipped

            replayed |> shouldBeGreaterThan 18

            // The rows of calls this kernel does not make here: a socket that moves
            // bytes (it models no peer to take them) or `send(2)`, a pipe write or
            // flock interrupted by a signal, Darwin's conversion of a lock (which
            // `flock` refuses), a Darwin pipe's flock (likewise), and the ended write
            // the test below replays on its own.
            let expectedSkips =
                [
                    "socket write"
                    "socket send"
                    "unconnected socket write"
                    "blocking pipe write interrupted part way"
                    "blocking pipe write asleep part way"
                    "blocking pipe write part way, ended by a close"
                    "blocking pipe write part way, after it returned"
                    "blocking flock granted after a wait"
                    "blocking flock interrupted"
                    "flock of a pipe"
                    if darwin then
                        "flock LOCK_SH then LOCK_EX"
                        "flock conversion refused"
                ]

            (run.Name, Set.ofList skipped)
            |> shouldEqual (run.Name, Set.ofList expectedSkips)

    /// Measured on Darwin (`fcntl-dup.c`, WRITTEN rows): a blocking write asleep
    /// having put part of itself in has not marked its description, and one a
    /// close ends, answering EPIPE, has: a dup kept open shows it.
    [<Test>]
    let ``Darwin: a write a close ends marks its description once it has moved bytes`` () : unit =
        let run = runs |> List.find (fun run -> run.Name = "darwin")
        let measured = FcntlWorld.rows "WRITTEN" run |> List.map (String.concat "\t")

        let row (prefix : string) =
            measured |> List.find (fun row -> row.StartsWith prefix)

        let (_, w), system = FcntlWorld.pipe 0 (FcntlWorld.system run |> ignoringSigPipe)

        let kept, system =
            match UnixDescriptor.dup w system with
            | SyscallAnswer.Completed kept, system -> int kept, system
            | other -> failwith $"dup: %A{other}"

        let bytes = ImmutableArray.CreateRange (Array.create 200000 0uy)

        let system =
            match WriteOutcomes.admitThenWrite 1 w UserBuffer.Mapped bytes system with
            | Ok (WriteOutcome.WouldBlock (_, system)) -> system
            | other -> failwith $"a write larger than the pipe: %A{other}"

        $"blocking pipe write asleep part way\tdup's F_GETFL %s{FcntlWorld.statusFlags kept system}"
        |> shouldEqual (row "blocking pipe write asleep part way")

        let system =
            match UnixDescriptor.close w system with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"close: %A{other}"

        let answer =
            match UnixReadWrite.admitFinishWrite 1 system with
            | Ok (WriteOutcome.Returns (WriteResumption.Answered (WriteAnswer.Failed error), _))
            | Ok (WriteOutcome.ReturnsRaising (WriteResumption.Answered (WriteAnswer.Failed error), _, _)) ->
                $"%A{error}"
            | other -> failwith $"finishing the ended write: %A{other}"

        $"blocking pipe write part way, ended by a close\tcall %s{answer}\tdup's F_GETFL %s{FcntlWorld.statusFlags kept system}"
        |> shouldEqual (row "blocking pipe write part way, ended by a close")

    // ------------------------------------------------------------------ SLEEP

    /// Measured (`fcntl-dup.c`, SLEEP rows, beside `close-ends-call.c`): `dup2`
    /// onto a descriptor an `accept` sleeps through fares as `close` of it
    /// does: on Linux the accept sleeps on; on Darwin it ends with
    /// ECONNABORTED. And onto one an `flock` waits through, Darwin's `dup2`
    /// blocks, as its `close` does, which this kernel refuses as it refuses the
    /// close; Linux's returns at once and the flock waits on.
    [<Test>]
    let ``dup2 onto a descriptor a call sleeps through fares as close does`` () : unit =
        for run in runs |> List.filter (fun run -> run.Name <> "linux uid 1000") do
            let darwin =
                SimulatedUnixPlatform.flavour run.Platform = SimulatedUnixFlavour.Darwin

            for keepDup in [ false ; true ] do
                // accept
                let system = FcntlWorld.system run

                let source, system =
                    FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadOnly) "f" system

                let listening, system = FcntlWorld.listener 5000us false system

                let system =
                    if keepDup then
                        snd (UnixDescriptor.dup listening system)
                    else
                        system

                let system =
                    match UnixConnection.accept 1 listening UserBuffer.Mapped 16u system with
                    | Ok (AcceptOutcome.WouldBlock _, system) -> system
                    | other -> failwith $"accept on an empty listener: %A{other}"

                let system =
                    match UnixDescriptor.dup2 source listening system with
                    | Ok (SyscallAnswer.Completed _, system) -> system
                    | other -> failwith $"dup2 onto the accept: %A{other}"

                UnixSystem.checkInvariants system |> shouldEqual []

                match darwin, UnixWait.wakes (Set.singleton 1) system with
                | false, [] -> ()
                | true, [ 1, _ ] ->
                    match UnixConnection.finishAccept 1 system with
                    | Ok (AcceptOutcome.Failed UnixError.ECONNABORTED, _) -> ()
                    | other -> failwith $"the ended accept answered %A{other}"
                | _, woken -> failwith $"%s{run.Name}, dup kept %b{keepDup}: the accept woke %A{woken}"

                // flock
                let system = FcntlWorld.system run
                let reading = FcntlWorld.opening FileAccessMode.ReadOnly
                let source, system = FcntlWorld.openWith reading "f" system
                let holder, system = FcntlWorld.openWith reading "f" system
                let target, system = FcntlWorld.openWith reading "f" system
                let _, system = flockOf holder 2 system

                let system =
                    if keepDup then
                        snd (UnixDescriptor.dup target system)
                    else
                        system

                let system =
                    match UnixDescriptor.flock 1 target 2 system with
                    | Ok (SyscallOutcome.WouldBlock _, system) -> system
                    | other -> failwith $"a contended flock: %A{other}"

                match darwin, UnixDescriptor.dup2 source target system with
                | true, Error (Dup2Refusal.ClosingTarget (CloseRefusal.DarwinFlockedDescriptorWithWaiter (_, 1))) -> ()
                | false, Ok (SyscallAnswer.Completed _, after) ->
                    UnixWait.wakes (Set.singleton 1) after |> shouldEqual []
                | _, other -> failwith $"%s{run.Name}, dup kept %b{keepDup}: dup2 onto the flock answered %A{other}"
