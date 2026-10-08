namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `utimensat(2)` held to `utimensat-rules.c`, which measured what
/// `at-dirfd.c` left open: what the times mean and which of an object's
/// timestamps move (EFFECT), a null pathname from each kind of `dirfd`
/// (NULLFD), Linux's `AT_EMPTY_PATH` (EMPTY), which of two failures is
/// reported (ORDER, and NSECOBJ for a bad nanosecond field against what each
/// kind of object answers), the range of times stored (NSEC, NSECX), Darwin's flag
/// bits (DFLAGS) and a trailing separator (TRAIL). Every row is replayed in a
/// cell made as the probe made it, and must answer and move exactly what the
/// probe saw, but for the rows `refusalExpected` names.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUTimensAt =

    let private context : string = "TestUTimensAt"

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private perms (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits
    let private uid (n : uint32) : UserId = UserId.parseOrFail context n
    let private gid (n : uint32) : GroupId = GroupId.parseOrFail context n

    let private ownedBy (user : uint32) (group : uint32) : InodeOwner =
        {
            User = uid user
            Group = gid group
        }

    let private timestamp (seconds : int64) (nanoseconds : int) : UnixTimestamp =
        UnixTimestamp.createOrFail context seconds nanoseconds

    // ------------------------------------------------------------ the probe's output

    let private linuxResource : string =
        "WoofWare.PosixKernel.Test.utimensatRules.linux.txt"

    let private darwinResource : string =
        "WoofWare.PosixKernel.Test.utimensatRules.darwin.txt"

    /// The Linux probe ran every section on ext4, then NSEC again on tmpfs.
    /// This library models no ext4, whose range of times is narrower; every
    /// other section's rows hold on either.
    [<RequireQualifiedAccess>]
    type private Block =
        | First
        | Tmpfs

    /// The fields of the probe's lines beginning `tag`, each with the block
    /// it was printed in.
    let private probeLines (resource : string) (tag : string) : (Block * string list) list =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)

        let lines = reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)

        lines
        |> Array.fold
            (fun (block, rows) line ->
                if line.StartsWith "# filesystem: /dev/shm" then
                    Block.Tmpfs, rows
                else
                    let fields = line.Split '\t' |> List.ofArray

                    if List.head fields = tag then
                        block, (block, List.tail fields) :: rows
                    else
                        block, rows
            )
            (Block.First, [])
        |> snd
        |> List.rev

    // ------------------------------------------------------------ the probe's cell

    /// Who ran a row.
    type private Caller =
        {
            Platform : SimulatedUnixPlatform
            Credentials : Credentials
        }

    let private flavourOf (caller : Caller) : SimulatedUnixFlavour =
        SimulatedUnixPlatform.flavour caller.Platform

    let private callerOf (platform : SimulatedUnixPlatform) (field : string) : Caller =
        let id = uint32 field

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                Platform = platform
                Credentials =
                    if id = 0u then
                        Owners.root
                    else
                        Credentials.ofIds (uid id) (gid id) []
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                Platform = platform
                Credentials = Credentials.ofIds (uid id) (gid 20u) []
            }

    let private atFdCwd (caller : Caller) : int = AtDirectory.atFdCwd (flavourOf caller)

    let private noFollow (caller : Caller) : int =
        match flavourOf caller with
        | SimulatedUnixFlavour.Linux -> 0x100
        | SimulatedUnixFlavour.Darwin -> 0x20

    let private linuxAtEmptyPath : int = 0x1000

    /// When the cell was made, and when the call is made: far enough apart that
    /// no time the fixture left reads as now, and with digits below the
    /// microsecond, which Darwin's libc drops from a time it reads as now.
    let private bootTime : UnixTimestamp = timestamp 1_790_000_000L 0
    let private settling : int64 = 1_000_000_123L

    /// What the probe gave every object of the cell before the call.
    let private initialAccess : UnixTimestamp = timestamp 1_000_000_000L 111_111_111

    let private initialModification : UnixTimestamp =
        timestamp 1_200_000_000L 222_222_222

    /// The objects whose timestamps the probe reported, by the path it
    /// `lstat`ed.
    let private objects (caller : Caller) : string list =
        match flavourOf caller with
        | SimulatedUnixFlavour.Linux ->
            [
                "."
                "f"
                "ro"
                "d"
                "d/x"
                "lf"
                "ld"
                "dang"
                "tw"
                "tr"
                "tg"
                "tl"
            ]
        | SimulatedUnixFlavour.Darwin ->
            [
                "."
                "f"
                "ro"
                "d"
                "d/x"
                "lf"
                "ld"
                "dang"
                "/Users/Shared"
                "/private/etc/hosts"
                "/tmp"
            ]

    /// What the probe gave its initial times: the objects of the cell that
    /// are the caller's or uid 2000's, and on Linux `/dev/null`, which every
    /// cell shares.
    let private initialised (caller : Caller) : string list =
        [ "f" ; "ro" ; "d" ; "d/x" ; "lf" ; "ld" ; "dang" ]
        @ (
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> [ "tw" ; "tr" ; "tg" ; "tl" ; "/dev/null" ]
            | SimulatedUnixFlavour.Darwin -> []
        )

    let private nofollowStat (caller : Caller) (path : string) (system : UnixSystem<int, string>) =
        UnixPathResolution.fstatat (atFdCwd caller) (PathArg.ofText path) (noFollow caller) system

    let private inodeAt (caller : Caller) (path : string) (system : UnixSystem<int, string>) : InodeNumber =
        match nofollowStat caller path system with
        | Ok (FileStatusAnswer.Reported status) -> status.Inode
        | other -> failwith $"%s{context}: lstat(%s{path}) did not report: %A{other}"

    /// The probe's cell, as the cwd `/c/w`: `f` (0644), `ro` (0444), `d/`
    /// (0755) holding `x`, `lf -> f`, `ld -> d`, `dang -> nx`, all the
    /// caller's; on Linux the cell is root's, mode 0777, and holds uid 2000's
    /// `tw` (0666), `tr` (0644), `tg` (group 1000, 0664) and `tl -> f`; on
    /// Darwin the cell is the caller's, and root's `/Users/Shared` (01777),
    /// `/private/etc/hosts` (0644) and `/tmp -> private/tmp` stand for another
    /// user's objects. The system is root's: `dropped` gives it the caller's
    /// credentials.
    let private boot (caller : Caller) : UnixSystem<int, string> =
        let callerId = UserId.toUInt32 caller.Credentials.EffectiveUser

        let mine =
            Some (ownedBy callerId (GroupId.toUInt32 caller.Credentials.EffectiveGroup))

        let file bits owner =
            SeedEntry.File (ImmutableArray<byte>.Empty, perms bits, owner)

        let link target owner =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, owner)

        let dir bits owner (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        let theirs = Some (ownedBy 2000u 2000u)

        let cell =
            [
                "f", file 0o644 mine
                "ro", file 0o444 mine
                "d", dir 0o755 mine [ "x", file 0o644 mine ]
                "lf", link "f" mine
                "ld", link "d" mine
                "dang", link "nx" mine
            ]
            @ (
                match flavourOf caller with
                | SimulatedUnixFlavour.Linux ->
                    [
                        "tw", file 0o666 theirs
                        "tr", file 0o644 theirs
                        "tg", file 0o664 (Some (ownedBy 2000u 1000u))
                        "tl", link "f" theirs
                    ]
                | SimulatedUnixFlavour.Darwin -> []
            )

        let cellOwner =
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> None
            | SimulatedUnixFlavour.Darwin -> mine

        let darwinSystem =
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> []
            | SimulatedUnixFlavour.Darwin ->
                [
                    "tmp", link "private/tmp" None
                    "private",
                    dir
                        0o755
                        None
                        [
                            "tmp", dir 0o1777 None []
                            "etc", dir 0o755 None [ "hosts", file 0o644 None ]
                        ]
                    "Users", dir 0o755 None [ "Shared", dir 0o1777 None [] ]
                ]

        let seed =
            [ "c", dir 0o755 None [ "w", dir 0o777 cellOwner cell ] ] @ darwinSystem
            |> List.map (fun (n, e) -> name n, e)
            |> Map.ofList

        let image : UnixBootImage<int, string> =
            UnixSystem.initial caller.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context Owners.root
            |> UnixBootImage.withBootTime bootTime
            |> Configured.expectOk BootTimeRefusal.describe

        let system =
            match
                UnixBootImage.withFileSystemAndCurrentDirectory
                    bootTime
                    (ownedBy 0u 0u)
                    seed
                    (AbsoluteUnixPath.parseOrFail context "/c/w")
                    image
            with
            | Ok image -> UnixBootImage.boot image
            | Error fault -> failwith $"%s{context}: could not build the probe's cell: %A{fault}"

        // Each object's initial times, as the probe's owner set them. Darwin
        // pulled each birth time back to the modification time; Linux reports
        // none.
        let birth =
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> bootTime
            | SimulatedUnixFlavour.Darwin -> initialModification

        let fileSystem =
            initialised caller
            |> List.fold
                (fun vfs path ->
                    VirtualFileSystem.setTimes
                        (inodeAt caller path system)
                        {
                            Access = initialAccess
                            Modification = initialModification
                            StatusChange = bootTime
                            Birth = birth
                        }
                        vfs
                )
                system.Machine.FileSystem

        { system with
            Machine =
                { system.Machine with
                    FileSystem = fileSystem
                }
        }

    /// `system` with the caller's credentials, as the probe's child had once it
    /// dropped.
    let private dropped (caller : Caller) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    Credentials = caller.Credentials
                }
        }

    let private readOnly : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    let private opened
        (flags : OpenFlags)
        (path : string)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match Answered.openPath flags (UnixPath.parseOrFail context path) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other -> failwith $"%s{context}: open(%s{path}) did not open: %O{other}"

    let private completed (what : string) (answer : SyscallAnswer * UnixSystem<int, string>) : UnixSystem<int, string> =
        match answer with
        | SyscallAnswer.Completed _, system -> system
        | other -> failwith $"%s{context}: %s{what} failed: %O{other}"

    let private pipe (system : UnixSystem<int, string>) : int * int * UnixSystem<int, string> =
        match UnixPipe.pipe2 0 UserBuffer.Mapped system with
        | Ok (Pipe2Answer.Created (readFd, writeFd), system) -> readFd, writeFd, system
        | other -> failwith $"%s{context}: pipe2 did not make a pipe: %A{other}"

    /// What the probe reported of the `dirfd` itself, beside the cell's
    /// objects.
    [<RequireQualifiedAccess>]
    type private Held =
        /// Nothing.
        | Nothing
        /// What `fstat` of it reports, as "fd".
        | Descriptor of fd : int
        /// What `fstat` of each end of the pipe reports, as "this-end" and
        /// "other-end".
        | PipeEnds of thisEnd : int * otherEnd : int

    /// Which `dirfd` kinds a section reported `fstat` of.
    [<RequireQualifiedAccess>]
    type private HeldReport =
        /// None.
        | None
        /// A pipe's two ends, and a socket, an event queue, `/dev/null`, an
        /// unlinked file and a removed directory: NULLFD.
        | Unnamed
        /// A pipe's two ends, and every other kind: EMPTY.
        | Every

    /// What a row of a section reporting `report` reports of a `dirfd` of
    /// `kind`, which is `fd`, and the other end of a pipe.
    let private heldOf (report : HeldReport) (kind : string) (fd : int) (otherEnd : int option) : Held =
        match report, otherEnd with
        | HeldReport.None, _ -> Held.Nothing
        | _, Some otherEnd -> Held.PipeEnds (fd, otherEnd)
        | HeldReport.Every, None -> Held.Descriptor fd
        | HeldReport.Unnamed, None ->
            match kind with
            | "socket"
            | "eventq"
            | "devnull"
            | "unlinked"
            | "orphan" -> Held.Descriptor fd
            | _ -> Held.Nothing

    /// The probe's `dirfd` of each kind, made as the probe made it in a cell
    /// the caller has dropped into, and for a pipe's end, the other end.
    let private directoryArgument
        (caller : Caller)
        (kind : string)
        (system : UnixSystem<int, string>)
        : int * int option * UnixSystem<int, string>
        =
        // Root's pipe, made before the drop, as the probe's cell made it.
        let rootRead, rootWrite, asRoot = pipe system
        let system = dropped caller asRoot

        match kind with
        | "cwd" -> atFdCwd caller, None, system
        | "minus1" -> -1, None, system
        | "closed" -> 999, None, system
        | "dir" ->
            let fd, system = opened readOnly "d" system
            fd, None, system
        | "file"
        | "ro"
        | "tw"
        | "tr"
        | "tg" ->
            let fd, system = opened readOnly (if kind = "file" then "f" else kind) system
            fd, None, system
        | "file-wronly" ->
            let fd, system =
                opened
                    { readOnly with
                        Access = FileAccessMode.WriteOnly
                    }
                    "f"
                    system

            fd, None, system
        | "pipe-read" ->
            let readFd, writeFd, system = pipe system
            readFd, Some writeFd, system
        | "pipe-write" ->
            let readFd, writeFd, system = pipe system
            writeFd, Some readFd, system
        | "pipe-theirs" -> rootRead, Some rootWrite, system
        | "socket" ->
            let fd, system =
                NewSocket.create SocketDomain.Unix SocketKind.Stream SocketProtocol.Default system

            fd, None, system
        | "eventq" ->
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux ->
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok (fd, system)) -> fd, None, system
                | other -> failwith $"%s{context}: epoll_create1 did not make an instance: %A{other}"
            | SimulatedUnixFlavour.Darwin ->
                match UnixKqueue.kqueue system with
                | Ok (fd, system) -> fd, None, system
                | Error refusal -> failwith $"%s{context}: kqueue was refused: %A{refusal}"
        | "devnull" ->
            let fd, system = opened readOnly "/dev/null" system
            fd, None, system
        | "unlinked" ->
            let fd, system = opened readOnly "f" system

            let system =
                Answered.unlink (UnixPath.parseOrFail context "f") system
                |> completed "unlink(f)"

            fd, None, system
        | "orphan" ->
            let system =
                Answered.mkdir (PathArg.ofText "gone") 0o755 system |> completed "mkdir(gone)"

            let fd, system = opened readOnly "gone" system

            let system =
                Answered.rmdir (UnixPath.parseOrFail context "gone") system
                |> completed "rmdir(gone)"

            fd, None, system
        | "locked" ->
            let fd, system = opened readOnly "d" system

            match UnixPathResolution.chmod (PathArg.ofText "d") 0 system with
            | Ok answer -> fd, None, completed "chmod(d, 0)" answer
            | Error refusal -> failwith $"%s{context}: chmod was refused: %A{refusal}"
        | other -> failwith $"%s{context}: the probe has no dirfd kind called %s{other}"

    /// The probe's pathname of each label.
    let private pathOf (caller : Caller) (label : string) : NullablePathArgument =
        match label with
        | "NULL" -> NullablePathArgument.Null
        | "PROT_NONE" -> NullablePathArgument.NotNull PathArgumentBytes.Unreadable
        | "overlong" ->
            let length =
                PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits caller.Platform) + 8

            NullablePathArgument.NotNull (PathArg.ofText (String.init length (fun i -> if i % 2 = 0 then "a" else "/")))
        | "empty" -> NullablePathArgument.NotNull (PathArg.ofText "")
        | "ABS" -> NullablePathArgument.NotNull (PathArg.ofText "/c/w/f")
        | "THEIRS-RO" ->
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> NullablePathArgument.NotNull (PathArg.ofText "tr")
            | SimulatedUnixFlavour.Darwin -> NullablePathArgument.NotNull (PathArg.ofText "/private/etc/hosts")
        | "THEIRS-RW" ->
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> NullablePathArgument.NotNull (PathArg.ofText "tw")
            | SimulatedUnixFlavour.Darwin -> NullablePathArgument.NotNull (PathArg.ofText "/Users/Shared")
        | text -> NullablePathArgument.NotNull (PathArg.ofText text)

    /// The probe's times argument of each label.
    let private timesOf (caller : Caller) (label : string) : TimesArgument =
        let flavour = flavourOf caller

        let one (text : string) (isAccess : bool) : TimespecFields =
            match text with
            | "NOW" ->
                {
                    Seconds = 0L
                    Nanoseconds = TimestampChangeRules.utimeNow flavour
                }
            | "OMIT" ->
                {
                    Seconds = 0L
                    Nanoseconds = TimestampChangeRules.utimeOmit flavour
                }
            | "X" ->
                if isAccess then
                    {
                        Seconds = 1_500_000_000L
                        Nanoseconds = 333_333_333L
                    }
                else
                    {
                        Seconds = 1_600_000_000L
                        Nanoseconds = 444_444_444L
                    }
            | "E" ->
                {
                    Seconds = 1_100_000_000L
                    Nanoseconds = 555_555_555L
                }
            | "BAD" ->
                {
                    Seconds = 1_500_000_000L
                    Nanoseconds = 1_000_000_000L
                }
            | "NEG" ->
                {
                    Seconds = 1_500_000_000L
                    Nanoseconds = -1L
                }
            | other -> failwith $"%s{context}: the probe has no time called %s{other}"

        let modificationX = one "X" false

        let bad (access : int64) (modification : int64) =
            TimesArgument.Fields (
                {
                    Seconds = 1_500_000_000L
                    Nanoseconds = access
                },
                { modificationX with
                    Nanoseconds = modification
                }
            )

        match label with
        | "NULL" -> TimesArgument.Null
        | "FAULT" -> TimesArgument.Unreadable
        | "BAD" -> bad 1_000_000_000L modificationX.Nanoseconds
        | "BADNEG" -> bad -3L modificationX.Nanoseconds
        | "BADNOW" -> bad 1_000_000_000L (TimestampChangeRules.utimeNow flavour)
        | pair ->
            match pair.Split '/' with
            | [| access ; modification |] -> TimesArgument.Fields (one access true, one modification false)
            | _ -> failwith $"%s{context}: the probe has no times called %s{pair}"

    // ------------------------------------------------------------ what moved

    /// The timestamps `stat` reports of one object.
    type private Snapshot =
        {
            Access : UnixTimestamp
            Modification : UnixTimestamp
            StatusChange : UnixTimestamp
            Birth : UnixTimestamp option
        }

    let private snapshotOf (answer : FileStatusAnswer) : Snapshot option =
        match answer with
        | FileStatusAnswer.Failed _ -> None
        | FileStatusAnswer.Reported status ->
            Some
                {
                    Access = status.AccessTime
                    Modification = status.ModificationTime
                    StatusChange = status.StatusChangeTime
                    Birth = status.BirthTime
                }

    let private pathSnapshot (caller : Caller) (system : UnixSystem<int, string>) (path : string) : Snapshot option =
        match nofollowStat caller path system with
        | Ok answer -> snapshotOf answer
        | Error refusal -> failwith $"%s{context}: lstat(%s{path}) was refused: %s{FStatAtRefusal.describe refusal}"

    /// What `fstat` reports of `fd`; nothing where it fails, and nothing
    /// where this library will not say, as for an event queue, whose times
    /// the probe never saw move.
    let private descriptorSnapshot (system : UnixSystem<int, string>) (fd : int) : Snapshot option =
        match UnixPathResolution.fstat fd system with
        | Ok answer -> snapshotOf answer
        | Error _ -> None

    /// A timestamp as the probe printed one that moved: "now" within 50ms
    /// before the call (which this machine's clock does not advance), "now/us"
    /// for such a time with no digits below the microsecond on Darwin, and
    /// otherwise the instant.
    let private rendered (caller : Caller) (now : UnixTimestamp) (time : UnixTimestamp) : string =
        let nanoseconds (t : UnixTimestamp) : Int128 =
            Int128.op_Implicit (UnixTimestamp.seconds t) * Int128.op_Implicit 1_000_000_000L
            + Int128.op_Implicit (int64 (UnixTimestamp.nanoseconds t))

        let seconds = UnixTimestamp.seconds time
        let near = seconds > 1_700_000_000L && seconds < 4_000_000_000L
        let gap = nanoseconds now - nanoseconds time

        if near && gap >= Int128.Zero && gap <= Int128.op_Implicit 50_000_000L then
            match flavourOf caller with
            | SimulatedUnixFlavour.Darwin when UnixTimestamp.nanoseconds time % 1000 = 0 -> "now/us"
            | _ -> "now"
        else
            time.ToString ()

    /// The probe's report of one object: its fields that moved.
    let private movedOf
        (caller : Caller)
        (now : UnixTimestamp)
        (label : string)
        (before : Snapshot option)
        (after : Snapshot option)
        : string option
        =
        match before, after with
        | None, None -> None
        | None, Some _ -> Some $"%s{label}:appeared"
        | Some _, None -> Some $"%s{label}:vanished"
        | Some before, Some after ->

        let field (letter : string) (b : UnixTimestamp) (a : UnixTimestamp) =
            if b = a then
                None
            else
                Some $"%s{letter}=%s{rendered caller now a}"

        let fields =
            [
                field "a" before.Access after.Access
                field "m" before.Modification after.Modification
                field "c" before.StatusChange after.StatusChange
                match before.Birth, after.Birth with
                | Some b, Some a -> field "b" b a
                | _ -> None
            ]
            |> List.choose id

        if fields.IsEmpty then
            None
        else
            Some $"%s{label}:%s{String.Join (',', fields)}"

    /// What one call answered and moved, rendered as the probe rendered it.
    let private replay
        (caller : Caller)
        (held : Held)
        (reportObjects : bool)
        (call : UnixSystem<int, string> -> Result<SyscallAnswer * UnixSystem<int, string>, UTimensAtRefusal>)
        (system : UnixSystem<int, string>)
        : Result<string * string, UTimensAtRefusal>
        =
        // The probe let the clock move on before it looked.
        let system =
            { system with
                Machine = UnixMachineState.advanceClock settling system.Machine
            }

        let now = UnixMachineState.realtime system.Machine
        let paths = if reportObjects then objects caller else []

        let heldSnapshots (system : UnixSystem<int, string>) : (string * Snapshot option) list =
            match held with
            | Held.Nothing -> []
            | Held.Descriptor fd -> [ "fd", descriptorSnapshot system fd ]
            | Held.PipeEnds (thisEnd, otherEnd) ->
                [
                    "this-end", descriptorSnapshot system thisEnd
                    "other-end", descriptorSnapshot system otherEnd
                ]

        let snapshots (system : UnixSystem<int, string>) : (string * Snapshot option) list =
            heldSnapshots system
            @ (paths |> List.map (fun path -> path, pathSnapshot caller system path))

        let before = snapshots system

        match call system with
        | Error refusal -> Error refusal
        | Ok (answer, after) ->

        let answer =
            match answer with
            | SyscallAnswer.Completed 0L -> "ok"
            | SyscallAnswer.Completed other -> $"returned %d{other}"
            | SyscallAnswer.Failed error -> $"%A{error}"

        let moved =
            List.zip before (snapshots after)
            |> List.choose (fun ((label, b), (_, a)) -> movedOf caller now label b a)

        Ok (answer, (if moved.IsEmpty then "-" else String.Join (' ', moved)))

    /// What the probe reported, less what this library does not model: on
    /// Linux, a symbolic link the walk followed has its access time moved
    /// (the mount's `relatime`), which the probe saw as a link's access time
    /// alone moving to now.
    let private expectedMoved (caller : Caller) (moved : string) : string =
        // An object's own report moving only its access time, to now, is that
        // and nothing else: every change this call makes on Linux moves the
        // status-change time too.
        match flavourOf caller with
        | SimulatedUnixFlavour.Darwin -> moved
        | SimulatedUnixFlavour.Linux ->
            let kept =
                moved.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.filter (fun entry ->
                    not (List.exists (fun link -> entry = $"%s{link}:a=now") [ "lf" ; "ld" ; "dang" ; "tl" ])
                )

            if kept.Length = 0 then "-" else String.Join (' ', kept)

    /// The name of a refusal's case, for comparing with what a row expects.
    let private refusalName (refusal : UTimensAtRefusal) : string =
        match refusal with
        | UTimensAtRefusal.UnmodelledFlags _ -> "UnmodelledFlags"
        | UTimensAtRefusal.UnreadableTimes -> "UnreadableTimes"
        | UTimensAtRefusal.Path _ -> "Path"
        | UTimensAtRefusal.Socket _ -> "Socket"
        | UTimensAtRefusal.LaunchedPipe _ -> "LaunchedPipe"
        | UTimensAtRefusal.UnmeasuredFileSystem _ -> "UnmeasuredFileSystem"
        | UTimensAtRefusal.UnmeasuredPrivilegedCaller _ -> "UnmeasuredPrivilegedCaller"
        | UTimensAtRefusal.UnmeasuredSymlinkWrite _ -> "UnmeasuredSymlinkWrite"

    /// One row, made ready to replay.
    type private Row =
        {
            /// The row as the probe printed it, for a failure message.
            Text : string
            Caller : Caller
            /// The `dirfd` kind.
            Directory : string
            Path : string
            Times : string
            Flags : int
            /// What the probe reported of the `dirfd`.
            Held : HeldReport
            Answer : string
            Moved : string
        }

    /// The refusal this library answers a row with, where it does not
    /// replay it, and why.
    let private refusalExpected (row : Row) : string option =
        let changesTimes = row.Times <> "OMIT/OMIT"

        match flavourOf row.Caller with
        | SimulatedUnixFlavour.Linux ->
            // A socket's times, which this library does not hold, where no
            // nanosecond field is one Linux answers EINVAL for first.
            let invalidField =
                row.Times.Split ('/') |> Array.exists (fun time -> time = "BAD" || time = "NEG")

            if
                row.Directory = "socket"
                && changesTimes
                && not invalidField
                && (row.Path = "NULL" && row.Flags = 0 || row.Path = "empty")
            then
                Some "Socket"
            else
                None
        | SimulatedUnixFlavour.Darwin ->
            // Darwin's libc reading an unreadable times pointer, and the
            // flags it reads that this library does not model.
            if row.Times = "FAULT" then
                Some "UnreadableTimes"
            elif row.Flags &&& (0x800 ||| 0x2000 ||| 0x8000) <> 0 then
                Some "UnmodelledFlags"
            else
                None

    let private run (row : Row) : string option =
        let system = boot row.Caller
        let dirfd, otherEnd, system = directoryArgument row.Caller row.Directory system

        let result =
            replay
                row.Caller
                (heldOf row.Held row.Directory dirfd otherEnd)
                true
                (UnixPathResolution.utimensat
                    dirfd
                    (pathOf row.Caller row.Path)
                    (timesOf row.Caller row.Times)
                    row.Flags)
                system

        match result, refusalExpected row with
        | Error refusal, Some expected when refusalName refusal = expected -> None
        | Error refusal, _ -> Some $"%s{row.Text}: this library refused (%s{UTimensAtRefusal.describe refusal})"
        | Ok (answer, moved), Some expected ->
            Some $"%s{row.Text}: expected a refusal (%s{expected}), but this library answered %s{answer} %s{moved}"
        | Ok (answer, moved), None ->
            let expected = row.Answer, expectedMoved row.Caller row.Moved

            if (answer, moved) = expected then
                None
            else
                Some $"%s{row.Text}: the probe answered %A{expected}, this library %A{(answer, moved)}"

    let private envelopes : (SimulatedUnixPlatform * string) list =
        [
            SimulatedUnixPlatform.linuxX64, linuxResource
            SimulatedUnixPlatform.macOsArm64, darwinResource
        ]

    /// Replay every row `parse` makes of the lines tagged `tag`, under both
    /// flavours, and count them.
    let private replayAll
        (tag : string)
        (parse : SimulatedUnixPlatform -> Block -> string list -> Row option)
        : Map<SimulatedUnixFlavour, int>
        =
        let counts, failures =
            envelopes
            |> List.fold
                (fun (counts, failures) (platform, resource) ->
                    let rows =
                        probeLines resource tag
                        |> List.choose (fun (block, fields) -> parse platform block fields)

                    Map.add (SimulatedUnixPlatform.flavour platform) rows.Length counts,
                    failures @ (rows |> List.choose run)
                )
                (Map.empty, [])

        failures |> shouldEqual []
        counts

    let private flagsOf (caller : Caller) (label : string) : int =
        match label with
        | "0" -> 0
        | "NOFOLLOW" -> noFollow caller
        | "EMPTY_PATH" -> linuxAtEmptyPath
        | "EMPTY_PATH|NOFOLLOW" -> linuxAtEmptyPath ||| noFollow caller
        | hex when hex.StartsWith "0x" -> Convert.ToInt32 (hex.Substring 2, 16)
        | other -> failwith $"%s{context}: the probe has no flag word called %s{other}"

    /// A row's answer and what it moved, less the `[uid= mode=]` the probe
    /// printed of a pipe's or another descriptor's owner.
    let private answerAndMoved (fields : string list) : string * string =
        match fields with
        | [ answer ; moved ] ->
            let moved =
                if moved.StartsWith "[" then
                    moved.Substring(moved.IndexOf ']' + 1).TrimStart ()
                else
                    moved

            answer, moved
        | [ answer ] when answer.StartsWith "SIG" -> answer, ""
        | other -> failwith $"%s{context}: a malformed row %A{other}"

    // ------------------------------------------------------------ the sections

    [<Test>]
    let ``CONST: UTIME_NOW and UTIME_OMIT are each flavour's own, as the probe printed them`` () : unit =
        for platform, resource in envelopes do
            let flavour = SimulatedUnixPlatform.flavour platform

            match probeLines resource "CONST" with
            | [ _, nowField :: omitField :: _ ] ->
                nowField |> shouldEqual $"UTIME_NOW=%d{TimestampChangeRules.utimeNow flavour}"

                omitField
                |> shouldEqual $"UTIME_OMIT=%d{TimestampChangeRules.utimeOmit flavour}"
            | other -> failwith $"%s{context}: a malformed CONST row %A{other}"

    [<Test>]
    let ``EFFECT: every times argument at every object of the cell answers and moves what the probe saw`` () : unit =
        replayAll
            "EFFECT"
            (fun platform _ fields ->
                match fields with
                | callerField :: path :: times :: flags :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest

                    Some
                        {
                            Text = String.Join ('\t', "EFFECT" :: fields)
                            Caller = caller
                            Directory = "cwd"
                            Path = path
                            Times = times
                            Flags = flagsOf caller flags
                            Held = HeldReport.None
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed EFFECT row %A{other}"
            )
        |> shouldEqual (
            Map.ofList
                [
                    SimulatedUnixFlavour.Linux, 2 * 11 * 13 * 2
                    SimulatedUnixFlavour.Darwin, 10 * 13 * 2
                ]
        )

    [<Test>]
    let ``NULLFD: a null pathname from every kind of dirfd answers and moves what the probe saw`` () : unit =
        replayAll
            "NULLFD"
            (fun platform _ fields ->
                match fields with
                | callerField :: kind :: times :: flags :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest

                    Some
                        {
                            Text = String.Join ('\t', "NULLFD" :: fields)
                            Caller = caller
                            Directory = kind
                            Path = "NULL"
                            Times = times
                            Flags = flagsOf caller flags
                            Held = HeldReport.Unnamed
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed NULLFD row %A{other}"
            )
        |> shouldEqual (
            Map.ofList
                [
                    SimulatedUnixFlavour.Linux, 2 * 18 * 6 * 4
                    SimulatedUnixFlavour.Darwin, 8 * 6 * 3
                ]
        )

    [<Test>]
    let ``EMPTY: Linux's AT_EMPTY_PATH from every kind of dirfd answers and moves what the probe saw`` () : unit =
        replayAll
            "EMPTY"
            (fun platform _ fields ->
                match fields with
                | callerField :: kind :: path :: times :: flags :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest

                    Some
                        {
                            Text = String.Join ('\t', "EMPTY" :: fields)
                            Caller = caller
                            Directory = kind
                            Path = path
                            Times = times
                            Flags = flagsOf caller flags
                            Held = HeldReport.Every
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed EMPTY row %A{other}"
            )
        |> shouldEqual (
            Map.ofList
                [
                    SimulatedUnixFlavour.Linux, 2 * 18 * 3 * 2 * 2
                    SimulatedUnixFlavour.Darwin, 0
                ]
        )

    [<Test>]
    let ``NSECOBJ: Linux answers EINVAL for a bad nanosecond field before whatever the object answers`` () : unit =
        replayAll
            "NSECOBJ"
            (fun platform _ fields ->
                match fields with
                | callerField :: kind :: times :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest

                    Some
                        {
                            Text = String.Join ('\t', "NSECOBJ" :: fields)
                            Caller = caller
                            Directory = kind
                            Path = "NULL"
                            Times = times
                            Flags = 0
                            Held = HeldReport.Every
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed NSECOBJ row %A{other}"
            )
        |> shouldEqual (Map.ofList [ SimulatedUnixFlavour.Linux, 2 * 8 * 10 ; SimulatedUnixFlavour.Darwin, 0 ])

    /// `utimensat-rules.c`'s ORDER rows: the `dirfd`, pathname, times and
    /// flag word of each, which may all be bad.
    let private orderRow (caller : Caller) (label : string) : string * string * string * int =
        let badFlag =
            match flavourOf caller with
            | SimulatedUnixFlavour.Linux -> 0x1
            | SimulatedUnixFlavour.Darwin -> 0x40000000

        match label with
        | "badflags+NULLpath" -> "cwd", "NULL", "NULL", badFlag
        | "badflags+badfd" -> "minus1", "f", "NULL", badFlag
        | "badflags+nx" -> "cwd", "nx", "NULL", badFlag
        | "badflags+PROT_NONE" -> "cwd", "PROT_NONE", "NULL", badFlag
        | "badflags+FAULT" -> "cwd", "f", "FAULT", badFlag
        | "badflags+OMIT" -> "cwd", "f", "OMIT/OMIT", badFlag
        | "badflags+BAD" -> "cwd", "f", "BAD", badFlag
        | "FAULT+NULLpath" -> "cwd", "NULL", "FAULT", 0
        | "FAULT+badfd" -> "minus1", "f", "FAULT", 0
        | "FAULT+nx" -> "cwd", "nx", "FAULT", 0
        | "FAULT+overlong" -> "cwd", "overlong", "FAULT", 0
        | "FAULT+f" -> "cwd", "f", "FAULT", 0
        | "FAULT+NULLpath+fd" -> "file", "NULL", "FAULT", 0
        | "OMIT+NULLpath" -> "cwd", "NULL", "OMIT/OMIT", 0
        | "OMIT+NULLpath+badfd" -> "minus1", "NULL", "OMIT/OMIT", 0
        | "OMIT+NULLpath+fd+badflags" -> "file", "NULL", "OMIT/OMIT", badFlag
        | "OMIT+badfd" -> "minus1", "f", "OMIT/OMIT", 0
        | "OMIT+nx" -> "cwd", "nx", "OMIT/OMIT", 0
        | "OMIT+PROT_NONE" -> "cwd", "PROT_NONE", "OMIT/OMIT", 0
        | "OMIT+overlong" -> "cwd", "overlong", "OMIT/OMIT", 0
        | "OMIT+notdir" -> "file", "f", "OMIT/OMIT", 0
        | "OMIT+empty" -> "cwd", "empty", "OMIT/OMIT", 0
        | "OMIT+theirs" -> "cwd", "THEIRS-RO", "OMIT/OMIT", 0
        | "OMIT+locked" -> "locked", "x", "OMIT/OMIT", 0
        | "BAD+f" -> "cwd", "f", "BAD", 0
        | "BADNEG+f" -> "cwd", "f", "BADNEG", 0
        | "BAD+nx" -> "cwd", "nx", "BAD", 0
        | "BAD+badfd" -> "minus1", "f", "BAD", 0
        | "BAD+NULLpath" -> "cwd", "NULL", "BAD", 0
        | "BAD+NULLpath+badfd" -> "minus1", "NULL", "BAD", 0
        | "BAD+NULLpath+fd" -> "file", "NULL", "BAD", 0
        | "BAD+PROT_NONE" -> "cwd", "PROT_NONE", "BAD", 0
        | "BAD+empty" -> "cwd", "empty", "BAD", 0
        | "BAD+notdir" -> "file", "f", "BAD", 0
        | "BAD+trailing" -> "cwd", "f/", "BAD", 0
        | "BAD+locked" -> "locked", "x", "BAD", 0
        | "BAD+theirs" -> "cwd", "THEIRS-RO", "BAD", 0
        | "BAD+theirs-writable" -> "cwd", "THEIRS-RW", "BAD", 0
        | "BAD+NOW" -> "cwd", "f", "BADNOW", 0
        | "NULLpath+fd+NOFOLLOW" -> "file", "NULL", "NULL", noFollow caller
        | "NULLpath+badfd+NOFOLLOW" -> "minus1", "NULL", "NULL", noFollow caller
        | "NULLpath+fd+EMPTY_PATH" -> "file", "NULL", "NULL", linuxAtEmptyPath
        | "NULLpath+badfd+EMPTY_PATH" -> "minus1", "NULL", "NULL", linuxAtEmptyPath
        | "NULLpath+badfd+badflags" -> "minus1", "NULL", "NULL", badFlag
        | "NULLpath+cwd+EMPTY_PATH" -> "cwd", "NULL", "NULL", linuxAtEmptyPath
        | "NULLpath+fd+badflags+BAD" -> "file", "NULL", "BAD", badFlag
        | "NULLpath+badfd+BAD+badflags" -> "minus1", "NULL", "BAD", badFlag
        | "theirs-ro+NOW" -> "cwd", "THEIRS-RO", "NOW/NOW", 0
        | "theirs-ro+X" -> "cwd", "THEIRS-RO", "X/X", 0
        | "theirs-rw+X" -> "cwd", "THEIRS-RW", "X/X", 0
        | "locked+X" -> "locked", "x", "X/X", 0
        | other -> failwith $"%s{context}: the probe has no ORDER row %s{other}"

    [<Test>]
    let ``ORDER: of two failures at once, each flavour reports the one the probe saw`` () : unit =
        replayAll
            "ORDER"
            (fun platform _ fields ->
                match fields with
                | callerField :: label :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest
                    let directory, path, times, flags = orderRow caller label

                    Some
                        {
                            Text = String.Join ('\t', "ORDER" :: fields)
                            Caller = caller
                            Directory = directory
                            Path = path
                            Times = times
                            Flags = flags
                            Held = HeldReport.None
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed ORDER row %A{other}"
            )
        |> shouldEqual (Map.ofList [ SimulatedUnixFlavour.Linux, 2 * 51 ; SimulatedUnixFlavour.Darwin, 45 ])

    [<Test>]
    let ``NSEC: each flavour stores the times the probe saw stored`` () : unit =
        // tmpfs on Linux, as this library models; APFS on Darwin.
        let rows =
            [
                for platform, resource in envelopes do
                    for block, fields in probeLines resource "NSEC" do
                        let flavour = SimulatedUnixPlatform.flavour platform

                        let inModelledFileSystem =
                            match flavour, block with
                            | SimulatedUnixFlavour.Linux, Block.Tmpfs -> true
                            | SimulatedUnixFlavour.Linux, Block.First -> false
                            | SimulatedUnixFlavour.Darwin, _ -> true

                        if inModelledFileSystem then
                            match fields with
                            | callerField :: which :: seconds :: nanoseconds :: rest ->
                                let caller = callerOf platform callerField
                                let answer, moved = answerAndMoved rest
                                yield fields, caller, which, int64 seconds, int64 nanoseconds, answer, moved
                            | other -> failwith $"%s{context}: a malformed NSEC row %A{other}"
            ]

        rows.Length |> shouldEqual (2 * 2 * (23 + 17 * 3) + 2 * (23 + 17 * 3))

        [
            for fields, caller, which, seconds, nanoseconds, answer, moved in rows do
                let field =
                    {
                        Seconds = seconds
                        Nanoseconds = nanoseconds
                    }

                let omit =
                    {
                        Seconds = 0L
                        Nanoseconds = TimestampChangeRules.utimeOmit (flavourOf caller)
                    }

                let times =
                    if which = "a" then
                        TimesArgument.Fields (field, omit)
                    else
                        TimesArgument.Fields (omit, field)

                let system = boot caller
                let dirfd, _, system = directoryArgument caller "cwd" system

                match
                    replay
                        caller
                        Held.Nothing
                        true
                        (UnixPathResolution.utimensat dirfd (pathOf caller "f") times 0)
                        system
                with
                | Error refusal -> yield $"%A{fields}: this library refused (%s{UTimensAtRefusal.describe refusal})"
                | Ok actual ->
                    if actual <> (answer, moved) then
                        yield $"%A{fields}: the probe answered %A{(answer, moved)}, this library %A{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``NSECX: Linux's pipes and devtmpfs store the extreme times the probe saw stored`` () : unit =
        let rows = probeLines linuxResource "NSECX"
        rows.Length |> shouldEqual (2 * 6)

        [
            for _, fields in rows do
                match fields with
                | callerField :: kind :: seconds :: nanoseconds :: rest ->
                    let caller = callerOf SimulatedUnixPlatform.linuxX64 callerField
                    let answer, moved = answerAndMoved rest

                    let times =
                        TimesArgument.Fields (
                            {
                                Seconds = int64 seconds
                                Nanoseconds = int64 nanoseconds
                            },
                            {
                                Seconds = 0L
                                Nanoseconds = TimestampChangeRules.utimeOmit SimulatedUnixFlavour.Linux
                            }
                        )

                    let system = boot caller
                    let dirfd, _, system = directoryArgument caller kind system

                    // The probe reported `fstat` of the descriptor alone.
                    match
                        replay
                            caller
                            (Held.Descriptor dirfd)
                            false
                            (UnixPathResolution.utimensat dirfd NullablePathArgument.Null times 0)
                            system
                    with
                    | Error refusal -> yield $"%A{fields}: this library refused (%s{UTimensAtRefusal.describe refusal})"
                    | Ok actual ->
                        if actual <> (answer, moved) then
                            yield $"%A{fields}: the probe answered %A{(answer, moved)}, this library %A{actual}"
                | other -> failwith $"%s{context}: a malformed NSECX row %A{other}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``DFLAGS: Darwin reads only the flag bits the probe saw it read`` () : unit =
        replayAll
            "DFLAGS"
            (fun platform _ fields ->
                match fields with
                | callerField :: path :: flags :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest

                    Some
                        {
                            Text = String.Join ('\t', "DFLAGS" :: fields)
                            Caller = caller
                            Directory = "cwd"
                            Path = path
                            Times = "NULL"
                            Flags = flagsOf caller flags
                            Held = HeldReport.None
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed DFLAGS row %A{other}"
            )
        |> shouldEqual (Map.ofList [ SimulatedUnixFlavour.Linux, 0 ; SimulatedUnixFlavour.Darwin, 4 * 33 ])

    [<Test>]
    let ``TRAIL: a trailing separator answers and moves what the probe saw`` () : unit =
        replayAll
            "TRAIL"
            (fun platform _ fields ->
                match fields with
                | callerField :: path :: flags :: rest ->
                    let caller = callerOf platform callerField
                    let answer, moved = answerAndMoved rest

                    Some
                        {
                            Text = String.Join ('\t', "TRAIL" :: fields)
                            Caller = caller
                            Directory = "cwd"
                            Path = path
                            Times = "NULL"
                            Flags = flagsOf caller flags
                            Held = HeldReport.None
                            Answer = answer
                            Moved = moved
                        }
                | other -> failwith $"%s{context}: a malformed TRAIL row %A{other}"
            )
        |> shouldEqual (Map.ofList [ SimulatedUnixFlavour.Linux, 2 * 6 * 2 ; SimulatedUnixFlavour.Darwin, 6 * 2 ])

    // ------------------------------------------------------------ beyond the probe

    [<Test>]
    let ``utimensat on NFS refuses to set a time, and answers what it answers without setting one`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let caller =
                {
                    Platform = platform
                    Credentials = Owners.root
                }

            let flavour = flavourOf caller
            let atFdCwd = atFdCwd caller

            let system : UnixSystem<int, string> =
                UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                |> UnixBootImage.withMount (Some EmulatedMount.Nfs)
                |> Configured.expectOk MountRefusal.describe
                |> UnixBootImage.boot

            let file, system =
                opened
                    { readOnly with
                        Access = FileAccessMode.WriteOnly
                        Create = true
                    }
                    "f"
                    system

            ignore file

            let omit =
                {
                    Seconds = 0L
                    Nanoseconds = TimestampChangeRules.utimeOmit flavour
                }

            match UnixPathResolution.utimensat atFdCwd (pathOf caller "f") TimesArgument.Null 0 system with
            | Error (UTimensAtRefusal.UnmeasuredFileSystem EmulatedFileSystemType.Nfs) -> ()
            | other -> failwith $"%s{context}: %A{flavour}: expected an NFS refusal, got %A{other}"

            UnixPathResolution.utimensat atFdCwd (pathOf caller "nx") TimesArgument.Null 0 system
            |> Result.map fst
            |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.ENOENT))

            UnixPathResolution.utimensat atFdCwd (pathOf caller "f") (TimesArgument.Fields (omit, omit)) 0 system
            |> shouldEqual (Ok (SyscallAnswer.Completed 0L, system))

    [<Test>]
    let ``utimensat refuses what Darwin's rules leave unmeasured, and a pipe the process was launched with`` () : unit =
        let darwin = SimulatedUnixPlatform.macOsArm64
        let user = callerOf darwin "501"

        let rootCaller =
            {
                Platform = darwin
                Credentials = Owners.root
            }

        // Darwin's root on the caller's own file, which it does not own.
        let system = boot user |> dropped rootCaller

        match UnixPathResolution.utimensat (atFdCwd rootCaller) (pathOf rootCaller "f") TimesArgument.Null 0 system with
        | Error (UTimensAtRefusal.UnmeasuredPrivilegedCaller _) -> ()
        | other -> failwith $"%s{context}: Darwin's root: %A{other}"

        // A link root made under umask 0, so that uid 501 may write it.
        let system : UnixSystem<int, string> =
            boot user
            |> fun system ->
                { system with
                    Process =
                        { system.Process with
                            Umask = PermissionBits.parseOrFail context 0
                        }
                }

        let system =
            match UnixNamespace.symlinkat (PathArg.ofText "f") (atFdCwd user) (PathArg.ofText "wl") system with
            | Ok (SyscallAnswer.Completed _, system) -> dropped user system
            | other -> failwith $"%s{context}: symlink: %A{other}"

        match
            UnixPathResolution.utimensat (atFdCwd user) (pathOf user "wl") TimesArgument.Null (noFollow user) system
        with
        | Error (UTimensAtRefusal.UnmeasuredSymlinkWrite _) -> ()
        | other -> failwith $"%s{context}: a writable link of root's: %A{other}"

        // Not both times now: EPERM, as for root's own link /tmp.
        UnixPathResolution.utimensat (atFdCwd user) (pathOf user "wl") (timesOf user "X/X") (noFollow user) system
        |> Result.map fst
        |> shouldEqual (Ok (SyscallAnswer.Failed UnixError.EPERM))

        // Linux, standard input: an end of a pipe the process was launched with.
        let linux = callerOf SimulatedUnixPlatform.linuxX64 "1000"
        let system = boot linux |> dropped linux

        match UnixPathResolution.utimensat 0 NullablePathArgument.Null TimesArgument.Null 0 system with
        | Error (UTimensAtRefusal.LaunchedPipe _) -> ()
        | other -> failwith $"%s{context}: a launched pipe: %A{other}"
