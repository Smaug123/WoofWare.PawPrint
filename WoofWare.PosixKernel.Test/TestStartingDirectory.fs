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

/// Where a `*at` syscall's relative path starts: its `dirfd`, decoded and
/// looked up after the path is copied in. `at-dirfd.c` measured that rule for
/// nineteen calls, thirteen kinds of `dirfd` and nine pathnames. This fixture
/// holds the library to it three ways:
///
/// - the probe's rows for each call this library has as a `*at` call
///   (`replayedEndToEnd`), replayed end to end under each envelope the probe
///   ran in;
/// - every other call's rows that the starting point alone decides: wherever
///   the copy-in or `UnixPathResolution.walkStart` fails, the probe answered
///   exactly that;
/// - properties over generated paths: a walk from `AT_FDCWD` is the walk from
///   the current directory's inode that every plain call made before `*at`
///   existed, a walk from a descriptor on a directory is the walk from that
///   directory as the current one, and each plain call is its `*at` call from
///   `AT_FDCWD`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStartingDirectory =

    let private context : string = "TestStartingDirectory"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private perms (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits

    // ------------------------------------------------------------ the probe's output

    /// One of the probe's envelopes: who ran it, on which flavour, and the
    /// embedded output it printed.
    type private Envelope =
        {
            Label : string
            Platform : SimulatedUnixPlatform
            Credentials : Credentials
            Resource : string
        }

    let private linuxRoot : Envelope =
        {
            Label = "Linux root"
            Platform = SimulatedUnixPlatform.linuxX64
            Credentials = Owners.root
            Resource = "WoofWare.PosixKernel.Test.atDirfd.linuxRoot.txt"
        }

    let private linuxUser : Envelope =
        {
            Label = "Linux uid 1000"
            Platform = SimulatedUnixPlatform.linuxX64
            Credentials = Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
            Resource = "WoofWare.PosixKernel.Test.atDirfd.linuxUser.txt"
        }

    let private darwinUser : Envelope =
        {
            Label = "Darwin uid 501"
            Platform = SimulatedUnixPlatform.macOsArm64
            Credentials = Credentials.ofIds (UserId.parseOrFail context 501u) (GroupId.parseOrFail context 20u) []
            Resource = "WoofWare.PosixKernel.Test.atDirfd.darwin.txt"
        }

    let private envelopes : Envelope list = [ linuxRoot ; linuxUser ; darwinUser ]

    /// The probe's `AT` rows: (call, dirfd kind, pathname) to what it answered.
    let private atRows (envelope : Envelope) : Map<string * string * string, string> =
        use stream =
            Assembly.GetExecutingAssembly().GetManifestResourceStream envelope.Resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{envelope.Resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t')
        |> Seq.filter (fun fields -> fields.[0] = "AT")
        |> Seq.collect (fun fields ->
            fields.[3..]
            |> Seq.map (fun cell ->
                let at = cell.IndexOf '='
                (fields.[1], fields.[2], cell.Substring (0, at)), cell.Substring (at + 1)
            )
        )
        |> Map.ofSeq

    // ------------------------------------------------------------ the probe's fixture

    /// The probe's cell: the cwd is `/c/w`, holding `f`, `f2` and `d/` (with
    /// its own `f` and `f2`), all the caller's, made under umask 022.
    let private seed : Map<DirectoryEntryName, SeedEntry> =
        let file = SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, None)

        let dir (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms 0o755, None)

        Map.ofList
            [
                name "c", dir [ "w", dir [ "f", file ; "f2", file ; "d", dir [ "f", file ; "f2", file ] ] ]
            ]

    let private boot (envelope : Envelope) : UnixSystem<int, string> =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial envelope.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context envelope.Credentials

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                (InodeOwner.ofProcess envelope.Credentials)
                seed
                (AbsoluteUnixPath.parseOrFail context "/c/w")
                image
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"%s{context}: could not build the probe's cell: %A{fault}"

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

    let private opened (p : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match Answered.openPath readOnly (UnixPath.parseOrFail context p) 0 system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | other -> failwith $"%s{context}: open(%s{p}) did not open: %O{other}"

    let private completed (what : string) (answer : SyscallAnswer * UnixSystem<int, string>) : UnixSystem<int, string> =
        match answer with
        | SyscallAnswer.Completed _, system -> system
        | other -> failwith $"%s{context}: %s{what} failed: %O{other}"

    let private atFdCwd (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> -100
        | SimulatedUnixFlavour.Darwin -> -2

    /// The probe's `dirfd` of each kind, made as the probe made it, and the
    /// system it was made in; `None` where this library cannot make one.
    let private directoryArgument (kind : string) (envelope : Envelope) : (int * UnixSystem<int, string>) option =
        let system = boot envelope
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform

        match kind with
        | "cwd" -> Some (atFdCwd flavour, system)
        | "othercwd" ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> Some (-2, system)
            | SimulatedUnixFlavour.Darwin -> Some (-100, system)
        | "minus1" -> Some (-1, system)
        | "closed" -> Some (999, system)
        | "dir" -> Some (opened "d" system)
        | "file" -> Some (opened "f2" system)
        | "pipe" ->
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readFd, _), system) -> Some (readFd, system)
            | other -> failwith $"%s{context}: pipe2 did not make a pipe: %A{other}"
        | "pipe-write" ->
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (_, writeFd), system) -> Some (writeFd, system)
            | other -> failwith $"%s{context}: pipe2 did not make a pipe: %A{other}"
        | "unlinked" ->
            let fd, system = opened "f2" system

            let system =
                Answered.unlink (UnixPath.parseOrFail context "f2") system
                |> completed "unlink(f2)"

            Some (fd, system)
        | "socket" -> Some (NewSocket.create SocketDomain.Unix SocketKind.Stream SocketProtocol.Default system)
        | "orphan" ->
            let system =
                Answered.mkdir (PathArg.ofText "gone") 0o755 system |> completed "mkdir(gone)"

            let fd, system = opened "gone" system

            let system =
                Answered.rmdir (UnixPath.parseOrFail context "gone") system
                |> completed "rmdir(gone)"

            Some (fd, system)
        | "moved" ->
            let fd, system = opened "d" system

            match UnixNamespace.rename (PathArg.ofText "d") (PathArg.ofText "d-moved") system with
            | Ok answer -> Some (fd, completed "rename(d, d-moved)" answer)
            | Error refusal -> failwith $"%s{context}: rename was refused: %s{RenameRefusal.describe refusal}"
        | "locked" ->
            let fd, system = opened "d" system

            match UnixPathResolution.chmod (PathArg.ofText "d") 0 system with
            | Ok answer -> Some (fd, completed "chmod(d, 0)" answer)
            | Error refusal -> failwith $"%s{context}: chmod was refused: %A{refusal}"
        | "devnull" ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> Some (opened "/dev/null" system)
            // This library models no Darwin devfs: every path into it is refused.
            | SimulatedUnixFlavour.Darwin -> None
        | "eventq" ->
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                match UnixPoll.epollCreate1 0 system with
                | Ok (Ok created) -> Some created
                | other -> failwith $"%s{context}: epoll_create1 did not make an instance: %A{other}"
            | SimulatedUnixFlavour.Darwin ->
                match UnixKqueue.kqueue system with
                | Ok created -> Some created
                | Error refusal -> failwith $"%s{context}: kqueue was refused: %A{refusal}"
        | other -> failwith $"%s{context}: the probe has no dirfd kind called %s{other}"

    /// `n` bytes of "a/a/...", as the probe's `overlong` was.
    let private slashed (n : int) : string =
        String.init n (fun i -> if i % 2 = 0 then "a" else "/")

    /// The probe's pathname of each kind.
    let private pathArgument (kind : string) (platform : SimulatedUnixPlatform) : PathArgumentBytes =
        match kind with
        | "f" -> PathArg.ofText "f"
        | "nx" -> PathArg.ofText "nx"
        | "empty" -> PathArg.ofText ""
        | "NULL"
        | "PROT_NONE" -> PathArgumentBytes.Unreadable
        | "overlong" -> PathArg.ofText (slashed (PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits platform)))
        | "abs" -> PathArg.ofText "/c/w/f"
        | "dot" -> PathArg.ofText "."
        | "dotdot" -> PathArg.ofText ".."
        | other -> failwith $"%s{context}: the probe has no pathname called %s{other}"

    let private rendered (result : Result<SyscallAnswer, AccessRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _) -> "ok"
        | Ok (SyscallAnswer.Failed error) -> $"%A{error}"
        | Error refusal -> $"refused: %s{AccessRefusal.describe refusal}"

    /// The probe's `faccessat` rows this library does not replay, and why.
    let private faccessatNotReplayed (envelope : Envelope) : Set<string> =
        if envelope = darwinUser then
            // Darwin's devfs is not modelled, so there is no `/dev/null` to open.
            Set.singleton "devnull"
        else
            Set.empty

    let private replayFAccessAt (envelope : Envelope) : unit =
        let rows = atRows envelope |> Map.filter (fun (call, _, _) _ -> call = "faccessat")

        rows.Count |> shouldBeGreaterThan 0

        let kinds =
            rows |> Map.toSeq |> Seq.map (fun ((_, kind, _), _) -> kind) |> Set.ofSeq

        kinds.Count |> shouldEqual 13

        let skipped = faccessatNotReplayed envelope

        let mismatches =
            [
                for kind in kinds do
                    if not (skipped.Contains kind) then
                        for KeyValue ((_, _, path), expected) in rows |> Map.filter (fun (_, k, _) _ -> k = kind) do
                            match directoryArgument kind envelope with
                            | None -> yield $"%s{kind} %s{path}: no such dirfd could be made"
                            | Some (dirfd, system) ->
                                let actual =
                                    UnixPathResolution.faccessat dirfd (pathArgument path envelope.Platform) 0 0 system
                                    |> rendered

                                if actual <> expected then
                                    yield $"%s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
            ]

        mismatches |> shouldEqual []

    [<Test>]
    let ``faccessat answers every dirfd and path the probe tried as Linux root`` () : unit = replayFAccessAt linuxRoot

    [<Test>]
    let ``faccessat answers every dirfd and path the probe tried as a Linux user`` () : unit = replayFAccessAt linuxUser

    [<Test>]
    let ``faccessat answers every dirfd and path the probe tried on Darwin`` () : unit = replayFAccessAt darwinUser

    let private replaySymlinkAt (envelope : Envelope) : unit =
        let rows = atRows envelope |> Map.filter (fun (call, _, _) _ -> call = "symlinkat")
        let skipped = faccessatNotReplayed envelope

        let renderedSymlink (result : Result<SyscallAnswer * UnixSystem<int, string>, SymlinkRefusal>) : string =
            match result with
            | Ok (SyscallAnswer.Completed _, _) -> "ok"
            | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
            | Error refusal -> $"refused: %s{SymlinkRefusal.describe refusal}"

        rows.Count |> shouldEqual (13 * 9)

        [
            for KeyValue ((_, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let actual =
                            UnixNamespace.symlinkat
                                (PathArg.ofText "target")
                                dirfd
                                (pathArgument path envelope.Platform)
                                system
                            |> renderedSymlink

                        if actual <> expected then
                            yield $"%s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``symlinkat answers every dirfd and path the probe tried as Linux root`` () : unit = replaySymlinkAt linuxRoot

    [<Test>]
    let ``symlinkat answers every dirfd and path the probe tried as a Linux user`` () : unit = replaySymlinkAt linuxUser

    [<Test>]
    let ``symlinkat answers every dirfd and path the probe tried on Darwin`` () : unit = replaySymlinkAt darwinUser

    let private replayLinkAt (side : string) (envelope : Envelope) : unit =
        let call = $"linkat[%s{side}]"
        let rows = atRows envelope |> Map.filter (fun (c, _, _) _ -> c = call)
        let skipped = faccessatNotReplayed envelope
        let atFdCwd = AtDirectory.atFdCwd (SimulatedUnixPlatform.flavour envelope.Platform)

        let renderedLink (result : Result<SyscallAnswer * UnixSystem<int, string>, LinkRefusal>) : string =
            match result with
            | Ok (SyscallAnswer.Completed _, _) -> "ok"
            | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
            | Error refusal -> $"refused: %s{LinkRefusal.describe refusal}"

        rows.Count |> shouldEqual (13 * 9)

        [
            for KeyValue ((_, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let p = pathArgument path envelope.Platform

                        let actual =
                            match side with
                            | "old" -> UnixNamespace.linkat dirfd p atFdCwd (PathArg.ofText "newlink") 0 system
                            | _ -> UnixNamespace.linkat atFdCwd (PathArg.ofText "f2") dirfd p 0 system
                            |> renderedLink

                        if actual <> expected then
                            yield
                                $"%s{call} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``linkat's source answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            replayLinkAt "old" envelope

    [<Test>]
    let ``linkat's destination answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            replayLinkAt "new" envelope

    // ------------------------------------------------------------ fstatat

    let private renderedStatus (result : Result<FileStatusAnswer, FStatAtRefusal>) : string =
        match result with
        | Ok (FileStatusAnswer.Reported _) -> "ok"
        | Ok (FileStatusAnswer.Failed error) -> $"%A{error}"
        | Error refusal -> $"refused: %s{FStatAtRefusal.describe refusal}"

    let private atSymlinkNoFollow (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x100
        | SimulatedUnixFlavour.Darwin -> 0x20

    let private linuxAtEmptyPath : int = 0x1000

    let private replayFStatAt (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform

        let rows =
            atRows envelope
            |> Map.filter (fun (call, _, _) _ -> call = "fstatat" || call = "fstatat(NOFOLLOW)")

        rows.Count |> shouldEqual (2 * 13 * 9)
        let skipped = faccessatNotReplayed envelope

        [
            for KeyValue ((call, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{call} %s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let flags = if call = "fstatat" then 0 else atSymlinkNoFollow flavour

                        let actual =
                            UnixPathResolution.fstatat dirfd (pathArgument path envelope.Platform) flags system
                            |> renderedStatus

                        if actual <> expected then
                            yield
                                $"%s{call} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``fstatat answers every dirfd and path the probe tried as Linux root`` () : unit = replayFStatAt linuxRoot

    [<Test>]
    let ``fstatat answers every dirfd and path the probe tried as a Linux user`` () : unit = replayFStatAt linuxUser

    [<Test>]
    let ``fstatat answers every dirfd and path the probe tried on Darwin`` () : unit = replayFStatAt darwinUser

    /// The fields of the probe's lines beginning `tag`, from `resource`.
    let private probeLines (resource : string) (tag : string) : string list list =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t' |> List.ofArray)
        |> Seq.filter (fun fields -> List.head fields = tag)
        |> Seq.map List.tail
        |> List.ofSeq

    /// Replay the probe's FLAGS and FLAGORDER rows for `call`, which answers
    /// `answer dirfd path flags system`, rendered: every flag bit alone with
    /// AT_FDCWD and "f", and a rejected bit against the FLAGORDER cells. Every
    /// bit the library refuses must be one of `expectedRefused`, each of which
    /// the probe saw accepted or answered other than EINVAL.
    let private replayFlagsWith<'Path>
        (textPath : string -> 'Path)
        (nullPath : 'Path)
        (call : string)
        (answer : int -> 'Path -> int -> UnixSystem<int, string> -> string)
        (expectedRefused : int list)
        (envelope : Envelope)
        : unit
        =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let atFdCwd = atFdCwd flavour

        let row =
            probeLines envelope.Resource "FLAGS" |> List.find (fun row -> row.[0] = call)

        row.[1] |> shouldEqual "0=ok"

        // "rejected:" lists each bit the call did not answer 0 for, with its
        // errno where that is not EINVAL.
        let rejected =
            (List.item 2 row).Substring("rejected:".Length).Split (' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun token ->
                match token.Split '=' with
                | [| bit ; error |] -> Convert.ToInt32 (bit, 16), error
                | _ -> Convert.ToInt32 (token, 16), "EINVAL"
            )
            |> Map.ofSeq

        let refused =
            [
                for bit in 0..31 do
                    let flag = 1 <<< bit
                    let expected = Map.tryFind flag rejected |> Option.defaultValue "ok"
                    let actual = answer atFdCwd (textPath "f") flag (boot envelope)

                    if actual.StartsWith "refused: the flag word" then
                        yield flag
                    elif actual <> expected then
                        failwith
                            $"%s{context}: %s{call} with flags 0x%x{flag}: the probe answered %s{expected}, this library %s{actual}"
            ]

        refused |> shouldEqual expectedRefused

        for flag in refused do
            Map.tryFind flag rejected |> Option.defaultValue "ok" |> shouldNotEqual "EINVAL"

        let order =
            probeLines envelope.Resource "FLAGORDER"
            |> List.find (fun row -> row.[0] = call)

        order.[1] |> shouldEqual "flags=0x40000000"

        [
            for cell in order.[2..] do
                let at = cell.IndexOf '='
                let label = cell.Substring (0, at)
                let expected = cell.Substring (at + 1)

                let dirfd, path =
                    match label with
                    | "NULL" -> atFdCwd, nullPath
                    | "minus1+f" -> -1, textPath "f"
                    | "empty" -> atFdCwd, textPath ""
                    | "nx" -> atFdCwd, textPath "nx"
                    | other -> failwith $"%s{context}: the probe has no FLAGORDER cell %s{other}"

                let actual = answer dirfd path 0x40000000 (boot envelope)

                if actual <> expected then
                    yield $"FLAGORDER %s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    /// `replayFlagsWith` for a call whose pathname is a `PathArgumentBytes`,
    /// which cannot tell the null pointer from any other unreadable address.
    let private replayFlags
        (call : string)
        (answer : int -> PathArgumentBytes -> int -> UnixSystem<int, string> -> string)
        (expectedRefused : int list)
        (envelope : Envelope)
        : unit
        =
        replayFlagsWith PathArg.ofText PathArgumentBytes.Unreadable call answer expectedRefused envelope

    let private replayFStatAtFlags (envelope : Envelope) : unit =
        let expectedRefused =
            match SimulatedUnixPlatform.flavour envelope.Platform with
            | SimulatedUnixFlavour.Linux -> [ 0x800 ; 0x2000 ; 0x4000 ]
            | SimulatedUnixFlavour.Darwin -> [ 0x200 ; 0x400 ; 0x800 ; 0x2000 ; 0x8000 ]

        replayFlags
            "fstatat"
            (fun dirfd path flags system -> UnixPathResolution.fstatat dirfd path flags system |> renderedStatus)
            expectedRefused
            envelope

    [<Test>]
    let ``fstatat screens every flag bit as measured as Linux root`` () : unit = replayFStatAtFlags linuxRoot

    [<Test>]
    let ``fstatat screens every flag bit as measured as a Linux user`` () : unit = replayFStatAtFlags linuxUser

    [<Test>]
    let ``fstatat screens every flag bit as measured on Darwin`` () : unit = replayFStatAtFlags darwinUser

    let private emptyPathResource : string =
        "WoofWare.PosixKernel.Test.fstatatEmptyPath.linux.txt"

    /// `fstatat-empty-path.c`'s name for a dirfd kind, as `directoryArgument`
    /// names it.
    let private emptyPathKind (kind : string) : string =
        match kind with
        | "pipe-read" -> "pipe"
        | "epoll" -> "eventq"
        | other -> other

    let private emptyPathEnvelope (row : string list) : Envelope =
        match row.[0] with
        | "caller=0" -> linuxRoot
        | "caller=1000" -> linuxUser
        | other -> failwith $"%s{context}: the probe has no caller %s{other}"

    [<Test>]
    let ``Linux's AT_EMPTY_PATH reports what fstat reports, for every dirfd the probe tried`` () : unit =
        let rows = probeLines emptyPathResource "EMPTY"
        rows.Length |> shouldEqual 28

        [
            for row in rows do
                let envelope = emptyPathEnvelope row
                let kind = row.[1]

                for cell in row.[2..] do
                    let at = cell.LastIndexOf '='
                    let label = cell.Substring (0, at)
                    let expected = cell.Substring (at + 1)

                    match directoryArgument (emptyPathKind kind) envelope with
                    | None -> yield $"%s{kind}: no such dirfd could be made"
                    | Some (dirfd, system) ->

                    let path =
                        if label.StartsWith "NULL" then
                            PathArgumentBytes.Unreadable
                        else
                            PathArg.ofText ""

                    let flags = linuxAtEmptyPath ||| (if label.EndsWith "NOFOLLOW" then 0x100 else 0)

                    let actual = UnixPathResolution.fstatat dirfd path flags system

                    // What `fstat`, or `stat(".")` for AT_FDCWD, reports.
                    let reference =
                        if kind = "cwd" then
                            UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText ".") system
                            |> Result.mapError FStatAtRefusal.Stat
                        else
                            UnixPathResolution.fstat dirfd system
                            |> Result.mapError FStatAtRefusal.Descriptor

                    let ok =
                        match expected, actual with
                        | _, Error FStatAtRefusal.UnreadableEmptyPath -> label.StartsWith "NULL"
                        | _ when label.StartsWith "NULL" -> false
                        | "same", Error _ ->
                            // This library cannot report what the probe compared;
                            // it must refuse exactly as `fstat` does.
                            actual = reference
                        | "same", Ok (FileStatusAnswer.Reported _) -> actual = reference
                        | errno, Ok (FileStatusAnswer.Failed error) -> errno = $"%A{error}" && actual = reference
                        | _ -> false

                    if not ok then
                        yield
                            $"%s{row.[0]} %s{kind} %s{label}: the probe answered %s{expected}; this library %s{renderedStatus actual}, against %s{renderedStatus reference}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``Linux's AT_EMPTY_PATH changes nothing for a path that is not empty`` () : unit =
        let rows = probeLines emptyPathResource "PATH"
        rows.Length |> shouldEqual 28

        [
            for row in rows do
                let envelope = emptyPathEnvelope row
                let kind = row.[1]

                for cell in row.[2..] do
                    let at = cell.LastIndexOf '='
                    let label = cell.Substring (0, at)
                    let expected = cell.Substring (at + 1)

                    match directoryArgument (emptyPathKind kind) envelope with
                    | None -> yield $"%s{kind}: no such dirfd could be made"
                    | Some (dirfd, system) ->

                    let path =
                        match label with
                        | "rooted f" -> "/c/w/f"
                        | other -> other

                    let without = UnixPathResolution.fstatat dirfd (PathArg.ofText path) 0 system

                    let flagged =
                        UnixPathResolution.fstatat dirfd (PathArg.ofText path) linuxAtEmptyPath system

                    let actual =
                        $"%s{renderedStatus without}/%s{renderedStatus flagged}"
                        + (if without <> flagged then " differs" else "")

                    if actual <> expected then
                        yield
                            $"%s{row.[0]} %s{kind} %s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ readlinkat

    let private renderedLink (result : Result<ReadLinkAnswer, ReadLinkRefusal>) : string =
        match result with
        | Ok (ReadLinkAnswer.Reported bytes) -> "ok:" + Text.Encoding.ASCII.GetString (bytes.AsSpan ())
        | Ok (ReadLinkAnswer.Failed error) -> $"%A{error}"
        | Error refusal -> $"refused: %s{ReadLinkRefusal.describe refusal}"

    let private replayReadLinkAt (envelope : Envelope) : unit =
        let rows = atRows envelope |> Map.filter (fun (call, _, _) _ -> call = "readlinkat")
        rows.Count |> shouldEqual (13 * 9)
        let skipped = faccessatNotReplayed envelope

        [
            for KeyValue ((_, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        // The probe's buffer was 64 bytes.
                        let actual =
                            UnixNamespace.readlinkat
                                dirfd
                                (pathArgument path envelope.Platform)
                                UserBuffer.Mapped
                                64
                                system
                            |> renderedLink

                        if actual <> expected then
                            yield $"%s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``readlinkat answers every dirfd and path the probe tried as Linux root`` () : unit = replayReadLinkAt linuxRoot

    [<Test>]
    let ``readlinkat answers every dirfd and path the probe tried as a Linux user`` () : unit =
        replayReadLinkAt linuxUser

    [<Test>]
    let ``readlinkat answers every dirfd and path the probe tried on Darwin`` () : unit = replayReadLinkAt darwinUser

    /// `readlinkat-empty-path.c`'s kinds of dirfd that name a symbolic link
    /// itself, through Linux's `O_PATH | O_NOFOLLOW` or Darwin's `O_SYMLINK`.
    /// Neither open flag is modelled, so no descriptor here can name one.
    let private symlinkDescriptors : Set<string> =
        set [ "link" ; "dangling-link" ; "dir-link" ]

    let private replayReadLinkEmptyPath (resource : string) (envelopeOf : string -> Envelope) : unit =
        let emptyRows = probeLines resource "EMPTY"
        let sizeRows = probeLines resource "SIZE"
        let nullRows = probeLines resource "NULL"
        emptyRows.Length |> shouldBeGreaterThan 0
        sizeRows.Length |> shouldBeGreaterThan 0
        nullRows.Length |> shouldBeGreaterThan 0

        let cells (row : string list) =
            row.[1..]
            |> List.map (fun cell ->
                let at = cell.IndexOf '='
                cell.Substring (0, at), cell.Substring (at + 1)
            )

        [
            for row in emptyRows do
                let envelope = envelopeOf row.[0]

                for kind, expected in cells row do
                    if not (symlinkDescriptors.Contains kind) then
                        match directoryArgument kind envelope with
                        | None -> yield $"%s{kind}: no such dirfd could be made"
                        | Some (dirfd, system) ->
                            let actual =
                                UnixNamespace.readlinkat dirfd (PathArg.ofText "") UserBuffer.Mapped 64 system
                                |> renderedLink

                            if actual <> expected then
                                yield
                                    $"EMPTY %s{row.[0]} %s{kind}: the probe answered %s{expected}, this library %s{actual}"

            for row in sizeRows do
                let envelope = envelopeOf row.[0]
                let size = int (row.[1].Substring "size=".Length)

                for kind, expected in cells row.[1..] do
                    if not (symlinkDescriptors.Contains kind) then
                        match directoryArgument kind envelope with
                        | None -> yield $"%s{kind}: no such dirfd could be made"
                        | Some (dirfd, system) ->
                            let actual =
                                UnixNamespace.readlinkat dirfd (PathArg.ofText "") UserBuffer.Mapped size system
                                |> renderedLink

                            if actual <> expected then
                                yield
                                    $"SIZE %s{row.[0]} size=%d{size} %s{kind}: the probe answered %s{expected}, this library %s{actual}"

            // The size against an unreadable path, from AT_FDCWD.
            for row in nullRows do
                let envelope = envelopeOf row.[0]
                let atFdCwd = atFdCwd (SimulatedUnixPlatform.flavour envelope.Platform)

                for cell in row.[1..] do
                    let at = cell.LastIndexOf '='
                    let size = int (cell.Substring ("size=".Length, at - "size=".Length))
                    let expected = cell.Substring (at + 1)

                    let actual =
                        UnixNamespace.readlinkat
                            atFdCwd
                            PathArgumentBytes.Unreadable
                            UserBuffer.Mapped
                            size
                            (boot envelope)
                        |> renderedLink

                    if actual <> expected then
                        yield
                            $"NULL %s{row.[0]} size=%d{size}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``Linux's readlinkat names the dirfd's own object by an empty path, as measured`` () : unit =
        replayReadLinkEmptyPath
            "WoofWare.PosixKernel.Test.readlinkatEmptyPath.linux.txt"
            (fun caller ->
                match caller with
                | "caller=0" -> linuxRoot
                | "caller=1000" -> linuxUser
                | other -> failwith $"%s{context}: the probe has no caller %s{other}"
            )

    [<Test>]
    let ``Darwin's readlinkat treats an empty path as every call does, as measured`` () : unit =
        replayReadLinkEmptyPath
            "WoofWare.PosixKernel.Test.readlinkatEmptyPath.darwin.txt"
            (fun caller ->
                match caller with
                | "caller=501" -> darwinUser
                | other -> failwith $"%s{context}: the probe has no caller %s{other}"
            )

    // ------------------------------------------------------------ openat

    /// The raw `O_CREAT`, `O_EXCL` and `O_TRUNC` of each flavour.
    let private openCreate (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x40
        | SimulatedUnixFlavour.Darwin -> 0x200

    let private openExclusive (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x80
        | SimulatedUnixFlavour.Darwin -> 0x800

    let private openTruncate (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x200
        | SimulatedUnixFlavour.Darwin -> 0x400

    let private renderedOpen (result : Result<SyscallAnswer * UnixSystem<int, string>, OpenRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{OpenRefusal.describe refusal}"

    let private replayOpenAt (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform

        let rows =
            atRows envelope
            |> Map.filter (fun (call, _, _) _ -> call = "openat(O_RDONLY)" || call = "openat(O_CREAT)")

        rows.Count |> shouldEqual (2 * 13 * 9)
        let skipped = faccessatNotReplayed envelope

        [
            for KeyValue ((call, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{call} %s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let flags = if call = "openat(O_RDONLY)" then 0 else openCreate flavour

                        let actual =
                            UnixNamespace.openat dirfd (pathArgument path envelope.Platform) flags 0o644 system
                            |> renderedOpen

                        if actual <> expected then
                            yield
                                $"%s{call} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``openat answers every dirfd and path the probe tried as Linux root`` () : unit = replayOpenAt linuxRoot

    [<Test>]
    let ``openat answers every dirfd and path the probe tried as a Linux user`` () : unit = replayOpenAt linuxUser

    [<Test>]
    let ``openat answers every dirfd and path the probe tried on Darwin`` () : unit = replayOpenAt darwinUser

    /// `openat-limit.c`'s rows: where EMFILE falls against what the dirfd and
    /// the pathname answer, with every descriptor below the bound taken (FULL)
    /// and with one left (ONELEFT). This library refuses rather than answers
    /// EMFILE (`OpenRefusal.DescriptorLimit`), so a probe's EMFILE must be that
    /// refusal here.
    let private replayOpenAtLimit (resource : string) (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let bound = SimulatedUnixPlatform.descriptorBound envelope.Platform

        let rows =
            [
                for table in [ "FULL" ; "ONELEFT" ] do
                    for row in probeLines resource table do
                        table :: row
            ]

        rows.Length |> shouldEqual (2 * 5 * 5 * 2)

        // The probe's descriptors: the directory d, the file f, a pipe's read
        // end; then every other one below the bound taken.
        let system = boot envelope
        let dir, system = opened "d" system
        let file, system = opened "f" system

        let pipe, system =
            match UnixPipe.pipe2 0 UserBuffer.Mapped system with
            | Ok (Pipe2Answer.Created (readFd, _), system) -> readFd, system
            | other -> failwith $"%s{context}: pipe2 did not make a pipe: %A{other}"

        // Bounded, so that a table that never fills fails the test rather
        // than hanging it.
        let rec fill (opened : int) (system : UnixSystem<int, string>) =
            if opened > bound then
                failwith $"%s{context}: %d{opened} opens and the table is still not full at the bound %d{bound}"

            match UnixNamespace.openPath 0 (PathArg.ofText "f") 0 system with
            | Ok (SyscallAnswer.Completed _, system) -> fill (opened + 1) system
            | Error (OpenRefusal.DescriptorLimit _) -> system
            | other -> failwith $"%s{context}: filling the table: %A{other}"

        let full = fill 0 system

        let oneLeft =
            match UnixDescriptor.close (bound - 1) full with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"%s{context}: closing the last descriptor: %A{other}"

        [
            for row in rows do
                let table, dirLabel, pathLabel, flagLabel, expected =
                    match row with
                    | [ table ; d ; p ; f ; answer ] -> table, d, p, f, answer
                    | other -> failwith $"%s{context}: a malformed row %A{other}"

                let system = if table = "FULL" then full else oneLeft

                let dirfd =
                    match dirLabel with
                    | "dir" -> dir
                    | "file" -> file
                    | "pipe" -> pipe
                    | "minus1" -> -1
                    | "closed" -> bound + 5
                    | other -> failwith $"%s{context}: the probe has no dirfd %s{other}"

                let path =
                    match pathLabel with
                    | "f" -> PathArg.ofText "f"
                    | "nx" -> PathArg.ofText "nx"
                    | "empty" -> PathArg.ofText ""
                    | "NULL" -> PathArgumentBytes.Unreadable
                    | "rooted" -> PathArg.ofText "/c/w/f"
                    | other -> failwith $"%s{context}: the probe has no pathname %s{other}"

                let flags = if flagLabel = "O_CREAT" then openCreate flavour else 0
                let result = UnixNamespace.openat dirfd path flags 0o644 system

                let ok =
                    match expected, result with
                    | "EMFILE", Error (OpenRefusal.DescriptorLimit _) -> true
                    | _ -> renderedOpen result = expected

                if not ok then
                    yield
                        $"%s{table} %s{dirLabel} %s{pathLabel} %s{flagLabel}: the probe answered %s{expected}, this library %s{renderedOpen result}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``openat's descriptor limit falls where Linux puts it`` () : unit =
        replayOpenAtLimit "WoofWare.PosixKernel.Test.openatLimit.linux.txt" linuxRoot

    [<Test>]
    let ``openat's descriptor limit falls where Darwin puts it`` () : unit =
        replayOpenAtLimit "WoofWare.PosixKernel.Test.openatLimit.darwin.txt" darwinUser

    // ------------------------------------------------------------ mkdirat

    let private renderedMkdirAt (result : Result<SyscallAnswer * UnixSystem<int, string>, PathRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{PathRefusal.describe refusal}"

    let private replayMkdirAt (envelope : Envelope) : unit =
        let rows = atRows envelope |> Map.filter (fun (call, _, _) _ -> call = "mkdirat")
        rows.Count |> shouldEqual (13 * 9)
        let skipped = faccessatNotReplayed envelope

        [
            for KeyValue ((_, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        // The probe's mode.
                        let actual =
                            UnixNamespace.mkdirat dirfd (pathArgument path envelope.Platform) 0o755 system
                            |> renderedMkdirAt

                        if actual <> expected then
                            yield $"%s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``mkdirat answers every dirfd and path the probe tried as Linux root`` () : unit = replayMkdirAt linuxRoot

    [<Test>]
    let ``mkdirat answers every dirfd and path the probe tried as a Linux user`` () : unit = replayMkdirAt linuxUser

    [<Test>]
    let ``mkdirat answers every dirfd and path the probe tried on Darwin`` () : unit = replayMkdirAt darwinUser

    /// `mkdirat-rules.c`'s cell, with the current directory at `cwd`: `/c/w`
    /// holding `d/`, which holds a file `f`, a directory `sub/`, the links
    /// `dang -> nx2`, `cyc -> cyc`, `lf -> f` and `ld -> sub`, an unwritable
    /// `ro/` holding `e/`, and a set-group-ID `sg/`, all the caller's, made
    /// under umask 022. As root, `sg`'s group is 4321, which is not the
    /// caller's. On Darwin every entry has the group of the probe's scratch
    /// directory, wheel, which its caller is not in.
    let private mkdirAtCell (envelope : Envelope) (cwd : string) : UnixSystem<int, string> =
        let owner =
            match SimulatedUnixPlatform.flavour envelope.Platform with
            | SimulatedUnixFlavour.Linux -> InodeOwner.ofProcess envelope.Credentials
            | SimulatedUnixFlavour.Darwin ->
                {
                    User = envelope.Credentials.EffectiveUser
                    Group = GroupId.parseOrFail context 0u
                }

        let setGroupIdOwner =
            if envelope = linuxRoot then
                Some
                    { owner with
                        Group = GroupId.parseOrFail context 4321u
                    }
            else
                None

        let file = SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, None)

        let link (target : string) =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

        let dir (bits : int) (owner : InodeOwner option) (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        let seed =
            Map.ofList
                [
                    name "c",
                    dir
                        0o777
                        None
                        [
                            "w",
                            dir
                                0o755
                                None
                                [
                                    "d",
                                    dir
                                        0o755
                                        None
                                        [
                                            "f", file
                                            "sub", dir 0o755 None []
                                            "dang", link "nx2"
                                            "cyc", link "cyc"
                                            "lf", link "f"
                                            "ld", link "sub"
                                            "ro", dir 0o555 None [ "e", dir 0o755 None [] ]
                                            "sg", dir 0o2775 setGroupIdOwner []
                                        ]
                                ]
                        ]
                ]

        let image : UnixBootImage<int, string> =
            UnixSystem.initial envelope.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context envelope.Credentials

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                owner
                seed
                (AbsoluteUnixPath.parseOrFail context cwd)
                image
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"%s{context}: could not build mkdirat-rules.c's cell: %A{fault}"

    /// Bytes as `mkdirat-rules.c` prints a name: printable ASCII as itself,
    /// any other byte as `\xNN`.
    let private escapedBytes (bytes : byte seq) : string =
        bytes
        |> Seq.map (fun b ->
            if b < 0x20uy || b >= 0x7fuy then
                $"\\x%02x{b}"
            else
                string (char b)
        )
        |> String.concat ""

    /// What `mkdirat-rules.c` prints for a call made in `before`: the errno,
    /// or "ok" with the new directory's permission bits, whose group it took
    /// (its directory's, the caller's, or "both" where they are one), and
    /// where under the cell `/c` it was made.
    let private renderedCreation
        (before : UnixSystem<int, string>)
        (result : Result<SyscallAnswer * UnixSystem<int, string>, PathRefusal>)
        : string
        =
        match result with
        | Error refusal -> $"refused: %s{PathRefusal.describe refusal}"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Ok (SyscallAnswer.Completed _, after) ->

        let vfs = after.Machine.FileSystem
        let existed = VirtualFileSystem.inodes before.Machine.FileSystem

        let created =
            VirtualFileSystem.inodes vfs
            |> Map.toList
            |> List.filter (fun (inode, _) -> not (existed.ContainsKey inode))

        match created with
        | [ inode, made ] ->
            let parent =
                match made.Content with
                | InodeContent.Directory content ->
                    match VirtualFileSystem.tryGet content.Parent vfs with
                    | Some parent -> parent
                    | None ->
                        failwith $"%s{context}: the new directory's parent %O{content.Parent} is not in the filesystem"
                | other -> failwith $"%s{context}: mkdir made something other than a directory: %A{other}"

            let egid = after.Process.Credentials.EffectiveGroup

            let group =
                if made.Owner.Group = parent.Owner.Group then
                    if made.Owner.Group = egid then "both" else "parent"
                elif made.Owner.Group = egid then
                    "egid"
                else
                    "other"

            let where =
                match VirtualFileSystem.pathOfDirectory inode vfs with
                | Some path ->
                    // Relative to the cell, "/c/".
                    AbsoluteUnixPath.toByteString path |> UnixByteString.toBytes |> Seq.skip 3
                | None -> failwith $"%s{context}: the new directory %O{inode} has no path"

            let mode = PermissionBits.toInt (Inode.permissions made)
            $"ok:mode=%04o{mode}:gid=%s{group}:new=%s{escapedBytes where}"
        | other -> failwith $"%s{context}: mkdir answered success and made %d{other.Length} inodes"

    /// `mkdirat-rules.c`'s PATH, MODE and ORDER rows for one caller, end to
    /// end. Each `at` cell is `mkdirat` from a descriptor on `d` with the
    /// current directory at `w`, and each `plain` cell `mkdir` with the current
    /// directory at `d`; the probe measured them equal in every row, so the
    /// starting directory is the only thing a descriptor changes.
    let private replayMkdirAtRules (resource : string) (envelope : Envelope) : unit =
        let caller = $"caller=%d{UserId.toUInt32 envelope.Credentials.EffectiveUser}"

        let rowsOf (table : string) =
            probeLines resource table |> List.filter (fun row -> List.head row = caller)

        let pairs = rowsOf "PATH" @ rowsOf "MODE"
        let order = rowsOf "ORDER"
        pairs.Length |> shouldEqual (26 + 12)
        order.Length |> shouldEqual 9

        let field (prefix : string) (cell : string) : string =
            if cell.StartsWith prefix then
                cell.Substring prefix.Length
            else
                failwith $"%s{context}: expected a %s{prefix} field, got %s{cell}"

        let pathOf (label : string) : PathArgumentBytes =
            match label with
            | "NULL" -> PathArgumentBytes.Unreadable
            | "empty" -> PathArg.ofText ""
            | "xff3" -> PathArg.ofBytes [ 0xffuy ; 0xffuy ; 0xffuy ]
            | "ro/xff3" -> PathArg.ofBytes [ byte 'r' ; byte 'o' ; byte '/' ; 0xffuy ; 0xffuy ; 0xffuy ]
            | text -> PathArg.ofText text

        let modeOf (cell : string) : int =
            match field "mode=" cell with
            | "ffff" -> 0xffff
            | octal -> Convert.ToInt32 (octal, 8)

        [
            for row in pairs do
                match row with
                | [ _ ; label ; mode ; at ; plain ] ->
                    let at = field "at=" at
                    let plain = field "plain=" plain
                    let path = pathOf label
                    let mode = modeOf mode

                    if at <> plain then
                        yield $"%s{label} %d{mode}: the probe's mkdirat answered %s{at} and its mkdir %s{plain}"

                    let fd, system = opened "d" (mkdirAtCell envelope "/c/w")

                    let actual = UnixNamespace.mkdirat fd path mode system |> renderedCreation system

                    if actual <> at then
                        yield $"mkdirat %s{label} 0o%o{mode}: the probe answered %s{at}, this library %s{actual}"

                    let system = mkdirAtCell envelope "/c/w/d"
                    let actual = UnixNamespace.mkdir path mode system |> renderedCreation system

                    if actual <> plain then
                        yield $"mkdir %s{label} 0o%o{mode}: the probe answered %s{plain}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"

            for row in order do
                match row with
                | [ _ ; dirfd ; path ; mode ; at ] ->
                    let system = mkdirAtCell envelope "/c/w"

                    let fd, system =
                        match field "dirfd=" dirfd with
                        | "dir" -> opened "d" system
                        | "file" -> opened "d/f" system
                        | "minus1" -> -1, system
                        | other -> failwith $"%s{context}: the probe has no dirfd %s{other}"

                    let expected = field "at=" at

                    let actual =
                        UnixNamespace.mkdirat fd (pathOf (field "path=" path)) (modeOf mode) system
                        |> renderedCreation system

                    if actual <> expected then
                        yield $"ORDER %s{dirfd} %s{path}: the probe answered %s{expected}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``mkdirat decides as mkdir from its directory, as Linux root measured`` () : unit =
        replayMkdirAtRules "WoofWare.PosixKernel.Test.mkdiratRules.linux.txt" linuxRoot

    [<Test>]
    let ``mkdirat decides as mkdir from its directory, as a Linux user measured`` () : unit =
        replayMkdirAtRules "WoofWare.PosixKernel.Test.mkdiratRules.linux.txt" linuxUser

    [<Test>]
    let ``mkdirat decides as mkdir from its directory, as Darwin measured`` () : unit =
        replayMkdirAtRules "WoofWare.PosixKernel.Test.mkdiratRules.darwin.txt" darwinUser

    // ------------------------------------------------------------ unlinkat

    let private atRemoveDir (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x200
        | SimulatedUnixFlavour.Darwin -> 0x80

    [<Test>]
    let ``AT_REMOVEDIR is each flavour's own number, as the probe printed it`` () : unit =
        for envelope in envelopes do
            let constants = probeLines envelope.Resource "CONST" |> List.exactlyOne

            let printed =
                constants
                |> List.pick (fun cell ->
                    if cell.StartsWith "AT_REMOVEDIR=0x" then
                        Some (Convert.ToInt32 (cell.Substring "AT_REMOVEDIR=0x".Length, 16))
                    else
                        None
                )

            UnlinkAtRules.atRemoveDir (SimulatedUnixPlatform.flavour envelope.Platform)
            |> shouldEqual printed

            atRemoveDir (SimulatedUnixPlatform.flavour envelope.Platform)
            |> shouldEqual printed

    let private renderedUnlinkAt (result : Result<SyscallAnswer * UnixSystem<int, string>, UnlinkAtRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{UnlinkAtRefusal.describe refusal}"

    let private replayUnlinkAt (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform

        let rows =
            atRows envelope
            |> Map.filter (fun (call, _, _) _ -> call = "unlinkat" || call = "unlinkat(REMOVEDIR)")

        rows.Count |> shouldEqual (2 * 13 * 9)
        let skipped = faccessatNotReplayed envelope

        [
            for KeyValue ((call, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{call} %s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let flags = if call = "unlinkat" then 0 else atRemoveDir flavour

                        let actual =
                            UnixNamespace.unlinkat dirfd (pathArgument path envelope.Platform) flags system
                            |> renderedUnlinkAt

                        if actual <> expected then
                            yield
                                $"%s{call} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``unlinkat answers every dirfd and path the probe tried as Linux root`` () : unit = replayUnlinkAt linuxRoot

    [<Test>]
    let ``unlinkat answers every dirfd and path the probe tried as a Linux user`` () : unit = replayUnlinkAt linuxUser

    [<Test>]
    let ``unlinkat answers every dirfd and path the probe tried on Darwin`` () : unit = replayUnlinkAt darwinUser

    let private replayUnlinkAtFlags (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let atFdCwd = atFdCwd flavour

        let row =
            probeLines envelope.Resource "FLAGS"
            |> List.find (fun row -> row.[0] = "unlinkat")

        row.[1] |> shouldEqual "0=ok"

        let rejected =
            (List.item 2 row).Substring("rejected:".Length).Split (' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun token ->
                match token.Split '=' with
                | [| bit ; error |] -> Convert.ToInt32 (bit, 16), error
                | _ -> Convert.ToInt32 (token, 16), "EINVAL"
            )
            |> Map.ofSeq

        let refused =
            [
                for bit in 0..31 do
                    let flag = 1 <<< bit
                    let expected = Map.tryFind flag rejected |> Option.defaultValue "ok"

                    let actual =
                        UnixNamespace.unlinkat atFdCwd (PathArg.ofText "f") flag (boot envelope)
                        |> renderedUnlinkAt

                    if actual.StartsWith "refused: the flag word" then
                        yield flag
                    elif actual <> expected then
                        failwith
                            $"%s{context}: unlinkat with flags 0x%x{flag}: the probe answered %s{expected}, this library %s{actual}"
            ]

        // Every flag refused is one the probe saw accepted.
        let expectedRefused =
            match flavour with
            | SimulatedUnixFlavour.Linux -> []
            | SimulatedUnixFlavour.Darwin -> [ 0x100 ; 0x800 ; 0x1000 ; 0x2000 ; 0x4000 ; 0x8000 ]

        refused |> shouldEqual expectedRefused

        for flag in refused do
            Map.tryFind flag rejected |> Option.defaultValue "ok" |> shouldNotEqual "EINVAL"

        let order =
            probeLines envelope.Resource "FLAGORDER"
            |> List.find (fun row -> row.[0] = "unlinkat")

        order.[1] |> shouldEqual "flags=0x40000000"

        [
            for cell in order.[2..] do
                let at = cell.IndexOf '='
                let label = cell.Substring (0, at)
                let expected = cell.Substring (at + 1)

                let dirfd, path =
                    match label with
                    | "NULL" -> atFdCwd, PathArgumentBytes.Unreadable
                    | "minus1+f" -> -1, PathArg.ofText "f"
                    | "empty" -> atFdCwd, PathArg.ofText ""
                    | "nx" -> atFdCwd, PathArg.ofText "nx"
                    | other -> failwith $"%s{context}: the probe has no FLAGORDER cell %s{other}"

                let actual =
                    UnixNamespace.unlinkat dirfd path 0x40000000 (boot envelope) |> renderedUnlinkAt

                if actual <> expected then
                    yield $"FLAGORDER %s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``unlinkat screens every flag bit as measured as Linux root`` () : unit = replayUnlinkAtFlags linuxRoot

    [<Test>]
    let ``unlinkat screens every flag bit as measured as a Linux user`` () : unit = replayUnlinkAtFlags linuxUser

    [<Test>]
    let ``unlinkat screens every flag bit as measured on Darwin`` () : unit = replayUnlinkAtFlags darwinUser

    /// `unlinkat-rules.c`'s cell, with the current directory at `cwd`: `/c/w`
    /// holding `d/`, which holds a file `f`, empty directories `sub/` and
    /// `e/`, `full/` holding `x`, the links `dang -> nx2`, `cyc -> cyc`,
    /// `lf -> f`, `ld -> sub` and `lroot -> /`, and an unwritable `ro/`
    /// holding a file `kid`, an empty `kdir/` and `kfull/` holding `x`, all the
    /// caller's, made under umask 022. On Linux, where the probe started as
    /// root, `d` also holds root's sticky `st/` (01777), holding uid 2000's
    /// file `of` and empty directory `od/`, and the caller's file `mine`.
    let private unlinkAtCell (envelope : Envelope) (cwd : string) : UnixSystem<int, string> =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let caller = InodeOwner.ofProcess envelope.Credentials
        let file = SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, None)

        let ownedFile (owner : InodeOwner) =
            SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, Some owner)

        let link (target : string) =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

        let dir (bits : int) (owner : InodeOwner option) (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        let sticky =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let rootOwner = InodeOwner.ofProcess Owners.root

                let other =
                    {
                        User = UserId.parseOrFail context 2000u
                        Group = GroupId.parseOrFail context 2000u
                    }

                [
                    "st",
                    dir
                        0o1777
                        (Some rootOwner)
                        [ "of", ownedFile other ; "od", dir 0o755 (Some other) [] ; "mine", file ]
                ]
            | SimulatedUnixFlavour.Darwin -> []

        let seed =
            Map.ofList
                [
                    name "c",
                    dir
                        0o777
                        None
                        [
                            "w",
                            dir
                                0o755
                                None
                                [
                                    "d",
                                    dir
                                        0o755
                                        None
                                        ([
                                            "f", file
                                            "sub", dir 0o755 None []
                                            "e", dir 0o755 None []
                                            "full", dir 0o755 None [ "x", file ]
                                            "dang", link "nx2"
                                            "cyc", link "cyc"
                                            "lf", link "f"
                                            "ld", link "sub"
                                            "lroot", link "/"
                                            "ro",
                                            dir
                                                0o555
                                                None
                                                [
                                                    "kid", file
                                                    "kdir", dir 0o755 None []
                                                    "kfull", dir 0o755 None [ "x", file ]
                                                ]
                                         ]
                                         @ sticky)
                                ]
                        ]
                ]

        let image : UnixBootImage<int, string> =
            UnixSystem.initial envelope.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context envelope.Credentials

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                caller
                seed
                (AbsoluteUnixPath.parseOrFail context cwd)
                image
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"%s{context}: could not build unlinkat-rules.c's cell: %A{fault}"

    /// Every path under the cell `/c`, relative to it, as `unlinkat-rules.c`
    /// collects them: a directory's entries, never following a link.
    let private cellPaths (system : UnixSystem<int, string>) : Set<string> =
        let vfs = system.Machine.FileSystem

        let rec under (prefix : string) (inode : InodeNumber) : string seq =
            match VirtualFileSystem.tryGetDirectory inode vfs with
            | None -> Seq.empty
            | Some content ->
                content.Entries
                |> Map.toSeq
                |> Seq.collect (fun (entry, child) ->
                    let path = prefix + DirectoryEntryName.toEscaped entry
                    Seq.append (Seq.singleton path) (under (path + "/") child)
                )

        let cell =
            match
                PathWalk.resolveExisting
                    (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
                    Owners.root
                    SymlinkProtection.Off
                    (VirtualFileSystem.root vfs)
                    SymlinkPolicy.NoFollowFinal
                    (UnixPath.parseOrFail context "/c")
                    vfs
            with
            | Ok inode -> inode
            | Error failure -> failwith $"%s{context}: /c does not resolve: %A{failure}"

        under "" cell |> Set.ofSeq

    /// The probe's ":gone=" list in a canonical order: `nftw` lists in the
    /// order the directory returns its entries.
    let private canonicalGone (answer : string) : string =
        match answer.IndexOf ":gone=" with
        | -1 -> answer
        | at ->
            let gone =
                answer.Substring(at + ":gone=".Length).Split (',', StringSplitOptions.RemoveEmptyEntries)
                |> Array.sort

            answer.Substring (0, at) + ":gone=" + String.Join (",", gone)

    let private goneSince (before : UnixSystem<int, string>) (after : UnixSystem<int, string>) : string =
        let gone =
            Set.difference (cellPaths before) (cellPaths after) |> Set.toArray |> Array.sort

        ":gone=" + String.Join (",", gone)

    /// What `unlinkat-rules.c` prints for one call made in `before`.
    let private renderedRemoval
        (before : UnixSystem<int, string>)
        (result : Result<SyscallAnswer * UnixSystem<int, string>, UnlinkAtRefusal>)
        : string
        =
        match result with
        | Error refusal -> $"refused: %s{UnlinkAtRefusal.describe refusal}"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Ok (SyscallAnswer.Completed _, after) -> "ok" + goneSince before after

    /// `unlink` or `rmdir` of `path` in `system`, by the probe's name for it.
    let private plainRemoval
        (call : string)
        (path : PathArgumentBytes)
        (system : UnixSystem<int, string>)
        : Result<SyscallAnswer * UnixSystem<int, string>, UnlinkAtRefusal>
        =
        match call with
        | "unlink" -> UnixNamespace.unlink path system
        | "rmdir" -> UnixNamespace.rmdir path system
        | other -> failwith $"%s{context}: the probe has no call %s{other}"
        |> Result.mapError UnlinkAtRefusal.Removal

    /// `unlinkat-rules.c`'s PATH, SELF and FLAGS2 rows for one caller, end to
    /// end. Each PATH `at` cell is `unlinkat` from a descriptor on `d` with the
    /// current directory at `w`, and each `plain` cell `unlink` or `rmdir` with
    /// the current directory at `d`; the probe measured them equal in every
    /// row, so the starting directory is the only thing a descriptor changes.
    let private replayUnlinkAtRules (resource : string) (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let caller = $"caller=%d{UserId.toUInt32 envelope.Credentials.EffectiveUser}"

        let rowsOf (table : string) =
            probeLines resource table |> List.filter (fun row -> List.head row = caller)

        let paths = rowsOf "PATH"
        let self = rowsOf "SELF"

        let stickyRows =
            match flavour with
            | SimulatedUnixFlavour.Linux -> 3
            | SimulatedUnixFlavour.Darwin -> 0

        paths.Length |> shouldEqual (2 * (31 + stickyRows))
        self.Length |> shouldEqual 7

        let field (prefix : string) (cell : string) : string =
            if cell.StartsWith prefix then
                cell.Substring prefix.Length
            else
                failwith $"%s{context}: expected a %s{prefix} field, got %s{cell}"

        let flagsOf (call : string) : int =
            match call with
            | "unlink" -> 0
            | "rmdir" -> atRemoveDir flavour
            | other -> failwith $"%s{context}: the probe has no call %s{other}"

        [
            for row in paths do
                match row with
                | [ _ ; call ; label ; at ; plain ] ->
                    let at = field "at=" at |> canonicalGone
                    let plain = field "plain=" plain |> canonicalGone
                    let path = PathArg.ofText label

                    if at <> plain then
                        yield $"%s{call} %s{label}: the probe's unlinkat answered %s{at} and its %s{call} %s{plain}"

                    let fd, system = opened "d" (unlinkAtCell envelope "/c/w")

                    let actual =
                        UnixNamespace.unlinkat fd path (flagsOf call) system |> renderedRemoval system

                    if actual <> at then
                        yield $"unlinkat %s{call} %s{label}: the probe answered %s{at}, this library %s{actual}"

                    let system = unlinkAtCell envelope "/c/w/d"
                    let actual = plainRemoval call path system |> renderedRemoval system

                    if actual <> plain then
                        yield $"%s{call} %s{label}: the probe answered %s{plain}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"

            for row in self do
                match row with
                | [ _ ; thenCall ; label ; at ; plain ] ->
                    let thenCall = field "then=" thenCall
                    let at = field "at=" at |> canonicalGone
                    let plain = field "plain=" plain |> canonicalGone
                    let path = PathArg.ofText label

                    let answer (result : Result<SyscallAnswer * UnixSystem<int, string>, UnlinkAtRefusal>) =
                        match result with
                        | Ok (SyscallAnswer.Completed _, after) -> "ok", after
                        | Ok (SyscallAnswer.Failed error, after) -> $"%A{error}", after
                        | Error refusal -> failwith $"%s{context}: refused: %s{UnlinkAtRefusal.describe refusal}"

                    // From a descriptor on d/sub, with the current directory at w.
                    let fd, before = opened "d/sub" (unlinkAtCell envelope "/c/w")

                    let first, system =
                        UnixNamespace.unlinkat fd (PathArg.ofText "../sub") (atRemoveDir flavour) before
                        |> answer

                    let second, after =
                        UnixNamespace.unlinkat fd path (flagsOf thenCall) system |> answer

                    let actual = $"%s{first};%s{second}" + goneSince before after

                    if actual <> at then
                        yield $"SELF unlinkat then %s{label}: the probe answered %s{at}, this library %s{actual}"

                    // From the current directory d/sub.
                    let before = unlinkAtCell envelope "/c/w/d/sub"

                    let first, system = plainRemoval "rmdir" (PathArg.ofText "../sub") before |> answer

                    let second, after = plainRemoval thenCall path system |> answer
                    let actual = $"%s{first};%s{second}" + goneSince before after

                    if actual <> plain then
                        yield $"SELF %s{thenCall} %s{label}: the probe answered %s{plain}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``unlinkat decides as unlink and rmdir from its directory, as Linux root measured`` () : unit =
        replayUnlinkAtRules "WoofWare.PosixKernel.Test.unlinkatRules.linux.txt" linuxRoot

    [<Test>]
    let ``unlinkat decides as unlink and rmdir from its directory, as a Linux user measured`` () : unit =
        replayUnlinkAtRules "WoofWare.PosixKernel.Test.unlinkatRules.linux.txt" linuxUser

    [<Test>]
    let ``unlinkat decides as unlink and rmdir from its directory, as Darwin measured`` () : unit =
        replayUnlinkAtRules "WoofWare.PosixKernel.Test.unlinkatRules.darwin.txt" darwinUser

    /// `unlinkat-rules.c`'s FLAGS2 rows: words of more than one bit, from
    /// `AT_FDCWD` with the current directory at `d`. A word carrying a bit the
    /// flavour rejects is EINVAL whatever else it carries; one that carries
    /// only accepted bits and some this library does not model is refused.
    let private replayUnlinkAtWords (resource : string) (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let rows = probeLines resource "FLAGS2"

        rows.Length
        |> shouldEqual (
            match flavour with
            | SimulatedUnixFlavour.Linux -> 4
            | SimulatedUnixFlavour.Darwin -> 2 + 3 * 6
        )

        [
            for row in rows do
                let flags = Convert.ToInt32 ((List.head row).Substring "flags=0x".Length, 16)

                for cell in List.tail row do
                    let at = cell.IndexOf '='
                    let label = cell.Substring (0, at)
                    let expected = cell.Substring (at + 1) |> canonicalGone

                    let dirfd, path =
                        match label with
                        | "f" -> atFdCwd flavour, PathArg.ofText "f"
                        | "sub" -> atFdCwd flavour, PathArg.ofText "sub"
                        | "NULL" -> atFdCwd flavour, PathArgumentBytes.Unreadable
                        | "minus1+f" -> -1, PathArg.ofText "f"
                        | other -> failwith $"%s{context}: the probe has no FLAGS2 cell %s{other}"

                    let system = unlinkAtCell envelope "/c/w/d"

                    match UnixNamespace.unlinkat dirfd path flags system with
                    | Error (UnlinkAtRefusal.UnmodelledFlags word) ->
                        // Refused only where the probe saw the word accepted.
                        if word <> flags || expected = "EINVAL" then
                            yield $"0x%x{flags} %s{label}: the probe answered %s{expected}, this library refused"
                    | result ->
                        let actual = renderedRemoval system result

                        if actual <> expected then
                            yield $"0x%x{flags} %s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``unlinkat screens a word of several bits as Linux measured`` () : unit =
        replayUnlinkAtWords "WoofWare.PosixKernel.Test.unlinkatRules.linux.txt" linuxRoot

    [<Test>]
    let ``unlinkat screens a word of several bits as Darwin measured`` () : unit =
        replayUnlinkAtWords "WoofWare.PosixKernel.Test.unlinkatRules.darwin.txt" darwinUser

    // ------------------------------------------------------------ renameat

    let private renderedRenameAt (result : Result<SyscallAnswer * UnixSystem<int, string>, RenameRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{RenameRefusal.describe refusal}"

    /// `at-dirfd.c`'s `renameat` rows on one side: `renameat(dirfd, path,
    /// AT_FDCWD, "renamed")` for the old side, `renameat(AT_FDCWD, "f2", dirfd,
    /// path)` for the new.
    let private replayRenameAt (side : string) (envelope : Envelope) : unit =
        let call = $"renameat[%s{side}]"
        let rows = atRows envelope |> Map.filter (fun (c, _, _) _ -> c = call)
        let skipped = faccessatNotReplayed envelope
        let atFdCwd = atFdCwd (SimulatedUnixPlatform.flavour envelope.Platform)

        rows.Count |> shouldEqual (13 * 9)

        [
            for KeyValue ((_, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{call} %s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let p = pathArgument path envelope.Platform

                        let actual =
                            match side with
                            | "old" -> UnixNamespace.renameat dirfd p atFdCwd (PathArg.ofText "renamed") system
                            | _ -> UnixNamespace.renameat atFdCwd (PathArg.ofText "f2") dirfd p system
                            |> renderedRenameAt

                        if actual <> expected then
                            yield
                                $"%s{call} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``renameat's old side answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            replayRenameAt "old" envelope

    [<Test>]
    let ``renameat's new side answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            replayRenameAt "new" envelope

    /// `at-dirfd.c`'s ORDER2 rows for `renameat`: a bad argument on each side
    /// at once, from the probe's cell. Linux reads the new side's pathname and
    /// `dirfd` before the old side's final name, so an absent old name loses
    /// to the new side's EFAULT and EBADF; Darwin finishes the old side first.
    let private replayRenameAtOrder2 (envelope : Envelope) : unit =
        let atFdCwd = atFdCwd (SimulatedUnixPlatform.flavour envelope.Platform)

        let rows =
            probeLines envelope.Resource "ORDER2"
            |> List.filter (fun row -> List.head row = "renameat")

        rows.Length |> shouldEqual 5

        let side (state : string) (isOld : bool) : int * PathArgumentBytes =
            match state with
            | "good" -> atFdCwd, PathArg.ofText (if isOld then "f" else "new")
            | "badfd" -> -1, PathArg.ofText (if isOld then "f" else "new")
            // A new side that exists.
            | "absent" -> atFdCwd, PathArg.ofText (if isOld then "nx" else "f2")
            | "NULL" -> atFdCwd, PathArgumentBytes.Unreadable
            | "nodir" -> atFdCwd, PathArg.ofText (if isOld then "nxdir/f" else "nxdir/new")
            | other -> failwith $"%s{context}: the probe has no ORDER2 state %s{other}"

        [
            for row in rows do
                let oldState = row.[1].Substring "old=".Length
                let olddirfd, oldpath = side oldState true

                for cell in row.[2..] do
                    let at = cell.IndexOf '='
                    let newState = cell.Substring ("new:".Length, at - "new:".Length)
                    let expected = cell.Substring (at + 1)
                    let newdirfd, newpath = side newState false

                    let actual =
                        UnixNamespace.renameat olddirfd oldpath newdirfd newpath (boot envelope)
                        |> renderedRenameAt

                    if actual <> expected then
                        yield
                            $"old=%s{oldState} new=%s{newState}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``renameat orders a bad argument on each side as the probe measured, under every envelope`` () : unit =
        for envelope in envelopes do
            replayRenameAtOrder2 envelope

    /// `renameat-rules.c`'s cell, with the current directory at `cwd`: `/c/w`
    /// holding `d/` and `d2/`. `d` holds a file `f` and `g`, a second name for
    /// it; empty directories `sub/` and `e/`; `full/` holding `x`; `nest/in/`;
    /// the links `dang -> nx2`, `cyc -> cyc`, `lf -> f`, `ld -> sub` and
    /// `lroot -> /`; and an unwritable `ro/` holding a file `kid`, an empty
    /// `kdir/` and `kfull/` holding `x`. `d2` holds a file `h`, an empty `hd/`
    /// and `hfull/` holding `x`. All the caller's, made under umask 022. On
    /// Linux, where the probe started as root, `d` also holds root's sticky
    /// `st/` (01777), holding uid 2000's file `of` and empty directory `od/`,
    /// and the caller's file `mine`.
    let private renameAtCell (envelope : Envelope) (cwd : string) : UnixSystem<int, string> =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let caller = InodeOwner.ofProcess envelope.Credentials
        let file = SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, None)

        let ownedFile (owner : InodeOwner) =
            SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, Some owner)

        let link (target : string) =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

        let dir (bits : int) (owner : InodeOwner option) (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        let sticky =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                let rootOwner = InodeOwner.ofProcess Owners.root

                let other =
                    {
                        User = UserId.parseOrFail context 2000u
                        Group = GroupId.parseOrFail context 2000u
                    }

                [
                    "st",
                    dir
                        0o1777
                        (Some rootOwner)
                        [ "of", ownedFile other ; "od", dir 0o755 (Some other) [] ; "mine", file ]
                ]
            | SimulatedUnixFlavour.Darwin -> []

        let seed =
            Map.ofList
                [
                    name "c",
                    dir
                        0o777
                        None
                        [
                            "w",
                            dir
                                0o755
                                None
                                [
                                    "d",
                                    dir
                                        0o755
                                        None
                                        ([
                                            "f", file
                                            "sub", dir 0o755 None []
                                            "e", dir 0o755 None []
                                            "full", dir 0o755 None [ "x", file ]
                                            "nest", dir 0o755 None [ "in", dir 0o755 None [] ]
                                            "dang", link "nx2"
                                            "cyc", link "cyc"
                                            "lf", link "f"
                                            "ld", link "sub"
                                            "lroot", link "/"
                                            "ro",
                                            dir
                                                0o555
                                                None
                                                [
                                                    "kid", file
                                                    "kdir", dir 0o755 None []
                                                    "kfull", dir 0o755 None [ "x", file ]
                                                ]
                                         ]
                                         @ sticky)
                                    "d2",
                                    dir
                                        0o755
                                        None
                                        [ "h", file ; "hd", dir 0o755 None [] ; "hfull", dir 0o755 None [ "x", file ] ]
                                ]
                        ]
                ]

        let image : UnixBootImage<int, string> =
            UnixSystem.initial envelope.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context envelope.Credentials

        let system =
            match
                UnixBootImage.withFileSystemAndCurrentDirectory
                    epoch
                    caller
                    seed
                    (AbsoluteUnixPath.parseOrFail context cwd)
                    image
            with
            | Ok image -> UnixBootImage.boot image
            | Error fault -> failwith $"%s{context}: could not build renameat-rules.c's cell: %A{fault}"

        // The probe made `g` with link(2), after `f`.
        UnixNamespace.link (PathArg.ofText "/c/w/d/f") (PathArg.ofText "/c/w/d/g") system
        |> function
            | Ok answer -> completed "link(d/f, d/g)" answer
            | Error refusal -> failwith $"%s{context}: link was refused: %s{LinkRefusal.describe refusal}"

    /// Bytes as `renameat-rules.c` prints a path: printable ASCII but the
    /// backslash as itself, any other byte as `\xNN`.
    let private escapedRenameBytes (bytes : byte seq) : string =
        bytes
        |> Seq.map (fun b ->
            if b < 0x20uy || b >= 0x7fuy || b = byte '\\' then
                $"\\x%02x{b}"
            else
                string (char b)
        )
        |> String.concat ""

    /// The bytes a path `renameat-rules.c` printed stands for.
    let private unescapedRenameBytes (text : string) : byte list =
        let rec go (i : int) : byte list =
            if i >= text.Length then
                []
            elif text.[i] = '\\' then
                Convert.ToByte (text.Substring (i + 2, 2), 16) :: go (i + 4)
            else
                byte text.[i] :: go (i + 1)

        go 0

    /// Every path under the cell `/c`, relative to it, with the inode it
    /// names, never following a link, in byte order: each path is held one
    /// byte to a character, so that ordinal order is byte order.
    let private cellInodes (system : UnixSystem<int, string>) : (string * InodeNumber) list =
        let vfs = system.Machine.FileSystem

        let rec under (prefix : string) (inode : InodeNumber) : (string * InodeNumber) seq =
            match VirtualFileSystem.tryGetDirectory inode vfs with
            | None -> Seq.empty
            | Some content ->
                content.Entries
                |> Map.toSeq
                |> Seq.collect (fun (entry, child) ->
                    let bytes = DirectoryEntryName.toByteString entry |> UnixByteString.toBytes
                    let path = prefix + String (bytes |> Seq.map char |> Array.ofSeq)
                    Seq.append (Seq.singleton (path, child)) (under (path + "/") child)
                )

        let cell =
            match
                PathWalk.resolveExisting
                    (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
                    Owners.root
                    SymlinkProtection.Off
                    (VirtualFileSystem.root vfs)
                    SymlinkPolicy.NoFollowFinal
                    (UnixPath.parseOrFail context "/c")
                    vfs
            with
            | Ok inode -> inode
            | Error failure -> failwith $"%s{context}: /c does not resolve: %A{failure}"

        under "" cell
        |> List.ofSeq
        |> List.sortWith (fun (a, _) (b, _) -> String.CompareOrdinal (a, b))

    /// What `renameat-rules.c` prints for a call made in `before`: the errno,
    /// or "ok" with the paths that went and every path whose inode is not the
    /// one it had, with the first path that had that inode.
    let private renderedRename
        (before : UnixSystem<int, string>)
        (result : Result<SyscallAnswer * UnixSystem<int, string>, RenameRefusal>)
        : string
        =
        match result with
        | Error refusal -> $"refused: %s{RenameRefusal.describe refusal}"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Ok (SyscallAnswer.Completed _, after) ->

        let escaped (path : string) =
            path |> Seq.map byte |> escapedRenameBytes

        let was = cellInodes before
        let now = cellInodes after
        let wasMap = Map.ofList was
        let nowMap = Map.ofList now

        let gone =
            was
            |> List.filter (fun (path, _) -> not (nowMap.ContainsKey path))
            |> List.map (fst >> escaped)

        let moved =
            now
            |> List.choose (fun (path, inode) ->
                match Map.tryFind path wasMap with
                | Some previous when previous = inode -> None
                | _ ->
                    let origin =
                        was
                        |> List.tryFind (fun (_, previous) -> previous = inode)
                        |> Option.map (fst >> escaped)
                        |> Option.defaultValue "?"

                    Some $"%s{escaped path}<-%s{origin}"
            )

        $"""ok:gone=%s{String.Join (",", gone)}:moved=%s{String.Join (",", moved)}"""

    /// `renameat-rules.c`'s rows the model does not replay under `envelope`,
    /// as (section, old, new), and why. Darwin's `lroot/.` reaches `/` through
    /// a link, and on the probe's machine `/` is the read-only system volume
    /// while the cell is on the data volume, so the rename is EXDEV there; this
    /// library holds one filesystem, where the root is any other directory
    /// `rename` meets (EINVAL, as `TestRenameRules` measured on an APFS image).
    let private renameAtNotReplayed (envelope : Envelope) : Set<string * string * string> =
        match SimulatedUnixPlatform.flavour envelope.Platform with
        | SimulatedUnixFlavour.Linux -> Set.empty
        | SimulatedUnixFlavour.Darwin -> set [ "SAME", "lroot/.", "nx" ]

    /// `renameat-rules.c`'s ROW and SELF rows for one caller, end to end. Each
    /// `at` cell is `renameat` with the current directory at `w`, from a
    /// descriptor on the directory each side names; each `plain` cell
    /// `rename` with the current directory where the row says. The probe
    /// measured them equal in every row, so where a walk starts is the only
    /// thing a descriptor changes, on either side.
    let private replayRenameAtRules (resource : string) (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let caller = $"caller=%d{UserId.toUInt32 envelope.Credentials.EffectiveUser}"

        let rowsOf (table : string) =
            probeLines resource table |> List.filter (fun row -> List.head row = caller)

        let rows = rowsOf "ROW"
        let self = rowsOf "SELF"
        let skipped = renameAtNotReplayed envelope

        let stickyRows =
            match flavour with
            | SimulatedUnixFlavour.Linux -> 6
            | SimulatedUnixFlavour.Darwin -> 0

        rows.Length |> shouldEqual (54 + stickyRows + 25 + 6 + 6)
        self.Length |> shouldEqual 12

        let field (prefix : string) (cell : string) : string =
            if cell.StartsWith prefix then
                cell.Substring prefix.Length
            else
                failwith $"%s{context}: expected a %s{prefix} field, got %s{cell}"

        let pathOf (printed : string) : PathArgumentBytes =
            PathArg.ofBytes (unescapedRenameBytes printed)

        let descriptor (dir : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
            match dir with
            | "cwd" -> atFdCwd flavour, system
            | dir -> opened dir system

        let firstAnswer (result : Result<SyscallAnswer * UnixSystem<int, string>, 'Refusal>) =
            match result with
            | Ok (SyscallAnswer.Completed _, after) -> "ok", after
            | Ok (SyscallAnswer.Failed error, after) -> $"%A{error}", after
            | Error refusal -> failwith $"%s{context}: the first call was refused: %A{refusal}"

        [
            for row in rows do
                match row with
                | [ _ ; section ; ofd ; old ; nfd ; newPath ; cwd ; pold ; pnew ; at ; plain ] ->
                    let ofd = field "ofd=" ofd
                    let old = field "old=" old
                    let nfd = field "nfd=" nfd
                    let newPath = field "new=" newPath
                    let cwd = field "cwd=" cwd
                    let at = field "at=" at
                    let plain = field "plain=" plain

                    if at <> plain then
                        yield
                            $"%s{section} %s{old} %s{newPath}: the probe's renameat answered %s{at} and its rename %s{plain}"

                    if not (skipped.Contains (section, old, newPath)) then
                        let system = renameAtCell envelope "/c/w"
                        let olddirfd, system = descriptor ofd system
                        let newdirfd, system = descriptor nfd system

                        let actual =
                            UnixNamespace.renameat olddirfd (pathOf old) newdirfd (pathOf newPath) system
                            |> renderedRename system

                        if actual <> at then
                            yield
                                $"renameat %s{section} %s{ofd}:%s{old} %s{nfd}:%s{newPath}: the probe answered %s{at}, this library %s{actual}"

                        let system = renameAtCell envelope (if cwd = "." then "/c/w" else "/c/w/" + cwd)

                        let actual =
                            UnixNamespace.rename (pathOf (field "pold=" pold)) (pathOf (field "pnew=" pnew)) system
                            |> renderedRename system

                        if actual <> plain then
                            yield
                                $"rename %s{section} %s{pold} %s{pnew}: the probe answered %s{plain}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"

            for row in self do
                match row with
                | [ _ ; first ; old ; newPath ; at ; plain ] ->
                    let first = field "first=" first
                    let old = pathOf (field "old=" old)
                    let newPath = pathOf (field "new=" newPath)
                    let at = field "at=" at
                    let plain = field "plain=" plain

                    if at <> plain then
                        yield $"SELF %s{first}: the probe's renameat answered %s{at} and its rename %s{plain}"

                    // Through a descriptor on d/sub, with the current directory at w.
                    let fd, before = opened "d/sub" (renameAtCell envelope "/c/w")

                    let firstDone, system =
                        match first with
                        | "rename" ->
                            UnixNamespace.renameat fd (PathArg.ofText "../sub") fd (PathArg.ofText "../sub2") before
                            |> firstAnswer
                        | _ ->
                            UnixNamespace.unlinkat fd (PathArg.ofText "../sub") (atRemoveDir flavour) before
                            |> firstAnswer

                    let actual =
                        firstDone
                        + ";"
                        + (UnixNamespace.renameat fd old fd newPath system |> renderedRename before)

                    if actual <> at then
                        yield
                            $"SELF renameat %s{first} %A{old} %A{newPath}: the probe answered %s{at}, this library %s{actual}"

                    // From the current directory d/sub.
                    let before = renameAtCell envelope "/c/w/d/sub"

                    let firstDone, system =
                        match first with
                        | "rename" ->
                            UnixNamespace.rename (PathArg.ofText "../sub") (PathArg.ofText "../sub2") before
                            |> firstAnswer
                        | _ -> UnixNamespace.rmdir (PathArg.ofText "../sub") before |> firstAnswer

                    let actual =
                        firstDone
                        + ";"
                        + (UnixNamespace.rename old newPath system |> renderedRename before)

                    if actual <> plain then
                        yield
                            $"SELF rename %s{first} %A{old} %A{newPath}: the probe answered %s{plain}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``renameat decides as rename with each path written from the cwd, as Linux root measured`` () : unit =
        replayRenameAtRules "WoofWare.PosixKernel.Test.renameatRules.linux.txt" linuxRoot

    [<Test>]
    let ``renameat decides as rename with each path written from the cwd, as a Linux user measured`` () : unit =
        replayRenameAtRules "WoofWare.PosixKernel.Test.renameatRules.linux.txt" linuxUser

    [<Test>]
    let ``renameat decides as rename with each path written from the cwd, as Darwin measured`` () : unit =
        replayRenameAtRules "WoofWare.PosixKernel.Test.renameatRules.darwin.txt" darwinUser

    [<Test>]
    let ``only Darwin's rename of the root through a link is left out of renameat-rules.c's rows`` () : unit =
        envelopes
        |> List.map (fun envelope -> envelope.Label, renameAtNotReplayed envelope |> Set.toList)
        |> shouldEqual
            [
                "Linux root", []
                "Linux uid 1000", []
                "Darwin uid 501", [ "SAME", "lroot/.", "nx" ]
            ]

    /// `renameat-rules.c`'s ORDER rows for one caller: fourteen kinds of old
    /// side crossed with fourteen kinds of new side, from the current
    /// directory d, which say where each side's copy-in, `dirfd` and walk fall
    /// against the other's.
    let private replayRenameAtOrder (resource : string) (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let caller = $"caller=%d{UserId.toUInt32 envelope.Credentials.EffectiveUser}"

        let rows =
            probeLines resource "ORDER" |> List.filter (fun row -> List.head row = caller)

        rows.Length |> shouldEqual 14

        let side
            (kind : string)
            (isOld : bool)
            (system : UnixSystem<int, string>)
            : int * PathArgumentBytes * UnixSystem<int, string>
            =
            let named (oldName : string) (newName : string) = if isOld then oldName else newName

            match kind with
            | "good" -> atFdCwd flavour, PathArg.ofText (named "f" "new"), system
            | "existing" -> atFdCwd flavour, PathArg.ofText (named "nx" "g"), system
            | "NULL" -> atFdCwd flavour, PathArgumentBytes.Unreadable, system
            | "empty" -> atFdCwd flavour, PathArg.ofText "", system
            | "badfd" -> -1, PathArg.ofText (named "f" "new"), system
            | "filefd" ->
                let fd, system = opened "f" system
                fd, PathArg.ofText (named "f" "new"), system
            | "pipefd" ->
                match UnixPipe.pipe2 0 UserBuffer.Mapped system with
                | Ok (Pipe2Answer.Created (readFd, _), system) -> readFd, PathArg.ofText (named "f" "new"), system
                | other -> failwith $"%s{context}: pipe2 did not make a pipe: %A{other}"
            | "nodir" -> atFdCwd flavour, PathArg.ofText (named "nxdir/f" "nxdir/new"), system
            | "notdir" -> atFdCwd flavour, PathArg.ofText (named "f/x" "f/new"), system
            | "dot" -> atFdCwd flavour, PathArg.ofText ".", system
            | "root" -> atFdCwd flavour, PathArg.ofText "/", system
            | "trail" -> atFdCwd flavour, PathArg.ofText (named "f/" "new/"), system
            | "long" -> atFdCwd flavour, PathArg.ofText (String ('a', 300)), system
            | "overlong" ->
                atFdCwd flavour,
                PathArg.ofText (slashed (PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits envelope.Platform))),
                system
            | other -> failwith $"%s{context}: the probe has no ORDER kind %s{other}"

        [
            for row in rows do
                let oldKind = row.[1].Substring "old=".Length

                for cell in row.[2..] do
                    let at = cell.IndexOf '='
                    let newKind = cell.Substring ("new:".Length, at - "new:".Length)
                    let expected = cell.Substring (at + 1)
                    let system = renameAtCell envelope "/c/w/d"
                    let olddirfd, oldpath, system = side oldKind true system
                    let newdirfd, newpath, system = side newKind false system

                    let actual =
                        UnixNamespace.renameat olddirfd oldpath newdirfd newpath system
                        |> renderedRename system

                    if actual <> expected then
                        yield
                            $"old=%s{oldKind} new=%s{newKind}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``renameat orders its two sides as Linux root measured`` () : unit =
        replayRenameAtOrder "WoofWare.PosixKernel.Test.renameatRules.linux.txt" linuxRoot

    [<Test>]
    let ``renameat orders its two sides as a Linux user measured`` () : unit =
        replayRenameAtOrder "WoofWare.PosixKernel.Test.renameatRules.linux.txt" linuxUser

    [<Test>]
    let ``renameat orders its two sides as Darwin measured`` () : unit =
        replayRenameAtOrder "WoofWare.PosixKernel.Test.renameatRules.darwin.txt" darwinUser

    [<Test>]
    let ``renameat across the device filesystem is EXDEV from a descriptor on either side, as Linux measured``
        ()
        : unit
        =
        let rows = probeLines "WoofWare.PosixKernel.Test.renameatRules.linux.txt" "DEV"
        rows.Length |> shouldEqual (2 * 5)

        [
            for row in rows do
                match row with
                | [ callerField ; ofd ; old ; nfd ; newPath ; at ] ->
                    let envelope =
                        match callerField with
                        | "caller=0" -> linuxRoot
                        | "caller=1000" -> linuxUser
                        | other -> failwith $"%s{context}: the probe has no caller %s{other}"

                    let field (prefix : string) (cell : string) = cell.Substring prefix.Length
                    let system = renameAtCell envelope "/c/w/d"
                    let dev, system = opened "/dev" system

                    let dirfd (cell : string) =
                        match field "ofd=" cell with
                        | "dev" -> dev
                        | _ -> atFdCwd SimulatedUnixFlavour.Linux

                    let expected = field "at=" at

                    let actual =
                        UnixNamespace.renameat
                            (dirfd ofd)
                            (PathArg.ofText (field "old=" old))
                            (dirfd ("ofd=" + field "nfd=" nfd))
                            (PathArg.ofText (field "new=" newPath))
                            system
                        |> renderedRename system

                    if actual <> expected then
                        yield $"%A{row}: the probe answered %s{expected}, this library %s{actual}"
                | other -> failwith $"%s{context}: a malformed row %A{other}"
        ]
        |> shouldEqual []

    // ------------------------------------------------------------ fchmodat and fchownat

    let private renderedChange
        (describe : 'Refusal -> string)
        (result : Result<SyscallAnswer * UnixSystem<int, string>, 'Refusal>)
        : string
        =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{describe refusal}"

    /// `fchmodat` as the probe called it: mode 0644.
    let private probeFChModAt
        (dirfd : int)
        (path : PathArgumentBytes)
        (flags : int)
        (system : UnixSystem<int, string>)
        =
        UnixPathResolution.fchmodat dirfd path 0o644 flags system
        |> renderedChange FChModAtRefusal.describe

    /// `fchownat` as the probe called it: (uid_t)-1 and (gid_t)-1.
    let private probeFChOwnAt
        (dirfd : int)
        (path : PathArgumentBytes)
        (flags : int)
        (system : UnixSystem<int, string>)
        =
        UnixPathResolution.fchownat dirfd path None None flags system
        |> renderedChange FChOwnAtRefusal.describe

    let private replayAttributeChange
        (call : string)
        (answer : int -> PathArgumentBytes -> int -> UnixSystem<int, string> -> string)
        (envelope : Envelope)
        : unit
        =
        let rows = atRows envelope |> Map.filter (fun (c, _, _) _ -> c = call)
        rows.Count |> shouldEqual (13 * 9)
        let skipped = faccessatNotReplayed envelope

        [
            for KeyValue ((_, kind, path), expected) in rows do
                if not (skipped.Contains kind) then
                    match directoryArgument kind envelope with
                    | None -> yield $"%s{kind} %s{path}: no such dirfd could be made"
                    | Some (dirfd, system) ->
                        let actual = answer dirfd (pathArgument path envelope.Platform) 0 system

                        if actual <> expected then
                            yield $"%s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

    [<Test>]
    let ``fchmodat answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            replayAttributeChange "fchmodat" probeFChModAt envelope

    [<Test>]
    let ``fchownat answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            replayAttributeChange "fchownat" probeFChOwnAt envelope

    /// The flags each flavour accepts on `fchmodat` and `fchownat` and this
    /// library does not model.
    let private unmodelledAttributeFlags (envelope : Envelope) : int list =
        match SimulatedUnixPlatform.flavour envelope.Platform with
        | SimulatedUnixFlavour.Linux -> []
        | SimulatedUnixFlavour.Darwin -> [ 0x800 ; 0x2000 ; 0x8000 ]

    [<Test>]
    let ``fchmodat screens every flag bit as measured, under every envelope`` () : unit =
        for envelope in envelopes do
            replayFlags "fchmodat" probeFChModAt (unmodelledAttributeFlags envelope) envelope

    [<Test>]
    let ``fchownat screens every flag bit as measured, under every envelope`` () : unit =
        for envelope in envelopes do
            replayFlags "fchownat" probeFChOwnAt (unmodelledAttributeFlags envelope) envelope

    // ------------------------------------------------------------ utimensat

    /// The probe's pathname of each kind as `utimensat` takes it: NULL is the
    /// null pointer, which that call reads as no pathname at all.
    let private nullablePathArgument (kind : string) (platform : SimulatedUnixPlatform) : NullablePathArgument =
        match kind with
        | "NULL" -> NullablePathArgument.Null
        | other -> NullablePathArgument.NotNull (pathArgument other platform)

    /// `utimensat` as the probe called it: both times now.
    let private probeUTimensAt
        (dirfd : int)
        (path : NullablePathArgument)
        (flags : int)
        (system : UnixSystem<int, string>)
        =
        UnixPathResolution.utimensat dirfd path TimesArgument.Null flags system
        |> renderedChange UTimensAtRefusal.describe

    /// The probe's `utimensat` cells this library does not replay, and why.
    let private utimensatNotReplayed (envelope : Envelope) : Set<string * string> =
        match SimulatedUnixPlatform.flavour envelope.Platform with
        // Linux sets a socket's times, and this library holds none for a
        // socket: it refuses (`UTimensAtRefusal.Socket`).
        | SimulatedUnixFlavour.Linux -> Set.singleton ("socket", "NULL")
        | SimulatedUnixFlavour.Darwin -> Set.empty

    [<Test>]
    let ``utimensat answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            let rows = atRows envelope |> Map.filter (fun (c, _, _) _ -> c = "utimensat")
            rows.Count |> shouldEqual (13 * 9)
            let skippedKinds = faccessatNotReplayed envelope
            let skippedCells = utimensatNotReplayed envelope

            [
                for KeyValue ((_, kind, path), expected) in rows do
                    // Linux numbers ENOTSUP and EOPNOTSUPP alike (95); the
                    // probe printed that number's first name, and this
                    // library calls it EOPNOTSUPP.
                    let expected =
                        match SimulatedUnixPlatform.flavour envelope.Platform, expected with
                        | SimulatedUnixFlavour.Linux, "ENOTSUP" -> "EOPNOTSUPP"
                        | _ -> expected

                    if not (skippedKinds.Contains kind) then
                        match directoryArgument kind envelope with
                        | None -> yield $"%s{envelope.Label} %s{kind} %s{path}: no such dirfd could be made"
                        | Some (dirfd, system) ->
                            let actual =
                                probeUTimensAt dirfd (nullablePathArgument path envelope.Platform) 0 system

                            if skippedCells.Contains (kind, path) then
                                if not (actual.StartsWith "refused: ") then
                                    yield
                                        $"%s{envelope.Label} %s{kind} %s{path}: left unreplayed as a refusal, but this library answered %s{actual}"
                            elif actual <> expected then
                                yield
                                    $"%s{envelope.Label} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``utimensat leaves only Linux's socket unreplayed, beside Darwin's /dev/null`` () : unit =
        envelopes
        |> List.map (fun envelope -> envelope.Label, utimensatNotReplayed envelope |> Set.toList)
        |> shouldEqual
            [
                "Linux root", [ "socket", "NULL" ]
                "Linux uid 1000", [ "socket", "NULL" ]
                "Darwin uid 501", []
            ]

    /// The flags each flavour's `utimensat` reads and this library does not
    /// model.
    let private unmodelledTimestampFlags (envelope : Envelope) : int list =
        match SimulatedUnixPlatform.flavour envelope.Platform with
        | SimulatedUnixFlavour.Linux -> []
        | SimulatedUnixFlavour.Darwin -> [ 0x800 ; 0x2000 ; 0x8000 ]

    [<Test>]
    let ``utimensat screens every flag bit as measured, under every envelope`` () : unit =
        for envelope in envelopes do
            replayFlagsWith
                (PathArg.ofText >> NullablePathArgument.NotNull)
                NullablePathArgument.Null
                "utimensat"
                probeUTimensAt
                (unmodelledTimestampFlags envelope)
                envelope

    // ------------------------------------------------------------ mknodat

    /// `S_IFREG | 0644`, the mode the probe's `mknodat` passed.
    let private regularFileMode : int = 0o100644

    let private renderedMknodAt (result : Result<SyscallAnswer * UnixSystem<int, string>, MkNodRefusal>) : string =
        match result with
        | Ok (SyscallAnswer.Completed _, _) -> "ok"
        | Ok (SyscallAnswer.Failed error, _) -> $"%A{error}"
        | Error refusal -> $"refused: %s{MkNodRefusal.describe refusal}"

    [<Test>]
    let ``mknodat answers every dirfd and path the probe tried, under every envelope`` () : unit =
        for envelope in envelopes do
            let rows = atRows envelope |> Map.filter (fun (c, _, _) _ -> c = "mknodat(S_IFREG)")
            rows.Count |> shouldEqual (13 * 9)
            let skipped = faccessatNotReplayed envelope

            [
                for KeyValue ((_, kind, path), expected) in rows do
                    if not (skipped.Contains kind) then
                        match directoryArgument kind envelope with
                        | None -> yield $"%s{envelope.Label} %s{kind} %s{path}: no such dirfd could be made"
                        | Some (dirfd, system) ->
                            let actual =
                                UnixNamespace.mknodat
                                    dirfd
                                    (pathArgument path envelope.Platform)
                                    regularFileMode
                                    0u
                                    system
                                |> renderedMknodAt

                            if actual <> expected then
                                yield
                                    $"%s{envelope.Label} %s{kind} %s{path}: the probe answered %s{expected}, this library %s{actual}"
            ]
            |> shouldEqual []

    [<Test>]
    let ``only Darwin's /dev/null is left unreplayed`` () : unit =
        envelopes
        |> List.map (fun envelope -> envelope.Label, faccessatNotReplayed envelope |> Set.toList)
        |> shouldEqual [ "Linux root", [] ; "Linux uid 1000", [] ; "Darwin uid 501", [ "devnull" ] ]

    // ------------------------------------------------------------ what the start alone decides

    /// What the copy-in and the starting point alone answer for the probe's
    /// cell, or `None` where they let the call go on to its own rules.
    let private startAnswer
        (path : PathArgumentBytes)
        (dirfd : int)
        (system : UnixSystem<int, string>)
        : string option
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        match UnixPathResolution.copyIn path system with
        | Error error -> Some $"%A{error}"
        | Ok path ->

        match UnixPathResolution.walkStart (AtDirectory.decode flavour dirfd) path system with
        | Error error -> Some $"%A{error}"
        | Ok _ -> None

    /// The calls this fixture replays end to end above, which the start-alone
    /// check below leaves to those replays: a call with a rule of its own
    /// about the empty path, as Linux's `readlinkat` has, is held to it there.
    let private replayedEndToEnd : Set<string> =
        set
            [
                "faccessat"
                "symlinkat"
                "fstatat"
                "fstatat(NOFOLLOW)"
                "readlinkat"
                "openat(O_RDONLY)"
                "openat(O_CREAT)"
                "mkdirat"
                "unlinkat"
                "unlinkat(REMOVEDIR)"
                "renameat[old]"
                "renameat[new]"
                "fchmodat"
                "fchownat"
                "utimensat"
                "mknodat(S_IFREG)"
            ]

    /// Every cell of every call whose answer the start alone decides and
    /// which the probe answered otherwise, as (call, pathname).
    let private startMismatches (envelope : Envelope) : Set<string * string> =
        let rows = atRows envelope
        let skipped = faccessatNotReplayed envelope

        // One fixture per dirfd kind, shared by every call and pathname: the
        // start looks at nothing a call could change.
        let fixtures =
            rows
            |> Map.toSeq
            |> Seq.map (fun ((_, kind, _), _) -> kind)
            |> Seq.distinct
            |> Seq.filter (fun kind -> not (skipped.Contains kind))
            |> Seq.map (fun kind ->
                match directoryArgument kind envelope with
                | Some fixture -> kind, fixture
                | None -> failwith $"%s{context}: no %s{kind} dirfd could be made"
            )
            |> Map.ofSeq

        rows
        |> Map.toSeq
        |> Seq.filter (fun ((call, _, _), _) -> not (replayedEndToEnd.Contains call))
        |> Seq.choose (fun ((call, kind, path), expected) ->
            match Map.tryFind kind fixtures with
            | None -> None
            | Some (dirfd, system) ->

            match startAnswer (pathArgument path envelope.Platform) dirfd system with
            | Some answer when answer <> expected -> Some (call, path)
            | Some _ -> None
            | None ->
                // The start let the call go on, so no failure of the start's own
                // can be the answer.
                if expected = "EBADF" || expected = "ENOTSUP" then
                    Some (call, path)
                else
                    None
        )
        |> Set.ofSeq

    let private replayStart (envelope : Envelope) : unit =
        let calls =
            atRows envelope
            |> Map.toSeq
            |> Seq.map (fun ((call, _, _), _) -> call)
            |> Set.ofSeq

        calls.Count |> shouldEqual 19
        startMismatches envelope |> shouldEqual Set.empty

    [<Test>]
    let ``every call's start answers as the probe measured as Linux root`` () : unit = replayStart linuxRoot

    [<Test>]
    let ``every call's start answers as the probe measured as a Linux user`` () : unit = replayStart linuxUser

    [<Test>]
    let ``every call's start answers as the probe measured on Darwin`` () : unit = replayStart darwinUser

    // ------------------------------------------------------------ generated walks

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    let private other : InodeOwner =
        {
            User = UserId.parseOrFail context 2000u
            Group = GroupId.parseOrFail context 2000u
        }

    /// A tree with a little of everything a walk can meet: links that loop,
    /// dangle, climb and name files and directories, and a directory nobody
    /// but its owner, someone else, may search.
    let private walkSeed : Map<DirectoryEntryName, SeedEntry> =
        let file = SeedEntry.File (ImmutableArray<byte>.Empty, perms 0o644, None)

        let link (target : string) =
            SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

        let dir (bits : int) (owner : InodeOwner option) (entries : (string * SeedEntry) list) =
            SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, perms bits, owner)

        Map.ofList
            [
                name "a",
                dir
                    0o755
                    None
                    [
                        "f", file
                        "l1", link "f"
                        "l2", link "../b"
                        "dang", link "nx"
                        "cyc", link "cyc"
                        "sub", dir 0o755 None [ "g", file ; "up", link ".." ]
                        "locked", dir 0o700 (Some other) [ "h", file ]
                    ]
                name "b", dir 0o755 None [ "x", file ; "abs", link "/a/sub" ]
                name "f", file
            ]

    let private walkDirectories : string list =
        [ "/" ; "/a" ; "/b" ; "/a/sub" ; "/a/locked" ]

    /// A process on `platform` as `credentials` in the tree above, with its
    /// current directory at `cwd`, which need not be one it could `chdir` to.
    let private walkSystem
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (cwd : string)
        : UnixSystem<int, string>
        =
        let image : UnixBootImage<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context credentials

        let system =
            match
                UnixBootImage.withFileSystemAndCurrentDirectory
                    epoch
                    (InodeOwner.ofProcess credentials)
                    walkSeed
                    (AbsoluteUnixPath.parseOrFail context "/")
                    image
            with
            | Ok image -> UnixBootImage.boot image
            | Error fault -> failwith $"%s{context}: could not build the walk tree: %A{fault}"

        let inode =
            match
                PathWalk.resolveExisting
                    (SimulatedUnixPlatform.pathLimits platform)
                    Owners.root
                    SymlinkProtection.Off
                    (VirtualFileSystem.root system.Machine.FileSystem)
                    SymlinkPolicy.Follow
                    (UnixPath.parseOrFail context cwd)
                    system.Machine.FileSystem
            with
            | Ok inode -> inode
            | Error failure -> failwith $"%s{context}: %s{cwd} does not resolve: %A{failure}"

        { system with
            Process =
                { system.Process with
                    CurrentDirectoryInode = inode
                }
        }

    let private walkPaths : Gen<UnixPath> =
        let pathComponent =
            Gen.elements
                [
                    "a"
                    "b"
                    "f"
                    "l1"
                    "l2"
                    "dang"
                    "cyc"
                    "sub"
                    "up"
                    "locked"
                    "h"
                    "g"
                    "x"
                    "abs"
                    "."
                    ".."
                    "nx"
                ]

        gen {
            let! count = Gen.choose (0, 4)
            let! components = Gen.listOfLength count pathComponent
            let! rooted = Gen.elements [ false ; false ; true ]
            let! trailing = Gen.elements [ false ; false ; true ]
            let body = String.Join ("/", components)

            let text =
                (if rooted then "/" else "")
                + body
                + (if trailing && body <> "" then "/" else "")

            return UnixPath.parseOrFail context text
        }

    let private walkCase
        : Gen<SimulatedUnixPlatform * Credentials * string * UnixPath * SymlinkPolicy * TrailingSeparatorPolicy> =
        gen {
            let! platform = Gen.elements [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

            let! credentials =
                Gen.elements
                    [
                        Owners.root
                        Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
                    ]

            let! cwd = Gen.elements walkDirectories
            let! path = walkPaths
            let! policy = Gen.elements [ SymlinkPolicy.Follow ; SymlinkPolicy.NoFollowFinal ]

            let! trailing =
                Gen.elements
                    [
                        TrailingSeparatorPolicy.Demand
                        TrailingSeparatorPolicy.RefuseIsDirectory
                        TrailingSeparatorPolicy.Ignore
                    ]

            return platform, credentials, cwd, path, policy, trailing
        }

    [<Test>]
    let ``a walk from AT_FDCWD is the walk from the current directory's inode`` () : unit =
        // The reference is what every plain call did before a walk could start
        // anywhere else: `PathWalk` from the process's current directory inode.
        let property (platform, credentials, cwd, path, policy, trailing) : unit =
            let system = walkSystem platform credentials cwd
            let limits = SimulatedUnixPlatform.pathLimits platform
            let vfs = system.Machine.FileSystem
            let protection = system.Machine.ProtectedFiles.Symlinks
            let start = system.Process.CurrentDirectoryInode

            UnixPathResolution.resolvePathFull AtDirectory.CurrentDirectory policy trailing path system
            |> shouldEqual (PathWalk.resolveFull limits credentials protection start policy trailing path vfs)

            UnixPathResolution.resolvePathParent AtDirectory.CurrentDirectory policy trailing path system
            |> Result.bind PathWalk.completeResolution
            |> shouldEqual (
                PathWalk.resolveParent limits credentials protection start policy trailing path vfs
                |> Result.bind PathWalk.completeResolution
            )

            UnixPathResolution.resolvePath AtDirectory.CurrentDirectory policy path system
            |> shouldEqual (PathWalk.resolveExisting limits credentials protection start policy path vfs)

        Check.One (config, Prop.forAll (Arb.fromGen walkCase) property)

    [<Test>]
    let ``a walk from a descriptor on a directory is the walk from that directory as the current one`` () : unit =
        let property (platform, credentials, cwd, path, policy, trailing) : unit =
            // Opened as root, so that a directory the caller may not read can
            // still be held; the walk itself is the caller's.
            let asRoot = walkSystem platform Owners.root cwd

            let fd, held =
                match Answered.openPath readOnly (UnixPath.parseOrFail context cwd) 0 asRoot with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let atRoot =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = VirtualFileSystem.root held.Machine.FileSystem
                        }
                }

            let inCwd = walkSystem platform credentials cwd

            UnixPathResolution.resolvePathFull (AtDirectory.Descriptor fd) policy trailing path atRoot
            |> shouldEqual (UnixPathResolution.resolvePathFull AtDirectory.CurrentDirectory policy trailing path inCwd)

            UnixPathResolution.resolvePathParent (AtDirectory.Descriptor fd) policy trailing path atRoot
            |> Result.bind PathWalk.completeResolution
            |> shouldEqual (
                UnixPathResolution.resolvePathParent AtDirectory.CurrentDirectory policy trailing path inCwd
                |> Result.bind PathWalk.completeResolution
            )

        Check.One (config, Prop.forAll (Arb.fromGen walkCase) property)

    [<Test>]
    let ``stat is fstatat from AT_FDCWD, and fstatat from a descriptor on a directory is stat from that directory``
        ()
        : unit
        =
        let property (platform, credentials, cwd, path : UnixPath, policy, _ : TrailingSeparatorPolicy) : unit =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)

            let flags =
                match policy with
                | SymlinkPolicy.Follow -> 0
                | SymlinkPolicy.NoFollowFinal -> atSymlinkNoFollow flavour

            let inCwd = walkSystem platform credentials cwd

            let viaStat =
                UnixPathResolution.stat policy argument inCwd
                |> Result.mapError FStatAtRefusal.Stat

            UnixPathResolution.fstatat (atFdCwd flavour) argument flags inCwd
            |> shouldEqual viaStat

            // Opened as root, as the descriptor property above does.
            let fd, held =
                match
                    Answered.openPath
                        readOnly
                        (UnixPath.parseOrFail context cwd)
                        0
                        (walkSystem platform Owners.root cwd)
                with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let atRoot =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = VirtualFileSystem.root held.Machine.FileSystem
                        }
                }

            // The tree was built by root, so its unowned entries are root's:
            // compare with stat from the same tree, by the same caller.
            let heldInCwd =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                        }
                }

            UnixPathResolution.fstatat fd argument flags atRoot
            |> shouldEqual (
                UnixPathResolution.stat policy argument heldInCwd
                |> Result.mapError FStatAtRefusal.Stat
            )

        Check.One (config, Prop.forAll (Arb.fromGen walkCase) property)

    [<Test>]
    let ``readlink is readlinkat from AT_FDCWD, and readlinkat from a descriptor on a directory is readlink from that directory``
        ()
        : unit
        =
        let property
            (platform, credentials, cwd, path : UnixPath, _ : SymlinkPolicy, _ : TrailingSeparatorPolicy)
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)
            let inCwd = walkSystem platform credentials cwd

            UnixNamespace.readlinkat (atFdCwd flavour) argument UserBuffer.Mapped 64 inCwd
            |> shouldEqual (UnixNamespace.readlink argument UserBuffer.Mapped 64 inCwd)

            // Opened as root, as the descriptor properties above do, and
            // compared with readlink from the same tree, whose unowned
            // entries are root's.
            let fd, held =
                match
                    Answered.openPath
                        readOnly
                        (UnixPath.parseOrFail context cwd)
                        0
                        (walkSystem platform Owners.root cwd)
                with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let atRoot =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = VirtualFileSystem.root held.Machine.FileSystem
                        }
                }

            let heldInCwd =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                        }
                }

            UnixNamespace.readlinkat fd argument UserBuffer.Mapped 64 atRoot
            |> shouldEqual (UnixNamespace.readlink argument UserBuffer.Mapped 64 heldInCwd)

        Check.One (config, Prop.forAll (Arb.fromGen walkCase) property)

    [<Test>]
    let ``open is openat from AT_FDCWD, and openat from a descriptor on a directory is open from that directory``
        ()
        : unit
        =
        let openCase =
            gen {
                let! case = walkCase
                let! shape = Gen.elements [ "rdonly" ; "wronly" ; "creat" ; "creat-excl" ; "creat-trunc" ; "directory" ]
                return case, shape
            }

        let property
            (
                (platform, credentials, cwd, path : UnixPath, _ : SymlinkPolicy, _ : TrailingSeparatorPolicy),
                shape : string
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform

            let flags =
                match shape with
                | "rdonly" -> 0
                | "wronly" -> 1
                | "creat" -> openCreate flavour
                | "creat-excl" -> openCreate flavour ||| openExclusive flavour
                | "creat-trunc" -> 1 ||| openCreate flavour ||| openTruncate flavour
                // O_DIRECTORY: x86-64 Linux's and Darwin's.
                | "directory" ->
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> 0x10000
                    | SimulatedUnixFlavour.Darwin -> 0x100000
                | other -> failwith $"%s{context}: no open shape %s{other}"

            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)
            let inCwd = walkSystem platform credentials cwd

            UnixNamespace.openat (atFdCwd flavour) argument flags 0o644 inCwd
            |> shouldEqual (UnixNamespace.openPath flags argument 0o644 inCwd)

            // Opened as root, as the descriptor properties above do, and
            // compared with open from the same tree, whose unowned entries are
            // root's, by the same caller. Both hold the directory's descriptor,
            // so both hand out the same next one.
            let fd, held =
                match
                    Answered.openPath
                        readOnly
                        (UnixPath.parseOrFail context cwd)
                        0
                        (walkSystem platform Owners.root cwd)
                with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let withCaller (cwdInode : InodeNumber) =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = cwdInode
                        }
                }

            let atRoot = withCaller (VirtualFileSystem.root held.Machine.FileSystem)
            let heldInCwd = withCaller held.Process.CurrentDirectoryInode

            let outcome (result : Result<SyscallAnswer * UnixSystem<int, string>, OpenRefusal>) =
                match result with
                | Ok (answer, system) ->
                    Ok (answer, system.Machine.FileSystem, (UnixSystemState.fileDescriptors system))
                | Error refusal -> Error refusal

            UnixNamespace.openat fd argument flags 0o644 atRoot
            |> outcome
            |> shouldEqual (UnixNamespace.openPath flags argument 0o644 heldInCwd |> outcome)

        Check.One (config, Prop.forAll (Arb.fromGen openCase) property)

    [<Test>]
    let ``mkdir is mkdirat from AT_FDCWD, and mkdirat from a descriptor on a directory is mkdir from that directory``
        ()
        : unit
        =
        let mkdirCase =
            gen {
                let! case = walkCase
                let! mode = Gen.elements [ 0o777 ; 0o755 ; 0o700 ; 0 ; 0o1777 ; 0o2777 ; 0o7777 ; 0xffff ]
                return case, mode
            }

        let property
            ((platform, credentials, cwd, path : UnixPath, _ : SymlinkPolicy, _ : TrailingSeparatorPolicy), mode : int)
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)
            let inCwd = walkSystem platform credentials cwd

            UnixNamespace.mkdirat (atFdCwd flavour) argument mode inCwd
            |> shouldEqual (UnixNamespace.mkdir argument mode inCwd)

            // Opened as root, as the descriptor properties above do, and
            // compared with mkdir from the same tree, whose unowned entries are
            // root's, by the same caller.
            let fd, held =
                match
                    Answered.openPath
                        readOnly
                        (UnixPath.parseOrFail context cwd)
                        0
                        (walkSystem platform Owners.root cwd)
                with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let withCaller (cwdInode : InodeNumber) =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = cwdInode
                        }
                }

            let atRoot = withCaller (VirtualFileSystem.root held.Machine.FileSystem)
            let heldInCwd = withCaller held.Process.CurrentDirectoryInode

            let outcome (result : Result<SyscallAnswer * UnixSystem<int, string>, PathRefusal>) =
                match result with
                | Ok (answer, system) -> Ok (answer, system.Machine.FileSystem)
                | Error refusal -> Error refusal

            UnixNamespace.mkdirat fd argument mode atRoot
            |> outcome
            |> shouldEqual (UnixNamespace.mkdir argument mode heldInCwd |> outcome)

        Check.One (config, Prop.forAll (Arb.fromGen mkdirCase) property)

    [<Test>]
    let ``mknod is mknodat from AT_FDCWD, and mknodat from a descriptor on a directory is mknod from that directory``
        ()
        : unit
        =
        let mknodCase =
            gen {
                let! case = walkCase
                // Every value of the type field, each with permission bits.
                let! kind = Gen.choose (0, 15)
                let! bits = Gen.elements [ 0o644 ; 0 ; 0o777 ; 0o7777 ; 0o2755 ]
                let! dev = Gen.elements [ 0u ; 0x103u ]
                return case, (kind <<< 12) ||| bits, dev
            }

        let property
            (
                (platform, credentials, cwd, path : UnixPath, _ : SymlinkPolicy, _ : TrailingSeparatorPolicy),
                mode : int,
                dev : uint32
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)
            let inCwd = walkSystem platform credentials cwd

            UnixNamespace.mknodat (atFdCwd flavour) argument mode dev inCwd
            |> shouldEqual (UnixNamespace.mknod argument mode dev inCwd)

            // Opened as root, as the descriptor properties above do, and
            // compared with mknod from the same tree, whose unowned entries are
            // root's, by the same caller.
            let fd, held =
                match
                    Answered.openPath
                        readOnly
                        (UnixPath.parseOrFail context cwd)
                        0
                        (walkSystem platform Owners.root cwd)
                with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let withCaller (cwdInode : InodeNumber) =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = cwdInode
                        }
                }

            let atRoot = withCaller (VirtualFileSystem.root held.Machine.FileSystem)
            let heldInCwd = withCaller held.Process.CurrentDirectoryInode

            let outcome (result : Result<SyscallAnswer * UnixSystem<int, string>, MkNodRefusal>) =
                match result with
                | Ok (answer, system) -> Ok (answer, system.Machine.FileSystem)
                | Error refusal -> Error refusal

            UnixNamespace.mknodat fd argument mode dev atRoot
            |> outcome
            |> shouldEqual (UnixNamespace.mknod argument mode dev heldInCwd |> outcome)

        Check.One (config, Prop.forAll (Arb.fromGen mknodCase) property)

    [<Test>]
    let ``unlink and rmdir are unlinkat from AT_FDCWD, and unlinkat from a descriptor on a directory is unlink or rmdir from that directory``
        ()
        : unit
        =
        let removalCase =
            gen {
                let! case = walkCase
                let! removeDir = Gen.elements [ false ; true ]
                return case, removeDir
            }

        let property
            (
                (platform, credentials, cwd, path : UnixPath, _ : SymlinkPolicy, _ : TrailingSeparatorPolicy),
                removeDir : bool
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)
            let flags = if removeDir then atRemoveDir flavour else 0

            let plain (system : UnixSystem<int, string>) =
                if removeDir then
                    UnixNamespace.rmdir argument system
                else
                    UnixNamespace.unlink argument system
                |> Result.mapError UnlinkAtRefusal.Removal

            let inCwd = walkSystem platform credentials cwd

            UnixNamespace.unlinkat (atFdCwd flavour) argument flags inCwd
            |> shouldEqual (plain inCwd)

            // Opened as root, as the descriptor properties above do, and
            // compared with unlink or rmdir from the same tree, whose unowned
            // entries are root's, by the same caller. Both hold the directory's
            // descriptor, so a removal of that very directory leaves the same
            // orphan behind.
            let fd, held =
                match
                    Answered.openPath
                        readOnly
                        (UnixPath.parseOrFail context cwd)
                        0
                        (walkSystem platform Owners.root cwd)
                with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

            let withCaller (cwdInode : InodeNumber) =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = cwdInode
                        }
                }

            let atRoot = withCaller (VirtualFileSystem.root held.Machine.FileSystem)
            let heldInCwd = withCaller held.Process.CurrentDirectoryInode

            let outcome (result : Result<SyscallAnswer * UnixSystem<int, string>, UnlinkAtRefusal>) =
                match result with
                | Ok (answer, system) -> Ok (answer, system.Machine.FileSystem)
                | Error refusal -> Error refusal

            UnixNamespace.unlinkat fd argument flags atRoot
            |> outcome
            |> shouldEqual (plain heldInCwd |> outcome)

        Check.One (config, Prop.forAll (Arb.fromGen removalCase) property)

    /// `path` written from `directory` rather than from where a walk of it
    /// starts: unchanged when it is rooted, or empty, which no prefix keeps.
    let private writtenFrom (directory : string) (path : UnixPath) : UnixPath =
        if UnixPath.isRooted path || UnixPath.isEmpty path then
            path
        else
            match UnixPath.tryToString path with
            | Some text -> UnixPath.parseOrFail context (directory.TrimEnd '/' + "/" + text)
            | None -> failwith $"%s{context}: a generated path is not text: %s{UnixPath.toEscaped path}"

    [<Test>]
    let ``rename is renameat from AT_FDCWD, and renameat from descriptors on directories is rename with each path written from them``
        ()
        : unit
        =
        let renameCase =
            gen {
                let! case = walkCase
                let! newPath = walkPaths
                let! newDirectory = Gen.elements walkDirectories
                return case, newPath, newDirectory
            }

        let property
            (
                (platform, credentials, cwd, path : UnixPath, _ : SymlinkPolicy, _ : TrailingSeparatorPolicy),
                newPath : UnixPath,
                newDirectory : string
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform

            let argument (path : UnixPath) =
                PathArgumentBytes.Bytes (UnixPath.toByteString path)

            let inCwd = walkSystem platform credentials cwd

            UnixNamespace.renameat (atFdCwd flavour) (argument path) (atFdCwd flavour) (argument newPath) inCwd
            |> shouldEqual (UnixNamespace.rename (argument path) (argument newPath) inCwd)

            // Opened as root, as the descriptor properties above do: one
            // descriptor on the current directory, and one on `newDirectory`.
            let asRoot = walkSystem platform Owners.root cwd

            let openedAsRoot (directory : string) (system : UnixSystem<int, string>) =
                match Answered.openPath readOnly (UnixPath.parseOrFail context directory) 0 system with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{directory}) did not open: %O{other}"

            let oldFd, held = openedAsRoot cwd asRoot
            let newFd, held = openedAsRoot newDirectory held

            let withCaller (cwdInode : InodeNumber) =
                { held with
                    Process =
                        { held.Process with
                            Credentials = credentials
                            CurrentDirectoryInode = cwdInode
                        }
                }

            let atRoot = withCaller (VirtualFileSystem.root held.Machine.FileSystem)
            let heldInCwd = withCaller held.Process.CurrentDirectoryInode

            let outcome (result : Result<SyscallAnswer * UnixSystem<int, string>, RenameRefusal>) =
                match result with
                | Ok (answer, system) -> Ok (answer, system.Machine.FileSystem)
                | Error refusal -> Error refusal

            // The same descriptor on both sides is rename from its directory
            // as the current one.
            UnixNamespace.renameat oldFd (argument path) oldFd (argument newPath) atRoot
            |> outcome
            |> shouldEqual (UnixNamespace.rename (argument path) (argument newPath) heldInCwd |> outcome)

            // Two descriptors are rename with each path written from its own
            // descriptor's directory, in the very same system.
            UnixNamespace.renameat oldFd (argument path) newFd (argument newPath) atRoot
            |> shouldEqual (
                UnixNamespace.rename
                    (argument (writtenFrom cwd path))
                    (argument (writtenFrom newDirectory newPath))
                    atRoot
            )

        Check.One (config, Prop.forAll (Arb.fromGen renameCase) property)

    /// What an attribute change left behind that a caller can observe: the
    /// answer and the filesystem, or the refusal.
    let private changed
        (result : Result<SyscallAnswer * UnixSystem<int, string>, 'Refusal>)
        : Result<SyscallAnswer * VirtualFileSystem, 'Refusal>
        =
        match result with
        | Ok (answer, system) -> Ok (answer, system.Machine.FileSystem)
        | Error refusal -> Error refusal

    /// `walkSystem`'s tree, built by root with the process holding a
    /// descriptor on `cwd`, for `credentials` with its current directory at
    /// the root and at `cwd`, and the descriptor.
    let private heldDirectory
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (cwd : string)
        : int * UnixSystem<int, string> * UnixSystem<int, string>
        =
        // Opened as root, as the descriptor properties above do, and compared
        // with the call from the same tree, whose unowned entries are root's,
        // by the same caller.
        let fd, held =
            match
                Answered.openPath readOnly (UnixPath.parseOrFail context cwd) 0 (walkSystem platform Owners.root cwd)
            with
            | SyscallAnswer.Completed fd, system -> int fd, system
            | other -> failwith $"%s{context}: open(%s{cwd}) did not open: %O{other}"

        let withCaller (cwdInode : InodeNumber) =
            { held with
                Process =
                    { held.Process with
                        Credentials = credentials
                        CurrentDirectoryInode = cwdInode
                    }
            }

        fd, withCaller (VirtualFileSystem.root held.Machine.FileSystem), withCaller held.Process.CurrentDirectoryInode

    [<Test>]
    let ``chmod is fchmodat from AT_FDCWD, and fchmodat from a descriptor on a directory is fchmodat from that directory``
        ()
        : unit
        =
        let modeCase =
            gen {
                let! case = walkCase
                let! mode = Gen.elements [ 0 ; 0o640 ; 0o2755 ; 0o1777 ; 0o7777 ; 0o170644 ]
                return case, mode
            }

        let property
            ((platform, credentials, cwd, path : UnixPath, policy, _ : TrailingSeparatorPolicy), mode : int)
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)

            let flags =
                match policy with
                | SymlinkPolicy.Follow -> 0
                | SymlinkPolicy.NoFollowFinal -> atSymlinkNoFollow flavour

            let inCwd = walkSystem platform credentials cwd

            UnixPathResolution.fchmodat (atFdCwd flavour) argument mode 0 inCwd
            |> changed
            |> shouldEqual (
                UnixPathResolution.chmod argument mode inCwd
                |> Result.mapError FChModAtRefusal.ChMod
                |> changed
            )

            let fd, atRoot, heldInCwd = heldDirectory platform credentials cwd

            UnixPathResolution.fchmodat fd argument mode flags atRoot
            |> changed
            |> shouldEqual (
                UnixPathResolution.fchmodat (atFdCwd flavour) argument mode flags heldInCwd
                |> changed
            )

        Check.One (config, Prop.forAll (Arb.fromGen modeCase) property)

    [<Test>]
    let ``chown and lchown are fchownat from AT_FDCWD, and fchownat from a descriptor on a directory is fchownat from that directory``
        ()
        : unit
        =
        let ownerCase =
            gen {
                let! case = walkCase

                let! ids =
                    Gen.elements
                        [
                            None, None
                            Some (UserId.parseOrFail context 1000u), None
                            None, Some (GroupId.parseOrFail context 1000u)
                            Some (UserId.parseOrFail context 2000u), Some (GroupId.parseOrFail context 2000u)
                        ]

                return case, ids
            }

        let property
            (
                (platform, credentials, cwd, path : UnixPath, policy, _ : TrailingSeparatorPolicy),
                (user : UserId option, group : GroupId option)
            )
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform
            let argument = PathArgumentBytes.Bytes (UnixPath.toByteString path)
            let inCwd = walkSystem platform credentials cwd

            let flags, plain =
                match policy with
                | SymlinkPolicy.Follow -> 0, UnixPathResolution.chown argument user group inCwd
                | SymlinkPolicy.NoFollowFinal ->
                    atSymlinkNoFollow flavour, UnixPathResolution.lchown argument user group inCwd

            UnixPathResolution.fchownat (atFdCwd flavour) argument user group flags inCwd
            |> changed
            |> shouldEqual (plain |> Result.mapError FChOwnAtRefusal.ChOwn |> changed)

            let fd, atRoot, heldInCwd = heldDirectory platform credentials cwd

            UnixPathResolution.fchownat fd argument user group flags atRoot
            |> changed
            |> shouldEqual (
                UnixPathResolution.fchownat (atFdCwd flavour) argument user group flags heldInCwd
                |> changed
            )

        Check.One (config, Prop.forAll (Arb.fromGen ownerCase) property)

    /// A `utimensat` times argument in `flavour`'s numbering: the null
    /// pointer, an unreadable one, or two `struct timespec`s each of which is
    /// `UTIME_NOW`, `UTIME_OMIT`, a time, or a nanosecond field Linux refuses.
    let private timesArgument (flavour : SimulatedUnixFlavour) : Gen<TimesArgument> =
        let one =
            Gen.elements
                [
                    {
                        Seconds = 0L
                        Nanoseconds = TimestampChangeRules.utimeNow flavour
                    }
                    {
                        Seconds = 0L
                        Nanoseconds = TimestampChangeRules.utimeOmit flavour
                    }
                    {
                        Seconds = 1500000000L
                        Nanoseconds = 333333333L
                    }
                    {
                        Seconds = -1L
                        Nanoseconds = 999999999L
                    }
                    {
                        Seconds = 1500000000L
                        Nanoseconds = 1000000000L
                    }
                    {
                        Seconds = 1500000000L
                        Nanoseconds = -3L
                    }
                ]

        Gen.frequency
            [
                1, Gen.constant TimesArgument.Null
                1, Gen.constant TimesArgument.Unreadable
                6, Gen.map2 (fun access modification -> TimesArgument.Fields (access, modification)) one one
            ]

    [<Test>]
    let ``utimensat from a descriptor on a directory is utimensat from that directory`` () : unit =
        let timesCase =
            gen {
                let! (platform, _, _, _, _, _) as case = walkCase
                let! times = timesArgument (SimulatedUnixPlatform.flavour platform)
                return case, times
            }

        let property
            ((platform, credentials, cwd, path : UnixPath, policy, _ : TrailingSeparatorPolicy), times : TimesArgument)
            : unit
            =
            let flavour = SimulatedUnixPlatform.flavour platform

            let argument =
                NullablePathArgument.NotNull (PathArgumentBytes.Bytes (UnixPath.toByteString path))

            let flags =
                match policy with
                | SymlinkPolicy.Follow -> 0
                | SymlinkPolicy.NoFollowFinal -> atSymlinkNoFollow flavour

            let fd, atRoot, heldInCwd = heldDirectory platform credentials cwd

            UnixPathResolution.utimensat fd argument times flags atRoot
            |> changed
            |> shouldEqual (
                UnixPathResolution.utimensat (atFdCwd flavour) argument times flags heldInCwd
                |> changed
            )

        Check.One (config, Prop.forAll (Arb.fromGen timesCase) property)

    /// Something `walkSystem`'s tree lets a process hold a descriptor on, and
    /// whether it is the caller's.
    let private heldObjects : string list =
        [
            "dir:/"
            "dir:/a"
            "dir:/a/locked"
            "file:/f"
            "file:/a/sub/g"
            "file:/a/locked/h"
            "pipe"
            "epoll"
            "closed"
        ]

    /// `walkSystem`'s tree, with the process as `credentials` holding a
    /// descriptor on `held` that root opened or made for it.
    let private holding
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (held : string)
        : int * UnixSystem<int, string>
        =
        let asRoot = walkSystem platform Owners.root "/"

        let fd, system =
            if held = "closed" then
                999, asRoot
            elif held = "pipe" then
                match UnixPipe.pipe2 0 UserBuffer.Mapped asRoot with
                | Ok (Pipe2Answer.Created (readFd, _), system) -> readFd, system
                | other -> failwith $"%s{context}: pipe2 did not make a pipe: %A{other}"
            elif held = "epoll" then
                match UnixPoll.epollCreate1 0 asRoot with
                | Ok (Ok created) -> created
                | other -> failwith $"%s{context}: epoll_create1 did not make an instance: %A{other}"
            else
                let path = held.Substring (held.IndexOf ':' + 1)

                match Answered.openPath readOnly (UnixPath.parseOrFail context path) 0 asRoot with
                | SyscallAnswer.Completed fd, system -> int fd, system
                | other -> failwith $"%s{context}: open(%s{path}) did not open: %O{other}"

        fd,
        { system with
            Process =
                { system.Process with
                    Credentials = credentials
                }
        }

    [<Test>]
    let ``Linux's utimensat of a null pathname is AT_EMPTY_PATH's of the empty one, from any descriptor`` () : unit =
        let platform = SimulatedUnixPlatform.linuxX64

        let heldCase =
            gen {
                let! credentials =
                    Gen.elements
                        [
                            Owners.root
                            Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
                        ]

                let! held = Gen.elements heldObjects
                let! times = timesArgument SimulatedUnixFlavour.Linux
                return credentials, held, times
            }

        let property (credentials : Credentials, held : string, times : TimesArgument) : unit =
            let fd, system = holding platform credentials held
            let empty = NullablePathArgument.NotNull (PathArg.ofText "")

            // The whole system, since a pipe's times are not the filesystem's.
            UnixPathResolution.utimensat fd NullablePathArgument.Null times 0 system
            |> shouldEqual (UnixPathResolution.utimensat fd empty times linuxAtEmptyPath system)

        Check.One (config, Prop.forAll (Arb.fromGen heldCase) property)

    [<Test>]
    let ``Linux's utimensat with both times omitted answers 0 and changes nothing, whatever else it is given``
        ()
        : unit
        =
        let omitCase =
            gen {
                let! (_, credentials, cwd, path, _, _) = walkCase
                let! held = Gen.elements ("cwd" :: heldObjects)
                let! nullPath = Gen.elements [ false ; true ]
                let! flags = Gen.elements [ 0 ; 0x100 ; 0x1000 ; 0x1 ; 0x40000000 ]
                let! seconds = Gen.elements [ 0L ; 1500000000L ; -1L ]
                return credentials, cwd, held, (if nullPath then None else Some path), flags, seconds
            }

        let property
            (
                credentials : Credentials,
                cwd : string,
                held : string,
                path : UnixPath option,
                flags : int,
                seconds : int64
            )
            : unit
            =
            let platform = SimulatedUnixPlatform.linuxX64

            let dirfd, system =
                if held = "cwd" then
                    atFdCwd SimulatedUnixFlavour.Linux, walkSystem platform credentials cwd
                else
                    holding platform credentials held

            let omit =
                {
                    Seconds = seconds
                    Nanoseconds = TimestampChangeRules.utimeOmit SimulatedUnixFlavour.Linux
                }

            let argument =
                match path with
                | None -> NullablePathArgument.Null
                | Some path -> NullablePathArgument.NotNull (PathArgumentBytes.Bytes (UnixPath.toByteString path))

            UnixPathResolution.utimensat dirfd argument (TimesArgument.Fields (omit, omit)) flags system
            |> shouldEqual (Ok (SyscallAnswer.Completed 0L, system))

        Check.One (config, Prop.forAll (Arb.fromGen omitCase) property)
