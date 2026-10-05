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
/// - the probe's `faccessat` rows, replayed end to end under each envelope the
///   probe ran in;
/// - every other call's rows that the starting point alone decides: wherever
///   the copy-in or `UnixPathResolution.walkStart` fails, the probe answered
///   exactly that, for all nineteen calls, but for three measured exceptions;
/// - two properties over generated paths: a walk from `AT_FDCWD` is the walk
///   from the current directory's inode that every plain call made before
///   `*at` existed, and a walk from a descriptor on a directory is the walk
///   from that directory as the current one.
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

    let private replayFStatAtFlags (envelope : Envelope) : unit =
        let flavour = SimulatedUnixPlatform.flavour envelope.Platform
        let atFdCwd = atFdCwd flavour

        let row =
            probeLines envelope.Resource "FLAGS"
            |> List.find (fun row -> row.[0] = "fstatat")

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

                    let actual =
                        UnixPathResolution.fstatat atFdCwd (PathArg.ofText "f") flag (boot envelope)
                        |> renderedStatus

                    if actual.StartsWith "refused: the flag word" then
                        yield flag
                    elif actual <> expected then
                        failwith
                            $"%s{context}: fstatat with flags 0x%x{flag}: the probe answered %s{expected}, this library %s{actual}"
            ]

        // Every flag refused is one the probe saw accepted, or answered other
        // than EINVAL.
        let expectedRefused =
            match flavour with
            | SimulatedUnixFlavour.Linux -> [ 0x800 ; 0x2000 ; 0x4000 ]
            | SimulatedUnixFlavour.Darwin -> [ 0x200 ; 0x400 ; 0x800 ; 0x2000 ; 0x8000 ]

        refused |> shouldEqual expectedRefused

        for flag in refused do
            Map.tryFind flag rejected |> Option.defaultValue "ok" |> shouldNotEqual "EINVAL"

        let order =
            probeLines envelope.Resource "FLAGORDER"
            |> List.find (fun row -> row.[0] = "fstatat")

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
                    UnixPathResolution.fstatat dirfd path 0x40000000 (boot envelope)
                    |> renderedStatus

                if actual <> expected then
                    yield $"FLAGORDER %s{label}: the probe answered %s{expected}, this library %s{actual}"
        ]
        |> shouldEqual []

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
            ]

    /// The cells where a call not replayed end to end answers before, or
    /// instead of, the starting point the other calls share, as (call,
    /// pathname) for every `dirfd`: Linux's `utimensat` acts on `dirfd` itself
    /// for a NULL path, and Darwin's `mknodat` refuses an unprivileged caller
    /// before it copies the path in. (That EPERM shows for every pathname but
    /// the absolute one, where the start fails nothing and EPERM is not an
    /// answer only a start can give.)
    let private ownRules (envelope : Envelope) : Set<string * string> =
        match SimulatedUnixPlatform.flavour envelope.Platform with
        | SimulatedUnixFlavour.Linux -> set [ "utimensat", "NULL" ]
        | SimulatedUnixFlavour.Darwin ->
            set
                [
                    for path in [ "f" ; "nx" ; "empty" ; "NULL" ; "PROT_NONE" ; "overlong" ; "dot" ; "dotdot" ] do
                        "mknodat(S_IFREG)", path
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
        startMismatches envelope |> shouldEqual (ownRules envelope)

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
                | Ok (answer, system) -> Ok (answer, system.Machine.FileSystem, system.Process.FileDescriptors)
                | Error refusal -> Error refusal

            UnixNamespace.openat fd argument flags 0o644 atRoot
            |> outcome
            |> shouldEqual (UnixNamespace.openPath flags argument 0o644 heldInCwd |> outcome)

        Check.One (config, Prop.forAll (Arb.fromGen openCase) property)
