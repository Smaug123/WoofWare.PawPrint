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

    /// The cells where a call answers before, or instead of, the starting
    /// point the other calls share, as (call, pathname) for every `dirfd`:
    /// Linux's `readlinkat` looks an empty path's `dirfd` up, Linux's
    /// `utimensat` acts on `dirfd` itself for a NULL path, and Darwin's
    /// `mknodat` refuses an unprivileged caller before it copies the path in.
    /// (That EPERM shows for every pathname but the absolute one, where the
    /// start fails nothing and EPERM is not an answer only a start can give.)
    let private ownRules (envelope : Envelope) : Set<string * string> =
        match SimulatedUnixPlatform.flavour envelope.Platform with
        | SimulatedUnixFlavour.Linux -> set [ "readlinkat", "empty" ; "utimensat", "NULL" ]
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
