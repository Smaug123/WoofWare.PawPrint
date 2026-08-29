namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The path walk was one indivisible function until `resolveParent` and
/// `completeResolution` split it in two, so that `rename` — the first syscall
/// resolving two paths — can interleave them the way Linux's `do_renameat2`
/// does.
///
/// That split was meant to change no behaviour at all, and nothing inside the
/// library can check the claim: `resolveFull` is now *defined* as the
/// composition, so a property relating the three is true by construction and
/// sees nothing. The oracle therefore has to come from outside, and it is the
/// pre-split walk, transcribed below from the commit the split landed on and
/// rewritten against public API alone.
///
/// Do not "improve" `referenceResolveFull`. Its value is entirely that it is
/// the old code; a tidy-up is a change to the oracle, which is the one thing
/// that must not track the implementation.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestResolutionSplit =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    // ------------------------------------------------------- the old walk

    /// `VirtualFileSystem`'s own `tryGetDirectory`, which is private to the
    /// library.
    let private tryGetDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : DirectoryContent option =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory content) -> Some content
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _)
        | None -> None

    /// `VirtualFileSystem.resolveFull` as it stood before the split: one walk,
    /// doing the parent traversal and the final lookup in a single call.
    let private referenceResolveFull
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<Resolution, UnixError>
        =
        let limits = PathLimits.assertValid "referenceResolveFull" limits

        if UnixPath.isEmpty path then
            Error UnixError.ENOENT
        else

        let start =
            if UnixPath.isRooted path then
                Ok (VirtualFileSystem.root vfs)
            else

            match VirtualFileSystem.tryGetContent startDirectory vfs with
            | None -> Error UnixError.ENOENT
            | Some (InodeContent.Directory _) -> Ok startDirectory
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR

        match start with
        | Error error -> Error error
        | Ok start ->

        let rec walk
            (directory : InodeNumber)
            (remaining : PathCursor)
            (trailing : bool)
            (finalSymlinkFollowed : bool)
            (lastNavigation : FinalNavigation)
            (symlinks : int)
            : Result<Resolution, UnixError>
            =
            match PathCursor.next remaining with
            | None ->
                Ok
                    {
                        Target = ResolvedTarget.Directory (directory, lastNavigation)
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    }
            | Some (nextComponent, rest) ->

            let searchBit = 0o100

            let directoryContent =
                match tryGetDirectory directory vfs with
                | Some content -> content
                | None -> failwith $"reference walk: inode %O{directory} is no longer a directory"

            let maySearch =
                match privilege with
                | CallerPrivilege.Privileged -> true
                | CallerPrivilege.Unprivileged ->
                    PermissionBits.toInt directoryContent.Permissions &&& searchBit = searchBit

            if not maySearch then
                Error UnixError.EACCES
            else

            match nextComponent with
            | PathComponent.Current ->
                walk directory rest trailing finalSymlinkFollowed FinalNavigation.Current symlinks
            | PathComponent.Parent ->
                walk directoryContent.Parent rest trailing finalSymlinkFollowed FinalNavigation.Parent symlinks
            | PathComponent.Name name ->

            let isFinal = PathCursor.isExhausted rest

            match trailingSeparatorPolicy with
            | TrailingSeparatorPolicy.RefuseIsDirectory when isFinal && trailing -> Error UnixError.EISDIR
            | TrailingSeparatorPolicy.RefuseIsDirectory
            | TrailingSeparatorPolicy.Demand
            | TrailingSeparatorPolicy.Ignore ->

            if not (PathLimits.nameWithinLimit limits name) then
                Error UnixError.ENAMETOOLONG
            else

            let entries = directoryContent.Entries

            let finish (target : ResolvedTarget) : Result<Resolution, UnixError> =
                Ok
                    {
                        Target = target
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    }

            match Map.tryFind name entries with
            | None ->
                if isFinal then
                    finish (ResolvedTarget.Entry (directory, name, None))
                else
                    Error UnixError.ENOENT
            | Some target ->

            let content =
                match VirtualFileSystem.tryGetContent target vfs with
                | Some content -> content
                | None -> failwith $"reference walk: inode %O{target} is not in the graph"

            let trailingActsOnFinal =
                match trailingSeparatorPolicy with
                | TrailingSeparatorPolicy.Demand
                | TrailingSeparatorPolicy.RefuseIsDirectory -> trailing
                | TrailingSeparatorPolicy.Ignore -> false

            let followFinal = policy = SymlinkPolicy.Follow || trailingActsOnFinal

            match content with
            | InodeContent.Symlink linkTarget when not isFinal || followFinal ->
                if symlinks + 1 > PathLimits.maxSymlinkTraversals limits then
                    Error UnixError.ELOOP
                else if not (PathLimits.spliceWithinLimit limits linkTarget rest) then
                    Error UnixError.ENAMETOOLONG
                else

                let linkPath = SymlinkTarget.toUnixPath linkTarget

                let next =
                    if UnixPath.isRooted linkPath then
                        VirtualFileSystem.root vfs
                    else
                        directory

                let trailing =
                    if isFinal then
                        trailing || UnixPath.hasTrailingSeparator linkPath
                    else
                        trailing

                let spliced = PathCursor.splice linkPath rest

                let lastNavigation =
                    if PathCursor.isExhausted spliced then
                        FinalNavigation.Root
                    else
                        lastNavigation

                walk next spliced trailing (finalSymlinkFollowed || isFinal) lastNavigation (symlinks + 1)
            | InodeContent.Symlink _ -> finish (ResolvedTarget.Entry (directory, name, Some target))
            | InodeContent.Directory _ ->
                if isFinal then
                    finish (ResolvedTarget.Entry (directory, name, Some target))
                else
                    walk target rest trailing finalSymlinkFollowed lastNavigation symlinks
            | InodeContent.RegularFile _ ->
                if isFinal then
                    if trailingActsOnFinal then
                        Error UnixError.ENOTDIR
                    else
                        finish (ResolvedTarget.Entry (directory, name, Some target))
                else
                    Error UnixError.ENOTDIR

        walk start (PathCursor.ofPath path) (UnixPath.hasTrailingSeparator path) false FinalNavigation.Root 0

    // -------------------------------------------------- the parameter space

    /// Three limit sets, not one. The two real ones because they are what the
    /// kernel resolves under, and a deliberately tiny one because it is the
    /// only way a small corpus reaches ENAMETOOLONG and the splice recheck at
    /// all: under a real NAME_MAX every generated name is legal, so the length
    /// arms sit unvisited and the reference proves nothing about them.
    ///
    /// The tiny one is not a kernel anyone ships. It does not have to be: this
    /// fixture compares two implementations of one walk, and `PathLimits.create`
    /// accepts it, so it is a legal input the two must agree on.
    let private tightLimits : PathLimits =
        PathLimits.create 2 256 (NameLengthLimit.Utf8Bytes 2) SpliceLengthRecheck.Recheck

    let private allLimits : (string * PathLimits) list =
        [
            "linux", SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64
            "darwin", SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64
            "tight", tightLimits
        ]

    let private allPolicies : SymlinkPolicy list =
        [ SymlinkPolicy.Follow ; SymlinkPolicy.NoFollowFinal ]

    let private allTrailingPolicies : TrailingSeparatorPolicy list =
        [
            TrailingSeparatorPolicy.Demand
            TrailingSeparatorPolicy.Ignore
            TrailingSeparatorPolicy.RefuseIsDirectory
        ]

    let private allPrivileges : CallerPrivilege list =
        [ CallerPrivilege.Unprivileged ; CallerPrivilege.Privileged ]

    /// A name that is over-long in bytes but not in UTF-16 code units, which is
    /// the one input that tells `NameLengthLimit.Utf8Bytes` from
    /// `Utf16CodeUnits`. An ASCII-only corpus reads the two as the same rule.
    let private wideName : string = "中中"

    let private longName : string = String.replicate 260 "z"

    /// Long enough that splicing it in front of any remainder overflows the
    /// tight PATH_MAX of 256, and short enough that it does not overflow
    /// Darwin's 1024 on its own — so the recheck arm is reached under one
    /// limit set and skipped under another.
    let private longTarget : string = String.replicate 250 "z"

    // ------------------------------------------------------ a fixed corpus

    let private makeDirectory (parent : InodeNumber) (n : string) (mode : int) (vfs : VirtualFileSystem) =
        VirtualFileSystem.createDirectory parent (name n) (PermissionBits.parseOrFail "test" mode) buildTime vfs

    let private orFail (result : Result<'a, UnixError>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"building the corpus failed with %O{error}"

    /// One filesystem holding a specimen of everything the walk dispatches on:
    /// a directory it may search and one it may not, a regular file, links that
    /// are relative, rooted, trailing-separated, dangling, self-referential,
    /// growing and over-long, and names that are over-long in bytes, in code
    /// units, or in neither.
    let private corpus : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let d, vfs = makeDirectory root "d" 0o700 vfs |> orFail
        let _, vfs = makeDirectory d "sub" 0o700 vfs |> orFail

        let _, vfs =
            VirtualFileSystem.createFile d (name "f") (PermissionBits.parseOrFail "test" 0o600) buildTime noBytes vfs
            |> orFail

        let nosearch, vfs = makeDirectory root "nosearch" 0o600 vfs |> orFail

        let _, vfs =
            VirtualFileSystem.createFile
                nosearch
                (name "kid")
                (PermissionBits.parseOrFail "test" 0o600)
                buildTime
                noBytes
                vfs
            |> orFail

        let _, vfs =
            VirtualFileSystem.createFile root (name "f") (PermissionBits.parseOrFail "test" 0o600) buildTime noBytes vfs
            |> orFail

        let _, vfs =
            VirtualFileSystem.createFile
                root
                (name longName)
                (PermissionBits.parseOrFail "test" 0o600)
                buildTime
                noBytes
                vfs
            |> orFail

        let _, vfs =
            VirtualFileSystem.createFile
                root
                (name wideName)
                (PermissionBits.parseOrFail "test" 0o600)
                buildTime
                noBytes
                vfs
            |> orFail

        let link (n : string) (t : string) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createSymlink (VirtualFileSystem.root vfs) (name n) buildTime (target t) vfs
            |> orFail
            |> snd

        vfs
        |> link "l" "d"
        |> link "ld" "d/"
        |> link "lf" "f"
        |> link "labs" "/d"
        |> link "lup" ".."
        |> link "lroot" "/"
        |> link "ldangling" "nx"
        |> link "lself" "lself"
        |> link "lgrow" "lgrow/x"
        |> link "llong" longTarget

    /// Paths chosen to reach every arm of the walk, in both the final and the
    /// non-final position where the arm has both.
    let private corpusPaths : string list =
        [
            ""
            "/"
            "."
            ".."
            "/.."
            "./."
            "d"
            "d/"
            "d/f"
            "d/f/"
            "d/f/x"
            "d/sub"
            "d/./sub"
            "d/../d/sub"
            "d//sub"
            "d///"
            "/d/sub/"
            "nx"
            "nx/"
            "nx/x"
            "d/nx"
            "d/nx/x"
            "nosearch"
            "nosearch/"
            "nosearch/kid"
            "nosearch/."
            "nosearch/.."
            "l"
            "l/"
            "l/f"
            "l/sub"
            "ld"
            "ld/"
            "lf"
            "lf/"
            "lf/x"
            "labs"
            "labs/sub"
            "lup"
            "lup/d"
            "lroot"
            "lroot/d"
            "ldangling"
            "ldangling/"
            "ldangling/x"
            "lself"
            "lself/x"
            "lgrow"
            "llong"
            "llong/a"
            longName
            longName + "/x"
            "d/" + longName
            wideName
            wideName + "/x"
            "d/" + wideName
        ]

    /// The four places a relative walk can be told to start: the root, an
    /// ordinary directory, a regular file (ENOTDIR) and an inode the graph does
    /// not contain (ENOENT). A rooted path ignores all of them, which is itself
    /// a row worth having on both sides.
    let private startDirectories : InodeNumber list =
        let rootInode = VirtualFileSystem.root corpus

        let resolved (p : string) =
            VirtualFileSystem.resolveExisting
                (SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64)
                CallerPrivilege.Privileged
                rootInode
                SymlinkPolicy.Follow
                (path p)
                corpus
            |> orFail

        [ rootInode ; resolved "d" ; resolved "f" ; VirtualFileSystem.nextInode corpus ]

    /// How a row came out, coarsely enough to enumerate: this is the alphabet
    /// the sweep asserts it covered, so that "the two agree" is not satisfied by
    /// a corpus on which both answer ENOENT every time.
    let private classify (outcome : Result<Resolution, UnixError>) : string =
        match outcome with
        | Error error -> $"Error %O{error}"
        | Ok resolution ->
            match resolution.Target with
            | ResolvedTarget.Directory (_, reachedBy) -> $"Directory %O{reachedBy}"
            | ResolvedTarget.Entry (_, _, None) -> "Entry free"
            | ResolvedTarget.Entry (_, _, Some _) -> "Entry bound"

    [<Test>]
    let ``the split walk answers what the walk before it answered`` () : unit =
        let mutable rows = 0
        let observed = System.Collections.Generic.HashSet<string> ()

        for _, limits in allLimits do
            for privilege in allPrivileges do
                for policy in allPolicies do
                    for trailing in allTrailingPolicies do
                        for start in startDirectories do
                            for p in corpusPaths do
                                let expected =
                                    referenceResolveFull limits privilege start policy trailing (path p) corpus

                                let actual =
                                    VirtualFileSystem.resolveFull limits privilege start policy trailing (path p) corpus

                                if actual <> expected then
                                    failwith
                                        $"resolving \"%s{p}\" from %O{start} under %O{policy}/%O{trailing}/%O{privilege} gave %O{actual}, where the pre-split walk gave %O{expected}"

                                rows <- rows + 1
                                observed.Add (classify expected) |> ignore<bool>

        // A guard on the corpus rather than on the code: every one of these is
        // an arm the two walks could disagree about, and a corpus that stopped
        // reaching one would let the disagreement through silently.
        let required =
            [
                "Error ENOENT"
                "Error ENOTDIR"
                "Error EACCES"
                "Error ELOOP"
                "Error ENAMETOOLONG"
                "Error EISDIR"
                "Directory Root"
                "Directory Current"
                "Directory Parent"
                "Entry free"
                "Entry bound"
            ]

        let missing = required |> List.filter (observed.Contains >> not)

        if not (List.isEmpty missing) then
            failwith $"""the corpus never produced: %s{String.concat ", " missing}"""

        // Not an assertion about the walk; an assertion that the loops above
        // were not silently emptied by an edit to one of the lists.
        rows
        |> shouldEqual (allLimits.Length * 2 * 2 * 3 * startDirectories.Length * corpusPaths.Length)

    // ---------------------------------------------- randomised filesystems

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 400

    type private Step =
        | MakeDirectory of parent : int * name : string * mode : int
        | MakeFile of parent : int * name : string
        | MakeSymlink of parent : int * name : string * target : string

    let private stepGen : Gen<Step> =
        let nameGen =
            Gen.elements [ "a" ; "b" ; "c" ; ".hidden" ; "..x" ; "..." ; wideName ; longName ]

        let targetGen =
            Gen.elements
                [
                    "a"
                    "a/b"
                    "/a"
                    ".."
                    "."
                    "a/"
                    "../a"
                    "b"
                    "b/x"
                    "/"
                    longTarget
                    longName
                ]

        // 0o600 has no search bit and 0o100 has nothing else, which is the pair
        // that tells "the walk checks search" from "the walk checks read".
        let modeGen = Gen.elements [ 0o700 ; 0o600 ; 0o100 ; 0o000 ]

        Gen.oneof
            [
                Gen.map3 (fun p n m -> Step.MakeDirectory (p, n, m)) (Gen.choose (0, 5)) nameGen modeGen
                Gen.map2 (fun p n -> Step.MakeFile (p, n)) (Gen.choose (0, 5)) nameGen
                Gen.map3 (fun p n t -> Step.MakeSymlink (p, n, t)) (Gen.choose (0, 5)) nameGen targetGen
            ]

    let private applyStep (step : Step) (vfs : VirtualFileSystem) : VirtualFileSystem =
        let directories =
            VirtualFileSystem.inodes vfs
            |> Map.toList
            |> List.filter (fun (_, entry) ->
                match entry.Content with
                | InodeContent.Directory _ -> true
                | InodeContent.RegularFile _
                | InodeContent.Symlink _ -> false
            )
            |> List.map fst

        let pick (i : int) : InodeNumber =
            directories.[i % List.length directories]

        let outcome =
            match step with
            | Step.MakeDirectory (p, n, m) ->
                VirtualFileSystem.createDirectory (pick p) (name n) (PermissionBits.parseOrFail "test" m) buildTime vfs
                |> Result.map snd
            | Step.MakeFile (p, n) ->
                VirtualFileSystem.createFile
                    (pick p)
                    (name n)
                    (PermissionBits.parseOrFail "test" 0o600)
                    buildTime
                    noBytes
                    vfs
                |> Result.map snd
            | Step.MakeSymlink (p, n, t) ->
                VirtualFileSystem.createSymlink (pick p) (name n) buildTime (target t) vfs
                |> Result.map snd

        // A rejected step (EEXIST, mostly) leaves the filesystem alone.
        match outcome with
        | Ok updated -> updated
        | Error _ -> vfs

    type private Case =
        {
            FileSystem : VirtualFileSystem
            Start : InodeNumber
            Path : UnixPath
            Limits : PathLimits
            Privilege : CallerPrivilege
            Policy : SymlinkPolicy
            Trailing : TrailingSeparatorPolicy
        }

    let private caseGen : Gen<Case> =
        gen {
            let! steps = Gen.listOf stepGen

            let vfs =
                steps
                |> List.fold (fun vfs step -> applyStep step vfs) (VirtualFileSystem.empty buildTime)

            // Every inode, not only the directories: a start that is a file or a
            // symlink is ENOTDIR, and one past the end is ENOENT, and both are
            // rows the two walks answer before the walk begins.
            let! start =
                Gen.elements (
                    VirtualFileSystem.nextInode vfs
                    :: (VirtualFileSystem.inodes vfs |> Map.toList |> List.map fst)
                )

            let! p =
                Gen.elements
                    [
                        ""
                        "/"
                        "."
                        ".."
                        "a"
                        "a/"
                        "a/b"
                        "a/b/c"
                        "a//b"
                        "a/./b"
                        "a/../b"
                        "/a/b"
                        "b"
                        "b/"
                        "c/x"
                        ".hidden"
                        "..x/a"
                        wideName
                        wideName + "/a"
                        longName
                        "a/" + longName
                    ]

            let! _, limits = Gen.elements allLimits
            let! privilege = Gen.elements allPrivileges
            let! policy = Gen.elements allPolicies
            let! trailing = Gen.elements allTrailingPolicies

            return
                {
                    FileSystem = vfs
                    Start = start
                    Path = path p
                    Limits = limits
                    Privilege = privilege
                    Policy = policy
                    Trailing = trailing
                }
        }

    [<Test>]
    let ``the split walk agrees with the walk before it on any filesystem`` () : unit =
        let property (case : Case) : unit =
            let expected =
                referenceResolveFull
                    case.Limits
                    case.Privilege
                    case.Start
                    case.Policy
                    case.Trailing
                    case.Path
                    case.FileSystem

            let actual =
                VirtualFileSystem.resolveFull
                    case.Limits
                    case.Privilege
                    case.Start
                    case.Policy
                    case.Trailing
                    case.Path
                    case.FileSystem

            actual |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    // ------------------------------------------------ where the pause falls

    let private linuxLimits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    let private parentOf (limits : PathLimits) (privilege : CallerPrivilege) (p : string) (vfs : VirtualFileSystem) =
        VirtualFileSystem.resolveParent
            limits
            privilege
            (VirtualFileSystem.root vfs)
            SymlinkPolicy.NoFollowFinal
            TrailingSeparatorPolicy.Ignore
            (path p)
            vfs

    /// What a paused resolution cannot answer: `PausedResolution` has no
    /// equality, on purpose, so a refusal is compared as the error alone.
    let private refusal (outcome : Result<PausedResolution, UnixError>) : UnixError option =
        match outcome with
        | Ok _ -> None
        | Error error -> Some error

    /// The whole point of the split, and the two rows that fix where it falls.
    /// Linux's `rename` walks both parents before it looks either final name
    /// up, so `rename("<300 bytes>", "nodir/x")` is ENOENT — the destination's
    /// parent fails while the source's final name is still unmeasured — while
    /// `rename("unsearchable/f", "nodir/x")` is EACCES, because the holding
    /// directory's search bit *is* consulted in the parent phase.
    ///
    /// Both are properties of `resolveParent` alone: the composition answers
    /// the same thing either way, so nothing below the split can see them.
    [<Test>]
    let ``the parent walk checks the holding directory's search bit`` () : unit =
        // `nosearch` is 0o600, so its owner may read it and may not search it.
        parentOf linuxLimits CallerPrivilege.Unprivileged "nosearch/kid" corpus
        |> refusal
        |> shouldEqual (Some UnixError.EACCES)

        // The same walk under privilege gets past it, so the row above is the
        // search check rather than anything else about that directory.
        parentOf linuxLimits CallerPrivilege.Privileged "nosearch/kid" corpus
        |> refusal
        |> shouldEqual None

    [<Test>]
    let ``the parent walk leaves the final name's length unmeasured`` () : unit =
        // A 260-byte final component, over Linux's NAME_MAX of 255. The parent
        // walk must not notice.
        match parentOf linuxLimits CallerPrivilege.Unprivileged longName corpus with
        | Error error ->
            failwith
                $"resolveParent refused an over-long *final* name with %O{error}; it is the lookup's job to refuse it"
        | Ok paused ->
            VirtualFileSystem.completeResolution paused
            |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``the parent walk does measure a non-final name's length`` () : unit =
        // The same name one position earlier. Here the length *is* the parent
        // walk's business, which is what makes "<300 bytes>/x" ENAMETOOLONG
        // while "nxdir/<300 bytes>" is ENOENT.
        parentOf linuxLimits CallerPrivilege.Unprivileged (longName + "/x") corpus
        |> refusal
        |> shouldEqual (Some UnixError.ENAMETOOLONG)

    [<Test>]
    let ``completing a resolution twice answers the same thing twice`` () : unit =
        // A paused resolution carries the filesystem it began against, so it is
        // a value rather than a position in a mutation: the caller cannot
        // resume it against a different filesystem, and resuming it twice
        // cannot differ.
        for p in corpusPaths do
            match parentOf linuxLimits CallerPrivilege.Unprivileged p corpus with
            | Error _ -> ()
            | Ok paused ->
                VirtualFileSystem.completeResolution paused
                |> shouldEqual (VirtualFileSystem.completeResolution paused)

    [<Test>]
    let ``a forged paused resolution is refused loudly`` () : unit =
        let exn =
            Assert.Throws<exn> (fun () ->
                VirtualFileSystem.completeResolution Unchecked.defaultof<PausedResolution>
                |> ignore<Result<Resolution, UnixError>>
            )

        exn.Message |> shouldContainText "resolveParent"
