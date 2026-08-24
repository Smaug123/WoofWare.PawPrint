namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `PathCursor` is the resolution walk's spine; unlike a `PathComponent list`
/// it knows where in the pathname buffer it is — which is what Darwin's
/// symlink-splice length rule needs and a component list cannot say.
///
/// Two things need pinning, and they are different in kind:
///
///   * that the cursor **decomposes** a path exactly as splitting on the
///     separator does, and that a whole resolution walk over it answers
///     exactly as a component-list walk does. These are equivalence claims,
///     checked against reference implementations vendored below;
///   * that `remainingBytes` reports what a **real kernel** would have left in
///     its buffer. That is not an equivalence claim and has no reference
///     implementation — writing one would restate the code under test — so
///     it is checked against numbers bisected out of a live Darwin kernel.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPathCursor =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private limits : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    // -------------------------------------------------------------- generators

    /// Segments chosen to exercise every case the decomposition distinguishes:
    /// ordinary names, "." and "..", and multibyte text whose UTF-8 byte count
    /// differs from its UTF-16 length (three bytes per unit for CJK, and an
    /// astral character that is two UTF-16 units and four UTF-8 bytes).
    let private segmentGen : Gen<string> =
        Gen.elements [ "a" ; "bb" ; "." ; ".." ; "中" ; "\U0001F436" ; "x中y" ; "...." ]

    /// Runs, not single separators: the whole point of keeping a path verbatim
    /// is that "a//b" and "a/b" are different text.
    let private separatorRunGen : Gen<string> = Gen.elements [ "/" ; "//" ; "///" ]

    let private optional (g : Gen<string>) : Gen<string> =
        Gen.frequency [ 2, Gen.constant "" ; 3, g ]

    let private pathStringGen : Gen<string> =
        gen {
            let! leading = optional separatorRunGen
            let! count = Gen.choose (0, 5)
            let! segments = Gen.listOfLength count segmentGen
            let! separators = Gen.listOfLength count separatorRunGen
            let! trailing = optional separatorRunGen

            let body =
                match segments with
                | [] -> ""
                | first :: rest ->
                    List.zip (List.truncate rest.Length separators) rest
                    |> List.map (fun (separator, segment) -> separator + segment)
                    |> String.concat ""
                    |> (+) first

            // A trailing run on a path that named nothing would just be more
            // leading separators, which `leading` already covers.
            return leading + body + (if List.isEmpty segments then "" else trailing)
        }

    // -------------------------------------------------- reference decomposition

    /// Reference decomposition, independent of the cursor: split on the
    /// separator and drop the empty segments. The cursor scans instead, so
    /// agreeing with this is a real claim rather than a tautology.
    let private referenceSegments (raw : string) : string list =
        raw.Split UnixPathText.separator
        |> Array.filter (fun segment -> segment.Length > 0)
        |> Array.toList

    let private segmentOf (component_ : PathComponent) : string =
        match component_ with
        | PathComponent.Current -> "."
        | PathComponent.Parent -> ".."
        | PathComponent.Name name -> FileName.toString name

    // ------------------------------------------------------------- equivalence

    [<Test>]
    let ``the cursor decomposes a path exactly as splitting on the separator does`` () : unit =
        let property (candidate : string) : unit =
            let parsed = path candidate

            UnixPath.components parsed
            |> List.map segmentOf
            |> shouldEqual (referenceSegments candidate)

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``a verbatim path reports rootedness, emptiness and its trailing separator from its text`` () : unit =
        let property (candidate : string) : unit =
            let parsed = path candidate

            UnixPath.toString parsed |> shouldEqual candidate
            UnixPath.isRooted parsed |> shouldEqual (candidate.StartsWith "/")
            UnixPath.isEmpty parsed |> shouldEqual (candidate = "")

            // "at least one component precedes it" is what distinguishes a
            // trailing separator from the one that roots "/" or "///".
            let expected =
                candidate.EndsWith "/" && not (List.isEmpty (referenceSegments candidate))

            UnixPath.hasTrailingSeparator parsed |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``exhaustion means no component remains, however many separators do`` () : unit =
        let property (candidate : string) : unit =
            let rec countdown (cursor : PathCursor) (remaining : int) : unit =
                PathCursor.isExhausted cursor |> shouldEqual (remaining = 0)

                match PathCursor.next cursor with
                | None -> remaining |> shouldEqual 0
                | Some (_, rest) -> countdown rest (remaining - 1)

            countdown (PathCursor.ofPath (path candidate)) (List.length (referenceSegments candidate))

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    /// The cursor advanced past `count` components, or `None` if it runs out.
    let private advance (count : int) (cursor : PathCursor) : PathCursor option =
        let rec go (n : int) (cursor : PathCursor) : PathCursor option =
            if n = 0 then
                Some cursor
            else

            match PathCursor.next cursor with
            | None -> None
            | Some (_, rest) -> go (n - 1) rest

        go count cursor

    [<Test>]
    let ``splicing a target agrees with appending its components, wherever the walk has got to`` () : unit =
        // The reference splice is list append: `components target @ rest`. The
        // cursor concatenates *text* instead, because only text carries the byte
        // count. Those must decompose the same way.
        let property (targetText : string, remainderText : string, skip : int) : unit =
            let target = path targetText
            let segments = referenceSegments remainderText

            // `skip` is at least one: a splice replaces the component the walk
            // has just consumed, so a cursor that has consumed nothing is a
            // misuse rather than an input. The test below pins that it is loud.
            if skip > List.length segments then
                ()
            else

            match advance skip (PathCursor.ofPath (path remainderText)) with
            | None -> ()
            | Some rest ->

            let componentOf (segment : string) : PathComponent =
                UnixPath.components (path segment) |> List.exactlyOne

            let expected =
                UnixPath.components target
                @ (segments |> List.skip skip |> List.map componentOf)

            let rec drain (cursor : PathCursor) (acc : PathComponent list) : PathComponent list =
                match PathCursor.next cursor with
                | None -> List.rev acc
                | Some (component_, next) -> drain next (component_ :: acc)

            drain (PathCursor.splice target rest) [] |> shouldEqual expected

        let generator =
            gen {
                let! target = pathStringGen
                let! remainder = pathStringGen
                let! skip = Gen.choose (1, 5)
                return target, remainder, skip
            }

        Check.One (config, Prop.forAll (Arb.fromGen generator) property)

    [<Test>]
    let ``splicing onto a cursor that has consumed nothing is refused`` () : unit =
        // A splice onto a fresh cursor would silently fuse the target with the
        // next component ("🐶" + "a" becoming the entry name "🐶a"), resolving
        // a path no kernel would.
        let fresh = PathCursor.ofPath (path "abc/d")

        let exn =
            Assert.Throws<exn> (fun () -> PathCursor.splice (path "l") fresh |> ignore<PathCursor>)

        exn.Message |> shouldContainText "has not consumed a component"

        // ...whereas one component in, the same splice is fine.
        match PathCursor.next fresh with
        | None -> failwith "\"abc/d\" has components"
        | Some (_, rest) -> PathCursor.splice (path "l") rest |> PathCursor.isExhausted |> shouldEqual false

    [<Test>]
    let ``a forged default cursor is refused by every entry point`` () : unit =
        // `PathCursor` is a struct, so C# `default` and `Unchecked.defaultof`
        // sidestep `ofPath` entirely and leave the buffer null. Every function
        // must name that rather than throwing NullReferenceException from
        // inside a scan — and must not treat it as the empty path, which would
        // resolve as "the directory I started from" instead of the ENOENT the
        // empty path owes its caller.
        let forged = Unchecked.defaultof<PathCursor>

        let refuses (what : string) (action : unit -> unit) : unit =
            let exn = Assert.Throws<exn> (fun () -> action ())

            exn.Message
            |> shouldContainText "came from `Unchecked.defaultof` or C# `default`"

            exn.Message |> shouldContainText "PathCursor"
            ignore<string> what

        refuses "next" (fun () -> PathCursor.next forged |> ignore<(PathComponent * PathCursor) option>)
        refuses "isExhausted" (fun () -> PathCursor.isExhausted forged |> ignore<bool>)
        refuses "remainingBytes" (fun () -> PathCursor.remainingBytes forged |> ignore<int>)
        refuses "splice" (fun () -> PathCursor.splice (path "l") forged |> ignore<PathCursor>)

    // ------------------------------------------ remainingBytes, against Darwin

    [<Test>]
    let ``remainingBytes reports what a real kernel would have left in its buffer`` () : unit =
        // Bisected on Darwin 25.6.0 (macOS 26.6, APFS): for a link L whose
        // dangling target is T bytes, the largest T that still resolves through
        // "L<suffix>". XNU refuses when `linklen + ni_pathlen > MAXPATHLEN`, so
        // the measurement gives `ni_pathlen = 1024 - T`, and `remainingBytes` is
        // that less the NUL.
        //
        // These numbers come from a kernel, not from this code. In particular
        // the rows that look redundant are not: "//a" and "///a" cost the same
        // as "/a" because the kernel collapses the run *adjacent to* the
        // component it just consumed, while "/a//b" costs one byte more than
        // "/a/b" because an interior run is not collapsed. Those two facts
        // together are what a component list cannot represent.
        let measured =
            [
                // suffix, largest target length that still resolved
                "", 1023
                "/", 1023
                "//", 1023
                "///", 1023
                "/a", 1021
                "/a/", 1020
                "//a", 1021
                "///a", 1021
                "/a/b", 1019
                "/a//b", 1018
                "/a///b", 1017
                "/./a", 1019
                "/a/.", 1019
                "/..", 1020
                "/../..", 1017
                "/a/..", 1018
                "/./.", 1019
                "/a/../b", 1016
            ]

        for suffix, largestResolvingTarget in measured do
            let cursor = PathCursor.ofPath (path ("L" + suffix))

            let afterL =
                match PathCursor.next cursor with
                | Some (_, rest) -> rest
                | None -> failwith $"the probe path \"L%s{suffix}\" must begin with a component"

            let niPathLen = 1024 - largestResolvingTarget

            PathCursor.remainingBytes afterL |> shouldEqual (niPathLen - 1)

    [<Test>]
    let ``remainingBytes counts UTF-8 bytes, not UTF-16 code units`` () : unit =
        // The distinguishing measurement, because every row above is ASCII and
        // would pass either way. On Darwin 25.6.0 a CJK-spelled target of 1022
        // raw bytes is refused where 1019 resolves, through a 2-byte remainder —
        // i.e. the budget tracks bytes. Counting UTF-16 units, as the NAME_MAX
        // rule next door legitimately does on Darwin, would permit roughly three
        // times as much path.
        let cjk = "中" // 3 UTF-8 bytes, 1 UTF-16 code unit
        let dog = "\U0001F436" // 4 UTF-8 bytes, 2 UTF-16 code units

        let remainingAfterFirst (candidate : string) : int =
            match PathCursor.next (PathCursor.ofPath (path candidate)) with
            | Some (_, rest) -> PathCursor.remainingBytes rest
            | None -> failwith $"\"%s{candidate}\" must begin with a component"

        // "/" plus three bytes.
        remainingAfterFirst ("L/" + cjk) |> shouldEqual 4
        // "/" plus four bytes, where String.Length would say 3.
        remainingAfterFirst ("L/" + dog) |> shouldEqual 5
        // The multibyte text ahead of the cursor is what counts, not behind it.
        remainingAfterFirst (cjk + "/a") |> shouldEqual 2

        String.length (cjk + cjk) |> shouldEqual 2
        remainingAfterFirst ("L/" + cjk + cjk) |> shouldEqual 7

    // ------------------------------------------------- whole-walk equivalence

    /// A reference resolution walk over a component-list spine, independent of
    /// the cursor. Deliberately a copy: if the production walk is edited, this
    /// must not follow, or the property stops meaning anything.
    let private referenceResolveFull
        (limits : PathLimits)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (target : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<Resolution, UnixError>
        =
        let tryGetDirectory (inode : InodeNumber) : DirectoryContent option =
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory content) -> Some content
            | _ -> None

        let parentOf (directory : InodeNumber) : InodeNumber =
            match tryGetDirectory directory with
            | Some content -> content.Parent
            | None -> failwith "reference walk: not a directory"

        if UnixPath.isEmpty target then
            Error UnixError.ENOENT
        else

        let start =
            if UnixPath.isRooted target then
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
            (remaining : PathComponent list)
            (trailing : bool)
            (finalSymlinkFollowed : bool)
            (lastNavigation : FinalNavigation)
            (symlinks : int)
            : Result<Resolution, UnixError>
            =
            match remaining with
            | [] ->
                Ok
                    {
                        Target = ResolvedTarget.Directory (directory, lastNavigation)
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    }
            | PathComponent.Current :: rest ->
                walk directory rest trailing finalSymlinkFollowed FinalNavigation.Current symlinks
            | PathComponent.Parent :: rest ->
                walk (parentOf directory) rest trailing finalSymlinkFollowed FinalNavigation.Parent symlinks
            | PathComponent.Name entryName :: rest ->

            if not (PathLimits.nameWithinLimit limits entryName) then
                Error UnixError.ENAMETOOLONG
            else

            let entries =
                match tryGetDirectory directory with
                | Some content -> content.Entries
                | None -> failwith "reference walk: not a directory"

            let isFinal = List.isEmpty rest

            let finish (resolved : ResolvedTarget) : Result<Resolution, UnixError> =
                Ok
                    {
                        Target = resolved
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    }

            match Map.tryFind entryName entries with
            | None ->
                if isFinal then
                    finish (ResolvedTarget.Entry (directory, entryName, None))
                else
                    Error UnixError.ENOENT
            | Some found ->

            let content =
                match VirtualFileSystem.tryGetContent found vfs with
                | Some content -> content
                | None -> failwith "reference walk: dangling entry"

            let followFinal = policy = SymlinkPolicy.Follow || trailing

            match content with
            | InodeContent.Symlink linkTarget when not isFinal || followFinal ->
                if symlinks + 1 > PathLimits.maxSymlinkTraversals limits then
                    Error UnixError.ELOOP
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

                let spliced = UnixPath.components linkPath @ rest

                let lastNavigation =
                    if List.isEmpty spliced then
                        FinalNavigation.Root
                    else
                        lastNavigation

                walk next spliced trailing (finalSymlinkFollowed || isFinal) lastNavigation (symlinks + 1)
            | InodeContent.Symlink _ -> finish (ResolvedTarget.Entry (directory, entryName, Some found))
            | InodeContent.Directory _ ->
                if isFinal then
                    finish (ResolvedTarget.Entry (directory, entryName, Some found))
                else
                    walk found rest trailing finalSymlinkFollowed lastNavigation symlinks
            | InodeContent.RegularFile _ ->
                if isFinal then
                    if trailing then
                        Error UnixError.ENOTDIR
                    else
                        finish (ResolvedTarget.Entry (directory, entryName, Some found))
                else
                    Error UnixError.ENOTDIR

        walk start (UnixPath.components target) (UnixPath.hasTrailingSeparator target) false FinalNavigation.Root 0

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    /// A filesystem holding every shape the walk branches on: nested
    /// directories, a regular file, and symbolic links that are absolute,
    /// relative, trailing-separator-bearing, dangling, chained, cyclic, rooted
    /// at "/", and pointing at "..".
    let private universe : VirtualFileSystem * InodeNumber =
        let ok (result : Result<'a, UnixError>) : 'a =
            match result with
            | Ok value -> value
            | Error error -> failwith $"building the test filesystem: %O{error}"

        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let d1, vfs =
            VirtualFileSystem.createDirectory root (name "d1") PermissionBits.defaultForDirectory buildTime vfs
            |> ok

        let d2, vfs =
            VirtualFileSystem.createDirectory d1 (name "d2") PermissionBits.defaultForDirectory buildTime vfs
            |> ok

        let _, vfs =
            VirtualFileSystem.createFile
                d2
                (name "f")
                PermissionBits.defaultForRegularFile
                buildTime
                ImmutableArray.Empty
                vfs
            |> ok

        let _, vfs =
            VirtualFileSystem.createFile
                root
                (name "f1")
                PermissionBits.defaultForRegularFile
                buildTime
                ImmutableArray.Empty
                vfs
            |> ok

        let link (parent : InodeNumber) (n : string) (t : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
            VirtualFileSystem.createSymlink parent (name n) buildTime (SymlinkTarget.parseOrFail "test" t) vfs
            |> ok
            |> snd

        let vfs =
            vfs
            |> link root "labs" "/d1/d2"
            |> link root "lrel" "d1"
            |> link root "ltr" "d1/"
            |> link root "lrun" "d1//d2"
            |> link root "ldang" "nx/nx"
            |> link root "lroot" "/"
            |> link root "lup" ".."
            |> link root "lself" "lself"
            |> link root "lc1" "lc2"
            |> link root "lc2" "d1"
            |> link d1 "lpar" "../"
            |> link d1 "ldot" "."

        vfs, d1

    /// Names drawn from `universe`, plus one that does not exist and the two
    /// navigation components.
    let private universeSegmentGen : Gen<string> =
        Gen.elements
            [
                "d1"
                "d2"
                "f"
                "f1"
                "labs"
                "lrel"
                "ltr"
                "lrun"
                "ldang"
                "lroot"
                "lup"
                "lself"
                "lc1"
                "lpar"
                "ldot"
                "nx"
                "."
                ".."
            ]

    let private universePathGen : Gen<string> =
        gen {
            let! leading = optional separatorRunGen
            let! count = Gen.choose (1, 4)
            let! segments = Gen.listOfLength count universeSegmentGen
            let! separators = Gen.listOfLength count separatorRunGen
            let! trailing = optional separatorRunGen

            let body =
                match segments with
                | [] -> ""
                | first :: rest ->
                    List.zip (List.truncate rest.Length separators) rest
                    |> List.map (fun (separator, segment) -> separator + segment)
                    |> String.concat ""
                    |> (+) first

            return leading + body + trailing
        }

    [<Test>]
    let ``the cursor walk resolves exactly as the component-list walk did`` () : unit =
        let vfs, d1 = universe
        let root = VirtualFileSystem.root vfs

        let property (candidate : string, fromRoot : bool, follow : bool) : unit =
            let start = if fromRoot then root else d1

            let policy =
                if follow then
                    SymlinkPolicy.Follow
                else
                    SymlinkPolicy.NoFollowFinal

            let parsed = path candidate

            VirtualFileSystem.resolveFull
                limits
                CallerPrivilege.Privileged
                start
                policy
                TrailingSeparatorPolicy.Demand
                parsed
                vfs
            |> shouldEqual (referenceResolveFull limits start policy parsed vfs)

        let generator =
            gen {
                let! candidate = universePathGen
                let! fromRoot = Gen.elements [ true ; false ]
                let! follow = Gen.elements [ true ; false ]
                return candidate, fromRoot, follow
            }

        Check.One (config, Prop.forAll (Arb.fromGen generator) property)
