namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `VirtualFileSystem.nextDirectoryEntry` seeks to the least name above its
/// cursor in names the filesystem keeps sorted beside its graph. These hold it
/// to the definition it replaces: a scan of the directory's entries.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryEntrySeek =

    /// The rarest thing the history property's coverage guard asks for, a read
    /// of a stream over a directory `rmdir` has removed, was measured at 76 to
    /// 102 occurrences in each of three checks of 1000 cases.
    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    let private tick (index : int) : UnixTimestamp =
        UnixTimestamp.createOrFail "test" (1_700_000_000L + int64 index) 0

    let private ok (context : string) (result : Result<'a, UnixError>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"%s{context}: expected success, got %O{error}"

    let private nameOfBytes (bytes : byte[]) : DirectoryEntryName =
        match UnixByteString.ofBytes (ImmutableArray.Create<byte> bytes) with
        | Error defect -> failwith $"test setup: %O{defect}"
        | Ok raw ->
            match DirectoryEntryName.ofByteString raw with
            | Error error -> failwith $"test setup: %O{error}"
            | Ok name -> name

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    /// The names a history binds: one a prefix of others, both cases of a
    /// letter, and bytes above 0x7F, which sort after every ASCII name only
    /// when bytes compare unsigned.
    let private alphabet : DirectoryEntryName list =
        [
            "a"B
            "aa"B
            "ab"B
            "b"B
            "B"B
            "Z"B
            [| 0x80uy |]
            [| 0xFFuy |]
            [| 0x61uy ; 0xFFuy |]
        ]
        |> List.map nameOfBytes

    /// Names no history binds, each falling below, between or above the
    /// alphabet's, for cursors resting on a name the directory never held.
    let private strangers : DirectoryEntryName list =
        [ "0"B ; "a0"B ; "ba"B ; [| 0xFEuy |] ; [| 0xFFuy ; 0xFFuy |] ]
        |> List.map nameOfBytes

    /// The definition the seek is held to: the least entry above the cursor
    /// found by scanning the directory's entries in order. This is the body
    /// `nextDirectoryEntry` had before it seeked, verbatim but for reading the
    /// directory through `tryGetContent`.
    let private scannedNextDirectoryEntry
        (directory : InodeNumber)
        (cursor : DirectoryCursor)
        (vfs : VirtualFileSystem)
        : (DirectoryStreamName * InodeNumber * DirectoryCursor) option
        =
        let content =
            match VirtualFileSystem.tryGetContent directory vfs with
            | Some (InodeContent.Directory content) -> content
            | other -> failwith $"scannedNextDirectoryEntry: inode %O{directory} holds %O{other}"

        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            None
        else

        let leastAbove (lower : DirectoryEntryName option) : (DirectoryEntryName * InodeNumber) option =
            content.Entries
            |> Map.toSeq
            |> Seq.filter (fun (name, _) ->
                match lower with
                | None -> true
                | Some lower -> name > lower
            )
            |> Seq.tryHead

        let fromEntries (lower : DirectoryEntryName option) =
            match leastAbove lower with
            | Some (name, inode) -> Some (DirectoryStreamName.Entry name, inode, DirectoryCursor.After name)
            | None -> Some (DirectoryStreamName.DotDot, content.Parent, DirectoryCursor.ReturnedDotDot)

        match cursor with
        | DirectoryCursor.Start -> fromEntries None
        | DirectoryCursor.After name -> fromEntries (Some name)
        | DirectoryCursor.ReturnedDotDot -> Some (DirectoryStreamName.Dot, directory, DirectoryCursor.ReturnedDot)
        | DirectoryCursor.ReturnedDot -> None

    let private directoriesOf (vfs : VirtualFileSystem) : (InodeNumber * DirectoryContent) list =
        VirtualFileSystem.inodes vfs
        |> Map.toList
        |> List.choose (fun (inode, node) ->
            match node.Content with
            | InodeContent.Directory content -> Some (inode, content)
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> None
        )

    /// Every cursor a stream over `content` could hold: the three that name no
    /// entry, every name it binds, and every name of the alphabet or the
    /// strangers, bound or not.
    let private cursorsOver (content : DirectoryContent) : DirectoryCursor list =
        [
            yield DirectoryCursor.Start
            yield DirectoryCursor.ReturnedDotDot
            yield DirectoryCursor.ReturnedDot
            for name in content.Entries |> Map.keys do
                yield DirectoryCursor.After name
            for name in alphabet @ strangers do
                yield DirectoryCursor.After name
        ]

    /// The seek and the scan agree on every directory the graph holds from
    /// every cursor, and the filesystem is sound.
    let private assertAgrees (context : string) (pinned : Set<InodeNumber>) (vfs : VirtualFileSystem) : unit =
        for directory, content in directoriesOf vfs do
            for cursor in cursorsOver content do
                let sought = VirtualFileSystem.nextDirectoryEntry directory cursor vfs
                let scanned = scannedNextDirectoryEntry directory cursor vfs

                if sought <> scanned then
                    failwith
                        $"%s{context}: from %O{cursor} in directory %O{directory}, nextDirectoryEntry answered %O{sought} where the scan answers %O{scanned}"

        match VirtualFileSystem.checkInvariants pinned vfs with
        | [] -> ()
        | defects -> failwith $"%s{context}: the filesystem is unsound: %A{defects}"

    // ------------------------------------------------------- the operations

    /// One operation on the graph or on an open stream. Every index is resolved
    /// modulo the list it indexes at the moment the operation runs, counting
    /// from the end, so that every generated operation names something that
    /// exists and most land on what was made most recently.
    type private Op =
        /// A regular file or an empty directory, in a directory the root
        /// still reaches.
        | Create of directory : int * name : int * isDirectory : bool
        /// `unlink(2)` of a non-directory, or `rmdir(2)` of an empty directory.
        | Remove of binding : int
        /// `rename(2)`, possibly over a name that is already bound.
        | Rename of binding : int * directory : int * name : int
        /// `opendir(3)`: a stream at `Start`, which keeps its directory alive.
        | Open of directory : int
        /// `readdir(3)` on an open stream, `steps` times or until it ends.
        | Advance of stream : int * steps : int
        /// `closedir(3)`.
        | Close of stream : int

    /// Which operations did something, summed across a whole check, so that a
    /// generator which never reaches one fails the test rather than passing it
    /// vacuously.
    [<RequireQualifiedAccess>]
    type private Reached =
        | CreateFile
        | CreateDirectory
        | Unlink
        | RemoveDirectory
        | RenameWithinDirectory
        | RenameAcrossDirectories
        | RenameOverExisting
        | Forget
        /// A stream handed back a name after its directory gained or lost one
        /// since the stream was opened.
        | AdvancePastMutation
        /// A stream reached its end.
        | StreamEnded
        /// A stream over a directory `rmdir` has removed.
        | AdvanceOverOrphan

    let private opGen : Gen<Op> =
        let names = Gen.choose (0, List.length alphabet - 1)
        let index = Gen.frequency [ 4, Gen.choose (0, 2) ; 1, Gen.choose (0, 50) ]

        Gen.frequency
            [
                8,
                Gen.map3
                    (fun d n k -> Op.Create (d, n, k))
                    index
                    names
                    (Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ])
                3, Gen.map Op.Remove index
                4, Gen.map3 (fun b d n -> Op.Rename (b, d, n)) index index names
                2, Gen.map Op.Open index
                4, Gen.map2 (fun s n -> Op.Advance (s, n)) index (Gen.choose (1, 4))
                1, Gen.map Op.Close index
            ]

    let private pick (xs : 'a list) (i : int) : 'a option =
        match xs with
        | [] -> None
        | _ -> Some xs.[List.length xs - 1 - i % List.length xs]

    let private livingDirectories (vfs : VirtualFileSystem) : InodeNumber list =
        directoriesOf vfs
        |> List.map fst
        |> List.filter (fun inode -> not (VirtualFileSystem.isOrphanedDirectory inode vfs))

    let private bindingsOf (vfs : VirtualFileSystem) : (InodeNumber * DirectoryEntryName * InodeNumber) list =
        directoriesOf vfs
        |> List.collect (fun (directory, content) ->
            content.Entries
            |> Map.toList
            |> List.map (fun (name, target) -> directory, name, target)
        )

    let private entriesOf (directory : InodeNumber) (vfs : VirtualFileSystem) : Map<DirectoryEntryName, InodeNumber> =
        match VirtualFileSystem.tryGetContent directory vfs with
        | Some (InodeContent.Directory content) -> content.Entries
        | Some _
        | None -> Map.empty

    let rec private isWithin (ancestor : InodeNumber) (candidate : InodeNumber) (vfs : VirtualFileSystem) : bool =
        if candidate = ancestor then
            true
        elif candidate = VirtualFileSystem.root vfs then
            false
        else

        match VirtualFileSystem.tryGetContent candidate vfs with
        | Some (InodeContent.Directory content) -> isWithin ancestor content.Parent vfs
        | Some _
        | None -> false

    /// `held`, and every directory a held directory's ".." chain passes
    /// through: a directory's ".." keeps its parent alive after the parent's
    /// last name has gone.
    let private withAncestors (held : Set<InodeNumber>) (vfs : VirtualFileSystem) : Set<InodeNumber> =
        let rec climb (frontier : InodeNumber list) (seen : Set<InodeNumber>) : Set<InodeNumber> =
            match frontier with
            | [] -> seen
            | inode :: rest ->
                if Set.contains inode seen then
                    climb rest seen
                else

                let seen = Set.add inode seen

                match VirtualFileSystem.tryGetContent inode vfs with
                | Some (InodeContent.Directory directory) -> climb (directory.Parent :: rest) seen
                | Some _
                | None -> climb rest seen

        climb (Set.toList held) Set.empty

    /// A stream: the directory it reads, its cursor, and the entries the
    /// directory held when it was last read, so that a later read can tell
    /// whether the directory changed beneath it.
    type private Stream =
        {
            Directory : InodeNumber
            Cursor : DirectoryCursor
            LastSeen : Map<DirectoryEntryName, InodeNumber>
        }

    let private heldBy (streams : Stream list) (vfs : VirtualFileSystem) : Set<InodeNumber> =
        withAncestors (streams |> List.map (fun s -> s.Directory) |> Set.ofList) vfs

    /// What a kernel does once a reference to `inode` has gone: free it if it
    /// has no name and no stream holds it, and if it was a directory, do the
    /// same to the parent its ".." held.
    let private reap
        (record : Reached -> unit)
        (streams : Stream list)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        let rec go (inode : InodeNumber) (vfs : VirtualFileSystem) : VirtualFileSystem =
            if inode = VirtualFileSystem.root vfs then
                vfs
            elif Set.contains inode (heldBy streams vfs) then
                vfs
            elif VirtualFileSystem.bindingCount inode vfs <> 0 then
                vfs
            else

            match VirtualFileSystem.tryGetContent inode vfs with
            | None -> vfs
            | Some content ->

            record Reached.Forget
            let vfs = VirtualFileSystem.forget inode vfs

            match content with
            | InodeContent.Directory directory -> go directory.Parent vfs
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> vfs

        go inode vfs

    let private apply
        (record : Reached -> unit)
        (now : UnixTimestamp)
        (op : Op)
        (vfs : VirtualFileSystem, streams : Stream list)
        : VirtualFileSystem * Stream list
        =
        match op with
        | Op.Create (d, n, isDirectory) ->
            match pick (livingDirectories vfs) d with
            | None -> vfs, streams
            | Some directory ->
                let entry = alphabet.[n]

                let created =
                    if isDirectory then
                        VirtualFileSystem.createDirectory
                            directory
                            entry
                            SeedEntry.defaultPermsForDirectory
                            Owners.linuxDefault
                            now
                            vfs
                    else
                        VirtualFileSystem.createFile
                            directory
                            entry
                            SeedEntry.defaultPermsForRegularFile
                            Owners.linuxDefault
                            now
                            ImmutableArray<byte>.Empty
                            vfs

                match created with
                | Ok (_, vfs) ->
                    record (
                        if isDirectory then
                            Reached.CreateDirectory
                        else
                            Reached.CreateFile
                    )

                    vfs, streams
                | Error _ -> vfs, streams
        | Op.Remove b ->
            let candidates =
                bindingsOf vfs
                |> List.filter (fun (_, _, target) ->
                    match VirtualFileSystem.tryGetContent target vfs with
                    | Some (InodeContent.Directory content) -> Map.isEmpty content.Entries
                    | Some _
                    | None -> true
                )

            match pick candidates b with
            | None -> vfs, streams
            | Some (directory, entry, target) ->
                let wasDirectory =
                    match VirtualFileSystem.tryGetContent target vfs with
                    | Some (InodeContent.Directory _) -> true
                    | Some _
                    | None -> false

                let removed, vfs =
                    VirtualFileSystem.unbind UnbindTargetEffect.LostALink directory entry now vfs
                    |> ok "remove"

                record (
                    if wasDirectory then
                        Reached.RemoveDirectory
                    else
                        Reached.Unlink
                )

                reap record streams removed vfs, streams
        | Op.Rename (b, d, n) ->
            match pick (bindingsOf vfs) b, pick (livingDirectories vfs) d with
            | Some (sourceDirectory, sourceName, moved), Some destinationDirectory ->
                let destinationName = alphabet.[n]
                let displaced = Map.tryFind destinationName (entriesOf destinationDirectory vfs)

                let movedIsDirectory =
                    match VirtualFileSystem.tryGetContent moved vfs with
                    | Some (InodeContent.Directory _) -> true
                    | Some _
                    | None -> false

                let displacedIsPopulated =
                    match displaced |> Option.bind (fun d -> VirtualFileSystem.tryGetContent d vfs) with
                    | Some (InodeContent.Directory content) -> not (Map.isEmpty content.Entries)
                    | Some _
                    | None -> false

                // The conditions the verdict answers before the graph is
                // touched, and which `VirtualFileSystem.rename` refuses loudly.
                if
                    displaced = Some moved
                    || (movedIsDirectory && isWithin moved destinationDirectory vfs)
                    || displacedIsPopulated
                then
                    vfs, streams
                else

                let outcome, vfs =
                    VirtualFileSystem.rename sourceDirectory sourceName destinationDirectory destinationName now vfs
                    |> ok "rename"

                record (
                    if sourceDirectory = destinationDirectory then
                        Reached.RenameWithinDirectory
                    else
                        Reached.RenameAcrossDirectories
                )

                match outcome.Displaced with
                | None -> vfs, streams
                | Some displaced ->
                    record Reached.RenameOverExisting
                    reap record streams displaced vfs, streams
            | _ -> vfs, streams
        | Op.Open d ->
            match pick (livingDirectories vfs) d with
            | None -> vfs, streams
            | Some directory ->
                let stream =
                    {
                        Directory = directory
                        Cursor = DirectoryCursor.Start
                        LastSeen = entriesOf directory vfs
                    }

                vfs, streams @ [ stream ]
        | Op.Advance (s, steps) ->
            match pick (List.indexed streams) s with
            | None -> vfs, streams
            | Some (index, stream) ->
                let rec go (remaining : int) (stream : Stream) : Stream =
                    if remaining = 0 then
                        stream
                    else

                    let sought = VirtualFileSystem.nextDirectoryEntry stream.Directory stream.Cursor vfs
                    let scanned = scannedNextDirectoryEntry stream.Directory stream.Cursor vfs

                    if sought <> scanned then
                        failwith
                            $"advancing a stream over %O{stream.Directory} from %O{stream.Cursor}: nextDirectoryEntry answered %O{sought} where the scan answers %O{scanned}"

                    if VirtualFileSystem.isOrphanedDirectory stream.Directory vfs then
                        record Reached.AdvanceOverOrphan

                    let current = entriesOf stream.Directory vfs

                    match sought with
                    | None ->
                        record Reached.StreamEnded
                        stream
                    | Some (entry, _, next) ->
                        match entry with
                        | DirectoryStreamName.Entry _ when current <> stream.LastSeen ->
                            record Reached.AdvancePastMutation
                        | _ -> ()

                        go
                            (remaining - 1)
                            { stream with
                                Cursor = next
                                LastSeen = current
                            }

                let advanced = go steps stream
                vfs, streams |> List.mapi (fun i s -> if i = index then advanced else s)
        | Op.Close s ->
            match pick (List.indexed streams) s with
            | None -> vfs, streams
            | Some (index, stream) ->
                let streams =
                    streams
                    |> List.indexed
                    |> List.filter (fun (i, _) -> i <> index)
                    |> List.map snd

                reap record streams stream.Directory vfs, streams

    let private runHistory (record : Reached -> unit) (ops : Op list) : unit =
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        assertAgrees "on the empty filesystem" Set.empty vfs

        ops
        |> List.indexed
        |> List.fold
            (fun state (index, op) ->
                let vfs, streams = apply record (tick (index + 1)) op state
                assertAgrees $"after step %d{index}, %A{op}" (heldBy streams vfs) vfs
                vfs, streams
            )
            (vfs, [])
        |> ignore<VirtualFileSystem * Stream list>

    [<Test>]
    let ``the seek agrees with the scan from every cursor, through any history`` () : unit =
        let reached = System.Collections.Concurrent.ConcurrentDictionary<Reached, int> ()

        let record (r : Reached) : unit =
            reached.AddOrUpdate (r, 1, fun _ n -> n + 1) |> ignore<int>

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.listOf opGen)) (runHistory record))

        let unreached =
            [
                Reached.CreateFile
                Reached.CreateDirectory
                Reached.Unlink
                Reached.RemoveDirectory
                Reached.RenameWithinDirectory
                Reached.RenameAcrossDirectories
                Reached.RenameOverExisting
                Reached.Forget
                Reached.AdvancePastMutation
                Reached.StreamEnded
                Reached.AdvanceOverOrphan
            ]
            |> List.filter (fun r -> not (reached.ContainsKey r))

        if not (List.isEmpty unreached) then
            failwith $"these were never reached: %A{unreached}; reached: %A{List.ofSeq reached}"

    // ------------------------------------------------------ the invariant

    /// The root, holding "a" and "b".
    let private twoNames () : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let create (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
            VirtualFileSystem.createFile
                root
                (name n)
                SeedEntry.defaultPermsForRegularFile
                Owners.linuxDefault
                buildTime
                ImmutableArray<byte>.Empty
                vfs
            |> ok "create"
            |> snd

        vfs |> create "b" |> create "a"

    [<Test>]
    let ``checkInvariants reports sorted names that disagree with the entries`` () : unit =
        let vfs = twoNames ()
        let root = VirtualFileSystem.root vfs

        VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual []

        let forged (directory : InodeNumber) (names : string list option) : VirtualFileSystemDefect list =
            let names = names |> Option.map (List.map name)

            VirtualFileSystem.checkInvariants Set.empty (VirtualFileSystem.Unchecked.setSortedNames directory names vfs)

        let bound = [ name "a" ; name "b" ]

        // Too few, too many, and none.
        forged root (Some [ "a" ])
        |> shouldEqual [ VirtualFileSystemDefect.SortedNamesMismatch (root, Some [ name "a" ], bound) ]

        forged root (Some [ "a" ; "b" ; "c" ])
        |> shouldEqual
            [
                VirtualFileSystemDefect.SortedNamesMismatch (root, Some [ name "a" ; name "b" ; name "c" ], bound)
            ]

        forged root None
        |> shouldEqual [ VirtualFileSystemDefect.SortedNamesMismatch (root, None, bound) ]

        // Names for an inode that is not a directory.
        let file = (entriesOf root vfs).[name "a"]

        forged file (Some [ "x" ])
        |> shouldEqual [ VirtualFileSystemDefect.SortedNamesMismatch (file, Some [ name "x" ], []) ]

        // An empty set agrees in content, but only directories that bind
        // something have one, so that two filesystems with the same graph are
        // equal.
        let empty, emptied =
            VirtualFileSystem.createDirectory
                root
                (name "d")
                SeedEntry.defaultPermsForDirectory
                Owners.linuxDefault
                buildTime
                vfs
            |> ok "mkdir"

        VirtualFileSystem.checkInvariants Set.empty (VirtualFileSystem.Unchecked.setSortedNames empty (Some []) emptied)
        |> shouldEqual [ VirtualFileSystemDefect.SortedNamesMismatch (empty, Some [], []) ]

    [<Test>]
    let ``Unchecked.ofParts stores the sorted names its entries imply`` () : unit =
        // Built name by name, against built all at once: equal, and comparing
        // equal, because what is compared is the names rather than how the
        // set holding them was assembled.
        let vfs = twoNames ()

        let forged =
            VirtualFileSystem.Unchecked.ofParts
                (VirtualFileSystem.inodes vfs)
                (VirtualFileSystem.root vfs)
                (VirtualFileSystem.nextInode vfs)

        VirtualFileSystem.checkInvariants Set.empty forged |> shouldEqual []
        forged |> shouldEqual vfs
        compare forged vfs |> shouldEqual 0
        hash forged |> shouldEqual (hash vfs)

    [<Test>]
    let ``filesystems whose sorted names differ are unequal`` () : unit =
        // The control for the test above: equality that ignored the sorted
        // names would pass it too.
        let vfs = twoNames ()
        let root = VirtualFileSystem.root vfs
        let forged = VirtualFileSystem.Unchecked.setSortedNames root (Some [ name "a" ]) vfs

        forged |> shouldNotEqual vfs
        compare forged vfs |> shouldNotEqual 0

    [<Test>]
    let ``a sorted name the entries lack is refused rather than handed back`` () : unit =
        let vfs = twoNames ()
        let root = VirtualFileSystem.root vfs

        let forged =
            VirtualFileSystem.Unchecked.setSortedNames root (Some [ name "a" ; name "ab" ; name "b" ]) vfs

        let thrown =
            Assert.Throws<exn> (fun () ->
                VirtualFileSystem.nextDirectoryEntry root (DirectoryCursor.After (name "a")) forged
                |> ignore<(DirectoryStreamName * InodeNumber * DirectoryCursor) option>
            )

        thrown.Message |> shouldContainText "checkInvariants"

    [<Test>]
    let ``unbinding a name the sorted names lack is refused`` () : unit =
        let vfs = twoNames ()
        let root = VirtualFileSystem.root vfs
        let forged = VirtualFileSystem.Unchecked.setSortedNames root (Some [ name "a" ]) vfs

        let thrown =
            Assert.Throws<exn> (fun () ->
                VirtualFileSystem.unbind UnbindTargetEffect.LostALink root (name "b") buildTime forged
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        thrown.Message |> shouldContainText "checkInvariants"

    [<Test>]
    let ``binding a name the sorted names already hold is refused`` () : unit =
        let vfs = twoNames ()
        let root = VirtualFileSystem.root vfs

        let forged =
            VirtualFileSystem.Unchecked.setSortedNames root (Some [ name "a" ; name "b" ; name "c" ]) vfs

        let thrown =
            Assert.Throws<exn> (fun () ->
                VirtualFileSystem.createFile
                    root
                    (name "c")
                    SeedEntry.defaultPermsForRegularFile
                    Owners.linuxDefault
                    buildTime
                    ImmutableArray<byte>.Empty
                    forged
                |> ignore<Result<InodeNumber * VirtualFileSystem, UnixError>>
            )

        thrown.Message |> shouldContainText "checkInvariants"
