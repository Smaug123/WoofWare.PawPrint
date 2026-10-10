namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `VirtualFileSystem.bindingCount` and `subdirectoryCount` answer from counts
/// the filesystem keeps beside its graph, and `entryCount` from the names it
/// keeps sorted. These hold each to its definition, a scan of the entries, and
/// hold the `st_nlink` that `stat` reports from them to the rule measured on
/// each filesystem.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBindingCount =

    /// Sized for the history property's coverage guard rather than for the
    /// property itself. The rarest thing it guards, a freeing cascade, is
    /// reached 31 times in the guard's fixed sample of 3000 cases; fresh
    /// samples reached it 22 to 28 times in 3000, 5 to 14 in 1000 and 0 to 7 in
    /// 300.
    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 3000

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private filePerms : PermissionBits = SeedEntry.defaultPermsForRegularFile

    let private dirPerms : PermissionBits = SeedEntry.defaultPermsForDirectory

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    let private tick (index : int) : UnixTimestamp =
        UnixTimestamp.createOrFail "test" (1_700_000_000L + int64 index) 0

    let private ok (context : string) (result : Result<'a, UnixError>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"%s{context}: expected success, got %O{error}"

    /// The definition of a binding count, and the reference the stored one is
    /// held to: every entry of every directory the graph contains that names
    /// `inode`. This is the body `bindingCount` had before it read a stored
    /// count, verbatim but for reading the graph through `inodes`.
    let private scannedBindingCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        VirtualFileSystem.inodes vfs
        |> Map.toSeq
        |> Seq.sumBy (fun (_, node) ->
            match node.Content with
            | InodeContent.Directory directory ->
                directory.Entries
                |> Map.toSeq
                |> Seq.filter (fun (_, target) -> target = inode)
                |> Seq.length
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> 0
        )

    /// `isOrphanedDirectory` as it was defined over the scan.
    let private scannedIsOrphanedDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        inode <> VirtualFileSystem.root vfs
        && (
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory _) -> scannedBindingCount inode vfs = 0
            | Some _
            | None -> false
        )

    let private isDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory _) -> true
        | Some _
        | None -> false

    /// The definition of `entryCount`: the names the directory at `inode`
    /// binds.
    let private scannedEntryCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Map.count directory.Entries
        | Some _
        | None -> 0

    /// The definition of `subdirectoryCount`: the entries of the directory at
    /// `inode` that name a directory.
    let private scannedSubdirectoryCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) ->
            directory.Entries
            |> Map.toSeq
            |> Seq.filter (fun (_, target) -> isDirectory target vfs)
            |> Seq.length
        | Some _
        | None -> 0

    /// The `st_nlink` measured on each filesystem by `stat-fields.c`, from the
    /// scans alone, or `None` where `stat` must refuse. Every directory here is
    /// far below Darwin's 65535 ceiling.
    let private measuredLinkCount
        (fsType : EmulatedFileSystemType)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : int64 option
        =
        match VirtualFileSystem.tryGetContent inode vfs with
        | None -> failwith $"test bug: inode %O{inode} is not in the graph"
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.CharacterDevice _)
        | Some (InodeContent.Symlink _) -> Some (int64 (scannedBindingCount inode vfs))
        | Some (InodeContent.Directory _) ->
            match fsType with
            | EmulatedFileSystemType.Tmpfs ->
                if scannedIsOrphanedDirectory inode vfs then
                    Some 0L
                else
                    Some (2L + int64 (scannedSubdirectoryCount inode vfs))
            | EmulatedFileSystemType.Apfs -> Some (2L + int64 (scannedEntryCount inode vfs))
            | EmulatedFileSystemType.Nfs -> None

    /// A system on each flavour's default filesystem and on NFS, whose
    /// filesystem a check replaces with the graph under test. `statOf` reads
    /// nothing of a system but its filesystem, its mount and its platform.
    let private statSystems : (SimulatedUnixPlatform * EmulatedFileSystemType * UnixSystem<int, string>) list =
        [
            SimulatedUnixPlatform.linuxX64, EmulatedFileSystemType.Tmpfs
            SimulatedUnixPlatform.macOsArm64, EmulatedFileSystemType.Apfs
            SimulatedUnixPlatform.linuxX64, EmulatedFileSystemType.Nfs
            SimulatedUnixPlatform.macOsArm64, EmulatedFileSystemType.Nfs
        ]
        |> List.map (fun (platform, fsType) ->
            let system : UnixBootImage<int, string> = UnixSystem.initial platform


            platform,
            fsType,
            system
            |> UnixBootImage.withMount (Some (EmulatedMount.defaultOf fsType))
            |> Configured.expectOk MountRefusal.describe
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
        )

    /// What `stat` reports for every inode of `vfs` agrees with the rule
    /// measured on each filesystem.
    let private assertStatAgrees (context : string) (vfs : VirtualFileSystem) : unit =
        for platform, fsType, system in statSystems do
            let system =
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = vfs
                        }
                }

            let darwin = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Darwin

            for inode in VirtualFileSystem.inodes vfs |> Map.keys do
                match UnixPathResolution.statOf inode system, measuredLinkCount fsType inode vfs with
                | Some (Ok status), Some expected ->
                    if status.LinkCount <> expected then
                        failwith
                            $"%s{context}: on %O{fsType}, stat of inode %O{inode} reports st_nlink %d{status.LinkCount}, but the measured rule says %d{expected}"

                    status.SpecialFileDevice |> shouldEqual 0L
                    status.FileFlags |> shouldEqual (if darwin then Some 0u else None)
                | Some (Error (StatRefusal.NfsDirectorySize refused)), None -> refused |> shouldEqual inode
                | answer, expected ->
                    failwith
                        $"%s{context}: on %O{fsType}, stat of inode %O{inode} answered %A{answer}, where the measured rule is %A{expected}"

    /// The stored count and the scan agree on every inode the graph holds, on
    /// the next inode it would allocate, and on one it never will; and
    /// everything that consults the count answers as it did over the scan.
    let private assertAgrees (context : string) (pinned : Set<InodeNumber>) (vfs : VirtualFileSystem) : unit =
        let (InodeNumber next) = VirtualFileSystem.nextInode vfs

        let candidates =
            [
                yield! VirtualFileSystem.inodes vfs |> Map.toSeq |> Seq.map fst
                yield InodeNumber next
                yield InodeNumber (next + 1000L)
            ]

        for inode in candidates do
            let stored = VirtualFileSystem.bindingCount inode vfs
            let scanned = scannedBindingCount inode vfs

            if stored <> scanned then
                failwith $"%s{context}: bindingCount of inode %O{inode} is %d{stored}, but %d{scanned} entries name it"

            let orphaned = VirtualFileSystem.isOrphanedDirectory inode vfs
            let scannedOrphaned = scannedIsOrphanedDirectory inode vfs

            if orphaned <> scannedOrphaned then
                failwith
                    $"%s{context}: isOrphanedDirectory of inode %O{inode} is %b{orphaned}, but over the scan it is %b{scannedOrphaned}"

            let subdirectories = VirtualFileSystem.subdirectoryCount inode vfs
            let scannedSubdirectories = scannedSubdirectoryCount inode vfs

            if subdirectories <> scannedSubdirectories then
                failwith
                    $"%s{context}: subdirectoryCount of inode %O{inode} is %d{subdirectories}, but %d{scannedSubdirectories} of its entries name a directory"

            let entries = VirtualFileSystem.entryCount inode vfs
            let scannedEntries = scannedEntryCount inode vfs

            if entries <> scannedEntries then
                failwith
                    $"%s{context}: entryCount of inode %O{inode} is %d{entries}, but it binds %d{scannedEntries} names"

        assertStatAgrees context vfs

        match VirtualFileSystem.checkInvariants pinned vfs with
        | [] -> ()
        | defects -> failwith $"%s{context}: the filesystem is unsound: %A{defects}"

    // ------------------------------------------------------------- the seed

    /// A seed of bounded depth: a directory's children are drawn from a small
    /// alphabet so that realisation creates siblings, nested directories and
    /// symlinks, but never a hard link, which a seed cannot express.
    let rec private seedGen (depth : int) : Gen<Map<DirectoryEntryName, SeedEntry>> =
        let entryGen : Gen<SeedEntry> =
            if depth <= 0 then
                Gen.elements
                    [
                        SeedEntry.file ImmutableArray<byte>.Empty
                        SeedEntry.Symlink (target "a", None)
                    ]
            else
                Gen.frequency
                    [
                        2, Gen.constant (SeedEntry.file ImmutableArray<byte>.Empty)
                        1, Gen.constant (SeedEntry.Symlink (target "../a", None))
                        2, seedGen (depth - 1) |> Gen.map SeedEntry.directory
                    ]

        Gen.choose (0, 3)
        |> Gen.bind (fun count -> Gen.listOfLength count (Gen.zip (Gen.elements [ "a" ; "b" ; "c" ; "d" ]) entryGen))
        |> Gen.map (fun pairs -> pairs |> List.map (fun (n, e) -> name n, e) |> Map.ofList)

    // ------------------------------------------------------- the operations

    /// One operation on the graph. Every index is resolved modulo the list it
    /// indexes at the moment the operation runs, so that every generated
    /// operation names something that exists.
    type private Op =
        | CreateFile of directory : int * name : string
        | MakeDirectory of directory : int * name : string
        | MakeSymlink of directory : int * name : string
        | Link of directory : int * name : string * file : int
        /// `unlink(2)`: a binding to a non-directory loses its name.
        | Unlink of binding : int
        /// `rmdir(2)`: a binding to an empty directory loses its name, with
        /// either of the effects the two flavours have on the removed inode.
        | RemoveDirectory of binding : int * darwin : bool
        /// `rename(2)`, possibly over a name that is already bound.
        | Rename of binding : int * directory : int * name : string
        /// A descriptor opened onto an inode, which keeps it alive after its
        /// last name has gone. `directory` draws only from the directories,
        /// because a held directory is what a freeing cascade starts from, and
        /// directories are too few among all inodes to be drawn often.
        | Hold of inode : int * directory : bool
        /// The last descriptor onto an inode closed.
        | Release of held : int
        /// `chmod(2)`, which rewrites the inode but names nothing.
        | Chmod of inode : int

    /// Which operations did something, rather than being skipped because the
    /// graph offered them nothing legal to do. Summed across a whole check, so
    /// that a generator which never reaches an operation fails the test rather
    /// than passing it vacuously.
    [<RequireQualifiedAccess>]
    type private Reached =
        | Create
        | MakeDirectory
        | Symlink
        | Link
        | Unlink
        | RemoveDirectory
        | RenameWithinDirectory
        | RenameAcrossDirectories
        | RenameOverExisting
        | RenameDirectory
        /// A rename displaced a directory, which leaves its parent with one
        /// subdirectory fewer.
        | RenameOverDirectory
        /// A directory had lost its last name while something held it, so
        /// `stat` reported an orphan.
        | OrphanedDirectory
        | Forget
        | ForgetCascade
        | Chmod

    let private opGen : Gen<Op> =
        let names = Gen.elements [ "a" ; "b" ; "c" ; "d" ; "e" ]
        // Mostly small, and `pick` counts from the end of lists that are in
        // inode order, so most operations land on what was made most recently.
        // That is what strings together the histories a uniform choice almost
        // never produces: a directory made inside the one just made, held,
        // removed, and its parent removed after it.
        let index = Gen.frequency [ 4, Gen.choose (0, 2) ; 1, Gen.choose (0, 50) ]

        Gen.frequency
            [
                2, Gen.map2 (fun d n -> Op.CreateFile (d, n)) index names
                4, Gen.map2 (fun d n -> Op.MakeDirectory (d, n)) index names
                1, Gen.map2 (fun d n -> Op.MakeSymlink (d, n)) index names
                2, Gen.map3 (fun d n f -> Op.Link (d, n, f)) index names index
                2, Gen.map Op.Unlink index
                5, Gen.map2 (fun b d -> Op.RemoveDirectory (b, d)) index (Gen.elements [ false ; true ])
                4, Gen.map3 (fun b d n -> Op.Rename (b, d, n)) index index names
                4, Gen.map2 (fun i d -> Op.Hold (i, d)) index (Gen.elements [ false ; true ; true ])
                3, Gen.map Op.Release index
                1, Gen.map Op.Chmod index
            ]

    let private pick (xs : 'a list) (i : int) : 'a option =
        match xs with
        | [] -> None
        | _ -> Some xs.[List.length xs - 1 - i % List.length xs]

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

    /// Directories a name may be added to: those the root still reaches. An
    /// orphaned directory refuses every creation, which the syscalls answer
    /// before they reach the graph.
    let private livingDirectories (vfs : VirtualFileSystem) : InodeNumber list =
        directoriesOf vfs
        |> List.map fst
        |> List.filter (fun inode -> not (scannedIsOrphanedDirectory inode vfs))

    let private bindingsOf (vfs : VirtualFileSystem) : (InodeNumber * DirectoryEntryName * InodeNumber) list =
        directoriesOf vfs
        |> List.collect (fun (directory, content) ->
            content.Entries
            |> Map.toList
            |> List.map (fun (name, target) -> directory, name, target)
        )

    /// Whether `candidate` is `ancestor` or lies beneath it, by climbing parents.
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

    /// What a kernel does once a reference to `inode` has gone: free it if it
    /// has no name and nothing holds it, and if it was a directory, do the same
    /// to the parent its ".." held. Decided by the scan rather than by the
    /// count under test, so that a wrong count cannot steer the history.
    let private reap
        (record : Reached -> unit)
        (pinned : Set<InodeNumber>)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        let rec go (cascaded : bool) (inode : InodeNumber) (vfs : VirtualFileSystem) : VirtualFileSystem =
            if inode = VirtualFileSystem.root vfs then
                vfs
            elif Set.contains inode (withAncestors pinned vfs) then
                vfs
            elif scannedBindingCount inode vfs <> 0 then
                vfs
            else

            match VirtualFileSystem.tryGetContent inode vfs with
            | None -> vfs
            | Some content ->

            record (if cascaded then Reached.ForgetCascade else Reached.Forget)
            let vfs = VirtualFileSystem.forget inode vfs

            match content with
            | InodeContent.Directory directory -> go true directory.Parent vfs
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> vfs

        go false inode vfs

    let private apply
        (record : Reached -> unit)
        (now : UnixTimestamp)
        (op : Op)
        (vfs : VirtualFileSystem, pinned : Set<InodeNumber>)
        : VirtualFileSystem * Set<InodeNumber>
        =
        let living = livingDirectories vfs

        match op with
        | Op.CreateFile (d, n) ->
            match pick living d with
            | None -> vfs, pinned
            | Some directory ->
                match
                    VirtualFileSystem.createFile
                        directory
                        (name n)
                        filePerms
                        Owners.linuxDefault
                        now
                        ImmutableArray<byte>.Empty
                        vfs
                with
                | Ok (_, vfs) ->
                    record Reached.Create
                    vfs, pinned
                | Error _ -> vfs, pinned
        | Op.MakeDirectory (d, n) ->
            match pick living d with
            | None -> vfs, pinned
            | Some directory ->
                match VirtualFileSystem.createDirectory directory (name n) dirPerms Owners.linuxDefault now vfs with
                | Ok (_, vfs) ->
                    record Reached.MakeDirectory
                    vfs, pinned
                | Error _ -> vfs, pinned
        | Op.MakeSymlink (d, n) ->
            match pick living d with
            | None -> vfs, pinned
            | Some directory ->
                match
                    VirtualFileSystem.createSymlink
                        directory
                        (name n)
                        SymlinkModes.linux
                        Owners.linuxDefault
                        now
                        (target "a")
                        vfs
                with
                | Ok (_, vfs) ->
                    record Reached.Symlink
                    vfs, pinned
                | Error _ -> vfs, pinned
        | Op.Link (d, n, f) ->
            // Only a file that still has a name: `link(2)` of a descriptor's
            // nameless inode is ENOENT on both flavours.
            let named =
                bindingsOf vfs
                |> List.choose (fun (_, _, target) ->
                    match VirtualFileSystem.tryGetContent target vfs with
                    | Some (InodeContent.Directory _) -> None
                    | Some _ -> Some target
                    | None -> None
                )
                |> List.distinct

            match pick living d, pick named f with
            | Some directory, Some file ->
                match VirtualFileSystem.hardLink directory (name n) file now vfs with
                | Ok vfs ->
                    record Reached.Link
                    vfs, pinned
                | Error _ -> vfs, pinned
            | _ -> vfs, pinned
        | Op.Unlink b ->
            let candidates =
                bindingsOf vfs
                |> List.filter (fun (_, _, target) ->
                    match VirtualFileSystem.tryGetContent target vfs with
                    | Some (InodeContent.Directory _) -> false
                    | Some _
                    | None -> true
                )

            match pick candidates b with
            | None -> vfs, pinned
            | Some (directory, entry, _) ->
                let removed, vfs =
                    VirtualFileSystem.unbind UnbindTargetEffect.LostALink directory entry now vfs
                    |> ok "unlink"

                record Reached.Unlink
                reap record pinned removed vfs, pinned
        | Op.RemoveDirectory (b, darwin) ->
            let candidates =
                bindingsOf vfs
                |> List.filter (fun (_, _, target) ->
                    match VirtualFileSystem.tryGetContent target vfs with
                    | Some (InodeContent.Directory content) -> Map.isEmpty content.Entries
                    | Some _
                    | None -> false
                )

            match pick candidates b with
            | None -> vfs, pinned
            | Some (directory, entry, _) ->
                let effect =
                    if darwin then
                        UnbindTargetEffect.Untouched
                    else
                        UnbindTargetEffect.LostALink

                let removed, vfs =
                    VirtualFileSystem.unbind effect directory entry now vfs |> ok "rmdir"

                record Reached.RemoveDirectory
                reap record pinned removed vfs, pinned
        | Op.Rename (b, d, n) ->
            match pick (bindingsOf vfs) b, pick living d with
            | Some (sourceDirectory, sourceName, moved), Some destinationDirectory ->
                let destinationName = name n

                let displaced =
                    VirtualFileSystem.tryGetContent destinationDirectory vfs
                    |> Option.bind (fun content ->
                        match content with
                        | InodeContent.Directory content -> Map.tryFind destinationName content.Entries
                        | InodeContent.RegularFile _
                        | InodeContent.CharacterDevice _
                        | InodeContent.Symlink _ -> None
                    )

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

                // The four conditions the verdict answers before the graph is
                // touched, and which `VirtualFileSystem.rename` refuses loudly.
                if
                    displaced = Some moved
                    || (movedIsDirectory && isWithin moved destinationDirectory vfs)
                    || displacedIsPopulated
                then
                    vfs, pinned
                else

                let outcome, vfs =
                    VirtualFileSystem.rename sourceDirectory sourceName destinationDirectory destinationName now vfs
                    |> ok "rename"

                if sourceDirectory = destinationDirectory then
                    record Reached.RenameWithinDirectory
                else
                    record Reached.RenameAcrossDirectories

                if movedIsDirectory then
                    record Reached.RenameDirectory

                match outcome.Displaced with
                | None -> vfs, pinned
                | Some displaced ->
                    record Reached.RenameOverExisting

                    if isDirectory displaced vfs then
                        record Reached.RenameOverDirectory

                    reap record pinned displaced vfs, pinned
            | _ -> vfs, pinned
        | Op.Hold (i, directory) ->
            let candidates =
                if directory then
                    directoriesOf vfs |> List.map fst
                else
                    VirtualFileSystem.inodes vfs |> Map.toList |> List.map fst

            match pick candidates i with
            | None -> vfs, pinned
            | Some inode -> vfs, Set.add inode pinned
        | Op.Chmod i ->
            let candidates =
                VirtualFileSystem.inodes vfs
                |> Map.toList
                |> List.choose (fun (inode, node) ->
                    match node.Content with
                    | InodeContent.Symlink _ -> None
                    | InodeContent.RegularFile _
                    | InodeContent.CharacterDevice _
                    | InodeContent.Directory _ -> Some inode
                )

            match pick candidates i with
            | None -> vfs, pinned
            | Some inode ->
                record Reached.Chmod
                VirtualFileSystem.setPermissions inode (PermissionBits.parseOrFail "test" 0o700) now vfs, pinned
        | Op.Release i ->
            match pick (Set.toList pinned) i with
            | None -> vfs, pinned
            | Some inode ->
                let pinned = Set.remove inode pinned
                reap record pinned inode vfs, pinned

    /// Realise `seed`, run `ops` over it, and check the count after every step.
    let private runHistory
        (record : Reached -> unit)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        (ops : Op list)
        : unit
        =
        let vfs =
            VirtualFileSystem.ofFileSystemSeed buildTime Owners.linuxDefault SymlinkModes.linux seed

        assertAgrees "after realising the seed" Set.empty vfs

        ops
        |> List.indexed
        |> List.fold
            (fun state (index, op) ->
                let vfs, pinned = apply record (tick (index + 1)) op state
                assertAgrees $"after step %d{index}, %A{op}" (withAncestors pinned vfs) vfs

                if
                    VirtualFileSystem.inodes vfs
                    |> Map.exists (fun inode _ -> scannedIsOrphanedDirectory inode vfs)
                then
                    record Reached.OrphanedDirectory

                vfs, pinned
            )
            (vfs, Set.empty)
        |> ignore<VirtualFileSystem * Set<InodeNumber>>

    [<Test>]
    let ``a freeing cascade is a history the operations can express`` () : unit =
        // Indices count from the end of each list, so 0 is the newest.
        let reached = System.Collections.Generic.List<Reached> ()

        runHistory
            reached.Add
            Map.empty
            [
                Op.MakeDirectory (0, "p")
                Op.MakeDirectory (0, "c")
                Op.Hold (0, true)
                Op.RemoveDirectory (0, false)
                Op.RemoveDirectory (0, false)
                Op.Release 0
            ]

        List.ofSeq reached
        |> shouldEqual
            [
                Reached.MakeDirectory
                Reached.MakeDirectory
                Reached.RemoveDirectory
                Reached.OrphanedDirectory
                Reached.RemoveDirectory
                Reached.OrphanedDirectory
                Reached.Forget
                Reached.ForgetCascade
            ]

    [<Test>]
    let ``the stored counts are the scans, and stat's link count is the measured rule, through any history`` () : unit =
        let property (record : Reached -> unit) (seed : Map<DirectoryEntryName, SeedEntry>, ops : Op list) : unit =
            runHistory record seed ops

        let gen = Gen.zip (seedGen 2) (Gen.listOf opGen)

        let reached =
            CoverageSample.check (CoverageSample.inParallel config) (Arb.fromGen gen) property

        // Every operation the property claims to cover must actually have run,
        // or a green result says nothing about it.
        let unreached =
            [
                Reached.Create
                Reached.MakeDirectory
                Reached.Symlink
                Reached.Link
                Reached.Unlink
                Reached.RemoveDirectory
                Reached.RenameWithinDirectory
                Reached.RenameAcrossDirectories
                Reached.RenameOverExisting
                Reached.RenameDirectory
                Reached.RenameOverDirectory
                Reached.OrphanedDirectory
                Reached.Forget
                Reached.ForgetCascade
                Reached.Chmod
            ]
            |> List.filter (fun r -> reached.Count r = 0)

        if not (List.isEmpty unreached) then
            failwith $"these were never reached: %A{unreached}; reached: %A{reached.Reached}"

    // ------------------------------------------------------ the invariant

    [<Test>]
    let ``checkInvariants reports a stored binding count that disagrees with the entries`` () : unit =
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let file, vfs =
            VirtualFileSystem.createFile
                root
                (name "f")
                filePerms
                Owners.linuxDefault
                buildTime
                ImmutableArray.Empty
                vfs
            |> ok "create"

        let vfs = VirtualFileSystem.hardLink root (name "g") file buildTime vfs |> ok "link"

        VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual []

        let forged (inode : InodeNumber) (count : int option) : VirtualFileSystemDefect list =
            VirtualFileSystem.checkInvariants Set.empty (VirtualFileSystem.Unchecked.setBindingCount inode count vfs)

        // Too few, and too many.
        forged file (Some 1)
        |> shouldEqual [ VirtualFileSystemDefect.BindingCountMismatch (file, Some 1, 2) ]

        forged file (Some 3)
        |> shouldEqual [ VirtualFileSystemDefect.BindingCountMismatch (file, Some 3, 2) ]

        // Nothing stored, which the count reads as zero.
        forged file None
        |> shouldEqual [ VirtualFileSystemDefect.BindingCountMismatch (file, None, 2) ]

        // A count for an inode no entry names: the root, which nothing may.
        forged root (Some 1)
        |> shouldEqual [ VirtualFileSystemDefect.BindingCountMismatch (root, Some 1, 0) ]

        // A stored zero agrees in value, but the count holds only non-zero
        // entries, so that two filesystems with the same graph are equal.
        forged root (Some 0)
        |> shouldEqual [ VirtualFileSystemDefect.BindingCountMismatch (root, Some 0, 0) ]

    [<Test>]
    let ``checkInvariants reports a stored subdirectory count that disagrees with the entries`` () : unit =
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let directory, vfs =
            VirtualFileSystem.createDirectory root (name "d") dirPerms Owners.linuxDefault buildTime vfs
            |> ok "mkdir d"

        let _, vfs =
            VirtualFileSystem.createDirectory directory (name "e") dirPerms Owners.linuxDefault buildTime vfs
            |> ok "mkdir d/e"

        let _, vfs =
            VirtualFileSystem.createFile
                directory
                (name "f")
                filePerms
                Owners.linuxDefault
                buildTime
                ImmutableArray.Empty
                vfs
            |> ok "create d/f"

        VirtualFileSystem.checkInvariants Set.empty vfs |> shouldEqual []
        VirtualFileSystem.subdirectoryCount root vfs |> shouldEqual 1
        VirtualFileSystem.subdirectoryCount directory vfs |> shouldEqual 1

        let forged (inode : InodeNumber) (count : int option) : VirtualFileSystemDefect list =
            VirtualFileSystem.checkInvariants
                Set.empty
                (VirtualFileSystem.Unchecked.setSubdirectoryCount inode count vfs)

        // Counting the file as well, and counting nothing.
        forged directory (Some 2)
        |> shouldEqual [ VirtualFileSystemDefect.SubdirectoryCountMismatch (directory, Some 2, 1) ]

        forged directory None
        |> shouldEqual [ VirtualFileSystemDefect.SubdirectoryCountMismatch (directory, None, 1) ]

        // A count for a directory with no subdirectory.
        let empty =
            match VirtualFileSystem.tryGetContent directory vfs with
            | Some (InodeContent.Directory content) -> content.Entries.[name "e"]
            | other -> failwith $"test bug: %A{other}"

        forged empty (Some 1)
        |> shouldEqual [ VirtualFileSystemDefect.SubdirectoryCountMismatch (empty, Some 1, 0) ]

        // A stored zero agrees in value, but only non-zero counts are stored.
        forged empty (Some 0)
        |> shouldEqual [ VirtualFileSystemDefect.SubdirectoryCountMismatch (empty, Some 0, 0) ]

    [<Test>]
    let ``Unchecked.ofParts stores the counts its entries imply`` () : unit =
        // A test that forges a graph to exercise some other defect must not
        // also trip over the count, or every such test would report two.
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let file, vfs =
            VirtualFileSystem.createFile
                root
                (name "f")
                filePerms
                Owners.linuxDefault
                buildTime
                ImmutableArray.Empty
                vfs
            |> ok "create"

        let vfs = VirtualFileSystem.hardLink root (name "g") file buildTime vfs |> ok "link"

        let forged =
            VirtualFileSystem.Unchecked.ofParts
                (VirtualFileSystem.inodes vfs)
                (VirtualFileSystem.root vfs)
                (VirtualFileSystem.nextInode vfs)

        VirtualFileSystem.bindingCount file forged |> shouldEqual 2
        forged |> shouldEqual vfs

        let _, withDirectory =
            VirtualFileSystem.createDirectory root (name "d") dirPerms Owners.linuxDefault buildTime vfs
            |> ok "mkdir"

        let forged =
            VirtualFileSystem.Unchecked.ofParts
                (VirtualFileSystem.inodes withDirectory)
                (VirtualFileSystem.root withDirectory)
                (VirtualFileSystem.nextInode withDirectory)

        VirtualFileSystem.subdirectoryCount root forged |> shouldEqual 1
        forged |> shouldEqual withDirectory

    [<Test>]
    let ``forget refuses a directory that still holds entries`` () : unit =
        // Its entries would otherwise go on counting towards their targets'
        // binding counts after the directory holding them had gone.
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let directory, vfs =
            VirtualFileSystem.createDirectory root (name "d") dirPerms Owners.linuxDefault buildTime vfs
            |> ok "mkdir"

        let _, vfs =
            VirtualFileSystem.createFile
                directory
                (name "f")
                filePerms
                Owners.linuxDefault
                buildTime
                ImmutableArray.Empty
                vfs
            |> ok "create"

        let _, vfs =
            VirtualFileSystem.unbind UnbindTargetEffect.LostALink root (name "d") buildTime vfs
            |> ok "unbind"

        let thrown =
            Assert.Throws<exn> (fun () -> VirtualFileSystem.forget directory vfs |> ignore<VirtualFileSystem>)

        thrown.Message |> shouldContainText "still holds"
