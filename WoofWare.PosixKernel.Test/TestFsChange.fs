namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The changes a `VirtualFileSystem` mutator reports beside its graph, held to
/// a diff of the graphs before and after it, and the commit every syscall's
/// change goes through.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFsChange =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private bits (mode : int) : PermissionBits = PermissionBits.parseOrFail "test" mode

    let private bytes (length : int) : ImmutableArray<byte> =
        ImmutableArray.CreateRange (Seq.init length byte)

    let private ok<'a, 'error> (result : Result<'a, 'error>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected success, got %A{error}"

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    /// A moment after every stamp a test's graph holds before its `index`th
    /// step, so that every timestamp a step moves is visible to the diff.
    let private tickOf (index : int) : UnixTimestamp =
        UnixTimestamp.createOrFail "test" (UnixTimestamp.seconds buildTime + int64 index) 0

    let private rootOwner : InodeOwner = InodeOwner.ofProcess Owners.root

    // ------------------------------------------------------------- the oracle

    /// What a diff of two graphs can tell about the change between them.
    [<RequireQualifiedAccess>]
    module private FsChangeDiff =

        type private Binding = InodeNumber * DirectoryEntryName * InodeNumber

        let private bindings (vfs : VirtualFileSystem) : Set<Binding> =
            VirtualFileSystem.inodes vfs
            |> Map.toSeq
            |> Seq.collect (fun (directory, entry) ->
                match entry.Content with
                | InodeContent.Directory content ->
                    content.Entries |> Map.toSeq |> Seq.map (fun (n, inode) -> directory, n, inode)
                | InodeContent.RegularFile _
                | InodeContent.CharacterDevice _
                | InodeContent.Socket _
                | InodeContent.Fifo _
                | InodeContent.Symlink _ -> Seq.empty
            )
            |> Set.ofSeq

        let private bytesOf (inode : Inode) : byte[] option =
            match inode.Content with
            | InodeContent.RegularFile (contents, _) -> Some (Seq.toArray contents)
            | InodeContent.Directory _
            | InodeContent.CharacterDevice _
            | InodeContent.Socket _
            | InodeContent.Fifo _
            | InodeContent.Symlink _ -> None

        let private modeOf (inode : Inode) : PermissionBits =
            match inode.Content with
            | InodeContent.RegularFile (_, permissions)
            | InodeContent.CharacterDevice (_, permissions)
            | InodeContent.Socket permissions
            | InodeContent.Fifo permissions
            | InodeContent.Symlink (_, permissions) -> permissions
            | InodeContent.Directory content -> content.Permissions

        let private isDirectory (inode : Inode) : bool =
            match inode.Content with
            | InodeContent.Directory _ -> true
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Socket _
            | InodeContent.Fifo _
            | InodeContent.Symlink _ -> false

        /// Everything about an inode a diff compares, with its contents as an
        /// array because `ImmutableArray` compares by reference.
        let private projection (vfs : VirtualFileSystem) (inode : InodeNumber) (entry : Inode) =
            let content =
                match entry.Content with
                | InodeContent.RegularFile (contents, permissions) ->
                    sprintf "file %A %O" (Seq.toArray contents) permissions
                | other -> sprintf "%A" other

            content, entry.Owner, entry.Times, VirtualFileSystem.bindingCount inode vfs

        let private inBoth
            (before : VirtualFileSystem)
            (after : VirtualFileSystem)
            : (InodeNumber * Inode * Inode) list
            =
            VirtualFileSystem.inodes before
            |> Map.toList
            |> List.choose (fun (inode, was) ->
                VirtualFileSystem.tryGet inode after |> Option.map (fun now -> inode, was, now)
            )

        /// The changes a diff of `before` and `after` can name for certain: a
        /// binding gone or new, an inode gone, a file's bytes different, and an
        /// inode's mode or owner different, or, for an inode that is no
        /// directory, how many names it has.
        ///
        /// It cannot pair a name gone with a name new as one rename, cannot
        /// order the changes, cannot tell a truncation to the same length from
        /// a change of timestamps alone, and sees nothing of a change that left
        /// the graph as it was.
        let ofDiff (before : VirtualFileSystem) (after : VirtualFileSystem) : FsChange list =
            let was = bindings before
            let now = bindings after

            let removed =
                Set.difference was now
                |> Set.toList
                |> List.map (fun (d, n, i) -> FsChange.EntryRemoved (d, n, i))

            let added =
                Set.difference now was
                |> Set.toList
                |> List.map (fun (d, n, i) -> FsChange.EntryAdded (d, n, i))

            let forgotten =
                VirtualFileSystem.inodes before
                |> Map.toList
                |> List.filter (fun (inode, _) -> (VirtualFileSystem.tryGet inode after).IsNone)
                |> List.map (fun (inode, _) -> FsChange.InodeForgotten inode)

            let content =
                inBoth before after
                |> List.filter (fun (_, was, now) -> bytesOf was <> bytesOf now)
                |> List.map (fun (inode, _, _) -> FsChange.ContentChanged inode)

            let attributes =
                inBoth before after
                |> List.filter (fun (inode, was, now) ->
                    modeOf was <> modeOf now
                    || was.Owner <> now.Owner
                    || (not (isDirectory now)
                        && VirtualFileSystem.bindingCount inode before
                           <> VirtualFileSystem.bindingCount inode after)
                )
                |> List.map (fun (inode, _, _) -> FsChange.AttributesChanged inode)

            removed @ added @ forgotten @ content @ attributes

        /// The inodes in both graphs that differ in anything at all, timestamps
        /// and link counts included.
        let touched (before : VirtualFileSystem) (after : VirtualFileSystem) : Set<InodeNumber> =
            inBoth before after
            |> List.filter (fun (inode, was, now) -> projection before inode was <> projection after inode now)
            |> List.map (fun (inode, _, _) -> inode)
            |> Set.ofList

        /// `changes` in the terms a diff has: a rename as the name it removed
        /// and the name it added, and the link its displaced inode lost as the
        /// removal of that name and, for an inode that is no directory, the
        /// change to its link count.
        let observable (before : VirtualFileSystem) (changes : FsChange list) : FsChange list =
            changes
            |> List.collect (fun change ->
                match change with
                | FsChange.EntryMoved (source, sourceName, destination, destinationName, inode, displaced) ->
                    [
                        yield FsChange.EntryRemoved (source, sourceName, inode)
                        yield FsChange.EntryAdded (destination, destinationName, inode)
                        match displaced with
                        | None -> ()
                        | Some displaced ->
                            yield FsChange.EntryRemoved (destination, destinationName, displaced)

                            match VirtualFileSystem.tryGet displaced before with
                            | Some entry when not (isDirectory entry) -> yield FsChange.AttributesChanged displaced
                            | Some _
                            | None -> ()
                    ]
                | FsChange.EntryAdded _
                | FsChange.EntryRemoved _
                | FsChange.ContentChanged _
                | FsChange.AttributesChanged _
                | FsChange.InodeForgotten _ -> [ change ]
            )

        /// Every inode `change` names.
        let names (change : FsChange) : InodeNumber list =
            match change with
            | FsChange.EntryAdded (directory, _, inode)
            | FsChange.EntryRemoved (directory, _, inode) -> [ directory ; inode ]
            | FsChange.EntryMoved (source, _, destination, _, inode, displaced) ->
                [ source ; destination ; inode ] @ Option.toList displaced
            | FsChange.ContentChanged inode
            | FsChange.AttributesChanged inode
            | FsChange.InodeForgotten inode -> [ inode ]

        /// The ways `mutation`'s reported changes disagree with a diff of its
        /// graphs, where the diff can tell; empty when they agree.
        let disagreements (mutation : VfsMutation) : string list =
            let before = VfsMutation.before mutation
            let after = VfsMutation.after mutation
            let reported = VfsMutation.changes mutation
            let observed = observable before reported
            let diffed = ofDiff before after
            let touched = touched before after

            let isEntry (change : FsChange) : bool =
                match change with
                | FsChange.EntryAdded _
                | FsChange.EntryRemoved _
                | FsChange.InodeForgotten _ -> true
                | FsChange.EntryMoved _
                | FsChange.ContentChanged _
                | FsChange.AttributesChanged _ -> false

            let sorted (changes : FsChange list) : FsChange list = List.sortBy (sprintf "%A") changes

            [
                // Names and forgotten inodes: exactly, each once.
                let reportedEntries = observed |> List.filter isEntry |> sorted
                let diffedEntries = diffed |> List.filter isEntry |> sorted

                if reportedEntries <> diffedEntries then
                    yield $"names and forgotten inodes: reported %A{reportedEntries}, diff %A{diffedEntries}"

                // What the diff names for certain about an inode in both graphs
                // is reported.
                for change in diffed do
                    if not (isEntry change) && not (List.contains change observed) then
                        yield $"the diff shows %A{change}, which was not reported"

                // A reported content or attribute change shows in the diff.
                for change in reported do
                    match change with
                    | FsChange.ContentChanged inode
                    | FsChange.AttributesChanged inode when not (Set.contains inode touched) ->
                        yield $"reported %A{change}, but the diff shows no change to inode %O{inode}"
                    | FsChange.ContentChanged inode ->
                        match bytesOf (VirtualFileSystem.tryGet inode after).Value with
                        | Some _ -> ()
                        | None -> yield $"reported %A{change} of an inode that is no regular file"
                    | _ -> ()

                // Whatever changed about an inode is accounted for by a change
                // naming it.
                let named = reported |> List.collect names |> Set.ofList

                for inode in touched do
                    if not (Set.contains inode named) then
                        yield $"inode %O{inode} changed, and no reported change names it"

                // An inode new to the graph came with a name.
                for inode, _ in VirtualFileSystem.inodes after |> Map.toList do
                    if (VirtualFileSystem.tryGet inode before).IsNone then
                        let introduced =
                            reported
                            |> List.exists (fun change ->
                                match change with
                                | FsChange.EntryAdded (_, _, added) -> added = inode
                                | _ -> false
                            )

                        if not introduced then
                            yield $"inode %O{inode} is new, and no reported EntryAdded binds it"
            ]

    // ---------------------------------------------------------------- the ops

    /// One mutator call, its arguments chosen by index among what the graph
    /// holds when it runs, so that a generated sequence is mostly calls that
    /// do something.
    type private Op =
        | MakeDirectory of parent : int * name : string
        | MakeFile of parent : int * name : string * length : int
        | MakeSymlink of parent : int * name : string
        | Clone of source : int * parent : int * name : string
        | Link of target : int * parent : int * name : string
        | Unbind of binding : int * untouched : bool
        | Rename of binding : int * parent : int * name : string
        | Write of file : int * offset : int * length : int * privileged : bool
        | Truncate of file : int * length : int * privileged : bool
        | Chmod of inode : int * mode : int
        | Chown of inode : int * toRoot : bool
        | SetTimes of inode : int
        | Forget of candidate : int

    /// What a call reached, for the coverage floors.
    [<RequireQualifiedAccess>]
    type private Reached =
        | Created
        | Cloned
        | Linked
        | UnboundFile
        | UnboundDirectory
        | UnboundUntouched
        | RenamedToFreeName
        | RenamedOverAnother
        | RenamedDirectoryAcross
        | Wrote
        | WroteStrippingSetId
        | Truncated
        | TruncatedStrippingSetId
        | Chmodded
        | Chowned
        | TimesSet
        | Forgot
        | ForgotDirectory

    let private opGen : Gen<Op> =
        let index = Gen.choose (0, 12)
        let entryName = Gen.elements [ "a" ; "b" ; "c" ; "d" ]

        Gen.frequency
            [
                3, Gen.map2 (fun p n -> Op.MakeDirectory (p, n)) index entryName
                3, Gen.map3 (fun p n l -> Op.MakeFile (p, n, l)) index entryName (Gen.choose (0, 4))
                1, Gen.map2 (fun p n -> Op.MakeSymlink (p, n)) index entryName
                1, Gen.map3 (fun s p n -> Op.Clone (s, p, n)) index index entryName
                2, Gen.map3 (fun t p n -> Op.Link (t, p, n)) index index entryName
                3, Gen.map2 (fun b u -> Op.Unbind (b, u)) index (Gen.elements [ false ; false ; true ])
                4, Gen.map3 (fun b p n -> Op.Rename (b, p, n)) index index entryName
                3,
                gen {
                    let! file = index
                    let! offset = Gen.choose (0, 6)
                    let! length = Gen.choose (1, 4)
                    let! privileged = Gen.elements [ false ; true ]
                    return Op.Write (file, offset, length, privileged)
                }
                2,
                Gen.map3 (fun f l p -> Op.Truncate (f, l, p)) index (Gen.choose (0, 6)) (Gen.elements [ false ; true ])
                2,
                Gen.map2
                    (fun i m -> Op.Chmod (i, m))
                    index
                    (Gen.elements [ 0o644 ; 0o755 ; 0o4755 ; 0o2755 ; 0o6755 ; 0o4644 ])
                1, Gen.map2 (fun i r -> Op.Chown (i, r)) index (Gen.elements [ false ; true ])
                1, Gen.map Op.SetTimes index
                2, Gen.map Op.Forget index
            ]

    /// Begins every sequence, so that every case has files and directories to
    /// work on from its first generated call.
    let private opening : Op list =
        [
            Op.MakeDirectory (0, "d")
            Op.MakeDirectory (0, "e")
            Op.MakeFile (0, "f", 3)
            Op.MakeFile (1, "g", 2)
            Op.Chmod (3, 0o4755)
        ]

    let private opsGen : Gen<Op list> =
        Gen.listOf opGen |> Gen.map (fun ops -> opening @ ops)

    let private pick<'a> (xs : 'a list) (i : int) : 'a option =
        if List.isEmpty xs then
            None
        else
            Some xs.[i % List.length xs]

    let private directories (vfs : VirtualFileSystem) : InodeNumber list =
        VirtualFileSystem.inodes vfs
        |> Map.toList
        |> List.choose (fun (inode, entry) ->
            match entry.Content with
            | InodeContent.Directory _ -> Some inode
            | _ -> None
        )

    let private regularFiles (vfs : VirtualFileSystem) : InodeNumber list =
        VirtualFileSystem.inodes vfs
        |> Map.toList
        |> List.choose (fun (inode, entry) ->
            match entry.Content with
            | InodeContent.RegularFile _ -> Some inode
            | _ -> None
        )

    let private bindingsOf (vfs : VirtualFileSystem) : (InodeNumber * DirectoryEntryName * InodeNumber) list =
        VirtualFileSystem.inodes vfs
        |> Map.toList
        |> List.collect (fun (directory, entry) ->
            match entry.Content with
            | InodeContent.Directory content ->
                content.Entries
                |> Map.toList
                |> List.map (fun (n, inode) -> directory, n, inode)
            | _ -> []
        )

    let private isPopulatedDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetDirectory inode vfs with
        | Some content -> not (Map.isEmpty content.Entries)
        | None -> false

    let private hasSetIdBit (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.RegularFile (_, permissions)) ->
            PermissionBits.toInt permissions
            &&& (PermissionBits.setUserId ||| PermissionBits.setGroupId)
            <> 0
        | _ -> false

    /// The mutation `op` makes of `vfs` at `now`, with what it reached; `None`
    /// where the graph offers nothing to apply it to, where the mutator refuses
    /// it, or where it would break a precondition the mutator states rather
    /// than refuses.
    let private apply (now : UnixTimestamp) (op : Op) (vfs : VirtualFileSystem) : (VfsMutation * Reached) option =
        let rule = SetGroupIdOnWrite.StripWhenGroupExecutableOrWriterOutsideGroup

        let caller (privileged : bool) =
            if privileged then
                Owners.root
            else
                Owners.linuxDefaultCaller

        match op with
        | Op.MakeDirectory (p, n) ->
            pick (directories vfs) p
            |> Option.bind (fun parent ->
                VirtualFileSystem.createDirectory parent (name n) (bits 0o755) Owners.linuxDefault now vfs
                |> Result.toOption
                |> Option.map (fun (_, mutation) -> mutation, Reached.Created)
            )
        | Op.MakeFile (p, n, length) ->
            pick (directories vfs) p
            |> Option.bind (fun parent ->
                VirtualFileSystem.createFile parent (name n) (bits 0o644) Owners.linuxDefault now (bytes length) vfs
                |> Result.toOption
                |> Option.map (fun (_, mutation) -> mutation, Reached.Created)
            )
        | Op.MakeSymlink (p, n) ->
            pick (directories vfs) p
            |> Option.bind (fun parent ->
                VirtualFileSystem.createSymlink
                    parent
                    (name n)
                    SymlinkModes.linux
                    Owners.linuxDefault
                    now
                    (SymlinkTarget.parseOrFail "test" "a/b")
                    vfs
                |> Result.toOption
                |> Option.map (fun (_, mutation) -> mutation, Reached.Created)
            )
        | Op.Clone (s, p, n) ->
            match pick (regularFiles vfs) s, pick (directories vfs) p with
            | Some source, Some parent ->
                VirtualFileSystem.cloneFile source parent (name n) (bits 0o644) Owners.linuxDefault now vfs
                |> Result.toOption
                |> Option.map (fun (_, mutation) -> mutation, Reached.Cloned)
            | _ -> None
        | Op.Link (t, p, n) ->
            let targets = VirtualFileSystem.inodes vfs |> Map.toList |> List.map fst

            match pick targets t, pick (directories vfs) p with
            | Some target, Some parent ->
                VirtualFileSystem.hardLink parent (name n) target now vfs
                |> Result.toOption
                |> Option.map (fun mutation -> mutation, Reached.Linked)
            | _ -> None
        | Op.Unbind (b, untouched) ->
            pick (bindingsOf vfs) b
            |> Option.bind (fun (directory, n, inode) ->
                let effect =
                    if untouched then
                        UnbindTargetEffect.Untouched
                    else
                        UnbindTargetEffect.LostALink

                let reached =
                    if untouched then
                        Reached.UnboundUntouched
                    elif (VirtualFileSystem.tryGetDirectory inode vfs).IsSome then
                        Reached.UnboundDirectory
                    else
                        Reached.UnboundFile

                VirtualFileSystem.unbind effect directory n now vfs
                |> Result.toOption
                |> Option.map (fun (_, mutation) -> mutation, reached)
            )
        | Op.Rename (b, p, n) ->
            match pick (bindingsOf vfs) b, pick (directories vfs) p with
            | Some (source, sourceName, moved), Some destination ->
                let destinationName = name n

                let displaced =
                    (VirtualFileSystem.tryGetDirectory destination vfs).Value.Entries
                    |> Map.tryFind destinationName

                let movedIsDirectory = (VirtualFileSystem.tryGetDirectory moved vfs).IsSome

                // The four shapes `rename` states as preconditions rather than
                // refusing: the verdict answers each before calling it.
                if
                    VirtualFileSystem.isOrphanedDirectory destination vfs
                    || displaced = Some moved
                    || (movedIsDirectory && VirtualFileSystem.isWithinSubtree moved destination vfs)
                    || (displaced
                        |> Option.map (fun d -> isPopulatedDirectory d vfs)
                        |> Option.defaultValue false)
                then
                    None
                else

                let reached =
                    match displaced with
                    | Some _ -> Reached.RenamedOverAnother
                    | None when movedIsDirectory && source <> destination -> Reached.RenamedDirectoryAcross
                    | None -> Reached.RenamedToFreeName

                VirtualFileSystem.rename source sourceName destination destinationName now vfs
                |> Result.toOption
                |> Option.map (fun (_, mutation) -> mutation, reached)
            | _ -> None
        | Op.Write (f, offset, length, privileged) ->
            pick (regularFiles vfs) f
            |> Option.bind (fun file ->
                let reached =
                    if hasSetIdBit file vfs && not privileged then
                        Reached.WroteStrippingSetId
                    else
                        Reached.Wrote

                VirtualFileSystem.writeFile file (int64 offset) (bytes length) rule (caller privileged) now vfs
                |> Result.toOption
                |> Option.map (fun mutation -> mutation, reached)
            )
        | Op.Truncate (f, length, privileged) ->
            pick (regularFiles vfs) f
            |> Option.bind (fun file ->
                let reached =
                    if hasSetIdBit file vfs && not privileged then
                        Reached.TruncatedStrippingSetId
                    else
                        Reached.Truncated

                VirtualFileSystem.truncateFile
                    file
                    (int64 length)
                    SetIdBitsOnTruncation.Strip
                    (caller privileged)
                    now
                    vfs
                |> Result.toOption
                |> Option.map (fun mutation -> mutation, reached)
            )
        | Op.Chmod (i, mode) ->
            pick (VirtualFileSystem.inodes vfs |> Map.toList |> List.map fst) i
            |> Option.map (fun inode -> VirtualFileSystem.setPermissions inode (bits mode) now vfs, Reached.Chmodded)
        | Op.Chown (i, toRoot) ->
            pick (VirtualFileSystem.inodes vfs |> Map.toList |> List.map fst) i
            |> Option.map (fun inode ->
                let owner = if toRoot then rootOwner else Owners.linuxDefault
                VirtualFileSystem.setOwner inode owner now vfs, Reached.Chowned
            )
        | Op.SetTimes i ->
            pick (VirtualFileSystem.inodes vfs |> Map.toList) i
            |> Option.map (fun (inode, entry) ->
                let times =
                    {
                        Access = now
                        Modification = now
                        StatusChange = now
                        Birth = entry.Times.Birth
                    }

                VirtualFileSystem.setTimes inode times vfs, Reached.TimesSet
            )
        | Op.Forget c ->
            let forgettable =
                VirtualFileSystem.inodes vfs
                |> Map.toList
                |> List.map fst
                |> List.filter (fun inode ->
                    inode <> VirtualFileSystem.root vfs
                    && VirtualFileSystem.bindingCount inode vfs = 0
                    && not (isPopulatedDirectory inode vfs)
                )

            pick forgettable c
            |> Option.map (fun inode ->
                let reached =
                    if (VirtualFileSystem.tryGetDirectory inode vfs).IsSome then
                        Reached.ForgotDirectory
                    else
                        Reached.Forgot

                VirtualFileSystem.forget inode vfs, reached
            )

    // ------------------------------------------------------------- properties

    [<Test>]
    let ``every mutator's changes agree with a diff of its graphs`` () : unit =
        let property (cover : Reached -> unit) (ops : Op list) : unit =
            ops
            |> List.indexed
            |> List.fold
                (fun vfs (index, op) ->
                    match apply (tickOf (index + 1)) op vfs with
                    | None -> vfs
                    | Some (mutation, reached) ->
                        cover reached

                        match FsChangeDiff.disagreements mutation with
                        | [] -> VfsMutation.after mutation
                        | disagreements ->
                            failwith (
                                $"%A{op}, reporting %A{VfsMutation.changes mutation}:\n"
                                + String.concat "\n" disagreements
                            )
                )
                (VirtualFileSystem.empty buildTime Owners.linuxDefault)
            |> ignore<VirtualFileSystem>

        let coverage = CoverageSample.check config (Arb.fromGen opsGen) property

        for label in
            [
                Reached.Created
                Reached.Cloned
                Reached.Linked
                Reached.UnboundFile
                Reached.UnboundDirectory
                Reached.UnboundUntouched
                Reached.RenamedToFreeName
                Reached.RenamedOverAnother
                Reached.RenamedDirectoryAcross
                Reached.Wrote
                Reached.WroteStrippingSetId
                Reached.Truncated
                Reached.TruncatedStrippingSetId
                Reached.Chmodded
                Reached.Chowned
                Reached.TimesSet
                Reached.Forgot
                Reached.ForgotDirectory
            ] do
            if coverage.Count label < 20 then
                failwith $"the fixed sample reached %A{label} only %d{coverage.Count label} times"

    // ----------------------------------------------- what each mutator reports

    /// The root with a directory `d`, a file `f` and an empty directory `e`,
    /// and `f` hard-linked into `d` as `h`.
    let private fixture : VirtualFileSystem * InodeNumber * InodeNumber * InodeNumber * InodeNumber =
        let vfs = VirtualFileSystem.empty buildTime Owners.linuxDefault
        let root = VirtualFileSystem.root vfs

        let d, vfs =
            GraphMutators.createDirectory root (name "d") (bits 0o755) Owners.linuxDefault buildTime vfs
            |> ok

        let e, vfs =
            GraphMutators.createDirectory root (name "e") (bits 0o755) Owners.linuxDefault buildTime vfs
            |> ok

        let f, vfs =
            GraphMutators.createFile root (name "f") (bits 0o644) Owners.linuxDefault buildTime (bytes 3) vfs
            |> ok

        let vfs = GraphMutators.hardLink d (name "h") f buildTime vfs |> ok
        vfs, d, e, f, root

    let private later : UnixTimestamp = tickOf 1

    [<Test>]
    let ``a creation reports the name it added`` () : unit =
        let vfs, d, _, f, _ = fixture

        let made, mutation =
            VirtualFileSystem.createFile d (name "n") (bits 0o644) Owners.linuxDefault later (bytes 0) vfs
            |> ok

        VfsMutation.changes mutation
        |> shouldEqual [ FsChange.EntryAdded (d, name "n", made) ]

        let made, mutation =
            VirtualFileSystem.createDirectory d (name "n") (bits 0o755) Owners.linuxDefault later vfs
            |> ok

        VfsMutation.changes mutation
        |> shouldEqual [ FsChange.EntryAdded (d, name "n", made) ]

        let made, mutation =
            VirtualFileSystem.createSymlink
                d
                (name "n")
                SymlinkModes.linux
                Owners.linuxDefault
                later
                (SymlinkTarget.parseOrFail "test" "x")
                vfs
            |> ok

        VfsMutation.changes mutation
        |> shouldEqual [ FsChange.EntryAdded (d, name "n", made) ]

        let made, mutation =
            VirtualFileSystem.cloneFile f d (name "n") (bits 0o644) Owners.linuxDefault later vfs
            |> ok

        VfsMutation.changes mutation
        |> shouldEqual [ FsChange.EntryAdded (d, name "n", made) ]

    [<Test>]
    let ``a hard link reports the target's new link and then its name`` () : unit =
        let vfs, _, e, f, _ = fixture

        VirtualFileSystem.hardLink e (name "n") f later vfs
        |> ok
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.AttributesChanged f ; FsChange.EntryAdded (e, name "n", f) ]

    [<Test>]
    let ``unbinding a file reports its lost link, and unbinding a directory only the name`` () : unit =
        let vfs, d, e, f, root = fixture

        for effect in [ UnbindTargetEffect.LostALink ; UnbindTargetEffect.Untouched ] do
            VirtualFileSystem.unbind effect d (name "h") later vfs
            |> ok
            |> snd
            |> VfsMutation.changes
            |> shouldEqual [ FsChange.AttributesChanged f ; FsChange.EntryRemoved (d, name "h", f) ]

            VirtualFileSystem.unbind effect root (name "e") later vfs
            |> ok
            |> snd
            |> VfsMutation.changes
            |> shouldEqual [ FsChange.EntryRemoved (root, name "e", e) ]

    [<Test>]
    let ``a rename reports one move, naming what it displaced`` () : unit =
        let vfs, d, e, f, root = fixture

        VirtualFileSystem.rename root (name "f") root (name "g") later vfs
        |> ok
        |> snd
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.EntryMoved (root, name "f", root, name "g", f, None) ]

        VirtualFileSystem.rename root (name "e") d (name "h") later vfs
        |> ok
        |> snd
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.EntryMoved (root, name "e", d, name "h", e, Some f) ]

    [<Test>]
    let ``a write or truncation reports the content, and the mode only when it strips a set-ID bit`` () : unit =
        let vfs, _, _, f, _ = fixture
        let rule = SetGroupIdOnWrite.StripWhenGroupExecutableOrWriterOutsideGroup
        let setUid = GraphMutators.setPermissions f (bits 0o4755) buildTime vfs

        VirtualFileSystem.writeFile f 0L (bytes 2) rule Owners.linuxDefaultCaller later vfs
        |> ok
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.ContentChanged f ]

        VirtualFileSystem.writeFile f 0L (bytes 2) rule Owners.linuxDefaultCaller later setUid
        |> ok
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.AttributesChanged f ; FsChange.ContentChanged f ]

        // To the length it already has: the bytes end as they were.
        VirtualFileSystem.truncateFile f 3L SetIdBitsOnTruncation.Strip Owners.linuxDefaultCaller later vfs
        |> ok
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.ContentChanged f ]

        VirtualFileSystem.truncateFile f 3L SetIdBitsOnTruncation.Strip Owners.linuxDefaultCaller later setUid
        |> ok
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.AttributesChanged f ; FsChange.ContentChanged f ]

    [<Test>]
    let ``setting an attribute reports it, even to the value it had`` () : unit =
        let vfs, _, _, f, _ = fixture
        let entry = (VirtualFileSystem.tryGet f vfs).Value

        VirtualFileSystem.setPermissions f (bits 0o644) later vfs
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.AttributesChanged f ]

        VirtualFileSystem.setOwner f entry.Owner later vfs
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.AttributesChanged f ]

        VirtualFileSystem.setTimes f entry.Times vfs
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.AttributesChanged f ]

    [<Test>]
    let ``forgetting an inode reports it`` () : unit =
        let vfs, _, e, _, root = fixture

        let _, unbound =
            GraphMutators.unbind UnbindTargetEffect.LostALink root (name "e") later vfs
            |> ok

        VirtualFileSystem.forget e unbound
        |> VfsMutation.changes
        |> shouldEqual [ FsChange.InodeForgotten e ]

    // ---------------------------------------------------------------- commit

    let private booted () : UnixSystem<int, string> =
        Launched.bootWith
            id
            UnixSystem.pipedStandardStreams
            0
            (CpuId 0)
            (UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxX64)

    [<Test>]
    let ``committing a mutation leaves its graph on the machine`` () : unit =
        let machine = (booted ()).Machine
        let root = VirtualFileSystem.root machine.FileSystem

        let _, mutation =
            VirtualFileSystem.createFile
                root
                (name "n")
                (bits 0o644)
                Owners.linuxDefault
                later
                (bytes 1)
                machine.FileSystem
            |> ok

        let committed =
            UnixMachineState.commitFileSystem ChangeNotice.Names mutation machine

        Object.ReferenceEquals (committed.FileSystem, VfsMutation.after mutation)
        |> shouldEqual true

    [<Test>]
    let ``committing a mutation of another graph is refused`` () : unit =
        let machine = (booted ()).Machine
        let root = VirtualFileSystem.root machine.FileSystem

        let _, first =
            VirtualFileSystem.createFile
                root
                (name "n")
                (bits 0o644)
                Owners.linuxDefault
                later
                (bytes 1)
                machine.FileSystem
            |> ok

        // Made from the graph `first` replaces, so committing it after `first`
        // would lose the file `first` made.
        let _, stale =
            VirtualFileSystem.createFile
                root
                (name "m")
                (bits 0o644)
                Owners.linuxDefault
                later
                (bytes 1)
                machine.FileSystem
            |> ok

        let committed = UnixMachineState.commitFileSystem ChangeNotice.Names first machine

        Assert.Throws<Exception> (fun () ->
            UnixMachineState.commitFileSystem ChangeNotice.Names stale committed
            |> ignore<UnixMachineState>
        )
        |> ignore<Exception>
