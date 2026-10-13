namespace WoofWare.PosixKernel.Test

open System.Collections.Generic
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test.InotifyReplay

/// Which records each filesystem change queues on the inotify instances
/// watching it, against a reference that states them by operation, as
/// docs/plans/2026-10-11-inotify's tables do (3.3, and `inotify-delivery-probe.c`).
///
/// Random sequences of path syscalls (creating, truncating, linking,
/// unlinking, renaming, changing modes, owners and times, through plain paths,
/// "/." and "/", symbolic links and hard links), of descriptors opened, written
/// through, truncated, `fchmod`ed and closed, and of watches added and
/// removed, some with `IN_EXCL_UNLINK`, on two instances, run against the
/// model; after each, both instances are drained, and their records must be
/// those the reference expects, cookies compared by which records share one.
/// The reference keeps its own tree of paths, and the name each descriptor
/// was opened through, and asks the model only whether a call succeeded.
///
/// What the reference states, per operation, for each instance watching the
/// inode involved and asking for the event:
/// - a name created (a file, a directory, a symbolic link): `IN_CREATE` at its
///   directory; a hard link: `IN_ATTRIB` at the inode's own watch, then
///   `IN_CREATE`;
/// - a name removed: `IN_ATTRIB` at a non-directory's own watch, then the
///   inode's going if the name was its last and nothing holds it, then
///   `IN_DELETE` at its directory;
/// - a rename: `IN_MOVED_FROM` and `IN_MOVED_TO` with one fresh cookie, then
///   `IN_ATTRIB` at the displaced inode's own watch, `IN_MOVE_SELF` at the
///   moved inode's, and the displaced inode's going if the name was its last
///   and nothing holds it; nothing for a rename of a name onto the inode it
///   already names;
/// - an inode's going, at its own watch: `IN_DELETE_SELF`, then `IN_IGNORED`,
///   and the watch is gone. A file goes when a name of it dies with no link
///   left: a name nothing holds, removed, or the last descriptor opened
///   through a name since unlinked, closed; a name linked again is another
///   name. A directory goes when nothing holds it: no descriptor onto it,
///   none opened through a name in it, and no removed directory in it that
///   something holds;
/// - a mode, owner or times change, or an `O_TRUNC`, through a path: one event
///   at the watch on the directory of the name the path resolved to, with that
///   name, then at the inode's own watch, `IN_ATTRIB` for a mode, an owner or
///   both times, `IN_MODIFY` for a truncation or the modification time alone,
///   and `IN_ACCESS`, which no watch can ask for, for the access time alone;
/// - a write of some bytes, an `ftruncate` or an `fchmod` through a
///   descriptor: as through a path, at the watch on the directory of the name
///   the descriptor was opened through, with that name, which a rename of it
///   moves, and which an unlink of it, or a rename displacing the inode from
///   it, leaves in place, unlinked; a watch with `IN_EXCL_UNLINK` hears nothing
///   of a write through an unlinked name, at the directory or the inode
///   itself, and hears its `ftruncate` and `fchmod`;
/// - a record carries every event of its change, whichever the watch asked
///   for;
/// - `IN_ISDIR` when the inode is a directory, but on `IN_MOVE_SELF` and
///   `IN_DELETE_SELF`; and a record repeating the newest queued is dropped.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyDelivery =

    let private dirs : string list = [ "/" ; "/a" ; "/a/s" ; "/b" ]
    let private names : string list = [ "x" ; "y" ; "s" ; "a" ]

    let private join (dir : string) (entry : string) : string =
        if dir = "/" then $"/%s{entry}" else $"%s{dir}/%s{entry}"

    /// Every path an operation names: each directory of the pool, and each
    /// name in each of them.
    let private paths : string list =
        dirs
        @ [
            for dir in dirs do
                for entry in names -> join dir entry
        ]
        |> List.distinct

    /// How a path names its final component.
    [<RequireQualifiedAccess>]
    type private Form =
        | Plain
        /// The path with "/." appended.
        | Dot
        /// The path with "/" appended.
        | Slash

    [<RequireQualifiedAccess>]
    type private Times =
        | Both
        | AccessOnly
        | ModificationOnly

    [<RequireQualifiedAccess>]
    type private Op =
        | Create of path : int * exclusive : bool
        | OpenTruncate of path : int
        | MkDir of path : int
        | RmDir of path : int
        | Unlink of path : int
        | Link of existing : int * path : int
        | Symlink of target : int * path : int
        | Rename of source : int * destination : int
        | Chmod of path : int * form : Form * mode : int
        /// `chown` to root, or to neither ID.
        | Chown of path : int * neither : bool
        | Utimes of path : int * times : Times
        | AddWatch of instance : int * path : int * mask : uint32 * dontFollow : bool
        | RmWatch of instance : int * wd : int
        /// `open(2)` of the path, kept: read-only, or for reading and writing.
        | Open of path : int * readOnly : bool
        /// Through the kept descriptor `slot` (0 the newest, and back from
        /// there, modulo how many are kept): a write of `count` bytes.
        | Write of slot : int * count : int
        | Ftruncate of slot : int
        | Fchmod of slot : int * mode : int
        | Close of slot : int

    let private eventBits : uint32 list =
        [ modify ; attrib ; movedFrom ; movedTo ; create ; delete ; 0x400u ; 0x800u ]

    let private exclUnlink : uint32 = 0x04000000u
    let private moveSelf : uint32 = 0x800u

    let private opGen : Gen<Op> =
        let path = Gen.choose (0, paths.Length - 1)

        let mask =
            gen {
                let! events =
                    Gen.subListOf eventBits
                    |> Gen.map (List.fold (|||) 0u)
                    |> Gen.filter (fun mask -> mask <> 0u)

                let! excl = Gen.elements [ false ; true ]
                return events ||| (if excl then exclUnlink else 0u)
            }

        let slot = Gen.choose (0, 7)

        Gen.frequency
            [
                4, Gen.map2 (fun p e -> Op.Create (p, e)) path (Gen.elements [ true ; false ])
                2, Gen.map Op.OpenTruncate path
                3, Gen.map Op.MkDir path
                2, Gen.map Op.RmDir path
                3, Gen.map Op.Unlink path
                2, Gen.map2 (fun e p -> Op.Link (e, p)) path path
                1, Gen.map2 (fun t p -> Op.Symlink (t, p)) path path
                4, Gen.map2 (fun s d -> Op.Rename (s, d)) path path
                4,
                Gen.map3
                    (fun p f m -> Op.Chmod (p, f, m))
                    path
                    (Gen.elements [ Form.Plain ; Form.Plain ; Form.Dot ; Form.Slash ])
                    (Gen.elements [ 0o755 ; 0o700 ])
                2, Gen.map2 (fun p n -> Op.Chown (p, n)) path (Gen.elements [ false ; false ; true ])
                2,
                Gen.map2
                    (fun p t -> Op.Utimes (p, t))
                    path
                    (Gen.elements [ Times.Both ; Times.AccessOnly ; Times.ModificationOnly ])
                5,
                gen {
                    let! instance = Gen.choose (0, 1)
                    let! p = path
                    let! m = mask
                    let! dontFollow = Gen.elements [ false ; false ; false ; true ]
                    return Op.AddWatch (instance, p, m, dontFollow)
                }
                1, Gen.map2 (fun i w -> Op.RmWatch (i, w)) (Gen.choose (0, 1)) (Gen.choose (1, 6))
                4, Gen.map2 (fun p r -> Op.Open (p, r)) path (Gen.elements [ false ; false ; true ])
                5, Gen.map2 (fun s c -> Op.Write (s, c)) slot (Gen.elements [ 0 ; 1 ; 1 ; 3 ])
                2, Gen.map Op.Ftruncate slot
                2, Gen.map2 (fun s m -> Op.Fchmod (s, m)) slot (Gen.elements [ 0o755 ; 0o700 ])
                2, Gen.map Op.Close slot
            ]

    /// A descriptor opened through a name that something then happens to,
    /// and a change through it: the name unlinked, renamed away, renamed over,
    /// given again by a link or a fresh file, its directory removed, or left
    /// alone. A slot of 0 is the newest kept descriptor, which this opened if
    /// the open succeeded.
    let private episodeGen : Gen<Op list> =
        gen {
            let path = Gen.choose (0, paths.Length - 1)
            let! p = path
            let! q = path
            let! readOnly = Gen.elements [ false ; false ; true ]

            let! meanwhile =
                Gen.elements
                    [
                        [ Op.Unlink p ]
                        [ Op.Rename (p, q) ]
                        [ Op.Rename (q, p) ]
                        [ Op.Unlink p ; Op.Link (q, p) ]
                        [ Op.Unlink p ; Op.Create (p, false) ]
                        [ Op.RmDir p ]
                        []
                    ]

            let! change = Gen.elements [ Op.Write (0, 1) ; Op.Ftruncate 0 ; Op.Fchmod (0, 0o700) ]
            return [ Op.Create (p, false) ; Op.Open (p, readOnly) ] @ meanwhile @ [ change ]
        }

    /// A file with two names, watched itself, opened through one or both or
    /// one twice, then its names unlinked or renamed over in some order and
    /// its descriptors closed: when it goes for inotify depends on which
    /// names the descriptors hold. Or a directory, watched, opened, removed
    /// and closed.
    let private selfEpisodeGen : Gen<Op list> =
        gen {
            let path = Gen.choose (0, paths.Length - 1)
            let! p = path
            let! q = path
            let! other = path
            let! instance = Gen.choose (0, 1)
            let! events = Gen.elements [ 0xC00u ; 0x400u ; 0x4u ; List.fold (|||) 0u eventBits ]

            let! opens =
                Gen.elements [ [] ; [ p ] ; [ q ] ; [ p ; q ] ; [ p ; p ] ; [ q ; p ; q ] ]
                |> Gen.map (List.map (fun opened -> Op.Open (opened, false)))

            let! removals =
                Gen.elements
                    [
                        [ Op.Unlink p ; Op.Unlink q ]
                        [ Op.Unlink q ; Op.Unlink p ]
                        [ Op.Unlink p ; Op.Rename (other, q) ]
                        [
                            Op.Unlink q
                            Op.Link (p, q)
                            Op.Open (q, false)
                            Op.Unlink p
                            Op.Unlink q
                        ]
                    ]

            let! closes = Gen.listOfLength 4 (Gen.map Op.Close (Gen.choose (0, 3)))
            let! directory = Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ]

            if directory then
                return
                    [
                        Op.MkDir p
                        Op.AddWatch (instance, p, events, false)
                        Op.Open (p, true)
                        Op.RmDir p
                        Op.Close 0
                    ]
            else
                return
                    [
                        Op.Create (p, false)
                        Op.Link (p, q)
                        Op.AddWatch (instance, p, events, false)
                    ]
                    @ opens
                    @ removals
                    @ closes
        }

    /// Half the sequences open by watching every directory of the pool, on
    /// both instances, for every event, half of those with `IN_EXCL_UNLINK`,
    /// so that most changes reach a watch.
    let private caseGen : Gen<Op list> =
        gen {
            let! opening = Gen.elements [ true ; false ]
            let! excl = Gen.elements [ 0u ; exclUnlink ]
            let! length = Gen.choose (10, 40)

            let! ops =
                Gen.listOfLength
                    length
                    (Gen.frequency [ 6, Gen.map List.singleton opGen ; 2, episodeGen ; 1, selfEpisodeGen ])
                |> Gen.map List.concat

            let watchAll =
                [
                    for instance in 0..1 do
                        for dir in dirs do
                            Op.AddWatch (
                                instance,
                                List.findIndex ((=) dir) paths,
                                List.fold (|||) 0u eventBits ||| excl,
                                false
                            )
                ]

            return (if opening then watchAll else []) @ ops
        }

    // ------------------------------------------------------------ the reference

    [<RequireQualifiedAccess>]
    type private Label =
        | Create
        | Delete
        | Moved
        | AttribNamed
        | AttribSelf
        | Modify
        | Directory
        | Ignored
        | Call
        /// A record of a change through a kept descriptor.
        | ThroughDescriptor
        /// A record of a change through a kept descriptor whose name has been
        /// unlinked.
        | ThroughUnlinked
        /// A write through an unlinked name, under a watch with
        /// `IN_EXCL_UNLINK` that the reference expects to hear nothing of it.
        | ExcludedUnlinked
        /// A record of a change through a kept descriptor whose name a rename
        /// moved.
        | ThroughMoved
        | DeleteSelf
        | MoveSelf
        /// A watched inode's going.
        | Gone
        /// A watched file's going while a descriptor still holds it, through
        /// another name.
        | GoneWhileOpen


    [<RequireQualifiedAccess>]
    type private Node =
        | File of id : int
        | Directory of id : int
        | Symlink of target : string * id : int

    let private idOf (node : Node) : int =
        match node with
        | Node.File id
        | Node.Directory id
        | Node.Symlink (_, id) -> id

    let private isDirectory (node : Node) : bool =
        match node with
        | Node.Directory _ -> true
        | Node.File _
        | Node.Symlink _ -> false

    let private parentOf (path : string) : string =
        match path.LastIndexOf '/' with
        | 0 -> "/"
        | i -> path.Substring (0, i)

    let private entryOf (path : string) : string =
        path.Substring (path.LastIndexOf '/' + 1)

    /// A record as compared: the watch descriptor, the mask, the cookie, and
    /// the name.
    type private Expected = int * uint32 * uint32 * string

    /// The name a kept descriptor was opened through: the directory, by its
    /// node's identity, and the name in it, and whether that name has since
    /// been unlinked; or none, for the root.
    type private OpenedThrough =
        {
            Parent : int
            Entry : string
            Unlinked : bool
            /// A rename has moved the name since the descriptor was opened.
            Moved : bool
            /// Which name this is: two descriptors opened through one name
            /// while it stays linked share one, and a name linked again is
            /// another.
            Dentry : int
        }

    /// A kept descriptor: what it names, how it was opened, and through which
    /// name.
    type private Kept =
        {
            Node : Node
            ReadOnly : bool
            Through : OpenedThrough option
        }

    type private Reference =
        {
            /// Every name, by its path from the root, which is always a
            /// directory.
            Tree : Map<string, Node>
            NextId : int
            NextCookie : uint32
            /// Each instance's watches: by watched inode, its descriptor and
            /// events.
            Watches : Map<int, Map<int, int * uint32>>
            NextWd : Map<int, int>
            /// Each instance's records since it was drained.
            Queued : Map<int, Expected list>
            /// The kept descriptors, oldest first, as the model keeps them.
            Kept : Kept list
            NextDentry : int
            /// Each directory removed while something held it, by its node's
            /// identity: the directory it was last named in.
            Orphans : Map<int, int>
        }

    let private initialReference : Reference =
        let tree =
            [
                "/", Node.Directory 0
                "/a", Node.Directory 1
                "/a/s", Node.Directory 2
                "/b", Node.Directory 3
                "/a/x", Node.File 4
                "/b/y", Node.File 5
            ]
            |> Map.ofList

        {
            Tree = tree
            NextId = 6
            NextCookie = 1u
            Watches = Map.ofList [ 0, Map.empty ; 1, Map.empty ]
            NextWd = Map.ofList [ 0, 1 ; 1, 1 ]
            Queued = Map.ofList [ 0, [] ; 1, [] ]
            Kept = []
            NextDentry = 0
            Orphans = Map.empty
        }

    /// The model's starting tree, matching `initialReference`.
    let private seed : (string * SeedEntry) list =
        [
            "a", directory 0o755 [ "s", directory 0o755 [] ; "x", file "x" ]
            "b", directory 0o755 [ "y", file "y" ]
        ]

    /// Where `path` resolves to: the path, with every symbolic link the walk
    /// follows replaced by its target, of the name it ends at, and what that
    /// name holds, if anything. A final link is followed when `followFinal`.
    /// `None` when the walk fails before its final component.
    let rec private resolve
        (depth : int)
        (tree : Map<string, Node>)
        (followFinal : bool)
        (path : string)
        : (string * Node option) option
        =
        if depth > 40 then
            None
        else

        let components =
            path.Split ('/', System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

        let rec walk (current : string) (components : string list) =
            match components with
            | [] -> Some (current, Map.tryFind current tree)
            | next :: rest ->

            let candidate = join current next

            match Map.tryFind candidate tree, rest with
            | None, [] -> Some (candidate, None)
            | None, _ -> None
            | Some (Node.Symlink (target, _)), _ when not rest.IsEmpty || followFinal ->
                match resolve (depth + 1) tree true target with
                | Some (resolved, Some (Node.Directory _)) -> walk resolved rest
                | Some (resolved, found) when rest.IsEmpty -> Some (resolved, found)
                | _ -> None
            | Some (Node.Directory _), _ -> walk candidate rest
            | Some node, [] -> Some (candidate, Some node)
            | Some _, _ -> None

        walk "/" components

    /// `reference` with `events` reported to each instance's watch on `inode`
    /// that asks for any of them, as one record each carrying all of them,
    /// but to none with `IN_EXCL_UNLINK` when `excluded`.
    let private notifyUnless
        (excluded : bool)
        (inode : int)
        (events : uint32)
        (isDir : bool)
        (cookie : uint32)
        (entry : string)
        (reference : Reference)
        : Reference
        =
        (reference, [ 0 ; 1 ])
        ||> List.fold (fun reference instance ->
            match Map.tryFind inode reference.Watches.[instance] with
            | None -> reference
            | Some (wd, mask) ->

            // The watch's mask decides whether the record is queued, not which
            // events it carries.
            if events &&& mask = 0u || excluded && mask &&& exclUnlink <> 0u then
                reference
            else

            let record : Expected =
                wd, events ||| (if isDir then 0x40000000u else 0u), cookie, entry

            let queued = reference.Queued.[instance]

            let repeats =
                match List.tryLast queued with
                | Some (lastWd, lastMask, _, lastEntry) ->
                    let wd, mask, _, entry = record
                    lastWd = wd && lastMask = mask && lastEntry = entry
                | None -> false

            if repeats then
                reference
            else
                { reference with
                    Queued = Map.add instance (queued @ [ record ]) reference.Queued
                }
        )

    let private notify = notifyUnless false

    /// `notify` at the directory holding `path`, naming its last component.
    let private notifyParent (path : string) (events : uint32) (node : Node) (cookie : uint32) (reference : Reference) =
        match Map.tryFind (parentOf path) reference.Tree with
        | Some parent -> notify (idOf parent) events (isDirectory node) cookie (entryOf path) reference
        | None -> failwith $"the reference holds %s{path} but not its directory"

    /// `notify` at `node`'s own watch.
    let private notifySelf (events : uint32) (node : Node) (reference : Reference) : Reference =
        notify (idOf node) events (isDirectory node) 0u "" reference

    /// A change through a path that resolved to the name `path`: at its
    /// directory under its name, unless it is the root, then at its own watch.
    let private notifyPath (path : string) (events : uint32) (node : Node) (reference : Reference) : Reference =
        let reference =
            if path = "/" then
                reference
            else
                notifyParent path events node 0u reference

        notifySelf events node reference

    /// A change through the kept descriptor `kept`: at the directory of the
    /// name it was opened through, under that name, then at its own watch,
    /// each but to a watch with `IN_EXCL_UNLINK` when the name is unlinked and
    /// `excludable`.
    let private notifyKept (kept : Kept) (events : uint32) (excludable : bool) (reference : Reference) : Reference =
        let excluded, reference =
            match kept.Through with
            | None -> false, reference
            | Some through ->
                let excluded = excludable && through.Unlinked

                excluded, notifyUnless excluded through.Parent events (isDirectory kept.Node) 0u through.Entry reference

        notifyUnless excluded (idOf kept.Node) events (isDirectory kept.Node) 0u "" reference

    /// `inode`'s going: at each instance's watch on it, `IN_DELETE_SELF` if
    /// asked for, then `IN_IGNORED`, and the watch gone.
    let private gone (cover : Label -> unit) (inode : int) (reference : Reference) : Reference =
        (reference, [ 0 ; 1 ])
        ||> List.fold (fun reference instance ->
            match Map.tryFind inode reference.Watches.[instance] with
            | None -> reference
            | Some (wd, mask) ->
                cover Label.Gone

                if reference.Kept |> List.exists (fun kept -> idOf kept.Node = inode) then
                    cover Label.GoneWhileOpen

                let deleted = if mask &&& 0x400u <> 0u then [ wd, 0x400u, 0u, "" ] else []

                { reference with
                    Watches = Map.add instance (Map.remove inode reference.Watches.[instance]) reference.Watches
                    Queued =
                        Map.add
                            instance
                            (reference.Queued.[instance] @ deleted @ [ wd, 0x8000u, 0u, "" ])
                            reference.Queued
                }
        )

    /// How many names the tree binds to `inode`.
    let private links (inode : int) (reference : Reference) : int =
        reference.Tree |> Map.filter (fun _ node -> idOf node = inode) |> Map.count

    /// Whether some kept descriptor was opened through the name `entry` in
    /// `parent`, still linked, onto `inode`.
    let private nameHeld (parent : int) (entry : string) (inode : int) (reference : Reference) : bool =
        reference.Kept
        |> List.exists (fun kept ->
            idOf kept.Node = inode
            && (
                match kept.Through with
                | Some through -> through.Parent = parent && through.Entry = entry && not through.Unlinked
                | None -> false
            )
        )

    /// Whether anything holds the directory `inode`: a kept descriptor onto
    /// it, or opened through a name in it, or a removed directory in it that
    /// is held.
    let private directoryHeld (inode : int) (reference : Reference) : bool =
        reference.Kept
        |> List.exists (fun kept ->
            idOf kept.Node = inode
            || (
                match kept.Through with
                | Some through -> through.Parent = inode
                | None -> false
            )
        )
        || reference.Orphans |> Map.exists (fun _ parent -> parent = inode)

    /// Each removed directory nothing holds any more goes, from `inode` up
    /// the directories it was last named in.
    let rec private collectOrphan (cover : Label -> unit) (inode : int) (reference : Reference) : Reference =
        match Map.tryFind inode reference.Orphans with
        | Some parent when not (directoryHeld inode reference) ->
            { reference with
                Orphans = Map.remove inode reference.Orphans
            }
            |> gone cover inode
            |> collectOrphan cover parent
        | Some _
        | None -> reference

    /// The name `entry` in the directory `parent`, bound to `node`, has just
    /// been removed: `node` goes if that was its last name and nothing holds
    /// the name (a file), or nothing holds it (a directory, which is
    /// otherwise kept as removed). `held` is whether a kept descriptor held
    /// the name before it was removed.
    let private nameRemoved
        (cover : Label -> unit)
        (parent : int)
        (node : Node)
        (held : bool)
        (reference : Reference)
        : Reference
        =
        match node with
        | Node.Directory id ->
            if directoryHeld id reference then
                { reference with
                    Orphans = Map.add id parent reference.Orphans
                }
            else
                gone cover id reference
        | Node.File id
        | Node.Symlink (_, id) ->
            if links id reference = 0 && not held then
                gone cover id reference
            else
                reference

    /// `reference` with the kept descriptors opened through the name `entry`
    /// in the directory `parent`, onto `node`, and not yet unlinked, rewritten
    /// by `f`.
    let private mapKept
        (parent : int)
        (entry : string)
        (node : Node)
        (f : OpenedThrough -> OpenedThrough)
        (reference : Reference)
        : Reference
        =
        { reference with
            Kept =
                reference.Kept
                |> List.map (fun kept ->
                    match kept.Through with
                    | Some through when
                        through.Parent = parent
                        && through.Entry = entry
                        && not through.Unlinked
                        && idOf kept.Node = idOf node
                        ->
                        { kept with
                            Through = Some (f through)
                        }
                    | _ -> kept
                )
        }

    /// `mapKept`, marking the name `path`, onto `node`, unlinked.
    let private unlinkKept (path : string) (node : Node) (reference : Reference) : Reference =
        match Map.tryFind (parentOf path) reference.Tree with
        | Some parent ->
            mapKept
                (idOf parent)
                (entryOf path)
                node
                (fun through ->
                    { through with
                        Unlinked = true
                    }
                )
                reference
        | None -> failwith $"the reference holds %s{path} but not its directory"

    /// The index, oldest first, of the kept descriptor `slot` picks among
    /// `count`: the newest for 0, and back from there.
    let private slotIndex (slot : int) (count : int) : int = count - 1 - slot % count

    /// The kept descriptor `slot` picks, if any is kept.
    let private keptAt (slot : int) (reference : Reference) : (int * Kept) option =
        match reference.Kept with
        | [] -> None
        | kept ->
            let index = slotIndex slot kept.Length
            Some (index, kept.[index])

    let private bind (path : string) (node : Node) (reference : Reference) : Reference =
        { reference with
            Tree = Map.add path node reference.Tree
        }

    let private fresh (reference : Reference) : int * Reference =
        reference.NextId,
        { reference with
            NextId = reference.NextId + 1
        }

    /// What `op` did to the reference, given that the model's call succeeded,
    /// and the watch descriptor the reference expects an `AddWatch` to answer.
    let private apply (cover : Label -> unit) (op : Op) (reference : Reference) : Reference * int option =
        let tree = reference.Tree

        let lookup (path : string) (followFinal : bool) =
            match resolve 0 tree followFinal path with
            | Some found -> found
            | None -> failwith $"the model's call on %s{path} succeeded, but the reference cannot walk it"

        match op with
        | Op.Create (path, _) ->
            match lookup paths.[path] true with
            | _, Some _ -> reference, None
            | created, None ->
                let id, reference = fresh reference
                let node = Node.File id
                bind created node reference |> notifyParent created create node 0u, None
        | Op.OpenTruncate path ->
            match lookup paths.[path] true with
            | resolved, Some (Node.File _ as node) -> notifyPath resolved modify node reference, None
            | other -> failwith $"O_TRUNC of %s{paths.[path]} succeeded on %A{other}"
        | Op.MkDir path ->
            let created, _ = lookup paths.[path] false
            let id, reference = fresh reference
            let node = Node.Directory id
            bind created node reference |> notifyParent created create node 0u, None
        | Op.Symlink (target, path) ->
            let created, _ = lookup paths.[path] false
            let id, reference = fresh reference
            let node = Node.Symlink (paths.[target], id)
            bind created node reference |> notifyParent created create node 0u, None
        | Op.RmDir path ->
            match lookup paths.[path] false with
            | removed, Some node ->
                let parent = idOf (Map.find (parentOf removed) tree)

                { reference with
                    Tree = Map.remove removed tree
                }
                |> unlinkKept removed node
                |> nameRemoved cover parent node false
                |> notifyParent removed delete node 0u,
                None
            | other -> failwith $"rmdir of %s{paths.[path]} succeeded on %A{other}"
        | Op.Unlink path ->
            match lookup paths.[path] false with
            | removed, Some node ->
                let parent = idOf (Map.find (parentOf removed) tree)
                let held = nameHeld parent (entryOf removed) (idOf node) reference

                notifySelf attrib node reference
                |> fun reference ->
                    { reference with
                        Tree = Map.remove removed reference.Tree
                    }
                |> unlinkKept removed node
                |> nameRemoved cover parent node held
                |> notifyParent removed delete node 0u,
                None
            | other -> failwith $"unlink of %s{paths.[path]} succeeded on %A{other}"
        | Op.Link (existing, path) ->
            match lookup paths.[existing] false, lookup paths.[path] false with
            | (_, Some node), (created, None) ->
                notifySelf attrib node reference
                |> bind created node
                |> notifyParent created create node 0u,
                None
            | other -> failwith $"link succeeded on %A{other}"
        | Op.Rename (source, destination) ->
            match lookup paths.[source] false, lookup paths.[destination] false with
            | (_, Some moved), (_, Some displaced) when idOf moved = idOf displaced -> reference, None
            | (from, Some moved), (towards, displaced) ->
                let cookie = reference.NextCookie

                let renamed (path : string) : string option =
                    if path = from then
                        Some towards
                    elif path.StartsWith (from + "/") then
                        Some (towards + path.Substring from.Length)
                    else
                        None

                let tree =
                    tree
                    |> Map.remove towards
                    |> Map.toList
                    |> List.map (fun (path, node) ->
                        match renamed path with
                        | Some path -> path, node
                        | None -> path, node
                    )
                    |> Map.ofList

                // Read before the rename: the directories the names are in.
                let fromParent = idOf (Map.find (parentOf from) tree)
                let towardsParent = idOf (Map.find (parentOf towards) tree)

                let displacedHeld =
                    match displaced with
                    | Some displaced -> nameHeld towardsParent (entryOf towards) (idOf displaced) reference
                    | None -> false

                let reference =
                    match displaced with
                    | Some displaced -> unlinkKept towards displaced reference
                    | None -> reference

                let reference =
                    mapKept
                        fromParent
                        (entryOf from)
                        moved
                        (fun through ->
                            { through with
                                Parent = towardsParent
                                Entry = entryOf towards
                                Moved = true
                            }
                        )
                        reference

                let reference =
                    { reference with
                        NextCookie = cookie + 1u
                    }
                    |> notifyParent from movedFrom moved cookie

                let reference =
                    { reference with
                        Tree = tree
                    }
                    |> notifyParent towards movedTo moved cookie

                let reference =
                    match displaced with
                    | Some displaced -> notifySelf attrib displaced reference
                    | None -> reference

                // The moved inode's own, which carries no `IN_ISDIR`.
                let reference = notify (idOf moved) moveSelf false 0u "" reference

                let reference =
                    match displaced with
                    | Some displaced -> nameRemoved cover towardsParent displaced displacedHeld reference
                    | None -> reference

                reference, None
            | other -> failwith $"rename succeeded on %A{other}"
        | Op.Chmod (path, _, _) ->
            match lookup paths.[path] true with
            | resolved, Some node -> notifyPath resolved attrib node reference, None
            | other -> failwith $"chmod succeeded on %A{other}"
        | Op.Chown (_, true) -> reference, None
        | Op.Chown (path, false) ->
            match lookup paths.[path] true with
            | resolved, Some node -> notifyPath resolved attrib node reference, None
            | other -> failwith $"chown succeeded on %A{other}"
        | Op.Utimes (path, times) ->
            let events =
                match times with
                | Times.Both -> attrib
                | Times.ModificationOnly -> modify
                | Times.AccessOnly -> 0x1u

            match lookup paths.[path] true with
            | resolved, Some node -> notifyPath resolved events node reference, None
            | other -> failwith $"utimensat succeeded on %A{other}"
        | Op.AddWatch (instance, path, mask, dontFollow) ->
            match lookup paths.[path] (not dontFollow) with
            | _, Some node ->
                let watches = reference.Watches.[instance]

                let wd, reference =
                    match Map.tryFind (idOf node) watches with
                    | Some (wd, _) -> wd, reference
                    | None ->
                        let wd = reference.NextWd.[instance]

                        wd,
                        { reference with
                            NextWd = Map.add instance (wd + 1) reference.NextWd
                        }

                { reference with
                    Watches = Map.add instance (Map.add (idOf node) (wd, mask) watches) reference.Watches
                },
                Some wd
            | other -> failwith $"inotify_add_watch succeeded on %A{other}"
        | Op.Open (path, readOnly) ->
            match lookup paths.[path] true with
            | opened, Some node ->
                let parent = idOf (Map.find (parentOf opened) tree)

                // The name, as a descriptor already opened through it holds it.
                let shared =
                    reference.Kept
                    |> List.tryPick (fun kept ->
                        match kept.Through with
                        | Some through when
                            through.Parent = parent
                            && through.Entry = entryOf opened
                            && not through.Unlinked
                            && idOf kept.Node = idOf node
                            ->
                            Some through.Dentry
                        | _ -> None
                    )

                let dentry, reference =
                    match shared with
                    | Some dentry -> dentry, reference
                    | None ->
                        reference.NextDentry,
                        { reference with
                            NextDentry = reference.NextDentry + 1
                        }

                let through =
                    if opened = "/" then
                        None
                    else
                        Some
                            {
                                Parent = parent
                                Entry = entryOf opened
                                Unlinked = false
                                Moved = false
                                Dentry = dentry
                            }

                { reference with
                    Kept =
                        reference.Kept
                        @ [
                            {
                                Node = node
                                ReadOnly = readOnly
                                Through = through
                            }
                        ]
                },
                None
            | other -> failwith $"open of %s{paths.[path]} succeeded on %A{other}"
        | Op.Write (slot, count) ->
            match keptAt slot reference with
            | Some (_, kept) when count > 0 -> notifyKept kept modify true reference, None
            | Some _ -> reference, None
            | None -> failwith $"a write through slot %d{slot} succeeded, but the reference keeps no descriptor"
        | Op.Ftruncate slot ->
            match keptAt slot reference with
            | Some (_, kept) -> notifyKept kept modify false reference, None
            | None -> failwith $"ftruncate through slot %d{slot} succeeded, but the reference keeps no descriptor"
        | Op.Fchmod (slot, _) ->
            match keptAt slot reference with
            | Some (_, kept) -> notifyKept kept attrib false reference, None
            | None -> failwith $"fchmod through slot %d{slot} succeeded, but the reference keeps no descriptor"
        | Op.Close slot ->
            match keptAt slot reference with
            | Some (index, closed) ->
                let reference =
                    { reference with
                        Kept = reference.Kept |> List.removeAt index
                    }

                // The last descriptor holding a name since unlinked lets go of
                // it: a file with no link left goes. A removed directory
                // onto it or holding that name goes once nothing holds it.
                let reference =
                    match closed.Node, closed.Through with
                    | (Node.File id | Node.Symlink (_, id)), Some through when
                        through.Unlinked
                        && links id reference = 0
                        && not (
                            reference.Kept
                            |> List.exists (fun kept ->
                                match kept.Through with
                                | Some other -> other.Unlinked && other.Dentry = through.Dentry
                                | None -> false
                            )
                        )
                        ->
                        gone cover id reference
                    | _ -> reference

                let reference = collectOrphan cover (idOf closed.Node) reference

                let reference =
                    match closed.Through with
                    | Some through -> collectOrphan cover through.Parent reference
                    | None -> reference

                reference, None
            | None -> failwith $"close of slot %d{slot} succeeded, but the reference keeps no descriptor"
        | Op.RmWatch (instance, wd) ->
            let watches = reference.Watches.[instance]

            match watches |> Map.tryFindKey (fun _ (watched, _) -> watched = wd) with
            | None -> failwith $"inotify_rm_watch of %d{wd} succeeded, but the reference has no such watch"
            | Some inode ->
                { reference with
                    Watches = Map.add instance (Map.remove inode watches) reference.Watches
                    Queued = Map.add instance (reference.Queued.[instance] @ [ wd, 0x8000u, 0u, "" ]) reference.Queued
                },
                None

    // ---------------------------------------------------------------- the model

    let private linuxAtFdCwd : int = -100

    /// What the model's call answered, and the system after it: `None` for a
    /// refusal of a path syscall or of a watch, which changes nothing, or for
    /// a call through a kept descriptor when none is kept, which is not made.
    /// `kept` is the descriptors the model keeps, oldest first. A call
    /// through a kept descriptor must not be refused.
    let private call
        (fds : int list)
        (kept : int list)
        (op : Op)
        (system : UnixSystem<int, string>)
        : (SyscallAnswer * UnixSystem<int, string>) option
        =
        let path (i : int) = PathArg.ofText paths.[i]

        let unrefused (result : Result<SyscallAnswer * UnixSystem<int, string>, 'r>) =
            match result with
            | Ok answered -> Some answered
            | Error _ -> None

        let opened (flags : OpenFlags) (i : int) =
            match OpenFlagWords.openPath flags (path i) 0o644 system with
            | Ok (SyscallAnswer.Completed fd, after) -> Some (SyscallAnswer.Completed 0L, close (int fd) after)
            | Ok answered -> Some answered
            | Error _ -> None

        match op with
        | Op.Create (i, exclusive) -> opened (openFlags FileAccessMode.WriteOnly true exclusive false) i
        | Op.OpenTruncate i -> opened (openFlags FileAccessMode.WriteOnly false false true) i
        | Op.MkDir i -> UnixNamespace.tryMkdir (path i) 0o755 system |> unrefused
        | Op.RmDir i -> UnixNamespace.tryRmdir (path i) system |> unrefused
        | Op.Unlink i -> UnixNamespace.tryUnlink (path i) system |> unrefused
        | Op.Link (existing, i) -> UnixNamespace.tryLinkEager (path existing) (path i) system |> unrefused
        | Op.Symlink (target, i) -> UnixNamespace.trySymlinkEager (path target) (path i) system |> unrefused
        | Op.Rename (source, destination) ->
            UnixNamespace.tryRenameEager (path source) (path destination) system
            |> unrefused
        | Op.Chmod (i, form, mode) ->
            let text =
                match form with
                | Form.Plain -> paths.[i]
                | Form.Dot -> paths.[i] + "/."
                | Form.Slash -> paths.[i] + "/"

            UnixPathResolution.tryChmod (PathArg.ofText text) mode system |> unrefused
        | Op.Chown (i, neither) ->
            let id = if neither then None else Some UserId.root
            let gid = if neither then None else Some (group 0u)
            UnixPathResolution.tryChown (path i) id gid system |> unrefused
        | Op.Utimes (i, times) ->
            let access, modification =
                match times with
                | Times.Both -> timespec 1L 0L, timespec 2L 0L
                | Times.AccessOnly -> timespec 1L 0L, timespec 0L utimeOmit
                | Times.ModificationOnly -> timespec 0L utimeOmit, timespec 2L 0L

            UnixPathResolution.tryUtimensat
                linuxAtFdCwd
                (NullablePathArgument.NotNull (path i))
                (TimesArgument.Fields (access, modification))
                0
                system
            |> unrefused
        | Op.AddWatch (instance, i, mask, dontFollow) ->
            UnixInotify.tryInotifyAddWatchEager
                fds.[instance]
                (path i)
                (mask ||| (if dontFollow then 0x02000000u else 0u))
                system
            |> unrefused
        | Op.RmWatch (instance, wd) -> UnixInotify.tryInotifyRmWatch fds.[instance] wd system |> unrefused
        | Op.Open (i, readOnly) ->
            let access =
                if readOnly then
                    FileAccessMode.ReadOnly
                else
                    FileAccessMode.ReadWrite

            match OpenFlagWords.openPath (openFlags access false false false) (path i) 0 system with
            | Ok answered -> Some answered
            | Error refusal -> failwith $"%A{op}: refused: %s{OpenRefusal.describe refusal}"
        | Op.Write _
        | Op.Ftruncate _
        | Op.Fchmod _
        | Op.Close _ ->

        match kept with
        | [] -> None
        | kept ->

        let slotOf (slot : int) = kept.[slotIndex slot kept.Length]

        let mustAnswer (result : Result<SyscallAnswer * UnixSystem<int, string>, 'r>) =
            match result with
            | Ok answered -> Some answered
            | Error refusal -> failwith $"%A{op}: refused: %A{refusal}"

        match op with
        | Op.Write (slot, count) ->
            let bytes =
                System.Collections.Immutable.ImmutableArray.CreateRange (Array.create count (byte 'w'))

            match
                WriteOutcomes.admitThenWrite 0 (slotOf slot) UserBuffer.Mapped bytes system
                |> WriteOutcomes.returned
            with
            | Ok (WriteAnswer.Completed written, system) -> Some (SyscallAnswer.Completed written, system)
            | Ok (WriteAnswer.Failed error, system) -> Some (SyscallAnswer.Failed error, system)
            | Error refusal -> failwith $"%A{op}: refused: %A{refusal}"
        | Op.Ftruncate slot -> UnixDescriptor.tryFtruncate (slotOf slot) 0L system |> mustAnswer
        | Op.Fchmod (slot, mode) -> UnixPathResolution.tryFchmod (slotOf slot) mode system |> mustAnswer
        | Op.Close slot -> UnixDescriptor.tryClose (slotOf slot) system |> mustAnswer
        | other -> failwith $"%A{other} is not a call through a kept descriptor"

    /// Numbers each cookie by the order it first appears in.
    type private Cookies () =
        let seen = Dictionary<uint32, uint32> ()

        member _.Ordinal (cookie : uint32) : uint32 =
            if cookie = 0u then
                0u
            else
                match seen.TryGetValue cookie with
                | true, ordinal -> ordinal
                | false, _ ->
                    let ordinal = uint32 seen.Count + 1u
                    seen.[cookie] <- ordinal
                    ordinal

    let private run (cover : Label -> unit) (ops : Op list) : unit =
        let system = boot root seed
        let first, system = init1 nonblock system
        let second, system = init1 nonblock system
        let fds = [ first ; second ]
        let modelCookies = Cookies ()
        let referenceCookies = Cookies ()

        let normalise (cookies : Cookies) (records : Expected list) : Expected list =
            records
            |> List.map (fun (wd, mask, cookie, entry) -> wd, mask, cookies.Ordinal cookie, entry)

        ((system, initialReference, []), List.indexed ops)
        ||> List.fold (fun (system, reference, kept) (step, op) ->
            cover Label.Call

            // What the reference keeps of the descriptor a call through one
            // reaches, and whether it is a write of some bytes.
            let through, writes =
                match op with
                | Op.Write (slot, count) -> keptAt slot reference |> Option.map snd, count > 0
                | Op.Ftruncate slot
                | Op.Fchmod (slot, _) -> keptAt slot reference |> Option.map snd, false
                | _ -> None, false

            let system, reference, kept =
                match call fds kept op system with
                | None -> system, reference, kept
                | Some (SyscallAnswer.Failed _, after) -> after, reference, kept
                | Some (SyscallAnswer.Completed answer, after) ->
                    let queuedBefore = reference.Queued
                    let reference, wd = apply cover op reference

                    match wd with
                    | Some wd -> answer |> shouldEqual (int64 wd)
                    | None -> ()

                    let kept =
                        match op with
                        | Op.Open _ -> kept @ [ int answer ]
                        | Op.Close slot -> kept |> List.removeAt (slotIndex slot kept.Length)
                        | _ -> kept

                    match through with
                    | Some through ->
                        if reference.Queued <> queuedBefore then
                            cover Label.ThroughDescriptor

                            match through.Through with
                            | Some name when name.Unlinked -> cover Label.ThroughUnlinked
                            | Some name when name.Moved -> cover Label.ThroughMoved
                            | _ -> ()

                        // A watch that would have heard of the write but for
                        // `IN_EXCL_UNLINK`.
                        match through.Through with
                        | Some name when writes && name.Unlinked ->
                            let excludes (inode : int) =
                                [ 0 ; 1 ]
                                |> List.exists (fun instance ->
                                    match Map.tryFind inode reference.Watches.[instance] with
                                    | Some (_, mask) -> mask &&& modify <> 0u && mask &&& exclUnlink <> 0u
                                    | None -> false
                                )

                            if excludes name.Parent || excludes (idOf through.Node) then
                                cover Label.ExcludedUnlinked
                        | _ -> ()
                    | None -> ()

                    after, reference, kept

            UnixSystem.checkInvariants system |> shouldEqual []

            let system =
                (system, List.indexed fds)
                ||> List.fold (fun system (instance, fd) ->
                    let records, system = drainRecords fd system

                    let actual =
                        records
                        |> List.map (fun record -> record.Wd, record.Mask, record.Cookie, record.Name)
                        |> normalise modelCookies

                    let expected = reference.Queued.[instance] |> normalise referenceCookies

                    if actual <> expected then
                        failwith
                            $"step %d{step}, %A{op}: instance %d{instance} queued %A{actual}, where the reference expects %A{expected}"

                    for _, mask, _, entry in expected do
                        if mask &&& create <> 0u then
                            cover Label.Create

                        if mask &&& delete <> 0u then
                            cover Label.Delete

                        if mask &&& movedTo <> 0u then
                            cover Label.Moved

                        if mask &&& attrib <> 0u && entry <> "" then
                            cover Label.AttribNamed

                        if mask &&& attrib <> 0u && entry = "" then
                            cover Label.AttribSelf

                        if mask &&& modify <> 0u then
                            cover Label.Modify

                        if mask &&& 0x40000000u <> 0u then
                            cover Label.Directory

                        if mask &&& 0x8000u <> 0u then
                            cover Label.Ignored

                        if mask &&& 0x400u <> 0u then
                            cover Label.DeleteSelf

                        if mask &&& moveSelf <> 0u then
                            cover Label.MoveSelf

                    system
                )

            system,
            { reference with
                Queued = Map.ofList [ 0, [] ; 1, [] ]
            },
            kept
        )
        |> ignore

    [<Test>]
    let ``each path syscall queues on each watching instance the records the reference states`` () : unit =
        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 300) (Arb.fromGen caseGen) run

        coverage.Count Label.Create |> shouldBeGreaterThan 100
        coverage.Count Label.Delete |> shouldBeGreaterThan 100
        coverage.Count Label.Moved |> shouldBeGreaterThan 100
        coverage.Count Label.AttribNamed |> shouldBeGreaterThan 100
        coverage.Count Label.AttribSelf |> shouldBeGreaterThan 50
        coverage.Count Label.Modify |> shouldBeGreaterThan 50
        coverage.Count Label.Directory |> shouldBeGreaterThan 100
        coverage.Count Label.Ignored |> shouldBeGreaterThan 20
        coverage.Count Label.ThroughDescriptor |> shouldBeGreaterThan 300
        coverage.Count Label.ThroughUnlinked |> shouldBeGreaterThan 100
        coverage.Count Label.ThroughMoved |> shouldBeGreaterThan 30
        coverage.Count Label.ExcludedUnlinked |> shouldBeGreaterThan 20
        coverage.Count Label.DeleteSelf |> shouldBeGreaterThan 20
        coverage.Count Label.MoveSelf |> shouldBeGreaterThan 20
        coverage.Count Label.Gone |> shouldBeGreaterThan 50
        coverage.Count Label.GoneWhileOpen |> shouldBeGreaterThan 5
