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
/// "/." and "/", symbolic links and hard links) and of watches added and
/// removed, on two instances, run against the model; after each, both
/// instances are drained, and their records must be those the reference
/// expects, cookies compared by which records share one. The reference keeps
/// its own tree of paths, and asks the model only whether a call succeeded.
///
/// What the reference states, per operation, for each instance watching the
/// inode involved and asking for the event:
/// - a name created (a file, a directory, a symbolic link): `IN_CREATE` at its
///   directory; a hard link: `IN_ATTRIB` at the inode's own watch, then
///   `IN_CREATE`;
/// - a name removed: `IN_ATTRIB` at a non-directory's own watch, then
///   `IN_DELETE` at its directory;
/// - a rename: `IN_MOVED_FROM` and `IN_MOVED_TO` with one fresh cookie, then
///   `IN_ATTRIB` at the displaced inode's own watch; nothing for a rename of a
///   name onto the inode it already names;
/// - a mode, owner or times change, or an `O_TRUNC`, through a path: one event
///   at the watch on the directory of the name the path resolved to, with that
///   name, then at the inode's own watch, `IN_ATTRIB` for a mode, an owner or
///   both times, `IN_MODIFY` for a truncation or the modification time alone,
///   and `IN_ACCESS`, which no watch can ask for, for the access time alone;
/// - a record carries every event of its change, whichever the watch asked
///   for;
/// - `IN_ISDIR` when the inode is a directory; and a record repeating the
///   newest queued is dropped.
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

    let private eventBits : uint32 list =
        [ modify ; attrib ; movedFrom ; movedTo ; create ; delete ; 0x400u ; 0x800u ]

    let private opGen : Gen<Op> =
        let path = Gen.choose (0, paths.Length - 1)

        let mask =
            Gen.subListOf eventBits
            |> Gen.map (List.fold (|||) 0u)
            |> Gen.filter (fun mask -> mask <> 0u)

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
            ]

    /// Half the sequences open by watching every directory of the pool, on
    /// both instances, for every event, so that most changes reach a watch.
    let private caseGen : Gen<Op list> =
        gen {
            let! opening = Gen.elements [ true ; false ]
            let! length = Gen.choose (10, 60)
            let! ops = Gen.listOfLength length opGen

            let watchAll =
                [
                    for instance in 0..1 do
                        for dir in dirs do
                            Op.AddWatch (instance, List.findIndex ((=) dir) paths, List.fold (|||) 0u eventBits, false)
                ]

            return (if opening then watchAll else []) @ ops
        }

    // ------------------------------------------------------------ the reference

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
    /// that asks for any of them, as one record each carrying all of them.
    let private notify
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
            if events &&& mask = 0u then
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
    let private apply (op : Op) (reference : Reference) : Reference * int option =
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
                { reference with
                    Tree = Map.remove removed tree
                }
                |> notifyParent removed delete node 0u,
                None
            | other -> failwith $"rmdir of %s{paths.[path]} succeeded on %A{other}"
        | Op.Unlink path ->
            match lookup paths.[path] false with
            | removed, Some node ->
                notifySelf attrib node reference
                |> fun reference ->
                    { reference with
                        Tree = Map.remove removed reference.Tree
                    }
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
    /// refusal, which changes nothing.
    let private call
        (fds : int list)
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
        | Op.Link (existing, i) -> UnixNamespace.tryLink (path existing) (path i) system |> unrefused
        | Op.Symlink (target, i) -> UnixNamespace.trySymlink (path target) (path i) system |> unrefused
        | Op.Rename (source, destination) ->
            UnixNamespace.tryRename (path source) (path destination) system |> unrefused
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
            UnixInotify.tryInotifyAddWatch
                fds.[instance]
                (path i)
                (mask ||| (if dontFollow then 0x02000000u else 0u))
                system
            |> unrefused
        | Op.RmWatch (instance, wd) -> UnixInotify.tryInotifyRmWatch fds.[instance] wd system |> unrefused

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

        ((system, initialReference), List.indexed ops)
        ||> List.fold (fun (system, reference) (step, op) ->
            cover Label.Call

            let system, reference =
                match call fds op system with
                | None -> system, reference
                | Some (SyscallAnswer.Failed _, after) -> after, reference
                | Some (SyscallAnswer.Completed answer, after) ->
                    let reference, wd = apply op reference

                    match wd with
                    | Some wd -> answer |> shouldEqual (int64 wd)
                    | None -> ()

                    after, reference

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

                    system
                )

            system,
            { reference with
                Queued = Map.ofList [ 0, [] ; 1, [] ]
            }
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
