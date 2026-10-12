namespace WoofWare.PosixKernel

/// Which of an inode's attributes a `setattr` set: what Linux's
/// `fsnotify_change` reads, of the `ia_valid` its caller passed, to choose the
/// event it raises.
type internal AttributesSet =
    {
        /// The mode, a set-ID bit cleared by a truncation or an owner change
        /// included.
        Mode : bool
        /// The owner or the group: a `chown(2)` naming either ID, even the one
        /// the inode has.
        Owner : bool
        /// The size: a truncation.
        Size : bool
        /// The access time.
        AccessTime : bool
        /// The modification time.
        ModificationTime : bool
    }

[<RequireQualifiedAccess>]
module internal AttributesSet =
    /// A `setattr` that set none of them, which changes only `ctime`.
    let none : AttributesSet =
        {
            Mode = false
            Owner = false
            Size = false
            AccessTime = false
            ModificationTime = false
        }

    /// The events Linux's `fsnotify_change` raises, as one record, for a
    /// `setattr` that set `set`: `IN_ATTRIB` for a mode or an owner, or both
    /// times; `IN_MODIFY` for a size, or the modification time alone;
    /// `IN_ACCESS` for the access time alone. None for a `setattr` that set
    /// none of them (measured, `inotify-delivery-probe.c` sections V, Z, C).
    let events (set : AttributesSet) : Set<InotifyEvent> =
        [
            if set.Mode || set.Owner then
                InotifyEvent.Attrib
            if set.Size then
                InotifyEvent.Modify
            match set.AccessTime, set.ModificationTime with
            | true, true -> InotifyEvent.Attrib
            | true, false -> InotifyEvent.Access
            | false, true -> InotifyEvent.Modify
            | false, false -> ()
        ]
        |> Set.ofList

/// How the syscall that made a `VfsMutation` reached what it changed, which
/// decides who hears of its changes (`CommittedFileSystem.commit`).
[<RequireQualifiedAccess>]
type internal ChangeNotice =
    /// A change of names: a creation, a link, an unlink, a `rmdir`, a rename,
    /// or an inode forgotten. Its `FsChange.AttributesChanged` are an inode's
    /// gained or lost links, and it has no `FsChange.ContentChanged`.
    | Names
    /// A `setattr` of one inode, which every change it holds is of, setting
    /// `set`, reached `through` a path or a descriptor: `chmod(2)`,
    /// `chown(2)`, `utimensat(2)`, a truncation. Its
    /// `FsChange.ContentChanged` is the size, and its
    /// `FsChange.AttributesChanged` the rest.
    | SetAttributes of through : ReachedThrough * set : AttributesSet
    /// A write through a descriptor: `write(2)`, `pwrite(2)` or
    /// `copy_file_range(2)`, of one inode, which every change it holds is of:
    /// its contents, and the set-ID bits it cleared.
    | Written

/// One event Linux's fsnotify raises at the watches on one inode: what each
/// inotify instance watching `Watched` queues as one record, if it asks for
/// any of `Events`.
type internal InotifyNotification =
    {
        /// The inode whose watches hear of the event: a directory, for an
        /// event about one of its entries, or the inode the event is about.
        Watched : InodeNumber
        Events : Set<InotifyEvent>
        /// `IN_ISDIR`: the inode the event is about is a directory.
        SubjectIsDirectory : bool
        Cookie : uint32
        /// The entry the event is about, or `None` for one about `Watched`
        /// itself.
        Name : DirectoryEntryName option
    }

/// Events Linux raises that this kernel cannot place, because it does not
/// keep the name the change went through: `CommittedFileSystem.commit`
/// refuses the change if a watch could hear of them.
[<RequireQualifiedAccess>]
type internal Unplaced =
    /// A change of `inode` through a descriptor, raising `events`: Linux
    /// reports them at the inode's own watch, and at the watch on the
    /// directory of the name the descriptor was opened through, which may
    /// since have been unlinked. `excludedWhenUnlinked` says Linux does not
    /// report them through an unlinked name to a watch with `IN_EXCL_UNLINK`:
    /// true of a write's `IN_MODIFY` (measured, `inotify-probe.c` row D
    /// write-unlinked-excl-unlink), and not of the `IN_ATTRIB` of the set-ID
    /// bits it clears, nor of any `setattr` (`inotify-delivery-probe.c` row Z
    /// write-unlinked-excl-unlink).
    | ThroughDescriptor of inode : InodeNumber * events : Set<InotifyEvent> * excludedWhenUnlinked : bool
    /// A change of a directory whose last name has gone, through a path,
    /// raising `events`, which Linux reports at `oldParent`'s watch under
    /// the name the directory had there.
    | AtOldParent of oldParent : InodeNumber * events : Set<InotifyEvent>

/// Why this kernel will not commit a filesystem change: Linux would report it
/// to an inotify watch through a name this kernel does not keep, and some
/// watch that could be that one asks for an event the change raises.
[<RequireQualifiedAccess>]
type internal UnplacedChangeRefusal =
    /// A change to `inode` through a descriptor, raising `events`, which
    /// `watch` of the instance `instance`, on `watched`, asks for: it is the
    /// watch on the inode itself, or on a directory that could hold the name
    /// the descriptor was opened through, which this kernel does not record.
    | DescriptorName of
        inode : InodeNumber *
        events : Set<InotifyEvent> *
        instance : OpenFileDescriptionId *
        watch : WatchDescriptor *
        watched : InodeNumber
    /// A change through a path to a directory whose last name has gone,
    /// raising `events`, which Linux reports to `watch` of the instance
    /// `instance`, on the directory it was last named in (`oldParent`), under
    /// the name it had there, which this kernel does not keep.
    | RemovedDirectoryName of
        oldParent : InodeNumber *
        events : Set<InotifyEvent> *
        instance : OpenFileDescriptionId *
        watch : WatchDescriptor

[<RequireQualifiedAccess>]
module internal UnplacedChangeRefusal =
    /// What this kernel knows about why it will not commit the change.
    let describe (refusal : UnplacedChangeRefusal) : string =
        match refusal with
        | UnplacedChangeRefusal.DescriptorName (inode, events, instance, watch, watched) ->
            $"a change to inode %O{inode} through a descriptor raises %A{Set.toList events}, which watch %O{watch} of the inotify instance %O{instance}, on inode %O{watched}, asks for and could be the watch Linux reports it to: the watch on the inode itself, or on the directory of the name the descriptor was opened through. This kernel does not record the name the descriptor was opened through, so it cannot say which watches Linux reports the change to."
        | UnplacedChangeRefusal.RemovedDirectoryName (oldParent, events, instance, watch) ->
            $"a change through a path to a directory whose last name has gone raises %A{Set.toList events}, which Linux reports to watch %O{watch} of the inotify instance %O{instance}, on the directory it was last named in (inode %O{oldParent}), under the name it had there. This kernel does not keep that name."

    /// Which exception a public entry point raises this refusal as.
    let kind (refusal : UnplacedChangeRefusal) : RefusalKind =
        match refusal with
        | UnplacedChangeRefusal.DescriptorName _ -> RefusalKind.Unmodelled
        | UnplacedChangeRefusal.RemovedDirectoryName _ -> RefusalKind.Unmodelled

/// The records a filesystem change queues on the inotify instances watching
/// it, in Linux's order (docs/plans/2026-10-11-inotify.md, 4.2 and 4.5).
[<RequireQualifiedAccess>]
module internal InotifyDelivery =

    let private isDirectoryIn (mutation : VfsMutation) (inode : InodeNumber) : bool =
        let ofGraph (vfs : VirtualFileSystem) : bool option =
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory _) -> Some true
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _)
            | Some (InodeContent.CharacterDevice _)
            | Some (InodeContent.Socket _)
            | Some (InodeContent.Fifo _) -> Some false
            | None -> None

        match ofGraph (VfsMutation.after mutation) with
        | Some isDirectory -> isDirectory
        | None ->

        match ofGraph (VfsMutation.before mutation) with
        | Some isDirectory -> isDirectory
        | None ->
            failwith
                $"InotifyDelivery: a change names inode %O{inode}, which neither the graph it was made from nor the one it made holds (this is a bug in this library)."

    /// The one inode every change of `changes` is about, and whether any
    /// changed its contents and any its attributes: the changes of a
    /// `ChangeNotice.SetAttributes` or `ChangeNotice.Written`.
    let private attributeChanges (context : string) (changes : FsChange list) : InodeNumber * bool * bool =
        let ofChange (change : FsChange) : InodeNumber * bool =
            match change with
            | FsChange.ContentChanged inode -> inode, true
            | FsChange.AttributesChanged inode -> inode, false
            | FsChange.EntryAdded _
            | FsChange.EntryRemoved _
            | FsChange.EntryMoved _
            | FsChange.InodeForgotten _ ->
                failwith
                    $"InotifyDelivery: %s{context} made %A{change}, which is no change of an inode's contents or attributes (this is a bug in this library)."

        match changes |> List.map ofChange with
        | [] -> failwith $"InotifyDelivery: %s{context} made no change (this is a bug in this library)."
        | (inode, _) :: _ as all ->
            if all |> List.exists (fun (other, _) -> other <> inode) then
                failwith
                    $"InotifyDelivery: %s{context} changed several inodes, %A{changes}, where it changes one (this is a bug in this library)."

            inode, List.exists snd all, List.exists (snd >> not) all

    /// One event, reported first to the watch on the directory `through` names,
    /// under its name, and then to the inode's own watch (measured,
    /// `inotify-delivery-probe.c` section Y).
    let private throughPath
        (mutation : VfsMutation)
        (through : Dentry option)
        (inode : InodeNumber)
        (events : Set<InotifyEvent>)
        : InotifyNotification list
        =
        let isDirectory = isDirectoryIn mutation inode

        let toParent =
            match through with
            | None -> []
            | Some dentry ->
                match VirtualFileSystem.tryGetDirectory dentry.Parent (VfsMutation.after mutation) with
                | Some parent when Map.tryFind dentry.Name parent.Entries = Some inode -> ()
                | _ ->
                    failwith
                        $"InotifyDelivery: the change to inode %O{inode} came through \"%s{DirectoryEntryName.toEscaped dentry.Name}\" in inode %O{dentry.Parent}, which does not name it (this is a bug in this library)."

                [
                    {
                        Watched = dentry.Parent
                        Events = events
                        SubjectIsDirectory = isDirectory
                        Cookie = 0u
                        Name = Some dentry.Name
                    }
                ]

        toParent
        @ [
            {
                Watched = inode
                Events = events
                SubjectIsDirectory = isDirectory
                Cookie = 0u
                Name = None
            }
        ]

    /// The events `mutation`, made under `notice`, raises, in the order Linux
    /// raises them; those this kernel cannot place (`Unplaced`); and the next
    /// rename's cookie, `nextCookie` being this one's.
    ///
    /// Every rename takes a cookie, whatever watches it (Linux's
    /// `fsnotify_get_cookie`, measured to count every rename since boot), and
    /// its two halves share it. The cookie after `UInt32.MaxValue` is 0, as
    /// Linux's counter wraps.
    let notifications
        (notice : ChangeNotice)
        (mutation : VfsMutation)
        (nextCookie : uint32)
        : InotifyNotification list * Unplaced list * uint32
        =
        let changes = VfsMutation.changes mutation
        let isDirectory = isDirectoryIn mutation

        let at (watched : InodeNumber) (event : InotifyEvent) (subject : InodeNumber) (cookie : uint32) name =
            {
                Watched = watched
                Events = Set.singleton event
                SubjectIsDirectory = isDirectory subject
                Cookie = cookie
                Name = name
            }

        match notice with
        | ChangeNotice.Names ->
            let reversed, cookie =
                (([], nextCookie), changes)
                ||> List.fold (fun (reversed, cookie) change ->
                    match change with
                    | FsChange.EntryAdded (directory, name, inode) ->
                        at directory InotifyEvent.Create inode 0u (Some name) :: reversed, cookie
                    | FsChange.EntryRemoved (directory, name, inode) ->
                        at directory InotifyEvent.Delete inode 0u (Some name) :: reversed, cookie
                    | FsChange.EntryMoved (source, sourceName, destination, destinationName, inode, displaced) ->
                        let moved =
                            [
                                at source InotifyEvent.MovedFrom inode cookie (Some sourceName)
                                at destination InotifyEvent.MovedTo inode cookie (Some destinationName)
                            ]

                        // Measured (`inotify-delivery-probe.c` section R): the
                        // displaced inode's lost link, at its own watch, after
                        // the two halves.
                        let lostLink =
                            match displaced with
                            | Some displaced -> [ at displaced InotifyEvent.Attrib displaced 0u None ]
                            | None -> []

                        List.rev (moved @ lostLink) @ reversed, cookie + 1u
                    // A link gained or lost: Linux's `fsnotify_link_count`,
                    // to the inode's own watch and no other (measured,
                    // `inotify-delivery-probe.c` section X).
                    | FsChange.AttributesChanged inode ->
                        at inode InotifyEvent.Attrib inode 0u None :: reversed, cookie
                    | FsChange.ContentChanged inode ->
                        failwith
                            $"InotifyDelivery: a change of names changed the contents of inode %O{inode} (this is a bug in this library)."
                    | FsChange.InodeForgotten _ -> reversed, cookie
                )

            List.rev reversed, [], cookie
        | ChangeNotice.SetAttributes (through, set) ->
            let inode, contents, attributes = attributeChanges "a setattr" changes

            if contents <> set.Size || (set.Mode || set.Owner) && not attributes then
                failwith $"InotifyDelivery: a setattr setting %A{set} made %A{changes} (this is a bug in this library)."

            let events = AttributesSet.events set

            if events.IsEmpty then
                [], [], nextCookie
            else

            match through with
            | ReachedThrough.Path through -> throughPath mutation through inode events, [], nextCookie
            // Linux reports the change at the old parent's watch, and then at
            // the directory's own; only the second can be placed.
            | ReachedThrough.RemovedDirectory oldParent ->
                throughPath mutation None inode events, [ Unplaced.AtOldParent (oldParent, events) ], nextCookie
            | ReachedThrough.Descriptor -> [], [ Unplaced.ThroughDescriptor (inode, events, false) ], nextCookie
        | ChangeNotice.Written ->
            // Linux's `file_remove_privs` raises `IN_ATTRIB` for the set-ID
            // bits a write clears, and the write `IN_MODIFY` (measured,
            // `inotify-delivery-probe.c` row Z write).
            let inode, contents, attributes = attributeChanges "a write" changes

            let unplaced =
                [
                    if attributes then
                        Unplaced.ThroughDescriptor (inode, Set.singleton InotifyEvent.Attrib, false)
                    if contents then
                        Unplaced.ThroughDescriptor (inode, Set.singleton InotifyEvent.Modify, true)
                ]

            [], unplaced, nextCookie

    /// Fails, naming the name this kernel does not keep, if some watch in
    /// `openFiles` asks for an event of `unplaced` and could be one Linux
    /// reports it to, in `graph`, the filesystem after the change.
    ///
    /// For a change through a descriptor, that is a watch on the inode itself,
    /// or on the directory of the name the descriptor was opened through: for
    /// a directory, its parent, whether or not it still names it; for any
    /// other inode, any directory, the name having perhaps been unlinked, but
    /// for a write's `IN_MODIFY` one with `IN_EXCL_UNLINK` that names it
    /// nowhere. The root
    /// and the root of a mounted filesystem are heard at their own watches
    /// alone. For a change of a removed directory through a path, it is a
    /// watch on its old parent.
    let refuseUnplaced (graph : VirtualFileSystem) (unplaced : Unplaced list) (openFiles : OpenFileTable) : unit =
        if not unplaced.IsEmpty then
            let watches =
                OpenFileTable.toSeq openFiles
                |> Seq.collect (fun (instance, description) ->
                    match description.Target with
                    | OpenFileTarget.Inotify state ->
                        state.Watches |> Map.toSeq |> Seq.map (fun (wd, watch) -> instance, wd, watch)
                    | OpenFileTarget.File _
                    | OpenFileTarget.Directory _
                    | OpenFileTarget.Epoll _
                    | OpenFileTarget.Kqueue _
                    | OpenFileTarget.Socket _
                    | OpenFileTarget.CharacterDevice _
                    | OpenFileTarget.Fifo _
                    | OpenFileTarget.Pipe _ -> Seq.empty
                )
                |> List.ofSeq

            let asking (events : Set<InotifyEvent>) =
                watches
                |> List.filter (fun (_, _, watch) -> not (Set.intersect events watch.Mask.Events).IsEmpty)

            let names (directory : InodeNumber) (inode : InodeNumber) : bool =
                match VirtualFileSystem.tryGetDirectory directory graph with
                | Some content -> content.Entries |> Map.exists (fun _ bound -> bound = inode)
                | None -> false

            for unplaced in unplaced do
                match unplaced with
                | Unplaced.ThroughDescriptor (inode, events, excludedWhenUnlinked) ->
                    let unnamed =
                        inode = VirtualFileSystem.root graph
                        || (VirtualFileSystem.mountOf inode graph).IsSome

                    let hears (watch : InotifyWatch) : bool =
                        watch.Inode = inode
                        || not unnamed
                           && (
                               match VirtualFileSystem.tryGetDirectory inode graph with
                               | Some directory -> watch.Inode = directory.Parent
                               | None ->
                                   (VirtualFileSystem.tryGetDirectory watch.Inode graph).IsSome
                                   && (names watch.Inode inode
                                       || not (excludedWhenUnlinked && watch.Mask.ExcludeUnlinked))
                           )

                    match asking events |> List.tryFind (fun (_, _, watch) -> hears watch) with
                    | None -> ()
                    | Some (instance, wd, watch) ->
                        failwith
                            $"CommittedFileSystem.commit: a change to inode %O{inode} through a descriptor raises %A{Set.toList events}, which watch %O{wd} of the inotify instance %O{instance}, on inode %O{watch.Inode}, asks for and could be the watch Linux reports it to: the watch on the inode itself, or on the directory of the name the descriptor was opened through. This kernel does not record the name the descriptor was opened through, so it refuses the change rather than report it to the wrong watches."
                | Unplaced.AtOldParent (oldParent, events) ->
                    match asking events |> List.tryFind (fun (_, _, watch) -> watch.Inode = oldParent) with
                    | None -> ()
                    | Some (instance, wd, _) ->
                        failwith
                            $"CommittedFileSystem.commit: a change through a path to a directory whose last name has gone raises %A{Set.toList events}, which Linux reports to watch %O{wd} of the inotify instance %O{instance}, on the directory it was last named in (inode %O{oldParent}), under the name it had there. This kernel does not keep that name, so it refuses the change rather than report it wrongly."

    /// `openFiles` with each of `notifications` queued, in order, on every
    /// inotify instance that watches its inode and asks for any of its events:
    /// one record carrying every one of its events, those the watch did not
    /// ask for included (`OpenFileTable.enqueueInotify`). Measured
    /// (`inotify-delivery-probe.c` rows Z open-trunc-modify-watch and
    /// open-trunc-attrib-watch): a truncation that clears set-ID bits queues
    /// `MODIFY+ATTRIB` under a watch asking for either alone. Instances hear
    /// of each notification in the order of their descriptions' IDs.
    let deliver
        (maxQueuedEvents : int)
        (notifications : InotifyNotification list)
        (openFiles : OpenFileTable)
        : OpenFileTable
        =
        if List.isEmpty notifications then
            openFiles
        else

        // Queueing a record changes no watch, so the watches are read once.
        let instances =
            OpenFileTable.toSeq openFiles
            |> Seq.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Inotify state ->
                    let byInode =
                        state.Watches
                        |> Map.toSeq
                        |> Seq.map (fun (watch, watched) -> watched.Inode, (watch, watched.Mask))
                        |> Map.ofSeq

                    Some (id, byInode)
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Fifo _
                | OpenFileTarget.Pipe _ -> None
            )
            |> List.ofSeq

        (openFiles, notifications)
        ||> List.fold (fun table notification ->
            (table, instances)
            ||> List.fold (fun table (instance, watches) ->
                match Map.tryFind notification.Watched watches with
                | None -> table
                | Some (watch, mask) ->

                if Set.intersect notification.Events mask.Events |> Set.isEmpty then
                    table
                else
                    let record =
                        InotifyRecord.Event (
                            watch,
                            notification.Events,
                            notification.SubjectIsDirectory,
                            notification.Cookie,
                            notification.Name
                        )

                    OpenFileTable.enqueueInotify maxQueuedEvents instance record table
            )
        )
