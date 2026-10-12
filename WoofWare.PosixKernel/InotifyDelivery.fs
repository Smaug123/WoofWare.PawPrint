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
    /// `set`, reached `through` a path, or through the name a descriptor was
    /// opened through: `chmod(2)`, `chown(2)`, `utimensat(2)`, a truncation,
    /// and their descriptor forms. Its `FsChange.ContentChanged` is the size,
    /// and its `FsChange.AttributesChanged` the rest.
    | SetAttributes of through : ReachedThrough * set : AttributesSet
    /// A write through a descriptor opened `through` a name: `write(2)`,
    /// `pwrite(2)` or `copy_file_range(2)`, of one regular file, which every
    /// change it holds is of: its contents, and the set-ID bits it cleared.
    | Written of through : ReachedThrough

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
        /// A watch with `IN_EXCL_UNLINK` hears nothing of it: an event Linux
        /// raises with the path the change went through, whose name has been
        /// unlinked since the descriptor was opened through it. That is a
        /// write's `IN_MODIFY`, at the directory's watch and the inode's own,
        /// and not a `setattr`'s, nor the `IN_ATTRIB` of the set-ID bits a
        /// write clears (measured, `inotify-dentry-probe.c` section E,
        /// `inotify-delivery-probe.c` row Z write-unlinked-excl-unlink).
        HiddenByExclUnlink : bool
    }

/// Events Linux raises at the watch on the directory a removed directory was
/// last named in, `OldParent`, under the name it had there, which this kernel
/// does not keep: `CommittedFileSystem.commit` refuses the change if a watch
/// there asks for any of `Events` (`UnplacedChangeRefusal`).
type internal Unplaced =
    {
        OldParent : InodeNumber
        Events : Set<InotifyEvent>
    }

/// Why this kernel will not commit a filesystem change: Linux would report it
/// to an inotify watch through a name this kernel does not keep, and some
/// watch that could be that one asks for an event the change raises.
[<RequireQualifiedAccess>]
type internal UnplacedChangeRefusal =
    /// A change to a directory whose last name had gone when the walk that
    /// reached it, or that opened the descriptor that did, reached it by ".",
    /// "..", or "/", raising `events`, which Linux reports to `watch` of the
    /// instance `instance`, on the directory it was last named in
    /// (`oldParent`), under the name it had there, which this kernel does not
    /// keep.
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
        | UnplacedChangeRefusal.RemovedDirectoryName (oldParent, events, instance, watch) ->
            $"a change to a directory reached by \".\", \"..\" or \"/\" after its last name had gone raises %A{Set.toList events}, which Linux reports to watch %O{watch} of the inotify instance %O{instance}, on the directory it was last named in (inode %O{oldParent}), under the name it had there. This kernel does not keep that name."

    /// Which exception a public entry point raises this refusal as.
    let kind (refusal : UnplacedChangeRefusal) : RefusalKind =
        match refusal with
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

    /// One event, reported first to the watch on the directory of the name
    /// the change went `through`, under that name, and then to the inode's
    /// own watch (measured, `inotify-delivery-probe.c` section Y); and the
    /// event Linux reports at a removed directory's old parent, which this
    /// kernel cannot place. `hiddenWhenUnlinked` says a watch with
    /// `IN_EXCL_UNLINK` hears nothing of it when that name has been unlinked.
    let private reached
        (mutation : VfsMutation)
        (through : ReachedThrough)
        (inode : InodeNumber)
        (events : Set<InotifyEvent>)
        (hiddenWhenUnlinked : bool)
        : InotifyNotification list * Unplaced option
        =
        let isDirectory = isDirectoryIn mutation inode
        let after = VfsMutation.after mutation

        let at (watched : InodeNumber) (name : DirectoryEntryName option) (hidden : bool) =
            {
                Watched = watched
                Events = events
                SubjectIsDirectory = isDirectory
                Cookie = 0u
                Name = name
                HiddenByExclUnlink = hidden
            }

        match through with
        | ReachedThrough.Name (dentry, unlinked) ->
            match VirtualFileSystem.tryGetDirectory dentry.Parent after with
            | Some parent when unlinked || Map.tryFind dentry.Name parent.Entries = Some inode -> ()
            | _ ->
                failwith
                    $"InotifyDelivery: the change to inode %O{inode} came through \"%s{DirectoryEntryName.toEscaped dentry.Name}\" in inode %O{dentry.Parent} (unlinked: %b{unlinked}), which is not a directory naming it, or, unlinked, not a directory at all (this is a bug in this library)."

            let hidden = unlinked && hiddenWhenUnlinked
            [ at dentry.Parent (Some dentry.Name) hidden ; at inode None hidden ], None
        | ReachedThrough.Unnamed -> [ at inode None false ], None
        // Linux reports the change at the old parent's watch, and then at
        // the directory's own; only the second can be placed.
        | ReachedThrough.RemovedDirectory oldParent ->
            [ at inode None false ],
            Some
                {
                    OldParent = oldParent
                    Events = events
                }

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
        : InotifyNotification list * Unplaced option * uint32
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
                HiddenByExclUnlink = false
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

            List.rev reversed, None, cookie
        | ChangeNotice.SetAttributes (through, set) ->
            let inode, contents, attributes = attributeChanges "a setattr" changes

            if contents <> set.Size || (set.Mode || set.Owner) && not attributes then
                failwith $"InotifyDelivery: a setattr setting %A{set} made %A{changes} (this is a bug in this library)."

            let events = AttributesSet.events set

            if events.IsEmpty then
                [], None, nextCookie
            else

            // A `setattr` carries no path, so no watch's `IN_EXCL_UNLINK`
            // keeps it out (measured, `inotify-dentry-probe.c` section E).
            let notifications, unplaced = reached mutation through inode events false
            notifications, unplaced, nextCookie
        | ChangeNotice.Written through ->
            // Linux's `file_remove_privs` raises `IN_ATTRIB` for the set-ID
            // bits a write clears, and the write `IN_MODIFY` (measured,
            // `inotify-delivery-probe.c` row Z write).
            let inode, contents, attributes = attributeChanges "a write" changes

            match through with
            | ReachedThrough.Name _ -> ()
            | ReachedThrough.Unnamed
            | ReachedThrough.RemovedDirectory _ ->
                failwith
                    $"InotifyDelivery: a write to inode %O{inode} came through %A{through}, where a write is of a regular file, which a descriptor is opened onto by a name (this is a bug in this library)."

            let cleared =
                if attributes then
                    fst (reached mutation through inode (Set.singleton InotifyEvent.Attrib) false)
                else
                    []

            let written =
                if contents then
                    fst (reached mutation through inode (Set.singleton InotifyEvent.Modify) true)
                else
                    []

            cleared @ written, None, nextCookie

    /// The refusal of the change that raised `unplaced`, if some watch in
    /// `openFiles` on its old parent asks for an event of it: the first such,
    /// in the order of the instances' IDs.
    let unplacedRefusal (unplaced : Unplaced option) (openFiles : OpenFileTable) : UnplacedChangeRefusal option =
        match unplaced with
        | None -> None
        | Some unplaced ->

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
        |> Seq.tryFind (fun (_, _, watch) ->
            watch.Inode = unplaced.OldParent
            && not (Set.intersect unplaced.Events watch.Mask.Events).IsEmpty
        )
        |> Option.map (fun (instance, wd, _) ->
            UnplacedChangeRefusal.RemovedDirectoryName (unplaced.OldParent, unplaced.Events, instance, wd)
        )

    /// `openFiles` with each of `notifications` queued, in order, on every
    /// inotify instance that watches its inode and asks for any of its events,
    /// but one whose watch has `IN_EXCL_UNLINK` where the notification is
    /// `HiddenByExclUnlink`: one record carrying every one of its events,
    /// those the watch did not ask for included (`OpenFileTable.enqueueInotify`). Measured
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

                if
                    Set.intersect notification.Events mask.Events |> Set.isEmpty
                    || notification.HiddenByExclUnlink && mask.ExcludeUnlinked
                then
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
