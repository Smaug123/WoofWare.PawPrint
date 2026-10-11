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

/// How a syscall reached the inode whose attributes it changed.
[<RequireQualifiedAccess>]
type internal ReachedThrough =
    /// A path, whose walk reached the inode through the dentry
    /// (`Dentry.ofResolved`).
    | Path of Dentry option
    /// A descriptor: `ftruncate(2)`, `fchmod(2)`, `fchown(2)`, `futimens(2)`,
    /// and the `*at` calls' empty path under `AT_EMPTY_PATH` with a
    /// descriptor. This kernel reports a change made through one to no watch.
    | Descriptor

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
    /// `copy_file_range(2)`, of one inode, which every change it holds is of.
    /// This kernel reports it to no watch.
    | Written

/// One event Linux's fsnotify raises at the watches on one inode: what each
/// inotify instance watching `Watched` queues, of the events it asks for, as
/// one record.
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
    /// raises them, and the next rename's cookie, `nextCookie` being this
    /// one's.
    ///
    /// Every rename takes a cookie, whatever watches it (Linux's
    /// `fsnotify_get_cookie`, measured to count every rename since boot), and
    /// its two halves share it. The cookie after `UInt32.MaxValue` is 0, as
    /// Linux's counter wraps.
    let notifications
        (notice : ChangeNotice)
        (mutation : VfsMutation)
        (nextCookie : uint32)
        : InotifyNotification list * uint32
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

            List.rev reversed, cookie
        | ChangeNotice.SetAttributes (through, set) ->
            let inode, contents, attributes = attributeChanges "a setattr" changes

            if contents <> set.Size || (set.Mode || set.Owner) && not attributes then
                failwith $"InotifyDelivery: a setattr setting %A{set} made %A{changes} (this is a bug in this library)."

            let events = AttributesSet.events set

            match through with
            | ReachedThrough.Path through when not events.IsEmpty ->
                throughPath mutation through inode events, nextCookie
            | ReachedThrough.Path _
            | ReachedThrough.Descriptor -> [], nextCookie
        | ChangeNotice.Written ->
            attributeChanges "a write" changes |> ignore<InodeNumber * bool * bool>
            [], nextCookie

    /// `openFiles` with each of `notifications` queued, in order, on every
    /// inotify instance that watches its inode and asks for any of its events:
    /// one record of the events it asks for (`OpenFileTable.enqueueInotify`).
    /// Instances hear of each notification in the order of their descriptions'
    /// IDs.
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

                let asked = Set.intersect notification.Events mask.Events

                if asked.IsEmpty then
                    table
                else
                    let record =
                        InotifyRecord.Event (
                            watch,
                            asked,
                            notification.SubjectIsDirectory,
                            notification.Cookie,
                            notification.Name
                        )

                    OpenFileTable.enqueueInotify maxQueuedEvents instance record table
            )
        )
