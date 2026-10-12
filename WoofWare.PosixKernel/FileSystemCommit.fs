namespace WoofWare.PosixKernel

/// The filesystem a machine holds (`UnixMachineState.CommittedFileSystem`),
/// and the cookie the next rename's inotify records share.
///
/// Its representation is private to this file, so only two functions make
/// one: `CommittedFileSystem.initial`, which boot calls, and
/// `CommittedFileSystem.commit`, which every syscall that changes the
/// filesystem calls. A graph `commit` stores is the `After` of a
/// `VfsMutation`, which only a `VirtualFileSystem` mutator makes, so `commit`
/// sees the changes that mutator reported, and reports them to every inotify
/// instance watching what they changed.
type internal CommittedFileSystem =
    private
        {
            Graph : VirtualFileSystem
            NextCookie : uint32
        }

[<RequireQualifiedAccess>]
module internal CommittedFileSystem =

    /// The inode graph.
    let graph (committed : CommittedFileSystem) : VirtualFileSystem = committed.Graph

    /// A filesystem no syscall has changed: the graph a machine boots with
    /// (`UnixSystem.initial`, `UnixBootImage.withFileSystem`), or one a test
    /// forges a machine around. A syscall's change goes through `commit`.
    let initial (graph : VirtualFileSystem) : CommittedFileSystem =
        {
            Graph = graph
            NextCookie = 1u
        }

    /// The filesystem after `mutation`, which must have been made from
    /// `committed`'s own graph, and `openFiles` with the records its changes
    /// raise queued on every inotify instance watching what they changed,
    /// each instance's limit being `maxQueuedEvents` records
    /// (`InotifyDelivery`), each watch on an inode that went for inotify
    /// ended, and each description's record of the name it was opened
    /// through moved or marked unlinked by them
    /// (`OpenFileTable.followChanges`). `notice` says how the syscall reached
    /// what it changed, or that a description let go of a name
    /// (`ChangeNotice.NameReleased`, with `VirtualFileSystem.unchanged`).
    ///
    /// Refused, changing nothing, where Linux would report a change to a
    /// watch through a name this kernel does not keep: the name a removed
    /// directory had, for a change that reached it by ".", ".." or "/"
    /// (`ReachedThrough.RemovedDirectory`, `UnplacedChangeRefusal`), when a
    /// watch on its old parent asks for an event the change raises. Any other
    /// change is committed.
    let commit
        (notice : ChangeNotice)
        (maxQueuedEvents : int)
        (mutation : VfsMutation)
        (openFiles : OpenFileTable)
        (committed : CommittedFileSystem)
        : Result<CommittedFileSystem * OpenFileTable, UnplacedChangeRefusal>
        =
        // By reference rather than by value: the graph a mutator was handed is
        // the very object this holds, and comparing two maps of every inode
        // would cost a walk of the filesystem per syscall.
        if not (LanguagePrimitives.PhysicalEquality (VfsMutation.before mutation) committed.Graph) then
            failwith
                "CommittedFileSystem.commit: the mutation was made from a graph other than the one this machine holds, so committing it would lose every change between the two (this is a bug in the caller)."

        let effects, unplaced, nextCookie =
            InotifyDelivery.notifications notice mutation openFiles committed.NextCookie

        match InotifyDelivery.unplacedRefusal unplaced openFiles with
        | Some refusal -> Error refusal
        | None ->

        Ok (
            {
                Graph = VfsMutation.after mutation
                NextCookie = nextCookie
            },
            InotifyDelivery.deliver maxQueuedEvents effects openFiles
            |> OpenFileTable.followChanges (VfsMutation.changes mutation)
        )
