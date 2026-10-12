namespace WoofWare.PosixKernel

/// The name a syscall reached an inode through: the directory holding it, and
/// the name it has there. A change to the inode's contents or attributes is
/// reported to a watch on that directory under that name.
type internal Dentry =
    {
        Parent : InodeNumber
        Name : DirectoryEntryName
    }

/// How a syscall reached the inode whose contents or attributes it changed,
/// which decides which directory's watch hears of the change beside the
/// inode's own: a path's walk, or the walk that opened the descriptor the
/// syscall was given, which the open file description records as Linux's
/// `struct file` keeps its dentry (`OpenFileTable.openedThrough`).
[<RequireQualifiedAccess>]
type internal ReachedThrough =
    /// The name `dentry`. A walk always reaches a name not `unlinked`. A
    /// description's record of one follows the graph: `rename` of exactly
    /// that name moves it (Linux's `d_move`), and an unlink of it, a `rmdir`
    /// of it, or a rename displacing the inode from it marks it `unlinked`,
    /// keeping the directory and the name it had, which Linux still reports
    /// a change under (measured, `inotify-probe.c` section H,
    /// `inotify-dentry-probe.c` sections O and Q). A name linked or renamed
    /// into that place afterwards is another name, and leaves it unlinked
    /// (`inotify-dentry-probe.c` section L).
    | Name of dentry : Dentry * unlinked : bool
    /// The root, or the root of a mounted filesystem, which no directory's
    /// watch hears of (measured, `inotify-delivery-probe.c` row P
    /// chmod-mount-point).
    | Unnamed
    /// A directory whose last name had gone when a walk reached it by ".",
    /// ".." or "/", last named in `oldParent`. Linux reports a change to it
    /// at `oldParent`'s watch under the name it had there (row P
    /// chmod-removed-current), which this kernel does not keep.
    | RemovedDirectory of oldParent : InodeNumber

[<RequireQualifiedAccess>]
module internal ReachedThrough =

    /// How a path that ended at the directory `inode` by ".", ".." or "/"
    /// reached it: through its own name in its parent.
    let ofDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : ReachedThrough =
        if
            inode = VirtualFileSystem.root vfs
            || (VirtualFileSystem.mountOf inode vfs).IsSome
        then
            ReachedThrough.Unnamed
        else

        match VirtualFileSystem.tryGetDirectory inode vfs with
        | None ->
            failwith
                $"ReachedThrough.ofDirectory: inode %O{inode} is not a directory this filesystem holds (this is a bug in this library)."
        | Some directory ->

        let name =
            VirtualFileSystem.tryGetDirectory directory.Parent vfs
            |> Option.bind (fun parent -> parent.Entries |> Map.tryFindKey (fun _ bound -> bound = inode))

        match name with
        | Some name ->
            ReachedThrough.Name (
                {
                    Parent = directory.Parent
                    Name = name
                },
                false
            )
        | None -> ReachedThrough.RemovedDirectory directory.Parent

    /// How a path walk that ended at `target` reached its inode: through the
    /// final name it looked up, or, for a path that ended in ".", ".." or "/"
    /// at a directory, through that directory's own name (`ofDirectory`). A
    /// target naming nothing reached nothing, and is refused loudly.
    let ofResolved (target : ResolvedTarget) (vfs : VirtualFileSystem) : ReachedThrough =
        match target with
        | ResolvedTarget.Directory (inode, _) -> ofDirectory inode vfs
        | ResolvedTarget.Entry (_, _, Some inode) when (VirtualFileSystem.mountOf inode vfs).IsSome ->
            ReachedThrough.Unnamed
        | ResolvedTarget.Entry (directory, name, Some _) ->
            ReachedThrough.Name (
                {
                    Parent = directory
                    Name = name
                },
                false
            )
        | ResolvedTarget.Entry (directory, name, None) ->
            failwith
                $"ReachedThrough.ofResolved: the walk ended at \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory}, which names nothing, so nothing was changed through it (this is a bug in this library)."

    /// The name `name` in `directory`, which a walk has just reached or made.
    let ofEntry (directory : InodeNumber) (name : DirectoryEntryName) : ReachedThrough =
        ReachedThrough.Name (
            {
                Parent = directory
                Name = name
            },
            false
        )

    /// `through`, recorded on a description onto `inode`, after `change`: a
    /// name `rename` moved, or one an unlink, a `rmdir` or a rename displacing
    /// `inode` took away, which is then unlinked. A name already unlinked is
    /// another name than the one now in its place, and stays as it is.
    let followChange (inode : InodeNumber) (change : FsChange) (through : ReachedThrough) : ReachedThrough =
        match through with
        | ReachedThrough.Unnamed
        | ReachedThrough.RemovedDirectory _
        | ReachedThrough.Name (_, true) -> through
        | ReachedThrough.Name (dentry, false) ->

        let names (directory : InodeNumber) (name : DirectoryEntryName) =
            dentry.Parent = directory && dentry.Name = name

        match change with
        | FsChange.EntryRemoved (directory, name, removed) when removed = inode && names directory name ->
            ReachedThrough.Name (dentry, true)
        | FsChange.EntryMoved (_, _, destination, destinationName, _, Some displaced) when
            displaced = inode && names destination destinationName
            ->
            ReachedThrough.Name (dentry, true)
        | FsChange.EntryMoved (source, sourceName, destination, destinationName, moved, _) when
            moved = inode && names source sourceName
            ->
            ReachedThrough.Name (
                {
                    Parent = destination
                    Name = destinationName
                },
                false
            )
        | FsChange.EntryAdded _
        | FsChange.EntryRemoved _
        | FsChange.EntryMoved _
        | FsChange.ContentChanged _
        | FsChange.AttributesChanged _
        | FsChange.InodeForgotten _ -> through
