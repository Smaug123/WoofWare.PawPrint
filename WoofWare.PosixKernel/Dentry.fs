namespace WoofWare.PosixKernel

/// The name a syscall reached an inode through: the directory holding it, and
/// the name it has there. A change to the inode's contents or attributes is
/// reported to a watch on that directory under that name.
type internal Dentry =
    {
        Parent : InodeNumber
        Name : DirectoryEntryName
    }

[<RequireQualifiedAccess>]
module internal Dentry =

    /// The name the directory `inode` has in its parent: `None` for the root,
    /// for the root of a mounted filesystem, which is no entry of the
    /// filesystem it is mounted on, and for a directory whose last name has
    /// gone.
    let ofDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : Dentry option =
        if
            inode = VirtualFileSystem.root vfs
            || (VirtualFileSystem.mountOf inode vfs).IsSome
        then
            None
        else

        match VirtualFileSystem.tryGetDirectory inode vfs with
        | None ->
            failwith
                $"Dentry.ofDirectory: inode %O{inode} is not a directory this filesystem holds (this is a bug in this library)."
        | Some directory ->

        // Linux keeps a removed directory's name, and names it to the watch
        // on its old parent (`inotify-delivery-probe.c` row P
        // chmod-removed-current); this graph keeps no name for it.
        VirtualFileSystem.tryGetDirectory directory.Parent vfs
        |> Option.bind (fun parent -> parent.Entries |> Map.tryFindKey (fun _ bound -> bound = inode))
        |> Option.map (fun name ->
            {
                Parent = directory.Parent
                Name = name
            }
        )

    /// The name a path walk that ended at `target` reached its inode through:
    /// the final name it looked up, or, for a path that ended in ".", ".." or
    /// "/" at a directory, that directory's own name (`ofDirectory`). `None`
    /// when the walk ended at the root, or at the root of a mounted
    /// filesystem: measured (`inotify-delivery-probe.c` row P
    /// chmod-mount-point), a change there is reported to no watch on the
    /// directory the mount covers. A target naming nothing has no dentry to
    /// report a change through, and is refused loudly.
    let ofResolved (target : ResolvedTarget) (vfs : VirtualFileSystem) : Dentry option =
        match target with
        | ResolvedTarget.Directory (inode, _) -> ofDirectory inode vfs
        | ResolvedTarget.Entry (_, _, Some inode) when (VirtualFileSystem.mountOf inode vfs).IsSome -> None
        | ResolvedTarget.Entry (directory, name, Some _) ->
            Some
                {
                    Parent = directory
                    Name = name
                }
        | ResolvedTarget.Entry (directory, name, None) ->
            failwith
                $"Dentry.ofResolved: the walk ended at \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory}, which names nothing, so nothing was changed through it (this is a bug in this library)."
