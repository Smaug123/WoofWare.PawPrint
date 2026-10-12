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
/// inode's own.
[<RequireQualifiedAccess>]
type internal ReachedThrough =
    /// A path, whose walk reached the inode through the dentry; `None` at the
    /// root, and at the root of a mounted filesystem, which no directory's
    /// watch hears of (measured, `inotify-delivery-probe.c` row P
    /// chmod-mount-point).
    | Path of Dentry option
    /// A path that reached, by ".", ".." or "/", a directory whose last name
    /// has gone, last named in `oldParent`. Linux reports a change to it at
    /// `oldParent`'s watch under the name it had there (row P
    /// chmod-removed-current), which this kernel does not keep.
    | RemovedDirectory of oldParent : InodeNumber
    /// A descriptor: `write(2)`, `pwrite(2)`, `copy_file_range(2)`,
    /// `ftruncate(2)`, `fchmod(2)`, `fchown(2)`, `futimens(2)`, and the `*at`
    /// calls' empty path under `AT_EMPTY_PATH` with a descriptor. Linux reports
    /// a change through one at the watch on the directory of the name it was
    /// opened through, which this kernel does not record.
    | Descriptor

[<RequireQualifiedAccess>]
module internal ReachedThrough =

    /// How a path that ended at the directory `inode` by ".", ".." or "/"
    /// reached it: through its own name in its parent.
    let ofDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : ReachedThrough =
        if
            inode = VirtualFileSystem.root vfs
            || (VirtualFileSystem.mountOf inode vfs).IsSome
        then
            ReachedThrough.Path None
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
            ReachedThrough.Path (
                Some
                    {
                        Parent = directory.Parent
                        Name = name
                    }
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
            ReachedThrough.Path None
        | ResolvedTarget.Entry (directory, name, Some _) ->
            ReachedThrough.Path (
                Some
                    {
                        Parent = directory
                        Name = name
                    }
            )
        | ResolvedTarget.Entry (directory, name, None) ->
            failwith
                $"ReachedThrough.ofResolved: the walk ended at \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory}, which names nothing, so nothing was changed through it (this is a bug in this library)."
