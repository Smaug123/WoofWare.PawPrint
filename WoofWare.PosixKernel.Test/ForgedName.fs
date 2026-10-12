namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// The name a test that makes an open file description onto an inode
/// directly, rather than through `open(2)`, records it as opened through
/// (`OpenFileTable.openedThrough`), so that the description agrees with the
/// filesystem as `UnixSystem.checkInvariants` holds it to.
[<RequireQualifiedAccess>]
module internal ForgedName =

    let private unlinked : DirectoryEntryName =
        DirectoryEntryName.parseOrFail "ForgedName" "forged-unlinked-name"

    /// What a walk to `inode` in `vfs` would have reached it through: no name
    /// for the root or a mounted filesystem's root, a directory's own name or
    /// its removal (`ReachedThrough.ofDirectory`), or the first name some
    /// directory binds to anything else; and, for an inode no directory names,
    /// a name in the root since unlinked.
    let inVfs (vfs : VirtualFileSystem) (inode : InodeNumber) : ReachedThrough =
        match VirtualFileSystem.tryGetDirectory inode vfs with
        | Some _ -> ReachedThrough.ofDirectory inode vfs
        | None ->

        let named =
            VirtualFileSystem.inodes vfs
            |> Map.toSeq
            |> Seq.tryPick (fun (directory, entry) ->
                match entry.Content with
                | InodeContent.Directory content ->
                    content.Entries
                    |> Map.tryFindKey (fun _ bound -> bound = inode)
                    |> Option.map (fun name -> ReachedThrough.ofEntry directory name)
                | InodeContent.RegularFile _
                | InodeContent.Symlink _
                | InodeContent.CharacterDevice _
                | InodeContent.Socket _
                | InodeContent.Fifo _ -> None
            )

        match named with
        | Some through -> through
        | None ->
            ReachedThrough.Name (
                {
                    Parent = VirtualFileSystem.root vfs
                    Name = unlinked
                },
                true
            )

    /// `inVfs`, of `system`'s filesystem.
    let ofInode<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        (inode : InodeNumber)
        : ReachedThrough
        =
        inVfs (UnixSystem.fileSystem system) inode
