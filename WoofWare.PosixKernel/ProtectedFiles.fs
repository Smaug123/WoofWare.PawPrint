namespace WoofWare.PosixKernel

/// Linux's `fs.protected_symlinks` sysctl: which symbolic links a path walk
/// refuses to follow.
///
/// Linux admits only 0 and 1 for it; Darwin has no such setting, and behaves
/// as `Off`.
[<RequireQualifiedAccess>]
type SymlinkProtection =
    /// 0, the kernel's own default: a walk follows any link it may search its
    /// way to.
    | Off
    /// 1: a walk refuses with EACCES to follow a link in the final position of
    /// a path when the directory holding the link is sticky and world-writable,
    /// unless the follower owns the link or the directory's owner does.
    ///
    /// Many distributions set this through `sysctl.d`.
    | InWorldWritableStickyDirectories

/// Linux's `fs.protected_regular` and `fs.protected_fifos` sysctls: which
/// existing files (or FIFOs) an `open(2)` with `O_CREAT` refuses to open in a
/// sticky directory.
///
/// Linux admits only 0, 1 and 2 for each; Darwin has no such settings, and
/// behaves as `Off`.
[<RequireQualifiedAccess>]
type CreationProtection =
    /// 0, the kernel's own default: no such open is refused for who owns what.
    | Off
    /// 1: an `O_CREAT` open of an existing object in a sticky, world-writable
    /// directory is refused with EACCES unless the caller owns the object or
    /// the directory's owner does.
    | InWorldWritableStickyDirectories
    /// 2: as for 1, and in a sticky directory that is group-writable but not
    /// world-writable as well.
    | InGroupOrWorldWritableStickyDirectories

/// Linux's `fs.protected_hardlinks` sysctl: which inodes a caller that does
/// not own them may hard-link.
///
/// Linux admits only 0 and 1 for it; Darwin has no such setting, and behaves
/// as `Off`.
[<RequireQualifiedAccess>]
type HardlinkProtection =
    /// 0, the kernel's own default: a caller may link any inode it may reach.
    | Off
    /// 1: a caller that does not own a regular file may link it only if it may
    /// both read and write it, and `link(2)` answers EPERM otherwise.
    ///
    /// Many distributions set this through `sysctl.d`.
    // Measured by `link-symlink.c` (LINKPERM, protected_hardlinks=1) on Linux
    // 6.18.5: as uid 1000, root's 0600 and 0644 files are EPERM and its 0666
    // file links. What the setting does to set-ID files and to other kinds of
    // inode is unmeasured, and is `link(2)`'s to measure.
    | NonOwnersNeedReadAndWrite

/// Linux's `fs.protected_*` sysctls that decide what a caller may do with
/// another user's files: machine configuration rather than a fact of the
/// kernel, since an administrator sets them.
type ProtectedFiles =
    {
        /// `fs.protected_symlinks`.
        Symlinks : SymlinkProtection
        /// `fs.protected_regular`.
        RegularFiles : CreationProtection
        /// `fs.protected_fifos`.
        ///
        /// No inode this library models is a FIFO, so this decides nothing yet.
        Fifos : CreationProtection
        /// `fs.protected_hardlinks`.
        ///
        /// This library models no `link(2)`, so this decides nothing yet.
        Hardlinks : HardlinkProtection
    }

[<RequireQualifiedAccess>]
module ProtectedFiles =
    let private groupWritable : int = 0o0020
    let private worldWritable : int = 0o0002
    let private stickyWorldWritable : int = PermissionBits.sticky ||| worldWritable

    /// Every one of the sysctls 0: Linux's own default, and how Darwin, which
    /// has none of them, behaves.
    let off : ProtectedFiles =
        {
            Symlinks = SymlinkProtection.Off
            RegularFiles = CreationProtection.Off
            Fifos = CreationProtection.Off
            Hardlinks = HardlinkProtection.Off
        }

    /// Whether a walk on behalf of `credentials`, following a symbolic link
    /// owned by `linkOwner` from the final position of a path, may not follow
    /// it, the link being in a directory owned by `directoryOwner` with the
    /// permission bits `directoryPermissions`.
    ///
    /// The effective user decides it, and privilege does not exempt a caller:
    /// root is refused like anyone else.
    let refusesToFollow
        (protection : SymlinkProtection)
        (credentials : Credentials)
        (directoryOwner : InodeOwner)
        (directoryPermissions : PermissionBits)
        (linkOwner : InodeOwner)
        : bool
        =
        // Measured on Linux 6.18.5 (`protected-sysctls.c`, SYMLINKS rows): each
        // value, every combination of the sticky, group-write and other-write
        // bits on a 0755 directory, and every relation between the directory's
        // owner, the link's owner and the follower among root, 1000, 1001 and
        // 1002. Group write never matters, and neither does privilege.
        match protection with
        | SymlinkProtection.Off -> false
        | SymlinkProtection.InWorldWritableStickyDirectories ->
            PermissionBits.toInt directoryPermissions &&& stickyWorldWritable = stickyWorldWritable
            && not (Standing.toward credentials linkOwner).Owns
            && linkOwner.User <> directoryOwner.User

    /// Whether an `open(2)` with `O_CREAT` on behalf of `credentials`, landing
    /// on the existing inode `existing` in a directory owned by
    /// `directoryOwner` with the permission bits `directoryPermissions`, is
    /// refused because of who owns them: Linux's screen of a sticky
    /// directory's entries.
    ///
    /// A regular file is screened as `protection.RegularFiles` says. Any other
    /// inode (a symbolic link, which only `O_NOFOLLOW` leaves unfollowed, or a
    /// directory) is screened whatever the sysctls say, as
    /// `CreationProtection.InWorldWritableStickyDirectories` would screen it.
    /// The effective user decides it, and privilege does not exempt a caller.
    let refusesCreatingOpen
        (protection : ProtectedFiles)
        (credentials : Credentials)
        (directoryOwner : InodeOwner)
        (directoryPermissions : PermissionBits)
        (existing : Inode)
        : bool
        =
        // Measured on Linux 6.18.5 (`protected-sysctls.c`, REGULAR and FIFOS
        // rows) over the geometry `refusesToFollow` was, for each value of each
        // sysctl: a regular file, a FIFO, a link opened `O_NOFOLLOW` and a
        // directory. The FIFO rows are the regular file's under the other
        // sysctl, value for value.
        let bits = PermissionBits.toInt directoryPermissions

        // `None` for a kind no sysctl governs.
        let governing =
            match existing.Content with
            | InodeContent.RegularFile _ -> Some protection.RegularFiles
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _
            | InodeContent.Directory _ -> None

        if bits &&& PermissionBits.sticky = 0 then
            false
        elif governing = Some CreationProtection.Off then
            false
        elif existing.Owner.User = directoryOwner.User then
            false
        elif (Standing.toward credentials existing.Owner).Owns then
            false
        elif bits &&& worldWritable <> 0 then
            true
        elif bits &&& groupWritable <> 0 then
            governing = Some CreationProtection.InGroupOrWorldWritableStickyDirectories
        else
            false
