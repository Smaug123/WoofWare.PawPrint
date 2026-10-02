namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// <summary>
/// Identity of a file within the emulated filesystem.
/// </summary>
/// <remarks>
/// This is the <c>st_ino</c> a guest reads back from <c>stat</c>.
///
/// The exact values are guest-observable: for example, the .NET BCL often
/// explicitly determines whether two paths name the same file by comparing
/// device and inode identifiers as integers.
/// </remarks>
[<Struct>]
type InodeNumber =
    | InodeNumber of value : int64

    /// <summary>
    /// The underlying integer, formatted as a string.
    /// </summary>
    override this.ToString () : string =
        match this with
        | InodeNumber value -> string<int64> value

/// <summary>
/// The timestamp metadata a kernel keeps for an inode.
/// </summary>
/// <remarks>
/// All these timestamps are stored on every platform.
/// That includes <c>Birth</c>, even though some syscalls (like Linux's <c>stat</c>) might not report it.
/// (<c>statx</c> does.)
/// </remarks>
type InodeTimes =
    {
        /// <summary>
        /// <c>st_atim</c>: last read.
        /// </summary>
        Access : UnixTimestamp
        /// <summary>
        /// <c>st_mtim</c>: last change to the contents.
        /// </summary>
        Modification : UnixTimestamp
        /// <summary>
        /// <c>st_ctim</c>: last change to the inode.
        /// </summary>
        /// <example>
        /// <c>chmod</c>, <c>link</c>, and <c>rename</c> all move this, even though they touch no content.
        /// </example>
        StatusChange : UnixTimestamp
        /// <summary>
        /// <c>st_birthtim</c>: when the inode was created.
        /// </summary>
        /// <remarks>
        /// Never moves after creation.
        /// </remarks>
        Birth : UnixTimestamp
    }

[<RequireQualifiedAccess>]
module InodeTimes =
    /// <summary>
    /// The timing metadata of a freshly-created inode has.
    /// </summary>
    /// <remarks>
    /// All four timestamps are equal, because creation is simultaneously its birth,
    /// its last content change, its last inode change, and its last access.
    /// </remarks>
    let createdAt (now : UnixTimestamp) : InodeTimes =
        {
            Access = now
            Modification = now
            StatusChange = now
            Birth = now
        }

    /// <summary>
    /// Record a change to the inode's contents.
    /// </summary>
    /// <remarks>
    /// <c>mtime</c> and <c>ctime</c> both move,
    /// because changing what a file or directory holds also changes the inode
    /// that describes it.
    /// <c>atime</c> and <c>birth</c> do not move.
    /// </remarks>
    let contentsChangedAt (now : UnixTimestamp) (times : InodeTimes) : InodeTimes =
        { times with
            Modification = now
            StatusChange = now
        }

    /// <summary>
    /// Record a change to the inode itself, its contents untouched.
    /// </summary>
    /// <remarks>
    /// <c>ctime</c> moves, and nothing else does.
    /// </remarks>
    /// <example>
    /// This is the timestamp change that happens when the inode gains or loses a link does,
    /// since a link count lives on the inode rather than in what the inode holds.
    /// </example>
    let statusChangedAt (now : UnixTimestamp) (times : InodeTimes) : InodeTimes =
        // Measured on both platforms through a held descriptor's `fstat`, which is
        // the only way to watch an inode whose last name has just gone: after
        // `unlink`, `ctime` has moved and `mtime` and `atime` have not — the same
        // for an inode that still has links left as for one that does not.
        { times with
            StatusChange = now
        }

/// <summary>
/// The contents of a directory: what it holds, and what contains it.
/// </summary>
type DirectoryContent =
    {
        /// <summary>
        /// The inodes contained in this directory.
        /// </summary>
        /// <remarks>
        /// Holds only <i>real</i> names. "." and ".." are genuine directory entries in the kernel,
        /// but we don't store them here (because that would make recursion harder).
        /// <c>readdir</c> synthesises them on demand.
        /// </remarks>
        Entries : Map<DirectoryEntryName, InodeNumber>
        /// <summary>
        /// The directory that holds this one, which is what ".." resolves to.
        /// </summary>
        /// <example>
        /// The root is its own parent.
        /// </example>
        /// <remarks>
        /// This is the <i>physical</i> parent, so it is still correct after a walk
        /// has crossed a symlink.
        /// By contrast, the <i>lexical</i> predecessor in the path need not be.
        /// </remarks>
        Parent : InodeNumber
        /// <summary>
        /// The <c>chmod</c>-able bits of this directory's mode.
        /// </summary>
        Permissions : PermissionBits
    }

/// <summary>
/// What lives at an inode.
/// </summary>
/// <remarks>
/// Carries only the metadata whose existence depends on which kind of thing
/// this inode is (e.g. permission bits, which can't necessarily be set on symlinks on Linux).
/// Metadata whose existence is guaranteed for all inodes lives on <c>Inode</c> instead.
///
/// The emulated filesystem is case-sensitive and normalisation-preserving,
/// because names are compared byte for byte.
/// This is more like a standard Linux filesystem than APFS, although it's not <i>wrong</i>
/// from the point of view of the kernel.
/// </remarks>
[<RequireQualifiedAccess>]
type InodeContent =
    | RegularFile of contents : ImmutableArray<byte> * permissions : PermissionBits
    | Directory of directory : DirectoryContent
    /// <summary>
    /// The link's target, unresolved.
    /// </summary>
    /// <remarks>
    /// The kernel treats a symlink's target as a string to be re-resolved
    /// on every traversal; it's not a reference to whatever it pointed at
    /// when it was made.
    /// </remarks>
    | Symlink of target : SymlinkTarget

[<RequireQualifiedAccess>]
module InodeContent =
    /// <summary>
    /// The <c>S_IFMT</c> band of <c>st_mode</c>, indicating which kind of thing (directory, regular file, symlink)
    /// lives at an inode.
    /// </summary>
    let fileTypeBits (content : InodeContent) : int =
        match content with
        | InodeContent.RegularFile _ -> 0o100000
        | InodeContent.Directory _ -> 0o40000
        | InodeContent.Symlink _ -> 0o120000

/// Who owns an inode: the `st_uid` and `st_gid` that `stat(2)` reports for it.
[<Struct>]
type InodeOwner =
    {
        User : UserId
        Group : GroupId
    }

/// Where a newly created inode's group comes from.
[<RequireQualifiedAccess>]
type NewInodeGroupRule =
    /// The creator's effective group, unless the directory the inode is created
    /// in is set-group-ID, in which case that directory's group.
    ///
    /// This is Linux, on a filesystem not mounted `grpid`.
    | CreatorsUnlessParentSetGroupId
    /// Always the group of the directory the inode is created in.
    ///
    /// This is Darwin, as it is every BSD.
    | Parents

[<RequireQualifiedAccess>]
module InodeOwner =
    /// The effective user and group of a process with `credentials`: who owns
    /// what that process creates when nothing else decides.
    let ofProcess (credentials : Credentials) : InodeOwner =
        {
            User = credentials.EffectiveUser
            Group = credentials.EffectiveGroup
        }

    /// The owner of an inode the process with `credentials` creates now, in a
    /// directory owned by `parentOwner` whose permission bits are
    /// `parentPermissions`.
    ///
    /// The user is always the creator's effective user ID.
    let ofNewInode
        (rule : NewInodeGroupRule)
        (credentials : Credentials)
        (parentOwner : InodeOwner)
        (parentPermissions : PermissionBits)
        : InodeOwner
        =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/new-inode-owner.c`,
        // for `open(O_CREAT)` and `mkdir` alike. Linux 6.18.5 (tmpfs and ext4):
        // real 1000 / effective 1001 creates uid 1001; the gid is the effective
        // gid in a plain parent whatever the parent's group, and the parent's gid
        // in a set-group-ID parent whether or not the creator is in that group.
        // Darwin 27.0: the parent's gid every time, including a `wheel` parent
        // the creator is not in, and whether or not the parent is set-group-ID.

        let group =
            match rule with
            | NewInodeGroupRule.Parents -> parentOwner.Group
            | NewInodeGroupRule.CreatorsUnlessParentSetGroupId ->
                if PermissionBits.toInt parentPermissions &&& PermissionBits.setGroupId <> 0 then
                    parentOwner.Group
                else
                    credentials.EffectiveGroup

        {
            User = credentials.EffectiveUser
            Group = group
        }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Standing =
    /// How a process with `credentials` stands towards an inode owned by
    /// `owner`.
    ///
    /// The effective IDs decide it, and the supplementary groups count as
    /// membership exactly as the effective group does; the real and saved IDs
    /// play no part.
    let toward (credentials : Credentials) (owner : InodeOwner) : Standing =
        // Measured on Linux 6.18.5 (`permission-standing.c`): real 1001 and
        // effective 1000 may read a 1000-owned 0400 file; real group 2000 and
        // effective group 1000, with no supplementary groups, may not read a
        // group-2000 0040 file and may read a group-1000 one; and a group
        // reached only through a supplementary group selects the group triple
        // over all 4096 modes. A Darwin process cannot have differing real and
        // effective IDs in this library (`UnixSystem.withCredentials`).
        {
            Privilege = Credentials.privilege credentials
            Owns = owner.User = credentials.EffectiveUser
            InGroup = Credentials.isInGroup credentials owner.Group
        }

/// <summary>
/// One inode: what lives there, and the metadata every inode carries whatever
/// kind of thing it is.
/// </summary>
type Inode =
    {
        Content : InodeContent
        Times : InodeTimes
        /// Who owns this inode.
        Owner : InodeOwner
    }

/// <summary>
/// An inode's permission bits as a caller must handle them.
/// </summary>
/// <remarks>
/// This is usually just a number as you might pass to <c>chmod</c>.
/// However, symlinks have platform-specific behaviour, so <c>InodePermissions</c> models them individually.
/// </remarks>
[<RequireQualifiedAccess>]
type InodePermissions =
    /// <summary>
    /// A regular file's or directory's stored, <c>chmod</c>-able bits.
    /// </summary>
    | Stored of bits : PermissionBits
    /// <summary>
    /// A symbolic link's permission bits.
    /// </summary>
    /// <remarks>
    /// Behaviour is platform-dependent. Darwin applies the creating process's <c>umask</c>
    /// to a symlink, and Darwin also has <c>lchmod</c>.
    /// By contrast, Linux reports <c>0o777</c> whatever the umask (and has no syscalls
    /// like BSD's <c>lchmod</c> which could change that value).
    /// </remarks>
    | PlatformSymlinkDefault

[<RequireQualifiedAccess>]
module Inode =
    /// <summary>
    /// An inode's permission bits.
    /// </summary>
    let permissions (inode : Inode) : InodePermissions =
        match inode.Content with
        | InodeContent.RegularFile (_, permissions) -> InodePermissions.Stored permissions
        | InodeContent.Directory directory -> InodePermissions.Stored directory.Permissions
        | InodeContent.Symlink _ -> InodePermissions.PlatformSymlinkDefault
