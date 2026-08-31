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

    /// The underlying integer, formatted as a string.
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

/// What lives at an inode. The `S_IFMT` file-type bits a guest reads from
/// `stat` are *derived* from which case this is, never stored, so the two can
/// never disagree.
///
/// Carries the metadata whose *existence* depends on which kind of thing this
/// is, and only that. A regular file and a directory have `chmod`-able
/// permission bits; a symbolic link does not, and the field is absent rather
/// than present-and-ignored — see `InodePermissions.PlatformSymlinkDefault` for
/// why a stored one could only ever describe a filesystem no kernel could
/// produce. Metadata that every inode has regardless (the four timestamps)
/// lives on `Inode` instead.
///
/// Names are compared with F#'s ordinal string comparison, so the emulated
/// filesystem is case-sensitive and normalisation-preserving. That is not a
/// platform divergence to crash on: case-sensitivity is a property of a
/// *filesystem* rather than of an OS (Linux mounts case-insensitive
/// directories; macOS runs case-sensitive APFS). It does mean the model
/// resembles a Linux default rather than a macOS one.
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
    /// The `S_IFMT` band of `st_mode`: which kind of thing lives at an inode.
    /// Derived from the content rather than stored, so the two cannot disagree.
    ///
    /// The values are `Interop.Sys.FileTypes`' (`Interop.Stat.cs`), which are in
    /// turn the POSIX ones. `TestVirtualFileSystemAgainstHost` pins them against
    /// that declaration *as read from the pinned runtime source*, rather than
    /// against a second copy of the same literals.
    let fileTypeBits (content : InodeContent) : int =
        match content with
        | InodeContent.RegularFile _ -> 0o100000
        | InodeContent.Directory _ -> 0o40000
        | InodeContent.Symlink _ -> 0o120000

/// <summary>
/// One inode: what lives there, and the metadata every inode carries whatever
/// kind of thing it is.
/// </summary>
type Inode =
    {
        Content : InodeContent
        Times : InodeTimes
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
