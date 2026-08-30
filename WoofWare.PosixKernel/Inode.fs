namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// Identity of a file within the emulated filesystem: the `st_ino` a guest
/// reads back from `stat`.
///
/// Guest-observable: the BCL compares `(Dev, Ino)` pairs directly to decide
/// whether two paths name the same file (`File.Copy`, `File.Move` and
/// `File.Replace` all do this before touching anything).
[<Struct>]
type InodeNumber =
    | InodeNumber of value : int64

    override this.ToString () : string =
        match this with
        | InodeNumber value -> string<int64> value

/// The four times a kernel keeps for an inode.
///
/// All four are stored on every platform, including `Birth` — which Linux's
/// `stat` does not report, but which *exists*: `pal_io.c` hard-zeroes it under
/// `#else` with the comment "Linux path: until we use statx()". Modelling it
/// here and gating only its *reporting* on the simulated platform confines the
/// platform flavour to the `stat` boundary.
type InodeTimes =
    {
        /// `st_atim`: last read.
        Access : UnixTimestamp
        /// `st_mtim`: last change to the *contents*.
        Modification : UnixTimestamp
        /// `st_ctim`: last change to the *inode* — which `chmod`, `link` and
        /// `rename` all move even though they touch no content, and which is
        /// why this is stored rather than derived from `Modification`.
        StatusChange : UnixTimestamp
        /// `st_birthtim`: when the inode was created. Never moves afterwards.
        Birth : UnixTimestamp
    }

[<RequireQualifiedAccess>]
module InodeTimes =
    /// The times a freshly-created inode has: all four equal, because creation
    /// is simultaneously its birth, its last content change, its last inode
    /// change, and (vacuously) its last access.
    let createdAt (now : UnixTimestamp) : InodeTimes =
        {
            Access = now
            Modification = now
            StatusChange = now
            Birth = now
        }

    /// Record a change to the inode's contents: `mtime` and `ctime` both move,
    /// because changing what a file or directory holds also changes the inode
    /// that describes it. `atime` and `birth` do not.
    let contentsChangedAt (now : UnixTimestamp) (times : InodeTimes) : InodeTimes =
        { times with
            Modification = now
            StatusChange = now
        }

    /// Record a change to the inode itself, its contents untouched: `ctime`
    /// moves and nothing else does. What gaining or losing a link does, since a
    /// link count lives on the inode rather than in what the inode holds.
    ///
    /// Measured on both platforms through a held descriptor's `fstat`, which is
    /// the only way to watch an inode whose last name has just gone: after
    /// `unlink`, `ctime` has moved and `mtime` and `atime` have not — the same
    /// for an inode that still has links left as for one that does not.
    let statusChangedAt (now : UnixTimestamp) (times : InodeTimes) : InodeTimes =
        { times with
            StatusChange = now
        }

/// The contents of a directory: what it holds, and what contains it.
///
/// `Entries` holds only *real* names. "." and ".." are genuine directory
/// entries in a kernel, but storing them here would mean every traversal either
/// special-cased them or recursed forever, and would let the graph express a
/// directory whose "." pointed somewhere else. Both are instead derived — ".."
/// from `Parent`, "." from the directory itself — so they cannot disagree with
/// the structure they describe. `readdir` synthesises them.
type DirectoryContent =
    {
        Entries : Map<DirectoryEntryName, InodeNumber>
        /// The directory that holds this one, which is what ".." resolves to.
        /// The root is its own parent, exactly as on a real Unix.
        ///
        /// This is the *physical* parent, so it is still correct after a walk
        /// has crossed a symlink — the lexical predecessor in the path is not.
        Parent : InodeNumber
        /// The `chmod`-able bits of this directory's mode.
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
    /// The link's target, unresolved: a symlink's target is a *string* to the
    /// kernel, re-resolved on every traversal, not a reference to whatever it
    /// pointed at when it was made.
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

/// One inode: what lives there, and the metadata every inode carries whatever
/// kind of thing it is.
type Inode =
    {
        Content : InodeContent
        Times : InodeTimes
    }

/// An inode's permission bits as a caller must handle them, which is not always
/// "here is a number".
///
/// A DU rather than an `option`, so that a caller cannot reach for a default
/// and quietly get the wrong answer: the symlink case is not "no permissions",
/// it is "the answer is a property of the platform, which this module cannot
/// see". `SimulatedUnixPlatform` compiles after this file; platform-flavoured
/// presentation is a `stat` concern.
[<RequireQualifiedAccess>]
type InodePermissions =
    /// A regular file's or directory's stored, `chmod`-able bits.
    | Stored of bits : PermissionBits
    /// A symbolic link's bits, which are **not stored** because no syscall
    /// PawPrint models can make two links differ: Linux has no `lchmod`,
    /// `chmod(2)` follows the link, and `fchmodat(AT_SYMLINK_NOFOLLOW)` is
    /// ENOTSUP there. Under Linux the answer is invariably 0o777, so a stored
    /// field could only ever express a filesystem no kernel could have
    /// produced.
    ///
    /// Platform-dependent, and measured rather than assumed: macOS applies the
    /// creating process's **umask** to a symlink (probed on this box: `umask
    /// 022` gives 0o755, `umask 077` gives 0o700, `umask 000` gives 0o777),
    /// while Linux reports 0o777 whatever the umask. So the caller — which
    /// knows the simulated platform — supplies the value.
    | PlatformSymlinkDefault

[<RequireQualifiedAccess>]
module Inode =
    /// An inode's permission bits, as something the caller must match rather
    /// than a number it might default. See `InodePermissions`.
    let permissions (inode : Inode) : InodePermissions =
        match inode.Content with
        | InodeContent.RegularFile (_, permissions) -> InodePermissions.Stored permissions
        | InodeContent.Directory directory -> InodePermissions.Stored directory.Permissions
        | InodeContent.Symlink _ -> InodePermissions.PlatformSymlinkDefault
