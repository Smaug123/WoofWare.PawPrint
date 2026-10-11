namespace WoofWare.PosixKernel

/// One change a `VirtualFileSystem` mutator made to the inode graph, as a
/// mutator reports it beside the graph it produced (`VfsMutation`).
///
/// Each change is reported once, in the most specific of these terms. An entry
/// change includes what every change of its kind does besides binding names:
/// the directories' `mtime` and `ctime` (and their link counts, which count
/// subdirectories), the moved inode's `ctime` and `..`, and the link a
/// displaced inode lost. None of those is reported again as
/// `AttributesChanged`.
[<RequireQualifiedAccess>]
type internal FsChange =
    /// `directory` gained the name `name`, bound to `inode`: a new inode, or an
    /// existing one gaining a hard link, which the mutator reports as
    /// `AttributesChanged` too.
    | EntryAdded of directory : InodeNumber * name : DirectoryEntryName * inode : InodeNumber
    /// `directory` lost the name `name`, which was bound to `inode`. The inode
    /// stays in the graph until `InodeForgotten`, whatever names it has left.
    | EntryRemoved of directory : InodeNumber * name : DirectoryEntryName * inode : InodeNumber
    /// A rename: the name `sourceName` in `source` is now `destinationName` in
    /// `destination`, still bound to `inode`. `displaced` is the inode the
    /// destination name was bound to before, which lost that link; `None` when
    /// the name was free.
    | EntryMoved of
        source : InodeNumber *
        sourceName : DirectoryEntryName *
        destination : InodeNumber *
        destinationName : DirectoryEntryName *
        inode : InodeNumber *
        displaced : InodeNumber option
    /// The regular file `inode` was written or truncated, even when its bytes
    /// end as they were. The `mtime` and `ctime` that moves is part of this
    /// change.
    | ContentChanged of inode : InodeNumber
    /// `inode`'s mode, owner or timestamps were set, even to what they were; a
    /// write or truncation stripped its set-ID bits; or, being no directory, it
    /// gained or lost a hard link other than by a rename.
    | AttributesChanged of inode : InodeNumber
    /// `inode` left the graph, its last name and every reference to it having
    /// gone.
    | InodeForgotten of inode : InodeNumber
