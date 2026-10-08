namespace WoofWare.PosixKernel

open System
open System.Collections.Immutable

/// The names one directory binds, sorted as `DirectoryContent.Entries` sorts
/// them, in a tree that finds the least name above a given one by descending
/// rather than by walking every name below it.
///
/// Equal and ordered by the names alone, so that two sets holding the same
/// names are indistinguishable however they were assembled.
[<CustomEquality>]
[<CustomComparison>]
type internal SortedEntryNames =
    private
        {
            Names : ImmutableSortedSet<DirectoryEntryName>
        }

    override this.Equals (other : obj) : bool =
        match other with
        | :? SortedEntryNames as other -> this.Names.Count = other.Names.Count && Seq.forall2 (=) this.Names other.Names
        | _ -> false

    // Folded by hand rather than with `HashCode.Combine`, whose seed differs
    // from one process to the next.
    override this.GetHashCode () : int =
        this.Names |> Seq.fold (fun acc name -> acc * 31 + hash name) this.Names.Count

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? SortedEntryNames as other -> Seq.compareWith compare this.Names other.Names
            | _ -> invalidArg "other" $"SortedEntryNames: cannot compare with %O{other}"

[<RequireQualifiedAccess>]
module internal SortedEntryNames =
    // The order `Map<DirectoryEntryName, _>` keeps its keys in, which is what
    // makes these names agree with a walk of the map.
    let private order : System.Collections.Generic.IComparer<DirectoryEntryName> =
        ComparisonIdentity.Structural<DirectoryEntryName>

    let empty : SortedEntryNames =
        {
            Names = ImmutableSortedSet.Create<DirectoryEntryName> order
        }

    let ofSeq (names : DirectoryEntryName seq) : SortedEntryNames =
        {
            Names = ImmutableSortedSet.CreateRange<DirectoryEntryName> (order, names)
        }

    let isEmpty (names : SortedEntryNames) : bool = names.Names.IsEmpty

    /// How many names there are, in constant time.
    let count (names : SortedEntryNames) : int = names.Names.Count

    let contains (name : DirectoryEntryName) (names : SortedEntryNames) : bool = names.Names.Contains name

    let toList (names : SortedEntryNames) : DirectoryEntryName list = List.ofSeq names.Names

    let add (name : DirectoryEntryName) (names : SortedEntryNames) : SortedEntryNames =
        {
            Names = names.Names.Add name
        }

    let remove (name : DirectoryEntryName) (names : SortedEntryNames) : SortedEntryNames =
        {
            Names = names.Names.Remove name
        }

    /// The least name strictly greater than `lower`, or the least of all when
    /// there is no lower bound; `lower` need not be one of the names.
    /// Logarithmic in the number of names.
    let leastAbove (lower : DirectoryEntryName option) (names : SortedEntryNames) : DirectoryEntryName option =
        let next =
            match lower with
            | None -> 0
            | Some lower ->
                // The index of `lower` if present, and otherwise the bitwise
                // complement of the index of the least name above it.
                let index = names.Names.IndexOf lower
                if index >= 0 then index + 1 else ~~~index

        if next < names.Names.Count then
            Some names.Names.[next]
        else
            None

/// A filesystem mounted over a directory of the root filesystem.
[<RequireQualifiedAccess>]
type MountedFileSystem =
    /// Linux's devtmpfs, which holds a node for each device the kernel has a
    /// driver for. This kernel's holds a node for each `CharacterDevice`, and
    /// nothing else: no name can be created in it, and none removed.
    | Devtmpfs
    /// Darwin's devfs, which this kernel does not model. A path that reaches it
    /// is refused.
    | Devfs

/// One mounted filesystem.
type Mount =
    {
        /// The inode number of the directory the mount covers. A listing of
        /// that directory's parent reports this number for its name, while
        /// `stat(2)` of the name reports the mounted filesystem's root.
        Covered : InodeNumber
        /// What is mounted.
        FileSystem : MountedFileSystem
    }

/// <summary>
/// A whole emulated filesystem: an inode graph rooted at a single directory.
/// </summary>
type VirtualFileSystem =
    private
        {
            Inodes : Map<InodeNumber, Inode>
            /// The directory absolute paths resolve from. Its `Parent` is
            /// itself.
            Root : InodeNumber
            /// <summary>
            /// The next inode number to hand out.
            /// </summary>
            /// <remarks>
            /// Numbers are never reused, even after the last link to a file is removed:
            /// reuse is observable to a process that cached an <c>(st_dev, st_ino)</c> pair,
            /// and a fresh number can only ever make a stale comparison report
            /// "different file", which is the safe direction to be wrong in.
            /// </remarks>
            NextInode : InodeNumber
            /// How many directory entries name each inode, holding only the
            /// non-zero counts: an inode absent from the map is named by
            /// nothing. Kept so that asking the count is not a scan of every
            /// directory; `checkInvariants` holds it to that scan. Holding no
            /// zeros is what makes the map a function of the entries alone,
            /// so that two filesystems with the same graph compare equal.
            BindingCounts : Map<InodeNumber, int>
            /// The names each directory binds, sorted for `nextDirectoryEntry`
            /// to seek in, holding only the directories that bind at least one
            /// name: a directory absent from the map binds nothing. Kept so that
            /// finding the next name is not a walk of the directory;
            /// `checkInvariants` holds it to the entries. Holding no empty sets
            /// is what makes the map a function of the entries alone, so that
            /// two filesystems with the same graph compare equal.
            SortedNames : Map<InodeNumber, SortedEntryNames>
            /// How many of each directory's entries name a directory, holding
            /// only the non-zero counts: a directory absent from the map holds
            /// no subdirectory. Kept so that asking the count is not a walk of
            /// the directory; `checkInvariants` holds it to the entries.
            /// Holding no zeros is what makes the map a function of the
            /// entries alone, so that two filesystems with the same graph
            /// compare equal.
            SubdirectoryCounts : Map<InodeNumber, int>
            /// Each mounted filesystem, by the inode of its root. The mounted
            /// root is bound in its parent in place of the directory it covers,
            /// whose inode number `Mount.Covered` keeps: that number has no
            /// inode in this graph, and is never handed out again.
            Mounts : Map<InodeNumber, Mount>
            /// The root of the mounted filesystem each inode is on, for every
            /// inode that is not on the root filesystem, mounted roots
            /// included.
            MountMembers : Map<InodeNumber, InodeNumber>
        }

/// A way in which a `VirtualFileSystem` fails to describe a filesystem any
/// kernel could produce. `VirtualFileSystem.checkInvariants` returns these;
/// none of the operations in this module can produce one.
[<RequireQualifiedAccess>]
type VirtualFileSystemDefect =
    /// `Root` names an inode the graph does not contain.
    | RootMissing of root : InodeNumber
    /// `Root` names something other than a directory.
    | RootIsNotDirectory of root : InodeNumber
    /// The root's `Parent` is not the root. On a real Unix "/.." is "/".
    | RootParentIsNotSelf of root : InodeNumber * recordedParent : InodeNumber
    /// Some directory holds an entry pointing at the root. The root is the one
    /// directory with *no* incoming entry link; giving it one would make the
    /// graph cyclic while leaving every individual link count plausible.
    | RootHasIncomingLink of parents : (InodeNumber * DirectoryEntryName) list
    /// A directory entry points at an inode the graph does not contain.
    | DanglingEntry of directory : InodeNumber * name : DirectoryEntryName * target : InodeNumber
    /// A directory's `Parent` names an inode the graph does not contain.
    | DanglingParent of directory : InodeNumber * recordedParent : InodeNumber
    /// A directory's `Parent` names something that is not a directory.
    | ParentIsNotDirectory of directory : InodeNumber * recordedParent : InodeNumber
    /// A directory's `Parent` disagrees with the directory that actually holds
    /// it, so ".." would walk somewhere the path did not come from.
    | ParentMismatch of directory : InodeNumber * recordedParent : InodeNumber * actualParent : InodeNumber
    /// A directory is held by more than one entry. Unix forbids hard links to
    /// directories precisely because they would make the graph a non-tree, and
    /// `Parent` could then name only one of them.
    | DirectoryMultiplyLinked of directory : InodeNumber * parents : (InodeNumber * DirectoryEntryName) list
    /// An inode no path from the root can reach, and which the caller did not
    /// declare pinned.
    ///
    /// Every inode in a real filesystem is reachable unless some process holds
    /// it open after its last link went away — which is exactly what
    /// `checkInvariants`'s `pinned` argument names. An unreachable inode nobody
    /// holds is a leak: nothing can ever name it again, and nothing will free
    /// it.
    | UnreachableFromRoot of inode : InodeNumber
    /// `NextInode` would hand out a number already in use.
    | NextInodeNotFresh of nextInode : InodeNumber * existing : InodeNumber
    /// The count `VirtualFileSystem.bindingCount` answers for `inode`
    /// disagrees with the number of directory entries that name it.
    ///
    /// `stored` is `None` where no count is stored, which the filesystem
    /// reads as zero. A stored `Some 0` is reported even though it agrees in
    /// value, because only non-zero counts are stored.
    | BindingCountMismatch of inode : InodeNumber * stored : int option * counted : int
    /// The names `VirtualFileSystem.nextDirectoryEntry` seeks in for
    /// `directory` disagree with the names its entries bind.
    ///
    /// `stored` is `None` where none are stored, which the filesystem reads as
    /// binding nothing. A stored `Some []` is reported even though it agrees
    /// in content, because names are stored only for a directory that binds
    /// some. `bound` is empty for an inode that is not a directory, or that
    /// the graph does not contain.
    | SortedNamesMismatch of
        directory : InodeNumber *
        stored : DirectoryEntryName list option *
        bound : DirectoryEntryName list
    /// The count `VirtualFileSystem.subdirectoryCount` answers for
    /// `directory` disagrees with the number of its entries that name a
    /// directory.
    ///
    /// `stored` is `None` where no count is stored, which the filesystem
    /// reads as zero. A stored `Some 0` is reported even though it agrees in
    /// value, because only non-zero counts are stored.
    | SubdirectoryCountMismatch of directory : InodeNumber * stored : int option * counted : int
    /// A mounted filesystem's root is absent, or is not a directory.
    | MountRootNotDirectory of root : InodeNumber
    /// The inodes recorded as being on the filesystem mounted at `root` are not
    /// that root and the entries it binds.
    | MountMembershipMismatch of root : InodeNumber * recorded : Set<InodeNumber> * actual : Set<InodeNumber>
    /// The inode number a mount keeps for the directory it covers is also the
    /// number of an inode in the graph.
    | CoveredInodeInUse of root : InodeNumber * covered : InodeNumber

/// Why `VirtualFileSystem.mountAtRoot` will not mount a filesystem.
[<RequireQualifiedAccess>]
type internal MountFault =
    /// The root already binds `name` to something other than an empty
    /// directory, which a mount over it would hide.
    | CoveredEntryNotAnEmptyDirectory of name : DirectoryEntryName

/// What losing a name does to the inode that had it, which is not the same for
/// every caller of `unbind`.
///
/// Names the *mechanism* rather than the stamp, because the stamp follows from
/// it: an inode whose link count changed has changed, so its `ctime` moves.
[<RequireQualifiedAccess>]
type UnbindTargetEffect =
    /// The inode lost a link, so its `ctime` moves and nothing else does.
    /// `unlink(2)` on both flavours, and Linux's `rmdir(2)`.
    | LostALink
    /// The inode is untouched, so no timestamp moves. Darwin's `rmdir(2)`:
    /// measured through a descriptor held across the call, the removed
    /// directory keeps its `ctime` and keeps `st_nlink` at 2, where Linux moves
    /// the one and drops the other to 0.
    | Untouched

/// What a `rename` displaced, for the caller that can see the descriptor table
/// to decide about.
///
/// A named record rather than a bare `InodeNumber option` beside the
/// filesystem: a rename has *two* inodes a caller could plausibly want — the
/// one that moved and the one that lost its name — and only the second has
/// anything left to decide. Naming the field is what stops the two being
/// confused at a call site where both are just numbers.
type internal RenameOutcome =
    {
        /// The inode the destination name was bound to before the rename took
        /// it, or `None` when that name was free.
        ///
        /// It may still have other names, and something may still hold it open;
        /// `VirtualFileSystem.rename` frees nothing, for the reason
        /// `VirtualFileSystem.unbind` frees nothing.
        Displaced : InodeNumber option
    }

/// Where `lseek(2)` measures its offset from.
///
/// Exactly the three POSIX values — and *not* the platforms' full `<unistd.h>`
/// vocabulary, which continues with `SEEK_DATA` and `SEEK_HOLE`. Those two are
/// deliberately absent: they are numbered 3 and 4 on Linux and **4 and 3** on
/// Darwin, so a raw whence of 3 does not name the same operation on the two
/// kernels, and there is no portable case to add. `UnixDescriptor.lseek`
/// decodes a raw `whence` under the flavour and refuses them.
[<RequireQualifiedAccess>]
type SeekWhence =
    /// `SEEK_SET` (0): from the start of the file.
    | Set
    /// `SEEK_CUR` (1): from the description's current offset.
    | Current
    /// `SEEK_END` (2): from the end of the file.
    | End

/// Why a write to a regular file has no answer this library can give.
///
/// `UnmeasuredSetIdChange` is a row no one has measured rather than a limit of
/// the model; the rest of what follows is about `WouldExceedMaxLength`.
///
/// Not a `UnixError`, and deliberately: this is a limit of the model rather than
/// anything a kernel does, so a caller must fail loudly rather than translate it
/// into an errno a process could catch and interpret. Measured on ext4 and APFS
/// alike, `pwrite` of one byte at offset 2^40 succeeds and leaves a sparse 1 TB
/// file behind.
[<RequireQualifiedAccess>]
type internal FileWriteRefusal =
    /// The write would leave the file longer than `VirtualFileSystem.maxFileLength`.
    /// Carries the write rather than the resulting length, which need not be a
    /// number: `offset + count` can leave `int64` entirely.
    | WouldExceedMaxLength of offset : int64 * count : int
    /// What the write would do to the file's set-ID bits has not been measured
    /// for this writer; see `SetIdChangeRefusal`.
    | UnmeasuredSetIdChange of refusal : SetIdChangeRefusal

/// Why a truncation has no answer.
///
/// Separate from `FileWriteRefusal` rather than a case added to it: the payloads
/// differ (a truncation names one length, a write names an offset and a count),
/// and neither operation can produce the other's case, so sharing the type would
/// force every `match` to handle something unreachable.
///
/// Like `FileWriteRefusal`, neither case is anything a kernel does, so a caller
/// fails loudly rather than translating it into an errno. Measured on ext4 and
/// APFS alike, `ftruncate(fd, 3e9)` succeeds and leaves a sparse
/// three-gigabyte file behind.
[<RequireQualifiedAccess>]
type internal FileTruncationRefusal =
    /// The requested length is more than `VirtualFileSystem.maxFileLength`.
    /// Carries the length as asked for, which need not fit in an `int`.
    | WouldExceedMaxLength of length : int64
    /// What the truncation would do to the file's set-ID bits has not been
    /// measured for this process; see `SetIdChangeRefusal`.
    | UnmeasuredSetIdChange of refusal : SetIdChangeRefusal

/// Why a seek computation has no answer.
///
/// Split into two cases rather than one because the platforms disagree about
/// only one of them: a computation landing below zero is `EINVAL` on both,
/// while one that leaves `int64` is `EINVAL` on Linux and `EOVERFLOW` on Darwin.
/// Collapsing them here would push that distinction into the caller as a
/// second computation of the same arithmetic.
[<RequireQualifiedAccess>]
type internal SeekFault =
    /// The computed position is negative. Real kernels reject rather than
    /// clamp, so a file offset is never pinned to 0 by a wild seek.
    | Negative
    /// The computed position does not fit in a signed 64-bit offset. Only
    /// reachable through `SEEK_CUR` and `SEEK_END`, whose arithmetic adds two
    /// values the caller does not jointly control.
    | Overflow

/// A name a directory stream can hand back. Neither "." nor ".." is a
/// `FileName` — `FileNameError.Reserved` rejects both, because a directory
/// binds neither and this library derives both from the graph — so a stream that
/// must produce all three needs a type that can say which it produced.
[<RequireQualifiedAccess>]
type DirectoryStreamName =
    /// The directory being enumerated.
    | Dot
    /// `DirectoryContent.Parent`, which is the *physical* parent and so is
    /// still right after a walk crossed a symlink to get here.
    | DotDot
    /// A name the directory actually binds.
    | Entry of name : DirectoryEntryName

    /// The name rendered for a diagnostic; `DirectoryStreamName.toByteString`
    /// is what `readdir(3)` puts in `d_name`.
    override this.ToString () : string =
        match this with
        | DirectoryStreamName.Dot -> "."
        | DirectoryStreamName.DotDot -> ".."
        | DirectoryStreamName.Entry name -> DirectoryEntryName.toEscaped name

[<RequireQualifiedAccess>]
module DirectoryStreamName =
    let private ofLiteral (name : string) : UnixByteString =
        match UnixByteString.ofString name with
        | Ok bytes -> bytes
        | Error defect -> failwith $"DirectoryStreamName: %s{UnixPathText.describe defect}"

    let private dot : UnixByteString = ofLiteral "."
    let private dotDot : UnixByteString = ofLiteral ".."

    /// The bytes `readdir(3)` would put in `d_name`.
    let toByteString (name : DirectoryStreamName) : UnixByteString =
        match name with
        | DirectoryStreamName.Dot -> dot
        | DirectoryStreamName.DotDot -> dotDot
        | DirectoryStreamName.Entry name -> DirectoryEntryName.toByteString name

/// How far through a directory an open stream has read.
///
/// A *name*, not a position. Measured on both kernels at 3000 entries, one
/// `getdents` call at a time and at every buffer size up to 64 KiB, deleting
/// each entry as it is returned skips nothing and leaves the directory empty.
/// A cursor that counted entries would break the usual recursive delete, which
/// removes each child while enumerating the live stream and then `rmdir`s the
/// parent: an enumeration that skipped anything would leave the parent
/// non-empty, and the `rmdir` would answer ENOTEMPTY.
///
/// Four cases rather than a `FileName option`, because "returned `..`, not yet
/// `.`" is a real position of the stream and neither dot is expressible as a
/// `FileName`.
///
/// What this does *not* claim is agreement with a real kernel about the order,
/// or about what a mutation part-way through does. Both are exact rules on each
/// real filesystem, and neither is this one: tmpfs yields the newest link first
/// and resumes by a per-directory offset, APFS yields in the order of a hash of
/// the name and resumes after the last key it returned. This model's order is
/// its own. See `docs/divergences.md`.
///
/// The cases are declared in the order the stream visits them.
[<RequireQualifiedAccess>]
type DirectoryCursor =
    /// Nothing returned yet; the next entry is the least name the directory
    /// binds, or `..` if it binds none.
    | Start
    /// The last name handed back, which the next entry must strictly exceed.
    | After of name : DirectoryEntryName

    /// The names are exhausted and `..` has been handed back; `.` is next.
    | ReturnedDotDot
    /// `.` has been handed back, which is the end of the stream.
    | ReturnedDot

/// Where an open file description onto a directory is positioned.
///
/// Held on the description, as both kernels hold it: measured, a `dup` of the
/// descriptor continues where the original stopped, a second `open` of the
/// same directory starts afresh, and `lseek(fd, 0, SEEK_SET)` rewinds.
[<RequireQualifiedAccess>]
type DirectoryPosition =
    /// A point this library's walk can resume from.
    | Cursor of cursor : DirectoryCursor
    /// A nonzero offset that `lseek(2)` moved the description to.
    ///
    /// Both kernels accept any non-negative offset on a directory and report it
    /// back, but what the next read yields from it is each filesystem's own:
    /// tmpfs resumes from the entry with the greatest offset at or below it,
    /// APFS skips that many entries or answers EAGAIN depending on its high
    /// word. Neither is an offset this model mints, so reading from here is
    /// refused rather than guessed. Always positive: offset 0 is `Cursor Start`.
    | Unenumerable of offset : int64

[<RequireQualifiedAccess>]
module VirtualFileSystem =

    /// Inode 1, matching the convention that no real filesystem hands out inode
    /// 0. A zero default would otherwise silently alias whichever inode was
    /// allocated first.
    let private firstInode : InodeNumber = InodeNumber 1L

    /// The `st_dev` every inode on the root filesystem reports. An inode on a
    /// mounted filesystem reports that filesystem's own, which its mount's
    /// configuration states. A
    /// program reads `(st_dev, st_ino)` pairs to decide whether two paths name
    /// the same file, and Darwin's `statfs(2)` reports the same number as the
    /// first word of `f_fsid`. It is *non-zero*: no mounted filesystem reports
    /// 0, so a zero here would be indistinguishable from a field nobody
    /// remembered to write.
    let deviceId : int64 = 0x1000001L

    /// A filesystem containing nothing but an empty root directory owned by
    /// `rootOwner`, created at `now`.
    ///
    /// Takes the time rather than reading a clock: a filesystem that read the
    /// host's clock would make a replay depend on when it was recorded.
    let internal empty (now : UnixTimestamp) (rootOwner : InodeOwner) : VirtualFileSystem =
        {
            Inodes =
                Map.ofList
                    [
                        firstInode,
                        {
                            Content =
                                InodeContent.Directory
                                    {
                                        Entries = Map.empty
                                        Parent = firstInode
                                        Permissions = SeedEntry.defaultPermsForDirectory
                                    }
                            Times = InodeTimes.createdAt now
                            Owner = rootOwner
                        }
                    ]
            Root = firstInode
            NextInode = InodeNumber 2L
            BindingCounts = Map.empty
            SortedNames = Map.empty
            SubdirectoryCounts = Map.empty
            Mounts = Map.empty
            MountMembers = Map.empty
        }

    let root (vfs : VirtualFileSystem) : InodeNumber = vfs.Root

    /// The mount whose root is `inode`, or `None` if `inode` is not the root of
    /// a mounted filesystem.
    let mountOf (inode : InodeNumber) (vfs : VirtualFileSystem) : Mount option = Map.tryFind inode vfs.Mounts

    /// The root of the mounted filesystem `inode` is on, or `None` if it is on
    /// the root filesystem (or is not in the graph at all).
    let mountedRootOf (inode : InodeNumber) (vfs : VirtualFileSystem) : InodeNumber option =
        Map.tryFind inode vfs.MountMembers

    /// Fail unless `inode` is on the root filesystem. Every mounted filesystem
    /// this module holds is closed — nothing can be created in it or removed
    /// from it — so a caller that reached a mutation of one has skipped the
    /// refusal its syscall owes.
    let private assertOnRootFileSystem (context : string) (inode : InodeNumber) (vfs : VirtualFileSystem) : unit =
        match Map.tryFind inode vfs.MountMembers with
        | None -> ()
        | Some root ->
            failwith
                $"VirtualFileSystem.%s{context}: inode %O{inode} is on the filesystem mounted at inode %O{root}, which nothing can change the names of (this is a bug in the caller of VirtualFileSystem.%s{context})."

    /// Fail if `inode` is the root of a mounted filesystem, which no name can
    /// be removed from or moved over while it is mounted.
    let private assertNotMountRoot (context : string) (inode : InodeNumber) (vfs : VirtualFileSystem) : unit =
        if Map.containsKey inode vfs.Mounts then
            failwith
                $"VirtualFileSystem.%s{context}: inode %O{inode} is the root of a mounted filesystem, whose name cannot be removed or replaced while it is mounted (this is a bug in the caller of VirtualFileSystem.%s{context})."


    let internal nextInode (vfs : VirtualFileSystem) : InodeNumber = vfs.NextInode

    let inodes (vfs : VirtualFileSystem) : Map<InodeNumber, Inode> = vfs.Inodes

    let tryGet (inode : InodeNumber) (vfs : VirtualFileSystem) : Inode option = Map.tryFind inode vfs.Inodes

    /// What lives at `inode`, discarding its metadata. A projection, for the
    /// many callers that are asking a question about the *shape* of the graph;
    /// `tryGet` is the one that answers about identity.
    let tryGetContent (inode : InodeNumber) (vfs : VirtualFileSystem) : InodeContent option =
        Map.tryFind inode vfs.Inodes |> Option.map (fun inode -> inode.Content)

    /// How many bytes a read of `count` bytes starting at `offset` transfers,
    /// from a file whose contents are `length` bytes long.
    ///
    /// Separated out because it is the whole of what `pread(2)` decides beyond
    /// its error cases, and because getting it wrong is an off-by-one that
    /// end-to-end tests report as "the file came back slightly wrong" from
    /// somewhere deep in a `StreamReader`. As a function of three integers it is
    /// property-testable against naive slicing instead.
    ///
    /// The result is what a *regular file* transfers, which is why this can
    /// be total: a short read is only ever "the file ended". Real `read(2)` may
    /// return fewer bytes than asked for on a pipe or socket with nothing to do
    /// with EOF, and nothing here models that.
    ///
    /// Reading at or past the end is 0 rather than an error — measured, and the
    /// same on Linux and Darwin. So is a zero-length request, which is why
    /// callers must not treat 0 as EOF-specific.
    let internal readTransferCount (offset : int64) (count : int) (length : int) : int =
        // The caller is responsible for rejecting a negative offset (EINVAL)
        // and refusing a negative size, so both are established before here.
        System.Diagnostics.Debug.Assert (offset >= 0L, "readTransferCount: offset must not be negative")
        System.Diagnostics.Debug.Assert (count >= 0, "readTransferCount: count must not be negative")
        System.Diagnostics.Debug.Assert (length >= 0, "readTransferCount: length must not be negative")

        if offset >= int64 length then
            // Includes an offset beyond `int` range, which no seeded file can
            // reach but a process can certainly ask for.
            0
        else
            // `length - offset` is in `(0, length]` here, so the `int` conversion
            // cannot overflow however large `offset` was.
            min (int64 count) (int64 length - offset) |> int

    /// The most bytes a regular file in this model can hold: `Array.MaxLength`,
    /// contents being an `ImmutableArray<byte>`.
    ///
    /// Not any real filesystem's ceiling — ext4's is about 16 TiB and APFS's is
    /// vastly larger — and reaching it is `FileWriteRefusal.WouldExceedMaxLength`
    /// rather than an errno for that reason.
    let maxFileLength : int64 = int64 System.Array.MaxLength

    /// How long a regular file becomes when `count` bytes are written at
    /// `offset` into contents `length` bytes long, or the refusal if that is
    /// more than this model can hold.
    ///
    /// An empty write leaves the length *exactly* as it was, however far past the
    /// end it was aimed: measured on both platforms, `pwrite(fd, buf, 0, 10000)`
    /// on a four-byte file leaves it four bytes long. So a caller must not infer
    /// a file's new length from `offset` and the count alone.
    ///
    /// Separate from `writtenContents` so that both sides of the ceiling can be
    /// checked without allocating two gigabytes to do it.
    let internal writtenLength (offset : int64) (count : int) (length : int) : Result<int, FileWriteRefusal> =
        System.Diagnostics.Debug.Assert (offset >= 0L, "writtenLength: offset must not be negative")
        System.Diagnostics.Debug.Assert (count >= 0, "writtenLength: count must not be negative")
        System.Diagnostics.Debug.Assert (length >= 0, "writtenLength: length must not be negative")

        if count = 0 then
            Ok length
        else if

            // Rearranged to subtract rather than add, so that an offset near the top
            // of the `int64` range is refused instead of wrapping onto a low sum the
            // comparison would accept. Both operands of the subtraction are
            // non-negative, so it cannot underflow.
            offset > maxFileLength - int64 count
        then
            Error (FileWriteRefusal.WouldExceedMaxLength (offset, count))
        else
            // Bounded by `maxFileLength` just above, so the `int` conversion is
            // exact however large `offset` was.
            Ok (max (int64 length) (offset + int64 count) |> int)

    /// The contents a regular file holds after `bytes` are written at `offset`.
    ///
    /// Bytes between the old end of the file and `offset` read as zero, which is
    /// what a real filesystem reports for the hole a sparse write leaves
    /// (measured on ext4 and APFS). A write landing inside the file overwrites
    /// in place, and never truncates what follows it.
    ///
    /// Separated from `writeFile` for the reason `readTransferCount` is
    /// separated from the syscalls that use it: as a function of a byte array, an
    /// offset and a byte array it is property-testable against naive splicing,
    /// where the same arithmetic inlined into a syscall is reachable only
    /// through a whole simulated system.
    let internal writtenContents
        (contents : ImmutableArray<byte>)
        (offset : int64)
        (bytes : ImmutableArray<byte>)
        : Result<ImmutableArray<byte>, FileWriteRefusal>
        =
        // Both are `ImmutableArray`, a struct wrapping an array, so `default`
        // carries a null one: it would throw on the first `Length` read rather
        // than at the point the mistake was made. Rejected rather than treated as
        // empty for the reason `createFile` gives.
        if contents.IsDefault then
            failwith
                "VirtualFileSystem.writtenContents: contents is the default ImmutableArray, whose underlying array is null. That is not an empty file; pass ImmutableArray<byte>.Empty."

        if bytes.IsDefault then
            failwith
                "VirtualFileSystem.writtenContents: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        // The caller is responsible for rejecting a negative offset (EINVAL), so
        // it is established before here.
        System.Diagnostics.Debug.Assert (offset >= 0L, "writtenContents: offset must not be negative")

        if bytes.IsEmpty then
            // Not merely an optimisation: the contents must come back untouched
            // rather than zero-extended to `offset`. See `writtenLength`.
            Ok contents
        else

        match writtenLength offset bytes.Length contents.Length with
        | Error refusal -> Error refusal
        | Ok length ->

        // Zero-initialised, which is what fills the hole between the old end of
        // the file and `offset` when there is one; where there is not, every byte
        // is overwritten by one of the two copies below.
        let result = Array.zeroCreate<byte> length
        contents.CopyTo result
        bytes.CopyTo (0, result, int offset, bytes.Length)

        // Wrapped rather than copied: nothing else holds `result`, and a file can
        // be two gigabytes long.
        Ok (System.Runtime.InteropServices.ImmutableCollectionsMarshal.AsImmutableArray result)

    /// The length a regular file becomes when truncated to `length`, or the
    /// refusal if that is more than this model can hold.
    ///
    /// Separate from `truncatedContents` for the reason `writtenLength` is
    /// separate from `writtenContents`: it is the only way to check both sides of
    /// the ceiling without allocating two gigabytes to do it.
    ///
    /// A negative length is the caller's to reject (EINVAL), so it is
    /// established before here.
    let internal truncatedLength (length : int64) : Result<int, FileTruncationRefusal> =
        System.Diagnostics.Debug.Assert (length >= 0L, "truncatedLength: length must not be negative")

        if length > maxFileLength then
            Error (FileTruncationRefusal.WouldExceedMaxLength length)
        else
            // Bounded by `maxFileLength` just above, so this conversion is exact.
            Ok (int length)

    /// The contents a regular file holds after being truncated to `length`.
    ///
    /// Shortening discards the tail; lengthening zero-fills, which is what a real
    /// filesystem reports for the hole (measured on ext4 and APFS). A truncation
    /// to the length the file already has returns it unchanged — but that is
    /// *not* a licence for the caller to skip the operation, because the inode's
    /// timestamps and set-ID bits move regardless; see `truncateFile`.
    ///
    /// Separated from `truncateFile` for the reason `writtenContents` is
    /// separated from `writeFile`: as a function of a byte array and a length it
    /// is property-testable against naive take/pad, where the same arithmetic
    /// inlined into a syscall is reachable only through a whole simulated system.
    let internal truncatedContents
        (contents : ImmutableArray<byte>)
        (length : int64)
        : Result<ImmutableArray<byte>, FileTruncationRefusal>
        =
        if contents.IsDefault then
            failwith
                "VirtualFileSystem.truncatedContents: contents is the default ImmutableArray, whose underlying array is null. That is not an empty file; pass ImmutableArray<byte>.Empty."

        match truncatedLength length with
        | Error refusal -> Error refusal
        | Ok length ->

        if length = contents.Length then
            Ok contents
        elif length < contents.Length then
            Ok (ImmutableArray.CreateRange (Seq.truncate length contents))
        else

        // Zero-initialised, which is exactly what the extension reads as.
        let result = Array.zeroCreate<byte> length
        contents.CopyTo result
        Ok (ImmutableArray.CreateRange result)

    /// Where `lseek(2)` would land, given where it is measuring from.
    ///
    /// The whole of what `lseek` computes, separated out for the same reason as
    /// `readTransferCount`: as a function of four integers it is
    /// property-testable, where the same arithmetic inlined in a syscall is
    /// reachable only through a whole simulated system.
    ///
    /// **Not bounded above by `size`.** Seeking past the end of a file is legal
    /// — it is how sparse files are made — and a subsequent read there simply
    /// transfers nothing. The only rejections are the two `SeekFault` cases.
    ///
    /// **No filesystem ceiling either.** A real Linux rejects an offset above
    /// the filesystem's `s_maxbytes` with `EINVAL`: measured, ext4 stops at
    /// `0xffffffff000` while **tmpfs accepts the full `int64` range**, as does
    /// macOS's APFS. This library's filesystem is in memory, so tmpfs is the honest
    /// analogue and the ceiling is `Int64.MaxValue`. The divergence is a
    /// *filesystem* difference, not a platform one, even though a dev box's
    /// APFS accepts what a CI container's ext4 refuses.
    /// **The size is deferred**: only `SEEK_END` consults it, and there are
    /// descriptors with no size this kernel will state — a directory on an NFS
    /// mount, whose size is the server's (see `UnixDescriptor.lseek`). Seeking
    /// such a descriptor with `SEEK_SET` or `SEEK_CUR` is portable and must keep
    /// working, so the caller passes a thunk that refuses, and only the `End`
    /// case forces it.
    let internal seekTarget
        (whence : SeekWhence)
        (current : int64)
        (size : Lazy<int64>)
        (offset : int64)
        : Result<int64, SeekFault>
        =
        // A property of the model rather than of the caller: a description's
        // offset is established non-negative by this very function.
        System.Diagnostics.Debug.Assert (current >= 0L, "seekTarget: the current offset must not be negative")

        let basis =
            match whence with
            | SeekWhence.Set -> 0L
            | SeekWhence.Current -> current
            | SeekWhence.End ->
                let size = size.Force ()

                System.Diagnostics.Debug.Assert (size >= 0L, "seekTarget: the file size must not be negative")

                size

        // Checked addition by inspection rather than by `Checked.(+)`, so that
        // overflow is a value this function returns rather than an exception
        // its caller must catch. `basis` is non-negative, so only a positive
        // `offset` can carry past `Int64.MaxValue`.
        if offset > 0L && basis > System.Int64.MaxValue - offset then
            Error SeekFault.Overflow
        else

        let target = basis + offset

        if target < 0L then Error SeekFault.Negative else Ok target

    /// The directory at `inode`, or `None` if it is absent or is not a
    /// directory. Honest about which: callers that must distinguish ENOENT from
    /// ENOTDIR use `tryGetContent` and match.
    let internal tryGetDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : DirectoryContent option =
        match tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Some directory
        | Some _
        | None -> None

    let private allocate
        (content : InodeContent)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : InodeNumber * VirtualFileSystem
        =
        let inode = vfs.NextInode
        let (InodeNumber raw) = inode

        let vfs =
            { vfs with
                Inodes =
                    Map.add
                        inode
                        {
                            Content = content
                            Times = InodeTimes.createdAt now
                            Owner = owner
                        }
                        vfs.Inodes
                NextInode = InodeNumber (raw + 1L)
            }

        inode, vfs

    /// `counts` with the number of entries naming `inode` moved by `delta`,
    /// dropping it from the map when it reaches zero.
    let private adjustBindingCount
        (inode : InodeNumber)
        (delta : int)
        (counts : Map<InodeNumber, int>)
        : Map<InodeNumber, int>
        =
        let current = Map.tryFind inode counts |> Option.defaultValue 0
        let updated = current + delta

        if updated < 0 then
            failwith
                $"VirtualFileSystem: inode %O{inode} was named by %d{current} entries, and removing %d{-delta} would leave a negative count. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."
        elif updated = 0 then
            Map.remove inode counts
        else
            Map.add inode updated counts

    /// `counts` with the number of subdirectories `directory` holds moved by
    /// `delta`, dropping it from the map when it reaches zero.
    let private adjustSubdirectoryCount
        (directory : InodeNumber)
        (delta : int)
        (counts : Map<InodeNumber, int>)
        : Map<InodeNumber, int>
        =
        let current = Map.tryFind directory counts |> Option.defaultValue 0
        let updated = current + delta

        if updated < 0 then
            failwith
                $"VirtualFileSystem: directory inode %O{directory} held %d{current} subdirectories, and removing %d{-delta} would leave a negative count. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."
        elif updated = 0 then
            Map.remove directory counts
        else
            Map.add directory updated counts

    /// Whether `inode` is a directory in `inodes`.
    let private isDirectoryIn (inodes : Map<InodeNumber, Inode>) (inode : InodeNumber) : bool =
        match Map.tryFind inode inodes with
        | Some {
                   Content = InodeContent.Directory _
               } -> true
        | Some _
        | None -> false

    /// `sortedNames` with `name` added to the names stored for `directory`,
    /// which must not already hold it.
    let private addSortedName
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (sortedNames : Map<InodeNumber, SortedEntryNames>)
        : Map<InodeNumber, SortedEntryNames>
        =
        let current =
            Map.tryFind directory sortedNames |> Option.defaultValue SortedEntryNames.empty

        if SortedEntryNames.contains name current then
            failwith
                $"VirtualFileSystem: \"%s{DirectoryEntryName.toEscaped name}\" is already among the names stored for directory inode %O{directory}, which did not bind it. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        Map.add directory (SortedEntryNames.add name current) sortedNames

    /// `sortedNames` with `name` removed from the names stored for
    /// `directory`, which must hold it, dropping the directory from the map
    /// when it is left binding nothing.
    let private removeSortedName
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (sortedNames : Map<InodeNumber, SortedEntryNames>)
        : Map<InodeNumber, SortedEntryNames>
        =
        let current =
            Map.tryFind directory sortedNames |> Option.defaultValue SortedEntryNames.empty

        if not (SortedEntryNames.contains name current) then
            failwith
                $"VirtualFileSystem: \"%s{DirectoryEntryName.toEscaped name}\" is not among the names stored for directory inode %O{directory}, which bound it. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        let updated = SortedEntryNames.remove name current

        if SortedEntryNames.isEmpty updated then
            Map.remove directory sortedNames
        else
            Map.add directory updated sortedNames

    /// Whether `name` could be bound in `directory` right now, with the errno
    /// the attempt would otherwise fail with.
    ///
    /// Separate from `bind` because the creators must check the *parent* before
    /// allocating the child. Allocating first is not merely wasteful: a
    /// `directory` that does not exist but happens to equal `NextInode` would
    /// be *created* by the allocation, so `bind` would then find it, bind the
    /// new inode as its own child, and return `Ok` for a filesystem unreachable
    /// from the root — instead of the ENOENT the operation promises.
    let private ensureBindable
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (vfs : VirtualFileSystem)
        : Result<unit, UnixError>
        =
        assertOnRootFileSystem "ensureBindable" directory vfs

        match tryGetContent directory vfs with
        | None -> Error UnixError.ENOENT
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.CharacterDevice _)
        | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR
        | Some (InodeContent.Directory content) ->
            if
                Map.containsKey
                    (DirectoryEntryName.assertValid "VirtualFileSystem: directory entry name" name)
                    content.Entries
            then
                Error UnixError.EEXIST
            else
                Ok ()

    /// Bind `name` to `inode` in `directory`, which must exist, be a directory,
    /// and not already hold `name`.
    let private bind
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (inode : InodeNumber)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        // Every builder binds through here, so this is the one place a name
        // enters the graph — and the one place a forged `default(FileName)` can
        // be stopped before it becomes an entry no path could ever name.
        let name =
            DirectoryEntryName.assertValid "VirtualFileSystem: directory entry name" name

        match Map.tryFind directory vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some ({
                    Content = InodeContent.RegularFile _
                })
        | Some ({
                    Content = InodeContent.CharacterDevice _
                })
        | Some ({
                    Content = InodeContent.Symlink _
                }) -> Error UnixError.ENOTDIR
        | Some ({
                    Content = InodeContent.Directory content
                } as existing) ->
            if Map.containsKey name content.Entries then
                Error UnixError.EEXIST
            else

            // Gaining an entry changes what the directory holds, so its `mtime`
            // moves, and with it the `ctime` of the inode describing it. Done
            // here because this is the single chokepoint through which a
            // directory ever gains an entry, so no builder can forget it.
            let updated =
                {
                    Content =
                        InodeContent.Directory
                            { content with
                                Entries = Map.add name inode content.Entries
                            }
                    Times = InodeTimes.contentsChangedAt now existing.Times
                    Owner = existing.Owner
                }

            Ok
                { vfs with
                    Inodes = Map.add directory updated vfs.Inodes
                    BindingCounts = adjustBindingCount inode 1 vfs.BindingCounts
                    SortedNames = addSortedName directory name vfs.SortedNames
                    SubdirectoryCounts =
                        if isDirectoryIn vfs.Inodes inode then
                            adjustSubdirectoryCount directory 1 vfs.SubdirectoryCounts
                        else
                            vfs.SubdirectoryCounts
                }

    /// Create an empty subdirectory owned by `owner`. Mirrors `mkdir(2)`: EEXIST
    /// if the name is taken, ENOTDIR if `directory` is not a directory, ENOENT if
    /// it is absent. Who a new inode belongs to is `InodeOwner.ofNewInode`'s
    /// decision, not this function's.
    let internal createDirectory
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated =
            allocate
                (InodeContent.Directory
                    {
                        Entries = Map.empty
                        Parent = directory
                        Permissions = permissions
                    })
                owner
                now
                vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a regular file with the given contents, owned by `owner`. Mirrors
    /// `open(2)` with `O_CREAT | O_EXCL`.
    let internal createFile
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (contents : ImmutableArray<byte>)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        // `ImmutableArray` is a struct wrapping an array, so `default` carries a
        // null one: it stores happily, passes `checkInvariants`, and throws only
        // when some later read touches `Length`. Rejected here for the same
        // reason as a forged `FileName` or `SymlinkTarget`, and deliberately
        // rejected rather than normalised to `Empty` — a caller who wrote
        // `default` meant something, and quietly turning it into an empty file
        // would hide the bug rather than surface it.
        if contents.IsDefault then
            failwith
                "VirtualFileSystem.createFile: contents is the default ImmutableArray, whose underlying array is null. That is not an empty file — it is an uninitialised value that would pass checkInvariants and then throw on the first read. Pass ImmutableArray<byte>.Empty for an empty file."

        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated =
            allocate (InodeContent.RegularFile (contents, permissions)) owner now vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a regular file under `name` in `directory` holding the contents of
    /// the regular file at `source`, with `permissions` and owned by `owner`,
    /// whose `atime`, `mtime` and birth time are `source`'s and whose `ctime`
    /// is `now`. `source` itself does not change.
    ///
    /// Partial in `source`, which must name a regular file this filesystem
    /// contains. Fails as `createFile` does when `directory` cannot take the
    /// name.
    let internal cloneFile
        (source : InodeNumber)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        let contents, sourceTimes =
            match Map.tryFind source vfs.Inodes with
            | Some {
                       Content = InodeContent.RegularFile (contents, _)
                       Times = times
                   } -> contents, times
            | Some _ ->
                failwith
                    $"VirtualFileSystem.cloneFile: inode %O{source} is not a regular file; only a regular file's contents can be cloned here (this is a bug in the caller)."
            | None ->
                failwith
                    $"VirtualFileSystem.cloneFile: inode %O{source} is not in this filesystem, but the caller resolved a path to it (this is a bug in the caller)."

        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated =
            allocate (InodeContent.RegularFile (contents, permissions)) owner now vfs

        let allocated =
            { allocated with
                Inodes =
                    Map.change
                        inode
                        (Option.map (fun entry ->
                            { entry with
                                Times =
                                    {
                                        Access = sourceTimes.Access
                                        Modification = sourceTimes.Modification
                                        StatusChange = now
                                        Birth = sourceTimes.Birth
                                    }
                            }
                        ))
                        allocated.Inodes
            }

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a symbolic link holding `target` verbatim, with permission bits
    /// `permissions`, owned by `owner`. Mirrors `symlink(2)`, including that the
    /// target is not resolved, need not exist, and may be relative. An empty
    /// target is unrepresentable by construction; see `SymlinkTargetError.Empty`.
    ///
    /// Which bits a link may have is a rule of the flavour, which this
    /// filesystem does not know: `SimulatedUnixPlatform.symlinkCreationPermissions`
    /// gives them, and `UnixSystem.checkInvariants` holds a system to it.
    let internal createSymlink
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (target : SymlinkTarget)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        let target = SymlinkTarget.assertValid "VirtualFileSystem.createSymlink" target

        let permissions =
            PermissionBits.assertValid "VirtualFileSystem.createSymlink" permissions

        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated =
            allocate (InodeContent.Symlink (target, permissions)) owner now vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Bind an existing inode under a second name. Mirrors `link(2)`, including
    /// its refusal to hard-link a directory (EPERM): that would make the graph
    /// a non-tree, and a directory's `Parent` could then name only one of its
    /// containers.
    let internal hardLink
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (target : InodeNumber)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        assertOnRootFileSystem "hardLink" directory vfs
        assertOnRootFileSystem "hardLink" target vfs

        match Map.tryFind target vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some {
                   Content = InodeContent.Directory _
               } -> Error UnixError.EPERM
        | Some ({
                    Content = InodeContent.RegularFile _
                } as existing)
        | Some ({
                    Content = InodeContent.CharacterDevice _
                } as existing)
        | Some ({
                    Content = InodeContent.Symlink _
                } as existing) ->
            match bind directory name target now vfs with
            | Error error -> Error error
            | Ok bound ->
                // The target's own `ctime` moves too: its link count changed,
                // which is a change to the inode even though its contents are
                // untouched. Its `mtime` does not. (`bind` has already moved the
                // *directory's* pair.)
                Ok
                    { bound with
                        Inodes =
                            Map.add
                                target
                                { existing with
                                    Times = InodeTimes.statusChangedAt now existing.Times
                                }
                                bound.Inodes
                    }

    /// Remove `name` from `directory`, answering the inode it named. Mirrors
    /// the *naming* half of `unlink(2)` and `rmdir(2)`: ENOENT if `directory`
    /// is absent or does not hold `name`, ENOTDIR if `directory` is not a
    /// directory.
    ///
    /// `effect` says what the removal did to the inode that lost the name, which
    /// the two syscalls do not agree on: see `UnbindTargetEffect`. The directory
    /// losing the entry is stamped the same way either way.
    ///
    /// Removing the last name an inode has does **not** remove the inode, and
    /// this function deliberately cannot: a real kernel keeps an unlinked inode
    /// alive for as long as any process holds it open, and whether one does is a
    /// fact about the open file descriptions rather than about this graph. The caller
    /// that can see both decides, and calls `forget`. Until it does, the inode
    /// is unreachable from the root, and the caller owes it to
    /// `checkInvariants` as a pinned inode.
    ///
    /// Mechanical, and it makes no policy check of its own: whether the caller
    /// was allowed to remove this name, and whether the name was one this
    /// syscall may remove at all, are the verdict's business. In particular an
    /// inode with entries of its own can be unbound — `rename(2)` moves a
    /// populated directory by unbinding and rebinding it, and the subtree is
    /// legitimately unreachable in between.
    let internal unbind
        (effect : UnbindTargetEffect)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        // As in `bind`, so that a forged `default(FileName)` is stopped at the
        // one chokepoint through which a directory ever loses an entry rather
        // than silently matching nothing.
        let name =
            DirectoryEntryName.assertValid "VirtualFileSystem: directory entry name" name

        match Map.tryFind directory vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some {
                   Content = InodeContent.RegularFile _
               }
        | Some {
                   Content = InodeContent.CharacterDevice _
               }
        | Some {
                   Content = InodeContent.Symlink _
               } -> Error UnixError.ENOTDIR
        | Some ({
                    Content = InodeContent.Directory content
                } as existing) ->

        match Map.tryFind name content.Entries with
        | None -> Error UnixError.ENOENT
        | Some target ->

        assertOnRootFileSystem "unbind" directory vfs
        assertNotMountRoot "unbind" target vfs

        // Losing an entry changes what the directory holds, so its `mtime`
        // moves and with it the `ctime` of the inode describing it -- the exact
        // mirror of `bind`, and measured to be so on both platforms.
        let updated =
            {
                Content =
                    InodeContent.Directory
                        { content with
                            Entries = Map.remove name content.Entries
                        }
                Times = InodeTimes.contentsChangedAt now existing.Times
                Owner = existing.Owner
            }

        let inodes = Map.add directory updated vfs.Inodes

        // Whether the target's own `ctime` moves is `effect`'s business, but the
        // target is looked up either way: a name bound to an inode the graph
        // does not contain is a broken graph whichever caller asked.
        let inodes =
            match Map.tryFind target inodes with
            | Some node ->
                match effect with
                | UnbindTargetEffect.Untouched -> inodes
                | UnbindTargetEffect.LostALink ->
                    // `mtime` does not move: the inode's contents are untouched,
                    // only the count of names pointing at it.
                    Map.add
                        target
                        { node with
                            Times = InodeTimes.statusChangedAt now node.Times
                        }
                        inodes
            | None ->
                failwith
                    $"VirtualFileSystem.unbind: directory inode %O{directory} bound \"%s{DirectoryEntryName.toEscaped name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        Ok (
            target,
            { vfs with
                Inodes = inodes
                BindingCounts = adjustBindingCount target -1 vfs.BindingCounts
                SortedNames = removeSortedName directory name vfs.SortedNames
                SubdirectoryCounts =
                    if isDirectoryIn inodes target then
                        adjustSubdirectoryCount directory -1 vfs.SubdirectoryCounts
                    else
                        vfs.SubdirectoryCounts
            }
        )

    /// How many directory entries name `inode`.
    ///
    /// This is `st_nlink` as a regular file or a symbolic link reports it. A
    /// directory's `st_nlink` is a rule of the filesystem it is on, which
    /// counts some of its entries rather than the one that names it: see
    /// `EmulatedFileSystemType.directoryLinkCount`.
    ///
    /// Zero means the inode has no name: either it is the root, or its last link
    /// has gone and only a descriptor is keeping it alive. Zero too for an inode
    /// the filesystem does not contain.
    ///
    /// Answered from a count the filesystem keeps as names come and go, so it
    /// costs a lookup rather than a scan of every directory.
    let bindingCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        Map.tryFind inode vfs.BindingCounts |> Option.defaultValue 0

    /// How many names the directory at `inode` binds, besides "." and "..".
    /// Zero for anything that is not a directory this filesystem contains.
    ///
    /// Answered from the names the filesystem keeps sorted, so it costs a
    /// lookup rather than a walk of the directory.
    let entryCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        match Map.tryFind inode vfs.SortedNames with
        | Some names -> SortedEntryNames.count names
        | None -> 0

    /// How many of the directory at `inode`'s entries name a directory. A
    /// symbolic link to a directory is not one. Zero for anything that is not
    /// a directory this filesystem contains.
    ///
    /// Answered from a count the filesystem keeps as names come and go, so it
    /// costs a lookup rather than a walk of the directory.
    let subdirectoryCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        Map.tryFind inode vfs.SubdirectoryCounts |> Option.defaultValue 0

    /// Whether `inode` is a directory that no path from the root can reach: its
    /// last name has gone, and only a descriptor or the current directory is
    /// keeping it alive.
    ///
    /// A real kernel refuses to create anything inside such a directory —
    /// `mkdir`, `open(O_CREAT)` and `symlink` are all ENOENT there, measured on
    /// both flavours — so a caller that is about to add a name must ask. That
    /// rule is also what keeps an orphan *empty*: a directory is orphaned only
    /// by `rmdir`, or by a `rename` that displaces it, both of which refuse a
    /// non-empty one, and it can never gain an entry afterwards.
    ///
    /// False for anything that is not a directory. A file with no names left is
    /// orphaned in the same sense, but nothing can be created inside it, so no
    /// caller has the question to ask.
    let isOrphanedDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        if inode = vfs.Root then
            false
        else

        match Map.tryFind inode vfs.Inodes with
        | Some {
                   Content = InodeContent.Directory _
               } -> bindingCount inode vfs = 0
        | Some {
                   Content = InodeContent.RegularFile _
               }
        | Some {
                   Content = InodeContent.CharacterDevice _
               }
        | Some {
                   Content = InodeContent.Symlink _
               }
        | None -> false

    /// Whether `candidate` is `root` itself, or a directory somewhere beneath
    /// it, by climbing `DirectoryContent.Parent`.
    ///
    /// This is the question `rename(2)` asks before moving a directory: a move
    /// whose destination lies inside the thing being moved detaches a cycle
    /// from the root, and both kernels answer EINVAL. Measured, the rule is on
    /// *inodes* rather than on path text — with `link -> a/b`,
    /// `rename("a", "link/inner")` is EINVAL although neither path is a prefix
    /// of the other, and `rename("a", "ab")` succeeds although one is.
    ///
    /// `root` need not be a directory: a non-directory never appears in any
    /// parent chain, so the honest answer for one is `false`. `candidate` must
    /// be a directory this filesystem contains, because a non-directory has no
    /// `..` to climb and no caller has that question — every caller obtains it
    /// from a resolution that has just named it as the directory a new entry
    /// would go into.
    let internal isWithinSubtree (root : InodeNumber) (candidate : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match tryGetDirectory candidate vfs with
        | None ->
            failwith
                $"VirtualFileSystem.isWithinSubtree: inode %O{candidate} is not a directory this filesystem contains, so it has no parent chain to climb. Only a directory can be the parent of a new entry."
        | Some _ ->

        // `visited` is not paranoia about this module's own operations, which
        // preserve tree-ness: `checkInvariants` can be handed a filesystem
        // assembled by a test, and a query that hangs would hang the suite
        // rather than fail it.
        let rec climb (current : InodeNumber) (visited : Set<InodeNumber>) : bool =
            if current = root then
                true
            elif Set.contains current visited then
                false
            elif current = vfs.Root then
                false
            else

            match tryGetDirectory current vfs with
            | None -> false
            | Some content -> climb content.Parent (Set.add current visited)

        climb candidate Set.empty

    /// Move the binding of `sourceName` in `sourceDirectory` to
    /// `destinationName` in `destinationDirectory`, displacing whatever was
    /// bound there. The naming half of `rename(2)`.
    ///
    /// Answers what the destination name was bound to before, if anything, for
    /// the caller to reap: as in `unbind`, this module cannot see whether a
    /// descriptor still holds it, so it frees nothing and the displaced inode
    /// is owed to `checkInvariants` as a pinned inode until the caller decides.
    ///
    /// Not `unbind` followed by `hardLink`, because that composition cannot
    /// express a directory move at all — `bind` is private, so there is no
    /// public way to attach a directory to a new parent, and `hardLink` refuses
    /// a directory with EPERM by design. For a *non-directory* source the two
    /// agree exactly, timestamps included, which is what makes the composition
    /// a usable reference implementation for that half of the domain.
    ///
    /// Makes no permission check and imposes no type rule: which caller may
    /// move what, and which of the several possible refusals wins, is the
    /// verdict's measured business and diverges between the flavours. What this
    /// function does insist on is that the *graph* survives, because a caller
    /// that got past its verdict with any of these four cannot leave a
    /// filesystem a kernel could produce:
    ///
    ///  * the two paths naming one inode. That is `rename(2)`'s no-op, which
    ///    succeeds and changes nothing at all — and whose position in the
    ///    ordering is one of the things the flavours disagree about, so it
    ///    belongs to the verdict rather than to a short-circuit here.
    ///  * a **populated directory** at the destination. Displacing it would
    ///    strand its children unreachable from the root, since a caller reaping
    ///    the displaced inode climbs parents rather than descending.
    ///  * a destination directory **inside the source's own subtree**, which
    ///    detaches a cycle.
    ///  * a destination directory whose own last name has gone. Binding into an
    ///    orphan strands the moved inode: it keeps a name, so nothing reaps it,
    ///    and no path reaches it. `mkdir`, `open(O_CREAT)` and `symlink` all
    ///    answer ENOENT there — measured on both kernels — and rename is the
    ///    third syscall that adds a name, so it owes the same.
    ///    This is also what keeps `isOrphanedDirectory`'s stated invariant true:
    ///    an orphan is empty because `rmdir` refuses a populated directory *and*
    ///    nothing can afterwards put an entry into one.
    let internal rename
        (sourceDirectory : InodeNumber)
        (sourceName : DirectoryEntryName)
        (destinationDirectory : InodeNumber)
        (destinationName : DirectoryEntryName)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<RenameOutcome * VirtualFileSystem, UnixError>
        =
        // As in `bind` and `unbind`, so that a forged `default(FileName)` is
        // stopped before it becomes an entry no path could name.
        let sourceName =
            DirectoryEntryName.assertValid "VirtualFileSystem: directory entry name" sourceName

        let destinationName =
            DirectoryEntryName.assertValid "VirtualFileSystem: directory entry name" destinationName

        match tryGetDirectory sourceDirectory vfs, tryGet sourceDirectory vfs with
        | None, Some _ -> Error UnixError.ENOTDIR
        | None, None -> Error UnixError.ENOENT
        | Some sourceContent, _ ->

        match tryGetDirectory destinationDirectory vfs, tryGet destinationDirectory vfs with
        | None, Some _ -> Error UnixError.ENOTDIR
        | None, None -> Error UnixError.ENOENT
        | Some destinationContent, _ ->

        match Map.tryFind sourceName sourceContent.Entries with
        | None -> Error UnixError.ENOENT
        | Some moved ->

        assertOnRootFileSystem "rename" sourceDirectory vfs
        assertOnRootFileSystem "rename" destinationDirectory vfs
        assertNotMountRoot "rename" moved vfs

        if isOrphanedDirectory destinationDirectory vfs then
            failwith
                $"VirtualFileSystem.rename: the destination directory %O{destinationDirectory} has lost its last name, so binding \"%s{DirectoryEntryName.toEscaped destinationName}\" into it would make inode %O{moved} unreachable from the root while it still has a name -- which nothing could then reap. The verdict owes ENOENT, exactly as it does for the creating operations."

        let displaced = Map.tryFind destinationName destinationContent.Entries

        displaced
        |> Option.iter (fun displaced -> assertNotMountRoot "rename" displaced vfs)

        if displaced = Some moved then
            failwith
                $"VirtualFileSystem.rename: \"%s{DirectoryEntryName.toEscaped sourceName}\" in inode %O{sourceDirectory} and \"%s{DirectoryEntryName.toEscaped destinationName}\" in inode %O{destinationDirectory} both name inode %O{moved}. That is rename(2)'s no-op, which changes nothing at all; the verdict must answer it rather than calling this."

        // Before the populated-destination check below, and the order is
        // load-bearing rather than arbitrary: the two overlap on
        // `rename(a, a/b)` with `a/b` populated, and both kernels answer
        // EINVAL there rather than ENOTEMPTY. Either arm would refuse, but
        // only this one names the errno the verdict will owe.
        match tryGetDirectory moved vfs with
        | Some _ when isWithinSubtree moved destinationDirectory vfs ->
            failwith
                $"VirtualFileSystem.rename: the destination directory %O{destinationDirectory} is inode %O{moved} itself or lies beneath it, so moving it there would detach a cycle from the root; the verdict owes EINVAL."
        | Some _
        | None ->

        match displaced |> Option.bind (fun inode -> tryGetDirectory inode vfs) with
        | Some content when not (Map.isEmpty content.Entries) ->
            failwith
                $"VirtualFileSystem.rename: the destination \"%s{DirectoryEntryName.toEscaped destinationName}\" in inode %O{destinationDirectory} names directory inode %O{displaced.Value}, which holds %i{Map.count content.Entries} entries. Displacing it would strand them unreachable from the root; the verdict owes ENOTEMPTY."
        | Some _
        | None ->

        // Both directories gain or lose an entry, so each one's `mtime` moves --
        // and when they are the same inode that is one stamp, not two, because
        // every stamp in one rename carries the same `now`.
        let inodes =
            if sourceDirectory = destinationDirectory then
                let entries =
                    sourceContent.Entries |> Map.remove sourceName |> Map.add destinationName moved

                let existing = Map.find sourceDirectory vfs.Inodes

                Map.add
                    sourceDirectory
                    {
                        Content =
                            InodeContent.Directory
                                { sourceContent with
                                    Entries = entries
                                }
                        Times = InodeTimes.contentsChangedAt now existing.Times
                        Owner = existing.Owner
                    }
                    vfs.Inodes
            else

            let source = Map.find sourceDirectory vfs.Inodes
            let destination = Map.find destinationDirectory vfs.Inodes

            vfs.Inodes
            |> Map.add
                sourceDirectory
                {
                    Content =
                        InodeContent.Directory
                            { sourceContent with
                                Entries = Map.remove sourceName sourceContent.Entries
                            }
                    Times = InodeTimes.contentsChangedAt now source.Times
                    Owner = source.Owner
                }
            |> Map.add
                destinationDirectory
                {
                    Content =
                        InodeContent.Directory
                            { destinationContent with
                                Entries = Map.add destinationName moved destinationContent.Entries
                            }
                    Times = InodeTimes.contentsChangedAt now destination.Times
                    Owner = destination.Owner
                }

        // The moved inode's `ctime` moves and its `mtime` does not: what changed
        // is which directory names it, not what it holds. Measured on both
        // kernels for a file and for a directory, and whether or not the parent
        // changed.
        //
        // A moved *directory* also carries its own ".." entry, which is the
        // physical parent rather than the lexical one, so a move to a new parent
        // rewrites it. Both kernels demand the write bit on the moved directory
        // for exactly this rewrite, and demand nothing when the parent is
        // unchanged -- which is the verdict's business, but is the reason this
        // is a real mutation rather than bookkeeping.
        let inodes =
            let existing =
                match Map.tryFind moved inodes with
                | Some node -> node
                | None ->
                    failwith
                        $"VirtualFileSystem.rename: directory inode %O{sourceDirectory} bound \"%s{DirectoryEntryName.toEscaped sourceName}\" to inode %O{moved}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

            let content =
                match existing.Content with
                | InodeContent.Directory content when destinationDirectory <> sourceDirectory ->
                    InodeContent.Directory
                        { content with
                            Parent = destinationDirectory
                        }
                | other -> other

            Map.add
                moved
                {
                    Content = content
                    Times = InodeTimes.statusChangedAt now existing.Times
                    Owner = existing.Owner
                }
                inodes

        // A displaced inode lost a name, so its `ctime` moves and nothing else
        // does -- `UnbindTargetEffect.LostALink`. Measured on both kernels for
        // both kinds a destination can be, and the second row is not a
        // generalisation of the first: a displaced *file* through a surviving
        // hard link, and a displaced empty *directory* through a descriptor held
        // across the call. The directory row had to be measured separately
        // because `rmdir`'s does not agree with it -- there Darwin leaves the
        // removed directory's inode alone (`RmDirRules.RemovedDirectoryEffect`
        // is `Untouched`) where Linux stamps it. Under `rename` both kernels
        // stamp, so this needs no per-flavour effect parameter the way `unbind`
        // does.
        let inodes =
            match displaced with
            | None -> inodes
            | Some displaced ->
                match Map.tryFind displaced inodes with
                | Some node ->
                    Map.add
                        displaced
                        { node with
                            Times = InodeTimes.statusChangedAt now node.Times
                        }
                        inodes
                | None ->
                    failwith
                        $"VirtualFileSystem.rename: directory inode %O{destinationDirectory} bound \"%s{DirectoryEntryName.toEscaped destinationName}\" to inode %O{displaced}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        // The moved inode loses one name and gains another, so only a displaced
        // inode's count changes.
        let counts =
            match displaced with
            | None -> vfs.BindingCounts
            | Some displaced -> adjustBindingCount displaced -1 vfs.BindingCounts

        // A displaced inode leaves its name bound, now to the moved inode, so
        // the destination's names gain one only when nothing was displaced.
        let sortedNames =
            let withoutSource = removeSortedName sourceDirectory sourceName vfs.SortedNames

            match displaced with
            | None -> addSortedName destinationDirectory destinationName withoutSource
            | Some _ -> withoutSource

        // A moved directory leaves the source's subdirectories for the
        // destination's (one directory's both, when they are the same), and a
        // displaced directory leaves the destination's.
        let subdirectoryCounts =
            let afterMove =
                if isDirectoryIn inodes moved then
                    vfs.SubdirectoryCounts
                    |> adjustSubdirectoryCount sourceDirectory -1
                    |> adjustSubdirectoryCount destinationDirectory 1
                else
                    vfs.SubdirectoryCounts

            match displaced with
            | Some displaced when isDirectoryIn inodes displaced ->
                adjustSubdirectoryCount destinationDirectory -1 afterMove
            | Some _
            | None -> afterMove

        Ok (
            {
                Displaced = displaced
            },
            { vfs with
                Inodes = inodes
                BindingCounts = counts
                SortedNames = sortedNames
                SubdirectoryCounts = subdirectoryCounts
            }
        )

    /// The next entry an open directory stream over `directory` hands back, and
    /// the cursor to resume from.
    ///
    /// `None` is end-of-stream. The names come first, in the order
    /// `DirectoryContent.Entries` holds them, and then `..` and `.` — in that
    /// order, at the *end*.
    ///
    /// That is a measured order rather than an invented one: a directory holding
    /// the single name `z` enumerates as `z .. .` on CI's ext4, where it
    /// enumerates as `. .. z` on APFS. Both are lawful, `readdir(3)` fixes no
    /// position for anything, and this is the less convenient of the two — it
    /// refuses a program that consumes two entries to skip the dots, or that
    /// expects the first entry to be one. A program doing either is already broken
    /// on ext4, and the point of this simulation is to say so deterministically
    /// rather than on whichever machine happens to run it.
    ///
    /// No caller may compare an enumeration order against a host: the order
    /// among the names is the map's, which matches no kernel at all.
    ///
    /// Each call costs time logarithmic in the number of names the directory
    /// binds, whatever the cursor, so a whole enumeration of `n` names costs
    /// `n log n`.
    ///
    /// A stream over a directory `rmdir` has since removed is at end-of-stream
    /// at once, `.` and `..` included, from every cursor position. That is the
    /// kernel's own rule rather than a choice: measured one `getdents` call at a
    /// time, both kernels stop yielding anything from a removed directory
    /// whatever had been read. (Linux says so with ENOENT, which
    /// `UnixNamespace.readDirectoryEntry` answers before it gets here; glibc
    /// turns that into end-of-stream.) `isOrphanedDirectory` is the whole test,
    /// because an orphan is empty by construction.
    let internal nextDirectoryEntry
        (directory : InodeNumber)
        (cursor : DirectoryCursor)
        (vfs : VirtualFileSystem)
        : (DirectoryStreamName * InodeNumber * DirectoryCursor) option
        =
        let content =
            match Map.tryFind directory vfs.Inodes with
            | Some {
                       Content = InodeContent.Directory content
                   } -> content
            | Some _
            | None ->
                failwith
                    $"VirtualFileSystem.nextDirectoryEntry: inode %O{directory} is not a directory this filesystem holds. A directory being read is pinned by the descriptor reading it, so this is a bug in the caller of VirtualFileSystem.nextDirectoryEntry."

        if isOrphanedDirectory directory vfs then
            None
        else

        /// The least name this directory binds that is strictly greater than
        /// `lower`, or the least of all when there is no lower bound. Sought in
        /// the names kept sorted beside the graph, because `Map` offers no
        /// "least key above" query.
        let leastAbove (lower : DirectoryEntryName option) : (DirectoryEntryName * InodeNumber) option =
            let names =
                Map.tryFind directory vfs.SortedNames
                |> Option.defaultValue SortedEntryNames.empty

            match SortedEntryNames.leastAbove lower names with
            | None -> None
            | Some name ->
                match Map.tryFind name content.Entries with
                | Some inode -> Some (name, inode)
                | None ->
                    failwith
                        $"VirtualFileSystem.nextDirectoryEntry: \"%s{DirectoryEntryName.toEscaped name}\" is among the names stored for directory inode %O{directory}, which binds no such name. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        /// The next entry when the stream is still among the names: the least
        /// name above `lower`, or — once they are exhausted — `..`, which is
        /// where this model puts the dots.
        let fromEntries (lower : DirectoryEntryName option) =
            match leastAbove lower with
            | Some (name, inode) -> Some (DirectoryStreamName.Entry name, inode, DirectoryCursor.After name)
            | None -> Some (DirectoryStreamName.DotDot, content.Parent, DirectoryCursor.ReturnedDotDot)

        match cursor with
        | DirectoryCursor.Start -> fromEntries None
        | DirectoryCursor.After name -> fromEntries (Some name)
        | DirectoryCursor.ReturnedDotDot -> Some (DirectoryStreamName.Dot, directory, DirectoryCursor.ReturnedDot)
        | DirectoryCursor.ReturnedDot -> None

    /// Remove an inode from the graph, which is what a kernel does when the last
    /// name for a file has gone *and* no open description is holding it.
    ///
    /// Partial, deliberately: the inode must be present, nothing may still
    /// name it, and if it is a directory it must be empty. All three are bugs
    /// in the caller rather than anything a process can cause — the caller has
    /// just unbound the last name and consulted the descriptor table, and a
    /// directory loses its last name only to an `rmdir` or a `rename` that
    /// displaced it, both of which refuse a populated one. Forgetting a
    /// still-bound inode would leave a dangling entry that every later walk
    /// would trip over far from here; forgetting a populated directory would
    /// strand what it holds.
    ///
    /// The number is not reused; see `VirtualFileSystem.NextInode`.
    let internal forget (inode : InodeNumber) (vfs : VirtualFileSystem) : VirtualFileSystem =
        if not (Map.containsKey inode vfs.Inodes) then
            failwith
                $"VirtualFileSystem.forget: inode %O{inode} is not in the graph, so it cannot be forgotten (this is a bug in the caller of VirtualFileSystem.forget)."

        if inode = vfs.Root then
            failwith
                "VirtualFileSystem.forget: the root cannot be forgotten; every path resolves from it (this is a bug in the caller of VirtualFileSystem.forget)."

        match Map.tryFind inode vfs.Inodes with
        | Some {
                   Content = InodeContent.Directory directory
               } when not (Map.isEmpty directory.Entries) ->
            failwith
                $"VirtualFileSystem.forget: directory inode %O{inode} still holds %d{Map.count directory.Entries} entries, so forgetting it would strand them unreachable while they went on counting as names (this is a bug in the caller of VirtualFileSystem.forget)."
        | _ -> ()

        // Nothing to adjust in `BindingCounts`: a count of zero is stored as
        // absence, and the directory emptiness above means this inode names
        // nothing either.
        match bindingCount inode vfs with
        | 0 ->
            { vfs with
                Inodes = Map.remove inode vfs.Inodes
            }
        | count ->
            failwith
                $"VirtualFileSystem.forget: inode %O{inode} is still named by %d{count} directory entry/entries, so forgetting it would leave the graph with a dangling entry (this is a bug in the caller of VirtualFileSystem.forget)."

    /// Mount `fileSystem` over the root's entry `name`, as a directory with
    /// `permissions` owned by `owner`, holding one node for each of `devices`
    /// owned by `owner` too, all created at `now`.
    ///
    /// The root may already bind `name` to an empty directory, which the mount
    /// then covers; otherwise the mount covers a directory that held nothing,
    /// whose inode number is taken from the counter. Anything else at `name` is
    /// refused: a mount over a populated directory would hide what it holds.
    let internal mountAtRoot
        (fileSystem : MountedFileSystem)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (devices : (DirectoryEntryName * CharacterDevice * PermissionBits) list)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, MountFault>
        =
        let name = DirectoryEntryName.assertValid "VirtualFileSystem.mountAtRoot" name

        let rootContent =
            match tryGetDirectory vfs.Root vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem.mountAtRoot: the root, inode %O{vfs.Root}, is not a directory this filesystem holds. Run VirtualFileSystem.checkInvariants."

        let covered =
            match Map.tryFind name rootContent.Entries with
            | None ->
                let (InodeNumber raw) = vfs.NextInode

                Ok (
                    vfs.NextInode,
                    { vfs with
                        NextInode = InodeNumber (raw + 1L)
                    }
                )
            | Some existing ->
                match tryGetDirectory existing vfs with
                | Some content when Map.isEmpty content.Entries && not (Map.containsKey existing vfs.Mounts) ->
                    match unbind UnbindTargetEffect.LostALink vfs.Root name now vfs with
                    | Ok (_, unbound) -> Ok (existing, forget existing unbound)
                    | Error error ->
                        failwith
                            $"VirtualFileSystem.mountAtRoot: unbinding \"%s{DirectoryEntryName.toEscaped name}\" from the root was refused with %O{error}, though the root binds it (this is a bug in this library)."
                | Some _
                | None -> Error (MountFault.CoveredEntryNotAnEmptyDirectory name)

        match covered with
        | Error fault -> Error fault
        | Ok (covered, vfs) ->

        let bound (context : string) (result : Result<VirtualFileSystem, UnixError>) : VirtualFileSystem =
            match result with
            | Ok vfs -> vfs
            | Error error ->
                failwith
                    $"VirtualFileSystem.mountAtRoot: binding %s{context} was refused with %O{error} (this is a bug in the caller of VirtualFileSystem.mountAtRoot, or in this library)."

        let mountRoot, vfs =
            allocate
                (InodeContent.Directory
                    {
                        Entries = Map.empty
                        Parent = vfs.Root
                        Permissions = permissions
                    })
                owner
                now
                vfs

        let vfs = bind vfs.Root name mountRoot now vfs |> bound "the mounted root"

        let vfs, members =
            devices
            |> List.fold
                (fun (vfs, members) (deviceName, device, devicePermissions) ->
                    let node, vfs =
                        allocate (InodeContent.CharacterDevice (device, devicePermissions)) owner now vfs

                    let vfs =
                        bind mountRoot deviceName node now vfs
                        |> bound $"the node \"%s{DirectoryEntryName.toEscaped deviceName}\""

                    vfs, Map.add node mountRoot members
                )
                (vfs, Map.add mountRoot mountRoot vfs.MountMembers)

        Ok
            { vfs with
                Mounts =
                    Map.add
                        mountRoot
                        {
                            Covered = covered
                            FileSystem = fileSystem
                        }
                        vfs.Mounts
                MountMembers = members
            }

    /// Write `bytes` at `offset` into the regular file at `inode`, moving its
    /// `mtime` and `ctime` and stripping whichever of its set-user-ID and
    /// set-group-ID bits `rule` says a writer with `credentials` strips. Refused,
    /// changing nothing, where `PermissionBits.afterContentChangingWrite` refuses.
    ///
    /// Those timestamps and no others: measured on both platforms, a write leaves
    /// `atime` where it was, and `birth` never moves at all.
    ///
    /// Partial in the inode, which must name a regular file this filesystem
    /// contains. A caller arrives here having resolved a descriptor open for
    /// writing, and only a regular file can be opened that way — `open(2)`
    /// answers EISDIR for a directory and resolves a symlink to whatever it names
    /// — so anything else is a bug in the caller rather than a process's error.
    ///
    /// Must not be called with an empty `bytes`: a zero-length write moves no
    /// timestamp and strips no bit, so treating it as an ordinary write of nothing
    /// would restamp the inode for a call a real kernel makes no record of. The
    /// caller short-circuits it.
    let internal writeFile
        (inode : InodeNumber)
        (offset : int64)
        (bytes : ImmutableArray<byte>)
        (rule : SetGroupIdOnWrite)
        (credentials : Credentials)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, FileWriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "VirtualFileSystem.writeFile: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; a write of no bytes must be short-circuited by the caller."

        System.Diagnostics.Debug.Assert (
            not bytes.IsEmpty,
            "writeFile: a zero-length write moves no timestamp, and must be short-circuited by the caller"
        )

        match Map.tryFind inode vfs.Inodes with
        | None ->
            failwith
                $"VirtualFileSystem.writeFile: inode %O{inode} is not in this filesystem. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."
        | Some {
                   Content = InodeContent.Directory _
               } ->
            failwith
                $"VirtualFileSystem.writeFile: inode %O{inode} is a directory, so no descriptor naming it can be open for writing — `open(2)` answers EISDIR for every write access mode. The caller resolved a writable descriptor to it anyway (this is a bug in the caller)."
        | Some {
                   Content = InodeContent.Symlink _
               } ->
            failwith
                $"VirtualFileSystem.writeFile: inode %O{inode} is a symbolic link. `open` resolves symlinks, so no descriptor should name one (this is a bug in the caller)."
        | Some {
                   Content = InodeContent.CharacterDevice _
               } ->
            failwith
                $"VirtualFileSystem.writeFile: inode %O{inode} is a character device, which holds no contents of its own; what a transfer does is its driver's business, not this filesystem's (this is a bug in the caller)."
        | Some ({
                    Content = InodeContent.RegularFile (contents, permissions)
                } as entry) ->

        match writtenContents contents offset bytes with
        | Error refusal -> Error refusal
        | Ok updated ->

        // Changing a file's contents strips its set-user-ID bit, and its
        // set-group-ID bit on whichever files `rule` says, unless the writer is
        // privileged — so this is a mode change as well as a content change, and
        // the `ctime` above covers both.
        match PermissionBits.afterContentChangingWrite rule (Standing.toward credentials entry.Owner) permissions with
        | Error refusal -> Error (FileWriteRefusal.UnmeasuredSetIdChange refusal)
        | Ok permissions ->

        Ok
            { vfs with
                Inodes =
                    Map.add
                        inode
                        { entry with
                            Content = InodeContent.RegularFile (updated, permissions)
                            Times = InodeTimes.contentsChangedAt now entry.Times
                        }
                        vfs.Inodes
            }

    /// Set the length of the regular file at `inode` to `length`, moving its
    /// `mtime` and `ctime` and clearing whichever of its set-user-ID and
    /// set-group-ID bits `rule` says a truncation by a process with
    /// `credentials` clears. Refused, changing nothing, where
    /// `PermissionBits.afterTruncation` refuses.
    ///
    /// **Unconditionally**, which is the whole of what separates this from
    /// `writeFile`. A write of no bytes is not a write and the caller must
    /// short-circuit it; a truncation to the length the file already has *is* a
    /// truncation. Measured on both platforms: `ftruncate(fd, 4)` on a four-byte
    /// file moves `mtime` and `ctime`, and on Linux non-root it strips `04755` to
    /// `00755` — as does `O_TRUNC` on a file that is already empty.
    ///
    /// Those two timestamps and no others: `atime` stays where it was and `birth`
    /// never moves, measured on both. A truncation that *fails* moves nothing,
    /// which falls out of this returning an error rather than a filesystem.
    ///
    /// Partial in the inode, which must name a regular file this filesystem
    /// contains, for the reason `writeFile` gives: a caller arrives having
    /// resolved a descriptor open for writing, and `open(2)` answers EISDIR for
    /// every write access mode on a directory and resolves a symlink to whatever
    /// it names.
    let internal truncateFile
        (inode : InodeNumber)
        (length : int64)
        (rule : SetIdBitsOnTruncation)
        (credentials : Credentials)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, FileTruncationRefusal>
        =
        // A hard check rather than a `Debug.Assert`, which a Release build
        // compiles out: a negative length reaches `Array.Take` as an empty
        // prefix, so the file would be silently emptied and stamped instead. The
        // same guard `FileDescriptorRegistry.setOffset` applies to a negative
        // offset, and for the same reason.
        if length < 0L then
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} was asked to become %d{length} bytes, which is negative. No kernel permits it; the caller must reject this as EINVAL before committing it (this is a bug in the caller)."


        match Map.tryFind inode vfs.Inodes with
        | None ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is not in this filesystem. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."
        | Some {
                   Content = InodeContent.Directory _
               } ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is a directory, so no descriptor naming it can be open for writing — `open(2)` answers EISDIR for every write access mode, and `ftruncate(2)` answers EINVAL for the read-only descriptor that is left. The caller resolved a writable descriptor to it anyway (this is a bug in the caller)."
        | Some {
                   Content = InodeContent.Symlink _
               } ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is a symbolic link. `open` resolves symlinks, so no descriptor should name one (this is a bug in the caller)."
        | Some {
                   Content = InodeContent.CharacterDevice _
               } ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is a character device, which holds no contents of its own; what a transfer does is its driver's business, not this filesystem's (this is a bug in the caller)."
        | Some ({
                    Content = InodeContent.RegularFile (contents, permissions)
                } as entry) ->

        match truncatedContents contents length with
        | Error refusal -> Error refusal
        | Ok updated ->

        // A truncation is a mode change as well as a content change on one of the
        // two platforms, and the `ctime` below covers both either way.
        match PermissionBits.afterTruncation rule (Standing.toward credentials entry.Owner) permissions with
        | Error refusal -> Error (FileTruncationRefusal.UnmeasuredSetIdChange refusal)
        | Ok permissions ->

        Ok
            { vfs with
                Inodes =
                    Map.add
                        inode
                        { entry with
                            Content = InodeContent.RegularFile (updated, permissions)
                            Times = InodeTimes.contentsChangedAt now entry.Times
                        }
                        vfs.Inodes
            }

    /// Give the regular file or directory at `inode` the permission bits `bits`,
    /// and move its `ctime`.
    ///
    /// `ctime` moves even when `bits` are the bits the inode already had, and
    /// no other timestamp moves.
    ///
    /// Partial in the inode, which must name a regular file, a device or a
    /// directory this filesystem contains: no syscall this library models
    /// changes a symbolic link's own bits.
    let internal setPermissions
        (inode : InodeNumber)
        (bits : PermissionBits)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        // Measured on both platforms (`chmod-rules.c`): `chmod` and `fchmod` to
        // a different mode, to the same mode, and to one the kernel then
        // narrows, each move `ctime` and neither `atime` nor `mtime`, on a
        // regular file and on a directory, for the owner and for root.
        let bits = PermissionBits.assertValid "VirtualFileSystem.setPermissions" bits

        match Map.tryFind inode vfs.Inodes with
        | None ->
            failwith
                $"VirtualFileSystem.setPermissions: inode %O{inode} is not in this filesystem. The caller resolved a path or a descriptor to it, and a descriptor outliving its inode means an unlink removed a still-open file (this is a bug in the caller)."
        | Some entry ->

        let content =
            match entry.Content with
            | InodeContent.RegularFile (contents, _) -> InodeContent.RegularFile (contents, bits)
            | InodeContent.CharacterDevice (device, _) -> InodeContent.CharacterDevice (device, bits)
            | InodeContent.Directory directory ->
                InodeContent.Directory
                    { directory with
                        Permissions = bits
                    }
            | InodeContent.Symlink _ ->
                failwith
                    $"VirtualFileSystem.setPermissions: inode %O{inode} is a symbolic link. `chmod` follows a final symlink, no descriptor names one, and Darwin's `lchmod` is not modelled, so the caller should never have reached a link (this is a bug in the caller)."

        { vfs with
            Inodes =
                Map.add
                    inode
                    { entry with
                        Content = content
                        Times = InodeTimes.statusChangedAt now entry.Times
                    }
                    vfs.Inodes
        }

    /// Give the inode at `inode`, of any kind, the owner `owner`, and move its
    /// `ctime`.
    ///
    /// `ctime` moves even when `owner` is the owner the inode already had, and
    /// no other timestamp moves.
    ///
    /// Partial in the inode, which must be one this filesystem contains.
    let internal setOwner
        (inode : InodeNumber)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        // Measured on both platforms (`chown-rules.c`): a successful `chown`,
        // `lchown` or `fchown` moves `ctime` and neither `atime` nor `mtime`,
        // on a regular file, a directory and a symbolic link, whether or not
        // the owner or the group changed.
        match Map.tryFind inode vfs.Inodes with
        | None ->
            failwith
                $"VirtualFileSystem.setOwner: inode %O{inode} is not in this filesystem. The caller resolved a path or a descriptor to it, and a descriptor outliving its inode means an unlink removed a still-open file (this is a bug in the caller)."
        | Some entry ->

        { vfs with
            Inodes =
                Map.add
                    inode
                    { entry with
                        Owner = owner
                        Times = InodeTimes.statusChangedAt now entry.Times
                    }
                    vfs.Inodes
        }

    /// Give the inode at `inode` exactly the timestamps `times`, birth time
    /// included. The caller decides which of them a change moves.
    ///
    /// Partial in the inode, which must be one this filesystem contains.
    let internal setTimes (inode : InodeNumber) (times : InodeTimes) (vfs : VirtualFileSystem) : VirtualFileSystem =
        match Map.tryFind inode vfs.Inodes with
        | None ->
            failwith
                $"VirtualFileSystem.setTimes: inode %O{inode} is not in this filesystem. The caller resolved a path or a descriptor to it, and a descriptor outliving its inode means an unlink removed a still-open file (this is a bug in the caller)."
        | Some entry ->

        { vfs with
            Inodes =
                Map.add
                    inode
                    { entry with
                        Times = times
                    }
                    vfs.Inodes
        }

    // ------------------------------------------------------------ resolution

    /// Every (directory, name, target) binding in the graph, including those in
    /// directories nothing can reach.
    let private allBindings (vfs : VirtualFileSystem) : (InodeNumber * DirectoryEntryName * InodeNumber) list =
        vfs.Inodes
        |> Map.toList
        |> List.collect (fun (inode, entry) ->
            match entry.Content with
            | InodeContent.Directory directory ->
                directory.Entries
                |> Map.toList
                |> List.map (fun (name, target) -> inode, name, target)
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> []
        )

    /// The names each directory in `inodes` binds, holding only directories
    /// that bind at least one, which is the form
    /// `VirtualFileSystem.SortedNames` is kept in.
    let private boundNames (inodes : Map<InodeNumber, Inode>) : Map<InodeNumber, DirectoryEntryName list> =
        inodes
        |> Map.toSeq
        |> Seq.choose (fun (inode, entry) ->
            match entry.Content with
            | InodeContent.Directory directory when not (Map.isEmpty directory.Entries) ->
                Some (inode, directory.Entries |> Map.keys |> List.ofSeq)
            | InodeContent.Directory _
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> None
        )
        |> Map.ofSeq

    /// How many of `bindings` name each target, holding only non-zero counts,
    /// which is the form `VirtualFileSystem.BindingCounts` is kept in.
    let private countBindings
        (bindings : (InodeNumber * DirectoryEntryName * InodeNumber) list)
        : Map<InodeNumber, int>
        =
        bindings
        |> List.fold (fun counts (_, _, target) -> adjustBindingCount target 1 counts) Map.empty

    /// How many of `bindings` in each directory name a directory in `inodes`,
    /// holding only non-zero counts, which is the form
    /// `VirtualFileSystem.SubdirectoryCounts` is kept in.
    let private countSubdirectories
        (inodes : Map<InodeNumber, Inode>)
        (bindings : (InodeNumber * DirectoryEntryName * InodeNumber) list)
        : Map<InodeNumber, int>
        =
        bindings
        |> List.fold
            (fun counts (directory, _, target) ->
                if isDirectoryIn inodes target then
                    adjustSubdirectoryCount directory 1 counts
                else
                    counts
            )
            Map.empty

    /// The absolute path of a directory, by walking `Parent` links to the root.
    ///
    /// Directories only: a regular file may be hard-linked under several names,
    /// so it has no single path. `None` if `inode` is absent, is not a
    /// directory, or sits in a graph whose parent links do not reach the root —
    /// including one whose parent links cycle, which the visited set bounds so
    /// that this stays total on a defective graph (it is used as a test oracle,
    /// and defective graphs are exactly what those tests construct).
    let pathOfDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : AbsoluteUnixPath option =
        let rec climb (current : InodeNumber) (acc : DirectoryEntryName list) (visited : Set<InodeNumber>) =
            if current = vfs.Root then
                Some acc
            elif Set.contains current visited then
                None
            else

            match tryGetDirectory current vfs with
            | None -> None
            | Some content ->

            // The name is not stored on the inode, so recover it from the
            // parent's own entries. A well-formed graph has exactly one.
            //
            // `Map.tryPick` walks the tree; a search over `Map.toList` copies it
            // first. That matters because this runs on every `getcwd`, which a
            // client may call for every relative path it resolves, and the copy
            // made the cost scale with how many *siblings* the directory has,
            // not with the depth of its path. Measured over one directory of 100,000 entries:
            // `Map.toList` 2.3 ms and 6.4 MB per call, `Map.tryPick` 0.39 ms and
            // 24 bytes. (`Map.tryFindKey`, which reads like the natural answer,
            // measures *worse* than `Map.toList` on both counts.)
            match
                tryGetDirectory content.Parent vfs
                |> Option.bind (fun parent ->
                    parent.Entries
                    |> Map.tryPick (fun name target -> if target = current then Some name else None)
                )
            with
            | None -> None
            | Some name -> climb content.Parent (name :: acc) (Set.add current visited)

        match tryGetDirectory inode vfs with
        | None -> None
        | Some _ ->

        climb inode [] Set.empty
        |> Option.map (fun names ->
            match names with
            | [] -> AbsoluteUnixPath.root
            | names ->

            // Bytes throughout: this is `getcwd`'s answer, not a diagnostic, so a
            // name that is not valid UTF-8 must come back exactly as it was bound.
            let builder = ImmutableArray.CreateBuilder<byte> ()

            for name in names do
                builder.Add UnixPathText.separatorByte
                builder.AddRange (UnixByteString.toBytes (DirectoryEntryName.toByteString name))

            let rendered =
                match UnixByteString.ofBytes (builder.ToImmutable ()) with
                | Ok bytes -> bytes
                | Error defect ->
                    failwith
                        $"VirtualFileSystem.pathOfDirectory: joining NUL-free names produced bytes that %s{UnixByteString.describe defect} (this is a bug in this library)."

            match AbsoluteUnixPath.ofByteString rendered with
            | Ok path -> path
            | Error error ->
                failwith
                    $"VirtualFileSystem.pathOfDirectory: the directory's names joined into %s{UnixByteString.toEscaped rendered}, which %s{AbsoluteUnixPath.describe error} (this is a bug in this library)."
        )

    /// Every way in which `vfs` fails to describe a filesystem a kernel could
    /// produce, or the empty list if it is sound. Deterministic in order, so a
    /// failing test reports the same thing every run.
    ///
    /// `pinned` names the inodes some process holds open. Deletion makes an
    /// inode with no remaining name legitimate — a real kernel keeps one alive
    /// for as long as a descriptor refers to it — but *only* while something
    /// holds it, and whether anything does is a fact about the open file
    /// descriptions rather than about this graph. So the caller that can see both supplies
    /// it, and every unreachable inode outside the set is still a defect. Pass
    /// `Set.empty` for a graph no process has opened anything in.
    ///
    /// A pinned inode that is perfectly reachable is not an error: the
    /// overwhelmingly common case is a descriptor on a file that still has its
    /// name. The set excuses unreachability; it does not assert it.
    ///
    /// Nothing here checks that a pinned inode is *in* the graph. That is the
    /// mirror-image defect — a descriptor naming an inode the filesystem has
    /// forgotten — and it belongs to the layer holding the open file descriptions:
    /// `UnixSystemDefect.DanglingOpenInode`.
    ///
    /// Together, the link-count and reachability rules make tree-ness a
    /// theorem rather than a further check: the root has no incoming entry
    /// link and every other directory has exactly one, so any cycle among
    /// reachable directories would force some directory to have two, and any
    /// cycle that avoids that is unreachable from the root and flagged as such.
    let checkInvariants (pinned : Set<InodeNumber>) (vfs : VirtualFileSystem) : VirtualFileSystemDefect list =
        let bindings = allBindings vfs

        let rootDefects =
            match tryGetContent vfs.Root vfs with
            | None -> [ VirtualFileSystemDefect.RootMissing vfs.Root ]
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.CharacterDevice _)
            | Some (InodeContent.Symlink _) -> [ VirtualFileSystemDefect.RootIsNotDirectory vfs.Root ]
            | Some (InodeContent.Directory content) ->
                if content.Parent = vfs.Root then
                    []
                else
                    [ VirtualFileSystemDefect.RootParentIsNotSelf (vfs.Root, content.Parent) ]

        let danglingEntries =
            bindings
            |> List.filter (fun (_, _, target) -> not (Map.containsKey target vfs.Inodes))
            |> List.map VirtualFileSystemDefect.DanglingEntry

        /// Which directories hold each inode, so that the link-count rules and
        /// "the recorded parent is the real one" can all be decided.
        let holders : Map<InodeNumber, (InodeNumber * DirectoryEntryName) list> =
            bindings
            |> List.fold
                (fun acc (directory, name, target) ->
                    let existing = Map.tryFind target acc |> Option.defaultValue []
                    Map.add target ((directory, name) :: existing) acc
                )
                Map.empty
            |> Map.map (fun _ holders -> List.rev holders)

        let rootLinks =
            match Map.tryFind vfs.Root holders with
            | None -> []
            | Some parents -> [ VirtualFileSystemDefect.RootHasIncomingLink parents ]

        let parentDefects =
            vfs.Inodes
            |> Map.toList
            |> List.collect (fun (inode, entry) ->
                match entry.Content with
                | InodeContent.RegularFile _
                | InodeContent.CharacterDevice _
                | InodeContent.Symlink _ -> []
                | InodeContent.Directory directory ->

                // The root's parent is checked above, where "is itself" is the
                // rule rather than "is whoever holds it".
                if inode = vfs.Root then
                    []
                else

                let recorded = directory.Parent

                let structural =
                    match tryGetContent recorded vfs with
                    | None -> [ VirtualFileSystemDefect.DanglingParent (inode, recorded) ]
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.CharacterDevice _)
                    | Some (InodeContent.Symlink _) ->
                        [ VirtualFileSystemDefect.ParentIsNotDirectory (inode, recorded) ]
                    | Some (InodeContent.Directory _) -> []

                match Map.tryFind inode holders with
                | None ->
                    // Held by nothing: reported as unreachable below, and there
                    // is no actual parent to disagree with.
                    structural
                | Some [ (actual, _) ] ->
                    if actual = recorded then
                        structural
                    else
                        structural
                        @ [ VirtualFileSystemDefect.ParentMismatch (inode, recorded, actual) ]
                | Some holders ->
                    structural
                    @ [ VirtualFileSystemDefect.DirectoryMultiplyLinked (inode, holders) ]
            )

        let reachable =
            // Breadth-first from the root through directory entries only.
            // Parent links deliberately do not count: a directory reachable
            // only by climbing out of an orphaned subtree is still orphaned.
            let rec explore (frontier : InodeNumber list) (seen : Set<InodeNumber>) : Set<InodeNumber> =
                match frontier with
                | [] -> seen
                | inode :: rest ->
                    if Set.contains inode seen then
                        explore rest seen
                    else

                    let children =
                        match tryGetContent inode vfs with
                        | Some (InodeContent.Directory directory) -> directory.Entries |> Map.toList |> List.map snd
                        | Some _
                        | None -> []

                    explore (children @ rest) (Set.add inode seen)

            if Map.containsKey vfs.Root vfs.Inodes then
                explore [ vfs.Root ] Set.empty
            else
                Set.empty

        let unreachable =
            vfs.Inodes
            |> Map.toList
            |> List.map fst
            |> List.filter (fun inode -> not (Set.contains inode reachable))
            |> List.filter (fun inode -> not (Set.contains inode pinned))
            |> List.map VirtualFileSystemDefect.UnreachableFromRoot

        let freshness =
            vfs.Inodes
            |> Map.toList
            |> List.map fst
            |> List.filter (fun inode -> inode >= vfs.NextInode)
            |> List.map (fun inode -> VirtualFileSystemDefect.NextInodeNotFresh (vfs.NextInode, inode))

        let bindingCounts =
            let counted = countBindings bindings

            Set.union (Map.keys counted |> Set.ofSeq) (Map.keys vfs.BindingCounts |> Set.ofSeq)
            |> Set.toList
            |> List.choose (fun inode ->
                let stored = Map.tryFind inode vfs.BindingCounts
                let counted = Map.tryFind inode counted |> Option.defaultValue 0

                if stored = (if counted = 0 then None else Some counted) then
                    None
                else
                    Some (VirtualFileSystemDefect.BindingCountMismatch (inode, stored, counted))
            )

        let sortedNames =
            let bound = boundNames vfs.Inodes

            Set.union (Map.keys bound |> Set.ofSeq) (Map.keys vfs.SortedNames |> Set.ofSeq)
            |> Set.toList
            |> List.choose (fun inode ->
                let stored = Map.tryFind inode vfs.SortedNames |> Option.map SortedEntryNames.toList
                let bound = Map.tryFind inode bound

                if stored = bound then
                    None
                else
                    Some (VirtualFileSystemDefect.SortedNamesMismatch (inode, stored, bound |> Option.defaultValue []))
            )

        let subdirectoryCounts =
            let counted = countSubdirectories vfs.Inodes bindings

            Set.union (Map.keys counted |> Set.ofSeq) (Map.keys vfs.SubdirectoryCounts |> Set.ofSeq)
            |> Set.toList
            |> List.choose (fun directory ->
                let stored = Map.tryFind directory vfs.SubdirectoryCounts
                let counted = Map.tryFind directory counted |> Option.defaultValue 0

                if stored = (if counted = 0 then None else Some counted) then
                    None
                else
                    Some (VirtualFileSystemDefect.SubdirectoryCountMismatch (directory, stored, counted))
            )

        // A mounted filesystem holds its root and what that root binds, and
        // nothing deeper: every mount this module makes is flat.
        let mountDefects =
            let recordedByRoot =
                vfs.MountMembers
                |> Map.toList
                |> List.groupBy snd
                |> List.map (fun (root, members) -> root, members |> List.map fst |> Set.ofList)
                |> Map.ofList

            Set.union (Map.keys vfs.Mounts |> Set.ofSeq) (Map.keys recordedByRoot |> Set.ofSeq)
            |> Set.toList
            |> List.collect (fun root ->
                let mount = Map.tryFind root vfs.Mounts

                let rootDefect, actual =
                    match mount, tryGetDirectory root vfs with
                    | None, _ -> [], Set.empty
                    | Some _, None -> [ VirtualFileSystemDefect.MountRootNotDirectory root ], Set.empty
                    | Some _, Some content -> [], content.Entries |> Map.values |> Set.ofSeq |> Set.add root

                let recorded = Map.tryFind root recordedByRoot |> Option.defaultValue Set.empty

                let membership =
                    if recorded = actual then
                        []
                    else
                        [ VirtualFileSystemDefect.MountMembershipMismatch (root, recorded, actual) ]

                let covered =
                    match mount with
                    | Some mount when Map.containsKey mount.Covered vfs.Inodes ->
                        [ VirtualFileSystemDefect.CoveredInodeInUse (root, mount.Covered) ]
                    | Some mount when mount.Covered >= vfs.NextInode ->
                        [ VirtualFileSystemDefect.NextInodeNotFresh (vfs.NextInode, mount.Covered) ]
                    | Some _
                    | None -> []

                rootDefect @ membership @ covered
            )

        rootDefects
        @ rootLinks
        @ danglingEntries
        @ parentDefects
        @ unreachable
        @ freshness
        @ bindingCounts
        @ sortedNames
        @ subdirectoryCounts
        @ mountDefects

    /// Fail loudly if `vfs` is not sound, naming `context`. For the operations
    /// that build a filesystem from host configuration, where a defect is a
    /// host bug rather than anything a process could have caused.
    let internal assertInvariants (context : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        // Nothing pinned: these callers build a filesystem out of host
        // configuration, before any process exists to have opened anything, so an
        // inode no path reaches is a bug in the builder every time.
        match checkInvariants Set.empty vfs with
        | [] -> vfs
        | defects ->
            let rendered = defects |> List.map (sprintf "%A") |> String.concat "; "
            failwith $"%s{context}: the inode graph is not a filesystem any kernel could produce: %s{rendered}"

    /// Realise a seed as an inode graph whose root directory holds `entries`.
    ///
    /// The root is a `Map` rather than a `SeedEntry` because a filesystem's
    /// root is always a directory: taking the entries directly makes "the root
    /// is a regular file" unrepresentable instead of an error to report.
    ///
    /// `createdAt` is every seeded inode's birth, mtime, ctime and atime — the
    /// filesystem springs into existence at one instant. Passed in rather than
    /// read from a clock: a filesystem that read the host's clock would make a
    /// replay depend on when it was recorded.
    ///
    /// `defaultOwner` owns the root directory and every entry that does not
    /// state an owner of its own. It is not inherited from an entry's parent:
    /// an entry without an owner belongs to `defaultOwner` wherever it is.
    ///
    /// `symlinkPermissions` are every seeded symbolic link's bits, which a seed
    /// does not state: see `SeedEntry.Symlink`.
    let internal ofFileSystemSeed
        (createdAt : UnixTimestamp)
        (defaultOwner : InodeOwner)
        (symlinkPermissions : PermissionBits)
        (entries : Map<DirectoryEntryName, SeedEntry>)
        : VirtualFileSystem
        =
        let rec install
            (directory : InodeNumber)
            (entries : Map<DirectoryEntryName, SeedEntry>)
            (vfs : VirtualFileSystem)
            : VirtualFileSystem
            =
            // `Map` iterates in key order, so the inode numbers a seed produces
            // are a function of the seed alone rather than of how the host
            // happened to build the map. Inode numbers are visible to a process
            // through `st_ino`, so this is part of the replay contract.
            entries
            |> Map.fold
                (fun vfs name entry ->
                    match entry with
                    | SeedEntry.File (contents, permissions, owner) ->
                        let owner = owner |> Option.defaultValue defaultOwner

                        match createFile directory name permissions owner createdAt contents vfs with
                        | Ok (_, vfs) -> vfs
                        | Error error ->
                            failwith
                                $"ofFileSystemSeed: could not create the file %s{DirectoryEntryName.toEscaped name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                    | SeedEntry.Symlink (target, owner) ->
                        let owner = owner |> Option.defaultValue defaultOwner

                        match createSymlink directory name symlinkPermissions owner createdAt target vfs with
                        | Ok (_, vfs) -> vfs
                        | Error error ->
                            failwith
                                $"ofFileSystemSeed: could not create the symlink %s{DirectoryEntryName.toEscaped name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                    | SeedEntry.Directory (children, permissions, owner) ->
                        let owner = owner |> Option.defaultValue defaultOwner

                        match createDirectory directory name permissions owner createdAt vfs with
                        | Ok (inode, vfs) -> install inode children vfs
                        | Error error ->
                            failwith
                                $"ofFileSystemSeed: could not create the directory %s{DirectoryEntryName.toEscaped name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                )
                vfs

        let vfs = empty createdAt defaultOwner

        install (root vfs) entries vfs
        |> assertInvariants "VirtualFileSystem.ofFileSystemSeed"

    /// Construction that bypasses every invariant this module maintains.
    ///
    /// Exists so that `checkInvariants` can be tested: a defect no test can
    /// construct is documentation rather than a check. Deliberately one
    /// greppable token, so that any non-test code reaching for it is visible
    /// in review — nothing outside tests should.
    [<RequireQualifiedAccess>]
    module internal Unchecked =
        /// The filesystem with exactly these parts. The binding counts, the
        /// sorted names and the subdirectory counts are computed from the
        /// entries, so a graph forged to exhibit some other defect does not
        /// also exhibit `BindingCountMismatch`, `SortedNamesMismatch` or
        /// `SubdirectoryCountMismatch`.
        let ofParts
            (inodes : Map<InodeNumber, Inode>)
            (root : InodeNumber)
            (nextInode : InodeNumber)
            : VirtualFileSystem
            =
            let vfs =
                {
                    Inodes = inodes
                    Root = root
                    NextInode = nextInode
                    BindingCounts = Map.empty
                    SortedNames = boundNames inodes |> Map.map (fun _ names -> SortedEntryNames.ofSeq names)
                    SubdirectoryCounts = Map.empty
                    Mounts = Map.empty
                    MountMembers = Map.empty
                }

            let bindings = allBindings vfs

            { vfs with
                BindingCounts = countBindings bindings
                SubdirectoryCounts = countSubdirectories inodes bindings
            }

        /// `vfs` with the count `bindingCount` answers for `inode` replaced by
        /// `count` verbatim, `None` storing nothing, and the graph untouched.
        let setBindingCount (inode : InodeNumber) (count : int option) (vfs : VirtualFileSystem) : VirtualFileSystem =
            { vfs with
                BindingCounts =
                    match count with
                    | None -> Map.remove inode vfs.BindingCounts
                    | Some count -> Map.add inode count vfs.BindingCounts
            }

        /// `vfs` with the count `subdirectoryCount` answers for `directory`
        /// replaced by `count` verbatim, `None` storing nothing, and the graph
        /// untouched.
        let setSubdirectoryCount
            (directory : InodeNumber)
            (count : int option)
            (vfs : VirtualFileSystem)
            : VirtualFileSystem
            =
            { vfs with
                SubdirectoryCounts =
                    match count with
                    | None -> Map.remove directory vfs.SubdirectoryCounts
                    | Some count -> Map.add directory count vfs.SubdirectoryCounts
            }

        /// `vfs` with the mount whose root is `root` replaced by `mount`, `None`
        /// recording no mount there, and the graph untouched.
        let setMount (root : InodeNumber) (mount : Mount option) (vfs : VirtualFileSystem) : VirtualFileSystem =
            { vfs with
                Mounts =
                    match mount with
                    | None -> Map.remove root vfs.Mounts
                    | Some mount -> Map.add root mount vfs.Mounts
            }

        /// `vfs` with the mounted filesystem `inode` is recorded as on replaced
        /// by the one whose root is `root`, `None` recording it as on the root
        /// filesystem, and the graph untouched.
        let setMountMember
            (inode : InodeNumber)
            (root : InodeNumber option)
            (vfs : VirtualFileSystem)
            : VirtualFileSystem
            =
            { vfs with
                MountMembers =
                    match root with
                    | None -> Map.remove inode vfs.MountMembers
                    | Some root -> Map.add inode root vfs.MountMembers
            }

        /// `vfs` with the names `nextDirectoryEntry` seeks in for `directory`
        /// replaced by `names`, `None` storing nothing, and the graph
        /// untouched. `Some []` stores an empty set.
        let setSortedNames
            (directory : InodeNumber)
            (names : DirectoryEntryName list option)
            (vfs : VirtualFileSystem)
            : VirtualFileSystem
            =
            { vfs with
                SortedNames =
                    match names with
                    | None -> Map.remove directory vfs.SortedNames
                    | Some names -> Map.add directory (SortedEntryNames.ofSeq names) vfs.SortedNames
            }
