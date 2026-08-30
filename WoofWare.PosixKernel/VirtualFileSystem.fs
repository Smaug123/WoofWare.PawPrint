namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// A whole emulated filesystem: an inode graph rooted at a single directory.
type VirtualFileSystem =
    private
        {
            Inodes : Map<InodeNumber, Inode>
            /// The directory absolute paths resolve from. Its `Parent` is
            /// itself.
            Root : InodeNumber
            /// The next inode number to hand out. Numbers are never reused,
            /// even after the last link to a file is removed: reuse is
            /// observable to a guest that cached an `(st_dev, st_ino)` pair,
            /// and a fresh number can only ever make a stale comparison report
            /// "different file", which is the safe direction to be wrong in.
            NextInode : InodeNumber
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

/// Whether a resolution follows a symlink found in the *final* position.
/// Symlinks in every earlier position are always followed; a path cannot
/// continue through one otherwise.
///
/// A trailing separator overrides this — see `Resolution`.
[<RequireQualifiedAccess>]
type SymlinkPolicy =
    /// Follow it: what `stat`, and `open` without `O_NOFOLLOW`, do.
    | Follow
    /// Stop at the link itself: what `lstat`, `readlink`, `unlink`, `rename`
    /// and `open` with `O_NOFOLLOW` do.
    | NoFollowFinal

/// What a trailing separator on the *final* component means to this walk.
///
/// A second axis from `SymlinkPolicy`, and independent of it: this one is not
/// about what the walk follows, but about whether "the final component must name
/// a directory" is a demand to record or a refusal to make.
///
/// The two Unixes split here for a *creating* open, and the split is not a
/// choice PawPrint makes — it is measured. Linux refuses such a path outright,
/// XNU resolves it as any other lookup would.
[<RequireQualifiedAccess>]
type TrailingSeparatorPolicy =
    /// Record the demand on `Resolution.TrailingSeparatorDemanded` and let the
    /// caller enforce it. Every lookup (`stat`, `lstat`, `readlink`), every
    /// non-creating `open`, and -- on Darwin -- a creating `open` and `mkdir`.
    | Demand
    /// Answer EISDIR on *reaching* a final component that carries a trailing
    /// separator — before that component's length is checked, before it is
    /// looked up, and before any symlink it names is traversed. What Linux does
    /// for a creating open.
    ///
    /// Measured on Linux, and it is the *position* of the check that these rows
    /// pin rather than the errno: `<300 a>/` is EISDIR, not ENAMETOOLONG;
    /// `cyc/` is EISDIR, not ELOOP; `d/` under `O_EXCL` is EISDIR, not EEXIST;
    /// `dang/` is EISDIR, not ENOENT. It does *not* pre-empt failures on earlier
    /// components: `nodir/new/` is ENOENT and `f/new/` is ENOTDIR, because the
    /// walk never reaches the final component in either.
    ///
    /// The separator may equally have arrived from a spliced symlink target
    /// rather than from the guest's own path: `l -> "cyc2/"` opened with
    /// `O_CREAT` is EISDIR rather than ELOOP, so the check has to sit inside the
    /// walk rather than at the syscall boundary.
    | RefuseIsDirectory
    /// Record the demand on `Resolution.TrailingSeparatorDemanded` and impose
    /// *nothing*: the final component is neither dereferenced because of the
    /// separator nor required to be a directory. What Linux's `mkdir` does,
    /// whose last component is resolved by `filename_create` -- a plain dentry
    /// lookup that never follows a link and never inspects what it found.
    ///
    /// Measured, and it is two suppressions rather than one. Under Linux
    /// `mkdir`, "lf/", "ld/", "dang/" and "cyc/" are all EEXIST -- no traversal,
    /// so no ELOOP and no chance to create a dangling link's target -- *and*
    /// "f/" is EEXIST rather than the ENOTDIR every lookup owes it. Darwin's
    /// `mkdir` wants `Demand` instead, and answers ENOTDIR, ELOOP, and (for
    /// "dang/") creates the link's target.
    ///
    /// The two suppressions co-occur here because Linux's creating lookup does
    /// neither, and they are *not* the same fact. Linux's deletion wants
    /// no-follow *with* the directory demand — `unlink("ld/")` and `unlink("f/")`
    /// are both ENOTDIR there — which looks like a second axis, and is not one:
    /// `do_unlinkat` takes a parent and a name and then inspects the byte after
    /// the name, so the demand is enforced *after* the walk, by the verdict,
    /// out of `Resolution.TrailingSeparatorDemanded`. Measured, and the row
    /// that proves it is `unlink("lroot/")` with `lroot -> "/"`: ENOTDIR on
    /// Linux, so the link was never traversed, where Darwin's `Demand` walk
    /// traverses it and answers EISDIR.
    ///
    /// So this stays one-dimensional, and `unlink` selects it here on Linux
    /// while enforcing the demand itself. See `UnlinkRules.verdict`.
    | Ignore

/// Which component a resolution last consumed, for the paths that end without
/// a name to look up.
///
/// Carried on `ResolvedTarget.Directory` rather than left to the caller to read
/// off its own path, because a symlink expansion replaces the final component:
/// with `l1 -> "."` and `l2 -> "d/.."`, the paths "l1/" and "l2/" are the same
/// shape but land on different navigation. Probed on macOS, `rmdir` owes them
/// different errnos — EINVAL and ENOTEMPTY respectively — so the distinction is
/// guest-observable and unrecoverable from the original path.
[<RequireQualifiedAccess>]
type FinalNavigation =
    /// The path named no component at all: "/" itself, or a symlink whose
    /// target was "/". `rmdir` owes this EBUSY on Linux and EISDIR on Darwin.
    | Root
    /// The last component consumed was ".". `rmdir` owes this EINVAL — except
    /// on Darwin at the root itself, which is EBUSY.
    | Current
    /// The last component consumed was "..". `rmdir` owes this ENOTEMPTY —
    /// except on Darwin at the root itself, which is EBUSY.
    | Parent

/// Where a path resolution ended up.
[<RequireQualifiedAccess>]
type ResolvedTarget =
    /// The path named `Name` inside `Directory`. `Existing` is the inode bound
    /// to that name, or `None` when the name is free — which is *not* an error
    /// here, because it is exactly the state `open(O_CREAT)`, `mkdir` and
    /// `symlink` need, and callers that require the file to exist report
    /// ENOENT themselves.
    | Entry of directory : InodeNumber * name : DirectoryEntryName * existing : InodeNumber option
    /// The path resolved straight to a directory with no final name to look
    /// up, because its last component — after any symlink expansion — was "/",
    /// "." or "..".
    ///
    /// `ReachedBy` says which, because the errno that follows depends on it and
    /// the caller cannot recover it from the path it passed in: see
    /// `FinalNavigation`. (ENOTEMPTY is itself platform-dependent: Linux 39,
    /// Darwin 66.)
    | Directory of inode : InodeNumber * reachedBy : FinalNavigation

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
type RenameOutcome =
    {
        /// The inode the destination name was bound to before the rename took
        /// it, or `None` when that name was free.
        ///
        /// It may still have other names, and something may still hold it open;
        /// `VirtualFileSystem.rename` frees nothing, for the reason
        /// `VirtualFileSystem.unbind` frees nothing.
        Displaced : InodeNumber option
    }

/// The outcome of a resolution, together with the facts about *how* it
/// finished that a caller cannot recover from the path it passed in.
type Resolution =
    {
        Target : ResolvedTarget
        /// The path, after any final symlink was spliced in, ended with a
        /// separator — so it demanded that its final component be a directory.
        ///
        /// Not simply the caller's `UnixPath.hasTrailingSeparator`: following a
        /// final symlink replaces the final path segment, so a link whose
        /// target is "d/" imposes the demand even when the guest's own path did
        /// not.
        ///
        /// What the demand *costs* is the walk's business under
        /// `TrailingSeparatorPolicy.Demand` (an existing non-directory final
        /// gives ENOTDIR) and under `RefuseIsDirectory` (EISDIR on reaching the
        /// component). Under `Ignore` it costs nothing and this flag is a report
        /// about the path rather than about anything enforced. The part that is
        /// left to the caller either way — see `FinalSymlinkFollowed`.
        TrailingSeparatorDemanded : bool
        /// A symlink in the *final* position was followed to get here.
        ///
        /// In combination with `TrailingSeparatorDemanded`, this pair is where
        /// Linux and macOS disagree destructively. Probed on macOS: with `ld -> realdir`,
        /// `rmdir("ld/")` *removes realdir*; with `dang -> nx`, `mkdir("dang/")`
        /// *creates nx*. Linux refuses both (ENOTDIR, EEXIST). A mutating
        /// operation that sees both flags set must therefore never pick a
        /// platform *unconditionally*, since the two choices destroy different
        /// objects: it either dispatches on the flavour, or fails loudly.
        ///
        /// Dispatching is only honest once both columns are measured at that
        /// operation's own scale, which is a per-syscall question rather than a
        /// property of this flag. `mkdir` is measured on both and dispatches
        /// (`SimulatedUnixPlatform.mkDirRules` picks the walk that reproduces
        /// each), and it never sees this flag set, because Linux's walk is
        /// `TrailingSeparatorPolicy.Ignore` and Darwin's acts on the following
        /// rather than reporting it. `unlink` is measured on both and dispatches
        /// the same way (`SimulatedUnixPlatform.unlinkRules`); it destroys
        /// nothing when the pair is set, since Darwin answers EPERM for the
        /// directory a followed link named and Linux never follows.
        ///
        /// `rmdir` is where the pair finally costs something. Measured on both
        /// and dispatched by `SimulatedUnixPlatform.rmDirRules`: with
        /// `ld -> d` and `d` empty, `rmdir("ld/")` removes `d` on Darwin and is
        /// ENOTDIR on Linux. Every mutating operation PawPrint models is now
        /// measured on both columns, so nothing here owes a loud failure any
        /// more — but a *new* one still does, and for the same reason.
        ///
        /// Lookup operations (`stat`, `lstat`) are unanimous and can ignore
        /// this.
        FinalSymlinkFollowed : bool
    }

/// A path resolution that has walked as far as the directory holding the final
/// name and stopped there, without looking that name up.
///
/// The point of pausing is that a syscall resolving *two* paths can interleave
/// them. Linux's `rename` walks both parents before it looks either final
/// component up, which is guest-visible: `rename("<300 bytes>", "nodir/x")` is
/// ENOENT because the destination's parent fails before the source's final name
/// is length-checked, while `rename("nope", "<300 bytes>")` is ENOENT the other
/// way round. One `resolveFull` cannot answer both, because it does the parent
/// walk and the final lookup in one indivisible call.
///
/// Opaque on purpose: the only thing to do with one is hand it to
/// `VirtualFileSystem.completeResolution`. There is no "the parent directory"
/// to read off it, because following a final symlink moves the directory the
/// name is looked up in, and that happens during the second half.
///
/// It carries the rules the walk began under and the filesystem it began
/// against, so a resumption cannot be given different ones.
[<NoEquality ; NoComparison>]
type PausedResolution =
    private
        {
            Limits : PathLimits
            Privilege : CallerPrivilege
            Policy : SymlinkPolicy
            TrailingSeparator : TrailingSeparatorPolicy
            FileSystem : VirtualFileSystem
            /// The directory the final name will be looked up in.
            Directory : InodeNumber
            /// The name still to look up, and the cursor positioned after it —
            /// which is what a splice of a final symlink is measured against.
            /// `None` when the path ran out of names before any could be looked
            /// up, which is the "/", "." and ".." case.
            Final : (DirectoryEntryName * PathCursor) option
            Trailing : bool
            FinalSymlinkFollowed : bool
            LastNavigation : FinalNavigation
            SymlinksTraversed : int
        }

/// Where `lseek(2)` measures its offset from.
///
/// Exactly the three `Interop.Sys.SeekWhence` values (`Interop.LSeek.cs`), which
/// are also the three POSIX ones — and *not* the platforms' full `<unistd.h>`
/// vocabulary, which continues with `SEEK_DATA` and `SEEK_HOLE`. Those two are
/// deliberately absent: they are numbered 3 and 4 on Linux and **4 and 3** on
/// Darwin, so a raw whence of 3 does not name the same operation on the two
/// kernels, and there is no portable case to add. `SystemNative_LSeek` decodes
/// the raw integer and refuses them; see the handler.
[<RequireQualifiedAccess>]
type SeekWhence =
    /// `SEEK_SET` (0): from the start of the file.
    | Set
    /// `SEEK_CUR` (1): from the description's current offset.
    | Current
    /// `SEEK_END` (2): from the end of the file.
    | End

/// Why a write to a regular file has no answer PawPrint can give.
///
/// Not a `UnixError`, and deliberately: this is a limit of the model rather than
/// anything a kernel does, so a caller must fail loudly rather than translate it
/// into an errno a guest could catch and interpret. Measured on ext4 and APFS
/// alike, `pwrite` of one byte at offset 2^40 succeeds and leaves a sparse 1 TB
/// file behind.
[<RequireQualifiedAccess>]
type FileWriteRefusal =
    /// The write would leave the file longer than `VirtualFileSystem.maxFileLength`.
    /// Carries the write rather than the resulting length, which need not be a
    /// number: `offset + count` can leave `int64` entirely.
    | WouldExceedMaxLength of offset : int64 * count : int

/// Why a truncation has no answer.
///
/// Separate from `FileWriteRefusal` rather than a case added to it: the payloads
/// differ (a truncation names one length, a write names an offset and a count),
/// and neither operation can produce the other's case, so sharing the type would
/// force every `match` to handle something unreachable.
///
/// Like `FileWriteRefusal`, this is a limit of the model rather than anything a
/// kernel does, so a caller fails loudly rather than translating it into an
/// errno. Measured on ext4 and APFS alike, `ftruncate(fd, 3e9)` succeeds and
/// leaves a sparse three-gigabyte file behind.
[<RequireQualifiedAccess>]
type FileTruncationRefusal =
    /// The requested length is more than `VirtualFileSystem.maxFileLength`.
    /// Carries the length as asked for, which need not fit in an `int`.
    | WouldExceedMaxLength of length : int64

/// Why a seek computation has no answer.
///
/// Split into two cases rather than one because the platforms disagree about
/// only one of them: a computation landing below zero is `EINVAL` on both,
/// while one that leaves `int64` is `EINVAL` on Linux and `EOVERFLOW` on Darwin.
/// Collapsing them here would push that distinction into the handler as a
/// second computation of the same arithmetic.
[<RequireQualifiedAccess>]
type SeekFault =
    /// The computed position is negative. Real kernels reject rather than
    /// clamp, so a file offset is never pinned to 0 by a wild seek.
    | Negative
    /// The computed position does not fit in a signed 64-bit offset. Only
    /// reachable through `SEEK_CUR` and `SEEK_END`, whose arithmetic adds two
    /// values the caller does not jointly control.
    | Overflow

/// A name a directory stream can hand back. Neither "." nor ".." is a
/// `FileName` — `FileNameError.Reserved` rejects both, because a directory
/// binds neither and PawPrint derives both from the graph — so a stream that
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

    /// The bytes `readdir(3)` would put in `d_name`.
    override this.ToString () : string =
        match this with
        | DirectoryStreamName.Dot -> "."
        | DirectoryStreamName.DotDot -> ".."
        | DirectoryStreamName.Entry name -> DirectoryEntryName.toString name

/// How far through a directory an open stream has read.
///
/// A *name*, not a position. Measured on both kernels at 5000 entries — well
/// past glibc's 32 KB `readdir` buffer — deleting each entry as it is returned
/// skips nothing and leaves the directory empty, so a real filesystem hands out
/// a stable per-entry cookie rather than an index into a shifting list. A
/// position would make `Directory.Delete(recursive: true)` fail: CoreLib's
/// `FileSystem.RemoveDirectoryRecursive` deletes each child inside the
/// `foreach` over the live enumerator and then `rmdir`s the parent, so an
/// enumeration that skipped anything would answer ENOTEMPTY.
///
/// Four cases rather than a `FileName option`, because "returned `..`, not yet
/// `.`" is a real position of the stream and neither dot is expressible as a
/// `FileName`.
///
/// What this does *not* claim is agreement with a real kernel about mutations:
/// whether an entry added after `opendir` becomes visible is unspecified, and
/// both kernels' answers are artefacts of when `getdents` happened to run. See
/// `docs/divergences.md`.
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

[<RequireQualifiedAccess>]
module VirtualFileSystem =
    /// Inode 1, matching the convention that no real filesystem hands out inode
    /// 0. A zero default would otherwise silently alias whichever inode was
    /// allocated first.
    let private firstInode : InodeNumber = InodeNumber 1L

    /// The `st_dev` every inode in this filesystem reports.
    ///
    /// One device for the whole tree, since this kernel models no mounts. The
    /// value itself is unobservable beyond comparison — a runtime reads
    /// `(st_dev, st_ino)` pairs to decide whether two paths name the same file,
    /// and never interprets the device number — but it is *non-zero*: no mounted
    /// filesystem reports 0, so a zero here would be indistinguishable from a
    /// field nobody remembered to write.
    let deviceId : int64 = 0x1000001L


    /// A filesystem containing nothing but an empty root directory, created at
    /// `now`.
    ///
    /// Takes the time rather than reading a clock: this file compiles before
    /// `EmulatedKernel.fs`, and a filesystem that read the host's clock would
    /// make a replay depend on when it was recorded.
    let empty (now : UnixTimestamp) : VirtualFileSystem =
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
                                        Permissions = PermissionBits.defaultForDirectory
                                    }
                            Times = InodeTimes.createdAt now
                        }
                    ]
            Root = firstInode
            NextInode = InodeNumber 2L
        }

    let root (vfs : VirtualFileSystem) : InodeNumber = vfs.Root

    let nextInode (vfs : VirtualFileSystem) : InodeNumber = vfs.NextInode

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
    let readTransferCount (offset : int64) (count : int) (length : int) : int =
        // The handler is responsible for rejecting a negative offset (EINVAL)
        // and refusing a negative size, so both are established before here.
        System.Diagnostics.Debug.Assert (offset >= 0L, "readTransferCount: offset must not be negative")
        System.Diagnostics.Debug.Assert (count >= 0, "readTransferCount: count must not be negative")
        System.Diagnostics.Debug.Assert (length >= 0, "readTransferCount: length must not be negative")

        if offset >= int64 length then
            // Includes an offset beyond `int` range, which no seeded file can
            // reach but a guest can certainly ask for.
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
    let writtenLength (offset : int64) (count : int) (length : int) : Result<int, FileWriteRefusal> =
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
    /// separated from the handlers that use it: as a function of a byte array, an
    /// offset and a byte array it is property-testable against naive splicing,
    /// where the same arithmetic inlined into a syscall handler is reachable only
    /// through a guest.
    let writtenContents
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

        // The handler is responsible for rejecting a negative offset (EINVAL), so
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

        Ok (ImmutableArray.CreateRange result)

    /// The length a regular file becomes when truncated to `length`, or the
    /// refusal if that is more than this model can hold.
    ///
    /// Separate from `truncatedContents` for the reason `writtenLength` is
    /// separate from `writtenContents`: it is the only way to check both sides of
    /// the ceiling without allocating two gigabytes to do it.
    ///
    /// A negative length is the handler's to reject (EINVAL), so it is
    /// established before here.
    let truncatedLength (length : int64) : Result<int, FileTruncationRefusal> =
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
    /// inlined into a syscall handler is reachable only through a guest.
    let truncatedContents
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
    /// property-testable, where the same arithmetic inlined in a handler is
    /// reachable only through a guest.
    ///
    /// **Not bounded above by `size`.** Seeking past the end of a file is legal
    /// — it is how sparse files are made — and a subsequent read there simply
    /// transfers nothing. The only rejections are the two `SeekFault` cases.
    ///
    /// **No filesystem ceiling either.** A real Linux rejects an offset above
    /// the filesystem's `s_maxbytes` with `EINVAL`: measured, ext4 stops at
    /// `0xffffffff000` while **tmpfs accepts the full `int64` range**, as does
    /// macOS's APFS. PawPrint's filesystem is in memory, so tmpfs is the honest
    /// analogue and the ceiling is `Int64.MaxValue`. The divergence is a
    /// *filesystem* difference, not a platform one, even though a dev box's
    /// APFS accepts what a CI container's ext4 refuses.
    /// **The size is deferred**: only `SEEK_END` consults it, and there are
    /// descriptors with no size PawPrint is willing to state — a directory's,
    /// which is a filesystem artefact rather than a fact (see the
    /// `SystemNative_LSeek` handler). Seeking such a descriptor with `SEEK_SET`
    /// or `SEEK_CUR` is portable and must keep working, so the caller passes a
    /// thunk that refuses, and only the `End` case forces it.
    let seekTarget
        (whence : SeekWhence)
        (current : int64)
        (size : Lazy<int64>)
        (offset : int64)
        : Result<int64, SeekFault>
        =
        // A property of the model rather than of the guest: a description's
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
    let private tryGetDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : DirectoryContent option =
        match tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Some directory
        | Some _
        | None -> None

    let private allocate
        (content : InodeContent)
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
                        }
                        vfs.Inodes
                NextInode = InodeNumber (raw + 1L)
            }

        inode, vfs

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
        match tryGetContent directory vfs with
        | None -> Error UnixError.ENOENT
        | Some (InodeContent.RegularFile _)
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
                }

            Ok
                { vfs with
                    Inodes = Map.add directory updated vfs.Inodes
                }

    /// Create an empty subdirectory. Mirrors `mkdir(2)`: EEXIST if the name is
    /// taken, ENOTDIR if `directory` is not a directory, ENOENT if it is absent.
    let createDirectory
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
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
                now
                vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a regular file with the given contents. Mirrors `open(2)` with
    /// `O_CREAT | O_EXCL`.
    let createFile
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
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
            allocate (InodeContent.RegularFile (contents, permissions)) now vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a symbolic link holding `target` verbatim. Mirrors `symlink(2)`,
    /// including that the target is not resolved, need not exist, and may be
    /// relative. An empty target is unrepresentable by construction; see
    /// `SymlinkTargetError.Empty`.
    /// There is no `permissions` parameter: see
    /// `InodePermissions.PlatformSymlinkDefault`.
    let createSymlink
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (now : UnixTimestamp)
        (target : SymlinkTarget)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        let target = SymlinkTarget.assertValid "VirtualFileSystem.createSymlink" target

        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated = allocate (InodeContent.Symlink target) now vfs
        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Bind an existing inode under a second name. Mirrors `link(2)`, including
    /// its refusal to hard-link a directory (EPERM): that would make the graph
    /// a non-tree, and a directory's `Parent` could then name only one of its
    /// containers.
    let hardLink
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (target : InodeNumber)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        match Map.tryFind target vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some {
                   Content = InodeContent.Directory _
               } -> Error UnixError.EPERM
        | Some ({
                    Content = InodeContent.RegularFile _
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
    /// fact about the descriptor table rather than about this graph. The caller
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
    let unbind
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
                   Content = InodeContent.Symlink _
               } -> Error UnixError.ENOTDIR
        | Some ({
                    Content = InodeContent.Directory content
                } as existing) ->

        match Map.tryFind name content.Entries with
        | None -> Error UnixError.ENOENT
        | Some target ->

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
                    $"VirtualFileSystem.unbind: directory inode %O{directory} bound \"%s{DirectoryEntryName.toString name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        Ok (
            target,
            { vfs with
                Inodes = inodes
            }
        )

    /// How many directory entries name `inode`.
    ///
    /// This is `st_nlink` as a *file* reports it. It is not what a directory
    /// reports, which also counts its own "." and each child's ".."; those are
    /// derived here rather than stored (see `DirectoryContent.Entries`), so
    /// counting them would mean re-deriving them, and no syscall PawPrint models
    /// reports the number anyway — `FileStatus` has no `nlink` field.
    ///
    /// Zero means the inode has no name: either it is the root, or its last link
    /// has gone and only a descriptor is keeping it alive.
    let bindingCount (inode : InodeNumber) (vfs : VirtualFileSystem) : int =
        vfs.Inodes
        |> Map.toSeq
        |> Seq.sumBy (fun (_, node) ->
            match node.Content with
            | InodeContent.Directory directory ->
                directory.Entries
                |> Map.toSeq
                |> Seq.filter (fun (_, target) -> target = inode)
                |> Seq.length
            | InodeContent.RegularFile _
            | InodeContent.Symlink _ -> 0
        )

    /// Whether `inode` is a directory that no path from the root can reach: its
    /// last name has gone, and only a descriptor or the current directory is
    /// keeping it alive.
    ///
    /// A real kernel refuses to create anything inside such a directory —
    /// `mkdir`, `open(O_CREAT)` and `symlink` are all ENOENT there, measured on
    /// both flavours — so a caller that is about to add a name must ask. That
    /// rule is also what keeps an orphan *empty*: a directory can only be
    /// orphaned by `rmdir`, which refuses a non-empty one, and it can never gain
    /// an entry afterwards.
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
    let isWithinSubtree (root : InodeNumber) (candidate : InodeNumber) (vfs : VirtualFileSystem) : bool =
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
    ///    third guest-reachable operation that adds a name, so it owes the same.
    ///    This is also what keeps `isOrphanedDirectory`'s stated invariant true:
    ///    an orphan is empty because `rmdir` refuses a populated directory *and*
    ///    nothing can afterwards put an entry into one.
    let rename
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

        if isOrphanedDirectory destinationDirectory vfs then
            failwith
                $"VirtualFileSystem.rename: the destination directory %O{destinationDirectory} has lost its last name, so binding \"%s{DirectoryEntryName.toString destinationName}\" into it would make inode %O{moved} unreachable from the root while it still has a name -- which nothing could then reap. The verdict owes ENOENT, exactly as it does for the creating operations."

        let displaced = Map.tryFind destinationName destinationContent.Entries

        if displaced = Some moved then
            failwith
                $"VirtualFileSystem.rename: \"%s{DirectoryEntryName.toString sourceName}\" in inode %O{sourceDirectory} and \"%s{DirectoryEntryName.toString destinationName}\" in inode %O{destinationDirectory} both name inode %O{moved}. That is rename(2)'s no-op, which changes nothing at all; the verdict must answer it rather than calling this."

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
                $"VirtualFileSystem.rename: the destination \"%s{DirectoryEntryName.toString destinationName}\" in inode %O{destinationDirectory} names directory inode %O{displaced.Value}, which holds %i{Map.count content.Entries} entries. Displacing it would strand them unreachable from the root; the verdict owes ENOTEMPTY."
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
                        $"VirtualFileSystem.rename: directory inode %O{sourceDirectory} bound \"%s{DirectoryEntryName.toString sourceName}\" to inode %O{moved}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

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
                        $"VirtualFileSystem.rename: directory inode %O{destinationDirectory} bound \"%s{DirectoryEntryName.toString destinationName}\" to inode %O{displaced}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        Ok (
            {
                Displaced = displaced
            },
            { vfs with
                Inodes = inodes
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
    /// refuses a guest that consumes two entries to skip the dots, or that
    /// expects the first entry to be one. A guest doing either is already broken
    /// on ext4, and the point of this interpreter is to say so deterministically
    /// rather than on whichever machine happens to run it.
    ///
    /// No caller may compare an enumeration order against a host: the order
    /// among the names is the map's, which matches no kernel at all.
    ///
    /// A stream over a directory `rmdir` has since removed is at end-of-stream
    /// at once, `.` and `..` included: probed on both kernels, `opendir` then
    /// `rmdir` then `readdir` answers NULL without yielding either dot. That is
    /// one of the two orderings a real kernel produces — reading an entry
    /// *first* and then removing yields the whole listing on both, because the
    /// answer depends on when `getdents` ran — so it is a lawful choice rather
    /// than a measured rule, and it is the less convenient of the two.
    /// `isOrphanedDirectory` is the whole test, because an orphan is empty by
    /// construction.
    let nextDirectoryEntry
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
                    $"VirtualFileSystem.nextDirectoryEntry: inode %O{directory} is not a directory this filesystem holds. A directory stream's inode is pinned by the descriptor that opened it, so this is an interpreter bug."

        if isOrphanedDirectory directory vfs then
            None
        else

        /// The least name this directory binds that is strictly greater than
        /// `lower`, or the least of all when there is no lower bound. A scan
        /// rather than a seek: `Map` offers no "least key above" query, and the
        /// cost (quadratic across a whole enumeration) is stated on the caller.
        let leastAbove (lower : DirectoryEntryName option) : (DirectoryEntryName * InodeNumber) option =
            content.Entries
            |> Map.toSeq
            |> Seq.filter (fun (name, _) ->
                match lower with
                | None -> true
                | Some lower -> name > lower
            )
            |> Seq.tryHead

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
    /// Partial, deliberately: the inode must be present and nothing may still
    /// name it. Both are interpreter bugs rather than anything a guest can
    /// cause — the caller has just unbound the last name and consulted the
    /// descriptor table — and forgetting a still-bound inode would leave a
    /// dangling entry that every later walk would trip over far from here.
    ///
    /// The number is not reused; see `VirtualFileSystem.NextInode`.
    let forget (inode : InodeNumber) (vfs : VirtualFileSystem) : VirtualFileSystem =
        if not (Map.containsKey inode vfs.Inodes) then
            failwith
                $"VirtualFileSystem.forget: inode %O{inode} is not in the graph, so it cannot be forgotten (this is an interpreter bug)."

        if inode = vfs.Root then
            failwith
                "VirtualFileSystem.forget: the root cannot be forgotten; every path resolves from it (this is an interpreter bug)."

        match bindingCount inode vfs with
        | 0 ->
            { vfs with
                Inodes = Map.remove inode vfs.Inodes
            }
        | count ->
            failwith
                $"VirtualFileSystem.forget: inode %O{inode} is still named by %d{count} directory entry/entries, so forgetting it would leave the graph with a dangling entry (this is an interpreter bug)."

    /// Write `bytes` at `offset` into the regular file at `inode`, moving its
    /// `mtime` and `ctime` and — unless `privilege` says otherwise — stripping its
    /// set-user-ID and set-group-ID bits.
    ///
    /// Those timestamps and no others: measured on both platforms, a write leaves
    /// `atime` where it was, and `birth` never moves at all.
    ///
    /// Partial in the inode, which must name a regular file this filesystem
    /// contains. A caller arrives here having resolved a descriptor open for
    /// writing, and only a regular file can be opened that way — `open(2)`
    /// answers EISDIR for a directory and resolves a symlink to whatever it names
    /// — so anything else is an interpreter bug rather than a guest error.
    ///
    /// Must not be called with an empty `bytes`: a zero-length write moves no
    /// timestamp and strips no bit, so treating it as an ordinary write of nothing
    /// would restamp the inode for a call a real kernel makes no record of. The
    /// caller short-circuits it.
    let writeFile
        (inode : InodeNumber)
        (offset : int64)
        (bytes : ImmutableArray<byte>)
        (rule : SetGroupIdOnWrite)
        (privilege : CallerPrivilege)
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
                $"VirtualFileSystem.writeFile: inode %O{inode} is a directory, so no descriptor naming it can be open for writing — `open(2)` answers EISDIR for every write access mode. The caller resolved a writable descriptor to it anyway (this is an interpreter bug)."
        | Some {
                   Content = InodeContent.Symlink _
               } ->
            failwith
                $"VirtualFileSystem.writeFile: inode %O{inode} is a symbolic link. `open` resolves symlinks, so no descriptor should name one (this is an interpreter bug)."
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
        let permissions =
            PermissionBits.afterContentChangingWrite rule privilege permissions

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
    /// `mtime` and `ctime` and — subject to `rule` and `privilege` — clearing its
    /// set-user-ID and set-group-ID bits.
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
    let truncateFile
        (inode : InodeNumber)
        (length : int64)
        (rule : SetIdBitsOnTruncation)
        (privilege : CallerPrivilege)
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
                $"VirtualFileSystem.truncateFile: inode %O{inode} was asked to become %d{length} bytes, which is negative. No kernel permits it; the caller must reject this as EINVAL before committing it (this is an interpreter bug)."


        match Map.tryFind inode vfs.Inodes with
        | None ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is not in this filesystem. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."
        | Some {
                   Content = InodeContent.Directory _
               } ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is a directory, so no descriptor naming it can be open for writing — `open(2)` answers EISDIR for every write access mode, and `ftruncate(2)` answers EINVAL for the read-only descriptor that is left. The caller resolved a writable descriptor to it anyway (this is an interpreter bug)."
        | Some {
                   Content = InodeContent.Symlink _
               } ->
            failwith
                $"VirtualFileSystem.truncateFile: inode %O{inode} is a symbolic link. `open` resolves symlinks, so no descriptor should name one (this is an interpreter bug)."
        | Some ({
                    Content = InodeContent.RegularFile (contents, permissions)
                } as entry) ->

        match truncatedContents contents length with
        | Error refusal -> Error refusal
        | Ok updated ->

        // A truncation is a mode change as well as a content change on one of the
        // two platforms, and the `ctime` below covers both either way.
        let permissions = PermissionBits.afterTruncation rule privilege permissions

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

    // ------------------------------------------------------------ resolution

    /// Expand a symbolic link into the walk's pathname buffer: the directory the
    /// expansion resumes from, and the buffer that replaces the one being
    /// walked.
    ///
    /// Refuses ELOOP when the traversal budget is spent and ENAMETOOLONG when
    /// the expansion would not fit, in that order.
    let private traverse
        (limits : PathLimits)
        (vfs : VirtualFileSystem)
        (directory : InodeNumber)
        (linkTarget : SymlinkTarget)
        (rest : PathCursor)
        (symlinks : int)
        : Result<InodeNumber * PathCursor, UnixError>
        =
        // Checked *before* the traversal happens, so a limit of 32 means
        // the 33rd attempt is the one that fails, matching both probes
        // (macOS resolves a chain of 32 and gives ELOOP at 33; Linux
        // resolves 40 and gives ELOOP at 41).
        //
        // This is also the bound that makes the walk terminate, which is
        // why there is no cycle detection: a seen-state set would *not*
        // be sufficient, because a link whose target names itself with a
        // suffix ("l" with target "l/x") grows the pathname buffer
        // forever without ever repeating a state. Only a count stops
        // that — and both real kernels use only a counter too, so this
        // is a transcription of their behaviour rather than an
        // approximation of it.
        if symlinks + 1 > PathLimits.maxSymlinkTraversals limits then
            Error UnixError.ELOOP
        else if

            // *After* the traversal count, because that is the order a
            // kernel checks them in and the two disagree. Measured on
            // Darwin: a chain whose last link both exhausts the budget and
            // would overflow the length reports ELOOP, while the same chain
            // one link shorter reports ENAMETOOLONG. XNU tests
            // `ni_loopcnt` in `namei` before it ever reads the target.
            //
            // Before the splice rather than after, so an overflowing
            // expansion is refused rather than performed — and note this
            // sees `rest`, whose cursor already sits past the separator run
            // the kernel collapsed, which is the whole reason the walk
            // carries a cursor.
            not (PathLimits.spliceWithinLimit limits linkTarget rest)
        then
            Error UnixError.ENAMETOOLONG
        else

        let linkPath = SymlinkTarget.toUnixPath linkTarget

        let next = if UnixPath.isRooted linkPath then vfs.Root else directory

        // Exactly what a kernel does to its pathname buffer: the target,
        // then whatever was left to resolve. Note this consumes `rest`,
        // whose cursor already sits past the separator run the kernel
        // collapsed — so the spliced buffer holds the same bytes the
        // kernel's would.
        Ok (next, PathCursor.splice linkPath rest)

    /// Consume components from `remaining` until the final name is in hand, or
    /// until the path runs out of names entirely.
    ///
    /// Every check the *prefix* of a path can fail — the holding directory's
    /// search bit, a non-final component's length, a non-final symlink's
    /// traversal budget and splice length — happens here. The final name's own
    /// length and lookup do not: they belong to `completeResolution`, and the
    /// split is what `PausedResolution` exists to express.
    ///
    /// The search bit is checked for the final component too, before this
    /// pauses. That is not an artefact of where the split fell: Linux's
    /// `link_path_walk` calls `may_lookup` at the top of each iteration, before
    /// it discovers whether the component it is about to hash is the last one,
    /// and `rename(unsearchable/f, absent/x)` is EACCES rather than ENOENT on
    /// both kernels.
    let rec private walkFrom
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (vfs : VirtualFileSystem)
        (directory : InodeNumber)
        (remaining : PathCursor)
        (trailing : bool)
        (finalSymlinkFollowed : bool)
        (lastNavigation : FinalNavigation)
        (symlinks : int)
        : Result<PausedResolution, UnixError>
        =
        let paused (final : (DirectoryEntryName * PathCursor) option) : PausedResolution =
            {
                Limits = limits
                Privilege = privilege
                Policy = policy
                TrailingSeparator = trailingSeparatorPolicy
                FileSystem = vfs
                Directory = directory
                Final = final
                Trailing = trailing
                FinalSymlinkFollowed = finalSymlinkFollowed
                LastNavigation = lastNavigation
                SymlinksTraversed = symlinks
            }

        match PathCursor.next remaining with
        // Reached when the path has no name left to look up: after a "." or
        // "..", or immediately for a path that named no component at all.
        //
        // Deliberately *before* the search check below, and that is what
        // makes `lstat("p")` and `lstat("p/")` succeed on an unsearchable
        // `p`: this walk never looks inside it. Measured on both kernels,
        // and `lstat("p/.")` is EACCES beside them -- one more place where
        // a trailing separator is not "/." and must not be desugared into
        // one.
        | None -> Ok (paused None)
        | Some (nextComponent, rest) ->

        // Consuming *any* component from `directory` -- a name, "." or ".."
        // alike -- is a lookup, and a lookup needs the directory's search
        // bit. Checked here, above the dispatch on which kind of component
        // it is, because all three need it: measured on both kernels,
        // `lstat("p/.")` and `lstat("p/..")` are EACCES exactly as
        // `lstat("p/kid")` is.
        //
        // Everything else this walk can refuse sits below this point, which
        // is what reproduces the measured precedence without encoding it:
        // against an unsearchable holding directory, a missing name, an
        // over-long one, a symlink cycle and (on Linux) a creating open's
        // trailing separator all report EACCES rather than the ENOENT,
        // ENAMETOOLONG, ELOOP or EISDIR the same call earns under a
        // searchable one.
        //
        // Only the *owner* triple can ever apply: `stat` reports
        // `Kernel.UserId` as every inode's `st_uid`, so the emulated process
        // owns everything it can see. Measured, and a corpus of ordinary
        // modes cannot show it: a 0o677 directory is EACCES to its owner
        // though group and other may search it, while 0o100 is searchable
        // though nobody else may.
        let searchBit = 0o100

        let directoryContent =
            match tryGetDirectory directory vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: about to consume a component from inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

        let maySearch =
            match privilege with
            | CallerPrivilege.Privileged -> true
            | CallerPrivilege.Unprivileged ->
                PermissionBits.toInt directoryContent.Permissions &&& searchBit = searchBit

        if not maySearch then
            Error UnixError.EACCES
        else

        match nextComponent with
        | PathComponent.Current ->
            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                directory
                rest
                trailing
                finalSymlinkFollowed
                FinalNavigation.Current
                symlinks
        | PathComponent.Parent ->
            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                directoryContent.Parent
                rest
                trailing
                finalSymlinkFollowed
                FinalNavigation.Parent
                symlinks
        | PathComponent.Name name ->

        // The final name's length is checked by `completeResolution` rather
        // than here, and that is measured rather than tidy: on Linux
        // `rename("<300 bytes>", "nodir/x")` is ENOENT, so the source's final
        // component is still unmeasured when the destination's parent walk
        // fails. A non-final component's length is checked here, in walk
        // order, which is what makes "<300 bytes>/x" ENAMETOOLONG while
        // "nxdir/<300 bytes>" is ENOENT.
        if PathCursor.isExhausted rest then
            Ok (paused (Some (name, rest)))
        else if

            not (PathLimits.nameWithinLimit limits name)
        then
            Error UnixError.ENAMETOOLONG
        else

        match Map.tryFind name directoryContent.Entries with
        | None -> Error UnixError.ENOENT
        | Some target ->

        let content =
            match tryGetContent target vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: directory inode %O{directory} binds \"%s{DirectoryEntryName.toString name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        match content with
        | InodeContent.Symlink linkTarget ->
            // A non-final symlink is always traversed, whatever the policy
            // says: `SymlinkPolicy.NoFollowFinal` is about the *final*
            // component alone, and no kernel offers a walk that stops at an
            // interior link.
            match traverse limits vfs directory linkTarget rest symlinks with
            | Error error -> Error error
            | Ok (next, spliced) ->

            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                next
                spliced
                trailing
                finalSymlinkFollowed
                // An interior link cannot leave the walk with nothing to
                // resolve — the remainder it was spliced in front of is
                // non-empty by construction — so, unlike the final case in
                // `completeResolution`, `lastNavigation` cannot become `Root`
                // here.
                lastNavigation
                (symlinks + 1)
        | InodeContent.Directory _ ->
            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                target
                rest
                trailing
                finalSymlinkFollowed
                lastNavigation
                symlinks
        // A path cannot continue through a regular file.
        | InodeContent.RegularFile _ -> Error UnixError.ENOTDIR

    /// Whether the directory this paused resolution would look its final name up
    /// in has lost its own last name — so no path reaches it, though whatever
    /// holds it keeps it alive. False for a path that has no final name to look
    /// up, matching the verdicts, which ask this only of an `Entry`.
    ///
    /// Answered here rather than by handing out the directory, because "the
    /// directory the final name is looked up in" is settled only while the walk
    /// is paused: completing it can follow a final symlink and move it. The one
    /// caller is `rename`, which must ask *between* its two final lookups —
    /// measured, Linux reports an orphaned destination parent before it measures
    /// the destination's final name, and after it measures the source's.
    let pausedParentIsOrphaned (paused : PausedResolution) : bool =
        match box paused with
        | null ->
            failwith
                "VirtualFileSystem.pausedParentIsOrphaned: this paused resolution is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from VirtualFileSystem.resolveParent instead."
        | _ ->

        match paused.Final with
        | None -> false
        | Some _ -> isOrphanedDirectory paused.Directory paused.FileSystem

    /// Look the final name up, finishing the resolution `resolveParent` paused.
    ///
    /// Not simply "one `Map.tryFind`": under a policy that follows the final
    /// symlink, the lookup can splice a target into the pathname buffer and
    /// send the walk on to a different directory, so this re-enters the parent
    /// walk and finishes again.
    let rec completeResolution (paused : PausedResolution) : Result<Resolution, UnixError> =
        // The record is a reference type, so `Unchecked.defaultof` and C#
        // `default` give null rather than a record of zeroed fields — and a
        // field access on that reports a NullReferenceException from inside the
        // walk, naming nothing a caller could act on.
        match box paused with
        | null ->
            failwith
                "VirtualFileSystem.completeResolution: this paused resolution is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from VirtualFileSystem.resolveParent instead."
        | _ -> ()

        let limits = paused.Limits
        let vfs = paused.FileSystem
        let directory = paused.Directory
        let trailing = paused.Trailing

        let finish (target : ResolvedTarget) : Result<Resolution, UnixError> =
            Ok
                {
                    Target = target
                    TrailingSeparatorDemanded = trailing
                    FinalSymlinkFollowed = paused.FinalSymlinkFollowed
                }

        match paused.Final with
        | None -> finish (ResolvedTarget.Directory (directory, paused.LastNavigation))
        | Some (name, rest) ->

        let directoryContent =
            match tryGetDirectory directory vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: about to look \"%s{DirectoryEntryName.toString name}\" up in inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

        // Before the length check, before the lookup, and before any symlink
        // this component names is traversed -- which is the whole content of
        // `RefuseIsDirectory`; see its docstring for the rows that pin the
        // position.
        match paused.TrailingSeparator with
        | TrailingSeparatorPolicy.RefuseIsDirectory when trailing -> Error UnixError.EISDIR
        | TrailingSeparatorPolicy.RefuseIsDirectory
        | TrailingSeparatorPolicy.Demand
        | TrailingSeparatorPolicy.Ignore ->

        // Before the lookup, and before anything notices whether the name
        // exists — which is what reproduces the measured precedence on both
        // kernels: "<300 bytes>/x" is ENAMETOOLONG (the over-long component
        // is reached and rejected) while "nxdir/<300 bytes>" is ENOENT (the
        // walk fails at the missing parent and never reaches it). Checking
        // after the lookup would report ENOENT for an over-long name that
        // does not exist, which is exactly what `stat` on a fresh long name
        // does *not* do.
        //
        // This is also the only place that sees components spliced in from a
        // symlink target, which a check at the syscall boundary could not.
        if not (PathLimits.nameWithinLimit limits name) then
            Error UnixError.ENAMETOOLONG
        else

        match Map.tryFind name directoryContent.Entries with
        | None ->
            // Not an error: the caller decides whether a free name is
            // ENOENT (`stat`) or the point of the call (`mkdir`). A
            // trailing separator does not change that — `mkdir("nx/")`
            // creates on both platforms.
            finish (ResolvedTarget.Entry (directory, name, None))
        | Some target ->

        let content =
            match tryGetContent target vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: directory inode %O{directory} binds \"%s{DirectoryEntryName.toString name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        // A trailing separator forces the final symlink to be followed even
        // under NoFollowFinal: POSIX resolves "p/" as "p/.", and both
        // platforms agree for lookups (probed: `lstat("ld/")` stats the
        // directory the link names). Where they *disagree* is what a
        // mutating caller may then do, which is why the fact is reported
        // rather than acted on.
        //
        // `Ignore` is the one walk that opts out, because Linux's creating
        // lookup does: see that case for the rows.
        let trailingActsOnFinal =
            match paused.TrailingSeparator with
            | TrailingSeparatorPolicy.Demand
            | TrailingSeparatorPolicy.RefuseIsDirectory -> trailing
            | TrailingSeparatorPolicy.Ignore -> false

        let followFinal = paused.Policy = SymlinkPolicy.Follow || trailingActsOnFinal

        match content with
        | InodeContent.Symlink linkTarget when followFinal ->
            match traverse limits vfs directory linkTarget rest paused.SymlinksTraversed with
            | Error error -> Error error
            | Ok (next, spliced) ->

            // The link's own trailing separator only takes effect when
            // nothing follows it: when the walk has more to resolve, the
            // separator joining the target to the remainder absorbs it.
            //
            // It *adds to* the outer demand rather than replacing it. The
            // separator in "ld/" applies to whatever ld expands to, so a
            // link with target "d" still has to land on a directory; and a
            // link with target "d/" imposes the demand even when the
            // guest's own path carried none.
            //
            // This demand is threaded rather than read back off the spliced
            // buffer, and must be: resolving "ld/" consumes the trailing
            // separator into the cursor (that is what the kernel's own
            // collapse does), so the spliced buffer is just the target and
            // has forgotten it. A kernel remembers the same fact the same
            // way — XNU latches `TRAILINGSLASH` on the component rather than
            // re-reading the buffer.
            let trailing =
                trailing || UnixPath.hasTrailingSeparator (SymlinkTarget.toUnixPath linkTarget)

            // An empty splice can only mean the target was "/", that being
            // the one path with no components; the effective path is then
            // the root itself rather than whatever navigation preceded the
            // link.
            let lastNavigation =
                if PathCursor.isExhausted spliced then
                    FinalNavigation.Root
                else
                    paused.LastNavigation

            walkFrom
                limits
                paused.Privilege
                paused.Policy
                paused.TrailingSeparator
                vfs
                next
                spliced
                trailing
                true
                lastNavigation
                (paused.SymlinksTraversed + 1)
            |> Result.bind completeResolution
        | InodeContent.Symlink _ ->
            // Final position under NoFollowFinal with no trailing
            // separator: the link itself is the answer, which is what
            // `lstat` and `readlink` need.
            finish (ResolvedTarget.Entry (directory, name, Some target))
        | InodeContent.Directory _ -> finish (ResolvedTarget.Entry (directory, name, Some target))
        | InodeContent.RegularFile _ ->
            // "p/" where p exists and is not a directory is ENOTDIR --
            // for every lookup, on both platforms, and for Darwin's
            // `mkdir`. Linux's `mkdir` is the exception and answers
            // EEXIST, which is what `Ignore` selects: it never asks what
            // the final component was.
            if trailingActsOnFinal then
                Error UnixError.ENOTDIR
            else
                finish (ResolvedTarget.Entry (directory, name, Some target))

    /// Resolve `path` against `startDirectory`, which is where a *relative*
    /// path begins; a rooted path ignores it and starts at the root — stopping
    /// short of the final lookup, which `completeResolution` performs.
    ///
    /// Everything a path's *prefix* can be refused for happens here, and the
    /// final name's length and lookup happen there. That boundary is measured,
    /// not chosen for tidiness: see `PausedResolution` for the pair of
    /// `rename` rows that pin it.
    ///
    /// A trailing separator is deliberately *not* desugared into a "." component
    /// here, even though POSIX describes the two as equivalent. They are not,
    /// for anything that mutates: probed on macOS, `mkdir("d/")` succeeds while
    /// `mkdir("nx/.")` gives ENOENT, and `rmdir("d/")` succeeds while
    /// `rmdir("d/.")` gives EINVAL. Desugaring would also collapse the
    /// `Entry` that `mkdir("d/")` needs into a `Directory`. The demand is
    /// instead recorded on `Resolution` and enforced only where every platform
    /// agrees — except where `trailingSeparatorPolicy` says the walk must refuse
    /// it outright, which is a fact about the kernel rather than about the
    /// caller; see `TrailingSeparatorPolicy`.
    let resolveParent
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<PausedResolution, UnixError>
        =
        // Checked here rather than trusted, because this is the boundary a
        // forged value crosses: `create` refuses a zero limit, but a struct's
        // `Unchecked.defaultof` carries one anyway.
        let limits = PathLimits.assertValid "VirtualFileSystem.resolveParent" limits

        // POSIX gives the empty path ENOENT (probed on both). Walking zero
        // components would instead silently answer "the directory I started
        // from".
        if UnixPath.isEmpty path then
            Error UnixError.ENOENT
        else

        let start =
            if UnixPath.isRooted path then
                Ok vfs.Root
            else

            match tryGetContent startDirectory vfs with
            | None -> Error UnixError.ENOENT
            | Some (InodeContent.Directory _) -> Ok startDirectory
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR

        match start with
        | Error error -> Error error
        | Ok start ->

        walkFrom
            limits
            privilege
            policy
            trailingSeparatorPolicy
            vfs
            start
            (PathCursor.ofPath path)
            (UnixPath.hasTrailingSeparator path)
            false
            FinalNavigation.Root
            0

    /// Resolve `path` all the way: `resolveParent` followed by
    /// `completeResolution`, which is what every caller resolving a single path
    /// wants. Only a caller interleaving two resolutions needs the halves.
    let resolveFull
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<Resolution, UnixError>
        =
        resolveParent limits privilege startDirectory policy trailingSeparatorPolicy path vfs
        |> Result.bind completeResolution

    /// `resolveFull`, discarding the how-it-finished facts. For the lookup
    /// operations, which are unanimous across platforms and so need none of
    /// them.
    let resolve
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<ResolvedTarget, UnixError>
        =
        resolveFull limits privilege startDirectory policy TrailingSeparatorPolicy.Demand path vfs
        |> Result.map (fun resolution -> resolution.Target)

    /// The inode a resolved target names. Turns a free final name into ENOENT,
    /// which is the one thing `resolve` deliberately does not do — so this is
    /// where "the file must already exist" is decided, once, for every caller
    /// that needs it.
    let existingOf (target : ResolvedTarget) : Result<InodeNumber, UnixError> =
        match target with
        | ResolvedTarget.Directory (inode, _) -> Ok inode
        | ResolvedTarget.Entry (_, _, Some inode) -> Ok inode
        | ResolvedTarget.Entry (_, _, None) -> Error UnixError.ENOENT

    /// The inode a path names, which is what `stat` and a non-creating `open`
    /// want: `resolve` followed by `existingOf`.
    let resolveExisting
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber, UnixError>
        =
        resolve limits privilege startDirectory policy path vfs
        |> Result.bind existingOf

    // ------------------------------------------------------------ inspection

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
            | InodeContent.Symlink _ -> []
        )

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
            // guest reaches through `Environment.CurrentDirectory` and every
            // relative `Path.GetFullPath` — and the copy made the cost scale
            // with how many *siblings* the directory has, not with the depth of
            // its path. Measured over one directory of 100,000 entries:
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
            let rendered =
                names
                |> List.map DirectoryEntryName.toString
                |> List.fold (fun acc name -> acc + string UnixPathText.separator + name) ""

            if rendered = "" then
                AbsoluteUnixPath.root
            else
                AbsoluteUnixPath.parseOrFail "VirtualFileSystem.pathOfDirectory" rendered
        )

    /// Every way in which `vfs` fails to describe a filesystem a kernel could
    /// produce, or the empty list if it is sound. Deterministic in order, so a
    /// failing test reports the same thing every run.
    ///
    /// `pinned` names the inodes some process holds open. Deletion makes an
    /// inode with no remaining name legitimate — a real kernel keeps one alive
    /// for as long as a descriptor refers to it — but *only* while something
    /// holds it, and whether anything does is a fact about a descriptor table
    /// rather than about this graph. So the caller that can see both supplies
    /// it, and every unreachable inode outside the set is still a defect. Pass
    /// `Set.empty` for a graph no process has opened anything in.
    ///
    /// A pinned inode that is perfectly reachable is not an error: the
    /// overwhelmingly common case is a descriptor on a file that still has its
    /// name. The set excuses unreachability; it does not assert it.
    ///
    /// Nothing here checks that a pinned inode is *in* the graph. That is the
    /// mirror-image defect — a descriptor naming an inode the filesystem has
    /// forgotten — and it belongs to the layer holding the descriptor table:
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

        rootDefects
        @ rootLinks
        @ danglingEntries
        @ parentDefects
        @ unreachable
        @ freshness

    /// Fail loudly if `vfs` is not sound, naming `context`. For the operations
    /// that build a filesystem from host configuration, where a defect is a
    /// host bug rather than anything a guest could have caused.
    let assertInvariants (context : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        // Nothing pinned: these callers build a filesystem out of host
        // configuration, before any guest exists to have opened anything, so an
        // inode no path reaches is a bug in the builder every time.
        match checkInvariants Set.empty vfs with
        | [] -> vfs
        | defects ->
            let rendered = defects |> List.map (sprintf "%A") |> String.concat "; "
            failwith $"%s{context}: the inode graph is not a filesystem any kernel could produce: %s{rendered}"

    /// Construction that bypasses every invariant this module maintains.
    ///
    /// Exists so that `checkInvariants` can be tested: a defect no test can
    /// construct is documentation rather than a check. Deliberately one
    /// greppable token, so that any interpreter code reaching for it is visible
    /// in review — nothing outside tests should.
    [<RequireQualifiedAccess>]
    module Unchecked =
        let ofParts
            (inodes : Map<InodeNumber, Inode>)
            (root : InodeNumber)
            (nextInode : InodeNumber)
            : VirtualFileSystem
            =
            {
                Inodes = inodes
                Root = root
                NextInode = nextInode
            }

/// Identity of one open directory stream. Never guest-visible: a guest holds a
/// `DIR*`, and what that pointer is made of is the client's business, not this
/// kernel's.
///
/// Minted monotonically and never reused, as `SocketId` and `InodeNumber` are;
/// `EmulatedKernel.NextDirectoryStreamId` is the counter, and `checkInvariants`
/// refuses a table holding an id at or above it.
[<Struct>]
type DirectoryStreamId =
    | DirectoryStreamId of value : int64

    override this.ToString () : string =
        match this with
        | DirectoryStreamId value -> string<int64> value

/// One open directory stream: what `opendir(3)` returns and `readdir`/`closedir`
/// consume.
///
/// Held in `EmulatedKernel.DirectoryStreams` rather than on the descriptor,
/// because libc keeps a `DIR`'s buffer and position in userspace and the
/// descriptor carries only the kernel's. The consequence is that two `opendir`s
/// of one directory advance independently, and a `dup` of the descriptor would
/// not share the cursor. Unobservable: `dirfd` appears nowhere in CoreLib or
/// the PAL, so no managed caller can reach the descriptor to `dup` it.
type DirectoryStream =
    {
        /// The descriptor `opendir` opened, closed again by `closedir`.
        Fd : int
        /// The directory being enumerated. Also reachable through `Fd`, but
        /// held directly so that a guest which closed that descriptor behind the
        /// stream's back — undefined behaviour on a real libc, and possible here
        /// because fd numbers are guessable — does not turn into an interpreter
        /// crash.
        Inode : InodeNumber
        /// How far through `Inode` this stream has read.
        Cursor : DirectoryCursor
    }
