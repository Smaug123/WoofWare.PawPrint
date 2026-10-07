namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// A bit of `open(2)`'s flag word that the simulated flavour defines and this
/// kernel does not model. `UnixNamespace.openPath` refuses a word holding any
/// of them (`OpenRefusal.UnmodelledFlags`) rather than ignore what the caller
/// asked for.
[<RequireQualifiedAccess>]
type UnmodelledOpenFlag =
    /// `O_NOCTTY`, on both flavours.
    | NoControllingTerminal
    /// `O_APPEND`, on both flavours.
    | Append
    /// `O_NONBLOCK`, on both flavours.
    | NonBlocking
    /// `O_ASYNC` (Linux's `FASYNC`), on both flavours.
    | Asynchronous
    /// `O_DSYNC` without `O_SYNC`, on both flavours.
    | DataSynchronous
    /// Linux's `O_DIRECT`.
    | Direct
    /// Linux's `O_LARGEFILE`.
    | LargeFile
    /// Linux's `O_NOATIME`.
    | NoAccessTime
    /// Linux's `O_PATH`.
    | PathOnly
    /// Linux's `__O_TMPFILE`, the bit `O_TMPFILE` adds to `O_DIRECTORY`.
    | TemporaryFile
    /// Darwin's `O_SHLOCK`.
    | SharedLock
    /// Darwin's `O_EXLOCK`.
    | ExclusiveLock
    /// Darwin's `O_RESOLVE_BENEATH`.
    | ResolveBeneath
    /// Darwin's `O_UNIQUE`.
    | Unique
    /// Darwin's `O_EVTONLY`.
    | EventOnly
    /// Darwin's `O_SYMLINK`.
    | Symlink
    /// Darwin's `O_CLOFORK`.
    | CloseOnFork
    /// Darwin's `O_NOFOLLOW_ANY`.
    | NoFollowAny
    /// Darwin's `O_EXEC`, which with `O_DIRECTORY` is `O_SEARCH`.
    | Execute
    /// Darwin's `O_POPUP`.
    | Popup

[<RequireQualifiedAccess>]
module UnmodelledOpenFlag =
    /// The flag's name in the flavour's `<fcntl.h>`.
    let name (flag : UnmodelledOpenFlag) : string =
        match flag with
        | UnmodelledOpenFlag.NoControllingTerminal -> "O_NOCTTY"
        | UnmodelledOpenFlag.Append -> "O_APPEND"
        | UnmodelledOpenFlag.NonBlocking -> "O_NONBLOCK"
        | UnmodelledOpenFlag.Asynchronous -> "O_ASYNC"
        | UnmodelledOpenFlag.DataSynchronous -> "O_DSYNC"
        | UnmodelledOpenFlag.Direct -> "O_DIRECT"
        | UnmodelledOpenFlag.LargeFile -> "O_LARGEFILE"
        | UnmodelledOpenFlag.NoAccessTime -> "O_NOATIME"
        | UnmodelledOpenFlag.PathOnly -> "O_PATH"
        | UnmodelledOpenFlag.TemporaryFile -> "__O_TMPFILE"
        | UnmodelledOpenFlag.SharedLock -> "O_SHLOCK"
        | UnmodelledOpenFlag.ExclusiveLock -> "O_EXLOCK"
        | UnmodelledOpenFlag.ResolveBeneath -> "O_RESOLVE_BENEATH"
        | UnmodelledOpenFlag.Unique -> "O_UNIQUE"
        | UnmodelledOpenFlag.EventOnly -> "O_EVTONLY"
        | UnmodelledOpenFlag.Symlink -> "O_SYMLINK"
        | UnmodelledOpenFlag.CloseOnFork -> "O_CLOFORK"
        | UnmodelledOpenFlag.NoFollowAny -> "O_NOFOLLOW_ANY"
        | UnmodelledOpenFlag.Execute -> "O_EXEC"
        | UnmodelledOpenFlag.Popup -> "O_POPUP"

/// What a caller asked `open(2)` for, as facts about the open rather than as a
/// bit pattern: the flag word as `OpenFlagWord.decode` reads it for one
/// flavour.
///
/// Every field is a flag this kernel acts on, or (`CloseOnExec`,
/// `Synchronous`, `DataSynchronous`) one whose only effect here is what
/// `fcntl` reports. A word
/// holding any other bit its flavour defines never becomes one of these: the
/// decoder refuses it, so the kernel cannot guess at a flag it does not
/// model.
type internal OpenFlags =
    {
        /// `O_RDONLY`, `O_WRONLY` or `O_RDWR`, access modes 0, 1 and 2. The
        /// fourth, 3, never decodes to one of these: Darwin answers EINVAL
        /// for it, and Linux opens a descriptor that can neither read nor
        /// write, which this kernel refuses.
        Access : FileAccessMode
        /// `O_CREAT`: create the final component if nothing holds that name.
        Create : bool
        /// `O_EXCL`: fail EEXIST if the final component exists.
        ///
        /// Exactly as the word set it, **without** first ANDing it with
        /// `Create`. That it does nothing on its own is a measured kernel fact
        /// `UnixNamespace.openPathParsed` owns — `open(existing,
        /// O_WRONLY|O_EXCL)` succeeds and `open(missing, O_WRONLY|O_EXCL)` is
        /// ENOENT, exactly as without it.
        Exclusive : bool
        /// `O_TRUNC`: empty a regular file that is opened successfully.
        ///
        /// Not confined to a write access mode: measured on both,
        /// `open(f, O_RDONLY | O_TRUNC)` on a writable file succeeds and empties
        /// it. What it does instead is demand the write permission bit.
        Truncate : bool
        /// `O_NOFOLLOW`: do not follow a symbolic link in the final position,
        /// which makes opening one ELOOP.
        NoFollow : bool
        /// `O_CLOEXEC`: give the new descriptor `FD_CLOEXEC`.
        CloseOnExec : bool
        /// `O_SYNC` (on Linux, the `__O_SYNC` bit, which the kernel completes to
        /// `O_SYNC` whether or not `O_DSYNC` is beside it). It governs when a
        /// write reaches storage rather than whether it is visible, and this
        /// filesystem holds its bytes in memory, so every write is already as
        /// durable as the model gets; the description carries it for `fcntl`
        /// to report.
        Synchronous : bool
        /// `O_DSYNC`, as the description will carry it: on Linux whenever
        /// `Synchronous` is set, the kernel completing `__O_SYNC` with it; on
        /// Darwin when the word holds it, which the decoder admits only beside
        /// `O_SYNC`.
        DataSynchronous : bool
        /// `O_DIRECTORY`: fail ENOTDIR unless the path names a directory.
        ///
        /// Modelled only as `opendir(3)` uses it: with `O_RDONLY`, and without
        /// `O_TRUNC` or `O_NOFOLLOW` (`O_CREAT` with it is EINVAL). The decoder
        /// refuses every other combination, whose check order against those
        /// flags is unmodelled.
        Directory : bool
    }

/// Why this kernel will not answer a `readlink(2)`.
[<RequireQualifiedAccess>]
type ReadLinkRefusal =
    /// The destination buffer has no answer at the step the call reached.
    | Buffer of BufferRefusal
    /// This kernel will not resolve the path.
    | Path of PathRefusal
    /// Whether the caller may read the symbolic link at `inode` has not been
    /// measured for this caller.
    | UnmeasuredLinkRead of inode : InodeNumber * refusal : LinkReadRefusal

[<RequireQualifiedAccess>]
module ReadLinkRefusal =
    /// What this kernel knows about why it cannot answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : ReadLinkRefusal) : string =
        match refusal with
        | ReadLinkRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | ReadLinkRefusal.Path refusal -> PathRefusal.describe refusal
        | ReadLinkRefusal.UnmeasuredLinkRead (inode, refusal) ->
            $"reading inode %O{inode}: %s{LinkReadRefusal.describe refusal}"

/// What `readlink(2)` puts in the caller's buffer and what it returns.
[<RequireQualifiedAccess>]
type ReadLinkAnswer =
    /// Place these bytes in the caller's buffer; the entry point returns how
    /// many there are.
    ///
    /// **No terminator**, and truncated to the capacity rather than refused:
    /// `readlink` writes exactly the bytes it reports and reports success by a
    /// non-negative count, so a NUL would corrupt the byte after a target that
    /// exactly fits. Truncation is not an error path: `readlink(2)` truncates
    /// silently, so a result that fills the buffer is how a caller learns to
    /// retry with a larger one.
    | Reported of bytes : ImmutableArray<byte>
    /// The entry point returns -1 and the caller stores `error` wherever its
    /// libc keeps errno.
    | Failed of error : UnixError

/// What kind of object one directory entry names.
///
/// Not `InodeContent`, which carries the payload as well — a caller enumerating
/// a directory is owed the *type* of each entry and nothing else, and handing it
/// the bytes of every file in the directory would be a different API. Not
/// `fileTypeBits` either: that is the `S_IFMT` numbering `stat` reports, where
/// `readdir` has its own (`DT_REG` and friends), and the two are not the same
/// numbers. A client encodes whichever its own struct wants.
[<RequireQualifiedAccess>]
type DirectoryEntryKind =
    | RegularFile
    | Directory
    | Symlink
    | CharacterDevice

[<RequireQualifiedAccess>]
module DirectoryEntryKind =
    /// What kind of entry a directory binding onto this content is.
    let ofContent (content : InodeContent) : DirectoryEntryKind =
        match content with
        | InodeContent.RegularFile _ -> DirectoryEntryKind.RegularFile
        | InodeContent.Directory _ -> DirectoryEntryKind.Directory
        | InodeContent.Symlink _ -> DirectoryEntryKind.Symlink
        | InodeContent.CharacterDevice _ -> DirectoryEntryKind.CharacterDevice

/// One entry of a directory, as `getdents(2)` reports it: the facts in a
/// `struct linux_dirent64` or a Darwin `struct direntry`, without either's
/// layout.
type DirectoryRecord =
    {
        /// `d_ino`: the inode the entry names. For `.` that is the directory
        /// itself and for `..` its parent, as `stat` would report them.
        Inode : InodeNumber
        /// `d_name`.
        Name : DirectoryStreamName
        /// `d_type`. Never unknown: measured on tmpfs and APFS, every entry,
        /// the dots included, reports its kind.
        Kind : DirectoryEntryKind
    }

/// What reading the next entry of a directory descriptor answers.
[<RequireQualifiedAccess>]
type ReadDirectoryAnswer =
    /// The next entry. The description's position has moved past it.
    | Entry of record : DirectoryRecord
    /// There is nothing further: `getdents` returns 0.
    | EndOfDirectory
    /// `getdents` returns -1 with this errno.
    | Failed of error : UnixError

/// Why this kernel will not say what reading a directory descriptor yields.
[<RequireQualifiedAccess>]
type ReadDirectoryRefusal =
    /// `lseek` moved the description to a nonzero offset (see
    /// `DirectoryPosition.Unenumerable`), and what each filesystem yields from
    /// an offset it did not hand out is its own.
    | UnenumerablePosition of inode : InodeNumber * offset : int64
    /// The description is onto `inode`, a directory of the device filesystem.
    /// A real one lists a node for every device the machine has and this
    /// kernel's holds only the nodes of the devices it has drivers for, so a
    /// listing would leave out names a real one reports.
    | DeviceFileSystem of inode : InodeNumber

[<RequireQualifiedAccess>]
module ReadDirectoryRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic.
    let describe (refusal : ReadDirectoryRefusal) : string =
        match refusal with
        | ReadDirectoryRefusal.UnenumerablePosition (inode, offset) ->
            $"the description onto directory %O{inode} is at offset %d{offset}, where lseek put it. Both kernels accept that offset, but what the next read yields from it is each filesystem's own (tmpfs resumes from the entry with the greatest offset at or below it; APFS skips that many entries, or answers EAGAIN, depending on the high word), and this kernel's position is a name rather than an offset. Only offset 0, which rewinds, is readable from."
        | ReadDirectoryRefusal.DeviceFileSystem inode ->
            $"the description is onto directory %O{inode}, on the device filesystem. A real one lists a node for every device the machine has, and this kernel's holds only the nodes of the devices it has drivers for, so its listing would leave out names a real one reports."

/// How far `rename(2)` had got with its source when it stopped to copy its
/// *destination* pathname in — which is not the same point on the two flavours.
///
/// See `RenameWalkOrder`, whose two cases these mirror.
[<RequireQualifiedAccess>]
type RenameSourceProgress =
    /// The source's parent is walked and its final component not yet looked up:
    /// Linux, which walks both parents before either final lookup.
    | ParentWalked of parent : PausedResolution
    /// The source is finished, `RenameRules.sourceScreen` included: Darwin,
    /// which resolves the source to completion before touching the destination.
    | Resolved of resolution : Resolution

/// A `rename(2)` that has run as far as the point where the kernel copies its
/// destination pathname in, and stopped there.
///
/// It stops rather than taking both pathnames up front because *reading* a
/// pathname out of a process's address space can fail — and on both flavours
/// there are calls that finish without ever reading the destination, where
/// failing to read it would answer about a pathname `rename(2)` never touched.
/// The caller supplies the bytes when handed one of these, and not before.
///
/// Opaque: the only thing to do with one is give it to
/// `UnixNamespace.renameWithDestination`.
[<NoEquality ; NoComparison>]
type PausedRename<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            System : UnixSystem<'Task, 'Handler>
            Rules : RenameRules
            SourceProgress : RenameSourceProgress
        }

/// What `UnixNamespace.renameSourcePhase` found: either the call is over without
/// the destination having been read at all, or the kernel has reached the point
/// where it copies that pathname in.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type RenameProgress<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// Finished. The destination pathname was never read, and must not be.
    | Answered of answer : SyscallAnswer * system : UnixSystem<'Task, 'Handler>
    /// The kernel is at the destination's copy-in. Hand its bytes to
    /// `UnixNamespace.renameWithDestination`.
    | NeedsDestination of paused : PausedRename<'Task, 'Handler>

/// Why this kernel will not answer a `rename(2)`.
[<RequireQualifiedAccess>]
type RenameRefusal =
    /// A sticky directory whose rule Darwin has not been measured to apply to
    /// this caller.
    | Sticky of refusal : StickyRefusal
    /// This kernel will not resolve one of the paths.
    | Path of refusal : PathRefusal
    /// One of the paths names `mountRoot`, the root of a mounted filesystem,
    /// in a combination whose answer has not been measured.
    | MountPoint of mountRoot : InodeNumber
    /// The call would move `name` out of `directory`, on the device
    /// filesystem, which no name can be removed from here.
    | DeviceFileSystem of directory : InodeNumber * name : DirectoryEntryName

[<RequireQualifiedAccess>]
module RenameRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which paths.
    let describe (refusal : RenameRefusal) : string =
        match refusal with
        | RenameRefusal.Sticky refusal -> StickyRefusal.describe refusal
        | RenameRefusal.Path refusal -> PathRefusal.describe refusal
        | RenameRefusal.MountPoint mountRoot ->
            $"one of the paths names inode %O{mountRoot}, the root of a mounted filesystem. Measured on Linux, renaming it to a free name in its own directory is EACCES without write on that directory and EBUSY with it. Any other rename involving it can depend on the covered directory's own owner and mode, which this kernel does not hold, and has not been measured."
        | RenameRefusal.DeviceFileSystem (directory, name) ->
            $"the call would move \"%s{DirectoryEntryName.toEscaped name}\" out of inode %O{directory}, on the device filesystem, which holds only the nodes of the devices this kernel has drivers for; it removes none of them, because it could not then say what a real one answers for the name."

/// Why this kernel will not answer a `clonefile(2)`.
[<RequireQualifiedAccess>]
type CloneFileRefusal =
    /// This kernel is not Darwin-flavoured, and only Darwin has `clonefile`.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The machine's mount is of this type, where whether a file can be cloned
    /// is unmeasured.
    | UnmeasuredFileSystem of fileSystem : EmulatedFileSystemType
    /// `flags` asks for `CLONE_NOFOLLOW`, `CLONE_NOFOLLOW_ANY` or
    /// `CLONE_RESOLVE_BENEATH`, which change how a pathname resolves in ways
    /// this kernel does not model.
    | UnmodelledFlags of flags : int
    /// The caller is privileged, which changes who owns the clone and which
    /// permission bits it keeps, unmeasured.
    | PrivilegedCaller
    /// The source is the directory at `inode`; cloning one copies its whole
    /// tree, which this kernel does not model.
    | DirectorySource of inode : InodeNumber
    /// The source at `inode` carries set-ID or sticky bits, and the caller
    /// stands towards it as `standing`: which of those bits a clone keeps has
    /// been measured only for an owner in the source's group, and for an
    /// owner outside it without `S_ISGID`.
    | UnmeasuredSpecialBits of inode : InodeNumber * standing : Standing * permissions : PermissionBits
    /// This kernel will not resolve one of the paths.
    | Path of PathRefusal

[<RequireQualifiedAccess>]
module CloneFileRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which paths.
    let describe (refusal : CloneFileRefusal) : string =
        match refusal with
        | CloneFileRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and clonefile exists on Darwin only."
        | CloneFileRefusal.UnmeasuredFileSystem fileSystem ->
            $"the machine's mount is %O{fileSystem}, where whether clonefile can share a file's blocks (or answers ENOTSUP or EXDEV) has not been measured."
        | CloneFileRefusal.UnmodelledFlags flags ->
            $"flags 0x%x{flags} ask for CLONE_NOFOLLOW (0x1), CLONE_NOFOLLOW_ANY (0x8) or CLONE_RESOLVE_BENEATH (0x10). Each changes how a pathname resolves, and cloning a symbolic link itself is not modelled."
        | CloneFileRefusal.PrivilegedCaller ->
            "the caller is privileged. A privileged clone keeps the source's owner unless CLONE_NOOWNERCOPY is given, and which permission bits it keeps has not been measured."
        | CloneFileRefusal.DirectorySource inode ->
            $"the source is directory %O{inode}. clonefile clones a directory's whole tree, which this kernel does not model."
        | CloneFileRefusal.UnmeasuredSpecialBits (inode, standing, permissions) ->
            $"the source, inode %O{inode}, has mode 0o%o{PermissionBits.toInt permissions}, and the caller stands towards it as %O{standing}. A clone drops both set-ID bits and keeps the sticky bit when an unprivileged owner in the source's group clones it, but which of these bits survive any other caller's clone has not been measured."
        | CloneFileRefusal.Path refusal -> PathRefusal.describe refusal

/// A `clonefile(2)` whose flags have been screened, stopped at the point
/// where the kernel copies its source pathname in.
///
/// It stops there because reading a pathname out of a process's address space
/// can fail, and a call whose flags are refused never reads either pathname.
///
/// Opaque: the only thing to do with one is give it to
/// `UnixNamespace.cloneFileSourcePhase`.
[<NoEquality ; NoComparison>]
type PausedCloneFileSource<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            System : UnixSystem<'Task, 'Handler>
        }

/// What `UnixNamespace.cloneFileFlagsPhase` found: either the call is over
/// without either pathname having been read, or the kernel has reached the
/// point where it copies the source pathname in.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type CloneFileScreen<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// Finished. Neither pathname was read, and neither must be.
    | Answered of answer : SyscallAnswer * system : UnixSystem<'Task, 'Handler>
    /// The kernel is at the source's copy-in. Hand its bytes to
    /// `UnixNamespace.cloneFileSourcePhase`.
    | NeedsSource of paused : PausedCloneFileSource<'Task, 'Handler>

/// A `clonefile(2)` that has resolved its source and stopped at the point
/// where the kernel copies its destination pathname in.
///
/// It stops rather than taking both pathnames up front because reading a
/// pathname out of a process's address space can fail, and a call whose
/// source fails never reads the destination.
///
/// Opaque: the only thing to do with one is give it to
/// `UnixNamespace.cloneFileWithDestination`.
[<NoEquality ; NoComparison>]
type PausedCloneFile<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            System : UnixSystem<'Task, 'Handler>
            Source : InodeNumber
        }

/// What `UnixNamespace.cloneFileSourcePhase` found: either the call is over
/// without the destination having been read, or the kernel has reached the
/// point where it copies that pathname in.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type CloneFileProgress<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// Finished. The destination pathname was never read, and must not be.
    | Answered of answer : SyscallAnswer * system : UnixSystem<'Task, 'Handler>
    /// The kernel is at the destination's copy-in. Hand its bytes to
    /// `UnixNamespace.cloneFileWithDestination`.
    | NeedsDestination of paused : PausedCloneFile<'Task, 'Handler>

/// Why this kernel will not answer a `symlink(2)` or `symlinkat(2)`.
[<RequireQualifiedAccess>]
type SymlinkRefusal =
    /// This kernel will not resolve the link's pathname.
    | Path of refusal : PathRefusal
    /// The call would create a link whose target is empty, which Darwin does
    /// and this library does not represent: see `SymlinkTargetError.Empty`.
    /// Every check that would fail the call has already passed.
    | EmptyTarget

[<RequireQualifiedAccess>]
module SymlinkRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which pathnames.
    let describe (refusal : SymlinkRefusal) : string =
        match refusal with
        | SymlinkRefusal.Path refusal -> PathRefusal.describe refusal
        | SymlinkRefusal.EmptyTarget ->
            "the call would create a symbolic link with an empty target. Darwin creates one, which every walk through answers ENOENT; this library does not represent an empty target (SymlinkTargetError.Empty)."

/// A `symlink(2)` or `symlinkat(2)` whose target this kernel has copied in,
/// paused at the point where it copies the link's own pathname in. Obtain one
/// from `UnixNamespace.symlinkatTargetPhase` or `symlinkTargetPhase`, and finish
/// it with `UnixNamespace.symlinkWithPath`.
[<NoEquality ; NoComparison>]
type PausedSymlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            System : UnixSystem<'Task, 'Handler>
            Directory : AtDirectory
            /// `None` for an empty target, which only a flavour that accepts one
            /// gets this far with.
            Target : SymlinkTarget option
        }

/// What copying in a `symlink(2)`'s target found: either the call is over
/// without the link's pathname having been read at all, or the kernel has
/// reached the point where it copies that pathname in.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type SymlinkProgress<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// Finished, and changing nothing. The link's pathname was never read, and
    /// must not be: an unreadable target is EFAULT whatever that pathname is.
    | Answered of answer : SyscallAnswer
    /// The kernel is at the link's pathname's copy-in. Hand its bytes to
    /// `UnixNamespace.symlinkWithPath`.
    | NeedsPath of paused : PausedSymlink<'Task, 'Handler>

/// Why this kernel will not answer a `link(2)` or `linkat(2)`.
[<RequireQualifiedAccess>]
type LinkRefusal =
    /// This kernel will not resolve one of the pathnames.
    | Path of refusal : PathRefusal
    /// The flag word `flags` carries flags the flavour accepts and this library
    /// does not model; see `LinkScreen.Unmodelled`.
    | UnmodelledFlags of flags : int
    /// Linux's `AT_EMPTY_PATH`, from a caller that is not privileged, with a
    /// path (an empty one included) relative to a descriptor. Linux then
    /// answers `ENOENT` unless the descriptor was opened with the very
    /// credentials the caller now holds: compared by identity, so that a fork,
    /// or a change of credentials that keeps every ID, fails it. This library
    /// does not record the credentials a descriptor was opened with.
    | OpenTimeCredentials
    /// `AT_EMPTY_PATH` names `inode`, which has no name left and is not a
    /// regular file: which of the call's refusals such an inode meets first
    /// has not been measured.
    | NamelessSource of inode : InodeNumber
    /// The source is on a `fileSystem` whose rules for `link(2)`, its ceiling
    /// on a file's names among them, have not been measured.
    | UnmeasuredFileSystem of fileSystem : EmulatedFileSystemType

[<RequireQualifiedAccess>]
module LinkRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which pathnames.
    let describe (refusal : LinkRefusal) : string =
        match refusal with
        | LinkRefusal.Path refusal -> PathRefusal.describe refusal
        | LinkRefusal.UnmodelledFlags flags ->
            $"the flag word 0x%x{flags} carries a flag Darwin accepts and this library does not model: AT_SYMLINK_NOFOLLOW_ANY (0x800), AT_RESOLVE_BENEATH (0x2000) or AT_UNIQUE (0x8000)."
        | LinkRefusal.OpenTimeCredentials ->
            "AT_EMPTY_PATH from a caller that is not privileged, with a path relative to a descriptor: Linux admits it only if the descriptor was opened with the caller's present credentials, and this library does not record the credentials a descriptor was opened with."
        | LinkRefusal.NamelessSource inode ->
            $"AT_EMPTY_PATH names inode %O{inode}, which has no name left and is not a regular file; which refusal such an inode meets first has not been measured."
        | LinkRefusal.UnmeasuredFileSystem fileSystem ->
            $"the source is on a %O{fileSystem} filesystem, whose rules for link(2) (its ceiling on a file's names among them) have not been measured."

/// A `link(2)` or `linkat(2)` whose source has resolved, paused at the point
/// where the kernel copies the new pathname in. Obtain one from
/// `UnixNamespace.linkatSourcePhase` or `linkSourcePhase`, and finish it with
/// `UnixNamespace.linkWithDestination`.
[<NoEquality ; NoComparison>]
type PausedLink<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            System : UnixSystem<'Task, 'Handler>
            Rules : LinkRules
            Source : InodeNumber
            Destination : AtDirectory
        }

/// What resolving a `link(2)`'s source found: either the call is over without
/// the new pathname having been read at all, or the kernel has reached the
/// point where it copies that pathname in.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type LinkProgress<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// Finished, and changing nothing. The new pathname was never read, and
    /// must not be.
    | Answered of answer : SyscallAnswer
    /// The kernel is at the new pathname's copy-in. Hand its bytes to
    /// `UnixNamespace.linkWithDestination`.
    | NeedsDestination of paused : PausedLink<'Task, 'Handler>

/// Why this kernel will not answer an `open(2)`.
[<RequireQualifiedAccess>]
type OpenRefusal =
    /// A descriptor the call would make lies at or above the bound this kernel
    /// assumes the process's `RLIMIT_NOFILE` reaches.
    | DescriptorLimit of DescriptorLimitRefusal
    /// An `O_TRUNC` open of the file at `inode`, whose effect on the file's
    /// set-ID bits has not been measured for this caller. Nothing was changed.
    | UnmeasuredSetIdChange of inode : InodeNumber * refusal : SetIdChangeRefusal
    /// The flag word `flags` holds bits the simulated flavour defines and this
    /// kernel does not model, named in `unmodelled` in ascending order of bit.
    /// Nothing was read or changed.
    | UnmodelledFlags of flags : int * unmodelled : UnmodelledOpenFlag list
    /// The flag word `flags` asks for Linux's access mode 3, which opens a
    /// descriptor that can neither read nor write and serves only `ioctl(2)`
    /// and the calls that need no access mode. This kernel's descriptions
    /// permit reading, writing or both. Nothing was read or changed.
    | IoctlOnlyAccessMode of flags : int
    /// The flag word `flags` asks for `O_DIRECTORY` with a write access mode,
    /// `O_TRUNC` or `O_NOFOLLOW`. This kernel models `O_DIRECTORY` only as
    /// `opendir(3)` uses it, alone with `O_RDONLY`. Nothing was read or
    /// changed.
    | UnmodelledDirectoryOpen of flags : int
    /// This kernel will not resolve the path. Nothing was read or changed.
    | Path of PathRefusal

[<RequireQualifiedAccess>]
module OpenRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : OpenRefusal) : string =
        match refusal with
        | OpenRefusal.DescriptorLimit refusal -> DescriptorLimitRefusal.describe refusal
        | OpenRefusal.UnmeasuredSetIdChange (inode, refusal) ->
            $"opening inode %O{inode} with O_TRUNC: %s{SetIdChangeRefusal.describe refusal}"
        | OpenRefusal.UnmodelledFlags (flags, unmodelled) ->
            let names = unmodelled |> List.map UnmodelledOpenFlag.name |> String.concat ", "

            $"flags 0x%x{flags} hold %s{names}, which this platform defines and this kernel does not model; model them before answering."
        | OpenRefusal.IoctlOnlyAccessMode flags ->
            $"flags 0x%x{flags} ask for access mode 3, which Linux opens (demanding the read and write permission bits) as a descriptor that can neither read nor write: read, write and flock answer EBADF, ftruncate EINVAL, and only ioctl and the calls that need no access mode succeed. This kernel's descriptions permit reading, writing or both; model the fourth before answering."
        | OpenRefusal.UnmodelledDirectoryOpen flags ->
            $"flags 0x%x{flags} ask for O_DIRECTORY with a write access mode, O_TRUNC or O_NOFOLLOW. Only O_DIRECTORY|O_RDONLY (what opendir(3) opens with) is modelled; where ENOTDIR falls among EISDIR, EACCES and ELOOP for any other combination is not."
        | OpenRefusal.Path refusal -> PathRefusal.describe refusal

/// `open(2)`'s flag word, in the simulated flavour's own `<fcntl.h>`
/// numbering, read as the kernel reads it.
[<RequireQualifiedAccess>]
module internal OpenFlagWord =

    /// What the flag word asks for, or how the call ends without reading the
    /// path.
    [<RequireQualifiedAccess>]
    type Decoding =
        /// The word is one this kernel models, asking for this.
        | Decoded of OpenFlags
        /// The kernel answers this errno before it copies the path in.
        | Fails of UnixError
        | Refused of OpenRefusal

    /// What one bit above the access mode asks for.
    [<RequireQualifiedAccess>]
    type private Meaning =
        | Create
        | Exclusive
        | Truncate
        | NoFollow
        | Directory
        | CloseOnExec
        | Synchronous
        /// `O_DSYNC`, which `O_SYNC` beside it subsumes.
        | DataSynchronous
        | Unmodelled of UnmodelledOpenFlag

    // Each flavour's numbering of every bit it defines above the access mode,
    // in ascending order of bit. Measured by open-flags.c on Linux 6.18.5
    // aarch64, Linux 6.12.111 x86-64 and Darwin 27.0 arm64: a single-bit row
    // differs from the bare access mode's for exactly these bits, bar those
    // that cannot act alone (O_EXCL; O_NOCTTY on a file that is not a
    // terminal; Linux's O_LARGEFILE, which a 64-bit kernel sets on every
    // description; Darwin's O_SYMLINK and O_POPUP). Every other bit is ignored
    // by both kernels, Darwin included, so it is ignored here too.
    //
    // Linux's O_SYNC is two bits, __O_SYNC and O_DSYNC, and the kernel reads
    // __O_SYNC alone as O_SYNC (F_GETFL reads both back).

    let private linuxBits (architecture : SimulatedUnixArchitecture) : (int * Meaning) list =
        // aarch64's <asm/fcntl.h> moves four bits; x86-64 keeps the generic
        // numbering.
        let directory = OpenFlagNumbering.linuxDirectory architecture
        let noFollow = OpenFlagNumbering.linuxNoFollow architecture
        let largeFile = OpenFlagNumbering.linuxLargeFile architecture

        let direct = OpenFlagNumbering.linuxDirect architecture

        [
            0x40, Meaning.Create
            OpenFlagNumbering.LinuxExclusive, Meaning.Exclusive
            0x100, Meaning.Unmodelled UnmodelledOpenFlag.NoControllingTerminal
            0x200, Meaning.Truncate
            OpenFlagNumbering.LinuxAppend, Meaning.Unmodelled UnmodelledOpenFlag.Append
            OpenFlagNumbering.LinuxNonBlock, Meaning.Unmodelled UnmodelledOpenFlag.NonBlocking
            OpenFlagNumbering.LinuxDataSynchronous, Meaning.DataSynchronous
            OpenFlagNumbering.LinuxAsynchronous, Meaning.Unmodelled UnmodelledOpenFlag.Asynchronous
            direct, Meaning.Unmodelled UnmodelledOpenFlag.Direct
            largeFile, Meaning.Unmodelled UnmodelledOpenFlag.LargeFile
            directory, Meaning.Directory
            noFollow, Meaning.NoFollow
            OpenFlagNumbering.LinuxNoAccessTime, Meaning.Unmodelled UnmodelledOpenFlag.NoAccessTime
            OpenFlagNumbering.LinuxCloseOnExec, Meaning.CloseOnExec
            OpenFlagNumbering.LinuxSynchronous, Meaning.Synchronous
            0x200000, Meaning.Unmodelled UnmodelledOpenFlag.PathOnly
            0x400000, Meaning.Unmodelled UnmodelledOpenFlag.TemporaryFile
        ]
        |> List.sortBy fst

    let private darwinBits : (int * Meaning) list =
        [
            OpenFlagNumbering.DarwinNonBlock, Meaning.Unmodelled UnmodelledOpenFlag.NonBlocking
            OpenFlagNumbering.DarwinAppend, Meaning.Unmodelled UnmodelledOpenFlag.Append
            0x10, Meaning.Unmodelled UnmodelledOpenFlag.SharedLock
            0x20, Meaning.Unmodelled UnmodelledOpenFlag.ExclusiveLock
            OpenFlagNumbering.DarwinAsynchronous, Meaning.Unmodelled UnmodelledOpenFlag.Asynchronous
            OpenFlagNumbering.DarwinSynchronous, Meaning.Synchronous
            0x100, Meaning.NoFollow
            0x200, Meaning.Create
            0x400, Meaning.Truncate
            0x800, Meaning.Exclusive
            0x1000, Meaning.Unmodelled UnmodelledOpenFlag.ResolveBeneath
            0x2000, Meaning.Unmodelled UnmodelledOpenFlag.Unique
            0x8000, Meaning.Unmodelled UnmodelledOpenFlag.EventOnly
            0x20000, Meaning.Unmodelled UnmodelledOpenFlag.NoControllingTerminal
            0x100000, Meaning.Directory
            0x200000, Meaning.Unmodelled UnmodelledOpenFlag.Symlink
            OpenFlagNumbering.DarwinDataSynchronous, Meaning.DataSynchronous
            OpenFlagNumbering.DarwinCloseOnExec, Meaning.CloseOnExec
            OpenFlagNumbering.DarwinCloseOnFork, Meaning.Unmodelled UnmodelledOpenFlag.CloseOnFork
            0x20000000, Meaning.Unmodelled UnmodelledOpenFlag.NoFollowAny
            0x40000000, Meaning.Unmodelled UnmodelledOpenFlag.Execute
            0x80000000, Meaning.Unmodelled UnmodelledOpenFlag.Popup
        ]

    /// Read `flags` as `platform`'s kernel does.
    let decode (platform : SimulatedUnixPlatform) (flags : int) : Decoding =
        let flavour = SimulatedUnixPlatform.flavour platform

        let meanings =
            match flavour with
            | SimulatedUnixFlavour.Linux -> linuxBits (SimulatedUnixPlatform.architecture platform)
            | SimulatedUnixFlavour.Darwin -> darwinBits
            |> List.filter (fun (bit, _) -> flags &&& bit <> 0)
            |> List.map snd

        let has (meaning : Meaning) : bool = List.contains meaning meanings

        let unmodelled =
            meanings
            |> List.choose (fun meaning ->
                match meaning with
                | Meaning.Unmodelled flag -> Some flag
                | Meaning.DataSynchronous when not (has Meaning.Synchronous) -> Some UnmodelledOpenFlag.DataSynchronous
                | Meaning.Create
                | Meaning.Exclusive
                | Meaning.Truncate
                | Meaning.NoFollow
                | Meaning.Directory
                | Meaning.CloseOnExec
                | Meaning.Synchronous
                | Meaning.DataSynchronous -> None
            )

        // Refused first: a flag this kernel does not model can lift a screen
        // below (Linux's O_PATH lifts O_CREAT|O_DIRECTORY's EINVAL), so no
        // answer is certain once one is present.
        if not unmodelled.IsEmpty then
            Decoding.Refused (OpenRefusal.UnmodelledFlags (flags, unmodelled))
        // Measured on both, for every word of modelled and undefined bits:
        // EINVAL, before the path is copied in. Linux has done so since 6.4.
        elif has Meaning.Create && has Meaning.Directory then
            Decoding.Fails UnixError.EINVAL
        else

        match flags &&& 3 with
        | 3 ->
            match flavour with
            // Measured for every word: EINVAL, before the path is copied in.
            | SimulatedUnixFlavour.Darwin -> Decoding.Fails UnixError.EINVAL
            | SimulatedUnixFlavour.Linux -> Decoding.Refused (OpenRefusal.IoctlOnlyAccessMode flags)
        | access ->

        let decoded : OpenFlags =
            {
                Access =
                    match access with
                    | 0 -> FileAccessMode.ReadOnly
                    | 1 -> FileAccessMode.WriteOnly
                    | _ -> FileAccessMode.ReadWrite
                Create = has Meaning.Create
                Exclusive = has Meaning.Exclusive
                Truncate = has Meaning.Truncate
                NoFollow = has Meaning.NoFollow
                CloseOnExec = has Meaning.CloseOnExec
                Synchronous = has Meaning.Synchronous
                DataSynchronous =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> has Meaning.Synchronous
                    | SimulatedUnixFlavour.Darwin -> has Meaning.DataSynchronous
                Directory = has Meaning.Directory
            }

        if
            decoded.Directory
            && (decoded.Access <> FileAccessMode.ReadOnly
                || decoded.Truncate
                || decoded.NoFollow)
        then
            Decoding.Refused (OpenRefusal.UnmodelledDirectoryOpen flags)
        else
            Decoding.Decoded decoded

[<RequireQualifiedAccess>]
module UnixNamespace =

    /// Who owns an inode the process is about to create in `directory`, which
    /// the caller's walk has just established is a directory.
    let private newInodeOwner<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (directory : InodeNumber)
        (system : UnixSystem<'Task, 'Handler>)
        : InodeOwner
        =
        match VirtualFileSystem.tryGet directory system.Machine.FileSystem with
        | Some ({
                    Content = InodeContent.Directory parent
                } as entry) ->
            InodeOwner.ofNewInode
                (SimulatedUnixPlatform.newInodeGroupRule system.Machine.UnixPlatform)
                system.Process.Credentials
                entry.Owner
                parent.Permissions
        | Some _
        | None ->
            failwith
                $"%s{context}: about to create an inode in inode %O{directory}, which the walk had established was a directory, but it is now absent or not a directory (this is a bug in this library)."

    /// `openat`, of a path this kernel has already copied in, starting from
    /// `directory` if it is relative.
    let internal openPathParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (directory : AtDirectory)
        (flags : OpenFlags)
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, OpenRefusal>
        =
        // Measured (`openat-limit.c`): Linux's empty path is ENOENT even with
        // no descriptor left, since its copy-in refuses it; Darwin has
        // allocated the descriptor before it copies anything in (`openPath`).
        match
            (SimulatedUnixPlatform.startingPointRules system.Machine.UnixPlatform).EmptyPath, UnixPath.isEmpty path
        with
        | EmptyPathRule.NoSuchEntryBeforeDescriptor, true -> Ok (SyscallAnswer.Failed UnixError.ENOENT, system)
        | EmptyPathRule.NoSuchEntryBeforeDescriptor, false
        | EmptyPathRule.NoSuchEntryAfterDescriptor, _ ->

        // Measured (`fcntl-dup.c`'s LIMIT rows, and `openat-limit.c` for every
        // kind of dirfd): with no descriptor left below the limit, a missing
        // file is EMFILE rather than ENOENT, and so is a dirfd naming nothing
        // or no directory, on both: the descriptor comes before the walk, and
        // with it before `dirfd` is looked at.
        match
            FileDescriptorRegistry.room
                (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform)
                0
                1
                (UnixSystemState.fileDescriptors system)
        with
        | Error refusal -> Error (OpenRefusal.DescriptorLimit refusal)
        | Ok () ->

        let rules = SimulatedUnixPlatform.creatingOpenRules system.Machine.UnixPlatform
        let credentials = system.Process.Credentials

        // `O_EXCL` on its own is neither an error nor a refusal: both kernels
        // ignore it entirely, measured. So it is read
        // only where `Create` is set, and that combining is done here rather than
        // by the caller -- it is the kernel's rule, and a caller that pre-ANDed
        // the two would leave it with nothing to be right or wrong about.
        let exclusive = flags.Create && flags.Exclusive

        /// Hand out a descriptor onto `inode` for the access that was asked for,
        /// with the descriptor flag and the status flags the word asked for.
        /// `truncatedExisting` is whether this open's `O_TRUNC` truncated a file
        /// that was there before it.
        let opened
            (truncatedExisting : bool)
            (inode : InodeNumber)
            (system : UnixSystem<'Task, 'Handler>)
            : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, OpenRefusal>
            =
            // A directory gets a description positioned in its entries rather
            // than at a byte offset. It can only be here for reading: every
            // writable or truncating open of one answered EISDIR before this.
            let fd, registry =
                match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                | Some (InodeContent.Directory _) ->
                    FileDescriptorRegistry.openDirectory inode (UnixSystemState.fileDescriptors system)
                | Some (InodeContent.CharacterDevice (device, _)) ->
                    FileDescriptorRegistry.openCharacterDevice
                        inode
                        device
                        flags.Access
                        (UnixSystemState.fileDescriptors system)
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _)
                | None -> FileDescriptorRegistry.openFile inode flags.Access (UnixSystemState.fileDescriptors system)

            let registry =
                FileDescriptorRegistry.setFlags
                    fd
                    { DescriptorFlags.none with
                        CloseOnExec = flags.CloseOnExec
                    }
                    registry

            // Each kept under the flavour whose F_GETFL reports it
            // (`OpenFileStatus`): measured (`open-flags.c`), Linux keeps
            // O_DIRECTORY and O_NOFOLLOW, and Darwin marks a description whose
            // open truncated a file that existed (`fcntl-dup.c`, WRITTEN rows).
            let linux =
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> true
                | SimulatedUnixFlavour.Darwin -> false

            let registry =
                match FileDescriptorRegistry.tryFindId fd registry with
                | Some id ->
                    registry
                    |> FileDescriptorRegistry.mapOpenFiles (
                        OpenFileTable.mapStatus
                            id
                            (fun status ->
                                { status with
                                    Synchronous = flags.Synchronous
                                    DataSynchronous = flags.DataSynchronous
                                    OpenedDirectory = linux && flags.Directory
                                    OpenedNoFollow = linux && flags.NoFollow
                                    Written = not linux && truncatedExisting
                                }
                            )
                    )
                | None ->
                    failwith
                        $"UnixNamespace.openPath: fd %d{fd} was handed out a moment ago and is not live (this is a bug in this library)."

            Ok (SyscallAnswer.Completed (int64 fd), UnixSystemState.withFileDescriptors registry system)

        if
            flags.Directory
            && (flags.Access <> FileAccessMode.ReadOnly
                || flags.Create
                || flags.Truncate
                || flags.NoFollow)
        then
            failwith
                $"UnixNamespace.openPath: O_DIRECTORY with %A{flags}, a combination OpenFlagWord.decode refuses or answers EINVAL before the path is read (this is a bug in this library)."

        if flags.Directory then
            // `opendir(3)` is exactly this open, and its rows are measured on
            // both kernels, which agree in every one: a final symlink is
            // followed, a trailing separator changes nothing, being a file beats
            // being unreadable, and it is the read bit rather than the search
            // bit that is demanded. `OpenDirRules` holds them.
            match
                UnixPathResolution.resolvePathFull
                    directory
                    SymlinkPolicy.Follow
                    TrailingSeparatorPolicy.Demand
                    path
                    system
            with
            | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
            | Error (PathFailure.Refused refusal) -> Error (OpenRefusal.Path refusal)
            | Ok resolution ->

            match OpenDirRules.verdict credentials resolution system.Machine.FileSystem with
            | OpenDirVerdict.Refuse error -> Ok (SyscallAnswer.Failed error, system)
            | OpenDirVerdict.Open inode -> opened false inode system
        else

        // `O_CREAT|O_EXCL` does not follow a final symlink -- measured
        // unanimously: an existing link is EEXIST whether it dangles, points at a
        // file, or points at itself, and nothing is created. Selecting `Follow`
        // here would create the *target* of a dangling link, and would answer
        // ELOOP for a cyclic one, where both kernels answer EEXIST.
        let policy =
            if flags.NoFollow || exclusive then
                SymlinkPolicy.NoFollowFinal
            else
                SymlinkPolicy.Follow

        let trailingSeparatorPolicy =
            if flags.Create then
                rules.TrailingSeparator
            else
                TrailingSeparatorPolicy.Demand

        match UnixPathResolution.resolvePathFull directory policy trailingSeparatorPolicy path system with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (OpenRefusal.Path refusal)
        | Ok resolution ->

        match
            CreatingOpenRules.verdict
                rules
                system.Machine.ProtectedFiles
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                credentials
                flags.Create
                exclusive
                resolution
                system.Machine.FileSystem
        with
        | CreatingOpenVerdict.Refuse error -> Ok (SyscallAnswer.Failed error, system)
        | CreatingOpenVerdict.Create (directory, name) ->
            let parent =
                match VirtualFileSystem.tryGet directory system.Machine.FileSystem with
                | Some ({
                            Content = InodeContent.Directory parent
                        } as entry) -> entry, parent
                | Some _
                | None ->
                    failwith
                        $"UnixNamespace.openPath: about to create \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory}, which the walk had established was a directory, but it is now absent or not a directory (this is a bug in this library)."

            let permissions =
                CreatingOpenRules.createdPermissions
                    rules
                    (Standing.toward credentials (fst parent).Owner)
                    (snd parent).Permissions
                    system.Process.Umask
                    mode

            let now = UnixMachineState.realtime system.Machine

            match
                VirtualFileSystem.createFile
                    directory
                    name
                    permissions
                    (newInodeOwner "UnixNamespace.openPath" directory system)
                    now
                    ImmutableArray<byte>.Empty
                    system.Machine.FileSystem
            with
            | Error error ->
                // `createFile` refuses a name the directory already holds, and a
                // parent that is not a directory. The walk has just established
                // neither is the case, so either is a broken graph rather than
                // something the caller did.
                failwith
                    $"UnixNamespace.openPath: creating \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and does not hold that name (this is a bug in this library)."
            | Ok (inode, filesystem) ->
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
                // A file this open created is not one it truncated, whatever
                // `O_TRUNC` asked: measured on Darwin (`fcntl-dup.c`, WRITTEN
                // rows), its description is not marked written.
                |> opened false inode
        | CreatingOpenVerdict.OpenExisting inode ->

        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixNamespace.openPath: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        match entry.Content with
        | InodeContent.Symlink _ ->
            // Only reachable under `O_NOFOLLOW`, which is what `NoFollowFinal`
            // above selects: without it the resolver would have followed the link
            // (or failed ENOENT on a dangling one). ELOOP rather than anything
            // more specific is what both Unixes answer.
            Ok (SyscallAnswer.Failed UnixError.ELOOP, system)
        | InodeContent.Directory _ when FileAccessMode.permitsWrite flags.Access || flags.Truncate ->
            // Measured on both flavours, for `O_WRONLY` and `O_RDWR` alike, and
            // at uid 0 as well as uid 1000: a directory cannot be opened for
            // writing, and this beats the EACCES check below (a mode-0000
            // directory opened `O_WRONLY` is EISDIR, not EACCES).
            //
            // This is also what makes every writable descriptor name a regular
            // file, which `VirtualFileSystem.writeFile` relies on.
            //
            // `O_TRUNC` earns the same refusal whatever the access mode:
            // measured, `open(d, O_RDONLY | O_TRUNC)` is EISDIR on both, so this
            // is the one row where the arm fires for a *read-only* open. That
            // includes `O_CREAT | O_RDONLY | O_TRUNC` on the flavour whose
            // `RefusesExistingDirectory` is false, where the verdict is therefore
            // `OpenExisting` on the directory itself.
            Ok (SyscallAnswer.Failed UnixError.EISDIR, system)
        | InodeContent.RegularFile _
        | InodeContent.Directory _
        | InodeContent.CharacterDevice _ ->

        // A directory opens perfectly well for *reading*, measured on both. A
        // caller that wants to know what it opened asks `fstat`. So does a
        // device's node, for every access mode, its permission bits checked as
        // a file's are (`devices.c`, OPEN rows).
        let permissionBits = Inode.permissions entry

        // What `open(2)` itself checks: whether this process may open *this
        // object* for the access it asked for. Measured identically on macOS and
        // Linux, at uid 1000:
        //
        //   mode   O_RDONLY  O_WRONLY  O_RDWR
        //   0644   ok        ok        ok
        //   0444   ok        EACCES    EACCES
        //   0200   EACCES    ok        EACCES
        //   0000   EACCES    EACCES    EACCES
        //
        // The triple consulted is the one the caller's standing towards the
        // file selects (`PermissionBits.deniedTo`).
        //
        // `O_TRUNC` adds the write bit to whatever the access mode already asked
        // for, and adds nothing else. Measured at uid 1000 on both:
        //
        //   mode   flags               answer
        //   0444   RDONLY|TRUNC        EACCES
        //   0400   RDONLY|TRUNC        EACCES
        //   0200   RDONLY|TRUNC        EACCES   (the read bit is still owed)
        //   0600   RDONLY|TRUNC        ok
        //   0200   WRONLY|TRUNC        ok
        //   0400   WRONLY|TRUNC        EACCES
        // Both halves, where the mode asks for both: `O_RDWR` on a 0o400 file is
        // refused for want of the write bit even though the read bit is there,
        // which is what the disjunction says.
        let standing = Standing.toward credentials entry.Owner

        let denied =
            (FileAccessMode.permitsRead flags.Access
             && PermissionBits.deniedTo standing AccessRequest.Read permissionBits)
            || ((FileAccessMode.permitsWrite flags.Access || flags.Truncate)
                && PermissionBits.deniedTo standing AccessRequest.Write permissionBits)

        if denied then
            Ok (SyscallAnswer.Failed UnixError.EACCES, system)
        else

        // Only now, with every refusal discharged: measured, a refused open
        // leaves the bytes alone, and specifically `O_CREAT | O_EXCL | O_TRUNC`
        // on an existing file is EEXIST with its contents intact, while
        // `O_NOFOLLOW | O_TRUNC` on a symbolic link is ELOOP with its target
        // intact.
        //
        // Unconditional rather than skipped for an already-empty file: the
        // inode's timestamps move and its set-ID bits go regardless. Only a
        // regular file is truncated -- a directory cannot reach here at all (the
        // arm above refuses every truncating open of one), so the match is over
        // what the descriptor may still name rather than a filter. A device's
        // node is left alone: measured, `O_TRUNC` opens one without moving
        // anything `stat` reports (`devices.c`, OPEN rows).
        let truncated =
            match entry.Content with
            | InodeContent.RegularFile _ when flags.Truncate ->
                match UnixDescriptor.truncateAt inode 0L system with
                | Ok system -> Ok (true, system)
                | Error (TruncationRefusal.UnmeasuredSetIdChange (inode, refusal)) ->
                    Error (OpenRefusal.UnmeasuredSetIdChange (inode, refusal))
                | Error (TruncationRefusal.ExceedsRepresentableLength _ as refusal) ->
                    // Truncating to zero cannot exceed a length limit.
                    failwith
                        $"UnixNamespace.openPath: truncating inode %O{inode} to zero was refused -- %s{TruncationRefusal.describe refusal} (this is a bug in this library)."
            | InodeContent.RegularFile _
            | InodeContent.Directory _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> Ok (false, system)

        truncated
        |> Result.bind (fun (truncatedExisting, system) -> opened truncatedExisting inode system)

    let private openFrom<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (directory : AtDirectory)
        (flags : int)
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, OpenRefusal>
        =
        // Before the path is copied in: measured (`open-flags.c`), each
        // kernel screens the word whatever the path pointer is.
        // Measured (`fcntl-dup.c`, LIMIT rows): Darwin allocates the
        // descriptor before it reads the word or the path, so with none left
        // below the limit even its EINVAL and EFAULT are EMFILE; Linux reads
        // both first.
        let room =
            FileDescriptorRegistry.room
                (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform)
                0
                1
                (UnixSystemState.fileDescriptors system)

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, room with
        | SimulatedUnixFlavour.Darwin, Error refusal -> Error (OpenRefusal.DescriptorLimit refusal)
        | SimulatedUnixFlavour.Darwin, Ok ()
        | SimulatedUnixFlavour.Linux, _ ->

        match OpenFlagWord.decode system.Machine.UnixPlatform flags with
        | OpenFlagWord.Decoding.Refused refusal -> Error refusal
        | OpenFlagWord.Decoding.Fails error -> Ok (SyscallAnswer.Failed error, system)
        | OpenFlagWord.Decoding.Decoded flags ->

        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> openPathParsed directory flags path mode system

    /// `open(2)`: resolve `path`, apply every check a kernel makes, and return a
    /// descriptor onto what it names. It is `openat` from `AT_FDCWD`.
    ///
    /// `flags` is raw, in the simulated flavour's own `<fcntl.h>` numbering,
    /// which differs between Linux's architectures as well as between
    /// flavours. A bit the flavour does not define is ignored, as both
    /// kernels ignore it. A bit it defines is either one this kernel models
    /// or a refusal naming it (`OpenRefusal.UnmodelledFlags`), never silently
    /// dropped.
    ///
    /// The word is screened before the path is copied in, so its EINVAL comes
    /// ahead of EFAULT, ENAMETOOLONG, EEXIST and EISDIR: on both flavours for
    /// `O_CREAT|O_DIRECTORY`, and on Darwin for access mode 3. Then `path` is
    /// the argument's bytes, copied in: EFAULT if they were unreadable,
    /// ENAMETOOLONG if they run past `PATH_MAX`.
    ///
    /// Named for the path it takes, `open` being an F# keyword and
    /// `FileDescriptorRegistry.openFile` already meaning "open this inode". It
    /// opens directories too, for reading.
    ///
    /// `mode` is raw and **unvalidated**, and must stay that way:
    /// callers commonly pass 0666 even for a read-only open of an existing file,
    /// and a kernel accepts that, so refusing a nonzero mode without `O_CREAT`
    /// would refuse an ordinary read. It is read only when a file is actually created,
    /// and then masked rather than rejected: measured, `mode` 0o10777 creates
    /// 0o0755 on both flavours, so a bit above the permission word is dropped
    /// exactly as the platform's own mask drops it.
    ///
    /// Refused, before anything is read or changed, for a flag word this kernel
    /// does not model (see `OpenRefusal`), for an `O_TRUNC` open whose effect
    /// on set-ID bits is unmeasured, and when no descriptor below the bound
    /// (`SimulatedUnixPlatform.descriptorBound`) is free: under Darwin ahead of
    /// every errno, and under Linux after the word's and the copy-in's (an
    /// empty path's ENOENT among them) and before the walk's. Every other
    /// outcome is a descriptor or an errno.
    let openPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : int)
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, OpenRefusal>
        =
        openFrom AtDirectory.CurrentDirectory flags path mode system

    /// `openat(2)`: `openPath` of `path`, starting from `dirfd` if it is
    /// relative; `openPath` is this from `AT_FDCWD`.
    ///
    /// `dirfd` is raw, in this platform's own numbering. Everything `openPath`
    /// says holds, with one more step: once the path is copied in, and once
    /// Linux has a descriptor to give, a relative path starts where `dirfd`
    /// says, as every `*at` call's does (`UnixPathResolution.walkStart`). So
    /// with no descriptor left below the bound the call is refused ahead of
    /// a `dirfd` that names nothing or no directory, on both flavours; and on
    /// Linux an unreadable, over-long or empty path is answered ahead of that
    /// refusal, while on Darwin the refusal comes first.
    let openat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (dirfd : int)
        (path : PathArgumentBytes)
        (flags : int)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, OpenRefusal>
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
        openFrom (AtDirectory.decode flavour dirfd) flags path mode system

    /// `readlinkat`, of a path this kernel has already copied in, starting
    /// from `directory` if it is relative.
    let internal readlinkParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (directory : AtDirectory)
        (path : UnixPath)
        (destination : UserBuffer)
        (capacity : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadLinkAnswer, ReadLinkRefusal>
        =
        let verdict =
            SimulatedUnixPlatform.readlinkCapacity system.Machine.UnixPlatform capacity

        match verdict with
        | ReadLinkCapacityVerdict.Refuse error -> Ok (ReadLinkAnswer.Failed error)
        | ReadLinkCapacityVerdict.ReportNothing
        | ReadLinkCapacityVerdict.Admit ->

        // The inode to read, and what the call answers if it is not a link.
        let (target : Result<Result<InodeNumber, UnixError>, ReadLinkRefusal>), (notALink : UnixError) =
            match SimulatedUnixPlatform.readlinkEmptyPath system.Machine.UnixPlatform, directory with
            | EmptyPathMeaning.NamesStartingPoint, AtDirectory.Descriptor fd when UnixPath.isEmpty path ->
                // Measured (`readlinkat-empty-path.c`, and `at-dirfd.c`'s
                // eventq rows): a pipe, a socket or an epoll instance is
                // ENOENT, as a file or directory that is not a link is.
                let named =
                    match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system) with
                    | None -> Error UnixError.EBADF
                    | Some description ->

                    match description.Target with
                    | OpenFileTarget.File (inode, _)
                    | OpenFileTarget.Directory (inode, _)
                    | OpenFileTarget.CharacterDevice (inode, _) -> Ok inode
                    | OpenFileTarget.Pipe _
                    | OpenFileTarget.Socket _
                    | OpenFileTarget.Epoll _
                    | OpenFileTarget.Kqueue _ -> Error UnixError.ENOENT

                Ok named, UnixError.ENOENT
            | EmptyPathMeaning.NamesStartingPoint, _
            | EmptyPathMeaning.Walked, _ ->
                // An empty path from AT_FDCWD names the current directory,
                // which is not a link, so it is ENOENT, as the walk answers it.
                //
                // `NoFollowFinal` is what makes this `readlink` rather than an
                // expensive way of asking about the target: a final symlink is
                // the thing being read, not something to step through. A
                // trailing separator still overrides that -- "lf/" demands
                // that `lf` be a directory -- and the resolver owns that rule,
                // answering ENOTDIR.
                let resolved =
                    match UnixPathResolution.resolvePath directory SymlinkPolicy.NoFollowFinal path system with
                    | Error (PathFailure.Errno error) -> Ok (Error error)
                    | Error (PathFailure.Refused refusal) -> Error (ReadLinkRefusal.Path refusal)
                    | Ok inode -> Ok (Ok inode)

                resolved, UnixError.EINVAL

        match target with
        | Error refusal -> Error refusal
        | Ok (Error error) -> Ok (ReadLinkAnswer.Failed error)
        | Ok (Ok inode) ->

        let record =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some record -> record
            | None ->
                failwith
                    $"UnixNamespace.readlink: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        match record.Content with
        | InodeContent.Directory _
        | InodeContent.RegularFile _
        | InodeContent.CharacterDevice _ ->
            // Not a link: EINVAL for a path, which is what distinguishes "not a
            // link" from a failure to read one, and ENOENT for an empty path
            // naming the starting point.
            // Decided before the destination is looked at, which is what a real
            // kernel does -- `vfs_readlink` refuses on the inode's operations
            // before it copies anything out. Measured on the host:
            // `readlink("f", (char*)8, 16)` is EINVAL, not EFAULT.
            Ok (ReadLinkAnswer.Failed notALink)
        | InodeContent.Symlink (target, bits) ->

        // The link's own mode, on a flavour that consults it, is judged once
        // the path is known to name a link and before the size or the buffer
        // is: measured on Darwin (`readlink-mode.c`'s ORDER rows), a link the
        // caller may not read is EACCES with a size of 0 and with a NULL or
        // unmapped buffer, while a negative size is EINVAL first and a
        // trailing separator ("ld/", "l/") follows the link whatever its mode.
        let standing = Standing.toward system.Process.Credentials record.Owner

        match
            PermissionBits.linkReadDenied (SimulatedUnixPlatform.linkReadRule system.Machine.UnixPlatform) standing bits
        with
        | Error refusal -> Error (ReadLinkRefusal.UnmeasuredLinkRead (inode, refusal))
        | Ok true -> Ok (ReadLinkAnswer.Failed UnixError.EACCES)
        | Ok false ->

        match verdict with
        | ReadLinkCapacityVerdict.Refuse _ ->
            failwith "UnixNamespace.readlink: a refused capacity was answered above (this is a bug in this library)."
        | ReadLinkCapacityVerdict.ReportNothing ->
            // Nothing is copied, so the destination is never looked at:
            // measured, a null buffer with a zero size answers 0.
            Ok (ReadLinkAnswer.Reported ImmutableArray<byte>.Empty)
        | ReadLinkCapacityVerdict.Admit ->

        // The destination is consulted only here, on the path that actually
        // writes through it. `readlink(2)` runs no up-front address check on
        // either flavour: the target is built in the kernel and handed over with
        // a single `copy_to_user`, so an unusable buffer is discovered at the
        // copy and every earlier refusal wins. Measured against a `PROT_READ`
        // page, both flavours answer EFAULT -- unlike `getcwd`, whose copy is a
        // user-space store on one of them.
        match destination with
        | UserBuffer.Unmapped _ -> Ok (ReadLinkAnswer.Failed UnixError.EFAULT)
        | UserBuffer.Opaque -> Error (ReadLinkRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (ReadLinkRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped ->

        let all = UnixByteString.toBytes (SymlinkTarget.toByteString target)

        // Truncated in *bytes*, not in characters: a symlink target is a byte
        // string, and truncating by character count would write two bytes where
        // the caller allowed one for any non-ASCII target.
        if all.Length <= capacity then
            Ok (ReadLinkAnswer.Reported all)
        else
            Ok (ReadLinkAnswer.Reported (ImmutableArray.CreateRange (Seq.truncate capacity all)))

    /// `readlink(2)`: report what the symbolic link at `path` points at. It is
    /// `readlinkat` from `AT_FDCWD`.
    ///
    /// Changes nothing and returns no system. That is *not* quite what POSIX
    /// says: a successful `readlink` marks the link's access time for update,
    /// and this kernel does not move it. Whether it would move is a property of
    /// the mount rather than of this syscall, and the two flavours disagree —
    /// measured on macOS (lstat, sleep, readlink, lstat) `st_atime` does not
    /// move, while Linux's default `relatime` updates whenever `mtime` or
    /// `ctime` is at or after the old `atime`, and a freshly seeded inode has
    /// all three equal, so the first read there *would* move it. Deciding it
    /// inside one entry point would set mount semantics for every future read
    /// by accident, and would make `readlink` the only syscall obeying them.
    ///
    /// `capacity` is the caller's buffer size. A size that is not positive is
    /// answered as this system's flavour answers it
    /// (`SimulatedUnixPlatform.readlinkCapacity`): EINVAL before the path is
    /// copied in on Linux and for a negative size on Darwin, and zero bytes
    /// from a resolved link on Darwin for a size of zero.
    ///
    /// The link's own mode is consulted as this system's flavour consults it
    /// (`SimulatedUnixPlatform.linkReadRule`): never on Linux, and on Darwin a
    /// caller whose standing selects a triple without the read bit gets
    /// EACCES, ahead of a size of zero and of the buffer. A privileged Darwin
    /// caller is refused an answer (`ReadLinkRefusal.UnmeasuredLinkRead`).
    ///
    /// `path` is the argument's bytes, copied in after that screen and before
    /// anything else: EFAULT if they were unreadable, ENAMETOOLONG if they run
    /// past `PATH_MAX`.
    let readlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (destination : UserBuffer)
        (capacity : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadLinkAnswer, ReadLinkRefusal>
        =
        // The size is screened before the path is copied in, on both flavours:
        // measured (`path-copyin-order.c`), a size Linux refuses is EINVAL with a
        // NULL path, and so is a negative one on Darwin, whose size of zero goes on
        // to copy the path in.
        match SimulatedUnixPlatform.readlinkCapacity system.Machine.UnixPlatform capacity with
        | ReadLinkCapacityVerdict.Refuse error -> Ok (ReadLinkAnswer.Failed error)
        | ReadLinkCapacityVerdict.ReportNothing
        | ReadLinkCapacityVerdict.Admit ->

        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (ReadLinkAnswer.Failed error)
        | Ok path -> readlinkParsed AtDirectory.CurrentDirectory path destination capacity system

    /// `readlinkat(2)`: `readlink` of `path`, starting from `dirfd` if it is
    /// relative; `readlink` is this from `AT_FDCWD`.
    ///
    /// `dirfd` is raw, in this platform's own numbering. The size is screened
    /// as `readlink` screens it, before `path` is copied in, and a relative
    /// path then starts where `dirfd` says, as every `*at` call's does,
    /// except for the empty path on Linux: that names what `dirfd` names
    /// (`SimulatedUnixPlatform.readlinkEmptyPath`), which is EBADF if it
    /// names nothing and ENOENT if it is not a symbolic link. No descriptor
    /// here names a symbolic link, since this library models neither
    /// `O_PATH` nor `O_SYMLINK`.
    let readlinkat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (dirfd : int)
        (path : PathArgumentBytes)
        (destination : UserBuffer)
        (capacity : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadLinkAnswer, ReadLinkRefusal>
        =
        match SimulatedUnixPlatform.readlinkCapacity system.Machine.UnixPlatform capacity with
        | ReadLinkCapacityVerdict.Refuse error -> Ok (ReadLinkAnswer.Failed error)
        | ReadLinkCapacityVerdict.ReportNothing
        | ReadLinkCapacityVerdict.Admit ->

        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (ReadLinkAnswer.Failed error)
        | Ok path ->
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
            readlinkParsed (AtDirectory.decode flavour dirfd) path destination capacity system

    /// Read the next entry of the directory `fd` names, and move its open file
    /// description's position past it: one record of `getdents(2)` (Linux) or
    /// `getdirentries(2)` (Darwin), without either's byte layout.
    ///
    /// The position is the description's, so every descriptor `dup(2)` made
    /// for it reads onward from the same place, and `lseek(fd, 0, SEEK_SET)`
    /// starts again from the first entry.
    ///
    /// The names come in this library's own order, which matches no real
    /// filesystem: see `VirtualFileSystem.nextDirectoryEntry`.
    let readDirectoryEntry<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadDirectoryAnswer * UnixSystem<'Task, 'Handler>, ReadDirectoryRefusal>
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system) with
        | None -> Ok (ReadDirectoryAnswer.Failed UnixError.EBADF, system)
        | Some description ->

        // Not a directory. Measured on every descriptor kind this library
        // models (a regular file opened each way, both ends of a pipe, stream
        // and datagram sockets of both domains, an epoll instance or kqueue):
        //
        //   descriptor                       Linux     Darwin
        //   regular file, readable           ENOTDIR   EINVAL
        //   regular file, O_WRONLY           ENOTDIR   EBADF
        //   pipe, socket, epoll or kqueue    ENOTDIR   ENOTSUP
        //
        // Ahead of the buffer on both (a NULL buffer on a regular file answers
        // the same), which is why a caller need not screen one first.
        let notADirectory (readable : bool) (isVnode : bool) : UnixError =
            match flavour with
            | SimulatedUnixFlavour.Linux -> UnixError.ENOTDIR
            | SimulatedUnixFlavour.Darwin ->
                if not isVnode then UnixError.ENOTSUP
                elif not readable then UnixError.EBADF
                else UnixError.EINVAL

        match description.Target with
        | OpenFileTarget.File _
        // ENOTDIR measured on Linux (`devices-l2.c`, GETDENTS64 rows), the one
        // flavour that holds a device.
        | OpenFileTarget.CharacterDevice _ ->
            let readable = FileAccessMode.permitsRead description.AccessMode
            Ok (ReadDirectoryAnswer.Failed (notADirectory readable true), system)
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Socket _
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _ -> Ok (ReadDirectoryAnswer.Failed (notADirectory true false), system)
        | OpenFileTarget.Directory (inode, position) ->

        match VirtualFileSystem.mountedRootOf inode system.Machine.FileSystem with
        | Some _ -> Error (ReadDirectoryRefusal.DeviceFileSystem inode)
        | None ->

        let withPosition (position : DirectoryPosition) (system : UnixSystem<'Task, 'Handler>) =
            UnixSystemState.withFileDescriptors
                (FileDescriptorRegistry.setDirectoryPosition fd position (UnixSystemState.fileDescriptors system))
                system

        // A directory `rmdir` has removed yields nothing, from any position:
        // measured one call at a time, on a fresh description, after a partial
        // read, after a full one, after a rewind, and at offsets 5, 2^31-1 and
        // 2^62. Linux answers ENOENT and leaves the position alone. Darwin
        // answers 0 and moves the position to its end-of-directory value --
        // except at 2^62, where it answers EAGAIN, so a position `lseek` chose
        // stays refused there.
        let orphaned = VirtualFileSystem.isOrphanedDirectory inode system.Machine.FileSystem

        match flavour, orphaned, position with
        | SimulatedUnixFlavour.Linux, true, _ -> Ok (ReadDirectoryAnswer.Failed UnixError.ENOENT, system)
        | _, _, DirectoryPosition.Unenumerable offset ->
            Error (ReadDirectoryRefusal.UnenumerablePosition (inode, offset))
        | SimulatedUnixFlavour.Darwin, true, DirectoryPosition.Cursor _ ->
            Ok (
                ReadDirectoryAnswer.EndOfDirectory,
                withPosition (DirectoryPosition.Cursor DirectoryCursor.ReturnedDot) system
            )
        | _, false, DirectoryPosition.Cursor cursor ->

        match VirtualFileSystem.nextDirectoryEntry inode cursor system.Machine.FileSystem with
        | None -> Ok (ReadDirectoryAnswer.EndOfDirectory, system)
        | Some (name, target, next) ->

        let kind =
            match VirtualFileSystem.tryGetContent target system.Machine.FileSystem with
            | Some content -> DirectoryEntryKind.ofContent content
            | None ->
                failwith
                    $"UnixNamespace.readDirectoryEntry: the entry \"%O{name}\" names inode %O{target}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        // A name a filesystem is mounted over reports the inode number of the
        // directory the mount covers, not the mounted root's: measured on both
        // flavours, `readdir("/")` reports `dev` as 14 and `stat("/dev")` as 1
        // on Linux (`mountpoint.c`).
        let reported =
            match name, VirtualFileSystem.mountOf target system.Machine.FileSystem with
            | DirectoryStreamName.Entry _, Some mount -> mount.Covered
            | _, _ -> target

        let record : DirectoryRecord =
            {
                Inode = reported
                Name = name
                Kind = kind
            }

        Ok (ReadDirectoryAnswer.Entry record, withPosition (DirectoryPosition.Cursor next) system)

    /// `mkdir`, of a path this kernel has already copied in.
    let internal mkdirParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, PathRefusal>
        =
        let rules = SimulatedUnixPlatform.mkDirRules system.Machine.UnixPlatform

        // `NoFollowFinal` on both flavours: `mkdir` never dereferences the name
        // it is about to bind, so an existing link is EEXIST whether it dangles,
        // points at a file, or points at itself. The trailing separator is the
        // only thing that can reach past it, and only on Darwin — see
        // `MkDirRules.TrailingSeparator`.
        match
            UnixPathResolution.resolvePathFull
                AtDirectory.CurrentDirectory
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                path
                system
        with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error refusal
        | Ok resolution ->

        match
            MkDirRules.verdict
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                system.Process.Credentials
                resolution
                system.Machine.FileSystem
        with
        | MkDirVerdict.Refuse error -> Ok (SyscallAnswer.Failed error, system)
        | MkDirVerdict.Create (directory, name, parentPermissions) ->

        let permissions =
            MkDirRules.createdPermissions rules parentPermissions system.Process.Umask mode

        let now = UnixMachineState.realtime system.Machine

        let owner = newInodeOwner "UnixNamespace.mkdir" directory system

        match VirtualFileSystem.createDirectory directory name permissions owner now system.Machine.FileSystem with
        | Error error ->
            // `createDirectory` refuses a name the directory already holds, and a
            // parent that is not a directory. The walk has just established
            // neither is the case, so either is a broken graph rather than
            // something the caller did.
            failwith
                $"UnixNamespace.mkdir: creating \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and does not hold that name (this is a bug in this library)."
        | Ok (_, filesystem) ->

        Ok (
            SyscallAnswer.Completed 0L,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
        )

    /// `mkdir(2)`: bind a new directory at `path`.
    ///
    /// `mode` is raw, exactly as the caller passed it, so what the created
    /// directory's permissions actually are depends on the umask and, on one
    /// flavour, on the parent's set-group-ID bit. `MkDirRules` holds that.
    ///
    /// Refuses only a path this kernel will not resolve (see `PathRefusal`):
    /// every other outcome is a success or an errno, the rules having been
    /// measured on both flavours.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let mkdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, PathRefusal>
        =
        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> mkdirParsed path mode system

    /// `unlink`, of a path this kernel has already copied in.
    let internal unlinkParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, RemovalRefusal>
        =
        let rules = SimulatedUnixPlatform.unlinkRules system.Machine.UnixPlatform

        // `NoFollowFinal` on both flavours — `unlink` removes the name it was
        // given, never what that name points at. The trailing separator is the
        // only thing that can reach past a final symlink, and only on Darwin;
        // see `UnlinkRules.TrailingSeparator`.
        match
            UnixPathResolution.resolvePathFull
                AtDirectory.CurrentDirectory
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                path
                system
        with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (RemovalRefusal.Path refusal)
        | Ok resolution ->

        // The covered directory, not the mounted root, is what a sticky
        // parent's rule consults; see `RemovalRefusal.MountPoint`.
        let coveredInStickyDirectory =
            match resolution.Target with
            | ResolvedTarget.Entry (directory, _, Some target) when
                (VirtualFileSystem.mountOf target system.Machine.FileSystem).IsSome
                ->
                match VirtualFileSystem.tryGetDirectory directory system.Machine.FileSystem with
                | Some content when PermissionBits.toInt content.Permissions &&& PermissionBits.sticky <> 0 ->
                    Some target
                | Some _
                | None -> None
            | ResolvedTarget.Entry _
            | ResolvedTarget.Directory _ -> None

        match coveredInStickyDirectory with
        | Some mountRoot -> Error (RemovalRefusal.MountPoint mountRoot)
        | None ->

        match
            UnlinkRules.verdict
                (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
                system.Process.Credentials
                resolution
                system.Machine.FileSystem
        with
        | Error refusal -> Error (RemovalRefusal.Sticky refusal)
        | Ok (UnlinkVerdict.Refuse error) -> Ok (SyscallAnswer.Failed error, system)
        | Ok (UnlinkVerdict.Remove (directory, name)) when
            (VirtualFileSystem.mountedRootOf directory system.Machine.FileSystem).IsSome
            ->
            Error (RemovalRefusal.DeviceFileSystem (directory, name))
        | Ok (UnlinkVerdict.Remove (directory, name)) ->

        let now = UnixMachineState.realtime system.Machine

        match VirtualFileSystem.unbind UnbindTargetEffect.LostALink directory name now system.Machine.FileSystem with
        | Error error ->
            // `unbind` refuses a directory it does not hold and a name that
            // directory does not bind. The walk has just established both, so
            // either is a broken graph rather than something the caller did.
            failwith
                $"UnixNamespace.unlink: removing \"%s{DirectoryEntryName.toEscaped name}\" from inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and holds that name (this is a bug in this library)."
        | Ok (target, filesystem) ->

        // The name is gone; whether the *inode* is depends on whether any other
        // name or any open descriptor still holds it. A real `unlink` of a file
        // something has open leaves it readable through that descriptor until the
        // last one closes.
        Ok (
            SyscallAnswer.Completed 0L,
            ObjectLifetime.forgetIfUnheld
                target
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
        )

    /// `unlink(2)`: remove the name `path`, and the inode it named if nothing
    /// else holds it.
    ///
    /// Every outcome is a success or an errno, except where Darwin's sticky
    /// rule has not been measured for this caller, where this kernel will not
    /// resolve the path, and where the name is on the device filesystem; see
    /// `RemovalRefusal`. A refusal changes nothing.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let unlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, RemovalRefusal>
        =
        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> unlinkParsed path system

    /// `rmdir`, of a path this kernel has already copied in.
    let internal rmdirParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, RemovalRefusal>
        =
        let rules = SimulatedUnixPlatform.rmDirRules system.Machine.UnixPlatform

        // `NoFollowFinal` on both flavours. The trailing separator is what
        // reaches past a final symlink, and only on Darwin — which is how
        // `rmdir("ld/")` removes the *link's target* there and is ENOTDIR on
        // Linux. See `RmDirRules.TrailingSeparator`.
        match
            UnixPathResolution.resolvePathFull
                AtDirectory.CurrentDirectory
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                path
                system
        with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (RemovalRefusal.Path refusal)
        | Ok resolution ->

        // The covered directory, not the mounted root, is what a sticky
        // parent's rule consults; see `RemovalRefusal.MountPoint`.
        let coveredInStickyDirectory =
            match resolution.Target with
            | ResolvedTarget.Entry (directory, _, Some target) when
                (VirtualFileSystem.mountOf target system.Machine.FileSystem).IsSome
                ->
                match VirtualFileSystem.tryGetDirectory directory system.Machine.FileSystem with
                | Some content when PermissionBits.toInt content.Permissions &&& PermissionBits.sticky <> 0 ->
                    Some target
                | Some _
                | None -> None
            | ResolvedTarget.Entry _
            | ResolvedTarget.Directory _ -> None

        match coveredInStickyDirectory with
        | Some mountRoot -> Error (RemovalRefusal.MountPoint mountRoot)
        | None ->

        match
            RmDirRules.verdict
                (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
                system.Process.Credentials
                resolution
                system.Machine.FileSystem
        with
        | Error refusal -> Error (RemovalRefusal.Sticky refusal)
        | Ok (RmDirVerdict.Refuse error) -> Ok (SyscallAnswer.Failed error, system)
        | Ok (RmDirVerdict.Remove (directory, name)) when
            (VirtualFileSystem.mountedRootOf directory system.Machine.FileSystem).IsSome
            ->
            Error (RemovalRefusal.DeviceFileSystem (directory, name))
        | Ok (RmDirVerdict.Remove (directory, name)) ->

        let now = UnixMachineState.realtime system.Machine

        match VirtualFileSystem.unbind rules.RemovedDirectoryEffect directory name now system.Machine.FileSystem with
        | Error error ->
            failwith
                $"UnixNamespace.rmdir: removing \"%s{DirectoryEntryName.toEscaped name}\" from inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and holds that name (this is a bug in this library)."
        | Ok (target, filesystem) ->

        // A directory has only ever had the one name, so this was the last — but
        // a descriptor or the current directory may still hold it, and a real
        // `rmdir` leaves such an orphan usable through what holds it.
        // `forgetIfUnheld` also collects the ancestors this directory's ".." was
        // keeping alive.
        Ok (
            SyscallAnswer.Completed 0L,
            ObjectLifetime.forgetIfUnheld
                target
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
        )

    /// `rmdir(2)`: remove the empty directory `path` names.
    ///
    /// Every outcome is a success or an errno, except where Darwin's sticky
    /// rule has not been measured for this caller, where this kernel will not
    /// resolve the path, and where the name is on the device filesystem; see
    /// `RemovalRefusal`. A refusal changes nothing.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let rmdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, RemovalRefusal>
        =
        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> rmdirParsed path system

    let private renameStopped
        (system : UnixSystem<'Task, 'Handler>)
        (error : UnixError)
        : RenameProgress<'Task, 'Handler>
        =
        RenameProgress.Answered (SyscallAnswer.Failed error, system)

    /// Everything `rename(2)` does before it copies its *destination* pathname
    /// in: on Linux the source's pathname and parent walk, on Darwin the whole
    /// source including `RenameRules.sourceScreen`.
    ///
    /// Stops there rather than taking both pathnames because reading one can
    /// fail, and a call that ends in this phase never reads the destination at
    /// all. See `PausedRename`.
    let renameSourcePhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (source : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<RenameProgress<'Task, 'Handler>, RenameRefusal>
        =
        let rules = SimulatedUnixPlatform.renameRules system.Machine.UnixPlatform

        let paused (progress : RenameSourceProgress) =
            RenameProgress.NeedsDestination
                {
                    System = system
                    Rules = rules
                    SourceProgress = progress
                }
            |> Ok

        let stopped (error : UnixError) = renameStopped system error |> Ok

        match UnixPathResolution.copyIn source system with
        | Error error -> stopped error
        | Ok sourcePath ->

        // `NoFollowFinal` for both paths on both flavours — `rename` moves the
        // name it was given, never what that name points at. The trailing
        // separator is the only thing that reaches past a final symlink, and
        // only on Darwin; see `RenameRules.TrailingSeparator`.
        match rules.WalkOrder with
        | RenameWalkOrder.ParentsThenFinals ->
            match
                UnixPathResolution.resolvePathParent
                    AtDirectory.CurrentDirectory
                    SymlinkPolicy.NoFollowFinal
                    rules.TrailingSeparator
                    sourcePath
                    system
            with
            | Error (PathFailure.Errno error) -> stopped error
            | Error (PathFailure.Refused refusal) -> Error (RenameRefusal.Path refusal)
            | Ok parent -> paused (RenameSourceProgress.ParentWalked parent)
        | RenameWalkOrder.SourceThenDestination ->

        match
            UnixPathResolution.resolvePathFull
                AtDirectory.CurrentDirectory
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                sourcePath
                system
        with
        | Error (PathFailure.Errno error) -> stopped error
        | Error (PathFailure.Refused refusal) -> Error (RenameRefusal.Path refusal)
        | Ok sourceResolution ->

        // Darwin's source-side `namei` runs under rename semantics, so two of
        // the refusals the verdict would otherwise make are settled here —
        // before the destination's pathname has been read at all.
        match RenameRules.sourceScreen rules.WalkOrder sourceResolution with
        | Some error -> stopped error
        | None -> paused (RenameSourceProgress.Resolved sourceResolution)

    /// The rest of `rename(2)`, given the destination pathname the kernel has
    /// just reached the point of copying in.
    ///
    /// Every outcome is a success or an errno, except where Darwin's sticky
    /// rule has not been measured for this caller, where this kernel will not
    /// resolve a path, where a path names a mount point in an unmeasured way,
    /// and where the source is on the device filesystem; see `RenameRefusal`.
    /// A refusal changes nothing.
    let renameWithDestination<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : PathArgumentBytes)
        (paused : PausedRename<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, RenameRefusal>
        =
        match box paused with
        | null ->
            failwith
                "UnixNamespace.renameWithDestination: this paused rename is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixNamespace.renameSourcePhase instead."
        | _ ->

        let system = paused.System
        let rules = paused.Rules

        let resolved : Result<Resolution * Resolution, PathFailure> =
            match UnixPathResolution.copyIn destination system with
            | Error error -> Error (PathFailure.Errno error)
            | Ok destinationPath ->

            match paused.SourceProgress with
            | RenameSourceProgress.Resolved sourceResolution ->
                // Darwin: the source is already finished, so the destination is
                // resolved to completion and the verdict judges the pair.
                UnixPathResolution.resolvePathFull
                    AtDirectory.CurrentDirectory
                    SymlinkPolicy.NoFollowFinal
                    rules.TrailingSeparator
                    destinationPath
                    system
                |> Result.map (fun destinationResolution -> sourceResolution, destinationResolution)
            | RenameSourceProgress.ParentWalked sourceParent ->

            // Linux: the destination's parent, then both final lookups.
            match
                UnixPathResolution.resolvePathParent
                    AtDirectory.CurrentDirectory
                    SymlinkPolicy.NoFollowFinal
                    rules.TrailingSeparator
                    destinationPath
                    system
            with
            | Error failure -> Error failure
            | Ok destinationParent ->

            // Linux compares the two parents' mounts before it looks either
            // final name up: measured (`devices.c`, NS rows), a rename between
            // `/tmp` and `/dev` is EXDEV whether or not the source exists and
            // whether or not the caller may write either directory.
            if
                PathWalk.pausedMountedRoot sourceParent
                <> PathWalk.pausedMountedRoot destinationParent
            then
                Error (PathFailure.Errno UnixError.EXDEV)
            else if

                // Then either path naming no final component, "/", "." or "..", is
                // EBUSY, still before either final name is looked up: measured
                // (`renameat-rules.c`, ORDER), `rename("nx", ".")` and
                // `rename(<300 bytes>, "/")` are EBUSY, not the source's ENOENT
                // and ENAMETOOLONG.
                PathWalk.pausedNamesNoFinal sourceParent
                || PathWalk.pausedNamesNoFinal destinationParent
            then
                Error (PathFailure.Errno UnixError.EBUSY)
            else

            // Source before destination, and here the order *is* pinned: the
            // orphan check below sits between the two, so a 300-byte source name
            // is ENAMETOOLONG while a 300-byte destination name under the same
            // orphaned parent is ENOENT. Measured both ways.
            match PathWalk.completeResolution sourceParent with
            | Error failure -> Error failure
            | Ok sourceResolution ->

            // Linux's source screen runs here: after both parents and the
            // source's own final lookup, and before the destination's. Its
            // EBUSY arm has been settled above; its free-name ENOENT beats the
            // orphan check below and the destination's NAME_MAX, which is what
            // makes `rename("nope", <300-byte name>)` ENOENT.
            match RenameRules.sourceScreen rules.WalkOrder sourceResolution with
            | Some error -> Error (PathFailure.Errno error)
            | None ->

            // A destination parent that has lost its own last name — reachable
            // only as an `rmdir`'d current directory — is ENOENT here, *before*
            // the destination's final name is measured. Both verdicts also
            // refuse it, and on Darwin that is where it is caught, after the
            // whole destination has resolved: measured, the same call is
            // ENAMETOOLONG there. So this is the Linux position of a check both
            // flavours make, not a check only Linux makes.
            if PathWalk.pausedParentIsOrphaned destinationParent then
                Error (PathFailure.Errno UnixError.ENOENT)
            else

            PathWalk.completeResolution destinationParent
            |> Result.map (fun destinationResolution -> sourceResolution, destinationResolution)

        match resolved with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (RenameRefusal.Path refusal)
        | Ok (sourceResolution, destinationResolution) ->

        let vfs = system.Machine.FileSystem

        let mountRootNamed (resolution : Resolution) : InodeNumber option =
            match resolution.Target with
            | ResolvedTarget.Entry (_, _, Some target) when (VirtualFileSystem.mountOf target vfs).IsSome -> Some target
            | ResolvedTarget.Entry _
            | ResolvedTarget.Directory _ -> None

        let verdict =
            RenameRules.verdict
                (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                system.Process.Credentials
                sourceResolution
                destinationResolution
                vfs

        // A mount point's own rows. Linux checks the permissions first and the
        // mount after (`rename("/dev", "/devx")` is EACCES at uid 1000 and EBUSY
        // at uid 0), and that pair, a mount point renamed within its own
        // directory, is all that has been measured. It is also all that can be
        // answered: the mount point's own inode is the covered directory, whose
        // owner a sticky parent consults and whose write bit a move to another
        // directory consults, and this kernel holds only the mounted root.
        let sticky (resolution : Resolution) : bool =
            match resolution.Target with
            | ResolvedTarget.Entry (directory, _, _) ->
                match VirtualFileSystem.tryGetDirectory directory vfs with
                | Some content -> PermissionBits.toInt content.Permissions &&& PermissionBits.sticky <> 0
                | None -> false
            | ResolvedTarget.Directory _ -> false

        let verdict =
            match mountRootNamed sourceResolution, mountRootNamed destinationResolution, verdict with
            | None, None, verdict -> Ok verdict
            | Some mountRoot, None, verdict when not (sticky sourceResolution) && not (sticky destinationResolution) ->
                match sourceResolution.Target, destinationResolution.Target, verdict with
                | ResolvedTarget.Entry (sourceDirectory, _, _),
                  ResolvedTarget.Entry (destinationDirectory, _, None),
                  Ok (RenameVerdict.Refuse UnixError.EACCES as refused) when sourceDirectory = destinationDirectory ->
                    Ok (Ok refused)
                | ResolvedTarget.Entry (sourceDirectory, _, _),
                  ResolvedTarget.Entry (destinationDirectory, _, None),
                  Ok (RenameVerdict.Move _) when sourceDirectory = destinationDirectory ->
                    Ok (Ok (RenameVerdict.Refuse UnixError.EBUSY))
                | _ -> Error (RenameRefusal.MountPoint mountRoot)
            | Some mountRoot, _, _
            | None, Some mountRoot, _ -> Error (RenameRefusal.MountPoint mountRoot)

        match verdict with
        | Error refusal -> Error refusal
        | Ok (Error refusal) -> Error (RenameRefusal.Sticky refusal)
        | Ok (Ok (RenameVerdict.Move (sourceDirectory, sourceName, _, _))) when
            (VirtualFileSystem.mountedRootOf sourceDirectory vfs).IsSome
            ->
            Error (RenameRefusal.DeviceFileSystem (sourceDirectory, sourceName))
        | Ok (Ok (RenameVerdict.Refuse error)) -> Ok (SyscallAnswer.Failed error, system)
        // Both paths name one inode: a success that changes nothing at all, not
        // a binding and not a timestamp. Deliberately not routed through
        // `VirtualFileSystem.rename`, which refuses it — the graph primitive
        // would have to invent a no-op stamp to express it.
        | Ok (Ok RenameVerdict.NoOp) -> Ok (SyscallAnswer.Completed 0L, system)
        | Ok (Ok (RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName))) ->

        let now = UnixMachineState.realtime system.Machine

        match
            VirtualFileSystem.rename
                sourceDirectory
                sourceName
                destinationDirectory
                destinationName
                now
                system.Machine.FileSystem
        with
        | Error error ->
            // `rename` refuses a directory it does not hold, a source name that
            // directory does not bind, and the four conditions that would leave
            // a graph no kernel could produce — the two paths naming one inode,
            // a populated destination directory, a destination inside the
            // source's own subtree, and an orphaned destination directory. The
            // verdict owes an errno for every one of those, so reaching here
            // means the verdict let something through rather than that the
            // caller did anything unusual.
            failwith
                $"UnixNamespace.rename: moving \"%s{DirectoryEntryName.toEscaped sourceName}\" from inode %O{sourceDirectory} to \"%s{DirectoryEntryName.toEscaped destinationName}\" in inode %O{destinationDirectory} was refused with %O{error}, but the verdict had just approved it (this is a bug in this library)."
        | Ok (outcome, filesystem) ->

        // A rename is the one syscall that can change the *path* of a directory
        // the process is already in, without changing its inode: moving any
        // ancestor of the current directory moves the current directory with it.
        // Nothing here has to notice. Relative paths resolve from the inode,
        // which has not moved, and `getcwd` derives the path from the graph this
        // rename has just rewritten — so it reports the new path however many
        // levels up the move was, and reports nothing if this rename displaced
        // the current directory itself.
        let moved =
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }

        // The destination name may have been the displaced inode's last, and
        // whether the inode goes with it depends on the descriptor table, which
        // the filesystem cannot see. When the displaced thing was a directory
        // this also collects the ancestors its ".." was keeping alive.
        Ok (
            SyscallAnswer.Completed 0L,
            match outcome.Displaced with
            | None -> moved
            | Some displaced -> ObjectLifetime.forgetIfUnheld displaced moved
        )

    /// `rename(2)` in one call, for a caller holding both pathnames already —
    /// every caller but the one reading them out of a process's memory, where
    /// reading the destination too early is itself observable.
    let rename<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (source : PathArgumentBytes)
        (destination : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, RenameRefusal>
        =
        match renameSourcePhase source system with
        | Error refusal -> Error refusal
        | Ok (RenameProgress.Answered (answer, system)) -> Ok (answer, system)
        | Ok (RenameProgress.NeedsDestination paused) -> renameWithDestination destination paused


    /// `clonefile(source, destination, flags)`, up to the point where the
    /// kernel copies the source pathname in: the flags are screened.
    ///
    /// `flags` is raw: any bit above `CLONE_RESOLVE_BENEATH` (0x10) is EINVAL
    /// before either pathname is read. `CLONE_ACL` (0x4) and
    /// `CLONE_NOOWNERCOPY` (0x2) change nothing for an unprivileged caller of a
    /// filesystem without access control lists, which is what this kernel
    /// models. See `PausedCloneFileSource`.
    let cloneFileFlagsPhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<CloneFileScreen<'Task, 'Handler>, CloneFileRefusal>
        =
        // Measured on Darwin 27.0 at uid 501 (`clonefile-rules.c`, APFS): bits
        // 0 to 4 alone are accepted and every higher bit alone is EINVAL, and
        // EINVAL beats an absent source and an existing destination.
        let cloneNoFollow = 0x1
        let cloneNoFollowAny = 0x8
        let cloneResolveBeneath = 0x10
        let known = 0x1F

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> Error (CloneFileRefusal.UnmodelledFlavour SimulatedUnixFlavour.Linux)
        | SimulatedUnixFlavour.Darwin ->

        match EmulatedMount.fileSystemType system.Machine.Mount with
        | EmulatedFileSystemType.Tmpfs
        | EmulatedFileSystemType.Nfs as fileSystem -> Error (CloneFileRefusal.UnmeasuredFileSystem fileSystem)
        | EmulatedFileSystemType.Apfs ->

        if flags &&& ~~~known <> 0 then
            Ok (CloneFileScreen.Answered (SyscallAnswer.Failed UnixError.EINVAL, system))
        elif flags &&& (cloneNoFollow ||| cloneNoFollowAny ||| cloneResolveBeneath) <> 0 then
            Error (CloneFileRefusal.UnmodelledFlags flags)
        elif Credentials.privilege system.Process.Credentials = CallerPrivilege.Privileged then
            Error CloneFileRefusal.PrivilegedCaller
        else
            Ok (
                CloneFileScreen.NeedsSource
                    {
                        System = system
                    }
            )

    /// The next part of `clonefile(2)`, given the source pathname the kernel
    /// has just reached the point of copying in: it is resolved, following a
    /// final symbolic link, up to the point where the kernel copies the
    /// destination pathname in. See `PausedCloneFile`.
    let cloneFileSourcePhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (source : PathArgumentBytes)
        (paused : PausedCloneFileSource<'Task, 'Handler>)
        : Result<CloneFileProgress<'Task, 'Handler>, CloneFileRefusal>
        =
        // Measured on Darwin 27.0 at uid 501 (`clonefile-rules.c`, APFS): the
        // source is resolved before anything about the destination: an absent
        // source is ENOENT whether the destination exists or its parent is
        // unwritable, and a source in an unsearchable directory is EACCES with
        // an existing destination. "f/" and "f/x" for a regular `f` are
        // ENOTDIR, a link to a file clones the file, and a dangling or cyclic
        // link is ENOENT or ELOOP.
        match box paused with
        | null ->
            failwith
                "UnixNamespace.cloneFileSourcePhase: this paused clonefile is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixNamespace.cloneFileFlagsPhase instead."
        | _ ->

        let system = paused.System

        match UnixPathResolution.copyIn source system with
        | Error error -> Ok (CloneFileProgress.Answered (SyscallAnswer.Failed error, system))
        | Ok sourcePath ->

        match UnixPathResolution.resolvePath AtDirectory.CurrentDirectory SymlinkPolicy.Follow sourcePath system with
        | Error (PathFailure.Errno error) -> Ok (CloneFileProgress.Answered (SyscallAnswer.Failed error, system))
        | Error (PathFailure.Refused refusal) -> Error (CloneFileRefusal.Path refusal)
        | Ok inode ->

        match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
        | Some (InodeContent.Directory _) -> Error (CloneFileRefusal.DirectorySource inode)
        | Some (InodeContent.RegularFile _) ->
            Ok (
                CloneFileProgress.NeedsDestination
                    {
                        System = system
                        Source = inode
                    }
            )
        | Some (InodeContent.CharacterDevice _) ->
            failwith
                $"UnixNamespace.cloneFileSourcePhase: the source resolved to inode %O{inode}, a character device; clonefile(2) is Darwin's, and this kernel holds no device on Darwin (this is a bug in this library)."
        | Some (InodeContent.Symlink _)
        | None ->
            failwith
                $"UnixNamespace.cloneFileSourcePhase: following every link, the source resolved to inode %O{inode}, which is a symbolic link or absent (this is a bug in this library)."

    /// The rest of `clonefile(2)`, given the destination pathname the kernel
    /// has just reached the point of copying in.
    ///
    /// The destination resolves as a creating `open(2)`'s does, following a
    /// final symbolic link, so a dangling link is replaced by a file at its
    /// target. Anything already there is EEXIST; then the source must be
    /// readable (EACCES), the destination's directory writable (EACCES), and
    /// its name one the filesystem admits (EILSEQ).
    ///
    /// The clone is a new regular file holding the source's bytes and its
    /// permission bits less both set-ID bits, owned as any new file in that
    /// directory is, with the source's access, modification and birth times;
    /// its status-change time is now, and the directory's modification and
    /// status-change times move. The source does not change.
    let cloneFileWithDestination<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : PathArgumentBytes)
        (paused : PausedCloneFile<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, CloneFileRefusal>
        =
        // Measured on Darwin 27.0 at uid 501 (`clonefile-rules.c`, APFS). The
        // destination: an existing file, directory ("e" and "e/"), link to a
        // file or a directory, "/", "." and a hard link to the source are all
        // EEXIST, and so is an existing name in an unwritable directory; a
        // dangling link creates its target (and is EEXIST under
        // CLONE_NOFOLLOW); a cyclic link is ELOOP; "new/" and "nx/new" are
        // ENOENT, "f/new" ENOTDIR, a 299-byte name ENAMETOOLONG, an
        // unsearchable parent EACCES, and a removed current directory ENOENT.
        // Each of those beats an unreadable source, which beats an unwritable
        // parent (both EACCES) and a name that is not UTF-8 (EILSEQ, last).
        // The clone: over every mode an owner can give a source, in a
        // directory of its own group, the clone's mode is the source's less
        // both set-ID bits (0 mismatches over 2048 readable modes); in a wheel
        // directory, where S_ISGID cannot be set, likewise. A source the
        // caller cannot read (no owner read bit) is EACCES. uid is the
        // caller's and gid the directory's, either way round; the umask plays
        // no part; atime, mtime and birth time are the source's to the
        // nanosecond, ctime is now; the parent's mtime and ctime move and its
        // atime does not; the source's times do not move.
        match box paused with
        | null ->
            failwith
                "UnixNamespace.cloneFileWithDestination: this paused clonefile is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixNamespace.cloneFileSourcePhase instead."
        | _ ->

        let system = paused.System
        let failed (error : UnixError) = Ok (SyscallAnswer.Failed error, system)
        let rules = SimulatedUnixPlatform.creatingOpenRules system.Machine.UnixPlatform
        let credentials = system.Process.Credentials

        match UnixPathResolution.copyIn destination system with
        | Error error -> failed error
        | Ok destinationPath ->

        match
            UnixPathResolution.resolvePathFull
                AtDirectory.CurrentDirectory
                SymlinkPolicy.Follow
                rules.TrailingSeparator
                destinationPath
                system
        with
        | Error (PathFailure.Errno error) -> failed error
        | Error (PathFailure.Refused refusal) -> Error (CloneFileRefusal.Path refusal)
        | Ok resolution ->

        match resolution.Target with
        | ResolvedTarget.Directory _
        | ResolvedTarget.Entry (_, _, Some _) -> failed UnixError.EEXIST
        | ResolvedTarget.Entry (_, _, None) when resolution.TrailingSeparatorDemanded -> failed UnixError.ENOENT
        | ResolvedTarget.Entry (directory, name, None) ->

        if VirtualFileSystem.isOrphanedDirectory directory system.Machine.FileSystem then
            failed UnixError.ENOENT
        else

        let source =
            match VirtualFileSystem.tryGet paused.Source system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixNamespace.cloneFileWithDestination: the source, inode %O{paused.Source}, is no longer in the filesystem, but nothing ran between resolving it and now (this is a bug in this library)."

        let sourceBits =
            match source.Content with
            | InodeContent.RegularFile (_, bits) -> bits
            | InodeContent.Directory _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ ->
                failwith
                    $"UnixNamespace.cloneFileWithDestination: the source, inode %O{paused.Source}, is not a regular file, but the source phase admitted only a regular file (this is a bug in this library)."

        let sourceStanding = Standing.toward credentials source.Owner

        if PermissionBits.deniedTo sourceStanding AccessRequest.Read sourceBits then
            failed UnixError.EACCES
        else

        let parent =
            match VirtualFileSystem.tryGet directory system.Machine.FileSystem with
            | Some ({
                        Content = InodeContent.Directory parent
                    } as entry) -> entry, parent
            | Some _
            | None ->
                failwith
                    $"UnixNamespace.cloneFileWithDestination: the walk resolved \"%s{DirectoryEntryName.toEscaped name}\" inside inode %O{directory}, which is absent or not a directory (this is a bug in this library)."

        if
            PermissionBits.deniedTo
                (Standing.toward credentials (fst parent).Owner)
                AccessRequest.Write
                (snd parent).Permissions
        then
            failed UnixError.EACCES
        elif
            not (BindableEntryNames.admits (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform) name)
        then
            failed UnixError.EILSEQ
        else

        let raw = PermissionBits.toInt sourceBits

        let measured =
            raw
            &&& (PermissionBits.setUserId ||| PermissionBits.setGroupId ||| PermissionBits.sticky) = 0
            || (sourceStanding.Owns && sourceStanding.InGroup)
            || (sourceStanding.Owns && raw &&& PermissionBits.setGroupId = 0)

        if not measured then
            Error (CloneFileRefusal.UnmeasuredSpecialBits (paused.Source, sourceStanding, sourceBits))
        else

        let permissions =
            PermissionBits.parseOrFail
                "UnixNamespace.cloneFileWithDestination"
                (raw &&& ~~~(PermissionBits.setUserId ||| PermissionBits.setGroupId))

        let now = UnixMachineState.realtime system.Machine

        match
            VirtualFileSystem.cloneFile
                paused.Source
                directory
                name
                permissions
                (newInodeOwner "UnixNamespace.cloneFileWithDestination" directory system)
                now
                system.Machine.FileSystem
        with
        | Error error ->
            failwith
                $"UnixNamespace.cloneFileWithDestination: binding \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and does not hold that name (this is a bug in this library)."
        | Ok (_, filesystem) ->
            Ok (
                SyscallAnswer.Completed 0L,
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
            )

    /// `clonefile(2)` in one call, for a caller holding both pathnames already.
    let cloneFile<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (source : PathArgumentBytes)
        (destination : PathArgumentBytes)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, CloneFileRefusal>
        =
        match cloneFileFlagsPhase flags system with
        | Error refusal -> Error refusal
        | Ok (CloneFileScreen.Answered (answer, system)) -> Ok (answer, system)
        | Ok (CloneFileScreen.NeedsSource paused) ->

        match cloneFileSourcePhase source paused with
        | Error refusal -> Error refusal
        | Ok (CloneFileProgress.Answered (answer, system)) -> Ok (answer, system)
        | Ok (CloneFileProgress.NeedsDestination paused) -> cloneFileWithDestination destination paused

    let private symlinkTargetFrom<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (directory : AtDirectory)
        (target : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : SymlinkProgress<'Task, 'Handler>
        =
        // Measured by `link-symlink.c` (SYMORDER) on Linux 6.18.5 and Darwin
        // 27.0: the target is copied in before anything else, so an unreadable
        // or over-long one wins against every other argument, and Linux's
        // empty target is ENOENT before the link's pathname is read. Darwin
        // goes on, and fails the call at whatever else fails it.
        match UnixPathResolution.copyIn target system with
        | Error error -> SymlinkProgress.Answered (SyscallAnswer.Failed error)
        | Ok target ->

        match SymlinkTarget.ofByteString (UnixPath.toByteString target) with
        | Ok target ->
            SymlinkProgress.NeedsPath
                {
                    System = system
                    Directory = directory
                    Target = Some target
                }
        | Error SymlinkTargetError.Empty ->
            match (SimulatedUnixPlatform.symlinkRules system.Machine.UnixPlatform).EmptyTarget with
            | EmptySymlinkTarget.NoSuchEntry -> SymlinkProgress.Answered (SyscallAnswer.Failed UnixError.ENOENT)
            | EmptySymlinkTarget.Accepted ->
                SymlinkProgress.NeedsPath
                    {
                        System = system
                        Directory = directory
                        Target = None
                    }
        | Error (SymlinkTargetError.Text defect) ->
            failwith
                $"UnixNamespace.symlink: the copied-in target has a text defect (%A{defect}), which only a target parsed from a .NET string can have (this is a bug in this library)."

    /// The first half of `symlinkat(2)`: decode its raw `dirfd` and copy its
    /// `target` in, before the link's own pathname is read.
    ///
    /// A client that reads pathnames out of a caller's memory calls this first,
    /// and reads the link's pathname only on `SymlinkProgress.NeedsPath`.
    let symlinkatTargetPhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (target : PathArgumentBytes)
        (dirfd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SymlinkProgress<'Task, 'Handler>
        =
        let directory =
            AtDirectory.decode (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform) dirfd

        symlinkTargetFrom directory target system

    /// The first half of `symlink(2)`, as `symlinkatTargetPhase` is of
    /// `symlinkat`: `symlink` is `symlinkat` from the current directory.
    let symlinkTargetPhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (target : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : SymlinkProgress<'Task, 'Handler>
        =
        symlinkTargetFrom AtDirectory.CurrentDirectory target system

    /// The second half of `symlink(2)` or `symlinkat(2)`: copy in `path`, the
    /// link's own pathname, and create the link the paused call describes.
    ///
    /// A relative `path` starts where the call's `dirfd` says. A final symbolic
    /// link is never followed, and a trailing separator reaches past the final
    /// name only as the flavour's `SymlinkRules.TrailingSeparator` says;
    /// `SymlinkRules.verdict` decides the rest. The new link holds the target
    /// byte for byte, has the bits `SimulatedUnixPlatform.symlinkCreationPermissions`
    /// gives under the process's umask, and is owned as any new inode in that
    /// directory is (`InodeOwner.ofNewInode`); the directory's modification and
    /// status-change times move.
    ///
    /// Refuses a pathname this kernel will not resolve, and a call that would
    /// create a link with an empty target; see `SymlinkRefusal`.
    let symlinkWithPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (paused : PausedSymlink<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SymlinkRefusal>
        =
        match box paused with
        | null ->
            failwith
                "UnixNamespace.symlinkWithPath: this paused symlink is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixNamespace.symlinkatTargetPhase or symlinkTargetPhase instead."
        | _ -> ()

        let system = paused.System
        let rules = SimulatedUnixPlatform.symlinkRules system.Machine.UnixPlatform

        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path ->

        match
            UnixPathResolution.resolvePathFull
                paused.Directory
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                path
                system
        with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (SymlinkRefusal.Path refusal)
        | Ok resolution ->

        match
            SymlinkRules.verdict
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                system.Process.Credentials
                resolution
                system.Machine.FileSystem
        with
        | SymlinkVerdict.Refuse error -> Ok (SyscallAnswer.Failed error, system)
        | SymlinkVerdict.Create (directory, name) ->

        match paused.Target with
        | None -> Error SymlinkRefusal.EmptyTarget
        | Some target ->

        let permissions =
            SimulatedUnixPlatform.symlinkCreationPermissions system.Machine.UnixPlatform system.Process.Umask

        let owner = newInodeOwner "UnixNamespace.symlink" directory system
        let now = UnixMachineState.realtime system.Machine

        match VirtualFileSystem.createSymlink directory name permissions owner now target system.Machine.FileSystem with
        | Error error ->
            failwith
                $"UnixNamespace.symlink: creating \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and does not hold that name (this is a bug in this library)."
        | Ok (_, filesystem) ->

        Ok (
            SyscallAnswer.Completed 0L,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
        )

    /// `symlinkat(2)`: create, at `path` relative to `dirfd`, a symbolic link
    /// whose target is `target`.
    ///
    /// `dirfd` is raw, in this platform's own numbering. `target` and `path`
    /// are the arguments' bytes, copied in in that order: a client that has yet
    /// to read the second should call `symlinkatTargetPhase` and
    /// `symlinkWithPath` instead. See `symlinkWithPath` for what the call
    /// answers and refuses.
    let symlinkat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (target : PathArgumentBytes)
        (dirfd : int)
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SymlinkRefusal>
        =
        match symlinkatTargetPhase target dirfd system with
        | SymlinkProgress.Answered answer -> Ok (answer, system)
        | SymlinkProgress.NeedsPath paused -> symlinkWithPath path paused

    /// `symlink(2)`: `symlinkat` from the current directory.
    let symlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (target : PathArgumentBytes)
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SymlinkRefusal>
        =
        match symlinkTargetPhase target system with
        | SymlinkProgress.Answered answer -> Ok (answer, system)
        | SymlinkProgress.NeedsPath paused -> symlinkWithPath path paused

    let private linkSourceFrom<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (arguments : LinkArguments)
        (source : AtDirectory)
        (destination : AtDirectory)
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<LinkProgress<'Task, 'Handler>, LinkRefusal>
        =
        let rules = SimulatedUnixPlatform.linkRules system.Machine.UnixPlatform
        let vfs = system.Machine.FileSystem

        // Measured by `link-rules.c` (ORDER, DESTORDER, EMPTY) and `at-dirfd.c`
        // (ORDER2) on Linux 6.18.5 and Darwin 27.0: the source is copied in and
        // resolved completely, its final lookup included, before the new
        // pathname is copied in, on both flavours.
        match UnixPathResolution.copyIn path system with
        | Error error -> Ok (LinkProgress.Answered (SyscallAnswer.Failed error))
        | Ok path ->

        let namesStartingPoint =
            UnixPath.isEmpty path
            && arguments.EmptyPath = EmptyPathMeaning.NamesStartingPoint

        // Measured by `link-empty-path.c` (CRED) on Linux 6.18.5: with
        // `AT_EMPTY_PATH`, a path relative to a descriptor, empty or not, is
        // checked against the descriptor's open-time credentials, unless the
        // caller is privileged. `AT_FDCWD` and a rooted path are not.
        let relativeToDescriptor =
            match source with
            | AtDirectory.Descriptor _ -> not (UnixPath.isRooted path)
            | AtDirectory.CurrentDirectory -> false

        let privileged =
            Credentials.privilege system.Process.Credentials = CallerPrivilege.Privileged

        if
            arguments.EmptyPath = EmptyPathMeaning.NamesStartingPoint
            && relativeToDescriptor
            && not privileged
        then
            Error LinkRefusal.OpenTimeCredentials
        else

        let resolved : Result<Result<InodeNumber, UnixError>, LinkRefusal> =
            if namesStartingPoint then
                match UnixPathResolution.startOf source arguments.EmptyPath path system with
                | Error (PathFailure.Errno error) -> Ok (Error error)
                | Error (PathFailure.Refused refusal) -> Error (LinkRefusal.Path refusal)
                | Ok (PathStart.StartingObject inode) ->
                    // An unlinked regular file goes on: `LinkRules.verdict`
                    // answers it once the destination has been looked at.
                    match VirtualFileSystem.tryGetContent inode vfs with
                    | Some (InodeContent.Directory _) when VirtualFileSystem.isOrphanedDirectory inode vfs ->
                        Error (LinkRefusal.NamelessSource inode)
                    | Some _ -> Ok (Ok inode)
                    | None ->
                        failwith
                            $"UnixNamespace.linkat: the descriptor names inode %O{inode}, which the filesystem does not contain (this is a bug in this library)."
                | Ok (PathStart.Walk _) ->
                    failwith
                        "UnixNamespace.linkat: an empty path naming its starting point walked instead (this is a bug in this library)."
            else
                match
                    UnixPathResolution.resolvePathFull
                        source
                        arguments.Source
                        TrailingSeparatorPolicy.Demand
                        path
                        system
                with
                | Error (PathFailure.Errno error) -> Ok (Error error)
                | Error (PathFailure.Refused refusal) -> Error (LinkRefusal.Path refusal)
                | Ok resolution -> Ok (PathWalk.existingOf resolution.Target)

        match resolved with
        | Error refusal -> Error refusal
        | Ok (Error error) -> Ok (LinkProgress.Answered (SyscallAnswer.Failed error))
        | Ok (Ok inode) ->

        match UnixMachineState.fileSystemTypeOf inode system.Machine with
        | EmulatedFileSystemType.Nfs -> Error (LinkRefusal.UnmeasuredFileSystem EmulatedFileSystemType.Nfs)
        | EmulatedFileSystemType.Tmpfs
        | EmulatedFileSystemType.Apfs ->

        let isDirectory =
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory _) -> true
            | Some _
            | None -> false

        match rules.DirectorySource with
        | DirectorySourceRefusal.BeforeDestination when isDirectory ->
            Ok (LinkProgress.Answered (SyscallAnswer.Failed UnixError.EPERM))
        | DirectorySourceRefusal.BeforeDestination
        | DirectorySourceRefusal.Last ->
            Ok (
                LinkProgress.NeedsDestination
                    {
                        System = system
                        Rules = rules
                        Source = inode
                        Destination = destination
                    }
            )

    /// The first half of `linkat(2)`: screen its raw flag word, decode its raw
    /// `dirfd`s, and copy in and resolve the source, before the new pathname is
    /// read. A client that reads pathnames out of a caller's memory reads the
    /// new one only on `LinkProgress.NeedsDestination`.
    let linkatSourcePhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (olddirfd : int)
        (oldpath : PathArgumentBytes)
        (newdirfd : int)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<LinkProgress<'Task, 'Handler>, LinkRefusal>
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        match LinkRules.screen flavour flags with
        | LinkScreen.Failed error -> Ok (LinkProgress.Answered (SyscallAnswer.Failed error))
        | LinkScreen.Unmodelled flags -> Error (LinkRefusal.UnmodelledFlags flags)
        | LinkScreen.Screened arguments ->
            linkSourceFrom
                arguments
                (AtDirectory.decode flavour olddirfd)
                (AtDirectory.decode flavour newdirfd)
                oldpath
                system

    /// The first half of `link(2)`, as `linkatSourcePhase` is of `linkat`:
    /// `link` is `linkat` from the current directory on both sides, following
    /// a final symbolic link in the source on Darwin and not on Linux
    /// (`LinkRules.PlainLinkSource`).
    let linkSourcePhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (oldpath : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<LinkProgress<'Task, 'Handler>, LinkRefusal>
        =
        let rules = SimulatedUnixPlatform.linkRules system.Machine.UnixPlatform

        linkSourceFrom
            {
                Source = rules.PlainLinkSource
                EmptyPath = EmptyPathMeaning.Walked
            }
            AtDirectory.CurrentDirectory
            AtDirectory.CurrentDirectory
            oldpath
            system

    /// The second half of `link(2)` or `linkat(2)`: copy in `newpath`, and give
    /// the paused call's source that name.
    ///
    /// A relative `newpath` starts where the call's new `dirfd` says; a final
    /// symbolic link is never followed, and a trailing separator reaches past
    /// the final name only as `LinkRules.TrailingSeparator` says.
    /// `LinkRules.verdict` decides the rest. On success the source gains a
    /// name, its status-change time moves, and so do the new name's
    /// directory's modification and status-change times.
    let linkWithDestination<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (newpath : PathArgumentBytes)
        (paused : PausedLink<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, LinkRefusal>
        =
        match box paused with
        | null ->
            failwith
                "UnixNamespace.linkWithDestination: this paused link is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixNamespace.linkatSourcePhase or linkSourcePhase instead."
        | _ -> ()

        let system = paused.System

        match UnixPathResolution.copyIn newpath system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok newpath ->

        match
            UnixPathResolution.resolvePathFull
                paused.Destination
                SymlinkPolicy.NoFollowFinal
                paused.Rules.TrailingSeparator
                newpath
                system
        with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (LinkRefusal.Path refusal)
        | Ok resolution ->

        match
            LinkRules.verdict
                paused.Rules
                system.Machine.ProtectedFiles.Hardlinks
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                system.Process.Credentials
                paused.Source
                resolution
                system.Machine.FileSystem
        with
        | LinkVerdict.Refuse error -> Ok (SyscallAnswer.Failed error, system)
        | LinkVerdict.Create (directory, name) ->

        let now = UnixMachineState.realtime system.Machine

        match VirtualFileSystem.hardLink directory name paused.Source now system.Machine.FileSystem with
        | Error error ->
            failwith
                $"UnixNamespace.link: binding inode %O{paused.Source} as \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory} was refused with %O{error}, but the verdict had just established that it may be (this is a bug in this library)."
        | Ok filesystem ->

        Ok (
            SyscallAnswer.Completed 0L,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
        )

    /// `linkat(2)`: give the inode `oldpath` names, relative to `olddirfd`, the
    /// further name `newpath`, relative to `newdirfd`.
    ///
    /// `olddirfd`, `newdirfd` and `flags` are raw, in this platform's own
    /// numbering; `LinkRules.screen` says which flags each flavour accepts.
    /// The pathnames are the arguments' bytes, copied in source first: a client
    /// that has yet to read the second should call `linkatSourcePhase` and
    /// `linkWithDestination` instead.
    ///
    /// Refuses what `LinkRefusal` lists.
    let linkat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (olddirfd : int)
        (oldpath : PathArgumentBytes)
        (newdirfd : int)
        (newpath : PathArgumentBytes)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, LinkRefusal>
        =
        match linkatSourcePhase olddirfd oldpath newdirfd flags system with
        | Error refusal -> Error refusal
        | Ok (LinkProgress.Answered answer) -> Ok (answer, system)
        | Ok (LinkProgress.NeedsDestination paused) -> linkWithDestination newpath paused

    /// `link(2)`: see `linkSourcePhase` for how it is `linkat`.
    let link<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (oldpath : PathArgumentBytes)
        (newpath : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, LinkRefusal>
        =
        match linkSourcePhase oldpath system with
        | Error refusal -> Error refusal
        | Ok (LinkProgress.Answered answer) -> Ok (answer, system)
        | Ok (LinkProgress.NeedsDestination paused) -> linkWithDestination newpath paused
