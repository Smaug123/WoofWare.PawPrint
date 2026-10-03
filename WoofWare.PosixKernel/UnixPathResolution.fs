namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// <summary>
/// What <c>stat(2)</c> and its siblings report about one inode.
/// </summary>
/// <remarks>
/// Stored as structured information, rather than the bytes of a <c>struct stat</c>.
/// This is because we don't want to be opinionated about the client's ABI.
///
/// Some fields are not yet modelled and so don't appear in here yet (e.g. no <c>st_blksize</c>).
/// </remarks>
type FileStatus =
    {
        /// `st_mode`: the file-type band and the permission band together, as
        /// `stat(2)` reports them and in the numbering `S_IFMT` uses. Composed
        /// here rather than by the client, so that the two bands are assembled
        /// in exactly one place.
        Mode : int
        /// <summary>
        /// <c>st_uid</c>.
        /// </summary>
        UserId : UserId
        /// <summary>
        /// <c>st_gid</c>.
        /// </summary>
        GroupId : GroupId
        /// <summary>
        /// <c>st_size</c>.
        /// </summary>
        /// <remarks>
        /// For a symbolic link this is the target's length in bytes, as you'd get
        /// from <c>readlink(2)</c>.
        /// </remarks>
        Size : int64
        /// <summary>
        /// <c>st_atim</c>.
        /// </summary>
        AccessTime : UnixTimestamp
        /// <summary>
        /// <c>st_mtim</c>.
        /// </summary>
        ModificationTime : UnixTimestamp
        /// <summary>
        /// <c>st_ctim</c>.
        /// </summary>
        StatusChangeTime : UnixTimestamp
        /// `st_birthtim`, or `None` on a flavour whose `stat(2)` has no such
        /// field.
        ///
        /// The inode knows when it was born either way; this says whether the
        /// *platform being simulated* would tell a process. `None` is what a
        /// Linux process sees, and a client with a birth-time field fills it
        /// with its own default when the kernel did not supply one.
        BirthTime : UnixTimestamp option
        /// <summary>
        /// <c>st_dev</c>.
        /// </summary>
        DeviceId : int64
        /// <summary>
        /// <c>st_ino</c>.
        /// </summary>
        Inode : InodeNumber
        /// `st_nlink`.
        ///
        /// For a regular file or a symbolic link this is how many names it
        /// has, and 0 once the last has gone while a descriptor still holds
        /// it. A directory's is a rule of the filesystem it is on, which
        /// `EmulatedFileSystemType.directoryLinkCount` states, and a pipe's is
        /// the flavour's. Darwin reports no more than 65535; see
        /// `SimulatedUnixPlatform.linkCountCeiling`.
        LinkCount : int64
        /// `st_rdev`: which device a character or block special file stands
        /// for, in the flavour's `dev_t` encoding, and 0 for every other kind
        /// of file.
        SpecialFileDevice : int64
        /// `st_flags`, the BSD file flags `chflags(2)` sets, or `None` on a
        /// flavour whose `stat(2)` has no such field.
        ///
        /// 0 for everything on a flavour that has the field: a new file has
        /// none set, and this kernel has no `chflags(2)` to set one.
        FileFlags : uint32 option
    }

/// Why this kernel refused to report a `struct stat` for a path.
[<RequireQualifiedAccess>]
type StatRefusal =
    /// The path names a directory on an NFS mount. Its `st_size` and its
    /// `st_nlink` are what the NFS server's GETATTR reports, and nothing in
    /// this machine determines them.
    | NfsDirectorySize of inode : InodeNumber
    /// The path names the root of the device filesystem. Its `st_size` and
    /// `st_nlink` count every node and subdirectory a real one holds, and this
    /// kernel's holds only the nodes of the devices it has drivers for.
    | DeviceFileSystemRoot of inode : InodeNumber
    /// This kernel will not resolve the path.
    | Path of PathRefusal

[<RequireQualifiedAccess>]
module StatRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic.
    let describe (refusal : StatRefusal) : string =
        match refusal with
        | StatRefusal.NfsDirectorySize inode ->
            $"inode %O{inode} is a directory on an NFS mount. Its st_size and st_nlink are what the NFS server's GETATTR reports, which nothing in this machine determines, so this kernel will not state them."
        | StatRefusal.DeviceFileSystemRoot inode ->
            $"inode %O{inode} is the root of the device filesystem. Its st_size and st_nlink count every node and subdirectory a real one holds, and this kernel's holds only the nodes of the devices it has drivers for, so it will not state them."
        | StatRefusal.Path refusal -> PathRefusal.describe refusal

/// <summary>
/// Why this kernel refused to report a <c>struct stat</c> for a descriptor.
/// </summary>
/// <remarks>
/// WoofWare.PosixKernel doesn't have an inode for some specific types of file descriptor, so can't provide
/// an answer to <c>fstat</c>.
/// </remarks>
[<RequireQualifiedAccess>]
type FStatRefusal =
    /// <summary>
    /// An end of a pipe that the process was launched with, rather than one it made.
    /// </summary>
    /// <remarks>
    /// The pipe's owner and timestamps are those of whoever launched the process, which the launch table
    /// <c>UnixSystem.initial</c> took does not state.
    /// </remarks>
    | LaunchedPipe of pipe : PipeId
    /// <summary>
    /// An event queue: an anonymous kernel object.
    /// </summary>
    | EventQueue
    /// <summary>
    /// A socket, which has an identity in WoofWare.PosixKernel, but not an inode-shaped one.
    /// </summary>
    | Socket of socket : SocketId
    /// <summary>
    /// A directory on an NFS mount.
    /// </summary>
    /// <remarks>
    /// Its <c>st_size</c> and its <c>st_nlink</c> are what the NFS server's GETATTR reports, and nothing in
    /// this machine determines them.
    /// </remarks>
    | NfsDirectorySize of inode : InodeNumber
    /// <summary>
    /// The root of the device filesystem.
    /// </summary>
    /// <remarks>
    /// Its <c>st_size</c> and <c>st_nlink</c> count every node and subdirectory a real one holds, and this
    /// kernel's holds only the nodes of the devices it has drivers for.
    /// </remarks>
    | DeviceFileSystemRoot of inode : InodeNumber

[<RequireQualifiedAccess>]
module FStatRefusal =
    /// <summary>Human-readable description.</summary>
    let describe (refusal : FStatRefusal) : string =
        match refusal with
        | FStatRefusal.LaunchedPipe pipe ->
            $"the descriptor is an end of pipe %O{pipe}, which the process was launched with rather than one it made. A real kernel answers here -- S_IFIFO, the launcher's user and group, the time the launcher made the pipe -- and the launch table states none of the launcher's half, so it would be invented, with nothing able to say the invention was wrong."
        | FStatRefusal.EventQueue ->
            "the descriptor is an event queue, an anonymous kernel object this kernel holds no inode for. Measured, the two flavours share not one field, and Linux's identity fields are facts about the machine that produced them rather than portable ones: Linux gives `st_mode` 0600 (permission bits and *no* file-type bits), `st_nlink` 1, `st_blksize` 4096, and a real anon-inode `st_dev`/`st_ino`; Darwin gives `st_mode` S_IFIFO (no permission bits), `st_nlink` 0, `st_blksize` 32, and zero for both identity fields."
        | FStatRefusal.Socket socket ->
            $"the descriptor is socket %O{socket}, for which this kernel holds no inode — a `SocketId` is a contention key rather than an inode number. Measured, only Linux gives a socket an inode at all (`st_dev` 8 and a distinct `st_ino` per socket, on `sockfs`), a Darwin AF_INET socket reporting 0 for both; and the rest would be invented either way — `st_mode` is S_IFSOCK|0777 on Linux against S_IFSOCK|0666 on Darwin, `st_nlink` 1 against 0, and Darwin's `st_blksize` varies with the socket itself (131072 for TCP, 9216 for UDP, 8192 for a Unix-domain socket)."
        | FStatRefusal.NfsDirectorySize inode -> StatRefusal.describe (StatRefusal.NfsDirectorySize inode)
        | FStatRefusal.DeviceFileSystemRoot inode -> StatRefusal.describe (StatRefusal.DeviceFileSystemRoot inode)

/// Why this kernel will not answer a `chmod(2)`.
[<RequireQualifiedAccess>]
type ChModRefusal =
    /// What the mode change would do to the inode at `inode` has not been
    /// measured for this caller.
    | UnmeasuredModeChange of inode : InodeNumber * refusal : ModeChangeRefusal
    /// This kernel will not resolve the path.
    | Path of PathRefusal

[<RequireQualifiedAccess>]
module ChModRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : ChModRefusal) : string =
        match refusal with
        | ChModRefusal.UnmeasuredModeChange (inode, refusal) ->
            $"changing the mode of inode %O{inode}: %s{ModeChangeRefusal.describe refusal}"
        | ChModRefusal.Path refusal -> PathRefusal.describe refusal

/// Why this kernel will not answer an `fchmod(2)`.
[<RequireQualifiedAccess>]
type FChModRefusal =
    /// What the mode change would do to the inode at `inode` has not been
    /// measured for this caller.
    | UnmeasuredModeChange of inode : InodeNumber * refusal : ModeChangeRefusal
    /// The descriptor is an end of a pipe the process was launched with, on a
    /// flavour where `fchmod` changes a pipe's mode if the caller may. Whether
    /// it may depends on the pipe's owner, which is the launcher's, and the
    /// launch table does not state it.
    | LaunchedPipe of pipe : PipeId
    /// The descriptor is a socket, on a flavour whose sockets have a mode that
    /// `fchmod` changes. This kernel holds no such mode.
    | Socket of socket : SocketId

[<RequireQualifiedAccess>]
module FChModRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which descriptor.
    let describe (refusal : FChModRefusal) : string =
        match refusal with
        | FChModRefusal.UnmeasuredModeChange (inode, refusal) ->
            $"changing the mode of inode %O{inode}: %s{ModeChangeRefusal.describe refusal}"
        | FChModRefusal.LaunchedPipe pipe ->
            $"the descriptor is an end of pipe %O{pipe}, which the process was launched with rather than one it made. Measured on Linux, fchmod on a pipe end changes the mode fstat then reports (0600 to 02750, say) if the caller owns the pipe or is privileged, and answers EPERM otherwise; this pipe's owner is whoever launched the process, which the launch table does not state, so either answer would be a guess."
        | FChModRefusal.Socket socket ->
            $"the descriptor is socket %O{socket}. Measured on Linux, fchmod on a socket of every domain and kind succeeds and changes the mode fstat then reports (0777 to 0600, say); this kernel holds no inode for a socket and so no mode to change, and answering success while changing nothing would be a lie the moment one is modelled."

/// Why this kernel will not answer a `chown(2)` or `lchown(2)`.
[<RequireQualifiedAccess>]
type ChOwnRefusal =
    /// What the owner change would do to the inode at `inode` has not been
    /// measured for this caller.
    | UnmeasuredOwnerChange of inode : InodeNumber * refusal : OwnerChangeRefusal
    /// This kernel will not resolve the path.
    | Path of PathRefusal

[<RequireQualifiedAccess>]
module ChOwnRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : ChOwnRefusal) : string =
        match refusal with
        | ChOwnRefusal.UnmeasuredOwnerChange (inode, refusal) ->
            $"changing the owner of inode %O{inode}: %s{OwnerChangeRefusal.describe refusal}"
        | ChOwnRefusal.Path refusal -> PathRefusal.describe refusal

/// Why this kernel will not answer an `fchown(2)`.
[<RequireQualifiedAccess>]
type FChOwnRefusal =
    /// What the owner change would do to the inode at `inode` has not been
    /// measured for this caller.
    | UnmeasuredOwnerChange of inode : InodeNumber * refusal : OwnerChangeRefusal
    /// The descriptor is an end of a pipe the process was launched with, on a
    /// flavour where `fchown` changes a pipe's owner if the caller may. Whether
    /// it may depends on the pipe's owner, which is the launcher's, and the
    /// launch table does not state it.
    | LaunchedPipe of pipe : PipeId
    /// The descriptor is a socket, on a flavour whose sockets have an owner
    /// that `fchown` changes. This kernel holds no such owner.
    | Socket of socket : SocketId

[<RequireQualifiedAccess>]
module FChOwnRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which descriptor.
    let describe (refusal : FChOwnRefusal) : string =
        match refusal with
        | FChOwnRefusal.UnmeasuredOwnerChange (inode, refusal) ->
            $"changing the owner of inode %O{inode}: %s{OwnerChangeRefusal.describe refusal}"
        | FChOwnRefusal.LaunchedPipe pipe ->
            $"the descriptor is an end of pipe %O{pipe}, which the process was launched with rather than one it made. Measured on Linux, fchown on a pipe end changes the owner fstat then reports by the same rule as a file's, which depends on who owns the pipe; this pipe's owner is whoever launched the process, which the launch table does not state, so any answer would be a guess."
        | FChOwnRefusal.Socket socket ->
            $"the descriptor is socket %O{socket}. Measured on Linux, fchown on a socket of every domain and kind changes the owner fstat then reports, by the same rule as a file's (the owner may give it one of its groups, a non-owner naming a user is EPERM, root may do anything); this kernel holds no owner for a socket, so it has nothing to judge the call against or to change."

/// Why this kernel will not answer a `futimens(2)`.
[<RequireQualifiedAccess>]
type FUTimensRefusal =
    /// This kernel is Darwin-flavoured. Darwin's `futimens` normalises a
    /// nanosecond field outside `[0, 1e9)` where Linux refuses it, and moves a
    /// file's birth time back to an earlier modification time, but not to one
    /// before the epoch; this kernel models neither.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// The descriptor names something other than a file or directory: a pipe,
    /// a socket or an event queue.
    | UnmodelledObject of object : OpenFileObject

[<RequireQualifiedAccess>]
module FUTimensRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which descriptor.
    let describe (refusal : FUTimensRefusal) : string =
        match refusal with
        | FUTimensRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and only Linux's futimens is modelled. Measured on Darwin 27.0, futimens accepts a nanosecond field of 1e9 or -1 (carrying it into the seconds, or reading -1 as UTIME_NOW) where Linux answers EINVAL, answers EACCES rather than EPERM for a file the caller does not own, and moves the file's birth time back to an earlier modification time unless that time is before the epoch."
        | FUTimensRefusal.UnmodelledObject object ->
            $"the descriptor names %O{object}, which is neither a file nor a directory. Measured on Linux 6.18.5 as root, futimens sets the times fstat reports for either end of a pipe and for a socket, and answers EOPNOTSUPP for an epoll instance; this kernel holds no times for a socket, and what a caller who does not own the pipe or the epoll inode gets is unmeasured."

/// <summary>
/// What <c>fstat(2)</c> reported.
/// </summary>
[<RequireQualifiedAccess>]
type FileStatusAnswer =
    /// <summary>
    /// The status of the inode named by the file descriptor.
    /// </summary>
    | Reported of status : FileStatus
    /// <summary>
    /// The entry point returns -1, stores `error` wherever its libc keeps errno,
    /// and leaves the caller's output struct untouched.
    /// </summary>
    | Failed of error : UnixError

/// What `getcwd(3)` does to the caller's buffer and what it returns.
///
/// The success value of a real `getcwd` is the caller's own buffer pointer,
/// which this library never possesses; the client composes that from the
/// pointer it already holds.
[<RequireQualifiedAccess>]
type GetCwdAnswer =
    /// Place these bytes in the caller's buffer. They are NUL-terminated
    /// already, because terminating is `getcwd`'s job rather than its caller's,
    /// and they fit: the length comparison that produces ERANGE has already been
    /// made against this exact sequence.
    | Reported of path : ImmutableArray<byte>
    /// The call returns NULL and the caller stores `error` wherever its libc
    /// keeps errno.
    ///
    /// Says nothing about the destination's *contents*. Every Linux failure
    /// path leaves it untouched, and Darwin's do not: see
    /// `GetCwdOrphanAnswer.ShortestPathFirst` for what was measured there and
    /// why this library does not reproduce it.
    | Failed of error : UnixError

/// Why this kernel will not answer a `getcwd`.
[<RequireQualifiedAccess>]
type GetCwdRefusal =
    /// The buffer has no answer at the step this `getcwd` reached — which is
    /// always the copy, never a screen: measured, neither flavour checks the
    /// destination's address before comparing sizes, so `getcwd(high, 1)` is
    /// ERANGE rather than EFAULT on both.
    | Buffer of BufferRefusal
    /// The destination names no writable storage, on a platform whose `getcwd`
    /// stores from user space. That is a fatal signal rather than an errno, and
    /// this kernel has no way to deliver one; answering EFAULT would turn a
    /// crash into a plausible wrong answer.
    ///
    /// Reported for every capacity of 2 or more, including calls that would
    /// have failed for another reason — because such a flavour may store before
    /// it decides which failure to report, and whether it has depends on a libc
    /// route this library cannot observe. It therefore over-refuses rather than
    /// answer some cells and die in others; the measurements are in
    /// `docs/divergences.md`.
    | FatalToTheProcess

[<RequireQualifiedAccess>]
module GetCwdRefusal =
    /// What this kernel knows about why it cannot answer a `getcwd`. The client
    /// supplies its own half — which entry point, and what the destination
    /// actually was, neither of which this library ever saw.
    let describe (refusal : GetCwdRefusal) : string =
        match refusal with
        | GetCwdRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | GetCwdRefusal.FatalToTheProcess ->
            "the destination names no storage this caller can write, and this platform's `getcwd(3)` assembles the path with stores executed in the caller's own context rather than copying from the kernel. Measured against a `PROT_READ` page: Darwin dies on a signal (SIGSEGV unmapped, SIGBUS read-only) where Linux answers EFAULT. It can die that way on calls that would otherwise report ERANGE or ENOENT, because it stores before it decides -- so this is reported for any capacity of two or more, which over-refuses the cells where the real call answers without storing. A dead process is not an errno, and guessing which cell this is would answer one for a call that really dies."

/// An `access(2)` or `faccessat(2)` whose mode and flag words this kernel has
/// screened and accepted, paused at the point where it copies its path in.
/// Obtain one from `UnixPathResolution.accessScreenPhase` or
/// `UnixPathResolution.faccessatScreenPhase`, and finish it with
/// `UnixPathResolution.accessWithPath`.
type PausedAccess<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    private
        {
            System : UnixSystem<'Task, 'Handler>
            Directory : AtDirectory
            Arguments : AccessArguments
        }

/// What screening an `access(2)` or `faccessat(2)`'s mode and flag words found:
/// either the call is over without its path having been read at all, or the
/// kernel has reached the point where it copies the path in.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type AccessProgress<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// Finished, and changing nothing. The path was never read, and must not
    /// be: Linux answers a bad mode or flag word EINVAL whatever the path
    /// pointer is.
    | Answered of answer : SyscallAnswer
    /// The kernel is at the path's copy-in. Hand its bytes to
    /// `UnixPathResolution.accessWithPath`.
    | NeedsPath of paused : PausedAccess<'Task, 'Handler>

[<RequireQualifiedAccess>]
module UnixPathResolution =

    /// `getname()`: copy a syscall's path argument in, as this system's kernel
    /// does, before anything looks at what it says. Every path-taking syscall
    /// here calls this at the point its kernel copies the pathname in.
    let internal copyIn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (argument : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixPath, UnixError>
        =
        match PathArgument.copyIn (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform) argument with
        | PathArgument.Failed error -> Error error
        | PathArgument.Parsed path -> Ok path

    /// <summary>
    /// The full result of walking <c>path</c>.
    /// </summary>
    /// <remarks>
    /// Callers that only want the resulting inode should use <c>resolvePath</c> instead.
    /// <c>path</c> is one this kernel has copied in, so within <c>PATH_MAX</c>; see
    /// <c>copyIn</c>.
    /// This function is for callers that must distinguish
    /// "the name exists" from "the name is free in a directory that exists", such as
    /// <c>rename</c>, <c>link</c>, and <c>open</c> with <c>O_CREAT</c>.
    ///
    /// Relative paths start at the process's current directory <i>inode</i>, not at a
    /// re-walk of its path.
    /// </remarks>
    let internal resolvePathFull<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Resolution, PathFailure>
        =
        // The held inode, not a re-walk of the recorded current directory: a real
        // process reaches its current directory through a reference it already
        // holds, so no component of that directory's own path is looked up here
        // and none of their permission bits are consulted. Measured on both
        // kernels — with the cwd at `outer/inner` and `outer` unsearchable, a
        // relative `lstat("target")` succeeds while `lstat("../inner/target")` is
        // EACCES.
        //
        // The cwd *itself* is not exempt: the walk starts there and checks its
        // search bit the moment it consumes a component, which is what makes
        // `lstat("target")` EACCES when the cwd itself is unsearchable — also
        // measured on both.
        //
        // Passed unconditionally, a rooted path included: `resolveFull` asks
        // `isRooted` itself and starts at the root regardless of what it is
        // handed, so a caller that branched here would be computing a value the
        // walk discards.
        PathWalk.resolveFull
            (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
            system.Process.Credentials
            system.Machine.ProtectedFiles.Symlinks
            system.Process.CurrentDirectoryInode
            policy
            trailingSeparatorPolicy
            path
            system.Machine.FileSystem

    /// <summary>
    /// <c>resolvePathFull</c>, but with resolution paused at the directory holding the final name.
    /// </summary>
    /// <remarks>
    /// This is here for <c>rename</c> under Linux.
    /// Linux's walk order resolves <i>both</i> paths' parents before looking up either final component.
    ///
    /// Finish such a walk with <c>PathWalk.completeResolution</c>.
    /// </remarks>
    let internal resolvePathParent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PausedResolution, PathFailure>
        =
        PathWalk.resolveParent
            (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
            system.Process.Credentials
            system.Machine.ProtectedFiles.Symlinks
            system.Process.CurrentDirectoryInode
            policy
            trailingSeparatorPolicy
            path
            system.Machine.FileSystem

    /// <summary>
    /// The inode a path names, or the errno the lookup would return to the caller.
    /// </summary>
    /// <remarks>
    /// This is the call path for every non-creating caller.
    /// </remarks>
    let internal resolvePath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<InodeNumber, PathFailure>
        =
        resolvePathFull policy TrailingSeparatorPolicy.Demand path system
        |> Result.bind (fun resolution -> PathWalk.existingOf resolution.Target |> Result.mapError PathFailure.Errno)

    /// `count` as the platform's `stat(2)` would report it in `st_nlink`.
    let private reportedLinkCount (platform : SimulatedUnixPlatform) (count : int64) : int64 =
        match SimulatedUnixPlatform.linkCountCeiling platform with
        | Some ceiling -> min ceiling count
        | None -> count

    /// `st_flags` for anything this kernel holds, which nothing can have set.
    /// Measured 2026-10-02 by `stat-fields.c` on Darwin 27.0: 0 for a fresh
    /// regular file (a dot-file too), directory, symbolic link, FIFO and pipe,
    /// where `chflags(UF_HIDDEN)` on the same file does then report it.
    let private newFileFlags (platform : SimulatedUnixPlatform) : uint32 option =
        if SimulatedUnixPlatform.reportsFileFlags platform then
            Some 0u
        else
            None

    /// The status of an inode this filesystem holds, or `None` if it holds no
    /// such inode.
    ///
    /// The whole of what a `stat`-family syscall reports; the syscalls differ
    /// only in how they reach the inode. `fstat` is this plus a descriptor
    /// lookup, and `stat`/`lstat` are this plus a path resolution.
    ///
    /// Refuses for a directory on an NFS mount, and for the root of the device
    /// filesystem, whose size and link count this kernel cannot state; see
    /// `StatRefusal`.
    let statOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileStatus, StatRefusal> option
        =
        match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
        | None -> None
        | Some entry ->

        let permissions =
            match Inode.permissions entry with
            | InodePermissions.Stored bits -> bits
            | InodePermissions.PlatformSymlinkDefault ->
                SimulatedUnixPlatform.symlinkPermissions system.Machine.UnixPlatform

        let fileSystem = system.Machine.FileSystem
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        // The device filesystem's own `st_dev`, or the root filesystem's.
        let deviceId =
            match VirtualFileSystem.mountedRootOf inode fileSystem, system.Machine.DeviceMount with
            | None, _ -> VirtualFileSystem.deviceId
            | Some _, DeviceFileSystemMount.Devtmpfs devtmpfs -> devtmpfs.DeviceId
            | Some root, DeviceFileSystemMount.Devfs ->
                failwith
                    $"UnixPathResolution.statOf: inode %O{inode} is on the filesystem mounted at inode %O{root}, which is Darwin's devfs; no path or descriptor can reach it (this is a bug in this library)."

        let sizeAndLinks : Result<int64 * int64, StatRefusal> =
            match entry.Content with
            | InodeContent.RegularFile (contents, _) ->
                Ok (int64 contents.Length, int64 (VirtualFileSystem.bindingCount inode fileSystem))
            // Measured 0 on both flavours, for every device.
            | InodeContent.CharacterDevice _ -> Ok (0L, int64 (VirtualFileSystem.bindingCount inode fileSystem))
            | InodeContent.Directory _ when (VirtualFileSystem.mountOf inode fileSystem).IsSome ->
                Error (StatRefusal.DeviceFileSystemRoot inode)
            // `readlink` reports the target's byte length as the link's size,
            // and a process can see it through `lstat`.
            | InodeContent.Symlink target ->
                Ok (
                    int64 (UnixByteString.length (SymlinkTarget.toByteString target)),
                    int64 (VirtualFileSystem.bindingCount inode fileSystem)
                )
            | InodeContent.Directory _ ->
                let fsType = UnixMachineState.fileSystemTypeOf inode system.Machine

                match
                    EmulatedFileSystemType.directorySize fsType (VirtualFileSystem.entryCount inode fileSystem),
                    EmulatedFileSystemType.directoryLinkCount fsType inode fileSystem
                with
                | Some size, Some links -> Ok (size, links)
                | _ ->
                    match fsType with
                    | EmulatedFileSystemType.Nfs -> Error (StatRefusal.NfsDirectorySize inode)
                    | EmulatedFileSystemType.Tmpfs
                    | EmulatedFileSystemType.Apfs ->
                        failwith
                            $"UnixPathResolution.statOf: EmulatedFileSystemType states no size or no link count for a %O{fsType} directory, which has measured ones (this is a bug in this library)"

        let birthTime =
            // Withheld rather than reported when the platform has no
            // `st_birthtime`. The inode knows its birth either way; this governs
            // only what a process is told.
            if SimulatedUnixPlatform.reportsBirthTime system.Machine.UnixPlatform then
                Some entry.Times.Birth
            else
                None

        match sizeAndLinks with
        | Error refusal -> Some (Error refusal)
        | Ok (size, links) ->

        Some (
            Ok
                {
                    Mode = InodeContent.fileTypeBits entry.Content ||| PermissionBits.toInt permissions
                    UserId = entry.Owner.User
                    GroupId = entry.Owner.Group
                    Size = size
                    AccessTime = entry.Times.Access
                    ModificationTime = entry.Times.Modification
                    StatusChangeTime = entry.Times.StatusChange
                    BirthTime = birthTime
                    DeviceId = deviceId
                    Inode = inode
                    LinkCount = reportedLinkCount system.Machine.UnixPlatform links
                    SpecialFileDevice =
                        match entry.Content with
                        | InodeContent.CharacterDevice (device, _) -> CharacterDevice.specialFileDevice flavour device
                        | InodeContent.RegularFile _
                        | InodeContent.Directory _
                        | InodeContent.Symlink _ -> 0L
                    FileFlags = newFileFlags system.Machine.UnixPlatform
                }
        )

    /// `stat`, of a path this kernel has already copied in.
    let internal statParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileStatusAnswer, StatRefusal>
        =
        match resolvePath policy path system with
        | Error (PathFailure.Errno error) -> Ok (FileStatusAnswer.Failed error)
        | Error (PathFailure.Refused refusal) -> Error (StatRefusal.Path refusal)
        | Ok inode ->

        match statOf inode system with
        | Some (Ok status) -> Ok (FileStatusAnswer.Reported status)
        | Some (Error refusal) -> Error refusal
        | None ->
            failwith
                $"UnixPathResolution.stat: resolving %O{path} returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

    /// `stat(2)` and `lstat(2)`: report the status of the inode `path` names,
    /// the two differing only in whether a symbolic link in the final position
    /// is followed.
    ///
    /// Changes nothing and returns no system, for the reason `fstat` does not:
    /// a `stat` records no access.
    ///
    /// Refuses for a directory on an NFS mount and for the device filesystem's
    /// root, as `statOf` does, and for a path this kernel will not resolve; see
    /// `StatRefusal`. The three descriptor kinds `fstat` also refuses for are
    /// unreachable from here: every inode a path resolves to is one this
    /// filesystem holds, since a name for an inode-free object cannot be
    /// created in it.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let stat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileStatusAnswer, StatRefusal>
        =
        match copyIn path system with
        | Error error -> Ok (FileStatusAnswer.Failed error)
        | Ok path -> statParsed policy path system

    /// `fstat(2)`: report the status of the inode `fd` names.
    ///
    /// Changes nothing and returns no system, which is not merely today's
    /// implementation: a real `fstat` records no access, and neither does this
    /// one, so there is nothing for a caller to write back.
    ///
    /// An end of a pipe the process made reports `S_IFIFO`, and each flavour's
    /// own permission bits, size, timestamps and identity: see `PipeInodes`,
    /// `PipeTimes` and `UnixMachineState.PipeDevice`.
    ///
    /// Refuses for a descriptor this kernel holds no inode for — an event
    /// queue, a socket — and for an end of a pipe the process was launched with,
    /// whose owner and timestamps are the launcher's. That is a limit of the
    /// model rather than an absent kernel answer; see `FStatRefusal`. Also
    /// refuses for a directory on an NFS mount, as `statOf` does.
    let fstat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileStatusAnswer, FStatRefusal>
        =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Ok (FileStatusAnswer.Failed UnixError.EBADF)
        | Some (OpenFileTarget.Kqueue _)
        | Some (OpenFileTarget.Epoll _) -> Error FStatRefusal.EventQueue
        | Some (OpenFileTarget.Socket socketId) -> Error (FStatRefusal.Socket socketId)
        | Some (OpenFileTarget.Pipe (pipeId, pipeEnd)) ->
            let pipe = UnixMachineState.pipe pipeId system.Machine

            match pipe.Origin with
            | PipeOrigin.Launched _ -> Error (FStatRefusal.LaunchedPipe pipeId)
            | PipeOrigin.Made status ->

            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
            let fifo = 0o010000

            // Linux reports 0 always. Darwin reports the bytes held, through the
            // write end too -- but not once the read end has closed, when the
            // write end reports 0 while FIONREAD on a surviving read end would
            // still see them.
            let size =
                match flavour, pipeEnd with
                | SimulatedUnixFlavour.Linux, _ -> 0L
                | SimulatedUnixFlavour.Darwin, PipeEnd.Read -> int64 (PipeBuffer.held pipe.Buffer)
                | SimulatedUnixFlavour.Darwin, PipeEnd.Write ->
                    if UnixProcessState.pipeEndOpen pipeId pipe PipeEnd.Read system.Process then
                        int64 (PipeBuffer.held pipe.Buffer)
                    else
                        0L

            let inode =
                match status.Inodes, pipeEnd with
                | PipeInodes.Shared inode, _ -> inode
                | PipeInodes.PerEnd (readEnd, _), PipeEnd.Read -> readEnd
                | PipeInodes.PerEnd (_, writeEnd), PipeEnd.Write -> writeEnd

            let access =
                match pipeEnd with
                | PipeEnd.Read -> status.Times.ReadEndAccess
                | PipeEnd.Write -> status.Times.Created

            // Darwin reports a pipe's birth time as 0: the epoch, not its
            // creation.
            let birthTime =
                if SimulatedUnixPlatform.reportsBirthTime system.Machine.UnixPlatform then
                    Some UnixTimestamp.epoch
                else
                    None

            // Measured 2026-10-02 by `stat-fields.c`: 1 through either end on
            // Linux 6.18.5 and 0 on Darwin 27.0, and the same through the read
            // end once the write end has closed.
            let links =
                match flavour with
                | SimulatedUnixFlavour.Linux -> 1L
                | SimulatedUnixFlavour.Darwin -> 0L

            {
                Mode = fifo ||| PermissionBits.toInt status.Permissions
                UserId = status.Owner.User
                GroupId = status.Owner.Group
                Size = size
                AccessTime = access
                ModificationTime = status.Times.Modification
                StatusChangeTime = status.Times.StatusChange
                BirthTime = birthTime
                DeviceId = system.Machine.PipeDevice
                Inode = inode
                LinkCount = reportedLinkCount system.Machine.UnixPlatform links
                // Measured 0 through either end on both flavours.
                SpecialFileDevice = 0L
                FileFlags = newFileFlags system.Machine.UnixPlatform
            }
            |> FileStatusAnswer.Reported
            |> Ok
        | Some (OpenFileTarget.File (inode, _))
        | Some (OpenFileTarget.Directory (inode, _))
        | Some (OpenFileTarget.CharacterDevice (inode, _)) ->

        match statOf inode system with
        | Some (Ok status) -> Ok (FileStatusAnswer.Reported status)
        | Some (Error (StatRefusal.NfsDirectorySize inode)) -> Error (FStatRefusal.NfsDirectorySize inode)
        | Some (Error (StatRefusal.DeviceFileSystemRoot inode)) -> Error (FStatRefusal.DeviceFileSystemRoot inode)
        | Some (Error (StatRefusal.Path refusal)) ->
            failwith
                $"UnixPathResolution.fstat: reporting the status of inode %O{inode} refused a path, though fstat resolves none: %s{PathRefusal.describe refusal} (this is a bug in this library)."
        | None ->
            failwith
                $"UnixPathResolution.fstat: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

    /// What `chmod` and `fchmod` do once they have reached `inode`, which is a
    /// regular file or a directory this filesystem holds: EPERM changing
    /// nothing, or the new bits with `ctime` moved.
    let private changeModeOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, InodeNumber * ModeChangeRefusal>
        =
        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixPathResolution.changeModeOf: inode %O{inode} is not in the filesystem, but a path or a descriptor resolved to it. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        let rule = SimulatedUnixPlatform.privilegedModeChange system.Machine.UnixPlatform

        match PermissionBits.afterModeChange rule (Standing.toward system.Process.Credentials entry.Owner) mode with
        | Error refusal -> Error (inode, refusal)
        | Ok ModeChange.Forbidden -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
        | Ok (ModeChange.Permitted bits) ->

        let now = UnixMachineState.realtime system.Machine

        Ok (
            SyscallAnswer.Completed 0L,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = VirtualFileSystem.setPermissions inode bits now system.Machine.FileSystem
                    }
            }
        )

    /// `chmod`, of a path this kernel has already copied in.
    let internal chmodParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ChModRefusal>
        =
        // Measured on both platforms (`chmod-rules.c`): through a link to a
        // file or a directory the target changes and the link does not;
        // "d/" and "ld/" are the directory, "f/" and "lf/" ENOTDIR, a
        // dangling link and an absent name ENOENT, a link to itself ELOOP,
        // the empty path ENOENT and "f/under" ENOTDIR. Those are exactly what
        // `Follow` with a demanded trailing separator answers.
        match resolvePath SymlinkPolicy.Follow path system with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (ChModRefusal.Path refusal)
        | Ok inode ->
            changeModeOf inode mode system
            |> Result.mapError ChModRefusal.UnmeasuredModeChange

    /// `chmod(2)`: change the mode of the inode `path` names.
    ///
    /// `mode` is the raw mode word, of which only the low twelve bits are read;
    /// see `PermissionBits.afterModeChange` for what the caller may set.
    ///
    /// A symbolic link in the final position is followed, so the link's target
    /// changes and a dangling link is ENOENT. Every other failure but EPERM is
    /// the path resolution's own.
    ///
    /// Refuses where the mode change has not been measured for this caller; see
    /// `ChModRefusal`.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let chmod<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ChModRefusal>
        =
        match copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> chmodParsed path mode system

    /// `fchmod(2)`: change the mode of the inode `fd` names.
    ///
    /// `mode` is the raw mode word, as for `chmod`. The descriptor's access mode
    /// plays no part: a descriptor open only for reading will do.
    ///
    /// A descriptor naming something other than a regular file or a directory
    /// answers as its flavour does, which on Linux can be a refusal; see
    /// `FChModRefusal`.
    let fchmod<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, FChModRefusal>
        =
        // Measured on both platforms (`chmod-rules.c`): a regular file open
        // read-only, write-only and read-write, a directory, and a file whose
        // last name has gone, all change; someone else's file is EPERM
        // however it was opened; a closed descriptor and -1 are EBADF. The
        // rest is per flavour: Linux changes the mode of a pipe end and of an
        // AF_INET, AF_INET6 and AF_UNIX socket, stream or datagram, listening
        // or not (fstat reports the new mode), and answers EOPNOTSUPP for an
        // epoll instance; Darwin answers EINVAL for all of those and for a
        // kqueue. A Linux pipe follows the same rule as a file, over every
        // mode, for its owner in and out of its group, a non-owner in and out
        // of it, and root; the change shows through both ends, and moves both
        // ends' ctime and nothing else.
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        match FileDescriptorRegistry.tryFindObject fd system.Process.FileDescriptors with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some (OpenFileObject.File inode) ->
            changeModeOf inode mode system
            |> Result.mapError FChModRefusal.UnmeasuredModeChange
        // An epoll instance (Linux) and a kqueue (Darwin).
        | Some OpenFileObject.AnonymousInode -> Ok (SyscallAnswer.Failed UnixError.EOPNOTSUPP, system)
        | Some (OpenFileObject.Kqueue _) -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | Some (OpenFileObject.Socket socket) ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> Error (FChModRefusal.Socket socket)
            | SimulatedUnixFlavour.Darwin -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | Some (OpenFileObject.Pipe pipeId) ->
            match flavour with
            | SimulatedUnixFlavour.Darwin -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            | SimulatedUnixFlavour.Linux ->

            let pipe = UnixMachineState.pipe pipeId system.Machine

            match pipe.Origin with
            | PipeOrigin.Launched _ -> Error (FChModRefusal.LaunchedPipe pipeId)
            | PipeOrigin.Made status ->

            let rule = SimulatedUnixPlatform.privilegedModeChange system.Machine.UnixPlatform

            match
                PermissionBits.afterModeChange rule (Standing.toward system.Process.Credentials status.Owner) mode
            with
            | Error refusal ->
                failwith
                    $"UnixPathResolution.fchmod: a Linux platform's mode-change rule refused a caller (%s{ModeChangeRefusal.describe refusal}), but Linux's rule answers every caller (this is a bug in this library)."
            | Ok ModeChange.Forbidden -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
            | Ok (ModeChange.Permitted bits) ->

            let changed =
                { pipe with
                    Origin =
                        PipeOrigin.Made
                            { status with
                                Permissions = bits
                                Times =
                                    { status.Times with
                                        StatusChange = UnixMachineState.realtime system.Machine
                                    }
                            }
                }

            Ok (
                SyscallAnswer.Completed 0L,
                { system with
                    Machine =
                        { system.Machine with
                            Pipes = Map.add pipeId changed system.Machine.Pipes
                        }
                }
            )

    /// What `chown`, `lchown` and `fchown` do once they have reached `inode`,
    /// which is an inode of any kind this filesystem holds: EPERM changing
    /// nothing, success changing nothing, or the new owner and bits with
    /// `ctime` moved.
    let private changeOwnerOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, InodeNumber * OwnerChangeRefusal>
        =
        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixPathResolution.changeOwnerOf: inode %O{inode} is not in the filesystem, but a path or a descriptor resolved to it. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        let platform = system.Machine.UnixPlatform
        let credentials = system.Process.Credentials

        let target, bits =
            match entry.Content with
            | InodeContent.RegularFile (_, bits)
            | InodeContent.CharacterDevice (_, bits) -> OwnerChangeTarget.NonDirectory, bits
            | InodeContent.Directory directory -> OwnerChangeTarget.Directory, directory.Permissions
            | InodeContent.Symlink _ ->
                OwnerChangeTarget.NonDirectory, SimulatedUnixPlatform.symlinkPermissions platform

        match
            OwnerChangeRules.verdict
                (SimulatedUnixPlatform.ownerChangeRule platform)
                (Standing.toward credentials entry.Owner)
                (OwnerChangeRequest.classify credentials entry.Owner user group)
                target
                bits
        with
        | Error refusal -> Error (inode, refusal)
        | Ok OwnerChange.Forbidden -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
        | Ok OwnerChange.Untouched -> Ok (SyscallAnswer.Completed 0L, system)
        | Ok (OwnerChange.Changed changed) ->

        let now = UnixMachineState.realtime system.Machine

        let owner =
            {
                User = defaultArg user entry.Owner.User
                Group = defaultArg group entry.Owner.Group
            }

        let vfs = VirtualFileSystem.setOwner inode owner now system.Machine.FileSystem

        let vfs =
            if changed = bits then
                vfs
            else

            match entry.Content with
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Directory _ -> VirtualFileSystem.setPermissions inode changed now vfs
            | InodeContent.Symlink _ ->
                failwith
                    $"UnixPathResolution.changeOwnerOf: the owner-change rule changed symbolic link %O{inode}'s permission bits from %O{bits} to %O{changed}, but a link's bits are the platform's and carry no set-ID bit for a rule to clear (this is a bug in this library)."

        Ok (
            SyscallAnswer.Completed 0L,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = vfs
                    }
            }
        )

    /// `chown`, of a path this kernel has already copied in.
    let internal chownParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ChOwnRefusal>
        =
        // Measured on both platforms (`chown-rules.c`): through a link to a
        // file or a directory the target changes and the link does not;
        // "d/" and "ld/" are the directory, "f/" and "lf/" ENOTDIR, a
        // dangling link and an absent name ENOENT, a link to itself ELOOP,
        // the empty path ENOENT, "f/under" ENOTDIR, and a path through an
        // unsearchable directory EACCES even naming another's uid. Someone
        // else's file named as "f/" or "f/under" is ENOTDIR, not EPERM.
        match resolvePath SymlinkPolicy.Follow path system with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (ChOwnRefusal.Path refusal)
        | Ok inode ->
            changeOwnerOf inode user group system
            |> Result.mapError ChOwnRefusal.UnmeasuredOwnerChange

    /// `chown(2)`: change the owner and group of the inode `path` names.
    ///
    /// `None` is `(uid_t)-1` or `(gid_t)-1`, which leaves that ID as it is.
    /// See `OwnerChangeRules.verdict` for who may name which IDs, and which
    /// set-ID bits a change clears.
    ///
    /// A symbolic link in the final position is followed, so the link's target
    /// changes and a dangling link is ENOENT. Every other failure but EPERM is
    /// the path resolution's own, and comes first: a path through a directory
    /// the caller may not search is EACCES whatever it asks.
    ///
    /// Refuses where the change has not been measured for this caller; see
    /// `ChOwnRefusal`.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let chown<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ChOwnRefusal>
        =
        match copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> chownParsed path user group system

    /// `lchown`, of a path this kernel has already copied in.
    let internal lchownParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ChOwnRefusal>
        =
        // Measured on both platforms (`chown-rules.c`): a link to a file, a
        // link to a directory, a dangling link and a link to itself each
        // change themselves; "ld/" changes the directory and "lf/" is
        // ENOTDIR; the remaining path rows answer as `chown`'s do.
        match resolvePath SymlinkPolicy.NoFollowFinal path system with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error (ChOwnRefusal.Path refusal)
        | Ok inode ->
            changeOwnerOf inode user group system
            |> Result.mapError ChOwnRefusal.UnmeasuredOwnerChange

    /// `lchown(2)`: change the owner and group of the inode `path` names,
    /// without following a symbolic link in the final position, so a link
    /// itself changes.
    ///
    /// A trailing separator makes the final component a directory, so "l/" for
    /// a link to a directory changes the directory, and for a link to anything
    /// else is ENOTDIR. Otherwise as `chown`.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let lchown<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, ChOwnRefusal>
        =
        match copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> lchownParsed path user group system

    /// `fchown(2)`: change the owner and group of the inode `fd` names.
    ///
    /// `None` is `(uid_t)-1` or `(gid_t)-1`, as for `chown`. The descriptor's
    /// access mode plays no part: a descriptor open only for reading will do.
    ///
    /// A descriptor naming something other than a regular file or a directory
    /// answers as its flavour does, which on Linux can be a refusal; see
    /// `FChOwnRefusal`.
    let fchown<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (user : UserId option)
        (group : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, FChOwnRefusal>
        =
        // Measured on both platforms (`chown-rules.c`): a regular file open
        // read-only, write-only and read-write, a directory, and a file whose
        // last name has gone, all change; someone else's file opened for
        // reading answers as `chown` would; a closed descriptor and -1 are
        // EBADF. Per flavour: Linux changes the owner of a pipe end (by the
        // same rule as a file, over every mode, for every caller standing,
        // seen through both ends, moving both ends' ctime) and of an AF_INET,
        // AF_INET6 and AF_UNIX socket, stream or datagram, and answers
        // EOPNOTSUPP for an epoll instance whoever asks; Darwin answers EINVAL
        // for a pipe end, every one of those sockets, and a kqueue.
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        match FileDescriptorRegistry.tryFindObject fd system.Process.FileDescriptors with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some (OpenFileObject.File inode) ->
            changeOwnerOf inode user group system
            |> Result.mapError FChOwnRefusal.UnmeasuredOwnerChange
        // An epoll instance (Linux) and a kqueue (Darwin).
        | Some OpenFileObject.AnonymousInode -> Ok (SyscallAnswer.Failed UnixError.EOPNOTSUPP, system)
        | Some (OpenFileObject.Kqueue _) -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | Some (OpenFileObject.Socket socket) ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> Error (FChOwnRefusal.Socket socket)
            | SimulatedUnixFlavour.Darwin -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | Some (OpenFileObject.Pipe pipeId) ->
            match flavour with
            | SimulatedUnixFlavour.Darwin -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            | SimulatedUnixFlavour.Linux ->

            let pipe = UnixMachineState.pipe pipeId system.Machine

            match pipe.Origin with
            | PipeOrigin.Launched _ -> Error (FChOwnRefusal.LaunchedPipe pipeId)
            | PipeOrigin.Made status ->

            let credentials = system.Process.Credentials

            match
                OwnerChangeRules.verdict
                    (SimulatedUnixPlatform.ownerChangeRule system.Machine.UnixPlatform)
                    (Standing.toward credentials status.Owner)
                    (OwnerChangeRequest.classify credentials status.Owner user group)
                    OwnerChangeTarget.NonDirectory
                    status.Permissions
            with
            | Error refusal ->
                failwith
                    $"UnixPathResolution.fchown: a Linux platform's owner-change rule refused a caller (%s{OwnerChangeRefusal.describe refusal}), but Linux's rule answers every caller (this is a bug in this library)."
            | Ok OwnerChange.Forbidden -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
            | Ok OwnerChange.Untouched -> Ok (SyscallAnswer.Completed 0L, system)
            | Ok (OwnerChange.Changed bits) ->

            let changed =
                { pipe with
                    Origin =
                        PipeOrigin.Made
                            { status with
                                Owner =
                                    {
                                        User = defaultArg user status.Owner.User
                                        Group = defaultArg group status.Owner.Group
                                    }
                                Permissions = bits
                                Times =
                                    { status.Times with
                                        StatusChange = UnixMachineState.realtime system.Machine
                                    }
                            }
                }

            Ok (
                SyscallAnswer.Completed 0L,
                { system with
                    Machine =
                        { system.Machine with
                            Pipes = Map.add pipeId changed system.Machine.Pipes
                        }
                }
            )

    /// `futimens(2)` with two explicit times: set the access and modification
    /// times of the file or directory `fd` names to `access` and
    /// `modification`, and its status-change time to now.
    ///
    /// Only the explicit form is expressible: a `UnixTimestamp` cannot hold the
    /// `UTIME_NOW` or `UTIME_OMIT` markers, nor a nanosecond field the kernel
    /// would answer EINVAL for.
    ///
    /// EBADF for a descriptor the process does not hold, and EPERM unless the
    /// caller owns the inode or is privileged; the descriptor's access mode
    /// plays no part. The birth time does not move.
    let futimens<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (access : UnixTimestamp)
        (modification : UnixTimestamp)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, FUTimensRefusal>
        =
        // Measured on Linux 6.18.5 (`copy-file-syscalls.c`, tmpfs and ext4
        // alike): a regular file opened read-only, write-only and read-write
        // and a directory all take the times; a closed descriptor and 9999 are
        // EBADF, ahead of a nanosecond field of 1e9 (EINVAL for an open one);
        // seconds -1e9, -1, 0, 2^31 - 1, 2^31 and 253402300799 are all stored
        // exactly. uid 1000 on root's 0666 file opened O_RDWR is EPERM and
        // changes nothing; uid 1000 on its own 0444 file opened O_RDONLY, and
        // root on uid 1000's 0600 file, succeed. ctime moves to now; the birth
        // time stays put even when the modification time is set before it.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (FUTimensRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        match FileDescriptorRegistry.tryFindObject fd system.Process.FileDescriptors with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some (OpenFileObject.AnonymousInode as object)
        | Some (OpenFileObject.Kqueue _ as object)
        | Some (OpenFileObject.Socket _ as object)
        | Some (OpenFileObject.Pipe _ as object) -> Error (FUTimensRefusal.UnmodelledObject object)
        | Some (OpenFileObject.File inode) ->

        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixPathResolution.futimens: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        let standing = Standing.toward system.Process.Credentials entry.Owner

        if not standing.Owns && standing.Privilege = CallerPrivilege.Unprivileged then
            Ok (SyscallAnswer.Failed UnixError.EPERM, system)
        else

        let now = UnixMachineState.realtime system.Machine

        Ok (
            SyscallAnswer.Completed 0L,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = VirtualFileSystem.setTimes inode access modification now system.Machine.FileSystem
                    }
            }
        )

    /// What `statfs(2)` reports for the filesystem `inode` is on.
    let private statisticsOfInode<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (system : UnixSystem<'Task, 'Handler>)
        : FileSystemStatisticsAnswer
        =
        match VirtualFileSystem.mountedRootOf inode system.Machine.FileSystem with
        | None ->
            FileSystemStatistics.ofObject system.Machine.UnixPlatform system.Machine.Mount (OpenFileObject.File inode)
        | Some _ ->
            FileSystemStatistics.ofDeviceMount system.Machine.UnixPlatform system.Machine.DeviceMount
            |> FileSystemStatisticsAnswer.Reported

    /// `statfs`, of a path this kernel has already copied in.
    let internal statfsParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileSystemStatisticsAnswer, PathRefusal>
        =
        FileSystemStatistics.assertCoherent "UnixPathResolution.statfs" system.Machine.UnixPlatform system.Machine.Mount

        match resolvePath SymlinkPolicy.Follow path system with
        | Error (PathFailure.Errno error) -> Ok (FileSystemStatisticsAnswer.Failed error)
        | Error (PathFailure.Refused refusal) -> Error refusal
        | Ok inode -> Ok (statisticsOfInode inode system)

    /// `statfs(2)`: report the filesystem the inode `path` names is on.
    ///
    /// A symbolic link in the final position is followed, as `statfs` always
    /// does, so a dangling link is ENOENT. Every failure is the path
    /// resolution's own, as for `stat(2)`.
    ///
    /// Changes nothing and returns no system. Refuses a path this kernel will
    /// not resolve (see `PathRefusal`), and throws for a machine whose platform
    /// and mount do not describe one machine, whatever the path.
    ///
    /// `path` is the argument's bytes, copied in before anything else: EFAULT
    /// if they were unreadable, ENAMETOOLONG if they run past `PATH_MAX`.
    let statfs<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileSystemStatisticsAnswer, PathRefusal>
        =
        FileSystemStatistics.assertCoherent "UnixPathResolution.statfs" system.Machine.UnixPlatform system.Machine.Mount

        match copyIn path system with
        | Error error -> Ok (FileSystemStatisticsAnswer.Failed error)
        | Ok path -> statfsParsed path system

    /// `fstatfs(2)`: report the filesystem the object `fd` names is on.
    ///
    /// EBADF for a descriptor the process does not hold. A file or directory
    /// is on the root filesystem or the device filesystem. Every other object
    /// is on one of Linux's internal filesystems, and Darwin answers EINVAL for
    /// it.
    ///
    /// Changes nothing and returns no system. Refuses a machine whose platform
    /// and mount do not describe one machine, whatever the descriptor.
    let fstatfs<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : FileSystemStatisticsAnswer
        =
        FileSystemStatistics.assertCoherent
            "UnixPathResolution.fstatfs"
            system.Machine.UnixPlatform
            system.Machine.Mount

        match FileDescriptorRegistry.tryFindObject fd system.Process.FileDescriptors with
        | None -> FileSystemStatisticsAnswer.Failed UnixError.EBADF
        | Some (OpenFileObject.File inode) -> statisticsOfInode inode system
        | Some target -> FileSystemStatistics.ofObject system.Machine.UnixPlatform system.Machine.Mount target

    /// <summary>
    /// The absolute path of this process's current working directory, or <c>None</c> if
    /// no path reaches it any more.
    /// </summary>
    /// <remarks>
    /// You get <c>None</c> if the working directory was deleted while the process was running.
    /// In that case, relative paths still resolve (because they start from the inode, which the
    /// process still holds), and you can use <c>chdir("..")</c>, for example, to step to a different
    /// directory and get a path again.
    /// </remarks>
    let currentDirectoryPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : AbsoluteUnixPath option
        =
        // Measured on both flavours; see docs/probes/chdir.
        VirtualFileSystem.pathOfDirectory system.Process.CurrentDirectoryInode system.Machine.FileSystem

    /// <summary>
    /// <c>getcwd(3)</c>: report the current directory's path into the caller's buffer.
    /// </summary>
    /// <remarks>
    /// This has no side-effects on the kernel.
    /// </remarks>
    /// <param name="destination">
    /// Where the kernel writes the result.
    /// </param>
    /// <param name="capacity">
    /// The caller's buffer size, as the <c>size_t</c> it is.
    /// </param>
    /// <param name="system">
    /// The state of the kernel we're reading data out of.
    /// </param>
    let getcwd<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : UserBuffer)
        (capacity : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetCwdAnswer, GetCwdRefusal>
        =
        // The whole measured ordering lives here, and the destination's
        // classification is consulted last on both flavours — a too-small buffer is
        // ERANGE whatever the destination is, and a removed current directory
        // outranks even that on Linux.

        /// The destination is about to be written. Every caller of this has
        /// already decided that the bytes are wanted, so a destination that
        /// cannot take them is the last thing left to fail.
        let transfer (onWritten : GetCwdAnswer) : Result<GetCwdAnswer, GetCwdRefusal> =
            match destination with
            | UserBuffer.Mapped -> Ok onWritten
            | UserBuffer.Opaque -> Error (GetCwdRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (GetCwdRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
            | UserBuffer.Unmapped _ ->
                match SimulatedUnixPlatform.getCwdDestinationFault system.Machine.UnixPlatform with
                | GetCwdDestinationFault.ReportedAsEfault -> Ok (GetCwdAnswer.Failed UnixError.EFAULT)
                | GetCwdDestinationFault.FatalToTheProcess -> Error GetCwdRefusal.FatalToTheProcess

        /// Whether a destination this caller cannot write makes the call fatal
        /// rather than answerable, on a flavour that assembles the path with
        /// stores executed in the caller's own context.
        let storeWouldBeFatal : bool =
            match destination with
            | UserBuffer.Unmapped _ ->
                match SimulatedUnixPlatform.getCwdDestinationFault system.Machine.UnixPlatform with
                | GetCwdDestinationFault.FatalToTheProcess -> true
                | GetCwdDestinationFault.ReportedAsEfault -> false
            // `Opaque` and `Addressless` name memory a real `getcwd` writes
            // perfectly well; what is missing is this client's ability to
            // perform the store. That only matters where bytes are actually
            // reported, so those two are screened at the transfer instead.
            | UserBuffer.Mapped
            | UserBuffer.Opaque
            | UserBuffer.Addressless -> false

        // Measured first on both, and it beats the removed-directory case below:
        // with the current directory gone, `getcwd(buf, 0)` is still EINVAL.
        if capacity = 0UL then
            Ok (GetCwdAnswer.Failed UnixError.EINVAL)
        elif capacity >= 2UL && storeWouldBeFatal then
            // From capacity 2 up, such a flavour may have stored *before* it
            // decides which answer to give, so a destination it cannot write
            // kills the process on paths that would otherwise be ERANGE or
            // ENOENT -- not only on the success path.
            //
            // Whether it has stored yet depends on which of libc's internal
            // routes the call took, and that is selected by the current
            // directory's own length against a threshold that is *not* a kernel
            // fact: measured on macOS 26.6 at capacity 8 with an unmapped
            // destination, a path of 1015 bytes is a clean ERANGE and one of
            // 1016 bytes is a SIGSEGV. That is neither PATH_MAX (1024) nor any
            // documented constant -- it is one libc build's internal slack.
            //
            // So this refuses from capacity 2 up rather than encoding 1016.
            // It deliberately over-refuses the short-path cell, where the real
            // call answers ERANGE without touching the destination: a refusal
            // says "this library cannot tell you", which is honest, where
            // picking a side would answer ERANGE for a call that really dies.
            Error GetCwdRefusal.FatalToTheProcess
        else

        match currentDirectoryPath system with
        | None ->
            // No path reaches the directory the process is in, so there is
            // nothing to measure against the buffer. What the buffer can still
            // change is per-flavour; see `GetCwdOrphanAnswer`.
            match SimulatedUnixPlatform.getCwdOrphanAnswer system.Machine.UnixPlatform with
            | GetCwdOrphanAnswer.AlwaysDetached -> Ok (GetCwdAnswer.Failed UnixError.ENOENT)
            | GetCwdOrphanAnswer.ShortestPathFirst ->
                // Room for "/" and its terminator, which is what this flavour
                // writes before it starts climbing. Two bytes, not the length of
                // the path that used to be here -- and below two it writes
                // nothing at all, which is why the refusal above starts at two.
                if capacity < 2UL then
                    Ok (GetCwdAnswer.Failed UnixError.ERANGE)
                else
                    Ok (GetCwdAnswer.Failed UnixError.ENOENT)
        | Some path ->

        /// The bytes a successful call would place, terminator included. Also
        /// what the comparison producing ERANGE is made against, so the two
        /// cannot disagree about whether the path fits.
        let terminated : ImmutableArray<byte> =
            (UnixByteString.toBytes (AbsoluteUnixPath.toByteString path)).Add 0uy

        if capacity < uint64 terminated.Length then
            // `getcwd` needs room for the path *and* its NUL, which is why a
            // buffer of the path's own length is one byte short rather than an
            // exact fit. Measured with an unwritable destination too: on the
            // flavour that copies from the kernel this answers before the
            // destination is looked at, `getcwd((char*)123, 1)` being ERANGE
            // rather than EFAULT.
            Ok (GetCwdAnswer.Failed UnixError.ERANGE)
        else
            transfer (GetCwdAnswer.Reported terminated)

    /// `chdir`, of a path this kernel has already copied in.
    let internal chdirParsed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, PathRefusal>
        =
        // Conformance across platforms was measured on both kernels across object type, final
        // symlink following, trailing separator, which permission bit, name length,
        // navigation, and the current directory removed underneath the process —
        // every row identical. See `docs/probes/chdir/`.
        //
        // `Follow`, which carries `TrailingSeparatorPolicy.Demand`. That one
        // call is most of this syscall's error surface: ENOENT for a name that
        // is not there and for a dangling link, ENOTDIR for a regular file and
        // for "f/", ENAMETOOLONG for an over-long component, ELOOP for a cycle —
        // and it follows "ld" to what it names, which is why `getcwd` afterwards
        // reports the target rather than the link.
        match resolvePath SymlinkPolicy.Follow path system with
        | Error (PathFailure.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Error (PathFailure.Refused refusal) -> Error refusal
        | Ok target ->

        match VirtualFileSystem.tryGetContent target system.Machine.FileSystem with
        | None ->
            failwith
                $"UnixPathResolution.chdir: the walk resolved \"%s{UnixPath.toEscaped path}\" to inode %O{target}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        // Reached by a symbolic link to a regular file as well as by a plain
        // one: `Follow` lands on the file, and only then is it a type error.
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.CharacterDevice _) -> Ok (SyscallAnswer.Failed UnixError.ENOTDIR, system)
        | Some (InodeContent.Symlink _) ->
            // Unreachable, and asserted rather than answered: `Follow` traverses
            // a final symlink, so the walk above cannot hand back a link — a
            // chain that never terminates is ELOOP and one that ends nowhere is
            // ENOENT, both refused before here. Answering ENOTDIR instead would
            // be a plausible-looking reply from a walk that had stopped doing
            // what this syscall asked of it. Found by mutation: the arm was
            // dead, so nothing could tell the two apart.
            failwith
                $"UnixPathResolution.chdir: the walk resolved \"%s{UnixPath.toEscaped path}\" to inode %O{target}, which is a symbolic link -- but it ran under SymlinkPolicy.Follow, which never finishes on one (this is a bug in this library)."
        | Some (InodeContent.Directory directory) ->

        // The *search* bit, and not the read bit: measured on both kernels, a
        // 0o100 directory can be entered and a 0o400 one is EACCES. That is the
        // opposite way round from `opendir`, which wants read — the second place
        // the two have come apart.
        //
        // The walk above checks search on every directory it *traverses*; this
        // is the target's own bit, which nothing has asked about yet.
        let owner =
            match VirtualFileSystem.tryGet target system.Machine.FileSystem with
            | Some inode -> inode.Owner
            | None ->
                failwith
                    $"UnixPathResolution.chdir: inode %O{target} was a directory a moment ago and is now absent (this is a bug in this library)."

        if
            PermissionBits.deniedTo
                (Standing.toward system.Process.Credentials owner)
                AccessRequest.SearchDirectory
                directory.Permissions
        then
            Ok (SyscallAnswer.Failed UnixError.EACCES, system)
        else

        let previous = system.Process.CurrentDirectoryInode

        // Only the inode moves. `getcwd` derives the path from it, so the two
        // measured facts about the path come out on their own: `chdir("ld")`
        // with `ld -> d` reports d's path because d is the inode the walk landed
        // on, and `chdir(".")` in an `rmdir`'d directory reports nothing because
        // no path reaches that inode.
        let moved =
            { system with
                Process =
                    { system.Process with
                        CurrentDirectoryInode = target
                    }
            }

        // The current directory is pinned — `UnixProcessState.heldInodes`
        // includes it — so leaving one is a reference-dropping operation, and the
        // directory a process `rmdir`d before stepping out of it becomes free
        // exactly here. Without this it would be stranded for the run.
        Ok (SyscallAnswer.Completed 0L, ObjectLifetime.forgetIfUnheld previous moved)

    /// <summary>
    /// <c>chdir(2)</c>: set the relative-path resolution base directory to <c>path</c>.
    /// </summary>
    /// <remarks>
    /// <c>path</c> is the argument's bytes, copied in before anything else: <c>EFAULT</c> if they
    /// were unreadable, <c>ENAMETOOLONG</c> if they run past <c>PATH_MAX</c>.
    ///
    /// Refuses a path this kernel will not resolve; see <c>PathRefusal</c>.
    /// </remarks>
    let chdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, PathRefusal>
        =
        match copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error, system)
        | Ok path -> chdirParsed path system

    // The order of every step of `access` and `faccessat` is measured by
    // `access-rules.c`, on Linux 6.18.5 and Darwin 27.0: the mode and flag
    // words (`AccessRules.screen`), then the path's copy-in (an unreadable
    // pointer is EFAULT, and an over-long one ENAMETOOLONG, ahead of any
    // dirfd), then Linux's empty path, then the dirfd (EBADF for a number
    // naming nothing, ENOTDIR for a regular file), then the walk, then the
    // permission bits. An absolute path never looks at its dirfd, even one
    // naming nothing.

    let private screenFrom<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (directory : AtDirectory)
        (mode : int)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AccessProgress<'Task, 'Handler>, AccessRefusal>
        =
        match AccessRules.screen (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform) mode flags with
        | AccessScreen.Refused refusal -> Error refusal
        | AccessScreen.Failed error -> Ok (AccessProgress.Answered (SyscallAnswer.Failed error))
        | AccessScreen.Screened arguments ->
            Ok (
                AccessProgress.NeedsPath
                    {
                        System = system
                        Directory = directory
                        Arguments = arguments
                    }
            )

    /// <summary>
    /// The first half of <c>faccessat(2)</c>: screen its raw <c>mode</c> and <c>flags</c>,
    /// and decode its raw <c>dirfd</c>, before the path is read.
    /// </summary>
    /// <remarks>
    /// A client that reads the path out of a caller's memory calls this first, and
    /// reads the path only on <c>AccessProgress.NeedsPath</c>: Linux answers a bad mode
    /// or flag word EINVAL without reading the path, so the path may be a pointer the
    /// client could not read at all. <c>faccessat</c> is this followed by
    /// <c>accessWithPath</c>.
    /// </remarks>
    let faccessatScreenPhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (dirfd : int)
        (mode : int)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AccessProgress<'Task, 'Handler>, AccessRefusal>
        =
        let directory =
            AccessRules.atDirectory (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform) dirfd

        screenFrom directory mode flags system

    /// The first half of <c>access(2)</c>, as <c>faccessatScreenPhase</c> is of
    /// <c>faccessat</c>: <c>access</c> is <c>faccessat</c> from the current directory with
    /// no flags.
    let accessScreenPhase<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<AccessProgress<'Task, 'Handler>, AccessRefusal>
        =
        screenFrom AtDirectory.CurrentDirectory mode 0 system

    /// <summary>
    /// The second half of <c>access(2)</c> or <c>faccessat(2)</c>: copy in <c>path</c>, and
    /// answer the call <c>paused</c> describes.
    /// </summary>
    /// <remarks>
    /// See <c>faccessat</c> for what it answers and refuses.
    /// </remarks>
    let accessWithPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (paused : PausedAccess<'Task, 'Handler>)
        : Result<SyscallAnswer, AccessRefusal>
        =
        match box paused with
        | null ->
            failwith
                "UnixPathResolution.accessWithPath: this paused access is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixPathResolution.accessScreenPhase or faccessatScreenPhase instead."
        | _ -> ()

        let system = paused.System
        let directory = paused.Directory
        let arguments = paused.Arguments
        let platform = system.Machine.UnixPlatform
        let vfs = system.Machine.FileSystem

        match copyIn path system with
        | Error error -> Ok (SyscallAnswer.Failed error)
        | Ok path ->

        let credentials =
            match arguments.Ids with
            | AccessIds.Real -> Credentials.realIdsAsEffective system.Process.Credentials
            | AccessIds.Effective -> system.Process.Credentials

        let empty = UnixPath.isEmpty path

        // The inode a relative or empty path starts from, and whether it is a
        // directory; `None` when the call does not need one.
        let start : Result<Result<(InodeNumber * bool) option, UnixError>, AccessRefusal> =
            if UnixPath.isRooted path then
                Ok (Ok None)
            elif empty && arguments.EmptyPath = AccessEmptyPath.NoSuchEntryBeforeDescriptor then
                Ok (Error UnixError.ENOENT)
            else

            match directory with
            | AtDirectory.CurrentDirectory -> Ok (Ok (Some (system.Process.CurrentDirectoryInode, true)))
            | AtDirectory.Descriptor fd ->

            match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | None -> Ok (Error UnixError.EBADF)
            | Some description ->

            match description.Target with
            | OpenFileTarget.Directory (inode, _) -> Ok (Ok (Some (inode, true)))
            | OpenFileTarget.File (inode, _)
            | OpenFileTarget.CharacterDevice (inode, _) -> Ok (Ok (Some (inode, false)))
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Pipe _ -> Error (AccessRefusal.UnmodelledDescriptor fd)

        let target : Result<Result<InodeNumber, UnixError>, AccessRefusal> =
            match start with
            | Error refusal -> Error refusal
            | Ok (Error error) -> Ok (Error error)
            | Ok (Ok start) ->

            match start with
            | Some (inode, _) when empty && arguments.EmptyPath = AccessEmptyPath.NamesStartingPoint -> Ok (Ok inode)
            | Some (_, false) -> Ok (Error UnixError.ENOTDIR)
            | _ ->

            // A rooted path ignores the starting directory, and an empty one is
            // ENOENT from the walk, as `NoSuchEntryAfterDescriptor` wants.
            let startDirectory =
                match start with
                | Some (inode, _) -> inode
                | None -> system.Process.CurrentDirectoryInode

            match
                PathWalk.resolveFull
                    (SimulatedUnixPlatform.pathLimits platform)
                    credentials
                    system.Machine.ProtectedFiles.Symlinks
                    startDirectory
                    arguments.FinalSymlink
                    TrailingSeparatorPolicy.Demand
                    path
                    vfs
            with
            | Error (PathFailure.Refused refusal) -> Error (AccessRefusal.Path refusal)
            | Error (PathFailure.Errno error) -> Ok (Error error)
            | Ok resolution -> Ok (PathWalk.existingOf resolution.Target)

        match target with
        | Error refusal -> Error refusal
        | Ok (Error error) -> Ok (SyscallAnswer.Failed error)
        | Ok (Ok inode) ->

        if arguments.ExtendedRights <> 0 then
            Error (AccessRefusal.ExtendedRights (inode, arguments.ExtendedRights))
        else

        let entry =
            match VirtualFileSystem.tryGet inode vfs with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixPathResolution.faccessat: inode %O{inode} is not in the filesystem, but a path or a descriptor resolved to it. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

        let bits =
            match Inode.permissions entry with
            | InodePermissions.Stored bits -> bits
            | InodePermissions.PlatformSymlinkDefault -> SimulatedUnixPlatform.symlinkPermissions platform

        match
            AccessRules.denied
                (SimulatedUnixPlatform.privilegedExecution platform)
                (Standing.toward credentials entry.Owner)
                entry.Content
                bits
                arguments.Question
        with
        | Error refusal -> Error (AccessRefusal.UnmeasuredExecution (inode, refusal))
        | Ok true -> Ok (SyscallAnswer.Failed UnixError.EACCES)
        | Ok false -> Ok (SyscallAnswer.Completed 0L)

    /// <summary>
    /// <c>faccessat(2)</c>: whether the calling process may do what <c>mode</c> asks of
    /// the inode <c>path</c> names, starting from <c>dirfd</c> if the path is relative.
    /// </summary>
    /// <remarks>
    /// <c>dirfd</c>, <c>mode</c> and <c>flags</c> are raw, in this platform's own numbering;
    /// <c>AccessRules.screen</c> says which words each flavour rejects and what the rest
    /// mean. <c>path</c> is the argument's bytes, copied in after those screens, as both
    /// kernels do; a client that has yet to read them should call
    /// <c>faccessatScreenPhase</c> and <c>accessWithPath</c> instead.
    ///
    /// Without <c>AT_EACCESS</c> the path is walked, and the inode judged, with the process's
    /// <i>real</i> user and group (see <c>Credentials.realIdsAsEffective</c>); with it, with
    /// the effective ones, as every other syscall is. On Darwin the two are always the
    /// same, since <c>UnixBootImage.withCredentials</c> admits no Darwin process whose real
    /// and effective IDs differ.
    ///
    /// Answers 0 or the errno, and changes nothing: measured on both, it moves no
    /// timestamp. EACCES is <c>AccessRules.denied</c>'s; every other failure is the
    /// screens', the copy-in's, the dirfd's or the walk's. This library models one
    /// filesystem, writable and not mounted <c>noexec</c>, so the EROFS a read-only mount
    /// gives <c>W_OK</c> and the EACCES a <c>noexec</c> one gives <c>X_OK</c> on a regular
    /// file are never answered.
    ///
    /// Refuses Darwin's extended rights and the flags Darwin accepts without this library
    /// modelling them, a privileged caller's execute question under a flavour where that is
    /// unmeasured, and a <c>dirfd</c> naming neither a directory nor a regular file when the
    /// call would start from it; see <c>AccessRefusal</c>.
    /// </remarks>
    let faccessat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (dirfd : int)
        (path : PathArgumentBytes)
        (mode : int)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer, AccessRefusal>
        =
        match faccessatScreenPhase dirfd mode flags system with
        | Error refusal -> Error refusal
        | Ok (AccessProgress.Answered answer) -> Ok answer
        | Ok (AccessProgress.NeedsPath paused) -> accessWithPath path paused

    /// <c>access(2)</c>: <c>faccessat</c> from the current directory with no flags, so
    /// checking with the process's real user and group. Measured on both, the two agree on
    /// every mode word and every path the probe asked.
    let access<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer, AccessRefusal>
        =
        match accessScreenPhase mode system with
        | Error refusal -> Error refusal
        | Ok (AccessProgress.Answered answer) -> Ok answer
        | Ok (AccessProgress.NeedsPath paused) -> accessWithPath path paused
