namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// What `stat(2)` and its siblings report about one inode.
///
/// The POSIX fields as *facts*, not as any particular `struct stat`'s bytes:
/// which struct a client fills in, at what offsets, and in what order is the
/// client's ABI rather than this kernel's, and a client that wrote these
/// straight out in declaration order would be filling in its own struct rather
/// than the one its runtime declares.
///
/// The fields this kernel does not model are absent rather than zeroed, so that
/// a client is not handed a zero it might mistake for a measurement: there is no
/// `st_nlink`, no `st_blksize`, no `st_blocks` and no BSD `st_flags` here. A
/// client whose ABI has those fields writes what its own runtime would write for
/// a filesystem that has no such notion.
type FileStatus =
    {
        /// `st_mode`: the file-type band and the permission band together, as
        /// `stat(2)` reports them and in the numbering `S_IFMT` uses. Composed
        /// here rather than by the client, so that the two bands are assembled
        /// in exactly one place.
        Mode : int
        /// `st_uid` and `st_gid`.
        ///
        /// The *calling process's* ids, always: this kernel stores no per-inode
        /// ownership, so every file it holds is owned by whoever is asking. A
        /// client must not read these as a claim that ownership was recorded.
        UserId : uint32
        GroupId : uint32
        /// `st_size`. For a symbolic link this is the target's length in bytes,
        /// which is what `readlink(2)` would copy out.
        Size : int64
        /// `st_atim`. Never moves in this kernel: no operation records an
        /// access, so this is the inode's creation time until something sets it.
        AccessTime : UnixTimestamp
        /// `st_mtim`.
        ModificationTime : UnixTimestamp
        /// `st_ctim`.
        StatusChangeTime : UnixTimestamp
        /// `st_birthtim`, or `None` on a flavour whose `stat(2)` has no such
        /// field.
        ///
        /// The inode knows when it was born either way; this says whether the
        /// *platform being simulated* would tell a guest. `None` is what a Linux
        /// guest sees, and a client with a birth-time field writes whatever its
        /// own runtime writes when the kernel did not supply one.
        BirthTime : UnixTimestamp option
        /// `st_dev`.
        DeviceId : int64
        /// `st_ino`.
        Inode : InodeNumber
    }

/// Why this kernel will not report a `struct stat` for a descriptor.
///
/// One genus, three shapes, and it is a limit of the model rather than a
/// measured absence of an answer: real kernels answer `fstat` for all three of
/// these quite happily. What this kernel has not got is an *inode* to report
/// them from, and every field would therefore be invented.
[<RequireQualifiedAccess>]
type FStatRefusal =
    /// A standard stream, which this kernel models as one end of a pipe.
    | StandardStream of role : FileDescriptorRole
    /// A socket event port: an anonymous kernel object.
    | SocketEventPort
    /// A socket, which has an identity here but not an inode-shaped one.
    | Socket of socket : SocketId

[<RequireQualifiedAccess>]
module FStatRefusal =
    /// What this kernel knows about why it cannot report a status. The client
    /// supplies its own half — which entry point, which descriptor, and what it
    /// would have to build to lift the refusal.
    let describe (refusal : FStatRefusal) : string =
        match refusal with
        | FStatRefusal.StandardStream role ->
            $"the descriptor is standard stream %O{role}, which this kernel models as one end of a pipe and holds no inode for. A real kernel answers here — S_IFIFO, a zero size, a device number — and every one of those fields would be invented, with nothing able to say the invention was wrong."
        | FStatRefusal.SocketEventPort ->
            "the descriptor is a socket event port, an anonymous kernel object this kernel holds no inode for. Measured, the two flavours share not one field, and Linux's identity fields are facts about the machine that produced them rather than portable ones: Linux gives `st_mode` 0600 (permission bits and *no* file-type bits), `st_nlink` 1, `st_blksize` 4096, and a real anon-inode `st_dev`/`st_ino`; Darwin gives `st_mode` S_IFIFO (no permission bits), `st_nlink` 0, `st_blksize` 32, and zero for both identity fields."
        | FStatRefusal.Socket socket ->
            $"the descriptor is socket %O{socket}, for which this kernel holds no inode — a `SocketId` is a contention key rather than an inode number. Measured, only Linux gives a socket an inode at all (`st_dev` 8 and a distinct `st_ino` per socket, on `sockfs`), a Darwin AF_INET socket reporting 0 for both; and the rest would be invented either way — `st_mode` is S_IFSOCK|0777 on Linux against S_IFSOCK|0666 on Darwin, `st_nlink` 1 against 0, and Darwin's `st_blksize` varies with the socket itself (131072 for TCP, 9216 for UDP, 8192 for a Unix-domain socket)."

/// What `fstat(2)` reported, for a descriptor this kernel could answer for.
[<RequireQualifiedAccess>]
type FileStatusAnswer =
    /// The status of the inode the descriptor names.
    | Reported of status : FileStatus
    /// The entry point returns -1, stores `error` wherever its libc keeps errno,
    /// and — measured on both flavours, and what `ConvertFileStatus` in
    /// `pal_io.c` relies on — leaves the caller's output struct untouched.
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

[<RequireQualifiedAccess>]
module UnixPathResolution =

    /// The full result of walking `path`, which a caller that must distinguish
    /// "the name exists" from "the name is free in a directory that exists"
    /// needs — `open` with `O_CREAT`, `rename`, `link`. Callers that only want
    /// the inode use `resolvePath`.
    ///
    /// Relative paths start at the process's current directory *inode*, not at a
    /// re-walk of its path.
    let resolvePathFull<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Resolution, UnixError>
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
            (UnixProcessState.callerPrivilege system.Process)
            system.Process.CurrentDirectoryInode
            policy
            trailingSeparatorPolicy
            path
            system.Machine.FileSystem

    /// `resolvePathFull`, stopped at the directory holding the final name.
    ///
    /// Only `rename` wants this, and only under Linux's walk order: it resolves
    /// *both* paths' parents before it looks either final component up, which
    /// no pair of `resolvePathFull` calls can express. Finish one with
    /// `PathWalk.completeResolution`.
    let resolvePathParent<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PausedResolution, UnixError>
        =
        PathWalk.resolveParent
            (SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform)
            (UnixProcessState.callerPrivilege system.Process)
            system.Process.CurrentDirectoryInode
            policy
            trailingSeparatorPolicy
            path
            system.Machine.FileSystem

    /// The inode a path names, or the errno the lookup owes the caller — what
    /// every non-creating caller wants.
    ///
    /// Shares `resolvePathFull`'s walk and `PathWalk.existingOf`'s
    /// free-name-is-ENOENT rule, rather than restating either.
    let resolvePath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<InodeNumber, UnixError>
        =
        resolvePathFull policy TrailingSeparatorPolicy.Demand path system
        |> Result.bind (fun resolution -> PathWalk.existingOf resolution.Target)

    /// The status of an inode this filesystem holds, or `None` if it holds no
    /// such inode.
    ///
    /// The whole of what a `stat`-family syscall reports; the syscalls differ
    /// only in how they reach the inode. `fstat` is this plus a descriptor
    /// lookup, and `stat`/`lstat` are this plus a path resolution.
    let statOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (system : UnixSystem<'Task, 'Handler>)
        : FileStatus option
        =
        match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
        | None -> None
        | Some entry ->

        let permissions =
            match Inode.permissions entry with
            | InodePermissions.Stored bits -> bits
            | InodePermissions.PlatformSymlinkDefault ->
                SimulatedUnixPlatform.symlinkPermissions system.Machine.UnixPlatform

        let size =
            match entry.Content with
            | InodeContent.RegularFile (contents, _) -> int64 contents.Length
            // `readlink` reports the target's byte length as the link's size,
            // and a guest can see it through a file-length API.
            | InodeContent.Symlink target -> int64 (SymlinkTarget.toUtf8 target).Length
            // Invented, and the only field here that is: this kernel has no
            // block allocator, so a directory has no natural size. 4096 is what
            // ext4 reports for a small directory, i.e. the least surprising
            // answer a guest could read.
            | InodeContent.Directory _ -> 4096L

        let birthTime =
            // Withheld rather than reported when the platform has no
            // `st_birthtime`. The inode knows its birth either way; this governs
            // only what a guest is told.
            if SimulatedUnixPlatform.reportsBirthTime system.Machine.UnixPlatform then
                Some entry.Times.Birth
            else
                None

        Some
            {
                Mode = InodeContent.fileTypeBits entry.Content ||| PermissionBits.toInt permissions
                // The calling process's, this kernel storing no per-inode
                // ownership. See `FileStatus.UserId`.
                UserId = system.Process.UserId
                GroupId = system.Process.GroupId
                Size = size
                AccessTime = entry.Times.Access
                ModificationTime = entry.Times.Modification
                StatusChangeTime = entry.Times.StatusChange
                BirthTime = birthTime
                DeviceId = VirtualFileSystem.deviceId
                Inode = inode
            }

    /// `stat(2)` and `lstat(2)`: report the status of the inode `path` names,
    /// the two differing only in whether a symbolic link in the final position
    /// is followed.
    ///
    /// Changes nothing and returns no system, for the reason `fstat` does not:
    /// a `stat` records no access.
    ///
    /// Cannot be refused, unlike `fstat`. Every inode a path resolves to is one
    /// this filesystem holds — a name for an inode-free object cannot be created
    /// in it — so the three descriptors `fstat` refuses for are unreachable from
    /// here.
    let stat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : FileStatusAnswer
        =
        match resolvePath policy path system with
        | Error error -> FileStatusAnswer.Failed error
        | Ok inode ->

        match statOf inode system with
        | Some status -> FileStatusAnswer.Reported status
        | None ->
            failwith
                $"UnixPathResolution.stat: resolving %O{path} returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

    /// `fstat(2)`: report the status of the inode `fd` names.
    ///
    /// Changes nothing and returns no system, which is not merely today's
    /// implementation: a real `fstat` records no access, and neither does this
    /// one, so there is nothing for a caller to write back.
    ///
    /// Refuses for a descriptor this kernel holds no inode for — the standard
    /// streams, a socket event port, a socket. That is a limit of the model
    /// rather than an absent kernel answer; see `FStatRefusal`.
    let fstat<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileStatusAnswer, FStatRefusal>
        =
        match FileDescriptorRegistry.tryFindObject fd system.Process.FileDescriptors with
        | None -> Ok (FileStatusAnswer.Failed UnixError.EBADF)
        | Some (OpenFileObject.StandardStream role) -> Error (FStatRefusal.StandardStream role)
        | Some OpenFileObject.AnonymousInode -> Error FStatRefusal.SocketEventPort
        | Some (OpenFileObject.Socket socketId) -> Error (FStatRefusal.Socket socketId)
        | Some (OpenFileObject.File inode) ->

        match statOf inode system with
        | Some status -> Ok (FileStatusAnswer.Reported status)
        | None ->
            failwith
                $"UnixPathResolution.fstat: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

    /// The absolute path of the directory the process is standing in, or `None`
    /// if no path reaches it any more.
    ///
    /// `None` is the state a process enters when the directory it is in is
    /// removed out from under it. It is not an error and not a latch: relative
    /// paths still resolve (they start from the inode, which the process holds),
    /// and stepping out with `chdir("..")` gives the process a path again.
    /// Measured on both flavours; see docs/probes/chdir.
    let currentDirectoryPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : AbsoluteUnixPath option
        =
        VirtualFileSystem.pathOfDirectory system.Process.CurrentDirectoryInode system.Machine.FileSystem

    /// `getcwd(3)`: report the current directory's path into the caller's buffer.
    ///
    /// Changes nothing and returns no system, for the reason `fstat` gives.
    ///
    /// `capacity` is the caller's buffer size and must not be negative: a
    /// negative size is not a value any `getcwd` sees, since the C library takes
    /// a `size_t`. Rejecting one is the PAL shim's own guard, and stays with the
    /// client that holds the shim's signature.
    ///
    /// The whole measured ordering lives here, and the destination's
    /// classification is consulted last on both flavours — a too-small buffer is
    /// ERANGE whatever the destination is, and a removed current directory
    /// outranks even that on Linux.
    let getcwd<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : UserBuffer)
        (capacity : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetCwdAnswer, GetCwdRefusal>
        =
        if capacity < 0 then
            failwith
                $"UnixPathResolution.getcwd: capacity %d{capacity} is negative, which no `getcwd(3)` can be asked for -- its size argument is a `size_t`. Screen this in the client, where the signature that admits a negative number lives (this is a bug in the caller)."

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
        if capacity = 0 then
            Ok (GetCwdAnswer.Failed UnixError.EINVAL)
        elif capacity >= 2 && storeWouldBeFatal then
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
                if capacity < 2 then
                    Ok (GetCwdAnswer.Failed UnixError.ERANGE)
                else
                    Ok (GetCwdAnswer.Failed UnixError.ENOENT)
        | Some path ->

        /// The bytes a successful call would place, terminator included. Also
        /// what the comparison producing ERANGE is made against, so the two
        /// cannot disagree about whether the path fits.
        let terminated : ImmutableArray<byte> = (AbsoluteUnixPath.toUtf8 path).Add 0uy

        if capacity < terminated.Length then
            // `getcwd` needs room for the path *and* its NUL, which is why a
            // buffer of the path's own length is one byte short rather than an
            // exact fit. Measured with an unwritable destination too: on the
            // flavour that copies from the kernel this answers before the
            // destination is looked at, `getcwd((char*)123, 1)` being ERANGE
            // rather than EFAULT.
            Ok (GetCwdAnswer.Failed UnixError.ERANGE)
        else
            transfer (GetCwdAnswer.Reported terminated)

    /// `chdir(2)`: make `path` the directory relative paths resolve from.
    ///
    /// Never refused: every outcome is a success or an errno.
    ///
    /// The one filesystem syscall here with no flavour divergence, so it takes
    /// no rules record. Measured on both kernels across object type, final
    /// symlink following, trailing separator, which permission bit, name length,
    /// navigation, and the current directory removed underneath the process —
    /// every row identical. See `docs/probes/chdir/`.
    let chdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        // `Follow`, which carries `TrailingSeparatorPolicy.Demand`. That one
        // call is most of this syscall's error surface: ENOENT for a name that
        // is not there and for a dangling link, ENOTDIR for a regular file and
        // for "f/", ENAMETOOLONG for an over-long component, ELOOP for a cycle —
        // and it follows "ld" to what it names, which is why `getcwd` afterwards
        // reports the target rather than the link.
        match resolvePath SymlinkPolicy.Follow path system with
        | Error error -> SyscallAnswer.Failed error, system
        | Ok target ->

        match VirtualFileSystem.tryGetContent target system.Machine.FileSystem with
        | None ->
            failwith
                $"UnixPathResolution.chdir: the walk resolved \"%s{UnixPath.toString path}\" to inode %O{target}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        // Reached by a symbolic link to a regular file as well as by a plain
        // one: `Follow` lands on the file, and only then is it a type error.
        | Some (InodeContent.RegularFile _) -> SyscallAnswer.Failed UnixError.ENOTDIR, system
        | Some (InodeContent.Symlink _) ->
            // Unreachable, and asserted rather than answered: `Follow` traverses
            // a final symlink, so the walk above cannot hand back a link — a
            // chain that never terminates is ELOOP and one that ends nowhere is
            // ENOENT, both refused before here. Answering ENOTDIR instead would
            // be a plausible-looking reply from a walk that had stopped doing
            // what this syscall asked of it. Found by mutation: the arm was
            // dead, so nothing could tell the two apart.
            failwith
                $"UnixPathResolution.chdir: the walk resolved \"%s{UnixPath.toString path}\" to inode %O{target}, which is a symbolic link -- but it ran under SymlinkPolicy.Follow, which never finishes on one (this is a bug in this library)."
        | Some (InodeContent.Directory directory) ->

        // The *search* bit, and not the read bit: measured on both kernels, a
        // 0o100 directory can be entered and a 0o400 one is EACCES. That is the
        // opposite way round from `opendir`, which wants read — the second place
        // the two have come apart.
        //
        // The walk above checks search on every directory it *traverses*; this
        // is the target's own bit, which nothing has asked about yet.
        if
            PermissionBits.deniedTo
                (UnixProcessState.callerPrivilege system.Process)
                AccessRequest.SearchDirectory
                directory.Permissions
        then
            SyscallAnswer.Failed UnixError.EACCES, system
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
        // directory a guest `rmdir`d before stepping out of it becomes free
        // exactly here. Without this it would be stranded for the run.
        SyscallAnswer.Completed 0L, UnixDescriptor.forgetIfUnheld previous moved
