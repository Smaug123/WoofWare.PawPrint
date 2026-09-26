namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// What a caller asked `open(2)` for, as facts about the open rather than as a
/// bit pattern.
///
/// Parsed rather than raw, unlike `mkdir`'s `mode`, because a bit pattern
/// lets this kernel *guess*: given an `int`, a flag it does not model is
/// indistinguishable from one it does, and it would silently do something the
/// caller did not ask for. A record has
/// exactly the fields this kernel acts on, so a caller can see what is
/// supported, and a flag that is not here is one the client had to decide about
/// before calling. An `int -> OpenFlags` decoder can be added later if a caller
/// wants one; it cannot be taken away once the surface is a number.
///
/// A caller holding a raw flag word answers two cases itself before calling,
/// an unrecognised bit and an access mode that is none of the three, because
/// neither is expressible once the flags are parsed.
type OpenFlags =
    {
        /// `O_RDONLY`, `O_WRONLY` or `O_RDWR`. A real `open` takes these as the
        /// low two bits and rejects the fourth combination; by the time the
        /// flags are a record there is no fourth combination to reject.
        Access : FileAccessMode
        /// `O_CREAT`: create the final component if nothing holds that name.
        Create : bool
        /// `O_EXCL`: fail EEXIST if the final component exists.
        ///
        /// Pass this exactly as the caller set it, **without** first ANDing it
        /// with `Create`. That it does nothing on its own is a measured kernel
        /// fact this library owns — `open(existing, O_WRONLY|O_EXCL)` succeeds
        /// and `open(missing, O_WRONLY|O_EXCL)` is ENOENT, exactly as without
        /// it — and a client that pre-combined them would be asserting the rule
        /// rather than exercising it.
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
        /// `O_CLOEXEC`. Accepted and ignored: it sets `FD_CLOEXEC`, which
        /// matters only across `exec`, and this kernel models neither `fork` nor
        /// `exec`. Here so that a caller can say it was asked for rather than
        /// having to drop it silently.
        CloseOnExec : bool
        /// `O_SYNC`. Accepted and ignored: it governs when a write reaches
        /// storage rather than whether it is visible, and this filesystem holds
        /// its bytes in memory, so every write is already as durable as the
        /// model gets. Here for the same reason as `CloseOnExec`.
        Synchronous : bool
        /// `O_DIRECTORY`: fail ENOTDIR unless the path names a directory.
        ///
        /// Modelled only as `opendir(3)` uses it: with `O_RDONLY`, and without
        /// `O_CREAT`, `O_TRUNC` or `O_NOFOLLOW`. `UnixNamespace.openPath`
        /// refuses every other combination, whose check order against those
        /// flags is unmeasured.
        Directory : bool
    }

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

[<RequireQualifiedAccess>]
module DirectoryEntryKind =
    /// What kind of entry a directory binding onto this content is.
    let ofContent (content : InodeContent) : DirectoryEntryKind =
        match content with
        | InodeContent.RegularFile _ -> DirectoryEntryKind.RegularFile
        | InodeContent.Directory _ -> DirectoryEntryKind.Directory
        | InodeContent.Symlink _ -> DirectoryEntryKind.Symlink

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

[<RequireQualifiedAccess>]
module ReadDirectoryRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic.
    let describe (refusal : ReadDirectoryRefusal) : string =
        match refusal with
        | ReadDirectoryRefusal.UnenumerablePosition (inode, offset) ->
            $"the description onto directory %O{inode} is at offset %d{offset}, where lseek put it. Both kernels accept that offset, but what the next read yields from it is each filesystem's own (tmpfs resumes from the entry with the greatest offset at or below it; APFS skips that many entries, or answers EAGAIN, depending on the high word), and this kernel's position is a name rather than an offset. Only offset 0, which rewinds, is readable from."

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

[<RequireQualifiedAccess>]
module UnixNamespace =

    /// `open(2)`: resolve `path`, apply every check a kernel makes, and return a
    /// descriptor onto what it names.
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
    /// Never refused: every outcome is a descriptor or an errno. The one
    /// exception is an `O_DIRECTORY` combination this library does not model;
    /// see `OpenFlags.Directory`.
    let openPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : OpenFlags)
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        let rules = SimulatedUnixPlatform.creatingOpenRules system.Machine.UnixPlatform
        let privilege = UnixProcessState.callerPrivilege system.Process

        // `O_EXCL` on its own is neither an error nor a refusal: both kernels
        // ignore it entirely, measured. So it is read
        // only where `Create` is set, and that combining is done here rather than
        // by the caller -- it is the kernel's rule, and a caller that pre-ANDed
        // the two would leave it with nothing to be right or wrong about.
        let exclusive = flags.Create && flags.Exclusive

        /// Hand out a descriptor onto `inode` for the access that was asked for.
        let opened
            (inode : InodeNumber)
            (system : UnixSystem<'Task, 'Handler>)
            : SyscallAnswer * UnixSystem<'Task, 'Handler>
            =
            // A directory gets a description positioned in its entries rather
            // than at a byte offset. It can only be here for reading: every
            // writable or truncating open of one answered EISDIR before this.
            let fd, registry =
                match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                | Some (InodeContent.Directory _) ->
                    FileDescriptorRegistry.openDirectory inode system.Process.FileDescriptors
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _)
                | None -> FileDescriptorRegistry.openFile inode flags.Access system.Process.FileDescriptors

            SyscallAnswer.Completed (int64 fd),
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        if flags.Directory then
            if
                flags.Access <> FileAccessMode.ReadOnly
                || flags.Create
                || flags.Truncate
                || flags.NoFollow
            then
                failwith
                    $"UnixNamespace.openPath: O_DIRECTORY with %A{flags}. Only O_DIRECTORY|O_RDONLY without O_CREAT, O_TRUNC or O_NOFOLLOW (what opendir(3) opens with) is modelled; where ENOTDIR falls among EISDIR, EACCES, ELOOP and the creation checks for any other combination is unmeasured."

            // `opendir(3)` is exactly this open, and its rows are measured on
            // both kernels, which agree in every one: a final symlink is
            // followed, a trailing separator changes nothing, being a file beats
            // being unreadable, and it is the read bit rather than the search
            // bit that is demanded. `OpenDirRules` holds them.
            match
                UnixPathResolution.resolvePathFull SymlinkPolicy.Follow TrailingSeparatorPolicy.Demand path system
            with
            | Error error -> SyscallAnswer.Failed error, system
            | Ok resolution ->

            match OpenDirRules.verdict privilege resolution system.Machine.FileSystem with
            | OpenDirVerdict.Refuse error -> SyscallAnswer.Failed error, system
            | OpenDirVerdict.Open inode -> opened inode system
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

        match UnixPathResolution.resolvePathFull policy trailingSeparatorPolicy path system with
        | Error error -> SyscallAnswer.Failed error, system
        | Ok resolution ->

        match
            CreatingOpenRules.verdict
                rules
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                privilege
                flags.Create
                exclusive
                resolution
                system.Machine.FileSystem
        with
        | CreatingOpenVerdict.Refuse error -> SyscallAnswer.Failed error, system
        | CreatingOpenVerdict.Create (directory, name) ->
            let permissions =
                CreatingOpenRules.createdPermissions rules system.Process.Umask mode

            let now = UnixMachineState.realtime system.Machine

            match
                VirtualFileSystem.createFile
                    directory
                    name
                    permissions
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
                |> opened inode
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
            SyscallAnswer.Failed UnixError.ELOOP, system
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
            SyscallAnswer.Failed UnixError.EISDIR, system
        | InodeContent.RegularFile _
        | InodeContent.Directory _ ->

        // A directory opens perfectly well for *reading*, measured on both. A
        // caller that wants to know what it opened asks `fstat`.
        let permissionBits =
            match Inode.permissions entry with
            | InodePermissions.Stored bits -> bits
            | InodePermissions.PlatformSymlinkDefault ->
                failwith
                    $"UnixNamespace.openPath: inode %O{inode} reports platform-default symlink permissions, but the symlink arm above answered ELOOP for every link (this is a bug in this library)."

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
        // Only the owner triple is ever consulted, and that is exact rather than
        // a simplification: `stat` reports the process's own `UserId` as *every*
        // inode's `st_uid`, so the emulated process owns everything it can see
        // and the group and other triples can never be the applicable ones.
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
        let denied =
            (FileAccessMode.permitsRead flags.Access
             && PermissionBits.deniedTo privilege AccessRequest.Read permissionBits)
            || ((FileAccessMode.permitsWrite flags.Access || flags.Truncate)
                && PermissionBits.deniedTo privilege AccessRequest.Write permissionBits)

        if denied then
            SyscallAnswer.Failed UnixError.EACCES, system
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
        // what the descriptor may still name rather than a filter.
        let system =
            match entry.Content with
            | InodeContent.RegularFile _ when flags.Truncate ->
                match UnixDescriptor.truncateAt inode 0L system with
                | Ok system -> system
                | Error refusal ->
                    // Truncating to zero cannot exceed a length limit and cannot
                    // be negative, which are the only two refusals `truncateAt`
                    // has.
                    failwith
                        $"UnixNamespace.openPath: truncating inode %O{inode} to zero was refused -- %s{TruncationRefusal.describe refusal} (this is a bug in this library)."
            | InodeContent.RegularFile _
            | InodeContent.Directory _
            | InodeContent.Symlink _ -> system

        opened inode system

    /// `readlink(2)`: report what the symbolic link at `path` points at.
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
    /// (`SimulatedUnixPlatform.readlinkCapacity`): EINVAL before resolution on
    /// Linux and for a negative size on Darwin, and zero bytes from a resolved
    /// link on Darwin for a size of zero.
    let readlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (destination : UserBuffer)
        (capacity : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadLinkAnswer, BufferRefusal>
        =
        let verdict =
            SimulatedUnixPlatform.readlinkCapacity system.Machine.UnixPlatform capacity

        match verdict with
        | ReadLinkCapacityVerdict.Refuse error -> Ok (ReadLinkAnswer.Failed error)
        | ReadLinkCapacityVerdict.ReportNothing
        | ReadLinkCapacityVerdict.Admit ->

        // `NoFollowFinal` is what makes this `readlink` rather than an expensive
        // way of asking about the target: a final symlink is the thing being
        // read, not something to step through. A trailing separator still
        // overrides that -- "lf/" demands that `lf` be a directory -- and the
        // resolver owns that rule, answering ENOTDIR.
        match UnixPathResolution.resolvePath SymlinkPolicy.NoFollowFinal path system with
        | Error error -> Ok (ReadLinkAnswer.Failed error)
        | Ok inode ->

        match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
        | None ->
            failwith
                $"UnixNamespace.readlink: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."
        | Some (InodeContent.Directory _)
        | Some (InodeContent.RegularFile _) ->
            // Not a link: EINVAL, which is what distinguishes "not a link" from
            // a failure to read one.
            // Decided before the destination is looked at, which is what a real
            // kernel does -- `vfs_readlink` refuses on the inode's operations
            // before it copies anything out. Measured on the host:
            // `readlink("f", (char*)8, 16)` is EINVAL, not EFAULT.
            Ok (ReadLinkAnswer.Failed UnixError.EINVAL)
        | Some (InodeContent.Symlink target) ->

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
        | UserBuffer.Opaque -> Error BufferRefusal.OpaqueAtTransfer
        | UserBuffer.Addressless -> Error BufferRefusal.AddresslessAtTransfer
        | UserBuffer.Mapped ->

        let all = UnixByteString.toBytes (SymlinkTarget.toByteString target)

        // Truncated in *bytes*, not in characters: a symlink target is a byte
        // string, and truncating by character count would write two bytes where
        // the caller allowed one for any non-ASCII target.
        if all.Length <= capacity then
            Ok (ReadLinkAnswer.Reported all)
        else
            Ok (ReadLinkAnswer.Reported (ImmutableArray.CreateRange (Seq.truncate capacity all)))

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

        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
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
        | OpenFileTarget.File _ ->
            let readable = FileAccessMode.permitsRead description.AccessMode
            Ok (ReadDirectoryAnswer.Failed (notADirectory readable true), system)
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.Socket _
        | OpenFileTarget.SocketEventPort _ -> Ok (ReadDirectoryAnswer.Failed (notADirectory true false), system)
        | OpenFileTarget.Directory (inode, position) ->

        let withPosition (position : DirectoryPosition) (system : UnixSystem<'Task, 'Handler>) =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.setDirectoryPosition fd position system.Process.FileDescriptors
                    }
            }

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

        let record : DirectoryRecord =
            {
                Inode = target
                Name = name
                Kind = kind
            }

        Ok (ReadDirectoryAnswer.Entry record, withPosition (DirectoryPosition.Cursor next) system)

    /// `mkdir(2)`: bind a new directory at `path`.
    ///
    /// `mode` is raw, exactly as the caller passed it, so what the created
    /// directory's permissions actually are depends on the umask and, on one
    /// flavour, on the parent's set-group-ID bit. `MkDirRules` holds that.
    ///
    /// Never refused: every outcome is a success or an errno, the rules having
    /// been measured on both flavours.
    let mkdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        let rules = SimulatedUnixPlatform.mkDirRules system.Machine.UnixPlatform

        // `NoFollowFinal` on both flavours: `mkdir` never dereferences the name
        // it is about to bind, so an existing link is EEXIST whether it dangles,
        // points at a file, or points at itself. The trailing separator is the
        // only thing that can reach past it, and only on Darwin — see
        // `MkDirRules.TrailingSeparator`.
        match UnixPathResolution.resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator path system with
        | Error error -> SyscallAnswer.Failed error, system
        | Ok resolution ->

        match
            MkDirRules.verdict
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                (UnixProcessState.callerPrivilege system.Process)
                resolution
                system.Machine.FileSystem
        with
        | MkDirVerdict.Refuse error -> SyscallAnswer.Failed error, system
        | MkDirVerdict.Create (directory, name, parentPermissions) ->

        let permissions =
            MkDirRules.createdPermissions rules parentPermissions system.Process.Umask mode

        let now = UnixMachineState.realtime system.Machine

        match VirtualFileSystem.createDirectory directory name permissions now system.Machine.FileSystem with
        | Error error ->
            // `createDirectory` refuses a name the directory already holds, and a
            // parent that is not a directory. The walk has just established
            // neither is the case, so either is a broken graph rather than
            // something the caller did.
            failwith
                $"UnixNamespace.mkdir: creating \"%s{DirectoryEntryName.toEscaped name}\" in inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and does not hold that name (this is a bug in this library)."
        | Ok (_, filesystem) ->

        SyscallAnswer.Completed 0L,
        { system with
            Machine =
                { system.Machine with
                    FileSystem = filesystem
                }
        }

    /// `unlink(2)`: remove the name `path`, and the inode it named if nothing
    /// else holds it.
    ///
    /// Never refused: every outcome is a success or an errno.
    let unlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        let rules = SimulatedUnixPlatform.unlinkRules system.Machine.UnixPlatform

        // `NoFollowFinal` on both flavours — `unlink` removes the name it was
        // given, never what that name points at. The trailing separator is the
        // only thing that can reach past a final symlink, and only on Darwin;
        // see `UnlinkRules.TrailingSeparator`.
        match UnixPathResolution.resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator path system with
        | Error error -> SyscallAnswer.Failed error, system
        | Ok resolution ->

        match
            UnlinkRules.verdict
                (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
                (UnixProcessState.callerPrivilege system.Process)
                resolution
                system.Machine.FileSystem
        with
        | UnlinkVerdict.Refuse error -> SyscallAnswer.Failed error, system
        | UnlinkVerdict.Remove (directory, name) ->

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
        SyscallAnswer.Completed 0L,
        UnixDescriptor.forgetIfUnheld
            target
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }

    /// `rmdir(2)`: remove the empty directory `path` names.
    ///
    /// Never refused: every outcome is a success or an errno.
    let rmdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        let rules = SimulatedUnixPlatform.rmDirRules system.Machine.UnixPlatform

        // `NoFollowFinal` on both flavours. The trailing separator is what
        // reaches past a final symlink, and only on Darwin — which is how
        // `rmdir("ld/")` removes the *link's target* there and is ENOTDIR on
        // Linux. See `RmDirRules.TrailingSeparator`.
        match UnixPathResolution.resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator path system with
        | Error error -> SyscallAnswer.Failed error, system
        | Ok resolution ->

        match
            RmDirRules.verdict
                (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
                (UnixProcessState.callerPrivilege system.Process)
                resolution
                system.Machine.FileSystem
        with
        | RmDirVerdict.Refuse error -> SyscallAnswer.Failed error, system
        | RmDirVerdict.Remove (directory, name) ->

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
        SyscallAnswer.Completed 0L,
        UnixDescriptor.forgetIfUnheld
            target
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }

    /// How a phase of `rename`'s two-path walk ended, when it did not produce a
    /// resolution.
    ///
    /// Two kinds, because the two are answered differently: an errno is what the
    /// caller is told, while a refusal is this kernel saying the bytes it was
    /// handed are not a pathname at all (see `PathArgumentRefusal`).
    [<RequireQualifiedAccess>]
    type private RenameStop =
        | Errno of error : UnixError
        | Refused of refusal : PathArgumentRefusal

    /// `getname()`: what the kernel learns when it copies one pathname in,
    /// before anything looks at what it says.
    ///
    /// The decode happens here rather than in the caller, and that is the point
    /// of taking bytes: a caller that decoded a pathname the syscall never
    /// copies in would refuse one `rename(2)` never read.
    let private copiedIn (limits : PathLimits) (argument : PathArgumentBytes) : Result<UnixPath, RenameStop> =
        match argument with
        | PathArgumentBytes.Unreadable -> Error (RenameStop.Errno UnixError.EFAULT)
        | PathArgumentBytes.Bytes bytes ->

        match PathArgument.parse limits bytes with
        | Error refusal -> Error (RenameStop.Refused refusal)
        | Ok (PathArgument.Failed error) -> Error (RenameStop.Errno error)
        | Ok (PathArgument.Parsed path) -> Ok path

    let private renameStopped
        (system : UnixSystem<'Task, 'Handler>)
        (stop : RenameStop)
        : Result<RenameProgress<'Task, 'Handler>, PathArgumentRefusal>
        =
        match stop with
        | RenameStop.Refused refusal -> Error refusal
        | RenameStop.Errno error -> Ok (RenameProgress.Answered (SyscallAnswer.Failed error, system))

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
        : Result<RenameProgress<'Task, 'Handler>, PathArgumentRefusal>
        =
        let rules = SimulatedUnixPlatform.renameRules system.Machine.UnixPlatform
        let limits = SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform

        let paused (progress : RenameSourceProgress) =
            Ok (
                RenameProgress.NeedsDestination
                    {
                        System = system
                        Rules = rules
                        SourceProgress = progress
                    }
            )

        match copiedIn limits source with
        | Error stop -> renameStopped system stop
        | Ok sourcePath ->

        // `NoFollowFinal` for both paths on both flavours — `rename` moves the
        // name it was given, never what that name points at. The trailing
        // separator is the only thing that reaches past a final symlink, and
        // only on Darwin; see `RenameRules.TrailingSeparator`.
        match rules.WalkOrder with
        | RenameWalkOrder.ParentsThenFinals ->
            match
                UnixPathResolution.resolvePathParent
                    SymlinkPolicy.NoFollowFinal
                    rules.TrailingSeparator
                    sourcePath
                    system
            with
            | Error error -> renameStopped system (RenameStop.Errno error)
            | Ok parent -> paused (RenameSourceProgress.ParentWalked parent)
        | RenameWalkOrder.SourceThenDestination ->

        match
            UnixPathResolution.resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator sourcePath system
        with
        | Error error -> renameStopped system (RenameStop.Errno error)
        | Ok sourceResolution ->

        // Darwin's source-side `namei` runs under rename semantics, so two of
        // the refusals the verdict would otherwise make are settled here —
        // before the destination's pathname has been read at all.
        match RenameRules.sourceScreen rules.WalkOrder sourceResolution with
        | Some error -> renameStopped system (RenameStop.Errno error)
        | None -> paused (RenameSourceProgress.Resolved sourceResolution)

    /// The rest of `rename(2)`, given the destination pathname the kernel has
    /// just reached the point of copying in.
    ///
    /// Never refused as a *syscall*: every outcome is a success or an errno. The
    /// `Result` is for the one thing that is not either — a pathname whose bytes
    /// name a file this kernel cannot represent.
    let renameWithDestination<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : PathArgumentBytes)
        (paused : PausedRename<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, PathArgumentRefusal>
        =
        match box paused with
        | null ->
            failwith
                "UnixNamespace.renameWithDestination: this paused rename is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from UnixNamespace.renameSourcePhase instead."
        | _ ->

        let system = paused.System
        let rules = paused.Rules
        let limits = SimulatedUnixPlatform.pathLimits system.Machine.UnixPlatform

        let resolved : Result<Resolution * Resolution, RenameStop> =
            match copiedIn limits destination with
            | Error stop -> Error stop
            | Ok destinationPath ->

            match paused.SourceProgress with
            | RenameSourceProgress.Resolved sourceResolution ->
                // Darwin: the source is already finished, so the destination is
                // resolved to completion and the verdict judges the pair.
                UnixPathResolution.resolvePathFull
                    SymlinkPolicy.NoFollowFinal
                    rules.TrailingSeparator
                    destinationPath
                    system
                |> Result.mapError RenameStop.Errno
                |> Result.map (fun destinationResolution -> sourceResolution, destinationResolution)
            | RenameSourceProgress.ParentWalked sourceParent ->

            // Linux: the destination's parent, then both final lookups.
            match
                UnixPathResolution.resolvePathParent
                    SymlinkPolicy.NoFollowFinal
                    rules.TrailingSeparator
                    destinationPath
                    system
            with
            | Error error -> Error (RenameStop.Errno error)
            | Ok destinationParent ->

            // Source before destination, and here the order *is* pinned: the
            // orphan check below sits between the two, so a 300-byte source name
            // is ENAMETOOLONG while a 300-byte destination name under the same
            // orphaned parent is ENOENT. Measured both ways.
            match PathWalk.completeResolution sourceParent with
            | Error error -> Error (RenameStop.Errno error)
            | Ok sourceResolution ->

            // Linux's source screen runs here: after both parents and the
            // source's own final lookup, and before the destination's. It beats
            // the orphan check below — `rename("d/.", "x")` from an orphaned
            // current directory is EBUSY where every other source there is
            // ENOENT — and it beats the destination's NAME_MAX, which is what
            // makes `rename("nope", <300-byte name>)` ENOENT.
            match RenameRules.sourceScreen rules.WalkOrder sourceResolution with
            | Some error -> Error (RenameStop.Errno error)
            | None ->

            // A destination parent that has lost its own last name — reachable
            // only as an `rmdir`'d current directory — is ENOENT here, *before*
            // the destination's final name is measured. Both verdicts also
            // refuse it, and on Darwin that is where it is caught, after the
            // whole destination has resolved: measured, the same call is
            // ENAMETOOLONG there. So this is the Linux position of a check both
            // flavours make, not a check only Linux makes.
            if PathWalk.pausedParentIsOrphaned destinationParent then
                Error (RenameStop.Errno UnixError.ENOENT)
            else

            PathWalk.completeResolution destinationParent
            |> Result.mapError RenameStop.Errno
            |> Result.map (fun destinationResolution -> sourceResolution, destinationResolution)

        match resolved with
        | Error (RenameStop.Refused refusal) -> Error refusal
        | Error (RenameStop.Errno error) -> Ok (SyscallAnswer.Failed error, system)
        | Ok (sourceResolution, destinationResolution) ->

        match
            RenameRules.verdict
                (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
                (SimulatedUnixPlatform.bindableEntryNames system.Machine.UnixPlatform)
                (UnixProcessState.callerPrivilege system.Process)
                sourceResolution
                destinationResolution
                system.Machine.FileSystem
        with
        | RenameVerdict.Refuse error -> Ok (SyscallAnswer.Failed error, system)
        // Both paths name one inode: a success that changes nothing at all, not
        // a binding and not a timestamp. Deliberately not routed through
        // `VirtualFileSystem.rename`, which refuses it — the graph primitive
        // would have to invent a no-op stamp to express it.
        | RenameVerdict.NoOp -> Ok (SyscallAnswer.Completed 0L, system)
        | RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName) ->

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
            // guest did anything unusual.
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
            | Some displaced -> UnixDescriptor.forgetIfUnheld displaced moved
        )

    /// `rename(2)` in one call, for a caller holding both pathnames already —
    /// every caller but the one reading them out of a guest's memory, where
    /// reading the destination too early is itself observable.
    let rename<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (source : PathArgumentBytes)
        (destination : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, PathArgumentRefusal>
        =
        match renameSourcePhase source system with
        | Error refusal -> Error refusal
        | Ok (RenameProgress.Answered (answer, system)) -> Ok (answer, system)
        | Ok (RenameProgress.NeedsDestination paused) -> renameWithDestination destination paused
