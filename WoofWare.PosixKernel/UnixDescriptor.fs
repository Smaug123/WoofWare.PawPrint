namespace WoofWare.PosixKernel

/// Which of the two non-portable `lseek` extensions a raw `whence` names on the
/// simulated platform. Linux numbers `SEEK_DATA` 3 and `SEEK_HOLE` 4; Darwin
/// transposes them, which is why the raw number alone names no operation.
[<RequireQualifiedAccess>]
type SeekExtension =
    | SeekData
    | SeekHole

/// What `lseek`'s `SEEK_END` counts from on one descriptor.
[<RequireQualifiedAccess>]
type private SeekEndBasis =
    /// The seek is `SEEK_SET` or `SEEK_CUR`, which never ask.
    | NotConsulted
    | Size of size : int64
    /// The file takes no `SEEK_END` at all, whatever the offset.
    | Fails of error : UnixError

/// Why this kernel will not answer an `lseek`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type LSeekRefusal =
    /// `SEEK_DATA` or `SEEK_HOLE` on a seekable file.
    | Sparseness of whence : int * meaning : SeekExtension
    /// `SEEK_END` on a directory whose size this kernel cannot state: one on an
    /// NFS mount.
    | DirectoryEnd of inode : InodeNumber
    /// `SEEK_CUR` on a directory that has been read part of the way through.
    ///
    /// Both kernels answer, but with a value only their own filesystem mints:
    /// tmpfs reports the offset of the next entry it will yield, APFS a count of
    /// entries returned under a per-directory sequence number in the high 32
    /// bits. This kernel's position is a name, and no number stands for it.
    | DirectoryPosition of inode : InodeNumber

[<RequireQualifiedAccess>]
module LSeekRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic. The client supplies its own half — which entry point, which
    /// descriptor — because those are things it decoded and this library never
    /// saw.
    let describe (refusal : LSeekRefusal) : string =
        match refusal with
        | LSeekRefusal.Sparseness (whence, meaning) ->
            let named =
                match meaning with
                | SeekExtension.SeekData -> "SEEK_DATA"
                | SeekExtension.SeekHole -> "SEEK_HOLE"

            $"whence %d{whence} is %s{named} on the simulated platform. This kernel models file contents as a byte array with no notion of sparseness, so it cannot say where the data and holes are; and the two platforms transpose the numbers (3 is SEEK_DATA on Linux and SEEK_HOLE on Darwin), so the raw value does not name one operation."
        | LSeekRefusal.DirectoryEnd inode ->
            $"inode %O{inode} is a directory on an NFS mount, and was asked to seek relative to its end. An NFS directory's size is whatever the server's own filesystem reports, which nothing in this machine determines, so this kernel cannot say where the end is. SEEK_SET and SEEK_CUR on a directory are portable and are supported, as is SEEK_END on a tmpfs or APFS directory."
        | LSeekRefusal.DirectoryPosition inode ->
            $"inode %O{inode} is a directory that this description has read part of the way through, and SEEK_CUR asks where it is. A real kernel answers with its own filesystem's resumption cookie (the next entry's offset on tmpfs; a sequence number and an entry count on APFS), which is not a number this kernel's position corresponds to. SEEK_CUR is answered at the start of a directory, and after a SEEK_SET or SEEK_END moved the description."

/// Why this kernel will not answer an `flock`.
///
/// Every case but `Interruption` is a measured divergence between the two
/// flavours that this library models Linux's side of. Darwin's `flock` is
/// unmodelled not because its return codes are unknown — they are measured,
/// and named in each case's description — but because what they leave the
/// *lock state* as is not, which is what a model would have to commit to.
[<RequireQualifiedAccess>]
type FLockRefusal =
    /// Not exactly one of LOCK_SH/LOCK_EX/LOCK_UN, optionally with LOCK_NB.
    | DarwinMalformedOperation of operation : int
    /// A pipe.
    | DarwinPipe of pipe : PipeId
    /// A kqueue.
    | DarwinKqueue
    | DarwinSocket of socket : SocketId
    /// An acquire by a description that already holds a lock. Only a conversion
    /// can expose the keep-versus-drop divergence, and only when it fails —
    /// refused on the request rather than on the outcome, so that the refusal is
    /// a property of what was asked rather than of who else held a lock.
    | DarwinConversion
    /// The acquisition was asleep and a signal is pending for the task, and this
    /// library will not say how the signal ends it.
    | Interruption of SyscallInterruptionRefusal

[<RequireQualifiedAccess>]
module FLockRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, and which of its callers could have
    /// reached it.
    let describe (refusal : FLockRefusal) : string =
        match refusal with
        | FLockRefusal.DarwinMalformedOperation operation ->
            $"operation %d{operation} is malformed (not exactly one of LOCK_SH/LOCK_EX/LOCK_UN, optionally with LOCK_NB), which Linux rejects with EINVAL unless LOCK_MAND (bit 32) is set, in which case it ignores the request and answers 0, and which Darwin does not treat uniformly -- measured, Darwin answers EBADF for 0, a bare LOCK_NB and unknown bits alone, but *succeeds* for LOCK_SH|LOCK_EX, LOCK_UN|LOCK_SH and LOCK_SH with an unknown bit."
        | FLockRefusal.DarwinPipe pipe ->
            $"the descriptor is an end of pipe %O{pipe}. Linux permits `flock` on a pipe, with both ends contending as one object; Darwin refuses it with ENOTSUP (raw 45, and note Darwin numbers ENOTSUP and EOPNOTSUPP differently, 45 against 102, while Linux gives both 95) on every pipe."
        | FLockRefusal.DarwinKqueue ->
            "the descriptor is a kqueue. Darwin refuses `flock` on one with ENOTSUP (raw 45), for every operation including LOCK_UN, where Linux permits it on an epoll descriptor and returns 0."
        | FLockRefusal.DarwinSocket socket ->
            $"the descriptor is socket %O{socket}. Linux permits `flock` on a socket and returns 0; Darwin refuses it with ENOTSUP (raw 45)."
        | FLockRefusal.DarwinConversion ->
            "the descriptor is converting a lock it already holds. Should that conversion fail, Linux leaves the description holding *nothing* (`flock` removes the old lock before establishing the new one, and the two steps are not atomic) while Darwin leaves the old lock in place -- measured on both, and indistinguishable from the return code, which is EWOULDBLOCK either way."
        | FLockRefusal.Interruption refusal -> SyscallInterruptionRefusal.describe refusal

/// Why this kernel will not answer a `posix_fadvise(2)`.
[<RequireQualifiedAccess>]
type PosixFadviseRefusal =
    /// The simulated platform's libc has no `posix_fadvise`
    /// (`SimulatedUnixPlatform.providesPosixFadvise` is `false`), so no program
    /// could have made the call and there is no answer to give.
    | NotProvided

/// What `posix_fadvise(2)` answered.
///
/// Not `SyscallAnswer`, because `posix_fadvise` reports failure differently from
/// every other call here: it *returns* the error number and leaves errno
/// untouched, which is why the failure case names an error but no sentinel for a
/// caller to return.
[<RequireQualifiedAccess>]
type FileAdviceAnswer =
    /// The hint was accepted. It changes nothing this kernel represents; a real
    /// kernel would adjust its readahead.
    | Completed
    /// The call returns `error` — as a positive number, not through errno.
    | Failed of error : UnixError

/// Why this kernel will not commit a truncation.
[<RequireQualifiedAccess>]
type TruncationRefusal =
    /// Longer than this kernel can represent. A real filesystem answers without
    /// difficulty — measured on ext4 and APFS alike, `ftruncate` to three
    /// gigabytes succeeds and leaves a sparse file — so this is a limit of the
    /// model, and refusing beats reporting an errno no kernel would produce for
    /// that length.
    | ExceedsRepresentableLength of inode : InodeNumber * length : int64
    /// What truncating the file at `inode` would do to its set-ID bits has
    /// not been measured for this process.
    | UnmeasuredSetIdChange of inode : InodeNumber * refusal : SetIdChangeRefusal

[<RequireQualifiedAccess>]
module TruncationRefusal =
    let describe (refusal : TruncationRefusal) : string =
        match refusal with
        | TruncationRefusal.ExceedsRepresentableLength (inode, length) ->
            $"inode %O{inode} was asked to become %d{length} bytes, which is longer than the %d{VirtualFileSystem.maxFileLength} bytes this kernel can represent. A real filesystem answers this without difficulty -- measured on ext4 and APFS alike, ftruncate to three gigabytes succeeds and leaves a sparse file -- so this is a limit of the model, and refusing is better than reporting an errno no kernel would have produced for that length."
        | TruncationRefusal.UnmeasuredSetIdChange (inode, refusal) ->
            $"truncating inode %O{inode}: %s{SetIdChangeRefusal.describe refusal}"

/// Why this kernel will not close a descriptor.
///
/// Generic in what names a task because most of them are about a task parked
/// in a wait, and which one that is cannot be recomputed by the client:
/// nothing stops two tasks parking on the same port, so a client repeating the
/// search could name a different one from the one this refusal is about.
///
/// Under Linux a sleeping call holds the description it sleeps on, so a close
/// under it is served: the description outlives its last descriptor until the
/// call returns. The Darwin cases are the flavour's own answers, which end or
/// hold up the sleeping call in ways this kernel does not model. A `kevent`
/// asleep on a kqueue is not among them: the close of the descriptor it was
/// entered through drains the kqueue (`KqueueState.Drained`), which is
/// modelled, so it is served.
[<RequireQualifiedAccess>]
type CloseRefusal<'Task> =
    /// Releasing the description destroys an object in a state this kernel
    /// has not measured.
    | Release of DescriptionReleaseRefusal
    /// Any descriptor onto an open file description that `task` is parked on
    /// an `flock` of, under the Darwin flavour.
    | DarwinFlockedDescriptorWithWaiter of description : OpenFileDescriptionId * task : 'Task
    /// The descriptor `fd`, which `task` is parked in a `poll(2)` watching.
    ///
    /// Refused under either flavour. Linux's sleeping poll keeps the file it
    /// found, which this kernel represents, but when it wakes it looks the
    /// number up again and reports what the number names by then, and it is
    /// woken only by the files it found: a wake that finds nothing ready under
    /// the number sleeps again until the next one. This kernel's wake
    /// conditions are levels, not edges, so such a poll would be woken again
    /// at once, for ever.
    | PolledDescriptor of fd : int * task : 'Task
    /// Any descriptor onto a listening socket that `task` is parked in an
    /// `accept(2)` on, under the Darwin flavour.
    | DarwinListenerDescriptorWithAccepter of listener : OpenFileDescriptionId * task : 'Task
    /// Any descriptor onto a pipe end that `task` is asleep in a `read` or
    /// `write` through, under the Darwin flavour.
    | DarwinPipeDescriptorWithTransfer of description : OpenFileDescriptionId * task : 'Task

[<RequireQualifiedAccess>]
module CloseRefusal =
    /// What this kernel knows about why it cannot close the descriptor. The
    /// client supplies its own half — which entry point, which descriptor
    /// number, and what it would have to build to lift the refusal.
    let describe (refusal : CloseRefusal<'Task>) : string =
        match refusal with
        | CloseRefusal.DarwinFlockedDescriptorWithWaiter (description, task) ->
            $"the descriptor names open file description %O{description}, and task %O{task} is parked on an `flock` of it. Measured on Darwin (open-file-references.c section D), closing the descriptor the flock was entered through does not return until the flock has, whether or not a dup keeps the description: the close blocks until the lock is granted or a signal ends the flock. This kernel models no close that sleeps, nor which descriptor a call was entered through."
        | CloseRefusal.PolledDescriptor (fd, task) ->
            $"task %O{task} is parked in a poll(2) watching fd %d{fd}. Measured on Linux (poll-timeout.c), the sleeping poll keeps the file it found: the close does not wake it, the closed file can still wake it (a datagram sent to a closed UDP socket's address did), and when it wakes it looks the number up again, answering POLLNVAL if the number is free and the new file's readiness if another open took the number. This kernel keeps the file alive, but a poll woken by that file and finding nothing under the number sleeps again until the file's next wake-up, an edge, where this kernel's wake conditions are levels: the poll would be woken again at once, for ever."
        | CloseRefusal.DarwinListenerDescriptorWithAccepter (listener, task) ->
            $"the descriptor names the listening socket of open file description %O{listener}, and task %O{task} is parked in an accept on it. Measured on Darwin (blocking-accept.c), closing the descriptor the accept was entered through ends it at once with ECONNABORTED, even while a dup keeps the listener open, and closing another descriptor onto it does not; this kernel models neither a close ending a sleeping call nor which descriptor a call was entered through."
        | CloseRefusal.DarwinPipeDescriptorWithTransfer (description, task) ->
            $"the descriptor names the pipe end of open file description %O{description}, and task %O{task} is asleep in a read or write through it. Measured on Darwin (pipe-blocking.c section K), closing the descriptor the call sleeps through ends it at once, a read with end of file and a write with EPIPE, while closing a dup of it does not; this kernel models neither a close ending a sleeping call nor which descriptor a call was entered through."
        | CloseRefusal.Release refusal -> DescriptionReleaseRefusal.describe refusal

/// What `ioctl(fd, FIONREAD, &count)` answered.
[<RequireQualifiedAccess>]
type BytesAvailableAnswer =
    /// The call returned 0, having written `count` into the caller's `int`.
    | Reported of count : int
    /// The call returned -1 with this errno, and wrote nothing.
    | Failed of error : UnixError

/// Why this kernel will not answer an `ioctl(FIONREAD)`.
[<RequireQualifiedAccess>]
type BytesAvailableRefusal =
    /// The destination has no answer at the copy.
    | Buffer of BufferRefusal
    /// The descriptor `fd` names something other than a pipe, for which what
    /// `FIONREAD` answers is not modelled.
    | UnmodelledTarget of fd : int

[<RequireQualifiedAccess>]
module BytesAvailableRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked.
    let describe (refusal : BytesAvailableRefusal) : string =
        match refusal with
        | BytesAvailableRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | BytesAvailableRefusal.UnmodelledTarget fd ->
            $"fd %d{fd} is not an end of a pipe, and FIONREAD is answered here for pipes only. The other kinds answer per kind and per flavour (measured, pipe-syscalls.c): a regular file reports its size less the offset on both; a directory is ENOTTY on Linux and reports a number of its own on Darwin; a socket reports what it has queued; an epoll port is EINVAL and a kqueue ENOTTY. Model the kind before answering."

/// What `tcgetattr(3)` answered, which is also what `isatty(3)` answers: it is
/// `tcgetattr` with the answer reduced to 1 or 0 and the errno left as it was.
///
/// Nothing this kernel models is a terminal, so the one case is a failure.
[<RequireQualifiedAccess>]
type TerminalAttributesAnswer =
    /// `tcgetattr` returns -1 and `isatty` returns 0, each with this errno.
    | NotATerminal of error : UnixError

/// What `getgroups(2)` does with the caller's buffer and what it returns.
[<RequireQualifiedAccess>]
type GetGroupsAnswer =
    /// The call was asked only how many groups there are (a size of 0). It
    /// returns `count` and writes nothing, whatever the buffer is.
    | Counted of count : int
    /// Place these groups in the caller's buffer, in this order, and return how
    /// many there are. They fit: the size has already been compared with this
    /// list. An empty list writes nothing and so never faults.
    | Copied of groups : GroupId list
    /// The call returns -1 and the caller stores `error` wherever its libc
    /// keeps errno. Nothing was written.
    | Failed of error : UnixError

/// Why this kernel will not answer a `getgroups(2)`.
[<RequireQualifiedAccess>]
type GetGroupsRefusal =
    /// The buffer has no answer at the copy, which is the only step that
    /// reads it.
    | Buffer of BufferRefusal
    /// This flavour's list for these credentials has not been measured; see
    /// `GroupListReport.Unmeasured`.
    | UnmeasuredGroupList of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module GetGroupsRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and what the buffer was.
    let describe (refusal : GetGroupsRefusal) : string =
        match refusal with
        | GetGroupsRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | GetGroupsRefusal.UnmeasuredGroupList flavour ->
            $"which groups %O{flavour}'s getgroups(2) reports for these credentials has not been measured. It depends on what setgroups(2) does with the list it is given, and setting that needs root. A login process reports its effective group first and the rest unsorted, which fits both of two rules (the effective group added in front of the supplementary groups, or a list that already began with it reported as given), and the two disagree about every other list."

/// Why this kernel will not answer an `ioctl(2)` of `FICLONE`.
[<RequireQualifiedAccess>]
type FileCloneRefusal =
    /// This kernel is not Linux-flavoured, and `FICLONE` is a Linux request.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// Both descriptors name regular files on a mount of this type, whose
    /// answer to a clone request is unmeasured.
    | UnmeasuredFileSystem of fileSystem : EmulatedFileSystemType

[<RequireQualifiedAccess>]
module FileCloneRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which descriptors.
    let describe (refusal : FileCloneRefusal) : string =
        match refusal with
        | FileCloneRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and FICLONE is a Linux ioctl request."
        | FileCloneRefusal.UnmeasuredFileSystem fileSystem ->
            $"both descriptors name regular files on a %O{fileSystem} mount. Whether a clone request succeeds there depends on the filesystem (tmpfs answers EOPNOTSUPP; Btrfs and XFS share the source's extents), and this filesystem's answer has not been measured."

/// Which filesystem an open object lives on, for a call that refuses to work
/// across two.
[<RequireQualifiedAccess>]
type private ObjectFileSystem =
    /// A file or directory, on the root filesystem (`None`) or on the
    /// filesystem mounted at `mountedRoot`.
    | Mounted of mountedRoot : InodeNumber option
    | Pseudo of PseudoFileSystem

/// Why a file descriptor cannot be seeked, as a *fault* rather than as the errno
/// it becomes.
///
/// Not a `UnixError`, because `lseek` orders the two faults differently per
/// flavour: measured, Linux validates `whence` between them while Darwin does
/// not, so an ordering written over errnos would let a future third fault
/// inherit whichever position its errno's arm happened to occupy.
[<RequireQualifiedAccess>]
type private DescriptorFault =
    /// No such descriptor in the process's table; `EBADF`. Precedes everything
    /// else on both platforms.
    | NotOpen
    /// The descriptor names something with no file offset — a pipe; `ESPIPE`.
    | NotSeekable

[<RequireQualifiedAccess>]
module UnixDescriptor =

    /// The effective user ID, as `geteuid(2)` reports it.
    ///
    /// Total, and changes nothing: `geteuid` cannot fail.
    let effectiveUserId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UserId
        =
        system.Process.Credentials.EffectiveUser

    /// The effective group ID, as `getegid(2)` reports it.
    ///
    /// Total, and changes nothing: `getegid` cannot fail.
    let effectiveGroupId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : GroupId
        =
        system.Process.Credentials.EffectiveGroup

    /// `getgroups(2)`: the process's groups, into `destination`, which has room
    /// for `size` of them.
    ///
    /// Changes nothing, so it returns no system.
    let getgroups<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : UserBuffer)
        (size : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetGroupsAnswer, GetGroupsRefusal>
        =
        // Measured on Linux 6.18.5 and Darwin 27.0 (`getgroups.c`), in this
        // order: a negative size is EINVAL on both (-1, -2, -16, -65536,
        // INT_MIN + 1 and INT_MIN, and -1 with a NULL buffer); a size of 0 returns the count and writes nothing,
        // even through NULL; a size below the count is EINVAL, NULL or not; and
        // only then is the buffer written, so NULL is EFAULT. Linux copies
        // nothing for an empty list, so no buffer faults then. Linux writes
        // element by element up to a fault (a buffer one element short of a
        // PROT_NONE page had its first element written), which an `Unmapped`
        // buffer, holding no storage at all, never shows.
        if size < 0 then
            Ok (GetGroupsAnswer.Failed UnixError.EINVAL)
        else

        match
            Credentials.reportedGroups
                (SimulatedUnixPlatform.groupListReport system.Machine.UnixPlatform)
                system.Process.Credentials
        with
        | None ->
            Error (GetGroupsRefusal.UnmeasuredGroupList (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform))
        | Some groups ->

        let count = List.length groups

        if size = 0 then
            Ok (GetGroupsAnswer.Counted count)
        elif size < count then
            Ok (GetGroupsAnswer.Failed UnixError.EINVAL)
        elif count = 0 then
            Ok (GetGroupsAnswer.Copied [])
        else

        match destination with
        | UserBuffer.Mapped -> Ok (GetGroupsAnswer.Copied groups)
        | UserBuffer.Unmapped _ -> Ok (GetGroupsAnswer.Failed UnixError.EFAULT)
        | UserBuffer.Opaque -> Error (GetGroupsRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (GetGroupsRefusal.Buffer BufferRefusal.AddresslessAtTransfer)

    /// `dup(2)`: the lowest non-negative descriptor not in use, sharing `fd`'s
    /// open file description. EBADF is its only failure.
    let dup<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match FileDescriptorRegistry.dup fd system.Process.FileDescriptors with
        | Ok (newFd, registry) ->
            SyscallAnswer.Completed (int64 newFd),
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | Error FileDescriptorDupError.BadFd -> SyscallAnswer.Failed UnixError.EBADF, system

    /// `lseek(2)`: move `fd`'s file offset and report where it lands.
    ///
    /// On a directory, the position is where the next read of its entries
    /// resumes, and offset 0 rewinds to the first of them.
    ///
    /// Refuses the inputs whose answer this kernel cannot state; see
    /// `LSeekRefusal`.
    let lseek<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (offset : int64)
        (whence : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, LSeekRefusal>
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        // POSIX's numbering, and both platforms' `<unistd.h>` — for these three. It
        // stops here; 3 and 4 are handled below and are *not* portable.
        let seekSet = 0
        let seekCur = 1
        let seekEnd = 2
        let seekMax = 4

        // The two orderings below are measured, and this is the syscall where
        // they differ most. On a single-fault input the platforms agree on
        // every row; they part company on two:
        //
        //   input                       Linux    Darwin
        //   pipe + whence 99            EINVAL   ESPIPE
        //   pipe + whence 99 + overflow EINVAL   ESPIPE
        //
        // So Linux validates `whence` before it asks whether the object is
        // seekable, and Darwin the other way round. The descriptor itself
        // precedes both on either platform — `lseek(badfd, ..)` is EBADF for
        // every whence and offset measured, including 99, 3, 4 and INT64_MAX —
        // and the offset arithmetic follows both, pinned by
        // `lseek(pipe, -1, SEEK_SET)` = ESPIPE on both (seekability first) and
        // `lseek(f, 1, 99)` from INT64_MAX = EINVAL on both (whence first).
        let whenceValid = whence >= seekSet && whence <= seekMax

        let target = FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors

        let descriptorFault : DescriptorFault option =
            match target with
            | None -> Some DescriptorFault.NotOpen
            | Some (OpenFileTarget.Pipe _) ->
                // Not seekable: `lseek` on a pipe is ESPIPE on both platforms
                // whichever end it is.
                Some DescriptorFault.NotSeekable
            // Measured: Darwin refuses `lseek` on a kqueue with ESPIPE, while
            // Linux gives an epoll descriptor `noop_llseek`, which succeeds and
            // reports 0 without consulting the offset or moving anything. So a
            // kqueue has a descriptor fault here and an epoll instance has none;
            // the epoll success is served below, after the whence check the
            // syscall still applies.
            | Some (OpenFileTarget.Kqueue _) -> Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.Epoll _) -> None
            | Some (OpenFileTarget.Socket _) ->
                // Unseekable on both, unlike the port above: measured, both
                // platforms answer ESPIPE for every whence in 0..4 and every
                // offset, `-1` and `INT64_MAX` alike. The whence-ordering
                // divergence still shows through this, and is exactly what the
                // ladder below reproduces — measured, `lseek(sock, 0, 9)` is
                // EINVAL on Linux (whence checked first) and ESPIPE on Darwin
                // (seekability checked first).
                Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.File _)
            | Some (OpenFileTarget.Directory _) -> None

        let ordered : UnixError option =
            match descriptorFault with
            | Some DescriptorFault.NotOpen ->
                // Ahead of everything on both platforms.
                Some UnixError.EBADF
            | notOpenRejected ->

            let unseekable =
                match notOpenRejected with
                | Some DescriptorFault.NotSeekable -> true
                | Some DescriptorFault.NotOpen
                | None -> false

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                if not whenceValid then Some UnixError.EINVAL
                elif unseekable then Some UnixError.ESPIPE
                else None
            | SimulatedUnixFlavour.Darwin ->
                if unseekable then Some UnixError.ESPIPE
                elif not whenceValid then Some UnixError.EINVAL
                else None

        match ordered with
        | Some error -> Ok (SyscallAnswer.Failed error, system)
        | None ->

        // Linux's `noop_llseek`, for an epoll instance (a kqueue answered ESPIPE
        // above). It returns the file position unchanged, and an
        // epoll descriptor's is always 0, so the answer is 0 for every input
        // that gets here — measured for `SEEK_SET` with offset -1 and with
        // INT64_MAX alike, and for whence 3 and 4.
        //
        // Ahead of the SEEK_DATA/SEEK_HOLE refusal below, which is why that
        // refusal is not simply hoisted to the whence check: it is a statement
        // about a *file's* sparseness, and a port has none. The syscall's own
        // `whence <= SEEK_MAX` guard still applies and has already run, so
        // whence 5 and above were rejected as EINVAL.
        match target with
        | Some (OpenFileTarget.Epoll _) -> Ok (SyscallAnswer.Completed 0L, system)
        // Each of these answered EBADF or ESPIPE above.
        | None
        | Some (OpenFileTarget.Kqueue _)
        | Some (OpenFileTarget.Pipe _)
        | Some (OpenFileTarget.Socket _) ->
            failwith
                $"UnixDescriptor.lseek: fd %d{fd} names %A{target}, which the descriptor checks above should have answered (this is a bug in this library)"
        | Some (OpenFileTarget.File _)
        | Some (OpenFileTarget.Directory _) ->

        // Whence *validity* is settled; whence *semantics* is not, and the two
        // sit at different points in Linux's order — which is why refusing 3 and
        // 4 up front would be wrong. Measured, `lseek(badfd, 0, 3)` is EBADF and
        // `lseek(pipe, 0, 3)` is ESPIPE on both platforms, so a caller reaching
        // here with whence 3 or 4 really is asking about a seekable file's
        // sparseness.
        if whence > seekEnd then
            let meaning =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    if whence = 3 then
                        SeekExtension.SeekData
                    else
                        SeekExtension.SeekHole
                | SimulatedUnixFlavour.Darwin ->
                    if whence = 3 then
                        SeekExtension.SeekHole
                    else
                        SeekExtension.SeekData

            Error (LSeekRefusal.Sparseness (whence, meaning))
        else

        let seekWhence =
            if whence = seekSet then
                SeekWhence.Set
            elif whence = seekCur then
                SeekWhence.Current
            elif whence = seekEnd then
                SeekWhence.End
            else
                failwith
                    $"UnixDescriptor.lseek: whence %d{whence} passed the validity and semantics checks but is not one of SEEK_SET, SEEK_CUR or SEEK_END (this is a bug in this library)"

        // A directory's position is a number only at its start and where an
        // `lseek` put it; anywhere else it is a name (see
        // `LSeekRefusal.DirectoryPosition`).
        let inode, current =
            match target with
            | Some (OpenFileTarget.File (inode, current)) -> inode, Ok current
            | Some (OpenFileTarget.Directory (inode, DirectoryPosition.Cursor DirectoryCursor.Start)) -> inode, Ok 0L
            | Some (OpenFileTarget.Directory (inode, DirectoryPosition.Unenumerable offset)) -> inode, Ok offset
            | Some (OpenFileTarget.Directory (inode, DirectoryPosition.Cursor _)) ->
                inode, Error (LSeekRefusal.DirectoryPosition inode)
            | _ ->
                failwith
                    $"UnixDescriptor.lseek: fd %d{fd} is not a seekable file, but the descriptor checks above did not reject it (this is a bug in this library)"

        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixDescriptor.lseek: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

        // The content is inspected only where a size is wanted, which is
        // `SEEK_END` alone. Some directories have none this kernel will state,
        // and a symlink should not be here at all — but `SEEK_SET` and
        // `SEEK_CUR` ask neither question, so neither may fire on those paths.
        let basis : Result<SeekEndBasis, LSeekRefusal> =
            match seekWhence with
            | SeekWhence.Set
            | SeekWhence.Current -> Ok SeekEndBasis.NotConsulted
            | SeekWhence.End ->
                match entry.Content with
                | InodeContent.RegularFile (contents, _) -> Ok (SeekEndBasis.Size (int64 contents.Length))
                | InodeContent.CharacterDevice _ ->
                    failwith
                        $"UnixDescriptor.lseek: fd %d{fd} names inode %O{inode}, which is a character device. This kernel opens no description of a device as a file (this is a bug in this library)."
                | InodeContent.Symlink _ ->
                    // Not reachable: `open` resolves symlinks, so no descriptor
                    // names one. Stated rather than folded in so that an
                    // `O_PATH`-style descriptor finds a decision here.
                    failwith
                        $"UnixDescriptor.lseek: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what seeking a link through a descriptor means (this is a bug in this library)."
                | InodeContent.Directory directory ->
                    // Measured 2026-09-23 on every step of the histories
                    // `EmulatedFileSystemType.directorySize` records, and at 0,
                    // 1, 5 and 37 entries for offsets INT64_MIN, -10000,
                    // -size-1, -size, -size+1, -1, 0, 1, 7, 2^40 and
                    // INT64_MAX-size-1 .. INT64_MAX.
                    match UnixMachineState.fileSystemTypeOf inode system.Machine with
                    // Linux 6.18.5: EINVAL for every offset, leaving the
                    // position where it was. A tmpfs directory's `llseek`
                    // takes `SEEK_SET` and `SEEK_CUR` only.
                    | EmulatedFileSystemType.Tmpfs -> Ok (SeekEndBasis.Fails UnixError.EINVAL)
                    // macOS 26.6: exactly a regular file's arithmetic over the
                    // size `stat` reports — EINVAL below zero, EOVERFLOW past
                    // INT64_MAX.
                    | EmulatedFileSystemType.Apfs ->
                        match
                            EmulatedFileSystemType.directorySize EmulatedFileSystemType.Apfs directory.Entries.Count
                        with
                        | Some size -> Ok (SeekEndBasis.Size size)
                        | None ->
                            failwith
                                "UnixDescriptor.lseek: EmulatedFileSystemType.directorySize states no size for an APFS directory, which is what SEEK_END on one is measured from (this is a bug in this library)"
                    // The size is the server's, as for `stat`.
                    | EmulatedFileSystemType.Nfs -> Error (LSeekRefusal.DirectoryEnd inode)

        match basis with
        | Error refusal -> Error refusal
        | Ok (SeekEndBasis.Fails error) -> Ok (SyscallAnswer.Failed error, system)
        | Ok basis ->

        let sizeOf =
            lazy
                match basis with
                | SeekEndBasis.Size size -> size
                | SeekEndBasis.NotConsulted
                | SeekEndBasis.Fails _ ->
                    failwith
                        "UnixDescriptor.lseek: the file size was consulted on a path that does not consult it (this is a bug in this library)"

        let current =
            match seekWhence, current with
            | SeekWhence.Current, current -> current
            // `seekTarget` does not consult the current position for these, so
            // a directory part of the way through is no obstacle to them.
            | SeekWhence.Set, current
            | SeekWhence.End, current -> current |> Result.defaultValue 0L |> Ok

        match current with
        | Error refusal -> Error refusal
        | Ok current ->

        match VirtualFileSystem.seekTarget seekWhence current sizeOf offset with
        | Error SeekFault.Negative ->
            // EINVAL on both, and the offset is left where it was — measured, a
            // failed `lseek` does not move the description.
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | Error SeekFault.Overflow ->
            // The one place the *errno* differs rather than the ordering.
            // Measured on a tmpfs-backed file, so that the filesystem is held
            // constant: `lseek(f, INT64_MAX-4, SEEK_END)` on a 5-byte file is
            // EINVAL on Linux and EOVERFLOW on Darwin.
            match flavour with
            | SimulatedUnixFlavour.Linux -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            | SimulatedUnixFlavour.Darwin -> Ok (SyscallAnswer.Failed UnixError.EOVERFLOW, system)
        | Ok position ->

        let registry =
            match target with
            | Some (OpenFileTarget.Directory _) ->
                // Measured on both, with the arithmetic above exactly a regular
                // file's (EINVAL below zero; EINVAL on Linux and EOVERFLOW on
                // Darwin past INT64_MAX): any non-negative offset is accepted and
                // reported back, and offset 0 rewinds to the first entry.
                let directoryPosition =
                    if position = 0L then
                        DirectoryPosition.Cursor DirectoryCursor.Start
                    else
                        DirectoryPosition.Unenumerable position

                FileDescriptorRegistry.setDirectoryPosition fd directoryPosition system.Process.FileDescriptors
            | _ -> FileDescriptorRegistry.setOffset fd position system.Process.FileDescriptors

        Ok (
            SyscallAnswer.Completed position,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        )

    /// Commit a truncation of the regular file `inode` to `length`, together with
    /// the `mtime`, `ctime` and set-ID bits it moves.
    ///
    /// Shared by `ftruncate` and by `open`'s `O_TRUNC`, which are the same
    /// operation with the same measured consequences — the mode rule, the
    /// timestamp rule and the truncate-to-the-same-length rule all agree between
    /// them on both platforms.
    ///
    /// Not short-circuited when the file is already that length: unlike a write
    /// of no bytes, a truncation that moves no bytes still stamps the inode.
    let truncateAt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (length : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixSystem<'Task, 'Handler>, TruncationRefusal>
        =
        let now = UnixMachineState.realtime system.Machine
        let rule = SimulatedUnixPlatform.setIdBitsOnTruncation system.Machine.UnixPlatform

        match
            VirtualFileSystem.truncateFile inode length rule system.Process.Credentials now system.Machine.FileSystem
        with
        | Ok filesystem ->
            Ok
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                }
        | Error (FileTruncationRefusal.WouldExceedMaxLength length) ->
            Error (TruncationRefusal.ExceedsRepresentableLength (inode, length))
        | Error (FileTruncationRefusal.UnmeasuredSetIdChange refusal) ->
            Error (TruncationRefusal.UnmeasuredSetIdChange (inode, refusal))

    /// `ftruncate(2)`: set a regular file's length through a descriptor open for
    /// writing.
    let ftruncate<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (length : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, TruncationRefusal>
        =
        // **Ahead of the descriptor**, measured on both platforms: the same
        // unknown fd is EBADF with a length of 0 and EINVAL with a length of -1,
        // so the length really is validated first rather than the two faults
        // merely sharing an errno.
        if length < 0L then
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        else

        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some description ->

        match description.Target with
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Socket _ ->
            // EINVAL on both platforms for every object that is not a regular
            // file: measured on a pipe (either end), an INET socket, a UNIX
            // socket, an epoll port and a kqueue. Unlike `pread`/`pwrite` there
            // is no unseekable-versus-unwritable tie for the platforms to break
            // differently, so this arm deliberately carries no Darwin flag.
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | OpenFileTarget.File (inode, _)
        | OpenFileTarget.Directory (inode, _) ->

        // A descriptor not open for writing is EINVAL rather than EBADF —
        // `ftruncate(2)` differs from `write(2)` here, and it is measured on both
        // platforms.
        //
        // This is also what makes a *directory* descriptor answer EINVAL without
        // a type check: one can only ever be opened `O_RDONLY`, `open` answering
        // EISDIR for every write access mode. Adding a type check here would be a
        // mistake as well as redundant — EISDIR is what path-based `truncate(2)`
        // answers for a directory, where `ftruncate(2)` answers EINVAL.
        if not (FileAccessMode.permitsWrite description.AccessMode) then
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        else

        truncateAt inode length system
        |> Result.map (fun system -> SyscallAnswer.Completed 0L, system)

    /// `posix_fadvise(2)`: tell the kernel how a region of `fd` will be read.
    ///
    /// `advice` is the platform's raw `POSIX_FADV_*` number. Linux accepts 0
    /// (`NORMAL`) to 5 (`NOREUSE`) on x86-64 and aarch64 alike, and answers any
    /// other value EINVAL, but only once the descriptor has been screened: a
    /// closed descriptor is EBADF and a pipe ESPIPE whatever the advice.
    ///
    /// Refused where the platform's libc has no such call (Darwin): no program
    /// could have made it.
    ///
    /// A pure hint: this kernel models no readahead, so a `Completed` answer
    /// leaves the system exactly as it arrived. That is why no system comes
    /// back.
    ///
    /// The rows this implements are measured on Linux 6.18.5 aarch64 by
    /// `docs/probes/fadvise/fadvise.py`. The path they exercise
    /// (`ksys_fadvise64_64` and `generic_fadvise`) is architecture-independent,
    /// and the advice numbering is `<linux/fadvise.h>`'s on both; x86-64 is not
    /// separately measured. Two rows contradict what "not seekable" would
    /// predict: a socket and an epoll instance both succeed, and only a pipe
    /// answers ESPIPE.
    let posixFadvise<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (offset : int64)
        (length : int64)
        (advice : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<FileAdviceAnswer, PosixFadviseRefusal>
        =
        // Measured, the offset is never validated at all: a negative one, and
        // one whose sum with the length overflows, both succeed.
        ignore<int64> offset

        if not (SimulatedUnixPlatform.providesPosixFadvise system.Machine.UnixPlatform) then
            Error PosixFadviseRefusal.NotProvided
        else

        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Ok (FileAdviceAnswer.Failed UnixError.EBADF)
        | Some description ->

        match description.Target with
        | OpenFileTarget.Pipe _ ->
            // Measured on both ends of a real pipe, and ahead of the length and
            // advice screens below: the pipe test sits in the syscall entry,
            // where the range and advice checks belong to the generic path it
            // dispatches to.
            Ok (FileAdviceAnswer.Failed UnixError.ESPIPE)
        | OpenFileTarget.File _
        | OpenFileTarget.Directory _
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Socket _ ->

        // The generic path every non-pipe reaches screens the length and the
        // advice, and both answer EINVAL, so their relative order is invisible.
        // All six advices then behave identically, this kernel modelling no
        // readahead for them to steer.
        if length < 0L || advice < 0 || advice > 5 then
            Ok (FileAdviceAnswer.Failed UnixError.EINVAL)
        else
            Ok FileAdviceAnswer.Completed

    /// `flock(2)`, made by `task`: take, convert or release an advisory lock on
    /// `fd`'s open file description.
    ///
    /// Models Linux's rules and refuses under Darwin rather than guessing, for
    /// each of the divergences `FLockRefusal` names.
    ///
    /// A blocking acquisition that another description's lock stands in the way
    /// of answers `SyscallOutcome.WouldBlock`, in the system a real kernel would
    /// have slept in — which is not the system the call arrived with, because
    /// the caller's own old lock has already gone — and that system records
    /// `task` as parked on the lock. The record is what `close` reads to refuse
    /// destroying the description, and what `flockAcquire` finishes from once
    /// the condition holds. `task` must be registered in `Tasks`, and must not
    /// already be parked: a task blocks in one syscall at a time, and a parked
    /// `flock` is finished through `flockAcquire`, never re-issued.
    let flock<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (operation : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallOutcome * UnixSystem<'Task, 'Handler>, FLockRefusal>
        =
        // Loudly partial in an unregistered task, before anything is answered:
        // a non-blocking call by one would otherwise be served and only a
        // blocking one refused.
        match UnixTaskTable.parkedFor task system.Tasks with
        | Some parked ->
            failwith
                $"UnixDescriptor.flock: task %O{task} is parked in %A{parked}, and is issuing an flock. A task blocks in one syscall at a time; a parked flock is finished with `flockAcquire`, and a socket wait's completion must clear its record first (this is a bug in the client)."
        | None ->

        // Unlike a foreign-function layer's error and open-flag encodings, these
        // are *not* values anything translates: `flock(2)` takes them verbatim,
        // and Linux and Darwin happen to agree on all four — measured on both
        // rather than assumed.
        let lockShared = 1
        let lockExclusive = 2
        let lockNonBlocking = 4
        let lockUnlock = 8
        let lockMandatory = 32

        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
        let nonBlocking = operation &&& lockNonBlocking <> 0
        let mode = operation &&& ~~~lockNonBlocking

        let descriptor = FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors

        // Where the descriptor is looked up relative to the operation screens
        // parts the flavours, and both are measured
        // (`docs/probes/flock/lock-mand.py`): Linux screens the operation
        // first, so a closed descriptor with a malformed operation is EINVAL,
        // and a `LOCK_MAND` request is answered 0 -- mandatory locking was
        // removed in 5.15 and the request is ignored -- before anything is
        // looked up, closed descriptors included. Darwin looks the descriptor
        // up first, so a closed one is EBADF whatever the operation.
        match flavour, descriptor with
        | SimulatedUnixFlavour.Linux, _ when operation &&& lockMandatory <> 0 ->
            Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), system)
        | SimulatedUnixFlavour.Darwin, None ->
            Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EBADF), system)
        | _ ->

        let request : FlockRequest option =
            if mode = lockUnlock then
                Some FlockRequest.Release
            elif mode = lockShared then
                Some (FlockRequest.Acquire FlockMode.Shared)
            elif mode = lockExclusive then
                Some (FlockRequest.Acquire FlockMode.Exclusive)
            else
                None

        // Linux validates strictly: exactly one of SH/EX/UN, optionally with NB,
        // and nothing else. Darwin is laxer *and* uses a different errno.
        match request with
        | None ->
            match flavour with
            | SimulatedUnixFlavour.Linux -> Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EINVAL), system)
            | SimulatedUnixFlavour.Darwin -> Error (FLockRefusal.DarwinMalformedOperation operation)
        | Some request ->

        // The remaining divergences are all about a descriptor already resolved,
        // so they are checked here rather than in the registry: that module
        // models one coherent set of rules. An unknown fd is EBADF on both
        // platforms, so there is nothing to refuse for one.
        let darwinRefusal : FLockRefusal option =
            match flavour, descriptor with
            | SimulatedUnixFlavour.Linux, _
            | _, None -> None
            | SimulatedUnixFlavour.Darwin, Some description ->
                match description.Target with
                | OpenFileTarget.Pipe (pipeId, _) -> Some (FLockRefusal.DarwinPipe pipeId)
                | OpenFileTarget.Kqueue _ -> Some FLockRefusal.DarwinKqueue
                | OpenFileTarget.Epoll _ ->
                    failwith
                        "UnixDescriptor.flock: a Darwin-flavoured kernel holds an epoll instance, which only Linux has (this is a bug in the caller's state construction)."
                | OpenFileTarget.Socket socketId -> Some (FLockRefusal.DarwinSocket socketId)
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _ ->
                    match request, description.Flock with
                    | FlockRequest.Acquire _, Some _ -> Some FLockRefusal.DarwinConversion
                    | _, _ -> None

        match darwinRefusal with
        | Some refusal -> Error refusal
        | None ->

        // The table advances even when the call fails: a conversion that could
        // not be granted has already dropped the caller's old lock. So the new
        // table is committed *before* the outcome is inspected, and every branch
        // below reports from `advanced`.
        let registry, error =
            FileDescriptorRegistry.flock fd request system.Process.FileDescriptors

        let advanced =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        match error with
        | Some FlockError.BadFd -> Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EBADF), advanced)
        | Some FlockError.WouldBlock ->
            if nonBlocking then
                Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EAGAIN), advanced)
            else

            // A blocking acquisition that *can* be satisfied is served above, so
            // only genuine contention reaches here. Parking must never quietly
            // become the non-blocking answer, which would hand the caller an
            // EWOULDBLOCK no kernel would have produced.
            let requested =
                match request with
                | FlockRequest.Acquire mode -> mode
                | FlockRequest.Release ->
                    // `FileDescriptorRegistry.flock` grants every release, so a
                    // release cannot be what contended.
                    failwith
                        $"flock: fd %d{fd} reported contention for a release, which cannot contend (this is a bug in this library)"

            // The requester is the description rather than the descriptor: a
            // `dup` of `fd` waits on the same lock, and a wake keyed on the
            // number would miss a waiter that had closed the one it asked
            // through.
            let requester =
                match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                | Some id -> id
                | None ->
                    failwith
                        $"flock: fd %d{fd} reported contention but names no open file description (this is a bug in this library)"

            let parked =
                ParkedSyscall.Flock
                    {
                        ParkedFlock.Requester = requester
                        Mode = requested
                    }

            // The condition is derived from the record rather than built beside
            // it, so a task cannot be parked on one lock while a client polls
            // for another.
            Ok (SyscallOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked advanced)
        | None -> Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), advanced)

    /// Finish the `flock` acquisition `task` parked in, against the open file
    /// description its park record names.
    ///
    /// From the record rather than by re-issuing `flock` with the descriptor
    /// the call was made through, and not as a convenience: descriptor numbers
    /// are allocated lowest-free and reused as soon as they are freed, so a
    /// `close` of that number elsewhere can leave it naming a different object
    /// by the time the lock frees. A real kernel has no such hazard: the
    /// sleeping call holds the file, and the park holds the description, which
    /// outlives its last descriptor until this call returns and goes then,
    /// taking with it any lock it was granted. `task` must be parked in an
    /// `flock`.
    ///
    /// A grant clears the park record. `WouldBlock`, with the same condition
    /// and the task re-parked on the same record behind every other park, is
    /// the answer when the lock has been taken since the waiter was woken. That is the ordinary case rather than
    /// an edge one: a release wakes every waiter and they race, so all but one
    /// of them find it gone.
    ///
    /// When the lock cannot be granted and a signal with a handler is pending
    /// for the task, the signal ends the call: `Restarts` if every handler that
    /// runs was installed with `SA_RESTART`, and `Answered (Failed EINTR)` if
    /// none was. Either clears the park, and a conversion's old lock stays
    /// dropped, as it is while the call sleeps.
    ///
    /// Most of what `flock` screens is not re-screened, because a screen over
    /// facts that cannot change is spent: the operation bits were validated
    /// before the park, and this signature makes a malformed resume
    /// unrepresentable; the Darwin refusals for a pipe, a socket and a kqueue
    /// are about the description's object kind, which never changes.
    /// `DarwinConversion` is the exception, because it screens *mutable* state —
    /// while this task held nothing, another through a `dup` of its descriptor
    /// could have taken a lock on this same description, which Darwin serves as
    /// a first acquisition, and the resume is then the conversion whose
    /// keep-versus-drop divergence is unmeasured.
    let flockAcquire<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallOutcome * UnixSystem<'Task, 'Handler>, FLockRefusal>
        =
        let requester, mode =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.Flock parked) -> parked.Requester, parked.Mode
            | Some (ParkedSyscall.SocketWait wait) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in a socket wait on %O{wait.Port}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.Kevent wait) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in a kevent on %O{wait.Kqueue}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.Poll poll) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in a poll of %A{poll.Entries}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.Accept accept) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in an accept on %O{accept.Listener}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.PipeRead _ as other)
            | Some (ParkedSyscall.PipeWrite _ as other) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in %A{other}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is not parked, so there is no acquisition to finish. A blocked `flock` records the park; only a task it answered `WouldBlock` finishes here (this is a bug in the client)."

        let descriptions =
            FileDescriptorRegistry.descriptions system.Process.FileDescriptors

        match Map.tryFind requester descriptions with
        | None ->
            failwith
                $"UnixDescriptor.flockAcquire: open file description %O{requester} is not in the table, but task %O{task} is parked on an flock of it, and a park holds its description until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
        | Some description ->

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, description.Flock with
        | SimulatedUnixFlavour.Darwin, Some _ -> Error FLockRefusal.DarwinConversion
        | SimulatedUnixFlavour.Darwin, None
        | SimulatedUnixFlavour.Linux, _ ->

        let registry, error =
            FileDescriptorRegistry.flockOn requester (FlockRequest.Acquire mode) system.Process.FileDescriptors

        let advanced =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        // The call returns: its park goes, and with it the call's reference to
        // the description, which goes too if no descriptor names it any more
        // (`open-file-references.c` sections D and F: the lock it was granted
        // goes with it).
        let finished () =
            let unparked =
                { advanced with
                    Tasks = UnixTaskTable.unpark task advanced.Tasks
                }

            // A socket has one description, so no other description's lock can
            // obstruct an `flock` of one, and such a call never sleeps.
            ObjectLifetime.releaseUnreferencedUnrefusable "UnixDescriptor.flockAcquire" [ requester ] unparked

        match error with
        | Some FlockError.BadFd ->
            // `flockOn` never resolves a descriptor, so it has no bad one to
            // report.
            failwith
                $"UnixDescriptor.flockAcquire: acquiring on open file description %O{requester} reported EBADF, which only a descriptor lookup can produce (this is a bug in this library)."
        | Some FlockError.WouldBlock ->
            match SyscallInterruption.ofPark task advanced with
            | Error refusal -> Error (FLockRefusal.Interruption refusal)
            | Ok (Some SyscallInterruption.Eintr) ->
                Ok (SyscallOutcome.Answered (SyscallAnswer.Failed UnixError.EINTR), finished ())
            | Ok (Some SyscallInterruption.Restart) -> Ok (SyscallOutcome.Restarts, finished ())
            | Ok None ->
                // Beaten: the waiter sleeps again on the same record, re-queued behind
                // every park already made, as a real kernel re-queues it.
                let parked =
                    ParkedSyscall.Flock
                        {
                            ParkedFlock.Requester = requester
                            Mode = mode
                        }

                Ok (SyscallOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked advanced)
        | None ->
            // Measured on Linux 6.18.5 (`signal-interrupt-requeue.c`, section
            // D): a lock that can be granted beats a pending signal. Darwin
            // answers whichever came first, which `beforeCompleting` refuses.
            match SyscallInterruption.beforeCompleting task advanced with
            | Error refusal -> Error (FLockRefusal.Interruption refusal)
            | Ok () -> Ok (SyscallOutcome.Answered (SyscallAnswer.Completed 0L), finished ())

    /// `ioctl(fd, FIONREAD, &count)`: how many bytes a read of the pipe end `fd`
    /// names could take now, written into the caller's `int` at `destination`.
    ///
    /// On Linux both ends report the bytes the pipe holds, even once the read
    /// end has closed; on Darwin the read end reports them and the write end
    /// reports 0. EBADF for a descriptor that is not open, whatever the
    /// destination; EFAULT for an unmapped destination, whatever the pipe
    /// holds. Refused for every descriptor that is not a pipe end.
    ///
    /// Changes nothing.
    let bytesAvailable<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<BytesAvailableAnswer, BytesAvailableRefusal>
        =
        // Measured by pipe-syscalls.c on both flavours: a descriptor that is not
        // open is EBADF through a bad pointer, and a pipe end is EFAULT through
        // one -- NULL included, and the write end included, though it reports 0
        // on Darwin.
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Ok (BytesAvailableAnswer.Failed UnixError.EBADF)
        | Some (OpenFileTarget.File _)
        | Some (OpenFileTarget.Directory _)
        | Some (OpenFileTarget.Kqueue _)
        | Some (OpenFileTarget.Epoll _)
        | Some (OpenFileTarget.Socket _) -> Error (BytesAvailableRefusal.UnmodelledTarget fd)
        | Some (OpenFileTarget.Pipe (pipeId, pipeEnd)) ->

        let held = PipeBuffer.held (UnixMachineState.pipe pipeId system.Machine).Buffer

        let count =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, pipeEnd with
            | SimulatedUnixFlavour.Linux, _
            | SimulatedUnixFlavour.Darwin, PipeEnd.Read -> held
            | SimulatedUnixFlavour.Darwin, PipeEnd.Write -> 0

        match destination with
        | UserBuffer.Unmapped _ -> Ok (BytesAvailableAnswer.Failed UnixError.EFAULT)
        | UserBuffer.Opaque -> Error (BytesAvailableRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (BytesAvailableRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (BytesAvailableAnswer.Reported count)

    /// `ioctl(destination, FICLONE, source)`: make the file `destination` names
    /// share the contents of the file `source` names.
    ///
    /// No filesystem this kernel answers for can share contents, so the answer
    /// is always an errno: EBADF for a descriptor the process does not hold,
    /// EXDEV for two objects on different filesystems, EISDIR if either is a
    /// directory, EINVAL if either is not a regular file, EBADF for a
    /// destination not open for writing or a source not open for reading, and
    /// otherwise EOPNOTSUPP on a tmpfs. Changes nothing.
    let fileClone<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destination : int)
        (source : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixError, FileCloneRefusal>
        =
        // Measured on Linux 6.18.5 (`copy-file-syscalls.c`, tmpfs and ext4
        // alike) over every pair of a file opened read-only, write-only and
        // read-write, a directory, each end of a pipe, a socket, an epoll
        // instance, a closed descriptor and 9999: either missing is EBADF; then
        // two objects on different filesystems (a file against a pipe, a pipe
        // against a socket) are EXDEV, a directory with anything on its own
        // filesystem is EISDIR, two pipe ends, two sockets or two epoll
        // instances are EINVAL, a read-only destination or a write-only source
        // is EBADF, and two regular files are EOPNOTSUPP, empty or not, and the
        // same description included. ext4 answers as tmpfs does.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (FileCloneRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let registry = system.Process.FileDescriptors

        match FileDescriptorRegistry.tryFind destination registry, FileDescriptorRegistry.tryFind source registry with
        | None, _
        | _, None -> Ok UnixError.EBADF
        | Some destinationDescription, Some sourceDescription ->

        let fileSystemOf (description : OpenFileDescription) : ObjectFileSystem =
            match description.Target with
            | OpenFileTarget.File (inode, _)
            | OpenFileTarget.Directory (inode, _) ->
                ObjectFileSystem.Mounted (VirtualFileSystem.mountedRootOf inode system.Machine.FileSystem)
            | OpenFileTarget.Pipe _ -> ObjectFileSystem.Pseudo PseudoFileSystem.Pipe
            | OpenFileTarget.Socket _ -> ObjectFileSystem.Pseudo PseudoFileSystem.Socket
            | OpenFileTarget.Epoll _ -> ObjectFileSystem.Pseudo PseudoFileSystem.AnonymousInode
            | OpenFileTarget.Kqueue _ ->
                failwith
                    "UnixDescriptor.fileClone: a Linux-flavoured kernel holds a kqueue, which only Darwin has (this is a bug in the caller's state construction)."

        let isDirectory (description : OpenFileDescription) : bool =
            match description.Target with
            | OpenFileTarget.Directory _ -> true
            | OpenFileTarget.File _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _ -> false

        let isRegularFile (description : OpenFileDescription) : bool =
            match description.Target with
            | OpenFileTarget.File _ -> true
            | OpenFileTarget.Directory _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _ -> false

        if fileSystemOf destinationDescription <> fileSystemOf sourceDescription then
            Ok UnixError.EXDEV
        elif isDirectory destinationDescription || isDirectory sourceDescription then
            Ok UnixError.EISDIR
        elif not (isRegularFile destinationDescription && isRegularFile sourceDescription) then
            Ok UnixError.EINVAL
        elif
            not (FileAccessMode.permitsWrite destinationDescription.AccessMode)
            || not (FileAccessMode.permitsRead sourceDescription.AccessMode)
        then
            Ok UnixError.EBADF
        else

        match EmulatedMount.fileSystemType system.Machine.Mount with
        | EmulatedFileSystemType.Tmpfs -> Ok UnixError.EOPNOTSUPP
        | EmulatedFileSystemType.Apfs
        | EmulatedFileSystemType.Nfs as fileSystem -> Error (FileCloneRefusal.UnmeasuredFileSystem fileSystem)

    /// `tcgetattr(3)`, and so `isatty(3)`: whether `fd` is a terminal. None of
    /// the objects this kernel models is one, so the answer is always the errno
    /// saying why not, which depends on the object and the flavour.
    ///
    /// Changes nothing.
    let terminalAttributes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : TerminalAttributesAnswer
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        // Measured by pipe-syscalls.c, through isatty, tcgetattr and
        // TIOCGWINSZ alike, which agree on every row:
        //
        //   descriptor                          Linux    Darwin
        //   not open                            EBADF    EBADF
        //   regular file, directory, pipe end   ENOTTY   ENOTTY
        //   TCP or UDP socket, IPv4 or IPv6     ENOTTY   ENXIO
        //   Unix-domain socket, either kind     ENOTTY   EOPNOTSUPP
        //   epoll port / kqueue                 EINVAL   ENOTTY
        let error =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | None -> UnixError.EBADF
            | Some (OpenFileTarget.File _)
            | Some (OpenFileTarget.Directory _)
            | Some (OpenFileTarget.Pipe _) -> UnixError.ENOTTY
            | Some (OpenFileTarget.Epoll _) -> UnixError.EINVAL
            | Some (OpenFileTarget.Kqueue _) -> UnixError.ENOTTY
            | Some (OpenFileTarget.Socket socketId) ->
                match flavour, (UnixMachineState.socket socketId system.Machine).Domain with
                | SimulatedUnixFlavour.Linux, _ -> UnixError.ENOTTY
                | SimulatedUnixFlavour.Darwin, SocketDomain.Inet
                | SimulatedUnixFlavour.Darwin, SocketDomain.Inet6 -> UnixError.ENXIO
                | SimulatedUnixFlavour.Darwin, SocketDomain.Unix -> UnixError.EOPNOTSUPP

        TerminalAttributesAnswer.NotATerminal error

    /// `close(2)`: drop `fd` from the process's table, together with the
    /// description it named if nothing references that any more, and the kernel
    /// objects the description was the last reference to — the socket, the
    /// connections nothing else references, the pipe, and the inode whose last
    /// name had already gone.
    ///
    /// A description is referenced by every descriptor naming it and by every
    /// syscall in flight that holds it (`ParkedSyscall.descriptions`), as a
    /// real kernel holds a file for a call that sleeps on it. So under Linux a
    /// close under a sleeping call is served, and the call sleeps on: the
    /// description goes when the call returns, which is when its finishing call
    /// releases it. Under Darwin a close of the descriptor a `kevent` sleeps
    /// through is served too, and drains the kqueue (`KqueueState.Drained`),
    /// which ends that wait and every other on the kqueue; the kqueue goes as
    /// the last of them returns, if no descriptor names it by then.
    ///
    /// `FileDescriptorRegistry.dropDescriptor` cannot do this itself: the
    /// socket table is the machine's rather than the process's, whether an
    /// inode is still named is a question about the filesystem, and what a
    /// sleeping call holds is the task table's.
    ///
    /// EBADF is its only errno; see `CloseRefusal` for the inputs it declines
    /// to answer at all.
    let close<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, CloseRefusal<'Task>>
        =
        match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some (closingId, closing) ->

        let parks =
            system.Tasks
            |> Map.toList
            |> List.choose (fun (task, state) -> state.Parked |> Option.map (fun park -> task, park.Syscall))

        // Under Darwin, a close of the descriptor a sleeping call was entered
        // through ends the call (accept, a pipe transfer), or itself waits
        // until the call has returned (flock), and the park does not record
        // which descriptor that was, so any close onto the description
        // refuses. Each is measured: `blocking-accept.c` section C,
        // `pipe-blocking.c` section K and `open-file-references.c` section D.
        // A `kevent` is not among them: its park records the descriptor it was
        // entered through, and the drain below is what the close does to it.
        //
        // Checked against the park record rather than a task's run state, so
        // the window between a wake and the woken task's re-entry is covered
        // too.
        let darwinRefusal : CloseRefusal<'Task> option =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> None
            | SimulatedUnixFlavour.Darwin ->
                parks
                |> List.tryPick (fun (task, parked) ->
                    match parked with
                    | ParkedSyscall.SocketWait wait when wait.Port = closingId ->
                        failwith
                            $"UnixDescriptor.close: task %O{task} is parked in an epoll_wait on %O{closingId} under the Darwin flavour, which has no epoll (this is a bug in the caller's state construction)."
                    | ParkedSyscall.Accept accept when accept.Listener = closingId ->
                        Some (CloseRefusal.DarwinListenerDescriptorWithAccepter (closingId, task))
                    | ParkedSyscall.PipeRead read when read.Reader = closingId ->
                        Some (CloseRefusal.DarwinPipeDescriptorWithTransfer (closingId, task))
                    | ParkedSyscall.PipeWrite write when write.Writer = closingId ->
                        Some (CloseRefusal.DarwinPipeDescriptorWithTransfer (closingId, task))
                    | ParkedSyscall.Flock parked when parked.Requester = closingId ->
                        Some (CloseRefusal.DarwinFlockedDescriptorWithWaiter (closingId, task))
                    | ParkedSyscall.SocketWait _
                    | ParkedSyscall.Kevent _
                    | ParkedSyscall.Accept _
                    | ParkedSyscall.PipeRead _
                    | ParkedSyscall.PipeWrite _
                    | ParkedSyscall.Flock _
                    | ParkedSyscall.Poll _ -> None
                )

        match darwinRefusal with
        | Some refusal -> Error refusal
        | None ->

        // A real poll looks each descriptor up again whenever it wakes, so
        // closing one it watches changes what it reports -- by number, not by
        // description, so a `dup` keeping the description alive does not help.
        let pollRefusal : CloseRefusal<'Task> option =
            parks
            |> List.tryPick (fun (task, parked) ->
                match parked with
                | ParkedSyscall.Poll parked ->
                    let watches =
                        parked.Entries
                        |> List.exists (fun entry ->
                            match entry with
                            | ParkedPollEntry.Watched (watched, _, _) -> watched = fd
                            | ParkedPollEntry.Ignored _ -> false
                        )

                    if watches then
                        Some (CloseRefusal.PolledDescriptor (fd, task))
                    else
                        None
                | ParkedSyscall.Flock _
                | ParkedSyscall.SocketWait _
                | ParkedSyscall.Kevent _
                | ParkedSyscall.Accept _
                | ParkedSyscall.PipeRead _
                | ParkedSyscall.PipeWrite _ -> None
            )

        match pollRefusal with
        | Some refusal -> Error refusal
        | None ->

        let registry, destroyed =
            match
                FileDescriptorRegistry.dropDescriptor
                    fd
                    (ObjectLifetime.heldByCalls system.Tasks)
                    system.Process.FileDescriptors
            with
            | Ok dropped -> dropped
            | Error FileDescriptorCloseError.BadFd ->
                failwith
                    $"UnixDescriptor.close: fd %d{fd} named open file description %O{closingId} (%A{closing.Target}) a moment ago, and the registry now calls it a bad descriptor (this is a bug in this library)."

        // Measured on Darwin 27.0.0 (`kqueue-kevent.c`, sections E and F):
        // closing a descriptor a task is asleep in `kevent` through drains the
        // kqueue, which ends that wait and every other wait on the kqueue with
        // EBADF, and leaves every later wait on it EBADF too. Closing one no
        // waiter entered through changes nothing. A waiter holds the kqueue
        // (`ParkedSyscall.descriptions`), so a close that drains it has not
        // destroyed it, even when it closed the last descriptor: the kqueue
        // goes as the last call holding it returns.
        //
        // Checked against the park records rather than a task's run state: a
        // woken waiter has not left the call yet.
        let registry =
            match closing.Target with
            | OpenFileTarget.Kqueue _ ->
                let enteredHere =
                    parks
                    |> List.exists (fun (_, parked) ->
                        match parked with
                        | ParkedSyscall.Kevent wait -> wait.Kqueue = closingId && wait.Fd = fd
                        | ParkedSyscall.SocketWait _
                        | ParkedSyscall.Flock _
                        | ParkedSyscall.Poll _
                        | ParkedSyscall.Accept _
                        | ParkedSyscall.PipeRead _
                        | ParkedSyscall.PipeWrite _ -> false
                    )

                if enteredHere then
                    FileDescriptorRegistry.drainKqueue closingId registry
                else
                    registry
            | OpenFileTarget.Epoll _
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Pipe _ -> registry

        let closed =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        match destroyed with
        | None -> Ok (SyscallAnswer.Completed 0L, closed)
        | Some destroyed ->

        match ObjectLifetime.releaseDestroyed destroyed closed with
        | Error refusal -> Error (CloseRefusal.Release refusal)
        | Ok released -> Ok (SyscallAnswer.Completed 0L, released)
