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

/// Why a Darwin close that ends a sleeping pipe `write` cannot be answered: the
/// write raises `SIGPIPE` for the process as it returns, before the close does,
/// and what that signal does is something a close has no way to report.
[<RequireQualifiedAccess>]
type EndedWriteSignalRefusal =
    /// `SIGPIPE`'s disposition is the default, so the signal ends the process.
    | TerminatesProcess
    /// The process is process ID 1, and what an init process does with a
    /// signal it has no handler for is not modelled.
    | InitProcess
    /// Which task would take the signal is not modelled.
    | Receiver of SignalReceiverRefusal

/// Why this kernel will not close a descriptor.
///
/// Generic in what names a task because most of them are about a task parked
/// in a wait, and which one that is cannot be recomputed by the client:
/// nothing stops two tasks parking on the same event queue, so a client repeating the
/// search could name a different one from the one this refusal is about.
///
/// Under Linux a sleeping call holds the description it sleeps on, so a close
/// under it is served: the description outlives its last descriptor until the
/// call returns. Under Darwin a close of the descriptor a sleeping `accept`,
/// pipe `read`, pipe `write` or `kevent` was made through ends the call, which
/// is modelled, so it is served; the Darwin cases here are the ones this
/// kernel does not model.
[<RequireQualifiedAccess>]
type CloseRefusal<'Task> =
    /// Releasing the description destroys an object in a state this kernel
    /// has not measured.
    | Release of DescriptionReleaseRefusal
    /// Any descriptor onto an open file description that `task` is parked on
    /// an `flock` of, under the Darwin flavour.
    | DarwinFlockedDescriptorWithWaiter of description : OpenFileDescriptionId * task : 'Task
    /// The descriptor `fd`, which `task` is parked in a Linux-flavoured
    /// `poll(2)` watching.
    ///
    /// Refused because Linux's sleeping poll keeps the file it found, which
    /// this kernel represents, but when it wakes it looks the number up again
    /// and reports what the number names by then, and it is woken only by the
    /// files it found: a wake that finds nothing ready under the number sleeps
    /// again until the next one. This kernel's wake conditions are levels, not
    /// edges, so such a poll would be woken again at once, for ever. A Darwin
    /// poll's watched descriptor closes: the close removes the filters
    /// registered through it from the kqueue the poll made, as Darwin does.
    | PolledDescriptor of fd : int * task : 'Task
    /// The descriptor a pipe `read` or `write` by `task`, asleep on the open
    /// file description `description`, was made through, under the Darwin
    /// flavour, when the call has something besides the close to answer: bytes
    /// to read, room to write into, a signal with a handler to take, or (a
    /// write whose description has become non-blocking) a read to give up at.
    ///
    /// Darwin's close ends such a call, a read with end of file and a write
    /// with `EPIPE`, and which of the close and what had already woken the
    /// call it answers is unmeasured.
    | DarwinWokenTransfer of description : OpenFileDescriptionId * task : 'Task
    /// The close would end the pipe `write` `task` is asleep in, under the
    /// Darwin flavour, and the `SIGPIPE` that write raises as it returns is one
    /// this kernel cannot answer (`refusal`).
    | DarwinEndedWriteSignal of task : 'Task * refusal : EndedWriteSignalRefusal

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
        | CloseRefusal.DarwinWokenTransfer (description, task) ->
            $"task %O{task} is asleep in a read or write of the pipe end of open file description %O{description}, made through this descriptor, and the call has something besides the close to answer: bytes, room, a signal, or a read to give up at. Measured on Darwin (close-ends-call.c sections P1-P7), closing the descriptor a sleeping read or write was made through ends it, a read with end of file and a write with EPIPE; which of that and what had already woken the call a kernel answers is unmeasured: no probe has held a woken call off the CPU until a close, since Darwin has no SCHED_FIFO, and a woken call in a stopped process finishes in the kernel all the same (pipe-blocking.c section N4)."
        | CloseRefusal.DarwinEndedWriteSignal (task, refusal) ->
            let why =
                match refusal with
                | EndedWriteSignalRefusal.TerminatesProcess ->
                    "SIGPIPE's disposition is the default, so it ends the process, which a close has no answer to report"
                | EndedWriteSignalRefusal.InitProcess ->
                    "the process is process ID 1, and what an init process does with a signal it has no handler for is not modelled"
                | EndedWriteSignalRefusal.Receiver refusal ->
                    $"which task would take the signal is not modelled (%A{refusal})"

            $"the close ends the pipe write task %O{task} is asleep in through this descriptor. Measured on Darwin (close-ends-call.c sections P2-P4), that write answers EPIPE and raises SIGPIPE for the process as it returns, which it does before the close does; but %s{why}."
        | CloseRefusal.Release refusal -> DescriptionReleaseRefusal.describe refusal

/// A status flag the flavour's `fcntl(F_SETFL)` would set, and this kernel
/// does not model.
[<RequireQualifiedAccess>]
type UnmodelledStatusFlag =
    /// `O_APPEND`, on both flavours.
    | Append
    /// `O_ASYNC` (Linux's `FASYNC`), on both flavours.
    | Asynchronous
    /// Linux's `O_DIRECT`.
    | Direct
    /// Linux's `O_NOATIME`.
    | NoAccessTime

[<RequireQualifiedAccess>]
module UnmodelledStatusFlag =
    /// The flag's name in the flavour's `<fcntl.h>`.
    let name (flag : UnmodelledStatusFlag) : string =
        match flag with
        | UnmodelledStatusFlag.Append -> "O_APPEND"
        | UnmodelledStatusFlag.Asynchronous -> "O_ASYNC"
        | UnmodelledStatusFlag.Direct -> "O_DIRECT"
        | UnmodelledStatusFlag.NoAccessTime -> "O_NOATIME"

/// Why this kernel will not answer an `fcntl(2)`.
[<RequireQualifiedAccess>]
type FcntlRefusal =
    /// A descriptor the call would make lies at or above the bound this kernel
    /// assumes the process's `RLIMIT_NOFILE` reaches.
    | DescriptorLimit of DescriptorLimitRefusal
    /// The command is none of `F_DUPFD`, `F_DUPFD_CLOEXEC`, `F_GETFD`,
    /// `F_SETFD`, `F_GETFL` and `F_SETFL` in the flavour's numbering.
    | UnmodelledCommand of command : int
    /// An `F_SETFL` word would set these flags, which change what later calls
    /// on the description do, in ways this kernel does not model.
    | UnmodelledStatusFlags of word : int * flags : UnmodelledStatusFlag list

[<RequireQualifiedAccess>]
module FcntlRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what it should do instead.
    let describe (refusal : FcntlRefusal) : string =
        match refusal with
        | FcntlRefusal.DescriptorLimit refusal -> DescriptorLimitRefusal.describe refusal
        | FcntlRefusal.UnmodelledCommand command ->
            $"fcntl command %d{command} is none of F_DUPFD, F_DUPFD_CLOEXEC, F_GETFD, F_SETFD, F_GETFL and F_SETFL in this flavour's numbering. Both kernels answer EBADF for a descriptor that is not open whatever the command, and that is answered; what a command they know does is not modelled."
        | FcntlRefusal.UnmodelledStatusFlags (word, flags) ->
            let names = flags |> List.map UnmodelledStatusFlag.name |> String.concat ", "
            $"F_SETFL with 0x%x{word} would set %s{names}, which change what later reads and writes on the description do (or, for O_NOATIME and O_DIRECT, answer EPERM or EINVAL depending on the file's owner and filesystem), and this kernel models none of that."

/// Why this kernel will not answer a `dup2(2)`.
[<RequireQualifiedAccess>]
type Dup2Refusal<'Task> =
    /// A descriptor the call would make lies at or above the bound this kernel
    /// assumes the process's `RLIMIT_NOFILE` reaches.
    | DescriptorLimit of DescriptorLimitRefusal
    /// The target is open, and this kernel will not close it (`refusal`): a
    /// `dup2` onto an open descriptor closes it as `close(2)` does, sleeping
    /// calls and all.
    | ClosingTarget of refusal : CloseRefusal<'Task>

[<RequireQualifiedAccess>]
module Dup2Refusal =
    /// What this kernel knows about why it cannot answer.
    let describe (refusal : Dup2Refusal<'Task>) : string =
        match refusal with
        | Dup2Refusal.DescriptorLimit refusal -> DescriptorLimitRefusal.describe refusal
        | Dup2Refusal.ClosingTarget refusal ->
            $"the target descriptor is open, and dup2 closes it as close(2) does, which this kernel will not do here: %s{CloseRefusal.describe refusal}"

/// Why this kernel will not answer a `dup3(2)`.
[<RequireQualifiedAccess>]
type Dup3Refusal<'Task> =
    /// A descriptor the call would make lies at or above the bound this kernel
    /// assumes the process's `RLIMIT_NOFILE` reaches.
    | DescriptorLimit of DescriptorLimitRefusal
    /// The flavour has no `dup3`.
    | NotProvided of flavour : SimulatedUnixFlavour
    /// As `Dup2Refusal.ClosingTarget`.
    | ClosingTarget of refusal : CloseRefusal<'Task>

[<RequireQualifiedAccess>]
module Dup3Refusal =
    /// What this kernel knows about why it cannot answer.
    let describe (refusal : Dup3Refusal<'Task>) : string =
        match refusal with
        | Dup3Refusal.DescriptorLimit refusal -> DescriptorLimitRefusal.describe refusal
        | Dup3Refusal.NotProvided flavour ->
            $"this kernel is %O{flavour}-flavoured, and only Linux has dup3; no program could have made the call."
        | Dup3Refusal.ClosingTarget refusal ->
            $"the target descriptor is open, and dup3 closes it as close(2) does, which this kernel will not do here: %s{CloseRefusal.describe refusal}"

/// What a change to a descriptor's `O_NONBLOCK` answered.
///
/// Store and answer are separate because for a kqueue they disagree: its bit
/// toggles and the call still reports a failure.
[<RequireQualifiedAccess>]
type SetNonBlockingAnswer =
    /// The flag is now what the caller asked for, and the call succeeded.
    | Set
    /// The call failed with this errno.
    ///
    /// The system still comes back, and the flag may have changed with it: on a
    /// kqueue the bit toggles and the answer is `ENOTTY` anyway.
    | Failed of error : UnixError

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
            $"fd %d{fd} is not an end of a pipe, and FIONREAD is answered here for pipes only. The other kinds answer per kind and per flavour (measured, pipe-syscalls.c): a regular file reports its size less the offset on both; a directory is ENOTTY on Linux and reports a number of its own on Darwin; a socket reports what it has queued; an epoll instance is EINVAL and a kqueue ENOTTY. Model the kind before answering."

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

/// Each flavour's `<fcntl.h>` numbers for the `fcntl(2)` commands
/// `UnixDescriptor.fcntl` models, and for `FD_CLOEXEC`.
[<RequireQualifiedAccess>]
module FcntlNumbering =
    /// `F_DUPFD`, on both flavours.
    [<Literal>]
    let DuplicateAtOrAbove : int = 0

    /// `F_GETFD`, on both flavours.
    [<Literal>]
    let GetDescriptorFlags : int = 1

    /// `F_SETFD`, on both flavours.
    [<Literal>]
    let SetDescriptorFlags : int = 2

    /// `F_GETFL`, on both flavours.
    [<Literal>]
    let GetStatusFlags : int = 3

    /// `F_SETFL`, on both flavours.
    [<Literal>]
    let SetStatusFlags : int = 4

    /// `F_DUPFD_CLOEXEC`: Linux's 1030, Darwin's 67.
    let duplicateCloseOnExec (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 1030
        | SimulatedUnixFlavour.Darwin -> 67

    /// `FD_CLOEXEC`, on both flavours.
    [<Literal>]
    let CloseOnExec : int = 1

    /// Darwin's `FD_CLOFORK`.
    [<Literal>]
    let DarwinCloseOnFork : int = 2

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
    /// open file description. EBADF is its only failure, and comes first: a
    /// descriptor that is not open is EBADF even when the new one would reach
    /// the bound (`SimulatedUnixPlatform.descriptorBound`), which is otherwise
    /// refused.
    let dup<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, DescriptorLimitRefusal>
        =
        let registry = UnixSystemState.fileDescriptors system

        // Measured (`fcntl-dup.c`, LIMIT rows): EBADF ahead of the allocation.
        match FileDescriptorRegistry.tryFindId fd registry with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some _ ->

        match
            FileDescriptorRegistry.room (SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform) 0 1 registry
        with
        | Error refusal -> Error refusal
        | Ok () ->

        match FileDescriptorRegistry.dup fd registry with
        | Ok (newFd, registry) ->
            Ok (SyscallAnswer.Completed (int64 newFd), UnixSystemState.withFileDescriptors registry system)
        | Error FileDescriptorDupError.BadFd ->
            failwith
                $"UnixDescriptor.dup: fd %d{fd} was open a moment ago and is not now (this is a bug in this library)."

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

        let target =
            FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system)

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
                // Unseekable on both, unlike the epoll instance above: measured, both
                // platforms answer ESPIPE for every whence in 0..4 and every
                // offset, `-1` and `INT64_MAX` alike. The whence-ordering
                // divergence still shows through this, and is exactly what the
                // ladder below reproduces — measured, `lseek(sock, 0, 9)` is
                // EINVAL on Linux (whence checked first) and ESPIPE on Darwin
                // (seekability checked first).
                Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.File _)
            | Some (OpenFileTarget.Directory _)
            | Some (OpenFileTarget.CharacterDevice _) -> None

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
        // about a *file's* sparseness, and an epoll instance has none. The syscall's own
        // `whence <= SEEK_MAX` guard still applies and has already run, so
        // whence 5 and above were rejected as EINVAL.
        match target with
        | Some (OpenFileTarget.Epoll _) -> Ok (SyscallAnswer.Completed 0L, system)
        // A device keeps no position, and answers 0 for every whence in 0..4
        // and every offset, negative and `INT64_MAX` included (`devices.c` and
        // `devices-l2.c`, LSEEK rows): `/dev/null`'s `null_lseek` resets a
        // position nothing moves, and `/dev/urandom`'s is `noop_llseek`.
        | Some (OpenFileTarget.CharacterDevice _) -> Ok (SyscallAnswer.Completed 0L, system)
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
                        $"UnixDescriptor.lseek: fd %d{fd} names inode %O{inode}, which is a character device, through a description of a regular file. A description of a device is OpenFileTarget.CharacterDevice, and UnixSystem.checkInvariants reports this one as DescriptionKindMismatch (this is a bug in this library)."
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

                FileDescriptorRegistry.setDirectoryPosition
                    fd
                    directoryPosition
                    (UnixSystemState.fileDescriptors system)
            | _ -> FileDescriptorRegistry.setOffset fd position (UnixSystemState.fileDescriptors system)

        Ok (SyscallAnswer.Completed position, UnixSystemState.withFileDescriptors registry system)

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
    let internal truncateAt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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

        match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system) with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some description ->

        match description.Target with
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Kqueue _
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _ ->
            // EINVAL on both platforms for every object that is not a regular
            // file: measured on a pipe (either end), an INET socket, a UNIX
            // socket, an epoll instance and a kqueue, and on Linux on `/dev/null`
            // and `/dev/urandom`, through descriptors opened for reading as
            // well as for writing (`devices.c`, FTRUNCATE rows). Unlike `pread`/`pwrite` there
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

        let id =
            match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
            | Some id -> id
            | None ->
                failwith
                    $"UnixDescriptor.ftruncate: fd %d{fd} named a description a moment ago and is not live now (this is a bug in this library)."

        // Measured on Darwin 27.0.0 (`fcntl-dup.c`, WRITTEN rows): a truncation
        // that succeeds marks the description written, even to the length the
        // file already had; one that fails does not.
        truncateAt inode length system
        |> Result.map (fun system ->
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> SyscallAnswer.Completed 0L, system
            | SimulatedUnixFlavour.Darwin ->
                SyscallAnswer.Completed 0L,
                UnixSystemState.mapOpenFiles
                    (OpenFileTable.mapStatus
                        id
                        (fun status ->
                            { status with
                                Written = true
                            }
                        ))
                    system
        )

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

        match FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system) with
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
        | OpenFileTarget.Socket _
        | OpenFileTarget.CharacterDevice _ ->

        // The generic path every non-pipe reaches screens the length and the
        // advice, and both answer EINVAL, so their relative order is invisible.
        // All six advices then behave identically, this kernel modelling no
        // readahead for them to steer.
        if length < 0L || advice < 0 || advice > 5 then
            Ok (FileAdviceAnswer.Failed UnixError.EINVAL)
        else
            Ok FileAdviceAnswer.Completed

    /// Record on the description `id` that an `flock` lock has been granted to
    /// it, under the flavour that reports that (`OpenFileStatus.Flocked`).
    ///
    /// Measured on Darwin 27.0.0 (`fcntl-dup.c`, WRITTEN rows): `F_GETFL` shows
    /// 0x4000 once a lock has been granted, blocking or not, and through a dup
    /// as well; a refused or interrupted request does not set it, and LOCK_UN
    /// does not clear it.
    let private recordGrant
        (flavour : SimulatedUnixFlavour)
        (id : OpenFileDescriptionId)
        (openFiles : OpenFileTable)
        : OpenFileTable
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> openFiles
        | SimulatedUnixFlavour.Darwin ->
            OpenFileTable.mapStatus
                id
                (fun status ->
                    { status with
                        Flocked = true
                    }
                )
                openFiles

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
                $"UnixDescriptor.flock: task %O{task} is parked in %A{parked}, and is issuing an flock. A task blocks in one syscall at a time; a parked flock is finished with `flockAcquire`, and any other parked call's completion must clear its record first (this is a bug in the client)."
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

        let descriptor =
            FileDescriptorRegistry.tryFind fd (UnixSystemState.fileDescriptors system)

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
                | OpenFileTarget.CharacterDevice _ ->
                    failwith
                        "UnixDescriptor.flock: a Darwin-flavoured kernel holds a description of a device, which only Linux's devtmpfs gives (this is a bug in the caller's state construction)."
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
            FileDescriptorRegistry.flock fd request (UnixSystemState.fileDescriptors system)

        let registry =
            match error, request, FileDescriptorRegistry.tryFindId fd registry with
            | None, FlockRequest.Acquire _, Some id ->
                FileDescriptorRegistry.mapOpenFiles (recordGrant flavour id) registry
            | _ -> registry

        let advanced = UnixSystemState.withFileDescriptors registry system

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
                match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
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
            | Some (ParkedSyscall.EpollWait wait) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in an epoll_wait on %O{wait.Epoll}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.Kevent wait) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in a kevent on %O{wait.Kqueue}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.Poll poll) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in a poll of %A{poll.Entries}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.Accept accept) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in an accept on %O{accept.Listener}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | Some (ParkedSyscall.KqueuePoll _ as other)
            | Some (ParkedSyscall.PipeRead _ as other)
            | Some (ParkedSyscall.PipeWrite _ as other) ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is parked in %A{other}, not in an flock, so there is no acquisition to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixDescriptor.flockAcquire: task %O{task} is not parked, so there is no acquisition to finish. A blocked `flock` records the park; only a task it answered `WouldBlock` finishes here (this is a bug in the client)."

        match OpenFileTable.tryFind requester system.Machine.OpenFiles with
        | None ->
            failwith
                $"UnixDescriptor.flockAcquire: open file description %O{requester} is not in the table, but task %O{task} is parked on an flock of it, and a park holds its description until the call returns (this is a bug in this library, or in a caller that ended a park without its finishing call or assembled the state by hand)."
        | Some description ->

        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, description.Flock with
        | SimulatedUnixFlavour.Darwin, Some _ -> Error FLockRefusal.DarwinConversion
        | SimulatedUnixFlavour.Darwin, None
        | SimulatedUnixFlavour.Linux, _ ->

        let openFiles, error =
            OpenFileTable.flockOn requester (FlockRequest.Acquire mode) system.Machine.OpenFiles

        let openFiles =
            match error with
            | None -> recordGrant (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform) requester openFiles
            | Some _ -> openFiles

        let advanced = UnixSystemState.mapOpenFiles (fun _ -> openFiles) system

        // The call returns: its park goes, and with it the call's reference to
        // the description, which goes too if no descriptor names it any more
        // (`open-file-references.c` sections D and F: the lock it was granted
        // goes with it).
        let finished () =
            let unparked = (UnixParkState.unpark task advanced)

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
    /// holds. A device answers its driver's errno whatever the destination
    /// (`CharacterDevice.unrecognisedIoctl`). Refused for every other
    /// descriptor that is not a pipe end.
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
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | None -> Ok (BytesAvailableAnswer.Failed UnixError.EBADF)
        | Some (OpenFileTarget.File _)
        | Some (OpenFileTarget.Directory _)
        | Some (OpenFileTarget.Kqueue _)
        | Some (OpenFileTarget.Epoll _)
        | Some (OpenFileTarget.Socket _) -> Error (BytesAvailableRefusal.UnmodelledTarget fd)
        | Some (OpenFileTarget.CharacterDevice (_, device)) ->
            // The driver's own answer, without the destination: Linux asks the
            // inode whether it is a regular file before it would write one.
            Ok (BytesAvailableAnswer.Failed (CharacterDevice.unrecognisedIoctl device))
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
        // same description included. ext4 answers as tmpfs does. A device
        // against a regular file is EXDEV either way round, and two devices
        // are EINVAL (`devices-l2.c`, FICLONE rows).
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (FileCloneRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let registry = UnixSystemState.fileDescriptors system

        match FileDescriptorRegistry.tryFind destination registry, FileDescriptorRegistry.tryFind source registry with
        | None, _
        | _, None -> Ok UnixError.EBADF
        | Some destinationDescription, Some sourceDescription ->

        let fileSystemOf (description : OpenFileDescription) : ObjectFileSystem =
            match description.Target with
            | OpenFileTarget.File (inode, _)
            | OpenFileTarget.Directory (inode, _)
            | OpenFileTarget.CharacterDevice (inode, _) ->
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
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _ -> false

        let isRegularFile (description : OpenFileDescription) : bool =
            match description.Target with
            | OpenFileTarget.File _ -> true
            | OpenFileTarget.Directory _
            | OpenFileTarget.CharacterDevice _
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
        //   epoll instance / kqueue             EINVAL   ENOTTY
        //
        // and a device answers what its driver does
        // (`CharacterDevice.unrecognisedIoctl`).
        let error =
            match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
            | None -> UnixError.EBADF
            | Some (OpenFileTarget.File _)
            | Some (OpenFileTarget.Directory _)
            | Some (OpenFileTarget.Pipe _) -> UnixError.ENOTTY
            | Some (OpenFileTarget.CharacterDevice (_, device)) -> CharacterDevice.unrecognisedIoctl device
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
    /// Under Darwin a close of the descriptor a sleeping pipe `read` or `write`
    /// was made through ends every such call made through it, and none made
    /// through another descriptor: the read with end of file, the write with
    /// `EPIPE` and `SIGPIPE`, whatever it had put in. A close of the
    /// descriptor a sleeping `accept` was made through ends every accept asleep
    /// on the listener, through any descriptor, with `ECONNABORTED`, and drains
    /// the listener (`ListenState.Drained`). Darwin's close returns only once
    /// such a call has, so an ended call holds nothing from the close on
    /// (`SleepTarget.EndedByClose`): the description goes at the close if no
    /// descriptor names it, the pipe's timestamps move as the call's return
    /// moves them, an ended write's `SIGPIPE` is generated for the process
    /// (refused where that would end the process, `CloseRefusal.DarwinEndedWriteSignal`),
    /// and the task, still parked, learns its answer from its finishing call. A pipe transfer that something had already woken is
    /// refused (`CloseRefusal.DarwinWokenTransfer`).
    ///
    /// `FileDescriptorRegistry.dropDescriptor` cannot do this itself: the
    /// socket table is the machine's rather than the process's, whether an
    /// inode is still named is a question about the filesystem, and what a
    /// sleeping call holds is the task table's.
    ///
    /// Every kqueue registration made through `fd` goes with it, in each kqueue
    /// the process owns (`KqueueState.Owner`) and in the kqueue of every Darwin
    /// `poll` asleep (`ParkedKqueuePoll`), whose entry then reports nothing.
    ///
    /// EBADF is its only errno; see `CloseRefusal` for the inputs it declines
    /// to answer at all.
    let close<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, CloseRefusal<'Task>>
        =
        match FileDescriptorRegistry.tryFindWithId fd (UnixSystemState.fileDescriptors system) with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some (closingId, closing) ->

        let parks =
            system.Tasks
            |> Map.toList
            |> List.choose (fun (task, state) -> state.Parked |> Option.map (fun park -> task, park.Syscall))

        let darwin =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> false
            | SimulatedUnixFlavour.Darwin -> true

        // Whether `target` is a sleep on the closing description that was made
        // through the closing descriptor.
        let enteredHere (target : SleepTarget<'Object>) : bool =
            match target with
            | SleepTarget.Waiting (description, entered) -> description = closingId && entered = fd
            | SleepTarget.EndedByClose _ -> false

        // Under Darwin, a close of the descriptor an flock was entered through
        // does not return until the flock has (measured,
        // `open-file-references.c` section D), and the park does not record
        // which descriptor that was, so any close onto the description
        // refuses. Checked against the park record rather than a task's run
        // state, so the window between a wake and the woken task's re-entry is
        // covered too.
        let darwinRefusal : CloseRefusal<'Task> option =
            if not darwin then
                None
            else
                parks
                |> List.tryPick (fun (task, parked) ->
                    match parked with
                    | ParkedSyscall.EpollWait wait when wait.Epoll = closingId ->
                        failwith
                            $"UnixDescriptor.close: task %O{task} is parked in an epoll_wait on %O{closingId} under the Darwin flavour, which has no epoll (this is a bug in the caller's state construction)."
                    | ParkedSyscall.Flock parked when parked.Requester = closingId ->
                        Some (CloseRefusal.DarwinFlockedDescriptorWithWaiter (closingId, task))
                    | ParkedSyscall.PipeRead read when enteredHere read.Reader ->
                        // Measured (`close-ends-call.c` sections P1, P6): the
                        // close ends the read with end of file, which is what a
                        // read woken by the last writer closing answers too.
                        match
                            WakeCondition.satisfied task (WakeCondition.ofPark parked) system
                            |> Set.filter (fun primitive ->
                                match primitive with
                                | WakePrimitive.PipeWriteEndClosed _ -> false
                                | _ -> true
                            )
                            |> Set.isEmpty
                        with
                        | true -> None
                        | false -> Some (CloseRefusal.DarwinWokenTransfer (closingId, task))
                    | ParkedSyscall.PipeWrite write when enteredHere write.Writer ->
                        // Measured (`close-ends-call.c` sections P2-P4): the
                        // close ends the write with EPIPE whatever it had put
                        // in, which is what a write woken by the last reader
                        // closing answers too.
                        match
                            WakeCondition.satisfied task (WakeCondition.ofPark parked) system
                            |> Set.filter (fun primitive ->
                                match primitive with
                                | WakePrimitive.PipeReadEndClosed _ -> false
                                | _ -> true
                            )
                            |> Set.isEmpty
                        with
                        | true -> None
                        | false -> Some (CloseRefusal.DarwinWokenTransfer (closingId, task))
                    | ParkedSyscall.EpollWait _
                    | ParkedSyscall.Kevent _
                    | ParkedSyscall.Accept _
                    | ParkedSyscall.PipeRead _
                    | ParkedSyscall.PipeWrite _
                    | ParkedSyscall.Flock _
                    | ParkedSyscall.Poll _
                    // A Darwin poll's filters go with the descriptor (below).
                    | ParkedSyscall.KqueuePoll _ -> None
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
                | ParkedSyscall.EpollWait _
                | ParkedSyscall.Kevent _
                | ParkedSyscall.KqueuePoll _
                | ParkedSyscall.Accept _
                | ParkedSyscall.PipeRead _
                | ParkedSyscall.PipeWrite _ -> None
            )

        match pollRefusal with
        | Some refusal -> Error refusal
        | None ->

        // Measured on Darwin 27.0.0 (`close-ends-call.c`): closing the
        // descriptor a sleeping call was made through ends the call before the
        // close returns. A pipe `read` or `write` ends alone, and every call
        // asleep through that descriptor with it (section P5), but none asleep
        // through a `dup` (P6, P7). An `accept` ends every accept asleep on the
        // listener, whichever descriptor it was made through (A3, A4), and
        // leaves the listener drained (A7). Each ended call has returned by the
        // time the close does, so it holds nothing from here on: its
        // description goes now if no descriptor names it, and the pipe's
        // timestamps move now (P8). What it answers waits in its park for its
        // finishing call.
        let drainsListener =
            darwin
            && parks
               |> List.exists (fun (_, parked) ->
                   match parked with
                   | ParkedSyscall.Accept accept -> enteredHere accept.Listener
                   | ParkedSyscall.EpollWait _
                   | ParkedSyscall.Kevent _
                   | ParkedSyscall.Flock _
                   | ParkedSyscall.Poll _
                   | ParkedSyscall.KqueuePoll _
                   | ParkedSyscall.PipeRead _
                   | ParkedSyscall.PipeWrite _ -> false
               )

        let pipeOfClosing () : PipeId =
            match closing.Target with
            | OpenFileTarget.Pipe (pipeId, _) -> pipeId
            | target ->
                failwith
                    $"UnixDescriptor.close: a pipe transfer sleeps through fd %d{fd}, which names %A{target} rather than a pipe end (this is a bug in this library, or in a caller that assembled the state by hand)."

        let ended (parked : ParkedSyscall) : ParkedSyscall option =
            match parked with
            | ParkedSyscall.Accept accept when
                drainsListener && SleepTarget.description accept.Listener = Some closingId
                ->
                match closing.Target with
                | OpenFileTarget.Socket socketId ->
                    Some (
                        ParkedSyscall.Accept
                            { accept with
                                Listener = SleepTarget.EndedByClose socketId
                            }
                    )
                | target ->
                    failwith
                        $"UnixDescriptor.close: an accept sleeps through fd %d{fd}, which names %A{target} rather than a socket (this is a bug in this library, or in a caller that assembled the state by hand)."
            | ParkedSyscall.PipeRead read when darwin && enteredHere read.Reader ->
                Some (
                    ParkedSyscall.PipeRead
                        { read with
                            Reader = SleepTarget.EndedByClose (pipeOfClosing ())
                        }
                )
            | ParkedSyscall.PipeWrite write when darwin && enteredHere write.Writer ->
                Some (
                    ParkedSyscall.PipeWrite
                        { write with
                            Writer = SleepTarget.EndedByClose (pipeOfClosing ())
                        }
                )
            | ParkedSyscall.Accept _
            | ParkedSyscall.PipeRead _
            | ParkedSyscall.PipeWrite _
            | ParkedSyscall.EpollWait _
            | ParkedSyscall.Kevent _
            | ParkedSyscall.Flock _
            | ParkedSyscall.Poll _
            | ParkedSyscall.KqueuePoll _ -> None

        let endedCalls =
            parks
            |> List.choose (fun (task, parked) -> ended parked |> Option.map (fun ended -> task, ended))

        let machine =
            (system.Machine, endedCalls)
            ||> List.fold (fun machine (_, ended) ->
                match ended with
                | ParkedSyscall.PipeRead {
                                             Reader = SleepTarget.EndedByClose pipeId
                                         } -> UnixMachineState.touchedByPipeRead pipeId machine
                | ParkedSyscall.PipeWrite {
                                              Writer = SleepTarget.EndedByClose pipeId
                                          } -> UnixMachineState.touchedByPipeWrite pipeId machine
                | _ -> machine
            )

        let machine =
            if not drainsListener then
                machine
            else
                match closing.Target with
                | OpenFileTarget.Socket socketId ->
                    let socket = UnixMachineState.socket socketId machine

                    match socket.Phase with
                    | SocketPhase.Listening listenState ->
                        { machine with
                            Sockets =
                                Map.add
                                    socketId
                                    { socket with
                                        Phase =
                                            SocketPhase.Listening
                                                { listenState with
                                                    Drained = true
                                                }
                                    }
                                    machine.Sockets
                        }
                    | phase ->
                        failwith
                            $"UnixDescriptor.close: an accept sleeps on socket %O{socketId}, which is in %A{phase} rather than listening (this is a bug in this library, or in a caller that assembled the state by hand)."
                | target ->
                    failwith
                        $"UnixDescriptor.close: an accept sleeps through fd %d{fd}, which names %A{target} rather than a socket (this is a bug in this library, or in a caller that assembled the state by hand)."

        // Each ended call keeps its park, for its finishing call to read, and
        // lets go of the hold it had on what it slept on.
        let ended =
            ({ system with
                Machine = machine
             },
             endedCalls)
            ||> List.fold (fun system (task, ended) ->
                match UnixTaskTable.parkOf task system.Tasks with
                | Some park ->
                    UnixParkState.setPark
                        task
                        { park with
                            Syscall = ended
                        }
                        system
                | None ->
                    failwith
                        $"UnixDescriptor.close: task %O{task} was parked a moment ago and is not now (this is a bug in this library)."
            )

        let machine = ended.Machine
        let tasks = ended.Tasks

        // Measured (`close-ends-call.c` sections P2-P4 and P7): each write the
        // close ends raises SIGPIPE for the process as it returns, which is
        // before the close returns, so the signal's disposition is the one it
        // has now, whatever it is by the time the write's task finishes.
        let signalled =
            (Ok system.Process.Signals, endedCalls)
            ||> List.fold (fun signals (task, ended) ->
                match signals, ended with
                | Error refusal, _ -> Error refusal
                | Ok signals,
                  ParkedSyscall.PipeWrite {
                                              Writer = SleepTarget.EndedByClose _
                                          } ->
                    if ProcessId.toInt32 system.Process.ProcessId = 1 then
                        Error (CloseRefusal.DarwinEndedWriteSignal (task, EndedWriteSignalRefusal.InitProcess))
                    else

                    match
                        SignalState.generate
                            system.Process.CoreDumps
                            system.Leader
                            (tasks |> Map.keys |> Set.ofSeq)
                            {
                                Signal = Signal.SIGPIPE
                                Target = ValueNone
                            }
                            signals
                    with
                    | Ok (SignalGeneration.ProcessContinues signals) -> Ok signals
                    | Ok (SignalGeneration.ProcessTerminated _) ->
                        Error (CloseRefusal.DarwinEndedWriteSignal (task, EndedWriteSignalRefusal.TerminatesProcess))
                    | Ok (SignalGeneration.ProcessStopped (signal, _)) ->
                        failwith
                            $"UnixDescriptor.close: generating %O{signal} for a pipe write the close ended stopped the process, but SIGPIPE's default is to terminate on every flavour (this is a bug in this library)."
                    | Error refusal ->
                        Error (CloseRefusal.DarwinEndedWriteSignal (task, EndedWriteSignalRefusal.Receiver refusal))
                | Ok signals, _ -> Ok signals
            )

        match signalled with
        | Error refusal -> Error refusal
        | Ok signals ->

        // Measured on Darwin 27.0.0 (`fcntl-dup.c`, WRITTEN rows): an ended
        // write that had moved bytes returns having written, which marks the
        // description it was made through, as any such write does; a dup that
        // keeps the description shows it.
        let openFiles =
            let endedHavingWritten =
                endedCalls
                |> List.exists (fun (_, ended) ->
                    match ended with
                    | ParkedSyscall.PipeWrite write -> write.Written > 0
                    | ParkedSyscall.PipeRead _
                    | ParkedSyscall.Accept _
                    | ParkedSyscall.EpollWait _
                    | ParkedSyscall.Kevent _
                    | ParkedSyscall.Flock _
                    | ParkedSyscall.Poll _
                    | ParkedSyscall.KqueuePoll _ -> false
                )

            if endedHavingWritten then
                OpenFileTable.mapStatus
                    closingId
                    (fun status ->
                        { status with
                            Written = true
                        }
                    )
                    machine.OpenFiles
            else
                machine.OpenFiles

        let system =
            { system with
                Machine =
                    { machine with
                        OpenFiles = openFiles
                    }
                Process =
                    { system.Process with
                        Signals = signals
                    }
                Tasks = tasks
            }

        let registry, destroyed =
            match
                FileDescriptorRegistry.dropDescriptor
                    system.Process.ProcessId
                    fd
                    (UnixSystemState.fileDescriptors system)
            with
            | Ok dropped -> dropped
            | Error FileDescriptorCloseError.BadFd ->
                failwith
                    $"UnixDescriptor.close: fd %d{fd} named open file description %O{closingId} (%A{closing.Target}) a moment ago, and the registry now calls it a bad descriptor (this is a bug in this library)."

        // `dropDescriptor` removed the kqueue registrations made through `fd`
        // from every kqueue the process owns; a sleeping Darwin poll's kqueue
        // is the call's own, so it loses them here (measured, `poll-timeout.c`
        // section E: the entry then reports nothing, and the poll sleeps on to
        // its timeout).
        let tasks = KqueuePoll.dropRegistrationsThrough fd system.Tasks

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
                        | ParkedSyscall.EpollWait _
                        | ParkedSyscall.Flock _
                        | ParkedSyscall.Poll _
                        | ParkedSyscall.KqueuePoll _
                        | ParkedSyscall.Accept _
                        | ParkedSyscall.PipeRead _
                        | ParkedSyscall.PipeWrite _ -> false
                    )

                if enteredHere then
                    FileDescriptorRegistry.mapOpenFiles (OpenFileTable.drainKqueue closingId) registry
                else
                    registry
            | OpenFileTarget.Epoll _
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Socket _
            | OpenFileTarget.Pipe _ -> registry

        let closed =
            { system with
                Tasks = tasks
            }
            |> UnixSystemState.withFileDescriptors registry

        match destroyed with
        | None -> Ok (SyscallAnswer.Completed 0L, closed)
        | Some destroyed ->

        match ObjectLifetime.releaseDestroyed destroyed closed with
        | Error refusal -> Error (CloseRefusal.Release refusal)
        | Ok released -> Ok (SyscallAnswer.Completed 0L, released)

    /// What an `fcntl(2)` command number names, among the commands modelled.
    [<RequireQualifiedAccess>]
    type private FcntlCommand =
        /// `F_DUPFD`, or with `closeOnExec` `F_DUPFD_CLOEXEC`.
        | DuplicateAtOrAbove of closeOnExec : bool
        /// `F_GETFD`.
        | GetDescriptorFlags
        /// `F_SETFD`.
        | SetDescriptorFlags
        /// `F_GETFL`.
        | GetStatusFlags
        /// `F_SETFL`.
        | SetStatusFlags

    let private decodeCommand (flavour : SimulatedUnixFlavour) (command : int) : FcntlCommand option =
        match command with
        | FcntlNumbering.DuplicateAtOrAbove -> Some (FcntlCommand.DuplicateAtOrAbove false)
        | FcntlNumbering.GetDescriptorFlags -> Some FcntlCommand.GetDescriptorFlags
        | FcntlNumbering.SetDescriptorFlags -> Some FcntlCommand.SetDescriptorFlags
        | FcntlNumbering.GetStatusFlags -> Some FcntlCommand.GetStatusFlags
        | FcntlNumbering.SetStatusFlags -> Some FcntlCommand.SetStatusFlags
        | command when command = FcntlNumbering.duplicateCloseOnExec flavour ->
            Some (FcntlCommand.DuplicateAtOrAbove true)
        | _ -> None

    /// The flavour's `O_NONBLOCK`.
    let private nonBlockingBit (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> OpenFlagNumbering.LinuxNonBlock
        | SimulatedUnixFlavour.Darwin -> OpenFlagNumbering.DarwinNonBlock

    /// The word `F_GETFL` reports for `description` on `platform`.
    let private statusWord (platform : SimulatedUnixPlatform) (description : OpenFileDescription) : int =
        let bit (condition : bool) (value : int) : int = if condition then value else 0
        let status = description.Status

        let accessMode =
            match description.AccessMode with
            | FileAccessMode.ReadOnly -> 0
            | FileAccessMode.WriteOnly -> 1
            | FileAccessMode.ReadWrite -> 2

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            let architecture = SimulatedUnixPlatform.architecture platform

            // Measured (`fcntl-dup.c`, KIND rows): a 64-bit kernel's `open(2)`
            // adds O_LARGEFILE to every description it makes, and nothing else
            // that makes a description does.
            let madeByOpen =
                match description.Target with
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.CharacterDevice _ -> true
                | OpenFileTarget.Pipe _
                | OpenFileTarget.Socket _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.Kqueue _ -> false

            accessMode
            ||| bit description.NonBlocking OpenFlagNumbering.LinuxNonBlock
            ||| bit status.DataSynchronous OpenFlagNumbering.LinuxDataSynchronous
            ||| bit status.Synchronous OpenFlagNumbering.LinuxSynchronous
            ||| bit status.OpenedDirectory (OpenFlagNumbering.linuxDirectory architecture)
            ||| bit status.OpenedNoFollow (OpenFlagNumbering.linuxNoFollow architecture)
            ||| bit madeByOpen (OpenFlagNumbering.linuxLargeFile architecture)
        | SimulatedUnixFlavour.Darwin ->
            // Measured (`fcntl-dup.c` and `open-flags.c`): Darwin keeps neither
            // O_DIRECTORY nor O_NOFOLLOW, and reports two bits of its own.
            accessMode
            ||| bit description.NonBlocking OpenFlagNumbering.DarwinNonBlock
            ||| bit status.Synchronous OpenFlagNumbering.DarwinSynchronous
            ||| bit status.DataSynchronous OpenFlagNumbering.DarwinDataSynchronous
            ||| bit status.Written OpenFlagNumbering.DarwinWritten
            ||| bit status.Flocked OpenFlagNumbering.DarwinFlocked

    /// Store `registry` as `system`'s descriptor table and open file descriptions.
    let private withRegistry<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        UnixSystemState.withFileDescriptors registry system

    /// `fcntl(fd, command, argument)`, for the commands that manage
    /// descriptors and status flags: `F_DUPFD`, `F_DUPFD_CLOEXEC`, `F_GETFD`,
    /// `F_SETFD`, `F_GETFL` and `F_SETFL`.
    ///
    /// `command` and `argument` are raw, as a caller of the flavour's libc
    /// passes them, in its `<fcntl.h>` numbering: Linux's `F_DUPFD_CLOEXEC` is
    /// 1030 and Darwin's 67, and the status flags are numbered as `open(2)`'s.
    ///
    /// A descriptor that is not open is `EBADF`, whatever the command. Then:
    ///
    /// - `F_GETFL` answers the access mode and the status flags of the
    ///   description: `O_NONBLOCK`, `O_SYNC` and `O_DSYNC` on both flavours;
    ///   on Linux, `O_DIRECTORY` and `O_NOFOLLOW` as `open(2)` was given them,
    ///   and `O_LARGEFILE` for every description `open(2)` made; on Darwin, its
    ///   own `0x10000` once the description has been written through
    ///   (`OpenFileStatus.Written`) and `0x4000` once a lock has been granted
    ///   to it (`OpenFileStatus.Flocked`).
    /// - `F_SETFL` sets `O_NONBLOCK` from `argument`, and on Darwin `O_SYNC`
    ///   and `O_DSYNC` too, and ignores every other bit the flavour does not
    ///   change, the access mode included; so a word `F_GETFL` reported may be
    ///   given back. A word that would set `O_APPEND` or `O_ASYNC`, or Linux's
    ///   `O_DIRECT` or `O_NOATIME`, is refused (`FcntlRefusal.UnmodelledStatusFlags`).
    ///   On a Darwin kqueue the flags are stored and the call answers `ENOTTY`.
    /// - `F_GETFD` answers the descriptor's `FD_CLOEXEC` (1), and on Darwin its
    ///   `FD_CLOFORK` (2); `F_SETFD` sets them from those bits of `argument`
    ///   and ignores the rest.
    /// - `F_DUPFD` makes the lowest descriptor not in use at or above
    ///   `argument` name the same description, with neither descriptor flag;
    ///   `F_DUPFD_CLOEXEC` gives it `FD_CLOEXEC`. A negative `argument` is
    ///   `EINVAL`.
    ///
    /// An `F_DUPFD` whose descriptor would lie at or above the bound
    /// (`SimulatedUnixPlatform.descriptorBound`) is refused
    /// (`FcntlRefusal.DescriptorLimit`), after `EBADF` and a negative
    /// argument's `EINVAL`. Every other command is refused
    /// (`FcntlRefusal.UnmodelledCommand`).
    let fcntl<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (command : int)
        (argument : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, FcntlRefusal>
        =
        let platform = system.Machine.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform
        let registry = UnixSystemState.fileDescriptors system

        // Measured on both (`fcntl-dup.c`, LIMIT rows): the descriptor is looked
        // up before the command is read, so an unknown command on a closed
        // descriptor is EBADF, and before F_DUPFD's argument is.
        match FileDescriptorRegistry.tryFindWithId fd registry with
        | None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some (id, description) ->

        match decodeCommand flavour command with
        | None -> Error (FcntlRefusal.UnmodelledCommand command)
        | Some FcntlCommand.GetStatusFlags ->
            Ok (SyscallAnswer.Completed (int64 (statusWord platform description)), system)
        | Some FcntlCommand.SetStatusFlags ->
            // Measured (`fcntl-dup.c`, SETFL rows, every bit on every kind):
            // these are the only bits each flavour's F_SETFL changes.
            let unmodelled =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    [
                        OpenFlagNumbering.LinuxAppend, UnmodelledStatusFlag.Append
                        OpenFlagNumbering.LinuxAsynchronous, UnmodelledStatusFlag.Asynchronous
                        OpenFlagNumbering.linuxDirect (SimulatedUnixPlatform.architecture platform),
                        UnmodelledStatusFlag.Direct
                        OpenFlagNumbering.LinuxNoAccessTime, UnmodelledStatusFlag.NoAccessTime
                    ]
                | SimulatedUnixFlavour.Darwin ->
                    [
                        OpenFlagNumbering.DarwinAppend, UnmodelledStatusFlag.Append
                        OpenFlagNumbering.DarwinAsynchronous, UnmodelledStatusFlag.Asynchronous
                    ]
                |> List.filter (fun (bit, _) -> argument &&& bit <> 0)
                |> List.map snd

            if not unmodelled.IsEmpty then
                Error (FcntlRefusal.UnmodelledStatusFlags (argument, unmodelled))
            else

            let registry =
                FileDescriptorRegistry.setNonBlocking fd (argument &&& nonBlockingBit flavour <> 0) registry

            match flavour with
            | SimulatedUnixFlavour.Linux -> Ok (SyscallAnswer.Completed 0L, withRegistry registry system)
            | SimulatedUnixFlavour.Darwin ->

            let registry =
                registry
                |> FileDescriptorRegistry.mapOpenFiles (
                    OpenFileTable.mapStatus
                        id
                        (fun status ->
                            { status with
                                Synchronous = argument &&& OpenFlagNumbering.DarwinSynchronous <> 0
                                DataSynchronous = argument &&& OpenFlagNumbering.DarwinDataSynchronous <> 0
                            }
                        )
                )

            // Measured: Darwin stores the flags and then asks the file to take
            // O_NONBLOCK, which a kqueue answers ENOTTY, every time.
            match description.Target with
            | OpenFileTarget.Kqueue _ -> Ok (SyscallAnswer.Failed UnixError.ENOTTY, withRegistry registry system)
            | OpenFileTarget.File _
            | OpenFileTarget.Directory _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Socket _
            | OpenFileTarget.CharacterDevice _
            | OpenFileTarget.Epoll _ -> Ok (SyscallAnswer.Completed 0L, withRegistry registry system)
        | Some FcntlCommand.GetDescriptorFlags ->
            let flags =
                match FileDescriptorRegistry.tryFindFlags fd registry with
                | Some flags -> flags
                | None ->
                    failwith
                        $"UnixDescriptor.fcntl: fd %d{fd} named a description a moment ago and has no flags now (this is a bug in this library)."

            let word =
                match flavour with
                | SimulatedUnixFlavour.Linux when flags.CloseOnFork ->
                    failwith
                        $"UnixDescriptor.fcntl: fd %d{fd} carries FD_CLOFORK under the Linux flavour, which has no such flag (this is a bug in this library)."
                | _ ->
                    (if flags.CloseOnExec then FcntlNumbering.CloseOnExec else 0)
                    ||| (if flags.CloseOnFork then
                             FcntlNumbering.DarwinCloseOnFork
                         else
                             0)

            Ok (SyscallAnswer.Completed (int64 word), system)
        | Some FcntlCommand.SetDescriptorFlags ->
            // Measured (`fcntl-dup.c`, SETFD rows, every bit): Linux keeps bit
            // 0 and Darwin bits 0 and 1, and every other bit is ignored.
            let flags =
                {
                    CloseOnExec = argument &&& FcntlNumbering.CloseOnExec <> 0
                    CloseOnFork =
                        match flavour with
                        | SimulatedUnixFlavour.Linux -> false
                        | SimulatedUnixFlavour.Darwin -> argument &&& FcntlNumbering.DarwinCloseOnFork <> 0
                }

            Ok (SyscallAnswer.Completed 0L, withRegistry (FileDescriptorRegistry.setFlags fd flags registry) system)
        | Some (FcntlCommand.DuplicateAtOrAbove closeOnExec) ->
            if argument < 0 then
                Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            else

            // Measured (`fcntl-dup.c`, LIMIT rows): EBADF and a negative
            // argument's EINVAL come ahead of the limit; an argument at or
            // above it is EINVAL, and a full table EMFILE, at a limit of the
            // bound, and a descriptor at a higher one.
            match FileDescriptorRegistry.room (SimulatedUnixPlatform.descriptorBound platform) argument 1 registry with
            | Error refusal -> Error (FcntlRefusal.DescriptorLimit refusal)
            | Ok () ->

            let flags =
                { DescriptorFlags.none with
                    CloseOnExec = closeOnExec
                }

            match FileDescriptorRegistry.dupAtOrAbove fd argument flags registry with
            | None ->
                failwith
                    $"UnixDescriptor.fcntl: no descriptor at or above %d{argument} is free, though one below the bound was a moment ago (this is a bug in this library)."
            | Some (newFd, registry) -> Ok (SyscallAnswer.Completed (int64 newFd), withRegistry registry system)

    /// `dup2` and `dup3` once the screens peculiar to `dup3` have passed: make
    /// `newFd` name the description `oldFd` names, with `flags`, closing it
    /// first if it is open.
    let private duplicateOnto<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (oldFd : int)
        (newFd : int)
        (flags : DescriptorFlags)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, Dup2Refusal<'Task>>
        =
        let registry = UnixSystemState.fileDescriptors system
        let bound = SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform

        // Measured (`fcntl-dup.c`, DUP2 rows): a negative target, and a source
        // that is not open, are each EBADF, and leave an open target open,
        // whatever the limit. A good source onto a target at or above the
        // limit is EBADF too, so onto one at or above the bound the answer
        // turns on the limit.
        if newFd < 0 || (FileDescriptorRegistry.tryFindId oldFd registry).IsNone then
            Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        elif oldFd = newFd then
            Ok (SyscallAnswer.Completed (int64 newFd), system)
        elif newFd >= bound then
            Error (
                Dup2Refusal.DescriptorLimit
                    {
                        Descriptor = newFd
                        Bound = bound
                    }
            )
        else

        // Measured (`fcntl-dup.c`, ONTO and SLEEP rows): an open target is
        // closed as close(2) closes it, on both flavours: its description is
        // released if nothing else holds it, and a call asleep through it fares
        // exactly as under close(2) (close-ends-call.c), Darwin's dup2 blocking
        // where its close blocks. So the close is close's own.
        let closed =
            match FileDescriptorRegistry.tryFindId newFd registry with
            | None -> Ok system
            | Some _ ->
                match close newFd system with
                | Error refusal -> Error (Dup2Refusal.ClosingTarget refusal)
                | Ok (SyscallAnswer.Completed _, system) -> Ok system
                | Ok (SyscallAnswer.Failed error, _) ->
                    failwith
                        $"UnixDescriptor.dup2: closing the open target %d{newFd} answered %O{error}, but close's only errno is EBADF for a descriptor that is not open (this is a bug in this library)."

        closed
        |> Result.map (fun system ->
            SyscallAnswer.Completed (int64 newFd),
            withRegistry
                (FileDescriptorRegistry.installAt oldFd newFd flags (UnixSystemState.fileDescriptors system))
                system
        )

    /// `dup2(2)`: make `newFd` name the open file description `oldFd` names,
    /// with neither descriptor flag, and answer `newFd`.
    ///
    /// A negative `newFd`, or an `oldFd` that is not open, is `EBADF`, and
    /// changes nothing. `dup2(fd, fd)` of an open `fd` answers `fd` and changes
    /// nothing, its flags included. An open `newFd` is closed first as
    /// `close(2)` closes it, with everything that does to the description it
    /// named and to calls asleep through it, and is refused where `close` is
    /// (`Dup2Refusal.ClosingTarget`); so is one naming the same description as
    /// `oldFd`, whose flags the `dup2` then clears.
    ///
    /// A `newFd` at or above the bound (`SimulatedUnixPlatform.descriptorBound`)
    /// is refused (`Dup2Refusal.DescriptorLimit`), after the `EBADF`s.
    let dup2<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (oldFd : int)
        (newFd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, Dup2Refusal<'Task>>
        =
        duplicateOnto oldFd newFd DescriptorFlags.none system

    /// `dup3(2)`: `dup2`, but `flags` may carry `O_CLOEXEC` in Linux's
    /// numbering, which gives `newFd` `FD_CLOEXEC`, and `oldFd` and `newFd`
    /// must differ.
    ///
    /// Any other bit in `flags` is `EINVAL`, ahead of everything; then the same
    /// descriptor on both sides is `EINVAL`, open or not; then `dup2`'s
    /// answers and refusals. Refused under the Darwin flavour, which has no
    /// `dup3`.
    let dup3<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (oldFd : int)
        (newFd : int)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, Dup3Refusal<'Task>>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (Dup3Refusal.NotProvided SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        // Measured (`fcntl-dup.c`, DUP2 rows): the flags first, then the same
        // descriptor on both sides, then dup2's EBADFs.
        if flags &&& ~~~OpenFlagNumbering.LinuxCloseOnExec <> 0 then
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        elif oldFd = newFd then
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        else

        let descriptorFlags =
            { DescriptorFlags.none with
                CloseOnExec = flags &&& OpenFlagNumbering.LinuxCloseOnExec <> 0
            }

        duplicateOnto oldFd newFd descriptorFlags system
        |> Result.mapError (fun refusal ->
            match refusal with
            | Dup2Refusal.ClosingTarget refusal -> Dup3Refusal.ClosingTarget refusal
            | Dup2Refusal.DescriptorLimit refusal -> Dup3Refusal.DescriptorLimit refusal
        )

    /// Set or clear `O_NONBLOCK` on the open file description `fd` names, as
    /// `fcntl(fd, F_GETFL)` and then `fcntl(fd, F_SETFL, word)` with the bit
    /// changed do; the flag is shared with every descriptor naming the
    /// description.
    ///
    /// `EBADF` for a descriptor that is not open. On a Darwin kqueue the flag
    /// is set and the answer is `ENOTTY` (see `SetNonBlockingAnswer.Failed`).
    let setNonBlocking<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (isNonBlocking : bool)
        (system : UnixSystem<'Task, 'Handler>)
        : SetNonBlockingAnswer * UnixSystem<'Task, 'Handler>
        =
        let bit = nonBlockingBit (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)

        // A word F_GETFL reported holds no flag F_SETFL refuses, since no
        // description carries one, so neither call is refused.
        let unrefused (operation : string) (result : Result<'a, FcntlRefusal>) : 'a =
            match result with
            | Ok answer -> answer
            | Error refusal ->
                failwith
                    $"UnixDescriptor.setNonBlocking: %s{operation} on fd %d{fd} was refused (%s{FcntlRefusal.describe refusal}), but it carries only what F_GETFL reported (this is a bug in this library)."

        match unrefused "F_GETFL" (fcntl fd FcntlNumbering.GetStatusFlags 0 system) with
        | SyscallAnswer.Failed error, system -> SetNonBlockingAnswer.Failed error, system
        | SyscallAnswer.Completed word, system ->

        let word =
            if isNonBlocking then
                int word ||| bit
            else
                int word &&& ~~~bit

        match unrefused "F_SETFL" (fcntl fd FcntlNumbering.SetStatusFlags word system) with
        | SyscallAnswer.Completed _, system -> SetNonBlockingAnswer.Set, system
        | SyscallAnswer.Failed error, system -> SetNonBlockingAnswer.Failed error, system

    /// Whether the open file description `fd` names carries `O_NONBLOCK`, as
    /// `fcntl(fd, F_GETFL)` reports it; `None` for a descriptor that is not
    /// open, which `F_GETFL` answers `EBADF`.
    let isNonBlocking<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : bool option
        =
        let bit = nonBlockingBit (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)

        match fcntl fd FcntlNumbering.GetStatusFlags 0 system with
        | Ok (SyscallAnswer.Completed word, _) -> Some (int word &&& bit <> 0)
        | Ok (SyscallAnswer.Failed UnixError.EBADF, _) -> None
        | Ok (SyscallAnswer.Failed error, _) ->
            failwith
                $"UnixDescriptor.isNonBlocking: F_GETFL on fd %d{fd} answered %O{error}, but its only errno is EBADF (this is a bug in this library)."
        | Error refusal ->
            failwith
                $"UnixDescriptor.isNonBlocking: F_GETFL on fd %d{fd} was refused (%s{FcntlRefusal.describe refusal}), but it is a modelled command (this is a bug in this library)."
