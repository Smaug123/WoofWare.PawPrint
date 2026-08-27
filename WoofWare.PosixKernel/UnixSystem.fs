namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// Everything one simulated POSIX process is, as a syscall sees it: the machine
/// it runs on, its own per-process state, and its tasks.
///
/// Generic in what names a task and what a signal handler is, for the same
/// reason `SignalState` is: those are the client's identities and this library
/// never learns them.
type UnixSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    {
        Machine : UnixMachineState
        Process : UnixProcessState<'Task, 'Handler>
        Tasks : Map<'Task, UnixTaskState>
    }

/// Which of the two non-portable `lseek` extensions a raw `whence` names on the
/// simulated platform. Linux numbers `SEEK_DATA` 3 and `SEEK_HOLE` 4; Darwin
/// transposes them, which is why the raw number alone names no operation.
[<RequireQualifiedAccess>]
type SeekExtension =
    | SeekData
    | SeekHole

/// Why this kernel will not answer an `lseek`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type LSeekRefusal =
    /// `SEEK_DATA` or `SEEK_HOLE` on a seekable file.
    | Sparseness of whence : int * meaning : SeekExtension
    /// `SEEK_END` on a directory.
    | DirectoryEnd of inode : InodeNumber

/// Why this kernel will not answer an `flock`.
///
/// Every case is a measured divergence between the two flavours that this
/// library models Linux's side of. Darwin's `flock` is unmodelled not because
/// its return codes are unknown — they are measured, and named in each case's
/// description — but because what they leave the *lock state* as is not, which
/// is what a model would have to commit to.
[<RequireQualifiedAccess>]
type FLockRefusal =
    /// Not exactly one of LOCK_SH/LOCK_EX/LOCK_UN, optionally with LOCK_NB.
    | DarwinMalformedOperation of operation : int
    /// A pipe, which is what this kernel models the standard streams as.
    | DarwinStandardStream of role : FileDescriptorRole
    /// A socket event port: an epoll descriptor on Linux, a kqueue on Darwin.
    | DarwinSocketEventPort
    | DarwinSocket of socket : SocketId
    /// An acquire by a description that already holds a lock. Only a conversion
    /// can expose the keep-versus-drop divergence, and only when it fails —
    /// refused on the request rather than on the outcome, so that the refusal is
    /// a property of what was asked rather than of who else held a lock.
    | DarwinConversion
    /// A blocking acquisition against a conflicting holder. Waiting is a
    /// scheduler feature rather than a filesystem one, and this library has no
    /// scheduler.
    ///
    /// Unlike every other case here, this one is not a gap in what has been
    /// *measured*: a real kernel parks the caller, and it does so having already
    /// dropped the caller's old lock, because `flock` removes before it
    /// establishes. That advance is discarded with the refusal — a refused call
    /// hands back no system at all, which is what stops a client continuing from
    /// a half-step — so a client that could park must not treat this as a park.
    /// When blocking gets an outcome of its own rather than a refusal, this case
    /// moves there and carries the advance with it.
    | WouldBlockIndefinitely of mode : FlockMode

/// Why this kernel will not commit a truncation.
[<RequireQualifiedAccess>]
type TruncationRefusal =
    /// Longer than this kernel can represent. A real filesystem answers without
    /// difficulty — measured on ext4 and APFS alike, `ftruncate` to three
    /// gigabytes succeeds and leaves a sparse file — so this is a limit of the
    /// model, and refusing beats reporting an errno no kernel would produce for
    /// that length.
    | ExceedsRepresentableLength of inode : InodeNumber * length : int64

/// Why this kernel will not close a descriptor.
///
/// Generic in what names a task because two of the three are about a task
/// parked in a wait, and which one that is cannot be recomputed by the client:
/// nothing stops two tasks parking on the same port, so a client repeating the
/// search could name a different one from the one this refusal is about.
[<RequireQualifiedAccess>]
type CloseRefusal<'Task> =
    /// The last descriptor onto a socket event port that `task` is parked in a
    /// wait on, under the Linux flavour.
    | LinuxLastPortDescriptorWithWaiter of port : OpenFileDescriptionId * task : 'Task
    /// Any descriptor onto a socket event port that `task` is parked in a wait
    /// on, under the Darwin flavour.
    | DarwinPortDescriptorWithWaiter of port : OpenFileDescriptionId * task : 'Task
    /// The last descriptor onto a listening socket whose accept queue still
    /// holds a connection whose client is open.
    | ListenerWouldResetUnacceptedClient of listener : SocketId * connection : ConnectionId * client : SocketId

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

/// What `read(2)` moved, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type ReadAnswer =
    /// The bytes to place in the caller's buffer; the entry point returns how
    /// many there are.
    ///
    /// Empty means the call moved nothing and **the buffer was not touched at
    /// all**, which is measured rather than incidental: `read(f, NULL, 5)` at
    /// end-of-file is 0 on both platforms, not EFAULT. A caller that
    /// dereferenced its buffer before checking for empty would turn that answer
    /// into a fault.
    | Completed of bytes : ImmutableArray<byte>
    /// The entry point returns -1 and the caller stores `error` wherever its
    /// libc keeps errno. The file offset does not move.
    | Failed of error : UnixError

/// Why this kernel will not answer a `read`.
[<RequireQualifiedAccess>]
type ReadRefusal =
    /// The buffer has no answer at the step the read reached.
    | Buffer of BufferRefusal
    /// A socket. Every answer a real kernel gives here is a claim about
    /// connection state, which this kernel does not model.
    | SocketConnectionState of socket : SocketId * domain : SocketDomain * kind : SocketKind

[<RequireQualifiedAccess>]
module ReadRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, which descriptor, and which of its own
    /// callers could have reached this.
    let describe (refusal : ReadRefusal) : string =
        match refusal with
        | ReadRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | ReadRefusal.SocketConnectionState (socket, domain, kind) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}). This kernel models no socket connection state, and `read(2)` on a socket is an answer about exactly that: measured on an unconnected socket it is ENOTCONN for a TCP socket, EINVAL on Linux against ENOTCONN on Darwin for a Unix-domain stream socket, and a block with no wake source for a datagram socket. Any constant here would become a lie the moment connection state is modelled."

/// What `write(2)` did, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type WriteAnswer =
    /// How many bytes moved, which the entry point returns. Never short: this
    /// kernel has nothing that could push back on a write, and its filesystem
    /// cannot run out of space.
    | Completed of written : int
    /// The entry point returns -1 and the caller stores `error` wherever its
    /// libc keeps errno.
    | Failed of error : UnixError

/// Whether a `write` reaches the point at which it reads the caller's buffer.
///
/// The question exists because a caller may not be able to produce the bytes
/// without failing: a foreign-function layer whose memory is not a flat array
/// has to resolve the pointer, and resolving it can be a mistake in itself. Every
/// answer a `write` gives *without* reading the buffer is therefore available
/// first, so that the caller extracts only when extraction is what a real kernel
/// would do.
[<RequireQualifiedAccess>]
type WriteAdmission =
    /// Answered without the buffer being read at all — a bad descriptor, an
    /// object with no write operation, a faulting address, or the zero-length
    /// no-op.
    | Answered of answer : WriteAnswer
    /// The copy is reached: extract exactly `count` bytes and pass them to
    /// `write`.
    | Transfer of count : int

/// Why this kernel will not answer a `write`.
[<RequireQualifiedAccess>]
type WriteRefusal =
    /// The buffer has no answer at the step the write reached.
    | Buffer of BufferRefusal
    /// A socket, reached with a buffer the screen did not answer for. What a
    /// real kernel says here depends on the socket's connection state and on its
    /// kind — three different errnos across the two flavours — and this kernel
    /// models none of it.
    | SocketConnectionState of socket : SocketId * domain : SocketDomain * kind : SocketKind
    /// The write would leave the file longer than this kernel can represent.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int

[<RequireQualifiedAccess>]
module WriteRefusal =
    // Its own function because `write` and `pwrite` reach the same limit from
    // different offsets and must say the same thing about it.
    let internal describeExceedsRepresentableLength (inode : InodeNumber) (offset : int64) (count : int) : string =
        $"writing %d{count} bytes at offset %d{offset} of inode %O{inode} would leave the file longer than the %d{VirtualFileSystem.maxFileLength} bytes this kernel can represent. A real filesystem answers this without difficulty -- measured on ext4 and APFS alike, a one-byte write at offset 2^40 succeeds and leaves a sparse 1 TB file -- so this is a limit of the model, and refusing beats reporting an errno no kernel would have produced."

    /// What this kernel knows about why it cannot complete a write. The client
    /// supplies its own half — which entry point, which descriptor, and what it
    /// would have to build or configure to lift the refusal.
    let describe (refusal : WriteRefusal) : string =
        match refusal with
        | WriteRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | WriteRefusal.SocketConnectionState (socket, domain, kind) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}). This kernel models no socket connection state, and `write(2)` on a socket is an answer about exactly that: measured on an unconnected socket it is EPIPE on Linux against ENOTCONN on Darwin for a TCP socket, ENOTCONN on both for a Unix-domain stream socket, and EDESTADDRREQ for a datagram socket. The Linux TCP row also raises SIGPIPE, though a runtime that ignores that signal process-wide sees only the errno."
        | WriteRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            describeExceedsRepresentableLength inode offset count

/// Why this kernel will not answer a `pwrite`.
///
/// `WriteRefusal` without its socket case, rather than the same type: a socket
/// is unseekable, so `pwrite` answers ESPIPE and never reaches the socket's own
/// write operation, and a shared type would hand every client an arm it could
/// not reach and would have to invent a message for.
[<RequireQualifiedAccess>]
type PWriteRefusal =
    /// The buffer has no answer at the step this `pwrite` reached: its screen, or
    /// the copy it never got to make.
    | Buffer of BufferRefusal
    /// The write would place its last byte past the longest file this kernel can
    /// represent. Easier to reach than `write`'s: the offset is an argument
    /// rather than a position the description was walked to.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int

[<RequireQualifiedAccess>]
module PWriteRefusal =
    /// What this kernel knows about why it cannot complete a `pwrite`. The client
    /// supplies its own half -- which entry point, which descriptor, and what it
    /// would have to build or configure to lift the refusal.
    let describe (refusal : PWriteRefusal) : string =
        match refusal with
        | PWriteRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | PWriteRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            // The same fact `write` reports, reached from an argument rather than
            // from the description's offset, so it says the same sentence.
            WriteRefusal.describeExceedsRepresentableLength inode offset count

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
            "the destination names no storage this caller can write, and this platform's `getcwd(3)` assembles the path with stores executed in the caller's own context rather than copying from the kernel. Measured against a `PROT_READ` page: Darwin dies on a signal (SIGSEGV unmapped, SIGBUS read-only) where Linux answers EFAULT. A dead process is not an errno, so there is nothing to report."

/// Why this kernel will not answer a syscall at all. The client decides what a
/// refusal means for it; nothing here is recoverable by retrying.
[<RequireQualifiedAccess>]
type SyscallRefusal<'Task> =
    | LSeek of LSeekRefusal
    | FLock of FLockRefusal
    | FTruncate of TruncationRefusal
    | Close of CloseRefusal<'Task>
    | Read of ReadRefusal

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
            $"inode %O{inode} is a directory, and was asked to seek relative to its end. A directory's size is a filesystem artefact rather than a fact about its contents, and there is no portable answer: measured, lseek(dir, 0, SEEK_END) is EINVAL on Linux/tmpfs, 4096 on Linux/ext4 and 64 on macOS/APFS. SEEK_SET and SEEK_CUR on a directory are portable and are supported."

[<RequireQualifiedAccess>]
module FLockRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, and which managed caller could have
    /// reached it.
    let describe (refusal : FLockRefusal) : string =
        match refusal with
        | FLockRefusal.DarwinMalformedOperation operation ->
            $"operation %d{operation} is malformed (not exactly one of LOCK_SH/LOCK_EX/LOCK_UN, optionally with LOCK_NB), which Linux rejects with EINVAL and Darwin does not treat uniformly -- measured, Darwin answers EBADF for 0, a bare LOCK_NB and unknown bits alone, but *succeeds* for LOCK_SH|LOCK_EX, LOCK_UN|LOCK_SH and LOCK_SH with an unknown bit."
        | FLockRefusal.DarwinStandardStream role ->
            $"the descriptor is the standard stream %O{role}, which this kernel models as a pipe. Linux permits `flock` on a pipe and returns 0; Darwin refuses it with ENOTSUP (raw 45, and note Darwin numbers ENOTSUP and EOPNOTSUPP differently, 45 against 102, while Linux gives both 95)."
        | FLockRefusal.DarwinSocketEventPort ->
            "the descriptor is a socket event port. Linux permits `flock` on an epoll descriptor and returns 0; Darwin refuses it on a kqueue with ENOTSUP (raw 45), for every operation including LOCK_UN."
        | FLockRefusal.DarwinSocket socket ->
            $"the descriptor is socket %O{socket}. Linux permits `flock` on a socket and returns 0; Darwin refuses it with ENOTSUP (raw 45)."
        | FLockRefusal.DarwinConversion ->
            "the descriptor is converting a lock it already holds. Should that conversion fail, Linux leaves the description holding *nothing* (`flock` removes the old lock before establishing the new one, and the two steps are not atomic) while Darwin leaves the old lock in place -- measured on both, and indistinguishable from the return code, which is EWOULDBLOCK either way."
        | FLockRefusal.WouldBlockIndefinitely mode ->
            let requested =
                match mode with
                | FlockMode.Shared -> "shared"
                | FlockMode.Exclusive -> "exclusive"

            $"a blocking %s{requested} lock was requested, and another open file description holds a conflicting one. This library cannot block a caller on a lock: that needs a scheduler to park it and wake it when the holder releases. If the holder is the same task, a real kernel would deadlock here rather than return. Pass LOCK_NB to get EWOULDBLOCK instead."

[<RequireQualifiedAccess>]
module CloseRefusal =
    /// What this kernel knows about why it cannot close the descriptor. The
    /// client supplies its own half — which entry point, which descriptor
    /// number, and what it would have to build to lift the refusal.
    let describe (refusal : CloseRefusal<'Task>) : string =
        match refusal with
        | CloseRefusal.LinuxLastPortDescriptorWithWaiter (port, task) ->
            $"it is the last descriptor onto socket event port %O{port}, and task %O{task} is parked in a wait on it. Measured, Linux's epoll_wait holds the port by file reference: the last close leaves the in-flight wait's registrations live, and a later edge can still complete it. Representing that needs the port to outlive its last descriptor, which this kernel's descriptor table cannot express."
        | CloseRefusal.DarwinPortDescriptorWithWaiter (port, task) ->
            $"the descriptor names socket event port %O{port}, and task %O{task} is parked in a wait on it. Measured, Darwin's kevent *ends* such a wait with an error when the fd it was entered through closes -- but which error is not measured precisely, and what a close of a *different* descriptor onto the same kqueue does is not measured at all."
        | CloseRefusal.ListenerWouldResetUnacceptedClient (listener, connection, client) ->
            $"the close destroys listening socket %O{listener} while connection %O{connection} sits unaccepted in its queue, and that connection's client (socket %O{client}) is still open. A real kernel RSTs the unaccepted client on listener close, leaving it in a state this kernel has not measured: its readiness level, and what connect(2) then answers, are both unknown, and it would otherwise be indistinguishable from a cleanly FIN'd peer."

[<RequireQualifiedAccess>]
module TruncationRefusal =
    let describe (refusal : TruncationRefusal) : string =
        match refusal with
        | TruncationRefusal.ExceedsRepresentableLength (inode, length) ->
            $"inode %O{inode} was asked to become %d{length} bytes, which is longer than the %d{VirtualFileSystem.maxFileLength} bytes this kernel can represent. A real filesystem answers this without difficulty -- measured on ext4 and APFS alike, ftruncate to three gigabytes succeeds and leaves a sparse file -- so this is a limit of the model, and refusing is better than reporting an errno no kernel would have produced for that length."

/// A request to this kernel, in the vocabulary of the kernel ABI rather than of
/// any client's foreign-function layer.
///
/// Arguments a real kernel validates arrive raw — `LSeek`'s `whence`, and every
/// `fd` — because rejecting them is behaviour this library models, and models
/// per flavour. Arguments only the client can classify arrive classified.
type Syscall =
    | GetEffectiveUserId
    | Dup of fd : int
    | LSeek of fd : int * offset : int64 * whence : int
    /// `operation` is raw: which combinations of LOCK_SH/LOCK_EX/LOCK_UN/LOCK_NB
    /// are legal, and what an illegal one earns, is behaviour this kernel models
    /// and models per flavour.
    | FLock of fd : int * operation : int
    | FTruncate of fd : int * length : int64
    | Close of fd : int
    /// `mode` is raw: the C shim passes it straight to `mkdir(2)`, so how it
    /// combines with the umask and with the parent's set-group-ID bit is
    /// behaviour this kernel models, and models per flavour.
    | MkDir of path : UnixPath * mode : int
    | Unlink of path : UnixPath
    | RmDir of path : UnixPath

/// What the entry point returns, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type SyscallAnswer =
    /// The entry point returns this.
    | Completed of answer : int64
    /// The entry point returns its failure sentinel, and the client stores
    /// `error` wherever its libc keeps errno.
    ///
    /// A failure still changes the system in general: `flock` advances the
    /// descriptor table before it can discover the conflict that fails it.
    | Failed of error : UnixError

/// What a `read` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
///
/// Narrower than `OpenFileTarget`: it excludes the descriptors a read refuses
/// outright, so a caller that screens the buffer between those two steps — as
/// `vfs_read` does — has no unreachable arm left to write.
[<RequireQualifiedAccess>]
type private ReadTarget =
    /// The read end of the pipe this kernel models standard input as.
    | Stdin
    /// A file, at the offset its open file description currently holds.
    | File of inode : InodeNumber * offset : int64
    /// A socket, which is refused rather than answered.
    | Socket of socket : SocketId

/// What a `write` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
[<RequireQualifiedAccess>]
type private WriteTarget =
    /// A file. The offset is the description's own, and the write advances it —
    /// which is the whole difference from `pwrite`.
    | File of inode : InodeNumber * offset : int64
    /// One of the standard streams, whose bytes this kernel records rather than
    /// storing.
    | StandardStream of role : FileDescriptorRole
    /// A socket, which is refused rather than answered — but only once the
    /// buffer screen has had its say, which on one flavour answers first.
    | Socket of socket : SocketId

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
    /// The descriptor names something with no file offset — a pipe, which is
    /// what this kernel models the standard streams as; `ESPIPE`.
    | NotSeekable

[<RequireQualifiedAccess>]
module UnixSystem =

    /// Every inode that must not be freed: `UnixProcessState.heldInodes`, closed under
    /// `DirectoryContent.Parent`.
    ///
    /// The closure is not caution — it is measured. `rmdir` can remove a
    /// directory something still holds, and that orphan keeps its "..": probed
    /// on both flavours, with `a/b` and the current directory inside `b`,
    /// `rmdir(b)` then `rmdir(a)` both succeed and `stat("..")` still answers
    /// `a`'s inode while `stat("../..")` still answers the live grandparent's.
    /// So a held orphan holds its whole ancestor chain, and freeing one of them
    /// would leave a `DirectoryContent.Parent` naming an inode the graph no
    /// longer contains.
    ///
    /// This is the set `VirtualFileSystem.checkInvariants` takes as `pinned`,
    /// and the check `forgetIfUnheld` makes before freeing an inode. Ancestors
    /// that are still reachable from the root are in it too, harmlessly: both
    /// callers only ever ask about an inode no name reaches.
    let pinnedInodes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Set<InodeNumber>
        =
        let rec climb (frontier : InodeNumber list) (seen : Set<InodeNumber>) : Set<InodeNumber> =
            match frontier with
            | [] -> seen
            | inode :: rest ->
                if Set.contains inode seen then
                    climb rest seen
                else

                let seen = Set.add inode seen

                match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                | Some (InodeContent.Directory directory) -> climb (directory.Parent :: rest) seen
                // A file or a link records no parent, and a held inode the graph
                // has already forgotten records nothing at all — which is a
                // defect (`EmulatedKernelDefect.DanglingOpenInode`) rather than
                // something to climb from.
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _)
                | None -> climb rest seen

        climb (UnixProcessState.heldInodes system.Process |> Set.toList) Set.empty

    /// Free `inode` if the filesystem no longer names it and this system holds
    /// no reference to it — what a real kernel does once the last link and the
    /// last descriptor have both gone.
    ///
    /// Total and idempotent: an inode that still has a name, that something
    /// still holds, or that is already gone, is left exactly as it was. Call it
    /// after anything that can drop a reference of either kind — removing a
    /// name, and closing a descriptor — because either may be the one that
    /// finishes the job, and which one that is cannot be known from the call
    /// site.
    ///
    /// Freeing a *directory* cascades onto its recorded parent, which the
    /// directory's ".." was the last reference to. So one call collects a whole
    /// orphaned chain, and the caller passes only the inode whose reference it
    /// just dropped.
    let rec forgetIfUnheld<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inode : InodeNumber)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        // The root is excluded explicitly rather than by the binding count,
        // which is zero for it by construction: nothing holds an entry naming
        // the root (`VirtualFileSystemDefect.RootHasIncomingLink` states that),
        // so the count alone would free the filesystem out from under every
        // path. A guest can reach here with it — `close(open("/"))` is an
        // ordinary thing to do.
        if inode = VirtualFileSystem.root system.Machine.FileSystem then
            system
        elif (VirtualFileSystem.tryGet inode system.Machine.FileSystem).IsNone then
            system
        elif VirtualFileSystem.bindingCount inode system.Machine.FileSystem <> 0 then
            system
        elif Set.contains inode (pinnedInodes system) then
            system
        else

        // Read before the removal, because it is the removal that makes the
        // parent's own reference count drop.
        let parent =
            match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
            | Some (InodeContent.Directory directory) -> Some directory.Parent
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _)
            | None -> None

        let freed =
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = VirtualFileSystem.forget inode system.Machine.FileSystem
                    }
            }

        // A directory freed here was the last thing holding its parent's ".."
        // reference, so the parent may now be free in turn — the chain a held
        // orphan kept alive is collected as soon as the last holder goes.
        // Terminating: each step has removed one inode, and the root is refused
        // above.
        match parent with
        | None -> freed
        | Some parent -> forgetIfUnheld parent freed

    /// The effective user ID, as `geteuid(2)` reports it.
    ///
    /// Total, and changes nothing: `geteuid` cannot fail, and this library
    /// models one identity for the whole process.
    let effectiveUserId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : uint32
        =
        system.Process.UserId

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
    /// Refuses the two inputs for which real kernels have been measured to
    /// disagree without a portable answer; see `LSeekRefusal`.
    let lseek<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (offset : int64)
        (whence : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, LSeekRefusal>
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        // `Interop.Sys.SeekWhence` (Interop.LSeek.cs), which is also POSIX's
        // numbering and both platforms' `<unistd.h>` — for these three. It
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
            | Some (OpenFileTarget.StandardStream _) ->
                // Not seekable: this kernel models the standard streams as
                // pipes, and `lseek` on a pipe is ESPIPE on both platforms
                // whichever end it is. This is the answer `SafeFileHandle` reads
                // back to decide `CanSeek`, so it is on the BCL's own path
                // rather than a corner.
                Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.SocketEventPort _) ->
                // The one target whose *seekability* depends on the platform,
                // rather than merely the errno or the ordering. Measured: Darwin
                // refuses `lseek` on a kqueue with ESPIPE, while Linux gives an
                // epoll descriptor `noop_llseek`, which succeeds and reports 0
                // without consulting the offset or moving anything. So Darwin
                // has a descriptor fault here and Linux has none; the Linux
                // success is served below, after the whence check the syscall
                // still applies.
                match flavour with
                | SimulatedUnixFlavour.Darwin -> Some DescriptorFault.NotSeekable
                | SimulatedUnixFlavour.Linux -> None
            | Some (OpenFileTarget.Socket _) ->
                // Unseekable on both, unlike the port above: measured, both
                // platforms answer ESPIPE for every whence in 0..4 and every
                // offset, `-1` and `INT64_MAX` alike. The whence-ordering
                // divergence still shows through this, and is exactly what the
                // ladder below reproduces — measured, `lseek(sock, 0, 9)` is
                // EINVAL on Linux (whence checked first) and ESPIPE on Darwin
                // (seekability checked first).
                Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.File _) -> None

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

        // Linux's `noop_llseek`, reached only under the Linux flavour (Darwin
        // answered ESPIPE above). It returns the file position unchanged, and an
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
        | Some (OpenFileTarget.SocketEventPort _) -> Ok (SyscallAnswer.Completed 0L, system)
        | _ ->

        // Whence *validity* is settled; whence *semantics* is not, and the two
        // sit at different points in Linux's order — which is why refusing 3 and
        // 4 up front would be wrong. Measured, `lseek(badfd, 0, 3)` is EBADF and
        // `lseek(pipe, 0, 3)` is ESPIPE on both platforms, so a guest reaching
        // here with whence 3 or 4 really is asking about a seekable file's
        // sparseness.
        //
        // No BCL caller can reach it: `Interop.Sys.SeekWhence` declares only 0,
        // 1 and 2.
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
                    $"UnixSystem.lseek: whence %d{whence} passed the validity and semantics checks but is not one of SEEK_SET, SEEK_CUR or SEEK_END (this is a bug in this library)"

        let inode, current =
            match target with
            | Some (OpenFileTarget.File (inode, current)) -> inode, current
            | _ ->
                failwith
                    $"UnixSystem.lseek: fd %d{fd} is not a seekable file, but the descriptor checks above did not reject it (this is a bug in this library)"

        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixSystem.lseek: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

        // The content is inspected only where a size is wanted, which is
        // `SEEK_END` alone. A directory has none this kernel will state, and a
        // symlink should not be here at all — but `SEEK_SET` and `SEEK_CUR` ask
        // neither question, so neither may fire on those paths.
        let sized : Result<int64 option, LSeekRefusal> =
            match seekWhence with
            | SeekWhence.Set
            | SeekWhence.Current -> Ok None
            | SeekWhence.End ->
                match entry.Content with
                | InodeContent.RegularFile (contents, _) -> Ok (Some (int64 contents.Length))
                | InodeContent.Symlink _ ->
                    // Not reachable: `open` resolves symlinks, so no descriptor
                    // names one. Stated rather than folded in so that an
                    // `O_PATH`-style descriptor finds a decision here.
                    failwith
                        $"UnixSystem.lseek: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what seeking a link through a descriptor means (this is a bug in this library)."
                | InodeContent.Directory _ -> Error (LSeekRefusal.DirectoryEnd inode)

        match sized with
        | Error refusal -> Error refusal
        | Ok forced ->

        let sizeOf =
            lazy
                match forced with
                | Some size -> size
                | None ->
                    failwith
                        "UnixSystem.lseek: the file size was consulted on a path that does not consult it (this is a bug in this library)"

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

        Ok (
            SyscallAnswer.Completed position,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = FileDescriptorRegistry.setOffset fd position system.Process.FileDescriptors
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
        let now = UnixMachineState.fileTimestamp system.Machine
        let rule = SimulatedUnixPlatform.setIdBitsOnTruncation system.Machine.UnixPlatform
        let privilege = UnixProcessState.callerPrivilege system.Process

        match VirtualFileSystem.truncateFile inode length rule privilege now system.Machine.FileSystem with
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
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _
        | OpenFileTarget.Socket _ ->
            // EINVAL on both platforms for every object that is not a regular
            // file: measured on a pipe (either end), an INET socket, a UNIX
            // socket, an epoll port and a kqueue. Unlike `pread`/`pwrite` there
            // is no unseekable-versus-unwritable tie for the platforms to break
            // differently, so this arm deliberately carries no Darwin flag.
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | OpenFileTarget.File (inode, _) ->

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

    /// `flock(2)`: take, convert or release an advisory lock on `fd`'s open file
    /// description.
    ///
    /// Models Linux's rules and refuses under Darwin rather than guessing, for
    /// each of the divergences `FLockRefusal` names.
    let flock<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (operation : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, FLockRefusal>
        =
        // Unlike a foreign-function layer's error and open-flag encodings, these
        // are *not* values anything translates: `flock(2)` takes them verbatim,
        // and Linux and Darwin happen to agree on all four — measured on both
        // rather than assumed.
        let lockShared = 1
        let lockExclusive = 2
        let lockNonBlocking = 4
        let lockUnlock = 8

        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
        let nonBlocking = operation &&& lockNonBlocking <> 0
        let mode = operation &&& ~~~lockNonBlocking

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
            | SimulatedUnixFlavour.Linux -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            | SimulatedUnixFlavour.Darwin -> Error (FLockRefusal.DarwinMalformedOperation operation)
        | Some request ->

        // The remaining divergences are all about a descriptor already resolved,
        // so they are checked here rather than in the registry: that module
        // models one coherent set of rules. An unknown fd is EBADF on both
        // platforms, so there is nothing to refuse for one.
        let darwinRefusal : FLockRefusal option =
            match flavour, FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | SimulatedUnixFlavour.Linux, _
            | _, None -> None
            | SimulatedUnixFlavour.Darwin, Some description ->
                match OpenFileDescription.object description with
                | OpenFileObject.StandardStream role -> Some (FLockRefusal.DarwinStandardStream role)
                | OpenFileObject.AnonymousInode -> Some FLockRefusal.DarwinSocketEventPort
                | OpenFileObject.Socket socketId -> Some (FLockRefusal.DarwinSocket socketId)
                | OpenFileObject.File _ ->
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
        | Some FlockError.BadFd -> Ok (SyscallAnswer.Failed UnixError.EBADF, advanced)
        | Some FlockError.WouldBlock ->
            if nonBlocking then
                Ok (SyscallAnswer.Failed UnixError.EAGAIN, advanced)
            else
                // A blocking acquisition that *can* be satisfied is served above,
                // so only genuine contention reaches here. Refusing must never
                // convert the request into a non-blocking one, which would hand
                // the caller an EWOULDBLOCK no kernel would have produced.
                //
                // `advanced` is deliberately dropped: see the case's own note.
                // The registry has already removed the caller's old lock, which
                // is what a real kernel does before it sleeps — but a refusal
                // carries no system, so a client cannot mistake this for a park.
                let requested =
                    if mode = lockShared then
                        FlockMode.Shared
                    else
                        FlockMode.Exclusive

                Error (FLockRefusal.WouldBlockIndefinitely requested)
        | None -> Ok (SyscallAnswer.Completed 0L, advanced)

    /// `close(2)`: drop `fd` from the process's table, together with the kernel
    /// objects the description it named was the last reference to — the socket,
    /// the connections nothing else references, and the inode whose last name
    /// had already gone.
    ///
    /// `FileDescriptorRegistry.close` cannot do this itself: the socket table is
    /// the machine's rather than the process's, and whether an inode is still
    /// named is a question about the filesystem. Closing one of several
    /// descriptors onto a description destroys nothing, and so frees neither.
    ///
    /// EBADF is its only errno; see `CloseRefusal` for the three inputs it
    /// declines to answer at all.
    let close<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, CloseRefusal<'Task>>
        =
        // Resolved before the close so both port refusals below can name what the
        // fd referred to.
        let closing = FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors

        match FileDescriptorRegistry.close fd system.Process.FileDescriptors with
        | Error FileDescriptorCloseError.BadFd -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Ok (registry, destroyed) ->

        // Closing a descriptor onto a port with a task parked in a wait on it is
        // where the flavours part, and each side is measured (PawPrint's
        // SocketEventWaitSurvivesCloseLinux.cs and its macOS run):
        //
        //   * Linux's epoll_wait holds the port by file reference — a close that
        //     leaves a dup changes nothing, and even the last close leaves the
        //     in-flight syscall's registrations alive for a later edge to
        //     complete. The dup case is modelled (the description survives and
        //     the wait completes); the last-close case would need retention this
        //     table does not represent, so it refuses.
        //   * Darwin's kevent *ends* with an error when the fd it was entered
        //     through closes (measured; which error, and what a close of a
        //     different descriptor onto the same kqueue does, are not), so any
        //     such close refuses.
        //
        // Checked against the parked-wait record rather than a task's run state,
        // so the window between a wake and the woken task's re-entry is covered
        // too.
        let portRefusal : CloseRefusal<'Task> option =
            match closing with
            | None -> None
            | Some (closingId, description) ->

            match description.Target with
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.File _
            | OpenFileTarget.Socket _ -> None
            | OpenFileTarget.SocketEventPort _ ->

            let waiter =
                system.Tasks
                |> Map.tryPick (fun task state ->
                    match state.ParkedSocketWait with
                    | Some wait when wait.Port = closingId -> Some task
                    | Some _
                    | None -> None
                )

            match waiter with
            | None -> None
            | Some task ->
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux ->
                    if destroyed.IsSome then
                        Some (CloseRefusal.LinuxLastPortDescriptorWithWaiter (closingId, task))
                    else
                        None
                | SimulatedUnixFlavour.Darwin -> Some (CloseRefusal.DarwinPortDescriptorWithWaiter (closingId, task))

        match portRefusal with
        | Some refusal -> Error refusal
        | None ->

        let socketEffects
            : Result<
                  Map<SocketId, SocketDescription> * Map<ConnectionId, TcpConnection> * SocketId list,
                  CloseRefusal<'Task>
               > =
            match destroyed with
            | None -> Ok (system.Machine.Sockets, system.Machine.Connections, [])
            | Some description ->

            match description.Target with
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.SocketEventPort _
            | OpenFileTarget.File _ -> Ok (system.Machine.Sockets, system.Machine.Connections, [])
            | OpenFileTarget.Socket socketId ->

            let dying =
                match Map.tryFind socketId system.Machine.Sockets with
                | Some socket -> socket
                | None ->
                    failwith
                        $"UnixSystem.close: fd %d{fd}'s description names socket %O{socketId}, which this system's socket table does not hold. Closing is the only operation here that removes a socket, and it removes it together with the description that named it, so a live descriptor onto an absent socket means the two tables were built out of step. There is nothing to repair it with: the objects this close would have released cannot be found (this is a bug in this library or in whatever assembled this system)."

            let sockets = Map.remove socketId system.Machine.Sockets

            // A connection lives while any socket phase or accept queue
            // references it. The dying socket may have been the last such
            // reference — directly, or by being the listener whose queue held it
            // (the queue dies with the listener, as Linux's
            // inet_csk_listen_stop discards a closed listener's accept queue).
            let candidates =
                match dying.Phase with
                | SocketPhase.Established connection
                | SocketPhase.EstablishedPendingReport connection -> [ connection ]
                | SocketPhase.Listening listenState -> listenState.Queue
                | SocketPhase.Idle
                | SocketPhase.RefusedPendingDelivery
                | SocketPhase.Dead
                | SocketPhase.DatagramPeer _ -> []

            let stillReferenced (connection : ConnectionId) : bool =
                sockets
                |> Map.exists (fun _ survivor ->
                    match survivor.Phase with
                    | SocketPhase.Established c
                    | SocketPhase.EstablishedPendingReport c -> c = connection
                    | SocketPhase.Listening listenState -> List.contains connection listenState.Queue
                    | SocketPhase.Idle
                    | SocketPhase.RefusedPendingDelivery
                    | SocketPhase.Dead
                    | SocketPhase.DatagramPeer _ -> false
                )

            // What this close does to the sockets sharing the dying socket's
            // connections splits by which end is dying. The peer of an
            // established pair sees the FIN: its level becomes the measured
            // half-closed IN|OUT|RDHUP and the driver signals it (`order3.c` row
            // Q) — collected here and signalled below, once the socket table
            // reflects the close, so the level the signal filters against is the
            // survivor's new one. A dying *listener* instead RSTs its unaccepted
            // queue entries' clients, whose resulting level is unmeasured — that
            // case refuses when a registration could observe it, and an RST
            // raises ERR, which no interest mask can hide, so any registration
            // could.
            let establishedSurvivors : Result<SocketId list, CloseRefusal<'Task>> =
                match dying.Phase with
                | SocketPhase.Established _
                | SocketPhase.EstablishedPendingReport _ ->
                    sockets
                    |> Map.toList
                    |> List.choose (fun (survivorId, survivor) ->
                        match survivor.Phase with
                        | SocketPhase.Established c
                        | SocketPhase.EstablishedPendingReport c when List.contains c candidates -> Some survivorId
                        | _ -> None
                    )
                    |> Ok
                | SocketPhase.Listening _ ->
                    // The first candidate with a live client, which is the one
                    // the old `for`-and-crash reported.
                    let refusal =
                        candidates
                        |> List.tryPick (fun candidate ->
                            sockets
                            |> Map.toSeq
                            |> Seq.filter (fun (_, survivor) ->
                                match survivor.Phase with
                                | SocketPhase.Established c
                                | SocketPhase.EstablishedPendingReport c -> c = candidate
                                | SocketPhase.Listening _
                                | SocketPhase.Idle
                                | SocketPhase.RefusedPendingDelivery
                                | SocketPhase.Dead
                                | SocketPhase.DatagramPeer _ -> false
                            )
                            |> Seq.map fst
                            |> Seq.tryHead
                            |> Option.map (fun survivor ->
                                CloseRefusal.ListenerWouldResetUnacceptedClient (socketId, candidate, survivor)
                            )
                        )

                    match refusal with
                    | Some refusal -> Error refusal
                    | None -> Ok []
                | SocketPhase.Idle
                | SocketPhase.RefusedPendingDelivery
                | SocketPhase.Dead
                | SocketPhase.DatagramPeer _ -> Ok []

            match establishedSurvivors with
            | Error refusal -> Error refusal
            | Ok establishedSurvivors ->

            let connections =
                (system.Machine.Connections, candidates)
                ||> List.fold (fun connections connection ->
                    if stillReferenced connection then
                        connections
                    else
                        Map.remove connection connections
                )

            Ok (sockets, connections, establishedSurvivors)

        match socketEffects with
        | Error refusal -> Error refusal
        | Ok (sockets, connections, establishedSurvivors) ->

        let closed =
            { system with
                Machine =
                    { system.Machine with
                        Sockets = sockets
                        Connections = connections
                    }
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        // The FIN's edge, raised now that the survivor's level is the
        // half-closed one. The signal filters by each registration's interest,
        // so a survivor nobody watches — or one watched only for conditions the
        // half-closed level does not meet — records nothing.
        let closed =
            (closed, establishedSurvivors)
            ||> List.fold (fun system survivor ->
                { system with
                    Process = UnixProcessState.signalSocketStateChange survivor system.Process
                }
            )

        // The close may have been the last reference to an inode whose last name
        // went away earlier, which is what keeps `read` on an unlinked descriptor
        // working right up until the descriptor goes. Reaped against the *closed*
        // system, so this description no longer counts as holding it.
        let reaped =
            match destroyed with
            | None -> closed
            | Some description ->

            match description.Target with
            | OpenFileTarget.File (inode, _) -> forgetIfUnheld inode closed
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.SocketEventPort _
            | OpenFileTarget.Socket _ -> closed

        Ok (SyscallAnswer.Completed 0L, reaped)

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
        VirtualFileSystem.resolveFull
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
    /// Shares `resolvePathFull`'s walk and `VirtualFileSystem.existingOf`'s
    /// free-name-is-ENOENT rule, rather than restating either.
    let resolvePath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<InodeNumber, UnixError>
        =
        resolvePathFull policy TrailingSeparatorPolicy.Demand path system
        |> Result.bind (fun resolution -> VirtualFileSystem.existingOf resolution.Target)

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
            match VirtualFileSystem.permissions entry with
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
                Mode =
                    VirtualFileSystem.fileTypeBits entry.Content
                    ||| PermissionBits.toInt permissions
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
                $"UnixSystem.stat: resolving %O{path} returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants (this is a bug in this library)."

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
                $"UnixSystem.fstat: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

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
                $"UnixSystem.getcwd: capacity %d{capacity} is negative, which no `getcwd(3)` can be asked for -- its size argument is a `size_t`. Screen this in the client, where the signature that admits a negative number lives (this is a bug in the caller)."

        /// The bytes a successful call would place, terminator included. Also
        /// what the comparison producing ERANGE is made against, so the two
        /// cannot disagree about whether the path fits.
        let terminated : ImmutableArray<byte> =
            (AbsoluteUnixPath.toUtf8 system.Process.CurrentDirectory).Add 0uy

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

        // Measured first on both, and it beats the removed-directory case below:
        // with the current directory gone, `getcwd(buf, 0)` is still EINVAL.
        if capacity = 0 then
            Ok (GetCwdAnswer.Failed UnixError.EINVAL)
        elif VirtualFileSystem.isOrphanedDirectory system.Process.CurrentDirectoryInode system.Machine.FileSystem then
            // The stored path is stale -- nothing reaches it any more -- so it is
            // not measured against the buffer. What the buffer can still change
            // is per-flavour; see `GetCwdOrphanAnswer`.
            match SimulatedUnixPlatform.getCwdOrphanAnswer system.Machine.UnixPlatform with
            | GetCwdOrphanAnswer.AlwaysDetached -> Ok (GetCwdAnswer.Failed UnixError.ENOENT)
            | GetCwdOrphanAnswer.ShortestPathFirst ->
                // Room for "/" and its terminator, which is what this flavour
                // writes before it starts climbing. Two bytes, not the length of
                // the path that used to be here.
                if capacity < 2 then
                    Ok (GetCwdAnswer.Failed UnixError.ERANGE)
                else
                    // It climbs, and so it writes -- which is why reaching the
                    // store makes an unwritable destination fatal here, exactly
                    // as on the success path, even though the call is going to
                    // fail. What it leaves behind is not modelled; see
                    // `GetCwdOrphanAnswer.ShortestPathFirst`.
                    transfer (GetCwdAnswer.Failed UnixError.ENOENT)
        elif capacity < terminated.Length then
            // `getcwd` needs room for the path *and* its NUL, which is why a
            // buffer of the path's own length is one byte short rather than an
            // exact fit. Measured with an unwritable destination too: this
            // answers before the destination is looked at.
            Ok (GetCwdAnswer.Failed UnixError.ERANGE)
        else
            transfer (GetCwdAnswer.Reported terminated)

    /// The object's own read operation on a regular file, which `read` and
    /// `pread` reach identically: the transfer window, the shortcut that touches
    /// no buffer at all, and the one point at which the buffer must hold bytes.
    /// What the two syscalls do *not* share is where `offset` comes from and
    /// whether the description's own offset then moves.
    ///
    /// The buffer screen has already had its say by here, so this is reached
    /// only with an address the flavour accepted.
    let private readFileAt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (syscall : string)
        (fd : int)
        (inode : InodeNumber)
        (offset : int64)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer, BufferRefusal>
        =
        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixSystem.%s{syscall}: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

        match entry.Content with
        | InodeContent.Directory _ ->
            // EISDIR on both, and behind the buffer screen rather than ahead of
            // it: measured, `read(dir, NULL, 5)` is EISDIR while
            // `read(dir, (void*)-1, 5)` is EFAULT under a screening flavour.
            Ok (ReadAnswer.Failed UnixError.EISDIR)
        | InodeContent.Symlink _ ->
            failwith
                $"UnixSystem.%s{syscall}: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what reading a link through a descriptor means (this is a bug in this library)."
        | InodeContent.RegularFile (contents, _) ->

        let transfer = VirtualFileSystem.readTransferCount offset count contents.Length

        if transfer = 0 then
            // Nothing moves, so the buffer is not consulted: measured,
            // `read(f, NULL, 5)` at end-of-file is 0 on both platforms rather
            // than EFAULT. A null pointer is an ordinary user address, so it
            // reaches here rather than being screened above.
            Ok (ReadAnswer.Completed ImmutableArray.Empty)
        else

        // The one point at which the buffer must actually hold bytes.
        match buffer with
        | UserBuffer.Unmapped _ ->
            // Measured: an EFAULT leaves the file's contents and the caller's
            // offset alone. A kernel faults in `copy_to_user`, after deciding
            // what it would have transferred but before consuming anything.
            Ok (ReadAnswer.Failed UnixError.EFAULT)
        | UserBuffer.Opaque -> Error BufferRefusal.OpaqueAtTransfer
        | UserBuffer.Addressless -> Error BufferRefusal.AddresslessAtTransfer
        | UserBuffer.Mapped ->

        // Indexed rather than `Seq.skip`, which would enumerate the whole prefix
        // on every read and make reading a file quadratic in its length.
        ImmutableArray.CreateRange (seq { for i in 0 .. transfer - 1 -> contents.[int offset + i] })
        |> ReadAnswer.Completed
        |> Ok

    /// `read(2)`: move up to `count` bytes from `fd`'s current offset into the
    /// caller's buffer, and advance the offset by what actually moved.
    ///
    /// `count` must not be negative. A negative count is a foreign-function
    /// layer's error rather than a kernel's — .NET's `Common_Read` returns
    /// before it evaluates the descriptor at all, so `read(badfd, buf, -1)` is
    /// EINVAL and not EBADF — and a client that models such a layer must answer
    /// it before asking here.
    ///
    /// The buffer is consulted at three points and *not* consulted at three
    /// others, and both sets are measured; see the comments inline.
    let read<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer * UnixSystem<'Task, 'Handler>, ReadRefusal>
        =
        if count < 0 then
            failwith
                $"UnixSystem.read: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it answers a negative count itself, before it looks at the descriptor. Reject it there."

        // The descriptor's access mode, which Linux's `vfs_read` decides before
        // it screens the buffer: measured on both platforms,
        // `read(wronlyFd, (void*)-1, 4)` is EBADF rather than EFAULT, and even
        // `read(wronlyFd, buf, 0)` is EBADF rather than a no-op.
        let target : Result<ReadTarget, UnixError> =
            match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | None -> Error UnixError.EBADF
            | Some description ->

            if not (FileAccessMode.permitsRead description.AccessMode) then
                // A regular file opened `O_WRONLY` and a pipe's write end alike:
                // EBADF on both platforms. `read` has no seekability
                // requirement, so unlike `pread` there is no tie for the
                // platforms to break differently.
                Error UnixError.EBADF
            else

            match description.Target with
            | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput -> Ok ReadTarget.Stdin
            | OpenFileTarget.StandardStream role ->
                failwith
                    $"UnixSystem.read: fd %d{fd} names standard stream %O{role}, whose access mode permits reading. This kernel models the output streams as the write ends of pipes, so only standard input is readable (this is a bug in this library)."
            | OpenFileTarget.SocketEventPort _ ->
                // A socket event port has no read operation, so the read is
                // refused for the *kind* of object rather than for the access
                // mode — which is why the port is `ReadWrite` and still gets
                // here rather than being EBADF above. The two platforms name
                // that refusal differently: measured, Linux answers EINVAL
                // (`vfs_read`'s `FMODE_CAN_READ` test) and Darwin answers ENXIO.
                //
                // Placed in this classification rather than after the buffer
                // screen because it precedes it on both: measured,
                // `read(port, (void*)-1, 8)` is EINVAL on Linux and ENXIO on
                // Darwin, not EFAULT. Length is irrelevant too —
                // `read(port, buf, 0)` gives the same answer as a non-zero
                // length, unlike standard input's zero-return shortcut below.
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
                | SimulatedUnixFlavour.Darwin -> Error UnixError.ENXIO
            | OpenFileTarget.Socket socketId -> Ok (ReadTarget.Socket socketId)
            | OpenFileTarget.File (inode, offset) -> Ok (ReadTarget.File (inode, offset))

        match target with
        | Error error -> Ok (ReadAnswer.Failed error, system)
        | Ok target ->

        // Everything below this point is the object's own read operation, which
        // on Linux the buffer screen precedes: hence EFAULT ahead of EISDIR, of
        // standard input's end-of-file and of a socket's connection state, and a
        // fault even for a zero-length request. Darwin screens nothing here, so
        // its answers come from the operation itself.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error (ReadRefusal.Buffer refusal)
        | Ok true -> Ok (ReadAnswer.Failed UnixError.EFAULT, system)
        | Ok false ->

        match target with
        | ReadTarget.Socket socketId ->
            // A zero-length read of a socket is where the flavours part, and it
            // is the one socket answer that needs no connection state — on one
            // of them. Measured across every phase this kernel can produce and
            // every kind it models, `read(sock, buf, 0)`:
            //
            //   socket state                     Linux   Darwin
            //   INET stream, idle                0       ENOTCONN
            //   UNIX stream, idle                0       ENOTCONN
            //   datagram, idle                   0       0
            //   INET stream, bound not listening 0       ENOTCONN
            //   INET stream, listening           0       ENOTCONN
            //   stream, connected, nothing queued 0      0
            //   stream, connected, a byte queued  0      0
            //   datagram, connected, empty        0      0
            //   datagram, connected, one queued   0      0
            //   stream, peer closed               0      0
            //
            // So **Linux answers 0 in every state**, which is why the flavour
            // alone decides it here: there is no phase or kind on which the
            // answer depends. Darwin's is 0 too except for a stream socket that
            // is not connected, and telling those apart means modelling exactly
            // the connection state this refusal exists to avoid — so Darwin
            // declines the whole class, which over-refuses the connected cases
            // and never answers wrongly.
            //
            // The same descriptors answer ENOTCONN (or EAGAIN, connected and
            // empty) at length 1, so the short-circuit is about the length
            // rather than the socket. The socket event port does not share it
            // and is answered above: measured, `read(port, buf, 0)` is EINVAL on
            // Linux like every other length.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, count with
            | SimulatedUnixFlavour.Linux, 0 -> Ok (ReadAnswer.Completed ImmutableArray.Empty, system)
            | SimulatedUnixFlavour.Linux, _
            | SimulatedUnixFlavour.Darwin, _ ->
                let socket = UnixMachineState.socket socketId system.Machine
                Error (ReadRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | ReadTarget.Stdin ->
            // **Immediate end-of-file**, and this is a claim about how the
            // process was launched rather than a fallback: this kernel models
            // standard input as the read end of a pipe whose write end was
            // closed by whoever started the process, so there is nothing to read
            // and never will be.
            //
            // The buffer is not consulted: measured on both platforms, a read
            // that returns end-of-file never touches it, so `read(0, NULL, 5)`
            // is 0 rather than EFAULT. Same rule as the transfer-window
            // shortcut below.
            Ok (ReadAnswer.Completed ImmutableArray.Empty, system)
        | ReadTarget.File (inode, offset) ->

        // The window is computed from the description's own offset, which is the
        // whole of what `pread` does differently; everything after it is the
        // same operation, so the two share it.
        match readFileAt "read" fd inode offset buffer count system with
        | Error refusal -> Error (ReadRefusal.Buffer refusal)
        | Ok (ReadAnswer.Failed error) -> Ok (ReadAnswer.Failed error, system)
        | Ok (ReadAnswer.Completed bytes) ->

        if bytes.IsEmpty then
            // Nothing moved, so the offset stays exactly where it was rather
            // than being clamped to the file's length or rewritten to itself.
            Ok (ReadAnswer.Completed bytes, system)
        else

        // Advanced by what actually moved, not by what was asked for: a short
        // read at the end of a file leaves the offset at the end rather than
        // past it, which is what makes a subsequent read return 0 instead of a
        // second short read.
        Ok (
            ReadAnswer.Completed bytes,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.setOffset
                                fd
                                (offset + int64 bytes.Length)
                                system.Process.FileDescriptors
                    }
            }
        )

    /// `pread(2)`: move up to `count` bytes from `offset` in the file `fd` names
    /// into the caller's buffer, without consulting or moving the description's
    /// own file offset.
    ///
    /// `count` must not be negative, for the reason `read`'s must not: a kernel
    /// never sees one, so whichever foreign-function layer produced it must
    /// answer it. That answer need not be `read`'s — a shim is free to validate
    /// one of the two and cast the other — which is why neither is given here.
    ///
    /// A negative `offset`, by contrast, *is* a request a kernel sees, and is
    /// EINVAL. Where in the order it is answered differs between the flavours,
    /// which is what makes this more than `read` with an extra argument.
    ///
    /// No system comes back, because a `pread` changes nothing in one: it moves
    /// no file offset, and nothing in this kernel moves `atime`.
    let pread<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer, BufferRefusal>
        =
        if count < 0 then
            failwith
                $"UnixSystem.pread: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it decides what a negative count means, before it looks at the descriptor. Reject it there."

        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        let offsetInvalid = offset < 0L

        // The order of the checks below is measured, and it differs between the
        // flavours. On a *single-fault* input they agree on every row; they part
        // company only when two things are wrong at once, which is why an
        // ordering has to be pinned at all:
        //
        //   input                          Linux    Darwin
        //   negative offset + bad fd       EINVAL   EBADF
        //   negative offset + pipe         EINVAL   ESPIPE
        //   negative offset + socket       EINVAL   ESPIPE
        //   negative offset + port         EINVAL   ESPIPE
        //   negative offset + O_WRONLY     EINVAL   EBADF
        //   negative offset + directory    EINVAL   EINVAL
        //   negative offset + bad address  EINVAL   EINVAL
        //
        // Linux validates the offset before it even looks the descriptor up
        // (`do_pread` checks `pos < 0` ahead of `fdget`); Darwin resolves the
        // descriptor, its seekability and its access mode first, and only then
        // the offset. Both orders are followed rather than one being imposed on
        // the other, because both are fully measured.
        //
        // `EISDIR` and the buffer screen both follow the offset check on
        // *both* — the last two rows — so only the descriptor steps actually
        // move, and one flag suffices rather than two orderings.
        let offsetCheckedBeforeDescriptor =
            match flavour with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        // The inode this `pread` will read from, once every question that
        // precedes the buffer screen has been settled. Only a file reaches it:
        // `pread` needs a seekable object, and a directory is one, so a
        // directory's EISDIR comes from the operation below rather than from
        // here.
        let target : Result<InodeNumber, UnixError> =
            if offsetCheckedBeforeDescriptor && offsetInvalid then
                Error UnixError.EINVAL
            else

            match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | None -> Error UnixError.EBADF
            | Some description ->

            // Whether this description was opened for reading at all. Two arms
            // below need it and neither may guess: for a standard stream it
            // breaks the ESPIPE/EBADF tie, and for a regular file it is the
            // whole answer.
            let readable = FileAccessMode.permitsRead description.AccessMode

            match description.Target with
            | OpenFileTarget.StandardStream _ ->
                // `pread` needs a seekable object, and this kernel models the
                // standard streams as pipes — standard input the read end,
                // output and error the write ends. Output and error therefore
                // fail two different tests at once: neither seekable nor open
                // for reading. Measured, the flavours break that tie
                // differently:
                //
                //   descriptor                        Linux    Darwin
                //   pipe read end (unseekable)        ESPIPE   ESPIPE
                //   pipe write end (also unreadable)  ESPIPE   EBADF
                //   regular file O_WRONLY (seekable)  EBADF    EBADF
                //
                // So Linux lets unseekability win and Darwin lets unreadability
                // win. The third row is the control that shows this is about the
                // tie rather than about readability generally, and it is the
                // `not readable` arm further down.
                match flavour with
                | SimulatedUnixFlavour.Darwin when not readable -> Error UnixError.EBADF
                | SimulatedUnixFlavour.Darwin
                | SimulatedUnixFlavour.Linux -> Error UnixError.ESPIPE
            | OpenFileTarget.SocketEventPort _ ->
                // Unseekable on both, with no tie to break: a port's description
                // is `ReadWrite`, so the unreadability arm above cannot apply to
                // it. Measured, `pread(port, buf, 8, 0)` and
                // `pread(port, buf, 0, 0)` are both ESPIPE on both flavours, and
                // so is `pread(port, (void*)-1, 8, 0)` — unseekability precedes
                // the buffer screen, which is why this is classified here rather
                // than after it.
                //
                // Note that this is *not* what `read` says of the same
                // descriptor, which is EINVAL on Linux and ENXIO on Darwin: the
                // object has no read operation at all, and `pread` never gets as
                // far as asking, having already failed on seekability.
                Error UnixError.ESPIPE
            | OpenFileTarget.Socket _ ->
                // Unseekable on both, for the same reason the port is, and
                // measured on a TCP, a UDP and a Unix-domain socket alike.
                //
                // Unlike `read`, this needs no connection state and so is an
                // answer rather than a refusal: every socket is unseekable
                // whatever it is connected to, so `pread` never reaches the
                // socket's own read operation.
                Error UnixError.ESPIPE
            | OpenFileTarget.File (inode, _) ->
                if not readable then
                    // A descriptor not open for reading: EBADF on both, which is
                    // `vfs_read`'s answer for a file whose `FMODE_READ` is
                    // clear.
                    //
                    // Ahead of Darwin's offset check rather than after it, and
                    // measured: `pread(wronlyFd, buf, 4, -1)` is EBADF on Darwin
                    // but EINVAL on Linux, so on Darwin the access mode is
                    // settled before the offset is looked at, exactly as
                    // seekability is above. On Linux this ordering cannot be
                    // observed, the offset check having already run.
                    Error UnixError.EBADF
                elif not offsetCheckedBeforeDescriptor && offsetInvalid then
                    // Darwin's turn to validate the offset: it has now resolved
                    // the descriptor, its seekability and its access mode, which
                    // is exactly the window in which it differs from Linux.
                    Error UnixError.EINVAL
                else
                    Ok inode

        match target with
        | Error error -> Ok (ReadAnswer.Failed error)
        | Ok inode ->

        // Everything below is the object's own read operation, which under a
        // screening flavour the buffer screen precedes: hence EFAULT ahead of
        // EISDIR and of the transfer window, and a fault even for a zero-length
        // request. The unscreened flavour discovers a bad address at the copy
        // instead.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error refusal
        | Ok true -> Ok (ReadAnswer.Failed UnixError.EFAULT)
        | Ok false ->

        readFileAt "pread" fd inode offset buffer count system

    /// What a `write` will operate on, once the descriptor's access mode has
    /// been checked: a file at its description's own offset, or a standard
    /// stream.
    let private writeTarget<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteTarget, UnixError>
        =
        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Error UnixError.EBADF
        | Some description ->

        if not (FileAccessMode.permitsWrite description.AccessMode) then
            // `write(2)` on a descriptor not open for writing is EBADF on both
            // platforms, and this precedes both the buffer screen and the
            // zero-size no-op: measured, `write(rdonlyFd, buf, 0)` is EBADF
            // rather than 0. It covers standard input — which a redirected
            // launch opens `O_RDONLY` — and a regular file opened `O_RDONLY`
            // alike, including a directory, which can only ever be opened for
            // reading.
            Error UnixError.EBADF
        else

        match description.Target with
        | OpenFileTarget.SocketEventPort _ ->
            // A socket event port has no write operation, so the refusal is for
            // the *kind* of object rather than for the access mode — the port
            // permits writing and so passes the EBADF arm above. Measured, Linux
            // answers EINVAL and Darwin ENXIO.
            //
            // Ahead of the buffer screen and of the zero-size no-op, on both
            // platforms: measured, `write(port, (void*)-1, 8)` is EINVAL/ENXIO
            // rather than EFAULT, and no length is a no-op.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
            | SimulatedUnixFlavour.Darwin -> Error UnixError.ENXIO
        | OpenFileTarget.Socket socketId -> Ok (WriteTarget.Socket socketId)
        | OpenFileTarget.File (inode, offset) -> Ok (WriteTarget.File (inode, offset))
        | OpenFileTarget.StandardStream role -> Ok (WriteTarget.StandardStream role)

    /// Every answer `write(2)` gives *without* reading the caller's buffer, and
    /// otherwise how many bytes to extract.
    ///
    /// Changes nothing: everything a write does before the copy is a question.
    /// See `WriteAdmission` for why this is a separate call rather than a
    /// `write` that takes the bytes.
    let admitWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission, WriteRefusal>
        =
        if count < 0 then
            failwith
                $"UnixSystem.admitWrite: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it answers a negative count itself, before it looks at the descriptor. Reject it there."

        match writeTarget fd system with
        | Error error -> Ok (WriteAdmission.Answered (WriteAnswer.Failed error))
        | Ok target ->

        // `vfs_write` screens the buffer between the access mode above and the
        // file operation, so on Linux this beats the zero-size no-op below:
        // measured, `write(1, (void*)-1, 0)` is EFAULT there and 0 on macOS.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error (WriteRefusal.Buffer refusal)
        | Ok true -> Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | Ok false ->

        // **After the screen, and before the zero-length no-op.** Both halves
        // are measured. Linux screens the address before the object's own write
        // operation, so `write(socket, (void*)-1, n)` there is EFAULT for every
        // `n` including 0: the screen answers and the socket is never consulted.
        // Darwin screens nothing, so the same call reaches the socket and earns
        // a connection-state answer (ENOTCONN for a stream socket,
        // EDESTADDRREQ for a datagram one), which is what this kernel cannot
        // give.
        //
        // And the no-op does *not* precede it: measured on both,
        // `write(socket, buf, 0)` is the socket's own error rather than 0.
        match target with
        | WriteTarget.Socket socketId ->
            let socket = UnixMachineState.socket socketId system.Machine
            Error (WriteRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | WriteTarget.File _
        | WriteTarget.StandardStream _ ->

        if count = 0 then
            // A no-op on both platforms, and specifically one that moves no
            // timestamp: measured, a zero-length write leaves `mtime` and
            // `ctime` where they were and does not extend the file, even at an
            // offset past its end. The buffer is not resolved: any address that
            // got past the screen is permitted, because it is not dereferenced.
            Ok (WriteAdmission.Answered (WriteAnswer.Completed 0))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // Real `write(2)` answers EFAULT for any non-dereferenceable
            // address, null included, having performed no I/O.
            Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | UserBuffer.Opaque -> Error (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (WriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (WriteAdmission.Transfer count)

    /// `write(2)`, given the bytes the caller extracted after `admitWrite` said
    /// to.
    ///
    /// Takes no buffer: every question about the caller's buffer is settled by
    /// `admitWrite`, and a signature that could not ask them again is the point.
    /// Still answers the descriptor questions itself, so a caller that skipped
    /// the admission gets a kernel's answer rather than an inconsistent one.
    ///
    /// Never short and never `EINTR`: this kernel has nothing that could push
    /// back on a write, and its filesystem cannot run out of space.
    let write<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAnswer * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixSystem.write: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        match writeTarget fd system with
        | Error error -> Ok (WriteAnswer.Failed error, system)
        | Ok (WriteTarget.Socket socketId) ->
            // There is no buffer here to screen, so the socket's own answer is
            // all there is — and this kernel cannot give it. A caller that used
            // `admitWrite` never reaches this: that call refused or answered
            // first.
            let socket = UnixMachineState.socket socketId system.Machine
            Error (WriteRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | Ok _ when bytes.IsEmpty ->
            // A no-op on both platforms, and specifically one that changes
            // nothing: measured, a zero-length write leaves `mtime` and `ctime`
            // where they were, does not extend the file, and does not strip the
            // set-ID bits. `admitWrite` answers this too, so the arm is
            // unreachable for a caller that used the pair — but a caller that
            // did not must get the same answer, and `VirtualFileSystem.writeFile`
            // below asserts a non-empty write precisely because it would
            // otherwise restamp the inode.
            //
            // After the descriptor checks, not before: `write(rdonlyFd, buf, 0)`
            // is EBADF rather than 0, measured on both.
            Ok (WriteAnswer.Completed 0, system)
        | Ok (WriteTarget.StandardStream role) ->
            Ok (
                WriteAnswer.Completed bytes.Length,
                { system with
                    Process =
                        { system.Process with
                            OutputLog =
                                system.Process.OutputLog.Add
                                    {
                                        OutputLogEntry.Role = role
                                        OutputLogEntry.Bytes = bytes
                                    }
                        }
                }
            )
        | Ok (WriteTarget.File (inode, offset)) ->

        let now = UnixMachineState.fileTimestamp system.Machine

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root; measured on both platforms, which
        // disagree only about `S_ISGID` on a file that is not group-executable.
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform
        let privilege = UnixProcessState.callerPrivilege system.Process

        match VirtualFileSystem.writeFile inode offset bytes rule privilege now system.Machine.FileSystem with
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            Error (WriteRefusal.ExceedsRepresentableLength (inode, offset, count))
        | Ok filesystem ->

        // At the description's own offset, and advancing it by what moved — the
        // entire difference from `pwrite`, which takes the offset as an argument
        // and leaves the description alone. Both measured.
        //
        // The commit comes first, so the advance cannot overflow: a write that
        // would carry the offset past what the model can represent has already
        // been refused there.
        Ok (
            WriteAnswer.Completed bytes.Length,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.setOffset
                                fd
                                (offset + int64 bytes.Length)
                                system.Process.FileDescriptors
                    }
            }
        )

    /// The inode a `pwrite` will write into, once every question that precedes
    /// the buffer screen has been settled.
    ///
    /// Only a regular file reaches it: `pwrite` needs a seekable object, and a
    /// directory can only ever be opened for reading, so `pwrite` to one is the
    /// access mode's EBADF rather than a kind's EISDIR.
    let private pwriteTarget<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<InodeNumber, UnixError>
        =
        // **Ahead of the descriptor, on both flavours** — which is exactly where
        // `pwrite` differs from `pread`, and it is measured rather than inferred
        // from the symmetry. Every second fault gives way to it:
        //
        //   negative offset with...    Linux    Darwin
        //   a bad descriptor           EINVAL   EINVAL
        //   a pipe's read end          EINVAL   EINVAL
        //   a pipe's write end         EINVAL   EINVAL
        //   a read-only file           EINVAL   EINVAL
        //   a directory                EINVAL   EINVAL
        //   a socket                   EINVAL   EINVAL
        //   a socket event port        EINVAL   EINVAL
        //   an unscreenable address    EINVAL   EINVAL
        //   a zero length              EINVAL   EINVAL
        //
        // `pread` needs a per-flavour flag for the same question, Darwin
        // resolving the descriptor first and answering EBADF or ESPIPE for these
        // shapes. Do not copy that flag here: the two syscalls genuinely differ.
        if offset < 0L then
            Error UnixError.EINVAL
        else

        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Error UnixError.EBADF
        | Some description ->

        // Whether this description was opened for writing at all. Two arms below
        // need it and neither may guess: for a standard stream it breaks the
        // ESPIPE/EBADF tie, and for a regular file it is the whole answer.
        let writable = FileAccessMode.permitsWrite description.AccessMode

        match description.Target with
        | OpenFileTarget.StandardStream _ ->
            // The mirror of `pread`'s tie, with the roles swapped: `pwrite` needs
            // a seekable object, this kernel models the standard streams as
            // pipes, and standard *input* therefore fails two tests at once —
            // neither seekable nor open for writing. Measured:
            //
            //   descriptor                        Linux    Darwin
            //   pipe write end (unseekable)       ESPIPE   ESPIPE
            //   pipe read end (also unwritable)   ESPIPE   EBADF
            //   regular file O_RDONLY (seekable)  EBADF    EBADF
            //
            // Linux lets unseekability win and Darwin lets unwritability win,
            // exactly as they do for `pread`. The third row is the control that
            // shows this is about the tie rather than about writability
            // generally, and it is the `not writable` arm further down.
            //
            // Ahead of the buffer screen on both, and measured that way rather
            // than assumed: `pwrite(pipeReadEnd, (void*)-1, 4, 0)` is ESPIPE on
            // Linux and EBADF on Darwin, not EFAULT.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin when not writable -> Error UnixError.EBADF
            | SimulatedUnixFlavour.Darwin
            | SimulatedUnixFlavour.Linux -> Error UnixError.ESPIPE
        | OpenFileTarget.SocketEventPort _ ->
            // Unseekable on both, with no tie to break: a port's description is
            // `ReadWrite`, so the unwritability arm above cannot apply to it.
            // Measured ESPIPE at length 8, at length 0, and with an unscreenable
            // address — so unseekability precedes both the no-op and the screen.
            //
            // Note that this is *not* what `write` says of the same descriptor,
            // which is EINVAL on Linux and ENXIO on Darwin: the object has no
            // write operation at all, and `pwrite` never gets as far as asking.
            Error UnixError.ESPIPE
        | OpenFileTarget.Socket _ ->
            // Unseekable on both, for the same reason the port is, and measured
            // on a TCP, a UDP and a Unix-domain socket alike.
            //
            // Unlike `write`, this needs no connection state and so is an answer
            // rather than a refusal: every socket is unseekable whatever it is
            // connected to, so `pwrite` never reaches the socket's own write
            // operation. That is why `PWriteRefusal` has no socket case.
            Error UnixError.ESPIPE
        | OpenFileTarget.File (inode, _) ->

        if not writable then
            // `vfs_write`'s EBADF for a descriptor whose `FMODE_WRITE` is clear,
            // and it precedes both the buffer screen and the zero-length no-op:
            // measured, `pwrite(rdonlyFd, (void*)-1, 4, 0)` is EBADF rather than
            // EFAULT and `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0.
            //
            // This is also what makes a directory unreachable below: one can only
            // be opened for reading, so it never gets past here.
            Error UnixError.EBADF
        else
            Ok inode

    /// Every answer `pwrite(2)` gives *without* reading the caller's buffer, and
    /// otherwise how many bytes to extract.
    ///
    /// Changes nothing: everything a write does before the copy is a question.
    /// See `WriteAdmission` for why this is a separate call rather than a
    /// `pwrite` that takes the bytes.
    ///
    /// Does not settle the length limit, which `pwrite` reports at the commit —
    /// so a caller can be told to extract bytes for a write that is then refused
    /// as unrepresentable. That costs the caller work rather than correctness,
    /// the refusal carrying no state, and it is what `admitWrite` does too.
    let admitPWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission, PWriteRefusal>
        =
        if count < 0 then
            failwith
                $"UnixSystem.admitPWrite: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it decides what a negative count means, before it looks at the descriptor. Reject it there."

        match pwriteTarget fd offset system with
        | Error error -> Ok (WriteAdmission.Answered (WriteAnswer.Failed error))
        | Ok _ ->

        // `vfs_write` screens the buffer between the access mode above and the
        // file operation, so under a screening flavour this beats the no-op
        // below: measured, `pwrite(f, (void*)-1, 0, 0)` is EFAULT on Linux and 0
        // on Darwin.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error (PWriteRefusal.Buffer refusal)
        | Ok true -> Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | Ok false ->

        if count = 0 then
            // A no-op on both flavours, and specifically one that moves no
            // timestamp: measured, a zero-length `pwrite` leaves `mtime` and
            // `ctime` where they were and does not extend the file, even at an
            // offset far past its end. The buffer is not resolved, because
            // nothing is read through it — a null pointer is an ordinary user
            // address, so it reaches here rather than being screened above, and
            // `pwrite(f, NULL, 0, 0)` is 0 on both.
            Ok (WriteAdmission.Answered (WriteAnswer.Completed 0))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // Real `pwrite(2)` answers EFAULT for any non-dereferenceable
            // address, null included, having performed no I/O: measured,
            // `pwrite(f, NULL, 4, 0)` is EFAULT on both, where the same pointer
            // at length 0 is a no-op.
            Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | UserBuffer.Opaque -> Error (PWriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (PWriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (WriteAdmission.Transfer count)

    /// `pwrite(2)`, given the bytes the caller extracted after `admitPWrite` said
    /// to: place them at `offset` without consulting or moving the description's
    /// own file offset.
    ///
    /// Takes no buffer, for the reason `write` does not: every question about the
    /// caller's buffer is settled by the admission, and a signature that could
    /// not ask them again is the point. Still answers the descriptor questions
    /// itself, so a caller that skipped the admission gets a kernel's answer
    /// rather than an inconsistent one.
    ///
    /// A system comes back, unlike `pread`'s: the offset does not move, but the
    /// file's contents and timestamps do.
    ///
    /// Never short and never `EINTR`: this kernel has nothing that could push
    /// back on a write, and its filesystem cannot run out of space.
    let pwrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAnswer * UnixSystem<'Task, 'Handler>, PWriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixSystem.pwrite: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        match pwriteTarget fd offset system with
        | Error error -> Ok (WriteAnswer.Failed error, system)
        | Ok inode ->

        if bytes.IsEmpty then
            // A no-op that changes nothing, and *after* the descriptor checks:
            // measured, `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0.
            // `admitPWrite` answers this too, so the arm is unreachable for a
            // caller that used the pair — but a caller that did not must get the
            // same answer, and `VirtualFileSystem.writeFile` below asserts a
            // non-empty write precisely because it would otherwise restamp the
            // inode.
            Ok (WriteAnswer.Completed 0, system)
        else

        let now = UnixMachineState.fileTimestamp system.Machine

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root, exactly as `write`'s does: the bits
        // follow the content changing, not which syscall changed it.
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform
        let privilege = UnixProcessState.callerPrivilege system.Process

        match VirtualFileSystem.writeFile inode offset bytes rule privilege now system.Machine.FileSystem with
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            Error (PWriteRefusal.ExceedsRepresentableLength (inode, offset, count))
        | Ok filesystem ->

        // The description is left exactly where it was — the whole of what
        // `pwrite` does differently from `write`, and measured on both flavours.
        Ok (
            WriteAnswer.Completed bytes.Length,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
        )

    /// `mkdir(2)`: bind a new directory at `path`.
    ///
    /// `mode` is raw — the shim passes it straight through — so what the created
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
        match resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator path system with
        | Error error -> SyscallAnswer.Failed error, system
        | Ok resolution ->

        match
            MkDirRules.verdict (UnixProcessState.callerPrivilege system.Process) resolution system.Machine.FileSystem
        with
        | MkDirVerdict.Refuse error -> SyscallAnswer.Failed error, system
        | MkDirVerdict.Create (directory, name, parentPermissions) ->

        let permissions =
            MkDirRules.createdPermissions rules parentPermissions system.Process.Umask mode

        let now = UnixMachineState.fileTimestamp system.Machine

        match VirtualFileSystem.createDirectory directory name permissions now system.Machine.FileSystem with
        | Error error ->
            // `createDirectory` refuses a name the directory already holds, and a
            // parent that is not a directory. The walk has just established
            // neither is the case, so either is a broken graph rather than
            // something the caller did.
            failwith
                $"UnixSystem.mkdir: creating \"%s{FileName.toString name}\" in inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and does not hold that name (this is a bug in this library)."
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
        match resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator path system with
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

        let now = UnixMachineState.fileTimestamp system.Machine

        match VirtualFileSystem.unbind UnbindTargetEffect.LostALink directory name now system.Machine.FileSystem with
        | Error error ->
            // `unbind` refuses a directory it does not hold and a name that
            // directory does not bind. The walk has just established both, so
            // either is a broken graph rather than something the caller did.
            failwith
                $"UnixSystem.unlink: removing \"%s{FileName.toString name}\" from inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and holds that name (this is a bug in this library)."
        | Ok (target, filesystem) ->

        // The name is gone; whether the *inode* is depends on whether any other
        // name or any open descriptor still holds it. A real `unlink` of a file
        // something has open leaves it readable through that descriptor until the
        // last one closes.
        SyscallAnswer.Completed 0L,
        forgetIfUnheld
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
        match resolvePathFull SymlinkPolicy.NoFollowFinal rules.TrailingSeparator path system with
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

        let now = UnixMachineState.fileTimestamp system.Machine

        match VirtualFileSystem.unbind rules.RemovedDirectoryEffect directory name now system.Machine.FileSystem with
        | Error error ->
            failwith
                $"UnixSystem.rmdir: removing \"%s{FileName.toString name}\" from inode %O{directory} was refused with %O{error}, but the walk had just established that the directory exists and holds that name (this is a bug in this library)."
        | Ok (target, filesystem) ->

        // A directory has only ever had the one name, so this was the last — but
        // a descriptor or the current directory may still hold it, and a real
        // `rmdir` leaves such an orphan usable through what holds it.
        // `forgetIfUnheld` also collects the ancestors this directory's ".." was
        // keeping alive.
        SyscallAnswer.Completed 0L,
        forgetIfUnheld
            target
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }

    /// Answer one syscall.
    ///
    /// Sugar over the per-syscall functions above, for a client that wants one
    /// surface — to log every syscall, to replay a recorded sequence, or to
    /// generate them. Where a syscall's own function has a narrower type (the
    /// answer to `GetEffectiveUserId` cannot be a failure, and `Dup` cannot be
    /// refused), that type is the one to prefer.
    ///
    /// **Not every syscall this module answers is reachable through here.** A
    /// syscall whose answer carries more than an integer — `read`, whose answer
    /// carries bytes — has no case in `Syscall`, because `SyscallAnswer` would
    /// have to grow a shape for it and nothing yet consumes that shape. Adding
    /// one for its own sake would be inventing an encoding before there is a
    /// client to be wrong about; the first thing that genuinely logs or replays
    /// a buffer-carrying syscall gets to choose it. Until then those syscalls
    /// are reached through their own functions, which lose nothing.
    let step<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (call : Syscall)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SyscallRefusal<'Task>>
        =
        match call with
        | Syscall.GetEffectiveUserId -> Ok (SyscallAnswer.Completed (int64 (effectiveUserId system)), system)
        | Syscall.Dup fd -> Ok (dup fd system)
        | Syscall.LSeek (fd, offset, whence) -> lseek fd offset whence system |> Result.mapError SyscallRefusal.LSeek
        | Syscall.FLock (fd, operation) -> flock fd operation system |> Result.mapError SyscallRefusal.FLock
        | Syscall.FTruncate (fd, length) -> ftruncate fd length system |> Result.mapError SyscallRefusal.FTruncate
        | Syscall.Close fd -> close fd system |> Result.mapError SyscallRefusal.Close
        | Syscall.MkDir (path, mode) -> Ok (mkdir path mode system)
        | Syscall.Unlink path -> Ok (unlink path system)
        | Syscall.RmDir path -> Ok (rmdir path system)
