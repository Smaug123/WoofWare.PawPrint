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
    /// A socket. Every answer a real kernel gives here is a claim about
    /// connection state, which this kernel does not model.
    | SocketConnectionState of socket : SocketId * domain : SocketDomain * kind : SocketKind
    /// The write would leave the file longer than this kernel can represent.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int

[<RequireQualifiedAccess>]
module WriteRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, which descriptor, and which of its own
    /// callers could have reached this.
    let describe (refusal : WriteRefusal) : string =
        match refusal with
        | WriteRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | WriteRefusal.SocketConnectionState (socket, domain, kind) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}). This kernel models no socket connection state, and `write(2)` on a socket is an answer about exactly that: measured on an unconnected socket it is EPIPE on Linux against ENOTCONN on Darwin for a TCP socket, ENOTCONN on both for a Unix-domain stream socket, and EDESTADDRREQ for a datagram socket. The Linux TCP row also raises SIGPIPE, though a runtime that ignores that signal process-wide sees only the errno."
        | WriteRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            $"writing %d{count} bytes at offset %d{offset} of inode %O{inode} would leave the file longer than the %d{VirtualFileSystem.maxFileLength} bytes this kernel can represent. A real filesystem answers this without difficulty -- measured on ext4 and APFS alike, a one-byte write at offset 2^40 succeeds and leaves a sparse 1 TB file -- so this is a limit of the model, and refusing beats reporting an errno no kernel would have produced."

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
        | Ok (ReadTarget.Socket socketId) ->
            let socket = UnixMachineState.socket socketId system.Machine
            Error (ReadRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | Ok target ->

        // Everything below this point is the file operation, which on Linux the
        // buffer screen precedes: hence EFAULT ahead of both EISDIR and standard
        // input's end-of-file, and a fault even for a zero-length request.
        // Darwin screens nothing here, so its answers come from the operation
        // itself.
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
        | ReadTarget.Socket _ ->
            failwith "UnixSystem.read: a socket was refused above and cannot reach here (this is a bug in this library)"
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

        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixSystem.read: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

        match entry.Content with
        | InodeContent.Directory _ ->
            // EISDIR on both, and ahead of the buffer: measured,
            // `read(dir, NULL, 5)` is EISDIR rather than EFAULT.
            Ok (ReadAnswer.Failed UnixError.EISDIR, system)
        | InodeContent.Symlink _ ->
            failwith
                $"UnixSystem.read: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what reading a link through a descriptor means (this is a bug in this library)."
        | InodeContent.RegularFile (contents, _) ->

        // The same window `pread` computes, from the description's offset rather
        // than from an argument — which is the entire difference between the two
        // syscalls.
        let transfer = VirtualFileSystem.readTransferCount offset count contents.Length

        if transfer = 0 then
            // Nothing moves, so neither the buffer nor the offset is touched:
            // measured, `read(f, NULL, 5)` at end-of-file is 0 on both
            // platforms, and the offset stays where it was rather than being
            // clamped to the file's length. A null pointer is an ordinary user
            // address, so it reaches here rather than being screened above.
            Ok (ReadAnswer.Completed ImmutableArray.Empty, system)
        else

        // The one point at which the buffer must actually hold bytes.
        match buffer with
        | UserBuffer.Unmapped _ ->
            // Measured: an EFAULT leaves the offset alone. A kernel faults in
            // `copy_to_user`, after deciding what it would have transferred but
            // before consuming anything.
            Ok (ReadAnswer.Failed UnixError.EFAULT, system)
        | UserBuffer.Opaque -> Error (ReadRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (ReadRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped ->

        let bytes =
            ImmutableArray.CreateRange (seq { for i in 0 .. transfer - 1 -> contents.[int offset + i] })

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
                            FileDescriptorRegistry.setOffset fd (offset + int64 transfer) system.Process.FileDescriptors
                    }
            }
        )

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
