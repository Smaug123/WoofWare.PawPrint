namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// A request to this kernel, in the vocabulary of the kernel ABI rather than of
/// any client's foreign-function layer.
///
/// Arguments a real kernel validates arrive raw — `LSeek`'s `whence`, and every
/// `fd` — because rejecting them is behaviour this library models, and models
/// per flavour. Arguments only the client can classify arrive classified.
///
/// Every pathname is the argument's bytes (`PathArgumentBytes`), which this
/// kernel copies in at the point the call's kernel does.
type Syscall =
    | GetEffectiveUserId
    | GetEffectiveGroupId
    | GetProcessId
    | Dup of fd : int
    /// `command` and `argument` are raw, in the flavour's `<fcntl.h>`
    /// numbering; see `UnixDescriptor.fcntl`.
    | Fcntl of fd : int * command : int * argument : int
    | Dup2 of oldFd : int * newFd : int
    /// `flags` is raw, in Linux's numbering.
    | Dup3 of oldFd : int * newFd : int * flags : int
    | LSeek of fd : int * offset : int64 * whence : int
    /// `operation` is raw: which combinations of LOCK_SH/LOCK_EX/LOCK_UN/LOCK_NB
    /// are legal, and what an illegal one earns, is behaviour this kernel models
    /// and models per flavour.
    | FLock of fd : int * operation : int
    | FTruncate of fd : int * length : int64
    | Close of fd : int
    /// `dirfd` and `mode` are raw, as `mkdirat(2)` takes them: how `mode`
    /// combines with the umask and with the parent's set-group-ID bit is
    /// behaviour this kernel models, and models per flavour. `mkdir(2)` is
    /// this with the flavour's `AT_FDCWD` (`AtDirectory.atFdCwd`).
    | MkDirAt of dirfd : int * path : PathArgumentBytes * mode : int
    /// `dirfd` and `flags` are raw, as `unlinkat(2)` takes them: each flavour
    /// numbers `AT_FDCWD` and `AT_REMOVEDIR` its own way, and which other bits
    /// it rejects is behaviour this kernel models per flavour. `unlink(2)` is
    /// this with the flavour's `AT_FDCWD` (`AtDirectory.atFdCwd`) and no
    /// flags, and `rmdir(2)` with `AT_REMOVEDIR` (`UnlinkAtRules.atRemoveDir`).
    | UnlinkAt of dirfd : int * path : PathArgumentBytes * flags : int
    | ChDir of path : PathArgumentBytes
    /// `dirfd`, `mode` and `flags` are raw, as `fchmodat(2)` takes them: each
    /// flavour numbers `AT_FDCWD` and the flags its own way, and which bits of
    /// `mode` the inode gets is behaviour this kernel models. `chmod(2)` is
    /// this with the flavour's `AT_FDCWD` (`AtDirectory.atFdCwd`) and no
    /// flags.
    | FChModAt of dirfd : int * path : PathArgumentBytes * mode : int * flags : int
    /// `mode` is raw, as `fchmod(2)` takes it.
    | FChMod of fd : int * mode : int
    /// `dirfd` and `flags` are raw, as `fchownat(2)` takes them. `None` is
    /// `(uid_t)-1` or `(gid_t)-1`: leave that ID as it is. `chown(2)` is this
    /// with the flavour's `AT_FDCWD` (`AtDirectory.atFdCwd`) and no flags, and
    /// `lchown(2)` with `AT_SYMLINK_NOFOLLOW` (0x100 on Linux, 0x20 on Darwin).
    | FChOwnAt of dirfd : int * path : PathArgumentBytes * user : UserId option * group : GroupId option * flags : int
    /// As `FChOwnAt`'s `chown(2)`, of the inode `fd` names.
    | FChOwn of fd : int * user : UserId option * group : GroupId option
    /// `mask` is raw, as `umask(2)` takes it: which of its bits the process
    /// keeps is behaviour this kernel models, and models per flavour. Answers
    /// the previous mask.
    | UMask of mask : int
    /// `dirfd`, `mode` and `flags` are raw, as `faccessat(2)` takes them: each
    /// flavour numbers `AT_FDCWD` and the flags its own way, and which bits of
    /// `mode` are rejected is behaviour this kernel models per flavour.
    /// `access(2)` is this with the flavour's `AT_FDCWD` (`AtDirectory.atFdCwd`)
    /// and no flags.
    | FAccessAt of dirfd : int * path : PathArgumentBytes * mode : int * flags : int
    /// `dirfd` is raw, as `symlinkat(2)` takes it. `symlink(2)` is this with
    /// the flavour's `AT_FDCWD` (`AtDirectory.atFdCwd`).
    | SymlinkAt of target : PathArgumentBytes * dirfd : int * path : PathArgumentBytes
    /// `olddirfd`, `newdirfd` and `flags` are raw, as `linkat(2)` takes them.
    /// `link(2)` is this with the flavour's `AT_FDCWD` on both sides and the
    /// flags `LinkRules.PlainLinkSource` stands for.
    | LinkAt of
        olddirfd : int *
        oldpath : PathArgumentBytes *
        newdirfd : int *
        newpath : PathArgumentBytes *
        flags : int
    /// `dirfd` and `flags` are raw, as `utimensat(2)` takes them: each flavour
    /// numbers `AT_FDCWD` and the flags its own way, and `times` holds the
    /// fields the caller stored, which each flavour reads its own way (see
    /// `UnixPathResolution.utimensat`). A null `path` means something of its
    /// own on Linux. `futimens(2)` is, on Linux, this with `fd` as `dirfd`, a
    /// null `path` and no flags.
    | UTimensAt of dirfd : int * path : NullablePathArgument * times : TimesArgument * flags : int
    /// `copy_file_range(2)` at both descriptions' own offsets. `length` is the
    /// `size_t` asked for and `flags` is raw, since any flag is refused.
    | CopyFileRange of inFd : int * outFd : int * length : uint64 * flags : int
    /// `ioctl(destination, FICLONE, source)`.
    | FileClone of destination : int * source : int
    /// `clonefile(2)`. The pathnames are their arguments' bytes, which this
    /// kernel copies in at the points it measured; `flags` is raw.
    | CloneFile of source : PathArgumentBytes * destination : PathArgumentBytes * flags : int
    /// `setresuid(2)`. `None` is `(uid_t)-1`: leave that ID as it is.
    | SetResUid of real : UserId option * effective : UserId option * saved : UserId option
    /// `setresgid(2)`. `None` is `(gid_t)-1`: leave that ID as it is.
    | SetResGid of real : GroupId option * effective : GroupId option * saved : GroupId option
    /// `setgroups(2)` of a list of `size` groups, whose words are as the caller
    /// read them.
    | SetGroups of size : int * words : GroupListWords

/// Why this kernel will not answer a syscall at all. The client decides what a
/// refusal means for it; nothing here is recoverable by retrying.
[<RequireQualifiedAccess>]
type SyscallRefusal<'Task> =
    | Dup of DescriptorLimitRefusal
    | Fcntl of FcntlRefusal
    | Dup2 of Dup2Refusal<'Task>
    | Dup3 of Dup3Refusal<'Task>
    | LSeek of LSeekRefusal
    | FLock of FLockRefusal
    | FTruncate of TruncationRefusal
    | MkDir of PathRefusal
    | UnlinkAt of UnlinkAtRefusal
    | ChDir of PathRefusal
    | FChModAt of FChModAtRefusal
    | FChMod of FChModRefusal
    | FChOwnAt of FChOwnAtRefusal
    | FChOwn of FChOwnRefusal
    | Access of AccessRefusal
    | Symlink of SymlinkRefusal
    | Link of LinkRefusal
    | Close of CloseRefusal<'Task>
    | UTimensAt of UTimensAtRefusal
    | CopyFileRange of CopyFileRangeRefusal
    | FileClone of FileCloneRefusal
    | CloneFile of CloneFileRefusal
    /// `setresuid(2)` and `setresgid(2)` alike.
    | SetIds of SetIdsRefusal
    | SetGroups of SetGroupsRefusal

[<RequireQualifiedAccess>]
module SyscallRefusal =
    /// What this kernel knows about why it cannot answer, after the name of
    /// the syscall it refused. The client supplies its own half: which task
    /// made the call.
    let describe<'Task> (refusal : SyscallRefusal<'Task>) : string =
        match refusal with
        | SyscallRefusal.Dup refusal -> $"dup: %s{DescriptorLimitRefusal.describe refusal}"
        | SyscallRefusal.Fcntl refusal -> $"fcntl: %s{FcntlRefusal.describe refusal}"
        | SyscallRefusal.Dup2 refusal -> $"dup2: %s{Dup2Refusal.describe refusal}"
        | SyscallRefusal.Dup3 refusal -> $"dup3: %s{Dup3Refusal.describe refusal}"
        | SyscallRefusal.LSeek refusal -> $"lseek: %s{LSeekRefusal.describe refusal}"
        | SyscallRefusal.FLock refusal -> $"flock: %s{FLockRefusal.describe refusal}"
        | SyscallRefusal.FTruncate refusal -> $"ftruncate: %s{TruncationRefusal.describe refusal}"
        | SyscallRefusal.MkDir refusal -> $"mkdirat: %s{PathRefusal.describe refusal}"
        | SyscallRefusal.UnlinkAt refusal -> $"unlinkat: %s{UnlinkAtRefusal.describe refusal}"
        | SyscallRefusal.ChDir refusal -> $"chdir: %s{PathRefusal.describe refusal}"
        | SyscallRefusal.FChModAt refusal -> $"fchmodat: %s{FChModAtRefusal.describe refusal}"
        | SyscallRefusal.FChMod refusal -> $"fchmod: %s{FChModRefusal.describe refusal}"
        | SyscallRefusal.FChOwnAt refusal -> $"fchownat: %s{FChOwnAtRefusal.describe refusal}"
        | SyscallRefusal.FChOwn refusal -> $"fchown: %s{FChOwnRefusal.describe refusal}"
        | SyscallRefusal.Access refusal -> $"faccessat: %s{AccessRefusal.describe refusal}"
        | SyscallRefusal.Symlink refusal -> $"symlinkat: %s{SymlinkRefusal.describe refusal}"
        | SyscallRefusal.Link refusal -> $"linkat: %s{LinkRefusal.describe refusal}"
        | SyscallRefusal.Close refusal -> $"close: %s{CloseRefusal.describe refusal}"
        | SyscallRefusal.UTimensAt refusal -> $"utimensat: %s{UTimensAtRefusal.describe refusal}"
        | SyscallRefusal.CopyFileRange refusal -> $"copy_file_range: %s{CopyFileRangeRefusal.describe refusal}"
        | SyscallRefusal.FileClone refusal -> $"ioctl(FICLONE): %s{FileCloneRefusal.describe refusal}"
        | SyscallRefusal.CloneFile refusal -> $"clonefile: %s{CloneFileRefusal.describe refusal}"
        | SyscallRefusal.SetIds refusal -> $"setresuid or setresgid: %s{SetIdsRefusal.describe refusal}"
        | SyscallRefusal.SetGroups refusal -> $"setgroups: %s{SetGroupsRefusal.describe refusal}"

/// A way this system's tables disagree with each other — a state no kernel
/// could be in, and which the operations here exist to keep unreachable.
/// `UnixSystem.checkInvariants` returns these.
///
/// Separate from `FileDescriptorRegistryDefect` and `VirtualFileSystemDefect`
/// because every case here is a claim about *two* tables at once, and neither
/// of those modules can see the other's: each is defined in a file that
/// compiles before this one.
///
/// Some cases concern the machine, and read facts that every process on it
/// contributes to: `UnixSystem.checkInvariants` reports those of a process
/// alone on its machine, and `SimulatedMachine.checkInvariants` those of a
/// machine holding several (as `SimulatedMachineDefect.Machine`). The rest
/// concern one process's view of the machine, and
/// `UnixSystem.checkViewInvariants` reports those.
[<RequireQualifiedAccess>]
type UnixSystemDefect<'Task> =
    /// A live open file description names a socket the socket table does not
    /// hold, so resolving that descriptor would fail.
    | DanglingSocket of description : OpenFileDescriptionId * socket : SocketId
    /// The socket table holds a socket no live description names.
    ///
    /// A leak, and deliberately a defect rather than a tolerated state: every
    /// way to make a socket — `UnixSocket.socket`, or `UnixConnection.accept`
    /// materialising a queued connection — hands back a descriptor at once,
    /// so an unreferenced socket means a close forgot to clean up. A
    /// connection awaiting accept is held on its listener's queue, not as a
    /// socket, which is what lets this rule stay strict.
    | UnreferencedSocket of socket : SocketId
    /// A socket in the table has an identity at or above the next one to
    /// allocate, so a future `socket(2)` would mint a duplicate.
    | NextSocketIdNotFresh of nextSocketId : SocketId * existing : SocketId
    /// `CurrentDirectoryInode` names something the filesystem does not hold, or
    /// holds as something other than a directory — so every relative path a
    /// process passes would resolve from a place that is not a directory.
    ///
    /// Deliberately *not* "the inode is reachable from the root": a real process
    /// keeps its current directory alive after the last name for it has gone,
    /// and the held inode is what expresses that.
    | CurrentDirectoryIsNotADirectory of inode : InodeNumber
    /// A live open file description names an inode the filesystem does not
    /// hold, so reading or `fstat`ing that descriptor would fail.
    ///
    /// The mirror image of `VirtualFileSystemDefect.UnreachableFromRoot`: that
    /// one catches an orphan nothing holds, and this one catches an inode freed
    /// while something still held it. Between them they bracket the reaping
    /// rule, so an inode freed too late is caught there and one freed too
    /// early is caught here.
    | DanglingOpenInode of description : OpenFileDescriptionId * inode : InodeNumber
    /// A description names an inode as the wrong kind of object: a
    /// `OpenFileTarget.File` onto a directory or a device, an
    /// `OpenFileTarget.Directory` onto anything but a directory, or an
    /// `OpenFileTarget.CharacterDevice` onto anything but the node of the device
    /// it names. `open` chooses the target by what it opened, so a directory's
    /// position is always a place in its entries and never a byte offset, and a
    /// device's operations are the driver's own.
    | DescriptionKindMismatch of description : OpenFileDescriptionId * inode : InodeNumber
    /// A socket's phase references a connection the connection table does not
    /// hold.
    | DanglingConnection of socket : SocketId * connection : ConnectionId
    /// A listener's accept queue references a connection the connection table
    /// does not hold.
    | DanglingQueuedConnection of listener : SocketId * connection : ConnectionId
    /// The connection table holds a connection no socket phase and no accept
    /// queue references — a leak `UnixDescriptor.close`'s sweep should have caught.
    | OrphanConnection of connection : ConnectionId
    /// One connection sits in two accept-queue slots (in one queue or two),
    /// so accepting it twice would materialise two sockets onto one
    /// connection.
    | DuplicateQueuedConnection of connection : ConnectionId
    /// More than one socket holds one end of a connection: two established as
    /// its client, or as its server, or an accepted server end while a
    /// listener still queues the connection (where its server end is before
    /// `accept(2)`). `holders` names each, in socket-table order, and a
    /// listener once however often it queues the connection.
    | ConnectionEndHeldTwice of connection : ConnectionId * connectionEnd : ConnectionEnd * holders : SocketId list
    /// A connection's bytes and end states break the rules its transfer keeps,
    /// each stated in `violations`: a buffer
    /// holding more than its capacity, bytes in flight to an end that was
    /// reset or closed, a FIN arrived ahead of bytes sent before it, an end
    /// told of an ending its peer never made, and so on.
    | TcpTransferBroken of connection : ConnectionId * violations : string list
    /// A connection's transfer rules are of `rules`, but the machine is
    /// `flavour`-flavoured.
    | TcpTransferNotOfFlavour of
        connection : ConnectionId *
        rules : SimulatedUnixFlavour *
        flavour : SimulatedUnixFlavour
    /// The socket `socket` is the `connectionEnd` end of `connection`, or is
    /// the listener whose accept queue holds its server end, which the
    /// connection records as closed.
    | ConnectionEndClosedUnderSocket of connection : ConnectionId * connectionEnd : ConnectionEnd * socket : SocketId
    /// The connection records its `connectionEnd` end as open, but no socket
    /// is that end and, for a server end, no listener queues the connection:
    /// its socket closed without the connection being told.
    | ConnectionEndOpenWithoutHolder of connection : ConnectionId * connectionEnd : ConnectionEnd
    /// A socket's phase is one its kind cannot enter: a datagram socket
    /// listening or holding a stream connection, or a non-datagram socket
    /// holding a datagram peer.
    | SocketPhaseKindMismatch of socket : SocketId * kind : SocketKind * phase : SocketPhase
    /// A connection in the table has an identity at or above the next one to
    /// allocate, so a future connect would mint a duplicate.
    | NextConnectionIdNotFresh of nextConnectionId : ConnectionId * existing : ConnectionId
    /// An event registration, an epoll instance's or a kqueue's, records
    /// an ADD ordinal at or above the
    /// next one to mint, so some future ADD would repeat it — and the
    /// ordinal's whole job is to order same-signal ties, which a repeat
    /// leaves unspecified.
    | EventRegistrationOrdinalNotFresh of next : int64 * queue : OpenFileDescriptionId * registeredAt : int64
    /// Two event registrations record the same ADD ordinal. Ordinals
    /// are minted from one monotonic counter, so a duplicate means two ADDs
    /// were stamped with one mint — and a same-signal tie between the pair
    /// would have no measured order.
    | DuplicateEventRegistrationOrdinal of registeredAt : int64
    /// A task is parked on an open file description the table does not hold,
    /// so its wait can never be satisfied, and asking `UnixWait.wakes` about
    /// it crashes. A park holds what it names until the call returns, so
    /// this is a park recorded without the description, or one destroyed
    /// without the park being consulted.
    | ParkedOnAbsentDescription of task : 'Task * description : OpenFileDescriptionId
    /// An open file description survives that no descriptor names and no
    /// syscall in flight holds (`ParkedSyscall.descriptions`). A real kernel
    /// frees a file when its last reference goes, so this is a leak: a close,
    /// or the return of a call that held it, failed to release it.
    | UnreferencedDescription of description : OpenFileDescriptionId
    /// A description records `recorded` holds by syscalls in flight, where the
    /// parks of every process's tasks name it `parks` times
    /// (`ParkedSyscall.descriptions`). The holds are what keep a description
    /// alive once no descriptor names it, so one too few lets a close destroy
    /// what a sleeping call still waits on, and one too many leaks it.
    | HoldCountMismatch of description : OpenFileDescriptionId * recorded : int * parks : int
    /// No descriptor names the open file description of the connected stream
    /// socket `socket`, whose `SO_LINGER` is on, and only syscalls in flight
    /// hold it. The socket is closed when the last of those calls returns, and
    /// that close is one this kernel may refuse (`DescriptionReleaseRefusal.AbortiveClose`,
    /// `DescriptionReleaseRefusal.LingeringClose`), which a call's return has
    /// no way to answer. `close` refuses the close that would leave the socket
    /// so (`CloseRefusal.LingeringCloseDeferredToCall`).
    | LingeringSocketHeldOnlyByCalls of description : OpenFileDescriptionId * socket : SocketId
    /// A task is parked in an `epoll_wait` on a description that is not an
    /// epoll instance, which no wait could have produced.
    | ParkedEpollWaitOnNonEpoll of task : 'Task * description : OpenFileDescriptionId * target : OpenFileTarget
    /// A task is parked in a `kevent` on a description that is not a kqueue,
    /// which no wait could have produced and on which `UnixWait.wakes`
    /// crashes.
    | ParkedKeventOnNonKqueue of task : 'Task * description : OpenFileDescriptionId * target : OpenFileTarget
    /// A task is parked in a `kevent` on a kqueue that has not been drained,
    /// and the descriptor the call was made through no longer names that
    /// kqueue (`current` is what it names now). Closing that descriptor drains
    /// the kqueue, so this is a park recorded without `kevent` or a descriptor
    /// closed around it.
    | ParkedKeventDescriptorRebound of
        task : 'Task *
        fd : int *
        kqueue : OpenFileDescriptionId *
        current : OpenFileDescriptionId option
    /// A task is parked in a `kevent` for `maxEvents` events, which is not
    /// positive: such a call returns at once.
    | ParkedKeventCountNotPositive of task : 'Task * maxEvents : int
    /// The process's descriptor `fd` names, or a `kevent` wait of one of its
    /// tasks made through `fd` sleeps on, the kqueue `kqueue`, which the
    /// process `owner` owns (`KqueueState.Owner`). A kqueue's registrations
    /// name descriptors in its owner's table, so this process's view would
    /// read them against the wrong one; and only the process that created a
    /// kqueue has a descriptor onto it.
    | KqueueOfAnotherProcess of fd : int * kqueue : OpenFileDescriptionId * owner : ProcessId
    /// An open file description names an object this flavour's kernel does
    /// not have: an epoll instance or a device under Darwin, or a kqueue under
    /// Linux.
    | DescriptionNotOfFlavour of
        description : OpenFileDescriptionId *
        target : OpenFileTarget *
        flavour : SimulatedUnixFlavour
    /// A descriptor is open at or above the bound this kernel assumes the
    /// process's `RLIMIT_NOFILE` reaches (`SimulatedUnixPlatform.descriptorBound`),
    /// which no call hands out.
    | DescriptorAtOrAboveBound of fd : int * bound : int
    /// A descriptor carries `FD_CLOFORK` under the Linux flavour, which has no
    /// such flag.
    | CloseOnForkUnderLinux of fd : int
    /// A description carries one of `O_SYNC` and `O_DSYNC` without the other
    /// under the Linux flavour, whose `open(2)` sets both together and whose
    /// `F_SETFL` changes neither.
    | UnpairedSynchronisationUnderLinux of description : OpenFileDescriptionId
    /// A description's status holds a fact the flavour's `F_GETFL` does not
    /// report, which `OpenFileStatus` keeps only under the other flavour.
    | StatusNotOfFlavour of
        description : OpenFileDescriptionId *
        status : OpenFileStatus *
        flavour : SimulatedUnixFlavour
    /// Under the Darwin flavour, a description holds an `flock` lock and its
    /// status does not record that one was granted (`OpenFileStatus.Flocked`).
    | FlockHeldNotRecorded of description : OpenFileDescriptionId
    /// A task is parked in a `poll` watching an event queue, which `poll`
    /// refuses before it parks and whose readiness is not modelled.
    | ParkedPollOnEventQueue of task : 'Task * description : OpenFileDescriptionId
    /// A task's parked `poll` watches `fd` on the description it named when
    /// the call went to sleep, and `fd` now names `current` instead. `close`
    /// refuses to close a descriptor a parked poll watches, so this is a park
    /// recorded without `poll` or a descriptor closed around it.
    | ParkedPollDescriptorRebound of
        task : 'Task *
        fd : int *
        watched : OpenFileDescriptionId *
        current : OpenFileDescriptionId option
    /// A task's parked Darwin `poll` registers a filter through `fd`, which is
    /// not open (`target` is `None`) or names something other than a socket
    /// whose filters are modelled or a pipe. Closing a descriptor removes its
    /// registrations, a poll registers on nothing else, and a regular file's
    /// filters are always ready, so they report before the call can sleep.
    | ParkedKqueuePollRegistrationTarget of task : 'Task * fd : int * target : OpenFileTarget option
    /// A task's parked Darwin `poll` attributes the registration `key` to
    /// entry `entry`, which the call does not have.
    | ParkedKqueuePollEntryOutOfRange of task : 'Task * key : (int * KqueueFilter) * entry : int
    /// A task's parked Darwin `poll` lists `key` as activated where the list
    /// may not hold it: `key` is not registered, is listed twice, or is not a
    /// socket's filter (see `PollQueue.Active`).
    | ParkedKqueuePollActiveMalformed of task : 'Task * key : (int * KqueueFilter)
    /// A task's parked Darwin `poll` registers the socket filter `key`, which is
    /// ready, and does not list it as activated: whatever made it ready did not
    /// activate it, so the call would sleep through what a real one wakes for.
    | ParkedKqueuePollActivationMissed of task : 'Task * key : (int * KqueueFilter)
    /// A task's parked Darwin `poll` registers the filter `key` as attached to
    /// the socket `attached` (`None`: to none), through a descriptor naming the
    /// socket `named` (`None`: something other than a socket). A poll attaches
    /// a socket's filter, and only a socket's, to the socket its descriptor
    /// names, and closing the descriptor removes the filter, so the two agree
    /// for as long as it lasts.
    | ParkedKqueuePollAttachedElsewhere of
        task : 'Task *
        key : (int * KqueueFilter) *
        attached : SocketId option *
        named : SocketId option
    /// A task is parked in a Darwin `poll` whose kqueue (`ParkedKqueuePoll.Queue`)
    /// is not on the machine. The park holds it until the park ends.
    | ParkedOnAbsentPollQueue of task : 'Task * queue : PollQueueId
    /// A task is parked in a Darwin `poll` whose kqueue another process,
    /// `owner`, made. A poll's kqueue is its own call's.
    | PollQueueOfAnotherProcess of task : 'Task * queue : PollQueueId * owner : ProcessId
    /// The machine holds a sleeping Darwin poll's kqueue that `parks` parks
    /// name, rather than exactly one: it is made as its call sleeps, and
    /// destroyed as the park naming it ends.
    | PollQueueNotHeldOnce of queue : PollQueueId * parks : int
    /// The machine holds a sleeping Darwin poll's kqueue whose identity is not
    /// below the counter it is minted from.
    | PollQueueIdNotFresh of next : PollQueueId * existing : PollQueueId
    /// A task is parked in an `accept` on a description that is not a listening
    /// socket, which no accept could have produced and on which
    /// `UnixWait.wakes` crashes.
    | ParkedAcceptOnNonListener of task : 'Task * description : OpenFileDescriptionId
    /// A task is asleep in an `accept` on a listener a close has drained
    /// (`ListenState.Drained`): the close that drains a listener ends every
    /// accept asleep on it, and `accept` refuses to sleep on one.
    | ParkedAcceptOnDrainedListener of task : 'Task * description : OpenFileDescriptionId
    /// Under Darwin, a task is asleep in an `accept`, or a pipe or connection
    /// transfer, made through `fd`, which no longer names the description the
    /// call sleeps on
    /// (`current` is what it names now). Closing that descriptor ends the call,
    /// so this is a park recorded without the syscall or a descriptor closed
    /// around it.
    | ParkedCallDescriptorRebound of
        task : 'Task *
        fd : int *
        description : OpenFileDescriptionId *
        current : OpenFileDescriptionId option
    /// Under Linux, a task's `accept`, or pipe or connection transfer, records
    /// that a close has ended it (`SleepTarget.EndedByClose`), which only
    /// Darwin's close does.
    | ParkedCallEndedByCloseUnderLinux of task : 'Task
    /// Under Linux, a listener records that a close has drained it
    /// (`ListenState.Drained`), which only Darwin's close does.
    | ListenerDrainedUnderLinux of socket : SocketId
    /// A task is asleep in a pipe `read` or `write` through a description that
    /// names something other than the pipe end the call needs, which no such
    /// call could have produced and on which `UnixWait.wakes` crashes.
    | ParkedPipeTransferOnWrongTarget of task : 'Task * description : OpenFileDescriptionId * target : OpenFileTarget
    /// A task is asleep in a pipe transfer whose progress no call could have
    /// made: a read of nothing, or a write of `count` bytes with `written` of
    /// them in, where a sleeping write has put in at least none and fewer than
    /// all.
    | ParkedPipeTransferProgress of task : 'Task * count : int * written : int
    /// A task is asleep in a `read`, `recv`, `write` or `send` of a connected
    /// socket through a description that names something other than an end
    /// of a connection,
    /// which no such call could have produced and on which
    /// `UnixWait.wakes` crashes.
    | ParkedConnectionTransferOnNonConnection of task : 'Task * description : OpenFileDescriptionId
    /// A task is asleep in a connection transfer whose progress no call could
    /// have made: a read of nothing but a Linux `recv`, or a write of `count`
    /// bytes with `written` of them taken, where a sleeping write has taken at
    /// least none and fewer than all.
    | ParkedConnectionTransferProgress of task : 'Task * count : int * written : int
    /// A task's park records an ordinal at or above the next one to mint, so
    /// some future park would repeat it, and the two waiters' order would be
    /// unspecified.
    | ParkOrdinalNotFresh of next : ParkOrdinal * task : 'Task * ordinal : ParkOrdinal
    /// Two tasks' parks record the same ordinal. Ordinals are minted from one
    /// monotonic counter, so a duplicate means two parks were stamped with one
    /// mint.
    | DuplicateParkOrdinal of ordinal : ParkOrdinal
    /// A listening socket has no address: `listen(2)` binds an unbound socket
    /// before it listens, and a connect looks listeners up by their binding,
    /// so this one can never be reached.
    | ListenerWithoutBinding of socket : SocketId
    /// A socket whose `SocketAddressing` can hold no address -- an IPv6 one
    /// with `IPV6_V6ONLY` on, or a Unix-domain one -- is listening, connected
    /// or latched by a refusal, each of which only a socket with an address
    /// reaches.
    | UnaddressableSocketInPhase of socket : SocketId * addressing : SocketAddressing * phase : SocketPhase
    /// A bound socket holds port 0, which is how a process *asks* for a port and
    /// never one it is given, with one exception: a datagram socket whose
    /// Linux `connect(AF_UNSPEC)` kept a locked concrete address and dropped
    /// an unlocked port is half-bound at `address:0`, which is measured and
    /// is what a later `bind` or `connect` completes.
    | BoundToPortZero of socket : SocketId
    /// A task the table does not hold has handler frames.
    | HandlerFramesWithoutTask of task : 'Task
    /// A task the table does not hold has a signal mask.
    | MaskWithoutTask of task : 'Task
    /// A task the table does not hold has a mask for a `sigsuspend(2)` to
    /// restore.
    | MaskToRestoreWithoutTask of task : 'Task
    /// A task asleep in a syscall other than `sigsuspend(2)` has a mask for a
    /// `sigsuspend` to restore. A task has one only while it is in that call or
    /// returning from it, and a task returns to user mode before it makes
    /// another call.
    | MaskToRestoreOutsideSigsuspend of task : 'Task * parked : ParkedSyscall
    /// A task asleep in `sigsuspend(2)` has no mask to restore, so its return
    /// would leave it with the call's temporary mask.
    | SigsuspendWithoutMaskToRestore of task : 'Task
    /// A pending signal is directed at a task the table does not hold, so it
    /// can never be delivered and sits in the queue for the rest of the run.
    | PendingSignalTargetWithoutTask of task : 'Task * signal : Signal
    /// The process's signal state reads signals under a numbering other than
    /// its machine's platform assigns, so a signal in the signal tables may
    /// have a different number, or none, under the kernel's numbering.
    /// `initial` derives the one from the other, so this is a state assembled
    /// some other way.
    | SignalNumberingMismatch of signals : SignalNumbering * platform : SignalNumbering
    /// The machine's mount claims a filesystem type its flavour cannot report,
    /// so `fstatfs` on a file would answer a fact no such machine could tell.
    | FileSystemTypeNotReportable of flavour : SimulatedUnixFlavour * fileSystemType : EmulatedFileSystemType
    /// The machine's buffer check is not one its platform can have: an up-front
    /// screen on a platform that has none or the other way about, or a limit no
    /// machine of the platform's architecture has been observed to have. See
    /// `UnixMachineState.isUserBufferCheckOf`.
    | UserBufferCheckNotOfPlatform of platform : SimulatedUnixPlatform * check : UserBufferCheck
    /// The process holds more supplementary groups than its machine's
    /// platform lets any process hold (`SimulatedUnixPlatform.supplementaryGroupLimit`).
    | TooManySupplementaryGroups of count : int * limit : int
    /// The process's file-mode creation mask holds a bit its machine's
    /// platform's `umask(2)` never stores (`SimulatedUnixPlatform.umaskStoredBits`),
    /// so every creation would clear a bit no kernel of that flavour clears.
    | UmaskNotOfPlatform of umask : PermissionBits * platform : SimulatedUnixPlatform
    /// The process's leader is not one of its tasks. A running process always has
    /// its leader, since the leader cannot exit while another task lives.
    | LeaderWithoutTask of leader : 'Task
    /// Two live tasks report the same thread ID, so a lock that records its owner
    /// by thread ID would take either for the other.
    | DuplicateOsThreadId of id : OsThreadId * tasks : 'Task list
    /// On Linux, the leader's thread ID is not the process ID, which it always is.
    | LeaderThreadIdNotProcessId of leader : 'Task * id : OsThreadId * pid : ProcessId
    /// A task's thread ID is one the machine's counter could not have handed out:
    /// at or above the greatest `pid_max` Linux takes
    /// (`ThreadIdAllocator.linuxPidMaxCeiling`), or not yet reached on Darwin,
    /// where a later thread could be given the same ID.
    | OsThreadIdNotMintable of task : 'Task * id : OsThreadId * allocator : ThreadIdAllocator
    /// The machine's thread ID counter is not its flavour's: a Linux counter on a
    /// Darwin machine, or the other way about.
    | ThreadIdAllocatorNotOfFlavour of flavour : SimulatedUnixFlavour * allocator : ThreadIdAllocator
    /// The machine's thread ID allocator records as live the IDs in
    /// `withoutTask`, which no task holds, and does not record those in
    /// `notRecorded`, which tasks hold. The allocator hands out only IDs it
    /// does not record as live, so one it fails to record could be handed to a
    /// second task, and one it records for no task is never handed out again.
    | LiveThreadIdsMismatch of withoutTask : Set<OsThreadId> * notRecorded : Set<OsThreadId>
    /// The machine records as live the process IDs in `withoutProcess`, which
    /// no process on it has, and does not record those in `notRecorded`, which
    /// processes on it have. A new process is never given an ID the machine
    /// records, so one it fails to record could be given to a second process.
    | LiveProcessIdsMismatch of withoutProcess : Set<ProcessId> * notRecorded : Set<ProcessId>
    /// The machine's process ID counter is not its flavour's: Darwin's own
    /// counter on a Linux machine, which takes process IDs from its thread IDs,
    /// or the other way about.
    | ProcessIdCounterNotOfFlavour of flavour : SimulatedUnixFlavour * counter : ProcessIdCounter
    /// On Darwin, a live process's ID is at or above the counter's next, so a
    /// later process could be given it.
    | ProcessIdNotBelowCounter of pid : ProcessId * next : int32
    /// The machine records `recorded` processes standing in the directory
    /// `inode`, where `standing`
    /// processes on it have it as their current directory. One too few lets a
    /// removal by another process free a directory a process stands in.
    | CurrentDirectoryHoldMismatch of inode : InodeNumber * recorded : int * standing : int
    /// A kqueue is owned (`KqueueState.Owner`) by `owner`, which is no live
    /// process on the machine. Its registrations name descriptors in that
    /// process's table, which no longer exists.
    | KqueueOwnerNotLive of kqueue : OpenFileDescriptionId * owner : ProcessId
    /// A live open file description names a pipe the pipe table does not hold.
    | DanglingPipe of description : OpenFileDescriptionId * pipe : PipeId
    /// The pipe table holds a pipe neither of whose ends is open: no live
    /// description names either end, and the client holds neither. A close that
    /// should have freed it did not.
    | UnreferencedPipe of pipe : PipeId
    /// A pipe the client drains holds bytes. The client reads every byte the
    /// moment it is written, so a write into such a pipe leaves it empty.
    | DrainedPipeHoldsBytes of pipe : PipeId * held : int
    /// The client is asleep in a write into a pipe that has room for some of
    /// what it has left. It writes whenever a read makes room, so a pipe it
    /// supplies is always as full as it can make it.
    | SuppliedPipeHasRoom of pipe : PipeId
    /// A pipe in the table has an identity at or above the next one to
    /// allocate, so a future `pipe2` would mint a duplicate.
    | NextPipeIdNotFresh of nextPipeId : PipeId * existing : PipeId
    /// A pipe end reports an inode number at or above the next one to mint, so
    /// a future pipe would report it too.
    | PipeInodeNotFresh of next : InodeNumber * pipe : PipeId * inode : InodeNumber
    /// Two pipe ends that are not two ends of one Linux pipe report the same
    /// inode number, so a process comparing them would take them for one pipe.
    | DuplicatePipeInode of inode : InodeNumber
    /// A pipe holds a buffer, or reports inode numbers, of a shape its
    /// machine's flavour does not give a pipe: see `PipeBuffer.isOf` and
    /// `PipeInodes`.
    | PipeNotOfPlatform of pipe : PipeId * platform : SimulatedUnixPlatform
    /// The machine's pipes report a device no machine of its flavour reports
    /// for them: a negative one, or on Darwin anything but 0.
    | PipeDeviceNotOfFlavour of device : int64 * flavour : SimulatedUnixFlavour
    /// The machine sets an `fs.protected_*` sysctl its flavour does not have:
    /// anything but `ProtectedFiles.off` on Darwin.
    | ProtectedFilesNotOfFlavour of protection : ProtectedFiles * flavour : SimulatedUnixFlavour
    /// The symbolic link at `inode` has permission bits no link of the
    /// platform's flavour can have: anything but 0o777 on Linux, which creates
    /// every link with those bits and never changes them. A Darwin link can
    /// have any bits, since `fchmodat(2)` changes them
    /// (`SimulatedUnixPlatform.symlinkModeChange`).
    | SymlinkPermissionsNotOfFlavour of
        inode : InodeNumber *
        permissions : PermissionBits *
        flavour : SimulatedUnixFlavour
    /// The task `task` is on the logical processor `cpu`, which is not one of
    /// the machine's `processorCount`, numbered from 0. See
    /// `UnixTaskState.cpu`.
    | CpuBeyondMachine of task : 'Task * cpu : CpuId * processorCount : int
    /// The machine records the task with thread ID `occupant` as running on
    /// the logical processor `cpu`, which is not one of its `processorCount`,
    /// numbered from 0. See `UnixScheduling`.
    | OccupiedCpuBeyondMachine of cpu : CpuId * occupant : OsThreadId * processorCount : int
    /// The machine records the task with thread ID `occupant` as running on
    /// the logical processor `cpu`, and no live task has that thread ID: a
    /// task left the processor without the machine recording it.
    | OccupantWithoutTask of cpu : CpuId * occupant : OsThreadId
    /// The machine records `task` as running on the logical processor
    /// `occupied`, and the task's own processor is `cpu`: a running task's
    /// processor is the one it runs on (`UnixTaskState.cpu`).
    | OccupantOnAnotherCpu of task : 'Task * occupied : CpuId * cpu : CpuId

/// Why the directory a host named cannot be the one a simulated process starts
/// in (`ProcessLaunch.withCurrentDirectory`). Launching the process returns one
/// instead of deciding what to say about it: the remedy is always "fix the
/// knob you set this from", and only the caller knows what that knob is called.
///
/// Every case is a host mistake rather than a process's, which is why none of
/// them is a `UnixError`: there is no errno for "you launched a process in a
/// directory the filesystem does not contain", and answering ENOENT would blame
/// a process's path when there is no process yet.
///
/// Four cases, and deliberately not six. The walk could also answer an inode
/// the filesystem does not contain, or a directory no path from the root
/// reaches — but not for a filesystem whose invariants hold, walked from its
/// root, so those are bugs in this library and crash rather than being handed
/// to a caller who could do nothing about them.
[<RequireQualifiedAccess>]
type CurrentDirectoryFault =
    /// The path does not resolve in the seeded filesystem at all. Carries what
    /// the walk answered.
    | DoesNotResolve of UnixError
    /// The walk refused the path as too long. Distinguished from
    /// `DoesNotResolve` because the directory may well be present: it is a
    /// *length* that is unusable, and the remedy is to shorten something rather
    /// than to go looking for a missing directory.
    ///
    /// Which length is deliberately not said, because the walk does not say:
    /// `ENAMETOOLONG` is one errno covering a component past this flavour's
    /// `NAME_MAX` and — on a flavour that re-checks, which is Darwin — a
    /// symbolic link whose expansion would carry the whole path past
    /// `PATH_MAX`. A real kernel conflates them too. Splitting the case would
    /// need the path walk to report which limit it hit, which every other
    /// caller of the walk would pay for.
    ///
    /// Carries the flavour so that a fault which outlives the call still says
    /// whose limits were in force -- 255 CJK characters name a directory a
    /// Darwin process can start in and a Linux one cannot, and only Darwin
    /// re-checks a splice at all.
    | TooLong of SimulatedUnixFlavour
    /// The path resolves, to something that is not a directory.
    | NotADirectory
    /// This kernel will not resolve the path.
    | Path of PathRefusal

/// Why `UnixBootImage.withFileSystem` refuses a seed: it describes a filesystem
/// no kernel of the machine's flavour could have mounted.
[<RequireQualifiedAccess>]
type FileSystemSeedFault =
    /// The seed holds a directory entry whose name is past this flavour's
    /// `NAME_MAX`, so it describes a filesystem no kernel of that flavour could
    /// have mounted: `stat` of the name would answer ENAMETOOLONG while
    /// `readdir` listed it.
    | SeedNameTooLong of name : DirectoryEntryName * flavour : SimulatedUnixFlavour
    /// The seed holds a directory entry whose name this flavour's filesystem
    /// will not bind (see `SimulatedUnixPlatform.bindableEntryNames`): on
    /// Darwin, a name that is not valid UTF-8. No kernel of that flavour could
    /// have created it, and a process there could not create it either.
    | SeedNameNotBindable of name : DirectoryEntryName * flavour : SimulatedUnixFlavour
    /// The seed binds `dev` at its root to something other than an empty
    /// directory. The kernel mounts its device filesystem there at boot, and a
    /// mount over a populated directory would hide what it holds.
    | SeedCoversDeviceFileSystem of name : DirectoryEntryName

[<RequireQualifiedAccess>]
module FileSystemSeedFault =
    /// What this library knows about why it refused the seed, for a client
    /// composing a diagnostic that names its own knob.
    let describe (fault : FileSystemSeedFault) : string =
        match fault with
        | FileSystemSeedFault.SeedNameTooLong (name, flavour) ->
            $"the seed holds a directory entry named %s{DirectoryEntryName.toEscaped name}, which is past the %O{flavour} flavour's NAME_MAX, so no kernel of that flavour could have mounted the filesystem it describes."
        | FileSystemSeedFault.SeedNameNotBindable (name, flavour) ->
            $"the seed holds a directory entry named %s{DirectoryEntryName.toEscaped name}, which a %O{flavour} filesystem will not bind, so no kernel of that flavour could have created it."
        | FileSystemSeedFault.SeedCoversDeviceFileSystem name ->
            $"the seed binds %s{DirectoryEntryName.toEscaped name} at its root to something other than an empty directory, and the kernel mounts its device filesystem there at boot, which would hide what the seed put there."

[<RequireQualifiedAccess>]
module UnixSystem =

    /// Mount `mount` over the root's `dev`, as the kernel does at boot, with a
    /// node for each device it has a driver for, all made at `bootTime` and
    /// owned by root.
    ///
    /// The directory and its nodes have the modes a measured devtmpfs and devfs
    /// report: 0755 and 0555 respectively for the directory, 0666 for each
    /// node.
    let internal mountDeviceFileSystem
        (mount : DeviceFileSystemMount)
        (bootTime : UnixTimestamp)
        (filesystem : VirtualFileSystem)
        : Result<VirtualFileSystem, MountFault>
        =
        let root =
            {
                User = UserId.root
                Group = GroupId.parseOrFail "UnixSystem.mountDeviceFileSystem" 0u
            }

        let permissions, devices =
            match mount with
            | DeviceFileSystemMount.Devtmpfs _ ->
                PermissionBits 0o755,
                CharacterDevice.all
                |> List.map (fun device -> CharacterDevice.name device, device, CharacterDevice.permissions device)
            | DeviceFileSystemMount.Devfs -> PermissionBits 0o555, []

        VirtualFileSystem.mountAtRoot
            (DeviceFileSystemMount.mounted mount)
            (DirectoryEntryName.parseOrFail "UnixSystem.mountDeviceFileSystem" "dev")
            permissions
            root
            devices
            bootTime
            filesystem

    /// The process's first task, which it started with: its thread-group
    /// leader.
    let leader<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : 'Task
        =
        system.Leader

    /// Every task the process has, by the client's name for it, which
    /// `UnixTaskState`'s readers read. A name the process has no task by,
    /// because the task was never created or has exited, has no entry.
    let tasks<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Map<'Task, UnixTaskState>
        =
        system.Tasks

    /// Every write that has reached a client draining one of the machine's
    /// pipes, oldest first: the bytes the outside world has received from the
    /// machine's processes. No process can read them back.
    ///
    /// A client reads what its own ends received by filtering on
    /// `Delivery.Endpoint`, which names the process the pipe was launched into
    /// as well as the descriptor. The log is one for the whole machine, so the
    /// order of writes across endpoints is kept. It grows without bound.
    let delivered<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : DeliveryLog
        =
        UnixMachineState.delivered system.Machine

    /// The process's signal state, which `SignalState`'s queries read.
    let signals<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        UnixProcessState.signals system.Process

    /// The environment the process was started with: the `envp` that
    /// `execve(2)` received, entry by entry, in order. Each entry is held
    /// exactly as given, whether or not it is `NAME=VALUE`.
    ///
    /// This is the exec-time image, not the C library's `environ`:
    /// `setenv(3)` and `putenv(3)` change the process's own copy in user
    /// space, which this library does not model.
    let environment<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixByteString list
        =
        UnixProcessState.environment system.Process

    /// The path of the executable that started the process, or `None` if it has
    /// none. `None` is an answer rather than a missing value: this library
    /// models no `exec(2)`, so a process has a path only if its launch set one
    /// (`ProcessLaunch.withProcessPath`). The path is not resolved against the
    /// filesystem, so a client that wants it to name a file seeds the file.
    let processPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : AbsoluteUnixPath option
        =
        UnixProcessState.processPath system.Process

    /// The platform the machine impersonates.
    let platform<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SimulatedUnixPlatform
        =
        UnixMachineState.platform system.Machine

    /// The number of logical processors the machine reports to the process:
    /// at least 1, and fixed at boot (`UnixBootImage.withProcessorCount`).
    let processorCount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : int
        =
        UnixMachineState.processorCount system.Machine

    /// How long the machine has been up, to the nanosecond. Only `advanceClock`
    /// moves it. A process reads the same instant through
    /// `UnixClock.clockGettime`, at the granularity its flavour reports.
    let nanosecondsSinceBoot<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : int64
        =
        UnixMachineState.nanosecondsSinceBoot system.Machine

    /// Let `nanoseconds` pass on the machine: every clock it has moves forward
    /// by that much. Advancing by zero changes nothing.
    ///
    /// Throws for a negative amount, since no clock this kernel models runs
    /// backwards, and for one that would take the uptime past
    /// `Int64.MaxValue` nanoseconds (about 292 years).
    let advanceClock<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (nanoseconds : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine = UnixMachineState.advanceClock nanoseconds system.Machine
        }

    /// The process's descriptor table, which `FileDescriptorRegistry`'s queries
    /// read: every descriptor it holds, and the open file description each
    /// names.
    let fileDescriptors<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : FileDescriptorRegistry
        =
        UnixSystemState.fileDescriptors system

    /// The machine's open file descriptions, which `OpenFileTable`'s queries
    /// read: every one any descriptor names, and any a call in flight holds.
    let openFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : OpenFileTable
        =
        system.Machine.OpenFiles

    /// The machine's filesystem, which `VirtualFileSystem`'s queries read.
    let fileSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : VirtualFileSystem
        =
        system.Machine.FileSystem

    /// The inode of the directory the process is standing in. The path
    /// `getcwd(3)` reports is `UnixPathResolution.currentDirectoryPath`.
    let currentDirectoryInode<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : InodeNumber
        =
        system.Process.CurrentDirectoryInode

    /// Who the process is: all six of its IDs, and its supplementary groups.
    let credentials<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Credentials
        =
        system.Process.Credentials

    /// The process's file-mode creation mask, without replacing it as `umask`
    /// does.
    let fileModeCreationMask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : PermissionBits
        =
        system.Process.Umask

    /// The machine's entropy pool, from which every random-bytes syscall draws.
    let entropyPool<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : EntropyPool
        =
        system.Machine.EntropyPool

    /// The range a `bind(2)` of port 0 draws from, inclusive at both ends.
    let ephemeralPortRange<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : uint16 * uint16
        =
        system.Machine.EphemeralPortRange

    /// The machine's `somaxconn` sysctl (`net.core.somaxconn` on Linux,
    /// `kern.ipc.somaxconn` on Darwin): the ceiling `listen(2)` clamps its
    /// backlog to. See `UnixBootImage.withSoMaxConn`.
    let soMaxConn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : int
        =
        system.Machine.SoMaxConn

    /// The mount the machine's root filesystem claims to be: its type and what
    /// `statfs(2)` reports about it. Fixed for the run, since this library
    /// models no `mount(2)`; see `UnixBootImage.withMount`.
    let mount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : EmulatedMount
        =
        system.Machine.Mount

    /// The machine's `fs.protected_*` sysctls. See
    /// `UnixBootImage.withProtectedFiles`.
    let protectedFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : ProtectedFiles
        =
        system.Machine.ProtectedFiles

    /// Every pipe with an end open, by identity: an end an open file
    /// description names, or one the client holds (`PipeState.heldByClient`).
    let pipes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Map<PipeId, PipeState>
        =
        system.Machine.Pipes

    /// The realtime clock's reading, to the nanosecond: the boot time plus
    /// `nanosecondsSinceBoot`. This is the instant the kernel stamps on an
    /// inode it changes. `clock_gettime(CLOCK_REALTIME)` reports the same
    /// clock, at the granularity its flavour reports it at.
    let realtime<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixTimestamp
        =
        UnixMachineState.realtime system.Machine

    /// Whether, and where, the machine's kernel screens a read or write buffer
    /// before performing the operation. See `UnixBootImage.withUserAddressLimit`.
    let userBufferCheck<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UserBufferCheck
        =
        UnixMachineState.userBufferCheck system.Machine

    /// The socket `socketId` names, or `None` if the machine holds none by
    /// that identity: a socket goes when its last open file description does,
    /// so an identity read from a descriptor that has since closed names
    /// nothing.
    let socket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : SocketDescription option
        =
        Map.tryFind socketId system.Machine.Sockets

    /// The kqueue of the sleeping Darwin `poll` whose park names `queue`
    /// (`ParkedKqueuePoll.Queue`), or `None` if the machine holds none by that
    /// identity.
    let pollQueue<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (queue : PollQueueId)
        (system : UnixSystem<'Task, 'Handler>)
        : PollQueue option
        =
        Map.tryFind queue system.Machine.PollQueues

    /// The pipe `pipeId` names. Loudly partial, as `UnixMachineState.pipe` is.
    let internal pipe<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (system : UnixSystem<'Task, 'Handler>)
        : PipeState
        =
        UnixMachineState.pipe pipeId system.Machine

    /// How ready the socket `socketId` is. See
    /// `UnixMachineState.socketReadinessLevel`.
    let internal socketReadinessLevel<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : ReadinessLevel
        =
        UnixMachineState.socketReadinessLevel socketId system.Machine

    /// What the descriptor `fd` names, or `None` if the process has no such
    /// descriptor open.
    let descriptorTarget<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : OpenFileTarget option
        =
        FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system)

    /// The process ID, as `getpid(2)` reports it.
    ///
    /// Total, and changes nothing: `getpid` cannot fail.
    let processId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : ProcessId
        =
        system.Process.ProcessId

    /// `umask(2)`: make `mask` the process's file-mode creation mask, and answer
    /// the mask it replaces.
    ///
    /// `mask` is raw, as the kernel takes it. The process keeps only the bits
    /// `SimulatedUnixPlatform.umaskStoredBits` names, 0o777 on Linux and 0o7777
    /// on Darwin, and ignores the rest, so a later call answers `mask` narrowed
    /// to those bits. Total: `umask` cannot fail.
    ///
    /// The mask is the process's, shared by all its threads: measured on both,
    /// a thread's `umask` is what every other thread's next call answers.
    let umask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (mask : int)
        (system : UnixSystem<'Task, 'Handler>)
        : PermissionBits * UnixSystem<'Task, 'Handler>
        =
        let kept =
            mask
            &&& PermissionBits.toInt (SimulatedUnixPlatform.umaskStoredBits system.Machine.UnixPlatform)
            |> PermissionBits.parseOrFail "UnixSystem.umask"

        system.Process.Umask,
        { system with
            Process =
                { system.Process with
                    Umask = kept
                }
        }

    /// Answer one syscall, made by `task`.
    ///
    /// The task is what a blocking answer is recorded against: `FLock` that
    /// would block parks it, and the returned system carries that park.
    ///
    /// Sugar over the per-syscall functions above, for a client that wants one
    /// surface — to log every syscall, to replay a recorded sequence, or to
    /// generate them. Where a syscall's own function has a narrower type (the
    /// answer to `GetEffectiveUserId` cannot be a failure, `Dup` cannot be
    /// refused, and only `FLock` can block), that type is the one to prefer.
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
        (task : 'Task)
        (call : Syscall)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallOutcome * UnixSystem<'Task, 'Handler>, SyscallRefusal<'Task>>
        =
        // `flock` is the only one of these that can block, so it is the only one
        // whose own function already speaks `SyscallOutcome`; the rest answer
        // and are lifted. That is this layer being uniform where the individual
        // functions are precise, which is what its docstring above says it is
        // for.
        let answered
            (result : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, 'refusal>)
            : Result<SyscallOutcome * UnixSystem<'Task, 'Handler>, 'refusal>
            =
            result
            |> Result.map (fun (answer, system) -> SyscallOutcome.Answered answer, system)

        match call with
        | Syscall.GetEffectiveUserId ->
            Ok (
                SyscallOutcome.Answered (
                    SyscallAnswer.Completed (int64 (UserId.toUInt32 (UnixDescriptor.effectiveUserId system)))
                ),
                system
            )
        | Syscall.GetEffectiveGroupId ->
            Ok (
                SyscallOutcome.Answered (
                    SyscallAnswer.Completed (int64 (GroupId.toUInt32 (UnixDescriptor.effectiveGroupId system)))
                ),
                system
            )
        | Syscall.GetProcessId ->
            Ok (
                SyscallOutcome.Answered (SyscallAnswer.Completed (int64 (ProcessId.toInt32 (processId system)))),
                system
            )
        | Syscall.Dup fd -> UnixDescriptor.dup fd system |> answered |> Result.mapError SyscallRefusal.Dup
        | Syscall.Fcntl (fd, command, argument) ->
            UnixDescriptor.fcntl fd command argument system
            |> answered
            |> Result.mapError SyscallRefusal.Fcntl
        | Syscall.Dup2 (oldFd, newFd) ->
            UnixDescriptor.dup2 oldFd newFd system
            |> answered
            |> Result.mapError SyscallRefusal.Dup2
        | Syscall.Dup3 (oldFd, newFd, flags) ->
            UnixDescriptor.dup3 oldFd newFd flags system
            |> answered
            |> Result.mapError SyscallRefusal.Dup3
        | Syscall.LSeek (fd, offset, whence) ->
            UnixDescriptor.lseek fd offset whence system
            |> answered
            |> Result.mapError SyscallRefusal.LSeek
        | Syscall.FLock (fd, operation) ->
            UnixDescriptor.flock task fd operation system
            |> Result.mapError SyscallRefusal.FLock
        | Syscall.FTruncate (fd, length) ->
            UnixDescriptor.ftruncate fd length system
            |> answered
            |> Result.mapError SyscallRefusal.FTruncate
        | Syscall.Close fd ->
            UnixDescriptor.close fd system
            |> answered
            |> Result.mapError SyscallRefusal.Close
        | Syscall.MkDirAt (dirfd, path, mode) ->
            UnixNamespace.mkdirat dirfd path mode system
            |> answered
            |> Result.mapError SyscallRefusal.MkDir
        | Syscall.UnlinkAt (dirfd, path, flags) ->
            UnixNamespace.unlinkat dirfd path flags system
            |> answered
            |> Result.mapError SyscallRefusal.UnlinkAt
        | Syscall.ChDir path ->
            UnixPathResolution.chdir path system
            |> answered
            |> Result.mapError SyscallRefusal.ChDir
        | Syscall.FChModAt (dirfd, path, mode, flags) ->
            UnixPathResolution.fchmodat dirfd path mode flags system
            |> answered
            |> Result.mapError SyscallRefusal.FChModAt
        | Syscall.FChMod (fd, mode) ->
            UnixPathResolution.fchmod fd mode system
            |> answered
            |> Result.mapError SyscallRefusal.FChMod
        | Syscall.FChOwnAt (dirfd, path, user, group, flags) ->
            UnixPathResolution.fchownat dirfd path user group flags system
            |> answered
            |> Result.mapError SyscallRefusal.FChOwnAt
        | Syscall.FChOwn (fd, user, group) ->
            UnixPathResolution.fchown fd user group system
            |> answered
            |> Result.mapError SyscallRefusal.FChOwn
        | Syscall.UMask mask ->
            let previous, system = umask mask system

            Ok (SyscallOutcome.Answered (SyscallAnswer.Completed (int64 (PermissionBits.toInt previous))), system)
        | Syscall.FAccessAt (dirfd, path, mode, flags) ->
            UnixPathResolution.faccessat dirfd path mode flags system
            |> Result.map (fun answer -> SyscallOutcome.Answered answer, system)
            |> Result.mapError SyscallRefusal.Access
        | Syscall.SymlinkAt (target, dirfd, path) ->
            UnixNamespace.symlinkat target dirfd path system
            |> Result.map (fun (answer, system) -> SyscallOutcome.Answered answer, system)
            |> Result.mapError SyscallRefusal.Symlink
        | Syscall.LinkAt (olddirfd, oldpath, newdirfd, newpath, flags) ->
            UnixNamespace.linkat olddirfd oldpath newdirfd newpath flags system
            |> Result.map (fun (answer, system) -> SyscallOutcome.Answered answer, system)
            |> Result.mapError SyscallRefusal.Link
        | Syscall.UTimensAt (dirfd, path, times, flags) ->
            UnixPathResolution.utimensat dirfd path times flags system
            |> answered
            |> Result.mapError SyscallRefusal.UTimensAt
        | Syscall.CopyFileRange (inFd, outFd, length, flags) ->
            UnixReadWrite.copyFileRange inFd outFd length flags system
            |> answered
            |> Result.mapError SyscallRefusal.CopyFileRange
        | Syscall.FileClone (destination, source) ->
            UnixDescriptor.fileClone destination source system
            |> Result.map (fun error -> SyscallOutcome.Answered (SyscallAnswer.Failed error), system)
            |> Result.mapError SyscallRefusal.FileClone
        | Syscall.CloneFile (source, destination, flags) ->
            UnixNamespace.cloneFile source destination flags system
            |> answered
            |> Result.mapError SyscallRefusal.CloneFile
        | Syscall.SetResUid (real, effective, saved) ->
            UnixCredentials.setresuid real effective saved system
            |> answered
            |> Result.mapError SyscallRefusal.SetIds
        | Syscall.SetResGid (real, effective, saved) ->
            UnixCredentials.setresgid real effective saved system
            |> answered
            |> Result.mapError SyscallRefusal.SetIds
        | Syscall.SetGroups (size, words) ->
            UnixCredentials.setgroups size words system
            |> answered
            |> Result.mapError SyscallRefusal.SetGroups

    /// Every way the machine's tables disagree with each other or with the
    /// processes on it: the clauses of `checkInvariants` that concern the
    /// machine, each of which reads facts that every process on it contributes
    /// to, so that no one process's view can check it.
    ///
    /// The socket table and the pipe table against the open file descriptions,
    /// each pipe and the pipe device against the platform, the connection
    /// table against the sockets that reference it, the open file descriptions
    /// against the filesystem and the platform, each description against the
    /// descriptors and calls that reference it and its holds against every
    /// process's parks, the park and event registration ordinals against the
    /// machine's counters, the thread ID allocator against every process's
    /// tasks, each processor's occupant against the tasks, and the machine's
    /// filesystem type, buffer check and symbolic links against its platform.
    ///
    /// When `complete`, `processes` is every process on the machine, each with
    /// its tasks, and every clause is checked. Otherwise `processes` is some of
    /// them, and only the clauses those can check truthfully are: every clause
    /// that counts or collects across every process (the holds on
    /// descriptions, the live thread and process IDs, the current directory
    /// holds) is skipped.
    let internal machineDefects<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (complete : bool)
        (processes : (UnixProcessState<'Task, 'Handler> * Map<'Task, UnixTaskState>) list)
        (machine : UnixMachineState)
        : UnixSystemDefect<'Task> list
        =
        let onlyIfComplete (defects : unit -> UnixSystemDefect<'Task> list) : UnixSystemDefect<'Task> list =
            if complete then defects () else []

        let allTasks : ('Task * UnixTaskState) list =
            processes |> List.collect (fun (_, tasks) -> Map.toList tasks)


        let named =
            OpenFileTable.descriptions machine.OpenFiles
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> None
                | OpenFileTarget.Socket socketId -> Some (id, socketId)
            )

        let dangling =
            named
            |> List.filter (fun (_, socketId) -> not (Map.containsKey socketId machine.Sockets))
            |> List.map UnixSystemDefect.DanglingSocket

        let namedIds = named |> List.map snd |> Set.ofList

        let unreferenced =
            machine.Sockets
            |> Map.toList
            |> List.map fst
            |> List.filter (fun socketId -> not (Set.contains socketId namedIds))
            |> List.map UnixSystemDefect.UnreferencedSocket

        // Against the table rather than against the descriptions: the table is
        // where a socket lives, so it is the table that must stay below the
        // counter even once a socket can outlive every descriptor of it.
        let freshness =
            machine.Sockets
            |> Map.toList
            |> List.map fst
            |> List.filter (fun socketId -> socketId >= machine.NextSocketId)
            |> List.map (fun socketId -> UnixSystemDefect.NextSocketIdNotFresh (machine.NextSocketId, socketId))

        let foreignObjects =
            let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

            OpenFileTable.descriptions machine.OpenFiles
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target, flavour with
                | OpenFileTarget.Epoll _, SimulatedUnixFlavour.Darwin
                | OpenFileTarget.Kqueue _, SimulatedUnixFlavour.Linux
                // Only Linux's devtmpfs holds a device's node.
                | OpenFileTarget.CharacterDevice _, SimulatedUnixFlavour.Darwin ->
                    Some (UnixSystemDefect.DescriptionNotOfFlavour (id, description.Target, flavour))
                | OpenFileTarget.Epoll _, SimulatedUnixFlavour.Linux
                | OpenFileTarget.Kqueue _, SimulatedUnixFlavour.Darwin
                | OpenFileTarget.CharacterDevice _, SimulatedUnixFlavour.Linux
                | OpenFileTarget.File _, _
                | OpenFileTarget.Directory _, _
                | OpenFileTarget.Socket _, _
                | OpenFileTarget.Pipe _, _ -> None
            )

        let danglingInodes =
            machine.OpenFiles
            |> OpenFileTable.descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _) ->
                    match VirtualFileSystem.tryGetContent inode machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.Directory _)
                    | Some (InodeContent.CharacterDevice _) ->
                        Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.Symlink _) -> None
                | OpenFileTarget.Directory (inode, _) ->
                    match VirtualFileSystem.tryGetContent inode machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.Directory _) -> None
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.CharacterDevice _)
                    | Some (InodeContent.Symlink _) -> Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                | OpenFileTarget.CharacterDevice (inode, device) ->
                    match VirtualFileSystem.tryGetContent inode machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.CharacterDevice (node, _)) when node = device -> None
                    | Some (InodeContent.CharacterDevice _)
                    | Some (InodeContent.Directory _)
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.Symlink _) -> Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.Socket _
                | OpenFileTarget.Pipe _ -> None
            )

        // Every reference any socket makes to a connection: as the end it is
        // (`Some`), or through an accept queue (`None`), which has its own
        // defect case and its own no-duplicates rule.
        let connectionReferences =
            machine.Sockets
            |> Map.toList
            |> List.collect (fun (socketId, socket) ->
                match socket.Phase with
                | SocketPhase.Established (connection, connectionEnd) -> [ socketId, connection, Some connectionEnd ]
                | SocketPhase.EstablishedPendingReport connection ->
                    [ socketId, connection, Some ConnectionEnd.Client ]
                | SocketPhase.Listening listenState ->
                    listenState.Queue |> List.map (fun connection -> socketId, connection, None)
                | SocketPhase.Idle
                | SocketPhase.Refused _
                | SocketPhase.DatagramPeer _ -> []
            )

        let danglingConnections =
            connectionReferences
            |> List.filter (fun (_, connection, _) -> not (Map.containsKey connection machine.Connections))
            |> List.map (fun (socketId, connection, heldAs) ->
                match heldAs with
                | None -> UnixSystemDefect.DanglingQueuedConnection (socketId, connection)
                | Some _ -> UnixSystemDefect.DanglingConnection (socketId, connection)
            )

        let referencedConnections =
            connectionReferences
            |> List.map (fun (_, connection, _) -> connection)
            |> Set.ofList

        let orphanConnections =
            machine.Connections
            |> Map.toList
            |> List.map fst
            |> List.filter (fun connection -> not (Set.contains connection referencedConnections))
            |> List.map UnixSystemDefect.OrphanConnection

        let duplicateQueued =
            connectionReferences
            |> List.choose (fun (_, connection, heldAs) ->
                match heldAs with
                | None -> Some connection
                | Some _ -> None
            )
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (connection, _) -> UnixSystemDefect.DuplicateQueuedConnection connection)

        // A connection has one client and one server. Until `accept(2)` the
        // server end is the listener's queue entry, so the server end is held
        // once in all, queued or accepted. A listener counts once however
        // often it queues the connection: that is `DuplicateQueuedConnection`.
        let connectionEndsHeldTwice =
            connectionReferences
            |> List.groupBy (fun (_, connection, _) -> connection)
            |> List.collect (fun (connection, references) ->
                [ ConnectionEnd.Client ; ConnectionEnd.Server ]
                |> List.choose (fun connectionEnd ->
                    let holders =
                        references
                        |> List.choose (fun (socketId, _, heldAs) ->
                            let holdsThisEnd =
                                match heldAs with
                                | Some held -> held = connectionEnd
                                | None -> connectionEnd = ConnectionEnd.Server

                            if holdsThisEnd then Some socketId else None
                        )
                        |> List.distinct

                    if List.length holders > 1 then
                        Some (UnixSystemDefect.ConnectionEndHeldTwice (connection, connectionEnd, holders))
                    else
                        None
                )
            )

        // Each connection's transfer against its own rules, the machine's
        // flavour, and the sockets that are its ends: an end is closed exactly
        // when no socket holds it, a queued server end counting as held.
        let transferDefects =
            machine.Connections
            |> Map.toList
            |> List.collect (fun (connection, tcp) ->
                let broken =
                    match TcpTransfer.violations tcp.Transfer with
                    | [] -> []
                    | violations -> [ UnixSystemDefect.TcpTransferBroken (connection, violations) ]

                let rulesFlavour =
                    match tcp.Transfer.Rules with
                    | TcpTransferRules.Linux _ -> SimulatedUnixFlavour.Linux
                    | TcpTransferRules.Darwin -> SimulatedUnixFlavour.Darwin

                let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

                let ofFlavour =
                    if rulesFlavour = flavour then
                        []
                    else
                        [ UnixSystemDefect.TcpTransferNotOfFlavour (connection, rulesFlavour, flavour) ]

                // An orphan has no holder of either end, which
                // `OrphanConnection` reports alone.
                let ends =
                    if not (Set.contains connection referencedConnections) then
                        []
                    else

                    [ ConnectionEnd.Client ; ConnectionEnd.Server ]
                    |> List.collect (fun connectionEnd ->
                        let holders =
                            connectionReferences
                            |> List.choose (fun (socketId, referenced, heldAs) ->
                                let holdsThisEnd =
                                    match heldAs with
                                    | Some held -> held = connectionEnd
                                    | None -> connectionEnd = ConnectionEnd.Server

                                if referenced = connection && holdsThisEnd then
                                    Some socketId
                                else
                                    None
                            )
                            |> List.distinct

                        match (TcpTransfer.towards connectionEnd tcp.Transfer).Receiver, holders with
                        | TcpEndState.Closed, [] -> []
                        | TcpEndState.Closed, holders ->
                            holders
                            |> List.map (fun socketId ->
                                UnixSystemDefect.ConnectionEndClosedUnderSocket (connection, connectionEnd, socketId)
                            )
                        | _, [] -> [ UnixSystemDefect.ConnectionEndOpenWithoutHolder (connection, connectionEnd) ]
                        | _, _ -> []
                    )

                broken @ ofFlavour @ ends
            )

        let phaseKindMismatches =
            machine.Sockets
            |> Map.toList
            |> List.choose (fun (socketId, socket) ->
                let mismatched =
                    match socket.Kind, socket.Phase with
                    | SocketKind.Datagram, SocketPhase.Idle
                    | SocketKind.Datagram, SocketPhase.DatagramPeer _ -> false
                    | SocketKind.Datagram, _ -> true
                    | _, SocketPhase.DatagramPeer _ -> true
                    | _, _ -> false

                if mismatched then
                    Some (UnixSystemDefect.SocketPhaseKindMismatch (socketId, socket.Kind, socket.Phase))
                else
                    None
            )

        let drainedUnderLinux =
            match SimulatedUnixPlatform.flavour machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin -> []
            | SimulatedUnixFlavour.Linux ->
                machine.Sockets
                |> Map.toList
                |> List.choose (fun (socketId, socket) ->
                    match socket.Phase with
                    | SocketPhase.Listening {
                                                Drained = true
                                            } -> Some (UnixSystemDefect.ListenerDrainedUnderLinux socketId)
                    | _ -> None
                )

        let connectionFreshness =
            machine.Connections
            |> Map.toList
            |> List.map fst
            |> List.filter (fun connection -> connection >= machine.NextConnectionId)
            |> List.map (fun connection ->
                UnixSystemDefect.NextConnectionIdNotFresh (machine.NextConnectionId, connection)
            )

        let registrationOrdinals =
            machine.OpenFiles
            |> OpenFileTable.descriptions
            |> Map.toList
            |> List.collect (fun (queueId, description) ->
                match description.Target with
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.Kqueue state ->
                    state.Registrations
                    |> Map.toList
                    |> List.map (fun (_, registration) -> queueId, registration.RegisteredAt)
                | OpenFileTarget.Epoll epollState ->
                    epollState.Registrations
                    |> Map.toList
                    |> List.map (fun (_, registration) -> queueId, registration.RegisteredAt)
            )

        let ordinalFreshness =
            registrationOrdinals
            |> List.filter (fun (_, registeredAt) -> registeredAt >= machine.NextEventRegistrationOrdinal)
            |> List.map (fun (queueId, registeredAt) ->
                UnixSystemDefect.EventRegistrationOrdinalNotFresh (
                    machine.NextEventRegistrationOrdinal,
                    queueId,
                    registeredAt
                )
            )

        let ordinalDuplicates =
            registrationOrdinals
            |> List.countBy snd
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (registeredAt, _) -> UnixSystemDefect.DuplicateEventRegistrationOrdinal registeredAt)

        let statusOfFlavour =
            let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

            OpenFileTable.descriptions machine.OpenFiles
            |> Map.toList
            |> List.collect (fun (id, description) ->
                let status = description.Status

                let foreign =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> status.Written || status.Flocked
                    | SimulatedUnixFlavour.Darwin -> status.OpenedDirectory || status.OpenedNoFollow

                let unrecorded =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> false
                    | SimulatedUnixFlavour.Darwin -> description.Flock.IsSome && not status.Flocked

                [
                    if foreign then
                        yield UnixSystemDefect.StatusNotOfFlavour (id, status, flavour)
                    if unrecorded then
                        yield UnixSystemDefect.FlockHeldNotRecorded id
                ]
            )


        let unpairedUnderLinux =
            match SimulatedUnixPlatform.flavour machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin -> []
            | SimulatedUnixFlavour.Linux ->
                OpenFileTable.descriptions machine.OpenFiles
                |> Map.toList
                |> List.filter (fun (_, description) ->
                    description.Status.Synchronous <> description.Status.DataSynchronous
                )
                |> List.map (fun (id, _) -> UnixSystemDefect.UnpairedSynchronisationUnderLinux id)

        // Every description is referenced: by a descriptor, or by a call in
        // flight that holds it. Both counts are the description's own: the
        // descriptors', which `OpenFileTable.checkInvariants` holds to the
        // descriptor tables, and the holds, which `holdCounts` holds to the
        // parks.
        let unreferencedDescriptions =
            OpenFileTable.descriptions machine.OpenFiles
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id ->
                OpenFileTable.descriptorCount id machine.OpenFiles = Some 0
                && OpenFileTable.holdCount id machine.OpenFiles = Some 0
            )
            |> List.map UnixSystemDefect.UnreferencedDescription

        // A socket whose release `SO_LINGER` could have refused is never left
        // for a call's return to release, which could not refuse it.
        let lingeringHeldOnlyByCalls =
            OpenFileTable.descriptions machine.OpenFiles
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Socket socketId when
                    OpenFileTable.descriptorCount id machine.OpenFiles = Some 0
                    && OpenFileTable.holdCount id machine.OpenFiles
                       |> Option.exists (fun holds -> holds > 0)
                    ->
                    // A description onto an absent socket is `DanglingSocket`'s.
                    match Map.tryFind socketId machine.Sockets with
                    | Some socket when ObjectLifetime.lingerCanRefuseRelease socket ->
                        Some (UnixSystemDefect.LingeringSocketHeldOnlyByCalls (id, socketId))
                    | Some _
                    | None -> None
                | OpenFileTarget.Socket _
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> None
            )

        // Each description's holds against the parks of every process's tasks:
        // one for each time a park names it. A park naming a description the
        // table does not hold is `ParkedOnAbsentDescription`'s.
        let holdCounts =
            onlyIfComplete
            <| fun () ->

                let implied =
                    allTasks
                    |> List.collect (fun (_, state) ->
                        match state.Parked with
                        | None -> []
                        | Some park -> ParkedSyscall.descriptions park.Syscall
                    )
                    |> List.countBy id
                    |> Map.ofList

                OpenFileTable.descriptions machine.OpenFiles
                |> Map.toList
                |> List.choose (fun (id, _) ->
                    let recorded = OpenFileTable.holdCount id machine.OpenFiles |> Option.defaultValue 0
                    let parks = Map.tryFind id implied |> Option.defaultValue 0

                    if recorded = parks then
                        None
                    else
                        Some (UnixSystemDefect.HoldCountMismatch (id, recorded, parks))
                )

        // Each sleeping Darwin poll's kqueue against the parks of every
        // process's tasks: exactly one names it. A park naming one the machine
        // does not hold is `ParkedOnAbsentPollQueue`'s.
        let pollQueueParks =
            onlyIfComplete
            <| fun () ->

                let parks =
                    allTasks
                    |> List.choose (fun (_, state) ->
                        match state.Parked with
                        | Some {
                                   Syscall = ParkedSyscall.KqueuePoll poll
                               } -> Some poll.Queue
                        | Some _
                        | None -> None
                    )
                    |> List.countBy id
                    |> Map.ofList

                machine.PollQueues
                |> Map.toList
                |> List.choose (fun (queue, _) ->
                    match Map.tryFind queue parks |> Option.defaultValue 0 with
                    | 1 -> None
                    | count -> Some (UnixSystemDefect.PollQueueNotHeldOnce (queue, count))
                )

        let pollQueueFreshness =
            machine.PollQueues
            |> Map.toList
            |> List.filter (fun (queue, _) -> queue >= machine.NextPollQueueId)
            |> List.map (fun (queue, _) -> UnixSystemDefect.PollQueueIdNotFresh (machine.NextPollQueueId, queue))


        let parkOrdinals =
            allTasks
            |> List.choose (fun (task, state) -> state.Parked |> Option.map (fun park -> task, park.Ordinal))

        let parkOrdinalFreshness =
            parkOrdinals
            |> List.filter (fun (_, ordinal) -> ordinal >= machine.NextParkOrdinal)
            |> List.map (fun (task, ordinal) ->
                UnixSystemDefect.ParkOrdinalNotFresh (machine.NextParkOrdinal, task, ordinal)
            )

        let parkOrdinalDuplicates =
            parkOrdinals
            |> List.countBy snd
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (ordinal, _) -> UnixSystemDefect.DuplicateParkOrdinal ordinal)

        // Bindings no bind or listen could have produced.
        let bindings =
            machine.Sockets
            |> Map.toList
            |> List.collect (fun (socketId, socket) ->
                let unboundListener =
                    match socket.Phase, socket.Binding with
                    | SocketPhase.Listening _, None -> [ UnixSystemDefect.ListenerWithoutBinding socketId ]
                    | _ -> []

                let unaddressable =
                    match socket.Addressing, socket.Phase with
                    | (SocketAddressing.Inet6V6Only | SocketAddressing.Unix), SocketPhase.Idle
                    | (SocketAddressing.Inet _ | SocketAddressing.Inet6DualMode _), _ -> []
                    | (SocketAddressing.Inet6V6Only | SocketAddressing.Unix) as addressing, phase ->
                        [ UnixSystemDefect.UnaddressableSocketInPhase (socketId, addressing, phase) ]

                let portZero =
                    match socket.Binding with
                    | Some binding when binding.Endpoint.Port = 0us ->
                        let halfBound =
                            // Only Linux's `connect(AF_UNSPEC)` produces this,
                            // and it always leaves the socket idle.
                            SimulatedUnixPlatform.flavour machine.UnixPlatform = SimulatedUnixFlavour.Linux
                            && socket.Kind = SocketKind.Datagram
                            && socket.Phase = SocketPhase.Idle
                            && not binding.LockedPort
                            && (
                                match binding.LockedAddress with
                                | Some locked ->
                                    locked <> InternetEndpoint.WildcardAddress && locked = binding.Endpoint.Address
                                | None -> false
                            )

                        if halfBound then
                            []
                        else
                            [ UnixSystemDefect.BoundToPortZero socketId ]
                    | _ -> []

                unboundListener @ unaddressable @ portZero
            )

        let fileSystemType =
            let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

            let fsType = EmulatedMount.fileSystemType machine.Mount

            if EmulatedFileSystemType.isReportableUnder flavour fsType then
                []
            else
                [ UnixSystemDefect.FileSystemTypeNotReportable (flavour, fsType) ]

        let protectedFiles =
            let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

            if UnixMachineState.isProtectedFilesOf flavour machine.ProtectedFiles then
                []
            else
                [
                    UnixSystemDefect.ProtectedFilesNotOfFlavour (machine.ProtectedFiles, flavour)
                ]

        let userBufferCheck =
            if UnixMachineState.isUserBufferCheckOf machine.UnixPlatform machine.UserBufferCheck then
                []
            else
                [
                    UnixSystemDefect.UserBufferCheckNotOfPlatform (machine.UnixPlatform, machine.UserBufferCheck)
                ]

        // Every symbolic link's bits are ones its flavour creates a link with,
        // under the umask that would leave exactly those bits, unless the
        // flavour changes a link's bits afterwards, which makes any bits ones a
        // link can have.
        let symlinkPermissions =
            let platform = machine.UnixPlatform

            match SimulatedUnixPlatform.symlinkModeChange platform with
            | SymlinkModeChange.ChangesLink -> []
            | SymlinkModeChange.NotSupported ->

            VirtualFileSystem.inodes machine.FileSystem
            |> Map.toList
            |> List.choose (fun (inode, entry) ->
                match entry.Content with
                | InodeContent.Symlink (_, bits) ->
                    let umask =
                        PermissionBits.parseOrFail
                            "UnixSystem.checkInvariants"
                            (0o777 &&& ~~~(PermissionBits.toInt bits))

                    if SimulatedUnixPlatform.symlinkCreationPermissions platform umask = bits then
                        None
                    else
                        Some (
                            UnixSystemDefect.SymlinkPermissionsNotOfFlavour (
                                inode,
                                bits,
                                SimulatedUnixPlatform.flavour platform
                            )
                        )
                | InodeContent.RegularFile _
                | InodeContent.Directory _
                | InodeContent.CharacterDevice _ -> None
            )

        // The machine's thread ID allocator against the tasks of every process
        // on it: the allocator is its flavour's, no two tasks share an ID, every
        // ID is one the counter could have handed out, and the IDs the
        // allocator records as live are exactly the tasks', so none can be
        // handed out again while its task lives.
        let threadIds =
            let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform
            let allocator = machine.ThreadIds

            let allocatorFlavour =
                match flavour, allocator.Counter with
                | SimulatedUnixFlavour.Linux, ThreadIdCounter.Linux _
                | SimulatedUnixFlavour.Darwin, ThreadIdCounter.Darwin _ -> []
                | SimulatedUnixFlavour.Linux, ThreadIdCounter.Darwin _
                | SimulatedUnixFlavour.Darwin, ThreadIdCounter.Linux _ ->
                    [ UnixSystemDefect.ThreadIdAllocatorNotOfFlavour (flavour, allocator) ]

            let duplicates =
                allTasks
                |> List.groupBy (fun (_, state) -> state.OsThreadId)
                |> List.choose (fun (id, holders) ->
                    match holders with
                    | []
                    | [ _ ] -> None
                    | _ -> Some (UnixSystemDefect.DuplicateOsThreadId (id, List.map fst holders))
                )

            let unmintable =
                allTasks
                |> List.filter (fun (_, state) -> not (ThreadIdAllocator.couldHaveMinted state.OsThreadId allocator))
                |> List.map (fun (task, state) ->
                    UnixSystemDefect.OsThreadIdNotMintable (task, state.OsThreadId, allocator)
                )

            let live =
                onlyIfComplete
                <| fun () ->

                    let held = allTasks |> List.map (fun (_, state) -> state.OsThreadId) |> Set.ofList
                    let recorded = ThreadIdAllocator.live allocator

                    if held = recorded then
                        []
                    else
                        [
                            UnixSystemDefect.LiveThreadIdsMismatch (
                                Set.difference recorded held,
                                Set.difference held recorded
                            )
                        ]

            allocatorFlavour @ duplicates @ unmintable @ live

        // Each processor's occupant against the machine and the tasks: the
        // processor is the machine's, the occupant is a live task, and that
        // task's processor is the one it occupies. Short of every process, an
        // occupant of no process given is checked against the thread IDs the
        // machine records as live, which every process's tasks contribute to.
        let occupants =
            let byThreadId =
                allTasks
                |> List.map (fun (task, state) -> state.OsThreadId, (task, state))
                |> Map.ofList

            machine.Occupants
            |> Map.toList
            |> List.collect (fun (cpu, occupant) ->
                let beyond =
                    if UnixMachineState.hasProcessor cpu machine then
                        []
                    else
                        [
                            UnixSystemDefect.OccupiedCpuBeyondMachine (cpu, occupant, machine.ProcessorCount)
                        ]

                let task =
                    match Map.tryFind occupant byThreadId with
                    | Some (task, state) ->
                        if state.Cpu = cpu then
                            []
                        else
                            [ UnixSystemDefect.OccupantOnAnotherCpu (task, cpu, state.Cpu) ]
                    | None ->
                        if
                            complete
                            || not (Set.contains occupant (ThreadIdAllocator.live machine.ThreadIds))
                        then
                            [ UnixSystemDefect.OccupantWithoutTask (cpu, occupant) ]
                        else
                            []

                beyond @ task
            )

        // The process table against the processes: the counter is the
        // flavour's, Darwin's is past every live ID, and the live IDs are the
        // processes'.
        let processIds =
            let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform
            let table = machine.ProcessIds

            let counter =
                match flavour, table.Counter with
                | SimulatedUnixFlavour.Linux, ProcessIdCounter.ThreadIds -> []
                | SimulatedUnixFlavour.Darwin, ProcessIdCounter.Darwin next ->
                    table.Live
                    |> Set.toList
                    |> List.filter (fun pid -> ProcessId.toInt32 pid >= next)
                    |> List.map (fun pid -> UnixSystemDefect.ProcessIdNotBelowCounter (pid, next))
                | SimulatedUnixFlavour.Linux, ProcessIdCounter.Darwin _
                | SimulatedUnixFlavour.Darwin, ProcessIdCounter.ThreadIds ->
                    [ UnixSystemDefect.ProcessIdCounterNotOfFlavour (flavour, table.Counter) ]

            let live =
                onlyIfComplete
                <| fun () ->

                    let held = processes |> List.map (fun (proc, _) -> proc.ProcessId) |> Set.ofList

                    let recorded = ProcessIdTable.live table

                    if held = recorded then
                        []
                    else
                        [
                            UnixSystemDefect.LiveProcessIdsMismatch (
                                Set.difference recorded held,
                                Set.difference held recorded
                            )
                        ]

            counter @ live

        // The current directory holds against the processes standing in them.
        let currentDirectories =
            onlyIfComplete
            <| fun () ->

                let standing =
                    processes
                    |> List.countBy (fun (proc, _) -> proc.CurrentDirectoryInode)
                    |> Map.ofList

                Set.union (Map.keys standing |> Set.ofSeq) (Map.keys machine.CurrentDirectories |> Set.ofSeq)
                |> Set.toList
                |> List.choose (fun inode ->
                    let recorded = Map.tryFind inode machine.CurrentDirectories |> Option.defaultValue 0
                    let standing = Map.tryFind inode standing |> Option.defaultValue 0

                    if recorded = standing then
                        None
                    else
                        Some (UnixSystemDefect.CurrentDirectoryHoldMismatch (inode, recorded, standing))
                )

        // Every kqueue belongs to a live process, whose table its registrations
        // are read in.
        let kqueueOwners =
            OpenFileTable.toSeq machine.OpenFiles
            |> Seq.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.Kqueue state when
                    not (Set.contains state.Owner (ProcessIdTable.live machine.ProcessIds))
                    ->
                    Some (UnixSystemDefect.KqueueOwnerNotLive (id, state.Owner))
                | OpenFileTarget.Kqueue _
                | OpenFileTarget.Epoll _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> None
            )
            |> Seq.toList


        // The pipe table against the descriptions naming its pipes, and each
        // pipe against its machine, as for sockets above.
        let pipes =
            let platform = machine.UnixPlatform
            let flavour = SimulatedUnixPlatform.flavour platform

            let named =
                OpenFileTable.descriptions machine.OpenFiles
                |> Map.toList
                |> List.choose (fun (id, description) ->
                    match description.Target with
                    | OpenFileTarget.Pipe (pipeId, _) -> Some (id, pipeId)
                    | OpenFileTarget.Kqueue _
                    | OpenFileTarget.Epoll _
                    | OpenFileTarget.File _
                    | OpenFileTarget.Directory _
                    | OpenFileTarget.CharacterDevice _
                    | OpenFileTarget.Socket _ -> None
                )

            let dangling =
                named
                |> List.filter (fun (_, pipeId) -> not (Map.containsKey pipeId machine.Pipes))
                |> List.map UnixSystemDefect.DanglingPipe

            let namedIds = named |> List.map snd |> Set.ofList

            let unreferenced =
                machine.Pipes
                |> Map.toList
                |> List.filter (fun (pipeId, pipe) ->
                    not (Set.contains pipeId namedIds)
                    && not (PipeState.heldByClient PipeEnd.Read pipe)
                    && not (PipeState.heldByClient PipeEnd.Write pipe)
                )
                |> List.map (fst >> UnixSystemDefect.UnreferencedPipe)

            let undrained =
                machine.Pipes
                |> Map.toList
                |> List.choose (fun (pipeId, pipe) ->
                    let held = PipeBuffer.held pipe.Buffer

                    match PipeState.drainedBy pipe with
                    | Some _ when held > 0 -> Some (UnixSystemDefect.DrainedPipeHoldsBytes (pipeId, held))
                    | Some _
                    | None -> None
                )

            let unsupplied =
                machine.Pipes
                |> Map.toList
                |> List.filter (fun (_, pipe) -> PipeState.clientWriteCouldProceed pipe)
                |> List.map (fst >> UnixSystemDefect.SuppliedPipeHasRoom)

            let freshness =
                machine.Pipes
                |> Map.toList
                |> List.map fst
                |> List.filter (fun pipeId -> pipeId >= machine.NextPipeId)
                |> List.map (fun pipeId -> UnixSystemDefect.NextPipeIdNotFresh (machine.NextPipeId, pipeId))

            let inodes =
                machine.Pipes
                |> Map.toList
                |> List.collect (fun (pipeId, pipe) ->
                    match pipe.Origin with
                    | PipeOrigin.Launched _ -> []
                    | PipeOrigin.Made status ->
                        match status.Inodes with
                        | PipeInodes.Shared inode -> [ pipeId, inode ]
                        | PipeInodes.PerEnd (readEnd, writeEnd) -> [ pipeId, readEnd ; pipeId, writeEnd ]
                )

            let inodeFreshness =
                inodes
                |> List.filter (fun (_, inode) -> inode >= machine.NextPipeInode)
                |> List.map (fun (pipeId, inode) ->
                    UnixSystemDefect.PipeInodeNotFresh (machine.NextPipeInode, pipeId, inode)
                )

            let inodeDuplicates =
                inodes
                |> List.countBy snd
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (inode, _) -> UnixSystemDefect.DuplicatePipeInode inode)

            let shapes =
                machine.Pipes
                |> Map.toList
                |> List.choose (fun (pipeId, pipe) ->
                    let inodesOfFlavour =
                        match pipe.Origin with
                        | PipeOrigin.Launched _ -> true
                        | PipeOrigin.Made status ->
                            match status.Inodes, flavour with
                            | PipeInodes.Shared _, SimulatedUnixFlavour.Linux
                            | PipeInodes.PerEnd _, SimulatedUnixFlavour.Darwin -> true
                            | PipeInodes.Shared _, SimulatedUnixFlavour.Darwin
                            | PipeInodes.PerEnd _, SimulatedUnixFlavour.Linux -> false

                    if inodesOfFlavour && PipeBuffer.isOf platform pipe.Buffer then
                        None
                    else
                        Some (UnixSystemDefect.PipeNotOfPlatform (pipeId, platform))
                )

            let device =
                let device = machine.PipeDevice

                let ofFlavour =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> device >= 0L
                    | SimulatedUnixFlavour.Darwin -> device = 0L

                if ofFlavour then
                    []
                else
                    [ UnixSystemDefect.PipeDeviceNotOfFlavour (device, flavour) ]

            dangling
            @ unreferenced
            @ undrained
            @ unsupplied
            @ freshness
            @ inodeFreshness
            @ inodeDuplicates
            @ shapes
            @ device


        dangling
        @ unreferenced
        @ freshness
        @ foreignObjects
        @ danglingInodes
        @ danglingConnections
        @ orphanConnections
        @ duplicateQueued
        @ connectionEndsHeldTwice
        @ transferDefects
        @ phaseKindMismatches
        @ drainedUnderLinux
        @ connectionFreshness
        @ ordinalFreshness
        @ ordinalDuplicates
        @ unpairedUnderLinux
        @ statusOfFlavour
        @ unreferencedDescriptions
        @ lingeringHeldOnlyByCalls
        @ holdCounts
        @ pollQueueParks
        @ pollQueueFreshness
        @ parkOrdinalFreshness
        @ parkOrdinalDuplicates
        @ bindings
        @ fileSystemType
        @ protectedFiles
        @ symlinkPermissions
        @ userBufferCheck
        @ threadIds
        @ occupants
        @ processIds
        @ currentDirectories
        @ kqueueOwners
        @ pipes


    /// Every way one process's view of the machine disagrees with itself: the
    /// clauses of `checkInvariants` that concern the process `system` is, read
    /// against the machine. The current directory against the filesystem, each
    /// descriptor against the bound and the flavour's descriptor flags, each
    /// task's park against the descriptor table and the machine, each kqueue
    /// a descriptor or a `kevent` wait names against its owner, the signal
    /// state against the task table, the leader against the tasks, each task's
    /// processor against the machine's, and the process's supplementary groups
    /// and file-mode creation mask against its platform.
    let checkViewInvariants<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystemDefect<'Task> list
        =

        let currentDirectory =
            match VirtualFileSystem.tryGetContent system.Process.CurrentDirectoryInode system.Machine.FileSystem with
            | Some (InodeContent.Directory _) -> []
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.CharacterDevice _)
            | Some (InodeContent.Symlink _)
            | None ->
                [
                    UnixSystemDefect.CurrentDirectoryIsNotADirectory system.Process.CurrentDirectoryInode
                ]

        let beyondBound =
            let bound = SimulatedUnixPlatform.descriptorBound system.Machine.UnixPlatform

            FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)
            |> Map.toList
            |> List.filter (fun (fd, _) -> fd >= bound)
            |> List.map (fun (fd, _) -> UnixSystemDefect.DescriptorAtOrAboveBound (fd, bound))

        let closeOnForkUnderLinux =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin -> []
            | SimulatedUnixFlavour.Linux ->
                let registry = UnixSystemState.fileDescriptors system

                FileDescriptorRegistry.fds registry
                |> Map.toList
                |> List.filter (fun (fd, _) ->
                    match FileDescriptorRegistry.tryFindFlags fd registry with
                    | Some flags -> flags.CloseOnFork
                    | None -> false
                )
                |> List.map (fun (fd, _) -> UnixSystemDefect.CloseOnForkUnderLinux fd)


        // Each task's park against the descriptor table. A park names what the
        // task waits on, and the wake reads the description back; the park
        // holds it until the call returns, so an absent one was parked on
        // without going through the syscall or destroyed around it.
        let parks =
            let descriptions = OpenFileTable.descriptions system.Machine.OpenFiles

            let darwin =
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> false
                | SimulatedUnixFlavour.Darwin -> true

            // Under Darwin a close of the descriptor a sleeping accept, or pipe
            // or connection transfer, was made through ends the call, so while it sleeps the
            // descriptor still names what it sleeps on. Under Linux the close
            // leaves it asleep, and the number is not consulted.
            let enteredThrough (task : 'Task) (fd : int) (description : OpenFileDescriptionId) =
                match FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system) with
                | Some current when current = description -> []
                | _ when not darwin -> []
                | current ->
                    [
                        UnixSystemDefect.ParkedCallDescriptorRebound (task, fd, description, current)
                    ]

            let endedByClose (task : 'Task) =
                if darwin then
                    []
                else
                    [ UnixSystemDefect.ParkedCallEndedByCloseUnderLinux task ]

            // What is wrong with what a connection transfer sleeps on.
            let connectionTarget (task : 'Task) (target : SleepTarget<SocketId>) =
                match target with
                | SleepTarget.EndedByClose _ -> endedByClose task
                | SleepTarget.Waiting (description, fd) ->

                match Map.tryFind description descriptions with
                | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, description) ]
                | Some found ->
                    let connected =
                        match found.Target with
                        | OpenFileTarget.Socket socketId ->
                            match Map.tryFind socketId system.Machine.Sockets with
                            | Some socket -> (SocketPhase.connectionEnd socket.Phase).IsSome
                            | None -> false
                        | OpenFileTarget.File _
                        | OpenFileTarget.Directory _
                        | OpenFileTarget.CharacterDevice _
                        | OpenFileTarget.Pipe _
                        | OpenFileTarget.Kqueue _
                        | OpenFileTarget.Epoll _ -> false

                    if connected then
                        enteredThrough task fd description
                    else
                        [ UnixSystemDefect.ParkedConnectionTransferOnNonConnection (task, description) ]

            system.Tasks
            |> Map.toList
            |> List.collect (fun (task, state) ->
                match state.Parked |> Option.map (fun park -> park.Syscall) with
                | None -> []
                // Names no description.
                | Some ParkedSyscall.SigSuspend -> []
                | Some (ParkedSyscall.Flock parked) ->
                    if Map.containsKey parked.Requester descriptions then
                        []
                    else
                        [ UnixSystemDefect.ParkedOnAbsentDescription (task, parked.Requester) ]
                | Some (ParkedSyscall.EpollWait wait) ->
                    match Map.tryFind wait.Epoll descriptions with
                    | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, wait.Epoll) ]
                    | Some description ->
                        match description.Target with
                        | OpenFileTarget.Epoll _ -> []
                        | OpenFileTarget.Kqueue _
                        | OpenFileTarget.File _
                        | OpenFileTarget.Directory _
                        | OpenFileTarget.Socket _
                        | OpenFileTarget.CharacterDevice _
                        | OpenFileTarget.Pipe _ ->
                            [
                                UnixSystemDefect.ParkedEpollWaitOnNonEpoll (task, wait.Epoll, description.Target)
                            ]
                | Some (ParkedSyscall.Kevent wait) ->
                    let count =
                        if wait.MaxEvents > 0 then
                            []
                        else
                            [ UnixSystemDefect.ParkedKeventCountNotPositive (task, wait.MaxEvents) ]

                    let target =
                        match Map.tryFind wait.Kqueue descriptions with
                        | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, wait.Kqueue) ]
                        | Some description ->
                            match description.Target with
                            | OpenFileTarget.Kqueue state ->
                                match
                                    FileDescriptorRegistry.tryFindId wait.Fd (UnixSystemState.fileDescriptors system)
                                with
                                | Some current when current = wait.Kqueue -> []
                                | _ when state.Drained -> []
                                | current ->
                                    [
                                        UnixSystemDefect.ParkedKeventDescriptorRebound (
                                            task,
                                            wait.Fd,
                                            wait.Kqueue,
                                            current
                                        )
                                    ]
                            | OpenFileTarget.Epoll _
                            | OpenFileTarget.File _
                            | OpenFileTarget.Directory _
                            | OpenFileTarget.CharacterDevice _
                            | OpenFileTarget.Socket _
                            | OpenFileTarget.Pipe _ ->
                                [
                                    UnixSystemDefect.ParkedKeventOnNonKqueue (task, wait.Kqueue, description.Target)
                                ]

                    count @ target
                | Some (ParkedSyscall.Poll parked) ->
                    parked.Entries
                    |> List.collect (fun entry ->
                        match entry with
                        | ParkedPollEntry.Ignored _ -> []
                        | ParkedPollEntry.Watched (fd, watched, _) ->
                            match Map.tryFind watched descriptions with
                            | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, watched) ]
                            | Some description ->
                                let target =
                                    match description.Target with
                                    | OpenFileTarget.Kqueue _
                                    | OpenFileTarget.Epoll _ ->
                                        [ UnixSystemDefect.ParkedPollOnEventQueue (task, watched) ]
                                    | OpenFileTarget.File _
                                    | OpenFileTarget.Directory _
                                    | OpenFileTarget.Socket _
                                    | OpenFileTarget.CharacterDevice _
                                    | OpenFileTarget.Pipe _ -> []

                                let rebound =
                                    match
                                        FileDescriptorRegistry.tryFindId fd (UnixSystemState.fileDescriptors system)
                                    with
                                    | Some current when current = watched -> []
                                    | current ->
                                        [ UnixSystemDefect.ParkedPollDescriptorRebound (task, fd, watched, current) ]

                                target @ rebound
                    )
                | Some (ParkedSyscall.KqueuePoll poll) ->
                    match Map.tryFind poll.Queue system.Machine.PollQueues with
                    | None -> [ UnixSystemDefect.ParkedOnAbsentPollQueue (task, poll.Queue) ]
                    | Some queue when queue.Owner <> system.Process.ProcessId ->
                        [ UnixSystemDefect.PollQueueOfAnotherProcess (task, poll.Queue, queue.Owner) ]
                    | Some queue ->

                    let entries = List.length poll.Entries

                    let socketOf (fd : int) : SocketId option =
                        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
                        | Some (OpenFileTarget.Socket socketId) -> Some socketId
                        | Some _
                        | None -> None

                    let attachments =
                        queue.Registrations
                        |> Map.toList
                        |> List.choose (fun ((fd, _ as key), registration) ->
                            let named = socketOf fd

                            // A closed descriptor is `ParkedKqueuePollRegistrationTarget`'s.
                            let fdOpen =
                                (FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system))
                                    .IsSome

                            if registration.Socket = named || not fdOpen then
                                None
                            else
                                Some (
                                    UnixSystemDefect.ParkedKqueuePollAttachedElsewhere (
                                        task,
                                        key,
                                        registration.Socket,
                                        named
                                    )
                                )
                        )

                    let registrations =
                        queue.Registrations
                        |> Map.toList
                        |> List.collect (fun ((fd, filter as key), registration) ->
                            let target =
                                match
                                    FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system)
                                with
                                | Some (OpenFileTarget.Socket socketId) as target ->
                                    match Map.tryFind socketId system.Machine.Sockets with
                                    | Some socket when DarwinReadiness.modelsSocket socket ->
                                        if
                                            not (List.contains key queue.Active)
                                            && Option.isSome (DarwinReadiness.ofSocket filter socketId system.Machine)
                                        then
                                            [ UnixSystemDefect.ParkedKqueuePollActivationMissed (task, key) ]
                                        else
                                            []
                                    | Some _
                                    | None ->
                                        [ UnixSystemDefect.ParkedKqueuePollRegistrationTarget (task, fd, target) ]
                                | Some (OpenFileTarget.Pipe _) -> []
                                | target -> [ UnixSystemDefect.ParkedKqueuePollRegistrationTarget (task, fd, target) ]

                            let entry =
                                if registration.Entry >= 0 && registration.Entry < entries then
                                    []
                                else
                                    [
                                        UnixSystemDefect.ParkedKqueuePollEntryOutOfRange (
                                            task,
                                            key,
                                            registration.Entry
                                        )
                                    ]

                            target @ entry
                        )

                    let active =
                        queue.Active
                        |> List.indexed
                        |> List.choose (fun (index, (fd, _ as key)) ->
                            let repeated = queue.Active |> List.take index |> List.contains key

                            if
                                repeated
                                || not (Map.containsKey key queue.Registrations)
                                || Option.isNone (socketOf fd)
                            then
                                Some (UnixSystemDefect.ParkedKqueuePollActiveMalformed (task, key))
                            else
                                None
                        )

                    attachments @ registrations @ active
                | Some (ParkedSyscall.Accept accept) ->
                    match accept.Listener with
                    | SleepTarget.EndedByClose _ -> endedByClose task
                    | SleepTarget.Waiting (listener, fd) ->

                    match Map.tryFind listener descriptions with
                    | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, listener) ]
                    | Some description ->
                        let listening =
                            match description.Target with
                            | OpenFileTarget.Socket socketId ->
                                match Map.tryFind socketId system.Machine.Sockets with
                                | Some {
                                           Phase = SocketPhase.Listening listenState
                                       } -> Some listenState.Drained
                                | Some _
                                | None -> None
                            | OpenFileTarget.File _
                            | OpenFileTarget.Directory _
                            | OpenFileTarget.CharacterDevice _
                            | OpenFileTarget.Pipe _
                            | OpenFileTarget.Kqueue _
                            | OpenFileTarget.Epoll _ -> None

                        match listening with
                        | None -> [ UnixSystemDefect.ParkedAcceptOnNonListener (task, listener) ]
                        | Some true -> [ UnixSystemDefect.ParkedAcceptOnDrainedListener (task, listener) ]
                        | Some false -> enteredThrough task fd listener
                | Some (ParkedSyscall.PipeRead read) ->
                    let progress =
                        if read.Count > 0 then
                            []
                        else
                            [ UnixSystemDefect.ParkedPipeTransferProgress (task, read.Count, 0) ]

                    let target =
                        match read.Reader with
                        | SleepTarget.EndedByClose _ -> endedByClose task
                        | SleepTarget.Waiting (reader, fd) ->

                        match Map.tryFind reader descriptions with
                        | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, reader) ]
                        | Some description ->
                            match description.Target with
                            | OpenFileTarget.Pipe (_, PipeEnd.Read) -> enteredThrough task fd reader
                            | target -> [ UnixSystemDefect.ParkedPipeTransferOnWrongTarget (task, reader, target) ]

                    progress @ target
                | Some (ParkedSyscall.PipeWrite write) ->
                    let progress =
                        if write.Written >= 0 && write.Written < write.Count then
                            []
                        else
                            [
                                UnixSystemDefect.ParkedPipeTransferProgress (task, write.Count, write.Written)
                            ]

                    let target =
                        match write.Writer with
                        | SleepTarget.EndedByClose _ -> endedByClose task
                        | SleepTarget.Waiting (writer, fd) ->

                        match Map.tryFind writer descriptions with
                        | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, writer) ]
                        | Some description ->
                            match description.Target with
                            | OpenFileTarget.Pipe (_, PipeEnd.Write) -> enteredThrough task fd writer
                            | target -> [ UnixSystemDefect.ParkedPipeTransferOnWrongTarget (task, writer, target) ]

                    progress @ target
                | Some (ParkedSyscall.ConnectionRead read) ->
                    // Only a Linux `recv` sleeps asking for nothing.
                    let mayAskNothing =
                        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, read.Call with
                        | SimulatedUnixFlavour.Linux, TcpReceiveCall.Receive
                        | SimulatedUnixFlavour.Linux, TcpReceiveCall.Peek -> true
                        | SimulatedUnixFlavour.Linux, TcpReceiveCall.Read
                        | SimulatedUnixFlavour.Darwin, _ -> false

                    let progress =
                        if read.Count > 0 || (read.Count = 0 && mayAskNothing) then
                            []
                        else
                            [ UnixSystemDefect.ParkedConnectionTransferProgress (task, read.Count, 0) ]

                    progress @ connectionTarget task read.Socket
                | Some (ParkedSyscall.ConnectionWrite write) ->
                    let progress =
                        if write.Written >= 0 && write.Written < write.Count then
                            []
                        else
                            [
                                UnixSystemDefect.ParkedConnectionTransferProgress (task, write.Count, write.Written)
                            ]

                    progress @ connectionTarget task write.Socket
            )


        // Every kqueue a descriptor of this process names, or a `kevent` wait of
        // one of its tasks sleeps on, is one this process owns, since its
        // registrations' descriptor numbers are read in its owner's table.
        let kqueueOwners =
            let ownedElsewhere (kqueue : OpenFileDescriptionId) : ProcessId option =
                match OpenFileTable.tryFind kqueue system.Machine.OpenFiles with
                | Some {
                           Target = OpenFileTarget.Kqueue state
                       } when state.Owner <> system.Process.ProcessId -> Some state.Owner
                | Some _
                | None -> None

            let named =
                FileDescriptorRegistry.fds (UnixSystemState.fileDescriptors system)
                |> Map.toList
                |> List.choose (fun (fd, kqueue) ->
                    ownedElsewhere kqueue
                    |> Option.map (fun owner -> UnixSystemDefect.KqueueOfAnotherProcess (fd, kqueue, owner))
                )

            let waited =
                system.Tasks
                |> Map.toList
                |> List.choose (fun (_, state) ->
                    match state.Parked with
                    | Some {
                               Syscall = ParkedSyscall.Kevent wait
                           } ->
                        ownedElsewhere wait.Kqueue
                        |> Option.map (fun owner ->
                            UnixSystemDefect.KqueueOfAnotherProcess (wait.Fd, wait.Kqueue, owner)
                        )
                    | Some _
                    | None -> None
                )

            List.distinct (named @ waited)


        // The signal state against the task table: every task it names must
        // be one, or the delivery that reads it has nowhere to go. And against
        // the machine's platform: the state checks every signal against
        // the numbering it was built with, so a state built under the wrong
        // one holds signals the kernel would number differently, or not at all.
        let signals =
            let signals = system.Process.Signals

            let numberings =
                let stateNumbering = SignalState.numbering signals

                let platformNumbering =
                    SimulatedUnixPlatform.signalNumbering system.Machine.UnixPlatform

                if stateNumbering = platformNumbering then
                    []
                else
                    [ UnixSystemDefect.SignalNumberingMismatch (stateNumbering, platformNumbering) ]

            let frames =
                SignalState.tasksWithFrames signals
                |> Set.toList
                |> List.filter (fun task -> not (Map.containsKey task system.Tasks))
                |> List.map UnixSystemDefect.HandlerFramesWithoutTask

            let masks =
                SignalState.tasksWithMasks signals
                |> Set.toList
                |> List.filter (fun task -> not (Map.containsKey task system.Tasks))
                |> List.map UnixSystemDefect.MaskWithoutTask

            // A mask to restore exists from a `sigsuspend` until its task
            // returns to user mode, so exactly while the task is parked in the
            // call or has been answered and not yet returned; which of those
            // two is true is the client's, so what is checked is that no task
            // parked in anything else has one, and every task parked in it does.
            let toRestore =
                let restoring = SignalState.tasksWithMasksToRestore signals

                let orphaned =
                    restoring
                    |> Set.toList
                    |> List.filter (fun task -> not (Map.containsKey task system.Tasks))
                    |> List.map UnixSystemDefect.MaskToRestoreWithoutTask

                let parked =
                    system.Tasks
                    |> Map.toList
                    |> List.choose (fun (task, state) ->
                        match UnixTaskState.park state with
                        | Some {
                                   Syscall = ParkedSyscall.SigSuspend
                               } when not (Set.contains task restoring) ->
                            Some (UnixSystemDefect.SigsuspendWithoutMaskToRestore task)
                        | Some {
                                   Syscall = ParkedSyscall.SigSuspend
                               } -> None
                        | Some park when Set.contains task restoring ->
                            Some (UnixSystemDefect.MaskToRestoreOutsideSigsuspend (task, park.Syscall))
                        | Some _
                        | None -> None
                    )

                orphaned @ parked

            let targets =
                SignalState.pending signals
                |> List.choose (fun entry ->
                    match entry.Target with
                    | ValueSome task when not (Map.containsKey task system.Tasks) ->
                        Some (UnixSystemDefect.PendingSignalTargetWithoutTask (task, entry.Signal))
                    | ValueSome _
                    | ValueNone -> None
                )

            numberings @ frames @ masks @ toRestore @ targets

        let supplementaryGroups =
            let count = List.length system.Process.Credentials.SupplementaryGroups

            let limit =
                SimulatedUnixPlatform.supplementaryGroupLimit system.Machine.UnixPlatform

            if count > limit then
                [ UnixSystemDefect.TooManySupplementaryGroups (count, limit) ]
            else
                []

        let umask =
            let platform = system.Machine.UnixPlatform
            let stored = PermissionBits.toInt (SimulatedUnixPlatform.umaskStoredBits platform)

            if PermissionBits.toInt system.Process.Umask &&& ~~~stored <> 0 then
                [ UnixSystemDefect.UmaskNotOfPlatform (system.Process.Umask, platform) ]
            else
                []

        // The process's leader against its tasks: it is one, and on Linux its
        // thread ID is the process ID.
        let leader =
            match Map.tryFind system.Leader system.Tasks with
            | None -> [ UnixSystemDefect.LeaderWithoutTask system.Leader ]
            | Some state ->
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux when
                    OsThreadId.toUInt64 state.OsThreadId
                    <> uint64 (ProcessId.toInt32 system.Process.ProcessId)
                    ->
                    [
                        UnixSystemDefect.LeaderThreadIdNotProcessId (
                            system.Leader,
                            state.OsThreadId,
                            system.Process.ProcessId
                        )
                    ]
                | SimulatedUnixFlavour.Linux
                | SimulatedUnixFlavour.Darwin -> []

        let cpus =
            system.Tasks
            |> Map.toList
            |> List.filter (fun (_, state) -> not (UnixMachineState.hasProcessor state.Cpu system.Machine))
            |> List.map (fun (task, state) ->
                UnixSystemDefect.CpuBeyondMachine (task, state.Cpu, system.Machine.ProcessorCount)
            )

        currentDirectory
        @ beyondBound
        @ closeOnForkUnderLinux
        @ parks
        @ kqueueOwners
        @ signals
        @ supplementaryGroups
        @ umask
        @ leader
        @ cpus


    /// Every way this system's tables disagree with each other: the machine's
    /// clauses, which read facts every process on the machine contributes to,
    /// and this process's view's (`checkViewInvariants`).
    ///
    /// On a machine holding other processes besides (a view a
    /// `SimulatedMachine` focused), only the machine's clauses this one
    /// process can check truthfully are run: those that count or collect
    /// across every process are `SimulatedMachine.checkInvariants`'s, which
    /// sees them all.
    ///
    /// Each table's own rules are elsewhere and are not repeated here:
    /// `FileDescriptorRegistry.checkInvariants` for the descriptor table and the
    /// open file descriptions it names, and
    /// `VirtualFileSystem.checkInvariants` for the filesystem. The latter takes
    /// a `pinned` argument, which is what `pinnedInodes` computes, so a caller
    /// wanting the whole picture pairs this with
    /// `VirtualFileSystem.checkInvariants (ObjectLifetime.pinnedInodes system) system.Machine.FileSystem`.
    ///
    /// A client that holds its own references into these tables owes its own
    /// rules about them on top of these.
    let checkInvariants<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystemDefect<'Task> list
        =
        let others =
            Set.remove system.Process.ProcessId (ProcessIdTable.live system.Machine.ProcessIds)

        machineDefects (Set.isEmpty others) [ system.Process, system.Tasks ] system.Machine
        @ checkViewInvariants system

    /// Logical-processor count a freshly-minted simulated process reports.
    /// One, because only single-processor behaviour has been exercised
    /// end-to-end, and because a fixed default is a prerequisite for
    /// replayability.
    /// A client that wants to exercise multi-processor code paths raises it with
    /// `UnixBootImage.withProcessorCount`.
    [<Literal>]
    let defaultProcessorCount : int = 1

    /// The buffer check a freshly-minted machine on `platform` applies: none up
    /// front where the platform screens nothing, and otherwise a screen at the
    /// commonest `TASK_SIZE_MAX` of the platform's architecture, which is
    /// four-level paging on x86-64 and a 48-bit virtual address on arm64. A client
    /// simulating a machine with a different address-space width sets it with
    /// `UnixBootImage.withUserAddressLimit`.
    let defaultUserBufferCheck (platform : SimulatedUnixPlatform) : UserBufferCheck =
        if SimulatedUnixPlatform.screensUserBufferUpFront platform then
            match SimulatedUnixPlatform.architecture platform with
            | SimulatedUnixArchitecture.X64 ->
                UserBufferCheck.BeforeOperation ObservedUserAddressLimit.X64FourLevelPaging
            | SimulatedUnixArchitecture.Arm64 ->
                UserBufferCheck.BeforeOperation ObservedUserAddressLimit.Arm64FortyEightBit
        else
            UserBufferCheck.AtCopyTime

    /// Unix platform identity a freshly-minted simulated process reports:
    /// Linux/x64. A client chooses another by passing it to `initial`.
    let defaultUnixPlatform : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    /// Current working directory a freshly-minted simulated process reports.
    /// The root, because it is the one directory that exists on every Unix and
    /// needs no name invented for it, and the one directory every filesystem
    /// this library can hold is guaranteed to have. (`init` itself starts at
    /// `/`, so this is not even an unusual cwd for a real process.) It is also
    /// the honest answer for a simulation that declines to read the host's: a
    /// process nobody has told where it is claims nothing beyond the root. A
    /// client that wants a particular directory sets it with
    /// `ProcessLaunch.withCurrentDirectory`.
    let defaultCurrentDirectory : AbsoluteUnixPath = AbsoluteUnixPath.root

    /// Executable path a freshly-minted simulated process reports: none at all.
    ///
    /// This library models no `exec(2)`, so there is no file that started this
    /// process, and the emulated filesystem holds no image of one. `None` is
    /// therefore the only true answer, and it is a *modelled* Unix state rather
    /// than an invention: on both flavours, resolving a live process's
    /// executable with `realpath` fails with ENOENT once that executable has
    /// been unlinked. Measured on both, by having a process unlink its own
    /// executable before first asking for its path.
    ///
    /// A client that wants a particular executable sets it with
    /// `ProcessLaunch.withProcessPath`.
    let defaultProcessPath : AbsoluteUnixPath option = None

    /// The range `bind(2)` draws from when asked for port 0, on a machine of
    /// this flavour that has not configured it.
    ///
    /// A sysctl on both platforms rather than a property of the kernel image,
    /// so a host may set it to anything; but each flavour ships a default, and
    /// a Darwin machine draws from Darwin's. Measured: Linux's
    /// `ip_local_port_range` reads 32768-60999, and Darwin's
    /// `net.inet.ip.portrange.first`/`last` read 49152-65535 (macOS 26,
    /// 2026-09-08).
    let defaultEphemeralPortRange (flavour : SimulatedUnixFlavour) : uint16 * uint16 =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 32768us, 60999us
        | SimulatedUnixFlavour.Darwin -> 49152us, 65535us

    /// The addresses this machine holds, as `bind(2)` decides whether an address
    /// is assignable. Loopback only: this library models no interface a process
    /// could reach, so anything else would be an address no packet could arrive on.
    ///
    /// `127.0.0.0/8` rather than `127.0.0.1/32` because that is what Linux
    /// assigns to `lo`, and the flavours read the list differently — see
    /// `UnixBootImage.withLocalAddresses`.
    let defaultLocalAddresses : uint32 list = [ InternetEndpoint.LoopbackAddress ]

    /// The prefixes Linux's local routing table holds, which it will `bind(2)`
    /// any address inside. Loopback's `127.0.0.0/8` is the one every Linux has,
    /// and is why `127.9.9.9` binds there and not on Darwin.
    let defaultLocalRoutes : Ipv4Prefix list = [ Ipv4Prefix.loopbackNetwork ]

    /// Effective user ID a freshly-minted simulated process runs as.
    ///
    /// Not 0: a process that defaulted to root would silently take the
    /// privileged branch of every check the kernel makes and every check it
    /// makes about itself (`geteuid() == 0`) — the uninteresting one, and not
    /// the one most programs are written for.
    /// Instead the first interactive user each flavour creates: 1000 on the
    /// Ubuntu-shaped Linux, and 501 on macOS (measured, `id -u` of the first
    /// account on a macOS 26 machine, 2026-09-08). A client that wants root says
    /// so with `ProcessLaunch.withCredentials`.
    let defaultUserId (flavour : SimulatedUnixFlavour) : UserId =
        match flavour with
        | SimulatedUnixFlavour.Linux -> UserId.parseOrFail "UnixSystem.defaultUserId" 1000u
        | SimulatedUnixFlavour.Darwin -> UserId.parseOrFail "UnixSystem.defaultUserId" 501u

    /// Effective group ID a freshly-minted simulated process runs as: the
    /// first user's primary group, which is a user-private group numbered as
    /// the user on Linux and `staff` (20) on macOS (measured with the uid).
    let defaultGroupId (flavour : SimulatedUnixFlavour) : GroupId =
        match flavour with
        | SimulatedUnixFlavour.Linux -> GroupId.parseOrFail "UnixSystem.defaultGroupId" 1000u
        | SimulatedUnixFlavour.Darwin -> GroupId.parseOrFail "UnixSystem.defaultGroupId" 20u

    /// Credentials a freshly-minted simulated process runs with: real,
    /// effective and saved IDs all `defaultUserId` and `defaultGroupId`, and no
    /// supplementary groups.
    let defaultCredentials (flavour : SimulatedUnixFlavour) : Credentials =
        Credentials.ofIds (defaultUserId flavour) (defaultGroupId flavour) []

    /// File-mode creation mask a freshly-minted simulated process reports.
    /// 0o022 because that is what essentially every Unix login shell and service
    /// manager sets, and because it is the mask the existing seed defaults were
    /// written against (`SeedEntry.defaultPermsForRegularFile` is 0o666 with
    /// these bits cleared). Measured as the mask a process inherits on both
    /// flavours. A client chooses otherwise with `ProcessLaunch.withUmask`, and
    /// the process itself with `umask`.
    let defaultUmask : PermissionBits =
        PermissionBits.parseOrFail "UnixSystem.defaultUmask" 0o022

    /// Whether a freshly-minted simulated process writes a core dump when a
    /// signal kills it: never, as under an `RLIMIT_CORE` of 0. A dump is a
    /// file the simulated process would leave behind, which a client has to
    /// ask for. A client chooses otherwise with
    /// `ProcessLaunch.withCoreDumps`.
    let defaultCoreDumps : CoreDumps = CoreDumps.Suppressed

    /// Process ID a freshly-minted simulated process reports: 4242.
    ///
    /// Not 1, which is the ID of a PID namespace's init process. A kernel
    /// treats init specially — a signal it has not installed a handler for is
    /// not delivered to it from inside its own namespace, so `kill -9` on
    /// itself does nothing — and a default that took that branch would be
    /// modelling a container's entry point rather than an ordinary process.
    /// A client chooses otherwise with `withProcessId`.
    let defaultProcessId : ProcessId =
        // Measured on Linux 6.18.5 in a container: `sh` running as pid 1
        // survives both `kill -9 $$` and `kill -TERM $$`, where the same
        // shell as pid 2 dies with status 137. Otherwise the number is
        // arbitrary; it differs from every other default ID here so that a
        // caller reading the wrong one fails a test that uses the defaults.
        ProcessId.parseOrFail "UnixSystem.defaultProcessId" 4242

    /// Seed for the entropy pool a freshly-minted machine boots with: the
    /// first 64 bits of the fractional part of pi. Any value would do; a
    /// constant nobody chose for its bits is the least arbitrary one.
    ///
    /// A client whose recorded runs must replay bit-for-bit depends on this
    /// value, because every byte the pool hands out follows from it. A client
    /// chooses another with `UnixBootImage.withEntropySeed`.
    let defaultEntropySeed : uint64 = 0x243F6A8885A308D3UL

    /// The `pid_max` a freshly-minted Linux machine has: 4194304, the most Linux
    /// allows, so that thread IDs are reused only after that many have been
    /// handed out. The administrator chooses otherwise with `writePidMaxSysctl`.
    ///
    /// A Darwin machine has no such setting.
    let defaultPidMax : int32 =
        // Measured in Apple's `container` VM (Linux 6.18.5, 2026-09-26), where
        // `kernel.pid_max` reads 4194304; `pid-allocation.c` measured that this is
        // the greatest value the sysctl accepts.
        ThreadIdAllocator.linuxPidMaxCeiling

    /// The launch table of a process started with each of its standard streams
    /// redirected to a pipe of its own, as a shell's `cmd < a | b 2> c` and
    /// most test harnesses do: descriptor 0 reads a pipe whose writer supplied
    /// nothing, and descriptors 1 and 2 write to pipes the client drains. A
    /// client supplying bytes on standard input replaces entry 0 with
    /// `LaunchDescriptor.Supplied` of them, and one that closed the reader of
    /// an output stream before the process started replaces its entry with
    /// `LaunchDescriptor.Gone`.
    ///
    /// Not the only shape a real process can inherit, and not the terminal one.
    /// Under a tty, descriptors 0, 1 and 2 are `dup`s of a single `O_RDWR`
    /// description: measured via `forkpty`, setting `O_NONBLOCK` through
    /// descriptor 1 becomes visible on 0 and 2, and `write(0, _, _)` succeeds.
    let pipedStandardStreams : Map<int, LaunchDescriptor> =
        Map.ofList
            [
                0, LaunchDescriptor.Supplied ImmutableArray.Empty
                1, LaunchDescriptor.Drained
                2, LaunchDescriptor.Drained
            ]

    /// The boot image of a machine of the given platform: no processes, no
    /// sockets, no connections, and an empty filesystem. Configure it with the
    /// setters in `UnixBootImage`, then `UnixBootImage.boot` it, which launches
    /// its first process, for its first syscall.
    ///
    /// The first process's ID is `defaultProcessId`, and so is its leader's
    /// thread ID: on Linux because it always is, and on Darwin as the start of a
    /// quiet machine's counter. A client moves them with
    /// `UnixBootImage.withProcessId` and `UnixBootImage.withLeaderThreadId`.
    ///
    /// The fields the platform *fixes* are derived from it rather than
    /// taken as arguments — `SoMaxConn`, the TCP buffer sysctls, `Mount`, and the platform
    /// itself — because a machine whose flavour and those disagree is one no
    /// real system could be: `EmulatedFileSystemType.isReportableUnder` says
    /// outright that a Darwin kernel never reports tmpfs. Building the record
    /// by hand is what lets that state exist, so the constructor is also the
    /// rule.
    ///
    /// The ephemeral port range is the flavour's shipped default too, though a
    /// host may set it: it is what a default machine of that flavour reports,
    /// not a fact of its kernel image. The buffer check is the platform's
    /// default too, though its limit is a property of the machine's paging
    /// depth rather than of its kernel. All of these are configuration a
    /// caller overrides with the setters in `UnixBootImage`, which is also how
    /// a caller supplies a non-empty filesystem or a different address list.
    let initial<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (platform : SimulatedUnixPlatform)
        : UnixBootImage<'Task, 'Handler>
        =
        // `SimulatedUnixPlatform.create` validates at construction, so a value
        // of the type is already a platform some Unix could be; this catches
        // the one value that bypasses that, the forged `Unchecked.defaultof`,
        // whose null release would otherwise reach a process as its `uname -r`.
        let platform = SimulatedUnixPlatform.assertValid "UnixSystem.initial" platform
        let flavour = SimulatedUnixPlatform.flavour platform

        let deviceMount = DeviceFileSystemMount.defaultFor flavour

        let filesystem =
            let bootTime = UnixTimestamp.ofMillisecondsSinceEpoch 0L

            match
                VirtualFileSystem.empty bootTime (InodeOwner.ofProcess (defaultCredentials flavour))
                |> mountDeviceFileSystem deviceMount bootTime
            with
            | Ok filesystem -> filesystem
            | Error fault ->
                failwith
                    $"UnixSystem.initial: mounting the device filesystem over an empty root failed with %A{fault} (this is a bug in this library)."

        let threadIds, processIds =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                ThreadIdAllocator.startLinux "UnixSystem.initial" defaultPidMax defaultProcessId
                |> snd,
                ProcessIdTable.linux
            | SimulatedUnixFlavour.Darwin ->
                ThreadIdAllocator.startDarwin "UnixSystem.initial" (uint64 (ProcessId.toInt32 defaultProcessId))
                |> snd,
                ProcessIdTable.darwinAfter defaultProcessId

        {
            Machine =
                {
                    OpenFiles = OpenFileTable.empty
                    Sockets = Map.empty
                    Pipes = Map.empty
                    NextPipeId = PipeId 0L
                    Delivered = DeliveryLog.empty
                    // Any start would do; one, because no filesystem hands out
                    // inode 0.
                    NextPipeInode = InodeNumber 1L
                    PipeDevice = UnixMachineState.defaultPipeDevice flavour
                    Connections = Map.empty
                    NextConnectionId = ConnectionId 0L
                    NextEventRegistrationOrdinal = 0L
                    NextParkOrdinal = ParkOrdinal 0L
                    PollQueues = Map.empty
                    NextPollQueueId = PollQueueId 0L
                    ThreadIds = threadIds
                    ProcessIds = processIds
                    CurrentDirectories = Map.empty
                    NextSocketId = SocketId 0L
                    NextEphemeralPort = fst (defaultEphemeralPortRange flavour)
                    EphemeralPortRange = defaultEphemeralPortRange flavour
                    SoMaxConn = UnixMachineState.defaultSoMaxConn flavour
                    TcpSendSpace = UnixMachineState.defaultTcpSendSpace flavour
                    Ipv6OnlyByDefault = false
                    TcpReceiveSpace = UnixMachineState.defaultTcpReceiveSpace flavour
                    TcpSendSpaceMax = UnixMachineState.defaultTcpSendSpaceMax flavour
                    LocalAddresses = defaultLocalAddresses
                    LocalRoutes = defaultLocalRoutes
                    NanosecondsSinceBoot = 0L
                    BootTime = UnixTimestamp.epoch
                    EntropyPool = EntropyPool.ofSeed defaultEntropySeed
                    ProcessorCount = defaultProcessorCount
                    Occupants = Map.empty
                    UserBufferCheck = defaultUserBufferCheck platform
                    UnixPlatform = platform
                    FileSystem = filesystem
                    Mount = EmulatedMount.defaultFor flavour
                    DeviceMount = deviceMount
                    ProtectedFiles = ProtectedFiles.off
                }
            ProcessId = defaultProcessId
        }

    /// The machine's administrator writes Linux's `kernel.pid_max` sysctl
    /// (through `/proc/sys`), which a sysctl allows at any time, the machine
    /// running or not: the thread IDs handed out from then on are below it, and
    /// once they reach it they start again from 300, skipping those still in use.
    ///
    /// Any value from 301 to 4194304 is accepted, including one at or below a live
    /// task's thread ID: as on Linux, that task keeps its ID, and the next thread
    /// takes its ID from 300 up.
    ///
    /// Not configuration, which is `UnixBootImage`'s, but the outside world
    /// acting on a running machine, as `UnixSystem.advanceClock` is.
    ///
    /// Throws for a Darwin machine, which has no such setting, and for a value
    /// outside 301 to 4194304 (`ThreadIdAllocator.linuxPidMaxFloor` to
    /// `ThreadIdAllocator.linuxPidMaxCeiling`), which Linux's sysctl answers
    /// with EINVAL.
    let writePidMaxSysctl<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (pidMax : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        // Measured on Linux 6.18.5 aarch64 by
        // `docs/plans/2026-08-23-posix-kernel-extraction/pid-max-below-live.c`:
        // with process 5000 and its thread 5001 alive, writes of 1000, 5000, 5001,
        // 5002 and 400 each take; both keep their IDs, `kill` and `tgkill` still
        // find them, and the threads started after each write get 300, 301, and
        // on up.
        let threadIds = ThreadIdAllocator.withPidMax context pidMax system.Machine.ThreadIds

        { system with
            Machine =
                { system.Machine with
                    ThreadIds = threadIds
                }
        }
