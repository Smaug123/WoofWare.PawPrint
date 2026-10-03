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
    | LSeek of fd : int * offset : int64 * whence : int
    /// `operation` is raw: which combinations of LOCK_SH/LOCK_EX/LOCK_UN/LOCK_NB
    /// are legal, and what an illegal one earns, is behaviour this kernel models
    /// and models per flavour.
    | FLock of fd : int * operation : int
    | FTruncate of fd : int * length : int64
    | Close of fd : int
    /// `mode` is raw, as `mkdir(2)` takes it: how it combines with the umask
    /// and with the parent's set-group-ID bit is behaviour this kernel models,
    /// and models per flavour.
    | MkDir of path : PathArgumentBytes * mode : int
    | Unlink of path : PathArgumentBytes
    | RmDir of path : PathArgumentBytes
    | ChDir of path : PathArgumentBytes
    /// `mode` is raw, as `chmod(2)` takes it: which of its bits the inode gets
    /// is behaviour this kernel models.
    | ChMod of path : PathArgumentBytes * mode : int
    /// `mode` is raw, as `fchmod(2)` takes it.
    | FChMod of fd : int * mode : int
    /// `None` is `(uid_t)-1` or `(gid_t)-1`: leave that ID as it is.
    | ChOwn of path : PathArgumentBytes * user : UserId option * group : GroupId option
    /// As `ChOwn`, without following a symbolic link in the final position.
    | LChOwn of path : PathArgumentBytes * user : UserId option * group : GroupId option
    /// As `ChOwn`, of the inode `fd` names.
    | FChOwn of fd : int * user : UserId option * group : GroupId option
    /// `mask` is raw, as `umask(2)` takes it: which of its bits the process
    /// keeps is behaviour this kernel models, and models per flavour. Answers
    /// the previous mask.
    | UMask of mask : int
    /// `path` is the argument's bytes, which this kernel copies in at the
    /// point it measured; `mode` is raw, as `access(2)` takes it, because
    /// which of its bits are rejected is behaviour this kernel models per
    /// flavour.
    | Access of path : PathArgumentBytes * mode : int
    /// `dirfd`, `mode` and `flags` are raw, as `faccessat(2)` takes them: each
    /// flavour numbers `AT_FDCWD` and the flags its own way.
    | FAccessAt of dirfd : int * path : PathArgumentBytes * mode : int * flags : int
    /// `futimens(2)` with two explicit times; see `UnixPathResolution.futimens`.
    | FUTimens of fd : int * access : UnixTimestamp * modification : UnixTimestamp
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
    | LSeek of LSeekRefusal
    | FLock of FLockRefusal
    | FTruncate of TruncationRefusal
    | MkDir of PathRefusal
    | Unlink of RemovalRefusal
    | RmDir of RemovalRefusal
    | ChDir of PathRefusal
    | ChMod of ChModRefusal
    | FChMod of FChModRefusal
    /// `chown(2)` and `lchown(2)` alike.
    | ChOwn of ChOwnRefusal
    | FChOwn of FChOwnRefusal
    | Access of AccessRefusal
    | Close of CloseRefusal<'Task>
    | FUTimens of FUTimensRefusal
    | CopyFileRange of CopyFileRangeRefusal
    | FileClone of FileCloneRefusal
    | CloneFile of CloneFileRefusal
    /// `setresuid(2)` and `setresgid(2)` alike.
    | SetIds of SetIdsRefusal
    | SetGroups of SetGroupsRefusal

/// A way this system's tables disagree with each other — a state no kernel
/// could be in, and which the operations here exist to keep unreachable.
/// `UnixSystem.checkInvariants` returns these.
///
/// Separate from `FileDescriptorRegistryDefect` and `VirtualFileSystemDefect`
/// because every case here is a claim about *two* tables at once, and neither
/// of those modules can see the other's: each is defined in a file that
/// compiles before this one.
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
    /// connection awaiting accept is a `TcpConnection`, not a socket, which
    /// is what lets this rule stay strict.
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
    /// rule, so a `VirtualFileSystem.forget` that fires too late is caught there
    /// and one that fires too early is caught here.
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
    /// A socket's phase is one its kind cannot enter: a datagram socket
    /// listening or holding a stream connection, or a non-datagram socket
    /// holding a datagram peer.
    | SocketPhaseKindMismatch of socket : SocketId * kind : SocketKind * phase : SocketPhase
    /// A connection in the table has an identity at or above the next one to
    /// allocate, so a future connect would mint a duplicate.
    | NextConnectionIdNotFresh of nextConnectionId : ConnectionId * existing : ConnectionId
    /// A socket event registration, an epoll instance's or a kqueue's, records
    /// an ADD ordinal at or above the
    /// next one to mint, so some future ADD would repeat it — and the
    /// ordinal's whole job is to order same-signal ties, which a repeat
    /// leaves unspecified.
    | SocketEventRegistrationOrdinalNotFresh of next : int64 * port : OpenFileDescriptionId * registeredAt : int64
    /// Two socket event registrations record the same ADD ordinal. Ordinals
    /// are minted from one monotonic counter, so a duplicate means two ADDs
    /// were stamped with one mint — and a same-signal tie between the pair
    /// would have no measured order.
    | DuplicateSocketEventRegistrationOrdinal of registeredAt : int64
    /// A task is parked on an open file description the table does not hold,
    /// so its wait can never be satisfied, and asking `WakeCondition.satisfied`
    /// about it crashes. A park holds what it names until the call returns, so
    /// this is a park recorded without the description, or one destroyed
    /// without the park being consulted.
    | ParkedOnAbsentDescription of task : 'Task * description : OpenFileDescriptionId
    /// An open file description survives that no descriptor names and no
    /// syscall in flight holds (`ParkedSyscall.descriptions`). A real kernel
    /// frees a file when its last reference goes, so this is a leak: a close,
    /// or the return of a call that held it, failed to release it.
    | UnreferencedDescription of description : OpenFileDescriptionId
    /// A task is parked in an `epoll_wait` on a description that is not an
    /// epoll instance, which no wait could have produced and which
    /// `SocketEventPort.hasDeliverableEvent` crashes on.
    | ParkedSocketWaitOnNonPort of task : 'Task * description : OpenFileDescriptionId * target : OpenFileTarget
    /// A task is parked in a `kevent` on a description that is not a kqueue,
    /// which no wait could have produced and on which `WakeCondition.satisfied`
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
    /// An open file description names an object this flavour's kernel does
    /// not have: an epoll instance or a device under Darwin, or a kqueue under
    /// Linux.
    | DescriptionNotOfFlavour of
        description : OpenFileDescriptionId *
        target : OpenFileTarget *
        flavour : SimulatedUnixFlavour
    /// A task is parked in a `poll` watching a socket event port, which `poll`
    /// refuses before it parks and whose readiness is not modelled.
    | ParkedPollOnSocketEventPort of task : 'Task * description : OpenFileDescriptionId
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
    /// socket's filter (see `ParkedKqueuePoll.Active`).
    | ParkedKqueuePollActiveMalformed of task : 'Task * key : (int * KqueueFilter)
    /// A task's parked Darwin `poll` registers the socket filter `key`, which is
    /// ready, and does not list it as activated: whatever made it ready did not
    /// activate it, so the call would sleep through what a real one wakes for.
    | ParkedKqueuePollActivationMissed of task : 'Task * key : (int * KqueueFilter)
    /// A task is parked in an `accept` on a description that is not a listening
    /// socket, which no accept could have produced and on which
    /// `WakeCondition.satisfied` crashes.
    | ParkedAcceptOnNonListener of task : 'Task * description : OpenFileDescriptionId
    /// A task is asleep in an `accept` on a listener a close has drained
    /// (`ListenState.Drained`): the close that drains a listener ends every
    /// accept asleep on it, and `accept` refuses to sleep on one.
    | ParkedAcceptOnDrainedListener of task : 'Task * description : OpenFileDescriptionId
    /// Under Darwin, a task is asleep in an `accept` or a pipe transfer made
    /// through `fd`, which no longer names the description the call sleeps on
    /// (`current` is what it names now). Closing that descriptor ends the call,
    /// so this is a park recorded without the syscall or a descriptor closed
    /// around it.
    | ParkedCallDescriptorRebound of
        task : 'Task *
        fd : int *
        description : OpenFileDescriptionId *
        current : OpenFileDescriptionId option
    /// Under Linux, a task's `accept` or pipe transfer records that a close has
    /// ended it (`SleepTarget.EndedByClose`), which only Darwin's close does.
    | ParkedCallEndedByCloseUnderLinux of task : 'Task
    /// Under Linux, a listener records that a close has drained it
    /// (`ListenState.Drained`), which only Darwin's close does.
    | ListenerDrainedUnderLinux of socket : SocketId
    /// A task is asleep in a pipe `read` or `write` through a description that
    /// names something other than the pipe end the call needs, which no such
    /// call could have produced and on which `WakeCondition.satisfied` crashes.
    | ParkedPipeTransferOnWrongTarget of task : 'Task * description : OpenFileDescriptionId * target : OpenFileTarget
    /// A task is asleep in a pipe transfer whose progress no call could have
    /// made: a read of nothing, or a write of `count` bytes with `written` of
    /// them in, where a sleeping write has put in at least none and fewer than
    /// all.
    | ParkedPipeTransferProgress of task : 'Task * count : int * written : int
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
    /// A bound socket holds port 0, which is how a process *asks* for a port and
    /// never one it is given, with one exception: a datagram socket whose
    /// Linux `connect(AF_UNSPEC)` kept a locked concrete address and dropped
    /// an unlocked port is half-bound at `address:0`, which is measured and
    /// is what a later `bind` or `connect` completes.
    | BoundToPortZero of socket : SocketId
    /// A task the table does not hold has handler frames.
    | HandlerFramesWithoutTask of task : 'Task
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
    /// at or above `pid_max` on Linux, or not yet reached on Darwin. A later
    /// thread could be given the same ID.
    | OsThreadIdNotMintable of task : 'Task * id : OsThreadId * allocator : ThreadIdAllocator
    /// The machine's thread ID counter is not its flavour's: a Linux counter on a
    /// Darwin machine, or the other way about.
    | ThreadIdAllocatorNotOfFlavour of flavour : SimulatedUnixFlavour * allocator : ThreadIdAllocator
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

/// Why the directory a host named cannot be the one a simulated process starts
/// in. `UnixBootImage.withFileSystemAndCurrentDirectory` returns one instead of
/// deciding what to say about it: the remedy is always "fix the knob you set
/// this from", and only the caller knows what that knob is called.
///
/// Every case is a host mistake rather than a process's, which is why none of
/// them is a `UnixError`: there is no errno for "you seeded a filesystem that
/// does not contain the directory you asked to start in", and answering ENOENT
/// would blame a process's path when there is no process yet.
///
/// Three cases, and deliberately not five. The walk can also answer an inode
/// the filesystem does not contain, or a directory it holds no path to — but
/// not for a filesystem `toVirtualFileSystem` has just built and asserted the
/// invariants of, so those are bugs in this library and crash here rather than
/// being handed to a caller who could do nothing about them.
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
    /// need `PathWalk.resolveExisting` to report which limit it hit,
    /// which every other caller of that walk would pay for.
    ///
    /// Carries the flavour so that a fault which outlives the call still says
    /// whose limits were in force -- 255 CJK characters name a directory a
    /// Darwin process can start in and a Linux one cannot, and only Darwin
    /// re-checks a splice at all.
    | TooLong of SimulatedUnixFlavour
    /// The path resolves, to something that is not a directory.
    | NotADirectory
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
    /// This kernel will not resolve the path.
    | Path of PathRefusal

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
    /// leader. See `UnixSystem.Leader`.
    let leader<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : 'Task
        =
        system.Leader

    /// Every task the process has, and what each is blocked in, which
    /// `UnixTaskTable`'s queries read.
    let tasks<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Map<'Task, UnixTaskState>
        =
        system.Tasks

    /// Every write that has reached a client draining one of the machine's
    /// pipes, oldest first. See `UnixMachineState.delivered`.
    let delivered<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : DeliveryLog
        =
        UnixMachineState.delivered system.Machine

    /// The process's signal state, which `SignalState`'s queries read. See
    /// `UnixProcessState.signals`.
    let signals<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        UnixProcessState.signals system.Process

    /// The environment the process was started with, entry by entry, in order.
    /// See `UnixProcessState.environment`.
    let environment<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixByteString list
        =
        UnixProcessState.environment system.Process

    /// The path of the executable that started the process, or `None` if it has
    /// none. See `UnixProcessState.processPath`.
    let processPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : AbsoluteUnixPath option
        =
        UnixProcessState.processPath system.Process

    /// The platform the machine impersonates. See `UnixMachineState.platform`.
    let platform<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : SimulatedUnixPlatform
        =
        UnixMachineState.platform system.Machine

    /// The number of logical processors the machine reports to the process. See
    /// `UnixMachineState.processorCount`.
    let processorCount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : int
        =
        UnixMachineState.processorCount system.Machine

    /// How long the machine has been up, to the nanosecond. See
    /// `UnixMachineState.nanosecondsSinceBoot`.
    let nanosecondsSinceBoot<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : int64
        =
        UnixMachineState.nanosecondsSinceBoot system.Machine

    /// Let `nanoseconds` pass on the machine: every clock it has moves forward
    /// by that much. See `UnixMachineState.advanceClock`, which says what it
    /// refuses.
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
        system.Process.FileDescriptors

    /// The machine's filesystem, which `VirtualFileSystem`'s queries read.
    let fileSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : VirtualFileSystem
        =
        system.Machine.FileSystem

    /// The inode of the directory the process is standing in. See
    /// `UnixProcessState.CurrentDirectoryInode`; the path `getcwd(3)` reports is
    /// `UnixPathResolution.currentDirectoryPath`.
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

    /// The machine's `somaxconn` sysctl. See `UnixMachineState.SoMaxConn`.
    let soMaxConn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : int
        =
        system.Machine.SoMaxConn

    /// The mount the machine's root filesystem claims to be. See
    /// `UnixMachineState.Mount`.
    let mount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : EmulatedMount
        =
        system.Machine.Mount

    /// The machine's `fs.protected_*` sysctls. See
    /// `UnixMachineState.ProtectedFiles`.
    let protectedFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : ProtectedFiles
        =
        system.Machine.ProtectedFiles

    /// Every pipe with an end open, by identity. See `UnixMachineState.Pipes`.
    let pipes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : Map<PipeId, PipeState>
        =
        system.Machine.Pipes

    /// The realtime clock's reading, to the nanosecond. See
    /// `UnixMachineState.realtime`.
    let realtime<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixTimestamp
        =
        UnixMachineState.realtime system.Machine

    /// Whether, and where, the machine's kernel screens a read or write buffer
    /// before performing the operation. See `UnixMachineState.userBufferCheck`.
    let userBufferCheck<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UserBufferCheck
        =
        UnixMachineState.userBufferCheck system.Machine

    /// The socket `socketId` names. Loudly partial, as
    /// `UnixMachineState.socket` is.
    let socket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (system : UnixSystem<'Task, 'Handler>)
        : SocketDescription
        =
        UnixMachineState.socket socketId system.Machine

    /// The pipe `pipeId` names. Loudly partial, as `UnixMachineState.pipe` is.
    let pipe<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (system : UnixSystem<'Task, 'Handler>)
        : PipeState
        =
        UnixMachineState.pipe pipeId system.Machine

    /// How ready the socket `socketId` is. See
    /// `UnixMachineState.socketReadinessLevel`.
    let socketReadinessLevel<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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
        FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors

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
        | Syscall.Dup fd -> Ok (UnixDescriptor.dup fd system) |> answered
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
        | Syscall.MkDir (path, mode) ->
            UnixNamespace.mkdir path mode system
            |> answered
            |> Result.mapError SyscallRefusal.MkDir
        | Syscall.Unlink path ->
            UnixNamespace.unlink path system
            |> answered
            |> Result.mapError SyscallRefusal.Unlink
        | Syscall.RmDir path ->
            UnixNamespace.rmdir path system
            |> answered
            |> Result.mapError SyscallRefusal.RmDir
        | Syscall.ChDir path ->
            UnixPathResolution.chdir path system
            |> answered
            |> Result.mapError SyscallRefusal.ChDir
        | Syscall.ChMod (path, mode) ->
            UnixPathResolution.chmod path mode system
            |> answered
            |> Result.mapError SyscallRefusal.ChMod
        | Syscall.FChMod (fd, mode) ->
            UnixPathResolution.fchmod fd mode system
            |> answered
            |> Result.mapError SyscallRefusal.FChMod
        | Syscall.ChOwn (path, user, group) ->
            UnixPathResolution.chown path user group system
            |> answered
            |> Result.mapError SyscallRefusal.ChOwn
        | Syscall.LChOwn (path, user, group) ->
            UnixPathResolution.lchown path user group system
            |> answered
            |> Result.mapError SyscallRefusal.ChOwn
        | Syscall.FChOwn (fd, user, group) ->
            UnixPathResolution.fchown fd user group system
            |> answered
            |> Result.mapError SyscallRefusal.FChOwn
        | Syscall.UMask mask ->
            let previous, system = umask mask system

            Ok (SyscallOutcome.Answered (SyscallAnswer.Completed (int64 (PermissionBits.toInt previous))), system)
        | Syscall.Access (path, mode) ->
            UnixPathResolution.access path mode system
            |> Result.map (fun answer -> SyscallOutcome.Answered answer, system)
            |> Result.mapError SyscallRefusal.Access
        | Syscall.FAccessAt (dirfd, path, mode, flags) ->
            UnixPathResolution.faccessat dirfd path mode flags system
            |> Result.map (fun answer -> SyscallOutcome.Answered answer, system)
            |> Result.mapError SyscallRefusal.Access
        | Syscall.FUTimens (fd, access, modification) ->
            UnixPathResolution.futimens fd access modification system
            |> answered
            |> Result.mapError SyscallRefusal.FUTimens
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

    /// Every way this system's tables disagree with each other: the socket table
    /// and the pipe table against the descriptor table, each pipe and the pipe
    /// device against the platform, the connection table against the sockets
    /// that reference it, the descriptor table against the filesystem, the
    /// current directory against both, each task's park against the descriptor
    /// table and each description against the descriptors and parks that
    /// reference it, the signal state against the task table, and the machine's
    /// filesystem type and buffer check and the process's supplementary groups
    /// and file-mode creation mask against its platform.
    ///
    /// Each table's own rules are elsewhere and are not repeated here:
    /// `FileDescriptorRegistry.checkInvariants` for the descriptor table, and
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
        let named =
            FileDescriptorRegistry.descriptions system.Process.FileDescriptors
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
            |> List.filter (fun (_, socketId) -> not (Map.containsKey socketId system.Machine.Sockets))
            |> List.map UnixSystemDefect.DanglingSocket

        let namedIds = named |> List.map snd |> Set.ofList

        let unreferenced =
            system.Machine.Sockets
            |> Map.toList
            |> List.map fst
            |> List.filter (fun socketId -> not (Set.contains socketId namedIds))
            |> List.map UnixSystemDefect.UnreferencedSocket

        // Against the table rather than against the descriptions: the table is
        // where a socket lives, so it is the table that must stay below the
        // counter even once a socket can outlive every descriptor of it.
        let freshness =
            system.Machine.Sockets
            |> Map.toList
            |> List.map fst
            |> List.filter (fun socketId -> socketId >= system.Machine.NextSocketId)
            |> List.map (fun socketId -> UnixSystemDefect.NextSocketIdNotFresh (system.Machine.NextSocketId, socketId))

        let foreignObjects =
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

            FileDescriptorRegistry.descriptions system.Process.FileDescriptors
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
            system.Process.FileDescriptors
            |> FileDescriptorRegistry.descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _) ->
                    match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.Directory _)
                    | Some (InodeContent.CharacterDevice _) ->
                        Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.Symlink _) -> None
                | OpenFileTarget.Directory (inode, _) ->
                    match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.Directory _) -> None
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.CharacterDevice _)
                    | Some (InodeContent.Symlink _) -> Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                | OpenFileTarget.CharacterDevice (inode, device) ->
                    match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
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

        // Every reference any socket makes to a connection, with whether it
        // came through an accept queue (which has its own defect case and its
        // own no-duplicates rule).
        let connectionReferences =
            system.Machine.Sockets
            |> Map.toList
            |> List.collect (fun (socketId, socket) ->
                match socket.Phase with
                | SocketPhase.Established connection
                | SocketPhase.EstablishedPendingReport connection -> [ socketId, connection, false ]
                | SocketPhase.Listening listenState ->
                    listenState.Queue |> List.map (fun connection -> socketId, connection, true)
                | SocketPhase.Idle
                | SocketPhase.Refused _
                | SocketPhase.DatagramPeer _ -> []
            )

        let danglingConnections =
            connectionReferences
            |> List.filter (fun (_, connection, _) -> not (Map.containsKey connection system.Machine.Connections))
            |> List.map (fun (socketId, connection, queued) ->
                if queued then
                    UnixSystemDefect.DanglingQueuedConnection (socketId, connection)
                else
                    UnixSystemDefect.DanglingConnection (socketId, connection)
            )

        let referencedConnections =
            connectionReferences
            |> List.map (fun (_, connection, _) -> connection)
            |> Set.ofList

        let orphanConnections =
            system.Machine.Connections
            |> Map.toList
            |> List.map fst
            |> List.filter (fun connection -> not (Set.contains connection referencedConnections))
            |> List.map UnixSystemDefect.OrphanConnection

        let duplicateQueued =
            connectionReferences
            |> List.choose (fun (_, connection, queued) -> if queued then Some connection else None)
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (connection, _) -> UnixSystemDefect.DuplicateQueuedConnection connection)

        let phaseKindMismatches =
            system.Machine.Sockets
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
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin -> []
            | SimulatedUnixFlavour.Linux ->
                system.Machine.Sockets
                |> Map.toList
                |> List.choose (fun (socketId, socket) ->
                    match socket.Phase with
                    | SocketPhase.Listening {
                                                Drained = true
                                            } -> Some (UnixSystemDefect.ListenerDrainedUnderLinux socketId)
                    | _ -> None
                )

        let connectionFreshness =
            system.Machine.Connections
            |> Map.toList
            |> List.map fst
            |> List.filter (fun connection -> connection >= system.Machine.NextConnectionId)
            |> List.map (fun connection ->
                UnixSystemDefect.NextConnectionIdNotFresh (system.Machine.NextConnectionId, connection)
            )

        let registrationOrdinals =
            system.Process.FileDescriptors
            |> FileDescriptorRegistry.descriptions
            |> Map.toList
            |> List.collect (fun (portId, description) ->
                match description.Target with
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _
                | OpenFileTarget.CharacterDevice _
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.Kqueue state ->
                    state.Registrations
                    |> Map.toList
                    |> List.map (fun (_, registration) -> portId, registration.RegisteredAt)
                | OpenFileTarget.Epoll portState ->
                    portState.Registrations
                    |> Map.toList
                    |> List.map (fun (_, registration) -> portId, registration.RegisteredAt)
            )

        let ordinalFreshness =
            registrationOrdinals
            |> List.filter (fun (_, registeredAt) -> registeredAt >= system.Machine.NextSocketEventRegistrationOrdinal)
            |> List.map (fun (portId, registeredAt) ->
                UnixSystemDefect.SocketEventRegistrationOrdinalNotFresh (
                    system.Machine.NextSocketEventRegistrationOrdinal,
                    portId,
                    registeredAt
                )
            )

        let ordinalDuplicates =
            registrationOrdinals
            |> List.countBy snd
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (registeredAt, _) -> UnixSystemDefect.DuplicateSocketEventRegistrationOrdinal registeredAt)

        // Each task's park against the descriptor table. A park names what the
        // task waits on, and the wake reads the description back; the park
        // holds it until the call returns, so an absent one was parked on
        // without going through the syscall or destroyed around it.
        let parks =
            let descriptions =
                FileDescriptorRegistry.descriptions system.Process.FileDescriptors

            let darwin =
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> false
                | SimulatedUnixFlavour.Darwin -> true

            // Under Darwin a close of the descriptor a sleeping accept or pipe
            // transfer was made through ends the call, so while it sleeps the
            // descriptor still names what it sleeps on. Under Linux the close
            // leaves it asleep, and the number is not consulted.
            let enteredThrough (task : 'Task) (fd : int) (description : OpenFileDescriptionId) =
                match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
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

            system.Tasks
            |> Map.toList
            |> List.collect (fun (task, state) ->
                match state.Parked |> Option.map (fun park -> park.Syscall) with
                | None -> []
                | Some (ParkedSyscall.Flock parked) ->
                    if Map.containsKey parked.Requester descriptions then
                        []
                    else
                        [ UnixSystemDefect.ParkedOnAbsentDescription (task, parked.Requester) ]
                | Some (ParkedSyscall.SocketWait wait) ->
                    match Map.tryFind wait.Port descriptions with
                    | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, wait.Port) ]
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
                                UnixSystemDefect.ParkedSocketWaitOnNonPort (task, wait.Port, description.Target)
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
                                match FileDescriptorRegistry.tryFindId wait.Fd system.Process.FileDescriptors with
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
                                        [ UnixSystemDefect.ParkedPollOnSocketEventPort (task, watched) ]
                                    | OpenFileTarget.File _
                                    | OpenFileTarget.Directory _
                                    | OpenFileTarget.Socket _
                                    | OpenFileTarget.CharacterDevice _
                                    | OpenFileTarget.Pipe _ -> []

                                let rebound =
                                    match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                                    | Some current when current = watched -> []
                                    | current ->
                                        [ UnixSystemDefect.ParkedPollDescriptorRebound (task, fd, watched, current) ]

                                target @ rebound
                    )
                | Some (ParkedSyscall.KqueuePoll poll) ->
                    let entries = List.length poll.Entries

                    let socketOf (fd : int) : SocketId option =
                        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                        | Some (OpenFileTarget.Socket socketId) -> Some socketId
                        | Some _
                        | None -> None

                    let registrations =
                        poll.Registrations
                        |> Map.toList
                        |> List.collect (fun ((fd, filter as key), registration) ->
                            let target =
                                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                                | Some (OpenFileTarget.Socket socketId) as target ->
                                    match Map.tryFind socketId system.Machine.Sockets with
                                    | Some socket when DarwinReadiness.modelsSocket socket ->
                                        if
                                            not (List.contains key poll.Active)
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
                        poll.Active
                        |> List.indexed
                        |> List.choose (fun (index, (fd, _ as key)) ->
                            let repeated = poll.Active |> List.take index |> List.contains key

                            if
                                repeated
                                || not (Map.containsKey key poll.Registrations)
                                || Option.isNone (socketOf fd)
                            then
                                Some (UnixSystemDefect.ParkedKqueuePollActiveMalformed (task, key))
                            else
                                None
                        )

                    registrations @ active
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
            )

        // Every description is referenced: by a descriptor, or by a call in
        // flight that holds it.
        let unreferencedDescriptions =
            let named =
                FileDescriptorRegistry.fds system.Process.FileDescriptors
                |> Map.toSeq
                |> Seq.map snd
                |> Set.ofSeq

            let held = ObjectLifetime.heldByCalls system.Tasks

            FileDescriptorRegistry.descriptions system.Process.FileDescriptors
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> not (Set.contains id named) && not (Set.contains id held))
            |> List.map UnixSystemDefect.UnreferencedDescription

        let parkOrdinals =
            system.Tasks
            |> Map.toList
            |> List.choose (fun (task, state) -> state.Parked |> Option.map (fun park -> task, park.Ordinal))

        let parkOrdinalFreshness =
            parkOrdinals
            |> List.filter (fun (_, ordinal) -> ordinal >= system.Machine.NextParkOrdinal)
            |> List.map (fun (task, ordinal) ->
                UnixSystemDefect.ParkOrdinalNotFresh (system.Machine.NextParkOrdinal, task, ordinal)
            )

        let parkOrdinalDuplicates =
            parkOrdinals
            |> List.countBy snd
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map (fun (ordinal, _) -> UnixSystemDefect.DuplicateParkOrdinal ordinal)

        // Bindings no bind or listen could have produced.
        let bindings =
            system.Machine.Sockets
            |> Map.toList
            |> List.collect (fun (socketId, socket) ->
                let unboundListener =
                    match socket.Phase, socket.Binding with
                    | SocketPhase.Listening _, None -> [ UnixSystemDefect.ListenerWithoutBinding socketId ]
                    | _ -> []

                let portZero =
                    match socket.Binding with
                    | Some binding when binding.Endpoint.Port = 0us ->
                        let halfBound =
                            // Only Linux's `connect(AF_UNSPEC)` produces this,
                            // and it always leaves the socket idle.
                            SimulatedUnixPlatform.flavour system.Machine.UnixPlatform = SimulatedUnixFlavour.Linux
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

                unboundListener @ portZero
            )

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

            let targets =
                SignalState.pending signals
                |> List.choose (fun entry ->
                    match entry.Target with
                    | ValueSome task when not (Map.containsKey task system.Tasks) ->
                        Some (UnixSystemDefect.PendingSignalTargetWithoutTask (task, entry.Signal))
                    | ValueSome _
                    | ValueNone -> None
                )

            numberings @ frames @ targets

        let fileSystemType =
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

            let fsType = EmulatedMount.fileSystemType system.Machine.Mount

            if EmulatedFileSystemType.isReportableUnder flavour fsType then
                []
            else
                [ UnixSystemDefect.FileSystemTypeNotReportable (flavour, fsType) ]

        let protectedFiles =
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

            if UnixMachineState.isProtectedFilesOf flavour system.Machine.ProtectedFiles then
                []
            else
                [
                    UnixSystemDefect.ProtectedFilesNotOfFlavour (system.Machine.ProtectedFiles, flavour)
                ]

        let userBufferCheck =
            if UnixMachineState.isUserBufferCheckOf system.Machine.UnixPlatform system.Machine.UserBufferCheck then
                []
            else
                [
                    UnixSystemDefect.UserBufferCheckNotOfPlatform (
                        system.Machine.UnixPlatform,
                        system.Machine.UserBufferCheck
                    )
                ]

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

        // The task table against the process and the machine's thread ID
        // counter: the leader is a task, no two tasks share an ID, and every ID is
        // one the counter could have handed out, so none can be handed out again
        // while its task lives.
        let threadIds =
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform
            let allocator = system.Machine.ThreadIds

            let allocatorFlavour =
                match flavour, allocator with
                | SimulatedUnixFlavour.Linux, ThreadIdAllocator.Linux _
                | SimulatedUnixFlavour.Darwin, ThreadIdAllocator.Darwin _ -> []
                | SimulatedUnixFlavour.Linux, ThreadIdAllocator.Darwin _
                | SimulatedUnixFlavour.Darwin, ThreadIdAllocator.Linux _ ->
                    [ UnixSystemDefect.ThreadIdAllocatorNotOfFlavour (flavour, allocator) ]

            let leader =
                match Map.tryFind system.Leader system.Tasks with
                | None -> [ UnixSystemDefect.LeaderWithoutTask system.Leader ]
                | Some state ->
                    match flavour with
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

            let duplicates =
                system.Tasks
                |> Map.toList
                |> List.groupBy (fun (_, state) -> state.OsThreadId)
                |> List.choose (fun (id, holders) ->
                    match holders with
                    | []
                    | [ _ ] -> None
                    | _ -> Some (UnixSystemDefect.DuplicateOsThreadId (id, List.map fst holders))
                )

            let unmintable =
                system.Tasks
                |> Map.toList
                |> List.filter (fun (_, state) -> not (ThreadIdAllocator.couldHaveMinted state.OsThreadId allocator))
                |> List.map (fun (task, state) ->
                    UnixSystemDefect.OsThreadIdNotMintable (task, state.OsThreadId, allocator)
                )

            allocatorFlavour @ leader @ duplicates @ unmintable

        // The pipe table against the descriptions naming its pipes, and each
        // pipe against its machine, as for sockets above.
        let pipes =
            let platform = system.Machine.UnixPlatform
            let flavour = SimulatedUnixPlatform.flavour platform

            let named =
                FileDescriptorRegistry.descriptions system.Process.FileDescriptors
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
                |> List.filter (fun (_, pipeId) -> not (Map.containsKey pipeId system.Machine.Pipes))
                |> List.map UnixSystemDefect.DanglingPipe

            let namedIds = named |> List.map snd |> Set.ofList

            let unreferenced =
                system.Machine.Pipes
                |> Map.toList
                |> List.filter (fun (pipeId, pipe) ->
                    not (Set.contains pipeId namedIds)
                    && not (PipeState.heldByClient PipeEnd.Read pipe)
                    && not (PipeState.heldByClient PipeEnd.Write pipe)
                )
                |> List.map (fst >> UnixSystemDefect.UnreferencedPipe)

            let undrained =
                system.Machine.Pipes
                |> Map.toList
                |> List.choose (fun (pipeId, pipe) ->
                    let held = PipeBuffer.held pipe.Buffer

                    match PipeState.drainedBy pipe with
                    | Some _ when held > 0 -> Some (UnixSystemDefect.DrainedPipeHoldsBytes (pipeId, held))
                    | Some _
                    | None -> None
                )

            let unsupplied =
                system.Machine.Pipes
                |> Map.toList
                |> List.filter (fun (_, pipe) -> PipeState.clientWriteCouldProceed pipe)
                |> List.map (fst >> UnixSystemDefect.SuppliedPipeHasRoom)

            let freshness =
                system.Machine.Pipes
                |> Map.toList
                |> List.map fst
                |> List.filter (fun pipeId -> pipeId >= system.Machine.NextPipeId)
                |> List.map (fun pipeId -> UnixSystemDefect.NextPipeIdNotFresh (system.Machine.NextPipeId, pipeId))

            let inodes =
                system.Machine.Pipes
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
                |> List.filter (fun (_, inode) -> inode >= system.Machine.NextPipeInode)
                |> List.map (fun (pipeId, inode) ->
                    UnixSystemDefect.PipeInodeNotFresh (system.Machine.NextPipeInode, pipeId, inode)
                )

            let inodeDuplicates =
                inodes
                |> List.countBy snd
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (inode, _) -> UnixSystemDefect.DuplicatePipeInode inode)

            let shapes =
                system.Machine.Pipes
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
                let device = system.Machine.PipeDevice

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
        @ currentDirectory
        @ danglingConnections
        @ orphanConnections
        @ duplicateQueued
        @ phaseKindMismatches
        @ drainedUnderLinux
        @ connectionFreshness
        @ ordinalFreshness
        @ ordinalDuplicates
        @ parks
        @ unreferencedDescriptions
        @ parkOrdinalFreshness
        @ parkOrdinalDuplicates
        @ bindings
        @ signals
        @ fileSystemType
        @ protectedFiles
        @ userBufferCheck
        @ supplementaryGroups
        @ umask
        @ threadIds
        @ pipes

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
    /// `withFileSystemAndCurrentDirectory`.
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
    /// `UnixBootImage.withProcessPath`.
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
    /// `SimulatedUnixPlatform.isBindableAddress`.
    let defaultLocalAddresses : uint32 list = [ InternetEndpoint.LoopbackAddress ]

    /// The prefixes Linux's local routing table holds, which it will `bind(2)`
    /// any address inside. Loopback's `127.0.0.0/8` is the one every Linux has,
    /// and is why `127.9.9.9` binds there and not on Darwin.
    let defaultLocalRoutes : Ipv4Prefix list = [ Ipv4Prefix.create 0x7F000000u 8 ]

    /// Effective user ID a freshly-minted simulated process runs as.
    ///
    /// Not 0: a process that defaulted to root would silently take the
    /// privileged branch of every check the kernel makes and every check it
    /// makes about itself (`geteuid() == 0`) — the uninteresting one, and not
    /// the one most programs are written for.
    /// Instead the first interactive user each flavour creates: 1000 on the
    /// Ubuntu-shaped Linux, and 501 on macOS (measured, `id -u` of the first
    /// account on a macOS 26 machine, 2026-09-08). A client that wants root says
    /// so with `UnixBootImage.withCredentials`.
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
    /// flavours. A client chooses otherwise with `UnixBootImage.withUmask`, and
    /// the process itself with `umask`.
    let defaultUmask : PermissionBits =
        PermissionBits.parseOrFail "UnixSystem.defaultUmask" 0o022

    /// Whether a freshly-minted simulated process writes a core dump when a
    /// signal kills it: never, as under an `RLIMIT_CORE` of 0. A dump is a
    /// file the simulated process would leave behind, which a client has to
    /// ask for. A client chooses otherwise with
    /// `UnixBootImage.withCoreDumps`.
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

    /// The boot image of a simulated process on a machine of the given platform:
    /// no sockets, no connections, an empty filesystem, and only the
    /// descriptors `launch` gives it open. Configure it with the setters in
    /// `UnixBootImage`, then `UnixBootImage.boot` it for its first syscall.
    ///
    /// Each entry of `launch` is a descriptor the launcher set up before the
    /// process started, at that number: a pipe end of its own, whose other end
    /// is the client's, as the entry says (see `LaunchDescriptor`). The pipes
    /// are the first the machine makes, in descriptor order. A launch table
    /// naming a negative descriptor is refused.
    ///
    /// It has one task, `leader`, on the logical processor `leaderCpu`. The
    /// leader's thread ID is the process ID, `defaultProcessId`: on Linux because
    /// it always is, and on Darwin as the start of a quiet machine's counter,
    /// which a client moves with `UnixBootImage.withLeaderThreadId`.
    ///
    /// The fields the platform *fixes* are derived from it rather than
    /// taken as arguments — `SoMaxConn`, `TcpSendSpace`, `Mount`, and the platform
    /// itself — because a machine whose flavour and those disagree is one no
    /// real system could be: `EmulatedFileSystemType.isReportableUnder` says
    /// outright that a Darwin kernel never reports tmpfs. Building the record
    /// by hand is what lets that state exist, so the constructor is also the
    /// rule.
    ///
    /// The ephemeral port range and the process identity are the flavour's
    /// shipped defaults too, though a host may set either: they are what a
    /// default machine of that flavour reports, not facts of its kernel image.
    /// The buffer check is the platform's default too, though its limit is a
    /// property of the machine's paging depth rather than of its kernel. All of
    /// these are configuration a caller overrides with the setters in
    /// `UnixBootImage`, which is also how a caller supplies a non-empty
    /// filesystem or a different address list.
    let initial<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (platform : SimulatedUnixPlatform)
        (launch : Map<int, LaunchDescriptor>)
        (leader : 'Task)
        (leaderCpu : CpuId)
        : UnixBootImage<'Task, 'Handler>
        =
        // `SimulatedUnixPlatform.create` validates at construction, so a value
        // of the type is already a platform some Unix could be; this catches
        // the one value that bypasses that, the forged `Unchecked.defaultof`,
        // whose null release would otherwise reach a process as its `uname -r`.
        let platform = SimulatedUnixPlatform.assertValid "UnixSystem.initial" platform
        let flavour = SimulatedUnixPlatform.flavour platform

        let deviceMount = DeviceFileSystemMount.defaultFor flavour

        // Bound once so that `CurrentDirectoryInode` is the root of *this*
        // filesystem rather than of a second one that merely looks like it.
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

        let leaderThreadId, threadIds =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                ThreadIdAllocator.startLinux "UnixSystem.initial" defaultPidMax defaultProcessId
            | SimulatedUnixFlavour.Darwin ->
                ThreadIdAllocator.startDarwin "UnixSystem.initial" (uint64 (ProcessId.toInt32 defaultProcessId))

        // One pipe per launch descriptor, numbered in descriptor order from the
        // first pipe the machine makes.
        let launched =
            launch
            |> Map.toList
            |> List.mapi (fun index (fd, descriptor) ->
                if fd < 0 then
                    failwith
                        $"UnixSystem.initial: the launch table names descriptor %d{fd}, which is negative; no process has a descriptor below 0."

                let pipeId = PipeId (int64 index)
                let pipe, pipeEnd = PipeState.launch platform fd descriptor
                fd, pipeId, pipeEnd, pipe
            )

        {
            System =
                {
                    Machine =
                        {
                            Sockets = Map.empty
                            Pipes = launched |> List.map (fun (_, pipeId, _, pipe) -> pipeId, pipe) |> Map.ofList
                            NextPipeId = PipeId (int64 (List.length launched))
                            Delivered = DeliveryLog.empty
                            // Any start would do; one, because no filesystem hands out
                            // inode 0.
                            NextPipeInode = InodeNumber 1L
                            PipeDevice = UnixMachineState.defaultPipeDevice flavour
                            Connections = Map.empty
                            NextConnectionId = ConnectionId 0L
                            NextSocketEventRegistrationOrdinal = 0L
                            NextParkOrdinal = ParkOrdinal 0L
                            ThreadIds = threadIds
                            NextSocketId = SocketId 0L
                            NextEphemeralPort = fst (defaultEphemeralPortRange flavour)
                            EphemeralPortRange = defaultEphemeralPortRange flavour
                            SoMaxConn = UnixMachineState.defaultSoMaxConn flavour
                            TcpSendSpace = UnixMachineState.defaultTcpSendSpace flavour
                            LocalAddresses = defaultLocalAddresses
                            LocalRoutes = defaultLocalRoutes
                            NanosecondsSinceBoot = 0L
                            BootTime = UnixTimestamp.epoch
                            EntropyPool = EntropyPool.ofSeed defaultEntropySeed
                            ProcessorCount = defaultProcessorCount
                            UserBufferCheck = defaultUserBufferCheck platform
                            UnixPlatform = platform
                            FileSystem = filesystem
                            Mount = EmulatedMount.defaultFor flavour
                            DeviceMount = deviceMount
                            ProtectedFiles = ProtectedFiles.off
                        }
                    Process =
                        {
                            FileDescriptors =
                                launched
                                |> List.map (fun (fd, pipeId, pipeEnd, _) -> fd, (pipeId, pipeEnd))
                                |> Map.ofList
                                |> FileDescriptorRegistry.ofLaunchedPipes
                            Environment = []
                            // The default current directory is the root, which every filesystem
                            // has and no operation can remove, so the pair starts consistent
                            // whatever else a host goes on to set.
                            CurrentDirectoryInode = VirtualFileSystem.root filesystem
                            ProcessPath = defaultProcessPath
                            Credentials = defaultCredentials flavour
                            Umask = defaultUmask
                            ProcessId = defaultProcessId
                            Signals = SignalState.initial (SimulatedUnixPlatform.signalNumbering platform) Set.empty
                            CoreDumps = defaultCoreDumps
                        }
                    Tasks = UnixTaskTable.add leader leaderCpu leaderThreadId Map.empty
                    Leader = leader
                }
        }

    /// The machine's administrator writes Linux's `kernel.pid_max` sysctl
    /// (through `/proc/sys`), which a sysctl allows at any time, the machine
    /// running or not: thread IDs are below it, and once they reach it they
    /// start again from 300, skipping those still in use.
    ///
    /// Not configuration, which is `UnixBootImage`'s, but the outside world
    /// acting on a running machine, as `UnixSystem.advanceClock` is.
    ///
    /// Refuses a Darwin machine, which has no such setting; a value Linux does not
    /// accept, which is anything outside 301 to 4194304; and a value at or below a
    /// live task's thread ID.
    let writePidMaxSysctl<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (pidMax : int32)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let threadIds = ThreadIdAllocator.withPidMax context pidMax system.Machine.ThreadIds

        // What Linux does with a live ID at or above a lowered `pid_max` has not
        // been measured.
        match
            system.Tasks
            |> Map.tryFindKey (fun _ state -> not (ThreadIdAllocator.couldHaveMinted state.OsThreadId threadIds))
        with
        | Some task ->
            failwith
                $"%s{context}: task %O{task} has thread ID %O{(UnixTaskTable.osThreadIdOf task system.Tasks)}, which is not below pid_max %d{pidMax}."
        | None ->

        { system with
            Machine =
                { system.Machine with
                    ThreadIds = threadIds
                }
        }
