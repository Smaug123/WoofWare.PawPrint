namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// A request to this kernel, in the vocabulary of the kernel ABI rather than of
/// any client's foreign-function layer.
///
/// Arguments a real kernel validates arrive raw — `LSeek`'s `whence`, and every
/// `fd` — because rejecting them is behaviour this library models, and models
/// per flavour. Arguments only the client can classify arrive classified.
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
    | MkDir of path : UnixPath * mode : int
    | Unlink of path : UnixPath
    | RmDir of path : UnixPath
    | ChDir of path : UnixPath
    /// `mode` is raw, as `chmod(2)` takes it: which of its bits the inode gets
    /// is behaviour this kernel models.
    | ChMod of path : UnixPath * mode : int
    /// `mode` is raw, as `fchmod(2)` takes it.
    | FChMod of fd : int * mode : int
    /// `mask` is raw, as `umask(2)` takes it: which of its bits the process
    /// keeps is behaviour this kernel models, and models per flavour. Answers
    /// the previous mask.
    | UMask of mask : int

/// Why this kernel will not answer a syscall at all. The client decides what a
/// refusal means for it; nothing here is recoverable by retrying.
[<RequireQualifiedAccess>]
type SyscallRefusal<'Task> =
    | LSeek of LSeekRefusal
    | FLock of FLockRefusal
    | FTruncate of TruncationRefusal
    | Unlink of StickyRefusal
    | RmDir of StickyRefusal
    | ChMod of ChModRefusal
    | FChMod of FChModRefusal
    | Close of CloseRefusal<'Task>

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
    /// guest passes would resolve from a place that is not a directory.
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
    /// `OpenFileTarget.File` onto a directory, or an `OpenFileTarget.Directory`
    /// onto anything else. `open` chooses the target by what it opened, so a
    /// directory's position is always a place in its entries and never a byte
    /// offset.
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
    /// A socket event registration records an ADD ordinal at or above the
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
    /// about it crashes. `close` refuses to destroy a description a task is
    /// parked on, so this is a park recorded without one or a close made
    /// around it.
    | ParkedOnAbsentDescription of task : 'Task * description : OpenFileDescriptionId
    /// A task is parked in a socket-event wait on a description that is not a
    /// socket event port, which no wait could have produced and which
    /// `SocketEventPort.hasDeliverableEvent` crashes on.
    | ParkedSocketWaitOnNonPort of task : 'Task * description : OpenFileDescriptionId * target : OpenFileTarget
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
    /// A task is parked in an `accept` on a description that is not a listening
    /// socket, which no accept could have produced and on which
    /// `WakeCondition.satisfied` crashes.
    | ParkedAcceptOnNonListener of task : 'Task * description : OpenFileDescriptionId
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
    /// A bound socket holds port 0, which is how a guest *asks* for a port and
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
    /// its machine's platform assigns, so the same `Signal.Other` payload
    /// means one signal to the kernel and another to the signal tables.
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

/// Why the directory a host named cannot be the one a simulated process starts
/// in. `UnixSystem.withFileSystemAndCurrentDirectory` returns one instead of
/// deciding what to say about it: the remedy is always "fix the knob you set
/// this from", and only the caller knows what that knob is called.
///
/// Every case is a host mistake rather than a guest one, which is why none of
/// them is a `UnixError`: there is no errno for "you seeded a filesystem that
/// does not contain the directory you asked to start in", and answering ENOENT
/// would blame a guest path that does not exist yet.
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
    /// have created it, and a guest there could not create it either.
    | SeedNameNotBindable of name : DirectoryEntryName * flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module UnixSystem =

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
        | Syscall.MkDir (path, mode) -> Ok (UnixNamespace.mkdir path mode system) |> answered
        | Syscall.Unlink path ->
            UnixNamespace.unlink path system
            |> answered
            |> Result.mapError SyscallRefusal.Unlink
        | Syscall.RmDir path ->
            UnixNamespace.rmdir path system
            |> answered
            |> Result.mapError SyscallRefusal.RmDir
        | Syscall.ChDir path -> Ok (UnixPathResolution.chdir path system) |> answered
        | Syscall.ChMod (path, mode) ->
            UnixPathResolution.chmod path mode system
            |> answered
            |> Result.mapError SyscallRefusal.ChMod
        | Syscall.FChMod (fd, mode) ->
            UnixPathResolution.fchmod fd mode system
            |> answered
            |> Result.mapError SyscallRefusal.FChMod
        | Syscall.UMask mask ->
            let previous, system = umask mask system

            Ok (SyscallOutcome.Answered (SyscallAnswer.Completed (int64 (PermissionBits.toInt previous))), system)

    /// Every way this system's tables disagree with each other: the socket table
    /// and the pipe table against the descriptor table, each pipe and the pipe
    /// device against the platform, the connection table against the sockets
    /// that reference it, the descriptor table against the filesystem, the
    /// current directory against both, each task's park against the descriptor
    /// table, the signal state against the task table, and the machine's
    /// filesystem type and buffer check and the process's supplementary groups
    /// and file-mode creation mask against its platform.
    ///
    /// Each table's own rules are elsewhere and are not repeated here:
    /// `FileDescriptorRegistry.checkInvariants` for the descriptor table, and
    /// `VirtualFileSystem.checkInvariants` for the filesystem. The latter takes
    /// a `pinned` argument, which is what `pinnedInodes` computes, so a caller
    /// wanting the whole picture pairs this with
    /// `VirtualFileSystem.checkInvariants (UnixDescriptor.pinnedInodes system) system.Machine.FileSystem`.
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
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
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

        let danglingInodes =
            system.Process.FileDescriptors
            |> FileDescriptorRegistry.descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _) ->
                    match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.Directory _) -> Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.Symlink _) -> None
                | OpenFileTarget.Directory (inode, _) ->
                    match VirtualFileSystem.tryGetContent inode system.Machine.FileSystem with
                    | None -> Some (UnixSystemDefect.DanglingOpenInode (id, inode))
                    | Some (InodeContent.Directory _) -> None
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.Symlink _) -> Some (UnixSystemDefect.DescriptionKindMismatch (id, inode))
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _
                | OpenFileTarget.Pipe _ -> None
            )

        let currentDirectory =
            match VirtualFileSystem.tryGetContent system.Process.CurrentDirectoryInode system.Machine.FileSystem with
            | Some (InodeContent.Directory _) -> []
            | Some (InodeContent.RegularFile _)
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
                | SocketPhase.RefusedPendingDelivery
                | SocketPhase.Dead
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
                | OpenFileTarget.Pipe _ -> []
                | OpenFileTarget.SocketEventPort portState ->
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
        // task waits on, and the wake reads the description back; `close`
        // refuses to destroy one a task is parked on, so an absent one was
        // parked on without going through the syscall or closed around it.
        let parks =
            let descriptions =
                FileDescriptorRegistry.descriptions system.Process.FileDescriptors

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
                        | OpenFileTarget.SocketEventPort _ -> []
                        | OpenFileTarget.File _
                        | OpenFileTarget.Directory _
                        | OpenFileTarget.Socket _
                        | OpenFileTarget.Pipe _ ->
                            [
                                UnixSystemDefect.ParkedSocketWaitOnNonPort (task, wait.Port, description.Target)
                            ]
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
                                    | OpenFileTarget.SocketEventPort _ ->
                                        [ UnixSystemDefect.ParkedPollOnSocketEventPort (task, watched) ]
                                    | OpenFileTarget.File _
                                    | OpenFileTarget.Directory _
                                    | OpenFileTarget.Socket _
                                    | OpenFileTarget.Pipe _ -> []

                                let rebound =
                                    match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
                                    | Some current when current = watched -> []
                                    | current ->
                                        [ UnixSystemDefect.ParkedPollDescriptorRebound (task, fd, watched, current) ]

                                target @ rebound
                    )
                | Some (ParkedSyscall.Accept accept) ->
                    match Map.tryFind accept.Listener descriptions with
                    | None -> [ UnixSystemDefect.ParkedOnAbsentDescription (task, accept.Listener) ]
                    | Some description ->
                        let listening =
                            match description.Target with
                            | OpenFileTarget.Socket socketId ->
                                match Map.tryFind socketId system.Machine.Sockets with
                                | Some {
                                           Phase = SocketPhase.Listening _
                                       } -> true
                                | Some _
                                | None -> false
                            | OpenFileTarget.File _
                            | OpenFileTarget.Directory _
                            | OpenFileTarget.Pipe _
                            | OpenFileTarget.SocketEventPort _ -> false

                        if listening then
                            []
                        else
                            [ UnixSystemDefect.ParkedAcceptOnNonListener (task, accept.Listener) ]
            )

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
        // the machine's platform: the state canonicalises every signal under
        // the numbering it was built with, so a state built under the wrong
        // one holds tables the kernel would read as different signals.
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
                    | OpenFileTarget.SocketEventPort _
                    | OpenFileTarget.File _
                    | OpenFileTarget.Directory _
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
            @ freshness
            @ inodeFreshness
            @ inodeDuplicates
            @ shapes
            @ device

        dangling
        @ unreferenced
        @ freshness
        @ danglingInodes
        @ currentDirectory
        @ danglingConnections
        @ orphanConnections
        @ duplicateQueued
        @ phaseKindMismatches
        @ connectionFreshness
        @ ordinalFreshness
        @ ordinalDuplicates
        @ parks
        @ parkOrdinalFreshness
        @ parkOrdinalDuplicates
        @ bindings
        @ signals
        @ fileSystemType
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
    /// `UnixMachineState.withProcessorCount`.
    [<Literal>]
    let defaultProcessorCount : int = 1

    /// The buffer check a freshly-minted machine on `platform` applies: none up
    /// front where the platform screens nothing, and otherwise a screen at the
    /// commonest `TASK_SIZE_MAX` of the platform's architecture, which is
    /// four-level paging on x86-64 and a 48-bit virtual address on arm64. A client
    /// simulating a machine with a different address-space width sets it with
    /// `UnixMachineState.withUserAddressLimit`.
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
    /// `UnixProcessState.withProcessPath`.
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
    /// so with `UnixSystem.withCredentials`.
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
    /// flavours. A client chooses otherwise with `UnixSystem.withUmask`, and
    /// the process itself with `umask`.
    let defaultUmask : PermissionBits =
        PermissionBits.parseOrFail "UnixSystem.defaultUmask" 0o022

    /// Whether a freshly-minted simulated process writes a core dump when a
    /// signal kills it: never, as under an `RLIMIT_CORE` of 0. A dump is a
    /// file the simulated process would leave behind, which a client has to
    /// ask for. A client chooses otherwise with
    /// `UnixProcessState.withCoreDumps`.
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
    /// value, because every byte the pool hands out follows from it.
    let defaultEntropySeed : uint64 = 0x243F6A8885A308D3UL

    /// The `pid_max` a freshly-minted Linux machine has: 4194304, the most Linux
    /// allows, so that thread IDs are reused only after that many have been
    /// handed out. A client chooses otherwise with `withPidMax`.
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
    /// nothing, and descriptors 1 and 2 write to pipes the client drains.
    ///
    /// Not the only shape a real process can inherit, and not the terminal one.
    /// Under a tty, descriptors 0, 1 and 2 are `dup`s of a single `O_RDWR`
    /// description: measured via `forkpty`, setting `O_NONBLOCK` through
    /// descriptor 1 becomes visible on 0 and 2, and `write(0, _, _)` succeeds.
    let pipedStandardStreams : Map<int, LaunchDescriptor> =
        Map.ofList
            [
                0, LaunchDescriptor.SuppliedNothing
                1, LaunchDescriptor.Drained
                2, LaunchDescriptor.Drained
            ]

    /// A simulated process on a machine of the given platform, before anything
    /// has happened to it: no sockets, no connections, an empty filesystem, and
    /// only the descriptors `launch` gives it open.
    ///
    /// Each entry of `launch` is a descriptor the launcher set up before the
    /// process started, at that number: a pipe end of its own, whose other end
    /// the client holds as the entry says (see `LaunchDescriptor`). The pipes
    /// are the first the machine makes, in descriptor order. A launch table
    /// naming a negative descriptor is refused.
    ///
    /// It has one task, `leader`, on the logical processor `leaderCpu`. The
    /// leader's thread ID is the process ID, `defaultProcessId`: on Linux because
    /// it always is, and on Darwin as the start of a quiet machine's counter,
    /// which a client moves with `withLeaderThreadId`.
    ///
    /// The three fields the platform *fixes* are derived from it rather than
    /// taken as arguments — `SoMaxConn`, `Mount`, and the platform
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
    /// these are configuration a caller overrides
    /// by record-update or the setters, which is also how a caller supplies a
    /// non-empty filesystem or a different address list.
    let initial<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (platform : SimulatedUnixPlatform)
        (launch : Map<int, LaunchDescriptor>)
        (leader : 'Task)
        (leaderCpu : CpuId)
        : UnixSystem<'Task, 'Handler>
        =
        // `SimulatedUnixPlatform.create` validates at construction, so a value
        // of the type is already a platform some Unix could be; this catches
        // the one value that bypasses that, the forged `Unchecked.defaultof`,
        // whose null release would otherwise reach a guest as its `uname -r`.
        let platform = SimulatedUnixPlatform.assertValid "UnixSystem.initial" platform
        let flavour = SimulatedUnixPlatform.flavour platform

        // Bound once so that `CurrentDirectoryInode` is the root of *this*
        // filesystem rather than of a second one that merely looks like it.
        let filesystem =
            VirtualFileSystem.empty
                (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                (InodeOwner.ofProcess (defaultCredentials flavour))

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

                let pipeEnd =
                    match descriptor with
                    | LaunchDescriptor.SuppliedNothing -> PipeEnd.Read
                    | LaunchDescriptor.Drained -> PipeEnd.Write

                let pipe =
                    {
                        Buffer = PipeBuffer.empty platform
                        Origin = PipeOrigin.Launched (ExternalEndpoint fd, descriptor)
                    }

                fd, pipeId, pipeEnd, pipe
            )

        {
            Machine =
                {
                    Sockets = Map.empty
                    Pipes = launched |> List.map (fun (_, pipeId, _, pipe) -> pipeId, pipe) |> Map.ofList
                    NextPipeId = PipeId (int64 (List.length launched))
                    Delivered = ImmutableArray.Empty
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

    // The process's leader and the only task it has, failing with `context` if it
    // has created a thread, even one that has since exited: every setter below is
    // a boot-time setting, and moving the counter back after a thread has taken an
    // id would hand that id out again.
    let private soleTask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixTaskState
        =
        let leader = UnixTaskTable.get system.Leader system.Tasks

        if
            system.Tasks.Count <> 1
            || not (ThreadIdAllocator.untouchedSince leader.OsThreadId system.Machine.ThreadIds)
        then
            failwith
                $"%s{context}: the process has created a thread, but this can only be set before any thread has been created."

        leader

    /// Set the ID `getpid(2)` reports for the simulated process. On Linux this is
    /// also the leader's thread ID, and the thread IDs the process's threads get
    /// follow on from it.
    ///
    /// `context` prefixes the rejection a configuration earns; see
    /// `withCredentials`.
    ///
    /// Refuses a process that has already created a thread, and on Linux a
    /// process ID that is not below the machine's `pid_max`.
    let withProcessId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (pid : ProcessId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let pid = ProcessId.assertValid context pid
        let leader = soleTask context system

        let tasks, machine =
            match system.Machine.ThreadIds with
            | ThreadIdAllocator.Linux (_, pidMax) ->
                let leaderThreadId, threadIds = ThreadIdAllocator.startLinux context pidMax pid

                Map.add
                    system.Leader
                    { leader with
                        OsThreadId = leaderThreadId
                    }
                    system.Tasks,
                { system.Machine with
                    ThreadIds = threadIds
                }
            | ThreadIdAllocator.Darwin _ -> system.Tasks, system.Machine

        { system with
            Machine = machine
            Process =
                { system.Process with
                    ProcessId = pid
                }
            Tasks = tasks
        }

    /// Set the leader's thread ID on Darwin, where it is unrelated to the process
    /// ID; the IDs the process's threads get follow on from it.
    ///
    /// Refuses a Linux machine, where the leader's thread ID is the process ID
    /// (set that with `withProcessId`); a process that has already created a
    /// thread; and 0.
    let withLeaderThreadId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (id : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        match system.Machine.ThreadIds with
        | ThreadIdAllocator.Linux _ ->
            failwith
                $"%s{context}: on Linux the leader's thread ID is the process ID, so it cannot be set apart from it; set the process ID instead."
        | ThreadIdAllocator.Darwin _ ->

        let leader = soleTask context system
        let leaderThreadId, threadIds = ThreadIdAllocator.startDarwin context id

        { system with
            Machine =
                { system.Machine with
                    ThreadIds = threadIds
                }
            Tasks =
                Map.add
                    system.Leader
                    { leader with
                        OsThreadId = leaderThreadId
                    }
                    system.Tasks
        }

    /// Set Linux's `pid_max`: thread IDs are below it, and once they reach it they
    /// start again from 300, skipping those still in use.
    ///
    /// Refuses a Darwin machine, which has no such setting; a value Linux does not
    /// accept, which is anything outside 301 to 4194304; and a value at or below a
    /// live task's thread ID.
    let withPidMax<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
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

    /// Set who the simulated process is.
    ///
    /// `context` prefixes the rejection a configuration earns, and is the
    /// client's to choose, so a host that has to fix one is told the name its
    /// own configuration gives it.
    ///
    /// Refuses more supplementary groups than the platform's
    /// `SimulatedUnixPlatform.supplementaryGroupLimit`, which no process on it
    /// could hold. On Darwin it also refuses credentials whose real, effective
    /// and saved IDs are not all the same: which of them a Darwin kernel
    /// consults has not been measured, so this library does not model such a
    /// process there.
    let withCredentials<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (credentials : Credentials)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let platform = system.Machine.UnixPlatform
        let count = List.length credentials.SupplementaryGroups
        let limit = SimulatedUnixPlatform.supplementaryGroupLimit platform

        if count > limit then
            failwith
                $"%s{context}: %d{count} supplementary groups is more than the %d{limit} a process can hold on %O{SimulatedUnixPlatform.flavour platform} (setgroups(2) answers EINVAL above NGROUPS_MAX)."

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> ()
        | SimulatedUnixFlavour.Darwin ->
            // Measuring it needs a process that can change its user ID, which is
            // root, and none has been available on Darwin.
            let usersAgree =
                credentials.RealUser = credentials.EffectiveUser
                && credentials.SavedUser = credentials.EffectiveUser

            let groupsAgree =
                credentials.RealGroup = credentials.EffectiveGroup
                && credentials.SavedGroup = credentials.EffectiveGroup

            if not (usersAgree && groupsAgree) then
                failwith
                    $"%s{context}: the credentials %O{credentials} have real, effective and saved IDs that differ, which this library does not model on Darwin: which of them a Darwin kernel consults has not been measured. Give all three the same user ID and the same group ID."

        { system with
            Process =
                { system.Process with
                    Credentials = credentials
                }
        }

    /// Set the file-mode creation mask the simulated process starts with: the
    /// one its parent left it, which it can read and replace with `umask`.
    ///
    /// `context` prefixes the rejection a configuration earns; see
    /// `withCredentials` for why the client supplies it.
    ///
    /// Refuses a mask with a bit the platform's `umask(2)` never stores
    /// (`SimulatedUnixPlatform.umaskStoredBits`): on Linux, any of 0o7000. No
    /// parent could have left such a mask, so it names a process that cannot
    /// exist.
    let withUmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (umask : PermissionBits)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let umask = PermissionBits.assertValid context umask
        let platform = system.Machine.UnixPlatform
        let stored = SimulatedUnixPlatform.umaskStoredBits platform

        if PermissionBits.toInt umask &&& ~~~(PermissionBits.toInt stored) <> 0 then
            failwith
                $"%s{context}: the mask 0o%04o{PermissionBits.toInt umask} holds a bit %O{SimulatedUnixPlatform.flavour platform}'s umask(2) never stores (it keeps only 0o%04o{PermissionBits.toInt stored}), so no process there could have it."

        { system with
            Process =
                { system.Process with
                    Umask = umask
                }
        }

    /// Realise `seed` as this system's filesystem and start the simulated
    /// process in `directory`, together.
    ///
    /// One operation rather than two because neither answer is well-formed
    /// without the other: a current directory is an inode of *this* filesystem,
    /// and a filesystem replaces every inode number the previous one handed
    /// out.
    ///
    /// Takes the moment explicitly rather than reading
    /// the machine's realtime clock, so that the result does not depend
    /// on whether the caller happened to set the clock before or after the
    /// filesystem — an ordering dependence between two `with` functions is
    /// exactly the kind of thing that works until someone reorders the calls.
    ///
    /// The system's own platform decides whether the *path the caller wrote*
    /// is one a process on that flavour could name at all, through its
    /// `NAME_MAX`: 255 CJK characters is a legal directory name on Darwin and
    /// too long on Linux. It is a check on that path and not on the graph —
    /// the seed itself is realised without consulting any limit, so a
    /// filesystem may perfectly well contain a directory whose name the
    /// current directory could not spell.
    ///
    /// A **boot-time** operation: it crashes if the process still holds any
    /// handle onto the filesystem being replaced — an open descriptor onto a
    /// file or directory — because the new filesystem hands out its own inode
    /// numbers, and such a handle would afterwards name a graph that no longer
    /// exists or, undetectably, whatever the new one gave the same number. The
    /// current directory is not such a handle: replacing it is the point.
    ///
    /// The walk is privileged and symlink-following, deliberately: this is a
    /// host saying where its guest was launched, not a guest looking anything
    /// up, and a process is launched into a directory its parent had already
    /// reached. It is also the only moment the name is resolved, because after
    /// it the process holds the *directory* rather than the name.
    ///
    /// So this records the inode alone. The path `getcwd` owes is derived from
    /// it, which is what makes that path the physical one with every symlink
    /// resolved away — measured on both kernels, `chdir("outer/lnk")` with
    /// `lnk -> inner` is followed by `getcwd() == ".../outer/inner"`.
    ///
    /// `defaultOwner` owns the root and every seed entry that states no owner
    /// of its own; see `VirtualFileSystem.ofFileSystemSeed`. It is an argument
    /// rather than read off the process, so that the result does not depend on
    /// whether the caller set the credentials before or after the filesystem.
    let withFileSystemAndCurrentDirectory<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (createdAt : UnixTimestamp)
        (defaultOwner : InodeOwner)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        (directory : AbsoluteUnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixSystem<'Task, 'Handler>, CurrentDirectoryFault>
        =
        // The directory is admitted under the platform the process will run
        // on, which is the system's own: `NAME_MAX` counts bytes on Linux and
        // UTF-16 code units on Darwin, so a name one flavour admits is one the
        // other refuses.
        let platform = system.Machine.UnixPlatform

        // Asserted here as well as by any caller that names its own knob: this
        // is a package boundary, so the precondition cannot be left to the one
        // client that happens to check it today.
        let directory =
            AbsoluteUnixPath.assertValid "UnixSystem.withFileSystemAndCurrentDirectory" directory

        // A precondition on the *system*, not on the arguments, and the reason
        // this is a boot-time operation: a new filesystem hands out its own
        // inode numbers, so a handle onto the old graph would afterwards dangle
        // or -- worse -- silently name an unrelated object given the same
        // number. `checkInvariants` reports the first as `DanglingOpenInode`
        // and cannot see the second at all, so this refuses rather than
        // producing a system whose corruption is only sometimes detectable.
        //
        // Counted as *holders*, never as a set of inode numbers with the
        // current directory subtracted out. The current directory is exempt
        // because this operation replaces it, not because its inode number is;
        // a descriptor or stream onto that same inode -- `opendir(".")` -- is a
        // holder like any other, and subtracting the value would erase it from
        // the reckoning along with the field that is genuinely exempt.
        //
        // Both holder kinds are read here rather than through
        // `UnixProcessState.heldInodes`, which answers a set for the reaper's
        // reachability question and so cannot distinguish them.
        let strandedDescriptions =
            system.Process.FileDescriptors
            |> FileDescriptorRegistry.descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.File (inode, _)
                | OpenFileTarget.Directory (inode, _) -> Some $"description %O{id} onto %O{inode}"
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _
                | OpenFileTarget.Pipe _ -> None
            )

        match strandedDescriptions with
        | [] -> ()
        | stranded ->
            let listed = String.concat "; " stranded

            failwith
                $"UnixSystem.withFileSystemAndCurrentDirectory: the process still holds %d{List.length stranded} handle(s) onto the current filesystem (%s{listed}). Replacing the filesystem would leave them naming a graph that no longer exists, or silently naming whatever the new one gives the same inode number. This is a boot-time operation; close them first, or build the system with the filesystem it is to run on."

        let limits = SimulatedUnixPlatform.pathLimits platform
        let bindable = SimulatedUnixPlatform.bindableEntryNames platform
        let flavour = SimulatedUnixPlatform.flavour platform

        // Every name in the seed, under this flavour's NAME_MAX and then its
        // rule for which names it binds -- the order a binding checks them in --
        // before the graph is built: a name a kernel could never have created
        // is not one its filesystem can hold. The first offender in `Map`
        // order, which is the order the seed is realised in.
        let rec firstImpossibleName (entries : Map<DirectoryEntryName, SeedEntry>) : CurrentDirectoryFault option =
            entries
            |> Map.toSeq
            |> Seq.tryPick (fun (name, entry) ->
                // A forged name is refused with the seed's context, as
                // `ofFileSystemSeed` would refuse it, rather than reaching the
                // measurement as a null.
                let name =
                    DirectoryEntryName.assertValid "UnixSystem.withFileSystemAndCurrentDirectory seed" name

                if not (PathLimits.nameWithinLimit limits name) then
                    Some (CurrentDirectoryFault.SeedNameTooLong (name, flavour))
                elif not (BindableEntryNames.admits bindable name) then
                    Some (CurrentDirectoryFault.SeedNameNotBindable (name, flavour))
                else
                    match entry with
                    | SeedEntry.Directory (children, _, _) -> firstImpossibleName children
                    | SeedEntry.File _
                    | SeedEntry.Symlink _ -> None
            )

        match firstImpossibleName seed with
        | Some fault -> Error fault
        | None ->

        let filesystem = VirtualFileSystem.ofFileSystemSeed createdAt defaultOwner seed
        let root = VirtualFileSystem.root filesystem

        let located =
            match
                PathWalk.resolveExisting
                    limits
                    // Root, so that no directory's search bit refuses the walk:
                    // it is privilege that exempts a caller, whoever owns what.
                    (Credentials.ofIds
                        UserId.root
                        (GroupId.parseOrFail "UnixSystem.withFileSystemAndCurrentDirectory" 0u)
                        [])
                    root
                    SymlinkPolicy.Follow
                    (UnixPath.ofAbsolute directory)
                    filesystem
            with
            | Ok inode ->
                match VirtualFileSystem.tryGetContent inode filesystem with
                | Some (InodeContent.Directory _) ->
                    // The walk started at the root, so a directory it
                    // reached has a path back by construction, and
                    // `toVirtualFileSystem` asserts its own invariants besides.
                    // Checked anyway: the alternative to crashing here is a
                    // guest whose `getcwd` reports ENOENT from its first
                    // instruction.
                    match VirtualFileSystem.pathOfDirectory inode filesystem with
                    | Some _ -> Ok inode
                    | None ->
                        failwith
                            $"UnixSystem.withFileSystemAndCurrentDirectory: \"%s{AbsoluteUnixPath.toEscaped directory}\" resolved to inode %O{inode}, but no path from the root reaches it. This is a bug in this library."
                | Some (InodeContent.RegularFile _) -> Error CurrentDirectoryFault.NotADirectory
                | Some (InodeContent.Symlink _) ->
                    // `SymlinkPolicy.Follow` never finishes on one; `chdir` says
                    // the same of the same walk.
                    failwith
                        $"UnixSystem.withFileSystemAndCurrentDirectory: the walk resolved \"%s{AbsoluteUnixPath.toEscaped directory}\" to inode %O{inode}, which is a symbolic link -- but it ran under SymlinkPolicy.Follow, which never finishes on one (this is a bug in this library)."
                | None ->
                    failwith
                        $"UnixSystem.withFileSystemAndCurrentDirectory: resolving \"%s{AbsoluteUnixPath.toEscaped directory}\" gave inode %O{inode}, which the filesystem does not contain. This is a bug in this library; run VirtualFileSystem.checkInvariants."
            | Error UnixError.ENAMETOOLONG ->
                Error (CurrentDirectoryFault.TooLong (SimulatedUnixPlatform.flavour platform))
            | Error error -> Error (CurrentDirectoryFault.DoesNotResolve error)

        located
        |> Result.map (fun inode ->
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
                Process =
                    { system.Process with
                        CurrentDirectoryInode = inode
                    }
            }
        )
