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

/// Why this kernel will not answer a syscall at all. The client decides what a
/// refusal means for it; nothing here is recoverable by retrying.
[<RequireQualifiedAccess>]
type SyscallRefusal<'Task> =
    | LSeek of LSeekRefusal
    | FLock of FLockRefusal
    | FTruncate of TruncationRefusal
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
    /// way to make a socket — `UnixSocket.createSocket`, or `UnixConnection.accept`
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
    /// An open directory stream names an inode the filesystem no longer holds.
    ///
    /// Unreachable by construction — `UnixProcessState.heldInodes` counts a stream's inode
    /// among the things pinning it, so `UnixDescriptor.forgetIfUnheld` cannot free one out from under
    /// a stream — which is exactly why a violation is a bug in this library (or
    /// in a caller that assembled the state by hand) rather than something the
    /// process did. The next `readdir` would crash, and this names the cause
    /// instead.
    | DanglingDirectoryStreamInode of stream : DirectoryStreamId * inode : InodeNumber
    /// An open directory stream names an inode that is not a directory.
    | DirectoryStreamIsNotADirectory of stream : DirectoryStreamId * inode : InodeNumber
    /// The stream table holds an id at or above `NextDirectoryStreamId`, so the
    /// next `opendir` would hand out an id that is already in use.
    | NextDirectoryStreamIdNotFresh of nextDirectoryStreamId : DirectoryStreamId * existing : DirectoryStreamId
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
    /// A per-task signal mask names a task the table does not hold.
    | SignalMaskWithoutTask of task : 'Task
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
        | Syscall.Unlink path -> Ok (UnixNamespace.unlink path system) |> answered
        | Syscall.RmDir path -> Ok (UnixNamespace.rmdir path system) |> answered
        | Syscall.ChDir path -> Ok (UnixPathResolution.chdir path system) |> answered

    /// Every way this system's tables disagree with each other: the socket table
    /// against the descriptor table, the connection table against the sockets
    /// that reference it, the descriptor table against the filesystem, the
    /// current directory against both, each task's park against the descriptor
    /// table, the signal state against the task table, and the machine's
    /// filesystem type and buffer check against its platform.
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
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _ -> None
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
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _ -> None
            )

        let danglingStreams =
            system.Process.DirectoryStreams
            |> Map.toList
            |> List.choose (fun (id, stream) ->
                match VirtualFileSystem.tryGetContent stream.Inode system.Machine.FileSystem with
                | Some (InodeContent.Directory _) -> None
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.Symlink _) ->
                    Some (UnixSystemDefect.DirectoryStreamIsNotADirectory (id, stream.Inode))
                | None -> Some (UnixSystemDefect.DanglingDirectoryStreamInode (id, stream.Inode))
            )

        let directoryStreamFreshness =
            system.Process.DirectoryStreams
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> id >= system.Process.NextDirectoryStreamId)
            |> List.map (fun id ->
                UnixSystemDefect.NextDirectoryStreamIdNotFresh (system.Process.NextDirectoryStreamId, id)
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
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _
                | OpenFileTarget.Directory _
                | OpenFileTarget.Socket _ -> []
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
                        | OpenFileTarget.StandardStream _
                        | OpenFileTarget.File _
                        | OpenFileTarget.Directory _
                        | OpenFileTarget.Socket _ ->
                            [
                                UnixSystemDefect.ParkedSocketWaitOnNonPort (task, wait.Port, description.Target)
                            ]
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

            let masks =
                SignalState.blockedTasks signals
                |> Set.toList
                |> List.filter (fun task -> not (Map.containsKey task system.Tasks))
                |> List.map UnixSystemDefect.SignalMaskWithoutTask

            let targets =
                SignalState.pending signals
                |> List.choose (fun entry ->
                    match entry.Target with
                    | ValueSome task when not (Map.containsKey task system.Tasks) ->
                        Some (UnixSystemDefect.PendingSignalTargetWithoutTask (task, entry.Signal))
                    | ValueSome _
                    | ValueNone -> None
                )

            numberings @ masks @ targets

        let fileSystemType =
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

            if EmulatedFileSystemType.isReportableUnder flavour system.Machine.FileSystemType then
                []
            else
                [
                    UnixSystemDefect.FileSystemTypeNotReportable (flavour, system.Machine.FileSystemType)
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

        dangling
        @ unreferenced
        @ freshness
        @ danglingInodes
        @ danglingStreams
        @ directoryStreamFreshness
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
    /// these bits cleared). A client chooses otherwise with
    /// `UnixProcessState.withUmask`.
    let defaultUmask : PermissionBits =
        PermissionBits.parseOrFail "UnixSystem.defaultUmask" 0o022

    /// Process ID a freshly-minted simulated process reports: 4242.
    ///
    /// Not 1, which is the ID of a PID namespace's init process. A kernel
    /// treats init specially — a signal it has not installed a handler for is
    /// not delivered to it from inside its own namespace, so `kill -9` on
    /// itself does nothing — and a default that took that branch would be
    /// modelling a container's entry point rather than an ordinary process.
    /// A client chooses otherwise with `UnixProcessState.withProcessId`.
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

    /// A simulated process on a machine of the given platform, before anything
    /// has happened to it: no sockets, no connections, an empty filesystem, and
    /// only the three standard streams open.
    ///
    /// The three fields the platform *fixes* are derived from it rather than
    /// taken as arguments — `SoMaxConn`, `FileSystemType`, and the platform
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
        let filesystem = VirtualFileSystem.empty (UnixTimestamp.ofMillisecondsSinceEpoch 0L)

        {
            Machine =
                {
                    Sockets = Map.empty
                    Connections = Map.empty
                    NextConnectionId = ConnectionId 0L
                    NextSocketEventRegistrationOrdinal = 0L
                    NextParkOrdinal = ParkOrdinal 0L
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
                    FileSystemType = EmulatedFileSystemType.defaultFor flavour
                }
            Process =
                {
                    FileDescriptors = FileDescriptorRegistry.initial
                    DirectoryStreams = Map.empty
                    NextDirectoryStreamId = DirectoryStreamId 0L
                    OutputLog = ImmutableArray<OutputLogEntry>.Empty
                    Environment = []
                    // The default current directory is the root, which every filesystem
                    // has and no operation can remove, so the pair starts consistent
                    // whatever else a host goes on to set.
                    CurrentDirectoryInode = VirtualFileSystem.root filesystem
                    ProcessPath = defaultProcessPath
                    Credentials = defaultCredentials flavour
                    Umask = defaultUmask
                    ProcessId = defaultProcessId
                    Signals = SignalState.initial (SimulatedUnixPlatform.signalNumbering platform)
                }
            Tasks = Map.empty
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
    /// handle onto the filesystem being replaced — an open descriptor or a
    /// directory stream — because the new filesystem hands out its own inode
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
    let withFileSystemAndCurrentDirectory<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (createdAt : UnixTimestamp)
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
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.SocketEventPort _
                | OpenFileTarget.Socket _ -> None
            )

        // Counted separately from the descriptions, and not because a stream
        // usually lacks one: `opendir` takes a descriptor too. It is for the
        // stream whose descriptor a guest closed out from under it, which
        // `UnixProcessState.heldInodes` documents and which a descriptions-only
        // guard would let through.
        let strandedStreams =
            system.Process.DirectoryStreams
            |> Map.toList
            |> List.map (fun (id, stream) -> $"directory stream %O{id} onto %O{stream.Inode}")

        match strandedDescriptions @ strandedStreams with
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
                    | SeedEntry.Directory (children, _) -> firstImpossibleName children
                    | SeedEntry.File _
                    | SeedEntry.Symlink _ -> None
            )

        match firstImpossibleName seed with
        | Some fault -> Error fault
        | None ->

        let filesystem = VirtualFileSystem.ofFileSystemSeed createdAt seed
        let root = VirtualFileSystem.root filesystem

        let located =
            match
                PathWalk.resolveExisting
                    limits
                    CallerPrivilege.Privileged
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
