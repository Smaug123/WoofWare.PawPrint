namespace WoofWare.PosixKernel

/// Why this kernel will not release an open file description nothing references
/// any more: what the release would do to an object the description was the
/// last reference to has not been measured.
[<RequireQualifiedAccess>]
type DescriptionReleaseRefusal =
    /// The description is the last reference to the listening socket
    /// `listener`, whose accept queue still holds `connection`, and that
    /// connection's client (socket `client`) is still open.
    | ListenerWouldResetUnacceptedClient of listener : SocketId * connection : ConnectionId * client : SocketId
    /// The description is the last reference to the connected stream socket
    /// `socket`, whose `SO_LINGER` is on with a time of zero, and its
    /// connection `connection` is still referenced: by its peer, or by a
    /// listener's accept queue.
    ///
    /// A real kernel closes such a socket abortively, resetting the connection
    /// rather than shutting it down in order, and the peer reads ECONNRESET
    /// from `SO_ERROR` (measured on both). This kernel's close resets a
    /// connection only when bytes are left unread, and otherwise sends a FIN.
    | AbortiveClose of socket : SocketId * connection : ConnectionId
    /// The description is the last reference to the connected stream socket
    /// `socket`, whose `SO_LINGER` is on with a time greater than zero, and
    /// `unsent` bytes it wrote to its connection `connection` are still in its
    /// send buffer; under Darwin, the description is also blocking.
    ///
    /// Measured on both, with the peer open and nothing left unread, a real
    /// kernel's close then waits, for up to the linger time, for those bytes
    /// to go: on Linux whether or not the description is non-blocking, on
    /// Darwin only when it is blocking. This kernel models no close that
    /// waits, and refuses whenever bytes are unsent, whatever else the close
    /// would do. With nothing unsent, or through a non-blocking description
    /// under Darwin, the close is the ordinary one.
    | LingeringClose of socket : SocketId * connection : ConnectionId * unsent : int

[<RequireQualifiedAccess>]
module DescriptionReleaseRefusal =
    /// What this kernel knows about why it cannot release the description. The
    /// client supplies its own half: which call let go of the last reference.
    let describe (refusal : DescriptionReleaseRefusal) : string =
        match refusal with
        | DescriptionReleaseRefusal.ListenerWouldResetUnacceptedClient (listener, connection, client) ->
            $"releasing the last reference destroys listening socket %O{listener} while connection %O{connection} sits unaccepted in its queue, and that connection's client (socket %O{client}) is still open. A real kernel RSTs the unaccepted client when the listener goes, leaving it in a state this kernel has not measured: its readiness level, and what connect(2) then answers, are both unknown, and it would otherwise be indistinguishable from a cleanly FIN'd peer."
        | DescriptionReleaseRefusal.AbortiveClose (socket, connection) ->
            $"releasing the last reference destroys socket %O{socket}, whose SO_LINGER is on with a time of zero, while its connection %O{connection} is still referenced. A real kernel resets the connection rather than shutting it down in order, and the peer reads ECONNRESET; this kernel models no abortive close, and its close would deliver an orderly end of stream instead unless bytes were left unread."
        | DescriptionReleaseRefusal.LingeringClose (socket, connection, unsent) ->
            $"releasing the last reference destroys socket %O{socket}, whose SO_LINGER is on with a time greater than zero, while %d{unsent} bytes it wrote to connection %O{connection} are still in its send buffer. Measured with the peer open and nothing unread, a real kernel's close then waits, for up to the linger time, for them to reach the peer -- on Linux whatever the description's O_NONBLOCK, on Darwin when the description is blocking -- and this kernel does not model that wait."

/// When this kernel frees what nothing references any more: an open file
/// description once no descriptor names it and no syscall in flight holds it,
/// the kernel objects such a description was the last reference to, and an
/// inode once nothing names or holds it.
[<RequireQualifiedAccess>]
module ObjectLifetime =

    /// Every inode that must not be freed: `UnixMachineState.heldInodes`, which
    /// holds every process's current directory as well as every open file
    /// description's inode, closed under `DirectoryContent.Parent`.
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
                // defect (`UnixSystemDefect.DanglingOpenInode`) rather than
                // something to climb from.
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.CharacterDevice _)
                | Some (InodeContent.Symlink _)
                | None -> climb rest seen

        climb (Set.toList (UnixMachineState.heldInodes system.Machine)) Set.empty

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
        // path. A process can reach here with it — `close(open("/"))` is an
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
            | Some (InodeContent.CharacterDevice _)
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

    /// Whether `SO_LINGER` could make this kernel refuse to release the last
    /// reference to `socket` (`DescriptionReleaseRefusal.AbortiveClose`,
    /// `DescriptionReleaseRefusal.LingeringClose`) in some state it can reach
    /// with no descriptor naming it: its `SO_LINGER` is on, whatever the time,
    /// and it is an end of a connection. Whether either refusal is made turns
    /// on the peer and the bytes unsent, which can change while a call holds
    /// the socket; nothing can turn the linger on, or make the socket a
    /// connection's end, without a descriptor onto it.
    let internal lingerCanRefuseRelease (socket : SocketDescription) : bool =
        socket.Options.Linger.Enabled && (SocketPhase.connectionEnd socket.Phase).IsSome

    /// Release what the open file description `destroyed` was the last
    /// reference to — its socket and the connections nothing else references,
    /// its pipe once neither end is open, its inode once nothing names or holds
    /// it — in `system`, whose open file table no longer holds the description.
    ///
    /// Destroying a connected stream socket's description closes its end of
    /// the connection (`TcpTransfer.close`): the peer gets a FIN, behind
    /// whatever the closer had sent it, or a reset if the closer left bytes
    /// unread, and its waiters the wake that raises. A close that `SO_LINGER`
    /// would make reset the connection or wait is refused
    /// (`DescriptionReleaseRefusal.AbortiveClose`,
    /// `DescriptionReleaseRefusal.LingeringClose`).
    let releaseDestroyed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (destroyed : OpenFileDescription)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixSystem<'Task, 'Handler>, DescriptionReleaseRefusal>
        =
        match destroyed.Target with
        // An epoll instance's registrations are its own state, and a kqueue
        // holds none, so neither is the last reference to anything.
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Kqueue _ -> Ok system
        | OpenFileTarget.File (inode, _)
        | OpenFileTarget.Directory (inode, _)
        | OpenFileTarget.CharacterDevice (inode, _) ->
            // The description may have been the last reference to an inode whose
            // last name went away earlier, which is what keeps `read` on an
            // unlinked descriptor working right up until the descriptor goes.
            Ok (forgetIfUnheld inode system)
        | OpenFileTarget.Pipe (pipeId, _) when
            not (Map.containsKey pipeId system.Machine.Pipes)
            && Set.isEmpty (UnixMachineState.descriptionsNamingPipeEnd pipeId PipeEnd.Read system.Machine)
            && Set.isEmpty (UnixMachineState.descriptionsNamingPipeEnd pipeId PipeEnd.Write system.Machine)
            ->
            // Both ends' descriptions were destroyed before either was
            // released, as a process's end on Linux destroys them, and the
            // other's release freed the pipe.
            Ok system
        | OpenFileTarget.Pipe (pipeId, _) ->
            // The pipe goes when neither end is open any more: it is the last
            // description onto either end that frees it, not the last onto both.
            // An end the client holds stays open whatever the process releases,
            // so a launched pipe the client drains outlives the process's last
            // reference to it. A client asleep in a write does not: once no
            // reader is left its write fails, and it closes its end.
            let pipe = UnixMachineState.pipe pipeId system.Machine
            let readable = UnixMachineState.pipeEndOpen pipeId pipe PipeEnd.Read system.Machine

            let pipe = if readable then pipe else PipeState.readEndClosed pipe

            if
                readable
                || UnixMachineState.pipeEndOpen pipeId pipe PipeEnd.Write system.Machine
            then
                Ok system
            else
                Ok
                    { system with
                        Machine =
                            { system.Machine with
                                Pipes = Map.remove pipeId system.Machine.Pipes
                            }
                    }
        | OpenFileTarget.Socket socketId ->

        let dying =
            match Map.tryFind socketId system.Machine.Sockets with
            | Some socket -> socket
            | None ->
                failwith
                    $"ObjectLifetime.releaseDestroyed: the description names socket %O{socketId}, which this system's socket table does not hold. Releasing a description is the only operation here that removes a socket, and it removes it together with the description that named it, so a live description onto an absent socket means the two tables were built out of step. There is nothing to repair it with: the objects this release would have freed cannot be found (this is a bug in this library or in whatever assembled this system)."

        let sockets = Map.remove socketId system.Machine.Sockets

        // A connection lives while any socket phase or accept queue
        // references it. The dying socket may have been the last such
        // reference — directly, or by being the listener whose queue held it
        // (the queue dies with the listener, as Linux's
        // inet_csk_listen_stop discards a closed listener's accept queue).
        let candidates =
            match dying.Phase with
            | SocketPhase.Established (connection, _)
            | SocketPhase.EstablishedPendingReport connection -> [ connection ]
            | SocketPhase.Listening listenState -> listenState.Queue
            | SocketPhase.Idle
            | SocketPhase.Refused _
            | SocketPhase.DatagramPeer _ -> []

        let stillReferenced (connection : ConnectionId) : bool =
            sockets
            |> Map.exists (fun _ survivor ->
                match survivor.Phase with
                | SocketPhase.Established (c, _)
                | SocketPhase.EstablishedPendingReport c -> c = connection
                | SocketPhase.Listening listenState -> List.contains connection listenState.Queue
                | SocketPhase.Idle
                | SocketPhase.Refused _
                | SocketPhase.DatagramPeer _ -> false
            )

        // What the release does to the dying socket's connections splits by
        // which end is dying. An established end closes its end of the
        // connection (`TcpTransfer.close`): a FIN, or a reset if bytes it had
        // not read remain; the wakes either raises are signalled below, once
        // the socket table reflects the release, so the level a wake filters
        // against is the survivor's new one. Under `SO_LINGER` {1, 0} the close
        // would be abortive, which is refused while the connection is still
        // referenced (`DescriptionReleaseRefusal.AbortiveClose`); under a
        // positive linger time it would wait for any bytes still in the send
        // buffer, which is refused where a real kernel waits
        // (`DescriptionReleaseRefusal.LingeringClose`). A dying
        // *listener* instead RSTs its unaccepted queue entries' clients, whose
        // resulting level is unmeasured -- that case refuses when a
        // registration could observe it, and an RST raises ERR, which no
        // interest mask can hide, so any registration could.
        let closing : Result<(ConnectionId * TcpWake list * TcpTransfer) option, DescriptionReleaseRefusal> =
            match SocketPhase.connectionEnd dying.Phase with
            | Some (connection, _) when
                dying.Options.Linger.Enabled
                && dying.Options.Linger.Hundredths = 0L
                && stillReferenced connection
                ->
                Error (DescriptionReleaseRefusal.AbortiveClose (socketId, connection))
            | Some (connection, connectionEnd) ->
                let transfer = (UnixMachineState.connection connection system.Machine).Transfer
                let unsent = TcpTransfer.unsent connectionEnd transfer

                // Under a positive linger time, a close with bytes still in the
                // send buffer waits for them to go (section G of
                // `tcp-shutdown.c`): on Linux whatever `O_NONBLOCK` is, and on
                // Darwin only through a blocking description, which is the
                // destroyed one, since it is the last reference to the socket.
                let waits =
                    dying.Options.Linger.Enabled
                    && dying.Options.Linger.Hundredths > 0L
                    && unsent > 0
                    && (
                        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                        | SimulatedUnixFlavour.Linux -> true
                        | SimulatedUnixFlavour.Darwin -> not destroyed.NonBlocking
                    )

                if waits then
                    Error (DescriptionReleaseRefusal.LingeringClose (socketId, connection, unsent))
                else
                    let wakes, transfer = TcpTransfer.close connectionEnd transfer
                    Ok (Some (connection, wakes, transfer))
            | None ->

            match dying.Phase with
            | SocketPhase.Listening _ ->
                // The first candidate with a live client.
                let refusal =
                    candidates
                    |> List.tryPick (fun candidate ->
                        sockets
                        |> Map.toSeq
                        |> Seq.filter (fun (_, survivor) ->
                            match survivor.Phase with
                            | SocketPhase.Established (c, _)
                            | SocketPhase.EstablishedPendingReport c -> c = candidate
                            | SocketPhase.Listening _
                            | SocketPhase.Idle
                            | SocketPhase.Refused _
                            | SocketPhase.DatagramPeer _ -> false
                        )
                        |> Seq.map fst
                        |> Seq.tryHead
                        |> Option.map (fun survivor ->
                            DescriptionReleaseRefusal.ListenerWouldResetUnacceptedClient (
                                socketId,
                                candidate,
                                survivor
                            )
                        )
                    )

                match refusal with
                | Some refusal -> Error refusal
                | None -> Ok None
            | SocketPhase.Idle
            | SocketPhase.Refused _
            | SocketPhase.DatagramPeer _
            | SocketPhase.Established _
            | SocketPhase.EstablishedPendingReport _ -> Ok None

        match closing with
        | Error refusal -> Error refusal
        | Ok closing ->

        let connections =
            match closing with
            | Some (connection, _, transfer) ->
                Map.change
                    connection
                    (Option.map (fun existing ->
                        { existing with
                            Transfer = transfer
                        }
                    ))
                    system.Machine.Connections
            | None -> system.Machine.Connections

        let connections =
            (connections, candidates)
            ||> List.fold (fun connections connection ->
                if stillReferenced connection then
                    connections
                else
                    Map.remove connection connections
            )

        let released =
            { system with
                Machine =
                    { system.Machine with
                        Sockets = sockets
                        Connections = connections
                    }
            }

        // The FIN's or the reset's edge, raised now that the survivor's level
        // is the one it leaves. An epoll registration is queued by its
        // interest and a kqueue registration activated only if its filter is
        // then ready, so a survivor nobody watches records nothing; nor does a
        // server end still queued, which has no socket yet.
        match closing with
        | Some (connection, wakes, _) -> Ok (SocketWake.signalTransfer connection wakes released)
        | None -> Ok released

    /// Destroy each of `descriptions` that nothing references any more — no
    /// descriptor names it, and no syscall in flight holds it
    /// (`OpenFileTable.holdCount`) — releasing what it was the last reference
    /// to (`releaseDestroyed`). A description still referenced, or already
    /// gone, is left as it is.
    ///
    /// What a syscall that held `descriptions` while it slept calls once it has
    /// returned, and its park has let go of the holds it took, so that a
    /// description whose last descriptor closed while it slept goes now, as a
    /// real kernel releases the file when the call drops its reference.
    let releaseUnreferenced<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (descriptions : OpenFileDescriptionId list)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<UnixSystem<'Task, 'Handler>, DescriptionReleaseRefusal>
        =
        (Ok system, List.distinct descriptions)
        ||> List.fold (fun result id ->
            match result with
            | Error refusal -> Error refusal
            | Ok system ->

            match OpenFileTable.destroyIfUnreferenced id system.Machine.OpenFiles with
            | _, None -> Ok system
            | openFiles, Some destroyed ->
                UnixSystemState.mapOpenFiles (fun _ -> openFiles) system
                |> releaseDestroyed destroyed
        )

    /// `releaseUnreferenced`, for the return of a call none of whose
    /// descriptions can be the last reference to a socket whose release can be
    /// refused: failing loudly, naming `caller`, if one is.
    ///
    /// A socket's release is refused only for a listener, which of the calls
    /// that hold a description only `accept` can leave as its last reference
    /// (a Linux `poll` watching one keeps a descriptor onto it,
    /// `CloseRefusal.PolledDescriptor`), and under `SO_LINGER`, which no
    /// description only calls hold has (`UnixSystemDefect.LingeringSocketHeldOnlyByCalls`).
    /// `accept` calls `releaseUnreferenced` instead.
    let internal releaseUnreferencedUnrefusable<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (caller : string)
        (descriptions : OpenFileDescriptionId list)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        match releaseUnreferenced descriptions system with
        | Ok released -> released
        | Error refusal ->
            failwith
                $"%s{caller}: releasing the open file descriptions %A{descriptions} as the call returned was refused, but this call holds none that can be the last reference to a socket whose release can be refused (this is a bug in this library): %s{DescriptionReleaseRefusal.describe refusal}"
