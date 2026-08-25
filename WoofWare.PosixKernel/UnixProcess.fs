namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// The state one POSIX process owns: what it inherited at exec, where it is, who
/// it is running as, and every kernel object its descriptors and streams name.
///
/// Distinct from `UnixMachineState`, which is true of the machine whatever process
/// is running: a second process on the same simulated kernel would have its own
/// copy of this and share that. PawPrint models one process, so there is one of
/// these, but the split is what makes `fork(2)` expressible later rather than a
/// rewrite.
type UnixProcessState<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    {
        /// In-memory model of the simulated process's Unix file descriptor
        /// table. Pre-seeded at startup with stdin (0), stdout (1), stderr
        /// (2), matching the kernel's behaviour of populating these slots
        /// at `exec` time. SystemNative_Dup / Close / Read / Write etc.
        /// route through this table; the host's real fds are never used.
        FileDescriptors : FileDescriptorRegistry
        /// Ordered, append-only log of every write the guest has performed
        /// against a writable standard stream via `SystemNative_Write`.
        /// Each entry carries the destination `Role` and the exact byte
        /// payload of that one call (chunks are not coalesced; ordering
        /// across roles is preserved). Acts as the canonical record the
        /// driver's end-of-run host drain reads from, and is what
        /// PawPrint-only tests assert on instead of trying to capture host
        /// stdout. The log grows unboundedly: a guest that prints
        /// gigabytes will pay the memory cost, but PawPrint is a slow
        /// deterministic interpreter and a guest of that scale is not in
        /// scope. Bound this with a streaming sink (consuming `StepEffect.
        /// WroteToFd` at each step) when a need arises.
        ///
        /// The single ordered log (rather than per-stream buffers)
        /// preserves cross-stream ordering: a guest that writes
        /// `err1, out1, err2` is replayed in that order under `2>&1`,
        /// matching real-CLR behaviour. Per-stream views are derived in
        /// `OutputLogEntry.bytesFor`.
        OutputLog : ImmutableArray<OutputLogEntry>
        /// Simulated process environment variable table, and the analogue of the
        /// Unix PAL's `palEnvironment` — which is likewise a snapshot taken once
        /// at startup rather than a view of the host, because libc's `setenv` is
        /// not usable concurrently. Consulted by
        /// `Environment.GetEnvironmentVariable` through the Win32
        /// `GetEnvironmentVariableW` shim, and flattened into an environment
        /// block by the `GetEnvironmentStringsW` shim that backs
        /// `Environment.GetEnvironmentVariables`. Seeded with
        /// `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` so guest BCL code that
        /// reads it during startup gets the invariant-globalization mode
        /// PawPrint requires; the CLI overlays the host process's env on top
        /// of this default at startup, and tests can pass their own overlay
        /// via `Program.run`.
        ///
        /// No guest can write to this: PawPrint services no
        /// `SetEnvironmentVariableW`, so `Environment.SetEnvironmentVariable`
        /// aborts loudly rather than mutating the table.
        ///
        /// Every name here is one a real process could hold: non-empty, free of
        /// `=`, and free of NUL, as is every value. `UnixProcessState
        /// .withEnvironment` — the only way an entry enters the table — rejects
        /// anything else, so readers may rely on it. See
        /// `environmentEntryProblem` for why those are exactly the expressible
        /// names.
        ///
        /// That invariant is what makes `GetEnvironmentVariableW`'s plain
        /// `Map.tryFind` faithful without reproducing the PAL's own two
        /// name guards: for a name the PAL would refuse, the lookup misses and
        /// reports `ERROR_ENVVAR_NOT_FOUND`, which is exactly what the PAL
        /// returns on that path.
        Environment : Map<string, string>
        /// The simulated process's current working directory, as observed
        /// through `SystemNative_GetCwd` — and hence through
        /// `Environment.CurrentDirectory` and every relative
        /// `Path.GetFullPath` on a Unix CoreLib.
        ///
        /// Like `UnixPlatform`, CoreLib does *not* latch this during static
        /// initialisation (`Interop.Sys.GetCwd()` is called afresh on every
        /// read), but hosts should still set it via `KernelConfig` rather than
        /// by record-copy after startup: PawPrint models no `chdir(2)`, so
        /// within a run the cwd is immutable and a guest must not be able to
        /// observe it changing under it.
        ///
        /// The **physical** path: every symlink resolved away, which is what
        /// `getcwd(3)` reports and so not necessarily the spelling
        /// `KernelConfig.CurrentDirectory` used. Derived from
        /// `CurrentDirectoryInode` when the kernel is built, so the two cannot
        /// describe a process no Unix could produce.
        CurrentDirectory : AbsoluteUnixPath
        /// The directory relative paths resolve against: the inode the
        /// simulated process holds its current directory *open on*, which is
        /// what a real process holds rather than a name it re-walks.
        ///
        /// Derived when the kernel is built, by the one setter that takes the
        /// current directory and the filesystem together — so this is not a
        /// second, independent knob a host may set. It is nonetheless the
        /// *identity* half of the pair, and the two answer
        /// different questions once a guest can delete a directory: this one
        /// says where a relative path starts, and `CurrentDirectory` says what
        /// the process would be told if it asked. A real kernel splits them the
        /// same way, which is why `getcwd` can fail while relative lookups
        /// still work.
        ///
        /// Holding the inode is also what makes the resolution of a relative
        /// path *not* a lookup: no component of the current directory's own
        /// path is walked, so none of its permission bits are consulted, and no
        /// intermediate symlink is re-traversed. Measured on both kernels: with
        /// the cwd at `outer/inner` and `outer` unsearchable, a relative
        /// `lstat("target")` succeeds while `lstat("../inner/target")` is
        /// EACCES.
        CurrentDirectoryInode : InodeNumber
        /// Path to the executable that started the simulated process, as
        /// observed through `SystemNative_GetProcessPath` and hence
        /// `Environment.ProcessPath`.
        ///
        /// `None` is an *answer*, not a request for a default: it says this
        /// process has no executable path, which the entry point reports the way
        /// both Unix flavours do — a null return with errno `ENOENT`. That is
        /// the truth about a PawPrint guest by default, because PawPrint models
        /// no `exec(2)`: nothing started this process from a file, and the
        /// emulated filesystem contains no image of it. Contrast
        /// `FileSystemType`, whose `None` *does* mean "derive one in `applyTo`".
        ///
        /// Not resolved against `FileSystem`. Real `realpath` succeeds only if
        /// every component resolves, so a host that wants
        /// `File.Exists(Environment.ProcessPath)` to hold must seed the file
        /// itself; see docs/divergences.md. The same is already true of
        /// `CurrentDirectory`.
        ///
        /// CoreLib latches this on first read — `Environment.ProcessPath` caches
        /// under an `Interlocked.CompareExchange` — so hosts must set it via
        /// `KernelConfig` rather than by record-copy after startup.
        ProcessPath : AbsoluteUnixPath option
        /// Every directory stream `SystemNative_OpenDir` has handed out and
        /// `SystemNative_CloseDir` has not yet reclaimed, under the id minted for
        /// it. `DirectoryStreamBlocks` is what turns a guest's `DIR*` into one of
        /// these ids.
        ///
        /// A stream is *not* a descriptor kind. Measured on both kernels,
        /// `opendir` consumes a file descriptor — an `open` either side of one
        /// returned fds 3 and 5 — so the descriptor is an ordinary
        /// `OpenFileTarget.File` on the directory, which is what pins the inode
        /// through `heldInodes` and so makes a stream over an `rmdir`'d
        /// directory behave. What cannot live there is the rest: the cursor and
        /// the name buffer have no home in `File (inode, offset)`.
        ///
        /// An absent key is not a default and must never be read as one. Every id
        /// `DirectoryStreamBlocks` names is present here — `checkInvariants`
        /// enforces that as `DirectoryStreamBlockDangling` — so an absent one is
        /// an interpreter bug rather than anything a guest did, and
        /// `directoryStream` says so loudly rather than inventing an errno, the
        /// way `UnixMachineState.connection` does for a `ConnectionId`.
        DirectoryStreams : Map<DirectoryStreamId, DirectoryStream>
        /// The id `withNewDirectoryStream` will hand out next.
        NextDirectoryStreamId : DirectoryStreamId
        /// The effective user ID the simulated process runs as, reported by
        /// `stat` as every inode's `st_uid` and by `SystemNative_GetEUid`.
        ///
        /// Process-wide rather than per-inode: no managed caller can change a
        /// file's owner, because `SystemNative_ChOwn` does not exist anywhere in
        /// the runtime's interop surface, so a per-inode field could never make
        /// two inodes differ and would carry no information this does not.
        UserId : uint32
        /// The effective group ID, reported as every inode's `st_gid`. See
        /// `UserId`.
        GroupId : uint32
        /// The simulated process's file-mode creation mask: the permission bits
        /// `open(O_CREAT)` clears from the mode its caller asked for.
        ///
        /// Process state rather than filesystem state, and immutable for the
        /// whole run: CoreLib's interop surface has no `SystemNative_UMask` at
        /// all, so no guest can read or change it, and a host that wants to
        /// replay a differently-masked process sets it once through
        /// `KernelConfig`.
        ///
        /// Deliberately *not* consulted for seed entries. A seed describes a
        /// tree that some other process built, so this run's mask has no bearing
        /// on it; `PermissionBits.defaultForRegularFile` shares the same 0o022
        /// literal but is not derived from this field, so raising the mask
        /// cannot silently change what an unannotated seed entry means.
        Umask : PermissionBits
        /// Pure data model of the simulated process's signal disposition,
        /// per-thread sigprocmasks, and pending-signal queue. Populated by
        /// future slices: nothing in the simulator dispatches signals yet,
        /// so the field stays at `SignalState.empty` across every run today.
        /// Held on `EmulatedKernel` (rather than per-thread) because POSIX
        /// signal disposition is process-wide; the per-thread piece lives
        /// inside `SignalState.Blocked`.
        Signals : SignalState<'Task, 'Handler>
    }

[<RequireQualifiedAccess>]
module UnixProcessState =

    /// Why `name`/`value` could not be a variable of a real process, or `None` if
    /// it could. The string describes the problem for a caller to prefix with its
    /// own context. Total: a null name or value is itself one of the answers,
    /// rather than something this dereferences.
    ///
    /// A real process's environment is not a name-to-value map at all: it is a
    /// list of `name=value` strings, and the map every environment API presents
    /// is a *view* of that list, obtained by splitting each entry at its first
    /// `=`. CoreCLR makes that view total by refusing, in
    /// `GetEnvironmentVariableA` (`pal/src/misc/environ.cpp`), to look up a name
    /// that is empty or contains `=`; `Environment.GetEnvironmentVariables`
    /// likewise discards any entry whose first `=` is not after the first
    /// character. So the set of names the view can ever produce is exactly the
    /// non-empty, `=`-free ones, and a NUL cannot occur at all because the
    /// entries are C strings.
    ///
    /// PawPrint stores the map rather than the list, which is the more convenient
    /// representation but admits names that view could never yield. Such a name
    /// has no consistent behaviour to model: measured against real .NET, an
    /// inherited entry `A=B=C` is the variable `A` with value `B=C`, and looking
    /// up `A=B` returns null — so a PawPrint table holding the key `A=B` would
    /// have to answer that lookup both ways at once. Rejecting the table is what
    /// keeps the two environment APIs in agreement with each other and with the
    /// real runtime.
    ///
    /// Shared with the `GetEnvironmentStringsW` shim, which flattens the map back
    /// into a list and so re-checks; keeping one copy of the rule is what stops
    /// the two disagreeing about which tables are legal.
    let environmentEntryProblem (name : string) (value : string) : string option =
        // Null first, and as its own case rather than lumped in with the empty
        // name. `Map<string, string>` holds a null key or value quite happily —
        // F#'s comparer sorts null first, and a consumer of this package writing
        // C# has nothing stopping it — so this function would otherwise dereference
        // null and abort a run with a bare NullReferenceException, which is the
        // opposite of what a validating classifier is for. Same reason
        // `AbsoluteUnixPath.assertValid` exists.
        if isNull name then
            Some "a variable whose name is null, which is not a string an environment list could hold"
        elif isNull value then
            // `name` is known non-null by now, so it is safe to name the offender.
            Some $"a variable (%s{name}) whose value is null, which is not a string an environment list could hold"
        elif name = "" then
            Some
                "a variable with an empty name, which no environment list can express (the entry would read `=value`, which every reader discards)"
        elif name.Contains '=' then
            Some
                $"a variable whose name contains '=' (%s{name}), which no environment list can express unambiguously: a reader splits at the first '=', so it would see a different name and value"
        elif name.Contains (char 0) then
            Some $"a variable whose name contains a NUL code unit (%s{name}), which would terminate its entry early"
        elif value.Contains (char 0) then
            Some $"a variable (%s{name}) whose value contains a NUL code unit, which would terminate its entry early"
        else
            None

    /// Set the path to the executable that started the simulated process, or
    /// `None` to report that it has none. `None` is preserved rather than
    /// defaulted; see `UnixProcessState.ProcessPath`.
    ///
    /// `context` prefixes the rejection a forged path earns, and is the client's
    /// to choose: the host that has to fix one knows it by whatever name the
    /// client's own configuration gives it, not by this field's.
    let withProcessPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (path : AbsoluteUnixPath option)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        { proc with
            ProcessPath = path |> Option.map (AbsoluteUnixPath.assertValid context)
        }

    /// Set the file-mode creation mask `open(O_CREAT)` clears from the mode its
    /// caller asked for. See `UnixProcessState.Umask` for why this is the only way
    /// to set it, and why a seed entry is not subject to it.
    ///
    /// `context` prefixes the rejection a forged mask earns; see
    /// `withProcessPath` for why the client supplies it.
    let withUmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (umask : PermissionBits)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        { proc with
            Umask = PermissionBits.assertValid context umask
        }

    /// Set the effective user and group IDs the simulated process runs as.
    let withUserAndGroupId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (userId : uint32)
        (groupId : uint32)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        { proc with
            UserId = userId
            GroupId = groupId
        }

    /// Whether the simulated process is exempt from the permission rules a kernel
    /// applies to everyone else: uid 0, and nothing else.
    ///
    /// One definition rather than a comparison at each site, because the sites
    /// answer *different* questions from the same fact — whether `open` may ignore
    /// a mode that forbids the access it was asked for, and whether a write keeps
    /// a file's set-user-ID bits — and they must not be able to drift apart about
    /// who root is. `CallerPrivilege` rather than a `bool` for the same reason:
    /// the answer travels through several signatures before it is used, and a
    /// bare flag arrives at them saying nothing about which fact it is.
    ///
    /// A client should think before defaulting `UserId` to 0. .NET's
    /// `Environment.IsPrivilegedProcess` is literally `GetEUid() == 0`, so a
    /// guest run as root skips its own privilege guards; that is why
    /// `EmulatedKernel.defaultUserId` is 1000.
    let callerPrivilege<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : CallerPrivilege
        =
        if proc.UserId = 0u then
            CallerPrivilege.Privileged
        else
            CallerPrivilege.Unprivileged

    /// Overlay `env` onto the environment the simulated process already holds:
    /// a name in both takes its value from `env`, and one only the process holds
    /// survives. An overlay rather than a replacement so that a host can set the
    /// variables it cares about without having to restate whatever its own
    /// startup seeded.
    ///
    /// Refuses, loudly, an entry no real environment list could express — see
    /// `environmentEntryProblem` for which those are. Rejecting rather than
    /// dropping, because a variable that silently failed to arrive would show up
    /// as the guest taking a different branch much later.
    ///
    /// `context` prefixes that rejection; see `withProcessPath` for why the
    /// client supplies it.
    let withEnvironment<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (env : Map<string, string>)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        for KeyValue (name, value) in env do
            match environmentEntryProblem name value with
            | None -> ()
            | Some problem -> failwith $"%s{context}: refusing to install %s{problem}."

        let merged =
            (proc.Environment, env)
            ||> Map.fold (fun acc key value -> Map.add key value acc)

        { proc with
            Environment = merged
        }

    /// Every live open file description naming `socketId`.
    let descriptionsNamingSocket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (proc : UnixProcessState<'Task, 'Handler>)
        : Set<OpenFileDescriptionId>
        =
        FileDescriptorRegistry.descriptions proc.FileDescriptors
        |> Map.toSeq
        |> Seq.choose (fun (descriptionId, description) ->
            match description.Target with
            | OpenFileTarget.Socket target when target = socketId -> Some descriptionId
            | _ -> None
        )
        |> Set.ofSeq

    /// Whether any socket event port holds a registration targeting an open
    /// file description that names `socketId`.
    ///
    /// This is what makes a readiness change on the socket *observable*: the
    /// client's `closeFd` consults it before destroying the peer of an
    /// established pair, because the survivor's level would change to one this
    /// kernel cannot represent, and with no registration there is nothing that
    /// could deliver the difference.
    let socketIsRegisteredWithAnyEventPort<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (proc : UnixProcessState<'Task, 'Handler>)
        : bool
        =
        let namingDescriptions = descriptionsNamingSocket socketId proc

        FileDescriptorRegistry.descriptions proc.FileDescriptors
        |> Map.exists (fun _ description ->
            match description.Target with
            | OpenFileTarget.SocketEventPort portState ->
                portState.Registrations
                |> Map.exists (fun (_, targetId) _ -> Set.contains targetId namingDescriptions)
            | _ -> false
        )

    /// A *state-change* wake on `socketId` — a connect resolving (completion
    /// or refusal), the refusal delivery's reset, a peer's FIN. Unkeyed:
    /// measured (`order8.c`, `order9.c`), such a wake queues every
    /// registration regardless of interest, the entry keeps the wake's
    /// position through a later interest change, and delivery's re-poll does
    /// the filtering.
    let signalSocketStateChange<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        { proc with
            FileDescriptors =
                FileDescriptorRegistry.signalSocketEventPorts
                    (descriptionsNamingSocket socketId proc)
                    None
                    proc.FileDescriptors
        }

    /// Every inode this kernel holds a reference to *directly*, independently of
    /// any name the filesystem binds to it.
    ///
    /// A real kernel keeps an inode alive while any reference survives; this
    /// enumerates the references this record holds of its own. Every live open file
    /// description onto a file is one, and so is the current directory — a
    /// process that has `chdir`ed somewhere keeps that directory alive whether
    /// or not its name outlives the call.
    ///
    /// Everything that can *create* a reference must appear here: an omission
    /// makes a live inode look free, and freeing it leaves a descriptor pointing
    /// at nothing. It is not what callers want, though — see the client's
    /// `pinnedInodes`, which adds the references the *filesystem* holds on
    /// behalf of these.
    let heldInodes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : Set<InodeNumber>
        =
        proc.FileDescriptors
        |> FileDescriptorRegistry.descriptions
        |> Map.toSeq
        |> Seq.choose (fun (_, description) ->
            match description.Target with
            | OpenFileTarget.File (inode, _) -> Some inode
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.Socket _
            | OpenFileTarget.SocketEventPort _ -> None
        )
        |> Set.ofSeq
        |> Set.add proc.CurrentDirectoryInode
        // An open directory stream holds its directory too. The descriptor it
        // opened already does, so this adds nothing while the stream is intact
        // — it is here for the guest that closes that descriptor out from under
        // the stream, which is undefined behaviour on a real libc but a
        // guessable fd number away here. Without it the next `readdir` would
        // reach a reaped inode and crash the client.
        |> Set.union (
            proc.DirectoryStreams
            |> Map.toSeq
            |> Seq.map (fun (_, stream) -> stream.Inode)
            |> Set.ofSeq
        )
