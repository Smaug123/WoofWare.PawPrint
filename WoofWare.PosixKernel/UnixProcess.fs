namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// <summary>
/// The state one POSIX process owns.
/// </summary>
/// <remarks>
/// Contains, for example: what it inherited at exec, where it is, who
/// it is running as, and every kernel object its descriptors and streams name.
///
/// Distinct from <c>UnixMachineState</c>, which describes the process-independent
/// state of the kernel.
/// A second process on the same simulated kernel would have its own
/// copy of <c>UnixProcessState</c> but would share <c>UnixMachineState</c>.
/// </remarks>
type UnixProcessState<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    {
        /// In-memory model of the simulated process's Unix file descriptor
        /// table. Pre-seeded at startup with stdin (0), stdout (1), stderr
        /// (2), matching the kernel's behaviour of populating these slots
        /// at `exec` time. Every descriptor operation this library models
        /// routes through this table; the host's real fds are never used.
        FileDescriptors : FileDescriptorRegistry
        /// Ordered, append-only log of every write the process has performed
        /// against a writable standard stream via `UnixReadWrite.write`.
        /// Each entry carries the destination `Role` and the exact byte
        /// payload of that one call (chunks are not coalesced; ordering
        /// across roles is preserved). It is the canonical record of what the
        /// process wrote to its standard streams, for a client to drain to
        /// wherever those streams really go. The log grows unboundedly: a
        /// process that prints gigabytes will pay the memory cost.
        ///
        /// The single ordered log (rather than per-stream buffers)
        /// preserves cross-stream ordering: a process that writes
        /// `err1, out1, err2` is replayed in that order under `2>&1`,
        /// as it would be on a real kernel. Per-stream views are derived in
        /// `OutputLogEntry.bytesFor`.
        OutputLog : ImmutableArray<OutputLogEntry>
        /// The environment the process was started with: the `envp` that
        /// `execve(2)` received, one entry per element, in order.
        ///
        /// An entry is conventionally `NAME=VALUE`, but the kernel does not parse
        /// it. An entry with no `=`, an entry beginning with `=`, and two entries
        /// naming the same variable are all held exactly as given; the only
        /// constraint is that of any C string, that it contains no NUL.
        ///
        /// This is the process's exec-time image, not libc's `environ`:
        /// `setenv(3)` and `putenv(3)` change the process's own copy in user
        /// space, and nothing here models them.
        Environment : UnixByteString list
        /// The directory the simulated process is standing in: the inode it
        /// holds its current directory *open on*, which is what a real process
        /// holds rather than a name it re-walks.
        ///
        /// This is the whole of the process's current directory. The *path* —
        /// what `getcwd(3)` reports — is derived from this
        /// inode and the filesystem by `UnixPathResolution.currentDirectoryPath`, and is
        /// not stored: a path is a fact about the directory graph, which a
        /// `rename` of any ancestor rewrites, and a second copy could only go
        /// stale. That derivation is also what makes the path the **physical**
        /// one, every symlink resolved away, which is what `getcwd(3)` reports
        /// and so not necessarily the spelling a client passed to
        /// `UnixSystem.withFileSystemAndCurrentDirectory`.
        ///
        /// Derived when the kernel is built, by the one setter that takes the
        /// current directory and the filesystem together — so this is not a
        /// knob a client may set on its own.
        ///
        /// Once a process can delete a directory, the inode outliving its own path
        /// is an ordinary state rather than a broken one: relative lookups keep
        /// working from here while `getcwd` has nothing to answer. A real kernel
        /// splits the two the same way.
        ///
        /// Holding the inode is also what makes the resolution of a relative
        /// path *not* a lookup: no component of the current directory's own
        /// path is walked, so none of its permission bits are consulted, and no
        /// intermediate symlink is re-traversed. Measured on both kernels: with
        /// the cwd at `outer/inner` and `outer` unsearchable, a relative
        /// `lstat("target")` succeeds while `lstat("../inner/target")` is
        /// EACCES.
        CurrentDirectoryInode : InodeNumber
        /// Path to the executable that started the simulated process.
        ///
        /// `None` is an *answer*, not a request for a default: it says this
        /// process has no executable path, which the entry point reports the way
        /// both Unix flavours do — a null return with errno `ENOENT`. That is
        /// the truth about a simulated process by default, because this library
        /// models no `exec(2)`: nothing started this process from a file, and the
        /// emulated filesystem contains no image of it. Contrast
        /// `UnixMachineState.withMount`, whose `None` *does* mean "derive one from
        /// the flavour".
        ///
        /// Not resolved against `FileSystem`. Real `realpath` succeeds only if
        /// every component resolves, so a client that wants the path to name a
        /// file must seed the file itself.
        ProcessPath : AbsoluteUnixPath option
        /// Who the simulated process is: its real, effective and saved user and
        /// group IDs, and its supplementary groups.
        ///
        /// `stat` reports the effective IDs as every inode's `st_uid` and
        /// `st_gid`, because this library stores no per-inode ownership yet.
        Credentials : Credentials
        /// The simulated process's file-mode creation mask: the permission bits
        /// `open(O_CREAT)` clears from the mode its caller asked for.
        ///
        /// Process state rather than filesystem state, and immutable for the
        /// whole run: this library models no `umask(2)`, so the process cannot
        /// read or change it, and a client that wants a differently-masked
        /// process sets it once with `UnixProcessState.withUmask`.
        ///
        /// Deliberately *not* consulted for seed entries. A seed describes a
        /// tree that some other process built, so this run's mask has no bearing
        /// on it; `SeedEntry.defaultPermsForRegularFile` shares the same 0o022
        /// literal but is not derived from this field, so raising the mask
        /// cannot silently change what an unannotated seed entry means.
        Umask : PermissionBits
        /// The ID `getpid(2)` reports for the simulated process.
        ///
        /// Fixed for the whole run: a process keeps its ID from `fork` to exit,
        /// and nothing here models either end.
        ProcessId : ProcessId
        /// Pure data model of the simulated process's signal disposition,
        /// per-thread sigprocmasks, and pending-signal queue.
        /// Held on the process (rather than per-thread) because POSIX
        /// signal disposition is process-wide; the per-thread piece lives
        /// inside `SignalState.Blocked`.
        Signals : SignalState<'Task, 'Handler>
    }

[<RequireQualifiedAccess>]
module UnixProcessState =

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

    /// Set the ID `getpid(2)` reports for the simulated process.
    ///
    /// `context` prefixes the rejection a forged ID earns; see
    /// `withProcessPath` for why the client supplies it.
    let withProcessId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (pid : ProcessId)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        { proc with
            ProcessId = ProcessId.assertValid context pid
        }

    /// Whether the simulated process is exempt from the permission rules a kernel
    /// applies to everyone else. This is `Credentials.privilege` of its
    /// credentials, which is where the rule is stated.
    ///
    /// `CallerPrivilege` rather than a `bool` because the answer travels through
    /// several signatures before it is used, and a bare flag arrives at them
    /// saying nothing about which fact it is.
    ///
    /// A client should think before making a process root: root passes every
    /// permission check this kernel models, and programs commonly skip their own
    /// guards when they find they are root. That is why
    /// `UnixSystem.defaultUserId` is not 0.
    let callerPrivilege<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : CallerPrivilege
        =
        Credentials.privilege proc.Credentials

    /// Set the environment the simulated process was started with, replacing
    /// whatever it held. The entries are kept in the order given, duplicates and
    /// all; see `UnixProcessState.Environment`.
    ///
    /// `context` prefixes the rejection a forged entry earns; see
    /// `withProcessPath` for why the client supplies it.
    let withEnvironment<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (env : UnixByteString list)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixProcessState<'Task, 'Handler>
        =
        { proc with
            Environment = env |> List.map (UnixByteString.assertValid context)
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
    /// at nothing. It is not what callers want, though — see
    /// `UnixDescriptor.pinnedInodes`, which adds the references the *filesystem*
    /// holds on behalf of these.
    let heldInodes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : Set<InodeNumber>
        =
        proc.FileDescriptors
        |> FileDescriptorRegistry.descriptions
        |> Map.toSeq
        |> Seq.choose (fun (_, description) ->
            match description.Target with
            | OpenFileTarget.File (inode, _)
            | OpenFileTarget.Directory (inode, _) -> Some inode
            | OpenFileTarget.StandardStream _
            | OpenFileTarget.Socket _
            | OpenFileTarget.SocketEventPort _ -> None
        )
        |> Set.ofSeq
        |> Set.add proc.CurrentDirectoryInode
