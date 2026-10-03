namespace WoofWare.PosixKernel

/// <summary>
/// The state one POSIX process owns.
/// </summary>
/// <remarks>
/// Contains, for example: what it inherited at exec, where it is, who
/// it is running as, and every kernel object its descriptors name.
///
/// Distinct from <c>UnixMachineState</c>, which describes the process-independent
/// state of the kernel.
/// A second process on the same simulated kernel would have its own
/// copy of <c>UnixProcessState</c> but would share <c>UnixMachineState</c>.
/// </remarks>
type UnixProcessState<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            /// In-memory model of the simulated process's Unix file descriptor
            /// table. Seeded at startup from the launch table `UnixSystem.initial`
            /// takes, as a real process inherits the descriptors its launcher set
            /// up before `exec`. Every descriptor operation this library models
            /// routes through this table; the host's real fds are never used.
            FileDescriptors : FileDescriptorRegistry
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
            /// `UnixBootImage.withFileSystemAndCurrentDirectory`.
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
            /// `UnixBootImage.withMount`, whose `None` *does* mean "derive one from
            /// the flavour".
            ///
            /// Not resolved against `FileSystem`. Real `realpath` succeeds only if
            /// every component resolves, so a client that wants the path to name a
            /// file must seed the file itself.
            ProcessPath : AbsoluteUnixPath option
            /// Who the simulated process is: its real, effective and saved user and
            /// group IDs, and its supplementary groups.
            ///
            /// Changing them changes nothing an inode records: an inode's owner is
            /// its own (`Inode.Owner`), fixed when it was created or seeded.
            Credentials : Credentials
            /// The simulated process's file-mode creation mask: the permission bits
            /// `open(O_CREAT)` and `mkdir(2)` clear from the mode their caller asked
            /// for, applied in full.
            ///
            /// Process state rather than filesystem state, and shared by every
            /// thread. Held at the width the platform's `umask(2)` stores
            /// (`SimulatedUnixPlatform.umaskStoredBits`): never above 0o777 on Linux.
            /// The process replaces it with `UnixSystem.umask`, and a client sets
            /// the one it starts with using `UnixBootImage.withUmask`; both keep it at
            /// that width.
            ///
            /// Deliberately *not* consulted for seed entries. A seed describes a
            /// tree that some other process built, so this run's mask has no bearing
            /// on it; `SeedEntry.defaultPermsForRegularFile` shares the same 0o022
            /// literal but is not derived from this field, so raising the mask
            /// cannot silently change what an unannotated seed entry means.
            Umask : PermissionBits
            /// The ID `getpid(2)` reports for the simulated process.
            ///
            /// Fixed for the whole run: a process keeps its ID from `fork` to exit.
            /// `UnixBootImage.withProcessId` sets it before the process has created a
            /// thread.
            ProcessId : ProcessId
            /// Pure data model of the simulated process's signal dispositions,
            /// each thread's handler frames, and pending-signal queue.
            /// Held on the process (rather than per-thread) because POSIX
            /// signal disposition is process-wide; the per-thread pieces, each
            /// thread's handler frames and own pending set, live inside it too.
            Signals : SignalState<'Task, 'Handler>
            /// Whether the process writes a core dump when a signal whose default
            /// action dumps core kills it. Fixed for the whole run: this library
            /// models no `setrlimit(2)`; a client sets it once with
            /// `UnixBootImage.withCoreDumps`.
            CoreDumps : CoreDumps
        }

[<RequireQualifiedAccess>]
module UnixProcessState =

    /// Whether the simulated process is exempt from the permission rules a kernel
    /// applies to everyone else. This is `Credentials.privilege` of its
    /// credentials, which is where the rule is stated.
    ///
    /// `CallerPrivilege` rather than a `bool` because the answer travels through
    /// several signatures before it is used, and a bare flag arrives at them
    /// saying nothing about which fact it is.
    ///
    /// A client should think before making a process root: root passes every
    /// permission check this kernel models (on Darwin, the few whose answer for
    /// root has not been measured are refused instead), and programs commonly
    /// skip their own guards when they find they are root. That is why
    /// `UnixSystem.defaultUserId` is not 0.
    let callerPrivilege<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : CallerPrivilege
        =
        Credentials.privilege proc.Credentials

    /// The environment the process was started with, entry by entry, in order.
    /// See `UnixProcessState.Environment`.
    let environment<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixByteString list
        =
        proc.Environment

    /// The path of the executable that started the process, or `None` if it has
    /// none. See `UnixProcessState.ProcessPath`.
    let processPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : AbsoluteUnixPath option
        =
        proc.ProcessPath

    /// The process's signal state, which `SignalState`'s queries read.
    ///
    /// For a client that runs the process's signal handlers, or checks its
    /// invariants. A process sees parts of this through `sigaction(2)` and the
    /// handlers it runs; this is the whole of it, every task's handler frames
    /// and every pending signal included.
    let signals<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (proc : UnixProcessState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        proc.Signals

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

    /// Every live open file description naming `pipeEnd` of `pipeId`.
    let descriptionsNamingPipeEnd<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (pipeEnd : PipeEnd)
        (proc : UnixProcessState<'Task, 'Handler>)
        : Set<OpenFileDescriptionId>
        =
        FileDescriptorRegistry.descriptions proc.FileDescriptors
        |> Map.toSeq
        |> Seq.choose (fun (descriptionId, description) ->
            if description.Target = OpenFileTarget.Pipe (pipeId, pipeEnd) then
                Some descriptionId
            else
                None
        )
        |> Set.ofSeq

    /// Whether `pipeEnd` of the pipe `pipeId`, which is `pipe`, is still open:
    /// whether some open file description names it, or the client holds it
    /// (`PipeState.heldByClient`).
    ///
    /// Derived rather than stored, so it cannot disagree with the table: the
    /// end closes when the last description onto it goes, which is when its
    /// last descriptor closes, or when a call that held it returns after that,
    /// unless the client holds it; and `dup` keeps it open.
    let pipeEndOpen<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (pipe : PipeState)
        (pipeEnd : PipeEnd)
        (proc : UnixProcessState<'Task, 'Handler>)
        : bool
        =
        PipeState.heldByClient pipeEnd pipe
        || FileDescriptorRegistry.descriptions proc.FileDescriptors
           |> Map.exists (fun _ description -> description.Target = OpenFileTarget.Pipe (pipeId, pipeEnd))

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
    /// `ObjectLifetime.pinnedInodes`, which adds the references the *filesystem*
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
            | OpenFileTarget.Directory (inode, _)
            | OpenFileTarget.CharacterDevice (inode, _) -> Some inode
            | OpenFileTarget.Socket _
            | OpenFileTarget.Kqueue _
            | OpenFileTarget.Epoll _
            | OpenFileTarget.Pipe _ -> None
        )
        |> Set.ofSeq
        |> Set.add proc.CurrentDirectoryInode
