namespace WoofWare.PawPrint

/// Which of the simulated process's inherited standard streams an open file
/// description refers to. Also the routing key for `EmulatedKernel.OutputLog`
/// and `StepEffect.WroteToFd`, so it names a stream rather than a file: the
/// host is what ultimately receives stdout/stderr bytes.
[<RequireQualifiedAccess>]
type FileDescriptorRole =
    | StandardInput
    | StandardOutput
    | StandardError

/// Identity of an open file description. Never guest-visible: no modelled
/// syscall reports one (Linux's `kcmp(2)`, which would, is not modelled), so
/// this exists purely to let two file descriptors denote the *same* open file
/// description rather than two equal copies of one.
[<Struct>]
type OpenFileDescriptionId =
    | OpenFileDescriptionId of value : int64

    override this.ToString () : string =
        match this with
        | OpenFileDescriptionId value -> string<int64> value

/// The kernel object a file descriptor points at: POSIX's "open file
/// description". Everything shared between file descriptors that `dup(2)`
/// produced belongs here.
///
/// **`File` carries an inode and nothing else, deliberately.** The two pieces
/// of state a real open file description also holds are absent because no
/// modelled syscall can yet make them differ:
///
///  - The **file offset**. `pread(2)` — the only reader CoreLib's
///    `RandomAccess` uses on a seekable handle — takes its offset as an
///    argument and leaves the description's alone, so an offset stored here
///    would be written once at `open` and never read. It becomes real with
///    `SystemNative_LSeek` and `SystemNative_Read`, which is where it belongs.
///  - The **access mode and status flags** (`O_APPEND`, `O_NONBLOCK`). PawPrint
///    refuses every write flag at `open` today, so the mode has exactly one
///    inhabitant, and a field with one inhabitant records a decision nothing
///    made. It becomes real with the write path.
///
/// Inodes, by contrast, are load-bearing immediately: an fd must keep naming
/// the file it was opened on, not the path it was opened by.
///
/// The standard streams carry no offset either, and for a different reason:
/// PawPrint models them as pipes (see `FileDescriptorRegistry.initial`), and a
/// pipe is not seekable at all.
[<RequireQualifiedAccess>]
type OpenFileDescription =
    | StandardStream of FileDescriptorRole
    /// A regular file, directory, or anything else `open(2)` returned a
    /// descriptor for, identified by the inode it resolved to at open time.
    /// Not by path: renaming or deleting the path leaves this description
    /// naming the same file, which is what a real kernel does.
    | File of inode : InodeNumber

/// In-memory model of a Unix per-process file descriptor table, and of the
/// open file descriptions those descriptors point at.
///
/// The indirection is POSIX's, not an implementation detail: a file descriptor
/// is a per-process integer *naming* an open file description, and `dup(2)`
/// allocates a fresh descriptor pointing at the same description. State that
/// belongs to the description (offset, status flags) is therefore shared by
/// every descriptor that names it, while the per-descriptor flags — `FD_CLOEXEC`,
/// to which POSIX-2024 adds `FD_CLOFORK` — are not. PawPrint models neither
/// per-descriptor flag, because it models neither `fork` nor `exec`.
///
/// Beware that the descriptor/description split does not exhaust kernel state.
/// `fcntl(2)` record locks — which CoreLib reaches through
/// `SystemNative_LockFileRegion`, and hence `FileStream.Lock`, on the Linux
/// platform PawPrint simulates — are associated with a *(process, file)* pair:
/// closing *any* descriptor for that file drops them, even one whose
/// description another live descriptor still shares. (Measured on macOS: with
/// `b = dup a`, a lock taken via `a` was released by `close b`.) `flock(2)`
/// locks, by contrast, do belong to the description. When locking lands, it
/// must not be hung on this table without deciding which of the two it is.
type FileDescriptorRegistry =
    private
        {
            /// The per-process descriptor table: which description each live
            /// file descriptor names.
            Fds : Map<int, OpenFileDescriptionId>
            /// The open file descriptions themselves. A description is live
            /// exactly while some descriptor in `Fds` names it — there is no
            /// stored reference count to drift, and PawPrint models none of
            /// the references that would make liveness more than reachability
            /// (`SCM_RIGHTS` descriptor passing, `mmap`).
            Descriptions : Map<OpenFileDescriptionId, OpenFileDescription>
            /// The identity the next `open` will allocate. Stored and
            /// monotonic rather than derived as one past the highest live id,
            /// which would reuse the identity of a description that has been
            /// closed. Nothing guest-visible could tell the difference — the
            /// id is never reported by any syscall — but a replay trace could,
            /// and "the same id names two different files at two different
            /// times" is exactly the ambiguity a time-travel debugger must not
            /// have. `VirtualFileSystem.NextInode` is stored for the stronger
            /// version of this reason, inode reuse being guest-visible.
            NextId : OpenFileDescriptionId
        }

[<RequireQualifiedAccess>]
type FileDescriptorDupError =
    /// The supplied fd is not a live entry in the table. `dup(2)` reports
    /// this as `EBADF`; the SystemNative_Dup handler translates this into
    /// a -1 return and `LastSystemError = EBADF`.
    | BadFd

[<RequireQualifiedAccess>]
type FileDescriptorCloseError =
    /// The supplied fd is not a live entry in the table. `close(2)` reports
    /// this as `EBADF`.
    | BadFd

/// A way in which a `FileDescriptorRegistry` fails to be a descriptor table any
/// kernel could produce. `FileDescriptorRegistry.checkInvariants` returns these.
[<RequireQualifiedAccess>]
type FileDescriptorRegistryDefect =
    /// A live descriptor names a description that is not present. Every lookup
    /// through this descriptor would fail, which no kernel permits.
    | DanglingFd of fd : int * description : OpenFileDescriptionId
    /// A description survives that no descriptor names. The kernel destroys a
    /// description when its last descriptor closes, so this is a leak.
    | UnreferencedDescription of description : OpenFileDescriptionId
    /// A live description's identity is at or above the next one to allocate,
    /// so some future `open` would collide with it — silently retargeting
    /// every descriptor that named it. Note "at or above" rather than "equal
    /// to": a cursor *below* a live id is just as unsound, it merely takes a
    /// few more opens to do the damage. `VirtualFileSystem`'s
    /// `NextInodeNotFresh` is the same check for the same reason.
    | NextIdNotFresh of nextId : OpenFileDescriptionId * existing : OpenFileDescriptionId

[<RequireQualifiedAccess>]
module FileDescriptorRegistry =
    let private stdinId : OpenFileDescriptionId = OpenFileDescriptionId 0L
    let private stdoutId : OpenFileDescriptionId = OpenFileDescriptionId 1L
    let private stderrId : OpenFileDescriptionId = OpenFileDescriptionId 2L

    /// Descriptor table as the simulated process inherits it at `exec` time:
    /// stdin (fd 0), stdout (fd 1), stderr (fd 2).
    ///
    /// The three descriptors name three *distinct* descriptions, which models a
    /// process launched with each standard stream separately redirected — the
    /// shape `RealRuntime` itself uses when it launches a guest on real .NET as
    /// PawPrint's differential oracle, giving it three separate pipes.
    ///
    /// This is not the only shape a real process can inherit, and deliberately
    /// not the terminal one. Under a tty, fds 0/1/2 are `dup`s of a *single*
    /// `O_RDWR` description: measured via `forkpty`, setting `O_NONBLOCK`
    /// through fd 1 becomes visible on fds 0 and 2, and `write(0, _, _)`
    /// succeeds. PawPrint has already committed against that model elsewhere —
    /// `SystemNative_IsATty` always reports 0, and `SystemNative_Write` to fd 0
    /// returns `EBADF`, which is true only of a redirected `O_RDONLY` stdin.
    /// Seeding one shared description here would contradict both.
    let initial : FileDescriptorRegistry =
        {
            Fds = Map.empty |> Map.add 0 stdinId |> Map.add 1 stdoutId |> Map.add 2 stderrId
            Descriptions =
                Map.empty
                |> Map.add stdinId (OpenFileDescription.StandardStream FileDescriptorRole.StandardInput)
                |> Map.add stdoutId (OpenFileDescription.StandardStream FileDescriptorRole.StandardOutput)
                |> Map.add stderrId (OpenFileDescription.StandardStream FileDescriptorRole.StandardError)
            NextId = OpenFileDescriptionId 3L
        }

    /// Which description `fd` names, if `fd` is live. Callers that need to know
    /// whether two descriptors share a description — rather than merely name
    /// equal ones — must compare these rather than the payloads.
    let tryFindId (fd : int) (registry : FileDescriptorRegistry) : OpenFileDescriptionId option =
        Map.tryFind fd registry.Fds

    /// The description `fd` names, if `fd` is live.
    let tryFind (fd : int) (registry : FileDescriptorRegistry) : OpenFileDescription option =
        Map.tryFind fd registry.Fds
        |> Option.map (fun id ->
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                // `checkInvariants` calls this a `DanglingFd`; reaching it
                // through a lookup means the table was mutated by something
                // other than this module's operations.
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"
        )

    /// Every live file descriptor, and the description each names.
    let fds (registry : FileDescriptorRegistry) : Map<int, OpenFileDescriptionId> = registry.Fds

    /// Every live open file description.
    let descriptions (registry : FileDescriptorRegistry) : Map<OpenFileDescriptionId, OpenFileDescription> =
        registry.Descriptions

    /// Lowest non-negative integer not currently used as a file descriptor.
    /// O(n) in the number of live fds, which is fine: process fd tables are
    /// small (typically a handful, rarely more than a few hundred), and the
    /// interpreter is not a performance-critical workload.
    let private lowestFree (fds : Map<int, OpenFileDescriptionId>) : int =
        let rec scan (candidate : int) =
            if Map.containsKey candidate fds then
                scan (candidate + 1)
            else
                candidate

        scan 0

    /// Mirrors `dup(2)`: allocate the lowest non-negative fd not in use, naming
    /// the *same* open file description as `oldFd`. No new description is
    /// created, so any state the description carries (once it carries any) is
    /// shared with `oldFd` rather than copied — which is the whole point of the
    /// indirection. When `oldFd` is not a live entry, returns `Error BadFd`,
    /// matching the `EBADF` behaviour of `dup(2)`.
    let dup
        (oldFd : int)
        (registry : FileDescriptorRegistry)
        : Result<int * FileDescriptorRegistry, FileDescriptorDupError>
        =
        match Map.tryFind oldFd registry.Fds with
        | None -> Error FileDescriptorDupError.BadFd
        | Some id ->
            let newFd = lowestFree registry.Fds

            Ok (
                newFd,
                { registry with
                    Fds = Map.add newFd id registry.Fds
                }
            )

    /// Remove a descriptor from the table, destroying the description it named
    /// if that was the last descriptor naming it. Mirrors `close(2)`: returns
    /// `Error BadFd` (= `EBADF`) when `fd` is not currently live.
    ///
    /// Closing one descriptor of a `dup` pair leaves the other's description
    /// intact — true of everything PawPrint models, though not of POSIX in
    /// general (see the record-lock note on `FileDescriptorRegistry`).
    ///
    /// Wired into the interpreter via the `SystemNative_Close` handler in
    /// `NativeSystemNative.fs`; the in-house property tests drive close+dup
    /// cycles directly against this function to exercise the `lowestFree`
    /// invariant against the gap structure that close produces.
    let close
        (fd : int)
        (registry : FileDescriptorRegistry)
        : Result<FileDescriptorRegistry, FileDescriptorCloseError>
        =
        match Map.tryFind fd registry.Fds with
        | None -> Error FileDescriptorCloseError.BadFd
        | Some id ->
            let fds = Map.remove fd registry.Fds

            let stillNamed =
                fds |> Map.exists (fun _ (other : OpenFileDescriptionId) -> other = id)

            Ok
                { registry with
                    Fds = fds
                    Descriptions =
                        if stillNamed then
                            registry.Descriptions
                        else
                            Map.remove id registry.Descriptions
                }

    /// Mirrors the descriptor half of `open(2)`: allocate a *fresh* open file
    /// description naming `inode`, and the lowest non-negative descriptor not
    /// in use to point at it.
    ///
    /// Fresh, unlike `dup`: two `open` calls on one path give two descriptions,
    /// which is why they can hold separate offsets and separate `flock` locks.
    /// Sharing here would make the second open silently alias the first.
    ///
    /// Total — there is no failure mode at this level. Whether the path
    /// resolves, whether the flags are ones PawPrint honours, and whether the
    /// process may open the file at all are decided before this is reached; a
    /// real kernel's `EMFILE`/`ENFILE` would belong here, but PawPrint models
    /// no descriptor limit (`RLIMIT_NOFILE` is not in the interop surface).
    let openFile (inode : InodeNumber) (registry : FileDescriptorRegistry) : int * FileDescriptorRegistry =
        let id = registry.NextId
        let (OpenFileDescriptionId raw) = id
        let fd = lowestFree registry.Fds

        fd,
        { registry with
            Fds = Map.add fd id registry.Fds
            Descriptions = Map.add id (OpenFileDescription.File inode) registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// Every way in which `registry` fails to be a descriptor table a kernel
    /// could produce. Empty for any registry built out of `initial`, `dup` and
    /// `close`; the property tests assert exactly that.
    let checkInvariants (registry : FileDescriptorRegistry) : FileDescriptorRegistryDefect list =
        let dangling =
            registry.Fds
            |> Map.toList
            |> List.filter (fun (_, id) -> not (Map.containsKey id registry.Descriptions))
            |> List.map FileDescriptorRegistryDefect.DanglingFd

        let named = registry.Fds |> Map.toList |> List.map snd |> Set.ofList

        let unreferenced =
            registry.Descriptions
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> not (Set.contains id named))
            |> List.map FileDescriptorRegistryDefect.UnreferencedDescription

        let freshness =
            registry.Descriptions
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> id >= registry.NextId)
            |> List.map (fun id -> FileDescriptorRegistryDefect.NextIdNotFresh (registry.NextId, id))

        dangling @ unreferenced @ freshness

    /// Fail loudly if `registry` is not sound, naming `context`.
    let assertInvariants (context : string) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        match checkInvariants registry with
        | [] -> registry
        | defects ->
            let rendered = defects |> List.map (sprintf "%A") |> String.concat "; "

            failwith $"%s{context}: the file descriptor table is not one any kernel could produce: %s{rendered}"

    /// Construction that bypasses every invariant this module maintains.
    ///
    /// Exists so that `checkInvariants` can be tested: a defect no test can
    /// construct is documentation rather than a check. Deliberately one
    /// greppable token, so that any interpreter code reaching for it is visible
    /// in review — nothing outside tests should.
    [<RequireQualifiedAccess>]
    module Unchecked =
        let ofParts
            (fds : Map<int, OpenFileDescriptionId>)
            (descriptions : Map<OpenFileDescriptionId, OpenFileDescription>)
            (nextId : OpenFileDescriptionId)
            : FileDescriptorRegistry
            =
            {
                Fds = fds
                Descriptions = descriptions
                NextId = nextId
            }
