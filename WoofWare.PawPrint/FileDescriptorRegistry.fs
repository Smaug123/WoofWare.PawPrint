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

/// What an open file description refers to — the kernel object on the far side
/// of the descriptor.
///
/// **`File` carries an inode and nothing else, deliberately.** An inode is
/// load-bearing immediately: an fd must keep naming the file it was opened on,
/// not the path it was opened by.
[<RequireQualifiedAccess>]
type OpenFileObject =
    | StandardStream of FileDescriptorRole
    /// A regular file, directory, or anything else `open(2)` returned a
    /// descriptor for, identified by the inode it resolved to at open time.
    /// Not by path: renaming or deleting the path leaves this description
    /// naming the same file, which is what a real kernel does.
    | File of inode : InodeNumber

/// The mode of an advisory whole-file lock taken by `flock(2)`.
///
/// Two modes rather than three: "no lock" is the *absence* of one of these
/// (`OpenFileDescription.Flock` is an option), because a description holding no
/// lock and a description holding a hypothetical `Unlocked` mode would be the
/// same state spelled two ways.
[<RequireQualifiedAccess>]
type FlockMode =
    /// `LOCK_SH`. Any number of descriptions may hold this on one file at once.
    | Shared
    /// `LOCK_EX`. Excludes every other description's lock on the same file,
    /// shared or exclusive.
    | Exclusive

/// What an open file description refers to, together with the state that only
/// that kind of object carries.
///
/// **Distinct from `OpenFileObject`, which is the *identity*.** This type is
/// the description's view of that object, and the two differ by exactly the
/// file offset: two descriptions at different offsets are positioned
/// differently on the *same* file, and `flock` must still see them as
/// contending. Folding the offset into `OpenFileObject` would break that —
/// the conflict test compares objects for equality, so two offsets on one
/// inode would stop excluding each other. `OpenFileDescription.object` is the
/// projection back to identity.
///
/// The offset lives in the `File` case rather than beside it because a standard
/// stream has no offset to hold: PawPrint models the standard streams as pipes
/// (see `FileDescriptorRegistry.initial`), and a pipe is not seekable at all —
/// `lseek` on one is `ESPIPE` and `read` from one consumes a queue rather than a
/// position. A flat `Offset` field would have to lie for a third of the
/// inhabitants; this way "a standard stream at offset 7" cannot be written down.
[<RequireQualifiedAccess>]
type OpenFileTarget =
    /// One of the inherited standard streams. No offset: not seekable.
    | StandardStream of role : FileDescriptorRole
    /// A regular file or directory, and where in it this description is
    /// positioned. `read(2)` consumes from here and advances it; `lseek(2)`
    /// sets it; `pread(2)` deliberately leaves it alone, which is the whole
    /// reason it exists as a separate syscall.
    ///
    /// A real kernel permits an offset arbitrarily far past the end of the
    /// file (`lseek` beyond EOF is how sparse files are made), so this is not
    /// bounded by the file's length — only by being non-negative, which
    /// `VirtualFileSystem.seekTarget` enforces.
    | File of inode : InodeNumber * offset : int64

/// The kernel object a file descriptor points at: POSIX's "open file
/// description". Everything shared between file descriptors that `dup(2)`
/// produced belongs here.
///
/// One piece of state a real open file description also holds is absent,
/// because no modelled syscall can yet make it differ: the **access mode and
/// status flags** (`O_APPEND`, `O_NONBLOCK`). PawPrint refuses every write flag
/// at `open` today, so the mode has exactly one inhabitant, and a field with one
/// inhabitant records a decision nothing made. It becomes real with the write
/// path.
type OpenFileDescription =
    {
        /// What this description refers to, and where in it.
        Target : OpenFileTarget
        /// The `flock(2)` lock this description holds, if any.
        ///
        /// **On the description, not on the inode**, which is where POSIX puts
        /// it and is the whole reason two `open(2)` calls on one path contend
        /// while a `dup(2)` pair does not. Storing it here also keeps it
        /// normalised: the description already records which object it names,
        /// so there is no second copy of that association to drift out of step,
        /// and closing the description destroys the lock with it rather than
        /// leaving a phantom entry in a side table that nothing can now release.
        ///
        /// Note this is `flock(2)` specifically. `fcntl(2)` record locks — which
        /// CoreLib reaches through `SystemNative_LockFileRegion`, and hence
        /// `FileStream.Lock` — belong to a *(process, file)* pair instead, and
        /// so must not be stored here when they land; see the note on
        /// `FileDescriptorRegistry`.
        Flock : FlockMode option
    }

[<RequireQualifiedAccess>]
module OpenFileDescription =
    /// Which kernel object this description names — its *identity*, with the
    /// per-description position discarded.
    ///
    /// This is what `flock(2)` contention is decided on: two descriptions
    /// contend exactly when they name the same object, whatever offsets they
    /// happen to be at. Callers asking "are these the same file?" must compare
    /// these rather than the descriptions.
    let object (description : OpenFileDescription) : OpenFileObject =
        match description.Target with
        | OpenFileTarget.StandardStream role -> OpenFileObject.StandardStream role
        | OpenFileTarget.File (inode, _) -> OpenFileObject.File inode

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
/// locks, by contrast, do belong to the description, and so live in
/// `OpenFileDescription.Flock`. A record lock must *not* join them there when
/// `SystemNative_LockFileRegion` lands: it would inherit the wrong release rule.
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

/// What `flock(2)` was asked to do, once the operation bits have been decoded.
///
/// `LOCK_NB` is deliberately *not* part of this: whether the caller is willing
/// to block is a property of the request that only matters once the answer is
/// "this would block", so it is the handler's business rather than the
/// registry's. The registry reports that the lock is unavailable and lets the
/// caller decide between failing and waiting.
[<RequireQualifiedAccess>]
type FlockRequest =
    /// `LOCK_SH` or `LOCK_EX`. Replaces whatever lock this description already
    /// held, which is how `flock(2)` spells conversion — there is no separate
    /// upgrade operation.
    | Acquire of mode : FlockMode
    /// `LOCK_UN`. Succeeds whether or not a lock was held, as `flock(2)` does.
    | Release

[<RequireQualifiedAccess>]
type FlockError =
    /// The supplied fd is not a live entry in the table; `EBADF`.
    | BadFd
    /// Another open file description holds a conflicting lock on the same file.
    /// A caller that passed `LOCK_NB` reports this as `EWOULDBLOCK`; one that
    /// did not would have to wait for the holder to release.
    | WouldBlock

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
    /// A description is positioned at a negative file offset. No kernel permits
    /// one: `lseek(2)` rejects a computation landing below zero with `EINVAL`
    /// rather than clamping, and `read(2)` never moves the offset backwards.
    ///
    /// Unlike the offset's *upper* end, which is unbounded on purpose — seeking
    /// arbitrarily far past EOF is legal, and is how sparse files are made — so
    /// there is no matching "too large" defect to pair this with.
    | NegativeOffset of description : OpenFileDescriptionId * offset : int64
    /// Two distinct descriptions name the same file and hold locks that
    /// `flock(2)` would never have granted together — at least one of them
    /// exclusive. This is the mutual-exclusion property itself rather than a
    /// bookkeeping check, so it is what a property test over random
    /// open/lock/close sequences is really asserting.
    | ConflictingFlocks of first : OpenFileDescriptionId * second : OpenFileDescriptionId

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
                let stream (role : FileDescriptorRole) : OpenFileDescription =
                    {
                        Target = OpenFileTarget.StandardStream role
                        Flock = None
                    }

                Map.empty
                |> Map.add stdinId (stream FileDescriptorRole.StandardInput)
                |> Map.add stdoutId (stream FileDescriptorRole.StandardOutput)
                |> Map.add stderrId (stream FileDescriptorRole.StandardError)
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

    /// What `fd` refers to, if `fd` is live. For the majority of callers, which
    /// want the file behind the descriptor and have no interest in the state the
    /// description carries alongside it.
    ///
    /// Note this discards the offset, so it is the wrong lookup for `read(2)`
    /// and `lseek(2)`; they want `tryFindTarget`.
    let tryFindObject (fd : int) (registry : FileDescriptorRegistry) : OpenFileObject option =
        tryFind fd registry |> Option.map OpenFileDescription.object

    /// What `fd` refers to and where in it, if `fd` is live. For the callers
    /// that move or consume the file offset.
    let tryFindTarget (fd : int) (registry : FileDescriptorRegistry) : OpenFileTarget option =
        tryFind fd registry |> Option.map (fun description -> description.Target)

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
    /// The offset starts at 0, which is `open(2)`'s answer for every flag
    /// PawPrint accepts. `O_APPEND` would start it at the end instead, and is
    /// refused at the handler — see `SystemNative_Open`, which refuses every
    /// write flag outright.
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
            Descriptions =
                Map.add
                    id
                    {
                        Target = OpenFileTarget.File (inode, 0L)
                        // `open(2)` never takes a lock; `FileStream` issues a
                        // separate `flock` immediately afterwards, which is
                        // exactly why `FileShare` is not atomic with opening on
                        // Unix and CoreLib's own comment says so.
                        Flock = None
                    }
                    registry.Descriptions
            NextId = OpenFileDescriptionId (raw + 1L)
        }

    /// May two *different* open file descriptions on one file hold these two
    /// locks at the same time?
    ///
    /// Shared against shared is the only compatible pair; every other
    /// combination involves an exclusive lock, which by definition excludes.
    /// Symmetric, which is what lets `checkInvariants` apply it to an unordered
    /// pair without asking which was taken first.
    let private locksConflict (a : FlockMode) (b : FlockMode) : bool =
        match a, b with
        | FlockMode.Shared, FlockMode.Shared -> false
        | _, _ -> true

    /// Mirrors `flock(2)`.
    ///
    /// The lock belongs to the open file description `fd` names, so two
    /// descriptors from one `dup(2)` share a single lock (releasing through
    /// either releases it), while two separate `open(2)` calls on one path hold
    /// two and therefore contend. That contention is the entire mechanism behind
    /// `FileShare` on Unix, and it works *within* one process — which is why a
    /// single-threaded guest can observe it at all.
    ///
    /// Contention is between descriptions naming the same `OpenFileObject`. For
    /// a standard stream that set is empty by construction — `initial` gives
    /// each role exactly one description and `dup` shares rather than copies —
    /// so `flock` on fd 0/1/2 succeeds and conflicts with nothing. That is what
    /// Linux does (measured: `flock` on a pipe returns 0), and it falls out of
    /// the general rule rather than being a special case.
    ///
    /// **This is Linux's mechanism, and it is flavour-agnostic on purpose.**
    /// Darwin diverges in three measured ways — it answers `ENOTSUP` for a pipe,
    /// it validates the operation differently, and it *keeps* a lock that a
    /// failed conversion would drop here. None of those live in this module:
    /// deciding what a Darwin-flavoured kernel does is the handler's job, and it
    /// currently refuses rather than modelling it (see `SystemNative_FLock` in
    /// `NativeSystemNative.fs`). Keeping the divergence out of here means this
    /// type stays a single coherent set of rules rather than two interleaved
    /// ones.
    ///
    /// `Acquire` replaces any lock this description already held, so a
    /// conversion cannot conflict with itself: `SH` to `EX` succeeds when this
    /// description is the only holder, and reports `WouldBlock` when another
    /// still holds `SH`.
    ///
    /// **A failed conversion still drops the old lock**, which is why this
    /// returns a table even on failure rather than an untouched one. `flock(2)`
    /// converts by removing the existing lock and then establishing the new one,
    /// and those two steps are not atomic — so when the second fails, the first
    /// has already happened and the caller is left holding nothing. That is the
    /// documented BSD-derived behaviour, and it is measured: with `a` and `b`
    /// both holding `SH`, a failed `a: SH -> EX` leaves `a` unlocked on Linux
    /// (a third description can then take `EX` once `b` releases) but still
    /// holding `SH` on Darwin. PawPrint simulates Linux. Note the *error* is the
    /// same on both platforms, so only a third description can tell them apart,
    /// which is what the test for this uses.
    ///
    /// `Release` succeeds whether or not a lock was held.
    let flock
        (fd : int)
        (request : FlockRequest)
        (registry : FileDescriptorRegistry)
        : FileDescriptorRegistry * FlockError option
        =
        match Map.tryFind fd registry.Fds with
        | None -> registry, Some FlockError.BadFd
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"

        let withFlock (flock : FlockMode option) : FileDescriptorRegistry =
            { registry with
                Descriptions =
                    Map.add
                        id
                        { description with
                            Flock = flock
                        }
                        registry.Descriptions
            }

        match request with
        | FlockRequest.Release -> withFlock None, None
        | FlockRequest.Acquire mode ->

        let blocked =
            registry.Descriptions
            |> Map.exists (fun otherId (other : OpenFileDescription) ->
                // `otherId <> id` is what makes conversion work: this
                // description's own lock is not an obstacle to replacing it.
                otherId <> id
                // Identity, not the whole description: two descriptions on one
                // file contend however far apart their offsets are.
                && OpenFileDescription.object other = OpenFileDescription.object description
                && (
                    match other.Flock with
                    | None -> false
                    | Some held -> locksConflict mode held
                )
            )

        if blocked then
            // The old lock is gone either way — see the note above. A caller
            // that held nothing is unaffected, so this is not a special case.
            withFlock None, Some FlockError.WouldBlock
        else
            withFlock (Some mode), None

    /// Move the file offset of the description `fd` names.
    ///
    /// Total in the offset — every non-negative `int64` is a position a real
    /// kernel would accept, including far past the end of the file — and
    /// deliberately *partial* in the descriptor: reaching this with an fd that
    /// is not live, or one naming an unseekable object, is an interpreter bug
    /// rather than a guest error. Both callers (`SystemNative_LSeek` and
    /// `SystemNative_Read`) have already resolved the description and rejected
    /// `EBADF`/`ESPIPE` before they get here, so a silent no-op would hide the
    /// bug that a crash names.
    ///
    /// Deciding *which* offset is not this module's business: `lseek`'s
    /// arithmetic needs the file's size, which lives in the filesystem, and its
    /// error vocabulary differs by platform. `VirtualFileSystem.seekTarget`
    /// computes the target and this stores it.
    let setOffset (fd : int) (offset : int64) (registry : FileDescriptorRegistry) : FileDescriptorRegistry =
        if offset < 0L then
            failwith
                $"setOffset: fd %d{fd} was asked to move to offset %d{offset}, which is negative. No kernel permits a negative file offset; the caller must reject this as EINVAL before storing it (this is an interpreter bug)."

        match Map.tryFind fd registry.Fds with
        | None ->
            failwith
                $"setOffset: fd %d{fd} is not a live file descriptor, so there is no offset to move (this is an interpreter bug: the caller should have answered EBADF)."
        | Some id ->

        let description =
            match Map.tryFind id registry.Descriptions with
            | Some description -> description
            | None ->
                failwith
                    $"file descriptor %d{fd} names open file description %O{id}, which is not present in the table (this is an interpreter bug)"

        match description.Target with
        | OpenFileTarget.StandardStream role ->
            failwith
                $"setOffset: fd %d{fd} names standard stream %O{role}, which PawPrint models as a pipe and so has no file offset (this is an interpreter bug: the caller should have answered ESPIPE)."
        | OpenFileTarget.File (inode, _) ->

        { registry with
            Descriptions =
                Map.add
                    id
                    { description with
                        Target = OpenFileTarget.File (inode, offset)
                    }
                    registry.Descriptions
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

        // Every unordered pair of distinct locked descriptions naming one file.
        // Quadratic in the number of live descriptions, which is a handful; the
        // clarity is worth more here than the asymptotics, since this is the one
        // check that states the actual `flock` guarantee.
        let negativeOffsets =
            registry.Descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                match description.Target with
                | OpenFileTarget.StandardStream _ -> None
                | OpenFileTarget.File (_, offset) ->
                    if offset < 0L then
                        Some (FileDescriptorRegistryDefect.NegativeOffset (id, offset))
                    else
                        None
            )

        let locked =
            registry.Descriptions
            |> Map.toList
            |> List.choose (fun (id, description) ->
                description.Flock
                |> Option.map (fun mode -> id, OpenFileDescription.object description, mode)
            )

        let conflicting =
            locked
            |> List.collect (fun (firstId, firstObject, firstMode) ->
                locked
                |> List.filter (fun (secondId, secondObject, secondMode) ->
                    firstId < secondId
                    && firstObject = secondObject
                    && locksConflict firstMode secondMode
                )
                |> List.map (fun (secondId, _, _) ->
                    FileDescriptorRegistryDefect.ConflictingFlocks (firstId, secondId)
                )
            )

        dangling @ unreferenced @ freshness @ negativeOffsets @ conflicting

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
