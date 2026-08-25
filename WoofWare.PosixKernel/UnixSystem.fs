namespace WoofWare.PosixKernel

/// Everything one simulated POSIX process is, as a syscall sees it: the machine
/// it runs on, its own per-process state, and its tasks.
///
/// Generic in what names a task and what a signal handler is, for the same
/// reason `SignalState` is: those are the client's identities and this library
/// never learns them.
type UnixSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    {
        Machine : UnixMachineState
        Process : UnixProcessState<'Task, 'Handler>
        Tasks : Map<'Task, UnixTaskState>
    }

/// Which of the two non-portable `lseek` extensions a raw `whence` names on the
/// simulated platform. Linux numbers `SEEK_DATA` 3 and `SEEK_HOLE` 4; Darwin
/// transposes them, which is why the raw number alone names no operation.
[<RequireQualifiedAccess>]
type SeekExtension =
    | SeekData
    | SeekHole

/// Why this kernel will not answer an `lseek`.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type LSeekRefusal =
    /// `SEEK_DATA` or `SEEK_HOLE` on a seekable file.
    | Sparseness of whence : int * meaning : SeekExtension
    /// `SEEK_END` on a directory.
    | DirectoryEnd of inode : InodeNumber

/// Why this kernel will not answer a syscall at all. The client decides what a
/// refusal means for it; nothing here is recoverable by retrying.
[<RequireQualifiedAccess>]
type SyscallRefusal = | LSeek of LSeekRefusal

[<RequireQualifiedAccess>]
module LSeekRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic. The client supplies its own half — which entry point, which
    /// descriptor — because those are things it decoded and this library never
    /// saw.
    let describe (refusal : LSeekRefusal) : string =
        match refusal with
        | LSeekRefusal.Sparseness (whence, meaning) ->
            let named =
                match meaning with
                | SeekExtension.SeekData -> "SEEK_DATA"
                | SeekExtension.SeekHole -> "SEEK_HOLE"

            $"whence %d{whence} is %s{named} on the simulated platform. This kernel models file contents as a byte array with no notion of sparseness, so it cannot say where the data and holes are; and the two platforms transpose the numbers (3 is SEEK_DATA on Linux and SEEK_HOLE on Darwin), so the raw value does not name one operation."
        | LSeekRefusal.DirectoryEnd inode ->
            $"inode %O{inode} is a directory, and was asked to seek relative to its end. A directory's size is a filesystem artefact rather than a fact about its contents, and there is no portable answer: measured, lseek(dir, 0, SEEK_END) is EINVAL on Linux/tmpfs, 4096 on Linux/ext4 and 64 on macOS/APFS. SEEK_SET and SEEK_CUR on a directory are portable and are supported."

/// A request to this kernel, in the vocabulary of the kernel ABI rather than of
/// any client's foreign-function layer.
///
/// Arguments a real kernel validates arrive raw — `LSeek`'s `whence`, and every
/// `fd` — because rejecting them is behaviour this library models, and models
/// per flavour. Arguments only the client can classify arrive classified.
type Syscall =
    | GetEffectiveUserId
    | Dup of fd : int
    | LSeek of fd : int * offset : int64 * whence : int

/// What the entry point returns, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type SyscallAnswer =
    /// The entry point returns this.
    | Completed of answer : int64
    /// The entry point returns its failure sentinel, and the client stores
    /// `error` wherever its libc keeps errno.
    ///
    /// A failure still changes the system in general: `flock` advances the
    /// descriptor table before it can discover the conflict that fails it.
    | Failed of error : UnixError

/// Why a file descriptor cannot be seeked, as a *fault* rather than as the errno
/// it becomes.
///
/// Not a `UnixError`, because `lseek` orders the two faults differently per
/// flavour: measured, Linux validates `whence` between them while Darwin does
/// not, so an ordering written over errnos would let a future third fault
/// inherit whichever position its errno's arm happened to occupy.
[<RequireQualifiedAccess>]
type private DescriptorFault =
    /// No such descriptor in the process's table; `EBADF`. Precedes everything
    /// else on both platforms.
    | NotOpen
    /// The descriptor names something with no file offset — a pipe, which is
    /// what this kernel models the standard streams as; `ESPIPE`.
    | NotSeekable

[<RequireQualifiedAccess>]
module UnixSystem =

    /// The effective user ID, as `geteuid(2)` reports it.
    ///
    /// Total, and changes nothing: `geteuid` cannot fail, and this library
    /// models one identity for the whole process.
    let effectiveUserId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : uint32
        =
        system.Process.UserId

    /// `dup(2)`: the lowest non-negative descriptor not in use, sharing `fd`'s
    /// open file description. EBADF is its only failure.
    let dup<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match FileDescriptorRegistry.dup fd system.Process.FileDescriptors with
        | Ok (newFd, registry) ->
            SyscallAnswer.Completed (int64 newFd),
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        | Error FileDescriptorDupError.BadFd -> SyscallAnswer.Failed UnixError.EBADF, system

    /// `lseek(2)`: move `fd`'s file offset and report where it lands.
    ///
    /// Refuses the two inputs for which real kernels have been measured to
    /// disagree without a portable answer; see `LSeekRefusal`.
    let lseek<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (offset : int64)
        (whence : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, LSeekRefusal>
        =
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        // `Interop.Sys.SeekWhence` (Interop.LSeek.cs), which is also POSIX's
        // numbering and both platforms' `<unistd.h>` — for these three. It
        // stops here; 3 and 4 are handled below and are *not* portable.
        let seekSet = 0
        let seekCur = 1
        let seekEnd = 2
        let seekMax = 4

        // The two orderings below are measured, and this is the syscall where
        // they differ most. On a single-fault input the platforms agree on
        // every row; they part company on two:
        //
        //   input                       Linux    Darwin
        //   pipe + whence 99            EINVAL   ESPIPE
        //   pipe + whence 99 + overflow EINVAL   ESPIPE
        //
        // So Linux validates `whence` before it asks whether the object is
        // seekable, and Darwin the other way round. The descriptor itself
        // precedes both on either platform — `lseek(badfd, ..)` is EBADF for
        // every whence and offset measured, including 99, 3, 4 and INT64_MAX —
        // and the offset arithmetic follows both, pinned by
        // `lseek(pipe, -1, SEEK_SET)` = ESPIPE on both (seekability first) and
        // `lseek(f, 1, 99)` from INT64_MAX = EINVAL on both (whence first).
        let whenceValid = whence >= seekSet && whence <= seekMax

        let target = FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors

        let descriptorFault : DescriptorFault option =
            match target with
            | None -> Some DescriptorFault.NotOpen
            | Some (OpenFileTarget.StandardStream _) ->
                // Not seekable: this kernel models the standard streams as
                // pipes, and `lseek` on a pipe is ESPIPE on both platforms
                // whichever end it is. This is the answer `SafeFileHandle` reads
                // back to decide `CanSeek`, so it is on the BCL's own path
                // rather than a corner.
                Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.SocketEventPort _) ->
                // The one target whose *seekability* depends on the platform,
                // rather than merely the errno or the ordering. Measured: Darwin
                // refuses `lseek` on a kqueue with ESPIPE, while Linux gives an
                // epoll descriptor `noop_llseek`, which succeeds and reports 0
                // without consulting the offset or moving anything. So Darwin
                // has a descriptor fault here and Linux has none; the Linux
                // success is served below, after the whence check the syscall
                // still applies.
                match flavour with
                | SimulatedUnixFlavour.Darwin -> Some DescriptorFault.NotSeekable
                | SimulatedUnixFlavour.Linux -> None
            | Some (OpenFileTarget.Socket _) ->
                // Unseekable on both, unlike the port above: measured, both
                // platforms answer ESPIPE for every whence in 0..4 and every
                // offset, `-1` and `INT64_MAX` alike. The whence-ordering
                // divergence still shows through this, and is exactly what the
                // ladder below reproduces — measured, `lseek(sock, 0, 9)` is
                // EINVAL on Linux (whence checked first) and ESPIPE on Darwin
                // (seekability checked first).
                Some DescriptorFault.NotSeekable
            | Some (OpenFileTarget.File _) -> None

        let ordered : UnixError option =
            match descriptorFault with
            | Some DescriptorFault.NotOpen ->
                // Ahead of everything on both platforms.
                Some UnixError.EBADF
            | notOpenRejected ->

            let unseekable =
                match notOpenRejected with
                | Some DescriptorFault.NotSeekable -> true
                | Some DescriptorFault.NotOpen
                | None -> false

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                if not whenceValid then Some UnixError.EINVAL
                elif unseekable then Some UnixError.ESPIPE
                else None
            | SimulatedUnixFlavour.Darwin ->
                if unseekable then Some UnixError.ESPIPE
                elif not whenceValid then Some UnixError.EINVAL
                else None

        match ordered with
        | Some error -> Ok (SyscallAnswer.Failed error, system)
        | None ->

        // Linux's `noop_llseek`, reached only under the Linux flavour (Darwin
        // answered ESPIPE above). It returns the file position unchanged, and an
        // epoll descriptor's is always 0, so the answer is 0 for every input
        // that gets here — measured for `SEEK_SET` with offset -1 and with
        // INT64_MAX alike, and for whence 3 and 4.
        //
        // Ahead of the SEEK_DATA/SEEK_HOLE refusal below, which is why that
        // refusal is not simply hoisted to the whence check: it is a statement
        // about a *file's* sparseness, and a port has none. The syscall's own
        // `whence <= SEEK_MAX` guard still applies and has already run, so
        // whence 5 and above were rejected as EINVAL.
        match target with
        | Some (OpenFileTarget.SocketEventPort _) -> Ok (SyscallAnswer.Completed 0L, system)
        | _ ->

        // Whence *validity* is settled; whence *semantics* is not, and the two
        // sit at different points in Linux's order — which is why refusing 3 and
        // 4 up front would be wrong. Measured, `lseek(badfd, 0, 3)` is EBADF and
        // `lseek(pipe, 0, 3)` is ESPIPE on both platforms, so a guest reaching
        // here with whence 3 or 4 really is asking about a seekable file's
        // sparseness.
        //
        // No BCL caller can reach it: `Interop.Sys.SeekWhence` declares only 0,
        // 1 and 2.
        if whence > seekEnd then
            let meaning =
                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    if whence = 3 then
                        SeekExtension.SeekData
                    else
                        SeekExtension.SeekHole
                | SimulatedUnixFlavour.Darwin ->
                    if whence = 3 then
                        SeekExtension.SeekHole
                    else
                        SeekExtension.SeekData

            Error (LSeekRefusal.Sparseness (whence, meaning))
        else

        let seekWhence =
            if whence = seekSet then
                SeekWhence.Set
            elif whence = seekCur then
                SeekWhence.Current
            elif whence = seekEnd then
                SeekWhence.End
            else
                failwith
                    $"UnixSystem.lseek: whence %d{whence} passed the validity and semantics checks but is not one of SEEK_SET, SEEK_CUR or SEEK_END (this is a bug in this library)"

        let inode, current =
            match target with
            | Some (OpenFileTarget.File (inode, current)) -> inode, current
            | _ ->
                failwith
                    $"UnixSystem.lseek: fd %d{fd} is not a seekable file, but the descriptor checks above did not reject it (this is a bug in this library)"

        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixSystem.lseek: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

        // The content is inspected only where a size is wanted, which is
        // `SEEK_END` alone. A directory has none this kernel will state, and a
        // symlink should not be here at all — but `SEEK_SET` and `SEEK_CUR` ask
        // neither question, so neither may fire on those paths.
        let sized : Result<int64 option, LSeekRefusal> =
            match seekWhence with
            | SeekWhence.Set
            | SeekWhence.Current -> Ok None
            | SeekWhence.End ->
                match entry.Content with
                | InodeContent.RegularFile (contents, _) -> Ok (Some (int64 contents.Length))
                | InodeContent.Symlink _ ->
                    // Not reachable: `open` resolves symlinks, so no descriptor
                    // names one. Stated rather than folded in so that an
                    // `O_PATH`-style descriptor finds a decision here.
                    failwith
                        $"UnixSystem.lseek: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what seeking a link through a descriptor means (this is a bug in this library)."
                | InodeContent.Directory _ -> Error (LSeekRefusal.DirectoryEnd inode)

        match sized with
        | Error refusal -> Error refusal
        | Ok forced ->

        let sizeOf =
            lazy
                match forced with
                | Some size -> size
                | None ->
                    failwith
                        "UnixSystem.lseek: the file size was consulted on a path that does not consult it (this is a bug in this library)"

        match VirtualFileSystem.seekTarget seekWhence current sizeOf offset with
        | Error SeekFault.Negative ->
            // EINVAL on both, and the offset is left where it was — measured, a
            // failed `lseek` does not move the description.
            Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
        | Error SeekFault.Overflow ->
            // The one place the *errno* differs rather than the ordering.
            // Measured on a tmpfs-backed file, so that the filesystem is held
            // constant: `lseek(f, INT64_MAX-4, SEEK_END)` on a 5-byte file is
            // EINVAL on Linux and EOVERFLOW on Darwin.
            match flavour with
            | SimulatedUnixFlavour.Linux -> Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            | SimulatedUnixFlavour.Darwin -> Ok (SyscallAnswer.Failed UnixError.EOVERFLOW, system)
        | Ok position ->

        Ok (
            SyscallAnswer.Completed position,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = FileDescriptorRegistry.setOffset fd position system.Process.FileDescriptors
                    }
            }
        )

    /// Answer one syscall.
    ///
    /// Sugar over the per-syscall functions above, for a client that wants one
    /// surface — to log every syscall, to replay a recorded sequence, or to
    /// generate them. Where a syscall's own function has a narrower type (the
    /// answer to `GetEffectiveUserId` cannot be a failure, and `Dup` cannot be
    /// refused), that type is the one to prefer.
    let step<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (call : Syscall)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SyscallRefusal>
        =
        match call with
        | Syscall.GetEffectiveUserId -> Ok (SyscallAnswer.Completed (int64 (effectiveUserId system)), system)
        | Syscall.Dup fd -> Ok (dup fd system)
        | Syscall.LSeek (fd, offset, whence) -> lseek fd offset whence system |> Result.mapError SyscallRefusal.LSeek
