namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// What `read(2)` moved, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type ReadAnswer =
    /// The bytes to place in the caller's buffer; the entry point returns how
    /// many there are.
    ///
    /// Empty means the call moved nothing and **the buffer was not touched at
    /// all**, which is measured rather than incidental: `read(f, NULL, 5)` at
    /// end-of-file is 0 on both platforms, not EFAULT. A caller that
    /// dereferenced its buffer before checking for empty would turn that answer
    /// into a fault.
    | Completed of bytes : ImmutableArray<byte>
    /// The entry point returns -1 and the caller stores `error` wherever its
    /// libc keeps errno. The file offset does not move.
    | Failed of error : UnixError

/// Why this kernel will not answer a `read`.
[<RequireQualifiedAccess>]
type ReadRefusal =
    /// The buffer has no answer at the step the read reached.
    | Buffer of BufferRefusal
    /// A socket. Every answer a real kernel gives here is a claim about
    /// connection state, which this kernel does not model.
    | SocketConnectionState of socket : SocketId * domain : SocketDomain * kind : SocketKind

[<RequireQualifiedAccess>]
module ReadRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, which descriptor, and which of its own
    /// callers could have reached this.
    let describe (refusal : ReadRefusal) : string =
        match refusal with
        | ReadRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | ReadRefusal.SocketConnectionState (socket, domain, kind) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}). This kernel models no socket connection state, and `read(2)` on a socket is an answer about exactly that: measured on an unconnected socket it is ENOTCONN for a TCP socket, EINVAL on Linux against ENOTCONN on Darwin for a Unix-domain stream socket, and a block with no wake source for a datagram socket. Any constant here would become a lie the moment connection state is modelled."

/// What `write(2)` did, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type WriteAnswer =
    /// How many bytes moved, which the entry point returns. Never short: this
    /// kernel has nothing that could push back on a write, and its filesystem
    /// cannot run out of space.
    | Completed of written : int
    /// The entry point returns -1 and the caller stores `error` wherever its
    /// libc keeps errno.
    | Failed of error : UnixError

/// Whether a `write` reaches the point at which it reads the caller's buffer.
///
/// The question exists because a caller may not be able to produce the bytes
/// without failing: a foreign-function layer whose memory is not a flat array
/// has to resolve the pointer, and resolving it can be a mistake in itself. Every
/// answer a `write` gives *without* reading the buffer is therefore available
/// first, so that the caller extracts only when extraction is what a real kernel
/// would do.
[<RequireQualifiedAccess>]
type WriteAdmission =
    /// Answered without the buffer being read at all — a bad descriptor, an
    /// object with no write operation, a faulting address, or the zero-length
    /// no-op.
    | Answered of answer : WriteAnswer
    /// The copy is reached: extract exactly `count` bytes and pass them to
    /// `write`.
    | Transfer of count : int

/// Why this kernel will not answer a `write`.
[<RequireQualifiedAccess>]
type WriteRefusal =
    /// The buffer has no answer at the step the write reached.
    | Buffer of BufferRefusal
    /// A socket, reached with a buffer the screen did not answer for. What a
    /// real kernel says here depends on the socket's connection state and on its
    /// kind — three different errnos across the two flavours — and this kernel
    /// models none of it.
    | SocketConnectionState of socket : SocketId * domain : SocketDomain * kind : SocketKind
    /// The write would leave the file longer than this kernel can represent.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int

[<RequireQualifiedAccess>]
module WriteRefusal =
    // Its own function because `write` and `pwrite` reach the same limit from
    // different offsets and must say the same thing about it.
    let internal describeExceedsRepresentableLength (inode : InodeNumber) (offset : int64) (count : int) : string =
        $"writing %d{count} bytes at offset %d{offset} of inode %O{inode} would leave the file longer than the %d{VirtualFileSystem.maxFileLength} bytes this kernel can represent. A real filesystem answers this without difficulty -- measured on ext4 and APFS alike, a one-byte write at offset 2^40 succeeds and leaves a sparse 1 TB file -- so this is a limit of the model, and refusing beats reporting an errno no kernel would have produced."

    /// What this kernel knows about why it cannot complete a write. The client
    /// supplies its own half — which entry point, which descriptor, and what it
    /// would have to build or configure to lift the refusal.
    let describe (refusal : WriteRefusal) : string =
        match refusal with
        | WriteRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | WriteRefusal.SocketConnectionState (socket, domain, kind) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}). This kernel models no socket connection state, and `write(2)` on a socket is an answer about exactly that: measured on an unconnected socket it is EPIPE on Linux against ENOTCONN on Darwin for a TCP socket, ENOTCONN on both for a Unix-domain stream socket, and EDESTADDRREQ for a datagram socket. The Linux TCP row also raises SIGPIPE, though a runtime that ignores that signal process-wide sees only the errno."
        | WriteRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            describeExceedsRepresentableLength inode offset count

/// Why this kernel will not answer a `pwrite`.
///
/// `WriteRefusal` without its socket case, rather than the same type: a socket
/// is unseekable, so `pwrite` answers ESPIPE and never reaches the socket's own
/// write operation, and a shared type would hand every client an arm it could
/// not reach and would have to invent a message for.
[<RequireQualifiedAccess>]
type PWriteRefusal =
    /// The buffer has no answer at the step this `pwrite` reached: its screen, or
    /// the copy it never got to make.
    | Buffer of BufferRefusal
    /// The write would place its last byte past the longest file this kernel can
    /// represent. Easier to reach than `write`'s: the offset is an argument
    /// rather than a position the description was walked to.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int

[<RequireQualifiedAccess>]
module PWriteRefusal =
    /// What this kernel knows about why it cannot complete a `pwrite`. The client
    /// supplies its own half -- which entry point, which descriptor, and what it
    /// would have to build or configure to lift the refusal.
    let describe (refusal : PWriteRefusal) : string =
        match refusal with
        | PWriteRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | PWriteRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            // The same fact `write` reports, reached from an argument rather than
            // from the description's offset, so it says the same sentence.
            WriteRefusal.describeExceedsRepresentableLength inode offset count

/// What a `read` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
///
/// Narrower than `OpenFileTarget`: it excludes the descriptors a read refuses
/// outright, so a caller that screens the buffer between those two steps — as
/// `vfs_read` does — has no unreachable arm left to write.
[<RequireQualifiedAccess>]
type private ReadTarget =
    /// The read end of the pipe this kernel models standard input as.
    | Stdin
    /// A file, at the offset its open file description currently holds.
    | File of inode : InodeNumber * offset : int64
    /// A socket, which is refused rather than answered.
    | Socket of socket : SocketId

/// What a `write` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
[<RequireQualifiedAccess>]
type private WriteTarget =
    /// A file. The offset is the description's own, and the write advances it —
    /// which is the whole difference from `pwrite`.
    | File of inode : InodeNumber * offset : int64
    /// One of the standard streams, whose bytes this kernel records rather than
    /// storing.
    | StandardStream of role : FileDescriptorRole
    /// A socket, which is refused rather than answered — but only once the
    /// buffer screen has had its say, which on one flavour answers first.
    | Socket of socket : SocketId

[<RequireQualifiedAccess>]
module UnixReadWrite =

    /// The object's own read operation on a regular file, which `read` and
    /// `pread` reach identically: the transfer window, the shortcut that touches
    /// no buffer at all, and the one point at which the buffer must hold bytes.
    /// What the two syscalls do *not* share is where `offset` comes from and
    /// whether the description's own offset then moves.
    ///
    /// The buffer screen has already had its say by here, so this is reached
    /// only with an address the flavour accepted.
    let private readFileAt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (syscall : string)
        (fd : int)
        (inode : InodeNumber)
        (offset : int64)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer, BufferRefusal>
        =
        let entry =
            match VirtualFileSystem.tryGet inode system.Machine.FileSystem with
            | Some entry -> entry
            | None ->
                failwith
                    $"UnixSystem.%s{syscall}: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink or rmdir removed a still-open file or directory; the open file description must keep it alive (this is a bug in this library)."

        match entry.Content with
        | InodeContent.Directory _ ->
            // EISDIR on both, and behind the buffer screen rather than ahead of
            // it: measured, `read(dir, NULL, 5)` is EISDIR while
            // `read(dir, (void*)-1, 5)` is EFAULT under a screening flavour.
            Ok (ReadAnswer.Failed UnixError.EISDIR)
        | InodeContent.Symlink _ ->
            failwith
                $"UnixSystem.%s{syscall}: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what reading a link through a descriptor means (this is a bug in this library)."
        | InodeContent.RegularFile (contents, _) ->

        let transfer = VirtualFileSystem.readTransferCount offset count contents.Length

        if transfer = 0 then
            // Nothing moves, so the buffer is not consulted: measured,
            // `read(f, NULL, 5)` at end-of-file is 0 on both platforms rather
            // than EFAULT. A null pointer is an ordinary user address, so it
            // reaches here rather than being screened above.
            Ok (ReadAnswer.Completed ImmutableArray.Empty)
        else

        // The one point at which the buffer must actually hold bytes.
        match buffer with
        | UserBuffer.Unmapped _ ->
            // Measured: an EFAULT leaves the file's contents and the caller's
            // offset alone. A kernel faults in `copy_to_user`, after deciding
            // what it would have transferred but before consuming anything.
            Ok (ReadAnswer.Failed UnixError.EFAULT)
        | UserBuffer.Opaque -> Error BufferRefusal.OpaqueAtTransfer
        | UserBuffer.Addressless -> Error BufferRefusal.AddresslessAtTransfer
        | UserBuffer.Mapped ->

        // Indexed rather than `Seq.skip`, which would enumerate the whole prefix
        // on every read and make reading a file quadratic in its length.
        ImmutableArray.CreateRange (seq { for i in 0 .. transfer - 1 -> contents.[int offset + i] })
        |> ReadAnswer.Completed
        |> Ok

    /// `read(2)`: move up to `count` bytes from `fd`'s current offset into the
    /// caller's buffer, and advance the offset by what actually moved.
    ///
    /// `count` must not be negative. A negative count is a foreign-function
    /// layer's error rather than a kernel's — .NET's `Common_Read` returns
    /// before it evaluates the descriptor at all, so `read(badfd, buf, -1)` is
    /// EINVAL and not EBADF — and a client that models such a layer must answer
    /// it before asking here.
    ///
    /// The buffer is consulted at three points and *not* consulted at three
    /// others, and both sets are measured; see the comments inline.
    let read<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer * UnixSystem<'Task, 'Handler>, ReadRefusal>
        =
        if count < 0 then
            failwith
                $"UnixReadWrite.read: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it answers a negative count itself, before it looks at the descriptor. Reject it there."

        // The descriptor's access mode, which Linux's `vfs_read` decides before
        // it screens the buffer: measured on both platforms,
        // `read(wronlyFd, (void*)-1, 4)` is EBADF rather than EFAULT, and even
        // `read(wronlyFd, buf, 0)` is EBADF rather than a no-op.
        let target : Result<ReadTarget, UnixError> =
            match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | None -> Error UnixError.EBADF
            | Some description ->

            if not (FileAccessMode.permitsRead description.AccessMode) then
                // A regular file opened `O_WRONLY` and a pipe's write end alike:
                // EBADF on both platforms. `read` has no seekability
                // requirement, so unlike `pread` there is no tie for the
                // platforms to break differently.
                Error UnixError.EBADF
            else

            match description.Target with
            | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput -> Ok ReadTarget.Stdin
            | OpenFileTarget.StandardStream role ->
                failwith
                    $"UnixReadWrite.read: fd %d{fd} names standard stream %O{role}, whose access mode permits reading. This kernel models the output streams as the write ends of pipes, so only standard input is readable (this is a bug in this library)."
            | OpenFileTarget.SocketEventPort _ ->
                // A socket event port has no read operation, so the read is
                // refused for the *kind* of object rather than for the access
                // mode — which is why the port is `ReadWrite` and still gets
                // here rather than being EBADF above. The two platforms name
                // that refusal differently: measured, Linux answers EINVAL
                // (`vfs_read`'s `FMODE_CAN_READ` test) and Darwin answers ENXIO.
                //
                // Placed in this classification rather than after the buffer
                // screen because it precedes it on both: measured,
                // `read(port, (void*)-1, 8)` is EINVAL on Linux and ENXIO on
                // Darwin, not EFAULT. Length is irrelevant too —
                // `read(port, buf, 0)` gives the same answer as a non-zero
                // length, unlike standard input's zero-return shortcut below.
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
                | SimulatedUnixFlavour.Darwin -> Error UnixError.ENXIO
            | OpenFileTarget.Socket socketId -> Ok (ReadTarget.Socket socketId)
            | OpenFileTarget.File (inode, offset) -> Ok (ReadTarget.File (inode, offset))

        match target with
        | Error error -> Ok (ReadAnswer.Failed error, system)
        | Ok target ->

        // Everything below this point is the object's own read operation, which
        // on Linux the buffer screen precedes: hence EFAULT ahead of EISDIR, of
        // standard input's end-of-file and of a socket's connection state, and a
        // fault even for a zero-length request. Darwin screens nothing here, so
        // its answers come from the operation itself.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error (ReadRefusal.Buffer refusal)
        | Ok true -> Ok (ReadAnswer.Failed UnixError.EFAULT, system)
        | Ok false ->

        match target with
        | ReadTarget.Socket socketId ->
            // A zero-length read of a socket is where the flavours part, and it
            // is the one socket answer that needs no connection state — on one
            // of them. Measured across every phase this kernel can produce and
            // every kind it models, `read(sock, buf, 0)`:
            //
            //   socket state                     Linux   Darwin
            //   INET stream, idle                0       ENOTCONN
            //   UNIX stream, idle                0       ENOTCONN
            //   datagram, idle                   0       0
            //   INET stream, bound not listening 0       ENOTCONN
            //   INET stream, listening           0       ENOTCONN
            //   stream, connected, nothing queued 0      0
            //   stream, connected, a byte queued  0      0
            //   datagram, connected, empty        0      0
            //   datagram, connected, one queued   0      0
            //   stream, peer closed               0      0
            //
            // So **Linux answers 0 in every state**, which is why the flavour
            // alone decides it here: there is no phase or kind on which the
            // answer depends. Darwin's is 0 too except for a stream socket that
            // is not connected, and telling those apart means modelling exactly
            // the connection state this refusal exists to avoid — so Darwin
            // declines the whole class, which over-refuses the connected cases
            // and never answers wrongly.
            //
            // The same descriptors answer ENOTCONN (or EAGAIN, connected and
            // empty) at length 1, so the short-circuit is about the length
            // rather than the socket. The socket event port does not share it
            // and is answered above: measured, `read(port, buf, 0)` is EINVAL on
            // Linux like every other length.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform, count with
            | SimulatedUnixFlavour.Linux, 0 -> Ok (ReadAnswer.Completed ImmutableArray.Empty, system)
            | SimulatedUnixFlavour.Linux, _
            | SimulatedUnixFlavour.Darwin, _ ->
                let socket = UnixMachineState.socket socketId system.Machine
                Error (ReadRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | ReadTarget.Stdin ->
            // **Immediate end-of-file**, and this is a claim about how the
            // process was launched rather than a fallback: this kernel models
            // standard input as the read end of a pipe whose write end was
            // closed by whoever started the process, so there is nothing to read
            // and never will be.
            //
            // The buffer is not consulted: measured on both platforms, a read
            // that returns end-of-file never touches it, so `read(0, NULL, 5)`
            // is 0 rather than EFAULT. Same rule as the transfer-window
            // shortcut below.
            Ok (ReadAnswer.Completed ImmutableArray.Empty, system)
        | ReadTarget.File (inode, offset) ->

        // The window is computed from the description's own offset, which is the
        // whole of what `pread` does differently; everything after it is the
        // same operation, so the two share it.
        match readFileAt "read" fd inode offset buffer count system with
        | Error refusal -> Error (ReadRefusal.Buffer refusal)
        | Ok (ReadAnswer.Failed error) -> Ok (ReadAnswer.Failed error, system)
        | Ok (ReadAnswer.Completed bytes) ->

        if bytes.IsEmpty then
            // Nothing moved, so the offset stays exactly where it was rather
            // than being clamped to the file's length or rewritten to itself.
            Ok (ReadAnswer.Completed bytes, system)
        else

        // Advanced by what actually moved, not by what was asked for: a short
        // read at the end of a file leaves the offset at the end rather than
        // past it, which is what makes a subsequent read return 0 instead of a
        // second short read.
        Ok (
            ReadAnswer.Completed bytes,
            { system with
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.setOffset
                                fd
                                (offset + int64 bytes.Length)
                                system.Process.FileDescriptors
                    }
            }
        )

    /// `pread(2)`: move up to `count` bytes from `offset` in the file `fd` names
    /// into the caller's buffer, without consulting or moving the description's
    /// own file offset.
    ///
    /// `count` must not be negative, for the reason `read`'s must not: a kernel
    /// never sees one, so whichever foreign-function layer produced it must
    /// answer it. That answer need not be `read`'s — a shim is free to validate
    /// one of the two and cast the other — which is why neither is given here.
    ///
    /// A negative `offset`, by contrast, *is* a request a kernel sees, and is
    /// EINVAL. Where in the order it is answered differs between the flavours,
    /// which is what makes this more than `read` with an extra argument.
    ///
    /// No system comes back, because a `pread` changes nothing in one: it moves
    /// no file offset, and nothing in this kernel moves `atime`.
    let pread<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer, BufferRefusal>
        =
        if count < 0 then
            failwith
                $"UnixReadWrite.pread: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it decides what a negative count means, before it looks at the descriptor. Reject it there."

        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        let offsetInvalid = offset < 0L

        // The order of the checks below is measured, and it differs between the
        // flavours. On a *single-fault* input they agree on every row; they part
        // company only when two things are wrong at once, which is why an
        // ordering has to be pinned at all:
        //
        //   input                          Linux    Darwin
        //   negative offset + bad fd       EINVAL   EBADF
        //   negative offset + pipe         EINVAL   ESPIPE
        //   negative offset + socket       EINVAL   ESPIPE
        //   negative offset + port         EINVAL   ESPIPE
        //   negative offset + O_WRONLY     EINVAL   EBADF
        //   negative offset + directory    EINVAL   EINVAL
        //   negative offset + bad address  EINVAL   EINVAL
        //
        // Linux validates the offset before it even looks the descriptor up
        // (`do_pread` checks `pos < 0` ahead of `fdget`); Darwin resolves the
        // descriptor, its seekability and its access mode first, and only then
        // the offset. Both orders are followed rather than one being imposed on
        // the other, because both are fully measured.
        //
        // `EISDIR` and the buffer screen both follow the offset check on
        // *both* — the last two rows — so only the descriptor steps actually
        // move, and one flag suffices rather than two orderings.
        let offsetCheckedBeforeDescriptor =
            match flavour with
            | SimulatedUnixFlavour.Linux -> true
            | SimulatedUnixFlavour.Darwin -> false

        // The inode this `pread` will read from, once every question that
        // precedes the buffer screen has been settled. Only a file reaches it:
        // `pread` needs a seekable object, and a directory is one, so a
        // directory's EISDIR comes from the operation below rather than from
        // here.
        let target : Result<InodeNumber, UnixError> =
            if offsetCheckedBeforeDescriptor && offsetInvalid then
                Error UnixError.EINVAL
            else

            match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
            | None -> Error UnixError.EBADF
            | Some description ->

            // Whether this description was opened for reading at all. Two arms
            // below need it and neither may guess: for a standard stream it
            // breaks the ESPIPE/EBADF tie, and for a regular file it is the
            // whole answer.
            let readable = FileAccessMode.permitsRead description.AccessMode

            match description.Target with
            | OpenFileTarget.StandardStream _ ->
                // `pread` needs a seekable object, and this kernel models the
                // standard streams as pipes — standard input the read end,
                // output and error the write ends. Output and error therefore
                // fail two different tests at once: neither seekable nor open
                // for reading. Measured, the flavours break that tie
                // differently:
                //
                //   descriptor                        Linux    Darwin
                //   pipe read end (unseekable)        ESPIPE   ESPIPE
                //   pipe write end (also unreadable)  ESPIPE   EBADF
                //   regular file O_WRONLY (seekable)  EBADF    EBADF
                //
                // So Linux lets unseekability win and Darwin lets unreadability
                // win. The third row is the control that shows this is about the
                // tie rather than about readability generally, and it is the
                // `not readable` arm further down.
                match flavour with
                | SimulatedUnixFlavour.Darwin when not readable -> Error UnixError.EBADF
                | SimulatedUnixFlavour.Darwin
                | SimulatedUnixFlavour.Linux -> Error UnixError.ESPIPE
            | OpenFileTarget.SocketEventPort _ ->
                // Unseekable on both, with no tie to break: a port's description
                // is `ReadWrite`, so the unreadability arm above cannot apply to
                // it. Measured, `pread(port, buf, 8, 0)` and
                // `pread(port, buf, 0, 0)` are both ESPIPE on both flavours, and
                // so is `pread(port, (void*)-1, 8, 0)` — unseekability precedes
                // the buffer screen, which is why this is classified here rather
                // than after it.
                //
                // Note that this is *not* what `read` says of the same
                // descriptor, which is EINVAL on Linux and ENXIO on Darwin: the
                // object has no read operation at all, and `pread` never gets as
                // far as asking, having already failed on seekability.
                Error UnixError.ESPIPE
            | OpenFileTarget.Socket _ ->
                // Unseekable on both, for the same reason the port is, and
                // measured on a TCP, a UDP and a Unix-domain socket alike.
                //
                // Unlike `read`, this needs no connection state and so is an
                // answer rather than a refusal: every socket is unseekable
                // whatever it is connected to, so `pread` never reaches the
                // socket's own read operation.
                Error UnixError.ESPIPE
            | OpenFileTarget.File (inode, _) ->
                if not readable then
                    // A descriptor not open for reading: EBADF on both, which is
                    // `vfs_read`'s answer for a file whose `FMODE_READ` is
                    // clear.
                    //
                    // Ahead of Darwin's offset check rather than after it, and
                    // measured: `pread(wronlyFd, buf, 4, -1)` is EBADF on Darwin
                    // but EINVAL on Linux, so on Darwin the access mode is
                    // settled before the offset is looked at, exactly as
                    // seekability is above. On Linux this ordering cannot be
                    // observed, the offset check having already run.
                    Error UnixError.EBADF
                elif not offsetCheckedBeforeDescriptor && offsetInvalid then
                    // Darwin's turn to validate the offset: it has now resolved
                    // the descriptor, its seekability and its access mode, which
                    // is exactly the window in which it differs from Linux.
                    Error UnixError.EINVAL
                else
                    Ok inode

        match target with
        | Error error -> Ok (ReadAnswer.Failed error)
        | Ok inode ->

        // Everything below is the object's own read operation, which under a
        // screening flavour the buffer screen precedes: hence EFAULT ahead of
        // EISDIR and of the transfer window, and a fault even for a zero-length
        // request. The unscreened flavour discovers a bad address at the copy
        // instead.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error refusal
        | Ok true -> Ok (ReadAnswer.Failed UnixError.EFAULT)
        | Ok false ->

        readFileAt "pread" fd inode offset buffer count system

    /// What a `write` will operate on, once the descriptor's access mode has
    /// been checked: a file at its description's own offset, or a standard
    /// stream.
    let private writeTarget<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteTarget, UnixError>
        =
        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Error UnixError.EBADF
        | Some description ->

        if not (FileAccessMode.permitsWrite description.AccessMode) then
            // `write(2)` on a descriptor not open for writing is EBADF on both
            // platforms, and this precedes both the buffer screen and the
            // zero-size no-op: measured, `write(rdonlyFd, buf, 0)` is EBADF
            // rather than 0. It covers standard input — which a redirected
            // launch opens `O_RDONLY` — and a regular file opened `O_RDONLY`
            // alike, including a directory, which can only ever be opened for
            // reading.
            Error UnixError.EBADF
        else

        match description.Target with
        | OpenFileTarget.SocketEventPort _ ->
            // A socket event port has no write operation, so the refusal is for
            // the *kind* of object rather than for the access mode — the port
            // permits writing and so passes the EBADF arm above. Measured, Linux
            // answers EINVAL and Darwin ENXIO.
            //
            // Ahead of the buffer screen and of the zero-size no-op, on both
            // platforms: measured, `write(port, (void*)-1, 8)` is EINVAL/ENXIO
            // rather than EFAULT, and no length is a no-op.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
            | SimulatedUnixFlavour.Darwin -> Error UnixError.ENXIO
        | OpenFileTarget.Socket socketId -> Ok (WriteTarget.Socket socketId)
        | OpenFileTarget.File (inode, offset) -> Ok (WriteTarget.File (inode, offset))
        | OpenFileTarget.StandardStream role -> Ok (WriteTarget.StandardStream role)

    /// Every answer `write(2)` gives *without* reading the caller's buffer, and
    /// otherwise how many bytes to extract.
    ///
    /// Changes nothing: everything a write does before the copy is a question.
    /// See `WriteAdmission` for why this is a separate call rather than a
    /// `write` that takes the bytes.
    let admitWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission, WriteRefusal>
        =
        if count < 0 then
            failwith
                $"UnixReadWrite.admitWrite: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it answers a negative count itself, before it looks at the descriptor. Reject it there."

        match writeTarget fd system with
        | Error error -> Ok (WriteAdmission.Answered (WriteAnswer.Failed error))
        | Ok target ->

        // `vfs_write` screens the buffer between the access mode above and the
        // file operation, so on Linux this beats the zero-size no-op below:
        // measured, `write(1, (void*)-1, 0)` is EFAULT there and 0 on macOS.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error (WriteRefusal.Buffer refusal)
        | Ok true -> Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | Ok false ->

        // **After the screen, and before the zero-length no-op.** Both halves
        // are measured. Linux screens the address before the object's own write
        // operation, so `write(socket, (void*)-1, n)` there is EFAULT for every
        // `n` including 0: the screen answers and the socket is never consulted.
        // Darwin screens nothing, so the same call reaches the socket and earns
        // a connection-state answer (ENOTCONN for a stream socket,
        // EDESTADDRREQ for a datagram one), which is what this kernel cannot
        // give.
        //
        // And the no-op does *not* precede it: measured on both,
        // `write(socket, buf, 0)` is the socket's own error rather than 0.
        match target with
        | WriteTarget.Socket socketId ->
            let socket = UnixMachineState.socket socketId system.Machine
            Error (WriteRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | WriteTarget.File _
        | WriteTarget.StandardStream _ ->

        if count = 0 then
            // A no-op on both platforms, and specifically one that moves no
            // timestamp: measured, a zero-length write leaves `mtime` and
            // `ctime` where they were and does not extend the file, even at an
            // offset past its end. The buffer is not resolved: any address that
            // got past the screen is permitted, because it is not dereferenced.
            Ok (WriteAdmission.Answered (WriteAnswer.Completed 0))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // Real `write(2)` answers EFAULT for any non-dereferenceable
            // address, null included, having performed no I/O.
            Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | UserBuffer.Opaque -> Error (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (WriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (WriteAdmission.Transfer count)

    /// `write(2)`, given the bytes the caller extracted after `admitWrite` said
    /// to.
    ///
    /// Takes no buffer: every question about the caller's buffer is settled by
    /// `admitWrite`, and a signature that could not ask them again is the point.
    /// Still answers the descriptor questions itself, so a caller that skipped
    /// the admission gets a kernel's answer rather than an inconsistent one.
    ///
    /// Never short and never `EINTR`: this kernel has nothing that could push
    /// back on a write, and its filesystem cannot run out of space.
    let write<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAnswer * UnixSystem<'Task, 'Handler>, WriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixReadWrite.write: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        match writeTarget fd system with
        | Error error -> Ok (WriteAnswer.Failed error, system)
        | Ok (WriteTarget.Socket socketId) ->
            // There is no buffer here to screen, so the socket's own answer is
            // all there is — and this kernel cannot give it. A caller that used
            // `admitWrite` never reaches this: that call refused or answered
            // first.
            let socket = UnixMachineState.socket socketId system.Machine
            Error (WriteRefusal.SocketConnectionState (socketId, socket.Domain, socket.Kind))
        | Ok _ when bytes.IsEmpty ->
            // A no-op on both platforms, and specifically one that changes
            // nothing: measured, a zero-length write leaves `mtime` and `ctime`
            // where they were, does not extend the file, and does not strip the
            // set-ID bits. `admitWrite` answers this too, so the arm is
            // unreachable for a caller that used the pair — but a caller that
            // did not must get the same answer, and `VirtualFileSystem.writeFile`
            // below asserts a non-empty write precisely because it would
            // otherwise restamp the inode.
            //
            // After the descriptor checks, not before: `write(rdonlyFd, buf, 0)`
            // is EBADF rather than 0, measured on both.
            Ok (WriteAnswer.Completed 0, system)
        | Ok (WriteTarget.StandardStream role) ->
            Ok (
                WriteAnswer.Completed bytes.Length,
                { system with
                    Process =
                        { system.Process with
                            OutputLog =
                                system.Process.OutputLog.Add
                                    {
                                        OutputLogEntry.Role = role
                                        OutputLogEntry.Bytes = bytes
                                    }
                        }
                }
            )
        | Ok (WriteTarget.File (inode, offset)) ->

        let now = UnixMachineState.fileTimestamp system.Machine

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root; measured on both platforms, which
        // disagree only about `S_ISGID` on a file that is not group-executable.
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform
        let privilege = UnixProcessState.callerPrivilege system.Process

        match VirtualFileSystem.writeFile inode offset bytes rule privilege now system.Machine.FileSystem with
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            Error (WriteRefusal.ExceedsRepresentableLength (inode, offset, count))
        | Ok filesystem ->

        // At the description's own offset, and advancing it by what moved — the
        // entire difference from `pwrite`, which takes the offset as an argument
        // and leaves the description alone. Both measured.
        //
        // The commit comes first, so the advance cannot overflow: a write that
        // would carry the offset past what the model can represent has already
        // been refused there.
        Ok (
            WriteAnswer.Completed bytes.Length,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.setOffset
                                fd
                                (offset + int64 bytes.Length)
                                system.Process.FileDescriptors
                    }
            }
        )

    /// The inode a `pwrite` will write into, once every question that precedes
    /// the buffer screen has been settled.
    ///
    /// Only a regular file reaches it: `pwrite` needs a seekable object, and a
    /// directory can only ever be opened for reading, so `pwrite` to one is the
    /// access mode's EBADF rather than a kind's EISDIR.
    let private pwriteTarget<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<InodeNumber, UnixError>
        =
        // **Ahead of the descriptor, on both flavours** — which is exactly where
        // `pwrite` differs from `pread`, and it is measured rather than inferred
        // from the symmetry. Every second fault gives way to it:
        //
        //   negative offset with...    Linux    Darwin
        //   a bad descriptor           EINVAL   EINVAL
        //   a pipe's read end          EINVAL   EINVAL
        //   a pipe's write end         EINVAL   EINVAL
        //   a read-only file           EINVAL   EINVAL
        //   a directory                EINVAL   EINVAL
        //   a socket                   EINVAL   EINVAL
        //   a socket event port        EINVAL   EINVAL
        //   an unscreenable address    EINVAL   EINVAL
        //   a zero length              EINVAL   EINVAL
        //
        // `pread` needs a per-flavour flag for the same question, Darwin
        // resolving the descriptor first and answering EBADF or ESPIPE for these
        // shapes. Do not copy that flag here: the two syscalls genuinely differ.
        if offset < 0L then
            Error UnixError.EINVAL
        else

        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> Error UnixError.EBADF
        | Some description ->

        // Whether this description was opened for writing at all. Two arms below
        // need it and neither may guess: for a standard stream it breaks the
        // ESPIPE/EBADF tie, and for a regular file it is the whole answer.
        let writable = FileAccessMode.permitsWrite description.AccessMode

        match description.Target with
        | OpenFileTarget.StandardStream _ ->
            // The mirror of `pread`'s tie, with the roles swapped: `pwrite` needs
            // a seekable object, this kernel models the standard streams as
            // pipes, and standard *input* therefore fails two tests at once —
            // neither seekable nor open for writing. Measured:
            //
            //   descriptor                        Linux    Darwin
            //   pipe write end (unseekable)       ESPIPE   ESPIPE
            //   pipe read end (also unwritable)   ESPIPE   EBADF
            //   regular file O_RDONLY (seekable)  EBADF    EBADF
            //
            // Linux lets unseekability win and Darwin lets unwritability win,
            // exactly as they do for `pread`. The third row is the control that
            // shows this is about the tie rather than about writability
            // generally, and it is the `not writable` arm further down.
            //
            // Ahead of the buffer screen on both, and measured that way rather
            // than assumed: `pwrite(pipeReadEnd, (void*)-1, 4, 0)` is ESPIPE on
            // Linux and EBADF on Darwin, not EFAULT.
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Darwin when not writable -> Error UnixError.EBADF
            | SimulatedUnixFlavour.Darwin
            | SimulatedUnixFlavour.Linux -> Error UnixError.ESPIPE
        | OpenFileTarget.SocketEventPort _ ->
            // Unseekable on both, with no tie to break: a port's description is
            // `ReadWrite`, so the unwritability arm above cannot apply to it.
            // Measured ESPIPE at length 8, at length 0, and with an unscreenable
            // address — so unseekability precedes both the no-op and the screen.
            //
            // Note that this is *not* what `write` says of the same descriptor,
            // which is EINVAL on Linux and ENXIO on Darwin: the object has no
            // write operation at all, and `pwrite` never gets as far as asking.
            Error UnixError.ESPIPE
        | OpenFileTarget.Socket _ ->
            // Unseekable on both, for the same reason the port is, and measured
            // on a TCP, a UDP and a Unix-domain socket alike.
            //
            // Unlike `write`, this needs no connection state and so is an answer
            // rather than a refusal: every socket is unseekable whatever it is
            // connected to, so `pwrite` never reaches the socket's own write
            // operation. That is why `PWriteRefusal` has no socket case.
            Error UnixError.ESPIPE
        | OpenFileTarget.File (inode, _) ->

        if not writable then
            // `vfs_write`'s EBADF for a descriptor whose `FMODE_WRITE` is clear,
            // and it precedes both the buffer screen and the zero-length no-op:
            // measured, `pwrite(rdonlyFd, (void*)-1, 4, 0)` is EBADF rather than
            // EFAULT and `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0.
            //
            // This is also what makes a directory unreachable below: one can only
            // be opened for reading, so it never gets past here.
            Error UnixError.EBADF
        else
            Ok inode

    /// Every answer `pwrite(2)` gives *without* reading the caller's buffer, and
    /// otherwise how many bytes to extract.
    ///
    /// Changes nothing: everything a write does before the copy is a question.
    /// See `WriteAdmission` for why this is a separate call rather than a
    /// `pwrite` that takes the bytes.
    ///
    /// Does not settle the length limit, which `pwrite` reports at the commit —
    /// so a caller can be told to extract bytes for a write that is then refused
    /// as unrepresentable. That costs the caller work rather than correctness,
    /// the refusal carrying no state, and it is what `admitWrite` does too.
    let admitPWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (buffer : UserBuffer)
        (count : int)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAdmission, PWriteRefusal>
        =
        if count < 0 then
            failwith
                $"UnixReadWrite.admitPWrite: a count of %d{count} is not a request a kernel ever sees — the foreign-function layer that produced it decides what a negative count means, before it looks at the descriptor. Reject it there."

        match pwriteTarget fd offset system with
        | Error error -> Ok (WriteAdmission.Answered (WriteAnswer.Failed error))
        | Ok _ ->

        // `vfs_write` screens the buffer between the access mode above and the
        // file operation, so under a screening flavour this beats the no-op
        // below: measured, `pwrite(f, (void*)-1, 0, 0)` is EFAULT on Linux and 0
        // on Darwin.
        match
            UserBufferCheck.faultsBeforeOperationFor
                (UnixMachineState.userBufferCheck system.Machine)
                buffer
                (uint64 count)
        with
        | Error refusal -> Error (PWriteRefusal.Buffer refusal)
        | Ok true -> Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | Ok false ->

        if count = 0 then
            // A no-op on both flavours, and specifically one that moves no
            // timestamp: measured, a zero-length `pwrite` leaves `mtime` and
            // `ctime` where they were and does not extend the file, even at an
            // offset far past its end. The buffer is not resolved, because
            // nothing is read through it — a null pointer is an ordinary user
            // address, so it reaches here rather than being screened above, and
            // `pwrite(f, NULL, 0, 0)` is 0 on both.
            Ok (WriteAdmission.Answered (WriteAnswer.Completed 0))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // Real `pwrite(2)` answers EFAULT for any non-dereferenceable
            // address, null included, having performed no I/O: measured,
            // `pwrite(f, NULL, 4, 0)` is EFAULT on both, where the same pointer
            // at length 0 is a no-op.
            Ok (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | UserBuffer.Opaque -> Error (PWriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (PWriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (WriteAdmission.Transfer count)

    /// `pwrite(2)`, given the bytes the caller extracted after `admitPWrite` said
    /// to: place them at `offset` without consulting or moving the description's
    /// own file offset.
    ///
    /// Takes no buffer, for the reason `write` does not: every question about the
    /// caller's buffer is settled by the admission, and a signature that could
    /// not ask them again is the point. Still answers the descriptor questions
    /// itself, so a caller that skipped the admission gets a kernel's answer
    /// rather than an inconsistent one.
    ///
    /// A system comes back, unlike `pread`'s: the offset does not move, but the
    /// file's contents and timestamps do.
    ///
    /// Never short and never `EINTR`: this kernel has nothing that could push
    /// back on a write, and its filesystem cannot run out of space.
    let pwrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteAnswer * UnixSystem<'Task, 'Handler>, PWriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixReadWrite.pwrite: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        match pwriteTarget fd offset system with
        | Error error -> Ok (WriteAnswer.Failed error, system)
        | Ok inode ->

        if bytes.IsEmpty then
            // A no-op that changes nothing, and *after* the descriptor checks:
            // measured, `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0.
            // `admitPWrite` answers this too, so the arm is unreachable for a
            // caller that used the pair — but a caller that did not must get the
            // same answer, and `VirtualFileSystem.writeFile` below asserts a
            // non-empty write precisely because it would otherwise restamp the
            // inode.
            Ok (WriteAnswer.Completed 0, system)
        else

        let now = UnixMachineState.fileTimestamp system.Machine

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root, exactly as `write`'s does: the bits
        // follow the content changing, not which syscall changed it.
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform
        let privilege = UnixProcessState.callerPrivilege system.Process

        match VirtualFileSystem.writeFile inode offset bytes rule privilege now system.Machine.FileSystem with
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            Error (PWriteRefusal.ExceedsRepresentableLength (inode, offset, count))
        | Ok filesystem ->

        // The description is left exactly where it was — the whole of what
        // `pwrite` does differently from `write`, and measured on both flavours.
        Ok (
            WriteAnswer.Completed bytes.Length,
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
        )
