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
    /// A socket in `phase`, which has a peer or a refused connection's error:
    /// what a read does there is not modelled.
    | UnmodelledSocketPhase of socket : SocketId * domain : SocketDomain * kind : SocketKind * phase : SocketPhase
    /// A blocking read of a datagram socket with no peer, which sleeps until a
    /// datagram arrives. Nothing in this kernel sends one, and a sleep only a
    /// signal could end is not modelled.
    | DatagramSleep of socket : SocketId * domain : SocketDomain
    /// A directory this description has read part of the way through, on a
    /// filesystem whose position there this kernel cannot bound, and a count
    /// for which the answer depends on that position. On Linux that is EINVAL
    /// if position + count passes `INT64_MAX`; on Darwin, 0 if the position is
    /// `INT64_MAX`; EISDIR otherwise.
    | ScannedDirectoryPosition of inode : InodeNumber * fileSystem : EmulatedFileSystemType
    /// A read asleep on a pipe has an answer, and the library will not say
    /// whether that or a signal ends it.
    | Interruption of SyscallInterruptionRefusal

[<RequireQualifiedAccess>]
module ReadRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, which descriptor, and which of its own
    /// callers could have reached this.
    let describe (refusal : ReadRefusal) : string =
        match refusal with
        | ReadRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | ReadRefusal.UnmodelledSocketPhase (socket, domain, kind, phase) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}), in phase %A{phase}. This kernel answers `read(2)` on a socket with no peer, but models no transfer of bytes between sockets, nor what a read reports of a refused connection's error, so it has no answer for a socket that has a peer or such an error."
        | ReadRefusal.DatagramSleep (socket, domain) ->
            $"the descriptor is socket %O{socket} (%O{domain}, datagram), which has no peer, and the read is a blocking one. Such a read sleeps until a datagram arrives; nothing in this kernel sends one, and a sleep that only a signal could end is not modelled."
        | ReadRefusal.ScannedDirectoryPosition (inode, fileSystem) ->
            $"the descriptor is directory %O{inode} on %O{fileSystem}, which this description has read part of the way through. Ahead of a directory's EISDIR, Linux answers EINVAL when position + count passes INT64_MAX and Darwin answers 0 when the position is INT64_MAX, and the position after a partial scan is %O{fileSystem}'s own cookie (on an NFS mount, whatever the server chose, up to INT64_MAX), which is not a number this kernel's position corresponds to. So whether this read is EISDIR or the position's answer is unknown here."
        | ReadRefusal.Interruption refusal ->
            $"the read was asleep on a pipe: %s{SyscallInterruptionRefusal.describe refusal}"

/// What became of a `read(2)` this kernel could answer, where "answer" may be
/// "the calling task sleeps".
[<RequireQualifiedAccess>]
type ReadOutcome =
    /// The entry point returned.
    | Answered of ReadAnswer
    /// The call did not return: it is a blocking read of a pipe that holds
    /// nothing while a write end is open. The calling task is parked, and
    /// sleeps until `UnixWait.wakes` wakes it; then `UnixReadWrite.finishRead`
    /// finishes the call.
    | WouldBlock of WakeCondition
    /// The call was asleep, a signal with a handler interrupted it, and the call
    /// restarts (`SyscallInterruption.Restart`): it never returns. The task is
    /// no longer parked. Once the handlers have run, the client issues the
    /// `read` again with the arguments it was first made with.
    ///
    /// Only `finishRead` answers this.
    | Restarts

/// What `write(2)` did, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type WriteAnswer =
    /// How many bytes moved, which the entry point returns: every one asked
    /// for, unless that was more than the platform moves in one call (see
    /// `TransferCountLimit` and `WriteAdmission.Transfer`), or the write was a
    /// non-blocking one into a pipe with room for only some of them, or a
    /// blocking one into a pipe that a signal or (on Linux) the reader leaving
    /// ended after some of it had gone in. Nothing else makes a write short
    /// here: this kernel's filesystem cannot run out of space, and a pipe the
    /// client drains has room for the whole of a blocking write.
    | Completed of written : int64
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
    /// object with no write operation, a faulting address, the zero-length
    /// no-op, or a non-blocking write into a pipe with no room.
    | Answered of answer : WriteAnswer
    /// The copy is reached: extract exactly `count` bytes and pass them to
    /// `write`. That is the count asked for, or one call's worth of it if it was
    /// more (see `TransferCountLimit`), so it is never more than
    /// `Int32.MaxValue`; or, for a non-blocking write into a pipe with room for
    /// only part of it, the part the pipe takes, which is all `write` then
    /// answers.
    | Transfer of count : int
    /// A blocking write of `total` bytes into a pipe with room for part of it:
    /// extract the first `count` of them, which the pipe takes now, and pass
    /// them to `UnixReadWrite.writeThenSleep`, which puts them in and sleeps
    /// for the rest. The rest is read from the caller's buffer only as room
    /// appears, as a real kernel copies it.
    | TransferThenSleep of count : int * total : int

/// Whether a blocking `write` into a pipe, asleep because the pipe had no room
/// for the rest of it, reaches the point at which it reads more of the caller's
/// buffer. `UnixReadWrite.admitFinishWrite` answers it, for the reason
/// `WriteAdmission` exists.
[<RequireQualifiedAccess>]
type WriteResumption =
    /// Answered without the buffer being read: what the call returns.
    | Answered of answer : WriteAnswer
    /// The pipe has room: extract the `count` bytes of the caller's buffer that
    /// start `offset` bytes in, and pass them to `UnixReadWrite.finishWrite`.
    /// The bytes before `offset` are in the pipe already.
    | Transfer of offset : int * count : int

/// What a write that answers `EPIPE` and raises `SIGPIPE` was writing to.
[<RequireQualifiedAccess>]
type BrokenWriteTarget =
    /// The write end of a pipe with no reader.
    | Pipe of pipe : PipeId
    /// A Linux stream socket with no peer.
    | Socket of socket : SocketId

[<RequireQualifiedAccess>]
module BrokenWriteTarget =
    /// The descriptor's object, for a message: "the write end of pipe ...".
    let describe (target : BrokenWriteTarget) : string =
        match target with
        | BrokenWriteTarget.Pipe pipe -> $"the write end of pipe %O{pipe}, which has no reader"
        | BrokenWriteTarget.Socket socket -> $"socket %O{socket}, a stream socket with no peer"

/// Why this kernel will not answer a `write`.
[<RequireQualifiedAccess>]
type WriteRefusal =
    /// The buffer has no answer at the step the write reached.
    | Buffer of BufferRefusal
    /// A socket in `phase`, which has a peer or a refused connection's error:
    /// what a write does there is not modelled.
    | UnmodelledSocketPhase of socket : SocketId * domain : SocketDomain * kind : SocketKind * phase : SocketPhase
    /// A Unix-domain datagram socket with no peer, on Linux, where the answer
    /// depends on the size of the socket's send buffer, which is not modelled
    /// (see `UnconnectedSocketWrite.DependsOnSendBuffer`).
    | SendBuffer of socket : SocketId
    /// An unbound IPv6 datagram socket with no peer, on Linux, which a write
    /// binds to an ephemeral port before it fails
    /// (`UnconnectedSocketRules.writeBindsFirst`): this kernel binds only IPv4
    /// sockets, so it cannot record the binding.
    | Inet6Binding of socket : SocketId
    /// An unbound datagram socket with no peer, on Linux, which a write binds
    /// to an ephemeral port before it fails, when every port in the ephemeral
    /// range is taken. What the kernel answers then is not measured.
    | EphemeralPortsExhausted of socket : SocketId * low : uint16 * high : uint16
    /// The write would leave the file longer than this kernel can represent.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int
    /// What writing to the file at `inode` would do to its set-ID bits has not
    /// been measured for this writer.
    | UnmeasuredSetIdChange of inode : InodeNumber * refusal : SetIdChangeRefusal
    /// A write asleep in a pipe has an answer, and the library will not say
    /// whether that or a signal ends it.
    | Interruption of SyscallInterruptionRefusal
    /// A write into a pipe with no reader, or a Linux stream socket with no
    /// peer, which answers `EPIPE` and raises `SIGPIPE`, when which task would
    /// receive the signal is not modelled.
    | SignalReceiver of target : BrokenWriteTarget * refusal : SignalReceiverRefusal
    /// A write into a pipe with no reader, or a Linux stream socket with no
    /// peer, which raises `SIGPIPE`, by process ID 1. An init process ignores,
    /// from inside its own PID namespace, every signal it has not installed a
    /// handler for, and this library does not model that.
    | InitProcess of target : BrokenWriteTarget

[<RequireQualifiedAccess>]
module WriteRefusal =
    // Its own function because `write` and `pwrite` reach the same limit from
    // different offsets and must say the same thing about it.
    let internal describeUnmeasuredSetIdChange (inode : InodeNumber) (refusal : SetIdChangeRefusal) : string =
        $"writing to inode %O{inode}: %s{SetIdChangeRefusal.describe refusal}"

    let internal describeExceedsRepresentableLength (inode : InodeNumber) (offset : int64) (count : int) : string =
        $"writing %d{count} bytes at offset %d{offset} of inode %O{inode} would leave the file longer than the %d{VirtualFileSystem.maxFileLength} bytes this kernel can represent. A real filesystem answers this without difficulty -- measured on ext4 and APFS alike, a one-byte write at offset 2^40 succeeds and leaves a sparse 1 TB file -- so this is a limit of the model, and refusing beats reporting an errno no kernel would have produced."

    /// What this kernel knows about why it cannot complete a write. The client
    /// supplies its own half — which entry point, which descriptor, and what it
    /// would have to build or configure to lift the refusal.
    let describe (refusal : WriteRefusal) : string =
        match refusal with
        | WriteRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | WriteRefusal.UnmodelledSocketPhase (socket, domain, kind, phase) ->
            $"the descriptor is socket %O{socket} (%O{domain}, %O{kind}), in phase %A{phase}. This kernel answers `write(2)` on a socket with no peer, but models no transfer of bytes between sockets, nor what a write reports of a refused connection's error, so it has no answer for a socket that has a peer or such an error."
        | WriteRefusal.Inet6Binding socket ->
            $"the descriptor is socket %O{socket}, an unbound IPv6 datagram socket with no peer. Linux binds it to an ephemeral port before it answers the write, and this kernel binds only IPv4 sockets, so it cannot record that binding."
        | WriteRefusal.EphemeralPortsExhausted (socket, low, high) ->
            $"the descriptor is socket %O{socket}, an unbound datagram socket with no peer, which Linux binds to an ephemeral port before it answers the write; but every port in the ephemeral range %d{low}-%d{high} is taken, and what the kernel answers then is not measured."
        | WriteRefusal.SendBuffer socket ->
            $"the descriptor is socket %O{socket}, a Unix-domain datagram socket with no peer. Linux answers EMSGSIZE for a write larger than the socket's send buffer less 32 bytes, ahead of the ENOTCONN it gives otherwise, and the send buffer's size (SO_SNDBUF, and before that the net.core.wmem_default sysctl) is not modelled."
        | WriteRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            describeExceedsRepresentableLength inode offset count
        | WriteRefusal.Interruption refusal ->
            $"the write was asleep in a pipe: %s{SyscallInterruptionRefusal.describe refusal}"
        | WriteRefusal.InitProcess target ->
            $"the descriptor is %s{BrokenWriteTarget.describe target}, so the write raises SIGPIPE; but the process is process ID 1, and what an init process does with a signal it has no handler for is not modelled."
        | WriteRefusal.SignalReceiver (target, refusal) ->
            $"the descriptor is %s{BrokenWriteTarget.describe target}, so the write answers EPIPE and raises SIGPIPE; but which task would take that signal is not modelled (%A{refusal})."
        | WriteRefusal.UnmeasuredSetIdChange (inode, refusal) -> describeUnmeasuredSetIdChange inode refusal

/// What a `write(2)` this kernel answered did to the process that made it,
/// besides answering: `'Answer` is what the call answered, a `WriteAdmission`
/// from `UnixReadWrite.admitWrite`, a `WriteResumption` from
/// `UnixReadWrite.admitFinishWrite`, or a `WriteAnswer` from
/// `UnixReadWrite.write` and `UnixReadWrite.finishWrite`.
[<RequireQualifiedAccess>]
type WriteOutcome<'Answer, 'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    /// The call answers `answer`, having raised no signal, and the process
    /// carries on as `system`.
    | Returns of answer : 'Answer * system : UnixSystem<'Task, 'Handler>
    /// The call answers `answer`, having generated `signal`, and the process
    /// carries on as `system`: the signal is pending there, or was discarded as
    /// it was generated (see `SignalState.generate`).
    ///
    /// The one signal a write raises is `SIGPIPE`, for a write into a pipe
    /// with no reader or into a Linux stream socket with no peer, which answers
    /// `EPIPE`.
    | ReturnsRaising of answer : 'Answer * signal : PendingSignal<'Task> * system : UnixSystem<'Task, 'Handler>
    /// The call generated a signal whose default action ended the process,
    /// which never returns from it. `EndedProcess.Termination` names the
    /// signal.
    | ProcessEnded of EndedProcess<'Task, 'Handler>
    /// The call did not return: it is a blocking write into a pipe with no room
    /// for the rest of it. The calling task is parked in `system`, and sleeps
    /// until `UnixWait.wakes` wakes it; then `UnixReadWrite.admitFinishWrite`
    /// and `UnixReadWrite.finishWrite` finish the call.
    | WouldBlock of condition : WakeCondition * system : UnixSystem<'Task, 'Handler>
    /// The call was asleep with nothing written, a signal with a handler
    /// interrupted it, and the call restarts (`SyscallInterruption.Restart`):
    /// it never returns. The task is no longer parked in `system`. Once the
    /// handlers have run, the client issues the `write` again with the
    /// arguments it was first made with.
    ///
    /// Only `admitFinishWrite` answers this.
    | Restarts of system : UnixSystem<'Task, 'Handler>

/// Whether a `pwrite` reaches the point at which it reads the caller's buffer,
/// as `WriteAdmission` is for `write`.
///
/// `WriteAdmission` without its sleeping write, rather than the same type: a
/// `pwrite` needs a seekable object, so it never reaches a pipe, the one object
/// whose write sleeps here.
[<RequireQualifiedAccess>]
type PWriteAdmission =
    /// Answered without the buffer being read at all.
    | Answered of answer : WriteAnswer
    /// The copy is reached: extract exactly `count` bytes and pass them to
    /// `pwrite`.
    | Transfer of count : int

/// Why this kernel will not answer a `pwrite`.
///
/// `WriteRefusal` without its socket and pipe cases, rather than the same type:
/// a socket or a pipe is unseekable, so `pwrite` answers ESPIPE and never
/// reaches its own write operation, and a shared type would hand every client
/// arms it could not reach and would have to invent messages for.
[<RequireQualifiedAccess>]
type PWriteRefusal =
    /// The buffer has no answer at the step this `pwrite` reached: its screen, or
    /// the copy it never got to make.
    | Buffer of BufferRefusal
    /// The write would place its last byte past the longest file this kernel can
    /// represent. Easier to reach than `write`'s: the offset is an argument
    /// rather than a position the description was walked to.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int
    /// What writing to the file at `inode` would do to its set-ID bits has not
    /// been measured for this writer.
    | UnmeasuredSetIdChange of inode : InodeNumber * refusal : SetIdChangeRefusal

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
        | PWriteRefusal.UnmeasuredSetIdChange (inode, refusal) ->
            WriteRefusal.describeUnmeasuredSetIdChange inode refusal

/// Why this kernel will not answer a `copy_file_range(2)`.
[<RequireQualifiedAccess>]
type CopyFileRangeRefusal =
    /// This kernel is not Linux-flavoured, and only Linux has `copy_file_range`.
    | UnmodelledFlavour of flavour : SimulatedUnixFlavour
    /// Both descriptors name regular files on a mount of this type, whose
    /// answer to a copy is unmeasured.
    | UnmeasuredFileSystem of fileSystem : EmulatedFileSystemType
    /// The copy would leave the destination longer than this kernel can
    /// represent.
    | ExceedsRepresentableLength of inode : InodeNumber * offset : int64 * count : int
    /// What writing to the destination at `inode` would do to its set-ID bits
    /// has not been measured for this writer.
    | UnmeasuredSetIdChange of inode : InodeNumber * refusal : SetIdChangeRefusal

[<RequireQualifiedAccess>]
module CopyFileRangeRefusal =
    /// What this kernel knows about why it cannot complete a copy. The client
    /// supplies its own half: which entry point, and which descriptors.
    let describe (refusal : CopyFileRangeRefusal) : string =
        match refusal with
        | CopyFileRangeRefusal.UnmodelledFlavour flavour ->
            $"this kernel is %O{flavour}-flavoured, and copy_file_range exists on Linux only."
        | CopyFileRangeRefusal.UnmeasuredFileSystem fileSystem ->
            $"both descriptors name regular files on a %O{fileSystem} mount, where whether a copy is made by the server, by the filesystem or by the generic page-cache path, and so how much one call moves, has not been measured."
        | CopyFileRangeRefusal.ExceedsRepresentableLength (inode, offset, count) ->
            WriteRefusal.describeExceedsRepresentableLength inode offset count
        | CopyFileRangeRefusal.UnmeasuredSetIdChange (inode, refusal) ->
            WriteRefusal.describeUnmeasuredSetIdChange inode refusal

/// What a `read` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
///
/// Narrower than `OpenFileTarget`: it excludes the descriptors a read refuses
/// outright, so a caller that screens the buffer between those two steps — as
/// `vfs_read` does — has no unreachable arm left to write.
[<RequireQualifiedAccess>]
type private ReadTarget =
    /// The read end of a pipe, the open file description it was reached
    /// through, and whether that description carries `O_NONBLOCK`.
    | Pipe of pipe : PipeId * description : OpenFileDescriptionId * nonBlocking : bool
    /// A file, at the offset its open file description currently holds.
    | File of inode : InodeNumber * offset : int64
    /// A socket, and whether the description it was reached through carries
    /// `O_NONBLOCK`.
    | Socket of socket : SocketId * nonBlocking : bool
    /// A directory, which has no byte contents to read, at the position its
    /// open file description holds.
    | Directory of inode : InodeNumber * position : DirectoryPosition

/// What a `write` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
[<RequireQualifiedAccess>]
type private WriteTarget =
    /// A file. The offset is the description's own, and the write advances it —
    /// which is the whole difference from `pwrite`.
    | File of inode : InodeNumber * offset : int64
    /// A socket, which answers only once the buffer screen has had its say,
    /// which on one flavour answers first.
    | Socket of socket : SocketId
    /// The write end of a pipe, the open file description it was reached
    /// through, and whether that description carries `O_NONBLOCK`.
    | Pipe of pipe : PipeId * description : OpenFileDescriptionId * nonBlocking : bool

/// How far a write into a pipe gets before it needs the caller's bytes.
[<RequireQualifiedAccess>]
type private PipeWriteStep =
    /// The write is answered without the bytes, and leaves the buffer alone.
    | Answered of WriteAnswer
    /// The write is answered without the bytes, and takes none of them, but
    /// leaves the buffer as `PipeBuffer.withoutTaking` says.
    | TakesNothing of WriteAnswer
    | Refused of WriteRefusal
    /// The pipe has no reader: the write answers `EPIPE` and raises `SIGPIPE`,
    /// takes nothing, and leaves the pipe and its timestamps alone.
    | Broken
    /// The write takes `taken` of the bytes offered, from the start, and
    /// answers that count.
    | Takes of taken : int
    /// A blocking write that takes nothing now, without reading the bytes, and
    /// sleeps; it leaves the buffer as `PipeBuffer.withoutTaking` says.
    | Sleeps
    /// A blocking write that takes `taken` of the bytes offered, from the
    /// start, fewer than all of them, and sleeps for the rest.
    | TakesThenSleeps of taken : int

[<RequireQualifiedAccess>]
module UnixReadWrite =

    /// Whether this platform refuses `count` outright, as EINVAL, before it
    /// looks at anything else about the call. See `TransferCountLimit.Refused`.
    let private countRefused (platform : SimulatedUnixPlatform) (count : uint64) : bool =
        match SimulatedUnixPlatform.transferCountLimit platform with
        | TransferCountLimit.Refused maxTransfer -> count > uint64 maxTransfer
        | TransferCountLimit.Shortened _ -> false

    /// What a platform answers of a transfer's position alone, ahead of the
    /// object's own operation. Only an object with a position is asked: a pipe
    /// or a socket has none.
    [<RequireQualifiedAccess>]
    type private PositionCheck =
        /// The object's own operation answers.
        | Passes
        /// Linux: position + count passes `INT64_MAX`. EINVAL.
        | Overflows
        /// Darwin: the position is `INT64_MAX` itself. A read there is
        /// end-of-file, a directory's included, and a write is EFBIG.
        | AtMaximum

    /// What this platform answers of a transfer of `count` bytes at file
    /// position `position`, from the position alone. `count` is the whole count
    /// asked for.
    let private positionCheck (platform : SimulatedUnixPlatform) (position : int64) (count : uint64) : PositionCheck =
        System.Diagnostics.Debug.Assert (position >= 0L, "positionCheck: a file position is never negative")

        // Measured by transfer-counts-position.c in
        // docs/plans/2026-08-23-posix-kernel-extraction/, for read and write at
        // the description's position as for pread and pwrite at the argument.
        //
        // Linux checks after the buffer screen and before anything the object
        // does (a directory's EISDIR, end-of-file's 0, the copy), over the count
        // as asked rather than as shortened. A count of zero never passes.
        //
        // Darwin checks no sum: a read past end-of-file is 0 at every position.
        // But at INT64_MAX itself, and nowhere below it, a directory read is 0
        // rather than EISDIR, and a write to a regular file is EFBIG for every
        // count its count check admits, zero included, and whatever the buffer.
        // The same on APFS and on HFS+, so it is the platform's rather than a
        // filesystem's. Darwin screens no buffer, so nothing orders this
        // against a screen.
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            if count > uint64 (System.Int64.MaxValue - position) then
                PositionCheck.Overflows
            else
                PositionCheck.Passes
        | SimulatedUnixFlavour.Darwin ->
            if position = System.Int64.MaxValue then
                PositionCheck.AtMaximum
            else
                PositionCheck.Passes

    /// `positionCheck` for a directory description at `position`. `Error`
    /// where the answer depends on a position this kernel cannot state: the
    /// filesystem's own cookie partway through a scan.
    let private directoryPositionCheck
        (platform : SimulatedUnixPlatform)
        (fileSystem : EmulatedFileSystemType)
        (position : DirectoryPosition)
        (count : uint64)
        : Result<PositionCheck, unit>
        =
        match position with
        | DirectoryPosition.Cursor DirectoryCursor.Start -> Ok (positionCheck platform 0L count)
        | DirectoryPosition.Unenumerable offset -> Ok (positionCheck platform offset count)
        | DirectoryPosition.Cursor (DirectoryCursor.After _)
        | DirectoryPosition.Cursor DirectoryCursor.ReturnedDotDot
        | DirectoryPosition.Cursor DirectoryCursor.ReturnedDot ->
            // The largest position a description partway through a scan can
            // hold, each measured by a probe in
            // docs/plans/2026-08-23-posix-kernel-extraction/.
            // - tmpfs, on Linux 6.18.5 aarch64 (transfer-counts-directory.c):
            //   each entry's own small offset partway through, and INT_MAX once
            //   the scan is done.
            // - APFS, on Darwin 27.0.0 arm64
            //   (transfer-counts-darwin-directory.c): INT_MAX once the scan is
            //   done, and partway through the number of getdirentries calls made
            //   in the high 32 bits and the entries returned so far in the low
            //   ones. INT64_MAX would take 2^31 - 1 calls that returned 2^32 - 1
            //   entries between them, so the bound is one below it.
            // - NFS: the server's cookie, which can be anything up to INT64_MAX
            //   (ext4's 64-bit hash cookies end at it).
            let largest =
                match fileSystem with
                | EmulatedFileSystemType.Tmpfs -> int64 System.Int32.MaxValue
                | EmulatedFileSystemType.Apfs -> System.Int64.MaxValue - 1L
                | EmulatedFileSystemType.Nfs -> System.Int64.MaxValue

            // The position is somewhere in [0, largest]. Each flavour's check is
            // monotone in it (Linux's sum passes INT64_MAX from some position
            // on, and Darwin's position is INT64_MAX at the top end alone), so
            // the two ends decide it whenever they agree.
            let atStart = positionCheck platform 0L count

            if atStart = positionCheck platform largest count then
                Ok atStart
            else
                Error ()

    /// What a read answers from its position alone, or `None` where the object
    /// answers. The end-of-file answer moves nothing, so a description's
    /// position stays where it was.
    let private readAnsweredByPosition (check : PositionCheck) : ReadAnswer option =
        match check with
        | PositionCheck.Passes -> None
        | PositionCheck.Overflows -> Some (ReadAnswer.Failed UnixError.EINVAL)
        | PositionCheck.AtMaximum -> Some (ReadAnswer.Completed ImmutableArray.Empty)

    /// What a write to a regular file answers from its position alone, or
    /// `None` where the file answers.
    let private writeFailedByPosition (check : PositionCheck) : UnixError option =
        match check with
        | PositionCheck.Passes -> None
        | PositionCheck.Overflows -> Some UnixError.EINVAL
        | PositionCheck.AtMaximum -> Some UnixError.EFBIG

    /// The count the object's own operation sees: at most one call's worth.
    /// Where the platform refuses a longer count instead, that refusal has
    /// already been made, so this is the count itself.
    let private oneCallsWorth (platform : SimulatedUnixPlatform) (count : uint64) : int =
        let maxTransfer =
            TransferCountLimit.maxTransfer (SimulatedUnixPlatform.transferCountLimit platform)

        int (min count (uint64 maxTransfer))

    /// `write` and `pwrite` take the bytes their admission said to extract,
    /// which is at most one call's worth; more is a caller that skipped the
    /// admission.
    let private assertOneCallsWorth
        (syscall : string)
        (platform : SimulatedUnixPlatform)
        (bytes : ImmutableArray<byte>)
        : unit
        =
        let maxTransfer =
            TransferCountLimit.maxTransfer (SimulatedUnixPlatform.transferCountLimit platform)

        if bytes.Length > maxTransfer then
            failwith
                $"UnixReadWrite.%s{syscall}: given %d{bytes.Length} bytes, more than the %d{maxTransfer} one call moves on this platform. Pass the bytes the admission said to transfer, whose count is already one call's worth (this is a bug in the caller)."

    /// `system` with `pipeId`'s table entry replaced by `pipe`.
    let private withPipe<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (pipe : PipeState)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine =
                { system.Machine with
                    Pipes = Map.add pipeId pipe system.Machine.Pipes
                }
        }

    /// `system` with the timestamps of the pipe `pipeId` passed through
    /// `touch`, if this kernel holds them: it holds none for a pipe the process
    /// was launched with, whose timestamps are the launcher's.
    let private withTimes<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (touch : PipeTimes -> PipeTimes)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let pipe = UnixMachineState.pipe pipeId system.Machine

        match pipe.Origin with
        | PipeOrigin.Launched _ -> system
        | PipeOrigin.Made status ->
            withPipe
                pipeId
                { pipe with
                    Origin =
                        PipeOrigin.Made
                            { status with
                                Times = touch status.Times
                            }
                }
                system

    /// What a read reaching `pipeId`'s read operation does to its timestamps:
    /// on Darwin, moves the read end's `st_atime` to now; on Linux, nothing.
    let private touchedByRead<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> system
        | SimulatedUnixFlavour.Darwin ->
            let now = UnixMachineState.realtime system.Machine

            withTimes
                pipeId
                (fun times ->
                    { times with
                        ReadEndAccess = now
                    }
                )
                system

    /// What a write reaching `pipeId`'s write operation does to its timestamps:
    /// on Darwin, moves `st_mtime` and `st_ctime` of both ends to now; on
    /// Linux, nothing.
    let private touchedByWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> system
        | SimulatedUnixFlavour.Darwin ->
            let now = UnixMachineState.realtime system.Machine

            withTimes
                pipeId
                (fun times ->
                    { times with
                        Modification = now
                        StatusChange = now
                    }
                )
                system

    /// `system` with `pipeId`'s buffer as a write of `count` bytes that took none
    /// of them leaves it.
    let private leftUntaken<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let pipe = UnixMachineState.pipe pipeId system.Machine

        withPipe
            pipeId
            { pipe with
                Buffer = PipeBuffer.withoutTaking count pipe.Buffer
            }
            system

    /// The buffer once `bytes` have passed through it to a reader that takes
    /// every byte the moment it is written: the write puts in what the buffer
    /// takes, the reader takes all of it, and so on until the last byte has
    /// gone. Empty, as it was; on Darwin, grown as those writes grew it.
    ///
    /// Also whether the reader ever took bytes from a buffer that was full,
    /// with no room for a write: that read is what wakes the write end's
    /// waiters.
    let private drainedThrough (bytes : ImmutableArray<byte>) (buffer : PipeBuffer) : PipeBuffer * bool =
        let rec pass (offset : int) (filled : bool) (buffer : PipeBuffer) : PipeBuffer * bool =
            if offset = bytes.Length then
                buffer, filled
            else

            let remaining = bytes.Length - offset
            let fits = PipeBuffer.wouldTake remaining buffer

            if fits = 0 then
                failwith
                    $"UnixReadWrite.drainedThrough: a pipe whose reader had just emptied it took none of %d{remaining} bytes (this is a bug in this library: an empty pipe takes something of every write)."

            let written, buffer =
                PipeBuffer.write (ImmutableArray.Create (bytes, offset, fits)) buffer

            if written <> fits then
                failwith
                    $"UnixReadWrite.drainedThrough: the pipe was to take %d{fits} bytes and took %d{written}; PipeBuffer.wouldTake and PipeBuffer.write disagree (this is a bug in this library)."

            let full = not (PipeBuffer.writable buffer)
            let _, buffer = PipeBuffer.read (PipeBuffer.held buffer) buffer
            pass (offset + written) (filled || full) buffer

        pass 0 false buffer

    /// Everything a write of `count` bytes into `pipeId` decides before it
    /// needs the bytes themselves, in the order the flavour decides it. `count`
    /// is already one call's worth, and `buffer` is `Mapped` for a caller that
    /// holds the bytes.
    ///
    /// Changes nothing: `touchedByWrite` is the caller's to apply, to every
    /// outcome but a refusal and `Broken`, `leftUntaken` to `TakesNothing` and
    /// `Sleeps`, and `broken` to `Broken`.
    let private pipeWriteStep<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (nonBlocking : bool)
        (count : int)
        (buffer : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : PipeWriteStep
        =
        let pipe = UnixMachineState.pipe pipeId system.Machine

        let readerOpen =
            UnixProcessState.pipeEndOpen pipeId pipe PipeEnd.Read system.Process

        // A write of no bytes, and a write with no reader: measured
        // (pipe-sigpipe.c, pipe-syscalls.c and pipe-epipe-sweep.c), Linux
        // answers 0 to the first whatever else holds -- a closed reader and a
        // full blocking pipe included -- and raises no SIGPIPE, while Darwin
        // answers EPIPE ahead of it, and raises SIGPIPE.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux when count = 0 -> PipeWriteStep.Answered (WriteAnswer.Completed 0L)
        | SimulatedUnixFlavour.Linux
        | SimulatedUnixFlavour.Darwin ->

        // EPIPE is ahead of EAGAIN, EFAULT and a short count on both, and
        // moves no timestamp even on Darwin, measured (pipe-epipe-sweep.c).
        if not readerOpen then
            PipeWriteStep.Broken
        elif count = 0 then
            // Darwin's zero-length write to a pipe with a reader: 0, full or
            // not, blocking or not, measured.
            PipeWriteStep.Answered (WriteAnswer.Completed 0L)
        else

        let taken =
            match PipeState.drainedBy pipe with
            // The client reads every byte as it arrives, so a blocking write
            // never waits: the pipe fills, the client empties it, and the write
            // carries on until the whole has gone.
            | Some _ when not nonBlocking -> count
            // A non-blocking write takes what the pipe takes now, the reader
            // having emptied it, and no more however fast that reader is:
            // measured on both flavours (stdio-nonblock.c), 20 writes to each
            // output stream of a process launched onto pipes, of each of
            // sixteen sizes from 1 byte to 1 MiB, each after the pipe had
            // drained, were whole up to 65536 bytes and 65536 beyond.
            | Some _
            | None -> PipeBuffer.wouldTake count pipe.Buffer

        if taken = 0 then
            // Nothing fits, so nothing is copied and the buffer is never looked
            // at: measured on both, a non-blocking write through a bad pointer
            // into a full pipe is EAGAIN, not EFAULT, and a blocking one sleeps
            // (pipe-blocking.c section G2), faulting only once there is room.
            if nonBlocking then
                PipeWriteStep.TakesNothing (WriteAnswer.Failed UnixError.EAGAIN)
            else
                PipeWriteStep.Sleeps
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // The copy faults before anything is taken: measured on both, EFAULT
            // into an empty pipe, which is left empty -- but on Darwin grown
            // (pipe-fault-aftermath.c).
            PipeWriteStep.TakesNothing (WriteAnswer.Failed UnixError.EFAULT)
        | UserBuffer.Opaque -> PipeWriteStep.Refused (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> PipeWriteStep.Refused (WriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped ->

        if taken < count && not nonBlocking then
            // Measured on both (pipe-blocking.c section D): a blocking write
            // puts in what fits and sleeps for the rest.
            PipeWriteStep.TakesThenSleeps taken
        else
            // A non-blocking write takes what fits and answers that count:
            // `PipeBuffer.write` is the measured rule.
            PipeWriteStep.Takes taken

    /// A write by `task` into `target`, which has no reader or no peer: it
    /// answers `answer`, `EPIPE`, and raises `SIGPIPE` before it returns, which
    /// the process's disposition for the signal then decides the fate of.
    let private broken<'Answer, 'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (answer : 'Answer)
        (task : 'Task)
        (target : BrokenWriteTarget)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<'Answer, 'Task, 'Handler>, WriteRefusal>
        =
        // Measured (pipe-sigpipe.c), a worker thread's write into a pipe:
        // Linux's handler ran on the writing thread, before its write
        // returned; Darwin's ran on the main thread, after the worker's write
        // had returned, as a signal sent to the process is delivered (see
        // `SignalState.generate`). A Linux stream socket with no peer raises it
        // as a pipe does (socket-unconnected-transfer.c: on the writing thread,
        // before the write returned); no Darwin socket with no peer raises it.
        if ProcessId.toInt32 system.Process.ProcessId = 1 then
            Error (WriteRefusal.InitProcess target)
        else

        let receiver =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> ValueSome task
            | SimulatedUnixFlavour.Darwin -> ValueNone

        let entry =
            {
                Signal = Signal.SIGPIPE
                Target = receiver
            }

        let generation =
            SignalState.generate
                system.Process.CoreDumps
                system.Leader
                (system.Tasks |> Map.keys |> Set.ofSeq)
                entry
                system.Process.Signals

        match generation with
        | Error refusal -> Error (WriteRefusal.SignalReceiver (target, refusal))
        | Ok (SignalGeneration.ProcessContinues signals) ->
            Ok (
                WriteOutcome.ReturnsRaising (
                    answer,
                    entry,
                    { system with
                        Process =
                            { system.Process with
                                Signals = signals
                            }
                    }
                )
            )
        | Ok (SignalGeneration.ProcessTerminated (signal, coreDumped)) ->
            UnixTaskLifecycle.endProcess (ProcessTermination.Signaled (signal, coreDumped)) system
            |> WriteOutcome.ProcessEnded
            |> Ok
        | Ok (SignalGeneration.ProcessStopped (signal, _)) ->
            failwith
                $"UnixReadWrite: generating %O{signal} for a write into %s{BrokenWriteTarget.describe target} stopped the process, but SIGPIPE's default is to terminate on every flavour (this is a bug in this library)."

    /// A write by `task` of `count` bytes to `socketId`, past the buffer screen:
    /// the socket's own answer, as `wrap` makes it the caller's. It never reads
    /// the buffer, because no socket this kernel answers for takes bytes.
    let private socketWrite<'Answer, 'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (wrap : WriteAnswer -> 'Answer)
        (task : 'Task)
        (socketId : SocketId)
        (count : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<'Answer, 'Task, 'Handler>, WriteRefusal>
        =
        let socket = UnixMachineState.socket socketId system.Machine
        let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

        match socket.Phase with
        | SocketPhase.Idle
        | SocketPhase.Listening _ ->
            // The binding a write makes before it answers, if any: to the
            // wildcard and an ephemeral port, with nothing locked, as
            // `listen(2)`'s implicit bind is.
            let bound : Result<UnixSystem<'Task, 'Handler>, WriteRefusal> =
                match socket.Binding with
                | Some _ -> Ok system
                | None when not (UnconnectedSocketRules.writeBindsFirst flavour socket.Domain socket.Kind) -> Ok system
                | None ->

                match socket.Domain with
                | SocketDomain.Inet6 -> Error (WriteRefusal.Inet6Binding socketId)
                | SocketDomain.Unix ->
                    failwith
                        $"UnixReadWrite: a write on Unix-domain socket %O{socketId} binds it first, which no flavour does (this is a bug in this library)."
                | SocketDomain.Inet ->

                let candidate (port : uint16) : SocketBinding =
                    {
                        Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port
                        LockedAddress = None
                        LockedPort = false
                    }

                match
                    UnixMachineState.allocateEphemeralPort
                        EphemeralPortUse.Reserve
                        socketId
                        socket
                        candidate
                        system.Machine
                with
                | None ->
                    let low, high = system.Machine.EphemeralPortRange
                    Error (WriteRefusal.EphemeralPortsExhausted (socketId, low, high))
                | Some (binding, machine) ->
                    Ok
                        { system with
                            Machine =
                                { machine with
                                    Sockets =
                                        Map.add
                                            socketId
                                            { socket with
                                                Binding = Some binding
                                            }
                                            machine.Sockets
                                }
                        }

            match UnconnectedSocketRules.write flavour socket.Domain socket.Kind count, bound with
            | UnconnectedSocketWrite.DependsOnSendBuffer, _ -> Error (WriteRefusal.SendBuffer socketId)
            | _, Error refusal -> Error refusal
            | answer, Ok system ->

            match answer with
            | UnconnectedSocketWrite.Fails error -> Ok (WriteOutcome.Returns (wrap (WriteAnswer.Failed error), system))
            | UnconnectedSocketWrite.Breaks ->
                broken (wrap (WriteAnswer.Failed UnixError.EPIPE)) task (BrokenWriteTarget.Socket socketId) system
            | UnconnectedSocketWrite.DependsOnSendBuffer -> Error (WriteRefusal.SendBuffer socketId)
        | SocketPhase.EstablishedPendingReport _
        | SocketPhase.Established _
        | SocketPhase.DatagramPeer _
        | SocketPhase.Refused _ ->
            // A refused socket is here too: measured, its write is EPIPE and
            // SIGPIPE on both, where Darwin's socket with no peer answers
            // ENOTCONN (socket-unconnected-transfer-after.c), and what a
            // pending error does to it is unmeasured.
            Error (WriteRefusal.UnmodelledSocketPhase (socketId, socket.Domain, socket.Kind, socket.Phase))

    /// Fails loudly unless `task` is one of `system`'s tasks and is not
    /// already asleep in a syscall: a task makes one call at a time, and a
    /// call that sleeps is finished, not issued again.
    let private checkIssuer<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (syscall : string)
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : unit
        =
        match Map.tryFind task system.Tasks with
        | None ->
            failwith
                $"UnixReadWrite.%s{syscall}: task %O{task} is not one of the process's tasks, so it cannot be making the call (this is a bug in the client)."
        | Some {
                   Parked = Some park
               } ->
            failwith
                $"UnixReadWrite.%s{syscall}: task %O{task} is asleep in %A{park.Syscall}, and is issuing a %s{syscall}. A task blocks in one syscall at a time, and a sleeping one is finished rather than issued again (this is a bug in the client)."
        | Some _ -> ()

    /// A read of up to `count` bytes taking them from `pipeId`, which holds
    /// some: the bytes, and the system after the read and whatever it set off.
    let private takeFromPipe<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pipeId : PipeId)
        (count : int)
        (system : UnixSystem<'Task, 'Handler>)
        : ImmutableArray<byte> * UnixSystem<'Task, 'Handler>
        =
        let pipe = UnixMachineState.pipe pipeId system.Machine

        // What the pipe holds, up to the count, without waiting for more:
        // measured on both, three 5-byte writes read back as one 15-byte
        // read, and a read of 3 from 10 held leaves 7.
        let bytes, remaining = PipeBuffer.read count pipe.Buffer

        // A client asleep in its write wakes into the room this read made,
        // and has written before the process's next call: the fastest a
        // real writer can be, and what a real one is measured to do given a
        // moment (supplied-pipe-refill.c: a reader that waits 20 ms after
        // each read finds the pipe exactly as full as this, on both
        // flavours, over 120 runs each). A reader on Linux sees it so even
        // without waiting; on Darwin, one that reads again at once can
        // find less, which a slower reader never does.
        let pipe, progress =
            PipeState.afterRead
                { pipe with
                    Buffer = remaining
                    Reads = if bytes.IsEmpty then pipe.Reads else pipe.Reads + 1L
                }

        // Measured on Linux 6.18.5 (supplied-pipe-epoll.c): the client's
        // write wakes an edge-triggered registration on the read end
        // exactly when it writes into a pipe the read had emptied, with
        // `EPOLLIN | EPOLLRDNORM`; and its close wakes the registration
        // whatever it waits for, reporting `EPOLLHUP` even to an
        // `EPOLLOUT`-only one, so the close's wake is unkeyed.
        let registry =
            let readers =
                UnixProcessState.descriptionsNamingPipeEnd pipeId PipeEnd.Read system.Process

            let registry = system.Process.FileDescriptors

            let registry =
                if progress.WroteIntoEmpty then
                    FileDescriptorRegistry.signalSocketEventPorts
                        readers
                        (Some (EpollEvents.In ||| EpollEvents.RdNorm))
                        registry
                else
                    registry

            if progress.Closed then
                FileDescriptorRegistry.signalSocketEventPorts readers None registry
            else
                registry

        bytes,
        { withPipe pipeId pipe system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

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

        // A range copy rather than an enumeration: one call can move nearly two
        // gigabytes.
        ImmutableArray.Create (contents, int offset, transfer)
        |> ReadAnswer.Completed
        |> Ok

    /// `read(2)`: move up to `count` bytes from `fd`'s current offset into the
    /// caller's buffer, and advance the offset by what actually moved.
    ///
    /// `count` is the `size_t` the caller asked for. A count longer than one
    /// call moves is refused or shortened as the platform's
    /// `TransferCountLimit` says.
    ///
    /// The buffer is consulted at three points and *not* consulted at three
    /// others, and both sets are measured; see the comments inline.
    ///
    /// A socket with no peer answers without the buffer, as
    /// `UnconnectedSocketRules.read` says, except that a blocking read of a
    /// datagram socket, which sleeps until a datagram arrives, is refused. A
    /// socket with a peer, or with a refused connection's error, is refused,
    /// except for Linux's zero-length read, which is 0 in every phase.
    ///
    /// A read by `task` of a pipe that holds nothing while a write end is open,
    /// through a description without `O_NONBLOCK`, sleeps (`ReadOutcome.WouldBlock`)
    /// without looking at the buffer, and `finishRead` finishes it. Setting
    /// `O_NONBLOCK` on the description while it sleeps does not wake it.
    ///
    /// Fails loudly if `task` is not one of the process's tasks, or is already
    /// asleep in a syscall.
    let read<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadOutcome * UnixSystem<'Task, 'Handler>, ReadRefusal>
        =
        checkIssuer "read" task system
        let platform = system.Machine.UnixPlatform

        let answered (answer : ReadAnswer) (system : UnixSystem<'Task, 'Handler>) =
            Ok (ReadOutcome.Answered answer, system)

        if countRefused platform count then
            answered (ReadAnswer.Failed UnixError.EINVAL) system
        else

        // The descriptor's access mode, which Linux's `vfs_read` decides before
        // it screens the buffer: measured on both platforms,
        // `read(wronlyFd, (void*)-1, 4)` is EBADF rather than EFAULT, and even
        // `read(wronlyFd, buf, 0)` is EBADF rather than a no-op.
        let target : Result<ReadTarget, UnixError> =
            match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
            | None -> Error UnixError.EBADF
            | Some (descriptionId, description) ->

            if not (FileAccessMode.permitsRead description.AccessMode) then
                // A regular file opened `O_WRONLY` and a pipe's write end alike:
                // EBADF on both platforms. `read` has no seekability
                // requirement, so unlike `pread` there is no tie for the
                // platforms to break differently.
                Error UnixError.EBADF
            else

            match description.Target with
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
                // length, unlike a pipe's zero-return shortcut below.
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
                | SimulatedUnixFlavour.Darwin -> Error UnixError.ENXIO
            | OpenFileTarget.Socket socketId -> Ok (ReadTarget.Socket (socketId, description.NonBlocking))
            | OpenFileTarget.File (inode, offset) -> Ok (ReadTarget.File (inode, offset))
            | OpenFileTarget.Directory (inode, position) -> Ok (ReadTarget.Directory (inode, position))
            | OpenFileTarget.Pipe (pipeId, PipeEnd.Read) ->
                Ok (ReadTarget.Pipe (pipeId, descriptionId, description.NonBlocking))
            | OpenFileTarget.Pipe (pipeId, PipeEnd.Write) ->
                failwith
                    $"UnixReadWrite.read: fd %d{fd} names the write end of pipe %O{pipeId}, whose access mode permits reading. `pipe(2)` opens the write end O_WRONLY and nothing reopens it (this is a bug in this library)."

        match target with
        | Error error -> answered (ReadAnswer.Failed error) system
        | Ok target ->

        // Everything below this point is the object's own read operation, which
        // on Linux the buffer screen precedes: hence EFAULT ahead of EISDIR, of
        // a pipe's end-of-file and of a socket's own answer, and a
        // fault even for a zero-length request. Darwin screens nothing here, so
        // its answers come from the operation itself.
        match
            UserBufferCheck.faultsBeforeOperationFor (UnixMachineState.userBufferCheck system.Machine) buffer count
        with
        | Error refusal -> Error (ReadRefusal.Buffer refusal)
        | Ok true -> answered (ReadAnswer.Failed UnixError.EFAULT) system
        | Ok false ->

        match target with
        | ReadTarget.Socket (socketId, nonBlocking) ->
            // A zero-length read of a socket is where the flavours part, and on
            // one of them it needs no phase at all. Measured across every phase
            // this kernel can produce and every kind it models,
            // `read(sock, buf, 0)`:
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
            // alone decides it here, ahead of the phase: a connected socket's
            // zero-length read is answered although its longer ones are not.
            // Darwin's is 0 too except for a stream socket that is not
            // connected, which `UnconnectedSocketRules` answers below with the
            // rest of a socket without a peer.
            //
            // The socket event port does not share the shortcut and is answered
            // above: measured, `read(port, buf, 0)` is EINVAL on Linux like
            // every other length.
            let flavour = SimulatedUnixPlatform.flavour system.Machine.UnixPlatform

            match flavour, count with
            | SimulatedUnixFlavour.Linux, 0UL -> answered (ReadAnswer.Completed ImmutableArray.Empty) system
            | SimulatedUnixFlavour.Linux, _
            | SimulatedUnixFlavour.Darwin, _ ->

            let socket = UnixMachineState.socket socketId system.Machine

            match socket.Phase with
            | SocketPhase.Idle
            | SocketPhase.Listening _ ->
                // No peer, so no byte can be on its way: a socket fresh from
                // `socket(2)`, bound, or listening, or one a Linux connect left
                // idle (a refusal it reported, or an `AF_UNSPEC` dissolve), which
                // answers as a fresh one does (socket-unconnected-transfer-after.c).
                match UnconnectedSocketRules.read flavour socket.Domain socket.Kind nonBlocking count with
                | UnconnectedSocketRead.Empty -> answered (ReadAnswer.Completed ImmutableArray.Empty) system
                | UnconnectedSocketRead.Fails error -> answered (ReadAnswer.Failed error) system
                | UnconnectedSocketRead.Sleeps -> Error (ReadRefusal.DatagramSleep (socketId, socket.Domain))
            | SocketPhase.EstablishedPendingReport _
            | SocketPhase.Established _
            | SocketPhase.DatagramPeer _
            | SocketPhase.Refused _ ->
                // A refused socket is here too: measured, its read is a pending
                // ECONNREFUSED on Darwin and end-of-file once the error is
                // reported, on both (socket-unconnected-transfer-after.c).
                Error (ReadRefusal.UnmodelledSocketPhase (socketId, socket.Domain, socket.Kind, socket.Phase))
        | ReadTarget.Directory (inode, position) ->
            // A directory has a position too, and each flavour's position rule
            // answers ahead of EISDIR, exactly as for a file.
            let fileSystem = EmulatedMount.fileSystemType system.Machine.Mount

            match directoryPositionCheck platform fileSystem position count with
            | Error () -> Error (ReadRefusal.ScannedDirectoryPosition (inode, fileSystem))
            | Ok check ->

            match readAnsweredByPosition check with
            | Some answer -> answered answer system
            | None ->
                // EISDIR on both, and behind the buffer screen rather than ahead
                // of it: measured, `read(dir, NULL, 5)` is EISDIR while
                // `read(dir, (void*)-1, 5)` is EFAULT under a screening flavour.
                // The same answer `pread` reaches through the directory's
                // content.
                answered (ReadAnswer.Failed UnixError.EISDIR) system
        | ReadTarget.Pipe (pipeId, descriptionId, nonBlocking) ->
            // On Darwin every read that reaches the pipe moves the read end's
            // atime as it returns, whatever it answers: measured
            // (pipe-syscalls.c), a read of 0 bytes, one answering EAGAIN, one
            // answering EFAULT, one at end of file and one that moves bytes all
            // do. A read that sleeps moves it only once it ends
            // (pipe-blocking.c section L). Linux moves none.
            let asleep = system
            let system = touchedByRead pipeId system
            let pipe = UnixMachineState.pipe pipeId system.Machine

            // Measured on both flavours: a read of 0 bytes answers 0 at once,
            // whatever the pipe holds and whether or not it would otherwise
            // wait -- a blocking `read(fd, NULL, 0)` on an empty pipe whose
            // writer stays open returns immediately.
            let count = oneCallsWorth platform count

            if count = 0 then
                answered (ReadAnswer.Completed ImmutableArray.Empty) system
            elif PipeBuffer.held pipe.Buffer = 0 then
                // Nothing held, so the buffer is not consulted: measured on
                // both, `read(fd, NULL, 10)` of an empty pipe is EAGAIN while
                // a writer is open and 0 once none is, never EFAULT.
                // A process launched with this pipe as its standard input is
                // here once it has read every byte the launcher supplied, and
                // the launcher has closed its end: measured on both flavours
                // (stdio-nonblock.c), a read of a stdin supplied nothing is 0,
                // `O_NONBLOCK` or not. A pipe whose launcher is still writing
                // is never empty, so never here.
                if not (UnixProcessState.pipeEndOpen pipeId pipe PipeEnd.Write system.Process) then
                    answered (ReadAnswer.Completed ImmutableArray.Empty) system
                elif nonBlocking then
                    answered (ReadAnswer.Failed UnixError.EAGAIN) system
                else
                    // Measured on both (pipe-states.c, and pipe-blocking.c
                    // section G1): the read sleeps until bytes arrive or the
                    // last writer closes, and a buffer naming no storage faults
                    // only once there are bytes to copy.
                    let parked =
                        ParkedSyscall.PipeRead
                            {
                                Reader = descriptionId
                                Buffer = buffer
                                Count = count
                            }

                    Ok (ReadOutcome.WouldBlock (WakeCondition.ofPark parked), UnixWait.park task parked asleep)
            else

            match buffer with
            | UserBuffer.Unmapped _ ->
                // Measured on both: EFAULT, and the bytes stay in the pipe.
                answered (ReadAnswer.Failed UnixError.EFAULT) system
            | UserBuffer.Opaque -> Error (ReadRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (ReadRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
            | UserBuffer.Mapped ->

            let bytes, system = takeFromPipe pipeId count system
            answered (ReadAnswer.Completed bytes) system
        | ReadTarget.File (inode, offset) ->

        match readAnsweredByPosition (positionCheck platform offset count) with
        | Some answer -> answered answer system
        | None ->

        // The window is computed from the description's own offset, which is the
        // whole of what `pread` does differently; everything after it is the
        // same operation, so the two share it.
        match readFileAt "read" fd inode offset buffer (oneCallsWorth platform count) system with
        | Error refusal -> Error (ReadRefusal.Buffer refusal)
        | Ok (ReadAnswer.Failed error) -> answered (ReadAnswer.Failed error) system
        | Ok (ReadAnswer.Completed bytes) ->

        if bytes.IsEmpty then
            // Nothing moved, so the offset stays exactly where it was rather
            // than being clamped to the file's length or rewritten to itself.
            answered (ReadAnswer.Completed bytes) system
        else

        // Advanced by what actually moved, not by what was asked for: a short
        // read at the end of a file leaves the offset at the end rather than
        // past it, which is what makes a subsequent read return 0 instead of a
        // second short read.
        answered
            (ReadAnswer.Completed bytes)
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

    /// The pipe the open file description `description`, which a task parked in
    /// `syscall` holds, names `pipeEnd` of.
    let private parkedPipe<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (syscall : string)
        (task : 'Task)
        (description : OpenFileDescriptionId)
        (pipeEnd : PipeEnd)
        (system : UnixSystem<'Task, 'Handler>)
        : PipeId
        =
        match
            FileDescriptorRegistry.descriptions system.Process.FileDescriptors
            |> Map.tryFind description
        with
        | None ->
            failwith
                $"UnixReadWrite.%s{syscall}: task %O{task} sleeps on open file description %O{description}, which is not in the table, so it was closed underneath the call. `close` refuses such a close (this is a bug in this library, or in a caller that destroyed the description without UnixDescriptor.close)."
        | Some found ->
            match found.Target with
            | OpenFileTarget.Pipe (pipeId, named) when named = pipeEnd -> pipeId
            | target ->
                failwith
                    $"UnixReadWrite.%s{syscall}: task %O{task} sleeps on open file description %O{description}, which names %A{target} rather than the %A{pipeEnd} end of a pipe (this is a bug in the caller that recorded the park)."

    /// Whether a transfer asleep on the open file description `description`,
    /// woken and finding nothing to take, gives up rather than sleeping again:
    /// on Darwin, if the description carries `O_NONBLOCK` by now.
    ///
    /// Measured (pipe-blocking.c sections N3 and N5): Darwin wakes every
    /// sleeper, and the one that finds nothing left looks at the flag again,
    /// answering EAGAIN for a read and for a write with nothing in, while
    /// Linux re-checks its condition before it leaves its sleep, so a sleeper
    /// that finds nothing sleeps on whatever the flag says. Setting the flag
    /// wakes neither (section I).
    let private givesUpWhenBeaten<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (description : OpenFileDescriptionId)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin ->
            (FileDescriptorRegistry.descriptions system.Process.FileDescriptors).[description].NonBlocking

    /// Finish the `read` `task` is asleep in: look at the pipe again, as a woken
    /// real read does, and answer.
    ///
    /// Bytes in the pipe are read, up to the count the call was made with, into
    /// the buffer it was made with: a buffer naming no storage answers `EFAULT`
    /// and leaves them there. A pipe still empty with no write end open is end
    /// of file. Under Linux either beats a signal pending for the task, whose
    /// handlers run as the call returns; under Darwin, a kernel answers
    /// whichever reached the sleeper first, which this library does not
    /// record, so both at once is refused.
    ///
    /// A pipe still empty with a write end open, and a signal with a handler
    /// pending, ends the call: `Restarts` if every handler that runs was
    /// installed with `SA_RESTART`, and `Failed EINTR` if none was. With no such
    /// signal, the task sleeps again, behind every other park; except that on
    /// Darwin, whose every sleeper wakes, one that finds nothing through a
    /// description that has since become non-blocking answers `EAGAIN`. An
    /// answer, `Restarts` included, clears the park.
    ///
    /// `task` must be asleep in a `read`.
    let finishRead<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadOutcome * UnixSystem<'Task, 'Handler>, ReadRefusal>
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.PipeRead parked) -> parked
            | Some other ->
                failwith
                    $"UnixReadWrite.finishRead: task %O{task} is parked in %A{other}, not in a read, so there is no read to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixReadWrite.finishRead: task %O{task} is not parked, so there is no read to finish. Only a task `read` answered `WouldBlock` finishes here (this is a bug in the client)."

        let pipeId = parkedPipe "finishRead" task parked.Reader PipeEnd.Read system
        let pipe = UnixMachineState.pipe pipeId system.Machine

        // Measured on Darwin (pipe-blocking.c section L): the read end's atime
        // moves when the sleeping call ends, however it ends (bytes, end of
        // file, EINTR, or a restart), and not while it sleeps.
        let finished =
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }
            |> touchedByRead pipeId

        let held = PipeBuffer.held pipe.Buffer

        if
            held > 0
            || not (UnixProcessState.pipeEndOpen pipeId pipe PipeEnd.Write system.Process)
        then
            // Measured on Linux 6.18.5 (pipe-blocking.c section H1): a reader
            // with a byte to read and a signal pending answered the byte,
            // whichever came first. Darwin answered whichever reached the
            // sleeper first (section J), which `beforeCompleting` refuses.
            match SyscallInterruption.beforeCompleting task system with
            | Error refusal -> Error (ReadRefusal.Interruption refusal)
            | Ok () ->

            if held = 0 then
                Ok (ReadOutcome.Answered (ReadAnswer.Completed ImmutableArray.Empty), finished)
            else

            match parked.Buffer with
            | UserBuffer.Unmapped _ ->
                // Measured on both (pipe-blocking.c section G1): the woken read
                // answers EFAULT, and the bytes stay in the pipe.
                Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EFAULT), finished)
            | UserBuffer.Opaque -> Error (ReadRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (ReadRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
            | UserBuffer.Mapped ->

            let bytes, finished = takeFromPipe pipeId parked.Count finished
            Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), finished)
        else

        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (ReadRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EINTR), finished)
        | Ok (Some SyscallInterruption.Restart) -> Ok (ReadOutcome.Restarts, finished)
        | Ok None when givesUpWhenBeaten parked.Reader system ->
            Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN), finished)
        | Ok None ->
            // Measured on both (pipe-blocking.c section A3): a reader that
            // finds the pipe empty again sleeps at the back of the queue.
            let parkedAgain = ParkedSyscall.PipeRead parked
            Ok (ReadOutcome.WouldBlock (WakeCondition.ofPark parkedAgain), UnixWait.park task parkedAgain system)

    /// `pread(2)`: move up to `count` bytes from `offset` in the file `fd` names
    /// into the caller's buffer, without consulting or moving the description's
    /// own file offset.
    ///
    /// `count` is the `size_t` the caller asked for, treated as `read`'s is.
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
        (count : uint64)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ReadAnswer, BufferRefusal>
        =
        let platform = system.Machine.UnixPlatform

        if countRefused platform count then
            Ok (ReadAnswer.Failed UnixError.EINVAL)
        else

        let flavour = SimulatedUnixPlatform.flavour platform

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
            // below need it and neither may guess: for a pipe it breaks the
            // ESPIPE/EBADF tie, and for a regular file it is the whole answer.
            let readable = FileAccessMode.permitsRead description.AccessMode

            match description.Target with
            | OpenFileTarget.Pipe _ ->
                // `pread` needs a seekable object, and a pipe is not one. A
                // write end therefore fails two different tests at once:
                // neither seekable nor open for reading. Measured, the flavours
                // break that tie differently:
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
                // Unlike `read`, this does not depend on the socket's phase:
                // every socket is unseekable whatever it is connected to, so
                // `pread` never reaches the socket's own read operation.
                Error UnixError.ESPIPE
            | OpenFileTarget.File (inode, _)
            | OpenFileTarget.Directory (inode, _) ->
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
            UserBufferCheck.faultsBeforeOperationFor (UnixMachineState.userBufferCheck system.Machine) buffer count
        with
        | Error refusal -> Error refusal
        | Ok true -> Ok (ReadAnswer.Failed UnixError.EFAULT)
        | Ok false ->

        match readAnsweredByPosition (positionCheck platform offset count) with
        | Some answer -> Ok answer
        | None ->

        readFileAt "pread" fd inode offset buffer (oneCallsWorth platform count) system

    /// What a `write` will operate on, once the descriptor's access mode has
    /// been checked: a file at its description's own offset, a socket, or a
    /// pipe's write end.
    let private writeTarget<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteTarget, UnixError>
        =
        match FileDescriptorRegistry.tryFindWithId fd system.Process.FileDescriptors with
        | None -> Error UnixError.EBADF
        | Some (descriptionId, description) ->

        if not (FileAccessMode.permitsWrite description.AccessMode) then
            // `write(2)` on a descriptor not open for writing is EBADF on both
            // platforms, and this precedes both the buffer screen and the
            // zero-size no-op: measured, `write(rdonlyFd, buf, 0)` is EBADF
            // rather than 0. It covers a pipe's read end — standard input among
            // them, which a launch onto pipes opens `O_RDONLY` — and a regular
            // file opened `O_RDONLY` alike, including a directory, which can
            // only ever be opened for reading.
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
        | OpenFileTarget.Pipe (pipeId, PipeEnd.Write) ->
            Ok (WriteTarget.Pipe (pipeId, descriptionId, description.NonBlocking))
        | OpenFileTarget.Pipe (pipeId, PipeEnd.Read) ->
            failwith
                $"UnixReadWrite.write: fd %d{fd} names the read end of pipe %O{pipeId}, whose access mode permits writing. `pipe(2)` opens the read end O_RDONLY and nothing reopens it (this is a bug in this library)."
        | OpenFileTarget.Directory (inode, _) ->
            failwith
                $"UnixReadWrite.write: fd %d{fd} names directory %O{inode} with an access mode that permits writing. A directory can only be opened for reading (open answers EISDIR otherwise), so FileDescriptorRegistry.checkInvariants reports this as WritableDirectory (this is a bug in this library)."

    /// `task` asleep in a write of `count` bytes through `writer`, the first
    /// `written` of them in already.
    let private parkWrite<'Answer, 'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (writer : OpenFileDescriptionId)
        (buffer : UserBuffer)
        (count : int)
        (written : int)
        (system : UnixSystem<'Task, 'Handler>)
        : WriteOutcome<'Answer, 'Task, 'Handler>
        =
        let reads =
            match
                FileDescriptorRegistry.descriptions system.Process.FileDescriptors
                |> Map.tryFind writer
            with
            | Some {
                       Target = OpenFileTarget.Pipe (pipeId, PipeEnd.Write)
                   } -> (UnixMachineState.pipe pipeId system.Machine).Reads
            | other ->
                failwith
                    $"UnixReadWrite: a write parking on open file description %O{writer} found %A{other} rather than a pipe's write end (this is a bug in this library)."

        let parked =
            ParkedSyscall.PipeWrite
                {
                    Writer = writer
                    Buffer = buffer
                    Count = count
                    Written = written
                    ReadsSeen = reads
                }

        WriteOutcome.WouldBlock (WakeCondition.ofPark parked, UnixWait.park task parked system)

    /// Every answer `write(2)` by `task` gives *without* reading the caller's
    /// buffer, and otherwise how many bytes to extract. See `WriteAdmission`
    /// for why this is a separate call rather than a `write` that takes the
    /// bytes.
    ///
    /// The system that comes back is the one a real kernel is in by the time it
    /// either answers or copies. It is the one that arrived, except that on
    /// Darwin a write reaching a pipe moves the pipe's timestamps whatever it
    /// answers but `EPIPE`, and that a write into a pipe with no reader raises
    /// `SIGPIPE`; so an answer here must be reported with it, and a transfer
    /// passed to `write` against it.
    ///
    /// A write into a pipe with no reader answers `EPIPE` without the buffer,
    /// ahead of `EAGAIN`, `EFAULT` and a short count, and raises `SIGPIPE`
    /// before it returns. That is a zero-length write too, except on Linux,
    /// where one answers 0 and raises nothing. The signal is `task`'s own on
    /// Linux and the process's on Darwin; what it does is then
    /// `SignalState.generate`'s to say, and if its disposition is the
    /// default, it ends the process.
    ///
    /// A blocking write into a pipe with no room for any of it sleeps
    /// (`WriteOutcome.WouldBlock`) without reading the buffer; one with room
    /// for part of it is given the whole count to transfer, and `write` puts in
    /// what fits and sleeps for the rest. `admitFinishWrite` and `finishWrite`
    /// finish a sleeping write.
    ///
    /// A socket with no peer answers without the buffer, as
    /// `UnconnectedSocketRules.write` says, at every length including zero; on
    /// Linux a stream socket's `EPIPE` raises `SIGPIPE` as a pipe's does. A
    /// socket with a peer, or with a refused connection's error, is refused.
    ///
    /// Fails loudly if `task` is not one of the process's tasks, or is already
    /// asleep in a syscall.
    let admitWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (buffer : UserBuffer)
        (count : uint64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<WriteAdmission, 'Task, 'Handler>, WriteRefusal>
        =
        checkIssuer "admitWrite" task system

        let platform = system.Machine.UnixPlatform

        let unchanged (admission : WriteAdmission) =
            WriteOutcome.Returns (admission, system)

        if countRefused platform count then
            Ok (unchanged (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EINVAL)))
        else

        match writeTarget fd system with
        | Error error -> Ok (unchanged (WriteAdmission.Answered (WriteAnswer.Failed error)))
        | Ok target ->

        // `vfs_write` screens the buffer between the access mode above and the
        // file operation, so on Linux this beats the zero-size no-op below:
        // measured, `write(1, (void*)-1, 0)` is EFAULT there and 0 on macOS.
        match
            UserBufferCheck.faultsBeforeOperationFor (UnixMachineState.userBufferCheck system.Machine) buffer count
        with
        | Error refusal -> Error (WriteRefusal.Buffer refusal)
        | Ok true -> Ok (unchanged (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT)))
        | Ok false ->

        // **After the screen, and before the zero-length no-op.** Both halves
        // are measured. Linux screens the address before the object's own write
        // operation, so `write(socket, (void*)-1, n)` there is EFAULT for every
        // `n` including 0: the screen answers and the socket is never consulted.
        // Darwin screens nothing, so the same call reaches the socket and earns
        // its own answer (ENOTCONN for a stream socket with no peer,
        // EDESTADDRREQ for a datagram one).
        //
        // And the no-op does *not* precede it: measured on both for an
        // unconnected socket, `write(socket, buf, 0)` is the socket's own
        // error rather than 0. (A connected stream socket's zero-length write
        // is unmeasured here; it is refused with the rest of that phase.)
        match target with
        | WriteTarget.Socket socketId -> socketWrite (WriteAdmission.Answered) task socketId count system
        | WriteTarget.Pipe (pipeId, descriptionId, nonBlocking) ->
            // A pipe has no position, and its own write decides the zero-length
            // case, which it answers differently from a file.
            let count = oneCallsWorth platform count

            match pipeWriteStep pipeId nonBlocking count buffer system with
            | PipeWriteStep.Refused refusal -> Error refusal
            | PipeWriteStep.Broken ->
                broken
                    (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EPIPE))
                    task
                    (BrokenWriteTarget.Pipe pipeId)
                    system
            | PipeWriteStep.Answered answer ->
                Ok (WriteOutcome.Returns (WriteAdmission.Answered answer, touchedByWrite pipeId system))
            | PipeWriteStep.TakesNothing answer ->
                Ok (
                    WriteOutcome.Returns (
                        WriteAdmission.Answered answer,
                        touchedByWrite pipeId (leftUntaken pipeId count system)
                    )
                )
            // A write that sleeps moves no timestamp until it ends, measured on
            // Darwin (pipe-blocking.c section L).
            | PipeWriteStep.Sleeps ->
                leftUntaken pipeId count system
                |> parkWrite task descriptionId buffer count 0
                |> Ok
            // Only the bytes the pipe will take: a short write never reads the
            // rest of the caller's buffer, and `write` offered this prefix takes
            // all of it and leaves the pipe as the whole would have.
            | PipeWriteStep.Takes taken ->
                Ok (WriteOutcome.Returns (WriteAdmission.Transfer taken, touchedByWrite pipeId system))
            | PipeWriteStep.TakesThenSleeps taken ->
                Ok (WriteOutcome.Returns (WriteAdmission.TransferThenSleep (taken, count), system))
        | WriteTarget.File (_, offset) ->

        // Ahead of the zero-length no-op below: Darwin's EFBIG at INT64_MAX
        // answers a count of zero too, measured.
        match writeFailedByPosition (positionCheck platform offset count) with
        | Some error -> Ok (unchanged (WriteAdmission.Answered (WriteAnswer.Failed error)))
        | None ->

        let count = oneCallsWorth platform count

        if count = 0 then
            // A no-op on both platforms, and specifically one that moves no
            // timestamp: measured, a zero-length write leaves `mtime` and
            // `ctime` where they were and does not extend the file, even at an
            // offset past its end. The buffer is not resolved: any address that
            // got past the screen is permitted, because it is not dereferenced.
            Ok (unchanged (WriteAdmission.Answered (WriteAnswer.Completed 0L)))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // Real `write(2)` answers EFAULT for any non-dereferenceable
            // address, null included, having performed no I/O.
            Ok (unchanged (WriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT)))
        | UserBuffer.Opaque -> Error (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (WriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (unchanged (WriteAdmission.Transfer count))

    /// `write(2)`, given the bytes the caller extracted after `admitWrite` said
    /// to.
    ///
    /// Takes no buffer: every question about the caller's buffer is settled by
    /// `admitWrite`, and a signature that could not ask them again is the point.
    /// Still answers the descriptor questions itself, so a caller that skipped
    /// the admission gets a kernel's answer rather than an inconsistent one.
    ///
    /// `bytes` is at most one call's worth, as the admission's
    /// `WriteAdmission.Transfer` says; a longer array is refused as the
    /// caller's mistake.
    ///
    /// Short only for a non-blocking write into a pipe with room for part of
    /// it: this kernel's filesystem cannot run out of space. A blocking write
    /// into a pipe with room for part of it puts that part in and sleeps for
    /// the rest (`WriteOutcome.WouldBlock`), as one with room for none does.
    ///
    /// A write into a pipe the client drains is read by the client as it is
    /// written, and recorded in `UnixMachineState.Delivered`. A write into a
    /// pipe with no reader answers `EPIPE` and raises `SIGPIPE`, and a socket
    /// answers or is refused, as `admitWrite` describes.
    ///
    /// Fails loudly if `task` is not one of the process's tasks, or is already
    /// asleep in a syscall.
    let write<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixReadWrite.write: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        checkIssuer "write" task system
        assertOneCallsWorth "write" system.Machine.UnixPlatform bytes

        let returns (answer : WriteAnswer) (system : UnixSystem<'Task, 'Handler>) =
            Ok (WriteOutcome.Returns (answer, system))

        match writeTarget fd system with
        | Error error -> returns (WriteAnswer.Failed error) system
        | Ok (WriteTarget.Socket socketId) ->
            // There is no buffer here to screen, so the socket's own answer is
            // all there is, and it never takes the bytes. A caller that used
            // `admitWrite` never reaches this: that call answered or refused
            // first.
            socketWrite id task socketId (uint64 bytes.Length) system
        | Ok (WriteTarget.Pipe (pipeId, descriptionId, nonBlocking)) ->
            match pipeWriteStep pipeId nonBlocking bytes.Length UserBuffer.Mapped system with
            | PipeWriteStep.Refused refusal -> Error refusal
            | PipeWriteStep.Broken ->
                broken (WriteAnswer.Failed UnixError.EPIPE) task (BrokenWriteTarget.Pipe pipeId) system
            | PipeWriteStep.Answered answer -> returns answer (touchedByWrite pipeId system)
            | PipeWriteStep.TakesNothing answer ->
                returns answer (touchedByWrite pipeId (leftUntaken pipeId bytes.Length system))
            | PipeWriteStep.Sleeps ->
                leftUntaken pipeId bytes.Length system
                |> parkWrite task descriptionId UserBuffer.Mapped bytes.Length 0
                |> Ok
            | PipeWriteStep.TakesThenSleeps taken ->
                let pipe = UnixMachineState.pipe pipeId system.Machine
                let written, buffer = PipeBuffer.write bytes pipe.Buffer

                if written <> taken then
                    failwith
                        $"UnixReadWrite.write: pipe %O{pipeId} was to take %d{taken} of %d{bytes.Length} bytes and took %d{written}; PipeBuffer.wouldTake and PipeBuffer.write disagree (this is a bug in this library)."

                withPipe
                    pipeId
                    { pipe with
                        Buffer = buffer
                    }
                    system
                |> parkWrite task descriptionId UserBuffer.Mapped bytes.Length written
                |> Ok
            | PipeWriteStep.Takes taken ->
                let pipe = UnixMachineState.pipe pipeId system.Machine

                match PipeState.drainedBy pipe with
                | Some endpoint ->
                    let delivered =
                        if taken = bytes.Length then
                            bytes
                        else
                            ImmutableArray.Create (bytes, 0, taken)

                    let buffer, filled = drainedThrough delivered pipe.Buffer

                    // Measured on Linux 6.18.5 (drained-pipe-epoll.c): an
                    // edge-triggered `EPOLLOUT` registration on the write end
                    // sees nothing from a write, and an edge from the reader's
                    // drain exactly when the write had filled the pipe (65535
                    // and 65536 bytes did; 61440 did not). The reader's wake
                    // carries `EPOLLOUT | EPOLLWRNORM` (`pipe_read`).
                    let registry =
                        if filled then
                            FileDescriptorRegistry.signalSocketEventPorts
                                (UnixProcessState.descriptionsNamingPipeEnd pipeId PipeEnd.Write system.Process)
                                (Some (EpollEvents.Out ||| EpollEvents.WrNorm))
                                system.Process.FileDescriptors
                        else
                            system.Process.FileDescriptors

                    returns
                        (WriteAnswer.Completed (int64 taken))
                        ({ system with
                            Process =
                                { system.Process with
                                    FileDescriptors = registry
                                }
                            Machine =
                                { system.Machine with
                                    Pipes =
                                        Map.add
                                            pipeId
                                            { pipe with
                                                Buffer = buffer
                                            }
                                            system.Machine.Pipes
                                    Delivered =
                                        DeliveryLog.append
                                            {
                                                Endpoint = endpoint
                                                Bytes = delivered
                                            }
                                            system.Machine.Delivered
                                }
                         }
                         |> touchedByWrite pipeId)
                | None ->

                let written, buffer = PipeBuffer.write bytes pipe.Buffer

                if written <> taken then
                    failwith
                        $"UnixReadWrite.write: pipe %O{pipeId} was to take %d{taken} of %d{bytes.Length} bytes and took %d{written}; PipeBuffer.wouldTake and PipeBuffer.write disagree (this is a bug in this library)."

                returns
                    (WriteAnswer.Completed (int64 written))
                    (touchedByWrite
                        pipeId
                        (withPipe
                            pipeId
                            { pipe with
                                Buffer = buffer
                            }
                            system))
        | Ok (WriteTarget.File (inode, offset)) ->

        // Ahead of the zero-length no-op: Darwin's EFBIG at INT64_MAX answers
        // a count of zero too, measured.
        match writeFailedByPosition (positionCheck system.Machine.UnixPlatform offset (uint64 bytes.Length)) with
        | Some error -> returns (WriteAnswer.Failed error) system
        | None ->

        if bytes.IsEmpty then
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
            returns (WriteAnswer.Completed 0L) system
        else

        let now = UnixMachineState.realtime system.Machine

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root; measured on both platforms, which
        // disagree only about `S_ISGID` on a file that is not group-executable.
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform

        match
            VirtualFileSystem.writeFile inode offset bytes rule system.Process.Credentials now system.Machine.FileSystem
        with
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            Error (WriteRefusal.ExceedsRepresentableLength (inode, offset, count))
        | Error (FileWriteRefusal.UnmeasuredSetIdChange refusal) ->
            Error (WriteRefusal.UnmeasuredSetIdChange (inode, refusal))
        | Ok filesystem ->

        // At the description's own offset, and advancing it by what moved — the
        // entire difference from `pwrite`, which takes the offset as an argument
        // and leaves the description alone. Both measured.
        //
        // The commit comes first, so the advance cannot overflow: a write that
        // would carry the offset past what the model can represent has already
        // been refused there.
        returns
            (WriteAnswer.Completed (int64 bytes.Length))
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

    /// The write `task` is asleep in, and the pipe it writes into.
    let private parkedWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (syscall : string)
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : ParkedPipeWrite * PipeId
        =
        let parked =
            match UnixTaskTable.parkedFor task system.Tasks with
            | Some (ParkedSyscall.PipeWrite parked) -> parked
            | Some other ->
                failwith
                    $"UnixReadWrite.%s{syscall}: task %O{task} is parked in %A{other}, not in a write, so there is no write to finish (this is a bug in the client)."
            | None ->
                failwith
                    $"UnixReadWrite.%s{syscall}: task %O{task} is not parked, so there is no write to finish. Only a task a write answered `WouldBlock` finishes here (this is a bug in the client)."

        parked, parkedPipe syscall task parked.Writer PipeEnd.Write system

    /// After a sleeping write has put bytes in, its count `parked.Count` not
    /// all in yet: whether it sleeps on, or ends with the count it has put in,
    /// as it does when a signal is pending or `givesUp`, with `system` the one
    /// it is in by now.
    ///
    /// Measured on both (pipe-blocking.c sections D and H): a write asleep
    /// with bytes in returns their count when signalled, under SA_RESTART or
    /// not, so the handlers' flags are not asked.
    let private afterPartWritten<'Task, 'Handler, 'Answer when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (pipeId : PipeId)
        (parked : ParkedPipeWrite)
        (givesUp : bool)
        (answered : WriteAnswer -> 'Answer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<'Answer, 'Task, 'Handler>, WriteRefusal>
        =
        match SyscallInterruption.interrupts task system with
        | Error refusal -> Error (WriteRefusal.Interruption refusal)
        | Ok interrupted when interrupted || givesUp ->
            let finished =
                { system with
                    Tasks = UnixTaskTable.unpark task system.Tasks
                }
                |> touchedByWrite pipeId

            Ok (WriteOutcome.Returns (answered (WriteAnswer.Completed (int64 parked.Written)), finished))
        | Ok _ -> Ok (parkWrite task parked.Writer parked.Buffer parked.Count parked.Written system)

    /// Every answer the `write` `task` is asleep in gives *without* reading more
    /// of the caller's buffer, and otherwise which of its bytes to extract next.
    /// See `WriteAdmission` for why that is a separate call.
    ///
    /// The pipe is looked at again, as a woken real write does:
    ///
    /// - **No reader is left.** On Linux the call answers the count it had put
    ///   in, or `EPIPE` if none; on Darwin, `EPIPE` whatever it had put in.
    ///   Either way it raises `SIGPIPE`, as `admitWrite` describes.
    /// - **There is room** for the rest, or (for a write of more than
    ///   `PIPE_BUF` bytes) for some of it: `WriteResumption.Transfer` names the
    ///   bytes to pass to `finishWrite`. A buffer naming no storage answers
    ///   `EFAULT`, the write having put nothing in.
    /// - **Neither**, and a signal with a handler is pending for the task: a
    ///   write that had put bytes in returns their count, whatever the
    ///   handlers' flags; one that had not ends as `read` does, `Restarts` or
    ///   `Failed EINTR`.
    /// - **Neither, and no signal**: the task sleeps again, behind every other
    ///   park; except that on Darwin, whose every sleeper wakes, one whose
    ///   description has since become non-blocking gives up, answering the
    ///   count it had put in, or `EAGAIN` if none.
    ///
    /// Under Linux the first two beat a pending signal, whose handlers run as
    /// the call returns; under Darwin a kernel answers whichever reached the
    /// sleeper first, which this library does not record, so either beside a
    /// signal is refused. An answer, `Restarts` included, clears the park, and
    /// on Darwin moves the pipe's timestamps, which do not move while the call
    /// sleeps.
    ///
    /// `task` must be asleep in a `write`.
    let admitFinishWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<WriteResumption, 'Task, 'Handler>, WriteRefusal>
        =
        let parked, pipeId = parkedWrite "admitFinishWrite" task system
        let pipe = UnixMachineState.pipe pipeId system.Machine

        // Measured on Darwin (pipe-blocking.c section L): a sleeping write
        // moves the write end's mtime and ctime when it ends, however it ends
        // (bytes in, EINTR, a count, EPIPE once the reader has gone, a
        // restart), and not while it sleeps. A write that answers EPIPE
        // without sleeping moves none (pipe-epipe-sweep.c).
        let finished =
            { system with
                Tasks = UnixTaskTable.unpark task system.Tasks
            }
            |> touchedByWrite pipeId

        let answered (answer : WriteAnswer) (system : UnixSystem<'Task, 'Handler>) =
            Ok (WriteOutcome.Returns (WriteResumption.Answered answer, system))

        if not (UnixProcessState.pipeEndOpen pipeId pipe PipeEnd.Read system.Process) then
            // Measured (pipe-blocking.c section E): the reader leaving answered
            // EPIPE if the write had put nothing in, on both, and on Linux the
            // count it had put in otherwise, where Darwin answered EPIPE; SIGPIPE
            // ran on the writing thread on Linux and the main thread on Darwin,
            // in every case. Linux's `pipe_write` checks for a reader before it
            // checks for a signal; Darwin's sleeper takes whichever reached it
            // first (section J).
            match SyscallInterruption.beforeCompleting task system with
            | Error refusal -> Error (WriteRefusal.Interruption refusal)
            | Ok () ->

            let answer =
                match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
                | SimulatedUnixFlavour.Linux when parked.Written > 0 ->
                    WriteResumption.Answered (WriteAnswer.Completed (int64 parked.Written))
                | SimulatedUnixFlavour.Linux
                | SimulatedUnixFlavour.Darwin -> WriteResumption.Answered (WriteAnswer.Failed UnixError.EPIPE)

            broken answer task (BrokenWriteTarget.Pipe pipeId) finished
        else

        let taking = PipeBuffer.resumeTakes parked.Count parked.Written pipe.Buffer

        if taking > 0 then
            match SyscallInterruption.beforeCompleting task system with
            | Error refusal -> Error (WriteRefusal.Interruption refusal)
            | Ok () ->

            match parked.Buffer with
            | UserBuffer.Unmapped _ ->
                // Measured on both (pipe-blocking.c section G2): the woken write
                // answers EFAULT and puts nothing in. Only a write that had put
                // nothing in can be holding such a buffer: putting bytes in
                // reads them.
                answered (WriteAnswer.Failed UnixError.EFAULT) finished
            | UserBuffer.Opaque -> Error (WriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
            | UserBuffer.Addressless -> Error (WriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
            | UserBuffer.Mapped -> Ok (WriteOutcome.Returns (WriteResumption.Transfer (parked.Written, taking), system))
        elif parked.Written > 0 then
            afterPartWritten task pipeId parked (givesUpWhenBeaten parked.Writer system) WriteResumption.Answered system
        else

        // Measured on both (pipe-blocking.c section D): a write asleep with
        // nothing in returned EINTR without SA_RESTART and slept on with it.
        match SyscallInterruption.ofPark task system with
        | Error refusal -> Error (WriteRefusal.Interruption refusal)
        | Ok (Some SyscallInterruption.Eintr) -> answered (WriteAnswer.Failed UnixError.EINTR) finished
        | Ok (Some SyscallInterruption.Restart) -> Ok (WriteOutcome.Restarts finished)
        | Ok None when givesUpWhenBeaten parked.Writer system -> answered (WriteAnswer.Failed UnixError.EAGAIN) finished
        | Ok None ->
            // Measured on both (pipe-blocking.c section B): a writer that finds
            // no room again sleeps at the back of the queue.
            Ok (parkWrite task parked.Writer parked.Buffer parked.Count parked.Written system)

    /// The `write` `task` is asleep in, given the bytes the caller extracted
    /// after `admitFinishWrite` said to: they go into the pipe, and the call
    /// returns its whole count if that was the last of it.
    ///
    /// Otherwise a signal with a handler pending for the task, or `O_NONBLOCK`
    /// set on the description while the call slept, ends it with the count it
    /// has put in by now, and with neither, it sleeps again for the rest.
    ///
    /// `bytes` must be exactly the ones `WriteResumption.Transfer` named, and
    /// the system the one it came with: anything else is the caller's mistake,
    /// and fails loudly.
    let finishWrite<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixReadWrite.finishWrite: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        let parked, pipeId = parkedWrite "finishWrite" task system
        let pipe = UnixMachineState.pipe pipeId system.Machine
        let taking = PipeBuffer.resumeTakes parked.Count parked.Written pipe.Buffer

        if taking = 0 || bytes.Length <> taking then
            failwith
                $"UnixReadWrite.finishWrite: task %O{task}'s write into pipe %O{pipeId} takes %d{taking} bytes now and was given %d{bytes.Length}. Pass the bytes `admitFinishWrite` named, against the system it answered with (this is a bug in the caller)."

        let system =
            withPipe
                pipeId
                { pipe with
                    Buffer = PipeBuffer.resumeWith parked.Count parked.Written bytes pipe.Buffer
                }
                system

        let parked =
            { parked with
                Written = parked.Written + taking
            }

        if parked.Written = parked.Count then
            let finished =
                { system with
                    Tasks = UnixTaskTable.unpark task system.Tasks
                }
                |> touchedByWrite pipeId

            Ok (WriteOutcome.Returns (WriteAnswer.Completed (int64 parked.Count), finished))
        else
            // Measured on Linux 6.18.5 (pipe-blocking.c sections H2, H3): a
            // writer given room and a signal fills the room, then returns its
            // count rather than sleeping again. Under Darwin a signal and room
            // at once were refused before the room was filled. Measured on both
            // (section N1, N2): one whose description became non-blocking
            // while it slept fills the room and returns its count too.
            let nonBlocking =
                (FileDescriptorRegistry.descriptions system.Process.FileDescriptors).[parked.Writer].NonBlocking

            afterPartWritten task pipeId parked nonBlocking id system

    /// A blocking `write` of `total` bytes by `task`, given the first of them
    /// that `WriteAdmission.TransferThenSleep` named: they go into the pipe, and
    /// the call sleeps for the rest (`WriteOutcome.WouldBlock`), to be finished
    /// with `admitFinishWrite` and `finishWrite`.
    ///
    /// `bytes` must be exactly the ones the admission named, and the system the
    /// one it came with: anything else is the caller's mistake, and fails
    /// loudly.
    let writeThenSleep<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (fd : int)
        (total : int)
        (bytes : ImmutableArray<byte>)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<WriteOutcome<WriteAnswer, 'Task, 'Handler>, WriteRefusal>
        =
        if bytes.IsDefault then
            failwith
                "UnixReadWrite.writeThenSleep: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        checkIssuer "writeThenSleep" task system

        let mismatch (what : string) : 'a =
            failwith
                $"UnixReadWrite.writeThenSleep: fd %d{fd}, given %d{bytes.Length} of %d{total} bytes, %s{what}. Pass the bytes `admitWrite`'s `TransferThenSleep` named, against the system it answered with (this is a bug in the caller)."

        match writeTarget fd system with
        | Ok (WriteTarget.Pipe (pipeId, descriptionId, nonBlocking)) ->
            match pipeWriteStep pipeId nonBlocking total UserBuffer.Mapped system with
            | PipeWriteStep.TakesThenSleeps taken when taken = bytes.Length ->
                let pipe = UnixMachineState.pipe pipeId system.Machine

                withPipe
                    pipeId
                    { pipe with
                        Buffer = PipeBuffer.writeWith total bytes pipe.Buffer
                    }
                    system
                |> parkWrite task descriptionId UserBuffer.Mapped total taken
                |> Ok
            | step -> mismatch $"is not the blocking write that puts in part of it and sleeps (%A{step})"
        | other -> mismatch $"names %A{other} rather than the write end of a pipe"

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
        // need it and neither may guess: for a pipe it breaks the ESPIPE/EBADF
        // tie, and for a regular file it is the whole answer.
        let writable = FileAccessMode.permitsWrite description.AccessMode

        match description.Target with
        | OpenFileTarget.Pipe _ ->
            // The mirror of `pread`'s tie, with the roles swapped: `pwrite` needs
            // a seekable object, a pipe is not one, and a read end therefore fails
            // two tests at once — neither seekable nor open for writing.
            // Measured:
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
            // Unlike `write`, this does not depend on the socket's phase: every
            // socket is unseekable whatever it is connected to, so `pwrite`
            // never reaches the socket's own write operation. That is why
            // `PWriteRefusal` has no socket case.
            Error UnixError.ESPIPE
        | OpenFileTarget.File (inode, _)
        | OpenFileTarget.Directory (inode, _) ->

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
        (count : uint64)
        (offset : int64)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<PWriteAdmission, PWriteRefusal>
        =
        let platform = system.Machine.UnixPlatform

        if countRefused platform count then
            Ok (PWriteAdmission.Answered (WriteAnswer.Failed UnixError.EINVAL))
        else

        match pwriteTarget fd offset system with
        | Error error -> Ok (PWriteAdmission.Answered (WriteAnswer.Failed error))
        | Ok _ ->

        // `vfs_write` screens the buffer between the access mode above and the
        // file operation, so under a screening flavour this beats the no-op
        // below: measured, `pwrite(f, (void*)-1, 0, 0)` is EFAULT on Linux and 0
        // on Darwin.
        match
            UserBufferCheck.faultsBeforeOperationFor (UnixMachineState.userBufferCheck system.Machine) buffer count
        with
        | Error refusal -> Error (PWriteRefusal.Buffer refusal)
        | Ok true -> Ok (PWriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | Ok false ->

        // Ahead of the zero-length no-op below: Darwin's EFBIG at INT64_MAX
        // answers a count of zero too, measured.
        match writeFailedByPosition (positionCheck platform offset count) with
        | Some error -> Ok (PWriteAdmission.Answered (WriteAnswer.Failed error))
        | None ->

        let count = oneCallsWorth platform count

        if count = 0 then
            // A no-op on both flavours, and specifically one that moves no
            // timestamp: measured, a zero-length `pwrite` leaves `mtime` and
            // `ctime` where they were and does not extend the file, even at an
            // offset far past its end. The buffer is not resolved, because
            // nothing is read through it — a null pointer is an ordinary user
            // address, so it reaches here rather than being screened above, and
            // `pwrite(f, NULL, 0, 0)` is 0 on both.
            Ok (PWriteAdmission.Answered (WriteAnswer.Completed 0L))
        else

        match buffer with
        | UserBuffer.Unmapped _ ->
            // Real `pwrite(2)` answers EFAULT for any non-dereferenceable
            // address, null included, having performed no I/O: measured,
            // `pwrite(f, NULL, 4, 0)` is EFAULT on both, where the same pointer
            // at length 0 is a no-op.
            Ok (PWriteAdmission.Answered (WriteAnswer.Failed UnixError.EFAULT))
        | UserBuffer.Opaque -> Error (PWriteRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (PWriteRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (PWriteAdmission.Transfer count)

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
    /// `bytes` is at most one call's worth, as the admission's
    /// `PWriteAdmission.Transfer` says; a longer array is refused as the
    /// caller's mistake.
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

        assertOneCallsWorth "pwrite" system.Machine.UnixPlatform bytes

        match pwriteTarget fd offset system with
        | Error error -> Ok (WriteAnswer.Failed error, system)
        | Ok inode ->

        // Ahead of the zero-length no-op: Darwin's EFBIG at INT64_MAX answers
        // a count of zero too, measured.
        match writeFailedByPosition (positionCheck system.Machine.UnixPlatform offset (uint64 bytes.Length)) with
        | Some error -> Ok (WriteAnswer.Failed error, system)
        | None ->

        if bytes.IsEmpty then
            // A no-op that changes nothing, and *after* the descriptor checks:
            // measured, `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0.
            // `admitPWrite` answers this too, so the arm is unreachable for a
            // caller that used the pair — but a caller that did not must get the
            // same answer, and `VirtualFileSystem.writeFile` below asserts a
            // non-empty write precisely because it would otherwise restamp the
            // inode.
            Ok (WriteAnswer.Completed 0L, system)
        else

        let now = UnixMachineState.realtime system.Machine

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root, exactly as `write`'s does: the bits
        // follow the content changing, not which syscall changed it.
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform

        match
            VirtualFileSystem.writeFile inode offset bytes rule system.Process.Credentials now system.Machine.FileSystem
        with
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            Error (PWriteRefusal.ExceedsRepresentableLength (inode, offset, count))
        | Error (FileWriteRefusal.UnmeasuredSetIdChange refusal) ->
            Error (PWriteRefusal.UnmeasuredSetIdChange (inode, refusal))
        | Ok filesystem ->

        // The description is left exactly where it was — the whole of what
        // `pwrite` does differently from `write`, and measured on both flavours.
        Ok (
            WriteAnswer.Completed (int64 bytes.Length),
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
            }
        )

    /// `copy_file_range(inFd, NULL, outFd, NULL, length, flags)`: copy up to
    /// `length` bytes from `inFd`'s offset to `outFd`'s, inside the kernel,
    /// and advance both offsets by what moved.
    ///
    /// Only the form that copies at the descriptions' own offsets is
    /// expressible. `length` is the `size_t` the caller asked for; it is
    /// shortened to what remains of the source, and then to what one call
    /// moves (see `TransferCountLimit`). The answer is the count copied, 0 at
    /// the source's end.
    ///
    /// EBADF for a descriptor the process does not hold, then EINVAL for any
    /// `flags`, EISDIR if either names a directory, EINVAL if either is not a
    /// regular file, EBADF for a source not open for reading or a destination
    /// not open for writing, EFBIG for a destination at offset `INT64_MAX`,
    /// and EINVAL for a copy within one file whose two ranges overlap. A copy writes as `write(2)` does: the destination's
    /// modification and status-change times move and its set-ID bits are
    /// stripped as a write strips them. A copy of nothing changes nothing.
    let copyFileRange<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (inFd : int)
        (outFd : int)
        (length : uint64)
        (flags : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, CopyFileRangeRefusal>
        =
        // Measured on Linux 6.18.5 (`copy-file-syscalls.c`, tmpfs and ext4
        // alike) over every pair of a file opened read-only, write-only and
        // read-write, a directory, each end of a pipe, a socket, an epoll
        // instance, a closed descriptor and 9999, at lengths 5 and 0 and with
        // flags 1: the order of the refusals is the one stated above, and a
        // length of 0 is answered only after all of them. Every flag bit alone
        // is EINVAL. A 0-to-0 copy on one description, 0-to-3 and 2-to-0 of 5
        // on two descriptions of one file, and a 0-to-0 copy between two hard
        // links are EINVAL; 0-to-5 of 5 is not, nor is 8-to-0 of 5 from a
        // 10-byte file, which copies 2. The overlap is judged on the count
        // shortened to the source's end: 0-to-12 of 20 from a 10-byte file
        // copies 10. Lengths SSIZE_MAX and SIZE_MAX copy what remains; a copy
        // from past the source's end is 0 and moves nothing, timestamps
        // included; a copy into an offset past the destination's end leaves a
        // hole of zeroes. A copy moves the destination's mtime and ctime and
        // nothing else of it, and strips the set-ID bits exactly as a write by
        // the same caller does (uid 1000: 06755 to 0755, 06745 to 02745,
        // 02644 kept; root keeps every bit). A destination at offset
        // INT64_MAX is EFBIG on tmpfs for lengths 0, 1 and 5 from sources of 0,
        // 1 and 5 bytes, the source's offset unmoved
        // (`copy-file-range-max-offset.c`). One call moves a whole 3 MiB file;
        // `copy-file-range-cap.c` shows a call moving at most 0x7ffff000 bytes.
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (CopyFileRangeRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let registry = system.Process.FileDescriptors

        match FileDescriptorRegistry.tryFind inFd registry, FileDescriptorRegistry.tryFind outFd registry with
        | None, _
        | _, None -> Ok (SyscallAnswer.Failed UnixError.EBADF, system)
        | Some source, Some destination ->

        let failed (error : UnixError) = Ok (SyscallAnswer.Failed error, system)

        let isDirectory (description : OpenFileDescription) : bool =
            match description.Target with
            | OpenFileTarget.Directory _ -> true
            | OpenFileTarget.File _
            | OpenFileTarget.Pipe _
            | OpenFileTarget.Socket _
            | OpenFileTarget.SocketEventPort _ -> false

        if flags <> 0 then
            failed UnixError.EINVAL
        elif isDirectory source || isDirectory destination then
            failed UnixError.EISDIR
        else

        match source.Target, destination.Target with
        | OpenFileTarget.File (sourceInode, sourceOffset), OpenFileTarget.File (destinationInode, destinationOffset) ->
            if
                not (FileAccessMode.permitsRead source.AccessMode)
                || not (FileAccessMode.permitsWrite destination.AccessMode)
            then
                failed UnixError.EBADF
            else

            match EmulatedMount.fileSystemType system.Machine.Mount with
            | EmulatedFileSystemType.Apfs
            | EmulatedFileSystemType.Nfs as fileSystem -> Error (CopyFileRangeRefusal.UnmeasuredFileSystem fileSystem)
            | EmulatedFileSystemType.Tmpfs ->

            // A destination already at tmpfs's largest file size, INT64_MAX,
            // takes nothing: EFBIG, ahead of the length and of the source's
            // end, so even a copy of nothing from an empty file is refused.
            if destinationOffset = System.Int64.MaxValue then
                failed UnixError.EFBIG
            else

            let contents =
                match VirtualFileSystem.tryGetContent sourceInode system.Machine.FileSystem with
                | Some (InodeContent.RegularFile (contents, _)) -> contents
                | other ->
                    failwith
                        $"UnixReadWrite.copyFileRange: fd %d{inFd} is a file description naming inode %O{sourceInode}, which holds %A{other} rather than a regular file (this is a bug in this library)."

            let remaining =
                if sourceOffset >= int64 contents.Length then
                    0UL
                else
                    uint64 (int64 contents.Length - sourceOffset)

            let count = min length remaining

            // [sourceOffset, +count) against [destinationOffset, +count),
            // written so that nothing can overflow: the source's range ends
            // inside the file, while the destination's offset can be anything
            // up to INT64_MAX.
            let overlaps =
                sourceInode = destinationInode
                && count > 0UL
                && destinationOffset < sourceOffset + int64 count
                && destinationOffset > sourceOffset - int64 count

            if overlaps then
                failed UnixError.EINVAL
            elif count = 0UL then
                Ok (SyscallAnswer.Completed 0L, system)
            else

            let count = oneCallsWorth system.Machine.UnixPlatform count
            let bytes = ImmutableArray.Create (contents, int sourceOffset, count)
            let now = UnixMachineState.realtime system.Machine
            let rule = SimulatedUnixPlatform.setGroupIdOnWrite system.Machine.UnixPlatform

            match
                VirtualFileSystem.writeFile
                    destinationInode
                    destinationOffset
                    bytes
                    rule
                    system.Process.Credentials
                    now
                    system.Machine.FileSystem
            with
            | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
                Error (CopyFileRangeRefusal.ExceedsRepresentableLength (destinationInode, offset, count))
            | Error (FileWriteRefusal.UnmeasuredSetIdChange refusal) ->
                Error (CopyFileRangeRefusal.UnmeasuredSetIdChange (destinationInode, refusal))
            | Ok filesystem ->

            let registry =
                registry
                |> FileDescriptorRegistry.setOffset inFd (sourceOffset + int64 count)
                |> FileDescriptorRegistry.setOffset outFd (destinationOffset + int64 count)

            Ok (
                SyscallAnswer.Completed (int64 count),
                { system with
                    Machine =
                        { system.Machine with
                            FileSystem = filesystem
                        }
                    Process =
                        { system.Process with
                            FileDescriptors = registry
                        }
                }
            )
        | _ -> failed UnixError.EINVAL
