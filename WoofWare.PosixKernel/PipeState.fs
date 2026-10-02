namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// The inode numbers `fstat(2)` reports for a pipe's two ends.
///
/// Minted by this kernel, as every other inode number is: no real kernel's
/// numbers are reproducible, since Linux takes them from a machine-wide counter
/// and Darwin reports a hash of a kernel address. What each flavour does
/// reproducibly is the shape, and a process can see it by comparing the ends.
[<RequireQualifiedAccess>]
type PipeInodes =
    /// Linux: both ends report this one number. Measured on 6.18.5, the ends of
    /// one pipe share an `st_ino`, and a second pipe has another.
    | Shared of InodeNumber
    /// Darwin: each end reports a number of its own. Measured on 27.0.0, the two
    /// ends of one pipe report different `st_ino`.
    | PerEnd of readEnd : InodeNumber * writeEnd : InodeNumber

/// The timestamps `fstat(2)` reports for a pipe's ends, and what moves them.
///
/// On Linux none of them ever moves: measured on 6.18.5, reads, writes, short
/// writes, `EAGAIN` and `EFAULT` in either direction all leave every timestamp
/// of both ends at the pipe's creation. On Darwin they move per end, as
/// `UnixPipe` describes.
type PipeTimes =
    {
        /// When the pipe was created: every timestamp of both ends starts here.
        /// It is also the write end's `st_atime` for the pipe's whole life, since
        /// nothing reads through the write end.
        Created : UnixTimestamp
        /// The read end's `st_atime`.
        ReadEndAccess : UnixTimestamp
        /// `st_mtime` of both ends.
        Modification : UnixTimestamp
        /// `st_ctime` of both ends.
        StatusChange : UnixTimestamp
    }

/// What `fstat(2)` reports about a pipe the process made with `UnixPipe.pipe2`.
type PipeStatus =
    {
        /// The `st_ino` each end reports.
        Inodes : PipeInodes
        /// The `st_uid` and `st_gid` both ends report: the effective user and
        /// group of the process that created the pipe. Measured on Linux 6.18.5
        /// with real IDs 0 and effective IDs 1000 and 2000: 1000 and 2000.
        Owner : InodeOwner
        /// The permission bits both ends report: `UnixPipe.pipe2` creates a
        /// pipe with its flavour's, and on Linux `fchmod(2)` through either end
        /// changes them for both.
        Permissions : PermissionBits
        /// The timestamps both ends report.
        Times : PipeTimes
    }

/// The end of a launched pipe that the client kept, named by the descriptor
/// the process was given onto the pipe's other end in the launch table that
/// `UnixSystem.initial` took.
///
/// So the client that launched a process with its output on descriptor 1 finds
/// what the process wrote there under `ExternalEndpoint 1`, whatever the
/// process has since done with descriptor 1.
[<Struct>]
type ExternalEndpoint =
    | ExternalEndpoint of launchedOn : int

    override this.ToString () : string =
        match this with
        | ExternalEndpoint fd -> $"the client's end of launch descriptor %d{fd}"

/// What one descriptor of a launch table is: a pipe end the launcher made
/// before the process started, whose other end is the client's.
///
/// Each entry makes a pipe of its own, with a description of its own, so no two
/// launch descriptors share a description or a pipe.
[<RequireQualifiedAccess>]
type LaunchDescriptor =
    /// The read end of a pipe, opened `O_RDONLY`. The client holds the write end
    /// and writes `bytes` into it with one blocking `write(2)`, then closes it.
    ///
    /// Bytes the pipe cannot hold yet go in as the process reads, so a read
    /// finds the pipe as full as that sleeping write could have made it, and
    /// end of file comes only once the process has read every one. With no
    /// bytes, a read answers end of file at once.
    ///
    /// More bytes than one `write(2)` moves on the platform are refused.
    | Supplied of bytes : ImmutableArray<byte>
    /// The write end of a pipe, opened `O_WRONLY`. The client holds the read end
    /// and reads every byte the moment it is written, so a blocking write is
    /// never short and never waits. What the client read is in
    /// `UnixMachineState.Delivered`.
    | Drained
    /// The write end of a pipe, opened `O_WRONLY`, whose read end the client
    /// closed before the process started. Nothing will ever read it, so a write
    /// answers `EPIPE` and raises `SIGPIPE`, as `UnixReadWrite.write` describes.
    | Gone

/// The bytes a client has still to write into a pipe, oldest first. Never
/// none: a client with nothing left to write has closed its end.
///
/// Two are equal when they hold the same bytes.
[<CustomEquality ; NoComparison>]
type UnwrittenBytes =
    private
        {
            /// Every byte the client's write was given.
            All : ImmutableArray<byte>
            /// How many of `All` are already in the pipe or read; less than
            /// `All`'s length.
            Written : int
        }

    /// How many bytes are still to be written.
    member this.Length : int = this.All.Length - this.Written

    /// The bytes still to be written, oldest first.
    member this.ToImmutableArray () : ImmutableArray<byte> =
        ImmutableArray.Create (this.All, this.Written, this.Length)

    override this.Equals (other : obj) : bool =
        match other with
        | :? UnwrittenBytes as other ->
            this.Length = other.Length
            && System.MemoryExtensions.SequenceEqual (
                this.All.AsSpan (this.Written, this.Length),
                other.All.AsSpan (other.Written, other.Length)
            )
        | _ -> false

    override this.GetHashCode () : int = this.Length

/// The end of a launched pipe that the client holds, as it stands now.
[<RequireQualifiedAccess>]
type ClientEnd =
    /// The read end, from which the client reads every byte the moment it is
    /// written.
    | Draining
    /// The write end, the client asleep in its one write of the bytes it
    /// supplied because the pipe has no room for `unwritten`, the rest of them.
    /// It writes more each time a read makes room, and closes the end once the
    /// last byte is in.
    | Supplying of unwritten : UnwrittenBytes
    /// Nothing: the client has closed the write end, either because every byte
    /// it supplied went in, or because no reader was left to take the rest.
    | WriteEndClosed
    /// Nothing: the client closed the read end before the process started, so
    /// the pipe has no reader outside the process.
    | ReadEndClosed

/// Where a pipe came from, and so what is known about it besides its bytes.
[<RequireQualifiedAccess>]
type PipeOrigin =
    /// `UnixPipe.pipe2`, in this process, which is why this kernel knows what
    /// `fstat(2)` reports about it.
    | Made of status : PipeStatus
    /// The launch table's entry for `endpoint`'s descriptor, before the process
    /// started. The client holds the pipe's other end, and `client` is what it
    /// is doing with it now.
    ///
    /// Its owner and timestamps are the launcher's, which the launch table does
    /// not state, so `fstat(2)`, and `fchmod(2)` where it would consult the
    /// owner, are refused rather than invented.
    | Launched of endpoint : ExternalEndpoint * client : ClientEnd

/// A pipe, as the kernel's pipe table holds it: the bytes in flight between its
/// ends, and where it came from.
///
/// Holds nothing about which of its ends the process has open. That is derived
/// from the descriptor table, which is the only thing that can say it
/// truthfully: an end is open while some open file description names it, or
/// while the client holds it (see `PipeState.heldByClient`).
[<NoComparison>]
type PipeState =
    {
        /// The bytes written and not yet read, stored as the flavour stores them.
        Buffer : PipeBuffer
        /// Who made the pipe, and what that says about it.
        Origin : PipeOrigin
        /// How many reads by the process have taken bytes from the pipe.
        ///
        /// Darwin wakes every writer asleep on a pipe at each such read, room
        /// or not, and one that still cannot write either sleeps again or,
        /// through a description that has become non-blocking, gives up; a
        /// write's park records this count, so that its wake condition can say
        /// whether a read has happened since (`WakePrimitive.PipeReadWhileNonBlocking`).
        Reads : int64
    }

/// What a client asleep in a write did when a read made room in its pipe.
[<Struct>]
type internal ClientWriteProgress =
    {
        /// It wrote bytes into a pipe the read had emptied, which wakes the
        /// read end's waiters.
        WroteIntoEmpty : bool
        /// It wrote its last byte and closed its end, which wakes the read
        /// end's waiters whatever they wait for.
        Closed : bool
    }

[<RequireQualifiedAccess>]
module PipeState =
    /// Whether the client holds `pipeEnd` of `pipe` open: the read end of a
    /// pipe it drains, the write end of one it is still writing into, and no
    /// other. A pipe the process made has no end outside it.
    let heldByClient (pipeEnd : PipeEnd) (pipe : PipeState) : bool =
        match pipe.Origin, pipeEnd with
        | PipeOrigin.Launched (_, ClientEnd.Draining), PipeEnd.Read
        | PipeOrigin.Launched (_, ClientEnd.Supplying _), PipeEnd.Write -> true
        | PipeOrigin.Launched (_, ClientEnd.Draining), PipeEnd.Write
        | PipeOrigin.Launched (_, ClientEnd.Supplying _), PipeEnd.Read
        | PipeOrigin.Launched (_, ClientEnd.WriteEndClosed), _
        | PipeOrigin.Launched (_, ClientEnd.ReadEndClosed), _
        | PipeOrigin.Made _, _ -> false

    /// The client that reads `pipe` as fast as it is written, if one does.
    let drainedBy (pipe : PipeState) : ExternalEndpoint option =
        match pipe.Origin with
        | PipeOrigin.Launched (endpoint, ClientEnd.Draining) -> Some endpoint
        | PipeOrigin.Launched (_, ClientEnd.Supplying _)
        | PipeOrigin.Launched (_, ClientEnd.WriteEndClosed)
        | PipeOrigin.Launched (_, ClientEnd.ReadEndClosed)
        | PipeOrigin.Made _ -> None

    /// The pipe `descriptor` makes for launch descriptor `fd` on a machine of
    /// `platform`, as it stands when the process starts, and which of its ends
    /// the process is given.
    ///
    /// A pipe the client supplies holds what the client's write put in before
    /// it slept: the whole of it if it fits.
    let internal launch
        (platform : SimulatedUnixPlatform)
        (fd : int)
        (descriptor : LaunchDescriptor)
        : PipeState * PipeEnd
        =
        let endpoint = ExternalEndpoint fd
        let empty = PipeBuffer.empty platform

        match descriptor with
        | LaunchDescriptor.Drained ->
            {
                Buffer = empty
                Origin = PipeOrigin.Launched (endpoint, ClientEnd.Draining)
                Reads = 0L
            },
            PipeEnd.Write
        | LaunchDescriptor.Gone ->
            {
                Buffer = empty
                Origin = PipeOrigin.Launched (endpoint, ClientEnd.ReadEndClosed)
                Reads = 0L
            },
            PipeEnd.Write
        | LaunchDescriptor.Supplied bytes ->
            if bytes.IsDefault then
                failwith
                    $"PipeState.launch: launch descriptor %d{fd} supplies the default ImmutableArray, whose underlying array is null. To supply nothing, pass ImmutableArray<byte>.Empty."

            let maxTransfer =
                TransferCountLimit.maxTransfer (SimulatedUnixPlatform.transferCountLimit platform)

            if bytes.Length > maxTransfer then
                failwith
                    $"PipeState.launch: launch descriptor %d{fd} supplies %d{bytes.Length} bytes, more than the %d{maxTransfer} one write(2) moves on this platform. A supplied pipe is the client's one write of its bytes, so this many would take more than one."

            // The client's write starts as any write into an empty pipe does.
            let written, buffer = PipeBuffer.write bytes empty

            let client =
                if written = bytes.Length then
                    ClientEnd.WriteEndClosed
                else
                    ClientEnd.Supplying
                        {
                            All = bytes
                            Written = written
                        }

            {
                Buffer = buffer
                Origin = PipeOrigin.Launched (endpoint, client)
                Reads = 0L
            },
            PipeEnd.Read

    /// `pipe` once a client asleep in a write into it has written what the
    /// pipe now has room for, and closed its end if that was the last; and what
    /// it did. A pipe with no such client is returned as it is.
    ///
    /// For the read operation to call after taking bytes out of the pipe: the
    /// client writes before the process's next call, as fast as a real writer
    /// woken by that read could.
    let internal afterRead (pipe : PipeState) : PipeState * ClientWriteProgress =
        match pipe.Origin with
        | PipeOrigin.Made _
        | PipeOrigin.Launched (_, ClientEnd.Draining)
        | PipeOrigin.Launched (_, ClientEnd.WriteEndClosed)
        | PipeOrigin.Launched (_, ClientEnd.ReadEndClosed) ->
            pipe,
            {
                WroteIntoEmpty = false
                Closed = false
            }
        | PipeOrigin.Launched (endpoint, ClientEnd.Supplying unwritten) ->
            let wasEmpty = PipeBuffer.held pipe.Buffer = 0
            let taken, buffer = PipeBuffer.resume unwritten.All unwritten.Written pipe.Buffer
            let written = unwritten.Written + taken
            let closed = written = unwritten.All.Length

            let client =
                if closed then
                    ClientEnd.WriteEndClosed
                else
                    ClientEnd.Supplying
                        { unwritten with
                            Written = written
                        }

            { pipe with
                Buffer = buffer
                Origin = PipeOrigin.Launched (endpoint, client)
            },
            {
                WroteIntoEmpty = wasEmpty && taken > 0
                Closed = closed
            }

    /// `pipe` once its read end has closed for good: a client asleep in a write
    /// into it is woken with `EPIPE`, and closes its end.
    let internal readEndClosed (pipe : PipeState) : PipeState =
        match pipe.Origin with
        | PipeOrigin.Launched (endpoint, ClientEnd.Supplying _) ->
            { pipe with
                Origin = PipeOrigin.Launched (endpoint, ClientEnd.WriteEndClosed)
            }
        | PipeOrigin.Made _
        | PipeOrigin.Launched (_, ClientEnd.Draining)
        | PipeOrigin.Launched (_, ClientEnd.WriteEndClosed)
        | PipeOrigin.Launched (_, ClientEnd.ReadEndClosed) -> pipe

    /// Whether a client asleep in a write into `pipe` could write now: if so,
    /// it would not be asleep. False for a pipe with no such client.
    let internal clientWriteCouldProceed (pipe : PipeState) : bool =
        match pipe.Origin with
        | PipeOrigin.Launched (_, ClientEnd.Supplying unwritten) ->
            fst (PipeBuffer.resume unwritten.All unwritten.Written pipe.Buffer) > 0
        | PipeOrigin.Made _
        | PipeOrigin.Launched (_, ClientEnd.Draining)
        | PipeOrigin.Launched (_, ClientEnd.WriteEndClosed)
        | PipeOrigin.Launched (_, ClientEnd.ReadEndClosed) -> false

/// The bytes one `write(2)` delivered to a client that drains a pipe: what the
/// client read, in the order the process wrote it.
///
/// One per write that moved bytes, and never coalesced across writes, because a
/// write's boundaries are what a reader draining the pipe as fast as it is
/// written observes.
type Delivery =
    {
        /// The client's end the bytes reached.
        Endpoint : ExternalEndpoint
        /// The bytes, every one of them that the write moved.
        Bytes : ImmutableArray<byte>
    }
