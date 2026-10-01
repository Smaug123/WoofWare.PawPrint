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
/// before the process started, whose other end the client holds.
///
/// Each entry makes a pipe of its own, with a description of its own, so no two
/// launch descriptors share a description or a pipe.
[<RequireQualifiedAccess>]
type LaunchDescriptor =
    /// The read end of a pipe, opened `O_RDONLY`. The client held the write end,
    /// wrote nothing into it, and closed it before the process started, so a
    /// read answers end of file at once.
    | SuppliedNothing
    /// The write end of a pipe, opened `O_WRONLY`. The client holds the read end
    /// and reads every byte the moment it is written, so a blocking write is
    /// never short and never waits. What the client read is in
    /// `UnixMachineState.Delivered`.
    | Drained

/// Where a pipe came from, and so what is known about it besides its bytes.
[<RequireQualifiedAccess>]
type PipeOrigin =
    /// `UnixPipe.pipe2`, in this process, which is why this kernel knows what
    /// `fstat(2)` reports about it.
    | Made of status : PipeStatus
    /// The launch table's entry for `endpoint`'s descriptor, before the process
    /// started. The client holds the pipe's other end, as `descriptor` says.
    ///
    /// Its owner and timestamps are the launcher's, which the launch table does
    /// not state, so `fstat(2)`, and `fchmod(2)` where it would consult the
    /// owner, are refused rather than invented.
    | Launched of endpoint : ExternalEndpoint * descriptor : LaunchDescriptor

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
    }

[<RequireQualifiedAccess>]
module PipeState =
    /// Whether the client holds `pipeEnd` of `pipe` open: the read end of a
    /// pipe launched as `LaunchDescriptor.Drained`, and no other: the client
    /// closed a `LaunchDescriptor.SuppliedNothing` pipe's write end before the
    /// process started, and a pipe the process made has no end outside it.
    let heldByClient (pipeEnd : PipeEnd) (pipe : PipeState) : bool =
        match pipe.Origin, pipeEnd with
        | PipeOrigin.Launched (_, LaunchDescriptor.Drained), PipeEnd.Read -> true
        | PipeOrigin.Launched (_, LaunchDescriptor.Drained), PipeEnd.Write
        | PipeOrigin.Launched (_, LaunchDescriptor.SuppliedNothing), _
        | PipeOrigin.Made _, _ -> false

    /// The client that reads `pipe` as fast as it is written, if one does.
    let drainedBy (pipe : PipeState) : ExternalEndpoint option =
        match pipe.Origin with
        | PipeOrigin.Launched (endpoint, LaunchDescriptor.Drained) -> Some endpoint
        | PipeOrigin.Launched (_, LaunchDescriptor.SuppliedNothing)
        | PipeOrigin.Made _ -> None

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
