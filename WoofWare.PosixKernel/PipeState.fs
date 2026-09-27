namespace WoofWare.PosixKernel

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

/// A pipe, as the kernel's pipe table holds it: the bytes in flight between its
/// ends, and what `fstat(2)` reports about it.
///
/// Holds nothing about which of its ends are open. That is derived from the
/// descriptor table, which is the only thing that can say it truthfully: an end
/// is open while some open file description names it.
[<NoComparison>]
type PipeState =
    {
        /// The bytes written and not yet read, stored as the flavour stores them.
        Buffer : PipeBuffer
        /// The `st_ino` each end reports.
        Inodes : PipeInodes
        /// The `st_uid` and `st_gid` both ends report: the effective user and
        /// group of the process that created the pipe. Measured on Linux 6.18.5
        /// with real IDs 0 and effective IDs 1000 and 2000: 1000 and 2000.
        Owner : InodeOwner
        /// The timestamps both ends report.
        Times : PipeTimes
    }
