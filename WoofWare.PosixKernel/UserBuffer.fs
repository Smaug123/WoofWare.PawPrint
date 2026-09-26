namespace WoofWare.PosixKernel

/// Whether a kernel validates the whole of a user buffer before it performs a
/// read or write, and if so which ranges it accepts.
///
/// The two Unixes differ, and the difference is observable: a platform that
/// checks up front refuses an out-of-range buffer even when the call would have
/// transferred nothing, and even when the descriptor names something the call
/// would have refused for another reason.
[<RequireQualifiedAccess>]
type UserBufferCheck =
    /// `vfs_read` and `vfs_write` run `access_ok(buf, count)` before reaching
    /// the file operation. A range is accepted when `address + length`, in
    /// exact arithmetic, is at most this value — the machine's `TASK_SIZE_MAX`
    /// (see `ObservedUserAddressLimit` for values real machines have).
    | BeforeOperation of highestRangeEnd : uint64
    /// No up-front check, so a bad address is discovered by the copy itself and
    /// a call that copies nothing never faults.
    | AtCopyTime

/// Limits on the user half of the address space that real Linux machines have
/// been observed to impose, for a client picking one for a simulated machine.
///
/// Every one of these is `TASK_SIZE_MAX` for some real configuration, and the
/// value a machine has depends on its paging depth and virtual-address width as
/// well as on its architecture, which is why the simulated one is configuration
/// rather than a constant derived from the platform. A machine's limit must
/// still be one its architecture has: see `architectureOf`.
[<RequireQualifiedAccess>]
module ObservedUserAddressLimit =
    /// x86-64 with four-level paging: 2^47 less one page.
    [<Literal>]
    let X64FourLevelPaging : uint64 = 0x0000_7FFF_FFFF_F000UL

    /// x86-64 with five-level paging (LA57): 2^56 less one page.
    [<Literal>]
    let X64FiveLevelPaging : uint64 = 0x00FF_FFFF_FFFF_F000UL

    /// arm64 with a 48-bit virtual address: 2^48 exactly, the one observed
    /// value that is not a page short of a power of two.
    [<Literal>]
    let Arm64FortyEightBit : uint64 = 0x0001_0000_0000_0000UL

    // Measured by bisecting the highest buffer address `read(2)` and
    // `epoll_wait(2)` do not answer EFAULT for: the two x86-64 values on GitHub
    // `ubuntu-latest` runners (two of them in one CI run disagreed, which is what
    // shows this varies by machine rather than by kernel) and again on Debian's
    // 6.12 kernel under QEMU with `-cpu qemu64` and `-cpu max`; the arm64 value
    // on Linux 6.18.5 under Apple's `container`.
    // (docs/plans/2026-08-23-posix-kernel-extraction/architecture-facts-linux.c)

    /// The architecture of the machines observed to have `limit` as their
    /// `TASK_SIZE_MAX`, or `None` for a value no machine has been observed to
    /// have.
    let architectureOf (limit : uint64) : SimulatedUnixArchitecture option =
        match limit with
        | X64FourLevelPaging
        | X64FiveLevelPaging -> Some SimulatedUnixArchitecture.X64
        | Arm64FortyEightBit -> Some SimulatedUnixArchitecture.Arm64
        | _ -> None

/// The two constants of Linux's `epoll_wait` that follow from
/// `sizeof(struct epoll_event)`, which is an architecture's fact rather than
/// the kernel source's: `linux/eventpoll.h` packs the struct on x86-64 alone.
///
/// Darwin has no answer to either: a wait on a kqueue reads neither of these.
[<RequireQualifiedAccess>]
module LinuxEpollLimits =
    /// `sizeof(struct epoll_event)` on `architecture`: the unit of the byte
    /// range `epoll_wait` screens with
    /// `access_ok(events, maxevents * sizeof(struct epoll_event))`.
    let eventSize (architecture : SimulatedUnixArchitecture) : int =
        // `{ __poll_t events; __u64 data; }`, which `EPOLL_PACKED` packs under
        // `#ifdef __x86_64__` and leaves padded to 16 everywhere else. Measured
        // three ways per architecture (the compiler's `sizeof`, the stride of the
        // range `access_ok` screens, and the bytes one delivered event writes):
        // 12 on x86-64 and 16 on aarch64, by
        // docs/plans/2026-08-23-posix-kernel-extraction/architecture-facts-linux.c.
        match architecture with
        | SimulatedUnixArchitecture.X64 -> 12
        | SimulatedUnixArchitecture.Arm64 -> 16

    /// `EP_MAX_EVENTS` on `architecture`, which is
    /// `INT_MAX / sizeof(struct epoll_event)`. `epoll_wait` rejects a
    /// `maxevents` above this with EINVAL, and the bound is what keeps
    /// `maxevents * eventSize architecture` inside `int32` for every count that
    /// gets past it, so a caller must consult it before computing that
    /// product, not after.
    let maxEvents (architecture : SimulatedUnixArchitecture) : int =
        // Measured by bisection over [1, INT_MAX]: 178956970 on x86-64 and
        // 134217727 on aarch64, each the quotient exactly.
        System.Int32.MaxValue / eventSize architecture

/// Where a buffer argument to a syscall is, as far as this kernel's own address
/// check can see.
///
/// Only buffers *the kernel itself* would dereference are described here. A
/// client whose foreign-function layer dereferences a pointer on its own account
/// — a wrapper reading a caller's length out-parameter before it makes the call
/// — answers for that itself, because a fault there happens in the client's code
/// and no kernel is involved.
///
/// There is deliberately no `Null` case. A null pointer is an ordinary low
/// address to a kernel: it passes the range check like any other, and faults
/// where any unmapped address would. Foreign-function layers commonly screen for
/// null before calling, and that screen belongs to whoever wrote it.
[<RequireQualifiedAccess>]
type UserBuffer =
    /// An address naming no storage the client can transfer bytes through. The
    /// kernel's answer is EFAULT — at whichever step it first looks, which is
    /// not necessarily the first step of the syscall.
    | Unmapped of address : uint64
    /// Real storage. The kernel never learns where: an address is what a range
    /// check needs, and `Mapped` is in range by construction.
    | Mapped
    /// A real user address whose bytes the client cannot produce.
    ///
    /// Not `Unmapped`: EFAULT would be a wrong answer rather than an approximate
    /// one, because the memory really is mapped and a real kernel really would
    /// transfer it. Passes every address check — there is nothing out of range
    /// about it — and has no answer only at the point of transfer.
    | Opaque
    /// Not an address at all: a value the client keeps symbolic because it has
    /// no number for it.
    ///
    /// Distinct from `Opaque`, which names real memory whose address merely goes
    /// unmodelled. A platform that screens addresses up front cannot be asked
    /// about this one — the answer is not "out of range", it is unknown — while
    /// a platform that screens nothing gets as far as the transfer before it
    /// runs out of answer.
    | Addressless

/// Why this kernel cannot say what a syscall does with the buffer it was given.
///
/// Both cases are gaps in *representation* rather than in measurement: what a
/// real kernel does is known in each, and it is the model that cannot hold the
/// answer. That is a second genus of refusal from the measured divergences
/// elsewhere in this library, and a message composed for one should not claim to
/// be the other.
[<RequireQualifiedAccess>]
type BufferRefusal =
    /// An `Opaque` buffer reached the transfer.
    | OpaqueAtTransfer
    /// An `Addressless` buffer reached an address check.
    | AddresslessAtScreen
    /// An `Addressless` buffer reached the transfer.
    | AddresslessAtTransfer

[<RequireQualifiedAccess>]
module BufferRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half — which entry point, which argument, and what the value
    /// actually was, none of which this library ever saw.
    let describe (refusal : BufferRefusal) : string =
        match refusal with
        | BufferRefusal.OpaqueAtTransfer ->
            "the buffer names memory whose bytes the caller cannot produce. A real kernel would transfer the bytes at that address, so EFAULT would be a wrong answer rather than an approximate one, and there is nothing else to give."
        | BufferRefusal.AddresslessAtScreen ->
            "the buffer is not an address at all, and this platform screens a buffer's address against its limit before performing the operation. There is no address to screen, so whether the kernel would accept it is unknown rather than false."
        | BufferRefusal.AddresslessAtTransfer ->
            "the buffer is not an address at all, and the transfer would have to dereference it. This platform screens nothing up front, so the call gets this far before running out of answer."

[<RequireQualifiedAccess>]
module UserBufferCheck =
    /// Whether this platform refuses a buffer of `length` bytes at `address`
    /// before performing the operation at all.
    let faultsBeforeOperation (check : UserBufferCheck) (address : uint64) (length : uint64) : bool =
        match check with
        | UserBufferCheck.AtCopyTime -> false
        | UserBufferCheck.BeforeOperation highestRangeEnd ->
            // Rearranged to subtract rather than add, so that a range end past
            // `UInt64.MaxValue` is a refusal instead of wrapping onto a low
            // address the check would accept. The first disjunct is what keeps
            // the subtraction in the second from underflowing.
            length > highestRangeEnd || address > highestRangeEnd - length

    /// Whether a buffer of `length` bytes faults before the operation is
    /// performed at all, for a buffer this kernel has classified.
    ///
    /// `false` is not "the buffer is fine": it means this step raises no
    /// objection, and a later one still may. `Opaque` and `Addressless` both
    /// pass a platform that screens nothing, and both still have no answer at
    /// the transfer.
    ///
    /// `length` is the count the caller asked for, not the storage's size. A
    /// kernel bounds a range against the address space, never against the
    /// caller's own allocation.
    let faultsBeforeOperationFor
        (check : UserBufferCheck)
        (buffer : UserBuffer)
        (length : uint64)
        : Result<bool, BufferRefusal>
        =
        match buffer with
        // In range by construction: real storage is inside the user address
        // space of every platform this library models, so the check is not
        // performed rather than performed and passed.
        | UserBuffer.Mapped -> Ok false
        // Real mapped memory, so it too is in range; it runs out of answer only
        // where bytes are wanted.
        | UserBuffer.Opaque -> Ok false
        | UserBuffer.Unmapped address -> Ok (faultsBeforeOperation check address length)
        | UserBuffer.Addressless ->
            match check with
            | UserBufferCheck.AtCopyTime -> Ok false
            | UserBufferCheck.BeforeOperation _ -> Error BufferRefusal.AddresslessAtScreen
