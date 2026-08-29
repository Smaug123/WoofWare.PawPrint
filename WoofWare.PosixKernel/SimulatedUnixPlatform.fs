namespace WoofWare.PosixKernel

open System.Buffers.Binary

/// <summary>
/// Which Unix we are simulating.
/// </summary>
/// <remarks>
/// This is essentially a bundling of the many different ways various Unix platforms can differ.
/// (For example, it includes errno numbering, permission bit handling on symlinks, whether <c>stat</c>
/// reports creation times, etc.)
/// </remarks>
[<RequireQualifiedAccess>]
type SimulatedUnixFlavour =
    /// <summary>Linux.</summary>
    | Linux
    /// <summary>Darwin, i.e. macOS.</summary>
    /// <remarks><c>uname -r</c> reports the Darwin kernel release rather than the macOS product version.</remarks>
    | Darwin

/// The filesystem an emulated mount claims to be, as `fstatfs(2)` reports it.
///
/// A *choice* rather than a measured fact, because PawPrint's filesystem is an
/// in-memory graph that is not any real filesystem. That is why it is
/// something a client configures rather than a derivation from the flavour the
/// way the errno numbering is: a single Linux reports `0xEF53`,
/// `0x01021994` and `0x9FA0` for three directories in one process, so a flavour
/// does not determine a mount's type. It does *constrain* it, which is what
/// `EmulatedFileSystemType.isReportableUnder` carries.
///
/// This changes what `SystemNative_GetFileSystemType` answers and nothing else.
/// Path resolution keeps its flavour's limits either way — `pathLimits`
/// carries `NameLengthLimit` as an ext4-versus-APFS fact — so a kernel
/// configured `Nfs` reports NFS while still resolving names as its flavour
/// does.
///
/// Only three cases, because only three have a consumer. Note that a fourth
/// could not be `Ext4`: the managed layer cannot distinguish it, CoreLib's
/// `UnixFileSystemTypes` having no such member (it is `ext2 = 0xEF53`, with
/// `ext4` commented out as an alias).
[<RequireQualifiedAccess>]
type EmulatedFileSystemType =
    /// Linux's in-memory filesystem, and so the honest analogue of a
    /// filesystem that only ever exists in memory.
    | Tmpfs
    /// What a macOS file is on. Darwin's answer, since it mounts no tmpfs.
    | Apfs
    /// One of the four filesystems CoreCLR refuses to take a *shared* lock on
    /// (`SafeFileHandle.CanLockTheFile`), so a mount of this type is the one
    /// configuration under which a `FileShare.Read` handle opened for writing
    /// takes no `flock` at all.
    | Nfs

/// What `fstatfs(2)` does when asked about one descriptor.
///
/// Modelled as a success-or-failure rather than as the bare `uint32` the PAL
/// returns, because the PAL folds *every* failure to 0 and the errno the
/// kernel left behind is still observable to a guest that declares
/// `SetLastError`. Collapsing the two here would lose it.
[<RequireQualifiedAccess>]
type FileSystemTypeAnswer =
    /// `fstatfs` succeeded and named this filesystem.
    | Reported of magic : uint32
    /// `fstatfs` failed, leaving this errno. The PAL reports 0 to its caller.
    | Failed of error : UnixError

[<RequireQualifiedAccess>]
module EmulatedFileSystemType =
    /// The number `fstatfs(2)` reports for a file on a mount of this type.
    ///
    /// These are the values CoreLib's `Interop.Sys.UnixFileSystemTypes` gives
    /// them, which is what matters: that enum is how the only managed consumer
    /// reads the number back. Each was also measured on a live kernel — tmpfs
    /// on Linux's `/dev/shm`, APFS on a macOS `/tmp`.
    ///
    /// Linux returns its `statfs.f_type` verbatim while Darwin maps
    /// `f_fstypename` through a name table (`MapFileSystemNameToEnum`,
    /// `pal_io.c`), so the two arrive at the same number by different routes;
    /// `Nfs` is the one case both flavours can produce, and both produce
    /// `0x6969`.
    let magic (fsType : EmulatedFileSystemType) : uint32 =
        match fsType with
        | EmulatedFileSystemType.Tmpfs -> 0x01021994u
        | EmulatedFileSystemType.Apfs -> 0x1Au
        | EmulatedFileSystemType.Nfs -> 0x6969u

    /// The type a mount reports when a host expresses no preference.
    ///
    /// `Tmpfs` under Linux because PawPrint's filesystem really is in memory,
    /// and `Apfs` under Darwin because macOS mounts no tmpfs, so nothing there
    /// could report one.
    let defaultFor (flavour : SimulatedUnixFlavour) : EmulatedFileSystemType =
        match flavour with
        | SimulatedUnixFlavour.Linux -> EmulatedFileSystemType.Tmpfs
        | SimulatedUnixFlavour.Darwin -> EmulatedFileSystemType.Apfs

    /// Whether a kernel of this flavour could report this filesystem type at
    /// all.
    ///
    /// The flavour does not *determine* a mount's type, but it does rule
    /// several out, and a kernel that claimed one of those would be handing a
    /// guest a fact no real system of the platform it impersonates could
    /// produce. Written as an exhaustive pair match rather than as a
    /// predicate over one axis, so that a new flavour or a new filesystem
    /// stops compiling until someone has looked the combination up.
    let isReportableUnder (flavour : SimulatedUnixFlavour) (fsType : EmulatedFileSystemType) : bool =
        match fsType, flavour with
        // Measured: `/dev/shm` reports it. macOS mounts no tmpfs at all, so
        // its `f_fstypename` is never "tmpfs" — the name table has a row for
        // it, but nothing on Darwin ever hits that row.
        | EmulatedFileSystemType.Tmpfs, SimulatedUnixFlavour.Linux -> true
        | EmulatedFileSystemType.Tmpfs, SimulatedUnixFlavour.Darwin -> false
        // No mainline Linux filesystem reports `0x1A`; a FUSE-mounted APFS
        // reports fuse's own `0x65735546`.
        | EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Linux -> false
        | EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Darwin -> true
        // Both mount NFS, and both report `0x6969` for it.
        | EmulatedFileSystemType.Nfs, SimulatedUnixFlavour.Linux
        | EmulatedFileSystemType.Nfs, SimulatedUnixFlavour.Darwin -> true

    /// What `fstatfs(2)` answers about one descriptor: `None` for an fd the
    /// process does not hold.
    ///
    /// The whole table lives here rather than in the handler, so that the unit
    /// tests, the host-comparison oracle and the guest all exercise the same
    /// function — a mutation swapping two of the rows below has nowhere to
    /// hide.
    ///
    /// Every row measured on both flavours (macOS 26.6, Linux 6.x), for both
    /// ends of a pipe, an `AF_INET` and an `AF_UNIX` socket, an epoll port, a
    /// kqueue, a regular file, a directory and an unknown descriptor.
    ///
    /// Refuses a `flavour` and `mount` that do not describe one machine.
    let reportedFor
        (flavour : SimulatedUnixFlavour)
        (mount : EmulatedFileSystemType)
        (target : OpenFileObject option)
        : FileSystemTypeAnswer
        =
        // The two arguments are a *pair*: a file's answer comes from the mount
        // and every other descriptor's from the flavour, so a caller supplying
        // one of each would get a machine that is Linux for its pipes and macOS
        // for its files. `withUnixPlatformAndFileSystemType` writes both fields
        // A client is expected to write both together for that reason, but no
        // client can be made to: a state record assembled field by field
        // bypasses whatever setter it provides. Checking here rather than
        // trusting the caller is what keeps this function's contract true
        // wherever it is reached.
        if not (isReportableUnder flavour mount) then
            failwith
                $"EmulatedFileSystemType.reportedFor: asked what a %O{flavour} kernel reports for a %O{mount} mount, which %O{flavour} cannot have. The flavour and the mount type have come apart; they constrain each other (see EmulatedFileSystemType.isReportableUnder) and must be chosen together rather than set one at a time."

        /// Darwin's `fstatfs` refuses every object that is not on a
        /// filesystem, uniformly; Linux's succeeds and names the
        /// pseudo-filesystem the object lives on. So each of these rows is a
        /// measured number rather than an invention — unlike `fstat`, which
        /// refuses the same descriptors because it owes them seventeen fields
        /// and the platforms agree on none of them.
        let pseudoFileSystem (linux : uint32) : FileSystemTypeAnswer =
            match flavour with
            | SimulatedUnixFlavour.Linux -> FileSystemTypeAnswer.Reported linux
            | SimulatedUnixFlavour.Darwin -> FileSystemTypeAnswer.Failed UnixError.EINVAL

        match target with
        | None -> FileSystemTypeAnswer.Failed UnixError.EBADF
        // Regular files and directories alike: measured identical, and one
        // mount has one answer.
        | Some (OpenFileObject.File _) -> FileSystemTypeAnswer.Reported (magic mount)
        // PawPrint models the standard streams as pipes (see
        // `FileDescriptorRegistry.initial`), so this row is a consequence of
        // that existing decision rather than a new one: Linux's `pipefs`.
        | Some (OpenFileObject.StandardStream _) -> pseudoFileSystem 0x50495045u
        // Linux's `sockfs`.
        | Some (OpenFileObject.Socket _) -> pseudoFileSystem 0x534F434Bu
        // Linux's `anon_inodefs`, which is where an epoll port lives — and
        // exactly the granularity this answer needs, which is why
        // `OpenFileObject` folding every anonymous object into one case costs
        // nothing here.
        | Some OpenFileObject.AnonymousInode -> pseudoFileSystem 0x09041934u

/// Why a string is not usable as a `utsname.release`.
[<RequireQualifiedAccess>]
type SimulatedUnixReleaseError =
    /// Every Unix fills `utsname.release`, so the empty string names no system.
    | Empty
    /// Longer than any `utsname.release` can hold.
    | TooLong of length : int * limit : int
    /// The value is handed to the guest as a C string of single bytes, so a
    /// non-ASCII character has no faithful encoding and an embedded NUL would
    /// silently truncate what the guest sees.
    | NotPrintableAscii of index : int * character : char

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

/// Limits on the user half of the address space that real machines have been
/// observed to impose, for a host picking one for a simulated machine.
///
/// Every one of these is `TASK_SIZE_MAX` for some real configuration; the value
/// is a property of the *machine* (its paging depth, its virtual-address width)
/// rather than of the kernel or the distribution, which is why the simulated
/// one is configuration rather than a constant derived from the platform.
[<RequireQualifiedAccess>]
module ObservedUserAddressLimit =
    /// x86-64 with four-level paging: 2^47 less one page. Measured on a GitHub
    /// `ubuntu-latest` runner.
    [<Literal>]
    let X64FourLevelPaging : uint64 = 0x0000_7FFF_FFFF_F000UL

    /// x86-64 with five-level paging (LA57): 2^56 less one page. Measured on a
    /// different `ubuntu-latest` runner in the same CI run as the above, which
    /// is what shows this varies by machine rather than by kernel.
    [<Literal>]
    let X64FiveLevelPaging : uint64 = 0x00FF_FFFF_FFFF_F000UL

    /// arm64 with a 48-bit virtual address: 2^48 exactly, the one observed
    /// value that is not a page short of a power of two. Measured on a Linux
    /// guest under Apple's `container`.
    [<Literal>]
    let Arm64FortyEightBit : uint64 = 0x0001_0000_0000_0000UL

/// The two constants of Linux's `epoll_wait` that follow from
/// `sizeof(struct epoll_event)`.
///
/// That size is an *architecture* fact, not a flavour one, so these are not
/// derived from `SimulatedUnixPlatform`: `linux/eventpoll.h` defines
/// `EPOLL_PACKED` as `__attribute__((packed))` under `#ifdef __x86_64__` and
/// empty otherwise, over `{ __poll_t events; __u64 data; }`. The values here are
/// x86-64's, which is right for `SimulatedUnixPlatform.linuxX64` — the only
/// Linux platform PawPrint can currently be asked to simulate. A linux-arm64
/// preset would want 16 and 134_217_727, and this is the one place to teach.
///
/// Kept out of `SimulatedUnixPlatform` itself because every fact derived from
/// that type is a total function of the flavour, and epoll has no Darwin answer:
/// `SystemNative_WaitForSocketEvents`' kqueue arm reads neither of these.
///
/// Note that `SocketEventBufferElementSize` — the stride of the buffer CoreLib
/// allocates — is *not* affected, and so is absent here: it is
/// `max(sizeof(struct epoll_event), sizeof(SocketEvent))`, and that `max` is 16
/// under either packing.
[<RequireQualifiedAccess>]
module LinuxEpollLimits =
    /// `sizeof(struct epoll_event)`. The unit of the byte range `epoll_wait`
    /// screens with `access_ok(events, maxevents * sizeof(struct epoll_event))`.
    [<Literal>]
    let EventSize : int = 12

    /// `EP_MAX_EVENTS`, which is `INT_MAX / sizeof(struct epoll_event)`
    /// (fs/eventpoll.c). `epoll_wait` rejects a `maxevents` above this with
    /// EINVAL, and the bound is what keeps `maxevents * EventSize` inside
    /// `int32` for every count that gets past it — so a handler must consult it
    /// before computing that product, not after.
    ///
    /// `TestLinuxEpollLimits` checks the arithmetic rather than trusting the
    /// literal.
    [<Literal>]
    let MaxEvents : int = 178_956_970

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

/// Identity of the Unix-shaped platform the simulated process believes it is
/// running on. Consulted by the `SystemNative_*` entry points that report
/// host identity — today only `SystemNative_GetUnixRelease`, which surfaces
/// as `Environment.OSVersion` on a Unix CoreLib.
///
/// This is a value in kernel state rather than a host read, for the same
/// reason `ProcessorCount` is: real CoreCLR answers it from `uname(2)`, which
/// would make a replay depend on the machine that produced it — and worse,
/// guests branch on `Environment.OSVersion` (feature detection, quirk
/// workarounds), so letting the host leak in here would change guest
/// *control flow* between runs.
///
/// Modelled as a flavour plus a release string, rather than as a bag of loose
/// `utsname` fields, so that the facts we report stay mutually consistent as
/// more of `utsname` gets implemented: a future `SystemNative_GetUnixVersion`
/// or `SystemNative_GetOSArchitecture` is a new total *function* of the
/// flavour, not a new independently-settable string that could claim a Darwin
/// release alongside an x86_64 machine.
///
/// One representation per platform, which is what the flavour buys: every
/// platform-dependent fact below is a total function of it, with no failure
/// arms for an unclassifiable platform.
///
/// Construct with `SimulatedUnixPlatform.linuxX64`, `macOsArm64`, or `create`
/// for a specific release string.
[<CustomEquality ; NoComparison>]
type SimulatedUnixPlatform =
    private
        {
            Flavour : SimulatedUnixFlavour
            Release : string
        }

    override this.ToString () : string = $"%O{this.Flavour} %s{this.Release}"

    override this.Equals (other : obj) : bool =
        match other with
        | :? SimulatedUnixPlatform as other -> this.Flavour = other.Flavour && this.Release = other.Release
        | _ -> false

    override this.GetHashCode () : int =
        System.HashCode.Combine (this.Flavour, this.Release)

/// The four `sizeof`s `SystemNative_GetSocketAddressSizes` reports in one call,
/// which `System.Net.Primitives`' `SocketAddressPal` class initialiser latches
/// and every `SocketAddress` is then sized by.
///
/// Compile-time properties of the native shim rather than of any socket, like
/// `reportsBirthTime`. Measured with a `sizeof` probe compiled on macOS arm64 and
/// on Linux, rather than recalled; all four are invariant of pointer width, since
/// every member of these structs is fixed-width and the two variable-length tails
/// (`sun_path`, `sockaddr_storage`'s padding) are sized from a constant.
type SocketAddressSizes =
    {
        /// `sizeof(struct sockaddr_in)`. 16 on both.
        InterNetwork : int
        /// `sizeof(struct sockaddr_in6)`. 28 on both.
        InterNetworkV6 : int
        /// `sizeof(struct sockaddr_un)`. The one of the four that differs: 110 on
        /// Linux, whose `sun_path` is 108 bytes, against 106 on Darwin, whose is
        /// 104.
        UnixDomain : int
        /// `sizeof(struct sockaddr_storage)`. 128 on both, and the same number
        /// `SystemNative_GetMaximumAddressSize` reports through its own entry
        /// point — hence `SimulatedUnixPlatform.maximumSocketAddressSize` rather
        /// than a second literal.
        Storage : int
    }

/// Where a `struct sockaddr`'s address family sits and how wide it is — the only
/// part of the socket-address layout the two Unixes lay out differently.
///
/// BSD gave `struct sockaddr` a leading one-byte `sa_len` and narrowed
/// `sa_family_t` to one byte to pay for it; Linux kept the original two-byte
/// `sa_family_t` and has no length byte. That is why every *later* field agrees
/// between the two — `sin_port` at 2, `sin_addr` at 4, `sin6_addr` at 8,
/// `sin6_scope_id` at 24, all measured on both — since the two layouts spend the
/// same two leading bytes differently rather than in different amounts.
///
/// One field of one of the `sockaddr` structs, as a byte range within it.
///
/// Offset and width travel together because a caller that has one always wants
/// the other: every use is either "read these bytes" or "does the caller's
/// declared length reach them".
///
/// **Carries no byte order**, deliberately. The fields' orders are kernel ABI --
/// `sin_port` and `sin_addr` are network order, `sin6_scope_id` is the host's --
/// but whether a given *caller* swaps is that caller's own contract, and the two
/// do not agree: `SystemNative_GetPort` byte-swaps where
/// `SystemNative_GetIPv4Address` copies the address word verbatim, both sides of
/// that call holding it in network order. An order carried here would invite an
/// order-normalising accessor, and the first caller to reach for one would
/// silently acquire a swap its own contract does not have.
type SockaddrField =
    {
        /// Byte offset of the field from the start of the struct.
        Offset : int
        /// The field's width in bytes.
        Width : int
    }

[<RequireQualifiedAccess>]
module SockaddrField =
    /// Whether a declared sockaddr length reaches all of this field.
    ///
    /// A negative length fails it, and that is not incidental: a layer that casts
    /// the length to an unsigned type makes the bound enormous rather than
    /// negative, so this answers for what the *caller declared* before any such
    /// cast.
    ///
    /// A malformed descriptor is refused rather than answered for. `SockaddrField`
    /// is a public record, so unlike the closed `SockaddrFamilyField` it can be
    /// built with nonsense; a negative offset or width describes no field of any
    /// struct.
    let reachedBy (field : SockaddrField) (declaredLength : int) : bool =
        if field.Offset < 0 || field.Width < 0 then
            failwith
                $"SockaddrField.reachedBy: a field at offset %d{field.Offset} of width %d{field.Width} describes no part of any struct (this is a bug in the caller)."

        // Rearranged to subtract rather than add, so that a field whose end is
        // past `Int32.MaxValue` is not reached instead of wrapping onto a low
        // bound that every length satisfies. The same rearrangement, for the same
        // reason, as `UserBufferCheck.faultsBeforeOperation`. The guard above is
        // what keeps this subtraction from underflowing.
        declaredLength >= field.Offset && declaredLength - field.Offset >= field.Width

/// `struct sockaddr_in`'s fields beyond the family, which
/// `SockaddrFamilyField` describes because it is the one that moves between
/// platforms.
///
/// Measured on Linux 6.18.5 and Darwin 25.6.0 with
/// `docs/plans/2026-08-23-posix-kernel-extraction/sockaddr-layout.c`: both put
/// `sin_port` at 2 and `sin_addr` at 4, and both make the struct 16 bytes. These
/// are therefore plain values rather than functions of the platform -- the same
/// distinction `internetAddressFamily` draws against `internetV6AddressFamily`.
[<RequireQualifiedAccess>]
module InternetSockaddr =
    /// `sin_port`, in network byte order.
    let port : SockaddrField =
        {
            Offset = 2
            Width = 2
        }

    /// `sin_addr`, four bytes in network byte order. A caller that holds an
    /// address in that order too moves it verbatim in both directions.
    let address : SockaddrField =
        {
            Offset = 4
            Width = 4
        }

/// `struct sockaddr_in6`'s fields beyond the family, measured alongside the
/// above and likewise identical on both platforms.
///
/// A separate module from `InternetSockaddr` rather than a shared set of
/// constants, though `sin6_port` and `sin_port` coincide: they are two fields of
/// two structs, and a use site should say which struct it means. Note what this
/// does *not* buy -- `sin6_flowinfo` and `sin_addr` both sit at offset 4, so
/// confusing them is still a mutation nothing can catch.
[<RequireQualifiedAccess>]
module InternetV6Sockaddr =
    /// `sin6_port`, in network byte order. The same offset and width as
    /// `sin_port`, and stated separately because it is a different field.
    let port : SockaddrField =
        {
            Offset = 2
            Width = 2
        }

    /// `sin6_flowinfo`. Nothing in the managed surface reads it, but
    /// `SystemNative_SetIPv6Address` zeroes it, so it is not merely ignored.
    let flowInfo : SockaddrField =
        {
            Offset = 4
            Width = 4
        }

    /// `sin6_addr`. Its width is `sizeof(struct in6_addr)`, which is the length
    /// every IPv6 address buffer must have room for.
    let address : SockaddrField =
        {
            Offset = 8
            Width = 16
        }

    /// `sin6_scope_id`, four bytes in the *host's* own byte order -- unlike the
    /// port beside it, which is network order.
    let scopeId : SockaddrField =
        {
            Offset = 24
            Width = 4
        }

/// A pair of numbers rather than an `int * int` so that no caller can pair an
/// offset with the wrong width: the two vary together and never independently.
[<RequireQualifiedAccess>]
type SockaddrFamilyField =
    /// Linux: `sa_family_t` is a two-byte `unsigned short` at offset 0, in the
    /// machine's own byte order, and there is no length byte before it.
    | TwoBytesAtOffsetZero
    /// Darwin and the BSDs: `sa_len` occupies byte 0 and the one-byte
    /// `sa_family_t` follows it at offset 1.
    ///
    /// Nothing in the shim writes `sa_len` — grep `pal_networking.c` and there is
    /// no mention of it. The byte a guest sees there is written by managed code:
    /// `SocketAddress..ctor` stores `(byte) _size` at index 0 before calling
    /// `SetAddressFamily`, unconditionally on every platform, so BSD gets its
    /// length byte and Linux has the same store overwritten by the wider family.
    | OneByteAtOffsetOne

[<RequireQualifiedAccess>]
module SockaddrFamilyField =
    /// Byte offset of the family field within any `struct sockaddr`.
    let offset (field : SockaddrFamilyField) : int =
        match field with
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> 0
        | SockaddrFamilyField.OneByteAtOffsetOne -> 1

    /// Width of the family field in bytes. Also what the shim's
    /// `sizeof_member(sockaddr, sa_family)` bounds check uses, and what a
    /// conversion failure truncates the unconvertible value to.
    let width (field : SockaddrFamilyField) : int =
        match field with
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> 2
        | SockaddrFamilyField.OneByteAtOffsetOne -> 1

    /// Whether a declared sockaddr length reaches the family field at all.
    ///
    /// Two callers with two justifications. A kernel's copy-in helper reads
    /// nothing on Darwin at a length this rejects, which is why `connect(2)` can
    /// answer without touching the caller's buffer at all; and a foreign-function
    /// layer that screens the field before reading or writing it asks exactly the
    /// same arithmetic. Both are this one comparison, so it lives here rather
    /// than being written out twice.
    ///
    /// A negative length fails it, and that is not incidental: a layer that casts
    /// the length to an unsigned type makes the bound enormous rather than
    /// negative, so this answers for what the *caller declared* before any such
    /// cast.
    let reachedBy (field : SockaddrFamilyField) (declaredLength : int) : bool =
        SockaddrField.reachedBy
            {
                Offset = offset field
                Width = width field
            }
            declaredLength

/// Everything a kernel does differently when `open(2)` is asked to *create*.
///
/// One record rather than a scatter of booleans, because the divergence is
/// several facts that always travel together: a platform that answers one of
/// them Linux's way answers all of them Linux's way, and a third Unix must
/// supply every field before it compiles. All four were measured on macOS
/// 26.6/APFS and Linux 6.x, at an unprivileged uid.
type CreatingOpenRules =
    {
        /// What the walk owes a final component carrying a trailing separator.
        /// Linux refuses such a path outright; Darwin resolves it as any lookup
        /// would, so `open("d/", O_CREAT)` opens the directory there and is
        /// EISDIR on Linux.
        TrailingSeparator : TrailingSeparatorPolicy
        /// Whether a creating open that lands on an existing *directory* is
        /// refused. Linux answers EISDIR — so `open(dir, O_RDONLY|O_CREAT)`
        /// fails where a plain `open(dir, O_RDONLY)` succeeds — while Darwin
        /// treats `O_CREAT` as having no bearing on an object that exists.
        ///
        /// `O_EXCL`'s EEXIST is measured to beat this on both, so a caller must
        /// check that first.
        RefusesExistingDirectory : bool
        /// What a path that consumed *no component at all* — "/" itself, or a
        /// symlink whose target is "/" — owes a creating open.
        ///
        /// Darwin answers EEXIST even without `O_EXCL`; Linux folds the case
        /// into `RefusesExistingDirectory` and so wants `None` here. Pinned as a
        /// property of the *navigation* rather than of the root inode: on macOS
        /// "/" is EEXIST while "/.", "/../" and "/private/.." reach the same
        /// inode and open fine, and "/System/Volumes/Data" — a writable volume's
        /// mount root — opens fine too, which rules out a read-only-mount
        /// artefact.
        RootNavigation : UnixError option
        /// The bits `open(2)` keeps from its `mode` argument before the umask is
        /// applied. XNU masks with `ACCESSPERMS`, so a Darwin guest cannot
        /// create a setuid, setgid or sticky file at all — measured, 0o4644,
        /// 0o2644 and 0o1644 all land as 0o644. Linux keeps all twelve bits.
        ModeMask : PermissionBits
    }

/// What `open(2)` should do next, once the path has been resolved and the
/// creating flags have been read.
///
/// A verdict rather than an action, so the rule can be decided — and compared
/// against a real kernel — without a machine to act on it. The handler is then
/// only the part that cannot be pure: allocating the inode, registering a
/// descriptor and pushing the result.
[<RequireQualifiedAccess>]
type CreatingOpenVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty regular file under `name` in `directory`.
    | Create of directory : InodeNumber * name : FileName
    /// The object is already there; open it, subject to the checks any
    /// non-creating open would apply.
    | OpenExisting of inode : InodeNumber

[<RequireQualifiedAccess>]
module CreatingOpenRules =
    /// Decide what an `open(2)` owes, given how its path resolved and whether it
    /// carried `O_CREAT` and `O_EXCL`.
    ///
    /// The order of the refusals is measured, and each beats the ones below it:
    ///
    ///  * `O_EXCL` on anything that exists is EEXIST — including a directory,
    ///    where it beats the EISDIR below: `open(".", O_CREAT|O_EXCL)` is EEXIST
    ///    while `open(".", O_CREAT)` is EISDIR on Linux.
    ///  * A *free* name that demands to be a directory creates nothing and is
    ///    ENOENT. Only Darwin reaches this: Linux refuses such a path inside the
    ///    walk, via `CreatingOpenRules.TrailingSeparator`.
    ///  * A path that consumed no component at all — "/" — is whatever
    ///    `RootNavigation` says, which is Darwin's EEXIST.
    ///  * A creating open landing on an existing directory is EISDIR on Linux.
    ///  * Binding a name needs the *write* bit on the directory that will hold
    ///    it: measured at uid 1000, 0o333 and 0o300 succeed while 0o644 and
    ///    0o555 are EACCES. Root bypasses it.
    ///
    ///    Binding needs the directory's *search* bit too — 0o111 is EACCES on
    ///    both kernels — but that half is not checked here: no resolution can
    ///    reach this function without it, because the walk refuses an
    ///    unsearchable directory before it looks a component up at all. See
    ///    `VirtualFileSystem.resolveFull`, which is also where the rows that
    ///    pin it live.
    ///
    /// A freshly created inode is deliberately *not* screened against the mode
    /// it was just given — measured unanimously, `open(free, O_CREAT|O_RDWR, 0)`
    /// succeeds and stores mode 0, while re-opening that same file `O_RDONLY` is
    /// EACCES. That is why `Create` is a distinct verdict from `OpenExisting`
    /// rather than a step before it.
    let verdict
        (rules : CreatingOpenRules)
        (privilege : CallerPrivilege)
        (creating : bool)
        (exclusive : bool)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : CreatingOpenVerdict
        =
        let existing = VirtualFileSystem.existingOf resolution.Target |> Result.toOption

        if not creating then
            match existing with
            | Some inode -> CreatingOpenVerdict.OpenExisting inode
            | None -> CreatingOpenVerdict.Refuse UnixError.ENOENT
        elif exclusive && existing.IsSome then
            CreatingOpenVerdict.Refuse UnixError.EEXIST
        else

        let isDirectory (inode : InodeNumber) : bool =
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory _) -> true
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _)
            | None -> false

        match resolution.Target with
        | ResolvedTarget.Entry (_, _, None) when resolution.TrailingSeparatorDemanded ->
            CreatingOpenVerdict.Refuse UnixError.ENOENT
        | ResolvedTarget.Directory (_, FinalNavigation.Root) when rules.RootNavigation.IsSome ->
            CreatingOpenVerdict.Refuse rules.RootNavigation.Value
        | ResolvedTarget.Directory (inode, _) ->
            if rules.RefusesExistingDirectory then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
            else
                CreatingOpenVerdict.OpenExisting inode
        | ResolvedTarget.Entry (_, _, Some inode) ->
            if rules.RefusesExistingDirectory && isDirectory inode then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
            else
                CreatingOpenVerdict.OpenExisting inode
        | ResolvedTarget.Entry (directory, name, None) ->

        // Nothing can be created inside a directory whose own last name has
        // gone: measured on both, `open("x", O_CREAT)` from inside an orphan is
        // ENOENT, at 0o755 and at 0o555 alike, so this beats the EACCES below.
        // `MkDirRules.verdict` states the same rule for the other creating
        // syscall.
        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            CreatingOpenVerdict.Refuse UnixError.ENOENT
        else

        // Write alone: the search half of the rule is the walk's, and a
        // resolution that reached here has already passed it. Only the owner
        // triple can ever apply, since `stat` reports `Kernel.UserId` as every
        // inode's `st_uid`.
        let bindBits = 0o200

        let parentBits =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match VirtualFileSystem.permissions parent with
                | InodePermissions.Stored bits -> PermissionBits.toInt bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"CreatingOpenRules.verdict: the walk resolved \"%s{FileName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"CreatingOpenRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{FileName.toString name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        let lacksBindBits =
            match privilege with
            | CallerPrivilege.Privileged -> false
            | CallerPrivilege.Unprivileged -> parentBits &&& bindBits <> bindBits

        if lacksBindBits then
            CreatingOpenVerdict.Refuse UnixError.EACCES
        else
            CreatingOpenVerdict.Create (directory, name)

    /// The permission bits a file created with this `mode` argument ends up
    /// with, under `umask`. See `PermissionBits.fromCreationMode`, which states
    /// the rule once for every creating syscall; `ModeMask` is `open`'s half of
    /// it, and is how a Darwin guest cannot create a setuid file at all.
    let createdPermissions (rules : CreatingOpenRules) (umask : PermissionBits) (mode : int) : PermissionBits =
        PermissionBits.fromCreationMode rules.ModeMask umask mode

/// Everything a kernel does differently when `mkdir(2)` creates a directory.
///
/// Deliberately not folded into `CreatingOpenRules`, even though two fields
/// share a name with one of its: the values differ, so a shared record would
/// have to be right for both syscalls at once and is right for neither.
/// Measured at `umask 022` on macOS 25.6/APFS at uid 501 and Linux 6.x arm64 at
/// uid 1000, fresh tree per row.
type MkDirRules =
    {
        /// The walk `mkdir` resolves its path with. Linux's last component is a
        /// plain dentry lookup (`filename_create`), so a trailing separator buys
        /// nothing there and every existing final name is EEXIST; Darwin
        /// resolves it as a lookup would, which is how `mkdir("dang/")` creates
        /// the dangling link's *target* on that platform and answers ENOTDIR for
        /// "f/" and ELOOP for "cyc/".
        ///
        /// This field is why `MkDirRules.verdict` needs no rules: the divergence
        /// is spent inside the walk, and what comes out the other side is
        /// decided identically on both platforms.
        TrailingSeparator : TrailingSeparatorPolicy
        /// The bits `mkdir(2)` keeps from its `mode` argument before the umask
        /// is applied — which is *not* `CreatingOpenRules.ModeMask`. Linux keeps
        /// the sticky bit and drops both set-ID bits (`vfs_mkdir` masks with
        /// `S_IRWXUGO|S_ISVTX`), where its `open` keeps all twelve: measured,
        /// `mkdir(p, 0o7777)` gives 0o1755 and `mkdir(p, 0o2777)` gives 0o755.
        /// Darwin drops all three, as its `open` does.
        ModeMask : PermissionBits
        /// Whether a new directory inherits `S_ISGID` from the directory that
        /// holds it. Measured with a parent `chmod`ed to 0o2777 and read back at
        /// 0o2777 first: Linux gives the child 0o2755 from mode 0o777 and
        /// 0o3755 from 0o7777, so the bit is OR-ed in *after* both masks;
        /// Darwin gives 0o755 from every mode and does not inherit it at all.
        ///
        /// A kernel fact rather than a mount one on Linux — `inode_init_owner`
        /// (fs/inode.c) is VFS-generic, and a directory inherits the bit
        /// unconditionally when its parent carries it. The group-membership
        /// proviso beside it in that function applies only to non-directories,
        /// which is source-derived rather than measured, and is unobservable
        /// here anyway: PawPrint has one process-wide gid, so a new inode's
        /// group always matches its parent's. (`mount -o grpid` varies *gid*
        /// inheritance, not the bit, and one gid cannot see that either.)
        InheritsSetGroupIdFromParent : bool
    }

/// What `mkdir(2)` should do next, once its path has been resolved.
///
/// The same shape as `CreatingOpenVerdict`, less `OpenExisting`: `mkdir` has no
/// success that is not a creation.
[<RequireQualifiedAccess>]
type MkDirVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty directory under `name` in `directory`, whose own
    /// permission bits are `parentPermissions` — carried out of the verdict
    /// because it read them to decide, and because `S_ISGID` inheritance needs
    /// them again.
    | Create of directory : InodeNumber * name : FileName * parentPermissions : PermissionBits

[<RequireQualifiedAccess>]
module MkDirRules =
    /// Decide what a `mkdir(2)` owes, given how its path resolved.
    ///
    /// Takes no `MkDirRules`, and that is the point: every rule below is
    /// measured *identical* on both platforms. Everything `mkdir` diverges about
    /// is spent earlier, in the walk `MkDirRules.TrailingSeparator` selects, or
    /// later, in `createdPermissions`. Contrast `CreatingOpenRules.verdict`,
    /// which genuinely reads two of its fields.
    ///
    /// The order of the refusals is measured, and each beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", ".", ".." — is
    ///    EEXIST, whichever `FinalNavigation` it was. `mkdir` does not
    ///    distinguish them, where `rmdir` does: EBUSY, EINVAL and ENOTEMPTY on
    ///    Linux, and on Darwin EISDIR for the first with EBUSY swallowing the
    ///    other two at the root.
    ///  * An existing final name is EEXIST: a file, a directory, or a symlink,
    ///    dangling or cyclic or not.
    ///  * EEXIST beats the *write* bit. Measured on both: an existing child of a
    ///    0o555 directory is EEXIST, where a free name there is EACCES.
    ///  * Binding a new name needs write on the directory that will hold it:
    ///    measured, 0o333 and 0o300 succeed while 0o555 and 0o644 are EACCES.
    ///    Root bypasses it.
    ///
    /// The holding directory's *search* bit is needed as well — and needed
    /// earlier, since without it the final name cannot be looked up at all, so
    /// its absence beats even EEXIST. That check is the walk's
    /// (`VirtualFileSystem.resolveFull`), which refuses before this function is
    /// reached; the rows that pin it live there.
    ///
    /// A *free* final name carrying a trailing separator creates, on both
    /// platforms — `mkdir("nx/")` succeeds. This is the one place `mkdir` and a
    /// creating `open` disagree about a resolution of the same shape: `open`
    /// owes it ENOENT on Darwin.
    ///
    let verdict (privilege : CallerPrivilege) (resolution : Resolution) (vfs : VirtualFileSystem) : MkDirVerdict =
        match resolution.Target with
        | ResolvedTarget.Directory _ -> MkDirVerdict.Refuse UnixError.EEXIST
        | ResolvedTarget.Entry (directory, name, existing) ->

        // Nothing can be created inside a directory whose own last name has
        // gone. Measured on both, at 0o755 and at 0o555, so ENOENT beats the
        // EACCES below; and `mkdir(".")` inside an orphan is still EEXIST, which
        // is why this sits under the `Directory` arm rather than above it.
        //
        // Above the `existing` match because that is where a real kernel puts
        // it: the ENOENT comes from the lookup itself failing against a dead
        // parent. The ordering is not observable — an orphan is necessarily
        // empty, since `rmdir` refuses a populated directory and this rule stops
        // one ever gaining an entry.
        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            MkDirVerdict.Refuse UnixError.ENOENT
        else

        // Write alone: the search half of the rule is the walk's, and a
        // resolution that reached here has already passed it. Only the owner
        // triple can ever apply, since `stat` reports `Kernel.UserId` as every
        // inode's `st_uid`.
        let write = 0o200

        let parentPermissions =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match VirtualFileSystem.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"MkDirRules.verdict: the walk resolved \"%s{FileName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"MkDirRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{FileName.toString name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        let lacks (bit : int) : bool =
            match privilege with
            | CallerPrivilege.Privileged -> false
            | CallerPrivilege.Unprivileged -> PermissionBits.toInt parentPermissions &&& bit <> bit

        match existing with
        | Some _ -> MkDirVerdict.Refuse UnixError.EEXIST
        | None ->

        if lacks write then
            MkDirVerdict.Refuse UnixError.EACCES
        else
            MkDirVerdict.Create (directory, name, parentPermissions)

    /// The permission bits a directory created with this `mode` argument ends up
    /// with, inside a parent whose own bits are `parentPermissions`.
    ///
    /// `PermissionBits.fromCreationMode` under `MkDirRules.ModeMask`, then
    /// `S_ISGID` OR-ed in where the platform inherits it. The OR is last, and
    /// measured to be: Linux's `mkdir(sg, 0o7777)` in a 0o2777 parent gives
    /// 0o3755, so the bit survives a mask that would otherwise have cleared it.
    let createdPermissions
        (rules : MkDirRules)
        (parentPermissions : PermissionBits)
        (umask : PermissionBits)
        (mode : int)
        : PermissionBits
        =
        let setGroupId = 0o2000
        let masked = PermissionBits.fromCreationMode rules.ModeMask umask mode

        let inherited =
            rules.InheritsSetGroupIdFromParent
            && PermissionBits.toInt parentPermissions &&& setGroupId <> 0

        if inherited then
            PermissionBits.toInt masked ||| setGroupId
            |> PermissionBits.parseOrFail "MkDirRules.createdPermissions"
        else
            masked

/// Everything a kernel does differently when `unlink(2)` removes a name.
///
/// One field, and that is the whole record: unlike `mkdir`, whose divergence is
/// spent entirely inside the walk, `unlink` diverges in the *order and
/// vocabulary* of its refusals as well, and those live in
/// `UnlinkRules.linuxVerdict` and `UnlinkRules.darwinVerdict` rather than in
/// fields here. See `UnlinkRules.verdict` for why there are two functions
/// rather than a table.
///
/// Measured on macOS 26.6/APFS at uid 501 and 0, and Linux 6.x arm64 at uid
/// 1000 and 0, one fresh tree per row.
type UnlinkRules =
    {
        /// The walk `unlink` resolves its path with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms.
        ///
        /// Linux's `do_unlinkat` takes a parent and a name and never resolves
        /// the final component at all, so a trailing separator neither
        /// dereferences a final symlink nor is enforced by the walk: it is
        /// reported on `Resolution.TrailingSeparatorDemanded` and enforced by
        /// `linuxVerdict`. Darwin's `namei` resolves it like any other lookup,
        /// which is `Demand`.
        ///
        /// The row that separates them is `unlink("lroot/")` with `lroot -> "/"`:
        /// ENOTDIR on Linux, which cannot have traversed the link, against
        /// EISDIR on Darwin, which did.
        TrailingSeparator : TrailingSeparatorPolicy
    }

/// What `unlink(2)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type UnlinkVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Remove `name` from `directory`, and — if that was the last name the
    /// inode had and no open file description holds it — free the inode.
    ///
    /// Carries no inode, though the verdict read one to decide. The removing
    /// code gets it from `VirtualFileSystem.unbind`, which answers the inode it
    /// actually unbound — so there is one source for "which inode lost a name",
    /// and it is the one the removal performed rather than the one a lookup saw
    /// beforehand.
    | Remove of directory : InodeNumber * name : FileName

/// The two questions `unlink(2)` and `rmdir(2)` both ask about a name they have
/// been asked to remove. Neither is a policy: which of them is asked first, and
/// what a "yes" costs, is each syscall's own measured business.
[<RequireQualifiedAccess>]
module private RemovalChecks =
    /// Whether the *holding* directory refuses this caller the write bit it
    /// needs to remove a name from it.
    ///
    /// Write alone: the search half is the walk's, and a resolution that got
    /// this far has passed it. Only the owner triple can ever apply, since
    /// `stat` reports `Kernel.UserId` as every inode's `st_uid`, and the sticky
    /// bit can never refuse for the same reason — POSIX permits the removal when
    /// the caller owns the file *or* the directory, and one kernel-wide identity
    /// owns both.
    ///
    /// Partial in `directory`, which the walk has just reported as the directory
    /// holding `name`.
    let lacksWrite
        (privilege : CallerPrivilege)
        (directory : InodeNumber)
        (name : FileName)
        (vfs : VirtualFileSystem)
        : bool
        =
        match privilege with
        | CallerPrivilege.Privileged -> false
        | CallerPrivilege.Unprivileged ->

        let permissions =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match VirtualFileSystem.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"RemovalChecks.lacksWrite: the walk resolved \"%s{FileName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"RemovalChecks.lacksWrite: resolution named inode %O{directory} as the directory holding \"%s{FileName.toString name}\", but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        PermissionBits.toInt permissions &&& 0o200 <> 0o200

    /// Whether the inode a name is bound to is a directory. Partial in the same
    /// way `lacksWrite` is: the walk has just reported this inode.
    let isDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory _) -> true
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) -> false
        | None ->
            failwith
                $"RemovalChecks.isDirectory: the walk resolved a name to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// Whether the directory at `inode` still holds an entry, which is what
    /// `rmdir(2)` answers ENOTEMPTY for. "." and ".." do not count: they are
    /// derived rather than stored (see `DirectoryContent.Entries`), and a real
    /// `rmdir` does not count them either.
    ///
    /// Partial in the same way the two above are, and additionally in the inode
    /// being a directory: the caller has just asked `isDirectory`.
    let isEmptyDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Map.isEmpty directory.Entries
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) ->
            failwith
                $"RemovalChecks.isEmptyDirectory: inode %O{inode} is not a directory, so it has no entries to count. Ask isDirectory first (this is an interpreter bug)."
        | None ->
            failwith
                $"RemovalChecks.isEmptyDirectory: the walk resolved a name to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

[<RequireQualifiedAccess>]
module UnlinkRules =
    /// Linux's `unlink(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path that consumed no component — "/", ".", "..", and any symlink
    ///    expansion of them — is EISDIR, whichever `FinalNavigation` it was and
    ///    whether or not the directory it reached is the root. Linux spends no
    ///    errno distinguishing them, where `rmdir` gives each its own (EBUSY,
    ///    EINVAL and ENOTEMPTY).
    ///  * A free final name is ENOENT, and that beats every check below:
    ///    `unlink("nowrite/nx/")` is ENOENT rather than the ENOTDIR the trailing
    ///    separator would earn or the EACCES the parent would.
    ///  * A trailing separator demands a directory, and reports what it found:
    ///    EISDIR for a directory, ENOTDIR for anything else. This is the arm
    ///    Linux's walk declines to make (`TrailingSeparatorPolicy.Ignore`), so
    ///    it never traverses a final symlink to get here — `unlink("ld/")`,
    ///    `unlink("dang/")`, `unlink("cyc/")` and `unlink("lroot/")` are all
    ///    ENOTDIR, with no ELOOP and no chance of destroying a link's target.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * The target being a directory is EISDIR — *below* the write check, and
    ///    measured to be: `unlink("nowrite/kdir")` is EACCES where
    ///    `unlink("nowrite/kdir/")` is EISDIR. That pair is the only thing
    ///    separating this arm from the trailing-separator one, since they share
    ///    an errno.
    ///
    /// EISDIR here is privilege-independent: measured at uid 0, Linux still
    /// refuses to `unlink` a directory. `CallerPrivilege` gates the write bit
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory _ -> UnlinkVerdict.Refuse UnixError.EISDIR
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> UnlinkVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if resolution.TrailingSeparatorDemanded then
            if RemovalChecks.isDirectory target vfs then
                UnlinkVerdict.Refuse UnixError.EISDIR
            else
                UnlinkVerdict.Refuse UnixError.ENOTDIR
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
        elif RemovalChecks.isDirectory target vfs then
            UnlinkVerdict.Refuse UnixError.EISDIR
        else
            UnlinkVerdict.Remove (directory, name)

    /// Darwin's `unlink(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", or a symlink whose
    ///    target was "/" — is EISDIR.
    ///  * The root reached by "." or ".." is EBUSY, which is XNU's `unlink1`
    ///    refusing a mount's root vnode (`vp->v_flag & VROOT`). PawPrint mounts
    ///    one filesystem, so "the root of a mount" and "the root" are the same
    ///    inode. Measured: `unlink("/.")`, `unlink("/..")` and — through
    ///    `lroot -> "/"` — `unlink("lroot/.")` are EBUSY, where `unlink("d/.")`
    ///    on an ordinary directory is EPERM.
    ///  * Any other directory reached with no final name is EPERM.
    ///  * A free final name is ENOENT.
    ///  * The target being a directory is EPERM, and beats the write check:
    ///    `unlink("nowrite/kdir")` is EPERM where `unlink("nowrite/kid")` is
    ///    EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///
    /// EPERM is privilege-independent — measured at uid 0, where `unlink("d")`
    /// is still EPERM and `rmdir("d")` succeeds. The `unlink(2)` man page's "and
    /// the effective user ID of the process is not the super-user" is stale
    /// relative to modern XNU, which refuses unconditionally.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so this function never
    /// sees `TrailingSeparatorDemanded` against a non-directory: the walk has
    /// already answered ENOTDIR (`unlink("f/")`, `unlink("lf/")`), ELOOP
    /// (`unlink("cyc/")`) or ENOENT (`unlink("dang/")`). What does reach here is
    /// a separator over a *directory*, whether named directly (`unlink("d/")`)
    /// or reached by following a final symlink (`unlink("ld/")`) — both EPERM,
    /// from the arm below, which is why the destructive divergence
    /// `Resolution.FinalSymlinkFollowed` warns about costs `unlink` nothing.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> UnlinkVerdict.Refuse UnixError.EISDIR
            | FinalNavigation.Current
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    UnlinkVerdict.Refuse UnixError.EBUSY
                else
                    UnlinkVerdict.Refuse UnixError.EPERM
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> UnlinkVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if RemovalChecks.isDirectory target vfs then
            UnlinkVerdict.Refuse UnixError.EPERM
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
        else
            UnlinkVerdict.Remove (directory, name)

    /// Decide what an `unlink(2)` owes, given how its path resolved.
    ///
    /// Two whole functions rather than one reading a rules record, against the
    /// `MkDirRules.verdict` precedent, because what diverges here is the *order*
    /// of the checks and the errno vocabulary rather than a constant they both
    /// consult. A record spelling that as `{ DirectoryErrno; RootNavigationErrno;
    /// TypeCheckPrecedesPermission : bool }` would make most of its inhabitants
    /// describe a kernel nobody ships, and a boolean that reorders control flow
    /// is exactly the illegal-state-representable shape this codebase avoids.
    /// Each function above instead reads top-to-bottom against its own measured
    /// column.
    ///
    /// The same argument rules out `SimulatedUnixPlatform.bindFaultOrder`'s
    /// shape — compute the fault set, then pick the first by a per-flavour
    /// order — which works there because both flavours agree on the faults and
    /// on the errno each carries. Here they agree on neither.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege resolution vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege resolution vfs

/// What the PAL puts in `DirectoryEntry.NameLength`, which is a fact about the
/// libc it was compiled against rather than about any directory.
///
/// `ConvertDirent` (`pal_io.c:497`) copies `d_namlen` under
/// `HAVE_DIRENT_NAME_LEN` and writes `-1` otherwise, the sentinel meaning "walk
/// to the NUL yourself". Established by compiling rather than by reading:
/// glibc's `struct dirent` has no `d_namlen` member at all (`gcc` rejects
/// `d.d_namlen`), while macOS's `sys/dirent.h` declares one.
///
/// Invisible to managed code — `DirectoryEntry.GetName` takes
/// `CreateReadOnlySpanFromNullTerminated` for the sentinel and a plain span
/// otherwise — so only a guest that hand-rolls the P/Invoke can tell.
[<RequireQualifiedAccess>]
type DirectoryEntryNameLength =
    /// The name's length in bytes, as macOS reports it.
    | Reported
    /// `-1`, as every libc without `d_namlen` gets.
    | WalkToTerminator

/// What `getcwd(3)` answers when the current directory has been *removed* — so
/// there is no path to report — and how small a buffer can still change that
/// answer.
///
/// Only reachable since `rmdir` could orphan a current directory. Measured on
/// both with the cwd removed out from under the process, sweeping the size from
/// 1 past the length of the path that used to be there: a zero-length buffer is
/// EINVAL everywhere (the shim's own guard, before `getcwd` is called at all),
/// and everything else splits on the *first byte* only.
[<RequireQualifiedAccess>]
type GetCwdOrphanAnswer =
    /// ENOENT whatever the size. Linux's `sys_getcwd` builds the path, fails
    /// because it is disconnected, and never reaches the length comparison —
    /// measured ENOENT at every size from 1 up.
    | AlwaysDetached
    /// ENOENT unless the buffer cannot hold even `"/"` and a terminator, which
    /// is ERANGE. Darwin's `getcwd(3)` builds the path from the root downwards,
    /// so it needs those two bytes before it can start; measured, size 1 is
    /// ERANGE and *every* larger size is ENOENT — including sizes far below the
    /// length of the path that used to be there. It is a minimum, not a
    /// comparison against a path that no longer exists.
    ///
    /// **This flavour's failing `getcwd` scribbles on the caller's buffer, and
    /// this library does not reproduce what it leaves.** `GetCwdAnswer.Failed`
    /// carries an errno and says nothing about the destination's contents; the
    /// errno itself is exact. Measured by sweeping the capacity with the
    /// destination prefilled `0xAA` and reporting every byte that changed:
    ///
    /// * orphaned, capacity 1: nothing written, ERANGE;
    /// * orphaned, 2 ≤ capacity < PATH_MAX: a NUL at the buffer's *last* byte;
    /// * orphaned, capacity ≥ PATH_MAX: that NUL, and the stale path at offset
    ///   0 as well;
    /// * intact but the path does not fit: a *suffix* of the path, filled
    ///   backwards from the last byte — 976 bytes at offsets 48..1023 for a
    ///   1418-byte path in a 1024-byte buffer — and ERANGE.
    ///
    /// That last shape is BSD `getcwd(3)` assembling the path backwards from
    /// the end of the buffer and moving it to the front once it fits, so the
    /// residue is a function of libc's internal progress rather than of
    /// anything a kernel decides. Reproducing it faithfully means reproducing
    /// that algorithm, including which of its paths a given capacity takes;
    /// reproducing it approximately means inventing bytes a guest can read. No
    /// caller in the BCL reads the destination after a NULL return, so this
    /// library reports the errno and leaves the buffer alone — recorded in
    /// `docs/divergences.md` rather than left to be discovered.
    ///
    /// Linux writes nothing on any failure path at any capacity, which is why
    /// only this case needs the note.
    | ShortestPathFirst

/// What an unwritable destination does to a `getcwd(3)` that has got as far as
/// storing into it — which is a question about *where the bytes are copied*,
/// and so splits by flavour rather than by kernel behaviour.
///
/// Measured with a destination that is mapped `PROT_READ` only, which
/// discriminates the two mechanisms where an unmapped address cannot: a kernel
/// copying with `copy_to_user` reports EFAULT, while a store executed in user
/// space takes a fatal signal. `readlink(2)` answers EFAULT on both platforms
/// in the same probe, so this is `getcwd`'s own property and not a general one.
[<RequireQualifiedAccess>]
type GetCwdDestinationFault =
    /// EFAULT, the destination untouched. Linux's `getcwd` is a syscall whose
    /// `copy_to_user` reports a bad destination as an ordinary error.
    | ReportedAsEfault
    /// A fatal signal — SIGSEGV for an unmapped destination, SIGBUS for a
    /// read-only one. Darwin's `getcwd(3)` assembles the path with stores
    /// executed in the caller's own context, so a destination it cannot write
    /// kills the process instead of producing an errno.
    ///
    /// A kernel cannot answer this, and neither can this library: see
    /// `GetCwdRefusal.FatalToTheProcess` for what it says instead.
    | FatalToTheProcess

/// What a `getsockname(2)` that faults copying the address out has already put
/// in the caller's length cell.
///
/// The two kernels order the two stores differently, so a call that fails
/// leaves the caller's `socklen_t` reading different things. Measured against a
/// wholly unmapped destination and against one writable for its first few bytes
/// only, with sentinel lengths of 7, 13, 100 and 4096 so that a value that came
/// back changed can only have been written: on Linux 6.18.5 every one of them
/// reads 16 afterwards, and on macOS 26.6 every one still reads what it went in
/// with. A descriptor that fails earlier -- EBADF, ENOTSOCK -- touches the cell
/// on neither, so this is the fault path's property rather than the failure
/// path's in general.
///
/// Whether a *client* can see this is a separate question, and for the .NET PAL
/// the answer is no: `SystemNative_GetSockName` copies the caller's length into
/// a local `socklen_t`, passes that, and writes it back only when the call
/// succeeded, so the kernel's store lands on the shim's stack. A client speaking
/// raw POSIX does see it.
[<RequireQualifiedAccess>]
type GetSockNameFaultLength =
    /// The cell still holds what the caller put there. Darwin copies the address
    /// out first and reports the length only once that has succeeded.
    | Untouched
    /// The cell holds the address's *untruncated* length -- what a successful
    /// call would have reported -- because the kernel stored that before
    /// attempting the copy that then faulted.
    | AlreadyReported

/// Everything a kernel does differently when `rmdir(2)` removes a directory.
///
/// Two fields, and the rest of the divergence — the *order* of the refusals and
/// the errno vocabulary — lives in `RmDirRules.linuxVerdict` and
/// `RmDirRules.darwinVerdict` rather than here, for the reason
/// `UnlinkRules.verdict` gives.
///
/// Measured on macOS 26.6/APFS at uid 501, and Linux 6.x arm64 at uid 1000 and
/// uid 0, one fresh tree per row.
type RmDirRules =
    {
        /// The walk `rmdir` resolves its path with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms. Linux `Ignore`,
        /// Darwin `Demand`, exactly as `unlink`'s is and for the same reason.
        ///
        /// This is the field that makes the two flavours **destroy different
        /// objects**. With `ld -> d` and `d` an empty directory, `rmdir("ld/")`
        /// is ENOTDIR on Linux — whose walk cannot have traversed the link — and
        /// *removes `d`* on Darwin, whose walk did. It is the divergence
        /// `Resolution.FinalSymlinkFollowed` warns about, and the reason this
        /// syscall dispatches on the flavour rather than picking a column.
        TrailingSeparator : TrailingSeparatorPolicy
        /// What removing the directory does to the removed directory's own
        /// inode, which the flavours do not agree on.
        ///
        /// Measured through a descriptor held across the call, reproduced 3/3 on
        /// each: Linux drops the directory's `st_nlink` from 2 to 0 and moves its
        /// `ctime`, while Darwin leaves both alone. It is one fact, not two —
        /// nothing about the Darwin inode changed, so its `ctime` has no reason
        /// to move.
        ///
        /// Guest-observable, which is why it is modelled rather than approximated:
        /// `SystemNative_FStat` on a directory descriptor writes
        /// `InodeTimes.StatusChange` into `FileStatus`. (`st_nlink` itself is not
        /// a `FileStatus` field, so only its shadow on `ctime` can be read.)
        ///
        /// `unlink` needs no such field: removing a *file*'s last name moves its
        /// `ctime` on both.
        RemovedDirectoryEffect : UnbindTargetEffect
    }

/// What `opendir(3)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type OpenDirVerdict =
    /// Answer the guest with this errno, and a NULL `DIR*`.
    | Refuse of error : UnixError
    /// Open a stream over this directory.
    | Open of directory : InodeNumber

[<RequireQualifiedAccess>]
module OpenDirRules =
    /// `opendir(3)`, transcribed from the measured ordering. Each arm beats the
    /// ones below it, and each bullet is a row measured on **both** kernels —
    /// there is no flavour parameter because there is no row they disagree on,
    /// which is why this takes none rather than defaulting one:
    ///
    ///  * A name nothing binds is ENOENT, and so is a dangling symlink: the walk
    ///    follows the final link, so there is nothing left to open.
    ///  * A target that is not a directory is ENOTDIR, and that beats the
    ///    permission check. The row proving it is a **mode-0000 regular file**,
    ///    which is ENOTDIR rather than EACCES — with and without a trailing
    ///    separator, and through a symlink to one. Pleasingly symmetric with
    ///    `open`'s own measured "EISDIR beats EACCES".
    ///  * A directory that refuses this caller the **read** bit is EACCES. Read,
    ///    not search, and this is the first place in this codebase where the two
    ///    come apart: a `0o111` directory (search, no read) is EACCES, while a
    ///    `0o444` one (read, no search) opens and lists every name. Search on the
    ///    *ancestors* is the walk's business and a resolution that got here has
    ///    passed it.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `opendir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// There is no root-navigation arm either, and `rmdir`'s three are the
    /// reason to say so rather than leave it implied: `opendir("/")`,
    /// `opendir("d/.")` and `opendir("d/..")` all simply succeed, on both.
    let verdict (privilege : CallerPrivilege) (resolution : Resolution) (vfs : VirtualFileSystem) : OpenDirVerdict =
        match VirtualFileSystem.existingOf resolution.Target with
        | Error error -> OpenDirVerdict.Refuse error
        | Ok inode ->

        match VirtualFileSystem.tryGetContent inode vfs with
        | None ->
            failwith
                $"OpenDirRules.verdict: the walk resolved to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) ->
            // The symlink arm is unreachable through the resolver, which
            // followed every final link and answered ENOENT for a dangling one.
            // It is the same answer either way, so there is nothing to refuse.
            OpenDirVerdict.Refuse UnixError.ENOTDIR
        | Some (InodeContent.Directory content) ->

        if PermissionBits.deniedTo privilege 0o400 content.Permissions then
            OpenDirVerdict.Refuse UnixError.EACCES
        else
            OpenDirVerdict.Open inode

/// What `rmdir(2)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type RmDirVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Remove `name` from `directory`, and — since no other name can point at a
    /// directory — free the inode unless a descriptor or the current directory
    /// still holds it.
    ///
    /// Carries no inode for the reason `UnlinkVerdict.Remove` carries none: the
    /// removing code gets it from `VirtualFileSystem.unbind`, which answers the
    /// inode it actually unbound.
    | Remove of directory : InodeNumber * name : FileName

[<RequireQualifiedAccess>]
module RmDirRules =
    /// Linux's `rmdir(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path that consumed no component at all — "/" — is EBUSY. Linux
    ///    specialises the *path*, not the inode: `rmdir("/")` is EBUSY where
    ///    `rmdir("/.")` is EINVAL.
    ///  * A path whose last component was "." is EINVAL, whatever directory it
    ///    reached: `rmdir(".")`, `rmdir("d/.")` and `rmdir("/.")` all are.
    ///  * A path whose last component was ".." is ENOTEMPTY, again whatever it
    ///    reached. Not a coincidence with the emptiness check below — the parent
    ///    of any directory necessarily contains that directory — but it *is* a
    ///    separate arm, and the row proving it is `rmdir("nowrite/kdir/..")`,
    ///    which is ENOTEMPTY where the write check below would say EACCES.
    ///  * A free final name is ENOENT, and that beats the write check:
    ///    `rmdir("nowrite/nx")` is ENOENT.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * The target not being a directory is ENOTDIR — *below* the write check,
    ///    and measured to be: `rmdir("nowrite/kid")` is EACCES at uid 1000 and
    ///    ENOTDIR at uid 0. This is the arm Darwin orders the other way round.
    ///  * A directory that still holds an entry is ENOTEMPTY.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `rmdir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// Measured at uid 0, every row: the EACCES rows fall through to their next
    /// check and nothing else moves, so `CallerPrivilege` gates the write bit
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RmDirVerdict.Refuse UnixError.EBUSY
            | FinalNavigation.Current -> RmDirVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Parent -> RmDirVerdict.Refuse UnixError.ENOTEMPTY
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> RmDirVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if RemovalChecks.lacksWrite privilege directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
        elif not (RemovalChecks.isEmptyDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTEMPTY
        else
            RmDirVerdict.Remove (directory, name)

    /// Darwin's `rmdir(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", or a symlink whose
    ///    target was "/" — is EISDIR. Where Linux gives that path EBUSY.
    ///  * The root reached by "." or ".." is EBUSY, which is XNU refusing a
    ///    mount's root vnode; PawPrint mounts one filesystem, so "the root of a
    ///    mount" and "the root" are the same inode. Measured: `rmdir("/.")`,
    ///    `rmdir("/..")` and — through `lroot -> "/"` — `rmdir("lroot/.")` are
    ///    EBUSY, where Linux answers those EINVAL and ENOTEMPTY. So Darwin
    ///    specialises the *inode* where Linux specialises the path.
    ///  * Any other directory reached by "." is EINVAL, and by ".." is
    ///    ENOTEMPTY — agreeing with Linux once the root is out of the way.
    ///  * A free final name is ENOENT.
    ///  * The target not being a directory is ENOTDIR, and beats the write
    ///    check: `rmdir("nowrite/kid")` is ENOTDIR where `rmdir("nowrite/kdir")`
    ///    is EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * A directory that still holds an entry is ENOTEMPTY, and the write
    ///    check beats it: `rmdir("nowrite/kfull")` is EACCES.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so a separator over a
    /// non-directory never reaches here — the walk has already answered ENOTDIR
    /// (`rmdir("f/")`, `rmdir("lf/")`), ELOOP (`rmdir("cyc/")`) or ENOENT
    /// (`rmdir("dang/")`). What does reach here is a separator over a directory
    /// a final symlink named, and that is the destructive row: `rmdir("ld/")`
    /// removes `d`.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RmDirVerdict.Refuse UnixError.EISDIR
            | FinalNavigation.Current ->
                if inode = VirtualFileSystem.root vfs then
                    RmDirVerdict.Refuse UnixError.EBUSY
                else
                    RmDirVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    RmDirVerdict.Refuse UnixError.EBUSY
                else
                    RmDirVerdict.Refuse UnixError.ENOTEMPTY
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> RmDirVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif not (RemovalChecks.isEmptyDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTEMPTY
        else
            RmDirVerdict.Remove (directory, name)

    /// Decide what an `rmdir(2)` owes, given how its path resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rmdir` makes the case more strongly than `unlink` did — the two flavours
    /// disagree about which of the root and the *path to it* is the special
    /// thing, which no table of errnos can express.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege resolution vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege resolution vfs

/// The order in which `rename(2)` resolves its two paths, which is
/// guest-visible because the two kernels answer different errnos when both
/// paths are bad.
///
/// Measured with pairs that *disagree* — a pair answering one errno either way
/// proves nothing. `rename(absent, "regularfile/x")` is ENOTDIR on Linux and
/// ENOENT on Darwin; `rename("<300 bytes>", "nodir/x")` is ENOENT on Linux and
/// ENAMETOOLONG on Darwin.
///
/// Not derivable from `RenameRules.TrailingSeparator`, and not something the
/// verdict could express: it decides which resolutions are *performed at all*,
/// before there are two `Resolution`s to judge.
[<RequireQualifiedAccess>]
type RenameWalkOrder =
    /// Both pathnames copied in, then both parents, then both final lookups:
    /// the shape of Linux's `do_renameat2`, which calls `getname` twice and
    /// then `filename_parentat` twice before either final component is looked
    /// up. Every refusal either path earns after that is the verdict's, judged
    /// against both.
    ///
    /// The source's parent is resolved before the destination's, which
    /// `rename("nodir/kid", "f/x")` pins: it is ENOENT, where the destination's
    /// parent alone would answer ENOTDIR.
    ///
    /// Everything about the source's *final component* loses to the
    /// destination's parent — measured across a free name, "/", ".", "..", a
    /// trailing separator and a 300-byte name, all of which answer the
    /// destination's ENOTDIR.
    | ParentsThenFinals
    /// The source finished before the destination is looked at at all — its
    /// pathname included, and including the two refusals Darwin's source-side
    /// `namei` makes for itself under rename semantics. See
    /// `RenameRules.sourceScreen`.
    | SourceThenDestination

/// Everything a kernel does differently when `rename(2)` moves a name.
///
/// Two fields, and the rest of the divergence — the *order* of the refusals and
/// the errno vocabulary — lives in `RenameRules.linuxVerdict` and
/// `RenameRules.darwinVerdict`, for the reason `UnlinkRules.verdict` gives.
/// `rename` diverges more than any operation before it: the two flavours
/// disagree about where the permission checks sit, about *which* directory's
/// write bit a directory-over-directory rename even consults, and about where
/// the no-op sits.
///
/// Measured on macOS 26.6/APFS at uid 501 and Linux 6.x arm64 at uid 1000 and
/// uid 0, one fresh tree per row; `docs/probes/rename/` holds the probes.
type RenameRules =
    {
        /// The walk `rename` resolves *both* of its paths with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms. Linux `Ignore`,
        /// Darwin `Demand`, exactly as `unlink`'s and `rmdir`'s are.
        ///
        /// One field for two paths because, measured, each kernel resolves its
        /// source and its destination under the same policy — there is no row
        /// where a separator costs one path something it does not cost the
        /// other.
        ///
        /// This is the field that makes the two flavours **destroy different
        /// objects**, the divergence `Resolution.FinalSymlinkFollowed` warns
        /// about. With `s -> real` a directory, `rename("s/", "moved")` moves
        /// *real* on Darwin, leaving `s` dangling, and is ENOTDIR on Linux;
        /// `rename("src", "s/")` replaces *real* on Darwin and is ENOTDIR on
        /// Linux.
        TrailingSeparator : TrailingSeparatorPolicy
        /// Which path is resolved first, and how far, before the other is
        /// looked at. See `RenameWalkOrder`.
        WalkOrder : RenameWalkOrder
    }

/// What `rename(2)` should do next, once both of its paths have been resolved.
[<RequireQualifiedAccess>]
type RenameVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Both paths name one inode. Succeed, and change nothing at all — not a
    /// binding, not a timestamp.
    ///
    /// A case here rather than a short-circuit in `VirtualFileSystem.rename`
    /// because its *position* is one of the things the flavours disagree about:
    /// Linux answers success for a no-op whose parent the caller may not write,
    /// and Darwin answers EACCES for the same call.
    | NoOp
    /// Move `sourceName` out of `sourceDirectory` and bind it as
    /// `destinationName` in `destinationDirectory`, displacing whatever is bound
    /// there.
    ///
    /// Carries no inode, though the verdict read several to decide: the moving
    /// code gets the displaced one from `VirtualFileSystem.rename`, which
    /// answers what it actually displaced, so there is one source for "which
    /// inode lost a name" and it is the one the move performed.
    | Move of
        sourceDirectory : InodeNumber *
        sourceName : FileName *
        destinationDirectory : InodeNumber *
        destinationName : FileName

/// The questions `rename(2)` asks about the four directories it can refuse for.
[<RequireQualifiedAccess>]
module private RenameChecks =
    /// Whether `inode` refuses this caller the write bit.
    ///
    /// Asked of four different directories — the source's parent, the
    /// destination's parent, the moved directory (whose ".." a change of parent
    /// rewrites) and, on Darwin only, the directory a directory displaces — so
    /// `role` names which, for the crash message. Only the owner triple can ever
    /// apply, since `stat` reports `Kernel.UserId` as every inode's `st_uid`,
    /// and the sticky bit can never refuse for the same reason.
    ///
    /// Partial in `inode`, which every caller has just obtained from a
    /// resolution or from a directory entry.
    let lacksWrite
        (role : string)
        (privilege : CallerPrivilege)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : bool
        =
        match VirtualFileSystem.tryGet inode vfs with
        | Some entry ->
            match VirtualFileSystem.permissions entry with
            | InodePermissions.Stored bits -> PermissionBits.deniedTo privilege 0o200 bits
            | InodePermissions.PlatformSymlinkDefault ->
                failwith
                    $"RenameChecks.lacksWrite: %s{role} is inode %O{inode}, which reports platform-default symlink permissions -- but rename only asks this of a directory (this is an interpreter bug)."
        | None ->
            failwith
                $"RenameChecks.lacksWrite: %s{role} is inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// The inode the destination name is bound to, when that inode is a
    /// directory. `None` covers both "the name is free" and "it names something
    /// that is not a directory", which no arm needs to tell apart — the arms
    /// that care about a non-directory ask `namesNonDirectory` instead.
    let existingDirectory (existing : InodeNumber option) (vfs : VirtualFileSystem) : InodeNumber option =
        existing |> Option.filter (fun inode -> RemovalChecks.isDirectory inode vfs)

    /// Whether the name is bound, and bound to something that is not a
    /// directory. False for a free name, which is what separates this from
    /// `not existingDirectory.IsSome`.
    let namesNonDirectory (existing : InodeNumber option) (vfs : VirtualFileSystem) : bool =
        match existing with
        | Some inode -> not (RemovalChecks.isDirectory inode vfs)
        | None -> false

[<RequireQualifiedAccess>]
module RenameRules =
    /// What the source earns *before* the destination is walked at all, under
    /// this walk order — `None` when it earns nothing yet and the verdict will
    /// judge it against both paths.
    ///
    /// Only Darwin has anything here, because only Darwin resolves the source
    /// to completion first, and what it refuses is exactly what a `namei` under
    /// rename semantics refuses: a final name that is not there, and the
    /// filesystem root named as the whole path.
    ///
    /// Measured against a destination whose parent is a regular file, so the
    /// destination alone answers ENOTDIR and anything else is a source-side
    /// refusal that ran first. On Darwin `rename("nope", "f/x")` is ENOENT and
    /// `rename("/", "f/x")` is EISDIR, while a directory, a symbolic link, a
    /// dangling link, a trailing separator, ".", "..", "/.", "/.." and
    /// "/dev/.." all answer ENOTDIR — so it is the *navigation* rather than the
    /// inode that is early, `FinalNavigation.Root` being the one case that
    /// consumed no component at all. On Linux every one of those rows, the two
    /// above included, answers ENOTDIR. See `docs/probes/rename/walk-order.py`.
    ///
    /// These two arms are also `RenameRules.darwinVerdict`'s, which is not
    /// duplication to remove: the verdict must still answer them when it is
    /// handed two resolutions, and this says *when* Darwin gets to ask. Under
    /// `SourceThenDestination` the verdict's copies are simply reached with the
    /// question already settled.
    let sourceScreen (order : RenameWalkOrder) (source : Resolution) : UnixError option =
        match order with
        | RenameWalkOrder.ParentsThenFinals -> None
        | RenameWalkOrder.SourceThenDestination ->

        match source.Target with
        | ResolvedTarget.Entry (_, _, None) -> Some UnixError.ENOENT
        | ResolvedTarget.Entry (_, _, Some _) -> None
        | ResolvedTarget.Directory (_, FinalNavigation.Root) -> Some UnixError.EISDIR
        | ResolvedTarget.Directory (_, FinalNavigation.Current)
        | ResolvedTarget.Directory (_, FinalNavigation.Parent) -> None

    /// Linux's `rename(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path — either path — that consumed no final name, so "/", "." and
    ///    ".." and any symlink expansion of them, is EBUSY. Linux spends one
    ///    errno on all six positions where `rmdir` spends three: `rename("/", x)`,
    ///    `rename("/.", x)`, `rename("d/.", x)`, `rename("d/..", x)`,
    ///    `rename(x, "d/.")` and `rename(dir, "/")` are all EBUSY. The source is
    ///    asked before the destination, which no row can see, since they share
    ///    an errno.
    ///  * A destination whose parent directory has lost its own last name is
    ///    ENOENT, exactly as `mkdir` and `open(O_CREAT)` answer — and it beats
    ///    the source's trailing-separator demand and the write checks below.
    ///    Only reachable when the current directory is that orphan.
    ///  * A free source name is ENOENT, and beats the trailing-separator arms:
    ///    `rename("nope/", "g")` is ENOENT where `rename("f/", "g")` is ENOTDIR.
    ///  * A trailing separator on the **source** demands the source be a
    ///    directory: ENOTDIR otherwise. This is the arm Linux's walk declines to
    ///    make (`TrailingSeparatorPolicy.Ignore`), so it never traverses a final
    ///    symlink to get here — `rename("lf/", "g")`, `rename("dang/", "g")` and
    ///    `rename("lroot/", "g")` are all ENOTDIR, with no chance of moving a
    ///    link's target.
    ///  * A trailing separator on the **destination** demands that the *source*
    ///    be a directory: `rename(f, "absent/")` and `rename(f, "d/")` are
    ///    ENOTDIR, and so is `rename(p/f, "q/absent/")` with `q` unwritable,
    ///    which is what puts this arm above the write checks.
    ///
    ///    It demands nothing of the destination, and seeing that needs an
    ///    unwritable parent: `rename(d, "q/l/")` with `l` a symlink to a
    ///    directory is ENOTDIR when `q` is writable and **EACCES** when it is
    ///    not. So the ENOTDIR there is the ordinary type rule further down, not
    ///    this arm — the two are indistinguishable until a check between them
    ///    fires.
    ///  * Both paths naming one inode changes nothing and succeeds, and that
    ///    beats every permission check below: `rename(f, g)` with `g` a hard link
    ///    to `f` succeeds from a parent the caller may not write, and so does the
    ///    self-rename of a non-empty directory.
    ///  * A destination directory inside the source's own subtree is EINVAL, and
    ///    it beats *both* write checks — `rename(p/a, p/a/b)` is EINVAL with `p`
    ///    unwritable — as well as the type arm below: `rename(a, a/b/f)` with
    ///    `a/b/f` a regular file is EINVAL, not ENOTDIR.
    ///  * Each parent must grant write: EACCES. Above the type arm, which is
    ///    where Linux and Darwin part company — `rename(p/f, q/dir)` with `p`
    ///    unwritable is EACCES here and EISDIR on Darwin.
    ///  * Then the type rule: a directory over a non-directory is ENOTDIR, a
    ///    non-directory over a directory is EISDIR. A symlink is a
    ///    non-directory whatever it points at, since both walks are
    ///    `NoFollowFinal`.
    ///  * Moving a **directory to a different parent** rewrites its own ".."
    ///    entry, so it demands write on the moved directory itself: EACCES.
    ///    Renaming one within its parent changes nothing inside it and demands
    ///    nothing -- and that holds even when it *displaces* a directory there,
    ///    which is measured (40/40) and is where Darwin diverges a second time. This check is *below* the type arm, unlike the parents' —
    ///    `rename(p/m, q/file)` with `p/m` unwritable is ENOTDIR — and above
    ///    ENOTEMPTY.
    ///  * A destination directory that still holds an entry is ENOTEMPTY.
    ///
    /// Linux never consults the mode of the thing being displaced: measured,
    /// `rename(dir, emptydir)` succeeds with the destination at mode 0. That is
    /// the arm Darwin has and this one does not.
    ///
    /// Measured at uid 0, every row: the EACCES rows fall through to their next
    /// check and nothing else moves, so `CallerPrivilege` gates the write bits
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        match source.Target with
        | ResolvedTarget.Directory _ -> RenameVerdict.Refuse UnixError.EBUSY
        | ResolvedTarget.Entry (sourceDirectory, sourceName, sourceExisting) ->

        match destination.Target with
        | ResolvedTarget.Directory _ -> RenameVerdict.Refuse UnixError.EBUSY
        | ResolvedTarget.Entry (destinationDirectory, destinationName, destinationExisting) ->

        if VirtualFileSystem.isOrphanedDirectory destinationDirectory vfs then
            RenameVerdict.Refuse UnixError.ENOENT
        else

        match sourceExisting with
        | None -> RenameVerdict.Refuse UnixError.ENOENT
        | Some moved ->

        let movedIsDirectory = RemovalChecks.isDirectory moved vfs
        let displacedDirectory = RenameChecks.existingDirectory destinationExisting vfs
        let displacesNonDirectory = RenameChecks.namesNonDirectory destinationExisting vfs

        if source.TrailingSeparatorDemanded && not movedIsDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif destination.TrailingSeparatorDemanded && not movedIsDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif destinationExisting = Some moved then
            RenameVerdict.NoOp
        elif
            movedIsDirectory
            && VirtualFileSystem.isWithinSubtree moved destinationDirectory vfs
        then
            RenameVerdict.Refuse UnixError.EINVAL
        elif RenameChecks.lacksWrite "the source's parent" privilege sourceDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif RenameChecks.lacksWrite "the destination's parent" privilege destinationDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif movedIsDirectory && displacesNonDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif not movedIsDirectory && displacedDirectory.IsSome then
            RenameVerdict.Refuse UnixError.EISDIR
        elif
            movedIsDirectory
            && sourceDirectory <> destinationDirectory
            && RenameChecks.lacksWrite "the moved directory" privilege moved vfs
        then
            RenameVerdict.Refuse UnixError.EACCES
        else

        match displacedDirectory with
        | Some displaced when not (RemovalChecks.isEmptyDirectory displaced vfs) ->
            RenameVerdict.Refuse UnixError.ENOTEMPTY
        | Some _
        | None -> RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName)

    /// Darwin's `rename(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A destination whose parent directory has lost its own last name is
    ///    ENOENT, and on this flavour that beats *everything*, including the
    ///    source's navigation refusal: from inside an `rmdir`'d current
    ///    directory, `rename("d/.", "x")` is ENOENT where it is EINVAL from
    ///    anywhere else.
    ///  * A **source** that consumed no final name: "/" is EISDIR, and any
    ///    directory reached by "." or ".." is EINVAL -- the root included, which
    ///    took an APFS disk image to establish because EXDEV masks it on some
    ///    approaches. See the arm for the rows. Where Linux spends EBUSY on all
    ///    of them.
    ///  * A free source name is ENOENT, and beats the destination's navigation
    ///    arm below: `rename("nope", "d/.")` is ENOENT here and EBUSY on Linux.
    ///  * A **destination** that consumed no final name: "." and ".." are EINVAL
    ///    whatever the source is and whatever they reached — measured with a ".."
    ///    that is not an ancestor of the source, so the rule is about the
    ///    component rather than about ancestry. "/" is not special-cased and
    ///    falls to the type rule: `rename(file, "/")` is EISDIR while
    ///    `rename(dir, "/")` is EINVAL.
    ///  * The type rule, which on this flavour is above everything below it: a
    ///    directory over a non-directory is ENOTDIR, a non-directory over a
    ///    directory is EISDIR. `rename(p/f, q/dir)` with `p` unwritable is EISDIR
    ///    here and EACCES on Linux, and `rename(a, a/b/f)` with `a/b/f` a file is
    ///    ENOTDIR here and EINVAL on Linux.
    ///  * A trailing separator on the destination, over a name that is *free*,
    ///    demands that the source be a directory: ENOENT otherwise.
    ///    `rename(f, "absent/")` is ENOENT where `rename(d, "absent/")` succeeds
    ///    — XNU passes `WILLBEDIR` to the destination lookup exactly when the
    ///    source is a directory. Linux answers the same shape ENOTDIR. The
    ///    source's own separator needs no arm: Darwin's walk is `Demand` and has
    ///    already refused it.
    ///  * A destination directory inside the source's own subtree is EINVAL,
    ///    beating both write checks below.
    ///  * The source's parent must grant write: EACCES. Above the no-op, which
    ///    is the arm Linux orders the other way round — `rename(f, g)` with `g` a
    ///    hard link to `f` is EACCES here from an unwritable parent, and succeeds
    ///    on Linux.
    ///  * Then a write check on the destination side, and *which* directory it
    ///    asks about is the strangest measured fact in this syscall: when a
    ///    directory replaces an existing directory, Darwin consults the write bit
    ///    of the **directory being displaced** and never looks at its parent at
    ///    all. Measured four ways — with the parent at 0o555 and the displaced
    ///    directory at 0o755 it succeeds, at 0o755 and 0o000 it is EACCES, at
    ///    0o555 and 0o300 it succeeds, and a control confirms the parent really
    ///    does refuse an ordinary create. Every other shape consults the
    ///    destination's parent as Linux does.
    ///  * Both paths naming one inode changes nothing and succeeds — below the
    ///    two write checks above, which is why the self-rename of a directory
    ///    whose own write bit is missing is EACCES here and succeeds on Linux.
    ///  * Moving a directory demands write on the moved directory -- on *two*
    ///    occasions where Linux wants one. Linux asks only when the parent
    ///    changes, which is the ".." rewrite; Darwin asks then and also whenever
    ///    the moved directory displaces another directory, within one parent
    ///    included. Measured 40/40: `rename("p/m", "p/d")` with `m` at 0o555 and
    ///    `d` an existing directory is EACCES here and succeeds on Linux, while
    ///    the same call to a free name succeeds on both.
    ///  * A destination directory that still holds an entry is ENOTEMPTY, below
    ///    the displaced-directory write check: `rename(dir, fulldir)` with the
    ///    non-empty destination at mode 0 is EACCES here and ENOTEMPTY on Linux.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so a separator over an
    /// *existing* non-directory never reaches here — the walk has already
    /// answered ENOTDIR, ELOOP or ENOENT. What does reach here is a separator
    /// over a directory a final symlink named, and that is the destructive row:
    /// `rename("s/", "moved")` moves the link's target.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        let destinationParentIsOrphan =
            match destination.Target with
            | ResolvedTarget.Entry (destinationDirectory, _, _) ->
                VirtualFileSystem.isOrphanedDirectory destinationDirectory vfs
            | ResolvedTarget.Directory _ -> false

        if destinationParentIsOrphan then
            RenameVerdict.Refuse UnixError.ENOENT
        else

        match source.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RenameVerdict.Refuse UnixError.EISDIR
            // No root special case, unlike Darwin's `unlink` and `rmdir`, which
            // each give the root its own EBUSY arm. Establishing that took some
            // care, because the obvious measurement is masked: a filesystem root
            // that is not "/" is a *mount* root, and renaming one is liable to
            // EXDEV.
            //
            // Measured on a fresh APFS image, 40 trials per row, all stable. The
            // discriminator turns out not to be "." against ".." but whether the
            // source's parent directory and the destination's parent directory
            // are the same object: with `p` a directory inside the mount,
            // `rename("base/.", "p/x")` and `rename("p/..", "base/x")` both reach
            // the mount root and both answer **EINVAL**, while the same two
            // sources with the destination in the other directory answer EXDEV.
            // So EXDEV is the mount boundary talking, and where it stays quiet
            // the root answers exactly what any other directory answers.
            //
            // PawPrint has one filesystem and no mounts, so nothing here can
            // produce EXDEV and the EINVAL readings are the applicable ones.
            | FinalNavigation.Current
            | FinalNavigation.Parent -> RenameVerdict.Refuse UnixError.EINVAL
        | ResolvedTarget.Entry (sourceDirectory, sourceName, sourceExisting) ->

        match sourceExisting with
        | None -> RenameVerdict.Refuse UnixError.ENOENT
        | Some moved ->

        let movedIsDirectory = RemovalChecks.isDirectory moved vfs

        match destination.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Current
            | FinalNavigation.Parent -> RenameVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Root ->
                if movedIsDirectory then
                    RenameVerdict.Refuse UnixError.EINVAL
                else
                    RenameVerdict.Refuse UnixError.EISDIR
        | ResolvedTarget.Entry (destinationDirectory, destinationName, destinationExisting) ->

        let displacedDirectory = RenameChecks.existingDirectory destinationExisting vfs
        let displacesNonDirectory = RenameChecks.namesNonDirectory destinationExisting vfs

        if movedIsDirectory && displacesNonDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif not movedIsDirectory && displacedDirectory.IsSome then
            RenameVerdict.Refuse UnixError.EISDIR
        elif
            destination.TrailingSeparatorDemanded
            && destinationExisting.IsNone
            && not movedIsDirectory
        then
            RenameVerdict.Refuse UnixError.ENOENT
        elif
            movedIsDirectory
            && VirtualFileSystem.isWithinSubtree moved destinationDirectory vfs
        then
            RenameVerdict.Refuse UnixError.EINVAL
        elif RenameChecks.lacksWrite "the source's parent" privilege sourceDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif
            // Which directory this asks about is the measured oddity. A
            // directory displacing a directory is the one shape where Darwin
            // consults the displaced object rather than the directory holding
            // it -- and `displacedDirectory` being `Some` here already implies
            // the source is a directory, because the EISDIR arm above refused
            // the only other way to reach this line with one.
            (match displacedDirectory with
             | Some displaced -> RenameChecks.lacksWrite "the displaced directory" privilege displaced vfs
             | None -> RenameChecks.lacksWrite "the destination's parent" privilege destinationDirectory vfs)
        then
            RenameVerdict.Refuse UnixError.EACCES
        elif destinationExisting = Some moved then
            RenameVerdict.NoOp
        elif
            // Two occasions, not one, and this is where Darwin parts from Linux
            // a second time. Linux wants this bit only when the parent changes,
            // which is the ".." rewrite and nothing else. Darwin wants it then
            // *and* whenever the moved directory displaces another directory,
            // even within one parent: measured 40/40, `rename("p/m", "p/d")`
            // with `m` at 0o555 and `d` an existing directory is EACCES, where
            // the same call to a free name succeeds and Linux allows both.
            //
            // It beats ENOTEMPTY below on the same shape -- a non-empty `d` is
            // still EACCES -- which is what makes it a check in its own right
            // rather than a spelling of the displaced-directory one above.
            movedIsDirectory
            && (sourceDirectory <> destinationDirectory || displacedDirectory.IsSome)
            && RenameChecks.lacksWrite "the moved directory" privilege moved vfs
        then
            RenameVerdict.Refuse UnixError.EACCES
        else

        match displacedDirectory with
        | Some displaced when not (RemovalChecks.isEmptyDirectory displaced vfs) ->
            RenameVerdict.Refuse UnixError.ENOTEMPTY
        | Some _
        | None -> RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName)

    /// Decide what a `rename(2)` owes, given how its two paths resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rename` makes the case more strongly than either removal did, because
    /// here the flavours do not even agree on *which object* a check is about —
    /// a directory displacing a directory consults the displaced directory's
    /// write bit on Darwin and its parent's on Linux, which is not a reordering
    /// of one check but two different checks.
    ///
    /// `source` and `destination` must both have been resolved under
    /// `SimulatedUnixPlatform.renameRules`' `TrailingSeparator` and
    /// `SymlinkPolicy.NoFollowFinal`, which is what makes the trailing-separator
    /// arms above mean what they say.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege source destination vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege source destination vfs

/// A reason `bind(2)` refuses, as one of the checks it makes rather than as an
/// errno: which errno a fault becomes is fixed, but *which fault is reported*
/// when several hold at once is per-flavour. See
/// `SimulatedUnixPlatform.bindFaultOrder`.
/// What this platform's `bind(2)` makes of a declared `socketAddressLen`.
///
/// The two rejections are not interchangeable, and the difference is *when* they
/// happen rather than which errno they carry. Measured on both: a length past the
/// upper bound is rejected before the kernel copies anything, so it beats a
/// faulting pointer and beats the family check — an unmapped pointer at 129 is
/// EINVAL on Linux where at 8 it is EFAULT, and a wrong-family blob at 256 is
/// ENAMETOOLONG on Darwin where at 129 it is EAFNOSUPPORT. A length merely too
/// short takes its ordinary place in `bindFaultOrder`.
[<RequireQualifiedAccess>]
type BindLengthVerdict =
    /// A length this platform will parse an address out of.
    | Accepted
    /// Past the greatest length this platform will consider, and so refused
    /// before the address is copied or read at all. Linux answers `EINVAL` above
    /// `sizeof(struct sockaddr_storage)`; Darwin answers `ENAMETOOLONG` above its
    /// own, larger threshold.
    | RejectedBeforeCopy of error : UnixError
    /// `EINVAL`, from the `Length` position of this platform's fault order.
    | Invalid

[<RequireQualifiedAccess>]
type BindFault =
    /// The declared `socketAddressLen` is not one this platform accepts for the
    /// address family in the blob. Which errno that becomes is the
    /// `BindLengthVerdict` the length classifier gave — `EINVAL`, or
    /// `ENAMETOOLONG` past the greatest length the platform considers — but the
    /// *position* in the order is the same either way, which is why the verdict
    /// is not carried here.
    | Length
    /// The blob's address family is not the socket's. `EAFNOSUPPORT`.
    | Family
    /// No local interface holds the address. `EADDRNOTAVAIL`.
    | AddressNotLocal
    /// The port is below `privilegedPortCeiling` and the process is not root.
    /// `EACCES`.
    | PrivilegedPort
    /// This socket already has a local address. `EINVAL`.
    | AlreadyBound
    /// Another socket holds a conflicting address. `EADDRINUSE`.
    | AddressInUse

[<RequireQualifiedAccess>]
module SimulatedUnixPlatform =
    /// Loosest ceiling any Unix we model imposes on `utsname.release`:
    /// macOS's `_SYS_NAMELEN` is 256 (including the NUL), while Linux's
    /// `_UTSNAME_LENGTH` is only 65. Bounded by the looser of the two rather
    /// than per-flavour, because the limit is about what a *guest* can be
    /// handed rather than about which kernel wrote it, and an unbounded string
    /// could hand a guest a release no real `uname` could produce.
    [<Literal>]
    let private maxReleaseLength : int = 255

    let describe (error : SimulatedUnixReleaseError) : string =
        match error with
        | SimulatedUnixReleaseError.Empty ->
            "release string is empty, but every Unix `uname(2)` fills `utsname.release`"
        | SimulatedUnixReleaseError.TooLong (length, limit) ->
            $"release string is %d{length} characters, exceeding the %d{limit}-character limit any Unix `utsname.release` can hold"
        | SimulatedUnixReleaseError.NotPrintableAscii (index, character) ->
            $"release string contains non-printable-ASCII character U+%04X{int character} at index %d{index}; `utsname.release` is reported to the guest as single-byte characters, so only printable ASCII round-trips faithfully"

    /// A platform of the given flavour reporting `release` from `uname -r`.
    ///
    /// Validated here rather than when the release is read, which is what makes
    /// every accessor below total: a value of this type is a platform some Unix
    /// could actually be.
    let create
        (flavour : SimulatedUnixFlavour)
        (release : string)
        : Result<SimulatedUnixPlatform, SimulatedUnixReleaseError>
        =
        if System.String.IsNullOrEmpty release then
            Error SimulatedUnixReleaseError.Empty
        elif String.length release > maxReleaseLength then
            Error (SimulatedUnixReleaseError.TooLong (String.length release, maxReleaseLength))
        else

        match release |> Seq.tryFindIndex (fun c -> c < ' ' || c > '~') with
        | Some i -> Error (SimulatedUnixReleaseError.NotPrintableAscii (i, release.[i]))
        | None ->
            Ok
                {
                    Flavour = flavour
                    Release = release
                }

    let createOrFail (context : string) (flavour : SimulatedUnixFlavour) (release : string) : SimulatedUnixPlatform =
        match create flavour release with
        | Ok platform -> platform
        | Error error -> failwith $"%s{context}: %s{describe error}"

    /// 64-bit x86 Linux, at the exact kernel PawPrint's CI runs: the release
    /// this reports and the behaviour derived from it below therefore describe
    /// one real machine rather than a plausible composite. The default, and the
    /// flavour whose CoreLib actually routes `Environment.OSVersion` through
    /// `SystemNative_GetUnixRelease` at all (the macOS CoreLib goes via
    /// `Interop.libobjc.GetOperatingSystemVersion` instead).
    ///
    /// Naming a real kernel rather than a plausible one matters because facts
    /// derived from a platform are claims about a machine somebody could be
    /// running. Note the division of labour: identity that a guest reads back,
    /// like this release, belongs to the platform, because it is the same on
    /// every machine running this kernel image; a fact that varies between two
    /// machines running this very kernel, like the user-address limit, is a
    /// client's configuration instead.
    let linuxX64 : SimulatedUnixPlatform =
        createOrFail "SimulatedUnixPlatform.linuxX64" SimulatedUnixFlavour.Linux "6.17.0-1022-azure"

    /// 64-bit ARM macOS. The release is the *Darwin* kernel's, so `24.6.0`
    /// (macOS 15.6) rather than `15.6.0`.
    let macOsArm64 : SimulatedUnixPlatform =
        createOrFail "SimulatedUnixPlatform.macOsArm64" SimulatedUnixFlavour.Darwin "24.6.0"

    /// Which Unix this platform is.
    let flavour (platform : SimulatedUnixPlatform) : SimulatedUnixFlavour = platform.Flavour

    /// The `utsname.release` string this platform reports, i.e. exactly what
    /// `uname -r` would print. Part of PawPrint's replay contract: changing a
    /// preset's value changes the `Environment.OSVersion` every recorded trace
    /// on that platform observes.
    let unixRelease (platform : SimulatedUnixPlatform) : string = platform.Release

    /// Re-check the invariant of a value that may not have come from `create`.
    /// See `FileName.assertValid`: the only value this can reject is
    /// `Unchecked.defaultof` / C# `default`, whose null release would otherwise
    /// be handed to a guest as its `uname -r`.
    let assertValid (context : string) (platform : SimulatedUnixPlatform) : SimulatedUnixPlatform =
        // A record is a reference type, so the forged value is `null` itself
        // rather than a record with a null field — and reading `Flavour` off it
        // would throw a `NullReferenceException` naming nothing useful.
        match box platform with
        | null ->
            failwith
                $"%s{context}: the platform is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create, or use the linuxX64 / macOsArm64 presets."
        | _ ->

        match create platform.Flavour platform.Release with
        | Ok _ -> platform
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SimulatedUnixPlatform that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create instead."

    /// Whose `<errno.h>` numbering this platform reports, for the errors where
    /// the two Unixes disagree.
    ///
    /// This is the choice `UnixError.toRawErrno` refuses to make on its own, and
    /// it is what lets an `ELOOP` reach a guest at all: raw 40 is `ELOOP` on
    /// Linux but `EMSGSIZE` on Darwin, so the number is meaningless until
    /// something says which Unix is being impersonated. The flavour says.
    let rawErrnoNumbering (platform : SimulatedUnixPlatform) : RawErrnoNumbering =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> RawErrnoNumbering.Linux
        | SimulatedUnixFlavour.Darwin -> RawErrnoNumbering.Darwin

    /// What this platform's `getcwd(3)` reports for a removed current directory.
    /// See `GetCwdOrphanAnswer`.
    let getCwdOrphanAnswer (platform : SimulatedUnixPlatform) : GetCwdOrphanAnswer =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetCwdOrphanAnswer.AlwaysDetached
        | SimulatedUnixFlavour.Darwin -> GetCwdOrphanAnswer.ShortestPathFirst

    /// What this platform's `getcwd(3)` does with a destination it cannot write.
    /// See `GetCwdDestinationFault`.
    let getCwdDestinationFault (platform : SimulatedUnixPlatform) : GetCwdDestinationFault =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetCwdDestinationFault.ReportedAsEfault
        | SimulatedUnixFlavour.Darwin -> GetCwdDestinationFault.FatalToTheProcess

    /// What this platform's `getsockname(2)` has already stored in the caller's
    /// length cell when the address copy faults. See `GetSockNameFaultLength`.
    let getSockNameFaultLength (platform : SimulatedUnixPlatform) : GetSockNameFaultLength =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetSockNameFaultLength.AlreadyReported
        | SimulatedUnixFlavour.Darwin -> GetSockNameFaultLength.Untouched

    /// Whether the socket `accept(2)` hands back inherits `O_NONBLOCK` from the
    /// listening descriptor.
    ///
    /// The classic BSD/POSIX divergence, measured 2026-08-28 with
    /// `docs/plans/2026-08-23-posix-kernel-extraction/accept-inherits-nonblock.c`:
    /// on Linux 6.18.5 a non-blocking listener yields a *blocking* accepted
    /// socket, and on Darwin 25.6.0 a non-blocking one. Blocking listeners yield
    /// blocking sockets on both.
    ///
    /// This is the kernel's answer and not a runtime's. A client whose own
    /// sockets expect one answer everywhere has to normalise it -- CoreCLR's
    /// `SystemNative_Accept` clears the flag under `#if !defined(__linux__)`,
    /// with the comment "Our socket code expects new socket to be in blocking
    /// mode by default" -- and that normalisation belongs to the client rather
    /// than here.
    let acceptedSocketInheritsNonBlocking (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// What this platform's PAL puts in `DirectoryEntry.NameLength`. See
    /// `DirectoryEntryNameLength`.
    let directoryEntryNameLength (platform : SimulatedUnixPlatform) : DirectoryEntryNameLength =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> DirectoryEntryNameLength.WalkToTerminator
        | SimulatedUnixFlavour.Darwin -> DirectoryEntryNameLength.Reported

    /// Whether this platform's `stat` reports a creation time.
    ///
    /// A compile-time property of the native shim rather than of any file:
    /// `ConvertFileStatus` in `pal_io.c` sets `BirthTime` and the
    /// `HAS_BIRTHTIME` flag under `#if HAVE_STAT_BIRTHTIME` — true on macOS,
    /// false on Linux, where it hard-zeroes both with the comment "Linux path:
    /// until we use statx()". So the birth time is a real fact about the inode
    /// on both, and this governs only whether the guest is told it.
    let reportsBirthTime (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// The permission bits this platform reports for a symbolic link, which no
    /// syscall can set and which the two Unixes disagree about.
    ///
    /// Measured rather than read: with `umask 022` macOS reports 0o755 for a
    /// fresh symlink, with `umask 077` it reports 0o700 and with `umask 000`
    /// 0o777 — it applies the creating process's umask, exactly as it does to a
    /// regular file. Linux reports 0o777 whatever the umask, which is why
    /// `InodePermissions` derives this rather than storing it: under a Linux
    /// simulation a stored value could only ever describe a filesystem no
    /// kernel produced.
    ///
    /// The Darwin answer here is the `umask 022` one, and stays a constant even
    /// though a process umask is modelled: a symbolic link can only enter this
    /// filesystem through a *seed*, and a seed describes a tree some other
    /// process built, so this run's configured umask is not the one that applied
    /// to it. The day a `symlink(2)` lets a guest create one, that link *is*
    /// created by this process and this must become a function of the configured
    /// umask — that is the trigger, not the existence of the field.
    let symlinkPermissions (platform : SimulatedUnixPlatform) : PermissionBits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" 0o777
        | SimulatedUnixFlavour.Darwin ->
            PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" (0o777 &&& ~~~0o022)

    /// Whether this platform clears a truncated file's set-user-ID and
    /// set-group-ID bits.
    ///
    /// The only thing about truncation the two Unixes disagree about — every
    /// other row measured (the errno order, which descriptors refuse, the
    /// zero-fill, the timestamps, and `O_TRUNC`'s extra write-permission
    /// requirement) is unanimous, which is why this is a lone value rather than a
    /// `CreatingOpenRules`-shaped record.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, for `ftruncate(2)`,
    /// `O_TRUNC` and a no-op `ftruncate` alike; `PermissionBits.afterTruncation`
    /// carries the table. Linux applies the same rule it applies to a write.
    /// **Darwin strips nothing at all**, and that is isolated rather than
    /// inferred: in one process, on one file, a one-byte `write` takes `04755` to
    /// `00755` there while `ftruncate` leaves it `04755`.
    let setIdBitsOnTruncation (platform : SimulatedUnixPlatform) : SetIdBitsOnTruncation =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetIdBitsOnTruncation.Strip
        | SimulatedUnixFlavour.Darwin -> SetIdBitsOnTruncation.Preserve

    /// Whether this platform's content-changing `write(2)` clears `S_ISGID` on a
    /// file that is not group-executable.
    ///
    /// The only thing about a write's effect on the mode that the two Unixes
    /// disagree about: `S_ISUID` goes on both whatever the execute bits say, and
    /// the sticky bit is left alone by both. So this is a lone value rather than
    /// a `CreatingOpenRules`-shaped record, for the reason
    /// `setIdBitsOnTruncation` above gives.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, one byte written over
    /// the front of a four-byte file; `PermissionBits.afterContentChangingWrite`
    /// carries the table. Linux applies to a write the same rule it applies to a
    /// truncation, and **Darwin does not** — there a write strips `02644` to
    /// `00644` while an `ftruncate` on the same file leaves the whole mode alone,
    /// which is why the two rules are separate values rather than one.
    ///
    /// The file must be handed to a group the caller belongs to before `chmod`,
    /// or the kernel drops `S_ISGID` silently and the measurement reads as
    /// agreement.
    let setGroupIdOnWrite (platform : SimulatedUnixPlatform) : SetGroupIdOnWrite =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetGroupIdOnWrite.StripWhenGroupExecutable
        | SimulatedUnixFlavour.Darwin -> SetGroupIdOnWrite.StripAlways

    /// How this platform's `open(2)` behaves when asked to create; see
    /// `CreatingOpenRules` for what each field means and how it was measured.
    let creatingOpenRules (platform : SimulatedUnixPlatform) : CreatingOpenRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.RefuseIsDirectory
                RefusesExistingDirectory = true
                RootNavigation = None
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o7777
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RefusesExistingDirectory = false
                RootNavigation = Some UnixError.EEXIST
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o0777
            }

    /// Everything this platform's `mkdir(2)` does differently. See `MkDirRules`
    /// for the measurements; note in particular that `ModeMask` is not
    /// `creatingOpenRules`' one on Linux.
    let mkDirRules (platform : SimulatedUnixPlatform) : MkDirRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.mkDirRules" 0o1777
                InheritsSetGroupIdFromParent = true
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.mkDirRules" 0o0777
                InheritsSetGroupIdFromParent = false
            }

    /// Everything this platform's `unlink(2)` does differently. See
    /// `UnlinkRules`, whose one field this picks; the rest of the divergence is
    /// in `UnlinkRules.verdict`, which takes the flavour directly.
    let unlinkRules (platform : SimulatedUnixPlatform) : UnlinkRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
            }

    /// Everything this platform's `rmdir(2)` does differently. See `RmDirRules`,
    /// whose two fields this picks; the ordering half of the divergence is in
    /// `RmDirRules.verdict`, which takes the flavour directly.
    let rmDirRules (platform : SimulatedUnixPlatform) : RmDirRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                RemovedDirectoryEffect = UnbindTargetEffect.LostALink
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RemovedDirectoryEffect = UnbindTargetEffect.Untouched
            }

    /// Everything this platform's `rename(2)` does differently. See
    /// `RenameRules`, whose two fields this picks; the ordering of the refusals
    /// — which is most of the divergence — is in `RenameRules.verdict`, which
    /// takes the flavour directly.
    let renameRules (platform : SimulatedUnixPlatform) : RenameRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                WalkOrder = RenameWalkOrder.ParentsThenFinals
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                WalkOrder = RenameWalkOrder.SourceThenDestination
            }

    /// Whether this platform's kernel screens a read or write buffer before it
    /// performs the operation.
    ///
    /// Linux's `vfs_read`/`vfs_write` (fs/read_write.c) reject an out-of-range
    /// buffer with EFAULT between the descriptor's access-mode check and the
    /// file operation, so the fault beats EISDIR and fires for a zero-length
    /// request. macOS screens nothing up front, so a call that transfers no
    /// bytes never looks at the buffer: measured, `read(f, (void*)-1, 5)` on a
    /// descriptor at end-of-file is EFAULT on Linux and 0 on macOS.
    ///
    /// *Where* it screens is the machine's `UserAddressLimit`, not a property
    /// of the flavour: both architectures compare the range end against
    /// `TASK_SIZE_MAX` (`valid_user_address` against `USER_PTR_MAX` in
    /// arch/x86/include/asm/uaccess_64.h, and the
    /// `(u65)addr + (u65)size <= (u65)TASK_SIZE_MAX` that
    /// arch/arm64/include/asm/uaccess.h documents), and that value varies with
    /// paging depth and virtual-address width — measured, two GitHub runners in
    /// one CI run disagreed. A caller combines the two: this predicate decides
    /// *whether* there is an up-front check, and its own configured limit says
    /// what that check compares against.
    let screensUserBufferUpFront (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The bounds this platform's kernel puts on path resolution.
    ///
    /// The numbers are measured facts about real kernels, which is why they are
    /// derived from the flavour rather than configured: a host that could set
    /// them could describe a Unix that does not exist, and a guest would then
    /// see a `MAXSYMLINKS` no real system has. `TestVirtualFileSystemAgainstHost`
    /// pins the value for whichever flavour it is running on against that
    /// kernel's *measured* behaviour, so macOS locally and Linux in CI each
    /// check one column.
    /// `PATH_MAX` counts the NUL, so the usable lengths are one less: measured,
    /// an argument of 1023 bytes resolves on macOS and 1024 does not, and 4095
    /// and 4096 respectively on Linux.
    ///
    /// `NAME_MAX` is 255 on both — but *of different things*, which is why it
    /// carries its unit. See `NameLengthLimit`: `中`×255 is 765 bytes and 255
    /// UTF-16 units, and APFS resolves it where ext4 refuses it.
    let pathLimits (platform : SimulatedUnixPlatform) : PathLimits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            PathLimits.create 40 4096 (NameLengthLimit.Utf8Bytes 255) SpliceLengthRecheck.NoRecheck
        | SimulatedUnixFlavour.Darwin ->
            PathLimits.create 32 1024 (NameLengthLimit.Utf16CodeUnits 255) SpliceLengthRecheck.Recheck

    /// `sizeof(struct sockaddr_storage)`: the size of the largest socket address
    /// any Unix we model can hand back, and so the buffer size CoreLib sizes
    /// every socket-address buffer by. Reported to the guest by
    /// `SystemNative_GetMaximumAddressSize`.
    ///
    /// A compile-time property of the native shim rather than of any socket, like
    /// `reportsBirthTime`. Unlike that one it takes no flavour: both families
    /// *define* the constant in their headers rather than computing it
    /// (`_SS_MAXSIZE` on Darwin, `_SS_SIZE` in glibc's generic `bits/sockaddr.h`)
    /// and derive the padding members from it, so the value is invariant of
    /// pointer width as well as agreed between the two — both descend from
    /// RFC 2553's sample definition. Measured 128 on macOS arm64 and on Linux
    /// alike, and re-pinned against a real platform on every test run by
    /// `sourcesPure/SystemNativeGetMaximumAddressSize.cs`. Make it a function of
    /// the flavour on the day one of them disagrees.
    ///
    /// Contrast `sockaddr_un`, which genuinely does differ (106 on Darwin, 110 on
    /// Linux). That is `SocketAddressSizes.UnixDomain` below, reported through a
    /// different entry point again; this binding is where the shared 128 is
    /// defined, and `socketAddressSizes` reads it rather than repeating it.
    let maximumSocketAddressSize : int = 128

    /// The sizes `SystemNative_GetSocketAddressSizes` reports. See
    /// `SocketAddressSizes` for where each number was measured.
    let socketAddressSizes (platform : SimulatedUnixPlatform) : SocketAddressSizes =
        {
            InterNetwork = 16
            InterNetworkV6 = 28
            UnixDomain =
                match flavour platform with
                | SimulatedUnixFlavour.Linux -> 110
                | SimulatedUnixFlavour.Darwin -> 106
            Storage = maximumSocketAddressSize
        }

    /// The order `bind(2)` reports its faults in, which is **not** the same on
    /// the two flavours.
    ///
    /// Measured pairwise, by presenting each pair of faults together and seeing
    /// which errno came back. Linux checks the declared length before it reads
    /// the family, and defers "this socket is already bound" until after it has
    /// validated the address; Darwin reads the family first and rejects an
    /// already-bound socket before it looks at the address at all. So
    /// a rebind to a non-local address is `EADDRNOTAVAIL` on Linux and `EINVAL`
    /// on Darwin, and a short `sockaddr_in6` on an IPv4 socket is `EINVAL` on
    /// Linux and `EAFNOSUPPORT` on Darwin.
    ///
    /// Expressed as an order over faults rather than as nested branches so that
    /// the divergence is one list rather than two code paths, and so a test can
    /// assert the order directly.
    let bindFaultOrder (platform : SimulatedUnixPlatform) : BindFault list =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            [
                BindFault.Length
                BindFault.Family
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AlreadyBound
                BindFault.AddressInUse
            ]
        | SimulatedUnixFlavour.Darwin ->
            [
                BindFault.Family
                BindFault.Length
                BindFault.AlreadyBound
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AddressInUse
            ]

    /// The first fault in this platform's order that `faults` contains.
    let firstBindFault (platform : SimulatedUnixPlatform) (faults : Set<BindFault>) : BindFault option =
        bindFaultOrder platform |> List.tryFind (fun fault -> Set.contains fault faults)

    /// How long `bind(2)` insists a `struct sockaddr_in` argument is.
    ///
    /// Measured, and not the same shape on the two: Linux accepts any length from
    /// the family's own `sizeof` up to `sizeof(struct sockaddr_storage)` — 16
    /// through 128 inclusive for IPv4, with 129 the least rejected — while Darwin
    /// insists on exactly 16 and answers `EINVAL` for every value from 17 to 32.
    ///
    /// Invisible through the managed API, which always passes
    /// `SocketAddress.Size`; a hand-rolled `[DllImport]` sees it immediately.
    /// The greatest `socketAddressLen` Darwin's `bind(2)` will consider at all.
    /// Above it the answer is `ENAMETOOLONG` rather than `EINVAL`; measured, 255
    /// is `EINVAL` and 256 is `ENAMETOOLONG`. Linux has no such threshold.
    let maximumDarwinSocketAddressLength : int = 255

    let bindAddressLength (platform : SimulatedUnixPlatform) (exactSize : int) (declared : int) : BindLengthVerdict =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            if declared > maximumSocketAddressSize then
                BindLengthVerdict.RejectedBeforeCopy UnixError.EINVAL
            elif declared >= exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid
        | SimulatedUnixFlavour.Darwin ->
            if declared > maximumDarwinSocketAddressLength then
                BindLengthVerdict.RejectedBeforeCopy UnixError.ENAMETOOLONG
            elif declared = exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid

    /// May a socket bind to this address, given the addresses this machine holds?
    ///
    /// The wildcard always binds. Beyond that the flavours read the same list
    /// differently, which is measured rather than inferred: `127.9.9.9` binds on
    /// Linux and is `EADDRNOTAVAIL` on Darwin, because Linux treats every address
    /// inside a local prefix as assigned while Darwin assigns loopback exactly
    /// one address.
    ///
    /// Is this the all-ones broadcast address, or a multicast one
    /// (`224.0.0.0/4`)?
    ///
    /// **PawPrint refuses to bind either**, rather than answering. Measured, the
    /// rule is not one rule: Linux takes both on a stream socket, Darwin answers
    /// `EAFNOSUPPORT` there, and on Darwin the answer depends on the socket's
    /// *kind* besides — a datagram socket binds a multicast group where a stream
    /// socket does not. Modelling that is modelling multicast, which is group
    /// membership and an interface to receive on, and PawPrint has neither; a
    /// bind that succeeded here would become a lie the moment `recvfrom` landed.
    ///
    /// So this classifier exists to *refuse* precisely, at the point in
    /// `bindFaultOrder` where the address is judged — a fault the platform ranks
    /// earlier still wins, which is what keeps the refusal from swallowing
    /// answers PawPrint does know.
    let isBroadcastOrMulticast (address : uint32) : bool =
        address = System.UInt32.MaxValue || (address >>> 28) = 0xEu

    /// Broadcast and multicast are a further Linux-only allowance
    /// (`255.255.255.255` and `224.0.0.1` bind there and are `EAFNOSUPPORT` on
    /// Darwin). Neither is modelled: PawPrint has no interface to broadcast on,
    /// and the entry point refuses such an address rather than answering, so a
    /// guest that needs one gets a diagnosis instead of a wrong errno.
    let isBindableAddress
        (platform : SimulatedUnixPlatform)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        if address = InternetEndpoint.WildcardAddress then
            true
        elif List.contains address localAddresses then
            // An address this machine holds binds on either flavour.
            true
        else

        match flavour platform with
        // Linux additionally takes anything it has a *local route* to, which is
        // why `127.9.9.9` binds there. An interface's subnet is not such a route
        // — holding `192.168.1.10/24` does not make `192.168.1.11` bindable — so
        // this reads the route table rather than widening the assigned addresses.
        | SimulatedUnixFlavour.Linux -> localRoutes |> List.exists (Ipv4Prefix.contains address)
        | SimulatedUnixFlavour.Darwin -> false

    /// Whether `bind(2)` has something to say about the address itself, as
    /// opposed to about the length, the family, or another socket. Callers rank
    /// this against the other faults in `bindFaultOrder`, at
    /// `BindFault.AddressNotLocal`.
    ///
    /// That is `EADDRNOTAVAIL` in every case PawPrint answers. A broadcast or
    /// multicast address faults here too, and its caller refuses it outright
    /// rather than reporting an errno — which is why this is not simply
    /// `not isBindableAddress`. Such an address is not necessarily *unbindable*:
    /// Linux binds `224.0.0.1` on a stream socket quite happily. It is one
    /// PawPrint declines to answer for, and a host that listed it in
    /// `LocalAddresses`, or covered it with a `LocalRoutes` prefix, would
    /// otherwise silence the refusal and record a multicast binding that nothing
    /// downstream can honour.
    let bindAddressFaults
        (platform : SimulatedUnixPlatform)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        isBroadcastOrMulticast address
        || not (isBindableAddress platform localAddresses localRoutes address)

    /// Does a bind of `candidate` collide with the socket already bound at
    /// `existing`?
    ///
    /// Both flavours refuse two sockets the same port on overlapping addresses,
    /// and both relax that when `SO_REUSEADDR` is set — in opposite directions,
    /// which is the whole of the divergence here and is measured in both:
    ///
    /// * **Linux** relaxes only while nothing is listening. Two sockets that both
    ///   set the flag may share an address, exactly or through the wildcard,
    ///   until one of them calls `listen(2)`; after that the second bind is
    ///   `EADDRINUSE`.
    /// * **Darwin** relaxes only for addresses that differ. Two sockets that both
    ///   set the flag may hold the wildcard and a specific address on one port,
    ///   listening or not; the exact duplicate is `EADDRINUSE` either way.
    ///
    /// With the flag absent on either side — every UDP bind through the shim, and
    /// every `ProtocolType.Unspecified` one — the two agree and refuse.
    ///
    /// The same relation answers `listen(2)`, which is measured rather than
    /// assumed: on Linux two reuse-carrying sockets may share an endpoint until
    /// one listens, and the *second* `listen` is then EADDRINUSE — exactly what
    /// this says when the other socket is already listening. Darwin never refuses
    /// a listen, and never lets the pair coexist in the first place.
    let bindConflict
        (platform : SimulatedUnixPlatform)
        (existing : SocketBinding)
        (existingReuse : bool)
        (existingPhase : SocketPhase)
        (candidate : SocketBinding)
        (candidateReuse : bool)
        : bool
        =
        if existing.Endpoint.Port <> candidate.Endpoint.Port then
            false
        elif not (InternetEndpoint.addressesOverlap existing.Endpoint candidate.Endpoint) then
            false
        else

        let existingIsListening = SocketPhase.isListening existingPhase

        // An established socket's pcb is keyed by its full peer tuple, and a
        // replacement listener can bind over it: measured on both kernels
        // (accept a connection, close the listener, bind a reuse-carrying
        // replacement at the exact endpoint — OK; without the candidate's
        // reuse flag — EADDRINUSE).
        let existingIsEstablished =
            match existingPhase with
            | SocketPhase.Established _
            | SocketPhase.EstablishedPendingReport _ -> true
            | SocketPhase.Idle
            | SocketPhase.Listening _
            | SocketPhase.RefusedPendingDelivery
            | SocketPhase.Dead
            | SocketPhase.DatagramPeer _ -> false

        match flavour platform with
        // Linux relaxes only while nothing listens, and only when *both* sockets
        // carry the flag. That rule already answers the measured established
        // rows correctly: an established child carries its listener's flag, so
        // a reuse-carrying rebind over it passes and a flagless one conflicts.
        | SimulatedUnixFlavour.Linux -> not (existingReuse && candidateReuse) || existingIsListening
        // Darwin relaxes only for addresses that differ, and keys on the
        // *candidate's* flag alone — measured: a wildcard listener that
        // `listen(2)` bound implicitly carries no flag at all, and a later
        // reuse-carrying bind to a specific address on its port still succeeds.
        // The exact-duplicate refusal exempts established sockets (measured
        // above).
        | SimulatedUnixFlavour.Darwin ->
            (existing.Endpoint.Address = candidate.Endpoint.Address
             && not existingIsEstablished)
            || not candidateReuse

    /// Whether `listen(2)` on a socket that is *already bound* asks the port
    /// admission question again, so that a binding admitted earlier can still be
    /// refused a listen.
    ///
    /// The flavours differ, and not merely in strictness. Linux's
    /// `inet_csk_listen_start` calls `get_port` a second time, which is why two
    /// sockets carrying SO_REUSEADDR may share an endpoint right up until one of
    /// them listens; Darwin's `tcp_usr_listen` binds only when the socket has no
    /// port yet, so an already-bound listen consults nothing. Both measured.
    ///
    /// This is not a strictness knob that could be left on for safety. Darwin's
    /// bind rule is asymmetric in SO_REUSEADDR -- it keys on the *candidate's*
    /// flag alone -- so re-asking it at listen time asks with the roles swapped,
    /// and a pair admitted at bind time answers the other way. Re-checking there
    /// would invent an EADDRINUSE, not merely tighten one.
    let listenRescreensBinding (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// Where this platform keeps a socket address's family, and how wide it is.
    /// See `SockaddrFamilyField`, which is also where the reason every other
    /// field's offset is flavour-free is written down.
    let sockaddrFamilyField (platform : SimulatedUnixPlatform) : SockaddrFamilyField =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SockaddrFamilyField.TwoBytesAtOffsetZero
        | SimulatedUnixFlavour.Darwin -> SockaddrFamilyField.OneByteAtOffsetOne

    /// Whether this platform's sockets report IPv4 packet information on a
    /// dual-mode socket — an IPv6 socket receiving IPv4-mapped traffic. Reported
    /// to the guest by `SystemNative_PlatformSupportsDualModeIPv4PacketInfo`.
    ///
    /// A compile-time property of the native shim rather than of any socket, like
    /// `reportsBirthTime`: upstream the whole function body is
    /// `#if HAVE_SUPPORT_FOR_DUAL_MODE_IPV4_PACKET_INFO return 1 #else return 0`,
    /// and `configure.cmake` sets that define to 1 for every Linux target and
    /// leaves it 0 elsewhere. There is no probe of the running kernel involved, so
    /// this is not a fact about the machine but about which shim was built.
    ///
    /// (Linux includes Android here: the `NOT CLR_CMAKE_TARGET_ANDROID` test
    /// nested inside that `if` scopes only a `CMAKE_REQUIRED_LIBRARIES` setting,
    /// not the define.)
    ///
    /// Follows the flavour rather than conservatively reporting `false`
    /// everywhere, because both of CoreLib's readers of it are guest-visible
    /// control flow (see the handler arm for which): answering `false` while
    /// impersonating Linux makes a guest see a `PlatformNotSupportedException`
    /// real Linux does not raise, and does so silently, with no abort and no
    /// diagnostic.
    ///
    /// Answering `true` carries an obligation for whoever implements the socket
    /// emulation this leads on to: a Linux-flavour `recvmsg` on a dual-mode
    /// socket must actually produce the IPv4 `pktinfo` control message, because
    /// CoreLib latches this once per process and will thereafter ask for the
    /// packet information and expect to be given it. Reporting support and then
    /// handing back a default `IPPacketInformation` would be the data-level
    /// version of the lie this function exists to avoid.
    let supportsDualModeIPv4PacketInfo (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The stride of the event buffer `SystemNative_CreateSocketEventBuffer`
    /// allocates and `SystemNative_WaitForSocketEvents` fills, in bytes.
    ///
    /// A compile-time property of the native shim, like `reportsBirthTime`:
    /// `pal_networking.c` defines `SocketEventBufferElementSize` once per backend,
    /// as `max(sizeof(struct epoll_event), sizeof(SocketEvent))` under epoll and
    /// `sizeof(struct kevent)` under kqueue.
    ///
    /// Note what the epoll `max` does, because it is the reason this is a total
    /// function of the flavour where `LinuxEpollLimits.EventSize` is not.
    /// `sizeof(struct epoll_event)` is architecture-dependent — 12 on x86-64 under
    /// `EPOLL_PACKED`, 16 everywhere else — and the `max` against the 16-byte
    /// `SocketEvent` erases exactly that difference, since `max(12, 16)` and
    /// `max(16, 16)` are both 16. So the buffer stride follows the flavour alone,
    /// while the `epoll_wait` constants that skip the `max` do not.
    ///
    /// `sizeof(struct kevent)` is 32 on every 64-bit Darwin:
    /// `{ uintptr_t ident; int16_t filter; uint16_t flags; uint32_t fflags;
    /// intptr_t data; void* udata; }`, measured rather than recalled.
    let socketEventBufferElementSize (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 16
        | SimulatedUnixFlavour.Darwin -> 32

    /// What `fcntl(F_SETFL)` answers on a socket event port — `None` for
    /// success — the `O_NONBLOCK` bit having changed *either way*.
    ///
    /// Measured, not derived: on Linux 6.18.5 the call succeeds and the flag
    /// round-trips; on Darwin (through the real shim's
    /// `SystemNative_FcntlSetIsNonBlocking`, macOS 26) it returns -1 with
    /// ENOTTY and a subsequent `F_GETFL` nevertheless reports the toggled bit,
    /// in both directions. So the caller must store the flag first and then
    /// report this answer.
    ///
    /// The stored bit changes no modelled wait: both `epoll_wait` and `kevent`
    /// take their blocking behaviour from their own timeout argument rather
    /// than from the descriptor's status flags, so
    /// `SystemNative_WaitForSocketEvents` rightly never consults it.
    let eventPortSetStatusFlagsError (platform : SimulatedUnixPlatform) : UnixError option =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> None
        | SimulatedUnixFlavour.Darwin -> Some UnixError.ENOTTY

    /// `AF_INET`, in the platform's own numbering. 2 on both, and on essentially
    /// every Unix — it is one of the handful of `AF_*` values that predate the
    /// BSD/Linux split and never moved.
    ///
    /// Exposed alongside `internetV6AddressFamily` because the `sockaddr`
    /// accessors switch on the raw `sa_family` in the blob rather than on a
    /// converted value: `SystemNative_GetPort` is a `switch (sockAddr->sa_family)`
    /// over exactly these two, and `SystemNative_GetIPv4Address` is an equality
    /// against the first.
    let internetAddressFamily : int = 2

    /// Ports a process may bind only as root.
    ///
    /// Measured as 1024 on both: binding 1023 is `EACCES` for an unprivileged
    /// caller and 1024 succeeds. A constant rather than a function of the
    /// platform because the two agree, and not configuration though Linux does
    /// expose it as `ip_unprivileged_port_start` -- nothing needs to vary it
    /// yet, and a knob with no consumer is a knob no test covers.
    let privilegedPortCeiling : uint16 = 1024us

    /// `AF_INET6`, in the platform's own numbering, which unlike `AF_INET` the two
    /// families disagree about: 10 on Linux against 30 on Darwin. Measured.
    let internetV6AddressFamily (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 10
        | SimulatedUnixFlavour.Darwin -> 30

    /// `struct sockaddr_in` for `endpoint`, as this platform's kernel copies one
    /// out: the family, the port and the address, and on the flavours that have
    /// the field, the `sa_len` byte in front of them.
    ///
    /// The copy-*out* direction specifically. Measured: a Darwin `getsockname`
    /// on a bound socket reports `10 02 ...`, the leading `0x10` being the
    /// 16-byte length, so the kernel fills `sa_len` in even though nothing in a
    /// runtime's shim writes it. `SockaddrFamilyField.OneByteAtOffsetOne`
    /// describes the same byte travelling the other way, where it is a caller's
    /// own store; the two do not disagree.
    ///
    /// Answers the struct's full length for the platform, so a caller bounded by
    /// a shorter declared length truncates what it writes rather than asking for
    /// a shorter blob.
    let encodeInternetSockaddr (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) : byte[] =
        let realLength = (socketAddressSizes platform).InterNetwork
        let blob = Array.zeroCreate<byte> realLength

        BinaryPrimitives.WriteUInt16BigEndian (
            System.Span<byte> (blob, InternetSockaddr.port.Offset, InternetSockaddr.port.Width),
            endpoint.Port
        )

        BinaryPrimitives.WriteUInt32BigEndian (
            System.Span<byte> (blob, InternetSockaddr.address.Offset, InternetSockaddr.address.Width),
            endpoint.Address
        )

        let field = sockaddrFamilyField platform
        let familyOffset = SockaddrFamilyField.offset field

        match SockaddrFamilyField.width field with
        | 1 ->
            blob.[familyOffset] <- byte internetAddressFamily
            // Written only on the flavour that has the field -- on Linux those
            // two bytes are the family itself.
            blob.[0] <- byte realLength
        | _ ->
            BinaryPrimitives.WriteUInt16LittleEndian (
                System.Span<byte> (blob, familyOffset, 2),
                uint16 internetAddressFamily
            )

        blob

    /// The socket shapes both flavours create for an unprivileged process.
    let private portableCreatableSockets : (SocketDomain * SocketKind * SocketProtocol) list =
        [
            SocketDomain.InterNetwork, SocketKind.Stream, SocketProtocol.Unspecified
            SocketDomain.InterNetwork, SocketKind.Stream, SocketProtocol.Tcp
            SocketDomain.InterNetwork, SocketKind.Datagram, SocketProtocol.Unspecified
            SocketDomain.InterNetwork, SocketKind.Datagram, SocketProtocol.Udp
            SocketDomain.InterNetworkV6, SocketKind.Stream, SocketProtocol.Unspecified
            SocketDomain.InterNetworkV6, SocketKind.Stream, SocketProtocol.Tcp
            SocketDomain.InterNetworkV6, SocketKind.Datagram, SocketProtocol.Unspecified
            SocketDomain.InterNetworkV6, SocketKind.Datagram, SocketProtocol.Udp
            SocketDomain.Unix, SocketKind.Stream, SocketProtocol.Unspecified
            SocketDomain.Unix, SocketKind.Datagram, SocketProtocol.Unspecified
        ]

    /// The two Linux adds, and they are the kernel's own divergence rather than
    /// any shim's: Darwin answers `EPROTONOSUPPORT` for both from `socket(2)`,
    /// having passed every screen a caller's runtime could apply.
    let private linuxOnlyCreatableSockets : (SocketDomain * SocketKind * SocketProtocol) list =
        [
            SocketDomain.Unix, SocketKind.Raw, SocketProtocol.Unspecified
            SocketDomain.Unix, SocketKind.SeqPacket, SocketProtocol.Unspecified
        ]

    let private linuxCreatableSockets : Set<SocketDomain * SocketKind * SocketProtocol> =
        Set.ofList (portableCreatableSockets @ linuxOnlyCreatableSockets)

    let private darwinCreatableSockets : Set<SocketDomain * SocketKind * SocketProtocol> =
        Set.ofList portableCreatableSockets

    /// Every socket shape this emulated kernel creates, under `platform`. A
    /// `socket(2)` for anything else is refused rather than answered.
    ///
    /// This is the kernel's declared protocol table, and it is deliberately
    /// smaller than what the platform would really create. The rows outside it
    /// are absent for three different reasons — some are privilege-dependent
    /// (every raw and packet socket: measured, 70 Linux rows change answer
    /// between euid 1000 and euid 0), some sysctl-dependent (Linux's ping
    /// sockets, gated by `net.ipv4.ping_group_range`), and some deterministic
    /// but simply not modelled. A shape outside this set is a socket PawPrint
    /// has not decided how to be, and refusing leaves that decision open where
    /// a guessed errno would not.
    ///
    /// Exposed as data rather than as a predicate because the set is the fact:
    /// a caller deciding whether to create one wants to ask, and a reader
    /// wanting to know what this kernel is wants to enumerate.
    let creatableSockets (platform : SimulatedUnixPlatform) : Set<SocketDomain * SocketKind * SocketProtocol> =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> linuxCreatableSockets
        | SimulatedUnixFlavour.Darwin -> darwinCreatableSockets
