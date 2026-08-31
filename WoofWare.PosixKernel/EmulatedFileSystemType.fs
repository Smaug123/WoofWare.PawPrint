namespace WoofWare.PosixKernel

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
