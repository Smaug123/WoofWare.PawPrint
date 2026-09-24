namespace WoofWare.PosixKernel

/// The filesystem an emulated mount claims to be, as `fstatfs(2)` reports it.
///
/// A *choice* rather than a measured fact, because this library's filesystem
/// is an in-memory graph that is not any real filesystem. That is why it is
/// something a client configures rather than a derivation from the flavour the
/// way the errno numbering is: a single Linux reports `0xEF53`,
/// `0x01021994` and `0x9FA0` for three directories in one process, so a flavour
/// does not determine a mount's type. It does *constrain* it, which is what
/// `EmulatedFileSystemType.isReportableUnder` carries.
///
/// This changes what `fstatfs(2)` reports for a file, a directory's `st_size`,
/// and where `lseek(2)` with `SEEK_END` lands on a directory. Path resolution keeps its flavour's limits either way — `pathLimits`
/// carries `NameLengthLimit` as an ext4-versus-APFS fact — so a kernel
/// configured `Nfs` reports NFS while still resolving names as its flavour
/// does.
///
/// Only three cases, because only three have a consumer. Note that a fourth
/// could not be told apart as `Ext4` by `fstatfs(2)` alone: Linux reports
/// `0xEF53` for ext2, ext3 and ext4 alike.
[<RequireQualifiedAccess>]
type EmulatedFileSystemType =
    /// Linux's in-memory filesystem, and so the honest analogue of a
    /// filesystem that only ever exists in memory.
    | Tmpfs
    /// What a macOS file is on. Darwin's answer, since a default macOS mounts
    /// no tmpfs.
    | Apfs
    /// A network filesystem, which both flavours can mount. Code that treats
    /// network mounts specially (for example, by declining to take a `flock`
    /// on one) sees one under this configuration.
    | Nfs

/// The fields of `struct statfs` that name the type of filesystem a
/// descriptor is on, as one flavour's `fstatfs(2)` fills them in.
///
/// The flavours name a filesystem differently, so each case carries its own
/// flavour's fields and nothing else.
[<RequireQualifiedAccess>]
type FileSystemTypeFields =
    /// Linux's `f_type`: the filesystem's magic number, such as `0x01021994`
    /// for tmpfs. The field is a `__fsword_t`, which is a `long` on the 64-bit
    /// Linuxes modelled.
    | Linux of fType : int64
    /// Darwin's `f_type` and `f_fstypename`. The `f_type` is the number the
    /// kernel gave the filesystem when it registered it, not a magic number
    /// taken from the filesystem's format; the `f_fstypename` is its name,
    /// such as `apfs`.
    | Darwin of fType : uint32 * fsTypeName : UnixByteString

/// What `fstatfs(2)` does when asked about one descriptor.
[<RequireQualifiedAccess>]
type FileSystemTypeAnswer =
    /// `fstatfs` succeeded, naming the filesystem with these fields.
    | Reported of fields : FileSystemTypeFields
    /// `fstatfs` failed with this errno.
    | Failed of error : UnixError

[<RequireQualifiedAccess>]
module EmulatedFileSystemType =
    /// The type-naming fields `fstatfs(2)` reports for a file on a mount of
    /// this type, under a kernel of this flavour.
    ///
    /// Refuses a `flavour` and `fsType` that do not describe one machine (see
    /// `isReportableUnder`).
    let fieldsFor (flavour : SimulatedUnixFlavour) (fsType : EmulatedFileSystemType) : FileSystemTypeFields =
        let darwinName (name : string) : UnixByteString =
            match UnixByteString.ofString name with
            | Ok name -> name
            | Error defect ->
                failwith
                    $"EmulatedFileSystemType.fieldsFor: the filesystem name %s{name} is not a Unix string (%O{defect}) (this is a bug in this library)"

        match flavour, fsType with
        // Linux's magic numbers, `<linux/magic.h>`. Tmpfs measured on
        // `/dev/shm` (Linux 6.18.5); NFS is `NFS_SUPER_MAGIC`, which no local
        // NFS mount was available to measure.
        | SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Tmpfs -> FileSystemTypeFields.Linux 0x01021994L
        | SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Nfs -> FileSystemTypeFields.Linux 0x6969L
        // Measured 2026-09-23 on macOS 26.6 (Darwin 25.6.0, arm64): `fstatfs`
        // on a file and a directory of every APFS volume mounted (`/`,
        // `/System/Volumes/Data`, `/private/tmp`, a home directory) reported
        // `f_type` 0x1A and `f_fstypename` "apfs", before and after a reboot,
        // and `/nix`'s volume too the second time. Darwin hands out type
        // numbers from 24 upwards in the order filesystems register, unless
        // the filesystem asks for a fixed one (`vfs_fsadd` in XNU's
        // `bsd/vfs/kpi_vfs.c`), and APFS's source is not published, so 0x1A is
        // a measurement of that release rather than a constant of the format.
        | SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Apfs ->
            FileSystemTypeFields.Darwin (0x1Au, darwinName "apfs")
        // NFS asks for a fixed number: `install_nfs_vfs_fs` in the NFS kext
        // (apple-oss-distributions/NFS, `kext/nfs_vfsops.c`) registers
        // `vfe_fstypenum = VT_NFS`, which is 2, under the name "nfs", and sets
        // no name override. `vfs_get_statfs64` (`bsd/vfs/vfs_syscalls.c`,
        // which `fstatfs64` answers from, and arm64's `fstatfs` is the same
        // libSystem symbol) copies both into `struct statfs`. Both read at
        // xnu `main` and NFS `main` on 2026-09-23. Measured the same day on macOS 26.6 by
        // `getvfsbyname("nfs")`, which reported type number 2 for the loaded
        // kext. No NFS mount was available to call `fstatfs` on.
        | SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Nfs -> FileSystemTypeFields.Darwin (2u, darwinName "nfs")
        | SimulatedUnixFlavour.Linux, EmulatedFileSystemType.Apfs
        | SimulatedUnixFlavour.Darwin, EmulatedFileSystemType.Tmpfs ->
            failwith
                $"EmulatedFileSystemType.fieldsFor: asked what a %O{flavour} kernel reports for a %O{fsType} mount, which %O{flavour} cannot have. The flavour and the mount type have come apart; they constrain each other (see EmulatedFileSystemType.isReportableUnder) and must be chosen together rather than set one at a time."

    /// A directory's `st_size` on a mount of this type, given how many names it
    /// holds besides `.` and `..`, or `None` where the mount's type does not
    /// determine it.
    ///
    /// NFS is the `None`: an NFS client reports whatever the server's own
    /// filesystem says, and nothing about this machine says what that is.
    let directorySize (fsType : EmulatedFileSystemType) (entries : int) : int64 option =
        System.Diagnostics.Debug.Assert (entries >= 0, "directorySize: a directory cannot hold fewer than no names")

        // Measured 2026-09-23 by a history probe run from an empty directory:
        // 300 regular files created one at a time with names of 1 to 255
        // bytes, 150 of them removed at random, then 600 random steps of
        // creat, mkdir, symlink, link, mkfifo, unlink, rmdir, and rename
        // within, into, out of and over the directory, then drained; twelve
        // seeds per filesystem, about 14,500 observations each, `stat` and
        // `fstat` agreeing on every one. Separately, 5000 files created and
        // removed one at a time, and a subdirectory holding 50 files. The
        // size is affine in the number of names alone: no step's name length,
        // entry kind, or history moved it, and neither did the contents of a
        // subdirectory.
        match fsType with
        // Linux 6.18.5 on `/dev/shm`: two 20-byte "entries" for `.` and `..`,
        // and 20 more per name.
        | EmulatedFileSystemType.Tmpfs -> Some (40L + 20L * int64 entries)
        // macOS 26.6 (Darwin 25.6.0) on the APFS volume holding `/tmp`: 32
        // bytes per name, `.` and `..` included.
        | EmulatedFileSystemType.Apfs -> Some (64L + 32L * int64 entries)
        // Not measured: no NFS server or mount was available. The answer
        // comes from the server's GETATTR, so it is the size the *server's*
        // filesystem gives the directory: a small one is 4096 bytes on a local
        // ext4, and would be the rule above on an exported tmpfs.
        | EmulatedFileSystemType.Nfs -> None

    /// The type a mount reports when a host expresses no preference.
    ///
    /// `Tmpfs` under Linux because this library's filesystem really is in
    /// memory, and `Apfs` under Darwin because a default macOS mounts no tmpfs.
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
        // Measured: `/dev/shm` reports it. A default macOS mounts no tmpfs,
        // and no Darwin tmpfs has been measured. Darwin can have one, though:
        // macOS 26.6 registers a "tmpfs" (type number 28 from
        // `getvfsbyname`) and ships `mount_tmpfs`, which refused uid 501 with
        // EPERM on 2026-09-23.
        | EmulatedFileSystemType.Tmpfs, SimulatedUnixFlavour.Linux -> true
        | EmulatedFileSystemType.Tmpfs, SimulatedUnixFlavour.Darwin -> false
        // No mainline Linux filesystem reports `0x1A`; a FUSE-mounted APFS
        // reports fuse's own `0x65735546`.
        | EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Linux -> false
        | EmulatedFileSystemType.Apfs, SimulatedUnixFlavour.Darwin -> true
        // Both mount NFS.
        | EmulatedFileSystemType.Nfs, SimulatedUnixFlavour.Linux
        | EmulatedFileSystemType.Nfs, SimulatedUnixFlavour.Darwin -> true

    /// What `fstatfs(2)` answers about one descriptor: `None` for an fd the
    /// process does not hold.
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
        // for its files. `UnixSystem.initial` derives the type from the
        // flavour and `UnixMachineState.withFileSystemType` refuses one the
        // flavour cannot mount, but no client can be made to go through
        // either: a state record assembled field by field bypasses both.
        // Checking here rather than trusting the caller is what keeps this
        // function's contract true wherever it is reached.
        if not (isReportableUnder flavour mount) then
            failwith
                $"EmulatedFileSystemType.reportedFor: asked what a %O{flavour} kernel reports for a %O{mount} mount, which %O{flavour} cannot have. The flavour and the mount type have come apart; they constrain each other (see EmulatedFileSystemType.isReportableUnder) and must be chosen together rather than set one at a time."

        /// Darwin's `fstatfs` refuses every object that is not on a
        /// filesystem, uniformly; Linux's succeeds and names the
        /// pseudo-filesystem the object lives on. So each of these rows is a
        /// measured number rather than an invention — unlike `fstat`, which
        /// refuses the same descriptors because it owes them seventeen fields
        /// and the platforms agree on none of them.
        let pseudoFileSystem (linux : int64) : FileSystemTypeAnswer =
            match flavour with
            | SimulatedUnixFlavour.Linux -> FileSystemTypeAnswer.Reported (FileSystemTypeFields.Linux linux)
            | SimulatedUnixFlavour.Darwin -> FileSystemTypeAnswer.Failed UnixError.EINVAL

        match target with
        | None -> FileSystemTypeAnswer.Failed UnixError.EBADF
        // Regular files and directories alike: measured identical, and one
        // mount has one answer.
        | Some (OpenFileObject.File _) -> FileSystemTypeAnswer.Reported (fieldsFor flavour mount)
        // This library models the standard streams as pipes (see
        // `FileDescriptorRegistry.initial`), so this row is a consequence of
        // that existing decision rather than a new one: Linux's `pipefs`.
        | Some (OpenFileObject.StandardStream _) -> pseudoFileSystem 0x50495045L
        // Linux's `sockfs`.
        | Some (OpenFileObject.Socket _) -> pseudoFileSystem 0x534F434BL
        // Linux's `anon_inodefs`, which is where an epoll port lives — and
        // exactly the granularity this answer needs, which is why
        // `OpenFileObject` folding every anonymous object into one case costs
        // nothing here.
        | Some OpenFileObject.AnonymousInode -> pseudoFileSystem 0x09041934L
