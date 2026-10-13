namespace WoofWare.PosixKernel

/// One of the filesystems a Linux kernel keeps for objects that are not on any
/// mount, which `fstatfs(2)` reports for a descriptor naming such an object.
[<RequireQualifiedAccess>]
type internal PseudoFileSystem =
    /// `pipefs`, where both ends of a pipe live.
    | Pipe
    /// `sockfs`, where every socket lives.
    | Socket
    /// `anon_inodefs`, where an epoll instance, an inotify instance and an
    /// eventfd live.
    | AnonymousInode

/// Why this kernel does not state a filesystem's block size and name limit.
[<RequireQualifiedAccess>]
type internal GeometryRefusal =
    /// The filesystem is an NFS mount, whose block size and name limit come
    /// from the server and the mount's negotiation with it.
    | Nfs

[<RequireQualifiedAccess>]
module internal GeometryRefusal =
    /// Human-readable description.
    let describe (refusal : GeometryRefusal) : string =
        match refusal with
        | GeometryRefusal.Nfs ->
            "the filesystem is an NFS mount, whose block size and name limit are whatever the server and the mount's negotiation with it decided, which nothing in this machine determines."

    /// Which exception a public entry point raises this refusal as.
    let kind (refusal : GeometryRefusal) : RefusalKind =
        match refusal with
        | GeometryRefusal.Nfs -> RefusalKind.Unmodelled

/// Why this kernel does not state a filesystem's block and file counts.
[<RequireQualifiedAccess>]
type internal CapacityRefusal =
    /// The filesystem is an APFS volume, whose counts are those of the whole
    /// container it shares with other volumes.
    | Apfs
    /// The filesystem is an NFS mount, whose counts are the server's.
    | Nfs
    /// The filesystem is the device filesystem, whose file count counts every
    /// node a real one holds, and whose block counts are a share of the
    /// machine's memory.
    | DeviceFileSystem

[<RequireQualifiedAccess>]
module internal CapacityRefusal =
    /// Human-readable description.
    let describe (refusal : CapacityRefusal) : string =
        match refusal with
        | CapacityRefusal.Apfs ->
            "the filesystem is an APFS volume, whose block and file counts are those of the whole container, shared with every other volume in it and moved by every process's writes. Measured, they are not even deterministic on a container nothing else writes to, so no configured value could be one a real volume reports while the process writes."
        | CapacityRefusal.Nfs ->
            "the filesystem is an NFS mount, whose block and file counts are the server's, which nothing in this machine determines."
        | CapacityRefusal.DeviceFileSystem ->
            "the filesystem is the device filesystem. Its free-file count is its inode limit less every node a real one holds, and this kernel's holds only the nodes of the devices it has drivers for; its block counts are a share of the machine's memory, which this machine does not describe."

    /// Which exception a public entry point raises this refusal as.
    let kind (refusal : CapacityRefusal) : RefusalKind =
        match refusal with
        | CapacityRefusal.Apfs -> RefusalKind.Unmodelled
        | CapacityRefusal.Nfs -> RefusalKind.Unmodelled
        | CapacityRefusal.DeviceFileSystem -> RefusalKind.Unmodelled

/// Why this kernel does not state a filesystem's `f_fsid`.
[<RequireQualifiedAccess>]
type internal FileSystemIdRefusal =
    /// The descriptor names an object on one of Linux's internal
    /// filesystems, whose `f_fsid` is a device number the kernel assigned
    /// when it booted.
    | PseudoFileSystemDevice of fileSystem : PseudoFileSystem
    /// The filesystem is an NFS mount.
    | Nfs

[<RequireQualifiedAccess>]
module internal FileSystemIdRefusal =
    /// Human-readable description.
    let describe (refusal : FileSystemIdRefusal) : string =
        match refusal with
        | FileSystemIdRefusal.PseudoFileSystemDevice fileSystem ->
            $"the descriptor names an object on Linux's internal %O{fileSystem} filesystem, whose f_fsid is the device number the kernel gave that filesystem at boot. It differs between kernel images (pipefs is 0xc on one measured 6.18.5 aarch64 kernel and 0xf on a 6.12.107 x86-64 one), so this kernel will not state one."
        | FileSystemIdRefusal.Nfs -> "the filesystem is an NFS mount, whose f_fsid has not been measured."

    /// Which exception a public entry point raises this refusal as.
    let kind (refusal : FileSystemIdRefusal) : RefusalKind =
        match refusal with
        | FileSystemIdRefusal.PseudoFileSystemDevice _ -> RefusalKind.Unmodelled
        | FileSystemIdRefusal.Nfs -> RefusalKind.Unmodelled

/// Why this kernel does not state what a filesystem's mount options and
/// mount-point fields are.
[<RequireQualifiedAccess>]
type internal MountFieldsRefusal =
    /// The filesystem is an NFS mount, whose options have not been measured.
    | Nfs

[<RequireQualifiedAccess>]
module internal MountFieldsRefusal =
    /// Human-readable description.
    let describe (refusal : MountFieldsRefusal) : string =
        match refusal with
        | MountFieldsRefusal.Nfs ->
            "the filesystem is an NFS mount, whose mount options and mount-point fields have not been measured."

    /// Which exception a public entry point raises this refusal as.
    let kind (refusal : MountFieldsRefusal) : RefusalKind =
        match refusal with
        | MountFieldsRefusal.Nfs -> RefusalKind.Unmodelled

/// The block and file counts `statfs(2)` reports, the same five on both
/// flavours.
type FileSystemCapacity =
    {
        /// `f_blocks`.
        Blocks : uint64
        /// `f_bfree`.
        FreeBlocks : uint64
        /// `f_bavail`.
        AvailableBlocks : uint64
        /// `f_files`.
        Files : uint64
        /// `f_ffree`.
        FreeFiles : uint64
    }

[<RequireQualifiedAccess>]
module FileSystemCapacity =
    /// Every count 0: what a filesystem with no limits reports, such as a
    /// tmpfs mounted with `size=0,nr_inodes=0`, or one of Linux's internal
    /// filesystems.
    let unlimited : FileSystemCapacity =
        {
            Blocks = 0UL
            FreeBlocks = 0UL
            AvailableBlocks = 0UL
            Files = 0UL
            FreeFiles = 0UL
        }

/// The sizes Linux's `statfs(2)` reports.
type LinuxFileSystemGeometry =
    {
        /// `f_bsize`.
        BlockSize : int64
        /// `f_frsize`.
        FragmentSize : int64
        /// `f_namelen`.
        NameLengthLimit : int64
    }

/// Linux's `struct statfs`, in groups of fields. This kernel may decline to
/// state a group, and reading such a group throws `UnmodelledException`.
/// `f_spare` is not carried: it is 0.
type LinuxFileSystemStatistics =
    internal
        {
            TypeValue : int64
            GeometryResult : Result<LinuxFileSystemGeometry, GeometryRefusal>
            CapacityResult : Result<FileSystemCapacity, CapacityRefusal>
            FileSystemIdResult : Result<FileSystemId, FileSystemIdRefusal>
            FlagsResult : Result<int64, MountFieldsRefusal>
        }

    /// `f_type`: the filesystem's magic number.
    member this.Type : int64 = this.TypeValue

    /// `f_bsize`, `f_frsize` and `f_namelen`. Throws `UnmodelledException`
    /// where this kernel does not state them.
    member this.Geometry : LinuxFileSystemGeometry =
        this.GeometryResult
        |> Refusal.orRaise "LinuxFileSystemStatistics.Geometry" GeometryRefusal.kind GeometryRefusal.describe

    /// `f_blocks`, `f_bfree`, `f_bavail`, `f_files` and `f_ffree`. Throws
    /// `UnmodelledException` where this kernel does not state them.
    member this.Capacity : FileSystemCapacity =
        this.CapacityResult
        |> Refusal.orRaise "LinuxFileSystemStatistics.Capacity" CapacityRefusal.kind CapacityRefusal.describe

    /// `f_fsid`. Throws `UnmodelledException` where this kernel does not state
    /// it.
    member this.FileSystemId : FileSystemId =
        this.FileSystemIdResult
        |> Refusal.orRaise
            "LinuxFileSystemStatistics.FileSystemId"
            FileSystemIdRefusal.kind
            FileSystemIdRefusal.describe

    /// `f_flags`: the `ST_*` bits, in Linux's numbering. Throws
    /// `UnmodelledException` where this kernel does not state them.
    member this.Flags : int64 =
        this.FlagsResult
        |> Refusal.orRaise "LinuxFileSystemStatistics.Flags" MountFieldsRefusal.kind MountFieldsRefusal.describe

/// The sizes Darwin's `statfs(2)` reports.
type DarwinFileSystemGeometry =
    {
        /// `f_bsize`.
        BlockSize : uint32
        /// `f_iosize`.
        IoSize : int32
    }

/// The fields of Darwin's `statfs(2)` that describe how and where the
/// filesystem is mounted.
type DarwinMountFields =
    {
        /// `f_owner`.
        Owner : uint32
        /// `f_flags`: the `MNT_*` bits, in Darwin's numbering.
        Flags : uint32
        /// `f_flags_ext`.
        ExtendedFlags : uint32
        /// `f_fssubtype`. For APFS, 0 means names compare case-sensitively and 1
        /// that they do not.
        SubType : uint32
        /// `f_mntonname`.
        MountedOn : UnixByteString
        /// `f_mntfromname`.
        MountedFrom : UnixByteString
    }

/// Darwin's `struct statfs`, in groups of fields. This kernel may decline to
/// state a group, and reading such a group throws `UnmodelledException`.
/// `f_reserved` is not carried: it is 0.
type DarwinFileSystemStatistics =
    internal
        {
            TypeValue : uint32
            TypeNameValue : UnixByteString
            GeometryResult : Result<DarwinFileSystemGeometry, GeometryRefusal>
            CapacityResult : Result<FileSystemCapacity, CapacityRefusal>
            FileSystemIdResult : Result<FileSystemId, FileSystemIdRefusal>
            MountResult : Result<DarwinMountFields, MountFieldsRefusal>
        }

    /// `f_type`: the number the kernel gave the filesystem when it registered
    /// it.
    member this.Type : uint32 = this.TypeValue

    /// `f_fstypename`.
    member this.TypeName : UnixByteString = this.TypeNameValue

    /// `f_bsize` and `f_iosize`. Throws `UnmodelledException` where this
    /// kernel does not state them.
    member this.Geometry : DarwinFileSystemGeometry =
        this.GeometryResult
        |> Refusal.orRaise "DarwinFileSystemStatistics.Geometry" GeometryRefusal.kind GeometryRefusal.describe

    /// `f_blocks`, `f_bfree`, `f_bavail`, `f_files` and `f_ffree`. Throws
    /// `UnmodelledException` where this kernel does not state them.
    member this.Capacity : FileSystemCapacity =
        this.CapacityResult
        |> Refusal.orRaise "DarwinFileSystemStatistics.Capacity" CapacityRefusal.kind CapacityRefusal.describe

    /// `f_fsid`. Throws `UnmodelledException` where this kernel does not state
    /// it.
    member this.FileSystemId : FileSystemId =
        this.FileSystemIdResult
        |> Refusal.orRaise
            "DarwinFileSystemStatistics.FileSystemId"
            FileSystemIdRefusal.kind
            FileSystemIdRefusal.describe

    /// `f_owner`, `f_flags`, `f_flags_ext`, `f_fssubtype`, `f_mntonname` and
    /// `f_mntfromname`. Throws `UnmodelledException` where this kernel does
    /// not state them.
    member this.Mount : DarwinMountFields =
        this.MountResult
        |> Refusal.orRaise "DarwinFileSystemStatistics.Mount" MountFieldsRefusal.kind MountFieldsRefusal.describe

/// What a successful `statfs(2)` or `fstatfs(2)` reports, in the flavour's own
/// struct.
[<RequireQualifiedAccess>]
type FileSystemStatistics =
    /// Linux's `struct statfs`.
    | Linux of LinuxFileSystemStatistics
    /// Darwin's `struct statfs`.
    | Darwin of DarwinFileSystemStatistics

/// What `statfs(2)` or `fstatfs(2)` does.
[<RequireQualifiedAccess>]
type FileSystemStatisticsAnswer =
    /// The call succeeded and reported these fields.
    | Reported of statistics : FileSystemStatistics
    /// The call failed with this errno.
    | Failed of error : UnixError

[<RequireQualifiedAccess>]
module FileSystemStatistics =

    /// The fields that name the filesystem's type, which this kernel always
    /// states.
    let typeFields (statistics : FileSystemStatistics) : FileSystemTypeFields =
        match statistics with
        | FileSystemStatistics.Linux linux -> FileSystemTypeFields.Linux linux.Type
        | FileSystemStatistics.Darwin darwin -> FileSystemTypeFields.Darwin (darwin.Type, darwin.TypeName)

    // Linux's `ST_*` bits, as `f_flags` reports them.
    let private linuxValid : int64 = 0x20L
    let private linuxNoAccessTime : int64 = 0x400L

    // Darwin's `MNT_*` bits, from `<sys/mount.h>`.
    let private darwinLocal : uint32 = 0x1000u
    let private darwinRootFileSystem : uint32 = 0x4000u
    let private darwinVolumeFileSystem : uint32 = 0x8000u
    let private darwinJournaled : uint32 = 0x800000u
    let private darwinMultiLabel : uint32 = 0x4000000u
    let private darwinNoAccessTime : uint32 = 0x10000000u

    let private unixString (context : string) (text : string) : UnixByteString =
        match UnixByteString.ofString text with
        | Ok text -> text
        | Error defect ->
            failwith
                $"FileSystemStatistics.%s{context}: %s{text} is not a Unix string (%O{defect}) (this is a bug in this library)"

    /// What `statfs(2)` reports for any file or directory on the machine's one
    /// mount, under a kernel of this platform.
    ///
    /// Refuses a platform and mount that do not describe one machine (see
    /// `EmulatedFileSystemType.isReportableUnder`).
    let ofMount (platform : SimulatedUnixPlatform) (mount : EmulatedMount) : FileSystemStatistics =
        let flavour = SimulatedUnixPlatform.flavour platform

        let pageSize =
            int64 (SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform))

        match EmulatedFileSystemType.fieldsFor flavour (EmulatedMount.fileSystemType mount), mount with
        | FileSystemTypeFields.Linux fType, EmulatedMount.Tmpfs tmpfs ->
            // Measured 2026-09-26 on Linux 6.18.5 aarch64 and 6.12.107 x86-64,
            // eleven tmpfs mounts; the table is in the header of
            // `docs/plans/2026-08-23-posix-kernel-extraction/statfs-fields.c`.
            FileSystemStatistics.Linux
                {
                    TypeValue = fType
                    // `f_bsize` is the page size, and `f_frsize` equals it.
                    GeometryResult =
                        Ok
                            {
                                BlockSize = pageSize
                                FragmentSize = pageSize
                                NameLengthLimit = 255L
                            }
                    // This filesystem never refuses a write for space, which is
                    // a tmpfs with no limits, and such a tmpfs reports every
                    // count as 0 however full it is.
                    CapacityResult = Ok FileSystemCapacity.unlimited
                    FileSystemIdResult = Ok tmpfs.FileSystemId
                    // `ST_VALID` is on every mount. `ST_NOATIME` because nothing
                    // in this kernel moves an access time, which is what a
                    // `noatime` mount does; tmpfs's own default is `relatime`,
                    // which this kernel does not implement.
                    FlagsResult = Ok (linuxValid ||| linuxNoAccessTime)
                }
        | FileSystemTypeFields.Darwin (fType, name), EmulatedMount.Apfs apfs ->
            // Measured 2026-09-26 on Darwin 27.0.0, nine APFS volumes in three
            // containers; the table is in the header of `statfs-fields.c`.
            FileSystemStatistics.Darwin
                {
                    TypeValue = fType
                    TypeNameValue = name
                    GeometryResult =
                        Ok
                            {
                                BlockSize = 4096u
                                IoSize = apfs.IoSize
                            }
                    CapacityResult = Error CapacityRefusal.Apfs
                    // Darwin's `f_fsid` is the volume's `st_dev` and its type
                    // number, measured on every volume. The `st_dev` is the one
                    // `stat(2)` reports for every inode here.
                    FileSystemIdResult =
                        Ok
                            {
                                First = int32 VirtualFileSystem.deviceId
                                Second = int32 fType
                            }
                    MountResult =
                        Ok
                            {
                                Owner = apfs.Owner
                                // Local, journaled, volfs and multilabel were set on
                                // every APFS volume measured. Root, because this
                                // filesystem is mounted at `/`. No-atime, because
                                // nothing in this kernel moves an access time.
                                Flags =
                                    darwinLocal
                                    ||| darwinJournaled
                                    ||| darwinVolumeFileSystem
                                    ||| darwinMultiLabel
                                    ||| darwinRootFileSystem
                                    ||| darwinNoAccessTime
                                // 0 on every volume measured but the root data
                                // volume, which this filesystem is not.
                                ExtendedFlags = 0u
                                // Case-sensitive, because this filesystem compares
                                // names byte for byte. A Mac's own volumes report 1.
                                SubType = 0u
                                MountedOn = unixString "ofMount" "/"
                                MountedFrom = apfs.MountedFrom
                            }
                }
        | FileSystemTypeFields.Linux fType, EmulatedMount.Nfs ->
            FileSystemStatistics.Linux
                {
                    TypeValue = fType
                    GeometryResult = Error GeometryRefusal.Nfs
                    CapacityResult = Error CapacityRefusal.Nfs
                    FileSystemIdResult = Error FileSystemIdRefusal.Nfs
                    FlagsResult = Error MountFieldsRefusal.Nfs
                }
        | FileSystemTypeFields.Darwin (fType, name), EmulatedMount.Nfs ->
            FileSystemStatistics.Darwin
                {
                    TypeValue = fType
                    TypeNameValue = name
                    GeometryResult = Error GeometryRefusal.Nfs
                    CapacityResult = Error CapacityRefusal.Nfs
                    FileSystemIdResult = Error FileSystemIdRefusal.Nfs
                    MountResult = Error MountFieldsRefusal.Nfs
                }
        | FileSystemTypeFields.Linux _, EmulatedMount.Apfs _
        | FileSystemTypeFields.Darwin _, EmulatedMount.Tmpfs _ ->
            failwith
                $"FileSystemStatistics.ofMount: the type fields for a %O{mount} mount are not %O{flavour}'s, which EmulatedFileSystemType.fieldsFor should have refused (this is a bug in this library)"

    /// What `statfs(2)` reports for anything on the device filesystem `mount`,
    /// under a kernel of this platform.
    ///
    /// Refuses a device filesystem this platform cannot mount, and Darwin's
    /// devfs, which this kernel does not model and no path reaches.
    let internal ofDeviceMount
        (platform : SimulatedUnixPlatform)
        (mount : DeviceFileSystemMount)
        : FileSystemStatistics
        =
        let flavour = SimulatedUnixPlatform.flavour platform

        match mount with
        | DeviceFileSystemMount.Devfs ->
            failwith
                "FileSystemStatistics.ofDeviceMount: asked what Darwin's devfs reports, which this kernel does not model and which no path or descriptor reaches (this is a bug in the caller)."
        | DeviceFileSystemMount.Devtmpfs devtmpfs ->

        if not (DeviceFileSystemMount.isMountableUnder flavour mount) then
            failwith
                $"FileSystemStatistics.ofDeviceMount: a %O{flavour} kernel cannot mount a devtmpfs (this is a bug in the caller)."

        let pageSize =
            int64 (SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform))

        // Measured 2026-10-02 on `/dev` in a Linux 6.18.5 aarch64 VM
        // (`devices.c`, STATFS rows): devtmpfs is tmpfs underneath and reports
        // tmpfs's magic, the page size as both block sizes, and a 255-byte name
        // limit, through a node and through `/dev` alike.
        FileSystemStatistics.Linux
            {
                TypeValue = 0x01021994L
                GeometryResult =
                    Ok
                        {
                            BlockSize = pageSize
                            FragmentSize = pageSize
                            NameLengthLimit = 255L
                        }
                CapacityResult = Error CapacityRefusal.DeviceFileSystem
                FileSystemIdResult = Ok devtmpfs.FileSystemId
                FlagsResult = Ok devtmpfs.Flags
            }

    /// Throws unless a kernel of this platform could have this mount (see
    /// `EmulatedFileSystemType.isReportableUnder`), naming `context` as the
    /// caller that was handed the pair.
    ///
    /// `UnixSystem.initial` and `UnixBootImage.withMount` keep the pair
    /// coherent, but a machine record assembled field by field bypasses both.
    let internal assertCoherent (context : string) (platform : SimulatedUnixPlatform) (mount : EmulatedMount) : unit =
        let flavour = SimulatedUnixPlatform.flavour platform
        let fsType = EmulatedMount.fileSystemType mount

        if not (EmulatedFileSystemType.isReportableUnder flavour fsType) then
            failwith
                $"%s{context}: asked what a %O{flavour} kernel reports on a %O{fsType} mount, which %O{flavour} cannot have. The flavour and the mount have come apart; they constrain each other (see EmulatedFileSystemType.isReportableUnder) and must be chosen together rather than set one at a time."

    /// What `fstatfs(2)` reports for a descriptor naming `target`, under a
    /// kernel of this platform whose root filesystem is `mount`.
    ///
    /// A file or directory gets the root filesystem's answer: `target` does not
    /// say which filesystem its inode is on, so this does not answer for one on
    /// the device filesystem. `UnixPathResolution.fstatfs` and `statfs` do.
    ///
    /// Refuses a platform and mount that do not describe one machine (see
    /// `EmulatedFileSystemType.isReportableUnder`), whatever the object.
    let ofObject
        (platform : SimulatedUnixPlatform)
        (mount : EmulatedMount)
        (target : OpenFileObject)
        : FileSystemStatisticsAnswer
        =
        // Checked before the object is looked at: an object not on the mount
        // is answered from the flavour alone, so a pipe on a machine that is
        // Linux for its files and macOS for its pipes would otherwise get a
        // quietly wrong answer rather than a loud one.
        assertCoherent "FileSystemStatistics.ofObject" platform mount
        let flavour = SimulatedUnixPlatform.flavour platform

        let pseudoFileSystem (fileSystem : PseudoFileSystem) : FileSystemStatisticsAnswer =
            match flavour with
            // Measured: Darwin refuses every object not on a mount, uniformly.
            | SimulatedUnixFlavour.Darwin -> FileSystemStatisticsAnswer.Failed UnixError.EINVAL
            | SimulatedUnixFlavour.Linux ->
                let pageSize =
                    int64 (SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform))

                let magic =
                    match fileSystem with
                    | PseudoFileSystem.Pipe -> 0x50495045L
                    | PseudoFileSystem.Socket -> 0x534F434BL
                    | PseudoFileSystem.AnonymousInode -> 0x09041934L

                // Measured on both Linux kernels, for both pipe ends, three
                // kinds of socket, an epoll instance and an eventfd: every field but
                // the fsid is a fact of the filesystem's type.
                FileSystemStatistics.Linux
                    {
                        TypeValue = magic
                        GeometryResult =
                            Ok
                                {
                                    BlockSize = pageSize
                                    FragmentSize = pageSize
                                    NameLengthLimit = 255L
                                }
                        CapacityResult = Ok FileSystemCapacity.unlimited
                        FileSystemIdResult = Error (FileSystemIdRefusal.PseudoFileSystemDevice fileSystem)
                        // No options: these filesystems are mounted by the
                        // kernel, not by anybody's `mount(2)`.
                        FlagsResult = Ok linuxValid
                    }
                |> FileSystemStatisticsAnswer.Reported

        match target with
        // Regular files and directories alike: the root filesystem has one
        // answer.
        | OpenFileObject.File _ -> FileSystemStatisticsAnswer.Reported (ofMount platform mount)
        | OpenFileObject.Pipe _ -> pseudoFileSystem PseudoFileSystem.Pipe
        | OpenFileObject.Socket _ -> pseudoFileSystem PseudoFileSystem.Socket
        // An epoll instance or an inotify instance. `OpenFileObject` folding
        // every anonymous object into one case costs nothing here: they share
        // one filesystem.
        | OpenFileObject.AnonymousInode -> pseudoFileSystem PseudoFileSystem.AnonymousInode
        | OpenFileObject.Kqueue _ ->
            match flavour with
            // Measured (`statfs-fields.c`): EINVAL, as for every Darwin object
            // not on a mount.
            | SimulatedUnixFlavour.Darwin -> FileSystemStatisticsAnswer.Failed UnixError.EINVAL
            | SimulatedUnixFlavour.Linux ->
                failwith
                    "FileSystemStatistics.ofObject: a Linux-flavoured kernel holds a kqueue, which only Darwin has (this is a bug in the caller's state construction)."
