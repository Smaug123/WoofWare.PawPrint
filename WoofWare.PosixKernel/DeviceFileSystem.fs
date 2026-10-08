namespace WoofWare.PosixKernel

/// The configuration of a Linux devtmpfs that `stat(2)` and `statfs(2)` can
/// see.
type DevtmpfsMount =
    {
        /// The `st_dev` every inode on the devtmpfs reports: the anonymous
        /// device the kernel gave it when it mounted it, which depends on what
        /// was mounted before it.
        DeviceId : int64
        /// The `f_fsid` the mount reports, which a real devtmpfs draws at
        /// random when it is mounted.
        FileSystemId : FileSystemId
        /// `f_flags`: the `ST_*` bits of the mount's options, in Linux's
        /// numbering, `ST_VALID` included.
        Flags : int64
    }

[<RequireQualifiedAccess>]
module DevtmpfsMount =
    /// A devtmpfs as one measured Linux machine mounted it.
    let defaults : DevtmpfsMount =
        {
            // Measured 2026-10-02 on `/dev` in a Linux 6.18.5 aarch64 VM under
            // Apple's `container` (`devices.c`, STAT and STATFS rows): `st_dev`
            // 0,6; `f_fsid` 544366de:ad9235de; `f_flags` 0x1022, which is
            // ST_VALID, ST_NOSUID and ST_RELATIME, from the mount's
            // `rw,nosuid,relatime`.
            DeviceId = 6L
            FileSystemId =
                {
                    First = int32 0x544366deu
                    Second = int32 0xad9235deu
                }
            Flags = 0x1022L
        }

/// The filesystem a machine has mounted at `/dev`.
[<RequireQualifiedAccess>]
type DeviceFileSystemMount =
    /// Linux's devtmpfs, holding a node for each `CharacterDevice`.
    | Devtmpfs of DevtmpfsMount
    /// Darwin's devfs, which this kernel does not model: a path that reaches
    /// `/dev` is refused.
    | Devfs

[<RequireQualifiedAccess>]
module DeviceFileSystemMount =
    /// The device filesystem a machine of `flavour` mounts at `/dev` when a
    /// client expresses no preference.
    let defaultFor (flavour : SimulatedUnixFlavour) : DeviceFileSystemMount =
        match flavour with
        | SimulatedUnixFlavour.Linux -> DeviceFileSystemMount.Devtmpfs DevtmpfsMount.defaults
        | SimulatedUnixFlavour.Darwin -> DeviceFileSystemMount.Devfs

    /// What the filesystem's inode graph holds of this mount.
    let internal mounted (mount : DeviceFileSystemMount) : MountedFileSystem =
        match mount with
        | DeviceFileSystemMount.Devtmpfs _ -> MountedFileSystem.Devtmpfs
        | DeviceFileSystemMount.Devfs -> MountedFileSystem.Devfs

    /// Whether a machine of `flavour` can have this device filesystem.
    let isMountableUnder (flavour : SimulatedUnixFlavour) (mount : DeviceFileSystemMount) : bool =
        match flavour, mount with
        | SimulatedUnixFlavour.Linux, DeviceFileSystemMount.Devtmpfs _
        | SimulatedUnixFlavour.Darwin, DeviceFileSystemMount.Devfs -> true
        | SimulatedUnixFlavour.Linux, DeviceFileSystemMount.Devfs
        | SimulatedUnixFlavour.Darwin, DeviceFileSystemMount.Devtmpfs _ -> false

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CharacterDevice =
    /// Every device this kernel has a driver for, in the order a devtmpfs
    /// makes their nodes.
    let all : CharacterDevice list = [ CharacterDevice.Null ; CharacterDevice.URandom ]

    /// The name of the device's node in `/dev`.
    let name (device : CharacterDevice) : DirectoryEntryName =
        match device with
        | CharacterDevice.Null -> DirectoryEntryName.parseOrFail "CharacterDevice.name" "null"
        | CharacterDevice.URandom -> DirectoryEntryName.parseOrFail "CharacterDevice.name" "urandom"

    /// The permission bits of the device's node.
    let permissions (device : CharacterDevice) : PermissionBits =
        // Measured on both flavours: crw-rw-rw- for both nodes.
        match device with
        | CharacterDevice.Null
        | CharacterDevice.URandom -> PermissionBits 0o666

    /// What an `ioctl(2)` the device's Linux driver does not recognise answers:
    /// `FIONREAD` and `TCGETS` (`tcgetattr`) among them, neither device being a
    /// queue or a terminal. The buffer the request names is never looked at.
    ///
    /// Linux only: this kernel holds no device on Darwin.
    let unrecognisedIoctl (device : CharacterDevice) : UnixError =
        // Measured on Linux 6.18.5 (`devices-l2.c`, FIONREAD and TCGETATTR
        // rows, through descriptors opened for reading and for writing): null
        // has no ioctl operation, so the call is ENOTTY, while urandom's
        // `random_ioctl` answers EINVAL for a command it does not know.
        match device with
        | CharacterDevice.Null -> UnixError.ENOTTY
        | CharacterDevice.URandom -> UnixError.EINVAL

    /// `st_rdev` for the device's node under a kernel of `flavour`, in that
    /// flavour's `dev_t` encoding.
    ///
    /// Linux only: this kernel holds no device on Darwin.
    let specialFileDevice (flavour : SimulatedUnixFlavour) (device : CharacterDevice) : int64 =
        match flavour with
        | SimulatedUnixFlavour.Darwin ->
            failwith
                $"CharacterDevice.specialFileDevice: asked for the number of %O{device} on Darwin, where this kernel holds no device (this is a bug in the caller)."
        | SimulatedUnixFlavour.Linux ->

        // The mem driver's registrations, which are fixed: 1,3 for null and 1,9
        // for urandom, measured on Linux 6.18.5 (`devices.c`, STAT rows).
        let major, minor =
            match device with
            | CharacterDevice.Null -> 1L, 3L
            | CharacterDevice.URandom -> 1L, 9L

        // glibc's `makedev`, which is how a 64-bit Linux `stat` encodes the pair:
        // measured, 1,3 is reported as 259 and 1,9 as 265.
        ((major &&& 0xFFFFF000L) <<< 32)
        ||| ((major &&& 0xFFFL) <<< 8)
        ||| ((minor &&& 0xFFFFFF00L) <<< 12)
        ||| (minor &&& 0xFFL)
