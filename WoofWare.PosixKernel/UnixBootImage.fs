namespace WoofWare.PosixKernel

/// Why `UnixBootImage.withUserAddressLimit` refuses a limit.
[<RequireQualifiedAccess>]
type UserAddressLimitRefusal =
    /// The machine's flavour screens no buffer before performing an
    /// operation (Darwin), so it has no `TASK_SIZE_MAX` for a limit to set.
    /// Contradictory: no machine of the flavour has the setting.
    | NoUpFrontScreen of flavour : SimulatedUnixFlavour
    /// No machine of `architecture` has been observed with `limit` as its
    /// `TASK_SIZE_MAX` (see `ObservedUserAddressLimit`). `observedOn` is the
    /// architecture whose machines have been, if any. Unmeasured: a machine
    /// may have such a limit, but this library has not seen one.
    | NotObservedOn of
        limit : uint64 *
        architecture : SimulatedUnixArchitecture *
        observedOn : SimulatedUnixArchitecture option

[<RequireQualifiedAccess>]
module UserAddressLimitRefusal =
    /// What this library knows about why it refused the limit, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : UserAddressLimitRefusal) : string =
        match refusal with
        | UserAddressLimitRefusal.NoUpFrontScreen flavour ->
            $"a %O{flavour} kernel screens no buffer before performing an operation, so it has no user address limit to set."
        | UserAddressLimitRefusal.NotObservedOn (limit, architecture, Some observedOn) ->
            $"0x%x{limit} is the TASK_SIZE_MAX of an %O{observedOn} machine, but this platform is %O{architecture}."
        | UserAddressLimitRefusal.NotObservedOn (limit, architecture, None) ->
            $"no %O{architecture} machine has been observed with a TASK_SIZE_MAX of 0x%x{limit}; ObservedUserAddressLimit lists those that have."

/// Why `UnixBootImage.withProcessorCount` refuses a count.
[<RequireQualifiedAccess>]
type ProcessorCountRefusal =
    /// The count is below 1. Contradictory: a machine running the process
    /// has at least the processor it runs on, and programs divide by the
    /// count.
    | NotPositive of count : int

[<RequireQualifiedAccess>]
module ProcessorCountRefusal =
    /// What this library knows about why it refused the count, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : ProcessorCountRefusal) : string =
        match refusal with
        | ProcessorCountRefusal.NotPositive count ->
            $"%d{count} logical processors is not a machine: a process runs on at least one, and programs divide by the count."

/// Why `UnixBootImage.withEphemeralPortRange` refuses a range.
[<RequireQualifiedAccess>]
type EphemeralPortRangeRefusal =
    /// The range starts at port 0, which is how a process *asks* for an
    /// ephemeral port, so it cannot also be one handed out. Contradictory.
    | LowIsZero of high : uint16
    /// The low end is above the high end, so the range holds no port and no
    /// bind of port 0 could be answered. Contradictory.
    | Empty of low : uint16 * high : uint16

[<RequireQualifiedAccess>]
module EphemeralPortRangeRefusal =
    /// What this library knows about why it refused the range, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : EphemeralPortRangeRefusal) : string =
        match refusal with
        | EphemeralPortRangeRefusal.LowIsZero high ->
            $"the range 0-%d{high} starts at port 0, which is how a process asks for an ephemeral port, so it cannot also be one that gets handed out. Start the range at 1 or above."
        | EphemeralPortRangeRefusal.Empty (low, high) ->
            $"the range %d{low}-%d{high} is empty, so no bind of port 0 could ever be answered."

/// Why `UnixBootImage.withBootTime` refuses an instant.
[<RequireQualifiedAccess>]
type BootTimeRefusal =
    /// The instant is before the Unix epoch. Unmodelled: this library models
    /// no realtime clock reading before it.
    | BeforeEpoch of bootTime : UnixTimestamp
    /// The instant is after `maxSeconds` (`UnixMachineState.maxBootTimeSeconds`)
    /// seconds since the epoch, from which the realtime clock could pass the
    /// largest `time_t` within the longest uptime this library represents.
    /// Unmodelled.
    | PastMaxBootTime of bootTime : UnixTimestamp * maxSeconds : int64
    /// On Darwin, the instant has a nonzero sub-microsecond part. Darwin
    /// keeps the time it booted as a `struct timeval` (`sysctl
    /// kern.boottime`), so no Darwin machine booted at it. Contradictory.
    | FinerThanMicrosecond of bootTime : UnixTimestamp

[<RequireQualifiedAccess>]
module BootTimeRefusal =
    /// What this library knows about why it refused the instant, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : BootTimeRefusal) : string =
        match refusal with
        | BootTimeRefusal.BeforeEpoch bootTime ->
            $"%O{bootTime} is before the Unix epoch, and this kernel does not model a realtime clock reading before it."
        | BootTimeRefusal.PastMaxBootTime (bootTime, maxSeconds) ->
            $"%O{bootTime} is after %d{maxSeconds} seconds since the Unix epoch, from which the realtime clock could pass the largest time_t within the longest uptime this kernel represents."
        | BootTimeRefusal.FinerThanMicrosecond bootTime ->
            $"%O{bootTime} is finer than a microsecond, and Darwin keeps its boot instant as a struct timeval (sysctl kern.boottime), so no Darwin machine booted at it."

/// Why `UnixBootImage.withMount` refuses a mount.
[<RequireQualifiedAccess>]
type MountRefusal =
    /// A kernel of `flavour` cannot report `fileSystemType`
    /// (`EmulatedFileSystemType.isReportableUnder`), so a process asking
    /// `fstatfs(2)` would learn a fact no such system could tell it: APFS on
    /// Linux is contradictory, since no mainline Linux filesystem reports its
    /// type; tmpfs on Darwin is unmeasured, since a Darwin machine can mount
    /// one but none has been measured.
    | NotReportableUnder of fileSystemType : EmulatedFileSystemType * flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module MountRefusal =
    /// What this library knows about why it refused the mount, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : MountRefusal) : string =
        match refusal with
        | MountRefusal.NotReportableUnder (fileSystemType, flavour) ->
            $"a %O{flavour} kernel cannot report %O{fileSystemType}, so a process asking fstatfs would learn a fact no such system could tell it."

/// Why `UnixBootImage.withProtectedFiles` refuses a setting.
[<RequireQualifiedAccess>]
type ProtectedFilesRefusal =
    /// `protection` sets an `fs.protected_*` sysctl, and a kernel of
    /// `flavour` (Darwin) has none: measured on Darwin 27.0, it applies none
    /// of their rules. Contradictory.
    | NoSuchSysctls of protection : ProtectedFiles * flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module ProtectedFilesRefusal =
    /// What this library knows about why it refused the setting, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : ProtectedFilesRefusal) : string =
        match refusal with
        | ProtectedFilesRefusal.NoSuchSysctls (protection, flavour) ->
            $"%A{protection} sets an fs.protected_* sysctl, which a %O{flavour} kernel does not have; it applies none of their rules (measured on Darwin 27.0, protected-sysctls.c). Leave every one of them Off."

/// Why `UnixBootImage.withPipeDevice` refuses a device.
[<RequireQualifiedAccess>]
type PipeDeviceRefusal =
    /// The machine is Darwin's, whose pipes all report `st_dev` 0 (measured
    /// on 27.0.0), and the device is not 0. Contradictory.
    | DarwinReportsZero of device : int64
    /// The device is negative, which no `dev_t` is. Contradictory.
    | Negative of device : int64

[<RequireQualifiedAccess>]
module PipeDeviceRefusal =
    /// What this library knows about why it refused the device, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : PipeDeviceRefusal) : string =
        match refusal with
        | PipeDeviceRefusal.DarwinReportsZero device ->
            $"%d{device} is not a device a Darwin pipe reports; every pipe on Darwin reports st_dev 0 (measured on 27.0.0)."
        | PipeDeviceRefusal.Negative device -> $"%d{device} is negative, which no dev_t is."

/// Why `UnixBootImage.withSoMaxConn` refuses a value.
[<RequireQualifiedAccess>]
type SoMaxConnRefusal =
    /// The value is below 1. Unmeasured: no kernel was measured with a
    /// non-positive `somaxconn`, so the accept-queue capacity it would imply
    /// is a guess.
    | NotPositive of value : int

[<RequireQualifiedAccess>]
module SoMaxConnRefusal =
    /// What this library knows about why it refused the value, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : SoMaxConnRefusal) : string =
        match refusal with
        | SoMaxConnRefusal.NotPositive value ->
            $"%d{value} is not positive, and no kernel was measured with a non-positive somaxconn, so the accept-queue capacity it would imply is a guess."

/// Why `UnixBootImage.withTcpSendSpace` refuses a value.
[<RequireQualifiedAccess>]
type TcpSendSpaceRefusal =
    /// A value was configured on a machine of `flavour` (Linux), where this
    /// library models no TCP send buffer, so nothing would read it.
    /// Unmodelled.
    | NotReadOn of flavour : SimulatedUnixFlavour * value : int
    /// The value is above `max` (`UnixMachineState.darwinSocketBufferMax`,
    /// `kern.ipc.maxsockbuf`), and Darwin refuses such a
    /// `net.inet.tcp.sendspace` with ERANGE. Contradictory.
    | AboveSocketBufferMax of value : int * max : int
    /// The value is below `sendPipe` (`UnixMachineState.darwinLoopbackSendPipe`),
    /// the send pipe of Darwin's route to 127.0.0.1. A connection's handshake
    /// grows a send buffer that small to the send pipe of the route it takes,
    /// and this library does not model routes. Unmodelled.
    | BelowLoopbackSendPipe of value : int * sendPipe : int

[<RequireQualifiedAccess>]
module TcpSendSpaceRefusal =
    /// What this library knows about why it refused the value, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : TcpSendSpaceRefusal) : string =
        match refusal with
        | TcpSendSpaceRefusal.NotReadOn (flavour, value) ->
            $"%d{value} was configured on a %O{flavour} machine, but this kernel models no %O{flavour} TCP send buffer, so nothing would read it."
        | TcpSendSpaceRefusal.AboveSocketBufferMax (value, max) ->
            $"%d{value} exceeds kern.ipc.maxsockbuf (%d{max}), and Darwin refuses such a net.inet.tcp.sendspace with ERANGE."
        | TcpSendSpaceRefusal.BelowLoopbackSendPipe (value, sendPipe) ->
            $"%d{value} is below %d{sendPipe}, the send pipe of Darwin's route to 127.0.0.1. A connection's handshake grows a send buffer that small to the send pipe of the route it takes, and this kernel does not model routes."

/// Why `UnixBootImage.withTcpReceiveSpace` refuses a value.
[<RequireQualifiedAccess>]
type TcpReceiveSpaceRefusal =
    /// The value is not positive, and Linux refuses such a
    /// `net.ipv4.tcp_rmem` default. Contradictory.
    | NotPositive of value : int
    /// Darwin would grow a receive buffer of this `net.inet.tcp.recvspace`
    /// again as data arrives, which this library does not model; `reason` is
    /// `TcpBufferSizing.darwinReceiveSpaceRefusal`'s account of why.
    /// Unmodelled.
    | GrowsAfterHandshake of value : int * reason : string

[<RequireQualifiedAccess>]
module TcpReceiveSpaceRefusal =
    /// What this library knows about why it refused the value, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : TcpReceiveSpaceRefusal) : string =
        match refusal with
        | TcpReceiveSpaceRefusal.NotPositive value ->
            $"%d{value} is not positive, and Linux refuses such a net.ipv4.tcp_rmem default."
        | TcpReceiveSpaceRefusal.GrowsAfterHandshake (_, reason) -> reason

/// Why `UnixBootImage.withTcpSendSpaceMax` refuses a value.
[<RequireQualifiedAccess>]
type TcpSendSpaceMaxRefusal =
    /// A value was configured on a machine of `flavour` (Darwin), where this
    /// library sizes a send buffer from `TcpSendSpace` alone, so nothing would
    /// read it. Unmodelled.
    | NotReadOn of flavour : SimulatedUnixFlavour * value : int
    /// The value is not positive, and Linux refuses such a `net.ipv4.tcp_wmem`
    /// maximum. Contradictory.
    | NotPositive of value : int

[<RequireQualifiedAccess>]
module TcpSendSpaceMaxRefusal =
    /// What this library knows about why it refused the value, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : TcpSendSpaceMaxRefusal) : string =
        match refusal with
        | TcpSendSpaceMaxRefusal.NotReadOn (flavour, value) ->
            $"%d{value} was configured on a %O{flavour} machine, but this kernel sizes a %O{flavour} send buffer from TcpSendSpace alone, so nothing would read it."
        | TcpSendSpaceMaxRefusal.NotPositive value ->
            $"%d{value} is not positive, and Linux refuses such a net.ipv4.tcp_wmem maximum."

/// The boot-time configuration of a simulated process and the machine it runs
/// on, applied to the `UnixBootImage` that `UnixSystem.initial` makes, and
/// `boot`, which ends it.
///
/// Every setter here takes and returns an image, and no syscall takes one, so
/// a setting cannot be applied to a system that has already run: it describes
/// the machine from the moment it booted. What changes while the machine runs
/// is a syscall's effect, or an operation of the outside world on the running
/// system, such as `UnixSystem.advanceClock`.
///
/// A machine setter that refuses a value returns `Result`, as a syscall does:
/// its `Error` is a refusal type of its own whose cases state the facts, and
/// whose `describe` says them. Only the caller knows what it called the value,
/// so naming that is the caller's.
[<RequireQualifiedAccess>]
module UnixBootImage =

    let private ofSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        {
            System = system
        }

    /// Fails loudly unless the boot image's system `system` has one task, its
    /// leader: a setter that restarts the thread ID allocator gives it the
    /// leader's ID as its one live ID, which is right only then.
    let private assertLeaderOnly<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (system : UnixSystem<'Task, 'Handler>)
        : unit
        =
        if system.Tasks.Count <> 1 then
            failwith
                $"%s{context}: the boot image has %d{system.Tasks.Count} tasks, where it starts with its leader alone and no syscall takes an image (this is a bug in this library)."

    let private withMachine<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (image : UnixBootImage<'Task, 'Handler>)
        (machine : UnixMachineState)
        : UnixBootImage<'Task, 'Handler>
        =
        ofSystem
            { image.System with
                Machine = machine
            }

    let private withProcess<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (image : UnixBootImage<'Task, 'Handler>)
        (proc : UnixProcessState<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        ofSystem
            { image.System with
                Process = proc
            }

    /// The system this image describes, ready for its first syscall. After
    /// this, nothing in this module applies to it.
    let boot<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        image.System

    /// Set the ID `getpid(2)` reports for the simulated process. On Linux this is
    /// also the leader's thread ID, and the thread IDs the process's threads get
    /// follow on from it.
    ///
    /// `context` prefixes the rejection a configuration earns; see
    /// `withCredentials`.
    ///
    /// Refuses, on Linux, a process ID that is not below the machine's
    /// `pid_max`.
    let withProcessId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (pid : ProcessId)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let system = image.System

        let pid = ProcessId.assertValid context pid
        assertLeaderOnly context system
        let leader = UnixTaskTable.get system.Leader system.Tasks

        let tasks, machine =
            match system.Machine.ThreadIds.Counter with
            | ThreadIdCounter.Linux (_, pidMax) ->
                let leaderThreadId, threadIds = ThreadIdAllocator.startLinux context pidMax pid

                Map.add
                    system.Leader
                    { leader with
                        OsThreadId = leaderThreadId
                    }
                    system.Tasks,
                { system.Machine with
                    ThreadIds = threadIds
                }
            | ThreadIdCounter.Darwin _ -> system.Tasks, system.Machine

        // The pipes the process was launched with name it as the process they
        // were launched into, so they follow it to its new ID. Nothing has been
        // delivered yet under the old one: only a write delivers, and no
        // syscall takes an image.
        if DeliveryLog.count machine.Delivered <> 0 then
            failwith
                $"%s{context}: the boot image has already delivered bytes, which only a write can do (this is a bug in this library)."

        let pipes =
            machine.Pipes
            |> Map.map (fun _ pipe ->
                match pipe.Origin with
                | PipeOrigin.Launched (ExternalEndpoint (launchedInto, fd), client) when
                    launchedInto = system.Process.ProcessId
                    ->
                    { pipe with
                        Origin = PipeOrigin.Launched (ExternalEndpoint (pid, fd), client)
                    }
                | PipeOrigin.Launched _
                | PipeOrigin.Made _ -> pipe
            )

        { system with
            Machine =
                { machine with
                    Pipes = pipes
                }
            Process =
                { system.Process with
                    ProcessId = pid
                }
            Tasks = tasks
        }
        |> ofSystem

    /// Set the leader's thread ID on Darwin, where it is unrelated to the process
    /// ID; the IDs the process's threads get follow on from it.
    ///
    /// Refuses a Linux machine, where the leader's thread ID is the process ID
    /// (set that with `withProcessId`), and 0.
    let withLeaderThreadId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (id : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let system = image.System

        match system.Machine.ThreadIds.Counter with
        | ThreadIdCounter.Linux _ ->
            failwith
                $"%s{context}: on Linux the leader's thread ID is the process ID, so it cannot be set apart from it; set the process ID instead."
        | ThreadIdCounter.Darwin _ ->

        assertLeaderOnly context system
        let leader = UnixTaskTable.get system.Leader system.Tasks
        let leaderThreadId, threadIds = ThreadIdAllocator.startDarwin context id

        { system with
            Machine =
                { system.Machine with
                    ThreadIds = threadIds
                }
            Tasks =
                Map.add
                    system.Leader
                    { leader with
                        OsThreadId = leaderThreadId
                    }
                    system.Tasks
        }
        |> ofSystem

    /// Set who the simulated process is.
    ///
    /// `context` prefixes the rejection a configuration earns, and is the
    /// client's to choose, so a host that has to fix one is told the name its
    /// own configuration gives it.
    ///
    /// Refuses more supplementary groups than the platform's
    /// `SimulatedUnixPlatform.supplementaryGroupLimit`, which no process on it
    /// could hold. On Darwin it also refuses credentials whose real, effective
    /// and saved IDs are not all the same: which of them a Darwin kernel
    /// consults has not been measured, so this library does not model such a
    /// process there.
    let withCredentials<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (credentials : Credentials)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let system = image.System

        let platform = system.Machine.UnixPlatform
        let count = List.length credentials.SupplementaryGroups
        let limit = SimulatedUnixPlatform.supplementaryGroupLimit platform

        if count > limit then
            failwith
                $"%s{context}: %d{count} supplementary groups is more than the %d{limit} a process can hold on %O{SimulatedUnixPlatform.flavour platform} (setgroups(2) answers EINVAL above NGROUPS_MAX)."

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> ()
        | SimulatedUnixFlavour.Darwin ->
            // Measuring it needs a process that can change its user ID, which is
            // root, and none has been available on Darwin.
            let usersAgree =
                credentials.RealUser = credentials.EffectiveUser
                && credentials.SavedUser = credentials.EffectiveUser

            let groupsAgree =
                credentials.RealGroup = credentials.EffectiveGroup
                && credentials.SavedGroup = credentials.EffectiveGroup

            if not (usersAgree && groupsAgree) then
                failwith
                    $"%s{context}: the credentials %O{credentials} have real, effective and saved IDs that differ, which this library does not model on Darwin: which of them a Darwin kernel consults has not been measured. Give all three the same user ID and the same group ID."

        { system with
            Process =
                { system.Process with
                    Credentials = credentials
                }
        }
        |> ofSystem

    /// Set the file-mode creation mask the simulated process starts with: the
    /// one its parent left it, which it can read and replace with `umask`.
    ///
    /// `context` prefixes the rejection a configuration earns; see
    /// `withCredentials` for why the client supplies it.
    ///
    /// Refuses a mask with a bit the platform's `umask(2)` never stores
    /// (`SimulatedUnixPlatform.umaskStoredBits`): on Linux, any of 0o7000. No
    /// parent could have left such a mask, so it names a process that cannot
    /// exist.
    let withUmask<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (umask : PermissionBits)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let system = image.System

        let umask = PermissionBits.assertValid context umask
        let platform = system.Machine.UnixPlatform
        let stored = SimulatedUnixPlatform.umaskStoredBits platform

        if PermissionBits.toInt umask &&& ~~~(PermissionBits.toInt stored) <> 0 then
            failwith
                $"%s{context}: the mask 0o%04o{PermissionBits.toInt umask} holds a bit %O{SimulatedUnixPlatform.flavour platform}'s umask(2) never stores (it keeps only 0o%04o{PermissionBits.toInt stored}), so no process there could have it."

        { system with
            Process =
                { system.Process with
                    Umask = umask
                }
        }
        |> ofSystem

    /// Realise `seed` as this image's filesystem and start the simulated
    /// process in `directory`, together.
    ///
    /// One operation rather than two because neither answer is well-formed
    /// without the other: a current directory is an inode of *this* filesystem,
    /// and a filesystem replaces every inode number the previous one handed
    /// out.
    ///
    /// Takes the moment explicitly rather than reading
    /// the machine's realtime clock, so that the result does not depend
    /// on whether the caller happened to set the clock before or after the
    /// filesystem — an ordering dependence between two `with` functions is
    /// exactly the kind of thing that works until someone reorders the calls.
    ///
    /// The system's own platform decides whether the *path the caller wrote*
    /// is one a process on that flavour could name at all, through its
    /// `NAME_MAX`: 255 CJK characters is a legal directory name on Darwin and
    /// too long on Linux. It is a check on that path and not on the graph —
    /// the seed itself is realised without consulting any limit, so a
    /// filesystem may perfectly well contain a directory whose name the
    /// current directory could not spell.
    ///
    /// The walk is privileged and symlink-following, deliberately: this is a
    /// host saying where its process was launched, not a process looking anything
    /// up, and a process is launched into a directory its parent had already
    /// reached. It is also the only moment the name is resolved, because after
    /// it the process holds the *directory* rather than the name.
    ///
    /// So this records the inode alone. The path `getcwd` owes is derived from
    /// it, which is what makes that path the physical one with every symlink
    /// resolved away — measured on both kernels, `chdir("outer/lnk")` with
    /// `lnk -> inner` is followed by `getcwd() == ".../outer/inner"`.
    ///
    /// `defaultOwner` owns the root and every seed entry that states no owner
    /// of its own; see `VirtualFileSystem.ofFileSystemSeed`. It is an argument
    /// rather than read off the process, so that the result does not depend on
    /// whether the caller set the credentials before or after the filesystem.
    let withFileSystemAndCurrentDirectory<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (createdAt : UnixTimestamp)
        (defaultOwner : InodeOwner)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        (directory : AbsoluteUnixPath)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, CurrentDirectoryFault>
        =
        let system = image.System

        // The directory is admitted under the platform the process will run
        // on, which is the system's own: `NAME_MAX` counts bytes on Linux and
        // UTF-16 code units on Darwin, so a name one flavour admits is one the
        // other refuses.
        let platform = system.Machine.UnixPlatform

        // Asserted here as well as by any caller that names its own knob: this
        // is a package boundary, so the precondition cannot be left to the one
        // client that happens to check it today.
        let directory =
            AbsoluteUnixPath.assertValid "UnixBootImage.withFileSystemAndCurrentDirectory" directory

        // A new filesystem hands out its own inode numbers, so a handle onto
        // the old graph would afterwards dangle or silently name whatever the
        // new one gave the same number. An image holds none: its only
        // descriptors are the launch table's pipes, and the current directory,
        // which this replaces, is not a handle the new graph must honour.

        let limits = SimulatedUnixPlatform.pathLimits platform
        let bindable = SimulatedUnixPlatform.bindableEntryNames platform
        let flavour = SimulatedUnixPlatform.flavour platform

        // Every name in the seed, under this flavour's NAME_MAX and then its
        // rule for which names it binds -- the order a binding checks them in --
        // before the graph is built: a name a kernel could never have created
        // is not one its filesystem can hold. The first offender in `Map`
        // order, which is the order the seed is realised in.
        let rec firstImpossibleName (entries : Map<DirectoryEntryName, SeedEntry>) : CurrentDirectoryFault option =
            entries
            |> Map.toSeq
            |> Seq.tryPick (fun (name, entry) ->
                // A forged name is refused with the seed's context, as
                // `ofFileSystemSeed` would refuse it, rather than reaching the
                // measurement as a null.
                let name =
                    DirectoryEntryName.assertValid "UnixBootImage.withFileSystemAndCurrentDirectory seed" name

                if not (PathLimits.nameWithinLimit limits name) then
                    Some (CurrentDirectoryFault.SeedNameTooLong (name, flavour))
                elif not (BindableEntryNames.admits bindable name) then
                    Some (CurrentDirectoryFault.SeedNameNotBindable (name, flavour))
                else
                    match entry with
                    | SeedEntry.Directory (children, _, _) -> firstImpossibleName children
                    | SeedEntry.File _
                    | SeedEntry.Symlink _ -> None
            )

        match firstImpossibleName seed with
        | Some fault -> Error fault
        | None ->

        match
            VirtualFileSystem.ofFileSystemSeed
                createdAt
                defaultOwner
                (SimulatedUnixPlatform.symlinkCreationPermissions platform SeedEntry.symlinkCreatorsUmask)
                seed
            |> UnixSystem.mountDeviceFileSystem system.Machine.DeviceMount createdAt
        with
        | Error (MountFault.CoveredEntryNotAnEmptyDirectory name) ->
            Error (CurrentDirectoryFault.SeedCoversDeviceFileSystem name)
        | Ok filesystem ->

        let root = VirtualFileSystem.root filesystem

        let located =
            match
                PathWalk.resolveExisting
                    limits
                    // Root, so that no directory's search bit refuses the walk:
                    // it is privilege that exempts a caller, whoever owns what.
                    (Credentials.ofIds
                        UserId.root
                        (GroupId.parseOrFail "UnixBootImage.withFileSystemAndCurrentDirectory" 0u)
                        [])
                    // The host names where the process starts; no process follows
                    // a link to get there, so no sysctl screens one.
                    SymlinkProtection.Off
                    root
                    SymlinkPolicy.Follow
                    (UnixPath.ofAbsolute directory)
                    filesystem
            with
            | Ok inode ->
                match VirtualFileSystem.tryGetContent inode filesystem with
                | Some (InodeContent.Directory _) ->
                    // The walk started at the root, so a directory it
                    // reached has a path back by construction, and
                    // `toVirtualFileSystem` asserts its own invariants besides.
                    // Checked anyway: the alternative to crashing here is a
                    // process whose `getcwd` reports ENOENT from its first
                    // instruction.
                    match VirtualFileSystem.pathOfDirectory inode filesystem with
                    | Some _ -> Ok inode
                    | None ->
                        failwith
                            $"UnixBootImage.withFileSystemAndCurrentDirectory: \"%s{AbsoluteUnixPath.toEscaped directory}\" resolved to inode %O{inode}, but no path from the root reaches it. This is a bug in this library."
                | Some (InodeContent.RegularFile _)
                | Some (InodeContent.CharacterDevice _) -> Error CurrentDirectoryFault.NotADirectory
                | Some (InodeContent.Symlink _) ->
                    // `SymlinkPolicy.Follow` never finishes on one; `chdir` says
                    // the same of the same walk.
                    failwith
                        $"UnixBootImage.withFileSystemAndCurrentDirectory: the walk resolved \"%s{AbsoluteUnixPath.toEscaped directory}\" to inode %O{inode}, which is a symbolic link -- but it ran under SymlinkPolicy.Follow, which never finishes on one (this is a bug in this library)."
                | None ->
                    failwith
                        $"UnixBootImage.withFileSystemAndCurrentDirectory: resolving \"%s{AbsoluteUnixPath.toEscaped directory}\" gave inode %O{inode}, which the filesystem does not contain. This is a bug in this library; run VirtualFileSystem.checkInvariants."
            | Error (PathFailure.Errno UnixError.ENAMETOOLONG) ->
                Error (CurrentDirectoryFault.TooLong (SimulatedUnixPlatform.flavour platform))
            | Error (PathFailure.Errno error) -> Error (CurrentDirectoryFault.DoesNotResolve error)
            | Error (PathFailure.Refused refusal) -> Error (CurrentDirectoryFault.Path refusal)

        located
        |> Result.map (fun inode ->
            { system with
                Machine =
                    { system.Machine with
                        FileSystem = filesystem
                    }
                Process =
                    { system.Process with
                        CurrentDirectoryInode = inode
                    }
            }
        )
        |> Result.map ofSystem

    /// Set the greatest range end a user buffer may reach: the machine's
    /// `TASK_SIZE_MAX`.
    ///
    /// Refuses a limit on a platform that screens no buffer up front, which
    /// has no such limit to set, and a limit no machine of the platform's
    /// architecture has been observed to have (see `ObservedUserAddressLimit`).
    let withUserAddressLimit<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (limit : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, UserAddressLimitRefusal>
        =
        let machine = image.System.Machine

        let platform = machine.UnixPlatform

        if not (SimulatedUnixPlatform.screensUserBufferUpFront platform) then
            Error (UserAddressLimitRefusal.NoUpFrontScreen (SimulatedUnixPlatform.flavour platform))
        else

        let architecture = SimulatedUnixPlatform.architecture platform

        match ObservedUserAddressLimit.architectureOf limit with
        | Some observed when observed = architecture ->
            { machine with
                UserBufferCheck = UserBufferCheck.BeforeOperation limit
            }
            |> withMachine image
            |> Ok
        | observedOn -> Error (UserAddressLimitRefusal.NotObservedOn (limit, architecture, observedOn))

    /// Seed the machine's entropy pool, which every random-bytes syscall draws
    /// from, with `seed` in place of `UnixSystem.defaultEntropySeed`. Every
    /// byte the pool hands out follows from the seed, so a run that must replay
    /// bit for bit depends on it.
    let withEntropySeed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (seed : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        { image.System.Machine with
            EntropyPool = EntropyPool.ofSeed seed
        }
        |> withMachine image

    /// Set the logical-processor count the simulated process reports.
    ///
    /// Refuses a count below 1 at the boundary, rather than letting it reach a
    /// program that will divide by it.
    let withProcessorCount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (count : int)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, ProcessorCountRefusal>
        =
        let machine = image.System.Machine

        if count < 1 then
            Error (ProcessorCountRefusal.NotPositive count)
        else

        { machine with
            ProcessorCount = count
        }
        |> withMachine image
        |> Ok

    /// Sets the ephemeral range, inclusive at both ends, and rewinds the
    /// cursor into it: a cursor left outside the range would hand out its first
    /// port from wherever the previous range had reached.
    ///
    /// Refuses a range that starts at port 0, and an empty one.
    let withEphemeralPortRange<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        ((low, high) : uint16 * uint16)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, EphemeralPortRangeRefusal>
        =
        let machine = image.System.Machine

        if low = 0us then
            Error (EphemeralPortRangeRefusal.LowIsZero high)
        elif low > high then
            Error (EphemeralPortRangeRefusal.Empty (low, high))
        else

        { machine with
            EphemeralPortRange = low, high
            NextEphemeralPort = low
        }
        |> withMachine image
        |> Ok

    /// Set what the realtime clock read when this machine booted.
    ///
    /// Refuses an instant before the Unix epoch, since this library models no
    /// realtime clock reading before it, and one after `UnixMachineState.maxBootTimeSeconds`, from
    /// which the realtime clock could leave `time_t`. On Darwin, also refuses an
    /// instant with a nonzero sub-microsecond part: Darwin keeps the time it
    /// booted as a `struct timeval`, so it has no finer boot instant.
    let withBootTime<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (bootTime : UnixTimestamp)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, BootTimeRefusal>
        =
        let machine = image.System.Machine

        let seconds = UnixTimestamp.seconds bootTime

        let finerThanDarwinKeeps =
            match SimulatedUnixPlatform.flavour machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> false
            | SimulatedUnixFlavour.Darwin -> UnixTimestamp.nanoseconds bootTime % 1000 <> 0

        if seconds < 0L then
            Error (BootTimeRefusal.BeforeEpoch bootTime)
        elif seconds > UnixMachineState.maxBootTimeSeconds then
            Error (BootTimeRefusal.PastMaxBootTime (bootTime, UnixMachineState.maxBootTimeSeconds))
        elif finerThanDarwinKeeps then
            Error (BootTimeRefusal.FinerThanMicrosecond bootTime)
        else

        { machine with
            BootTime = bootTime
        }
        |> withMachine image
        |> Ok

    /// Set the IPv4 addresses this machine holds (`addresses`, host order) and
    /// the prefixes of its local routes (`routes`). Together they decide which
    /// addresses `bind(2)` takes, which the flavours read differently (see
    /// `SimulatedUnixPlatform.isBindableAddress`), and which destinations
    /// `connect(2)` treats as this machine's own.
    ///
    /// Admits any lists, stored as given. An empty list is a machine with
    /// nothing in it: with no addresses, only the wildcard binds. Entries may
    /// repeat or overlap, as on a real machine: every Linux's local table holds
    /// both `127.0.0.0/8` and `127.0.0.1/32`, and an address assigned to two
    /// interfaces holds two routes to one prefix.
    let withLocalAddresses<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (addresses : uint32 list)
        (routes : Ipv4Prefix list)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.System.Machine

        { machine with
            LocalAddresses = addresses
            LocalRoutes = routes
        }
        |> withMachine image

    /// Set the mount the machine's filesystem claims to be. `None` takes the
    /// flavour's own default.
    ///
    /// Refuses a mount of a type this machine's flavour could not report,
    /// because `fstatfs(2)` answers a *file* from the mount and every other
    /// descriptor from the flavour, so the pair must describe one machine.
    let withMount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (mount : EmulatedMount option)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, MountRefusal>
        =
        let machine = image.System.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match mount with
            | None -> Ok (EmulatedMount.defaultFor flavour)
            | Some requested ->
                let fsType = EmulatedMount.fileSystemType requested

                if EmulatedFileSystemType.isReportableUnder flavour fsType then
                    Ok requested
                else
                    Error (MountRefusal.NotReportableUnder (fsType, flavour))

        resolved
        |> Result.map (fun resolved ->
            { machine with
                Mount = resolved
            }
            |> withMachine image
        )

    /// Set the `fs.protected_*` sysctls.
    ///
    /// Refuses anything but `ProtectedFiles.off` on Darwin, which has no such
    /// settings.
    let withProtectedFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (protection : ProtectedFiles)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, ProtectedFilesRefusal>
        =
        let machine = image.System.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        if not (UnixMachineState.isProtectedFilesOf flavour protection) then
            Error (ProtectedFilesRefusal.NoSuchSysctls (protection, flavour))
        else

        { machine with
            ProtectedFiles = protection
        }
        |> withMachine image
        |> Ok

    /// Set the `st_dev` every pipe reports. `None` takes the flavour's default
    /// (`UnixMachineState.defaultPipeDevice`).
    ///
    /// Refuses anything but 0 on Darwin, whose pipes all report 0, and a
    /// negative device on Linux, which no `dev_t` is.
    let withPipeDevice<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (device : int64 option)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, PipeDeviceRefusal>
        =
        let machine = image.System.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match device, flavour with
            | None, _ -> Ok (UnixMachineState.defaultPipeDevice flavour)
            | Some device, SimulatedUnixFlavour.Darwin when device <> 0L ->
                Error (PipeDeviceRefusal.DarwinReportsZero device)
            | Some device, _ when device < 0L -> Error (PipeDeviceRefusal.Negative device)
            | Some device, _ -> Ok device

        resolved
        |> Result.map (fun resolved ->
            { machine with
                PipeDevice = resolved
            }
            |> withMachine image
        )

    /// Set the `somaxconn` sysctl.
    ///
    /// `None` takes the measured default of this machine's flavour. The clamp
    /// this feeds (`connectSocket`'s capacity rule) was measured with the
    /// sysctl set to 3 on Linux and at the default 128 on Darwin, so a
    /// configured value is on measured ground.
    ///
    /// Refuses a value below 1: no machine was measured with a non-positive
    /// somaxconn.
    let withSoMaxConn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : int option)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, SoMaxConnRefusal>
        =
        let machine = image.System.Machine

        let resolved =
            match value with
            | None -> Ok (UnixMachineState.defaultSoMaxConn (SimulatedUnixPlatform.flavour machine.UnixPlatform))
            | Some value when value < 1 -> Error (SoMaxConnRefusal.NotPositive value)
            | Some value -> Ok value

        resolved
        |> Result.map (fun resolved ->
            { machine with
                SoMaxConn = resolved
            }
            |> withMachine image
        )

    /// Set the TCP send buffer sysctl (`TcpSendSpace`). `None` takes the
    /// measured default of this machine's flavour.
    ///
    /// Under Darwin a value must lie between `UnixMachineState.darwinLoopbackSendPipe` and
    /// `UnixMachineState.darwinSocketBufferMax`, inclusive, and this refuses one
    /// outside them: Darwin itself refuses a sendspace above the maximum, and
    /// below the send pipe the size a connection's buffer grows to depends on
    /// which route it took, which this kernel does not model. Under Linux
    /// nothing reads the value, so this refuses any configured one rather than
    /// silently ignoring it.
    let withTcpSendSpace<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : int option)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, TcpSendSpaceRefusal>
        =
        let machine = image.System.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match value, flavour with
            | None, _ -> Ok (UnixMachineState.defaultTcpSendSpace flavour)
            | Some value, SimulatedUnixFlavour.Linux -> Error (TcpSendSpaceRefusal.NotReadOn (flavour, value))
            | Some value, SimulatedUnixFlavour.Darwin ->
                if value > UnixMachineState.darwinSocketBufferMax then
                    Error (TcpSendSpaceRefusal.AboveSocketBufferMax (value, UnixMachineState.darwinSocketBufferMax))
                elif value < UnixMachineState.darwinLoopbackSendPipe then
                    Error (TcpSendSpaceRefusal.BelowLoopbackSendPipe (value, UnixMachineState.darwinLoopbackSendPipe))
                else
                    Ok value

        resolved
        |> Result.map (fun resolved ->
            { machine with
                TcpSendSpace = resolved
            }
            |> withMachine image
        )

    /// Set the TCP receive buffer sysctl (`TcpReceiveSpace`). `None` takes the
    /// measured default of this machine's flavour.
    ///
    /// Under Linux a value must be positive. Under Darwin it must be one
    /// `TcpBufferSizing.darwinReceiveSpaceRefusal` admits: one whose buffer a
    /// connection's handshake sizes once and for all.
    let withTcpReceiveSpace<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : int option)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, TcpReceiveSpaceRefusal>
        =
        let machine = image.System.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match value, flavour with
            | None, _ -> Ok (UnixMachineState.defaultTcpReceiveSpace flavour)
            | Some value, SimulatedUnixFlavour.Linux ->
                if value <= 0 then
                    Error (TcpReceiveSpaceRefusal.NotPositive value)
                else
                    Ok value
            | Some value, SimulatedUnixFlavour.Darwin ->
                match TcpBufferSizing.darwinReceiveSpaceRefusal value with
                | Some reason -> Error (TcpReceiveSpaceRefusal.GrowsAfterHandshake (value, reason))
                | None -> Ok value

        resolved
        |> Result.map (fun resolved ->
            { machine with
                TcpReceiveSpace = resolved
            }
            |> withMachine image
        )

    /// Set the ceiling a TCP send buffer autotunes to (`TcpSendSpaceMax`).
    /// `None` takes the measured default of this machine's flavour.
    ///
    /// Under Linux a value must be positive. Under Darwin nothing reads the
    /// value, since this kernel sizes a Darwin send buffer from
    /// `TcpSendSpace` alone, so configuring one is refused rather than
    /// silently ignored.
    let withTcpSendSpaceMax<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : int option)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, TcpSendSpaceMaxRefusal>
        =
        let machine = image.System.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match value, flavour with
            | None, _ -> Ok (UnixMachineState.defaultTcpSendSpaceMax flavour)
            | Some value, SimulatedUnixFlavour.Darwin -> Error (TcpSendSpaceMaxRefusal.NotReadOn (flavour, value))
            | Some value, SimulatedUnixFlavour.Linux ->
                if value <= 0 then
                    Error (TcpSendSpaceMaxRefusal.NotPositive value)
                else
                    Ok value

        resolved
        |> Result.map (fun resolved ->
            { machine with
                TcpSendSpaceMax = resolved
            }
            |> withMachine image
        )

    /// Set the path to the executable that started the simulated process, or
    /// `None` to report that it has none. `None` is preserved rather than
    /// defaulted; see `UnixProcessState.ProcessPath`.
    ///
    /// `context` prefixes the rejection a forged path earns, and is the client's
    /// to choose: the host that has to fix one knows it by whatever name the
    /// client's own configuration gives it, not by this field's.
    let withProcessPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (path : AbsoluteUnixPath option)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let proc = image.System.Process

        { proc with
            ProcessPath = path |> Option.map (AbsoluteUnixPath.assertValid context)
        }
        |> withProcess image

    /// Set whether the process writes a core dump when a signal whose default
    /// action dumps core kills it. See `UnixProcessState.CoreDumps`.
    let withCoreDumps<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (coreDumps : CoreDumps)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let proc = image.System.Process

        { proc with
            CoreDumps = coreDumps
        }
        |> withProcess image

    /// Set the environment the simulated process was started with, replacing
    /// whatever it held. The entries are kept in the order given, duplicates and
    /// all; see `UnixProcessState.Environment`.
    ///
    /// `context` prefixes the rejection a forged entry earns; see
    /// `withProcessPath` for why the client supplies it.
    let withEnvironment<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (env : UnixByteString list)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let proc = image.System.Process

        { proc with
            Environment = env |> List.map (UnixByteString.assertValid context)
        }
        |> withProcess image
