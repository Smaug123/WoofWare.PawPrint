namespace WoofWare.PosixKernel

/// Why `UnixBootImage.withProcessId` refuses a process ID.
[<RequireQualifiedAccess>]
type ProcessIdRefusal =
    /// On Linux, the ID is not below the machine's `pid_max`, so a Linux kernel
    /// could not have handed it out. Contradictory.
    | NotBelowPidMax of pid : ProcessId * pidMax : int32
    /// On Darwin, the ID is not below `PID_MAX` (`ProcessIdTable.darwinPidMax`,
    /// read from xnu's source and not measured), so a Darwin kernel could not
    /// have handed it out. Contradictory.
    | NotBelowDarwinPidMax of pid : ProcessId * pidMax : int32

[<RequireQualifiedAccess>]
module ProcessIdRefusal =
    /// What this library knows about why it refused the process ID, for a
    /// client composing a diagnostic that names its own knob.
    let describe (refusal : ProcessIdRefusal) : string =
        match refusal with
        | ProcessIdRefusal.NotBelowPidMax (pid, pidMax) ->
            $"process ID %O{pid} is not below pid_max %d{pidMax}, so a Linux kernel could not have handed it out."
        | ProcessIdRefusal.NotBelowDarwinPidMax (pid, pidMax) ->
            $"process ID %O{pid} is not below Darwin's PID_MAX %d{pidMax}, so a Darwin kernel could not have handed it out."

/// Why `UnixBootImage.withLeaderThreadId` refuses a thread ID.
[<RequireQualifiedAccess>]
type LeaderThreadIdRefusal =
    /// The machine is Linux's, where the leader's thread ID is the process ID,
    /// so it cannot be set apart from it. Contradictory.
    | IsTheProcessIdOnLinux
    /// The ID is 0, which nothing says a Darwin kernel reports, or the largest
    /// 64-bit ID, which leaves the counter no ID for a second thread.
    /// Unmeasured.
    | OutsideCounter of id : uint64

[<RequireQualifiedAccess>]
module LeaderThreadIdRefusal =
    /// What this library knows about why it refused the thread ID, for a
    /// client composing a diagnostic that names its own knob.
    let describe (refusal : LeaderThreadIdRefusal) : string =
        match refusal with
        | LeaderThreadIdRefusal.IsTheProcessIdOnLinux ->
            "on Linux the leader's thread ID is the process ID, so it cannot be set apart from it; set the process ID instead."
        | LeaderThreadIdRefusal.OutsideCounter id ->
            $"%d{id} is not a thread ID this library will start a Darwin counter at; it must be between 1 and %d{System.UInt64.MaxValue - 1UL}."

/// The boot-time configuration of a simulated machine, applied to the
/// `UnixBootImage` that `UnixSystem.initial` makes, and `boot`, which ends it
/// by launching the machine's first process.
///
/// Every setter here takes and returns an image, and no syscall takes one, so
/// a setting cannot be applied to a system that has already run: it describes
/// the machine from the moment it booted. What changes while the machine runs
/// is a syscall's effect, or an operation of the outside world on the running
/// system, such as `UnixSystem.advanceClock`. How a process starts is not
/// here but in its `ProcessLaunch`, which `boot` takes, apart from the IDs of
/// the first process, which are where the machine's counters start.
[<RequireQualifiedAccess>]
module UnixBootImage =

    let private withMachine<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (image : UnixBootImage<'Task, 'Handler>)
        (machine : UnixMachineState)
        : UnixBootImage<'Task, 'Handler>
        =
        { image with
            Machine = machine
        }

    /// The platform the machine was made for, which every launch onto it must
    /// be described for (`ProcessLaunch.create`).
    let platform<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (image : UnixBootImage<'Task, 'Handler>)
        : SimulatedUnixPlatform
        =
        image.Machine.UnixPlatform

    /// The machine this image describes, booted, with `launch` as its first
    /// process, ready for that process's first syscall. After this, nothing in
    /// this module applies to it.
    ///
    /// The process has the ID `withProcessId` set, `UnixSystem.defaultProcessId`
    /// unless it was set, and its leader the thread ID the machine's counter
    /// starts at. Refuses a launch described for another platform, and one
    /// whose directory the machine's filesystem does not let it start in.
    let boot<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (launch : ProcessLaunch<'Task>)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixSystem<'Task, 'Handler>, LaunchRefusal>
        =
        let leaderThreadId =
            match ThreadIdAllocator.live image.Machine.ThreadIds |> Set.toList with
            | [ id ] -> id
            | ids ->
                failwith
                    $"UnixBootImage.boot: the boot image's thread ID allocator records %A{ids} as live, where it holds the first process's leader's alone (this is a bug in this library)."

        ProcessLaunch.launchOnto launch image.ProcessId leaderThreadId image.Machine
        |> Result.map (fun (machine, proc, tasks) ->
            {
                Machine = machine
                Process = proc
                Tasks = tasks
                Leader = ProcessLaunch.leader launch
                Generation = MachineGeneration.first
            }
        )

    /// Set the ID `getpid(2)` reports for the machine's first process. On
    /// Linux this is also its leader's thread ID, and the thread IDs the
    /// machine hands out next follow on from it. On Darwin the process IDs the
    /// machine hands out next follow on from it.
    ///
    /// Refuses, on Linux, a process ID that is not below the machine's
    /// `pid_max`, and on Darwin one that is not below `PID_MAX`
    /// (`ProcessIdTable.darwinPidMax`).
    let withProcessId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pid : ProcessId)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, ProcessIdRefusal>
        =
        let pid = ProcessId.assertValid "UnixBootImage.withProcessId" pid
        let machine = image.Machine

        match machine.ThreadIds.Counter with
        | ThreadIdCounter.Linux (_, pidMax) ->
            if ProcessId.toInt32 pid >= pidMax then
                Error (ProcessIdRefusal.NotBelowPidMax (pid, pidMax))
            else
                let _, threadIds =
                    ThreadIdAllocator.startLinux "UnixBootImage.withProcessId" pidMax pid

                Ok
                    {
                        Machine =
                            { machine with
                                ThreadIds = threadIds
                            }
                        ProcessId = pid
                    }
        | ThreadIdCounter.Darwin _ ->
            if ProcessId.toInt32 pid >= ProcessIdTable.darwinPidMax then
                Error (ProcessIdRefusal.NotBelowDarwinPidMax (pid, ProcessIdTable.darwinPidMax))
            else
                Ok
                    {
                        Machine =
                            { machine with
                                ProcessIds = ProcessIdTable.darwinAfter pid
                            }
                        ProcessId = pid
                    }

    /// Set the first process's leader's thread ID on Darwin, where it is
    /// unrelated to the process ID; the IDs the machine's later threads get
    /// follow on from it.
    ///
    /// Refuses a Linux machine, where the leader's thread ID is the process ID
    /// (set that with `withProcessId`), and 0 and the largest 64-bit ID.
    let withLeaderThreadId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (id : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, LeaderThreadIdRefusal>
        =
        let machine = image.Machine

        match machine.ThreadIds.Counter with
        | ThreadIdCounter.Linux _ -> Error LeaderThreadIdRefusal.IsTheProcessIdOnLinux
        | ThreadIdCounter.Darwin _ ->
            // Neither end has been observed. 0 is not refused because a kernel
            // was seen not to report it, but because nothing says one would,
            // and the largest leaves no ID for a second thread.
            if id = 0UL || id = System.UInt64.MaxValue then
                Error (LeaderThreadIdRefusal.OutsideCounter id)
            else
                let _, threadIds =
                    ThreadIdAllocator.startDarwin "UnixBootImage.withLeaderThreadId" id

                { machine with
                    ThreadIds = threadIds
                }
                |> withMachine image
                |> Ok

    /// Realise `seed` as this machine's filesystem, created at `createdAt`.
    ///
    /// Takes the moment explicitly rather than reading the machine's realtime
    /// clock, so that the result does not depend on whether the caller
    /// happened to set the clock before or after the filesystem.
    ///
    /// `defaultOwner` owns the root and every seed entry that states no owner
    /// of its own; see `VirtualFileSystem.ofFileSystemSeed`.
    ///
    /// Every name in the seed is checked against the machine's platform: its
    /// `NAME_MAX`, then its rule for which names it binds, the order a binding
    /// checks them in. A name a kernel could never have created is not one its
    /// filesystem can hold. Also refuses a seed whose `dev` at the root is
    /// anything but an empty directory, since the device filesystem is mounted
    /// there.
    let withFileSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (createdAt : UnixTimestamp)
        (defaultOwner : InodeOwner)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        (image : UnixBootImage<'Task, 'Handler>)
        : Result<UnixBootImage<'Task, 'Handler>, FileSystemSeedFault>
        =
        let machine = image.Machine
        let platform = machine.UnixPlatform

        // A new filesystem hands out its own inode numbers, so a handle onto
        // the old graph would afterwards dangle or silently name whatever the
        // new one gave the same number. An image holds none: no process has
        // been launched onto it.

        let limits = SimulatedUnixPlatform.pathLimits platform
        let bindable = SimulatedUnixPlatform.bindableEntryNames platform
        let flavour = SimulatedUnixPlatform.flavour platform

        // The first offender in `Map` order, which is the order the seed is
        // realised in.
        let rec firstImpossibleName (entries : Map<DirectoryEntryName, SeedEntry>) : FileSystemSeedFault option =
            entries
            |> Map.toSeq
            |> Seq.tryPick (fun (name, entry) ->
                // A forged name is refused with the seed's context, as
                // `ofFileSystemSeed` would refuse it, rather than reaching the
                // measurement as a null.
                let name = DirectoryEntryName.assertValid "UnixBootImage.withFileSystem seed" name

                if not (PathLimits.nameWithinLimit limits name) then
                    Some (FileSystemSeedFault.SeedNameTooLong (name, flavour))
                elif not (BindableEntryNames.admits bindable name) then
                    Some (FileSystemSeedFault.SeedNameNotBindable (name, flavour))
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
            |> UnixSystem.mountDeviceFileSystem machine.DeviceMount createdAt
        with
        | Error (MountFault.CoveredEntryNotAnEmptyDirectory name) ->
            Error (FileSystemSeedFault.SeedCoversDeviceFileSystem name)
        | Ok filesystem ->
            { machine with
                FileSystem = filesystem
            }
            |> withMachine image
            |> Ok

    /// Set the greatest range end a user buffer may reach: the machine's
    /// `TASK_SIZE_MAX`.
    ///
    /// Refused on a platform that screens no buffer up front, which has no such
    /// limit to set, and for a limit no machine of the platform's architecture
    /// has been observed to have (see `ObservedUserAddressLimit`).
    let withUserAddressLimit<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (limit : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let platform = machine.UnixPlatform

        if not (SimulatedUnixPlatform.screensUserBufferUpFront platform) then
            failwith
                $"UnixBootImage.withUserAddressLimit: a %O{SimulatedUnixPlatform.flavour platform} kernel screens no buffer before performing an operation, so it has no user address limit to set; got 0x%x{limit}."

        let architecture = SimulatedUnixPlatform.architecture platform

        match ObservedUserAddressLimit.architectureOf limit with
        | Some observed when observed = architecture -> ()
        | Some observed ->
            failwith
                $"UnixBootImage.withUserAddressLimit: 0x%x{limit} is the TASK_SIZE_MAX of an %O{observed} machine, but this platform is %O{architecture}."
        | None ->
            failwith
                $"UnixBootImage.withUserAddressLimit: no %O{architecture} machine has been observed with a TASK_SIZE_MAX of 0x%x{limit}; ObservedUserAddressLimit lists those that have."

        { machine with
            UserBufferCheck = UserBufferCheck.BeforeOperation limit
        }
        |> withMachine image

    /// Seed the machine's entropy pool, which every random-bytes syscall draws
    /// from, with `seed` in place of `UnixSystem.defaultEntropySeed`. Every
    /// byte the pool hands out follows from the seed, so a run that must replay
    /// bit for bit depends on it.
    let withEntropySeed<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (seed : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        { image.Machine with
            EntropyPool = EntropyPool.ofSeed seed
        }
        |> withMachine image

    /// Set the logical-processor count the simulated process reports. Rejects
    /// non-positive values at the boundary rather than letting them reach a
    /// program that will divide by them.
    let withProcessorCount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (count : int)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        if count < 1 then
            failwith $"ProcessorCount must be at least 1; got %d{count}"

        { machine with
            ProcessorCount = count
        }
        |> withMachine image

    /// Sets the ephemeral range, and rewinds the cursor into it: a cursor left
    /// outside the range would hand out its first port from wherever the previous
    /// range had reached.
    let withEphemeralPortRange<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        ((low, high) : uint16 * uint16)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        if low = 0us then
            failwith
                "UnixMachineState.EphemeralPortRange: port 0 is how a process *asks* for an ephemeral port, so it cannot also be one that gets handed out. Start the range at 1 or above."

        if low > high then
            failwith
                $"UnixMachineState.EphemeralPortRange: the range %d{low}-%d{high} is empty, so no bind of port 0 could ever be answered."

        { machine with
            EphemeralPortRange = low, high
            NextEphemeralPort = low
        }
        |> withMachine image

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
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let seconds = UnixTimestamp.seconds bootTime

        if seconds < 0L then
            failwith
                $"UnixMachineState.BootTime: %O{bootTime} is before the Unix epoch, and this kernel does not model a realtime clock reading before it."

        if seconds > UnixMachineState.maxBootTimeSeconds then
            failwith
                $"UnixMachineState.BootTime: %O{bootTime} is after %d{UnixMachineState.maxBootTimeSeconds} seconds since the Unix epoch, from which the realtime clock could pass the largest time_t within the longest uptime this kernel represents."

        match SimulatedUnixPlatform.flavour machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux -> ()
        | SimulatedUnixFlavour.Darwin ->
            if UnixTimestamp.nanoseconds bootTime % 1000 <> 0 then
                failwith
                    $"UnixMachineState.BootTime: %O{bootTime} is finer than a microsecond, and Darwin keeps its boot instant as a struct timeval (sysctl kern.boottime), so no Darwin machine booted at it."

        { machine with
            BootTime = bootTime
        }
        |> withMachine image

    let withLocalAddresses<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (addresses : uint32 list)
        (routes : Ipv4Prefix list)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        // The prefix record is public, so a host can build one whose length is
        // outside [0, 32]; the CLI masks such a shift rather than faulting, which
        // would give an unrelated mask and a silently wrong bindability.
        let routes =
            routes |> List.map (Ipv4Prefix.assertValid "UnixMachineState.LocalRoutes")

        // An empty list is legal and means a machine with no addresses at all,
        // on which only the wildcard binds. That is a strange machine but a
        // representable one, and refusing it here would be inventing a rule.
        { machine with
            LocalAddresses = addresses
            LocalRoutes = routes
        }
        |> withMachine image

    /// Set the mount the machine's filesystem claims to be. `None` takes the
    /// flavour's own default; a mount of a type this machine's flavour could
    /// not mount is refused, because `fstatfs(2)` answers a *file* from the
    /// mount and every other descriptor from the flavour, so the pair must
    /// describe one machine.
    let withMount<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (mount : EmulatedMount option)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match mount with
            | None -> EmulatedMount.defaultFor flavour
            | Some requested ->
                let fsType = EmulatedMount.fileSystemType requested

                if not (EmulatedFileSystemType.isReportableUnder flavour fsType) then
                    failwith
                        $"UnixMachineState.Mount: a %O{flavour} kernel cannot report %O{fsType}, so a process asking `fstatfs` would learn a fact no such system could tell it. Pass None to take %O{flavour}'s own default, or pick a type that flavour mounts."

                requested

        { machine with
            Mount = resolved
        }
        |> withMachine image

    /// Set the `fs.protected_*` sysctls. Refused on Darwin for anything but
    /// `ProtectedFiles.off`, since Darwin has no such settings; `context` names
    /// the caller's knob in the refusal.
    let withProtectedFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (context : string)
        (protection : ProtectedFiles)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        if not (UnixMachineState.isProtectedFilesOf flavour protection) then
            failwith
                $"%s{context}: %A{protection} sets an fs.protected_* sysctl, which a %O{flavour} kernel does not have; it applies none of their rules (measured on Darwin 27.0, protected-sysctls.c). Leave every one of them Off."

        { machine with
            ProtectedFiles = protection
        }
        |> withMachine image

    /// Set the `st_dev` every pipe reports. `None` takes the flavour's default
    /// (`UnixMachineState.defaultPipeDevice`).
    ///
    /// Refuses anything but 0 on Darwin, whose pipes all report 0, and a
    /// negative device on Linux, which no `dev_t` is.
    let withPipeDevice<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (device : int64 option)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match device, flavour with
            | None, _ -> UnixMachineState.defaultPipeDevice flavour
            | Some device, SimulatedUnixFlavour.Darwin when device <> 0L ->
                failwith
                    $"UnixMachineState.PipeDevice: %d{device} is not a device a Darwin pipe reports; every pipe on Darwin reports st_dev 0 (measured on 27.0.0). Pass None, or 0."
            | Some device, _ when device < 0L ->
                failwith $"UnixMachineState.PipeDevice: %d{device} is negative, which no dev_t is."
            | Some device, _ -> device

        { machine with
            PipeDevice = resolved
        }
        |> withMachine image

    /// Set the `somaxconn` sysctl.
    ///
    /// `None` takes the measured default of this machine's flavour. The clamp
    /// this feeds (`connectSocket`'s capacity rule) was measured with the
    /// sysctl set to 3 on Linux and at the default 128 on Darwin, so a
    /// configured value is on measured ground, but it must be positive: no
    /// machine was measured with a non-positive somaxconn.
    let withSoMaxConn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : int option)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let resolved =
            match value with
            | None -> UnixMachineState.defaultSoMaxConn (SimulatedUnixPlatform.flavour machine.UnixPlatform)
            | Some value ->
                if value < 1 then
                    failwith
                        $"UnixMachineState.SoMaxConn: %d{value} is not positive, and no kernel was measured with a non-positive somaxconn — the accept-queue capacity it would imply is a guess. Configure a positive value, or None for the flavour's default."

                value

        { machine with
            SoMaxConn = resolved
        }
        |> withMachine image

    /// Set the TCP send buffer sysctl (`TcpSendSpace`). `None` takes the
    /// measured default of this machine's flavour.
    ///
    /// Under Darwin a value must lie between `UnixMachineState.darwinLoopbackSendPipe` and
    /// `UnixMachineState.darwinSocketBufferMax`, inclusive: Darwin itself refuses a sendspace
    /// above the maximum, and below the send pipe the size a connection's
    /// buffer grows to depends on which route it took, which this kernel does
    /// not model. Under Linux nothing reads the value, so configuring one is
    /// refused rather than silently ignored.
    let withTcpSendSpace<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (value : int option)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        let machine = image.Machine

        let flavour = SimulatedUnixPlatform.flavour machine.UnixPlatform

        let resolved =
            match value, flavour with
            | None, _ -> UnixMachineState.defaultTcpSendSpace flavour
            | Some value, SimulatedUnixFlavour.Linux ->
                failwith
                    $"UnixMachineState.TcpSendSpace: %d{value} was configured on a Linux machine, but this kernel models no Linux TCP send buffer, so nothing would read it. Pass None."
            | Some value, SimulatedUnixFlavour.Darwin ->
                if value > UnixMachineState.darwinSocketBufferMax then
                    failwith
                        $"UnixMachineState.TcpSendSpace: %d{value} exceeds kern.ipc.maxsockbuf (%d{UnixMachineState.darwinSocketBufferMax}), and Darwin refuses such a net.inet.tcp.sendspace with ERANGE. Configure at most %d{UnixMachineState.darwinSocketBufferMax}, or None for the default."

                if value < UnixMachineState.darwinLoopbackSendPipe then
                    failwith
                        $"UnixMachineState.TcpSendSpace: %d{value} is below %d{UnixMachineState.darwinLoopbackSendPipe}, the send pipe of Darwin's route to 127.0.0.1. A connection's handshake grows a send buffer that small to the send pipe of the route it takes, and this kernel does not model routes. Configure at least %d{UnixMachineState.darwinLoopbackSendPipe}, or None for the default."

                value

        { machine with
            TcpSendSpace = resolved
        }
        |> withMachine image
