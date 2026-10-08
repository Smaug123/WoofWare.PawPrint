namespace WoofWare.PosixKernel

/// Why `ProcessLaunch.create` refuses a launch table.
[<RequireQualifiedAccess>]
type LaunchTableRefusal =
    /// The table names a negative descriptor, which no process has.
    /// Contradictory.
    | NegativeDescriptor of fd : int
    /// The table names a descriptor at or above `bound`
    /// (`SimulatedUnixPlatform.descriptorBound`), which this kernel assumes the
    /// process's `RLIMIT_NOFILE` reaches and models no higher. Unmodelled.
    | AtOrAboveBound of fd : int * bound : int

[<RequireQualifiedAccess>]
module LaunchTableRefusal =
    /// What this library knows about why it refused the table, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : LaunchTableRefusal) : string =
        match refusal with
        | LaunchTableRefusal.NegativeDescriptor fd ->
            $"the launch table names descriptor %d{fd}, which is negative; no process has a descriptor below 0."
        | LaunchTableRefusal.AtOrAboveBound (fd, bound) ->
            $"the launch table names descriptor %d{fd}, at or above the bound %d{bound} this kernel assumes the process's RLIMIT_NOFILE reaches; it hands out no descriptor there."

/// Why `ProcessLaunch.withCredentials` refuses credentials.
[<RequireQualifiedAccess>]
type CredentialsRefusal =
    /// More supplementary groups than the `limit` a process can hold on
    /// `flavour` (`SimulatedUnixPlatform.supplementaryGroupLimit`): `setgroups(2)`
    /// answers EINVAL above `NGROUPS_MAX`. Contradictory.
    | TooManySupplementaryGroups of count : int * limit : int * flavour : SimulatedUnixFlavour
    /// On Darwin, real, effective and saved IDs that are not all the same.
    /// Which of them a Darwin kernel consults has not been measured, since that
    /// needs a process that can change its user ID, which is root, and none has
    /// been available on Darwin. Unmeasured.
    | IdsDifferOnDarwin of credentials : Credentials

[<RequireQualifiedAccess>]
module CredentialsRefusal =
    /// What this library knows about why it refused the credentials, for a
    /// client composing a diagnostic that names its own knob.
    let describe (refusal : CredentialsRefusal) : string =
        match refusal with
        | CredentialsRefusal.TooManySupplementaryGroups (count, limit, flavour) ->
            $"%d{count} supplementary groups is more than the %d{limit} a process can hold on %O{flavour} (setgroups(2) answers EINVAL above NGROUPS_MAX)."
        | CredentialsRefusal.IdsDifferOnDarwin credentials ->
            $"the credentials %O{credentials} have real, effective and saved IDs that differ, which this library does not model on Darwin: which of them a Darwin kernel consults has not been measured. Give all three the same user ID and the same group ID."

/// Why `ProcessLaunch.withUmask` refuses a mask.
[<RequireQualifiedAccess>]
type UmaskRefusal =
    /// The mask holds a bit `flavour`'s `umask(2)` never stores: it keeps only
    /// `stored` (`SimulatedUnixPlatform.umaskStoredBits`). No parent could
    /// have left such a mask. Contradictory.
    | BitsNotStored of umask : PermissionBits * stored : PermissionBits * flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module UmaskRefusal =
    /// What this library knows about why it refused the mask, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : UmaskRefusal) : string =
        match refusal with
        | UmaskRefusal.BitsNotStored (umask, stored, flavour) ->
            $"the mask 0o%04o{PermissionBits.toInt umask} holds a bit %O{flavour}'s umask(2) never stores (it keeps only 0o%04o{PermissionBits.toInt stored}), so no process there could have it."

/// Why a process cannot be launched onto a machine as its `ProcessLaunch`
/// describes it.
[<RequireQualifiedAccess>]
type LaunchRefusal =
    /// The launch was described for the platform `launch`, and the machine is
    /// `machine`: the description was checked against the wrong platform's
    /// rules.
    | NotOfPlatform of launch : SimulatedUnixPlatform * machine : SimulatedUnixPlatform
    /// The process cannot start in `directory` on this machine's filesystem.
    | CurrentDirectory of directory : AbsoluteUnixPath * fault : CurrentDirectoryFault

[<RequireQualifiedAccess>]
module LaunchRefusal =
    /// What this library knows about why it refused the launch, for a client
    /// composing a diagnostic that names its own knob.
    let describe (refusal : LaunchRefusal) : string =
        match refusal with
        | LaunchRefusal.NotOfPlatform (launch, machine) ->
            $"the launch was described for %O{SimulatedUnixPlatform.flavour launch} (%O{launch}), but the machine is %O{SimulatedUnixPlatform.flavour machine} (%O{machine}); describe it with the machine's platform."
        | LaunchRefusal.CurrentDirectory (directory, fault) ->
            let described = AbsoluteUnixPath.toEscaped directory

            match fault with
            | CurrentDirectoryFault.DoesNotResolve error ->
                $"the process cannot start in \"%s{described}\", which does not resolve in the machine's filesystem (%O{error})."
            | CurrentDirectoryFault.TooLong flavour ->
                $"%O{flavour} refuses \"%s{described}\" as too long: a component is past NAME_MAX, or, on Darwin, a symbolic link it goes through expands past PATH_MAX."
            | CurrentDirectoryFault.NotADirectory ->
                $"the process cannot start in \"%s{described}\", which resolves to something that is not a directory."
            | CurrentDirectoryFault.Path refusal ->
                $"the kernel will not resolve \"%s{described}\": %s{PathRefusal.describe refusal}"

/// How a process starts: everything about a process that is fixed before its
/// first instruction, as its launcher sets it up before `exec`. Both the first
/// process on a machine (`UnixBootImage.boot`) and every later one
/// (`SimulatedMachine.launch`) are launched from one.
///
/// The process's ID is not here: the kernel chooses it, from the machine's
/// counters, when the process is launched.
///
/// Opaque: made by `create`, and configured by the setters in
/// `ProcessLaunch`, each of which checks its value against the platform the
/// launch was made for.
type ProcessLaunch<'Task when 'Task : comparison> =
    internal
        {
            Platform : SimulatedUnixPlatform
            /// The descriptors the launcher set up, each a pipe end of its own
            /// whose other end is the client's.
            Streams : Map<int, LaunchDescriptor>
            /// The process's first task, its thread-group leader.
            Leader : 'Task
            /// The logical processor the leader starts on.
            LeaderCpu : CpuId
            Credentials : Credentials
            Umask : PermissionBits
            Environment : UnixByteString list
            ProcessPath : AbsoluteUnixPath option
            CoreDumps : CoreDumps
            /// The directory the process starts in, resolved against the
            /// machine's filesystem when the process is launched.
            CurrentDirectory : AbsoluteUnixPath
        }

[<RequireQualifiedAccess>]
module ProcessLaunch =

    /// A process on `platform` with the descriptors `streams` open, and one
    /// task, `leader`, on the logical processor `leaderCpu`; otherwise as a
    /// default process of the platform's flavour starts: in the root, with
    /// `UnixSystem.defaultCredentials`, `UnixSystem.defaultUmask`, no
    /// environment, no executable path (`UnixSystem.defaultProcessPath`) and
    /// `UnixSystem.defaultCoreDumps`.
    ///
    /// Each entry of `streams` is a descriptor the launcher set up before the
    /// process started, at that number: a pipe end of its own, whose other end
    /// is the client's, as the entry says (see `LaunchDescriptor`). The pipes
    /// are made, in descriptor order, when the process is launched.
    ///
    /// The leader starts blocking no signal. A real process starts with the
    /// mask its parent's thread had when it called `execve(2)`, which keeps the
    /// mask; a client launching a process that inherits one sets it with
    /// `UnixSignal.pthreadSigmask` and `SIG_SETMASK` before the leader runs its
    /// first instruction, as inherited ignores are installed with
    /// `UnixSignal.sigaction`.
    ///
    /// Refuses a table naming a negative descriptor, or one at or above the
    /// bound (`SimulatedUnixPlatform.descriptorBound`).
    let create<'Task when 'Task : comparison>
        (platform : SimulatedUnixPlatform)
        (streams : Map<int, LaunchDescriptor>)
        (leader : 'Task)
        (leaderCpu : CpuId)
        : Result<ProcessLaunch<'Task>, LaunchTableRefusal>
        =
        let platform = SimulatedUnixPlatform.assertValid "ProcessLaunch.create" platform
        let flavour = SimulatedUnixPlatform.flavour platform
        let bound = SimulatedUnixPlatform.descriptorBound platform

        let offending =
            streams
            |> Map.toSeq
            |> Seq.tryPick (fun (fd, _) ->
                if fd < 0 then
                    Some (LaunchTableRefusal.NegativeDescriptor fd)
                elif fd >= bound then
                    Some (LaunchTableRefusal.AtOrAboveBound (fd, bound))
                else
                    None
            )

        match offending with
        | Some refusal -> Error refusal
        | None ->
            Ok
                {
                    Platform = platform
                    Streams = streams
                    Leader = leader
                    LeaderCpu = leaderCpu
                    Credentials = UnixSystem.defaultCredentials flavour
                    Umask = UnixSystem.defaultUmask
                    Environment = []
                    ProcessPath = UnixSystem.defaultProcessPath
                    CoreDumps = UnixSystem.defaultCoreDumps
                    CurrentDirectory = UnixSystem.defaultCurrentDirectory
                }

    /// The platform the launch was described for, whose rules its setters
    /// check against.
    let platform<'Task when 'Task : comparison> (launch : ProcessLaunch<'Task>) : SimulatedUnixPlatform =
        launch.Platform

    /// The process's first task.
    let leader<'Task when 'Task : comparison> (launch : ProcessLaunch<'Task>) : 'Task = launch.Leader

    /// Set who the process is.
    ///
    /// Refuses more supplementary groups than the platform's
    /// `SimulatedUnixPlatform.supplementaryGroupLimit`, which no process on it
    /// could hold. On Darwin it also refuses credentials whose real, effective
    /// and saved IDs are not all the same: which of them a Darwin kernel
    /// consults has not been measured, so this library does not model such a
    /// process there.
    let withCredentials<'Task when 'Task : comparison>
        (credentials : Credentials)
        (launch : ProcessLaunch<'Task>)
        : Result<ProcessLaunch<'Task>, CredentialsRefusal>
        =
        let platform = launch.Platform
        let flavour = SimulatedUnixPlatform.flavour platform
        let count = List.length credentials.SupplementaryGroups
        let limit = SimulatedUnixPlatform.supplementaryGroupLimit platform

        let idsAgree =
            credentials.RealUser = credentials.EffectiveUser
            && credentials.SavedUser = credentials.EffectiveUser
            && credentials.RealGroup = credentials.EffectiveGroup
            && credentials.SavedGroup = credentials.EffectiveGroup

        if count > limit then
            Error (CredentialsRefusal.TooManySupplementaryGroups (count, limit, flavour))
        else

        match flavour with
        | SimulatedUnixFlavour.Darwin when not idsAgree -> Error (CredentialsRefusal.IdsDifferOnDarwin credentials)
        | SimulatedUnixFlavour.Darwin
        | SimulatedUnixFlavour.Linux ->
            Ok
                { launch with
                    Credentials = credentials
                }

    /// Set the file-mode creation mask the process starts with: the one its
    /// parent left it, which it can read and replace with `umask`.
    ///
    /// Refuses a mask with a bit the platform's `umask(2)` never stores
    /// (`SimulatedUnixPlatform.umaskStoredBits`): on Linux, any of 0o7000. No
    /// parent could have left such a mask, so it names a process that cannot
    /// exist.
    let withUmask<'Task when 'Task : comparison>
        (umask : PermissionBits)
        (launch : ProcessLaunch<'Task>)
        : Result<ProcessLaunch<'Task>, UmaskRefusal>
        =
        let umask = PermissionBits.assertValid "ProcessLaunch.withUmask" umask
        let stored = SimulatedUnixPlatform.umaskStoredBits launch.Platform

        if PermissionBits.toInt umask &&& ~~~(PermissionBits.toInt stored) <> 0 then
            Error (UmaskRefusal.BitsNotStored (umask, stored, SimulatedUnixPlatform.flavour launch.Platform))
        else
            Ok
                { launch with
                    Umask = umask
                }

    /// Set the environment the process was started with, replacing whatever
    /// it held. The entries are kept in the order given, duplicates and all;
    /// see `UnixProcessState.Environment`.
    ///
    /// `context` prefixes the rejection a forged entry earns, and is the
    /// client's to choose: the host that has to fix one knows it by whatever
    /// name the client's own configuration gives it.
    let withEnvironment<'Task when 'Task : comparison>
        (context : string)
        (environment : UnixByteString list)
        (launch : ProcessLaunch<'Task>)
        : ProcessLaunch<'Task>
        =
        { launch with
            Environment = environment |> List.map (UnixByteString.assertValid context)
        }

    /// Set the path to the executable that started the process, or `None` to
    /// report that it has none. `None` is preserved rather than defaulted; see
    /// `UnixProcessState.ProcessPath`.
    ///
    /// `context` prefixes the rejection a forged path earns; see
    /// `withEnvironment`.
    let withProcessPath<'Task when 'Task : comparison>
        (context : string)
        (path : AbsoluteUnixPath option)
        (launch : ProcessLaunch<'Task>)
        : ProcessLaunch<'Task>
        =
        { launch with
            ProcessPath = path |> Option.map (AbsoluteUnixPath.assertValid context)
        }

    /// Set whether the process writes a core dump when a signal whose default
    /// action dumps core kills it. See `UnixProcessState.CoreDumps`.
    let withCoreDumps<'Task when 'Task : comparison>
        (coreDumps : CoreDumps)
        (launch : ProcessLaunch<'Task>)
        : ProcessLaunch<'Task>
        =
        { launch with
            CoreDumps = coreDumps
        }

    /// Set the directory the process starts in.
    ///
    /// Resolved when the process is launched, against the filesystem of the
    /// machine it is launched onto, which is what refuses a directory that is
    /// not there. The walk is privileged and symlink-following, deliberately:
    /// this is a host saying where its process was launched, not a process
    /// looking anything up, and a process is launched into a directory its
    /// parent had already reached. It is also the only moment the name is
    /// resolved, because after it the process holds the *directory* rather than
    /// the name, so the path `getcwd` reports is the physical one with every
    /// symlink resolved away.
    let withCurrentDirectory<'Task when 'Task : comparison>
        (directory : AbsoluteUnixPath)
        (launch : ProcessLaunch<'Task>)
        : ProcessLaunch<'Task>
        =
        { launch with
            CurrentDirectory = AbsoluteUnixPath.assertValid "ProcessLaunch.withCurrentDirectory" directory
        }

    /// The directory `directory` names in `machine`'s filesystem, walked as
    /// `withCurrentDirectory` says.
    let internal resolveCurrentDirectory
        (directory : AbsoluteUnixPath)
        (machine : UnixMachineState)
        : Result<InodeNumber, CurrentDirectoryFault>
        =
        let platform = machine.UnixPlatform
        let filesystem = machine.FileSystem
        let limits = SimulatedUnixPlatform.pathLimits platform

        match
            PathWalk.resolveExisting
                limits
                // Root, so that no directory's search bit refuses the walk:
                // it is privilege that exempts a caller, whoever owns what.
                (Credentials.ofIds UserId.root (GroupId.parseOrFail "ProcessLaunch.resolveCurrentDirectory" 0u) [])
                // The host names where the process starts; no process follows
                // a link to get there, so no sysctl screens one.
                SymlinkProtection.Off
                (VirtualFileSystem.root filesystem)
                SymlinkPolicy.Follow
                (UnixPath.ofAbsolute directory)
                filesystem
        with
        | Ok inode ->
            match VirtualFileSystem.tryGetContent inode filesystem with
            | Some (InodeContent.Directory _) ->
                // The walk started at the root, so a directory it reached has a
                // path back by construction. Checked anyway: the alternative to
                // crashing here is a process whose `getcwd` reports ENOENT from
                // its first instruction.
                match VirtualFileSystem.pathOfDirectory inode filesystem with
                | Some _ -> Ok inode
                | None ->
                    failwith
                        $"ProcessLaunch.resolveCurrentDirectory: \"%s{AbsoluteUnixPath.toEscaped directory}\" resolved to inode %O{inode}, but no path from the root reaches it (this is a bug in this library)."
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.CharacterDevice _) -> Error CurrentDirectoryFault.NotADirectory
            | Some (InodeContent.Symlink _) ->
                // `SymlinkPolicy.Follow` never finishes on one; `chdir` says
                // the same of the same walk.
                failwith
                    $"ProcessLaunch.resolveCurrentDirectory: the walk resolved \"%s{AbsoluteUnixPath.toEscaped directory}\" to inode %O{inode}, which is a symbolic link -- but it ran under SymlinkPolicy.Follow, which never finishes on one (this is a bug in this library)."
            | None ->
                failwith
                    $"ProcessLaunch.resolveCurrentDirectory: resolving \"%s{AbsoluteUnixPath.toEscaped directory}\" gave inode %O{inode}, which the filesystem does not contain (this is a bug in this library; run VirtualFileSystem.checkInvariants)."
        | Error (PathFailure.Errno UnixError.ENAMETOOLONG) ->
            Error (CurrentDirectoryFault.TooLong (SimulatedUnixPlatform.flavour platform))
        | Error (PathFailure.Errno error) -> Error (CurrentDirectoryFault.DoesNotResolve error)
        | Error (PathFailure.Refused refusal) -> Error (CurrentDirectoryFault.Path refusal)

    /// `launch` started on `machine` as the process `pid`, whose leader has
    /// the thread ID `leaderThreadId`, which the machine's allocator already
    /// records as live: the machine after it, with the launch table's pipes,
    /// the process on its process table and a hold on its current directory,
    /// and the process and its tasks.
    let internal launchOnto<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (launch : ProcessLaunch<'Task>)
        (pid : ProcessId)
        (leaderThreadId : OsThreadId)
        (machine : UnixMachineState)
        : Result<UnixMachineState * UnixProcessState<'Task, 'Handler> * Map<'Task, UnixTaskState>, LaunchRefusal>
        =
        let platform = machine.UnixPlatform

        if launch.Platform <> platform then
            Error (LaunchRefusal.NotOfPlatform (launch.Platform, platform))
        else

        match resolveCurrentDirectory launch.CurrentDirectory machine with
        | Error fault -> Error (LaunchRefusal.CurrentDirectory (launch.CurrentDirectory, fault))
        | Ok directory ->

        if not (Set.contains leaderThreadId (ThreadIdAllocator.live machine.ThreadIds)) then
            failwith
                $"ProcessLaunch.launchOnto: the leader's thread ID %O{leaderThreadId} is not live in the machine's allocator (this is a bug in this library)."

        // One pipe per launch descriptor, numbered in descriptor order from the
        // next pipe the machine makes.
        let (PipeId first) = machine.NextPipeId

        let launched =
            launch.Streams
            |> Map.toList
            |> List.mapi (fun index (fd, descriptor) ->
                let pipeId = PipeId (first + int64 index)
                let pipe, pipeEnd = PipeState.launch platform pid fd descriptor
                fd, pipeId, pipeEnd, pipe
            )

        let census =
            if Set.isEmpty (ProcessIdTable.live machine.ProcessIds) then
                DescriptorCensus.Complete
            else
                DescriptorCensus.OneProcessOf pid

        let registry =
            FileDescriptorRegistry.ofLaunchedPipes
                census
                (launched
                 |> List.map (fun (fd, pipeId, pipeEnd, _) -> fd, (pipeId, pipeEnd))
                 |> Map.ofList)
                machine.OpenFiles

        let machine =
            { machine with
                OpenFiles = FileDescriptorRegistry.openFiles registry
                Pipes =
                    (machine.Pipes, launched)
                    ||> List.fold (fun pipes (_, pipeId, _, pipe) -> Map.add pipeId pipe pipes)
                NextPipeId = PipeId (first + int64 (List.length launched))
                ProcessIds = ProcessIdTable.add pid machine.ProcessIds
            }
            |> UnixMachineState.holdCurrentDirectory directory

        let proc : UnixProcessState<'Task, 'Handler> =
            {
                FileDescriptors = FileDescriptorRegistry.descriptorTable registry
                Environment = launch.Environment
                CurrentDirectoryInode = directory
                ProcessPath = launch.ProcessPath
                Credentials = launch.Credentials
                Umask = launch.Umask
                ProcessId = pid
                Signals = SignalState.initial (SimulatedUnixPlatform.signalNumbering platform) Set.empty
                CoreDumps = launch.CoreDumps
            }

        Ok (machine, proc, UnixTaskTable.add launch.Leader launch.LeaderCpu leaderThreadId Map.empty)
